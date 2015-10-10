-------------------------------------------------------------------------------
-- Adaview - A PostScript/PDF viewer based on ghostscript                    --
--                                                                           --
-- Copyright (c) 2014-2015 Zhu Qun-Ying.                                     --
--                                                                           --
-- This file is part of Adaview.                                             --
--                                                                           --
-- Adaview is free software; you can redistribute it and/or modify           --
-- it under the terms of the GNU General Public License as published by      --
-- the Free Software Foundation; either version 3 of the License, or         --
-- (at your option) any later version.                                       --
--                                                                           --
-- Adaview is distributed in the hope that it will be useful,                --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of            --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             --
-- GNU General Public License for more details.                              --
--                                                                           --
-- You should have received a copy of the GNU General Public License         --
-- along with this program; if not, see <http://www.gnu.org/licenses/>.      --
-------------------------------------------------------------------------------

with Ada.Characters.Latin_1;

with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with GNATCOLL.Mmap;           use GNATCOLL.Mmap;
with GNAT.String_Split;       use GNAT.String_Split;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Interfaces;              use Interfaces;

with GNAT.Source_Info;
with Ada.Directories;
with Adaview.Debug;
with Ada.Strings.Equal_Case_Insensitive;
with Adaview.Sys_Util; use Adaview.Sys_Util;

package body Adaview.PS is

   type File_Data_T is record
      File      : Mapped_File;
      Size      : File_Size;
      Page_Size : Integer;

      Str          : Str_Access;
      Str_End      : Integer   := 0;
      Offs         : File_Size := 0;
      Request_Size : Integer   := 0;

      Line_Begin : Integer := 0;
      Line_End   : Integer := 0;
      Line_Len   : Integer := 0;

      Skipped_Line : Boolean := False;
   end record;

   Break_Get_Chars_Buf_Size : constant := 49152;
   PS_Line_Length           : constant := 256;
   -- 255 characters + 1 newline

   package ACL renames Ada.Characters.Latin_1;
   package Dbg renames Adaview.Debug;
   package GSI renames GNAT.Source_Info;

   procedure IO_Init (File_Name : String; FD : in out File_Data_T);

   function Read_Line (FD : in out File_Data_T) return Boolean;
   -- Read the next line in the postscript file.
   -- Automatically skip over data (as indicated by
   -- %%BeginBinary/%%EndBinary or %%BeginData/%%EndData
   -- comments.)
   -- Also, skip over included documents (as indicated by
   -- %%BeginDocument/%%EndDocument comments.)

   function Get_Chars (FD : in out File_Data_T; Num : Integer) return Boolean;

   function Is_Comment
     (FD      : File_Data_T;
      Content : String;
      Offset  : Integer) return Boolean;
   pragma Inline (Is_Comment);
   function DSC_Comment (FD : File_Data_T) return Boolean;
   pragma Inline (DSC_Comment);
   function Is_Begin (FD : File_Data_T; Content : String) return Boolean;
   pragma Inline (Is_Begin);
   function Is_End (FD : File_Data_T; Content : String) return Boolean;
   pragma Inline (Is_End);
   function Skip_Until
     (FD     : in out File_Data_T;
      Target :        String) return Boolean;
   pragma Inline (Skip_Until);
   function Skip_Until_2
     (FD               : in out File_Data_T;
      Target, Target_2 :        String) return Boolean;
   pragma Inline (Skip_Until_2);

   function First_Word (FD : File_Data_T; Offset : Integer) return Integer;
   pragma Inline (First_Word);

   procedure Test_For_PDF_Password
     (Ctx  :        Context_T;
      File : in out File_Data_T);
   pragma Inline (Test_For_PDF_Password);

   function Get_Text_Line
     (FD     : File_Data_T;
      Offset : Integer) return Unbounded_String;
   -- skip over white space and return the rest of the line.
   -- If the text begins with '(' return the text string using Get_Text

   function Get_Text
     (FD     : File_Data_T;
      Offset : Integer) return Unbounded_String;
   -- return the next text string on the line.
   -- return Null_Unbonded_String if nothing is present.

   ---------------------------------------------------------------------------
   procedure Scan (Ctx : in out Context_T) is
      File        : File_Data_T;
      I           : Integer;
      Section_Len : Unsigned_64;
      Preread     : Boolean;
   begin
      Dbg.Put_Line (Dbg.Trace, "Enter " & GSI.Enclosing_Entity);
      if Length (Ctx.Cur_Doc.DCS_Name) > 0 then
         -- use the DCS file created from previous run
         IO_Init (To_String (Ctx.Cur_Doc.DCS_Name), File);
      else
         IO_Init (To_String (Ctx.Cur_Doc.Temp_Name), File);
      end if;

      if not Read_Line (File) then
         Close (File.File);
         return;
      end if;

      -- header comments
      if File.Line_Len > 1
        and then (Is_Comment (File, "%!PS", 0) or Is_Comment (File, "%!PS", 1))
      then
         Dbg.Put_Line (Dbg.Trace, "found PS-Adobe- comment");
         Ctx.Cur_Doc.Kind := PS_FILE;
         I                := File.Line_Begin;
         while I <= File.Line_End loop
            exit when File.Str (I) = ACL.Space or File.Str (I) = ACL.HT;
            I := I + 1;
         end loop;
         I := First_Word (File, I);
         if Is_Comment (File, "EPSF", I - File.Line_Begin) then
            Ctx.Cur_Doc.Kind := EPSF_FILE;
         end if;
         Ctx.Cur_Doc.Header_Pos :=
           Unsigned_64 (File.Offs - Offset (File.File)) + Unsigned_64 (I);
         Section_Len := Unsigned_64 (File.Line_Len);
      elsif Is_Comment (File, "%PDF-", 0) then
         Ctx.Cur_Doc.Kind := PDF_FILE;
         Create_PDF_DSC_File
           (Ctx.Cur_Doc.Temp_Name,
            Ctx.Cur_Doc.DCS_Name,
            Ctx.Password);
         Scan (Ctx);
      else
         Close (File.File);
         raise Unknown_File_Type with "Unable to identify file type.";
      end if;

      Test_For_PDF_Password (Ctx, File);

      Preread := False;
      while Preread or else Read_Line (File) loop
         if not Preread then
            Section_Len := Section_Len + Unsigned_64 (File.Line_Len);
         end if;
         Preread := False;
         exit when File.Str (File.Line_Begin) /= '%'
           or else Is_Comment (File, "%EndComments", 1)
           or else File.Str (File.Line_Begin + 1) = ACL.HT
           or else File.Str (File.Line_Begin + 1) = ACL.LF
           or else File.Str (File.Line_Begin + 1) = ACL.CR
           or else Is_Graphic (File.Str (File.Line_Begin + 1));

         if File.Str (File.Line_Begin + 1) /= '%' then
            null; -- do nothing
         elsif Ctx.Cur_Doc.Title = Null_Unbounded_String
           and then Is_Comment (File, "Title:", 2)
         then
            Ctx.Cur_Doc.Title := Get_Text_Line (File, 8);
            -- Using length of "%%Title:"
         end if;
      end loop;
      Close (File.File);
   end Scan;

   ---------------------------------------------------------------------------
   procedure IO_Init (File_Name : String; FD : in out File_Data_T) is
   begin
      FD.File      := Open_Read (File_Name);
      FD.Size      := Length (FD.File);
      FD.Page_Size := Get_Page_Size;
      Dbg.Put_Line
        (Dbg.Trace,
         "File [" & File_Name & "] size" & File_Size'Image (FD.Size));
   end IO_Init;

   ---------------------------------------------------------------------------
   function Read_Line (FD : in out File_Data_T) return Boolean is
      Ret           : Boolean;
      Nesting_Level : Integer;
      Data_Num      : File_Size;
      Tokens        : Slice_Set;
      Separator     : constant String := " ";
   begin
      Dbg.Put_Line (Dbg.Trace, "Enter " & GSI.Enclosing_Entity);
      Ret := Get_Chars (FD, -1);
      if Ret = False then
         return False;
      end if;
      if not DSC_Comment (FD) or else not Is_Comment (FD, "Begin", 2) then
         return True;
      end if;

      if Is_Begin (FD, "Document") then
         -- skip the EPS without handling its content
         Nesting_Level := 1;
         loop
            if not Get_Chars (FD, -1) then
               return False;
            end if;
            if DSC_Comment (FD) then
               if Is_Comment (FD, "Begin", 2)
                 and then Is_Begin (FD, "Document:")
               then
                  Nesting_Level := Nesting_Level + 1;
               elsif Is_Comment (FD, "End", 2)
                 and then Is_End (FD, "Document")
               then
                  Nesting_Level := Nesting_Level - 1;
               end if;
               exit when Nesting_Level = 0;
            end if;
         end loop;
      elsif Is_Begin (FD, "Feature:") then
         if not Skip_Until (FD, "EndFeature") then
            return False;
         end if;
      elsif Is_Begin (FD, "File") then
         if not Skip_Until_2 (FD, "EndFile", "EOF") then
            return False;
         end if;
      elsif Is_Begin (FD, "Font") then
         if not Skip_Until (FD, "EndFont") then
            return False;
         end if;
      elsif Is_Begin (FD, "ProcSet") then
         if not Skip_Until (FD, "EndProcSet") then
            return False;
         end if;
      elsif Is_Begin (FD, "Resource") then
         if not Skip_Until (FD, "EndResource") then
            return False;
         end if;
      elsif Is_Begin (FD, "Data") then
         Dbg.Put_Line (Dbg.Info, "Encountered BeginData:");
         GNAT.String_Split.Create
           (S          => Tokens,
            From       => String (FD.Str (FD.Line_Begin + 12 .. FD.Line_End)),
            Separators => Separator,
            Mode       => Multiple);
         Data_Num := File_Size'Value (Slice (Tokens, 1));
         if Slice (Tokens, 3) = "Lines" then
            Dbg.Put_Line (Dbg.Info, "skip lines " & Slice (Tokens, 1));
            while Data_Num > 0 loop
               if not Get_Chars (FD, -1) then
                  return False;
               end if;
               Data_Num := Data_Num - 1;
            end loop;
         else
            FD.Offs         := FD.Offs + Data_Num;
            FD.Request_Size := 0;
            FD.Str_End      := 0;
         end if;
         if not Skip_Until (FD, "EndData") then
            return False;
         end if;
      elsif Is_Begin (FD, "Binary:") then
         Dbg.Put_Line (Dbg.Info, "Encountered BeginNinary:");
         Data_Num :=
           File_Size'Value
             (String (FD.Str (FD.Line_Begin + 14 .. FD.Line_End)));
         FD.Offs         := FD.Offs + Data_Num;
         FD.Request_Size := 0;
         FD.Str_End      := 0;
         if not Skip_Until (FD, "EndBinary") then
            return False;
         end if;
      end if;

      return Ret;
   end Read_Line;

   ---------------------------------------------------------------------------
   function Get_Chars
     (FD  : in out File_Data_T;
      Num :        Integer) return Boolean is
      I            : Integer;
      Request_Size : Integer;
   begin
      Dbg.Put_Line (Dbg.Trace, "Enter " & GSI.Enclosing_Entity);

      FD.Line_Begin := FD.Line_End;

      Outter :
      loop
         if Num < 0 then -- reading whole line
            if FD.Str_End - FD.Line_End > 0 then
               I := FD.Line_End + 1;
               loop
                  exit when I > Last (FD.File)
                    or else (FD.Str (I) = ACL.LF or FD.Str (I) = ACL.CR);
                  I := I + 1;
               end loop;
               if I <= FD.Str_End then
                  if I < FD.Str_End
                    and then FD.Str (I) = ACL.CR
                    and then FD.Str (I + 1) = ACL.LF
                  then
                     I := I + 2;
                  else
                     I := I + 1;
                  end if;
                  FD.Line_End := I;
                  FD.Line_Len := FD.Line_End - FD.Line_Begin + 1;
                  exit Outter;
               elsif FD.Offs + Offset (FD.File) + File_Size (FD.Str_End) =
                 FD.Size
               then
                  return False;
               else
                  FD.Line_End := 0;
               end if;
            end if;
         else
            if FD.Str_End >=
              FD.Line_Begin + Num
            then -- reading specified Num of chars
               FD.Line_End := FD.Line_Begin + Num;
               exit Outter;
            else
               FD.Line_End := 0;
            end if;
         end if;

         Dbg.Put_Line (Dbg.Trace, "no end of line yet");

         if FD.Str_End - FD.Line_Begin > Break_Get_Chars_Buf_Size then
            Dbg.Put_Line (Dbg.Trace, "breaking line artifically");
            FD.Line_End := FD.Str_End;
            FD.Line_Len := FD.Line_End - FD.Line_Begin + 1;
            exit Outter;
         end if;

         -- map file with increase requesting size
         if FD.Request_Size = 0 or FD.Line_End = 0 then
            Request_Size := FD.Request_Size + FD.Page_Size * 4;
            if Num > Request_Size then
               Request_Size := (Num / FD.Page_Size + 1) * FD.Page_Size;
            end if;
            FD.Request_Size := Request_Size;
         end if;

         FD.Offs := File_Size (FD.Line_Begin);
         Read (FD.File, FD.Offs, File_Size (FD.Request_Size));

         FD.Line_Begin := Integer (FD.Offs - Offset (FD.File)) + 1;
         FD.Str        := Data (FD.File);
         FD.Str_End    := Last (FD.File);
         FD.Line_End   := 0;
      end loop Outter;

      return True;
   end Get_Chars;

   -----------------------------------------------------------------------
   function DSC_Comment (FD : File_Data_T) return Boolean is
   begin
      if FD.Line_Len >= 2
        and then FD.Str (FD.Line_Begin) = '%'
        and then FD.Str (FD.Line_Begin + 1) = '%'
      then
         return True;
      else
         return False;
      end if;
   end DSC_Comment;

   -----------------------------------------------------------------------
   function Is_Comment
     (FD      : File_Data_T;
      Content : String;
      Offset  : Integer) return Boolean is
      Start_Idx : constant Integer := FD.Line_Begin + Offset;
      function Eq
        (Left, Right : String) return Boolean renames
        Ada.Strings.Equal_Case_Insensitive;
   begin
      if FD.Line_Len + Offset >= Content'Length
        and then Eq
          (String (FD.Str (Start_Idx .. Start_Idx + Content'Length)),
           Content)
      then
         return True;
      elsif Content (Content'Last) = ':'
        and then Eq
          (String (FD.Str (Start_Idx .. Start_Idx + Content'Length - 1)),
           Content (Content'First .. Content'Last - 1))
        and then FD.Str (Content'Last) = ' '
      then
         return True;
      else
         return False;
      end if;
   end Is_Comment;

-----------------------------------------------------------------------
   function Is_Begin (FD : File_Data_T; Content : String) return Boolean is
   begin
      return Is_Comment (FD, Content, 7);
   end Is_Begin;

   -----------------------------------------------------------------------
   function Is_End (FD : File_Data_T; Content : String) return Boolean is
   begin
      return Is_Comment (FD, Content, 5);
   end Is_End;

   -----------------------------------------------------------------------
   function Skip_Until
     (FD     : in out File_Data_T;
      Target :        String) return Boolean is
   begin
      Dbg.Put_Line (Dbg.Info, "Skip until " & Target);
      FD.Skipped_Line := True;
      loop
         if not Get_Chars (FD, -1) then
            return False;
         end if;
         exit when DSC_Comment (FD) and then Is_Comment (FD, Target, 2);
      end loop;
      return True;
   end Skip_Until;

   -----------------------------------------------------------------------
   function Skip_Until_2
     (FD               : in out File_Data_T;
      Target, Target_2 :        String) return Boolean is
   begin
      Dbg.Put_Line (Dbg.Info, "Skipp until " & Target & " or " & Target_2);
      FD.Skipped_Line := True;
      loop
         if not Get_Chars (FD, -1) then
            return False;
         end if;
         exit when DSC_Comment (FD)
           and then
           (Is_Comment (FD, Target, 2) or Is_Comment (FD, Target_2, 2));
      end loop;
      return True;
   end Skip_Until_2;

   -----------------------------------------------------------------------
   function First_Word (FD : File_Data_T; Offset : Integer) return Integer is
      I : Integer := Offset;
   begin
      while I <= FD.Line_End loop
         exit when FD.Str (I) /= ACL.Space and FD.Str (I) /= ACL.HT;
         I := I + 1;
      end loop;
      return I;
   end First_Word;

   ---------------------------------------------------------------------------
   procedure Test_For_PDF_Password
     (Ctx  :        Context_T;
      File : in out File_Data_T) is
      Current_Line : constant String :=
        String (File.Str (File.Line_Begin .. File.Line_End));
   begin
      if Length (Ctx.Cur_Doc.DCS_Name) > 0 then
         -- handle wrong password or need password case ...
         --!pp off
         if Index (Current_Line,
                   "This file requires a password for access.") > 0
           or Index (Current_Line, "Password did not work.") > 0
         then
            Ada.Directories.Delete_File (To_String (Ctx.Cur_Doc.DCS_Name));
            Close (File.File);
            raise Need_Password with "PDF file need password.";
         end if;
         --!pp on
      end if;
   end Test_For_PDF_Password;

   ---------------------------------------------------------------------------
   function Get_Text_Line
     (FD     : File_Data_T;
      Offset : Integer) return Unbounded_String is
      Line_Idx : Integer := FD.Line_Begin + Offset;
   begin
      while Line_Idx <= FD.Line_End loop
         exit when FD.Str (Line_Idx) /= ' '
           and then FD.Str (Line_Idx) /= ACL.HT;
         Line_Idx := Line_Idx + 1;
      end loop;
      if Line_Idx > FD.Line_End then
         return Null_Unbounded_String;
      end if;
      if FD.Str (Line_Idx) = '(' then
         return Get_Text (FD, Line_Idx - FD.Line_Begin);
      end if;
      return To_Unbounded_String (String (FD.Str (Line_Idx .. FD.Line_End)));
   end Get_Text_Line;

   ---------------------------------------------------------------------------
   function Get_Text
     (FD     : File_Data_T;
      Offset : Integer) return Unbounded_String is
      Line_Idx : Integer := FD.Line_Begin + Offset;
      Level    : Integer;
      Quoted   : Boolean := False;
      Text     : String (1 .. PS_Line_Length);
      Text_End   : Integer := 1;
      Char, Char1, Char2, Char3, Char4 : Character;
      Zero_Pos : Integer := Character'Pos ('0');
   begin
      while Line_Idx <= FD.Line_End loop
         exit when FD.Str (Line_Idx) /= ' '
           and then FD.Str (Line_Idx) /= ACL.HT;
         Line_Idx := Line_Idx + 1;
      end loop;
      if FD.Str (Line_Idx) = '(' then
         Level := 0;
         Quoted := True;
         Line_Idx := Line_Idx + 1;
         while Line_Idx <= FD.Line_End
           and then not (Fd.Str (FD.Line_Idx) = ')' and then Level = 0)
           and then Text_End > PS_Line_Length;
         loop
            Char := FD.Str (Line_Idx);
            if Char = '\' and Line_Idx + 1 <= FD.Line_End then
               Char1 := FD.Str (Line_Idx + 1);
               case Char1 is
                  when 'n' =>
                  Text (Text_End) := ACL.LF;
                  Increment (Text_End);
                  Increment (Line_Idx, 2);
                  when 'r' =>
                  Text (Text_End) := ACL.CR;
                  Increment (Text_End);
                  Increment (Line_Idx, 2);
                  when 't' =>
                  Text (Text_End) := ACL.HT;
                  Increment (Text_End);
                  Increment (Line_Idx, 2);
               when 'b' =>
                  Text (Text_End) := ACL.BEL;
                  Increment (Text_End);
                  Increment (Line_Idx, 2);
               when 'f' =>
                  Text (Text_End) := ACL.LF;
                  Increment (Text_End);
                  Increment (Line_Idx, 2);
               when '\' =>
                  Text (Text_End) := '\';
                  Increment (Text_End);
                  Increment (Line_Idx, 2);
               when '(' =>
                  Text (Text_End) := '(';
                  Increment (Text_End);
                  Increment (Line_Idx, 2);
               when ')' =>
                  Text (Text_End) := ')';
                  Increment (Text_End);
                     Increment (Line_Idx, 2);
               when '0' .. '9' =>
                  if Line_Idx + 2 <= FD.Line_End then
                     Char2 := FD.Str (Line_Idx + 2);
                     if Is_Digit (Char2) then
                        if Line_Idx + 3 <= FD.Line_End then
                           Char3 := Fd.Str (Line_Idx + 3);
                           if Is_Digit (Char3) then
                              Text (Text_End) := Character'Val(((Char1'Pos - Zero_Pos) * 8
                                                               + Char2'Pos - Zero_Pos) * 8
                                                               + Char3'Pos - Zero_Pos);
                              Increment (Text_End);
                              Increment (Line_Idx, 4);
                           else
                              Text (Text_End) := Character'Val ((Char1'Pos - Zero_Pos) * 8
                                                                + Char2'Pos - Zero_Pos);
                              Increment (Text_End);
                              Increment (Line_Idx, 3);
                           end if;
                        else
                           Text (Text_End) := Character'Val ((Char1'Pos - Zero_Pos) * 8
                                                             + Char2'Pos - Zero_Pos);
                           Increment (Text_End);
                           Increment (Line_Idx, 3);
                        end if;
                     else
                        Text (TexT_End) := Character'Val (Char1'Pos - Zero_Pos);
                        Increment (Text_End);
                        Increment (Line_Idx, 2);
                     end if;
                  else
                     Text (TexT_End) := Character'Val (Char1'Pos - Zero_Pos);
                     Increment (Text_End);
                     Increment (Line_Idx, 2);
                  end if;
               when others =>
                  Text (Text_End) := FD.Str (Line_Idx + 1);
                  Increment (Text_End);
                  Increment (Line_Idx, 2);
               end case;
            elsif Char = '(' then
               Increment (Level);
               Text (Text_End) := Char;
               Increment (Text_End);
               Increment (Line_Idx);
            elsif Char = ')' then
               Decrement (Level);
               Text (Text_End) := Char;
               Increment (Text_End);
               Increment (Line_Idx);
            else
               Text (Text_End) := Char;
               Increment (Text_End);
               Increment (Line_Idx);
            end if;
         end loop;
         -- delet trailing ')'
         if Line_Idx < FD.Line_End then
            Increment (Line_Idx);
         end if;
      else
         while Line_Idx <= FD.Line_End loop
            Char := FD.Str (Line_Idx);
            exit when not (Char = ACL.Space or Char = ACL.HT or Char = ACL.LF
                           or Char = ACL.CR) and Text_End > PS_Line_Length;
            Text (Text_End) := Char;
            Increment (Text_End);
            Increment (Line_Idx);
         end loop;
      end if;
      if TextEnd > 1 then
         return To_Unbounded_String (Text (1 .. Text_End - 1));
      else
         return Null_Unbounded_String;
      end if;
   end Get_Text;
end Adaview.PS;
