-------------------------------------------------------------------------------
-- Adaview - A PostScript/PDF viewer based on ghostscript                    --
--                                                                           --
-- Copyright (c) 2014 Zhu Qun-Ying.                                          --
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
with Ada.Text_IO;                   use Ada.Text_IO;
with Interfaces.C.Strings;          use Interfaces.C.Strings;
with Ada.Directories;               use Ada.Directories;
with Ada.Environment_Variables;     use Ada.Environment_Variables;
with Ada.Strings;                   use Ada.Strings;
with Ada.Strings.Unbounded;         use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Exceptions;                use Ada.Exceptions;

with GNAT.String_Split; use GNAT.String_Split;

with System;
with Ada.Unchecked_Conversion;
with Ada.Streams.Stream_IO;
with System.Address_To_Access_Conversions;
with Ada.Characters.Latin_1;
use Interfaces.C;

with Glib.Option;       use Glib.Option;
with Glib.Option.Extra; use Glib.Option.Extra;
with Gtk.Main.Extra;    use Gtk.Main.Extra;
with Gtkada.Intl;       use Gtkada.Intl;
with Glib.Error;        use Glib.Error;
with Glib;              use Glib;

with Adaview.Version; use Adaview.Version;
with System.OS_Lib;   use System.OS_Lib;

package body Adaview.Config is

   Opts_Ctx      : Goption_Context;
   Opts          : GOption_Entry_Array (1 .. 4);
   Gtk_Opts_Grp  : GOption_Group;
   Show_Version  : aliased Gboolean;
   Show_Help     : aliased Gboolean;
   Show_Help_All : aliased Gboolean;
   Cmd_Gzip      : aliased String := "gzip";
   Cmd_Bzip2     : aliased String := "bzip2";
   Cmd_Xz        : aliased String := "xz";

   type Compress_T is (NO_COMPRESS, COMPRESS, GZIP, BZIP2, XZ);

   type Gboolean_Access is access all Gboolean;
   package ACL renames Ada.Characters.Latin_1;

   ---------------------------------------------------------------------------
   function To_Address (C : Gboolean_Access) return System.Address is
      function Convert is new Ada.Unchecked_Conversion
        (Gboolean_Access,
         Glib.C_Proxy);
   begin
      return Glib.To_Address (Convert (C));
   end To_Address;

   ---------------------------------------------------------------------------
   procedure Print_Short_Version is
   begin
      Put (Prg_Name & " " & Adaview.Version.Text & " - ");
      Put_Line (Get_Description);
      Put_Line (Get_Copyright);
   end Print_Short_Version;

   procedure Print_Version is
   begin
      Print_Short_Version;
      New_Line;
      Put_Line (Get_License);
   end Print_Version;

   ---------------------------------------------------------------------------
   procedure Usage (Opts_Ctx : in Goption_Context; Main_Help : Boolean) is
   begin
      Print_Short_Version;
      Put_Line (-"Usage: adaview [OPTIONS...] [file [page]]");
      declare
         Help_Msg : String  := Get_Help (Opts_Ctx, Main_Help, null);
         Idx      : Positive;
         Count    : Natural := 0;
      begin
         -- skip the first few lines to use our own format
         for i in Help_Msg'Range loop
            if Help_Msg (i) = ACL.LF then
               Count := Count + 1;
               Idx   := i;
            end if;
            exit when Count = 2;
         end loop;
         Put_Line (Help_Msg (Idx + 1 .. Help_Msg'Last));
      end;
      System.OS_Lib.OS_Exit (0);
   end Usage;

   ---------------------------------------------------------------------------
   procedure Process_Options is
      Error : aliased GError;
      Ret   : Boolean;
   begin
      -- fill in the options
      Opts (1).Long_Name   := New_String ("version");
      Opts (1).Short_Name  := 'v';
      Opts (1).Description := New_String (-"Show version information");
      Opts (1).Arg         := G_Option_Arg_None;
      Opts (1).Arg_Data    := To_Address (Show_Version'Access);

      Opts (2).Long_Name   := New_String ("help");
      Opts (2).Short_Name  := 'h';
      Opts (2).Description := New_String (-"Show help options");
      Opts (2).Arg         := G_Option_Arg_None;
      Opts (2).Arg_Data    := To_Address (Show_Help'Access);

      Opts (3).Long_Name   := New_String ("help-all");
      Opts (3).Description := New_String (-"Show all help options");
      Opts (3).Arg         := G_Option_Arg_None;
      Opts (3).Arg_Data    := To_Address (Show_Help_All'Access);

      Opts_Ctx := G_New;
      Add_Main_Entries (Opts_Ctx, Opts);
      Gtk_Opts_Grp := Get_Option_Group (False);
      Add_Group (Opts_Ctx, Gtk_Opts_Grp);
      Set_Help_Enabled (Opts_Ctx, False);
      Ret := Parse (Opts_Ctx, Error'Access);
      if Ret = False then
         Put_Line (-"Error: " & Get_Message (Error));
         raise Parameter_Error;
      end if;
      if Show_Version /= 0 then
         Print_Version;
         System.OS_Lib.OS_Exit (0);
      end if;
      if Show_Help /= 0 then
         Usage (Opts_Ctx, True);
      end if;

      if Show_Help_All /= 0 then
         Usage (Opts_Ctx, False);
      end if;
      -- free the opts and context
      for i in Opts'Range loop
         Free (Opts (i).Long_Name);
         Free (Opts (i).Description);
      end loop;
      Free (Opts_Ctx);
   end Process_Options;

   ---------------------------------------------------------------------------
   procedure Decompress_File
     (File_Name : in     Unbounded_String;
      Temp_Name : in out Unbounded_String;
      Compress_Type : in     Compress_T) is
      Base          : String := Base_Name (To_String (File_Name));
      Template_Name : char_array := To_C ("/tmp/adaview-" & Base & ".XXXXXX");
      CMD_Ptr       : System.OS_Lib.String_Access;
      Arguments     : Argument_List (1 .. 2);
      Fd            : File_Descriptor;
      Ret           : Integer;
      function C_Mkstemp (filename : char_array) return File_Descriptor;
      pragma Import (C, C_Mkstemp, "mkstemp");

      function Sys (Arg : char_array) return Integer;
      pragma Import (C, Sys, "system");
   begin
      Fd := C_Mkstemp (Template_Name);
      if Fd = -1 then
         raise No_temp_file;
      end if;
      Close (Fd);

      Put_Line ("got temp file " & To_Ada (Template_Name));
      -- remove the trailling NUL character
      Temp_Name :=
        To_Unbounded_String (To_Ada (Template_Name));
      case Compress_Type is
         when COMPRESS | GZIP =>
            CMD_Ptr := Cmd_Gzip'Access;
         when BZIP2 =>
            CMD_Ptr := Cmd_Bzip2'Access;
         when XZ =>
            CMD_Ptr := Cmd_Xz'Access;
         when others =>
            null;
      end case;
      --!pp off
      Ret := Sys (To_C (CMD_Ptr.all & " -dc " & To_String (File_Name)
                        & " > " & To_String (Temp_Name)));
      --!pp on
      Put_Line ("decompress result: " & Integer'Image (Ret));
      -- decompress file have zero length, consider failed.
      if Size (To_String (Temp_Name)) = 0 then
         raise Invalid_file
           with "file " & To_String (File_Name) & " is invalid.";
      end if;
   end Decompress_File;

   ---------------------------------------------------------------------------
   procedure Get_File_MD5
     (File_Name : in     Unbounded_String;
      Temp_Name : in out Unbounded_String;
      Checksum  :    out String) is
      use Ada.Streams;

      In_File         : Stream_IO.File_Type;
      Data_Block_Size : constant := 8192;

      Data : Byte_String_T (1 .. Data_Block_Size);
      -- so that we could use it for compression magic header detection

      Last           : Natural;
      Compress_Magic : constant Byte_String_T := (16#1F#, 16#9d#);
      Gzip_Magic     : constant Byte_String_T := (16#1F#, 16#8B#);

      Bzip2_Magic : constant Byte_String_T := (16#42#, 16#5a#, 16#68#);
      --"BZh"

      Xz_Magic : constant Byte_String_T :=
        (16#FD#, 16#37#, 16#7a#, 16#58#, 16#5A#, 16#00#);
      -- {0xFD, '7', 'z', 'X', 'Z', 0x00}
      Compress_Type : Compress_T := NO_COMPRESS;

      subtype SEA_T is Stream_Element_Array (1 .. Data_Block_Size);
      package SEA_Addr is new System.Address_To_Access_Conversions (SEA_T);
      Into    : SEA_Addr.Object_Pointer := SEA_Addr.To_Pointer (Data'Address);
      Got     : Stream_Element_Offset;
      MD5_Ctx : GNAT.MD5.Context        := GNAT.MD5.Initial_Context;
   begin
      Stream_IO.Open (In_File, Stream_IO.In_File, To_String (File_Name));
      -- read in sample first
      Stream_IO.Read (In_File, Into.all, Got);

      -- test for compression magic headers
      if Integer (Got) > Xz_Magic'Last then
         if Data (1 .. Compress_Magic'Last) = Compress_Magic then
            Compress_Type := COMPRESS;
         elsif Data (1 .. Gzip_Magic'Last) = Gzip_Magic then
            Compress_Type := GZIP;
         elsif Data (1 .. Bzip2_Magic'Last) = Bzip2_Magic then
            Compress_Type := BZIP2;
         elsif Data (1 .. Xz_Magic'Last) = Xz_Magic then
            Compress_Type := XZ;
         end if;
      end if;
      Put_Line ("compression method " & Compress_T'Image (Compress_Type));
      if Compress_Type = NO_COMPRESS then
         Last := Natural (Got);
         -- Update MD5
         GNAT.MD5.Update (MD5_Ctx, Into.all (1 .. Got));
      else
         Stream_IO.Close (In_File);
         Decompress_File (File_Name, Temp_Name, Compress_Type);
         Stream_IO.Open (In_File, Stream_IO.In_File, To_String (Temp_Name));
         Stream_IO.Read (In_File, Into.all, Got);
      end if;

      while not Ada.Streams.Stream_IO.End_Of_File (In_File) loop
         Stream_IO.Read (In_File, Into.all, Got);
         Last := Natural (Got);
         -- Update MD5
         GNAT.MD5.Update (MD5_Ctx, Into.all (1 .. Got));
      end loop;
      Stream_IO.Close (In_File);
      Checksum := GNAT.MD5.Digest (MD5_Ctx);
   end Get_File_MD5;

   ---------------------------------------------------------------------------
   procedure Load_Config (ctx : in out Context_T) is
      Data_Path        : Path_T;
      Conf_Path        : Path_T;
      Recent_File_Name : Path_T;
      Config_File_Name : Path_T;
   begin
      -- form data and config home, follow the xdg guideline
      declare
         Home      : String := Value ("HOME");
         Data_Home : String := Value ("XDG_DATA_HOME", Home & "/.local/share");
         Conf_Home : String := Value ("XDG_CONFIG_HOME", Home & "/.config");
      begin
         Data_Path := To_Unbounded_String (Data_Home & "/" & Prg_Name);
         Conf_Path := To_Unbounded_String (Conf_Home & "/" & Prg_Name);
      end;
      -- make directories
      Create_Path (To_String (Data_Path));
      Create_Path (To_String (Conf_Path));

      -- ok, try forming data and config file path
      ctx.Data_File   := Data_Path & "/history";
      ctx.Config_File := Conf_Path & "/config.txt";
      Load_History (ctx);
      Put_Line ("Got" & Integer'Image (ctx.Total_Doc) & " entries in history");
   end Load_Config;

   ---------------------------------------------------------------------------
   procedure Save_Config (Ctx : in Context_T) is
   begin
      if Ctx.History_Changed then
         Save_History (Ctx);
      end if;
   end Save_Config;

   ---------------------------------------------------------------------------
   procedure Load_History (Ctx : in out Context_T) is
      In_File   : File_Type;
      Tokens    : Slice_Set;
      Separator : constant String := "|";
      Data_Line : Unbounded_String;
   begin
      Put_Line ("open history file:" & To_String (Ctx.Data_File));
      Open (In_File, Ada.Text_IO.In_File, To_String (Ctx.Data_File));

      Read_Line :
      loop
         Data_Line := Trim (Get_Line (In_File), Both);

         -- skip empty line
         if Length (Data_Line) = 0 then
            goto Continue;
         end if;

         Put_Line
           ("got a line with" &
            Integer'Image (Length (Data_Line)) &
            " characters");
         GNAT.String_Split.Create
           (S          => Tokens,
            From       => To_String (Data_Line),
            Separators => Separator,
            Mode       => Multiple);

         if Slice_Count (Tokens) > 1 then
            Put_Line
              ("Token number " & Slice_Number'Image (Slice_Count (Tokens)));
            Ctx.Total_Doc := Ctx.Total_Doc + 1;
         end if;

         for i in 1 .. Slice_Count (Tokens) loop
            case i is
               when 1 =>
                  Ctx.History (Ctx.Total_Doc).Checksum := Slice (Tokens, i);
               when 2 =>
                  Ctx.History (Ctx.Total_Doc).Name :=
                    To_Unbounded_String (Slice (Tokens, i));
               when 3 =>
                  Ctx.History (Ctx.Total_Doc).Class :=
                    Doc_Class_T'Value (Slice (Tokens, i));
               when 4 =>
                  Ctx.History (Ctx.Total_Doc).Cur_Page :=
                    Integer'Value (Slice (Tokens, i));
               when 5 =>
                  Ctx.History (Ctx.Total_Doc).Total_Page :=
                    Integer'Value (Slice (Tokens, i));
               when others =>
                  null; -- ignore any other fields for now
            end case;
         end loop;

         <<Continue>>
         null;
      end loop Read_Line;
   exception
      when Ada.Text_IO.Name_Error =>
         null; -- ignore Name error
      when Ada.Text_IO.End_Error =>
         if Is_Open (In_File) then
            Close (In_File);
         end if;
      when others =>
         raise;
   end Load_History;

   ---------------------------------------------------------------------------
   procedure Save_One_Entry (Out_File : in File_Type; Doc : in Doc_T) is
   begin
      Put (Out_File, Doc.Checksum);
      Put (Out_File, "|" & To_String (Doc.Name));
      Put (Out_File, "|" & Doc_Class_T'Image (Doc.Class));
      Put (Out_File, "|" & Natural'Image (Doc.Cur_Page));
      Put (Out_File, "|" & Natural'Image (Doc.Total_Page));

      New_Line (Out_File);
   end Save_One_Entry;

   ---------------------------------------------------------------------------
   procedure Save_History (Ctx : in Context_T) is
      Out_File : File_Type;
   begin
      Put_Line ("save history to " & To_String (Ctx.Data_File));
      Create (Out_File, Ada.Text_IO.Out_File, To_String (Ctx.Data_File));
      if Ctx.Cur_Doc.Checksum (1) /= ' ' then
         Save_One_Entry (Out_File, Ctx.Cur_Doc);
      end if;
      for i in 1 .. Ctx.Total_Doc loop
         if Ctx.Cur_Doc.Checksum /= Ctx.History (i).Checksum then
            Save_One_Entry (Out_File, Ctx.History (i));
         end if;
      end loop;
      Close (Out_File);
   end Save_History;

end Adaview.Config;
