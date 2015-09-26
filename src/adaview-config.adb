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
with Ada.Text_IO;                   use Ada.Text_IO;
with Interfaces.C.Strings;          use Interfaces.C.Strings;
with Ada.Directories;               use Ada.Directories;
with Ada.Environment_Variables;     use Ada.Environment_Variables;
with Ada.Strings;                   use Ada.Strings;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Strings.Fixed;             use Ada.Strings.Fixed;

with GNAT.String_Split; use GNAT.String_Split;

with System;
with Ada.Unchecked_Conversion;
with Ada.Streams.Stream_IO;
with System.Address_To_Access_Conversions;
with Ada.Characters.Latin_1;

with Glib.Option;       use Glib.Option;
with Glib.Option.Extra; use Glib.Option.Extra;
with Gtk.Main.Extra;    use Gtk.Main.Extra;
with Gtkada.Intl;       use Gtkada.Intl;
with Glib.Error;        use Glib.Error;
with Glib;              use Glib;

with Adaview.Version;  use Adaview.Version;
with Adaview.Sys_Util; use Adaview.Sys_Util;
with GNAT.OS_Lib;      use GNAT.OS_Lib;
with String_Format;    use String_Format;
with Adaview.Debug;
with GNAT.MD5;

package body Adaview.Config is

   Opts_Ctx      : Goption_Context;
   Opts          : GOption_Entry_Array (1 .. 5);
   Gtk_Opts_Grp  : GOption_Group;
   Show_Version  : aliased Gboolean;
   Show_Help     : aliased Gboolean;
   Show_Help_All : aliased Gboolean;
   Debug_Level   : aliased Integer;
   Cmd_Gzip      : aliased String := "gzip";
   Cmd_Bzip2     : aliased String := "bzip2";
   Cmd_Xz        : aliased String := "xz";

   Entry_Mark      : constant String := "[File]";
   Checksum_Mark   : constant String := "Checksum";
   MD5_Method      : constant String := "MD5";
   Path_Mark       : constant String := "Path";
   Kind_Mark       : constant String := "Kind";
   Cur_Page_Mark   : constant String := "Cur_Page";
   Total_Page_Mark : constant String := "Total_Page";
   Backend_Mark    : constant String := "Backend";

   type Compress_T is (NO_COMPRESS, COMPRESS, GZIP, BZIP2, XZ);

   type Gboolean_Access is access all Gboolean;
   package ACL renames Ada.Characters.Latin_1;
   package Sys_Addr is new System.Address_To_Access_Conversions (Integer);
   package Dbg renames Adaview.Debug;

   function To_Address (C : Gboolean_Access) return System.Address;
   procedure Print_Short_Version;
   procedure Print_Version;
   procedure Usage (Opts_Ctx : in Goption_Context; Main_Help : Boolean);
   procedure Decompress_File
     (File_Name     : in     Unbounded_String;
      Temp_Name     : in out Unbounded_String;
      Compress_Type : in     Compress_T);
   procedure Save_One_Entry (Out_File : in File_Type; Doc : in Doc_T);

   procedure Load_History (Ctx : in out Context_T);
   procedure Save_History (Ctx : in Context_T);

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

      Opts (4).Long_Name   := New_String ("debug");
      Opts (4).Short_Name  := 'd';
      Opts (4).Description := New_String (-"Set debug level (0 ~ 8)");
      Opts (4).Arg         := G_Option_Arg_Int;
      Opts (4).Arg_Data    := Sys_Addr.To_Address (Debug_Level'Access);

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
         GNAT.OS_Lib.OS_Exit (0);
      end if;
      if Show_Help /= 0 then
         Usage (Opts_Ctx, True);
      end if;

      if Show_Help_All /= 0 then
         Usage (Opts_Ctx, False);
      end if;

      if Debug_Level < 0 or Debug_Level > 8 then
         Put_Line (-"Error: debug level could only be 0 ~ 8.");
         raise Parameter_Error;
      end if;

      case Debug_Level is
         when 0 =>
            Dbg.Set_Flag (Dbg.None);
         when 8 =>
            Dbg.Set_Flag (Dbg.Trace);
         when others =>
            Dbg.Set_Flag (Dbg.Info);
      end case;

      -- free the opts and context
      for i in Opts'Range loop
         Free (Opts (i).Long_Name);
         Free (Opts (i).Description);
      end loop;
      Free (Opts_Ctx);
   end Process_Options;

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

      --!pp off
      Compress_Magic : constant Byte_String_T := (16#1F#, 16#9d#);
      Gzip_Magic     : constant Byte_String_T := (16#1F#, 16#8B#);
      Bzip2_Magic    : constant Byte_String_T := (Character'Pos ('B'),
                                                  Character'Pos ('Z'),
                                                  Character'Pos ('h'));
      Xz_Magic       : constant Byte_String_T := (16#FD#,
                                                  Character'Pos ('7'),
                                                  Character'Pos ('z'),
                                                  Character'Pos ('X'),
                                                  Character'Pos ('Z'),
                                                  16#00#);
      Compress_Type  : Compress_T := NO_COMPRESS;
      --!pp on

      subtype SEA_T is Stream_Element_Array (1 .. Data_Block_Size);
      package SEA_Addr is new System.Address_To_Access_Conversions (SEA_T);
      Into : constant SEA_Addr.Object_Pointer :=
        SEA_Addr.To_Pointer (Data'Address);
      Got     : Stream_Element_Offset;
      MD5_Ctx : GNAT.MD5.Context := GNAT.MD5.Initial_Context;
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
      Dbg.Put_Line
        (Dbg.Trace,
         "compression method " & Compress_T'Image (Compress_Type));
      if Compress_Type /= NO_COMPRESS then
         Stream_IO.Close (In_File);
         Decompress_File (File_Name, Temp_Name, Compress_Type);
         Stream_IO.Open (In_File, Stream_IO.In_File, To_String (Temp_Name));
         Stream_IO.Read (In_File, Into.all, Got);
      end if;
      -- Update MD5
      GNAT.MD5.Update (MD5_Ctx, Into.all (1 .. Got));

      while not Ada.Streams.Stream_IO.End_Of_File (In_File) loop
         Stream_IO.Read (In_File, Into.all, Got);
         -- Update MD5
         GNAT.MD5.Update (MD5_Ctx, Into.all (1 .. Got));
      end loop;
      Stream_IO.Close (In_File);
      Checksum := GNAT.MD5.Digest (MD5_Ctx);
   end Get_File_MD5;

   ---------------------------------------------------------------------------
   procedure Load_Config (ctx : in out Context_T) is
      Data_Path : Path_T;
      Conf_Path : Path_T;
   begin
      -- form data and config home, follow the xdg guideline
      declare
         Home      : constant String := Value ("HOME");
         Data_Home : constant String :=
           Value ("XDG_DATA_HOME", Home & "/.local/share");
         Conf_Home : constant String :=
           Value ("XDG_CONFIG_HOME", Home & "/.config");
      begin
         Data_Path := +(Data_Home & "/" & Prg_Name);
         Conf_Path := +(Conf_Home & "/" & Prg_Name);
      end;
      -- make directories
      Create_Path (To_String (Data_Path));
      Create_Path (To_String (Conf_Path));

      -- ok, try forming data and config file path
      ctx.Data_File   := Data_Path & "/history";
      ctx.Config_File := Conf_Path & "/config.txt";
      Load_History (ctx);
      Dbg.Put_Line
        (Dbg.Trace,
         "Got" & Integer'Image (ctx.Total_Doc) & " entries in history");
   end Load_Config;

   ---------------------------------------------------------------------------
   procedure Save_Config (Ctx : in Context_T) is
   begin
      if Ctx.History_Changed then
         Save_History (Ctx);
      end if;
   end Save_Config;

   ---------------------------------------------------------------------------
   procedure Decompress_File
     (File_Name     : in     Unbounded_String;
      Temp_Name     : in out Unbounded_String;
      Compress_Type : in     Compress_T) is
      Base          : constant String     := Base_Name (To_String (File_Name));
      Template_Name : String := "/tmp/adaview_" & Base & ".XXXXXX";
      CMD_Ptr : GNAT.OS_Lib.String_Access;
      Ret     : Integer;

   begin
      mkstemp (Template_Name);

      Dbg.Put_Line (Dbg.Trace, "got temp file " & Template_Name);
      -- remove the trailling NUL character
      Temp_Name := +Template_Name;
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
      Ret := Sys_Util.system (CMD_Ptr.all & " -dc "
                              & To_String (File_Name) & " > "
                              & To_String (Temp_Name));
      --!pp on
      Dbg.Put_Line (Dbg.Trace, "decompress result: " & Integer'Image (Ret));
      -- decompress file have zero length, consider failed.
      if Ret /= 0 or else Size (To_String (Temp_Name)) = 0 then
         Delete_File (To_String (Temp_Name));
         raise Invalid_File
           with "file " & To_String (File_Name) & " is invalid.";
      end if;
   end Decompress_File;

   ---------------------------------------------------------------------------
   procedure Load_History (Ctx : in out Context_T) is
      In_File   : File_Type;
      Tokens    : Slice_Set;
      Separator : constant String := ":";
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

         if Length (Data_Line) = Entry_Mark'Length
           and then To_String (Data_Line) = Entry_Mark
         then
            Ctx.Total_Doc := Ctx.Total_Doc + 1;
            goto Continue;
         end if;

         GNAT.String_Split.Create
           (S          => Tokens,
            From       => To_String (Data_Line),
            Separators => Separator,
            Mode       => Multiple);
         declare
            Mark : constant String := Slice (Tokens, 1);
         begin
            if Mark = Path_Mark then
               Ctx.History (Ctx.Total_Doc).Name := +Slice (Tokens, 2);
            elsif Mark = Checksum_Mark then
               if Slice_Count (Tokens) /= 3 then
                  Dbg.Put_Line (Dbg.Trace, "Wrong checksum line.");
                  goto Continue;
               end if;
               if Slice (Tokens, 2) = MD5_Method then
                  Ctx.History (Ctx.Total_Doc).Checksum :=
                    Trim (Slice (Tokens, 3), Both);
               else
                  Dbg.Put_Line (Dbg.Trace, "Wrong checksum method.");
                  goto Continue;
               end if;
            elsif Mark = Kind_Mark then
               Ctx.History (Ctx.Total_Doc).Kind :=
                 Doc_Kind_T'Value (Slice (Tokens, 2));
            elsif Mark = Cur_Page_Mark then
               Ctx.History (Ctx.Total_Doc).Cur_Page :=
                 Integer'Value (Slice (Tokens, 2));
            elsif Mark = Total_Page_Mark then
               Ctx.History (Ctx.Total_Doc).Total_Page :=
                 Integer'Value (Slice (Tokens, 2));
            elsif Mark = Backend_Mark then
               Ctx.History (Ctx.Total_Doc).Backend :=
                 Backend_T'Value (Slice (Tokens, 2));
            end if;
         end;
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
   end Load_History;

   ---------------------------------------------------------------------------
   procedure Save_One_Entry (Out_File : in File_Type; Doc : in Doc_T) is
   begin
      Put_Line (Out_File, Entry_Mark);
      Put_Line
        (Out_File,
         Checksum_Mark & ":" & MD5_Method & ":" & Doc.Checksum);
      Put_Line (Out_File, Path_Mark & ":" & To_String (Doc.Name));
      Put_Line (Out_File, Kind_Mark & ":" & Doc_Kind_T'Image (Doc.Kind));
      Put_Line (Out_File, Cur_Page_Mark & ":" & Natural'Image (Doc.Cur_Page));
      Put_Line
        (Out_File,
         Total_Page_Mark & ":" & Natural'Image (Doc.Total_Page));
      Put_Line (Out_File, Backend_Mark & ":" & Backend_T'Image (Doc.Backend));
      New_Line (Out_File);
   end Save_One_Entry;

   ---------------------------------------------------------------------------
   procedure Save_History (Ctx : in Context_T) is
      Out_File : File_Type;
   begin
      Dbg.Put_Line (Dbg.Trace, "save history to " & To_String (Ctx.Data_File));
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
      Put_Line
        (Prg_Name & " " & Adaview.Version.Text & " - " & Get_Description);
      Put_Line (Get_Copyright);
   end Print_Short_Version;

   ---------------------------------------------------------------------------
   procedure Print_Version is
   begin
      Print_Short_Version;
      New_Line;
      Put_Line (Get_License);
   end Print_Version;

   ---------------------------------------------------------------------------
   procedure Usage (Opts_Ctx : in Goption_Context; Main_Help : Boolean) is
   begin
      Put_Line (-"Usage: adaview [OPTIONS...] [file [page]]");
      declare
         Help_Msg : constant String := Get_Help (Opts_Ctx, Main_Help, null);
         Idx      : Positive;
         Count    : Natural         := 0;
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
      GNAT.OS_Lib.OS_Exit (0);
   end Usage;
end Adaview.Config;
