-------------------------------------------------------------------------------
-- Adaview - A PostScript/PDF viewer based on ghostscript                    --
--                                                                           --
-- Copyright (c) 2014-2017 Zhu Qun-Ying.                                     --
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
with Ada.Strings.Unbounded;         use Ada.Strings.Unbounded;
with GNAT.String_Split;             use GNAT.String_Split;

with System;
with Ada.Unchecked_Conversion;
with System.Address_To_Access_Conversions;
with Ada.Characters.Latin_1;

with Glib.Option;       use Glib.Option;
with Glib.Option.Extra; use Glib.Option.Extra;
with Gtk.Main.Extra;    use Gtk.Main.Extra;
with Gtkada.Intl;       use Gtkada.Intl;
with Glib.Error;        use Glib.Error;
with Glib;              use Glib;

with Adaview.Version;  use Adaview.Version;
with GNAT.OS_Lib;      use GNAT.OS_Lib;
with Adaview.Sys_Util; use Adaview.Sys_Util;
with String_Format;    use String_Format;

with Adaview.Path;
with Adaview.Debug;

package body Adaview.Config is

   Opts_Ctx      : Goption_Context;
   Opts          : GOption_Entry_Array (1 .. 6);
   Gtk_Opts_Grp  : GOption_Group;
   Show_Version  : aliased Gboolean;
   Show_Help     : aliased Gboolean;
   Show_Help_All : aliased Gboolean;
   Password      : aliased chars_ptr := Null_Ptr;
   Debug_Level   : aliased Integer;

   Entry_Mark      : constant String := "[File]";
   Checksum_Mark   : constant String := "Checksum";
   MD5_Method      : constant String := "MD5";
   Path_Mark       : constant String := "Path";
   Kind_Mark       : constant String := "Kind";
   Cur_Page_Mark   : constant String := "Cur_Page";
   Total_Page_Mark : constant String := "Total_Page";
   Backend_Mark    : constant String := "Backend";

   type Gboolean_Access is access all Gboolean;
   package ACL renames Ada.Characters.Latin_1;
   package Sys_Addr is new System.Address_To_Access_Conversions (Integer);
   package Char_Ptr_Addr is new System.Address_To_Access_Conversions
     (chars_ptr);
   package Dbg renames Adaview.Debug;

   function To_Address (C : Gboolean_Access) return System.Address;
   procedure Print_Short_Version;
   procedure Print_Version;
   procedure Usage (Opts_Ctx : in Goption_Context; Main_Help : Boolean);
   procedure Save_One_Entry (Out_File : in File_Type; Doc : in Doc_T);

   procedure Load_History (Ctx : in out Context_T);
   procedure Save_History (Ctx : in Context_T);

   procedure Process_Media_File (M_File : File_Type);

   ---------------------------------------------------------------------------
   procedure Process_Options (Ctx : in out Context_T) is
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

      Opts (5).Long_Name   := New_String ("password");
      Opts (5).Short_Name  := 'p';
      Opts (5).Description := New_String (-"Password for the PDF file");
      Opts (5).Arg         := G_Option_Arg_String;
      Opts (5).Arg_Data    := Char_Ptr_Addr.To_Address (Password'Access);

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
            Dbg.Set_Flag (Dbg.NONE);
         when 8 =>
            Dbg.Set_Flag (Dbg.TRACE);
         when others =>
            Dbg.Set_Flag (Dbg.INFO);
      end case;

      if Password /= Null_Ptr then
         Ctx.Password := +Value (Password);
         Dbg.Put_Line (Dbg.TRACE, "got password " & To_String (Ctx.Password));
      end if;
      -- free the opts and context
      for i in Opts'Range loop
         Free (Opts (i).Long_Name);
         Free (Opts (i).Description);
      end loop;
      Free (Opts_Ctx);
   end Process_Options;

   ---------------------------------------------------------------------------
   procedure Load_Config (ctx : in out Context_T) is
      Data_Path : Path_T;
      Conf_Path : Path_T;
      Home      : constant String := Value ("HOME");
      Data_Home : constant String :=
        Value ("XDG_DATA_HOME", Home & "/.local/share");
      Conf_Home : constant String :=
        Value ("XDG_CONFIG_HOME", Home & "/.config");
   begin
      -- form data and config home, follow the xdg guideline
      Data_Path := +(Data_Home & "/" & Prg_Name);
      Conf_Path := +(Conf_Home & "/" & Prg_Name);
      -- make directories
      Create_Path (To_String (Data_Path));
      Create_Path (To_String (Conf_Path));

      -- ok, try forming data and config file path
      ctx.Data_File   := Data_Path & "/history";
      ctx.Config_File := Conf_Path & "/config.txt";
      Load_History (ctx);
      Dbg.Put_Line
        (Dbg.TRACE,
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
   procedure Load_History (Ctx : in out Context_T) is
      His_File  : File_Type;
      Tokens    : Slice_Set;
      Separator : constant String := ":";
      Data_Line : Unbounded_String;
   begin
      Put_Line ("open history file:" & To_String (Ctx.Data_File));
      Open (His_File, In_File, To_String (Ctx.Data_File));

      Read_Line :
      loop
         Data_Line := Trim (Get_Line (His_File), Both);

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
                  Dbg.Put_Line (Dbg.TRACE, "Wrong checksum line.");
                  goto Continue;
               end if;
               if Slice (Tokens, 2) = MD5_Method then
                  Ctx.History (Ctx.Total_Doc).Checksum :=
                    Trim (Slice (Tokens, 3), Both);
               else
                  Dbg.Put_Line (Dbg.TRACE, "Wrong checksum method.");
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
         if Is_Open (His_File) then
            Close (His_File);
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
      Dbg.Put_Line (Dbg.TRACE, "save history to " & To_String (Ctx.Data_File));
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
               Increment (Count);
               Idx := i;
            end if;
            exit when Count = 2;
         end loop;
         Put_Line (Help_Msg (Idx + 1 .. Help_Msg'Last));
      end;
      GNAT.OS_Lib.OS_Exit (0);
   end Usage;

   ---------------------------------------------------------------------------
   procedure Load_Medias is
      Home            : constant String := Value ("HOME");
      Medias_File     : constant String := "medias";
      Sys_Medias_File : constant String :=
        Adaview.Path.Share & "/" & Prg_Name & "/" & Medias_File;
      User_Medias_File : constant String :=
        Value ("XDG_CONFIG_HOME", Home & "/.config") & "/" & Prg_Name & "/"
        & Medias_File;

      M_File     : File_Type;
      Fail_Count : Integer := 0;

      procedure Open_Media_File (Name : String) is
      begin
         Put_Line ("open medias file: " & Name);
         Open (M_File, In_File, Name);
         Process_Media_File (M_File);
         Close (M_File);
      end;
   begin
      begin
	 Open_Media_File (Sys_Medias_File);
      exception
         when Ada.Text_IO.Name_Error =>
            Increment (Fail_Count);
      end;
      begin
	 Open_Media_File (User_Medias_File);
      exception
         when Ada.Text_IO.Name_Error =>
            Increment (Fail_Count);
      end;
      -- only try to open a local medias file when all the above failed.
      if Fail_Count = 2 then
         begin
	    Open_Media_File (Medias_File);
         exception
            when Ada.Text_IO.Name_Error =>
               Put_Line (+"No medias file found.");
         end;
      end if;
   end Load_Medias;

   ---------------------------------------------------------------------------
   procedure Process_Media_File (M_File : File_Type) is
      Tokens    : Slice_Set;
      Separator : constant String := " ";
      Data_Line : Unbounded_String;
      Media     : Media_T;
   begin
      Read_Line :
      loop
         Data_Line := Trim (Get_Line (M_File), Both);
         if Length (Data_Line) = 0
           or else Element (Data_Line, 1) = '#'
         -- # aa the first denotes a comment
         then
            goto continue;
         end if;
         GNAT.String_Split.Create
           (S          => Tokens,
            From       => To_String (Data_Line),
            Separators => Separator,
            Mode       => Multiple);
         if Slice_Count (Tokens) >= 3 then
            Media.Name   := +Slice (Tokens, 1);
            Media.Width  := Integer'Value (Slice (Tokens, 2));
            Media.Height := Integer'Value (Slice (Tokens, 3));
            Media.Used   := 0;
            Media_Vector.Append (Medias, Media);
         end if;
         <<continue>>
         null;
      end loop Read_Line;
   exception
      when Ada.Text_IO.End_Error =>
         null; -- ignore End_Error;
   end Process_Media_File;
end Adaview.Config;
-- vim: set expandtab ts=3 sts=3 sw=3 smarttab :
