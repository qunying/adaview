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

with Glib.Option;       use Glib.Option;
with Glib.Option.Extra; use Glib.Option.Extra;
with Gtk.Main.Extra;    use Gtk.Main.Extra;
with Gtkada.Intl;       use Gtkada.Intl;
with Glib.Error;        use Glib.Error;
with Glib;              use Glib;

with Adaview.Version; use Adaview.Version;
with GNAT.OS_Lib;

package body Adaview.Config is

   use BString;

   opts_ctx      : Goption_Context;
   opts          : GOption_Entry_Array (1 .. 4);
   gtk_opts_grp  : GOption_Group;
   show_version  : aliased Gboolean;
   show_help     : aliased Gboolean;
   show_help_all : aliased Gboolean;

   type gboolean_access is access all Gboolean;
   package ACL renames Ada.Characters.Latin_1;

   function To_Address (C : gboolean_access) return System.Address is
      function Convert is new Ada.Unchecked_Conversion
        (gboolean_access,
         Glib.C_Proxy);
   begin
      return Glib.To_Address (Convert (C));
   end To_Address;

   procedure print_short_version is
   begin
      Put (prgname & " " & Adaview.Version.Text & " - ");
      Put_Line (get_description);
      Put_Line (get_copyright);
   end print_short_version;

   procedure print_version is
   begin
      print_short_version;
      New_Line;
      Put_Line (get_license);
   end print_version;

   procedure usage (opts_ctx : in Goption_Context; Main_Help : Boolean) is
   begin
      print_short_version;
      Put_Line (-"Usage: adaview [OPTIONS...] [file [page]]");
      declare
         help_msg : String  := Get_Help (opts_ctx, Main_Help, null);
         idx      : Positive;
         count    : Natural := 0;
      begin
         -- skip the first few lines to use our own format
         for i in help_msg'Range loop
            if help_msg (i) = ACL.LF then
               count := count + 1;
               idx   := i;
            end if;
            exit when count = 2;
         end loop;
         Put_Line (help_msg (idx + 1 .. help_msg'Last));
      end;
      GNAT.OS_Lib.OS_Exit (0);
   end usage;

   procedure process_options is
      error : aliased GError;
      ret   : Boolean;
   begin
      -- fill in the options
      opts (1).Long_Name   := New_String ("version");
      opts (1).Short_Name  := 'v';
      opts (1).Description := New_String (-"Show version information");
      opts (1).Arg         := G_Option_Arg_None;
      opts (1).Arg_Data    := To_Address (show_version'Access);

      opts (2).Long_Name   := New_String ("help");
      opts (2).Short_Name  := 'h';
      opts (2).Description := New_String (-"Show help options");
      opts (2).Arg         := G_Option_Arg_None;
      opts (2).Arg_Data    := To_Address (show_help'Access);

      opts (3).Long_Name   := New_String ("help-all");
      opts (3).Short_Name  := Gchar (ACL.NUL);
      opts (3).Description := New_String (-"Show help options");
      opts (3).Arg         := G_Option_Arg_None;
      opts (3).Arg_Data    := To_Address (show_help_all'Access);

      opts_ctx := G_New;
      Add_Main_Entries (opts_ctx, opts);
      gtk_opts_grp := Get_Option_Group (False);
      Add_Group (opts_ctx, gtk_opts_grp);
      Set_Help_Enabled (opts_ctx, False);
      ret := Parse (opts_ctx, error'Access);
      if (ret = False) then
         Put_Line (-"Error: " & Get_Message (error));
         raise Parameter_Error;
      end if;
      if show_version /= 0 then
         print_version;
         GNAT.OS_Lib.OS_Exit (0);
      end if;
      if show_help /= 0 then
         usage (opts_ctx, True);
      end if;

      if show_help_all /= 0 then
         usage (opts_ctx, False);
      end if;
   end process_options;

   function get_file_md5 (file_name : Bounded_String) return String is
      use Ada.Streams;
      in_file         : Stream_IO.File_Type;
      in_stream       : Stream_IO.Stream_Access;
      data_block_size : constant := 8192;
      data            : byte_string_t (1 .. data_block_size);
      last            : Natural;
      subtype SEA_T is Stream_Element_Array (1 .. data_block_size);
      package SEA_Addr is new System.Address_To_Access_Conversions (SEA_T);
      into : SEA_Addr.Object_Pointer := SEA_Addr.To_Pointer (data'Address);
      got  : Stream_Element_Offset;

      md5_ctx : GNAT.MD5.Context := GNAT.MD5.Initial_Context;
   begin
      Stream_IO.Open (in_file, Stream_IO.In_File, To_String (file_name));
      in_stream := Stream_IO.Stream (in_file);

      while not Ada.Streams.Stream_IO.End_Of_File (in_file) loop
         Stream_IO.Read (in_file, into.all, got);
         last := Natural (got);
         -- Update MD5
         GNAT.MD5.Update (md5_ctx, into.all (1 .. got));
      end loop;
      Stream_IO.Close (in_file);
      return GNAT.MD5.Digest (md5_ctx);
   end get_file_md5;

   procedure load_config (ctx : in out context_t) is
      home             : path_string_t;
      data_home        : path_string_t;
      config_home      : path_string_t;
      recent_file_name : path_string_t;
      config_file_name : path_string_t;
   begin
      -- form data and config home, follow the xdg guideline
      home := To_Bounded_String (Ada.Environment_Variables.Value ("HOME"));
      data_home :=
        To_Bounded_String
          (Value ("XDG_DATA_HOME", To_String (home) & "/.local/share") &
           "/" &
           prgname);
      config_home :=
        To_Bounded_String
          (Value ("XDG_CONFIG_HOME", To_String (home) & "/.config") &
           "/" &
           prgname);
      -- make directories
      Create_Path (To_String (data_home));
      Create_Path (To_String (config_home));

      -- ok, try forming data and config file path
      ctx.data_file   := data_home & "/history";
      ctx.config_file := config_home & "/config.txt";
      load_history (ctx);
      Put_Line ("Got" & Integer'Image (ctx.total_doc) & " entries in history");
   end load_config;

   procedure save_config (ctx : in context_t) is
   begin
      if ctx.history_changed then
         save_history (ctx);
      end if;
   end save_config;

   procedure load_history (ctx : in out context_t) is
      in_file   : File_Type;
      tokens    : Slice_Set;
      separator : constant String := " ";
      data_line : Unbounded_String;
   begin
      Put_Line ("open config file:" & To_String (ctx.data_file));
      Open (in_file, Ada.Text_IO.In_File, To_String (ctx.data_file));

      read_line :
      loop
         data_line := Trim (Get_Line (in_file), Both);

         -- skip empty line
         if Length (data_line) = 0 then
            goto Continue;
         end if;

         Put_Line
           ("got a line with" &
            Integer'Image (Length (data_line)) &
            " characters");
         GNAT.String_Split.Create
           (S          => tokens,
            From       => To_String (data_line),
            Separators => separator,
            Mode       => Multiple);

         if Slice_Count (tokens) > 1 then
            Put_Line
              ("Token number " & Slice_Number'Image (Slice_Count (tokens)));
            ctx.total_doc := ctx.total_doc + 1;
         end if;

         for i in 1 .. Slice_Count (tokens) loop
            case i is
               when 1 =>
                  ctx.history (ctx.total_doc).checksum := Slice (tokens, i);
               when 2 =>
                  ctx.history (ctx.total_doc).name :=
                    To_Bounded_String (Slice (tokens, i));
               when 3 =>
                  ctx.history (ctx.total_doc).cur_page :=
                    Integer'Value (Slice (tokens, i));
               when 4 =>
                  ctx.history (ctx.total_doc).total_page :=
                    Integer'Value (Slice (tokens, i));
               when 5 =>
                  ctx.history (ctx.total_doc).class :=
                    doc_class_t'Value (Slice (tokens, i));
               when others =>
                  null; -- ignore any other fields for now
            end case;
         end loop;

         <<Continue>>
         null;
      end loop read_line;
   exception
      when Ada.Text_IO.Name_Error =>
         null; -- ignore Name error
      when Ada.Text_IO.End_Error =>
         if Is_Open (in_file) then
            Close (in_file);
         end if;
      when others =>
         raise;
   end load_history;

   procedure save_one_entry (out_file : in File_Type; doc : in doc_t) is
   begin
      Put (out_file, doc.checksum);
      Put (out_file, " " & To_String (doc.name));
      Put (out_file, Natural'Image (doc.cur_page));
      Put (out_file, Natural'Image (doc.total_page));
      Put (out_file, " " & doc_class_t'Image (doc.class));
      New_line (out_file);
   end save_one_entry;

   procedure save_history (ctx : in context_t) is
      out_file : File_Type;
   begin
      Put_Line ("save history to " & To_String (ctx.data_file));
      Open (out_file, Ada.Text_IO.Out_File, To_String (ctx.data_file));
      if ctx.current_doc.checksum (1) /= ' ' then
         save_one_entry (out_file, ctx.current_doc);
      end if;
      for i in 1 .. ctx.total_doc loop
         if ctx.current_doc.checksum /= ctx.history (i).checksum then
            save_one_entry (out_file, ctx.history (i));
         end if;
      end loop;
      Close (out_file);
   end save_history;

end Adaview.Config;
