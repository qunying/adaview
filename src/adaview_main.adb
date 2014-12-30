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

with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Long_Integer_Text_IO; use Ada.Long_Integer_Text_IO;
with Ada.Integer_Text_IO;      use Ada.Integer_Text_IO;
with Ada.Command_Line;         use Ada.Command_Line;
with Ada.Directories;          use Ada.Directories;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;

with Interfaces.C; use Interfaces.C;
with System;       use System;

with GS.API;    use GS.API;
with GS.Errors; use GS.Errors;

with Gtkada.Intl;    use Gtkada.Intl;
with Gtk.Main.Extra; use Gtk.Main.Extra;

with Adaview.Version;
with Adaview.Config;
with Adaview.Locale;

procedure Adaview_main is
   gs_version  : aliased revision_t;
   instance    : aliased instance_t;
   ret         : code_t;
   doc_ctx     : Adaview.Config.context_t;
   matched_idx : Natural := 0;
begin

   Setlocale;
   Text_Domain (Adaview.Version.prgname);
   Bind_Text_Domain (Adaview.Version.prgname, Adaview.Locale.Path);

   Adaview.Config.process_options;

   -- we only use the first unknow argument as file name
   if Argument_Count > 2 then
      Put_Line ("Too many arguments.");
      Set_Exit_Status (Failure);
      return;
   end if;

   Adaview.Config.load_config (doc_ctx);

   if Argument_Count >= 1 then
      doc_ctx.cur_doc.name := To_Unbounded_String ((Argument (1)));
      Put_Line ("Got document: " & To_String (doc_ctx.cur_doc.name));
      doc_ctx.cur_doc.temp_name := doc_ctx.cur_doc.name;
      Adaview.Config.get_file_md5
        (doc_ctx.cur_doc.name,
         doc_ctx.cur_doc.temp_name,
         doc_ctx.cur_doc.checksum);
      Put_Line ("md5: " & doc_ctx.cur_doc.checksum);

      if (Argument_Count = 2) then
         doc_ctx.cur_doc.cur_page := Integer'Value (Argument (2));
      end if;
      -- try to see if we have the file in the history
      for i in 1 .. doc_ctx.total_doc loop
         if doc_ctx.cur_doc.checksum = doc_ctx.history (i).checksum then
            -- we found an entry
            Put_Line ("we got an entry in the history.");
            matched_idx := i;
            if doc_ctx.cur_doc.name /= doc_ctx.history (i).name then
               doc_ctx.history (i).name := doc_ctx.cur_doc.name;
               doc_ctx.cur_doc.class    := doc_ctx.history (i).class;
               if doc_ctx.cur_doc.cur_page = 0 then
                  doc_ctx.cur_doc.cur_page := doc_ctx.history (i).cur_page;
               end if;
               doc_ctx.cur_doc.total_page := doc_ctx.history (i).total_page;
               doc_ctx.history_changed    := True;
            end if;
            if i /= 1 then
               doc_ctx.history_changed := True;
            end if;
            exit;
         end if;
      end loop;
   else
      Put_Line ("No more argument");
   end if;

   if matched_idx = 0 then
      doc_ctx.history_changed := True;
   end if;

   if revision (gs_version'Access, gs_version'Size / 8) > 0 then
      Put_Line ("GS revision size not matching the ghostscript library.");
      return;
   end if;

   --   Gtk.Main.Init;
   Put (get_product (gs_version) & ", ");
   Put_Line (get_copyright (gs_version));
   Put ("Revision ");
   Put (get_revision_num (gs_version), 1);
   Put (" - ");
   Put (get_revision_date (gs_version), 1);
   New_Line;

   ret := new_instance (instance'Access, Null_Address);
   if ret < 0 then
      Put_Line ("call new instance failed.");
      Put ("Error code: ");
      Put (Integer (ret));
      New_Line;
   else
      Put_Line ("Got instance.");
   end if;

   Put_Line ("delete instance");
   delete_instance (instance);
   Adaview.Config.save_config (doc_ctx);
   if doc_ctx.cur_doc.name /= doc_ctx.cur_doc.temp_name then
      Delete_File (To_String (doc_ctx.cur_doc.temp_name));
   end if;
end Adaview_main;
