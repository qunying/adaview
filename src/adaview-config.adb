-------------------------------------------------------------------------------
-- Adaview - A PostScript/PDF viewer based on ghostscript                    --
--                                                                           --
-- Copyright (c) 2014 Zhu Qun-Ying.                                          --
--                                                                           --
-- This program is free software; you can redistribute it and/or modify      --
-- it under the terms of the GNU General Public License as published by      --
-- the Free Software Foundation; either version 3 of the License, or         --
-- (at your option) any later version.                                       --
--                                                                           --
-- This program is distributed in the hope that it will be useful,           --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of            --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             --
-- GNU General Public License for more details.                              --
--                                                                           --
-- You should have received a copy of the GNU General Public License         --
-- along with this program; if not, see <http://www.gnu.org/licenses/>.      --
-------------------------------------------------------------------------------
with Ada.Text_IO;          use Ada.Text_IO;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with Glib.Option;    use Glib.Option;
with Gtk.Main.Extra; use Gtk.Main.Extra;
with Gtkada.Intl;    use Gtkada.Intl;
with Glib.Error;     use Glib.Error;
with Glib;           use Glib;

with Adaview.Version; use Adaview.Version;

with System;
with Ada.Unchecked_Conversion;

package body Adaview.Config is
   Opts         : GOption_Entry_Array (1 .. 2);
   Show_Version : aliased Gboolean;
   type gboolean_access is access all Gboolean;

   function To_Address (C : gboolean_access) return System.Address is
      function Convert is new Ada.Unchecked_Conversion (
         gboolean_access,
         Glib.C_Proxy);
   begin
      return Glib.To_Address (Convert (C));
   end To_Address;

   procedure print_version is
   begin
      Put (prgname & " " & Adaview.Version.Text & " - ");
      Put_Line (get_description);
      Put_Line (get_copyright);
      Put_Line (get_license);
   end print_version;

   function process_options return Boolean is
      ret   : Boolean;
      error : aliased GError;
   begin
      -- fill in the option
      Opts (1).Long_Name   := New_String ("version");
      Opts (1).Short_Name  := 'v';
      Opts (1).Description :=
         New_String (-"Display version information");
      Opts (1).Arg         := G_Option_Arg_None;
      Opts (1).Arg_Data    := To_Address (Show_Version'Access);

      ret := Init_With_Args (get_description, Opts, prgname, error'Access);
      if (ret = False) then
         Put_Line (-"Error: " & Get_Message (error));
         return False;
      end if;
      if Show_Version /= 0 then
         print_version;
         return False;
      end if;

      return True;
   end process_options;
end Adaview.Config;
