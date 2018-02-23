-------------------------------------------------------------------------------
-- Gtk.Main.Extra - A child package to add gtk_init_with_args()              --
--                  and gtk_get_option_group ()                              --
-- Copyright (c) 2014-2018 Zhu Qun-Ying.                                       --
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

with System;
with Interfaces.C;

package body Gtk.Main.Extra is

   gnat_argc : Interfaces.C.int;
   pragma Import (C, gnat_argc);

   gnat_argv : System.Address;
   pragma Import (C, gnat_argv);

   function Init_With_Args
     (parameter_string   : UTF8_String;
      entries            : GOption_Entry_Array;
      translation_domain : String;
      error              : access GError) return Boolean is
      function Internal
        (argc         : System.Address;
         argv         : System.Address;
         param_str    : UTF8_String;
         opts         : GOption_Entry_Array;
         trans_domain : String;
         err          : access GError) return Gboolean;
      pragma Import (C, Internal, "gtk_init_with_args");
   begin
      return Boolean'Val
          (Internal
             (gnat_argc'Address,
              gnat_argv'Address,
              parameter_string & ASCII.NUL,
              entries,
              translation_domain & ASCII.NUL,
              error));
   end Init_With_Args;

   function Get_Option_Group
     (Open_Default_Display : Boolean := Standard.True) return GOption_Group is
      function Internal (default_open : Gboolean) return GOption_Group;
      pragma Import (C, Internal, "gtk_get_option_group");
   begin
      return Internal (Boolean'Pos (Open_Default_Display));
   end Get_Option_Group;

end Gtk.Main.Extra;
-- vim: set expandtab ts=3 sts=3 sw=3 smarttab :
