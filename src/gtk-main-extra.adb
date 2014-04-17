-------------------------------------------------------------------------------
-- Gtk.Main.Extra - A child package to add gtk_init_with_args()              --
-- Copyright (c) 2014, Zhu Qun-Ying.                                         --
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
      error              : access GError)
      return               Boolean is
      function Internal
        (argc         : System.Address;
         argv         : System.Address;
         param_str    : UTF8_String;
         opts         : GOption_Entry_Array;
         trans_domain : String;
         err          : access GError)
         return         Gboolean;
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
end Gtk.Main.Extra;
