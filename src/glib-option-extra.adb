-------------------------------------------------------------------------------
-- Glib.Option.Extra - A child package to add g_option_context_parse()       --
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

package body Glib.Option.Extra is
   gnat_argc : Interfaces.C.int;
   pragma Import (C, gnat_argc);

   gnat_argv : System.Address;
   pragma Import (C, gnat_argv);

   function Parse
     (Ctx   : Goption_Context;
      error : access GError) return Boolean is
      function Internal
        (context : System.Address;
         argc    : System.Address;
         argv    : System.Address;
         err     : access GError) return Gboolean;
      pragma Import (C, Internal, "g_option_context_parse");
   begin
      return Boolean'Val
          (Internal
             (Get_Object (Ctx),
              gnat_argc'Address,
              gnat_argv'Address,
              error));
   end Parse;

end Glib.Option.Extra;
