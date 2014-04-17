-------------------------------------------------------------------------------
-- Adaview - A PostScript/PDF viewer based on ghostscript                    --
--                                                                           --
-- Copyright (c) 2014 Zhu Qun-Ying.                                          --
--                                                                           --
-- Adaview is free software; you can redistribute it and/or modify      --
-- it under the terms of the GNU General Public License as published by      --
-- the Free Software Foundation; either version 3 of the License, or         --
-- (at your option) any later version.                                       --
--                                                                           --
-- Adaview is distributed in the hope that it will be useful,           --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of            --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             --
-- GNU General Public License for more details.                              --
--                                                                           --
-- You should have received a copy of the GNU General Public License         --
-- along with this program; if not, see <http://www.gnu.org/licenses/>.      --
-------------------------------------------------------------------------------

with Gtkada.Intl; use Gtkada.Intl;
with Ada.Characters.Latin_1;

package body Adaview.Version is

   LF : Character := Ada.Characters.Latin_1.LF;

   function get_description return  Glib.UTF8_String is
   begin
      return -"A PostScript/PDF viewer.";
   end get_description;

   function get_copyright return  Glib.UTF8_String is
   begin
      return -"Copyright (C) 2014 Zhu Qun-Ying";
   end get_copyright;

   function get_license return  Glib.UTF8_String is
   begin
      return -(
"Adaview is free software; you can redistribute it and/or modify" & LF &
"it under the terms of the GNU General Public License as published by" & LF &
"the Free Software Foundation; either version 3 of the License, or" & LF &
"(at your option) any later version." & LF &
LF &
"Adaview is distributed in the hope that it will be useful," & LF &
"but WITHOUT ANY WARRANTY; without even the implied warranty of" & LF &
"MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the" & LF &
"GNU General Public License for more details." & LF &
LF &
"You should have received a copy of the GNU General Public License" & LF &
"along with this program; if not, see <http://www.gnu.org/licenses/>." & LF);
   end get_license;
end Adaview.Version;
