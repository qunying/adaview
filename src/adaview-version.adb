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

with Gtkada.Intl; use Gtkada.Intl;

package body Adaview.Version is
   function get_description return Glib.UTF8_String is
   begin
      return -"A PostScript/PDF viewer.";
   end get_description;

   function get_copyright return Glib.UTF8_String is
   begin
      return -"Copyright (C) 2014 Zhu Qun-Ying";
   end get_copyright;

   function get_license return Glib.UTF8_String is
   begin
      return -("This program is free software released under the GPLv3 or latter.");
   end get_license;
end Adaview.Version;