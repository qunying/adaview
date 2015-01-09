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

with Ada.Strings.Fixed;
with Glib;

package Adaview.Version is
   use Ada.Strings.Fixed;

   Major : constant                  := 0;
   Minor : constant                  := 0;
   Date  : constant                  := 20140328;
   Text  : constant Glib.UTF8_String :=
     Trim (Integer'Image (Major), Ada.Strings.Left) &
     "." &
     Trim (Integer'Image (Minor), Ada.Strings.Left) &
     Integer'Image (Date);

   Prg_Name : constant String := "adaview";

   function Get_Description return Glib.UTF8_String;
   -- return program description message

   function Get_Copyright return Glib.UTF8_String;
   -- return program copyright information

   function Get_License return Glib.UTF8_String;
   -- return program license information
end Adaview.Version;
