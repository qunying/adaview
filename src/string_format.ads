-------------------------------------------------------------------------------
-- Adaview - A PostScript/PDF viewer based on ghostscript                    --
--                                                                           --
-- Copyright (c) 2015-2017 Zhu Qun-Ying.                                          --
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

with Ada.Strings.Unbounded;

package String_Format is
   use Ada.Strings.Unbounded;

   type UString_Array is array (Positive range <>) of Unbounded_String;
   Null_UString_Array : UString_Array (2 .. 1);

   No_Element    : exception;
   Invalid_Index : exception;

   function "+" (S : String) return Unbounded_String renames
     To_Unbounded_String;

   function Format_String
     (Format   : String;
      Elements : UString_Array) return String;
   -- Format a string using position holder %1 %2 and the correspoding
   -- Element in the Elements. To output % character, if no digit follow the
   -- %, then % will be output, othewise %% is needed to escape %.
   --
   -- When Elements is null, exception No_Element will be raised
   -- When position holder's index is large than Elements'Last,
   -- exception Invalid_Index will be raised.

   function Fmt
     (Format   : String;
      Elements : UString_Array) return String renames
     Format_String;

end String_Format;
-- vim: set expandtab ts=3 sts=3 sw=3 smarttab :
