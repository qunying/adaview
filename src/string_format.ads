-------------------------------------------------------------------------------
-- Adaview - A PostScript/PDF viewer based on ghostscript                    --
--                                                                           --
-- Copyright (c) 2015-2018 Zhu Qun-Ying.                                          --
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

with GNATCOLL.Strings;

package String_Format is

   use GNATCOLL.Strings;

   No_Element    : exception;
   Invalid_Index : exception;

   Null_XString_Array : XString_Array (2 .. 1);

   function "+" (S : String) return XString renames To_XString;

   function Format_String
     (Format   : String;
      Elements : XString_Array) return String;
   -- Format a string using position holder %1 %2 and the correspoding
   -- Element in the Elements. To output % character, if no digit follow the
   -- %, then % will be output, othewise %% is needed to escape %.
   --
   -- When Elements is null, exception No_Element will be raised
   -- When position holder's index is large than Elements'Last,
   -- exception Invalid_Index will be raised.

   function Fmt
     (Format   : String;
      Elements : XString_Array) return String renames
     Format_String;

end String_Format;
-- vim: set expandtab ts=3 sts=3 sw=3 smarttab :
