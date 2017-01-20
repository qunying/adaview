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

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings;       use Ada.Strings;
with Gtkada.Intl;       use Gtkada.Intl;

package body String_Format is

   ---------------------------------------------------------------------------
   function Format_String
     (Format   : String;
      Elements : UString_Array) return String is
      Last_Digit_Pos : Positive;
      Idx            : Positive;
      K              : Positive           := Format'First;
      Result         : Unbounded_String   := +"";
      Esc_Char       : constant Character := '%';
   begin
      if Elements = Null_UString_Array then
         raise No_Element with -"Elements array is empty.";
      end if;

      loop
         -- check if we have %
         if Format (K) = Esc_Char and K < Format'Last then
            Last_Digit_Pos := K;
            for J in K + 1 .. Format'Last loop
               -- check numbers follow the % character
               if Character'Pos (Format (J)) >= Character'Pos ('0') and
                 Character'Pos (Format (J)) <= Character'Pos ('9')
               then
                  Last_Digit_Pos := J;
               else
                  exit;
               end if;
            end loop;
            if Last_Digit_Pos > K then
               Idx := Positive'Value (Format (K + 1 .. Last_Digit_Pos));
               if Idx > Elements'Last then
                  raise Invalid_Index
                    with Fmt
                      (-
                       ("Format string has %1, larger than maximum " &
                        "index %2 of Elements."),
                       (+Format (K .. Last_Digit_Pos),
                        +Trim (Positive'Image (Elements'Last), Left)));
               end if;
               Append (Result, Elements (Idx));
               K := Last_Digit_Pos;
            else
               Append (Result, Esc_Char);
               if Format (K + 1) = '%' then -- found %%, output one only
                  K := K + 1;
               end if;
            end if;
         else
            Append (Result, Format (K));
         end if;
         K := K + 1;
         exit when K > Format'Last;
      end loop;

      return To_String (Result);
   end Format_String;

end String_Format;
-- vim: set expandtab ts=3 sts=3 sw=3 smarttab :
