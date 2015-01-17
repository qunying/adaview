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

package body String_Format is

   procedure Increment (Num : in out Positive) is
   begin
      Num := Num + 1;
   end Increment;
   pragma Inline (Increment);

   ---------------------------------------------------------------------------
   function Format_String
     (Format   : String;
      Elements : UString_Array) return String
   is
      I              : Positive         := Format'First;
      Last_Digit_Pos : Positive;
      Idx            : Positive;
      Result         : Unbounded_String := +"";
      Esc_Char       : Character        := '%';
   begin
      if Elements = Null_UString_Array then
         raise No_Element;
      end if;

      loop
         -- check if we have %
         if Format (I) = Esc_Char and I < Format'Last then
            Last_Digit_Pos := I;
            for J in I + 1 .. Format'Last loop
               -- check numbers follow the % character
               if Character'Pos (Format (J)) >= Character'Pos ('0') and
                 Character'Pos (Format (J)) <= Character'Pos ('9')
               then
                  Last_Digit_Pos := J;
               else
                  exit;
               end if;
            end loop;
            if Last_Digit_Pos > I then
               Idx := Positive'Value (Format (I + 1 .. Last_Digit_Pos));
               if Idx > Elements'Last then
                  raise Invalid_Index
                    with "Max index of Elements is" &
                    Positive'Image (Elements'Last) &
                    ", found" &
                    Positive'Image (Idx) &
                    " in format string.";
               end if;
               Append (Result, Elements (Idx));
               I := Last_Digit_Pos;
            else -- only a single % is found, keep it
               Append (Result, Esc_Char);
            end if;
         else
            Append (Result, Format (I));
         end if;
         Increment (I);
         exit when I > Format'Last;
      end loop;

      return To_String (Result);
   end Format_String;

end String_Format;
