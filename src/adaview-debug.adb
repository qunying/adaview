-------------------------------------------------------------------------------
-- Adaview - A PostScript/PDF viewer based on ghostscript                    --
--                                                                           --
-- Copyright (c) 2014-2015 Zhu Qun-Ying.                                     --
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

package body Adaview.Debug is
   Cur_Flag : Flag := None;

   ---------------------------------------------------------------------------
   procedure Set_Flag (In_Flag : Flag) is
   begin
      Cur_Flag := In_Flag;
   end Set_Flag;

   ---------------------------------------------------------------------------
   function Get_Flag return Flag is
   begin
      return Cur_Flag;
   end Get_Flag;

   ---------------------------------------------------------------------------
   procedure Put (In_Flag : Flag; Msg : String) is
   begin
      if Cur_Flag /= None and then Cur_Flag >= In_Flag then
         Ada.Text_IO.Put (Msg);
      end if;
   end Put;

   ---------------------------------------------------------------------------
   procedure Put_Line (In_Flag : Flag; Msg : String) is
   begin
      if Cur_Flag /= None and then Cur_Flag >= In_Flag then
         Ada.Text_IO.Put_Line (Msg);
      end if;
   end Put_Line;

   ---------------------------------------------------------------------------
   procedure New_Line
     (In_Flag : Flag;
      Count   : Ada.Text_IO.Positive_Count := 1) is
   begin
      if Cur_Flag /= None and then Cur_Flag >= In_Flag then
         Ada.Text_IO.New_Line (Count);
      end if;
   end New_Line;
end Adaview.Debug;
