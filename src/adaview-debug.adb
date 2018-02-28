-------------------------------------------------------------------------------
-- Adaview - A PostScript/PDF viewer based on ghostscript                    --
--                                                                           --
-- Copyright (c) 2014-2018 Zhu Qun-Ying.                                     --
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

with Ada.Text_IO.Text_Streams; use Ada.Text_IO.Text_Streams;
with Ada.Characters; use Ada.Characters;
with Ada.Characters.Latin_1;

package body Adaview.Debug is

   Cur_Flag : Flag := NONE;
   Std_Out : Stream_Access;

   ---------------------------------------------------------------------------
   procedure Init is
   begin
      Std_Out := Stream(Ada.TexT_IO.Standard_Output);
   end Init;

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
      if Cur_Flag /= NONE and then Cur_Flag >= In_Flag then
         String'Write(Std_Out, Msg);
      end if;
   end Put;

   ---------------------------------------------------------------------------
   procedure Put_Line (In_Flag : Flag; Msg : String) is
   begin
      if Cur_Flag /= NONE and then Cur_Flag >= In_Flag then
         String'Write (Std_Out, Msg);
         New_Line;
      end if;
   end Put_Line;

   procedure New_Line is
   begin
      Character'Write(Std_Out, Latin_1.LF);
   end New_Line;

   ---------------------------------------------------------------------------
   procedure New_Line (In_Flag : Flag;
                       Count   : Ada.Text_IO.Positive_Count := 1) is
   begin
      if Cur_Flag /= NONE and then Cur_Flag >= In_Flag then
         for I in Ada.Text_IO.Positive_Count range 1 .. Count loop
            New_Line;
         end loop;
      end if;
   end New_Line;

end Adaview.Debug;
-- vim: set expandtab ts=3 sts=3 sw=3 smarttab :
