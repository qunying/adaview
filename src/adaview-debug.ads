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

with Ada.Text_IO;

package Adaview.Debug is

   type Flag is (NONE, ERROR, WARNING, INFO, TRACE);

   procedure Init;

   procedure Set_Flag (In_Flag : Flag);

   function Get_Flag return Flag;

   procedure Put (In_Flag : Flag; Msg : String);

   procedure Put_Line (In_Flag : Flag; Msg : String);

   procedure New_Line;
   pragma Inline (New_Line);

   procedure New_Line (In_Flag : Flag;
                       Count   : Ada.Text_IO.Positive_Count := 1);
end Adaview.Debug;
-- vim: set expandtab ts=3 sts=3 sw=3 smarttab :
