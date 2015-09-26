-------------------------------------------------------------------------------
-- Adaview - A PostScript/PDF viewer based on ghostscript                    --
--                                                                           --
-- Copyright (c) 2015 Zhu Qun-Ying.                                          --
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
with Interfaces.C; use Interfaces.C;
with GNAT.OS_Lib;  use GNAT.OS_Lib;
with Adaview.Debug;

package body Adaview.Sys_Util is

   package Dbg renames Adaview.Debug;

   function system (Arg : String) return Integer is
      function Sys (Arg : char_array) return Integer;
      pragma Import (C, Sys, "system");
   begin
      Dbg.Put_Line (Dbg.Trace, "system(""" & Arg & """)");
      return Sys (To_C (Arg));
   end system;

   procedure mkstemp (filename : in out String) is
      function c_mkstemp (filename : char_array) return File_Descriptor;
      pragma Import (C, c_mkstemp, "mkstemp");
      Fd : File_Descriptor;
      In_File : constant char_array := To_C (filename);
   begin
      Fd := c_mkstemp (In_File);
      if Fd = -1 then
         raise No_Temp_File;
      end if;
      Close (Fd);
      filename := To_Ada (In_File);
   end mkstemp;
end Adaview.Sys_Util;
