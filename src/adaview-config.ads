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

-- Define adaview's configuration and command line arguments

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Adaview.Sys_Util; use Adaview.Sys_Util;

package Adaview.Config is

   Max_File_Path_Length       : constant := 512;
   Max_Recent_Document_Number : constant := 10;

   Parameter_Error : exception;

   subtype Path_T is Unbounded_String;

   type Uint64_T is mod 2 ** 64;

   type Doc_T is record
      Name       : Path_T     := To_Unbounded_String ("");
      Temp_Name  : Path_T     := To_Unbounded_String ("");
      DCS_Name   : Path_T     := To_Unbounded_String ("");
      Backend    : Backend_T  := Ghostscript;
      Kind       : Doc_Kind_T := UNKNOWN_FILE;
      Cur_Page   : Natural    := 0;
      Total_Page : Natural    := 0;
      Header_Pos : Uint64_T   := 0;
      Checksum   : String (1 .. MD5_Length);
   end record;

   type Doc_History_T is array (Positive range <>) of Doc_T;

   type Context_T is record
      Config_File     : Path_T;
      Data_File       : Path_T;
      Cur_Doc         : Doc_T;
      Password        : Unbounded_String;
      History         : Doc_History_T (1 .. Max_Recent_Document_Number);
      Total_Doc       : Natural := 0;
      History_Changed : Boolean := False;
   end record;

   procedure Process_Options (Ctx : in out Context_T);
   -- Parse command line arguments.
   -- Raise exception Parameter_Error when arugment parsing failed

   procedure Load_Config (ctx : in out Context_T);
   -- load configuration and recent histories

   procedure Save_Config (Ctx : in Context_T);

end Adaview.Config;
