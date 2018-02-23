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

-- Define adaview's configuration and command line arguments

with GNATCOLL.Strings; use GNATCOLL.Strings;

package Adaview.Config is

   Max_File_Path_Length       : constant := 512;
   Max_Recent_Document_Number : constant := 10;

   Parameter_Error : exception;

   type Doc_History_T is array (Positive range <>) of Doc_T;

   type Context_T is record
      Config_File     : Path_T;
      Data_File       : Path_T;
      Cur_Doc         : Doc_T;
      Password        : XString;
      History         : Doc_History_T (1 .. Max_Recent_Document_Number);
      Total_Doc       : Natural := 0;
      History_Changed : Boolean := False;
      Respect_EOF     : Boolean := False;
      -- Derived from the scanstyle argument.
      -- If set to False EOF comments will be ignored,
      -- if set to True they will be taken seriously.
      -- Purpose; Out there are many documents which
      -- include other DSC conforming documents without
      -- without enclosing them by 'BeginDocument' and
      -- 'EndDocument' comments. This may cause fake EOF
      -- comments to appear in the body of a page.
      -- Similarly, if respect_eof is set to false
      -- 'Trailer' comments are ignored except of the
      -- last one found in the document.
   end record;

   procedure Process_Options (Ctx : in out Context_T);
   -- Parse command line arguments.
   -- Raise exception Parameter_Error when arugment parsing failed

   procedure Load_Config (ctx : in out Context_T);
   -- load configuration and recent histories

   procedure Save_Config (Ctx : in Context_T);
   -- save cofinguration and recent histories

   procedure Load_Medias;
   -- load medias definition from system wide/user defined or current directory
end Adaview.Config;
-- vim: set expandtab ts=3 sts=3 sw=3 smarttab :
