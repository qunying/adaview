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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Adaview.Sys_Util is

   MD5_Length                 : constant := 32;

   No_Temp_File : exception;
   Invalid_File : exception;

   function system (Arg : String) return Integer;
   -- thin binding to C's system function.

   procedure mkstemp (filename : in out String);
   -- thin binding around C's mkstemp function, except the return FD is closed.

   type Compress_T is (NO_COMPRESS, COMPRESS, GZIP, BZIP2, XZ);

   procedure Get_File_MD5
     (File_Name : in     Unbounded_String;
      Temp_Name : in out Unbounded_String;
      Checksum  :    out String);
   -- Calculate the MD5 sum of a given file
   -- Auto decompress if it is compressed with compress/gzip/bzip2/xz and
   -- calculate the MD5 sum against the uncompressed file

   procedure Create_PDF_DSC_File (PDF_File : in Unbounded_String;
                                  DSC_File : in out Unbounded_String;
                                  Password : in Unbounded_String);

end Adaview.Sys_Util;
