-------------------------------------------------------------------------------
-- Adaview - A PostScript/PDF viewer based on ghostscript                    --
--                                                                           --
-- Copyright (c) 2015-2018 Zhu Qun-Ying.                                     --
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

package Adaview.Sys_Util is

   No_Temp_File : exception;
   Invalid_File : exception;

   function system (Arg : String) return Integer;
   -- thin binding to C's system function.

   procedure mkstemp (filename : in out String);
   -- thin binding around C's mkstemp function, except the return FD is closed.

   type Compress_T is (NO_COMPRESS, COMPRESS, GZIP, BZIP2, XZ, ZSTD, LZIP);

   procedure Get_File_MD5 (File_Name : in     XString;
                           Temp_Name : in out XString;
                           Checksum  :    out String);
   -- Calculate the MD5 sum of a given file.
   -- Auto decompress if it is compressed with compress/gzip/bzip2/xz and
   -- calculate the MD5 sum against the uncompressed file.

   procedure Create_PDF_DSC_File (PDF_File : in     XString;
                                  DSC_File : in out XString;
                                  Password : in     XString);

   procedure Increment (Num : in out Integer);
   pragma Inline (Increment);

   procedure Increment (Num : in out Integer; Step : in Integer);
   pragma Inline (Increment);

   procedure Increment (Num : in out Unsigned_64; Step : in Integer);
   pragma Inline (Increment);

   procedure Decrement (Num : in out Integer);
   pragma Inline (Decrement);
end Adaview.Sys_Util;
-- vim: set expandtab ts=3 sts=3 sw=3 smarttab :
