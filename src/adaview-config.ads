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

with Ada.Strings.Unbounded;

with GNAT.MD5;

package Adaview.Config is

   use Ada.Strings.Unbounded;

   MD5_Length                 : constant := 32;
   Max_File_Path_Length       : constant := 512;
   Max_Recent_Document_Number : constant := 10;
   -- store only 10 recent documents, may change to configuratble
   Parameter_Error : exception;
   No_temp_file : exception;
   Invalid_file : exception;

   type Byte_T is mod 2**8;
   type Byte_String_T is array (Positive range <>) of Byte_T;
   subtype Path_T is Unbounded_String;

   type Backend_T is (GHOSTSCRIPT, MUPDF);
   type Doc_Class_T is (UNKNOWN, PS, PDF);

   type Doc_T is record
      Name       : Path_T := To_Unbounded_String ("");
      Temp_Name  : Path_T := To_Unbounded_String ("");
      DCS_Name   : Path_T := To_Unbounded_String ("");
      Backend    : Backend_T := GHOSTSCRIPT;
      Checksum   : String (1 .. MD5_Length);
      Class      : Doc_Class_T   := UNKNOWN;
      Cur_Page   : Natural       := 0;
      Total_Page : Natural       := 0;
   end record;

   type Doc_History_T is array (Positive range <>) of Doc_T;

   type Context_T is record
      Config_File     : Path_T;
      Data_File       : Path_T;
      Cur_Doc         : Doc_T;
      History         : Doc_History_T (1 .. Max_Recent_Document_Number);
      Total_Doc       : Natural := 0;
      History_Changed : Boolean := False;
   end record;

   procedure Process_Options;
   -- Parse command line arguments.
   -- Raise exception Parameter_Error when arugment parsing failed

   procedure Get_File_MD5
     (File_Name : in     Unbounded_String;
      Temp_Name : in out Unbounded_String;
      Checksum  :    out String);
   -- Calculate the MD5 sum of a given file
   -- Auto decompress if it is compressed with compress/gzip/bzip2/xz and
   -- calculate the MD5 sum against the uncompressed file

   procedure Load_Config (ctx : in out Context_T);
   -- load configuration and recent histories

   procedure Save_Config (Ctx : in Context_T);

private
   procedure Load_History (Ctx : in out Context_T);
   procedure Save_History (Ctx : in Context_T);
end Adaview.Config;
