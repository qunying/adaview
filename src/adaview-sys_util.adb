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

with Ada.Directories; use Ada.Directories;
with Ada.Streams.Stream_IO;
with System.Address_To_Access_Conversions;

with String_Format; use String_Format;

with Adaview.Debug;
with GNAT.MD5;

package body Adaview.Sys_Util is

   package Dbg renames Adaview.Debug;
   Cmd_Gzip  : aliased String := "gzip";
   Cmd_Bzip2 : aliased String := "bzip2";
   Cmd_Xz    : aliased String := "xz";

   type Byte_T is mod 2**8;
   type Byte_String_T is array (Positive range <>) of Byte_T;

   procedure Decompress_File
     (File_Name     : in     Unbounded_String;
      Temp_Name     : in out Unbounded_String;
      Compress_Type : in     Compress_T);

   ---------------------------------------------------------------------------
   function system (Arg : String) return Integer is
      function Sys (Arg : char_array) return Integer;
      pragma Import (C, Sys, "system");
   begin
      Dbg.Put_Line (Dbg.TRACE, "system(""" & Arg & """)");
      return Sys (To_C (Arg));
   end system;

   ---------------------------------------------------------------------------
   procedure mkstemp (filename : in out String) is
      function c_mkstemp (filename : char_array) return File_Descriptor;
      pragma Import (C, c_mkstemp, "mkstemp");
      Fd      : File_Descriptor;
      In_File : constant char_array := To_C (filename);
   begin
      Fd := c_mkstemp (In_File);
      if Fd = -1 then
         raise No_Temp_File;
      end if;
      Close (Fd);
      filename := To_Ada (In_File);
   end mkstemp;

   ---------------------------------------------------------------------------
   procedure Decompress_File
     (File_Name     : in     Unbounded_String;
      Temp_Name     : in out Unbounded_String;
      Compress_Type : in     Compress_T) is
      Base          : constant String := Base_Name (To_String (File_Name));
      Template_Name : String          := "/tmp/adaview_" & Base & ".XXXXXX";
      CMD_Ptr       : GNAT.OS_Lib.String_Access;
      Ret           : Integer;

   begin
      mkstemp (Template_Name);

      Dbg.Put_Line (Dbg.TRACE, "got temp file " & Template_Name);
      -- remove the trailling NUL character
      case Compress_Type is
         when COMPRESS | GZIP =>
            CMD_Ptr := Cmd_Gzip'Access;
         when BZIP2 =>
            CMD_Ptr := Cmd_Bzip2'Access;
         when XZ =>
            CMD_Ptr := Cmd_Xz'Access;
         when others =>
            null;
      end case;
      --!pp off
      Ret := system (CMD_Ptr.all & " -dc " & To_String (File_Name) & " > "
                     & Template_Name);
      --!pp on
      Dbg.Put_Line (Dbg.TRACE, "decompress result: " & Integer'Image (Ret));
      -- decompress file have zero length, consider failed.
      if Ret /= 0 or else Size (Template_Name) = 0 then
         Delete_File (Template_Name);
         raise Invalid_File
           with "file " & To_String (File_Name) & " is invalid.";
      end if;
      Temp_Name := +Template_Name;
   end Decompress_File;

   ---------------------------------------------------------------------------
   procedure Create_PDF_DSC_File
     (PDF_File : in     Unbounded_String;
      DSC_File : in out Unbounded_String;
      Password : in     Unbounded_String) is
      Base          : constant String := Base_Name (To_String (PDF_File));
      Template_Name : String          := "/tmp/adaview_" & Base & ".XXXXXX";
      Ret           : Integer;
   begin
      mkstemp (Template_Name);
      --!pp off
      if Length (Password) > 0 then
         Ret := system ("gs -P- -dSAFER -dDELAYSAFER -dNODISPLAY "
                        & "-dQUIET -sPDFname=" & To_String (PDF_File)
                        & " -sDSCname=" & Template_Name
                        & " pdf2dsc.ps -c quit -dsPDFPassword="
                        & To_String (Password));
      else
         Ret := system ("gs -P- -dSAFER -dDELAYSAFER -dNODISPLAY "
                        & "-dQUIET -sPDFname=" & To_String (PDF_File)
                        & " -sDSCname=" & Template_Name
                        & " pdf2dsc.ps -c quit");
      end if;
      --!pp on

      Dbg.Put_Line (Dbg.TRACE, "PDF to DSC result: " & Integer'Image (Ret));
      -- DSC file have zero length, consider failed.
      if Ret /= 0 or else Size (Template_Name) = 0 then
         Delete_File (Template_Name);
         raise Invalid_File with "Failed to create DSC for PDF file.";
      end if;
      DSC_File := +Template_Name;
   end Create_PDF_DSC_File;

   ---------------------------------------------------------------------------
   procedure Get_File_MD5
     (File_Name : in     Unbounded_String;
      Temp_Name : in out Unbounded_String;
      Checksum  :    out String) is
      use Ada.Streams;

      In_File         : Stream_IO.File_Type;
      Data_Block_Size : constant := 8_192;

      Data : Byte_String_T (1 .. Data_Block_Size);
      -- so that we could use it for compression magic header detection

      --!pp off
      Compress_Magic : constant Byte_String_T := (16#1F#, 16#9d#);
      Gzip_Magic     : constant Byte_String_T := (16#1F#, 16#8B#);
      Bzip2_Magic    : constant Byte_String_T := (Character'Pos ('B'),
                                                  Character'Pos ('Z'),
                                                  Character'Pos ('h'));
      Xz_Magic       : constant Byte_String_T := (16#FD#,
                                                  Character'Pos ('7'),
                                                  Character'Pos ('z'),
                                                  Character'Pos ('X'),
                                                  Character'Pos ('Z'),
                                                  16#00#);
      Compress_Type  : Compress_T := NO_COMPRESS;
      --!pp on

      subtype Sea_T is Stream_Element_Array (1 .. Data_Block_Size);
      package SEA_Addr is new Standard.System.Address_To_Access_Conversions
        (Sea_T);
      Into : constant SEA_Addr.Object_Pointer :=
        SEA_Addr.To_Pointer (Data'Address);
      Got     : Stream_Element_Offset;
      MD5_Ctx : GNAT.MD5.Context := GNAT.MD5.Initial_Context;
   begin
      Stream_IO.Open (In_File, Stream_IO.In_File, To_String (File_Name));
      -- read in sample first
      Stream_IO.Read (In_File, Into.all, Got);

      -- test for compression magic headers
      if Integer (Got) > Xz_Magic'Last then
         if Data (1 .. Compress_Magic'Last) = Compress_Magic then
            Compress_Type := COMPRESS;
         elsif Data (1 .. Gzip_Magic'Last) = Gzip_Magic then
            Compress_Type := GZIP;
         elsif Data (1 .. Bzip2_Magic'Last) = Bzip2_Magic then
            Compress_Type := BZIP2;
         elsif Data (1 .. Xz_Magic'Last) = Xz_Magic then
            Compress_Type := XZ;
         end if;
      end if;
      Dbg.Put_Line
        (Dbg.TRACE,
         "compression method " & Compress_T'Image (Compress_Type));
      if Compress_Type /= NO_COMPRESS then
         Stream_IO.Close (In_File);
         Decompress_File (File_Name, Temp_Name, Compress_Type);
         Stream_IO.Open (In_File, Stream_IO.In_File, To_String (Temp_Name));
         Stream_IO.Read (In_File, Into.all, Got);
      end if;
      -- Update MD5
      GNAT.MD5.Update (MD5_Ctx, Into.all (1 .. Got));

      while not Ada.Streams.Stream_IO.End_Of_File (In_File) loop
         Stream_IO.Read (In_File, Into.all, Got);
         -- Update MD5
         GNAT.MD5.Update (MD5_Ctx, Into.all (1 .. Got));
      end loop;
      Stream_IO.Close (In_File);
      Checksum := GNAT.MD5.Digest (MD5_Ctx);
   end Get_File_MD5;

   ---------------------------------------------------------------------------
   procedure Increment (Num : in out Integer) is
   begin
      Num := Num + 1;
   end Increment;

   ---------------------------------------------------------------------------
   procedure Increment (Num : in out Integer; Step : in Integer) is
   begin
      Num := Num + Step;
   end Increment;

   ---------------------------------------------------------------------------
   procedure Increment (Num : in out Unsigned_64; Step : in Integer) is
   begin
      Num := Num + Unsigned_64 (Step);
   end Increment;

   ---------------------------------------------------------------------------
   procedure Decrement (Num : in out Integer) is
   begin
      Num := Num - 1;
   end Decrement;
end Adaview.Sys_Util;
