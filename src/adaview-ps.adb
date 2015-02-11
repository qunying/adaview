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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Directories;

with GNATCOLL.Mmap; use GNATCOLL.Mmap;

package body Adaview.PS is

   type File_Data_T is record
      File : Mapped_File;
      File_Size : Ada.Directories.File_Size;
      Str  : Str_Access;
      Offs : Long_Integer := 0;

      Line_Begin : Long_Integer := 0;
      Line_End   : Long_Integer := 0;
      Line_Len   : Long_Integer := 0;
      Status     : Boolean := True; -- True for OK, False for failed
   end record;

   procedure IO_Init (File_Name : String; File : in out File_Data_T);
   function Read_Line (File : File_Data_T) return Boolean;

   ---------------------------------------------------------------------------
   procedure Scan (Ctx : in out Context_T) is
      File : File_Data_T;
   begin
      if Length (Ctx.Cur_Doc.DCS_Name) > 0 then
         -- use the DCS file created from previous run
         IO_Init (To_String (Ctx.Cur_Doc.DCS_Name), File);
      else
         IO_Init (To_String (Ctx.Cur_Doc.Temp_Name), File);
      end if;

      if Read_Line (File) then
         null;
      end if;

      Close (File.File);
   end Scan;

   ---------------------------------------------------------------------------
   procedure IO_Init (File_Name : String; File : in out File_Data_T) is
   begin
      File.File := Open_Read (File_Name);
      File.File_Size := Ada.Directories.Size (File_Name);
   end IO_Init;

   ---------------------------------------------------------------------------
   function Read_Line (File : File_Data_T) return Boolean is
      Ret : Boolean := False;
   begin
      return Ret;
   end Read_Line;

end Adaview.PS;
