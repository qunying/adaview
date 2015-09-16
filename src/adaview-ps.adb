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

with Ada.Characters.Latin_1;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNATCOLL.Mmap;         use GNATCOLL.Mmap;

with Adaview.Debug;
package body Adaview.PS is

   type File_Data_T is record
      File         : Mapped_File;
      Size         : File_Size;
      Page_Size    : Integer;

      Str     : Str_Access;
      Str_End : Integer   := 0;
      Offs    : File_Size := 0;

      Line_Begin : Integer := 0;
      Line_End   : Integer := 0;
      Line_Len   : Integer := 0;
      Status     : Boolean := True; -- True for OK, False for failed
   end record;

   package ACL renames Ada.Characters.Latin_1;
   package Dbg renames Adaview.Debug;

   procedure IO_Init (File_Name : String; FD : in out File_Data_T);
   function Read_Line (FD : in out File_Data_T) return Boolean;
   function Get_Chars (FD : in out File_Data_T; Num : Integer) return Boolean;

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
   procedure IO_Init (File_Name : String; FD : in out File_Data_T) is
   begin
      FD.File         := Open_Read (File_Name);
      FD.Size         := Length (FD.File);
      FD.Page_Size    := Get_Page_Size;
      FD.Request_Size := 2 * FD.Page_Size;
   end IO_Init;

   ---------------------------------------------------------------------------
   function Read_Line (FD : in out File_Data_T) return Boolean is
      Ret : Boolean;
   begin
      Ret := Get_Chars (FD, -1);
      return Ret;
   end Read_Line;

   ---------------------------------------------------------------------------
   function Get_Chars
     (FD  : in out File_Data_T;
      Num :        Integer) return Boolean is
      I : Integer;
   begin
      if FD.Status = False then
         return False;
      end if;

      FD.Line_Begin := FD.Line_End;

      outter :
      loop
         if Num < 0 then -- reading whole line
            if FD.Str_End - FD.Line_End > 0 then
               I := FD.Line_End + 1;
               loop
                  exit when I > Last (FD.File)
                    or else (FD.Str (I) = ACL.LF or FD.Str (I) = ACL.CR);
                  I := I + 1;
               end loop;
               if I <= FD.Str_End then
                  if I < FD.Str_End
                    and then FD.Str (I) = ACL.CR
                    and then FD.Str (I + 1) = ACL.LF
                  then
                     I := I + 2;
                  else
                     I := I + 1;
                  end if;
                  FD.Line_End := I;
                  exit outter;
               end if;
            end if;
         else -- reading specified Num of chars
            if FD.Str_End >= FD.Line_Begin + Num then
               FD.Line_End := FD.Line_Begin + Num;
               exit outter;
            end if;
         end if;

         -- no end of line yet

      end loop outter;
      return True;
   end Get_Chars;
end Adaview.PS;
