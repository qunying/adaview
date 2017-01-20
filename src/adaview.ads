-------------------------------------------------------------------------------
-- Adaview - A PostScript/PDF viewer based on ghostscript                    --
--                                                                           --
-- Copyright (c) 2014-2017 Zhu Qun-Ying.                                     --
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
with Interfaces;            use Interfaces;
with Ada.Containers.Vectors;

package Adaview is

   type Backend_T is (GHOSTSCRIPT, MUPDF);
   type Doc_Kind_T is (UNKNOWN_FILE, PS_FILE, EPSF_FILE, PDF_FILE);
   type Bounding_Box_T is array (1 .. 4) of Integer;

   type Bounding_Box_Set_T is (ATEND, NONE, SET);
   type Orientation_T is (ATEND, NONE, PORTRAIT, LANDSCAPE, SEASCAPE);
   type Page_Order_T is (ATEND, NONE, ASCEND, DESCEND, SPECIAL);

   MD5_Length : constant := 32;

   subtype Path_T is Unbounded_String;

   type Media_T is record
      Name          : Unbounded_String := Null_Unbounded_String;
      Width, Height : Integer;
      Used          : Integer          := 0;
   end record;

   type Page_T is record
      Label : Unbounded_String := Null_Unbounded_String;
      Bounding_Box : Bounding_Box_T;
      Orientation  : Orientation_T;
      Start, The_End, Len : Unsigned_64;
   end record;

   package Media_Vector is new Ada.Containers.Vectors
     (Element_Type => Media_T,
      Index_Type   => Positive);

   package Page_Vector is new Ada.Containers.Vectors
     (Element_Type => Page_T,
      Index_Type   => Positive);

   type Doc_T is record
      Name                     : Path_T           := Null_Unbounded_String;
      Temp_Name                : Path_T           := Null_Unbounded_String;
      DCS_Name                 : Path_T           := Null_Unbounded_String;
      Backend                  : Backend_T        := GHOSTSCRIPT;
      Kind                     : Doc_Kind_T       := UNKNOWN_FILE;
      Cur_Page                 : Natural          := 0;
      Total_Page               : Natural          := 0;
      Header_Pos               : Unsigned_64      := 0;
      Title                    : Unbounded_String := Null_Unbounded_String;
      Creation_Date            : Unbounded_String := Null_Unbounded_String;
      Date                     : Unbounded_String := Null_Unbounded_String;
      Bounding_Box             : Bounding_Box_T;
      Page_Bounding_Box        : Bounding_Box_T;
      Orientation              : Orientation_T    := NONE;
      Page_Order               : Page_Order_T     := NONE;
      Default_Page_Orientation : Orientation_T    := NONE;
      Checksum                 : String (1 .. MD5_Length);
      Media                    : Media_Vector.Vector;
      Default_Page_Media       : Media_T;
      Page                     : Page_Vector.Vector;
   end record;

   type Media_Array_T is array (Positive range <>) of Media_T;

   Medias : Media_Vector.Vector;

end Adaview;
-- vim: set expandtab ts=3 sts=3 sw=3 smarttab :
