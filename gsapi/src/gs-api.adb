-------------------------------------------------------------------------------
-- GhostScript API Ada binding                                               --
--                                                                           --
-- Copyright (c) 2014-2015 Zhu Qun-Ying.                                     --
--                                                                           --
-- * Public API for Ghostscript interpreter                                  --
-- * Current problems:                                                       --
-- * 1. Ghostscript does not support multiple instances.                     --
-- * 2. Global variables in gs_main_instance_default()                       --
-- *    and gsapi_instance_counter                                           --
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

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
use Ada.Strings;

package body GS.API is

   ---------------------------------------------------------------------------
   function Get_Product (Rev : Revision_T) return String is
   begin
      return Interfaces.C.Strings.Value (Rev.Product);
   end Get_Product;

   ---------------------------------------------------------------------------
   function Get_Copyright (Rev : Revision_T) return String is
   begin
      return Interfaces.C.Strings.Value (Rev.Copyright);
   end Get_Copyright;

   ---------------------------------------------------------------------------
   function Get_Revision_Num (Rev : Revision_T) return Long_Integer is
   begin
      return Long_Integer (Rev.Revision);
   end Get_Revision_Num;

   ---------------------------------------------------------------------------
   function Get_Revision_Num_String (Rev : Revision_T) return String is
      Major : Long_Integer := Long_Integer (Rev.Revision) / 100;
      Minor : Long_Integer := Long_Integer (Rev.Revision) rem 100;
   begin
      if Minor < 10 then
         return Trim (Long_Integer'Image (Major), Left) &
           ".0" &
           Trim (Long_Integer'Image (Minor), Left);
      else
         return Trim (Long_Integer'Image (Major), Left) &
           "." &
           Trim (Long_Integer'Image (Minor), Left);
      end if;
   end Get_Revision_Num_String;

   ---------------------------------------------------------------------------
   function Get_Revision_Date (Rev : Revision_T) return Long_Integer is
   begin
      return Long_Integer (Rev.Revision_Date);
   end Get_Revision_Date;

   function Get_Revision_Date_String (Rev : Revision_T) return String is
      Year  : Long_Integer := Long_Integer (Rev.Revision_Date) / 10_000;
      Month : Long_Integer :=
        Long_Integer (Rev.Revision_Date) rem 10_000 / 100;
      Date : Long_Integer := Long_Integer (Rev.Revision_Date) - Year * 10_000 - Month * 100;
   begin
      return Trim (Long_Integer'Image (Year), Left) &
        "-" &
        Trim (Long_Integer'Image (Month), Left) &
        "-" &
        Trim (Long_Integer'Image (Date), Left);
   end Get_Revision_Date_String;
end GS.API;
