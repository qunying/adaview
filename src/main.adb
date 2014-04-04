-------------------------------------------------------------------------------
-- Adaview - A PostScript/PDF viewer based on ghostscript                    --
--                                                                           --
-- Copyright (c) 2014 Zhu Qun-Ying.                                          --
--                                                                           --
-- This program is free software; you can redistribute it and/or modify      --
-- it under the terms of the GNU General Public License as published by      --
-- the Free Software Foundation; either version 3 of the License, or         --
-- (at your option) any later version.                                       --
--                                                                           --
-- This program is distributed in the hope that it will be useful,           --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of            --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             --
-- GNU General Public License for more details.                              --
--                                                                           --
-- You should have received a copy of the GNU General Public License         --
-- along with this program; if not, see <http://www.gnu.org/licenses/>.      --
-------------------------------------------------------------------------------

with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Long_Integer_Text_IO; use Ada.Long_Integer_Text_IO;
with Ada.Integer_Text_IO;      use Ada.Integer_Text_IO;

with GS.API;       use GS.API;
with GS.Errors;    use GS.Errors;
with Interfaces.C; use Interfaces.C;
with System;       use System;

with Adaview.Version;

with Gtkada.Intl; use Gtkada.Intl;

procedure main is
   gs_version : aliased revision_t;
   instance   : aliased instance_t;
   ret        : Code_t;
begin
   Put_Line ("Adaview version " & Adaview.Version.Text);
   if revision (gs_version'Access, gs_version'Size / 8) > 0 then
      Put_Line ("GS revision size not matching the ghostscript library.");
      return;
   end if;

   Put (get_product (gs_version) & ", ");
   Put_Line (get_copyright (gs_version));
   Put ("Revision ");
   Put (get_revision_num (gs_version), 1);
   Put (" - ");
   Put (get_revision_date (gs_version), 1);
   New_Line;

   ret := new_instance (instance'Access, Null_Address);
   if ret < 0 then
      Put_Line ("call new instance failed.");
      Put ("Error code: ");
      Put (Integer (ret));
      New_Line;
   else
      Put_Line ("Got instance.");
   end if;

   Put_Line ("delete instance");
   delete_instance (instance);
end main;
