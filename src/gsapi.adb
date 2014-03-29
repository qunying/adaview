-------------------------------------------------------------------------------
-- gsapi - Ghostscript API Ada binding                                       --
--                                                                           --
-- Copyright (c) 2014 Zhu Qun-Ying.                                          --
--
-- * Public API for Ghostscript interpreter                                  --
-- * Current problems:                                                       --
-- * 1. Ghostscript does not support multiple instances.                     --
-- * 2. Global variables in gs_main_instance_default()                       --
-- *    and gsapi_instance_counter                                           --
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

package body GSAPI is

   function revision (pr : access revision_t; len : int) return Integer is
      function gsapi_revision
        (pr   : access revision_t;
         len  : int)
         return int;
      pragma Import (C, gsapi_revision, "gsapi_revision");
   begin
      return Integer (gsapi_revision (pr, len));
   end revision;

   function get_product (pr : revision_t) return String is
   begin
      return Interfaces.C.Strings.Value (pr.product);
   end get_product;

   function get_copyright (pr : revision_t) return String is
   begin
      return Interfaces.C.Strings.Value (pr.copyright);
   end get_copyright;

   function get_revision_num (pr : revision_t) return Long_Integer is
   begin
      return Long_Integer (pr.revision);
   end get_revision_num;

   function get_revision_date (pr : revision_t) return Long_Integer is
   begin
      return Long_Integer (pr.revisiondate);
   end get_revision_date;

end GSAPI;
