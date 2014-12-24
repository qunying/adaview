-------------------------------------------------------------------------------
-- Adaview - A PostScript/PDF viewer based on ghostscript                    --
--                                                                           --
-- Copyright (c) 2014 Zhu Qun-Ying.                                          --
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

with Ada.Strings.Bounded;

with GNAT.MD5;

package Adaview.Config is

   md5_length                 : constant := 32;
   max_file_path_length       : constant := 512;
   max_recent_document_number : constant := 10;
   -- store only 10 recent documents, may change to configuratble
   Parameter_Error : exception;
   No_temp_file : exception;

   type byte_t is mod 2**8;
   type byte_string_t is array (Positive range <>) of byte_t;

   package BString is new Ada.Strings.Bounded.Generic_Bounded_Length
     (Max => max_file_path_length);
   subtype path_string_t is BString.Bounded_String;
   use BString;

   type doc_class_t is (UNKNOWN, PS, PDF);

   type doc_t is record
      name       : path_string_t := To_Bounded_String("");
      temp_name  : path_string_t := To_Bounded_String("");
      checksum   : String (1 .. md5_length);
      class      : doc_class_t := UNKNOWN;
      cur_page   : Natural := 0;
      total_page : Natural := 0;
   end record;

   type doc_history_t is array (Positive range <>) of doc_t;

   type context_t is record
      config_file     : path_string_t;
      data_file       : path_string_t;
      current_doc     : doc_t;
      history         : doc_history_t (1 .. max_recent_document_number);
      total_doc       : Natural := 0;
      history_changed : Boolean := False;
   end record;

   procedure process_options;
   -- Parse command line arguments.
   -- Raise exception Parameter_Error when arugment parsing failed

   procedure get_file_md5 (file_name : in Bounded_String;
                           temp_name : in out Bounded_String;
                           checksum  : out String);
   -- Calculate the MD5 sum of a given file
   -- Auto decompress if it is compressed with compress/gzip/bzip2/xz and
   -- calculate the MD5 sum against the uncompressed file

   procedure load_config (ctx : in out context_t);
   -- load configuration and recent histories

   procedure save_config (ctx : in context_t);

private
   procedure load_history (ctx : in out context_t);
   procedure save_history (ctx : in context_t);
end Adaview.Config;
