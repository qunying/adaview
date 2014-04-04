-------------------------------------------------------------------------------
-- GhostScript API Ada binding                                               --
--                                                                           --
-- Copyright (c) 2014 Zhu Qun-Ying.                                          --
--                                                                           --
-- Public API for Ghostscript interpreter                                    --
-- Current problems:                                                         --
-- 1. Ghostscript does not support multiple instances.                       --
-- 2. Global variables in gs_main_instance_default()                         --
--    and gsapi_instance_counter                                             --
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

with Interfaces.C;
with Interfaces.C.Strings;
with System;

with GS.Errors;

-- Public API for Ghostscirpt interpreter from ghostscript/iapi.h
package GS.API is
   use Interfaces.C;
   use GS.Errors;

   type revision_t is limited private;

   function revision (pr : access revision_t; len : int) return Integer;
   pragma Import (C, revision, "gsapi_revision");
   -- Get version numbers and strings This is safe to call at any time. You
   -- should call this first to make sure that the correct version of the
   -- Ghostscript is being used. pr is a pointer to a revision structure. len
   -- is the size of this structure in bytes. Returns 0 if OK, or if len too
   -- small (additional parameters have been added to the structure) it will
   -- return the required size of the structure.

   function get_product (pr : revision_t) return String;
   pragma Inline (get_product);
   -- return product as Ada string

   function get_copyright (pr : revision_t) return String;
   pragma Inline (get_copyright);
   -- return copyright as Ada string

   function get_revision_num (pr : revision_t) return Long_Integer;
   pragma Inline (get_revision_num);
   -- return revision number

   function get_revision_date (pr : revision_t) return Long_Integer;
   pragma Inline (get_revision_date);
   -- return revision date

   type instance_t is limited private;

   function new_instance
     (pinstance      : access instance_t;
      caller_handler : System.Address)
      return           Code_t;
   pragma Import (C, new_instance, "gsapi_new_instance");
   -- Create a new instance of Ghostscript. This instance is passed to most
   -- other API functions. The caller_handle will be provided to callback
   -- functions.
   -- WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING
   --  Ghostscript supports only one instance.
   --  The current implementation uses a global static instance
   --  counter to make sure that only a single instance is used.
   --  If you try to create two instances, the second attempt
   --  will return < 0 and set pinstance to NULL.
   -- WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING

   procedure delete_instance (instance : instance_t);
   pragma Import (C, delete_instance, "gsapi_delete_instance");
   -- Destroy an instance of Ghostscript Before you call this, Ghostscript must
   -- have finished. If Ghostscript has been initialised, you must call
   -- gsapi.gsapi_exit() before gsapi_delete_instance.

   procedure gsapi_exit (instance : instance_t);
   pragma Import (C, gsapi_exit, "gsapi_exit");
   -- Exit the interpreter.
   -- This must be called on shutdown if gsapi.init_with_args()
   -- has been called, and just before gsapi.delete_instance().
   -- "gsapi_" is added as prefix to avoid the crash with Ada's keyword.

private
   type instance_t is new System.Address;

   type revision_t is limited record
      product      : Interfaces.C.Strings.chars_ptr;
      copyright    : Interfaces.C.Strings.chars_ptr;
      revision     : long;
      revisiondate : long;
   end record;
   pragma Convention (C_Pass_By_Copy, revision_t);
end GS.API;
