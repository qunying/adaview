-------------------------------------------------------------------------------
-- gsapi - Ghostscript API Ada binding                                       --
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

with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings;
with System;

package GSAPI is

   -- definition of error codes, from ierrors.h PostScript Level 1 errors
   e_unknownerror       : constant := -1;
   e_dictfull           : constant := -2;
   e_dictstackoverflow  : constant := -3;
   e_dictstackunderflow : constant := -4;
   e_execstackoverflow  : constant := -5;
   e_interrupt          : constant := -6;
   e_invalidaccess      : constant := -7;
   e_invalidexit        : constant := -8;
   e_invalidfileaccess  : constant := -9;
   e_invalidfont        : constant := -10;
   e_invalidrestore     : constant := -11;
   e_ioerror            : constant := -12;
   e_limitcheck         : constant := -13;
   e_nocurrentpoint     : constant := -14;
   e_rangecheck         : constant := -15;
   e_stackoverflow      : constant := -16;
   e_stackunderflow     : constant := -17;
   e_syntaxerror        : constant := -18;
   e_timeout            : constant := -19;
   e_typecheck          : constant := -20;
   e_undefined          : constant := -21;
   e_undefinedfilename  : constant := -22;
   e_undefinedresult    : constant := -23;
   e_unmatchedmark      : constant := -24;
   e_VMerror            : constant := -25; -- must be the last Level 1 error

   -- ---- Additional Level 2 errors (also in DPS)
   e_configurationerror : constant := -26;
   e_undefinedresource  : constant := -27;
   e_unregistered       : constant := -28;

   ------ Additional DPS errors ------
   e_invalidcontext : constant := -29;
   e_invalidid      : constant := -30;  -- invalidid is for the NeXT DPS
   --extension.

   -- Pseudo-errors used internally

   -- Internal code for a fatal error
   -- gs_interpret also returns this for a .quit with a positive exit code.
   e_Fatal : constant := -100;

   -- Internal code for the .quit operator. The real quit code is an integer on
   -- the operand stack. gs_interpret returns this only for a .quit with a zero
   -- exit code.
   e_Quit : constant := -101;

   -- Internal code for a normal exit from the interpreter. Do not use outside
   -- of interp.c.
   e_InterpreterExit : constant := -102;

   -- Internal code that indicates that a procedure has been stored in the
   -- remap_proc of the graphics state, and should be called before retrying
   -- the current token. This is used for color remapping involving a call back
   -- into the interpreter -- inelegant, but effective.
   e_RemapColor : constant := -103;

   -- Internal code to indicate we have underflowed the top block of the
   -- e-stack.
   e_ExecStackUnderflow : constant := -104;

   -- Internal code for the vmreclaim operator with a positive operand. We need
   -- to handle this as an error because otherwise the interpreter won't reload
   -- enough of its state when the operator returns.
   e_VMreclaim : constant := -105;

   -- Internal code for requesting more input from run_string.
   e_NeedInput : constant := -106;

   -- Internal code for a normal exit when usage info is displayed. This allows
   -- Window versions of Ghostscript to pause until the message can be read.
   e_Info : constant := -110;

   type revision_t is limited private;

   -- Get version numbers and strings This is safe to call at any time. You
   -- should call this first to make sure that the correct version of the
   -- Ghostscript is being used. pr is a pointer to a revision structure. len
   -- is the size of this structure in bytes. Returns 0 if OK, or if len too
   -- small (additional parameters have been added to the structure) it will
   -- return the required size of the structure.
   function revision (pr : access revision_t; len : int) return Integer;

   -- return product as Ada string
   function get_product (pr : revision_t) return String;
   pragma Inline (get_product);

   -- return copyright as Ada string
   function get_copyright (pr : revision_t) return String;
   pragma Inline (get_copyright);

   -- return revision number
   function get_revision_num (pr : revision_t) return Long_Integer;
   pragma Inline (get_revision_num);

   -- return revision date
   function get_revision_date (pr : revision_t) return Long_Integer;
   pragma Inline (get_revision_date);

   type instance_t is limited private;

   -- Create a new instance of Ghostscript. This instance is passed to most
   -- other API functions. The caller_handle will be provided to callback
   -- functions.
   function new_instance
     (pinstance      : access instance_t;
      caller_handler : System.Address)
      return           Integer;
   pragma Import (C, new_instance, "gsapi_new_instance");

   -- Destroy an instance of Ghostscript Before you call this, Ghostscript must
   -- have finished. If Ghostscript has been initialised, you must call
   -- gsapi.exit() before gsapi_delete_instance.
   procedure delete_instance (instance : instance_t);
   pragma Import (C, delete_instance, "gsapi_delete_instance");

private
   type instance_t is new System.Address;

   type revision_t is limited record
      product      : Interfaces.C.Strings.chars_ptr;
      copyright    : Interfaces.C.Strings.chars_ptr;
      revision     : long;
      revisiondate : long;
   end record;
   pragma Convention (C_Pass_By_Copy, revision_t);
end GSAPI;
