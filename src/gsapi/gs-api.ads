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

with Interfaces.C;
with Interfaces.C.Strings;
with System;
with GS.Display_Device;
with GS.Errors;

-- Public API for Ghostscirpt interpreter from ghostscript/iapi.h
package GS.API is
   use Interfaces.C;
   use Interfaces.C.Strings;
   use GS.Display_Device;
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
      return           code_t;
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

   type std_cb_fn_t is access function
     (caller_handle : System.Address;
      buf           : chars_ptr;
      len           : int)
      return          int;
   pragma Convention (C, std_cb_fn_t);
   function set_stdio
     (instance  : instance_t;
      stdin_fn  : std_cb_fn_t;
      stdout_fn : std_cb_fn_t;
      stderr_fn : std_cb_fn_t)
      return      code_t;
   pragma Import (C, set_stdio, "gsapi_set_stdio");
   -- Set the callback functions for stdio
   -- The stdin callback function should return the number of
   -- characters read, 0 for EOF, or -1 for error.
   -- The stdout and stderr callback functions should return
   -- the number of characters written.
   -- If a callback address is NULL, the real stdio will be used.

   type poll_cb_fn_t is access function
     (caller_handle : System.Address)
      return          int;
   pragma Convention (C, poll_cb_fn_t);
   function set_poll
     (instance : instance_t;
      poll_fn  : poll_cb_fn_t)
      return     code_t;
   pragma Import (C, set_poll, "gsapi_set_poll");
   -- Set the callback function for polling.
   -- This is used for handling window events or cooperative
   -- multitasking.  This function will only be called if
   -- Ghostscript was compiled with CHECK_INTERRUPTS
   -- as described in gpcheck.h.
   -- The polling function should return 0 if all is well,
   -- and negative if it wants ghostscript to abort.
   -- The polling function must be fast.

   function set_display_callback
     (instance : instance_t;
      callback : access display_callback_t)
      return     code_t;
   pragma Import (C, set_display_callback, "gsapi_set_display_callback");
   -- Set the display device callback structure.
   -- If the display device is used, this must be called
   -- after GS.API.new_instance() and before GS.API.init_with_args().
   -- See gdevdisp.h for more details.

   function init_with_args
     (instance : instance_t;
      argc     : int;
      argv     : chars_ptr_array)
      return     code_t;
   pragma Import (C, init_with_args, "gsapi_init_with_args");
   -- Initialise the interpreter.
   -- This calls gs_main_init_with_args() in imainarg.c
   -- 1. If quit or EOF occur during gsapi_init_with_args(),
   --    the return value will be e_Quit.  This is not an error.
   --    You must call gsapi_exit() and must not call any other
   --    gsapi_XXX functions.
   -- 2. If usage info should be displayed, the return value will be e_Info
   --    which is not an error.  Do not call gsapi_exit().
   -- 3. Under normal conditions this returns 0.  You would then
   --    call one or more gsapi_run_*() functions and then finish
   --    with gsapi_exit().

   -- The gsapi_run_* functions are like gs_main_run_* except
   -- that the error_object is omitted.
   -- If these functions return <= -100, either quit or a fatal
   -- error has occured.  You then call gsapi_exit() next.
   -- The only exception is run_string_continue()
   -- which will return e_NeedInput if all is well.

   function run_string_begin
     (instance    : instance_t;
      user_errors : int;
      pexit_code  : access int)
      return        code_t;
   pragma Import (C, run_string_begin, "gsapi_run_string_begin");

   function run_string_continue
     (instance    : instance_t;
      str         : chars_ptr;
      length      : unsigned;
      user_errors : int;
      pexit_code  : access int)
      return        code_t;
   pragma Import (C, run_string_continue, "gsapi_run_string_continue");

   function run_string_end
     (instance    : instance_t;
      user_errors : int;
      pexit_code  : access int)
      return        code_t;
   pragma Import (C, run_string_end, "gsapi_run_string_end");

   function run_string_with_length
     (instance    : instance_t;
      str         : chars_ptr;
      length      : unsigned;
      user_errors : int;
      pexit_code  : access int)
      return        code_t;
   pragma Import (C, run_string_with_length, "gsapi_run_string_with_length");

   function run_string
     (instance    : instance_t;
      str         : chars_ptr;
      user_errors : int;
      pexit_code  : access int)
      return        code_t;
   pragma Import (C, run_string, "gsapi_run_string");

   function run_file
     (instance    : instance_t;
      file_name   : chars_ptr;
      user_errors : int;
      pexit_code  : access int)
      return        code_t;
   pragma Import (C, run_file, "gsapi_run_file");

   procedure gsapi_exit (instance : instance_t);
   pragma Import (C, gsapi_exit, "gsapi_exit");
   -- Exit the interpreter.
   -- This must be called on shutdown if gsapi.init_with_args()
   -- has been called, and just before gsapi.delete_instance().
   -- "gsapi_" is added as prefix to avoid the crash with Ada's keyword.

   -- no gsapi_set_visual_tracer for now
private
   type instance_t is new System.Address;

   type revision_t is limited record
      product      : chars_ptr;
      copyright    : chars_ptr;
      revision     : long;
      revisiondate : long;
   end record;
   pragma Convention (C_Pass_By_Copy, revision_t);
end GS.API;
