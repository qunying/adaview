-------------------------------------------------------------------------------
-- GhostScript API Ada binding                                               --
--                                                                           --
-- Copyright (c) 2014-2015 Zhu Qun-Ying.                                     --
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

   type Revision_T is limited private;

   function Revision (Rev : access Revision_T; Len : int) return Integer;
   pragma Import (C, Revision, "gsapi_revision");
   -- Get version numbers and strings This is safe to call at any time. You
   -- should call this first to make sure that the correct version of the
   -- Ghostscript is being used. pr is a pointer to a revision structure. len
   -- is the size of this structure in bytes. Returns 0 if OK, or if len too
   -- small (additional parameters have been added to the structure) it will
   -- return the required size of the structure.

   function Get_Product (Rev : Revision_T) return String;
   pragma Inline (Get_Product);
   -- return product as Ada string

   function Get_Copyright (Rev : Revision_T) return String;
   pragma Inline (Get_Copyright);
   -- return copyright as Ada string

   function Get_Revision_Num (Rev : Revision_T) return Long_Integer;
   pragma Inline (Get_Revision_Num);
   -- return revision number

   function Get_Revision_Num_String (Rev : Revision_T) return String;
   pragma Inline (Get_Revision_Num_String);
   -- return major revision in string format

   function Get_Revision_Date (Rev : Revision_T) return Long_Integer;
   pragma Inline (Get_Revision_Date);
   -- return revision date

   type Instance_T is limited private;

   function New_Instance
     (Instance_Ptr   : access Instance_T;
      Caller_Handler : System.Address) return Code_T;
   pragma Import (C, New_Instance, "gsapi_new_instance");
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

   procedure Delete_Instance (instance : Instance_T);
   pragma Import (C, Delete_Instance, "gsapi_delete_instance");
   -- Destroy an instance of Ghostscript Before you call this, Ghostscript must
   -- have finished. If Ghostscript has been initialised, you must call
   -- gsapi.gsapi_exit() before gsapi_Delete_Instance.

   type Std_Cb_Fn_T is access function
     (Caller_Handle : System.Address;
      Buf           : chars_ptr;
      Len           : int) return int;
   pragma Convention (C, Std_Cb_Fn_T);
   function Set_Stdio
     (Instance  : Instance_T;
      Stdin_Fn  : Std_Cb_Fn_T;
      Stdout_Fn : Std_Cb_Fn_T;
      Stderr_Fn : Std_Cb_Fn_T) return Code_T;
   pragma Import (C, Set_Stdio, "gsapi_set_stdio");
   -- Set the callback functions for stdio
   -- The stdin callback function should return the number of
   -- characters read, 0 for EOF, or -1 for error.
   -- The stdout and stderr callback functions should return
   -- the number of characters written.
   -- If a callback address is NULL, the real stdio will be used.

   type Poll_Cb_Fn_T is access function
     (Caller_Handle : System.Address) return int;
   pragma Convention (C, Poll_Cb_Fn_T);
   function Set_Poll
     (Instance : Instance_T;
      Poll_Fn  : Poll_Cb_Fn_T) return Code_T;
   pragma Import (C, Set_Poll, "gsapi_set_poll");
   -- Set the callback function for polling.
   -- This is used for handling window events or cooperative
   -- multitasking.  This function will only be called if
   -- Ghostscript was compiled with CHECK_INTERRUPTS
   -- as described in gpcheck.h.
   -- The polling function should return 0 if all is well,
   -- and negative if it wants ghostscript to abort.
   -- The polling function must be fast.

   function Set_Display_Callback
     (Instance : Instance_T;
      Callback : access Display_Callback_T) return Code_T;
   pragma Import (C, Set_Display_Callback, "gsapi_set_display_callback");
   -- Set the display device callback structure.
   -- If the display device is used, this must be called
   -- after GS.API.New_Instance() and before GS.API.init_with_args().
   -- See gdevdisp.h for more details.

   function Init_With_Args
     (Instance : Instance_T;
      Argc     : int;
      Argv     : chars_ptr_array) return Code_T;
   pragma Import (C, Init_With_Args, "gsapi_init_with_args");
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

   function Run_String_Begin
     (Instance    : Instance_T;
      User_Errors : int;
      Pexit_Code  : access int) return Code_T;
   pragma Import (C, Run_String_Begin, "gsapi_run_string_begin");

   function Run_String_Continue
     (Instance    : Instance_T;
      Str         : chars_ptr;
      Length      : unsigned;
      User_Errors : int;
      Pexit_Code  : access int) return Code_T;
   pragma Import (C, Run_String_Continue, "gsapi_run_string_continue");

   function Run_String_End
     (Instance    : Instance_T;
      User_Errors : int;
      Pexit_Code  : access int) return Code_T;
   pragma Import (C, Run_String_End, "gsapi_run_string_end");

   function Run_String_With_Length
     (Instance    : Instance_T;
      Str         : chars_ptr;
      Length      : unsigned;
      User_Errors : int;
      Pexit_Code  : access int) return Code_T;
   pragma Import (C, Run_String_With_Length, "gsapi_run_string_with_length");

   function Run_String
     (Instance    : Instance_T;
      Str         : chars_ptr;
      User_Errors : int;
      Pexit_Code  : access int) return Code_T;
   pragma Import (C, Run_String, "gsapi_run_string");

   function Run_File
     (Instance    : Instance_T;
      File_Name   : chars_ptr;
      User_Errors : int;
      Pexit_Code  : access int) return Code_T;
   pragma Import (C, Run_File, "gsapi_run_file");

   procedure Gsapi_Exit (instance : Instance_T);
   pragma Import (C, Gsapi_Exit, "gsapi_exit");
   -- Exit the interpreter.
   -- This must be called on shutdown if gsapi.init_with_args()
   -- has been called, and just before gsapi.Delete_Instance().
   -- "gsapi_" is added as prefix to avoid the crash with Ada's keyword.

   -- no gsapi_set_visual_tracer for now
private
   type Instance_T is new System.Address;

   type Revision_T is limited record
      Product       : chars_ptr;
      Copyright     : chars_ptr;
      Revision      : long;
      Revision_Date : long;
   end record;
   pragma Convention (C_Pass_By_Copy, Revision_T);
end GS.API;
