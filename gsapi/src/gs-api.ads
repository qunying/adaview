-------------------------------------------------------------------------------
-- GhostScript API Ada binding                                               --
--                                                                           --
-- Copyright (c) 2014-2017 Zhu Qun-Ying.                                     --
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

   ARG_ENCODING_LOCAL   : constant := 0;
   ARG_ENCODING_UTF8    : constant := 1;
   ARG_ENCODING_UTF16LE : constant := 2;

   function Revision (Rev : access Revision_T; Len : int) return Integer;
   pragma Import (C, Revision, "gsapi_revision");
   -- This function returns the revision numbers and strings of the
   -- Ghostscript interpreter library;
   -- This is safe to call at any time.  You should call this first to make
   -- sure that the correct version of the Ghostscript is being used.
   -- Rev is a pointer to a revision record.
   -- len is the size of this record in bytes.
   -- Returns 0 if OK, or if len too small (additional parameters
   -- have been added to the structure) it will return the required
   -- size of the structure.

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
   -- return major revision in string format

   function Get_Revision_Date (Rev : Revision_T) return Long_Integer;
   pragma Inline (Get_Revision_Date);
   -- return revision date

   function Get_Revision_Date_String (Rev : Revision_T) return String;
   -- return revision date in string

   type Instance_T is limited private;

   function New_Instance
     (Instance_Ptr   : access Instance_T;
      Caller_Handler : System.Address) return Code_T;
   pragma Import (C, New_Instance, "gsapi_new_instance");
   -- Create a new instance of Ghostscript.
   -- This instance is passed to most other API functions.
   -- The caller_handle will be provided to callback functions.
   -- WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING
   --  Ghostscript supports only one instance.
   --  The current implementation uses a global static instance
   --  counter to make sure that only a single instance is used.
   --  If you try to create two instances, the second attempt
   --  will return < 0 and set Instance_Ptr to NULL.
   -- WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING

   procedure Delete_Instance (instance : Instance_T);
   pragma Import (C, Delete_Instance, "gsapi_delete_instance");
   -- Destroy an instance of Ghostscript.
   -- Before you call this, Ghostscript must have finished.
   -- If Ghostscript has been initialised, you must call
   -- GS.API.GSAPI_Exit() before GS.API.Delete_Instance.

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
   -- This is used for handling window events or cooperative multitasking.
   -- This function will only be called if Ghostscript was compiled with
   -- CHECK_INTERRUPTS as described in gpcheck.h.
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

   function Set_Default_Device_List
      (Instance : Instance_T;
      List      : chars_ptr;
      List_Len  : int) return Code_T;
   pragma Import (C, Set_Default_Device_List, "gsapi_set_default_device_list");
   -- Set the string containing the list of default device names
   -- for example "display x11alpha x11 bbox". Allows the calling
   -- application to influence which device(s) gs will try in order
   -- to select the default device
   --
   -- *Must* be called after GS.API.New_Instance() and before
   -- GS.API.Init_With_Args().

   function Get_Default_Devlice_List
      (Instance : Instance_T;
      List      : chars_ptr_array;
      List_Len  : out int) return Code_T;
   pragma Import (C, Get_Default_Devlice_List,
   "gsapi_get_default_device_list");
   -- Returns a pointer to the current default device string
   -- *Must* be called after GS.API.New_Instance()

   function Set_Arg_Encoding
     (Instance : Instance_T;
      Encoding : int) return Code_T;
   pragma Import (C, Set_Arg_Encoding, "gsapi_set_arg_encoding");
   -- Set the encoding used for the args. By default we assume
   -- 'local' encoding. For windows this equates to whatever the current
   -- codepage is. For linux this is utf8.
   --
   -- Use of this API (GS.API) with 'local' encodings (and hence without
   -- calling this function) is now deprecated!

   function Init_With_Args
     (Instance : Instance_T;
      Argc     : int;
      Argv     : chars_ptr_array) return Code_T;
   pragma Import (C, Init_With_Args, "gsapi_init_with_args");
   -- Initialise the interpreter.
   -- This calls gs_main_init_with_args() in imainarg.c
   -- 1. If quit or EOF occur during gsapi_init_with_args(),
   --    the return value will be e_Quit.  This is not an error.
   --    You must call GSAPI_Exit() and must not call any other
   --    gsapi_XXX functions.
   -- 2. If usage info should be displayed, the return value will be e_Info
   --    which is not an error.  Do not call GSAPI_Exit().
   -- 3. Under normal conditions this returns 0.  You would then
   --    call one or more gsapi_run_*() functions and then finish
   --    with GSAPI_Exit().

   -- The gsapi_run_* functions are like gs_main_run_* except
   -- that the error_object is omitted.
   -- If these functions return <= -100, either quit or a fatal
   -- error has occured.  You then call GSAPI_Exit() next.
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

   procedure GSAPI_Exit (instance : Instance_T);
   pragma Import (C, GSAPI_Exit, "gsapi_exit");
   -- Exit the interpreter.
   -- This must be called on shutdown if GS.API.Init_With_Args()
   -- has been called, and just before GS.API.Delete_Instance().
   -- "GSAPI_" is added as prefix to avoid the crash with Ada's keyword exit

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
-- vim: set expandtab ts=3 sts=3 sw=3 smarttab :
