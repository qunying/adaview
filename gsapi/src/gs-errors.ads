-------------------------------------------------------------------------------
-- GhostScript API Ada binding                                               --
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

-- error definitions from ghostscript/ierrors.h

package GS.Errors is

   type Code_T is new Integer;

   OK : constant Code_T := 0;

   -- definition of error codes, PostScript Level 1 errors
   E_Unknownerror       : constant Code_T := -1;
   E_Dictfull           : constant Code_T := -2;
   E_Dictstackoverflow  : constant Code_T := -3;
   E_Dictstackunderflow : constant Code_T := -4;
   E_Execstackoverflow  : constant Code_T := -5;
   E_Interrupt          : constant Code_T := -6;
   E_Invalidaccess      : constant Code_T := -7;
   E_Invalidexit        : constant Code_T := -8;
   E_Invalidfileaccess  : constant Code_T := -9;
   E_Invalidfont        : constant Code_T := -10;
   E_Invalidrestore     : constant Code_T := -11;
   E_Ioerror            : constant Code_T := -12;
   E_Limitcheck         : constant Code_T := -13;
   E_Nocurrentpoint     : constant Code_T := -14;
   E_Rangecheck         : constant Code_T := -15;
   E_Stackoverflow      : constant Code_T := -16;
   E_Stackunderflow     : constant Code_T := -17;
   E_Syntaxerror        : constant Code_T := -18;
   E_Timeout            : constant Code_T := -19;
   E_Typecheck          : constant Code_T := -20;
   E_Undefined          : constant Code_T := -21;
   E_Undefinedfilename  : constant Code_T := -22;
   E_Undefinedresult    : constant Code_T := -23;
   E_Unmatchedmark      : constant Code_T := -24;
   E_Vmerror            : constant Code_T := -25;
   -- E_Vmerror must be the last Level 1 error

   ------ Additional Level 2 errors (also in DPS)
   E_Configurationerror : constant Code_T := -26;
   E_Undefinedresource  : constant Code_T := -27;
   E_Unregistered       : constant Code_T := -28;

   ------ Additional DPS errors ------
   E_Invalidcontext : constant Code_T := -29;
   E_Invalidid      : constant Code_T := -30;
   -- invalidid is for the NeXT DPS extension.

   -- Pseudo-errors used internally

   E_Hit_Detected : constant Code_T := -99;
   E_Fatal        : constant Code_T := -100;

   E_Quit : constant Code_T := -101;
   -- Internal code for the .quit operator. The real quit code is an integer on
   -- the operand stack. gs_interpret returns this only for a .quit with a zero
   -- exit code.

   E_Interpreterexit : constant Code_T := -102;
   -- Internal code for a normal exit from the interpreter. Do not use outside
   -- of interp.c.

   E_RemapColor : constant Code_T := -103;
   -- Need the remap color error for high level pattern support

   E_ExecStackUnderflow : constant Code_T := -104;
   -- Internal code to indicate we have underflowed the top block of the
   -- e-stack.

   E_VMreclaim : constant Code_T := -105;
   -- Internal code for the vmreclaim operator with a positive operand. We need
   -- to handle this as an error because otherwise the interpreter won't reload
   -- enough of its state when the operator returns.

   E_NeedInput : constant Code_T := -106;
   -- Internal code for requesting more input from run_string.

   E_Info : constant Code_T := -110;
   -- Internal code for a normal exit when usage info is displayed. This allows
   -- Window versions of Ghostscript to pause until the message can be read.

   E_Handled : constant Code_T := -111;
-- A special 'error', like remap color above. This is used by a subclassing
-- device to indicate that it has fully processed a device method, and parent
-- subclasses should not perform any further action. Currently this is
-- limited to compositor creation.

   function Is_Interrupt (E_Code : Code_T) return Boolean;
   pragma Inline (Is_Interrupt);
   -- Define which error codes require re-executing the current object
end GS.Errors;
-- vim: set expandtab ts=3 sts=3 sw=3 smarttab :
