-------------------------------------------------------------------------------
-- gsapi - Ghostscript API Ada binding                                       --
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

-- error definitions from ghostscript/ierrors.h
package GSAPI.Errors is
   -- definition of error codes, PostScript Level 1 errors
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

   -- Define which error codes require re-executing the current object
   function is_interrupt (ecode : Integer) return Boolean;
   pragma Inline (is_interrupt);
end GSAPI.Errors;
