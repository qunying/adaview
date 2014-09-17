-------------------------------------------------------------------------------
-- Glib.Option.Extra - A child package to add g_option_context_parse()      --
-- Copyright (c) 2014, Zhu Qun-Ying.                                         --
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

pragma Ada_2005;

with Glib.Option;
with Glib.Error;

package Glib.Option.Extra is
   use Glib.Option;
   use Glib.Error;

   function Parse
     (Ctx   : Goption_Context;
      error : access GError) return Boolean;
   -- Parses the command line arguments, recognizing options which have been
   -- added to context.  A side-effect of calling this function is that
   -- g_set_prgname() will be called.
   --
   -- If the parsing is successful, any parsed arguments are removed from the
   -- array and argc and argv are updated accordingly.  A '--' option is
   -- stripped from argv unless there are unparsed options before and after it,
   -- or some of the options after it start with '-'.  In case of an error,
   -- argc and argv are left unmodified.
   --
   -- If automatic --help support is enabled (see Set_Help_Enabled), and the
   -- argv array contains one of the recognized help options, this function
   -- will produce help output to stdout and call exit (0).
   --
   -- Note that function depends on the current locale for automatic character
   -- set conversion of string and filename arguments.
   --
   -- context : GOptionContext
   -- error   : a return location for errors.
   -- Return  : Ture if the parsing was successful, False if an error occurred

end Glib.Option.Extra;
