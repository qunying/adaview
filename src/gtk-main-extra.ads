-------------------------------------------------------------------------------
-- Gtk.Main.Extra - A child package to add gtk_init_with_args()              --
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

pragma Ada_2005;

with Glib.Option;
with Glib.Error;

package Gtk.Main.Extra is
   use Glib.Option;
   use Glib.Error;

   function Init_With_Args
     (parameter_string   : UTF8_String;
      entries            : GOption_Entry_Array;
      translation_domain : String;
      error              : access GError) return Boolean;
   -- This function does the same work as Init_Check. Additionally, it
   -- allows you to add your own commandline options, and it automatically
   -- generates nicesly formatted --help output. Note that your program will
   -- be terminated after writering out the help output.
   -- parameter_string   : A string which is displayed in the first line of
   --                      --help output, after programname [OPTION...].
   -- entries            : A NULL-terminated array of GOptionsEntrys describing
   --                      the options of your program.
   -- translation_domain : A translation domain to use for translating the
   --                      --help output for the options in entries and
   --                      the parameter_string with gettext() or Empty_String
   -- error              : a return location for errors
   --
   -- Return             : True if the windowing system has been successfully
   --                      initialized, False otherwise.

   function Get_Option_Group
     (Open_Default_Display : Boolean := Standard.True) return GOption_Group;
   -- Returns a GOptionGroup for the commandline arguments recognized by GTK+
   -- and GDK.
   --
   -- You should add this group to your GOption_Context with Add_Group,
   -- if you are using Glib.Option.Extra.Parse to parse your commandline
   -- arguments.
   -- Open_Default_Display : whether to open the default display when parsing
   --                        the commandline arguments.
   --                        Default is true.

end Gtk.Main.Extra;
-- vim: set expandtab ts=3 sts=3 sw=3 smarttab :
