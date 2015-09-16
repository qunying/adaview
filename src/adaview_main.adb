-------------------------------------------------------------------------------
-- Adaview - A PostScript/PDF viewer based on ghostscript                    --
--                                                                           --
-- Copyright (c) 2014-2015 Zhu Qun-Ying.                                     --
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

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Command_Line;      use Ada.Command_Line;
with Ada.Directories;       use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Interfaces.C; use Interfaces.C;
with System;       use System;

with POSIX.Process_Environment; use POSIX.Process_Environment;

with GS.API;    use GS.API;
with GS.Errors; use GS.Errors;

with Gtkada.Intl; use Gtkada.Intl;

with Adaview.Version;
with Adaview.Config;
with Adaview.Locale;
with Adaview.Debug;
--with Adaview.PS;

procedure Adaview_main is
   GS_Rev      : aliased Revision_T;
   Instance    : aliased Instance_T;
   Ret         : Code_T;
   Doc_Ctx     : Adaview.Config.Context_T;
   Matched_Idx : Natural := 0;

   package Dbg renames Adaview.Debug;

begin
   Setlocale;
   Text_Domain (Adaview.Version.Prg_Name);
   Bind_Text_Domain (Adaview.Version.Prg_Name, Adaview.Locale.Path);

   Dbg.Set_Flag (Dbg.Trace);

   Adaview.Config.Process_Options;

   -- we only use the first unknow argument as file name
   if Argument_Count > 2 then
      Put_Line ("Too many arguments.");
      Set_Exit_Status (Failure);
      return;
   end if;

   Adaview.Config.Load_Config (Doc_Ctx);

   if Argument_Count >= 1 then
      if Argument (1) (1) = '/' then
         Doc_Ctx.Cur_Doc.Name := To_Unbounded_String (Argument (1));
      else
         declare
            cwd : constant String := POSIX.To_String (Get_Working_Directory);
         begin
            Doc_Ctx.Cur_Doc.Name :=
              To_Unbounded_String (cwd & "/" & Argument (1));
         end;
      end if;

      Dbg.Put_Line
        (Dbg.Trace,
         "Got document: " & To_String (Doc_Ctx.Cur_Doc.Name));
      Doc_Ctx.Cur_Doc.Temp_Name := Doc_Ctx.Cur_Doc.Name;
      Adaview.Config.Get_File_MD5
        (Doc_Ctx.Cur_Doc.Name,
         Doc_Ctx.Cur_Doc.Temp_Name,
         Doc_Ctx.Cur_Doc.Checksum);
      Dbg.Put_Line (Dbg.Trace, "md5: " & Doc_Ctx.Cur_Doc.Checksum);

      if Argument_Count = 2 then
         Doc_Ctx.Cur_Doc.Cur_Page := Integer'Value (Argument (2));
      end if;
      -- try to see if we have the file in the history
      for i in 1 .. Doc_Ctx.Total_Doc loop
         if Doc_Ctx.Cur_Doc.Checksum = Doc_Ctx.History (i).Checksum then
            -- we found an entry
            Dbg.Put_Line (Dbg.Trace, "we got an entry in the history.");
            Matched_Idx := i;
            if Doc_Ctx.Cur_Doc.Name /= Doc_Ctx.History (i).Name then
               Doc_Ctx.History (i).Name := Doc_Ctx.Cur_Doc.Name;
               Doc_Ctx.Cur_Doc.Kind     := Doc_Ctx.History (i).Kind;
               if Doc_Ctx.Cur_Doc.Cur_Page = 0 then
                  Doc_Ctx.Cur_Doc.Cur_Page := Doc_Ctx.History (i).Cur_Page;
               end if;
               Doc_Ctx.Cur_Doc.Total_Page := Doc_Ctx.History (i).Total_Page;
               Doc_Ctx.History_Changed    := True;
            end if;
            if i /= 1 then
               Doc_Ctx.History_Changed := True;
            end if;
            exit;
         end if;
      end loop;
   else
      Dbg.Put_Line (Dbg.Trace, "No more argument");
   end if;

   if Matched_Idx = 0 and Argument_Count > 0 then
      Doc_Ctx.History_Changed := True;
   end if;

--   Adaview.PS.Scan (Doc_Ctx);

   if Revision (GS_Rev'Access, GS_Rev'Size / 8) > 0 then
      Put_Line ("GS revision size not matching the ghostscript library.");
      return;
   end if;

   --   Gtk.Main.Init;
   Dbg.Put (Dbg.Trace, Get_Product (GS_Rev) & ", ");
   Dbg.Put_Line (Dbg.Trace, Get_Copyright (GS_Rev));
   Dbg.Put (Dbg.Trace, "Version " & Get_Revision_Num_String (GS_Rev));
   Dbg.Put_Line (Dbg.Trace, " (" & Get_Revision_Date_String (GS_Rev) & ")");

   Ret := New_Instance (Instance'Access, Null_Address);
   if Ret < 0 then
      Put_Line ("call new instance failed.");
      Put ("Error code: ");
      Put (Integer (Ret));
      New_Line;
   else
      Dbg.Put_Line (Dbg.Trace, "Got instance.");
   end if;

   Dbg.Put_Line (Dbg.Trace, "delete instance");
   Delete_Instance (Instance);
   Adaview.Config.Save_Config (Doc_Ctx);
   if Doc_Ctx.Cur_Doc.Name /= Doc_Ctx.Cur_Doc.Temp_Name then
      Delete_File (To_String (Doc_Ctx.Cur_Doc.Temp_Name));
   end if;
end Adaview_main;
