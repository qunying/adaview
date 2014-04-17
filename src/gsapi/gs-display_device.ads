-------------------------------------------------------------------------------
-- GhostScript API Ada binding                                               --
--                                                                           --
-- Copyright (c) 2014 Zhu Qun-Ying.                                          --
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

-- display device definition from ghostscript/gdevdsp.h

--
-- The callback structure must be provided by calling the
-- Ghostscript APIs in the following order:
-- GS.API.new_instance (minst);
-- GS.API.set_display_callback (minst, callback);
-- GS.API.init_with_args (minst, argc, argv);
--
-- Supported parameters and default values are:
-- -sDisplayHandle=16#04d2 or 1234         string
--    Caller supplied handle as a decimal or hexadecimal number
--    in a string.  On 32-bit platforms, it may be set
--    using -dDisplayHandle=1234 for backward compatibility.
--    Included as first parameter of all callback functions.
--
-- -dDisplayFormat=0                       long
--    Color format specified using bitfields below.
--    Included as argument of display_size() and display_presize()
-- These can only be changed when the device is closed.
--
-- The second parameter of all callback functions "void *device"
-- is the address of the Ghostscript display device instance.
-- The arguments "void *handle" and "void *device" together
-- uniquely identify an instance of the display device.
--
-- A typical sequence of callbacks would be
--  open, presize, memalloc, size, sync, page
--  presize, memfree, memalloc, size, sync, page
--  preclose, memfree, close
-- The caller should not access the image buffer:
--  - before the first sync
--  - between presize and size
--  - after preclose
-- If opening the device fails, you might see the following:
--  open, presize, memalloc, memfree, close
--

package GS.Display_Device is

   use Interfaces;
   use Interfaces.C;

   Display_Version_Major : constant := 2;
   Display_Version_Minor : constant := 0;

   Display_Version_Major_V1 : constant := 1;
   Display_Version_Minor_V1 : constant := 0;
   -- before separation fromat was added

   -- The display format is set by a combination of the following bitfields
   subtype display_format_t is Unsigned_32;

   -- Define the color space alternatives
   Display_Colors_Native     : constant display_format_t := 1;
   Display_Colors_Gray       : constant display_format_t := Shift_Left (1, 1);
   Display_Colors_RGB        : constant display_format_t := Shift_Left (1, 2);
   Display_Colors_CMYK       : constant display_format_t := Shift_Left (1, 3);
   Display_Colors_Separation : constant display_format_t :=
     Shift_Left (1, 19);
   Display_Colors_Mask       : constant display_format_t := 16#8_000f#;

   -- Define whether alpha information, or an extra unused bytes is included
   -- Display_Alpha_First and Display_Alpha_Last are not implemented
   Display_Alpha_None   : constant display_format_t := 0;
   Display_Alpha_First  : constant display_format_t := Shift_Left (1, 4);
   Display_Alpha_Last   : constant display_format_t := Shift_Left (1, 5);
   Display_Unused_First : constant display_format_t := Shift_Left (1, 6); -- e.g. MAc xRGB (1, 6);
   Display_Unused_Last  : constant display_format_t := Shift_Left (1, 7); -- e.g. Windows BGRx
   Display_Alpha_Mask   : constant display_format_t := 16#00f0#;

   -- Define the depth per component for Display_Colors_Gray,
   -- Display_Colors_RGB and Display_Colors_CMYK,
   -- or the depth per pixel for Display_Colors_Native
   -- Display_Depth_2 and Display_Depth_12 have not been tested.
   Display_Depth_1      : constant display_format_t := Shift_Left (1, 8);
   Display_Depth_2      : constant display_format_t := Shift_Left (1, 9);
   Display_Depth_4      : constant display_format_t := Shift_Left (1, 10);
   Display_Depth_8      : constant display_format_t := Shift_Left (1, 11);
   Display_Depth_12     : constant display_format_t := Shift_Left (1, 12);
   Display_Depth_16     : constant display_format_t := Shift_Left (1, 13);
   -- unused Shift_Left (1, 14) and Shift_Left (1 15);
   Display_Depth_Mask   : constant display_format_t := 16#ff00#;

   -- Define whether Red/Cyan should come first,
   -- or whether Blue/Black should come first
   Display_Bigendian    : constant display_format_t := 0;
   -- Red/Cyan first
   Display_Littleendian : constant display_format_t := Shift_Left (1, 16);
   -- Blue/Black first
   Display_Endian_Mask  : constant display_format_t := 16#1_0000#;

   --  Define whether the raster starts at the top or bottom of the bitmap
   Display_Top_First    : constant display_format_t := 0;
   Display_Bottom_First : constant display_format_t := Shift_Left (1, 17);

   -- Define whether packing RGB in 16-bits should use 555
   -- or 565 (extra bit for green)
   Display_Row_Align_Default : constant display_format_t := 0;
   -- Display_Row_Align_1 : constant display_format_t := Shift_Left (1, 20);
   -- Display_Row_Align_2 : constant display_format_t := Shift_Left (2, 20);
   -- not currently possible for the above two
   Display_Row_Align_4  : constant display_format_t := Shift_Left (3, 20);
   Display_Row_Align_8  : constant display_format_t := Shift_Left (4, 20);
   Display_Row_Align_16 : constant display_format_t := Shift_Left (5, 20);
   Display_Row_Align_32 : constant display_format_t := Shift_Left (6, 20);
   Display_Row_Align_64 : constant display_format_t := Shift_Left (7, 20);
   Display_Row_Align_Mask : constant display_format_t := 16#70_0000#;

   type display_callback_t is record
      size : int;
      -- Size of this structure
      -- used for checking if we have been handed a valid structure

      version_major : int;
      -- Major version of this structure
      -- The major version number will change if this structure changes.

      version_minor : int;
      -- Minor version of this structure
      -- The minor version number will change if new features are added
      -- without changes to this structure.  For example, a new color
      -- format.

   end record;
   pragma Convention (C, display_callback_t);
   -- Note that for Windows, the display callback functions are
   -- cdecl, not stcall.  This differs from those in GS.API.

end GS.Display_Device;
