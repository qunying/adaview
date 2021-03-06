-------------------------------------------------------------------------------
-- Adaview - A PostScript/PDF viewer based on ghostscript                    --
--                                                                           --
-- Copyright (c) 2014-2018 Zhu Qun-Ying.                                     --
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

-- Postscript scanning and copying routines.
-- Translated from the ps.c file from gv to suite the need of Adaview.

with Adaview.Config; use Adaview.Config;

package Adaview.PS is

   Need_Password     : exception;
   Unknown_File_Type : exception;

   procedure Scan (Ctx : in out Context_T);
   -- scan the PostScript file for document structuring comments.
   --
   -- This scanner is designed to retrieve the information necessary for
   -- the ghostview previewer.  It will scan files that conform to any
   -- version (1.0, 2.0, 2.1, or 3.0) of the document structuring conventions.
   -- It does not really care which version of comments the file contains.
   -- (The comments are largely upward compatible.)  It will scan a number
   -- of non-conforming documents.  (You could have part of the document
   -- conform to V2.0 and the rest conform to V3.0.  It would be similar
   -- to the DC-2 1/2+, it would look funny but it can still fly.)
   --
   -- This routine returns a pointer to the document structure.
   -- The structure contains the information relevant to previewing.
   -- These include EPSF flag (to tell if the file is a encapsulated figure),
   -- Page Media (for the Page Size), Bounding Box (to minimize backing
   -- pixmap size or determine window size for encapsulated PostScript),
   -- Orientation of Paper (for default transformation matrix), and
   -- Page Order.  The title and CreationDate are also retrieved to
   -- help identify the document.
   --
   -- The following comments are examined:
   --
   -- Header section:
   -- Must start with %!PS-Adobe-.  Version numbers ignored.
   --
   -- %!PS-Adobe-* [EPSF*] (changed EPSF-* to EPSF* to do XFig a favor ...###jp###)
   -- %%BoundingBox: <int> <int> <int> <int>|(atend)
   -- %%CreationDate: <textline>
   -- %%Orientation: Portrait|Landscape|(atend)
   -- %%Pages: <uint> [<int>]|(atend)
   -- %%PageOrder: Ascend|Descend|Special|(atend)
   -- %%Title: <textline>
   -- %%DocumentMedia: <text> <real> <real> <real> <text> <text>
   -- %%DocumentPaperSizes: <text>
   -- %%EndComments
   --
   -- Note: Either the 3.0 or 2.0 syntax for %%Pages is accepted.
   --       Also either the 2.0 %%DocumentPaperSizes or the 3.0
   --       %%DocumentMedia comments are accepted as well.
   --
   -- The header section ends either explicitly with %%EndComments or
   -- implicitly with any line that does not begin with %X where X is
   -- a not whitespace character.
   --
   -- If the file is encapsulated PostScript the optional Preview section
   -- is next:
   --
   -- %%BeginPreview
   -- %%EndPreview
   --
   -- This section explicitly begins and ends with the above comments.
   --
   -- Next the Defaults section for version 3 page defaults:
   --
   -- %%BeginDefaults
   -- %%PageBoundingBox: <int> <int> <int> <int>
   -- %%PageOrientation: Portrait|Landscape
   -- %%PageMedia: <text>
   -- %%EndDefaults
   --
   -- This section explicitly begins and ends with the above comments.
   --
   -- The prolog section either explicitly starts with %%BeginProlog or
   -- implicitly with any nonblank line.
   --
   -- %%BeginProlog
   -- %%EndProlog
   --
   -- The Prolog should end with %%EndProlog, however the proglog implicitly
   -- ends when %%BeginSetup, %%Page, %%Trailer or %%EOF are encountered.
   --
   -- The Setup section is where the version 3 page defaults are found.
   -- This section either explicitly begins with %%BeginSetup or implicitly
   -- with any nonblank line after the Prolog.
   --
   -- %%BeginSetup
   -- %%PageBoundingBox: <int> <int> <int> <int>
   -- %%PageOrientation: Portrait|Landscape
   -- %%PaperSize: <text>
   -- %%EndSetup
   --
   -- The Setup should end with %%EndSetup, however the setup implicitly
   -- ends when %%Page, %%Trailer or %%EOF are encountered.
   --
   -- Next each page starts explicitly with %%Page and ends implicitly with
   -- %%Page or %%Trailer or %%EOF.  The following comments are recognized:
   --
   -- %%Page: <text> <uint>
   -- %%PageBoundingBox: <int> <int> <int> <int>|(atend)
   -- %%PageOrientation: Portrait|Landscape
   -- %%PageMedia: <text>
   -- %%PaperSize: <text>
   --
   -- The trailer section start explicitly with %%Trailer and end with %%EOF.
   -- The following comment are examined with the proper (atend) notation
   -- was used in the header:
   --
   -- %%Trailer
   -- %%BoundingBox: <int> <int> <int> <int>|(atend)
   -- %%Orientation: Portrait|Landscape|(atend)
   -- %%Pages: <uint> [<int>]|(atend)
   -- %%PageOrder: Ascend|Descend|Special|(atend)
   -- %%EOF
   --
   --
   --  A DC-3 received severe damage to one of its wings.  The wing was a total
   --  loss.  There was no replacement readily available, so the mechanic
   --  installed a wing from a DC-2.

end Adaview.PS;
-- vim: set expandtab ts=3 sts=3 sw=3 smarttab :
