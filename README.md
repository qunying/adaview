adaview - A PostScript/PDF viewer written in Ada
================================================

This is another PostScript/PDF reader GUI front-end.  There are some
features I would like to have in a PS/PDF reader, but none of the
existing ones have them all.  And I would also like it to be simple.

Currently just use [Ghostscript](http://ghostscript.com)  for both PS
and PDF. Latter maybe use [mupdf](http://mupdf.com) for PDF rendering.

Some plan features:
  1. Could change the backgroud color easily.
  2. When moving forward/backward, indicate the last seen line.
  3. Remember the last read location and zoom setting, when re-open,
     restore them.
  4. Use MD5 sum for recently read document, so even moving the file
     to another location could still matched the same settings.
  5. View gzip/bzip2/xz/zstd/lzip compressed file.
