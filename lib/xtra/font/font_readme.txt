
Fonts with ".fon" suffixes are Windows fonts; they work in the Windows port.

Fonts with ".bdf" suffixes are portable XWindows fonts; when compiled, they work in the X11, XAW, and GTK ports.

The file "makefile.am" is needed to let autoconf know about the new fonts.  
The file "compile_bdf_fonts.sh" is a script that auto-compiles .bdf to .pcf fonts for X11 users.


------------------


Character listing:

  0  Blank
  1  Solid
  2  Heavy pattern
  3  Wall pattern
  4  Many dots
  5  Medium dots
  6  Few dots
  7  Tiny centered dot
  8  Small centered dot
  9  Small boulder
 10  Large boulder
 11  Rubble
 12  Treasure
 13  Closed door
 14  Open door
 15  Broken door
 16  Pillar
 17  Water
 18  Tree
 19  Lava/Fire
 20  Pit/Portal

blanks

 32 - 126:  The ASCII characters, optimized for Angband
127:  Medium shade, traditionally used for walls

blanks

161 - 167:  Some Latin-1 symbols likely to be useful
168 - 171:  Bolt picts
172  Spell - cloud
173  Spell - electricity or webbing
174  Spell - explosion

blanks

191+ The Latin-1 accented and special characters


------------------

     These font are copyrighted freeware; there are no restrictions on usage, sharing, 
or editing other than the basic one of retaining the copyright.

