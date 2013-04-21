This is the documentation for the iso-view addon for Angband.
                

Table of contents

1.) Disclaimer
2.) Introduction
3.) Special keys
4.) Troubleshooting
4.1) Known Bugs


1.) Disclaimer

The isometric view addon is copyright (c) by Hj. Malthaner.
For a list of contributing artists see images/list.txt (the
images and the list are only included inthe sources distribution)


2.) Introduction 

The isometric view is a graphical view fro Angband. It uses tiles
of 64x64 pixels to display the Angband world. The tiles are truecolor
images. This means the iso-view addon needs at least 16 bit graphics
hardware to display the tiles properly. The iso-view addon uses the
SDL library (http://www.libsdl.org) to display the graphis in a 
platform independant way. A version for X-Windows which does not use
the SDL library is also available, but no longer maintained.

The main-sdl.c was written by Greg Velichansky, I just modified it
a bit to be able to use it for the iso-view, too. Please note that
main-sdl.c is not considered to be part of the iso-view addon.


3.) Special keys

The isometric view addon uses some special keys to control the view:

- Scroll Lock switches between two zoom modes. I agree scroll lock is
pretty illogical for this, but Angband itself uses that many keys, that
I found nothing better than scroll lock

- Print Screen saves a screen dump as BMP. Thanks to Greg Velichansky
for this function!


4.) Troubleshooting

Well, no FAQ or similar thing exists yet. But if you run into problems
feel free to contact me by email at  hansjoerg.malthaner@gmx.de

4.1) Known Bugs

Please contact DarkGod for bugs in PernAngband (darkgod@ifrance.com)


In general you should turn the following options OFF. They interfere
with the iso-view display code and may cause really weird results:

view_yellow_lite, "Use special colors for torch-lit grids"
view_bright_lite, "Use special colors for 'viewable' grids"
view_granite_lite, "Use special colors for wall grids (slow)"
view_special_lite, "Use special colors for floor grids (slow)"


Known Bugs in the iso-view code

- initial display is sometimes wrong; you have to move at least one 
square to get it right (I don't know yet if this is a bug of Pern or 
the iso-view code)
- highliting of things doesn't work yet (this causes problems with the 
look (l) command and the targetting code)
- the big look command (L) doesn't work at all
- the map (m) command doesn't work at all
- ball spells/breath attacks might be displayed wrong
- some buildings (wall types) and city wall types may be displayed wrong
- town buildings are displayed incorrect at night (only outer walls are
displayed)
- many tiles are missing, maybe some are wrong. 


Written by Hj. Malthaner, May 2001
hansjoerg.malthaner@gmx.de
