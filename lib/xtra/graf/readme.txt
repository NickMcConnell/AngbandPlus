Copyrights:
32x32.bmp, 32x32m.bmp:  David Gervais
16x16.bmp, 16x16m.bmp:  Adam Bolt

(various modifications and these notes by -LM-)


Technical notes:
     I've changed the graphics palettes slightly in order to even out the color distribution (at present, the palettes for both sets include many almost identical colors, which wastes valuable space needed for more hues and shades).  So if you put these graphics in with the Angband sets, or vice versa, be sure to notice and correct the glitches that may appear.
     Black (RGB 0,0,0) may be used as a transparent color in some ports (the SDL port being one of them); use the darkest grey for "black" that you want to display.  Or harass the porters until they come up with a more intelligent design!   :-/
     On 256-color systems, the operating system needs a few colors for its own purposes.  Therefore, be sure to leave about 10-20 entries free (black) at the end of the palette.