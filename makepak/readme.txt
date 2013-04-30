To build both daten.pak files needed by Iso-Angband use the following 
instructions:


1) execute 'makepak/makepak -'
-> this will create the 32x32 pixel tiles

3) execute 'makepak/makepak'
-> this will create the 64x64 pixel tiles


Now you should have two *.pak files in the current directory, one 
called daten2.pak the other called daten.pak 

Please note, that in future versions, the iso-view will search
the *.pak files in /lib/xtra/graf/iso but currently they are
searched in the current directoy. I assumed in this description,
that you start Iso-Angband from current directory, like this:

./angband or ./src/angband

written by Hj. Malthaner, 12-Dec-2003
Email: hansjoerg.malthaner@gmx.de
