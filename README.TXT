=== Angband 2.8.2 for Windows -- Extra Files ===

This folder contains all of the "extra" files needed to compile and use
Angband 2.8.2 for Windows, using the Metrowerks CodeWarrior Pro compiler
(other compilers are not officially supported, but may work as well).

To use this folder, you must first install the "standard source archive",
which produces a folder called (for example) "c:\angband\", containing
a "README" file, and two sub-folders called "src" and "lib".

Then, simply merge the contents of this folder into the "standard" one,
so that (for example) the "ext-win\lib\xtra\graf\8x8.bmp" file becomes
"c:\angband\lib\xtra\graf\8x8.bmp" file, and the "ext-win\angband.mcp"
file becomes "c:\angband\angband.mcp".

Then, you can simply launch the "angband.mcp" project file, and select
"make" from the appropriate menu.  This will create the "angband.exe"
executable.

Finally, you can actually launch the "angband.exe" executable, which
uses the "angband.ini" preference file and the "lib" folder, to play.

See "http://www.voicenet.com/~benh/Angband/" for more information.

--- Ben ---
