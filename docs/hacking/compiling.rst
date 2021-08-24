Compiling Instructions
======================

Remember that PWMAngband can only be built and run under Windows. If you're
using Linux, get the PWMAngband binaries directly and use WinE.

PWMAngband comes with various text files that explain how to compile the game
and several batch files that actually do the job.

PWMAngband uses Borland C++ as compiler tool. You can download a free version
of the compiler (see for example: https://edn.embarcadero.com/article/20633).

How to build all libraries required to run the WIN client
---------------------------------------------------------

Just follow the instructions given in the WIN.txt file located in the /src
directory. This will generate the ZLIB and LIBPNG libraries using the WIN.bat
file in the same directory.

How to build all SDL libraries required to run the SDL client
-------------------------------------------------------------

Just follow the instructions given in the SDL.txt file located in the /src
directory. This will generate the SDL, FREETYPE, SDL_TTF, SDL_IMAGE, LIBMAD and
SDL_MIXER libraries using the SDL.bat file in the same directory.

How to build the PWMAngband client
----------------------------------

This is done by running the client.bat file in the /src directory. This will
generate the mangclient_gcu.exe, mangclient_sdl.exe and mangclient.exe
executable files corresponding to the GCU, SDL and WIN client.

How to build the PWMAngband server
----------------------------------

This is done by running the server.bat file in the /src directory. This will
generate the mangband.exe executable file corresponding to the server.

The clean.bat file will delete all generated binaries in case you want to
recompile everything from scratch.

How to build the HTML manual from the help files
------------------------------------------------

Just follow the instructions given in the make.txt file located in the /docs
directory. This will generate the Manual.html file using the make.bat file in
the same directory.

How to build the PWMAngband setup
---------------------------------

This is done by running the setup.bat file in the /src directory. This will
generate all the files required to run PWMAngband.

The cleansetup.bat file will delete all generated files in case you want to
regenerate everything from scratch.
