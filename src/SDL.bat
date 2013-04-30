make -f SDL.mak
pause
move ..\SDL.lib SDL\SDL.lib
pause
make -f freetype.mak
pause
move ..\freetype.lib SDL\freetype.lib
pause
make -f SDL_ttf.mak
pause
move ..\SDL_ttf.lib SDL\SDL_ttf.lib
pause
make -f SDL_image.mak
pause
move ..\SDL_image.lib SDL\SDL_image.lib
pause
make -f SDL_mixer.mak
pause
move ..\SDL_mixer.lib SDL\SDL_mixer.lib
pause