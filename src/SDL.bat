make -f SDL.mak
pause
move ..\SDL.lib _SDL\SDL.lib
pause
make -f freetype.mak
pause
move ..\freetype.lib _SDL\freetype.lib
pause
make -f SDL_ttf.mak
pause
move ..\SDL_ttf.lib _SDL\SDL_ttf.lib
pause
make -f SDL_image.mak
pause
move ..\SDL_image.lib _SDL\SDL_image.lib
pause
make -f SDL_mixer.mak
pause
move ..\SDL_mixer.lib _SDL\SDL_mixer.lib
pause