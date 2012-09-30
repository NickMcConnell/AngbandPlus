/* Copyright (C) 2003 Neil Stevens <neil@qualityassistant.com>
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
// THE AUTHOR(S) BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
// AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
// CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
//
// Except as contained in this notice, the name(s) of the author(s) shall not be
// used in advertising or otherwise to promote the sale, use or other dealings
// in this Software without prior written authorization from the author(s).
*/

#ifdef USE_SDL

#include "angband.h"
#include <SDL.h>
#include <SDL_image.h>

#define MAX_CONSOLE_COUNT 3
static int arg_console_count = 1;
static bool arg_old_graphics = FALSE;
static bool arg_double_width = FALSE;
static int arg_width = 640;
static int arg_height = 480;
static int arg_bpp = 32;
static bool arg_full_screen = FALSE;

static SDL_Surface *surface = 0;

static void sdl_quit(cptr string)
{
	/* Clean up SDL */
	SDL_Quit();

	/* And now for the default quit behavior */
	quit_aux = 0;
	quit(string);
}

errr init_sdl(int argc, char **argv)
{
	int i;
	float gamma;
	char filename[256];
	filename[255] = 0;

	if (SDL_Init(SDL_INIT_EVERYTHING | SDL_INIT_NOPARACHUTE))
	{
		printf("SDL_Init failed: %s\n", SDL_GetError());
		return -1;
	}

	/* Skip to our arguments */
	for (i = 1; (i < argc) && (0 != strcmp(argv[i], "--")); ++i);
	/* Handle our arguments */
	for (++i; i < argc ; ++i)
	{
		if (0 == strcmp(argv[i], "-n"))
		{
			if (++i == argc)
			{
				printf("Argument missing for option -n\n");
				return -1;
			}

			arg_console_count = atoi(argv[i]);
			if (arg_console_count <= 0 || arg_console_count > MAX_CONSOLE_COUNT)
			{
				printf("Invalid console count given.\n");
				arg_console_count = 1;
			}
		}
		else if (0 == strcmp(argv[i], "-o"))
		{
			arg_old_graphics = TRUE;
		}
		else if (0 == strcmp(argv[i], "-b"))
		{
			arg_double_width = TRUE;
		}
		else if (0 == strcmp(argv[i], "-w"))
		{
			if (++i == argc)
			{
				printf("Argument missing for option -w\n");
				return -1;
			}

			arg_width = atoi(argv[i]);
		}
		else if (0 == strcmp(argv[i], "-h"))
		{
			if (++i == argc)
			{
				printf("Argument missing for option -h\n");
				return -1;
			}

			arg_height = atoi(argv[i]);
		}
		else if (0 == strcmp(argv[i], "-fs"))
		{
			arg_full_screen = TRUE;
		}
		else if (0 == strcmp(argv[i], "-bpp"))
		{
			if (++i == argc)
			{
				printf("Argument missing for option -bpp\n");
				return -1;
			}

			arg_bpp = atoi(argv[i]);
		}
	}

	/* Now for the meat of the initialization */
	quit_aux = sdl_quit;

	/* Window Manager stuff */
	path_build(filename, 255, ANGBAND_DIR_XTRA, "graf/icon.png");
	SDL_WM_SetIcon(IMG_Load(filename), 0);
	SDL_WM_SetCaption("ToME", "tome");

	/* User's requested gamma */
	if (gamma_val != 10)
	{
		gamma = 256.0;
		gamma /= gamma_val;
		SDL_SetGamma(gamma, gamma, gamma);
	}

	/* Display */
	surface = SDL_SetVideoMode(arg_width, arg_height, arg_bpp, SDL_HWSURFACE |
	                           SDL_ANYFORMAT | SDL_DOUBLEBUF |
	                           (arg_full_screen ? SDL_FULLSCREEN : 0));

	if (!surface)
	{
		printf("Failed to create SDL Surface.\n");
		return -1;
	}

	return 0;
}

#endif
