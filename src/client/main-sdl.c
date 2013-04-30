/*
 * File: main-sdl.c
 * Purpose: Angband SDL port
 *
 * Copyright (c) 2007 Ben Harrison, Gregory Velichansky, Eric Stevens,
 * Leon Marrick, Iain McFall, and others
 * Copyright (c) 2012 MAngband and PWMAngband Developers
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */
#include "c-angband.h"
#include "../common/buildid.h"
#include "c-cmds.h"
#include "netclient.h"

#ifdef USE_SDL

#include "main.h"
#include "..\SDL\SDL.h"
#include "..\SDL\SDL_ttf.h"
#include "..\SDL\SDL_image.h"

#define MIN_SCREEN_WIDTH    640
#define MIN_SCREEN_HEIGHT   480

/* SDL flags used for the main window surface */
static Uint32 vflags = SDL_ANYFORMAT;

/* Current screen dimensions */
static int screen_w = 800;
static int screen_h = 600;

/* Fullscreen dimensions */
static int full_w;
static int full_h;

/* Want fullscreen? */
static bool fullscreen = FALSE;

/* Want nice graphics? */
static bool nicegfx = FALSE;

static int overdraw = 0;
static int overdraw_max = 0;

/* XXXXXXXXX */
static char *ANGBAND_DIR_USER_SDL;

/*
 * Used as 'system' font
 */
static const char *DEFAULT_FONT_FILE = "6x10x.fon";

#define MAX_FONTS 40
char *FontList[MAX_FONTS];
static int num_fonts = 0;

/*
 * A font structure
 * Note that the data is only valid for a surface with matching
 * values for pitch & bpp. If a surface is resized the data _must_ be
 * recalculated.
 */
typedef struct sdl_Font sdl_Font;
struct sdl_Font
{
    int width;          /* The dimensions of this font (in pixels)*/
    int height;
    char name[32];      /* The name of this font */
    Uint16 pitch;       /* Pitch of the surface this font is made for */
    Uint8 bpp;          /* Bytes per pixel of the surface */
    Uint8 something;    /* Padding */
    int *data;          /* The data */
    TTF_Font *sdl_font; /* The native font */
};

static sdl_Font SystemFont;

#define NUM_GLYPHS 256

/*
 * Window information
 * Each window has its own surface and coordinates
 */
typedef struct term_window term_window;
struct term_window
{
    term term_data;
    SDL_Surface *surface;   /* The surface for this window */
#ifdef USE_GRAPHICS
    SDL_Surface *tiles;     /* The appropriately sized tiles for this window */
    SDL_Surface *ttrans;    /* Translucent tiles (pseudo-3D) */
#endif
    byte Term_idx;          /* Index of term that relates to this */
    int top;                /* Window Coordinates on the main screen */
    int left;
    int keys;               /* Size of keypress storage */
    sdl_Font font;          /* Font info */
    char *req_font;         /* Requested font */
    int rows;               /* Dimension in tiles */
    int cols;
    int border;             /* Border width */
    int title_height;       /* Height of title bar */
    int width;              /* Dimension in pixels == tile_wid * cols + 2 x border*/
    int height;
    int tile_wid;           /* Size in pixels of a char */
    int tile_hgt;
    bool visible;           /* Can we see this window? */
    SDL_Rect uRect;         /* The part that needs to be updated */
    bool minimap_active;    /* Are we looking at the minimap? */
    int max_rows;           /* Maximum number of lines */
};

typedef struct mouse_info mouse_info;
struct mouse_info
{
    int left;       /* Is it pressed? */
    int right;
    int leftx;      /* _IF_ left button is pressed these */
    int lefty;      /* show where it was pressed */
    int rightx;
    int righty;
    int x;          /* Current position of mouse */
    int y;
};

#define WINDOW_DRAW (SDL_USEREVENT + 1)

/*
 * The basic angband text colours in an sdl friendly form
 */
static SDL_Color text_colours[MAX_COLORS];

SDL_Color back_colour;    /* Background colour */
Uint32 back_pixel_colour;

typedef struct sdl_ButtonBank sdl_ButtonBank;
typedef struct sdl_Button sdl_Button;
typedef struct sdl_Window sdl_Window;

typedef void (*button_press_func)(sdl_Button *sender);
struct sdl_Button
{
    SDL_Rect pos;               /* Position & Size */
    bool selected;              /* Selected? */
    bool visible;               /* Visible? */
    button_press_func activate; /* A function to call when pressed */
    sdl_ButtonBank *owner;      /* Which bank is this in? */
    char caption[50];           /* Text for this button */
    SDL_Color unsel_colour;     /* Button unselected colour */
    SDL_Color sel_colour;       /* Selected colour */
    SDL_Color cap_colour;       /* Caption colour */
    void *data;                 /* Something */
    int tag;                    /* Something */
};

struct sdl_ButtonBank
{
    sdl_Button *buttons;        /* A collection of buttons */
    bool *used;                 /* What buttons are available? */
    sdl_Window *window;         /* The window that these buttons are on */
    bool need_update;
};

/*
 * Other 'windows' (basically a surface with a position and buttons on it)
 * Currently used for the top status bar and popup windows
 */
typedef void (*sdl_WindowCustomDraw)(sdl_Window *window);
struct sdl_Window
{
    int top;                            /* Position on main window */
    int left;
    int width;                          /* Dimensions */
    int height;
    bool visible;                       /* Visible? */
    SDL_Surface *surface;               /* SDL surface info */
    sdl_ButtonBank buttons;             /* Buttons */
    sdl_Font font;                      /* Font */
    SDL_Surface *owner;                 /* Who shall I display on */
    sdl_WindowCustomDraw draw_extra;    /* Stuff to draw on the surface */
    bool need_update;
};

/*
 * The main surface of the application
 */
static SDL_Surface *AppWin;

/*
 * The status bar
 */
static sdl_Window StatusBar;

/*
 * The Popup window
 */
static sdl_Window PopUp;
static bool popped;

/*
 * Term windows
 */
static term_window windows[ANGBAND_TERM_MAX];
static int Zorder[ANGBAND_TERM_MAX];

/* Keep track of the mouse status */
static mouse_info mouse;

/*
 * The number pad consists of 10 keys, each with an SDL identifier
 */
#define is_numpad(k) \
((k == SDLK_KP0) || (k == SDLK_KP1) || (k == SDLK_KP2) || (k == SDLK_KP3) || \
 (k == SDLK_KP4) || (k == SDLK_KP5) || (k == SDLK_KP6) || \
 (k == SDLK_KP7) || (k == SDLK_KP8) || (k == SDLK_KP9) || (k == SDLK_KP_ENTER))

static int SnapRange = 5;   /* Window snap range (pixels) */
static int StatusHeight;    /* The height in pixels of the status bar */
static int SelectedTerm;    /* Current selected Term */

static int AboutSelect;     /* About button */
static int TermSelect;      /* Term selector button */
static int FontSelect;      /* Font selector button */
static int VisibleSelect;   /* Hide/unhide window button*/
static int MoreSelect;      /* Other options button */
static int QuitSelect;      /* Quit button */

/* For saving the icon for the About Box */
static SDL_Surface *mratt = NULL;

/* Buttons on the 'More' panel */
static int MoreOK;          /* Accept changes */
static int MoreFullscreen;  /* Fullscreen toggle button */
static int MoreNiceGfx;     /* Nice graphics toggle button */
static int MoreSnapPlus;    /* Increase snap range */
static int MoreSnapMinus;   /* Decrease snap range */

static bool Moving;             /* Moving a window */
static bool Sizing;             /* Sizing a window */
static SDL_Rect SizingSpot;     /* Rect to describe the sizing area */
static bool Sizingshow = FALSE; /* Is the resize thingy displayed? */
static SDL_Rect SizingRect;     /* Rect to describe the current resize window */

#ifdef USE_GRAPHICS
#include "grafmode.h"

static SDL_Surface *GfxSurface = NULL;  /* A surface for the graphics */
static SDL_Surface *mmSurface = NULL;   /* Minimap surface (pseudo-3D) */

static int MoreWidthPlus;   /* Increase tile width */
static int MoreWidthMinus;  /* Decrease tile width */
static int MoreHeightPlus;  /* Increase tile height */
static int MoreHeightMinus; /* Decrease tile height */
static int *GfxButtons;     /* Graphics mode buttons */
static int SelectedGfx;     /* Current selected gfx */
#endif


/*
 * Fill in an SDL_Rect structure.
 * Note it also returns the value adjusted
 */
static SDL_Rect *sdl_RECT(int x, int y, int w, int h, SDL_Rect *rect)
{
    rect->x = x;
    rect->y = y;
    rect->w = w;
    rect->h = h;
    
    return rect;
}


/*
 * Is a point(x, y) in a rectangle?
 */
static bool point_in(SDL_Rect *rect, int x, int y)
{
    if (x < rect->x) return (FALSE);
    if (y < rect->y) return (FALSE);
    if (x >= rect->x + rect->w) return (FALSE);
    if (y >= rect->y + rect->h) return (FALSE);
    
    /* Must be inside */
    return (TRUE);
}


/*
 * Draw an outline box
 * Given the top, left, width & height
 */
static void sdl_DrawBox(SDL_Surface *surface, SDL_Rect *rect, SDL_Color colour, int width)
{
    SDL_Rect rc;
    int left = rect->x;
    int right = rect->x + rect->w - width;
    int top = rect->y;
    int bottom = rect->y + rect->h - width;
    Uint32 pixel_colour = SDL_MapRGB(surface->format, colour.r, colour.g, colour.b);
    
    /* Top left -> Top Right */
    sdl_RECT(left, top, rect->w, width, &rc);
    SDL_FillRect(surface, &rc, pixel_colour);
    
    /* Bottom left -> Bottom Right */
    sdl_RECT(left, bottom, rect->w, width, &rc);
    SDL_FillRect(surface, &rc, pixel_colour);
    
    /* Top left -> Bottom left */
    sdl_RECT(left, top, width, rect->h, &rc);
    SDL_FillRect(surface, &rc, pixel_colour);
    
    /* Top right -> Bottom right */
    sdl_RECT(right, top, width, rect->h, &rc);
    SDL_FillRect(surface, &rc, pixel_colour);
}


/*
 * Get the width and height of a given font file
 */
static errr sdl_CheckFont(const char *fontname, int *width, int *height)
{
    char buf[MSG_LEN];
    TTF_Font *ttf_font;
    
    /* Build the path */
    path_build(buf, sizeof(buf), ANGBAND_DIR_XTRA_FONT, fontname);
    
    /* Attempt to load it */
    ttf_font = TTF_OpenFont(buf, 0);
    
    /* Bugger */
    if (!ttf_font) return (-1);
    
    /* Get the size */
    if (TTF_SizeText(ttf_font, "M", width, height)) return (-1);
    
    /* Finished with the font */
    TTF_CloseFont(ttf_font);
    
    return (0);
}


/*
 * The sdl_Font routines
 */


/*
 * Free any memory assigned by Create()
 */
static void sdl_FontFree(sdl_Font *font)
{
    /* Finished with the font */
    TTF_CloseFont(font->sdl_font);
}


/*
 * Create new font data with font fontname, optimizing the data
 * for the surface given
 */
static errr sdl_FontCreate(sdl_Font *font, const char *fontname, SDL_Surface *surface)
{
    char buf[MSG_LEN];
    TTF_Font *ttf_font;
    
    /* Build the path */
    path_build(buf, sizeof(buf), ANGBAND_DIR_XTRA_FONT, fontname);
    
    /* Attempt to load it */
    ttf_font = TTF_OpenFont(buf, 0);
    
    /* Bugger */
    if (!ttf_font) return (-1);
    
    /* Get the size */
    if (TTF_SizeText(ttf_font, "M", &font->width, &font->height)) return (-1);

    /* Get the best quality */
    TTF_SetFontHinting(ttf_font, TTF_HINTING_LIGHT);
    
    /* Fill in some of the font struct */
    my_strcpy(font->name, fontname, 30);
    font->pitch = surface->pitch;
    font->bpp = surface->format->BytesPerPixel;
    font->sdl_font = ttf_font;
    
    /* Success */
    return (0); 
}


/*
 * Draw some text onto a surface
 * The surface is first checked to see if it is compatible with
 * this font, if it isn't the the font will be 're-precalculated'
 *
 * You can, I suppose, use one font on many surfaces, but it is
 * definitely not recommended. One font per surface is good enough.
 */
static errr sdl_FontDraw(sdl_Font *font, SDL_Surface *surface, SDL_Color colour, int x, int y,
    int n, const char *s)
{
    Uint8 bpp = surface->format->BytesPerPixel;
    Uint16 pitch = surface->pitch;
    SDL_Rect rc;
    SDL_Surface *text;

    if ((bpp != font->bpp) || (pitch != font->pitch))
        sdl_FontCreate(font, font->name, surface);

    /* Lock the window surface (if necessary) */
    if (SDL_MUSTLOCK(surface))
    {
        if (SDL_LockSurface(surface) < 0) return (-1);
    }

    sdl_RECT(x, y, n * font->width, font->height, &rc);
    text = TTF_RenderText_Solid(font->sdl_font, s, colour);
    if (text)
    {
        SDL_BlitSurface(text, NULL, surface, &rc);
        SDL_FreeSurface(text);
    }
    
    /* Unlock the surface */
    if (SDL_MUSTLOCK(surface)) SDL_UnlockSurface(surface);
    
    /* Success */
    return (0);
}


/*
 * Draw a button on the screen
 */
static void sdl_ButtonDraw(sdl_Button *button)
{
    SDL_Surface *surface = button->owner->window->surface;
    sdl_Font *font = &button->owner->window->font;
    SDL_Color colour = button->selected? button->sel_colour: button->unsel_colour;
    
    if (!button->visible) return;

    SDL_FillRect(surface, &button->pos, SDL_MapRGB(surface->format, colour.r, colour.g, colour.b));

    if (strlen(button->caption))
    {
        size_t len = strlen(button->caption);
        unsigned max = button->pos.w / font->width;
        int n = MIN(len, max);
        int l = n * font->width / 2;
        int x = button->pos.x + ((button->pos.w) / 2) - l;
        
        sdl_FontDraw(font, surface, button->cap_colour, x, button->pos.y + 1, n, button->caption);
    }
}


/*
 * Adjust the position of a button
 */
static void sdl_ButtonMove(sdl_Button *button, int x, int y)
{
    button->pos.x = x;
    button->pos.y = y;
    button->owner->need_update = TRUE;
}


/*
 * Adjust the size of a button
 */
static void sdl_ButtonSize(sdl_Button *button, int w, int h)
{
    button->pos.w = w;
    button->pos.h = h;
    button->owner->need_update = TRUE;
}


/*
 * Set the caption
 */
static void sdl_ButtonCaption(sdl_Button *button, const char *s)
{
    my_strcpy(button->caption, s, sizeof(button->caption));
    button->owner->need_update = TRUE;
}


/*
 * Set the visibility of a button
 */
static void sdl_ButtonVisible(sdl_Button *button, bool visible)
{
    if (button->visible != visible)
    {
        button->visible = visible;
        
        button->owner->need_update = TRUE;
    }
}


/* Maximum amount of buttons in a bank */
#define MAX_BUTTONS 40


/*
 * The button_bank package
 */


/*
 * Initialize it
 */
static void sdl_ButtonBankInit(sdl_ButtonBank *bank, sdl_Window *window)
{
    bank->window = window;
    bank->buttons = C_ZNEW(MAX_BUTTONS, sdl_Button);
    bank->used = C_ZNEW(MAX_BUTTONS, bool);
    bank->need_update = TRUE;
}


/*
 * Clear the bank
 */
static void sdl_ButtonBankFree(sdl_ButtonBank *bank)
{
    mem_free(bank->buttons);
    mem_free(bank->used);
}


/*
 * Draw all the buttons on the screen
 */
static void sdl_ButtonBankDrawAll(sdl_ButtonBank *bank)
{
    int i;
    
    for (i = 0; i < MAX_BUTTONS; i++)
    {
        sdl_Button *button = &bank->buttons[i];
        
        if (!bank->used[i]) continue;
        if (!button->visible) continue;
        
        sdl_ButtonDraw(button);
    }
    bank->need_update = FALSE;
}


/*
 * Get a new button index
 */
static int sdl_ButtonBankNew(sdl_ButtonBank *bank)
{
    int i = 0;
    sdl_Button *new_button;

    while (bank->used[i] && (i < MAX_BUTTONS)) i++;
    
    if (i == MAX_BUTTONS)
    {
        /* Bugger! */
        return (-1);
    }
    
    /* Get the button */
    new_button = &bank->buttons[i];
    
    /* Mark the button as used */
    bank->used[i] = TRUE;

    /* Clear it */
    WIPE(new_button, sdl_Button);
    
    /* Mark it as mine */
    new_button->owner = bank;
    
    /* Default colours */
    new_button->unsel_colour.r = 160;
    new_button->unsel_colour.g = 160;
    new_button->unsel_colour.b = 60;
    new_button->sel_colour.r = 210;
    new_button->sel_colour.g = 210;
    new_button->sel_colour.b = 110;
    new_button->cap_colour.r = 0;
    new_button->cap_colour.g = 0;
    new_button->cap_colour.b = 0;
    
    /* Success */
    return (i);
}


/*
 * Retrieve button 'idx' or NULL
 */
static sdl_Button *sdl_ButtonBankGet(sdl_ButtonBank *bank, int idx)
{
    /* Check the index */
    if ((idx < 0) || (idx >= MAX_BUTTONS)) return (NULL);
    if (!bank->used[idx]) return (NULL);
    
    /* Return it */
    return &bank->buttons[idx];
}


/*
 * Examine and respond to mouse presses
 * Return if we 'handled' the click
 */
static bool sdl_ButtonBankMouseDown(sdl_ButtonBank *bank, int x, int y)
{
    int i;
    
    /* Check every button */
    for (i = 0; i < MAX_BUTTONS; i++)
    {
        sdl_Button *button = &bank->buttons[i];
        
        /* Discard some */
        if (!bank->used[i]) continue;
        if (!button->visible) continue;
        
        /* Check the coordinates */
        if (point_in(&button->pos, x, y))
        {
            button->selected = TRUE;
            
            /* Draw it */
            bank->need_update = TRUE;
            
            return (TRUE);
        }
    }
    return (FALSE);
}


/*
 * Respond to a mouse button release
 */
static bool sdl_ButtonBankMouseUp(sdl_ButtonBank *bank, int x, int y)
{
    int i;
    
    /* Check every button */
    for (i = 0; i < MAX_BUTTONS; i++)
    {
        sdl_Button *button = &bank->buttons[i];
        
        /* Discard some */
        if (!bank->used[i]) continue;
        if (!button->visible) continue;
        
        /* Check the coordinates */
        if (point_in(&button->pos, x, y))
        {
            /* Has this butoon been 'selected'? */
            if (button->selected)
            {
                /* Activate the button (usually) */
                if (button->activate) (*button->activate)(button);
                
                /* Now not selected */
                button->selected = FALSE;
                
                /* Draw it */
                bank->need_update = TRUE;
                
                return (TRUE);
            }
        }
        else
        {
            /* This button was 'selected' but the release of the */
            /* mouse button was outside the area of this button */
            if (button->selected)
            {
                /* Now not selected */
                button->selected = FALSE;
                
                /* Draw it */
                bank->need_update = TRUE;
            }
        }
    }
    
    return (FALSE);
}


/*
 * sdl_Window functions
 */
static void sdl_WindowFree(sdl_Window* window)
{
    if (window->surface)
    {
        SDL_FreeSurface(window->surface);
        sdl_ButtonBankFree(&window->buttons);
        sdl_FontFree(&window->font);
        WIPE(window, sdl_Window);
    }
}


/*
 * Initialize a window
 */
static void sdl_WindowInit(sdl_Window* window, int w, int h, SDL_Surface *owner,
    const char *fontname)
{
    sdl_WindowFree(window);
    window->owner = owner;
    window->width = w;
    window->height = h;
    window->surface = SDL_CreateRGBSurface(SDL_SWSURFACE, w, h,
        owner->format->BitsPerPixel, owner->format->Rmask, owner->format->Gmask,
        owner->format->Bmask, owner->format->Amask);
    sdl_ButtonBankInit(&window->buttons, window);
    sdl_FontCreate(&window->font, fontname, window->surface);
    window->visible = TRUE;
    window->need_update = TRUE;
}


static void sdl_WindowBlit(sdl_Window* window)
{
    SDL_Rect rc;
    
    if (!window->visible) return;
    
    sdl_RECT(window->left, window->top, window->width, window->height, &rc);
    
    SDL_BlitSurface(window->surface, NULL, window->owner, &rc);
    SDL_UpdateRects(window->owner, 1, &rc);
}


static void sdl_WindowText(sdl_Window* window, SDL_Color c, int x, int y, const char *s)
{
    sdl_FontDraw(&window->font, window->surface, c, x, y, strlen(s), s);
}


static void sdl_WindowUpdate(sdl_Window* window)
{
    if ((window->need_update || window->buttons.need_update) && (window->visible))
    {
        SDL_Event Event;
        
        SDL_FillRect(window->surface, NULL, back_pixel_colour);
        
        if (window->draw_extra) (*window->draw_extra)(window);
        
        sdl_ButtonBankDrawAll(&window->buttons);
        
        window->need_update = FALSE;
        
        WIPE(&Event, SDL_Event);
        
        Event.type = WINDOW_DRAW;
        
        Event.user.data1 = (void*)window;
        
        SDL_PushEvent(&Event);
    }
}


static void term_windowFree(term_window* win)
{
    if (win->surface)
    {
        SDL_FreeSurface(win->surface);
        win->surface = NULL;

#ifdef USE_GRAPHICS
        /* Invalidate the gfx surface */
        if (win->tiles)
        {
            SDL_FreeSurface(win->tiles);
            win->tiles = NULL;
        }
        if (win->ttrans)
        {
            SDL_FreeSurface(win->ttrans);
            win->ttrans = NULL;
        }
#endif

        term_nuke(&win->term_data);
    }
    
    sdl_FontFree(&win->font);
}


static errr save_prefs(void);


/*
 * Display warning message (see "z-util.c")
 */
static void hook_plog(const char *str)
{
    /* Warning */
    if (str) MessageBox(NULL, str, "Warning", MB_ICONEXCLAMATION | MB_OK);
}


static void hook_quit(const char *str)
{
    int i;
    static bool quitting = FALSE;

    /* Don't re-enter if already quitting */
    if (quitting) return;
    quitting = TRUE;
    
    save_prefs();
    
    string_free(ANGBAND_DIR_USER_SDL);
    
    /* Free the surfaces of the windows */
    for (i = 0; i < ANGBAND_TERM_MAX; i++)
    {
        term_windowFree(&windows[i]);
        string_free(windows[i].req_font);
    }

#ifdef USE_GRAPHICS
    /* Free the graphics surfaces */
    if (GfxSurface) SDL_FreeSurface(GfxSurface);
    if (mmSurface) SDL_FreeSurface(mmSurface);
#endif

    /* Free the 'System font' */
    sdl_FontFree(&SystemFont);
    
    /* Free the statusbar window */
    sdl_WindowFree(&StatusBar);
    
    /* Free the popup window */
    sdl_WindowFree(&PopUp);
    
    /* Free the main surface */
    SDL_FreeSurface(AppWin);

    mem_free(GfxButtons);
    close_graphics_modes();

    /* Shut down the font library */
    TTF_Quit();
    
    /* Shut down SDL */
    SDL_Quit();
    
    for (i = 0; i < MAX_FONTS; i++)
        string_free(FontList[i]);

    /* Free resources */
    cleanup_angband();

    /* Cleanup network stuff */
    Net_cleanup();

    /* Cleanup WinSock */
    WSACleanup();
}


static BOOL CtrlHandler(DWORD fdwCtrlType)
{
    switch (fdwCtrlType)
    {
        case CTRL_CLOSE_EVENT:
            quit(NULL);
            return FALSE;
        default:
            return FALSE;
    }
}


static void BringToTop(void)
{
    int i, idx;
    
    for (idx = 0; idx < ANGBAND_TERM_MAX; idx++)
    {
        if (Zorder[idx] == SelectedTerm) break;
    }
    
    if (idx == ANGBAND_TERM_MAX) return;
    
    for (i = idx; i < ANGBAND_TERM_MAX - 1; i++)
    {
        Zorder[i] = Zorder[i + 1];
    }
    
    Zorder[ANGBAND_TERM_MAX - 1] = SelectedTerm;
}


/*
 * Validate a file
 */
static void validate_file(const char *s)
{
    if (!file_exists(s))
        quit_fmt("Cannot find required file:\n%s", s);
}


/*
 * Find a window that is under the points x,y on
 * the main screen
 */

static int sdl_LocateWin(int x, int y)
{
    int i;
    
    for (i = ANGBAND_TERM_MAX - 1; i >= 0; i--)
    {
        term_window *win = &windows[Zorder[i]];
        SDL_Rect rc;
        
        if (!win->visible) continue;
        if (!point_in(sdl_RECT(win->left, win->top, win->width, win->height, &rc), x, y))
            continue;
        
        return (Zorder[i]);
    }
    
    return (-1);
}


static void draw_statusbar(sdl_Window *window)
{
    char buf[128];
    term_window *win = &windows[SelectedTerm];
    int fw = window->font.width;
    int x = 1;
    sdl_Button *button;
    SDL_Rect rc;
    SDL_Color c = {160, 160, 60, 0};

    sdl_RECT(0, StatusBar.height - 1, StatusBar.width, 1, &rc);
    SDL_FillRect(StatusBar.surface, &rc, SDL_MapRGB(StatusBar.surface->format, c.r, c.g, c.b));
    
    button = sdl_ButtonBankGet(&StatusBar.buttons, AboutSelect);
    x += button->pos.w + 20;

    sdl_WindowText(&StatusBar, c, x, 1, "Term:");
    x += 5 * fw;
    
    button = sdl_ButtonBankGet(&StatusBar.buttons, TermSelect);
    button->pos.x = x;
    x += button->pos.w + 10;
    
    my_strcpy(buf, format("(%dx%d)", win->cols, win->rows), sizeof(buf));
    sdl_WindowText(&StatusBar, c, x, 1, buf);
    x += strlen(buf) * fw + 20;
    
    sdl_WindowText(&StatusBar, c, x, 1, "Visible:");
    x += 8 * fw;
    
    button = sdl_ButtonBankGet(&StatusBar.buttons, VisibleSelect);
    button->pos.x = x;
    x += button->pos.w + 20;
    
    button = sdl_ButtonBankGet(&StatusBar.buttons, FontSelect);
    if (button->visible) sdl_WindowText(&StatusBar, c, x, 1, "Font:");
    x += 5 * fw;
    
    button->pos.x = x;
    x += button->pos.w + 20;
    
    button = sdl_ButtonBankGet(&StatusBar.buttons, MoreSelect);
    button->pos.x = x;
    
    x += button->pos.w + 20;
}


static void sdl_BlitWin(term_window *win)
{
    SDL_Rect rc;

    if (!win->surface) return;
    if (!win->visible) return;
    if (win->uRect.x == -1) return;

    /* Select the area to be updated */
    sdl_RECT(win->left + win->uRect.x, win->top + win->uRect.y, win->uRect.w, win->uRect.h, &rc);
    
    SDL_BlitSurface(win->surface, &win->uRect, AppWin, &rc);
    SDL_UpdateRects(AppWin, 1, &rc);

    /* Mark the update as complete */
    win->uRect.x = -1;
}


static void sdl_SizingSpot(term_window *win, bool relative, SDL_Rect *prc)
{
    int xoffset = (relative? 0: win->left);
    int yoffset = (relative? 0: win->top);

    sdl_RECT(xoffset + win->width - 10, yoffset + win->height - 10, 8, 8, prc);
}


static void sdl_BlitAll(void)
{
    SDL_Rect rc;
    sdl_Window *window = &StatusBar;
    int i;
    SDL_Color colour = {160, 160, 60, 0};

    SDL_FillRect(AppWin, NULL, back_pixel_colour);

    for (i = 0; i < ANGBAND_TERM_MAX; i++)
    {
        term_window *win = &windows[Zorder[i]];
        
        if (!win->surface) continue;
        if (!win->visible) continue;
        
        sdl_RECT(win->left, win->top, win->width, win->height, &rc);
        SDL_BlitSurface(win->surface, NULL, AppWin, &rc);

        if (Zorder[i] == SelectedTerm)
        {
            sdl_SizingSpot(win, FALSE, &SizingSpot);

            if (Sizing)
            {
                int grabsize = 10;

                rc = SizingRect;
                sdl_RECT(SizingRect.x + SizingRect.w - grabsize,
                    SizingRect.y + SizingRect.h - grabsize, grabsize, grabsize, &SizingSpot);
            }
        }

        /* Paranoia: always redraw the borders of the window */
        sdl_DrawBox(AppWin, &rc, colour, win->border);
    }
    
    sdl_RECT(window->left, window->top, window->width, window->height, &rc);

    SDL_BlitSurface(window->surface, NULL, AppWin, &rc);

    SDL_UpdateRect(AppWin, 0, 0, AppWin->w, AppWin->h);
}


static void RemovePopUp(void)
{
    PopUp.visible = FALSE;
    popped = FALSE;
    sdl_BlitAll();
}


static void QuitActivate(sdl_Button *sender)
{
    SDL_Event Event;
    
    Event.type = SDL_QUIT;
    
    SDL_PushEvent(&Event);
}


static void SetStatusButtons(void)
{
    term_window *win = &windows[SelectedTerm];
    sdl_Button *button = sdl_ButtonBankGet(&StatusBar.buttons, TermSelect);
    sdl_Button *fontbutton = sdl_ButtonBankGet(&StatusBar.buttons, FontSelect);
    sdl_Button *visbutton = sdl_ButtonBankGet(&StatusBar.buttons, VisibleSelect);
    
    sdl_ButtonCaption(button, angband_term_name[SelectedTerm]);
    
    if (!win->visible)
    {
        sdl_ButtonVisible(fontbutton, FALSE);
        sdl_ButtonCaption(visbutton, "No");
    }
    else
    {
        sdl_ButtonVisible(fontbutton, TRUE);
        sdl_ButtonCaption(fontbutton, win->font.name);
        sdl_ButtonCaption(visbutton, "Yes");
    }
}


static void TermFocus(int idx)
{
    if (SelectedTerm == idx) return;
    
    SelectedTerm = idx;
    
    BringToTop();
    
    SetStatusButtons();
    
    sdl_BlitAll();
}


static void AboutDraw(sdl_Window *win)
{
    SDL_Rect rc;
    SDL_Rect icon;
    
    /* Wow - a different colour! */
    SDL_Color colour = {160, 60, 60, 0};
    
    sdl_RECT(0, 0, win->width, win->height, &rc);
    
    /* Draw a nice box */
    SDL_FillRect(win->surface, &win->surface->clip_rect,
        SDL_MapRGB(win->surface->format, 255, 255, 255));
    sdl_DrawBox(win->surface, &win->surface->clip_rect, colour, 5);
    if (mratt)
    {
        sdl_RECT((win->width - mratt->w) / 2, 5, mratt->w, mratt->h, &icon);
        SDL_BlitSurface(mratt, NULL, win->surface, &icon);
    }
    sdl_WindowText(win, colour, 20, 150, format("You are playing %s", get_buildid(TRUE)));
    sdl_WindowText(win, colour, 20, 160, "See http://www.mangband.org");
}


static void AboutActivate(sdl_Button *sender)
{
    int width = 350;
    int height = 200;
    
    sdl_WindowInit(&PopUp, width, height, AppWin, StatusBar.font.name);
    PopUp.left = (AppWin->w / 2) - width / 2;
    PopUp.top = (AppWin->h / 2) - height / 2;
    PopUp.draw_extra = AboutDraw;
    
    popped = TRUE;
}


static void SelectTerm(sdl_Button *sender)
{
    RemovePopUp();
    
    TermFocus(sender->tag);
}


static int get_term_namewidth(void)
{
    int i, maxl = 0;

    for (i = 0; i < ANGBAND_TERM_MAX; i++)
    {
        int l = strlen(angband_term_name[i]);
        if (l > maxl) maxl = l;
    }

    return (maxl * StatusBar.font.width + 20);
}


static void TermActivate(sdl_Button *sender)
{
    int i;
    int width, height = ANGBAND_TERM_MAX * (StatusBar.font.height + 1);
    
    width = get_term_namewidth();

    sdl_WindowInit(&PopUp, width, height, AppWin, StatusBar.font.name);
    PopUp.left = sender->pos.x;
    PopUp.top = sender->pos.y;

    for (i = 0; i < ANGBAND_TERM_MAX; i++)
    {
        int h = PopUp.font.height;
        int b = sdl_ButtonBankNew(&PopUp.buttons);
        sdl_Button *button = sdl_ButtonBankGet(&PopUp.buttons, b);
        sdl_ButtonSize(button, width - 2 , h);
        sdl_ButtonMove(button, 1, i * (h + 1));
        sdl_ButtonCaption(button, angband_term_name[i]);
        sdl_ButtonVisible(button, TRUE);
        button->tag = i;
        button->activate = SelectTerm;
    }
    popped = TRUE;
}


static void ResizeWin(term_window* win, int w, int h);
static void term_data_link_sdl(term_window *win);


static void VisibleActivate(sdl_Button *sender)
{
    term_window *window = &windows[SelectedTerm];
    
    if (SelectedTerm == 0) return;

    /* Reinitialize all subwindows */
    subwindows_reinit_flags();

    if (window->visible)
    {
        window->visible = FALSE;
        term_windowFree(window);
        angband_term[SelectedTerm] = NULL;
    }
    else
    {
        window->visible = TRUE;
        ResizeWin(window, window->width, window->height);
    }

    /* Set up the subwindows */
    subwindows_init_flags();
    
    SetStatusButtons();
    sdl_BlitAll();

    /* Push a key to force redraw */
    Term_key_push(ESCAPE);
}


static void SelectFont(sdl_Button *sender)
{
    term_window *window = &windows[SelectedTerm];
    int w, h;
    
    sdl_FontFree(&window->font);
    string_free(window->req_font);

    window->req_font = string_make(sender->caption);
    
    sdl_CheckFont(window->req_font, &w, &h);

#ifdef USE_GRAPHICS
    /* Invalidate the gfx surface */
    if (window->tiles)
    {
        SDL_FreeSurface(window->tiles);
        window->tiles = NULL;
    }
    if (window->ttrans)
    {
        SDL_FreeSurface(window->ttrans);
        window->ttrans = NULL;
    }
#endif
    
    ResizeWin(window, (w * window->cols) + (2 * window->border),
        (h * window->rows) + window->border + window->title_height);
    
    SetStatusButtons();
    
    RemovePopUp();
}


static int get_font_namewidth(void)
{
    int i, maxl = 0;

    for (i = 0; i < num_fonts; i++)
    {
        int l = strlen(FontList[i]);
        if (l > maxl) maxl = l;
    }

    return (maxl * StatusBar.font.width + 20);
}


static void FontActivate(sdl_Button *sender)
{
    int i;
    int width, height = num_fonts * (StatusBar.font.height + 1);

    width = get_font_namewidth();

    sdl_WindowInit(&PopUp, width, height, AppWin, StatusBar.font.name);
    PopUp.left = sender->pos.x;
    PopUp.top = sender->pos.y;

    for (i = 0; i < num_fonts; i++)
    {
        int h = PopUp.font.height;
        int b = sdl_ButtonBankNew(&PopUp.buttons);
        sdl_Button *button = sdl_ButtonBankGet(&PopUp.buttons, b);
        sdl_ButtonSize(button, width - 2 , h);
        sdl_ButtonMove(button, 1, i * (h + 1));
        sdl_ButtonCaption(button, FontList[i]);
        sdl_ButtonVisible(button, TRUE);
        button->activate = SelectFont;
    }
    popped = TRUE;
}


#ifdef USE_GRAPHICS
static errr load_gfx(void);
static bool do_update = FALSE;

static void SelectGfx(sdl_Button *sender)
{
    SelectedGfx = sender->tag;
}
#endif


/*
 * Compute tile width/height multipliers to display the best possible tiles
 */
static void apply_nice_graphics(term_window* win)
{
    graphics_mode *mode = get_graphics_mode(p_ptr->use_graphics, TRUE);
    int nice_tile_wid = 0, nice_tile_hgt = 0;

    if (!nicegfx) return;

    if (mode && mode->grafID)
    {
        if (mode->file[0])
        {
            char *end;

            nice_tile_wid = strtol(mode->file, &end, 10);
            nice_tile_hgt = strtol(end + 1, NULL, 10);
        }
        if ((nice_tile_wid == 0) || (nice_tile_hgt == 0))
        {
            nice_tile_wid = mode->cell_width;
            nice_tile_hgt = mode->cell_height;
        }
    }
    if ((nice_tile_wid == 0) || (nice_tile_hgt == 0))
    {
        nice_tile_wid = win->tile_wid;
        nice_tile_hgt = win->tile_hgt;
    }

    if (nice_tile_wid >= win->tile_wid * 2)
        tile_width = nice_tile_wid / win->tile_wid;
    if (nice_tile_hgt >= win->tile_hgt * 2)
        tile_height = nice_tile_hgt / win->tile_hgt;
}


static void AcceptChanges(sdl_Button *sender)
{
    sdl_Button *button;
    bool do_video_reset = FALSE;

#ifdef USE_GRAPHICS
    /* Allow only in initial phase */
    if (!Setup.initialized)
    {
        if (p_ptr->use_graphics != SelectedGfx)
        {
            do_update = TRUE;

            p_ptr->use_graphics = SelectedGfx;
        }

        if (!p_ptr->use_graphics) reset_tile_params();

        button = sdl_ButtonBankGet(&PopUp.buttons, MoreNiceGfx);
        if (button->tag != nicegfx)
        {
            nicegfx = !nicegfx;
            do_update = TRUE;
        }

        load_gfx();

        /* Reset visuals */
        reset_visuals(TRUE);

        /* Invalidate all the gfx surfaces */
        if (do_update)
        {
            int i;

            for (i = 0; i < ANGBAND_TERM_MAX; i++)
            {
                term_window *win = &windows[i];

                if (win->tiles)
                {
                    SDL_FreeSurface(win->tiles);
                    win->tiles = NULL;
                }
                if (win->ttrans)
                {
                    SDL_FreeSurface(win->ttrans);
                    win->ttrans = NULL;
                }
            }
        }
    }
#endif

    button = sdl_ButtonBankGet(&PopUp.buttons, MoreFullscreen);

    if (button->tag != fullscreen)
    {
        fullscreen = !fullscreen;

        do_video_reset = TRUE;
    }

    SetStatusButtons();

    RemovePopUp();

    /* Hacks */
    if (do_update)
    {
        /* Redraw */
        if (Setup.initialized)
            do_cmd_redraw();

        /* This will set up the window correctly */
        else
            ResizeWin(&windows[0], windows[0].width, windows[0].height);
    }

    if (do_video_reset)
    {
        SDL_Event Event;

        WIPE(&Event, SDL_Event);

        Event.type = SDL_VIDEORESIZE;
        Event.resize.w = screen_w;
        Event.resize.h = screen_h;

        SDL_PushEvent(&Event);
    }

	do_update = FALSE;
}


static void FlipTag(sdl_Button *sender)
{
    if (sender->tag)
    {
        sender->tag = 0;
        sdl_ButtonCaption(sender, "Off");
    }
    else
    {
        sender->tag = 1;
        sdl_ButtonCaption(sender, "On");
    }
}


static void SnapChange(sdl_Button *sender)
{
    SnapRange += sender->tag;
    if (SnapRange < 0) SnapRange = 0;
    if (SnapRange > 20) SnapRange = 20;
    PopUp.need_update = TRUE;
}


#ifdef USE_GRAPHICS
static void WidthChange(sdl_Button *sender)
{
	tile_width += sender->tag;
	if (tile_width < 1) tile_width = 1;
	if (tile_width > 12) tile_width = 12;
	do_update = TRUE;
}


static void HeightChange(sdl_Button *sender)
{
	tile_height += sender->tag;
	if (tile_height < 1) tile_height = 1;
	if (tile_height > 8) tile_height = 8;
	do_update = TRUE;
}
#endif


static void MoreDraw(sdl_Window *win)
{
    SDL_Rect rc;
    sdl_Button *button;
    int y = 20;
    graphics_mode *mode;

    /* Wow - a different colour! */
    SDL_Color colour = {160, 60, 60, 0};
    SDL_Color select_colour = {210, 110, 110, 0};

    sdl_RECT(0, 0, win->width, win->height, &rc);

    /* Draw a nice box */
    sdl_DrawBox(win->surface, &rc, colour, 5);

#ifdef USE_GRAPHICS
    /* Allow only in initial phase */
    if (!Setup.initialized)
    {
        button = sdl_ButtonBankGet(&win->buttons, MoreWidthMinus);
        sdl_ButtonVisible(button, SelectedGfx? TRUE: FALSE);

        button = sdl_ButtonBankGet(&win->buttons, MoreWidthPlus);
        sdl_ButtonVisible(button, SelectedGfx? TRUE: FALSE);

        button = sdl_ButtonBankGet(&win->buttons, MoreHeightMinus);
        sdl_ButtonVisible(button, SelectedGfx? TRUE: FALSE);

        button = sdl_ButtonBankGet(&win->buttons, MoreHeightPlus);
        sdl_ButtonVisible(button, SelectedGfx? TRUE: FALSE);

        if (SelectedGfx)
        {
	        sdl_WindowText(win, colour, 20, y, format("Tile width is %d.", tile_width));
            button = sdl_ButtonBankGet(&win->buttons, MoreWidthMinus);
            sdl_ButtonMove(button, 150, y);

            button = sdl_ButtonBankGet(&win->buttons, MoreWidthPlus);
            sdl_ButtonMove(button, 180, y);

            y += 20;

            sdl_WindowText(win, colour, 20, y, format("Tile height is %d.", tile_height));
            button = sdl_ButtonBankGet(&win->buttons, MoreHeightMinus);
            sdl_ButtonMove(button, 150, y);

            button = sdl_ButtonBankGet(&win->buttons, MoreHeightPlus);
            sdl_ButtonMove(button, 180, y);

            y += 20;
        }

        button = sdl_ButtonBankGet(&win->buttons, MoreNiceGfx);
        sdl_WindowText(win, colour, 20, y, "Nice graphics is:");

        sdl_ButtonMove(button, 150, y);
        y += 20;

        sdl_WindowText(win, colour, 20, y, "Selected Graphics:");

        mode = get_graphics_mode(SelectedGfx, FALSE);
        if (mode && mode->grafID)
            sdl_WindowText(win, select_colour, 150, y, mode->menuname);
        else
            sdl_WindowText(win, select_colour, 150, y, "None");

        y += 20;

        sdl_WindowText(win, colour, 20, y, "Available Graphics:");

        mode = graphics_modes;
        while (mode)
        {
            if (mode->menuname[0])
            {
                button = sdl_ButtonBankGet(&win->buttons, GfxButtons[mode->grafID]);
                sdl_ButtonMove(button, 150, y);
                y += 20;
            }
            mode = mode->pNext;
        }
    }
#endif  

    button = sdl_ButtonBankGet(&win->buttons, MoreFullscreen);
    sdl_WindowText(win, colour, 20, y, "Fullscreen is:");

    sdl_ButtonMove(button, 150, y);
    y += 20;

    sdl_WindowText(win, colour, 20, y, format("Snap range is %d.", SnapRange));
    button = sdl_ButtonBankGet(&win->buttons, MoreSnapMinus);
    sdl_ButtonMove(button, 150, y);

    button = sdl_ButtonBankGet(&win->buttons, MoreSnapPlus);
    sdl_ButtonMove(button, 180, y);
}


static int get_gfx_namewidth(void)
{
    int maxl = 0, l;
    graphics_mode *mode = graphics_modes;

    while (mode)
    {
        if (mode->menuname[0])
        {
            l = strlen(mode->menuname);
            if (l > maxl) maxl = l;
        }
        mode = mode->pNext;
    }

    return (maxl * StatusBar.font.width + 20);
}


static void MoreActivate(sdl_Button *sender)
{
    int width = 300;
    int height = 320;
    sdl_Button *button;
    graphics_mode *mode;
    SDL_Color ucolour = {160, 60, 60, 0};
    SDL_Color scolour = {210, 110, 110, 0};
    int gfx_namewidth = get_gfx_namewidth();

    sdl_WindowInit(&PopUp, width, height, AppWin, StatusBar.font.name);
    PopUp.left = (AppWin->w / 2) - width / 2;
    PopUp.top = (AppWin->h / 2) - height / 2;
    PopUp.draw_extra = MoreDraw;

#ifdef USE_GRAPHICS
    /* Allow only in initial phase */
    if (!Setup.initialized)
    {
        SelectedGfx = p_ptr->use_graphics;

        MoreWidthPlus = sdl_ButtonBankNew(&PopUp.buttons);
        button = sdl_ButtonBankGet(&PopUp.buttons, MoreWidthPlus);

        button->unsel_colour = ucolour;
        button->sel_colour = scolour;
        sdl_ButtonSize(button, 20, PopUp.font.height + 2);
        sdl_ButtonCaption(button, "+");
        button->tag = 1;
        sdl_ButtonVisible(button, SelectedGfx? TRUE: FALSE);
        button->activate = WidthChange;

        MoreWidthMinus = sdl_ButtonBankNew(&PopUp.buttons);
        button = sdl_ButtonBankGet(&PopUp.buttons, MoreWidthMinus);

        button->unsel_colour = ucolour;
        button->sel_colour = scolour;
        sdl_ButtonSize(button, 20, PopUp.font.height + 2);
        sdl_ButtonCaption(button, "-");
        button->tag = -1;
        sdl_ButtonVisible(button, SelectedGfx? TRUE: FALSE);
        button->activate = WidthChange;

        MoreHeightPlus = sdl_ButtonBankNew(&PopUp.buttons);
        button = sdl_ButtonBankGet(&PopUp.buttons, MoreHeightPlus);

        button->unsel_colour = ucolour;
        button->sel_colour = scolour;
        sdl_ButtonSize(button, 20, PopUp.font.height + 2);
        sdl_ButtonCaption(button, "+");
        button->tag = 1;
        sdl_ButtonVisible(button, SelectedGfx? TRUE: FALSE);
        button->activate = HeightChange;

        MoreHeightMinus = sdl_ButtonBankNew(&PopUp.buttons);
        button = sdl_ButtonBankGet(&PopUp.buttons, MoreHeightMinus);

        button->unsel_colour = ucolour;
        button->sel_colour = scolour;
        sdl_ButtonSize(button, 20, PopUp.font.height + 2);
        sdl_ButtonCaption(button, "-");
        button->tag = -1;
        sdl_ButtonVisible(button, SelectedGfx? TRUE: FALSE);
	    button->activate = HeightChange;

        MoreNiceGfx = sdl_ButtonBankNew(&PopUp.buttons);
        button = sdl_ButtonBankGet(&PopUp.buttons, MoreNiceGfx);

        button->unsel_colour = ucolour;
        button->sel_colour = scolour;
        sdl_ButtonSize(button, 50, PopUp.font.height + 2);
        sdl_ButtonVisible(button, TRUE);
        sdl_ButtonCaption(button, nicegfx? "On": "Off");
        button->tag = nicegfx;
        button->activate = FlipTag;

        mode = graphics_modes;
        while (mode)
        {
            if (mode->menuname[0])
            {
                GfxButtons[mode->grafID] = sdl_ButtonBankNew(&PopUp.buttons);
                button = sdl_ButtonBankGet(&PopUp.buttons, GfxButtons[mode->grafID]);

                button->unsel_colour = ucolour;
                button->sel_colour = scolour;
                sdl_ButtonSize(button, gfx_namewidth, PopUp.font.height + 2);
                sdl_ButtonVisible(button, TRUE);
                sdl_ButtonCaption(button, mode->menuname);
                button->tag = mode->grafID;
                button->activate = SelectGfx;
            }
            mode = mode->pNext;
        }
    }
#endif

    MoreFullscreen = sdl_ButtonBankNew(&PopUp.buttons);
    button = sdl_ButtonBankGet(&PopUp.buttons, MoreFullscreen);

    button->unsel_colour = ucolour;
    button->sel_colour = scolour;
    sdl_ButtonSize(button, 50, PopUp.font.height + 2);
    sdl_ButtonVisible(button, TRUE);
    sdl_ButtonCaption(button, fullscreen? "On": "Off");
    button->tag = fullscreen;
    button->activate = FlipTag;

    MoreSnapPlus = sdl_ButtonBankNew(&PopUp.buttons);
    button = sdl_ButtonBankGet(&PopUp.buttons, MoreSnapPlus);

    button->unsel_colour = ucolour;
    button->sel_colour = scolour;
    sdl_ButtonSize(button, 20, PopUp.font.height + 2);
    sdl_ButtonCaption(button, "+");
    button->tag = 1;
    sdl_ButtonVisible(button, TRUE);
    button->activate = SnapChange;

    MoreSnapMinus = sdl_ButtonBankNew(&PopUp.buttons);
    button = sdl_ButtonBankGet(&PopUp.buttons, MoreSnapMinus);

    button->unsel_colour = ucolour;
    button->sel_colour = scolour;
    sdl_ButtonSize(button, 20, PopUp.font.height + 2);
    sdl_ButtonCaption(button, "-");
    button->tag = -1;
    sdl_ButtonVisible(button, TRUE);
    button->activate = SnapChange;

    MoreOK = sdl_ButtonBankNew(&PopUp.buttons);
    button = sdl_ButtonBankGet(&PopUp.buttons, MoreOK);

    button->unsel_colour = ucolour;
    button->sel_colour = scolour;
    sdl_ButtonSize(button, 50, PopUp.font.height + 2);
    sdl_ButtonVisible(button, TRUE);
    sdl_ButtonCaption(button, "OK");
    sdl_ButtonMove(button, width / 2 - 25, height - 40);
    button->activate = AcceptChanges;

    popped = TRUE;
}


static errr Term_xtra_sdl_clear(void);


/* Window size bounds checking */
static void check_bounds_resize(term_window *win, int *cols, int *rows, int *max_rows, int *width,
    int *height)
{
    int dummy;

    /* Get the amount of columns & rows */
    *cols = dummy = (*width - (win->border * 2)) / win->tile_wid;
    *rows = *max_rows = (*height - win->border - win->title_height) / win->tile_hgt;

    /* Do something to display more lines in pseudo-3D mode */
    if ((win->Term_idx == 0) && streq(get_graphics_mode(p_ptr->use_graphics, TRUE)->pref, "pseudo"))
        *rows = (*rows - ROW_MAP - 1) * 2 - 3 + ROW_MAP + 1;

    check_term_resize((win->Term_idx == 0), cols, rows);
    check_term_resize((win->Term_idx == 0), &dummy, max_rows);

    /* Calculate the width & height */
    *width = (*cols * win->tile_wid) + (win->border * 2);
    *height = (*max_rows * win->tile_hgt) + win->border + win->title_height;
}


/*
 * Make a window with size (x,y) pixels
 * Note: The actual size of the window may end up smaller.
 * This may be called when a window wants resizing,
 * is made visible, or the font has changed.
 * This function doesn't go in for heavy optimization, and doesn't need it-
 * it may initialize a few too many redraws or whatnot, but everything gets done!
 */
static void ResizeWin(term_window *win, int w, int h)
{
    /* Don't bother */
    if (!win->visible) return;

    win->border = 2;
    win->title_height = StatusHeight;

    /* No font - a new font is needed -> get dimensions */
    if (!win->font.data)
    {
        /* Get font dimensions */
        sdl_CheckFont(win->req_font, &win->tile_wid, &win->tile_hgt);

        /* Oops */
        if (!win->tile_wid || !win->tile_hgt)
        {
			quit_fmt("Unable to find font '%s'.\n"
                "Note that there are new extended font files ending in 'x' in %s.\n"
                "Please check %s and edit if necessary.",
                win->req_font, ANGBAND_DIR_XTRA_FONT, ANGBAND_DIR_USER_SDL);
        }

        /* Apply nice graphics */
        if (win->Term_idx == 0)
        {
            apply_nice_graphics(win);
            tile_distorted = is_tile_distorted(p_ptr->use_graphics, tile_width, tile_height);
        }
    }

    /* Initialize the width & height */
    win->width = w;
    win->height = h;

    /* Window size bounds checking */
    check_bounds_resize(win, &win->cols, &win->rows, &win->max_rows, &win->width, &win->height);

    /* Delete the old surface */
    if (win->surface) SDL_FreeSurface(win->surface);

    /* Create a new surface */
    win->surface = SDL_CreateRGBSurface(SDL_SWSURFACE, win->width, win->height,
        AppWin->format->BitsPerPixel, AppWin->format->Rmask, AppWin->format->Gmask,
        AppWin->format->Bmask, AppWin->format->Amask);

    /* Fill it */
    SDL_FillRect(win->surface, NULL, SDL_MapRGB(AppWin->format, 160, 160, 60));

    /* Label it */
    sdl_FontDraw(&SystemFont, win->surface, back_colour, 1, 1,
        strlen(angband_term_name[win->Term_idx]), angband_term_name[win->Term_idx]);

    /* Mark the whole window for redraw */
    sdl_RECT(0, 0, win->width, win->height, &win->uRect);

    /* Create the font if we need to */
    if (!win->font.data)
        sdl_FontCreate(&win->font, win->req_font, win->surface);

    /* This window was never visible before */
    if (!angband_term[win->Term_idx])
    {
        term *old = Term;

        /* Initialize the term data */
        term_data_link_sdl(win);

        /* Make it visible to angband */
        angband_term[win->Term_idx] = &win->term_data;

        /* Activate it */
        Term_activate((term*)&win->term_data);

        /* Redraw */
        Term_redraw();

        /* Restore */
        Term_activate(old);
    }

    /* Resize the term */
    else
    {
        term *old = Term;

        /* Activate it */
        Term_activate((term*)&win->term_data);

        /* Resize */
        Term_resize(win->cols, win->rows, win->max_rows);

        /* Redraw */
        Term_redraw();

        /* Restore */
        Term_activate(old);
    }

    /* Calculate the hotspot */
    if (win->Term_idx == SelectedTerm) sdl_SizingSpot(win, FALSE, &SizingSpot);

    StatusBar.need_update = TRUE;

    /* Dungeon size */
    if (win->Term_idx == 0)
        net_term_resize(win->cols, win->rows, win->max_rows);

    /* Hack - Redraw all windows */
    if (Setup.initialized) do_cmd_redraw();
}


static errr load_prefs(void)
{
    char buf[MSG_LEN];
    ang_file *fff;

    /* Build the path */
    path_build(buf, sizeof(buf), ANGBAND_DIR_USER, "sdlinit.txt");

    /* XXXXX */
    ANGBAND_DIR_USER_SDL = string_make(buf);

    /* Open the file */
    fff = file_open(buf, MODE_READ, FTYPE_TEXT);

    /* Check it */
    if (!fff) return (1);

    /* Process the file */
    while (file_getl(fff, buf, sizeof(buf)))
    {
        char *s;
        if (!buf[0]) continue;

        if (buf[0] == '#') continue;

        s = strchr(buf, '=');
        s++;
        while (!isalnum(*s)) s++;

        if (strstr(buf, "Resolution"))
        {
            screen_w = atoi(s);
            s = strchr(buf, 'x');
            screen_h = atoi(s + 1);
        }
        else if (strstr(buf, "Fullscreen"))
            fullscreen = atoi(s);
        else if (strstr(buf, "NiceGraphics"))
            nicegfx = atoi(s);
        else if (strstr(buf, "Graphics"))
            p_ptr->use_graphics = atoi(s);
        else if (strstr(buf, "TileWidth"))
			tile_width = atoi(s);
		else if (strstr(buf, "TileHeight"))
			tile_height = atoi(s);
    }

    if (screen_w < MIN_SCREEN_WIDTH) screen_w = MIN_SCREEN_WIDTH;
    if (screen_h < MIN_SCREEN_HEIGHT) screen_h = MIN_SCREEN_HEIGHT;

    file_close(fff);

    tile_distorted = is_tile_distorted(p_ptr->use_graphics, tile_width, tile_height);

    return (0);
}


static errr load_window_prefs(void)
{
    char buf[MSG_LEN];
    ang_file *fff;
    term_window *win;
    int i, w, h, b = 2;

    /* Initialize the windows with default values */
    for (i = 0; i < ANGBAND_TERM_MAX; i++)
    {
        win = &windows[i];

        /* Clear the data */
        WIPE(win, term_window);

        /* Who? */
        win->Term_idx = i;

        /* Default font */
        win->req_font = string_make(DEFAULT_FONT_FILE);

        /* Default width & height */
        sdl_CheckFont(win->req_font, &w, &h);
        win->width = (NORMAL_WID * w) + (b * 2);
        win->height = (NORMAL_HGT * h) + b + StatusHeight;

        /* Default values */
        if (i == 0)
        {
            win->top = StatusHeight;
            win->keys = 1024;
            win->visible = TRUE;
        }
        else
        {
            win->top = windows[0].top + windows[0].height + (i * 10);
            win->left = (i - 1) * 10;
            win->keys = 32;
            win->visible = FALSE;
        }
    }

    /* Open the file */
    fff = file_open(ANGBAND_DIR_USER_SDL, MODE_READ, FTYPE_TEXT);

    /* Check it */
    if (!fff) return (1);

    /* Process the file */
    while (file_getl(fff, buf, sizeof(buf)))
    {
        char *s;
        if (!buf[0]) continue;

        if (buf[0] == '#') continue;

        s = strchr(buf, '=');
        s++;
        while (!isalnum(*s)) s++;

        if (strstr(buf, "Window"))
            win = &windows[atoi(s)];
        else if (strstr(buf, "Visible"))
            win->visible = atoi(s);
        else if (strstr(buf, "Left"))
            win->left = atoi(s);
        else if (strstr(buf, "Top"))
            win->top = atoi(s);
        else if (strstr(buf, "Width"))
            win->width = atoi(s);
        else if (strstr(buf, "Height"))
            win->height = atoi(s);
        else if (strstr(buf, "Keys"))
            win->keys = atoi(s);
        else if (strstr(buf, "Font"))
            win->req_font = string_make(s);
    }

    file_close(fff);

    return (0);
}


static errr save_prefs(void)
{
    ang_file *fff;
    int i;

    /* Open the file */
    fff = file_open(ANGBAND_DIR_USER_SDL, MODE_WRITE, FTYPE_TEXT);

    /* Check it */
    if (!fff) return (1);

    file_putf(fff, "Resolution = %dx%d\n", screen_w, screen_h);
    file_putf(fff, "Fullscreen = %d\n", fullscreen);
    file_putf(fff, "NiceGraphics = %d\n", nicegfx);
    file_putf(fff, "Graphics = %d\n", p_ptr->use_graphics);
    file_putf(fff, "TileWidth = %d\n", tile_width);
    file_putf(fff, "TileHeight = %d\n", tile_height);

    for (i = 0; i < ANGBAND_TERM_MAX; i++)
    {
        term_window *win = &windows[i];

        file_putf(fff, "\nWindow = %d\n", i);
        file_putf(fff, "Visible = %d\n", (int)win->visible);
        file_putf(fff, "Left = %d\n", win->left);
        file_putf(fff, "Top = %d\n", win->top);
        file_putf(fff, "Width = %d\n", win->width);
        file_putf(fff, "Height = %d\n", win->height);
        file_putf(fff, "Keys = %d\n", win->keys);
        file_putf(fff, "Font = %s\n", win->req_font);
    }   

    file_close(fff);

    /* Done */
    return (0);
}


static void set_update_rect(term_window *win, SDL_Rect *rc);


static void DrawSizeWidget(void)
{
    Uint32 colour = SDL_MapRGB(AppWin->format, 30, 160, 70);

    SDL_FillRect(AppWin, &SizingSpot, colour);
    SDL_UpdateRects(AppWin, 1, &SizingSpot);
}


static int Movingx;
static int Movingy;


/*
 * Is What within Range units of Origin
 */
#define closeto(Origin, What, Range) \
    ((ABS((Origin) - (What))) < (Range))


/*
 * This function keeps the 'mouse' info up to date,
 * and reacts to mouse buttons appropriately.
 */
static void sdl_HandleMouseEvent(SDL_Event *event)
{
    term_window *win;

    switch (event->type)
    {
        /* Mouse moved */
        case SDL_MOUSEMOTION:
        {
            mouse.x = event->motion.x;
            mouse.y = event->motion.y;
            win = &windows[SelectedTerm];

            /* We are moving a window */
            if (Moving)
            {
                int i;

                /* Move the window */
                win->left = (mouse.x - Movingx);
                win->top = (mouse.y - Movingy);

                /* Left bounds check */
                if (win->left < 0)
                {
                    win->left = 0;
                    Movingx = mouse.x;
                }

                /* Right bounds check */
                if ((win->left + win->width) > AppWin->w)
                {
                    win->left = AppWin->w - win->width;
                    Movingx = mouse.x - win->left;
                }

                /* Top bounds check */
                if (win->top < StatusHeight)
                {
                    win->top = StatusHeight;
                    Movingy = mouse.y - win->top;
                }

                /* Bottom bounds check */
                if ((win->top + win->height) > AppWin->h)
                {
                    win->top = AppWin->h - win->height;
                    Movingy = mouse.y - win->top;
                }

                for (i = 0; i < ANGBAND_TERM_MAX; i++)
                {
                    term_window *snapper = &windows[i];

                    /* Can't snap to self... */
                    if (i == SelectedTerm) continue;

                    /* Can't snap to the invisible */
                    if (!snapper->visible) continue;

                    /* Check the windows are across from each other */
                    if ((snapper->top < win->top + win->height) &&
                        (win->top < snapper->top + snapper->height))
                    {
                        /* Lets try to the left... */
                        if (closeto(win->left, snapper->left + snapper->width, SnapRange))
                        {
                            win->left = snapper->left + snapper->width;
                            Movingx = mouse.x - win->left;
                        }

                        /* Maybe to the right */
                        if (closeto(win->left + win->width, snapper->left, SnapRange))
                        {
                            win->left = snapper->left - win->width;
                            Movingx = mouse.x - win->left;
                        }
                    }
                    
                    /* Check the windows are above/below each other */
                    if ((snapper->left < win->left + win->width) &&
                        (win->left < snapper->left + snapper->width))
                    {
                        /* Lets try to the top... */
                        if (closeto(win->top, snapper->top + snapper->height, SnapRange))
                        {
                            win->top = snapper->top + snapper->height;
                            Movingy = mouse.y - win->top;
                        }

                        /* Maybe to the bottom */
                        if (closeto(win->top + win->height, snapper->top, SnapRange))
                        {
                            win->top = snapper->top - win->height;
                            Movingy = mouse.y - win->top;
                        }
                    }
                }

                /* Show on the screen */
                sdl_BlitAll();
            }
            
            /* A window is being re-sized */
            else if (Sizing)
            {
                int dummy_cols, dummy_rows, dummy_max_rows;
                int rect_width, rect_height;

                /* Calculate the dimensions of the sizing rectangle */
                rect_width = win->width - win->left + (mouse.x - Movingx);
                rect_height = win->height - win->top + (mouse.y - Movingy);

                /* Window size bounds checking */
                check_bounds_resize(win, &dummy_cols, &dummy_rows, &dummy_max_rows, &rect_width,
                    &rect_height);

                /* Adjust the sizing rectangle */
                SizingRect.w = rect_width;
                SizingRect.h = rect_height;

                /* Show on the screen */                
                sdl_BlitAll();
            }

            else if (!popped)
            {
                /* Have a look for the corner stuff */
                if (point_in(&SizingSpot, mouse.x, mouse.y))
                {
                    if (!Sizingshow)
                    {
                        /* Indicate the hotspot */
                        Sizingshow = TRUE;
                        DrawSizeWidget();
                    }
                }

                /* Remove the hotspot */
                else if (Sizingshow)
                {
                    SDL_Rect rc;

                    Sizingshow = FALSE;
                    sdl_SizingSpot(win, TRUE, &rc);
                    set_update_rect(win, &rc);
                    sdl_BlitWin(win);
                }
            }

            break;
        }
            
        /* A button has been pressed */
        case SDL_MOUSEBUTTONDOWN:
        {
            sdl_Window *window;
            bool res;
            int idx = sdl_LocateWin(mouse.x, mouse.y);
            
            if (event->button.button == SDL_BUTTON_LEFT)
            {
                mouse.left = 1;
                mouse.leftx = event->button.x;
                mouse.lefty = event->button.y;

                /* Pop up window gets priority */
                if (popped) window = &PopUp; else window = &StatusBar;

                /* React to a button press */
                res = sdl_ButtonBankMouseDown(&window->buttons,
                    mouse.x - window->left, mouse.y - window->top);

                /* If pop-up window active and no reaction then cancel the popup */
                if (popped && !res)
                {
                    RemovePopUp();
                    break;
                }

                /* Has this mouse press been handled */
                if (res) break;

                /* Is the mouse press in a term_window? */
                if (idx < 0) break;

                /* The 'focused' window has changed */
                if (idx != SelectedTerm) TermFocus(idx);

                /* A button press has happened on the focused term window */
                win = &windows[idx];

                /* Check for mouse press in the title bar */
                if (mouse.y < win->top + win->title_height)
                {
                    /* Let's get moving */
                    Moving = TRUE;

                    /* Remember where we started */
                    Movingx = mouse.x - win->left;
                    Movingy = mouse.y - win->top;
                }

                /* Check for the little hotspot in the botton right corner */
                else if (point_in(&SizingSpot, mouse.x, mouse.y))
                {
                    /* Let's get sizing */
                    Sizing = TRUE;

                    /* Create the sizing rectangle */
                    sdl_RECT(win->left, win->top, win->width, win->height, &SizingRect);

                    /* Remember where we started */
                    Movingx = mouse.x - win->left;
                    Movingy = mouse.y - win->top;
                }
            }

            /* Process right mouse button */
            else if (event->button.button == SDL_BUTTON_RIGHT)
            {
                mouse.right = 1;
                mouse.rightx = event->button.x;
                mouse.righty = event->button.y;

                /* Right-click always cancels the popup */
                if (popped) popped = FALSE;
            }

            break;
        }

        case SDL_MOUSEBUTTONUP:
        {
            /* Handle release of left button */
            if (event->button.button == SDL_BUTTON_LEFT)
            {
                sdl_Window *window;
                bool res;
                mouse.left = 0;

                /* Pop up window gets priority */
                if (popped) window = &PopUp; else window = &StatusBar;

                /* React to a button release */
                res = sdl_ButtonBankMouseUp(&window->buttons, mouse.x - window->left,
                    mouse.y - window->top);

                /* Cancel popup */
                if (popped && !res) RemovePopUp();

                /* Finish moving */
                if (Moving)
                {
                    Moving = FALSE;

                    /* Update */
                    sdl_BlitAll();
                }

                /* Finish sizing */
                if (Sizing)
                {
                    /* Sort out the window */
                    ResizeWin(&windows[SelectedTerm], SizingRect.w, SizingRect.h);
                    Sizing = FALSE;
                    Sizingshow = FALSE;

                    /* Update */
                    sdl_BlitAll();
                }
            }
            else if (event->button.button == SDL_BUTTON_RIGHT)
                mouse.right = 0;

            break;
        }
    }
}


/*
 * Handle keypresses.
 *
 * We treat left and right modifier keys as equivalent.
 * We ignore any key without a valid SDLK index.
 */
static void sdl_keypress(SDL_keysym keysym)
{
    u16b key_code = keysym.unicode;
    SDLKey key_sym = keysym.sym;
    int ch = 0;

    /* Store the value of various modifier keys */
    bool mc = ((keysym.mod & KMOD_CTRL) > 0);
	bool ms = ((keysym.mod & KMOD_SHIFT) > 0);
	bool ma = ((keysym.mod & KMOD_ALT) > 0);
	bool mm = ((keysym.mod & KMOD_META) > 0);
    /*bool mg = ((keysym.mod & KMOD_MODE) > 0);*/
    bool kp = FALSE;
    byte mods = ((ma? KC_MOD_ALT: 0) | (mm? KC_MOD_META: 0));

    /* Hack -- For keyboards with Alt-Gr translated into KMOD_RALT | KMOD_LCTRL */
    /*if ((keysym.mod & KMOD_RALT) && (keysym.mod & KMOD_LCTRL)) mg = TRUE;*/

    /* Ignore if main term is not initialized */
    if (!Term) return;

    /* Handle all other valid SDL keys */
    switch (key_sym)
    {
        /* Keypad */
        case SDLK_KP0: ch = '0'; kp = TRUE; break;
        case SDLK_KP1: ch = '1'; kp = TRUE; break;
        case SDLK_KP2: ch = '2'; kp = TRUE; break;
        case SDLK_KP3: ch = '3'; kp = TRUE; break;
        case SDLK_KP4: ch = '4'; kp = TRUE; break;
        case SDLK_KP5: ch = '5'; kp = TRUE; break;
        case SDLK_KP6: ch = '6'; kp = TRUE; break;
        case SDLK_KP7: ch = '7'; kp = TRUE; break;
        case SDLK_KP8: ch = '8'; kp = TRUE; break;
        case SDLK_KP9: ch = '9'; kp = TRUE; break;
        case SDLK_KP_PERIOD: ch = '.'; kp = TRUE; break;
        case SDLK_KP_DIVIDE: ch = '/'; kp = TRUE; break;
        case SDLK_KP_MULTIPLY: ch = '*'; kp = TRUE; break;
        case SDLK_KP_MINUS: ch = '-'; kp = TRUE; break;
        case SDLK_KP_PLUS: ch = '+'; kp = TRUE; break;
        case SDLK_KP_ENTER: ch = KC_ENTER; kp = TRUE; break;
        case SDLK_KP_EQUALS: ch = '='; kp = TRUE; break;

        /* Have these to get consistent ctrl-shift behaviour */
        /*case SDLK_0: if ((!ms || mc || ma) && !mg) ch = '0'; break;
        case SDLK_1: if ((!ms || mc || ma) && !mg) ch = '1'; break;
        case SDLK_2: if ((!ms || mc || ma) && !mg) ch = '2'; break;
        case SDLK_3: if ((!ms || mc || ma) && !mg) ch = '3'; break;
        case SDLK_4: if ((!ms || mc || ma) && !mg) ch = '4'; break;
        case SDLK_5: if ((!ms || mc || ma) && !mg) ch = '5'; break;
        case SDLK_6: if ((!ms || mc || ma) && !mg) ch = '6'; break;
        case SDLK_7: if ((!ms || mc || ma) && !mg) ch = '7'; break;
        case SDLK_8: if ((!ms || mc || ma) && !mg) ch = '8'; break;
        case SDLK_9: if ((!ms || mc || ma) && !mg) ch = '9'; break;*/

        case SDLK_UP: ch = ARROW_UP; break;
        case SDLK_DOWN: ch = ARROW_DOWN; break;
        case SDLK_RIGHT: ch = ARROW_RIGHT; break;
        case SDLK_LEFT: ch = ARROW_LEFT; break;

        case SDLK_INSERT: ch = KC_INSERT; break;
        case SDLK_HOME: ch = KC_HOME; break;
        case SDLK_PAGEUP: ch = KC_PGUP; break;
        case SDLK_DELETE: ch = KC_DELETE; break;
        case SDLK_END: ch = KC_END; break;
        case SDLK_PAGEDOWN: ch = KC_PGDOWN; break;
        case SDLK_ESCAPE: ch = ESCAPE; break;
        case SDLK_BACKSPACE: ch = KC_BACKSPACE; break;
        case SDLK_RETURN: ch = KC_ENTER; break;
        case SDLK_TAB: ch = KC_TAB; break;

        case SDLK_F1: ch = KC_F1; break;
        case SDLK_F2: ch = KC_F2; break;
        case SDLK_F3: ch = KC_F3; break;
        case SDLK_F4: ch = KC_F4; break;
        case SDLK_F5: ch = KC_F5; break;
        case SDLK_F6: ch = KC_F6; break;
        case SDLK_F7: ch = KC_F7; break;
        case SDLK_F8: ch = KC_F8; break;
        case SDLK_F9: ch = KC_F9; break;
        case SDLK_F10: ch = KC_F10; break;
        case SDLK_F11: ch = KC_F11; break;
        case SDLK_F12: ch = KC_F12; break;
        case SDLK_F13: ch = KC_F13; break;
        case SDLK_F14: ch = KC_F14; break;
        case SDLK_F15: ch = KC_F15; break;

        default: break;
    }

    if (ch)
    {
        if (kp) mods |= KC_MOD_KEYPAD;
        if (mc) mods |= KC_MOD_CONTROL;
        if (ms) mods |= KC_MOD_SHIFT;
        Term_keypress(ch, mods);
    }
    else if (key_code)
    {
        /* If the keycode is 7-bit ASCII (except numberpad) send directly to the game */
        if (mc && ((key_sym == SDLK_TAB) || (key_sym == SDLK_RETURN) ||
            (key_sym == SDLK_BACKSPACE) || MODS_INCLUDE_CONTROL(key_code)))
        {
            mods |= KC_MOD_CONTROL;
        }
        if (ms && MODS_INCLUDE_SHIFT(key_code)) mods |= KC_MOD_SHIFT;

        Term_keypress(key_code, mods);
    }
}


static void init_windows(void);
static void init_morewindows(void);


/*
 * Handle a single message sent to the application.
 *
 * Functions that are either called from a separate thread or which need to
 * create a separate thread (such as sounds) need to pass messages to this
 * function in order to execute most operations.  See the useage of
 * "SDL_USEREVENT".
 */
static errr sdl_HandleEvent(SDL_Event *event)
{
    /* Handle message */
    switch (event->type)
    {
        /* Keypresses */
        case SDL_KEYDOWN:
        {
            /* Handle keypress */
            sdl_keypress(event->key.keysym);
            break;
        }
        case SDL_MOUSEBUTTONDOWN:
        case SDL_MOUSEBUTTONUP:
        {
            /* Handle mouse stuff */
            sdl_HandleMouseEvent(event);
            break;
        }

        case SDL_MOUSEMOTION:
        {
            int i;
            SDL_Event events[10];

            /*
             * If there are a bundle of mouse movements pending,
             * we'll just take every tenth one - this makes a
             * simple approach to dragging practical, for instance.
             */  
            i = SDL_PeepEvents(events, 10, SDL_GETEVENT,
                SDL_EVENTMASK(SDL_MOUSEMOTION));
            if (i > 0) *event = events[i - 1];

            /* Handle mouse stuff */
            sdl_HandleMouseEvent(event);
            break;
        }
            
        /* Shut down the game */
        /* XXX - check for stuck inside menu etc... */
        case SDL_QUIT:
        {
            save_prefs();

            quit(NULL);
            break;
        }
            
        /* Resize the application */
        case SDL_VIDEORESIZE:
        {
            /* Free the surface */
            SDL_FreeSurface(AppWin);

            if (!fullscreen)
            {
                /* Make sure */
                vflags &= ~(SDL_FULLSCREEN);
                vflags |= SDL_RESIZABLE;

                screen_w = event->resize.w;
                screen_h = event->resize.h;

                if (screen_w < MIN_SCREEN_WIDTH) screen_w = MIN_SCREEN_WIDTH;
                if (screen_h < MIN_SCREEN_HEIGHT) screen_h = MIN_SCREEN_HEIGHT;

                /* Resize the application surface */
                AppWin = SDL_SetVideoMode(screen_w, screen_h, 0, vflags);
            }
            else
            {
                /* Make sure */
                vflags |= SDL_FULLSCREEN;
                vflags &= ~(SDL_RESIZABLE);

                AppWin = SDL_SetVideoMode(full_w, full_h, 0, vflags);
            }
            init_windows();
            init_morewindows();
            sdl_BlitAll();

            break;
        }
            
        case WINDOW_DRAW:
        {
            /* Redraw window that have asked */
            sdl_Window *window = (sdl_Window*)event->user.data1;
            sdl_WindowBlit(window);
            break;
        }

        default:
        {
            /* Do nothing */
            break;
        }
    }   
    sdl_WindowUpdate(&StatusBar);
    sdl_WindowUpdate(&PopUp);
    return (0);
}


/*
 * Update the redraw rect
 * A simple but effective way to keep track of what
 * parts of a window need to updated.
 * Any new areas that are updated before a blit are simply combined
 * into a new larger rectangle to encompass all changes.
 */
static void set_update_rect(term_window *win, SDL_Rect *rc)
{
    /* No outstanding update areas yet? */
    if (win->uRect.x == -1)
    {
        /* Simple copy */
        win->uRect = *rc;
    }
    else
    {
        /* Combine the old update area with the new */
        int x = MIN(win->uRect.x, rc->x);
        int y = MIN(win->uRect.y, rc->y);
        int x2 = MAX(win->uRect.x + win->uRect.w, rc->x + rc->w);
        int y2 = MAX(win->uRect.y + win->uRect.h, rc->y + rc->h);
        sdl_RECT(x, y, x2 - x, y2 - y, &win->uRect);
    }
}


/*
 * Clear a terminal window
 */
static errr Term_xtra_sdl_clear(void)
{
    term_window *win = (term_window*)(Term->data);
    SDL_Rect rc;

    /* Oops */
    if (!win->surface) return (1);

    /* Create the fill area */
    sdl_RECT(win->border, win->title_height, win->width - (2 * win->border),
         win->height - win->border - win->title_height, &rc);

    /* Fill the rectangle */
    SDL_FillRect(win->surface, &rc, back_pixel_colour);

    /* Rectangle to update */
    set_update_rect(win, &rc);

    /* Success */
    return (0);
}


/*
 * Process at least one event
 */
static errr Term_xtra_sdl_event(int v)
{
    SDL_Event event;
    errr error = 0;

    /* Wait for an event */
    if (v)
    {
        /* Wait for an event */
        if (SDL_WaitEvent(&event))
        {
            /* Handle it */
            error = sdl_HandleEvent(&event);
        }
        else return (1);
    }

    /* Check for an event */
    else
    {
        /* Get a single pending event */
        if (SDL_PollEvent(&event))
        {
            /* Handle it */
            error = sdl_HandleEvent(&event);
        }
    }

    /* Note success or failure */
    return (error);
}


/*
 * Process all pending events
 */
static errr Term_xtra_sdl_flush(void)
{
    SDL_Event event;

    /* Get all pending events */
    while (SDL_PollEvent(&event))
    {
        /* Handle them (ignore errors) */
        sdl_HandleEvent(&event);
    }

    /* Done */
    return (0);
}


/*
 * Delay for "x" milliseconds
 */
static errr Term_xtra_sdl_delay(int v)
{
    /* Sleep */
    if (v > 0)
    {
        Term_xtra_sdl_event(0);
        SDL_Delay(v);
    }

    /* Success */
    return (0);
}


/*
 * Returns TRUE if pseudo-3D tiles are allowed
 */
static bool pseudo_allowed(term_window *win)
{
    if (!Setup.initialized) return FALSE;
    if (strcmp(get_graphics_mode(p_ptr->use_graphics, TRUE)->pref, "pseudo")) return FALSE;
    if (win->minimap_active) return FALSE;
    return TRUE;
}


/*
 * Moves a pseudo-3D tile to its display position
 */
static bool pseudo_translate(term_window *win, SDL_Rect *prc)
{
    /* Display rectangle */
    int x0 = COL_MAP * win->tile_wid;
    int y0 = ROW_MAP * win->tile_hgt;
    int xmax = win->cols * win->tile_wid;
    int ymax = win->rows * win->tile_hgt;

    if (!pseudo_allowed(win)) return TRUE;

    /* Inscribe the tile in the display rectangle */
    prc->x = x0 + 2 * (prc->x - x0) / 3 + (ymax - prc->y) / 4;
    prc->y = y0 + (prc->y - y0) / 2;

    if (prc->x + prc->w > xmax) return FALSE;
    return TRUE;
}


static errr get_sdl_rect(term_window *win, int col, int row, bool translate, SDL_Rect *prc)
{
    /* Make the destination rectangle */
    sdl_RECT(col * win->tile_wid, row * win->tile_hgt, win->tile_wid, win->tile_hgt, prc);

    /* Stretch for bigtile mode */
    if (!Term->minimap_active)
    {
	    prc->w *= tile_width;
	    prc->h *= tile_height;
    }

    /* Translate it (pseudo-3D) */
    if (translate && !pseudo_translate(win, prc)) return (1);

    /* Translate it */
    prc->x += win->border;
    prc->y += win->title_height;

    /* Success */
    return (0);
}


/*
 * Displays the "big" cursor
 */
static errr Term_bigcurs_sdl(int col, int row)
{
    term_window *win = (term_window*)(Term->data);
    SDL_Color colour = text_colours[TERM_YELLOW];
    SDL_Rect rc;
    /*byte a, ta;
    char c, tc;
    int j = 0;*/

    if (Term->minimap_active)
    {
        /* Normal cursor in map window */
        Term_curs_sdl(col, row);
        return 0;
    }

    /* Make a rectangle */
    if (get_sdl_rect(win, col, row, TRUE, &rc)) return (1);

    /* If we are using overdraw, draw a double height cursor (disabled for now) */
    /*if (!Term_info(col, row, &a, &c, &ta, &tc)) j = (a & 0x7F);
    if (overdraw && (j > 2) && (j >= overdraw) && (j <= overdraw_max))
    {
        rc.y -= rc.h;
        rc.h = (rc.h << 1);
    }*/

    /* Draw it */
    sdl_DrawBox(win->surface, &rc, colour, 1);

    /* Update area */
    set_update_rect(win, &rc);

    /* Success */
    return (0);
}


/*
 * Displays the "normal" cursor
 */
static errr Term_curs_sdl(int col, int row)
{
    term_window *win = (term_window*)(Term->data);
    SDL_Color colour = text_colours[TERM_YELLOW];
    SDL_Rect rc;

    /* Make a rectangle */
    sdl_RECT(col * win->tile_wid, row * win->tile_hgt, win->tile_wid, win->tile_hgt, &rc);

    /* Translate it */
    rc.x += win->border;
    rc.y += win->title_height;

    /* Paranoia */
    if (rc.y > win->height) return (-1);

    /* Draw it */
    sdl_DrawBox(win->surface, &rc, colour, 1);

    /* Update area */
    set_update_rect(win, &rc);

    /* Success */
    return (0);
}


static errr Term_xtra_sdl(int n, int v)
{
    switch (n)
    {
        /* Process an event */
        case TERM_XTRA_EVENT:
            return (Term_xtra_sdl_event(v));

        /* Flush all events */
        case TERM_XTRA_FLUSH:
            return (Term_xtra_sdl_flush());

        /* Clear the screen */
        case TERM_XTRA_CLEAR:
            return (Term_xtra_sdl_clear());

        /* Show or hide the cursor */
        case TERM_XTRA_SHAPE:
        {
            int x, y;

            /* Obtain the cursor */
            Term_locate(&x, &y);

            /* Show or hide the cursor */
            Term_curs_sdl(x, y);
            return (0);
        }

        case TERM_XTRA_FRESH:
        {
            /* Get the current window data */
            term_window *win = (term_window*)(Term->data);

            /* Blat it! */
            sdl_BlitWin(win);

            /* Done */
            return (0);
        }

        case TERM_XTRA_DELAY:
            return (Term_xtra_sdl_delay(v));

        case TERM_XTRA_REACT:
        {
            int i;

            /* Re-initialize the colours */
            for (i = 0; i < MAX_COLORS; i++)
            {
                text_colours[i].r = angband_color_table[i][1];
                text_colours[i].g = angband_color_table[i][2];
                text_colours[i].b = angband_color_table[i][3];
            }

            if (p_ptr->use_graphics != v)
            {
                p_ptr->use_graphics = v;
                if (!p_ptr->use_graphics) reset_tile_params();
                load_gfx();
                reset_visuals(TRUE);

                /* Redraw */
                if (Setup.initialized)
                    do_cmd_redraw();

                /* Apply nice graphics */
                else
                {
                    apply_nice_graphics(&windows[0]);
                    tile_distorted = is_tile_distorted(p_ptr->use_graphics, tile_width, tile_height);
                }
            }
        }
    }

    return (1);
}


static errr Term_wipe_sdl(int col, int row, int n)
{
    term_window *win = (term_window*)(Term->data);
    SDL_Rect rc;

    /* Build the area to black out */
    rc.x = col * win->tile_wid;
    rc.y = row * win->tile_hgt;
    rc.w = win->tile_wid * n;
    rc.h = win->tile_hgt;

    /* Translate it */
    rc.x += win->border;
    rc.y += win->title_height;

    /* Paranoia */
    if (rc.y > win->height) return (-1);

    /* Wipe it */
    SDL_FillRect(win->surface, &rc, back_pixel_colour);

    /* Update */
    set_update_rect(win, &rc);

    return (0);
}


#ifdef USE_GRAPHICS
/*
 * Do a 'stretched blit'
 * SDL has no support for stretching... What a bastard!
 */
static void sdl_StretchBlit(SDL_Surface *src, SDL_Rect *srcRect, SDL_Surface *dest,
    SDL_Rect *destRect)
{
    int x, y;
    int sx, sy, dx, dy;
    Uint8 *ps, *pd;

    for (y = 0; y < destRect->h; y++)
    {
        for (x = 0; x < destRect->w; x++)
        {
            /* Actual source coords */
            sx = (srcRect->w * x / (destRect->w)) + srcRect->x;
            sy = (srcRect->h * y / (destRect->h)) + srcRect->y;

            /* Find a source pixel */
            ps = (Uint8 *)src->pixels + (sx * src->format->BytesPerPixel) + (sy * src->pitch);

            /* Actual destination pixel coords */
            dx = x + destRect->x;
            dy = y + destRect->y;

            /* Destination pixel */
            pd = (Uint8 *)dest->pixels + (dx * dest->format->BytesPerPixel) + (dy * dest->pitch);

            switch (dest->format->BytesPerPixel)
            {
                case 1:
                {
                    *pd = *ps;
                    break;
                }
                case 2:
                {
                    Uint16 *ps16 = (Uint16*) ps;
                    Uint16 *pd16 = (Uint16*) pd;
                    *pd16 = *ps16;
                    break;
                }
                case 3:
                case 4:
                {
                    Uint32 *ps32 = (Uint32*) ps;
                    Uint32 *pd32 = (Uint32*) pd;
                    *pd32 = *ps32;
                }
            }
        }
    }
}


/*
 * Make the 'pre-stretched' tiles for this window
 * Assumes the tiles surface was freed elsewhere
 */
static errr sdl_BuildTileset(term_window *win)
{
    int x, y;
    int ta, td;
    int xx, yy;
    graphics_mode *info;
    int dwid, dhgt;
    SDL_Surface *surface = GfxSurface;

    info = get_graphics_mode(p_ptr->use_graphics, TRUE);
    if (!(info && info->grafID)) return (1);

    /* Hack -- Use 32x32 tiles in pseudo-3D mode for minimap */
    if (streq(info->pref, "pseudo") && Term->minimap_active)
    {
        info = get_graphics_mode_by_name("david");
        if (!(info && info->grafID)) return (1);
        surface = mmSurface;
    }

    if (!surface) return (1);

    if (Term->minimap_active)
    {
        dwid = win->tile_wid;
        dhgt = win->tile_hgt;
    }
    else
    {
        dwid = win->tile_wid * tile_width;
        dhgt = win->tile_hgt * tile_height;
    }

    /* Calculate the number of tiles across & down */
    ta = surface->w / info->cell_width;
    td = surface->h / info->cell_height;

    /* Calculate the size of the new surface */
    x = ta * dwid;
    y = td * dhgt;

    /* Make it */
    win->tiles = SDL_CreateRGBSurface(SDL_SWSURFACE, x, y, surface->format->BitsPerPixel,
        surface->format->Rmask, surface->format->Gmask, surface->format->Bmask,
        surface->format->Amask);

    /* Bugger */
    if (!win->tiles) return (1);

    /* Pseudo-3D: blit using the colour key AND the per-surface alpha value */
    if (streq(info->pref, "pseudo"))
    {
        /* Normal tiles */
        SDL_SetColorKey(win->tiles, SDL_SRCCOLORKEY, back_pixel_colour);
        SDL_SetAlpha(win->tiles, SDL_SRCALPHA, 255);

        /* Translucent tiles */
        win->ttrans = SDL_CreateRGBSurface(SDL_SWSURFACE, x, y, surface->format->BitsPerPixel,
            surface->format->Rmask, surface->format->Gmask, surface->format->Bmask,
            surface->format->Amask);
        if (!win->ttrans) return (1);
        SDL_SetColorKey(win->ttrans, SDL_SRCCOLORKEY, back_pixel_colour);
        SDL_SetAlpha(win->ttrans, SDL_SRCALPHA, 128);
    }

    /* For every tile... */
    for (xx = 0; xx < ta; xx++)
    {
        for (yy = 0; yy < td; yy++)
        {
            SDL_Rect src, dest;

            /* Source rectangle (on surface) */
            sdl_RECT(xx * info->cell_width, yy * info->cell_height, info->cell_width,
                info->cell_height, &src);

            /* Destination rectangle (win->tiles) */
			sdl_RECT(xx * dwid, yy * dhgt, dwid, dhgt, &dest);

            /* Do the stretch thing */
            sdl_StretchBlit(surface, &src, win->tiles, &dest);
            if (win->ttrans) sdl_StretchBlit(surface, &src, win->ttrans, &dest);
        }
    }

    return (0);
}


/*
 * Draw a tile, given its position and dimensions.
 *
 * If "prc" is not null, only draw the portion of the tile inside that rectangle. We suppose here
 * that the two rectangles "rc" and "prc" are overlapping.
 */
static void sdl_DrawTile(term_window *win, int col, int row, SDL_Rect rc, SDL_Rect *prc, byte a,
    char c, bool background)
{
    int j = (a & 0x7F);
    SDL_Rect src;

    /* Get the dimensions of the graphic surface */
    src.w = rc.w;
    src.h = rc.h;

    /* Default background to darkness */
    src.x = 0;
    src.y = 0;

    /* Use the terrain picture only if mapped */
    if ((a & 0x80) || !background)
    {
        src.x = (c & 0x7F) * src.w;
        src.y = j * src.h;
    }

    /* If we are using overdraw, draw the top rectangle */
    if (overdraw && (row > 2) && (j >= overdraw) && (j <= overdraw_max))
    {
        /* Double the height */
        src.y -= rc.h;
        rc.y -= rc.h;
        rc.h = (rc.h << 1);
        src.h = rc.h;
        SDL_BlitSurface(win->tiles, &src, win->surface, &rc);
    }

    /* Draw a portion of the tile */
    else if (prc)
    {
        int dx = prc->x - rc.x;
        int dy = prc->y - rc.y;

        if (dx > 0)
        {
            src.x += dx;
            rc.x = prc->x;
        }
        rc.w -= abs(dx);
        src.w = rc.w;
        if (dy > 0)
        {
            src.y += dy;
            rc.y = prc->y;
        }
        rc.h -= abs(dy);
        src.h = rc.h;
        if (win->ttrans && background)
            SDL_BlitSurface(win->ttrans, &src, win->surface, &rc);
        else
            SDL_BlitSurface(win->tiles, &src, win->surface, &rc);
    }

    /* Draw the tile */
    else if (win->ttrans && background)
        SDL_BlitSurface(win->ttrans, &src, win->surface, &rc);
    else
        SDL_BlitSurface(win->tiles, &src, win->surface, &rc);
}


/*
 * Draw foreground and background tiles, given their position and dimensions.
 *
 * If "prc" is not null, only draw the portion of the tiles inside that rectangle. We suppose here
 * that the two rectangles "rc" and "prc" are overlapping.
 */
static void sdl_DrawTiles(term_window *win, int col, int row, SDL_Rect rc, SDL_Rect *prc, byte a,
    char c, byte ta, char tc)
{
    /* Draw the terrain tile */
    sdl_DrawTile(win, col, row, rc, prc, ta, tc, TRUE);

    /* If foreground is the same as background, we're done */
    if ((ta == a) && (tc == c)) return;

    /* Draw the foreground tile */
    sdl_DrawTile(win, col, row, rc, prc, a, c, FALSE);
}


/*
 * To redraw pseudo-3D tiles correctly, we need to memorize the 6 closest neighbors
 */
static const POINT pointsBef[3] = {{-1, -1}, {0, -1}, {-1, 0}};
static const POINT pointsAft[3] = {{1, 0}, {0, 1}, {1, 1}};


/*
 * Draw foreground and background tiles for the closest neighbors, given their position and
 * dimensions.
 *
 * If "prc" is not null, only draw the portion of the tiles inside that rectangle. We suppose here
 * that the two rectangles "rc" and "prc" are overlapping.
 */
static void sdl_DrawNeighbors(term_window *win, int col, int row, SDL_Rect *prc,
    const POINT points[3])
{
    int j;
    byte a, ta;
    char c, tc;
    SDL_Rect rc;
    int tile_wid = 1, tile_hgt = 1;

    /* Large tile mode */
    if (!map_active && !Term->minimap_active)
    {
        tile_wid = tile_width;
        tile_hgt = tile_height;
    }

    /* Redraw overlapping tiles */
    for (j = 0; j < 3; j++)
    {
        int jcol = col + points[j].x * tile_wid;
        int jrow = row + points[j].y * tile_hgt;

        /* Check bounds */
        if ((jcol < COL_MAP) || (jcol >= win->cols) || (jrow < ROW_MAP) || (jrow >= win->rows))
            continue;

        /* Get tile info */
        if (Term_info(jcol, jrow, &a, &c, &ta, &tc)) continue;

        /* Make the destination rectangle */
        if (get_sdl_rect(win, jcol, jrow, TRUE, &rc)) continue;

        /* Only redraw overlapping parts */
        sdl_DrawTiles(win, jcol, jrow, rc, prc, a, c, ta, tc);
    }
}
#endif


/*
 * Draw some text to a window
 */
static errr Term_text_sdl_aux(int col, int row, int n, byte a, const char *s)
{
    term_window *win = (term_window*)(Term->data);
    SDL_Color colour = text_colours[a];
    int x = col * win->tile_wid;
    int y = row * win->tile_hgt;
    char buf[256];

    /* Paranoia */
    if (n > win->cols) return (-1);

    /* Do something to display more lines in pseudo-3D mode */
    if ((win->Term_idx == 0) && streq(get_graphics_mode(p_ptr->use_graphics, TRUE)->pref, "pseudo"))
    {
        int sizey = (win->height - win->border - win->title_height) / win->tile_hgt;

        /* Draw the status line properly */
        if (row == win->rows - 1)
        {
            row = sizey - 1;
            y = row * win->tile_hgt;
        }

        /* Don't draw other extra lines */
        if (row >= sizey) return (-1);
    }

    /* Translate */
    x += win->border;
    y += win->title_height;

    /* Not much point really... */
    if (!win->visible) return (0);

    /* Clear the way */
    Term_wipe_sdl(col, row, n);

    /* Take a copy of the incoming string, but truncate it at n chars */
    strncpy(buf, s, n);
    buf[n] = '\0';

    /* Draw it */
    return (sdl_FontDraw(&win->font, win->surface, colour, x, y, n, buf));
}


/*
 * Draw some text to a window.
 *
 * For double-height tiles, we redraw all double-height tiles below.
 */
static errr Term_text_sdl(int col, int row, int n, byte a, const char *s)
{
#ifdef USE_GRAPHICS
    term_window *win = (term_window*)(Term->data);
    SDL_Rect rc;
    int i;
    byte fa, ta;
    char fc, tc;
    int tile_wid = 1, tile_hgt = 1;

    /* Large tile mode */
    if (!Term->minimap_active)
    {
        tile_wid = tile_width;
        tile_hgt = tile_height;
    }
#endif /* USE_GRAPHICS */

    /* Redraw the current text */
    Term_text_sdl_aux(col, row, n, a, s);

#ifdef USE_GRAPHICS
    /* Redraw the bottom tiles (recursively) */
    for (i = 0; i < n; i++)
    {
        int j = 1, tilex, tiley;

        while (j)
        {
            /* Get the position of the jth tile below the ith character */
            tilex = COL_MAP + ((col - COL_MAP + i) / tile_wid) * tile_wid;
            tiley = ROW_MAP + ((row - ROW_MAP) / tile_hgt + j) * tile_hgt;

            if (overdraw && (tiley > 2) && !Term_info(tilex, tiley, &fa, &fc, &ta, &tc))
            {
                int row = (fa & 0x7F);
                int trow = (ta & 0x7F);

                if (((trow >= overdraw) && (trow <= overdraw_max)) ||
                    ((row >= overdraw) && (row <= overdraw_max)))
                {
                    get_sdl_rect(win, tilex, tiley, FALSE, &rc);
                    set_update_rect(win, &rc);
                    sdl_DrawTiles(win, tilex, tiley, rc, NULL, fa, fc, ta, tc);
                    j++;
                }
                else j = 0;
            }
            else j = 0;
        }
    }
#endif /* USE_GRAPHICS */

    /* Success */
    return 0;
}


/*
 * Put some gfx on the screen.
 *
 * Called with n > 1 only if always_pict is true, which is never the case.
 *
 * For double-height tiles, we redraw the tile just above and all double-height tiles below.
 */
static errr Term_pict_sdl(int col, int row, int n, const byte *ap, const char *cp,
    const byte *tap, const char *tcp)
{
#ifdef USE_GRAPHICS
    /* Get the right window */
    term_window *win = (term_window*)(Term->data);
    SDL_Rect rc, rc2;
	int i;
    byte a, ta;
    char c, tc;
    int tile_wid = 1, tile_hgt = 1;

    /* Large tile mode */
    if (!Term->minimap_active)
    {
        tile_wid = tile_width;
        tile_hgt = tile_height;
    }

    /* Toggle minimap view */
    if (win->minimap_active != Term->minimap_active)
    {
        win->minimap_active = Term->minimap_active;
        if (win->tiles)
        {
            SDL_FreeSurface(win->tiles);
            win->tiles = NULL;
        }
        if (win->ttrans)
        {
            SDL_FreeSurface(win->ttrans);
            win->ttrans = NULL;
        }
    }

    /* First time a pict is requested we load the tileset in */
    if (!win->tiles)
    {
        sdl_BuildTileset(win);
        if (!win->tiles) return (1);
    }

    /* Make the destination rectangle */
    if (get_sdl_rect(win, col, row, TRUE, &rc)) return (1);

    /* Blit 'em! (it) */
    for (i = 0; i < n; i++)
    {
        int j = 1;

        /* Update area */
        set_update_rect(win, &rc);

        /* Clear the way */
        SDL_FillRect(win->surface, &rc, back_pixel_colour);

        /* Redraw the neighbor tiles (part 1 -- west and north) */
        if (pseudo_allowed(win))
            sdl_DrawNeighbors(win, col + i * tile_wid, row, &rc, pointsBef);

        /* Redraw the top tile */
        else if (overdraw && !Term_info(col + i * tile_wid, row - tile_hgt, &a, &c, &ta, &tc))
        {
            if (a & 0x80)
            {
                get_sdl_rect(win, col + i * tile_wid, row - tile_hgt, FALSE, &rc2);
                set_update_rect(win, &rc2);
                sdl_DrawTiles(win, col + i * tile_wid, row - tile_hgt, rc2, NULL, a, c, ta, tc);
            }
            else
            {
                int tx, ty;

                for (tx = col + i * tile_wid; tx < col + (i + 1) * tile_wid; tx++)
                {
                    for (ty = row - tile_hgt; ty < row; ty++)
                    {
                        Term_info(tx, ty, &a, &c, &ta, &tc);
                        Term_text_sdl_aux(tx, ty, 1, a, &c);
                    }
                }
            }
        }

        /* Draw the terrain and foreground tiles */
        sdl_DrawTiles(win, col + i * tile_wid, row, rc, NULL, ap[i], cp[i], tap[i], tcp[i]);

        /* Redraw the neighbor tiles (part 2 -- east and south) */
        if (pseudo_allowed(win))
            sdl_DrawNeighbors(win, col + i * tile_wid, row, &rc, pointsAft);

        /* Redraw the bottom tile (recursively) */
        else while (j)
        {
            if (overdraw && (row + j * tile_hgt > 2) &&
                !Term_info(col + i * tile_wid, row + j * tile_hgt, &a, &c, &ta, &tc))
            {
                int frow = (a & 0x7F);
                int trow = (ta & 0x7F);

                if (((trow >= overdraw) && (trow <= overdraw_max)) ||
                    ((frow >= overdraw) && (frow <= overdraw_max)))
                {
                    get_sdl_rect(win, col + i * tile_wid, row + j * tile_hgt, FALSE, &rc2);
                    set_update_rect(win, &rc2);
                    sdl_DrawTiles(win, col + i * tile_wid, row + j * tile_hgt, rc2, NULL, a, c,
                        ta, tc);
                    j++;
                }
                else j = 0;
            }
            else j = 0;
        }

        /* Advance */
        rc.x += rc.w;
    }
#endif

    return (0);
}


/*
 * Create and initialize the Term contained within this window.
 */
static void term_data_link_sdl(term_window *win)
{
    term *t = &win->term_data;

    /* Initialize the term */
    term_init(t, win->cols, win->rows, win->max_rows, win->keys);

    t->higher_pict = TRUE;

    /* Use a "software" cursor */
    t->soft_cursor = TRUE;

    /* Erase with "white space" */
    t->attr_blank = TERM_WHITE;
    t->char_blank = ' ';

    /* Differentiate between BS/^h, Tab/^i, etc. */
    t->complex_input = TRUE;

    /* Never refresh one row */
    t->never_frosh = TRUE;

    /* Ignore the init/nuke hooks */

    /* Prepare the template hooks */
    t->xtra_hook = Term_xtra_sdl;
    t->curs_hook = Term_curs_sdl;
    t->bigcurs_hook = Term_bigcurs_sdl;
    t->wipe_hook = Term_wipe_sdl;
    t->text_hook = Term_text_sdl;
    t->pict_hook = Term_pict_sdl;

    /* Remember where we came from */
    t->data = win;
}


/*
 * Initialize the status bar:
 *  Populate it with some buttons
 *  Set the custom draw function for the bar
 */
static void init_morewindows(void)
{
    char buf[128];
    sdl_Button *button;
    int x;

    popped = FALSE;

    /* Make sure */
    sdl_WindowFree(&PopUp);

    /* Initialize the status bar */
    sdl_WindowInit(&StatusBar, AppWin->w, StatusHeight, AppWin, DEFAULT_FONT_FILE);

    /* Custom drawing function */
    StatusBar.draw_extra = draw_statusbar;

    /* Don't overlap the buttons */
    if (AppWin->w >= 720) my_strcpy(buf, get_buildid(TRUE), sizeof(buf));
    else my_strcpy(buf, "About...", sizeof(buf));

    AboutSelect = sdl_ButtonBankNew(&StatusBar.buttons);
    button = sdl_ButtonBankGet(&StatusBar.buttons, AboutSelect);

    /* Initialize the 'about' button */
    sdl_ButtonSize(button, StatusBar.font.width * strlen(buf) + 5, StatusHeight - 2);
    sdl_ButtonMove(button, 1, 1);
    sdl_ButtonVisible(button, TRUE);
    sdl_ButtonCaption(button, buf);
    button->activate = AboutActivate;

    /* New button */
    TermSelect = sdl_ButtonBankNew(&StatusBar.buttons);
    button = sdl_ButtonBankGet(&StatusBar.buttons, TermSelect);

    /* Initialize the 'term' button */
    sdl_ButtonSize(button, get_term_namewidth(), StatusHeight - 2);
    x = 100 + (StatusBar.font.width * 5);
    sdl_ButtonMove(button, x, 1);
    sdl_ButtonVisible(button, TRUE);
    button->activate = TermActivate;

    /* Another new button */
    VisibleSelect = sdl_ButtonBankNew(&StatusBar.buttons);
    button = sdl_ButtonBankGet(&StatusBar.buttons, VisibleSelect);

    /* Initialize the 'visible' button */
    sdl_ButtonSize(button, 50, StatusHeight - 2);
    x = 200 + (StatusBar.font.width * 8);
    sdl_ButtonMove(button, x, 1);
    sdl_ButtonVisible(button, TRUE);
    button->activate = VisibleActivate;

    /* Another new button */
    FontSelect = sdl_ButtonBankNew(&StatusBar.buttons);
    button = sdl_ButtonBankGet(&StatusBar.buttons, FontSelect);

    /* Initialize the 'font_select' button */
    sdl_ButtonSize(button, get_font_namewidth(), StatusHeight - 2);
    sdl_ButtonMove(button, 400, 1);
    button->activate = FontActivate;

    /* Another new button */
    MoreSelect = sdl_ButtonBankNew(&StatusBar.buttons);
    button = sdl_ButtonBankGet(&StatusBar.buttons, MoreSelect);

    /* Initialize the 'more' button */
    sdl_ButtonSize(button, 50, StatusHeight - 2);
    sdl_ButtonMove(button, 400, 1);
    sdl_ButtonVisible(button, TRUE);
    sdl_ButtonCaption(button, "Options");
    button->activate = MoreActivate;

    /* Another new button */
    QuitSelect = sdl_ButtonBankNew(&StatusBar.buttons);
    button = sdl_ButtonBankGet(&StatusBar.buttons, QuitSelect);

    /* Initialize the 'quit' button */
    sdl_ButtonSize(button, 50, StatusHeight - 2);
    sdl_ButtonMove(button, AppWin->w - 51, 1);
    sdl_ButtonCaption(button, "Quit");
    button->activate = QuitActivate;
    sdl_ButtonVisible(button, TRUE);

    SetStatusButtons();

    TermFocus(0);
}


#ifdef USE_GRAPHICS
/*
 * The new streamlined graphics loader.
 * Only uses colour keys.
 * Much more tolerant of different bit-planes
 */
static errr load_gfx(void)
{
    char buf[MSG_LEN];
    const char *filename = NULL;
    SDL_Surface *temp;
    graphics_mode *mode;

    if (GfxSurface && is_current_graphics_mode(p_ptr->use_graphics)) return (0);

    mode = get_graphics_mode(p_ptr->use_graphics, TRUE);
    if (mode && mode->grafID) filename = mode->file;

    /* Free the old surfaces */
    if (GfxSurface)
    {
        SDL_FreeSurface(GfxSurface);
        GfxSurface = NULL;
    }
    if (mmSurface)
    {
        SDL_FreeSurface(mmSurface);
        mmSurface = NULL;
    }

    /* This may be called when GRAPHICS_NONE is set */
    ANGBAND_GRAF = "none";
    if (!filename) return (0);

    /* Find and load the file into a temporary surface */
    path_build(buf, sizeof(buf), ANGBAND_DIR_XTRA_GRAF, filename);
    temp = IMG_Load(buf);
    if (!temp) return (1);

    /* Change the surface type to the current video surface format */
    if (streq(mode->pref, "pseudo"))
        GfxSurface = SDL_DisplayFormat(temp);
    else
        GfxSurface = SDL_DisplayFormatAlpha(temp);

    /* Minimap surface (pseudo-3D) */
    if (streq(mode->pref, "pseudo"))
    {
        graphics_mode *info = get_graphics_mode_by_name("david");

        if (!(info && info->grafID)) return (1);
        path_build(buf, sizeof(buf), ANGBAND_DIR_XTRA_GRAF, info->file);
        temp = IMG_Load(buf);
        if (!temp) return (1);
        mmSurface = SDL_DisplayFormatAlpha(temp);
    }

    /* Make sure we know what pref file to use */
    ANGBAND_GRAF = mode->pref;

    overdraw = mode->overdrawRow;
    overdraw_max = mode->overdrawMax;

    /* All good */
    return (0);
}
#endif


/*
 * Initialize the graphics
 */
static void init_gfx(void)
{
#ifndef USE_GRAPHICS
    /* Make sure */
    p_ptr->use_graphics = GRAPHICS_NONE;
    reset_tile_params();
#else
    graphics_mode *mode;

    /* Check for existence of required files */
    mode = graphics_modes;
    while (mode)
    {
        char path[MSG_LEN];

        /* Check the graphic file */
        if (mode->file[0])
        {
            path_build(path, sizeof(path), ANGBAND_DIR_XTRA_GRAF, mode->file);

            if (!file_exists(path))
            {
                plog_fmt("Can't find file %s - graphics mode '%s' will be disabled.", path,
                    mode->menuname);
                mode->file[0] = 0;
            }
        }
        mode = mode->pNext;
    }

    /* Check availability (default to no graphics) */
    mode = get_graphics_mode(p_ptr->use_graphics, TRUE);
    if (!(mode && mode->grafID && mode->file[0]))
    {
        p_ptr->use_graphics = GRAPHICS_NONE;
        reset_tile_params();
    }

    /* Load the graphics stuff in */
    load_gfx();
#endif
}

/*
 * Create the windows
 * Called sometime after load_prefs()
 */
static void init_windows(void)
{
    int i;

    for (i = 0; i < ANGBAND_TERM_MAX; i++)
    {
        term_window *win = &windows[i];

        /* Only bother with visible windows */
        if (win->visible)
        {
            /* Left bounds check */
            if (win->left < 0) win->left = 0;

            /* Right bounds check */
            if ((win->left + win->width) > AppWin->w)
            {
                if (win->width > AppWin->w) win->width = AppWin->w;
                win->left = AppWin->w - win->width;
            }

            /* Top bounds check */
            if (win->top < StatusHeight) win->top = StatusHeight;

            /* Bottom bounds check */
            if ((win->top + win->height) > AppWin->h)
            {
                if (win->height > AppWin->h) win->height = AppWin->h;
                win->top = AppWin->h - win->height;
            }

#ifdef USE_GRAPHICS
            /* Invalidate the gfx surface */
            if (win->tiles)
            {
                SDL_FreeSurface(win->tiles);
                win->tiles = NULL;
            }
            if (win->ttrans)
            {
                SDL_FreeSurface(win->ttrans);
                win->ttrans = NULL;
            }
#endif

            /* This will set up the window correctly */
            ResizeWin(win, win->width, win->height);
        }
        else
        {
            /* Doesn't exist */
            angband_term[i] = NULL;
        }

        /* Term 0 is at the top */
        Zorder[i] = ANGBAND_TERM_MAX - i - 1;
    }

    /* Good to go... */
    Term_activate(term_screen);
}


/*
 * Set up some SDL stuff
 */
static void init_sdl_local(void)
{
    const SDL_VideoInfo *VideoInfo;
    int i;
    int h, w;
    char path[MSG_LEN];

    /* Get information about the video hardware */
    VideoInfo = SDL_GetVideoInfo();

    /* Require at least 256 colors */
    if (VideoInfo->vfmt->BitsPerPixel < 8)
        quit_fmt("This %s port requires lots of colors.", get_buildid(TRUE));

    full_w = VideoInfo->current_w;
    full_h = VideoInfo->current_h;

    /* Use a software surface - A tad inefficient, but stable... */
    vflags |= SDL_SWSURFACE;

    /* Set fullscreen flag */
    if (fullscreen) vflags |= SDL_FULLSCREEN;

    /* otherwise we make this surface resizable */
    else vflags |= SDL_RESIZABLE;

    /* Create the main window */
    AppWin = SDL_SetVideoMode(fullscreen? full_w: screen_w, fullscreen? full_h: screen_h, 0, vflags);

    /* Handle failure */
    if (!AppWin)
    {
        quit(format("Failed to create %dx%d window at %d bpp!", screen_w, screen_h,
            VideoInfo->vfmt->BitsPerPixel));
    }

    /* Set the window caption */
    SDL_WM_SetCaption(get_buildid(TRUE), NULL);

    /* Enable key repeating; use defaults */
    SDL_EnableKeyRepeat(SDL_DEFAULT_REPEAT_DELAY, SDL_DEFAULT_REPEAT_INTERVAL);

    /* Enable Unicode (so we can read key codes) */
    SDL_EnableUNICODE(1);

    /* Build a color for "black" that matches the pixel depth of this surface */
    back_colour.r = angband_color_table[TERM_DARK][1];
    back_colour.g = angband_color_table[TERM_DARK][2];
    back_colour.b = angband_color_table[TERM_DARK][3];
    back_pixel_colour = SDL_MapRGB(AppWin->format, back_colour.r, back_colour.g, back_colour.b);

    /* Initialize the colours */
    for (i = 0; i < MAX_COLORS; i++)
    {
        text_colours[i].r = angband_color_table[i][1];
        text_colours[i].g = angband_color_table[i][2];
        text_colours[i].b = angband_color_table[i][3];
    }

    /* Get the height of the status bar */
    sdl_CheckFont(DEFAULT_FONT_FILE, &w, &h);
    StatusHeight = h + 3;

    /* Font used for window titles */
    sdl_FontCreate(&SystemFont, DEFAULT_FONT_FILE, AppWin);

    /* Get the icon for display in the About box */
    path_build(path, sizeof(path), ANGBAND_DIR_XTRA_ICON, "att-128.png");
    if (file_exists(path))
        mratt = IMG_Load(path);
}


/*
 * Font sorting function
 *
 * Orders by width, then height, then face
 */
static int cmp_font(const void *f1, const void *f2)
{
    const char *font1 = *(const char **)f1;
    const char *font2 = *(const char **)f2;
    int height1, height2;
    int width1, width2;
    char face1[5], face2[5];

    sscanf(font1, "%dx%d%4s.", &width1, &height1, face1);
    sscanf(font2, "%dx%d%4s.", &width2, &height2, face2);

    if (width1 < width2)
        return -1;
    else if (width1 > width2)
        return 1;
    else
    {
        if (height1 < height2)
            return -1;
        else if (height1 > height2)
            return 1;
        else
            return strcmp(face1, face2);
    }
}


static void init_paths(void)
{
    int i;
    char path[MSG_LEN];
    char buf[MSG_LEN];
    ang_dir *dir;

    /* Build the gfx path */
    path_build(path, sizeof(path), ANGBAND_DIR_XTRA, "graf");
    ANGBAND_DIR_XTRA_GRAF = string_make(path);

    /* Build the "font" path */
    path_build(path, sizeof(path), ANGBAND_DIR_XTRA, "font");
    ANGBAND_DIR_XTRA_FONT = string_make(path);

    /* Build the "icon" path */
    path_build(path, sizeof(path), ANGBAND_DIR_XTRA, "icon");
    ANGBAND_DIR_XTRA_ICON = string_make(path);

    /* Build the filename */
    path_build(path, sizeof(path), ANGBAND_DIR_XTRA_FONT, DEFAULT_FONT_FILE);

    /* Hack -- Validate the basic font */
    validate_file(path);

    for (i = 0; i < MAX_FONTS; i++)
        FontList[i] = NULL;

    /** Scan for fonts **/

    /* Open the directory */
    dir = my_dopen(ANGBAND_DIR_XTRA_FONT);
    if (!dir) return;

    /* Read every font to the limit */
    while (my_dread(dir, buf, sizeof(buf)))
    {
        /* Check for file extension */
        if (suffix(buf, ".fon"))
            FontList[num_fonts++] = string_make(buf);

        /* Don't grow to long */
        if (num_fonts == MAX_FONTS) break;
    }

    sort(FontList, num_fonts, sizeof(FontList[0]), cmp_font);

    /* Done */
    my_dclose(dir);
}


/*
 * The SDL port's "main()" function.
 */
errr init_sdl(void)
{
    /* Remove W8080 warnings: SDL_Swap16/64 is declared but never used */
    SDL_Swap16(0);
    SDL_Swap64(0);

    /* Activate hook */
    plog_aux = hook_plog;

    /* Initialize SDL:  Timer, video, and audio functions */
    if (SDL_Init(SDL_INIT_VIDEO) < 0)
    {
        plog_fmt("Couldn't initialize SDL: %s\n", SDL_GetError());
        return (2);
    }

#ifdef USE_SOUND
    /* Initialise the mixer  */
    if (!open_audio())
    {
        SDL_Quit();
        return (2);
    }
#endif /* USE_SOUND */

    /* Initialize the TTF library */
    if (TTF_Init() < 0)
    {
        plog_fmt("Couldn't initialize TTF: %s\n", SDL_GetError());
        SDL_Quit();
        return (2);
    }

    /* Init some extra paths */
    init_paths();

    /* Load possible graphics modes */
    init_graphics_modes("graphics.txt");
    GfxButtons = mem_zalloc(sizeof(int) * (graphics_mode_high_id + 1));

    /* Load prefs */
    load_prefs();

    /* Get sdl going */
    init_sdl_local();

    /* Load window prefs */
    load_window_prefs();

    /* Prepare the windows */
    init_windows();

    /* Prepare the gfx */
    init_gfx();

    /* Prepare some more windows(!) */
    init_morewindows();

    /* Show on the screen */
    sdl_BlitAll();

    /* Activate hook */
    quit_aux = hook_quit;

    /* Register a control handler */
    if (!SetConsoleCtrlHandler((PHANDLER_ROUTINE)CtrlHandler, TRUE))
        quit("Could not set control handler");

    /* Paranoia */
    return (0);
}

#endif /* USE_SDL */

