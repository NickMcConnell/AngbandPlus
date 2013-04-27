// File: sys-win/main.cpp
// Main system-specific file for Win32/DirectX machines


/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

/*
 * Original code by Billy Tanksley (wtanksle@ucsd.edu)
 *
 * Support for DJGPP v2 by Scott Egashira (egashira@u.washington.edu)
 *
 * Extensive modifications by Ben Harrison (benh@voicenet.com).
 *
 * True color palette support by Mike Marcelais (mrmarcel@eos.ncsu.edu),
 * with interface to the "color_table" array by Ben Harrison.  Matt
 * Craighead also helped with developing and testing the palette code.
 *
 * Massive revision for graphics and other stuff by Matt Craighead.
 *
 * Rewritten for Win32 by Matt Craighead.
 */


#include "sys-win.h"


// Include the character bitmaps, palette data, mouse cursor, and fonts
#include "chars.h"
#include "palette.h"
#include "cursor.h"
#include "bold.h"
#include "regular.h"


struct CFrame {
    byte height;
    u16b ndata;
    byte *data;
};

struct CScene {
    int nframes;
    char *name;
    CFrame *frames; // allocate nviews*nframes of these
};

struct CTile {
    char *name;
    int nviews;
    int nscenes;
    CScene *scenes;
};

// Maximum number of tiles
const int MAX_TILES = 1024;

// The graphics mode we are using
const int VESA_MODE = 0x101; // 640x480, 256 colors

// The height/width of the graphics mode
const int SCREEN_WIDTH = 640;
const int SCREEN_HEIGHT = 480;


// Darkening info
static byte darken_tab[256];

// The tile database
static int ntiles;
static CTile *tiles[MAX_TILES];

// The current clipping rectangle
static int cx1 = 0, cy1 = 0, cx2 = 639, cy2 = 479;

// Verify against the clipping rectangle
#define test_clip(x, y) (((x) < cx1) || ((x) > cx2) || ((y) < cy1) || ((y) > cy2))


// The DirectX wrapper
CDirectX *cdx;

// Our window
HWND hWnd;

// Message-related data
int mouse_x, mouse_y;
bool left_down;
bool left_click, right_click;
int left_x, left_y;
int right_x, right_y;
LARGE_INTEGER freq, restart;
bool bActive, bQuit;


/*
 * Get all pending messages
 */
static void message_flush(void)
{
    MSG msg;

redo:
    while (PeekMessage(&msg, NULL, 0, 0, PM_REMOVE)) {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }

    if (!bActive && !bQuit) {
        WaitMessage();
        goto redo;
    }
}


/*
 * Flush changes to the screen
 */
void screen_refresh(void)
{
    // Call the DX wrapper
    cdx->ScreenRefresh();
}


// Timer Code
bool install_timer(void)
{
    QueryPerformanceCounter(&restart);
    if (!QueryPerformanceFrequency(&freq)) return FALSE;
    freq.QuadPart /= 146;
    return TRUE;
}

u32b get_timer_value(void)
{
    LARGE_INTEGER timer;
    QueryPerformanceCounter(&timer);
    return (timer.QuadPart - restart.QuadPart) / freq.QuadPart;
}

void reset_timer(void)
{
    LARGE_INTEGER timer, excess;
    QueryPerformanceCounter(&timer);
    excess.QuadPart = (timer.QuadPart - restart.QuadPart) % freq.QuadPart;
    QueryPerformanceCounter(&restart);
    restart.QuadPart -= excess.QuadPart;
}


/*
 * Set a clipping rectangle
 */
void set_clip_rect(int x1, int y1, int x2, int y2)
{
    cx1 = x1; cx2 = x2; cy1 = y1; cy2 = y2;
}


/*
 * Remove any clipping rectangle
 */
void clear_clip_rect(void) { set_clip_rect(0, 0, 639, 479); }


/*
 * Is shift depressed?
 */
bool get_shift(void)
{
    if (GetAsyncKeyState(VK_SHIFT) & 0x8000) return TRUE;
    return FALSE;
}


/*
 * Is caps lock on?
 */
bool get_capslock(void)
{
    if (GetKeyState(VK_CAPITAL) & 1) return TRUE;
    return FALSE;
}


/*
 * Key queue code
 */
#define KEYQ_SIZE 256
static int queue[KEYQ_SIZE];
static int queue_start, queue_end;
bool queue_empty(void) { return (queue_start == queue_end); }
void enqueue_key(int k) { queue[queue_end++] = k; queue_end = queue_end % KEYQ_SIZE; }
int dequeue_key(void)
{
    int v = queue[queue_start++];
    queue_start = queue_start % KEYQ_SIZE;
    return v;
}


/*
 * Scan for a keypress; return its scan code or 0
 */
int scan_inkey_scan(void)
{
    message_flush();
    if (queue_empty()) return 0;
    return dequeue_key();
}


// Save/restore the virtual screen
byte *save_screen(void)
{
    byte *rval = new byte[SCREEN_HEIGHT*SCREEN_WIDTH];
    LONG pitch;
    byte *vs;

    // Lock using wrapper
    cdx->Lock(&vs, &pitch);

    // Copy data
    for (int y = 0; y < SCREEN_HEIGHT; y++) {
        memcpy(rval + y*SCREEN_WIDTH, vs + y*pitch, SCREEN_WIDTH);
    }

    // Unlock using wrapper
    cdx->Unlock();

    return rval;
}

void restore_screen(byte *from)
{
    LONG pitch;
    byte *vs;

    // Lock using wrapper
    cdx->Lock(&vs, &pitch);

    // Copy data
    for (int y = 0; y < SCREEN_HEIGHT; y++) {
        memcpy(vs + y*pitch, from + y*SCREEN_WIDTH, SCREEN_WIDTH);
    }

    // Unlock using wrapper
    cdx->Unlock();
}

/*
 * Get mouse location with button status
 *
 * x = x coordinate
 * y = y coordinate
 * left = left button
 */
void get_mouse_status(int *x, int *y, bool *left)
{
    message_flush();
    *x = mouse_x; *y = mouse_y;
    *left = left_down;
}


/*
 * Get last release of left mouse button
 */
bool get_last_left_button_release(int *rx, int *ry)
{
    if (!left_click) return FALSE;
    left_click = FALSE;
    *rx = left_x;
    *ry = left_y;
    return TRUE;
}


/*
 * Get last release of right mouse button
 */
bool get_last_right_button_release(int *rx, int *ry)
{
    if (!right_click) return FALSE;
    right_click = FALSE;
    *rx = right_x;
    *ry = right_y;
    return TRUE;
}


/*
 * Mouse stuff
 */
static byte backup[160];

void virt_draw_mouse(int x, int y)
{
    byte *screen_p;
    int pixel;
    int xx, yy;
    byte data;
    byte *vs;
    LONG pitch;

    // Lock using wrapper
    cdx->Lock(&vs, &pitch);

    for (pixel = 0; pixel < 160; pixel++) {
        xx = (pixel % 10) + x - hotspot_x;
        if ((xx < 0) || (xx > SCREEN_WIDTH-1)) continue;
        yy = (pixel / 10) + y - hotspot_y;
        if ((yy < 0) || (yy > SCREEN_HEIGHT-1)) continue;
        screen_p = &vs[yy*pitch + xx];
        backup[pixel] = *screen_p;
        data = mouse_pointer_data[pixel];
        if (data != 255) *screen_p = data;
    }

    // Unlock using wrapper
    cdx->Unlock();
}

void virt_kill_mouse(int x, int y)
{
    int pixel;
    int xx, yy;
    byte *vs;
    LONG pitch;

    // Lock using wrapper
    cdx->Lock(&vs, &pitch);

    for (pixel = 0; pixel < 160; pixel++) {
        xx = (pixel % 10) + x - hotspot_x;
        if ((xx < 0) || (xx > SCREEN_WIDTH-1)) continue;
        yy = (pixel / 10) + y - hotspot_y;
        if ((yy < 0) || (yy > SCREEN_HEIGHT-1)) continue;
        vs[yy*pitch + xx] = backup[pixel];
    }

    // Unlock using wrapper
    cdx->Unlock();
}


/*
 * Set a palette entry
 *
 * c, r, g, b all from 0..255
 *
 * WARNING: do not use except at times when it is safe!
 */
void set_palette_entry(int c, int r, int g, int b)
{
    // Set using the DX wrapper
    cdx->SetPaletteEntry(c, r, g, b);
}

/*
 * Reset the palette to the default
 */
void set_default_palette(void)
{
    for (int c = 0; c < 256; c++) {
        set_palette_entry(c, palette[c*3], palette[c*3+1], palette[c*3+2]);
    }
}


/*
 * Starting here are the graphics primitives.
 */


// Begin or end a batch of pixel draws
static byte *lpVirtScreen;
static LONG lPitch;

void start_pixel_draw(void)
{
    // Lock using wrapper
    cdx->Lock(&lpVirtScreen, &lPitch);
}

void end_pixel_draw(void)
{
    // Unlock using wrapper
    cdx->Unlock();
}


// Draw a pixel to the virtual screen
void draw_pixel(int x, int y, byte c)
{
    if (!test_clip(x, y)) {
        lpVirtScreen[y*lPitch + x] = c;
    }
}

// Draw a box
void box(int x1, int y1, int x2, int y2, byte color)
{
    // Clip
    if ((x1 > cx2) || (x2 < cx1)) return;
    if (x2 < x1) return;
    if (x1 < cx1) x1 = cx1;
    if (x2 > cx2) x2 = cx2;
    if ((y1 > cy2) || (y2 < cy1)) return;
    if (y2 < y1) return;
    if (y1 < cy1) y1 = cy1;
    if (y2 > cy2) y2 = cy2;

    // Call the DX wrapper
    cdx->FillRect(x1, y1, x2, y2, color);
}

// Set the whole screen to one color
// Bypasses any clipping rectangle
void blank_screen(byte color)
{
    // Call the DX wrapper
    cdx->FillRect(0, 0, SCREEN_WIDTH-1, SCREEN_HEIGHT-1, color);
}

// Draw a horizontal or vertical line
void horiz_line(int x1, int x2, int y, byte color)
{
    // Clip
    if ((y < cy1) || (y > cy2)) return;
    if ((x1 > cx2) || (x2 < cx1)) return;
    if (x2 < x1) return;
    if (x1 < cx1) x1 = cx1;
    if (x2 > cx2) x2 = cx2;

    // Call the DX wrapper
    cdx->FillRect(x1, y, x2, y, color);
}

void vert_line(int x, int y1, int y2, byte color)
{
    // Clip
    if ((x < cx1) || (x > cx2)) return;
    if ((y1 > cy2) || (y2 < cy1)) return;
    if (y2 < y1) return;
    if (y1 < cy1) y1 = cy1;
    if (y2 > cy2) y2 = cy2;

    // Call the DX wrapper
    cdx->FillRect(x, y1, x, y2, color);
}


// Draw a rectangle
void rectangle(int x1, int y1, int x2, int y2, byte color)
{
    horiz_line(x1, x2, y1, color);
    horiz_line(x1, x2, y2, color);
    vert_line(x1, y1, y2, color);
    vert_line(x2, y1, y2, color);
}

// Draw a character of text
static void put_character(int x, int y, int c, byte color, byte *vs, LONG pitch)
{
    int cx, cy, px, py;

    for (cy = 0; cy < 16; cy++) {
        py = y+cy;
        if ((py < cy1) || (py > cy2)) continue;
        for (cx = 0; cx < 8; cx++) {
            px = x+cx;
            if ((px < cx1) || (px > cx2)) continue;
            if ((char_map[c*16 + cy] >> (7-cx)) & 1) {
                vs[py*pitch + px] = color;
            }
        }
    }
}

// Draw a string
void put_string(int x, int y, char *c, byte color)
{
    int i, len = strlen(c);
    byte *vs;
    LONG pitch;

    // Lock using wrapper
    cdx->Lock(&vs, &pitch);

    // Dump characters
    for (i = 0; i < len; i++) {
        put_character(x + 8*i, y, c[i], color, vs, pitch);
    }

    // Unlock using wrapper
    cdx->Unlock();
}

int get_char_width(int c, int font)
{
    if ((c < 32) || (c > 127)) return 0;

    switch (font) {
        case FONT_REGULAR: return font_regular_widths[c-32];
        case FONT_BOLD:    return font_bold_widths[c-32];
        default:           return 0;
    }
}

int string_width(char *c, int font)
{
    int i, len = strlen(c);
    int pixlen = 0;

    for (i = 0; i < len; i++) {
        pixlen += get_char_width(c[i], font);
    }
    return pixlen;
}

// Draw a character of text in a font
static void put_character_font(int x, int y, int c, byte color, int font, byte *vs, LONG pitch)
{
    int cx, cy, px, py;
    int height, width, ohang;
    int data[16], i;

    if ((c < 32) || (c > 127)) return;

    switch (font) {
        case FONT_REGULAR:
            height = font_regular_height;
            ohang = font_regular_overhang;
            width = font_regular_widths[c-32];
            for (i = 0; i < height; i++) data[i] = font_regular_data[(c-32)*height+i];
            break;

        case FONT_BOLD:
            height = font_bold_height;
            ohang = font_bold_overhang;
            width = font_bold_widths[c-32];
            for (i = 0; i < height; i++) data[i] = font_bold_data[(c-32)*height+i];
            break;

        default: return;
    }

    for (cy = 0; cy < height; cy++) {
        py = y+cy;
        if ((py < cy1) || (py > cy2)) continue;
        for (cx = 0; cx < width+ohang; cx++) {
            px = x+cx;
            if ((px < cx1) || (px > cx2)) continue;
            if ((data[cy] >> cx) & 1) {
                vs[py*pitch + px] = color;
            }
        }
    }
}

// Draw a string in a font
void put_text(int x, int y, char *c, byte color, int font)
{
    int i, len = strlen(c);
    byte *vs;
    LONG pitch;

    // Lock using wrapper
    cdx->Lock(&vs, &pitch);

    // Dump characters
    for (i = 0; i < len; i++) {
        put_character_font(x, y, c[i], color, font, vs, pitch);
        x += get_char_width(c[i], font);
    }

    // Unlock using wrapper
    cdx->Unlock();
}


/*
 * Load a tile
 */
void load_tile(char *filename)
{
    FILE *f;
    char c, buf[80];
    int i, j, k;
    CTile *map;

    strcpy(buf, "dat/tile/");
    strcat(buf, filename);
    strcat(buf, ".til");

    f = fopen(buf, "rb");
    if (f == NULL) {
        quit(format("Missing tilemap: %s\n", filename));
    }

    // Signature
    c = fgetc(f);
    if (c != 'T') {
        quit(format("Bad signature in tilemap: %s.\n", filename));
    }
    c = fgetc(f);
    if (c != 'I') {
        quit(format("Bad signature in tilemap: %s.\n", filename));
    }
    c = fgetc(f);
    if (c != 'L') {
        quit(format("Bad signature in tilemap: %s.\n", filename));
    }

    // Revision
const int PROPER_REVISION = 2;
    c = fgetc(f);
    if (c != PROPER_REVISION) {
        quit(format("Improper revision in tilemap %s: %d.  This version uses revision %d.",
            filename, c, PROPER_REVISION));
    }

    // Create a tilemap
    map = new CTile;

    // Set tile name
    map->name = new char[strlen(filename)+1];
    strcpy(map->name, filename);

    // Get nviews
    map->nviews = fgetc(f);

    // Get nscenes, allocate scenes
    map->nscenes = fgetc(f);
    map->scenes = new CScene[map->nscenes];

    // For each scene...
    for (i = 0; i < map->nscenes; i++) {
        CScene *scene = &map->scenes[i];

        // Get number of frames, allocate frames
        scene->nframes = fgetc(f);
        scene->frames = new CFrame[map->nviews * scene->nframes];

        // Get name
        j = 0;
        for (;;) {
            char c = fgetc(f);
            buf[j++] = c;
            if (!c) break;
        }
        scene->name = new char[j];
        strcpy(scene->name, buf);

        // For each frame and view...
        for (j = 0; j < scene->nframes*map->nviews; j++) {
            CFrame *frame = &scene->frames[j];

            // Height
            frame->height = fgetc(f);

            // The data
            frame->ndata = fgetc(f);
            frame->ndata += fgetc(f) << 8;
            frame->data = new byte[frame->ndata];
            for (k = 0; k < frame->ndata; k++) frame->data[k] = fgetc(f);
        }
    }

    // Save the tile
    tiles[ntiles] = map;
    ntiles++;

    // Done
    fclose(f);
}


/*
 * A compare function for sort_tiles.
 */
int tile_compare(const void *a, const void *b)
{
    CTile *x = *((CTile **) a);
    CTile *y = *((CTile **) b);
    return strcmp(x->name, y->name);
}


/*
 * Sort the tiles once they are all loaded, for purposes of binary-search access.
 */
void sort_tiles(void)
{
    qsort(tiles, ntiles, sizeof(CTile *), tile_compare);
}


/*
 * Draw an isometric tile at isometric location (ix, iy).
 *
 * The screen is laid out in this fashion:
 *
 * ----------------------
 * |(-1,  /\            /
 * | 0)  /  \  (0,-1)  /
 * |    /    \        /
 * |   /      \      /
 * |  /        \    /
 * | /          \  /
 * |/   (0,0)    \/
 * |\            /\
 * | \          /  \
 * |  \        /    \
 * |   \      /      \
 * |    \    /        \
 * |     \  /          \
 * |(0,1) \/    (1,0)   \
 *
 * and so on.
 */
static void draw_iso_tile(int off_x, int off_y, CFrame *frame, bool darken)
{
    int n, x, y;
    byte *d = frame->data;
    bool can_plot;
    byte *vs;
    LONG pitch;

    off_y += ISO_HEIGHT - frame->height;

    x = off_x; y = off_y;

    if ((x > cx2) || (y > cy2)) return;
    if (x+ISO_WIDTH-1 < cx1) return;

    // Lock using DX wrapper
    cdx->Lock(&vs, &pitch);

    for (;;) {
        can_plot = ((y >= cy1) && (x <= cx2));
        switch (*d++) {
            case 0:
                n = *d++;
                if (can_plot) {
                    while ((x < cx1) && (n > 0)) {
                        x++; n--; d++;
                    }
                    if (x+n-1 <= cx2) {
                        if (!darken) {
                            while (n >= 4) {
                                int py = pitch*y;
                                *((u32b *) (vs+py+x)) = *((u32b *) d);
                                n -= 4; d += 4; x += 4;
                            }
                            for (; n > 0; n--) {
                                int py = pitch*y;
                                vs[py+x] = *d;
                                d++; x++;
                            }
                        }
                        else {
                            for (; n > 0; n--) {
                                int py = pitch*y;
                                vs[py+x] = darken_tab[*d];
                                d++; x++;
                            }
                        }
                    }
                    else {
                        for (; n > 0; n--) {
                            int py = pitch*y;
                            if (x <= cx2) {
                                if (!darken) {
                                    vs[py+x] = *d;
                                }
                                else {
                                    vs[py+x] = darken_tab[*d];
                                }
                            }
                            d++; x++;
                        }
                    }
                }
                else {
                    d += n;
                }
                break;
            case 1:
                x += *d++;
                break;
            case 2:
                x = off_x;
                y++;
                if (y > cy2) goto done;
                break;
            case 3: goto done;
        }
    }

done:
    // Unlock using DX wrapper
    cdx->Unlock();
}

/*
 * Find an isometric tile by name.
 */
static int find_tile(char *name, int first, int last)
{
    if (first == last) {
        if (strcmp(name, tiles[first]->name) == 0) return first;
        return -1;
    }
    else if (last == first+1) {
        if (strcmp(name, tiles[first]->name) == 0) return first;
        if (strcmp(name, tiles[last]->name) == 0) return last;
        return -1;
    }
    else {
        int middle, result;
        middle = (first+last)/2;
        result = strcmp(tiles[middle]->name, name);
        if (result == 0) return middle;
        else if (result < 0) return find_tile(name, middle, last);
        else return find_tile(name, first, middle);
    }
}

/*
 * A global version of find_tile for general use
 */
int locate_tile(char *name)
{
    return find_tile(name, 0, ntiles-1);
}

/*
 * Draw an isometric tile given its index
 */
void draw_tile_idx(int off_x, int off_y, int index, bool darken, char *scene_name, int view,
    int frame)
{
    CTile *tile = tiles[index];
    CScene *scene = NULL;
    int i;
    
    // Find scene
    for (i = 0; i < tile->nscenes; i++) {
        if (!strcmp(tile->scenes[i].name, scene_name)) {
            scene = &tile->scenes[i];
            break;
        }
    }
    if (!scene) {
        quit(format("Could not find scene %s\n"));
    }

    draw_iso_tile(off_x, off_y, &scene->frames[tile->nviews*frame + view], darken);
}

/*
 * Draw an isometric tile given its name
 */
void draw_tile(int off_x, int off_y, char *tile_name, bool darken, char *scene_name,
    int view, int frame)
{
    int result = locate_tile(tile_name);
    if (result == -1) {
        quit(format("Could not find tile: %s", tile_name));
    }
    else draw_tile_idx(off_x, off_y, result, darken, scene_name, view, frame);
}

/*
 * Miscellaneous system routines
 */
void bell(void) { MessageBeep(0xFFFFFFFF); }


/*
 * Dump the screen into the .pnm format.  You must then use a converter
 * if you want a decent format, but this format is a pretty easy one
 * to code for and I found a converter on the 'net that works well.
 * Someday I might rewrite this to dump in a better format.
 */
void dump_screen(void)
{
    FILE *f;
    int x, y;
    int color;
    byte *vs;
    LONG pitch;

    // Lock using wrapper
    cdx->Lock(&vs, &pitch);

    f = fopen("dump.pnm", "wt");
    fprintf(f, "P3\n");
    fprintf(f, "640 480\n");
    fprintf(f, "255\n");
    for (y = 0; y < 480; y++) {
        for (x = 0; x < 640; x++) {
            color = vs[y*pitch+x];
            fprintf(f, "%d %d %d ", palette[color*3], palette[color*3+1],
                palette[color*3+2]);
        }
    }
    fclose(f);

    // Unlock using wrapper
    cdx->Unlock();
}


/*
 * The WindowProc for the game
 */
LRESULT CALLBACK WndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
    int scan;

    switch (message) {
        case WM_ACTIVATEAPP:
            if (wParam) bActive = TRUE;
            else bActive = FALSE;
            return 0;

        case WM_SETCURSOR:
            SetCursor(NULL);
            return TRUE;

        case WM_DESTROY:
            bQuit = TRUE;
            PostQuitMessage(0);
            break;

        case WM_LBUTTONDOWN:
            left_down = TRUE;
            break;

        case WM_LBUTTONUP:
            left_down = FALSE;
            left_click = TRUE;
            left_x = LOWORD(lParam);
            left_y = HIWORD(lParam);
            break;

        case WM_RBUTTONUP:
            right_click = TRUE;
            right_x = LOWORD(lParam);
            right_y = HIWORD(lParam);
            break;

        case WM_MOUSEMOVE:
            mouse_x = LOWORD(lParam);
            mouse_y = HIWORD(lParam);
            break;

        case WM_KEYDOWN:
        case WM_SYSKEYDOWN:
            //# Handle keys that aren't important, like shift/ctrl/alt
            scan = LOBYTE(HIWORD(lParam));
            if (scan_valid(scan)) enqueue_key(scan);
            break;

        case WM_SYSCOMMAND:
            switch (wParam & 0xFFF0) {
                case SC_SCREENSAVE:
                case SC_MONITORPOWER:
                case SC_CLOSE:
                    return 0;
            }
            break;
    }
    return DefWindowProc(hWnd, message, wParam, lParam);
}


/*
 * The WinMain() function for the game
 */
int APIENTRY WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpszCmdLine,
    int nCmdShow)
{
    WNDCLASSEX wcex;

    // Hack -- fix file modes
    _fmode = O_BINARY;

    // Register window class
    wcex.cbSize = sizeof(wcex);
    wcex.style = CS_HREDRAW | CS_VREDRAW;
    wcex.lpfnWndProc = WndProc;
    wcex.cbClsExtra = 0;
    wcex.cbWndExtra = 0;
    wcex.hInstance = hInstance;
    wcex.hIcon = LoadIcon(NULL, IDI_WINLOGO);
    wcex.hIconSm = LoadIcon(NULL, IDI_WINLOGO);
    wcex.hCursor = NULL;
    wcex.hbrBackground = NULL;
    wcex.lpszMenuName = "Utumno";
    wcex.lpszClassName = "Utumno";
    RegisterClassEx(&wcex);

    // Create window
    hWnd = CreateWindow("Utumno", "Utumno", WS_POPUP | WS_VISIBLE, 0, 0,
        GetSystemMetrics(SM_CXSCREEN), GetSystemMetrics(SM_CYSCREEN), NULL, NULL,
        hInstance, NULL);
    if (!hWnd) quit("Could not create window");
    
    // Display/update window
    ShowWindow(hWnd, nCmdShow);
    UpdateWindow(hWnd);
    SetFocus(hWnd);

    // Set up DirectX with the wrapper
    cdx = new CDirectX(hWnd);


    // Install the timer
    install_timer();

    // Set the color regs
    set_default_palette();


    // Create darken table
    for (int c = 0; c < 256; c++) {
        int r, g;
        r = c/12;
        g = c%12;
        if ((r != 0) || (g != 0)) {
            g += 2;
            if (g >= 12) g = 11;
        }
        darken_tab[c] = r*12+g;
    }

    // Handle messages once
    message_flush();

    // Play the game
    play_game();

    // Quit
    quit(NULL);

    // Exit
    return 0;
}



/*
 * Exit (ala "exit()").  If 'str' is NULL, do "exit(0)".
 * If 'str' begins with "+" or "-", do "exit(atoi(str))".
 * Otherwise, output str and exit with an error code of -1.
 * But always use 'quit_aux', if set, before anything else.
 */
void quit(char *str)
{
    // Kill system-specific stuff
    if (cdx) delete cdx;

    // If there is an error, display it
    if (str) MessageBox(hWnd, str, "Utumno Error", MB_OK | MB_ICONERROR);

    // Exit
    DestroyWindow(hWnd);
    message_flush();
    ExitProcess(0);
}


/*
 * Convert a scan code along with the status of the modifiers to an ASCII code
 */
int convert(int scan, bool shift, bool caps)
{
    // Shift effectively switches caps lock
    if (shift) caps = !caps;

    switch (scan) {
        case KEY_ESCAPE:
            return ESCAPE;
        case KEY_1:
            if (shift) return '!';
            return '1';
        case KEY_2:
            if (shift) return '@';
            return '2';
        case KEY_3:
            if (shift) return '#';
            return '3';
        case KEY_4:
            if (shift) return '$';
            return '4';
        case KEY_5:
            if (shift) return '%';
            return '5';
        case KEY_6:
            if (shift) return '^';
            return '6';
        case KEY_7:
            if (shift) return '&';
            return '7';
        case KEY_8:
            if (shift) return '*';
            return '8';
        case KEY_9:
            if (shift) return '(';
            return '9';
        case KEY_0:
            if (shift) return ')';
            return '0';
        case KEY_MINUS:
            if (shift) return '_';
            return '-';
        case KEY_EQUAL:
            if (shift) return '+';
            return '=';
        case KEY_BACKSPACE:
            return '\b';
        case KEY_TAB:
            return '\t';
        case KEY_Q:
            if (caps) return 'Q';
            return 'q';
        case KEY_W:
            if (caps) return 'W';
            return 'w';
        case KEY_E:
            if (caps) return 'E';
            return 'e';
        case KEY_R:
            if (caps) return 'R';
            return 'r';
        case KEY_T:
            if (caps) return 'T';
            return 't';
        case KEY_Y:
            if (caps) return 'Y';
            return 'y';
        case KEY_U:
            if (caps) return 'U';
            return 'u';
        case KEY_I:
            if (caps) return 'I';
            return 'i';
        case KEY_O:
            if (caps) return 'O';
            return 'o';
        case KEY_P:
            if (caps) return 'P';
            return 'p';
        case KEY_LBRACKET:
            if (shift) return '{';
            return '[';
        case KEY_RBRACKET:
            if (shift) return '}';
            return ']';
        case KEY_ENTER:
            return '\n';
        case KEY_A:
            if (caps) return 'A';
            return 'a';
        case KEY_S:
            if (caps) return 'S';
            return 's';
        case KEY_D:
            if (caps) return 'D';
            return 'd';
        case KEY_F:
            if (caps) return 'F';
            return 'f';
        case KEY_G:
            if (caps) return 'G';
            return 'g';
        case KEY_H:
            if (caps) return 'H';
            return 'h';
        case KEY_J:
            if (caps) return 'J';
            return 'j';
        case KEY_K:
            if (caps) return 'K';
            return 'k';
        case KEY_L:
            if (caps) return 'L';
            return 'l';
        case KEY_SEMICOLON:
            if (shift) return ':';
            return ';';
        case KEY_QUOTE:
            if (shift) return '"';
            return '\'';
        case KEY_BACKQUOTE:
            if (shift) return '~';
            return '`';
        case KEY_BACKSLASH:
            if (shift) return '|';
            return '\\';
        case KEY_Z:
            if (caps) return 'Z';
            return 'z';
        case KEY_X:
            if (caps) return 'X';
            return 'x';
        case KEY_C:
            if (caps) return 'C';
            return 'c';
        case KEY_V:
            if (caps) return 'V';
            return 'v';
        case KEY_B:
            if (caps) return 'B';
            return 'b';
        case KEY_N:
            if (caps) return 'N';
            return 'n';
        case KEY_M:
            if (caps) return 'M';
            return 'm';
        case KEY_COMMA:
            if (shift) return '<';
            return ',';
        case KEY_PERIOD:
            if (shift) return '>';
            return '.';
        case KEY_SLASH:
            if (shift) return '?';
            return '/';
        case KEYPAD_STAR:
            return '*';
        case KEY_SPACE:
            return ' ';
        case KEYPAD_MINUS:
            return '-';
        case KEYPAD_PLUS:
            return '+';
    }
    return 0;
}