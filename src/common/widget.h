/* File: widget.h */

/* Purpose: Widget definitions */

/*
 * Copyright (c) 1997-2009 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "widget-dll.h"

#ifndef GRID_EFFECT
#define WIDGET_EFFECT /* Mar 9 2009 */
#ifdef WIDGET_EFFECT
enum {
	WIDGET_EFFECT_INVERT = 0x0001,
};
typedef struct ExWidgetEffect {
	IconSpec icon;
	int flags; /* WIDGET_EFFECT_XXX */
} ExWidgetEffect;
#endif /* WIDGET_EFFECT */
#endif /* GRID_EFFECT */

#define PLAYER_HEALTH_BAR /* May 7 2009 */

/* Visual indication on micro-maps where player/stairs are located. */
enum {
	BLINK_NONE = 0,
	BLINK_UP,
	BLINK_DOWN,
	BLINK_PLAYER
};
typedef struct MapBlinker MapBlinker;
struct MapBlinker
{
    int type; /* BLINK_XXX */
    int x;
    int y;
    MapBlinker *next;
};

/* Extended Widget record */
typedef struct ExWidget {
	Widget widget;
	int spriteCnt;
	int vaultNum;
	void *vaultPtr;
#ifdef GRID_EFFECT
#elif defined(WIDGET_EFFECT)
	ExWidgetEffect *effect; /* Per-tile effect icons */
#else /* WIDGET_EFFECT */
	IconSpec *effect; /* Per-tile effect icons */
#endif /* WIDGET_EFFECT */

	/* WIDGET_STYLE_ICON WIDGET_STYLE_ISO */
	void (*whatToDrawProc)(Widget *widgetPtr, int y, int x, t_display *wtd);

	/* WIDGET_STYLE_MAP */
	int (*symbolProc)(Widget *widgetPtr, int y, int x); /* For micro-map */

	/* WIDGET_STYLE_ISO */
	int hit, hitx, hity;		/* Highlight */

#ifdef PLAYER_HEALTH_BAR
	struct {
		bool drawn;
		int width;
		XColor *firstColorPtr;
		XColor *secondColorPtr;
	} healthBar;
#endif /* PLAYER_HEALTH_BAR */

	int blink; /* -blink */
	XColor *blink1ColorPtr; /* -blinkupcolor */
	XColor *blink2ColorPtr; /* -blinkdowncolor */
	XColor *blink3ColorPtr; /* -blinkplayercolor */
	MapBlinker *blinkPtr; /* For micro-map */

#ifdef WIDGET_STYLE_TEXT
    GC gcFg, gcBg, gcTerm[16];
    Tk_Font tkfont;
    Tk_FontMetrics fm;
#endif
} ExWidget;

extern void Widget_SetVault(Widget *widgetPtr);

