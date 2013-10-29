/*
 * Copyright (c) 2007 Pete Mack and others
 * This code released under the Gnu Public License. See www.fsf.org
 * for current GPL license details. Addition permission granted to
 * incorporate modifications in all Angband variants as defined in the
 * Angband variants FAQ. See rec.games.roguelike.angband for FAQ.
 */



#ifndef INCLUDED_UI_H
#define INCLUDED_UI_H

/* ================== GEOMETRY ====================== */

/* Defines a rectangle on the screen that is bound to a Panel or subpanel */
typedef struct region region;

struct region {
	s16b col;	/* x-coordinate of upper right corner */
	s16b row;	/* y-coord of upper right coordinate */
	s16b width;	/* width of display area. 1 - use system default. */
			/* non-positive - rel to right of screen */
	s16b page_rows;	/* non-positive value is relative to the bottom of the screen */
};

/* Region that defines the full screen */
static const region SCREEN_REGION = {0, 0, 0, 0};

/* Erase the contents of a region */
void region_erase(const region *loc);

/* Check whether a (mouse) event is inside a region */
bool region_inside(const region *loc, const ui_event_data *key);


/*** Misc ***/

void window_make(s16b origin_x, s16b origin_y, s16b end_x, s16b end_y);


#endif /* INCLUDED_UI_H */
