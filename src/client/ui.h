/*
 * File: ui.h
 * Purpose: Generic ui functions
 */

#ifndef INCLUDED_UI_H
#define INCLUDED_UI_H

/* ================== GEOMETRY ====================== */

/* Region that defines the full screen */
extern const region SCREEN_REGION;

/* Erase the contents of a region */
extern void region_erase(const region *loc);

/* Erase the contents of a region + 1 char each way */
extern void region_erase_bordered(const region *loc);

/* Given a region with relative values, make them absolute */
extern region region_calculate(region loc);

/*** Misc ***/

extern void window_make(int origin_x, int origin_y, int end_x, int end_y); 

#endif /* INCLUDED_UI_H */



