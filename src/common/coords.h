/*
 * File: coords.h
 * Purpose: Coordinates definition
 */

#ifndef COORDS_H
#define COORDS_H

/* Coordinates on the world map */
struct worldpos
{
    s16b wy;        /* The wilderness coordinates */
    s16b wx;
    s16b depth;     /* Cur depth */
};

/* Setter */
#define COORDS_SET(C, Y, X, D) \
    (C)->wy = (Y); \
    (C)->wx = (X); \
    (C)->depth = (D)

/* Macros */
#define COORDS_EQUAL(C1, C2) \
    (((C1)->wy == (C2)->wy) && ((C1)->wx == (C2)->wx) && ((C1)->depth == (C2)->depth))

#define COORDS_NULL(C) \
    (((C)->wy == 0) && ((C)->wx == 0) && ((C)->depth == 0))

#endif /* COORDS_H */
