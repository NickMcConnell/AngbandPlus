/*
 * File: z-type.h
 * Purpose: Helper classes for the display of typed data
 */

#ifndef INCLUDED_ZTYPE_H
#define INCLUDED_ZTYPE_H

typedef enum type_val
{
    T_END = 0,
    T_INTEGER,
    T_FLOAT,
    T_CHAR,
    T_STRING
} type_val;

typedef struct
{
    type_val t; 
    union
    {
        float f;
        int i;
        char c;
        const char *s;
    } u;
} type_union;

extern type_union null2u();
extern type_union i2u(int i);
extern type_union f2u(float f);
extern type_union c2u(char c);
extern type_union s2u(const char *s);

/* Helper classes for the display of typed data */

#define MAX_FMT 2
typedef struct
{
    byte color;
    const char *label;
    const char *fmt;            /* printf format argument */
    type_union value[MAX_FMT];  /* (short) argument list */
} data_panel;

/* Defines a rectangle on the screen that is bound to a Panel or subpanel */
typedef struct
{
    int col;        /* x-coordinate of upper right corner */
    int row;        /* y-coordinate of upper right corner */
    int width;      /* width of display area. 1 - use system default. */
                    /* non-positive - rel to right of screen */
    int page_rows;  /* non-positive value is relative to the bottom of the screen */
} region;

struct loc
{
    int x;
    int y;
};

struct cmp_loc
{
    int Ind;
    int x;
    int y;
};

/*
 * A set of points that can be constructed to apply a set of changes to
 */
struct point_set
{
    int n;
    int allocated;
    struct cmp_loc *pts;
};

extern struct point_set *point_set_new(int initial_size);
extern void point_set_dispose(struct point_set *ps);
extern void add_to_point_set(struct point_set *ps, int Ind, int y, int x);
extern int point_set_size(struct point_set *ps);

#endif /* INCLUDED_ZTYPE_H */
