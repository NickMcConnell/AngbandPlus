/*
 * File: list-square-flags.h
 * Purpose: Special grid flags
 */

/* symbol descr */
SQUARE(NONE, "")
SQUARE(MARK, "memorized feature")                       /* player */
SQUARE(GLOW, "self-illuminating")                       /* global */
SQUARE(VAULT, "part of a vault")                        /* global */
SQUARE(ROOM, "part of a room")                          /* global */
SQUARE(SEEN, "seen flag")                               /* player */
SQUARE(VIEW, "view flag")                               /* player */
SQUARE(WASSEEN, "previously seen (during update)")      /* global */
SQUARE(DTRAP, "trap detected square")                   /* player */
SQUARE(FEEL, "hidden points to trigger feelings")       /* player + global */
SQUARE(DEDGE, "border of trap detected area")           /* player */
SQUARE(TRAP, "square containing a known trap")          /* global */
/*SQUARE(INVIS, "square containing an unknown trap")*/      /* global */
SQUARE(WALL_INNER, "inner wall generation flag")        /* global */
SQUARE(WALL_OUTER, "outer wall generation flag")        /* global */
SQUARE(WALL_SOLID, "solid wall generation flag")        /* global */
SQUARE(MON_RESTRICT, "no random monster flag")          /* global */
SQUARE(NO_TELEPORT, "can't teleport from this square")  /* global */
SQUARE(NO_MAP, "square can't be magically mapped")      /* global */
SQUARE(NO_ESP, "telepathy doesn't work on this square") /* global */
SQUARE(PROJECT, "marked for projection processing")     /* global */
