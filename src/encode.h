/*
 * Convert a "location" (Y,X) into a "grid" (G)
 */
#define GRID(Y,X) \
	((((u16b)(Y) & 0xFF) << 8) + ((u16b)(X) & 0xFF))

/*
 * Convert a "grid" (G) into a "location" (Y)
 */
#define GRID_Y(G) \
	(((G) >> 8) & 0xFF)

/*
 * Convert a "grid" (G) into a "location" (X)
 */
#define GRID_X(G) \
	(((G) >> 0) & 0xFF)

/*
 * Convert an attr/char pair into a pict P
 */
#define PICT(A,C) \
	(((u16b)((byte)(A)) << 8) + ((u16b)((byte)(C)) << 0))

/*
 * Convert a pict P into an attr A
 */
#define PICT_A(P) \
	((byte)(((P) >> 8) & 0xFF))

/*
 * Convert a pict P into a char C
 */
#define PICT_C(P) \
	((char)(((P) >> 0) & 0xFF)))

/*
 * Convert a default attr DA, default char DC, desired attr XA, and desired char XC into a visual V
 */
#define VISUAL_DX(DA,DC,XA,XC) \
	(((u32b)((byte)(DA)) << 24) + ((u32b)((byte)(DC)) << 16) + \
	 ((u32b)((byte)(XA)) <<  8) + ((u32b)((byte)(XC)) <<	0)))

/*
 * Convert a default pict D and a desired pict X into a visual V
 */
#define VISUAL_P(D,X) \
	((((u32b)(D) & 0xFFFF) << 16) + ((u32b)(X) & 0xFFFF))

/*
 * Convert a visual V into a default pict D
 */
#define VISUAL_D(V) \
	(((V) >> 16) & 0xFFFF)

/*
 * Convert a visual V into a desired pict X
 */
#define VISUAL_X(V) \
	(((V) >>  0) & 0xFFFF)

/*
 * Convert a visual V into a default attr DA
 */
#define VISUAL_DA(V) \
	((byte)(((V) >> 24) & 0xFF))

/*
 * Convert a visual V into a default char DC
 */
#define VISUAL_DC(V) \
	((char)(((V) >> 16) & 0xFF))

/*
 * Convert a visual V into a desired attr XA
 */
#define VISUAL_XA(V) \
	((byte)(((V) >>  8) & 0xFF))

/*
 * Convert a visual V into a desired char XC
 */
#define VISUAL_XC(V) \
	((char)(((V) >>  0) & 0xFF))

