/*
 * File: target.h
 * Purpose: Targeting code
 */

#ifndef TARGET_H
#define TARGET_H

/*
 * Keyset mappings for various keys.
 */
#define ARROW_DOWN      0x80
#define ARROW_LEFT      0x81
#define ARROW_RIGHT     0x82
#define ARROW_UP        0x83
#define KC_ENTER        0x9C
#define ESCAPE          0xE000

/* Analogous to isdigit() etc in ctypes */
#define isarrow(c)  ((c >= ARROW_DOWN) && (c <= ARROW_UP))

/*
 * Steps for the "target_set_interactive" function
 *
 *    NONE:  Initial lookup
 *    MON:   Describe monster (or player)
 *    TRAP:  Describe trap
 *    OBJ:   Describe object
 *    FEAT:  Describe feature
 */
#define TARGET_NONE 0
#define TARGET_MON  1
#define TARGET_TRAP 2
#define TARGET_OBJ  3
#define TARGET_FEAT 4

extern void look_mon_desc(struct monster *mon, char *buf, size_t max);
extern void look_player_desc(struct player *p, char *buf, size_t max);
extern bool target_able(struct player *p, struct source *who);
extern bool target_okay(struct player *p);
extern bool target_set_monster(struct player *p, struct source *who);
extern void target_set_location(struct player *p, struct loc *grid);
extern int cmp_distance(const void *a, const void *b);
extern s16b target_pick(int y1, int x1, int dy, int dx, struct point_set *targets);
extern bool target_accept(struct player *p, struct loc *grid);
extern void grid_desc(struct player *p, char *buf, int size, struct loc *grid);
extern void target_get(struct player *p, struct loc *grid);
extern bool target_equals(struct player *p, struct source *who);
extern void draw_path_grid(struct player *p, struct loc *grid, byte a, char c);
extern void flush_path_grid(struct player *p, struct chunk *cv, struct loc *grid, byte a, char c);
extern bool panel_contains(struct player *p, struct loc *grid);
extern struct point_set *target_get_monsters(struct player *p, int mode);
extern bool target_set_closest(struct player *p, int mode);

#endif /* TARGET_H */
