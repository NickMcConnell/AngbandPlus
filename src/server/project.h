/*
 * File: project.h
 * Purpose: The project() function and helpers
 */

#ifndef PROJECT_H
#define PROJECT_H

#define ATT_SAVE        0x01
#define ATT_DAMAGE      0x02
#define ATT_NON_PHYS    0x04
#define ATT_RAW         0x08

/*
 * Bit flags for the "project()" function
 *
 *   NONE: No flags
 *   JUMP: Jump directly to the target location without following a path
 *   BEAM: Work as a beam weapon (affect every grid passed through)
 *   THRU: May continue through the target (used for bolts and beams)
 *   STOP: Stop as soon as we hit a monster (used for bolts)
 *   GRID: May affect terrain in the blast area in some way
 *   ITEM: May affect objects in the blast area in some way
 *   KILL: May affect monsters in the blast area in some way
 *   HIDE: Disable visual feedback from projection
 *   AWARE: Effects are already obvious to the player
 *   SAFE: Doesn't affect monsters of the same race as the caster
 *   ARC: Projection is a sector of circle radiating from the caster
 *   PLAY: May affect players
 *   INFO: Use believed map rather than truth for player ui
 *   SHORT: Use one quarter of max_range
 *   CONST: Effect doesn't decrease with distance
 */
#define PROJECT_NONE    0x0000
#define PROJECT_JUMP    0x0001
#define PROJECT_BEAM    0x0002
#define PROJECT_THRU    0x0004
#define PROJECT_STOP    0x0008
#define PROJECT_GRID    0x0010
#define PROJECT_ITEM    0x0020
#define PROJECT_KILL    0x0040
#define PROJECT_HIDE    0x0080
#define PROJECT_AWARE   0x0100

#define PROJECT_SAFE    0x0200
#define PROJECT_ARC     0x0400
#define PROJECT_PLAY    0x0800
#define PROJECT_INFO    0x1000
#define PROJECT_SHORT   0x2000
#define PROJECT_CONST   0x4000
#define PROJECT_ROCK    0x8000

/*
 * Projection struct
 */
struct projection
{
    int index;
    char *name;
    char *type;
    char *desc;
    char *blind_desc;
    char *lash_desc;
    int numerator;
    random_value denominator;
    int divisor;
    int damage_cap;
    int msgt;
    bool obvious;
    bool wake;
    int color;
    byte flags;
    char *threat;
    int threat_flag;
    struct projection *next;
};

extern struct projection *projections;

/* Display attrs and chars */
extern byte proj_to_attr[PROJ_MAX][BOLT_MAX];
extern char proj_to_char[PROJ_MAX][BOLT_MAX];

/* project.c */
extern int proj_name_to_idx(const char *name);
extern const char *proj_idx_to_name(int type);
extern int project_path(struct player *p, struct loc *gp, int range, struct chunk *c,
    struct loc *grid1, struct loc *grid2, int flg);
extern bool projectable(struct player *p, struct chunk *c, struct loc *grid1, struct loc *grid2,
    int flg, bool nowall);
extern byte proj_color(int type);
extern void origin_get_loc(struct loc *ploc, struct source *origin);
extern bool project(struct source *origin, int rad, struct chunk *cv, struct loc *finish, int dam,
    int typ, int flg, int degrees_of_arc, byte diameter_of_source, const char *what);

/* project-feat.c */
extern bool project_f(struct source *origin, int r, struct chunk *c, struct loc *grid, int dam,
    int typ);

/* project-mon.c */
extern void thrust_away(struct chunk *c, struct source *origin, struct loc *centre, int grids_away);
extern bool project_m_monster_attack_aux(struct monster *attacker, struct chunk *c,
    struct monster *mon, int dam, byte note);
extern void project_m(struct source *origin, int r, struct chunk *c, struct loc *grid, int dam,
    int typ, int flg, bool *did_hit, bool *was_obvious, int *newy, int *newx);
extern void monster_set_master(struct monster *mon, struct player *p, byte status);
extern bool can_charm_monster(struct player *p);
extern int charm_monster(struct monster *mon, struct player *p, byte status);

/* project-obj.c */
extern int inven_damage(struct player *p, int type, int cperc);
extern struct monster_race *get_race(const char *name);
extern bool project_o(struct source *origin, int r, struct chunk *c, struct loc *grid, int dam,
    int typ);

/* project-player.c */
extern int adjust_dam(struct player *p, int type, int dam, aspect dam_aspect, int resist);
extern void project_player_time_effects(struct player *p, struct source *who);
extern void project_p(struct source *origin, int r, struct chunk *c, struct loc *grid, int dam,
    int typ, int power, const char *what, bool *did_hit, bool *was_obvious, struct loc *newgrid);

#endif /* PROJECT_H */
