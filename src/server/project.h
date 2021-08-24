/*
 * File: project.h
 * Purpose: The project() function and helpers
 */

#ifndef PROJECT_H
#define PROJECT_H

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

/* project.c */
extern byte gf_to_attr[GF_MAX][BOLT_MAX];
extern char gf_to_char[GF_MAX][BOLT_MAX];

extern int project_path(struct loc *gp, int range, struct chunk *c, int y1, int x1, int y2, int x2,
    int flg);
extern bool projectable(struct chunk *c, int y1, int x1, int y2, int x2, int flg);
extern bool projectable_wall(struct chunk *c, int y1, int x1, int y2, int x2);
extern bool gf_force_obvious(int type);
extern byte gf_color(int type);
extern int gf_num(int type);
extern random_value gf_denom(int type);
extern const char *gf_desc(int type);
extern const char *gf_blind_desc(int type);
extern byte gf_flags(int type);
extern bool project(struct actor *who, int rad, struct chunk *cv, int y, int x, int dam, int typ,
    int flg, int degrees_of_arc, byte diameter_of_source, const char *what);

/* project-feat.c */
extern bool project_f(struct actor *who, int r, struct chunk *c, int y, int x, int dam, int typ);

/* project-mon.c */
extern bool project_m_monster_attack_aux(struct monster *attacker, struct chunk *c,
    struct monster *mon, int dam, byte note);
extern void project_m(struct actor *who, int r, struct chunk *c, int y, int x, int dam, int typ,
    int flg, bool *did_hit, bool *was_obvious, int *newy, int *newx);
extern void monster_set_master(struct monster *mon, struct player *p, byte status);
extern bool can_charm_monster(struct player *p);
extern int charm_monster(struct monster *mon, struct player *p, byte status);

/* project-obj.c */
extern int inven_damage(struct player *p, int type, int cperc);
extern struct monster_race *get_race(const char *name);
extern bool project_o(struct actor *who, int r, struct chunk *c, int y, int x, int dam, int typ);

/* project-player.c */
extern int adjust_dam(struct player *p, int type, int dam, aspect dam_aspect, int resist);
extern void project_player_swap_stats(struct player *p);
extern void project_player_time_effects(struct player *p, struct actor *who);
extern void project_p(struct actor *who, int r, struct chunk *c, int y, int x, int dam, int typ,
    const char *what, bool *did_hit, bool *was_obvious, int *newy, int *newx);

#endif /* PROJECT_H */
