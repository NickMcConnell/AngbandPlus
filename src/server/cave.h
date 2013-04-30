/*
 * File: cave.h
 * Purpose: Cave interface
 */

#ifndef CAVE_H
#define CAVE_H

extern int distance(int y1, int x1, int y2, int x2);
extern bool los(int depth, int y1, int x1, int y2, int x2);
extern bool no_light(struct player *p);
extern bool cave_valid_bold(int depth, int y, int x);
extern byte get_color(byte a, int attr, int n);
extern bool dtrap_edge(struct player *p, int y, int x);
extern void grid_data_as_text(struct player *p, bool server, grid_data *g, byte *ap,
    char *cp, byte *tap, char *tcp);
extern void map_info(struct player *p, unsigned y, unsigned x, grid_data *g);
extern void prt_map(struct player *p);
extern void display_map(struct player *p, bool subwindow);
extern errr vinfo_init(void);
extern void forget_view(struct player *p);
extern void update_view(struct player *p);
extern void wiz_light(struct player *p, bool full);
extern int project_path(u16b *gp, int range, int depth, int y1, int x1,
    int y2, int x2, int flg);
extern bool projectable(int depth, int y1, int x1, int y2, int x2, int flg);
extern void scatter(int depth, int *yp, int *xp, int y, int x, int d, bool skip_los);
extern void health_track(struct player *p, int who);
extern void monster_race_track(int Ind, int r_idx);
extern void disturb(struct player *p, int stop_search, int flush_output);
extern bool is_quest(int depth);
extern void update_visuals(int depth);
extern void fully_update_flow(int depth);
extern void forget_spot(int depth, int y, int x);
extern void display_fullmap(int Ind);
extern void deep_nights(struct player *p);
extern bool projectable_wall(int depth, int y1, int x1, int y2, int x2);
extern void cursor_track(int Ind, int m_idx);
extern void update_cursor(int);
extern void update_health(int m_idx);

struct cave
{
    s16b depth;
    u32b obj_rating;
    u32b mon_rating;
    bool good_item;
    int height;
    int width;
    byte (*info)[DUNGEON_WID];
    byte (*feat)[DUNGEON_WID];
    s16b (*m_idx)[DUNGEON_WID];
    s16b (*o_idx)[DUNGEON_WID];
    struct monster *monsters;
    int mon_max;
    int mon_cnt;

    /* PWMAngband */
    s16b num_clones;
    bool scan_monsters;
    hturn generated;
};

extern struct cave **cave;

extern struct cave *cave_new(s16b depth);
extern void cave_free(struct cave *c);

extern void cave_set_feat(struct cave *c, int y, int x, int feat);
extern void cave_note_spot(struct cave *c, int y, int x);
extern void cave_light_spot_aux(struct player *p, struct cave *cv, int y, int x);
extern void cave_light_spot(struct cave *c, int y, int x);
extern void cave_forget_flow(struct player *p);
extern void cave_update_flow(struct player *p, struct cave *c);
extern void cave_illuminate(struct player *p, struct cave *c, bool daytime);

/*
 * cave_predicate is a function pointer which tests a given square to
 * see if the predicate in question is true.
 */
typedef bool (*cave_predicate)(struct cave *c, int y, int x);

/* FEATURE PREDICATES */
extern bool cave_isfloor(struct cave *c, int y, int x);
extern bool cave_issafefloor(struct cave *c, int y, int x);
extern bool cave_isotherfloor(struct cave *c, int y, int x);
extern bool cave_isanyfloor(struct cave *c, int y, int x);
extern bool cave_isrock(struct cave *c, int y, int x);
extern bool cave_isperm(struct cave *c, int y, int x);
extern bool cave_ismagma(struct cave *c, int y, int x);
extern bool cave_isquartz(struct cave *c, int y, int x);
extern bool cave_ismineral(struct cave *c, int y, int x);
extern bool cave_isrubble(struct cave *c, int y, int x);
extern bool cave_issecretdoor(struct cave *c, int y, int x);
extern bool cave_isopendoor(struct cave *c, int y, int x);
extern bool cave_isbasicopen(struct cave *c, int y, int x);
extern bool cave_iscloseddoor(struct cave *c, int y, int x);
extern bool cave_islockeddoor(struct cave *c, int y, int x);
extern bool cave_isjammeddoor(struct cave *c, int y, int x);
extern bool cave_ishomedoor(struct cave *c, int y, int x);
extern bool cave_isbasicdoor(struct cave *c, int y, int x);
extern bool cave_isdoor(struct cave *c, int y, int x);
extern bool cave_issecrettrap(struct cave *c, int y, int x);
extern bool cave_isknowntrap(struct cave *c, int y, int x);
extern bool cave_istrap(struct cave *c, int y, int x);
extern bool cave_isupstairs(struct cave *c, int y, int x);
extern bool cave_isdownstairs(struct cave *c, int y, int x);
extern bool cave_isstairs(struct cave *c, int y, int x);
extern bool cave_istree(struct cave *c, int y, int x);
extern bool cave_isshop(struct cave *c, int y, int x);

/* BEHAVIOR PREDICATES */
extern bool cave_isopen(struct cave *c, int y, int x);
extern bool cave_isempty(struct cave *c, int y, int x);
extern bool cave_canputitem(struct cave *c, int y, int x);
extern bool cave_isdiggable(struct cave *c, int y, int x);
extern bool cave_ispassable(struct cave *c, int y, int x);
extern bool cave_iswall(struct cave *c, int y, int x);
extern bool cave_isstrongwall(struct cave *c, int y, int x);
extern bool cave_isvault(struct cave *c, int y, int x);
extern bool cave_isroom(struct cave *c, int y, int x);
extern bool cave_isfeel(struct cave *c, int y, int x);

extern bool cave_in_bounds(struct cave *c, int y, int x);
extern bool cave_in_bounds_fully(struct cave *c, int y, int x);

extern struct monster *cave_monster(struct cave *c, int idx);
extern struct monster *cave_monster_at(struct cave *c, int y, int x);
extern int cave_monster_max(struct cave *c);
extern int cave_monster_count(struct cave *c);

extern void upgrade_mineral(struct cave *c, int y, int x);

extern struct cave *cave_get(s16b depth);

#endif /* CAVE_H */
