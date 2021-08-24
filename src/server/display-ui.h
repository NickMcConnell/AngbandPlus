/*
 * File: display-ui.h
 * Purpose: Server-side game UI.
 */

#ifndef INCLUDED_DISPLAY_UI_H
#define INCLUDED_DISPLAY_UI_H

struct explosion
{
    int proj_type;
    int num_grids;
    const int *distance_to_grid;
    const struct loc *blast_grid;
};

struct bolt
{
    int proj_type;
    bool beam;
    struct loc ogrid;
    struct loc grid;
};

struct missile
{
    byte mattr;
    char mchar;
    struct loc grid;
};

struct message
{
    const char *msg;
    int type;
};

extern void dump_spells(struct player *p, struct object *obj);
extern void player_elements(struct player *p, struct element_info el_info[ELEM_MAX]);
extern void redraw_stuff(struct player *p);
extern void restore_hp(struct player *p);
extern void restore_sp(struct player *p);
extern void death_knowledge(struct player *p);
extern void player_death(struct player *p);
extern void resurrect_player(struct player *p, struct chunk *c);
extern bool modify_panel(struct player *p, struct loc *grid);
extern bool change_panel(struct player *p, int dir);
extern void verify_panel(struct player *p);
extern void center_panel(struct player *p);
extern int move_energy(int depth);
extern int time_factor(struct player *p, struct chunk *c);
extern int pick_arena(struct worldpos *wpos, struct loc *grid);
extern void access_arena(struct player *p, struct loc *grid);
extern void describe_player(struct player *p, struct player *q);
extern void describe_trap(struct player *p, struct trap *trap);
extern void describe_feat(struct player *p, struct feature *feat);
extern void player_dump(struct player *p, bool server);
extern void bolt_pict(struct player *p, struct loc *start, struct loc *end, int typ, byte *a,
    char *c);
extern void display_explosion(struct chunk *cv, struct explosion *data, const bool *drawing,
    bool arc);
extern void display_bolt(struct chunk *cv, struct bolt *data, bool *drawing);
extern void display_missile(struct chunk *cv, struct missile *data);
extern void display_message(struct player *p, struct message *data);

#endif /* INCLUDED_DISPLAY_UI_H */
