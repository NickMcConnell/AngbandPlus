#ifndef INCLUDED_DUN_GEN_H
#define INCLUDED_DUN_GEN_H

#include "rect.h"

/* free list of available space for rooms */
typedef struct dun_space_s dun_space_t, *dun_space_ptr;
struct dun_space_s
{
    dun_space_ptr next, prev;
    rect_t rect;
};

/* generation info */
struct dun_gen_s
{
    dun_ptr       dun;

    /* options */
    int           scale_pct;
    bool          destroyed;
    bool          arena;
    bool          cavern;
    bool          river;
    int           lake;
    int           tower_max;

    /* rooms */
    dun_space_ptr space;     /* manage free space allocation for rooms (cf dun_gen_reserve) */
    point_vec_ptr rooms;     /* center points for tunneler */

    /* tunneler */
    point_vec_ptr floors;    /* current tunnel: wall->floor */
    point_vec_ptr walls;     /* current tunnel: room piercings */
    point_vec_ptr doors;     /* overall tunnel junctions */
    int rand_dir_pct;        /* chance of random direction changes during tunneling */
    int change_dir_pct;      /* chance of direction change; diagonal paths can alternate primary directions */
    int quit_pct;            /* chance of quitting a tunnel at an intersection */
    int door_pierce_pct;     /* chance of doors at room piercings */
    int door_intersect_pct;  /* chance of doors at hallway intersections */

    cptr error;
};

typedef bool (*dun_gen_room_f)(dun_gen_ptr gen);

/* api */
extern void        dun_gen(dun_ptr dun);

extern rect_t      dun_gen_reserve(dun_gen_ptr gen, point_t size);
extern bool        dun_gen_reserve_rect(dun_gen_ptr gen, rect_t rect);
extern bool        dun_gen_rooms(dun_gen_ptr gen);
extern bool        dun_gen_tunnels(dun_gen_ptr gen);
extern void        dun_gen_monsters(dun_gen_ptr gen);
extern void        dun_gen_traps(dun_gen_ptr gen);
extern void        dun_gen_rubble(dun_gen_ptr gen);
extern void        dun_gen_objects(dun_gen_ptr gen);
extern void        dun_gen_streamers(dun_gen_ptr gen);
extern void        dun_gen_lava_vault(dun_gen_ptr gen);

extern bool        dun_gen_normal_room(dun_gen_ptr gen);
extern bool        dun_gen_overlap_room(dun_gen_ptr gen);
extern bool        dun_gen_template_room(dun_gen_ptr gen, int type, int subtype);
extern bool        dun_gen_cave_room(dun_gen_ptr gen);
extern void        dun_gen_cave_wizard(void);

extern void        dun_gen_secret_door(point_t pos, dun_grid_ptr grid, int type);
extern void        _sort_cluster(point_vec_ptr rooms, point_t origin);

#endif
