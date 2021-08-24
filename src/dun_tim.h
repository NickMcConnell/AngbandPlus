#ifndef INCLUDED_DUN_TIM_H
#define INCLUDED_DUN_TIM_H

/* Timer */
enum {
    DT_NONE = 0,
    DT_BOMB,     /* goes boom on expiration */
    DT_WALL_BREAKER, /* cf "Wall Breaker" room templates; traps spring on their own! */
    DT_MON_GENERATOR, /* spitting out an orc every tick! */
    DT_ILLUSION, /* Illusionist: Timed feat mimicry using CAVE_ILLUSION */
    DT_KLAXON,   /* Illusionist: Timed noise-maker to draw/divert monsters */
    DT_CUSTOM = 1000 /* Class specific dungeon timers go here */
};
struct dun_tim_s
{
    dun_ptr dun;
    s16b id;
    point_t pos;

    s16b count;
    u16b flags;         /* custom flags */
    s16b parm1, parm2;  /* for custom timers. E.g. gf and dam for DT_BOMB */
    s16b old_feat;

    dun_tim_ptr next;
};

/* Timer Info */
#define DTF_UNIQUE_TYPE 0x0001
#define DTF_TRAP        0x0002

typedef struct dun_tim_info_s dun_tim_info_t, *dun_tim_info_ptr;
struct dun_tim_info_s
{
    s16b   id;           /* unique identifier for this timer */
    cptr   name, desc;
    void (*on_f)(dun_tim_ptr timer);
    void (*off_f)(dun_tim_ptr timer);  /* e.g. timed explosion at timer->pos */
    void (*tick_f)(dun_tim_ptr timer); /* manage timer->count. also, do interesting stuff! */
    void (*disarm_f)(dun_tim_ptr timer); /* for DTF_TRAPs */
    u32b   flags;
};
/* allocate and register custom timers on a per class basis (cf DT_CUSTOM) */
extern dun_tim_info_ptr dun_tim_info_alloc(int id, cptr name);
extern void dun_tim_register(dun_tim_info_ptr info);
extern dun_tim_info_ptr dun_tim_info_lookup(int id);

/* Dungeon Timers:
 * I envisage two types of timers. In the usual case, timers are added to a specific
 * location in the dungeon. For example, a "time bomb" that ticks down and gives an
 * explosion at a given location. Or, a "volcanic geyser" that periodically erupts over
 * and over again, spilling lava. */
extern dun_tim_ptr dun_tim_add_at(dun_ptr dun, point_t pos, int id, int count);
extern bool        dun_tim_remove_at(dun_ptr dun, point_t pos, int id);

/* The following are "traps" that could be disarmed in time */
extern void        dun_add_time_bomb(dun_ptr dun, point_t pos, int count, int gf, int dam);
extern void        dun_add_wall_breaker(dun_ptr dun, point_t pos, int count);
extern void        dun_add_mon_generator(dun_ptr dun, point_t pos, int summon_type); /* summon_specific_e */
extern void        dun_tim_disarm(dun_ptr dun, point_t pos);

/* The other type of timer is global to the entire dungeon, though I don't have many ideas
 * yet. Perhaps this would be a way to get custom "process_world" type behavior. Or perhaps
 * a level would have a clock that the plr must race against before something nasty happens
 * (e.g. all monsters wake up and are hasted). */
extern dun_tim_ptr dun_tim_add(dun_ptr dun, int id, int count);
extern bool        dun_tim_remove(dun_ptr dun, int id);

extern void        dun_tim_tick(dun_ptr dun);  /* every 10 game turns */
extern void        dun_tim_load(dun_ptr dun, savefile_ptr file);
extern void        dun_tim_save(dun_ptr dun, savefile_ptr file);

#endif
