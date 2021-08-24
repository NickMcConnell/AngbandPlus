#ifndef INCLUDED_MON_TIM_H
#define INCLUDED_MON_TIM_H

/************************************************************************
 * Timed Monster Bonuses/Effects (see plr_tim.h for documentation)
 ************************************************************************/

enum {
    MT_SLEEP = T_MONSTER, /* XXX we share some T_* codes with plr_tim (T_PARALYZED, T_INVULN, etc) */
    MT_AMNESIA,
    MT_DISCORD,
    MT_BOUND,
    MT_ILLUSION,
};

struct mon_tim_s
{
    s16b id;
    s16b count;
    u16b flags;
    s32b parm;
    mon_tim_ptr next;
};
typedef bool (*mon_tim_p)( mon_tim_ptr timer);

typedef struct mon_tim_info_s mon_tim_info_t, *mon_tim_info_ptr;
struct mon_tim_info_s
{
    s16b   id;
    cptr   name;
    void (*on_f)(mon_ptr mon, mon_tim_ptr timer);
    void (*off_f)(mon_ptr mon, mon_tim_ptr timer);
    void (*tick_f)(mon_ptr mon, mon_tim_ptr timer);
    void (*display_f)(mon_ptr mon, mon_tim_ptr timer, doc_ptr doc);
    void (*probe_f)(mon_ptr mon, mon_tim_ptr timer, doc_ptr doc);
    u32b   flags;
};

extern mon_tim_info_ptr mon_tim_info_alloc(int id, cptr name);
extern void mon_tim_register(mon_tim_info_ptr info);

extern void mon_tim_add(mon_ptr mon, int id, int count);
extern void mon_tim_add_aux(mon_ptr mon, int id, int count, int parm);
extern void mon_tim_subtract(mon_ptr mon, int id, int count);
extern void mon_tim_recover(mon_ptr mon, int id, int pct, int min);
extern void mon_tim_remove(mon_ptr mon, int id);
extern void mon_tim_delete(mon_ptr mon, int id); /* same as remove, but no msg */
extern void mon_tim_dispel(mon_ptr mon);
extern void mon_tim_disenchant(mon_ptr mon);
extern bool mon_tim_find(mon_ptr mon, int id);
extern bool mon_tim_find_p(mon_ptr mon, mon_tim_p p);
extern int  mon_tim_parm(mon_ptr mon, int id);
extern int  mon_tim_amount(mon_ptr mon, int id);
extern int  mon_tim_count(mon_ptr mon);

extern void mon_tim_tick(mon_ptr mon);
extern void mon_tim_fast_tick(mon_ptr mon);
extern void mon_tim_clear(mon_ptr mon);
extern void mon_tim_display(mon_ptr mon, doc_ptr doc);
extern void mon_tim_probe(mon_ptr mon, doc_ptr doc);

extern void mon_tim_load(mon_ptr mon, savefile_ptr file);
extern void mon_tim_save(mon_ptr mon, savefile_ptr file);
#endif
