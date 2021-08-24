#ifndef INCLUDED_PLR_SHOOT_H
#define INCLUDED_PLR_SHOOT_H

/*************************************************************************
 * Player Archery
 ************************************************************************/
struct plr_shoot_info_s
{
    int slot;
    int to_h;
    int to_d;
    int dis_to_h;
    int dis_to_d;
    int to_mult;
    int to_dd;
    int to_ds;
    int base_shot;
    int xtra_shot;
    int tval_ammo;
    int breakage; /* pct of normal breakage odds ... default is 100 */
    bool heavy_shoot;
    plr_crit_t crit;
    u32b flags[OF_ARRAY_SIZE];
    u32b known_flags[OF_ARRAY_SIZE];
};
extern void plr_shoot_info_wipe(plr_shoot_info_ptr info);


enum { /* processing codes for path hooks (path_f and prepath_f): */
    PATH_OK,   /* normal processing */
    PATH_STOP, /* quit the projection */
    PATH_SKIP  /* skip this pos and go to next pos in path (e.g. skip intervening monsters) */
};
enum { /* what to do with ammo after the projection is done? hook into after_hit_f to mod default behavior */
    AMMO_DROP,    /* possibly break (breakage_chance?) */
    AMMO_BREAK,   /* always break */
    AMMO_RETURN,  /* return to quiver */
    AMMO_BOUNCE,  /* weaponmaster ... but cool */
    AMMO_PIERCE,  /* continue the projection after hitting mon */
    AMMO_EXPLODE,
};

typedef struct {
bool  (*begin_f)(plr_shoot_ptr context);        /* e.g. you can choose ammo, target ... */
bool  (*target_f)(plr_shoot_ptr context);       /* override normal target selection; only return FALSE on UI cancel */
bool  (*begin_bow_f)(plr_shoot_ptr context);    /* return false to skip this bow XXX only one bow atm XXX */
int   (*get_shots_f)(plr_shoot_ptr context);    /* return number of shots (default is 1) */
void  (*begin_path_f)(plr_shoot_ptr context);
int   (*prepath_f)(plr_shoot_ptr context, point_t pos); /* before checking FF_PROJECT */
int   (*path_f)(plr_shoot_ptr context, point_t pos); /* after checking FF_PROJECT; before hitting monster or wall */
bool  (*check_hit_f)(plr_shoot_ptr context, mon_ptr mon);    /* some techniques might always hit. non-null bypasses normal logic. */
void  (*before_hit_f)(plr_shoot_ptr context, mon_ptr mon);   /* called before every hit (but not for misses ... cf miss_f) */
void  (*mod_dam1_f)(plr_shoot_ptr context, mon_ptr mon);   /* goose context->dam before multiplier */
void  (*mod_dam2_f)(plr_shoot_ptr context, mon_ptr mon);   /* goose context->dam after multiplier */
void  (*after_hit_f)(plr_shoot_ptr context, mon_ptr mon);    /* called after every hit, even the killing blow (e.g. Mauler's Splatter) */
void  (*end_path_f)(plr_shoot_ptr context);
void  (*explode_f)(plr_shoot_ptr context, point_t pos);  /* override default explosion */
void  (*miss_f)(plr_shoot_ptr context, mon_ptr mon); /* called after every miss */
void  (*end_bow_f)(plr_shoot_ptr context);
void  (*end_f)(plr_shoot_ptr context);          /* always called if plr actually shot */

      /* Custom slays and brands. The best so far is provided for reference.
       * Returned slay_t is only used if it is the best (In other words, there
       * is no need for you to check). Be very careful with custom slay and
       * brand hooks if you allow PSC_DISPLAY. context->mon and context->race
       * are null for PSC_DISPLAY and you really shouldn't be loring anyway. */
slay_t (*calc_slay_f)(plr_shoot_ptr context, mon_ptr mon, slay_ptr best_slay);
slay_t (*calc_brand_f)(plr_shoot_ptr context, mon_ptr mon, slay_ptr best_brand);
} plr_shoot_hooks_t, *plr_shoot_hooks_ptr;

/* Hook Summary
 * begin_f
 * target_f
 * loop for each bow:
 *   begin_bow_f
 *   get_shots_f
 *   loop for each shot:
 *     begin_path_f
 *     loop for each path pos:
 *       prepath_f
 *       path_f
 *       if mon:
 *         if check_hit_f or <roll for hit>:
 *           calc_slay_f
 *           calc_brand_f
 *           mod_dam1_f
 *           <apply bow multiplier>
 *           mod_dam2_f
 *           before_hit_f
 *           <mon_take_hit>
 *           after_hit_f
 *         else miss_f
 *     end_path_f
 *     [explode_f]
 *   end_bow_f
 * end_f
 */
typedef void (*plr_shoot_init_f)(plr_shoot_ptr context);

enum {
    PLR_SHOOT_NORMAL = 0,
    PLR_SHOOT_PIERCE,
    PLR_SHOOT_AMBUSH,
    PLR_SHOOT_EXPLODE,
    PLR_SHOOT_CUSTOM = 5000 /* <== Begin custom context->type codes here */
};
/* Flags for the player shoot context (PSC) */
#define PSC_DISPLAY       0x0001 /* hack for character sheet display */
#define PSC_BOUNCE        0x0002 /* hack: only bounce once */
#define PSC_NO_ENERGY     0x0004 /* hack: don't clobber energy_use for weaponmaster talents (e.g. shoot on run) */
#define PSC_NO_SLAY       0x0008 /* hack: don't apply slays and brands to damage calc */

struct plr_shoot_s
{
/* Input */
int             mode;
u32b            flags;    /* PSC_DISPLAY */
int             to_h;     /* bonus to hit over and above normal (calc_bonuses) */
int             to_d;     /* extra damage *before* multiplier (extremely OP) */
int             energy;   /* 0=>normal energy use (bow_energy) */

/* State of current plr_shoot */
obj_ptr         bow;
u32b            bow_flags[OF_ARRAY_SIZE];
obj_ptr         ammo;
char            ammo_desc[MAX_NLEN]; /* OD_OMIT_INSCRIPTION */
int             range;
int             mult;
point_t         target;   /* initial target ... bounce effects might path someplace else */
plr_shoot_info_t info;
u32b            ammo_flags[OF_ARRAY_SIZE];
int             skill;    /* calculated skill of shot (not counting stun) */

/* State of current projection */
dun_path_ptr    path;
int             path_pos;
u32b            path_flags;

/* State: Info about current fire. Resets for each hit monster */
int             dam_base;   /* base damage of current shot */
int             dam;        /* total damage of current shot */
int             shoot_sleeping;
int             shoot_fleeing;
bool            hit_body;
int             action;
bool            fear;

/* State: Info about all shots ... running totals */
int             dam_total;  /* running total for all shots against this monster. */
int             hits;       /* number of hits landed in all shots */
int             misses;     /* number of misses in all shots */
int             shots;      /* note: hits + misses != shots */
int             pierce;

/* Delayed Effets done once at the end */

/* Customization */
plr_shoot_hooks_t hooks;
void             *cookie; /* in case you need more info for your callbacks */
};

extern bool plr_shoot(void);
extern bool plr_shoot_special(int type, u32b flags);
extern bool plr_shoot_aux(plr_shoot_ptr context);
extern void plr_shoot_display(void);
extern void plr_shoot_display_special(int type, int flags);
extern void plr_shoot_display_aux(plr_shoot_ptr context);
extern void plr_shoot_doc(doc_ptr doc);
extern void plr_shoot_doc_aux(plr_shoot_ptr context, doc_ptr doc);

#endif
