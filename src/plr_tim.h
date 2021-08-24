#ifndef INCLUDED_PLR_TIM_H
#define INCLUDED_PLR_TIM_H

/************************************************************************
 * Timed Player Bonuses
 *
 * This module handles timed bonuses such as quaffing a potion of speed
 * or casting a spell of resistance. In addition, it handles temporary
 * untimed effects from special player abilities (like bard music or
 * hex chanting). Classes can register their own custom timers.
 *
 * Note that race and realm timers usually need to be made public,
 * since players can change race (polymorph, toxic waste) and realms.
 * Monster races can use private timers since they behave like classes.
 ************************************************************************/

/* Each type of timer gets a unique identifier (T_*). Monster timers (mon_tim)
 * and player timers (plr_tim) will share the same id pool to facilitate the
 * possessor and mimic. */
enum {
    T_NONE = 0,
    /* speed */
    T_FAST, T_SLOW, T_LIGHT_SPEED,
    /* buff */
    T_BLESSED, T_HERO, T_BERSERK, T_GIANT_STRENGTH,
    T_STONE_SKIN, T_STEALTH, T_SUPERSTEALTH, 
    T_PROT_EVIL, T_REVENGE, T_INVULN, T_WRAITH, T_MULTISHADOW,
    T_DEVICE_POWER,
    /* brands */
    T_BRAND_ACID,  T_BRAND_ELEC, T_BRAND_FIRE, T_BRAND_COLD, T_BRAND_POIS, T_BRAND_MANA,
    T_WEAPONMASTERY,
    /* resists */
    T_RES_ACID,    T_RES_ELEC,   T_RES_FIRE,   T_RES_COLD,   T_RES_POIS,
    T_RES_NETHER,  T_RES_DISEN,  T_RES_TIME,   T_ULT_RES,
    T_IM_ACID,     T_IM_ELEC,    T_IM_FIRE,    T_IM_COLD,
    /* auras */
    T_AURA_ELEC, T_AURA_FIRE, T_AURA_COLD, T_AURA_SHARDS, T_AURA_HOLY,
    /* abilities */
    T_SUSTAIN,
    T_INFRAVISION, T_SEE_INVIS, T_TELEPATHY,
    T_REGEN, T_STAR_REGEN,
    T_LEVITATION, T_REFLECT, T_RES_MAGIC, T_MAGICAL_ARMOR, T_PASSWALL,
    T_INV_PROT,
    /* biff */
    T_POISON, T_BLIND, T_PARALYZED, T_CONFUSED, T_HALLUCINATE, T_NO_SPELLS,
    T_STUN, T_CUT, T_FEAR,
    T_EGO_WHIP, T_MIND_TRAP, T_DRAIN_MANA, T_DRAIN_FOOD, T_TELEPORT,
    /* special */
    T_KUTAR_EXPAND, T_ENLARGE_WEAPON,
    /* XXX add new effects here without breaking savefiles (cf plr_tim_save and plr_tim_load) */
    T_EXTRA = 300,
    /* monsters share some of the forgoing, but also have their own types */
    T_MONSTER = 800,
    /* classes may add custom timers (plr_tim_register) */
    T_CUSTOM = 1000,
};
enum { /* levels for T_STUN */
    STUN_NONE = 0,
    STUN_DAZE = 1,
    STUN_LIGHT = 10,
    STUN_MODERATE = 25,
    STUN_HEAVY = 50,
    STUN_MASSIVE = 75,
    STUN_KNOCKED_OUT = 100
};
enum { /* levels for T_CUT */
    CUT_NONE = 0,
    CUT_GRAZE = 1,
    CUT_LIGHT = 10,
    CUT_BAD = 25,
    CUT_NASTY = 50,
    CUT_SEVERE = 100,
    CUT_DEEP_GASH = 200,
    CUT_MORTAL_WOUND = 1000
};

/* timer flags (TF_*) */
#define TF_FAST_TICK 0x0001  /* timer ticks every player action (vs every 10 game turns) */
#define TF_LOCKED    0x0002  /* timer remains even if expired (e.g. bard songs) */
#define TF_NO_DISPEL 0x0004  /* timer never gets dispelled (perhaps it is something bad) */
#define TF_AUGMENT   0x0008  /* count accumulates on each plr_tim_add() (e.g. E_POISON) */
#define TF_IGNORE    0x0010  /* count ignores new additions until it expires (e.g. E_PARALYZED) */
#define TF_NO_MSG    0x0020  /* omit messages ... for timer_on and timer_off plr hooks XXX You'll need to implement this in plr_tim.c as needed! cf _paralyzed */
#define TF_DUPLICATE 0x0040  /* allow duplicate timers with same id (MT_AMNESIA) XXX not implemented for plr_tim_add yet */
#define TF_BIFF (TF_NO_DISPEL | TF_AUGMENT)

/* the timer */
typedef struct plr_tim_s  plr_tim_t, *plr_tim_ptr;
struct plr_tim_s
{
    s16b id;
    s16b count;
    u16b flags;
    s16b parm;  /* for custom timers (cf psion) */
    plr_tim_ptr next;
};

/* info about a timer for bonuses and status bar display */
typedef struct {
    cptr name;
    cptr abbrev;
    byte color;
} status_display_t, *status_display_ptr;
extern status_display_t status_display_create(cptr name, cptr abbrev, byte color);
typedef struct shooter_info_s shooter_info_t, *shooter_info_ptr; /* XXX Move me */
typedef struct plr_tim_info_s plr_tim_info_t, *plr_tim_info_ptr;
struct plr_tim_info_s
{
    s16b   id;           /* unique identifier for this timer */
    byte   dispel_prob;  /* probability of triggering BIFF_DISPEL_MAGIC (cf dispel_check) */
    cptr   name, desc;   /* name and desc are descriptive for Self Knowledge et. al. */
    bool (*on_f)(plr_tim_ptr timer);
    void (*add_f)(plr_tim_ptr timer, int amt); /* custom adding to existing timer (e.g. T_STUN, T_CUT) */
    void (*off_f)(plr_tim_ptr timer);
    void (*tick_f)(plr_tim_ptr timer); /* you manage decrementing timer->count */
    void (*calc_bonuses_f)(plr_tim_ptr timer);
    void (*calc_weapon_bonuses_f)(plr_tim_ptr timer, obj_ptr obj, plr_attack_info_ptr info); /* obj == NULL for PAT_MONK */
    void (*calc_shooter_bonuses_f)(plr_tim_ptr timer, obj_ptr obj, shooter_info_ptr info);
    void (*flags_f)(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE]);
    void (*stats_f)(plr_tim_ptr timer, s16b stats[MAX_STATS]);
    bool (*dispel_check_f)(plr_tim_ptr timer, mon_ptr mon); /* e.g. _brand_fire or _res_acid */
    status_display_t (*status_display_f)(plr_tim_ptr timer);
    void (*prt_effects_f)(plr_tim_ptr timer, doc_ptr doc);
    u32b   flags;
};

/* allocate and register custom timers on a per class basis (cf E_CUSTOM) */
extern plr_tim_info_ptr plr_tim_info_alloc(int id, cptr name);
extern void plr_tim_register(plr_tim_info_ptr info);

/* Adding a timer (plr_tim_add):
 * [1] If the timer does not already exist, it gets added with specified count.
 *     The on_f hook is called.
 * [2] If the timer already exists, its current count gets modified by one of the following:
 *   [2.a] Default behaviour is to take MAX(current count, new count)
 *   [2.b] Use TF_IGNORE to *not* adjust the current count at all (e.g. T_PARALYZED)
 *   [2.c] Use TF_AUGMENT to add the new count to the current count (e.g. T_POISONED and most "biffs")
 *   [2.d] Use add_f to control things yourself. This gets called only when adjusting an
 *         *existing* timer using plr_tim_add, _subtract, _recover or _remove (never on [1]).
 *
 * Bard Music:
 * [1] Use plr_tim_lock when the song begins. This adds a new locked timer (count = 0) if one is
 *     not already present, or locks an already existing timer (count > 0).
 * [2] When a locked timer expires (count decremented to 0), it is not removed.
 * [3] Use plr_tim_unlock when the song ends. This will dismiss the timer (off_f) if the count
 *     is 0. Otherwise, the timer remains until its count expires naturally. (e.g. Bard sings
 *     MUSIC_SPEED and then zaps a staff of speed. Stopping the song will not remove the T_FAST
 *     bonus until the _Speed expires).
 * [4] TF_LOCKED timers cannot be dispelled or disenchanted (cf dispel_player and consider
 *     the "constant hero" builds).
 *
 * Accessing Timer Info:
 * [1] Normally, you call plr_tim_find(T_WHATEVER) to see if the timer is present.
 * [2] Should you need the current count, call plr_tim_amount (e.g. T_STUN).
 * [3] Should you need the parm on a custom timer, call plr_tim_parm (e.g. T_REVENGE in psion_backlash_dam)
 *
 * Removing an Existing Timer:
 * [1] Just let the timer count down and expire. plr_tim_tick will handle things for you.
 * [2] Or, call plr_tim_remove. Currently this even removes TF_LOCKED timers (XXX)
 * [3] Or, call plr_tim_subtract to decrement the count. This will remove if count goes to 0.
 * [4] Or, call plr_tim_recover to recover from "biffs" by magical means.
 * [5] Or, call plr_tim_unlock to release a locked timer, removing it if the count is 0.
 * [6] Or, call plr_tim_dispel when monsters do BIFF_DISPEL_MAGIC (removes all qualifying timers).
 * [7] Or, call plr_tim_disenchant when plr hit by GF_DISENCHANT (removes 1 random qualifying timer).
 *
 * For [6] and [7], use TF_NO_DISPEL in your timer registration to protect from
 * BIFF_DISPEL_MAGIC and GF_DISENCHANT. For example, we don't want the player to
 * benefit by having their T_POISON cured! TF_LOCKED timers are also protected.
 *
 * Miscellaneous:
 * [1] Use plr_hooks_s.timer_on to notice effects like T_CONFUSED, T_STUN, etc.
 *     For example, see samurai.c. You might also want to override normal timer
 *     messaging for some crazy reason: see repose_timer_on for example, which
 *     uses TF_NO_MSG to block the default on_f and off_f messaging for T_PARALYZED.
 * [2] There is also a plr_hooks_s.timer_off to notice when timers are removed.
 *     For example, see repose_timer_off.
 * [3] XXX We do not currently have plr hooks to notice timer changes.
 */

/* add, remove, query existing timers */
extern bool plr_tim_add(int id, int count);
extern bool plr_tim_add_aux(int id, int count, int parm);
extern bool plr_tim_augment(int id, int count); /* force TF_AUGMENT behavior for counters lacking this flag */
extern void plr_tim_subtract(int id, int count);
extern bool plr_tim_recover(int id, int pct, int min);  /* for E_POISON */
extern bool plr_tim_remove(int id);
extern void plr_tim_lock(int id);
extern void plr_tim_unlock(int id);
extern bool plr_tim_dispel(void);
extern bool plr_tim_disenchant(void);
extern bool plr_tim_find(int id);
extern int  plr_tim_parm(int id);   /* e.g. psion_backlash_dam() */
extern int  plr_tim_amount(int id); /* e.g. prt_effects for E_POISON */
extern int  plr_tim_count(void);    /* number of active timers */

/* system hooks */
extern void plr_tim_tick(void);  /* every 10 game turns */
extern void plr_tim_fast_tick(void); /* every player action if TF_FAST_TICK */
extern void plr_tim_calc_bonuses(void); /* PU_BONUS */
extern void plr_tim_calc_weapon_bonuses(obj_ptr obj, plr_attack_info_ptr info);
extern void plr_tim_calc_shooter_bonuses(obj_ptr obj, shooter_info_ptr info);
extern void plr_tim_flags(u32b flgs[OF_ARRAY_SIZE]); /* plr_display */
extern void plr_tim_stats(s16b stats[MAX_STATS]); /* PU_BONUS and plr_display */
extern void plr_tim_status_bar(void); /* PR_STATUS */
extern void plr_tim_prt_effects(doc_ptr doc); /* PR_EFFECTS */
extern void plr_tim_clear(void); /* XXX player death, buildings, etc. (reset_tim_flags)  */
extern bool plr_tim_dispel_check(mon_ptr mon); /* dispel_check */
extern void plr_tim_self_knowledge(doc_ptr doc);
extern void plr_tim_weigh_magic(doc_ptr doc);

/* savefile support */
extern void plr_tim_load(savefile_ptr file);
extern void plr_tim_save(savefile_ptr file);

/* XXX move to plr_bonus.c */
/* Note these are for magical enhancements not intended to stack.
 * For example Stone Skin gives +50 AC and Ult Res +75. Together, only +75 (not +125) */
extern void plr_bonus_ac(int amt);
extern void plr_bonus_speed(int amt);

#endif
