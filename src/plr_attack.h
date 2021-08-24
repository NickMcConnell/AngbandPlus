#ifndef INCLUDED_PLR_ATTACK_H
#define INCLUDED_PLR_ATTACK_H

#include "obj.h"

/*************************************************************************
 * Player Melee
 * Handle attacks with normal melee weapons or with martial arts.
 * Handle innate attacks for Monster mode, mutations and possessors/mimics.
 * Allow for rich class-specific customization.
 *************************************************************************/
typedef struct plr_crit_s plr_crit_t, *plr_crit_ptr;
struct plr_crit_s
{
    s16b freq_mul;  /* per cent; so 150 makes 10%->15% and 20%->30%; 0 disables crits; 100 default */
    s16b freq_add;  /* per mil; so 25 makes 10%->12.5% and 20%->22.5% (ninja); 1000 forces every hit critical */
    s16b qual_mul;  /* per cent; scale the base of critical roll (i.e., the weight) */
    s16b qual_add;  /* add to quality roll (see _plr_attack_crit_aux) */
};

/*************************************************************************
 * Player Attack Info
 *************************************************************************/
/* player attack type (PAT_*) */
enum {
    PAT_NONE = 0, /* no attack on this hand ... but some of the flags may still be useful */
    PAT_WEAPON,   /* this hand holds a normal melee weapon */
    PAT_MONK,     /* this hand holds info for martial arts */
    PAT_INNATE    /* this info is for all innate attacks */
};

/* player attack flags (PAF_*) */
enum {
    PAF_TWO_HANDS,      /* player is using 2 hands to wield this weapon */
    PAF_NEEDS_TWO_HANDS,/* negates 2 handed bonus since unable to wield weapon with just 1 hand */
    PAF_CONTROL_MOUNT,  /* hand is being used for riding for equip_describe_slot() */
    PAF_DUAL_WIELDING,  /* player is dual wielding on the set of arms for this hand */
    PAF_GENJI,          /* player has a genji bonus for this hand */
    PAF_HEAVY,          /* weapon is too heavy for the player */
    PAF_ICKY,           /* weapon is not suitable for the player's class */
    PAF_ICKY_RIDING,    /* weapon is not suitable for riding */
    PAF_SHIELD,
    PAF_COUNT
};
#define PAF_ARRAY_SIZE 1  /* (PAF_COUNT + 31)/32 */

struct plr_attack_info_s
{
    int  type;   /* type of attack: none, weapon, monk or innate (PAT_*) */
    int  which;  /* which hand ... or which blow index into plr->innate_blows */
    int  slot;   /* equipment slot for this hand */
    int  to_h;
    int  to_d;
    int  dis_to_h;
    int  dis_to_d;
    int  to_dd;
    int  to_ds;
    blows_calc_t blows_calc; /* set in calc_weapon_bonuses (class_t or sometimes race_t) */
    int  base_blow;
    int  xtra_blow;
    int  giant_wield;
    int  skill_mul;      /* per mil; scale the player's skill with this attack (cf dual wielding) */
    plr_crit_t crit;
    u32b paf_flags[PAF_ARRAY_SIZE]; /* paf_ prevents confusion with obj_flags */
    u32b obj_flags[OF_ARRAY_SIZE];
    u32b obj_known_flags[OF_ARRAY_SIZE];
    byte info_attr;
    cptr info;
};
extern void plr_attack_info_wipe(plr_attack_info_ptr info);
extern bool plr_attack_info_two_hand(plr_attack_info_ptr info);
extern bool plr_attack_info_two_hand_bonus(plr_attack_info_ptr info);

/*************************************************************************
 * Boost Melee Damage (Slays, Criticals, Brands, etc.)
 *************************************************************************/
typedef struct {
    int id;     /* OF_BRAND_ACID */
    int mul;    /* 250 -> 2.5x dice multiplier */
    int add;    /*  10 -> +10 extra damage */
    cptr msg;   /* "%^s is <color:G>dissolved</color>!" */
    cptr name;  /* "Acid" */
} slay_t, *slay_ptr;
extern int slay_compare(slay_ptr left, slay_ptr right);
extern void slay_scale(slay_ptr slay, int pct); /* e.g. 200 => doubling the slay effectiveness */

/* for plr_shoot and plr_throw: */
typedef struct {
    int        id;
    mon_p      check_p;
    mon_lore_f lore_f;
    int        mul;
    int        add;
    cptr       name;  /* for plr_attack_display() */
    cptr       lore_msg; /* for obj_learn_slay() */
    int        gf;  /* better support for branded weapons */
    int        mask_id; /* don't show SLAY_ORC if KILL_ORC as well */
} slay_info_t, *slay_info_ptr;

extern slay_info_t slay_tbl[];
extern slay_info_t brand_tbl[];

extern slay_t slay_info_apply(slay_info_ptr info, mon_ptr mon, obj_ptr obj);
extern slay_t slay_find_best(u32b flags[OF_ARRAY_SIZE], mon_ptr mon, obj_ptr obj, slay_info_ptr tbl);
extern slay_t slay_apply_force(slay_t slay, obj_ptr obj, int cost, bool display);

extern slay_t slay_mon(u32b flags[OF_ARRAY_SIZE], mon_ptr mon, obj_ptr obj);
extern slay_t slay_lookup(int id); /* e.g. lookup OF_BRAND_ACID or OF_KILL_ORC */
extern bool   obj_slays_mon(obj_ptr obj, mon_ptr mon);

/*************************************************************************
 * Context: Holds information about the current round of attacks, including the 
 * current weapon, total damage amounts, and monster we are fighting. Offers hooks
 * for customization as well as a few input fields clients can set as they like.
 *************************************************************************/

/* Stop codes allow the melee to end prematurely, and give the reason why (context->stop).
 * These are shared with mon_attack_t.stop (mon_attack.h). stop behaves as a "bool" so
 * that non-zero is true and zero is false. */
enum {
    STOP_CONTINUE = 0,  /* i.e., don't stop! */
    STOP_MON_DEAD,
    STOP_MON_MOVED,
    STOP_MON_FEAR,
    STOP_MON_PARALYZED,
    STOP_MON_CONFUSED,
    STOP_MON_FRIENDLY,
    STOP_MON_SPECIAL,
    STOP_PLR_FEAR,
    STOP_PLR_DEAD,
    STOP_PLR_LEAVING,
    STOP_PLR_MOVED,
    STOP_PLR_PARALYZED,
    STOP_PLR_CONFUSED,
    STOP_PLR_SPECIAL
};

/* Flags for the player attack context (PAC) */
#define PAC_RETALIATE     0x0001
#define PAC_NO_START_MSG  0x0002 /* omit banner message (e.g. "You attack the Big Ugly Monster:") */
#define PAC_NO_INNATE     0x0004 /* skip innate attacks ... normal melee only */
#define PAC_NO_WEAPON     0x0008 /* skip normal melee ... innate only */
#define PAC_DISPLAY       0x0010 /* hack for character sheet display ... no attack forthcoming! */
#define PAC_ANIMATE       0x0020
#define PAC_ONE_BLOW      0x0040 /* this common case is hard for clients to do correctly */
#define PAC_NO_AURA       0x0080 /* "ranged melee" should not burn the plr (e.g. Tobi Izuna) */

/* We will handle a (hopefully) small number of special attack codes (context->type).
 * Everything else should be handled thru hooks */
enum {
    PLR_HIT_NORMAL = 0,
    PLR_HIT_ACID,
    PLR_HIT_ELEC,
    PLR_HIT_FIRE,
    PLR_HIT_COLD,
    PLR_HIT_POISON,
    PLR_HIT_VAMP,
    PLR_HIT_VORPAL,
    PLR_HIT_MANA,
    PLR_HIT_STUN,
    PLR_HIT_CRIT,
    PLR_HIT_CONFUSE,
    PLR_HIT_KILL,   /* same as poison needle */
    PLR_HIT_QUAKE,
    PLR_HIT_KNOCKBACK,
    PLR_HIT_RUSH_ATTACK,
    PLR_HIT_RETALIATE,
    PLR_HIT_TELEPORT,
    PLR_HIT_CUSTOM = 5000 /* <== Begin custom context->type codes here */
};

typedef struct plr_attack_s plr_attack_t, *plr_attack_ptr;

/* callback hooks allow for rich customization. set these up directly in the context
 * before calling plr_attack, or rely on class|race_t.attack_init to install handlers
 * in case you need control over normal (movement initiated) player attacks.
 * XXX Not needed yet, but these are in a struct to allow chaining overrides. */
typedef struct {
void  (*begin_f)(plr_attack_ptr context);        /* perform custom initialization: called only if attack will actually occur */
bool  (*begin_weapon_f)(plr_attack_ptr context); /* return false to skip this weapon or blow */
void  (*mod_blows_f)(plr_attack_ptr context);    /* adjust context->blow_ct ... see comment below! */
bool  (*check_hit_f)(plr_attack_ptr context);    /* some techniques might always hit. non-null bypasses normal logic. */
void  (*before_hit_f)(plr_attack_ptr context);   /* called before every hit (but not for misses) */
void  (*mod_damage_f)(plr_attack_ptr context);   /* goose context->dam if you like */
void  (*after_hit_f)(plr_attack_ptr context);    /* called after every hit, even the killing blow (e.g. Mauler's Splatter) */
void  (*end_weapon_f)(plr_attack_ptr context);
void  (*end_f)(plr_attack_ptr context);          /* always called if plr actually attacked. check context->stop to see if monster survived */

      /* Custom slays and brands. The best so far is provided for reference.
       * Returned slay_t is only used if it is the best (In other words, there
       * is no need for you to check). Be very careful with custom slay and
       * brand hooks if you allow PAC_DISPLAY. context->mon and context->race
       * are null for PAC_DISPLAY and you really shouldn't be loring anyway.
       * Checkout samurai.c for how to do things properly. */
slay_t (*calc_slay_f)(plr_attack_ptr context, slay_ptr best_slay);
slay_t (*calc_brand_f)(plr_attack_ptr context, slay_ptr best_brand);
} plr_attack_hooks_t, *plr_attack_hooks_ptr;
/* Hook Summary: Each indentation is a loop. Setting stop aborts *all* remaining attacks.
 * begin_f
 *   begin_weapon_f    <= loop for each melee weapon and each innate blow
 *   mod_blows_f
 *     check_hit_f     <= loop for each strike 1 .. blow_ct
 *     if hit
 *       before_hit_f
 *       calc_slay_f
 *       calc_brand_f
 *       mod_damage_f
 *       after_hit_f
 *   end_weapon_f      <= end processing for current weapon or innate blow ... onto the next
 * end_f               <= end of *all* attacks with all possible weapons and innate blows
 *
 * XXX mod_blows_f won't be called during PAC_DISPLAY code. If you are
 * offering the user custom display on SPELL_ON_BROWSE, you'll need to use
 * the begin_weapon_f and end_weapon_f hooks. This is especially true as
 * blow_ct is a non-fractional number. For example 260->2 or 3 in blow_ct,
 * and it is 2 or 3 that gets modified by mod_blows_f, not 260. cf _rapid_strike_spell
 * in race_dragon.c for an example of adding fractional blows with correct display.
 * monk_double_attack_spell is another example. Since this can be confusing,
 * I may remove the mod_blows_f altogether. XXX
 */

/* Allow class specific initialization of context (cf class_t and race_t).
 * Initializer is called during plr_attack_begin() after making some preliminary checks
 * and setting up default info. Typically, this handler will then install specific hooks
 * in context (cf samurai.c or ninja.c for examples) */
typedef void (*plr_attack_init_f)(plr_attack_ptr context);

/* OK, here is the actual context. We support normal melee with weapons or bare-hands.
 * We support innate blows (plr->innate_blows). Possessors should use calc_innate_attacks
 * to *copy* unmasked blows from mon_race.blows to plr->innate_blows. XXX */
struct plr_attack_s
{
/* Input */
int             mode;     /* e.g. PLR_HIT_ELEC. Optional and reserved for special class techniques */
int             flags;    /* options for this attack (see PAC_* flags) */
cptr            attack_desc; /* "You <desc> <monster>:"; defaults to "attack" */
int             to_h;     /* bonus to hit over and above normal (calc_bonuses) */
int             to_d;     /* bonus to damage over and above normal (calc_bonuses) */
int             energy;   /* 0=>normal energy use (100) */

/* State: Info about monster we are fighting. Global state for all attacks this round. */
int             stop;       /* non-zero ends the carnage and gives the reason why (e.g. STOP_MON_DEAD) */
mon_ptr         mon;        /* monster we are fighting */
int             retaliation_ct;
mon_race_ptr    race;       /* XXX Need to recompute if monster polymorphs. Don't forget this!! */
                            /* XXX duplicate of mon->race, but needed to detect polymorph race change */
char            mon_full_name[MAX_NLEN]; /* "the Greater titan" */
char            mon_name[MAX_NLEN];      /* "he" */
char            mon_name_obj[MAX_NLEN];  /* "him" */
point_t         mon_pos;    /* remember starting pos to detect monster relocation. distance attacks ok */
point_t         plr_pos;    /* remember starting pos to detect player relocation (e.g. Nexus aura) */
bool            fear;       /* mon_take_hit wants this ... limit message spam to a single fear message at the end */

/* State: Information about the current attack. Always a copy of appropriate plr->attack_info or innate_attack_info */
plr_attack_info_t info;
u32b            obj_flags[OF_ARRAY_SIZE];

/* State: Current Weapon or Martial Arts. Managed by plr_attack() */
int             hand;
obj_ptr         obj;
int             skill;    /* calculated skill of attack (not counting stun) */
char            obj_name[MAX_NLEN];

/* State: Innate Attacks. plr->innate_blows includes possessor blows, mutations, and monster mode blows
 * During innate processing, the hand fields will be cleared, and vice versa. (check hand == HAND_NONE) */
int             which_blow;   /* runs [0, vec_length(plr->innate_blows)) */
mon_blow_ptr    blow;

/* State: Info about current round of attacks. Resets for each weapon */
int             blow_ct;    /* calculated from fractional blows (e.g. 260 -> 2(40%) or 3(60%)) */
int             drain_left; /* OF_BRAND_VAMP limited to 50 per round of attacks with each weapon */
int             drain_hits; /* number of hits that drained life */

/* State: Info about current strike. Resets for each blow. Used for both normal and innate processing.  */
int             dam_base;   /* base damage of current strike (dice only, with crit and vorpal boosts) */
                            /* XXX This is for OF_BRAND_TIME and OF_BRAND_CHAOS weapons so they can
                             * gf_affect_m with the correct amount of damage. For innate attacks, the
                             * monastic lich is using this to gf_affect_m with GF_UNLIFE. Since the intention
                             * is to gf_affect_m with this amount, it should *not* be boosted by any
                             * existing slays but should be boosted by crit/vorpal/technique  XXX */
int             dam_xtra;   /* slay_t.add for crits and slays */
int             dam;        /* total damage of current strike */
int             dam_drain;  /* typically same as dam, but perhaps less (cf ninja). For OF_BRAND_VAMP */
int             technique;  /* Handle ambush, backstab, shadowstrike ... This is an extra multipler
                set to non-zero if a known technique is being used. For example, plr->ambush or
                plr->backstab allow any class to gain access to these abilities. We also handle
                the ninja's shadowstrike since burglary rogues can do this too. Don't mess with this! */

/* State: Info about all attacks ... running totals for all weapons and innate attacks */
int             dam_total;  /* running total for all hits against this monster. */
int             hits;       /* number of hits landed in all attacks */
int             misses;     /* number of misses in all attacks */
int             counter;    /* total number of swings. stealth techniques give  bonuses on the first
                               swing with the first weapon so check counter == 1 */

/* Delayed Effets done once at the end */
bool            do_quake;
bool            do_blink;
int             do_knockback;
bool            do_sleep;

plr_attack_hooks_t hooks;
void           *cookie; /* in case you need more info for your callbacks */
};

/* The currently active attack ... provided you are using the normal APIs of
 * plr_attack[_normal]() and plr_retaliate(). The goal is to remove some of
 * the "hack" variables like retaliation_hack and melee_hack (XXX In progress) */
extern plr_attack_ptr plr_attack_current(void);

/*************************************************************************
 * Normal Entrypoints
 *************************************************************************/

/* Attack with all weapons + innate attacks. */
extern bool plr_attack_normal(point_t pos); 
extern bool plr_attack(plr_attack_ptr context, point_t pos); 

/* For class specific techniques, you can either rely on class_t.attack_init
 * to install hooks, or you can install them up front on a context.
 * Everything ends up going thru plr_attack_special_aux (usually with range=1)
 * which then uses the current target if possible, auto-selects the nearest
 * target in range if possible or finally, as a last resort, prompts the
 * player for a target. */
extern bool plr_attack_special(int type, int flags); /* same as plr_attack_ranged(type, flags, 1) */
extern bool plr_attack_ranged(int type, int flags, int range);
extern bool plr_attack_special_aux(plr_attack_ptr context, int range);

/* Retaliate with a single strike (random weapon selection) */
extern bool plr_retaliate(plr_attack_ptr context, point_t pos);

/*************************************************************************
 * Low Level Entrypoints: You better know what you are doing!
 *************************************************************************/
extern void plr_attack_start_msg(plr_attack_ptr context);

/* Setup the context. check for anti-melee, etc. Every successful call to
 * plr_attack_begin() *must* be matched with a call to plr_attack_end() */
extern bool plr_attack_begin(plr_attack_ptr context, point_t pos); 
/* Continue setting up the context for weapon-based melee -or-
 * for innate attacks. */
extern bool plr_attack_init_hand(plr_attack_ptr context, int hand);
extern bool plr_attack_init_innate(plr_attack_ptr context, int which);
/* Every *successful* call to plr_attack_begin() *must* be matched with a
 * call to plr_attack_end() for cleanup */
extern void plr_attack_end(plr_attack_ptr context); 

/* Do a single round of attacks, or a single strike. Calculate slays,
 * apply criticals, handle vampiric weapons and other fun stuff */
extern bool plr_attack_mon(plr_attack_ptr context); /* round of attacks with single weapon */
extern bool plr_hit_mon(plr_attack_ptr context);  /* single strike */
extern bool plr_check_hit(plr_attack_ptr context); /* check for hit. handle standard techniques like backstab */
extern void plr_on_hit_mon(plr_attack_ptr context); /* monster auras */

/* Apply auras for spells that touch the player; or for riding players */
extern void plr_on_touch_mon(mon_ptr mon); /* necromancer */

extern void monk_hit_mon(plr_attack_ptr context);  /* single successful strike (monk.c) */
extern void monk_apply_slays(plr_attack_ptr context); /* monk.c */

/*************************************************************************
 * Criticals
 *************************************************************************/
/* for mon_attack */
extern slay_t mon_attack_crit(mon_race_ptr race, mon_blow_ptr blow);
extern int mon_crit_avg_mul(mon_race_ptr race, mon_blow_ptr blow);
extern int mon_crit_chance(mon_race_ptr race, mon_blow_ptr blow);

/* for plr_throw and plr_shoot */
#define CRIT_FREQ_ROLL  5000
#define CRIT_QUAL_ROLL  650
extern slay_t crit_aux(int roll, int weight, int skill);
extern int crit_chance_aux(int roll, int weight, int skill);
extern slay_t crit_avg(int roll, int weight); /* add is scaled by 100 */
extern void   crit_doc(doc_ptr doc, slay_t crit, int crit_chance);

extern slay_t plr_crit(int roll, int weight, int skill, plr_crit_ptr scale); 
extern int    plr_crit_chance(int roll, int weight, int skill, plr_crit_ptr scale);
extern slay_t plr_crit_avg(int roll, int weight, plr_crit_ptr scale, int crit_chance);

/*************************************************************************
 * Display Code (Character Sheet)
 *************************************************************************/
extern void plr_attack_display(void);
extern void plr_attack_display_special(int type, int flags);
extern void plr_attack_display_aux(plr_attack_ptr context);
extern void plr_attack_display_doc(plr_attack_ptr context, doc_ptr doc);

/*************************************************************************
 * Calculate Blows per Round
 * By tradition, the number of blows are calculated in calc_bonuses (PU_BONUS)
 *************************************************************************/
extern void plr_calc_blows_hand(int hand);  /* normal melee weapon (not martial arts) */
extern void plr_calc_blows_innate(mon_blow_ptr blow);

#endif
