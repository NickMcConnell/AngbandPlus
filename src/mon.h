#ifndef INCLUDED_MON_H
#define INCLUDED_MON_H

#include "c-vec.h"

/*************************************************************************
 * Source of a projection, effect, damage or summons. Also used for target
 * which might be a position.
 *************************************************************************/
enum who_e {
    WHO_NULL,
    WHO_PLR,
    WHO_MON,
    WHO_TRAP,
    WHO_MIRROR,
    WHO_UNCTRL_POWER,
    WHO_POS,
};
struct who_s
{
    int tag;
    union {
        mon_ptr mon;
        point_t pos;
    } v;
};
typedef struct who_s who_t;
extern who_t who_create_null(void);
extern who_t who_create_plr(void);
extern who_t who_create_mon(mon_ptr mon);
extern who_t who_create_trap(point_t pos);
extern who_t who_create_mirror(point_t pos);
extern who_t who_create_unctrl_power(void);
extern who_t who_create_pos(point_t pos);

extern bool who_is_null(who_t who);
extern bool who_is_plr(who_t who);
extern bool who_is_mon(who_t who);
extern bool who_is_pet(who_t who);
extern bool who_is_mon_id(who_t who, u32b id); /* e.g. if (who_is_mon_id(who, plr->riding)) { ... } */
extern bool who_is_trap(who_t who);
extern bool who_is_mirror(who_t who);
extern bool who_is_unctrl_power(who_t who);
extern bool who_is_pos(who_t who);

extern bool who_equals(who_t who, who_t what);

extern mon_ptr who_mon(who_t who); /* NULL unless WHO_MON */
extern point_t who_pos(who_t who);

/************************************************************************
 * MON() directives for quests, rooms, friends and kin
 *
 * 'friends' is from Vanilla and allows more interesting monster packs/groups.
 * Use the M: line in r_info.txt.
 * I added 'kin' as well for S_KIN spells (K: line in r_info.txt)
 ************************************************************************/
#define MON_RULE_RANDOM      0x0001  /* which is ignored */
#define MON_RULE_TYPE        0x0002  /* which is a summon_specific_e constant */
#define MON_RULE_CHAR        0x0004  /* which is a d_char */
                                     /* otherwise, which is a mon_race->id (sym_t) */
#define MON_RULE_NO_GROUP    0x0008
#define MON_RULE_NO_SLEEP    0x0010
#define MON_RULE_UNIQUE      0x0020
#define MON_RULE_FRIENDLY    0x0040
#define MON_RULE_HASTE       0x0080
#define MON_RULE_EVIL        0x0100
#define MON_RULE_GOOD        0x0200
#define MON_RULE_VAULT       0x0400
#define MON_RULE_STOP        0x0800  /* stop applying rules if this one worked */
#define MON_RULE_ANCESTOR    0x1000  /* e.g. MON("D.black", ANCESTOR) allows d.black.baby|young|mature */
#define MON_RULE_SAME        0x2000

struct mon_rule_s
{
    mon_rule_ptr next;

    u32b   flags;
    s32b   which;
    byte   pct;
    byte   lvl_boost;
    byte   min_lvl;
    dice_t amt;
};
extern mon_rule_ptr mon_rule_alloc(void);
extern void         mon_rule_free(mon_rule_ptr rule);

extern errr         mon_rule_parse(mon_rule_ptr rule, char *buf);
extern errr         mon_rule_parse_aux(mon_rule_ptr rule, char **args, int arg_ct);

extern bool         mon_rule_is_valid(mon_rule_ptr rule);
extern mon_race_ptr mon_rule_race(mon_rule_ptr rule);
extern bool         mon_rule_filter(mon_rule_ptr rule, mon_race_ptr race);
extern u32b         mon_rule_mode(mon_rule_ptr rule);
extern int          mon_rule_amt(mon_rule_ptr rule); /* checks pct and rolls amt */

/************************************************************************
 * XXX
 ************************************************************************/
extern mon_ptr mon_alloc(void);
extern void    mon_free(mon_ptr mon);

extern mon_ptr mon_parent(mon_ptr mon);
extern int     mon_lvl(mon_ptr mon);
extern int     mon_race_lvl(mon_race_ptr race);
extern int     mon_ac(mon_ptr mon);
extern int     mon_dis(mon_ptr mon); /* effective distance to plr (cached) even if off-level */
extern int     mon_skill_thn(mon_ptr mon);
extern int     mon_race_skill_thn(mon_race_ptr race);

extern bool mon_show_msg(mon_ptr mon);

extern mon_race_ptr mon_true_race(mon_ptr mon);

extern void mon_anger(mon_ptr mon);
extern void mon_anger_spell(mon_ptr mon, int dam);
extern void mon_anger_shoot(mon_ptr mon, int dam);

extern int  mon_stun_amount(int dam);
extern bool mon_stun(mon_ptr mon, int amt);

extern void mon_set_hunted(mon_ptr mon);
extern void mon_drop_carried_obj(mon_ptr mon);

/************************************************************************
 * Lore
 ************************************************************************/
extern bool mon_lore_allow(mon_ptr mon);
extern void mon_lore_move(mon_ptr mon, u32b mask);
extern void mon_race_lore_move(mon_race_ptr race, u32b mask);
extern void mon_lore_kind(mon_ptr mon, u32b mask);
extern void mon_race_lore_kind(mon_race_ptr race, u32b mask);
extern void mon_lore_abilities(mon_ptr mon, u32b mask);
extern void mon_race_lore_abilities(mon_race_ptr race, u32b mask);
extern void mon_lore_attributes(mon_ptr mon, u32b mask);
extern void mon_race_lore_attributes(mon_race_ptr race, u32b mask);
extern void mon_lore_resist(mon_ptr mon, int gf);
extern void mon_race_lore_resist(mon_race_ptr race, int gf);
extern void mon_lore_align(mon_ptr mon);
extern void mon_race_lore_align(mon_race_ptr race);

extern void mon_lore_sighting(mon_ptr mon);
extern void mon_race_lore_sighting(mon_race_ptr race);
extern void mon_lore_death(mon_ptr mon);
extern void mon_lore_turn(mon_ptr mon);
extern void mon_lore_spell(mon_ptr mon, mon_spell_ptr spell);
extern void mon_race_lore_spell(mon_race_ptr race, mon_spell_ptr spell);
extern void mon_lore_spell_failure(mon_ptr mon);
extern void mon_lore_wake(mon_ptr mon);
extern void mon_lore_sleep(mon_ptr mon);

extern void mon_lore_aura(mon_ptr mon, mon_aura_ptr aura);
extern void mon_race_lore_aura(mon_race_ptr race, mon_aura_ptr aura);

/************************************************************************
 * Resistance
 ************************************************************************/
extern bool mon_resist(mon_ptr mon, int gf);
extern bool mon_race_resist(mon_race_ptr race, int gf);
extern bool mon_immune(mon_ptr mon, int gf);
extern bool mon_race_immune(mon_race_ptr race, int gf);
extern bool mon_vuln(mon_ptr mon, int gf);
extern bool mon_race_vuln(mon_race_ptr race, int gf);
extern int  mon_res_pct(mon_ptr mon, int gf);
extern int  mon_race_res_pct(mon_race_ptr race, int gf);
extern int  mon_res_calc_dam(mon_ptr mon, int gf, int dam);

/************************************************************************
 * Motion
 ************************************************************************/
extern int  mon_move_range(mon_ptr mon);
extern int  mon_race_move_range(mon_race_ptr race);

extern bool mon_never_move(mon_ptr mon);
extern bool mon_race_never_move(mon_race_ptr race);
extern void mon_lore_never_move(mon_ptr mon);

extern bool mon_can_open_door(mon_ptr mon);
extern bool mon_race_can_open_door(mon_race_ptr race);
extern void mon_lore_open_door(mon_ptr mon);

extern bool mon_can_bash_door(mon_ptr mon);
extern bool mon_race_can_bash_door(mon_race_ptr race);
extern void mon_lore_bash_door(mon_ptr mon);

extern bool mon_can_push_mon(mon_ptr mon);
extern bool mon_race_can_push_mon(mon_race_ptr race);
extern void mon_lore_push_mon(mon_ptr mon);

extern bool mon_can_trample_mon(mon_ptr mon);
extern bool mon_race_can_trample_mon(mon_race_ptr race);
extern void mon_lore_trample_mon(mon_ptr mon);

extern bool mon_can_pickup_obj(mon_ptr mon);
extern bool mon_race_can_pickup_obj(mon_race_ptr race);
extern void mon_lore_pickup_obj(mon_ptr mon);

extern bool mon_can_destroy_obj(mon_ptr mon);
extern bool mon_race_can_destroy_obj(mon_race_ptr race);
extern void mon_lore_destroy_obj(mon_ptr mon);

extern bool mon_can_tunnel(mon_ptr mon);
extern bool mon_race_can_tunnel(mon_race_ptr race);
extern void mon_lore_tunnel(mon_ptr mon);

extern bool mon_can_passwall(mon_ptr mon);
extern bool mon_race_can_passwall(mon_race_ptr race);
extern void mon_lore_passwall(mon_ptr mon);

extern bool mon_can_passweb(mon_ptr mon);
extern bool mon_race_can_passweb(mon_race_ptr race);
extern void mon_lore_passweb(mon_ptr mon);

extern bool mon_can_clearweb(mon_ptr mon);
extern bool mon_race_can_clearweb(mon_race_ptr race);
extern void mon_lore_clearweb(mon_ptr mon);

extern bool mon_can_swim(mon_ptr mon);
extern bool mon_race_can_swim(mon_race_ptr race);
extern void mon_lore_swim(mon_ptr mon);

extern bool mon_can_fly(mon_ptr mon);
extern bool mon_race_can_fly(mon_race_ptr race);
extern void mon_lore_fly(mon_ptr mon);

extern bool mon_can_climb(mon_ptr mon);
extern bool mon_race_can_climb(mon_race_ptr race);
extern void mon_lore_climb(mon_ptr mon);

extern bool mon_is_trump(mon_ptr mon);
extern bool mon_race_is_trump(mon_race_ptr race);
extern void mon_lore_trump(mon_ptr mon);

extern bool mon_move_quick(mon_ptr mon);
extern bool mon_race_move_quick(mon_race_ptr race);
extern void mon_lore_move_quick(mon_ptr mon);

extern bool mon_ignore_walls(mon_ptr mon);
extern bool mon_ignore_webs(mon_ptr mon);
extern bool mon_race_ignore_webs(mon_race_ptr race);
extern bool mon_can_enter(mon_ptr mon, point_t pos);
extern bool mon_race_can_enter(mon_race_ptr race, point_t pos);
extern bool mon_can_cross(mon_ptr mon, point_t pos);
extern bool mon_can_cross_illusion(mon_ptr mon, point_t pos);
extern bool mon_can_follow_teleport(mon_ptr mon);
extern bool mon_will_flow(mon_ptr mon);

/************************************************************************
 * Kind
 ************************************************************************/
extern bool mon_is_animal(mon_ptr mon);
extern bool mon_race_is_animal(mon_race_ptr race);
extern void mon_lore_animal(mon_ptr mon);

extern bool mon_is_demon(mon_ptr mon);
extern bool mon_race_is_demon(mon_race_ptr race);
extern void mon_lore_demon(mon_ptr mon);

extern bool mon_is_dragon(mon_ptr mon);
extern bool mon_race_is_dragon(mon_race_ptr race);
extern void mon_lore_dragon(mon_ptr mon);

extern bool mon_is_giant(mon_ptr mon);
extern bool mon_race_is_giant(mon_race_ptr race);
extern void mon_lore_giant(mon_ptr mon);

extern bool mon_is_human(mon_ptr mon);
extern bool mon_race_is_human(mon_race_ptr race);
extern void mon_lore_human(mon_ptr mon);

extern bool mon_is_living(mon_ptr mon);
extern bool mon_race_is_living(mon_race_ptr race);
extern void mon_lore_living(mon_ptr mon);

extern bool mon_is_nonliving(mon_ptr mon);
extern bool mon_race_is_nonliving(mon_race_ptr race);
extern void mon_lore_nonliving(mon_ptr mon);

extern bool mon_is_orc(mon_ptr mon);
extern bool mon_race_is_orc(mon_race_ptr race);
extern void mon_lore_orc(mon_ptr mon);

extern bool mon_is_troll(mon_ptr mon);
extern bool mon_race_is_troll(mon_race_ptr race);
extern void mon_lore_troll(mon_ptr mon);

extern bool mon_is_undead(mon_ptr mon);
extern bool mon_race_is_undead(mon_race_ptr race);
extern void mon_lore_undead(mon_ptr mon);

extern bool mon_is_elf(mon_ptr mon);
extern bool mon_race_is_elf(mon_race_ptr race);
extern void mon_lore_elf(mon_ptr mon);

extern bool mon_is_dark_elf(mon_ptr mon);
extern bool mon_race_is_dark_elf(mon_race_ptr race);
extern void mon_lore_dark_elf(mon_ptr mon);

extern bool mon_is_hobbit(mon_ptr mon);
extern bool mon_race_is_hobbit(mon_race_ptr race);
extern void mon_lore_hobbit(mon_ptr mon);

extern bool mon_is_dwarf(mon_ptr mon);
extern bool mon_race_is_dwarf(mon_race_ptr race);
extern void mon_lore_dwarf(mon_ptr mon);

extern bool mon_is_amberite(mon_ptr mon);
extern bool mon_race_is_amberite(mon_race_ptr race);
extern void mon_lore_amberite(mon_ptr mon);

extern bool mon_is_thief(mon_ptr mon);
extern bool mon_race_is_thief(mon_race_ptr race);
extern void mon_lore_thief(mon_ptr mon);

extern bool mon_is_knight(mon_ptr mon);
extern bool mon_race_is_knight(mon_race_ptr race);
extern void mon_lore_knight(mon_ptr mon);

extern bool mon_is_olympian(mon_ptr mon);
extern bool mon_race_is_olympian(mon_race_ptr race);
extern void mon_lore_olympian(mon_ptr mon);

extern bool mon_is_aquatic(mon_ptr mon);
extern bool mon_race_is_aquatic(mon_race_ptr race);
extern void mon_lore_aquatic(mon_ptr mon);

extern bool mon_is_nazgul(mon_ptr mon);
extern bool mon_race_is_nazgul(mon_race_ptr race);
extern void mon_lore_nazgul(mon_ptr mon);

extern bool mon_is_horror(mon_ptr mon);
extern bool mon_race_is_horror(mon_race_ptr race);
extern void mon_lore_horror(mon_ptr mon);

/* XXX Extra 'kinds' XXX */
extern bool mon_race_is_chameleon(mon_race_ptr race);

extern bool mon_is_hound(mon_ptr mon);
extern bool mon_race_is_hound(mon_race_ptr race);

extern bool mon_is_mimic(mon_ptr mon);
extern bool mon_race_is_mimic(mon_race_ptr race);

/* Uniques */
extern bool mon_is_unique(mon_ptr mon);
extern bool mon_race_is_unique(mon_race_ptr race);

extern bool mon_is_fixed_unique(mon_ptr mon);
extern bool mon_race_is_fixed_unique(mon_race_ptr race);

extern bool mon_is_dead_unique(mon_ptr mon);
extern bool mon_race_is_dead_unique(mon_race_ptr race);
extern bool mon_is_living_unique(mon_ptr mon);
extern bool mon_race_is_living_unique(mon_race_ptr race);

/************************************************************************
 * Alignment: Note `align` is now a number ... cf align_e
 ************************************************************************/
extern bool mon_is_evil(mon_ptr mon);
extern bool mon_race_is_evil(mon_race_ptr race);
extern void mon_lore_evil(mon_ptr mon);

extern bool mon_is_good(mon_ptr mon);
extern bool mon_race_is_good(mon_race_ptr race);
extern void mon_lore_good(mon_ptr mon);

extern bool mon_is_neutral(mon_ptr mon);
extern bool mon_race_is_neutral(mon_race_ptr race);

extern cptr mon_align_desc(mon_ptr mon);
extern cptr mon_race_align_desc(mon_race_ptr race);

extern cptr align_desc(int align);
extern int holy_align_dam_pct(int align);
extern int hell_align_dam_pct(int align);

extern int align_hostile(int a1, int a2);
extern int align_hostile_aux(int a1, int a2, int threshold);

/************************************************************************
 * Abilities
 ************************************************************************/
extern bool mon_can_speak(mon_ptr mon);
extern bool mon_race_can_speak(mon_race_ptr race);
extern void mon_lore_can_speak(mon_ptr mon);

extern bool mon_can_reflect(mon_ptr mon);
extern bool mon_race_can_reflect(mon_race_ptr race);
extern void mon_lore_can_reflect(mon_ptr mon);

extern bool mon_is_invisible(mon_ptr mon);
extern bool mon_race_is_invisible(mon_race_ptr race);
extern void mon_lore_is_invisible(mon_ptr mon);

extern bool mon_can_multiply(mon_ptr mon);
extern bool mon_race_can_multiply(mon_race_ptr race);
extern void mon_lore_can_multiply(mon_ptr mon);

extern bool mon_can_regen(mon_ptr mon);
extern bool mon_race_can_regen(mon_race_ptr race);
extern void mon_lore_can_regen(mon_ptr mon);

extern bool mon_can_retaliate(mon_ptr mon);
extern bool mon_race_can_retaliate(mon_race_ptr race);
extern void mon_lore_can_retaliate(mon_ptr mon);

extern bool mon_projects_fear(mon_ptr mon);
extern bool mon_race_projects_fear(mon_race_ptr race);
extern void mon_lore_projects_fear(mon_ptr mon);

/************************************************************************
 * Attributes
 ************************************************************************/
extern bool mon_is_male(mon_ptr mon);
extern bool mon_race_is_male(mon_race_ptr race);
extern void mon_lore_male(mon_ptr mon);

extern bool mon_is_female(mon_ptr mon);
extern bool mon_race_is_female(mon_race_ptr race);
extern void mon_lore_female(mon_ptr mon);

extern bool mon_is_smart(mon_ptr mon);
extern bool mon_race_is_smart(mon_race_ptr race);
extern void mon_lore_smart(mon_ptr mon);

extern bool mon_is_stupid(mon_ptr mon);
extern bool mon_race_is_stupid(mon_race_ptr race);
extern void mon_lore_stupid(mon_ptr mon);

extern bool mon_has_weird_mind(mon_ptr mon);
extern bool mon_race_has_weird_mind(mon_race_ptr race);
extern void mon_lore_weird_mind(mon_ptr mon);

extern bool mon_has_empty_mind(mon_ptr mon);
extern bool mon_race_has_empty_mind(mon_race_ptr race);
extern void mon_lore_empty_mind(mon_ptr mon);

extern bool mon_is_cold_blooded(mon_ptr mon);
extern bool mon_race_is_cold_blooded(mon_race_ptr race);
extern void mon_lore_cold_blooded(mon_ptr mon);

extern bool mon_is_friendly(mon_ptr mon);
extern bool mon_is_temp_friendly(mon_ptr mon);
extern bool mon_race_is_friendly(mon_race_ptr race);
extern void mon_lore_friendly(mon_ptr mon);

extern bool mon_is_ridable(mon_ptr mon);
extern bool mon_race_is_ridable(mon_race_ptr race);
extern void mon_lore_ridable(mon_ptr mon);

extern bool mon_kill_exp(mon_ptr mon);
extern bool mon_race_kill_exp(mon_race_ptr race);
extern void mon_lore_kill_exp(mon_ptr mon);

extern bool mon_immune_illusion(mon_ptr mon);
extern bool mon_race_immune_illusion(mon_race_ptr race);
extern void mon_lore_immune_illusion(mon_ptr mon);

extern bool mon_race_is_template(mon_race_ptr race);
extern bool mon_race_is_deprecated(mon_race_ptr race);

/************************************************************************
 * Saving Throws
 ************************************************************************/
extern bool mon_save_stun(int rlev, int dam);
extern bool mon_save_slow(int rlev, int dam);
extern bool mon_save_disenchant(int r_idx, int dam, int flags);
extern bool mon_save_poly(int rlev, int dam);
extern bool mon_save_smash(int rlev, int dam);
extern bool mon_save_psi(int rlev, int dam);
extern bool mon_save_time(int r_idx, int dam, int flags);
extern bool mon_save_p(mon_ptr mon, int stat);
extern bool mon_save_aux(mon_ptr mon, int power);
extern int  mon_save_r_level(int r_idx);
extern int  mon_save_level(mon_ptr mon);

extern void mon_load(mon_ptr mon, savefile_ptr file);
extern void mon_save(mon_ptr mon, savefile_ptr file);

extern bool mon_can_attack(mon_ptr mon);
extern bool mon_never_blow(mon_ptr mon);
extern bool mon_race_never_blow(mon_race_ptr race);

extern cptr mon_race_describe_singular(mon_race_ptr race);  /* 'o'->"Orc", etc. */
extern cptr mon_race_describe_plural(mon_race_ptr race);  /* 'o'->"Orcs", etc. */

/* XXX */
extern bool mon_is_dead(mon_ptr mon);
extern bool mon_is_deleted(mon_ptr mon);
extern bool mon_is_valid(mon_ptr mon);
extern bool mon_has_smart_flag(mon_ptr mon, int sm);
extern bool mon_is_hostile(mon_ptr mon);
extern bool mon_is_pet(mon_ptr mon);
extern bool mon_is_temp_pet(mon_ptr mon);
extern bool mon_is_cloned(mon_ptr mon);
extern bool mon_is_guardian(mon_ptr mon);

extern term_char_t mon_visual(mon_ptr mon);
extern term_char_t mon_race_visual(mon_race_ptr race);
extern term_char_t mon_visual_ascii(mon_ptr mon);
extern term_char_t mon_race_visual_ascii(mon_race_ptr race);
extern term_char_t mon_race_visual_fuzzy(mon_race_ptr race);
extern char mon_char(mon_ptr mon);
extern char mon_race_char(mon_race_ptr race);
extern bool mon_is_char(mon_ptr mon, char c);
extern bool mon_race_is_char(mon_race_ptr race, char c);
extern bool mon_is_char_ex(mon_ptr mon, cptr s);
extern bool mon_race_is_char_ex(mon_race_ptr race, cptr s);


extern bool mon_race_can_evolve(mon_race_ptr race, mon_race_ptr target);

/************************************************************************
 * Drops
 ************************************************************************/
enum r_drop_e
{
    R_DROP_NONE = 0,

    /* Class Themes */
    R_DROP_WARRIOR,
    R_DROP_WARRIOR_SHOOT,
    R_DROP_ARCHER,
    R_DROP_MAGE,
    R_DROP_PRIEST,
    R_DROP_PRIEST_EVIL,
    R_DROP_PALADIN,
    R_DROP_PALADIN_EVIL,
    R_DROP_SAMURAI,
    R_DROP_NINJA,
    R_DROP_ROGUE,

    /* Racial Themes */
    R_DROP_HOBBIT,
    R_DROP_DWARF,

    R_DROP_JUNK,
    R_DROP_SPELLBOOK,
    R_DROP_MAX
};

struct mon_drop_s
{
    mon_drop_ptr next;
    byte pct;          /* odds of this drop rule applying (0 => 100%) */
    byte dd, ds, base; /* number of objects: XdY+Z. Or 1 if no dice given. */
    byte theme;        /* use a theme rather than an obj_drop_t rule */
    obj_drop_t drop;   /* info on how to make objects for this rule */
};
extern mon_drop_ptr mon_drop_alloc(void);
extern void         mon_drop_free(mon_drop_ptr drop);

extern vec_ptr mon_drop_make(mon_ptr mon);
extern errr    mon_drop_parse(char *buf, mon_race_ptr race, int options); /* parse a single O: line in r_info */
extern void    mon_drop_init(mon_ptr mon);
extern obj_ptr mon_pick_pocket(mon_ptr mon);

/************************************************************************* 
 * Monster Blows
 *   Monsters use blows to attack the player and each other. Set these
 *   in ../lib/edit/r_info.txt on the B: line. Browse the file to learn
 *   the syntax. Savefiles store the "lore" fields by ordinal position,
 *   so reordering blows and effects in r_info may mis-inform players
 *   about their experiences (but won't crash). Monster blows are stored
 *   in monster_race.blows and each monster can have as many as you want.
 *   Each mon_blow_t can also have as many blows as you like. So, you can
 *   totally maul the player if that is your goal!
 *
 *   Players use blows for innate attacks. For example, the possessor will
 *   copy the monster's blows allowing the player to use them in melee.
 *   Most monster mode races also dynamically build blows for the player 
 *   to use. And finally, a few attack mutations will grant weird innate
 *   attacks (horns, beak, trunk, tentacles, etc). All of these blows are
 *   stored in plr->innate_blows in response to the PU_INNATE update
 *   flag. See calc_innate_attacks on race_t and class_t.
 *************************************************************************/

/* [1] Blows have multiple effects, each with its own dice and probability
 *     of occuring. The first effect on the blow is considered the "base"
 *     effect and can be boosted by criticals (player and monster) and
 *     slays (player). Usually, the base effect codes pure damage while
 *     later effects do special stuff. For example:
 *       B:HIT:HURT(5d5+10):STEAL_ITEM
 *       B:TOUCH:DAM(3d3):DISENCHANT
 *       B:SLASH:HURT(8d4+10):CUT(3d3, 25%) */
struct mon_effect_s
{
    s16b   type;   /* either RBE_* or GF_* ... they don't overlap. */
    dice_t dice;   /* damage or the power of the effect */
    byte   pct;    /* probability of occuring: 0 => 100% (as does 100) */
    u16b   lore;   /* monster lore: number of times this effect experienced */
};

/* [2] monster blow flags (MBF_) 
 *     Note that mon_blow_alloc(method) sets blow->flags with table set
 *     defaults (_mon_blow_info). You may then override them as necessary.
 *     For example, RBM_HIT has MBF_MASK_HAND by default, since usually
 *     we can assume a monster hits with its hand (e.g. Novice warrior).
 *     However, various mutations need to turn this off so the player can
 *     still use melee weapons. */
#define MBF_TOUCH      0x0001 /* blow touches so apply auras */
#define MBF_ALLOW_CRIT 0x0002 /* blow can land criticals (if base effect is RBE_HURT or _SHATTER) */
#define MBF_POSSESSOR  0x0004 /* for possessor proficiency */
#define MBF_MONK       0x0008 /* for martial arts XXX unused atm */
#define MBF_MASK_HAND  0x0010 /* player skips this blow when using a weapon (e.g. RBM_PUNCH) */
#define MBF_MASK_BLIND 0x0020 /* player skips this blow when blinded (e.g. RBM_GAZE) */

#define MBF_MONK_PUNCH (MBF_TOUCH | MBF_ALLOW_CRIT | MBF_MONK | MBF_MASK_HAND)
#define MBF_MONK_KICK  (MBF_TOUCH | MBF_ALLOW_CRIT | MBF_MONK)

/* [3] The monster blow (mon_blow_t)
 *     Use mon_blow_alloc(method) to create one and mon_blow_free to destroy one.
 *     Since we use dynamic storage, never declare one on the stack. */
struct mon_blow_s
{
    cptr name;      /* player: display and skills */
    cptr msg;       /* player: custom hit message (e.g. "You strangle %s.") */
    byte method;    /* RBM_* code gives default messaging and behavior. */
    byte power;     /* added to skill. This is 3x a normal to hit bonus (cf BTH_PLUS_ADJ) */
    s16b blows;     /* fractional ... 125 is 1.25 blows per round (25% 2 blows; 75% 1 blow) */
    s16b lore;      /* monster lore: number of times this blow seen (hit or miss) */
    s16b flags;     /* MBF_* flags: default from _mon_blow_info table in mon.c */
    s16b weight;    /* criticals: if 0 see _mon_blow_weight(). Can be set in r_info. */
    byte allocated; /* effects: vec<mon_effect_t> */
    byte effect_ct; /* iterate effects[i] for 0 <= i < effect_ct */
    mon_effect_ptr effects; /* use mon_blow_push_effect() and mon_blow_pop_effect() to manage effects */
};

/* find the first blow of a particular type (e.g. RBM_EXPLODE) */
extern mon_blow_ptr mon_blows_find(vec_ptr blows, int method);

extern mon_blow_ptr mon_blow_alloc(int method);
extern void         mon_blow_free(mon_blow_ptr blow);
extern mon_blow_ptr mon_blow_copy(mon_blow_ptr blow);
extern bool         mon_blow_allow_crit(mon_blow_ptr blow);
extern bool         mon_blow_allow_slay(mon_blow_ptr blow);
extern mon_effect_ptr mon_blow_push_effect(mon_blow_ptr blow, int type, dice_t dice);
extern mon_effect_ptr mon_blow_push_effect_aux(mon_blow_ptr blow, mon_effect_ptr effect); /* r_info parser */
extern void         mon_blow_pop_effect(mon_blow_ptr blow); /* cf _deadly_bite_spell */
extern dice_t       mon_blow_base_dice(mon_blow_ptr blow);
extern int          mon_blow_base_effect(mon_blow_ptr blow);

/* [4] Finally, we have information describing the various RBM_* methods.
 *     These are used for default messages and behavior during combat.
 *     See _mon_blow_info for the table. */
typedef struct {
    int  id;          /* code:    RBM_GAZE */
    cptr name;        /* display: "Gaze" */
    cptr mon_hit_msg; /* monster: "%^s gazes at %s." */
    cptr plr_hit_msg; /* player:  "You gaze at %s." */
    cptr parse;       /* r_info:  "GAZE" */
    int  flags;       /* info:    RBM_MASK_BLIND */
} mon_blow_info_t, *mon_blow_info_ptr;

extern mon_blow_info_ptr mon_blow_info_lookup(int method);
extern mon_blow_info_ptr mon_blow_info_parse(cptr name); /* r_info parser */

extern mon_aura_ptr mon_auras_find(mon_race_ptr race, int gf);

/************************************************************************* 
 * Monster Packs
 ************************************************************************/
enum {
    AI_SEEK = 0,
    AI_LURE,
    AI_GUARD_MON,
    AI_GUARD_POS,
    AI_FEAR,
    AI_SHOOT,
    AI_MAINTAIN_DISTANCE,
    AI_WANDER,
    AI_HUNT,
    AI_PATROL,  /* XXX */
    AI_PETS,
};

struct mon_pack_s
{
    u16b id;
    s16b ai;            /* How is the pack behaving? */
    u32b leader_id;     /* Kill the leader of the pack, and morale will break! (AI_FEAR) */
    u32b guard_id;      /* AI_GUARD_MON ... usually same as leader_id */
    point_t pos;        /* AI_GAURD_POS; AI_WANDER */
    s16b distance;      /* AI_MAINTAIN_DISTANCE */
    u32b prey_id;       /* AI_HUNT uses prey->flow for pathfinding */
    vec_ptr members;    /* current members: use mon_pack_add|remove to maintain */
    dun_flow_ptr flow;  /* AI_WANDER and AI_PATROL */
};

extern mon_pack_ptr mon_pack_alloc(void);
extern void mon_pack_free(mon_pack_ptr pack);

extern void mon_pack_load(mon_pack_ptr pack, savefile_ptr file);
extern void mon_pack_save(mon_pack_ptr pack, savefile_ptr file);

extern void mon_pack_add(mon_pack_ptr pack, mon_ptr mon);
extern void mon_pack_remove(mon_pack_ptr pack, mon_ptr mon);
extern mon_ptr mon_pack_leader(mon_pack_ptr pack);
extern mon_ptr mon_pack_mon(mon_pack_ptr pack, u32b id);
extern mon_ptr mon_pack_representative(mon_pack_ptr pack);
extern int mon_pack_count(mon_pack_ptr pack);
extern str_ptr mon_pack_desc(mon_pack_ptr pack); /* for monster_desc (wizard) */
extern void mon_pack_doc(mon_pack_ptr pack, doc_ptr doc); /* wizard */

extern int mon_pack_align(mon_pack_ptr pack);

extern point_t mon_pack_scatter(mon_pack_ptr pack, int radius);

extern void mon_pack_iter(mon_pack_ptr pack, mon_f f);
extern vec_ptr mon_pack_filter(mon_pack_ptr pack, mon_p p);
extern void mon_pack_anger(mon_pack_ptr pack);
extern void mon_pack_set_target(mon_pack_ptr pack, mon_ptr victim, mon_ptr culprit);

extern void mon_pack_choose_ai(mon_pack_ptr pack);
extern void mon_pack_wander(mon_pack_ptr pack);
extern mon_ptr mon_pack_hunt(mon_pack_ptr pack);

extern void mon_packs_on_death(mon_ptr mon);
extern void mon_packs_on_damage(mon_ptr mon);

/************************************************************************
 * Debugging
 ************************************************************************/
extern void mon_wizard(mon_ptr mon);
extern void mon_wizard_doc(mon_ptr mon, doc_ptr doc); /* debugging for wizard mode */
#endif
