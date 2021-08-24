#ifndef INCLUDED_POSCHENGBAND_H
#define INCLUDED_POSCHENGBAND_H

/* Poschengband is an Angband variant based upon Hengband
 * see ../lib/file/credits.txt
 * 
 * The code is currently being extensively re-factored.
 * This file defines and explains some of the core data
 * types in the codebase.
 */

/* plr
 * You are the player (plr), and there is only one of you against many, many monsters.
 * This is a single player game, and the "plr" module manages everything about the current
 * player. There is a global "plr_ptr plr" so that code like "plr->pos" or "plr->chp" is
 * quite common and we don't pass around plr_ptrs when calling functions.
 *
 * cf plr_race.h for races (human, elf, dwarf, ... as well as race_dragon.c, race_troll.c, ...)
 *    plr_class.h for classes (warrior.c, mage.c, ninja.c, ...)
 *    plr_attack.h for melee
 *    XXX plr_shoot.h for archery (Pending) XXX
 *    plr_throw.h for throwing objects
 *    do-spell.c for book-based spell realms XXX refactor pending
 *    plr_tim.h for timed effects
 *    plr_birth.c for creating a new character
 *    plr_display.c for the "character sheet"
 */
typedef struct player_type plr_t, *plr_ptr;
    typedef struct race_s plr_race_t, *plr_race_ptr;
    typedef struct class_s plr_class_t, *plr_class_ptr;
    typedef struct plr_attack_info_s plr_attack_info_t, *plr_attack_info_ptr; /* plr->attack_info */
    typedef struct plr_shoot_info_s plr_shoot_info_t, *plr_shoot_info_ptr; /* plr->shooter_info */
    typedef struct plr_shoot_s plr_shoot_t, *plr_shoot_ptr;
    typedef struct plr_tim_s  plr_tim_t, *plr_tim_ptr; /* XXX plr->timers is currently _timers in plr_tim.c */
    typedef struct caster_info_s plr_magic_t, *plr_magic_ptr;
    typedef struct blows_calc_s blows_calc_t, *blows_calc_ptr;
    typedef struct skills_s skills_t, *skills_ptr;
    /* XXX player_type and race_s need to be refactored ... cf types.h */

/* mon
 * Monsters are out to get you, and they don't mind swarming in vast hordes to do so!
 * mon_ptrs are generally passed as the first parameter to monster functions.
 * XXX but many still take a "m_idx" and do a lookup ... still refactoring XXX
 *
 * cf mon_ai.h for monster behavior
 *    mon_attack.h for monster melee
 *    mon_spell.h for monster spells
 *    mon_display.c for monster recall
 *    mon_tim.h for timed monster effects
 */
typedef struct monster_type mon_t, *mon_ptr;
    /* mon_race: o.black or D.Smaug. Every monster has a race that determines how it
     * may attack, what spells it may cast, how it moves, thinks and acts. (../lib/edit/r_info.txt) */
    typedef struct mon_race_s mon_race_t, *mon_race_ptr; /* XXX mon->race and mon->ap_race */
    typedef struct mon_race_s monster_race; /* XXX remove these */
    /* mon_pack: monsters may group into packs for collective behavior (mon_ai.c) */
    typedef struct mon_pack_s mon_pack_t, *mon_pack_ptr;   /* XXX mon->pack */
    /* mon_tim: manages timed effects such as fast, invulnerable, confused, etc. */
    typedef struct mon_tim_s  mon_tim_t, *mon_tim_ptr;     /* mon->timers */
    typedef struct equip_template_s equip_template_t, *equip_template_ptr;
    typedef struct mon_blow_s mon_blow_t, *mon_blow_ptr;
    typedef struct mon_spell_s mon_spell_t, *mon_spell_ptr;
    typedef struct mon_spells_s mon_spells_t, *mon_spells_ptr;
    typedef struct mon_effect_s mon_effect_t, *mon_effect_ptr;
    typedef struct mon_aura_s mon_aura_t, *mon_aura_ptr;
    typedef struct mon_drop_s mon_drop_t, *mon_drop_ptr;
    typedef struct mon_rule_s mon_rule_t, *mon_rule_ptr;

    typedef bool (*mon_p)(mon_ptr mon);
    typedef void (*mon_f)(mon_ptr mon);
    typedef bool (*mon_race_p)(mon_race_ptr race);
    typedef void (*mon_race_f)(mon_race_ptr race);
    typedef void (*mon_lore_f)(mon_ptr mon);
    /* monster_type and monster_race need to be refactored ... cf types.h */

/* obj
 * Objects are loot, earned thru the sweat of your brow. Wear them, eat them,
 * horde them! The obj module manages this sort of stuff.
 *
 * cf obj_display for object recall
 *    ego.c for "excellent" object
 *    art.c for "special" objects
 *    devices.c for wands, staves, rods, potions and scrolls
 *    obj_prompt for object selection (UI)
 *    inv.h for plr inventory
 *    pack.h for plr's pack
 *    equip.h for plr's worn equipment
 *    quiver.h for plr's quiver (if one is equipped)
 */
typedef struct object_kind obj_kind_t, *obj_kind_ptr;
typedef struct object_type obj_t, *obj_ptr;
typedef struct artifact_type art_t, *art_ptr;
typedef struct ego_type ego_t, *ego_ptr;

typedef void (*obj_f)(obj_ptr obj);
typedef bool (*obj_p)(obj_ptr obj);
typedef int  (*obj_cmp_f)(obj_ptr left, obj_ptr right);
typedef bool (*obj_create_f)(obj_ptr obj, u32b mode);

/* dun */
typedef struct dun_s dun_t, *dun_ptr;
typedef struct dun_cell_s dun_grid_t, *dun_grid_ptr;  /* 'grid' is a poor name: synecdoche */
typedef struct dun_cell_s dun_cell_t, *dun_cell_ptr;
typedef struct dun_flow_s dun_flow_t, *dun_flow_ptr;  /* dun->flow, mon->flow, travel.flow, stairs->flow, pack->flow */
typedef struct dun_frac_s dun_frac_t, *dun_frac_ptr;
typedef struct dun_gen_s dun_gen_t, *dun_gen_ptr;
typedef struct dun_mgr_s dun_mgr_t, *dun_mgr_ptr;     /* dun_mgr() singleton */
typedef struct dun_type_s dun_type_t, *dun_type_ptr;
typedef struct dun_tim_s  dun_tim_t, *dun_tim_ptr;    /* dun->timers */
typedef struct dun_world_s dun_world_t, *dun_world_ptr;

typedef bool (*dun_cell_p)(dun_cell_ptr);

/* Note: I personally prefer typedefs to "struct dun *"
 * style syntax. This increases readability for me, but
 * makes forward declares impossible since typedefs may
 * not be duplicated (-wpedantic).
 *
 * Example: dun and mon each depend on the other, since
 * a dun manages all the monsters on the level, while each
 * monster knows which level it is on.
 * dun.h:
 * struct mon_t;
 * struct dun_t { "map<int, struct mon_t *>"; ... };
 * void foo(struct mon_t *mon);
 *
 * mon.h:
 * struct dun_t;
 * struct mon_t { struct dun_t *dun; ... };
 * void bar(struct mon_t *mon);
 *
 * This becomes:
 * poschengband.h:
 * typedef struct mon_s mon_t, *mon_ptr;
 * typedef struct dun_s dun_t, *dun_ptr;
 *
 * dun.h:
 * struct dun_s { "map<int, mon_ptr>"; ... };
 * void foo(mon_ptr mon);
 *
 * mon.h:
 * struct mon_s { dun_ptr dun; ... };
 * void bar(mon_ptr mon);
 */

/* Finally, some of my personal coding idioms:
 *
 * Every module has a short name like "obj" or "mon", and defines
 * a core type like "obj_t" or "mon_t". The type is generally a heap
 * object created with "obj_alloc" or "mon_alloc" and then destroyed
 * with "obj_free" or "mon_free".
 *
 * XXX Occasionally, stack based object memory management is provided
 * with "foo_create" and "foo_destroy". You need not zero initialize
 * stack structures: foo_create will do that for you. XXX
 *
 * Every module api is prefixed with the short module name and
 * takes the core type as the first parameter: mon_load(mon_ptr mon, ...)
 * mon_save(mon_ptr mon, ...). (In C++, this would be mon->load(...)
 * and mon->save(...) but C programmers build up "programming character"
 * thru more typing.)
 *
 * Modules often have submodules with short names prefixed with the
 * parent module name such as "mon_tim". This is the "tim" submodule
 * of the "mon" module. Submodules follow the module rules as if
 * the module name were "foo_bar" (so core type is foo_bar_t, created
 * with foo_bar_alloc, free'd with foo_bar_free, apis like 
 * foo_bar_display(foo_bar_ptr fb, ...))
 */
#endif
