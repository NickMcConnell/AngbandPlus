/*
 * File: object.h
 * Purpose: "extern" declarations for object routines
 */

#ifndef INCLUDED_OBJECT_H
#define INCLUDED_OBJECT_H

#include "../cave.h"

/*** Constants ***/

/* Object origin kinds */

enum
{
    ORIGIN_NONE = 0,
    ORIGIN_MIXED,           /* Stack with mixed origins */
    ORIGIN_BIRTH,           /* Objects created at character birth */
    ORIGIN_STORE,           /* Something you bought */
    ORIGIN_FLOOR,           /* Found on the dungeon floor */
    ORIGIN_DROP,            /* Normal monster drops */
    ORIGIN_DROP_UNKNOWN,    /* Drops from unseen foes */
    ORIGIN_ACQUIRE,         /* Called forth by scroll */
    ORIGIN_CHEAT,           /* Created by wizard mode */
    ORIGIN_CHEST,
    ORIGIN_DROP_SPECIAL,    /* From monsters in special rooms */
    ORIGIN_DROP_PIT,        /* From monsters in pits/nests */
    ORIGIN_DROP_VAULT,      /* From monsters in vaults */
    ORIGIN_DROP_SUMMON,     /* From combat summons */
    ORIGIN_DROP_BREED,      /* From breeders */
    ORIGIN_DROP_POLY,       /* From polymorphees */
    ORIGIN_SPECIAL,         /* On the floor of a special room */
    ORIGIN_PIT,             /* On the floor of a pit/nest */
    ORIGIN_VAULT,           /* On the floor of a vault */
    ORIGIN_LABYRINTH,       /* On the floor of a labyrinth */
    ORIGIN_CAVERN,          /* On the floor of a cavern */
    ORIGIN_RUBBLE,          /* Found under rubble */
    ORIGIN_STOLEN,          /* Stolen by monster (used only for gold) */
    ORIGIN_PLAYER,
    ORIGIN_FOUNTAIN,

    ORIGIN_MAX
};

#define ORIGIN_SIZE FLAG_SIZE(ORIGIN_MAX)

/* ID flags */
#define IDENT_SENSE         0x0001  /* Has been "sensed" */
#define IDENT_WORN          0x0002  /* Has been worn */
#define IDENT_EMPTY         0x0004  /* Is known to be empty */
#define IDENT_KNOWN         0x0008  /* Fully known */
/* xxx */
#define IDENT_ATTACK        0x0020  /* Know combat dice/ac/bonuses */
#define IDENT_DEFENCE       0x0040  /* Know AC/etc bonuses */
#define IDENT_EFFECT        0x0080  /* Know item activation/effect */
/* xxx */
#define IDENT_INDESTRUCT    0x0200  /* Tried to destroy it and failed */
#define IDENT_NAME          0x0400  /* Know the name of ego/artifact if there is one */
#define IDENT_FIRED         0x0800  /* Has been used as a missile */
#define IDENT_NOTART        0x1000  /* Item is known not to be an artifact */
/* xxx */
#define IDENT_AWARE         0x4000  /* Bypasses the "aware" flag */
#define IDENT_WORTHLESS     0x8000  /* Is known to be worthless */

/* Whether to learn egos and flavors with less than complete information */
#define EASY_LEARN 1

/* Maximum number of scroll titles generated */
#define MAX_TITLES 50

/* Values for struct object->marked */
enum
{
    MARK_UNAWARE = 0,
    MARK_AWARE = 1,
    MARK_SEEN = 2
};

/*** Macros ***/

/*
 * Return the "attr" for a given item kind.
 * Use "flavor" if available.
 * Default to user definitions.
 */
#define object_kind_attr(P, K) \
    ((P)->obj_aware[(K)->kidx]? (P)->k_attr[(K)->kidx]: (P)->d_attr[(K)->kidx])

/*
 * Return the "char" for a given item kind.
 * Use "flavor" if available.
 * Default to user definitions.
 */
#define object_kind_char(P, K) \
    ((P)->obj_aware[(K)->kidx]? (P)->k_char[(K)->kidx]: (P)->d_char[(K)->kidx])

/*
 * Return the "attr" for a given item.
 * Use "flavor" if available.
 * Default to user definitions.
 */
#define object_attr(P, T) \
    (object_kind_attr(P, (T)->kind))

/*
 * Return the "char" for a given item.
 * Use "flavor" if available.
 * Default to user definitions.
 */
#define object_char(P, T) \
    (object_kind_char(P, (T)->kind))

/*
 * Return the "attr" for a given item.
 * Use "flavor" if available.
 * Use default definitions.
 */
#define object_attr_default(kind) \
    (kind->flavor? kind->flavor->d_attr: kind->d_attr)

/*
 * True artifacts
 */
#define true_artifact_p(T) \
    ((T)->artifact && !(T)->randart_seed)

/*
 * Rings and Amulets
 */
#define object_is_jewelry(T) \
    (((T)->tval == TV_RING) || ((T)->tval == TV_AMULET))

/*
 * Worthless items
 */
#define worthless_p(T) \
    (object_value_real(NULL, T, 1) < 1)

/*
 * Modes for object_desc().
 */
typedef enum
{
    ODESC_BASE      = 0x00, /* Only describe the base name */
    ODESC_ARTIFACT  = 0x01, /* Describe the base name for artifacts */
    ODESC_SALE      = 0x02, /* Describe the base name for items purchased from floor */
    ODESC_STORE     = 0x04, /* Show entire description */
    ODESC_FULL      = 0x08, /* Also show (squelch) marker */
    ODESC_SINGULAR  = 0x10, /* Always singular */
    ODESC_PREFIX    = 0x20  /* Show prefix */
} odesc_detail_t;

/*
 * Modes for object_info()
 */
typedef enum
{
    OINFO_NONE      = 0x00, /* No options */
    OINFO_FULL      = 0x01  /* Treat object as if fully IDd */
} oinfo_detail_t;

/*
 * Modes for stacking by object_similar()
 */
typedef enum
{
    OSTACK_NONE    = 0x00,  /* No options (this does NOT mean no stacking) */
    OSTACK_STORE   = 0x01,  /* Store stacking */
    OSTACK_PACK    = 0x02,  /* Inventory and home */
    OSTACK_LIST    = 0x04,  /* Object list */
    OSTACK_MONSTER = 0x08,  /* Monster carrying objects */
    OSTACK_FLOOR   = 0x10,  /* Floor stacking */
    OSTACK_QUIVER  = 0x20   /* Quiver */
} object_stack_t;

/*
 * Pseudo-ID markers.
 */
typedef enum
{
    INSCRIP_NULL = 0,       /* No pseudo-ID status */
    INSCRIP_AVERAGE,        /* Item with no interesting features */
    INSCRIP_MAGICAL_BAD,    /* Item with bad combat bonuses */
    INSCRIP_STRANGE,        /* Item with mixed combat bonuses */
    INSCRIP_MAGICAL_GOOD,   /* Item with good combat bonuses */
    INSCRIP_SPLENDID,       /* Obviously good item */
    INSCRIP_EXCELLENT,      /* Ego-item */
    INSCRIP_SPECIAL,        /* Artifact */

    INSCRIP_MAX             /* Maximum number of pseudo-ID markers */
} obj_pseudo_t;

/*
 * Chest check types
 */
enum chest_query
{
    CHEST_ANY,
    CHEST_OPENABLE,
    CHEST_TRAPPED
};

/*
 * Some constants used in randart generation and power calculation
 * - thresholds for limiting to_hit, to_dam and to_ac
 * - fudge factor for rescaling ammo cost
 * (a stack of this many equals a weapon of the same damage output)
 */
#define INHIBIT_POWER   20000
#define INHIBIT_BLOWS   3   /* PWMAngband: limit extra blows/shots/might to +2 */
#define INHIBIT_MIGHT   3
#define INHIBIT_SHOTS   3
#define HIGH_TO_AC      26
#define VERYHIGH_TO_AC  36
#define INHIBIT_AC      56
#define HIGH_TO_HIT     16
#define VERYHIGH_TO_HIT 26
#define INHIBIT_TO_HIT  41
#define HIGH_TO_DAM     16
#define VERYHIGH_TO_DAM 26
#define INHIBIT_TO_DAM  41
#define AMMO_RESCALER   20

#define MAX_WEAPON_DICE 11
#define MAX_AMMO_DICE   9

#define sign(x) ((x) > 0 ? 1 : ((x) < 0 ? -1 : 0))

/*** Functions ***/

/* chest.c */
extern byte chest_trap_type(const object_type *o_ptr);
extern bool is_trapped_chest(const object_type *o_ptr);
extern bool is_locked_chest(const object_type *o_ptr);
extern void unlock_chest(object_type *o_ptr);
extern s16b chest_check(struct player *p, int y, int x, enum chest_query check_type);
extern int count_chests(struct player *p, int *y, int *x, enum chest_query check_type);
extern bool do_cmd_open_chest(struct player *p, int y, int x, s16b o_idx);
extern bool do_cmd_disarm_chest(struct player *p, int y, int x, s16b o_idx);

/* identify.c */
extern bool easy_know(const object_type *o_ptr, bool aware);
extern bool object_is_known(struct player *p, const object_type *o_ptr);
extern bool object_is_known_artifact(const object_type *o_ptr);
extern bool object_is_known_blessed(struct player *p, const object_type *o_ptr);
extern bool object_is_known_not_artifact(const object_type *o_ptr);
extern bool object_was_worn(const object_type *o_ptr);
extern bool object_was_fired(const object_type *o_ptr);
extern bool object_was_sensed(const object_type *o_ptr);
extern bool object_flavor_is_aware(struct player *p, const object_type *o_ptr);
extern bool object_flavor_was_tried(struct player *p, const object_type *o_ptr);
extern bool object_effect_is_known(struct player *p, const object_type *o_ptr);
extern bool object_name_is_visible(const object_type *o_ptr);
extern bool object_ego_is_visible(const object_type *o_ptr);
extern bool object_attack_plusses_are_visible(struct player *p, const object_type *o_ptr);
extern bool object_defence_plusses_are_visible(struct player *p, const object_type *o_ptr);
extern bool object_flag_is_known(struct player *p, const object_type *o_ptr, int flag);
extern bool object_high_resist_is_possible(const object_type *o_ptr);
extern bool object_check_for_ident(struct player *p, object_type *o_ptr);
extern void object_flavor_aware(struct player *p, object_type *o_ptr);
extern void object_flavor_tried(int Ind, object_type *o_ptr);
extern void object_know_all_flags(object_type *o_ptr);
extern void object_notice_everything(struct player *p, object_type *o_ptr,
    bool bypass_aware);
extern void object_notice_indestructible(int Ind, object_type *o_ptr);
extern void object_notice_ego(struct player *p, object_type *o_ptr);
extern void object_notice_sensing(struct player *p, object_type *o_ptr);
extern void object_sense_artifact(struct player *p, object_type *o_ptr);
extern void object_notice_effect(int Ind, object_type *o_ptr);
extern void object_notice_attack_plusses(int Ind, object_type *o_ptr);
extern bool object_notice_flag(struct player *p, object_type *o_ptr, int flag);
extern bool object_notice_flags(struct player *p, object_type *o_ptr, bitflag flags[OF_SIZE]);
extern bool object_notice_curses(struct player *p, object_type *o_ptr);
extern void object_notice_on_defend(struct player *p);
extern void object_notice_on_firing(int Ind, object_type *o_ptr);
extern void object_notice_on_wield(int Ind, object_type *o_ptr);
extern void wieldeds_notice_flag(struct player *p, int flag);
extern void wieldeds_notice_to_hit_on_attack(int Ind);
extern void wieldeds_notice_on_attack(int Ind);
extern bool object_FA_would_be_obvious(struct player *p, const object_type *o_ptr);
extern obj_pseudo_t object_pseudo_other(const object_type *o_ptr);
extern obj_pseudo_t object_pseudo(struct player *p, const object_type *o_ptr, bool aware,
    bool known);
extern void sense_inventory(int Ind);
extern void object_aware_tried(int Ind, object_type *o_ptr, bool ident, bool used);

/* obj-desc.c */
extern void object_kind_name(char *buf, size_t max, const object_kind *kind, bool aware);
extern size_t object_desc(struct player *p, char *buf, size_t max, const object_type *o_ptr,
    odesc_detail_t mode);

/* obj-info.c */
extern void object_info(struct player *p, const object_type *o_ptr, oinfo_detail_t mode);

/* obj-make.c */
extern void free_obj_alloc(void);
extern bool init_obj_alloc(void);
extern object_kind *get_obj_num(int level, bool good);
extern void init_powers(const object_type *o_ptr, int *xtra1, int *xtra2);
extern void dec_power1(const object_type *o_ptr, int *xtra1);
extern void dec_power2(const object_type *o_ptr, int *xtra2);
extern void inc_power1(const object_type *o_ptr, int *xtra1);
extern void inc_power2(const object_type *o_ptr, int *xtra2);
extern void do_fixed_powers(object_type *o_ptr, int xtra1, int xtra2);
extern void undo_fixed_powers(object_type *o_ptr, int xtra1, int xtra2);
extern void get_extra_mods(int xtra1, int xtra2, char *buf, int len);
extern void object_prep(object_type *o_ptr, struct object_kind *k, int lev,
    aspect rand_aspect);
extern s16b apply_magic(struct player *p, int depth, object_type *o_ptr, int lev,
    bool allow_artifacts, bool good, bool great);
extern bool make_object(struct player *p, struct cave *c, object_type *j_ptr, int lev,
    bool good, bool great, s32b *value);
extern void make_gold(struct player *p, object_type *j_ptr, int lev, int coin_type);
extern void make_randart(struct player *p, object_type *o_ptr, artifact_type *a_ptr,
    s32b randart_seed);
extern void ego_min_pvals(object_type *o_ptr);
extern void copy_artifact_data(object_type *o_ptr, const artifact_type *a_ptr);
extern bool make_fake_artifact(object_type *o_ptr, struct artifact *artifact);
extern bool kind_is_good(const object_kind *kind);
extern void ego_apply_magic(object_type *o_ptr, int lev);
extern void fuel_default(object_type *o_ptr);

/* obj-power.c */
extern s32b object_power(struct player *p, const object_type* o_ptr);

/* obj-ui.c */
extern void display_inven(struct player *p);
extern void display_equip(struct player *p);
extern void display_floor(struct player *p, const int *floor_list, int floor_num);
extern void show_floor(int Ind, olist_detail_t mode);
extern bool get_item(struct player *p, byte tester_tval, byte tester_hook);

/* obj-util.c */
extern void flavor_init(void);
extern void reset_visuals(void);
extern void object_flags(const object_type *o_ptr, bitflag flags[OF_SIZE]);
extern void object_flags_known(const object_type *o_ptr, bitflag flags[OF_SIZE], bool aware);
extern s16b index_to_label(int i);
extern bool wearable_p(const object_type *o_ptr);
extern s16b wield_slot(struct player *p, const object_type *o_ptr);
extern int slot_can_wield_item(int Ind, int slot, const object_type *o_ptr);
extern const char *mention_use(int slot);
extern const char *describe_use(struct player *p, int i);
extern int scan_floor(struct player *p, int *items, int max_size, int depth, int y, int x,
    int mode);
extern void delete_object_idx(int i);
extern void delete_object(int depth, int y, int x);
extern void compact_objects(int size);
extern void wipe_o_list(struct cave *c);
extern void preserve_artifacts(int depth);
extern s16b o_pop(void);
extern object_type *get_first_object(int depth, int y, int x);
extern object_type *get_next_object(const object_type *o_ptr);
extern s32b object_value_real(struct player *p, const object_type *o_ptr, int qty);
extern s32b object_value(struct player *p, const object_type *o_ptr, int qty);
extern bool object_similar(const object_type *o_ptr, const object_type *j_ptr,
    object_stack_t mode);
extern void object_absorb(object_type *o_ptr, object_type *j_ptr);
extern void object_wipe(object_type *o_ptr);
extern void object_copy(object_type *o_ptr, const object_type *j_ptr);
extern void object_copy_amt(object_type *dst, object_type *src, int amt);
extern s16b floor_carry(struct player *p, struct cave *c, int y, int x, object_type *j_ptr,
    bool verbose);
extern byte drop_near(struct player *p, struct cave *c, object_type *j_ptr, int chance,
    int y, int x, bool verbose);
extern void push_object(struct player *p, int y, int x);
extern void acquirement(struct player *p, int z1, int y1, int x1, int num, quark_t quark); 
extern void inven_item_charges(int Ind, int item);
extern void inven_item_describe(struct player *p, int item);
extern void inven_item_increase(struct player *p, int item, int num);
extern void save_quiver_size(struct player *p, bool report);
extern void sort_quiver(struct player *p);
extern void open_quiver_slot(int Ind, int slot);
extern void inven_item_optimize(struct player *p, int item);
extern void floor_item_increase(struct player *p, int item, int num);
extern void floor_item_optimize(int item);
extern bool inven_carry_okay(struct player *p, object_type *o_ptr);
extern bool inven_stack_okay(struct player *p, object_type *o_ptr);
extern s16b inven_takeoff(struct player *p, int item, int amt);
extern bool inven_drop(struct player *p, int item, int amt, bool bypass_inscr);
extern void combine_pack(struct player *p);
extern void reorder_pack(struct player *p);
extern int get_use_device_chance(struct player *p, const object_type *o_ptr);
extern void reduce_charges(object_type *o_ptr, int amt);
extern int number_charging(const object_type *o_ptr);
extern bool recharge_timeout(object_type *o_ptr);
extern void display_itemlist(struct player *p, bool do_cmd);
extern object_type *object_from_item_idx(struct player *p, int item, int inv_start,
    bool prompt);
extern void pack_overflow(int Ind);
extern void get_object_info(struct player *p, object_type *o_ptr, byte *attr, byte *act,
    byte *fuel, byte *fail, int *slot);
extern int get_owner_id(const object_type *o_ptr);
extern void set_artifact_info(struct player *p, const object_type *o_ptr, byte info);
extern void object_absorb_origin(object_type *o_ptr, object_type *j_ptr);
extern bool kind_is_good_other(const object_kind *kind);
extern void set_origin(object_type *i_ptr, byte origin, s16b origin_depth,
    u16b origin_xtra);
extern void process_objects(void);
extern void place_objects(int depth);
extern void object_distribute_amt(object_type *dst, object_type *src, int amt);
extern bool is_owner(struct player *p, object_type *o_ptr);
extern void object_own(struct player *p, object_type *o_ptr);
extern void preserve_artifact_aux(const object_type *o_ptr);
extern void preserve_artifact(const object_type *o_ptr);
extern bool is_sense_machine(const object_type *o_ptr);
extern bool object_activation(struct player *p, const object_type *o_ptr, bool known,
    int *effect);
extern void item_decrease(struct player *p, int item, int amount, bool describe);
extern void redraw_floor(int depth, int y, int x);
extern void clear_visibility(s16b o_idx, int depth, bool check_depth);
extern struct object *object_byid(s16b oidx);
extern void objects_init(void);
extern void objects_destroy(void);

#endif /* INCLUDED_OBJECT_H */
