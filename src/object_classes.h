#ifndef OBJECT_TYPE_CLASS_H
#define OBJECT_TYPE_CLASS_H

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *                    Jeff Greene, Diego Gonzalez
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 3, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */
#include <src/object.h>
#include <src/defines.h>
#include <QString>
#include <QColor>
#include <QChar>

typedef struct artifact_lore artifact_lore;
/*
 * Artifact "lore" information
 *
 */
struct artifact_lore
{
        bool was_fully_identified;      /* Preserved between diferent games */
};

/*
 * Object information, for a specific object.
 *
 * Note that a "discount" on an item is permanent and never goes away.
 *
 * Note that "object" records are "copied" and wiped frequently.
 * It is important to use the methods below instead of C commands like memset.
 *
 * Note that "object flags" must now be derived from the object kind,
 * the artifact and ego-item indexes, and the two "xtra" fields.
 *
 * Each cave grid points to one (or zero) objects via the "o_idx"
 * field (above).  Each object then points to one (or zero) objects
 * via the "next_o_idx" field, forming a singly linked list, which
 * in game terms, represents a "stack" of objects in the same grid.
 *
 * Each monster points to one (or zero) objects via the "hold_o_idx"
 * field (below).  Each object then points to one (or zero) objects
 * via the "next_o_idx" field, forming a singly linked list, which
 * in game terms, represents a pile of objects held by the monster.
 *
 * The "held_m_idx" field is used to indicate which monster, if any,
 * is holding the object.  Objects being held have "ix=0" and "iy=0".
 */
class object_type
{
    public:
    object_type();

    s16b k_idx;			/* Kind index (zero if "dead") */

    byte iy;			/* Y-position on map, or zero */
    byte ix;			/* X-position on map, or zero */

    byte tval;			/* Item type (from kind) */
    byte sval;			/* Item sub-type (from kind) */

    s16b pval;			/* Item extra-parameter */

    byte discount;		/* Discount (if any) */

    byte number;		/* Number of items */

    s16b weight;		/* Item weight */

    byte art_num;		/* Artifact type, if any */
    byte ego_num;		/* Ego-Item type, if any */

    byte xtra1;			/* Extra info type */
    u32b xtra2;			/* Extra info index */

    s16b to_h;			/* Plusses to hit */
    s16b to_d;			/* Plusses to damage */
    s16b to_a;			/* Plusses to AC */

    s16b ac;			/* Normal AC */

    byte dd, ds;		/* Damage dice/sides */

    // Current Object flags, both known and unknown
    u32b obj_flags_1;
    u32b obj_flags_2;
    u32b obj_flags_3;
    u32b obj_flags_native;

    u32b known_obj_flags_1;
    u32b known_obj_flags_2;
    u32b known_obj_flags_3;
    u32b known_obj_flags_native;

    bool use_verify[VERIFY_MAX]; //Verify use of objects

    s16b timeout;		/* Timeout Counter */

    u32b ident;			/* Special flags (was byte) */

    byte marked;		/* Object is marked */
    bool obj_in_use; 	/* Object is in use */

    QString inscription;		/* Inscription  */

    s16b next_o_idx;	/* Next object in stack (if any) */

    s16b held_m_idx;	/* Monster holding us (if any) */

        /* Object history - DRS */

    byte origin_nature;	/* ORIGIN_* */
    s16b origin_dlvl;	/* Depth */
    s16b origin_r_idx;	/* Monster race */
    QString origin_m_name;	/* monster name. Used only for player ghosts */

    s16b mimic_r_idx;	/* Object is a mimic */

    // Methods - all in object_class.cpp
    void object_wipe();
    void object_copy (object_type *j_ptr);
    void settings_erase();
    bool has_hidden_powers();
    bool is_wieldable();
    bool is_wearable();
    bool is_easy_know();
    void mark_known(bool aware);
    void mark_fully_known(bool aware);
    void mark_aware();
    void mark_tried();
    bool is_known();
    bool is_aware();
    bool is_tried();
    void has_been_seen();
    void mark_object();
    bool is_flavor_known();
    bool can_be_pseudo_ided();
    bool is_artifact();
    bool is_known_artifact();
    bool is_quest_object();
    bool is_mimic();
    bool is_ego_item();
    bool is_cursed();
    bool is_known_cursed();
    bool is_broken();
    bool was_sensed();
    bool is_spellbook();
    bool is_shovel();
    bool is_bow();
    bool is_staff();
    bool is_wand();
    bool is_rod();
    bool is_potion();
    bool is_scroll();
    bool is_parchment();
    bool is_food();
    bool is_mushroom();
    bool is_wine();
    bool is_ale();
    bool is_light();
    bool is_usable_item();
    bool is_ring();
    bool is_amulet();
    bool is_jewlery();
    bool is_chest();
    bool is_fuel();
    bool is_fuelable_lite();
    bool is_ammo();
    bool is_weapon();
    bool can_zap();
    bool could_be_zapped();
    bool can_browse();
    bool can_takeoff();
    bool has_inscription();
    bool has_charges();
    bool could_have_charges();
    void uncurse();
    byte object_color();
    void update_object_flags();
    bool use_flavor();
    QChar get_char();
    QColor get_color();
    QString get_tile_id();


    // return pseudo-id
    s16b pseudo_heavy();
    s16b pseudo_light();

};

/*
 * Information about object "kinds", including player knowledge.
 */
class object_kind
{
public:


    QString k_name;			/* Name  */
    QString k_text;			/* Text  */

    byte tval;			/* Object type */
    byte sval;			/* Object sub type */

    s16b pval;			/* Object extra info */

    s16b to_h;			/* Bonus to hit */
    s16b to_d;			/* Bonus to damage */
    s16b to_a;			/* Bonus to armor */

    s16b ac;			/* Base armor */

    byte dd, ds;		/* Damage dice/sides */

    s16b weight;		/* Weight */

    s32b cost;			/* Object "base cost" */

    u32b k_flags1;		/* Flags, set 1 */
    u32b k_flags2;		/* Flags, set 2 */
    u32b k_flags3;		/* Flags, set 3 */
    u32b k_native;		/* Flags, native */
    u32b k_store;		/* Flags, sold in a store */

    u16b effect;         /**< Effect this item produces (effects.c) */

    byte locale[4];		/* Allocation level(s) */
    byte chance[4];		/* Allocation chance(s) */

    byte k_level;			/* Level */
    byte extra;			/* Something */

    byte color_num;     //The number of any default color.  CUSTOM_COLOR for all others.
    QColor d_color;		/* Default feature color */
    QChar d_char;		/* Default object character */

    QString tile_id;

    QString autoinscribe;  //Default inscription for this object

    u16b flavor;		/* Special object flavor (or zero) */

    bool aware;			/* The player is "aware" of the item's effects */

    bool tried;			/* The player has "tried" one of the items */

    bool use_verify[VERIFY_MAX]; //Verify use of objects

    byte squelch;		/* Squelch setting for the particular item */

    bool everseen;		/* Used to see if the player savefile have ever seen this item */

    object_kind();
    void object_kind_wipe();
    bool use_flavor();
    QChar get_char();
    QColor get_color();
    QString get_tile_id();
    bool is_trap_object_kind();
};



/*
 * Information about "artifacts".
 *
 * Note that the save-file only writes "cur_num" to the savefile,
 * except for the random artifacts
 *
 * Note that "max_num" is always "1" (if that artifact "exists")
 */
class artifact_type
{
public:
    artifact_type();

    QString a_name;       /* Name */
    QString a_text;		/* Text  */

    byte tval;			/* Artifact type */
    byte sval;			/* Artifact sub type */

    s16b pval;			/* Artifact extra info */

    s16b to_h;			/* Bonus to hit */
    s16b to_d;			/* Bonus to damage */
    s16b to_a;			/* Bonus to armor */

    s16b ac;			/* Base armor */

    byte dd, ds;		/* Damage when hits */

    s16b weight;		/* Weight */

    s32b cost;			/* Artifact "cost" */

    u32b a_flags1;		/* Artifact Flags, set 1 */
    u32b a_flags2;		/* Artifact Flags, set 2 */
    u32b a_flags3;		/* Artifact Flags, set 3 */
    u32b a_native;		/* Flags, native */

    byte a_level;			/* Artifact level */
    byte a_rarity;		/* Artifact rarity */

    byte a_cur_num;		/* Number created (0 or 1) */
    byte a_max_num;		/* Unused (should be "1") */

    byte activation;	/* Activation to use */
    u16b time;			/* Activation time */
    u16b randtime;		/* Activation time dice */

    // All variables should be included in this method.
    // Shoudl only be used on randart slots!!!
    void artifact_wipe();

    bool is_special_artifact();

};


/*
 * Information about "ego-items".
 */
class ego_item_type
{
public:
    ego_item_type();
    void ego_item_wipe();

    QString e_name;        /* Name  */
    QString e_text;			/* Text  */

    s32b cost;			/* Ego-item "cost" */

    u32b e_flags1;		/* Ego-Item Flags, set 1 */
    u32b e_flags2;		/* Ego-Item Flags, set 2 */
    u32b e_flags3;		/* Ego-Item Flags, set 3 */
    u32b e_native;		/* Flags, native */

    byte level;			/* Minimum level */
    byte rarity;		/* Object rarity */
    byte rating;		/* Level rating boost */

    byte tval[EGO_TVALS_MAX]; /* Legal tval */
    byte min_sval[EGO_TVALS_MAX];	/* Minimum legal sval */
    byte max_sval[EGO_TVALS_MAX];	/* Maximum legal sval */

    byte max_to_h;		/* Maximum to-hit bonus */
    byte max_to_d;		/* Maximum to-dam bonus */
    byte max_to_a;		/* Maximum to-ac bonus */
    byte max_pval;		/* Maximum pval */

    byte xtra;			/* Extra sustain/resist/power */

    bool everseen;			/* Do not spoil squelch menus */
    bool squelch;			/* Squelch this ego-item */
};


class flavor_type
{
public:
    flavor_type();
    void flavor_wipe();

    QString text;      /* Text */

    byte tval;      /* Associated object type */
    byte sval;      /* Associated object sub-type */

    byte color_num;     //The number of any default color.  CUSTOM_COLOR for all others.
    QColor d_color;		/* Default flavor color */
    QChar d_char;    /* Default flavor character */

    QString tile_id;
};


class cmd_arg
{
public:
    QString string1;
    QString string2;

    int choice;
    int item;
    int number;
    int direction;
    int slot;
    bool verify;
    int repeats;
    int k_idx;

    void wipe();
} ;


#endif // OBJECT_TYPE_CLASS_H
