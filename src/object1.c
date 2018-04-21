/* File: object1.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies. Other copyrights may also apply.
 */

/* Purpose: Object code, part 1 */

#include "angband.h"

#include <assert.h>

#if defined(MACINTOSH) || defined(MACH_O_CARBON)
#ifdef verify
#undef verify
#endif
#endif
/*
 * Reset the "visual" lists
 *
 * This involves resetting various things to their "default" state.
 *
 * If the "prefs" flag is TRUE, then we will also load the appropriate
 * "user pref file" based on the current setting of the "use_graphics"
 * flag. This is useful for switching "graphics" on/off.
 *
 * The features, objects, and monsters, should all be encoded in the
 * relevant "font.pref" and/or "graf.prf" files. XXX XXX XXX
 *
 * The "prefs" parameter is no longer meaningful. XXX XXX XXX
 */
void reset_visuals(void)
{
    int i, j;

    /* Extract some info about terrain features */
    for (i = 0; i < max_f_idx; i++)
    {
        feature_type *f_ptr = &f_info[i];

        /* Assume we will use the underlying values */
        for (j = 0; j < F_LIT_MAX; j++)
        {
            f_ptr->x_attr[j] = f_ptr->d_attr[j];
            f_ptr->x_char[j] = f_ptr->d_char[j];
        }
    }

    /* Extract default attr/char code for objects */
    for (i = 0; i < max_k_idx; i++)
    {
        object_kind *k_ptr = &k_info[i];

        /* Default attr/char */
        k_ptr->x_attr = k_ptr->d_attr;
        k_ptr->x_char = k_ptr->d_char;
    }

    /* Extract default attr/char code for monsters */
    for (i = 0; i < max_r_idx; i++)
    {
        monster_race *r_ptr = &r_info[i];

        /* Default attr/char */
        r_ptr->x_attr = r_ptr->d_attr;
        r_ptr->x_char = r_ptr->d_char;
    }

    if (use_graphics)
    {
        char buf[1024];

        /* Process "graf.prf" */
        process_pref_file("graf.prf");

        /* Access the "character" pref file */
        sprintf(buf, "graf-%s.prf", player_base);

        /* Process "graf-<playername>.prf" */
        process_pref_file(buf);
    }

    /* Normal symbols */
    else
    {
        char buf[1024];

        /* Process "font.prf" */
        process_pref_file("font.prf");

        /* Access the "character" pref file */
        sprintf(buf, "font-%s.prf", player_base);

        /* Process "font-<playername>.prf" */
        process_pref_file(buf);
    }
}


/*
 * Obtain the "flags" for an item
 */
void weapon_flags(int hand, u32b flgs[OF_ARRAY_SIZE])
{
    object_type *o_ptr = equip_obj(p_ptr->weapon_info[hand].slot);
    if (o_ptr)
    {
        int i;
        obj_flags(o_ptr, flgs);
        for (i = 0; i < OF_ARRAY_SIZE; i++)
            flgs[i] |= p_ptr->weapon_info[hand].flags[i];
    }
}

void weapon_flags_known(int hand, u32b flgs[OF_ARRAY_SIZE])
{
    object_type *o_ptr = equip_obj(p_ptr->weapon_info[hand].slot);
    if (o_ptr)
    {
        int i;
        obj_flags_known(o_ptr, flgs);
        /* TODO: Some of the following flags might not be known ... */
        for (i = 0; i < OF_ARRAY_SIZE; i++)
            flgs[i] |= p_ptr->weapon_info[hand].flags[i];
    }
}

void missile_flags(object_type *arrow, u32b flgs[OF_ARRAY_SIZE])
{
    int i;
    int slot = equip_find_first(object_is_bow);

    obj_flags(arrow, flgs);
    for (i = 0; i < OF_ARRAY_SIZE; i++)
        flgs[i] |= p_ptr->shooter_info.flags[i];

    if (slot)
    {
        object_type *bow = equip_obj(slot);
        u32b         bow_flgs[OF_ARRAY_SIZE];

        obj_flags(bow, bow_flgs);
        for (i = 0; i < OF_ARRAY_SIZE; i++)
            flgs[i] |= bow_flgs[i]; /* Mask? */
    }
}

void missile_flags_known(object_type *arrow, u32b flgs[OF_ARRAY_SIZE])
{
    int i;
    int slot = equip_find_first(object_is_bow);

    obj_flags_known(arrow, flgs);
    for (i = 0; i < OF_ARRAY_SIZE; i++)
        flgs[i] |= p_ptr->shooter_info.flags[i];

    if (slot)
    {
        object_type *bow = equip_obj(slot);
        u32b         bow_flgs[OF_ARRAY_SIZE];

        obj_flags_known(bow, bow_flgs);
        for (i = 0; i < OF_ARRAY_SIZE; i++)
            flgs[i] |= bow_flgs[i]; /* Mask? */
    }
}

void obj_flags(object_type *o_ptr, u32b flgs[OF_ARRAY_SIZE])
{
    object_kind *k_ptr = &k_info[o_ptr->k_idx];
    int i;

    /* Base object */
    for (i = 0; i < OF_ARRAY_SIZE; i++)
        flgs[i] = k_ptr->flags[i];

    /* Artifact */
    if (object_is_fixed_artifact(o_ptr))
    {
        artifact_type *a_ptr = &a_info[o_ptr->name1];

        for (i = 0; i < OF_ARRAY_SIZE; i++)
            flgs[i] = a_ptr->flags[i];
    }

    /* Ego-item */
    if (object_is_ego(o_ptr))
    {
        ego_type *e_ptr = &e_info[o_ptr->name2];
        bool      skip = FALSE;

        /* Ego lamps lose powers when they run out of fuel */
        switch (o_ptr->name2)
        {
        case EGO_LITE_IMMOLATION:
        case EGO_LITE_INFRAVISION:
        case EGO_LITE_IMMORTAL_EYE:
            if (o_ptr->sval <= SV_LITE_LANTERN && !o_ptr->xtra4)
                skip = TRUE;
            break;
        }

        if (!skip)
        {
            for (i = 0; i < OF_ARRAY_SIZE; i++)
                flgs[i] |= e_ptr->flags[i];
        }
    }

    /* Random artifact ! */
    for (i = 0; i < OF_ARRAY_SIZE; i++)
        flgs[i] |= o_ptr->flags[i];

    if (object_is_smith(o_ptr))
        weaponsmith_object_flags(o_ptr, flgs);
}

/*************************************************************
 *   Object Lore
 *************************************************************/

void obj_flags_known(object_type *o_ptr, u32b flgs[OF_ARRAY_SIZE])
{
    object_kind *k_ptr = &k_info[o_ptr->k_idx];
    int i;

    if (o_ptr->ident & IDENT_STORE)
    {
        obj_flags(o_ptr, flgs);
        return;
    }

    for (i = 0; i < OF_ARRAY_SIZE; i++)
        flgs[i] = 0;

    /* Base object: Note you still know an unidentified blade of chaos
       grants resist chaos, provided your aware of the object kind.*/
    if (k_ptr->aware)
    {
        for (i = 0; i < OF_ARRAY_SIZE; i++)
            flgs[i] = k_ptr->flags[i];
    }

    /* Unidentified objects require special work. Anything you learn about
       an object before idenitification is marked in o_ptr->know_flags. We
       must not reveal ego_type or artifact_type known_flags, since that
       would reveal the underlying ego! cf obj_identify and obj_learn_flag.*/
    if (!obj_is_identified(o_ptr))
    {
        u32b actual[OF_ARRAY_SIZE];
        obj_flags(o_ptr, actual);
        for (i = 0; i < OF_ARRAY_SIZE; i++)
            flgs[i] = actual[i] & o_ptr->known_flags[i];
        return;
    }

    /* Identified objects generally mark lore at the level of the flag itself */
    if (object_is_fixed_artifact(o_ptr))
    {
        artifact_type *a_ptr = &a_info[o_ptr->name1];

        for (i = 0; i < OF_ARRAY_SIZE; i++)
            flgs[i] |= (a_ptr->flags[i] & a_ptr->known_flags[i]);
    }
    else if (object_is_ego(o_ptr))
    {
        ego_type *e_ptr = &e_info[o_ptr->name2];
        bool      skip = FALSE;

        /* Ego lamps lose powers when they run out of fuel */
        switch (o_ptr->name2)
        {
        case EGO_LITE_IMMOLATION:
        case EGO_LITE_INFRAVISION:
        case EGO_LITE_IMMORTAL_EYE:
            if (o_ptr->sval <= SV_LITE_LANTERN && !o_ptr->xtra4)
                skip = TRUE;
            break;
        }

        if (!skip)
        {
            for (i = 0; i < OF_ARRAY_SIZE; i++)
            {
                flgs[i] |= (e_ptr->flags[i] & e_ptr->known_flags[i]);
                flgs[i] |= (o_ptr->flags[i] & e_ptr->known_flags[i]);
            }
        }
    }

    /* Random artifacts, extra resists, biffs on cursed egos, etc. */
    for (i = 0; i < OF_ARRAY_SIZE; i++)
        flgs[i] |= (o_ptr->flags[i] & o_ptr->known_flags[i]);

    /* Patch Up Activation overrides. For example, ego dragon scale
       mail might have an unlearned activation override. So, the player
       would know about the default Breathe activation, but might not
       yet have discovered that the activation is actually for Mass Genocide! */
    if ( have_flag(o_ptr->flags, OF_ACTIVATE)
      && !have_flag(o_ptr->known_flags, OF_ACTIVATE)
      && have_flag(flgs, OF_ACTIVATE) )
    {
        remove_flag(flgs, OF_ACTIVATE);
    }

    if (object_is_smith(o_ptr))
        weaponsmith_object_flags(o_ptr, flgs);
}

static void _obj_flags_purify(u32b flgs[OF_ARRAY_SIZE])
{
    remove_flag(flgs, OF_HIDE_TYPE);
    remove_flag(flgs, OF_SHOW_MODS);
    remove_flag(flgs, OF_FULL_NAME);
    remove_flag(flgs, OF_FIXED_FLAVOR);
}

static bool _obj_flags_any(u32b flgs[OF_ARRAY_SIZE])
{
    int i;
    for (i = 0; i < OF_ARRAY_SIZE; i++)
    {
        if (flgs[i])
            return TRUE;
    }
    return FALSE;
}

void obj_flags_unknown(object_type *o_ptr, u32b flgs[OF_ARRAY_SIZE])
{
    u32b actual[OF_ARRAY_SIZE];
    u32b known[OF_ARRAY_SIZE];
    int  i;

    assert(o_ptr);

    obj_flags(o_ptr, actual);
    _obj_flags_purify(actual);

    obj_flags_known(o_ptr, known);

    for (i = 0; i < OF_ARRAY_SIZE; i++)
        flgs[i] = actual[i] & (~known[i]);
}

static void _obj_identify_aux(object_type *o_ptr)
{
    int i;

    k_info[o_ptr->k_idx].aware = TRUE;

    o_ptr->feeling = FEEL_NONE;
    o_ptr->ident &= ~(IDENT_SENSE | IDENT_EMPTY | IDENT_TRIED);
    o_ptr->ident |= IDENT_KNOWN;

    /* Lore on unidentified objects is tricky, but flavorful.
       Patch up the lore flags, putting them in their correct
       hierarchical locations. */
    if (o_ptr->name1)
    {
        artifact_type *a_ptr = &a_info[o_ptr->name1];
        for (i = 0; i < OF_ARRAY_SIZE; i++)
        {
            a_ptr->known_flags[i] |= (o_ptr->known_flags[i] & a_ptr->flags[i]);
            o_ptr->known_flags[i] &= ~a_ptr->flags[i];
        }
    }
    else if (o_ptr->name2)
    {
        ego_type *e_ptr = &e_info[o_ptr->name2];
        bool      activate = have_flag(o_ptr->known_flags, OF_ACTIVATE);

        for (i = 0; i < OF_ARRAY_SIZE; i++)
        {
            e_ptr->known_flags[i] |= (o_ptr->known_flags[i] & e_ptr->flags[i]);
            e_ptr->known_flags[i] |= (o_ptr->known_flags[i] & e_ptr->xtra_flags[i]);
            o_ptr->known_flags[i] &= ~(e_ptr->flags[i] | e_ptr->xtra_flags[i]);
        }

        /* Patch up activation overrides. OF_ACTIVATE will exist in e_ptr->xtra_flags,
           and the user may have learned the activation prior to Identify */
        if (o_ptr->activation.type && activate)
            add_flag(o_ptr->known_flags, OF_ACTIVATE);

        /* Automatically know previously learned random activations */
        if (o_ptr->activation.type && effect_is_known(o_ptr->activation.type))
            add_flag(o_ptr->known_flags, OF_ACTIVATE);
    }
}

static void _obj_learn_curses(object_type *o_ptr)
{
    /* Relearn about curses ... This knowledge will be wiped on Remove Curse
       so that recursing repeats the fun (As opposed to the old system, where
       you automatically knew new curses on *identified* items). */
    o_ptr->known_curse_flags = o_ptr->curse_flags;
}

static void _obj_identify_fully_aux(object_type *o_ptr)
{
    int i;

    if (o_ptr->name1)
    {
        artifact_type *a_ptr = &a_info[o_ptr->name1];
        for (i = 0; i < OF_ARRAY_SIZE; i++)
        {
            a_ptr->known_flags[i] |= a_ptr->flags[i];
            o_ptr->known_flags[i] |= o_ptr->flags[i] & (~a_ptr->flags[i]);
        }
    }
    else if (o_ptr->name2)
    {
        ego_type *e_ptr = &e_info[o_ptr->name2];
        for (i = 0; i < OF_ARRAY_SIZE; i++)
        {
            e_ptr->known_flags[i] |= e_ptr->flags[i];
            o_ptr->known_flags[i] |= o_ptr->flags[i] & (~e_ptr->flags[i]);

            /* Mark variable ego attributes as possibilities for future.
               Note: The next time an ego of this type spawns with a known
               possible flag, it will be learned on Identify. See above. */
            if (object_is_cursed(o_ptr))
                e_ptr->known_flags[i] |= (o_ptr->flags[i] & e_ptr->xtra_flags[i]);
            else
                e_ptr->known_flags[i] |= o_ptr->flags[i];
        }
        if (object_is_device(o_ptr))
            remove_flag(e_ptr->known_flags, OF_ACTIVATE);
    }
    else /* perhaps a rand-art? */
    {
        for (i = 0; i < OF_ARRAY_SIZE; i++)
            o_ptr->known_flags[i] |= o_ptr->flags[i];
    }

    /* Learn random activations */
    if (o_ptr->activation.type && !object_is_device(o_ptr))
    {
        add_flag(o_ptr->known_flags, OF_ACTIVATE);
        effect_learn(o_ptr->activation.type);
    }

    _obj_learn_curses(o_ptr);
}

bool obj_is_identified(object_type *o_ptr)
{
    assert(o_ptr);
    return (o_ptr->ident & IDENT_KNOWN) ? TRUE : FALSE;
}

bool obj_is_identified_fully(object_type *o_ptr)
{
    u32b flgs[OF_ARRAY_SIZE];
    obj_flags_unknown(o_ptr, flgs);
    return !_obj_flags_any(flgs);
}

void obj_identify(object_type *o_ptr)
{
    assert(o_ptr);
    if (easy_id)
        obj_identify_fully(o_ptr);
    else if (!obj_is_identified(o_ptr))
        _obj_identify_aux(o_ptr);
}

void obj_identify_fully(object_type *o_ptr)
{
    assert(o_ptr);
    if (!obj_is_identified(o_ptr))
        _obj_identify_aux(o_ptr);    /* v~~~~Store items have no 'unknown' flags */
    if (!obj_is_identified_fully(o_ptr) || (o_ptr->ident & IDENT_STORE))
        _obj_identify_fully_aux(o_ptr);
    else
        _obj_learn_curses(o_ptr);
}

bool obj_learn_flag(object_type *o_ptr, int which)
{
    assert(o_ptr);
    /* Lore on unidentified objects is tricky, but flavorful */
    if (!obj_is_identified(o_ptr))
    {
        u32b flgs[OF_ARRAY_SIZE];
        obj_flags(o_ptr, flgs);
        if (have_flag(flgs, which) && !have_flag(o_ptr->known_flags, which))
        {
            add_flag(o_ptr->known_flags, which);
            return TRUE;
        }
        return FALSE;
    }
    if (o_ptr->name1)
    {
        artifact_type *a_ptr = &a_info[o_ptr->name1];
        if (have_flag(a_ptr->flags, which))
        {
            if (have_flag(a_ptr->known_flags, which)) return FALSE;
            add_flag(a_ptr->known_flags, which);
            return TRUE;
        }
        else if (have_flag(o_ptr->flags, which))
        {
            if (have_flag(o_ptr->known_flags, which)) return FALSE;
            add_flag(o_ptr->known_flags, which);
            return TRUE;
        }
    }
    else if (o_ptr->name2)
    {
        ego_type *e_ptr = &e_info[o_ptr->name2];
        if (have_flag(e_ptr->flags, which))
        {
            if (have_flag(e_ptr->known_flags, which)) return FALSE;
            add_flag(e_ptr->known_flags, which);
            return TRUE;
        }
        else if (have_flag(o_ptr->flags, which))
        {
            if (have_flag(e_ptr->xtra_flags, which))
            {
                if (have_flag(e_ptr->known_flags, which)) return FALSE;
                add_flag(e_ptr->known_flags, which);
            }
            else
            {
                if (have_flag(o_ptr->known_flags, which)) return FALSE;
                add_flag(o_ptr->known_flags, which);
            }
            return TRUE;
        }
    }
    /* Random Artifact */
    else if (have_flag(o_ptr->flags, which))
    {
        if (!have_flag(o_ptr->known_flags, which))
        {
            add_flag(o_ptr->known_flags, which);
            return TRUE;
        }
        return FALSE;
    }

    return FALSE;
}
void obj_learn_activation(object_type *o_ptr)
{
    assert(o_ptr);
    /* Lore on unidentified objects is tricky, but flavorful */
    if (!obj_is_identified(o_ptr))
    {
        if (obj_has_effect(o_ptr) && !have_flag(o_ptr->known_flags, OF_ACTIVATE))
            add_flag(o_ptr->known_flags, OF_ACTIVATE);
        return;
    }

    if (o_ptr->activation.type)
    {
        add_flag(o_ptr->known_flags, OF_ACTIVATE);
        effect_learn(o_ptr->activation.type);
    }
    else if (o_ptr->name1)
    {
        artifact_type *a_ptr = &a_info[o_ptr->name1];
        if (a_ptr->activation.type)
            add_flag(a_ptr->known_flags, OF_ACTIVATE);
        else
            add_flag(o_ptr->known_flags, OF_ACTIVATE); /* Paranoia: Activation on k_ptr, but that should be known by default! */
    }
    else if (o_ptr->name2)
    {
        ego_type *e_ptr = &e_info[o_ptr->name2];
        if (e_ptr->activation.type)
            add_flag(e_ptr->known_flags, OF_ACTIVATE);
        else
            add_flag(o_ptr->known_flags, OF_ACTIVATE); /* Paranoia: Activation on k_ptr, but that should be known by default! */
    }
}

bool obj_learn_curse(object_type *o_ptr, int flag)
{
    assert(o_ptr);
    if (o_ptr->curse_flags & flag)
    {
        if (!(o_ptr->known_curse_flags & flag))
        {
            o_ptr->known_curse_flags |= flag;
            return TRUE;
        }
    }
    return FALSE;
}

void obj_learn_slay(object_type *o_ptr, int which, cptr msg)
{
    assert(o_ptr);
    if (obj_learn_flag(o_ptr, which))
    {
        char buf[MAX_NLEN];
        object_desc(buf, o_ptr, OD_LORE);
        msg_format("<color:B>You learn that your %s %s.</color>", buf, msg);
    }
}

const int _xtra_lore_flags[] = {
    OF_LEVITATION, OF_REGEN, OF_EASY_SPELL, OF_DEC_MANA,
    OF_AURA_FIRE, OF_AURA_ELEC, OF_AURA_COLD, OF_AURA_SHARDS,
    OF_LITE, OF_DARKNESS, OF_SLOW_DIGEST,
    OF_INVALID
};

void obj_learn_equipped(object_type *o_ptr)
{
    bool learned = FALSE;
    int  i;

    for (i = 0; ; i++)
    {
        int flg = pval_flags[i];
        if (flg == OF_INVALID) break;
        if (obj_learn_flag(o_ptr, flg)) learned = TRUE;
    }

    for (i = 0; ; i++)
    {
        int flg = _xtra_lore_flags[i];
        if (flg == OF_INVALID) break;
        if (obj_learn_flag(o_ptr, flg)) learned = TRUE;
    }

    if (p_ptr->pclass == CLASS_PRIEST)
    {
        if (obj_learn_flag(o_ptr, OF_BLESSED)) learned = TRUE;
    }

    if (learned) /* TODO: Give messages for each learned flag? */
    {
        char buf[MAX_NLEN];
        object_desc(buf, o_ptr, OD_LORE);
        msg_format("<color:B>You learn more about your %s.</color>", buf);
    }
}

bool ego_has_lore(ego_type *e_ptr)
{
    return _obj_flags_any(e_ptr->known_flags);
}

bool art_has_lore(artifact_type *a_ptr)
{
    return _obj_flags_any(a_ptr->known_flags);
}

bool obj_has_lore(object_type *o_ptr)
{
    if (_obj_flags_any(o_ptr->known_flags))
        return TRUE;
    if (o_ptr->name1)
        return art_has_lore(&a_info[o_ptr->name1]);
    if (o_ptr->name2)
        return ego_has_lore(&e_info[o_ptr->name2]);
    return FALSE;
}

/*
 * Convert an inventory index into a one character label
 * Note that the label does NOT distinguish inven/equip.
 */
char index_to_label(int i)
{
    /* Indexes for "inven" are easy */
    if (i <= INVEN_PACK) return (I2A(i));

    /* Indexes for "equip" are offset */
    return (I2A(i - EQUIP_BEGIN));
}


/*
 * Convert a label into the index of an item in the "inven"
 * Return "-1" if the label does not indicate a real item
 */
s16b label_to_inven(int c)
{
    int i;

    /* Convert */
    i = (islower(c) ? A2I(c) : -1);

    /* Verify the index */
    if ((i < 0) || (i > INVEN_PACK)) return (-1);

    /* Empty slots can never be chosen */
    if (!inventory[i].k_idx) return (-1);

    /* Return the index */
    return (i);
}


/*
 * Convert a label into the index of a item in the "equip"
 * Return "-1" if the label does not indicate a real item
 */
s16b label_to_equip(int c)
{
    int i = (islower(c) ? A2I(c) : -1) + EQUIP_BEGIN;
    if (!equip_is_valid_slot(i)) return -1;
    if (!equip_obj(i)) return -1;
    return i;
}

/*
 * Return a string describing how a given item is being worn.
 * Currently, only used for items in the equipment, not inventory.
 */
cptr describe_use(int i)
{
    if (equip_is_valid_slot(i))
        return "wearing";
    return "carrying in your pack;";
}


/* Hack: Check if a spellbook is one of the realms we can use. -- TY */

bool check_book_realm(const byte book_tval, const byte book_sval)
{
    if (book_tval < TV_LIFE_BOOK) return FALSE;
    if (p_ptr->pclass == CLASS_SORCERER)
    {
        return is_magic(tval2realm(book_tval));
    }
    else if (p_ptr->pclass == CLASS_RED_MAGE)
    {
        if (is_magic(tval2realm(book_tval)))
            return ((book_tval == TV_ARCANE_BOOK) || (book_sval < 2));
    }
    return (REALM1_BOOK == book_tval || REALM2_BOOK == book_tval);
}


/*
 * Check an item against the item tester info
 */
bool item_tester_okay(object_type *o_ptr)
{
    /* Hack -- allow listing empty slots */
    if (item_tester_full) return (TRUE);

    if (!o_ptr) return FALSE;

    /* Require an item */
    if (!o_ptr->k_idx) return (FALSE);

    /* Hack -- ignore "gold" */
    if (o_ptr->tval == TV_GOLD)
    {
        /* See xtra2.c */
        extern bool show_gold_on_floor;

        if (!show_gold_on_floor) return (FALSE);
    }

    /* Check the tval */
    if (item_tester_tval)
    {
        /* Is it a spellbook? If so, we need a hack -- TY */
        if ((item_tester_tval <= TV_DEATH_BOOK) &&
            (item_tester_tval >= TV_LIFE_BOOK))
            return check_book_realm(o_ptr->tval, o_ptr->sval);
        else
            if (item_tester_tval != o_ptr->tval) return (FALSE);
    }

    /* Check the hook */
    if (item_tester_hook)
    {
        if (!(*item_tester_hook)(o_ptr)) return (FALSE);
    }

    /* Assume okay */
    return (TRUE);
}




/*
 * Choice window "shadow" of the "show_inven()" function
 */
void display_inven(void)
{
    register        int i, n, z = 0;
    object_type     *o_ptr;
    byte            attr = TERM_WHITE;
    char            tmp_val[80];
    char            o_name[MAX_NLEN];
    int             wid, hgt;

    /* Get size */
    Term_get_size(&wid, &hgt);

    /* Find the "final" slot */
    for (i = 0; i < INVEN_PACK; i++)
    {
        o_ptr = &inventory[i];

        /* Skip non-objects */
        if (!o_ptr->k_idx) continue;

        /* Track */
        z = i + 1;
    }

    /* Display the pack */
    for (i = 0; i < z; i++)
    {
        /* Examine the item */
        o_ptr = &inventory[i];

        /* Start with an empty "index" */
        tmp_val[0] = tmp_val[1] = tmp_val[2] = ' ';

        /* Is this item "acceptable"? */
        if (item_tester_okay(o_ptr))
        {
            /* Prepare an "index" */
            tmp_val[0] = index_to_label(i);

            /* Bracket the "index" --(-- */
            tmp_val[1] = ')';
        }

        /* Display the index (or blank space) */
        Term_putstr(0, i, 3, TERM_WHITE, tmp_val);

        /* Obtain an item description */
        object_desc(o_name, o_ptr, 0);

        /* Obtain the length of the description */
        n = strlen(o_name);

        /* Get a color */
        attr = tval_to_attr[o_ptr->tval % 128];

        /* Grey out charging items */
        if (o_ptr->timeout)
        {
            attr = TERM_L_DARK;
        }

        /* Display the entry itself */
        Term_putstr(3, i, n, attr, o_name);

        /* Erase the rest of the line */
        Term_erase(3+n, i, 255);

        /* Display the weight if needed */
        if (show_weights)
        {
            int wgt = o_ptr->weight * o_ptr->number;
            sprintf(tmp_val, "%3d.%1d lb", wgt / 10, wgt % 10);

            prt(tmp_val, i, wid - 9);
        }
    }

    /* Erase the rest of the window */
    for (i = z; i < hgt; i++)
    {
        /* Erase the line */
        Term_erase(0, i, 255);
    }
}



/*
 * Choice window "shadow" of the "show_equip()" function
 */
void display_equip(void)
{
    int             i, n, r = 0;
    object_type    *o_ptr;
    byte            attr = TERM_WHITE;
    char            tmp_val[80];
    char            o_name[MAX_NLEN];
    int             wid, hgt;

    /* Get size */
    Term_get_size(&wid, &hgt);

    /* Display the equipment */
    for (i = EQUIP_BEGIN; i < EQUIP_BEGIN + equip_count(); i++, r++)
    {
        o_ptr = equip_obj(i);

        /* Start with an empty "index" */
        tmp_val[0] = tmp_val[1] = tmp_val[2] = ' ';

        /* Is this item "acceptable"? */
        if (item_tester_okay(o_ptr))
        {
            /* Prepare an "index" */
            tmp_val[0] = index_to_label(i);

            /* Bracket the "index" --(-- */
            tmp_val[1] = ')';
        }

        /* Display the index (or blank space) */
        Term_putstr(0, r, 3, TERM_WHITE, tmp_val);

        if (o_ptr)
        {
            object_desc(o_name, o_ptr, 0);
            attr = tval_to_attr[o_ptr->tval % 128];
            if (o_ptr->timeout)
                attr = TERM_L_DARK;
        }
        else
            sprintf(o_name, "%s", "");

        n = strlen(o_name);
        Term_putstr(3, r, n, attr, o_name);
        Term_erase(3+n, r, 255);

        if (show_weights && o_ptr)
        {
            int wgt = o_ptr->weight * o_ptr->number;
            sprintf(tmp_val, "%3d.%1d lb", wgt / 10, wgt % 10);
            prt(tmp_val, r, wid - (show_labels ? 28 : 9));
        }
        if (show_labels && o_ptr)
        {
            Term_putstr(wid - 20, r, -1, TERM_WHITE, " <-- ");
            prt(equip_describe_slot(i), r, wid - 15);
        }
    }

    /* Erase the rest of the window */
    for (; r < hgt; r++)
        Term_erase(0, r, 255);
}


/*
 * Find the "first" inventory object with the given "tag".
 *
 * A "tag" is a numeral "n" appearing as "@n" anywhere in the
 * inscription of an object. Alphabetical characters don't work as a
 * tag in this form.
 *
 * Also, the tag "@xn" will work as well, where "n" is a any tag-char,
 * and "x" is the "current" command_cmd code.
 */
static bool get_tag(int *cp, char tag, int mode)
{
    int i, start, end;
    cptr s;

    /* Extract index from mode */
    switch (mode)
    {
    case USE_EQUIP:
        start = EQUIP_BEGIN;
        end = EQUIP_BEGIN + equip_count() - 1;
        break;

    case USE_INVEN:
        start = 0;
        end = INVEN_PACK - 1;
        break;

    default:
        return FALSE;
    }

    /**** Find a tag in the form of {@x#} (allow alphabet tag) ***/

    /* Check every inventory object */
    for (i = start; i <= end; i++)
    {
        object_type *o_ptr = &inventory[i];

        /* Skip non-objects */
        if (!o_ptr->k_idx) continue;

        /* Skip empty inscriptions */
        if (!o_ptr->inscription) continue;

        /* Skip non-choice */
        if (!item_tester_okay(o_ptr)) continue;

        /* Find a '@' */
        s = my_strchr(quark_str(o_ptr->inscription), '@');

        /* Process all tags */
        while (s)
        {
            /* Check the special tags */
            if ((s[1] == command_cmd) && (s[2] == tag))
            {
                /* Save the actual inventory ID */
                *cp = i;

                /* Success */
                return (TRUE);
            }

            /* Find another '@' */
            s = my_strchr(s + 1, '@');
        }
    }


    /**** Find a tag in the form of {@#} (allows only numerals)  ***/

    /* Don't allow {@#} with '#' being alphabet */
    if (tag < '0' || '9' < tag)
    {
        /* No such tag */
        return FALSE;
    }

    /* Check every object */
    for (i = start; i <= end; i++)
    {
        object_type *o_ptr = &inventory[i];

        /* Skip non-objects */
        if (!o_ptr->k_idx) continue;

        /* Skip empty inscriptions */
        if (!o_ptr->inscription) continue;

        /* Skip non-choice */
        if (!item_tester_okay(o_ptr)) continue;

        /* Find a '@' */
        s = my_strchr(quark_str(o_ptr->inscription), '@');

        /* Process all tags */
        while (s)
        {
            /* Check the normal tags */
            if (s[1] == tag)
            {
                /* Save the actual inventory ID */
                *cp = i;

                /* Success */
                return (TRUE);
            }

            /* Find another '@' */
            s = my_strchr(s + 1, '@');
        }
    }

    /* No such tag */
    return (FALSE);
}


/*
 * Find the "first" floor object with the given "tag".
 *
 * A "tag" is a numeral "n" appearing as "@n" anywhere in the
 * inscription of an object. Alphabetical characters don't work as a
 * tag in this form.
 *
 * Also, the tag "@xn" will work as well, where "n" is a any tag-char,
 * and "x" is the "current" command_cmd code.
 */
static bool get_tag_floor(int *cp, char tag, int floor_list[], int floor_num)
{
    int i;
    cptr s;

    /**** Find a tag in the form of {@x#} (allow alphabet tag) ***/

    /* Check every object in the grid */
    for (i = 0; i < floor_num && i < 23; i++)
    {
        object_type *o_ptr = &o_list[floor_list[i]];

        /* Skip empty inscriptions */
        if (!o_ptr->inscription) continue;

        /* Find a '@' */
        s = my_strchr(quark_str(o_ptr->inscription), '@');

        /* Process all tags */
        while (s)
        {
            /* Check the special tags */
            if ((s[1] == command_cmd) && (s[2] == tag))
            {
                /* Save the actual floor object ID */
                *cp = i;

                /* Success */
                return (TRUE);
            }

            /* Find another '@' */
            s = my_strchr(s + 1, '@');
        }
    }


    /**** Find a tag in the form of {@#} (allows only numerals)  ***/

    /* Don't allow {@#} with '#' being alphabet */
    if (tag < '0' || '9' < tag)
    {
        /* No such tag */
        return FALSE;
    }

    /* Check every object in the grid */
    for (i = 0; i < floor_num && i < 23; i++)
    {
        object_type *o_ptr = &o_list[floor_list[i]];

        /* Skip empty inscriptions */
        if (!o_ptr->inscription) continue;

        /* Find a '@' */
        s = my_strchr(quark_str(o_ptr->inscription), '@');

        /* Process all tags */
        while (s)
        {
            /* Check the normal tags */
            if (s[1] == tag)
            {
                /* Save the floor object ID */
                *cp = i;

                /* Success */
                return (TRUE);
            }

            /* Find another '@' */
            s = my_strchr(s + 1, '@');
        }
    }

    /* No such tag */
    return (FALSE);
}


/*
 * Move around label characters with correspond tags
 */
static void prepare_label_string(char *label, int mode)
{
    cptr alphabet_chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
    int  offset = (mode == USE_EQUIP) ? EQUIP_BEGIN : 0;
    int  i;

    /* Prepare normal labels */
    strcpy(label, alphabet_chars);

    /* Move each label */
    for (i = 0; i < 52; i++)
    {
        int index;
        char c = alphabet_chars[i];

        /* Find a tag with this label */
        if (get_tag(&index, c, mode))
        {
            /* Delete the overwritten label */
            if (label[i] == c) label[i] = ' ';

            /* Move the label to the place of corresponding tag */
            label[index - offset] = c;
        }
    }
}


/*
 * Move around label characters with correspond tags (floor version)
 */
static void prepare_label_string_floor(char *label, int floor_list[], int floor_num)
{
    cptr alphabet_chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
    int  i;

    /* Prepare normal labels */
    strcpy(label, alphabet_chars);

    /* Move each label */
    for (i = 0; i < 52; i++)
    {
        int index;
        char c = alphabet_chars[i];

        /* Find a tag with this label */
        if (get_tag_floor(&index, c, floor_list, floor_num))
        {
            /* Delete the overwritten label */
            if (label[i] == c) label[i] = ' ';

            /* Move the label to the place of corresponding tag */
            label[index] = c;
        }
    }
}


/*
 * Display the inventory.
 *
 * Hack -- do not display "trailing" empty slots
 */
int show_inven(int target_item, int mode)
{
    int             i, j, k, l, z = 0;
    int             cur_col, len = 0, padding, max_o_len;
    object_type     *o_ptr;
    char            o_name[MAX_NLEN];
    char            tmp_val[80];
    int             out_index[INVEN_PACK];
    byte            out_color[INVEN_PACK];
    char            out_desc[INVEN_PACK][MAX_NLEN];
    int             target_item_label = 0;
    rect_t          rect = ui_menu_rect();
    char            inven_label[52 + 1];

    /* Compute Padding */
    padding = 5; /* " a) " + trailing " " */
    if (mode & SHOW_FAIL_RATES) padding += 12;  /* " Fail: 23.2%" */
    else if (mode & SHOW_VALUE) padding += 13;  /* " Pow: 1234567" */
    else if (show_weights) padding += 9;        /* " 123.0 lb" */
    if (show_item_graph)
    {
        padding += 2;
        if (use_bigtile) padding++;
    }

    /* Find the "final" slot */
    for (i = 0; i < INVEN_PACK; i++)
    {
        o_ptr = &inventory[i];

        /* Skip non-objects */
        if (!o_ptr->k_idx) continue;

        /* Track */
        z = i + 1;
    }

    prepare_label_string(inven_label, USE_INVEN);

    /* Compute/Measure Display Strings */
    for (k = 0, i = 0; i < z; i++)
    {
        o_ptr = &inventory[i];
        if (!item_tester_okay(o_ptr)) continue;
        object_desc(o_name, o_ptr, 0);
        out_index[k] = i;
        out_color[k] = tval_to_attr[o_ptr->tval % 128];

        /* Grey out charging items */
        if (obj_has_effect(o_ptr))
        {
            bool darken = FALSE;

            switch (o_ptr->tval)
            {
            case TV_WAND: case TV_ROD: case TV_STAFF:
                if (device_sp(o_ptr) < o_ptr->activation.cost)
                    darken = TRUE;
                break;

            default:
                if (o_ptr->timeout)
                    darken = TRUE;
            }

            if (darken)
                out_color[k] = TERM_L_DARK;
        }

        strcpy(out_desc[k], o_name);

        l = strlen(out_desc[k]);
        if (l > len)
            len = l;

        k++;
    }

    /* Shorten Display Strings if Too Long */
    max_o_len = rect.cx - padding;
    if (len > max_o_len)
    {
        for (j = 0; j < k; j++)
        {
            l = strlen(out_desc[j]);
            if (l > max_o_len)
            {
                assert(max_o_len >= 3);
                /*out_desc[j][max_o_len - 3] = '.';
                out_desc[j][max_o_len - 2] = '.';
                out_desc[j][max_o_len - 1] = '.';*/
                out_desc[j][max_o_len] = '\0';
            }
        }
    }
    else
        rect.cx = padding + len;

    /* Display */
    for (j = 0; j < k; j++)
    {
        if (j >= rect.cy) break; /* out of bounds on the terminal currently = GPF! */

        /* Get the index */
        i = out_index[j];

        /* Get the item */
        o_ptr = &inventory[i];

        /* Clear the line */
        Term_erase(rect.x, rect.y + j, rect.cx);

        /* " a) " */
        if (use_menu && target_item)
        {
            if (j == (target_item-1))
            {
                strcpy(tmp_val, "> ");
                target_item_label = i;
            }
            else strcpy(tmp_val, "  ");
        }
        else if (i <= INVEN_PACK)
        {
            /* Prepare an index --(-- */
            sprintf(tmp_val, "%c)", inven_label[i]);
        }
        else
        {
            /* Prepare an index --(-- */
            sprintf(tmp_val, "%c)", index_to_label(i));
        }
        put_str(tmp_val, rect.y + j, rect.x + 1);

        cur_col = rect.x + 4;

        /* Display graphics for object, if desired */
        if (show_item_graph)
        {
            byte  a = object_attr(o_ptr);
            char c = object_char(o_ptr);

#ifdef AMIGA
            if (a & 0x80) a |= 0x40;
#endif

            Term_queue_bigchar(cur_col, rect.y + j, a, c, 0, 0);
            if (use_bigtile) cur_col++;

            cur_col += 2;
        }


        /* Display the entry itself */
        c_put_str(out_color[j], out_desc[j], rect.y + j, cur_col);

        /* Display the weight if needed */
        if (mode & SHOW_FAIL_RATES)
        {
            if (obj_is_identified_fully(o_ptr))
            {
                int fail = device_calc_fail_rate(o_ptr);
                if (fail == 1000)
                    sprintf(tmp_val, "Fail: %3d%%", fail/10);
                else
                    sprintf(tmp_val, "Fail: %2d.%d%%", fail/10, fail%10);
            }
            else
                sprintf(tmp_val, "Fail:    ?");
            put_str(tmp_val, rect.y + j, rect.x + rect.cx - 12);
        }
        else if (mode & SHOW_VALUE)
        {
            int value = obj_value_real(o_ptr);
            sprintf(tmp_val, "Pow: %7d", value);
            put_str(tmp_val, rect.y + j, rect.x + rect.cx - 13);
        }
        else if (show_weights)
        {
            int wgt = o_ptr->weight * o_ptr->number;
            (void)sprintf(tmp_val, "%3d.%1d lb", wgt / 10, wgt % 10);
            put_str(tmp_val, rect.y + j, rect.x + rect.cx - 9);
        }
    }

    /* Make a "shadow" below the list (only if needed) */
    if (j && j < rect.cy)
        Term_erase(rect.x, rect.y + j, rect.cx);

    return target_item_label;
}



/*
 * Display the equipment.
 */
int show_equip(int target_item, int mode)
{
    int             i, j, k, l;
    int             cur_col, o_len = 0, lbl_len = 0, padding, max_o_len;
    object_type     *o_ptr;
    char            tmp_val[80];
    int             out_index[EQUIP_MAX_SLOTS];
    byte            out_color[EQUIP_MAX_SLOTS];
    char            out_label[EQUIP_MAX_SLOTS][MAX_NLEN];
    char            out_desc[EQUIP_MAX_SLOTS][MAX_NLEN];
    int             target_item_label = 0;
    rect_t          rect = ui_menu_rect();
    char            equip_label[52 + 1];

    /* Compute Padding */
    padding = 5; /* " a) " + trailing " " */
    if (mode & SHOW_FAIL_RATES) padding += 12;  /* " Fail: 23.2%" */
    else if (mode & SHOW_VALUE) padding += 13;  /* " Pow: 1234567" */
    else if (show_weights) padding += 9;        /* " 123.0 lb" */
    if (show_item_graph)
    {
        padding += 2;
        if (use_bigtile) padding++;
    }

    /* Compute/Measure Display Strings */
    for (k = 0, i = EQUIP_BEGIN; i < EQUIP_BEGIN + equip_count(); i++)
    {
        o_ptr = equip_obj(i);

        if (!item_tester_okay(o_ptr)) continue; /* NULL is OK ... */
        if (equip_is_empty_two_handed_slot(i)) continue;

        if (o_ptr)
        {
            object_desc(out_desc[k], o_ptr, 0);
            if (o_ptr->timeout)
                out_color[k] = TERM_L_DARK;
            else
                out_color[k] = tval_to_attr[o_ptr->tval % 128];
        }
        else
            sprintf(out_desc[k], "%s", "");

        if (show_labels)
            sprintf(out_label[k], "%-10.10s: ", equip_describe_slot(i));
        else
            sprintf(out_label[k], "%s", "");

        out_index[k] = i;

        /* Extract the maximal length (see below) */
        l = strlen(out_desc[k]);
        if (l > o_len)
            o_len = l;

        l = strlen(out_label[k]);
        if (l > lbl_len)
            lbl_len = l;

        k++;
    }

    /* Shorten Display Strings if Too Long */
    padding += lbl_len;
    max_o_len = rect.cx - padding;
    if (o_len > max_o_len)
    {
        for (j = 0; j < k; j++)
        {
            l = strlen(out_desc[j]);
            if (l > max_o_len)
            {
                assert(max_o_len >= 3);
                /*out_desc[j][max_o_len - 3] = '.';
                out_desc[j][max_o_len - 2] = '.';
                out_desc[j][max_o_len - 1] = '.';*/
                out_desc[j][max_o_len] = '\0';
            }
        }
        o_len = max_o_len;
    }
    else
        rect.cx = padding + o_len;

    prepare_label_string(equip_label, USE_EQUIP);

    /* Display */
    for (j = 0; j < k; j++)
    {
        if (j >= rect.cy) break; /* out of bounds on the terminal currently = GPF! */

        i = out_index[j];
        o_ptr = equip_obj(i);

        Term_erase(rect.x, rect.y + j, rect.cx);

        if (use_menu && target_item)
        {
            if (j == (target_item-1))
            {
                strcpy(tmp_val, "> ");
                target_item_label = i;
            }
            else strcpy(tmp_val, "  ");
        }
        else
        {
            sprintf(tmp_val, "%c)", equip_label[i - EQUIP_BEGIN]);
        }

        /* Clear the line with the (possibly indented) index */
        put_str(tmp_val, rect.y + j, rect.x + 1);

        cur_col = rect.x + 4;

        /* Display graphics for object, if desired */
        if (show_item_graph)
        {
            if (o_ptr)
            {
                byte a = object_attr(o_ptr);
                char c = object_char(o_ptr);

                Term_queue_bigchar(cur_col, j + 1, a, c, 0, 0);
                if (use_bigtile) cur_col++;
            }
            else
                put_str(" ", rect.y + j, cur_col);

            cur_col += 2;
        }

        if (show_labels)
        {
            put_str(out_label[j], rect.y + j, cur_col);
            c_put_str(out_color[j], out_desc[j], rect.y + j, cur_col + lbl_len);
        }
        else
        {
            c_put_str(out_color[j], out_desc[j], rect.y + j, cur_col);
        }

        if (!o_ptr) continue;
        if (mode & SHOW_FAIL_RATES)
        {
            if (obj_is_identified_fully(o_ptr))
            {
                effect_t e = obj_get_effect(o_ptr);
                int      fail = effect_calc_fail_rate(&e);

                if (fail == 1000)
                    sprintf(tmp_val, "Fail: %3d%%", fail/10);
                else
                    sprintf(tmp_val, "Fail: %2d.%d%%", fail/10, fail%10);
            }
            else
                sprintf(tmp_val, "Fail:    ?");
            put_str(tmp_val, rect.y + j, rect.x + rect.cx - 12);
        }
        else if (mode & SHOW_VALUE)
        {
            int value = obj_value_real(o_ptr);
            sprintf(tmp_val, "Pow: %7d", value);
            put_str(tmp_val, rect.y + j, rect.cx - 13);
        }
        else if (show_weights)
        {
            int wgt = o_ptr->weight * o_ptr->number;
            (void)sprintf(tmp_val, "%3d.%d lb", wgt / 10, wgt % 10);
            put_str(tmp_val, rect.y + j, rect.cx - 9);
        }
    }

    /* Make a "shadow" below the list (only if needed) */
    if (j && j < rect.cy)
        Term_erase(rect.x, rect.y + j, rect.cx);

    return target_item_label;
}


/*
 * Flip "inven" and "equip" in any sub-windows
 */
void toggle_inven_equip(void)
{
    int j;

    /* Scan windows */
    for (j = 0; j < 8; j++)
    {
        /* Unused */
        if (!angband_term[j]) continue;

        /* Flip inven to equip */
        if (window_flag[j] & (PW_INVEN))
        {
            /* Flip flags */
            window_flag[j] &= ~(PW_INVEN);
            window_flag[j] |= (PW_EQUIP);

            /* Window stuff */
            p_ptr->window |= (PW_EQUIP);
        }

        /* Flip inven to equip */
        else if (window_flag[j] & (PW_EQUIP))
        {
            /* Flip flags */
            window_flag[j] &= ~(PW_EQUIP);
            window_flag[j] |= (PW_INVEN);

            /* Window stuff */
            p_ptr->window |= (PW_INVEN);
        }
    }
}



/*
 * Verify the choice of an item.
 *
 * The item can be negative to mean "item on floor".
 */
static bool verify(cptr prompt, int item)
{
    char        o_name[MAX_NLEN];
    char        out_val[MAX_NLEN+20];
    object_type *o_ptr;


    /* Inventory */
    if (item >= 0)
    {
        o_ptr = &inventory[item];
    }

    /* Floor */
    else
    {
        o_ptr = &o_list[0 - item];
    }

    /* Describe */
    object_desc(o_name, o_ptr, 0);

    /* Prompt */
    (void)sprintf(out_val, "%s %s? ", prompt, o_name);


    /* Query */
    return (get_check(out_val));
}


/*
 * Hack -- allow user to "prevent" certain choices
 *
 * The item can be negative to mean "item on floor".
 */
static bool get_item_allow(int item)
{
    cptr s;

    object_type *o_ptr;

    if (!command_cmd) return TRUE; /* command_cmd is no longer effective */

    /* Inventory */
    if (item >= 0)
    {
        o_ptr = &inventory[item];
    }

    /* Floor */
    else
    {
        o_ptr = &o_list[0 - item];
    }

    /* No inscription */
    if (!o_ptr->inscription) return (TRUE);

    /* Find a '!' */
    s = my_strchr(quark_str(o_ptr->inscription), '!');

    /* Process preventions */
    while (s)
    {
        /* Check the "restriction" */
        if ((s[1] == command_cmd) || (s[1] == '*'))
        {
            /* Verify the choice */
            if (!verify("Really try", item)) return (FALSE);

        }

        /* Find another '!' */
        s = my_strchr(s + 1, '!');
    }

    /* Allow it */
    return (TRUE);
}



/*
 * Auxiliary function for "get_item()" -- test an index
 */
static bool get_item_okay(int i)
{
    /* Illegal items */
    if ((i < 0) || (i >= INVEN_TOTAL)) return (FALSE);

    /* Verify the item */
    if (!item_tester_okay(&inventory[i])) return (FALSE);

    /* Assume okay */
    return (TRUE);
}



/*
 * Determine whether get_item() can get some item or not
 * assuming mode = (USE_EQUIP | USE_INVEN | USE_FLOOR).
 */
bool can_get_item(void)
{
    int j, floor_list[23], floor_num = 0;

    for (j = 0; j < INVEN_TOTAL; j++)
        if (item_tester_okay(&inventory[j]))
            return TRUE;

    floor_num = scan_floor(floor_list, py, px, 0x03);
    if (floor_num)
        return TRUE;

    return FALSE;
}

/*
 * Let the user select an item, save its "index"
 *
 * Return TRUE only if an acceptable item was chosen by the user.
 *
 * The selected item must satisfy the "item_tester_hook()" function,
 * if that hook is set, and the "item_tester_tval", if that value is set.
 *
 * All "item_tester" restrictions are cleared before this function returns.
 *
 * The user is allowed to choose acceptable items from the equipment,
 * inventory, or floor, respectively, if the proper flag was given,
 * and there are any acceptable items in that location.
 *
 * The equipment or inventory are displayed (even if no acceptable
 * items are in that location) if the proper flag was given.
 *
 * If there are no acceptable items available anywhere, and "str" is
 * not NULL, then it will be used as the text of a warning message
 * before the function returns.
 *
 * Note that the user must press "-" to specify the item on the floor,
 * and there is no way to "examine" the item on the floor, while the
 * use of "capital" letters will "examine" an inventory/equipment item,
 * and prompt for its use.
 *
 * If a legal item is selected from the inventory, we save it in "cp"
 * directly (0 to 35), and return TRUE.
 *
 * If a legal item is selected from the floor, we save it in "cp" as
 * a negative (-1 to -511), and return TRUE.
 *
 * If no item is available, we do nothing to "cp", and we display a
 * warning message, using "str" if available, and return FALSE.
 *
 * If no item is selected, we do nothing to "cp", and return FALSE.
 *
 * Global "p_ptr->command_new" is used when viewing the inventory or equipment
 * to allow the user to enter a command while viewing those screens, and
 * also to induce "auto-enter" of stores, and other such stuff.
 *
 * Global "p_ptr->command_see" may be set before calling this function to start
 * out in "browse" mode. It is cleared before this function returns.
 *
 * Global "p_ptr->command_wrk" is used to choose between equip/inven listings.
 * If it is TRUE then we are viewing inventory, else equipment.
 *
 * We always erase the prompt when we are done, leaving a blank line,
 * or a warning message, if appropriate, if no items are available.
 */
bool get_item(int *cp, cptr pmt, cptr str, int mode)
{
    s16b this_o_idx, next_o_idx = 0;

    char which = ' ';

    int j, k, i1, i2, e1, e2;

    bool done, item;

    bool oops = FALSE;

    bool equip = FALSE;
    bool inven = FALSE;
    bool floor = FALSE;
    bool quiver = FALSE;

    bool allow_floor = FALSE;

    bool toggle = FALSE;

    char tmp_val[160];
    char out_val[160];

    /* See cmd5.c */
    extern bool select_the_force;

    int menu_line = (use_menu ? 1 : 0);
    int max_inven = 0;
    int max_equip = 0;

#ifdef ALLOW_REPEAT

    static char prev_tag = '\0';
    char cur_tag = '\0';

#endif /* ALLOW_REPEAT */

#ifdef ALLOW_EASY_FLOOR /* TNB */

    if (easy_floor || use_menu) return get_item_floor(cp, pmt, str, mode);

#endif /* ALLOW_EASY_FLOOR -- TNB */

    /* Extract args */
    if (mode & USE_EQUIP) equip = TRUE;
    if (mode & USE_INVEN) inven = TRUE;
    if (mode & USE_FLOOR) floor = TRUE;
    if (mode & USE_QUIVER) quiver = TRUE;

#ifdef ALLOW_REPEAT

    /* Get the item index */
    if (repeat_pull(cp))
    {
        if (select_the_force && (*cp == INVEN_FORCE))
        {
            item_tester_tval = 0;
            item_tester_hook = NULL;
            command_cmd = 0; /* Hack -- command_cmd is no longer effective */
            return (TRUE);
        }
        else if (quiver && (*cp == INVEN_UNLIMITED_QUIVER))
        {
            item_tester_tval = 0;
            item_tester_hook = NULL;
            command_cmd = 0; /* Hack -- command_cmd is no longer effective */
            return (TRUE);            
        }
        else if ((mode & OPTION_ALL) && *cp == INVEN_ALL)
        {
            item_tester_tval = 0;
            item_tester_hook = NULL;
            command_cmd = 0; /* Hack -- command_cmd is no longer effective */
            return (TRUE);            
        }

        /* Floor item? */
        else if (floor && (*cp < 0))
        {
            object_type *o_ptr;

            /* Special index */
            k = 0 - (*cp);

            /* Acquire object */
            o_ptr = &o_list[k];

            /* Validate the item */
            if (item_tester_okay(o_ptr))
            {
                /* Forget restrictions */
                item_tester_tval = 0;
                item_tester_hook = NULL;
                command_cmd = 0; /* Hack -- command_cmd is no longer effective */

                /* Success */
                return TRUE;
            }
        }

        else if ( (inven && (*cp >= 0) && (*cp < INVEN_PACK)) 
               || (equip && equip_is_valid_slot(*cp)) )
        {
            if (prev_tag && command_cmd)
            {
                /* Look up the tag and validate the item */
                if (!get_tag(&k, prev_tag, equip_is_valid_slot(*cp) ? USE_EQUIP : USE_INVEN)) /* Reject */;
                else if ((k < INVEN_PACK) ? !inven : !equip) /* Reject */;
                else if (!get_item_okay(k)) /* Reject */;
                else
                {
                    /* Accept that choice */
                    (*cp) = k;

                    /* Forget restrictions */
                    item_tester_tval = 0;
                    item_tester_hook = NULL;
                    command_cmd = 0; /* Hack -- command_cmd is no longer effective */

                    /* Success */
                    return TRUE;
                }

                prev_tag = '\0'; /* prev_tag is no longer effective */
            }

            /* Verify the item */
            else if (get_item_okay(*cp))
            {
                /* Forget restrictions */
                item_tester_tval = 0;
                item_tester_hook = NULL;
                command_cmd = 0; /* Hack -- command_cmd is no longer effective */

                /* Success */
                return TRUE;
            }
        }
    }

#endif /* ALLOW_REPEAT */


    /* Paranoia XXX XXX XXX */
    msg_print(NULL);


    /* Not done */
    done = FALSE;

    /* No item selected */
    item = FALSE;


    /* Full inventory */
    i1 = 0;
    i2 = INVEN_PACK - 1;

    /* Forbid inventory */
    if (!inven) i2 = -1;
    else if (use_menu)
    {
        for (j = 0; j < INVEN_PACK; j++)
            if (item_tester_okay(&inventory[j])) max_inven++;
    }

    /* Restrict inventory indexes */
    while ((i1 <= i2) && (!get_item_okay(i1))) i1++;
    while ((i1 <= i2) && (!get_item_okay(i2))) i2--;


    /* Full equipment */
    e1 = EQUIP_BEGIN;
    e2 = EQUIP_BEGIN + equip_count() - 1;

    /* Forbid equipment */
    if (!equip) e2 = -1;
    else if (use_menu)
    {
        for (j = EQUIP_BEGIN; j < EQUIP_BEGIN + equip_count(); j++)
            if (item_tester_okay(&inventory[j])) max_equip++;
    }

    /* Restrict equipment indexes */
    while ((e1 <= e2) && (!get_item_okay(e1))) e1++;
    while ((e1 <= e2) && (!get_item_okay(e2))) e2--;

    /* Restrict floor usage */
    if (floor)
    {
        /* Scan all objects in the grid */
        for (this_o_idx = cave[py][px].o_idx; this_o_idx; this_o_idx = next_o_idx)
        {
            object_type *o_ptr;

            /* Acquire object */
            o_ptr = &o_list[this_o_idx];

            /* Acquire next object */
            next_o_idx = o_ptr->next_o_idx;

            /* Accept the item on the floor if legal */
            if (item_tester_okay(o_ptr) && (o_ptr->marked & OM_FOUND)) allow_floor = TRUE;
        }
    }

    /* Require at least one legal choice */
    if (!allow_floor && (i1 > i2) && (e1 > e2))
    {
        /* Cancel p_ptr->command_see */
        command_see = FALSE;

        /* Oops */
        oops = TRUE;

        /* Done */
        done = TRUE;

        if (select_the_force) {
            *cp = INVEN_FORCE;
            item = TRUE;
        }
        if (quiver && p_ptr->unlimited_quiver) {
            *cp = INVEN_UNLIMITED_QUIVER;
            item = TRUE;
            oops = FALSE;
        }
    }

    /* Analyze choices */
    else
    {
        /* Hack -- Start on equipment if requested */
        if (command_see && command_wrk && equip)
        {
            command_wrk = TRUE;
        }

        /* Use inventory if allowed */
        else if (inven)
        {
            command_wrk = FALSE;
        }

        /* Use equipment if allowed */
        else if (equip)
        {
            command_wrk = TRUE;
        }

        /* Use inventory for floor */
        else
        {
            command_wrk = FALSE;
        }
    }


    if ((always_show_list == TRUE) || use_menu) command_see = TRUE;

    /* Hack -- start out in "display" mode */
    if (command_see)
    {
        /* Save screen */
        screen_save();
    }


    /* Repeat until done */
    while (!done)
    {
        int get_item_label = 0;

        /* Show choices */
        int ni = 0;
        int ne = 0;

        /* Scan windows */
        for (j = 0; j < 8; j++)
        {
            /* Unused */
            if (!angband_term[j]) continue;

            /* Count windows displaying inven */
            if (window_flag[j] & (PW_INVEN)) ni++;

            /* Count windows displaying equip */
            if (window_flag[j] & (PW_EQUIP)) ne++;
        }

        /* Toggle if needed */
        if ((command_wrk && ni && !ne) ||
            (!command_wrk && !ni && ne))
        {
            /* Toggle */
            toggle_inven_equip();

            /* Track toggles */
            toggle = !toggle;
        }

        /* Update */
        p_ptr->window |= (PW_INVEN | PW_EQUIP);

        /* Redraw windows */
        window_stuff();


        /* Inventory screen */
        if (!command_wrk)
        {
            /* Redraw if needed */
            if (command_see) get_item_label = show_inven(menu_line, mode);
        }

        /* Equipment screen */
        else
        {
            /* Redraw if needed */
            if (command_see) get_item_label = show_equip(menu_line, mode);
        }

        /* Viewing inventory */
        if (!command_wrk)
        {
            /* Begin the prompt */
            sprintf(out_val, "Inven:");

            /* Some legal items */
            if ((i1 <= i2) && !use_menu)
            {
                /* Build the prompt */
                sprintf(tmp_val, " %c-%c,'(',')',",
                    index_to_label(i1), index_to_label(i2));

                /* Append */
                strcat(out_val, tmp_val);
            }

            /* Indicate ability to "view" */
            if (!(mode & OPTION_ALL) && !command_see && !use_menu) strcat(out_val, " * to see,");

            /* Append */
            if (equip) strcat(out_val, format(" %s for Equip,", use_menu ? "4 or 6" : "/"));
        }

        /* Viewing equipment */
        else
        {
            /* Begin the prompt */
            sprintf(out_val, "Equip:");

            /* Some legal items */
            if ((e1 <= e2) && !use_menu)
            {
                /* Build the prompt */
                sprintf(tmp_val, " %c-%c,'(',')',",
                    index_to_label(e1), index_to_label(e2));

                /* Append */
                strcat(out_val, tmp_val);
            }

            /* Indicate ability to "view" */
            else if (!(mode & OPTION_ALL) && !command_see) strcat(out_val, " * to see,");

            /* Append */
            if (inven) strcat(out_val, format(" %s for Inven,", use_menu ? "4 or 6" : "'/'"));
        }

        /* Indicate legality of the "floor" item */
        if (allow_floor) strcat(out_val, " - for floor,");
        if (select_the_force) strcat(out_val, " w for the Force,");
        if (quiver && p_ptr->unlimited_quiver) strcat(out_val, " z for unlimited quiver,");
        if (mode & OPTION_ALL) strcat(out_val, " * for All,");

        /* Finish the prompt */
        strcat(out_val, " ESC");

        /* Build the prompt */
        sprintf(tmp_val, "(%s) %s", out_val, pmt);

        /* Show the prompt */
        prt(tmp_val, 0, 0);

        /* Get a key */
        which = inkey();

        if (use_menu)
        {
            int max_line = (command_wrk ? max_equip : max_inven);
            switch (which)
            {
                case ESCAPE:
                case '0':
                {
                    done = TRUE;
                    break;
                }

                case '8':
                case 'k':
                case 'K':
                {
                    menu_line += (max_line - 1);
                    break;
                }

                case '2':
                case 'j':
                case 'J':
                {
                    menu_line++;
                    break;
                }

                case '4':
                case '6':
                case 'h':
                case 'H':
                case 'l':
                case 'L':
                {
                    /* Verify legality */
                    if (!inven || !equip)
                    {
                        bell();
                        break;
                    }

                    /* Hack -- Fix screen */
                    if (command_see)
                    {
                        /* Load screen */
                        screen_load();

                        /* Save screen */
                        screen_save();
                    }

                    /* Switch inven/equip */
                    command_wrk = !command_wrk;
                    max_line = (command_wrk ? max_equip : max_inven);
                    if (menu_line > max_line) menu_line = max_line;

                    /* Need to redraw */
                    break;
                }

                case 'x':
                case 'X':
                case '\r':
                case '\n':
                {
                    if (command_wrk == USE_FLOOR)
                    {
                        /* Special index */
                        (*cp) = -get_item_label;
                    }
                    else
                    {
                        /* Validate the item */
                        if (!get_item_okay(get_item_label))
                        {
                            bell();
                            break;
                        }

                        /* Allow player to "refuse" certain actions */
                        if (!get_item_allow(get_item_label))
                        {
                            done = TRUE;
                            break;
                        }

                        /* Accept that choice */
                        (*cp) = get_item_label;
                    }

                    item = TRUE;
                    done = TRUE;
                    break;
                }
                case 'w':
                {
                    if (select_the_force) {
                        *cp = INVEN_FORCE;
                        item = TRUE;
                        done = TRUE;
                    }
                    break;

                case 'z':
                    if (quiver && p_ptr->unlimited_quiver) {
                        *cp = INVEN_UNLIMITED_QUIVER;
                        item = TRUE;
                        done = TRUE;
                    }
                    break;
                }
            }
            if (menu_line > max_line) menu_line -= max_line;
        }
        else
        {
        /* Parse it */
        switch (which)
        {
            case ESCAPE:
            {
                done = TRUE;
                break;
            }

            case '*':
                if (mode & OPTION_ALL)
                {
                    (*cp) = INVEN_ALL;
                    item = TRUE;
                    done = TRUE;
                    break;
                }

            case '?':
            case ' ':
            {
                /* Hide the list */
                if (command_see)
                {
                    /* Flip flag */
                    command_see = FALSE;

                    /* Load screen */
                    screen_load();
                }

                /* Show the list */
                else
                {
                    /* Save screen */
                    screen_save();

                    /* Flip flag */
                    command_see = TRUE;
                }
                break;
            }

            case '/':
            {
                /* Verify legality */
                if (!inven || !equip)
                {
                    bell();
                    break;
                }

                /* Hack -- Fix screen */
                if (command_see)
                {
                    /* Load screen */
                    screen_load();

                    /* Save screen */
                    screen_save();
                }

                /* Switch inven/equip */
                command_wrk = !command_wrk;

                /* Need to redraw */
                break;
            }

            case '-':
            {
                /* Use floor item */
                if (allow_floor)
                {
                    /* Scan all objects in the grid */
                    for (this_o_idx = cave[py][px].o_idx; this_o_idx; this_o_idx = next_o_idx)
                    {
                        object_type *o_ptr;

                        /* Acquire object */
                        o_ptr = &o_list[this_o_idx];

                        /* Acquire next object */
                        next_o_idx = o_ptr->next_o_idx;

                        /* Validate the item */
                        if (!item_tester_okay(o_ptr)) continue;

                        /* Special index */
                        k = 0 - this_o_idx;

                        /* Verify the item (if required) */
                        if (other_query_flag && !verify("Try", k)) continue;


                        /* Allow player to "refuse" certain actions */
                        if (!get_item_allow(k)) continue;

                        /* Accept that choice */
                        (*cp) = k;
                        item = TRUE;
                        done = TRUE;
                        break;
                    }

                    /* Outer break */
                    if (done) break;
                }

                /* Oops */
                bell();
                break;
            }

            case '0':
            case '1': case '2': case '3':
            case '4': case '5': case '6':
            case '7': case '8': case '9':
            {
                /* Look up the tag */
                if (!get_tag(&k, which, command_wrk ? USE_EQUIP : USE_INVEN))
                {
                    bell();
                    break;
                }

                /* Hack -- Validate the item */
                if ((k < INVEN_PACK) ? !inven : !equip)
                {
                    bell();
                    break;
                }

                /* Validate the item */
                if (!get_item_okay(k))
                {
                    bell();
                    break;
                }

                /* Allow player to "refuse" certain actions */
                if (!get_item_allow(k))
                {
                    done = TRUE;
                    break;
                }

                /* Accept that choice */
                (*cp) = k;
                item = TRUE;
                done = TRUE;
#ifdef ALLOW_REPEAT
                cur_tag = which;
#endif /* ALLOW_REPEAT */
                break;
            }

#if 0
            case '\n':
            case '\r':
            {
                /* Choose "default" inventory item */
                if (!command_wrk)
                {
                    k = ((i1 == i2) ? i1 : -1);
                }

                /* Choose "default" equipment item */
                else
                {
                    k = ((e1 == e2) ? e1 : -1);
                }

                /* Validate the item */
                if (!get_item_okay(k))
                {
                    bell();
                    break;
                }

                /* Allow player to "refuse" certain actions */
                if (!get_item_allow(k))
                {
                    done = TRUE;
                    break;
                }

                /* Accept that choice */
                (*cp) = k;
                item = TRUE;
                done = TRUE;
                break;
            }
#endif

            case 'w':
            {
                if (select_the_force) {
                    *cp = INVEN_FORCE;
                    item = TRUE;
                    done = TRUE;
                    break;
                }

            case 'z':
                if (quiver && p_ptr->unlimited_quiver) {
                    *cp = INVEN_UNLIMITED_QUIVER;
                    item = TRUE;
                    done = TRUE;
                    break;
                }

                /* Fall through */
            }

            default:
            {
                int ver;
                bool not_found = FALSE;

                /* Look up the alphabetical tag */
                if (!get_tag(&k, which, command_wrk ? USE_EQUIP : USE_INVEN))
                {
                    not_found = TRUE;
                }

                /* Hack -- Validate the item */
                else if ((k < INVEN_PACK) ? !inven : !equip)
                {
                    not_found = TRUE;
                }

                /* Validate the item */
                else if (!get_item_okay(k))
                {
                    not_found = TRUE;
                }

                if (!not_found)
                {
                    /* Accept that choice */
                    (*cp) = k;
                    item = TRUE;
                    done = TRUE;
#ifdef ALLOW_REPEAT
                    cur_tag = which;
#endif /* ALLOW_REPEAT */
                    break;
                }

                /* Extract "query" setting */
                ver = isupper(which);
                which = tolower(which);

                /* Convert letter to inventory index */
                if (!command_wrk)
                {
                    if (which == '(') k = i1;
                    else if (which == ')') k = i2;
                    else k = label_to_inven(which);
                }

                /* Convert letter to equipment index */
                else
                {
                    if (which == '(') k = e1;
                    else if (which == ')') k = e2;
                    else k = label_to_equip(which);
                }

                /* Validate the item */
                if (!get_item_okay(k))
                {
                    bell();
                    break;
                }

                /* Verify the item */
                if (ver && !verify("Try", k))

                {
                    done = TRUE;
                    break;
                }

                /* Allow player to "refuse" certain actions */
                if (!get_item_allow(k))
                {
                    done = TRUE;
                    break;
                }

                /* Accept that choice */
                (*cp) = k;
                item = TRUE;
                done = TRUE;
                break;
            }
        }
        }
    }


    /* Fix the screen if necessary */
    if (command_see)
    {
        /* Load screen */
        screen_load();

        /* Hack -- Cancel "display" */
        command_see = FALSE;
    }


    /* Forget the item_tester_tval restriction */
    item_tester_tval = 0;

    item_tester_no_ryoute = FALSE;

    /* Forget the item_tester_hook restriction */
    item_tester_hook = NULL;


    /* Clean up  'show choices' */
    /* Toggle again if needed */
    if (toggle) toggle_inven_equip();

    /* Update */
    p_ptr->window |= (PW_INVEN | PW_EQUIP);

    /* Window stuff */
    window_stuff();


    /* Clear the prompt line */
    prt("", 0, 0);

    /* Warning if needed */
    if (oops && str) msg_print(str);

    if (item)
    {
#ifdef ALLOW_REPEAT
        repeat_push(*cp);
        if (command_cmd) prev_tag = cur_tag;
#endif /* ALLOW_REPEAT */

        command_cmd = 0; /* Hack -- command_cmd is no longer effective */
    }

    /* Result */
    return (item);
}


#ifdef ALLOW_EASY_FLOOR

/*
 * scan_floor --
 *
 * Return a list of o_list[] indexes of items at the given cave
 * location. Valid flags are:
 *
 *        mode & 0x01 -- Item tester
 *        mode & 0x02 -- Marked items only
 *        mode & 0x04 -- Stop after first
 */
int scan_floor(int *items, int y, int x, int mode)
{
    int this_o_idx, next_o_idx;

    int num = 0;

    /* Sanity */
    if (!in_bounds(y, x)) return 0;

    /* Scan all objects in the grid */
    for (this_o_idx = cave[y][x].o_idx; this_o_idx; this_o_idx = next_o_idx)
    {
        object_type *o_ptr;

        /* Acquire object */
        o_ptr = &o_list[this_o_idx];

        /* Acquire next object */
        next_o_idx = o_ptr->next_o_idx;

        /* Item tester */
        if ((mode & 0x01) && !item_tester_okay(o_ptr)) continue;

        /* Marked */
        if ((mode & 0x02) && !(o_ptr->marked & OM_FOUND)) continue;

        /* Accept this item */
        /* XXX Hack -- Enforce limit */
        if (num < 23)
            items[num] = this_o_idx;

        num++;

        /* Only one */
        if (mode & 0x04) break;
    }

    /* Result */
    return num;
}


/*
 * Display a list of the items on the floor at the given location.
 */
int show_floor(int target_item, int y, int x, int *min_width)
{
    int i, j, k, l;
    int col, len;

    object_type *o_ptr;

    char o_name[MAX_NLEN];

    char tmp_val[80];

    int out_index[23];
    byte out_color[23];
    char out_desc[23][MAX_NLEN];
    int target_item_label = 0;

    int floor_list[23], floor_num;
    int wid, hgt;
    char floor_label[52 + 1];

    bool dont_need_to_show_weights = TRUE;

    /* Get size */
    Term_get_size(&wid, &hgt);

    /* Default length */
    len = MAX((*min_width), 20);


    /* Scan for objects in the grid, using item_tester_okay() */
    floor_num = scan_floor(floor_list, y, x, 0x03);

    /* Display the floor objects */
    for (k = 0, i = 0; i < floor_num && i < 23; i++)
    {
        o_ptr = &o_list[floor_list[i]];

        /* Describe the object */
        object_desc(o_name, o_ptr, 0);

        /* Save the index */
        out_index[k] = i;

        /* Acquire inventory color */
        out_color[k] = tval_to_attr[o_ptr->tval & 0x7F];

        /* Save the object description */
        strcpy(out_desc[k], o_name);

        /* Find the predicted "line length" */
        l = strlen(out_desc[k]) + 5;

        /* Be sure to account for the weight */
        if (show_weights) l += 9;

        if (o_ptr->tval != TV_GOLD) dont_need_to_show_weights = FALSE;

        /* Maintain the maximum length */
        if (l > len) len = l;

        /* Advance to next "line" */
        k++;
    }

    if (show_weights && dont_need_to_show_weights) len -= 9;

    /* Save width */
    *min_width = len;

    /* Find the column to start in */
    col = (len > wid - 4) ? 0 : (wid - len - 1);

    prepare_label_string_floor(floor_label, floor_list, floor_num);

    /* Output each entry */
    for (j = 0; j < k; j++)
    {
        /* Get the index */
        i = floor_list[out_index[j]];

        /* Get the item */
        o_ptr = &o_list[i];

        /* Clear the line */
        prt("", j + 1, col ? col - 2 : col);

        if (use_menu && target_item)
        {
            if (j == (target_item-1))
            {
                strcpy(tmp_val, "> ");
                target_item_label = i;
            }
            else strcpy(tmp_val, "   ");
        }
        else
        {
            /* Prepare an index --(-- */
            sprintf(tmp_val, "%c)", floor_label[j]);
        }

        /* Clear the line with the (possibly indented) index */
        put_str(tmp_val, j + 1, col);

        /* Display the entry itself */
        c_put_str(out_color[j], out_desc[j], j + 1, col + 3);

        /* Display the weight if needed */
        if (show_weights && (o_ptr->tval != TV_GOLD))
        {
            int wgt = o_ptr->weight * o_ptr->number;
            sprintf(tmp_val, "%3d.%1d lb", wgt / 10, wgt % 10);

            prt(tmp_val, j + 1, wid - 9);
        }
    }

    /* Make a "shadow" below the list (only if needed) */
    if (j && (j < 23)) prt("", j + 1, col ? col - 2 : col);

    return target_item_label;
}

/*
 * This version of get_item() is called by get_item() when
 * the easy_floor is on.
 */
bool get_item_floor(int *cp, cptr pmt, cptr str, int mode)
{
    char n1 = ' ', n2 = ' ', which = ' ';

    int j, k, i1, i2, e1, e2;

    bool done, item;

    bool oops = FALSE;

    /* Extract args */
    bool equip = (mode & USE_EQUIP) ? TRUE : FALSE;
    bool inven = (mode & USE_INVEN) ? TRUE : FALSE;
    bool floor = (mode & USE_FLOOR) ? TRUE : FALSE;

    bool allow_equip = FALSE;
    bool allow_inven = FALSE;
    bool allow_floor = FALSE;

    bool toggle = FALSE;

    char tmp_val[160];
    char out_val[160];

    int floor_num, floor_list[23], floor_top = 0;
    int min_width = 0;

    extern bool select_the_force;

    int menu_line = (use_menu ? 1 : 0);
    int max_inven = 0;
    int max_equip = 0;

#ifdef ALLOW_REPEAT

    static char prev_tag = '\0';
    char cur_tag = '\0';

    /* Get the item index */
    if (repeat_pull(cp))
    {
        /* the_force */
        if (select_the_force && (*cp == INVEN_FORCE))
        {
            item_tester_tval = 0;
            item_tester_hook = NULL;
            command_cmd = 0; /* Hack -- command_cmd is no longer effective */
            return (TRUE);
        }

        /* Floor item? */
        else if (floor && (*cp < 0))
        {
            if (prev_tag && command_cmd)
            {
                /* Scan all objects in the grid */
                floor_num = scan_floor(floor_list, py, px, 0x03);

                /* Look up the tag */
                if (get_tag_floor(&k, prev_tag, floor_list, floor_num))
                {
                    /* Accept that choice */
                    (*cp) = 0 - floor_list[k];

                    /* Forget restrictions */
                    item_tester_tval = 0;
                    item_tester_hook = NULL;
                    command_cmd = 0; /* Hack -- command_cmd is no longer effective */

                    /* Success */
                    return TRUE;
                }

                prev_tag = '\0'; /* prev_tag is no longer effective */
            }

            /* Validate the item */
            else if (item_tester_okay(&o_list[0 - (*cp)]))
            {
                /* Forget restrictions */
                item_tester_tval = 0;
                item_tester_hook = NULL;
                command_cmd = 0; /* Hack -- command_cmd is no longer effective */

                /* Success */
                return TRUE;
            }
        }

        else if ( (inven && (*cp >= 0) && (*cp < INVEN_PACK)) 
               || (equip && equip_is_valid_slot(*cp)) )
        {
            if (prev_tag && command_cmd)
            {
                /* Look up the tag and validate the item */
                if (!get_tag(&k, prev_tag, equip_is_valid_slot(*cp) ? USE_EQUIP : USE_INVEN)) /* Reject */;
                else if ((k < EQUIP_BEGIN) ? !inven : !equip) /* Reject */;
                else if (!get_item_okay(k)) /* Reject */;
                else
                {
                    /* Accept that choice */
                    (*cp) = k;

                    /* Forget restrictions */
                    item_tester_tval = 0;
                    item_tester_hook = NULL;
                    command_cmd = 0; /* Hack -- command_cmd is no longer effective */

                    /* Success */
                    return TRUE;
                }

                prev_tag = '\0'; /* prev_tag is no longer effective */
            }

            /* Verify the item */
            else if (get_item_okay(*cp))
            {
                /* Forget restrictions */
                item_tester_tval = 0;
                item_tester_hook = NULL;
                command_cmd = 0; /* Hack -- command_cmd is no longer effective */

                /* Success */
                return TRUE;
            }
        }
    }

#endif /* ALLOW_REPEAT */


    /* Paranoia XXX XXX XXX */
    msg_print(NULL);


    /* Not done */
    done = FALSE;

    /* No item selected */
    item = FALSE;


    /* Full inventory */
    i1 = 0;
    i2 = INVEN_PACK - 1;

    /* Forbid inventory */
    if (!inven) i2 = -1;
    else if (use_menu)
    {
        for (j = 0; j < INVEN_PACK; j++)
            if (item_tester_okay(&inventory[j])) max_inven++;
    }

    /* Restrict inventory indexes */
    while ((i1 <= i2) && (!get_item_okay(i1))) i1++;
    while ((i1 <= i2) && (!get_item_okay(i2))) i2--;


    /* Full equipment */
    e1 = EQUIP_BEGIN;
    e2 = EQUIP_BEGIN + equip_count() - 1;

    /* Forbid equipment */
    if (!equip) e2 = -1;
    else if (use_menu)
    {
        for (j = EQUIP_BEGIN; j < EQUIP_BEGIN + equip_count(); j++)
            if (item_tester_okay(&inventory[j])) max_equip++;
    }

    /* Restrict equipment indexes */
    while ((e1 <= e2) && (!get_item_okay(e1))) e1++;
    while ((e1 <= e2) && (!get_item_okay(e2))) e2--;

    /* Count "okay" floor items */
    floor_num = 0;

    /* Restrict floor usage */
    if (floor)
    {
        /* Scan all objects in the grid */
        floor_num = scan_floor(floor_list, py, px, 0x03);
    }

    /* Accept inventory */
    if (i1 <= i2) allow_inven = TRUE;

    /* Accept equipment */
    if (e1 <= e2) allow_equip = TRUE;

    /* Accept floor */
    if (floor_num) allow_floor = TRUE;

    /* Require at least one legal choice */
    if (!allow_inven && !allow_equip && !allow_floor)
    {
        /* Cancel p_ptr->command_see */
        command_see = FALSE;

        /* Oops */
        oops = TRUE;

        /* Done */
        done = TRUE;

        if (select_the_force) {
            *cp = INVEN_FORCE;
            item = TRUE;
        }
    }

    /* Analyze choices */
    else
    {
        /* Hack -- Start on equipment if requested */
        if (command_see && (command_wrk == (USE_EQUIP))
            && allow_equip)
        {
            command_wrk = (USE_EQUIP);
        }

        /* Use inventory if allowed */
        else if (allow_inven)
        {
            command_wrk = (USE_INVEN);
        }

        /* Use equipment if allowed */
        else if (allow_equip)
        {
            command_wrk = (USE_EQUIP);
        }

        /* Use floor if allowed */
        else if (allow_floor)
        {
            command_wrk = (USE_FLOOR);
        }
    }

    if ((always_show_list == TRUE) || use_menu) command_see = TRUE;

    /* Hack -- start out in "display" mode */
    if (command_see)
    {
        /* Save screen */
        screen_save();
    }

    /* Repeat until done */
    while (!done)
    {
        int get_item_label = 0;

        /* Show choices */
        int ni = 0;
        int ne = 0;

        /* Scan windows */
        for (j = 0; j < 8; j++)
        {
            /* Unused */
            if (!angband_term[j]) continue;

            /* Count windows displaying inven */
            if (window_flag[j] & (PW_INVEN)) ni++;

            /* Count windows displaying equip */
            if (window_flag[j] & (PW_EQUIP)) ne++;
        }

        /* Toggle if needed */
        if ((command_wrk == (USE_EQUIP) && ni && !ne) ||
            (command_wrk == (USE_INVEN) && !ni && ne))
        {
            /* Toggle */
            toggle_inven_equip();

            /* Track toggles */
            toggle = !toggle;
        }

        /* Update */
        p_ptr->window |= (PW_INVEN | PW_EQUIP);

        /* Redraw windows */
        window_stuff();

        /* Inventory screen */
        if (command_wrk == (USE_INVEN))
        {
            /* Extract the legal requests */
            n1 = I2A(i1);
            n2 = I2A(i2);

            /* Redraw if needed */
            if (command_see) get_item_label = show_inven(menu_line, mode);
        }

        /* Equipment screen */
        else if (command_wrk == (USE_EQUIP))
        {
            /* Extract the legal requests */
            n1 = I2A(e1 - EQUIP_BEGIN);
            n2 = I2A(e2 - EQUIP_BEGIN);

            /* Redraw if needed */
            if (command_see) get_item_label = show_equip(menu_line, mode);
        }

        /* Floor screen */
        else if (command_wrk == (USE_FLOOR))
        {
            j = floor_top;
            k = MIN(floor_top + 23, floor_num) - 1;

            /* Extract the legal requests */
            n1 = I2A(j - floor_top);
            n2 = I2A(k - floor_top);

            /* Redraw if needed */
            if (command_see) get_item_label = show_floor(menu_line, py, px, &min_width);
        }

        /* Viewing inventory */
        if (command_wrk == (USE_INVEN))
        {
            /* Begin the prompt */
            sprintf(out_val, "Inven:");

            if (!use_menu)
            {
                /* Build the prompt */
                sprintf(tmp_val, " %c-%c,'(',')',",
                    index_to_label(i1), index_to_label(i2));

                /* Append */
                strcat(out_val, tmp_val);
            }

            /* Indicate ability to "view" */
            if (!command_see && !use_menu) strcat(out_val, " * to see,");

            /* Append */
            if (allow_equip)
            {
                if (!use_menu)
                    strcat(out_val, " / for Equip,");
                else if (allow_floor)
                    strcat(out_val, " 6 for Equip,");
                else
                    strcat(out_val, " 4 or 6 for Equip,");
            }

            /* Append */
            if (allow_floor)
            {
                if (!use_menu)
                    strcat(out_val, " - for floor,");
                else if (allow_equip)
                    strcat(out_val, " 4 for floor,");
                else
                    strcat(out_val, " 4 or 6 for floor,");
            }
        }

        /* Viewing equipment */
        else if (command_wrk == (USE_EQUIP))
        {
            /* Begin the prompt */
            sprintf(out_val, "Equip:");

            if (!use_menu)
            {
                /* Build the prompt */
                sprintf(tmp_val, " %c-%c,'(',')',",
                    index_to_label(e1), index_to_label(e2));

                /* Append */
                strcat(out_val, tmp_val);
            }

            /* Indicate ability to "view" */
            if (!command_see && !use_menu) strcat(out_val, " * to see,");

            /* Append */
            if (allow_inven)
            {
                if (!use_menu)
                    strcat(out_val, " / for Inven,");
                else if (allow_floor)
                    strcat(out_val, " 4 for Inven,");
                else
                    strcat(out_val, " 4 or 6 for Inven,");
            }

            /* Append */
            if (allow_floor)
            {
                if (!use_menu)
                    strcat(out_val, " - for floor,");
                else if (allow_inven)
                    strcat(out_val, " 6 for floor,");
                else
                    strcat(out_val, " 4 or 6 for floor,");
            }
        }

        /* Viewing floor */
        else if (command_wrk == (USE_FLOOR))
        {
            /* Begin the prompt */
            sprintf(out_val, "Floor:");

            if (!use_menu)
            {
                /* Build the prompt */
                sprintf(tmp_val, " %c-%c,'(',')',", n1, n2);

                /* Append */
                strcat(out_val, tmp_val);
            }

            /* Indicate ability to "view" */
            if (!command_see && !use_menu) strcat(out_val, " * to see,");

            if (use_menu)
            {
                if (allow_inven && allow_equip)
                {
                    strcat(out_val, " 4 for Equip, 6 for Inven,");
                }
                else if (allow_inven)
                {
                    strcat(out_val, " 4 or 6 for Inven,");
                }
                else if (allow_equip)
                {
                    strcat(out_val, " 4 or 6 for Equip,");
                }
            }
            /* Append */
            else if (allow_inven)
            {
                strcat(out_val, " / for Inven,");
            }
            else if (allow_equip)
            {
                strcat(out_val, " / for Equip,");
            }

            /* Append */
            if (command_see && !use_menu)
            {
                strcat(out_val, " Enter for scroll down,");
            }
        }

        /* Append */
        if (select_the_force) strcat(out_val, " w for the Force,");

        /* Finish the prompt */
        strcat(out_val, " ESC");

        /* Build the prompt */
        sprintf(tmp_val, "(%s) %s", out_val, pmt);

        /* Show the prompt */
        prt(tmp_val, 0, 0);

        /* Get a key */
        which = inkey();

        if (use_menu)
        {
        int max_line = 1;
        if (command_wrk == USE_INVEN) max_line = max_inven;
        else if (command_wrk == USE_EQUIP) max_line = max_equip;
        else if (command_wrk == USE_FLOOR) max_line = MIN(23, floor_num);
        switch (which)
        {
            case ESCAPE:
            case 'z':
            case 'Z':
            case '0':
            {
                done = TRUE;
                break;
            }

            case '8':
            case 'k':
            case 'K':
            {
                menu_line += (max_line - 1);
                break;
            }

            case '2':
            case 'j':
            case 'J':
            {
                menu_line++;
                break;
            }

            case '4':
            case 'h':
            case 'H':
            {
                /* Verify legality */
                if (command_wrk == (USE_INVEN))
                {
                    if (allow_floor) command_wrk = USE_FLOOR;
                    else if (allow_equip) command_wrk = USE_EQUIP;
                    else
                    {
                        bell();
                        break;
                    }
                }
                else if (command_wrk == (USE_EQUIP))
                {
                    if (allow_inven) command_wrk = USE_INVEN;
                    else if (allow_floor) command_wrk = USE_FLOOR;
                    else
                    {
                        bell();
                        break;
                    }
                }
                else if (command_wrk == (USE_FLOOR))
                {
                    if (allow_equip) command_wrk = USE_EQUIP;
                    else if (allow_inven) command_wrk = USE_INVEN;
                    else
                    {
                        bell();
                        break;
                    }
                }
                else
                {
                    bell();
                    break;
                }

                /* Hack -- Fix screen */
                if (command_see)
                {
                    /* Load screen */
                    screen_load();

                    /* Save screen */
                    screen_save();
                }

                /* Switch inven/equip */
                if (command_wrk == USE_INVEN) max_line = max_inven;
                else if (command_wrk == USE_EQUIP) max_line = max_equip;
                else if (command_wrk == USE_FLOOR) max_line = MIN(23, floor_num);
                if (menu_line > max_line) menu_line = max_line;

                /* Need to redraw */
                break;
            }

            case '6':
            case 'l':
            case 'L':
            {
                /* Verify legality */
                if (command_wrk == (USE_INVEN))
                {
                    if (allow_equip) command_wrk = USE_EQUIP;
                    else if (allow_floor) command_wrk = USE_FLOOR;
                    else
                    {
                        bell();
                        break;
                    }
                }
                else if (command_wrk == (USE_EQUIP))
                {
                    if (allow_floor) command_wrk = USE_FLOOR;
                    else if (allow_inven) command_wrk = USE_INVEN;
                    else
                    {
                        bell();
                        break;
                    }
                }
                else if (command_wrk == (USE_FLOOR))
                {
                    if (allow_inven) command_wrk = USE_INVEN;
                    else if (allow_equip) command_wrk = USE_EQUIP;
                    else
                    {
                        bell();
                        break;
                    }
                }
                else
                {
                    bell();
                    break;
                }

                /* Hack -- Fix screen */
                if (command_see)
                {
                    /* Load screen */
                    screen_load();

                    /* Save screen */
                    screen_save();
                }

                /* Switch inven/equip */
                if (command_wrk == USE_INVEN) max_line = max_inven;
                else if (command_wrk == USE_EQUIP) max_line = max_equip;
                else if (command_wrk == USE_FLOOR) max_line = MIN(23, floor_num);
                if (menu_line > max_line) menu_line = max_line;

                /* Need to redraw */
                break;
            }

            case 'x':
            case 'X':
            case '\r':
            case '\n':
            {
                if (command_wrk == USE_FLOOR)
                {
                    /* Special index */
                    (*cp) = -get_item_label;
                }
                else
                {
                    /* Validate the item */
                    if (!get_item_okay(get_item_label))
                    {
                        bell();
                        break;
                    }

                    /* Allow player to "refuse" certain actions */
                    if (!get_item_allow(get_item_label))
                    {
                        done = TRUE;
                        break;
                    }

                    /* Accept that choice */
                    (*cp) = get_item_label;
                }

                item = TRUE;
                done = TRUE;
                break;
            }
            case 'w':
            {
                if (select_the_force) {
                    *cp = INVEN_FORCE;
                    item = TRUE;
                    done = TRUE;
                    break;
                }
            }
        }
        if (menu_line > max_line) menu_line -= max_line;
        }
        else
        {
        /* Parse it */
        switch (which)
        {
            case ESCAPE:
            {
                done = TRUE;
                break;
            }

            case '*':
            case '?':
            case ' ':
            {
                /* Hide the list */
                if (command_see)
                {
                    /* Flip flag */
                    command_see = FALSE;

                    /* Load screen */
                    screen_load();
                }

                /* Show the list */
                else
                {
                    /* Save screen */
                    screen_save();

                    /* Flip flag */
                    command_see = TRUE;
                }
                break;
            }

            case '\n':
            case '\r':
            case '+':
            {
                int i, o_idx;
                cave_type *c_ptr = &cave[py][px];

                if (command_wrk != (USE_FLOOR)) break;

                /* Get the object being moved. */
                o_idx = c_ptr->o_idx;

                /* Only rotate a pile of two or more objects. */
                if (!(o_idx && o_list[o_idx].next_o_idx)) break;

                /* Remove the first object from the list. */
                excise_object_idx(o_idx);

                /* Find end of the list. */
                i = c_ptr->o_idx;
                while (o_list[i].next_o_idx)
                    i = o_list[i].next_o_idx;

                /* Add after the last object. */
                o_list[i].next_o_idx = o_idx;

                /* Re-scan floor list */ 
                floor_num = scan_floor(floor_list, py, px, 0x03);

                /* Hack -- Fix screen */
                if (command_see)
                {
                    /* Load screen */
                    screen_load();

                    /* Save screen */
                    screen_save();
                }

                break;
            }

            case '/':
            {
                if (command_wrk == (USE_INVEN))
                {
                    if (!allow_equip)
                    {
                        bell();
                        break;
                    }
                    command_wrk = (USE_EQUIP);
                }
                else if (command_wrk == (USE_EQUIP))
                {
                    if (!allow_inven)
                    {
                        bell();
                        break;
                    }
                    command_wrk = (USE_INVEN);
                }
                else if (command_wrk == (USE_FLOOR))
                {
                    if (allow_inven)
                    {
                        command_wrk = (USE_INVEN);
                    }
                    else if (allow_equip)
                    {
                        command_wrk = (USE_EQUIP);
                    }
                    else
                    {
                        bell();
                        break;
                    }
                }

                /* Hack -- Fix screen */
                if (command_see)
                {
                    /* Load screen */
                    screen_load();

                    /* Save screen */
                    screen_save();
                }

                /* Need to redraw */
                break;
            }

            case '-':
            {
                if (!allow_floor)
                {
                    bell();
                    break;
                }

                /*
                 * If we are already examining the floor, and there
                 * is only one item, we will always select it.
                 * If we aren't examining the floor and there is only
                 * one item, we will select it if floor_query_flag
                 * is FALSE.
                 */
                if (floor_num == 1)
                {
                    if ((command_wrk == (USE_FLOOR)) || (!carry_query_flag))
                    {
                        /* Special index */
                        k = 0 - floor_list[0];

                        /* Allow player to "refuse" certain actions */
                        if (!get_item_allow(k))
                        {
                            done = TRUE;
                            break;
                        }

                        /* Accept that choice */
                        (*cp) = k;
                        item = TRUE;
                        done = TRUE;

                        break;
                    }
                }

                /* Hack -- Fix screen */
                if (command_see)
                {
                    /* Load screen */
                    screen_load();

                    /* Save screen */
                    screen_save();
                }

                command_wrk = (USE_FLOOR);

                break;
            }

            case '0':
            case '1': case '2': case '3':
            case '4': case '5': case '6':
            case '7': case '8': case '9':
            {
                if (command_wrk != USE_FLOOR)
                {
                    /* Look up the tag */
                    if (!get_tag(&k, which, command_wrk))
                    {
                        bell();
                        break;
                    }

                    /* Hack -- Validate the item */
                    if (equip_is_valid_slot(k) ? !equip : !inven)
                    {
                        bell();
                        break;
                    }

                    /* Validate the item */
                    if (!get_item_okay(k))
                    {
                        bell();
                        break;
                    }
                }
                else
                {
                    /* Look up the alphabetical tag */
                    if (get_tag_floor(&k, which, floor_list, floor_num))
                    {
                        /* Special index */
                        k = 0 - floor_list[k];
                    }
                    else
                    {
                        bell();
                        break;
                    }
                }

                /* Allow player to "refuse" certain actions */
                if (!get_item_allow(k))
                {
                    done = TRUE;
                    break;
                }

                /* Accept that choice */
                (*cp) = k;
                item = TRUE;
                done = TRUE;
#ifdef ALLOW_REPEAT
                cur_tag = which;
#endif /* ALLOW_REPEAT */
                break;
            }

#if 0
            case '\n':
            case '\r':
            {
                /* Choose "default" inventory item */
                if (command_wrk == (USE_INVEN))
                {
                    k = ((i1 == i2) ? i1 : -1);
                }

                /* Choose "default" equipment item */
                else if (command_wrk == (USE_EQUIP))
                {
                    k = ((e1 == e2) ? e1 : -1);
                }

                /* Choose "default" floor item */
                else if (command_wrk == (USE_FLOOR))
                {
                    if (floor_num == 1)
                    {
                        /* Special index */
                        k = 0 - floor_list[0];

                        /* Allow player to "refuse" certain actions */
                        if (!get_item_allow(k))
                        {
                            done = TRUE;
                            break;
                        }

                        /* Accept that choice */
                        (*cp) = k;
                        item = TRUE;
                        done = TRUE;
                    }
                    break;
                }

                /* Validate the item */
                if (!get_item_okay(k))
                {
                    bell();
                    break;
                }

                /* Allow player to "refuse" certain actions */
                if (!get_item_allow(k))
                {
                    done = TRUE;
                    break;
                }

                /* Accept that choice */
                (*cp) = k;
                item = TRUE;
                done = TRUE;
                break;
            }
#endif

            case 'w':
            {
                if (select_the_force) {
                    *cp = INVEN_FORCE;
                    item = TRUE;
                    done = TRUE;
                    break;
                }

                /* Fall through */
            }

            default:
            {
                int ver;

                if (command_wrk != USE_FLOOR)
                {
                    bool not_found = FALSE;

                    /* Look up the alphabetical tag */
                    if (!get_tag(&k, which, command_wrk))
                    {
                        not_found = TRUE;
                    }

                    /* Hack -- Validate the item */
                    else if ((k < INVEN_PACK) ? !inven : !equip)
                    {
                        not_found = TRUE;
                    }

                    /* Validate the item */
                    else if (!get_item_okay(k))
                    {
                        not_found = TRUE;
                    }

                    if (!not_found)
                    {
                        /* Accept that choice */
                        (*cp) = k;
                        item = TRUE;
                        done = TRUE;
#ifdef ALLOW_REPEAT
                        cur_tag = which;
#endif /* ALLOW_REPEAT */
                        break;
                    }
                }
                else
                {
                    /* Look up the alphabetical tag */
                    if (get_tag_floor(&k, which, floor_list, floor_num))
                    {
                        /* Special index */
                        k = 0 - floor_list[k];

                        /* Accept that choice */
                        (*cp) = k;
                        item = TRUE;
                        done = TRUE;
#ifdef ALLOW_REPEAT
                        cur_tag = which;
#endif /* ALLOW_REPEAT */
                        break;
                    }
                }

                /* Extract "query" setting */
                ver = isupper(which);
                which = tolower(which);

                /* Convert letter to inventory index */
                if (command_wrk == (USE_INVEN))
                {
                    if (which == '(') k = i1;
                    else if (which == ')') k = i2;
                    else k = label_to_inven(which);
                }

                /* Convert letter to equipment index */
                else if (command_wrk == (USE_EQUIP))
                {
                    if (which == '(') k = e1;
                    else if (which == ')') k = e2;
                    else k = label_to_equip(which);
                }

                /* Convert letter to floor index */
                else if (command_wrk == USE_FLOOR)
                {
                    if (which == '(') k = 0;
                    else if (which == ')') k = floor_num - 1;
                    else k = islower(which) ? A2I(which) : -1;
                    if (k < 0 || k >= floor_num || k >= 23)
                    {
                        bell();
                        break;
                    }

                    /* Special index */
                    k = 0 - floor_list[k];
                }

                /* Validate the item */
                if ((command_wrk != USE_FLOOR) && !get_item_okay(k))
                {
                    bell();
                    break;
                }

                /* Verify the item */
                if (ver && !verify("Try", k))

                {
                    done = TRUE;
                    break;
                }

                /* Allow player to "refuse" certain actions */
                if (!get_item_allow(k))
                {
                    done = TRUE;
                    break;
                }

                /* Accept that choice */
                (*cp) = k;
                item = TRUE;
                done = TRUE;
                break;
            }
        }
        }
    }

    /* Fix the screen if necessary */
    if (command_see)
    {
        /* Load screen */
        screen_load();

        /* Hack -- Cancel "display" */
        command_see = FALSE;
    }


    /* Forget the item_tester_tval restriction */
    item_tester_tval = 0;

    /* Forget the item_tester_hook restriction */
    item_tester_hook = NULL;


    /* Clean up  'show choices' */
    /* Toggle again if needed */
    if (toggle) toggle_inven_equip();

    /* Update */
    p_ptr->window |= (PW_INVEN | PW_EQUIP);

    /* Window stuff */
    window_stuff();


    /* Clear the prompt line */
    prt("", 0, 0);

    /* Warning if needed */
    if (oops && str) msg_print(str);

    if (item)
    {
#ifdef ALLOW_REPEAT
        repeat_push(*cp);
        if (command_cmd) prev_tag = cur_tag;
#endif /* ALLOW_REPEAT */

        command_cmd = 0; /* Hack -- command_cmd is no longer effective */
    }

    /* Result */
    return (item);
}


static bool py_pickup_floor_aux(void)
{
    s16b this_o_idx;

    cptr q, s;

    int item;

    /* Restrict the choices */
    item_tester_hook = inven_carry_okay;

    /* Get an object */
    q = "Get which item? ";
    s = "You no longer have any room for the objects on the floor.";

    if (get_item(&item, q, s, (USE_FLOOR)))
    {
        this_o_idx = 0 - item;
    }
    else
    {
        return (FALSE);
    }

    /* Pick up the object */
    py_pickup_aux(this_o_idx);

    return (TRUE);
}


/*
 * Make the player carry everything in a grid
 *
 * If "pickup" is FALSE then only gold will be picked up
 *
 * This is called by py_pickup() when easy_floor is TRUE.
 */
void py_pickup_floor(bool pickup)
{
    s16b this_o_idx, next_o_idx = 0;

    char o_name[MAX_NLEN];
    object_type *o_ptr;

    int floor_num = 0, floor_o_idx = 0;

    int can_pickup = 0;

    /* Scan the pile of objects */
    for (this_o_idx = cave[py][px].o_idx; this_o_idx; this_o_idx = next_o_idx)
    {
        object_type *o_ptr;

        /* Access the object */
        o_ptr = &o_list[this_o_idx];

        /* Describe the object */
        object_desc(o_name, o_ptr, 0);

        /* Access the next object */
        next_o_idx = o_ptr->next_o_idx;

        /* Hack -- disturb */
        disturb(0, 0);

        /* Pick up gold */
        if (o_ptr->tval == TV_GOLD)
        {
            /* Message */
            msg_format("You have found %d gold pieces worth of %s.",
                (int)o_ptr->pval, o_name);

            /* Collect the gold */
            p_ptr->au += o_ptr->pval;
            stats_on_gold_find(o_ptr->pval);

            /* Redraw gold */
            p_ptr->redraw |= (PR_GOLD);

            if (prace_is_(RACE_MON_LEPRECHAUN))
                p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA);

            /* Delete the gold */
            delete_object_idx(this_o_idx);

            /* Check the next object */
            continue;
        }
        else if (o_ptr->marked & OM_NOMSG)
        {
            /* If 0 or 1 non-NOMSG items are in the pile, the NOMSG ones are
             * ignored. Otherwise, they are included in the prompt. */
            o_ptr->marked &= ~(OM_NOMSG);
            continue;
        }

        /* Count non-gold objects that can be picked up. */
        if (inven_carry_okay(o_ptr))
        {
            can_pickup++;
        }

        /* Count non-gold objects */
        floor_num++;

        /* Remember this index */
        floor_o_idx = this_o_idx;
    }

    /* There are no non-gold objects */
    if (!floor_num)
        return;

    /* Mention the number of objects */
    if (!pickup)
    {
        /* One object */
        if (floor_num == 1)
        {
            /* Access the object */
            o_ptr = &o_list[floor_o_idx];

#ifdef ALLOW_EASY_SENSE

            /* Option: Make object sensing easy */
            if (easy_sense)
            {
                /* Sense the object */
                (void) sense_object(o_ptr);
            }

#endif /* ALLOW_EASY_SENSE */

            /* Describe the object */
            object_desc(o_name, o_ptr, 0);

            /* Message */
            msg_format("You see %s.", o_name);

        }

        /* Multiple objects */
        else
        {
            /* Message */
            msg_format("You see a pile of %d items.", floor_num);

        }

        /* Done */
        return;
    }

    /* The player has no room for anything on the floor. */
    if (!can_pickup)
    {
        /* One object */
        if (floor_num == 1)
        {
            /* Access the object */
            o_ptr = &o_list[floor_o_idx];

#ifdef ALLOW_EASY_SENSE

            /* Option: Make object sensing easy */
            if (easy_sense)
            {
                /* Sense the object */
                (void) sense_object(o_ptr);
            }

#endif /* ALLOW_EASY_SENSE */

            /* Describe the object */
            object_desc(o_name, o_ptr, 0);

            /* Message */
            msg_format("You have no room for %s.", o_name);

        }

        /* Multiple objects */
        else
        {
            /* Message */
            msg_print("You have no room for any of the objects on the floor.");

        }

        /* Done */
        return;
    }

    /* One object */
    if (floor_num == 1)
    {
        /* Hack -- query every object */
        if (carry_query_flag)
        {
            char out_val[MAX_NLEN+20];

            /* Access the object */
            o_ptr = &o_list[floor_o_idx];

#ifdef ALLOW_EASY_SENSE

            /* Option: Make object sensing easy */
            if (easy_sense)
            {
                /* Sense the object */
                (void) sense_object(o_ptr);
            }

#endif /* ALLOW_EASY_SENSE */

            /* Describe the object */
            object_desc(o_name, o_ptr, 0);

            /* Build a prompt */
            (void) sprintf(out_val, "Pick up %s? ", o_name);


            /* Ask the user to confirm */
            if (!get_check(out_val))
            {
                /* Done */
                return;
            }
        }

        /* Access the object */
        o_ptr = &o_list[floor_o_idx];

#ifdef ALLOW_EASY_SENSE

        /* Option: Make object sensing easy */
        if (easy_sense)
        {
            /* Sense the object */
            (void) sense_object(o_ptr);
        }

#endif /* ALLOW_EASY_SENSE */

        /* Pick up the object */
        py_pickup_aux(floor_o_idx);
    }

    /* Allow the user to choose an object */
    else
    {
        while (can_pickup--)
        {
            if (!py_pickup_floor_aux()) break;
        }
    }
}

#endif /* ALLOW_EASY_FLOOR */
