/* File: object_classes.cpp */

/*
 * Copyright (c) 2006 Jeff Greene, Diego Gonzalez
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include "src/npp.h"

ego_item_type::ego_item_type()
{
    ego_item_wipe();
}

void ego_item_type::ego_item_wipe()
{
    e_name.clear();
    e_text.clear();

    cost = e_flags1 = e_flags2 = e_flags3 = e_native = 0;
    level = rarity = rating = 0;

    for (int i = 0; i < EGO_TVALS_MAX; i++) {
        tval[i] = min_sval[i] = max_sval[i] = 0;
    }

    max_to_h = max_to_a = max_to_d = max_pval = 0;
    xtra = 0;
    everseen = squelch = 0;
}

object_type::object_type()
{
    object_wipe();
}

/*
 * Wipe all data in an object.
 * This function needs to have all variables in objct_type
 * This function does not clear the monster pointer from dungeon_type.
 * It should be not called directly on any object already mapped to
 *dungeon square or held by a monster.
 */

void object_type::object_wipe()
{
    k_idx = iy = ix = 0;
    pval = sval = tval = 0;
    discount = number = weight = 0;
    art_num = ego_num = xtra1 = dd = ds = 0;
    xtra2 = 0;
    to_h = to_d = to_a = ac = 0;
    timeout = discount = ident = 0;
    next_o_idx = held_m_idx = 0;
    origin_nature = marked = 0;
    origin_dlvl = origin_r_idx = mimic_r_idx = 0;
    obj_in_use = FALSE;
    inscription.clear();
    origin_m_name.clear();
    obj_flags_1 = obj_flags_2 = obj_flags_3 = obj_flags_native = 0;
    known_obj_flags_1 = known_obj_flags_2 = known_obj_flags_3 = known_obj_flags_native = 0;
    settings_erase();
}

// Copy object safely without using memset.
void object_type::object_copy(object_type *j_ptr)
{
    k_idx = j_ptr->k_idx;
    iy = j_ptr->iy;
    ix = j_ptr->ix;
    tval = j_ptr->tval;
    pval = j_ptr->pval;
    sval = j_ptr->sval;
    discount = j_ptr->discount;
    number = j_ptr->number;
    weight = j_ptr->weight;
    art_num = j_ptr->art_num;
    ego_num = j_ptr->ego_num;
    xtra1 = j_ptr->xtra1;
    xtra2 = j_ptr->xtra2;
    to_h = j_ptr->to_h;
    to_d = j_ptr->to_d;
    to_a = j_ptr->to_a;
    ac = j_ptr->ac;
    dd = j_ptr->dd;
    ds = j_ptr->ds;
    obj_flags_1 = j_ptr->obj_flags_1;
    obj_flags_2 = j_ptr->obj_flags_2;
    obj_flags_3 = j_ptr->obj_flags_3;
    obj_flags_native = j_ptr->obj_flags_native;
    known_obj_flags_1 = j_ptr->known_obj_flags_1;
    known_obj_flags_2 = j_ptr->known_obj_flags_2;
    known_obj_flags_3 = j_ptr->known_obj_flags_3;
    known_obj_flags_native = j_ptr->known_obj_flags_native;
    for (int i = 0; i < VERIFY_MAX; i++) use_verify[i] = j_ptr->use_verify[i];
    timeout = j_ptr->timeout;
    discount = j_ptr->discount;
    ident = j_ptr->ident;
    obj_in_use = j_ptr->obj_in_use;
    inscription = j_ptr->inscription;
    marked = j_ptr->marked;
    next_o_idx = j_ptr->next_o_idx;
    held_m_idx = j_ptr->held_m_idx;
    origin_nature = j_ptr->origin_nature;
    origin_dlvl = j_ptr->origin_dlvl;
    origin_r_idx = j_ptr->origin_r_idx;
    origin_m_name = j_ptr->origin_m_name;
    mimic_r_idx = j_ptr->mimic_r_idx;
}

void object_type::settings_erase()
{
    for (int i = 0; i < VERIFY_MAX; i++) use_verify[i] = FALSE;
}

/*
 * Returns TRUE if an object has some ego-powers that should be ignored if
 * the game does not want *full* knowledge of it.
*/
bool object_type::has_hidden_powers()
{
    /* *identified* items are never hidden*/
    if (ident & (IDENT_MENTAL)) return (FALSE);

    /* Hack - Ignore chests */
    if (tval == TV_CHEST) return (FALSE);

    /* Analyze xtra1 */
    switch (xtra1)
    {
        case OBJECT_XTRA_STAT_SUSTAIN:
        case OBJECT_XTRA_TYPE_HIGH_RESIST:
        case OBJECT_XTRA_TYPE_POWER:
        case OBJECT_XTRA_TYPE_IMMUNITY:
        case OBJECT_XTRA_TYPE_STAT_ADD:
        {
            return (TRUE);
        }
    }

    return (FALSE);
}

/*
 * Hack -- determine if an item is "wearable" (not ammo)
 */
bool object_type::is_wieldable()
{
    /* Valid "tval" codes */
    switch (tval)
    {
        case TV_BOW:
        case TV_DIGGING:
        case TV_HAFTED:
        case TV_POLEARM:
        case TV_SWORD:
        case TV_BOOTS:
        case TV_GLOVES:
        case TV_HELM:
        case TV_CROWN:
        case TV_SHIELD:
        case TV_CLOAK:
        case TV_SOFT_ARMOR:
        case TV_HARD_ARMOR:
        case TV_DRAG_ARMOR:
        case TV_DRAG_SHIELD:
        case TV_LIGHT:
        case TV_AMULET:
        case TV_RING: return (TRUE);
    }

    /* Nope */
    return (FALSE);
}

/*
 * Hack -- determine if an item is "wearable" (or a missile)
 */
bool object_type::is_wearable()
{
    /* Valid "tval" codes */
    switch (tval)
    {
        case TV_SHOT:
        case TV_ARROW:
        case TV_BOLT:
        case TV_BOW:
        case TV_DIGGING:
        case TV_HAFTED:
        case TV_POLEARM:
        case TV_SWORD:
        case TV_BOOTS:
        case TV_GLOVES:
        case TV_HELM:
        case TV_CROWN:
        case TV_SHIELD:
        case TV_CLOAK:
        case TV_SOFT_ARMOR:
        case TV_HARD_ARMOR:
        case TV_DRAG_ARMOR:
        case TV_DRAG_SHIELD:
        case TV_LIGHT:
        case TV_AMULET:
        case TV_RING: return (TRUE);
    }

    /* Nope */
    return (FALSE);
}

/*
 * returns whether an object counts as "known" due to EASY_KNOW status
 */
bool object_type::is_easy_know()
{
    object_kind *k_ptr = &k_info[k_idx];

    if (k_ptr->aware && (k_ptr->k_flags3 & TR3_EASY_KNOW)) return (TRUE);

    return (FALSE);
}
/*
 *
 * These attributes include tohit, todam, toac, cost, and pval (charges).
 *
 * Note that "knowing" an object gives you everything that an "awareness"
 * gives you, and much more.  In fact, the player is always "aware" of any
 * item which he "knows", except items in stores.
 *
 * But having full knowledge of, say, one "wand of wonder", does not, by
 * itself, give you knowledge, or even awareness, of other "wands of wonder".
 * It happens that most "identify" routines (including "buying from a shop")
 * will make the player "aware" of the object as well as "know" it.
 *
 * This routine also removes any inscriptions generated by "feelings".
 */
void object_type::mark_known(bool aware)
{
    if (aware) mark_aware();

    /* Remove special inscription, if any */
    if (discount >= INSCRIP_NULL) discount = 0;

    /* The object is not "sensed" */
    ident &= ~(IDENT_SENSE);

    /* Clear the "Empty" info */
    ident &= ~(IDENT_EMPTY);

    /* Now we know about the item */
    ident |= (IDENT_KNOWN);

    update_object_flags();
}

// *Identify* object
void object_type::mark_fully_known(bool aware)
{
    ident |= (IDENT_MENTAL);

    mark_known(aware);
}

/*
 * The player is now aware of the effects of the given object.
 */
void object_type::mark_aware()
{
    bool flag = k_info[k_idx].aware;

    /* Fully aware of the effects */
    k_info[k_idx].aware = TRUE;
    k_info->everseen = TRUE;

    /* MEGA-HACK - scrolls can change the graphics when becoming aware */
    if (tval == TV_SCROLL  && (use_graphics != GRAPHICS_NONE))
    {
        /* Redraw map */
        p_ptr->redraw |= (PR_MAP);
    }

    // No updates needed
    if (flag || (k_info[k_idx].squelch == SQUELCH_ALWAYS)) return;
    if (!character_dungeon) return;

    // Update the map
    for (int x = 0; x < p_ptr->cur_map_wid; x++)
    {
        for (int y = 0; y < p_ptr->cur_map_hgt; y++)
        {
            rearrange_stack(y, x);
        }
    }
}

/*
 * Something has been "sampled"
 */
void object_type::mark_tried()
{
    k_info[k_idx].tried = TRUE;
}


/*
 * returns whether an object should be treated as fully known (e.g. ID'd)
 */
bool object_type::is_known()
{
    if (is_easy_know())
    {
        object_kind *k_ptr = &k_info[k_idx];
        if (k_ptr->aware) return (TRUE);
    }
    if (ident & (IDENT_KNOWN)) return (TRUE);
    if (ident & (IDENT_STORE)) return (TRUE);
    return (FALSE);
}

/*
 * returns whether an object should be treated as fully known (e.g. ID'd)
 */
bool object_type::is_known_fully()
{
    if (ident & (IDENT_MENTAL)) return (TRUE);
    return (FALSE);
}

/*
 * returns whether an object is aware (e.g. ID'd)
 */
bool object_type::is_aware()
{
    if (k_info[k_idx].aware) return (TRUE);
    return (FALSE);
}

/*
 * returns whether an object has been tried
 */
bool object_type::is_tried()
{
    if (k_info[k_idx].tried) return (TRUE);
    return (FALSE);
}

/*
 * Record when an object type has been seen
 */
void object_type::has_been_seen()
{
    k_info[k_idx].everseen = TRUE;
    if (ego_num) e_info[ego_num].everseen = TRUE;
}

// Mark it and remember it has been seen.
void object_type::mark_object()
{
    marked = TRUE;
    has_been_seen();
    if (tval != TV_GOLD) p_ptr->redraw |= (PR_WIN_OBJLIST);
}

/*
 * returns whether the player is aware of the object's flavour
 */
bool object_type::is_flavor_known()
{
    return k_info[k_idx].aware;
}

/*
 * Returns TRUE if this object can be pseudo-ided.
 */
bool object_type::can_be_pseudo_ided()
{
    /* Valid "tval" codes */
    switch (tval)
    {
        case TV_SHOT:
        case TV_ARROW:
        case TV_BOLT:
        case TV_BOW:
        case TV_DIGGING:
        case TV_HAFTED:
        case TV_POLEARM:
        case TV_SWORD:
        case TV_BOOTS:
        case TV_GLOVES:
        case TV_HELM:
        case TV_CROWN:
        case TV_SHIELD:
        case TV_CLOAK:
        case TV_SOFT_ARMOR:
        case TV_HARD_ARMOR:
        case TV_DRAG_ARMOR:
        case TV_DRAG_SHIELD:
        {
            return (TRUE);
        }
        case TV_LIGHT:
        {
            if (game_mode == GAME_NPPMORIA) return (FALSE);

            if (sval == SV_LIGHT_LANTERN)
            return (TRUE);
            break;
        }
    }
    return (FALSE);
}

/*
 * Artifacts use the "art_num" field
 */
bool object_type::is_artifact()
{
    if (art_num) return (TRUE);
    return (FALSE);
}


/*
 * Returns whether the object is known to be an artifact
 */
bool object_type::is_known_artifact()
{
    if (!is_artifact()) return (FALSE);
    if (is_known()) return (TRUE);
    if (discount == INSCRIP_INDESTRUCTIBLE) return (TRUE);
    if (discount == INSCRIP_TERRIBLE) return (TRUE);
    if (discount == INSCRIP_SPECIAL) return (TRUE);
    return (FALSE);
}

/*
 * An object designed specifically for a quest
 */
bool object_type::is_quest_object()
{
    if(ident & (IDENT_QUEST)) return (TRUE);
    return (FALSE);
}

/*
 * Is an object a mimic?
 */
bool object_type::is_mimic()
{
    if (mimic_r_idx) return (TRUE);
    return (FALSE);
}

/*
 * Artifacts use the "ego_num" field
 */
bool object_type::is_ego_item()
{
    if (ego_num) return (TRUE);
    return (FALSE);
}

/*
 * Returns TRUE if this object is cursed.
 */
bool object_type::is_cursed()
{
    if (ident & (IDENT_CURSED)) return (TRUE);
    return (FALSE);
}

bool object_type::is_known_cursed()
{
    if (!is_cursed()) return (FALSE);
    if (is_known()) return (TRUE);
    if (!was_sensed()) return (FALSE);
    return (TRUE);
}

/*
 * Returns TRUE if this object is broken.
 */
bool object_type::is_broken()
{
    if (ident & (IDENT_BROKEN)) return (TRUE);
    return (FALSE);
}

/**
 * \returns whether the object has been sensed with pseudo-ID
 */
bool object_type::was_sensed()
{
    if (ident & (IDENT_SENSE)) return (TRUE);
    return (FALSE);
}

/*
 * Is this a spellbook?
 */
bool object_type::is_spellbook()
{
    if (tval == TV_MAGIC_BOOK) return (TRUE);
    if (tval == TV_PRAYER_BOOK) return (TRUE);
    if (tval == TV_DRUID_BOOK) return (TRUE);

    return (FALSE);
}

/* Basic tval testers */
bool object_type::is_shovel()       { return tval == TV_DIGGING;}
bool object_type::is_bow()          { return tval == TV_BOW; }
bool object_type::is_staff()        { return tval == TV_STAFF; }
bool object_type::is_wand()         { return tval == TV_WAND; }
bool object_type::is_rod()          { return tval == TV_ROD; }
bool object_type::is_potion()       { return tval == TV_POTION; }
bool object_type::is_scroll()       { return tval == TV_SCROLL; }
bool object_type::is_parchment()    { return tval == TV_PARCHMENT; }
bool object_type::is_food()         { return tval == TV_FOOD; }
bool object_type::is_light()        { return tval == TV_LIGHT; }

//  Return an object that an be used
bool object_type::is_mushroom()
{
    if (!is_food())     return (FALSE);
    if (sval <= SV_FOOD_MAJOR_CURES) return (TRUE);
    return (FALSE);
}

//  Return an object that an be used
bool object_type::is_wine()
{
    if (!is_food())     return (FALSE);
    if (sval == SV_FOOD_FINE_WINE) return (TRUE);
    return (FALSE);
}

//  Return an object that an be used
bool object_type::is_ale()
{
    if (!is_food())     return (FALSE);
    if (sval == SV_FOOD_FINE_ALE) return (TRUE);
    return (FALSE);
}

//  Return an object that an be used
bool object_type::is_usable_item()
{
    if (is_staff())     return (TRUE);
    if (is_rod())       return (TRUE);
    if (is_wand())      return (TRUE);
    if (is_potion())    return (TRUE);
    if (is_scroll())    return (TRUE);
    if (is_food())      return (TRUE);
    return (FALSE);
}


bool object_type::is_ring()         { return tval == TV_RING; }
bool object_type::is_amulet()       { return tval == TV_AMULET; }
bool object_type::is_jewlery()
{
    if (is_amulet())    return (TRUE);
    if (is_ring())      return (TRUE);
    return (FALSE);
}
bool object_type::is_chest()        { return tval == TV_CHEST; }

/*
 * Returns whether the object can be used as fuel
 */
bool object_type::is_fuel()
{
    if (tval == TV_FLASK)   return (TRUE);
    if (tval != TV_LIGHT)   return (FALSE);
    if (sval == SV_LIGHT_TORCH) return (TRUE);
    return (FALSE);
}

/*
 * Returns whether the object can be filled with oil
 */
bool object_type::is_fuelable_lite()
{
    if (tval != TV_LIGHT)   return (FALSE);
    if (is_artifact())      return (FALSE);
    return (TRUE);
}

/**
 * \returns whether the object is ammunition
 */
bool object_type::is_ammo()
{
    if (tval == TV_BOLT)   return (TRUE);
    if (tval == TV_ARROW)   return (TRUE);
    if (tval == TV_SHOT)   return (TRUE);
    return (FALSE);
}

/*
 * Determine whether an object is a weapon
 */
bool object_type::is_weapon()
{
    switch (tval)
    {
        case TV_HAFTED:
        case TV_POLEARM:
        case TV_SWORD:
        case TV_DIGGING:
            return TRUE;
        default:
            return FALSE;
    }
}


/*
 * Determine if an object has charges
 */
bool object_type::can_zap()
{
    object_kind *k_ptr = &k_info[k_idx];

    if (!is_rod()) return FALSE;
    if (!timeout) return (TRUE);
    if (number == 1) return (FALSE);

    if (timeout > (pval - k_ptr->pval)) return (FALSE);

    return (TRUE);
}

/*
 * Determine if an object has charges
 */
bool object_type::could_be_zapped()
{
    if (!can_zap() && is_known()) return (FALSE);
    return (TRUE);
}

/*
 * Determine if an object can be browsed (spellbook)
 */
bool object_type::can_browse()
{
    if (tval != cp_ptr->spell_book) return (FALSE);
    return TRUE;
}



/*
 * Can only take off non-cursed items
 */
bool object_type::can_takeoff()
{
    return (!is_cursed());
}


/*
 * Can has inscrip pls
 */
bool object_type::has_inscription()
{
    return (!inscription.isEmpty());
}

/*
 * Determine if an object has charges
 */
bool object_type::has_charges()
{
    if (!is_wand() && !is_staff()) return (FALSE);

    if (pval <= 0) return (FALSE);

    return (TRUE);
}

bool object_type::could_have_charges()
{
    if (!has_charges()) return (FALSE);
    if (is_known()) return (TRUE);
    if (ident & (IDENT_EMPTY)) return (FALSE);
    return (TRUE);
}

/*
 * Return a "feeling" (or NULL) about an item.  Method 1 (Heavy).
 */
s16b object_type::pseudo_heavy()
{
    /* Artifacts */
    if (is_artifact())
    {
        /* Cursed/Broken */
        if (is_cursed() || is_broken()) return (INSCRIP_TERRIBLE);

        /* Normal */
        return (INSCRIP_SPECIAL);
    }

    /* Ego-Items */
    if (is_ego_item())
    {
        /* Cursed/Broken */
        if (is_cursed() || is_broken()) return (INSCRIP_WORTHLESS);

        /* Normal */
        return (INSCRIP_EXCELLENT);
    }

    /* Cursed items */
    if (is_cursed()) return (INSCRIP_CURSED);

    /* Broken items */
    if (is_broken()) return (INSCRIP_BROKEN);

    /* Good "armor" bonus */
    if (to_a > 0) return (INSCRIP_GOOD_STRONG);

    /* Good "weapon" bonus */
    if (to_h + to_d > 0) return (INSCRIP_GOOD_STRONG);

    /* Default to "average" */
    return (INSCRIP_AVERAGE);
}


/*
 * Return a "feeling" (or NULL) about an item.  Method 2 (Light).
 */
s16b object_type::pseudo_light()
{
    /* Cursed items (all of them) */
    if (is_cursed()) return (INSCRIP_CURSED);

    /* Broken items (all of them) */
    if (is_broken()) return (INSCRIP_BROKEN);

    /* Artifacts -- except cursed/broken ones */
    if (is_artifact()) return (INSCRIP_GOOD_WEAK);

    /* Ego-Items -- except cursed/broken ones */
    if (is_ego_item()) return (INSCRIP_GOOD_WEAK);

    /* Good armor bonus */
    if (to_a > 0) return (INSCRIP_GOOD_WEAK);

    /* Good weapon bonuses */
    if (to_h + to_d > 0) return (INSCRIP_GOOD_WEAK);

    /* Default to "average" */
    return (INSCRIP_AVERAGE);
}

/*
 * Hack -- Removes curse from an object.
 */
void object_type::uncurse()
{
    /* Uncurse it */
    ident &= ~(IDENT_CURSED);

    /* Remove special inscription, if any */
    if (discount >= INSCRIP_NULL) discount = 0;

    /* Take note if allowed */
    if (discount == 0) discount = INSCRIP_UNCURSED;

    /* The object has been "sensed" */
    ident |= (IDENT_SENSE);
}

/*
 * Hack -- Removes curse from an object.
 */
byte object_type::object_color()
{
    return (k_info[k_idx].color_num);
}



/*Helper function for add extra flags. Adds the flags from xtra2*/
static u32b add_xtra2_flags(u32b xtra_flags, byte xtra_size, u32b xtra_base)
{
    byte i;

    u32b flag_check = 0x00000001L;

    u32b return_flag = 0;

    for (i = 0; i < xtra_size; i++)
    {
        /*Do we have this flag?*/
        if (xtra_flags & flag_check)
        {
            /*mark it*/
            return_flag |= xtra_base;
        }

        /*shift everything for the next check*/
        flag_check  = flag_check << 1;
        xtra_base  = xtra_base << 1;

    }

    return (return_flag);
}


void object_type::update_object_flags()
{
    obj_flags_1 = obj_flags_2 = obj_flags_3 = obj_flags_native = 0L;
    known_obj_flags_1 = known_obj_flags_2 = known_obj_flags_3 = known_obj_flags_native = 0L;

    object_kind *k_ptr = &k_info[k_idx];

    // Start with the actual object flags.  The known flags will be figured out further below
    /* Base object */
    obj_flags_1 = k_ptr->k_flags1;
    obj_flags_2 = k_ptr->k_flags2;
    obj_flags_3 = k_ptr->k_flags3;
    obj_flags_native = k_ptr->k_native;

    /* Artifact */
    if (art_num)
    {
        artifact_type *a_ptr = &a_info[art_num];

        obj_flags_1 |= a_ptr->a_flags1;
        obj_flags_2 |= a_ptr->a_flags2;
        obj_flags_3 |= a_ptr->a_flags3;
        obj_flags_native |= a_ptr->a_native;
    }

    /* Ego-item */
    if (ego_num)
    {
        ego_item_type *e_ptr = &e_info[ego_num];

        obj_flags_1 |= e_ptr->e_flags1;
        obj_flags_2 |= e_ptr->e_flags2;
        obj_flags_3 |= e_ptr->e_flags3;
        obj_flags_native |= e_ptr->e_native;
    }

    /*hack - chests use xtra1 to store the theme, don't give additional powers to chests*/
    /* Extra powers */
    if (!is_chest())
    {
        switch (xtra1)
        {
            case OBJECT_XTRA_STAT_SUSTAIN:
            {
                /* Flag 2 */
                obj_flags_2 |= add_xtra2_flags(xtra2, OBJECT_XTRA_SIZE_SUSTAIN,
                                            OBJECT_XTRA_BASE_SUSTAIN);
                break;
            }

            case OBJECT_XTRA_TYPE_HIGH_RESIST:
            {
                /* Flag 2 */
                obj_flags_2 |= add_xtra2_flags(xtra2, OBJECT_XTRA_SIZE_HIGH_RESIST,
                                            OBJECT_XTRA_BASE_HIGH_RESIST);
                break;
            }

            case OBJECT_XTRA_TYPE_POWER:
            {
                /* Flag 3 */
                obj_flags_3 |= add_xtra2_flags(xtra2, OBJECT_XTRA_SIZE_POWER,
                                            OBJECT_XTRA_BASE_POWER);
                break;
            }
            case OBJECT_XTRA_TYPE_IMMUNITY:
            {
                /* Flag 2 */
                obj_flags_2 |= add_xtra2_flags(xtra2, OBJECT_XTRA_SIZE_IMMUNITY,
                                            OBJECT_XTRA_BASE_IMMUNITY);
                break;
            }
            case OBJECT_XTRA_TYPE_STAT_ADD:
            {
                /* Flag 1 */
                obj_flags_1 |= add_xtra2_flags(xtra2, OBJECT_XTRA_SIZE_STAT_ADD,
                                            OBJECT_XTRA_BASE_STAT_ADD);
                /*Stat add Also sustains*/
                obj_flags_2 |= add_xtra2_flags(xtra2, OBJECT_XTRA_SIZE_SUSTAIN,
                                            OBJECT_XTRA_BASE_SUSTAIN);
                break;
            }
            case OBJECT_XTRA_TYPE_SLAY:
            {
                /* Flag 1 */
                obj_flags_1 |= add_xtra2_flags(xtra2, OBJECT_XTRA_SIZE_SLAY,
                                            OBJECT_XTRA_BASE_SLAY);
                break;
            }
            case OBJECT_XTRA_TYPE_KILL:
            {
                /* Flag 1 */
                obj_flags_1 |= add_xtra2_flags(xtra2, OBJECT_XTRA_SIZE_KILL,
                                            OBJECT_XTRA_BASE_KILL);
                break;
            }
            case OBJECT_XTRA_TYPE_BRAND:
            {
                /* Flag 1 */
                obj_flags_1 |= add_xtra2_flags(xtra2, OBJECT_XTRA_SIZE_BRAND,
                                            OBJECT_XTRA_BASE_BRAND);
                /*
                 * elemental brands also provide the appropriate resist
                 * Note that the OBJECT_XTRA_SIZE_LOW_RESIST is not used.  There
                 * are only 4 base resists, but 5 base brands (+poison).  Hence the
                 * OBJECT_XTRA_SIZE_BRAND used here is deliberate and not a bug.
                 */
                obj_flags_2 |= add_xtra2_flags(xtra2, OBJECT_XTRA_SIZE_BRAND,
                                            OBJECT_XTRA_BASE_LOW_RESIST);

                break;
            }
            case OBJECT_XTRA_TYPE_LOW_RESIST:
            {
                /* Flag 2 */
                obj_flags_2 |= add_xtra2_flags(xtra2, OBJECT_XTRA_SIZE_LOW_RESIST,
                                            OBJECT_XTRA_BASE_LOW_RESIST);
                break;
            }
            case OBJECT_XTRA_TYPE_NATIVE:
            {
                /* Flag native */
               obj_flags_native |= add_xtra2_flags(xtra2, OBJECT_XTRA_SIZE_NATIVE,
                                            OBJECT_XTRA_BASE_NATIVE);
                break;
            }
        }

        /*Now add the ignores for any xtra above*/
        if (obj_flags_2 & (TR2_RES_ACID))	obj_flags_3 |= TR3_IGNORE_ACID;
        if (obj_flags_2 & (TR2_RES_ELEC))	obj_flags_3 |= TR3_IGNORE_ELEC;
        if (obj_flags_2 & (TR2_RES_FIRE))	obj_flags_3 |= TR3_IGNORE_FIRE;
        if (obj_flags_2 & (TR2_RES_COLD))	obj_flags_3 |= TR3_IGNORE_COLD;
        if (obj_flags_native & (TN1_NATIVE_LAVA | TN1_NATIVE_FIRE)) obj_flags_3 |= TR3_IGNORE_FIRE;
        if (obj_flags_native & (TN1_NATIVE_ICE)) obj_flags_3 |= TR3_IGNORE_COLD;
        if (obj_flags_native & (TN1_NATIVE_ACID)) obj_flags_3 |= TR3_IGNORE_ACID;
    }

    // Now handle what flags the player is aware of

    // The simplest cases first, the player knows everything...
    if (is_known_fully() && !is_chest())
    {
        known_obj_flags_1 = obj_flags_1;
        known_obj_flags_2 = obj_flags_2;
        known_obj_flags_3 = obj_flags_3;
        known_obj_flags_native = obj_flags_native;
        return;
    }

    // or the player knows nothing (includes chests with unknown traps)
    if (!is_known()) return;

    // Proceed with simple id'ed items

    /* Base object */
    known_obj_flags_1 = k_ptr->k_flags1;
    known_obj_flags_2 = k_ptr->k_flags2;
    known_obj_flags_3 = k_ptr->k_flags3;
    known_obj_flags_native = k_ptr->k_native;

    /* Basic ego-item values are known*/
    if (ego_num)
    {
        ego_item_type *e_ptr = &e_info[ego_num];

        known_obj_flags_1 |= e_ptr->e_flags1;
        known_obj_flags_2 |= e_ptr->e_flags2;
        known_obj_flags_3 |= e_ptr->e_flags3;
        known_obj_flags_native |= e_ptr->e_native;
    }

    // Artifacts have some known flags
    if (art_num)
    {
        if (is_known())
        {
            artifact_type *a_ptr = &a_info[art_num];

            known_obj_flags_1 |= (a_ptr->a_flags1 & (TR1_PVAL_MASK));
            known_obj_flags_3 |= (a_ptr->a_flags3 & (TR3_IGNORE_MASK));
        }
    }

    // Chests use xtra1 to store the theme.  Don't give them special powers
    if (is_chest() || has_hidden_powers()) return;

    // add known extra powers to id'ed items
    switch (xtra1)
    {
        case OBJECT_XTRA_TYPE_SLAY:
        {
            /* Flag 1 */
            known_obj_flags_1 |= add_xtra2_flags(xtra2, OBJECT_XTRA_SIZE_SLAY,
                                        OBJECT_XTRA_BASE_SLAY);
            break;
        }
        case OBJECT_XTRA_TYPE_KILL:
        {
            known_obj_flags_1 |= add_xtra2_flags(xtra2, OBJECT_XTRA_SIZE_KILL,
                                                  OBJECT_XTRA_BASE_KILL);
            break;
        }
        case OBJECT_XTRA_TYPE_BRAND:
        {
            /* Flag 1 */
            known_obj_flags_1 |= add_xtra2_flags(xtra2, OBJECT_XTRA_SIZE_BRAND,
                                        OBJECT_XTRA_BASE_BRAND);
            /*
             * elemental brands also provide the appropriate resist
             * Note that the OBJECT_XTRA_SIZE_LOW_RESIST is not used.  There
             * are only 4 base resists, but 5 base brands (+poison).  Hence the
             * OBJECT_XTRA_SIZE_BRAND used here is deliberate and not a bug.
             */
            known_obj_flags_2 |= add_xtra2_flags(xtra2, OBJECT_XTRA_SIZE_BRAND,
                                        OBJECT_XTRA_BASE_LOW_RESIST);
            break;
        }
        case OBJECT_XTRA_TYPE_LOW_RESIST:
        {
            /* Flag 2 */
            known_obj_flags_2 |= add_xtra2_flags(xtra2, OBJECT_XTRA_SIZE_LOW_RESIST,
                                                 OBJECT_XTRA_BASE_LOW_RESIST);
            break;
        }
        case OBJECT_XTRA_TYPE_NATIVE:
        {
            /* Flag native */
            known_obj_flags_native |= add_xtra2_flags(xtra2, OBJECT_XTRA_SIZE_NATIVE,
                                        OBJECT_XTRA_BASE_NATIVE);
            break;
        }
    }

    /*Now add the ignores for any xtra above*/
    if (known_obj_flags_2 & (TR2_RES_ACID))	known_obj_flags_3 |= TR3_IGNORE_ACID;
    if (known_obj_flags_2 & (TR2_RES_ELEC))	known_obj_flags_3 |= TR3_IGNORE_ELEC;
    if (known_obj_flags_2 & (TR2_RES_FIRE))	known_obj_flags_3 |= TR3_IGNORE_FIRE;
    if (known_obj_flags_2 & (TR2_RES_COLD))	known_obj_flags_3 |= TR3_IGNORE_COLD;
    if (known_obj_flags_native & (TN1_NATIVE_LAVA | TN1_NATIVE_FIRE)) known_obj_flags_3 |= TR3_IGNORE_FIRE;
    if (known_obj_flags_native & (TN1_NATIVE_ICE)) known_obj_flags_3 |= TR3_IGNORE_COLD;
    if (known_obj_flags_native & (TN1_NATIVE_ACID)) known_obj_flags_3 |= TR3_IGNORE_ACID;
}

// Return the object kind character
bool object_type::use_flavor()
{
    return (k_info[k_idx].use_flavor());
}

// Return the object character
QChar object_type::get_char()
{
    return (k_info[k_idx].get_char());
}

// Return the object color
QColor object_type::get_color()
{
    return (k_info[k_idx].get_color());
}

QString object_type::get_tile_id()
{
    return (k_info[k_idx].get_tile_id());
}

object_kind::object_kind()
{
    object_kind_wipe();
}

void object_kind::object_kind_wipe()
{
    k_name.clear();
    k_text.clear();

    tval = sval = pval = 0;
    to_h = to_d = to_a = 0;
    ac = dd = ds = weight = cost = 0;
    k_flags1 = k_flags2 = k_flags3 = k_native = k_store = 0;
    effect = 0;
    C_WIPE(locale, 4, byte);
    C_WIPE(chance, 4, byte);
    k_level = extra = 0;
    color_num = 0;
    d_color = QColor("black");
    d_char = QChar(' ');
    autoinscribe.clear();
    flavor = squelch = 0;
    aware = tried = everseen = FALSE;
    C_WIPE(use_verify, VERIFY_MAX, byte);

}

// Determine if flavor glyph should be used.
bool object_kind::use_flavor()
{
    if (!flavor) return (FALSE);

    if (tval != TV_SCROLL) return TRUE;

    // Id'ed scrolls use the object info.
    if (aware) return (TRUE);
    return (FALSE);
}

// Factor in flavor when returning object kind character
QChar object_kind::get_char()
{
    if (use_flavor()) return (flavor_info[flavor].d_char);

    return (d_char);
}

// Factor in flavor when returning object kind character
QColor object_kind::get_color()
{
    if (use_flavor()) return (flavor_info[flavor].d_color);

    return (d_color);
}

// Factor in flavor when returning tile id
QString object_kind::get_tile_id()
{
    if (use_flavor()) return (flavor_info[flavor].tile_id);

    return (tile_id);
}

bool object_kind::is_trap_object_kind()
{
    if (tval == TV_WAND)
    {
        switch (sval)
        {
            case SV_WAND_DISARMING:
            case SV_WAND_TRAP_DOOR_DEST:
            {
                return (TRUE);
            }
            default:    return (FALSE);
        }

    }
    if (tval == TV_ROD)
    {
        switch (sval)
        {
            case SV_ROD_DISARMING:
            {
                return (TRUE);
            }
            default:    return (FALSE);
        }
    }

    return (FALSE);
}

artifact_type::artifact_type()
{
    artifact_wipe();
}

/*
 * Wipe all data in an randart.
 * This function needs to have all variables in artifact_type
 * This function does not clear the object that contains the randart.
 * It should be not called directly on any artifact already mapped to an object.
 */
void artifact_type::artifact_wipe()
{
    a_name.clear();
    a_text.clear();
    sval = tval = pval = 0;
    to_h = to_d = to_a = ac = 0;
    dd = ds = 0;
    weight = 0;
    cost = 0;
    a_flags1 = a_flags2 = a_flags3 = a_native = 0;
    a_level = a_rarity = a_cur_num = a_max_num = activation = 0;
    time = randtime = 0;
}

bool artifact_type::is_special_artifact()
{
    if (a_flags3 & (TR3_INSTA_ART)) return (TRUE);
    return (FALSE);
}


flavor_type::flavor_type()
{
    flavor_wipe();
}

void flavor_type::flavor_wipe()
{
    text.clear();
    tval = sval = color_num = 0;
    d_color = QColor("black");
    d_char = QChar(' ');
    tile_id.clear();
}

void cmd_arg::wipe()
{
    repeats = choice = item = number = direction = slot = k_idx = 0;
    verify = FALSE;
    string1.clear();
    string2.clear();
};


