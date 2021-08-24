
/* File: was object2.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * 						Jeff Greene, Diego Gonzalez
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include "src/npp.h"
#include <src/cmds.h>
#include "src/store.h"





/*
 * Choose an object kind that seems "appropriate" to the given level
 *
 * This function uses the "prob2" field of the "object allocation table",
 * and various local information, to calculate the "prob3" field of the
 * same table, which is then used to choose an "appropriate" object, in
 * a relatively efficient manner.
 *
 * It is (slightly) more likely to acquire an object of the given level
 * than one of a lower level.  This is done by choosing several objects
 * appropriate to the given level and keeping the "hardest" one.
 *
 * Note that if no objects are "appropriate", then this function will
 * fail, and return zero, but this should *almost* never happen.
 * (but it does happen with certain themed items occasionally). -JG
 */
s16b get_obj_num(int level)
{
    int i, j, p;

    int k_idx;

    long value, total;

    /* Boost level */
    if (level > 0)
    {
        /* Occasional "boost" */
        if (one_in_(GREAT_OBJ))
        {
            /* What a bizarre calculation */
            level = 1 + (level * MAX_DEPTH    / randint(MAX_DEPTH));
        }
    }

    /* Reset total */
    total = 0L;

    /* Process probabilities */
    for (i = 0; i < alloc_kind_table.size(); i++)
    {
        alloc_entry_new *ae_ptr = &alloc_kind_table[i];

        object_kind *k_ptr = &k_info[ae_ptr->index];

        /* Objects are sorted by depth */
        if (ae_ptr->level > level) break;

        /* Default */
        ae_ptr->final_probability = 0;

        /* Get the index */
        k_idx = ae_ptr->index;

        /* Get the actual kind */
        k_ptr = &k_info[k_idx];

        /* Hack -- prevent embedded chests, but allow them for quests*/
        if ((object_generation_mode == OB_GEN_MODE_CHEST)
                && (k_ptr->tval == TV_CHEST)) continue;

        /* Accept */
        ae_ptr->final_probability = ae_ptr->hook_probability;

        /* Total */
        total += ae_ptr->final_probability;
    }

    /* No legal objects */
    if (total <= 0) return (0);

    /* Pick an object */
    value = rand_int(total);

    /* Find the object */
    for (i = 0; i < alloc_kind_table.size(); i++)
    {
        /* Found the entry */
        if (value < alloc_kind_table[i].final_probability) break;

        /* Decrement */
        value = value - alloc_kind_table[i].final_probability;
    }


    /* Power boost */
    p = rand_int(100);

    /* Try for a "better" object once (50%) or twice (10%) */
    if (p < 60)
    {
        /* Save old */
        j = i;

        /* Pick a object */
        value = rand_int(total);

        /* Find the object */
        for (i = 0; i < alloc_kind_table.size(); i++)
        {

            /* Found the entry */
            if (value < alloc_kind_table[i].final_probability) break;

            /* Decrement */
            value = value - alloc_kind_table[i].final_probability;
        }

        /* Keep the "best" one */
        if (alloc_kind_table[i].level < alloc_kind_table[j].level) i = j;
    }

    /* Try for a "better" object twice (10%) */
    if (p < 10)
    {
        /* Save old */
        j = i;

        /* Pick a object */
        value = rand_int(total);

        /* Find the object */
        for (i = 0; i < alloc_kind_table.size(); i++)
        {

            /* Found the entry */
            if (value < alloc_kind_table[i].final_probability) break;

            /* Decrement */
            value = value - alloc_kind_table[i].final_probability;
        }

        /* Keep the "best" one */
        if (alloc_kind_table[i].level < alloc_kind_table[j].level) i = j;
    }

    /* Result */
    return (alloc_kind_table[i].index);
}





/*
 * Prepare an object based on an object kind.
 */
void object_prep(object_type *o_ptr, int k_idx)
{
    object_kind *k_ptr = &k_info[k_idx];

    /* Clear the record */
    o_ptr->object_wipe();

    /* Save the kind index */
    o_ptr->k_idx = k_idx;

    /* Efficiency -- tval/sval */
    o_ptr->tval = k_ptr->tval;    
    o_ptr->sval = k_ptr->sval;

    /* Default "pval" */
    o_ptr->pval = k_ptr->pval;

    /* Default number */
    o_ptr->number = 1;

    /* Default weight */
    o_ptr->weight = k_ptr->weight;

    /* Default magic */
    o_ptr->to_h = k_ptr->to_h;
    o_ptr->to_d = k_ptr->to_d;
    o_ptr->to_a = k_ptr->to_a;

    /* Default power */
    o_ptr->ac = k_ptr->ac;
    o_ptr->dd = k_ptr->dd;
    o_ptr->ds = k_ptr->ds;

    /* Hack -- worthless items are always "broken" */
    if (k_ptr->cost <= 0) o_ptr->ident |= (IDENT_BROKEN);

    /* Hack -- cursed items are always "cursed" */
    if (k_ptr->k_flags3 & (TR3_LIGHT_CURSE)) o_ptr->ident |= (IDENT_CURSED);
    if (k_ptr->k_flags3 & (TR3_HEAVY_CURSE)) o_ptr->ident |= (IDENT_CURSED);
    if (k_ptr->k_flags3 & (TR3_PERMA_CURSE)) o_ptr->ident |= (IDENT_CURSED);

    /* Hack -- extract the perfect_balance flag */
    if (k_ptr->k_flags3 & (TR3_PERFECT_BALANCE)) o_ptr->ident |= (IDENT_PERFECT_BALANCE);
}





/*
 * Cheat -- describe a created object for the user
 */
static void object_mention(object_type *o_ptr)
{
    QString o_name = object_desc_spoil(o_ptr);

    /* Artifact */
    if (o_ptr->is_artifact())
    {
        /* Silly message */
        message(QString("Artifact (%1)") .arg(o_name));
    }

    /* Ego-item */
    else if (o_ptr->is_ego_item())
    {
        /* Silly message */
        message(QString("Ego-item (%1)") .arg(o_name));
    }

    /* Normal item */
    else
    {
        /* Silly message */
        message(QString("Object (%1)") .arg(o_name));
    }
}


/*
 * Attempt to change an object into an ego-item -MWK-
 * Better only called by apply_magic().
 * The return value says if we picked a cursed item (if allowed) and is
 * passed on to a_m_aux1/2().
 * If no legal ego item is found, this routine returns 0, resulting in
 * an unenchanted item.
 */
static int make_ego_item(object_type *o_ptr, bool only_good, bool only_great)
{
    int i, j, level;

    int e_idx;

    long value, total;

    ego_item_type *e_ptr;

    /* Fail if object already is ego or artifact */
    if (o_ptr->art_num) return (FALSE);
    if (o_ptr->ego_num) return (FALSE);

    level = object_level;

    /* Boost level (like with object base types) */
    if (level > 0)
    {
        /* Occasional "boost" */
        if (one_in_(GREAT_EGO))
        {
            /* The bizarre calculation again */
            level = 1 + (level * MAX_DEPTH / randint(MAX_DEPTH));
        }
    }

    /* Reset total */
    total = 0L;

    /* Process probabilities */
    for (i = 0; i < alloc_ego_table.size(); i++)
    {
        alloc_entry_new *ae_ptr = &alloc_ego_table[i];

        // Ego items don't currently use the "hook" phase
        ae_ptr->hook_probability = ae_ptr->base_probability;

        /* Default */
        ae_ptr->final_probability = 0;

        /* Ego Items are sorted by depth */
        if (ae_ptr->level > level) continue;

        /* Get the index */
        e_idx = ae_ptr->index;

        /* Get the actual kind */
        e_ptr = &e_info[e_idx];

        /* If we force good/great, don't create cursed */
        if (only_good && (e_ptr->e_flags3 & TR3_LIGHT_CURSE)) continue;

        /* Test if this is a legal ego-item type for this object */
        for (j = 0; j < EGO_TVALS_MAX; j++)
        {
            /* Require identical base type */
            if (o_ptr->tval == e_ptr->tval[j])
            {
                /* Require sval in bounds, lower */
                if (o_ptr->sval >= e_ptr->min_sval[j])
                {
                    /* Require sval in bounds, upper */
                    if (o_ptr->sval <= e_ptr->max_sval[j])
                    {
                        /* Accept */
                        ae_ptr->final_probability = ae_ptr->hook_probability;
                    }
                }
            }
        }

        /* Total */
        total += ae_ptr->final_probability;
    }

    /* No ego-item types for this object (example: books in non-ironman mode) */
    if (total == 0) return (0);

    /*enforce a true rarity if there are only one or a few rare ego items*/
    if ((randint(100) > total) && (!only_great)) return (0);

    /* Pick an ego-item */
    value = rand_int(total);

    /* Find the ego-item */
    for (i = 0; i < alloc_ego_table.size(); i++)
    {
        alloc_entry_new *ae_ptr = &alloc_ego_table[i];

        /* Found the entry */
        if (value < ae_ptr->final_probability) break;

        /* Decrement */
        value = value - ae_ptr->final_probability;
    }

    /* We have one */
    e_idx = (byte)alloc_ego_table[i].index;
    o_ptr->ego_num = e_idx;

    return ((e_info[e_idx].e_flags3 & TR3_LIGHT_CURSE) ? -2 : 2);
}


/*
 * Mega-Hack -- Attempt to create one of the "Special Objects".
 *
 * We are only called from "make_object()", and we assume that
 * "apply_magic()" is called immediately after we return.
 *
 * Note -- see "make_artifact()" and "apply_magic()".
 *
 * We *prefer* to create the special artifacts in order, but this is
 * normally outweighed by the "rarity" rolls for those artifacts.  The
 * only major effect of this logic is that the Phial (with rarity one)
 * is always the first special artifact created.
 */
static bool make_artifact_special(object_type *o_ptr)
{
    int i;

    int k_idx;

    int depth_check = ((object_generation_mode) ?  object_level : p_ptr->depth);

    /*no artifacts while making items for stores*/
    if ((object_generation_mode >= OB_GEN_STORE_HEAD) &&
        (object_generation_mode <= OB_GEN_STORE_TAIL)) return (FALSE);

    /*no special artifacts as quest rewards */
    if (object_generation_mode == OB_GEN_MODE_QUEST) return (FALSE);

    /* No artifacts, do nothing */
    if (birth_no_artifacts) return (FALSE);

    /* No artifacts in the town, unless opening a chest or creating chest item */
    if (!depth_check) return (FALSE);

    /* Check the special artifacts */
    for (i = 0; i < z_info->art_spec_max; ++i)
    {
        artifact_type *a_ptr = &a_info[i];

        /* Skip "empty" artifacts */
        if (a_ptr->tval + a_ptr->sval == 0) continue;

        /* Cannot make an artifact twice */
        if (a_ptr->a_cur_num) continue;

        /*Hack - don't allow cursed artifacts as quest items*/
        if (object_generation_mode == OB_GEN_MODE_QUEST)
        {
            if (a_ptr->a_flags3 & (TR3_LIGHT_CURSE)) continue;
            if (a_ptr->a_flags3 & (TR3_HEAVY_CURSE)) continue;
            if (a_ptr->a_flags3 & (TR3_PERMA_CURSE)) continue;
        }

        /* Enforce minimum "depth" (loosely) */
        if (a_ptr->a_level > depth_check)
        {
            /* Get the "out-of-depth factor" */
            int d = (a_ptr->a_level - depth_check) * 2;

            /* Roll for out-of-depth creation */
            if (rand_int(d) != 0) continue;
        }

        /* Artifact "rarity roll" */
        if (rand_int(a_ptr->a_rarity) != 0) continue;

        /* Find the base object */
        k_idx = lookup_kind(a_ptr->tval, a_ptr->sval);

        /* Enforce minimum "object" level (loosely) */
        if (k_info[k_idx].k_level > depth_check)
        {
            /* Get the "out-of-depth factor" */
            int d = (k_info[k_idx].k_level - depth_check) * 5;

            /* Roll for out-of-depth creation */
            if (rand_int(d) != 0) continue;
        }

        /* Assign the template */
        object_prep(o_ptr, k_idx);

        /* Mark the item as an artifact */
        o_ptr->art_num = i;

        /* Success */
        return (TRUE);
    }

    /* Failure */
    return (FALSE);
}


/*
 * Attempt to change an object into an artifact
 *
 * This routine should only be called by "apply_magic()"
 *
 * Note -- see "make_artifact_special()" and "apply_magic()"
 */
static bool make_artifact(object_type *o_ptr)
{
    int i;

    int depth_check = ((object_generation_mode) ?  object_level : p_ptr->depth);

    /*no artifacts while making items for stores, this is a double-precaution*/
    if ((object_generation_mode >= OB_GEN_MODE_GEN_ST) &&
        (object_generation_mode <= OB_GEN_MODE_BOOKSHOP)) return (FALSE);

    /* No artifacts, do nothing */
    if (birth_no_artifacts) return (FALSE);

    /* No artifacts in the town, unless opening a chest or creating chest item */
    if (!depth_check) return (FALSE);

    /* First try to create a randart, if allowed */
    if ((can_be_randart(o_ptr)) && (!birth_no_xtra_artifacts))
    {
        int chance = depth_check;

        /* Hack - harder for quest objects because so many of them are generated */
        if (object_generation_mode == OB_GEN_MODE_QUEST) if (chance < 20) chance = 20;

        chance += (MAX_DEPTH + depth_check) / 10;

        /*occasionally make a randart*/
        if(one_in_(chance))
        {
            /*artifact power is based on depth*/
            int randart_power = 20 + (depth_check * 6 / 5);

            /*occasional power boost*/
            while (one_in_(25)) randart_power += 35;

            /*
             * Make a randart.  This should always succeed, unless
             * there is no space for another randart
             */
            if (make_one_randart(o_ptr, randart_power, FALSE)) return (TRUE);
        }
    }

    /* Paranoia -- no "plural" artifacts */
    if (o_ptr->number != 1) return (FALSE);

    /* Check the artifact list (skip the "specials" and randoms) */
    for (i = z_info->art_spec_max; i < z_info->art_norm_max; i++)
    {
        artifact_type *a_ptr = &a_info[i];

        /* Skip "empty" items */
        if (a_ptr->tval + a_ptr->sval == 0) continue;

        /* Cannot make an artifact twice */
        if (a_ptr->a_cur_num) continue;

        /* Must have the correct fields */
        if (a_ptr->tval != o_ptr->tval) continue;
        if (a_ptr->sval != o_ptr->sval) continue;

        /*Hack - don't allow cursed artifacts as quest items*/
        if (object_generation_mode == OB_GEN_MODE_QUEST)
        {
            if (a_ptr->a_flags3 & (TR3_LIGHT_CURSE)) continue;
            if (a_ptr->a_flags3 & (TR3_HEAVY_CURSE)) continue;
            if (a_ptr->a_flags3 & (TR3_PERMA_CURSE)) continue;
        }

        /* XXX XXX Enforce minimum "depth" (loosely) */
        if (a_ptr->a_level > depth_check)
        {
            /* Get the "out-of-depth factor" */
            int d = (a_ptr->a_level - depth_check) * 2;

            /* Roll for out-of-depth creation */
            if (rand_int(d) != 0) continue;
        }

        /* We must make the "rarity roll" */
        if (!one_in_(a_ptr->a_rarity)) continue;

        /* Mark the item as an artifact */
        o_ptr->art_num = i;

        /* Success */
        return (TRUE);
    }

    /* Failure */
    return (FALSE);
}


/*
 * Charge a new wand.
 */
s16b charge_wand(object_type *o_ptr, int percent)
{
    s16b pval = 0;

    switch (o_ptr->sval)
    {
        case SV_WAND_HEAL_MONSTER:		pval = randint(20) + 8; break;
        case SV_WAND_HASTE_MONSTER:		pval = randint(20) + 8; break;
        case SV_WAND_CLONE_MONSTER:		pval = randint(5)  + 3; break;
        case SV_WAND_TELEPORT_AWAY:		pval = randint(5)  + 6; break;
        case SV_WAND_DISARMING:			pval = randint(5)  + 4; break;
        case SV_WAND_TRAP_DOOR_DEST:	pval = randint(8)  + 6; break;
        case SV_WAND_STONE_TO_MUD:		pval = randint(4)  + 3; break;
        case SV_WAND_LIGHT:				pval = randint(10) + 6; break;
        case SV_WAND_SLEEP_MONSTER:		pval = randint(15) + 8; break;
        case SV_WAND_SLOW_MONSTER:		pval = randint(10) + 6; break;
        case SV_WAND_CONFUSE_MONSTER:	pval = randint(12) + 6; break;
        case SV_WAND_FEAR_MONSTER:		pval = randint(5)  + 3; break;
        case SV_WAND_DRAIN_LIFE:		pval = randint(3)  + 3; break;
        case SV_WAND_POLYMORPH:			pval = randint(8)  + 6; break;
        case SV_WAND_STINKING_CLOUD:	pval = randint(8)  + 6; break;
        case SV_WAND_MAGIC_MISSILE:		pval = randint(10) + 6; break;
        case SV_WAND_ACID_BOLT:			pval = randint(8)  + 6; break;
        case SV_WAND_ELEC_BOLT:			pval = randint(8)  + 6; break;
        case SV_WAND_FIRE_BOLT:			pval = randint(8)  + 6; break;
        case SV_WAND_COLD_BOLT:			pval = randint(5)  + 6; break;
        case SV_WAND_ACID_BALL:			pval = randint(5)  + 2; break;
        case SV_WAND_ELEC_BALL:			pval = randint(8)  + 4; break;
        case SV_WAND_FIRE_BALL:			pval = randint(4)  + 2; break;
        case SV_WAND_COLD_BALL:			pval = randint(6)  + 2; break;
        case SV_WAND_WONDER:			pval = randint(15) + 8; break;
        case SV_WAND_ANNIHILATION:		pval = randint(2)  + 1; break;
        case SV_WAND_DRAGON_FIRE:		pval = randint(3)  + 1; break;
        case SV_WAND_DRAGON_COLD:		pval = randint(3)  + 1; break;
        case SV_WAND_DRAGON_BREATH:		pval = randint(3)  + 1; break;
        case SV_WAND_WALL_BUILDING:		pval = randint(5)  + 3; break;
    }

    /* Apply the proportional amount of charges */
    pval = pval * percent / 100;
    if (pval < 1) pval = 1;

    return (pval);
}



/*
 * Charge a new staff.
 */
s16b charge_staff(object_type *o_ptr, int percent)
{
    s16b pval = 0;

    switch (o_ptr->sval)
    {
        case SV_STAFF_DARKNESS:			pval = randint(8)  + 8; break;
        case SV_STAFF_SLOWNESS:			pval = randint(8)  + 8; break;
        case SV_STAFF_HASTE_MONSTERS:	pval = randint(8)  + 8; break;
        case SV_STAFF_SUMMONING:		pval = randint(3)  + 1; break;
        case SV_STAFF_TELEPORTATION:	pval = randint(4)  + 5; break;
        case SV_STAFF_IDENTIFY:			pval = randint(10) + 10; break;
        case SV_STAFF_STARLIGHT:		pval = randint(5)  + 6; break;
        case SV_STAFF_LIGHT:			pval = randint(20) + 8; break;
        case SV_STAFF_MAPPING:			pval = randint(5)  + 5; break;
        case SV_STAFF_DETECT_GOLD:		pval = randint(20) + 8; break;
        case SV_STAFF_DETECT_ITEM:		pval = randint(15) + 6; break;
        case SV_STAFF_DETECT_TRAP:		pval = randint(5)  + 6; break;
        case SV_STAFF_DETECT_DOOR:		pval = randint(8)  + 6; break;
        case SV_STAFF_DETECT_INVIS:		pval = randint(15) + 8; break;
        case SV_STAFF_DETECT_EVIL:		pval = randint(15) + 8; break;
        case SV_STAFF_CURE_LIGHT:		pval = randint(5)  + 6; break;
        case SV_STAFF_CURING:			pval = randint(3)  + 4; break;
        case SV_STAFF_HEALING:			pval = randint(2)  + 1; break;
        case SV_STAFF_THE_MAGI:			pval = randint(2)  + 2; break;
        case SV_STAFF_SLEEP_MONSTERS:	pval = randint(5)  + 6; break;
        case SV_STAFF_SLOW_MONSTERS:	pval = randint(5)  + 6; break;
        case SV_STAFF_SPEED:			pval = randint(3)  + 4; break;
        case SV_STAFF_PROBING:			pval = randint(6)  + 2; break;
        case SV_STAFF_DISPEL_EVIL:		pval = randint(3)  + 4; break;
        case SV_STAFF_POWER:			pval = randint(3)  + 1; break;
        case SV_STAFF_HOLINESS:			pval = randint(2)  + 2; break;
        case SV_STAFF_BANISHMENT:		pval = randint(2)  + 1; break;
        case SV_STAFF_EARTHQUAKES:		pval = randint(5)  + 3; break;
        case SV_STAFF_DESTRUCTION:		pval = randint(3)  + 1; break;
        case SV_STAFF_MASS_IDENTIFY:	pval = randint(5) + 5; break;
        case SV_STAFF_MASS_POLYMORPH:	pval = randint(5)  + 6; break;
        case SV_STAFF_REMOVE_CURSE:		pval = randint(3)  + 4; break;
    }

    /* Apply the proportional amount of charges */
    pval = pval * percent / 100;
    if (pval < 1) pval = 1;

    return (pval);
}

/*
 *
 * Determines the theme of a chest.  This function is called
 *  when the chest is being created. JG
 *
 */
static int choose_chest_contents (void)
{
    int chest_theme; /*the returned chest theme*/

    int minlevel; /*helps keep low level themes from appearing at higher levels*/

    int chestlevel; /* random number which determines type of chest theme*/

    int num; /*number used in random section*/

    /*keep weaker themes out of deeper levels*/
    minlevel = object_level / 4;

    /*Hack - don't wan't results over 100 to keep dragon armor themed chests rare*/
    if ((object_level + minlevel) > 100) num = 100 - minlevel;

    else num = object_level;

    /* Hack - some themes don't work in Moria, and there are fewer levels. Cut it off at 60. */
    if (game_mode == GAME_NPPMORIA)
    {
        if ((object_level + minlevel) > 60) num = 60 - minlevel;

        else num = object_level;
    }

    chestlevel = randint (num) + minlevel;

    /* Hack - simpler themes for Moria */
    if (game_mode == GAME_NPPMORIA)
    {
        /* chest theme #1 is treasure, theme 16 is a chest, not used here.  */
        if (chestlevel <= 10) 	chest_theme = DROP_TYPE_GOLD;
        else if (chestlevel <=25) chest_theme = DROP_TYPE_MORIA_ITEMS;
        else if (one_in_(3)) 	chest_theme = DROP_TYPE_MORIA_WEAPONS;
        else if (one_in_(2)) 	chest_theme = DROP_TYPE_MORIA_ARMOR_BODY;
        else 					chest_theme = DROP_TYPE_MORIA_ARMOR_OTHER;
        return(chest_theme);
    }

    /*now determine the chest theme*/

    /* chest theme #1 is treasure, theme 16 is a chest, not used here.  */
    if (chestlevel <= 10) chest_theme = DROP_TYPE_GOLD;

    /*
     * from 500' to 1100", treasure begins to give way to
     * potions, rods/wands/staffs, and scrolls all with almost equal chances.
     * chest theme #16 is reserved generating an actual chest, it shouldn't be returned here
     *     which returns the object *nothing* while opening a chest.
     * chest theme #2 is potions  (+ mushroom of restoring)
     * chest theme #3 is rods/wands/staffs
     * chest theme #4 is scrolls
     * with gold, these are the themes up to 1100', where the weapons and
     * armor gradually begin to take over.
     * JG
     */
    else if (chestlevel <=25) chest_theme = (randint (3)) + 1;

    /*
     * The next nine themes are armor/weapons,
     * along with the potions, scrolls, and rods, all with equal chances
     *
     * chest theme # 5 is shields
     * chest theme # 6 is weapons
     * chest theme # 7 is armor (includes dragon armor)
     * chest theme # 8 is boots
     * chest theme # 9 is bow
     * chest theme #10 is cloak
     * chest theme #11 is gloves
     * chest theme #12 is hafted weapons (for the priests)
     * chest theme #13 is headgear (including crowns)
     * JG
     */
    else if (chestlevel <=60) chest_theme = (randint (12)) + 1;

    /*
     * Now 10 themes are available, with
     * jewlery (rings of speed, amulets, and crowns) added.
     * Equal probability for all themes.
     *
     * chest theme # 14 is jewelery
     * JG
     */

    else if (chestlevel <=99) chest_theme = (randint (10)) + 4;

    /*
     * If 100, chest theme # 15 is exclusively
     * dragon armor scale mail.
     */
    else chest_theme = DROP_TYPE_DRAGON_ARMOR;



    return(chest_theme);
}

/*
 * Apply magic to an item known to be a "weapon"
 *
 * Hack -- note special base to hit and damage dice boosting
 * Hack -- note special processing for weapon/digger
 * Hack -- note special rating boost for dragon scale mail
 */
void a_m_aux_1(object_type *o_ptr, int level, int power)
{
    int tohit1 = randint(5) + m_bonus(5, level);
    int todam1 = randint(5) + m_bonus(5, level);

    int tohit2 = m_bonus(10, level);
    int todam2 = m_bonus(10, level);


    /* Good */
    if (power > 0)
    {
        /* Enchant */
        o_ptr->to_h += tohit1;
        o_ptr->to_d += todam1;

        /* Very good */
        if (power > 1)
        {
            /* Enchant again */
            o_ptr->to_h += tohit2;
            o_ptr->to_d += todam2;
        }
    }

    /* Cursed */
    else if (power < 0)
    {
        /* Penalize */
        o_ptr->to_h -= tohit1;
        o_ptr->to_d -= todam1;

        /* Very cursed */
        if (power < -1)
        {
            /* Penalize again */
            o_ptr->to_h -= tohit2;
            o_ptr->to_d -= todam2;
        }

        /* Cursed (if "bad") */
        if (o_ptr->to_h + o_ptr->to_d < 0) o_ptr->ident |= (IDENT_CURSED);
    }


    /* Analyze type */
    switch (o_ptr->tval)
    {
        case TV_DIGGING:
        {
            /* Very bad */
            if (power < -1)
            {
                /* Hack -- Horrible digging bonus */
                o_ptr->pval = 0 - (5 + randint(5));
            }

            /* Bad */
            else if (power < 0)
            {
                /* Hack -- Reverse digging bonus */
                o_ptr->pval = 0 - (o_ptr->pval);
            }

            break;
        }

        case TV_HAFTED:
        case TV_POLEARM:
        case TV_SWORD:
        case TV_BOLT:
        case TV_ARROW:
        case TV_SHOT:
        {
            /*average items*/
            int chance = 50;

            if (power < 0) break;

            /* Very Good */
            if (power > 1) 	chance = 15;
            else if (power > 0) chance = 35;

            /* Hack -- Super-charge the damage dice */
            while ((o_ptr->dd * o_ptr->ds > 0) &&
                   (one_in_(chance)))
            {
                o_ptr->dd++;
            }

            /* Hack -- Limit the damage dice to max of 9*/
            if (o_ptr->dd > 9) o_ptr->dd = 9;

            /* Hack -- Super-charge the damage sides */
            while ((o_ptr->dd * o_ptr->ds > 0) &&
                   (one_in_(chance)))
            {
                o_ptr->ds++;
            }

            /* Hack -- Limit the damage dice to max of 9*/
            if (o_ptr->ds > 9) o_ptr->ds = 9;

            break;
        }

    }
}


/*
 * Apply magic to an item known to be "armor"
 */
void a_m_aux_2(object_type *o_ptr, int level, int power)
{
    int toac1 = randint(5) + m_bonus(5, level);

    int toac2 = m_bonus(10, level);


    /* Good */
    if (power > 0)
    {
        /* Enchant */
        o_ptr->to_a += toac1;

        /* Very good */
        if (power > 1)
        {
            /* Enchant again */
            o_ptr->to_a += toac2;
        }
    }

    /* Cursed */
    else if (power < 0)
    {
        /* Penalize */
        o_ptr->to_a -= toac1;

        /* Very cursed */
        if (power < -1)
        {
            /* Penalize again */
            o_ptr->to_a -= toac2;
        }

        /* Cursed (if "bad") */
        if (o_ptr->to_a < 0) o_ptr->ident |= (IDENT_CURSED);
    }


    /* Analyze type */
    switch (o_ptr->tval)
    {
        case TV_DRAG_ARMOR:
        case TV_DRAG_SHIELD:
        {
            /* Mention the item */
            if (cheat_peek) object_mention(o_ptr);

            break;
        }
    }
}



/*
 * Apply magic to an item known to be a "ring" or "amulet"
 *
 * Hack -- note special rating boost for ring of speed
 * Hack -- note special rating boost for certain amulets
 * Hack -- note special "pval boost" code for ring of speed
 * Hack -- note that some items must be cursed (or blessed)
 */
static void a_m_aux_3(object_type *o_ptr, int level, int power)
{
    /* Apply magic (good or bad) according to type */
    switch (o_ptr->tval)
    {
        case TV_RING:
        {
            /* Analyze */
            switch (o_ptr->sval)
            {
                /* Strength, Constitution, Dexterity, Intelligence */
                case SV_RING_STR:
                case SV_RING_CON:
                case SV_RING_DEX:
                case SV_RING_INT:
                {
                    /* Stat bonus */
                    o_ptr->pval = 1 + m_bonus(5 + (level / 35), level);

                    /*cut it off at 6*/
                    if (o_ptr->pval > 6) o_ptr->pval = 6;

                    /* Cursed */
                    if (power < 0)
                    {
                        /* Broken */
                        o_ptr->ident |= (IDENT_BROKEN);

                        /* Cursed */
                        o_ptr->ident |= (IDENT_CURSED);

                        /* Reverse pval */
                        o_ptr->pval = 0 - (o_ptr->pval);
                    }

                    break;
                }

                /* Ring of Speed! */
                case SV_RING_SPEED:
                {
                    if (game_mode == GAME_NPPMORIA)
                    {
                        /* Cursed Ring */
                        if (power < 0)
                        {
                            /* Broken */
                            o_ptr->ident |= (IDENT_BROKEN);

                            /* Cursed */
                            o_ptr->ident |= (IDENT_CURSED);

                            /* Reverse pval */
                            o_ptr->pval = -1;
                        }
                        else
                        {
                            /* Rating boost for rings of speed that are not cursed */
                            rating += 25;

                            o_ptr->pval = 1;
                        }

                        break;
                    }

                    /* Base speed (1 to 10) */
                    o_ptr->pval = randint(5) + m_bonus(5, level);

                    /* Super-charge the ring */
                    while (one_in_(2)) o_ptr->pval++;

                    /* Cursed Ring */
                    if (power < 0)
                    {
                        /* Broken */
                        o_ptr->ident |= (IDENT_BROKEN);

                        /* Cursed */
                        o_ptr->ident |= (IDENT_CURSED);

                        /* Reverse pval */
                        o_ptr->pval = 0 - (o_ptr->pval);

                        break;
                    }

                    /* Rating boost for rings of speed that are not cursed */
                    else rating += 25;

                    /* Mention the item */
                    if (cheat_peek) object_mention(o_ptr);

                    break;
                }

                /* Searching */
                case SV_RING_SEARCHING:
                {
                    /* Bonus to searching */
                    o_ptr->pval = 1 + m_bonus(5, level);

                    /* Cursed */
                    if (power < 0)
                    {
                        /* Broken */
                        o_ptr->ident |= (IDENT_BROKEN);

                        /* Cursed */
                        o_ptr->ident |= (IDENT_CURSED);

                        /* Reverse pval */
                        o_ptr->pval = 0 - (o_ptr->pval);
                    }

                    break;
                }

                /* Searching */
                case SV_RING_AGGRAVATION:
                {
                    /* Cursed */
                    o_ptr->ident |= (IDENT_CURSED);

                    break;
                }


                /* Flames, Acid, Ice, Lightning */
                case SV_RING_FLAMES:
                case SV_RING_ACID:
                case SV_RING_ICE:
                case SV_RING_LIGHTNING:
                {
                    /* Bonus to armor class */
                    o_ptr->to_a = 5 + randint(5) + m_bonus(10, level) + (level / 10);
                    break;
                }

                /* Weakness, Stupidity */
                case SV_RING_WEAKNESS:
                case SV_RING_STUPIDITY:
                {
                    /* Broken */
                    o_ptr->ident |= (IDENT_BROKEN);

                    /* Cursed */
                    o_ptr->ident |= (IDENT_CURSED);

                    /* Penalize */
                    o_ptr->pval = 0 - (1 + m_bonus(5, level));

                    break;
                }

                /* WOE, Stupidity */
                case SV_RING_WOE:
                {
                    /* Broken */
                    o_ptr->ident |= (IDENT_BROKEN);

                    /* Cursed */
                    o_ptr->ident |= (IDENT_CURSED);

                    /* Penalize */
                    o_ptr->to_a = 0 - (5 + m_bonus(10, level));
                    o_ptr->pval = 0 - (1 + m_bonus(5, level));

                    break;
                }

                /* Ring of damage */
                case SV_RING_DAMAGE:
                {
                    /* Bonus to damage */
                    o_ptr->to_d = 5 + randint(3) + m_bonus(7, level) + (level / 10);

                    /* Cursed */
                    if (power < 0)
                    {
                        /* Broken */
                        o_ptr->ident |= (IDENT_BROKEN);

                        /* Cursed */
                        o_ptr->ident |= (IDENT_CURSED);

                        /* Reverse bonus */
                        o_ptr->to_d = 0 - (o_ptr->to_d);
                    }

                    break;
                }

                /* Ring of Accuracy */
                case SV_RING_ACCURACY:
                {
                    /* Bonus to hit */
                    o_ptr->to_h = 5 + randint(3) + m_bonus(7, level) + (level / 10);

                    /* Cursed */
                    if (power < 0)
                    {
                        /* Broken */
                        o_ptr->ident |= (IDENT_BROKEN);

                        /* Cursed */
                        o_ptr->ident |= (IDENT_CURSED);

                        /* Reverse tohit */
                        o_ptr->to_h = 0 - (o_ptr->to_h);
                    }

                    break;
                }

                /* Ring of Protection */
                case SV_RING_PROTECTION:
                {
                    /* Bonus to armor class */
                    o_ptr->to_a = 5 + randint(5) + m_bonus(10, level) + (level / 5);

                    /* Cursed */
                    if (power < 0)
                    {
                        /* Broken */
                        o_ptr->ident |= (IDENT_BROKEN);

                        /* Cursed */
                        o_ptr->ident |= (IDENT_CURSED);

                        /* Reverse toac */
                        o_ptr->to_a = 0 - (o_ptr->to_a);
                    }

                    break;
                }

                /* Ring of Slaying */
                case SV_RING_SLAYING:
                {
                    /* Bonus to damage and to hit */
                    o_ptr->to_d = randint(5) + m_bonus(5, level) + (level / 10);
                    o_ptr->to_h = randint(5) + m_bonus(5, level) + (level / 10);

                    /* Cursed */
                    if (power < 0)
                    {
                        /* Broken */
                        o_ptr->ident |= (IDENT_BROKEN);

                        /* Cursed */
                        o_ptr->ident |= (IDENT_CURSED);

                        /* Reverse bonuses */
                        o_ptr->to_h = 0 - (o_ptr->to_h);
                        o_ptr->to_d = 0 - (o_ptr->to_d);
                    }

                    break;
                }
                /* Ring of Slaying */
                case SV_RING_LORD_PROT_ACID:
                case SV_RING_LORD_PROT_FIRE:
                case SV_RING_LORD_PROT_COLD:
                {
                    /* Bonus plus to AC */
                    o_ptr->to_a = randint(5) + m_bonus(5, level) + (level / 10);

                    /* Cursed */
                    if (power < 0)
                    {
                        /* Broken */
                        o_ptr->ident |= (IDENT_BROKEN);

                        /* Cursed */
                        o_ptr->ident |= (IDENT_CURSED);

                        /* Reverse bonus */
                        o_ptr->to_h = 0 - (o_ptr->to_h);
                    }

                    break;
                }

            }

            break;
        }

        case TV_AMULET:
        {
            /* Analyze */
            switch (o_ptr->sval)
            {
                /* Amulet of wisdom/charisma/infravision */
                case SV_AMULET_WISDOM:
                case SV_AMULET_CHARISMA:
                case SV_AMULET_INFRAVISION:
                {
                    /* Stat bonus */
                    o_ptr->pval = 1 + m_bonus(5 + (level / 35), level);

                    /*cut it off at 6*/
                    if (o_ptr->pval > 6) o_ptr->pval = 6;

                    /* Cursed */
                    if (power < 0)
                    {
                        /* Broken */
                        o_ptr->ident |= (IDENT_BROKEN);

                        /* Cursed */
                        o_ptr->ident |= (IDENT_CURSED);

                        /* Reverse bonuses */
                        o_ptr->pval = 0 - (o_ptr->pval);
                    }

                    break;
                }

                /* Amulet of searching */
                case SV_AMULET_SEARCHING:
                {
                    o_ptr->pval = randint(5) + m_bonus(5, level);

                    /* Cursed */
                    if (power < 0)
                    {
                        /* Broken */
                        o_ptr->ident |= (IDENT_BROKEN);

                        /* Cursed */
                        o_ptr->ident |= (IDENT_CURSED);

                        /* Reverse bonuses */
                        o_ptr->pval = 0 - (o_ptr->pval);
                    }

                    break;
                }

                /* Amulet of ESP -- never cursed */
                case SV_AMULET_ESP:
                {
                    o_ptr->pval = randint(5) + m_bonus(5, level);

                    break;
                }

                /* Amulet of the Magi -- never cursed */
                case SV_AMULET_THE_MAGI:
                {
                    o_ptr->pval = 1 + m_bonus(3, level);
                    o_ptr->to_a = randint(5) + m_bonus(5, level);

                    /* Boost the rating */
                    rating += 10;

                    /* Mention the item */
                    if (cheat_peek) object_mention(o_ptr);

                    break;
                }

                /* Amulet of Devotion -- never cursed */
                case SV_AMULET_DEVOTION:
                {
                    o_ptr->pval = 1 + m_bonus(3, level);

                    /* Boost the rating */
                    rating += 10;

                    /* Mention the item */
                    if (cheat_peek) object_mention(o_ptr);

                    break;
                }

                /* Amulet of Weaponmastery -- never cursed */
                case SV_AMULET_WEAPONMASTERY:
                {
                    o_ptr->to_h = 1 + m_bonus(4, level);
                    o_ptr->to_d = 1 + m_bonus(4, level);
                    o_ptr->pval = 1 + m_bonus(2, level);

                    /* Boost the rating */
                    rating += 10;

                    /* Mention the item */
                    if (cheat_peek) object_mention(o_ptr);

                    break;
                }

                /* Amulet of Trickery -- never cursed */
                case SV_AMULET_TRICKERY:
                {
                    o_ptr->pval = randint(1) + m_bonus(3, level);

                    /* Boost the rating */
                    rating += 10;

                    /* Mention the item */
                    if (cheat_peek) object_mention(o_ptr);

                    break;
                }

                /* Amulet of Doom -- always cursed */
                case SV_AMULET_DOOM:
                case SV_AMULET_WOE:
                {
                    /* Broken */
                    o_ptr->ident |= (IDENT_BROKEN);

                    /* Cursed */
                    o_ptr->ident |= (IDENT_CURSED);

                    /* Penalize */
                    o_ptr->pval = 0 - (randint(5) + m_bonus(5, level));
                    o_ptr->to_a = 0 - (randint(5) + m_bonus(5, level));

                    break;
                }
            }

            break;
        }
    }
}


/*
 * Apply magic to an item known to be "boring"
 *
 * Hack -- note the special code for various items
 */
static void a_m_aux_4(object_type *o_ptr, int level, int power, bool good, bool great)
{
    object_kind *k_ptr = &k_info[o_ptr->k_idx];

    /* Unused parameters */
    (void)level;
    (void)power;

    /* Apply magic (good or bad) according to type */
    switch (o_ptr->tval)
    {
        case TV_LIGHT:
        {
            /* Hack -- Torches -- random fuel */
            if (o_ptr->sval == SV_LIGHT_TORCH)
            {
                o_ptr->timeout = DEFAULT_TORCH;
            }

            /* Hack -- Lanterns -- random fuel */
            else if (o_ptr->sval == SV_LIGHT_LANTERN)
            {
                o_ptr->timeout = DEFAULT_LAMP;
            }

            break;
        }

        case TV_WAND:
        {
            /* Hack -- charge wands */
            o_ptr->pval = charge_wand(o_ptr, 100);

            break;
        }

        case TV_STAFF:
        {
            /* Hack -- charge staffs */
            o_ptr->pval = charge_staff(o_ptr, 100);

            break;
        }

        case TV_ROD:
        {
            /* Transfer the pval. */
            o_ptr->pval = k_ptr->pval;
            break;
        }

        case TV_CHEST:
        {
            /* Hack -- chest level is fixed at player level at time of generation */
            o_ptr->pval = object_level;

            /*chest created with good flag get a level boost*/
            if (good) o_ptr->pval += 5;

            /*chest created with great flag also gets a level boost*/
            if (great) o_ptr->pval += 5;

            /*chests now increase level rating*/
            rating += 5;

            /* Don't exceed "chest level" of 110 */
            if (o_ptr->pval > 110) o_ptr->pval = 110;

            /*a minimum pval of 1, or else it will be empty in the town*/
            if (o_ptr->pval < 1) o_ptr->pval = 1;

            /*a guild reward chest shouldn't be trapped*/
            if (object_generation_mode == OB_GEN_MODE_QUEST) o_ptr->pval = (0 - o_ptr->pval);

            /*save the chest theme in xtra1, used in chest death*/
            o_ptr->xtra1 = choose_chest_contents ();

            break;
        }
    }
}

void object_into_artifact(object_type *o_ptr, artifact_type *a_ptr)
{

    /* Extract the other fields */
    o_ptr->pval = a_ptr->pval;
    o_ptr->ac = a_ptr->ac;
    o_ptr->dd = a_ptr->dd;
    o_ptr->ds = a_ptr->ds;
    o_ptr->to_a = a_ptr->to_a;
    o_ptr->to_h = a_ptr->to_h;
    o_ptr->to_d = a_ptr->to_d;
    o_ptr->weight = a_ptr->weight;

    /* Hack - mark the depth of artifact creation for the notes function
     * probably a bad idea to use this flag.  It is used when making ego-items,
     * which currently fails when an item is an artifact.  If this was changed
     * this would be the cause of some major bugs.
     */
    if (p_ptr->depth)
    {
        o_ptr->xtra1 = p_ptr->depth;
    }

    /*hack - mark chest items with a special level so the notes patch
     * knows where it is coming from.
     */
    else if (object_generation_mode == OB_GEN_MODE_CHEST) o_ptr->xtra1 = CHEST_LEVEL;
    else if (object_generation_mode == OB_GEN_MODE_QUEST) o_ptr->xtra1 = QUEST_LEVEL;

    /* Hack -- extract the "broken" flag */
    if (!a_ptr->cost) o_ptr->ident |= (IDENT_BROKEN);

    /* Hack -- extract the "cursed" flag */
    if (a_ptr->a_flags3 & (TR3_LIGHT_CURSE)) o_ptr->ident |= (IDENT_CURSED);
    if (a_ptr->a_flags3 & (TR3_HEAVY_CURSE)) o_ptr->ident |= (IDENT_CURSED);
    if (a_ptr->a_flags3 & (TR3_PERMA_CURSE)) o_ptr->ident |= (IDENT_CURSED);

    /* Hack -- extract the "perfect balance" flag */
    if (a_ptr->a_flags3 & (TR3_PERFECT_BALANCE)) o_ptr->ident |= (IDENT_PERFECT_BALANCE);
}

void apply_ego_item_magic(object_type *o_ptr, int lev)
{
    ego_item_type *e_ptr = &e_info[o_ptr->ego_num];

    /* Examine the item */
    o_ptr->update_object_flags();

    /* Extra powers */
    if (e_ptr->xtra)
    {
        byte size = 0;
        byte max_flags;
        u32b flag, base = 0;
        u32b flag_cur = 0;
        int x;

        /*Mark what type of extra feature we have here*/
        o_ptr->xtra1 = e_ptr->xtra;

        switch (o_ptr->xtra1)
        {
            case OBJECT_XTRA_STAT_SUSTAIN:
            {
                size = OBJECT_XTRA_SIZE_SUSTAIN;
                base = OBJECT_XTRA_BASE_SUSTAIN;
                flag_cur = o_ptr->obj_flags_2;
                break;
            }

            case OBJECT_XTRA_TYPE_HIGH_RESIST:
            {
                size = OBJECT_XTRA_SIZE_HIGH_RESIST;
                base = OBJECT_XTRA_BASE_HIGH_RESIST;
                flag_cur = o_ptr->obj_flags_2;
                break;
            }

            case OBJECT_XTRA_TYPE_POWER:
            {
                size = OBJECT_XTRA_SIZE_POWER;
                base = OBJECT_XTRA_BASE_POWER;
                flag_cur = o_ptr->obj_flags_3;
                break;
            }
            case OBJECT_XTRA_TYPE_IMMUNITY:
            {
                size = OBJECT_XTRA_SIZE_IMMUNITY;
                base = OBJECT_XTRA_BASE_IMMUNITY;
                flag_cur = o_ptr->obj_flags_2;
                break;
            }
            case OBJECT_XTRA_TYPE_STAT_ADD:
            {
                size = OBJECT_XTRA_SIZE_STAT_ADD;
                base = OBJECT_XTRA_BASE_STAT_ADD;
                flag_cur = o_ptr->obj_flags_1;

                /* Calculate Stat bonus */
                o_ptr->pval = 1 + m_bonus(5 + (lev / 35), lev);

                /*cut it off at 6*/
                if (o_ptr->pval > 6) o_ptr->pval = 6;
                break;
            }
            case OBJECT_XTRA_TYPE_SLAY:
            {
                size = OBJECT_XTRA_SIZE_SLAY;
                base = OBJECT_XTRA_BASE_SLAY;
                flag_cur = o_ptr->obj_flags_1;
                break;
            }
            case OBJECT_XTRA_TYPE_KILL:
            {
                size = OBJECT_XTRA_SIZE_KILL;
                base = OBJECT_XTRA_BASE_KILL;
                flag_cur = o_ptr->obj_flags_1;
                break;
            }
            case OBJECT_XTRA_TYPE_BRAND:
            {
                size = OBJECT_XTRA_SIZE_BRAND;
                base = OBJECT_XTRA_BASE_BRAND;
                flag_cur = o_ptr->obj_flags_1;
                break;
            }
            case OBJECT_XTRA_TYPE_LOW_RESIST:
            {
                size = OBJECT_XTRA_SIZE_LOW_RESIST;
                base = OBJECT_XTRA_BASE_LOW_RESIST;
                flag_cur = o_ptr->obj_flags_2;
                break;
            }
            case OBJECT_XTRA_TYPE_NATIVE:
            {
                size = OBJECT_XTRA_SIZE_NATIVE;
                base = OBJECT_XTRA_BASE_NATIVE;
                flag_cur = o_ptr->obj_flags_native;
                break;
            }
        }

        /*start with a clean slate*/
        o_ptr->xtra2 = 0;

        /* Mark when there are no more flags to give */
        max_flags = size;

        /*Check to see if any object already has some of these flags*/
        for (x = 0; x < size; x++)
        {
            /*Go to the flag*/
            u32b current_bit = base << x;

            /* Do we have this flag already?*/
            if (flag_cur & current_bit)
            {
                /*Mark it in xtra2*/
                flag = 0x00000001L << x;

                /* Assign the flag */
                o_ptr->xtra2 |= flag;

                /* Note how many we have left */
                max_flags--;
            }


        }

        while (max_flags)
        {
            /* Make a random flag */
            flag = 0x00000001L << rand_int(size);

            /* Duplicated flag? */
            if (o_ptr->xtra2 & flag)	continue;

            /* Assign the flag */
            o_ptr->xtra2 |= flag;

            /* Note how many we have left */
            max_flags--;

            /* Another flag sometimes? */
            if (!one_in_(EXTRA_FLAG_CHANCE)) break;

        }
    }

    /* Ego-item throwing weapons may sometimes be perfectly
     * balanced.
     */
    if ((o_ptr->obj_flags_3 & (TR3_THROWING)) && (randint(3) == 1))
    {
        (o_ptr->ident |= IDENT_PERFECT_BALANCE);
    }

    if (o_ptr->obj_flags_3 & (TR3_PERFECT_BALANCE))
    {
        (o_ptr->ident |= IDENT_PERFECT_BALANCE);
    }

    /* Hack -- acquire "broken" flag */
    if (!e_ptr->cost) o_ptr->ident |= (IDENT_BROKEN);

    /* Hack -- acquire "cursed" flag */
    if (e_ptr->e_flags3 & (TR3_LIGHT_CURSE)) o_ptr->ident |= (IDENT_CURSED);
    if (e_ptr->e_flags3 & (TR3_HEAVY_CURSE)) o_ptr->ident |= (IDENT_CURSED);
    if (e_ptr->e_flags3 & (TR3_PERMA_CURSE)) o_ptr->ident |= (IDENT_CURSED);

    /* Hack -- apply extra penalties if needed */
    if (o_ptr->is_cursed() || o_ptr->is_broken())
    {
        /* Hack -- obtain bonuses */
        if (e_ptr->max_to_h > 0) o_ptr->to_h -= randint(e_ptr->max_to_h);
        if (e_ptr->max_to_d > 0) o_ptr->to_d -= randint(e_ptr->max_to_d);
        if (e_ptr->max_to_a > 0) o_ptr->to_a -= randint(e_ptr->max_to_a);

        /* Hack -- obtain pval, unless one has already been assigned */
        if ((e_ptr->max_pval > 0) && (o_ptr->pval == 0))
              o_ptr->pval -= randint(e_ptr->max_pval);
    }

    /* Hack -- apply extra bonuses if needed */
    else
    {
        /* Hack -- obtain bonuses */
        if (e_ptr->max_to_h > 0) o_ptr->to_h += randint(e_ptr->max_to_h);
        if (e_ptr->max_to_d > 0) o_ptr->to_d += randint(e_ptr->max_to_d);
        if (e_ptr->max_to_a > 0) o_ptr->to_a += randint(e_ptr->max_to_a);

        /* Hack -- obtain pval */
        if (e_ptr->max_pval > 0)
        {
            /*Handle stat pvals differently*/
            if (e_ptr->e_flags1 & TR1_ALL_STATS)
            {
                byte bonus = m_bonus(e_ptr->max_pval, lev);

                /*min of 1*/
                if (bonus < 1) bonus = 1;

                o_ptr->pval += bonus;

                /*hard limit*/
                if(o_ptr->pval > 6) o_ptr->pval = 6;
            }
            else o_ptr->pval += randint(e_ptr->max_pval);
        }
    }

    /* Hack -- apply rating bonus */
    rating += e_ptr->rating;

    /* Cheat -- describe the item */
    if (cheat_peek) object_mention(o_ptr);
}

/*
 * Complete the "creation" of an object by applying "magic" to the item
 *
 * This includes not only rolling for random bonuses, but also putting the
 * finishing touches on ego-items and artifacts, giving charges to wands and
 * staffs, giving fuel to lites, and placing traps on chests.
 *
 * In particular, note that "Instant Artifacts", if "created" by an external
 * routine, must pass through this function to complete the actual creation.
 *
 * The base "chance" of the item being "good" increases with the "level"
 * parameter, which is usually derived from the dungeon level, being equal
 * to the level plus 10, up to a maximum of 75.  If "good" is true, then
 * the object is guaranteed to be "good".  If an object is "good", then
 * the chance that the object will be "great" (ego-item or artifact), also
 * increases with the "level", being equal to half the level, plus 5, up to
 * a maximum of 20.  If "great" is true, then the object is guaranteed to be
 * "great".  At dungeon level 65 and below, 15/100 objects are "great".
 *
 * If the object is not "good", there is a chance it will be "cursed", and
 * if it is "cursed", there is a chance it will be "broken".  These chances
 * are related to the "good" / "great" chances above.
 *
 * Otherwise "normal" rings and amulets will be "good" half the time and
 * "cursed" half the time, unless the ring/amulet is always good or cursed.
 *
 * If "okay" is true, and the object is going to be "great", then there is
 * a chance that an artifact will be created.  This is true even if both the
 * "good" and "great" arguments are false.  Objects which are forced "great"
 * get three extra "attempts" to become an artifact.
 */
void apply_magic(object_type *o_ptr, int lev, bool okay, bool good, bool great, bool interesting)
{
    int i, rolls, test_good, test_great, power;

    /* Maximum "level" for various things */
    if (lev > MAX_DEPTH - 1) lev = MAX_DEPTH - 1;

    /* Base chance of being "good" */
    /* Moria is a base +15 */
    if (game_mode == GAME_NPPMORIA) test_good = lev + 15;
    else test_good = lev + 10;

    /* Maximal chance of being "good" */
    if (game_mode == GAME_NPPMORIA)
    {
        if (test_good > 70) test_good = 70;
    }
    else if (test_good > 75) test_good = 75;

    /* Base chance of being "great", different for Moria */
    if (game_mode == GAME_NPPMORIA) test_great = test_good / 6;
    else test_great = test_good / 2;

    /* Maximal chance of being "great" */
    if (test_great > 20) test_great = 20;

    /* Assume normal */
    power = 0;

    /* Roll for "good", notice that great items don't necessarily need the good flag */
    if ((good) || (great) || (rand_int(100) < test_good))
    {
        /* Assume "good" */
        power = 1;

        /* Roll for "great" */
        if (great || (rand_int(100) < test_great)) power = 2;
    }

    /* Roll for "cursed if not opening a chest" */
    else if ((rand_int(100) < test_good) && (!interesting) &&
             (object_generation_mode != OB_GEN_MODE_CHEST))
    {
        /* Assume "cursed" */
        power = -1;

        /* Roll for "broken" */
        if (rand_int(100) < test_great) power = -2;
    }

    /* Assume no rolls */
    rolls = 0;

    /* Get one roll if excellent */
    if (power >= 2) rolls = 1;

    /*
     * Get four rolls if good and great flags are true,
     * only 1 for quests since they are so repetitive
     */
    if ((good) && (great))
    {
        if (object_generation_mode == OB_GEN_MODE_QUEST) rolls = 1;
        else rolls = 4;
    }

    /* Get no rolls if not allowed */
    if (!okay || o_ptr->art_num) rolls = 0;

    /* Roll for artifacts if allowed */
    for (i = 0; i < rolls; i++)
    {
        /* Roll for an artifact */
        if (make_artifact(o_ptr)) break;
    }

    /* Hack -- analyze artifacts */
    if (o_ptr->art_num)
    {
        artifact_type *a_ptr = &a_info[o_ptr->art_num];

        /* Hack -- Mark the artifact as "created" */
        a_ptr->a_cur_num = 1;

        object_into_artifact(o_ptr, a_ptr);

        /* Mega-Hack -- increase the rating */
        rating += 10;

        /* Mega-Hack -- increase the rating again */
        if (a_ptr->cost > 50000L) rating += 10;

        /* Set the good item flag */
        good_item_flag = TRUE;

        /* Cheat -- peek at the item */
        if (cheat_peek) object_mention(o_ptr);

        // Make sure all flags are up-to-date
        o_ptr->update_object_flags();

        /* Done */
        return;
    }


    /* Apply magic */
    switch (o_ptr->tval)
    {
        case TV_DIGGING:
        case TV_HAFTED:
        case TV_POLEARM:
        case TV_SWORD:
        case TV_BOW:
        case TV_SHOT:
        case TV_ARROW:
        case TV_BOLT:
        {
            if ((power > 1) || (power < -1))
            {
                int ego_power;

                ego_power = make_ego_item(o_ptr, (bool)(good || great), great);

                if (ego_power) power = ego_power;
            }

            if (power) a_m_aux_1(o_ptr, lev, power);

            break;
        }

        case TV_HARD_ARMOR:
        case TV_SOFT_ARMOR:
        case TV_SHIELD:
        case TV_HELM:
        case TV_CROWN:
        case TV_CLOAK:
        case TV_GLOVES:
        case TV_BOOTS:
        {
            if ((power > 1) || (power < -1))
            {
                int ego_power;

                ego_power = make_ego_item(o_ptr, (bool)(good || great), great);

                if (ego_power) power = ego_power;
            }

            if (power) a_m_aux_2(o_ptr, lev, power);

            break;
        }

        /*Dragon Armor or shield is always an ego-item*/
        case TV_DRAG_ARMOR:
        case TV_DRAG_SHIELD:
        {
            /*Always great*/
            power = 2;

            /*Continue until success*/
            while (!make_ego_item(o_ptr, TRUE, TRUE)) continue;

            /*add the power*/
            a_m_aux_2(o_ptr, lev, power);
        }


        case TV_RING:
        case TV_AMULET:
        {
            if (!power && !interesting && (one_in_(2))) power = -1;
            a_m_aux_3(o_ptr, lev, power);
            break;
        }

        case TV_LIGHT:
        {
            if ((power > 1) || (power < -1))
            {
                make_ego_item(o_ptr, (bool)(good || great), great);
            }

            /* Fuel it */
            a_m_aux_4(o_ptr, lev, power, good, great);
            break;
        }

        case TV_MAGIC_BOOK:
        case TV_PRAYER_BOOK:
        case TV_DRUID_BOOK:
        {
            if ((power > 1) || (power < -1))
            {
                make_ego_item(o_ptr, (bool)(good || great), great);
            }

            a_m_aux_4(o_ptr, lev, power, good, great);
            break;
        }

        default:
        {
            a_m_aux_4(o_ptr, lev, power, good, great);
            break;
        }
    }


    /* Hack -- analyze ego-items */
    if (o_ptr->ego_num)
    {
        apply_ego_item_magic(o_ptr, lev);

        // Make sure all flags are up-to-date
        o_ptr->update_object_flags();

        /* Done */
        return;
    }

    /* Examine real objects */
    if (o_ptr->k_idx)
    {
        object_kind *k_ptr = &k_info[o_ptr->k_idx];

        /* Hack -- acquire "broken" flag */
        if (!k_ptr->cost) o_ptr->ident |= (IDENT_BROKEN);

        /* Hack -- acquire "cursed" flag */
        if (k_ptr->k_flags3 & (TR3_LIGHT_CURSE)) o_ptr->ident |= (IDENT_CURSED);
        if (k_ptr->k_flags3 & (TR3_HEAVY_CURSE)) o_ptr->ident |= (IDENT_CURSED);
        if (k_ptr->k_flags3 & (TR3_PERMA_CURSE)) o_ptr->ident |= (IDENT_CURSED);
    }

    // Make sure all flags are up-to-date
    o_ptr->update_object_flags();
}

/*
 * Hack -- determine if a template suitabel for the dungeon.
 * This is just to eliminate store_only items like wine and hard biscuits
  */
static bool kind_is_dungeon(int k_idx)
{
    object_kind *k_ptr = &k_info[k_idx];

    if (k_ptr->k_flags3 & (TR3_STORE_ONLY)) return (FALSE);

    return (TRUE);
}


/*
 * Hack -- determine if a template is suitable for a general store.
 *
 * Flag needs to be specified in the object.txt file.
 */
static bool kind_is_gen_store(int k_idx)
{
    object_kind *k_ptr = &k_info[k_idx];

    /* No "worthless" items */
    if (k_ptr->cost < 0) return (FALSE);

    if (k_ptr->k_store & (SF1_GENERAL_STORE)) return (TRUE);

    /* Analyze the item type */
    switch (k_ptr->tval)
    {
        case TV_FOOD:
        case TV_LIGHT:
        case TV_FLASK:
        case TV_SPIKE:
        case TV_SHOT:
        case TV_ARROW:
        case TV_BOLT:
        case TV_DIGGING:
        case TV_CLOAK:
        {
            if (allow_altered_inventory) return (TRUE);
            break;
        }
        default: break;
    }

    /* Assume not good */
    return (FALSE);
}



/*
 * Hack -- determine if a template is suitable for the armoury.
 *
 * Flag needs to be specified in the object.txt file.
 */
static bool kind_is_armoury(int k_idx)
{
    object_kind *k_ptr = &k_info[k_idx];

    /* No "worthless" items */
    if (k_ptr->cost < 0) return (FALSE);

    if (k_ptr->k_store & (SF1_ARMORY)) return (TRUE);

    /* Analyze the item type */
    switch (k_ptr->tval)
    {
        case TV_HARD_ARMOR:
        case TV_SOFT_ARMOR:
        case TV_SHIELD:
        case TV_BOOTS:
        case TV_GLOVES:
        case TV_HELM:
        {
            if (allow_altered_inventory) return (TRUE);
            break;
        }
        default: break;
    }

    /* Assume not good */
    return (FALSE);
}

/*
 * Hack -- determine if a template is suitable for the weaponsmith.
 *
 * Flag needs to be specified in the object.txt file.
 */
static bool kind_is_weaponsmith(int k_idx)
{
    object_kind *k_ptr = &k_info[k_idx];

    /* No "worthless" items */
    if (k_ptr->cost < 0) return (FALSE);

    if (k_ptr->k_store & (SF1_WEAPONSMITH)) return (TRUE);

    /* Analyze the item type */
    switch (k_ptr->tval)
    {
        case TV_SWORD:
        case TV_HAFTED:
        case TV_POLEARM:
        case TV_BOW:
        case TV_SHOT:
        case TV_ARROW:
        case TV_BOLT:
        {
            if (allow_altered_inventory) return (TRUE);
            break;
        }
        default: break;
    }

    /* Assume not suitable */
    return (FALSE);
}

/*
 * Hack -- determine if a template is suitable for the temple.
 *
 * Flag needs to be specified in the object.txt file.
 */
static bool kind_is_temple(int k_idx)
{
    object_kind *k_ptr = &k_info[k_idx];

    /* No "worthless" items */
    if (k_ptr->cost < 0) return (FALSE);

    if (k_ptr->k_store & (SF1_TEMPLE)) return (TRUE);

    /* Analyze the item type */
    switch (k_ptr->tval)
    {
        case TV_HAFTED:
        case TV_SCROLL:
        case TV_POTION:
        {
            if (allow_altered_inventory) return (TRUE);
            break;
        }
        default: break;
    }


    /* Assume not suitable */
    return (FALSE);
}



/*
 * Hack -- determine if a template is suitable for the alchemy shop.
 *
 *  Flag needs to be specified in the object.txt file.
 *
 * Note that this test only applies to the object *kind*, so it is
 * possible to cause the object to be cursed.
 */
static bool kind_is_alchemy(int k_idx)
{
    object_kind *k_ptr = &k_info[k_idx];

    /* No "worthless" items */
    if (k_ptr->cost < 0) return (FALSE);

    if (k_ptr->k_store & (SF1_ALCHEMIST)) return (TRUE);

    /* Analyze the item type */
    switch (k_ptr->tval)
    {
        case TV_SCROLL:
        case TV_POTION:
        {
            if (allow_altered_inventory) return (TRUE);
            break;
        }
        default: break;
    }

    /* Assume not suitable */
    return (FALSE);
}

/*
 * Hack -- determine if a template is suitable for the magic_shop.
 *
 * Flag needs to be specified in the object.txt file.
 *
 */
static bool kind_is_magic_shop(int k_idx)
{
    object_kind *k_ptr = &k_info[k_idx];

    /* No "worthless" items */
    if (k_ptr->cost < 0) return (FALSE);

    /* Analyze the item type */
    switch (k_ptr->tval)
    {
        case TV_RING:
        case TV_AMULET:
        case TV_WAND:
        case TV_STAFF:
        {
            if (allow_altered_inventory) return (TRUE);
            break;
        }
        default: break;
    }

    if (k_ptr->k_store & (SF1_MAGIC_SHOP)) return (TRUE);

    /* Assume not suitable */
    return (FALSE);
}


/*
 * Hack -- determine if a template is suitable for the black_market.
 *
 * Note that this test only applies to the object *kind*, so it is
 * possible to cause the object to less valuable later.
 */
static bool kind_is_black_market(int k_idx)
{
    object_kind *k_ptr = &k_info[k_idx];

    if (k_ptr->k_store & (SF1_BLACK_MARKET)) return (TRUE);

    return (FALSE);
}

/*
 * Hack -- determine if a template is suitable for the magic_shop.
 *
 * Note that this test only applies to the object *kind*, so it is
 * possible to cause the object to be cursed.
 */
static bool kind_is_bookshop(int k_idx)
{
    object_kind *k_ptr = &k_info[k_idx];

    /* No "worthless" items */
    if (k_ptr->cost < 0) return (FALSE);

    if (k_ptr->k_store & (SF1_BOOKSHOP)) return (TRUE);

    /* Analyze the item type */
    switch (k_ptr->tval)
    {
        case TV_MAGIC_BOOK:
        case TV_PRAYER_BOOK:
        case TV_DRUID_BOOK:
        {
            if (allow_altered_inventory) return (TRUE);
            break;
        }
        default: break;
    }

    /* Assume not suitable */
    return (FALSE);
}

/*
 * Hack -- determine if a template is "a dungeon spellbook".
 */
static bool kind_is_dungeon_spellbook(int k_idx)
{
    object_kind *k_ptr = &k_info[k_idx];

    /* Analyze the item type */
    switch (k_ptr->tval)
    {
        /* Books **/

        case TV_MAGIC_BOOK:
        case TV_DRUID_BOOK:
        case TV_PRAYER_BOOK:
        {
            if (k_ptr->sval >= SV_BOOK_MIN_GOOD) return (TRUE);
            return(FALSE);
        }

    }
    return(FALSE);

}

/*
 * Hack -- determine if a template is "a priestly dungeon prayerbook".
 */
static bool kind_is_dungeon_prayer_book(int k_idx)
{
    object_kind *k_ptr = &k_info[k_idx];

    /* Analyze the item type */
    switch (k_ptr->tval)
    {
        /* Books **/

        case TV_PRAYER_BOOK:
        {
            if (k_ptr->sval >= SV_BOOK_MIN_GOOD) return (TRUE);
            return(FALSE);
        }

    }
    return(FALSE);

}

/*
 * Hack -- determine if a template is "a druid dungeon spellbook".
 *
 */
static bool kind_is_dungeon_druid_book(int k_idx)
{
    object_kind *k_ptr = &k_info[k_idx];

    /* Analyze the item type */
    switch (k_ptr->tval)
    {
        /* Books **/

        case TV_DRUID_BOOK:
        {
            if (k_ptr->sval >= SV_BOOK_MIN_GOOD) return (TRUE);
            return(FALSE);
        }

    }
    return(FALSE);

}


/*
 * Hack -- determine if a template is "a mage dungeon spellbook".
 *
 */
static bool kind_is_dungeon_magic_book(int k_idx)
{
    object_kind *k_ptr = &k_info[k_idx];

    /* Analyze the item type */
    switch (k_ptr->tval)
    {
        /* Books **/

        case TV_MAGIC_BOOK:
        {
            if (k_ptr->sval >= SV_BOOK_MIN_GOOD) return (TRUE);
            return(FALSE);
        }

    }
    return (FALSE);
}


/*
 * Hack -- determine if a template is "great".
 *
 * Note that this test only applies to the object *kind*, so it is
 * possible to choose a kind which is "great", and then later cause
 * the actual object to be cursed.  We do explicitly forbid objects
 * which are known to be boring or which start out somewhat damaged.
 */
static bool kind_is_great(int k_idx)
{
    object_kind *k_ptr = &k_info[k_idx];

    /* Analyze the item type */
    switch (k_ptr->tval)
    {
        /* Armor -- great unless damaged */
        case TV_HARD_ARMOR:
        case TV_SOFT_ARMOR:
        case TV_DRAG_ARMOR:
        case TV_DRAG_SHIELD:
        case TV_SHIELD:
        case TV_CLOAK:
        case TV_BOOTS:
        case TV_GLOVES:
        case TV_HELM:
        case TV_CROWN:
        {
            if (k_ptr->to_a < 0) return (FALSE);
            return (TRUE);
        }

        /* Weapons -- great unless damaged */
        case TV_BOW:
        case TV_SWORD:
        case TV_HAFTED:
        case TV_POLEARM:
        {
            if (k_ptr->to_h < 0) return (FALSE);
            if (k_ptr->to_d < 0) return (FALSE);
            return (TRUE);
        }

        /* Ammo -- Arrows/Bolts are great, unless quest item */
        case TV_BOLT:
        case TV_ARROW:
        case TV_SHOT:
        {
            if ((object_generation_mode == OB_GEN_MODE_QUEST) ||
                (object_generation_mode == OB_GEN_MODE_RANDART)) return (FALSE);
            return (TRUE);
        }

        /* Rings -- Rings of Speed are great */
        case TV_RING:
        {
            if (k_ptr->sval == SV_RING_SPEED) return (TRUE);
            return (FALSE);
        }

        /*scrolls of "*Acquirement*" are great*/
        case TV_SCROLL:
        {
            if (k_ptr->sval == SV_SCROLL_STAR_ACQUIREMENT) return (TRUE);
            if ((k_ptr->sval == SV_SCROLL_CREATE_RANDART) &&
                (!birth_no_xtra_artifacts))   return (TRUE);
            return (FALSE);
        }

        /*
         * Stat gain potions can be great at the lower levels.
         */

        case TV_POTION:
        {
            switch (k_ptr->sval)
            {
                case SV_POTION_INC_STR:
                case SV_POTION_INC_INT:
                case SV_POTION_INC_WIS:
                case SV_POTION_INC_CON:
                case SV_POTION_INC_DEX:
                case SV_POTION_AUGMENTATION:
                {
                    if (object_generation_mode != OB_GEN_MODE_QUEST) return (FALSE);

                    if (object_level < (k_ptr->k_level - 5)) return (TRUE);
                    return (FALSE);
                }
                default: break;
            }
            return (FALSE);
        }

        /* Chests -- Chests are great, except for quests.*/
        case TV_CHEST:
        {
            if ((object_generation_mode == OB_GEN_MODE_QUEST) ||
                (object_generation_mode == OB_GEN_MODE_RANDART))  return (FALSE);
            return (TRUE);
        }
        default:  break;
    }

    /* Assume not great */
    return (FALSE);
}


/*
 * Hack -- determine if a template is a chest.
 *
 */
static bool kind_is_chest(int k_idx)
{
    object_kind *k_ptr = &k_info[k_idx];

    /* Analyze the item type */
    switch (k_ptr->tval)
    {
        /* Chests -- */
        case TV_CHEST:
        {
            return (TRUE);
        }

    }

    /* Assume not chest */
    return (FALSE);
}

/*
 * Hack -- determine if a template is footwear.
 *
 */
static bool kind_is_boots(int k_idx)
{
    object_kind *k_ptr = &k_info[k_idx];

    /* Analyze the item type */
    switch (k_ptr->tval)
    {
        /* footwear -- */
        case TV_BOOTS:
        {
            return (TRUE);
        }

    }

    /* Assume not footwear */
    return (FALSE);
}

/*
 * Hack -- determine if a template is headgear.
 *
 */
static bool kind_is_headgear(int k_idx)
{
    object_kind *k_ptr = &k_info[k_idx];

    /* Analyze the item type */
    switch (k_ptr->tval)
    {
        /* Headgear -- Suitable unless damaged */
        case TV_HELM:
        case TV_CROWN:
        {
            if (k_ptr->to_a < 0) return (FALSE);
            return (TRUE);
        }
    }

    /* Assume not headgear */
    return (FALSE);
}

/*
 * Hack -- determine if a template is wearable armor (all types).
 *
 */
static bool kind_is_armor(int k_idx)
{
    object_kind *k_ptr = &k_info[k_idx];

    /* Analyze the item type */
    switch (k_ptr->tval)
    {
        /* Armor -- suitable  unless damaged */
        case TV_HARD_ARMOR:
        case TV_SOFT_ARMOR:
        case TV_DRAG_ARMOR:
        case TV_DRAG_SHIELD:
        case TV_SHIELD:
        case TV_CLOAK:
        case TV_BOOTS:
        case TV_GLOVES:
        case TV_HELM:
        case TV_CROWN:
        {
            if (k_ptr->to_a < 0) return (FALSE);
            return (TRUE);
        }
    }

    /* Assume not armor */
    return (FALSE);
}

/*
 * Hack -- determine if a template is wearable armor (all types).
 *
 */
static bool kind_is_moria_armor_body(int k_idx)
{
    object_kind *k_ptr = &k_info[k_idx];

    /* Analyze the item type */
    switch (k_ptr->tval)
    {
        /* Armor -- suitable  unless damaged */
        case TV_HARD_ARMOR:
        case TV_SOFT_ARMOR:
        case TV_SHIELD:
        {
            if (k_ptr->to_a < 0) return (FALSE);
            return (TRUE);
        }
    }

    /* Assume not armor */
    return (FALSE);
}

/*
 * Hack -- determine if a template is wearable armor (all types).
 *
 */
static bool kind_is_moria_armor_other(int k_idx)
{
    object_kind *k_ptr = &k_info[k_idx];

    /* Analyze the item type */
    switch (k_ptr->tval)
    {
        /* Armor -- suitable  unless damaged */
        case TV_CLOAK:
        case TV_BOOTS:
        case TV_GLOVES:
        case TV_HELM:
        case TV_CROWN:
        {
            if (k_ptr->to_a < 0) return (FALSE);
            return (TRUE);
        }
    }

    /* Assume not armor */
    return (FALSE);
}


/*
 * Hack -- determine if a template is armor.
 *
 */
static bool kind_is_body_armor(int k_idx)
{
    object_kind *k_ptr = &k_info[k_idx];

    /* Analyze the item type */
    switch (k_ptr->tval)
    {
        /* Armor -- suitable  unless damaged */
        case TV_HARD_ARMOR:
        case TV_SOFT_ARMOR:
        case TV_DRAG_ARMOR:
        {
            if (k_ptr->to_a < 0) return (FALSE);
            return (TRUE);
        }
    }

    /* Assume not armor */
    return (FALSE);
}

/*
 * Hack -- determine if a template is Dragon Scale Mail or shield.
 *
 */
static bool kind_is_dragarmor(int k_idx)
{
    object_kind *k_ptr = &k_info[k_idx];

    /* Analyze the item type */
    switch (k_ptr->tval)
    {
        /* Dragon Armor -- suitable  unless damaged */
        case TV_DRAG_ARMOR:
        case TV_DRAG_SHIELD:
        {
            if (k_ptr->to_a < 0) return (FALSE);
            return (TRUE);
        }
    }

    /* Assume not Dragon Scale Mail */
    return (FALSE);
}

/*
 * Hack -- determine if a template is gloves.
 *
 */
static bool kind_is_gloves(int k_idx)
{
    object_kind *k_ptr = &k_info[k_idx];

    /* Analyze the item type */
    switch (k_ptr->tval)
    {
        /* Gloves -- suitable  unless damaged */
        case TV_GLOVES:
        {
            if (k_ptr->to_a < 0) return (FALSE);
            return (TRUE);
        }
    }

    /* Assume not suitable  */
    return (FALSE);
}

/*
 * Hack -- determine if a template is a cloak.
 *
 */
static bool kind_is_cloak(int k_idx)
{
    object_kind *k_ptr = &k_info[k_idx];

    /* Analyze the item type */
    switch (k_ptr->tval)
    {
        /* Cloaks -- suitable  unless damaged */

        case TV_CLOAK:
        {
            if (k_ptr->to_a < 0) return (FALSE);
            return (TRUE);
        }
    }

    /* Assume not a suitable  */
    return (FALSE);
}

/*
 * Hack -- determine if a template is a shield.
 *
 */
static bool kind_is_shield(int k_idx)
{
    object_kind *k_ptr = &k_info[k_idx];

    /* Analyze the item type */
    switch (k_ptr->tval)
    {
        /* shield -- suitable  Unless Damaged*/
        case TV_SHIELD:
        case TV_DRAG_SHIELD:
        {
            if (k_ptr->to_a < 0) return (FALSE);
            return (TRUE);
        }

    }

    /* Assume not suitable */
    return (FALSE);
}

/*
 * Hack -- determine if a template is a bow/ammo.
 */

static bool kind_is_bow(int k_idx)
{
    object_kind *k_ptr = &k_info[k_idx];

    /* Analyze the item type */
    switch (k_ptr->tval)
    {

        /* All firing weapons and Ammo are suitable  */
        case TV_BOW:
        {
            return (TRUE);
        }

        /*hack - don't allow ammo as a quest reward*/
        case TV_BOLT:
        case TV_ARROW:
        case TV_SHOT:
        {
            if ((object_generation_mode == OB_GEN_MODE_QUEST) ||
                (object_generation_mode == OB_GEN_MODE_RANDART))  return (FALSE);
            return (TRUE);

        }

    }

    /* Assume not suitable  */
    return (FALSE);
}


/*
 * Hack -- determine if a template is a hafted weapon.
 */

static bool kind_is_hafted(int k_idx)
{
    object_kind *k_ptr = &k_info[k_idx];

    /* Analyze the item type */
    switch (k_ptr->tval)
    {
        /* Hafted Weapons -- suitable  unless damaged */
        case TV_HAFTED:
        {
            if (k_ptr->to_h < 0) return (FALSE);
            if (k_ptr->to_d < 0) return (FALSE);
            return (TRUE);
        }
    }

    /* Assume not suitable  */
    return (FALSE);
}

/*
 * Hack -- determine if a template is a "good" digging tool
 *
 */
static bool kind_is_digging_tool(int k_idx)
{
    object_kind *k_ptr = &k_info[k_idx];

    /* Analyze the item type */
    switch (k_ptr->tval)
    {

        /* Diggers -- Good unless damaged */
        case TV_DIGGING:
        {
            if (k_ptr->to_h < 0) return (FALSE);
            if (k_ptr->to_d < 0) return (FALSE);
            return (TRUE);
        }

    }

    /* Assume not good */
    return (FALSE);
}

/*
 * Hack -- determine if a template is a edged weapon.
 */
static bool kind_is_edged(int k_idx)
{
    object_kind *k_ptr = &k_info[k_idx];

    /* Analyze the item type */
    switch (k_ptr->tval)
    {
        /* Edged Weapons -- suitable unless damaged */
        case TV_SWORD:
        {
            if (k_ptr->to_h < 0) return (FALSE);
            if (k_ptr->to_d < 0) return (FALSE);
            return (TRUE);
        }
    }

    /* Assume not suitable */
    return (FALSE);
}

/*
 * Hack -- determine if a template is a polearm.
 */
static bool kind_is_polearm(int k_idx)
{
    object_kind *k_ptr = &k_info[k_idx];

    /* Analyze the item type */
    switch (k_ptr->tval)
    {
        /* Weapons -- suitable  unless damaged */
        case TV_POLEARM:
        {
            if (k_ptr->to_h < 0) return (FALSE);
            if (k_ptr->to_d < 0) return (FALSE);
            return (TRUE);
        }
    }

    /* Assume not suitable */
    return (FALSE);
}

/*
 * Hack -- determine if a template is a weapon.
 */
static bool kind_is_weapon(int k_idx)
{
    object_kind *k_ptr = &k_info[k_idx];

    /* Analyze the item type */
    switch (k_ptr->tval)
    {
        /* Weapons -- suitable  unless damaged */
        case TV_SWORD:
        case TV_HAFTED:
        case TV_POLEARM:
        {
            if (k_ptr->to_h < 0) return (FALSE);
            if (k_ptr->to_d < 0) return (FALSE);
            return (TRUE);
        }
    }

    /* Assume not suitable */
    return (FALSE);
}

/*
 * Hack -- determine if a weapon is suitable for a moria chest.
 */
static bool kind_is_moria_weapons(int k_idx)
{
    object_kind *k_ptr = &k_info[k_idx];

    /* Analyze the item type */
    switch (k_ptr->tval)
    {
        /* Weapons -- suitable  unless damaged */
        case TV_SWORD:
        case TV_HAFTED:
        case TV_POLEARM:
        {
            if (k_ptr->to_h < 0) return (FALSE);
            if (k_ptr->to_d < 0) return (FALSE);
            return (TRUE);
        }

        /* All firing weapons are suitable  */
        case TV_BOW:
        {
            return (TRUE);
        }
    }

    /* Assume not suitable */
    return (FALSE);
}


/*
 * Hack -- determine if a scroll is suitable for a chest.
 *
 */
static bool kind_is_scroll(int k_idx)
{
    object_kind *k_ptr = &k_info[k_idx];

    /* Analyze the item type */
    switch (k_ptr->tval)
    {

        /*scrolls suitable for a chest*/
        case TV_SCROLL:
        {
            if (k_ptr->sval == SV_SCROLL_ACQUIREMENT) return (TRUE);
            if ((k_ptr->sval == SV_SCROLL_CREATE_RANDART) &&
                (!birth_no_xtra_artifacts))   return (TRUE);
            if (k_ptr->sval == SV_SCROLL_STAR_ACQUIREMENT) return (TRUE);
            if (k_ptr->sval == SV_SCROLL_BANISHMENT) return (TRUE);
            if (k_ptr->sval == SV_SCROLL_MASS_BANISHMENT) return (TRUE);
            if (k_ptr->sval == SV_SCROLL_RUNE_OF_PROTECTION) return (TRUE);
            if ((k_ptr->sval == SV_SCROLL_TELEPORT) &&
                ((k_ptr->k_level + 15) >= object_level )) return (TRUE);
            if (k_ptr->sval == SV_SCROLL_STAR_IDENTIFY) return (TRUE);
            if ((k_ptr->sval == SV_SCROLL_RECHARGING) &&
                ((k_ptr->k_level + 15) >= object_level )) return (TRUE);
            if (k_ptr->sval == SV_SCROLL_STAR_RECHARGING) return (TRUE);
            if (k_ptr->sval == SV_SCROLL_CREATE_MONSTER_TRAP) return (TRUE);

            return (FALSE);
        }

        /* Books -- HACK - High level books are good only
         * if within 5 levels of being out of depth */
        case TV_MAGIC_BOOK:
        case TV_PRAYER_BOOK:
        case TV_DRUID_BOOK:
        {
            if (((k_ptr->k_level - 5) < object_level ) &&
            (k_ptr->sval >= SV_BOOK_MIN_GOOD)) return (TRUE);
            return (FALSE);
        }

    }

    /* Assume not suitable */
    return (FALSE);
}

/*
 * Hack -- determine if a potion is good for a chest.
 * includes mushroom of restoring
 *
 */
static bool kind_is_potion(int k_idx)
{
    object_kind *k_ptr = &k_info[k_idx];

    /* Analyze the item type */
    switch (k_ptr->tval)
    {

        /*potions suitable for a chest*/
        case TV_POTION:
        {
            if (k_ptr->sval == SV_POTION_SPEED) return (TRUE);
            if ((k_ptr->sval == SV_POTION_HEALING) &&
                ((k_ptr->k_level + 20) >= object_level )) return (TRUE);
            if (k_ptr->sval == SV_POTION_STAR_HEALING) return (TRUE);
            if (k_ptr->sval == SV_POTION_LIFE) return (TRUE);
            if ((k_ptr->sval == SV_POTION_INC_STR) &&
                ((k_ptr->k_level + 10) >= object_level )) return (TRUE);
            if ((k_ptr->sval == SV_POTION_INC_INT) &&
                ((k_ptr->k_level + 10) >= object_level )) return (TRUE);
            if ((k_ptr->sval == SV_POTION_INC_WIS) &&
                ((k_ptr->k_level + 10) >= object_level )) return (TRUE);
            if ((k_ptr->sval == SV_POTION_INC_DEX) &&
                ((k_ptr->k_level + 10) >= object_level )) return (TRUE);
            if ((k_ptr->sval == SV_POTION_INC_CON) &&
                ((k_ptr->k_level + 10) >= object_level )) return (TRUE);
            if ((k_ptr->sval == SV_POTION_INC_CHR) &&
                ((k_ptr->k_level + 10) >= object_level )) return (TRUE);
            if ((k_ptr->sval == SV_POTION_AUGMENTATION) &&
                ((k_ptr->k_level + 10) >= object_level )) return (TRUE);
            if (k_ptr->sval == SV_POTION_EXPERIENCE) return (TRUE);
            if (k_ptr->sval == SV_POTION_ENLIGHTENMENT) return (TRUE);
            if (k_ptr->sval == SV_POTION_RESISTANCE) return (TRUE);
            if (k_ptr->sval == SV_POTION_INVULNERABILITY) return (TRUE);

            return (FALSE);
        }

        case TV_FOOD:
        /* HACK -  Mushrooms of restoring can be with potions */
        {
            if ((k_ptr->sval == SV_FOOD_RESTORING) &&
                ((k_ptr->k_level + 25) >= object_level )) return (TRUE);
            return (FALSE);
        }

    }

    /* Assume not suitable */
    return (FALSE);
}

/*
 * Hack -- determine if a rod/wand/staff is good for a chest.
 *
 */
static bool kind_is_rod_wand_staff(int k_idx)
{
    object_kind *k_ptr = &k_info[k_idx];

    /* Analyze the item type */
    switch (k_ptr->tval)
    {

        /*wands suitable for a chest*/
        case TV_WAND:

        {
            if ((k_ptr->sval == SV_WAND_TELEPORT_AWAY) &&
                ((k_ptr->k_level + 20) >= object_level )) return (TRUE);
            if ((k_ptr->sval == SV_WAND_STONE_TO_MUD) &&
                ((k_ptr->k_level + 20) >= object_level )) return (TRUE);
            if (k_ptr->sval == SV_WAND_ANNIHILATION) return (TRUE);
            if (k_ptr->sval == SV_WAND_DRAGON_FIRE) return (TRUE);
            if (k_ptr->sval == SV_WAND_DRAGON_COLD) return (TRUE);
            if (k_ptr->sval == SV_WAND_DRAGON_BREATH) return (TRUE);

            return (FALSE);

        }

        /*staffs suitable for a chest*/
        case TV_STAFF:

        {
            if ((k_ptr->sval == SV_STAFF_TELEPORTATION) &&
                ((k_ptr->k_level + 20) >= object_level )) return (TRUE);
            if (k_ptr->sval == SV_STAFF_THE_MAGI) return (TRUE);
            if (k_ptr->sval == SV_STAFF_SPEED) return (TRUE);
            if (k_ptr->sval == SV_STAFF_DISPEL_EVIL) return (TRUE);
            if (k_ptr->sval == SV_STAFF_POWER) return (TRUE);
            if (k_ptr->sval == SV_STAFF_HOLINESS) return (TRUE);
            if (k_ptr->sval == SV_STAFF_BANISHMENT) return (TRUE);
            if (k_ptr->sval == SV_STAFF_MASS_IDENTIFY) return (TRUE);
            if ((k_ptr->sval == SV_STAFF_DESTRUCTION) &&
                ((k_ptr->k_level + 20) >= object_level )) return (TRUE);
            return (FALSE);
        }

        /*rods suitable for a chest*/
        case TV_ROD:

        {
            if ((k_ptr->sval == SV_ROD_IDENTIFY) &&
                ((k_ptr->k_level + 20) >= object_level )) return (TRUE);
            if ((k_ptr->sval == SV_ROD_DETECTION) &&
                ((k_ptr->k_level + 20) >= object_level )) return (TRUE);
            if ((k_ptr->sval == SV_ROD_STONE_TO_MUD) &&
                ((k_ptr->k_level + 10) >= object_level )) return (TRUE);
            if (k_ptr->sval == SV_ROD_HEALING) return (TRUE);
            if (k_ptr->sval == SV_ROD_RESTORATION) return (TRUE);
            if (k_ptr->sval == SV_ROD_SPEED) return (TRUE);
            if (k_ptr->sval == SV_ROD_STAR_IDENTIFY) return (TRUE);
            if (k_ptr->sval == SV_ROD_MASS_IDENTIFY) return (TRUE);
            if ((k_ptr->sval == SV_ROD_TELEPORT_AWAY) &&
                ((k_ptr->k_level + 20) >= object_level )) return (TRUE);
            return (FALSE);
        }

    }

    /* Assume not suitable for a chest */
    return (FALSE);
}

/*
 * Hack -- determine if a template is "jewelry for chests".
 *
 */
static bool kind_is_jewelry(int k_idx)
{
    object_kind *k_ptr = &k_info[k_idx];

    /* Analyze the item type */
    switch (k_ptr->tval)
    {
        /* Crowns are suitable for a chest */
        case TV_CROWN:
        {
            if (k_ptr->to_a < 0) return (FALSE);
            return (TRUE);
        }

        /*  Rings of Speed are suitable for a chest */
        case TV_RING:
        {
            if (k_ptr->sval == SV_RING_SPEED) return (TRUE);
            return (FALSE);
        }

        /* Some Amulets are suitable for a chest*/
        case TV_AMULET:

        {
            if (k_ptr->sval == SV_AMULET_THE_MAGI) return (TRUE);
            if (k_ptr->sval == SV_AMULET_DEVOTION) return (TRUE);
            if (k_ptr->sval == SV_AMULET_WEAPONMASTERY) return (TRUE);
            if (k_ptr->sval == SV_AMULET_TRICKERY) return (TRUE);
            return (FALSE);
        }

    }

    /* Assume not suitable for a chest */
    return (FALSE);
}

/*
 * Hack -- determine if a rods/wands/staves are good for a moria chest.
 *
 */
static bool kind_is_moria_items(int k_idx)
{
    object_kind *k_ptr = &k_info[k_idx];

    /* Analyze the item type */
    switch (k_ptr->tval)
    {
        /*potions suitable for a moria chest*/
        case TV_POTION:
        {
            if (k_ptr->sval == SV_POTION_SPEED) return (TRUE);
            if (k_ptr->sval == SV_POTION_HEALING) return (TRUE);
            if ((k_ptr->sval == SV_POTION_INC_STR) &&
                ((k_ptr->k_level + 10) >= object_level )) return (TRUE);
            if ((k_ptr->sval == SV_POTION_INC_INT) &&
                ((k_ptr->k_level + 10) >= object_level )) return (TRUE);
            if ((k_ptr->sval == SV_POTION_INC_WIS) &&
                ((k_ptr->k_level + 10) >= object_level )) return (TRUE);
            if ((k_ptr->sval == SV_POTION_INC_DEX) &&
                ((k_ptr->k_level + 10) >= object_level )) return (TRUE);
            if ((k_ptr->sval == SV_POTION_INC_CON) &&
                ((k_ptr->k_level + 10) >= object_level )) return (TRUE);
            if ((k_ptr->sval == SV_POTION_INC_CHR) &&
                ((k_ptr->k_level + 10) >= object_level )) return (TRUE);
            if (k_ptr->sval == SV_POTION_EXPERIENCE) return (TRUE);
            if (k_ptr->sval == SV_POTION_INVULNERABILITY) return (TRUE);

            return (FALSE);
        }

        /*wands suitable for a chest*/
        case TV_WAND:

        {
            if ((k_ptr->sval == SV_WAND_TELEPORT_AWAY) &&
                ((k_ptr->k_level + 20) >= object_level )) return (TRUE);
            if ((k_ptr->sval == SV_WAND_STONE_TO_MUD) &&
                ((k_ptr->k_level + 20) >= object_level )) return (TRUE);
            return (FALSE);

        }

        /*staffs suitable for a chest*/
        case TV_STAFF:

        {
            if ((k_ptr->sval == SV_STAFF_TELEPORTATION) &&
                ((k_ptr->k_level + 20) >= object_level )) return (TRUE);
            if (k_ptr->sval == SV_STAFF_SPEED) return (TRUE);
            if (k_ptr->sval == SV_STAFF_DISPEL_EVIL) return (TRUE);
            if (k_ptr->sval == SV_STAFF_BANISHMENT) return (TRUE);
            if ((k_ptr->sval == SV_STAFF_DESTRUCTION) &&
                ((k_ptr->k_level + 20) >= object_level )) return (TRUE);
            return (FALSE);
        }
    }

    /* Assume not suitable */
    return (FALSE);
}




/*
 * Hack -- determine if a template is "good".
 *
 * Note that this test only applies to the object *kind*, so it is
 * possible to choose a kind which is "good", and then later cause
 * the actual object to be cursed.  We do explicitly forbid objects
 * which are known to be boring or which start out somewhat damaged.
 */
static bool kind_is_good(int k_idx)
{
    object_kind *k_ptr = &k_info[k_idx];

    /* Analyze the item type */
    switch (k_ptr->tval)
    {
        /* Armor -- Good unless damaged */
        case TV_HARD_ARMOR:
        case TV_SOFT_ARMOR:
        case TV_DRAG_ARMOR:
        case TV_DRAG_SHIELD:
        case TV_SHIELD:
        case TV_CLOAK:
        case TV_BOOTS:
        case TV_GLOVES:
        case TV_HELM:
        case TV_CROWN:
        {
            if (k_ptr->to_a < 0) return (FALSE);
            return (TRUE);
        }

        /* Weapons -- Good unless damaged */
        case TV_BOW:
        case TV_SWORD:
        case TV_HAFTED:
        case TV_POLEARM:
        case TV_DIGGING:
        {
            if (k_ptr->to_h < 0) return (FALSE);
            if (k_ptr->to_d < 0) return (FALSE);
            return (TRUE);
        }

        /* Ammo -- Arrows/Bolts are good */
        case TV_BOLT:
        case TV_ARROW:
        case TV_SHOT:
        {
            if ((object_generation_mode == OB_GEN_MODE_QUEST) ||
                (object_generation_mode == OB_GEN_MODE_RANDART))  return (FALSE);
            return (TRUE);
        }

        /* Books -- HACK - High level books are good only
         * if within 15 levels of being out of depth
         **/

        case TV_MAGIC_BOOK:
        case TV_PRAYER_BOOK:
        case TV_DRUID_BOOK:
        {
            if (((k_ptr->k_level - 15) < object_level ) &&
            (k_ptr->sval >= SV_BOOK_MIN_GOOD)) return (TRUE);
            return (FALSE);
        }

        /* Rings -- Rings of Speed are good */
        case TV_RING:
        {
            if (k_ptr->sval == SV_RING_SPEED) return (TRUE);
            return (FALSE);
        }

        /* Amulets -- Amulets are good*/
        case TV_AMULET:

        {
            if (k_ptr->sval == SV_AMULET_THE_MAGI) return (TRUE);
            if (k_ptr->sval == SV_AMULET_DEVOTION) return (TRUE);
            if (k_ptr->sval == SV_AMULET_WEAPONMASTERY) return (TRUE);
            if (k_ptr->sval == SV_AMULET_TRICKERY) return (TRUE);
            return (FALSE);
        }

        /*scrolls of "*acquirement*" and "acquirement" are good*/
        case TV_SCROLL:

        {
            if (k_ptr->sval == SV_SCROLL_ACQUIREMENT) return (TRUE);
            if (k_ptr->sval == SV_SCROLL_STAR_ACQUIREMENT) return (TRUE);
            if ((k_ptr->sval == SV_SCROLL_CREATE_RANDART) &&
                (!birth_no_xtra_artifacts))   return (TRUE);
            return (FALSE);
        }

        /*
         * The very powerful healing potions can be good.
         * Stat gain potions can be good at the lower levels.
         */
        case TV_POTION:
        {
            switch (k_ptr->sval)
            {
                case SV_POTION_STAR_HEALING:
                case SV_POTION_LIFE:
                {
                    if (object_level > 85) return (TRUE);
                    return (FALSE);
                }
                case SV_POTION_INC_STR:
                case SV_POTION_INC_INT:
                case SV_POTION_INC_WIS:
                case SV_POTION_INC_CON:
                case SV_POTION_INC_DEX:
                case SV_POTION_AUGMENTATION:
                {
                    if (object_generation_mode != OB_GEN_MODE_QUEST) return (FALSE);

                    if (object_level < (k_ptr->k_level - 5)) return (TRUE);
                    return (FALSE);
                }
                default: break;

            }
            return (FALSE);
        }

        /* Chests -- Chests are good. */
        case TV_CHEST:
        {
            return (TRUE);
        }

    }

    /* Assume not good */
    return (FALSE);
}

void object_quantities(object_type *j_ptr)
{
    /* Hack -- generate multiple spikes/missiles */
    switch (j_ptr->tval)
    {
        case TV_SPIKE:
        case TV_SHOT:
        case TV_ARROW:
        case TV_BOLT:
        {
            j_ptr->number = damroll(6, 7);
            return;
        }
        /* Occasionally do multiple potions, scrolls */
        case TV_SCROLL:
        case TV_POTION:
        {
            if (one_in_(10)) j_ptr->number = randint1(2);
            return;
        }
        case TV_FOOD:
        {
            if (one_in_(5)) j_ptr->number = damroll(2,2);
            return;
        }

    }
}



/*
 * Attempt to make an object (normal or good/great)
 *
 * This routine plays nasty games to generate the "special artifacts".
 *
 * This routine uses "object_level" for the "generation level".
 *
 * We assume that the given object has been "wiped".
 */
bool make_object(object_type *j_ptr, bool good, bool great, int objecttype, bool interesting)
{
    int prob, base;

    /* Chance of "special object" */
    prob = ((good || great) ? 10 : 1000);

    /*better chance to check special artifacts if there is a jewelry theme*/
    if (objecttype == DROP_TYPE_JEWELRY) prob /= 2;

    /* Base level for the object */
    base = ((good || great) ? (object_level + 10) : object_level);

    /* Give only interesting items*/
    if (interesting)
    {
        get_obj_num_hook = kind_is_good;
    }

    /* Attempt to generate a special artifact if prob = 0, or a normal object
     * if not.
     */
    if ((rand_int(prob) != 0) || (!make_artifact_special(j_ptr)))
    {
        int k_idx;

        /*
         * Find out what kind of objects we want.
         */
        /*note - theme 1 is gold, sent to the make_gold function*/
        if (objecttype == DROP_TYPE_POTION)						get_obj_num_hook = kind_is_potion;
        else if (objecttype == DROP_TYPE_ROD_WAND_STAFF) 		get_obj_num_hook = kind_is_rod_wand_staff;
        else if (objecttype == DROP_TYPE_SCROLL) 				get_obj_num_hook = kind_is_scroll;
        else if (objecttype == DROP_TYPE_SHIELD) 				get_obj_num_hook = kind_is_shield;
        else if (objecttype == DROP_TYPE_WEAPON) 				get_obj_num_hook = kind_is_weapon;
        else if (objecttype == DROP_TYPE_ARMOR) 				get_obj_num_hook = kind_is_body_armor;
        else if (objecttype == DROP_TYPE_BOOTS) 				get_obj_num_hook = kind_is_boots;
        else if (objecttype == DROP_TYPE_BOW) 					get_obj_num_hook = kind_is_bow;
        else if (objecttype == DROP_TYPE_CLOAK)					get_obj_num_hook = kind_is_cloak;
        else if (objecttype == DROP_TYPE_GLOVES)				get_obj_num_hook = kind_is_gloves;
        else if (objecttype == DROP_TYPE_HAFTED)				get_obj_num_hook = kind_is_hafted;
        else if (objecttype == DROP_TYPE_HEADGEAR)				get_obj_num_hook = kind_is_headgear;
        else if (objecttype == DROP_TYPE_JEWELRY)				get_obj_num_hook = kind_is_jewelry;
        else if (objecttype == DROP_TYPE_DRAGON_ARMOR)			get_obj_num_hook = kind_is_dragarmor;
        else if (objecttype == DROP_TYPE_CHEST)					get_obj_num_hook = kind_is_chest;
        else if (objecttype == DROP_TYPE_DUNGEON_MAGIC_BOOK)	get_obj_num_hook = kind_is_dungeon_magic_book;
        else if (objecttype == DROP_TYPE_DUNGEON_PRAYER_BOOK)	get_obj_num_hook = kind_is_dungeon_prayer_book;
        else if (objecttype == DROP_TYPE_DUNGEON_DRUID_BOOK)	get_obj_num_hook = kind_is_dungeon_druid_book;
        else if (objecttype == DROP_TYPE_EDGED)					get_obj_num_hook = kind_is_edged;
        else if (objecttype == DROP_TYPE_POLEARM)				get_obj_num_hook = kind_is_polearm;
        else if (objecttype == DROP_TYPE_DIGGING)				get_obj_num_hook = kind_is_digging_tool;
        else if (objecttype == DROP_TYPE_MORIA_ITEMS)			get_obj_num_hook = kind_is_moria_items;
        else if (objecttype == DROP_TYPE_MORIA_WEAPONS)			get_obj_num_hook = kind_is_moria_weapons;
        else if (objecttype == DROP_TYPE_MORIA_ARMOR_BODY)		get_obj_num_hook = kind_is_moria_armor_body;
        else if (objecttype == DROP_TYPE_MORIA_ARMOR_OTHER)		get_obj_num_hook = kind_is_moria_armor_other;
        /*
         *	If it isn't a chest, check good and great flags.
         *  They each now have their own templates.
         */
        else if (great)	get_obj_num_hook = kind_is_great;
        else if (good)	get_obj_num_hook = kind_is_good;
        /* DROP_TYPE_UNTHEMED - just eliminate STORE_ONLY items */
        else get_obj_num_hook = kind_is_dungeon;

        /* Prepare allocation table */
        get_obj_num_prep();

        /* Pick a random object */
        k_idx = get_obj_num(base);

        /* Clear the objects template*/
        get_obj_num_hook = NULL;

        /* Prepare allocation table */
        get_obj_num_prep();

        /* Handle failure*/
        if (!k_idx) return (FALSE);

        /* Prepare the object */
        object_prep(j_ptr, k_idx);

    }

    /* Apply magic (allow artifacts) */
    apply_magic(j_ptr, object_level, TRUE, good, great, interesting);

    /* Hack -- generate multiple objects occasionally */
    object_quantities(j_ptr);

    /* Notice "okay" out-of-depth objects */
    if (!j_ptr->is_cursed() && !j_ptr->is_broken() &&
        (k_info[j_ptr->k_idx].k_level > p_ptr->depth))
    {
        /* Rating increase */
        rating += (k_info[j_ptr->k_idx].k_level - p_ptr->depth);

        /* Cheat -- peek at items */
        if (cheat_peek) object_mention(j_ptr);
    }

    // Make sure all flags are up-to-date
    j_ptr->update_object_flags();

    /* Success */
    return (TRUE);
}

/*
 * Sets the object generation mode for the store, since in doing
 * store inventory we make many objects of the same type
 * The function is here rather than store.c because
 * This is where all the other needed functions are.
 */

bool prep_store_object(int storetype)
{
    /*get the store creation mode*/
    switch (storetype)
    {
        case STORE_GENERAL:
        {
            object_generation_mode = OB_GEN_MODE_GEN_ST;
            get_obj_num_hook = kind_is_gen_store;
            break;
        }
        case STORE_ARMOR:
        {
            object_generation_mode = OB_GEN_MODE_ARMOURY;
            get_obj_num_hook = kind_is_armoury;
            break;
        }
        case STORE_WEAPON:
        {
            object_generation_mode = OB_GEN_MODE_WEAPONSMITH;
            get_obj_num_hook = kind_is_weaponsmith;
            break;
        }
        case STORE_TEMPLE:
        {
            object_generation_mode = OB_GEN_MODE_TEMPLE;
            get_obj_num_hook = kind_is_temple;
            break;
        }
        case STORE_ALCHEMY:
        {
            object_generation_mode = OB_GEN_MODE_ALCHEMY;
            get_obj_num_hook = kind_is_alchemy;
            break;
        }
        case STORE_MAGIC:
        {
            object_generation_mode = OB_GEN_MODE_MAGIC_SHOP;
            get_obj_num_hook = kind_is_magic_shop;
            break;
        }
        case STORE_B_MARKET:
        {
            object_generation_mode = OB_GEN_MODE_BLACK_MARK;
            get_obj_num_hook = kind_is_black_market;
            break;
        }
        case STORE_BOOKSHOP:
        {
            object_generation_mode = OB_GEN_MODE_BOOKSHOP;
            get_obj_num_hook = kind_is_bookshop;
            break;
        }

        default: return (FALSE);
    }

    /*prepare the allocation table*/
    get_obj_num_prep();

    return(TRUE);

}

/*
 * Set the object theme
 */


/*
 * This is an imcomplete list of themes.  Returns false if theme not found.
 * Used primarily for Randarts
 */
bool prep_object_theme(int themetype)
{
    /*get the store creation mode*/
    switch (themetype)
    {
        case DROP_TYPE_SHIELD:
        {
            get_obj_num_hook = kind_is_shield;
            break;
        }
        case DROP_TYPE_WEAPON:
        {
            get_obj_num_hook = kind_is_weapon;
            break;
        }
        case DROP_TYPE_EDGED:
        {
            get_obj_num_hook = kind_is_edged;
            break;
        }
        case DROP_TYPE_POLEARM:
        {
            get_obj_num_hook = kind_is_polearm;
            break;
        }
        case DROP_TYPE_ARMOR:
        {
            get_obj_num_hook = kind_is_body_armor;
            break;
        }
        case DROP_TYPE_BOOTS:
        {
            get_obj_num_hook = kind_is_boots;
            break;
        }
        case DROP_TYPE_BOW:
        {
            get_obj_num_hook = kind_is_bow;
            break;
        }
        case DROP_TYPE_CLOAK:
        {
            get_obj_num_hook = kind_is_cloak;
            break;
        }
        case DROP_TYPE_GLOVES:
        {
            get_obj_num_hook = kind_is_gloves;
            break;
        }
        case DROP_TYPE_HAFTED:
        {
            get_obj_num_hook = kind_is_hafted;
            break;
        }
        case DROP_TYPE_HEADGEAR:
        {
            get_obj_num_hook = kind_is_headgear;
            break;
        }
        case DROP_TYPE_DRAGON_ARMOR:
        {
            get_obj_num_hook = kind_is_dragarmor;
            break;
        }
        case DROP_TYPE_DIGGING:
        {
            get_obj_num_hook = kind_is_digging_tool;

            break;
        }

        default: return (FALSE);
    }

    /*prepare the allocation table*/
    get_obj_num_prep();

    return(TRUE);
}

// Routines for making "fake" objects for dialog box purposes

/*
 * Hack -- Create a "forged" artifact
 */
bool make_fake_artifact(object_type *o_ptr, byte art_num)
{
    int i;

    artifact_type *a_ptr = &a_info[art_num];

    /* Ignore "empty" artifacts */
    if (a_ptr->tval + a_ptr->sval == 0) return FALSE;

    /* Get the "kind" index */
    i = lookup_kind(a_ptr->tval, a_ptr->sval);

    /* Oops */
    if (!i) return (FALSE);

    o_ptr->object_wipe();

    /* Create the artifact */
    object_prep(o_ptr, i);

    /* Save the name */
    o_ptr->art_num = art_num;

    /* Extract the fields */
    o_ptr->pval = a_ptr->pval;
    o_ptr->ac = a_ptr->ac;
    o_ptr->dd = a_ptr->dd;
    o_ptr->ds = a_ptr->ds;
    o_ptr->to_a = a_ptr->to_a;
    o_ptr->to_h = a_ptr->to_h;
    o_ptr->to_d = a_ptr->to_d;
    o_ptr->weight = a_ptr->weight;

    /*identify it*/
    o_ptr->mark_known(FALSE);

    /*make it a store item*/
    o_ptr->ident |= IDENT_STORE;

    /* Hack -- extract the "cursed" flag */
    if (a_ptr->a_flags3 & (TR3_LIGHT_CURSE)) o_ptr->ident |= (IDENT_CURSED);

    /* Success */
    return (TRUE);
}


/*
 * Add a pval so the object descriptions don't look strange*
 */
void apply_magic_fake(object_type *o_ptr)
{
    /* Analyze type */
    switch (o_ptr->tval)
    {
        case TV_DIGGING:
        {
            o_ptr->pval = 1;
            break;
        }

        /*many rings need a pval*/
        case TV_RING:
        {
            switch (o_ptr->sval)
            {
                case SV_RING_STR:
                case SV_RING_CON:
                case SV_RING_DEX:
                case SV_RING_INT:
                case SV_RING_SPEED:
                case SV_RING_SEARCHING:
                {
                    o_ptr->pval = 1;
                    break;
                }

                case SV_RING_AGGRAVATION:
                {
                    o_ptr->ident |= (IDENT_CURSED);
                    break;
                }
                case SV_RING_WEAKNESS:
                case SV_RING_STUPIDITY:
                {
                    /* Broken */
                    o_ptr->ident |= (IDENT_BROKEN);

                    /* Cursed */
                    o_ptr->ident |= (IDENT_CURSED);

                    /* Penalize */
                    o_ptr->pval = -1;

                    break;
                }
                /* WOE */
                case SV_RING_WOE:
                {
                    /* Broken */
                    o_ptr->ident |= (IDENT_BROKEN);

                    /* Cursed */
                    o_ptr->ident |= (IDENT_CURSED);

                    /* Penalize */
                    o_ptr->to_a = -1;
                    o_ptr->pval = -1;

                    break;
                }
                /* Ring that increase damage */
                case SV_RING_DAMAGE:
                {
                    /* Bonus to damage */
                    o_ptr->to_d = 1;

                    break;
                }
                /* Ring that increase accuracy */
                case SV_RING_ACCURACY:
                {
                    /* Bonus to hit */
                    o_ptr->to_h = 1;

                    break;
                }
                /* Rings that provide of Protection */
                case SV_RING_PROTECTION:
                case SV_RING_FLAMES:
                case SV_RING_ACID:
                case SV_RING_ICE:
                case SV_RING_LIGHTNING:
                {
                    /* Bonus to armor class */
                    o_ptr->to_a = 1;

                    break;
                }
                /* Rings that provide of Protection */
                case SV_RING_LORD_PROT_ACID:
                case SV_RING_LORD_PROT_FIRE:
                case SV_RING_LORD_PROT_COLD:
                {
                    /* Bonus to armor class */
                    o_ptr->to_a = 5;

                    break;
                }
                /*both to-hit and to-damage*/
                case SV_RING_SLAYING:
                {
                    /* Bonus to damage and to hit */
                    o_ptr->to_d = 1;
                    o_ptr->to_h = 1;

                    break;
                }
                default: break;

            }
            /*break for TVAL-Rings*/
            break;
        }

        case TV_AMULET:
        {
            /* Analyze */
            switch (o_ptr->sval)
            {
                /* Amulet of wisdom/charisma/infravision */
                case SV_AMULET_WISDOM:
                case SV_AMULET_CHARISMA:
                case SV_AMULET_INFRAVISION:
                case SV_AMULET_SEARCHING:
                case SV_AMULET_ESP:
                case SV_AMULET_DEVOTION:
                case SV_AMULET_TRICKERY:
                {
                    /* Stat bonus */
                    o_ptr->pval = 1;

                    break;
                }

                /* Amulet of the Magi -- never cursed */
                case SV_AMULET_THE_MAGI:
                {
                    o_ptr->pval = 1;
                    o_ptr->to_a = 1;

                    break;
                }

                /* Amulet of Weaponmastery -- never cursed */
                case SV_AMULET_WEAPONMASTERY:
                {
                    o_ptr->to_h = 1;
                    o_ptr->to_d = 1;
                    o_ptr->pval = 1;

                    break;
                }

                /* Amulet of Doom -- always cursed */
                case SV_AMULET_DOOM:
                case SV_AMULET_WOE:
                {
                    /* Broken */
                    o_ptr->ident |= (IDENT_BROKEN);

                    /* Cursed */
                    o_ptr->ident |= (IDENT_CURSED);

                    /* Penalize */
                    o_ptr->pval = -1;
                    o_ptr->to_a = -1;

                    break;
                }

                default: break;

            }
            /*break for TVAL-Amulets*/
            break;
        }

        case TV_LIGHT:
        {
            /* Analyze */
            switch (o_ptr->sval)
            {
                case SV_LIGHT_TORCH:
                case SV_LIGHT_LANTERN:
                {
                    o_ptr->timeout = 1;

                    break;
                }

            }
            /*break for TVAL-Lites*/
            break;
        }

        /*give then one charge*/
        case TV_STAFF:
        case TV_WAND:
        {
            o_ptr->pval = 1;

            break;
        }
    }

    if (o_ptr->ego_num)
    {
        ego_item_type *ego_ptr = &e_info[o_ptr->ego_num];
        if (ego_ptr->max_to_a)
        {
            if (ego_ptr->max_to_a > 0) o_ptr->to_a = 1;
            else o_ptr->to_a = -1;
        }
        if (ego_ptr->max_to_h)
        {
            if (ego_ptr->max_to_h > 0) o_ptr->to_h = 1;
            else o_ptr->to_h = -1;
        }
        if (ego_ptr->max_to_d)
        {
            if (ego_ptr->max_to_d > 0) o_ptr->to_d = 1;
            else o_ptr->to_d = -1;
        }
        if (ego_ptr->max_pval)
        {
            if (ego_ptr->max_pval > 0) o_ptr->pval = 1;
            else o_ptr->pval = -1;
        }

        if (ego_ptr->e_flags3 & (TR3_LIGHT_CURSE)) o_ptr->ident |= (IDENT_CURSED);
        if (ego_ptr->e_flags3 & (TR3_HEAVY_CURSE)) o_ptr->ident |= (IDENT_CURSED);
        if (ego_ptr->e_flags3 & (TR3_PERMA_CURSE)) o_ptr->ident |= (IDENT_CURSED);
    }

    o_ptr->update_object_flags();
}



// Make an object just for display purposes.
void make_object_fake(object_type *o_ptr, int k_idx, byte ego_num, bool update_tracking)
{
    o_ptr->object_wipe();

    /* Create the object */
    object_prep(o_ptr, k_idx);

    // Do an ego item if specified
    o_ptr->ego_num = ego_num;

    /* Add minimum bonuses so the descriptions don't look strange. */
    apply_magic_fake(o_ptr);

    /* Update the object recall window */
    if (update_tracking)
    {
        track_object_kind(k_idx);
        handle_stuff();
    }

    /* Hack -- its in the store */
    if (k_info[k_idx].aware) o_ptr->ident |= (IDENT_STORE);

    /* It's fully known */
    if (!k_info[k_idx].flavor)
    {
        /* Mark the item as fully known */
        o_ptr->mark_fully_known(FALSE);
    }
}


/*
 * Get the mimic k_idx for certain objects.
 */
int get_object_mimic_k_idx(const monster_race *r_ptr)
{
    int k_idx = 0;

    if (r_ptr->d_char == ')') get_obj_num_hook = kind_is_armor;
    else if (r_ptr->d_char == '|') get_obj_num_hook = kind_is_weapon;
    else if (r_ptr->d_char == '?') get_obj_num_hook = kind_is_dungeon_spellbook;
    else if (r_ptr->d_char == '[') get_obj_num_hook = kind_is_dragarmor;
    /* Whoops! */
    else return (0);

    /* Prepare allocation table */
    get_obj_num_prep();

    /* Find an object to mimic */
    while (!k_idx) k_idx = get_obj_num(p_ptr->depth);

    /* Clear restriction */
    get_obj_num_hook = NULL;

    /* Prepare allocation table */
    get_obj_num_prep();

    return(k_idx);

}


/*
 * Make a treasure object
 *
 * The location must be a legal, clean, floor grid.
 */
bool make_gold(object_type *j_ptr)
{
    int sval;
    int k_idx;
    s32b base;
    s32b value;
    s32b mult = (birth_no_selling ? object_level : 1);

    /* Boundry control for gold multiplier birth_no_selling option*/
    if (mult < 1) 		mult = 1;
    else if (mult > 5) 	mult = 5;

    /* Hack -- Pick a Treasure variety */
    sval = ((randint(object_level + 2) + 2) / 2);

    /* Apply "extra" magic */
    if (one_in_(GREAT_OBJ))
    {
        sval += randint(object_level + 1);
    }

    /* Hack -- Creeping Coins only generate "themselves" */
    if (coin_type) sval = coin_type;

    /* Do not create "illegal" Treasure Types */
    if (game_mode == GAME_NPPMORIA)
    {
        if (sval > MAX_GOLD_NPPMORIA) sval = MAX_GOLD_NPPMORIA;
    }
    else if (sval > MAX_GOLD_NPPANGBAND) sval = MAX_GOLD_NPPANGBAND;

    k_idx = lookup_kind(TV_GOLD, sval);

    /* Prepare a gold object */
    object_prep(j_ptr, k_idx);

    /* Hack -- Base coin cost */
    base = k_info[k_idx].cost * mult;

    /* Determine how much the treasure is "worth" */
    value = (base + (8L * randint(base)) + randint((8 * mult)));

    /*chests containing gold are very lucrative*/
    if (object_generation_mode > 0)
    {
        value += ((randint((4 * mult)) + randint((4 * mult)) + object_level / 4 ) * 50);
    }

    /* Cap gold at max short (or alternatively make pvals s32b) */
    if (value > SHRT_MAX)	value = SHRT_MAX;

    j_ptr->pval = value;

    // Make sure all flags are up-to-date
    j_ptr->update_object_flags();

    /* Success */
    return (TRUE);
}





/*
 * Attempt to place an object (normal or good/great) at the given location.
 */
void place_object(int y, int x, bool good, bool great, int droptype)
{
    int tries = 0;


    object_type *i_ptr;
    object_type object_type_body;

    /* Paranoia */
    if (!in_bounds(y, x)) return;

    /* Hack -- clean floor space */
    if (!cave_clean_bold(y, x)) return;

    /* Get local object */
    i_ptr = &object_type_body;

    /* Make an object (if possible) */
    while (TRUE)
    {
        /* Avoid infinite loops */
        if (++tries > 100) return;

        /* Wipe the object */
        i_ptr->object_wipe();

        /* Create the object */
        if (!make_object(i_ptr, good, great, droptype, FALSE)) continue;

        /* Check compatibility with terrain */
        if (object_hates_location(y, x, i_ptr))
        {
            /* Hack -- Preserve artifacts */
            a_info[i_ptr->art_num].a_cur_num = 0;

            /* Try again */
            continue;
        }

        /* We have a suitable object */
        break;
    }

    /* Remember history */
    object_history(i_ptr, ORIGIN_FLOOR, 0);

    /* Give it to the floor */
    if (!floor_carry(y, x, i_ptr))

    {
        /* Hack -- Preserve artifacts */
        a_info[i_ptr->art_num].a_cur_num = 0;
    }
}

/*
 * Attempt to place a quest artifact at the given location.
 */
bool place_quest_artifact(int y, int x)
{
    object_type object_type_body;
    object_type *i_ptr = &object_type_body;;

    /* Paranoia */
    if (!in_bounds(y, x)) return (FALSE);

    /* Hack -- clean floor space */
    if (!cave_clean_bold(y, x)) return (FALSE);

    /* Already created */
    if (a_info[QUEST_ART_SLOT].a_cur_num) return (FALSE);

    /* Wipe the object */
    i_ptr->object_wipe();

    /* Make a quest artifact (should never fail) */
    create_quest_artifact(i_ptr);

    /* Give it to the floor */
    if (!floor_carry(y, x, i_ptr)) return (FALSE);

    /* Success */
    return (TRUE);

}

/*
 * Places a treasure (Gold or Gems) at given location
 */
void place_gold(int y, int x)
{
    object_type *i_ptr;
    object_type object_type_body;

    /* Paranoia */
    if (!in_bounds(y, x)) return;

    /* Require clean floor space */
    if (!cave_clean_bold(y, x)) return;

    /* Get local object */
    i_ptr = &object_type_body;

    /* Wipe the object */
    i_ptr->object_wipe();

    /* Make some gold */
    if (make_gold(i_ptr))
    {
        /* Give it to the floor */
        (void)floor_carry(y, x, i_ptr);
    }
}

/*
 * Hack -- Return the "automatic coin type" of a monster race
 * Used to allocate proper treasure when "Creeping coins" die
 *
 * Note the use of actual "monster names".  XXX XXX XXX
 */
int get_coin_type(const monster_race *r_ptr)
{
    QString name = r_ptr->r_name_full;

    /* Analyze "coin" or golem monsters */
    if ((r_ptr->d_char == '$') || (r_ptr->d_char == 'g'))
    {
        /* Look for textual clues */
        if (name.contains(" copper ", Qt::CaseInsensitive)) return (SV_GOLD_COPPER);
        if (name.contains(" silver ", Qt::CaseInsensitive)) return (SV_GOLD_SILVER);
        if (name.contains(" garnet ", Qt::CaseInsensitive)) return (SV_GOLD_GARNET);
        if (name.contains(" gold ", Qt::CaseInsensitive)) return (SV_GOLD_GOLD);
        if (name.contains(" mithril ", Qt::CaseInsensitive)) return (SV_GOLD_MITHRIL);
        if (name.contains(" opal", Qt::CaseInsensitive)) return (SV_GOLD_OPALS);
        if (name.contains(" sapphire", Qt::CaseInsensitive)) return (SV_GOLD_SAPPHIRES);
        if (name.contains(" ruby", Qt::CaseInsensitive)) return (SV_GOLD_RUBIES);
        if (name.contains(" emerald", Qt::CaseInsensitive))return (SV_GOLD_EMERALD);
        if (name.contains(" diamond", Qt::CaseInsensitive)) return (SV_GOLD_DIAMOND);
        if (name.contains(" adamantite ", Qt::CaseInsensitive)) return (SV_GOLD_ADAMANTITE);
    }

    /* Assume nothing */
    return (0);
}


/*
 * Scatter some "great" objects near the player
 */
void acquirement(int y1, int x1, int num, bool great)
{
    object_type *i_ptr;
    object_type object_type_body;

    /* Acquirement */
    while (num--)
    {
        /* Get local object */
        i_ptr = &object_type_body;

        /* Wipe the object */
        i_ptr->object_wipe();

        /* Make a good (or great) object (if possible) */
        if (!make_object(i_ptr, TRUE, great, DROP_TYPE_UNTHEMED, FALSE)) continue;

        /* Remember history */
        object_history(i_ptr, ORIGIN_ACQUIRE, 0);

        /* Drop the object */
        drop_near(i_ptr, -1, y1, x1);
    }
}

/*
 * Create a pint of fine grade mush and either put
 * it in the player's inventory or on the floor
 */
void create_food(void)
{
    object_type *i_ptr;
    object_type object_type_body;

    /* Get local object */
    i_ptr = &object_type_body;

    /* Wipe the object */
    i_ptr->object_wipe();

    object_prep(i_ptr, lookup_kind(TV_FOOD, SV_FOOD_FINE_MUSH));

    /* Remember history */
    object_history(i_ptr, ORIGIN_MAGIC, 0);

    /* Either put it in the inventory or on the floor */
    if (inven_carry_okay(i_ptr))
    {
        // put object in inventory (cmd1)
        put_object_in_inventory(i_ptr);
    }
    else drop_near(i_ptr, -1, p_ptr->py, p_ptr->px);
}

