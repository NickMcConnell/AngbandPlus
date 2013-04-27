// File: object.cpp
// Purpose: misc code for objects

// Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
//
// This software may be copied and distributed for educational, research, and
// not for profit purposes provided that this copyright and statement are
// included in all such copies.

#include "utumno.h"



// Blank an item out on initialization
void CItem::wipe()
{
    k_idx = 0;
    pval = 0;
    discount = 0;
    number = 0;
    name1 = 0;
    name2 = 0;
    xtra1 = 0;
    xtra2 = 0;
    to_h = 0;
    to_d = 0;
    to_a = 0;
    ac = 0;
    dd = 0;
    ds = 0;
    timeout = 0;
    ident = 0;
    marked = 0;
    note = NULL;
    next_i_ptr = NULL;
}

// Constructor: use tv and sv to create
CItem::CItem(int tv, int sv)
{
    wipe();
    invcopy(lookup_kind(tv, sv));
}

// Deal with an item's baggage on deletion
CItem::~CItem()
{
    if (note) {
        delete[] note;
        note = NULL;
    }
}

// Set an object's note
void CItem::SetNote(char *n)
{
    if (note) {
        delete[] note;
        note = NULL;
    }
    if (!n) return;
    note = new char[strlen(n)+1];
    strcpy(note, n);
}

// Get the object weight
s16b CItem::GetWeight(void)
{
    if (isArtifact()) return get_a_ptr()->weight;
    else return get_k_ptr()->weight;
}

// Get the object kind pointer
CObjectKind *CItem::get_k_ptr(void)
{
    return &k_info[GetKIdx()];
}

// Get the artifact pointer
artifact_type *CItem::get_a_ptr(void)
{
    return &a_info[GetName1()];
}

// Get the ego item pointer
ego_item_type *CItem::get_e_ptr(void)
{
    return &e_info[GetName2()];
}


// Get a damage roll
s16b CItem::GetDamRoll(void)
{
    return damroll(GetDD(), GetDS());
}

bool CItem::isKnown(void)
{
    CObjectKind *k_ptr = get_k_ptr();

    if (TestIdentFlag(ID_KNOWN)) return TRUE;
    if (k_ptr->easy_know && k_ptr->aware) return TRUE;
    return FALSE;
}


static bool preserve_flag = FALSE;


// Delete a dungeon object
void delete_object(CItem *i_ptr)
{
    CItem *j_ptr;
    CGrid *g_ptr = i_ptr->get_g_ptr();

    if (preserve_flag) {
        // Hack -- Preserve unknown artifacts
        if (i_ptr->isArtifact() && !i_ptr->isKnown()) {
            // Mega-Hack -- Preserve the artifact
            (i_ptr->get_a_ptr())->cur_num = 0;
        }
    }


    // Is it the top of a pile?
    if (g_ptr->i_ptr == i_ptr) {
        // New top
        g_ptr->i_ptr = i_ptr->next_i_ptr;
    }

    // Not at top of a pile
    else {
        // Get top
        j_ptr = g_ptr->i_ptr;

        // Search for object above this one
        while (j_ptr->next_i_ptr != i_ptr) {
            if (!j_ptr->next_i_ptr) {
                quit("Item pile bug 1 in delete_object_idx");
            }
            else {
                j_ptr = j_ptr->next_i_ptr;
            }
        }

        // Skip this object in the pile
        j_ptr->next_i_ptr = i_ptr->next_i_ptr;
    }


    // Delete
    delete i_ptr;
}


// Deletes object from given location
void delete_object(int y, int x)
{
    CGrid *g_ptr;

    // Refuse "illegal" locations
    if (!in_bounds(y, x)) return;

    // Find where it was
    g_ptr = &cave[y][x];

    // Delete the object
    if (g_ptr->i_ptr) delete_object(g_ptr->i_ptr);
}


// Deletes all objects from given location
void delete_objects(int y, int x)
{
    CGrid *g_ptr;

    // Refuse "illegal" locations
    if (!in_bounds(y, x)) return;

    // Find where it was
    g_ptr = &cave[y][x];

    // Delete objects
    while (g_ptr->i_ptr) delete_object(g_ptr->i_ptr);
}


/*
 * Delete all the items when player leaves the level
 * Note -- we do NOT visually reflect these (irrelevant) changes
 */
void wipe_im_lists(void)
{
    int x, y;
    CGrid *g_ptr;
    CItem *i_ptr;
    CMonster *m_ptr;

    preserve_flag = TRUE;

    // Go through every tile
    for (x = 0; x < cur_wid; x++) {
        for (y = 0; y < cur_hgt; y++) {
            g_ptr = &cave[y][x];
            m_ptr = g_ptr->m_ptr;

            // Delete all objects on the ground
            while (g_ptr->i_ptr) delete_object(g_ptr->i_ptr);

            // Delete all objects in monsters and the monsters themselves
            if (m_ptr) {
                while (m_ptr->i_ptr) {
                    i_ptr = m_ptr->i_ptr;
                    m_ptr->i_ptr = m_ptr->i_ptr->next_i_ptr;
                    delete i_ptr;
                }

                delete_monster(m_ptr);
            }
        }
    }

    preserve_flag = FALSE;
}


/*
 * Choose an object kind that seems "appropriate" to the given level
 *
 * This function uses the "allocation table" built in "init.c".
 *
 * There is a small chance (1/20) of "boosting" the given depth by
 * a potentially large amount (see below).
 *
 * It is (slightly) more likely to acquire an object of the given level
 * than one of a lower level.  This is done by choosing several objects
 * appropriate to the given level and keeping the "hardest" one.
 *
 * XXX XXX XXX Note that this function may fail (very rarely).
 */
s16b get_obj_num(int level)
{
    int i, p, k;

    int k_idx;


    /* Obtain the table */
    s16b *t_lev = alloc_kind_index;
    kind_entry *table = alloc_kind_table;


    // Sometimes boost level
    if (level > 0) {
        // Occasionally, get a better object
        if (one_in(GREAT_OBJ)) {
            // What a bizarre calculation
            level = 1 + (level * MAX_DEPTH / randint(MAX_DEPTH));
        }

        // Maximum level
        if (level > MAX_DEPTH - 1) level = MAX_DEPTH - 1;
    }


    /* Hack -- Pick an object */
    for (k = 0; k < 10000; k++) {
        /* Town level is easy */
        if (level <= 0) {
            /* Pick a level 0 entry */
            i = rand_int(t_lev[0]);
        }

        /* Other levels sometimes have great stuff */
        else {
            /* Roll for rerolling */
            p = rand_int(100);

            /* Pick any object at or below the given level */
            i = rand_int(t_lev[level]);

            /* Try for a "better" item twice (10%) */
            if (p < 10)
            {
                /* Pick another object at or below the given level */
                int j = rand_int(t_lev[level]);

                /* Keep it if it is "better" */
                if (table[i].locale < table[j].locale) i = j;
            }

            /* Try for a "better" item once (50%) */
            if (p < 60) {
                /* Pick another object at or below the given level */
                int j = rand_int(t_lev[level]);

                /* Keep it if it is "better" */
                if (table[i].locale < table[j].locale) i = j;
            }
        }

        /* Access the "k_idx" of the chosen item */
        k_idx = table[i].k_idx;


        /* Hack -- apply the hook (if given) */
        if (get_obj_num_hook && (!(*get_obj_num_hook)(k_idx))) continue;


        /* Hack -- prevent embedded chests */
        if (opening_chest && (k_info[k_idx].tval == TV_CHEST)) continue;


        /* Roll for "rarity" */
        if (rand_int(table[i].chance) != 0) continue;


        /* Use this object */
        return k_idx;
    }


    /* Oops */
    return 0;
}






/*
 * Known is true when the "attributes" of an object are "known".
 * These include tohit, todam, toac, cost, and pval (charges).
 *
 * Note that "knowing" an object gives you everything that an "awareness"
 * gives you, and much more.  In fact, the player is always "aware" of any
 * item of which he has full "knowledge".
 *
 * But having full knowledge of, say, one "wand of wonder", does not, by
 * itself, give you knowledge, or even awareness, of other "wands of wonder".
 * It happens that most "identify" routines (including "buying from a shop")
 * will make the player "aware" of the object as well as fully "know" it.
 *
 * This routine also removes any inscriptions generated by "feelings".
 */
void CItem::MakeKnown(void)
{
    /* Remove "default inscriptions" */
    if (GetNote() && TestIdentFlag(ID_SENSE)) {
        /* Access the inscription */
        char *q = GetNote();

        /* Hack -- Remove auto-inscriptions */
        if ((streq(q, "cursed")) ||
            (streq(q, "broken")) ||
            (streq(q, "good")) ||
            (streq(q, "average")) ||
            (streq(q, "excellent")) ||
            (streq(q, "worthless")) ||
            (streq(q, "special")) ||
            (streq(q, "terrible")))
        {
            /* Forget the inscription */
            SetNote(NULL);
        }
    }

    // Clear the "Felt" info 
    ClearIdentFlag(ID_SENSE);

    // Clear the "Empty" info
    ClearIdentFlag(ID_EMPTY);

    // Now we know about the item
    SetIdentFlag(ID_KNOWN);
}





/*
 * The player is now aware of the effects of the given object.
 */
void CItem::object_aware(void)
{
    /* Fully aware of the effects */
    get_k_ptr()->aware = TRUE;
}



/*
 * Something has been "sampled"
 */
void CItem::object_tried(void)
{
    /* Mark it as tried (even if "aware") */
    get_k_ptr()->tried = TRUE;
}



/*
 * Return the "value" of an "unknown" item
 * Make a guess at the value of non-aware items
 */
s32b CItem::GetValueBase(void)
{
    CObjectKind *k_ptr = get_k_ptr();

    // Aware item -- use template cost
    if (isAware()) return k_ptr->cost;

    // If unaware, analyze the type
    switch (GetTval()) {
        case TV_FOOD: return 5L;
        case TV_POTION: return 20L;
        case TV_SCROLL: return 20L;
        case TV_STAFF: return 70L;
        case TV_WAND: return 50L;
        case TV_ROD: return 90L;
        case TV_RING: return 45L;
        case TV_AMULET: return 45L;
    }

    /* Paranoia -- Oops */
    return 0L;
}


/*
 * Return the "real" price of a "known" item, not including discounts
 *
 * Wand and staffs get cost for each charge
 *
 * Armor is worth an extra 100 gold per bonus point to armor class.
 *
 * Weapons are worth an extra 100 gold per bonus point (AC,TH,TD).
 *
 * Missiles are only worth 5 gold per bonus point, since they
 * usually appear in groups of 20, and we want the player to get
 * the same amount of cash for any "equivalent" item.  Note that
 * missiles never have any of the "pval" flags, and in fact, they
 * only have a few of the available flags, primarily of the "slay"
 * and "brand" and "ignore" variety.
 *
 * Armor with a negative armor bonus is worthless.
 * Weapons with negative hit+damage bonuses are worthless.
 *
 * Every wearable item with a "pval" bonus is worth extra (see below).
 */
s32b CItem::GetValueReal(void)
{
    s32b value;
    u32b f1, f2, f3;
    CObjectKind *k_ptr = get_k_ptr();


    // Artifacts: simply return the value of the artifact
    if (isArtifact()) return get_a_ptr()->cost;


    /* Hack -- "worthless" items */
    if (!k_ptr->cost) return (0L);

    /* Base cost */
    value = k_ptr->cost;


    /* Extract some flags */
    GetFlags(&f1, &f2, &f3);


    /* Ego-Item */
    if (isEgoItem()) {
        ego_item_type *e_ptr = get_e_ptr();

        /* Hack -- "worthless" ego-items */
        if (!e_ptr->cost) return 0L;

        /* Hack -- Reward the ego-item with a bonus */
        value += e_ptr->cost;
    }


    /* Analyze pval bonus */
    switch (GetTval()) {
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
        case TV_LITE:
        case TV_AMULET:
        case TV_RING:

            /* Hack -- Negative "pval" is always bad */
            if (GetPval() < 0) return 0L;

            /* No pval */
            if (!GetPval()) break;

            /* Give credit for stat bonuses */
            if (f1 & TR1_STR) value += GetPval() * 200L;
            if (f1 & TR1_INT) value += GetPval() * 200L;
            if (f1 & TR1_WIS) value += GetPval() * 200L;
            if (f1 & TR1_DEX) value += GetPval() * 200L;
            if (f1 & TR1_CON) value += GetPval() * 200L;
            if (f1 & TR1_CHR) value += GetPval() * 200L;

            // Give credit for stealth and searching
            if (f1 & TR1_STEALTH) value += GetPval() * 100L;
            if (f1 & TR1_SEARCH) value += GetPval() * 100L;

            // Give credit for infra-vision and tunneling
            if (f1 & TR1_INFRA) value += GetPval() * 50L;
            if (f1 & TR1_TUNNEL) value += GetPval() * 50L;

            // Give credit for extra attacks
            if (f1 & TR1_BLOWS) value += GetPval() * 2000L;

            // Give credit for speed bonus
            if (f1 & TR1_SPEED) value += GetPval() * 30000L;

            break;
    }


    /* Analyze the item */
    switch (GetTval()) {
        /* Wands/Staffs */
        case TV_WAND:
        case TV_STAFF:

            /* Pay extra for charges */
            value += (value / 20) * GetPval();

            /* Done */
            break;

        /* Rings/Amulets */
        case TV_RING:
        case TV_AMULET:

            /* Hack -- negative bonuses are bad */
            if (GetToA() < 0) return 0L;
            if (GetToH() < 0) return 0L;
            if (GetToD() < 0) return 0L;

            /* Give credit for bonuses */
            value += (GetToH() + GetToD() + GetToA()) * 100L;

            /* Done */
            break;

        /* Armor */
        case TV_BOOTS:
        case TV_GLOVES:
        case TV_CLOAK:
        case TV_CROWN:
        case TV_HELM:
        case TV_SHIELD:
        case TV_SOFT_ARMOR:
        case TV_HARD_ARMOR:
        case TV_DRAG_ARMOR:

            /* Hack -- negative armor bonus */
            if (GetToA() < 0) return (0L);

            /* Give credit for bonuses */
            value += ((GetToH() + GetToD() + GetToA()) * 100L);

            /* Done */
            break;

        /* Bows/Weapons */
        case TV_BOW:
        case TV_DIGGING:
        case TV_HAFTED:
        case TV_SWORD:
        case TV_POLEARM:

            /* Hack -- negative hit/damage bonuses */
            if (GetToH() + GetToD() < 0) return 0L;

            /* Factor in the bonuses */
            value += (GetToH() + GetToD() + GetToA()) * 100L;

            /* Hack -- Factor in extra damage dice */
            if ((GetDD() > k_ptr->dd) && (GetDS() == k_ptr->ds)) {
                value += (GetDD() - k_ptr->dd) * GetDS() * 100L;
            }

            /* Done */
            break;

        /* Ammo */
        case TV_SHOT:
        case TV_ARROW:
        case TV_BOLT:

            /* Hack -- negative hit/damage bonuses */
            if (GetToH() + GetToD() < 0) return 0L;

            /* Factor in the bonuses */
            value += (GetToH() + GetToD()) * 5L;

            /* Hack -- Factor in extra damage dice */
            if ((GetDD() > k_ptr->dd) && (GetDS() == k_ptr->ds)) {
                value += (GetDD() - k_ptr->dd) * GetDS() * 5L;
            }

            /* Done */
            break;
    }


    /* Return the value */
    return value;
}


/*
 * Return the price of an item including plusses (and charges)
 *
 * This function returns the "value" of the given item (qty one)
 *
 * Never notice "unknown" bonuses or properties, including "curses",
 * since that would give the player information he did not have.
 *
 * Note that discounted items stay discounted forever, even if
 * the discount is "forgotten" by the player via memory loss.
 */
s32b CItem::GetValue(void)
{
    s32b value;


    /* Known items -- acquire the actual value */
    if (isKnown()) {
        /* Broken or cursed items -- worthless */
        if (isBroken() || isCursed()) return 0L;

        /* Real value (see above) */
        value = GetValueReal();
    }

    /* Unknown items -- acquire a base value */
    else {
        /* Hack -- Felt broken or cursed items */
        if (TestIdentFlag(ID_SENSE)) {
            if (isBroken() || isCursed()) return 0L;
        }

        /* Base value (see above) */
        value = GetValueBase();
    }


    /* Apply discount (if any) */
    if (GetDiscount()) value -= (value * GetDiscount() / 100L);


    /* Return the final value */
    return value;
}





/*
 * Determine if an item can "absorb" a second item
 *
 * See "object_absorb()" for the actual "absorption" code.
 *
 * If permitted, we allow wands/staffs (if they are known to have equal
 * charges) and rods (if fully charged) to combine.
 *
 * Note that rods/staffs/wands are then unstacked when they are used.
 *
 * If permitted, we allow weapons/armor to stack, if they both known.
 *
 * Food, potions, scrolls, and "easy know" items always stack.
 *
 * Chests never stack (for various reasons).
 *
 * We do NOT allow activatable items (artifacts or dragon scale mail)
 * to stack, to keep the "activation" code clean.  Artifacts may stack,
 * but only with another identical artifact (which does not exist).
 *
 * Ego items may stack as long as they have the same ego-item type.
 * This is primarily to allow ego-missiles to stack.
 */
bool object_similar(CItem *i_ptr, CItem *j_ptr)
{
    int total = i_ptr->GetNumber() + j_ptr->GetNumber();


    /* Require identical object types */
    if (i_ptr->GetKIdx() != j_ptr->GetKIdx()) return (0);


    /* Analyze the items */
    switch (i_ptr->GetTval()) {
        /* Chests */
        case TV_CHEST:

            /* Never okay */
            return FALSE;


        /* Food and Potions and Scrolls */
        case TV_FOOD:
        case TV_POTION:
        case TV_SCROLL:

            /* Assume okay */
            break;

        /* Staffs and Wands */
        case TV_STAFF:
        case TV_WAND:

            /* Require knowledge */
            if (!i_ptr->isKnown() || !j_ptr->isKnown()) return (0);

            /* Fall through */

        /* Staffs and Wands and Rods */
        case TV_ROD:

            /* Require permission */
            if (!stack_allow_wands) return (0);

            /* Require identical charges */
            if (i_ptr->GetPval() != j_ptr->GetPval()) return (0);

            /* Probably okay */
            break;


        /* Weapons and Armor */
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

            /* Require permission */
            if (!stack_allow_items) return (0);

            /* XXX XXX XXX Require identical "sense" status */
            /* if ((i_ptr->GetIdent() & ID_SENSE) != */
            /*     (j_ptr->GetIdent() & ID_SENSE)) return (0); */

            /* Fall through */

        /* Rings, Amulets, Lites */
        case TV_RING:
        case TV_AMULET:
        case TV_LITE:

            /* Require full knowledge of both items */
            if (!i_ptr->isKnown() || !j_ptr->isKnown()) return FALSE;

            /* Fall through */

        /* Missiles */
        case TV_BOLT:
        case TV_ARROW:
        case TV_SHOT:

            /* Require identical "bonuses" */
            if (i_ptr->GetToH() != j_ptr->GetToH()) return (FALSE);
            if (i_ptr->GetToD() != j_ptr->GetToD()) return (FALSE);
            if (i_ptr->GetToA() != j_ptr->GetToA()) return (FALSE);

            /* Require identical "pval" code */
            if (i_ptr->GetPval() != j_ptr->GetPval()) return FALSE;

            /* Require identical "artifact" names */
            if (i_ptr->GetName1() != j_ptr->GetName1()) return FALSE;

            /* Require identical "ego-item" names */
            if (i_ptr->GetName2() != j_ptr->GetName2()) return FALSE;

            /* Hack -- Never stack "powerful" items */
            if (i_ptr->GetXtra1() || j_ptr->GetXtra1()) return FALSE;

            /* Hack -- Never stack recharging items */
            if (i_ptr->GetTimeout() || j_ptr->GetTimeout()) return FALSE;

            /* Require identical "values" */
            if (i_ptr->GetAC() != j_ptr->GetAC()) return FALSE;
            if (i_ptr->GetDD() != j_ptr->GetDD()) return FALSE;
            if (i_ptr->GetDS() != j_ptr->GetDS()) return FALSE;

            /* Require identical known status */
            if (i_ptr->isKnown() != j_ptr->isKnown()) return FALSE;

            /* Probably okay */
            break;

        /* Various */
        default:

            /* Require knowledge */
            if (!i_ptr->isKnown() || !j_ptr->isKnown()) return FALSE;

            /* Probably okay */
            break;
    }


    /* Hack -- Require identical "cursed" status */
    if (i_ptr->TestIdentFlag(ID_CURSED) != j_ptr->TestIdentFlag(ID_CURSED)) return FALSE;

    /* Hack -- Require identical "broken" status */
    if (i_ptr->TestIdentFlag(ID_BROKEN) != j_ptr->TestIdentFlag(ID_BROKEN)) return FALSE;


    /* Hack -- require semi-matching "inscriptions" */
    if (i_ptr->GetNote() && j_ptr->GetNote() &&
        !streq(i_ptr->GetNote(), j_ptr->GetNote())) return FALSE;

    /* Hack -- normally require matching "discounts" */
    if (!stack_force_costs &&
        (i_ptr->GetDiscount() != j_ptr->GetDiscount())) {
        return FALSE;
    }


    /* Maximal "stacking" limit */
    if (total >= MAX_STACK_SIZE) return FALSE;


    /* They match, so they must be similar */
    return TRUE;
}


/*
 * Allow one item to "absorb" another, assuming they are similar
 */
void object_absorb(CItem *i_ptr, CItem *j_ptr)
{
    int total = i_ptr->GetNumber() + j_ptr->GetNumber();

    // Add together the item counts
    i_ptr->SetNumber((total < MAX_STACK_SIZE) ? total : (MAX_STACK_SIZE - 1));

    // Blend "known" status
    if (j_ptr->isKnown()) i_ptr->MakeKnown();

    /* Hack -- blend "rumour" status */
    if (j_ptr->TestIdentFlag(ID_RUMOUR)) i_ptr->SetIdentFlag(ID_RUMOUR);

    /* Hack -- blend "mental" status */
    if (j_ptr->TestIdentFlag(ID_MENTAL)) i_ptr->SetIdentFlag(ID_MENTAL);

    /* Hack -- blend "inscriptions" */
    if (j_ptr->GetNote()) i_ptr->SetNote(j_ptr->GetNote());

    /* Hack -- could average discounts XXX XXX XXX */
    /* Hack -- save largest discount XXX XXX XXX */
    if (i_ptr->GetDiscount() < j_ptr->GetDiscount()) {
        i_ptr->SetDiscount(j_ptr->GetDiscount());
    }
}



/*
 * Find the index of the CObjectKind with the given tval and sval
 */
s16b lookup_kind(int tval, int sval)
{
    int k;

    /* Look for it */
    for (k = 1; k < MAX_K_IDX; k++) {
        CObjectKind *k_ptr = &k_info[k];

        /* Found a match */
        if ((k_ptr->tval == tval) && (k_ptr->sval == sval)) return (k);
    }

    /* Oops */
    quit(format("No object (%d,%d)", tval, sval));

    /* Oops */
    return (0);
}


/*
 * Make "i_ptr" a "clean" copy of the given "kind" of object
 */
void CItem::invcopy(int k_idx)
{
    CObjectKind *k_ptr = &k_info[k_idx];

    // Clear the record
    wipe();

    // Save the kind index
    SetKIdx(k_idx);

    // Default "pval"
    SetPval(k_ptr->pval);

    // Default number
    SetNumber(1);

    // Default magic
    SetToH(k_ptr->to_h);
    SetToD(k_ptr->to_d);
    SetToA(k_ptr->to_a);

    // Default power
    SetAC(k_ptr->ac);
    SetDD(k_ptr->dd);
    SetDS(k_ptr->ds);

    // Hack -- worthless items are always "broken"
    if (k_ptr->cost <= 0) SetIdentFlag(ID_BROKEN);

    // Hack -- cursed items are always "cursed"
    if (k_ptr->flags3 & TR3_CURSED) SetIdentFlag(ID_CURSED);
}





/*
 * Help determine an "enchantment bonus" for an object.
 *
 * To avoid floating point but still provide a smooth distribution of bonuses,
 * we simply round the results of division in such a way as to "average" the
 * correct floating point value.
 *
 * This function has been changed.  It uses "randnor()" to choose values from
 * a normal distribution, whose mean moves from zero towards the max as the
 * level increases, and whose standard deviation is equal to 1/4 of the max,
 * and whose values are forced to lie between zero and the max, inclusive.
 *
 * Since the "level" rarely passes 100 before Morgoth is dead, it is very
 * rare to get the "full" enchantment on an object, even a deep levels.
 *
 * It is always possible (albeit unlikely) to get the "full" enchantment.
 *
 * A sample distribution of values from "m_bonus(10, N)" is shown below:
 *
 *   N       0     1     2     3     4     5     6     7     8     9    10
 * ---    ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----
 *   0   66.37 13.01  9.73  5.47  2.89  1.31  0.72  0.26  0.12  0.09  0.03
 *   8   46.85 24.66 12.13  8.13  4.20  2.30  1.05  0.36  0.19  0.08  0.05
 *  16   30.12 27.62 18.52 10.52  6.34  3.52  1.95  0.90  0.31  0.15  0.05
 *  24   22.44 15.62 30.14 12.92  8.55  5.30  2.39  1.63  0.62  0.28  0.11
 *  32   16.23 11.43 23.01 22.31 11.19  7.18  4.46  2.13  1.20  0.45  0.41
 *  40   10.76  8.91 12.80 29.51 16.00  9.69  5.90  3.43  1.47  0.88  0.65
 *  48    7.28  6.81 10.51 18.27 27.57 11.76  7.85  4.99  2.80  1.22  0.94
 *  56    4.41  4.73  8.52 11.96 24.94 19.78 11.06  7.18  3.68  1.96  1.78
 *  64    2.81  3.07  5.65  9.17 13.01 31.57 13.70  9.30  6.04  3.04  2.64
 *  72    1.87  1.99  3.68  7.15 10.56 20.24 25.78 12.17  7.52  4.42  4.62
 *  80    1.02  1.23  2.78  4.75  8.37 12.04 27.61 18.07 10.28  6.52  7.33
 *  88    0.70  0.57  1.56  3.12  6.34 10.06 15.76 30.46 12.58  8.47 10.38
 *  96    0.27  0.60  1.25  2.28  4.30  7.60 10.77 22.52 22.51 11.37 16.53
 * 104    0.22  0.42  0.77  1.36  2.62  5.33  8.93 13.05 29.54 15.23 22.53
 * 112    0.15  0.20  0.56  0.87  2.00  3.83  6.86 10.06 17.89 27.31 30.27
 * 120    0.03  0.11  0.31  0.46  1.31  2.48  4.60  7.78 11.67 25.53 45.72
 * 128    0.02  0.01  0.13  0.33  0.83  1.41  3.24  6.17  9.57 14.22 64.07
 */
static s16b m_bonus(int max, int level)
{
    int bonus, stand, extra, value;


    // Paranoia -- enforce maximal "level"
    if (level > MAX_DEPTH - 1) level = MAX_DEPTH - 1;


    // The "bonus" moves towards the max
    bonus = ((max * level) / MAX_DEPTH);

    // Hack -- determine fraction of error
    extra = ((max * level) % MAX_DEPTH);

    // Hack -- simulate floating point computations
    if (rand_int(MAX_DEPTH) < extra) bonus++;


    /* The "stand" is equal to one quarter of the max */
    stand = (max / 4);

    /* Hack -- determine fraction of error */
    extra = (max % 4);

    /* Hack -- simulate floating point computations */
    if (rand_int(4) < extra) stand++;


    /* Choose an "interesting" value */
    value = randnor(bonus, stand);

    /* Enforce the minimum value */
    if (value < 0) return 0;

    /* Enforce the maximum value */
    if (value > max) return max;

    /* Result */
    return value;
}


/*
 * Mega-Hack -- Attempt to create one of the "Special Items"
 *
 * We are only called from "place_object()", and we assume that
 * "apply_magic()" is called immediately after we return.
 *
 * Note -- see "make_artifact()" and "apply_magic()"
 */
bool CItem::make_artifact_special(void)
{
    int i;
    int k_idx = 0;


    /* No artifacts in the town */
    if (!dun_level) return FALSE;

    /* Check the artifact list (just the "specials") */
    for (i = 0; i < ART_MIN_NORMAL; i++) {
        artifact_type *a_ptr = &a_info[i];

        /* Skip "empty" artifacts */
        if (!a_ptr->name) continue;

        /* Cannot make an artifact twice */
        if (a_ptr->cur_num) continue;

        /* XXX XXX Enforce minimum "depth" (loosely) */
        if (a_ptr->level > dun_level) {
            /* Acquire the "out-of-depth factor" */
            int d = (a_ptr->level - dun_level) * 2;

            /* Roll for out-of-depth creation */
            if (rand_int(d) != 0) continue;
        }

        /* Artifact "rarity roll" */
        if (rand_int(a_ptr->rarity) != 0) return (0);

        /* Find the base object */
        k_idx = lookup_kind(a_ptr->tval, a_ptr->sval);

        /* Assign the template */
        invcopy(k_idx);

        /* Mega-Hack -- mark the item as an artifact */
        SetName1(i);

        /* Success */
        return TRUE;
    }

    /* Failure */
    return FALSE;
}


/*
 * Attempt to change an object into an artifact
 *
 * This routine should only be called by "apply_magic()"
 *
 * Note -- see "make_artifact_special()" and "apply_magic()"
 */
bool CItem::make_artifact(void)
{
    int i;


    /* No artifacts in the town */
    if (!dun_level) return FALSE;

    /* Paranoia -- no "plural" artifacts */
    if (GetNumber() != 1) return FALSE;

    /* Check the artifact list (skip the "specials") */
    for (i = ART_MIN_NORMAL; i < MAX_A_IDX; i++) {
        artifact_type *a_ptr = &a_info[i];

        /* Skip "empty" items */
        if (!a_ptr->name) continue;

        /* Cannot make an artifact twice */
        if (a_ptr->cur_num) continue;

        /* Must have the correct fields */
        if (a_ptr->tval != GetTval()) continue;
        if (a_ptr->sval != GetSval()) continue;

        /* XXX XXX Enforce minimum "depth" (loosely) */
        if (a_ptr->level > dun_level) {
            /* Acquire the "out-of-depth factor" */
            int d = (a_ptr->level - dun_level) * 2;

            /* Roll for out-of-depth creation */
            if (rand_int(d) != 0) continue;
        }

        /* We must make the "rarity roll" */
        if (rand_int(a_ptr->rarity) != 0) continue;

        /* Mark the item as an artifact */
        SetName1(i);

        /* Success */
        return TRUE;
    }

    /* Failure */
    return FALSE;
}


/*
 * Charge a new wand.
 */
void CItem::charge_up(void)
{
    SetPval(2 + randint(1 + (60 / (get_k_ptr()->level + 2))));
}



/*
 * Apply magic to an item known to be a "weapon"
 *
 * Hack -- note special base damage dice boosting
 * Hack -- note special processing for weapon/digger
 */
void CItem::a_m_aux_1(int level, int power)
{
    int tohit1 = randint(5) + m_bonus(5, level);
    int todam1 = randint(5) + m_bonus(5, level);

    int tohit2 = m_bonus(10, level);
    int todam2 = m_bonus(10, level);


    /* Good */
    if (power > 0) {
        /* Enchant */
        SetToH(GetToH() + tohit1);
        SetToD(GetToD() + todam1);

        /* Very good */
        if (power > 1) {
            /* Enchant again */
            SetToH(GetToH() + tohit2);
            SetToD(GetToD() + todam2);
        }
    }

    /* Cursed */
    else if (power < 0) {
        /* Penalize */
        SetToH(GetToH() - tohit1);
        SetToD(GetToD() - todam1);

        /* Very cursed */
        if (power < -1) {
            /* Penalize again */
            SetToH(GetToH() - tohit2);
            SetToD(GetToD() - todam2);
        }

        /* Cursed (if "bad") */
        if (GetToH() + GetToD() < 0) {
            SetIdentFlag(ID_CURSED);
        }
    }


    /* Analyze type */
    switch (GetTval()) {
        case TV_DIGGING:

            /* Very good */
            if (power > 1) {
                /* Special Ego-item */
                SetName2(EGO_DIGGING);
            }

            /* Bad */
            else if (power < 0) {
                /* Hack -- Reverse digging bonus */
                SetPval(-GetPval());
            }

            /* Very bad */
            else if (power < -1) {
                /* Hack -- Horrible digging bonus */
                SetPval(-5 - randint(5));
            }

            break;


        case TV_HAFTED:
        case TV_POLEARM:
        case TV_SWORD:
            /* Good */
            if (power > 0) {
                /* Hack -- Super-charge the damage dice */
                while (rand_int(8L * GetDD() * GetDS()) == 0) {
                    SetDD(GetDD() + 1);
                }

                /* Hack -- Lower the damage dice */
                if (GetDD() > 9) SetDD(9);
            }

            /* Very Good */
            if (power > 1) {
                /* Roll for an ego-item */
                switch (randint(29)) {
                    case 1: SetName2(EGO_HA); break;
                    case 2: SetName2(EGO_DF); break;
                    case 3: SetName2(EGO_BRAND_ACID); break;
                    case 4: SetName2(EGO_BRAND_ELEC); break;
                    case 5: SetName2(EGO_BRAND_FIRE); break;
                    case 6: SetName2(EGO_BRAND_COLD); break;

                    case 7: case 8:
                        SetName2(EGO_SLAY_ANIMAL);
                        break;

                    case 9: case 10:
                        SetName2(EGO_SLAY_DRAGON);
                        break;

                    case 11: case 12:
                        SetName2(EGO_SLAY_EVIL);
                        break;

                    case 13: case 14:
                        SetName2(EGO_SLAY_UNDEAD);
                        break;

                    case 15: case 16: case 17:
                        SetName2(EGO_SLAY_ORC);
                        break;

                    case 18: case 19: case 20:
                        SetName2(EGO_SLAY_TROLL);
                        break;

                    case 21: case 22: case 23:
                        SetName2(EGO_SLAY_GIANT);
                        break;

                    case 24: case 25: case 26:
                        SetName2(EGO_SLAY_DEMON);
                        break;

                    case 27: SetName2(EGO_WEST); break;
                    case 28: SetName2(EGO_BLESS_BLADE); break;
                    case 29: SetName2(EGO_ATTACKS); break;
                }
            }

            /* Very cursed */
            else if (power < -1) {
                /* Roll for ego-item */
                if (rand_int(MAX_DEPTH) < level) {
                    SetName2(EGO_MORGUL);
                }
            }

            break;


        case TV_BOW:

            /* Very good */
            if (power > 1) {
                /* Roll for ego-item */
                switch (randint(10)) {
                    case 1: SetName2(EGO_EXTRA_MIGHT); break;
                    case 2: SetName2(EGO_EXTRA_SHOTS); break;

                    case 3: case 4: case 5: case 6:
                        SetName2(EGO_VELOCITY);
                        break;

                    case 7: case 8: case 9: case 10:
                        SetName2(EGO_ACCURACY);
                        break;
                }
            }

            break;


        case TV_BOLT:
        case TV_ARROW:
        case TV_SHOT:

            /* Very good */
            if (power > 1) {
                /* Roll for ego-item */
                switch (randint(11)) {
                    case 1: case 2: case 3:
                        SetName2(EGO_WOUNDING);
                        break;

                    case 4: SetName2(EGO_FLAME); break;
                    case 5: SetName2(EGO_FROST); break;

                    case 6: case 7:
                        SetName2(EGO_HURT_ANIMAL);
                        break;

                    case 8: case 9:
                        SetName2(EGO_HURT_EVIL);
                        break;

                    case 10: SetName2(EGO_HURT_DRAGON); break;
                    case 11: SetName2(EGO_VENOM); break;
                }

                /* Hack -- super-charge the damage dice */
                while (rand_int(4L * GetDD() * GetDS()) == 0) {
                    SetDD(GetDD() + 1);
                }

                /* Hack -- restrict the damage dice */
                if (GetDD() > 9) SetDD(9);
            }

            // Very cursed
            else if (power < -1) {
                /* Roll for ego-item */
                if (rand_int(MAX_DEPTH) < level) {
                    SetName2(EGO_BACKBITING);
                }
            }

            break;
    }
}


/*
 * Apply magic to an item known to be "armor"
 *
 * Hack -- note special processing for crown/helm
 * Hack -- note special processing for robe of permanence
 */
void CItem::a_m_aux_2(int level, int power)
{
    int toac1 = randint(5) + m_bonus(5, level);

    int toac2 = m_bonus(10, level);


    /* Good */
    if (power > 0) {
        /* Enchant */
        SetToA(GetToA() + toac1);

        /* Very good */
        if (power > 1) {
            /* Enchant again */
            SetToA(GetToA() + toac2);
        }
    }

    /* Cursed */
    else if (power < 0) {
        /* Penalize */
        SetToA(GetToA() - toac1);

        /* Very cursed */
        if (power < -1) {
            /* Penalize again */
            SetToA(GetToA() - toac2);
        }

        /* Cursed (if "bad") */
        if (GetToA() < 0) SetIdentFlag(ID_CURSED);
    }


    /* Analyze type */
    switch (GetTval()) {
        case TV_DRAG_ARMOR:
            break;


        case TV_HARD_ARMOR:
        case TV_SOFT_ARMOR:

            /* Very good */
            if (power > 1) {
                /* Hack -- Try for "Robes of the Magi" */
                if ((GetTval() == TV_SOFT_ARMOR) &&
                    (GetSval() == SV_ROBE) &&
                    percent(10))
                {
                    SetName2(EGO_PERMANENCE);
                    break;
                }

                /* Roll for ego-item */
                switch (randint(19)) {
                    case 1: case 2: case 3: case 4:
                        SetName2(EGO_RESIST_ACID);
                        break;

                    case 5: case 6: case 7: case 8:
                        SetName2(EGO_RESIST_ELEC);
                        break;

                    case 9: case 10: case 11: case 12:
                        SetName2(EGO_RESIST_FIRE);
                        break;

                    case 13: case 14: case 15: case 16:
                        SetName2(EGO_RESIST_COLD);
                        break;

                    case 17: case 18:
                        SetName2(EGO_RESISTANCE);
                        break;

                    default:
                        SetName2(EGO_ELVENKIND);
                        break;
                }
            }

            break;


        case TV_SHIELD:

            /* Very good */
            if (power > 1) {
                /* Roll for ego-item */
                switch (randint(10)) {
                    case 1: SetName2(EGO_ENDURE_ACID); break;

                    case 2: case 3: case 4:
                        SetName2(EGO_ENDURE_ELEC);
                        break;

                    case 5: case 6:
                        SetName2(EGO_ENDURE_FIRE);
                        break;

                    case 7: case 8: case 9:
                        SetName2(EGO_ENDURE_COLD);
                        break;

                    case 10:
                        SetName2(EGO_ENDURANCE);
                        break;
                }
            }

            break;


        case TV_GLOVES:

            /* Very good */
            if (power > 1) {
                /* Roll for ego-item */
                switch (randint(10)) {
                    case 1: SetName2(EGO_FREE_ACTION); break;
                    case 2: SetName2(EGO_FREE_ACTION); break;
                    case 3: SetName2(EGO_FREE_ACTION); break;
                    case 4: SetName2(EGO_FREE_ACTION); break;

                    case 5: case 6: case 7:
                        SetName2(EGO_SLAYING);
                        break;

                    case 8: case 9:
                        SetName2(EGO_AGILITY);
                        break;

                    case 10:
                        SetName2(EGO_POWER);
                        break;
                }
            }

            /* Very cursed */
            else if (power < -1) {
                /* Roll for ego-item */
                switch (randint(2)) {
                    case 1: SetName2(EGO_CLUMSINESS); break;
                    case 2: SetName2(EGO_WEAKNESS); break;
                }
            }

            break;


        case TV_BOOTS:

            /* Very good */
            if (power > 1) {
                /* Roll for ego-item */
                switch (randint(24)) {
                    case 1:
                        SetName2(EGO_SPEED);
                        break;

                    case 2: case 3: case 4: case 5:
                        SetName2(EGO_MOTION);
                        break;

                    case 6: case 7: case 8: case 9:
                    case 10: case 11: case 12: case 13:
                        SetName2(EGO_QUIET);
                        break;

                    default: SetName2(EGO_SLOW_DESCENT); break;
                }
            }

            /* Very cursed */
            else if (power < -1) {
                /* Roll for ego-item */
                switch (randint(3)) {
                    case 1: SetName2(EGO_NOISE); break;
                    case 2: SetName2(EGO_SLOWNESS); break;
                    case 3: SetName2(EGO_ANNOYANCE); break;
                }
            }

            break;


        case TV_CROWN:

            /* Very good */
            if (power > 1) {
                /* Roll for ego-item */
                switch (randint(8)) {
                    case 1: SetName2(EGO_MAGI); break;
                    case 2: SetName2(EGO_MIGHT); break;
                    case 3: SetName2(EGO_TELEPATHY); break;
                    case 4: SetName2(EGO_REGENERATION); break;
                    case 5: SetName2(EGO_LORDLINESS); break;
                    case 6: SetName2(EGO_LORDLINESS); break;
                    case 7: SetName2(EGO_SEEING); break;
                    case 8: SetName2(EGO_SEEING); break;
                }
            }

            /* Very cursed */
            else if (power < -1) {
                /* Roll for ego-item */
                switch (randint(7)) {
                    case 1: case 2:
                        SetName2(EGO_STUPIDITY);
                        break;
                    case 3: case 4:
                        SetName2(EGO_NAIVETY);
                        break;
                    case 5:
                        SetName2(EGO_UGLINESS);
                        break;
                    case 6:
                        SetName2(EGO_SICKLINESS);
                        break;
                    case 7:
                        SetName2(EGO_TELEPORTATION);
                        break;
                }
            }

            break;


        case TV_HELM:

            /* Very good */
            if (power > 1) {
                /* Roll for ego-item */
                switch (randint(14)) {
                    case 1: case 2:
                        SetName2(EGO_INTELLIGENCE);
                        break;

                    case 3: case 4:
                        SetName2(EGO_WISDOM);
                        break;

                    case 5: case 6:
                        SetName2(EGO_BEAUTY);
                        break;

                    case 7: case 8:
                        SetName2(EGO_SEEING);
                        break;

                    case 9: case 10:
                        SetName2(EGO_LITE);
                        break;

                    default:
                        SetName2(EGO_INFRAVISION);
                        break;
                }
            }

            /* Very cursed */
            else if (power < -1) {
                /* Roll for ego-item */
                switch (randint(7)) {
                    case 1: case 2:
                        SetName2(EGO_STUPIDITY);
                        break;
                    case 3: case 4:
                        SetName2(EGO_NAIVETY);
                        break;
                    case 5:
                        SetName2(EGO_UGLINESS);
                        break;
                    case 6:
                        SetName2(EGO_SICKLINESS);
                        break;
                    case 7:
                        SetName2(EGO_TELEPORTATION);
                        break;
                }
            }

            break;


        case TV_CLOAK:

            /* Very good */
            if (power > 1) {
                /* Roll for ego-item */
                switch (randint(17)) {
                    case 1: case 2: case 3: case 4:
                    case 5: case 6: case 7: case 8:
                        SetName2(EGO_PROTECTION);
                        break;

                    case 9: case 10: case 11: case 12:
                    case 13: case 14: case 15: case 16:
                        SetName2(EGO_STEALTH);
                        break;

                    case 17:
                        SetName2(EGO_AMAN);
                        break;
                }
            }

            /* Very cursed */
            else if (power < -1) {
                /* Choose some damage */
                switch (randint(3)) {
                    case 1:
                        SetName2(EGO_IRRITATION);
                        break;
                    case 2:
                        SetName2(EGO_VULNERABILITY);
                        break;
                    case 3:
                        SetName2(EGO_ENVELOPING);
                        break;
                }
            }

            break;
    }
}



/*
 * Apply magic to an item known to be a "ring" or "amulet"
 *
 * Hack -- note special "pval boost" code for ring of speed
 * Hack -- note that some items must be cursed (or blessed)
 */
void CItem::a_m_aux_3(int level, int power)
{
    bool gen_cursed = (power < 0);

    /* Apply magic (good or bad) according to type */
    switch (GetTval()) {
        case TV_RING:
            /* Analyze */
            switch (GetSval()) {
                /* Strength, Constitution, Dexterity, Intelligence */
                case SV_RING_STR:
                case SV_RING_CON:
                case SV_RING_DEX:
                case SV_RING_INT:
                    /* Stat bonus */
                    SetPval(1 + m_bonus(5, level));

                    /* Cursed */
                    if (gen_cursed) {
                        // Broken and cursed
                        SetIdentFlag(ID_BROKEN);
                        SetIdentFlag(ID_CURSED);

                        // Reverse pval
                        SetPval(-GetPval());
                    }

                    break;

                /* Ring of Speed */
                case SV_RING_SPEED:
                    // Base speed (1 to 10)
                    SetPval(randint(5) + m_bonus(5, level));

                    // Super-charge the ring
                    while (percent(50)) SetPval(GetPval() + 1);

                    // Cursed Ring
                    if (gen_cursed) {
                        // Broken and cursed
                        SetIdentFlag(ID_BROKEN);
                        SetIdentFlag(ID_CURSED);

                        // Reverse pval
                        SetPval(-GetPval());

                        break;
                    }

                    break;

                // Searching
                case SV_RING_SEARCHING:
                    // Bonus to searching
                    SetPval(1 + m_bonus(5, level));

                    // Cursed
                    if (gen_cursed) {
                        // Broken and cursed
                        SetIdentFlag(ID_BROKEN);
                        SetIdentFlag(ID_CURSED);

                        // Reverse pval
                        SetPval(-GetPval());
                    }

                    break;

                // Flames, Acid, Ice
                case SV_RING_FLAMES:
                case SV_RING_ACID:
                case SV_RING_ICE:
                    /* Bonus to armor class */
                    SetToA(5 + randint(5) + m_bonus(10, level));
                    break;

                // WOE
                case SV_RING_WOE:
                    // Broken and cursed
                    SetIdentFlag(ID_BROKEN);
                    SetIdentFlag(ID_CURSED);

                    // Penalize
                    SetToA(-5 - m_bonus(10, level));
                    SetPval(-1 - m_bonus(5, level));

                    break;

                // Damage
                case SV_RING_DAMAGE:
                    // Bonus to damage
                    SetToD(5 + randint(5) + m_bonus(6, level));

                    // Cursed
                    if (gen_cursed) {
                        // Broken and cursed
                        SetIdentFlag(ID_BROKEN);
                        SetIdentFlag(ID_CURSED);

                        // Reverse bonus
                        SetToD(-GetToD());
                    }

                    break;

                // Ring of Accuracy
                case SV_RING_ACCURACY:
                    // Bonus to hit
                    SetToH(5 + randint(5) + m_bonus(6, level));

                    // Cursed
                    if (gen_cursed) {
                        // Broken and cursed
                        SetIdentFlag(ID_BROKEN);
                        SetIdentFlag(ID_CURSED);

                        // Reverse tohit
                        SetToH(-GetToH());
                    }

                    break;

                // Ring of Protection
                case SV_RING_PROTECTION:
                    // Bonus to armor class
                    SetToA(5 + randint(5) + m_bonus(10, level));

                    // Cursed
                    if (gen_cursed) {
                        // Broken and cursed
                        SetIdentFlag(ID_BROKEN);
                        SetIdentFlag(ID_CURSED);

                        // Reverse toac
                        SetToA(-GetToA());
                    }

                    break;

                // Ring of Slaying
                case SV_RING_SLAYING:
                    // Bonus to damage and to hit
                    SetToD(5 + randint(5) + m_bonus(6, level));
                    SetToH(5 + randint(5) + m_bonus(6, level));

                    /* Cursed */
                    if (gen_cursed) {
                        /* Broken and cursed */
                        SetIdentFlag(ID_BROKEN);
                        SetIdentFlag(ID_CURSED);

                        /* Reverse bonuses */
                        SetToH(-GetToH());
                        SetToD(-GetToD());
                    }

                    break;
            }

            break;

        case TV_AMULET:
            // Analyze
            switch (GetSval()) {
                // Amulet of wisdom/charisma
                case SV_AMULET_WISDOM:
                case SV_AMULET_CHARISMA:
                    SetPval(1 + m_bonus(5, level));

                    // Cursed
                    if (gen_cursed) {
                        // Broken and cursed
                        SetIdentFlag(ID_BROKEN);
                        SetIdentFlag(ID_CURSED);

                        // Reverse bonuses
                        SetPval(-GetPval());
                    }

                    break;

                // Amulet of searching
                case SV_AMULET_SEARCHING:
                    SetPval(randint(5) + m_bonus(5, level));

                    // Cursed
                    if (gen_cursed) {
                        // Broken and cursed
                        SetIdentFlag(ID_BROKEN);
                        SetIdentFlag(ID_CURSED);

                        // Reverse bonuses
                        SetPval(-GetPval());
                    }

                    break;

                // Amulet of the Magi -- never cursed
                case SV_AMULET_THE_MAGI:
                    SetPval(randint(5) + m_bonus(5, level));
                    SetToA(randint(5) + m_bonus(5, level));

                    break;

                // Amulet of Doom -- always cursed
                case SV_AMULET_DOOM:
                    // Broken and cursed
                    SetIdentFlag(ID_BROKEN);
                    SetIdentFlag(ID_CURSED);

                    // Penalize
                    SetPval(-randint(5) - m_bonus(5, level));
                    SetToA(-randint(5) - m_bonus(5, level));

                    break;
            }

            break;
    }
}


/*
 * Apply magic to an item known to be "boring"
 *
 * Hack -- note the special code for various items
 */
void CItem::a_m_aux_4(int level, int power)
{
    // Apply magic (good or bad) according to type
    switch (GetTval()) {
        case TV_LITE:
            // Hack -- Torches -- random fuel
            if (GetSval() == SV_LITE_TORCH) {
                if (GetPval()) {
                    SetPval(randint(GetPval()));
                }
            }

            // Hack -- Lanterns -- random fuel
            if (GetSval() == SV_LITE_LANTERN) {
                if (GetPval()) {
                    SetPval(randint(GetPval()));
                }
            }

            break;


        case TV_WAND:
        case TV_STAFF:
            // charge wands/staves
            charge_up();

            break;


        case TV_CHEST:

            int lev = get_k_ptr()->level;

            // Hack -- skip ruined chests
            if (lev <= 0) break;

            // Hack -- pick a "difficulty"
            SetPval(randint(lev));

            // Never exceed "difficulty" of 59
            if (GetPval() > 59) SetPval(59);

            break;
    }
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
 * "good" and "great" arguments are false.  As a total hack, if "great" is
 * true, then the item gets 3 extra "attempts" to become an artifact.
 */
void CItem::apply_magic(int lev, byte flags)
{
    int i, rolls, f1, f2, power;


    /* Maximum "level" for various things */
    if (lev > MAX_DEPTH - 1) lev = MAX_DEPTH - 1;


    // Base chance of being "good"
    f1 = lev + 10;

    /* Maximal chance of being "good" */
    if (f1 > 75) f1 = 75;

    /* Base chance of being "great" */
    f2 = f1 / 2;

    /* Maximal chance of being "great" */
    if (f2 > 20) f2 = 20;


    /* Assume normal */
    power = 0;

    /* Roll for "good" */
    if ((flags & AM_FORCE_GOOD) || percent(f1)) {
        /* Assume "good" */
        power = 1;

        /* Roll for "great" */
        if ((flags & AM_FORCE_GREAT) || percent(f2)) power = 2;
    }

    /* Roll for "cursed" */
    else if (percent(f1)) {
        /* Assume "cursed" */
        power = -1;

        /* Roll for "broken" */
        if (percent(f2)) power = -2;
    }


    /* Assume no rolls */
    rolls = 0;

    /* Get one roll if excellent */
    if (power >= 2) rolls = 1;

    /* Hack -- Get four rolls if forced great */
    if (flags & AM_FORCE_GREAT) rolls = 4;

    /* Hack -- Get no rolls if not allowed */
    if (!(flags & AM_ALLOW_ART) || isArtifact()) rolls = 0;

    /* Roll for artifacts if allowed */
    for (i = 0; i < rolls; i++) {
        /* Roll for an artifact */
        if (make_artifact()) break;
    }


    /* Hack -- analyze artifacts */
    if (isArtifact()) {
        artifact_type *a_ptr = get_a_ptr();

        /* Hack -- Mark the artifact as "created" */
        a_ptr->cur_num = 1;

        /* Extract the other fields */
        SetPval(a_ptr->pval);
        SetAC(a_ptr->ac);
        SetDD(a_ptr->dd);
        SetDS(a_ptr->ds);
        SetToA(a_ptr->to_a);
        SetToH(a_ptr->to_h);
        SetToD(a_ptr->to_d);

        /* Hack -- extract the "broken" flag */
        if (!a_ptr->cost) SetIdentFlag(ID_BROKEN);

        /* Hack -- extract the "cursed" flag */
        if (a_ptr->flags3 & TR3_CURSED) SetIdentFlag(ID_CURSED);

        /* Done */
        return;
    }


    /* Apply magic */
    switch (GetTval()) {
        case TV_DIGGING:
        case TV_HAFTED:
        case TV_POLEARM:
        case TV_SWORD:
        case TV_BOW:
        case TV_SHOT:
        case TV_ARROW:
        case TV_BOLT:
            if (power) a_m_aux_1(lev, power);
            break;

        case TV_DRAG_ARMOR:
        case TV_HARD_ARMOR:
        case TV_SOFT_ARMOR:
        case TV_SHIELD:
        case TV_HELM:
        case TV_CROWN:
        case TV_CLOAK:
        case TV_GLOVES:
        case TV_BOOTS:
            if (power) a_m_aux_2(lev, power);
            break;

        case TV_RING:
        case TV_AMULET:
            if (!power && percent(50)) power = -1;
            a_m_aux_3(lev, power);
            break;

        default:
            a_m_aux_4(lev, power);
            break;
    }


    /* Hack -- analyze ego-items */
    if (isEgoItem()) {
        ego_item_type *e_ptr = get_e_ptr();

        /* Hack -- extra powers */
        switch (GetName2()) {
            // Weapon (Holy Avenger)
            case EGO_HA:
                SetXtra1(EGO_XTRA_SUSTAIN);
                break;

            // Weapon (Defender)
            case EGO_DF:
                SetXtra1(EGO_XTRA_SUSTAIN);
                break;

            // Weapon (Blessed)
            case EGO_BLESS_BLADE:
                SetXtra1(EGO_XTRA_ABILITY);
                break;

            // Robe of Permanance
            case EGO_PERMANENCE:
                SetXtra1(EGO_XTRA_POWER);
                break;

            // Armor of Elvenkind
            case EGO_ELVENKIND:
                SetXtra1(EGO_XTRA_POWER);
                break;

            // Crown of the Magi
            case EGO_MAGI:
                SetXtra1(EGO_XTRA_ABILITY);
                break;

            // Cloak of Aman
            case EGO_AMAN:
                SetXtra1(EGO_XTRA_POWER);
                break;
        }

        /* Randomize the "xtra" power */
        if (GetXtra1()) SetXtra2(randint(256));

        /* Hack -- acquire "broken" flag */
        if (!e_ptr->cost) SetIdentFlag(ID_BROKEN);

        /* Hack -- acquire "cursed" flag */
        if (e_ptr->flags3 & TR3_CURSED) SetIdentFlag(ID_CURSED);

        /* Hack -- apply extra penalties if needed */
        if (isCursed() || isBroken()) {
            // Hack -- obtain bonuses
            if (e_ptr->max_to_h) {
                SetToH(GetToH() - randint(e_ptr->max_to_h));
            }
            if (e_ptr->max_to_d) {
                SetToD(GetToD() - randint(e_ptr->max_to_d));
            }
            if (e_ptr->max_to_a) {
                SetToA(GetToA() - randint(e_ptr->max_to_a));
            }

            /* Hack -- obtain pval */
            if (e_ptr->max_pval) {
                SetPval(GetPval() - randint(e_ptr->max_pval));
            }
        }

        /* Hack -- apply extra bonuses if needed */
        else {
            /* Hack -- obtain bonuses */
            if (e_ptr->max_to_h) {
                SetToH(GetToH() + randint(e_ptr->max_to_h));
            }
            if (e_ptr->max_to_d) {
                SetToD(GetToD() + randint(e_ptr->max_to_d));
            }
            if (e_ptr->max_to_a) {
                SetToA(GetToA() + randint(e_ptr->max_to_a));
            }

            /* Hack -- obtain pval */
            if (e_ptr->max_pval) {
                SetPval(GetPval() + randint(e_ptr->max_pval));
            }
        }

        /* Done */
        return;
    }


    /* Examine real objects */
    if (exists()) {
        CObjectKind *k_ptr = get_k_ptr();

        /* Hack -- acquire "broken" flag */
        if (!k_ptr->cost) SetIdentFlag(ID_BROKEN);

        /* Hack -- acquire "cursed" flag */
        if (k_ptr->flags3 & TR3_CURSED) SetIdentFlag(ID_CURSED);
    }
}



/*
 * Hack -- determine if a template is "good"
 */
static bool kind_is_good(int k_idx)
{
    CObjectKind *k_ptr = &k_info[k_idx];

    // Analyze the item type
    switch (k_ptr->tval) {
        /* Armor -- Good unless damaged */
        case TV_HARD_ARMOR:
        case TV_SOFT_ARMOR:
        case TV_DRAG_ARMOR:
        case TV_SHIELD:
        case TV_CLOAK:
        case TV_BOOTS:
        case TV_GLOVES:
        case TV_HELM:
        case TV_CROWN:
            if (k_ptr->to_a < 0) return FALSE;
            return TRUE;

        /* Weapons -- Good unless damaged or a digger */
        case TV_BOW:
        case TV_SWORD:
        case TV_HAFTED:
        case TV_POLEARM:
            if (k_ptr->to_h < 0) return FALSE;
            if (k_ptr->to_d < 0) return FALSE;
            return TRUE;

        /* Ammo -- Arrows/Bolts are good unless damaged */
        case TV_BOLT:
        case TV_ARROW:
            if (k_ptr->to_h < 0) return FALSE;
            if (k_ptr->to_d < 0) return FALSE;
            return TRUE;

        /* Books -- High level books are good */
        case TV_MAGIC_BOOK:
        case TV_PRAYER_BOOK:
            if (k_ptr->sval >= SV_BOOK_MIN_GOOD) return TRUE;
            return FALSE;

        /* Rings -- Rings of Speed are good */
        case TV_RING:
            return (k_ptr->sval == SV_RING_SPEED);

        /* Amulets -- Amulets of the Magi are good */
        case TV_AMULET:
            return (k_ptr->sval == SV_AMULET_THE_MAGI);
    }

    /* Assume not good */
    return FALSE;
}



/*
 * Attempt to place an object (normal or good/great) at the given location.
 *
 * This routine plays nasty games to generate the "special artifacts".
 *
 * This routine uses olev for the "generation level".
 *
 * This routine requires a clean floor grid destination.
 */
void place_object(int y, int x, bool good, bool great, int olev)
{
    int prob, base;
    CItem *i_ptr;
    CGrid *g_ptr;
    byte flags;


    /* Paranoia -- check bounds */
    if (!in_bounds(y, x)) return;

    /* Require clean floor space */
    if (!clean_stackable_grid_bold(y, x)) return;


    /* Chance of "special object" */
    prob = (good ? 10 : 1000);

    /* Base level for the object */
    base = (good ? (olev + 10) : olev);

    
    // Allocate an object
    i_ptr = new CItem;


    /* Generate a special object, or a normal object */
    if (!one_in(prob) || !i_ptr->make_artifact_special()) {
        int k_idx;

        /* Require "good" object */
        if (good) get_obj_num_hook = kind_is_good;

        /* Pick a random object */
        k_idx = get_obj_num(base);

        /* Hack -- forget the hook */
        get_obj_num_hook = NULL;

        /* Handle failure */
        if (!k_idx) {
            delete i_ptr;
            return;
        }

        /* Prepare the object */
        i_ptr->invcopy(k_idx);
    }

    // Calculate flags
    flags = AM_ALLOW_ART;
    if (good) flags |= AM_FORCE_GOOD;
    if (great) flags |= AM_FORCE_GREAT;

    /* Apply magic (allow artifacts) */
    i_ptr->apply_magic(olev, flags);

    /* Hack -- generate multiple missiles */
    switch (i_ptr->GetTval()) {
        case TV_SHOT:
        case TV_ARROW:
        case TV_BOLT:
            i_ptr->SetNumber(damroll(6, 7));
    }

    
    // Set the location
    i_ptr->SetLocation(x, y);

    // Set up the grid
    g_ptr = &cave[y][x];
    i_ptr->next_i_ptr = g_ptr->i_ptr;
    g_ptr->i_ptr = i_ptr;
}



/*
 * Scatter some "great" objects near the player
 */
void acquirement(int y1, int x1, int num, bool great)
{
    int y, x, i, d;

    /* Scatter some objects */
    for (; num > 0; num--) {
        /* Check near the player for space */
        for (i = 0; i < 25; i++) {
            /* Increasing Distance */
            d = (i + 4) / 5;

            // Pick a location
            scatter(&y, &x, y1, x1, d, 0);

            // Must have a clean grid
            if (!clean_stackable_grid_bold(y, x)) continue;

            // Place a good (or great) object
            place_object(y, x, TRUE, great, dun_level);

            // Notice
            note_spot(y, x);

            /* Placement accomplished */
            break;
        }
    }
}





/*
 * Places a random trap at the given location.
 *
 * The location must be a valid, empty, clean, floor grid.
 *
 * Note that all traps start out as "invisible" and "untyped", and then
 * when they are "discovered" (by detecting them or setting them off),
 * the trap is "instantiated" as a visible, "typed", trap.
 */
void place_trap(int y, int x)
{
    CGrid *g_ptr;

    /* Paranoia -- verify location */
    if (!in_bounds(y, x)) return;

    /* Require empty, clean, floor grid */
    if (!naked_grid_bold(y, x)) return;

    /* Access the grid */
    g_ptr = &cave[y][x];

    /* Place an invisible trap */
    g_ptr->set_feat(CF_TRAP_INVIS);
}


/*
 * XXX XXX XXX Do not use these hard-coded values.
 */
const int OBJ_GOLD_LIST = 480; // First "gold" entry
const int MAX_GOLD      = 18;  // Number of "gold" entries

/*
 * Places a treasure (Gold or Gems) at given location
 * The location must be a valid, empty, floor grid.
 */
void place_gold(int x, int y, int olev)
{
    int i;
    s32b base;
    CGrid *g_ptr;
    CItem *i_ptr;


    // Paranoia -- check bounds
    if (!in_bounds(y, x)) return;

    // Require clean floor grid
    if (!clean_stackable_grid_bold(y, x)) return;


    // Hack -- Pick a Treasure variety
    i = ((randint(olev + 2) + 2) / 2) - 1;

    // Apply "extra" magic
    if (one_in(GREAT_OBJ)) {
        i += randint(olev + 1);
    }

    // Hack -- Creeping Coins only generate "themselves"
    if (coin_type) i = coin_type;

    // Do not create "illegal" Treasure Types
    if (i >= MAX_GOLD) i = MAX_GOLD - 1;


    // Make an object
    i_ptr = new CItem(OBJ_GOLD_LIST + i);

    // Set the location
    i_ptr->SetLocation(x, y);

    // Set up the grid
    g_ptr = &cave[y][x];
    i_ptr->next_i_ptr = g_ptr->i_ptr;
    g_ptr->i_ptr = i_ptr;

    /* Hack -- Base coin cost */
    base = k_info[OBJ_GOLD_LIST+i].cost;

    /* Determine how much the treasure is "worth" */
    i_ptr->SetPval(base + damroll(8, base));
}



/*
 * Let an item 'i_ptr' fall to the ground at or near (y,x).
 * The initial location is assumed to be "in_bounds()".
 *
 * This function takes a parameter "chance".  This is the percentage
 * chance that the item will "disappear" instead of drop.  If the object
 * has been thrown, then this is the chance of disappearance on contact.
 *
 * Hack -- this function uses "chance" to determine if it should produce
 * some form of "description" of the drop event (under the player).
 *
 * This function should probably be broken up into a function to determine
 * a "drop location", and several functions to actually "drop" an object.
 *
 * XXX XXX XXX Consider allowing objects to combine on the ground.
 */
void drop_near(CItem *i_ptr, int chance, int y, int x)
{
    int k, d, nx, ny, x1, y1;
    CGrid *g_ptr;
    bool flag = FALSE;


    /* Start at the drop point */
    nx = x1 = x;
    ny = y1 = y;

    /* See if the object "survives" the fall */
    if (i_ptr->isArtifact() || !percent(chance)) {
        /* Start at the drop point */
        ny = y1 = y; nx = x1 = x;

        /* Try (20 times) to find an adjacent usable location */
        for (k = 0; !flag && (k < 20); ++k) {
            // Distance distribution
            d = ((k + 14) / 15);

            // Pick a "nearby" location
            scatter(&ny, &nx, y1, x1, d, 0);

            /* Require clean floor space */
            if (!clean_stackable_grid_bold(ny, nx)) continue;

            /* Here looks good */
            flag = TRUE;
        }
    }

    /* Try really hard to place an artifact */
    if (!flag && i_ptr->isArtifact()) {
        /* Start at the drop point */
        ny = y1 = y;  nx = x1 = x;

        /* Try really hard to drop it */
        for (k = 0; !flag && (k < 1000); k++) {
            d = 1;

            /* Pick a location */
            scatter(&ny, &nx, y1, x1, d, 0);

            /* Do not move through walls */
            if (!floor_grid_bold(ny, nx)) continue;

            /* Hack -- "bounce" to that location */
            y1 = ny; x1 = nx;

            /* Get the cave grid */
            g_ptr = &cave[ny][nx];

            /* Use it */
            flag = TRUE;
        }

        /* Hack -- Artifacts will destroy ANYTHING to stay alive */
        // XXX -- MJC -- won't this be bad if it's a wall?
        if (!flag) {
            char i_name[80];

            /* Location */
            nx = x;
            ny = y;

            /* Always okay */
            flag = TRUE;

            /* Description */
            i_ptr->object_desc(i_name, FALSE, 0);

            /* Message */
            msg_format("The %s crashes to the floor.", i_name);
        }
    }


    /* Successful drop */
    if (flag) {
        /* Assume fails */
        flag = FALSE;

        /* Make a new object */
        CItem *j_ptr = new CItem;

        // Copy
        *j_ptr = *i_ptr;
        i_ptr = j_ptr;

        // Locate
        i_ptr->SetLocation(nx, ny);

        // Place
        g_ptr = &cave[ny][nx];
        i_ptr->next_i_ptr = g_ptr->i_ptr;
        g_ptr->i_ptr = i_ptr;

        // Note the spot
        note_spot(ny, nx);

        //# Drop

        /* Success */
        flag = TRUE;
    }


    /* Poor little object */
    if (!flag) {
        char i_name[80];

        /* Describe */
        i_ptr->object_desc(i_name, FALSE, 0);

        /* Message */
        msg_format("The %s disappear%s.",
            i_name, ((i_ptr->GetNumber() == 1) ? "s" : ""));
    }
}




/*
 * Hack -- instantiate a trap
 *
 * XXX XXX XXX This routine should be redone to reflect trap "level".
 * That is, it does not make sense to have spiked pits at 50 feet.
 */
void pick_trap(int y, int x)
{
    int f;

    CGrid *g_ptr = &cave[y][x];

    // Paranoia -- Verify terrain
    if (g_ptr->get_feat() != CF_TRAP_INVIS) return;

    // Pick a trap
    while (1) {
        // Hack -- pick a trap
        f = 0x10 + rand_int(16);

        // Hack -- no trap doors on quest levels
        if ((f == CF_TRAP_TRAP_DOOR) && is_quest(dun_level)) continue;

        // Hack -- no trap doors on the deepest level
        if ((f == CF_TRAP_TRAP_DOOR) && (dun_level >= MAX_DEPTH-1)) continue;

        // Done
        break;
    }

    // Activate the trap
    g_ptr->set_feat(f);

    // Notice
    note_spot(y, x);
}




/*
 * Describe the charges on an item in the inventory.
 */
void inven_item_charges(int item)
{
    CItem *i_ptr = &inventory[item];

    /* Require staff/wand */
    if ((i_ptr->GetTval() != TV_STAFF) && (i_ptr->GetTval() != TV_WAND)) {
        return;
    }

    /* Require known item */
    if (!i_ptr->isKnown()) return;

    /* Multiple charges */
    if (i_ptr->GetPval() != 1) {
        /* Print a message */
        msg_format("You have %d charges remaining.", i_ptr->GetPval());
    }

    /* Single charge */
    else {
        /* Print a message */
        msg_format("You have %d charge remaining.", i_ptr->GetPval());
    }
}


/*
 * Describe an item in the inventory.
 */
void inven_item_describe(int item)
{
    CItem *i_ptr = &inventory[item];
    char i_name[80];

    /* Get a description */
    i_ptr->object_desc(i_name, TRUE, 3);

    /* Print a message */
    msg_format("You have %s.", i_name);
}


/*
 * Increase the "number" of an item in the inventory
 */
void inven_item_increase(int item, int num)
{
    CItem *i_ptr = &inventory[item];

    /* Apply */
    num += i_ptr->GetNumber();

    /* Bounds check */
    if (num > 255) num = 255;
    else if (num < 0) num = 0;

    /* Un-apply */
    num -= i_ptr->GetNumber();

    /* Change the number and weight */
    if (num) {
        /* Add the number */
        i_ptr->SetNumber(i_ptr->GetNumber() + num);

        /* Recalculate bonuses */
        p_ptr->set_update(p_ptr->get_update() | PU_BONUS);

        /* Recalculate mana XXX */
        p_ptr->set_update(p_ptr->get_update() | PU_MANA);

        /* Combine the pack */
        p_ptr->set_notice(p_ptr->get_notice() | PN_COMBINE);
    }
}


/*
 * Erase an inventory slot if it has no more items
 */
void inven_item_optimize(int item)
{
    CItem *i_ptr = &inventory[item];

    /* Only optimize real items */
    if (!i_ptr->exists()) return;

    /* Only optimize empty items */
    if (i_ptr->GetNumber()) return;

    /* The item is in the pack */
    if (item < INVEN_WIELD) {
        int i;

        /* One less item */
        inven_cnt--;

        /* Slide everything down */
        for (i = item; i < INVEN_PACK; i++) {
            /* Structure copy */
            inventory[i] = inventory[i+1];
        }

        /* Erase the "final" slot */
        inventory[i].wipe();
    }

    /* The item is being wielded */
    else {
        // One less item
        equip_cnt--;

        // Erase the empty slot 
        inventory[item].wipe();

        // Recalculate bonuses, torch, mana XXX
        p_ptr->set_update(p_ptr->get_update() | PU_BONUS | PU_TORCH | PU_MANA);
    }
}


/*
 * Describe the charges on an item on the floor.
 */
void floor_item_charges(CItem *i_ptr)
{
    /* Require staff/wand */
    if ((i_ptr->GetTval() != TV_STAFF) && (i_ptr->GetTval() != TV_WAND)) {
        return;
    }

    /* Require known item */
    if (!i_ptr->isKnown()) return;

    /* Multiple charges */
    if (i_ptr->GetPval() != 1) {
        /* Print a message */
        msg_format("There are %d charges remaining.", i_ptr->GetPval());
    }

    /* Single charge */
    else {
        /* Print a message */
        msg_format("There is %d charge remaining.", i_ptr->GetPval());
    }
}



/*
 * Describe an item in the inventory.
 */
void floor_item_describe(CItem *i_ptr)
{
    char i_name[80];

    /* Get a description */
    i_ptr->object_desc(i_name, TRUE, 3);

    /* Print a message */
    msg_format("You see %s.", i_name);
}


/*
 * Increase the "number" of an item on the floor
 */
void floor_item_increase(CItem *i_ptr, int num)
{
    /* Apply */
    num += i_ptr->GetNumber();

    /* Bounds check */
    if (num > 255) num = 255;
    else if (num < 0) num = 0;

    /* Un-apply */
    num -= i_ptr->GetNumber();

    /* Change the number */
    i_ptr->SetNumber(i_ptr->GetNumber() + num);
}


/*
 * Optimize an item on the floor (destroy "empty" items)
 */
void floor_item_optimize(CItem *i_ptr)
{
    /* Paranoia -- be sure it exists */
    if (!i_ptr->exists()) return;

    /* Only optimize empty items */
    if (i_ptr->GetNumber()) return;

    /* Delete it */
    delete_object(i_ptr);
}





/*
 * Check if we have space for an item in the pack without overflow
 */
bool inven_carry_okay(CItem *i_ptr)
{
    int i;

    /* Empty slot? */
    if (inven_cnt < INVEN_PACK) return (TRUE);

    /* Similar slot? */
    for (i = 0; i < INVEN_PACK; i++) {
        /* Get that item */
        CItem *j_ptr = &inventory[i];

        /* Check if the two items can be combined */
        if (object_similar(j_ptr, i_ptr)) return TRUE;
    }

    /* Nope */
    return FALSE;
}


/*
 * Add an item to the players inventory, and return the slot used.
 *
 * If the new item can combine with an existing item in the inventory,
 * it will do so, using "object_similar()" and "object_absorb()", otherwise,
 * the item will be placed into the "proper" location in the inventory.
 *
 * This function can be used to "over-fill" the player's pack, but only
 * once, and such an action must trigger the "overflow" code immediately.
 * Note that when the pack is being "over-filled", the new item must be
 * placed into the "overflow" slot, and the "overflow" must take place
 * before the pack is reordered, but (optionally) after the pack is
 * combined.  This may be tricky.  See "dungeon.c" for info.
 */
s16b inven_carry(CItem *i_ptr)
{
    int         i, j, k;
    int         n = -1;

    CItem *j_ptr;


    /* Check for combining */
    for (j = 0; j < INVEN_PACK; j++) {
        j_ptr = &inventory[j];

        /* Skip empty items */
        if (!j_ptr->exists()) continue;

        /* Hack -- track last item */
        n = j;

        /* Check if the two items can be combined */
        if (object_similar(j_ptr, i_ptr)) {
            /* Combine the items */
            object_absorb(j_ptr, i_ptr);

            /* Recalculate bonuses */
            p_ptr->set_update(p_ptr->get_update() | PU_BONUS);

            /* Success */
            return (j);
        }
    }


    // Paranoia
    if (inven_cnt > INVEN_PACK) return (-1);


    /* Find an empty slot */
    for (j = 0; j <= INVEN_PACK; j++) {
        j_ptr = &inventory[j];

        /* Use it if found */
        if (!j_ptr->exists()) break;
    }

    /* Use that slot */
    i = j;


    /* Hack -- pre-reorder the pack */
    if (i < INVEN_PACK) {
        s32b i_value, j_value;

        /* Get the "value" of the item */
        i_value = i_ptr->GetValue();

        /* Scan every occupied slot */
        for (j = 0; j < INVEN_PACK; j++) {
            j_ptr = &inventory[j];

            // Use empty slots
            if (!j_ptr->exists()) break;

            // Hack -- readable books always come first
            if ((i_ptr->GetTval() == mp_ptr->spell_book) &&
                (j_ptr->GetTval() != mp_ptr->spell_book)) break;
            if ((j_ptr->GetTval() == mp_ptr->spell_book) &&
                (i_ptr->GetTval() != mp_ptr->spell_book)) continue;

            // Items sort by decreasing type
            if (i_ptr->GetTval() > j_ptr->GetTval()) break;
            if (i_ptr->GetTval() < j_ptr->GetTval()) continue;

            // Non-aware (flavored) items always come last
            if (!i_ptr->isAware()) continue;
            if (!j_ptr->isAware()) break;

            // Items sort by increasing sval
            if (i_ptr->GetSval() < j_ptr->GetSval()) break;
            if (i_ptr->GetSval() > j_ptr->GetSval()) continue;

            // Unidentified objects always come last
            if (!i_ptr->isKnown()) continue;
            if (!j_ptr->isKnown()) break;

            // Hack - otherwise identical rods sort by increasing
            // recharge time  --dsb
            if (i_ptr->GetTval() == TV_ROD) {
                if (i_ptr->GetPval() < j_ptr->GetPval()) break;
                if (i_ptr->GetPval() > j_ptr->GetPval()) continue;
            }

            // Determine the "value" of the pack item
            j_value = j_ptr->GetValue();

            // Items sort by decreasing value
            if (i_value > j_value) break;
            if (i_value < j_value) continue;
        }

        // Use that slot
        i = j;

        // Structure slide (make room)
        for (k = n; k >= i; k--) {
            // Hack -- Slide the item
            inventory[k+1] = inventory[k];
        }

        // Paranoia -- Wipe the new slot
        inventory[i].wipe();
    }


    /* Structure copy to insert the new item */
    inventory[i] = (*i_ptr);

    /* Forget the old location */
    inventory[i].SetLocation(0, 0);

    /* Forget pile info */
    inventory[i].next_i_ptr = 0;

    /* Count the items */
    inven_cnt++;

    /* Recalculate bonuses */
    p_ptr->set_update(p_ptr->get_update() | PU_BONUS);

    /* Reorder pack */
    p_ptr->set_notice(p_ptr->get_update() | PN_REORDER);

    /* Return the slot */
    return i;
}




// Combine items in the pack
// Note special handling of the "overflow" slot
void CPlayer::combine_pack(void)
{
    int i, j;

    CItem *i_ptr, *j_ptr;

    bool flag = FALSE;


    /* Combine the pack (backwards) */
    for (i = INVEN_PACK; i > 0; i--) {
        /* Get the item */
        i_ptr = &inventory[i];

        /* Skip empty items */
        if (!i_ptr->exists()) continue;

        /* Scan the items above that item */
        for (j = 0; j < i; j++) {
            /* Get the item */
            j_ptr = &inventory[j];

            /* Skip empty items */
            if (!j_ptr->exists()) continue;

            /* Can we drop "i_ptr" onto "j_ptr"? */
            if (object_similar(j_ptr, i_ptr)) {
                /* Take note */
                flag = TRUE;

                /* Add together the item counts */
                object_absorb(j_ptr, i_ptr);

                /* One object is gone */
                inven_cnt--;

                /* Erase the last object */
                inventory[i].wipe();

                /* XXX XXX XXX Reorder the pack */
                set_notice(get_notice() | PN_REORDER);

                /* Done */
                break;
            }
        }
    }

    /* Message */
    if (flag) msg_print("You combine some items in your pack.");
}


// Reorder items in the pack
// Note special handling of the "overflow" slot
// Note special handling of empty slots  XXX XXX XXX XXX
void CPlayer::reorder_pack(void)
{
    int i, j, k;
    s32b i_value, j_value;
    CItem *i_ptr, *j_ptr;
    CItem temp;
    bool flag = FALSE;


    /* Re-order the pack (forwards) */
    for (i = 0; i < INVEN_PACK; i++) {
        /* Mega-Hack -- allow "proper" over-flow */
        if ((i == INVEN_PACK) && (inven_cnt == INVEN_PACK)) break;

        /* Get the item */
        i_ptr = &inventory[i];

        /* Skip empty slots */
        if (!i_ptr->exists()) continue;

        /* Get the "value" of the item */
        i_value = i_ptr->GetValue();

        // Scan every occupied slot
        for (j = 0; j < INVEN_PACK; j++) {
            // Get the item already there
            j_ptr = &inventory[j];

            // Use empty slots
            if (!j_ptr->exists()) break;

            // Hack -- readable books always come first
            if ((i_ptr->GetTval() == mp_ptr->spell_book) &&
                (j_ptr->GetTval() != mp_ptr->spell_book)) break;
            if ((j_ptr->GetTval() == mp_ptr->spell_book) &&
                (i_ptr->GetTval() != mp_ptr->spell_book)) continue;

            /* Items sort by decreasing type */
            if (i_ptr->GetTval() > j_ptr->GetTval()) break;
            if (i_ptr->GetTval() < j_ptr->GetTval()) continue;

            /* Non-aware (flavored) items always come last */
            if (!i_ptr->isAware()) continue;
            if (!j_ptr->isAware()) break;

            /* Items sort by increasing sval */
            if (i_ptr->GetSval() < j_ptr->GetSval()) break;
            if (i_ptr->GetSval() > j_ptr->GetSval()) continue;

            /* Unidentified objects always come last */
            if (!i_ptr->isKnown()) continue;
            if (!j_ptr->isKnown()) break;

            /* Hack -- otherwise identical rods sort by increasing
               recharge time  --dsb */
            if (i_ptr->GetTval() == TV_ROD) {
                if (i_ptr->GetPval() < j_ptr->GetPval()) break;
                if (i_ptr->GetPval() > j_ptr->GetPval()) continue;
            }

            // Determine the "value" of the pack item
            j_value = j_ptr->GetValue();

            // Items sort by decreasing value
            if (i_value > j_value) break;
            if (i_value < j_value) continue;
        }

        /* Never move down */
        if (j >= i) continue;

        /* Take note */
        flag = TRUE;

        /* Save the moving item */
        temp = inventory[i];

        /* Structure slide (make room) */
        for (k = i; k > j; k--) {
            /* Slide the item */
            inventory[k] = inventory[k-1];
        }

        /* Insert the moved item */
        inventory[j] = temp;
    }

    /* Message */
    if (flag) msg_print("You reorder some items in your pack.");
}



/*
 * Process an object
 */
void CItem::Process(void)
{
    // Recharge rods on the ground
    if ((GetTval() == TV_ROD) && GetPval()) {
        SetPval(GetPval() - 1);
    }
}


/*
 * Hack -- process the objects
 */
void process_objects(void)
{
    int x, y;
    CItem *i_ptr;

    // Hack -- only every ten turns
    if ((game_turn % 10) != 5) return;

    // Process all objects
    for (x = 0; x < cur_wid; x++) {
        for (y = 0; y < cur_hgt; y++) {
            CGrid *g_ptr = &cave[y][x];

            // Objects in the grid
            i_ptr = g_ptr->i_ptr;
            while (i_ptr) {
                i_ptr->Process();
                i_ptr = i_ptr->next_i_ptr;
            }

            // Objects carried by monsters
            if (g_ptr->m_ptr) {
                i_ptr = g_ptr->m_ptr->i_ptr;
                while (i_ptr) {
                    i_ptr->Process();
                    i_ptr = i_ptr->next_i_ptr;
                }
            }
        }
    }
}

