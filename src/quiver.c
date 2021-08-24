#include "angband.h"

#include <assert.h>

static inv_ptr _inv = NULL;

void quiver_init(void)
{
   inv_free(_inv);
   _inv = inv_alloc("Quiver", INV_QUIVER, QUIVER_MAX); 
}

void quiver_display(doc_ptr doc, obj_p p, int flags)
{
    inv_display(_inv, 1, quiver_max(), p, doc, flags);
}

static obj_ptr _get_quiver(void)
{
    slot_t slot = equip_find_obj(TV_QUIVER, SV_ANY);
    if (!slot) return NULL;
    return equip_obj(slot);
}

/* Adding and removing: Quivers allow a large number of slots
 * (QUIVER_MAX) but restrict the number arrows, etc. The capacity 
 * of the quiver may change as the user finds new and better 
 * quivers in the dungeon. */
bool quiver_likes(obj_ptr obj)
{
    obj_ptr quiver = _get_quiver();
    if (!quiver) return FALSE;
    if (quiver->sval == SV_QUIVER_MAGE)
    {
        /* XXX Mage quiver never auto equips? */
    }
    else
    {
        if (obj->tval != plr->shooter_info.tval_ammo) return FALSE;
        /* Restrict what automatically goes into the quiver a bit. For
         * example, if an Archer is doing a lot of Create Ammo, then it
         * is annoying to have the junk results automatically added. On
         * the other hand, one wants artifact ammo to always add, so we
         * can't just rely on object piles ... */
        if (inv_can_combine(_inv, obj)) return TRUE;
        /* Require identified ammo *after* combining. Early game players shooting
         * piles of un-identified ammo from their quiver should recombine on auto-pickup */
        if (!obj_is_identified(obj)) return FALSE;
        if (obj->inscription && strstr(quark_str(obj->inscription), "=g")) return TRUE;
    }
    return FALSE;
}

bool quiver_tolerates(obj_ptr obj)
{
    obj_ptr quiver = _get_quiver();
    if (!quiver) return FALSE;
    if (quiver->sval == SV_QUIVER_MAGE)
        return obj_is_wand(obj) || obj_is_rod(obj);
    else
        return obj_is_ammo(obj);
}

int quiver_capacity(void)
{
    obj_ptr quiver = _get_quiver();
    if (!quiver) return 0;
    return quiver->xtra4;
}

void quiver_carry(obj_ptr obj)
{
    /* Helper for pack_carry and equip_wield */
    int ct = quiver_count(NULL);
    int cap = quiver_capacity();
    int xtra = 0;
    if (ct >= cap) return;
    if (ct + obj->number > cap)
    {
        xtra = ct + obj->number - cap;
        obj->number -= xtra;
    }
    inv_combine_ex(_inv, obj);
    if (obj->number)
    {
        slot_t slot = inv_add(_inv, obj);
        if (slot)
        {
            obj_ptr new_obj = inv_obj(_inv, slot);
            new_obj->marked |= OM_TOUCHED;
            new_obj->marked &= ~OM_WORN;
            autopick_alter_obj(new_obj, FALSE);
            plr->notice |= PN_OPTIMIZE_QUIVER;
        }
    }
    obj->number += xtra;
    plr->update |= PU_BONUS; /* Weight changed */
    plr->window |= PW_EQUIP; /* a Quiver [32 of 110] */
    plr->notice |= PN_CARRY;
}

bool quiver_check_swap(obj_ptr obj)
{
    obj_ptr quiver = _get_quiver();
    int     ct = quiver_count(NULL);
    if (!quiver) return TRUE;
    /* XXX Swapping from ammo to devices or vice versa is currently clumsy */
    if (ct && obj->sval != quiver->sval)
    {
        msg_print("You must remove the contents of your current quiver first.");
        return FALSE;
    }
    /* Make sure the new quiver can hold the current contents */
    if (ct > obj->xtra4)
    {
        cptr s = quiver->sval == SV_QUIVER_AMMO ? "missiles" : "devices";
        msg_format("Failed! Your current quiver holds %d %s but this quiver "
            "only has a capacity for %d %s.", quiver_count(NULL), s, obj->xtra4, s);
        return FALSE;
    }
    return TRUE;
}

void quiver_remove(slot_t slot)
{
    inv_remove(_inv, slot);
}

void quiver_remove_all(void)
{
    slot_t slot;
    for (slot = 1; slot <= QUIVER_MAX; slot++)
    {
        obj_ptr obj = quiver_obj(slot);

        if (!obj) continue;
        obj->marked |= OM_WORN;
        pack_carry_aux(obj);
        obj_release(obj, OBJ_RELEASE_QUIET);
    }
}

void quiver_drop(obj_ptr obj)
{
    int amt = obj->number;

    assert(obj);
    assert(obj->loc.where == INV_QUIVER);
    assert(obj->number > 0);

    if (obj->number > 1)
    {
        amt = get_quantity(NULL, obj->number);
        if (amt <= 0)
        {
            energy_use = 0;
            return;
        }
    }

    obj_drop(obj, amt);
}

/* Accessing, Iterating, Searching */
obj_ptr quiver_obj(slot_t slot)
{
    return inv_obj(_inv, slot);
}

int quiver_max(void)
{
    return QUIVER_MAX;
}

inv_ptr quiver_filter(obj_p p)
{
    return inv_filter(_inv, p);
}

void quiver_for_each(obj_f f)
{
    inv_for_each(_inv, f);
}

void quiver_for_each_that(obj_f f, obj_p p)
{
    inv_for_each_that(_inv, f, p);
}

slot_t quiver_find_first(obj_p p)
{
    return inv_first(_inv, p);
}

slot_t quiver_find_next(obj_p p, slot_t prev_match)
{
    return inv_next(_inv, p, prev_match);
}

slot_t quiver_find_art(cptr which)
{
    return inv_find_art(_inv, which);
}

slot_t quiver_find_ego(int which)
{
    return inv_find_ego(_inv, which);
}

slot_t quiver_find_obj(int tval, int sval)
{
    return inv_find_obj(_inv, tval, sval);
}

slot_t quiver_find_device(int effect)
{
    int     slot;
    obj_ptr quiver = _get_quiver();

    if (!quiver) return 0;
    if (quiver->sval != SV_QUIVER_MAGE) return 0;

    for (slot = 1; slot <= PACK_MAX; slot++)
    {
        obj_ptr obj = inv_obj(_inv, slot);
        if (!obj) continue;
        if (!obj_is_device(obj)) continue;
        if (!obj_is_known(obj)) continue;
        if (obj->activation.type != effect) continue;
        if (device_sp(obj) < obj->activation.cost) continue;
        return slot;
    }

    return 0;
}

slot_t quiver_random_slot(obj_p p)
{
    return inv_random_slot(_inv, p);
}

/* Optimize */
bool quiver_optimize(void)
{
    if (inv_optimize(_inv))
    {
        /*msg_print("You reorder your quiver.");*/
        return TRUE;
    }
    return FALSE;
}
void quiver_delayed_describe(void)
{
    quiver_for_each(obj_delayed_describe);
}

/* Properties of the Entire Inventory */
int quiver_weight(obj_p p)
{
    slot_t  slot = equip_find_obj(TV_QUIVER, SV_ANY);
    obj_ptr obj;
    if (!slot) return 0;
    obj = equip_obj(slot);
    if (obj->name2 == EGO_QUIVER_PHASE) return 0;
    return inv_weight(_inv, p);
}

int quiver_count(obj_p p)
{
    return inv_count(_inv, p);
}

int quiver_count_slots(obj_p p)
{
    return inv_count_slots(_inv, p);
}

/* Savefiles */
void quiver_load(savefile_ptr file)
{
    inv_load(_inv, file);
}

void quiver_save(savefile_ptr file)
{
    inv_save(_inv, file);
}

