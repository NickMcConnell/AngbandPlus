#include "angband.h"
#include "equip.h"

#include <assert.h>

static plr_attack_info_ptr _hand(int h) { return &plr->attack_info[h]; }
static bool _hand_test(int h, int flag) { return have_flag(_hand(h)->paf_flags, flag); }
static void _hand_set(int h, int flag)  { add_flag(_hand(h)->paf_flags, flag); }

/* Slots on the equipment template now *match* slots in our inventory. */
static equip_template_ptr _template = NULL;
static inv_ptr _inv = NULL;

/************************************************************************
 * Slot Accept Functions
 * XXX Just because EQUIP_SLOT_AMULET accepts whistles doesn't make them
 *     count as amulets for the rest of the system. Same thing with
 *     EQUIP_SLOT_WEAPON_SHIELD and TV_CAPTURE. XXX
 ************************************************************************/
static bool _slot_accept_amulet(obj_ptr obj)
    { return (tv_lookup(obj->tval)->flags & TVF_AMULET) || obj->tval == TV_WHISTLE; }

static bool _slot_accept_anything(obj_ptr obj)
    { return BOOL(tv_lookup(obj->tval)->flags & TVF_WEARABLE); }

static bool _slot_accept_body_armor(obj_ptr obj)
    { return BOOL(tv_lookup(obj->tval)->flags & TVF_BODY_ARMOR); }

static bool _slot_accept_boots(obj_ptr obj)
    { return BOOL(tv_lookup(obj->tval)->flags & TVF_BOOTS); }

static bool _slot_accept_bow(obj_ptr obj)
    { return BOOL(tv_lookup(obj->tval)->flags & TVF_BOW); }

static bool _slot_accept_quiver(obj_ptr obj)
    { return BOOL(tv_lookup(obj->tval)->flags & TVF_QUIVER); }

static bool _slot_accept_cloak(obj_ptr obj)
    { return BOOL(tv_lookup(obj->tval)->flags & TVF_CLOAK); }

static bool _slot_accept_gloves(obj_ptr obj)
    { return BOOL(tv_lookup(obj->tval)->flags & TVF_GLOVES); }

static bool _slot_accept_helmet(obj_ptr obj)
    { return BOOL(tv_lookup(obj->tval)->flags & TVF_HELMET); }

static bool _slot_accept_lite(obj_ptr obj)
    { return BOOL(tv_lookup(obj->tval)->flags & TVF_LIGHT); }

static bool _slot_accept_dark(obj_ptr obj)
{
    u32b flags[OF_ARRAY_SIZE];
    if (!(tv_lookup(obj->tval)->flags & TVF_LIGHT)) return FALSE;
    obj_flags(obj, flags);
    return have_flag(flags, OF_DARKNESS);
}

static bool _slot_accept_ring(obj_ptr obj)
    { return BOOL(tv_lookup(obj->tval)->flags & TVF_RING); }

static bool _slot_accept_weapon(obj_ptr obj)
    { return BOOL(tv_lookup(obj->tval)->flags & TVF_WEAPON); }

static bool _slot_accept_weapon_or_shield(obj_ptr obj)
    { return (tv_lookup(obj->tval)->flags & (TVF_WEAPON | TVF_SHIELD)) || obj->tval == TV_CAPTURE; }

static bool _slot_accept_capture_ball(obj_ptr obj)
    { return obj->tval == TV_CAPTURE; }

static obj_p _accept[EQUIP_SLOT_MAX] = {
    NULL,
    _slot_accept_gloves,
    _slot_accept_weapon_or_shield,
    _slot_accept_ring,
    _slot_accept_bow,
    _slot_accept_amulet,
    _slot_accept_lite,
    _slot_accept_body_armor,
    _slot_accept_cloak,
    _slot_accept_boots,
    _slot_accept_helmet,
    _slot_accept_anything,
    _slot_accept_weapon,
    _slot_accept_capture_ball,
    _slot_accept_quiver,
    _slot_accept_dark,
};

static int _slot_count(obj_ptr obj)
{
    int     ct = 0;
    slot_t  slot;
    for (slot = 1; slot <= _template->max; slot++)
    {
        obj_p p = _accept[_template->slots[slot].type];
        if (p(obj))
            ct++;
    }
    return ct;
}

static bool _can_wield(obj_ptr obj)
{
    if (!obj) return FALSE;
    if (quiver_tolerates(obj)) return TRUE;
    if (_slot_count(obj) > 0) return TRUE;
    return FALSE;
}

static int _get_slots(obj_ptr obj, slot_t slots[EQUIP_MAX + 1])
{
    int    ct = 0;
    slot_t slot = equip_first_slot(obj);

    while (equip_is_valid_slot(slot))
    {
        slots[ct++] = slot;
        slot = equip_next_slot(obj, slot);
    }
    return ct;
}

static void _slot_menu_fn(int cmd, int which, vptr cookie, var_ptr res)
{
    slot_t  slot = ((slot_t*)cookie)[which];
    obj_ptr obj = equip_obj(slot);

    switch (cmd)
    {
    case MENU_KEY:
        var_set_int(res, slot - 1 + 'a');
        break;
    case MENU_TEXT:
        if (obj)
        {
            char buf[MAX_NLEN+50];
            char o_name[MAX_NLEN];
            object_desc(o_name, obj, 0);
            sprintf(buf, "%-14s: %s", sym_str(_template->slots[slot].tag), o_name);
            var_set_string(res, buf);
        }
        else
        {
            char buf[MAX_NLEN+50];
            sprintf(buf, "%-14s:", sym_str(_template->slots[slot].tag));
            var_set_string(res, buf);
        }
        break;
    case MENU_COLOR:
        if (obj)
        {
            if (obj->timeout)
                var_set_int(res, TERM_L_DARK);
            else
                var_set_int(res, tv_color(obj->tval));
        }
        break;
    default:
        default_menu(cmd, which, cookie, res);
    }
}

static slot_t _prompt_wield_slot(obj_ptr obj)
{
    slot_t slots[EQUIP_MAX + 1];
    int    ct = _get_slots(obj, slots);

    if (ct == 1)
        return slots[0];
    else if (ct > 1)
    {
        int    idx;
        menu_t menu = { "Choose an equipment slot", NULL, NULL,
                        _slot_menu_fn, slots, ct };

        idx = menu_choose(&menu);
        if (idx >= 0)
            return slots[idx];
    }

    return 0;
}

/*************************************************************************
 Public Interface
 *************************************************************************/
cptr equip_describe_slot(slot_t slot)
{
    if (_template->slots[slot].type == EQUIP_SLOT_WEAPON_SHIELD || _template->slots[slot].type == EQUIP_SLOT_WEAPON)
    {
        int hand = _template->slots[slot].hand;
        if (_hand_test(hand, PAF_HEAVY))
            return "Just Lifting";
        if (_hand_test(hand, PAF_TWO_HANDS) && !prace_is_(RACE_MON_SWORD))
        {
            if (plr_mon_race_is_("U.bloodthirster"))
                return "Both Paws";
            else
                return "Both Arms";
        }
        if (_hand_test(hand, PAF_CONTROL_MOUNT))
            return "Riding Reins";
    }
    if (_template->slots[slot].type == EQUIP_SLOT_BOW)
    {
        if (plr->shooter_info.heavy_shoot)
            return "Just Holding";
    }
    return sym_str(_template->slots[slot].tag);
}

slot_t equip_find_art(cptr which)
{
    return inv_find_art(_inv, which);
}

slot_t equip_find_ego(int which)
{
    return inv_find_ego(_inv, which);
}

slot_t equip_find_device(int effect)
{
    int slot;
    for (slot = 1; slot <= _template->max; slot++)
    {
        obj_ptr obj = equip_obj(slot);
        if (!obj) continue;
        if (!obj_is_(obj, TV_HAFTED, SV_WIZSTAFF)) continue;
        if (!obj_is_known(obj)) continue;
        if (obj->activation.type != effect) continue;
        if (device_sp(obj) < obj->activation.cost) continue;
        return slot;
    }
    return 0;
}

slot_t equip_find_obj(int tval, int sval)
{
    return inv_find_obj(_inv, tval, sval);
}

slot_t equip_find_first(obj_p p)
{
    return inv_first(_inv, p);
}

slot_t equip_find_next(obj_p p, slot_t prev_match)
{
    return inv_next(_inv, p, prev_match);
}

slot_t equip_first_empty_slot(obj_ptr obj)
{
    slot_t slot;
    for (slot = 1; slot <= _template->max; slot++)
    {
        obj_p p = _accept[_template->slots[slot].type];
        if (p(obj) && !equip_obj(slot))
            return slot;
    }
    return 0;
}

slot_t equip_find_empty_hand(void)
{
    slot_t slot;
    for (slot = 1; slot <= _template->max; slot++)
    {
        if ( (_template->slots[slot].type == EQUIP_SLOT_WEAPON_SHIELD || _template->slots[slot].type == EQUIP_SLOT_WEAPON)
          && !equip_obj(slot) )
        {
            return slot;
        }
    }
    return 0;
}

bool equip_can_wield_kind(int tval, int sval)
{
    object_type forge;
    int         k_idx = lookup_kind(tval, sval);

    object_prep(&forge, k_idx);
    if (equip_first_slot(&forge))
        return TRUE;

    return FALSE;
}

bool equip_has_slot_type(int which)
{
    slot_t slot;
    for (slot = 1; slot <= _template->max; slot++)
    {
        int type = _template->slots[slot].type;
        if (which != EQUIP_SLOT_BOW && type == EQUIP_SLOT_ANY) return TRUE; /* XXX Jellies can't shoot */
        if (which == EQUIP_SLOT_WEAPON && type == EQUIP_SLOT_WEAPON_SHIELD) return TRUE;
        if (which == type) return TRUE;
    }
    return FALSE;
}

int equip_first_slot(obj_ptr obj)
{
    return equip_next_slot(obj, 0);
}

int equip_next_slot(obj_ptr obj, slot_t last)
{
    slot_t slot;
    for (slot = last + 1; slot <= _template->max; slot++)
    {
        obj_p p = _accept[_template->slots[slot].type];
        if (p(obj))
            return slot;
    }
    return 0;
}

bool equip_is_valid_slot(slot_t slot)
{
    if (slot >= 1 && slot <= _template->max)
        return TRUE;
    return FALSE;
}

bool equip_verify_slot(slot_t slot, obj_ptr obj)
{
    if (equip_is_valid_slot(slot))
    {
        obj_p p = _accept[_template->slots[slot].type];
        if (p(obj))
            return TRUE;
    }
    return FALSE;
}

inv_ptr equip_filter(obj_p p)
{
    return inv_filter(_inv, p);
}

void equip_for_each(obj_f f)
{
    inv_for_each(_inv, f);
}

void equip_for_each_that(obj_f f, obj_p p)
{
    inv_for_each_that(_inv, f, p);
}

int equip_weight(obj_p p)
{
    return inv_weight(_inv, p);
}

int equip_count_used(void)
{
    return inv_count_slots(_inv, obj_exists);
}

slot_t equip_is_worn(obj_ptr obj)
{
    slot_t slot;
    for (slot = 1; slot <= _template->max; slot++)
    {
        object_type *o = equip_obj(slot);
        if (o == obj)
            return slot;
    }
    return 0;
}

int equip_which_hand(obj_ptr obj)
{
    slot_t slot;
    for (slot = 1; slot <= _template->max; slot++)
    {
        object_type *o = equip_obj(slot);
        if (o == obj)
            return _template->slots[slot].hand;
    }
    return HAND_NONE;
}

void equip_for_each_slot(slot_f f)
{
    inv_for_each_slot(_inv, f);
}

int equip_random_slot(obj_p p)
{
    return inv_random_slot(_inv, p);
}

obj_ptr equip_obj(slot_t slot)
{
    return inv_obj(_inv, slot);
}

int equip_max(void)
{
    return _template->max;
}

int equip_slot_type(slot_t slot)
{
    if (equip_is_valid_slot(slot))
        return _template->slots[slot].type;
    return EQUIP_SLOT_NONE;
}

bool equip_is_empty_two_handed_slot(int slot)
{
    if (equip_obj(slot)) return FALSE;

    if (_template->slots[slot].type == EQUIP_SLOT_WEAPON_SHIELD)
    {
        int hand = _template->slots[slot].hand;
        int arm = hand / 2;
        int rhand = arm*2;
        int lhand = arm*2 + 1;
        int other_hand = (hand == rhand) ? lhand : rhand;

        if (_hand_test(other_hand, PAF_TWO_HANDS))
            return TRUE;
    }
    return FALSE;
}

/************************************************************************
 * Display Equipment List
 ***********************************************************************/
void equip_ui(void)
{
    gear_ui(INV_EQUIP);
}

void equip_display(doc_ptr doc, obj_p p, int flags)
{
    inv_display(
        _inv,
        1, equip_max(),
        p,
        doc,
        flags
    );
}

/************************************************************************
 * Wielding
 ***********************************************************************/

/* Wielding has the following phases where various things might/must happen */
static obj_ptr _wield_get_obj(void);
static bool    _wield_verify(obj_ptr obj);
static slot_t  _wield_get_slot(obj_ptr obj);
static bool    _wield_confirm(obj_ptr obj, slot_t slot);
static void    _wield_before(obj_ptr obj, slot_t slot);
static void    _wield(obj_ptr obj, slot_t slot);
static void    _wield_after(slot_t slot);

void equip_wield_ui(void)
{
    slot_t  slot;
    obj_ptr obj = _wield_get_obj();

    if (!obj) return;
    if (obj_is_ammo(obj))
    {
        int amt = obj->number;
        assert(equip_find_obj(TV_QUIVER, SV_QUIVER_AMMO));
        if (quiver_capacity() <= quiver_count(NULL))
        {
            msg_print("Your quiver is full.");
            return;
        }
        if (amt == 1 || msg_input_num("Quantity", &amt, 1, obj->number))
        {
            obj_t copy = *obj;
            copy.number = amt;
            quiver_carry(&copy);
            amt -= copy.number; /* quiver might not hold the requested amt */
            obj->number -= amt;
            obj_release(obj, obj->number ? OBJ_RELEASE_DELAYED_MSG : OBJ_RELEASE_QUIET);
            energy_use = 50;
        }
    }
    else if (obj_is_wand(obj) || obj_is_rod(obj))
    {
        assert(equip_find_obj(TV_QUIVER, SV_QUIVER_MAGE));
        assert(obj->number == 1);
        if (quiver_capacity() <= quiver_count(NULL))
        {
            msg_print("Your quiver is full.");
            return;
        }
        else
        {
            obj_t copy = *obj;
            quiver_carry(&copy);
            obj->number = 0;
            obj_release(obj, obj->number ? OBJ_RELEASE_DELAYED_MSG : OBJ_RELEASE_QUIET);
            energy_use = 50;
        }
    }
    else
    {
        if (!_wield_verify(obj)) return;

        slot = _wield_get_slot(obj);
        if (!slot) return;
        if (!_wield_confirm(obj, slot)) return;

        _wield_before(obj, slot);

        energy_use = weaponmaster_wield_hack(obj);
        _wield(obj, slot);

        _wield_after(slot);
        obj_release(obj, OBJ_RELEASE_QUIET);
    }
}

void equip_wield(obj_ptr obj, slot_t slot)
{
    _wield(obj, slot);
    _wield_after(slot);
}

static obj_ptr _wield_get_obj(void)
{
    obj_prompt_t prompt = {0};

    prompt.prompt = "Wear/Wield which item?";
    prompt.error = "You have nothing you can wear or wield.";
    prompt.filter = _can_wield;
    prompt.where[0] = INV_PACK;
    prompt.where[1] = INV_FLOOR;

    obj_prompt(&prompt);
    return prompt.obj;
}

static bool _wield_verify(obj_ptr obj)
{
    if (!psion_can_wield(obj)) return FALSE;
    /* We'll confirm cursed gear later (_wield_confirm)
     * since the user might cancle the slot prompt */
    if (obj->tval == TV_QUIVER)
        return quiver_check_swap(obj);
    return TRUE;
}

static slot_t _wield_get_slot(obj_ptr obj)
{
    slot_t slot = _prompt_wield_slot(obj);
    if (!equip_is_valid_slot(slot)) return 0;
    return slot;
}

static bool _wield_confirm(obj_ptr obj, slot_t slot)
{
    obj_ptr old_obj = inv_obj(_inv, slot);
    char    o_name[MAX_NLEN];

    if (old_obj && have_flag(old_obj->flags, OF_NO_REMOVE)) /* Hack!!!! */
    {
        msg_print("You can't replace yourself with that!");
        return FALSE;
    }

    if (old_obj && obj_is_cursed(old_obj))
    {
        object_desc(o_name, old_obj, OD_OMIT_PREFIX | OD_NAME_ONLY | OD_COLOR_CODED);
        msg_format("The %s you are wearing appears to be cursed.", o_name);
        return FALSE;
    }

    if (confirm_wear)
    {
        bool do_prompt = FALSE;

        if (obj_is_known(obj) && obj_is_cursed(obj))
        {
            do_prompt = TRUE;
        }
        else if (obj->ident & IDENT_SENSE)
        {
            switch (obj->feeling)
            {
            case FEEL_BROKEN:
            case FEEL_BAD:
            case FEEL_TERRIBLE:
            case FEEL_AWFUL:
            case FEEL_CURSED:
                do_prompt = TRUE;
                break;
            }
        }
        if (do_prompt)
        {
            char dummy[MAX_NLEN+80];
            object_desc(o_name, obj, OD_OMIT_PREFIX | OD_NAME_ONLY | OD_COLOR_CODED);
            sprintf(dummy, "Really use the %s {cursed}? ", o_name);
            if (!get_check(dummy)) return FALSE;
        }
    }
    if ( obj_is_specified_art(obj, "~.Vecna")
      || obj_is_specified_art(obj, "].Vecna") )
    {
        char dummy[MAX_NLEN+80];
        object_desc(o_name, obj, OD_OMIT_PREFIX | OD_NAME_ONLY | OD_COLOR_CODED);
        sprintf(dummy, "Really use the %s? It can never be removed! ", o_name);
        if (!get_check(dummy)) return FALSE;
    }
    if ( obj_is_specified_art(obj, "].Stone Mask")
      && obj_is_known(obj)
      && plr->prace != RACE_VAMPIRE
      && plr->prace != RACE_ANDROID
      && !(get_race()->flags & RACE_IS_MONSTER)
      && plr->pclass != CLASS_BLOOD_KNIGHT)
    {
        char dummy[MAX_NLEN+80];
        object_desc(o_name, obj, OD_OMIT_PREFIX | OD_NAME_ONLY);
        msg_format("%s will transforms you into a vampire permanently when equiped.", o_name);
        sprintf(dummy, "Do you become a vampire?");
        if (!get_check(dummy)) return FALSE;
    }
    return TRUE;
}

static void _wield_before(obj_ptr obj, slot_t slot)
{
    if (plr->special_defense & KATA_MUSOU)
        set_action(ACTION_NONE);

    quests_on_get_obj(obj);
}

static void equip_takeoff(slot_t slot);
static void _wield(obj_ptr obj, slot_t slot)
{
    obj_ptr old_obj = inv_obj(_inv, slot);

    if (old_obj)
        equip_takeoff(slot);

    stats_on_use(obj, 1);
    inv_add_at(_inv, obj, slot);
}

static void _wield_after(slot_t slot)
{
    char    o_name[MAX_NLEN];
    obj_ptr obj = inv_obj(_inv, slot);

    obj_learn_equipped(obj);
    stats_on_equip(obj);
    obj->marked |= OM_TOUCHED;
    obj->marked &= ~OM_WORN;

    /* Hack: Extra Might and Weaponmastery require a calc_bonus() to display correctly */
    plr->update |= PU_BONUS;
    handle_stuff();

    object_desc(o_name, obj, OD_COLOR_CODED);
    if (!(plr->pflag & PFLAG_BIRTH))
    {
        if (plr->prace == RACE_MON_SWORD || plr->prace == RACE_MON_RING)
            msg_format("You are %s.", o_name);
        else
            msg_format("You are wearing %s (%c).", o_name, slot - 1 + 'a');
    }

    /* After Effects? */
    if (obj_is_cursed(obj))
    {
        msg_print("Oops! It feels deathly cold!");
        virtue_add(VIRTUE_HARMONY, -1);
        obj->ident |= IDENT_SENSE;
    }
    if (obj_is_specified_art(obj, "].Vecna"))
    {
        cmsg_print(TERM_VIOLET, "You chop off your own hand to wield the Hand of Vecna!");
        if (!plr->no_cut) plr_tim_add(T_CUT, CUT_MORTAL_WOUND);
        plr->update |= PU_INNATE; /* necromancer */
    }
    if (obj_is_specified_art(obj, "~.Vecna"))
    {
        cmsg_print(TERM_VIOLET, "You pluck out your own eye to wield the Eye of Vecna!");
        if (!plr->no_cut) plr_tim_add(T_CUT, CUT_MORTAL_WOUND);
    }
    if ( obj_is_specified_art(obj, "].Stone Mask")
      && plr->prace != RACE_VAMPIRE
      && plr->prace != RACE_ANDROID
      && plr->pclass != CLASS_BLOOD_KNIGHT )
    {
        change_race(RACE_VAMPIRE, "");
    }

    plr->update |= PU_BONUS;
    plr->update |= PU_TORCH;
    plr->update |= PU_MANA;
    plr->redraw |= PR_EQUIPPY;
    plr->window |= PW_INVEN | PW_EQUIP;
    if (obj->tval == TV_LIGHT && display_light_bar)
        plr->redraw |= PR_HEALTH_BARS;
    android_calc_exp();
}

/************************************************************************
 * Unwielding (Take Off)
 ***********************************************************************/

void equip_remove(slot_t slot)
{
    inv_remove(_inv, slot);
    plr->update |= PU_BONUS | PU_TORCH | PU_MANA;
    plr->window |= PW_EQUIP;
    plr->redraw |= PR_EQUIPPY;
    android_calc_exp();
}

/* Unwielding has the following phases where various things might/must happen */
static obj_ptr _unwield_get_obj(void);
static bool    _unwield_verify(obj_ptr obj);
static void    _unwield_before(obj_ptr obj);
static void    _unwield(obj_ptr obj, bool drop);
static void    _unwield_after(void);

void equip_takeoff_ui(void)
{
    obj_ptr obj = _unwield_get_obj();

    if (!obj) return;
    if (obj->tval == TV_QUIVER && quiver_count(NULL))
    {
        msg_print("Your quiver still holds ammo. Remove all the ammo from your quiver first.");
        return;
    }
    energy_use = 50;
    if (!_unwield_verify(obj)) return;

    _unwield_before(obj);
    _unwield(obj, FALSE);
    _unwield_after();
}

bool equip_can_takeoff(obj_ptr obj)
{
    assert(obj->loc.where == INV_EQUIP);
    return _unwield_verify(obj);
}

static void equip_takeoff(slot_t slot)
{
    obj_ptr obj = equip_obj(slot);

    if (obj)
    {
        _unwield(obj, FALSE);
        _unwield_after();
    }
}

void equip_drop(obj_ptr obj)
{
    assert(obj);
    assert(obj->loc.where == INV_EQUIP);
    assert(obj->number == 1);

    if (obj->tval == TV_QUIVER && quiver_count(NULL))
    {
        msg_print("Your quiver still holds ammo. Remove all the ammo from your quiver first.");
        return;
    }
    if (!_unwield_verify(obj)) return;

    _unwield(obj, TRUE);
    _unwield_after();
}

static obj_ptr _unwield_get_obj(void)
{
    obj_prompt_t prompt = {0};

    prompt.prompt = "Take off which item?";
    prompt.error = "You are not wearing anything to take off.";
    prompt.where[0] = INV_EQUIP;
    prompt.where[1] = INV_QUIVER;

    obj_prompt(&prompt);
    return prompt.obj;
}

bool _unwield_verify(obj_ptr obj)
{
    if (!psion_can_wield(obj)) return FALSE;
    if (have_flag(obj->flags, OF_NO_REMOVE))
    {
        msg_print("You try to take yourself off, but fail!");
        energy_use = 0;
        return FALSE;
    }
    if (obj_is_cursed(obj) && obj->loc.where == INV_EQUIP)
    {
        if (obj->curse_flags & OFC_PERMA_CURSE)
        {
            msg_print("Hmmm, it seems to be cursed.");
            energy_use = 0;
            return FALSE;
        }
        if (((obj->curse_flags & OFC_HEAVY_CURSE) && one_in_(7)) || one_in_(4))
        {
            msg_print("You tear the cursed equipment off by sheer strength!");
            obj->ident |= IDENT_SENSE;
            obj->curse_flags = 0L;
            obj->feeling = FEEL_NONE;
            plr->update |= PU_BONUS;
            plr->window |= PW_EQUIP;
            plr->redraw |= PR_EFFECTS;
            msg_print("You break the curse.");
        }
        else
        {
            msg_print("You couldn't remove the equipment.");
            /* still takes energy! */
            return FALSE;
        }
    }
    return TRUE;
}

void _unwield_before(obj_ptr obj)
{
    if (plr->special_defense & KATA_MUSOU)
        set_action(ACTION_NONE);
}

void _unwield(obj_ptr obj, bool drop)
{
    if (obj->loc.where == INV_QUIVER)
    {
        int amt = obj->number;
        assert(equip_find_obj(TV_QUIVER, SV_ANY));
        assert(!drop); /* quiver_drop ... not us. cf do_cmd_drop */
        if (obj->number == 1 || msg_input_num("Quantity", &amt, 1, obj->number))
        {
            obj_t copy = *obj;

            copy.number = amt;
            pack_carry_aux(&copy); /* Hack: don't put ammo back in the quiver if we just removed it! */

            obj->number -= amt;
            obj_release(obj, obj->number ? OBJ_RELEASE_DELAYED_MSG : OBJ_RELEASE_QUIET);
            energy_use = 50;
        }
    }
    else
    {
        char name[MAX_NLEN];
        object_desc(name, obj, OD_COLOR_CODED);
        msg_format("You are no longer wearing %s.", name);
        if (drop)
        {
            obj_drop(obj, obj->number);
        }
        else
        {
            pack_carry_aux(obj);
            obj_release(obj, OBJ_RELEASE_QUIET);
        }
        plr->update |= PU_BONUS | PU_TORCH | PU_MANA;
        plr->redraw |= PR_EQUIPPY;
        plr->window |= PW_EQUIP;
    }
}

void _unwield_after(void)
{
    if (weaponmaster_is_(WEAPONMASTER_SHIELDS))
        handle_stuff(); /* Explain! */
    android_calc_exp();
}

/************************************************************************
 * Calc Bonuses ... This monster has the dreaded RF2_ELDRITCH_HORROR flag!
 ***********************************************************************/

/* Rings and Gloves are somewhat complicated.
   We support an arbitrary number of hands (cf MAX_HANDS) paired
   into an arbitrary number of sets of arms (cf MAX_ARMS). Gloves
   affect both weapons for that arm, possibly with proration. Rings
   affect the hand in question, unless the other hand on that set
   of arms is wielding a weapon two handed.
 */
static void _add_weapon_info_flag(int hand, int flg, bool known)
{
    add_flag(_hand(hand)->obj_flags, flg);
    if (known)
        add_flag(_hand(hand)->obj_known_flags, flg);
}
static void _weapon_info_flag(slot_t slot, u32b flgs[OF_ARRAY_SIZE], u32b known_flgs[OF_ARRAY_SIZE], int flg)
{
    if (have_flag(flgs, flg))
    {
        if (!plr->weapon_ct) /* XXX plr->innate_blows hasn't been built yet ... */
        {
            add_flag(plr->innate_attack_info.obj_flags, flg);
            if (have_flag(known_flgs, flg))
                add_flag(plr->innate_attack_info.obj_known_flags, flg);
        }
        else
        {
            int  hand = _template->slots[slot].hand;
            int  arm = hand / 2;
            int  rhand = arm*2;
            int  lhand = arm*2 + 1;
            int  other_hand = (hand == rhand) ? lhand : rhand;
            bool known = have_flag(known_flgs, flg);

            switch (_template->slots[slot].type)
            {
            case EQUIP_SLOT_GLOVES:
                if (_hand(rhand)->type)
                    _add_weapon_info_flag(rhand, flg, known);
                if (_hand(lhand)->type)
                    _add_weapon_info_flag(lhand, flg, known);
                break;
            case EQUIP_SLOT_RING:
                if (_hand(hand)->type)
                    _add_weapon_info_flag(hand, flg, known);
                else if (_hand_test(other_hand, PAF_TWO_HANDS))
                    _add_weapon_info_flag(other_hand, flg, known);
                break;
            default:
                for (hand = 0; hand < MAX_HANDS; hand++)
                {
                    if (_hand(hand)->type)
                        _add_weapon_info_flag(hand, flg, known);
                }
                break;
            }
        }
    }
}

static void _weaponmastery(slot_t slot, int amt)
{
    int hand = _template->slots[slot].hand;
    int arm = hand / 2;
    int rhand = arm*2;
    int lhand = arm*2 + 1;
    int other_hand = (hand == rhand) ? lhand : rhand;

    switch (_template->slots[slot].type)
    {
    case EQUIP_SLOT_RING:
        if (_hand(hand)->type)
            _hand(hand)->to_dd += amt;
        else if (_hand_test(other_hand, PAF_TWO_HANDS))
            _hand(other_hand)->to_dd += amt;
        else
            plr->innate_attack_info.to_dd += amt;
        break;
    case EQUIP_SLOT_ANY:
        if (_hand(hand)->type)
            _hand(hand)->to_dd += amt;
        else
            plr->innate_attack_info.to_dd += amt;
        break;
    default: /* At the moment, this is just the Robe of the Kamikaze Warrior (+2) */
        if (plr->weapon_ct)
        {
            for (hand = 0; hand < MAX_HANDS; hand++)
            {
                if (_hand(hand)->type)
                    _hand(hand)->to_dd += amt / plr->weapon_ct;
            }
        }
        else if (_hand(hand)->type) /* TODO: I'm not sure martial arts should boost the weapon_ct ... */
            _hand(hand)->to_dd += amt;
        else
            plr->innate_attack_info.to_dd += amt;
    }
}

static void _weapon_bonus_hand(int hand, int to_h, int to_d, bool known)
{
    plr_attack_info_ptr info = _hand(hand);
    info->to_h += to_h;
    info->to_d += to_d;
    if (known)
    {
        info->dis_to_h += to_h;
        info->dis_to_d += to_d;
    }
}
static void _weapon_bonus_innate(int to_h, int to_d, bool known)
{
    plr->innate_attack_info.to_h += to_h;
    plr->innate_attack_info.to_d += to_d;
    if (known)
    {
        plr->innate_attack_info.dis_to_h += to_h;
        plr->innate_attack_info.dis_to_d += to_d;
    }
}

static int _sign(int n)
{
    if (n > 0) return 1;
    if (n < 0) return -1;
    return 0;
}

static void _weapon_bonus(slot_t slot, int to_h, int to_d)
{
    int hand = _template->slots[slot].hand;
    int arm = hand / 2;
    int rhand = arm*2;
    int lhand = arm*2 + 1;
    int other_hand = (hand == rhand) ? lhand : rhand;
    obj_ptr obj = inv_obj(_inv, slot);

    if (!plr->weapon_ct) return;

    switch (_template->slots[slot].type)
    {
    case EQUIP_SLOT_GLOVES:
        if (_hand(rhand)->type && _hand(lhand)->type)
        {
            _weapon_bonus_hand(rhand, (to_h + 1) / 2, (to_d + 1) / 2, obj_is_known(obj));
            _weapon_bonus_hand(lhand, to_h/2, to_d/2, obj_is_known(obj));
        }
        else if (_hand(rhand)->type)
            _weapon_bonus_hand(rhand, to_h, to_d, obj_is_known(obj));
        else if (_hand(lhand)->type)
            _weapon_bonus_hand(lhand, to_h, to_d, obj_is_known(obj));
        break;
    case EQUIP_SLOT_RING:
        if (_hand(hand)->type)
            _weapon_bonus_hand(hand, to_h, to_d, obj_is_known(obj));
        else if (_hand_test(other_hand, PAF_TWO_HANDS))
            _weapon_bonus_hand(other_hand, to_h, to_d, obj_is_known(obj));
        break;
    default:
    {
        int x_to_h = to_h - (to_h/plr->weapon_ct)*plr->weapon_ct;
        int x_to_d = to_d - (to_d/plr->weapon_ct)*plr->weapon_ct;
        for (hand = 0; hand < MAX_HANDS; hand++)
        {
            if (_hand(hand)->type)
                _weapon_bonus_hand(hand, to_h/plr->weapon_ct, to_d/plr->weapon_ct, obj_is_known(obj));
        }
        for (hand = 0; hand < MAX_HANDS && (x_to_h || x_to_d); hand++)
        {
            if (_hand(hand)->type)
            {
                _weapon_bonus_hand(hand, _sign(x_to_h), _sign(x_to_d), obj_is_known(obj));
                if (x_to_h > 0) x_to_h--;
                else if (x_to_h < 0) x_to_h++;
                if (x_to_d > 0) x_to_d--;
                else if (x_to_d < 0) x_to_d++;
            }
        }
    }
    }
}

bool equip_is_valid_hand(int hand)
{
    if (hand == HAND_NONE) return FALSE;
    return _hand(hand)->slot;
}

bool equip_is_empty_hand(int hand)
{
    return equip_is_valid_hand(hand)
        && !equip_obj(_hand(hand)->slot);
}

void equip_xtra_might(int pval)
{
    slot_t slot = equip_find_obj(TV_BOW, SV_ANY);
    if (slot)
    {
        obj_ptr bow = equip_obj(slot);
        plr->shooter_info.to_mult += 20 * pval * bow_energy(bow->sval) / 10000;
    }
}

void equip_calc_bonuses(void)
{
    slot_t slot;
    int    i;

    /* Find the weapons */
    for (slot = 1; slot <= _template->max; slot++)
    {
        if ( _template->slots[slot].type == EQUIP_SLOT_WEAPON_SHIELD
          || _template->slots[slot].type == EQUIP_SLOT_WEAPON )
        {
            obj_ptr obj;
            plr_attack_info_ptr info = _hand(_template->slots[slot].hand);

            info->slot = slot;
            info->type = PAT_NONE;
            obj = equip_obj(slot);

            if (obj)
            {
                if (obj_is_weapon(obj) && weaponmaster_get_toggle() != TOGGLE_SHIELD_BASH)
                    info->type = PAT_WEAPON;
                else if (obj_is_shield(obj))
                {
                    add_flag(info->paf_flags, PAF_SHIELD);
                    if (weaponmaster_get_toggle() == TOGGLE_SHIELD_BASH)
                        info->type = PAT_WEAPON;
                }
            }
            else if (plr_allow_martial_arts())
                info->type = PAT_MONK;
        }
    }

    /* Patch up for monks using weapons (with empty hands too)
     * Flag hands belonging to dual wielding arms. */
    for (i = 0; i < MAX_ARMS; i++)
    {
        plr_attack_info_ptr right = _hand(i*2);
        plr_attack_info_ptr left = _hand(i*2 + 1);

        if (!right->slot || !left->slot) continue; /* e.g. Benedict, the Ideal Warrior */

        if (right->type == PAT_WEAPON && left->type == PAT_WEAPON)
        {
            add_flag(right->paf_flags, PAF_DUAL_WIELDING);
            add_flag(left->paf_flags, PAF_DUAL_WIELDING);
        }
        /* turn off monk attacks if other hand is weaponed */
        else if (right->type == PAT_MONK && left->type == PAT_WEAPON)
        {
            right->type = PAT_NONE;
        }
        else if (left->type == PAT_MONK && right->type == PAT_WEAPON)
        {
            left->type = PAT_NONE;
        }
    }

    /* Control mount with a free hand if possible using the last available hand */
    if (plr->riding)
    {
        plr->riding_ryoute = TRUE;
        if (plr->prace == RACE_MON_RING)
        {
            plr->riding_ryoute = FALSE;
        }
        else if (!(plr->pet_extra_flags & PF_RYOUTE))
        {
            for (i = MAX_HANDS - 1; i >= 0; i--)
            {
                if (equip_is_empty_hand(i))
                {
                    _hand_set(i, PAF_CONTROL_MOUNT);
                    _hand(i)->type = PAT_NONE; /* could have been PAT_MONK */
                    plr->riding_ryoute = FALSE;
                    break;
                }
            }
        }
    }

    /* Figure out which weapons are being used with 2 hands (requires riding checks first) */
    if (CAN_TWO_HANDS_WIELDING())
    {
        for (i = 0; i < MAX_ARMS; i++)
        {
            plr_attack_info_ptr right = _hand(i*2);
            plr_attack_info_ptr left = _hand(i*2 + 1);

            if (right->type == PAT_MONK && left->type == PAT_MONK)
            {
                add_flag(right->paf_flags, PAF_TWO_HANDS);
                left->type = PAT_NONE;
                continue;
            }
            if ( right->type == PAT_WEAPON && !left->type
              && object_allow_two_hands_wielding(equip_obj(right->slot)) )
            {
                if (!have_flag(left->paf_flags, PAF_SHIELD) || weaponmaster_get_toggle() == TOGGLE_SHIELD_BASH)
                    add_flag(right->paf_flags, PAF_TWO_HANDS);
            }
            if ( left->type == PAT_WEAPON && !right->type
              && object_allow_two_hands_wielding(equip_obj(left->slot)) )
            {
                if (!have_flag(right->paf_flags, PAF_SHIELD) || weaponmaster_get_toggle() == TOGGLE_SHIELD_BASH)
                    add_flag(left->paf_flags, PAF_TWO_HANDS);
            }
        }
    }

    /* Hack for Death Swords ... but not Broken Death Swords ;) */
    if (prace_is_(RACE_MON_SWORD) && plr->lev >= 10)
        _hand_set(0, PAF_TWO_HANDS);

    /* Its convenient to have an accurate weapon count later */
    plr->weapon_ct = 0;
    for (i = 0; i < MAX_HANDS; i++)
    {
        if (_hand(i)->type) /* PAT_MONK counts as a weapon */
            plr->weapon_ct++;
    }

    /* Scan equipment for bonuses. */
    for (slot = 1; slot <= _template->max; slot++)
    {
        obj_ptr obj = inv_obj(_inv, slot);
        u32b    flgs[OF_ARRAY_SIZE];
        u32b    known_flgs[OF_ARRAY_SIZE];
        int     bonus_to_h, bonus_to_d;

        if (!obj) continue;

        obj_flags(obj, flgs);
        obj_flags_known(obj, known_flgs);

        plr->cursed |= obj->curse_flags;
        if (plr->cursed)
            plr->redraw |= PR_EFFECTS;
        if (obj_is_specified_art(obj, "|.Chainsword"))
            plr->cursed |= OFC_CHAINSWORD;

        if (have_flag(flgs, OF_LORE2))
            plr->auto_id = TRUE;
        else if (have_flag(flgs, OF_LORE1))
            plr->auto_pseudo_id = TRUE;

        if (obj->name2 == EGO_GLOVES_GIANT)
        {
            int hand = _template->slots[slot].hand;
            int arm = hand / 2;
            int rhand = arm*2;
            int lhand = arm*2 + 1;
            if (_hand_test(rhand, PAF_TWO_HANDS))
                _hand(rhand)->giant_wield += obj->pval;
            else if (_hand_test(lhand, PAF_TWO_HANDS))
                _hand(lhand)->giant_wield += obj->pval;
        }

        if (obj->rune)
        {
            rune_calc_bonuses(obj);
            rune_calc_stats(obj, plr->stat_add);
        }

        if (have_flag(flgs, OF_STR)) plr->stat_add[A_STR] += obj->pval;
        if (have_flag(flgs, OF_INT)) plr->stat_add[A_INT] += obj->pval;
        if (have_flag(flgs, OF_WIS)) plr->stat_add[A_WIS] += obj->pval;
        if (have_flag(flgs, OF_DEX)) plr->stat_add[A_DEX] += obj->pval;
        if (have_flag(flgs, OF_CON)) plr->stat_add[A_CON] += obj->pval;
        if (have_flag(flgs, OF_CHR)) plr->stat_add[A_CHR] += obj->pval;

        if (have_flag(flgs, OF_DEC_STR)) plr->stat_add[A_STR] -= obj->pval;
        if (have_flag(flgs, OF_DEC_INT)) plr->stat_add[A_INT] -= obj->pval;
        if (have_flag(flgs, OF_DEC_WIS)) plr->stat_add[A_WIS] -= obj->pval;
        if (have_flag(flgs, OF_DEC_DEX)) plr->stat_add[A_DEX] -= obj->pval;
        if (have_flag(flgs, OF_DEC_CON)) plr->stat_add[A_CON] -= obj->pval;
        if (have_flag(flgs, OF_DEC_CHR)) plr->stat_add[A_CHR] -= obj->pval;

        if (have_flag(flgs, OF_MAGIC_MASTERY))
            plr->skills.dev += 8*obj->pval;

        if (have_flag(flgs, OF_DEVICE_POWER))
            plr->device_power += obj->pval;

        if (have_flag(flgs, OF_DEC_MAGIC_MASTERY))
        {
            plr->skills.dev -= 8*obj->pval;
            plr->device_power -= obj->pval;
        }

        if (have_flag(flgs, OF_STEALTH)) plr->skills.stl += obj->pval;
        if (have_flag(flgs, OF_DEC_STEALTH)) plr->skills.stl -= obj->pval;
        if (have_flag(flgs, OF_SEARCH))
        {
            plr->skills.srh += (obj->pval * 5);
            plr->skills.fos += (obj->pval * 5);
        }
        if (have_flag(flgs, OF_INFRA)) plr->see_infra += obj->pval;
        if (have_flag(flgs, OF_TUNNEL)) plr->skill_dig += (obj->pval * 20);
        if (have_flag(flgs, OF_SPEED)) plr->pspeed += obj->pval;
        if (have_flag(flgs, OF_DEC_SPEED)) plr->pspeed -= obj->pval;

        if (have_flag(flgs, OF_BLOWS) || have_flag(flgs, OF_DEC_BLOWS))
        {
            int hand = _template->slots[slot].hand;
            int amt = 0;

            if (have_flag(flgs, OF_BLOWS))
                amt += obj->pval * 50;
            if (have_flag(flgs, OF_DEC_BLOWS))
                amt -= obj->pval * 100;
            if (plr->pclass == CLASS_MAULER && amt > 0)
                amt = 0;

            switch (_template->slots[slot].type)
            {
            case EQUIP_SLOT_GLOVES:
            {
                int arm = hand / 2;
                plr_attack_info_ptr right = _hand(arm*2);
                plr_attack_info_ptr left = _hand(arm*2 + 1);
                if (right->type && left->type)
                {
                    if (amt > 0)
                    {
                        right->xtra_blow += amt/2;
                        left->xtra_blow += amt/2;
                    }
                    else
                    {
                        right->xtra_blow += amt;
                        left->xtra_blow += amt;
                    }
                }
                else if (right->type)
                    right->xtra_blow += amt;
                else if (left->type)
                    left->xtra_blow += amt;
                else
                    plr->innate_attack_info.xtra_blow += amt;
                break;
            }
            case EQUIP_SLOT_RING:
                if (_hand(hand)->type)
                    _hand(hand)->xtra_blow += amt;
                else
                {
                    int other_hand;
                    if (hand % 2 == 0)
                        other_hand = hand + 1;
                    else
                        other_hand = hand - 1;
                    if (_hand_test(other_hand, PAF_TWO_HANDS))
                        _hand(other_hand)->xtra_blow += amt;
                    else if (!_hand(other_hand)->type) /* no weapons on this set of arms */
                        plr->innate_attack_info.xtra_blow += amt;
                }
                break;
            case EQUIP_SLOT_WEAPON_SHIELD:
            case EQUIP_SLOT_WEAPON:
                if (_hand(hand)->type)
                    _hand(hand)->xtra_blow += amt;
                break;
            default:
            {
                if (obj_is_weapon(obj)) break; /* Hack for Jellies ... */
                if (plr->weapon_ct)
                {
                    int  j;
                    for (j = 0; j < MAX_HANDS; j++)
                    {
                        if (_hand(j)->type)
                            _hand(j)->xtra_blow += amt/plr->weapon_ct;
                    }
                }
                else
                    plr->innate_attack_info.xtra_blow += amt;
            }
            }
        }

        /* New: Rings and Gloves can grant weapon slays */
        if ( !obj_is_weapon(obj) /* Hack for Jellies ... */
          && !obj_is_bow(obj) )
        {
            vec_ptr v = of_lookup_brand();
            for (i = 0; i < vec_length(v); i++)
            {
                of_info_ptr info = vec_get(v, i);
                _weapon_info_flag(slot, flgs, known_flgs, info->id);
            }
            vec_free(v);
            v = of_lookup_slay();
            for (i = 0; i < vec_length(v); i++)
            {
                of_info_ptr info = vec_get(v, i);
                _weapon_info_flag(slot, flgs, known_flgs, info->id);
            }
            vec_free(v);
        }

        if (have_flag(flgs, OF_XTRA_SHOTS))
            plr->shooter_info.xtra_shot += 15 * obj->pval;

        if (have_flag(flgs, OF_LIFE))
            plr->life += 3*obj->pval;
        if (have_flag(flgs, OF_DEC_LIFE))
            plr->life -= 3*obj->pval;

        if (have_flag(flgs, OF_AGGRAVATE))   plr->cursed |= OFC_AGGRAVATE;
        if (have_flag(flgs, OF_DRAIN_EXP))   plr->cursed |= OFC_DRAIN_EXP;
        if (have_flag(flgs, OF_TY_CURSE))    plr->cursed |= OFC_TY_CURSE;

        if (have_flag(flgs, OF_DEC_MANA))
        {
            /* In general, you need to be a Mage/Priest to gain from wizardstaves, et. al.
             * There are exceptions (e.g. Bards and the two artifact harps; Vampires
             * and The Amulet of the Pitch Dark Night; etc). You will find code
             * for these exceptions in class/race specific calc_bonuses functions. */
            caster_info *caster_ptr = get_caster_info();
            if (caster_ptr && (caster_ptr->options & CASTER_ALLOW_DEC_MANA))
                plr->dec_mana++;
        }
        if (have_flag(flgs, OF_EASY_SPELL))
        {
            caster_info *caster_ptr = get_caster_info();
            if (caster_ptr && (caster_ptr->options & CASTER_ALLOW_DEC_MANA))
                plr->easy_spell++;
        }

        if (have_flag(flgs, OF_SPELL_POWER)) plr->spell_power += obj->pval;
        if (have_flag(flgs, OF_DEC_SPELL_POWER)) plr->spell_power -= obj->pval;
        if (have_flag(flgs, OF_SPELL_CAP))   plr->spell_cap += obj->pval;
        if (have_flag(flgs, OF_DEC_SPELL_CAP))   plr->spell_cap -= obj->pval;
        if (have_flag(flgs, OF_MAGIC_RESISTANCE))   plr->magic_resistance += 5*obj->pval;

        if (have_flag(flgs, OF_XTRA_MIGHT) && obj->tval != TV_BOW)
            equip_xtra_might(obj->pval);

        if (have_flag(flgs, OF_SLOW_DIGEST)) plr->slow_digest = TRUE;
        if (have_flag(flgs, OF_REGEN))       plr->regen += 100;
        if (have_flag(flgs, OF_TELEPATHY))   plr->telepathy = TRUE;
        if (have_flag(flgs, OF_ESP_ANIMAL))  plr->esp_animal = TRUE;
        if (have_flag(flgs, OF_ESP_UNDEAD))  plr->esp_undead = TRUE;
        if (have_flag(flgs, OF_ESP_DEMON))   plr->esp_demon = TRUE;
        if (have_flag(flgs, OF_ESP_ORC))     plr->esp_orc = TRUE;
        if (have_flag(flgs, OF_ESP_TROLL))   plr->esp_troll = TRUE;
        if (have_flag(flgs, OF_ESP_GIANT))   plr->esp_giant = TRUE;
        if (have_flag(flgs, OF_ESP_DRAGON))  plr->esp_dragon = TRUE;
        if (have_flag(flgs, OF_ESP_HUMAN))   plr->esp_human = TRUE;
        if (have_flag(flgs, OF_ESP_EVIL))    plr->esp_evil = TRUE;
        if (have_flag(flgs, OF_ESP_GOOD))    plr->esp_good = TRUE;
        if (have_flag(flgs, OF_ESP_NONLIVING)) plr->esp_nonliving = TRUE;
        if (have_flag(flgs, OF_ESP_UNIQUE))  plr->esp_unique = TRUE;

        if (have_flag(flgs, OF_SEE_INVIS))   plr->see_inv++;
        if (have_flag(flgs, OF_LEVITATION))  plr->levitation = TRUE;
        if (have_flag(flgs, OF_FREE_ACT))    plr->free_act++;
        if (have_flag(flgs, OF_HOLD_LIFE))   plr->hold_life++;
        if (have_flag(flgs, OF_WARNING))
        {
            if (!obj->inscription || !(my_strchr(quark_str(obj->inscription),'$')))
              plr->warning = TRUE;
        }

        if (have_flag(flgs, OF_TELEPORT))
        {
            if (obj_is_cursed(obj)) plr->cursed |= OFC_TELEPORT;
            else
            {
                cptr insc = quark_str(obj->inscription);
                if (obj->inscription && my_strchr(insc, '.')) {}
                else plr->cursed |= OFC_TELEPORT_SELF;
            }
        }

        res_calc_bonuses(flgs);

        if (have_flag(flgs, OF_REFLECT))  plr->reflect = TRUE;
        if (have_flag(flgs, OF_AURA_FIRE))  plr->sh_fire = TRUE;
        if (have_flag(flgs, OF_AURA_ELEC))  plr->sh_elec = TRUE;
        if (have_flag(flgs, OF_AURA_COLD))  plr->sh_cold = TRUE;
        if (have_flag(flgs, OF_AURA_SHARDS))  plr->sh_shards = TRUE;
        if (have_flag(flgs, OF_AURA_REVENGE))  plr->sh_retaliation = TRUE;
        if (have_flag(flgs, OF_NO_MAGIC)) plr->anti_magic = TRUE;
        if (have_flag(flgs, OF_NO_TELE))  plr->anti_tele = TRUE;
        if (have_flag(flgs, OF_NO_SUMMON)) plr->block_summon = TRUE;

        if (have_flag(flgs, OF_SUST_STR)) plr->sustain_str = TRUE;
        if (have_flag(flgs, OF_SUST_INT)) plr->sustain_int = TRUE;
        if (have_flag(flgs, OF_SUST_WIS)) plr->sustain_wis = TRUE;
        if (have_flag(flgs, OF_SUST_DEX)) plr->sustain_dex = TRUE;
        if (have_flag(flgs, OF_SUST_CON)) plr->sustain_con = TRUE;
        if (have_flag(flgs, OF_SUST_CHR)) plr->sustain_chr = TRUE;

        if (have_flag(flgs, OF_DUAL_WIELDING))
        {
            switch (_template->slots[slot].type)
            {
            case EQUIP_SLOT_GLOVES:
            {
                int hand = _template->slots[slot].hand;
                int arm = hand / 2;
                int rhand = arm*2;
                int lhand = arm*2 + 1;

                _hand_set(rhand, PAF_GENJI);
                _hand_set(lhand, PAF_GENJI);
                break;
            }
            default: /* Weaponsmith with their beloved Boots of Genji :) */
                _hand_set(0, PAF_GENJI); /* first set of arms only */
                _hand_set(1, PAF_GENJI);
            }
        }

        if (obj_is_specified_art(obj, "[.Spectral"))
        {
            plr->pass_wall = TRUE;
            plr->no_passwall_dam = TRUE;
        }

        if (obj->curse_flags & OFC_LOW_MAGIC)
        {
            if (obj->curse_flags & OFC_HEAVY_CURSE)
                plr->to_m_chance += 10;
            else
                plr->to_m_chance += 3;
        }

        if (obj->tval == TV_CAPTURE) continue;

        /* Modify the base armor class */
        plr->ac += obj->ac;
        plr->dis_ac += obj->ac;

        /* Apply the bonuses to armor class */
        plr->to_a += obj->to_a;
        if (obj_is_known(obj)) plr->dis_to_a += obj->to_a;

        if (obj->curse_flags & OFC_LOW_MELEE)
        {
            int penalty = (obj->curse_flags & OFC_HEAVY_CURSE) ? -15 : -5;
            switch (_template->slots[slot].type)
            {
            case EQUIP_SLOT_BOW:
                plr->shooter_info.to_h += penalty;
                if (obj->known_curse_flags & OFC_LOW_MELEE)
                    plr->shooter_info.dis_to_h += penalty;
                break;
            case EQUIP_SLOT_WEAPON_SHIELD:
            case EQUIP_SLOT_WEAPON:
            {
                int hand = _template->slots[slot].hand;
                _hand(hand)->to_h += penalty;
                if (obj->known_curse_flags & OFC_LOW_MELEE)
                    _hand(hand)->dis_to_h += penalty;
                break;
            }
            }
        }
        if (obj->curse_flags & OFC_LOW_AC)
        {
            if (obj->curse_flags & OFC_HEAVY_CURSE)
            {
                plr->to_a -= 30;
                if (obj->known_curse_flags & OFC_LOW_AC)
                    plr->dis_to_a -= 30;
            }
            else
            {
                plr->to_a -= 10;
                if (obj->known_curse_flags & OFC_LOW_AC)
                    plr->dis_to_a -= 10;
            }
        }

        /* Hack -- do not apply "weapon" bonuses */
        if (obj_is_weapon(obj)) continue;

        /* Hack -- do not apply "bow" bonuses */
        if (obj_is_bow(obj))
        {
            /* but give some special love to archery! */
            if (obj->name2 == EGO_BOW_ACCURACY)
                plr->shooter_info.crit.freq_add += 100;
            if (obj->name2 == EGO_BOW_VELOCITY)
                plr->shooter_info.crit.qual_add += CRIT_QUAL_ROLL;
            if (have_flag(flgs, OF_WEAPONMASTERY)) /* aka "bowmastery" */
                plr->shooter_info.to_dd += obj->pval;
            continue;
        }

        if (have_flag(flgs, OF_ARCHERY))
        {
            plr->shooter_info.to_h += obj->to_h;
            plr->shooter_info.to_d += obj->to_d;
            if (obj_is_known(obj))
            {
                plr->shooter_info.dis_to_h += obj->to_h;
                plr->shooter_info.dis_to_d += obj->to_d;
            }
        }
        if (have_flag(flgs, OF_SPELL_DAM))
        {
            plr->to_d_spell += obj->to_d;
        }

        if (have_flag(flgs, OF_MELEE))
        {
            bonus_to_h = obj->to_h;
            bonus_to_d = obj->to_d;

            if (plr->pclass == CLASS_NINJA)
            {
                if (obj->to_h > 0) bonus_to_h = (obj->to_h+1)/2;
                if (obj->to_d > 0) bonus_to_d = (obj->to_d+1)/2;
            }

            _weapon_bonus_innate(bonus_to_h, bonus_to_d, obj_is_known(obj));

            plr->to_h_m += bonus_to_h;
            plr->to_d_m += bonus_to_d;

            _weapon_bonus(slot, bonus_to_h, bonus_to_d);
        }
        else if (obj_is_body_armor(obj) && obj->to_h < 0)
        {
            _weapon_bonus_innate(obj->to_h, 0, obj_is_known(obj));
            plr->to_h_m += obj->to_h;
            _weapon_bonus(slot, obj->to_h, 0);
        }

        if (have_flag(flgs, OF_WEAPONMASTERY))
        {
            _weaponmastery(slot, obj->pval);
        }
    }
}

void equip_init(void)
{
    race_t *race_ptr = get_race();
    if (race_ptr->equip_template)
        _template = race_ptr->equip_template;
    else
        _template = equip_template_parse("Standard");

    inv_free(_inv);
    _inv = inv_alloc("Equipment", INV_EQUIP, EQUIP_MAX);
}

/* Attempt to gracefully handle changes to body type between
   releases. New slots may be added, old slots may be removed.
   Slots may be shuffled or have their types changed.
   This is called by process_player() during startup if a
   savefile has been loaded. At this point, drop_near is allowed. */
void equip_on_load(void)
{
    slot_t  slot, max = inv_last(_inv, obj_exists);
    inv_ptr temp = inv_alloc("Temp", INV_EQUIP, EQUIP_MAX);

    for (slot = 1; slot <= max; slot++)
    {
        obj_ptr obj = inv_obj(_inv, slot);
        if (!obj) continue;

        if (slot > _template->max)
        {
            inv_add(temp, obj);
            inv_remove(_inv, slot);
        }
        else
        {
            obj_p p = _accept[_template->slots[slot].type];
            if (!p(obj))
            {
                inv_add(temp, obj);
                inv_remove(_inv, slot);
            }
        }
    }

    max = inv_last(temp, obj_exists);
    for (slot = 1; slot <= max; slot++)
    {
        obj_ptr obj = inv_obj(temp, slot);
        slot_t  new_slot;

        if (!obj) continue;
        new_slot = equip_first_empty_slot(obj);

        if (new_slot)
            inv_add_at(_inv, obj, new_slot);
        else
        {
            char name[MAX_NLEN];
            object_desc(name, obj, OD_COLOR_CODED);
            msg_format("You can no longer wield %s.", name);
            pack_carry(obj);
        }
    }
    inv_free(temp);
}

void equip_on_change_race(void)
{
    equip_template_ptr old_template = _template;
    equip_template_ptr new_template = get_race()->equip_template;

    if (!new_template)
        new_template = equip_template_parse("Standard");

    if (old_template != new_template)
    {
        slot_t  slot;
        inv_ptr temp = inv_copy(_inv);

        inv_clear(_inv);
        _template = new_template;

        for (slot = 1; slot <= old_template->max; slot++)
        {
            obj_ptr src = inv_obj(temp, slot);
            slot_t  new_slot;

            if (!src) continue;
            new_slot = equip_first_empty_slot(src);
            if (new_slot)
                inv_add_at(_inv, src, new_slot);
            else
            {
                char name[MAX_NLEN];

                object_desc(name, src, 0);
                msg_format("You can no longer wield %s.", name);

                /* Mark the object as previously worn. Next time we shift bodies,
                   we will attempt to wield this item again automatically */
                if (!src->inscription)
                    src->marked |= OM_WORN;
                else
                {
                    cptr inscription = quark_str(src->inscription);
                    if ( !strstr(inscription, "@mimic")
                      && !strstr(inscription, "@vampire")
                      && !strstr(inscription, "@bat")
                      && !strstr(inscription, "@mist")
                      && !strstr(inscription, "@wolf") )
                    {
                        src->marked |= OM_WORN;
                    }
                }
                pack_carry(src);
            }
        }
        inv_free(temp);
        temp = NULL;

        if (!equip_find_obj(TV_QUIVER, SV_ANY))
            quiver_remove_all();

        pack_overflow();
        for (slot = 1; slot <= pack_max(); slot++)
        {
            obj_ptr obj = pack_obj(slot);
            slot_t  new_slot;

            if (!obj) continue;
            if (!(obj->marked & OM_WORN)) continue;

            if (obj_is_ammo(obj))
            {
                if (equip_find_obj(TV_QUIVER, SV_ANY))
                {
                    obj->marked &= ~OM_WORN;
                    quiver_carry(obj);
                    obj_release(obj, OBJ_RELEASE_QUIET);
                }
            }
            else
            {
                new_slot = equip_first_empty_slot(obj);
                if (new_slot && obj->number == 1)
                {
                    obj->marked &= ~OM_WORN;
                    equip_wield(obj, new_slot);
                    obj_release(obj, OBJ_RELEASE_QUIET);
                }
            }
        }

        plr->notice |= PN_OPTIMIZE_PACK;
        plr->update |= PU_BONUS | PU_TORCH | PU_MANA;
        plr->redraw |= PR_EQUIPPY;
        plr->window |= PW_INVEN | PW_EQUIP;
        android_calc_exp();
    }
}

void equip_learn_curse(int flag)
{
    slot_t slot;
    for (slot = 1; slot <= _template->max; slot++)
    {
        obj_ptr obj = inv_obj(_inv, slot);
        if (obj && obj_learn_curse(obj, flag))
        {
            char buf[MAX_NLEN];
            object_desc(buf, obj, OD_LORE);
            msg_format("<color:B>You feel that your %s is <color:r>cursed</color>.</color>", buf);
        }
    }
}

void _learn_resist_aux(int obj_flag, cptr desc)
{
    slot_t slot;
    for (slot = 1; slot <= _template->max; slot++)
    {
        obj_ptr obj = inv_obj(_inv, slot);
        if (obj && obj_learn_flag(obj, obj_flag))
        {
            char buf[MAX_NLEN];
            object_desc(buf, obj, OD_LORE);
            msg_format("<color:B>You feel that your %s is %s you.</color>", buf, desc);
        }
    }
}

void equip_learn_resist(int obj_flag)
{
    _learn_resist_aux(obj_flag, "protecting");
}

void equip_learn_vuln(int obj_flag)
{
    _learn_resist_aux(obj_flag, "exposing");
}

void equip_learn_flag(int obj_flag)
{
    slot_t slot;
    for (slot = 1; slot <= _template->max; slot++)
    {
        obj_ptr obj = inv_obj(_inv, slot);
        if (obj && obj_learn_flag(obj, obj_flag))
        {
            char buf[MAX_NLEN];
            object_desc(buf, obj, OD_LORE);
            msg_format("<color:B>You learn more about your %s.</color>", buf);
        }
    }
}

void equip_learn_slay(int slay_flag, cptr msg)
{
    slot_t slot;
    for (slot = 1; slot <= _template->max; slot++)
    {
        obj_ptr obj = inv_obj(_inv, slot);
        if ( obj 
          && !obj_is_weapon(obj) /* Hack for Jellies ... */
          && !obj_is_bow(obj)
          && obj_learn_flag(obj, slay_flag) )
        {
            char buf[MAX_NLEN];
            object_desc(buf, obj, OD_LORE);
            msg_format("<color:B>You learn that your %s %s.</color>", buf, msg);
            /* We need to update plr->attack_info[].known_flags (cf equip_calc_bonuses()) */
            plr->update |= PU_BONUS;
        }
    }
}

void equip_load(savefile_ptr file)
{
    inv_load(_inv, file);
}

void equip_save(savefile_ptr file)
{
    inv_save(_inv, file);
}

static int_map_ptr _equip_templates = NULL;
equip_template_ptr equip_template_parse(cptr token)
{
    sym_t name = sym_find(token);
    assert(_equip_templates);
    if (!name) return NULL;
    return int_map_find(_equip_templates, name);
}
equip_template_ptr equip_template_lookup(sym_t name)
{
    assert(_equip_templates);
    assert(name);
    return int_map_find(_equip_templates, name);
}
equip_template_ptr equip_template_alloc(sym_t name)
{
    equip_template_ptr body = malloc(sizeof(equip_template_t));
    memset(body, 0, sizeof(equip_template_t));
    body->name = name;
    return body;
}
void equip_template_free(equip_template_ptr body)
{
    if (!body) return;
    free(body);
}
static cptr b_info_slots[] =
{
    "NONE",
    "GLOVES",
    "WEAPON_SHIELD",
    "RING",
    "BOW",
    "AMULET",
    "LIGHT",
    "BODY_ARMOR",
    "CLOAK",
    "BOOTS",
    "HELMET",
    "ANY",
    "WEAPON",
    "CAPTURE_BALL",
    "QUIVER",
    "DARK",
    NULL
};
static errr _parse_equip_template(char *line, int options)
{
    static equip_template_ptr current = NULL;

    /* N:<name> */
    if (line[0] == 'N')
    {
        sym_t name = sym_add(line + 2);
        if (int_map_find(_equip_templates, name)) return PARSE_ERROR_NON_SEQUENTIAL_RECORDS;
        current = equip_template_alloc(name);
        int_map_add(_equip_templates, name, current);
    }

    else if (!current) return 3; /* missing initial N: line */

    /* S:WEAPON_SHIELD:Left Hand:1
       S:BOW:Shooting */
    else if (line[0] == 'S')
    {
        char *zz[10];
        int   num = tokenize(line + 2, 10, zz, 0);
        int   j;
        int   slot = ++current->max;

        if (slot > EQUIP_MAX) return PARSE_ERROR_OUT_OF_BOUNDS;
        if (num < 2) return PARSE_ERROR_TOO_FEW_ARGUMENTS;

        /* Slot Type */
        for (j = 0; j < EQUIP_SLOT_MAX; j++)
        {
            if (!b_info_slots[j]) break;
            if (streq(zz[0], b_info_slots[j]))
            {
                current->slots[slot].type = j;
                break;
            }
        }
        if (!current->slots[slot].type) return (1);

        /* Label */
        current->slots[slot].tag = sym_add(zz[1]);

        /* Hand */
        if (num >= 3)
            current->slots[slot].hand = atoi(zz[2]);
    }
    else return 6; /* unknown line type */

    return 0;
}

bool equip_template_init(void)
{
    assert(!_equip_templates);
    _equip_templates = int_map_alloc((int_map_free_f)equip_template_free);
    return !parse_edit_file("b_info.txt", _parse_equip_template, 0); /* errr -> bool */
}

