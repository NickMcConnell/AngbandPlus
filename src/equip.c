#include "angband.h"
#include "equip.h"

static equip_template_ptr _template = NULL;

bool _object_is_amulet(object_type *o_ptr) 
{
    if (o_ptr->tval == TV_AMULET || o_ptr->tval == TV_WHISTLE) return TRUE;
    return FALSE;
}

bool _object_is_anything(object_type *o_ptr) 
{
    if (TV_WEARABLE_BEGIN <= o_ptr->tval && o_ptr->tval <= TV_WEARABLE_END) 
        return TRUE;
    return FALSE; 
}

bool _object_is_body_armor(object_type *o_ptr) 
{
    switch (o_ptr->tval)
    {
    case TV_SOFT_ARMOR: case TV_HARD_ARMOR: case TV_DRAG_ARMOR:
        return TRUE;
    }
    return FALSE;
}

bool _object_is_boots(object_type *o_ptr) 
{
    if (o_ptr->tval == TV_BOOTS) return TRUE;
    return FALSE;
}

bool _object_is_bow(object_type *o_ptr) 
{
    if (o_ptr->tval == TV_BOW) return TRUE;
    return FALSE;
}

bool _object_is_cloak(object_type *o_ptr) 
{
    if (o_ptr->tval == TV_CLOAK) return TRUE;
    return FALSE;
}

bool _object_is_gloves(object_type *o_ptr) 
{
    if (o_ptr->tval == TV_GLOVES) return TRUE;
    return FALSE;
}

bool _object_is_helmet(object_type *o_ptr) 
{
    switch (o_ptr->tval)
    {
    case TV_HELM: case TV_CROWN:
        return TRUE;
    }
    return FALSE;
}

bool _object_is_lite(object_type *o_ptr) 
{
    if (o_ptr->tval == TV_LITE) return TRUE;
    return FALSE;
}

bool _object_is_ring(object_type *o_ptr) 
{
    if (o_ptr->tval == TV_RING) return TRUE;
    return FALSE;
}

bool _object_is_weapon(object_type *o_ptr) 
{
    switch (o_ptr->tval)
    {
    case TV_DIGGING: case TV_HAFTED: case TV_POLEARM: case TV_SWORD:
        return TRUE;
    }
    return FALSE;
}

bool _object_is_weapon_or_shield(object_type *o_ptr) 
{
    switch (o_ptr->tval)
    {
    case TV_DIGGING: case TV_HAFTED: case TV_POLEARM: case TV_SWORD:
    case TV_SHIELD:  case TV_CARD: case TV_CAPTURE:
        return TRUE;
    }
    return FALSE;
}

bool _object_is_capture_ball(object_type *o_ptr) 
{
    switch (o_ptr->tval)
    {
    case TV_CAPTURE:
        return TRUE;
    }
    return FALSE;
}

static object_p _accept[EQUIP_SLOT_MAX] = {
    NULL,
    _object_is_gloves,
    _object_is_weapon_or_shield,
    _object_is_ring,
    _object_is_bow,
    _object_is_amulet,
    _object_is_lite,
    _object_is_body_armor,
    _object_is_cloak,
    _object_is_boots,
    _object_is_helmet,
    _object_is_anything,
    _object_is_weapon,
    _object_is_capture_ball
};

static int _slot_count(object_type *o_ptr)
{
    int result = 0;
    int i;
    for (i = 0; i < _template->count; i++)
    {
        object_p p = _accept[_template->slots[i].type];
        if (p(o_ptr))
            result++;
    }
    return result;
}

static bool _can_wield(object_type *o_ptr)
{
    if (_slot_count(o_ptr) > 0) return TRUE;
    return FALSE;
}

static int _get_slots(object_type *o_ptr, int slots[EQUIP_MAX_SLOTS])
{
    int ct = 0;
    int slot = equip_first_slot(o_ptr);

    while (equip_is_valid_slot(slot))
    {
        slots[ct++] = slot;
        slot = equip_next_slot(o_ptr, slot);
    }
    return ct;
}

static void _slot_menu_fn(int cmd, int which, vptr cookie, variant *res)
{
    int          slot = ((int*)cookie)[which];
    int          idx = slot - EQUIP_BEGIN;
    object_type *o_ptr = equip_obj(slot);

    switch (cmd)
    {
    case MENU_KEY:
        var_set_int(res, idx + 'a');
        break;
    case MENU_TEXT:
        if (o_ptr)
        {
            char buf[MAX_NLEN+50];
            char o_name[MAX_NLEN];
            object_desc(o_name, o_ptr, 0);
            sprintf(buf, "%-14s: %s", b_tag + _template->slots[idx].tag, o_name);
            var_set_string(res, buf);
        }
        else
        {
            char buf[MAX_NLEN+50];
            sprintf(buf, "%-14s:", b_tag + _template->slots[idx].tag);
            var_set_string(res, buf);
        }
        break;
    case MENU_COLOR:
        if (o_ptr)
        {
            if (o_ptr->timeout)
                var_set_int(res, TERM_L_DARK);
            else
                var_set_int(res, tval_to_attr[o_ptr->tval % 128]);
        }
        break;
    default:
        default_menu(cmd, which, cookie, res);
    }
}

static int _prompt_wield_slot(object_type *o_ptr)
{
    int slots[EQUIP_MAX_SLOTS];
    int ct = _get_slots(o_ptr, slots);

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
extern cptr equip_describe_slot(int slot)
{
    int i = slot - EQUIP_BEGIN;

    if (_template->slots[i].type == EQUIP_SLOT_WEAPON_SHIELD || _template->slots[i].type == EQUIP_SLOT_WEAPON)
    {
        int hand = _template->slots[i].hand;
        if (p_ptr->weapon_info[hand].heavy_wield)
            return "Just Lifting";
        if (p_ptr->weapon_info[hand].wield_how == WIELD_TWO_HANDS && !prace_is_(RACE_MON_SWORD))
        {
            if (p_ptr->current_r_idx == MON_BLOODTHIRSTER)
                return "Both Paws";
            else
                return "Both Arms";
        }
        if (p_ptr->weapon_info[hand].riding)
            return "Riding Reins";
    }
    if (_template->slots[i].type == EQUIP_SLOT_BOW)
    {
        if (p_ptr->shooter_info.heavy_shoot)
            return "Just Holding";
    }
    return b_tag + _template->slots[i].tag;
}

int equip_find_artifact(int which)
{
    int i;
    for (i = EQUIP_BEGIN; i < EQUIP_BEGIN + _template->count; i++)
    {
        object_type *o_ptr = equip_obj(i);

        if (o_ptr && o_ptr->name1 == which)
            return i;
    }
    return 0;
}

int equip_find_ego(int which)
{
    int i;
    for (i = EQUIP_BEGIN; i < EQUIP_BEGIN + _template->count; i++)
    {
        object_type *o_ptr = equip_obj(i);

        if (o_ptr && o_ptr->name2 == which)
            return i;
    }
    return 0;
}

int equip_find_object(int tval, int sval)
{
    int i;
    for (i = EQUIP_BEGIN; i < EQUIP_BEGIN + _template->count; i++)
    {
        object_type *o_ptr = equip_obj(i);

        if ( o_ptr 
          && o_ptr->tval == tval 
          && (sval == SV_ANY || o_ptr->sval == sval) )
        {
            return i;
        }
    }
    return 0;
}

int equip_find_first(object_p p)
{
    return equip_find_next(p, EQUIP_BEGIN - 1);
}

int equip_find_next(object_p p, int last)
{
    int i;
    for (i = last + 1; i < EQUIP_BEGIN + _template->count; i++)
    {
        object_type *o_ptr = equip_obj(i);

        if (o_ptr && (!p || p(o_ptr)))
            return i;
    }
    return 0;
}

int equip_first_empty_slot(object_type *o_ptr)
{
    int i;
    for (i = 0; i < _template->count; i++)
    {
        int      slot = EQUIP_BEGIN + i;
        object_p p = _accept[_template->slots[i].type];
        
        if (p(o_ptr) && !equip_obj(slot))
            return slot;
    }
    return 0;
}

int equip_find_empty_hand(void)
{
    int i;
    for (i = 0; i < _template->count; i++)
    {
        int slot = EQUIP_BEGIN + i;
        if ( (_template->slots[i].type == EQUIP_SLOT_WEAPON_SHIELD || _template->slots[i].type == EQUIP_SLOT_WEAPON)
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

int equip_first_slot(object_type *o_ptr)
{
    return equip_next_slot(o_ptr, EQUIP_BEGIN - 1);
}

int equip_next_slot(object_type *o_ptr, int last)
{
    int i;
    for (i = last + 1; i < EQUIP_BEGIN + _template->count; i++)
    {
        object_p p = _accept[_template->slots[i - EQUIP_BEGIN].type];
        if (p(o_ptr))
            return i;
    }
    return 0;
}

bool equip_is_valid_slot(int slot)
{
    if (slot >= EQUIP_BEGIN && slot < EQUIP_BEGIN + _template->count)
        return TRUE;
    return FALSE;
}

bool equip_verify_slot(int slot, object_type *o_ptr)
{
    if (equip_is_valid_slot(slot))
    {
        object_p p = _accept[_template->slots[slot - EQUIP_BEGIN].type];
        if (p(o_ptr))
            return TRUE;
    }
    return FALSE;
}

void equip_for_each_obj(object_fn f)
{
    int i;
    for (i = 0; i < _template->count; i++)
    {
        int          slot = EQUIP_BEGIN + i;
        object_type *o_ptr = equip_obj(slot);
        
        if (o_ptr)
            f(o_ptr);
    }
}

int equip_weight(object_p p)
{
    int i;
    int w = 0;
    for (i = 0; i < _template->count; i++)
    {
        int          slot = EQUIP_BEGIN + i;
        object_type *o_ptr = equip_obj(slot);
        
        if (o_ptr && (!p || p(o_ptr)))
            w += o_ptr->weight;
    }
    return w;
}

int equip_count_used(void)
{
    int i;
    int ct = 0;
    for (i = 0; i < _template->count; i++)
    {
        int          slot = EQUIP_BEGIN + i;
        object_type *o_ptr = equip_obj(slot);
        
        if (o_ptr)
            ct++;
    }
    return ct;
}

int equip_is_worn(object_type *o_ptr)
{
    int i;
    for (i = 0; i < _template->count; i++)
    {
        int          slot = EQUIP_BEGIN + i;
        object_type *o = equip_obj(slot);
        
        if (o == o_ptr) 
            return slot;
    }
    return 0;
}

int equip_which_hand(object_type *o_ptr)
{
    int i;
    for (i = 0; i < _template->count; i++)
    {
        int          slot = EQUIP_BEGIN + i;
        object_type *o = equip_obj(slot);
        
        if (o == o_ptr) 
            return _template->slots[i].hand;
    }
    return -1;
}

void equip_for_each_slot(slot_fn f)
{
    int i;
    for (i = 0; i < _template->count; i++)
    {
        int slot = EQUIP_BEGIN + i;
        f(slot);
    }
}

int equip_random_slot(object_p p)
{
    int ct = 0;
    int i;

    for (i = 0; i < _template->count; i++)
    {
        int          slot = EQUIP_BEGIN + i;
        object_type *o = equip_obj(slot);
        
        if (o && (!p || p(o)))
            ct++;
    }

    if (ct)
    {
        int which = randint0(ct);
        for (i = 0; i < _template->count; i++)
        {
            int          slot = EQUIP_BEGIN + i;
            object_type *o = equip_obj(slot);
        
            if (o && (!p || p(o)))
            {
                if (!which) return slot;
                which--;
            }
        }
    }
    return 0;
}

object_type *equip_obj(int slot)
{
    object_type *result = NULL;
    if (equip_is_valid_slot(slot) && inventory[slot].k_idx)
        result = &inventory[slot];
    return result;
}

int equip_count(void)
{
    return _template->count;
}

int equip_slot_type(int slot)
{
    if (equip_is_valid_slot(slot))
    {
        int idx = slot - EQUIP_BEGIN;
        return _template->slots[idx].type;
    }
    return EQUIP_SLOT_NONE;
}

void equip_wield(void)
{
    int item, slot, i;
    object_type *o_ptr;
    object_type copy;
    char o_name[MAX_NLEN];

    if (p_ptr->special_defense & KATA_MUSOU)
        set_action(ACTION_NONE);

    /* Prompt for Object */
    item_tester_hook = _can_wield;
    if (!get_item(&item, "Wear/Wield which item? ", "You have nothing you can wear or wield.", USE_INVEN | USE_FLOOR)) return;
    if (item >= 0)
        o_ptr = &inventory[item];
    else
        o_ptr = &o_list[0 - item];

    if (!psion_can_wield(o_ptr)) return;

    /* Prompt for Slot */
    slot = _prompt_wield_slot(o_ptr);
    if (!equip_is_valid_slot(slot)) return;

    if (have_flag(inventory[slot].art_flags, TR_NO_REMOVE))
    {
        msg_print("You can't replace yourself with that!");
        return;
    }

    /* Double Check */
    if (object_is_cursed(&inventory[slot]))
    {
        object_desc(o_name, &inventory[slot], (OD_OMIT_PREFIX | OD_NAME_ONLY));
        msg_format("The %s you are wearing appears to be cursed.", o_name);
        return;
    }
	if (confirm_wear)
	{
		bool do_prompt = FALSE;

		if (object_is_known(o_ptr) && object_is_cursed(o_ptr))
		{
			do_prompt = TRUE;
		}
		else if (o_ptr->ident & IDENT_SENSE)
		{
			switch (o_ptr->feeling)
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
			object_desc(o_name, o_ptr, (OD_OMIT_PREFIX | OD_NAME_ONLY));
			sprintf(dummy, "Really use the %s {cursed}? ", o_name);
			if (!get_check(dummy)) return;
		}
	}
    if ( o_ptr->name1 == ART_STONEMASK 
      && object_is_known(o_ptr) 
      && p_ptr->prace != RACE_VAMPIRE 
      && p_ptr->prace != RACE_ANDROID 
      && !(get_race_t()->flags & RACE_IS_MONSTER)
      && p_ptr->pclass != CLASS_BLOOD_KNIGHT)
    {
        char dummy[MAX_NLEN+80];
        object_desc(o_name, o_ptr, (OD_OMIT_PREFIX | OD_NAME_ONLY));
        msg_format("%s will transforms you into a vampire permanently when equiped.", o_name);
        sprintf(dummy, "Do you become a vampire?");
        if (!get_check(dummy)) return;
    }

    /* Quest Completed? */
    for (i = 0; i < max_quests; i++)
    {
        if ((quest[i].type == QUEST_TYPE_FIND_ARTIFACT) &&
            (quest[i].status == QUEST_STATUS_TAKEN) &&
            (quest[i].k_idx == o_ptr->name1 || quest[i].k_idx == o_ptr->name3))
        {
            quest[i].status = QUEST_STATUS_COMPLETED;
            quest[i].complev = (byte)p_ptr->lev;
            msg_print("You completed the quest!");
            msg_print(NULL);
        }
    }

    if (p_ptr->personality == PERS_MUNCHKIN)
    {
        identify_item(o_ptr);
        autopick_alter_item(item, FALSE);
    }

    /* Wear It */
    object_copy(&copy, o_ptr);
    copy.number = 1;

    energy_use = weaponmaster_wield_hack(o_ptr);

    if (item >= 0)
    {
        inven_item_increase(item, -1);
        inven_item_optimize(item);
    }
    else
    {
        floor_item_increase(0 - item, -1);
        floor_item_optimize(0 - item);
    }

    equip_wield_aux(&copy, slot);
}

void equip_wield_aux(object_type *src, int slot)
{
    object_type *dest = &inventory[slot];
    char         o_name[MAX_NLEN];

    if (dest->k_idx)
        equip_takeoff_aux(slot);
    
    object_copy(dest, src);
    stats_on_equip(dest);
    dest->marked |= OM_TOUCHED;
    dest->marked &= ~OM_WORN;
    p_ptr->total_weight += dest->weight;

    /* Hack: Extra Might and Weaponmastery require a calc_bonus() to display correctly */
    p_ptr->update |= PU_BONUS;
    handle_stuff();

    object_desc(o_name, dest, 0);
    if (p_ptr->prace == RACE_MON_SWORD || p_ptr->prace == RACE_MON_RING)
        msg_format("You are %s.", o_name);
    else
        msg_format("You are wearing %s (%c).", o_name, index_to_label(slot));

    /* After Effects? */
    if (object_is_cursed(dest))
    {
        msg_print("Oops! It feels deathly cold!");
        virtue_add(VIRTUE_HARMONY, -1);
        dest->ident |= IDENT_SENSE;
    }
    if (dest->name1 == ART_HAND_OF_VECNA)
    {
        msg_print("You chop off your own hand to wield the Hand of Vecna!");
        set_cut(CUT_MORTAL_WOUND, FALSE);
    }
    if (dest->name1 == ART_EYE_OF_VECNA)
    {
        msg_print("You pluck out your own eye to wield the Eye of Vecna!");
        set_cut(CUT_MORTAL_WOUND, FALSE);
    }
    if ( dest->name1 == ART_STONEMASK 
      && object_is_known(dest) 
      && p_ptr->prace != RACE_VAMPIRE 
      && p_ptr->prace != RACE_ANDROID 
      && p_ptr->pclass != CLASS_BLOOD_KNIGHT )
    {
        change_race(RACE_VAMPIRE, "");
    }

    p_ptr->update |= PU_BONUS;
    p_ptr->update |= PU_TORCH;
    p_ptr->update |= PU_MANA;
    p_ptr->redraw |= PR_EQUIPPY;
    p_ptr->window |= PW_INVEN | PW_EQUIP | PW_PLAYER;
    calc_android_exp();
}

void equip_takeoff(void)
{
    int slot;
    object_type *o_ptr;

    if (p_ptr->special_defense & KATA_MUSOU)
        set_action(ACTION_NONE);

    if (!get_item(&slot, "Take off which item? ", "You are not wearing anything to take off.", USE_EQUIP)) return;
    o_ptr = &inventory[slot];
    
    if (!psion_can_wield(o_ptr)) return;
    if (have_flag(o_ptr->art_flags, TR_NO_REMOVE)) /* Hack!!!! */
    {
        msg_print("You try to take yourself off, but fail!");
        return;
    }

    if (object_is_cursed(o_ptr))
    {
        if (o_ptr->curse_flags & TRC_PERMA_CURSE || (p_ptr->pclass != CLASS_BERSERKER && !prace_is_(RACE_MON_JELLY)))
        {
            msg_print("Hmmm, it seems to be cursed.");
            return;
        }
        if (((o_ptr->curse_flags & TRC_HEAVY_CURSE) && one_in_(7)) || one_in_(4))
        {
            msg_print("You teared a cursed equipment off by sheer strength!");
            o_ptr->ident |= (IDENT_SENSE);
            o_ptr->curse_flags = 0L;
            o_ptr->feeling = FEEL_NONE;
            p_ptr->update |= (PU_BONUS);
            p_ptr->window |= (PW_EQUIP);

            msg_print("You break the curse.");
        }
        else
        {
            msg_print("You couldn't remove the equipment.");
            energy_use = 50;
            return;
        }
    }
    energy_use = 50;
    equip_takeoff_aux(slot);
    if (weaponmaster_is_(WEAPONMASTER_SHIELDS))
        handle_stuff();
    
    calc_android_exp();
}

void equip_takeoff_aux(int slot)
{
    object_type *o_ptr = equip_obj(slot);

    if (o_ptr)
    {
        object_type copy;    
        int  new_slot;
        char o_name[MAX_NLEN];

        object_copy(&copy, o_ptr);
        object_desc(o_name, &copy, 0);
        inven_item_increase(slot, -1);
        inven_item_optimize(slot);

        new_slot = inven_carry(&copy);
        msg_format("You were using %s (%c).", o_name, index_to_label(new_slot));

        p_ptr->update |= PU_BONUS;
        p_ptr->update |= PU_TORCH;
        p_ptr->update |= PU_MANA;
        p_ptr->redraw |= PR_EQUIPPY;
        p_ptr->window |= PW_INVEN | PW_EQUIP | PW_PLAYER;
    }
}

bool equip_is_empty_two_handed_slot(int slot)
{
    int idx = slot - EQUIP_BEGIN;
    if (equip_obj(slot)) return FALSE;

    if (_template->slots[idx].type == EQUIP_SLOT_WEAPON_SHIELD)
    {
        int hand = _template->slots[idx].hand;
        int arm = hand / 2;
        int rhand = arm*2;
        int lhand = arm*2 + 1;
        int other_hand = (hand == rhand) ? lhand : rhand;

        if (p_ptr->weapon_info[other_hand].wield_how == WIELD_TWO_HANDS)
            return TRUE;
    }
    return FALSE;
}

/* Rings and Gloves are somewhat complicated. 
   We support an arbitrary number of hands (cf MAX_HANDS) paired
   into an arbitrary number of sets of arms (cf MAX_ARMS). Gloves
   affect both weapons for that arm, possibly with proration. Rings
   affect the hand in question, unless the other hand on that set
   of arms is wielding a weapon two handed.
 */
static void _weapon_info_flag(int idx, u32b flgs[TR_FLAG_SIZE], int flg)
{
    if (have_flag(flgs, flg))
    {
        int hand = _template->slots[idx].hand;
        int arm = hand / 2;
        int rhand = arm*2;
        int lhand = arm*2 + 1;
        int other_hand = (hand == rhand) ? lhand : rhand;
        
        switch (_template->slots[idx].type)
        {
        case EQUIP_SLOT_GLOVES:
            if (p_ptr->weapon_info[rhand].wield_how != WIELD_NONE)
                add_flag(p_ptr->weapon_info[rhand].flags, flg);
            if (p_ptr->weapon_info[lhand].wield_how != WIELD_NONE)
                add_flag(p_ptr->weapon_info[lhand].flags, flg);
            break;
        case EQUIP_SLOT_RING:
            if (p_ptr->weapon_info[hand].wield_how != WIELD_NONE)
                add_flag(p_ptr->weapon_info[hand].flags, flg);
            else if (p_ptr->weapon_info[other_hand].wield_how == WIELD_TWO_HANDS)
                add_flag(p_ptr->weapon_info[other_hand].flags, flg);
            break;
        case EQUIP_SLOT_ANY:
            add_flag(p_ptr->weapon_info[hand].flags, flg);
            break;
        }
    }
}

static void _weaponmastery(int idx, int amt)
{
    int hand = _template->slots[idx].hand;
    int arm = hand / 2;
    int rhand = arm*2;
    int lhand = arm*2 + 1;
    int other_hand = (hand == rhand) ? lhand : rhand;

    switch (_template->slots[idx].type)
    {
    case EQUIP_SLOT_RING:
        if (p_ptr->weapon_info[hand].wield_how != WIELD_NONE)
            p_ptr->weapon_info[hand].to_dd += amt;
        else if (p_ptr->weapon_info[other_hand].wield_how == WIELD_TWO_HANDS)
            p_ptr->weapon_info[other_hand].to_dd += amt;
        else
            p_ptr->innate_attack_info.to_dd += amt;
        break;
    case EQUIP_SLOT_ANY:
        if (p_ptr->weapon_info[hand].wield_how != WIELD_NONE)
            p_ptr->weapon_info[hand].to_dd += amt;
        else
            p_ptr->innate_attack_info.to_dd += amt;
        break;
    default: /* At the moment, this is just the Robe of the Kamikaze Warrior (+2) */
        if (p_ptr->weapon_ct)
        {
            for (hand = 0; hand < MAX_HANDS; hand++)
            {
                if (p_ptr->weapon_info[hand].wield_how != WIELD_NONE)
                    p_ptr->weapon_info[hand].to_dd += amt / p_ptr->weapon_ct;
            }
        }
        else if (p_ptr->weapon_info[hand].wield_how != WIELD_NONE) /* TODO: I'm not sure martial arts should boost the weapon_ct ... */
            p_ptr->weapon_info[hand].to_dd += amt;
        else
            p_ptr->innate_attack_info.to_dd += amt;
    }
}

static void _weapon_bonus_hand(int hand, int to_h, int to_d, bool known)
{
    p_ptr->weapon_info[hand].to_h += to_h;
    p_ptr->weapon_info[hand].to_d += to_d;
    if (known)
    {
        p_ptr->weapon_info[hand].dis_to_h += to_h;
        p_ptr->weapon_info[hand].dis_to_d += to_d;
    }
}

static int _sign(int n)
{
    if (n > 0) return 1;
    if (n < 0) return -1;
    return 0;
}

static void _weapon_bonus(int idx, int to_h, int to_d)
{
    int hand = _template->slots[idx].hand;
    int arm = hand / 2;
    int rhand = arm*2;
    int lhand = arm*2 + 1;
    int other_hand = (hand == rhand) ? lhand : rhand;
    object_type *o_ptr = equip_obj(EQUIP_BEGIN + idx);

    if (!p_ptr->weapon_ct) return;

    switch (_template->slots[idx].type)
    {
    case EQUIP_SLOT_GLOVES:
        if (p_ptr->weapon_info[rhand].wield_how != WIELD_NONE && p_ptr->weapon_info[lhand].wield_how != WIELD_NONE)
        {
            _weapon_bonus_hand(rhand, (to_h + 1) / 2, (to_d + 1) / 2, object_is_known(o_ptr));
            _weapon_bonus_hand(lhand, to_h/2, to_d/2, object_is_known(o_ptr));
        }
        else if (p_ptr->weapon_info[rhand].wield_how != WIELD_NONE)
            _weapon_bonus_hand(rhand, to_h, to_d, object_is_known(o_ptr));
        else if (p_ptr->weapon_info[lhand].wield_how != WIELD_NONE)
            _weapon_bonus_hand(lhand, to_h, to_d, object_is_known(o_ptr));
        break;
    case EQUIP_SLOT_RING:
        if (p_ptr->weapon_info[hand].wield_how != WIELD_NONE)
            _weapon_bonus_hand(hand, to_h, to_d, object_is_known(o_ptr));
        else if (p_ptr->weapon_info[other_hand].wield_how == WIELD_TWO_HANDS)
            _weapon_bonus_hand(other_hand, to_h, to_d, object_is_known(o_ptr));
        break;
    default:
    {
        int x_to_h = to_h - (to_h/p_ptr->weapon_ct)*p_ptr->weapon_ct;
        int x_to_d = to_d - (to_d/p_ptr->weapon_ct)*p_ptr->weapon_ct;
        for (hand = 0; hand < MAX_HANDS; hand++)
        {
            if (p_ptr->weapon_info[hand].wield_how != WIELD_NONE)
                _weapon_bonus_hand(hand, to_h/p_ptr->weapon_ct, to_d/p_ptr->weapon_ct, object_is_known(o_ptr));
        }
        for (hand = 0; hand < MAX_HANDS && (x_to_h || x_to_d); hand++)
        {
            if (p_ptr->weapon_info[hand].wield_how != WIELD_NONE)
            {
                _weapon_bonus_hand(hand, _sign(x_to_h), _sign(x_to_d), object_is_known(o_ptr));
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
    return p_ptr->weapon_info[hand].slot;
}

bool equip_is_empty_hand(int hand)
{
    return equip_is_valid_hand(hand)
        && !equip_obj(p_ptr->weapon_info[hand].slot);
}

void equip_calc_bonuses(void)
{
    int i;

    /* Find the weapons */
    for (i = 0; i < _template->count; i++)
    {
        if ( _template->slots[i].type == EQUIP_SLOT_WEAPON_SHIELD
          || _template->slots[i].type == EQUIP_SLOT_WEAPON )
        {
            object_type *o_ptr;
            int          hand = _template->slots[i].hand;
            int          slot = EQUIP_BEGIN + i;

            p_ptr->weapon_info[hand].slot = slot;
            p_ptr->weapon_info[hand].wield_how = WIELD_NONE;
            o_ptr = equip_obj(slot);

            if (o_ptr)
            {
                if (weaponmaster_get_toggle() == TOGGLE_SHIELD_BASH)
                {
                    if (object_is_shield(o_ptr))
                        p_ptr->weapon_info[hand].wield_how = WIELD_ONE_HAND;
                }
                else if (object_is_melee_weapon(o_ptr)) 
                {
                    p_ptr->weapon_info[hand].wield_how = WIELD_ONE_HAND;
                    if (o_ptr->rune == RUNE_AIR)
                        p_ptr->weapon_info[hand].xtra_blow += 100;
                }
            }
            else if (p_ptr->pclass == CLASS_MONK || p_ptr->pclass == CLASS_FORCETRAINER || p_ptr->pclass == CLASS_MYSTIC)
            {
                p_ptr->weapon_info[hand].wield_how = WIELD_ONE_HAND;
                p_ptr->weapon_info[hand].bare_hands = TRUE;
            }
        }
    }

    /* Patch up for monks using weapons (with empty hands too) */
    for (i = 0; i < MAX_ARMS; i++)
    {
        int rhand = i*2;
        int lhand = i*2+1;

        if (!equip_is_valid_hand(rhand) || !equip_is_valid_hand(lhand)) continue;

        if ( p_ptr->weapon_info[rhand].wield_how != WIELD_NONE
          && p_ptr->weapon_info[rhand].bare_hands
          && p_ptr->weapon_info[lhand].wield_how != WIELD_NONE
          && equip_obj(p_ptr->weapon_info[lhand].slot) )
        {
            p_ptr->weapon_info[rhand].wield_how = WIELD_NONE;
            p_ptr->weapon_info[rhand].bare_hands = FALSE;
        }
        if ( p_ptr->weapon_info[lhand].wield_how != WIELD_NONE
          && p_ptr->weapon_info[lhand].bare_hands
          && p_ptr->weapon_info[rhand].wield_how != WIELD_NONE
          && equip_obj(p_ptr->weapon_info[rhand].slot) )
        {
            p_ptr->weapon_info[lhand].wield_how = WIELD_NONE;
            p_ptr->weapon_info[lhand].bare_hands = FALSE;
        }
    }

    /* Control mount with a free hand if possible using the last available hand */
    if (p_ptr->riding)
    {
        p_ptr->riding_ryoute = TRUE;
        if (p_ptr->prace == RACE_MON_RING)
        {
            p_ptr->riding_ryoute = FALSE;
        }
        else if (!(p_ptr->pet_extra_flags & PF_RYOUTE))
        {
            for (i = MAX_HANDS - 1; i >= 0; i--)
            {
                if (equip_is_empty_hand(i))
                {
                    p_ptr->weapon_info[i].riding = TRUE;
                    p_ptr->weapon_info[i].wield_how = WIELD_NONE;
                    p_ptr->weapon_info[i].bare_hands = FALSE;
                    p_ptr->riding_ryoute = FALSE;
                    break;
                }
            }
        }
    }

    /* Figure out which weapons are being used with 2 hands */
    if (CAN_TWO_HANDS_WIELDING())
    {
        for (i = 0; i < MAX_ARMS; i++)
        {
            int rhand = 2*i;
            int lhand = 2*i+1;

            if ( p_ptr->weapon_info[rhand].wield_how == WIELD_ONE_HAND
              && (p_ptr->weapon_info[rhand].bare_hands || object_allow_two_hands_wielding(equip_obj(p_ptr->weapon_info[rhand].slot)))
              && equip_is_empty_hand(lhand) )
            {
                p_ptr->weapon_info[rhand].wield_how = WIELD_TWO_HANDS;
                p_ptr->weapon_info[lhand].wield_how = WIELD_NONE;
            }

            if ( p_ptr->weapon_info[lhand].wield_how == WIELD_ONE_HAND
              && (p_ptr->weapon_info[lhand].bare_hands || object_allow_two_hands_wielding(equip_obj(p_ptr->weapon_info[lhand].slot)))
              && equip_is_empty_hand(rhand) )
            {
                p_ptr->weapon_info[lhand].wield_how = WIELD_TWO_HANDS;
                p_ptr->weapon_info[rhand].wield_how = WIELD_NONE;
            }
        }
    }

    /* Hack for Death Swords ... but not Broken Death Swords ;) */
    if (prace_is_(RACE_MON_SWORD) && p_ptr->lev >= 10)
        p_ptr->weapon_info[0].wield_how = WIELD_TWO_HANDS;

    /* Its convenient to have an accurate weapon count later */
    p_ptr->weapon_ct = 0;
    for (i = 0; i < MAX_HANDS; i++)
    {
        if (p_ptr->weapon_info[i].wield_how != WIELD_NONE)
            p_ptr->weapon_ct++;
    }

    /* Scan equipment for bonuses. */
    for (i = 0; i < _template->count; i++)
    {
        int          slot = EQUIP_BEGIN + i;
        object_type *o_ptr = &inventory[slot];
        u32b         flgs[TR_FLAG_SIZE];
        int          bonus_to_h, bonus_to_d;

        if (!o_ptr->k_idx) continue;

        object_flags(o_ptr, flgs);

        p_ptr->cursed |= (o_ptr->curse_flags & (0xFFFFFFF0L));
        if (o_ptr->name1 == ART_CHAINSWORD) p_ptr->cursed |= TRC_CHAINSWORD;

        if (o_ptr->name1 == ART_MAUL_OF_VICE)
            p_ptr->maul_of_vice = TRUE;

        if (o_ptr->name1 == ART_STONE_LORE)
            p_ptr->loremaster = TRUE;

        if (o_ptr->name2 == EGO_BOOTS_FAIRY || o_ptr->name2 == EGO_CLOAK_FAIRY)
            p_ptr->fairy_stealth = TRUE;

        if (o_ptr->name2 == EGO_GLOVES_GIANT)
        {
            int hand = _template->slots[i].hand;
            int arm = hand / 2;
            int rhand = arm*2;
            int lhand = arm*2 + 1;
            if (p_ptr->weapon_info[rhand].wield_how == WIELD_TWO_HANDS)
                p_ptr->weapon_info[rhand].giant_wield = TRUE;
            else if (p_ptr->weapon_info[lhand].wield_how == WIELD_TWO_HANDS)
                p_ptr->weapon_info[lhand].giant_wield = TRUE;
        }

        rune_calc_bonuses(o_ptr);

        if (have_flag(flgs, TR_STR)) p_ptr->stat_add[A_STR] += o_ptr->pval;
        if (have_flag(flgs, TR_INT)) p_ptr->stat_add[A_INT] += o_ptr->pval;
        if (have_flag(flgs, TR_WIS)) p_ptr->stat_add[A_WIS] += o_ptr->pval;
        if (have_flag(flgs, TR_DEX)) p_ptr->stat_add[A_DEX] += o_ptr->pval;
        if (have_flag(flgs, TR_CON)) p_ptr->stat_add[A_CON] += o_ptr->pval;
        if (have_flag(flgs, TR_CHR)) p_ptr->stat_add[A_CHR] += o_ptr->pval;

        if (have_flag(flgs, TR_DEC_STR)) p_ptr->stat_add[A_STR] -= o_ptr->pval;
        if (have_flag(flgs, TR_DEC_INT)) p_ptr->stat_add[A_INT] -= o_ptr->pval;
        if (have_flag(flgs, TR_DEC_WIS)) p_ptr->stat_add[A_WIS] -= o_ptr->pval;
        if (have_flag(flgs, TR_DEC_DEX)) p_ptr->stat_add[A_DEX] -= o_ptr->pval;
        if (have_flag(flgs, TR_DEC_CON)) p_ptr->stat_add[A_CON] -= o_ptr->pval;
        if (have_flag(flgs, TR_DEC_CHR)) p_ptr->stat_add[A_CHR] -= o_ptr->pval;

        if (have_flag(flgs, TR_MAGIC_MASTERY))
            p_ptr->skills.dev += 8*o_ptr->pval;

        if (have_flag(flgs, TR_DEVICE_POWER))
            p_ptr->device_power += o_ptr->pval;

        if (have_flag(flgs, TR_DEC_MAGIC_MASTERY))
        {
            p_ptr->skills.dev -= 8*o_ptr->pval;
            p_ptr->device_power -= o_ptr->pval;
        }

        if (have_flag(flgs, TR_STEALTH)) p_ptr->skills.stl += o_ptr->pval;
        if (have_flag(flgs, TR_DEC_STEALTH)) p_ptr->skills.stl -= o_ptr->pval;
        if (have_flag(flgs, TR_SEARCH)) p_ptr->skills.srh += (o_ptr->pval * 5);
        if (have_flag(flgs, TR_SEARCH)) p_ptr->skills.fos += (o_ptr->pval * 5);
        if (have_flag(flgs, TR_INFRA)) p_ptr->see_infra += o_ptr->pval;
        if (have_flag(flgs, TR_TUNNEL)) p_ptr->skill_dig += (o_ptr->pval * 20);
        if (have_flag(flgs, TR_SPEED)) p_ptr->pspeed += o_ptr->pval;
        if (have_flag(flgs, TR_DEC_SPEED)) p_ptr->pspeed -= o_ptr->pval;

        if (have_flag(flgs, TR_BLOWS) && (p_ptr->pclass != CLASS_MAULER || o_ptr->pval < 0))
        {
            int hand = _template->slots[i].hand;
            int amt = o_ptr->pval * 50;
            switch (_template->slots[i].type)
            {
            case EQUIP_SLOT_GLOVES:
            {
                int arm = hand / 2;
                int rhand = arm*2;
                int lhand = arm*2 + 1;
                if (p_ptr->weapon_info[rhand].wield_how != WIELD_NONE && p_ptr->weapon_info[lhand].wield_how != WIELD_NONE)
                {
                    if (amt > 0)
                    {
                        p_ptr->weapon_info[rhand].xtra_blow += amt/2;
                        p_ptr->weapon_info[lhand].xtra_blow += amt/2;
                    }
                    else
                    {
                        p_ptr->weapon_info[rhand].xtra_blow += amt;
                        p_ptr->weapon_info[lhand].xtra_blow += amt;
                    }
                }
                else if (p_ptr->weapon_info[rhand].wield_how != WIELD_NONE)
                    p_ptr->weapon_info[rhand].xtra_blow += amt;
                else if (p_ptr->weapon_info[lhand].wield_how != WIELD_NONE)
                    p_ptr->weapon_info[lhand].xtra_blow += amt;
                else
                    p_ptr->innate_attack_info.xtra_blow += amt;
                break;
            }
            case EQUIP_SLOT_RING:
                if (p_ptr->weapon_info[hand].wield_how != WIELD_NONE)
                    p_ptr->weapon_info[hand].xtra_blow += amt;
                else
                {
                    int other_hand;
                    if (hand % 2 == 0)
                        other_hand = hand + 1;
                    else
                        other_hand = hand - 1;
                    if (p_ptr->weapon_info[other_hand].wield_how == WIELD_TWO_HANDS)
                        p_ptr->weapon_info[other_hand].xtra_blow += amt;
                    else if (p_ptr->weapon_info[other_hand].wield_how == WIELD_NONE)
                        p_ptr->innate_attack_info.xtra_blow += amt;
                }
                break;
            case EQUIP_SLOT_WEAPON_SHIELD:
            case EQUIP_SLOT_WEAPON:
                if (p_ptr->weapon_info[hand].wield_how != WIELD_NONE)
                    p_ptr->weapon_info[hand].xtra_blow += amt;
                break;
            default:
            {
                if (object_is_melee_weapon(o_ptr)) break; /* Hack for Jellies ... */
                if (p_ptr->weapon_ct)
                {
                    int  j;
                    for (j = 0; j < MAX_HANDS; j++)
                    {
                        if (p_ptr->weapon_info[j].wield_how != WIELD_NONE)
                            p_ptr->weapon_info[j].xtra_blow += amt/p_ptr->weapon_ct;
                    }
                }
                else
                    p_ptr->innate_attack_info.xtra_blow += amt;
            }
            }
        }

        /* New: Rings and Gloves can grant weapon slays */
        if (!object_is_melee_weapon(o_ptr)) /* Hack for Jellies ... */
        {
            _weapon_info_flag(i, flgs, TR_BRAND_FIRE);
            _weapon_info_flag(i, flgs, TR_BRAND_COLD);
            _weapon_info_flag(i, flgs, TR_BRAND_ELEC);
            _weapon_info_flag(i, flgs, TR_BRAND_ACID);
            _weapon_info_flag(i, flgs, TR_BRAND_POIS);
            _weapon_info_flag(i, flgs, TR_IMPACT);     /* Quaker */
            _weapon_info_flag(i, flgs, TR_SLAY_GOOD);  /* Thanos, Nazgul */
            _weapon_info_flag(i, flgs, TR_SLAY_HUMAN); /* Nazgul */
        }

        if (have_flag(flgs, TR_XTRA_SHOTS))
            p_ptr->shooter_info.num_fire += 25 * o_ptr->pval;

        if (have_flag(flgs, TR_LIFE))
            p_ptr->life += 3*o_ptr->pval;
        if (have_flag(flgs, TR_DEC_LIFE))
            p_ptr->life -= 3*o_ptr->pval;

        if (have_flag(flgs, TR_AGGRAVATE))   p_ptr->cursed |= TRC_AGGRAVATE;
        if (have_flag(flgs, TR_DRAIN_EXP))   p_ptr->cursed |= TRC_DRAIN_EXP;
        if (have_flag(flgs, TR_TY_CURSE))    p_ptr->cursed |= TRC_TY_CURSE;

        if (have_flag(flgs, TR_DEC_MANA))    p_ptr->dec_mana = TRUE;
        if (have_flag(flgs, TR_SPELL_POWER)) p_ptr->spell_power += o_ptr->pval;
        if (have_flag(flgs, TR_DEC_SPELL_POWER)) p_ptr->spell_power -= o_ptr->pval;
        if (have_flag(flgs, TR_SPELL_CAP))   p_ptr->spell_cap += o_ptr->pval;
        if (have_flag(flgs, TR_DEC_SPELL_CAP))   p_ptr->spell_cap -= o_ptr->pval;
        if (have_flag(flgs, TR_MAGIC_RESISTANCE))   p_ptr->magic_resistance += 5*o_ptr->pval;
        if (have_flag(flgs, TR_BLESSED))     p_ptr->bless_blade = TRUE;

        if (have_flag(flgs, TR_XTRA_MIGHT) && o_ptr->tval != TV_BOW)  
            p_ptr->shooter_info.to_mult += 25 * o_ptr->pval;

        if (have_flag(flgs, TR_SLOW_DIGEST)) p_ptr->slow_digest = TRUE;
        if (have_flag(flgs, TR_REGEN))       p_ptr->regenerate = TRUE;
        if (have_flag(flgs, TR_TELEPATHY))   p_ptr->telepathy = TRUE;
        if (have_flag(flgs, TR_ESP_ANIMAL))  p_ptr->esp_animal = TRUE;
        if (have_flag(flgs, TR_ESP_UNDEAD))  p_ptr->esp_undead = TRUE;
        if (have_flag(flgs, TR_ESP_DEMON))   p_ptr->esp_demon = TRUE;
        if (have_flag(flgs, TR_ESP_ORC))     p_ptr->esp_orc = TRUE;
        if (have_flag(flgs, TR_ESP_TROLL))   p_ptr->esp_troll = TRUE;
        if (have_flag(flgs, TR_ESP_GIANT))   p_ptr->esp_giant = TRUE;
        if (have_flag(flgs, TR_ESP_DRAGON))  p_ptr->esp_dragon = TRUE;
        if (have_flag(flgs, TR_ESP_HUMAN))   p_ptr->esp_human = TRUE;
        if (have_flag(flgs, TR_ESP_EVIL))    p_ptr->esp_evil = TRUE;
        if (have_flag(flgs, TR_ESP_GOOD))    p_ptr->esp_good = TRUE;
        if (have_flag(flgs, TR_ESP_NONLIVING)) p_ptr->esp_nonliving = TRUE;
        if (have_flag(flgs, TR_ESP_UNIQUE))  p_ptr->esp_unique = TRUE;

        if (have_flag(flgs, TR_SEE_INVIS))   p_ptr->see_inv = TRUE;
        if (have_flag(flgs, TR_LEVITATION))  p_ptr->levitation = TRUE;
        if (have_flag(flgs, TR_FREE_ACT))    p_ptr->free_act = TRUE;
        if (have_flag(flgs, TR_HOLD_LIFE))   p_ptr->hold_life = TRUE;
        if (have_flag(flgs, TR_WARNING))
        {
            if (!o_ptr->inscription || !(my_strchr(quark_str(o_ptr->inscription),'$')))
              p_ptr->warning = TRUE;
        }

        if (have_flag(flgs, TR_TELEPORT))
        {
            if (object_is_cursed(o_ptr)) p_ptr->cursed |= TRC_TELEPORT;
            else
            {
                cptr insc = quark_str(o_ptr->inscription);
                if (o_ptr->inscription && my_strchr(insc, '.')) {}
                else p_ptr->cursed |= TRC_TELEPORT_SELF;
            }
        }

        res_calc_bonuses(flgs);

        if (have_flag(flgs, TR_REFLECT))  p_ptr->reflect = TRUE;
        if (have_flag(flgs, TR_SH_FIRE))  p_ptr->sh_fire = TRUE;
        if (have_flag(flgs, TR_SH_ELEC))  p_ptr->sh_elec = TRUE;
        if (have_flag(flgs, TR_SH_COLD))  p_ptr->sh_cold = TRUE;
        if (have_flag(flgs, TR_SH_SHARDS))  p_ptr->sh_shards = TRUE;
        if (have_flag(flgs, TR_SH_REVENGE))  p_ptr->sh_retaliation = TRUE;
        if (have_flag(flgs, TR_NO_MAGIC)) p_ptr->anti_magic = TRUE;
        if (have_flag(flgs, TR_NO_TELE))  p_ptr->anti_tele = TRUE;
        if (have_flag(flgs, TR_NO_SUMMON)) p_ptr->anti_summon = TRUE;

        if (have_flag(flgs, TR_SUST_STR)) p_ptr->sustain_str = TRUE;
        if (have_flag(flgs, TR_SUST_INT)) p_ptr->sustain_int = TRUE;
        if (have_flag(flgs, TR_SUST_WIS)) p_ptr->sustain_wis = TRUE;
        if (have_flag(flgs, TR_SUST_DEX)) p_ptr->sustain_dex = TRUE;
        if (have_flag(flgs, TR_SUST_CON)) p_ptr->sustain_con = TRUE;
        if (have_flag(flgs, TR_SUST_CHR)) p_ptr->sustain_chr = TRUE;

        if (o_ptr->name2 == EGO_GLOVES_GENJI || o_ptr->name1 == ART_MASTER_TONBERRY || o_ptr->name1 == ART_MEPHISTOPHELES)
        {
            switch (_template->slots[i].type)
            {
            case EQUIP_SLOT_GLOVES:
            {
                int hand = _template->slots[i].hand;
                int arm = hand / 2;
                int rhand = arm*2;
                int lhand = arm*2 + 1;

                p_ptr->weapon_info[rhand].genji = TRUE;
                p_ptr->weapon_info[lhand].genji = TRUE;
                break;
            }
            }
        }

        if (o_ptr->name1 == ART_SPECTRAL_DSM)
        {
            p_ptr->pass_wall = TRUE;
            p_ptr->no_passwall_dam = TRUE;
        }

        if (have_flag(flgs, TR_EASY_SPELL)) p_ptr->easy_spell = TRUE;

        if (o_ptr->curse_flags & TRC_LOW_MAGIC)
        {
            if (o_ptr->curse_flags & TRC_HEAVY_CURSE)
                p_ptr->to_m_chance += 10;
            else
                p_ptr->to_m_chance += 3;
        }

        if (o_ptr->tval == TV_CAPTURE) continue;

        /* Modify the base armor class */
        p_ptr->ac += o_ptr->ac;
        p_ptr->dis_ac += o_ptr->ac;

        /* Apply the bonuses to armor class */
        p_ptr->to_a += o_ptr->to_a;
        if (object_is_known(o_ptr)) p_ptr->dis_to_a += o_ptr->to_a;

        if (o_ptr->curse_flags & TRC_LOW_MELEE)
        {
            int penalty = (o_ptr->curse_flags & TRC_HEAVY_CURSE) ? -15 : -5;
            switch (_template->slots[i].type)
            {
            case EQUIP_SLOT_BOW:
                p_ptr->shooter_info.to_h += penalty;
                if (o_ptr->ident & IDENT_MENTAL)
                    p_ptr->shooter_info.dis_to_h += penalty;
                break;                
            case EQUIP_SLOT_WEAPON_SHIELD:
            case EQUIP_SLOT_WEAPON:
            {
                int hand = _template->slots[i].hand;
                p_ptr->weapon_info[hand].to_h += penalty;
                if (o_ptr->ident & IDENT_MENTAL)
                    p_ptr->weapon_info[hand].dis_to_h += penalty;
                break;
            }
            }
        }
        if (o_ptr->curse_flags & TRC_LOW_AC)
        {
            if (o_ptr->curse_flags & TRC_HEAVY_CURSE)
            {
                p_ptr->to_a -= 30;
                if (o_ptr->ident & IDENT_MENTAL) p_ptr->dis_to_a -= 30;
            }
            else
            {
                p_ptr->to_a -= 10;
                if (o_ptr->ident & IDENT_MENTAL) p_ptr->dis_to_a -= 10;
            }
        }

        /* Hack -- do not apply "weapon" bonuses */
        if (object_is_melee_weapon(o_ptr)) continue;

        /* Hack -- do not apply "bow" bonuses */
        if (_object_is_bow(o_ptr)) continue;

        /* Hack -- Sniper gloves apply to missiles only */
        if (o_ptr->name2 == EGO_GLOVES_SNIPER || o_ptr->name2 == EGO_RING_ARCHERY)
        {
            p_ptr->shooter_info.to_h += o_ptr->to_h;
            p_ptr->shooter_info.to_d += o_ptr->to_d;
            if (object_is_known(o_ptr))
            {
                p_ptr->shooter_info.dis_to_h += o_ptr->to_h;
                p_ptr->shooter_info.dis_to_d += o_ptr->to_d;
            }
            continue;
        }
        /* Hack -- Spell Damage on Wizard Egos */
        if ( o_ptr->name2 == EGO_RING_WIZARDRY
          || o_ptr->name2 == EGO_AMULET_MAGI
          || o_ptr->name2 == EGO_CROWN_MAGI )
        {
            p_ptr->to_d_spell += o_ptr->to_d;
            continue;
        }

        /* Hack -- Archery now benefits from equipment slays. Without this, only
           Sniper gloves and Archery rings affect shooting, and that seems a bit
           unfair (especially since melee is so favored)  */
        else if ( o_ptr->tval != TV_GLOVES
               && o_ptr->name2 != EGO_RING_COMBAT
               && o_ptr->name2 != EGO_CROWN_MIGHT
               && o_ptr->name2 != EGO_CLOAK_FAIRY  /* Hey, fairies can shoot just fine :) */
               && o_ptr->name1 != ART_TERROR
               && o_ptr->name1 != ART_HAMMERHAND )
        {
            p_ptr->shooter_info.to_h += o_ptr->to_h;
            p_ptr->shooter_info.to_d += o_ptr->to_d;
            if (object_is_known(o_ptr))
            {
                p_ptr->shooter_info.dis_to_h += o_ptr->to_h;
                p_ptr->shooter_info.dis_to_d += o_ptr->to_d;
            }
        }

        bonus_to_h = o_ptr->to_h;
        bonus_to_d = o_ptr->to_d;

        if (p_ptr->pclass == CLASS_NINJA)
        {
            if (o_ptr->to_h > 0) bonus_to_h = (o_ptr->to_h+1)/2;
            if (o_ptr->to_d > 0) bonus_to_d = (o_ptr->to_d+1)/2;
        }

        p_ptr->to_h_m += bonus_to_h;
        p_ptr->to_d_m += bonus_to_d;

        _weapon_bonus(i, bonus_to_h, bonus_to_d);
        if (have_flag(flgs, TR_WEAPONMASTERY))
            _weaponmastery(i, o_ptr->pval);
    }
}

void equip_on_init(void)
{
    race_t *race_ptr = get_race_t();
    if (race_ptr->equip_template)
        _template = race_ptr->equip_template;
    else
        _template = &b_info[0];
}

/* Attempt to gracefully handle changes to body type between
   releases. New slots may be added, old slots may be removed.
   Slots may be shuffled or have their types changed. 
   This is called by process_player() during startup if a 
   savefile has been loaded. At this point, drop_near is allowed. */
void equip_on_load(void)
{
    int i, ct = 0;
    object_type temp[EQUIP_MAX_SLOTS] = {{0}};

    for (i = 0; i < EQUIP_MAX_SLOTS; i++)
    {
        int slot = i + EQUIP_BEGIN;
        
        if (!inventory[slot].k_idx) continue;

        if (i >= _template->count)
        {
            object_copy(&temp[ct++], &inventory[slot]);
            object_wipe(&inventory[slot]);
        }
        else
        {
            object_p p = _accept[_template->slots[i].type];
            if (!p(&inventory[slot]))
            {
                object_copy(&temp[ct++], &inventory[slot]);
                object_wipe(&inventory[slot]);
            }
        }
    }

    for (i = 0; i < ct; i++)
    {
        int slot = equip_first_empty_slot(&temp[i]);
        
        if (slot)
            object_copy(&inventory[slot], &temp[i]);
        else 
        {
            char name[MAX_NLEN];
            object_desc(name, &temp[i], 0);
            msg_format("You can no longer wield %s.", name);
            p_ptr->total_weight -= temp[i].weight;
            if (inven_carry_okay(&temp[i]))
                inven_carry(&temp[i]);
            else
            {
                msg_print("Your pack overflows!");
                msg_format("You drop %s.", name);
                drop_near(&temp[i], -1, py, px);
            }
        }
    }
}

void equip_on_change_race(void)
{
    equip_template_ptr old_template = _template;
    equip_template_ptr new_template = get_race_t()->equip_template;

    if (!new_template)
        new_template = &b_info[0];

    if (old_template != new_template)
    {
        int i;
        object_type temp[EQUIP_MAX_SLOTS];
    
        _template = new_template;

        for (i = 0; i < old_template->count; i++)
        {
            object_copy(&temp[i], &inventory[EQUIP_BEGIN + i]);
            object_wipe(&inventory[EQUIP_BEGIN + i]);
        }

        for (i = 0; i < old_template->count; i++)
        {
            object_type *src = &temp[i];
            int          slot;

            if (!src->k_idx) continue;

            slot = equip_first_empty_slot(src);
            if (slot)
                object_copy(&inventory[slot], src);
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

                p_ptr->total_weight -= src->weight;
                if (inven_carry_okay(src))
                    inven_carry(src);
                else
                {
                    msg_print("Your pack overflows!");
                    msg_format("You drop %s.", name);
                    drop_near(src, -1, py, px);
                }
            }
        }

        for (i = INVEN_PACK - 1; i >= 0; i--)
        {
            object_type *o_ptr = &inventory[i];
            int          slot;

            if (!o_ptr->k_idx) continue;
            if (!(o_ptr->marked & OM_WORN)) continue;
        
            slot = equip_first_empty_slot(o_ptr);
            if (slot && o_ptr->number == 1)
            {
                object_type copy;

                object_copy(&copy, o_ptr);
                copy.number = 1;
                copy.marked &= ~OM_WORN;

                inven_item_increase(i, -1);
                inven_item_optimize(i);

                equip_wield_aux(&copy, slot);
            }
        }

        p_ptr->update |= PU_BONUS;
        p_ptr->update |= PU_TORCH;
        p_ptr->update |= PU_MANA;
        p_ptr->redraw |= PR_EQUIPPY;
        p_ptr->window |= PW_INVEN | PW_EQUIP | PW_PLAYER;
        calc_android_exp();
    }
}
