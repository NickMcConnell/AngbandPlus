/* File: cmd6.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies. Other copyrights may also apply.
 */

/* Purpose: Object commands */

#include <assert.h>

#include "angband.h"
#include "equip.h"

/*
 * This file includes code for eating food, drinking potions,
 * reading scrolls, aiming wands, using staffs, zapping rods,
 * and activating artifacts.
 *
 * In all cases, if the player becomes "aware" of the item's use
 * by testing it, mark it as "aware" and reward some experience
 * based on the object's level, always rounding up. If the player
 * remains "unaware", mark that object "kind" as "tried".
 *
 * Note that food/potions/scrolls no longer use bit-flags for effects,
 * but instead use the "sval" (which is also used to sort the objects).
 */

bool restore_mana(void)
{
    bool   result = FALSE;
    slot_t slot;

    if (p_ptr->pclass == CLASS_MAGIC_EATER)
    {
        magic_eater_restore();
        result = TRUE;
    }
    else if (p_ptr->csp < p_ptr->msp)
    {
        if (p_ptr->pclass == CLASS_RUNE_KNIGHT)
            p_ptr->csp += (p_ptr->msp - p_ptr->csp) / 3;
        else
            p_ptr->csp = p_ptr->msp;

        p_ptr->csp_frac = 0;
        p_ptr->redraw |= (PR_MANA);
        p_ptr->window |= (PW_SPELL);
        result = TRUE;
    }

    for (slot = 1; slot <= pack_max(); slot++)
    {
        obj_ptr obj = pack_obj(slot);

        if (!obj) continue;
        if (!object_is_device(obj)) continue;
        if (obj->activation.type == EFFECT_RESTORE_MANA) continue;

        if (obj->tval == TV_ROD)
            device_regen_sp_aux(obj, 500);
        else
            device_regen_sp_aux(obj, 250);
        result = TRUE;
    }

    msg_print("You feel your head clear.");
    return result;
}

static void do_cmd_eat_food_aux(obj_ptr obj)
{
    int  lev = k_info[obj->k_idx].level;
    bool ident = FALSE;

    if (music_singing_any()) bard_stop_singing();
    if (hex_spelling_any()) stop_hex_spell_all();
    warlock_stop_singing();

    if (object_is_mushroom(obj) && obj->art_name && obj->timeout)
    {
        msg_print("Your mushroom is still charging.");
        return;
    }

    sound(SOUND_EAT);
    energy_use = 100;
    ident = FALSE;

    /* Food may have effects */
    if (obj->tval == TV_FOOD)
    {
        switch (obj->sval)
        {
            case SV_FOOD_POISON:
            {
                if (!res_save_default(RES_POIS))
                {
                    if (set_poisoned(p_ptr->poisoned + randint0(10) + 10, FALSE))
                        ident = TRUE;
                }
                break;
            }

            case SV_FOOD_BLINDNESS:
            {
                if (!res_save_default(RES_BLIND))
                {
                    if (set_blind(p_ptr->blind + randint0(25) + 25, FALSE))
                        ident = TRUE;
                }
                break;
            }

            case SV_FOOD_PARANOIA:
            {
                if (!fear_save_p(fear_threat_level()))
                    ident = fear_add_p(FEAR_SCARED);
                break;
            }

            case SV_FOOD_CONFUSION:
            {
                if (!res_save_default(RES_CONF))
                {
                    if (set_confused(p_ptr->confused + randint0(10) + 10, FALSE))
                        ident = TRUE;
                }
                break;
            }

            case SV_FOOD_HALLUCINATION:
            {
                if (!res_save_default(RES_CHAOS))
                {
                    if (set_image(p_ptr->image + randint0(25) + 25, FALSE))
                        ident = TRUE;
                }
                break;
            }

            case SV_FOOD_PARALYSIS:
            {
                if (!free_act_save_p(0))
                    ident = set_paralyzed(randint1(4), FALSE);
                break;
            }

            case SV_FOOD_WEAKNESS:
            {
                take_hit(DAMAGE_NOESCAPE, damroll(6, 6), "poisonous food");
                do_dec_stat(A_STR);
                ident = TRUE;
                break;
            }

            case SV_FOOD_SICKNESS:
            {
                take_hit(DAMAGE_NOESCAPE, damroll(6, 6), "poisonous food");
                do_dec_stat(A_CON);
                ident = TRUE;
                break;
            }

            case SV_FOOD_STUPIDITY:
            {
                take_hit(DAMAGE_NOESCAPE, damroll(8, 8), "poisonous food");
                do_dec_stat(A_INT);
                ident = TRUE;
                break;
            }

            case SV_FOOD_NAIVETY:
            {
                take_hit(DAMAGE_NOESCAPE, damroll(8, 8), "poisonous food");
                do_dec_stat(A_WIS);
                ident = TRUE;
                break;
            }

            case SV_FOOD_UNHEALTH:
            {
                take_hit(DAMAGE_NOESCAPE, damroll(10, 10), "poisonous food");
                do_dec_stat(A_CON);
                ident = TRUE;
                break;
            }

            case SV_FOOD_DISEASE:
            {
                take_hit(DAMAGE_NOESCAPE, damroll(10, 10), "poisonous food");
                do_dec_stat(A_STR);
                ident = TRUE;
                break;
            }

            case SV_FOOD_CURE_POISON:
            {
                if (set_poisoned(0, TRUE)) ident = TRUE;
                break;
            }

            case SV_FOOD_CURE_BLINDNESS:
            {
                if (set_blind(0, TRUE)) ident = TRUE;
                break;
            }

            case SV_FOOD_CURE_PARANOIA:
            {
                if (p_ptr->afraid)
                {
                    fear_clear_p();
                    ident = TRUE;
                }
                break;
            }

            case SV_FOOD_CURE_CONFUSION:
            {
                if (set_confused(0, TRUE)) ident = TRUE;
                break;
            }

            case SV_FOOD_FAST_RECOVERY:
            {
				if (hp_player(damroll(2, 8))) ident = TRUE;
                if (set_cut((p_ptr->cut / 2) - 50, TRUE)) ident = TRUE;
				if (set_tim_regen(100 + randint1(100), FALSE)) ident = TRUE;
                break;
            }

            case SV_FOOD_RESTORE_STR:
            {
                if (do_res_stat(A_STR)) ident = TRUE;
                break;
            }

            case SV_FOOD_RESTORE_CON:
            {
                if (do_res_stat(A_CON)) ident = TRUE;
                break;
            }

            case SV_FOOD_RESTORING:
            {
                if (do_res_stat(A_STR)) ident = TRUE;
                if (do_res_stat(A_INT)) ident = TRUE;
                if (do_res_stat(A_WIS)) ident = TRUE;
                if (do_res_stat(A_DEX)) ident = TRUE;
                if (do_res_stat(A_CON)) ident = TRUE;
                if (do_res_stat(A_CHR)) ident = TRUE;
                break;
            }
            case SV_FOOD_RATION:
            case SV_FOOD_BISCUIT:
            case SV_FOOD_JERKY:
            case SV_FOOD_SLIME_MOLD:
            {
                msg_print("That tastes good.");
                ident = TRUE;
                break;
            }
            case SV_FOOD_AMBROSIA:
            {
                msg_print("That tastes divine!");
                set_poisoned(p_ptr->poisoned - MAX(100, p_ptr->poisoned / 5), TRUE);
                hp_player(damroll(15, 15));
                do_res_stat(A_STR);
                do_res_stat(A_INT);
                do_res_stat(A_WIS);
                do_res_stat(A_DEX);
                do_res_stat(A_CON);
                do_res_stat(A_CHR);
                restore_level();
                ident = TRUE;
                break;
            }

            case SV_FOOD_WAYBREAD:
            {
                msg_print("That tastes good.");
                set_poisoned(p_ptr->poisoned - MAX(100, p_ptr->poisoned / 5), TRUE);
                hp_player(damroll(4, 8));
                ident = TRUE;
                break;
            }

            case SV_FOOD_PINT_OF_ALE:
            case SV_FOOD_PINT_OF_WINE:
            {
                msg_print("That tastes good.");
                ident = TRUE;
                break;
            }
        }
    }

    if (prace_is_(RACE_SNOTLING) && object_is_mushroom(obj))
    {
        int dur = lev + randint1(lev);
        set_fast(p_ptr->fast + dur, FALSE);
        set_shield(p_ptr->shield + dur, FALSE);
        set_hero(p_ptr->hero + dur, FALSE);
        set_tim_building_up(p_ptr->tim_building_up + dur, FALSE);
    }

    if (!object_is_aware(obj))
    {
        virtue_add(VIRTUE_KNOWLEDGE, -1);
        virtue_add(VIRTUE_PATIENCE, -1);
        virtue_add(VIRTUE_CHANCE, 1);
    }

    /* We have tried it */
    if (obj->tval == TV_FOOD) object_tried(obj);

    stats_on_use(obj, 1);

    /* The player is now aware of the object */
    if (ident && !object_is_aware(obj))
    {
        object_aware(obj);
        stats_on_notice(obj, 1);
        gain_exp((lev + (p_ptr->lev >> 1)) / p_ptr->lev);
        p_ptr->notice |= PN_OPTIMIZE_PACK;
    }

    /* Food can feed the player */
    if ( prace_is_(RACE_VAMPIRE)
      || prace_is_(RACE_MON_VAMPIRE)
      || p_ptr->mimic_form == MIMIC_VAMPIRE )
    {
        /* Reduced nutritional benefit */
        set_food(p_ptr->food + obj->pval / 10);
        msg_print("Mere victuals hold scant sustenance for a being such as yourself.");
        if (p_ptr->food < PY_FOOD_ALERT)   /* Hungry */
            msg_print("Your hunger can only be satisfied with fresh blood!");
    }
    else if (prace_is_(RACE_MON_JELLY))
    {
        jelly_eat_object(obj);
    }
    else if ( ( prace_is_(RACE_SKELETON)
             || prace_is_(RACE_GOLEM)
             || prace_is_(RACE_MON_GOLEM)
             || prace_is_(RACE_MON_SWORD)
             || prace_is_(RACE_MON_RING)
             || p_ptr->mimic_form == MIMIC_CLAY_GOLEM
             || p_ptr->mimic_form == MIMIC_IRON_GOLEM
             || p_ptr->mimic_form == MIMIC_MITHRIL_GOLEM
             || p_ptr->mimic_form == MIMIC_COLOSSUS
             || prace_is_(RACE_ZOMBIE)
             || prace_is_(RACE_MON_LICH)
             || prace_is_(RACE_SPECTRE)
             || prace_is_(RACE_MON_VORTEX)
             || elemental_is_(ELEMENTAL_AIR) )
           && object_is_device(obj) )
    {
        int amt = obj->activation.cost;

        if (amt > device_sp(obj))
            amt = device_sp(obj);

        if (!amt)
        {
            msg_print("The device has no energy left.");
            return;
        }

        device_decrease_sp(obj, amt);
        set_food(p_ptr->food + 5000);

        obj_describe_charges(obj);
        p_ptr->window |= PW_INVEN;

        /* Don't consume the object */
        return;
    }
    else if ( (get_race()->flags & RACE_IS_DEMON)
           && obj->tval == TV_CORPSE
           && obj->sval == SV_CORPSE
           && my_strchr("pht", r_info[obj->pval].d_char) )
    {
        /* Drain vitality of humanoids */
        char o_name[MAX_NLEN];

        object_desc(o_name, obj, (OD_OMIT_PREFIX | OD_NAME_ONLY));

        msg_format("%^s is burnt to ashes. You absorb its vitality!", o_name);
        set_food(PY_FOOD_MAX - 1);
    }
    else if (prace_is_(RACE_SKELETON))
    {
        if (!(obj->sval == SV_FOOD_WAYBREAD ||
              obj->sval < SV_FOOD_BISCUIT))
        {
            int luku = obj->number;
			msg_print("The food falls through your jaws!");
			obj->number = 1;
            drop_near(obj, -1, py, px);
			obj->number = luku;
        }
        else
        {
            msg_print("The food falls through your jaws and vanishes!");
        }
    }
    else if ((get_race()->flags & RACE_IS_NONLIVING) || prace_is_(RACE_ENT))
    {
        msg_print("The food of mortals is poor sustenance for you.");
        set_food(p_ptr->food + obj->pval / 20);
    }
    else if (obj->tval == TV_FOOD && (obj->sval == SV_FOOD_WAYBREAD || obj->sval == SV_FOOD_AMBROSIA))
    {
        /* Waybread is always fully satisfying. */
        set_food(MAX(p_ptr->food, PY_FOOD_MAX - 1));
    }
    else
    {
        /* Food can feed the player */
        set_food(p_ptr->food + obj->pval);
    }

    /* Consume the object */
    if (obj->art_name) /* Hack: Artifact Food does not get destroyed! */
        obj->timeout += 99;
    else
    {
        obj->number--;
        obj_release(obj, 0);
    }
}


/*
 * Hook to determine if an object is eatable
 */
static bool _can_eat(object_type *o_ptr)
{
    if (o_ptr->tval==TV_FOOD) return TRUE;

    if (prace_is_(RACE_SKELETON) ||
        prace_is_(RACE_GOLEM) ||
        prace_is_(RACE_MON_GOLEM) ||
        prace_is_(RACE_MON_SWORD) ||
        prace_is_(RACE_MON_RING) ||
        p_ptr->mimic_form == MIMIC_CLAY_GOLEM ||
        p_ptr->mimic_form == MIMIC_IRON_GOLEM ||
        p_ptr->mimic_form == MIMIC_MITHRIL_GOLEM ||
        p_ptr->mimic_form == MIMIC_COLOSSUS ||
        prace_is_(RACE_ZOMBIE) ||
        prace_is_(RACE_MON_LICH) ||
        prace_is_(RACE_MON_VORTEX) ||
        prace_is_(RACE_SPECTRE) ||
        elemental_is_(ELEMENTAL_AIR))
    {
        if (object_is_device(o_ptr))
            return TRUE;
    }
    else if (get_race()->flags & RACE_IS_DEMON)
    {
        if (o_ptr->tval == TV_CORPSE &&
            o_ptr->sval == SV_CORPSE &&
            my_strchr("pht", r_info[o_ptr->pval].d_char))
            return TRUE;
    }
    else if (prace_is_(RACE_MON_JELLY))
        return TRUE;

    return FALSE;
}


/*
 * Eat some food (from the pack or floor)
 */
void do_cmd_eat_food(void)
{
    obj_prompt_t prompt = {0};

    if (p_ptr->special_defense & (KATA_MUSOU | KATA_KOUKIJIN))
        set_action(ACTION_NONE);

    prompt.prompt = "Eat which item?";
    prompt.error = "You have nothing to eat.";
    prompt.filter = _can_eat;
    prompt.where[0] = INV_PACK;
    prompt.where[1] = INV_FLOOR;

    obj_prompt(&prompt);
    if (!prompt.obj) return;

    do_cmd_eat_food_aux(prompt.obj);
}


/*
 * Quaff a potion (from the pack or the floor)
 */
static void do_cmd_quaff_potion_aux(obj_ptr obj)
{
    int   lev = k_info[obj->k_idx].level;
    int   number = 1;

    /* Take a turn */
    if (mut_present(MUT_POTION_CHUGGER))
        energy_use = 50;
    else
        energy_use = 100;

    if (world_player)
    {
        if (flush_failure) flush();
        msg_print("The potion doesn't flow out from the bottle.");
        if (prompt_on_failure) msg_print(NULL);
        sound(SOUND_FAIL);
        return;
    }

    if (music_singing_any()) bard_stop_singing();
    if (hex_spelling_any())
    {
        if (!hex_spelling(HEX_INHAIL)) stop_hex_spell_all();
    }
    warlock_stop_singing();

    if (devicemaster_is_(DEVICEMASTER_POTIONS) && !devicemaster_desperation)
    {
        int delta = MIN(50, 2*p_ptr->lev - lev);
        if (delta > 0)
        {
            energy_use -= delta;
            if (energy_use < 25) energy_use = 25; /* Potion Chuggers could go negative! */
        }
    }
    if (devicemaster_desperation)
    {
        int i, amt = 50;
        number = obj->number;
        if (number > 4) number = 4;
        for (i = 1; i < number && amt; i++)
        {
            device_extra_power += amt;
            amt /= 2;
        }
    }

    sound(SOUND_QUAFF);
    if (obj->tval == TV_POTION) /* Skip Flasks of Oil */
        device_use(obj, 0);

    if (prace_is_(RACE_SKELETON) || (p_ptr->current_r_idx && r_info[p_ptr->current_r_idx].d_char == 's'))
    {
        msg_print("Some of the fluid falls through your jaws!");
        potion_smash_effect(0, py, px, obj->k_idx);
    }

    if (!object_is_aware(obj))
    {
        virtue_add(VIRTUE_PATIENCE, -1);
        virtue_add(VIRTUE_CHANCE, 1);
        virtue_add(VIRTUE_KNOWLEDGE, -1);
    }

    object_tried(obj);
    if (device_noticed && !object_is_aware(obj))
    {
        object_aware(obj);
        gain_exp((lev + (p_ptr->lev >> 1)) / p_ptr->lev);
        p_ptr->notice |= PN_OPTIMIZE_PACK;
    }

    /* Potions can feed the player */
    switch (p_ptr->mimic_form)
    {
    case MIMIC_NONE:
        switch (p_ptr->prace)
        {
            case RACE_VAMPIRE:
            case RACE_MON_VAMPIRE:
                set_food(p_ptr->food + obj->pval / 10);
                break;
            case RACE_SKELETON:
            case RACE_MON_JELLY:
                break;
            case RACE_GOLEM:
            case RACE_MON_GOLEM:
            case RACE_MON_VORTEX:
            case RACE_ZOMBIE:
            case RACE_MON_LICH:
            case RACE_BALROG:
            case RACE_SPECTRE:
            case RACE_MON_DEMON:
            case RACE_MON_SWORD:
            case RACE_MON_RING:
                set_food(p_ptr->food + obj->pval / 20);
                break;
            case RACE_ANDROID:
                if (obj->tval == TV_FLASK)
                {
                    msg_print("You replenish yourself with the oil.");
                    set_food(p_ptr->food + 5000);
                }
                else
                {
                    set_food(p_ptr->food + obj->pval / 20);
                }
                break;
            case RACE_ENT:
                msg_print("You are moistened.");
                set_food(MIN(p_ptr->food + obj->pval + MAX(0, obj->pval * 10) + 2000, PY_FOOD_MAX - 1));
                break;
            default:
                if (elemental_is_(ELEMENTAL_WATER))
                {
                    msg_print("That tastes delicious.");
                    set_food(MIN(p_ptr->food + obj->pval + MAX(0, obj->pval * 10) + 2000, PY_FOOD_MAX - 1));
                }
                else if (elemental_is_(ELEMENTAL_FIRE) && obj->tval == TV_FLASK)
                {
                    msg_print("Your body flames up with renewed vigor.");
                    set_food(p_ptr->food + 5000);
                }
                else
                    set_food(p_ptr->food + obj->pval);
                break;
        }
        break;
    case MIMIC_DEMON:
    case MIMIC_DEMON_LORD:
        set_food(p_ptr->food + obj->pval / 20);
        break;
    case MIMIC_VAMPIRE:
        set_food(p_ptr->food + obj->pval / 10);
        break;
    default:
        set_food(p_ptr->food + obj->pval);
        break;
    }

    /* Consume Item */
    if (devicemaster_is_(DEVICEMASTER_POTIONS) && !devicemaster_desperation && randint1(3*p_ptr->lev/2) > MAX(10, lev))
    {
        msg_print("You sip the potion sparingly.");
    }
    else
    {
        stats_on_use(obj, number);
        if (obj->loc.where == INV_FLOOR)
            stats_on_pickup(obj);

        obj->number -= number;
        obj_release(obj, OBJ_RELEASE_DELAYED_MSG);
    }
}


/*
 * Hook to determine if an object can be quaffed
 */
static bool _can_quaff(object_type *o_ptr)
{
    if (o_ptr->tval == TV_POTION) return TRUE;

    if (prace_is_(RACE_ANDROID) || elemental_is_(ELEMENTAL_FIRE))
    {
        if (o_ptr->tval == TV_FLASK && o_ptr->sval == SV_FLASK_OIL)
            return TRUE;
    }

    return FALSE;
}


/*
 * Quaff some potion (from the pack or floor)
 */
void do_cmd_quaff_potion(void)
{
    obj_prompt_t prompt = {0};

    if (p_ptr->special_defense & (KATA_MUSOU | KATA_KOUKIJIN))
        set_action(ACTION_NONE);

    prompt.prompt = "Quaff which potion?";
    prompt.error = "You have no potions to quaff.";
    prompt.filter = _can_quaff;
    prompt.where[0] = INV_PACK;
    prompt.where[1] = INV_FLOOR;

    obj_prompt(&prompt);
    if (!prompt.obj) return;

    do_cmd_quaff_potion_aux(prompt.obj);
}


/*
 * Read a scroll (from the pack or floor).
 *
 * Certain scrolls can be "aborted" without losing the scroll. These
 * include scrolls with no effects but recharge or identify, which are
 * cancelled before use. XXX Reading them still takes a turn, though.
 */
static void do_cmd_read_scroll_aux(obj_ptr o_ptr)
{
    int  used_up, lev = k_info[o_ptr->k_idx].level;
    int  number = 1;
    bool known = object_is_aware(o_ptr);

    /* Take a turn */
    if (mut_present(MUT_SPEED_READER))
        energy_use = 50;
    else
        energy_use = 100;

    if (devicemaster_is_(DEVICEMASTER_SCROLLS) && !devicemaster_desperation)
    {
        int delta = MIN(50, 2*p_ptr->lev - lev);
        if (delta > 0)
        {
            energy_use -= delta;
            if (energy_use < 25) energy_use = 25; /* Speed Readers could go negative! */
        }
    }

    /* Hack: Block devices *after* consuming player energy */
    if (p_ptr->tim_no_device)
    {
        msg_print("An evil power blocks your magic!");
        return;
    }

    if (world_player)
    {
        if (flush_failure) flush();
        msg_print("Nothing happens.");
        if (prompt_on_failure) msg_print(NULL);
        sound(SOUND_FAIL);
        return;
    }

    if (p_ptr->pclass == CLASS_BERSERKER || (get_race()->flags & RACE_IS_ILLITERATE))
    {
        msg_print("You cannot read.");
        return;
    }

    if (music_singing_any()) bard_stop_singing();

    /* Hex */
    if (hex_spelling_any() && ((p_ptr->lev < 35) || hex_spell_fully())) stop_hex_spell_all();

    warlock_stop_singing();

    /* Assume the scroll will get used up */
    used_up = TRUE;

    if (o_ptr->tval == TV_SCROLL)
    {
        if (object_is_(o_ptr, TV_SCROLL, SV_SCROLL_IDENTIFY))
            device_available_charges = o_ptr->number;

        if (!device_try(o_ptr))
        {
            if (flush_failure) flush();
            msg_print("You failed to pronounce the incantation properly.");
            if (prompt_on_failure) msg_print(NULL);
            sound(SOUND_FAIL);
            return;
        }

        if (devicemaster_desperation)
        {
            int i, amt = 50;
            number = o_ptr->number;
            if (number > 4) number = 4;
            for (i = 1; i < number && amt; i++)
            {
                device_extra_power += amt;
                amt /= 2;
            }
        }
        used_up = device_use(o_ptr, 0);
        if (object_is_(o_ptr, TV_SCROLL, SV_SCROLL_IDENTIFY))
            number = device_used_charges;
    }
    else if (o_ptr->name1 == ART_POWER)
    {
        msg_print("'One Ring to rule them all, ");
        msg_print(NULL);
        msg_print("One Ring to find them, ");
        msg_print(NULL);
        msg_print("One Ring to bring them all ");
        msg_print(NULL);
        msg_print("and in the darkness bind them.'");
        used_up = FALSE;
    }
    else if (o_ptr->tval==TV_PARCHMENT)
    {
        cptr q;
        char o_name[MAX_NLEN];
        char buf[1024];

        screen_save();
        q=format("book-%d_jp.txt",o_ptr->sval);
        object_desc(o_name, o_ptr, OD_NAME_ONLY);
        path_build(buf, sizeof(buf), ANGBAND_DIR_FILE, q);
        (void)show_file(TRUE, buf, o_name, 0, 0);
        screen_load();

        used_up = FALSE;
    }

    if (!known)
    {
        virtue_add(VIRTUE_PATIENCE, -1);
        virtue_add(VIRTUE_CHANCE, 1);
        virtue_add(VIRTUE_KNOWLEDGE, -1);
    }

    object_tried(o_ptr);
    if (device_noticed && !known)
    {
        object_aware(o_ptr);
        gear_notice_id(o_ptr);
        stats_on_notice(o_ptr, o_ptr->number);
        gain_exp((lev + (p_ptr->lev >> 1)) / p_ptr->lev);
    }

    if (!used_up)
        return;

    sound(SOUND_SCROLL);
    if (devicemaster_is_(DEVICEMASTER_SCROLLS) && !devicemaster_desperation && randint1(2*p_ptr->lev) > MAX(10, lev))
    {
        msg_print("Your mental focus preserves the scroll!");
    }
    else
    {
        stats_on_use(o_ptr, number);
        o_ptr->number -= number;
        obj_release(o_ptr, OBJ_RELEASE_DELAYED_MSG);
    }
}

static bool _can_read(object_type *o_ptr)
{
    if (!o_ptr) return FALSE;
    if (o_ptr->tval==TV_SCROLL || o_ptr->tval==TV_PARCHMENT || o_ptr->name1 == ART_POWER)
        return TRUE;
    return FALSE;
}

void do_cmd_read_scroll(void)
{
    obj_prompt_t prompt = {0};

    if (p_ptr->special_defense & (KATA_MUSOU | KATA_KOUKIJIN))
        set_action(ACTION_NONE);

    /* Check some conditions */
    if (p_ptr->blind)
    {
        msg_print("You can't see anything.");
        return;
    }
    if (no_lite())
    {
        msg_print("You have no light to read by.");
        return;
    }
    if (p_ptr->confused)
    {
        msg_print("You are too confused!");
        return;
    }

    prompt.prompt = "Read which scroll? ";
    prompt.error = "You have no scrolls to read.";
    prompt.filter = _can_read;
    prompt.where[0] = INV_PACK;
    prompt.where[1] = INV_FLOOR;
    prompt.flags = INV_SHOW_FAIL_RATES;

    obj_prompt(&prompt);
    if (!prompt.obj) return;

    do_cmd_read_scroll_aux(prompt.obj);
}

/* Helper for Rods, Wands and Staves */
static void do_cmd_device_aux(obj_ptr obj)
{
    bool used = FALSE;
    int  charges = 1;
    int  boost;
    bool is_devicemaster = FALSE;
    u32b flgs[OF_ARRAY_SIZE];

    assert(obj->number == 1); /* Devices no longer stack */
    obj_flags(obj, flgs);

    /* Devicemasters get extra power */
    is_devicemaster = devicemaster_is_speciality(obj);
    if (is_devicemaster)
        boost = device_power_aux(100, p_ptr->device_power + p_ptr->lev/10) - 100;
    else
        boost = device_power(100) - 100;

    /* Devicemasters use devices more quickly */
    energy_use = 100;
    if (is_devicemaster && !devicemaster_desperation)
    {
        int delta = MIN(50, 2*p_ptr->lev - obj->activation.power);
        if (delta > 0)
            energy_use -= delta;
    }

    if (have_flag(flgs, OF_SPEED))
        energy_use -= energy_use * obj->pval / 10;

    if (p_ptr->tim_no_device)
    {
        msg_print("An evil power blocks your magic!");
        return;
    }

    /* Devicemasters use devices even when afraid */
    if (!(is_devicemaster || fear_allow_device()))
    {
        msg_print("You are too scared!");
        return;
    }

    if (world_player)
    {
        if (flush_failure) flush();
        msg_print("Nothing happens. Maybe this device is freezing too.");
        if (prompt_on_failure) msg_print(NULL);
        sound(SOUND_FAIL);
        return;
    }

    if (!device_try(obj))
    {
        if (flush_failure) flush();
        msg_print("You failed to use the device properly.");
        if (prompt_on_failure) msg_print(NULL);
        sound(SOUND_FAIL);
        if ( obj_is_identified(obj)
          && one_in_(10)
          && !obj_is_identified_fully(obj)
          && !(obj->known_xtra & OFL_DEVICE_FAIL))
        {
            char buf[MAX_NLEN];
            object_desc(buf, obj, OD_LORE);
            msg_format("<color:B>You learn more about your %s.</color>", buf);
            obj->known_xtra |= OFL_DEVICE_FAIL;
            if (obj->known_xtra & OFL_DEVICE_POWER)
                add_flag(obj->known_flags, OF_ACTIVATE);
        }
        return;
    }

    if ((obj->curse_flags & OFC_CURSED) && one_in_(6))
    {
        msg_print("Oops! The device explodes!");
        project(
            PROJECT_WHO_UNCTRL_POWER, 4, py, px,
            device_sp(obj), GF_MANA,
            PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL);
        obj->number = 0;
        obj_release(obj, OBJ_RELEASE_QUIET);
        return;
    }

    if (device_sp(obj) < obj->activation.cost)
    {
        if (flush_failure) flush();
        msg_print("The device has no charges left.");
        if (prompt_on_failure) msg_print(NULL);
        energy_use = 0;
        return;
    }

    if (is_devicemaster && devicemaster_desperation)
    {
        int i, amt = 50;
        charges = device_sp(obj) / obj->activation.cost;
        for (i = 1; i < charges && amt; i++)
        {
            boost += amt;
            amt /= 2;
        }
    }

    if (obj->activation.type == EFFECT_IDENTIFY)
        device_available_charges = device_sp(obj) / obj->activation.cost;

    sound(SOUND_ZAP);
    used = device_use(obj, boost);

    if (obj->activation.type == EFFECT_IDENTIFY)
        charges = device_used_charges;

    object_tried(obj);
    if (device_noticed && !object_is_known(obj))
    {
        identify_item(obj);
        autopick_alter_obj(obj, destroy_identify);
    }
    if ( device_lore
      && !obj_is_identified_fully(obj)
      && !(obj->known_xtra & OFL_DEVICE_POWER) )
    {
        char buf[MAX_NLEN];
        object_desc(buf, obj, OD_LORE);
        msg_format("<color:B>You learn more about your %s.</color>", buf);
        obj->known_xtra |= OFL_DEVICE_POWER;
        if (obj->known_xtra & OFL_DEVICE_FAIL)
            add_flag(obj->known_flags, OF_ACTIVATE);
    }

    if (used)
    {
        stats_on_use(obj, charges);

        /* Devicemasters can power the device with their mana once in a while */
        if ( is_devicemaster
          && !devicemaster_desperation
          && p_ptr->csp > obj->activation.cost
          && randint1(20 + obj->activation.difficulty) <= p_ptr->lev )
        {
            msg_print("Your mental focus powers the device!");
            sp_player(-obj->activation.cost);
        }
        else
        {
            /* Devicemaster Desperation can destroy the device! */
            if (is_devicemaster && devicemaster_desperation && randint0(p_ptr->lev*7) < k_info[obj->k_idx].level)
            {
                char o_name[MAX_NLEN];
                object_desc(o_name, obj, OD_OMIT_PREFIX | OD_NAME_ONLY | OD_COLOR_CODED);
                msg_format("Desperation magic consumes your %s!", o_name);
                obj->number = 0;
            }
            else
            {
                device_decrease_sp(obj, obj->activation.cost * charges);
                obj->marked |= OM_DELAYED_MSG;
                p_ptr->notice |= PN_CARRY;
            }
        }
    }
    else
        energy_use = 0;

    obj_release(obj, OBJ_RELEASE_QUIET);
}

void do_cmd_use_staff(void)
{
    obj_prompt_t prompt = {0};

    if (p_ptr->special_defense & (KATA_MUSOU | KATA_KOUKIJIN))
        set_action(ACTION_NONE);

    if (p_ptr->pclass == CLASS_MAGIC_EATER && !pack_find_obj(TV_STAFF, SV_ANY))
    {
        magic_eater_cast(TV_STAFF);
        return;
    }

    prompt.prompt = "Use which staff?";
    prompt.error = "You have no staff to use.";
    prompt.filter = obj_is_staff;
    prompt.where[0] = INV_PACK;
    prompt.where[1] = INV_FLOOR;
    prompt.flags = INV_SHOW_FAIL_RATES;

    obj_prompt(&prompt);
    if (!prompt.obj) return;

    do_cmd_device_aux(prompt.obj);
}


void do_cmd_aim_wand(void)
{
    obj_prompt_t prompt = {0};

    if (p_ptr->special_defense & (KATA_MUSOU | KATA_KOUKIJIN))
        set_action(ACTION_NONE);

    if (p_ptr->pclass == CLASS_MAGIC_EATER && !pack_find_obj(TV_WAND, SV_ANY))
    {
        magic_eater_cast(TV_WAND);
        return;
    }

    prompt.prompt = "Aim which wand?";
    prompt.error = "You have no wand to aim.";
    prompt.filter = obj_is_wand;
    prompt.where[0] = INV_PACK;
    prompt.where[1] = INV_FLOOR;
    prompt.flags = INV_SHOW_FAIL_RATES;

    obj_prompt(&prompt);
    if (!prompt.obj) return;

    do_cmd_device_aux(prompt.obj);
}

void do_cmd_zap_rod(void)
{
    obj_prompt_t prompt = {0};

    if (p_ptr->special_defense & (KATA_MUSOU | KATA_KOUKIJIN))
        set_action(ACTION_NONE);

    if (p_ptr->pclass == CLASS_MAGIC_EATER && !pack_find_obj(TV_ROD, SV_ANY))
    {
        magic_eater_cast(TV_ROD);
        return;
    }

    prompt.prompt = "Zap which rod?";
    prompt.error = "You have no rod to zap.";
    prompt.filter = obj_is_rod;
    prompt.where[0] = INV_PACK;
    prompt.where[1] = INV_FLOOR;
    prompt.flags = INV_SHOW_FAIL_RATES;

    obj_prompt(&prompt);
    if (!prompt.obj) return;

    do_cmd_device_aux(prompt.obj);
}


/*
 * Hack -- activate the ring of power
 */
void ring_of_power(int dir)
{
    /* Pick a random effect */
    switch (randint1(10))
    {
        case 1:
        case 2:
        {
            /* Message */
            msg_print("You are surrounded by a malignant aura.");

            sound(SOUND_EVIL);

            /* Decrease all stats (permanently) */
            (void)dec_stat(A_STR, 50, TRUE);
            (void)dec_stat(A_INT, 50, TRUE);
            (void)dec_stat(A_WIS, 50, TRUE);
            (void)dec_stat(A_DEX, 50, TRUE);
            (void)dec_stat(A_CON, 50, TRUE);
            (void)dec_stat(A_CHR, 50, TRUE);

            /* Lose some experience (permanently) */
            p_ptr->exp -= (p_ptr->exp / 4);
            p_ptr->max_exp -= (p_ptr->exp / 4);
            check_experience();

            break;
        }

        case 3:
        {
            /* Message */
            msg_print("You are surrounded by a powerful aura.");


            /* Dispel monsters */
            dispel_monsters(1000);

            break;
        }

        case 4:
        case 5:
        case 6:
        {
            /* Mana Ball */
            fire_ball(GF_MANA, dir, 600, 3);

            break;
        }

        case 7:
        case 8:
        case 9:
        case 10:
        {
            /* Mana Bolt */
            fire_bolt(GF_MANA, dir, 500);

            break;
        }
    }
}


static void _do_capture_ball(object_type *o_ptr)
{
    int dir;
    if (!o_ptr->pval)
    {
        bool old_target_pet = target_pet;
        target_pet = TRUE;
        if (!get_aim_dir(&dir))
        {
            target_pet = old_target_pet;
            return;
        }
        target_pet = old_target_pet;

        if (fire_ball(GF_CAPTURE, dir, 0, 0))
        {
            o_ptr->pval = cap_mon;
            o_ptr->xtra3 = cap_mspeed;
            o_ptr->xtra4 = cap_hp;
            o_ptr->xtra5 = cap_maxhp;
            if (cap_nickname)
            {
                cptr t;
                char *s;
                char buf[80] = "";

                if (o_ptr->inscription)
                    strcpy(buf, quark_str(o_ptr->inscription));
                s = buf;
                for (s = buf;*s && (*s != '#'); s++)
                {
                }
                *s = '#';
                s++;
                *s++ = '\'';
                t = quark_str(cap_nickname);
                while (*t)
                {
                    *s = *t;
                    s++;
                    t++;
                }
                *s++ = '\'';
                *s = '\0';
                o_ptr->inscription = quark_add(buf);
            }
        }
    }
    else
    {
        bool success = FALSE;
        if (!get_rep_dir2(&dir)) return;
        if (monster_can_enter(py + ddy[dir], px + ddx[dir], &r_info[o_ptr->pval], 0))
        {
            if (place_monster_aux(0, py + ddy[dir], px + ddx[dir], o_ptr->pval, (PM_FORCE_PET | PM_NO_KAGE)))
            {
                if (o_ptr->xtra3) m_list[hack_m_idx_ii].mspeed = o_ptr->xtra3;
                if (o_ptr->xtra5) m_list[hack_m_idx_ii].max_maxhp = o_ptr->xtra5;
                if (o_ptr->xtra4) m_list[hack_m_idx_ii].hp = o_ptr->xtra4;
                m_list[hack_m_idx_ii].maxhp = m_list[hack_m_idx_ii].max_maxhp;
                if (o_ptr->inscription)
                {
                    char buf[80];
                    cptr t;
                    bool quote = FALSE;

                    t = quark_str(o_ptr->inscription);
                    for (t = quark_str(o_ptr->inscription);*t && (*t != '#'); t++)
                    {
                    }
                    if (*t)
                    {
                        char *s = buf;
                        t++;
                        if (*t =='\'')
                        {
                            t++;
                            quote = TRUE;
                        }
                        while(*t)
                        {
                            *s = *t;
                            t++;
                            s++;
                        }
                        if (quote && *(s-1) =='\'')
                            s--;
                        *s = '\0';
                        m_list[hack_m_idx_ii].nickname = quark_add(buf);
                        t = quark_str(o_ptr->inscription);
                        s = buf;
                        while(*t && (*t != '#'))
                        {
                            *s = *t;
                            t++;
                            s++;
                        }
                        *s = '\0';
                        o_ptr->inscription = quark_add(buf);
                    }
                }
                o_ptr->pval = 0;
                o_ptr->xtra3 = 0;
                o_ptr->xtra4 = 0;
                o_ptr->xtra5 = 0;
                success = TRUE;
            }
        }
        if (!success)
            msg_print("Oops. You failed to release your pet.");
    }
}

/*
 * Activate a wielded object. Wielded objects never stack.
 * And even if they did, activatable objects never stack.
 *
 * Return false for illegal activation attempts and use no energy
 *
 */
static bool do_cmd_activate_aux(obj_ptr obj)
{
    cptr     msg;
    effect_t effect;
    int      boost = device_power(100) - 100;
    u32b     flgs[OF_ARRAY_SIZE];

    obj_flags_known(obj, flgs);

    if (world_player)
    {
        if (flush_failure) flush();
        msg_print("It shows no reaction.");
        sound(SOUND_FAIL);
        return FALSE;
    }

	if (obj->timeout)
	{
		msg_print("This item is still recharging.");
		return FALSE;
	}

    effect = obj_get_effect(obj);
    if (!effect_try(&effect))
    {
        if (flush_failure) flush();
        msg_print("You failed to activate it properly.");
        if (prompt_on_failure) msg_print(NULL);
        sound(SOUND_FAIL);
        return TRUE;
    }

    msg_print("You activate it...");
    sound(SOUND_ZAP);

    msg = obj_get_effect_msg(obj);
    if (msg)
        msg_print(msg);

    if (obj->tval == TV_CAPTURE)
    {
        _do_capture_ball(obj);
        return TRUE;
    }
    device_known = have_flag(flgs, OF_ACTIVATE);
    if (effect_use(&effect, boost))
    {
        if (device_noticed)
            obj_learn_activation(obj);

        obj->timeout = effect.cost;
        p_ptr->window |= (PW_INVEN | PW_EQUIP);
    }
	return TRUE;
}

static bool _activate_p(object_type *o_ptr)
{
    return /*obj_is_identified(o_ptr) &&*/ obj_has_effect(o_ptr);
}

void do_cmd_activate(void)
{
    obj_prompt_t prompt = {0};

    if (p_ptr->special_defense & (KATA_MUSOU | KATA_KOUKIJIN))
        set_action(ACTION_NONE);

    prompt.prompt ="Activate which item?"; 
    prompt.error = "You have nothing to activate.";
    prompt.filter = _activate_p;
    prompt.where[0] = INV_EQUIP;
    prompt.flags = INV_SHOW_FAIL_RATES;

    obj_prompt(&prompt);
    if (!prompt.obj) return;
    if (do_cmd_activate_aux(prompt.obj))
		/* Take a turn */
		energy_use = 100;
}

/* helper code from experimental aut-activations */
/* auto-activations have no energy cost but they have increased  cooldowns */
/* they incur cooldown periods even when the activation fails */
bool do_auto_activate(obj_ptr obj)
{
	cptr     msg;
	cptr     desc;
	effect_t effect;
	int      boost = device_power(100) - 100;
	u32b     flgs[OF_ARRAY_SIZE];

	if (world_player)
	{
		if (flush_failure) flush();
		return FALSE;
	}

	effect = obj_get_effect(obj);

	msg = obj_get_effect_msg(obj);
	if (msg)
	{
		msg_print(msg);
	}

}