/* File: cmd6.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* Purpose: Object commands */

#include "angband.h"
#include "equip.h"

/*
 * This file includes code for eating food, drinking potions,
 * reading scrolls, aiming wands, using staffs, zapping rods,
 * and activating artifacts.
 *
 * In all cases, if the player becomes "aware" of the item's use
 * by testing it, mark it as "aware" and reward some experience
 * based on the object's level, always rounding up.  If the player
 * remains "unaware", mark that object "kind" as "tried".
 *
 * This code now correctly handles the unstacking of wands, staffs,
 * and rods.  Note the overly paranoid warning about potential pack
 * overflow, which allows the player to use and drop a stacked item.
 *
 * In all "unstacking" scenarios, the "used" object is "carried" as if
 * the player had just picked it up.  In particular, this means that if
 * the use of an item induces pack overflow, that item will be dropped.
 *
 * For simplicity, these routines induce a full "pack reorganization"
 * which not only combines similar items, but also reorganizes various
 * items to obey the current "sorting" method.  This may require about
 * 400 item comparisons, but only occasionally.
 *
 * There may be a BIG problem with any "effect" that can cause "changes"
 * to the inventory.  For example, a "scroll of recharging" can cause
 * a wand/staff to "disappear", moving the inventory up.  Luckily, the
 * scrolls all appear BEFORE the staffs/wands, so this is not a problem.
 * But, for example, a "staff of recharging" could cause MAJOR problems.
 * In such a case, it will be best to either (1) "postpone" the effect
 * until the end of the function, or (2) "change" the effect, say, into
 * giving a staff "negative" charges, or "turning a staff into a stick".
 * It seems as though a "rod of recharging" might in fact cause problems.
 * The basic problem is that the act of recharging (and destroying) an
 * item causes the inducer of that action to "move", causing "o_ptr" to
 * no longer point at the correct item, with horrifying results.
 *
 * Note that food/potions/scrolls no longer use bit-flags for effects,
 * but instead use the "sval" (which is also used to sort the objects).
 */

static bool _pack_find_tval(int tval)
{
    int i;
    for (i = 0; i < EQUIP_BEGIN; i++)
    {
        if (inventory[i].tval == tval)
            return TRUE;
    }
    return FALSE;
}


bool restore_mana(void)
{
    bool result = FALSE;
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
        p_ptr->window |= (PW_PLAYER);
        p_ptr->window |= (PW_SPELL);
        result = TRUE;
    }
    msg_print("You feel your head clear.");
    return result;
}

static void do_cmd_eat_food_aux(int item)
{
    int ident, lev;
    object_type *o_ptr;

    if (music_singing_any()) bard_stop_singing();
    if (hex_spelling_any()) stop_hex_spell_all();

    /* Get the item (in the pack) */
    if (item >= 0)
    {
        o_ptr = &inventory[item];
    }

    /* Get the item (on the floor) */
    else
    {
        o_ptr = &o_list[0 - item];
    }

    if (object_is_mushroom(o_ptr) && o_ptr->art_name && o_ptr->timeout)
    {
        msg_print("Your mushroom is still charging.");
        return;
    }

    /* Sound */
    sound(SOUND_EAT);

    /* Take a turn */
    energy_use = 100;

    /* Identity not known yet */
    ident = FALSE;

    /* Object level */
    lev = k_info[o_ptr->k_idx].level;

    if (o_ptr->tval == TV_FOOD)
    {
        /* Analyze the food */
        switch (o_ptr->sval)
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
                    if (set_blind(p_ptr->blind + randint0(200) + 200, FALSE))
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
                if (!p_ptr->free_act)
                {
                    if (set_paralyzed(randint1(4), FALSE))
                    {
                        ident = TRUE;
                    }
                }
                break;
            }

            case SV_FOOD_WEAKNESS:
            {
                take_hit(DAMAGE_NOESCAPE, damroll(6, 6), "poisonous food", -1);
                (void)do_dec_stat(A_STR);
                ident = TRUE;
                break;
            }

            case SV_FOOD_SICKNESS:
            {
                take_hit(DAMAGE_NOESCAPE, damroll(6, 6), "poisonous food", -1);
                (void)do_dec_stat(A_CON);
                ident = TRUE;
                break;
            }

            case SV_FOOD_STUPIDITY:
            {
                take_hit(DAMAGE_NOESCAPE, damroll(8, 8), "poisonous food", -1);
                (void)do_dec_stat(A_INT);
                ident = TRUE;
                break;
            }

            case SV_FOOD_NAIVETY:
            {
                take_hit(DAMAGE_NOESCAPE, damroll(8, 8), "poisonous food", -1);
                (void)do_dec_stat(A_WIS);
                ident = TRUE;
                break;
            }

            case SV_FOOD_UNHEALTH:
            {
                take_hit(DAMAGE_NOESCAPE, damroll(10, 10), "poisonous food", -1);
                (void)do_dec_stat(A_CON);
                ident = TRUE;
                break;
            }

            case SV_FOOD_DISEASE:
            {
                take_hit(DAMAGE_NOESCAPE, damroll(10, 10), "poisonous food", -1);
                (void)do_dec_stat(A_STR);
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

            case SV_FOOD_CURE_SERIOUS:
            {
                if (hp_player(damroll(6, 8))) ident = TRUE;
                if (set_cut((p_ptr->cut / 2) - 50, TRUE)) ident = TRUE;
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
                set_poisoned(0, TRUE);
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
                (void)set_poisoned(0, TRUE);
                (void)hp_player(damroll(4, 8));
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

    if (prace_is_(RACE_SNOTLING) && object_is_mushroom(o_ptr))
    {
        int lev = k_info[o_ptr->k_idx].level;
        int dur = lev + randint1(lev);
        set_fast(p_ptr->fast + dur, FALSE);
        set_shield(p_ptr->shield + dur, FALSE);
        set_hero(p_ptr->hero + dur, FALSE);
        set_tim_building_up(p_ptr->tim_building_up + dur, FALSE);
    }

    /* Combine / Reorder the pack (later) */
    p_ptr->notice |= (PN_COMBINE | PN_REORDER);

    if (!(object_is_aware(o_ptr)))
    {
        virtue_add(VIRTUE_KNOWLEDGE, -1);
        virtue_add(VIRTUE_PATIENCE, -1);
        virtue_add(VIRTUE_CHANCE, 1);
    }

    /* We have tried it */
    if (o_ptr->tval == TV_FOOD) object_tried(o_ptr);

    stats_on_use(o_ptr, 1);

    /* The player is now aware of the object */
    if (ident && !object_is_aware(o_ptr))
    {
        object_aware(o_ptr);
        stats_on_notice(o_ptr, 1);
        gain_exp((lev + (p_ptr->lev >> 1)) / p_ptr->lev);
    }

    /* Window stuff */
    p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);


    /* Food can feed the player */
    if ( prace_is_(RACE_VAMPIRE) 
      || prace_is_(RACE_MON_VAMPIRE) 
      || p_ptr->mimic_form == MIMIC_VAMPIRE )
    {
        /* Reduced nutritional benefit */
        (void)set_food(p_ptr->food + (o_ptr->pval / 10));
        msg_print("Mere victuals hold scant sustenance for a being such as yourself.");
        if (p_ptr->food < PY_FOOD_ALERT)   /* Hungry */
            msg_print("Your hunger can only be satisfied with fresh blood!");
    }
    else if (prace_is_(RACE_MON_JELLY))
    {
        jelly_eat_object(o_ptr);
    }
    else if ((prace_is_(RACE_SKELETON) ||
          prace_is_(RACE_GOLEM) || 
          prace_is_(RACE_MON_GOLEM) || 
          prace_is_(RACE_MON_SWORD) || 
          p_ptr->mimic_form == MIMIC_CLAY_GOLEM ||
          p_ptr->mimic_form == MIMIC_IRON_GOLEM ||
          p_ptr->mimic_form == MIMIC_MITHRIL_GOLEM ||
          p_ptr->mimic_form == MIMIC_COLOSSUS ||
          prace_is_(RACE_ZOMBIE) ||
          prace_is_(RACE_MON_LICH) ||
          prace_is_(RACE_SPECTRE)) &&
         (o_ptr->tval == TV_STAFF || o_ptr->tval == TV_WAND))
    {
        cptr staff;

        if (o_ptr->tval == TV_STAFF &&
            (item < 0) && (o_ptr->number > 1))
        {
            msg_print("You must first pick up the staffs.");
            return;
        }

        staff = (o_ptr->tval == TV_STAFF) ? "staff" : "wand";

        /* "Eat" charges */
        if (o_ptr->pval == 0)
        {
            msg_format("The %s has no charges left.", staff);
            o_ptr->ident |= (IDENT_EMPTY);

            /* Combine / Reorder the pack (later) */
            p_ptr->notice |= (PN_COMBINE | PN_REORDER);
            p_ptr->window |= (PW_INVEN);

            return;
        }

        msg_format("You absorb mana of the %s as your energy.", staff);

        /* Use a single charge */
        o_ptr->pval--;

        /* Eat a charge */
        set_food(p_ptr->food + 5000);

        /* XXX Hack -- unstack if necessary */
        if (o_ptr->tval == TV_STAFF &&
            (item >= 0) && (o_ptr->number > 1))
        {
            object_type forge;
            object_type *q_ptr;

            /* Get local object */
            q_ptr = &forge;

            /* Obtain a local object */
            object_copy(q_ptr, o_ptr);

            /* Modify quantity */
            q_ptr->number = 1;

            /* Restore the charges */
            o_ptr->pval++;

            /* Unstack the used item */
            o_ptr->number--;
            p_ptr->total_weight -= q_ptr->weight;
            item = inven_carry(q_ptr);

            /* Message */
            msg_print("You unstack your staff.");
        }

        /* Describe charges in the pack */
        if (item >= 0)
        {
            inven_item_charges(item);
        }

        /* Describe charges on the floor */
        else
        {
            floor_item_charges(0 - item);
        }

        /* Window stuff */
        p_ptr->window |= (PW_INVEN | PW_EQUIP);

        /* Don't eat a staff/wand itself */
        return;
    }
    else if ((p_ptr->mimic_form == MIMIC_DEMON || p_ptr->mimic_form == MIMIC_DEMON_LORD || prace_is_(RACE_BALROG) || prace_is_(RACE_MON_DEMON)) 
           && (o_ptr->tval == TV_CORPSE && o_ptr->sval == SV_CORPSE && my_strchr("pht", r_info[o_ptr->pval].d_char)))
    {
        /* Drain vitality of humanoids */
        char o_name[MAX_NLEN];

        object_desc(o_name, o_ptr, (OD_OMIT_PREFIX | OD_NAME_ONLY));

        msg_format("%^s is burnt to ashes.  You absorb its vitality!", o_name);
        (void)set_food(PY_FOOD_MAX - 1);
    }
    else if (prace_is_(RACE_SKELETON))
    {
        if (!((o_ptr->sval == SV_FOOD_WAYBREAD) ||
              (o_ptr->sval < SV_FOOD_BISCUIT)))
        {
            object_type forge;
            object_type *q_ptr = &forge;

            msg_print("The food falls through your jaws!");

            /* Create the item */
            object_prep(q_ptr, lookup_kind(o_ptr->tval, o_ptr->sval));

            /* Drop the object from heaven */
            (void)drop_near(q_ptr, -1, py, px);
        }
        else
        {
            msg_print("The food falls through your jaws and vanishes!");
        }
    }
    else if ((get_race_t()->flags & RACE_IS_NONLIVING) || prace_is_(RACE_ENT))
    {
        msg_print("The food of mortals is poor sustenance for you.");
        set_food(p_ptr->food + ((o_ptr->pval) / 20));
    }
    else if (o_ptr->tval == TV_FOOD && (o_ptr->sval == SV_FOOD_WAYBREAD || o_ptr->sval == SV_FOOD_AMBROSIA))
    {
        /* Waybread is always fully satisfying. */
        set_food(MAX(p_ptr->food, PY_FOOD_MAX - 1));
    }
    else
    {
        /* Food can feed the player */
        (void)set_food(p_ptr->food + o_ptr->pval);
    }

    /* Destroy a food in the pack */
    if (o_ptr->art_name) /* Hack: Artifact Food does not get destroyed! */
    {
        o_ptr->timeout += 99;
    }
    else
    {
        if (item >= 0)
        {
            inven_item_increase(item, -1);
            inven_item_describe(item);
            inven_item_optimize(item);
        }

        /* Destroy a food on the floor */
        else
        {
            floor_item_increase(0 - item, -1);
            floor_item_describe(0 - item);
            floor_item_optimize(0 - item);
        }
    }
}


/*
 * Hook to determine if an object is eatable
 */
static bool item_tester_hook_eatable(object_type *o_ptr)
{
    if (o_ptr->tval==TV_FOOD) return TRUE;

    if (prace_is_(RACE_SKELETON) ||
        prace_is_(RACE_GOLEM) ||
        prace_is_(RACE_MON_GOLEM) || 
        prace_is_(RACE_MON_SWORD) || 
        p_ptr->mimic_form == MIMIC_CLAY_GOLEM ||
        p_ptr->mimic_form == MIMIC_IRON_GOLEM ||
        p_ptr->mimic_form == MIMIC_MITHRIL_GOLEM ||
        p_ptr->mimic_form == MIMIC_COLOSSUS ||
        prace_is_(RACE_ZOMBIE) ||
        prace_is_(RACE_MON_LICH) ||
        prace_is_(RACE_SPECTRE))
    {
        if (o_ptr->tval == TV_STAFF || o_ptr->tval == TV_WAND)
            return TRUE;
    }
    else if (prace_is_(RACE_BALROG) || prace_is_(RACE_MON_DEMON) || p_ptr->mimic_form == MIMIC_DEMON || p_ptr->mimic_form == MIMIC_DEMON_LORD)
    {
        if (o_ptr->tval == TV_CORPSE &&
            o_ptr->sval == SV_CORPSE &&
            my_strchr("pht", r_info[o_ptr->pval].d_char))
            return TRUE;
    }
    else if (prace_is_(RACE_MON_JELLY))
        return TRUE;

    /* Assume not */
    return (FALSE);
}


/*
 * Eat some food (from the pack or floor)
 */
void do_cmd_eat_food(void)
{
    int         item;
    cptr        q, s;


    if (p_ptr->special_defense & (KATA_MUSOU | KATA_KOUKIJIN))
    {
        set_action(ACTION_NONE);
    }

    /* Restrict choices to food */
    item_tester_hook = item_tester_hook_eatable;

    /* Get an item */
    q = "Eat which item? ";
    s = "You have nothing to eat.";

    if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

    /* Eat the object */
    do_cmd_eat_food_aux(item);

    if (p_ptr->fasting)
    {
        msg_print("You break your fast.");
        p_ptr->redraw |= PR_STATUS;
        p_ptr->fasting = FALSE;
    }
}


/*
 * Quaff a potion (from the pack or the floor)
 */
static void do_cmd_quaff_potion_aux(int item)
{
    int         lev;
    object_type    *o_ptr;
    object_type forge;
    object_type *q_ptr;
    int         number = 1;


    /* Take a turn */
    if (mut_present(MUT_POTION_CHUGGER) || p_ptr->tim_shrike)
        energy_use = 50;
    else
        energy_use = 100;

    if (world_player)
    {
        if (flush_failure) flush();
        msg_print("The potion doesn't flow out from a bottle.");
        sound(SOUND_FAIL);
        return;
    }

    if (music_singing_any()) bard_stop_singing();
    if (hex_spelling_any())
    {
        if (!hex_spelling(HEX_INHAIL)) stop_hex_spell_all();
    }

    /* Get the item (in the pack) */
    if (item >= 0)
    {
        o_ptr = &inventory[item];
    }

    /* Get the item (on the floor) */
    else
    {
        o_ptr = &o_list[0 - item];
    }

    /* Object level */
    lev = k_info[o_ptr->k_idx].level;

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
        number = o_ptr->number;
        if (number > 4) number = 4;
        for (i = 1; i < number && amt; i++)
        {
            device_extra_power += amt;
            amt /= 2;
        }
    }

    /* Copy */
    q_ptr = &forge;
    object_copy(q_ptr, o_ptr);
    q_ptr->number = number;

    /* Consume Item */
    if (devicemaster_is_(DEVICEMASTER_POTIONS) && !devicemaster_desperation && randint1(3*p_ptr->lev/2) > MAX(10, lev))
    {
        msg_print("You sip the potion sparingly.");
    }
    else
    {
        stats_on_use(q_ptr, number);

        if (item >= 0)
        {
            inven_item_increase(item, -number);
            inven_item_describe(item);
            inven_item_optimize(item);
        }
        else
        {
            floor_item_increase(0 - item, -number);
            floor_item_describe(0 - item);
            floor_item_optimize(0 - item);
        }
    }
    o_ptr = NULL; /* Crappy code warning: Use q_ptr from here on ... */

    sound(SOUND_QUAFF);
    if (q_ptr->tval == TV_POTION)
        device_use(q_ptr);

    if (prace_is_(RACE_SKELETON) || (p_ptr->current_r_idx && r_info[p_ptr->current_r_idx].d_char == 's'))
    {
        msg_print("Some of the fluid falls through your jaws!");
        potion_smash_effect(0, py, px, q_ptr->k_idx);
    }

    /* Combine / Reorder the pack (later) */
    p_ptr->notice |= (PN_COMBINE | PN_REORDER);

    if (!(object_is_aware(q_ptr)))
    {
        virtue_add(VIRTUE_PATIENCE, -1);
        virtue_add(VIRTUE_CHANCE, 1);
        virtue_add(VIRTUE_KNOWLEDGE, -1);
    }

    object_tried(q_ptr);
    if (device_noticed && !object_is_aware(q_ptr))
    {
        object_aware(q_ptr);
        gain_exp((lev + (p_ptr->lev >> 1)) / p_ptr->lev);

        /* Try to keep stats up to date. It's possible inventory[item]
           is no longer the item we just noticed and I'm rather frustrated
           that the code consumes the objects before actually using them. */
        if (item >= 0)
            o_ptr = &inventory[item];
        else
            o_ptr = &o_list[0 - item];
        if (o_ptr->k_idx == q_ptr->k_idx)
            stats_on_notice(o_ptr, o_ptr->number + number);
    }

    /* Window stuff */
    p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);

    /* Potions can feed the player */
    switch (p_ptr->mimic_form)
    {
    case MIMIC_NONE:
        switch (p_ptr->prace)
        {
            case RACE_VAMPIRE:
            case RACE_MON_VAMPIRE:
                (void)set_food(p_ptr->food + (q_ptr->pval / 10));
                break;
            case RACE_SKELETON:
            case RACE_MON_JELLY:
                /* Do nothing */
                break;
            case RACE_GOLEM:
            case RACE_MON_GOLEM:
            case RACE_ZOMBIE:
            case RACE_MON_LICH:
            case RACE_BALROG:
            case RACE_SPECTRE:
            case RACE_MON_DEMON:
            case RACE_MON_SWORD:
                set_food(p_ptr->food + ((q_ptr->pval) / 20));
                break;
            case RACE_ANDROID:
                if (q_ptr->tval == TV_FLASK)
                {
                    msg_print("You replenish yourself with the oil.");
                    set_food(p_ptr->food + 5000);
                }
                else
                {
                    set_food(p_ptr->food + ((q_ptr->pval) / 20));
                }
                break;
            case RACE_ENT:
                msg_print("You are moistened.");
                set_food(MIN(p_ptr->food + q_ptr->pval + MAX(0, q_ptr->pval * 10) + 2000, PY_FOOD_MAX - 1));
                break;
            default:
                (void)set_food(p_ptr->food + q_ptr->pval);
                break;
        }
        break;
    case MIMIC_DEMON:
    case MIMIC_DEMON_LORD:
        set_food(p_ptr->food + ((q_ptr->pval) / 20));
        break;
    case MIMIC_VAMPIRE:
        (void)set_food(p_ptr->food + (q_ptr->pval / 10));
        break;
    default:
        (void)set_food(p_ptr->food + q_ptr->pval);
        break;
    }
}


/*
 * Hook to determine if an object can be quaffed
 */
static bool item_tester_hook_quaff(object_type *o_ptr)
{
    if (o_ptr->tval == TV_POTION) return TRUE;

    if (prace_is_(RACE_ANDROID))
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
    int  item;
    cptr q, s;

    if (p_ptr->special_defense & (KATA_MUSOU | KATA_KOUKIJIN))
    {
        set_action(ACTION_NONE);
    }

    /* Restrict choices to potions */
    item_tester_hook = item_tester_hook_quaff;

    /* Get an item */
    q = "Quaff which potion? ";
    s = "You have no potions to quaff.";

    if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

    /* Quaff the potion */
    do_cmd_quaff_potion_aux(item);

    if (p_ptr->fasting)
    {
        msg_print("You break your fast.");
        p_ptr->redraw |= PR_STATUS;
        p_ptr->fasting = FALSE;
    }
}


/*
 * Read a scroll (from the pack or floor).
 *
 * Certain scrolls can be "aborted" without losing the scroll.  These
 * include scrolls with no effects but recharge or identify, which are
 * cancelled before use.  XXX Reading them still takes a turn, though.
 */
static void do_cmd_read_scroll_aux(int item, bool known)
{
    int         used_up, lev;
    object_type *o_ptr;
    int         number = 1;


    /* Get the item (in the pack) */
    if (item >= 0)
    {
        o_ptr = &inventory[item];
    }

    /* Get the item (on the floor) */
    else
    {
        o_ptr = &o_list[0 - item];
    }

    /* Object level */
    lev = k_info[o_ptr->k_idx].level;

    /* Take a turn */
    if (mut_present(MUT_SPEED_READER) || p_ptr->tim_shrike)
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
        msg_print("Nothing happen.");
        sound(SOUND_FAIL);
        return;
    }

    if (p_ptr->pclass == CLASS_BERSERKER || (get_race_t()->flags & RACE_IS_ILLITERATE))
    {
        msg_print("You cannot read.");
        return;
    }

    if (music_singing_any()) bard_stop_singing();

    /* Hex */
    if (hex_spelling_any() && ((p_ptr->lev < 35) || hex_spell_fully())) stop_hex_spell_all();

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
        used_up = device_use(o_ptr);
        if (object_is_(o_ptr, TV_SCROLL, SV_SCROLL_IDENTIFY))
            number = device_used_charges;
    }
    else if (o_ptr->name1 == ART_GHB)
    {
        msg_print("I had a very hard time to kill the Greater hell-beast, ");
        msg_print("but all I got was this lousy t-shirt!");
        used_up = FALSE;
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

    if (!(object_is_aware(o_ptr)))
    {
        virtue_add(VIRTUE_PATIENCE, -1);
        virtue_add(VIRTUE_CHANCE, 1);
        virtue_add(VIRTUE_KNOWLEDGE, -1);
    }

    object_tried(o_ptr);
    if (device_noticed && !object_is_aware(o_ptr))
    {
        object_aware(o_ptr);
        stats_on_notice(o_ptr, o_ptr->number);
        gain_exp((lev + (p_ptr->lev >> 1)) / p_ptr->lev);
    }

    p_ptr->notice |= PN_COMBINE | PN_REORDER;
    p_ptr->window |= PW_INVEN | PW_EQUIP | PW_PLAYER;

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
        if (item >= 0)
        {
            inven_item_increase(item, -number);
            inven_item_describe(item);
            inven_item_optimize(item);
        }
        else
        {
            floor_item_increase(0 - item, -number);
            floor_item_describe(0 - item);
            floor_item_optimize(0 - item);
        }
    }
}


/*
 * Hook to determine if an object is readable
 */
static bool item_tester_hook_readable(object_type *o_ptr)
{
    if ((o_ptr->tval==TV_SCROLL) || (o_ptr->tval==TV_PARCHMENT) || (o_ptr->name1 == ART_GHB) || (o_ptr->name1 == ART_POWER)) return (TRUE);

    /* Assume not */
    return (FALSE);
}


void do_cmd_read_scroll(void)
{
    object_type *o_ptr;
    int  item;
    cptr q, s;

    if (p_ptr->special_defense & (KATA_MUSOU | KATA_KOUKIJIN))
    {
        set_action(ACTION_NONE);
    }

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


    /* Restrict choices to scrolls */
    item_tester_hook = item_tester_hook_readable;

    /* Get an item */
    q = "Read which scroll? ";
    s = "You have no scrolls to read.";
    if (!get_item(&item, q, s, USE_INVEN | USE_FLOOR | SHOW_FAIL_RATES)) return;

    /* Get the item (in the pack) */
    if (item >= 0)
    {
        o_ptr = &inventory[item];
    }

    /* Get the item (on the floor) */
    else
    {
        o_ptr = &o_list[0 - item];
    }

    /* Read the scroll */
    do_cmd_read_scroll_aux(item, object_is_aware(o_ptr));
}


/*
 * Use a staff.            -RAK-
 *
 * One charge of one staff disappears.
 *
 * Hack -- staffs of identify can be "cancelled".
 */
static void do_cmd_use_staff_aux(int item)
{
    object_type *o_ptr;
    bool         used = FALSE;
    int          charges = 1;

    if (item >= 0)
        o_ptr = &inventory[item];
    else
    {
        o_ptr = &o_list[0 - item];
        if (o_ptr->number > 1)
        {
            msg_print("You must first pick up the staffs.");
            return;
        }
    }

    energy_use = 100;

    if (devicemaster_is_(DEVICEMASTER_STAVES) && !devicemaster_desperation)
    {
        int delta = MIN(50, 2*p_ptr->lev - k_info[o_ptr->k_idx].level);
        if (delta > 0)
            energy_use -= delta;
    }

    if (p_ptr->tim_no_device)
    {
        msg_print("An evil power blocks your magic!");
        return;
    }
    if (!(devicemaster_is_(DEVICEMASTER_STAVES) || fear_allow_device()))
    {
        msg_print("You are too scared!");
        return;
    }
    if (world_player)
    {
        if (flush_failure) flush();
        msg_print("Nothing happen. Maybe this staff is freezing too.");
        sound(SOUND_FAIL);
        return;
    }

    if (!device_try(o_ptr))
    {
        if (flush_failure) flush();
        msg_print("You failed to use the staff properly.");
        sound(SOUND_FAIL);
        return;
    }

    if (o_ptr->pval <= 0)
    {
        if (flush_failure) flush();
        msg_print("The staff has no charges left.");
        o_ptr->ident |= (IDENT_EMPTY);
        p_ptr->notice |= (PN_COMBINE | PN_REORDER);
        p_ptr->window |= (PW_INVEN);
        return;
    }

    if (devicemaster_desperation)
    {
        int i, amt = 50;
        charges = o_ptr->pval;
        for (i = 1; i < charges && amt; i++)
        {
            device_extra_power += amt;
            amt /= 2;
        }
    }
    
    if (object_is_(o_ptr, TV_STAFF, SV_STAFF_IDENTIFY))
        device_available_charges = o_ptr->number * o_ptr->pval;

    sound(SOUND_ZAP);
    used = device_use(o_ptr);

    if (!(object_is_aware(o_ptr)))
    {
        virtue_add(VIRTUE_PATIENCE, -1);
        virtue_add(VIRTUE_CHANCE, 1);
        virtue_add(VIRTUE_KNOWLEDGE, -1);
    }

    p_ptr->notice |= (PN_COMBINE | PN_REORDER);
    object_tried(o_ptr);
    if (device_noticed && !object_is_aware(o_ptr))
    {
        object_aware(o_ptr);
        stats_on_notice(o_ptr, o_ptr->number);
        gain_exp((k_info[o_ptr->k_idx].level + (p_ptr->lev >> 1)) / p_ptr->lev);
    }

    p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);
    if (!used) 
    {
        energy_use = 0;
        return;
    }

    if (object_is_(o_ptr, TV_STAFF, SV_STAFF_IDENTIFY))
        charges = device_used_charges;

    if (devicemaster_is_(DEVICEMASTER_STAVES) && !devicemaster_desperation && randint1(100) <= p_ptr->lev)
    {
        msg_print("Your mental focus powers the staff!");
    }
    else
    {
        stats_on_use(o_ptr, charges);
        if (devicemaster_desperation && randint0(p_ptr->lev*7) < k_info[o_ptr->k_idx].level)
        {
            char o_name[MAX_NLEN];
            object_desc(o_name, o_ptr, (OD_OMIT_PREFIX | OD_NAME_ONLY));
            if (o_ptr->number > 1)
                msg_format("Desperation magic consumes one of your %s!", o_name);
            else
                msg_format("Desperation magic consumes your %s!", o_name);
            if (item >= 0)
            {
                inven_item_increase(item, -1);
                inven_item_describe(item);
                inven_item_optimize(item);
            }
            else
            {
                floor_item_increase(0 - item, -1);
                floor_item_describe(0 - item);
                floor_item_optimize(0 - item);
            }
        }
        else if ((item >= 0) && (o_ptr->number > 1))
        {
            int overflow = charges % o_ptr->number;

            /* Draw down total stack uniformly */
            o_ptr->pval -= charges / o_ptr->number;

            /* Overflow the rest */
            if (overflow)
            {
                object_type copy;
                object_copy(&copy, o_ptr);
                copy.number = overflow;
                copy.pval--;
                o_ptr->number -= overflow;
                p_ptr->total_weight -= copy.number * copy.weight;
                item = inven_carry(&copy);
                msg_print("You unstack your staff.");
            }
        }
        else
            o_ptr->pval -= charges;

        if (item >= 0)
            inven_item_charges(item);
        else
            floor_item_charges(0 - item);
    }
}


void do_cmd_use_staff(void)
{
    int  item;
    cptr q, s;

    if (p_ptr->special_defense & (KATA_MUSOU | KATA_KOUKIJIN))
    {
        set_action(ACTION_NONE);
    }

    if (p_ptr->pclass == CLASS_MAGIC_EATER && !_pack_find_tval(TV_STAFF))
    {
        magic_eater_cast(TV_STAFF);
        return;
    }

    /* Restrict choices to wands */
    item_tester_tval = TV_STAFF;

    /* Get an item */
    q = "Use which staff? ";
    s = "You have no staff to use.";
    if (!get_item(&item, q, s, USE_INVEN | USE_FLOOR | SHOW_FAIL_RATES)) return;

    do_cmd_use_staff_aux(item);
}


/*
 * Aim a wand (from the pack or floor).
 *
 * Use a single charge from a single item.
 * Handle "unstacking" in a logical manner.
 *
 * For simplicity, you cannot use a stack of items from the
 * ground.  This would require too much nasty code.
 *
 * There are no wands which can "destroy" themselves, in the inventory
 * or on the ground, so we can ignore this possibility.  Note that this
 * required giving "wand of wonder" the ability to ignore destruction
 * by electric balls.
 *
 * All wands can be "cancelled" at the "Direction?" prompt for free.
 *
 * Note that the basic "bolt" wands do slightly less damage than the
 * basic "bolt" rods, but the basic "ball" wands do the same damage
 * as the basic "ball" rods.
 */
static void do_cmd_aim_wand_aux(int item)
{
    object_type *o_ptr;
    bool         used = FALSE;
    int          charges = 1;

    if (item >= 0)
        o_ptr = &inventory[item];
    else
    {
        o_ptr = &o_list[0 - item];
        if (o_ptr->number > 1)
        {
            msg_print("You must first pick up the wands.");
            return;
        }
    }

    energy_use = 100;

    if (devicemaster_is_(DEVICEMASTER_WANDS) && !devicemaster_desperation)
    {
        int delta = MIN(50, 2*p_ptr->lev - k_info[o_ptr->k_idx].level);
        if (delta > 0)
            energy_use -= delta;
    }

    if (p_ptr->tim_no_device)
    {
        msg_print("An evil power blocks your magic!");
        return;
    }
    if (!(devicemaster_is_(DEVICEMASTER_WANDS) || fear_allow_device()))
    {
        msg_print("You are too scared!");
        return;
    }
    if (world_player)
    {
        if (flush_failure) flush();
        msg_print("Nothing happen. Maybe this wand is freezing too.");
        sound(SOUND_FAIL);
        return;
    }

    if (!device_try(o_ptr))
    {
        if (flush_failure) flush();
        msg_print("You failed to use the wand properly.");
        sound(SOUND_FAIL);
        return;
    }

    if (o_ptr->pval <= 0)
    {
        if (flush_failure) flush();
        msg_print("The wand has no charges left.");
        o_ptr->ident |= (IDENT_EMPTY);
        p_ptr->notice |= (PN_COMBINE | PN_REORDER);
        p_ptr->window |= (PW_INVEN);
        return;
    }

    if (devicemaster_desperation)
    {
        int i, amt = 50, num = o_ptr->number;
        charges = (o_ptr->pval + num - 1)/num;
        for (i = 1; i < charges && amt; i++)
        {
            device_extra_power += amt;
            amt /= 2;
        }
    }

    sound(SOUND_ZAP);
    used = device_use(o_ptr);

    p_ptr->notice |= (PN_COMBINE | PN_REORDER);
    if (!(object_is_aware(o_ptr)))
    {
        virtue_add(VIRTUE_PATIENCE, -1);
        virtue_add(VIRTUE_CHANCE, 1);
        virtue_add(VIRTUE_KNOWLEDGE, -1);
    }
    object_tried(o_ptr);
    if (device_noticed && !object_is_aware(o_ptr))
    {
        object_aware(o_ptr);
        stats_on_notice(o_ptr, o_ptr->number);
        gain_exp((k_info[o_ptr->k_idx].level + (p_ptr->lev >> 1)) / p_ptr->lev);
    }
    p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);

    if (used)
    {
        if (devicemaster_is_(DEVICEMASTER_WANDS) && !devicemaster_desperation && randint1(100) <= p_ptr->lev)
        {
            msg_print("Your mental focus powers the wand!");
        }
        else
        {
            stats_on_use(o_ptr, charges);
            if (devicemaster_desperation && randint0(p_ptr->lev*7) < k_info[o_ptr->k_idx].level)
            {
                char o_name[MAX_NLEN];
                object_desc(o_name, o_ptr, (OD_OMIT_PREFIX | OD_NAME_ONLY));
                if (o_ptr->number > 1)
                    msg_format("Desperation magic consumes one of your %s!", o_name);
                else
                    msg_format("Desperation magic consumes your %s!", o_name);
                if (item >= 0)
                {
                    inven_item_increase(item, -1);
                    inven_item_describe(item);
                    inven_item_optimize(item);
                }
                else
                {
                    floor_item_increase(0 - item, -1);
                    floor_item_describe(0 - item);
                    floor_item_optimize(0 - item);
                }
            }
            o_ptr->pval -= charges;
            if (item >= 0)
                inven_item_charges(item);
            else
                floor_item_charges(0 - item);
        }
    }
    else
        energy_use = 0;
}


void do_cmd_aim_wand(void)
{
    int     item;
    cptr    q, s;

    if (p_ptr->special_defense & (KATA_MUSOU | KATA_KOUKIJIN))
    {
        set_action(ACTION_NONE);
    }

    if (p_ptr->pclass == CLASS_MAGIC_EATER && !_pack_find_tval(TV_WAND))
    {
        magic_eater_cast(TV_WAND);
        return;
    }

    /* Restrict choices to wands */
    item_tester_tval = TV_WAND;

    /* Get an item */
    q = "Aim which wand? ";
    s = "You have no wand to aim.";

    if (!get_item(&item, q, s, USE_INVEN | USE_FLOOR | SHOW_FAIL_RATES)) return;

    /* Aim the wand */
    do_cmd_aim_wand_aux(item);
}

/*
 * Activate (zap) a Rod
 *
 * Unstack fully charged rods as needed.
 *
 * Hack -- rods of perception/genocide can be "cancelled"
 * All rods can be cancelled at the "Direction?" prompt
 *
 * pvals are defined for each rod in k_info. -LM-
 */
static void do_cmd_zap_rod_aux(int item)
{
    object_type *o_ptr;
    bool         used = FALSE;
    object_kind *k_ptr;
    int          charges = 1;

    if (item >= 0)
        o_ptr = &inventory[item];
    else
    {
        o_ptr = &o_list[0 - item];
        if (o_ptr->number > 1)
        {
            msg_print("You must first pick up the rods.");
            return;
        }
    }

    energy_use = 100;

    if (devicemaster_is_(DEVICEMASTER_RODS) && !devicemaster_desperation)
    {
        int delta = MIN(50, 2*p_ptr->lev - k_info[o_ptr->k_idx].level);
        if (delta > 0)
            energy_use -= delta;
    }

    if (p_ptr->tim_no_device)
    {
        msg_print("An evil power blocks your magic!");
        return;
    }
    if (!(devicemaster_is_(DEVICEMASTER_RODS) || fear_allow_device()))
    {
        msg_print("You are too scared!");
        return;
    }
    if (world_player)
    {
        if (flush_failure) flush();
        msg_print("Nothing happen. Maybe this rod is freezing too.");
        sound(SOUND_FAIL);
        return;
    }

    if (!device_try(o_ptr))
    {
        if (flush_failure) flush();
        msg_print("You failed to use the rod properly.");
        sound(SOUND_FAIL);
        return;
    }

    k_ptr = &k_info[o_ptr->k_idx];

    /* A single rod is still charging */
    if ((o_ptr->number == 1) && (o_ptr->timeout))
    {
        if (flush_failure) flush();
        msg_print("The rod is still charging.");
        return;
    }
    /* A stack of rods lacks enough energy. */
    else if ((o_ptr->number > 1) && (o_ptr->timeout > k_ptr->pval * (o_ptr->number - 1)))
    {
        if (flush_failure) flush();
        msg_print("The rods are all still charging.");
        return;
    }

    if (devicemaster_desperation)
    {
        int i, amt = 50;
        charges = o_ptr->number - (o_ptr->timeout + k_ptr->pval - 1)  / k_ptr->pval;
        if (charges > 3) charges = 3;
        for (i = 1; i < charges && amt; i++)
        {
            device_extra_power += amt;
            amt /= 2;
        }
    }

    sound(SOUND_ZAP);
    used = device_use(o_ptr);

    /* Increase the timeout by the rod kind's pval. -LM- */
    if (used) 
    {
        if (devicemaster_is_(DEVICEMASTER_RODS) && !devicemaster_desperation && randint1(100) <= p_ptr->lev)
        {
            msg_print("Your mental focus powers the rod!");
        }
        else
        {
            stats_on_use(o_ptr, charges);
            o_ptr->timeout += k_ptr->pval * charges;
            if (devicemaster_desperation && randint0(p_ptr->lev*11) < k_info[o_ptr->k_idx].level)
            {
                char o_name[MAX_NLEN];
                object_desc(o_name, o_ptr, (OD_OMIT_PREFIX | OD_NAME_ONLY));
                if (o_ptr->number > 1)
                    msg_format("Desperation magic consumes one of your %s!", o_name);
                else
                    msg_format("Desperation magic consumes your %s!", o_name);
                if (item >= 0)
                {
                    inven_item_increase(item, -1);
                    inven_item_describe(item);
                    inven_item_optimize(item);
                }
                else
                {
                    floor_item_increase(0 - item, -1);
                    floor_item_describe(0 - item);
                    floor_item_optimize(0 - item);
                }
            }
        }
    }
    else
        energy_use = 0;

    p_ptr->notice |= (PN_COMBINE | PN_REORDER);
    if (!(object_is_aware(o_ptr)))
    {
        virtue_add(VIRTUE_PATIENCE, -1);
        virtue_add(VIRTUE_CHANCE, 1);
        virtue_add(VIRTUE_KNOWLEDGE, -1);
    }
    object_tried(o_ptr);

    if (device_noticed && !object_is_aware(o_ptr))
    {
        object_aware(o_ptr);
        stats_on_notice(o_ptr, o_ptr->number);
        gain_exp((k_info[o_ptr->k_idx].level + (p_ptr->lev >> 1)) / p_ptr->lev);
    }

    p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);
}

void do_cmd_zap_rod(void)
{
    int item;
    cptr q, s;

    if (p_ptr->special_defense & (KATA_MUSOU | KATA_KOUKIJIN))
    {
        set_action(ACTION_NONE);
    }

    if (p_ptr->pclass == CLASS_MAGIC_EATER && !_pack_find_tval(TV_ROD))
    {
        magic_eater_cast(TV_ROD);
        return;
    }

    /* Restrict choices to rods */
    item_tester_tval = TV_ROD;

    /* Get an item */
    q = "Zap which rod? ";
    s = "You have no rod to zap.";

    if (!get_item(&item, q, s, USE_INVEN | USE_FLOOR | SHOW_FAIL_RATES)) return;

    /* Zap the rod */
    do_cmd_zap_rod_aux(item);
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
            msg_print("Oops.  You failed to release your pet.");
    }
}

/*
 * Activate a wielded object.  Wielded objects never stack.
 * And even if they did, activatable objects never stack.
 *
 * Currently, only (some) artifacts, and Dragon Scale Mail, can be activated.
 * But one could, for example, easily make an activatable "Ring of Plasma".
 *
 * Note that it always takes a turn to activate an artifact, even if
 * the user hits "escape" at the "direction" prompt.
 */
static void do_cmd_activate_aux(int item)
{
    object_type *o_ptr;
    effect_t     effect;
    cptr         msg;
    int          boost = device_power(100) - 100;

    /* Get the item (in the pack) */
    if (item >= 0)
    {
        o_ptr = &inventory[item];
    }

    /* Get the item (on the floor) */
    else
    {
        o_ptr = &o_list[0 - item];
    }

    /* Take a turn */
    energy_use = 100;

    if (world_player)
    {
        if (flush_failure) flush();
        msg_print("It shows no reaction.");
        sound(SOUND_FAIL);
        return;
    }

    effect = obj_get_effect(o_ptr);
    if (!effect_try(&effect))
    {
        if (flush_failure) flush();
        msg_print("You failed to activate it properly.");
        sound(SOUND_FAIL);
        return;
    }

    if (o_ptr->timeout)
    {
        msg_print("It whines, glows and fades...");
        return;
    }

    msg_print("You activate it...");
    sound(SOUND_ZAP);

    msg = obj_get_effect_msg(o_ptr);
    if (msg)
        msg_print(msg);

    if (o_ptr->tval == TV_CAPTURE)
    {
        _do_capture_ball(o_ptr);
        return;
    }
    
    if (effect_use(&effect, boost))
    {
        o_ptr->timeout = effect.timeout;
        p_ptr->window |= (PW_INVEN | PW_EQUIP);
    }
}

void do_cmd_activate(void)
{
    int     item;
    cptr    q, s;


    if (p_ptr->special_defense & (KATA_MUSOU | KATA_KOUKIJIN))
    {
        set_action(ACTION_NONE);
    }

    item_tester_no_ryoute = TRUE;
    /* Prepare the hook */
    /*item_tester_hook = item_tester_hook_activate;*/
    item_tester_hook = obj_has_effect;

    /* Get an item */
    q = "Activate which item? ";
    s = "You have nothing to activate.";
    if (!get_item(&item, q, s, USE_EQUIP | SHOW_FAIL_RATES)) return;

    /* Activate the item */
    do_cmd_activate_aux(item);
}


/*
 * Hook to determine if an object is useable
 */
static bool item_tester_hook_use(object_type *o_ptr)
{
    /* Ammo */
    if (o_ptr->tval == p_ptr->shooter_info.tval_ammo)
        return (TRUE);

    /* Useable object */
    switch (o_ptr->tval)
    {
        case TV_SPIKE:
        case TV_STAFF:
        case TV_WAND:
        case TV_ROD:
        case TV_SCROLL:
        case TV_POTION:
        case TV_FOOD:
        {
            return (TRUE);
        }

        default:
        {
            if (!object_is_known(o_ptr)) return FALSE;    
            if (!equip_is_worn(o_ptr)) return FALSE;
            if (obj_has_effect(o_ptr)) return TRUE;
        }
    }

    /* Assume not */
    return (FALSE);
}


/*
 * Use an item
 * XXX - Add actions for other item types
 */
void do_cmd_use(void)
{
    int         item;
    object_type *o_ptr;
    cptr        q, s;

    if (p_ptr->special_defense & (KATA_MUSOU | KATA_KOUKIJIN))
    {
        set_action(ACTION_NONE);
    }

    item_tester_no_ryoute = TRUE;
    /* Prepare the hook */
    item_tester_hook = item_tester_hook_use;

    /* Get an item */
    q = "Use which item? ";
    s = "You have nothing to use.";

    if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP | USE_FLOOR))) return;

    /* Get the item (in the pack) */
    if (item >= 0)
    {
        o_ptr = &inventory[item];
    }
    /* Get the item (on the floor) */
    else
    {
        o_ptr = &o_list[0 - item];
    }

    switch (o_ptr->tval)
    {
        /* Spike a door */
        case TV_SPIKE:
        {
            do_cmd_spike();
            break;
        }

        /* Eat some food */
        case TV_FOOD:
        {
            do_cmd_eat_food_aux(item);
            break;
        }

        /* Aim a wand */
        case TV_WAND:
        {
            do_cmd_aim_wand_aux(item);
            break;
        }

        /* Use a staff */
        case TV_STAFF:
        {
            do_cmd_use_staff_aux(item);
            break;
        }

        /* Zap a rod */
        case TV_ROD:
        {
            do_cmd_zap_rod_aux(item);
            break;
        }

        /* Quaff a potion */
        case TV_POTION:
        {
            do_cmd_quaff_potion_aux(item);
            break;
        }

        /* Read a scroll */
        case TV_SCROLL:
        {
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

          do_cmd_read_scroll_aux(item, TRUE);
          break;
        }

        /* Fire ammo */
        case TV_SHOT:
        case TV_ARROW:
        case TV_BOLT:
        {
            int slot = equip_find_object(TV_BOW, SV_ANY);
            if (slot)
                do_cmd_fire_aux1(item, equip_obj(slot));
            else
                msg_print("You need a bow to use that!");
            break;
        }

        /* Activate an artifact */
        default:
        {
            do_cmd_activate_aux(item);
            break;
        }
    }
}

