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

static int _device_power_hack(int pow, bool magic)
{
    if (magic) return spell_power(pow);
    return device_power(pow);
}

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
                    if (set_paralyzed(p_ptr->paralyzed + randint0(10) + 10, FALSE))
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

    /* The player is now aware of the object */
    if (ident && !object_is_aware(o_ptr))
    {
        object_aware(o_ptr);
        gain_exp((lev + (p_ptr->lev >> 1)) / p_ptr->lev);
    }

    /* Window stuff */
    p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);


    /* Food can feed the player */
    if (prace_is_(RACE_VAMPIRE) || (p_ptr->mimic_form == MIMIC_VAMPIRE))
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
          prace_is_(MIMIC_CLAY_GOLEM) ||
          prace_is_(MIMIC_IRON_GOLEM) ||
          prace_is_(MIMIC_MITHRIL_GOLEM) ||
          prace_is_(MIMIC_COLOSSUS) ||
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
        prace_is_(MIMIC_CLAY_GOLEM) ||
        prace_is_(MIMIC_IRON_GOLEM) ||
        prace_is_(MIMIC_MITHRIL_GOLEM) ||
        prace_is_(MIMIC_COLOSSUS) ||
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

    if (prace_is_(RACE_SKELETON))
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
                (void)set_food(p_ptr->food + (q_ptr->pval / 10));
                break;
            case RACE_SKELETON:
            case RACE_MON_JELLY:
                /* Do nothing */
                break;
            case RACE_GOLEM:
            case RACE_ZOMBIE:
            case RACE_MON_LICH:
            case RACE_BALROG:
            case RACE_SPECTRE:
            case RACE_MON_DEMON:
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
        gain_exp((k_info[o_ptr->k_idx].level + (p_ptr->lev >> 1)) / p_ptr->lev);
    }

    p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);
    if (!used) 
    {
        energy_use = 0;
        return;
    }

    if (devicemaster_is_(DEVICEMASTER_STAVES) && !devicemaster_desperation && randint1(100) <= p_ptr->lev)
    {
        msg_print("Your mental focus powers the staff!");
    }
    else
    {
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
            object_type forge;
            object_type *q_ptr;

            q_ptr = &forge;
            object_copy(q_ptr, o_ptr);
            q_ptr->number = 1;
            q_ptr->pval = o_ptr->pval - charges;
            o_ptr->number--;
            p_ptr->total_weight -= q_ptr->weight;
            item = inven_carry(q_ptr);
            msg_print("You unstack your staff.");
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
 * Hook to determine if an object is activatable
 */
static bool item_tester_hook_activate(object_type *o_ptr)
{
    u32b flgs[TR_FLAG_SIZE];

    /* Not known */
    if (!object_is_known(o_ptr)) return (FALSE);

    /* Extract the flags */
    object_flags(o_ptr, flgs);

    /* Check activation flag */
    if (have_flag(flgs, TR_ACTIVATE)) return (TRUE);

    /* Assume not */
    return (FALSE);
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


static bool ang_sort_comp_pet(vptr u, vptr v, int a, int b)
{
    u16b *who = (u16b*)(u);

    int w1 = who[a];
    int w2 = who[b];

    monster_type *m_ptr1 = &m_list[w1];
    monster_type *m_ptr2 = &m_list[w2];
    monster_race *r_ptr1 = &r_info[m_ptr1->r_idx];
    monster_race *r_ptr2 = &r_info[m_ptr2->r_idx];

    /* Unused */
    (void)v;

    if (m_ptr1->nickname && !m_ptr2->nickname) return TRUE;
    if (m_ptr2->nickname && !m_ptr1->nickname) return FALSE;

    if ((r_ptr1->flags1 & RF1_UNIQUE) && !(r_ptr2->flags1 & RF1_UNIQUE)) return TRUE;
    if ((r_ptr2->flags1 & RF1_UNIQUE) && !(r_ptr1->flags1 & RF1_UNIQUE)) return FALSE;

    if (r_ptr1->level > r_ptr2->level) return TRUE;
    if (r_ptr2->level > r_ptr1->level) return FALSE;

    if (m_ptr1->hp > m_ptr2->hp) return TRUE;
    if (m_ptr2->hp > m_ptr1->hp) return FALSE;
    
    return w1 <= w2;
}

static int _activation_level(object_type *o_ptr)
{
    int lev = k_info[o_ptr->k_idx].level;

    if (object_is_fixed_artifact(o_ptr))
    {
        /* Hack -- Blood Knights love Bloodrip!! */
        if (o_ptr->name1 == ART_BLOODRIP && p_ptr->pclass == CLASS_BLOOD_KNIGHT)
            lev = 5; /* vs 66! */
        else
            lev = a_info[o_ptr->name1].level;
    }
    else if (o_ptr->art_name)
    {
        switch (o_ptr->xtra2)
        {
            case ACT_SUNLIGHT:
            case ACT_BO_MISS_1:
            case ACT_BA_POIS_1:
            case ACT_CONFUSE:
            case ACT_SLEEP:
            case ACT_CURE_LW:
            case ACT_CURE_POISON:
            case ACT_BERSERK:
            case ACT_LIGHT:
            case ACT_DEST_DOOR:
            case ACT_TELEPORT:
                lev = 10;
                break;
            case ACT_BO_ELEC_1:
            case ACT_BO_ACID_1:
            case ACT_BO_COLD_1:
            case ACT_BO_FIRE_1:
            case ACT_MAP_LIGHT:
            case ACT_STONE_MUD:
            case ACT_CURE_MW:
            case ACT_QUAKE:
                lev = 20;
                break;
            case ACT_DRAIN_1:
            case ACT_TELE_AWAY:
            case ACT_ESP:
            case ACT_RESIST_ALL:
            case ACT_DETECT_ALL:
            case ACT_RECALL:
            case ACT_SATIATE:
            case ACT_RECHARGE:
                lev = 30;
                break;
            case ACT_BA_COLD_1:
            case ACT_BA_FIRE_1:
            case ACT_TERROR:
            case ACT_PROT_EVIL:
            case ACT_ID_PLAIN:
            case ACT_REST_LIFE:
            case ACT_SPEED:
            case ACT_BANISH_EVIL:
                lev = 40;
                break;
            case ACT_DRAIN_2:
            case ACT_VAMPIRE_1:
            case ACT_BO_MISS_2:
            case ACT_BA_FIRE_2:
            case ACT_WHIRLWIND:
            case ACT_CHARM_ANIMAL:
            case ACT_SUMMON_ANIMAL:
            case ACT_DISP_EVIL:
            case ACT_DISP_GOOD:
            case ACT_XTRA_SPEED:
            case ACT_DETECT_XTRA:
            case ACT_ID_FULL:
                lev = 50;
                break;
            case ACT_VAMPIRE_2:
            case ACT_BA_COLD_3:
            case ACT_BA_ELEC_3:
            case ACT_GENOCIDE:
            case ACT_CHARM_UNDEAD:
            case ACT_CHARM_OTHER:
            case ACT_SUMMON_PHANTOM:
            case ACT_SUMMON_ELEMENTAL:
            case ACT_RUNE_EXPLO:
                lev = 60;
                break;
            case ACT_MASS_GENO:
            case ACT_CHARM_ANIMALS:
            case ACT_CHARM_OTHERS:
            case ACT_CURE_700:
            case ACT_RUNE_PROT:
            case ACT_ALCHEMY:
            case ACT_REST_ALL:
                lev = 70;
                break;
            case ACT_CALL_CHAOS:
            case ACT_ROCKET:
            case ACT_BA_MISS_3:
            case ACT_CURE_1000:
            case ACT_DIM_DOOR:
            case ACT_SUMMON_UNDEAD:
            case ACT_SUMMON_DEMON:
                lev = 80;
                break;
            case ACT_WRAITH:
            case ACT_INVULN:
                lev = 100;
                break;
            default:
                lev = 0;
        }
    }
    else if (((o_ptr->tval == TV_RING) || (o_ptr->tval == TV_AMULET)) && o_ptr->name2) lev = e_info[o_ptr->name2].level;

    return lev;
}

/* Scaled by 10 so that, for example, 83.4% failure returns 834 */
int activation_fail_rate(object_type *o_ptr)
{
    int chance, fail;
    int lev = _activation_level(o_ptr);

    if (p_ptr->pclass == CLASS_BERSERKER) return 1000;

    chance = p_ptr->skills.dev;
    if (p_ptr->confused) chance = chance / 2;

    fail = lev+5;
    if (chance > fail) fail -= (chance - fail)*2;
    else chance -= (fail - chance)*2;
    if (fail < USE_DEVICE) fail = USE_DEVICE;
    if (chance < USE_DEVICE) chance = USE_DEVICE;

    if (chance > fail)
        return fail * 1000 / (chance*2);
    else
        return 1000 - chance * 1000 / (fail*2);
}

static bool _activate_try(object_type *o_ptr)
{
    int fail = activation_fail_rate(o_ptr);
    if (randint0(1000) < fail)
        return FALSE;
    return TRUE;
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
    int         k, dir;
    object_type *o_ptr;
    bool success;


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

    success = _activate_try(o_ptr);

    if (!success)
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

    if (o_ptr->art_name && o_ptr->xtra2)
    {
        (void)activate_random_artifact(o_ptr);
        p_ptr->window |= (PW_INVEN | PW_EQUIP);
        return;
    }
    else if (object_is_fixed_artifact(o_ptr))
    {
        switch (o_ptr->name1)
        {
            case ART_HOLY_GRAIL:
            {
                msg_print("You drink from the holy grail!");
                hp_player(50);
                set_resist_magic(device_power(10 + randint1(10)), FALSE);
                o_ptr->timeout = 12;
                break;
            }
            case ART_STONE_OF_NATURE:
            {
                msg_print("Your stone hums softly.");
                set_shield(device_power(randint1(30) + 20), FALSE);
                o_ptr->timeout = 100;
                break;
            }
            case ART_STONE_OF_LIFE:
            {
                msg_print("Your stone glows a pure white.");
                do_res_stat(A_STR);
                do_res_stat(A_INT);
                do_res_stat(A_WIS);
                do_res_stat(A_DEX);
                do_res_stat(A_CON);
                do_res_stat(A_CHR);
                restore_level();
                o_ptr->timeout = 500;
                break;
            }
            case ART_STONE_OF_SORCERY:
            {
                msg_print("Your stone begins to vibrate rapidly.");
                set_fast(device_power(randint1(30) + 20), FALSE);
                o_ptr->timeout = 100;
                break;
            }
            case ART_STONE_OF_CHAOS:
            {
                msg_print("Your stone shifts colors rapidly.");
                if (!get_check("You will polymorph yourself. Are you sure? ")) return;
                do_poly_self();            
                o_ptr->timeout = 500;
                break;
            }
            case ART_STONE_OF_DEATH:
            {
                msg_print("Your stone emits a foul breeze.");
                animate_dead(0, py, px);
                o_ptr->timeout = 666;
                break;
            }
            case ART_STONE_OF_TRUMP:
            {
                if (mut_present(MUT_ASTRAL_GUIDE))
                    energy_use = 30;
                teleport_player(200, 0);
            /*    o_ptr->timeout = 5; */
                break;
            }
            case ART_STONE_OF_DAEMON:
            {
                msg_print("Your stone glows a fiery red.");
                if (!get_aim_dir(&dir)) return;
                fire_ball(GF_FIRE, dir, device_power(p_ptr->chp), 3);
                o_ptr->timeout = 666;
                break;
            }
            case ART_STONE_OF_CRUSADE:
            {
                msg_print("Your stone offers heavenly protection.");
                set_protevil(device_power(randint1(25) + p_ptr->lev*3), FALSE);
                o_ptr->timeout = 555;
                break;
            }
            case ART_STONE_OF_CRAFT:
            {
                msg_print("Your stone glows many colors.");
                (void)set_oppose_acid(device_power(randint1(20) + 20), FALSE);
                (void)set_oppose_elec(device_power(randint1(20) + 20), FALSE);
                (void)set_oppose_fire(device_power(randint1(20) + 20), FALSE);
                (void)set_oppose_cold(device_power(randint1(20) + 20), FALSE);
                (void)set_oppose_pois(device_power(randint1(20) + 20), FALSE);
                o_ptr->timeout = 100;
                break;
            }
            case ART_STONE_OF_WAR:
            {
                msg_print("Your stone enrages you!");
                set_shero(device_power(20 + randint0(20)), FALSE);
                o_ptr->timeout = 100;
                break;
            }
            case ART_STONE_OF_ARMAGEDDON:
            {
                msg_print("Your stone glows an intense red.");
                if (!destroy_area(py, px, 13 + randint0(5), 1000))
                    msg_print("The dungeon trembles...");
                o_ptr->timeout = 10;
                break;
            }
            case ART_ZEUS:
            {
                msg_print("Your pendant crackles with power.");
                if (!get_aim_dir(&dir)) return;
                fire_ball(GF_ELEC, dir, device_power(555), 3);
                o_ptr->timeout = 5;
                break;
            }
            case ART_POSEIDON:
            {
                msg_print("The ground trembles!");
                earthquake(py, px, 5);
                break;
            }
            case ART_HADES:
            {
                msg_print("Your clothes glow black.");
                restore_level();
                do_res_stat(A_STR);
                do_res_stat(A_INT);
                do_res_stat(A_WIS);
                do_res_stat(A_DEX);
                do_res_stat(A_CON);
                do_res_stat(A_CHR);
                o_ptr->timeout = 5;
                break;
            }
            case ART_ATHENA:
            {
                msg_print("Your spear glows red.");
                recharge(device_power(200));
                o_ptr->timeout = 15;
                break;
            }
            case ART_ARES:
            {
                msg_print("Your head fills with the dying cries of fallen heroes.");
                cast_berserk();
                o_ptr->timeout = 20;
                break;
            }
            case ART_HERMES:
            {
                msg_print("Your boots shine.");
                set_fast(device_power(randint1(75) + 75), FALSE);
                dimension_door(device_power(p_ptr->lev / 2 + 10));
                o_ptr->timeout = 100;
                break;
            }
            case ART_APOLLO:
            {
                msg_print("Your harp flashes bright red!");
                wiz_lite(p_ptr->tim_superstealth > 0);
                detect_traps(DETECT_RAD_DEFAULT, TRUE);
                detect_doors(DETECT_RAD_DEFAULT);
                detect_stairs(DETECT_RAD_DEFAULT);
                o_ptr->timeout = 20;
                break;
            }
            case ART_ARTEMIS:
            {
                object_type forge;
                char o_name[MAX_NLEN];
                int slot;

                msg_print("Your bow glows purple.");

                object_prep(&forge, lookup_kind(TV_ARROW, m_bonus(1, p_ptr->lev)+ 1));
                forge.number = (byte)rand_range(5, 10);
                object_aware(&forge);
                object_known(&forge);
                apply_magic(&forge, p_ptr->lev, AM_NO_FIXED_ART);

                forge.discount = 99;

                object_desc(o_name, &forge, 0);

                slot = inven_carry(&forge);
                if (slot >= 0) autopick_alter_item(slot, FALSE);

                o_ptr->timeout = 999;
                break;
            }
            case ART_HEPHAESTUS:
            {
                msg_print("You mighty hammer lets out a shrill wail.");
                enchantment_hack = TRUE;
                if (cast_enchantment())
                    o_ptr->timeout = 200;
                enchantment_hack = FALSE;
                break;
            }
            case ART_HERA:
            {
                restore_mana();
                set_shero(0,TRUE);
                o_ptr->timeout = 55;
                break;
            }
            case ART_DEMETER:
            {
                msg_print("Your torch engulfs you in a purifying flame!");
                hp_player(device_power(500));
                set_food(PY_FOOD_MAX - 1);
                o_ptr->timeout = 100;
                break;
            }
            case ART_APHRODITE:
            {
                msg_print("Your golden apple pulsates!");
                summon_specific(-1, py, px, device_power(MIN(p_ptr->lev, dun_level)), 0, (PM_ALLOW_GROUP | PM_FORCE_PET));
                o_ptr->timeout = 25;
                break;
            }
            case ART_GALADRIEL:
            {
                msg_print("The phial wells with clear light...");

                lite_area(device_power(damroll(2, 15)), 3);
                o_ptr->timeout = randint0(10) + 10;
                break;
            }

            case ART_ELENDIL:
            {
                msg_print("The star shines brightly...");

                map_area(DETECT_RAD_MAP);
                lite_area(device_power(damroll(2, 15)), 3);
                o_ptr->timeout = randint0(50) + 50;
                break;
            }
            case ART_EYE_OF_VECNA:
            {
                msg_print("The Eye burns painfully!");
                take_hit(DAMAGE_LOSELIFE, damroll(8, 8), "the Eye of Vecna", -1);
                wiz_lite(TRUE);
                o_ptr->timeout = randint0(20) + 20;
                break;
            }
            case ART_OMARAX:
                msg_print("The crown grants amazing sight!");
                wiz_lite(p_ptr->tim_superstealth > 0);
                detect_traps(DETECT_RAD_DEFAULT, TRUE);
                detect_doors(DETECT_RAD_DEFAULT);
                detect_stairs(DETECT_RAD_DEFAULT);
                o_ptr->timeout = randint0(20) + 20;
                break;
            case ART_LERNEAN:
                msg_print("The Eye engulfs you in healing flames!");
                hp_player(700);
                set_poisoned(0, TRUE);
                set_cut(0, TRUE);
                set_stun(0, TRUE);
                set_confused(0, TRUE);
                set_blind(0, TRUE);
                o_ptr->timeout = 300;
                break;
            case ART_JUDGE:
            {
                msg_print("The Jewel flashes bright red!");
                virtue_add(VIRTUE_KNOWLEDGE, 1);
                virtue_add(VIRTUE_ENLIGHTENMENT, 1);
                wiz_lite(p_ptr->tim_superstealth > 0);
                msg_print("The Jewel drains your vitality...");
                take_hit(DAMAGE_LOSELIFE, damroll(3, 8), "the Jewel of Judgement", -1);
                (void)detect_traps(DETECT_RAD_DEFAULT, TRUE);
                (void)detect_doors(DETECT_RAD_DEFAULT);
                (void)detect_stairs(DETECT_RAD_DEFAULT);
                if (get_check("Activate recall? "))
                    (void)word_of_recall();
                o_ptr->timeout = randint0(20) + 20;
                break;
            }

            case ART_CARLAMMAS:
            {
                msg_print("The amulet lets out a shrill wail...");

                k = 3 * p_ptr->lev;
                (void)set_protevil(device_power(randint1(25) + k), FALSE);
                o_ptr->timeout = randint0(225) + 225;
                break;
            }

            case ART_INGWE:
            {
                msg_print("The amulet floods the area with goodness...");

                dispel_evil(device_power(p_ptr->lev * 5));
                o_ptr->timeout = randint0(200) + 200;
                break;
            }

            case ART_YATA:
            {
                msg_print("The mirror floods the area with goodness...");

                dispel_evil(device_power(p_ptr->lev * 5));
                o_ptr->timeout = randint0(200) + 200;
                break;
            }

            case ART_FRAKIR:
            {
                msg_print("You order Frakir to strangle your opponent.");

                if (!get_aim_dir(&dir)) return;
                if (drain_life(dir, device_power(100)))
                o_ptr->timeout = randint0(100) + 100;
                break;
            }

            case ART_TULKAS:
            {
                msg_print("The ring glows brightly...");

                (void)set_fast(device_power(randint1(75) + 75), FALSE);
                o_ptr->timeout = randint0(150) + 150;
                break;
            }

            case ART_NARYA:
            {
                msg_print("The ring glows deep red...");

                if (!get_aim_dir(&dir)) return;
                fire_ball(GF_FIRE, dir, device_power(300), 3);
                o_ptr->timeout = randint0(225) + 225;
                break;
            }

            case ART_NENYA:
            {
                msg_print("The ring glows bright white...");

                if (!get_aim_dir(&dir)) return;
                fire_ball(GF_COLD, dir, device_power(400), 3);
                o_ptr->timeout = randint0(325) + 325;
                break;
            }

            case ART_VILYA:
            case ART_GOURYU:
            {
                msg_format("The %s glows deep blue...", o_ptr->name1 == ART_VILYA ? "ring" : "sword");

                if (!get_aim_dir(&dir)) return;
                fire_ball(GF_ELEC, dir, device_power(500), 3);
                o_ptr->timeout = randint0(425) + 425;
                break;
            }

            case ART_POWER:
            case ART_AHO:
            {
                msg_print("The ring glows intensely black...");

                if (!get_aim_dir(&dir)) return;
                ring_of_power(dir);
                o_ptr->timeout = randint0(450) + 450;
                break;
            }

            case ART_RAZORBACK:
            {
                int num = device_power(damroll(5, 3));
                int y, x;
                int attempts;

                msg_print("Your armor is surrounded by lightning...");


                for (k = 0; k < num; k++)
                {
                    attempts = 1000;

                    while (attempts--)
                    {
                        scatter(&y, &x, py, px, 4, 0);

                        if (!cave_have_flag_bold(y, x, FF_PROJECT)) continue;

                        if (!player_bold(y, x)) break;
                    }

                    project(0, 3, y, x, device_power(150), GF_ELEC,
                              (PROJECT_THRU | PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL), -1);
                }

                o_ptr->timeout = 1000;
                break;
            }

            case ART_BLADETURNER:
            {
                if (!get_aim_dir(&dir)) return;
                msg_print("You breathe the elements.");

                fire_ball(GF_MISSILE, dir, device_power(300), 4);
                msg_print("Your armor glows many colours...");

                (void)set_hero(device_power(randint1(50) + 50), FALSE);
                (void)set_blessed(device_power(randint1(50) + 50), FALSE);
                (void)set_oppose_acid(device_power(randint1(50) + 50), FALSE);
                (void)set_oppose_elec(device_power(randint1(50) + 50), FALSE);
                (void)set_oppose_fire(device_power(randint1(50) + 50), FALSE);
                (void)set_oppose_cold(device_power(randint1(50) + 50), FALSE);
                (void)set_oppose_pois(device_power(randint1(50) + 50), FALSE);
                o_ptr->timeout = 400;
                break;
            }

            case ART_SOULKEEPER:
            {
                msg_print("Your armor glows a bright white...");
                msg_print("You feel much better...");

                (void)hp_player(device_power(1000));
                (void)set_cut(0, TRUE);
                o_ptr->timeout = 888;
                break;
            }

            case ART_DUELIST:
                msg_print("Your rapier glows a deep purple...");
                if (mut_present(MUT_ASTRAL_GUIDE))
                    energy_use = 30;
                teleport_player(10, TELEPORT_LINE_OF_SIGHT);
                o_ptr->timeout = 3;
                break;

            case ART_LOHENGRIN:
            case ART_DAERON:
            {
                if (o_ptr->name1 == ART_LOHENGRIN)
                {
                    msg_print("A heavenly choir sings...");
                }
                else
                    msg_print("Your harps plays a restoring melody...");

                (void)set_poisoned(0, TRUE);
                (void)set_cut(0, TRUE);
                (void)set_stun(0, TRUE);
                (void)set_confused(0, TRUE);
                (void)set_blind(0, TRUE);
                (void)set_hero(device_power(randint1(25) + 25), FALSE);
                (void)hp_player(device_power(777));
                o_ptr->timeout = 300;
                break;
            }

            case ART_JULIAN:
            {
                msg_print("Your armor glows deep blue...");

                (void)symbol_genocide(device_power(200), TRUE);
                o_ptr->timeout = 500;
                break;
            }

            case ART_CASPANION:
            {
                msg_print("Your armor glows bright red...");

                destroy_doors_touch();
                o_ptr->timeout = 10;
                break;
            }

            case ART_DOR:
            case ART_TERROR:
            case ART_STONEMASK:
            {
                turn_monsters(device_power(40 + p_ptr->lev));
                o_ptr->timeout = 3 * (p_ptr->lev + 10);

                break;
            }

            case ART_HOLHENNETH:
            {
                msg_print("Your helm glows bright white...");
                msg_print("An image forms in your mind...");

                detect_all(DETECT_RAD_DEFAULT);
                o_ptr->timeout = randint0(55) + 55;
                break;
            }

            case ART_AMBER:
            {
                msg_print("Your crown glows deep blue...");
                msg_print("You feel a warm tingling inside...");

                (void)hp_player(device_power(700));
                (void)set_cut(0, TRUE);
                o_ptr->timeout = 250;
                break;
            }

            case ART_SARUMAN:
            {
                msg_print("Your staff glows many colours...");

                (void)set_oppose_acid(device_power(randint1(20) + 20), FALSE);
                (void)set_oppose_elec(device_power(randint1(20) + 20), FALSE);
                (void)set_oppose_fire(device_power(randint1(20) + 20), FALSE);
                (void)set_oppose_cold(device_power(randint1(20) + 20), FALSE);
                (void)set_oppose_pois(device_power(randint1(20) + 20), FALSE);
                o_ptr->timeout = 111;
                break;
            }

            case ART_COLLUIN:
            case ART_SEIRYU:
            {
                msg_format("Your %s glows many colours...", o_ptr->name1 == ART_COLLUIN ? "cloak" : "armor");

                (void)set_oppose_acid(device_power(randint1(20) + 20), FALSE);
                (void)set_oppose_elec(device_power(randint1(20) + 20), FALSE);
                (void)set_oppose_fire(device_power(randint1(20) + 20), FALSE);
                (void)set_oppose_cold(device_power(randint1(20) + 20), FALSE);
                (void)set_oppose_pois(device_power(randint1(20) + 20), FALSE);
                o_ptr->timeout = 111;
                break;
            }
            case ART_MAGLOR:
            {
                msg_print("Your harps plays a restoring melody...");
                restore_mana();
                set_shero(0,TRUE);
                o_ptr->timeout = 777;
                break;
            }

            case ART_BLOODRIP:
            {
                int y = 0, x = 0;
                cave_type       *c_ptr;
                monster_type    *m_ptr;

                msg_print("Your sword glows blood red...");
                for (dir = 0; dir < 8; dir++)
                {
                    y = py + ddy_ddd[dir];
                    x = px + ddx_ddd[dir];
                    c_ptr = &cave[y][x];

                    /* Get the monster */
                    m_ptr = &m_list[c_ptr->m_idx];

                    /* Hack -- attack monsters */
                    if (c_ptr->m_idx && (m_ptr->ml || cave_have_flag_bold(y, x, FF_PROJECT)))
                        py_attack(y, x, 0);
                }
                o_ptr->timeout = 66;
                break;
            }
            case ART_HOLCOLLETH:
            {
                msg_print("Your cloak glows deep blue...");

                sleep_monsters_touch();
                o_ptr->timeout = 55;
                break;
            }

            case ART_THINGOL:
            {
                msg_print("Your cloak glows bright yellow...");

                recharge(device_power(130));
                o_ptr->timeout = 70;
                break;
            }

            case ART_COLANNON:
            {
                msg_print("Your cloak twists space around you...");
                if (mut_present(MUT_ASTRAL_GUIDE))
                    energy_use = 30;

                teleport_player(100, 0L);
                o_ptr->timeout = 45;
                break;
            }

            case ART_LUTHIEN:
            {
                msg_print("Your cloak glows a deep red...");

                restore_level();
                o_ptr->timeout = 450;
                break;
            }

            case ART_HEAVENLY_MAIDEN:
            {
                msg_print("Your cloak glows soft white...");
                if (!word_of_recall()) return;
                o_ptr->timeout = 200;
                break;
            }

            case ART_CAMMITHRIM:
            {
                msg_print("Your gloves glow extremely brightly...");

                if (!get_aim_dir(&dir)) return;
                fire_bolt(GF_MISSILE, dir, device_power(damroll(2, 6)));
                o_ptr->timeout = 2;
                break;
            }

            case ART_FINGOLFIN:
            {
                msg_print("Your cesti grows magical spikes...");

                if (!get_aim_dir(&dir)) return;
                fire_bolt(GF_ARROW, dir, device_power(150));
                o_ptr->timeout = randint0(90) + 90;
                break;
            }

            case ART_FEANOR:
            {
                msg_print("Your boots glow bright green...");

                (void)set_fast(device_power(randint1(20) + 20), FALSE);
                o_ptr->timeout = 200;
                break;
            }

            case ART_FLORA:
            {
                msg_print("Your boots glow deep blue...");

                fear_clear_p();
                (void)set_poisoned(0, TRUE);
                o_ptr->timeout = 5;
                break;
            }

            case ART_NARTHANC:
            {
                msg_print("Your dagger is covered in fire...");

                if (!get_aim_dir(&dir)) return;
                fire_bolt(GF_FIRE, dir, device_power(damroll(9, 8)));
                o_ptr->timeout = randint0(8) + 8;
                break;
            }

            case ART_KAMIKAZE_ROBE:
            {
                msg_print("Your robe glows crimson...");
                set_tim_speed_essentia(5 + randint1(5), FALSE);
                o_ptr->timeout = 111;
                break;
            }
            case ART_BALLISTA:
            {
                bool fired = FALSE;
                msg_print("Your ballista glows black...");
                shoot_hack = SHOOT_PIERCE;
                fired = do_cmd_fire();
                shoot_hack = SHOOT_NONE;
                if (!fired) return;
                o_ptr->timeout = 100;
                break;
            }
            case ART_STOMPER:
            {
                msg_print("You stomp your foot down powerfully...");
                earthquake(py, px, 10);
                o_ptr->timeout = 35;
                break;
            }
            case ART_RAILGUN:
            {
                msg_print("Your gun is covered with a blinding light...");
                if (!get_aim_dir(&dir)) return;
                fire_beam(GF_LITE, dir, device_power(300));
                o_ptr->timeout = 0; /* Every turn? */
                break;
            }
            case ART_GONG:
            {    
                int dam = damroll(p_ptr->lev, 5);
                msg_print("BOOOOOOOOOOOOOOOOOOONGGGGGGGGGGG!!!");
                if (!res_save_default(RES_SOUND))
                    project(-1, 0, py, px, device_power(dam), GF_SOUND, PROJECT_KILL | PROJECT_HIDE, -1);
                project(0, 18, py, px, device_power(dam*2), GF_SOUND, PROJECT_KILL | PROJECT_ITEM, -1);
                o_ptr->timeout = 5 + randint1(5);
                break;
            }
            case ART_NIMTHANC:
            {
                msg_print("Your dagger is covered in frost...");

                if (!get_aim_dir(&dir)) return;
                fire_bolt(GF_COLD, dir, device_power(damroll(6, 8)));
                o_ptr->timeout = randint0(7) + 7;
                break;
            }

            case ART_DETHANC:
            {
                msg_print("Your dagger is covered in sparks...");

                if (!get_aim_dir(&dir)) return;
                fire_bolt(GF_ELEC, dir, device_power(damroll(4, 8)));
                o_ptr->timeout = randint0(5) + 5;
                break;
            }

            case ART_RILIA:
            {
                msg_print("Your dagger throbs deep green...");

                if (!get_aim_dir(&dir)) return;
                fire_ball(GF_POIS, dir, device_power(12), 3);
                o_ptr->timeout = randint0(4) + 4;
                break;
            }

            case ART_NUMAHOKO:
            {
                msg_print("Your dagger throbs deep blue...");

                if (!get_aim_dir(&dir)) return;
                fire_ball(GF_WATER, dir, device_power(200), 3);
                o_ptr->timeout = 250;
                break;
            }

            case ART_FIONA:
            {
                msg_print("Your dagger is covered in frost...");

                if (!get_aim_dir(&dir)) return;
                fire_ball(GF_COLD, dir, device_power(48), 2);
                o_ptr->timeout = randint0(5) + 5;
                break;
            }

            case ART_KUSANAGI:
            case ART_WEREWINDLE:
            {
                switch (randint1(13))
                {
                case 1: case 2: case 3: case 4: case 5:
                    if (mut_present(MUT_ASTRAL_GUIDE))
                        energy_use = 30;
                    teleport_player(10, 0L);
                    break;
                case 6: case 7: case 8: case 9: case 10:
                    if (mut_present(MUT_ASTRAL_GUIDE))
                        energy_use = 30;
                    teleport_player(222, 0L);
                    break;
                case 11: case 12:
                    (void)stair_creation(FALSE);
                    break;
                default:
                    if (get_check("Leave this level? "))

                    {
                        if (autosave_l) do_cmd_save_game(TRUE);

                        /* Leaving */
                        p_ptr->leaving = TRUE;
                    }
                }
                o_ptr->timeout = 35;
                break;
            }

            case ART_KAMUI:
            {
                teleport_player(222, 0L);
                o_ptr->timeout = 25;
                break;
            }

            case ART_RINGIL:
            {
                msg_print("Your sword glows an intense blue...");

                if (!get_aim_dir(&dir)) return;
                fire_ball(GF_COLD, dir, device_power(100), 2);
                o_ptr->timeout = 200;
                break;
            }

            case ART_DAWN:
            {
                msg_print("You summon the Legion of the Dawn.");

                (void)summon_specific(-1, py, px, device_power(dun_level), SUMMON_DAWN, (PM_ALLOW_GROUP | PM_FORCE_PET));
                o_ptr->timeout = 500 + randint1(500);
                break;
            }

            case ART_ANDURIL:
            {
                msg_print("Your sword glows an intense red...");

                if (!get_aim_dir(&dir)) return;
                fire_ball(GF_FIRE, dir, device_power(72), 2);
                o_ptr->timeout = 400;
                break;
            }

            case ART_THEODEN:
            {
                msg_print("Your axe blade glows black...");

                if (!get_aim_dir(&dir)) return;
                if (drain_life(dir, device_power(120)))
                    hp_player(device_power(120));
                o_ptr->timeout = 400;
                break;
            }

            case ART_RUNESPEAR:
            {
                msg_print("Your spear crackles with electricity...");

                if (!get_aim_dir(&dir)) return;
                fire_ball(GF_ELEC, dir, device_power(100), 3);
                o_ptr->timeout = 200;
                break;
            }

            case ART_AEGLOS:
            {
                msg_print("Your spear glows a bright white...");

                if (!get_aim_dir(&dir)) return;
                fire_ball(GF_COLD, dir, device_power(100), 3);
                o_ptr->timeout = 200;
                break;
            }

            case ART_DESTINY:
            {
                msg_print("Your spear pulsates...");

                if (!get_aim_dir(&dir)) return;
                wall_to_mud(dir);
                o_ptr->timeout = 5;
                break;
            }

            case ART_NAIN:
            {
                msg_print("Your mattock pulsates...");

                if (!get_aim_dir(&dir)) return;
                wall_to_mud(dir);
                o_ptr->timeout = 2;
                break;
            }

            case ART_EONWE:
            {
                msg_print("Your axe lets out a long, shrill note...");

                (void)mass_genocide(device_power(200), TRUE);
                o_ptr->timeout = 1000;
                break;
            }

            case ART_LOTHARANG:
            {
                msg_print("Your battle axe radiates deep purple...");

                hp_player(device_power(damroll(4, 8)));
                (void)set_cut((p_ptr->cut / 2) - 50, TRUE);
                o_ptr->timeout = randint0(3) + 3;
                break;
            }

            case ART_ULMO:
            {
                msg_print("Your trident glows deep red...");

                if (!get_aim_dir(&dir)) return;
                teleport_monster(dir);
                o_ptr->timeout = 150;
                break;
            }

            case ART_AVAVIR:
            {
                msg_print("Your scythe glows soft white...");
                if (!word_of_recall()) return;
                o_ptr->timeout = 200;
                break;
            }

            case ART_MAGATAMA:
            {
                msg_print("Your scythe glows soft white...");
                if (!word_of_recall()) return;
                o_ptr->timeout = 200;
                break;
            }

            case ART_TOTILA:
            {
                msg_print("Your flail glows in scintillating colours...");

                if (!get_aim_dir(&dir)) return;
                confuse_monster(dir, device_power(20));
                o_ptr->timeout = 15;
                break;
            }

            case ART_FIRESTAR:
            {
                msg_print("Your morning star rages in fire...");

                if (!get_aim_dir(&dir)) return;
                fire_ball(GF_FIRE, dir, device_power(72), 3);
                o_ptr->timeout = 100;
                break;
            }

            case ART_AEGIR:
            {
                msg_print("Your club shines golden ...");
                set_mimic(device_power(25 + randint1(25)), MIMIC_COLOSSUS, FALSE);
                o_ptr->timeout = 500;
                break;
            }

            case ART_DEFENDER_OF_THE_CROWN:
            {
                msg_print("Your ball and chain goes 'boop' ...");
                set_shield(device_power(randint1(30) + 20), FALSE);
                o_ptr->timeout = 300;
                break;
            }
            case ART_MONKEY_KING:
            {
                msg_print("Your cudgel grows bigger ...");
                set_tim_enlarge_weapon(device_power(5), FALSE);
                o_ptr->timeout = 555;
                break;
            }
            case ART_MAUL_OF_VICE:
            {
                msg_print("Your maul rends the fabric of space-time ...");
                set_lightspeed(8, FALSE);
                o_ptr->timeout = 888;
                break;
            }
            case ART_GOTHMOG:
            {
                msg_print("Your whip glows deep red...");

                if (!get_aim_dir(&dir)) return;
                fire_ball(GF_FIRE, dir, device_power(120), 3);
                o_ptr->timeout = 15;
                break;
            }

            case ART_TARATOL:
            {
                msg_print("Your mace glows bright green...");

                (void)set_fast(device_power(randint1(20) + 20), FALSE);
                o_ptr->timeout = randint0(100) + 100;
                break;
            }

            case ART_ERIRIL:
            {
                msg_print("Your quarterstaff glows yellow...");

                if (!ident_spell(NULL)) return;
                o_ptr->timeout = 10;
                break;
            }

            case ART_GANDALF:
            {
                msg_print("Your quarterstaff glows brightly...");

                detect_all(DETECT_RAD_DEFAULT);
                probing();
                identify_fully(NULL);
                o_ptr->timeout = 100;
                break;
            }

            case ART_TURMIL:
            {
                msg_print("Your hammer glows white...");

                if (!get_aim_dir(&dir)) return;
                if (drain_life(dir, device_power(90)))
                    hp_player(device_power(90));
                o_ptr->timeout = 70;
                break;
            }

            case ART_CRIMSON:
            {
                int num = 1;
                int i;
                int flg = PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;
                int tx, ty;
                msg_print("I'll fire CRIMSON! SEKKAKUDAKARA!");

                if (!get_aim_dir(&dir)) return;

                /* Use the given direction */
                tx = px + 99 * ddx[dir];
                ty = py + 99 * ddy[dir];

                /* Hack -- Use an actual "target" */
                if ((dir == 5) && target_okay())
                {
                    tx = target_col;
                    ty = target_row;
                }

                if (p_ptr->pclass == CLASS_ARCHER)
                {
                    /* Extra shot at level 10 */
                    if (p_ptr->lev >= 10) num++;

                    /* Extra shot at level 30 */
                    if (p_ptr->lev >= 30) num++;

                    /* Extra shot at level 45 */
                    if (p_ptr->lev >= 45) num++;
                }

                for (i = 0; i < num; i++)
                    project(0, p_ptr->lev/20+1, ty, tx, device_power(p_ptr->lev*p_ptr->lev*6/50), GF_ROCKET, flg, -1);
                o_ptr->timeout = 15;
                break;
            }
            case ART_PALANTIR:
            {
                monster_type *m_ptr;
                monster_race *r_ptr;
                int i;

                msg_print("Some strange places show up in your mind. And you see ...");

                /* Process the monsters (backwards) */
                for (i = m_max - 1; i >= 1; i--)
                {
                    /* Access the monster */
                    m_ptr = &m_list[i];

                    /* Ignore "dead" monsters */
                    if (!m_ptr->r_idx) continue;

                    r_ptr = &r_info[m_ptr->r_idx];

                    if(r_ptr->flags1 & RF1_UNIQUE)
                    {
                        msg_format("%s. ",r_name + r_ptr->name);
                    }
                }
                o_ptr->timeout = 200;
                break;
            }

            case ART_BOROMIR:
            {
                if (music_singing_any()) bard_stop_singing();
                if (hex_spelling_any()) stop_hex_spell_all();
                msg_print("You wind a mighty blast; your enemies tremble!");
                (void)turn_monsters(device_power((3 * p_ptr->lev / 2) + 10));
                o_ptr->timeout = randint0(40) + 40;
                break;
            }
            case ART_FARAMIR:
            {
                msg_print("You exterminate small life.");
                (void)dispel_monsters(device_power(4));
                o_ptr->timeout = randint0(55) + 55;
                break;
            }

            case ART_HIMRING:
            {
                msg_print("A shrill wailing sound surrounds you.");
                (void)set_protevil(device_power(randint1(25) + p_ptr->lev), FALSE);
                o_ptr->timeout = randint0(200) + 200;
                break;
            }

            case ART_ICANUS:
            {

                msg_print("The robe pulsates with raw mana...");
                if (!get_aim_dir(&dir)) return;
                fire_bolt(GF_MANA, dir, device_power(120));
                o_ptr->timeout = randint0(120) + 120;
                break;
            }
            case ART_HURIN:
            {
                (void)set_fast(device_power(randint1(50) + 50), FALSE);
                set_hero(device_power(randint1(50) + 50), FALSE);
                o_ptr->timeout = randint0(200) + 100;
                break;
            }
            case ART_GIL_GALAD:
            {
                msg_print("Your shield gleams with blinding light...");
                fire_ball(GF_LITE, 0, device_power(300), 6);
                confuse_monsters(device_power(3 * p_ptr->lev / 2));
                o_ptr->timeout = 250;
                break;
            }
            case ART_YENDOR:
            {
                msg_print("Your card gleams with blinding light...");
                if (!recharge(device_power(1000))) return;
                o_ptr->timeout = 200;
                break;
            }
            case ART_MURAMASA:
            {
                if (get_check("Are you sure?!"))
                {
                    msg_print("The Muramasa pulsates...");
                    do_inc_stat(A_STR);
                    if (one_in_(2))
                    {
                        msg_print("The Muramasa is destroyed!");
                        curse_weapon(TRUE, item);
                    }
                }
                break;
            }
            case ART_FLY_STONE:
            {
                msg_print("Your stone glows pale...");

                if (!get_aim_dir(&dir)) return;
                fire_ball(GF_MANA, dir, device_power(400), 4);
                o_ptr->timeout = randint0(250) + 250;
                break;
            }
            case ART_TAIKOBO:
            {
                int x, y;

                if (!get_rep_dir2(&dir)) return;
                y = py+ddy[dir];
                x = px+ddx[dir];
                tsuri_dir = dir;
                if (!cave_have_flag_bold(y, x, FF_WATER))
                {
                    msg_print("There is no fishing place.");
                    return;
                }
                else if (cave[y][x].m_idx)
                {
                    char m_name[80];
                    monster_desc(m_name, &m_list[cave[y][x].m_idx], 0);
                    msg_format("%^s is stand in your way.", m_name);
                    energy_use = 0;
                    return;
                }
                set_action(ACTION_FISH);
                p_ptr->redraw |= (PR_STATE);
                break;
            }
            case ART_JONES:
            {
                if (!get_aim_dir(&dir)) return;
                msg_print("You stretched your whip.");

                fetch(dir, 500, TRUE);
                o_ptr->timeout = randint0(25) + 25;
                break;
            }
            case ART_ARRYU:
            {
                u32b mode = PM_ALLOW_GROUP;
                bool pet = !one_in_(5);
                if (pet) mode |= PM_FORCE_PET;
                else mode |= PM_NO_PET;

                if (summon_specific((pet ? -1 : 0), py, px, device_power((p_ptr->lev * 3) / 2), SUMMON_HOUND, mode))
                {

                    if (pet)
                    msg_print("A group of hounds appear as your servant.");

                    else
                        msg_print("A group of hounds appear as your enemy!");

                }

                o_ptr->timeout = 300 + randint1(150);
                break;
            }

            case ART_GAEBOLG:
            {
                msg_print("Your spear grows brightly...");

                if (!get_aim_dir(&dir)) return;
                fire_ball(GF_LITE, dir, device_power(200), 3);
                o_ptr->timeout = randint0(200) + 200;
                break;
            }

            case ART_INROU:
            {
                int count = 0, i;
                monster_type *m_ptr;
                cptr kakusan = "";

                if (summon_named_creature(0, py, px, MON_SUKE, PM_FORCE_PET))
                {
                    msg_print("Suke-san appears.");
                    kakusan = "Suke-san";
                    count++;
                }
                if (summon_named_creature(0, py, px, MON_KAKU, PM_FORCE_PET))
                {
                    msg_print("Kaku-san appears.");
                    kakusan = "Kaku-san";
                    count++;
                }
                if (!count)
                {
                    for (i = m_max - 1; i > 0; i--)
                    {
                        m_ptr = &m_list[i];
                        if (!m_ptr->r_idx) continue;
                        if (!((m_ptr->r_idx == MON_SUKE) || (m_ptr->r_idx == MON_KAKU))) continue;
                        if (!los(m_ptr->fy, m_ptr->fx, py, px)) continue;
                        if (!projectable(m_ptr->fy, m_ptr->fx, py, px)) continue;
                        count++;
                        break;
                    }
                }

                if (count)
                {
                    msg_format("%^s says 'WHO do you think this person is! Bow your head, down your knees!'", kakusan);

                    sukekaku = TRUE;
                    stun_monsters(120);
                    confuse_monsters(120);
                    turn_monsters(120);
                    stasis_monsters(120);
                    sukekaku = FALSE;
                }
                else
                {
                    msg_print("Nothing happen.");
                }
                o_ptr->timeout = randint0(150) + 150;
                break;
            }

            case ART_HYOUSIGI:
            {
                msg_print("You beat Your wooden clappers.");
                aggravate_monsters(0);
                break;
            }

            case ART_MATOI:
            case ART_AEGISFANG:
            {
                set_hero(device_power(randint1(25)+25), FALSE);
                o_ptr->timeout = randint0(30) + 30;
                break;
            }

            case ART_EARENDIL:
            {
                (void)set_poisoned(0, TRUE);
                (void)set_confused(0, TRUE);
                (void)set_blind(0, TRUE);
                (void)set_stun(0, TRUE);
                (void)set_cut(0, TRUE);
                (void)set_image(0, TRUE);

                o_ptr->timeout = 100;
                break;
            }

            case ART_BOLISHOI:
            {
                if (!get_aim_dir(&dir)) return;
                (void)charm_animal(dir, device_power(p_ptr->lev));

                o_ptr->timeout = 200;
                break;
            }

            case ART_ARUNRUTH:
            {
                msg_print("Your sword glows a pale blue...");
                if (!get_aim_dir(&dir)) return;
                fire_bolt(GF_COLD, dir, device_power(damroll(12, 8)));
                o_ptr->timeout = 50;
                break;
            }
            case ART_BLOOD:
            {
                msg_print("Your scythe glows brightly!");
                get_bloody_moon_flags(o_ptr);
                o_ptr->timeout = 3333;
                if (p_ptr->prace == RACE_ANDROID) calc_android_exp();
                p_ptr->update |= (PU_BONUS | PU_HP);
                break;
            }
            case ART_KESHO:
            {
                msg_print("You stamp your feet (as if you are in a ring.)");
                (void)set_hero(device_power(randint1(20) + 20), FALSE);
                dispel_evil(device_power(p_ptr->lev * 3));
                o_ptr->timeout = 100 + randint1(100);
                break;
            }
            case ART_MOOK:
            {
                msg_print("Your cloak grows white.");
                (void)set_oppose_cold(device_power(randint1(20) + 20), FALSE);
                o_ptr->timeout = 40 + randint1(40);
                break;
            }
            case ART_HERMIT:
            {
                msg_print("The whip lets out a shrill wail...");

                k = 3 * p_ptr->lev;
                (void)set_protevil(device_power(randint1(25) + k), FALSE);
                o_ptr->timeout = randint0(225) + 225;
                break;
            }
            case ART_JIZO:
            {
                u32b mode = PM_ALLOW_GROUP;
                bool pet = !one_in_(5);
                if (pet) mode |= PM_FORCE_PET;

                if (summon_named_creature(0, py, px, MON_JIZOTAKO, mode))
                {
                    if (pet)
                    msg_print("A group of octopuses appear as your servant.");

                    else
                        msg_print("A group of octopuses appear as your enemy!");

                }

                o_ptr->timeout = 300 + randint1(150);
                break;
            }

            case ART_FUNDIN:
            {
                msg_print("The iron ball floods the area with goodness...");

                dispel_evil(device_power(p_ptr->lev * 5));
                o_ptr->timeout = randint0(100) + 100;
                break;
            }

            case ART_AESCULAPIUS:
            {
                msg_print("The jo staff glows a deep green...");

                (void)do_res_stat(A_STR);
                (void)do_res_stat(A_INT);
                (void)do_res_stat(A_WIS);
                (void)do_res_stat(A_DEX);
                (void)do_res_stat(A_CON);
                (void)do_res_stat(A_CHR);
                (void)restore_level();
                o_ptr->timeout = 750;
                break;
            }

            case ART_NIGHT:
            {
                msg_print("Your amulet is coverd in pitch-darkness...");
                if (!get_aim_dir(&dir)) return;
                fire_ball(GF_DARK, dir, device_power(250), 4);
                o_ptr->timeout = randint0(150) + 150;
                break;
            }
            case ART_HELL:
            {
                msg_print("Your collar harness is coverd in pitch-darkness...");
                if (!get_aim_dir(&dir)) return;
                fire_ball(GF_DARK, dir, device_power(250), 4);
                o_ptr->timeout = randint0(150) + 150;
                break;
            }
            case ART_SACRED_KNIGHTS:
            {
                msg_print("Your amulet exhibits the truth...");
                if (remove_all_curse())
                {
                    msg_print("You feel as if someone is watching over you.");
                }
                (void)probing();
                break;
            }
            case ART_CHARMED:
            {
                msg_print("Your pendant glows pale...");
                restore_mana();
                o_ptr->timeout = 777;
                break;
            }
        }

        /* Window stuff */
        p_ptr->window |= (PW_INVEN | PW_EQUIP);

        /* Done */
        return;
    }

    if (object_is_smith(o_ptr))
    {
        switch (o_ptr->xtra3-1)
        {
        case ESSENCE_TMP_RES_ACID:
            (void)set_oppose_acid(randint1(20) + 20, FALSE);
            o_ptr->timeout = randint0(50) + 50;
            return;

        case ESSENCE_TMP_RES_ELEC:
            (void)set_oppose_elec(randint1(20) + 20, FALSE);
            o_ptr->timeout = randint0(50) + 50;
            return;

        case ESSENCE_TMP_RES_FIRE:
            (void)set_oppose_fire(randint1(20) + 20, FALSE);
            o_ptr->timeout = randint0(50) + 50;
            return;

        case ESSENCE_TMP_RES_COLD:
            (void)set_oppose_cold(randint1(20) + 20, FALSE);
            o_ptr->timeout = randint0(50) + 50;
            return;

        case TR_IMPACT:
            earthquake(py, px, 5);
            o_ptr->timeout = 100 + randint1(100);
            
            /* Window stuff */
            p_ptr->window |= (PW_INVEN | PW_EQUIP);

            /* Done */
            return;
        }
    }


    if (o_ptr->name2 == EGO_TRUMP)
    {
        if (mut_present(MUT_ASTRAL_GUIDE))
            energy_use = 30;
        teleport_player(100, 0L);
        o_ptr->timeout = 50 + randint1(50);

        /* Window stuff */
        p_ptr->window |= (PW_INVEN | PW_EQUIP);

        /* Done */
        return;
    }


    if (o_ptr->name2 == EGO_LITE_ILLUMINATION)
    {
        if (!o_ptr->xtra4 && ((o_ptr->sval == SV_LITE_TORCH) || (o_ptr->sval == SV_LITE_LANTERN)))
        {
            msg_print("It has no fuel.");
            energy_use = 0;
            return;
        }
        lite_area(damroll(2, 15), 3);
        o_ptr->timeout = randint0(10) + 10;

        /* Window stuff */
        p_ptr->window |= (PW_INVEN | PW_EQUIP);

        return;
    }


    if (o_ptr->name2 == EGO_EARTHQUAKES)
    {
        earthquake(py, px, 5);
        o_ptr->timeout = 100 + randint1(100);

        /* Window stuff */
        p_ptr->window |= (PW_INVEN | PW_EQUIP);

        /* Done */
        return;
    }


    if (o_ptr->name2 == EGO_JUMP)
    {
        if (mut_present(MUT_ASTRAL_GUIDE))
            energy_use = 30;
        teleport_player(10, 0L);
        o_ptr->timeout = 10 + randint1(10);

        /* Window stuff */
        p_ptr->window |= (PW_INVEN | PW_EQUIP);

        /* Done */
        return;
    }

    if (o_ptr->name2 == EGO_DAEMON)
    {
        destroy_area(py, px, 13 + randint0(5), device_power(4 * p_ptr->lev));
        o_ptr->timeout = 100 + randint1(100);
        p_ptr->window |= (PW_INVEN | PW_EQUIP);
        return;
    }


    /* Hack -- Dragon Scale Mail can be activated as well */
    if (o_ptr->tval == TV_DRAG_ARMOR)
    {
        /* Get a direction for breathing (or abort) */
        if (!get_aim_dir(&dir)) return;

        if (music_singing_any()) bard_stop_singing();
        if (hex_spelling_any()) stop_hex_spell_all();

        /* Branch on the sub-type */
        switch (o_ptr->sval)
        {
            case SV_DRAGON_BLUE:
            {
                msg_print("You breathe lightning.");

                fire_ball(GF_ELEC, dir, device_power(100), -2);
                o_ptr->timeout = randint0(15) + 15;
                break;
            }

            case SV_DRAGON_WHITE:
            {
                msg_print("You breathe frost.");

                fire_ball(GF_COLD, dir, device_power(110), -2);
                o_ptr->timeout = randint0(15) + 15;
                break;
            }

            case SV_DRAGON_BLACK:
            {
                msg_print("You breathe acid.");

                fire_ball(GF_ACID, dir, device_power(130), -2);
                o_ptr->timeout = randint0(15) + 15;
                break;
            }

            case SV_DRAGON_GREEN:
            {
                msg_print("You breathe poison gas.");

                fire_ball(GF_POIS, dir, device_power(150), -2);
                o_ptr->timeout = randint0(18) + 18;
                break;
            }

            case SV_DRAGON_RED:
            {
                msg_print("You breathe fire.");

                fire_ball(GF_FIRE, dir, device_power(200), -2);
                o_ptr->timeout = randint0(20) + 20;
                break;
            }

            case SV_DRAGON_MULTIHUED:
            {
                int chance = randint0(5);
                msg_format("You breathe %s.",
                       ((chance == 1) ? "lightning" :
                        ((chance == 2) ? "frost" :
                         ((chance == 3) ? "acid" :
                          ((chance == 4) ? "poison gas" : "fire")))));

                fire_ball(((chance == 1) ? GF_ELEC :
                       ((chance == 2) ? GF_COLD :
                        ((chance == 3) ? GF_ACID :
                         ((chance == 4) ? GF_POIS : GF_FIRE)))),
                      dir, device_power(250), -2);
                o_ptr->timeout = randint0(20) + 20;
                break;
            }

            case SV_DRAGON_BRONZE:
            {
                msg_print("You breathe confusion.");

                fire_ball(GF_CONFUSION, dir, device_power(120), -2);
                o_ptr->timeout = randint0(18) + 18;
                break;
            }

            case SV_DRAGON_GOLD:
            {
                msg_print("You breathe sound.");

                fire_ball(GF_SOUND, dir, device_power(130), -2);
                o_ptr->timeout = randint0(18) + 18;
                break;
            }

            case SV_DRAGON_CHAOS:
            {
                int chance = randint0(2);
                msg_format("You breathe %s.",
                       ((chance == 1 ? "chaos" : "disenchantment")));

                fire_ball((chance == 1 ? GF_CHAOS : GF_DISENCHANT),
                      dir, device_power(220), -2);
                o_ptr->timeout = randint0(20) + 20;
                break;
            }

            case SV_DRAGON_LAW:
            {
                int chance = randint0(2);
                msg_format("You breathe %s.",
                       ((chance == 1 ? "sound" : "shards")));

                fire_ball((chance == 1 ? GF_SOUND : GF_SHARDS),
                      dir, device_power(230), -2);
                o_ptr->timeout = randint0(20) + 20;
                break;
            }

            case SV_DRAGON_BALANCE:
            {
                int chance = randint0(4);
                msg_format("You breathe %s.",
                       ((chance == 1) ? "chaos" :
                        ((chance == 2) ? "disenchantment" :
                         ((chance == 3) ? "sound" : "shards"))));

                fire_ball(((chance == 1) ? GF_CHAOS :
                       ((chance == 2) ? GF_DISENCHANT :
                        ((chance == 3) ? GF_SOUND : GF_SHARDS))),
                      dir, device_power(250), -2);
                o_ptr->timeout = randint0(20) + 20;
                break;
            }

            case SV_DRAGON_SHINING:
            {
                int chance = randint0(2);
                msg_format("You breathe %s.",
                       ((chance == 0 ? "light" : "darkness")));

                fire_ball((chance == 0 ? GF_LITE : GF_DARK), dir, device_power(200), -2);
                o_ptr->timeout = randint0(20) + 20;
                break;
            }

            case SV_DRAGON_POWER:
            {
                msg_print("You breathe the elements.");

                fire_ball(GF_MISSILE, dir, device_power(300), -3);
                o_ptr->timeout = randint0(20) + 20;
                break;
            }
        }

        /* Window stuff */
        p_ptr->window |= (PW_INVEN | PW_EQUIP);

        /* Success */
        return;
    }

    else if (o_ptr->tval == TV_RING)
    {
        if (object_is_ego(o_ptr))
        {
            bool success = TRUE;

            switch (o_ptr->name2)
            {
            case EGO_RING_HERO:
                (void)set_hero(device_power(randint1(25) + 25), FALSE);
                o_ptr->timeout = randint1(100)+100;
                break;
            case EGO_RING_MAGIC_MIS:
                if (!get_aim_dir(&dir)) return;
                fire_bolt(GF_MISSILE, dir, device_power(damroll(2, 6)));
                o_ptr->timeout = 2;
                break;
            case EGO_RING_FIRE_BOLT:
                if (!get_aim_dir(&dir)) return;
                fire_bolt(GF_FIRE, dir, device_power(damroll(9, 8)));
                o_ptr->timeout = randint0(8) + 8;
                break;
            case EGO_RING_COLD_BOLT:
                if (!get_aim_dir(&dir)) return;
                fire_bolt(GF_COLD, dir, device_power(damroll(6, 8)));
                o_ptr->timeout = randint0(7) + 7;
                break;
            case EGO_RING_ELEC_BOLT:
                if (!get_aim_dir(&dir)) return;
                fire_bolt(GF_ELEC, dir, device_power(damroll(4, 8)));
                o_ptr->timeout = randint0(5) + 5;
                break;
            case EGO_RING_ACID_BOLT:
                if (!get_aim_dir(&dir)) return;
                fire_bolt(GF_ACID, dir, device_power(damroll(5, 8)));
                o_ptr->timeout = randint0(6) + 6;
                break;
            case EGO_RING_MANA_BOLT:
                if (!get_aim_dir(&dir)) return;
                fire_bolt(GF_MANA, dir, device_power(120));
                o_ptr->timeout = randint0(120)+120;
                break;
            case EGO_RING_FIRE_BALL:
                if (!get_aim_dir(&dir)) return;
                fire_ball(GF_FIRE, dir, device_power(100), 2);
                o_ptr->timeout = randint0(80) + 80;
                break;
            case EGO_RING_COLD_BALL:
                if (!get_aim_dir(&dir)) return;
                fire_ball(GF_COLD, dir, device_power(100), 2);
                o_ptr->timeout = randint0(80) + 80;
                break;
            case EGO_RING_ELEC_BALL:
                if (!get_aim_dir(&dir)) return;
                fire_ball(GF_ELEC, dir, device_power(100), 2);
                o_ptr->timeout = randint0(80) + 80;
                break;
            case EGO_RING_ACID_BALL:
                if (!get_aim_dir(&dir)) return;
                fire_ball(GF_ACID, dir, device_power(100), 2);
                o_ptr->timeout = randint0(80) + 80;
                break;
            case EGO_RING_MANA_BALL:
                if (!get_aim_dir(&dir)) return;
                fire_ball(GF_MANA, dir, device_power(250), 2);
                o_ptr->timeout = 300;
                break;
            case EGO_RING_DRAGON_F:
                if (!get_aim_dir(&dir)) return;
                fire_ball(GF_FIRE, dir, device_power(200), -2);
                if (o_ptr->sval == SV_RING_FLAMES)
                {
                    (void)set_oppose_fire(device_power(randint1(20) + 20), FALSE);
                    o_ptr->timeout = 200;
                }
                else o_ptr->timeout = 250;
                break;
            case EGO_RING_DRAGON_C:
                if (!get_aim_dir(&dir)) return;
                fire_ball(GF_COLD, dir, device_power(200), -2);
                if (o_ptr->sval == SV_RING_ICE)
                {
                    (void)set_oppose_cold(device_power(randint1(20) + 20), FALSE);
                    o_ptr->timeout = 200;
                }
                else o_ptr->timeout = 250;
                break;
            case EGO_RING_M_DETECT:
                (void)detect_monsters_invis(255);
                (void)detect_monsters_normal(255);
                o_ptr->timeout = 150;
                break;
            case EGO_RING_D_SPEED:
                (void)set_fast(device_power(randint1(30) + 15), FALSE);
                o_ptr->timeout = 100;
                break;
            case EGO_RING_BERSERKER:
                (void)set_shero(device_power(randint1(25) + 25), FALSE);
                o_ptr->timeout = randint0(75)+75;
                break;
            case EGO_RING_TELE_AWAY:
                if (!get_aim_dir(&dir)) return;
                teleport_monster(dir);
                o_ptr->timeout = 150;
                break;
            case EGO_RING_TRUE:
            {
                int v = device_power(randint1(25)+25);
                (void)set_hero(v, FALSE);
                (void)set_blessed(v, FALSE);
                (void)set_oppose_acid(v, FALSE);
                (void)set_oppose_elec(v, FALSE);
                (void)set_oppose_fire(v, FALSE);
                (void)set_oppose_cold(v, FALSE);
                (void)set_oppose_pois(v, FALSE);
                (void)set_ultimate_res(v, FALSE);
                o_ptr->timeout = 777;
                break;
            }
            default:
                success = FALSE;
                break;
            }
            if (success) return;
        }

        /* Get a direction for breathing (or abort) */
        if (!get_aim_dir(&dir)) return;

        switch (o_ptr->sval)
        {
            case SV_RING_ACID:
            {
                fire_ball(GF_ACID, dir, device_power(100), 2);
                (void)set_oppose_acid(device_power(randint1(20) + 20), FALSE);
                o_ptr->timeout = randint0(50) + 50;
                break;
            }

            case SV_RING_ICE:
            {
                fire_ball(GF_COLD, dir, device_power(100), 2);
                (void)set_oppose_cold(device_power(randint1(20) + 20), FALSE);
                o_ptr->timeout = randint0(50) + 50;
                break;
            }

            case SV_RING_FLAMES:
            {
                fire_ball(GF_FIRE, dir, device_power(100), 2);
                (void)set_oppose_fire(device_power(randint1(20) + 20), FALSE);
                o_ptr->timeout = randint0(50) + 50;
                break;
            }

            case SV_RING_ELEC:
            {
                fire_ball(GF_ELEC, dir, device_power(100), 2);
                (void)set_oppose_elec(device_power(randint1(20) + 20), FALSE);
                o_ptr->timeout = randint0(50) + 50;
                break;
            }
        }

        /* Window stuff */
        p_ptr->window |= (PW_INVEN | PW_EQUIP);

        /* Success */
        return;
    }

    else if (o_ptr->tval == TV_AMULET)
    {
        if (object_is_ego(o_ptr))
        {
            switch (o_ptr->name2)
            {
            case EGO_AMU_IDENT:
                if (!ident_spell(NULL)) return;
                o_ptr->timeout = 10;
                break;
            case EGO_AMU_CHARM:
                if (!get_aim_dir(&dir)) return;
                charm_monster(dir, device_power(MAX(20, p_ptr->lev)));
                o_ptr->timeout = 200;
                break;
            case EGO_AMU_JUMP:
                if (mut_present(MUT_ASTRAL_GUIDE))
                    energy_use = 30;
                teleport_player(10, 0L);
                o_ptr->timeout = randint0(10) + 10;
                break;
            case EGO_AMU_TELEPORT:
                teleport_player(100, 0L);
                o_ptr->timeout = randint0(50) + 50;
                break;
            case EGO_AMU_RESISTANCE:
                (void)set_oppose_acid(device_power(randint1(20) + 20), FALSE);
                (void)set_oppose_elec(device_power(randint1(20) + 20), FALSE);
                (void)set_oppose_fire(device_power(randint1(20) + 20), FALSE);
                (void)set_oppose_cold(device_power(randint1(20) + 20), FALSE);
                (void)set_oppose_pois(device_power(randint1(20) + 20), FALSE);
                o_ptr->timeout = randint0(75) + 75;
                break;
            case EGO_AMU_D_DOOR:
                (void)dimension_door(device_power(p_ptr->lev / 2 + 10));
                o_ptr->timeout = 200;
                break;
            case EGO_AMU_RES_FIRE_:
                (void)set_oppose_fire(device_power(randint1(20) + 20), FALSE);
                o_ptr->timeout = randint0(50) + 50;
                break;
            case EGO_AMU_RES_COLD_:
                (void)set_oppose_cold(device_power(randint1(20) + 20), FALSE);
                o_ptr->timeout = randint0(50) + 50;
                break;
            case EGO_AMU_RES_ELEC_:
                (void)set_oppose_elec(device_power(randint1(20) + 20), FALSE);
                o_ptr->timeout = randint0(50) + 50;
                break;
            case EGO_AMU_RES_ACID_:
                (void)set_oppose_acid(device_power(randint1(20) + 20), FALSE);
                o_ptr->timeout = randint0(50) + 50;
                break;
            case EGO_AMU_DETECTION:
                detect_all(DETECT_RAD_DEFAULT);
                o_ptr->timeout = randint0(55)+55;
                break;
            }
        }
        return;
    }

    else if (o_ptr->tval == TV_WHISTLE)
    {
        if (music_singing_any()) bard_stop_singing();
        if (hex_spelling_any()) stop_hex_spell_all();

#if 0
        if (object_is_cursed(o_ptr))
        {
            msg_print("You produce a shrill whistling sound.");
            aggravate_monsters(0);
        }
        else
#endif
        {
            int pet_ctr, i;
            u16b *who;
            int max_pet = 0;
            u16b dummy_why;

            /* Allocate the "who" array */
            C_MAKE(who, max_m_idx, u16b);

            /* Process the monsters (backwards) */
            for (pet_ctr = m_max - 1; pet_ctr >= 1; pet_ctr--)
            {
                if (is_pet(&m_list[pet_ctr]) && (p_ptr->riding != pet_ctr))
                  who[max_pet++] = pet_ctr;
            }

            /* Select the sort method */
            ang_sort_comp = ang_sort_comp_pet;
            ang_sort_swap = ang_sort_swap_hook;

            ang_sort(who, &dummy_why, max_pet);

            /* Process the monsters (backwards) */
            for (i = 0; i < max_pet; i++)
            {
                pet_ctr = who[i];
                teleport_monster_to(pet_ctr, py, px, 100, TELEPORT_PASSIVE);
            }

            /* Free the "who" array */
            C_KILL(who, max_m_idx, u16b);
        }
        o_ptr->timeout = 100+randint1(100);
        return;
    }
    else if (o_ptr->tval == TV_CAPTURE)
    {
        if(!o_ptr->pval)
        {
            bool old_target_pet = target_pet;
            target_pet = TRUE;
            if (!get_aim_dir(&dir))
            {
                target_pet = old_target_pet;
                return;
            }
            target_pet = old_target_pet;

            if(fire_ball(GF_CAPTURE, dir, 0, 0))
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
        return;
    }

    /* Mistake */
    msg_print("Oops.  That object cannot be activated.");

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
    item_tester_hook = item_tester_hook_activate;

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
            if (object_can_activate(o_ptr)) return TRUE;
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

