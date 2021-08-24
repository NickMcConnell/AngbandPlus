
/*
 * File: cmd-obj.c
 * Purpose: Handle objects in various ways
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2007-9 Andrew Sidwell, Chris Carr, Ed Graham, Erik Osheim
 *                       Jeff Greene, Diego Gonzalez
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include <src/npp.h>
#include <src/cmds.h>
#include "src/player_command.h"
#include "src/object_settings.h"

/*
 * Determine if the player can read scrolls.
 */
bool player_can_read(void)
{
    if (p_ptr->timed[TMD_BLIND])
    {
        message(QString("You can't see anything."));
        return FALSE;
    }

    if (!player_can_see_bold(p_ptr->py, p_ptr->px))
    {
        message(QString("You have no light to read by."));
        return FALSE;
    }

    if (p_ptr->timed[TMD_CONFUSED])
    {
        message(QString("You are too confused to read!"));
        return FALSE;
    }


    return TRUE;
}

/*
 * Check to see if the player can use a rod/wand/staff/activatable object.
 */
static int check_devices(object_type *o_ptr)
{
    int fail;
    QString msg;
    QString what;

    what.clear();

    /* Get the right string */
    switch (o_ptr->tval)
    {
        case TV_ROD:   msg = "zap the rod";   break;
        case TV_WAND:  msg = "use the wand";  what = "wand";  break;
        case TV_STAFF: msg = "use the staff"; what = "staff"; break;
        default:       msg = "activate it";  break;
    }

    /* Figure out how hard the item is to use */
    fail = get_use_device_chance(o_ptr);

    /* Roll for usage */
    if (randint1(1000) < fail)
    {
        message(QString("You failed to %1 properly.") .arg(msg));
        return FALSE;
    }

    /* Notice empty staffs */
    if (!what.isEmpty() && o_ptr->pval <= 0)
    {
        message(QString("The %1 has no charges left.") .arg(msg));
        o_ptr->ident |= (IDENT_EMPTY);
        p_ptr->notice |= (PN_COMBINE | PN_REORDER);
        p_ptr->redraw |= (PR_WIN_INVENTORY);

        return FALSE;
    }

    return TRUE;
}

/*
 * Helper function for the get_item or the cmd_use_item functions.
 * Find the object in the inventory or on the floor that is in use by the get_item function.
 * Only one object should be in use at a time to avoid confusion.
 * Returns TRUE/FALSE, and uses a pointer to return the item in use
 */
bool find_object_in_use(int *item)
{
    int i;
    object_type *o_ptr;

    /* First look through the backpack, equipment, and quiver */
    for (i = 0; i < ALL_INVEN_TOTAL; i++)
    {
        /* Get the object */
        o_ptr = &inventory[i];

        /* Skip empty slots */
        if (!o_ptr->k_idx) continue;

        /* Found it */
        if (o_ptr->obj_in_use)
        {
            *item = i;
            return (TRUE);
        }
    }

    /* Now look on the floor */
    for (i = 1; i < o_max; i++)
    {
        /* Get the object */
        o_ptr = &o_list[i];

        /* Skip dead objects */
        if (!o_ptr->k_idx) continue;

        /* Found it */
        if (o_ptr->obj_in_use)
        {
            *item = -i;
            return (TRUE);
        }

    }

    return (FALSE);
}




/*
 * Eat something
 */
static bool eat_food(object_type *o_ptr, bool *ident)
{
    /* Analyze the food */
    switch (o_ptr->sval)
    {
        case SV_FOOD_POISON:
        {
            if (!(p_ptr->state.resist_pois || p_ptr->timed[TMD_OPP_POIS] || p_ptr->state.immune_pois))
            {
                if (inc_timed(TMD_POISONED, rand_int(10) + 10, TRUE))
                {
                    *ident = TRUE;
                }
            }
            break;
        }

        case SV_FOOD_BLINDNESS:
        {
            if (!p_ptr->state.resist_blind)
            {
                if (inc_timed(TMD_BLIND, rand_int(200) + 200, TRUE))
                {
                    *ident = TRUE;
                }
            }
            break;
        }

        case SV_FOOD_PARANOIA:
        {
            if (!p_ptr->state.resist_fear)
            {
                if (inc_timed(TMD_AFRAID, rand_int(10) + 10, TRUE))
                {
                    *ident = TRUE;
                }
            }
            break;
        }

        case SV_FOOD_CONFUSION:
        {
            if (allow_player_confusion())
            {
                if (inc_timed(TMD_CONFUSED, rand_int(10) + 10, TRUE))
                {
                    *ident = TRUE;
                }
            }
            break;
        }

        case SV_FOOD_HALLUCINATION:
        {
            if (!p_ptr->state.resist_chaos)
            {
                if (inc_timed(TMD_IMAGE, rand_int(250) + 250, TRUE))
                {
                    *ident = TRUE;
                }
            }
            break;
        }

        case SV_FOOD_PARALYSIS:
        {
            if (!p_ptr->state.free_act)
            {
                if (inc_timed(TMD_PARALYZED, rand_int(10) + 10, TRUE))
                {
                    *ident = TRUE;
                }
            }
            break;
        }

        case SV_FOOD_WEAKNESS:
        {
            take_hit(damroll(6, 6), "poisonous food");
            (void)do_dec_stat(A_STR);
            *ident = TRUE;
            break;
        }

        case SV_FOOD_SICKNESS:
        {
            take_hit(damroll(6, 6), "poisonous food");
            (void)do_dec_stat(A_CON);
            *ident = TRUE;
            break;
        }

        case SV_FOOD_STUPIDITY:
        {
            take_hit(damroll(8, 8), "poisonous food");
            (void)do_dec_stat(A_INT);
            *ident = TRUE;
            break;
        }

        case SV_FOOD_NAIVETY:
        {
            take_hit(damroll(8, 8), "poisonous food");
            (void)do_dec_stat(A_WIS);
            *ident = TRUE;
            break;
        }

        case SV_FOOD_UNHEALTH:
        {
            take_hit(damroll(10, 10), "poisonous food");
            (void)do_dec_stat(A_CON);
            *ident = TRUE;
            break;
        }

        case SV_FOOD_DISEASE:
        {
            take_hit(damroll(10, 10), "poisonous food");
            (void)do_dec_stat(A_STR);
            *ident = TRUE;
            break;
        }

        case SV_FOOD_CURE_POISON:
        {
            if (clear_timed(TMD_POISONED, TRUE)) *ident = TRUE;
            break;
        }

        case SV_FOOD_CURE_BLINDNESS:
        {
            if (clear_timed(TMD_BLIND, TRUE)) *ident = TRUE;
            break;
        }

        case SV_FOOD_CURE_PARANOIA:
        {
            if (clear_timed(TMD_AFRAID, TRUE)) *ident = TRUE;
            break;
        }

        case SV_FOOD_CURE_CONFUSION:
        {
            if (clear_timed(TMD_CONFUSED, TRUE)) *ident = TRUE;
            break;
        }

        case SV_FOOD_CURE_SERIOUS:
        {
            if (hp_player(damroll(6, 8))) *ident = TRUE;
            break;
        }

        case SV_FOOD_RESTORE_STR:
        {
            if (do_res_stat(A_STR)) *ident = TRUE;
            break;
        }

        case SV_FOOD_RESTORE_CON:
        {
            if (do_res_stat(A_CON)) *ident = TRUE;
            break;
        }

        case SV_FOOD_RESTORING:
        {
            if (do_res_stat(A_STR)) *ident = TRUE;
            if (do_res_stat(A_INT)) *ident = TRUE;
            if (do_res_stat(A_WIS)) *ident = TRUE;
            if (do_res_stat(A_DEX)) *ident = TRUE;
            if (do_res_stat(A_CON)) *ident = TRUE;
            if (do_res_stat(A_CHR)) *ident = TRUE;
            break;
        }

        case SV_FOOD_FIRST_AID:
        {
            if (hp_player(damroll(1, 6))) *ident = TRUE;
            break;
        }

        case SV_FOOD_MINOR_CURES:
        {
            if (hp_player(damroll(2, 6))) *ident = TRUE;
            break;
        }

        case SV_FOOD_LIGHT_CURES:
        {
            if (hp_player(damroll(3, 6))) *ident = TRUE;
            break;
        }

        case SV_FOOD_RESTORATION:
        {
            if (restore_level()) *ident = TRUE;
            if (do_res_stat(A_STR)) *ident = TRUE;
            if (do_res_stat(A_INT)) *ident = TRUE;
            if (do_res_stat(A_WIS)) *ident = TRUE;
            if (do_res_stat(A_DEX)) *ident = TRUE;
            if (do_res_stat(A_CON)) *ident = TRUE;
            if (do_res_stat(A_CHR)) *ident = TRUE;
            break;
        }

        case SV_FOOD_MAJOR_CURES:
        {
            if (hp_player(damroll(3, 12))) *ident = TRUE;
            break;
        }

        case SV_FOOD_RATION:
        case SV_FOOD_FINE_MUSH:
        case SV_FOOD_SLIME_MOLD:
        {
            message(QString("That tastes good."));
            *ident = TRUE;
            break;
        }

        case SV_FOOD_WAYBREAD:
        {
            message(QString("That tastes good."));
            (void)clear_timed(TMD_POISONED, TRUE);
            (void)hp_player(damroll(4, 8));
            *ident = TRUE;
            break;
        }

    }

    /* Food can feed the player */
    (void)set_food(p_ptr->food + o_ptr->pval);

    return (TRUE);
}


/*
 * Quaff a potion
 */
static bool quaff_potion(object_type *o_ptr, bool *ident)
{
    /* Analyze the potion */
    switch (o_ptr->sval)
    {
        case SV_POTION_WATER:
        case SV_POTION_APPLE_JUICE:
        case SV_POTION_SLIME_MOLD:
        {
            message(QString("You feel less thirsty."));
            *ident = TRUE;
            break;
        }

        case SV_POTION_SLOWNESS:
        {
            if (inc_timed(TMD_SLOW, randint(25) + 15, TRUE)) *ident = TRUE;
            break;
        }

        case SV_POTION_SALT_WATER:
        {
            message(QString("The potion makes you vomit!"));
            (void)set_food(PY_FOOD_STARVE - 1);
            (void)clear_timed(TMD_POISONED, TRUE);
            (void)inc_timed(TMD_PARALYZED, 4, TRUE);
            *ident = TRUE;
            break;
        }

        case SV_POTION_POISON:
        {
            if (!(p_ptr->state.resist_pois || p_ptr->timed[TMD_OPP_POIS] || p_ptr->state.immune_pois))
            {
                if (inc_timed(TMD_POISONED, rand_int(15) + 10, TRUE))
                {
                    *ident = TRUE;
                }
            }
            break;
        }

        case SV_POTION_BLINDNESS:
        {
            if (!p_ptr->state.resist_blind)
            {
                if (inc_timed(TMD_BLIND, rand_int(100) + 100, TRUE))
                {
                    *ident = TRUE;
                }
            }
            break;
        }

        case SV_POTION_CONFUSION:
        {
            if (allow_player_confusion())
            {
                if (inc_timed(TMD_CONFUSED, rand_int(20) + 15, TRUE))
                {
                    *ident = TRUE;
                }
            }
            break;
        }

        case SV_POTION_SLEEP:
        {
            if (!p_ptr->state.free_act)
            {
                if (inc_timed(TMD_PARALYZED, rand_int(4) + 4, TRUE))
                {
                    *ident = TRUE;
                }
            }
            break;
        }

        case SV_POTION_LOSE_MEMORIES:
        {
            if (!p_ptr->state.hold_life && (p_ptr->exp > 0))
            {
                message(QString("You feel your memories fade."));
                lose_exp(p_ptr->exp / 4);
                *ident = TRUE;
            }
            break;
        }

        case SV_POTION_DRAIN_MANA:
        {
            if (p_ptr->csp)
            {
                p_ptr->csp /= 2;
                message(QString("Your feel your head cloud up."));
                p_ptr->redraw |= (PR_SIDEBAR_PL);
                *ident = TRUE;
            }
            break;
        }

        case SV_POTION_RUINATION:
        {
            message(QString("Your nerves and muscles feel weak and lifeless!"));
            take_hit(damroll(10, 10), "a potion of Ruination");
            (void)dec_stat(A_DEX, 25, TRUE);
            (void)dec_stat(A_WIS, 25, TRUE);
            (void)dec_stat(A_CON, 25, TRUE);
            (void)dec_stat(A_STR, 25, TRUE);
            (void)dec_stat(A_CHR, 25, TRUE);
            (void)dec_stat(A_INT, 25, TRUE);
            *ident = TRUE;
            break;
        }

        case SV_POTION_DEC_STR:
        {
            if (do_dec_stat(A_STR)) *ident = TRUE;
            break;
        }

        case SV_POTION_DEC_INT:
        {
            if (do_dec_stat(A_INT)) *ident = TRUE;
            break;
        }

        case SV_POTION_DEC_WIS:
        {
            if (do_dec_stat(A_WIS)) *ident = TRUE;
            break;
        }

        case SV_POTION_DEC_DEX:
        {
            if (do_dec_stat(A_DEX)) *ident = TRUE;
            break;
        }

        case SV_POTION_DEC_CON:
        {
            if (do_dec_stat(A_CON)) *ident = TRUE;
            break;
        }

        case SV_POTION_DEC_CHR:
        {
            if (do_dec_stat(A_CHR)) *ident = TRUE;
            break;
        }

        case SV_POTION_DETONATIONS:
        {
            message(QString("Massive explosions rupture your body!"));
            take_hit(damroll(50, 20), "a potion of Detonation");
            (void)set_stun(p_ptr->timed[TMD_STUN] + 75);
            (void)set_cut(p_ptr->timed[TMD_CUT] + 5000);
            *ident = TRUE;
            break;
        }

        case SV_POTION_DEATH:
        {
            message(QString("A feeling of Death flows through your body."));
            take_hit(5000, "a potion of Death");
            *ident = TRUE;
            break;
        }

        case SV_POTION_INFRAVISION:
        {
            if (inc_timed(TMD_SINFRA, 100 + randint(100), TRUE))
            {
                *ident = TRUE;
            }
            break;
        }

        case SV_POTION_DETECT_INVIS:
        {
            if (inc_timed(TMD_SINVIS, 12 + randint(12), TRUE))
            {
                *ident = TRUE;
            }
            break;
        }

        case SV_POTION_SLOW_POISON:
        {
            if (dec_timed(TMD_POISONED, p_ptr->timed[TMD_POISONED] / 2 + 1, TRUE)) *ident = TRUE;
            break;
        }

        case SV_POTION_CURE_POISON:
        {
            if (clear_timed(TMD_POISONED, TRUE)) *ident = TRUE;
            break;
        }

        case SV_POTION_BOLDNESS:
        {
            if (clear_timed(TMD_AFRAID, TRUE)) *ident = TRUE;
            break;
        }

        case SV_POTION_SPEED:
        {
            if (!p_ptr->timed[TMD_FAST])
            {
                if (set_timed(TMD_FAST, randint(25) + 15, TRUE)) *ident = TRUE;
            }
            else
            {
                (void)inc_timed(TMD_FAST, 5, TRUE);
            }
            break;
        }

        case SV_POTION_RESIST_HEAT:
        {
            if (inc_timed(TMD_OPP_FIRE, randint(10) + 10, TRUE))
            {
                *ident = TRUE;
            }
            break;
        }

        case SV_POTION_RESIST_COLD:
        {
            if (inc_timed(TMD_OPP_COLD, randint(10) + 10, TRUE))
            {
                *ident = TRUE;
            }
            break;
        }

        case SV_POTION_HEROISM:
        {
            if (game_mode == GAME_NPPMORIA)
            {
                if (!p_ptr->timed[TMD_HERO])
                {
                    p_ptr->chp +=10;
                    p_ptr->mhp +=10;
                }
            }
            if (hp_player(10)) *ident = TRUE;
            if (clear_timed(TMD_AFRAID, TRUE)) *ident = TRUE;
            if (inc_timed(TMD_HERO, randint(25) + 25, TRUE)) *ident = TRUE;

            break;
        }

        case SV_POTION_BERSERK_STRENGTH:
        {
            if (game_mode == GAME_NPPMORIA)
            {
                if (!p_ptr->timed[TMD_BERSERK])
                {
                    p_ptr->mhp +=15;
                    p_ptr->chp +=15;
                }
            }
            if (hp_player(30)) *ident = TRUE;
            if (clear_timed(TMD_AFRAID, TRUE)) *ident = TRUE;
            if (inc_timed(TMD_BERSERK, randint(25) + 25, TRUE)) *ident = TRUE;
            break;
        }

        case SV_POTION_CURE_LIGHT:
        {
            int dam_dice = 3;
            int dam_side = 8;

            if (game_mode == GAME_NPPMORIA)
            {
                dam_dice = 2;
                dam_side = 7;
            }

            if (hp_player(damroll(dam_dice, dam_side))) *ident = TRUE;
            if (clear_timed(TMD_BLIND, TRUE)) *ident = TRUE;
            if (set_cut(p_ptr->timed[TMD_CUT] - 10)) *ident = TRUE;
            break;
        }

        case SV_POTION_CURE_SERIOUS:
        {
            int dam_dice = 5;
            int dam_side = 10;

            if (game_mode == GAME_NPPMORIA)
            {
                dam_dice = 4;
                dam_side = 7;
            }

            if (hp_player(damroll(dam_dice, dam_side))) *ident = TRUE;
            if (clear_timed(TMD_BLIND, TRUE)) *ident = TRUE;
            if (clear_timed(TMD_CONFUSED, TRUE)) *ident = TRUE;
            if (set_cut((p_ptr->timed[TMD_CUT] / 2) - 50)) *ident = TRUE;
            break;
        }

        case SV_POTION_CURE_CRITICAL:
        {
            int dam_dice = 8;
            int dam_side = 10;

            if (game_mode == GAME_NPPMORIA)
            {
                dam_dice = 6;
                dam_side = 7;
            }

            if (hp_player(damroll(dam_dice, dam_side))) *ident = TRUE;
            if (clear_timed(TMD_BLIND, TRUE)) *ident = TRUE;
            if (clear_timed(TMD_CONFUSED, TRUE)) *ident = TRUE;
            if (clear_timed(TMD_POISONED, TRUE)) *ident = TRUE;
            if (set_stun(0)) *ident = TRUE;
            if (set_cut(0)) *ident = TRUE;
            break;
        }

        case SV_POTION_HEALING:
        {
            int heal = 325;

            if (game_mode == GAME_NPPMORIA)
            {
                heal = 1000;
            }

            if (hp_player(heal)) *ident = TRUE;
            if (clear_timed(TMD_BLIND, TRUE)) *ident = TRUE;
            if (clear_timed(TMD_CONFUSED, TRUE)) *ident = TRUE;
            if (clear_timed(TMD_POISONED, TRUE)) *ident = TRUE;
            if (set_stun(0)) *ident = TRUE;
            if (set_cut(0)) *ident = TRUE;
            break;
        }

        case SV_POTION_STAR_HEALING:
        {
            if (hp_player(1500)) *ident = TRUE;
            if (clear_timed(TMD_BLIND, TRUE)) *ident = TRUE;
            if (clear_timed(TMD_CONFUSED, TRUE)) *ident = TRUE;
            if (clear_timed(TMD_POISONED, TRUE)) *ident = TRUE;
            if (set_stun(0)) *ident = TRUE;
            if (set_cut(0)) *ident = TRUE;
            break;
        }

        case SV_POTION_LIFE:
        {
            message(QString("You feel life flow through your body!"));
            restore_level();
            (void)clear_timed(TMD_POISONED, TRUE);
            (void)clear_timed(TMD_BLIND, TRUE);
            (void)clear_timed(TMD_CONFUSED, TRUE);
            (void)clear_timed(TMD_IMAGE, TRUE);
            (void)set_stun(0);
            (void)set_cut(0);
            (void)do_res_stat(A_STR);
            (void)do_res_stat(A_CON);
            (void)do_res_stat(A_DEX);
            (void)do_res_stat(A_WIS);
            (void)do_res_stat(A_INT);
            (void)do_res_stat(A_CHR);

            /* Recalculate max. hitpoints */
            update_stuff();

            hp_player(5000);

            *ident = TRUE;
            break;
        }

        case SV_POTION_RESTORE_MANA:
        {
            if (p_ptr->csp < p_ptr->msp)
            {
                p_ptr->csp = p_ptr->msp;
                p_ptr->csp_frac = 0;
                message(QString("Your feel your head clear."));
                p_ptr->redraw |= (PR_SIDEBAR_PL);
                *ident = TRUE;
            }
            break;
        }

        case SV_POTION_RESTORE_EXP:
        {
            if (restore_level()) *ident = TRUE;
            break;
        }

        case SV_POTION_RES_STR:
        {
            if (do_res_stat(A_STR)) *ident = TRUE;
            break;
        }

        case SV_POTION_RES_INT:
        {
            if (do_res_stat(A_INT)) *ident = TRUE;
            break;
        }

        case SV_POTION_RES_WIS:
        {
            if (do_res_stat(A_WIS)) *ident = TRUE;
            break;
        }

        case SV_POTION_RES_DEX:
        {
            if (do_res_stat(A_DEX)) *ident = TRUE;
            break;
        }

        case SV_POTION_RES_CON:
        {
            if (do_res_stat(A_CON)) *ident = TRUE;
            break;
        }

        case SV_POTION_RES_CHR:
        {
            if (do_res_stat(A_CHR)) *ident = TRUE;
            break;
        }

        case SV_POTION_INC_STR:
        {
            if (do_inc_stat(A_STR)) *ident = TRUE;
            break;
        }

        case SV_POTION_INC_INT:
        {
            if (do_inc_stat(A_INT)) *ident = TRUE;
            break;
        }

        case SV_POTION_INC_WIS:
        {
            if (do_inc_stat(A_WIS)) *ident = TRUE;
            break;
        }

        case SV_POTION_INC_DEX:
        {
            if (do_inc_stat(A_DEX)) *ident = TRUE;
            break;
        }

        case SV_POTION_INC_CON:
        {
            if (do_inc_stat(A_CON)) *ident = TRUE;
            break;
        }

        case SV_POTION_INC_CHR:
        {
            if (do_inc_stat(A_CHR)) *ident = TRUE;
            break;
        }

        case SV_POTION_AUGMENTATION:
        {
            if (do_inc_stat(A_STR)) *ident = TRUE;
            if (do_inc_stat(A_INT)) *ident = TRUE;
            if (do_inc_stat(A_WIS)) *ident = TRUE;
            if (do_inc_stat(A_DEX)) *ident = TRUE;
            if (do_inc_stat(A_CON)) *ident = TRUE;
            if (do_inc_stat(A_CHR)) *ident = TRUE;
            break;
        }

        case SV_POTION_ENLIGHTENMENT:
        {
            message(QString("An image of your surroundings forms in your mind..."));
            wiz_light();
            *ident = TRUE;
            break;
        }

        case SV_POTION_STAR_ENLIGHTENMENT:
        {
            message(QString("You begin to feel more enlightened..."));
            wiz_light();
            (void)do_inc_stat(A_INT);
            (void)do_inc_stat(A_WIS);
            detect(DETECT_RADIUS, DETECT_ENLIGHTENMENT);
            identify_and_squelch_pack();
            self_knowledge();
            *ident = TRUE;
            break;
        }

        case SV_POTION_SELF_KNOWLEDGE:
        {
            message(QString("You begin to know yourself a little better..."));
            self_knowledge();
            *ident = TRUE;
            break;
        }

        case SV_POTION_EXPERIENCE:
        {
            if (p_ptr->exp < PY_MAX_EXP)
            {
                s32b ee = (p_ptr->exp / 2) + 10;
                if (ee > 100000L) ee = 100000L;
                message(QString("You feel more experienced."));
                gain_exp(ee);
                *ident = TRUE;
            }
            break;
        }

        case SV_POTION_RESIST_ACID:
        {
            if (inc_timed(TMD_OPP_ACID, randint(10) + 10, TRUE))
            {
                *ident = TRUE;
            }
            break;
        }

        case SV_POTION_RESIST_ELECTRICITY:
        {
            if (inc_timed(TMD_OPP_ELEC, randint(10) + 10, TRUE))
            {
                *ident = TRUE;
            }
            break;
        }

        case SV_POTION_RESIST_POISON:
        {
            if (inc_timed(TMD_OPP_POIS, randint(15) + 15, TRUE))
            {
                *ident = TRUE;
            }
            break;
        }

        case SV_POTION_RESISTANCE:
        {
            int act_time = randint(30) + 30;
            if(inc_timed(TMD_OPP_ACID, act_time, TRUE)) *ident = TRUE;
            if(inc_timed(TMD_OPP_ELEC, act_time, TRUE)) *ident = TRUE;
            if(inc_timed(TMD_OPP_FIRE, act_time, TRUE)) *ident = TRUE;
            if(inc_timed(TMD_OPP_COLD, act_time, TRUE)) *ident = TRUE;
            if(inc_timed(TMD_OPP_POIS, act_time, TRUE)) *ident = TRUE;
            break;
        }

        case SV_POTION_INVULNERABILITY:
        {
            if (p_ptr->timed[TMD_INVULN])
            {
                if (inc_timed(TMD_INVULN, 5, TRUE)) *ident = TRUE;
            }
            else if (inc_timed(TMD_INVULN, randint(10) + 10, TRUE)) *ident = TRUE;
            break;
        }

    }

    /*
     * Some potions can feed the player
     * Hack - but they can't gorge the player.
     */
    if (o_ptr->pval)
    {
        int new_food = p_ptr->food + o_ptr->pval;

        if (new_food >= PY_FOOD_MAX)
        {
            new_food = PY_FOOD_MAX - 1;
        }

        if (new_food > p_ptr->food)(void)set_food(new_food);
    }

    return (TRUE);
}


/*
 * Read a scroll
 */
static bool read_scroll(object_type *o_ptr, bool *ident)
{
    int py = p_ptr->py;
    int px = p_ptr->px;

    int k;

    bool used_up = TRUE;

    /* Analyze the scroll */
    switch (o_ptr->sval)
    {
        case SV_SCROLL_DARKNESS:
        {
            if (!p_ptr->state.resist_blind)
            {
                (void)inc_timed(TMD_BLIND, 3 + randint(5), TRUE);
            }
            if (unlight_area(10, 3)) *ident = TRUE;
            break;
        }

        case SV_SCROLL_AGGRAVATE_MONSTER:
        {
            message(QString("There is a high pitched humming noise."));
            aggravate_monsters(SOURCE_PLAYER);
            *ident = TRUE;
            break;
        }

        case SV_SCROLL_CURSE_ARMOR:
        {
            if (curse_armor()) *ident = TRUE;
            break;
        }

        case SV_SCROLL_CURSE_WEAPON:
        {
            if (curse_weapon()) *ident = TRUE;
            break;
        }

        case SV_SCROLL_SUMMON_MONSTER:
        {
            sound(MSG_SUM_MONSTER);
            for (k = 0; k < randint(3); k++)
            {
                if (summon_specific(py, px, p_ptr->depth, 0, MPLACE_OVERRIDE))
                {
                    *ident = TRUE;
                }
            }
            break;
        }

        case SV_SCROLL_SUMMON_UNDEAD:
        {
            sound(MSG_SUM_UNDEAD);
            for (k = 0; k < randint(3); k++)
            {
                if (summon_specific(py, px, p_ptr->depth, SUMMON_UNDEAD, MPLACE_OVERRIDE))
                {
                    *ident = TRUE;
                }
            }
            break;
        }

        case SV_SCROLL_SUMMON_UNIQUE:
        {

            if (summon_specific(py, px, p_ptr->depth, SUMMON_UNIQUE, MPLACE_OVERRIDE))
            {
                *ident = TRUE;
            }

            break;
        }

        case SV_SCROLL_TRAP_CREATION:
        {
            if (trap_creation(SOURCE_OTHER)) *ident = TRUE;
            break;
        }

        case SV_SCROLL_PHASE_DOOR:
        {
            teleport_player(10, FALSE);
            *ident = TRUE;
            break;
        }

        case SV_SCROLL_TELEPORT:
        {
            teleport_player(100, FALSE);
            *ident = TRUE;
            break;
        }

        case SV_SCROLL_TELEPORT_LEVEL:
        {
            if (!teleport_player_level(SOURCE_PLAYER)) used_up = FALSE;
            *ident = TRUE;
            break;
        }

        case SV_SCROLL_WORD_OF_RECALL:
        {
            if (!set_recall()) used_up = FALSE;
            *ident = TRUE;
            break;
        }

        case SV_SCROLL_IDENTIFY:
        {
            *ident = TRUE;
            if (!ident_spell()) used_up = FALSE;
            break;
        }

        case SV_SCROLL_STAR_IDENTIFY:
        {
            *ident = TRUE;
            if (!identify_fully()) used_up = FALSE;
            break;
        }

        case SV_SCROLL_REMOVE_CURSE:
        {
            if (remove_curse(FALSE))
            {
                message(QString("You feel as if someone is watching over you."));
                *ident = TRUE;
            }
            break;
        }

        case SV_SCROLL_STAR_REMOVE_CURSE:
        {
            remove_curse(TRUE);
            *ident = TRUE;
            break;
        }

        case SV_SCROLL_ENCHANT_ARMOR:
        {
            *ident = TRUE;
            if (!enchant_spell(0, 0, 1)) used_up = FALSE;
            break;
        }

        case SV_SCROLL_ENCHANT_WEAPON_TO_HIT:
        {
            if (!enchant_spell(1, 0, 0)) used_up = FALSE;
            *ident = TRUE;
            break;
        }

        case SV_SCROLL_ENCHANT_WEAPON_TO_DAM:
        {
            if (!enchant_spell(0, 1, 0)) used_up = FALSE;
            *ident = TRUE;
            break;
        }

        case SV_SCROLL_STAR_ENCHANT_ARMOR:
        {
            if (!enchant_spell(0, 0, randint(3) + 2)) used_up = FALSE;
            *ident = TRUE;
            break;
        }

        case SV_SCROLL_STAR_ENCHANT_WEAPON:
        {
            if (!enchant_spell(randint(3), randint(3), 0)) used_up = FALSE;
            *ident = TRUE;
            break;
        }

        case SV_SCROLL_RECHARGING:
        {
            if (!recharge(60, FALSE, 50)) used_up = FALSE;
            *ident = TRUE;
            break;
        }

        case SV_SCROLL_STAR_RECHARGING:
        {
            if (!recharge(150, TRUE, 75)) used_up = FALSE;
            *ident = TRUE;
            break;
        }

        case SV_SCROLL_LIGHT:
        {
            int dam_dice = 2;

            if (game_mode == GAME_NPPMORIA) dam_dice = 1;

            if (light_area(damroll(dam_dice, 8), 2)) *ident = TRUE;
            break;
        }

        case SV_SCROLL_MAPPING:
        {
            detect(DETECT_RADIUS + 5, DETECT_MAP);
            *ident = TRUE;
            break;
        }

        case SV_SCROLL_DETECT_GOLD:
        {
            if (detect(DETECT_RADIUS, DETECT_ALL_TREASURE)) *ident = TRUE;
            break;
        }

        case SV_SCROLL_DETECT_ITEM:
        {
            if (detect(DETECT_RADIUS, DETECT_OBJECTS)) *ident = TRUE;
            break;
        }

        case SV_SCROLL_DETECT_TRAP:
        {
            if (detect(DETECT_RADIUS, DETECT_TRAPS)) *ident = TRUE;
            break;
        }

        case SV_SCROLL_DETECT_DOOR:
        {
            if (detect(DETECT_RADIUS, DETECT_DOORS_STAIRS)) *ident = TRUE;
            break;
        }

        case SV_SCROLL_DETECT_INVIS:
        {
            if (detect(DETECT_RADIUS, DETECT_INVISIBLE)) *ident = TRUE;
            break;
        }

        case SV_SCROLL_SATISFY_HUNGER:
        {
            if (set_food(PY_FOOD_MAX - 1)) *ident = TRUE;
            break;
        }

        case SV_SCROLL_BLESSING:
        {
            if (inc_timed(TMD_BLESSED, randint(12) + 6, TRUE)) *ident = TRUE;
            break;
        }

        case SV_SCROLL_HOLY_CHANT:
        {
            if (inc_timed(TMD_BLESSED, randint(24) + 12, TRUE)) *ident = TRUE;
            break;
        }

        case SV_SCROLL_HOLY_PRAYER:
        {
            if (inc_timed(TMD_BLESSED, randint(48) + 24, TRUE)) *ident = TRUE;
            break;
        }

        case SV_SCROLL_MONSTER_CONFUSION:
        {
            if (p_ptr->confusing == 0)
            {
                message(QString("Your hands begin to glow."));
                p_ptr->confusing = TRUE;
                *ident = TRUE;
            }
            break;
        }

        case SV_SCROLL_PROTECTION_FROM_EVIL:
        {
            k = 3 * p_ptr->lev;
            if (inc_timed(TMD_PROTEVIL, randint(25) + k, TRUE)) *ident = TRUE;
            break;
        }

        case SV_SCROLL_RUNE_OF_PROTECTION:
        {
            if (!warding_glyph()) used_up = FALSE;
            *ident = TRUE;
            break;
        }

        case SV_SCROLL_TRAP_DOOR_DESTRUCTION:
        {
            if (destroy_doors_touch()) *ident = TRUE;
            break;
        }

        case SV_SCROLL_STAR_DESTRUCTION:
        {
            destroy_area(py, px, 15);
            *ident = TRUE;
            break;
        }

        case SV_SCROLL_CREATE_MONSTER_TRAP:
        {
            if (make_monster_trap()) *ident = TRUE;
            else used_up = FALSE;
            break;
        }

        case SV_SCROLL_DISPEL_UNDEAD:
        {
            if (dispel_undead(60)) *ident = TRUE;
            break;
        }

        case SV_SCROLL_CREATE_RANDART:
        {
            object_type *o_ptr;

            int item;

            /* artifact power is based on depth */
            int randart_power = 10 + p_ptr->depth;

            /* Get an item */
            QString q = "Choose an item to be made into an artifact. ";
            QString s = "You have no eligible item.";
            /* Only accept legal items. */
            item_tester_hook = item_tester_hook_randart;

            if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) break;

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

            if ((birth_no_artifacts) || (birth_no_xtra_artifacts))
            {
                message(QString("Nothing happens."));
                break;
            }

            /* occasional power boost */
            while (one_in_(25)) randart_power += 25;

            if (make_one_randart(o_ptr, randart_power, FALSE))
            {
                *ident = TRUE;
                used_up = TRUE;

                /* Mark the item as fully known */
                o_ptr->mark_fully_known(TRUE);

                object_history(o_ptr, ORIGIN_ACQUIRE, 0);

                /* Update the flags */
                o_ptr->update_object_flags();

                /* Let the player know what they just got */
                object_info_screen(o_ptr);

            }
            else message(QString("The Artifact creation failed"));
            break;
        }

        case SV_SCROLL_BANISHMENT:
        {
            if (!banishment()) used_up = FALSE;
            *ident = TRUE;
            break;
        }

        case SV_SCROLL_MASS_BANISHMENT:
        {
            (void)mass_banishment();
            *ident = TRUE;
            break;
        }

        case SV_SCROLL_ACQUIREMENT:
        {
            acquirement(py, px, 1, TRUE);
            *ident = TRUE;
            break;
        }

        case SV_SCROLL_STAR_ACQUIREMENT:
        {
            acquirement(py, px, randint(2) + 1, TRUE);
            *ident = TRUE;
            break;
        }
        case SV_SCROLL_CREATE_FOOD:
        {
            create_food();
            *ident = TRUE;
            break;
        }
        case SV_SCROLL_CREATE_DOORS:
        {
            door_creation();
            *ident = TRUE;
            break;
        }
        case SV_SCROLL_SLEEP_MONSTER:
        {
            sleep_monsters_touch();
            *ident = TRUE;
            break;
        }
    }

    return (used_up);
}


/*
 * Use a staff
 */
static bool use_staff(object_type *o_ptr, bool *ident)
{
    int py = p_ptr->py;
    int px = p_ptr->px;

    int k;

    bool use_charge = TRUE;

    /* Analyze the staff */
    switch (o_ptr->sval)
    {
        case SV_STAFF_DARKNESS:
        {
            if (!p_ptr->state.resist_blind)
            {
                if (inc_timed(TMD_BLIND, 3 + randint(5), TRUE)) *ident = TRUE;
            }
            if (unlight_area(10, 3)) *ident = TRUE;
            break;
        }

        case SV_STAFF_SLOWNESS:
        {
            if (inc_timed(TMD_SLOW, randint(30) + 15, TRUE)) *ident = TRUE;
            break;
        }

        case SV_STAFF_HASTE_MONSTERS:
        {
            if (speed_monsters()) *ident = TRUE;
            break;
        }

        case SV_STAFF_SUMMONING:
        {
            sound(MSG_SUM_MONSTER);
            for (k = 0; k < randint(4); k++)
            {
                if (summon_specific(py, px, p_ptr->depth, 0, MPLACE_OVERRIDE))
                {
                    *ident = TRUE;
                }
            }
            break;
        }

        case SV_STAFF_TELEPORTATION:
        {
            teleport_player(100, FALSE);
            *ident = TRUE;
            break;
        }

        case SV_STAFF_IDENTIFY:
        {
            if (!ident_spell()) use_charge = FALSE;
            *ident = TRUE;
            break;
        }

        case SV_STAFF_STARLIGHT:
        {
            int dam_dice = 6; /* (game_mode == GAME_NPPANGBAND) */
            if (game_mode == GAME_NPPMORIA) dam_dice = 2;

            if (!p_ptr->timed[TMD_BLIND])
            {
                message(QString("The end of the staff glows brightly..."));
            }
            for (k = 0; k < 8; k++) light_line(ddd[k], damroll(dam_dice, 8));
            *ident = TRUE;
            break;
        }

        case SV_STAFF_LIGHT:
        {
            if (light_area(damroll(2, 8), 2)) *ident = TRUE;
            break;
        }

        case SV_STAFF_MAPPING:
        {
            detect(DETECT_RADIUS + 5, DETECT_MAP);
            *ident = TRUE;
            break;
        }

        case SV_STAFF_DETECT_GOLD:
        {
            if (detect(DETECT_RADIUS, DETECT_ALL_TREASURE)) *ident = TRUE;
            break;
        }

        case SV_STAFF_DETECT_ITEM:
        {
            if (detect(DETECT_RADIUS, DETECT_OBJECTS)) *ident = TRUE;
            break;
        }

        case SV_STAFF_DETECT_TRAP:
        {
            if (detect(DETECT_RADIUS, DETECT_TRAPS)) *ident = TRUE;
            break;
        }

        case SV_STAFF_DETECT_DOOR:
        {
            if (detect(DETECT_RADIUS, DETECT_DOORS_STAIRS)) *ident = TRUE;
            break;
        }

        case SV_STAFF_DETECT_INVIS:
        {
            if (detect(DETECT_RADIUS, DETECT_INVISIBLE)) *ident = TRUE;
            break;
        }

        case SV_STAFF_DETECT_EVIL:
        {
            if (detect(DETECT_RADIUS, DETECT_EVIL)) *ident = TRUE;
            break;
        }

        case SV_STAFF_CURE_LIGHT:
        {
            int dam_dice = 2;  /* (game_mode == GAME_NPPANGBAND) */
            int dam_side = 10;

            if (game_mode == GAME_NPPMORIA)
            {
                dam_dice = 1;
                dam_side = 8;
            }

            if (hp_player(damroll(dam_dice, dam_side))) *ident = TRUE;
            break;
        }

        case SV_STAFF_CURING:
        {
            if (clear_timed(TMD_BLIND, TRUE)) *ident = TRUE;
            if (clear_timed(TMD_POISONED, TRUE)) *ident = TRUE;
            if (clear_timed(TMD_CONFUSED, TRUE)) *ident = TRUE;
            if (set_stun(0)) *ident = TRUE;
            if (set_cut(0)) *ident = TRUE;
            break;
        }

        case SV_STAFF_HEALING:
        {
            if (hp_player(325)) *ident = TRUE;
            if (set_stun(0)) *ident = TRUE;
            if (set_cut(0)) *ident = TRUE;
            break;
        }

        case SV_STAFF_THE_MAGI:
        {
            if (do_res_stat(A_INT)) *ident = TRUE;
            if (p_ptr->csp < p_ptr->msp)
            {
                p_ptr->csp = p_ptr->msp;
                p_ptr->csp_frac = 0;
                *ident = TRUE;
                message(QString("Your feel your head clear."));
                p_ptr->redraw |= (PR_SIDEBAR_PL);
            }
            break;
        }

        case SV_STAFF_SLEEP_MONSTERS:
        {
            if (game_mode == GAME_NPPMORIA)
            {
                if (sleep_monsters(500)) *ident = TRUE;
            }
            else if (sleep_monsters(damroll(3, p_ptr->lev))) *ident = TRUE; /* (game_mode == GAME_NPPANGBAND) */
            break;
        }

        case SV_STAFF_SLOW_MONSTERS:
        {
            if (slow_monsters(damroll (2, p_ptr->lev))) *ident = TRUE;
            break;
        }

        case SV_STAFF_SPEED:
        {
            if (!p_ptr->timed[TMD_FAST])
            {
                if (set_timed(TMD_FAST, randint(30) + 15, TRUE)) *ident = TRUE;
            }
            else
            {
                (void)inc_timed(TMD_FAST, 5, FALSE);
            }
            break;
        }

        case SV_STAFF_PROBING:
        {
            probing();
            *ident = TRUE;
            break;
        }

        case SV_STAFF_DISPEL_EVIL:
        {
            if (dispel_evil(60)) *ident = TRUE;
            break;
        }

        case SV_STAFF_MASS_POLYMORPH:
        {
            if (mass_polymorph()) *ident = TRUE;
            break;
        }

        case SV_STAFF_REMOVE_CURSE:
        {
            if (remove_curse(FALSE)) *ident = TRUE;
            break;
        }

        case SV_STAFF_POWER:
        {
            if (dispel_monsters(120)) *ident = TRUE;
            break;
        }

        case SV_STAFF_HOLINESS:
        {
            if (dispel_evil(120)) *ident = TRUE;
            k = 3 * p_ptr->lev;
            if (inc_timed(TMD_PROTEVIL, randint(25) + k, TRUE)) *ident = TRUE;
            if (clear_timed(TMD_POISONED, TRUE)) *ident = TRUE;
            if (clear_timed(TMD_AFRAID, TRUE)) *ident = TRUE;
            if (hp_player(75)) *ident = TRUE;
            if (set_stun(0)) *ident = TRUE;
            if (set_cut(0)) *ident = TRUE;
            break;
        }

        case SV_STAFF_BANISHMENT:
        {
            if (!banishment()) use_charge = FALSE;
            *ident = TRUE;
            break;
        }

        case SV_STAFF_EARTHQUAKES:
        {
            earthquake(py, px, 10, FALSE);
            *ident = TRUE;
            break;
        }

        case SV_STAFF_DESTRUCTION:
        {
            destroy_area(py, px, 15);
            *ident = TRUE;
            break;
        }

        case SV_STAFF_MASS_IDENTIFY:
        {
            mass_identify(3);
            *ident = TRUE;
            break;
        }

    }

    return (use_charge);
}


/*
 * Aim a wand
 */
static bool aim_wand(object_type *o_ptr, bool *ident, int dir)
{
    int sval;


    /* Not identified yet */
    *ident = FALSE;

    /* Sound */
    /* TODO: Create wand sound?  Do the individual effects have sounds? */
    /* sound(MSG_ZAP_ROD); */

    /* XXX Hack -- Extract the "sval" effect */
    sval = o_ptr->sval;

    /* XXX Hack -- Wand of wonder can do anything before it */
    if (sval == SV_WAND_WONDER) sval = rand_int(SV_WAND_WONDER);

    /* Analyze the wand */
    switch (sval)
    {
        case SV_WAND_HEAL_MONSTER:
        {
            if (heal_monster(dir, damroll(4, 6))) *ident = TRUE;
            break;
        }

        case SV_WAND_HASTE_MONSTER:
        {
            if (speed_monster(dir)) *ident = TRUE;
            break;
        }

        case SV_WAND_CLONE_MONSTER:
        {
            if (clone_monster(dir)) *ident = TRUE;
            break;
        }

        case SV_WAND_TELEPORT_AWAY:
        {
            if (teleport_monster(dir)) *ident = TRUE;
            break;
        }

        case SV_WAND_DISARMING:
        {
            if (disarm_trap(dir)) *ident = TRUE;
            break;
        }

        case SV_WAND_TRAP_DOOR_DEST:
        {
            if (destroy_door(dir)) *ident = TRUE;
            break;
        }

        case SV_WAND_STONE_TO_MUD:
        {
            if (wall_to_mud(dir, 20 + randint(30))) *ident = TRUE;
            break;
        }

        case SV_WAND_LIGHT:
        {
            int dam_dice = 6;

            if (game_mode == GAME_NPPMORIA) dam_dice = 2;

            message(QString("A line of blue shimmering light appears."));
            light_line(dir, damroll(dam_dice, 8));
            *ident = TRUE;
            break;
        }

        case SV_WAND_SLEEP_MONSTER:
        {
            if (sleep_monster(dir)) *ident = TRUE;
            break;
        }

        case SV_WAND_SLOW_MONSTER:
        {
            if (slow_monster(dir)) *ident = TRUE;
            break;
        }

        case SV_WAND_CONFUSE_MONSTER:
        {
            if (confuse_monster(dir, (p_ptr->lev * 2 / 3))) *ident = TRUE;
            break;
        }

        case SV_WAND_FEAR_MONSTER:
        {
            if (fear_monster(dir, 10)) *ident = TRUE;
            break;
        }

        case SV_WAND_DRAIN_LIFE:
        {
            int dam = 150;

            if (game_mode == GAME_NPPMORIA) dam = 75;

            if (drain_life(dir, dam)) *ident = TRUE;
            break;
        }

        case SV_WAND_POLYMORPH:
        {
            if (poly_monster(dir)) *ident = TRUE;
            break;
        }

        case SV_WAND_STINKING_CLOUD:
        {
            fire_ball(GF_POIS, dir, 12, 2);
            *ident = TRUE;
            break;
        }

        case SV_WAND_MAGIC_MISSILE:
        {
            fire_bolt_or_beam(20, GF_MISSILE, dir, damroll(3, 4));
            *ident = TRUE;
            break;
        }

        case SV_WAND_ACID_BOLT:
        {
            fire_bolt_or_beam(20, GF_ACID, dir, damroll(10, 8));
            *ident = TRUE;
            break;
        }

        case SV_WAND_ELEC_BOLT:
        {
            int dam_dice = 6;
            int dam_side = 6;

            if (game_mode == GAME_NPPMORIA)
            {
                dam_dice = 4;
                dam_side = 8;
            }

            fire_bolt_or_beam(20, GF_ELEC, dir, damroll(dam_dice, dam_side));
            *ident = TRUE;
            break;
        }

        case SV_WAND_FIRE_BOLT:
        {
            int dam_dice = 12;

            if (game_mode == GAME_NPPMORIA)
            {
                dam_dice = 9;
            }


            fire_bolt_or_beam(20, GF_FIRE, dir, damroll(dam_dice, 8));
            *ident = TRUE;
            break;
        }

        case SV_WAND_COLD_BOLT:
        {
            fire_bolt_or_beam(20, GF_COLD, dir, damroll(6, 8));
            *ident = TRUE;
            break;
        }

        case SV_WAND_ACID_BALL:
        {
            int dam = 120;

            if (game_mode == GAME_NPPMORIA)
            {
                dam = 60;
            }

            fire_ball(GF_ACID, dir, dam, 2);
            *ident = TRUE;
            break;
        }

        case SV_WAND_ELEC_BALL:
        {
            int dam = 64;

            if (game_mode == GAME_NPPMORIA)
            {
                dam = 32;
            }

            fire_ball(GF_ELEC, dir, dam, 2);
            *ident = TRUE;
            break;
        }

        case SV_WAND_FIRE_BALL:
        {
            int dam = 144;

            if (game_mode == GAME_NPPMORIA)
            {
                dam = 72;
            }

            fire_ball(GF_FIRE, dir, dam, 2);
            *ident = TRUE;
            break;
        }

        case SV_WAND_COLD_BALL:
        {
            int dam = 96;

            if (game_mode == GAME_NPPMORIA)
            {
                dam = 48;
            }

            fire_ball(GF_COLD, dir, dam, 2);
            *ident = TRUE;
            break;
        }

        case SV_WAND_WONDER:
        {
            message(QString("Oops.  Wand of wonder activated."));
            break;
        }

        case SV_WAND_DRAGON_FIRE:
        {
            fire_ball(GF_FIRE, dir, 200, 3);
            *ident = TRUE;
            break;
        }

        case SV_WAND_DRAGON_COLD:
        {
            fire_ball(GF_COLD, dir, 160, 3);
            *ident = TRUE;
            break;
        }

        case SV_WAND_DRAGON_BREATH:
        {
            switch (randint(5))
            {
                case 1:
                {
                    fire_ball(GF_ACID, dir, 200, 3);
                    break;
                }

                case 2:
                {
                    fire_ball(GF_ELEC, dir, 160, 3);
                    break;
                }

                case 3:
                {
                    fire_ball(GF_FIRE, dir, 200, 3);
                    break;
                }

                case 4:
                {
                    fire_ball(GF_COLD, dir, 160, 3);
                    break;
                }

                default:
                {
                    fire_ball(GF_POIS, dir, 120, 3);
                    break;
                }
            }

            *ident = TRUE;
            break;
        }

        case SV_WAND_ANNIHILATION:
        {
            if (drain_life(dir, 250)) *ident = TRUE;
            break;
        }

        case SV_WAND_WALL_BUILDING:
        {
            if (build_wall(dir, damroll(4, 8))) *ident = TRUE;
            break;
        }
    }


    return (TRUE);
}

/*
 * Zap a rod
 */
static bool zap_rod(object_type *o_ptr, bool *ident, int dir)
{
    bool used_charge = TRUE;
    object_kind *k_ptr = &k_info[o_ptr->k_idx];

    /* Not identified yet */
    *ident = FALSE;

    /* Still charging? */
    if (o_ptr->timeout > (o_ptr->pval - k_ptr->pval))
    {

        if (o_ptr->number == 1)
            message(QString("The rod is still charging."));
        else
            message(QString("The rods are all still charging."));

        return FALSE;
    }

    /* Sound */
    sound(MSG_ZAP_ROD);

    /* Analyze the rod */
    switch (o_ptr->sval)
    {
        case SV_ROD_DETECT_TRAP:
        {
            if (detect(DETECT_RADIUS, DETECT_TRAPS)) *ident = TRUE;
            break;
        }

        case SV_ROD_DETECT_DOOR:
        {
            if (detect(DETECT_RADIUS, DETECT_DOORS_STAIRS)) *ident = TRUE;
            break;
        }

        case SV_ROD_IDENTIFY:
        {
            *ident = TRUE;
            if (!ident_spell()) used_charge = FALSE;
            break;
        }

        case SV_ROD_RECALL:
        {
            if (!set_recall()) used_charge = FALSE;
            *ident = TRUE;
            break;
        }

        case SV_ROD_ILLUMINATION:
        {
            if (light_area(damroll(2, 8), 2)) *ident = TRUE;
            break;
        }

        case SV_ROD_MAPPING:
        {
            detect(DETECT_RADIUS + 5, DETECT_MAP);
            *ident = TRUE;
            break;
        }

        case SV_ROD_DETECTION:
        {
            if (detect(DETECT_RADIUS, DETECT_ALL)) *ident = TRUE;
            *ident = TRUE;
            break;
        }

        case SV_ROD_PROBING:
        {
            if (!probing()) used_charge = FALSE;
            *ident = TRUE;
            break;
        }

        case SV_ROD_CURING:
        {
            if (clear_timed(TMD_BLIND, TRUE)) *ident = TRUE;
            if (clear_timed(TMD_POISONED, TRUE)) *ident = TRUE;
            if (clear_timed(TMD_CONFUSED, TRUE)) *ident = TRUE;
            if (set_stun(0)) *ident = TRUE;
            if (set_cut(0)) *ident = TRUE;
            break;
        }

        case SV_ROD_HEALING:
        {
            if (hp_player(550)) *ident = TRUE;
            if (set_stun(0)) *ident = TRUE;
            if (set_cut(0)) *ident = TRUE;
            break;
        }

        case SV_ROD_RESTORATION:
        {
            if (restore_level()) *ident = TRUE;
            if (do_res_stat(A_STR)) *ident = TRUE;
            if (do_res_stat(A_INT)) *ident = TRUE;
            if (do_res_stat(A_WIS)) *ident = TRUE;
            if (do_res_stat(A_DEX)) *ident = TRUE;
            if (do_res_stat(A_CON)) *ident = TRUE;
            if (do_res_stat(A_CHR)) *ident = TRUE;
            break;
        }

        case SV_ROD_SPEED:
        {
            if (!p_ptr->timed[TMD_FAST])
            {
                if (set_timed(TMD_FAST, randint(30) + 15, TRUE)) *ident = TRUE;
            }
            else
            {
                (void)inc_timed(TMD_FAST, 5, FALSE);
            }
            break;
        }

        case SV_ROD_STONE_TO_MUD:
        {
            if (wall_to_mud(dir, 20 + randint(30))) *ident = TRUE;
            break;
        }

        case SV_ROD_TELEPORT_AWAY:
        {
            if (teleport_monster(dir)) *ident = TRUE;
            break;
        }

        case SV_ROD_DISARMING:
        {
            if (disarm_trap(dir)) *ident = TRUE;
            break;
        }

        case SV_ROD_LIGHT:
        {
            message(QString("A line of blue shimmering light appears."));
            light_line(dir, damroll(6, 8));
            *ident = TRUE;
            break;
        }

        case SV_ROD_SLEEP_MONSTER:
        {
            if (sleep_monster(dir)) *ident = TRUE;
            break;
        }

        case SV_ROD_SLOW_MONSTER:
        {
            if (slow_monster(dir)) *ident = TRUE;
            break;
        }

        case SV_ROD_DRAIN_LIFE:
        {
            if (drain_life(dir, 150)) *ident = TRUE;
            break;
        }

        case SV_ROD_POLYMORPH:
        {
            if (poly_monster(dir)) *ident = TRUE;
            break;
        }

        case SV_ROD_ACID_BOLT:
        {
            fire_bolt_or_beam(10, GF_ACID, dir, damroll(12, 8));
            *ident = TRUE;
            break;
        }

        case SV_ROD_ELEC_BOLT:
        {
            fire_bolt_or_beam(10, GF_ELEC, dir, damroll(6, 6));
            *ident = TRUE;
            break;
        }

        case SV_ROD_FIRE_BOLT:
        {
            fire_bolt_or_beam(10, GF_FIRE, dir, damroll(16, 8));
            *ident = TRUE;
            break;
        }

        case SV_ROD_COLD_BOLT:
        {
            fire_bolt_or_beam(10, GF_COLD, dir, damroll(10, 8));
            *ident = TRUE;
            break;
        }

        case SV_ROD_ACID_BALL:
        {
            fire_ball(GF_ACID, dir, 120, 2);
            *ident = TRUE;
            break;
        }

        case SV_ROD_ELEC_BALL:
        {
            fire_ball(GF_ELEC, dir, 64, 2);
            *ident = TRUE;
            break;
        }

        case SV_ROD_FIRE_BALL:
        {
            fire_ball(GF_FIRE, dir, 144, 2);
            *ident = TRUE;
            break;
        }

        case SV_ROD_COLD_BALL:
        {
            fire_ball(GF_COLD, dir, 96, 2);
            *ident = TRUE;
            break;
        }
        case SV_ROD_STAR_IDENTIFY:
        {
            if (!identify_fully()) used_charge = FALSE;
            *ident = TRUE;
            break;
        }
        case SV_ROD_MASS_IDENTIFY:
        {
            mass_identify(3);
            *ident = TRUE;
            break;
        }
    }

    /* We used the object. */
    if (used_charge)
    {
        o_ptr->timeout += k_ptr->pval;

    }

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
static bool activate_object(object_type *o_ptr, int dir)
{
    int k, i, chance;


    /* Check the recharge */
    if (o_ptr->timeout)
    {
        message(QString("It whines, glows and fades..."));
        return FALSE;
    }

    /* Activate the artifact */
    color_message(QString("You activate it..."), MSG_ACT_ARTIFACT);

    /* Artifacts, except for special artifacts with dragon scale mail*/
    if ((o_ptr->art_num) && (o_ptr->art_num < z_info->art_norm_max))
    {
        bool did_activation = TRUE;

        artifact_type *a_ptr = &a_info[o_ptr->art_num];
        QString o_name;

        /* Get the basic name of the object */
        o_name = object_desc(o_ptr, ODESC_BASE);

        switch (a_ptr->activation)
        {
            case ACT_ILLUMINATION:
            {
                message(QString("The %1 wells with clear light...") .arg(o_name));
                light_area(damroll(2, 15), 3);
                break;
            }

            case ACT_MAGIC_MAP:
            {
                message(QString("The %1 shines brightly...") .arg(o_name));
                detect(DETECT_RADIUS + 5, DETECT_MAP);
                break;
            }

            case ACT_CLAIRVOYANCE:
            {
                message(QString("The %1 glows a deep green...") .arg(o_name));
                wiz_light();
                detect(DETECT_RADIUS, DETECT_DOORS_STAIRS_TRAPS);
                break;
            }

            case ACT_PROT_EVIL:
            {
                message(QString("The %1 lets out a shrill wail...") .arg(o_name));
                k = 3 * p_ptr->lev;
                (void)inc_timed(TMD_PROTEVIL, randint(25) + k, TRUE);
                break;
            }

            case ACT_DISP_EVIL:
            {
                message(QString("The %1 floods the area with goodness...") .arg(o_name));
                dispel_evil(p_ptr->lev * 5);
                break;
            }

            case ACT_HASTE2:
            {
                message(QString("The %1 glows brightly...") .arg(o_name));
                if (!p_ptr->timed[TMD_FAST])
                {
                    (void)set_timed(TMD_FAST, randint(75) + 75, TRUE);
                }
                else
                {
                    (void)inc_timed(TMD_FAST, 5, FALSE);
                }
                break;
            }

            case ACT_FIRE3:
            {
                message(QString("The %1 glows deep red...") .arg(o_name));
                fire_ball(GF_FIRE, dir, 120, 3);
                break;
            }

            case ACT_FROST5:
            {
                message(QString("The %1 glows bright white...") .arg(o_name));
                fire_ball(GF_COLD, dir, 200, 3);
                break;
            }

            case ACT_ELEC2:
            {
                message(QString("The %1 glows deep blue...") .arg(o_name));
                fire_ball(GF_ELEC, dir, 250, 3);
                break;
            }

            case ACT_BIZZARE:
            {
                message(QString("The %1 glows intensely black...") .arg(o_name));
                ring_of_power(dir);
                break;
            }


            case ACT_STAR_BALL:
            {
                message(QString("Your %1 is surrounded by lightning...") .arg(o_name));
                for (i = 0; i < 8; i++) fire_ball(GF_ELEC, ddd[i], 150, 3);
                break;
            }

            case ACT_RAGE_BLESS_RESIST:
            {
                int act_time = randint(50) + 50;
                message(QString("Your %1 glows many colours...") .arg(o_name));
                if (game_mode == GAME_NPPMORIA)
                {
                    if (!p_ptr->timed[TMD_BERSERK])
                    {
                        p_ptr->mhp +=15;
                        p_ptr->chp +=15;
                    }
                }
                (void)hp_player(30);
                (void)clear_timed(TMD_AFRAID, TRUE);
                (void)inc_timed(TMD_BERSERK, randint(50) + 50, TRUE);
                (void)inc_timed(TMD_BLESSED, randint(50) + 50, TRUE);
                (void)inc_timed(TMD_OPP_ACID, act_time, TRUE);
                (void)inc_timed(TMD_OPP_ELEC, act_time, TRUE);
                (void)inc_timed(TMD_OPP_FIRE, act_time, TRUE);
                (void)inc_timed(TMD_OPP_COLD, act_time, TRUE);
                (void)inc_timed(TMD_OPP_POIS, act_time, TRUE);
                break;
            }

            case ACT_HEAL2:
            {
                message(QString("Your %1 glows a bright white...") .arg(o_name));
                message(QString("You feel much better..."));
                (void)hp_player(1000);
                (void)set_cut(0);
                break;
            }

            case ACT_PHASE:
            {
                message(QString("Your %1 twists space around you...") .arg(o_name));
                teleport_player(10, FALSE);
                break;
            }

            case ACT_BANISHMENT:
            {
                message(QString("Your %1 glows deep blue...") .arg(o_name));
                if (!banishment()) return FALSE;
                break;
            }

            case ACT_TRAP_DOOR_DEST:
            {
                message(QString("Your %1 glows bright red...") .arg(o_name));
                destroy_doors_touch();
                break;
            }

            case ACT_DETECT:
            {
                message(QString("Your %1 glows bright white...") .arg(o_name));
                message(QString("An image forms in your mind..."));
                (void)detect(DETECT_RADIUS, DETECT_ALL);
                break;
            }

            case ACT_HEAL1:
            {
                message(QString("Your %1 glows deep blue...") .arg(o_name));
                message(QString("You feel a warm tingling inside..."));
                (void)hp_player(500);
                (void)set_cut(0);
                break;
            }

            case ACT_RESIST:
            {
                int act_time = randint(20) + 20;
                message(QString("Your %1 glows many colours...") .arg(o_name));
                (void)inc_timed(TMD_OPP_ACID, act_time, TRUE);
                (void)inc_timed(TMD_OPP_ELEC, act_time, TRUE);
                (void)inc_timed(TMD_OPP_FIRE, act_time, TRUE);
                (void)inc_timed(TMD_OPP_COLD, act_time, TRUE);
                (void)inc_timed(TMD_OPP_POIS, act_time, TRUE);
                break;
            }

            case ACT_SLEEP:
            {
                message(QString("Your %1 glows deep blue...") .arg(o_name));
                sleep_monsters_touch();
                break;
            }

            case ACT_RECHARGE1:
            {
                message(QString("Your %1 glows bright yellow...") .arg(o_name));
                if (!recharge(60, FALSE, 50)) return FALSE;
                break;
            }

            case ACT_TELEPORT:
            {
                message(QString("Your %1 twists space around you...") .arg(o_name));
                teleport_player(100, FALSE);
                break;
            }

            case ACT_RESTORE_LIFE:
            {
                message(QString("Your %1 glows a deep red...") .arg(o_name));
                restore_level();
                break;
            }

            case ACT_MISSILE:
            {
                message(QString("Your %1 glows extremely brightly...") .arg(o_name));
                fire_bolt(GF_MISSILE, dir, damroll(2, 6));
                break;
            }

            case ACT_FIRE1:
            {
                message(QString("Your %1 is covered in fire...") .arg(o_name));
                fire_bolt(GF_FIRE, dir, damroll(9, 8));
                break;
            }

            case ACT_FROST1:
            {
                message(QString("Your %1 is covered in frost...") .arg(o_name));
                fire_bolt(GF_COLD, dir, damroll(6, 8));
                break;
            }

            case ACT_LIGHTNING_BOLT:
            {
                message(QString("Your %1 is covered in sparks...") .arg(o_name));
                fire_bolt(GF_ELEC, dir, damroll(4, 8));
                break;
            }

            case ACT_ACID1:
            {
                message(QString("Your %1 is covered in acid...") .arg(o_name));
                fire_bolt(GF_ACID, dir, damroll(5, 8));
                break;
            }

            case ACT_ARROW:
            {
                message(QString("Your %1 grows magical spikes...") .arg(o_name));
                fire_bolt(GF_ARROW, dir, 150);
                break;
            }

            case ACT_HASTE1:
            {
                message(QString("Your %1 glows bright green...") .arg(o_name));
                if (!p_ptr->timed[TMD_FAST])
                {
                    (void)set_timed(TMD_FAST, randint(20) + 20, TRUE);
                }
                else
                {
                    (void)inc_timed(TMD_FAST, 5, FALSE);
                }
                break;
            }

            case ACT_REM_FEAR_POIS:
            {
                message(QString("Your %1 glows deep blue...") .arg(o_name));
                (void)clear_timed(TMD_AFRAID, TRUE);
                (void)clear_timed(TMD_POISONED, TRUE);
                break;
            }

            case ACT_STINKING_CLOUD:
            {
                message(QString("Your %1 throbs deep green...") .arg(o_name));
                fire_ball(GF_POIS, dir, 12, 3);
                break;
            }

            case ACT_FROST2:
            {
                message(QString("Your %1 is covered in frost...") .arg(o_name));
                fire_ball(GF_COLD, dir, 48, 2);
                break;
            }

            case ACT_FROST3:
            {
                message(QString("Your %1 glows a intense blue...") .arg(o_name));
                fire_ball(GF_COLD, dir, 100, 2);
                break;
            }

            case ACT_FROST4:
            {
                message(QString("Your %1 glows a pale blue...") .arg(o_name));
                fire_bolt(GF_COLD, dir, damroll(12, 8));
                break;
            }

            case ACT_FIRE2:
            {
                message(QString("Your %1 rages in fire...") .arg(o_name));
                fire_ball(GF_FIRE, dir, 72, 2);
                break;
            }

            case ACT_DRAIN_LIFE2:
            {
                message(QString("Your %1 glows black...") .arg(o_name));
                drain_life(dir, 120);
                break;
            }

            case ACT_STONE_TO_MUD:
            {
                message(QString("Your %1 pulsates...") .arg(o_name));
                (void)wall_to_mud(dir, 20 + randint(30));
                break;
            }

            case ACT_MASS_BANISHMENT:
            {
                message(QString("Your %1 lets out a long, shrill note...") .arg(o_name));
                (void)mass_banishment();
                break;
            }

            case ACT_CURE_WOUNDS:
            {
                message(QString("Your %1 radiates deep purple...") .arg(o_name));
                hp_player(damroll(6, 10));
                (void)set_cut((p_ptr->timed[TMD_CUT] / 2) - 50);
                break;
            }

            case ACT_TELE_AWAY:
            {
                message(QString("Your %1 glows deep red...") .arg(o_name));
                teleport_monster(dir);
                break;
            }

            case ACT_WOR:
            {
                message(QString("Your %1 glows soft white...") .arg(o_name));
                if (!set_recall()) return (FALSE);
                break;
            }

            case ACT_CONFUSE:
            {
                message(QString("Your %1 glows in scintillating colours...") .arg(o_name));
                confuse_monster(dir, (p_ptr->lev * 2 / 3));
                break;
            }

            case ACT_IDENTIFY:
            {
                message(QString("Your %1 glows yellow...") .arg(o_name));
                if (!identify_fully()) return FALSE;
                break;
            }

            case ACT_PROBE:
            {
                message(QString("Your %1 glows brightly...") .arg(o_name));
                probing();
                break;
            }

            case ACT_DRAIN_LIFE1:
            {
                message(QString("Your %1 glows white...") .arg(o_name));
                drain_life(dir, 90);
                break;
            }

            case ACT_FIREBRAND:
            {
                message(QString("Your %1 glows deep red...") .arg(o_name));
                if (!brand_bolts(TRUE)) return FALSE;
                break;
            }

            case ACT_STARLIGHT:
            {
                message(QString("Your %1 glows with the light of a thousand stars...") .arg(o_name));
                for (k = 0; k < 8; k++) strong_light_line(ddd[k]);
                break;
            }

            case ACT_MANA_BOLT:
            {
                message(QString("Your %1 glows white...") .arg(o_name));
                fire_bolt(GF_MANA, dir, damroll(12, 8));
                break;
            }

            case ACT_BERSERKER:
            {
                message(QString("Your %1 glows in anger...") .arg(o_name));
                if (game_mode == GAME_NPPMORIA)
                {
                    if (!p_ptr->timed[TMD_BERSERK])
                    {
                        p_ptr->mhp +=15;
                        p_ptr->chp +=15;
                    }
                }
                inc_timed(TMD_BERSERK, randint(50) + 50, TRUE);
                break;
            }

            case ACT_RES_ACID:
            {
                message(QString("Your %1 glows light gray...") .arg(o_name));
                (void)inc_timed(TMD_OPP_ACID, randint(20) + 20, TRUE);
                break;
            }

            case ACT_RES_ELEC:
            {
                message(QString("Your %1 glows light blue...") .arg(o_name));
                (void)inc_timed(TMD_OPP_ELEC, randint(20) + 20, TRUE);
                break;
            }

            case ACT_RES_FIRE:
            {
                message(QString("Your %1 glows light red...") .arg(o_name));
                (void)inc_timed(TMD_OPP_FIRE, randint(20) + 20, TRUE);
                break;
            }

            case ACT_RES_COLD:
            {
                message(QString("Your %1 glows bright white...") .arg(o_name));
                (void)inc_timed(TMD_OPP_COLD, randint(20) + 20, TRUE);
                break;
            }

            case ACT_RES_POIS:
            {
                message(QString("Your %1 glows light green...") .arg(o_name));
                (void)inc_timed(TMD_OPP_POIS, randint(20) + 20, TRUE);
                break;
            }

            default:
            {
                if ((a_ptr->tval != TV_DRAG_ARMOR) &&
                    (a_ptr->tval != TV_DRAG_SHIELD)) return (FALSE);
                else did_activation = FALSE;

                break;
            }
        }

        if (did_activation)
        {

            /* Set the recharge time */
            if (a_ptr->randtime)
                o_ptr->timeout = a_ptr->time + (byte)randint(a_ptr->randtime);
            else o_ptr->timeout = a_ptr->time;

            /* Window stuff */
            p_ptr->redraw |= (PR_WIN_INVENTORY | PR_WIN_EQUIPMENT);

            /* Done */
            return FALSE;
        }
    }

    /* Hack -- Dragon Scale Mail can be activated as well */
    if ((o_ptr->tval == TV_DRAG_ARMOR) ||
        (o_ptr->tval == TV_DRAG_SHIELD))
    {

        u16b value = o_ptr->sval;
        int time_2 = randint(10) + 10;

        if (o_ptr->tval == TV_DRAG_ARMOR) value *= 2;

        /* Branch on the sub-type */
        switch (o_ptr->ego_num)
        {
            case EGO_DRAGON_BLUE:
            {
                value *= 50;

                sound(MSG_BR_ELEC);
                message(QString("You breathe lightning."));
                fire_arc(GF_ELEC, dir, value, 0, 30);

                inc_timed(TMD_OPP_ELEC, time_2, TRUE);

                o_ptr->timeout = rand_int(value) + (value);
                break;
            }

            case EGO_DRAGON_WHITE:
            {
                value *= 50;

                sound(MSG_BR_FROST);
                message(QString("You breathe frost."));
                fire_arc(GF_COLD, dir, value, 0, 30);

                inc_timed(TMD_OPP_COLD, time_2, TRUE);

                o_ptr->timeout = rand_int(value) + (value);
                break;
            }

            case EGO_DRAGON_BLACK:
            {
                value *= 50;

                sound(MSG_BR_ACID);
                message(QString("You breathe acid."));
                fire_arc(GF_ACID, dir, value, 0, 30);

                inc_timed(TMD_OPP_ACID, time_2, TRUE);

                o_ptr->timeout = rand_int(value) + value;
                break;
            }

            case EGO_DRAGON_GREEN:
            {
                value *= 50;

                sound(MSG_BR_GAS);
                message(QString("You breathe poison gas."));
                fire_arc(GF_POIS, dir, value, 0, 30);

                inc_timed(TMD_OPP_POIS, time_2, TRUE);

                o_ptr->timeout = rand_int(value) + value;
                break;
            }

            case EGO_DRAGON_RED:
            {
                value *= 50;

                sound(MSG_BR_FIRE);
                message(QString("You breathe fire."));
                fire_arc(GF_FIRE, dir, value, 0, 30);

                (void)inc_timed(TMD_OPP_FIRE, time_2, TRUE);

                o_ptr->timeout = rand_int(value) + value;
                break;
            }

            case EGO_DRAGON_MULTIHUED:
            {
                value *= 75;

                chance = rand_int(5);
                sound(((chance == 1) ? MSG_BR_ELEC :
                       ((chance == 2) ? MSG_BR_FROST :
                        ((chance == 3) ? MSG_BR_ACID :
                         ((chance == 4) ? MSG_BR_GAS : MSG_BR_FIRE)))));
                message(QString("You breathe %1.")
                           .arg(((chance == 1) ? "lightning" :
                            ((chance == 2) ? "frost" :
                             ((chance == 3) ? "acid" :
                              ((chance == 4) ? "poison gas" : "fire"))))));
                fire_arc(((chance == 1) ? GF_ELEC :
                           ((chance == 2) ? GF_COLD :
                            ((chance == 3) ? GF_ACID :
                             ((chance == 4) ? GF_POIS : GF_FIRE)))),
                          dir, value, 0, 30);

                /* Increase the bonus to resistances */
                time_2 = randint(20) + 20;
                inc_timed(TMD_OPP_ELEC, time_2, TRUE);
                inc_timed(TMD_OPP_COLD, time_2, TRUE);
                inc_timed(TMD_OPP_ACID, time_2, TRUE);
                inc_timed(TMD_OPP_POIS, time_2, TRUE);
                inc_timed(TMD_OPP_FIRE, time_2, TRUE);

                o_ptr->timeout = rand_int(value * 3 / 4) + (value * 3 / 4);
                break;
            }

            case EGO_DRAGON_BRONZE:
            {
                value *= 50;

                sound(MSG_BR_CONF);
                message(QString("You breathe confusion."));
                fire_arc(GF_CONFUSION, dir, value, 0, 30);
                o_ptr->timeout = rand_int(value) + value;
                break;
            }

            case EGO_DRAGON_GOLD:
            {
                value *= 50;

                sound(MSG_BR_SOUND);
                message(QString("You breathe sound."));
                fire_arc(GF_SOUND, dir, value, 0, 30);
                o_ptr->timeout = rand_int(value) + value;
                break;
            }

            case EGO_DRAGON_CHAOS:
            {
                value *= 60;

                chance = rand_int(2);

                sound(((chance == 1 ? MSG_BR_CHAOS : MSG_BR_DISENCHANT)));

                message(QString("You breathe %1.")
                           .arg(chance == 1 ? "chaos" : "disenchantment"));
                fire_arc((chance == 1 ? GF_CHAOS : GF_DISENCHANT),
                          dir, value, 0, 30);
                o_ptr->timeout = rand_int(value) + value;
                break;
            }

            case EGO_DRAGON_LAW:
            {
                value *= 60;

                chance = rand_int(2);
                sound(((chance == 1 ? MSG_BR_SOUND : MSG_BR_SHARDS)));
                message(QString("You breathe %1.")
                           .arg(chance == 1 ? "sound" : "shards"));
                fire_arc((chance == 1 ? GF_SOUND : GF_SHARD),
                          dir, value, 0, 30);
                o_ptr->timeout = rand_int(value) + value;
                break;
            }

            case EGO_DRAGON_BALANCE:
            {
                value *= 75;

                chance = rand_int(4);

                sound(((chance == 1) ? MSG_BR_CHAOS :
                       ((chance == 2) ? MSG_BR_DISENCHANT :
                        ((chance == 3) ? MSG_BR_SOUND : MSG_BR_SHARDS))));

                message(QString("You breathe %1.")
                           .arg((chance == 1) ? "chaos" :
                            ((chance == 2) ? "disenchantment" :
                             ((chance == 3) ? "sound" : "shards"))));

                fire_arc(((chance == 1) ? GF_CHAOS :
                           ((chance == 2) ? GF_DISENCHANT :
                            ((chance == 3) ? GF_SOUND : GF_SHARD))),
                          dir, value, 0, 30);

                o_ptr->timeout = rand_int(value) + value;
                break;
            }

            case EGO_DRAGON_PSEUDO:
            {
                value *= 65;

                chance = rand_int(2);

                sound(((chance == 0 ? MSG_BR_LIGHT : MSG_BR_DARK)));

                message(QString("You breathe %1.")
                           .arg(chance == 0 ? "light" : "darkness"));

                fire_arc((chance == 0 ? GF_LIGHT : GF_DARK), dir, value, 0, 30);

                o_ptr->timeout = rand_int(value) + value;
                break;
            }

            case EGO_DRAGON_POWER:
            {
                value *= 100;

                sound(MSG_BR_ELEMENTS);
                message(QString("You breathe the elements."));
                fire_arc(GF_MISSILE, dir, value, 0, 30);
                o_ptr->timeout = rand_int(value) + value;
                break;
            }
        }

        /* Window stuff */
        p_ptr->redraw |= (PR_WIN_INVENTORY | PR_WIN_EQUIPMENT);

        /* Success */
        return FALSE;
    }

    /* Hack -- some Rings can be activated for double resist and element ball */
    if (o_ptr->tval == TV_RING)
    {
        /* Branch on the sub-type */
        switch (o_ptr->sval)
        {
            case SV_RING_ACID:
            {
                fire_ball(GF_ACID, dir, 70, 2);
                inc_timed(TMD_OPP_ACID, randint(20) + 20, TRUE);
                o_ptr->timeout = rand_int(50) + 50;
                break;
            }

            case SV_RING_FLAMES:
            {
                fire_ball(GF_FIRE, dir, 80, 2);
                inc_timed(TMD_OPP_FIRE, randint(20) + 20, TRUE);
                o_ptr->timeout = rand_int(50) + 50;
                break;
            }

            case SV_RING_ICE:
            {
                fire_ball(GF_COLD, dir, 75, 2);
                inc_timed(TMD_OPP_COLD, randint(20) + 20, TRUE);
                o_ptr->timeout = rand_int(50) + 50;
                break;
            }

            case SV_RING_LIGHTNING:
            {
                fire_ball(GF_ELEC, dir, 85, 2);
                inc_timed(TMD_OPP_ELEC, randint(20) + 20, TRUE);
                o_ptr->timeout = rand_int(50) + 50;
                break;
            }
        }

        /* Redraw stuff */
        p_ptr->redraw |= (PR_WIN_EQUIPMENT);

        /* Success */
        return FALSE;
    }

    /* Mistake */
    message(QString("Oops.  That object cannot be activated."));

    /* Not used up */
    return (FALSE);
}


/*
 * Use an object
 */
static bool use_object(object_type *o_ptr, bool *ident, int dir)
{
    bool used;

    /* Analyze the object */
    switch (o_ptr->tval)
    {
        case TV_FOOD:
        {
            used = eat_food(o_ptr, ident);
            break;
        }

        case TV_POTION:
        {
            used = quaff_potion(o_ptr, ident);
            break;
        }

        case TV_SCROLL:
        {
            used = read_scroll(o_ptr, ident);
            break;
        }

        case TV_STAFF:
        {
            used = use_staff(o_ptr, ident);
            break;
        }

        case TV_WAND:
        {
            used = aim_wand(o_ptr, ident, dir);
            break;
        }

        case TV_ROD:
        {
            used = zap_rod(o_ptr, ident, dir);

            break;
        }

        default:
        {
            used = activate_object(o_ptr, dir);
            break;
        }
    }

    return (used);
}


/*
 * Describe the charges on an item in the inventory.
 */
void staff_wand_item_charges(object_type *o_ptr)
{
    /* Require staff/wand */
    if ((o_ptr->tval != TV_STAFF) && (o_ptr->tval != TV_WAND)) return;

    /* Require known item */
    if (!o_ptr->is_known()) return;

    QString o_name = object_desc(o_ptr, ODESC_BASE);

    /* Print a message */
    if (!o_ptr->pval) message(QString("The %1 has no charges remaining") .arg(o_name));
    else if (o_ptr->pval == 1) message(QString("The %1 has one charge remaining") .arg(o_name));
    else message(QString("The %1 has %2 charges remaining.") .arg(o_name) .arg(o_ptr->pval));
}


/*** Using items the traditional way ***/


/*
 * Use an object.
 *
 */
void command_use(cmd_arg args)
{
    if (!args.verify) return;

    int item = args.item;
    object_type *o_ptr = object_from_item_idx(item);
    bool ident = FALSE;
    bool used = FALSE;
    bool was_aware = o_ptr->is_flavor_known();
    int dir = DIR_TARGET;
    int px = p_ptr->px, py = p_ptr->py;
    int snd;
    use_type use;
    int items_allowed = 0;

    /* Determine how this item is used. */
    if (o_ptr->is_rod())
    {
        object_kind *k_ptr = &k_info[o_ptr->k_idx];

        if (o_ptr->timeout > (o_ptr->pval - k_ptr->pval))
        {
            if (o_ptr->number == 1) message(QString("The rod is still charging"));
            else message(QString("The rods are all still charging"));

            return;
        }

        use = USE_TIMEOUT;
        snd = MSG_ZAP_ROD;
        items_allowed = USE_INVEN | USE_FLOOR;
    }
    else if (o_ptr->is_wand())
    {
        if (!obj_has_charges(o_ptr))
        {
            message(QString("That wand has no charges."));
            o_ptr->ident |= (IDENT_EMPTY);
            p_ptr->notice |= (PN_COMBINE | PN_REORDER);
            p_ptr->redraw |= (PR_WIN_INVENTORY);
            return;
        }

        use = USE_CHARGE;
        snd = MSG_ZAP_ROD;
        items_allowed = USE_INVEN | USE_FLOOR;
    }
    else if (o_ptr->is_staff())
    {
        if (!o_ptr->has_charges())
        {
            message(QString("That staff has no charges."));
            o_ptr->ident |= (IDENT_EMPTY);
            p_ptr->notice |= (PN_COMBINE | PN_REORDER);
            p_ptr->redraw |= (PR_WIN_INVENTORY);
            return;
        }

        use = USE_CHARGE;
        snd = MSG_ZAP_ROD;
        items_allowed = USE_INVEN | USE_FLOOR;
    }
    else if (o_ptr->is_food())
    {
        use = USE_SINGLE;
        snd = MSG_EAT;
        items_allowed = USE_INVEN | USE_FLOOR;
    }
    else if (o_ptr->is_potion())
    {
        use = USE_SINGLE;
        snd = MSG_QUAFF;
        items_allowed = USE_INVEN | USE_FLOOR;
    }
    else if (o_ptr->is_scroll())
    {
        /* Check player can use scroll */
        if (!player_can_read())
            return;

        use = USE_SINGLE;
        snd = MSG_GENERIC;
        items_allowed = USE_INVEN | USE_FLOOR;
    }
    else if (obj_is_activatable(o_ptr))
    {
        if (!obj_can_activate(o_ptr))
        {
            message(QString("That item is still charging."));
            return;
        }

        use = USE_TIMEOUT;
        snd = MSG_ACT_ARTIFACT;
        items_allowed = USE_EQUIP;
    }
    else
    {
        message(QString("The item cannot be used at the moment"));
    }

    /* Check if item is within player's reach. */
    if (items_allowed == 0 || !item_is_available(item, NULL, items_allowed))
    {
        message(QString("You cannot use that item from its current location."));
        return;
    }

    /* track the object used */
    track_object(item);

    /* If the item requires a direction, get one (allow canceling) */
    if (obj_needs_aim(o_ptr))
    {
        dir = args.direction;

        if (dir == DIR_CLOSEST)
        {
            int mode = TARGET_QUIET;

            if (!obj_aim_trap(o_ptr)) mode |= TARGET_KILL;
            else mode |= TARGET_TRAP;

            if (!target_set_closest(mode)) dir = DIR_UNKNOWN;
        }

        if (dir == DIR_UNKNOWN)
        {
            if (!get_aim_dir(&dir, obj_aim_trap(o_ptr))) return;
        }
    }

    p_ptr->message_append_start();

    /* Check for use if necessary, and execute the effect */
    if ((use != USE_CHARGE && use != USE_TIMEOUT) ||
        check_devices(o_ptr))
    {
        /* Special message for artifacts */
        if (!o_ptr->is_artifact())
        {
            /* Make a noise! */
            sound(snd);
        }

        /* mark the item (the place in the inventory might shift) */
        o_ptr->obj_in_use = TRUE;

        if (obj_is_activatable(o_ptr))
        {
            if (!get_item_allow(item, VERIFY_ACTIVATE)) return;
        }
        else if (!get_item_allow(item, VERIFY_USE)) return;

        /* Do effect */
        used = use_object(o_ptr, &ident, dir);

        /* make sure we still have the right item if the inventory was moved around */
        if (find_object_in_use(&item))
        {
            o_ptr = object_from_item_idx(item);
        }

        /* Clear the item mark */
        o_ptr->obj_in_use = FALSE;

        // Set up the repeat
        p_ptr->player_previous_command_update(CMD_ITEM_USE, args);
        p_ptr->command_previous_args.k_idx = o_ptr->k_idx;

        /* Quit if the item wasn't used and no knowledge was gained */
        if (!used && (was_aware || !ident))
        {
            p_ptr->message_append_stop();
            return;
        }

    }

    /* Tried the object */
    o_ptr->mark_tried();

    /* Combine / Reorder the pack (later) */
    p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);

    /* Window stuff */
    p_ptr->redraw |= (PR_WIN_INVENTORY | PR_WIN_EQUIPMENT);

    /* Handle first-time use */
    if (ident)
    {
        /* Successfully determined the object function */
        if (!o_ptr->is_aware())
        {
            /* Object level */
            int lev = k_info[o_ptr->k_idx].k_level;
            o_ptr->mark_aware();
            gain_exp((lev + (p_ptr->lev / 2)) / p_ptr->lev);
            apply_autoinscription(o_ptr);
        }
    }

    /* If the item is a null pointer or has been wiped, be done now */
    if (!o_ptr || o_ptr->k_idx <= 1)
    {
        /* Take a turn */
        process_player_energy(BASE_ENERGY_MOVE);
        return;
    }

    /* If there are no more of the item left, then we're done. */
    if (!o_ptr->number)
    {
       // deliberately do nothing
    }

    /* Chargeables act differently to single-used items when not used up */
    else if (used && (use == USE_CHARGE))
    {

        /* Use a single charge */
        o_ptr->pval--;

        /* Describe charges */
        staff_wand_item_charges(o_ptr);

    }
    else if (used && use == USE_SINGLE)
    {

        /* Destroy a potion in the pack */
        if (item >= 0)
        {
            inven_item_increase(item, -1);
            inven_item_describe(item);
            inven_item_optimize(item);
        }

        /* Destroy a potion on the floor */
        else
        {
            floor_item_increase(0 - item, -1);
            floor_item_describe(0 - item);
            floor_item_optimize(0 - item);
        }
    }

    /* Hack to make Glyph of Warding work properly */
    if (dungeon_info[py][px].effect_idx == FEAT_GLYPH_WARDING)
    {
        /* Shift any objects to further away */
        for (o_ptr = get_first_object(py, px); o_ptr; o_ptr = get_next_object(o_ptr))
        {
            drop_near(o_ptr, 0, py, px);
        }

        /* Delete the "moved" objects from their original position */
        delete_object(py, px);
    }

    /* Take a turn */
    process_player_energy(BASE_ENERGY_MOVE);
}
