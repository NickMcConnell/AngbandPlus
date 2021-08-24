/* File: calcs.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *                    Jeff Greene, Diego Gonzalez
 *
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include "src/npp.h"
#include <src/player_scores.h>



/* Returns a character's adjustment to armor class	 -JWT-	 */
static int moria_toac_adj()
{
    int stat;

    stat = p_ptr->state.stat_loaded_cur[A_DEX];
    if	  (stat <   4)	return(-4);
    else if (stat ==  4)	return(-3);
    else if (stat ==  5)	return(-2);
    else if (stat ==  6)	return(-1);
    else if (stat <  15)	return( 0);
    else if (stat <  18)	return( 1);
    else if (stat <  59)	return( 2);
    else if (stat <  94)	return( 3);
    else if (stat <= 117)	return( 4);
    else			return( 5);
}


/* Returns a character's adjustment to damage		 -JWT-	 */
static int moria_todam_adj()
{
    int stat = p_ptr->state.stat_loaded_cur[A_STR];
    if	  (stat <   4)	return(-2);
    else if (stat <   5)	return(-1);
    else if (stat <  16)	return( 0);
    else if (stat <  17)	return( 1);
    else if (stat <  18)	return( 2);
    else if (stat <  94)	return( 3);
    else if (stat < 109)	return( 4);
    else if (stat <= 117)	return( 5);
    else return( 6);
}

/* Returns a character's adjustment to hit.		 -JWT-	 */
static int moria_tohit_adj()
{
    int total;

    /* First do dexterity adjustments */
    int stat = p_ptr->state.stat_loaded_cur[A_DEX];
    if	  (stat <   4)		total = -3;
    else if (stat <   6)	total = -2;
    else if (stat <   8)	total = -1;
    else if (stat <  16)	total =	 0;
    else if (stat <  17)	total =	 1;
    else if (stat <  18)	total =	 2;
    else if (stat <  69)	total =	 3;
    else if (stat <= 117)	total =	 4;
    else			total =	 5;

    /* Now do strength adjustments */
    stat = p_ptr->state.stat_loaded_cur[A_STR];
    if	  (stat <   4)	total -= 3;
    else if (stat <   5)	total -= 2;
    else if (stat <   7)	total -= 1;
    else if (stat <  18)	total -= 0;
    else if (stat <  94)	total += 1;
    else if (stat < 109)	total += 2;
    else if (stat <= 117)	total += 3;
    else			total += 4;
    return(total);
}




/* Adjustment for wisdom/intelligence in moria */

int stat_adj_moria(int stat)
{


    int value = p_ptr->state.stat_loaded_cur[stat];

    if (value > 117) 		return(7);
    else if (value > 107)	return(6);
    else if (value > 87)	return(5);
    else if (value > 67)	return(4);
    else if (value > 17)	return(3);
    else if (value > 14)	return(2);
    else if (value > 7)		return(1);
    else	return(0);
}



/*
 * Calculate number of spells player should have, and forget,
 * or remember, spells until that number is properly reflected.
 *
 * Note that this function induces various "status" messages,
 * which must be bypasses until the character is created.
 */
void calc_spells(void)
{
    int i, j, k, levels;
    int num_allowed, num_known;
    int percent_spells;

    const magic_type *s_ptr;

    s16b old_spells;

    QString p = cast_spell(MODE_SPELL_NOUN, cp_ptr->spell_book, 1, 0);

    /* Hack -- must be literate */
    if (!cp_ptr->spell_book) return;

    /* Hack -- wait for creation */
    if (!character_generated) return;

    /* Hack -- handle "xtra" mode */
    if (character_xtra) return;

    /* Save the new_spells value */
    old_spells = p_ptr->new_spells;

    /* Determine the number of spells allowed */
    levels = p_ptr->lev - cp_ptr->spell_first + 1;

    /* Hack -- no negative spells */
    if (levels < 0) levels = 0;

    if (game_mode == GAME_NPPMORIA)
    {
        int stat_adj = stat_adj_moria(MORIA_SPELL_STAT);
        if (stat_adj == 7) 		percent_spells = 250;
        else if (stat_adj == 6) percent_spells = 200;
        else if (stat_adj >= 4) percent_spells = 200;
        else if (stat_adj >= 1) percent_spells = 100;
        else 					percent_spells = 0;
    }

    /* Number of 1/100 spells per level */
    else percent_spells = adj_mag_study[SPELL_STAT_SLOT];

    /* Extract total allowed spells (rounded up) */
    num_allowed = (((percent_spells * levels) + 50) / 100);

    /* Assume none known */
    num_known = 0;

    /* Count the number of spells we know */
    for (j = 0; j < PY_MAX_SPELLS; j++)
    {
        /* Count known spells */
        if (p_ptr->spell_flags[j] & PY_SPELL_LEARNED)
        {
            num_known++;
        }
    }

    /* See how many spells we must forget or may learn */
    p_ptr->new_spells = num_allowed - num_known;

    /* Forget spells which are too hard */
    for (i = PY_MAX_SPELLS - 1; i >= 0; i--)
    {
        /* Get the spell */
        j = p_ptr->spell_order[i];

        /* Skip non-spells */
        if (j >= 99) continue;

        /* Get the spell */
        s_ptr = &mp_ptr->info[j];

        /* Skip spells we are allowed to know */
        if (s_ptr->slevel <= p_ptr->lev) continue;

        /* Is it known? */
        if (p_ptr->spell_flags[j] & PY_SPELL_LEARNED)
        {
            /* Mark as forgotten */
            p_ptr->spell_flags[j] |= PY_SPELL_FORGOTTEN;

            /* No longer known */
            p_ptr->spell_flags[j] &= ~PY_SPELL_LEARNED;

            /* Message */
            message(QString("You have forgotten the %1 of %2.") .arg(p) .arg(get_spell_name(cp_ptr->spell_book, j)));

            /* One more can be learned */
            p_ptr->new_spells++;
        }
    }

    /* Forget spells if we know too many spells */
    for (i = PY_MAX_SPELLS - 1; i >= 0; i--)
    {
        /* Stop when possible */
        if (p_ptr->new_spells >= 0) break;

        /* Get the (i+1)th spell learned */
        j = p_ptr->spell_order[i];

        /* Skip unknown spells */
        if (j >= 99) continue;

        /* Forget it (if learned) */
        if (p_ptr->spell_flags[j] & PY_SPELL_LEARNED)
        {
            /* Mark as forgotten */
            p_ptr->spell_flags[j] |= PY_SPELL_FORGOTTEN;

            /* No longer known */
            p_ptr->spell_flags[j] &= ~PY_SPELL_LEARNED;

            /* Message */
            message(QString("You have forgotten the %1 of %2.") .arg(p) .arg(get_spell_name(cp_ptr->spell_book, j)));

            /* One more can be learned */
            p_ptr->new_spells++;
        }
    }

    /* Check for spells to remember */
    for (i = 0; i < PY_MAX_SPELLS; i++)
    {
        /* None left to remember */
        if (p_ptr->new_spells <= 0) break;

        /* Get the next spell we learned */
        j = p_ptr->spell_order[i];

        /* Skip unknown spells */
        if (j >= 99) break;

        /* Get the spell */
        s_ptr = &mp_ptr->info[j];

        /* Skip spells we cannot remember */
        if (s_ptr->slevel > p_ptr->lev) continue;

        /* First set of spells */
        if (p_ptr->spell_flags[j] & PY_SPELL_FORGOTTEN)
        {
            /* No longer forgotten */
            p_ptr->spell_flags[j] &= ~PY_SPELL_FORGOTTEN;

            /* Known once more */
            p_ptr->spell_flags[j] |= PY_SPELL_LEARNED;

            /* Message */
            message(QString("You have remembered the %1 of %2.") .arg(p) .arg(get_spell_name(cp_ptr->spell_book, j)));

            /* One less can be learned */
            p_ptr->new_spells--;
        }
    }

    /* Assume no spells available */
    k = 0;

    /* Count spells that can be learned */
    for (j = 0; j < PY_MAX_SPELLS; j++)
    {

        /* Get the spell */
        s_ptr = &mp_ptr->info[j];

        /* Skip spells we cannot remember */
        if (s_ptr->slevel > p_ptr->lev) continue;

        /* Don't count Ironman Spells. */
        if (p_ptr->spell_flags[j] & PY_SPELL_IRONMAN) continue;

        /* Skip spells we already know */
        if (p_ptr->spell_flags[j] & PY_SPELL_LEARNED)
        {
            continue;
        }

        /* Count it */
        k++;
    }

    /* Cannot learn more spells than exist */
    if (p_ptr->new_spells > k) p_ptr->new_spells = k;

    /* Spell count changed */
    if (old_spells != p_ptr->new_spells)
    {
        /* Message if needed */
        if (p_ptr->new_spells)
        {
            /* Message */
            message(QString("You can learn %1 more %2%3.") .arg(p_ptr->new_spells) .arg(p) .arg((p_ptr->new_spells != 1) ? "s" : ""));
        }

        /* Redraw Study Status */
        p_ptr->redraw |= (PR_STATUSBAR);
    }

}


/*
 * Calculate maximum mana.  You do not need to know any spells.
 * Note that mana is lowered by heavy (or inappropriate) armor.
 *
 * This function induces status messages.
 */
static void calc_mana(void)
{
    int msp, levels, cur_wgt, max_wgt;

    object_type *o_ptr;

    bool old_cumber_glove = p_ptr->state.cumber_glove;
    bool old_cumber_armor = p_ptr->state.cumber_armor;

    /* Hack -- Must be literate */
    if (!cp_ptr->spell_book)
    {
        p_ptr->csp_frac = p_ptr->csp = p_ptr->msp = 0;
        return;
    }

    /* Extract "effective" player level */
    levels = (p_ptr->lev - cp_ptr->spell_first) + 1;
    if (levels > 0)
    {
        long int mana_percent = adj_mag_mana[SPELL_STAT_SLOT];

        if (game_mode == GAME_NPPMORIA)
        {
            int stat_adj = stat_adj_moria(MORIA_SPELL_STAT);
            if (stat_adj == 7) 		mana_percent = 400;
            else if (stat_adj == 6) mana_percent = 300;
            else if (stat_adj == 5) mana_percent = 250;
            else if (stat_adj == 4) mana_percent = 200;
            else if (stat_adj == 3) mana_percent = 150;
            else if (stat_adj >= 1) mana_percent = 100;
            else 					mana_percent = 0;
        }


        msp = 1 + mana_percent * levels / 100;
    }
    else
    {
        levels = 0;
        msp = 0;
    }

    /* Process gloves for those disturbed by them */
    if (cp_ptr->flags & CF_CUMBER_GLOVE)
    {
        /* Assume player is not encumbered by gloves */
        p_ptr->state.cumber_glove = FALSE;

        /* Get the gloves */
        o_ptr = &inventory[INVEN_HANDS];

        /* Examine the gloves */
        o_ptr->update_object_flags();

        /* Normal gloves hurt mage-type spells */
        if (o_ptr->k_idx &&
            !(o_ptr->obj_flags_3 & (TR3_FREE_ACT)) &&
            !((o_ptr->obj_flags_1 & (TR1_DEX)) && (o_ptr->pval > 0)))
        {
            /* Encumbered */
            p_ptr->state.cumber_glove = TRUE;

            /* Reduce mana */
            msp = (3 * msp) / 4;
        }
    }

    /* Assume player not encumbered by armor */
    p_ptr->state.cumber_armor = FALSE;

    /* Weigh the armor */
    cur_wgt = 0;
    cur_wgt += inventory[INVEN_BODY].weight;
    cur_wgt += inventory[INVEN_HEAD].weight;
    cur_wgt += inventory[INVEN_ARM].weight;
    cur_wgt += inventory[INVEN_OUTER].weight;
    cur_wgt += inventory[INVEN_HANDS].weight;
    cur_wgt += inventory[INVEN_FEET].weight;

    /* Determine the weight allowance */
    max_wgt = cp_ptr->spell_weight;

    /* Heavy armor penalizes mana */
    if (((cur_wgt - max_wgt) / 10) > 0)
    {
        /* Encumbered */
        p_ptr->state.cumber_armor = TRUE;

        /* Reduce mana */
        msp -= ((cur_wgt - max_wgt) / 10);
    }

    /* Mana can never be negative */
    if (msp < 0) msp = 0;

    /* Maximum mana has changed */
    if (p_ptr->msp != msp)
    {
        /* Save new limit */
        p_ptr->msp = msp;

        /* Enforce new limit */
        if (p_ptr->csp >= msp)
        {
            p_ptr->csp = msp;
            p_ptr->csp_frac = 0;
        }

        /* Display mana later */
        p_ptr->redraw |= (PR_SIDEBAR_PL);
    }

    /* Hack -- handle "xtra" mode */
    if (character_xtra) return;

    /* Take note when "glove state" changes */
    if (old_cumber_glove != p_ptr->state.cumber_glove)
    {
        /* Message */
        if (p_ptr->state.cumber_glove)
        {
            message(QString("Your covered hands feel unsuitable for spellcasting."));
        }
        else
        {
            message(QString("Your hands feel more suitable for spellcasting."));
        }
    }

    /* Take note when "armor state" changes */
    if (old_cumber_armor != p_ptr->state.cumber_armor)
    {
        /* Message */
        if (p_ptr->state.cumber_armor)
        {
            message(QString("The weight of your equipment is reducing your mana."));
        }
        else
        {
            message(QString("Your equipment is no longer reducing your mana."));
        }
    }
}


/*
 * Calculate the players (maximal) hit points
 *
 * Adjust current hitpoints if necessary
 */
static void calc_hitpoints(void)
{
    long bonus;
    int mhp;

    if (game_mode == GAME_NPPMORIA)
    {
        int con = p_ptr->state.stat_loaded_cur[A_CON];

        if (con < 7) 			bonus = -700;
        else if (con < 17)		bonus = 0;
        else if (con ==  17)	bonus = 100;
        else if (con <  94)		bonus = 200;
        else if (con <= 117)	bonus = 300;
        else					bonus = 400;
    }
    else
    {
        /* Get "1/100th hitpoint bonus per level" value */
        bonus = adj_con_mhp[p_ptr->state.stat_index[A_CON]];
    }

    /* Calculate hitpoints */
    mhp = p_ptr->player_hp[p_ptr->lev-1] + (bonus * p_ptr->lev / 100);

    /* Always have at least one hitpoint per level */
    if (mhp < p_ptr->lev + 1) mhp = p_ptr->lev + 1;

    if (game_mode == GAME_NPPMORIA)
    {
        if (p_ptr->timed[TMD_HERO]) mhp += 10;
        if (p_ptr->timed[TMD_BERSERK]) mhp += 15;
    }

    /* New maximum hitpoints */
    if (p_ptr->mhp != mhp)
    {
        int i = 100;

        /* Get percentage of maximum hp */
        if (p_ptr->mhp) i = ((100 * p_ptr->chp) / p_ptr->mhp);

        /* Save new limit */
        p_ptr->mhp = mhp;

        /* Update current maximum hp */
        p_ptr->chp = ((i * p_ptr->mhp) / 100) + (((i * p_ptr->mhp) % 100 >= 50)	? 1 : 0);

        /* Hack - any change in max hitpoint resets frac */
        p_ptr->chp_frac = 0;

        /* Display hp later */
        p_ptr->redraw |= (PR_SIDEBAR_PL);

    }
}


/*
 * Extract and set the current "lite radius"
 */
static void calc_torch(void)
{
    int i;
    object_type *o_ptr;

    /* Assume no light */
    p_ptr->state.cur_light = 0;

    /* Loop through all wielded items */
    for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
    {
        o_ptr = &inventory[i];

        /* Skip empty slots */
        if (!o_ptr->k_idx) continue;

        /* Don't count the swap weapon */
        if ((birth_swap_weapons) && (i == INVEN_SWAP_WEAPON)) continue;

        /* Examine actual lites */
        if (o_ptr->tval == TV_LIGHT)
        {
            /* Artifact Lites provide permanent, bright, lite */
            if (o_ptr->is_artifact())
            {
                p_ptr->state.cur_light += 3;
                continue;                
            }            

            /* Lanterns (with fuel) provide more lite */
            if ((o_ptr->sval == SV_LIGHT_LANTERN) && (o_ptr->timeout > 0))
            {
                /* Resist Dark means light radius of 3 */
                if ((o_ptr->obj_flags_2 & TR2_RES_DARK) && (o_ptr->pval >= 0))
                {
                  p_ptr->state.cur_light += 3;
                }
                /* Resist Light means light radius of 1 */
                /* The same with cursed lanterns */
                else if ((o_ptr->obj_flags_2 & TR2_RES_LIGHT) || (o_ptr->pval < 0))
                {
                  p_ptr->state.cur_light += 1;
                }
                else
                {
                  p_ptr->state.cur_light += 2;
                }

                continue;
            }

            /* Torches (with fuel) provide some lite */
            if ((o_ptr->sval == SV_LIGHT_TORCH) && (o_ptr->timeout > 0))
            {
                p_ptr->state.cur_light += 1;
                continue;
            }
        }
        else
        {
            /* does this item glow? */
            if (o_ptr->obj_flags_3 & TR3_LIGHT) p_ptr->state.cur_light++;
        }
    }


    /* Player is glowing */
    if (p_ptr->state.light) p_ptr->state.cur_light++;    

    /* Notice changes in the "lite radius" */

    /* Update the visuals */
    p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
}



/*
 * Computes current weight limit in ounces.
 */
int weight_limit(void)
{
    int i;

    /* Weight limit based only on strength */
    i = adj_str_wgt[p_ptr->state.stat_index[A_STR]] * 100;

    /* Return the result */
    return (i);
}

// returned in deca-pounds
int normal_speed_weight_limit(void)
{
    // In Moria it is half the weight limit
    if (game_mode == GAME_NPPMORIA)
    {
        return (adj_str_wgt[p_ptr->state.stat_index[A_STR]] *50);
    }

    // Slowing starts at 60% of max_weight
    // and increases by 1 every extra 10%
    return (60 * adj_str_wgt[p_ptr->state.stat_index[A_STR]]);
}


/*Re-calculate the player stealth*/
void calc_stealth(void)
{
    int old_skill_stl, i;

    /* Save the old stealth */
    old_skill_stl = p_ptr->state.skills[SKILL_STEALTH];

    /* Base skill -- stealth */
    p_ptr->state.skills[SKILL_STEALTH] = rp_ptr->r_stl + cp_ptr->c_stl;

    /* Very simple if flying */
    if (p_ptr->timed[TMD_FLYING])
    {
        /*Very quiet*/
        p_ptr->state.skills[SKILL_STEALTH] += 3;
    }

    /* Scan the equipment */
    for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
    {
        object_type *o_ptr = &inventory[i];

        /* Skip non-objects */
        if (!o_ptr->k_idx) continue;

        /* Don't count the swap weapon */
        if ((birth_swap_weapons) && (i == INVEN_SWAP_WEAPON)) continue;

        /* Extract the item flags */
        o_ptr->update_object_flags();

        /* Affect stealth */
        if (o_ptr->obj_flags_1 & (TR1_STEALTH)) p_ptr->state.skills[SKILL_STEALTH] += o_ptr->pval;
    }

    /* Affect Skill -- stealth (bonus one) */
    p_ptr->state.skills[SKILL_STEALTH] += 1;

    /* Affect Skill -- stealth (Level, by Class) */
    p_ptr->state.skills[SKILL_STEALTH] += (cp_ptr->x_stl * p_ptr->lev / 10);

    /*Feature affects skill*/
    p_ptr->state.skills[SKILL_STEALTH] += f_info[dungeon_info[p_ptr->py][p_ptr->px].feature_idx].f_stealth_adj;

    /* Limit Skill -- stealth from 0 to 30 */
    if (p_ptr->state.skills[SKILL_STEALTH] > 30) p_ptr->state.skills[SKILL_STEALTH] = 30;
    if (p_ptr->state.skills[SKILL_STEALTH] < 0) p_ptr->state.skills[SKILL_STEALTH] = 0;

    /* Recalculate stealth when needed */
    if ((p_ptr->state.skills[SKILL_STEALTH] != old_skill_stl) || (!p_ptr->state.skills[SKILL_STEALTH]))
    {
        /* Assume character is extremely noisy. */
        p_ptr->base_wakeup_chance = WAKEUP_MAX;

        /* For every increase in stealth past 0, multiply wakeup chance by 0.8. */
        for (i = 0; i < p_ptr->state.skills[SKILL_STEALTH]; i++)
        {
            p_ptr->base_wakeup_chance = 4 * p_ptr->base_wakeup_chance / 5;

            /* Always make at least some innate noise */
            if (p_ptr->base_wakeup_chance < WAKEUP_MIN)
            {
                p_ptr->base_wakeup_chance = WAKEUP_MIN;
                break;
            }
        }
    }
}

/* return energy gain for a player or monster */
byte calc_energy_gain(byte speed)
{
    if (game_mode == GAME_NPPMORIA)
    {
        /* Boundry control */
        if (speed < NPPMORIA_LOWEST_SPEED) speed = NPPMORIA_LOWEST_SPEED;
        else if (speed > NPPMORIA_MAX_SPEED) speed = NPPMORIA_MAX_SPEED;

        return (extract_energy_nppmoria[speed - NPPMORIA_LOWEST_SPEED]);
    }

    // Boundry control
    if (speed > NPPANGBAND_MAX_SPEED) speed = 199;

    return (extract_energy_nppangband[speed]);
}


/*
 * Go through player inventory, equipment, and quiver to calculate the player weight.
 */
static void calc_player_weight(void)
{
    int i;
    object_type *o_ptr;

    p_ptr->total_weight = 0;

    /*
     * First, scan the backpack 0 to INVEN_PACK,
     * then the equipment INVEN_WIELD to INVEN_MAX_PACK,
     * then the quiver QUIVER_START to QUIVER_END/ALL_INVEN_TOTAL
     */
    for (i = 0; i < ALL_INVEN_TOTAL; i++)
    {
        o_ptr = &inventory[i];

        /* Skip non-objects */
        if (!o_ptr->k_idx) continue;

        p_ptr->total_weight += o_ptr->weight * o_ptr->number;
    }
}


/*
 * Go through player inventory, equipment, and quiver to calculate inventory count.
 */
static void calc_inven_cnt(void)
{
    int i;
    object_type *o_ptr;

    p_ptr->inven_cnt = 0;

    /*
     * First, scan the backpack 0 to INVEN_PACK,
     * then the equipment INVEN_WIELD to INVEN_MAX_PACK,
     * then the quiver QUIVER_START to QUIVER_END/ALL_INVEN_TOTAL
     */
    for (i = 0; i < INVEN_PACK; i++)
    {
        o_ptr = &inventory[i];

        /* Skip non-objects */
        if (!o_ptr->k_idx) continue;

        p_ptr->inven_cnt++;
    }
}



/* Weapon weight VS strength and dexterity	*/
static int calc_blows_moria(const object_type *o_ptr, player_state *new_state)
{
    int adj_weight;
    int str_index, dex_index;
    int str = new_state->stat_loaded_cur[A_STR];
    int dex = new_state->stat_loaded_cur[A_DEX];

    /* Boundry control */
    if (str > 118) str = 118;
    if (dex  > 118) dex = 118;

    if ((str * 15) < o_ptr->weight)
    {
        return 1;
    }
    else
    {

        /* First figure out the dex index */
        if      (dex < 10)	 dex_index = 0;
        else if (dex<  19)	 dex_index = 1;
        else if (dex < 68)	 dex_index = 2;
        else if (dex < 108)	 dex_index = 3;
        else if (dex < 118)	 dex_index = 4;
        else		 dex_index = 5;

        /* Now do the str_index */

        adj_weight = ((str * 10) / o_ptr->weight);
        if      (adj_weight < 2)	str_index = 0;
        else if (adj_weight < 3)	str_index = 1;
        else if (adj_weight < 4)	str_index = 2;
        else if (adj_weight < 5)	str_index = 3;
        else if (adj_weight < 7)	str_index = 4;
        else if (adj_weight < 9)	str_index = 5;
        else			str_index = 6;
        return moria_blows_table[str_index][dex_index];
    }
}


/*
 * Calculate the number of attacks a player gets with a weapon per round.
 * Does not factor in extra attacks from weapon flags
 */
int calc_blows(object_type *o_ptr, player_state *new_state)
{
    int str_index, dex_index, str_ind, dex_ind;

    int divide_by, new_blows;

    if (game_mode == GAME_NPPMORIA)
    {
        return calc_blows_moria(o_ptr, new_state);
    }

    str_ind = new_state->stat_index[A_STR];
    dex_ind = new_state->stat_index[A_DEX];

    /* Boundary control */
    if (str_ind <  0) str_ind =  0;
    if (str_ind > STAT_TABLE_MAX_VALUE) str_ind = STAT_TABLE_MAX_VALUE;
    if (dex_ind <  0) dex_ind =  0;
    if (dex_ind > STAT_TABLE_MAX_VALUE) dex_ind = STAT_TABLE_MAX_VALUE;

    /* Enforce a minimum "weight" (tenth pounds) */
    divide_by = ((o_ptr->weight < cp_ptr->min_weight) ? cp_ptr->min_weight : o_ptr->weight);

    /* Get the strength vs weight */
    str_index = (adj_str_blow[str_ind] * cp_ptr->att_multiply / divide_by);

    /* Maximal value */
    if (str_index > 11) str_index = 11;

    /* Index by dexterity */
    dex_index = (adj_dex_blow[dex_ind]);

    /* Maximal value */
    if (dex_index > 11) dex_index = 11;

    /* Use the blows table */
    new_blows = blows_table[str_index][dex_index];

    /* Maximal value */
    if (new_blows > cp_ptr->max_attacks) new_blows = cp_ptr->max_attacks;

    /* Require at least one blow */
    if (new_state->num_blow < 1) new_state->num_blow = 1;

    return (new_blows);
}


/*
 * Calculate the players current "state", taking into account
 * not only race/class intrinsics, but also objects being worn
 * and temporary spell effects.
 *
 * See also calc_mana() and calc_hitpoints().
 *
 * Take note of the new "speed code", in particular, a very strong
 * player will start slowing down as soon as he reaches 150 pounds,
 * but not until he reaches 450 pounds will he be half as fast as
 * a normal kobold.  This both hurts and helps the player, hurts
 * because in the old days a player could just avoid 300 pounds,
 * and helps because now carrying 300 pounds is not very painful.
 *
 * The "weapon" and "bow" do *not* add to the bonuses to hit or to
 * damage, since that would affect non-combat things.  These values
 * are actually added in later, at the appropriate place.
 *
 *  If id_only is true, calc_bonuses() will only use the known
 * information of objects; thus it returns what the player _knows_
 * the character state to be.
 *
 * This function induces various "status" messages.
 */
void calc_bonuses(object_type calc_inven[], player_state *new_state, bool id_only)
{

    int i, j, hold;

    int old_speed;

    bool old_telepathy;
    bool old_see_inv;


    int old_known_ac;
    int old_known_to_a;

    int extra_blows = 0;
    int extra_shots = 0;
    int extra_might = 0;

    int old_stat_loaded_max[A_MAX];
    int old_stat_loaded_cur[A_MAX];
    int old_stat_index[A_MAX];

    bool old_heavy_shoot;
    bool old_heavy_wield;
    bool old_icky_wield;

    object_type *o_ptr;

    u32b f1, f2, f3, fn;


    /*** Memorize ***/

    /* Save the old speed */
    old_speed = new_state->p_speed;

    /* confirm equipment weight and backpack quantity */
    if (!id_only)
    {
        calc_player_weight();
        calc_inven_cnt();
    }

    /* Save the old vision stuff */
    old_telepathy = new_state->telepathy;
    old_see_inv = new_state->see_inv;

    /* Save the old armor class */
    old_known_ac = new_state->known_ac;
    old_known_to_a = new_state->known_to_a;

    /* Save the old stats */
    for (i = 0; i < A_MAX; i++)
    {
        old_stat_loaded_max[i] = new_state->stat_loaded_max[i];
        old_stat_loaded_cur[i] = new_state->stat_loaded_cur[i];
        old_stat_index[i] = new_state->stat_index[i];
    }

    old_heavy_shoot = new_state->heavy_shoot;
    old_heavy_wield = new_state->heavy_wield;
    old_icky_wield = new_state->icky_wield;

    // Hack - these are handled by calc_mana;
    bool old_cumber_glove = new_state->cumber_glove;
    bool old_cumber_armor = new_state->cumber_armor;

    /*** Reset ***/

    /* Reset player speed */

    new_state->player_state_wipe();

    //  Hack - part 2 reinstante the old cumber_glove and armor settings.
    new_state->cumber_glove = old_cumber_glove;
    new_state->cumber_armor = old_cumber_armor;

    if (game_mode == GAME_NPPMORIA) new_state->p_speed = NPPMORIA_NORMAL_SPEED;
    else new_state->p_speed = 110;

    /* Reset "blow" info */
    new_state->num_blow = 1;


    /*** Extract race/class info ***/

    /* Base infravision plus current lite radius */
    new_state->see_infra = rp_ptr->infra;

    /* Base skill -- disarming */
    new_state->skills[SKILL_DISARM] = rp_ptr->r_dis + cp_ptr->c_dis;

    /* Base skill -- magic devices */
    new_state->skills[SKILL_DEVICE] = rp_ptr->r_dev + cp_ptr->c_dev;

    /* Base skill -- saving throw */
    new_state->skills[SKILL_SAVE] = rp_ptr->r_sav + cp_ptr->c_sav;

    /* Base skill -- searching ability */
    new_state->skills[SKILL_SEARCH_CHANCE] = rp_ptr->r_srh + cp_ptr->c_srh;

    /* Base skill -- searching frequency */
    new_state->skills[SKILL_SEARCH_FREQUENCY] = rp_ptr->r_fos + cp_ptr->c_fos;

    /* Base skill -- combat (normal) */
    new_state->skills[SKILL_TO_HIT_MELEE] = rp_ptr->r_thn + cp_ptr->c_thn;

    /* Base skill -- combat (shooting) */
    new_state->skills[SKILL_TO_HIT_BOW] = rp_ptr->r_thb + cp_ptr->c_thb;

    /* Base skill -- combat (throwing) */
    new_state->skills[SKILL_TO_HIT_THROW] = rp_ptr->r_thb + cp_ptr->c_thb;

    /* Base skill -- digging */
    new_state->skills[SKILL_DIGGING] = 0;


    /*** Analyze player ***/

    /* Extract the player flags */
    player_flags(&f1, &f2, &f3, &fn);

    /* Good flags */
    if (f3 & (TR3_SLOW_DIGEST)) 	new_state->slow_digest = TRUE;
    if (f3 & (TR3_FEATHER)) 		new_state->ffall = TRUE;
    if (f3 & (TR3_LIGHT)) 			new_state->light = TRUE;
    if (f3 & (TR3_REGEN)) 			new_state->regenerate = TRUE;
    if (f3 & (TR3_TELEPATHY)) 		new_state->telepathy = TRUE;
    if (f3 & (TR3_SEE_INVIS)) 		new_state->see_inv = new_state->see_inv_perm = TRUE;
    if (f3 & (TR3_FREE_ACT)) 		new_state->free_act = TRUE;
    if (f3 & (TR3_HOLD_LIFE)) 		new_state->hold_life = TRUE;

    /* Weird flags */
    if (f3 & (TR3_BLESSED)) 		new_state->bless_blade = TRUE;

    /* Bad flags */
    if (f3 & (TR3_IMPACT)) 			new_state->impact = TRUE;
    if (f3 & (TR3_AGGRAVATE)) 		new_state->aggravate = TRUE;
    if (f3 & (TR3_TELEPORT)) 		new_state->teleport = TRUE;
    if (f3 & (TR3_DRAIN_EXP)) 		new_state->exp_drain = TRUE;

    /* Immunity flags */
    if (f2 & (TR2_IM_FIRE)) 		new_state->immune_fire = TRUE;
    if (f2 & (TR2_IM_ACID)) 		new_state->immune_acid = TRUE;
    if (f2 & (TR2_IM_COLD)) 		new_state->immune_cold = TRUE;
    if (f2 & (TR2_IM_ELEC)) 		new_state->immune_elec = TRUE;
    if (f2 & (TR2_IM_POIS)) 		new_state->immune_pois = TRUE;

    /* Resistance flags */
    if (f2 & (TR2_RES_ACID)) 		new_state->resist_acid = TRUE;
    if (f2 & (TR2_RES_ELEC)) 		new_state->resist_elec = TRUE;
    if (f2 & (TR2_RES_FIRE)) 		new_state->resist_fire = TRUE;
    if (f2 & (TR2_RES_COLD)) 		new_state->resist_cold = TRUE;
    if (f2 & (TR2_RES_POIS)) 		new_state->resist_pois = TRUE;
    if (f2 & (TR2_RES_FEAR)) 		new_state->resist_fear = TRUE;
    if (f2 & (TR2_RES_LIGHT)) 		new_state->resist_light = TRUE;
    if (f2 & (TR2_RES_DARK)) 		new_state->resist_dark = TRUE;
    if (f2 & (TR2_RES_BLIND)) 		new_state->resist_blind = TRUE;
    if (f2 & (TR2_RES_CONFU)) 		new_state->resist_confu = TRUE;
    if (f2 & (TR2_RES_SOUND)) 		new_state->resist_sound = TRUE;
    if (f2 & (TR2_RES_SHARD)) 		new_state->resist_shard = TRUE;
    if (f2 & (TR2_RES_NEXUS)) 		new_state->resist_nexus = TRUE;
    if (f2 & (TR2_RES_NETHR)) 		new_state->resist_nethr = TRUE;
    if (f2 & (TR2_RES_CHAOS)) 		new_state->resist_chaos = TRUE;
    if (f2 & (TR2_RES_DISEN)) 		new_state->resist_disen = TRUE;

    if (fn & (TN1_NATIVE_LAVA))     new_state->native_lava = TRUE;
    if (fn & (TN1_NATIVE_ICE))      new_state->native_ice = TRUE;
    if (fn & (TN1_NATIVE_OIL))      new_state->native_oil = TRUE;
    if (fn & (TN1_NATIVE_FIRE))     new_state->native_fire = TRUE;
    if (fn & (TN1_NATIVE_SAND))     new_state->native_sand = TRUE;
    if (fn & (TN1_NATIVE_FOREST))   new_state->native_forest = TRUE;
    if (fn & (TN1_NATIVE_WATER))    new_state->native_water = TRUE;
    if (fn & (TN1_NATIVE_ACID))     new_state->native_acid = TRUE;
    if (fn & (TN1_NATIVE_MUD))      new_state->native_mud = TRUE;

    /* Sustain flags */
    if (f2 & (TR2_SUST_STR)) 		new_state->sustain_str = TRUE;
    if (f2 & (TR2_SUST_INT)) 		new_state->sustain_int = TRUE;
    if (f2 & (TR2_SUST_WIS)) 		new_state->sustain_wis = TRUE;
    if (f2 & (TR2_SUST_DEX)) 		new_state->sustain_dex = TRUE;
    if (f2 & (TR2_SUST_CON)) 		new_state->sustain_con = TRUE;
    if (f2 & (TR2_SUST_CHR)) 		new_state->sustain_chr = TRUE;

    new_state->p_flags_native_no_temp = new_state->p_flags_native_with_temp = fn;

    /*** Analyze equipment ***/

    /* Scan the equipment */
    for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
    {
        o_ptr = &calc_inven[i];

        /* Skip non-objects */
        if (!o_ptr->k_idx) continue;

        /* Don't count the swap weapon */
        if ((birth_swap_weapons) && (i == INVEN_SWAP_WEAPON)) continue;

        /* Extract the item flags */
        o_ptr->update_object_flags();
        if (id_only)
        {
            f1 = o_ptr->obj_flags_1;
            f2 = o_ptr->obj_flags_2;
            f3 = o_ptr->obj_flags_3;
            fn = o_ptr->obj_flags_native;
        }
        else
        {
            f1 = o_ptr->known_obj_flags_1;
            f2 = o_ptr->known_obj_flags_2;
            f3 = o_ptr->known_obj_flags_3;
            fn = o_ptr->known_obj_flags_native;
        }

        /* Affect stats */
        if (f1 & (TR1_STR)) 		new_state->stat_equip[A_STR] += o_ptr->pval;
        if (f1 & (TR1_INT)) 		new_state->stat_equip[A_INT] += o_ptr->pval;
        if (f1 & (TR1_WIS)) 		new_state->stat_equip[A_WIS] += o_ptr->pval;
        if (f1 & (TR1_DEX)) 		new_state->stat_equip[A_DEX] += o_ptr->pval;
        if (f1 & (TR1_CON)) 		new_state->stat_equip[A_CON] += o_ptr->pval;
        if (f1 & (TR1_CHR)) 		new_state->stat_equip[A_CHR] += o_ptr->pval;

        /* Affect searching ability (factor of five) */
        if (f1 & (TR1_SEARCH)) 		new_state->skills[SKILL_SEARCH_CHANCE] += (o_ptr->pval * 5);

        /* Affect searching frequency (factor of five) */
        if (f1 & (TR1_SEARCH)) 		new_state->skills[SKILL_SEARCH_FREQUENCY] += (o_ptr->pval * 5);

        /* Affect infravision */
        if (f1 & (TR1_INFRA)) 		new_state->see_infra += o_ptr->pval;

        /* Affect digging (factor of 20) */
        if (f1 & (TR1_TUNNEL)) 		new_state->skills[SKILL_DIGGING] += (o_ptr->pval * 20);

        /* Affect speed */
        if (f1 & (TR1_SPEED))
        {
            if (game_mode == GAME_NPPMORIA)
            {
                if (o_ptr->pval > 0) new_state->p_speed++;
                else if (o_ptr->pval < 0) new_state->p_speed--;
            }
            else new_state->p_speed += o_ptr->pval;
        }


        /* Affect blows */
        if (f1 & (TR1_BLOWS)) 		extra_blows += o_ptr->pval;

        /* Affect shots */
        if (f1 & (TR1_SHOTS)) 		extra_shots += o_ptr->pval;

        /* Affect Might */
        if (f1 & (TR1_MIGHT)) 		extra_might += o_ptr->pval;

        /* Good flags */
        if (f3 & (TR3_SLOW_DIGEST)) 	new_state->slow_digest = TRUE;
        if (f3 & (TR3_FEATHER)) 		new_state->ffall = TRUE;
        if (f3 & (TR3_REGEN)) 			new_state->regenerate = TRUE;
        if (f3 & (TR3_TELEPATHY)) 		new_state->telepathy = TRUE;
        if (f3 & (TR3_SEE_INVIS)) 		new_state->see_inv = new_state->see_inv_perm = TRUE;
        if (f3 & (TR3_FREE_ACT)) 		new_state->free_act = TRUE;
        if (f3 & (TR3_HOLD_LIFE)) 		new_state->hold_life = TRUE;

        /* Weird flags */
        if (f3 & (TR3_BLESSED)) 		new_state->bless_blade = TRUE;

        /* Bad flags */
        if (f3 & (TR3_IMPACT)) 			new_state->impact = TRUE;
        if (f3 & (TR3_AGGRAVATE)) 		new_state->aggravate = TRUE;
        if (f3 & (TR3_TELEPORT)) 		new_state->teleport = TRUE;
        if (f3 & (TR3_DRAIN_EXP)) 		new_state->exp_drain = TRUE;

        /* Immunity flags */
        if (f2 & (TR2_IM_FIRE)) 		new_state->immune_fire = TRUE;
        if (f2 & (TR2_IM_ACID)) 		new_state->immune_acid = TRUE;
        if (f2 & (TR2_IM_COLD)) 		new_state->immune_cold = TRUE;
        if (f2 & (TR2_IM_ELEC)) 		new_state->immune_elec = TRUE;
        if (f2 & (TR2_IM_POIS)) 		new_state->immune_pois = TRUE;

        /* Resistance flags */
        if (f2 & (TR2_RES_ACID)) 		new_state->resist_acid = TRUE;
        if (f2 & (TR2_RES_ELEC)) 		new_state->resist_elec = TRUE;
        if (f2 & (TR2_RES_FIRE)) 		new_state->resist_fire = TRUE;
        if (f2 & (TR2_RES_COLD)) 		new_state->resist_cold = TRUE;
        if (f2 & (TR2_RES_POIS)) 		new_state->resist_pois = TRUE;
        if (f2 & (TR2_RES_FEAR)) 		new_state->resist_fear = TRUE;
        if (f2 & (TR2_RES_LIGHT)) 		new_state->resist_light = TRUE;
        if (f2 & (TR2_RES_DARK)) 		new_state->resist_dark = TRUE;
        if (f2 & (TR2_RES_BLIND)) 		new_state->resist_blind = TRUE;
        if (f2 & (TR2_RES_CONFU)) 		new_state->resist_confu = TRUE;
        if (f2 & (TR2_RES_SOUND)) 		new_state->resist_sound = TRUE;
        if (f2 & (TR2_RES_SHARD)) 		new_state->resist_shard = TRUE;
        if (f2 & (TR2_RES_NEXUS)) 		new_state->resist_nexus = TRUE;
        if (f2 & (TR2_RES_NETHR)) 		new_state->resist_nethr = TRUE;
        if (f2 & (TR2_RES_CHAOS)) 		new_state->resist_chaos = TRUE;
        if (f2 & (TR2_RES_DISEN)) 		new_state->resist_disen = TRUE;

        if (fn & (TN1_NATIVE_LAVA))     new_state->native_lava = TRUE;
        if (fn & (TN1_NATIVE_ICE))      new_state->native_ice = TRUE;
        if (fn & (TN1_NATIVE_OIL))      new_state->native_oil = TRUE;
        if (fn & (TN1_NATIVE_FIRE))     new_state->native_fire = TRUE;
        if (fn & (TN1_NATIVE_SAND))     new_state->native_sand = TRUE;
        if (fn & (TN1_NATIVE_FOREST))   new_state->native_forest = TRUE;
        if (fn & (TN1_NATIVE_WATER))    new_state->native_water = TRUE;
        if (fn & (TN1_NATIVE_ACID))     new_state->native_acid = TRUE;
        if (fn & (TN1_NATIVE_MUD))      new_state->native_mud = TRUE;

        /* Sustain flags */
        if (f2 & (TR2_SUST_STR)) 		new_state->sustain_str = TRUE;
        if (f2 & (TR2_SUST_INT)) 		new_state->sustain_int = TRUE;
        if (f2 & (TR2_SUST_WIS)) 		new_state->sustain_wis = TRUE;
        if (f2 & (TR2_SUST_DEX)) 		new_state->sustain_dex = TRUE;
        if (f2 & (TR2_SUST_CON)) 		new_state->sustain_con = TRUE;
        if (f2 & (TR2_SUST_CHR)) 		new_state->sustain_chr = TRUE;

        /* Modify the base armor class */
        new_state->ac += o_ptr->ac;

        /* The base armor class is always known */
        new_state->known_ac += o_ptr->ac;

        /* Apply the bonuses to armor class */
        new_state->to_a += o_ptr->to_a;

        /* Apply the mental bonuses to armor class, if known */
        if (o_ptr->is_known()) new_state->known_to_a += o_ptr->to_a;

        /* Hack -- do not apply "weapon" bonuses */
        if (i == INVEN_WIELD) continue;

        /* Hack -- do not apply "bow" bonuses */
        if (i == INVEN_BOW) continue;

        /* Apply the bonuses to hit/damage (if known) */
        if ((!id_only) || o_ptr->is_known())
        {
            new_state->to_h += o_ptr->to_h;
            new_state->to_d += o_ptr->to_d;
        }

        /* Apply the mental bonuses to hit/damage, if known */
        if (o_ptr->is_known())
        {
            new_state->known_to_h += o_ptr->to_h;
            new_state->known_to_d += o_ptr->to_d;
        }

        new_state->p_flags_native_with_temp |= fn;
        new_state->p_flags_native_no_temp |= fn;
    }

    /* Scan the quiver */
    for (i = QUIVER_START; i < QUIVER_END; i++)
    {
        /* Get the object */
        o_ptr = &calc_inven[i];

        /* Ignore empty objects */
        if (!o_ptr->k_idx) continue;

        /* Found cursed ammo */
        if (o_ptr->is_cursed())
        {
            /* Remember it */
            new_state->cursed_quiver = TRUE;

            /* Done */
            break;
        }
    }

    /*finally, add lite radius to infravision range*/
    if (new_state->see_infra) new_state->see_infra += new_state->cur_light;


    /*** Handle stats ***/

    /* Calculate stats */
    for (i = 0; i < A_MAX; i++)
    {
        int add, top, use, ind;

        /* Extract modifier */
        add = new_state->stat_equip[i];

        /* Maximize mode */
        if (birth_maximize)
        {
            /* Modify the stats for race/class */
            add += (rp_ptr->r_adj[i] + cp_ptr->c_adj[i]);
        }

        /* Add permanent stat increases */
        add += p_ptr->stat_quest_add[i];

        /* Extract the new "stat_top" value for the stat */
        top = modify_stat_value(p_ptr->stat_base_max[i], add);

        /* Stats max out at 118 in Moria */
        if (game_mode == GAME_NPPMORIA)
        {
            if (top > 117) top = 118;
        }

        /* Save the new value */
        new_state->stat_loaded_max[i] = top;

        /* Extract the new "stat_use" value for the stat */
        use = modify_stat_value(p_ptr->stat_base_cur[i], add);

        /* Stats max out at 118 in Moria */
        if (game_mode == GAME_NPPMORIA)
        {
            if (use > 117) use = 118;
        }

        /* Save the new value */
        new_state->stat_loaded_cur[i] = use;

        /* Values: 3, 4, ..., 17 */
        if (use <= 18) ind = (use - 3);

        /* Ranges: 18/00-18/09, ..., 18/210-18/219 */
        else if (use <= 18+219) ind = (15 + (use - 18) / 10);

        /* Range: 18/220+ */
        else ind = (STAT_TABLE_MAX_VALUE);

        /* Save the new index */
        new_state->stat_index[i] = ind;
    }



    /*** Temporary flags ***/

    /* Apply temporary "stun" */
    if (p_ptr->timed[TMD_STUN] > STUN_HEAVY)
    {
        new_state->to_h -= 20;
        new_state->known_to_h -= 20;
        new_state->to_d -= 20;
        new_state->known_to_d -= 20;
    }
    else if (p_ptr->timed[TMD_STUN])
    {
        new_state->to_h -= 5;
        new_state->known_to_h -= 5;
        new_state->to_d -= 5;
        new_state->known_to_d -= 5;
    }

    /* Invulnerability */
    if (p_ptr->timed[TMD_INVULN])
    {
        new_state->to_a += 100;
        new_state->known_to_a += 100;
    }

    /* Temporary blessing */
    if (p_ptr->timed[TMD_BLESSED])
    {
        new_state->to_a += 5;
        new_state->known_to_a += 5;
        new_state->to_h += 10;
        new_state->known_to_h += 10;
    }

    /* Temporary shield */
    if (p_ptr->timed[TMD_SHIELD])
    {
        new_state->to_a += 50;
        new_state->known_to_a += 50;
    }

    /* Temporary "Hero" */
    if (p_ptr->timed[TMD_HERO])
    {
        new_state->to_h += 12;
        new_state->known_to_h += 12;
    }

    /* Temporary "Berserk" */
    if (p_ptr->timed[TMD_BERSERK])
    {
        new_state->to_h += 24;
        new_state->known_to_h += 24;
        new_state->to_a -= 10;
        new_state->known_to_a -= 10;
    }

    /* Temporary "fast" */
    if (p_ptr->timed[TMD_FAST])
    {
        if (game_mode == GAME_NPPMORIA) new_state->p_speed++;
        else new_state->p_speed += 10;
    }

    /* Temporary "slow" */
    if (p_ptr->timed[TMD_SLOW])
    {
        if (game_mode == GAME_NPPMORIA) new_state->p_speed--;
        else new_state->p_speed -= 10;
    }

    /* Temporary see invisible */
    if (p_ptr->timed[TMD_SINVIS])
    {
        new_state->see_inv = TRUE;
    }

    /* Temporary infravision boost */
    if (p_ptr->timed[TMD_SINFRA])
    {
        new_state->see_infra += 5;
    }

    /*Manually add the temporary native flags.  Assume known*/
    if (p_ptr->timed[TMD_NAT_LAVA])
    {
        new_state->p_flags_native_with_temp |= P_NATIVE_LAVA;
        new_state->native_lava = TRUE;
    }
    if (p_ptr->timed[TMD_NAT_OIL])
    {
        new_state->p_flags_native_with_temp |= P_NATIVE_OIL;
        new_state->native_oil = TRUE;
    }
    if (p_ptr->timed[TMD_NAT_SAND])
    {
        new_state->p_flags_native_with_temp |= P_NATIVE_SAND;
        new_state->native_sand = TRUE;
    }
    if (p_ptr->timed[TMD_NAT_TREE])
    {
        new_state->p_flags_native_with_temp |= P_NATIVE_FOREST;
        new_state->native_forest = TRUE;
    }
    if (p_ptr->timed[TMD_NAT_WATER])
    {
        new_state->p_flags_native_with_temp |= P_NATIVE_WATER;
        new_state->native_water = TRUE;
    }
    if (p_ptr->timed[TMD_NAT_MUD])
    {
        new_state->p_flags_native_with_temp |= P_NATIVE_MUD;
        new_state->native_mud = TRUE;
    }

    //Add the combined nativities
    if (new_state->native_lava)
    {
        if (new_state->native_water) new_state->native_boiling_water = TRUE;
        if (new_state->native_mud)   new_state->native_boiling_mud = TRUE;
    }

    /*** Special flags ***/

    /* Hack -- Hero/Shero -> Res fear */
    if (p_ptr->timed[TMD_HERO] || p_ptr->timed[TMD_BERSERK])
    {
        new_state->resist_fear = TRUE;
    }


    /*** Analyze weight ***/

    /* Extract the current weight (in tenth pounds) */
    j = p_ptr->total_weight;

    /* Extract the "weight limit" (in tenth pounds) */
    i = weight_limit();

    /* Apply "encumbrance" from weight */
    if (j > i / 2)
    {
        if (game_mode == GAME_NPPMORIA) new_state->p_speed--;
        else new_state->p_speed -= ((j - (i / 2)) / (i / 10));
    }

    /* Bloating slows the player down (a little) */
    if (p_ptr->food >= PY_FOOD_MAX)
    {
        if (game_mode == GAME_NPPMORIA) new_state->p_speed--;
        else new_state->p_speed -= 10;
    }

    /* Searching slows the player down */
    if (p_ptr->searching)
    {
        if (game_mode == GAME_NPPMORIA) new_state->p_speed--;
        else new_state->p_speed -= 10;
    }

    /* Sanity check on extreme speeds */
    if (game_mode == GAME_NPPMORIA)
    {
        if (new_state->p_speed < NPPMORIA_LOWEST_SPEED) 		new_state->p_speed = NPPMORIA_LOWEST_SPEED;
        else if (new_state->p_speed > NPPMORIA_MAX_SPEED) 		new_state->p_speed = NPPMORIA_MAX_SPEED;
    }
    else
    {
        if (new_state->p_speed < 0) 		new_state->p_speed = 0;
        if (new_state->p_speed > 199) 		new_state->p_speed = 199;
    }


    /*** Apply modifier bonuses ***/

    /*** Modify skills ***/

    if (game_mode == GAME_NPPMORIA)
    {
        /* Affect Skill -- magic devices (INT) */
        new_state->skills[SKILL_DEVICE] += (moria_class_level_adj[p_ptr->pclass][MORIA_CLA_DEVICE] * p_ptr->lev / 3);
        new_state->skills[SKILL_DISARM] += (moria_class_level_adj[p_ptr->pclass][MORIA_CLA_DISARM] * p_ptr->lev / 3);
        new_state->skills[SKILL_TO_HIT_MELEE] += (moria_class_level_adj[p_ptr->pclass][MORIA_CLA_BTH] * p_ptr->lev);
        new_state->skills[SKILL_SAVE] += (moria_class_level_adj[p_ptr->pclass][MORIA_CLA_SAVE] * p_ptr->lev / 3);
        /* Throwing and bows used the same factor in Moria */
        new_state->skills[SKILL_TO_HIT_BOW] += (moria_class_level_adj[p_ptr->pclass][MORIA_CLA_BTHB] * p_ptr->lev);
        new_state->skills[SKILL_TO_HIT_THROW] += (moria_class_level_adj[p_ptr->pclass][MORIA_CLA_BTHB] * p_ptr->lev);

        new_state->to_a += moria_toac_adj();
        new_state->to_d += moria_todam_adj();
        new_state->to_h += moria_tohit_adj();

        new_state->known_to_a += moria_toac_adj();
        new_state->known_to_d += moria_todam_adj();
        new_state->known_to_h += moria_tohit_adj();

    }
    else
    {
        /* Affect Skill -- disarming (DEX and INT) */
        new_state->skills[SKILL_DISARM] += adj_dex_dis[new_state->stat_index[A_DEX]];
        new_state->skills[SKILL_DISARM] += adj_int_dis[new_state->stat_index[A_INT]];

        /* Affect Skill -- magic devices (INT) */
        new_state->skills[SKILL_DEVICE] += adj_int_dev[new_state->stat_index[A_INT]];

        /* Affect Skill -- saving throw (WIS) */
        new_state->skills[SKILL_SAVE] += adj_wis_sav[new_state->stat_index[A_WIS]];

        /* Actual Modifier Bonuses (Un-inflate stat bonuses) */
        new_state->to_a += ((int)(adj_dex_ta[new_state->stat_index[A_DEX]]) - 128);
        new_state->to_d += ((int)(adj_str_td[new_state->stat_index[A_STR]]) - 128);
        new_state->to_h += ((int)(adj_dex_th[new_state->stat_index[A_DEX]]) - 128);
        new_state->to_h += ((int)(adj_str_th[new_state->stat_index[A_STR]]) - 128);

        /* Displayed Modifier Bonuses (Un-inflate stat bonuses) */
        new_state->known_to_a += ((int)(adj_dex_ta[new_state->stat_index[A_DEX]]) - 128);
        new_state->known_to_d += ((int)(adj_str_td[new_state->stat_index[A_STR]]) - 128);
        new_state->known_to_h += ((int)(adj_dex_th[new_state->stat_index[A_DEX]]) - 128);
        new_state->known_to_h += ((int)(adj_str_th[new_state->stat_index[A_STR]]) - 128);
    }



    /* Affect Skill -- digging (STR) */
    new_state->skills[SKILL_DIGGING] += adj_str_dig[new_state->stat_index[A_STR]];

    /* Affect Skill -- disarming (Level, by Class) */
    new_state->skills[SKILL_DISARM] += (cp_ptr->x_dis * p_ptr->lev / 10);

    /* Affect Skill -- magic devices (Level, by Class) */
    new_state->skills[SKILL_DEVICE] += (cp_ptr->x_dev * p_ptr->lev / 10);

    /* Affect Skill -- saving throw (Level, by Class) */
    new_state->skills[SKILL_SAVE] += (cp_ptr->x_sav * p_ptr->lev / 10);

    /* Affect Skill -- search ability (Level, by Class) */
    new_state->skills[SKILL_SEARCH_CHANCE] += (cp_ptr->x_srh * p_ptr->lev / 10);

    /* Affect Skill -- search frequency (Level, by Class) */
    new_state->skills[SKILL_SEARCH_FREQUENCY] += (cp_ptr->x_fos * p_ptr->lev / 10);

    /* Affect Skill -- combat (normal) (Level, by Class) */
    new_state->skills[SKILL_TO_HIT_MELEE] += (cp_ptr->x_thn * p_ptr->lev / 10);

    /* Affect Skill -- combat (shooting) (Level, by Class) */
    new_state->skills[SKILL_TO_HIT_BOW] += (cp_ptr->x_thb * p_ptr->lev / 10);

    /* Affect Skill -- combat (throwing) (Level, by Class) */
    new_state->skills[SKILL_TO_HIT_THROW] += (cp_ptr->x_thb * p_ptr->lev / 10);

    /* Limit Skill -- digging from 1 up */
    if (new_state->skills[SKILL_DIGGING] < 1) new_state->skills[SKILL_DIGGING] = 1;

    if (cp_ptr->flags & (CF_ROGUE_COMBAT)) new_state->skills[SKILL_TO_HIT_THROW] += 20 +  p_ptr->lev / 3;
    if (cp_ptr->flags & (CF_BRIGAND_COMBAT)) new_state->skills[SKILL_TO_HIT_THROW] += 20 +  p_ptr->lev / 3;

    /* Obtain the "hold" value */
    hold = adj_str_hold[new_state->stat_index[A_STR]];


    /*** Analyze current bow ***/

    /* Examine the "current bow" */
    o_ptr = &calc_inven[INVEN_BOW];

    /* Don't count the swap weapon */
    if (birth_swap_weapons)
    {
        o_ptr = &calc_inven[INVEN_MAIN_WEAPON];
    }


    if (o_ptr->is_bow())
    {
        /* It is hard to hold a heavy bow */
        if (hold < o_ptr->weight / 10)
        {
            /* Hard to wield a heavy bow */
            new_state->to_h += 2 * (hold - o_ptr->weight / 10);
            new_state->known_to_h += 2 * (hold - o_ptr->weight / 10);

            /* Heavy Bow */
            new_state->heavy_shoot = TRUE;
        }

        /* Analyze launcher */
        if (o_ptr->k_idx)
        {
            /* Get to shoot */
            new_state->num_fire = 1;

            /* Analyze the launcher */
            switch (o_ptr->sval)
            {
                /* Sling and ammo */
                case SV_SLING:
                {
                    new_state->ammo_tval = TV_SHOT;
                    new_state->ammo_mult = 2;

                    /*Hack - Rogues get increased skill with slings*/
                    if (cp_ptr->flags & CF_ROGUE_COMBAT)
                    {
                        new_state->skills[SKILL_TO_HIT_BOW] += 3 + p_ptr->lev / 4;
                    }
                    break;
                }

                /* Short Bow and Arrow */
                case SV_SHORT_BOW:
                {
                    new_state->ammo_tval = TV_ARROW;
                    new_state->ammo_mult = 2;
                    break;
                }

                /* Long Bow and Arrow */
                case SV_LONG_BOW:
                {
                    new_state->ammo_tval = TV_ARROW;
                    new_state->ammo_mult = 3;
                    break;
                }

                /* Composite Bow and Arrow */
                case SV_COMPOSITE_BOW:
                {
                    new_state->ammo_tval = TV_ARROW;
                    new_state->ammo_mult = 4;
                    break;
                }

                /* Light Crossbow and Bolt */
                case SV_LIGHT_XBOW:
                {
                    new_state->ammo_tval = TV_BOLT;
                    new_state->ammo_mult = 3;
                    break;
                }

                /* Heavy Crossbow and Bolt */
                case SV_HEAVY_XBOW:
                {
                    new_state->ammo_tval = TV_BOLT;
                    new_state->ammo_mult = 4;
                    break;
                }
            }

            /* Apply special flags */
            if (o_ptr->k_idx && !new_state->heavy_shoot)
            {
                /* Extra shots */
                new_state->num_fire += extra_shots;

                /* Extra might */
                new_state->ammo_mult += extra_might;

                /* Hack -- Rangers love Bows, rogues love slings */
                if (((cp_ptr->flags & CF_EXTRA_SHOT) && (new_state->ammo_tval == TV_SHOT)) ||
                        ((cp_ptr->flags & CF_EXTRA_ARROW) && (new_state->ammo_tval == TV_ARROW)))
                {
                    if (p_ptr->lev >= LEV_EXTRA_COMBAT) new_state->num_fire++;
                }
            }

            /* Require at least one shot */
            if (new_state->num_fire < 1) new_state->num_fire = 1;
        }
    }

    /* Brigands get poison resistance */
    if ((p_ptr->lev >= LEV_RES_POIS) && (cp_ptr->flags & (CF_BRIGAND_COMBAT)))
    {
        new_state->resist_pois = TRUE;
    }

    /*** Analyze weapon ***/

    /* Examine the "current weapon" */
    o_ptr = &calc_inven[INVEN_WIELD];

    /* It is hard to hold a heavy weapon */
    if (hold < o_ptr->weight / 10)
    {
        /* Hard to wield a heavy weapon */
        new_state->to_h += 2 * (hold - o_ptr->weight / 10);
        new_state->known_to_h += 2 * (hold - o_ptr->weight / 10);

        /* Heavy weapon */
        new_state->heavy_wield = TRUE;
    }

    /* Normal weapons */
    if (o_ptr->k_idx && !new_state->heavy_wield)
    {
        /* Use the blows table */
        new_state->num_blow = calc_blows(o_ptr, new_state);

        /* Add in the "bonus blows" */
        new_state->num_blow += extra_blows;

        /* Boost digging skill by weapon weight */
        new_state->skills[SKILL_DIGGING] += (o_ptr->weight / 10);

        /*add extra attack for those who have the flag*/
        if ((p_ptr->lev >= LEV_EXTRA_COMBAT) && (cp_ptr->flags & CF_EXTRA_ATTACK) && o_ptr->is_weapon())
            new_state->num_blow += 1;
    }

    /* Priest weapon penalty for non-blessed edged weapons */
    if ((cp_ptr->flags & CF_BLESS_WEAPON) && (!new_state->bless_blade) &&
        ((o_ptr->tval == TV_SWORD) || (o_ptr->tval == TV_POLEARM)))
    {
        /* Reduce the real bonuses */
        new_state->to_h -= 2;
        new_state->to_d -= 2;

        /* Reduce the mental bonuses */
        new_state->known_to_h -= 2;
        new_state->known_to_d -= 2;

        /* Icky weapon */
        new_state->icky_wield = TRUE;
    }


    /*** Notice changes ***/

    /* Probably just examing an item */
    if (id_only) return;

    /* Analyze stats */
    for (i = 0; i < A_MAX; i++)
    {
        /* Notice changes */
        if (new_state->stat_loaded_max[i] != old_stat_loaded_max[i])
        {
            /* Redisplay the stats later */
            p_ptr->redraw |= (PR_SIDEBAR_PL);

        }

        /* Notice changes */
        if (new_state->stat_loaded_cur[i] != old_stat_loaded_cur[i])
        {
            /* Redisplay the stats later */
            p_ptr->redraw |= (PR_SIDEBAR_PL);

        }

        /* Notice changes */
        if (new_state->stat_index[i] != old_stat_index[i])
        {
            /* Change in CON affects Hitpoints */
            if (i == A_CON)
            {
                p_ptr->update |= (PU_HP);
            }

            /* Change in INT may affect Mana/Spells */
            else if (i == A_INT)
            {

                if ((cp_ptr->spell_book == TV_MAGIC_BOOK) ||
                    (cp_ptr->spell_book == TV_DRUID_BOOK))
                {
                    p_ptr->update |= (PU_MANA | PU_SPELLS);
                }
            }

            /* Change in WIS may affect Mana/Spells */
            else if (i == A_WIS)
            {
                if ((cp_ptr->spell_book == TV_PRAYER_BOOK) ||
                    (cp_ptr->spell_book == TV_DRUID_BOOK))
                {
                    p_ptr->update |= (PU_MANA | PU_SPELLS);
                }
            }
        }
    }

    /* Hack -- Telepathy Change */
    if (new_state->telepathy != old_telepathy)
    {
        /* Update monster visibility */
        p_ptr->update |= (PU_MONSTERS);
    }

    /* Hack -- See Invis Change */
    if (new_state->see_inv != old_see_inv)
    {
        /* Update monster visibility */
        p_ptr->update |= (PU_MONSTERS);
    }

    /* Redraw speed (if needed) */
    if (new_state->p_speed != old_speed)
    {
        /* Redraw speed */
        p_ptr->redraw |= (PR_SIDEBAR_PL | PR_STATUSBAR);
    }

    /* Redraw armor (if needed) */
    if ((new_state->known_ac != old_known_ac) || (new_state->known_to_a != old_known_to_a))
    {
        /* Redraw */
        p_ptr->redraw |= (PR_SIDEBAR_PL);

    }

    /* Hack -- handle "xtra" mode */
    if (character_xtra) return;

    /* Take note when "heavy bow" changes */
    if (old_heavy_shoot != new_state->heavy_shoot)
    {
        /* default: SV_SHORT_BOW or SV_LONG_BOW	*/
        QString launcher = "bow";

        int bow_slot = (birth_swap_weapons ? INVEN_MAIN_WEAPON : INVEN_BOW);

        /* Examine the "current bow" */
        object_kind *k_ptr = &k_info[calc_inven[bow_slot].k_idx];

        /* Make sure we are calling the launcher by the right name */
        if (k_ptr->sval == SV_SLING) launcher = "sling";
        else if ((k_ptr->sval == SV_LIGHT_XBOW) ||
                 (k_ptr->sval == SV_LIGHT_XBOW)) launcher = "crossbow";

        /* Message */
        if (new_state->heavy_shoot)
        {
            message(QString("You have trouble aiming such a heavy %1.") .arg(launcher));
        }
        else if (calc_inven[bow_slot].k_idx)
        {
            message(QString("You have no trouble aiming your %1.") .arg(launcher));
        }
        else
        {
            message(QString("You feel relieved to stop using your heavy %1.") .arg(launcher));
        }
    }

    /* Take note when "heavy weapon" changes */
    if (old_heavy_wield != new_state->heavy_wield)
    {
        QString o_name;
        /* Examine the "current weapon" */
        o_ptr = &calc_inven[INVEN_WIELD];
        o_name = object_desc(o_ptr, (ODESC_BASE));

        /* Message */
        if (new_state->heavy_wield)
        {
            message(QString("You have trouble wielding such a heavy %1.") .arg(o_name));
        }
        else if (calc_inven[INVEN_WIELD].k_idx)
        {
            message(QString("You have no trouble wielding your %1.") .arg(o_name));
        }
        else
        {
            message(QString("You feel relieved to stop wielding your heavy %1.") .arg(o_name));
        }
    }

    /* Take note when "illegal weapon" changes */
    if (old_icky_wield != new_state->icky_wield)
    {
        /* Message */
        if (new_state->icky_wield)
        {
            message(QString("You do not feel comfortable with your weapon."));
        }
        else if (calc_inven[INVEN_WIELD].k_idx)
        {
            message(QString("You feel comfortable with your weapon."));
        }
        else
        {
            message(QString("You feel more comfortable after removing your weapon."));
        }
    }

}


/*
 * Handle "p_ptr->notice"
 */
void notice_stuff(void)
{
    /* Notice stuff */
    if (!p_ptr->notice) return;

    /* Combine the pack */
    if (p_ptr->notice & (PN_COMBINE))
    {
        p_ptr->notice &= ~(PN_COMBINE);
        combine_pack();
        combine_quiver();
        calc_inven_cnt();
    }

    /* Reorder the pack */
    if (p_ptr->notice & (PN_REORDER))
    {
        p_ptr->notice &= ~(PN_REORDER);
        reorder_pack();
    }

    if(p_ptr->notice & PN_AUTOINSCRIBE)
    {
        p_ptr->notice &= ~(PN_AUTOINSCRIBE);
        autoinscribe_pack();
        autoinscribe_ground();
    }

    /* Sort the quiver */
    if (p_ptr->notice & PN_SORT_QUIVER)
    {
        p_ptr->notice &= ~(PN_SORT_QUIVER);
        sort_quiver(0);
        save_quiver_size();
    }

    /* Dump the monster messages */
    if (p_ptr->notice & PN_MON_MESSAGE)
    {
        p_ptr->notice &= ~(PN_MON_MESSAGE);

        /* Make sure this comes after all of the monster messages */
        flush_monster_messages();
    }

    /*
     * Let the player know how many quest monsters remain.
     * Make sure this comes after all of the monster messages
     */
    if (p_ptr->notice & PN_QUEST_REMAIN)
    {
        p_ptr->notice &= ~(PN_QUEST_REMAIN);

        quest_status_update();
    }

    /* Clear all flags */
    p_ptr->notice = 0;

}


/*
 * Handle "p_ptr->update"
 */
void update_stuff(void)
{
    /* Update stuff */
    if (!p_ptr->update) return;

    if (p_ptr->update & (PU_BONUS))
    {
        calc_bonuses(inventory, &p_ptr->state, FALSE);

        /*hack = always re-check stealth, torch & nativity*/
        p_ptr->update |= (PU_STEALTH | PU_TORCH);
    }

    if (p_ptr->update & (PU_TORCH))	calc_torch();

    if (p_ptr->update & (PU_STEALTH))	calc_stealth();
    if (p_ptr->update & (PU_HP))		calc_hitpoints();
    if (p_ptr->update & (PU_MANA))		calc_mana();
    if (p_ptr->update & (PU_SPELLS)) 	calc_spells();
    if (p_ptr->update & (PU_PLAYER_SCORE)) update_player_score();

    /* Character is not ready yet, no screen updates */
    if (!character_generated)
    {
        /* Clear the flags */
        p_ptr->update &= ~(PU_TORCH | PU_BONUS | PU_STEALTH | \
                            PU_HP | PU_MANA | PU_SPELLS);
        return;
    }

    if (p_ptr->update & (PU_FORGET_VIEW))	forget_view();
    if (p_ptr->update & (PU_UPDATE_VIEW))	update_view();
    if (p_ptr->update & (PU_DISTANCE))		update_monsters(TRUE);
    /* No need to do this twice */
    else if (p_ptr->update & (PU_MONSTERS))	update_monsters(FALSE);
    if (p_ptr->update & (PU_PANEL))
    {
        ui_player_moved();
    }
    if (p_ptr->update & (PU_FLOW_DOORS | PU_FLOW_NO_DOORS))	update_flows(TRUE);

    /* Clear all flags */
    p_ptr->update = 0;
}



/*
 * Handle "p_ptr->redraw"
 */
void redraw_stuff(void)
{
    /* Redraw stuff */
    if (!p_ptr->redraw) return;

    /* Character is not ready yet, no screen updates */
    if (!character_generated) return;

    if (p_ptr->redraw & (PR_MAP))
    {
        if (!p_ptr->is_resting()) ui_redraw_all();
    }

    if (p_ptr->redraw & (PR_WIN_EQUIPMENT))
    {
        p_ptr->redraw |= (PR_WIN_CHAR_BASIC | PR_WIN_CHAR_EQUIP_INFO);
        ui_update_char_equipment_window();
    }
    if (p_ptr->redraw & (PR_WIN_INVENTORY))
    {
        p_ptr->redraw |= (PR_WIN_CHAR_BASIC);
        ui_update_char_inventory_window();
    }

    if (p_ptr->redraw & (PR_SIDEBAR_PL))
    {
        if (!p_ptr->is_resting() && !p_ptr->is_running())
        {
            p_ptr->redraw |= (PR_WIN_CHAR_BASIC);
            ui_update_sidebar_player();
        }
    }

    if (p_ptr->redraw & (PR_SIDEBAR_MON))
    {
        if (!p_ptr->is_resting() && !p_ptr->is_running())
        {
            ui_update_sidebar_mon();
        }
    }

    if (p_ptr->redraw & (PR_STATUSBAR))
    {
        if (!p_ptr->is_running())
        {
            p_ptr->redraw |= (PR_WIN_CHAR_BASIC);
            ui_update_statusbar();
        }
    }

    if (p_ptr->redraw & (PR_TITLEBAR))
    {
        p_ptr->redraw |= (PR_WIN_CHAR_BASIC);
        ui_update_titlebar();
    }
    if (p_ptr->redraw & (PR_MESSAGES)) ui_update_messages();
    if (p_ptr->redraw & (PR_WIN_MONLIST)) ui_update_monlist();
    if (p_ptr->redraw & (PR_WIN_OBJLIST)) ui_update_objlist();
    if (p_ptr->redraw & (PR_WIN_MON_RECALL)) ui_update_mon_recall();
    if (p_ptr->redraw & (PR_WIN_OBJ_RECALL)) ui_update_obj_recall();
    if (p_ptr->redraw & (PR_WIN_FEAT_RECALL)) ui_update_feat_recall();
    if (p_ptr->redraw & (PR_WIN_MESSAGES)) ui_update_message_window();

    // This should come before PR_PLYR_SCORE and PR_TURNCOUNT
    if (p_ptr->redraw & (PR_WIN_CHAR_BASIC)) ui_update_char_basic_window();
    if (p_ptr->redraw & (PR_PLYR_SCORE)) ui_update_char_score();
    if (p_ptr->redraw & (PR_TURNCOUNT)) ui_update_char_turncount();

    if (p_ptr->redraw & (PR_WIN_CHAR_EQUIP_INFO)) ui_update_char_equip_info_window();

    if (p_ptr->redraw & (PR_DRAW))
    {
        if (!p_ptr->is_resting()) draw_coords();
    }
}


/*
 * Handle "p_ptr->update" and "p_ptr->redraw" and "p_ptr->window"
 */
void handle_stuff(void)
{

    /* Update stuff */
    update_stuff();

    /* Redraw stuff */
    redraw_stuff();
}

