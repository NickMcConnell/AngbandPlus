/*
 * File: attack.c
 * Purpose: Attacking (both throwing and melee) code
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *                    Jeff Greene, Diego Gonzalez
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include <src/npp.h>
#include "src/player_command.h"
#include "src/object_settings.h"

/*
 * Determine if the player "hits" a monster.
 *
 * Note -- Always miss 5%, always hit 5%, otherwise random.
 */
bool test_hit(int chance, int ac, int vis)
{
    int k;

    int numerator = (game_mode == GAME_NPPMORIA ? 4 : 3);

    /* Percentile dice */
    k = randint0(100);

    /* Hack -- Instant miss or hit */
    if (k < 10) return (k < 5);

    /* Penalize invisible targets */
    if (!vis) chance = chance / 2;

    /* Power competes against armor */
    if ((chance > 0) && (randint0(chance) >= (ac * numerator / 4))) return (TRUE);

    /* Assume miss */
    return (FALSE);
}

static void mod_dd_slays(u32b f1, u32b r3, u32b *r_l3, int *mult, bool seen)
{
    u16b i;
    int max_mult = 1;

    u16b counter = N_ELEMENTS(slays_info_nppangband);
    if (game_mode == GAME_NPPMORIA) counter = N_ELEMENTS(slays_info_nppmoria);

    /* Go through each slay/kill and get the best multiplier */
    for (i = 0; i < counter; i++)
    {
        const slays_structure *si;
        if (game_mode == GAME_NPPMORIA) si = &slays_info_nppmoria[i];
        else si = &slays_info_nppangband[i];

        /* See if any of the weapons's slays flag matches the monster race flags */
        if ((f1 & (si->slay_flag)) &&
            (r3 & (si->mon_flag)))
        {

            /* If the player can see the monster, mark the lore */
            if (seen)
            {
                *r_l3 |= (si->mon_flag);
            }

            /* Use the highest possible multiplier */
            if (max_mult < si->multiplier) max_mult = si->multiplier;
        }
    }

    if (max_mult > *mult) *mult = max_mult;
}

static int mod_dd_brands(u32b f1, u32b r3, u32b *r_l3, byte *divider, int *mult, bool seen, bool is_native, bool is_flying, u32b element)
{
    u16b i;
    int max_mult = 1;
    int terrain_flag = 0;

    u16b counter = N_ELEMENTS(brands_info_nppangband);
    if (game_mode == GAME_NPPMORIA) counter = N_ELEMENTS(brands_info_nppmoria);

    /* Use the hackish slays info to find succeptibilities in Moria */
    if (game_mode == GAME_NPPMORIA)
    {
        for (i = 0; i < counter; i++)
        {
            const slays_structure *si = &brands_info_nppmoria[i];

            /* See if any of the weapons's slays flag matches the monster race flags */
            if ((f1 & (si->slay_flag)) &&
                        (r3 & (si->mon_flag)))
            {

                /* If the player can see the monster, mark the lore */
                if (seen)
                {
                    *r_l3 |= (si->mon_flag);
                }

                /* Use the highest possible multiplier */
                if (max_mult < si->multiplier) max_mult = si->multiplier;
            }
        }
    }

    /* Go through each brand and look for a better multiplier, also factor in terrain */
    else for (i = 0; i < counter; i++)
    {
        const brands_structure *bi = &brands_info_nppangband[i];

        if (f1 & (bi->brand_flag))
        {
            /* If the monster is immune to the elemental brand, notice it */
            if (r3 & (bi->mon_flag))
            {
                if (seen)
                {
                    *r_l3 |= (bi->mon_flag);
                }
            }

            /* Possible increase in damage when standing in a terrain made of the element */
            else if (!is_native && !is_flying && (element & (bi->element)) && (bi->element))
            {
                /* First handle damage reductions due to terrain */
                if (bi->divisor > *divider)
                {
                    *divider = bi->divisor;
                    terrain_flag = -1;
                }
            }

            /* Otherwise, use the simple brand multplier */
            else
            {
                if (max_mult < bi->multiplier) max_mult = bi->multiplier;
            }
        }
    }

    if (max_mult > *mult) *mult = max_mult;

    return (terrain_flag);
}

static int mod_dd_succept(u32b f1, u32b r3, u32b *r_l3, bool seen)
{
    u16b i;
    int extra_dam = 0;

    /* Moria doesn't have succeptabilities */
    if (game_mode == GAME_NPPMORIA) return 0;

    /* Check for increased damage due to monster susceptibility */
    for (i = 0; i < N_ELEMENTS(mon_suscept); i++)
    {
        const mon_susceptibility_struct *ms = &mon_suscept[i];

        /* Does the weapon have this elemental brand? */
        if (f1 & (ms->brand_flag))
        {
            /* Does the monster take extra damage from this brand? */
            if (r3 & (ms->mon_flag))
            {
                extra_dam = 1;

                if (seen)
                {
                    *r_l3 |= (ms->mon_flag);
                }
            }
        }
    }

    return (extra_dam);
}

static void mod_dd_elem_brand(u32b r3, u32b *r_l3, int *mult, bool seen, bool is_weapon)
{
    if (!is_weapon) 					return;
    if (!p_ptr->timed[TMD_SLAY_ELEM]) 	return;

    /*First, Mark all resists in the lore if applicable*/
    if (r3 & (RF3_IM_ELEM))
    {
        if (seen)
        {
            u32b flags = r3;

            /*Just the elemental flags*/
            flags &= RF3_IM_ELEM;

            *r_l3 |= flags;
        }

        /*Now increase the damage, but only by two if they don't resist any of the elements.*/
        if ((r3 & (RF3_IM_ELEM)) != (RF3_IM_ELEM))
        {
            if (*mult < 2 ) *mult = 2;
        }
    }
}

/* Rogues are deadly with a sling and sling ammo. */
int rogue_shot(const object_type *o_ptr, int *plus, player_state shot_state)
{
    /* Must be a rogue using a sling */
    if (!(cp_ptr->flags & (CF_ROGUE_COMBAT))) return(0);
    if (shot_state.ammo_tval != TV_SHOT) return (0);
    if (o_ptr->tval != TV_SHOT) return(0);

    /* Big bonus for damage, when player is doing positive damage only */
    if (*plus >=0)
    {
        *plus += 1 + (p_ptr->lev / 5);
    }

    return (1);
}

/* Brigands can sometimes get extra multiplier with a sling. */
int brigand_shot(const object_type *o_ptr, u32b mon_race_flag_r1, bool asleep, player_state shot_state)
{
    int extra_dam = 0;

    /* Must be a brigand using a sling and sling ammo, and monster must not resist poison */
    if (!(cp_ptr->flags & (CF_BRIGAND_COMBAT))) return(0);
    if (shot_state.ammo_tval != TV_SHOT) return (0);
    if (o_ptr->tval != TV_SHOT) return(0);
    if (mon_race_flag_r1 & (RF3_IM_POIS)) return(0);

    /* Bigger chance to get a poisoned shot if the monster is sleeping */
    if (asleep)
    {
        /* Chance varies with player level */
        if (rand_int(100) < (50 + 25 * p_ptr->lev / 25))
        {
            extra_dam = 1;
        }
    }

    /* Monster is awake */
    else if (rand_int(100) < p_ptr->lev)
    {
        extra_dam = 1;
    }

    /* Print message if applicable */
    if (extra_dam) message(QString("Your killer arts allowed you to create a poisoned shot!"));

    return (extra_dam);
}

/*
 * Adjust the damage dice for a given object hitting a given monster.
 *
 * Note that "flasks of oil" do NOT do fire damage, although they
 * certainly could be made to do so.  XXX XXX
 *
 * Note that most brands and slays are x3, except Slay Animal (x2),
 * Slay Evil (x2), and Kill dragon (x5).
 */
static void dam_dice_aux(object_type *o_ptr, int *dd, const monster_type *m_ptr, bool is_weapon)
{
    byte extra_dam = 0;
    int mult = 1;
    byte divider = 1;

    int y = m_ptr->fy;
    int x = m_ptr->fx;

    int terrain_flag = 0;

    monster_race *r_ptr = &r_info[m_ptr->r_idx];
    monster_lore *l_ptr = &l_list[m_ptr->r_idx];

    /* Get the feature */
    u32b element = cave_ff3_match(y, x, TERRAIN_MASK);

    /* Find out if monster is native to terrain */
    bool is_native = is_monster_native(y, x, r_ptr);

    /* Find out if monster is flying over terrain */
    bool is_flying = (m_ptr->mflag & (MFLAG_FLYING)) != 0;

    /* Mod damage dice for slays */
    mod_dd_slays(o_ptr->obj_flags_1, r_ptr->flags3, &l_ptr->r_l_flags3, &mult, m_ptr->ml);

    /* Modify damage dice for branding */
    terrain_flag = mod_dd_brands(o_ptr->obj_flags_1, r_ptr->flags3, &l_ptr->r_l_flags3,
                                 &divider, &mult, m_ptr->ml, is_native, is_flying, element);

    extra_dam = mod_dd_succept(o_ptr->obj_flags_1, r_ptr->flags3, &l_ptr->r_l_flags3, m_ptr->ml);

    mod_dd_elem_brand(r_ptr->flags3, &l_ptr->r_l_flags3, &mult, m_ptr->ml, is_weapon);

    mult += extra_dam;

    /* Show a message if necessary */
    if ((terrain_flag != 0) && player_can_see_bold(y, x) && !p_ptr->timed[TMD_BLIND])
    {

        /* Get the feature's name */
        QString name = feature_desc(dungeon_info[y][x].feature_idx, FALSE, TRUE);

        /* Damage was increased */
        if (terrain_flag > 0)
        {
            message(QString("The %1 increased the damage of the weapon!") .arg(name));
        }
        /* Damage was decreased */
        else
        {
            message(QString("The %1 reduced the damage of the weapon!") .arg(name));
        }
    }

    /* Paranoia */
    if (mult < 1) mult = 1;

    /* Factor in the increased damage */
    *dd *= mult;

    /* Factor in reduced damage */
    *dd /= divider;

    /* Boundry Control */
    if (*dd < 1) *dd = 1;
}

int critical_shot_chance(object_type *o_ptr, player_state a_state, bool is_throw, bool id_only, u32b f3)
{
    int i = (is_throw ? p_ptr->state.skills[SKILL_TO_HIT_THROW] : p_ptr->state.skills[SKILL_TO_HIT_BOW]) * 2;

    /* Extract "shot" power */
    if (id_only)
    {
        i += o_ptr->weight + (a_state.known_to_h + (o_ptr->is_known() ? o_ptr->to_h : 0)) * 3;
    }

    else i += o_ptr->weight + (a_state.to_h + o_ptr->to_h) * 3;

    if (is_throw)
    {
        /* Rogues are especially good at throwing weapons */
        if ((cp_ptr->flags & (CF_ROGUE_COMBAT)) && (f3 & (TR3_THROWING)))
        {
            i += p_ptr->lev * 10;
        }
    }
    return (i);
}

/*
 * Critical hits (from objects thrown by player)
 * Factor in item weight, total plusses, and player level, bow skill.
 */
static int critical_shot_check(object_type *o_ptr, int *dd, int *plus, bool throwing, u32b f3)
{
    int i = critical_shot_chance(o_ptr, p_ptr->state, throwing, FALSE, f3);

    /* Critical hit */
    if (randint(CRIT_HIT_CHANCE) <= i)
    {
        int k;
        int crit_hit_bonus = 250 + (throwing ? p_ptr->state.skills[SKILL_TO_HIT_THROW] : p_ptr->state.skills[SKILL_TO_HIT_BOW]);
        crit_hit_bonus += (p_ptr->state.to_h + o_ptr->to_h) * 2;

        /* Rogues are especially good at throwing weapons */
        if ((throwing) && (cp_ptr->flags & (CF_ROGUE_COMBAT)) && (f3 & (TR3_THROWING)))
        {
            i += p_ptr->lev * 5;
        }
        k = o_ptr->weight + randint(crit_hit_bonus);

        if (k < 400)
        {
            color_message(QString("It was a good hit!"), TERM_YELLOW);
            *dd *= 2;
            *plus += 5;
        }
        else if (k < 1000)
        {
            color_message(QString("It was a great hit!"), TERM_ORANGE);
            *dd *= 2;
            *plus += 10;
        }
        else
        {
            color_message(QString("It was a superb hit!"), TERM_L_RED);
            *dd *= 3;
            *plus += 15;
        }

        return (2);
    }

    return (1);
}

/*
 * Determines the odds of an object breaking when thrown at a monster
 *
 * Note that artifacts never break, see the "drop_near()" function.
 */
static int breakage_chance(const object_type *o_ptr)
{
    /* Examine the item type */
    switch (o_ptr->tval)
    {
        /* Always break */
        case TV_FLASK:
        case TV_POTION:
        case TV_BOTTLE:
        case TV_FOOD:
        case TV_JUNK:
        {
            return (100);
        }

        /* Often break */
        case TV_LIGHT:
        case TV_SCROLL:
        case TV_PARCHMENT:
        case TV_SKELETON:
        {
            return (50);
        }

        /* Sometimes break */
        case TV_ARROW:
        {
            return (35);
        }

        /* Sometimes break */
        case TV_WAND:
        case TV_SHOT:
        case TV_BOLT:
        case TV_SPIKE:
        {
            return (25);
        }
    }

    /* Rarely break */
    return (10);
}

int critical_hit_chance(object_type *o_ptr, player_state a_state, bool id_only)
{
    int i = o_ptr->weight + (p_ptr->state.to_h + o_ptr->to_h) * 3;
    i += a_state.skills[SKILL_TO_HIT_MELEE] * 2;

    /* Re-do depending on the known variable */
    if (id_only)
    {
        i = o_ptr->weight + (p_ptr->state.known_to_h + (o_ptr->is_known() ? o_ptr->to_h : 0)) * 3;
        i += a_state.skills[SKILL_TO_HIT_MELEE] * 2;
    }

    return (i);
}

/*
 * Critical hits (by player)
 *
 * Factor in weapon weight, total plusses, player level.
 */
int critical_hit_check(object_type *o_ptr, int *dd, int *plus)
{
    int i = critical_hit_chance(o_ptr, p_ptr->state, FALSE);

    /* Chance */
    if (randint(CRIT_HIT_CHANCE) <= i)
    {
        int k;
        int crit_hit_bonus = 250 + p_ptr->state.skills[SKILL_TO_HIT_MELEE];
        crit_hit_bonus += (p_ptr->state.to_h + o_ptr->to_h);

        k = o_ptr->weight + randint(crit_hit_bonus);

        if (k < 400)
        {
            sound(MSG_HIT_GOOD);
            color_message(QString("It was a good hit!"), TERM_YELLOW);
            *dd *= 2;
            *plus += 5;
        }
        else if (k < 700)
        {
            sound(MSG_HIT_GREAT);
            color_message(QString("It was a great hit!"), TERM_ORANGE);
            *dd *= 2;
            *plus += 10;
        }
        else if (k < 900)
        {
            sound(MSG_HIT_SUPERB);
            color_message(QString("It was a superb hit!"), TERM_L_RED);
            *dd *= 3;
            *plus += 15;
        }
        else if (k < 1300)
        {
            sound(MSG_HIT_HI_GREAT);
            color_message(QString("It was a *GREAT* hit!"), TERM_RED);
            *dd *= 3;
            *plus += 20;
        }
        else
        {
            sound(MSG_HIT_HI_SUPERB);
            color_message(QString("It was a *SUPERB* hit!"), TERM_RED_LAVA);
            *dd *= 7;
            *dd /= 2;
            *plus += 25;
        }
    }

    else
    {
        sound(MSG_HIT);
        return (1);
    }

    /* Exceptional hit */
    return (2);
}

/*
 * Determine if a trap affects the player.
 * Always miss 5% of the time, Always hit 5% of the time.
 * Otherwise, match trap power against player armor.
 */
bool check_hit(int power)
{
    return test_hit(power, p_ptr->state.ac + p_ptr->state.to_a, TRUE);
}

/*
 * Attack the monster at the given location
 *
 * If no "weapon" is available, then "punch" the monster one time.
 * Note p_ptr->base_energy_use needs to be declared before this function is called,
 * so energy use can be reduced if the player doesn't use full energy.
 */
void py_attack(int y, int x)
{
    int num = 0, k, bonus, chance;
    int hits = 0;

    int sleeping_bonus = 0;

    monster_type *m_ptr;
    monster_race *r_ptr;
    monster_lore *l_ptr;

    object_type *o_ptr;

    QString m_name;

    bool fear = FALSE;

    bool do_quake = FALSE;

    bool was_asleep = FALSE;

    byte energy_use = BASE_ENERGY_MOVE;

    /* Get the monster */
    m_ptr = &mon_list[dungeon_info[y][x].monster_idx];
    r_ptr = &r_info[m_ptr->r_idx];
    l_ptr = &l_list[m_ptr->r_idx];

    /*record if monster was sleeping before waking*/
    if (m_ptr->m_timed[MON_TMD_SLEEP]) was_asleep = TRUE;

    /* Disturb the monster */
    wake_monster_attack(m_ptr, MON_TMD_FLG_NOMESSAGE);

    /*possibly update the monster health bar*/
    if (m_ptr->sidebar) p_ptr->redraw |= (PR_SIDEBAR_MON);

    /* Disturb the player */
    disturb(TRUE, TRUE);

    /* Extract monster name (or "it") */
    m_name = monster_desc(m_ptr, 0);

    /* Auto-Recall if possible and visible */
    if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

    /* Track a new monster */
    if (m_ptr->ml) health_track(dungeon_info[y][x].monster_idx);

    // Show red line
    if (m_ptr->ml) light_spot(y, x);

    /* Handle player fear */
    if (p_ptr->timed[TMD_AFRAID])
    {
        /* Message */
        message(QString("You are too afraid to attack %1!") .arg(m_name));
        /* Done */
        return;
    }

    /* Get the weapon */
    o_ptr = &inventory[INVEN_WIELD];

    /* Make sure we are using a weapon instead of a bow/shovel */
    if (birth_swap_weapons)
    {
        if (o_ptr->is_bow() || o_ptr->is_shovel())
        {
            /* Only check if the bow/shovel has not yet been confirmed */
            if (!(o_ptr->ident & (IDENT_CONFIRMED_USE)))
            {
                QString o_name = object_desc(o_ptr, (ODESC_BASE));

                if (!get_check(QString("Really attack with your %1? ") .arg(o_name)))
                {
                    return;
                }
                /* Mark it as OK to use in melee so we don't check every time*/
                else o_ptr->ident |= IDENT_CONFIRMED_USE;
            }
        }
    }

    /* Calculate the "attack quality" */
    bonus = p_ptr->state.to_h + o_ptr->to_h;

    /*
     * If the monster is sleeping and visible, it can be hit more easily.
     * Especially by Rogues
     */

    if ((was_asleep) && (m_ptr->ml))
    {
        sleeping_bonus =  5 + p_ptr->lev / 5;

        if (cp_ptr->flags & CF_ROGUE_COMBAT)
        {
            /*50 % increase*/
            sleeping_bonus *= 3;
            sleeping_bonus /= 2;
        }
    }
    chance = (p_ptr->state.skills[SKILL_TO_HIT_MELEE] + (bonus * BTH_PLUS_ADJ) + sleeping_bonus);

    /*Mark the monster as attacked*/
    m_ptr->mflag |= (MFLAG_HIT_BY_MELEE);

    // Stop returning after every message
    p_ptr->message_append_start();

    /* Attack once for each legal blow */
    while (num++ < p_ptr->state.num_blow)
    {
        int mon_ac = r_ptr->ac;

        /* Monster is different if desperate */
        if (m_ptr->mflag & (MFLAG_DESPERATE))
        {
            if (mon_ac < 10) mon_ac = 0;
            else mon_ac -= 10;
        }

        /*Adjust for player terrain*/
        chance = feat_adjust_combat_for_player(chance, FALSE);

        /*Adjust for monster terrain*/
        chance = feat_adjust_combat_for_monster(m_ptr, chance, TRUE);

        /* Some monsters are great at dodging  -EZ- */
        if ((r_ptr->flags2 & (RF2_EVASIVE)) && (!was_asleep) &&
            (!m_ptr->m_timed[MON_TMD_STUN]) && (!m_ptr->m_timed[MON_TMD_CONF]) &&
            (!m_ptr->m_timed[MON_TMD_FEAR]) && (one_in_(2)))
        {            
            color_message(QString("%1 evades your blow!") .arg(capitalize_first(m_name)), TERM_YELLOW);

            /* Learn that monster can dodge */
            l_ptr->r_l_flags2 |= (RF2_EVASIVE);

            continue;
        }

        /* Test for hit */
        else if (test_hit(chance, mon_ac, m_ptr->ml))
        {
            int dd, ds, tries, plus;

            if (was_asleep)
            {
                if (cp_ptr->flags & CF_ROGUE_COMBAT)
                {
                    color_message(QString("You ruthlessly sneak attack %1!") .arg(m_name), TERM_L_RED);
                }
                else
                {
                    color_message(QString("You sneak attack %1!") .arg(m_name), TERM_L_RED);
                }
            }

            else
            {
                /* Message */
                message(QString("You hit %1!") .arg(m_name));
            }

            /* If this was the first hit, make some noise */
            hits++;
            if (hits == 1) add_wakeup_chance += p_ptr->base_wakeup_chance;

            /* Nothing wielded */
            if (!o_ptr->k_idx)
            {
                dd = ds = 1;
                plus = 0;
            }

            dd = o_ptr->dd;
            ds = o_ptr->ds;
            plus = (p_ptr->state.to_d + o_ptr->to_d);

            /* Possibly increase the damage dice due to brands, slays, etc */
            dam_dice_aux(o_ptr, &dd, m_ptr, TRUE);

            tries = critical_hit_check(o_ptr, &dd, &plus);

            k = max_damroll(dd, ds, tries) + plus;

            /* Must have at least 1 hp damage */
            if (k < 1) k = 1;

            if (p_ptr->state.impact && (k > 50)) do_quake = TRUE;

            /* Complex message */
            if (cheat_know)
            {
                message(QString("You do %1 (out of %2) damage.") .arg(k) .arg(m_ptr->hp));
            }

            /* Damage, check for fear and death */
            if (mon_take_hit(dungeon_info[y][x].monster_idx, k, &fear, NULL, SOURCE_PLAYER, TRUE))
            {
                /*return energy from unused attacks*/
                if (num < p_ptr->state.num_blow)
                {
                    energy_use -= (((p_ptr->state.num_blow - (num)) * BASE_ENERGY_MOVE ) /
                            p_ptr->state.num_blow);
                }
                break;
            }

            /* Confusion attack */
            if (p_ptr->confusing)
            {
                /* Cancel glowing hands */
                p_ptr->confusing = FALSE;
                        /* Message */
                message(QString("Your hands stop glowing."));
                        /* Confuse the monster */
                mon_inc_timed(m_ptr->get_mon_idx(), MON_TMD_CONF,
                        (10 + rand_int(p_ptr->lev) / 10), (MON_TMD_FLG_NOTIFY));
            }
        }

        /* Player misses */
        else
        {
            /* Message */
            message(QString("You miss %1.") .arg(m_name));
        }
    }

    /* Mega-Hack -- apply earthquake brand */
    if (do_quake) earthquake(p_ptr->py, p_ptr->px, 10, TRUE);

    process_player_energy(energy_use);

    return;
}

/*
 * Fire an object from the pack or floor.
 *
 * You may only fire items that "match" your missile launcher.
 *
 * You must use slings + pebbles/shots, bows + arrows, xbows + bolts.
 *
 * See "calc_bonuses()" for more calculations and such.
 *
 * Note that "firing" a missile is MUCH better than "throwing" it.
 *
 * Note: "unseen" monsters are very hard to hit.
 *
 * Objects are more likely to break if they "attempt" to hit a monster.
 *
 * Ranger  s (with Bows) and Anyone (with "Extra Shots") get extra shots.
 *
 * The "extra shot" code works by decreasing the amount of energy
 * required to make each shot, spreading the shots out over time.
 *
 * Note that when firing missiles, the launcher multiplier is applied
 * after all the bonuses are added in, making multipliers very useful.
 *
 * Note that Bows of "Extra Might" get extra range and an extra bonus
 * for the damage multiplier.
 *
 * Note that Bows of "Extra Shots" give an extra shot.
 */
void command_fire(cmd_arg args)
{
    int dir, item;
    int i, j, y, x, ty, tx;
    int tmul, tdis;
    int bonus, chance;

    object_type *o_ptr;
    object_type *j_ptr;

    object_type *i_ptr;
    object_type object_type_body;

    bool hit_body = FALSE;

    QString o_name;

    int path_n;
    u16b path_g[PATH_SIZE];
    u16b path_gx[PATH_SIZE];

    /* Get the "bow" (if any) */
    j_ptr = &inventory[INVEN_BOW];

    /* Make sure we are using a weapon instead of a bow/shovel */
    if (birth_swap_weapons)
    {
        j_ptr = &inventory[INVEN_MAIN_WEAPON];

        if (!j_ptr->is_bow() && (j_ptr->tval))
        {
            message(QString("You must first wield a weapon you can fire with."));
            return;
        }
    }

    /* Require a usable launcher */
    if (!j_ptr->tval || !p_ptr->state.ammo_tval)
    {
        message(QString("You have nothing to fire with."));
        return;
    }

    /* Get item to fire and direction to fire in. */
    item = args.item;
    dir = args.direction;

    /* Check the item being fired is usable by the player. */
    if (!item_is_available(item, NULL, (USE_EQUIP | USE_INVEN | USE_FLOOR | USE_QUIVER | QUIVER_FIRST)))
    {
        message(QString("That item is not within your reach."));
        return;
    }

    /* Get the object for the ammo */
    o_ptr = object_from_item_idx(item);

    /* A cursed quiver disables the use of non-cursed ammo */
    if (IS_QUIVER_SLOT(item) && p_ptr->state.cursed_quiver && !o_ptr->is_cursed())
    {
        message(QString("Your quiver is cursed!"));
        return;
    }

    /* Check the ammo can be used with the launcher */
    if (o_ptr->tval != p_ptr->state.ammo_tval)
    {
        message(QString("That ammo cannot be fired by your current weapon."));
        return;
    }

    if (!get_item_allow(item, VERIFY_FIRE)) return;

    p_ptr->player_previous_command_update(CMD_FIRE, args);
    p_ptr->command_previous_args.k_idx = o_ptr->k_idx;

    /* Get local object */
    i_ptr = &object_type_body;

    /* Obtain a local object */
    i_ptr->object_copy(o_ptr);

    /* Single object, not marked */
    i_ptr->number = 1;
    i_ptr->obj_in_use = FALSE;

    if (IS_QUIVER_SLOT(item))
    {
        /*Mark it to go in the quiver */
        i_ptr->ident |= (IDENT_QUIVER);
    }

    /* Sound */
    sound(MSG_SHOOT);

    /* Describe the object */
    o_name = object_desc(i_ptr, (ODESC_FULL | ODESC_SINGULAR));

    /* Cursed ammunition can hurt the player sometimes */
    if (IS_QUIVER_SLOT(item) && i_ptr->is_cursed() && (rand_int(100) < 70))
    {
        /* Get amount of damage */
        int dam = damroll(i_ptr->dd, i_ptr->ds) + ABS(i_ptr->to_d) + ABS(j_ptr->to_d) + ABS(p_ptr->state.to_d);

        /* Message */
        message(QString("The %1 releases its curse on you!") .arg(o_name));

        /* Hurt the player */
        project_p(SOURCE_OTHER, p_ptr->py, p_ptr->px, dam, GF_NETHER, "firing a cursed projectile");
        process_player_energy(BASE_ENERGY_MOVE);
        return;
    }

    /* Reduce and describe inventory */
    if (item >= 0)
    {
        inven_item_increase(item, -1);
        inven_item_describe(item);
        inven_item_optimize(item);
    }

    /* Reduce and describe floor item */
    else
    {
        floor_item_increase(0 - item, -1);
        floor_item_optimize(0 - item);
    }

    /* Actually "fire" the object */
    bonus = (p_ptr->state.to_h + i_ptr->to_h + j_ptr->to_h);
    chance = (p_ptr->state.skills[SKILL_TO_HIT_BOW] + (bonus * BTH_PLUS_ADJ));

    /* Assume a base multiplier */
    tmul = p_ptr->state.ammo_mult;

    /* Base range XXX XXX */
    tdis = 10 + 5 * tmul;

    /* Start at the player */
    y = p_ptr->py;
    x = p_ptr->px;

    // First target closest, if there is anything there
    if (dir == DIR_CLOSEST)
    {
        if (target_set_closest(TARGET_KILL | TARGET_QUIET))
        {
            ty = p_ptr->target_row;
            tx = p_ptr->target_col;
        }
        else if (!get_aim_dir(&dir, FALSE)) return;
    }

    /* Check for "target request" */
    else if ((dir == DIR_TARGET) && target_okay())
    {
        tx = p_ptr->target_col;
        ty = p_ptr->target_row;
    }

    else
    {
        /* Predict the "target" location */
        ty = p_ptr->py + 99 * ddy[dir];
        tx = p_ptr->px + 99 * ddx[dir];
    }

    /* Calculate the path */
    path_n = project_path(path_g, path_gx, tdis, p_ptr->py, p_ptr->px, &ty, &tx, 0);

    /* Hack -- Handle stuff */
    handle_stuff();

    p_ptr->message_append_start();

    /* Project along the path */
    for (i = 0; i < path_n; ++i)
    {
        int ny = GRID_Y(path_g[i]);
        int nx = GRID_X(path_g[i]);

        /* Hack -- Stop before hitting walls */
        if (!cave_project_bold(ny, nx) && !cave_passable_bold(ny, nx)) break;

        /* Advance */
        x = nx;
        y = ny;

        // Monster
        if ((dungeon_info[y][x].monster_idx > 0) &&
                !(mon_list[dungeon_info[y][x].monster_idx].mflag & (MFLAG_HIDE)))
        {
            hit_body = true;
            break;
        }
     }

     // Visuals
     if (((y != p_ptr->py) || (x != p_ptr->px)) && player_can_see_bold(p_ptr->py, p_ptr->px))
     {
         u32b flg = 0;
         int end_square_y = p_ptr->py;
         int end_square_x = p_ptr->px;
         if (i_ptr->tval == TV_SHOT) flg |= PROJECT_SHOT;
         else flg |= PROJECT_AMMO;

         bool do_animate = FALSE;

         // Don't animate past the stopping point
         path_n = i;

         // Find the last square in the player's sight
         for (i = 0; i < path_n; ++i)
         {
             // First non-visible square
             if (!player_can_see_bold(GRID_Y(path_g[i]), GRID_X(path_g[i]))) break;

             do_animate = TRUE;
             end_square_y = GRID_Y(path_g[i]);
             end_square_x = GRID_X(path_g[i]);
         }

         if (do_animate) ui_animate_bolt(p_ptr->py, p_ptr->px, end_square_y, end_square_x, GF_ARROW, flg);
     }

     /* Handle monster */
     if (hit_body)
     {
        monster_type *m_ptr = &mon_list[dungeon_info[y][x].monster_idx];
        monster_race *r_ptr = &r_info[m_ptr->r_idx];
        monster_lore *l_ptr = &l_list[m_ptr->r_idx];

        int chance2;

        int visible = m_ptr->ml;

        /*Adjust for player terrain*/
        chance = feat_adjust_combat_for_player(chance, FALSE);

        /*Adjust for monster terrain*/
        chance = feat_adjust_combat_for_monster(m_ptr, chance, TRUE);

        chance2 = chance - distance(p_ptr->py, p_ptr->px, y, x);

        /* Note the collision */
        hit_body = TRUE;

        /* Some monsters are great at dodging  -EZ- */
        if ((r_ptr->flags2 & (RF2_EVASIVE)) && (!m_ptr->m_timed[MON_TMD_SLEEP]) &&
            (!m_ptr->m_timed[MON_TMD_STUN]) && (!m_ptr->m_timed[MON_TMD_CONF]) &&
            (!m_ptr->m_timed[MON_TMD_FEAR]) && (rand_int(5 + m_ptr->cdis) >= 3))
        {
            if (visible)
            {
                /* Get "the monster" or "it" */
                QString m_name = monster_desc(m_ptr, 0);

                message(QString("%1 dodges!") .arg(capitalize_first(m_name)));

                /* Learn that monster can dodge */
                l_ptr->r_l_flags2 |= (RF2_EVASIVE);
            }
        }

        /* Did we hit it (penalize distance travelled) */
        else if (test_hit(chance2, r_ptr->ac, m_ptr->ml))
        {
            bool fear = FALSE;
            int tdam, dd, ds, tries;
            int plus = i_ptr->to_d + j_ptr->to_d;

            /* Assume a default death */
            QString note_dies = " dies.";

            /*Mark the monster as attacked by the player*/
            m_ptr->mflag |= (MFLAG_HIT_BY_RANGED);

            if (monster_nonliving(r_ptr))
            {
                /* Special note at death */
                note_dies = " is destroyed.";
            }

            /* Make some noise */
            add_wakeup_chance += p_ptr->base_wakeup_chance / 2;

            /* Handle unseen monster */
            if (!visible)
            {
                /* Invisible monster */
                message(QString("The %1 finds a mark.") .arg(o_name));
            }

            /* Handle visible monster */
            else
            {

                /* Get "the monster" or "it" */
                QString m_name = monster_desc(m_ptr, 0);

                /* Message */
                message(QString("The %1 hits %2.") .arg(o_name) .arg(m_name));

                /* Hack -- Track this monster race */
                if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

                /* Hack -- Track this monster */
                if (m_ptr->ml) health_track(dungeon_info[y][x].monster_idx);
            }

            dd = i_ptr->dd * tmul;
            ds = i_ptr->ds;

            /* Apply special damage XXX XXX XXX */
            tries = critical_shot_check(i_ptr, &dd, &plus, FALSE, o_ptr->obj_flags_3);

            /* Possibly increase the damage dice due to brands, slays, etc */
            dam_dice_aux(i_ptr, &dd, m_ptr, FALSE);

            /* Check for extra damage with a sling for a rogue */
            tmul += rogue_shot(o_ptr, &plus, p_ptr->state);

            /* Check for extra damage from a brigand */
            tmul += brigand_shot(o_ptr, r_ptr->flags1, (m_ptr->m_timed[MON_TMD_SLEEP] ? TRUE: FALSE), p_ptr->state);

            /* Now factor the extra damage */
            plus *= tmul;

            /* Base damage from thrown object plus bonuses */
            tdam = max_damroll(dd, ds, tries) + plus;

            /* No negative damage */
            if (tdam < 0) tdam = 0;

            /* Complex message */
            if (cheat_know)
            {
                message(QString("You do %1d%2 + %3 damage.") .arg(dd) .arg(ds) .arg(plus));
                message(QString("You do %1 (out of %1) damage.") .arg(tdam) .arg(m_ptr->hp));
            }

            /* Hit the monster, check for death */
            if (mon_take_hit(dungeon_info[y][x].monster_idx, tdam, &fear, note_dies, SOURCE_PLAYER, TRUE))
            {
                /* Dead monster */
            }

            /* No death */
            else
            {
                /* Message */
                message_pain(dungeon_info[y][x].monster_idx, tdam);

            }
        }
    }

    /* Chance of breakage (during attacks) */
    j = (hit_body ? breakage_chance(i_ptr) : 0);

    /* Drop (or break) near that location */
    drop_near(i_ptr, j, y, x);

    process_player_energy(BASE_ENERGY_MOVE / p_ptr->state.num_fire);
}

void do_cmd_fire(void)
{
    if (!character_dungeon) return;

    object_type *j_ptr;
    int item;
    int dir;
    QString q = "Fire which item? ";
    QString s = "You have nothing to fire.";

    /* Get the "bow" (if any) */
    j_ptr = &inventory[INVEN_BOW];

    /* Make sure we are using a weapon instead of a bow/shovel */
    if (birth_swap_weapons)
    {
        j_ptr = &inventory[INVEN_MAIN_WEAPON];

        if (!j_ptr->is_bow() && (j_ptr->tval))
        {
            message(QString("You must first wield a weapon you can fire with."));
            return;
        }
    }

    /* Require a usable launcher */
    if (!j_ptr->tval || !p_ptr->state.ammo_tval)
    {
        message(QString("You have nothing to fire with."));
        return;
    }

    /* Require proper missile; prefer the quiver */
    item_tester_tval = p_ptr->state.ammo_tval;

    /* Get an item */
    if (!get_item(&item, q, s, (USE_INVEN | USE_QUIVER | QUIVER_FIRST | USE_FLOOR))) return;

    /* Get a direction (or cancel) */
    if (!get_aim_dir(&dir, FALSE)) return;

    cmd_arg args;
    args.item = item;
    args.direction = dir;

    command_fire(args);
}

//  A placeholder for cmd_fire_at nearest for the command_type structure
void command_fire_nearest(cmd_arg args)
{
    if (args.item < 0)
    {
        do_cmd_fire_at_nearest();
        return;
    }

    if (!target_set_closest(TARGET_KILL | TARGET_QUIET))
        return;
    command_fire(args);
}

void do_cmd_fire_at_nearest(void)
{
    object_type *j_ptr = &inventory[INVEN_BOW];

    /* the direction '5' means 'use the target' */
    int i, dir = DIR_CLOSEST, item = -1;

    /* Make sure we are using a weapon instead of a bow/shovel */
    if (birth_swap_weapons)
    {
        j_ptr = &inventory[INVEN_MAIN_WEAPON];

        if (!j_ptr->is_bow() && (j_ptr->tval))
        {
            message(QString("You must first wield a weapon you can fire with."));
            return;
        }
    }

    /* Require a usable launcher */
    if (!j_ptr->tval || !p_ptr->state.ammo_tval)
    {
        message(QString("You have nothing to fire with."));
        return;
    }

    /* Find first eligible ammo in the quiver */
    for (i=QUIVER_START; i < QUIVER_END; i++)
    {
        object_type *o_ptr = & inventory [i];

        if (!ammo_can_fire(o_ptr, i)) continue;

        item = i;
        break;
    }

    /* Next, try the backpack if necessary*/
    if (item < 0)
    {
        for (i = 0; i < INVEN_PACK; i++)
        {
            object_type *o_ptr = & inventory [i];

            if (!ammo_can_fire(o_ptr, i)) continue;

            item = i;
            break;
        }
    }

    /* Require usable ammo */
    if (item < 0)
    {
        message(QString("You have no ammunition to fire"));
        return;
    }

    /* Require foe */
    if (!target_set_closest(TARGET_KILL | TARGET_QUIET))
        return;

    /* Check for confusion */
    if (p_ptr->timed[TMD_CONFUSED])
    {
        message(QString("You are confused."));
        dir = ddd[randint0(8)];
    }

    cmd_arg args;
    args.item = item;
    args.direction = dir;

    command_fire(args);
}

/*
 * Flavor. Some objects cause random effects when thrown.
 * Returns TRUE if the object affected the dungeon in some way.
 */
static bool do_flavor_breakage(const object_type *o_ptr, int y, int x)
{
    /* Get the feature */
    feature_type *f_ptr = &f_info[dungeon_info[y][x].feature_idx];

    /* Analyze object */
    switch (o_ptr->tval)
    {
        /* Flasks of oil can create oil patches */
        case TV_FLASK:
        {
            /* Hack -- Check if oil exists */
            if (f_ptr->f_flags3 & (FF3_OIL)) break;

            /* Hack -- Graphics (don't hurt anything) */
            project(SOURCE_OTHER, 1, y, x, y, x, 0, GF_SMOKE, PROJECT_BOOM, 0, 0);

            /* Message. Flasks of oil are always broken now */
            message(QString("The flask is broken!"));

            /* Check if the grid can be transformed to oil */
            if (!_feat_ff3_match(f_ptr, TERRAIN_MASK) && cave_passable_bold(y, x) &&
                    !_feat_ff1_match(f_ptr, FF1_PERMANENT | FF1_STAIRS | FF1_DOOR) &&
                    (_feat_ff1_match(f_ptr, FF1_LOS | FF1_PROJECT) ==
                    feat_ff1_match(FEAT_FLOOR_OIL, FF1_LOS | FF1_PROJECT)) &&
                    one_in_(4))
            {
                /* Create oil */
                cave_set_feat(y, x, FEAT_FLOOR_OIL);

                /* Message */
                message(QString("There is oil on the floor."));
            }

            /* Success */
            return (TRUE);
        }
        /* Torches can ignite oil and forests */
        case TV_LIGHT:
        {
            /* Check if the lite is a torch and if the grid can be burned */
            if ((o_ptr->sval == SV_LIGHT_TORCH) && _feat_ff3_match(f_ptr, FF3_OIL | FF3_FOREST) &&
                _feat_ff2_match(f_ptr, FF2_HURT_FIRE))
            {
                /* Create fire */
                cave_alter_feat(y, x, FS_HURT_FIRE);

                /* Message */
                message(QString("The torch sets the dungeon on fire!"));

                /* Get the fire feature */
                f_ptr = &f_info[dungeon_info[y][x].feature_idx];

                /* Hurt objects sensitive to fire on the floor */
                if (_feat_ff3_match(f_ptr, FF3_FIRE) && (f_ptr->dam_non_native > 0))
                {
                    /* Hurt only items */
                    u32b flags = PROJECT_ITEM | PROJECT_HIDE;

                    /* Burn objects */
                    project(SOURCE_OTHER, 0, y, x, y, x, f_ptr->dam_non_native, GF_FIRE, flags, 0, 0);
                }

                /* Success */
                return (TRUE);
            }

            break;
        }
    }

    /* Failure */
    return (FALSE);
}

/*handle special effects of throwing certain potions*/
static bool thrown_potion_effects(object_type *o_ptr, bool *is_dead, bool *fear, int m_idx)
{
    monster_type *m_ptr = &mon_list[m_idx];
    monster_race *r_ptr = &r_info[m_ptr->r_idx];
    monster_lore *l_ptr = &l_list[m_ptr->r_idx];

    /*Assume it hurts everything*/
    u32b flag = (PROJECT_KILL | PROJECT_PLAY);

    int y = m_ptr->fy;
    int x = m_ptr->fx;

    bool ident = FALSE;

    bool do_stun = FALSE;
    bool un_confuse = FALSE;
    bool un_stun = FALSE;
    bool un_fear = FALSE;

    bool used_potion = TRUE;

    /* Hold the monster name */
    QString m_name;
    QString m_poss;

    /* Get the monster name*/
    m_name = monster_desc(m_ptr, 0);

    /* Get the monster possessive ("his"/"her"/"its") */
    m_poss = monster_desc(m_ptr, 0x22);

    /* Analyze the potion */
    switch (o_ptr->sval)
    {

        case SV_POTION_SLOWNESS:
        {
            /*slowness explosion at the site, radius 1*/
            ident = explosion(SOURCE_PLAYER, 1, y, x, damroll (2, p_ptr->lev), GF_OLD_SLOW, flag);
            break;
        }

        case SV_POTION_CONFUSION:
        {
            /*confusion explosion at the site, radius 1*/
            ident = explosion(SOURCE_PLAYER, 1, y, x, damroll (2, p_ptr->lev), GF_OLD_CONF, flag);
            break;
        }

        case SV_POTION_SLEEP:
        {
            /*sleep explosion at the site, radius 1*/
            ident = explosion(SOURCE_PLAYER, 1, y, x, damroll(3, p_ptr->lev), GF_OLD_SLEEP, flag);
            break;
        }

        case SV_POTION_LOSE_MEMORIES:
        {

            if (m_ptr->smart)
            {

                /*erase monster memory of player*/
                m_ptr->smart = 0L;

                if (m_ptr->ml)
                {
                    ident = TRUE;

                    /*monster forgets player history*/
                    message(QString("%1 forgets all %2 knows about you!") .arg(capitalize_first(m_name)) .arg(m_poss));
                }
            }
            /*monster forgets player history*/
            else used_potion = FALSE;

            break;
        }

        case SV_POTION_DRAIN_MANA:
        {
            if (m_ptr->mana)
            {
                if (m_ptr->ml)
                {
                    ident = TRUE;

                    /*monster forgets player history*/
                    message(QString("%1 loses some of %1 mana!") .arg(capitalize_first(m_name)) .arg(m_poss));
                }

                /*reduce mana by about 11%*/
                m_ptr->mana = m_ptr->mana * 9 / 10;

            }

            /*monster forgets player history*/
            else used_potion = FALSE;

            break;
        }

        case SV_POTION_RUINATION:
        {
            ident = TRUE;

            /*slight damage to monster*/
            mon_take_hit(dungeon_info[y][x].monster_idx, damroll(10, 10), fear, NULL, SOURCE_PLAYER, TRUE);

            break;
        }

        case SV_POTION_DETONATIONS:
        {

            ident = TRUE;

            /*slight damage to monster*/
            mon_take_hit(dungeon_info[y][x].monster_idx, damroll(25, 25), fear, NULL, SOURCE_PLAYER, TRUE);

            /*set the stun counter*/
            do_stun = TRUE;

            break;
        }

        case SV_POTION_DEATH:
        {
            /*drain life explosion at the site, radius 1*/
            ident = explosion(SOURCE_PLAYER, 1, y, x, damroll(30, 30), GF_LIFE_DRAIN, flag);

            break;
        }

        case SV_POTION_DETECT_INVIS:
        {
            if ((!m_ptr->ml)&& (r_ptr->flags2 & (RF2_INVISIBLE)))
            {
                /* Mark as visible */
                m_ptr->ml = TRUE;

                /*re-draw the spot*/
                light_spot(y, x);

                /* Update the monster name*/
                m_name = monster_desc(m_ptr, 0);

                /*monster appears for a turn*/
                message(QString("%1 appears for an instant!") .arg(capitalize_first(m_name)));

                /*update the lore*/
                l_ptr->r_l_flags2 |= (RF2_INVISIBLE);

                ident = TRUE;
            }

            /* Potion isn't identified */
            else used_potion = FALSE;

            break;
        }

        case SV_POTION_BOLDNESS:
        {

            un_fear = TRUE;

            break;
        }

        case SV_POTION_SPEED:
        {

            /*speed explosion at the site, radius 1*/
            ident = explosion(SOURCE_PLAYER, 1, y, x, 20 + rand_int(20), GF_OLD_SPEED, flag);

            break;
        }

        case SV_POTION_HEROISM:
        {
            /*healing explosion at the site, radius 1*/
            if (explosion(SOURCE_PLAYER, 1, y, x, 10, GF_OLD_HEAL, flag)) ident = TRUE;

            un_fear = TRUE;

            break;
        }

        case SV_POTION_BERSERK_STRENGTH:
        {
            /*healing explosion at the site, radius 1*/
            if (explosion(SOURCE_PLAYER, 1, y, x, 10, GF_OLD_HEAL, flag)) ident = TRUE;

            un_fear = TRUE;

            break;
        }

        case SV_POTION_CURE_LIGHT:
        {
            /*healing explosion at the site, radius 1*/
            if (explosion(SOURCE_PLAYER, 1, y, x, damroll(3, 8), GF_OLD_HEAL, flag)) ident = TRUE;

            break;
        }

        case SV_POTION_CURE_SERIOUS:
        {
            /*healing explosion at the site, radius 1*/
            if (explosion(SOURCE_PLAYER, 1, y, x, damroll(5, 10), GF_OLD_HEAL, flag)) ident = TRUE;

            un_confuse = TRUE;

            break;
        }

        case SV_POTION_CURE_CRITICAL:
        {
            /*healing explosion at the site, radius 1*/
            if (explosion(SOURCE_PLAYER, 1, y, x, damroll(8, 10), GF_OLD_HEAL, flag)) ident = TRUE;

            un_confuse = TRUE;
            un_stun = TRUE;

            break;
        }

        case SV_POTION_HEALING:
        {
            /*healing explosion at the site, radius 1*/
            if (explosion(SOURCE_PLAYER, 1, y, x, 325, GF_OLD_HEAL, flag)) ident = TRUE;

            un_confuse = TRUE;
            un_stun = TRUE;

            break;
        }

        case SV_POTION_STAR_HEALING:
        {
            /*healing explosion at the site, radius 1*/
            if (explosion(SOURCE_PLAYER, 1, y, x, 1500, GF_OLD_HEAL, flag)) ident = TRUE;

            un_confuse = TRUE;
            un_stun = TRUE;

            break;
        }

        case SV_POTION_LIFE:
        {
            /*only for the living*/
            if (monster_nonliving(r_ptr))
            {
                used_potion = FALSE;

                break;
            }

            /*healing explosion at the site, radius 1*/
            if (explosion(SOURCE_PLAYER, 1, y, x, 5000, GF_OLD_HEAL, flag)) ident = TRUE;

            un_confuse = TRUE;
            un_stun = TRUE;

            break;
        }

        case SV_POTION_RESTORE_MANA:
        {

            if (r_ptr->mana > m_ptr->mana)
            {
                if (m_ptr->ml)
                {
                    ident = TRUE;

                    /*monster forgets player history*/
                    message(QString("%1 gains back all %2 mana!") .arg(capitalize_first(m_name)) .arg(m_poss));
                }

                /*restore mana%*/
                m_ptr->mana = r_ptr->mana;
            }

            /*monster forgets player history*/
            else used_potion = FALSE;

            break;
        }

        /*potion just gets thrown as normal object*/
        default:
        {
            used_potion = FALSE;

            break;
        }
    }

    /*monster is now dead, skip messages below*/
    if (dungeon_info[y][x].monster_idx == 0)
    {
        do_stun = FALSE;
        un_confuse = FALSE;
        un_stun = FALSE;
        un_fear = FALSE;
        *is_dead = TRUE;
    }

    if (un_confuse)
    {
        mon_clear_timed(m_idx, MON_TMD_CONF, MON_TMD_FLG_NOTIFY);

        /* Dump a message */
        if (m_ptr->ml) ident = TRUE;
    }

    if (un_stun)
    {
        mon_clear_timed(m_idx, MON_TMD_STUN, MON_TMD_FLG_NOTIFY);

        /* Dump a message */
        if (m_ptr->ml) ident = TRUE;
    }

    if (un_fear)
    {
        mon_clear_timed(m_idx, MON_TMD_FEAR, MON_TMD_FLG_NOTIFY);

        /* Dump a message */
        if (m_ptr->ml) ident = TRUE;
    }

    /* Sound and Impact breathers never stun */
    if (do_stun)
    {
        mon_inc_timed(m_idx, MON_TMD_STUN, 15, MON_TMD_FLG_NOTIFY);

        ident = TRUE;
    }

    /*inform them of the potion, mark it as known*/
    if ((ident) && (!(k_info[o_ptr->k_idx].aware)))
    {
        QString o_name;

        /* Identify it */
        o_ptr->mark_known(TRUE);

        /* Description */
        o_name = object_desc(o_ptr, (ODESC_FULL | ODESC_SINGULAR));

        /* Describe the potion */
        message(QString("You threw %1.") .arg(o_name));

        /* Combine / Reorder the pack (later) */
        p_ptr->notice |= (PN_COMBINE | PN_REORDER);

        /* Window stuff */
        p_ptr->redraw |= (PR_WIN_INVENTORY | PR_WIN_EQUIPMENT);
    }

    /* Redraw if necessary*/
    if (used_potion)
    {
        if (m_ptr->sidebar) p_ptr->redraw |= (PR_SIDEBAR_MON);
    }

    /* Handle stuff */
    handle_stuff();

    return (used_potion);
}

int weapon_throw_adjust(object_type *o_ptr, u32b f3, int *plus, bool id_only)
{
    int mult = 1;

    /* Not a throwing weapon */
    if (!(f3 & (TR3_THROWING))) return (mult);

    /* Double the damage for throwing weapons */
    mult *= 2;

    if ((!id_only) || o_ptr->is_known())
    {
        /* Perfectly balanced weapons do even more damage. */
        if (o_ptr->ident & IDENT_PERFECT_BALANCE) mult *= 2;
    }

    /* Rogues are especially good at thrown weapons */
    if (cp_ptr->flags & CF_ROGUE_COMBAT)
    {
        mult *= 2;

        /* Big bonus for damage, when player is doing positive damage only */
        if (*plus >=0)
        {
            *plus += 1 + (p_ptr->lev / 5) * mult;
        }
    }

    return (mult);
}

/*
 * Throw an object from the pack or floor.
 *
 * Note: "unseen" monsters are very hard to hit.
 *
 * Should throwing a weapon do full damage?  Should it allow the magic
 * to hit bonus of the weapon to have an effect?  Should it ever cause
 * the item to be destroyed?  Should it do any damage at all?
 */
void command_throw(cmd_arg args)
{
    int dir, item;
    int i, j, y, x, ty, tx;
    int chance, tdis;
    int mul, divider;

    object_type *o_ptr;

    object_type *i_ptr;
    object_type object_type_body;

    bool hit_body = FALSE;

    int path_n;
    u16b path_g[PATH_SIZE];
    u16b path_gx[PATH_SIZE];

    QString o_name;

    /* Get item to throw and direction in which to throw it. */
    item = args.item;
    dir = args.direction;

    /* Make sure the player isn't throwing wielded items */
    if (item >= INVEN_WIELD && item < QUIVER_START)
    {
        message(QString("You have cannot throw wielded items."));
        return;
    }

    /* Get the object */
    o_ptr = object_from_item_idx(item);

    /* A cursed quiver disables the use of non-cursed ammo */
    if (IS_QUIVER_SLOT(item) && p_ptr->state.cursed_quiver && !o_ptr->is_cursed())
    {
        message(QString("Your quiver is cursed!"));
        return;
    }

    if (item_is_available(item, NULL, USE_EQUIP))
    {
        message(QString("You have cannot throw wielded items."));
        return;
    }

    /* Check the item being thrown is usable by the player. */
    if (!item_is_available(item, NULL, (USE_INVEN | USE_FLOOR | USE_QUIVER)))
    {
        message(QString("That item is not within your reach."));
        return;
    }

    if (!get_item_allow(item, VERIFY_THROW)) return;

    // Check for direction if necessary
    if (dir == DIR_UNKNOWN)
    {
        if (!get_aim_dir(&dir, FALSE)) return;
    }

    p_ptr->message_append_start();

    /* Get local object */
    i_ptr = &object_type_body;

    /* Obtain a local object */
    i_ptr->object_copy(o_ptr);

    /* Distribute the charges of rods/wands between the stacks */
    distribute_charges(o_ptr, i_ptr, 1);

    /* Single object */
    i_ptr->number = 1;
    i_ptr->obj_in_use = FALSE;

    p_ptr->player_previous_command_update(CMD_THROW, args);
    p_ptr->command_previous_args.k_idx = o_ptr->k_idx;

    if (IS_QUIVER_SLOT(item))
    {
        /*Mark it to go in the quiver */
        i_ptr->ident |= (IDENT_QUIVER);
    }

    /* Description */
    o_name = object_desc(i_ptr, ODESC_FULL);

    /* Cursed throwing weapons can hurt the player sometimes */
    if (IS_QUIVER_SLOT(item) && i_ptr->is_cursed() && (rand_int(100) < 70))
    {
        /* Get amount of damage */
        int dam = damroll(i_ptr->dd, i_ptr->ds) + ABS(i_ptr->to_d);

        /* Message */
        message(QString("The %1 releases its curse on you!") .arg(o_name));

        /* Hurt the player */
        project_p(SOURCE_OTHER, p_ptr->py, p_ptr->px, dam, GF_NETHER, "throwing a cursed weapon");
        process_player_energy(BASE_ENERGY_MOVE);
        return;
    }    

    /* Reduce and describe inventory */
    if (item >= 0)
    {
        inven_item_increase(item, -1);
        inven_item_describe(item);
        inven_item_optimize(item);
    }

    /* Reduce and describe floor item */
    else
    {
        floor_item_increase(0 - item, -1);
        floor_item_optimize(0 - item);
    }

    /* Extract a "distance multiplier" */
    mul = 10;

    /* Enforce a minimum "weight" of one pound */
    divider = ((i_ptr->weight > 10) ? i_ptr->weight : 10);

    /* Hack -- Distance -- Reward strength, penalize weight */
    tdis = (adj_str_blow[p_ptr->state.stat_index[A_STR]] + 20) * mul / divider;

    /* Max distance of 10 */
    if (tdis > 10) tdis = 10;

    /* Chance of hitting */
    if (o_ptr->obj_flags_3 & (TR3_THROWING))
    {
        chance = p_ptr->state.skills[SKILL_TO_HIT_THROW] + BTH_PLUS_ADJ * (p_ptr->state.to_h + i_ptr->to_h);
    }
    else
    {
        chance = (3 * p_ptr->state.skills[SKILL_TO_HIT_THROW] / 2) + (BTH_PLUS_ADJ * i_ptr->to_h);
    }

    /* Start at the player */
    y = p_ptr->py;
    x = p_ptr->px;

    // First target closest, if there is anything there
    if (dir == DIR_CLOSEST)
    {
        if (target_set_closest(TARGET_KILL | TARGET_QUIET))
        {
            ty = p_ptr->target_row;
            tx = p_ptr->target_col;
        }
        else if (!get_aim_dir(&dir, FALSE)) return;
    }

    /* Check for "target request" */
    else if ((dir == DIR_TARGET) && target_okay())
    {
        tx = p_ptr->target_col;
        ty = p_ptr->target_row;
    }

    else
    {
        /* Predict the "target" location */
        ty = p_ptr->py + 99 * ddy[dir];
        tx = p_ptr->px + 99 * ddx[dir];
    }

    /* Calculate the path */
    path_n = project_path(path_g, path_gx, tdis, p_ptr->py, p_ptr->px, &ty, &tx, 0);

    /* Hack -- Handle stuff */
    handle_stuff();

    /* Project along the path */
    for (i = 0; i < path_n; ++i)
    {
        int ny = GRID_Y(path_g[i]);
        int nx = GRID_X(path_g[i]);

        /* Hack -- Stop before hitting walls */
        if (!cave_project_bold(ny, nx) && !cave_passable_bold(ny, nx)) break;

        /* Advance */
        x = nx;
        y = ny;

        /* Handle monster */
        if ((dungeon_info[y][x].monster_idx > 0) && !(mon_list[dungeon_info[y][x].monster_idx].mflag & (MFLAG_HIDE))) {
            hit_body = true;
            break;
        }
    }

    // Visuals
    if ((y != p_ptr->py || x != p_ptr->px) && player_can_see_bold(y, x))
    {
        int end_square_y = p_ptr->py;
        int end_square_x = p_ptr->px;

        bool do_animate = FALSE;

        // Don't animate past the stopping point
        path_n = i;

        // Find the last square in the player's sight
        for (i = 0; i < path_n; ++i)
        {
            // First non-visible square
            if (!player_can_see_bold(GRID_Y(path_g[i]), GRID_X(path_g[i]))) break;

            do_animate = TRUE;
            end_square_y = GRID_Y(path_g[i]);
            end_square_x = GRID_X(path_g[i]);
        }

        if (do_animate) ui_animate_throw(p_ptr->py, p_ptr->px, end_square_y, end_square_x, i_ptr);
    }

    if (hit_body)
    {
        monster_type *m_ptr = &mon_list[dungeon_info[y][x].monster_idx];
        monster_race *r_ptr = &r_info[m_ptr->r_idx];
        monster_lore *l_ptr = &l_list[m_ptr->r_idx];

        int chance2;

        int visible = m_ptr->ml;

        int sleeping_bonus = 0;

        bool potion_effect = FALSE;
        int pdam = 0;
        bool is_dead = FALSE;

        /*Adjust for player terrain*/
        chance = feat_adjust_combat_for_player(chance, FALSE);

        /*Adjust for monster terrain*/
        chance = feat_adjust_combat_for_monster(m_ptr, chance, TRUE);

        chance2 = chance - distance(p_ptr->py, p_ptr->px, y, x);

        /* Note the collision */
        hit_body = TRUE;

        /* Rogues Get extra to-hit from throwing weapons*/

        if ((cp_ptr->flags & CF_ROGUE_COMBAT)
            && (m_ptr->ml) && (o_ptr->obj_flags_3 & (TR3_THROWING)))
        {
            sleeping_bonus = 30 + p_ptr->lev / 2;
        }

        /* Some monsters are great at dodging  -EZ- */
        if ((r_ptr->flags2 & (RF2_EVASIVE)) && (!m_ptr->m_timed[MON_TMD_SLEEP]) &&
                (!m_ptr->m_timed[MON_TMD_STUN]) && (!m_ptr->m_timed[MON_TMD_CONF]) &&
                (!m_ptr->m_timed[MON_TMD_FEAR]) && (rand_int(5 + m_ptr->cdis) >= 3))
        {
            if (visible)
            {

                /* Get "the monster" or "it" */
                QString m_name = monster_desc(m_ptr, 0);

                message(QString("%1 dodges!") .arg(capitalize_first(m_name)));

                /* Learn that monster can dodge */
                l_ptr->r_l_flags2 |= (RF2_EVASIVE);
            }
        }

        /* Did we hit it (penalize range) */
        else if (test_hit((chance2 + sleeping_bonus), r_ptr->ac, m_ptr->ml))
        {
            bool fear = FALSE;
            int tdam, dd, ds, tries;
            int plus = i_ptr->to_d + p_ptr->state.to_d;

            /* Assume a default death */
            QString note_dies = " dies.";

            /*Mark the monster as attacked by the player*/
            m_ptr->mflag |= (MFLAG_HIT_BY_RANGED);

            /* Some monsters get "destroyed" */
            if (monster_nonliving(r_ptr))
            {
                /* Special note at death */
                note_dies = " is destroyed.";
            }

            /* Make some noise */
            add_wakeup_chance += p_ptr->base_wakeup_chance / 2;

            /* Handle unseen monster */
            if (!visible)
            {
                /* Invisible monster */
                message(QString("The %1 finds a mark.") .arg(o_name));
            }

            /* Handle visible monster */
            else
            {

                /* Get "the monster" or "it" */
                QString m_name = monster_desc(m_ptr, 0);

                if (o_ptr->obj_flags_3 & (TR3_THROWING))
                {
                    /* Message */
                    message(QString("The %1 hits %2 with great accuracy!.") .arg(o_name) .arg(m_name));
                }
                else
                {
                /* Message */
                    message(QString("The %1 hits %2.") .arg(o_name) .arg(m_name));
                }
                /* Hack -- Track this monster race */
                if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

                /* Hack -- Track this monster */
                if (m_ptr->ml) health_track(dungeon_info[y][x].monster_idx);

            }

            dd = i_ptr->dd;
            ds = i_ptr->ds;

            /*special effects sometimes reveal the kind of potion*/
            if (i_ptr->tval == TV_POTION)
            {
                /*record monster hit points*/
                pdam = m_ptr->hp;

                /*returns true if the damage has already been handled*/
                potion_effect = (thrown_potion_effects(i_ptr, &is_dead, &fear, dungeon_info[y][x].monster_idx));

                /*check the change in monster hp*/
                pdam -= m_ptr->hp;

                /*monster could have been healed*/
                if (pdam < 0) pdam = 0;
            }

            /* Apply special damage XXX XXX XXX */
            if (!potion_effect) dam_dice_aux(i_ptr, &dd, m_ptr, FALSE);

            /* Object is a throwing weapon. */
            weapon_throw_adjust(o_ptr, o_ptr->obj_flags_3, &plus, FALSE);

            /* Critical hits may add damage dice. */
            tries = critical_shot_check(i_ptr, &dd, &plus, TRUE, o_ptr->obj_flags_3);

            /* Base damage from thrown object plus bonuses */
            tdam = max_damroll(dd, ds, tries) + plus;

            /* No negative damage */
            if (tdam < 0) tdam = 0;

            /* Complex message */
            if (cheat_know)
            {
                message(QString("You do %1d%2 + %3 damage.") .arg(dd)  .arg(ds) .arg(plus));
                message(QString("You do %1 (out of %2) damage.")
                           .arg(potion_effect ? pdam : tdam) .arg(m_ptr->hp));
            }

            /* Hit the monster, unless a potion effect has already been done */
            if (!potion_effect)
            {
                 is_dead = (mon_take_hit(dungeon_info[y][x].monster_idx, tdam, &fear, note_dies, SOURCE_PLAYER, TRUE));
            }

            /* Still alive */
            if (!is_dead)
            {
                /* Message if applicable*/
                if ((!potion_effect) || (pdam > 0))
                    message_pain(dungeon_info[y][x].monster_idx,  (pdam ? pdam : tdam));
            }
        }
    }

    /* Check for special breakage */
    if (!hit_body && do_flavor_breakage(i_ptr, y, x)) return;

    /* Chance of breakage (during attacks) - potions always break*/
    if (i_ptr->ident & IDENT_PERFECT_BALANCE) j = 0;
    else j = (hit_body ? breakage_chance(i_ptr) : 0);

    /*hack - throwing weapons have a lesser chance*/
    if (o_ptr->obj_flags_3 & (TR3_THROWING)) j /= 2;

    /* Drop (or break) near that location */
    drop_near(i_ptr, j, y, x);

    process_player_energy(BASE_ENERGY_MOVE);
}

void do_cmd_throw(void)
{
    if (!character_dungeon) return;

    int item, dir;
    QString q, s;

    /* Get an item */
    q = "Throw which item? ";
    s = "You have nothing to throw.";
    if (!get_item(&item, q, s, (USE_INVEN | USE_QUIVER | USE_FLOOR))) return;

    if (item >= INVEN_WIELD && item < QUIVER_START)
    {
        message(QString("You have cannot throw wielded items."));
        return;
    }

    /* Get a direction (or cancel) */
    if (!get_aim_dir(&dir, FALSE)) return;

    cmd_arg args;
    args.direction = dir;
    args.item = item;

    command_throw(args);
}


