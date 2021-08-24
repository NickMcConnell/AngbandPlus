/* File: cmd1.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies. Other copyrights may also apply.
 */

/* Purpose: Movement commands (part 1) */

#include "angband.h"
#include "equip.h"
#include <assert.h>

void death_scythe_miss(object_type *o_ptr, int hand, int skill)
{
    int k;
    slay_t crit;
    int dd = o_ptr->dd;
    int ds = o_ptr->ds;
    int to_h = 0;
    int to_d = 0;

    /* Sound */
    sound(SOUND_HIT);

    /* Message */
    if (hand == HAND_NONE) /* this is a thrown  weapon */
        cmsg_print(TERM_VIOLET, "Your scythe viciously slashes you!");
    else
    {
        cmsg_print(TERM_VIOLET, "Your scythe returns to you!");
        dd += p_ptr->attack_info[hand].to_dd;
        ds += p_ptr->attack_info[hand].to_ds;
        to_h += p_ptr->attack_info[hand].to_h;
        to_d += p_ptr->attack_info[hand].to_d;
    }

    k = damroll(dd, ds);
    {
        int mult;
        switch (p_ptr->mimic_form)
        {
        case MIMIC_NONE:
            switch (p_ptr->prace)
            {
                case RACE_YEEK:
                case RACE_KLACKON:
                case RACE_HUMAN:
                case RACE_AMBERITE:
                case RACE_DUNADAN:
                case RACE_BARBARIAN:
                case RACE_BEASTMAN:
                case RACE_DEMIGOD:
                    mult = 25;break;
                case RACE_SNOTLING:
                case RACE_HALF_TROLL:
                case RACE_HALF_OGRE:
                case RACE_HALF_GIANT:
                case RACE_HALF_TITAN:
                case RACE_CYCLOPS:
                case RACE_IMP:
                case RACE_SKELETON:
                case RACE_ZOMBIE:
                case RACE_VAMPIRE:
                case RACE_MON_VAMPIRE:
                case RACE_SPECTRE:
                case RACE_BALROG:
                case RACE_DRACONIAN:
                case RACE_MON_LICH:
                case RACE_MON_DRAGON:
                    mult = 30;break;
                default:
                    mult = 10;break;
            }
            break;
        case MIMIC_DEMON:
        case MIMIC_DEMON_LORD:
        case MIMIC_VAMPIRE:
            mult = 30;break;
        default:
            mult = 10;break;
        }

        if (p_ptr->align < 0 && mult < 20)
            mult = 20;
        if (!res_save_default(RES_ACID) && mult < 25)
            mult = 25;
        if (!res_save_default(RES_ELEC) && mult < 25)
            mult = 25;
        if (!res_save_default(RES_FIRE) && mult < 25)
            mult = 25;
        if (!res_save_default(RES_COLD) && mult < 25)
            mult = 25;
        if (!res_save_default(RES_POIS) && mult < 25)
            mult = 25;

        if (obj_has_flag(o_ptr, OF_BRAND_MANA) && (p_ptr->csp > (p_ptr->msp / 30)))
        {
            p_ptr->csp -= (1+(p_ptr->msp / 30));
            p_ptr->redraw |= (PR_MANA);
            mult = mult * 3 / 2 + 15;
        }

        k *= mult;
        k /= 10;
    }

    crit = crit_aux(CRIT_FREQ_ROLL, o_ptr->weight, skill);
    if (crit.id)
    {
        k = k * crit.mul/100 + crit.add;
        if (crit.msg) msg_print(crit.msg);
    }

    if (one_in_(6))
    {
        int mult = 2;
        msg_format("Your weapon cuts deep into yourself!");
        /* Try to increase the damage */
        while (one_in_(4))
        {
            mult++;
        }

        k *= mult;
    }
    k += to_d + o_ptr->to_d;

    if (k < 0) k = 0;

    take_hit(DAMAGE_FORCE, k, "Death scythe");
}

/*
 * Determine if the player "hits" a monster (normal combat).
 * Note -- Always miss 5%, always hit 5%, otherwise random.
 */
bool test_hit_fire(int chance, int ac, int vis)
{
    int k;

    /* Never hit */
    if (chance <= 0) return (FALSE);

    /* Invisible monsters are harder to hit */
    if (!vis) chance = (chance + 1) / 2;

    /* Percentile dice */
    k = randint0(100);

    /* Hack -- Instant miss or hit */
    if (k < 10) return (k < 5);

    /* Power competes against armor */
    if (randint0(chance) < (ac * 3 / 4)) return (FALSE);

    /* Assume hit */
    return (TRUE);
}



/*
 * Determine if the player "hits" a monster (normal combat).
 *
 * Note -- Always miss 5%, always hit 5%, otherwise random.
 */
bool test_hit_norm(int chance, int ac, int vis)
{
    int k;

    /* Wimpy attack never hits */
    if (chance <= 0) return (FALSE);

    /* Penalize invisible targets */
    if (!vis) chance = (chance + 1) / 2;

    /* Percentile dice */
    k = randint0(100);

    /* Hack -- Instant miss or hit */
    if (k < 10) return (k < 5);

    /* Power must defeat armor */
    if (randint0(chance) < (ac * 3 / 4)) return (FALSE);

    /* Assume hit */
    return (TRUE);
}



/*
 * Critical hits (from bows/crossbows/slings)
 * Factor in item weight, total plusses, and player level.
 */
critical_t critical_shot(int weight, int plus)
{
    critical_t result = {0};
    int i, k;

    /* Extract "shot" power */
    i = (p_ptr->shooter_info.to_h + plus) * 3 + p_ptr->skills.thb * 2;

    /* Snipers and Crossbowmasters get more crits */
    if (p_ptr->concent) i += i * p_ptr->concent / 10;
    if (p_ptr->pclass == CLASS_SNIPER && p_ptr->shooter_info.tval_ammo == TV_BOLT) i = i * 3 / 2;
    if (weaponmaster_get_toggle() == TOGGLE_CAREFUL_AIM)
        i *= 3;
    if (p_ptr->pclass == CLASS_ARCHER) i += i * p_ptr->lev / 100;

    /* Critical hit */
    if (randint1(5000) <= i)
    {
        k = weight * randint1(500);
        result.mul = 150 + k * 200 / 2000;

        if (result.mul < 200)
            result.desc = "It was a <color:y>decent</color> shot!";
        else if (result.mul < 240)
            result.desc = "It was a <color:R>good</color> shot!";
        else if (result.mul < 270)
            result.desc = "It was a <color:r>great</color> shot!";
        else if (result.mul < 300)
            result.desc = "It was a <color:v>superb</color> shot!";
        else
            result.desc = "It was a <color:v>*GREAT*</color> shot!";
    }

    return result;
}

/*
 * Search for hidden things
 */
static void _search_grid(point_t pos, dun_grid_ptr grid)
{
    obj_ptr obj;

    int chance = p_ptr->skills.srh;
    if (plr_tim_find(T_BLIND) || no_lite()) chance = chance / 10;
    if (plr_tim_find(T_CONFUSED) || plr_tim_find(T_HALLUCINATE)) chance = chance / 10;
    if (randint1(100) > chance) return;

    /* Invisible trap */
    if (grid->mimic && is_trap(grid->feat))
    {
        disclose_grid(pos.y, pos.x);
        msg_print("You have found a trap.");
        disturb(0, 0);
    }

    /* Secret door */
    if (is_hidden_door(grid))
    {
        msg_print("You have found a secret door.");
        disclose_grid(pos.y, pos.x);
        disturb(0, 0);
    }

    /* Trapped Chests */
    for (obj = obj_at(pos); obj; obj = obj->next)
    {
        if (obj->tval != TV_CHEST) continue;
        if (obj->pval < 0 || !chest_traps[obj->pval]) continue; /* not trapped */
        if (!obj_is_known(obj))
        {
            msg_print("You have discovered a trap on the chest!");
            obj_identify(obj);
            disturb(0, 0);
        }
    }
}
void search(void)
{
    int i;
    for (i = 0; i < 9; i++) /* XXX need to search '5' for chests under foot */
    {
        point_t p = point_step(p_ptr->pos, ddd[i]);
        if (dun_pos_interior(cave, p))
            _search_grid(p, dun_grid_at(cave, p));
    }
}

/*
 * Determine if a trap affects the player.
 * Always miss 5% of the time, Always hit 5% of the time.
 * Otherwise, match trap power against player armor.
 */
static int _check_hit(int power)
{
    int k, ac;

    /* Percentile dice */
    k = randint0(100);

    /* Hack -- 5% hit, 5% miss */
    if (k < 10) return (k < 5);

    /* Paranoia -- No power */
    if (power <= 0) return (FALSE);

    /* Total armor */
    ac = p_ptr->ac + p_ptr->to_a;

    /* Power competes against Armor */
    if (randint1(power) > ((ac * 3) / 4)) return (TRUE);

    /* Assume miss */
    return (FALSE);
}



/*
 * Handle player hitting a real trap
 */
static void hit_trap(bool break_trap)
{
    int i, num, dam;
    point_t pos = p_ptr->pos;

    /* Get the cave grid */
    cave_type *c_ptr = cave_at(pos);
    feature_type *f_ptr = &f_info[c_ptr->feat];
    int trap_feat_type = have_flag(f_ptr->flags, FF_TRAP) ? f_ptr->subtype : NOT_TRAP;

    cptr name = "a trap";

    /* Disturb the player */
    disturb(0, 0);

    cave_alter_feat(pos.y, pos.x, FF_HIT_TRAP);

    /* Analyze XXX XXX XXX */
    switch (trap_feat_type)
    {
        case TRAP_TRAPDOOR:
        {
            if (p_ptr->levitation)
            {
                msg_print("You fly over a trap door.");

            }
            else
            {
                msg_print("You have fallen through a trap door!");
                sound(SOUND_FALL);
                dam = damroll(2, 8);
                name = "a trap door";

                take_hit(DAMAGE_NOESCAPE, dam, name);
                dun_trap_door_plr(cave);
            }
            break;
        }

        case TRAP_PIT:
        {
            if (p_ptr->levitation)
            {
                msg_print("You fly over a pit trap.");

            }
            else
            {
                msg_print("You have fallen into a pit!");

                dam = damroll(2, 6);
                name = "a pit trap";

                take_hit(DAMAGE_NOESCAPE, dam, name);
            }
            break;
        }

        case TRAP_SPIKED_PIT:
        {
            if (p_ptr->levitation)
            {
                msg_print("You fly over a spiked pit.");

            }
            else
            {
                msg_print("You fall into a spiked pit!");


                /* Base damage */
                name = "a pit trap";

                dam = damroll(2, 6);

                /* Extra spike damage */
                if (randint0(100) < 50)
                {
                    msg_print("You are impaled!");


                    name = "a spiked pit";

                    dam = dam * 2;
                    if (!p_ptr->no_cut) plr_tim_add(T_CUT, randint1(dam));
                }

                /* Take the damage */
                take_hit(DAMAGE_NOESCAPE, dam, name);
            }
            break;
        }

        case TRAP_POISON_PIT:
        {
            if (p_ptr->levitation)
            {
                msg_print("You fly over a spiked pit.");

            }
            else
            {
                msg_print("You fall into a spiked pit!");


                /* Base damage */
                dam = damroll(2, 6);

                name = "a pit trap";


                /* Extra spike damage */
                if (randint0(100) < 50)
                {
                    msg_print("You are impaled on poisonous spikes!");


                    name = "a spiked pit";


                    dam = dam * 2;
                    if (!p_ptr->no_cut) plr_tim_add(T_CUT, randint1(dam));

                    if (res_save_default(RES_POIS))
                        msg_print("The poison does not affect you!");
                    else
                        plr_tim_add(T_POISON, dam);
                }

                /* Take the damage */
                take_hit(DAMAGE_NOESCAPE, dam, name);
            }

            break;
        }

        case TRAP_TY_CURSE:
        {
            msg_print("There is a flash of shimmering light!");

            num = 2 + randint1(3);
            for (i = 0; i < num; i++)
                summon_specific(0, pos, cave->dun_lvl, 0, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET));

            if (cave->dun_lvl > randint1(100)) /* No nasty effect for low levels */
            {
                bool stop_ty = FALSE;
                int count = 0;
                do {
                    stop_ty = activate_ty_curse(stop_ty, &count);
                } while (one_in_(6));
            }
            break;
        }

        case TRAP_TELEPORT:
        {
            msg_print("You hit a teleport trap!");

            teleport_player(100, TELEPORT_PASSIVE);
            break;
        }

        case TRAP_FIRE:
        {
            msg_print("You are enveloped in flames!");

            dam = damroll(4, 6);
            gf_affect_p(GF_WHO_TRAP, GF_FIRE, dam, GF_AFFECT_TRAP);
            break;
        }

        case TRAP_ACID:
        {
            msg_print("You are splashed with acid!");

            dam = damroll(4, 6);
            gf_affect_p(GF_WHO_TRAP, GF_ACID, dam, GF_AFFECT_TRAP);
            break;
        }

        case TRAP_SLOW:
        {
            if (_check_hit(125))
            {
                msg_print("A small dart hits you!");

                dam = damroll(1, 4);
                take_hit(DAMAGE_ATTACK, dam, "a dart trap");

                if (!CHECK_MULTISHADOW() && !free_act_save_p(cave->dun_lvl))
                    plr_tim_add(T_SLOW, randint0(20) + 20);
            }
            else
            {
                msg_print("A small dart barely misses you.");

            }
            break;
        }

        case TRAP_LOSE_STR:
        {
            if (_check_hit(125))
            {
                msg_print("A small dart hits you!");

                dam = damroll(1, 4);
                take_hit(DAMAGE_ATTACK, dam, "a dart trap");

                if (!CHECK_MULTISHADOW()) (void)do_dec_stat(A_STR);
            }
            else
            {
                msg_print("A small dart barely misses you.");

            }
            break;
        }

        case TRAP_LOSE_DEX:
        {
            if (_check_hit(125))
            {
                msg_print("A small dart hits you!");

                dam = damroll(1, 4);
                take_hit(DAMAGE_ATTACK, dam, "a dart trap");

                if (!CHECK_MULTISHADOW()) (void)do_dec_stat(A_DEX);
            }
            else
            {
                msg_print("A small dart barely misses you.");

            }
            break;
        }

        case TRAP_LOSE_CON:
        {
            if (_check_hit(125))
            {
                msg_print("A small dart hits you!");

                dam = damroll(1, 4);
                take_hit(DAMAGE_ATTACK, dam, "a dart trap");

                if (!CHECK_MULTISHADOW()) (void)do_dec_stat(A_CON);
            }
            else
            {
                msg_print("A small dart barely misses you.");

            }
            break;
        }

        case TRAP_BLIND:
        {
            msg_print("A black gas surrounds you!");
            if (!res_save_default(RES_BLIND))
                plr_tim_add(T_BLIND, randint0(50) + 25);
            break;
        }

        case TRAP_CONFUSE:
        {
            msg_print("A gas of scintillating colors surrounds you!");
            if (!res_save_default(RES_CONF))
                plr_tim_add(T_CONFUSED, randint0(20) + 10);
            break;
        }

        case TRAP_POISON:
        {
            msg_print("A pungent green gas surrounds you!");
            if (!res_save_default(RES_POIS))
                plr_tim_add(T_POISON, randint0(20) + 10);
            break;
        }

        case TRAP_SLEEP:
        {
            msg_print("A strange white mist surrounds you!");

            if (!free_act_save_p(0))
            {
                msg_print("You fall asleep.");
                plr_tim_add(T_PARALYZED, randint1(4));
            }
            else equip_learn_flag(OF_FREE_ACT);
            break;
        }

        case TRAP_TRAPS:
        {
            msg_print("There is a bright flash of light!");
            /* Make some new traps */
            project(0, 1, pos.y, pos.x, 0, GF_MAKE_TRAP, PROJECT_HIDE | PROJECT_JUMP | PROJECT_GRID);
            break;
        }

        case TRAP_ALARM:
        {
            msg_print("An alarm sounds!");
            aggravate_monsters(0);
            break;
        }

        case TRAP_OPEN:
        {
            msg_print("Suddenly, surrounding walls are opened!");
            /*TODO: Fire beams in 4 principle directions that kill adjacent walls ... */
            project(0, 10, pos.y, pos.x, 0, GF_DISINTEGRATE, PROJECT_GRID | PROJECT_HIDE);
            aggravate_monsters(0);
            break;
        }

        case TRAP_ARMAGEDDON:
        {
            static int levs[10] = {0, 0, 20, 10, 5, 3, 2, 1, 1, 1};
            int evil_idx = 0, good_idx = 0;

            int lev;
            msg_print("Suddenly, you are surrounded by immotal beings!");

            /* Summon Demons and Angels */
            for (lev = cave->dun_lvl; lev >= 20; lev -= 1 + lev/16)
            {
                num = levs[MIN(lev/10, 9)];
                for (i = 0; i < num; i++)
                {
                    point_t pos2 = point_random_jump(pos, 6);
                    if (!dun_pos_interior(cave, pos2)) continue;
                    if (!point_project(p_ptr->pos, pos2)) continue;
                    if (summon_specific(0, pos2, lev, SUMMON_ARMAGE_EVIL, (PM_NO_PET)))
                        evil_idx = hack_m_idx_ii;

                    if (summon_specific(0, pos2, lev, SUMMON_ARMAGE_GOOD, (PM_NO_PET)))
                        good_idx = hack_m_idx_ii;

                    /* Let them fight each other */
                    if (evil_idx && good_idx)
                    {
                        monster_type *evil_ptr = dun_mon(cave, evil_idx);
                        monster_type *good_ptr = dun_mon(cave, good_idx);
                        evil_ptr->target = good_ptr->pos;
                        good_ptr->target = evil_ptr->pos;
                    }
                }
            }
            break;
        }

        case TRAP_PIRANHA:
        {
            msg_print("Suddenly, the room is filled with water with piranhas!");

            /* Water fills room */
            fire_ball_hide(GF_WATER_FLOW, 0, 1, 10);

            /* Summon Piranhas */
            num = 1 + cave->dun_lvl/20;
            for (i = 0; i < num; i++)
                summon_specific(0, pos, cave->dun_lvl, SUMMON_PIRANHA, (PM_ALLOW_GROUP | PM_NO_PET));
            break;
        }
    }

    if (break_trap && is_trap(c_ptr->feat))
    {
        cave_alter_feat(pos.y, pos.x, FF_DISARM);
        msg_print("You destroyed the trap.");
    }
}

int calculate_dir(int sx, int sy, int tx, int ty)
{
    int dir;
    for (dir = 0; dir <= 9; dir++)
    {
        int x = sx + ddx[dir];
        int y = sy + ddy[dir];

        if (x == tx && y == ty) return dir;
    }
    return 5;
}

void do_monster_knockback(int x, int y, int dist)
{
    mon_ptr mon = dun_mon_at(cave, point_create(x, y));
    mon_race_ptr race = mon_race(mon);
    int     dir = calculate_dir(p_ptr->pos.x, p_ptr->pos.y, x, y);

    if (dir != 5)
    {
        int i;

        for (i = 0; i < dist; i++)
        {
            int ty = y, tx = x;
            int oy = y, ox = x;

            y += ddy[dir];
            x += ddx[dir];
            if (cave_empty_bold(y, x))
            {
                ty = y;
                tx = x;
            }
            if (ty != oy || tx != ox)
            {
                dun_move_mon(cave, mon, point_create(tx, ty));
                lite_spot(oy, ox);
                lite_spot(ty, tx);

                if (race->flags7 & (RF7_LITE_MASK | RF7_DARK_MASK))
                    p_ptr->update |= PU_MON_LITE;

                Term_xtra(TERM_XTRA_DELAY, delay_animation);
                Term_fresh();
            }
            else
                break;
        }
    }
}
bool random_opponent(int *y, int *x)
{
    int dirs[9];
    int ct = 0;
    int i;
    point_t p;

    for (i = 0; i < 8; i++)
    {
        p = point_step(p_ptr->pos, ddd[i]);
        if (mon_at(p))
            dirs[ct++] = i;
    }

    if (ct)
    {
        i = randint0(ct);
        p = point_step(p_ptr->pos, ddd[dirs[i]]);
        *y = p.y;
        *x = p.x;
        return TRUE;
    }
    return FALSE;
}

bool pattern_seq(point_t cur, point_t next)
{
    feature_type *cur_f_ptr = feat_at(cur);
    feature_type *new_f_ptr = feat_at(next);
    bool is_pattern_tile_cur = have_flag(cur_f_ptr->flags, FF_PATTERN);
    bool is_pattern_tile_new = have_flag(new_f_ptr->flags, FF_PATTERN);
    int pattern_type_cur, pattern_type_new;

    if (!is_pattern_tile_cur && !is_pattern_tile_new) return TRUE;

    pattern_type_cur = is_pattern_tile_cur ? cur_f_ptr->subtype : NOT_PATTERN_TILE;
    pattern_type_new = is_pattern_tile_new ? new_f_ptr->subtype : NOT_PATTERN_TILE;

    if (pattern_type_new == PATTERN_TILE_START)
    {
        if (!is_pattern_tile_cur && !plr_tim_find(T_CONFUSED) && !plr_tim_find(T_STUN) && !plr_tim_find(T_HALLUCINATE))
        {
            if (get_check("If you start walking the Pattern, you must walk the whole way. Ok? "))
                return TRUE;
            else
                return FALSE;
        }
        else
            return TRUE;
    }
    else if ((pattern_type_new == PATTERN_TILE_OLD) ||
         (pattern_type_new == PATTERN_TILE_END) ||
         (pattern_type_new == PATTERN_TILE_WRECKED))
    {
        if (is_pattern_tile_cur)
        {
            return TRUE;
        }
        else
        {
            msg_print("You must start walking the Pattern from the startpoint.");

            return FALSE;
        }
    }
    else if ((pattern_type_new == PATTERN_TILE_TELEPORT) ||
         (pattern_type_cur == PATTERN_TILE_TELEPORT))
    {
        return TRUE;
    }
    else if (pattern_type_cur == PATTERN_TILE_START)
    {
        if (is_pattern_tile_new)
            return TRUE;
        else
        {
            msg_print("You must walk the Pattern in correct order.");

            return FALSE;
        }
    }
    else if ((pattern_type_cur == PATTERN_TILE_OLD) ||
         (pattern_type_cur == PATTERN_TILE_END) ||
         (pattern_type_cur == PATTERN_TILE_WRECKED))
    {
        if (!is_pattern_tile_new)
        {
            msg_print("You may not step off from the Pattern.");

            return FALSE;
        }
        else
        {
            return TRUE;
        }
    }
    else
    {
        if (!is_pattern_tile_cur)
        {
            msg_print("You must start walking the Pattern from the startpoint.");

            return FALSE;
        }
        else
        {
            byte ok_move = PATTERN_TILE_START;
            switch (pattern_type_cur)
            {
                case PATTERN_TILE_1:
                    ok_move = PATTERN_TILE_2;
                    break;
                case PATTERN_TILE_2:
                    ok_move = PATTERN_TILE_3;
                    break;
                case PATTERN_TILE_3:
                    ok_move = PATTERN_TILE_4;
                    break;
                case PATTERN_TILE_4:
                    ok_move = PATTERN_TILE_1;
                    break;
                default:
                    if (p_ptr->wizard)
                        msg_format("Funny Pattern walking, %d.", pattern_type_cur);

                    return TRUE; /* Goof-up */
            }

            if ((pattern_type_new == ok_move) ||
                (pattern_type_new == pattern_type_cur))
                return TRUE;
            else
            {
                if (!is_pattern_tile_new)
                    msg_print("You may not step off from the Pattern.");
                else
                    msg_print("You must walk the Pattern in correct order.");

                return FALSE;
            }
        }
    }
}


bool player_can_enter(s16b feature, u16b mode)
{
    feature_type *f_ptr = &f_info[feature];

    if (p_ptr->riding)
    {
        mon_race_ptr race = plr_riding_race();
        return monster_can_cross_terrain(feature, race, mode | CEM_RIDING);
    }

    if (p_ptr->prace == RACE_MON_POSSESSOR && p_ptr->current_r_idx != MON_POSSESSOR_SOUL)
    {
        mon_race_ptr race = mon_race_lookup(p_ptr->current_r_idx);
        return monster_can_cross_terrain(feature, race, mode | CEM_MIMIC);
    }

    if (p_ptr->prace == RACE_MON_MIMIC && p_ptr->current_r_idx != MON_MIMIC)
    {
        mon_race_ptr race = mon_race_lookup(p_ptr->current_r_idx);
        return monster_can_cross_terrain(feature, race, mode | CEM_MIMIC);
    }

    /* Pattern */
    if (have_flag(f_ptr->flags, FF_PATTERN))
    {
        if (!(mode & CEM_P_CAN_ENTER_PATTERN)) return FALSE;
    }

    /* "CAN" flags */
    if (have_flag(f_ptr->flags, FF_CAN_FLY) && p_ptr->levitation) return TRUE;
    /*if (have_flag(f_ptr->flags, FF_CAN_CLIMB) && p_ptr->climbing) return TRUE;*/
    if (have_flag(f_ptr->flags, FF_CAN_SWIM) && p_ptr->can_swim) return TRUE;
    if (have_flag(f_ptr->flags, FF_CAN_PASS) && p_ptr->pass_wall) return TRUE;

    if (!have_flag(f_ptr->flags, FF_MOVE)) return FALSE;

    return TRUE;
}

static bool _auto_detect_traps(void)
{
    slot_t slot;
    if (!auto_detect_traps) return FALSE;
    if (p_ptr->pclass == CLASS_MAGIC_EATER && magic_eater_auto_detect_traps()) return TRUE;
    if ((prace_is_(RACE_DWARF) || prace_is_(RACE_NIBELUNG)) && p_ptr->lev > 20)
    {
        cast_detect_doors_stairs_traps();
        /* hp charge is only 5 ... forget about it. */
        return TRUE;
    }

    slot = pack_find_obj(TV_SCROLL, SV_SCROLL_DETECT_TRAP);
    if (slot && !plr_tim_find(T_BLIND) && !(get_race()->flags & RACE_IS_ILLITERATE))
    {
        obj_ptr scroll = pack_obj(slot);
        if (obj_is_known(scroll))
        {
            detect_traps(DETECT_RAD_DEFAULT, TRUE);
            stats_on_use(scroll, 1);
            scroll->number--;
            obj_release(scroll, 0);
            return TRUE;
        }
    }
    slot = pack_find_device(EFFECT_DETECT_TRAPS);
    if (slot)
    {
        obj_ptr device = pack_obj(slot);
        detect_traps(DETECT_RAD_DEFAULT, TRUE);
        stats_on_use(device, 1);
        device_decrease_sp(device, device->activation.cost);
        return TRUE;
    }
    slot = pack_find_device(EFFECT_DETECT_ALL);
    if (slot)
    {
        obj_ptr device = pack_obj(slot);
        detect_all(DETECT_RAD_DEFAULT);
        stats_on_use(device, 1);
        device_decrease_sp(device, device->activation.cost);
        return TRUE;
    }
    slot = quiver_find_device(EFFECT_DETECT_ALL);
    if (slot)
    {
        obj_ptr device = quiver_obj(slot);
        detect_all(DETECT_RAD_DEFAULT);
        stats_on_use(device, 1);
        device_decrease_sp(device, device->activation.cost);
        return TRUE;
    }
    return FALSE;
}

/*
 * Move the player
 */
bool move_player_effect(point_t pos, u32b mpe_mode)
{
    cave_type *c_ptr = cave_at(pos);
    feature_type *f_ptr = &f_info[c_ptr->feat];
    bool old_dtrap = FALSE, new_dtrap = FALSE;

    if (cave_at(p_ptr->pos)->info & CAVE_IN_DETECT)
        old_dtrap = TRUE;
    if (c_ptr->info & CAVE_IN_DETECT)
        new_dtrap = TRUE;

    /* Stop running if leaving a trap detected zone */
    if (!(mpe_mode & MPE_STAYING) && (running || travel.run) && disturb_trap_detect)
    {
        if (old_dtrap && !new_dtrap && !_auto_detect_traps())
        {
            disturb(0, 0);
            energy_use = 0;
            cmsg_print(TERM_VIOLET, "You are about to leave a trap detected zone.");
            return FALSE;
        }
    }

    if (cave_at(p_ptr->pos)->info & CAVE_IN_DETECT)
        old_dtrap = TRUE;
    if (c_ptr->info & CAVE_IN_DETECT)
        new_dtrap = TRUE;

    if (!(mpe_mode & MPE_STAYING))
    {
        point_t old_pos = p_ptr->pos;
        mon_ptr old_mon = mon_at(old_pos);
        mon_ptr new_mon = mon_at(pos);
        bool diagonal = old_pos.y != pos.y && old_pos.x != pos.x;

        /* Move the player */
        dun_move_plr(cave, pos);

        /* XXX (Proposed): Diagonal movement costs 50% more energy */
        if (diagonal)
            energy_use += 50;

        /* Hack -- For moving monster or riding player's moving */
        if (!(mpe_mode & MPE_DONT_SWAP_MON))
        {
            if (old_mon) dun_move_mon(cave, old_mon, pos);
            else if (new_mon) dun_move_mon(cave, new_mon, old_pos);
        }

        dun_lite_pos(cave, old_pos);
        dun_lite_pos(cave, pos);
        viewport_verify();

        /* Check detection status */
        if (old_dtrap && !new_dtrap)
        {
            if (alert_trap_detect)
            {
                cmsg_print(TERM_VIOLET, "You leave a trap detected zone.");
                msg_print(NULL); /* Force a -more- prompt (unless auto_more is enabled!) */
            }
            p_ptr->redraw |= PR_STATUS;
        }
        else if (!old_dtrap && new_dtrap)
        {
            if (alert_trap_detect)
                cmsg_print(TERM_L_BLUE, "You enter a trap detected zone.");
            p_ptr->redraw |= PR_STATUS;
        }

        if (mpe_mode & MPE_FORGET_FLOW)
        {
            dun_forget_flow(cave);
            p_ptr->update |= PU_UN_VIEW;
            p_ptr->redraw |= PR_MAP;
        }
        p_ptr->update |= PU_VIEW | PU_LITE | PU_FLOW | PU_MON_LITE;
        p_ptr->window |= PW_MONSTER_LIST | PW_OBJECT_LIST;

        /* Position Targets are confusing. They should be dismissed when no longer valid.
         * Note: Originally, I had this check in target_okay(), which is, of course, called
         * fairly often and repeatedly. While this had the fortunate side effect of preventing
         * many 'trick shot' projection abuses, it also messed up 'disintegration' effects
         * (such as Breathe Disintegration or Beam of Disintegration). For these, the user
         * needs to target a non-projectable monster. As a compromise, we will continue to
         * dismiss such targets, but only once the player moves. */
        if (target_who < 0)
        {
            point_t tgt = point_create(target_col, target_row);
            if (!dun_pos_interior(cave, tgt) || !point_project(p_ptr->pos, tgt))
            {
                target_who = 0;
                target_row = 0;
                target_col = 0;
                p_ptr->redraw |= PR_HEALTH_BARS;
            }
        }

        if (!view_unsafe_grids)
            p_ptr->redraw |= PR_STATUS;

        plr_hook_move_player();

        p_ptr->window |= PW_OVERHEAD | PW_DUNGEON;

        /* Remove "unsafe" flag */
        if ((!plr_tim_find(T_BLIND) && !no_lite()) || !is_trap(c_ptr->feat)) c_ptr->info &= ~(CAVE_UNSAFE);

        /* Handle stuff */
        if (mpe_mode & MPE_HANDLE_STUFF) handle_stuff();

        if (p_ptr->pclass == CLASS_NINJA || plr_tim_find(T_SUPERSTEALTH))
        {
            if (c_ptr->info & (CAVE_GLOW)) set_superstealth(FALSE);
            else if (p_ptr->cur_lite <= 0) set_superstealth(TRUE);
        }

        if (p_ptr->action == ACTION_QUICK_WALK &&
            (!have_flag(f_ptr->flags, FF_PROJECT) ||
             (!p_ptr->levitation && have_flag(f_ptr->flags, FF_DEEP))))
        {
            msg_print("You cannot run in here.");
            set_action(ACTION_NONE);

            if (weaponmaster_get_toggle() == TOGGLE_SHADOW_STANCE)
            {
                msg_print("Your shadow stance is disrupted!");
                weaponmaster_set_toggle(TOGGLE_NONE);
            }
        }
    }

    if (mpe_mode & MPE_ENERGY_USE)
    {
        if (music_singing(MUSIC_WALL))
        {
            (void)project(0, 0, p_ptr->pos.y, p_ptr->pos.x, (60 + p_ptr->lev), GF_DISINTEGRATE,
                PROJECT_KILL | PROJECT_ITEM);

            if (!plr_at(pos) || p_ptr->is_dead || p_ptr->leaving) return FALSE;
        }

        /* Spontaneous Searching */
        if ((p_ptr->skills.fos >= 50) || (0 == randint0(50 - p_ptr->skills.fos)))
            search();

        /* Continuous Searching */
        if (p_ptr->action == ACTION_SEARCH)
            search();
    }

    /* Handle "objects" */
    if (!(mpe_mode & MPE_DONT_PICKUP))
    {
        if (mpe_mode & MPE_DO_PICKUP)
            pack_get_floor();
        else
        {
            char name[MAX_NLEN_OBJ];
            obj_ptr obj;
            autopick_get_floor();
            for (obj = obj_at(p_ptr->pos); obj; obj = obj->next)
            {
                object_desc(name, obj, OD_COLOR_CODED);
                msg_format("You see %s.", name);
                disturb(0, 0);
            }
        }
    }

    /* Handle "store doors" */
    if (have_flag(f_ptr->flags, FF_STORE))
    {
        /* Disturb */
        disturb(0, 0);

        energy_use = 0;
        /* Hack -- Enter store */
        command_new = SPECIAL_KEY_STORE;
    }

    /* Handle "building doors" -KMW- */
    else if (have_flag(f_ptr->flags, FF_BLDG))
    {
        /* Disturb */
        disturb(0, 0);

        energy_use = 0;
        /* Hack -- Enter building */
        command_new = SPECIAL_KEY_BUILDING;
    }

    /* Handle quest areas -KMW- */
    else if (have_flag(f_ptr->flags, FF_QUEST_ENTER))
    {
        /* Disturb */
        disturb(0, 0);

        energy_use = 0;
        /* Hack -- Enter quest level */
        command_new = SPECIAL_KEY_QUEST;
    }
    else if (have_flag(f_ptr->flags, FF_TRAVEL))
    {
        disturb(0, 0);
        if (msg_prompt("Travel to next world? <color:y>[y/n]</color>", "ny", PROMPT_YES_NO) == 'y')
            dun_mgr_travel_plr();
    }
    else if (have_flag(f_ptr->flags, FF_RECALL))
    {
        disturb(0, 0);
        if (msg_prompt("Activate Recall? <color:y>[y/n]</color>", "ny", PROMPT_YES_NO) == 'y')
            dun_mgr_recall_plr();
    }
    /* Set off a trap */
    else if (have_flag(f_ptr->flags, FF_HIT_TRAP) && !(mpe_mode & MPE_STAYING))
    {
        /* Disturb */
        disturb(0, 0);

        /* Hidden trap */
        if (c_ptr->mimic || have_flag(f_ptr->flags, FF_SECRET))
        {
            /* Message */
            msg_print("You found a trap!");

            /* Pick a trap */
            disclose_grid(p_ptr->pos.y, p_ptr->pos.x);
        }

        /* Hit the trap */
        hit_trap((mpe_mode & MPE_BREAK_TRAP) ? TRUE : FALSE);

        if (!plr_at(pos) || p_ptr->is_dead || p_ptr->leaving) return FALSE;
    }

    return plr_at(pos) && !p_ptr->is_dead && !p_ptr->leaving;
}


bool trap_can_be_ignored(int feat)
{
    feature_type *f_ptr = &f_info[feat];

    if (!have_flag(f_ptr->flags, FF_TRAP)) return TRUE;

    switch (f_ptr->subtype)
    {
    case TRAP_TRAPDOOR:
    case TRAP_PIT:
    case TRAP_SPIKED_PIT:
    case TRAP_POISON_PIT:
        if (p_ptr->levitation) return TRUE;
        break;
    case TRAP_TELEPORT:
        if (p_ptr->anti_tele) return TRUE;
        break;
    case TRAP_FIRE:
        if (res_can_ignore(RES_FIRE)) return TRUE;
        break;
    case TRAP_ACID: /* Note: Your armor still gets messed up even if your pack is safe! */
        if (res_pct(RES_ACID) == 100) return TRUE;
        break;
    case TRAP_BLIND:
        if (res_can_ignore(RES_BLIND)) return TRUE;
        break;
    case TRAP_CONFUSE:
        if (res_can_ignore(RES_CONF)) return TRUE;
        break;
    case TRAP_POISON:
        if (res_can_ignore(RES_POIS)) return TRUE;
        break;
    }

    return FALSE;
}


/*
 * Determine if a "boundary" grid is "floor mimic"
 */
#define boundary_floor(C, F, MF) \
    ((C)->mimic && permanent_wall(F) && \
     (have_flag((MF)->flags, FF_MOVE) || have_flag((MF)->flags, FF_CAN_FLY)) && \
     have_flag((MF)->flags, FF_PROJECT) && \
     !have_flag((MF)->flags, FF_OPEN))

/*
 * Move player in the given direction, with the given "pickup" flag.
 *
 * This routine should (probably) always induce energy expenditure.
 *
 * Note that moving will *always* take a turn, and will *always* hit
 * any monster which might be in the destination grid. Previously,
 * moving into walls was "free" and did NOT hit invisible monsters.
 */
void move_player(int dir, bool do_pickup, bool break_trap)
{
    /* Find the result of moving */
    point_t pos = point_step(p_ptr->pos, dir);

    /* Examine the destination */
    cave_type    *c_ptr = cave_at(pos);
    feature_type *f_ptr = &f_info[c_ptr->feat];
    monster_type *m_ptr = mon_at(pos);

    char m_name[80];

    bool p_can_enter = player_can_enter(c_ptr->feat, CEM_P_CAN_ENTER_PATTERN);
    bool p_can_kill_walls = FALSE;
    bool stormbringer = equip_find_art(ART_STORMBRINGER);
    bool shadow_strike = FALSE;
    bool oktomove = TRUE;
    bool do_past = FALSE;
    bool ring_lev = p_ptr->prace == RACE_MON_RING && p_ptr->levitation;


    /* Player can not walk through "walls"... */
    /* unless in Shadow Form */
    p_can_kill_walls = p_ptr->kill_wall && have_flag(f_ptr->flags, FF_HURT_DISI) &&
        (!p_can_enter || !have_flag(f_ptr->flags, FF_LOS)) &&
        !have_flag(f_ptr->flags, FF_PERMANENT);

    /* Hack -- attack monsters */
    if (m_ptr && (m_ptr->ml || p_can_enter || p_can_kill_walls))
    {
        monster_race *r_ptr = mon_race(m_ptr);

        /* Normally, the plr should not attack friendly monsters when moving */
        if ( !is_hostile(m_ptr)
          && !( !m_ptr->ml 
             || plr_tim_find(T_CONFUSED)
             || plr_tim_find(T_HALLUCINATE)
             || plr_tim_find(T_STUN)
             || (mut_present(MUT_BERS_RAGE) && plr_tim_find(T_BERSERK)) )
          && pattern_seq(p_ptr->pos, pos)
          && (p_can_enter || p_can_kill_walls) )
        {
            mon_tim_delete(m_ptr, MT_SLEEP);
            monster_desc(m_name, m_ptr, 0); /* see "You push past %s" below */
            if (m_ptr->ml)
            {
                if (!plr_tim_find(T_HALLUCINATE)) mon_track(m_ptr);
                health_track(m_ptr->id);
            }

            /* displace? */
            if (stormbringer && randint1(1000) > 666)
            {
                plr_attack_normal(pos);
                oktomove = FALSE;
            }
            else if (monster_can_cross_terrain(cave_at(p_ptr->pos)->feat, r_ptr, 0))
            {
                do_past = TRUE;
            }
            else
            {
                msg_format("%^s is in your way!", m_name);
                energy_use = 0;
                oktomove = FALSE;
            }
            /* now continue on to 'movement' */
        }
        else
        {
            plr_attack_normal(pos);
            if (weaponmaster_get_toggle() == TOGGLE_SHADOW_STANCE)
                shadow_strike = TRUE; /* attack then swap positions */
            else
                oktomove = FALSE;
        }
    }

    if (!oktomove)
    {
    }
    else if (p_ptr->prace == RACE_MON_POSSESSOR || p_ptr->prace == RACE_MON_MIMIC)
    {
        monster_race *r_ptr = mon_race_lookup(p_ptr->current_r_idx);
        if (r_ptr->flags1 & RF1_NEVER_MOVE)
        {
            energy_use *= 3;
            /*msg_print("You can't move!");
            energy_use = 0;
            oktomove = FALSE;
            disturb(0, 0);*/
        }
        else if (have_flag(f_ptr->flags, FF_CAN_FLY) && ((r_ptr->flags7 & RF7_CAN_FLY) || p_ptr->levitation))
        {
            /* Allow moving */
        }
        else if (have_flag(f_ptr->flags, FF_CAN_CLIMB) && (r_ptr->flags7 & RF7_CAN_CLIMB))
        {
            /* Allow moving */
        }
        else if (have_flag(f_ptr->flags, FF_CAN_SWIM) && (r_ptr->flags7 & RF7_CAN_SWIM))
        {
            /* Allow moving */
        }
        else if (have_flag(f_ptr->flags, FF_WATER) &&
            !(r_ptr->flags7 & RF7_AQUATIC) &&
            (have_flag(f_ptr->flags, FF_DEEP) || mon_auras_find(r_ptr, GF_FIRE)) )
        {
            msg_print("You can't swim.");
            energy_use = 0;
            oktomove = FALSE;
            disturb(0, 0);
        }
        else if (!have_flag(f_ptr->flags, FF_WATER) && (r_ptr->flags7 & RF7_AQUATIC))
        {
            msg_print("You can't move onto dry land.");
            energy_use = 0;
            oktomove = FALSE;
            disturb(0, 0);
        }
    }
    else if (p_ptr->prace == RACE_MON_RING && !p_ptr->riding)
    {
        msg_print("You can't move! Try using your Glitter power to lure a ringbearer instead.");
        energy_use = 0;
        oktomove = FALSE;
        disturb(0, 0);
    }
    else if (p_ptr->riding)
    {
        mon_ptr riding_m_ptr = plr_riding_mon();
        mon_race_ptr riding_r_ptr = mon_race(riding_m_ptr);
        if (riding_r_ptr->flags1 & RF1_NEVER_MOVE)
        {
            msg_print("You can't move!");
            energy_use = 0;
            oktomove = FALSE;
            disturb(0, 0);
        }
        else if (mon_tim_find(riding_m_ptr, MT_SLEEP))
        {
            char m_name[80];
            monster_desc(m_name, riding_m_ptr, 0);
            msg_format("%^s is sleeping.", m_name);
            oktomove = FALSE;
            disturb(0,0);
        }
        else if (mon_tim_find(riding_m_ptr, T_PARALYZED))
        {
            char m_name[80];
            monster_desc(m_name, riding_m_ptr, 0);
            msg_format("%^s is paralyzed.", m_name);
            oktomove = FALSE;
            disturb(0,0);
        }
        else if (mon_tim_find(riding_m_ptr, T_FEAR))
        {
            char m_name[80];
            monster_desc(m_name, riding_m_ptr, 0);
            msg_format("%^s is too scared to control.", m_name);
            oktomove = FALSE;
            disturb(0, 0);
        }
        else if (p_ptr->riding_ryoute)
        {
            oktomove = FALSE;
            disturb(0, 0);
        }
        else if (have_flag(f_ptr->flags, FF_CAN_FLY) && ((riding_r_ptr->flags7 & RF7_CAN_FLY) || ring_lev))
        {
            /* Allow moving */
        }
        else if (have_flag(f_ptr->flags, FF_CAN_CLIMB) && (riding_r_ptr->flags7 & RF7_CAN_CLIMB))
        {
            /* Allow moving */
        }
        else if (have_flag(f_ptr->flags, FF_CAN_SWIM) && ((riding_r_ptr->flags7 & RF7_CAN_SWIM) || ring_lev))
        {
            /* Allow moving */
        }
        else if (have_flag(f_ptr->flags, FF_WATER) &&
            !(riding_r_ptr->flags7 & RF7_AQUATIC) &&
            (have_flag(f_ptr->flags, FF_DEEP) || mon_auras_find(riding_r_ptr, GF_FIRE)))
        {
            msg_print("Can't swim.");
            energy_use = 0;
            oktomove = FALSE;
            disturb(0, 0);
        }
        else if (!have_flag(f_ptr->flags, FF_WATER) && (riding_r_ptr->flags7 & RF7_AQUATIC))
        {
            msg_print("Can't land.");
            energy_use = 0;
            oktomove = FALSE;
            disturb(0, 0);
        }
        else if (have_flag(f_ptr->flags, FF_LAVA) && !(riding_r_ptr->flagsr & RFR_EFF_IM_FIRE_MASK))
        {
            msg_print("Too hot to go through.");
            energy_use = 0;
            oktomove = FALSE;
            disturb(0, 0);
        }

        if (oktomove && randint0(50) < mon_tim_amount(riding_m_ptr, T_STUN))
        {
            char m_name[80];
            monster_desc(m_name, riding_m_ptr, 0);
            msg_format("You cannot control stunned %s!",m_name);
            oktomove = FALSE;
            disturb(0, 0);
        }
    }
    else if ( !have_flag(f_ptr->flags, FF_MOVE)
           && (have_flag(f_ptr->flags, FF_CAN_FLY) || have_flag(f_ptr->flags, FF_CAN_CLIMB))
           && !p_ptr->levitation )
    {
        msg_format("You need to fly to go through the %s.", f_name + f_info[get_feat_mimic(c_ptr)].name);
        oktomove = FALSE;
        if (!shadow_strike)
            energy_use = 0;
        running = 0;
    }


    if (!oktomove)
    {
        /* FYI: Either the player was blocked from movement -OR- the player attacked
           because a monster was in the way.*/
    }
    /*
     * Player can move through trees and
     * has effective -10 speed
     */
    else if (have_flag(f_ptr->flags, FF_TREE) && !p_can_kill_walls)
    {
        if ( p_ptr->pclass != CLASS_RANGER
          && p_ptr->pclass != CLASS_SCOUT
          && !prace_is_(RACE_ENT)
          && !prace_is_(RACE_CENTAUR)
          && !prace_is_(RACE_WOOD_ELF)
          && !p_ptr->levitation
          && (!p_ptr->riding || !(plr_riding_race()->flags8 & RF8_WILD_WOOD)))
        {
            energy_use *= 2;
        }
    }
    else if (have_flag(f_ptr->flags, FF_WEB))
    {
        if (prace_is_(RACE_MON_SPIDER))
            energy_use = energy_use * (90 - p_ptr->lev) / 100;
        else if (warlock_is_(WARLOCK_SPIDERS))
            energy_use = energy_use * (150 - p_ptr->lev) / 150;
        else
            energy_use *= 2;
    }

#ifdef ALLOW_EASY_DISARM /* TNB */

    /* Disarm a visible trap */
    else if ((do_pickup != easy_disarm) && have_flag(f_ptr->flags, FF_DISARM) && !c_ptr->mimic)
    {
        if (!trap_can_be_ignored(c_ptr->feat))
        {
            (void)do_cmd_disarm_aux(pos.y, pos.x, dir);
            return;
        }
    }

#endif /* ALLOW_EASY_DISARM -- TNB */

    /* Player can not walk through "walls" unless in wraith form...*/
    else if (!p_can_enter && !p_can_kill_walls)
    {
        /* Feature code (applying "mimic" field) */
        s16b feat = get_feat_mimic(c_ptr);
        feature_type *mimic_f_ptr = &f_info[feat];
        cptr name = f_name + mimic_f_ptr->name;

        oktomove = FALSE;

        /* Disturb the player */
        disturb(0, 0);

        /* Notice things in the dark */
        if (!(c_ptr->info & CAVE_MARK) && !plr_can_see(pos))
        {
            /* Boundary floor mimic */
            if (boundary_floor(c_ptr, f_ptr, mimic_f_ptr))
            {
                msg_print("You feel you cannot go any more.");
            }

            /* Wall (or secret door) */
            else
            {
                msg_format("You feel %s %s blocking your way.",
                    is_a_vowel(name[0]) ? "an" : "a", name);

                c_ptr->info |= (CAVE_MARK | CAVE_AWARE);
                dun_lite_pos(cave, pos);
            }
        }

        /* Notice things */
        else
        {
            /* Boundary floor mimic */
            if (boundary_floor(c_ptr, f_ptr, mimic_f_ptr))
            {
                msg_print("You cannot go any more.");

                if (!(plr_tim_find(T_CONFUSED) || plr_tim_find(T_STUN) || plr_tim_find(T_HALLUCINATE)))
                {
                    if (!shadow_strike)
                        energy_use = 0;
                }
            }

            /* Wall (or secret door) */
            else
            {
#ifdef ALLOW_EASY_OPEN
                /* Closed doors */
                if (easy_open && is_closed_door(feat) && easy_open_door(pos.y, pos.x, dir))
                {
                    /* Hack. Try to deduce what happened since easy_open_door hides this.
                       Try to repeat attempting to unlock the door, but do a quick check
                       for jammed doors so we don't waste 99 turns. Also, only make
                       99 attempts to pick the lock ... But using command_rep would be
                       unwise since we will then run thru the door once we pick the lock! */
                    if (always_repeat)
                    {
                        static int _repeat_count = 0;

                        cave_type *c_ptr = cave_at(pos);
                        feature_type *f_ptr = &f_info[c_ptr->feat];

                        if (is_closed_door(c_ptr->feat) && have_flag(f_ptr->flags, FF_OPEN))
                        {
                            if (_repeat_count == 0)
                                _repeat_count = 99;
                            else
                                --_repeat_count;

                            if (_repeat_count)
                                command_rep = 1;
                        }
                        else
                            _repeat_count = 0;
                    }
                    return;
                }
#endif /* ALLOW_EASY_OPEN */

                msg_format("There is %s %s blocking your way.",
                    is_a_vowel(name[0]) ? "an" : "a", name);

                /*
                 * Well, it makes sense that you lose time bumping into
                 * a wall _if_ you are confused, stunned or blind; but
                 * typing mistakes should not cost you a turn...
                 */
                if (!(plr_tim_find(T_CONFUSED) || plr_tim_find(T_STUN) || plr_tim_find(T_HALLUCINATE)))
                {
                    if (!shadow_strike)
                        energy_use = 0;
                }
            }
        }

        /* Sound */
        if (!boundary_floor(c_ptr, f_ptr, mimic_f_ptr)) sound(SOUND_HITWALL);
    }

    if (oktomove)
    {
        if (have_flag(f_ptr->flags, FF_CAN_PASS) && !p_can_kill_walls && !elemental_is_(ELEMENTAL_EARTH))
            energy_use = energy_use * 3 / 2;

        if (have_flag(f_ptr->flags, FF_LAVA) && elemental_is_(ELEMENTAL_FIRE))
            energy_use /= 2;

        if (have_flag(f_ptr->flags, FF_WATER))
        {
            if (elemental_is_(ELEMENTAL_WATER))
                energy_use /= 2;
            else if (prace_is_(RACE_WATER_ELF))
                energy_use = energy_use * 7 / 10;
        }
    }

    /* Normal movement */
    if (oktomove && !pattern_seq(p_ptr->pos, pos))
    {
        if (!(plr_tim_find(T_CONFUSED) || plr_tim_find(T_STUN) || plr_tim_find(T_HALLUCINATE)))
        {
            if (!shadow_strike)
                energy_use = 0;
        }

        /* To avoid a loop with running */
        disturb(0, 0);

        oktomove = FALSE;
    }

    /* Normal movement */
    if (oktomove)
    {
        u32b mpe_mode = MPE_ENERGY_USE;

        if (p_ptr->warning)
        {
            if (!process_warning(pos))
            {
                if (!shadow_strike)
                    energy_use = 25;
                return;
            }
        }

        if (do_past)
            msg_format("You push past %s.", m_name);

        if (p_can_kill_walls)
        {
            cave_alter_feat(pos.y, pos.x, FF_HURT_DISI);
            p_ptr->update |= PU_FLOW;
        }

#ifdef ALLOW_EASY_DISARM /* TNB */

        if (do_pickup != always_pickup) mpe_mode |= MPE_DO_PICKUP;

#else /* ALLOW_EASY_DISARM -- TNB */

        if (do_pickup) mpe_mode |= MPE_DO_PICKUP;

#endif /* ALLOW_EASY_DISARM -- TNB */

        if (break_trap) mpe_mode |= MPE_BREAK_TRAP;

        /* Move the player */
        move_player_effect(pos, mpe_mode);
    }
}


static bool ignore_avoid_run;

/*
 * Hack -- Check for a "known wall" (see below)
 */
static int see_wall(int dir, int y, int x)
{
    cave_type   *c_ptr;

    /* Get the new location */
    y += ddy[dir];
    x += ddx[dir];

    /* Illegal grids are not known walls */
    if (!in_bounds2(y, x)) return (FALSE);

    /* Access grid */
    c_ptr = cave_at_xy(x, y);

    /* Must be known to the player */
    if (c_ptr->info & (CAVE_MARK))
    {
        /* Feature code (applying "mimic" field) */
        s16b         feat = get_feat_mimic(c_ptr);
        feature_type *f_ptr = &f_info[feat];

        /* Wall grids are known walls */
        if (!player_can_enter(feat, 0)) return !have_flag(f_ptr->flags, FF_DOOR);

        /* Don't run on a tree unless explicitly requested */
        if (have_flag(f_ptr->flags, FF_AVOID_RUN) && !ignore_avoid_run)
            return TRUE;

        /* Don't run in a wall */
        if (!have_flag(f_ptr->flags, FF_MOVE) && !have_flag(f_ptr->flags, FF_CAN_FLY))
            return !have_flag(f_ptr->flags, FF_DOOR);
    }

    return FALSE;
}


/*
 * Hack -- Check for an "unknown corner" (see below)
 */
static int see_nothing(int dir, int y, int x)
{
    /* Get the new location */
    y += ddy[dir];
    x += ddx[dir];

    /* Illegal grids are unknown */
    if (!in_bounds2(y, x)) return (TRUE);

    /* Memorized grids are always known */
    if (cave_at_xy(x, y)->info & (CAVE_MARK)) return (FALSE);

    /* Viewable door/wall grids are known */
    if (player_can_see_bold(y, x)) return (FALSE);

    /* Default */
    return (TRUE);
}





/*
 * The running algorithm:                       -CJS-
 *
 * In the diagrams below, the player has just arrived in the
 * grid marked as '@', and he has just come from a grid marked
 * as 'o', and he is about to enter the grid marked as 'x'.
 *
 * Of course, if the "requested" move was impossible, then you
 * will of course be blocked, and will stop.
 *
 * Overview: You keep moving until something interesting happens.
 * If you are in an enclosed space, you follow corners. This is
 * the usual corridor scheme. If you are in an open space, you go
 * straight, but stop before entering enclosed space. This is
 * analogous to reaching doorways. If you have enclosed space on
 * one side only (that is, running along side a wall) stop if
 * your wall opens out, or your open space closes in. Either case
 * corresponds to a doorway.
 *
 * What happens depends on what you can really SEE. (i.e. if you
 * have no light, then running along a dark corridor is JUST like
 * running in a dark room.) The algorithm works equally well in
 * corridors, rooms, mine tailings, earthquake rubble, etc, etc.
 *
 * These conditions are kept in static memory:
 * find_openarea    You are in the open on at least one side.
 * find_breakleft   You have a wall on the left, and will stop if it opens
 * find_breakright  You have a wall on the right, and will stop if it opens
 *
 * To initialize these conditions, we examine the grids adjacent
 * to the grid marked 'x', two on each side (marked 'L' and 'R').
 * If either one of the two grids on a given side is seen to be
 * closed, then that side is considered to be closed. If both
 * sides are closed, then it is an enclosed (corridor) run.
 *
 * LL           L
 * @x          LxR
 * RR          @R
 *
 * Looking at more than just the immediate squares is
 * significant. Consider the following case. A run along the
 * corridor will stop just before entering the center point,
 * because a choice is clearly established. Running in any of
 * three available directions will be defined as a corridor run.
 * Note that a minor hack is inserted to make the angled corridor
 * entry (with one side blocked near and the other side blocked
 * further away from the runner) work correctly. The runner moves
 * diagonally, but then saves the previous direction as being
 * straight into the gap. Otherwise, the tail end of the other
 * entry would be perceived as an alternative on the next move.
 *
 * #.#
 * ##.##
 * .@x..
 * ##.##
 * #.#
 *
 * Likewise, a run along a wall, and then into a doorway (two
 * runs) will work correctly. A single run rightwards from @ will
 * stop at 1. Another run right and down will enter the corridor
 * and make the corner, stopping at the 2.
 *
 * ##################
 * o@x       1
 * ########### ######
 * #2          #
 * #############
 *
 * After any move, the function area_affect is called to
 * determine the new surroundings, and the direction of
 * subsequent moves. It examines the current player location
 * (at which the runner has just arrived) and the previous
 * direction (from which the runner is considered to have come).
 *
 * Moving one square in some direction places you adjacent to
 * three or five new squares (for straight and diagonal moves
 * respectively) to which you were not previously adjacent,
 * marked as '!' in the diagrams below.
 *
 *   ...!              ...
 *   .o@!  (normal)    .o.!  (diagonal)
 *   ...!  (east)      ..@!  (south east)
 *                      !!!
 *
 * You STOP if any of the new squares are interesting in any way:
 * for example, if they contain visible monsters or treasure.
 *
 * You STOP if any of the newly adjacent squares seem to be open,
 * and you are also looking for a break on that side. (that is,
 * find_openarea AND find_break).
 *
 * You STOP if any of the newly adjacent squares do NOT seem to be
 * open and you are in an open area, and that side was previously
 * entirely open.
 *
 * Corners: If you are not in the open (i.e. you are in a corridor)
 * and there is only one way to go in the new squares, then turn in
 * that direction. If there are more than two new ways to go, STOP.
 * If there are two ways to go, and those ways are separated by a
 * square which does not seem to be open, then STOP.
 *
 * Otherwise, we have a potential corner. There are two new open
 * squares, which are also adjacent. One of the new squares is
 * diagonally located, the other is straight on (as in the diagram).
 * We consider two more squares further out (marked below as ?).
 *
 * We assign "option" to the straight-on grid, and "option2" to the
 * diagonal grid, and "check_dir" to the grid marked 's'.
 *
 * ##s
 * @x?
 * #.?
 *
 * If they are both seen to be closed, then it is seen that no benefit
 * is gained from moving straight. It is a known corner. To cut the
 * corner, go diagonally, otherwise go straight, but pretend you
 * stepped diagonally into that next location for a full view next
 * time. Conversely, if one of the ? squares is not seen to be closed,
 * then there is a potential choice. We check to see whether it is a
 * potential corner or an intersection/room entrance. If the square
 * two spaces straight ahead, and the space marked with 's' are both
 * unknown space, then it is a potential corner and enter if
 * find_examine is set, otherwise must stop because it is not a
 * corner. (find_examine option is removed and always is TRUE.)
 */




/*
 * Hack -- allow quick "cycling" through the legal directions
 */
static byte cycle[] =
{ 1, 2, 3, 6, 9, 8, 7, 4, 1, 2, 3, 6, 9, 8, 7, 4, 1 };

/*
 * Hack -- map each direction into the "middle" of the "cycle[]" array
 */
static byte chome[] =
{ 0, 8, 9, 10, 7, 0, 11, 6, 5, 4 };

/*
 * The direction we are running
 */
static byte find_current;

/*
 * The direction we came from
 */
static byte find_prevdir;

/*
 * We are looking for open area
 */
static bool find_openarea;

/*
 * We are looking for a break
 */
static bool find_breakright;
static bool find_breakleft;

static bool find_road;

/*
 * Initialize the running algorithm for a new direction.
 *
 * Diagonal Corridor -- allow diaginal entry into corridors.
 *
 * Blunt Corridor -- If there is a wall two spaces ahead and
 * we seem to be in a corridor, then force a turn into the side
 * corridor, must be moving straight into a corridor here. ???
 *
 * Diagonal Corridor    Blunt Corridor (?)
 *       # #                  #
 *       #x#                 @x#
 *       @p.                 p
 */
static void run_init(int dir)
{
    int row, col, deepleft, deepright;
    int i, shortleft, shortright;


    /* Save the direction */
    find_current = dir;

    /* Assume running straight */
    find_prevdir = dir;

    p_ptr->run_py = p_ptr->pos.y;
    p_ptr->run_px = p_ptr->pos.x;

    /* Hack: Stay on the road when running on surface */
    find_road = FALSE;
    if (cave->dun_type_id == D_SURFACE && cave_at(p_ptr->pos)->feat == feat_road)
    {
        point_t next = point_step(p_ptr->pos, dir);
        if (cave_at(next)->feat == feat_road)
        {
            find_road = TRUE;
            find_openarea = FALSE;
            find_breakright = find_breakleft = FALSE;
            return;
        }
    }

    /* Assume looking for open area */
    find_openarea = TRUE;

    /* Assume not looking for breaks */
    find_breakright = find_breakleft = FALSE;

    /* Assume no nearby walls */
    deepleft = deepright = FALSE;
    shortright = shortleft = FALSE;

    /* Find the destination grid */
    row = p_ptr->pos.y + ddy[dir];
    col = p_ptr->pos.x + ddx[dir];

    ignore_avoid_run = cave_have_flag_bold(row, col, FF_AVOID_RUN);


    /* Extract cycle index */
    i = chome[dir];

    /* Check for walls */
    if (see_wall(cycle[i+1], p_ptr->pos.y, p_ptr->pos.x))
    {
        find_breakleft = TRUE;
        shortleft = TRUE;
    }
    else if (see_wall(cycle[i+1], row, col))
    {
        find_breakleft = TRUE;
        deepleft = TRUE;
    }

    /* Check for walls */
    if (see_wall(cycle[i-1], p_ptr->pos.y, p_ptr->pos.x))
    {
        find_breakright = TRUE;
        shortright = TRUE;
    }
    else if (see_wall(cycle[i-1], row, col))
    {
        find_breakright = TRUE;
        deepright = TRUE;
    }

    /* Looking for a break */
    if (find_breakleft && find_breakright)
    {
        /* Not looking for open area */
        find_openarea = FALSE;

        /* Hack -- allow angled corridor entry */
        if (dir & 0x01)
        {
            if (deepleft && !deepright)
            {
                find_prevdir = cycle[i - 1];
            }
            else if (deepright && !deepleft)
            {
                find_prevdir = cycle[i + 1];
            }
        }

        /* Hack -- allow blunt corridor entry */
        else if (see_wall(cycle[i], row, col))
        {
            if (shortleft && !shortright)
            {
                find_prevdir = cycle[i - 2];
            }
            else if (shortright && !shortleft)
            {
                find_prevdir = cycle[i + 2];
            }
        }
    }
}


/*
 * Update the current "run" path
 *
 * Return TRUE if the running should be stopped
 */
static bool run_test(void)
{
    int         prev_dir, new_dir, check_dir = 0;
    int         row, col;
    int         i, max, inv;
    int         option = 0, option2 = 0;
    cave_type  *c_ptr;
    s16b        feat;
    feature_type *f_ptr;
    obj_ptr     obj;
    mon_ptr     mon;

    /* Where we came from */
    prev_dir = find_prevdir;


    /* Range of newly adjacent grids */
    max = (prev_dir & 0x01) + 1;

    /* Look at every newly adjacent square. */
    for (i = -max; i <= max; i++)
    {
        /* New direction */
        new_dir = cycle[chome[prev_dir] + i];

        /* New location */
        row = p_ptr->pos.y + ddy[new_dir];
        col = p_ptr->pos.x + ddx[new_dir];

        /* Access grid */
        c_ptr = cave_at_xy(col, row);

        /* Feature code (applying "mimic" field) */
        feat = get_feat_mimic(c_ptr);
        f_ptr = &f_info[feat];

        /* Visible monsters abort running */
        mon = mon_at_xy(col, row);
        if (mon && mon->ml) return TRUE;

        /* Visible objects abort running */
        for (obj = obj_at_xy(col, row); obj; obj = obj->next)
            if (obj->marked & OM_FOUND) return TRUE;

        /* Assume unknown */
        inv = TRUE;

        /* Check memorized grids */
        if (c_ptr->info & (CAVE_MARK))
        {
            bool notice = have_flag(f_ptr->flags, FF_NOTICE);

            if (notice && have_flag(f_ptr->flags, FF_MOVE))
            {
                /* Open doors */
                if (find_ignore_doors && have_flag(f_ptr->flags, FF_DOOR) && have_flag(f_ptr->flags, FF_CLOSE))
                {
                    /* Option -- ignore */
                    notice = FALSE;
                }

                /* Stairs */
                else if (find_ignore_stairs && have_flag(f_ptr->flags, FF_STAIRS))
                {
                    /* Option -- ignore */
                    notice = FALSE;
                }

                /* Lava */
                else if (have_flag(f_ptr->flags, FF_LAVA) && (res_pct(RES_FIRE) >= 100 || plr_tim_find(T_INVULN)))
                {
                    /* Ignore */
                    notice = FALSE;
                }

                /* Deep water */
                else if (have_flag(f_ptr->flags, FF_WATER) && have_flag(f_ptr->flags, FF_DEEP) &&
                         (p_ptr->levitation || p_ptr->can_swim || (plr_total_weight() <= weight_limit())))
                {
                    /* Ignore */
                    notice = FALSE;
                }
            }

            /* Interesting feature */
            if (notice) return (TRUE);

            /* The grid is "visible" */
            inv = FALSE;
        }

        /* Analyze unknown grids and floors considering mimic */
        if (inv || !see_wall(0, row, col))
        {
            /* Looking for open area */
            if (find_openarea)
            {
                /* Nothing */
            }
            else if (find_road && c_ptr->feat != feat_road)
            {
                /* Nothing */
            }

            /* The first new direction. */
            else if (!option)
            {
                option = new_dir;
            }

            /* Three new directions. Stop running. */
            else if (option2)
            {
                return (TRUE);
            }

            /* Two non-adjacent new directions. Stop running. */
            else if (option != cycle[chome[prev_dir] + i - 1])
            {
                return (TRUE);
            }

            /* Two new (adjacent) directions (case 1) */
            else if (new_dir & 0x01)
            {
                check_dir = cycle[chome[prev_dir] + i - 2];
                option2 = new_dir;
            }

            /* Two new (adjacent) directions (case 2) */
            else
            {
                check_dir = cycle[chome[prev_dir] + i + 1];
                option2 = option;
                option = new_dir;
            }
        }

        /* Obstacle, while looking for open area */
        else
        {
            if (find_openarea)
            {
                if (i < 0)
                {
                    /* Break to the right */
                    find_breakright = TRUE;
                }

                else if (i > 0)
                {
                    /* Break to the left */
                    find_breakleft = TRUE;
                }
            }
        }
    }

    /* Looking for open area */
    if (find_openarea)
    {
        /* Hack -- look again */
        for (i = -max; i < 0; i++)
        {
            /* Unknown grid or non-wall */
            if (!see_wall(cycle[chome[prev_dir] + i], p_ptr->pos.y, p_ptr->pos.x))
            {
                /* Looking to break right */
                if (find_breakright)
                {
                    return (TRUE);
                }
            }

            /* Obstacle */
            else
            {
                /* Looking to break left */
                if (find_breakleft)
                {
                    return (TRUE);
                }
            }
        }

        /* Hack -- look again */
        for (i = max; i > 0; i--)
        {
            /* Unknown grid or non-wall */
            if (!see_wall(cycle[chome[prev_dir] + i], p_ptr->pos.y, p_ptr->pos.x))
            {
                /* Looking to break left */
                if (find_breakleft)
                {
                    return (TRUE);
                }
            }

            /* Obstacle */
            else
            {
                /* Looking to break right */
                if (find_breakright)
                {
                    return (TRUE);
                }
            }
        }
    }

    /* Not looking for open area */
    else
    {
        /* No options */
        if (!option)
        {
            return (TRUE);
        }

        /* One option */
        else if (!option2)
        {
            /* Primary option */
            find_current = option;

            /* No other options */
            find_prevdir = option;
        }

        /* Two options, examining corners */
        else if (!find_cut)
        {
            /* Primary option */
            find_current = option;

            /* Hack -- allow curving */
            find_prevdir = option2;
        }

        /* Two options, pick one */
        else
        {
            /* Get next location */
            row = p_ptr->pos.y + ddy[option];
            col = p_ptr->pos.x + ddx[option];

            /* Don't see that it is closed off. */
            /* This could be a potential corner or an intersection. */
            if (!see_wall(option, row, col) ||
                !see_wall(check_dir, row, col))
            {
                /* Can not see anything ahead and in the direction we */
                /* are turning, assume that it is a potential corner. */
                if (see_nothing(option, row, col) &&
                    see_nothing(option2, row, col))
                {
                    find_current = option;
                    find_prevdir = option2;
                }

                /* STOP: we are next to an intersection or a room */
                else
                {
                    return (TRUE);
                }
            }

            /* This corner is seen to be enclosed; we cut the corner. */
            else if (find_cut)
            {
                find_current = option2;
                find_prevdir = option2;
            }

            /* This corner is seen to be enclosed, and we */
            /* deliberately go the long way. */
            else
            {
                find_current = option;
                find_prevdir = option2;
            }
        }
    }

    /* About to hit a known wall, stop */
    if (see_wall(find_current, p_ptr->pos.y, p_ptr->pos.x))
    {
        return (TRUE);
    }

    /* Failure */
    return (FALSE);
}



/*
 * Take one step along the current "run" path
 */
void run_step(int dir)
{
    /* Start running */
    if (dir)
    {
        /* Ignore AVOID_RUN on a first step */
        ignore_avoid_run = TRUE;

        /* Hack -- do not start silly run */
        if (see_wall(dir, p_ptr->pos.y, p_ptr->pos.x))
        {
            /* Message */
            msg_print("You cannot run in that direction.");

            /* Disturb */
            disturb(0, 0);

            /* Done */
            return;
        }

        /* Initialize */
        run_init(dir);
    }

    /* Keep running */
    else
    {
        /* Update run */
        if (run_test())
        {
            /* Disturb */
            disturb(0, 0);

            /* Done */
            return;
        }
    }

    /* Decrease the run counter */
    if (--running <= 0) return;

    /* Take time */
    energy_use = 100;

    /* Move the player, using the "pickup" flag */
#ifdef ALLOW_EASY_DISARM /* TNB */

    move_player(find_current, FALSE, FALSE);

#else /* ALLOW_EASY_DISARM -- TNB */

    move_player(find_current, always_pickup, FALSE);

#endif /* ALLOW_EASY_DISARM -- TNB */

    Term_xtra(TERM_XTRA_DELAY, delay_run);
    Term_fresh();

    if (player_bold(p_ptr->run_py, p_ptr->run_px))
    {
        p_ptr->run_py = 0;
        p_ptr->run_px = 0;
        disturb(0, 0);
    }
}


static int travel_cost(point_t pos)
{
    int cost = dun_flow_at(travel.flow, pos).cost;
    assert(travel.flow);
    if (!cost && !point_equals(pos, travel.pos))
        cost = 9999;
    return cost;
}

static bool _travel_abort_direct(point_t pos)
{
    mon_ptr mon;

    if (plr_tim_find(T_BLIND) || no_lite())
    {
        msg_print("You cannot see!");
        return TRUE;
    }
    if (disturb_trap_detect)
    {
        bool         old_dtrap = FALSE;
        bool         new_dtrap = FALSE;
        dun_grid_ptr grid = cave_at(pos);

        if (cave_at(p_ptr->pos)->info & CAVE_IN_DETECT)
            old_dtrap = TRUE;

        if (grid->info & CAVE_IN_DETECT)
            new_dtrap = TRUE;

        if (old_dtrap && !new_dtrap && !_auto_detect_traps())
        {
            cmsg_print(TERM_VIOLET, "You are about to leave a trap detected zone.");
            return TRUE;
        }
    }
    mon = mon_at(pos);
    if (mon && mon->ml) return TRUE;

    return FALSE;
}

static bool travel_abort(void)
{
    int prev_dir;
    int i, max;
    bool stop = TRUE;
    int current_cost = travel_cost(p_ptr->pos);

    /* Where we came from */
    prev_dir = find_prevdir;

    /* Range of newly adjacent grids */
    max = (prev_dir & 0x01) + 1;

    for (i = 0; i < 8; i++)
    {
        point_t pos = point_step(p_ptr->pos, ddd[i]);
        int     cost = travel_cost(pos);
        if (cost < current_cost)
            stop = FALSE;
    }

    if (stop) return TRUE;

    /* Cannot travel when blind */
    if (plr_tim_find(T_BLIND) || no_lite())
    {
        msg_print("You cannot see!");
        return TRUE;
    }

    /* Look at every newly adjacent square. */
    for (i = -max; i <= max; i++)
    {
        int          dir = cycle[chome[prev_dir] + i];
        point_t      pos = point_step(p_ptr->pos, dir);
        dun_grid_ptr grid;
        mon_ptr      mon;

        if (!dun_pos_interior(cave, pos)) continue;
        grid = dun_grid_at(cave, pos);

        if (disturb_trap_detect)
        {
            bool old_dtrap = FALSE;
            bool new_dtrap = FALSE;

            if (cave_at(p_ptr->pos)->info & CAVE_IN_DETECT)
                old_dtrap = TRUE;

            if (grid->info & CAVE_IN_DETECT)
                new_dtrap = TRUE;

            if (old_dtrap && !new_dtrap && !_auto_detect_traps())
            {
                cmsg_print(TERM_VIOLET, "You are about to leave a trap detected zone.");
                return TRUE;
            }
        }

        /* Visible monsters abort running */
        mon = mon_at(pos);
        if (mon && mon->ml) return TRUE;
    }

    return FALSE;
}

void travel_step(void)
{
    int dir = 0;
    int old_run = travel.run;
    point_t next_pos = {0};
    dun_grid_ptr grid;

    assert(travel.path);
    if (point_vec_length(travel.path))
    {
        point_t pos;

        assert(0 <= travel.path_idx && travel.path_idx < point_vec_length(travel.path) - 1);
        pos = point_vec_get(travel.path, travel.path_idx);
        next_pos = point_vec_get(travel.path, ++travel.path_idx);

        if (!point_equals(pos, p_ptr->pos) || _travel_abort_direct(next_pos))
        {
            disturb(0, 0);
            return;
        }
        dir = point_step_dir(p_ptr->pos, next_pos); /* XXX */
    }
    else
    {
        int i;
        int dirs[8] = { 2, 4, 6, 8, 1, 7, 9, 3 };

        assert(travel.flow);

        find_prevdir = travel.dir;

        if (travel_abort())
        {
            if (travel.run == 255)
                msg_print("No route is found!");
            disturb(0, 0);
            return;
        }


        for (i = 0; i < 8; i++)
        {
            int     d = dirs[i];
            point_t pt = point_step(p_ptr->pos, d);

            if (!dir || travel_cost(pt) < travel_cost(next_pos))
            {
                dir = d;
                next_pos = pt;
            }
        }
    }

    energy_use = 100;

    assert(dun_pos_interior(cave, next_pos));
    grid = dun_grid_at(cave, next_pos);

    /* Travelling is bumping into jammed doors and getting stuck */
    if (is_jammed_door(grid->feat))
    {
        disturb(0, 0);
        return;
    }

    /* Closed door */
    else if (is_closed_door(grid->feat))
    {
        if (!easy_open)
        {
            disturb(0, 0);
            return;
        }
    }
    /* Travelling is bumping into mountains and permanent walls and getting stuck */
    else if (!player_can_enter(grid->feat, 0))
    {
        disturb(0, 0);
        return;
    }

    travel.dir = dir;
    move_player(dir, always_pickup, easy_disarm);
    if (!point_equals(p_ptr->pos, next_pos)) /* open door; attack invisible monster; etc. */
        --travel.path_idx;
    Term_xtra(TERM_XTRA_DELAY, delay_run);
    Term_fresh();
    travel.run = old_run;

    if (point_equals(p_ptr->pos, travel.pos))
        travel_end();
    else
        travel.run--;
}


