#include "angband.h"

/* TODO: Rewrite ... I'd be OK with the do_spell() approach. */
static int damage;
static void mane_info(char *p, int power, int dam)
{
    int plev = p_ptr->lev;
    cptr s_dam = "dam ";
    cptr s_dur = "dur ";
    cptr s_range = "range ";
    cptr s_heal = "heal ";

    strcpy(p, "");
    if ((power > 2 && power < 41) || (power > 41 && power < 59) || (power == 75))
        sprintf(p, " %s%d", s_dam, dam);
    else
    {
        switch (power)
        {
            case 41:
                sprintf(p, " %sd%d+%d", s_heal, plev * 3, plev);
                break;
            case 64:
                sprintf(p, " %sd%d+%d", s_dur, 20+plev, plev);
                break;
            case 66:
                sprintf(p, " %s%d", s_heal, plev*6);
                break;
            case 67:
                sprintf(p, " %sd7+7", s_dur);
                break;
            case 68:
                sprintf(p, " %s10", s_range);
                break;
            case 69:
                sprintf(p, " %s%d", s_range, plev * 5);
                break;
            case 79:
                sprintf(p, " %s5", s_range);
                break;
            default:
                break;
        }
    }
}


/*
 * Allow user to choose a imitation.
 *
 * If a valid spell is chosen, saves it in '*sn' and returns TRUE
 * If the user hits escape, returns FALSE, and set '*sn' to -1
 * If there are no legal choices, returns FALSE, and sets '*sn' to -2
 *
 * The "prompt" should be "cast", "recite", or "study"
 * The "known" should be TRUE for cast/pray, FALSE for study
 *
 * nb: This function has a (trivial) display bug which will be obvious
 * when you run it. It's probably easy to fix but I haven't tried,
 * sorry.
 */
static int get_mane_power(int *sn, bool revenge)
{
    int             i = 0;
    int             num = 0;
    int             y = 1;
    int             x = 18;
    int             minfail = 0;
    int             plev = p_ptr->lev;
    int             chance = 0;
    int             ask;
    char            choice;
    char            out_val[160];
    char            comment[80];
    cptr            p = "power";
    monster_power   spell;
    bool            flag, redraw;

    *sn = -1;
    flag = FALSE;
    redraw = FALSE;
    num = p_ptr->mane_num;

    /* Build a prompt (accept all spells) */
    strnfmt(out_val, 78, "(%c-%c, *=List, ESC=exit) Use which %s? ",
              I2A(0), I2A(num - 1), p);

    /* Get a spell from the user */
    choice= always_show_list ? ESCAPE : 1;
    while (!flag)
    {
        if(choice==ESCAPE) choice = ' '; 
        else if(!get_com(out_val, &choice, TRUE) )break; 

        /* Request redraw */
        if ((choice == ' ') || (choice == '*') || (choice == '?'))
        {
            /* Show the list */
            if (!redraw)
            {
                char psi_desc[80];

                /* Show list */
                redraw = TRUE;

                /* Save the screen */
                screen_save();

                /* Display a list of spells */
                prt("", y, x);
                put_str("Name", y, x + 5);
                put_str("Fail Info", y, x + 35);

                /* Dump the spells */
                for (i = 0; i < num; i++)
                {
                    /* Access the spell */
                    spell = monster_powers[p_ptr->mane_spell[i]];

                    chance = spell.manefail;

                    /* Reduce failure rate by "effective" level adjustment */
                    if (plev > spell.level) chance -= 3 * (plev - spell.level);

                    /* Reduce failure rate by INT/WIS adjustment */
                    chance -= 3 * (adj_mag_stat[p_ptr->stat_ind[spell.use_stat]] + adj_mag_stat[p_ptr->stat_ind[A_DEX]] - 2) / 2;

                    if (spell.manedam) chance = chance * p_ptr->mane_dam[i] / spell.manedam;

                    chance += p_ptr->to_m_chance;

                    /* Extract the minimum failure rate */
                    minfail = adj_mag_fail[p_ptr->stat_ind[spell.use_stat]];

                    /* Minimum failure rate */
                    if (chance < minfail) chance = minfail;

                    /* Stunning makes spells harder */
                    if (p_ptr->stun > 50) chance += 25;
                    else if (p_ptr->stun) chance += 15;

                    /* Always a 5 percent chance of working */
                    if (chance > 95) chance = 95;

                    /* Get info */
                    mane_info(comment, p_ptr->mane_spell[i], (revenge ? p_ptr->mane_dam[i]*2 : p_ptr->mane_dam[i]));

                    /* Dump the spell --(-- */
                    sprintf(psi_desc, "  %c) %-30s %3d%%%s",
                        I2A(i), spell.name,
                        chance, comment);
                    prt(psi_desc, y + i + 1, x);
                }

                /* Clear the bottom line */
                prt("", y + i + 1, x);
            }

            /* Hide the list */
            else
            {
                /* Hide list */
                redraw = FALSE;

                /* Restore the screen */
                screen_load();
            }

            /* Redo asking */
            continue;
        }

        /* Note verify */
        ask = isupper(choice);

        /* Lowercase */
        if (ask) choice = tolower(choice);

        /* Extract request */
        i = (islower(choice) ? A2I(choice) : -1);

        /* Totally Illegal */
        if ((i < 0) || (i >= num))
        {
            bell();
            continue;
        }

        /* Save the spell index */
        spell = monster_powers[p_ptr->mane_spell[i]];

        /* Verify it */
        if (ask)
        {
            char tmp_val[160];

            /* Prompt */
            (void)strnfmt(tmp_val, 78, "Use %s? ", monster_powers[p_ptr->mane_spell[i]].name);

            /* Belay that order */
            if (!get_check(tmp_val)) continue;
        }

        /* Stop the loop */
        flag = TRUE;
    }

    /* Restore the screen */
    if (redraw) screen_load();

    /* Show choices */
    p_ptr->window |= (PW_SPELL);

    /* Window stuff */
    window_stuff();

    /* Abort if needed */
    if (!flag) return (FALSE);

    /* Save the choice */
    (*sn) = i;

    damage = (revenge ? p_ptr->mane_dam[i]*2 : p_ptr->mane_dam[i]);

    /* Success */
    return (TRUE);
}


/*
 * do_cmd_cast calls this function if the player's class
 * is 'imitator'.
 */
static bool use_mane(int spell)
{
    int             dir;
    int             plev = p_ptr->lev;
    u32b mode = (PM_ALLOW_GROUP | PM_FORCE_PET);
    u32b u_mode = 0L;

    if (randint1(50+plev) < plev/10) u_mode = PM_ALLOW_UNIQUE;

    /* spell code */
    switch (spell)
    {
    case MS_SHRIEK:
        msg_print("You make a high pitched shriek.");
        aggravate_monsters(0);
        break;
    case MS_XXX1:
        break;
    case MS_DISPEL:
    {
        int m_idx;

        if (!target_set(TARGET_KILL)) return FALSE;
        m_idx = cave[target_row][target_col].m_idx;
        if (!m_idx) break;
        if (!player_has_los_bold(target_row, target_col)) break;
        if (!projectable(py, px, target_row, target_col)) break;
        dispel_monster_status(m_idx);
        break;
    }
    case MS_ROCKET:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You fire a rocket.");
        fire_rocket(GF_ROCKET, dir, damage, 2);
        break;
    case MS_SHOOT:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You fire an arrow.");
        fire_bolt(GF_ARROW, dir, damage);
        break;
    case MS_XXX2:
        break;
    case MS_XXX3:
        break;
    case MS_BR_STORM:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You breathe storm.");
        fire_ball(GF_STORM, dir, damage, (plev > 35 ? -3 : -2));
        break;
    case MS_BR_ACID:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You breathe acid.");
        fire_ball(GF_ACID, dir, damage, (plev > 35 ? -3 : -2));
        break;
    case MS_BR_ELEC:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You breathe lightning.");
        fire_ball(GF_ELEC, dir, damage, (plev > 35 ? -3 : -2));
        break;
    case MS_BR_FIRE:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You breathe fire.");
        fire_ball(GF_FIRE, dir, damage, (plev > 35 ? -3 : -2));
        break;
    case MS_BR_COLD:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You breathe frost.");
        fire_ball(GF_COLD, dir, damage, (plev > 35 ? -3 : -2));
        break;
    case MS_BR_POIS:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You breathe gas.");
        fire_ball(GF_POIS, dir, damage, (plev > 35 ? -3 : -2));
        break;
    case MS_BR_NETHER:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You breathe nether.");
        fire_ball(GF_NETHER, dir, damage, (plev > 35 ? -3 : -2));
        break;
    case MS_BR_LITE:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You breathe light.");
        fire_ball(GF_LITE, dir, damage, (plev > 35 ? -3 : -2));
        break;
    case MS_BR_DARK:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You breathe darkness.");
        fire_ball(GF_DARK, dir, damage, (plev > 35 ? -3 : -2));
        break;
    case MS_BR_CONF:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You breathe confusion.");
        fire_ball(GF_CONFUSION, dir, damage, (plev > 35 ? -3 : -2));
        break;
    case MS_BR_SOUND:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You breathe sound.");
        fire_ball(GF_SOUND, dir, damage, (plev > 35 ? -3 : -2));
        break;
    case MS_BR_CHAOS:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You breathe chaos.");
        fire_ball(GF_CHAOS, dir, damage, (plev > 35 ? -3 : -2));
        break;
    case MS_BR_DISEN:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You breathe disenchantment.");
        fire_ball(GF_DISENCHANT, dir, damage, (plev > 35 ? -3 : -2));
        break;
    case MS_BR_NEXUS:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You breathe nexus.");
        fire_ball(GF_NEXUS, dir, damage, (plev > 35 ? -3 : -2));
        break;
    case MS_BR_TIME:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You breathe time.");
        fire_ball(GF_TIME, dir, damage, (plev > 35 ? -3 : -2));
        break;
    case MS_BR_INERTIA:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You breathe inertia.");
        fire_ball(GF_INERT, dir, damage, (plev > 35 ? -3 : -2));
        break;
    case MS_BR_GRAVITY:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You breathe gravity.");
        fire_ball(GF_GRAVITY, dir, damage, (plev > 35 ? -3 : -2));
        break;
    case MS_BR_SHARDS:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You breathe shards.");
        fire_ball(GF_SHARDS, dir, damage, (plev > 35 ? -3 : -2));
        break;
    case MS_BR_PLASMA:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You breathe plasma.");
        fire_ball(GF_PLASMA, dir, damage, (plev > 35 ? -3 : -2));
        break;
    case MS_BR_FORCE:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You breathe force.");
        fire_ball(GF_FORCE, dir, damage, (plev > 35 ? -3 : -2));
        break;
    case MS_BR_MANA:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You breathe mana.");
        fire_ball(GF_MANA, dir, damage, (plev > 35 ? -3 : -2));
        break;
    case MS_BALL_NUKE:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You cast a ball of radiation.");
        fire_ball(GF_NUKE, dir, damage, 2);
        break;
    case MS_BR_NUKE:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You breathe toxic waste.");
        fire_ball(GF_NUKE, dir, damage, (plev > 35 ? -3 : -2));
        break;
    case MS_BALL_CHAOS:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You invoke a raw Logrus.");
        fire_ball(GF_CHAOS, dir, damage, 4);
        break;
    case MS_BR_DISI:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You breathe disintegration.");
        fire_ball(GF_DISINTEGRATE, dir, damage, (plev > 35 ? -3 : -2));
        break;
    case MS_BALL_ACID:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You cast an acid ball.");
        fire_ball(GF_ACID, dir, damage, 2);
        break;
    case MS_BALL_ELEC:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You cast a lightning ball.");
        fire_ball(GF_ELEC, dir, damage, 2);
        break;
    case MS_BALL_FIRE:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You cast a fire ball.");
        fire_ball(GF_FIRE, dir, damage, 2);
        break;
    case MS_BALL_COLD:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You cast a frost ball.");
        fire_ball(GF_COLD, dir, damage, 2);
        break;
    case MS_BALL_POIS:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You cast a stinking cloud.");
        fire_ball(GF_POIS, dir, damage, 2);
        break;
    case MS_BALL_NETHER:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You cast a nether ball.");
        fire_ball(GF_NETHER, dir, damage, 2);
        break;
    case MS_BALL_WATER:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You gesture fluidly.");
        fire_ball(GF_WATER, dir, damage, 4);
        break;
    case MS_BALL_MANA:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You invoke a mana storm.");
        fire_ball(GF_MANA, dir, damage, 4);
        break;
    case MS_BALL_DARK:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You invoke a darkness storm.");
        fire_ball(GF_DARK, dir, damage, 4);
        break;
    case MS_DRAIN_MANA:
        if (!get_aim_dir(&dir)) return FALSE;
        fire_ball_hide(GF_DRAIN_MANA, dir, randint1(plev*3)+plev, 0);
        break;
    case MS_MIND_BLAST:
        if (!get_aim_dir(&dir)) return FALSE;
        fire_ball_hide(GF_MIND_BLAST, dir, damage, 0);
        break;
    case MS_BRAIN_SMASH:
        if (!get_aim_dir(&dir)) return FALSE;
        fire_ball_hide(GF_BRAIN_SMASH, dir, damage, 0);
        break;
    case MS_CAUSE_1:
        if (!get_aim_dir(&dir)) return FALSE;
        fire_ball_hide(GF_CAUSE_1, dir, damage, 0);
        break;
    case MS_CAUSE_2:
        if (!get_aim_dir(&dir)) return FALSE;
        fire_ball_hide(GF_CAUSE_2, dir, damage, 0);
        break;
    case MS_CAUSE_3:
        if (!get_aim_dir(&dir)) return FALSE;
        fire_ball_hide(GF_CAUSE_3, dir, damage, 0);
        break;
    case MS_CAUSE_4:
        if (!get_aim_dir(&dir)) return FALSE;
        fire_ball_hide(GF_CAUSE_4, dir, damage, 0);
        break;
    case MS_BOLT_ACID:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You cast an acid bolt.");
        fire_bolt(GF_ACID, dir, damage);
        break;
    case MS_BOLT_ELEC:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You cast a lightning bolt.");
        fire_bolt(GF_ELEC, dir, damage);
        break;
    case MS_BOLT_FIRE:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You cast a fire bolt.");
        fire_bolt(GF_FIRE, dir, damage);
        break;
    case MS_BOLT_COLD:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You cast a frost bolt.");
        fire_bolt(GF_COLD, dir, damage);
        break;
    case MS_STARBURST:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You invoke a starburst.");
        fire_ball(GF_LITE, dir, damage, 4);
        break;
    case MS_BOLT_NETHER:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You cast a nether bolt.");
        fire_bolt(GF_NETHER, dir, damage);
        break;
    case MS_BOLT_WATER:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You cast a water bolt.");
        fire_bolt(GF_WATER, dir, damage);
        break;
    case MS_BOLT_MANA:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You cast a mana bolt.");
        fire_bolt(GF_MANA, dir, damage);
        break;
    case MS_BOLT_PLASMA:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You cast a plasma bolt.");
        fire_bolt(GF_PLASMA, dir, damage);
        break;
    case MS_BOLT_ICE:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You cast a ice bolt.");
        fire_bolt(GF_ICE, dir, damage);
        break;
    case MS_MAGIC_MISSILE:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You cast a magic missile.");
        fire_bolt(GF_MISSILE, dir, damage);
        break;
    case MS_SCARE:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You cast a fearful illusion.");
        fear_monster(dir, plev+10);
        break;
    case MS_BLIND:
        if (!get_aim_dir(&dir)) return FALSE;
        confuse_monster(dir, plev * 2);
        break;
    case MS_CONF:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You cast a mesmerizing illusion.");
        confuse_monster(dir, plev * 2);
        break;
    case MS_SLOW:
        if (!get_aim_dir(&dir)) return FALSE;
        slow_monster(dir);
        break;
    case MS_SLEEP:
        if (!get_aim_dir(&dir)) return FALSE;
        sleep_monster(dir, plev*3);
        break;
    case MS_SPEED:
        (void)set_fast(randint1(20 + plev) + plev, FALSE);
        break;
    case MS_HAND_DOOM:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You invoke the Hand of Doom!");
        fire_ball_hide(GF_HAND_DOOM, dir, 200, 0);
        break;
    case MS_HEAL:
        msg_print("You concentrate on your wounds!");
        (void)hp_player(plev*6);
        (void)set_stun(0, TRUE);
        (void)set_cut(0, TRUE);
        break;
    case MS_INVULNER:
        msg_print("You cast a Globe of Invulnerability.");
        (void)set_invuln(randint1(7) + 7, FALSE);
        break;
    case MS_BLINK:
        if (mut_present(MUT_ASTRAL_GUIDE))
            energy_use = 30;
        teleport_player(10, 0L);
        break;
    case MS_TELEPORT:
        if (mut_present(MUT_ASTRAL_GUIDE))
            energy_use = 30;
        teleport_player(plev * 5, 0L);
        break;
    case MS_WORLD:
        world_player = TRUE;
        if (damage == 1 || damage == 2)
            msg_print("You yell 'The World! Time has stopped!'");
        else if (damage == 3 || damage == 6)
            msg_print("You yell 'Time!'");
        else
            msg_print("hek!");
        msg_print(NULL);

        p_ptr->energy_need -= 1000 + (100 + randint1(200)+200)*TURNS_PER_TICK/10;
        p_ptr->redraw |= (PR_MAP);
        p_ptr->update |= (PU_MONSTERS);
        p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);
        handle_stuff();
        break;
    case MS_SPECIAL:
        break;
    case MS_TELE_TO:
    {
        monster_type *m_ptr;
        monster_race *r_ptr;
        char m_name[80];

        if (!target_set(TARGET_KILL)) return FALSE;
        if (!cave[target_row][target_col].m_idx) break;
        if (!player_has_los_bold(target_row, target_col)) break;
        if (!projectable(py, px, target_row, target_col)) break;
        m_ptr = &m_list[cave[target_row][target_col].m_idx];
        r_ptr = &r_info[m_ptr->r_idx];
        monster_desc(m_name, m_ptr, 0);
        if (r_ptr->flagsr & RFR_RES_TELE)
        {
            if ((r_ptr->flags1 & (RF1_UNIQUE)) || (r_ptr->flagsr & RFR_RES_ALL))
            {
                mon_lore_r(m_ptr, RFR_RES_TELE);
                msg_format("%s is unaffected!", m_name);
                break;
            }
            else if (r_ptr->level > randint1(100))
            {
                mon_lore_r(m_ptr, RFR_RES_TELE);
                msg_format("%s resists!", m_name);
                break;
            }
        }
        msg_format("You command %s to return.", m_name);
        teleport_monster_to(cave[target_row][target_col].m_idx, py, px, 100, TELEPORT_PASSIVE);
        break;
    }
    case MS_TELE_AWAY:
        if (!get_aim_dir(&dir)) return FALSE;
        (void)fire_beam(GF_AWAY_ALL, dir, plev);
        break;
    case MS_TELE_LEVEL:
    {
        int target_m_idx;
        monster_type *m_ptr;
        monster_race *r_ptr;
        char m_name[80];

        if (!target_set(TARGET_KILL)) return FALSE;
        target_m_idx = cave[target_row][target_col].m_idx;
        if (!target_m_idx) break;
        if (!player_has_los_bold(target_row, target_col)) break;
        if (!projectable(py, px, target_row, target_col)) break;
        m_ptr = &m_list[target_m_idx];
        r_ptr = &r_info[m_ptr->r_idx];
        monster_desc(m_name, m_ptr, 0);
        msg_format("You gesture at %^s's feet.", m_name);
        if ((r_ptr->flagsr & (RFR_EFF_RES_NEXU_MASK | RFR_RES_TELE)) ||
            (r_ptr->flags1 & RF1_QUESTOR) || (r_ptr->level + randint1(50) > plev + randint1(60)))
        {
            msg_format("%^s is unaffected!", m_name);
        }
        else teleport_level(target_m_idx);
        break;
    }
    case MS_PSY_SPEAR:
        if (!get_aim_dir(&dir)) return FALSE;
        else msg_print("You throw a psycho-spear.");
        fire_beam(GF_PSY_SPEAR, dir, damage);
        break;
    case MS_DARKNESS:
        msg_print("You gesture in shadow.");
        unlite_area(10, 3);
        break;
    case MS_MAKE_TRAP:
        if (!target_set(TARGET_KILL)) return FALSE;
        msg_print("You cast a spell and cackle evilly.");
        trap_creation(target_row, target_col);
        break;
    case MS_FORGET:
        msg_print("Nothing happens.");
        /* TODO: forget_spell(m_idx) ... */
        break;
    case MS_RAISE_DEAD:
        msg_print("You cast a animate dead.");
        animate_dead(0, py, px);
        break;
    case MS_S_KIN:
    {
        int k;
        if (!target_set(TARGET_KILL)) return FALSE;
        msg_print("You summon minions.");
        for (k = 0;k < 4; k++)
        {
            (void)summon_kin_player(plev, target_row, target_col, (PM_FORCE_PET | PM_ALLOW_GROUP));
        }
        break;
    }
    case MS_S_CYBER:
    {
        int k;
        int max_cyber = (dun_level / 50) + randint1(3);
        if (!target_set(TARGET_KILL)) return FALSE;
            msg_print("You summon Cyberdemons!");
        if (max_cyber > 4) max_cyber = 4;
        for (k = 0;k < max_cyber; k++)
            summon_specific(-1, target_row, target_col, plev, SUMMON_CYBER, mode);
        break;
    }
    case MS_S_MONSTER:
    {
        int k;
        if (!target_set(TARGET_KILL)) return FALSE;
        msg_print("You summon help.");
        for (k = 0;k < 1; k++)
            summon_specific(-1, target_row, target_col, plev, 0, (mode | u_mode));
        break;
    }
    case MS_S_MONSTERS:
    {
        int k;
        if (!target_set(TARGET_KILL)) return FALSE;
            msg_print("You summon monsters!");
        for (k = 0;k < 6; k++)
            summon_specific(-1, target_row, target_col, plev, 0, (mode | u_mode));
        break;
    }
    case MS_S_ANT:
    {
        int k;
        if (!target_set(TARGET_KILL)) return FALSE;
        msg_print("You summon ants.");
        for (k = 0;k < 6; k++)
            summon_specific(-1, target_row, target_col, plev, SUMMON_ANT, mode);
        break;
    }
    case MS_S_SPIDER:
    {
        int k;
        if (!target_set(TARGET_KILL)) return FALSE;
        msg_print("You summon spiders.");
        for (k = 0;k < 6; k++)
            summon_specific(-1, target_row, target_col, plev, SUMMON_SPIDER, mode);
        break;
    }
    case MS_S_HOUND:
    {
        int k;
        if (!target_set(TARGET_KILL)) return FALSE;
        msg_print("You summon hounds.");
        for (k = 0;k < 4; k++)
            summon_specific(-1, target_row, target_col, plev, SUMMON_HOUND, mode);
        break;
    }
    case MS_S_HYDRA:
    {
        int k;
        if (!target_set(TARGET_KILL)) return FALSE;
        msg_print("You summon hydras.");
        for (k = 0;k < 4; k++)
            summon_specific(-1, target_row, target_col, plev, SUMMON_HYDRA, mode);
        break;
    }
    case MS_S_ANGEL:
    {
        int k;
        if (!target_set(TARGET_KILL)) return FALSE;
        msg_print("You summon angel!");
        for (k = 0;k < 1; k++)
            summon_specific(-1, target_row, target_col, plev, SUMMON_ANGEL, mode);
        break;
    }
    case MS_S_DEMON:
    {
        int k;
        if (!target_set(TARGET_KILL)) return FALSE;
        msg_print("You summon a demon from the Courts of Chaos!");
        for (k = 0;k < 1; k++)
            summon_specific(-1, target_row, target_col, plev, SUMMON_DEMON, (mode | u_mode));
        break;
    }
    case MS_S_UNDEAD:
    {
        int k;
        if (!target_set(TARGET_KILL)) return FALSE;
        msg_print("You summon an undead adversary!");
        for (k = 0;k < 1; k++)
            summon_specific(-1, target_row, target_col, plev, SUMMON_UNDEAD, (mode | u_mode));
        break;
    }
    case MS_S_DRAGON:
    {
        int k;
        if (!target_set(TARGET_KILL)) return FALSE;
        msg_print("You summon dragon!");
        for (k = 0;k < 1; k++)
            summon_specific(-1, target_row, target_col, plev, SUMMON_DRAGON, (mode | u_mode));
        break;
    }
    case MS_S_HI_UNDEAD:
    {
        int k;
        if (!target_set(TARGET_KILL)) return FALSE;
        msg_print("You summon greater undead!");
        for (k = 0;k < 6; k++)
            summon_specific(-1, target_row, target_col, plev, SUMMON_HI_UNDEAD, (mode | u_mode));
        break;
    }
    case MS_S_HI_DRAGON:
    {
        int k;
        if (!target_set(TARGET_KILL)) return FALSE;
        msg_print("You summon ancient dragons!");
        for (k = 0;k < 4; k++)
            summon_specific(-1, target_row, target_col, plev, SUMMON_HI_DRAGON, (mode | u_mode));
        break;
    }
    case MS_S_AMBERITE:
    {
        int k;
        if (!target_set(TARGET_KILL)) return FALSE;
        msg_print("You summon Lords of Amber!");
        for (k = 0;k < 4; k++)
            summon_specific(-1, target_row, target_col, plev, SUMMON_AMBERITE, (mode | PM_ALLOW_UNIQUE));
        break;
    }
    case MS_S_UNIQUE:
    {
        int k, count = 0;
        if (!target_set(TARGET_KILL)) return FALSE;
        msg_print("You summon special opponents!");
        for (k = 0;k < 4; k++)
            if (summon_specific(-1, target_row, target_col, plev, SUMMON_UNIQUE, (mode | PM_ALLOW_UNIQUE))) count++;
        for (k = count;k < 4; k++)
            summon_specific(-1, target_row, target_col, plev, SUMMON_HI_UNDEAD, (mode | u_mode));
        break;
    }
    default:
        msg_print("hoge?");
    }

    return TRUE;
}


bool imitator_cast(bool revenge)
{
    int             n = 0, j;
    int             chance;
    int             minfail = 0;
    int             plev = p_ptr->lev;
    monster_power   spell;
    bool            cast;

    if (p_ptr->confused)
    {
        msg_print("You are too confused!");
        return TRUE;
    }

    if (!p_ptr->mane_num)
    {
        msg_print("You don't remember any action!");
        return FALSE;
    }

    /* get power */
    if (!get_mane_power(&n, revenge)) return FALSE;

    spell = monster_powers[p_ptr->mane_spell[n]];

    /* Spell failure chance */
    chance = spell.manefail;

    /* Reduce failure rate by "effective" level adjustment */
    if (plev > spell.level) chance -= 3 * (plev - spell.level);

    /* Reduce failure rate by 1 stat and DEX adjustment */
    chance -= 3 * (adj_mag_stat[p_ptr->stat_ind[spell.use_stat]] + adj_mag_stat[p_ptr->stat_ind[A_DEX]] - 2) / 2;

    if (spell.manedam) chance = chance * damage / spell.manedam;

    chance += p_ptr->to_m_chance;

    /* Extract the minimum failure rate */
    minfail = adj_mag_fail[p_ptr->stat_ind[spell.use_stat]];

    /* Minimum failure rate */
    if (chance < minfail) chance = minfail;

    /* Stunning makes spells harder */
    if (p_ptr->stun > 50) chance += 25;
    else if (p_ptr->stun) chance += 15;

    /* Always a 5 percent chance of working */
    if (chance > 95) chance = 95;

    /* Failed spell */
    if (randint0(100) < chance)
    {
        if (flush_failure) flush();
        msg_print("You failed to concentrate hard enough!");
        sound(SOUND_FAIL);
    }
    else
    {
        sound(SOUND_ZAP);
        cast = use_mane(p_ptr->mane_spell[n]);
        if (!cast) return FALSE;
    }

    p_ptr->mane_num--;
    for (j = n; j < p_ptr->mane_num;j++)
    {
        p_ptr->mane_spell[j] = p_ptr->mane_spell[j+1];
        p_ptr->mane_dam[j] = p_ptr->mane_dam[j+1];
    }

    energy_use = 100;
    p_ptr->redraw |= PR_EFFECTS;
    p_ptr->window |= (PW_SPELL);
    return TRUE;
}

static void _double_revenge_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Double Revenge");
        break;
    case SPELL_DESC:
        var_set_string(res, "");
        break;
    case SPELL_CAST:
        handle_stuff();
        var_set_bool(res, imitator_cast(TRUE));
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static int _get_powers(spell_info* spells, int max)
{
    int ct = 0;

    spell_info* spell = &spells[ct++];
    spell->level = 30;
    spell->cost = 100;
    spell->fail = calculate_fail_rate(spell->level, 90, p_ptr->stat_ind[A_DEX]);
    spell->fn = _double_revenge_spell;

    return ct;
}

class_t *imitator_get_class(void)
{
    static class_t me = {0};
    static bool init = FALSE;

    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 25,  30,  36,   2,  18,  16,  60,  50};
    skills_t xs = {  7,  10,  10,   0,   0,   0,  18,  20};

        me.name = "Imitator";
        me.desc = "Imitators have enough fighting skills to survive, but rely on "
                    "their unique technique - 'Imitation' - which imitates monster "
                    "spells/techniques including their damage and duration. Dexterity "
                    "determines general imitation ability, but a stat related to the "
                    "specific action is often also taken into account.\n \n"
                    "To use imitation, Imitators must see monster's spell at first. "
                    "When a viewable monsters uses a spell, it is added to a temporary "
                    "spell list which the imitator can choose from. Spells should be "
                    "imitated quickly, because timing and situation are everything. An "
                    "imitator can only repeat a spell once each time he observes it. "
                    "They only have a small long-term memory for spells, which ranges "
                    "from one to three, depending on their level. When they memorize "
                    "more spells than this, they will forget the oldest spell in the "
                    "list. They have a class power - 'Double Revenge' - which allows "
                    "them to imitate spells at double damage or duration.";

        me.stats[A_STR] =  0;
        me.stats[A_INT] =  1;
        me.stats[A_WIS] = -1;
        me.stats[A_DEX] =  2;
        me.stats[A_CON] =  0;
        me.stats[A_CHR] = -1;
        me.base_skills = bs;
        me.extra_skills = xs;
        me.life = 108;
        me.base_hp = 10;
        me.exp = 110;
        me.pets = 20;
        
        me.get_powers = _get_powers;
        init = TRUE;
    }

    return &me;
}
