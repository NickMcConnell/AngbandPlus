/* File: racial.c */

/* Purpose: Racial powers */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"



/*
 * Returns the chance to activate a racial power/mutation
 */
static int racial_chance(s16b min_level, int use_stat, int difficulty)
{
    int i;
    int val;
    int sum = 0;
    int stat = p_ptr->stat_cur[use_stat];

    /* No chance for success */
    if ((p_ptr->lev < min_level) || p_ptr->confused)
    {
        return (0);
    }

    /* Calculate difficulty */
    if (p_ptr->stun)
    {
        difficulty += p_ptr->stun;
    }
    else if (p_ptr->lev > min_level)
    {
        int lev_adj = ((p_ptr->lev - min_level) / 3);
        if (lev_adj > 10) lev_adj = 10;
        difficulty -= lev_adj;
    }

    if (difficulty < 5) difficulty = 5;

    /* We only need halfs of the difficulty */
    difficulty = difficulty / 2;

    for (i = 1; i <= stat; i++)
    {
        val = i - difficulty;
        if (val > 0)
            sum += (val <= difficulty) ? val : difficulty;
    }

    if (difficulty == 0)
        return (100);
    else
        return (((sum * 100) / difficulty) / stat);
}


/* Note: return value indicates that we have succesfully used the power */

bool racial_aux(s16b min_level, int cost, int use_stat, int difficulty)
{
    bool use_hp = FALSE;

    /* Not enough mana - use hp */
    if (p_ptr->csp < cost) use_hp = TRUE;

    /* Always use hps with dragons */
    if (p_ptr->prace >= RACE_MIN_DRAGON) use_hp = TRUE;

    /* Power is not available yet */
    if (p_ptr->lev < min_level)
    {
        msg_format("You need to attain level %d to use this power.", min_level);
                p_ptr->energy_use = 0;
        return FALSE;
    }

    /* Too confused */
    else if (p_ptr->confused)
    {
        msg_print("You are too confused to use this power.");
                p_ptr->energy_use = 0;
        return FALSE;
    }

    /* Risk death? */
    else if (use_hp && (p_ptr->chp < cost))
    {
        if (!get_check("Really use the power in your weakened state? "))
        {
                        p_ptr->energy_use = 0;
            return FALSE;
        }
    }

    /* Else attempt to do it! */

    if (p_ptr->stun)
    {
        difficulty += p_ptr->stun;
    }
    else if (p_ptr->lev > min_level)
    {
        int lev_adj = ((p_ptr->lev - min_level) / 3);
        if (lev_adj > 10) lev_adj = 10;
        difficulty -= lev_adj;
    }

    if (difficulty < 5) difficulty = 5;

    /* take time and pay the price */
        p_ptr->energy_use = 100;

    if (use_hp)
    {
        take_hit((cost / 2) + randint(cost / 2),
            "concentrating too hard");
    }
    else
    {
        p_ptr->csp -= (cost / 2) + randint(cost / 2);
    }


    /* Redraw mana and hp */
    p_ptr->redraw |= (PR_HP | PR_MANA);

    /* Success? */
    if (randint(p_ptr->stat_cur[use_stat]) >=
        ((difficulty / 2) + randint(difficulty / 2)))
    {
        return TRUE;
    }

    msg_print("You've failed to concentrate hard enough.");
    return FALSE;
}


static void cmd_racial_power_aux(s32b command)
{
    s16b        plev = p_ptr->lev;
    int         dir = 0;


    switch (p_ptr->prace)
    {
        case RACE_YEEK:
        {
            if (racial_aux(1, 2, A_CON, 5))
            {
                msg_print("You flee in terror!");

                set_afraid(randint(10)+5);
                set_fast(randint(5)+5);
            }
            break;
        }

        case RACE_PIXIE:
        {
            if (command == -2)
            {
                if (racial_aux(10, 3, A_INT, 10))
                {
                    msg_format("You concentrate and blink away!");
                    teleport_player(5);
                }
            }
            else if (command == -1)
            {
                if (racial_aux(4, 5, A_CHR, 10))
                {
                    if (!get_aim_dir(&dir)) break;
                    msg_format("You throw pixie dust!");
                        fire_bolt(GF_CONFUSION, dir, 1);
                }
            }
            break;
        }


        case RACE_CRYSTALDRAG:
        {
            if (command == -2)
            {
                if (racial_aux(25, ((3*plev/2)>4 ? (3*plev/2) : 4), A_CON, 6))
                {
                    if (!get_aim_dir(&dir)) break;
                    msg_format("You breathe shards!");
                            fire_arc(GF_SHARD, dir, p_ptr->chp/3, (plev / 10) + 10, 45);
                }
            }
            else if (command == -1)
            {
                if (racial_aux(5, ((2*plev/3)>4 ? (2*plev/3) : 4), A_CON, 3))
                {
                    if (!get_aim_dir(&dir)) break;
                    msg_format("You cast a ray of shards!");
                                        fire_beam(GF_SHARD, dir, p_ptr->chp/4);
                }
            }
            break;
        }

        case RACE_COPPERDRAG:
        {
            if (command == -2)
            {
                if (racial_aux(25, ((3*plev/2)>4 ? (3*plev/2) : 4), A_CON, 6))
                {
                    if (!get_aim_dir(&dir)) break;
                    msg_format("You breathe disenchantment!");
                            fire_arc(GF_DISENCHANT, dir, p_ptr->chp/3, (plev / 10) + 10, 45);
                }
            }
            else if (command == -1)
            {
                if (racial_aux(5, ((2*plev/3)>4 ? (2*plev/3) : 4), A_CON, 3))
                {
                    if (!get_aim_dir(&dir)) break;
                    msg_format("You emit a disenchantment ray!");
                                        fire_beam(GF_DISENCHANT, dir, p_ptr->chp/4);
                }
            }
            break;
        }

        case RACE_BRONZEDRAG:
        {
            if (command == -2)
            {
                if (racial_aux(25, ((3*plev/2)>4 ? (3*plev/2) : 4), A_CON, 6))
                {
                    if (!get_aim_dir(&dir)) break;
                    msg_format("You breathe confusion!");
                            fire_arc(GF_CONFUSION, dir, p_ptr->chp/3, (plev / 10) + 10, 45);
                }
            }
            else if (command == -1)
            {
                if (racial_aux(5, ((2*plev/3)>4 ? (2*plev/3) : 4), A_CON, 3))
                {
                    if (!get_aim_dir(&dir)) break;
                    msg_format("You cast a confusion bolt!");
                                        fire_bolt(GF_CONFUSION, dir, p_ptr->chp/4);
                }
            }
            break;
        }

        case RACE_GOLDDRAG:
        {
            if (command == -2)
            {
                if (racial_aux(25, ((3*plev/2)>4 ? (3*plev/2) : 4), A_CON, 6))
                {
                    if (!get_aim_dir(&dir)) break;
                    msg_format("You breathe sound!");
                            fire_arc(GF_SOUND, dir, p_ptr->chp/3, (plev / 10) + 10, 45);
                }
            }
                        else if (command == -1)
            {
                if (racial_aux(5, ((2*plev/3)>4 ? (2*plev/3) : 4), A_CON, 3))
                {
                    if (!get_aim_dir(&dir)) break;
                    msg_format("You cast a stun bolt!");
                                        fire_bolt(GF_SOUND, dir, p_ptr->chp/4);
                }
            }
            break;
        }

        case RACE_PSEUDODRAG:
        {
            if (command == -2)
            {
                if (racial_aux(25, ((3*plev/2)>4 ? (3*plev/2) : 4), A_CON, 6))
                {
                    msg_format("You shine with light!");
                            fire_sphere(GF_LITE, 0, p_ptr->chp/3, (plev / 5) + 1, 0);
                }
            }
                        else if (command == -1)
            {
                if (racial_aux(5, ((2*plev/3)>4 ? (2*plev/3) : 4), A_CON, 3))
                {
                    if (!get_aim_dir(&dir)) break;
                    msg_format("You cast ray of light!");
                        fire_beam(GF_LITE, dir, p_ptr->chp/4);
                }
            }
            break;
        }

        case RACE_MULTIHUEDDRAG:
        {
            if (command == -5)
            {
                if (racial_aux(45, ((plev*3/2)>4 ? (plev*3/2) : 4), A_CON, 30))
                {
                    if (!get_aim_dir(&dir)) break;
                    msg_format("You breathe poison!");
                            fire_arc(GF_POIS, dir, (p_ptr->chp*10)/36, (plev / 10) + 10, 45);
                }
            }

            if (command == -4)
            {
                if (racial_aux(35, (plev>4 ? plev : 4), A_CON, 20))
                {
                    if (!get_aim_dir(&dir)) break;
                    msg_format("You breathe fire!");
                            fire_arc(GF_FIRE, dir, (p_ptr->chp*10)/30, (plev / 10) + 10, 45);
                }
            }

            else if (command == -3)
            {
                if (racial_aux(25, ((plev*2/3)>4 ? (plev*2/3) : 4), A_CON, 12))
                {
                    if (!get_aim_dir(&dir)) break;
                    msg_format("You breathe ice!");
                            fire_arc(GF_ICE, dir, (p_ptr->chp*10)/35, (plev / 10) + 10, 45);
                }
            }

            else if (command == -2)
            {
                if (racial_aux(15, ((plev/2)>4 ? (plev/2) : 4), A_CON, 6))
                {
                    if (!get_aim_dir(&dir)) break;
                    msg_format("You breathe electricity!");
                            fire_arc(GF_ELEC, dir, (p_ptr->chp*10)/40, (plev / 10) + 10, 45);
                }
            }

            else if (command == -1)
            {
                if (racial_aux(5, ((plev/3)>4 ? (plev/3) : 4), A_CON, 3))
                {
                    if (!get_aim_dir(&dir)) break;
                    msg_format("You breathe acid!");
                            fire_arc(GF_ACID, dir, (p_ptr->chp*10)/45, (plev / 10) + 10, 45);
                }
            }
            break;
        }

        default:
            msg_print("This race has no bonus power.");
                        p_ptr->energy_use = 0;
    }

    /* Redraw mana and hp */
    p_ptr->redraw |= (PR_HP | PR_MANA);
}


typedef struct power_desc_type power_desc_type;

struct power_desc_type
{
    char name[40];
    int  level;
    int  cost;
    int  fail;
    int  number;
    int  range;
};


/*
 * Allow user to choose a power (racial / mutation) to activate
 */
void do_cmd_racial_power(void)
{
    power_desc_type power_desc[36];
    int             num, ask, i = 0;
    int             lvl = p_ptr->lev;
    bool            flag, redraw;
    bool            has_racial = FALSE;
    char            choice;
    char            out_val[160];
    s16b            plev = p_ptr->lev;


    for (num = 0; num < 36; num++)
    {
        strcpy(power_desc[num].name, "");
        power_desc[num].number = 0;
    }

    num = 0;

    if (p_ptr->confused)
    {
        msg_print("You are too confused to use any powers!");
                p_ptr->energy_use = 0;
        return;
    }

    switch (p_ptr->prace)
    {
        /* Note that the dragon powers cost whatever is larger,
         * 4 mana or something dependent on player level. */
        case RACE_YEEK:
            sprintf(power_desc[0].name, "flee in terror!");
            power_desc[0].level = 1;
            power_desc[0].cost = 2;
            power_desc[0].fail = 100 - racial_chance(1, A_CON, 5);
            power_desc[0].range = 5;
            has_racial = TRUE;
            break;
        case RACE_PIXIE:
            sprintf(power_desc[0].name, "throw pixie dust");
            power_desc[0].level = 4;
            power_desc[0].cost = 5;
            power_desc[0].fail = 100 - racial_chance(4, A_CHR, 10);
            power_desc[0].range = MAX_RANGE;

            sprintf(power_desc[1].name, "phase door");
            power_desc[1].level = 10;
            power_desc[1].cost = 3;
            power_desc[1].fail = 100 - racial_chance(10, A_INT, 10);
            power_desc[1].number = -2;
            power_desc[1].range = 5;
            num++;
            has_racial = TRUE;
            break;
        case RACE_CRYSTALDRAG:
            sprintf(power_desc[0].name, "shard bolt (dam %d)", p_ptr->chp / 4);
            power_desc[0].level = 5;
            power_desc[0].cost = ((2*lvl/3) > 4 ? (2*lvl/3) : 4);
            power_desc[0].fail = 100 - racial_chance(5, A_CON, 3);
            power_desc[0].range = MAX_RANGE;

            sprintf(power_desc[1].name, "breathe shards (dam %d)", p_ptr->chp / 3);
            power_desc[1].level = 25;
            power_desc[1].cost = ((3*lvl/2) > 4 ? (3*lvl/2) : 4);
            power_desc[1].fail = 100 - racial_chance(25, A_CON, 6);
            power_desc[1].number = -2;
            power_desc[1].range = (plev / 10) + 10;

            num++;
            has_racial = TRUE;
            break;
        case RACE_COPPERDRAG:
            sprintf(power_desc[0].name, "disenc ray (dam %d)", p_ptr->chp / 4);
            power_desc[0].level = 5;
            power_desc[0].cost = ((2*lvl/3) > 4 ? (2*lvl/3) : 4);
            power_desc[0].fail = 100 - racial_chance(5, A_CON, 3);
            power_desc[0].range = MAX_RANGE;

            sprintf(power_desc[1].name, "breathe disenc (dam %d)", p_ptr->chp / 3);
            power_desc[1].level = 25;
            power_desc[1].cost = ((3*lvl/2) > 4 ? (3*lvl/2) : 4);
            power_desc[1].fail = 100 - racial_chance(25, A_CON, 6);
            power_desc[1].range = (plev / 10) + 10;
            power_desc[1].number = -2;
            num++;
            has_racial = TRUE;
            break;
        case RACE_BRONZEDRAG:
            sprintf(power_desc[0].name, "confusion bolt (dam %d)", p_ptr->chp / 4);
            power_desc[0].level = 5;
            power_desc[0].cost = ((2*lvl/3) > 4 ? (2*lvl/3) : 4);
            power_desc[0].fail = 100 - racial_chance(5, A_CON, 3);
            power_desc[0].range = MAX_RANGE;

            sprintf(power_desc[1].name, "breathe conf (dam %d)", p_ptr->chp / 3);
            power_desc[1].level = 25;
            power_desc[1].cost = ((3*lvl/2) > 4 ? (3*lvl/2) : 4);
            power_desc[1].fail = 100 - racial_chance(25, A_CON, 6);
            power_desc[1].number = -2;
            power_desc[1].range = (plev / 10) + 10;
            num++;
            has_racial = TRUE;
            break;
        case RACE_GOLDDRAG:
            sprintf(power_desc[0].name, "sonic bolt (dam %d)", p_ptr->chp / 4);
            power_desc[0].level = 5;
            power_desc[0].cost = ((2*lvl/3) > 4 ? (2*lvl/3) : 4);
            power_desc[0].fail = 100 - racial_chance(5, A_CON, 3);
            power_desc[0].range = MAX_RANGE;

            sprintf(power_desc[1].name, "sonic storm (dam %d)", p_ptr->chp / 3);
            power_desc[1].level = 25;
            power_desc[1].cost = ((3*lvl/2) > 4 ? (3*lvl/2) : 4);
            power_desc[1].fail = 100 - racial_chance(25, A_CON, 6);
            power_desc[1].number = -2;
            power_desc[1].range = (plev / 10) + 10;
            num++;
            has_racial = TRUE;
            break;
        case RACE_PSEUDODRAG:
            sprintf(power_desc[0].name, "light ray (dam %d)", p_ptr->chp / 4);
            power_desc[0].level = 5;
            power_desc[0].cost = ((2*lvl/3) > 4 ? (2*lvl/3) : 4);
            power_desc[0].fail = 100 - racial_chance(5, A_CON, 3);
            power_desc[0].range = MAX_RANGE;

            sprintf(power_desc[1].name, "light blast (dam %d)", p_ptr->chp / 3);
            power_desc[1].level = 25;
            power_desc[1].cost = ((3*lvl/2) > 4 ? (3*lvl/2) : 4);
            power_desc[1].fail = 100 - racial_chance(25, A_CON, 6);
            power_desc[1].number = -2;
            power_desc[1].range = (plev / 5) + 1;
            num++;
            has_racial = TRUE;
            break;
        case RACE_MULTIHUEDDRAG:
            sprintf(power_desc[0].name, "breathe acid (dam %d)", (p_ptr->chp*10)/45);
            power_desc[0].level = 5;
            power_desc[0].cost = ((lvl/3) > 4 ? (lvl/3) : 4);
            power_desc[0].fail = 100 - racial_chance(5, A_CON, 3);
            power_desc[0].range = (plev / 10) + 10;

            sprintf(power_desc[1].name, "breathe elec (dam %d)", (p_ptr->chp*10)/40);
            power_desc[1].level = 15;
            power_desc[1].cost = ((lvl/2) > 4 ? (lvl/2) : 4);
            power_desc[1].fail = 100 - racial_chance(15, A_CON, 6);
            power_desc[1].number = -2;
            power_desc[1].range = (plev / 10) + 10;
            num++;

            sprintf(power_desc[2].name, "breathe ice (dam %d)", (p_ptr->chp*10)/35);
            power_desc[2].level = 25;
            power_desc[2].cost = ((lvl*2/3) > 4 ? (lvl*2/3) : 4);
            power_desc[2].fail = 100 - racial_chance(25, A_CON, 12);
            power_desc[2].number = -3;
            power_desc[2].range = (plev / 10) + 10;
            num++;

            sprintf(power_desc[3].name, "breathe fire (dam %d)", (p_ptr->chp*10)/30);
            power_desc[3].level = 35;
            power_desc[3].cost = (lvl > 4 ? lvl : 4);
            power_desc[3].cost = lvl;
            power_desc[3].fail = 100 - racial_chance(35, A_CON, 20);
            power_desc[3].number = -4;
            power_desc[3].range = (plev / 10) + 10;
            num++;

            sprintf(power_desc[4].name, "breathe poison (dam %d)", (p_ptr->chp*10)/36);
            power_desc[4].level = 45;
            power_desc[4].cost = (3*lvl/2 > 4 ? 3*lvl/2 : 4);
            power_desc[4].fail = 100 - racial_chance(45, A_CON, 30);
            power_desc[4].number = -5;
            power_desc[4].range = (plev / 10) + 10;
            num++;
            has_racial = TRUE;
            break;

        default:
            strcpy(power_desc[0].name, "(none)");
    }

        if (!has_racial)
    {
        msg_print("You have no powers to activate.");
                p_ptr->energy_use = 0;
        return;
    }

    if (has_racial)
    {
        power_desc[0].number = -1;
        num++;
    }

    /* Nothing chosen yet */
    flag = FALSE;

    /* No redraw yet */
    redraw = FALSE;

    /* Build a prompt */
    (void)strnfmt(out_val, 78, "(Powers %c-%c, *=List, ESC=exit) Use which power? ",
        I2A(0), (num <= 26) ? I2A(num - 1) : '0' + num - 27);

    /* Get a spell from the user */
    while (!flag && get_com(out_val, &choice))
    {
        /* Request redraw */
        if ((choice == ' ') || (choice == '*') || (choice == '?'))
        {
            /* Show the list */
            if (!redraw)
            {
                byte y = 1, x = 0;
                int ctr = 0;
                char dummy[80];
                char letter;
                int x1, y1;

                strcpy(dummy, "");

                /* Show list */
                redraw = TRUE;

                /* Save the screen */
                screen_save();

                /* Print header(s) */
                if (num < 17)
                    prt("                            Lv Cost Fail Range", y++, x);
                else
                    prt("                            Lv Cost Fail Range                       Lv Cost Fail Range", y++, x);

                /* Print list */
                while (ctr < num)
                {
                    /* letter/number for power selection */
                    if (ctr < 26)
                        letter = I2A(ctr);
                    else
                        letter = '0' + ctr - 26;
                    x1 = ((ctr < 17) ? x : x + 40);
                    y1 = ((ctr < 17) ? y + ctr : y + ctr - 17);

                    sprintf(dummy, " %c) %-23.23s %2d %4d %3d%% %2d", letter, power_desc[ctr].name, power_desc[ctr].level, power_desc[ctr].cost, power_desc[ctr].fail, power_desc[ctr].range);
                    prt(dummy, y1, x1);
                    ctr++;
                }
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

        if (choice == '\r' && num == 1)
        {
            choice = 'a';
        }

        if (isalpha(choice))
        {
            /* Note verify */
            ask = (isupper(choice));

            /* Lowercase */
            if (ask) choice = tolower(choice);

            /* Extract request */
            i = (islower(choice) ? A2I(choice) : -1);
        }
        else
        {
            ask = FALSE; /* Can't uppercase digits */

            i = choice - '0' + 26;
        }

        /* Totally Illegal */
        if ((i < 0) || (i >= num))
        {
                        bell("");
            continue;
        }

        /* Verify it */
        if (ask)
        {
            char tmp_val[160];

            /* Prompt */
            (void)strnfmt(tmp_val, 78, "Use %s? ", power_desc[i].name);

            /* Belay that order */
            if (!get_check(tmp_val)) continue;
        }

        /* Stop the loop */
        flag = TRUE;
    }

    /* Restore the screen */
    if (redraw) screen_load();

    /* Abort if needed */
    if (!flag)
    {
                p_ptr->energy_use = 0;
        return;
    }

    if (power_desc[i].number < 0)
    {
        cmd_racial_power_aux(power_desc[i].number);
    }

        /* Success */
    return;
}
