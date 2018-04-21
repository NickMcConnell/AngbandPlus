#include "angband.h"

#define MAX_SNIPE_POWERS 16

typedef struct snipe_power snipe_power;
struct snipe_power
{
    int     min_lev;
    int     mana_cost;
    const char *name;
};

static const char *snipe_tips[MAX_SNIPE_POWERS] =
{
    "Concentrate your mind for shooting.",
    "Shoot an arrow with brightness.",
    "Blink after shooting.",
    "Shoot an arrow able to shatter traps.",
    "Deals extra damege of fire.",
    "Shoot an arrow able to shatter rocks.",
    "Deals extra damege of ice.",
    "An arrow rushes away a target.",
    "An arrow pierces some monsters.",
    "Deals more damage to good monsters.",
    "Deals more damage to evil monsters.",
    "An arrow explodes when it hits a monster.",
    "Shoot arrows twice.",
    "Deals extra damage of lightning.",
    "Deals quick death or 1 damage.",
    "Deals great damage to all monsters, and some side effects to you.",
};

snipe_power snipe_powers[MAX_SNIPE_POWERS] =
{
   /*lvl, cst,  name */
    {  1,   0,  "Concentration" },
    {  2,   1,  "Flush Arrow" },
    {  3,   1,  "Shoot & Away" },
    {  5,   1,  "Disarm Shot" },
    {  8,   2,  "Fire Shot" },
    { 10,   2,  "Shatter Arrow" },
    { 13,   2,  "Ice Shot" },
    { 18,   2,  "Rushing Arrow" },
    { 22,   3,  "Piercing Shot" },
    { 25,   4,  "Evil Shot"},
    { 26,   4,  "Holy Shot" },
    { 30,   3,  "Missile"},
    { 32,   4,  "Double Shot" },
    { 36,   3,  "Plasma Bolt" },
    { 40,   3,  "Needle Shot" },
    { 48,   7,  "Saint Stars Arrow" },
};


static bool snipe_concentrate(void)
{
    if ((int)p_ptr->concent < (2 + (p_ptr->lev + 5) / 10)) p_ptr->concent++;

    msg_format("You concentrate deeply. (lvl %d)", p_ptr->concent);
    reset_concent = FALSE;

    p_ptr->update |= PU_BONUS;
    p_ptr->redraw |= PR_STATUS;
    p_ptr->update |= PU_MONSTERS;
    return TRUE;
}

void reset_concentration(bool msg)
{
    if (msg)
        msg_print("Stop concentrating.");

    p_ptr->concent = 0;
    reset_concent = FALSE;
    p_ptr->update |= PU_BONUS;
    p_ptr->redraw |= PR_STATUS;
    p_ptr->update |= PU_MONSTERS;
}

int boost_concentration_damage(int tdam)
{
    tdam *= (10 + p_ptr->concent);
    tdam /= 10;
    return tdam;
}

void display_snipe_list(void)
{
    int             i;
    int             y = 1;
    int             x = 1;
    int             plev = p_ptr->lev;
    snipe_power     spell;
    char            desc[80];

    prt("", y, x);
    put_str("Name", y, x + 5);
    put_str("Lv Mana", y, x + 35);

    for (i = 0; i < MAX_SNIPE_POWERS; i++)
    {
        spell = snipe_powers[i];
        if (spell.min_lev > plev) continue;
        if (spell.mana_cost > (int)p_ptr->concent) continue;
        sprintf(desc, "  %c) %-30s%2d %4d",
            I2A(i), spell.name, spell.min_lev, spell.mana_cost);
        Term_putstr(x, y + i + 1, -1, TERM_WHITE, desc);
    }
    return;
}

static int get_snipe_power(int *sn, bool only_browse)
{
    int             i;
    int             num = 0;
    int             y = 1;
    int             x = 20;
    int             plev = p_ptr->lev;
    int             ask;
    char            choice;
    char            out_val[160];
    cptr            p = "power";
    snipe_power     spell;
    bool            flag, redraw;

#ifdef ALLOW_REPEAT /* TNB */

    repeat_push(*sn);

    /* Assume cancelled */
    *sn = (-1);

    /* Repeat previous command */
    /* Get the spell, if available */
    if (repeat_pull(sn))
    {
        /* Verify the spell */
        if ((snipe_powers[*sn].min_lev <= plev) && (snipe_powers[*sn].mana_cost <= (int)p_ptr->concent))
        {
            /* Success */
            return (TRUE);
        }
    }

#endif /* ALLOW_REPEAT -- TNB */

    /* Nothing chosen yet */
    flag = FALSE;

    /* No redraw yet */
    redraw = FALSE;

    for (i = 0; i < MAX_SNIPE_POWERS; i++)
    {
        if ((snipe_powers[i].min_lev <= plev) &&
            ((only_browse) || (snipe_powers[i].mana_cost <= (int)p_ptr->concent)))
        {
            num = i;
        }
    }

    /* Build a prompt (accept all spells) */
    if (only_browse)
    {
        (void)strnfmt(out_val, 78, "(%^ss %c-%c, *=List, ESC=exit) Use which %s? ",
                  p, I2A(0), I2A(num), p);
    }
    else
    {
        (void)strnfmt(out_val, 78, "(%^ss %c-%c, *=List, ESC=exit) Use which %s? ",
              p, I2A(0), I2A(num), p);
    }

    /* Get a spell from the user */
    choice = always_show_list ? ESCAPE : 1;
    while (!flag)
    {
        if(choice == ESCAPE) choice = ' ';
        else if( !get_com(out_val, &choice, FALSE) )break; 

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
                if (!only_browse) screen_save();

                /* Display a list of spells */
                prt("", y, x);
                put_str("Name", y, x + 5);
                if (only_browse) put_str("Lv Pow", y, x + 35);

                /* Dump the spells */
                for (i = 0; i < MAX_SNIPE_POWERS; i++)
                {
                    Term_erase(x, y + i + 1, 255);

                    /* Access the spell */
                    spell = snipe_powers[i];
                    if (spell.min_lev > plev) continue;
                    if (!only_browse && (spell.mana_cost > (int)p_ptr->concent)) continue;

                    /* Dump the spell --(-- */
                    if (only_browse)
                        sprintf(psi_desc, "  %c) %-30s%2d %4d",
                            I2A(i), spell.name,    spell.min_lev, spell.mana_cost);
                    else
                        sprintf(psi_desc, "  %c) %-30s", I2A(i), spell.name);
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
                if (!only_browse) screen_load();
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
        if ((i < 0) || (i > num) || 
            (!only_browse &&(snipe_powers[i].mana_cost > (int)p_ptr->concent)))
        {
            bell();
            continue;
        }

        /* Save the spell index */
        spell = snipe_powers[i];

        /* Verify it */
        if (ask)
        {
            char tmp_val[160];

            /* Prompt */
            (void)strnfmt(tmp_val, 78, "Use %s? ", snipe_powers[i].name);

            /* Belay that order */
            if (!get_check(tmp_val)) continue;
        }

        /* Stop the loop */
        flag = TRUE;
    }

    /* Restore the screen */
    if (redraw && !only_browse) screen_load();

    /* Show choices */
    p_ptr->window |= (PW_SPELL);

    /* Window stuff */
    window_stuff();

    /* Abort if needed */
    if (!flag) return (FALSE);

    /* Save the choice */
    (*sn) = i;

#ifdef ALLOW_REPEAT /* TNB */

    repeat_push(*sn);

#endif /* ALLOW_REPEAT -- TNB */

    /* Success */
    return (TRUE);
}


int tot_dam_aux_snipe (int mult, monster_type *m_ptr)
{
    monster_race *r_ptr = &r_info[m_ptr->r_idx];

    switch (snipe_type)
    {
    case SP_LITE:
        if (r_ptr->flags3 & (RF3_HURT_LITE))
        {
            int n = 20 + p_ptr->concent;
            mon_lore_3(m_ptr, RF3_HURT_LITE);
            if (mult < n) mult = n;
        }
        break;
    case SP_FIRE:
        if (r_ptr->flagsr & RFR_EFF_IM_FIRE_MASK)
        {
            mon_lore_r(m_ptr, RFR_EFF_IM_FIRE_MASK);
        }
        else
        {
            int n = 15 + (p_ptr->concent * 3);
            if (mult < n) mult = n;
        }
        break;
    case SP_COLD:
        if (r_ptr->flagsr & RFR_EFF_IM_COLD_MASK)
        {
            mon_lore_r(m_ptr, RFR_EFF_IM_COLD_MASK);
        }
        else
        {
            int n = 15 + (p_ptr->concent * 3);
            if (mult < n) mult = n;
        }
        break;
    case SP_ELEC:
        if (r_ptr->flagsr & RFR_EFF_IM_ELEC_MASK)
        {
            mon_lore_r(m_ptr, RFR_EFF_IM_ELEC_MASK);
        }
        else
        {
            int n = 18 + (p_ptr->concent * 4);
            if (mult < n) mult = n;
        }
        break;
    case SP_KILL_WALL:
        if (r_ptr->flags3 & RF3_HURT_ROCK)
        {
            int n = 15 + (p_ptr->concent * 2);
            mon_lore_3(m_ptr, RF3_HURT_ROCK);
            if (mult < n) mult = n;
        }
        else if (r_ptr->flags3 & RF3_NONLIVING)
        {
            int n = 15 + (p_ptr->concent * 2);
            mon_lore_3(m_ptr, RF3_NONLIVING);
            if (mult < n) mult = n;
        }
        break;
    case SP_EVILNESS:
        if (r_ptr->flags3 & RF3_GOOD)
        {
            int n = 15 + (p_ptr->concent * 4);
            mon_lore_3(m_ptr, RF3_GOOD);
            if (mult < n) mult = n;
        }
        break;
    case SP_HOLYNESS:
        if (r_ptr->flags3 & RF3_EVIL)
        {
            int n = 12 + (p_ptr->concent * 3);
            mon_lore_3(m_ptr, RF3_EVIL);
            if (r_ptr->flags3 & (RF3_HURT_LITE))
            {
                n += (p_ptr->concent * 3);
                mon_lore_3(m_ptr, RF3_HURT_LITE);
            }
            if (mult < n) mult = n;
        }
        break;
    case SP_FINAL:
        if (mult < 50) mult = 50;
        break;
    }

    return (mult);
}

static bool cast_sniper_spell(int spell)
{
    if (!equip_find_object(TV_BOW, SV_ANY))
    {
        msg_print("You wield no bow!");
        return (FALSE);
    }

    /* spell code */
    switch (spell)
    {
    case 0: /* Concentration */
        if (!snipe_concentrate()) return (FALSE);
        energy_use = 100;
        return (TRUE);
    case 1: snipe_type = SP_LITE; break;
    case 2: snipe_type = SP_AWAY; break;
    case 3: snipe_type = SP_KILL_TRAP; break;
    case 4: snipe_type = SP_FIRE; break;
    case 5: snipe_type = SP_KILL_WALL; break;
    case 6: snipe_type = SP_COLD; break;
    case 7: snipe_type = SP_RUSH; break;
    case 8: snipe_type = SP_PIERCE; break;
    case 9: snipe_type = SP_EVILNESS; break;
    case 10: snipe_type = SP_HOLYNESS; break;
    case 11: snipe_type = SP_EXPLODE; break;
    case 12: snipe_type = SP_DOUBLE; break;
    case 13: snipe_type = SP_ELEC; break;
    case 14: snipe_type = SP_NEEDLE; break;
    case 15: snipe_type = SP_FINAL; break;
    default:
        msg_print("Zap?");
    }

    command_cmd = 'f';
    do_cmd_fire();
    snipe_type = 0;
    return is_fired;
}


void do_cmd_snipe(void)
{
    int  n = 0;
    bool cast;

    if (p_ptr->confused)
    {
        msg_print("You are too confused!");
        return;
    }
    if (p_ptr->image)
    {
        msg_print("You are hallucinating!");
        return;
    }
    if (p_ptr->stun)
    {
        msg_print("You are too stuned!");
        return;
    }

    if (!get_snipe_power(&n, FALSE)) return;
    sound(SOUND_SHOOT);
    cast = cast_sniper_spell(n);
    if (!cast) return;

    p_ptr->redraw |= (PR_HP | PR_MANA);
    p_ptr->window |= (PW_SPELL);
}

void do_cmd_snipe_browse(void)
{
    int n = 0;
    int j, line;
    char temp[62 * 4];

    screen_save();

    while(1)
    {
        /* get power */
        if (!get_snipe_power(&n, TRUE))
        {
            screen_load();
            return;
        }

        /* Clear lines, position cursor  (really should use strlen here) */
        Term_erase(12, 22, 255);
        Term_erase(12, 21, 255);
        Term_erase(12, 20, 255);
        Term_erase(12, 19, 255);
        Term_erase(12, 18, 255);

        roff_to_buf(snipe_tips[n], 62, temp, sizeof(temp));
        for(j = 0, line = 19; temp[j]; j += (1 + strlen(&temp[j])))
        {
            prt(&temp[j], line, 15);
            line++;
        }
    }
}

static void _calc_shooter_bonuses(object_type *o_ptr, shooter_info_t *info_ptr)
{
    if (info_ptr->tval_ammo == TV_BOLT)
    {
        info_ptr->to_h += 10 + p_ptr->lev/5;
        info_ptr->dis_to_h += 10 + p_ptr->lev/5;
    }
}

static int _get_powers(spell_info* spells, int max)
{
    int ct = 0;

    spell_info* spell = &spells[ct++];
    spell->level = 15;
    spell->cost = 20;
    spell->fail = calculate_fail_rate(spell->level, 80, p_ptr->stat_ind[A_INT]);
    spell->fn = probing_spell;

    return ct;
}

class_t *sniper_get_class(void)
{
    static class_t me = {0};
    static bool init = FALSE;

    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 25,  24,  28,   5,  32,  18,  35,  72};
    skills_t xs = { 12,  10,  10,   0,   0,   0,  12,  28};

        me.name = "Sniper";
        me.desc = "Snipers are specialists in marksmanship, but not like archers who "
                    "fire off arrow after arrow in swift succession. They don't just "
                    "increase accuracy and power of shots by concentration, they can use "
                    "fearsome archery techniques.\n \n"
                    "What they require is powerful bows or crossbows, good quality "
                    "ammunition and the fortitude to bear up without flinching under " 
                    "any situation.\n \n"
                    "Snipers know their enemies well and can shoot them from the shadows. "
                    "They have no time for magic.";

        me.stats[A_STR] =  2;
        me.stats[A_INT] = -1;
        me.stats[A_WIS] = -1;
        me.stats[A_DEX] =  2;
        me.stats[A_CON] =  1;
        me.stats[A_CHR] =  0;
        me.base_skills = bs;
        me.extra_skills = xs;
        me.life = 100;
        me.base_hp = 4;
        me.exp = 110;
        me.pets = 40;
        
        me.calc_shooter_bonuses = _calc_shooter_bonuses;
        me.get_powers = _get_powers;
        init = TRUE;
    }

    return &me;
}
