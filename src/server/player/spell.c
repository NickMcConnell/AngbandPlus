/*
 * File: spell.c
 * Purpose: Spell and prayer casting/praying
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * Copyright (c) 2012 MAngband and PWMAngband Developers
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */
 

#include "../s-angband.h"
#include "../monster/mon-spell.h"
#include "../monster/mon-util.h"
#include "../netserver.h"
#include "../s-spells.h"


void cast_spell_end(int Ind)
{
    player_type *p_ptr = player_get(Ind);
    int j = p_ptr->current_spell;
    const magic_type *s_ptr;

    /* Affect other spell */
    if (j >= PY_MAX_SPELLS) j -= PY_MAX_SPELLS;

    /* Access the spell */
    s_ptr = &p_ptr->clazz->spells.info[j];

    /* A spell was cast */
    if (!(p_ptr->spell_flags[j] & PY_SPELL_WORKED))
    {
        int e = s_ptr->sexp;

        /* The spell worked */
        p_ptr->spell_flags[j] |= PY_SPELL_WORKED;

        /* Gain experience */
        player_exp_gain(p_ptr, e * s_ptr->slevel);

        /* Redraw */
        p_ptr->redraw |= (PR_SPELL);
    }
}


/*
 * Send the ghost spell info to the client.
 */
void show_ghost_spells(struct player *p)
{
    magic_type *s_ptr;
    int i;
    char out_val[NORMAL_WID];
    char out_desc[MSG_LEN];
    byte line_attr;
    char help[20];
    const char *comment = help;
    spell_flags flags;

    flags.line_attr = TERM_WHITE;
    flags.flag = RSF_NONE;
    flags.dir_attr = 0;
    flags.proj_attr = 0;

    /* Wipe the spell array */
    Send_spell_info(p, 0, 0, "", &flags);

    /* Check each spell */
    for (i = 0; i < GHOST_SPELLS; i++)
    {
        /* Access the spell */
        s_ptr = &ghost_spells[i];

        /* Get extra info */
        get_ghost_spell_info(p, i, help, sizeof(help));

        /* Assume spell is known and tried */
        comment = help;
        line_attr = TERM_WHITE;

        /* Format information */
        strnfmt(out_val, sizeof(out_val), "%-30s%2d %4d %3d%%%s", ghost_spell_names[i],
            s_ptr->slevel, s_ptr->smana, 0, comment);
        my_strcpy(out_desc, ghost_spell_descs[i], sizeof(out_desc));

        flags.line_attr = line_attr;
        flags.flag = RSF_NONE;
        flags.dir_attr = ghost_spell_dirs[i];
        flags.proj_attr = ghost_spell_projs[i];

        /* Send it */
        Send_spell_info(p, 0, i, out_val, &flags);
        Send_spell_desc(p, 0, i, out_desc);
    }
}


/*
 * Check if the antimagic field around a player will disrupt the caster's spells.
 */
bool check_antimagic(struct player *p, struct monster *m)
{
    s16b id;
    int y, x, i, amchance, amrad, dist;

    /* The caster is a monster */
    if (m)
    {
        id = m->master;
        y = m->fy;
        x = m->fx;
    }

    /* The caster is the player */
    else
    {
        id = p->id;
        y = p->py;
        x = p->px;
    }

    /* Check each player */
    for (i = 1; i <= NumPlayers; i++)
    {
        player_type *q_ptr = player_get(i);
        object_type *o_ptr;
        bitflag f[OF_SIZE];

        /* Skip players not on this depth */
        if (q_ptr->depth != p->depth) continue;

        /* Compute the probability of an unbeliever to disrupt any magic attempts */
        if (player_has(q_ptr, PF_ANTIMAGIC))
        {
            amchance = q_ptr->lev;
            amrad = 1 + (q_ptr->lev / 10);
        }
        else
        {
            amchance = 0;
            amrad = 0;
        }

        /* Handle polymorphed players */
        if (q_ptr->r_idx)
        {
            monster_race *r_ptr = &r_info[q_ptr->r_idx];

            if (rf_has(r_ptr->flags, RF_ANTI_MAGIC))
            {
                amchance = r_ptr->level / 2;
                amrad = 1 + (r_ptr->level / 20);
            }
        }

        /* Dark swords can disrupt magic attempts too */
        o_ptr = &q_ptr->inventory[INVEN_WIELD];
        object_flags(o_ptr, f);
        if (of_has(f, OF_ANTI_MAGIC))
        {
            /* Enchantments lessen the antimagic field */
            int minus = o_ptr->to_h + o_ptr->to_d;

            if (minus < 0) minus = 0;
            if (minus > 50) minus = 50;

            /* Apply penalty */
            amchance = amchance + 50 - minus;
            amrad = amrad + 5 - (minus / 10);
        }

        /* Own antimagic field */
        if (p == q_ptr)
        {
            /* Antimagic field is capped at 95% */
            if (amchance > 95) amchance = 95;

            /* Check antimagic */
            if (magik(amchance))
            {
                msg(p, "Your anti-magic field disrupts your attempt.");
                return TRUE;
            }
        }

        /* Antimagic field from other players */
        else
        {
            /* Lower effect from party mates (greatly) */
            if (master_in_party(id, q_ptr->id)) amchance >>= 2;

            /* Antimagic field is capped at 95% */
            if (amchance > 95) amchance = 95;

            /* Compute distance */
            dist = distance(y, x, q_ptr->py, q_ptr->px);
            if (dist > amrad) amchance = 0;

            /* Check antimagic */
            if (magik(amchance))
            {
                if (m)
                {
                    char m_name[NORMAL_WID];

                    monster_desc(p, m_name, sizeof(m_name), m, MDESC_CAPITAL);
                    msg(p, "%s fails to cast a spell.", m_name);
                }
                else
                {
                    if (p->play_vis[i])
                        msg(p, "%s's anti-magic field disrupts your attempt.", q_ptr->name);
                    else
                        msg(p, "An anti-magic field disrupts your attempt.");
                }
                return TRUE;
            }
        }
    }

    /* Check each monster */
    for (i = 1; i < cave_monster_max(cave_get(p->depth)); i++)
    {
        monster_type *m_ptr = cave_monster(cave_get(p->depth), i);
        monster_lore *l_ptr;

        /* Paranoia -- Skip dead monsters */
        if (!m_ptr->r_idx) continue;

        /* Learn about antimagic field */
        l_ptr = &p->lore[m_ptr->r_idx];
        if (p->mon_vis[i] && !m) rf_on(l_ptr->flags, RF_ANTI_MAGIC);

        /* Skip monsters without antimagic field */
        if (!rf_has(r_info[m_ptr->r_idx].flags, RF_ANTI_MAGIC)) continue;

        /* Compute the probability of a monster to disrupt any magic attempts */
        amchance = 25 + m_ptr->level;
        amrad = 3 + (m_ptr->level / 10);

        /* Lower effect from party mates (greatly) */
        if (master_in_party(id, m_ptr->master)) amchance >>= 2;

        /* Antimagic field is capped at 95% */
        if (amchance > 95) amchance = 95;

        /* Compute distance */
        dist = distance(y, x, m_ptr->fy, m_ptr->fx);
        if (dist > amrad) amchance = 0;

        /* Check antimagic */
        if (magik(amchance))
        {
            if (m)
            {
                char m_name[NORMAL_WID];

                monster_desc(p, m_name, sizeof(m_name), m, MDESC_CAPITAL);
                msg(p, "%s fails to cast a spell.", m_name);
            }
            else
            {
                if (p->mon_vis[i])
                {
                    char m_name[NORMAL_WID];

                    monster_desc(p, m_name, sizeof(m_name), m_ptr, MDESC_CAPITAL);
                    msg(p, "%s's anti-magic field disrupts your attempt.", m_name);
                }
                else
                    msg(p, "An anti-magic field disrupts your attempt.");
            }

            return TRUE;
        }
    }

    /* Assume no antimagic */
    return FALSE;
}


/*
 * Check if the antisummon field around a player will disrupt the caster's summoning spells.
 * Note: antisummon field is perfect -- it will prevent 100% of summoning from friends or foes.
 */
bool check_antisummon(struct player *p, struct monster *m)
{
    int y, x, i;

    /* The caster is a monster */
    if (m)
    {
        y = m->fy;
        x = m->fx;
    }

    /* The caster is the player */
    else
    {
        y = p->py;
        x = p->px;
    }

    /* Check each player */
    for (i = 1; i <= NumPlayers; i++)
    {
        player_type *q_ptr = player_get(i);

        /* Skip players not on this depth */
        if (q_ptr->depth != p->depth) continue;

        /* No antisummon */
        if (!q_ptr->timed[TMD_ANTISUMMON]) continue;

        /* Own antisummon field */
        if (p == q_ptr)
        {
            msg(p, "Your anti-summon field disrupts your attempt.");
            return TRUE;
        }

        /* Antisummon field from other players */
        else
        {
            /* Compute distance */
            int dist = distance(y, x, q_ptr->py, q_ptr->px);

            /* Check antisummon */
            if (dist <= MAX_SIGHT)
            {
                if (m)
                {
                    char m_name[NORMAL_WID];

                    monster_desc(p, m_name, sizeof(m_name), m, MDESC_CAPITAL);
                    msg(p, "%s fails to cast a spell.", m_name);
                }
                else
                {
                    if (p->play_vis[i])
                        msg(p, "%s's anti-summon field disrupts your attempt.", q_ptr->name);
                    else
                        msg(p, "An anti-summon field disrupts your attempt.");
                }
                return TRUE;
            }
        }
    }

    /* Assume no antisummon */
    return FALSE;
}


/*
 * Send the mimic spell info to the client.
 */
void show_mimic_spells(struct player *p)
{
    monster_race *r_ptr = &r_info[p->r_idx];
    magic_type *s_ptr;
    int i, j = 0, k = 0;
    char out_val[NORMAL_WID];
    char out_desc[MSG_LEN];
    byte line_attr;
    char help[20];
    const char *comment = help;
    int flag;
    spell_flags flags;

    flags.line_attr = TERM_WHITE;
    flags.flag = RSF_NONE;
    flags.dir_attr = 0;
    flags.proj_attr = 0;

    /* Wipe the spell array */
    Send_spell_info(p, 0, 0, "", &flags);

    /* Check each spell */
    for (i = 0; i < MIMIC_SPELLS; i++)
    {
        /* Access the spell */
        s_ptr = &mimic_spells[i];

        /* Access the spell flag */
        flag = s_ptr->sexp;

        /* Check spell availability */
        if (!rsf_has(r_ptr->spell_flags, flag)) continue;

        /* Get extra info */
        get_mimic_spell_info(p, flag, help, sizeof(help));

        /* Assume spell is known and tried */
        comment = help;
        line_attr = TERM_WHITE;

        /* Format information */
        strnfmt(out_val, sizeof(out_val), "%-30s%2d %4d %3d%%%s", mimic_spell_names[i],
            0, s_ptr->smana, s_ptr->sfail, comment);
        strnfmt(out_desc, sizeof(out_desc), "%s (#%d).", mimic_spell_descs[i], flag);

        flags.line_attr = line_attr;
        flags.flag = flag;
        flags.dir_attr = mimic_spell_dirs[i];
        flags.proj_attr = mimic_spell_projs[i];

        /* Send it */
        Send_spell_info(p, k, j, out_val, &flags);
        Send_spell_desc(p, k, j, out_desc);

        /* Next spell */
        j++;
        if (j == SPELLS_PER_BOOK)
        {
            j = 0;
            k++;
        }
    }
}
