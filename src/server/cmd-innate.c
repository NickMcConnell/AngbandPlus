/*
 * File: cmd-innate.c
 * Purpose: Innate casting
 *
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
 

#include "s-angband.h"
#include "cmds.h"
#include "monster/mon-spell.h"
#include "s-spells.h"


/*
 * Use a ghostly ability.
 */
void do_cmd_ghost(int Ind, int ability, int dir)
{
    player_type *p_ptr = player_get(Ind);
    magic_type *s_ptr;
    int plev = p_ptr->lev;
    const char *pself = player_self(p_ptr);
    int j;

    /* Check for ghost-ness */
    if (!p_ptr->ghost || player_can_undead(p_ptr)) return;

    /* Not when confused */
    if (p_ptr->timed[TMD_CONFUSED])
    {
        msg(p_ptr, "You are too confused!");
        return;
    }

    /* Set the ability number */
    if (ability < PY_MAX_SPELLS)
        j = ability;
    else
    {
        j = ability - PY_MAX_SPELLS;

        /* Projected spells */
        if (!ghost_spell_projs[j])
        {
            msg(p_ptr, "You cannot project that ability.");
            return;
        }
    }

    /* Access the ability */
    s_ptr = &ghost_spells[j];

    /* Check for level */
    if (s_ptr->slevel > plev)
    {
        /* Message */
        msg(p_ptr, "You aren't powerful enough to use that ability.");
        return;
    }

    /* Cast the spell */
    if (!cast_ghost_spell(p_ptr, ability, dir)) return;

    /* Take a turn */
    use_energy(Ind);

    /* Too much can kill you */
    strnfmt(p_ptr->died_flavor, sizeof(p_ptr->died_flavor),
        "exhausted %s with ghostly spells", pself);
    if (p_ptr->exp < s_ptr->slevel * s_ptr->smana)
        take_hit(p_ptr, 5000, "the strain of ghostly powers", FALSE);

    /* Take some experience */
    player_exp_lose(p_ptr, s_ptr->slevel * s_ptr->smana, TRUE);
}


/*
 * Cast a breath attack
 *
 * Can be "cancelled" at the "Direction?" prompt for free.
 */
void do_cmd_breath(int Ind, int dir)
{
    player_type *p_ptr = player_get(Ind);
    bitflag mon_breath[RSF_SIZE];

    rsf_wipe(mon_breath);

    /* Restrict ghosts */
    if (p_ptr->ghost && !(p_ptr->dm_flags & DM_GHOST_BODY))
    {
        msg(p_ptr, "You need a tangible body to breathe!");
        return;
    }

    /* Take a turn */
    use_energy(Ind);

    /* Apply confusion */
    player_confuse_dir(p_ptr, &dir);

    /* Handle polymorphed players */
    if (p_ptr->r_idx)
    {
        monster_race *r_ptr = &r_info[p_ptr->r_idx];

        /* Hack -- Require correct "breath attack" */
        rsf_copy(mon_breath, r_ptr->spell_flags);
        set_spells(mon_breath, RST_BREATH);
    }

    /* Cast the breath attack */
    fire_breath(p_ptr, mon_breath, dir, 0);
}


/*
 * Cast a mimic spell
 */
void do_cmd_mimic(int Ind, int page, int spell, int dir)
{
    player_type *p_ptr = player_get(Ind);
    monster_race *r_ptr = &r_info[p_ptr->r_idx];
    magic_type *s_ptr;
    int i, j = 0, k = 0, chance;
    int flag, plev = p_ptr->lev;
    int old_num = get_player_num(p_ptr);

    /* Restrict ghosts */
    if (p_ptr->ghost && !is_dm_p(p_ptr))
    {
        msg(p_ptr, "You cannot cast monster spells!");
        return;
    }

    /* Not when confused */
    if (p_ptr->timed[TMD_CONFUSED])
    {
        msg(p_ptr, "You are too confused!");
        return;
    }

    /* Check each spell */
    for (i = 0; i < MIMIC_SPELLS; i++)
    {
        /* Access the spell */
        s_ptr = &mimic_spells[i];

        /* Access the spell flag */
        flag = s_ptr->sexp;

        /* Check spell availability */
        if (!rsf_has(r_ptr->spell_flags, flag)) continue;

        /* Did we find it? */
        if (page == -1)
        {
            /* Normal spell */
            if (flag == spell) break;

            /* Projected spell */
            if (flag == 0 - spell)
            {
                if (!mimic_spell_projs[i])
                {
                    msg(p_ptr, "You cannot project that spell.");
                    return;
                }
                flag = 0 - flag;
                break;
            }
        }
        if (page == k)
        {
            /* Normal spell */
            if (j == spell) break;

            /* Projected spell */
            if (j == spell - PY_MAX_SPELLS)
            {
                if (!mimic_spell_projs[i])
                {
                    msg(p_ptr, "You cannot project that spell.");
                    return;
                }
                flag = 0 - flag;
                break;
            }
        }

        /* Next spell */
        j++;
        if (j == SPELLS_PER_BOOK)
        {
            j = 0;
            k++;
        }
    }

    /* Paranoia */
    if (i == MIMIC_SPELLS) return;

    /* Check mana */
    if (s_ptr->smana > p_ptr->csp)
    {
        msg(p_ptr, "You do not have enough mana.");
        return;
    }

    /* Antimagic field (no effect on spells that are not "magical") */
    if ((s_ptr->smana > 0) && check_antimagic(p_ptr, NULL)) return;

    /* Spell cost */
    p_ptr->spell_cost = s_ptr->smana;

    /* Spell failure chance */
    chance = s_ptr->sfail;

    /* Failed spell */
    if (magik(chance))
        msg(p_ptr, "You failed to get the spell off!");

    /* Process spell */
    else
    {
        /* Cast the spell */
        if (!cast_mimic_spell(p_ptr, flag, dir)) return;
    }

    /* Take a turn */
    use_energy(Ind);

    /* Use some mana */
    p_ptr->csp -= p_ptr->spell_cost;

    /* Hack -- Redraw picture */
    redraw_picture(p_ptr, old_num);

    /* Redraw mana */
    p_ptr->redraw |= (PR_MANA);
}
