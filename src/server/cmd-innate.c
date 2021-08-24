/*
 * File: cmd-innate.c
 * Purpose: Innate casting
 *
 * Copyright (c) 2018 MAngband and PWMAngband Developers
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


/*
 * Use a ghostly ability.
 */
void do_cmd_ghost(struct player *p, int ability, int dir)
{
    struct player_class *c = player_id2class(CLASS_GHOST);
    const struct class_book *book = &c->magic.books[0];
    int spell_index;
    struct class_spell *spell;
    struct source who_body;
    struct source *who = &who_body;

    /* Check for ghost-ness */
    if (!p->ghost || player_can_undead(p)) return;

    /* Not when confused */
    if (p->timed[TMD_CONFUSED])
    {
        msg(p, "You are too confused!");
        return;
    }

    /* Set the ability number */
    if (ability < c->magic.total_spells)
    {
        spell_index = ability;

        /* Access the ability */
        spell = &book->spells[spell_index];
    }
    else
    {
        spell_index = ability - c->magic.total_spells;

        /* Access the ability */
        spell = &book->spells[spell_index];

        /* Projected spells */
        if (!spell->sproj)
        {
            msg(p, "You cannot project that ability.");
            return;
        }
    }

    /* Check for level */
    if (spell->slevel > p->lev)
    {
        /* Message */
        msg(p, "You aren't powerful enough to use that ability.");
        return;
    }

    /* Set current spell and inscription */
    p->current_spell = spell_index;
    p->current_item = 0;

    /* Only fire in direction 5 if we have a target */
    if ((dir == 5) && !target_okay(p)) return;

    source_player(who, get_player_index(get_connection(p->conn)), p);

    /* Projected */
    if (ability >= c->magic.total_spells)
    {
        project_aimed(who, PROJ_PROJECT, dir, spell_index, PROJECT_STOP | PROJECT_KILL | PROJECT_PLAY,
            "killed");
    }

    /* Cast the spell */
    else
    {
        bool ident = false;
        struct beam_info beam;

        fill_beam_info(p, spell_index, &beam);

        if (spell->effect && spell->effect->other_msg)
            msg_print_near(p, MSG_PY_SPELL, spell->effect->other_msg);
        if (!effect_do(spell->effect, who, &ident, true, dir, &beam, 0, 0, NULL))
            return;
    }

    /* Take a turn */
    use_energy(p);

    /* Too much can kill you */
    if (p->exp < spell->slevel * spell->smana)
    {
        const char *pself = player_self(p);
        char df[160];

        strnfmt(df, sizeof(df), "exhausted %s with ghostly spells", pself);
        take_hit(p, 5000, "the strain of ghostly powers", false, df);
    }

    /* Take some experience */
    player_exp_lose(p, spell->slevel * spell->smana, true);
}


/*
 * Cast a breath attack
 *
 * Can be "cancelled" at the "Direction?" prompt for free.
 */
void do_cmd_breath(struct player *p, int dir)
{
    bitflag mon_breath[RSF_SIZE];
    int typ;
    struct effect *effect;
    bool ident = false;
    struct source who_body;
    struct source *who = &who_body;

    /* Restrict ghosts */
    if (p->ghost && !(p->dm_flags & DM_GHOST_BODY))
    {
        msg(p, "You need a tangible body to breathe!");
        return;
    }

    /* Handle polymorphed players */
    rsf_wipe(mon_breath);
    if (p->poly_race)
    {
        /* Hack -- require correct "breath attack" */
        rsf_copy(mon_breath, p->poly_race->spell_flags);
        set_breath(mon_breath);
    }

    /* No breath attacks */
    if (rsf_is_empty(mon_breath))
    {
        msg(p, "You are not able to breathe anything but air...");
        return;
    }

    /* Take a turn */
    use_energy(p);

    /* Apply confusion */
    player_confuse_dir(p, &dir);

    /* Get breath effect */
    typ = breath_effect(p, mon_breath);

    /* Make the breath attack an effect */
    effect = mem_zalloc(sizeof(struct effect));
    effect->index = EF_BREATH;
    effect->params[0] = typ;
    effect->params[1] = 20;

    /* Cast the breath attack */
    source_player(who, get_player_index(get_connection(p->conn)), p);
    effect_do(effect, who, &ident, true, dir, NULL, 0, 0, NULL);

    free_effect(effect);
}


/*
 * Cast a mimic spell
 */
void do_cmd_mimic(struct player *p, int page, int spell_index, int dir)
{
    int i, j = 0, k = 0, chance;
    struct class_spell *spell;
    bool projected = false;
    int old_num = get_player_num(p);

    /* Restrict ghosts */
    if (p->ghost && !is_dm_p(p))
    {
        msg(p, "You cannot cast monster spells!");
        return;
    }

    /* Not when confused */
    if (p->timed[TMD_CONFUSED])
    {
        msg(p, "You are too confused!");
        return;
    }

    /* Check each spell */
    for (i = 0; i < p->clazz->magic.books[0].num_spells; i++)
    {
        int flag;

        /* Access the spell */
        spell = &p->clazz->magic.books[0].spells[i];

        /* Access the spell flag */
        flag = spell->effect->flag;

        /* Check spell availability */
        if (!(p->poly_race && rsf_has(p->poly_race->spell_flags, flag))) continue;

        /* Did we find it? */
        if (page == -1)
        {
            /* Normal spell */
            if (flag == spell_index) break;

            /* Projected spell */
            if (flag == 0 - spell_index)
            {
                if (!spell->sproj)
                {
                    msg(p, "You cannot project that spell.");
                    return;
                }
                projected = true;
                break;
            }
        }
        if (page == k)
        {
            /* Normal spell */
            if (j == spell_index) break;

            /* Projected spell */
            if (j == spell_index - p->clazz->magic.total_spells)
            {
                if (!spell->sproj)
                {
                    msg(p, "You cannot project that spell.");
                    return;
                }
                projected = true;
                break;
            }
        }

        /* Next spell */
        j++;
        if (j == MAX_SPELLS_PER_PAGE)
        {
            j = 0;
            k++;
        }
    }

    /* Paranoia */
    if (i == p->clazz->magic.books[0].num_spells) return;

    /* Check mana */
    if (spell->smana > p->csp)
    {
        msg(p, "You do not have enough mana.");
        return;
    }

    /* Antimagic field (no effect on spells that are not "magical") */
    if ((spell->smana > 0) && check_antimagic(p, chunk_get(&p->wpos), NULL))
    {
        use_energy(p);
        return;
    }

    /* Spell cost */
    p->spell_cost = spell->smana;

    /* Spell failure chance */
    chance = spell->sfail;

    /* Failed spell */
    if (magik(chance))
        msg(p, "You failed to get the spell off!");

    /* Process spell */
    else
    {
        struct source who_body;
        struct source *who = &who_body;

        /* Set current spell and inscription */
        p->current_spell = spell->sidx;
        p->current_item = 0;

        /* Only fire in direction 5 if we have a target */
        if ((dir == 5) && !target_okay(p)) return;

        /* Unaware players casting spells reveal themselves */
        if (p->k_idx) aware_player(p, p);

        source_player(who, get_player_index(get_connection(p->conn)), p);

        /* Projected */
        if (projected)
        {
            project_aimed(who, PROJ_PROJECT, dir, spell->sidx,
                PROJECT_STOP | PROJECT_KILL | PROJECT_PLAY, "killed");
        }

        /* Cast the spell */
        else
        {
            bool ident = false;

            if (spell->effect && spell->effect->other_msg)
            {
                /* Hack -- formatted message */
                switch (spell->effect->flag)
                {
                    case RSF_HEAL:
                    case RSF_TELE_TO:
                    case RSF_FORGET:
                    case RSF_S_KIN:
                    {
                        msg_format_near(p, MSG_PY_SPELL, spell->effect->other_msg,
                            player_poss(p));
                        break;
                    }
                    default:
                    {
                        msg_print_near(p, MSG_PY_SPELL, spell->effect->other_msg);
                        break;
                    }
                }
            }
            if (!effect_do(spell->effect, who, &ident, true, dir, NULL, 0, 0, NULL))
                return;
        }
    }

    /* Take a turn */
    use_energy(p);

    /* Use some mana */
    p->csp -= p->spell_cost;

    /* Hack -- redraw picture */
    redraw_picture(p, old_num);

    /* Redraw mana */
    p->upkeep->redraw |= (PR_MANA);
}
