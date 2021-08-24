/* File: cmd5.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies. Other copyrights may also apply.
 */

/* Purpose: Spell/Prayer commands */

#include "angband.h"

cptr spell_category_name(int tval)
{
    switch (tval)
    {
    case TV_HISSATSU_BOOK:
        return "art";
    case TV_LIFE_BOOK:
        return "prayer";
    case TV_MUSIC_BOOK:
        return "song";
    default:
        return "spell";
    }
}

/*
 * Allow user to choose a spell/prayer from the given book.
 *
 * If a valid spell is chosen, saves it in '*sn' and returns TRUE
 * If the user hits escape, returns FALSE, and set '*sn' to -1
 * If there are no legal choices, returns FALSE, and sets '*sn' to -2
 *
 * The "prompt" should be "cast", "recite", or "study"
 * The "known" should be TRUE for cast/pray, FALSE for study
 */

bool select_the_force = FALSE;

static int get_spell(int *sn, cptr prompt, int sval, bool learned, int use_realm)
{
    int         i;
    int         spell = -1;
    int         num = 0;
    int         ask = TRUE;
    int         need_mana;
    byte        spells[64];
    bool        flag, redraw, okay;
    char        choice;
    magic_type  *s_ptr;
    char        out_val[160];
    cptr        p;
    rect_t      display = ui_menu_rect();
    int menu_line = (use_menu ? 1 : 0);

#ifdef ALLOW_REPEAT /* TNB */

    /* Get the spell, if available */
    if (repeat_pull(sn))
    {
        /* Verify the spell */
        if (spell_okay(*sn, learned, FALSE, use_realm))
        {
            /* Success */
            return (TRUE);
        }
    }

#endif /* ALLOW_REPEAT -- TNB */

    p = spell_category_name(mp_ptr->spell_book);

    /* Extract spells */
    for (spell = 0; spell < 32; spell++)
    {
        /* Check for this spell */
        if ((fake_spell_flags[sval] & (1L << spell)))
        {
            /* Collect this spell */
            spells[num++] = spell;
        }
    }

    /* Assume no usable spells */
    okay = FALSE;

    /* Assume no spells available */
    (*sn) = -2;

    /* Check for "okay" spells */
    for (i = 0; i < num; i++)
    {
        /* Look for "okay" spells */
        if (spell_okay(spells[i], learned, FALSE, use_realm)) okay = TRUE;
    }

    /* No "okay" spells */
    if (!okay) return (FALSE);
    if (((use_realm) != plr->realm1) && ((use_realm) != plr->realm2) && (plr->pclass != CLASS_SORCERER) && (plr->pclass != CLASS_RED_MAGE)) return FALSE;
    if (((plr->pclass == CLASS_SORCERER) || (plr->pclass == CLASS_RED_MAGE)) && !is_magic(use_realm)) return FALSE;
    if ((plr->pclass == CLASS_RED_MAGE) && ((use_realm) != REALM_ARCANE) && (sval > 1)) return FALSE;

    /* Assume cancelled */
    *sn = (-1);

    /* Nothing chosen yet */
    flag = FALSE;

    /* No redraw yet */
    redraw = FALSE;

    /* Show choices */
    plr->window |= (PW_SPELL);

    /* Window stuff */
    window_stuff();

    /* Build a prompt (accept all spells) */
    (void)strnfmt(out_val, 78, "(%^ss %c-%c, *=List, ESC=exit) %^s which %s? ",
        p, I2A(0), I2A(num - 1), prompt, p);

    /* Get a spell from the user */

    choice = (always_show_list || use_menu) ? ESCAPE : 1;
    while (!flag)
    {
        if (choice == ESCAPE) choice = ' ';
        else if (!get_com(out_val, &choice, TRUE))break;

        if (use_menu && choice != ' ')
        {
            switch (choice)
            {
                case '0':
                {
                    screen_load();
                    return FALSE;
                }

                case '8':
                case 'k':
                case 'K':
                {
                    menu_line += (num - 1);
                    break;
                }

                case '2':
                case 'j':
                case 'J':
                {
                    menu_line++;
                    break;
                }

                case 'x':
                case 'X':
                case '\r':
                case '\n':
                {
                    i = menu_line - 1;
                    ask = FALSE;
                    break;
                }
            }
            if (menu_line > num) menu_line -= num;
            /* Display a list of spells */
            print_spells(menu_line, spells, num, display, use_realm);
            if (ask) continue;
        }
        else
        {
            /* Request redraw */
            if ((choice == ' ') || (choice == '*') || (choice == '?'))
            {
                /* Show the list */
                if (!redraw)
                {
                    /* Show list */
                    redraw = TRUE;

                    /* Save the screen */
                    screen_save();

                    /* Display a list of spells */
                    print_spells(menu_line, spells, num, display, use_realm);
                }

                /* Hide the list */
                else
                {
                    if (use_menu) continue;

                    /* Hide list */
                    redraw = FALSE;

                    /* Restore the screen */
                    screen_load();
                }

                /* Redo asking */
                continue;
            }


            /* Note verify */
            ask = (isupper(choice));

            /* Lowercase */
            if (ask) choice = tolower(choice);

            /* Extract request */
            i = (islower(choice) ? A2I(choice) : -1);
        }

        /* Totally Illegal */
        if ((i < 0) || (i >= num))
        {
            bell();
            continue;
        }

        /* Save the spell index */
        spell = spells[i];

        /* Require "okay" spells */
        if (!spell_okay(spell, learned, FALSE, use_realm))
        {
            bell();
            msg_format("You may not %s that %s.", prompt, p);
            continue;
        }

        /* Verify it */
        if (ask)
        {
            char tmp_val[160];

            /* Access the spell */
            if (!is_magic(use_realm))
            {
                s_ptr = &technic_info[use_realm - MIN_TECHNIC][spell];
            }
            else
            {
                s_ptr = &mp_ptr->info[use_realm - 1][spell];
            }

            /* Extract mana consumption rate */
            if (use_realm == REALM_HISSATSU)
            {
                need_mana = s_ptr->smana;
            }
            else
            {
                need_mana = mod_need_mana(s_ptr->smana, spell, use_realm);
            }

            /* Prompt */
            (void)strnfmt(tmp_val, 78, "%^s %s (%d mana, %d%% fail)? ",
                prompt, do_spell(use_realm, spell, SPELL_NAME), need_mana,
                spell_chance(spell, use_realm));

            /* Belay that order */
            if (!get_check(tmp_val)) continue;
        }

        /* Stop the loop */
        flag = TRUE;
    }


    /* Restore the screen */
    if (redraw) screen_load();


    /* Show choices */
    plr->window |= (PW_SPELL);

    /* Window stuff */
    window_stuff();


    /* Abort if needed */
    if (!flag) return FALSE;

    /* Save the choice */
    (*sn) = spell;

#ifdef ALLOW_REPEAT /* TNB */

    repeat_push(*sn);

#endif /* ALLOW_REPEAT -- TNB */

    /* Success */
    return TRUE;
}


static caster_info *_caster_info(void)
{
    class_t *class_ptr = get_class_aux(plr->pclass, plr->psubclass);
    if (class_ptr->hooks.caster_info)
        return class_ptr->hooks.caster_info();
    return NULL;
}
static u32b _realm2_bits(void)
{
    caster_info *info = _caster_info();
    if (!info) return 0;
    return info->realm2_choices;
}
static bool item_tester_learn_spell(object_type *o_ptr)
{
    u32b choices = _realm2_bits();

    if (plr->pclass == CLASS_PRIEST)
    {
        if (plr->realm1 == REALM_NATURE)
            choices &= ~(CH_DEATH | CH_DAEMON | CH_LIFE | CH_CRUSADE);
        else if (is_good_realm(plr->realm1))
            choices &= ~(CH_DEATH | CH_DAEMON);
        else
            choices &= ~(CH_LIFE | CH_CRUSADE);
    }

    if (!obj_is_spellbook(o_ptr)) return FALSE;
    if (o_ptr->tval == TV_MUSIC_BOOK && plr->pclass == CLASS_BARD) return TRUE;
    if (o_ptr->tval == TV_BURGLARY_BOOK && plr->pclass == CLASS_ROGUE) return TRUE;
    else if (o_ptr->tval == TV_HEX_BOOK && plr->pclass == CLASS_HIGH_PRIEST && REALM1_BOOK == o_ptr->tval) return TRUE;
    else if (o_ptr->tval == TV_BLESS_BOOK && plr->pclass == CLASS_HIGH_PRIEST && REALM1_BOOK == o_ptr->tval) return TRUE;
    else if (!is_magic(tval2realm(o_ptr->tval))) return FALSE;
    if (REALM1_BOOK == o_ptr->tval || REALM2_BOOK == o_ptr->tval) return TRUE;
    if (choices & (0x0001 << (tval2realm(o_ptr->tval) - 1))) return TRUE;
    return FALSE;
}

static void change_realm2(int next_realm)
{
    int i, j = 0;
    for (i = 0; i < 64; i++)
    {
        plr->spell_order[j] = plr->spell_order[i];
        if (plr->spell_order[i] < 32) j++;
    }
    for (; j < 64; j++)
        plr->spell_order[j] = 99;

    for (i = 32; i < 64; i++)
    {
        plr->spell_exp[i] = SPELL_EXP_UNSKILLED;
    }
    plr->spell_learned2 = 0L;
    plr->spell_worked2 = 0L;
    plr->spell_forgotten2 = 0L;

    plr->old_realm |= 1 << (plr->realm2-1);
    plr->realm2 = next_realm;

    plr->notice |= (PN_OPTIMIZE_PACK); /* cf obj_cmp's initial hack */
    plr->update |= (PU_SPELLS);
    handle_stuff();

    /* Load an autopick preference file */
    autopick_load_pref(FALSE);
}


/*
 * Study a book to gain a new spell/prayer
 */
void do_cmd_study(void)
{
    obj_prompt_t prompt = {0};
    int          i;
    int          increment = 0;
    bool         learned = FALSE;
    int          spell = -1; /* Spells of realm2 will have an increment of +32 */
    cptr         p = spell_category_name(mp_ptr->spell_book);

    if (!plr->realm1)
    {
        msg_print("You cannot read books!");
        return;
    }

    if (plr_tim_find(T_BLIND) || no_light())
    {
        msg_print("You cannot see!");
        return;
    }

    if (plr_tim_find(T_CONFUSED))
    {
        msg_print("You are too confused!");
        return;
    }

    if (!plr->new_spells)
    {
        msg_format("You cannot learn any new %ss!", p);
        return;
    }

    if (plr->special_defense & KATA_MUSOU)
        set_action(ACTION_NONE);

/*  Please, no more -more-!
    msg_format("You can learn %d new %s%s.", plr->new_spells, p,
        (plr->new_spells == 1?"":"s"));

    msg_print(NULL);*/


    /* Get an item */
    prompt.prompt = "Study which book?";
    prompt.error = "You have no books that you can read.";
    prompt.filter = item_tester_learn_spell;
    prompt.where[0] = INV_PACK;
    prompt.where[1] = INV_FLOOR;
    
    obj_prompt(&prompt);
    if (!prompt.obj) return;

    if (prompt.obj->tval == REALM2_BOOK) increment = 32;
    else if (prompt.obj->tval != REALM1_BOOK)
    {
        if (!get_check("Really, change magic realm? ")) return;
        change_realm2(tval2realm(prompt.obj->tval));
        increment = 32;
        autopick_alter_obj(prompt.obj, FALSE);
    }

    /* Track the object kind */
    object_kind_track(prompt.obj->k_idx);

    /* Hack -- Handle stuff */
    handle_stuff();

    /* Mage -- Learn a selected spell */
    if (mp_ptr->spell_book != TV_LIFE_BOOK)
    {
        /* Ask for a spell, allow cancel */
        if (!get_spell(&spell, "study", prompt.obj->sval, FALSE, prompt.obj->tval - TV_LIFE_BOOK + 1)
            && (spell == -1)) return;

    }
    /* Priest -- Learn a random prayer */
    else
    {
        int k = 0;

        int gift = -1;

        /* Extract spells */
        for (spell = 0; spell < 32; spell++)
        {
            /* Check spells in the book */
            if ((fake_spell_flags[prompt.obj->sval] & (1L << spell)))
            {
                /* Skip non "okay" prayers */
                if (!spell_okay(spell, FALSE, TRUE,
                    (increment ? plr->realm2 : plr->realm1))) continue;

                /* Hack -- Prepare the randomizer */
                k++;

                /* Hack -- Apply the randomizer */
                if (one_in_(k)) gift = spell;
            }
        }

        /* Accept gift */
        spell = gift;
    }

    /* Nothing to study */
    if (spell < 0)
    {
        msg_format("You cannot learn any %ss in that book.", p);
        return;
    }

    if (increment) spell += increment;

    /* Learn the spell */
    if (spell < 32)
    {
        if (plr->spell_learned1 & (1L << spell)) learned = TRUE;
        else plr->spell_learned1 |= (1L << spell);
    }
    else
    {
        if (plr->spell_learned2 & (1L << (spell - 32))) learned = TRUE;
        else plr->spell_learned2 |= (1L << (spell - 32));
    }

    if (learned)
    {
        int max_exp = (spell < 32) ? SPELL_EXP_MASTER : SPELL_EXP_EXPERT;
        int old_exp = plr->spell_exp[spell];
        int new_rank = EXP_LEVEL_UNSKILLED;
        cptr name = do_spell(increment ? plr->realm2 : plr->realm1, spell%32, SPELL_NAME);

        if (old_exp >= max_exp)
        {
            msg_format("You don't need to study this %s anymore.", p);
            return;
        }
        if (!get_check(format("You will study a %s of %s again. Are you sure? ", p, name)))
        {
            return;
        }
        else if (old_exp >= SPELL_EXP_EXPERT)
        {
            plr->spell_exp[spell] = SPELL_EXP_MASTER;
            new_rank = EXP_LEVEL_MASTER;
        }
        else if (old_exp >= SPELL_EXP_SKILLED)
        {
            if (spell >= 32) plr->spell_exp[spell] = SPELL_EXP_EXPERT;
            else plr->spell_exp[spell] += SPELL_EXP_EXPERT - SPELL_EXP_SKILLED;
            new_rank = EXP_LEVEL_EXPERT;
        }
        else if (old_exp >= SPELL_EXP_BEGINNER)
        {
            plr->spell_exp[spell] = SPELL_EXP_SKILLED + (old_exp - SPELL_EXP_BEGINNER) * 2 / 3;
            new_rank = EXP_LEVEL_SKILLED;
        }
        else
        {
            plr->spell_exp[spell] = SPELL_EXP_BEGINNER + old_exp / 3;
            new_rank = EXP_LEVEL_BEGINNER;
        }
        msg_format("Your proficiency of %s is now %s rank.", name, exp_level_str[new_rank]);
    }
    else
    {
        /* Find the next open entry in "plr->spell_order[]" */
        for (i = 0; i < 64; i++)
        {
            /* Stop at the first empty space */
            if (plr->spell_order[i] == 99) break;
        }

        /* Add the spell to the known list */
        plr->spell_order[i++] = spell;

        /* Mention the result */
        msg_format("You have learned the %s of %s.",
            p, do_spell(increment ? plr->realm2 : plr->realm1, spell % 32, SPELL_NAME));
    }

    /* Take a turn */
    energy_use = 100;

    switch (mp_ptr->spell_book)
    {
    case TV_LIFE_BOOK:
        virtue_add(VIRTUE_FAITH, 1);
        break;
    case TV_DEATH_BOOK:
    case TV_NECROMANCY_BOOK:
        virtue_add(VIRTUE_UNLIFE, 1);
        break;
    case TV_NATURE_BOOK:
        virtue_add(VIRTUE_NATURE, 1);
        break;
    default:
        virtue_add(VIRTUE_KNOWLEDGE, 1);
        break;
    }

    /* Sound */
    sound(SOUND_STUDY);

    /* One less spell available */
    plr->learned_spells++;
#if 0
    /* Message if needed */
    if (plr->new_spells)
    {
        /* Message */
        msg_format("You can learn %d more %s%s.", plr->new_spells, p,
                   (plr->new_spells != 1) ? "s" : "");
    }
#endif

    plr->update |= PU_SPELLS;
    plr->redraw |= PR_EFFECTS;
    plr->window |= PW_OBJECT;
}


static void wild_magic(int spell)
{
    switch (randint1(spell) + randint0(8))
    {
    case 1:
    case 2:
    case 3:
        teleport_player(10, TELEPORT_PASSIVE);
        break;
    case 4:
    case 5:
    case 6:
        teleport_player(100, TELEPORT_PASSIVE);
        break;
    case 7:
    case 8:
        teleport_player(200, TELEPORT_PASSIVE);
        break;
    case 9:
    case 10:
    case 11:
        unlite_area(10, 3);
        break;
    case 12:
    case 13:
    case 14:
        lite_area(damroll(2, 3), 2);
        break;
    case 15:
        destroy_doors_touch();
        break;
    case 16: case 17:
        wall_breaker();
        break;
    case 18:
        plr_burst(1, GF_SLEEP, plr->lev);
        break;
    case 19:
    case 20:
        trap_creation(plr->pos);
        break;
    case 21:
    case 22:
        door_creation();
        break;
    case 23:
    case 24:
    case 25:
        aggravate_monsters(who_create_null());
        break;
    case 26:
    case 27:
        earthquake(plr->pos, 5);
        break;
    case 28:
        mut_gain_random(NULL);
        break;
    case 29:
    case 30:
        apply_disenchant(1);
        break;
    case 31:
        lose_all_info();
        break;
    case 32:
        plr_burst(1 + spell/10, GF_CHAOS, spell + 5);
        break;
    case 33:
    case 34:
        wall_stone();
        break;
    case 35:
    case 36: {
        int counter = 0;
        int type = rand_range(SUMMON_BIZARRE1, SUMMON_BIZARRE6);
        int dl = cave->dun_lvl*3/2;
        while (counter++ < 8)
            summon_specific(who_create_null(), plr->pos, dl, type, PM_ALLOW_GROUP | PM_NO_PET);
        break; }
    case 37:
    case 38:
    case 39: /* current max */
    default: /* paranoia */
        activate_hi_summon(plr->pos, FALSE);
        break;
    }

    return;
}


/*
 * Cast a spell
 */
static int _force_handler(obj_prompt_context_ptr context, int cmd)
{
    if (cmd == 'F')
        return OP_CMD_DISMISS;
    return OP_CMD_SKIPPED;
}

#define _CAST 1
#define _BROWSE 2
static obj_ptr _get_spellbook(int mode)
{
    obj_prompt_t prompt = {0};
    char         msg[255];

    sprintf(msg, "%s which book%s?",
        mode == _CAST ? "Use" : "Browse",
        plr->pclass == CLASS_FORCETRAINER ?
            " (<color:keypress>F</color> for the Force)" : "");

    prompt.prompt = msg;
    prompt.error = "You have no books that you can read.";
    prompt.filter = obj_is_readable_book;
    prompt.where[0] = INV_PACK;
    prompt.where[1] = INV_FLOOR;

    if (plr->pclass == CLASS_FORCETRAINER)
    {
        prompt.error = NULL;
        prompt.cmd_handler = _force_handler;
        switch (obj_prompt(&prompt))
        {
        case OP_CUSTOM:
        case OP_NO_OBJECTS:
            if (mode == _CAST)
                do_cmd_spell();
            else
                do_cmd_spell_browse();
            return NULL;
        }
        return prompt.obj;
    }

    obj_prompt(&prompt);
    return prompt.obj;
}

void do_cmd_cast(void)
{
    obj_ptr      book;
    int          spell;
    int          chance;
    int          increment = 0;
    int          use_realm;
    int          need_mana;
    int          take_mana;
    cptr         prayer;
    magic_type  *s_ptr;
    caster_info *caster_ptr = get_caster_info();

    /* Require spell ability */
    if (!plr->realm1 && plr->pclass != CLASS_SORCERER && plr->pclass != CLASS_RED_MAGE)
    {
        msg_print("You cannot cast spells!");
        return;
    }

    /* Require lite */
    if (plr_tim_find(T_BLIND) || no_light())
    {
        if (plr->pclass == CLASS_FORCETRAINER) do_cmd_spell();
        else
        {
            msg_print("You cannot see!");
            flush();
        }
        return;
    }

    /* Not when confused */
    if (plr_tim_find(T_CONFUSED))
    {
        msg_print("You are too confused!");
        flush();
        return;
    }

    /* Chants */
    if (plr->realm1 == REALM_HEX)
    {
        if (hex_count() == hex_max())
        {
            bool flag = FALSE;
            msg_print("You cannot chant any more foul curses.");
            flush();
            if (plr->lev >= 35)
            {
                msg_print(NULL);
                flag = hex_stop_one();
                if (!msg_line_is_empty())
                    msg_line_clear(); /* XXX 'Stop All' emits 'You stop chanting' due to ACTION_SPELL->ACTION_NONE */
            }
            if (!flag) return;
        }
    }
    if (plr->realm1 == REALM_BLESS)
    {
        if (bless_count() == bless_max())
        {
            bool flag = FALSE;
            msg_print("You cannot chant any more blessings.");
            flush();
            if (plr->lev >= 35)
            {
                msg_print(NULL);
                flag = bless_stop_one();
                if (!msg_line_is_empty())
                    msg_line_clear(); /* XXX 'Stop All' emits 'You stop chanting' due to ACTION_SPELL->ACTION_NONE */
            }
            if (!flag) return;
        }
    }

    prayer = spell_category_name(mp_ptr->spell_book);

    book = _get_spellbook(_CAST);
    if (!book) return;

    if (plr->pclass != CLASS_SORCERER && plr->pclass != CLASS_RED_MAGE && book->tval == REALM2_BOOK)
        increment = 32;

    object_kind_track(book->k_idx);
    handle_stuff();

    use_realm = tval2realm(book->tval);

    /* Ask for a spell */
    if (!get_spell(&spell, (mp_ptr->spell_book == TV_LIFE_BOOK) ? "recite" : "cast",
        book->sval, TRUE, use_realm))
    {
        if (spell == -2)
            msg_format("You don't know any %ss in that book.", prayer);
        return;
    }

    if (!is_magic(use_realm))
    {
        s_ptr = &technic_info[use_realm - MIN_TECHNIC][spell];
    }
    else
    {
        s_ptr = &mp_ptr->info[use_realm - 1][spell];
    }

    /* Extract mana consumption rate */
    need_mana = mod_need_mana(s_ptr->smana, spell, use_realm);
    current_spell_cost = need_mana; /* XXX */

    /* Verify "dangerous" spells */
    if (caster_ptr && (caster_ptr->options & CASTER_USE_HP))
    {
        if (need_mana > plr->chp)
        {
            msg_print("You do not have enough hp to use this spell.");
            if (flush_failure) flush();
            return;
        }
    }
    else if (need_mana > plr->csp)
    {
        if (flush_failure) flush();

        /* Warning */
        msg_format("You do not have enough mana to %s this %s.",
            ((mp_ptr->spell_book == TV_LIFE_BOOK) ? "recite" : "cast"),
            prayer);

        return;
    }

    /* Spell failure chance */
    chance = spell_chance(spell, use_realm);

    /* Take spell cost eagerly unless we are exerting ourselves.
       This is to prevent death from using a force weapon with a spell
       that also attacks, like Cyclone.
    */
    if (caster_ptr && (caster_ptr->options & CASTER_USE_HP))
    {
        take_mana = 0;
    }
    else
    {
        take_mana = 0;
        if (need_mana <= plr->csp)
        {
            plr->csp -= need_mana;
            take_mana = need_mana;
        }
    }

    /* Take a turn ... Note some spells might have variable
        energy costs, so we allow them to override the default
        value when handling SPELL_CAST.
    */
    energy_use = 100;
    if (plr->pclass == CLASS_YELLOW_MAGE)
    {
        int delta = plr->lev - s_ptr->slevel;
        if (delta > 0) /* paranoia */
            energy_use -= delta;
    }
    energy_use = energy_use * 100 / plr->spells_per_round;

    /* Failed spell */
    if (randint0(100) < chance)
    {
        if (flush_failure) flush();

        msg_format("You failed to cast %s!", do_spell(use_realm, spell % 32, SPELL_NAME));

        if (take_mana && prace_is_(RACE_DEMIGOD) && plr->psubrace == DEMIGOD_ATHENA)
            plr->csp += take_mana/2;

        spell_stats_on_fail_old(use_realm, spell);
        sound(SOUND_FAIL);

        if (caster_ptr && caster_ptr->on_fail != NULL)
        {
            spell_info hack = {0};
            hack.level = s_ptr->slevel;
            hack.cost = need_mana;
            hack.fail = chance;
            (caster_ptr->on_fail)(&hack);
        }
        if (caster_ptr && (caster_ptr->options & CASTER_USE_HP))
            take_hit(DAMAGE_USELIFE, need_mana, "concentrating too hard");

        virtue_on_fail_spell(use_realm, chance);

        /* Failure casting may activate some side effect */
        do_spell(use_realm, spell, SPELL_FAIL);

        if ((book->tval == TV_CHAOS_BOOK) && (randint1(100) < spell))
        {
            msg_print("You produce a chaotic effect!");
            wild_magic(spell);
        }
        else if ((book->tval == TV_DEATH_BOOK) && (randint1(100) < spell))
        {
            if ((book->sval == 3) && one_in_(2))
            {
                sanity_blast(0, TRUE);
            }
            else
            {
                msg_print("It hurts!");

                take_hit(DAMAGE_LOSELIFE, damroll(book->sval + 1, 6), "a miscast Death spell");

                if ((spell > 15) && one_in_(6) && !plr->hold_life)
                    lose_exp(spell * 250);
            }
        }
        else if ((book->tval == TV_MUSIC_BOOK) && (randint1(200) < spell))
        {
            msg_print("An infernal sound echoed.");
            aggravate_monsters(who_create_null());
        }
    }

    /* Process spell */
    else
    {
        /* Canceled spells cost neither a turn nor mana */
        if (!do_spell(use_realm, spell, SPELL_CAST))
        {
            /* If we eagerly took mana for this spell, then put it back! */
            if (take_mana > 0)
                plr->csp += take_mana;
            energy_use = 0;
            return;
        }

        spell_stats_on_cast_old(use_realm, spell);

        if (caster_ptr && (caster_ptr->options & CASTER_USE_HP))
            take_hit(DAMAGE_USELIFE, need_mana, "concentrating too hard");

        if (caster_ptr && caster_ptr->on_cast != NULL)
        {
            spell_info hack = {0};
            hack.level = s_ptr->slevel;
            hack.cost = need_mana;
            hack.fail = chance;
            (caster_ptr->on_cast)(&hack);
        }

        /* A spell was cast */
        if (!(increment ?
            (plr->spell_worked2 & (1L << spell)) :
            (plr->spell_worked1 & (1L << spell)))
            && (plr->pclass != CLASS_SORCERER)
            && (plr->pclass != CLASS_RED_MAGE))
        {
            int e = s_ptr->sexp;

            /* The spell worked */
            if (use_realm == plr->realm1)
            {
                plr->spell_worked1 |= (1L << spell);
            }
            else
            {
                plr->spell_worked2 |= (1L << spell);
            }

            /* Gain experience */
            gain_exp(e * s_ptr->slevel);

            /* Redraw object recall */
            plr->window |= (PW_OBJECT);

            virtue_on_first_cast_spell(use_realm);
        }

        virtue_on_cast_spell(use_realm, need_mana, chance);

        if (mp_ptr->spell_xtra & MAGIC_GAIN_EXP)
        {
            int  index = (increment ? 32 : 0)+spell;
            s16b cur_exp = plr->spell_exp[index];
            int  dlvl = cave->difficulty; /* gain prof in wilderness ... */
            s16b exp_gain = 0;

            if (dlvl) /* ... but not in town */
            {  /* You'll need to spreadsheet this to see if this is any good ...
                * Try a cross tab spell level vs dun level. Here is a rough summary
                * of minimum dlvl to reach desired proficiency (but remember that
                * interpolation smooths things out. So you can reach 1530 prof with
                * a L50 spell on DL90, for instance):
                * SLvl  Be  Sk  Ex  Ma
                *   50  24  57  73 100
                *   40  19  47  61  84
                *   30  14  37  49  68
                *   20   9  27  36  51
                *   10   4  17  24  35
                *    5   1  12  18  27
                *    1   1   8  13  20 */
                int ratio = (17 + s_ptr->slevel) * 100 / (10 + dlvl);
                point_t max_tbl[4] = { {60, 1600}, {100, 1200}, {200, 900}, {300, 0} };
                int max_exp = interpolate(ratio, max_tbl, 4);

                if (cur_exp < max_exp)
                {
                    point_t gain_tbl[9] = { /* 0->900->1200->1400->1600 */
                        {0, 128}, {200, 64}, {400, 32}, {600, 16},
                        {800, 8}, {1000, 4}, {1200, 2}, {1400, 1}, {1600, 1} };
                    exp_gain = interpolate(cur_exp, gain_tbl, 9);
                }
                else if (0 || plr->wizard)
                {
                    msg_format("<color:B>When casting an <color:R>L%d</color> spell on "
                        "<color:R>DL%d</color> your max proficiency is <color:R>%d</color> "
                        "(Current: <color:R>%d</color>).</color> <color:D>%d</color>",
                        s_ptr->slevel, dlvl, max_exp, cur_exp, ratio);
                }
            }

            if (exp_gain)
            {
                int  old_level = spell_exp_level(cur_exp);
                int  new_level = old_level;
                int  max = increment ? SPELL_EXP_EXPERT : SPELL_EXP_MASTER;

                plr->spell_exp[index] += exp_gain;
                if (plr->spell_exp[index] > max)
                    plr->spell_exp[index] = max;
                new_level = spell_exp_level(plr->spell_exp[index]);
                if (new_level > old_level)
                {
                    cptr desc[5] = { "Unskilled", "a Beginner", "Skilled", "an Expert", "a Master" };
                    msg_format("You are now <color:B>%s</color> in <color:R>%s</color>.",
                        desc[new_level],
                        do_spell(use_realm, spell % 32, SPELL_NAME));
                }
                else if (plr->wizard)
                {
                    msg_format("You now have <color:B>%d</color> proficiency in <color:R>%s</color>.",
                        plr->spell_exp[index],
                        do_spell(use_realm, spell % 32, SPELL_NAME));
                }
                else if (plr->spell_exp[index]/100 > cur_exp/100)
                {
                    msg_format("<color:B>You are getting more proficient with <color:R>%s</color>.</color>",
                        do_spell(use_realm, spell % 32, SPELL_NAME));
                }
            }
        }
    }

    /* In general, we already charged the players sp. However, in the event the
       player knowingly exceeded their csp, then, well, they get what they deserve!
    */
    if (take_mana == 0)
    {
        /* Sufficient mana ... this should no longer fire ... unless we add a spell
           to gain spellpoints, but that seems unlikely ;)  Actually, there is a mincrafter
           spell that might do just that, but I don't think that spell comes thru this fn.
           So it is prudent to double check for overexertion ...
        */
        if (caster_ptr && (caster_ptr->options & CASTER_USE_HP))
        {
        }
        else if (need_mana <= plr->csp)
        {
            plr->csp -= need_mana;
        }
    }

    /* Redraw mana */
    plr->redraw |= (PR_MANA);

    /* Window stuff */
    plr->window |= (PW_SPELL);
}

/*
 * Peruse the spells/prayers in a book
 *
 * Note that *all* spells in the book are listed
 *
 * Note that browsing is allowed while confused or blind,
 * and in the dark, primarily to allow browsing in stores.
 */
void do_cmd_browse(void)
{
    obj_ptr book;
    int     use_realm = 0, j, line;
    int     spell = -1;
    int     num = 0;
    rect_t  display = ui_menu_rect();
    byte    spells[64];
    char    temp[62*4];

    if (!(plr->realm1 || plr->realm2) && (plr->pclass != CLASS_SORCERER) && (plr->pclass != CLASS_RED_MAGE))
    {
        msg_print("You cannot read books!");
        return;
    }

    if (plr->special_defense & KATA_MUSOU)
        set_action(ACTION_NONE);

    book = _get_spellbook(_BROWSE);
    if (!book) return;

    use_realm = tval2realm(book->tval);

    /* Track the object kind */
    object_kind_track(book->k_idx);
    handle_stuff();

    /* Extract spells */
    for (spell = 0; spell < 32; spell++)
    {
        /* Check for this spell */
        if ((fake_spell_flags[book->sval] & (1L << spell)))
        {
            /* Collect this spell */
            spells[num++] = spell;
        }
    }

    screen_save();
    prt("", 0, 0);

    /* Keep browsing spells. Exit browsing on cancel. */
    while(TRUE)
    {
        /* Ask for a spell, allow cancel */
        if (!get_spell(&spell, "browse", book->sval, TRUE, use_realm))
        {
            /* If cancelled, leave immediately. */
            if (spell == -1) break;

            /* Display a list of spells */
            print_spells(0, spells, num, display, use_realm);

            /* Notify that there's nothing to see, and wait. */
            if (use_realm == REALM_HISSATSU)
                prt("No techniques to browse.", 0, 0);
            else
                prt("No spells to browse.", 0, 0);
            (void)inkey();


            /* Restore the screen */
            screen_load();

            return;
        }

        if (do_spell(use_realm, spell, SPELL_ON_BROWSE)) continue;

        /* Clear lines, position cursor  (really should use strlen here) */
        line = display.y + num + 1;
        Term_erase(display.x, line, display.cx);
        Term_erase(display.x, line + 1, display.cx);
        Term_erase(display.x, line + 2, display.cx);
        Term_erase(display.x, line + 3, display.cx);
        Term_erase(display.x, line + 4, display.cx);

        roff_to_buf(do_spell(use_realm, spell, SPELL_DESC), 62, temp, sizeof(temp));

        for (j = 0; temp[j]; j += 1 + strlen(&temp[j]))
        {
            put_str(&temp[j], ++line, display.x);
        }
    }
    screen_load();
}

/* we need to calc_bonuses any time the plr_pack() changes, since
 * plr->align initializes to `plr_pack()->align`. since calc_bonuses
 * is an enormous sledge-hammer, try to detect changes ... */
void check_pets_num_and_align(void)
{
    static int old_align = 2000; /* impossible! */
    int align = mon_pack_align(plr_pack()); /* -255 to 255 */

    if (align != old_align) /* XXX ignore slight differences? */
    {
        old_align = align;
        plr->update |= PU_BONUS;
    }
}

int calculate_upkeep(void)
{
    mon_pack_ptr pets = plr_pack();
    int     i, ct = mon_pack_count(pets);
    int     upkeep_factor = 0;
    bool    have_a_unique = FALSE;
    s32b    total_friend_levels = 0;

    for (i = 0; i < ct; i++)
    {
        mon_ptr mon = vec_get(pets->members, i);
        mon_race_ptr race = mon->race;

        if (!mon_is_valid(mon)) continue;
        if (mon_has_smart_flag(mon, SM_TEMP_PET)) continue;
        if (warlock_is_pact_monster(race) || (mon->mflag2 & MFLAG2_ILLUSION))
        {
            total_friend_levels += mon_race_lvl(race)/2;
            if (mon_race_is_unique(race))
            {
                total_friend_levels += mon_race_lvl(race)/2;
                have_a_unique = TRUE;
            }
        }
        else if (mon_race_is_unique(race))
        {
            if (plr->pclass == CLASS_CAVALRY || plr->prace == RACE_MON_RING)
            {
                if (plr->riding == mon->id)
                    total_friend_levels += (mon_race_lvl(race)+5)*2;
                else if (!have_a_unique && mon_race_is_ridable(race))
                    total_friend_levels += (mon_race_lvl(race)+5)*7/2;
                else
                    total_friend_levels += (mon_race_lvl(race)+5)*10;
                have_a_unique = TRUE;
            }
            else
                total_friend_levels += (mon_race_lvl(race)+5)*10;
        }
        else
            total_friend_levels += mon_race_lvl(race);
    }
    check_pets_num_and_align();
    if (ct)
    {
        int div = plr_class()->pets;

        if (prace_is_(RACE_DEMIGOD) && plr->psubrace == DEMIGOD_APHRODITE)
            div /= 2;

        if (prace_is_(RACE_MON_QUYLTHULG))
            div = 7;

        if (plr->dragon_realm == DRAGON_REALM_DOMINATION)
            div = 9;

        if (prace_is_(RACE_MON_VAMPIRE))
            div = 10;

        upkeep_factor = (total_friend_levels - (plr->lev * 80 / div));

        if (upkeep_factor < 0) upkeep_factor = 0;
        if (upkeep_factor > 1000) upkeep_factor = 1000;
    }
    return upkeep_factor;
}

void do_cmd_pet_dismiss(void)
{
    vec_ptr pets = plr_pets_for_dismiss();
    int     max_pet = 0;
    bool    all_pets = FALSE;
    int     Dismissed = 0;
    int     i, cu, cv;

    cu = Term->scr->cu;
    cv = Term->scr->cv;
    Term->scr->cu = 0;
    Term->scr->cv = 1;

    max_pet = vec_length(pets);
    for (i = 0; i < max_pet; i++)
    {
        mon_ptr m_ptr = vec_get(pets, i);
        bool delete_this;
        char friend_name[MAX_NLEN];
        char buf[512];
        bool kakunin;

        delete_this = FALSE;
        kakunin = m_ptr->id == plr->riding || m_ptr->nickname;
        monster_desc(friend_name, m_ptr, MD_ASSUME_VISIBLE);

        if (!all_pets)
        {
            /* Hack -- health bar for this monster */
            health_track(m_ptr);

            /* Hack -- handle stuff */
            handle_stuff();

            sprintf(buf, "Dismiss %s? [Yes/No/Unnamed (%d remain)]", friend_name, max_pet - i);
            prt(buf, 0, 0);

            if (m_ptr->ml)
                move_cursor_relative(m_ptr->pos);

            while (TRUE)
            {
                char ch = inkey();

                if (ch == 'Y' || ch == 'y')
                {
                    delete_this = TRUE;

                    if (kakunin)
                    {
                        sprintf(buf, "Are you sure? (%s) ", friend_name);
                        if (!get_check(buf))
                            delete_this = FALSE;
                    }
                    break;
                }

                if (ch == 'U' || ch == 'u')
                {
                    all_pets = TRUE;
                    break;
                }

                if (ch == ESCAPE || ch == 'N' || ch == 'n')
                    break;

                bell();
            }
        }

        if ((all_pets && !kakunin) || (!all_pets && delete_this))
        {
            if (m_ptr->id == plr->riding)
            {
                msg_format("You have got off %s. ", friend_name);
                plr->riding = 0;
                plr->update |= (PU_BONUS | PU_MONSTERS);
                plr->redraw |= (PR_EXTRA | PR_HEALTH_BARS);
            }

            sprintf(buf, "Dismissed %s.", friend_name);

            msg_add(buf);
            plr->window |= (PW_MESSAGE);
            window_stuff();

            delete_monster(m_ptr);
            Dismissed++;
        }
    }

    Term->scr->cu = cu;
    Term->scr->cv = cv;
    Term_fresh();

    vec_free(pets);

    msg_format("You have dismissed %d pet%s.", Dismissed,
           (Dismissed == 1 ? "" : "s"));
    if (Dismissed == 0 && all_pets)
        msg_print("'U'nnamed means all your pets except named pets and your mount.");
}

bool player_can_ride_aux(point_t pos, bool now_riding)
{
    bool p_can_enter;
    bool old_character_xtra = character_xtra;
    int  old_riding = plr->riding;
    bool old_riding_ryoute = plr->riding_ryoute;
    bool old_old_riding_ryoute = plr->old_riding_ryoute;
    bool old_pf_ryoute = (plr->pet_extra_flags & PF_RYOUTE) ? TRUE : FALSE;
    dun_cell_ptr cell = dun_cell_at(cave, pos);
    mon_ptr mon = dun_mon_at(cave, pos);

    /* Hack -- prevent "icky" message */
    character_xtra = TRUE;

    if (now_riding && mon) plr->riding = mon->id;
    else
    {
        plr->riding = 0;
        plr->pet_extra_flags &= ~(PF_RYOUTE);
        plr->riding_ryoute = plr->old_riding_ryoute = FALSE;
    }

    calc_bonuses();

    p_can_enter = cell_allow_plr(cell);

    plr->riding = old_riding;
    if (old_pf_ryoute) plr->pet_extra_flags |= (PF_RYOUTE);
    else plr->pet_extra_flags &= ~(PF_RYOUTE);
    plr->riding_ryoute = old_riding_ryoute;
    plr->old_riding_ryoute = old_old_riding_ryoute;

    calc_bonuses();

    character_xtra = old_character_xtra;

    return p_can_enter;
}

bool rakuba(int dam, bool force)
{
    int i;
    point_t o;
    point_t s = {0};
    int sn = 0;
    char m_name[80];
    monster_type *m_ptr = dun_mon(cave, plr->riding);
    monster_race *r_ptr = m_ptr->race;
    bool fall_dam = FALSE;

    if (!plr->riding) return FALSE;
    if (plr->prace == RACE_MON_RING) return FALSE; /* cf ring_process_m instead ... */

    if (dam >= 0 || force)
    {
        if (!force)
        {
            int cur = skills_riding_current();
            int max = skills_riding_max();
            int rakubalevel = mon_race_lvl(r_ptr);
            if (plr->riding_ryoute) rakubalevel += 20;

            skills_riding_gain_rakuba(dam);

            #ifdef DEVELOPER
            if (0 || plr->wizard)
            {
                int r = dam/2 + rakubalevel*2;
                int n = cur/30 + 10;
                if (r > n)
                {
                    double p1 = (double)n/(double)r;
                    int    r2 = plr->lev*(plr->riding_ryoute ? 2 : 3) + 30;
                    double p2 = 1.0 - 1.0/(double)r2;
                    if (max == RIDING_EXP_MASTER && !plr->riding_ryoute) p2 = 1.0;
                    msg_format("<color:D>StayMounted = %.2f%% * %.2f%% = %.2f%%.</color>", 
                        p1*100., p2*100., p1*p2*100.);
                }
            }
            #endif

            if (randint0(dam / 2 + rakubalevel * 2) < cur / 30 + 10)
            {
                if (max == RIDING_EXP_MASTER && !plr->riding_ryoute)
                    return FALSE;
                if (!one_in_(plr->lev*(plr->riding_ryoute ? 2 : 3) + 30))
                    return FALSE;
            }
        }

        /* Check around the player */
        for (i = 0; i < 8; i++)
        {
            point_t pos = point_step(plr->pos, ddd[i]);
            dun_cell_ptr cell = dun_cell_at(cave, pos);
            mon_ptr mon = dun_mon_at(cave, pos);

            if (mon) continue;

            /* Skip non-empty grids */
            if (cell_is_wall(cell) || door_is_closed(cell))
            {
                if (!player_can_ride_aux(pos, FALSE)) continue;
            }

            if (cell->type == FEAT_PATTERN) continue;

            /* Count "safe" grids */
            sn++;

            /* Randomize choice */
            if (randint0(sn) > 0) continue;

            /* Save the safe location */
            s = pos;
        }
        if (!sn)
        {
            monster_desc(m_name, m_ptr, 0);
            msg_format("You have nearly fallen from %s, but bumped into wall.",m_name);
            take_hit(DAMAGE_NOESCAPE, mon_race_lvl(r_ptr)+3, "bumping into wall");
            return FALSE;
        }

        o = plr->pos;
        dun_move_plr(cave, s);
        draw_pos(o);
        viewport_verify();
    }

    plr->riding = 0;
    plr->pet_extra_flags &= ~(PF_RYOUTE);
    plr->riding_ryoute = plr->old_riding_ryoute = FALSE;

    calc_bonuses();

    plr->update |= (PU_BONUS);

    /* Update stuff */
    plr->update |= (PU_VIEW | PU_LIGHT | PU_FLOW | PU_MON_LIGHT | PU_MONSTERS);

    /* Window stuff */
    plr->window |= (PW_OVERHEAD | PW_DUNGEON);

    plr->redraw |= (PR_EXTRA);

    /* Update health track of mount */
    plr->redraw |= PR_HEALTH_BARS;

    if (plr->levitation && !force)
    {
        monster_desc(m_name, m_ptr, 0);
        msg_format("You are thrown from %s, but make a good landing.",m_name);
    }
    else
    {
        take_hit(DAMAGE_NOESCAPE, mon_race_lvl(r_ptr)+3, "Falling from riding");
        fall_dam = TRUE;
    }

    /* Move the player */
    if (s.y && !plr->is_dead)
        move_player_effect(plr->pos, MPE_DONT_PICKUP | MPE_DONT_SWAP_MON);

    return fall_dam;
}

bool do_riding(bool force)
{
    int dir = 0;
    point_t pos;
    dun_cell_ptr old_cell = dun_cell_at(cave, plr->pos);
    dun_cell_ptr cell;
    mon_ptr mon;

    if (!get_rep_dir2(&dir)) return FALSE;
    pos = point_step(plr->pos, dir);
    cell = dun_cell_at(cave, pos);
    mon = dun_mon_at(cave, pos);

    if (plr->special_defense & KATA_MUSOU) set_action(ACTION_NONE);

    if (plr->riding)
    {
        /* Skip non-empty grids */
        if (!player_can_ride_aux(pos, FALSE))
        {
            msg_print("You cannot go to that direction.");
            return FALSE;
        }

        if (!pattern_legal_move(old_cell, cell)) return FALSE;

        if (mon)
        {
            energy_use = 100;
            msg_print("There is a monster in the way!");
            plr_attack_normal(pos);
            return FALSE;
        }

        plr->riding = 0;
        plr->pet_extra_flags &= ~(PF_RYOUTE);
        plr->riding_ryoute = plr->old_riding_ryoute = FALSE;
    }
    else
    {
        int roll;

        if (plr_tim_find(T_CONFUSED))
        {
            msg_print("You are too confused!");
            return FALSE;
        }
        if (!mon || !mon->ml)
        {
            msg_print("There is no monster.");
            return FALSE;
        }
        if (!mon_is_pet(mon) && !force)
        {
            msg_print("That monster is not a pet.");
            return FALSE;
        }
        if (plr->prace == RACE_MON_RING)
        {
            if (!mon_is_type(mon->race, SUMMON_RING_BEARER))
            {
                msg_print("This monster is not a suitable ring bearer.");
                return FALSE;
            }
        }
        else
        {
            if (!mon_race_is_ridable(mon->race))
            {
                msg_print("This monster doesn't seem suitable for riding.");
                return FALSE;
            }
            if (warlock_is_(WARLOCK_DRAGONS) && !mon_is_dragon(mon))
            {
                msg_print("You are a dragon rider!");
                return FALSE;
            }
        }

        if (!pattern_legal_move(old_cell, cell)) return FALSE;

        if (!player_can_ride_aux(pos, TRUE))
        {
            cptr desc = cell_desc(cell);
            cptr join = "on";
            if (cell->type == FEAT_WALL && !wall_is_mountain(cell)) join = "in";
            if (!cell_los(cell) && !cell_is_tree(cell)) join = "in";
            msg_format("This monster is %s the %s.", join, desc);
            return FALSE;
        }
        roll = skills_riding_current()/50 + plr->lev/2 + 20;
        if (0 || plr->wizard)
        {
            int n = roll - mon_lvl(mon) + 1;
            msg_format("<color:D>Riding = %d.%d%%</color>", n * 100 /roll, (n * 1000 / roll) % 10);
        }
        if (plr->prace != RACE_MON_RING && mon_lvl(mon) > randint1(roll))
        {
            if (mon_lvl(mon) > roll) /* XXX spoil that it is hopeless! */
                msg_print("This monster is too powerful for you to ride!");
            else
                msg_print("You failed to ride.");

            energy_use = 100;
            return FALSE;
        }

        mon_tim_remove(mon, MT_SLEEP);
        if (plr->action == ACTION_KAMAE) set_action(ACTION_NONE);
        if (plr->action == ACTION_GLITTER) set_action(ACTION_NONE);

        plr->riding = mon->id;
        if (plr->riding == plr->health_who) health_track(NULL);
    }

    energy_use = 100;

    /* Mega-Hack -- Forget the view and lite */
    plr->update |= (PU_UN_VIEW | PU_UN_LIGHT);

    /* Update the monsters */
    plr->update |= (PU_BONUS);

    /* Redraw map */
    plr->redraw |= (PR_MAP | PR_EXTRA);

    plr->redraw |= PR_HEALTH_BARS;

    /* Move the player */
    move_player_effect(pos, MPE_HANDLE_STUFF | MPE_ENERGY_USE | MPE_DONT_PICKUP | MPE_DONT_SWAP_MON);

    return TRUE;
}

static void do_name_pet(void)
{
    monster_type *m_ptr;
    char out_val[20];
    char m_name[80];
    bool old_target_pet = target_pet;

    target_pet = TRUE;
    if (!target_set(TARGET_KILL) || !who_is_mon(plr->target))
    {
        target_pet = old_target_pet;
        return;
    }
    target_pet = old_target_pet;

    m_ptr = who_mon(plr->target);
    if (m_ptr)
    {
        if (!mon_is_pet(m_ptr))
        {
            /* Message */
            msg_format("This monster is not a pet.");
            return;
        }
        if (mon_is_unique(m_ptr))
        {
            msg_format("You cannot change name of this monster!");
            return;
        }
        monster_desc(m_name, m_ptr, 0);

        /* Message */
        msg_format("Name %s.", m_name);

        msg_print(NULL);

        /* Start with nothing */
        strcpy(out_val, "");

        /* Use old inscription */
        if (m_ptr->nickname)
        {
            /* Start with the old inscription */
            strcpy(out_val, quark_str(m_ptr->nickname));
        }

        /* Get a new inscription (possibly empty) */
        if (get_string("Name: ", out_val, 15))
        {
            if (out_val[0])
            {
                m_ptr->nickname = quark_add(out_val);
            }
            else
            {
                m_ptr->nickname = 0;
            }
        }
    }
}

/*
 * Issue a pet command
 */
static cptr _pet_tgt_desc(void)
{
    if (who_is_null(plr->pet_target)) return "nothing";
    if (plr_tim_find(T_HALLUCINATE)) return "something strange";
    if (who_is_mon(plr->pet_target))
    {
        mon_ptr mon = who_mon(plr->pet_target);
        if (mon->dun != cave)
        {
            static char buf[MAX_NLEN_MON];
            sprintf(buf, "%s (off level)", mon->apparent_race->name);
            return buf;
        }
        return mon->apparent_race->name;
    }
    plr->pet_target = who_create_null(); /* positional pet targets disallowed */
    return "nothing";
}

void do_cmd_pet(void)
{
    int            i = 0;
    int            num;
    int            powers[36];
    cptr            power_desc[36];
    bool            flag, redraw;
    int            ask = FALSE;
    char            choice;
    char            out_val[160];

    int mode = 0;

    char buf[160];
    char target_buf[160];

    int menu_line = use_menu ? 1 : 0;

    num = 0;

    power_desc[num] = "dismiss pets";

    powers[num++] = PET_DISMISS;

    sprintf(target_buf, "specify a target of pet (now:%s)", _pet_tgt_desc());
    power_desc[num] = target_buf;

    powers[num++] = PET_TARGET;

    power_desc[num] = "stay close";

    if (plr->pet_follow_distance == PET_CLOSE_DIST) mode = num;
    powers[num++] = PET_STAY_CLOSE;

    power_desc[num] = "follow me";

    if (plr->pet_follow_distance == PET_FOLLOW_DIST) mode = num;
    powers[num++] = PET_FOLLOW_ME;

    power_desc[num] = "seek and destroy";

    if (plr->pet_follow_distance == PET_DESTROY_DIST) mode = num;
    powers[num++] = PET_SEEK_AND_DESTROY;

    power_desc[num] = "give me space";

    if (plr->pet_follow_distance == PET_SPACE_DIST) mode = num;
    powers[num++] = PET_ALLOW_SPACE;

    power_desc[num] = "stay away";

    if (plr->pet_follow_distance == PET_AWAY_DIST) mode = num;
    powers[num++] = PET_STAY_AWAY;

    if (plr->pet_extra_flags & PF_OPEN_DOORS)
    {
        power_desc[num] = "pets open doors (now On)";
    }
    else
    {
        power_desc[num] = "pets open doors (now Off)";
    }
    powers[num++] = PET_OPEN_DOORS;

    if (plr->pet_extra_flags & PF_PICKUP_ITEMS)
    {
        power_desc[num] = "pets pick up items (now On)";
    }
    else
    {
        power_desc[num] = "pets pick up items (now Off)";
    }
    powers[num++] = PET_TAKE_ITEMS;

    if (plr->pet_extra_flags & PF_TELEPORT)
    {
        power_desc[num] = "allow teleport (now On)";
    }
    else
    {
        power_desc[num] = "allow teleport (now Off)";
    }
    powers[num++] = PET_TELEPORT;

    if (plr->pet_extra_flags & PF_ATTACK_SPELL)
    {
        power_desc[num] = "allow cast attack spell (now On)";
    }
    else
    {
        power_desc[num] = "allow cast attack spell (now Off)";
    }
    powers[num++] = PET_ATTACK_SPELL;

    if (plr->pet_extra_flags & PF_SUMMON_SPELL)
    {
        power_desc[num] = "allow cast summon spell (now On)";
    }
    else
    {
        power_desc[num] = "allow cast summon spell (now Off)";
    }
    powers[num++] = PET_SUMMON_SPELL;

    if (plr->pet_extra_flags & PF_BALL_SPELL)
    {
        power_desc[num] = "allow involve player in area spell (now On)";
    }
    else
    {
        power_desc[num] = "allow involve player in area spell (now Off)";
    }
    powers[num++] = PET_BALL_SPELL;

    if (plr->riding)
    {
        power_desc[num] = "get off a pet";
    }
    else
    {
        power_desc[num] = "ride a pet";
    }
    powers[num++] = PET_RIDING;

    power_desc[num] = "name pets";

    powers[num++] = PET_NAME;

    if (plr->riding && plr->prace != RACE_MON_RING)
    {
        /* TODO: We used to check weapons to see if 2-handed was an option ... */
        if (plr->pet_extra_flags & PF_RYOUTE)
            power_desc[num] = "use one hand to control a riding pet";
        else
            power_desc[num] = "use both hands for a weapon";

        powers[num++] = PET_RYOUTE;
    }

    if (plr->pet_extra_flags & PF_NO_BREEDING)
    {
        power_desc[num] = "no breeding (now On)";
    }
    else
    {
        power_desc[num] = "no breeding (now Off)";
    }
    powers[num++] = PET_NO_BREEDING;

    if (plr->pet_extra_flags & PF_HILITE)
    {
        power_desc[num] = "highlight pets (now On)";
    }
    else
    {
        power_desc[num] = "highlight pets (now Off)";
    }
    powers[num++] = PET_HILITE;

#ifdef ALLOW_REPEAT
    if (!(repeat_pull(&i) && (i >= 0) && (i < num)))
    {
#endif /* ALLOW_REPEAT */

    /* Nothing chosen yet */
    flag = FALSE;

    /* No redraw yet */
    redraw = FALSE;

    if (use_menu)
    {
        /* Save the screen */
        screen_save();

        /* Build a prompt */
        strnfmt(out_val, 78, "(Command, ESC=exit) Choose command from menu.");
    }
    else
    {
        /* Build a prompt */
        strnfmt(out_val, 78,
                "(Command %c-%c, *=List, ESC=exit) Select a command: ",
                I2A(0), I2A(num - 1));
    }

    choice = (always_show_list || use_menu) ? ESCAPE : 1;

    /* Get a command from the user */
    while (!flag)
    {
        if (choice == ESCAPE) choice = ' ';
        else if (!get_com(out_val, &choice, TRUE)) break;

        if (use_menu && (choice != ' '))
        {
            switch (choice)
            {
            case '0':
                screen_load();
                return;

            case '8':
            case 'k':
            case 'K':
                menu_line += (num - 1);
                break;

            case '2':
            case 'j':
            case 'J':
                menu_line++;
                break;

            case '4':
            case 'h':
            case 'H':
                menu_line = 1;
                break;

            case '6':
            case 'l':
            case 'L':
                menu_line = num;
                break;

            case 'x':
            case 'X':
            case '\r':
            case '\n':
                i = menu_line - 1;
                ask = FALSE;
                break;
            }
            if (menu_line > num) menu_line -= num;
        }

        /* Request redraw */
        if ((choice == ' ') || (choice == '*') || (choice == '?') || (use_menu && ask))
        {
            /* Show the list */
            if (!redraw || use_menu)
            {
                byte y = 1, x = 0;
                int ctr = 0;

                /* Show list */
                redraw = TRUE;

                /* Save the screen */
                if (!use_menu) screen_save();

                prt("", y++, x);

                /* Print list */
                for (ctr = 0; ctr < num; ctr++)
                {
                    /* Letter/number for power selection */
                    if (use_menu) sprintf(buf, "%c%s ", (ctr == mode) ? '*' : ' ', (ctr == (menu_line - 1)) ? "> " : "  ");
                    else sprintf(buf, "%c%c) ", (ctr == mode) ? '*' : ' ', I2A(ctr));

                    strcat(buf, power_desc[ctr]);

                    prt(buf, y + ctr, x);
                }

                prt("", y + MIN(ctr, 18), x);
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

        if (!use_menu)
        {
            /* Note verify */
            ask = (isupper(choice));

            /* Lowercase */
            if (ask) choice = tolower(choice);

            /* Extract request */
            i = (islower(choice) ? A2I(choice) : -1);
        }

        /* Totally Illegal */
        if ((i < 0) || (i >= num))
        {
            bell();
            continue;
        }

        /* Verify it */
        if (ask)
        {
            /* Prompt */
            strnfmt(buf, 78, "Use %s? ", power_desc[i]);

            /* Belay that order */
            if (!get_check(buf)) continue;
        }

        /* Stop the loop */
        flag = TRUE;
    }

    /* Restore the screen */
    if (redraw) screen_load();

    /* Abort if needed */
    if (!flag)
    {
        energy_use = 0;
        return;
    }

#ifdef ALLOW_REPEAT
    repeat_push(i);
    }
#endif /* ALLOW_REPEAT */

    switch (powers[i])
    {
        case PET_DISMISS: /* Dismiss pets */
        {
            if (mon_pack_count(plr_pack()))
            {
                do_cmd_pet_dismiss();
                calculate_upkeep();
            }
            else msg_print("You have no pets!");
            break;
        }
        case PET_TARGET:
        {
            project_length = -1;
            target_pet = FALSE;
            if (!target_set(TARGET_MARK))
                plr->pet_target = who_create_null();
            else
            {
                plr->pet_target = plr->target;
                if (who_is_mon(plr->pet_target))
                    mon_set_hunted(who_mon(plr->pet_target)); /* flow */
            }
            project_length = 0;

            break;
        }
        /* Call pets */
        case PET_STAY_CLOSE:
        {
            plr->pet_follow_distance = PET_CLOSE_DIST;
            plr->pet_target = who_create_null();
            break;
        }
        /* "Follow Me" */
        case PET_FOLLOW_ME:
        {
            plr->pet_follow_distance = PET_FOLLOW_DIST;
            plr->pet_target = who_create_null();
            break;
        }
        /* "Seek and destoy" */
        case PET_SEEK_AND_DESTROY:
        {
            plr->pet_follow_distance = PET_DESTROY_DIST;
            break;
        }
        /* "Give me space" */
        case PET_ALLOW_SPACE:
        {
            plr->pet_follow_distance = PET_SPACE_DIST;
            break;
        }
        /* "Stay away" */
        case PET_STAY_AWAY:
        {
            plr->pet_follow_distance = PET_AWAY_DIST;
            break;
        }
        /* flag - allow pets to open doors */
        case PET_OPEN_DOORS:
        {
            if (plr->pet_extra_flags & PF_OPEN_DOORS) plr->pet_extra_flags &= ~(PF_OPEN_DOORS);
            else plr->pet_extra_flags |= (PF_OPEN_DOORS);
            break;
        }
        /* flag - allow pets to pickup items */
        case PET_TAKE_ITEMS:
        {
            if (plr->pet_extra_flags & PF_PICKUP_ITEMS)
            {
                mon_pack_iter(plr_pack(), mon_drop_carried_obj);
                plr->pet_extra_flags &= ~(PF_PICKUP_ITEMS);
            }
            else plr->pet_extra_flags |= (PF_PICKUP_ITEMS);

            break;
        }
        /* flag - allow pets to teleport */
        case PET_TELEPORT:
        {
            if (plr->pet_extra_flags & PF_TELEPORT) plr->pet_extra_flags &= ~(PF_TELEPORT);
            else plr->pet_extra_flags |= (PF_TELEPORT);
            break;
        }
        /* flag - allow pets to cast attack spell */
        case PET_ATTACK_SPELL:
        {
            if (plr->pet_extra_flags & PF_ATTACK_SPELL) plr->pet_extra_flags &= ~(PF_ATTACK_SPELL);
            else plr->pet_extra_flags |= (PF_ATTACK_SPELL);
            break;
        }
        /* flag - allow pets to cast attack spell */
        case PET_SUMMON_SPELL:
        {
            if (plr->pet_extra_flags & PF_SUMMON_SPELL) plr->pet_extra_flags &= ~(PF_SUMMON_SPELL);
            else plr->pet_extra_flags |= (PF_SUMMON_SPELL);
            break;
        }
        /* flag - allow pets to cast attack spell */
        case PET_BALL_SPELL:
        {
            if (plr->pet_extra_flags & PF_BALL_SPELL) plr->pet_extra_flags &= ~(PF_BALL_SPELL);
            else plr->pet_extra_flags |= (PF_BALL_SPELL);
            break;
        }

        case PET_RIDING:
        {
            (void)do_riding(FALSE);
            break;
        }

        case PET_NAME:
        {
            do_name_pet();
            break;
        }

        case PET_RYOUTE:
        {
            if (plr->pet_extra_flags & PF_RYOUTE) plr->pet_extra_flags &= ~(PF_RYOUTE);
            else plr->pet_extra_flags |= (PF_RYOUTE);
            plr->update |= (PU_BONUS);
            handle_stuff();
            break;
        }
        case PET_NO_BREEDING:
        {
            if (plr->pet_extra_flags & PF_NO_BREEDING) plr->pet_extra_flags &= ~(PF_NO_BREEDING);
            else plr->pet_extra_flags |= PF_NO_BREEDING;
            break;
        }
        case PET_HILITE:
        {
            if (plr->pet_extra_flags & PF_HILITE) plr->pet_extra_flags &= ~(PF_HILITE);
            else plr->pet_extra_flags |= PF_HILITE;
            plr->redraw |= PR_MAP;
            break;
        }
    }
}
