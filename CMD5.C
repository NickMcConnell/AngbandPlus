/* File: cmd5.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"



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
static int get_spell(int *sn, cptr prompt, int sval, bool known)
{
    int i;

    int spell = -1;
    int num = 0;

    byte spells[64];

    int ver;

    bool flag, redraw, okay;
    char choice;

    magic_type *s_ptr;

    char out_val[160];

    cptr p = ((mp_ptr->spell_book == TV_MAGIC_BOOK) ? "spell" : "prayer");


    /* Extract spells */
    for (spell = 0; spell < 64; spell++)
    {
        /* Check for this spell */
        if ((spell < 32) ?
            (spell_flags[mp_ptr->spell_type][sval][0] & (1L << spell)) :
            (spell_flags[mp_ptr->spell_type][sval][1] & (1L << (spell - 32))))
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
        if (spell_okay(spells[i], known)) okay = TRUE;
    }

    /* No "okay" spells */
    if (!okay) return (FALSE);


    /* Assume cancelled */
    *sn = (-1);

    /* Nothing chosen yet */
    flag = FALSE;

    /* No redraw yet */
    redraw = FALSE;

#if 0
    /* Show the list */
    if (redraw)
    {
        /* Save screen */
        screen_save();

        /* Display a list of spells */
        print_spells(spells, num, 1, 20);
    }

#endif


    /* Build a prompt (accept all spells) */
    strnfmt(out_val, 78, "(%^ss %c-%c, *=List, ESC=exit) %^s which %s? ",
            p, I2A(0), I2A(num - 1), prompt, p);

    /* Get a spell from the user */
    while (!flag && get_com(out_val, &choice))
    {
        /* Request redraw */
        if ((choice == ' ') || (choice == '*') || (choice == '?'))
        {
            /* Hide the list */
            if (redraw)
            {
                /* Load screen */
                screen_load();

                /* Hide list */
                redraw = FALSE;
            }

            /* Show the list */
            else
            {
                /* Show list */
                redraw = TRUE;

                /* Save screen */
                screen_save();

                /* Display a list of spells */
                print_spells(spells, num, 1, 20);
            }

            /* Ask again */
            continue;
        }


        /* Note verify */
        ver = (isupper(choice));

        /* Lowercase */
        choice = tolower(choice);

        /* Extract request */
        i = (islower(choice) ? A2I(choice) : -1);

        /* Totally Illegal */
        if ((i < 0) || (i >= num))
        {
            bell("Illegal spell choice!");
            continue;
        }

        /* Save the spell index */
        spell = spells[i];

        /* Require "okay" spells */
        if (!spell_okay(spell, known))
        {
            bell("Illegal spell choice!");
            msg_format("You may not %s that %s.", prompt, p);
            continue;
        }

        /* Verify it */
        if (ver)
        {
            char tmp_val[160];

            /* Access the spell */
            s_ptr = &mp_ptr->info[spell];

            /* Prompt */
            strnfmt(tmp_val, 78, "%^s %s (%d mana, %d%% fail)? ",
                    prompt, spell_names[mp_ptr->spell_type][spell],
                    s_ptr->smana, spell_chance(spell));

            /* Belay that order */
            if (!get_check(tmp_val)) continue;
        }

        /* Stop the loop */
        flag = TRUE;
    }


    /* Restore the screen */
    if (redraw)
    {
        /* Load screen */
        screen_load();

        /* Hack -- forget redraw */
        /* redraw = FALSE; */
    }


    /* Abort if needed */
    if (!flag) return (FALSE);

    /* Save the choice */
    (*sn) = spell;

    /* Success */
    return (TRUE);
}




/*
 * Peruse the spells/prayers in a Book
 *
 * Note that *all* spells in the book are listed
 *
 * Note that browsing is allowed while confused or blind,
 * and in the dark, primarily to allow browsing in stores.
 */
void do_cmd_browse(void)
{
    int item, sval;

    int spell = -1;
    int num = 0;

    byte spells[64];

    object_type *o_ptr;

    cptr q, s;


    /* Warriors are illiterate */
    if (!mp_ptr->spell_book)
    {
        msg_print("You cannot read books!");
        return;
    }

#if 0

    /* No lite */
    if (p_ptr->blind || no_lite())
    {
        msg_print("You cannot see!");
        return;
    }

    /* Confused */
    if (p_ptr->confused)
    {
        msg_print("You are too confused!");
        return;
    }

#endif

    /* Restrict choices to "useful" books */
    item_tester_tval = mp_ptr->spell_book;

    /* Get an item */
    q = "Browse which book? ";
    s = "You have no books that you can read.";
    if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

    /* Get the item (in the pack) */
    if (item >= 0)
    {
        o_ptr = &inventory[item];
    }

    /* Get the item (on the floor) */
    else
    {
        o_ptr = &o_list[0 - item];
    }

    /* Access the item's sval */
    sval = o_ptr->sval;


    /* Track the object kind */
    object_kind_track(o_ptr->k_idx);

    /* Hack -- Handle stuff */
    handle_stuff();


    /* Extract spells */
    for (spell = 0; spell < 64; spell++)
    {
        /* Check for this spell */
        if ((spell < 32) ?
            (spell_flags[mp_ptr->spell_type][sval][0] & (1L << spell)) :
            (spell_flags[mp_ptr->spell_type][sval][1] & (1L << (spell - 32))))
        {
            /* Collect this spell */
            spells[num++] = spell;
        }
    }


    /* Save screen */
    screen_save();

    /* Display the spells */
    print_spells(spells, num, 1, 20);

    /* Prompt for a command */
    put_str("(Browsing) Command: ", 0, 0);

        /* Hack -- Get a new command */
        p_ptr->command_new = inkey();

    /* Load screen */
    screen_load();


    /* Hack -- Process "Escape" */
    if (p_ptr->command_new == ESCAPE)
    {
        /* Reset stuff */
        p_ptr->command_new = 0;
    }
}




/*
 * Study a book to gain a new spell/prayer
 */
void do_cmd_study(void)
{
    int i, item, sval;

    int spell = -1;

    cptr p = ((mp_ptr->spell_book == TV_MAGIC_BOOK) ? "spell" : "prayer");

    cptr q, s;

    object_type *o_ptr;


    if (!mp_ptr->spell_book)
    {
        msg_print("You cannot read books!");
        return;
    }

    if (p_ptr->blind || no_lite())
    {
        msg_print("You cannot see!");
        return;
    }

    if (p_ptr->confused)
    {
        msg_print("You are too confused!");
        return;
    }

    if (!(p_ptr->new_spells))
    {
        msg_format("You cannot learn any new %ss!", p);
        return;
    }


    /* Restrict choices to "useful" books */
    item_tester_tval = mp_ptr->spell_book;

    /* Get an item */
    q = "Study which book? ";
    s = "You have no books that you can read.";
    if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

    /* Get the item (in the pack) */
    if (item >= 0)
    {
        o_ptr = &inventory[item];
    }

    /* Get the item (on the floor) */
    else
    {
        o_ptr = &o_list[0 - item];
    }

    /* Access the item's sval */
    sval = o_ptr->sval;


    /* Track the object kind */
    object_kind_track(o_ptr->k_idx);

    /* Hack -- Handle stuff */
    handle_stuff();


    /* Mage -- Learn a selected spell */
    if (mp_ptr->spell_book == TV_MAGIC_BOOK)
    {
        /* Ask for a spell, allow cancel */
        if (!get_spell(&spell, "study", sval, FALSE) && (spell == -1)) return;
    }

    /* Priest -- Learn a random prayer */
    if (mp_ptr->spell_book == TV_PRAYER_BOOK)
    {
        int k = 0;

        int gift = -1;

        /* Extract spells */
        for (spell = 0; spell < 64; spell++)
        {
            /* Check spells in the book */
            if ((spell < 32) ?
                (spell_flags[mp_ptr->spell_type][sval][0] & (1L << spell)) :
                (spell_flags[mp_ptr->spell_type][sval][1] & (1L << (spell - 32))))
            {
                /* Skip non "okay" prayers */
                if (!spell_okay(spell, FALSE)) continue;

                /* Apply the randomizer */
                if ((++k > 1) && (rand_int(k) != 0)) continue;

                /* Track it */
                gift = spell;
            }
        }

        /* Accept gift */
        spell = gift;
    }

    /* Nothing to study */
    if (spell < 0)
    {
        /* Message */
        msg_format("You cannot learn any %ss in that book.", p);

        /* Abort */
        return;
    }


    /* Take a turn */
    p_ptr->energy_use = 100;

    /* Learn the spell */
    if (spell < 32)
    {
        p_ptr->spell_learned1 |= (1L << spell);
    }
    else
    {
        p_ptr->spell_learned2 |= (1L << (spell - 32));
    }

    /* Find the next open entry in "spell_order[]" */
    for (i = 0; i < 64; i++)
    {
        /* Stop at the first empty space */
        if (p_ptr->spell_order[i] == 99) break;
    }

    /* Add the spell to the known list */
    p_ptr->spell_order[i++] = spell;

    /* Mention the result */
    msg_format("You have learned the %s of %s.",
               p, spell_names[mp_ptr->spell_type][spell]);

    /* Sound */
    sound(SOUND_STUDY);

    /* One less spell available */
    p_ptr->new_spells--;

    /* Message if needed */
    if (p_ptr->new_spells)
    {
        /* Message */
        msg_format("You can learn %d more %s%s.",
                   p_ptr->new_spells, p,
                   (p_ptr->new_spells != 1) ? "s" : "");
    }

    /* Save the new_spells value */
    p_ptr->old_spells = p_ptr->new_spells;

    /* Redraw Study Status */
    p_ptr->redraw |= (PR_STUDY);
}



/*
 * Cast a spell
 */
void do_cmd_cast(void)
{
    int py = p_ptr->py;
    int px = p_ptr->px;

    int item, sval, spell, dir;
    int chance, beam;

    int plev = p_ptr->lev;

    object_type *o_ptr;

    magic_type *s_ptr;

    cptr q, s;


    /* Require spell ability */
    if (mp_ptr->spell_book != TV_MAGIC_BOOK)
    {
        msg_print("You cannot cast spells!");
        return;
    }

    /* Require lite */
    if (p_ptr->blind || no_lite())
    {
        msg_print("You cannot see!");
        return;
    }

    /* Not when confused */
    if (p_ptr->confused)
    {
        msg_print("You are too confused!");
        return;
    }


    /* Restrict choices to spell books */
    item_tester_tval = mp_ptr->spell_book;

    /* Get an item */
    q = "Use which book? ";
    s = "You have no spell books!";
    if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

    /* Get the item (in the pack) */
    if (item >= 0)
    {
        o_ptr = &inventory[item];
    }

    /* Get the item (on the floor) */
    else
    {
        o_ptr = &o_list[0 - item];
    }

    /* Access the item's sval */
    sval = o_ptr->sval;


    /* Track the object kind */
    object_kind_track(o_ptr->k_idx);

    /* Hack -- Handle stuff */
    handle_stuff();


    /* Ask for a spell */
    if (!get_spell(&spell, "cast", sval, TRUE))
    {
        if (spell == -2) msg_print("You don't know any spells in that book.");
        return;
    }


    /* Access the spell */
    s_ptr = &mp_ptr->info[spell];


    /* Verify "dangerous" spells */
    if (s_ptr->smana > p_ptr->csp)
    {
        /* Warning */
        msg_print("You do not have enough mana to cast this spell.");

        /* Verify */
        if (!get_check("Attempt it anyway? ")) return;
    }


    /* Spell failure chance */
    chance = spell_chance(spell);

    /* Failed spell */
    if (rand_int(100) < chance)
    {
        if (flush_failure) flush();
        msg_print("You failed to get the spell off!");
    }

    /* Process spell */
    else
    {
        /* Hack -- chance of "beam" instead of "bolt" */
        beam = ((p_ptr->pclass == 1) ? plev : (plev / 2));

        /* Spells.  */
        switch (spell)
        {
            /* Book 1:
             * Magic Missile */
            case 0:
            {
                if (!get_aim_dir(&dir)) return;
                fire_bolt_or_beam(beam-10, GF_MISSILE, dir,
                                  damroll(3 + ((plev - 1) / 5), 4));
                break;
            }

            /* Detect Monsters I */
            case 1:
            {
                /* With 20% miss rate */
                (void)detect_monsters_normal(20);
                break;
            }

            /* Detect gold */
            case 2:
            {
                (void)detect_treasure();
                (void)detect_objects_gold();
                break;
            }

            /* Light area */
            case 3:
            {
                (void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
                break;
            }

            /* Cure light wounds */
            case 4:
            {
                (void)hp_player(damroll(2, 8));
                (void)set_cut(p_ptr->cut - 15);
                break;
            }

            /* Object detection */
            case 5:
            {
                (void)detect_objects_normal();
                break;
            }

            /* Detect traps/doors I */
            case 6:
            {
                (void)detect_traps(10);
                (void)detect_doors(10);
                (void)detect_stairs(10);
                break;
            }

            /* Stinking cloud */
            case 7:
            {
                if (!get_aim_dir(&dir)) return;
                fire_ball(GF_POIS, dir,
                          10 + (plev / 2), (plev / 20) + 2);
                break;
            }

            /* Book 2:
             * Sleep I */
            case 8:
            {
                if (!get_aim_dir(&dir)) return;
                (void)sleep_monster(dir);
                break;
            }

            /* Disarm traps/doors */
            case 9:
            {
                if (!get_aim_dir(&dir)) return;
                disarm_trap(dir);
                break;
            }

            /* Elec bolt */
            case 10:
            {
                if (!get_aim_dir(&dir)) return;
                fire_bolt_or_beam(beam-10, GF_ELEC, dir,
                                  damroll(3+(plev/4), 8));
                break;
            }

            /* Slow poison */
            case 11:
            {
                set_poisoned(p_ptr->poisoned / 2);
                break;
            }

            /* Phase Door */
            case 12:
            {
                teleport_player(10);
                break;
            }

            /* Spear of light */
            case 13:
            {
                if (!get_aim_dir(&dir)) return;
                msg_print("A line of blue shimmering light appears.");
                lite_line(dir);
                break;
            }

            /* Cone of cold -LM- */
            case 14:
            {
                if (!get_aim_dir(&dir)) return;
                fire_arc(GF_COLD, dir, 20 + plev, 10 + plev / 10, 45);
                break;
            }

            /* Confuse Monster */
            case 15:
            {
                if (!get_aim_dir(&dir)) return;
                (void)confuse_monster(dir, plev);
                break;
            }

            /* Book 3:
             * Striking */
            case 16:
            {
                if (!get_aim_dir(&dir)) return;
                fire_bolt(GF_FORCE, dir,
                                  damroll(3+(plev/3), 9));
                break;
            }

            /* Identify */
            case 17:
            {
                (void)ident_spell();
                break;
            }

            /* Door creation */
            case 18:
            {
                (void)door_creation();
                break;
            }

            /* Recharge I */
            case 19:
            {
                (void)recharge(40);
                break;
            }

            /* Fire Bolt */
            case 20:
            {
                if (!get_aim_dir(&dir)) return;
                fire_bolt_or_beam(beam, GF_FIRE, dir,
                                  damroll(8+(plev/4), 8));
                break;
            }

            /* Mass Sleep */
            case 21:
            {
                (void)sleep_monsters();
                break;
            }

            /* Teleport I */
            case 22:
            {
                teleport_player(20);
                break;
            }

            /* Satisfy hunger */
            case 23:
            {
                (void)set_food(PY_FOOD_MAX - 1);
                break;
            }

            /* Book 4:
             * Temporary invisibility */
            case 24:
            {
                set_temp_invis(p_ptr->temp_invis + damroll(4, 4));
                break;
            }

            /* Cold ball */
            case 25:
            {
                if (!get_aim_dir(&dir)) return;
                fire_ball(GF_COLD, dir,
                          30 + (2 * (plev)), 2);
                break;
            }

            /* Hold foe */
            case 26:
            {
                if (!get_aim_dir(&dir)) return;
                fire_bolt(GF_PARALYSE, dir,
                                  damroll(4, 4));
                break;
            }

            /* Haste self */
            case 27:
            {
                if (!p_ptr->fast)
                {
                    (void)set_fast(randint(20) + plev);
                }
                else
                {
                    (void)set_fast(p_ptr->fast + randint(5));
                }
                break;
            }

            /* Teleport other */
            case 28:
            {
                if (!get_aim_dir(&dir)) return;
                (void)teleport_monster(dir);
                break;
            }

            /* Fireball */
            case 29:
            {
                if (!get_aim_dir(&dir)) return;
                fire_ball(GF_FIRE, dir,
                          75 + (2 * (plev)), 2);
                break;
            }

            /* Word of destruction */
            case 30:
            {
                destroy_area(py, px, 15, TRUE);
                break;
            }

            /* Plasma Storm */
            case 31:
            {
                if (!get_aim_dir(&dir)) return;
                fire_ball(GF_PLASMA, dir,
                          5 * (plev), 3);
                break;
            }

            /* Resistance of Scarabtarces:
             * Resist Fire */
            case 32:
            {
                (void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
                break;
            }

            /* Resist Cold */
            case 33:
            {
                (void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
                break;
            }

            /* Resist Acid & Elec */
            case 34:
            {
                (void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20);
                (void)set_oppose_elec(p_ptr->oppose_elec + randint(20) + 20);
                break;
            }

            /* Resist Poison */
            case 35:
            {
                (void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20);
                break;
            }

            /* Cure Poison */
            case 36:
            {
                (void)set_poisoned(0);
                break;
            }

            /* Resistance */
            case 37:
            {
                (void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20);
                (void)set_oppose_elec(p_ptr->oppose_elec + randint(20) + 20);
                (void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
                (void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
                (void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20);
                break;
            }


            /* Mordenkainen's Escapes:
             * Stair creation */
            case 38:
            {
                (void)stair_creation();
                break;
            }

            /* Teleport II */
            case 39:
            {
                teleport_player(30);
                break;
            }

            /* Confusion */
            case 40:
            {
                confuse_monsters(randint(10)+5);
                break;
            }

            /* Earthquake */
            case 41:
            {
                earthquake(py, px, 10);
                break;
            }

            /* Word of recall */
            case 42:
            {
                if (!p_ptr->word_recall && p_ptr->ethereal_lock == 0)
                {
                    p_ptr->word_recall = rand_int(20) + 15;
                    msg_print("The air about you becomes charged...");
                }
                else
                {
                    p_ptr->word_recall = 0;
                    msg_print("A tension leaves the air around you...");
                }
                break;
            }

            /* Create wall */
            case 43:
            {
                if (!get_aim_dir(&dir)) return;
                fire_arc(GF_MAKE_WALL, dir, 0, 1, 45);
                break;
            }

            /* Kelek's Grimoire:
             * Detect Enchantment */
            case 44:
            {
                (void)detect_objects_magic();
                break;
            }

            /* Magic Mapping */
            case 45:
            {
                map_area();
                break;
            }

            /* Detect Monsters II */
            case 46:
            {
                (void)detect_monsters_normal(0);
                break;
            }

            /* Recharge II */
            case 47:
            {
                recharge(100);
                break;
            }

            /* Mana Bolt */
            case 48:
            {
                if (!get_aim_dir(&dir)) return;
                fire_bolt(GF_MANA, dir, damroll(plev / 2, 10));

                break;
            }

            /* Scatter Foe */
            case 49:
            {
                (void)scattering();
                break;
            }

            /* Tenser's Transformations:
             * Heroism */
            case 50:
            {
                (void)hp_player(10);
                (void)set_hero(p_ptr->hero + randint(25) + 25);
                (void)set_afraid(0);
                break;
            }

            /* Slow monsters */
            case 51:
            {
                if (!get_aim_dir(&dir)) return;
                (void)slow_monster(dir);
                break;
            }

            /* Shield */
            case 52:
            {
                (void)set_shield(p_ptr->shield + randint(20) + 30);
                break;
            }

            /* Stone to mud */
            case 53:
            {
                if (!get_aim_dir(&dir)) return;
                (void)wall_to_mud(dir);
                break;
            }

            /* Chaotic change */
            case 54:
            {
                if (!get_aim_dir(&dir)) return;
                (void)poly_monster(dir);
                break;
            }

            /* Globe of Resilience */
            case 55:
            {
                (void)set_invuln(p_ptr->invuln + randint(8) + 6);
                break;
            }


            /* Raal's Tomes:
             * Acid Bolt */
            case 56:
            {
                if (!get_aim_dir(&dir)) return;
                fire_bolt_or_beam(beam, GF_ACID, dir,
                                  damroll(8+(plev/4), 8));
                break;
            }

            /* Acid ball */
            case 57:
            {
                if (!get_aim_dir(&dir)) return;
                fire_ball(GF_ACID, dir,
                          4 * (plev), 2);
                break;
            }

            /* Ice Storm */
            case 58:
            {
                if (!get_aim_dir(&dir)) return;
                fire_ball(GF_COLD, dir,
                          5 * (plev), 3);
                break;
            }

            /* Meteor Swarm */
            case 59:
            {
                if (!get_aim_dir(&dir)) return;
                fire_ball(GF_METEOR, dir,
                          7 * (plev), 3);
                break;
            }

            /* Storm of Destruction */
            case 60:
            {
                great_storm(200);
                break;
            }

            /* Mana Storm */
            case 61:
            {
                if (!get_aim_dir(&dir)) return;
                fire_ball(GF_MANA, dir,
                          300 + (plev * 4), 3);
                break;
            }



        }

        /* A spell was cast */
        if (!((spell < 32) ?
              (p_ptr->spell_worked1 & (1L << spell)) :
              (p_ptr->spell_worked2 & (1L << (spell - 32)))))
        {
            int e = s_ptr->sexp;

            /* The spell worked */
            if (spell < 32)
            {
                p_ptr->spell_worked1 |= (1L << spell);
            }
            else
            {
                p_ptr->spell_worked2 |= (1L << (spell - 32));
            }

            /* Gain experience */
            gain_exp(e * s_ptr->slevel);
        }
    }

    /* Take a turn */
    p_ptr->energy_use = 100;

    /* Sufficient mana */
    if (s_ptr->smana <= p_ptr->csp)
    {
        /* Use some mana */
        p_ptr->csp -= s_ptr->smana;
    }

    /* Over-exert the player */
    else
    {
        int oops = s_ptr->smana - p_ptr->csp;

        /* No mana left */
        p_ptr->csp = 0;
        p_ptr->csp_frac = 0;

        /* Message */
        msg_print("You faint from the effort!");

        /* Hack -- Bypass free action */
        (void)set_paralyzed(p_ptr->paralyzed + randint(5 * oops + 1));

        /* Damage CON (possibly permanently) */
        if (rand_int(100) < 50)
        {
            bool perm = (rand_int(100) < 25);

            /* Message */
            msg_print("You have damaged your health!");

            /* Reduce constitution */
            (void)dec_stat(A_CON, 15 + randint(10), perm);
        }
    }

    /* Redraw mana */
    p_ptr->redraw |= (PR_MANA);

    /* Window stuff */
    p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
}


/*
 * Enchant some missiles
 */
static void brand_missile(void)
{
    int i;

    /* Use the first acceptable missile */
    for (i = 0; i < INVEN_PACK; i++)
    {
        object_type *o_ptr = &inventory[i];

        /* Skip non-bolts */
        if (o_ptr->tval != TV_BOLT &&
            o_ptr->tval != TV_ARROW &&
            o_ptr->tval != TV_SHOT) continue;

        /* Skip artifacts and ego-items */
        if (artifact_p(o_ptr) || ego_item_p(o_ptr)) continue;

        /* Skip cursed/broken items */
        if (cursed_p(o_ptr) || broken_p(o_ptr)) continue;

        /* Randomize */
        if (rand_int(100) < 75) continue;

        /* Message */
        msg_print("Your missiles are covered in a holy aura!");

        /* Ego-item */
        o_ptr->name2 = EGO_HURT_EVIL;

        /* Enchant */
        enchant(o_ptr, rand_int(3) + 4, ENCH_TOHIT | ENCH_TODAM);

        /* Notice */
        return;
    }

    /* Flush */
    if (flush_failure) flush();

    /* Fail */
    msg_print("The holy enchantment failed.");

    /* Notice */
    return;
}

/*
 * Pray a prayer
 */
void do_cmd_pray(void)
{
    int py = p_ptr->py;
    int px = p_ptr->px;

    int item, sval, spell, dir, chance;

    int plev = p_ptr->lev;

    object_type *o_ptr;

    magic_type *s_ptr;

    cptr q, s;


    /* Must use prayer books */
    if (mp_ptr->spell_book != TV_PRAYER_BOOK)
    {
        msg_print("Pray hard enough and your prayers may be answered.");
        return;
    }

    /* Must have lite */
    if (p_ptr->blind || no_lite())
    {
        msg_print("You cannot see!");
        return;
    }

    /* Must not be confused */
    if (p_ptr->confused)
    {
        msg_print("You are too confused!");
        return;
    }


    /* Restrict choices */
    item_tester_tval = mp_ptr->spell_book;

    /* Get an item */
    q = "Use which book? ";
    s = "You have no prayer books!";
    if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

    /* Get the item (in the pack) */
    if (item >= 0)
    {
        o_ptr = &inventory[item];
    }

    /* Get the item (on the floor) */
    else
    {
        o_ptr = &o_list[0 - item];
    }

    /* Access the item's sval */
    sval = o_ptr->sval;


    /* Track the object kind */
    object_kind_track(o_ptr->k_idx);

    /* Hack -- Handle stuff */
    handle_stuff();


    /* Choose a spell */
    if (!get_spell(&spell, "recite", sval, TRUE))
    {
        if (spell == -2) msg_print("You don't know any prayers in that book.");
        return;
    }


    /* Access the spell */
    s_ptr = &mp_ptr->info[spell];


    /* Verify "dangerous" prayers */
    if (s_ptr->smana > p_ptr->csp)
    {
        /* Warning */
        msg_print("You do not have enough mana to recite this prayer.");

        /* Verify */
        if (!get_check("Attempt it anyway? ")) return;
    }


    /* Spell failure chance */
    chance = spell_chance(spell);

    /* Check for failure */
    if (rand_int(100) < chance)
    {
        if (flush_failure) flush();
        msg_print("You failed to concentrate hard enough!");
    }

    /* Success */
    else
    {
        switch (spell)
        {
            /* Book 1:
             * Detect Evil */
            case 0:
            {
                (void)detect_monsters_evil();
                break;
            }

            /* Cure Light Wounds */
            case 1:
            {
                (void)hp_player(damroll(2, 10));
                (void)set_cut(p_ptr->cut - 10);
                break;
            }

            /* Bless */
            case 2:
            {
                (void)set_blessed(p_ptr->blessed + randint(12) + 12);
                break;
            }

            /* Remove Fear */
            case 3:
            {
                (void)set_afraid(0);
                break;
            }

            /* Call Light */
            case 4:
            {
                (void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
                break;
            }

            /* Spiritual Hammer */
            case 5:
            {
                if (!get_aim_dir(&dir)) return;
                fire_bolt(GF_HOLY_ORB, dir, damroll(3,3));
                break;
            }

            /* Detect Doors/Traps */
            case 6:
            {
                /* 10% fail for the moment... */
                (void)detect_doors(10);
                (void)detect_stairs(10);
                (void)detect_traps(10);
                break;
            }

            /* Slow poison */
            case 7:
            {
                (void)set_poisoned(p_ptr->poisoned / 2);
                break;
            }

            /* Book 2:
             * Scare Monster */
            case 8:
            {
                if (!get_aim_dir(&dir)) return;
                (void)fear_monster(dir, plev);
                break;
            }

            /* Portal */
            case 9:
            {
                teleport_player(20);
                break;
            }

            /* Cure Serious */
            case 10:
            {
                (void)hp_player(damroll(4, 10));
                (void)set_cut((p_ptr->cut / 2) - 20);
                break;
            }

            /* Chant */
            case 11:
            {
                (void)set_blessed(p_ptr->blessed + randint(24) + 24);
                break;
            }

            /* Sanctuary */
            case 12:
            {
                (void)sleep_monsters_touch();
                break;
            }

            /* Sense Surroundings */
            case 13:
            {
                map_area();
                break;
            }

            /* Remove Curse */
            case 14:
            {
                remove_curse();
                break;
            }

            /* Resist Heat/Cold */
            case 15:
            {
                (void)set_oppose_fire(p_ptr->oppose_fire + randint(10) + 10);
                (void)set_oppose_cold(p_ptr->oppose_cold + randint(10) + 10);
                break;
            }

            /* Book 3:
             * Satisfy Hunger */
            case 16:
            {
                (void)set_food(PY_FOOD_MAX - 1);
                break;
            }

            /* Orb of Draining */
            case 17:
            {
                if (!get_aim_dir(&dir)) return;
                fire_ball(GF_HOLY_ORB, dir,
                          (damroll(3, 6) + plev +
                           (plev / ((p_ptr->pclass == CLASS_PRIEST) ? 2 : 4))),
                          ((plev < 30) ? 2 : 3));
                break;
            }

            /* Cure Critical Wounds */
            case 18:
            {
                (void)hp_player(damroll(6, 10));
                (void)set_cut(0);
                break;
            }

            /* Sense Invisible */
            case 19:
            {
                (void)set_tim_invis(p_ptr->tim_invis + randint(24) + 24);
                break;
            }

            /* Protection from Evil */
            case 20:
            {
                (void)set_protevil(p_ptr->protevil + randint(25) + 3 * p_ptr->lev);
                break;
            }

            /* Earthquake */
            case 21:
            {
                earthquake(py, px, 10);
                break;
            }

            /* Cure Mortal Wounds */
            case 22:
            {
                (void)hp_player(damroll(8, 10));
                (void)set_stun(0);
                (void)set_cut(0);
                break;
            }

            /* Turn Undead */
            case 23:
            {
                (void)turn_undead();
                break;
            }

            /* Book 4:
             * Cure Poison */
            case 24:
            {
                (void)set_poisoned(0);
                break;
            }

            /* Prayer */
            case 25:
            {
                (void)set_blessed(p_ptr->blessed + randint(48) + 48);
                break;
            }

            /* Dispel Undead */
            case 26:
            {
                (void)dispel_undead(randint(plev * 4));
                break;
            }

            /* Confuse Monster */
            case 27:
            {
                if (!get_aim_dir(&dir)) return;
                (void)confuse_monster(dir, plev);
                break;
            }

            /* Heal */
            case 28:
            {
                (void)hp_player(200);
                (void)set_stun(0);
                (void)set_cut(0);
                break;
            }

            /* Dispel Evil */
            case 29:
            {
                (void)dispel_evil(randint(plev * 3));
                break;
            }

            /* Glyph of Warding */
            case 30:
            {
                warding_glyph();
                break;
            }

            /* Holy Word */
            case 31:
            {
                (void)dispel_evil(randint(plev * 4));
                (void)hp_player(350);
                (void)set_afraid(0);
                (void)set_poisoned(p_ptr->poisoned/2);
                (void)set_stun(0);
                (void)set_cut(0);
                break;
            }

            /* Ethereal Openings:
             * Blink */
            case 32:
            {
                teleport_player(10);
                break;
            }

            /* Teleport Self */
            case 33:
            {
                teleport_player(30);
                break;
            }

            /* Teleport Other */
            case 34:
            {
                if (!get_aim_dir(&dir)) return;
                (void)teleport_monster(dir);
                break;
            }

            /* Create (Jammed) Door */
            case 35:
            {
                if (!get_aim_dir(&dir)) return;
                fire_arc(GF_MAKE_DOOR, dir, 0, 1, 45);

                (void)jam_doors();
                break;
            }

            /* Word of Recall */
            case 36:
            {
                if (p_ptr->word_recall == 0 && p_ptr->ethereal_lock == 0)
                {
                    p_ptr->word_recall = rand_int(20) + 15;
                    msg_print("The air about you becomes charged...");
                }
                else
                {
                    p_ptr->word_recall = 0;
                    msg_print("A tension leaves the air around you...");
                }
                break;
            }

            /* Ethereal Lock */
            case 37:
            {
                (void)set_ethereal_lock(p_ptr->ethereal_lock + randint(4) + 4);
                if (p_ptr->word_recall !=0)
                {
                    p_ptr->word_recall = 0;
                    msg_print("A tension leaves the air around you...");
                }
                break;

            }

            /* Godly Insights:
             * Detect Monsters */
            case 38:
            {
                (void)detect_monsters_normal(0);
                break;
            }

            /* Detection */
            case 39:
            {
                (void)detect_all();
                break;
            }

            /* Perception */
            case 40:
            {
                (void)ident_spell();
                break;
            }

            /* Probing */
            case 41:
            {
                (void)probing();
                break;
            }

            /* Self Knowledge */
            case 42:
            {
                msg_print("You begin to know yourself a little better...");
                msg_print(NULL);
                self_knowledge();
                break;
            }

            /* Clairvoyance */
            case 43:
            {
                wiz_lite();
                break;
            }

            /* Purification & Healing:
             * Cure Serious Wounds */
            case 44:
            {
                (void)hp_player(damroll(4, 10));
                (void)set_cut(0);
                break;
            }

            /* Cure Mortal Wounds */
            case 45:
            {
                (void)hp_player(damroll(8, 10));
                (void)set_stun(0);
                (void)set_cut(0);
                break;
            }

            /* Healing */
            case 46:
            {
                (void)hp_player(500);
                (void)set_stun(0);
                (void)set_cut(0);
                break;
            }

            /* Armour of Light */
            case 47:
            {
                (void)set_shield(p_ptr->shield + randint(20) + 30);
                (void)set_protevil(p_ptr->protevil + randint(20) + 30);
                break;
            }

            /* Restoration */
            case 48:
            {
                (void)do_res_stat(A_STR);
                (void)do_res_stat(A_INT);
                (void)do_res_stat(A_WIS);
                (void)do_res_stat(A_DEX);
                (void)do_res_stat(A_CON);
                (void)do_res_stat(A_CHR);
                break;
            }

            /* Remembrance */
            case 49:
            {
                (void)restore_level();
                break;
            }

            /* Holy Infusions:
             * Unbarring Ways */
            case 50:
            {
                (void)destroy_doors_touch();
                break;
            }

            /* Recharging */
            case 51:
            {
                (void)recharge(15);
                break;
            }

            /* Dispel Curse */
            case 52:
            {
                (void)remove_all_curse();
                break;
            }

            /* Enchant Weapon */
            case 53:
            {
                (void)enchant_spell(rand_int(4) + 1, rand_int(4) + 1, 0);
                break;
            }

            /* Enchant Armour */
            case 54:
            {
                (void)enchant_spell(0, 0, rand_int(3) + 2);
                break;
            }

            /* Elemental Brand */
            case 55:
            {
                brand_missile();
                break;
            }

            /* Wrath of God:
             * Dispel Undead */
            case 56:
            {
                (void)dispel_undead(randint(plev * 4));
                break;
            }

            /* Dispel Evil */
            case 57:
            {
                (void)dispel_evil(randint(plev * 4));
                break;
            }

            /* Banishment */
            case 58:
            {
                if (banish_evil(50))
                {
                    msg_print("The power of your god banishes evil!");
                }
                break;
            }

            /* Word of Destruction */
            case 59:
            {
                destroy_area(py, px, 15, TRUE);
                break;
            }

            /* Divine Intervention */
            case 60:
            {
                if (!get_aim_dir(&dir)) return;
                fire_ball(GF_PARALYSE, dir, damroll(4, 4), 2);
                break;
            }

            /* Annihilation */
            case 61:
            {
                if (!get_aim_dir(&dir)) return;
                drain_life(dir, 300);
                break;
            }
        }

        /* A prayer was prayed */
        if (!((spell < 32) ?
              (p_ptr->spell_worked1 & (1L << spell)) :
              (p_ptr->spell_worked2 & (1L << (spell - 32)))))
        {
            int e = s_ptr->sexp;

            /* The spell worked */
            if (spell < 32)
            {
                p_ptr->spell_worked1 |= (1L << spell);
            }
            else
            {
                p_ptr->spell_worked2 |= (1L << (spell - 32));
            }

            /* Gain experience */
            gain_exp(e * s_ptr->slevel);
        }
    }

    /* Take a turn */
    p_ptr->energy_use = 100;

    /* Sufficient mana */
    if (s_ptr->smana <= p_ptr->csp)
    {
        /* Use some mana */
        p_ptr->csp -= s_ptr->smana;
    }

    /* Over-exert the player */
    else
    {
        int oops = s_ptr->smana - p_ptr->csp;

        /* No mana left */
        p_ptr->csp = 0;
        p_ptr->csp_frac = 0;

        /* Message */
        msg_print("You faint from the effort!");

        /* Hack -- Bypass free action */
        (void)set_paralyzed(p_ptr->paralyzed + randint(5 * oops + 1));

        /* Damage CON (possibly permanently) */
        if (rand_int(100) < 50)
        {
            bool perm = (rand_int(100) < 25);

            /* Message */
            msg_print("You have damaged your health!");

            /* Reduce constitution */
            (void)dec_stat(A_CON, 15 + randint(10), perm);
        }
    }

    /* Redraw mana */
    p_ptr->redraw |= (PR_MANA);

    /* Window stuff */
    p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
}

