/* File: cmd5.c */

/* Purpose: Spell/Prayer/Mindcraft commands */

/* Selection, browsing, learning, and casting of spells and prayers.
 * Includes definitions of all spells and prayers.  Praying and using
 * mindcraft.
 *
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */


#include "angband.h"

extern void do_cmd_rerate(void);
extern bool item_tester_hook_armour(object_type *o_ptr);


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
static int get_spell(int *sn, cptr prompt, int sval, bool known, bool realm_2)
{
        int         i;
        int         spell = -1;
        int         num = 0;
        int         ask;
        byte        spells[64];
        bool        flag, redraw, okay;
        char        choice;
        magic_type  *s_ptr;
        char        out_val[160];
        int         use_realm = (realm_2?p_ptr->realm2:p_ptr->realm1);
        cptr        p = ((mp_ptr->spell_book == TV_LIFE_BOOK) ? "prayer" : "spell");

#ifdef ALLOW_REPEAT /* TNB */

        /* Get the spell, if available */
        if (repeat_pull(sn))
        {
                /* Verify the spell */
                if (spell_okay(*sn, known, use_realm - 1))
                {
                        /* Success */
                        return (TRUE);
                }
        }

#endif /* ALLOW_REPEAT -- TNB */

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
                if (spell_okay(spells[i], known, use_realm-1)) okay = TRUE;
        }

        /* No "okay" spells */
        if (!okay) return (FALSE);

        /* Assume cancelled */
        *sn = (-1);

        /* Nothing chosen yet */
        flag = FALSE;

        /* No redraw yet */
        redraw = FALSE;

        /* Show choices */
        if (show_choices)
        {
                /* Update */
                p_ptr->window |= (PW_SPELL);

                /* Window stuff */
                window_stuff();
        }


        /* Build a prompt (accept all spells) */
        (void)strnfmt(out_val, 78, "(%^ss %c-%c, *=List, ESC=exit) %^s which %s? ",
                p, I2A(0), I2A(num - 1), prompt, p);

        /* Get a spell from the user */
        while (!flag && get_com(out_val, &choice))
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
                                print_spells(spells, num, 1, 20, use_realm-1);
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
                ask = (isupper(choice));

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
                spell = spells[i];

                /* Require "okay" spells */
                if (!spell_okay(spell, known, use_realm-1))
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
                        s_ptr = &mp_ptr->info[use_realm-1][spell%32];

                        /* Prompt */
                        strnfmt(tmp_val, 78, "%^s %s (%d mana, %d%% fail)? ",
                                prompt, spell_names[use_realm-1][spell%32],
                                s_ptr->smana, spell_chance(spell,use_realm-1));

                        /* Belay that order */
                        if (!get_check(tmp_val)) continue;
                }

                /* Stop the loop */
                flag = TRUE;
        }


        /* Restore the screen */
        if (redraw) screen_load();


        /* Show choices */
        if (show_choices)
        {
                /* Update */
                p_ptr->window |= (PW_SPELL);

                /* Window stuff */
                window_stuff();
        }


        /* Abort if needed */
        if (!flag) return (FALSE);

        /* Save the choice */
        (*sn) = spell;

#ifdef ALLOW_REPEAT /* TNB */

        repeat_push(*sn);

#endif /* ALLOW_REPEAT -- TNB */

        /* Success */
        return (TRUE);
}


static void rustproof(void)
{
        int         item;
        object_type *o_ptr;
        char        o_name[80];
        cptr        q, s;

        /* Select a piece of armour */
        item_tester_hook = item_tester_hook_armour;

        /* Get an item */
        q = "Rustproof which piece of armour? ";
        s = "You have nothing to rustproof.";
        if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return;

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


        /* Description */
        object_desc(o_name, o_ptr, FALSE, 0);

        o_ptr->art_flags3 |= TR3_IGNORE_ACID;

        if ((o_ptr->to_a < 0) && !(o_ptr->ident & IDENT_CURSED))
        {
                msg_format("%s %s look%s as good as new!",
                        ((item >= 0) ? "Your" : "The"), o_name,
                        ((o_ptr->number > 1) ? "" : "s"));
                o_ptr->to_a = 0;
        }

        msg_format("%s %s %s now protected against corrosion.",
                ((item >= 0) ? "Your" : "The"), o_name,
                ((o_ptr->number > 1) ? "are" : "is"));

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
        int             item, sval;
        int             spell = -1;
        int             num = 0;

        byte            spells[64];

        object_type     *o_ptr;

        cptr q, s;

        /* Warriors are illiterate */
        if (!(p_ptr->realm1 || p_ptr->realm2))
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
        for (spell = 0; spell < 32; spell++)
        {
                /* Check for this spell */
                if ((fake_spell_flags[sval] & (1L << spell)))
                {
                        /* Collect this spell */
                        spells[num++] = spell;
                }
        }


        /* Save the screen */
        screen_save();

        /* Display the spells */
        print_spells(spells, num, 1, 20, (o_ptr->tval-90));

        /* Clear the top line */
        prt("", 0, 0);

        /* Prompt user */
        put_str("[Press any key to continue]", 0, 23);

        /* Wait for key */
        (void)inkey();

        /* Restore the screen */
        screen_load();
}




/*
 * Study a book to gain a new spell/prayer
 */
void do_cmd_study(void)
{
        int     i, item, sval;
        int     increment = 0;

        /* Spells of realm2 will have an increment of +32 */
        int     spell = -1;

        cptr p = ((mp_ptr->spell_book == TV_AIR_BOOK) ? "spell" : "prayer");

        object_type *o_ptr;

        cptr q, s;

        if (!p_ptr->realm1)
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

        msg_format("You can learn %d new %s%s.", p_ptr->new_spells, p,
                (p_ptr->new_spells == 1?"":"s"));
        msg_print(NULL);


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

        if (o_ptr->tval==p_ptr->realm2+89) increment=32;

        /* Track the object kind */
        object_kind_track(o_ptr->k_idx);

        /* Hack -- Handle stuff */
        handle_stuff();

        /* Mage -- Learn a selected spell */
        if (mp_ptr->spell_book != TV_LIFE_BOOK)
        {
                /* Ask for a spell, allow cancel */
                if (!get_spell(&spell, "study", sval, FALSE, (bool)(increment ? TRUE : FALSE))
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
                        if ((fake_spell_flags[sval] & (1L << spell)))
                        {
                                /* Skip non "okay" prayers */
                                if (!spell_okay(spell, FALSE,
                                        (increment?(p_ptr->realm2)-1:(p_ptr->realm1)-1))) continue;

                                /* Hack -- Prepare the randomizer */
                                k++;

                                /* Hack -- Apply the randomizer */
                                if (rand_int(k) == 0) gift = spell;
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
        energy_use = 100;

        if (increment) spell += increment;

        /* Learn the spell */
        if (spell < 32)
        {
                spell_learned1 |= (1L << spell);
        }
        else
        {
                spell_learned2 |= (1L << (spell - 32));
        }

        /* Find the next open entry in "spell_order[]" */
        for (i = 0; i < 64; i++)
        {
                /* Stop at the first empty space */
                if (spell_order[i] == 99) break;
        }

        /* Add the spell to the known list */
        spell_order[i++] = spell;

        /* Mention the result */
        msg_format("You have learned the %s of %s.",
                p, spell_names
                [(increment?(p_ptr->realm2)-1:(p_ptr->realm1)-1)][spell%32]);

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

        sc_time += randint(60);
}


void do_poly_wounds(void)
{
        /* Changed to always provide at least _some_ healing */
        s16b wounds = p_ptr->cut;
        s16b hit_p = (p_ptr->mhp - p_ptr->chp);
        s16b change = damroll(p_ptr->lev, 5);
        bool Nasty_effect = (randint(5)==1);

        if (!(wounds || hit_p || Nasty_effect)) return;

        msg_print("Your wounds are polymorphed into less serious ones.");
        hp_player(change);
        if (Nasty_effect)
        {
                msg_print("A new wound was created!");
                take_hit(change/2, "a polymorphed wound");
                set_cut(change);
        }
        else
        {
                set_cut((p_ptr->cut)-(change/2));
        }
}


static int corpse_to_weapon_aux (object_type *o_ptr)
{
       monster_race  *r_ptr = NULL;
       byte effect = 0;
/*       cptr name = (r_name + r_ptr->name); not needed now... */

       r_ptr = &r_info[o_ptr->pval];

       effect = r_ptr->blow[randint(4)-1].effect;

                        switch (effect)
                           {
                           case RBE_UN_POWER:
                              return(EGO_VAMPIRIC);
                              break;
                           case RBE_SHATTER:
                              return(EGO_EARTHQUAKES);
                              break;
                           case RBE_FIRE:
                              return(EGO_BRAND_FIRE);
                              break;
                           case RBE_ELEC:
                              return(EGO_BRAND_ELEC);
                              break;
                           case RBE_COLD:
                              return(EGO_BRAND_COLD);
                              break;
                           case RBE_POISON:
                              return(EGO_BRAND_POIS);
                              break;
                           case RBE_ACID:
                              return(EGO_BRAND_ACID);
                              break;
                           case RBE_TERRIFY:
                              return(EGO_HA);
                              break;
                           case RBE_PARALYZE:
                              return(EGO_DF);
                              break;
                           case RBE_INSANITY:
                              return(EGO_WPN_INSANE);
                              break;
                           case RBE_HALLU:
                              return(EGO_CHAOTIC); /* rare... */
                              break;
                           default:
                              break;
                           }

/* Didn't work? Then try something based on flags */

  if (r_ptr->flags3 & RF3_ORC) return (EGO_SLAY_ORC);
  if (r_ptr->flags3 & RF3_TROLL) return (EGO_SLAY_TROLL);
  if (r_ptr->flags3 & RF3_GIANT) return (EGO_SLAY_GIANT);
  if (r_ptr->flags3 & RF3_DRAGON) return (EGO_SLAY_DRAGON);
  if (r_ptr->flags3 & RF3_DEMON) return (EGO_SLAY_DEMON);
  if (r_ptr->flags3 & RF3_UNDEAD) return (EGO_SLAY_UNDEAD);
  if (r_ptr->flags3 & RF3_ANIMAL) return (EGO_SLAY_ANIMAL);
  if (r_ptr->flags3 & RF3_DUNADAN) return (EGO_WEST);

/* And finally, on names and chars */

      if (r_ptr->d_char == 'A') return (EGO_VALINOR);
      if (r_ptr->d_char == 'J') return (EGO_SERPENT);
/*    if (strstr(name, "haos")) return (EGO_CHAOTIC);  causes SIGSEGV? */

/* Anyway... */

     return (0);
}


/* Transfer some "life" of the monster to the weapon */

static bool corpse_to_weapon(bool perm)
{
        object_type *o1_ptr, *q_ptr;
        int obtained_ego, item;
        cptr q, s;

        o1_ptr = NULL; q_ptr = NULL;

        /* Get ingredients */
        q = "Select the corpse you want to use.";
        s = "You have no corpses available.";
        if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN)))
        {
                inven_item_increase(INVEN_PACK, -1);
                inven_item_optimize(INVEN_PACK);
                return(FALSE);
        }

        /* Get the item (in the pack) */
        if (item >= 0)
                o1_ptr = &inventory[item];

        if ((o1_ptr->tval != TV_CORPSE) && (o1_ptr->sval != SV_CORPSE_CORPSE))
        {
                msg_print("Only whole corpses can be used for this spell.");
                msg_print(NULL);
                inven_item_increase(INVEN_PACK, -1);
                inven_item_optimize(INVEN_PACK);
                return(FALSE);
        }

        /* Eliminate the items (from the pack) */
        if (item >= 0)
        {
                inven_item_increase(item, -1);
                inven_item_optimize(item);
        }

        obtained_ego = corpse_to_weapon_aux(o1_ptr);

        if (obtained_ego == 0)
        {
          msg_print("You fail to extract something usable from the corpse.");
          return(FALSE);
        }
        else
        {
        brand_weapon(obtained_ego, perm);
        return(TRUE);
        }
}

/* Helper function for mix_potions. We could use some more combinations!
   -GSN- */

static int mix_potions_aux (object_type *o_ptr1, object_type *o_ptr2)
{
        /* Handle same potions */

        if ((o_ptr1->sval == SV_POTION_CURE_LIGHT) &&
           (o_ptr2->sval == SV_POTION_CURE_LIGHT))
           return (SV_POTION_CURE_SERIOUS);
        if ((o_ptr1->sval == SV_POTION_CURE_SERIOUS) &&
           (o_ptr2->sval == SV_POTION_CURE_SERIOUS))
           return (SV_POTION_CURE_CRITICAL);
        if ((o_ptr1->sval == SV_POTION_HEALING) &&
           (o_ptr2->sval == SV_POTION_HEALING))
           return (SV_POTION_STAR_HEALING);
        if ((o_ptr1->sval == SV_POTION_INSANE_LIGHT) &&
           (o_ptr2->sval == SV_POTION_INSANE_LIGHT))
           return (SV_POTION_INSANE_SERIOUS);
        if ((o_ptr1->sval == SV_POTION_INSANE_SERIOUS) &&
           (o_ptr2->sval == SV_POTION_INSANE_SERIOUS))
           return (SV_POTION_INSANE_CRIT);

        /* Handle different potions. Note that order matters. */

        if ((o_ptr1->sval == SV_POTION_HEALING) &&
           (o_ptr2->sval == SV_POTION_SPEED))
           return (SV_POTION_STAR_HEALING);
        if ((o_ptr1->sval == SV_POTION_STAR_HEALING) &&
           (o_ptr2->sval == SV_POTION_SPEED))
           return (SV_POTION_CURING);
        if ((o_ptr1->sval == SV_POTION_HEALING) &&
           (o_ptr2->sval == SV_POTION_RESTORE_EXP))
           return (rand_range(SV_POTION_RES_STR, SV_POTION_RES_CHR));
        if ((o_ptr1->sval == SV_POTION_STAR_HEALING) &&
           (o_ptr2->sval == SV_POTION_RESTORE_EXP))
           return (rand_range(SV_POTION_INC_STR, SV_POTION_INC_CHR));

        /* All combinations used up */

        return(0); /* or "no success" */

}

/* Nix potions, get more powerful stuff */

static bool mix_potions(void)
{
        object_type *o1_ptr, *o2_ptr, *q_ptr;
        int result, choice, item, item2;
        cptr q, s;

        o1_ptr = NULL; o2_ptr = NULL; q_ptr = NULL;

        /* Get ingredients */
        q = "Select the first mixing ingredient.";
        s = "You have nothing to mix.";
        if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN)))
        {
                inven_item_increase(INVEN_PACK, -1);
                inven_item_optimize(INVEN_PACK);
                return(FALSE);
        }

        /* Get the item (in the pack) */
        if (item >= 0)
                o1_ptr = &inventory[item];

        if (o1_ptr->tval != TV_POTION)
        {
                msg_print("Only potions can be mixed.");
                msg_print(NULL);
                inven_item_increase(INVEN_PACK, -1);
                inven_item_optimize(INVEN_PACK);
                return(FALSE);
        }

        /* Eliminate the items (from the pack) */
        if (item >= 0)
        {
                inven_item_increase(item, -1);
                inven_item_optimize(item);
        }

        /* Get an item */
        q = "Select the second mixing ingredient.";
        s = "You have nothing to mix.";
        if (!get_item(&item2, q, s, (USE_EQUIP | USE_INVEN)))
        {
                inven_item_increase(INVEN_PACK, -1);
                inven_item_optimize(INVEN_PACK);
                return(FALSE);
        }

        /* Get the item (in the pack) */
        if (item2 >= 0)
                o2_ptr = &inventory[item2];

        if (o2_ptr->tval != TV_POTION)
        {
                msg_print("Only potions can be mixed.");
                msg_print(NULL);
                inven_item_increase(INVEN_PACK, -1);
                inven_item_optimize(INVEN_PACK);
                return(FALSE);
        }

        result = mix_potions_aux(o1_ptr, o2_ptr);

        if (result > 0) choice = lookup_kind(TV_POTION, result);
        else
        {
          msg_print("You fail to mix something useful");
          return(FALSE);
        }

        object_prep(q_ptr, choice);
        object_aware(q_ptr);
        object_known(q_ptr);
        drop_near(q_ptr, -1, py, px);

        /* Delay deleting the second item until now, in case it's the
           last one of the bundle */

        if (item2 >= 0)
        {
                inven_item_increase(item2, -1);
                inven_item_describe(item2);
                inven_item_optimize(item2);
        }

        return(TRUE);
}

/*
 * Brand the current weapon
 */
void brand_weapon(int brand_type, bool perm)
{
        object_type *o_ptr;
        int power = 0;

        o_ptr = &inventory[INVEN_WIELD];

        /* you can never modify artifacts / ego-items */
        /* you can never modify cursed items */
        /* TY: You _can_ modify broken items (if you're silly enough) */
        if ((o_ptr->k_idx) &&
                (!artifact_p(o_ptr)) && (!ego_item_p(o_ptr)) &&
                (!(o_ptr->art_name)) && (!cursed_p(o_ptr)))
        {
                cptr act = NULL;

                /* Let's get the name before it is changed... */
                char o_name[80];
                object_desc(o_name, o_ptr, FALSE, 0);

                if (!perm)
                /* Get power */
                power = get_quantity("Use how much mana? ", p_ptr->csp);

                switch (brand_type)
                {
                case EGO_BRAND_ACID:
                        act = "is covered in seething fluids!";
                        break;
                case EGO_BRAND_ELEC:
                        act = "is covered in sizzling sparks!";
                        break;
                case EGO_BRAND_COLD:
                        act = "is covered in a frosty sheath!";
                        break;
                case EGO_BRAND_FIRE:
                        act = "is covered in a fiery shield!";
                        break;
                case EGO_SLAYING:
                        act = "is noticeably more warlike!";
                        o_ptr->pval = randint(2);
                        o_ptr->dd *= 2;
                        break;
                case EGO_VAMPIRIC:
                        act = "thirsts for blood!";
                        break;
                case EGO_BRAND_POIS:
                        act = "is coated with poison.";
                        break;
                case EGO_CHAOTIC:
                        act = "is engulfed in raw Logrus!";
                        break;
                default:
                        act = "is magically empowered!";
                        break;
                }

                msg_format("Your %s %s", o_name, act);

                o_ptr->name2 = brand_type;

                if (o_ptr->to_h < 1)
                enchant(o_ptr, rand_int(3) + 4, ENCH_TOHIT);
                if (o_ptr->to_d < 1)
                enchant(o_ptr, rand_int(3) + 4, ENCH_TODAM);

                o_ptr->ident |= (IDENT_KNOWN);

                 if (!perm)
                 {
                 if (p_ptr->pclass == CLASS_PRIEST)
                     o_ptr->timeout = 5+(power*4);
                 else if (p_ptr->pclass == CLASS_MAGE)
                     o_ptr->timeout = 5+(power*5);
                 else if (p_ptr->pclass == CLASS_HIGH_MAGE)
                     o_ptr->timeout = 5+(power*6);
                 else o_ptr->timeout = 5+(power*3);
                 }

                 p_ptr->csp -= power;
        }
        else
        {
                if (flush_failure) flush();

                if (alert_failure)
                {
                if (cursed_p(o_ptr))
                msg_print("Your weapon is too malevolent.");
                else if (ego_item_p(o_ptr))
                msg_print("Your weapon has an ego already.");
                else if (!o_ptr->k_idx)
                msg_print("You wield no weapon.");
                }

                msg_print("The branding failed.");
        }
}


static void call_the_(void)
{
        int i;

        if (cave_floor_bold(py-1,px-1) && cave_floor_bold(py-1, px) &&
                cave_floor_bold(py-1,px+1) && cave_floor_bold(py,px-1) &&
                cave_floor_bold(py,px+1) && cave_floor_bold(py+1,px-1) &&
                cave_floor_bold(py+1,px) && cave_floor_bold(py+1,px+1))
        {
                for (i = 1; i < 10; i++)
                {
                        if (i-5) fire_ball(GF_ROCKET, i, 175, 2);
                }

                for (i = 1; i < 10; i++)
                {
                        if (i-5) fire_ball(GF_MANA, i, 175, 3);
                }

                for (i = 1; i < 10; i++)
                {
                        if (i-5) fire_ball(GF_NUKE, i, 175, 4);
                }
        }
        else
        {
                msg_format("You %s the %s too close to a wall!",
                        ((mp_ptr->spell_book == TV_LIFE_BOOK) ? "recite" : "cast"),
                        ((mp_ptr->spell_book == TV_LIFE_BOOK) ? "prayer" : "spell"));
                msg_print("There is a loud explosion!");

                if (destroy_area(py, px, 20+(p_ptr->lev), TRUE))
                        msg_print("The dungeon collapses...");
                else
                        msg_print("The dungeon trembles.");

                take_hit(100 + (randint(150)), "a suicidal Call the Void");
        }
}


/*
 * Fetch an item (teleport it right underneath the caster)
 */
void fetch(int dir, int wgt, bool require_los)
{
        int             ty, tx, i;
        bool            flag;
        cave_type       *c_ptr;
        object_type     *o_ptr;
        char            o_name[80];

        /* Check to see if an object is already there */
        if (cave[py][px].o_idx)
        {
                msg_print("You can't fetch when you're already standing on something.");
                return;
        }

        /* Use a target */
        if (dir == 5 && target_okay())
        {
                tx = target_col;
                ty = target_row;

                if (distance(py, px, ty, tx) > MAX_RANGE)
                {
                        msg_print("You can't fetch something that far away!");
                        return;
                }

                c_ptr = &cave[ty][tx];

                if (!c_ptr->o_idx)
                {
                        msg_print("There is no object at this place.");
                        return;
                }

                /* No fetching from vault */
                if (c_ptr->info & CAVE_ICKY)
                {
                        msg_print("The item slips from your control.");
                        return;
                }

                if (require_los && (!player_has_los_bold(ty, tx)))
                {
                        msg_print("You have no direct line of sight to that location.");
                        return;
                }
        }
        else
        {
                /* Use a direction */
                ty = py; /* Where to drop the item */
                tx = px;
                flag = FALSE;

                do
                {
                        ty += ddy[dir];
                        tx += ddx[dir];
                        c_ptr = &cave[ty][tx];

                        if ((distance(py, px, ty, tx)> MAX_RANGE)
                                || !cave_floor_bold(ty, tx)) return;
                }
                while(!c_ptr->o_idx);
        }

        o_ptr = &o_list[c_ptr->o_idx];

        if (o_ptr->weight > wgt)
        {
                /* Too heavy to 'fetch' */
                msg_print("The object is too heavy.");
                return;
        }

        i = c_ptr->o_idx;
        c_ptr->o_idx = o_ptr->next_o_idx;
        cave[py][px].o_idx = i; /* 'move' it */
        o_ptr->next_o_idx = 0;
        o_ptr->iy = py;
        o_ptr->ix = px;

        object_desc(o_name, o_ptr, TRUE, 0);
        msg_format("%^s flies through the air to your feet.", o_name);

        note_spot(py,px);
        p_ptr->redraw |= PR_MAP;
}


void wild_magic(int spell)
{
        int counter = 0;
        int type = SUMMON_BIZARRE1 - rand_int(6);

        if (type < SUMMON_BIZARRE1) type = SUMMON_BIZARRE1;
        else if (type > SUMMON_BIZARRE6) type = SUMMON_BIZARRE6;

        switch(randint(spell) + randint(8) + 1)
        {
        case 1:
        case 2:
        case 3:
                teleport_player(10);
                break;
        case 4:
        case 5:
        case 6:
                teleport_player(100);
                break;
        case 7:
        case 8:
                teleport_player(200);
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
        case 18:
                sleep_monsters_touch();
                break;
        case 19:
        case 20:
                trap_creation();
                break;
        case 21:
        case 22:
                door_creation();
                break;
        case 23:
        case 24:
        case 25:
                aggravate_monsters(1);
                break;
        case 26:
                /* Prevent destruction of quest levels and town */
                if (!is_quest(dun_level) && dun_level)
                        earthquake(py, px, 5);
                break;
        case 27:
        case 28:
                (void) gain_random_mutation(0);
                break;
        case 29:
        case 30:
                apply_disenchant(0);
                break;
        case 31:
                lose_all_info();
                break;
        case 32:
                fire_ball(GF_CHAOS, 0, spell + 5, 1 + (spell / 10));
                break;
        case 33:
                wall_stone();
                break;
        case 34:
        case 35:
                while (counter++ < 8)
                {
                        (void) summon_specific(py, px, (dun_level * 3) / 2, type, FALSE, FALSE, FALSE);
                }
                break;
        case 36:
        case 37:
                activate_hi_summon();
                break;
        case 38:
                summon_cyber();
        default:
                (void)activate_ty_curse(FALSE);
        }

        return;
}



static void cast_life_spell(int spell)
{
        int     dir;
        int     plev = p_ptr->lev;

        switch (spell)
        {
           case 0: /* Detect Evil */
                   (void)detect_monsters_evil();
                   break;
           case 1: /* Cure Light Wounds */
                   (void)hp_player(damroll(2, 10));
                   (void)set_cut(p_ptr->cut - 10);
                   break;
           case 2: /* Bless */
                   (void)set_blessed(p_ptr->blessed + randint(12) + 12);
                   break;
           case 3: /* Remove Fear */
                   (void)set_afraid(0);
                   break;
           case 4: /* Call Light */
                   (void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
                   break;
           case 5: /* Detect Traps + Secret Doors */
                   (void)detect_traps();
                   (void)detect_doors();
                   (void)detect_stairs();
                   break;
           case 6: /* Cure Medium Wounds */
                   (void)hp_player(damroll(4, 10));
                   (void)set_cut((p_ptr->cut / 2) - 20);
                   break;
           case 7: /* Satisfy Hunger */
                   (void)set_food(PY_FOOD_MAX - 1);
                   break;
           case 8: /* Remove Curse */
                   remove_curse();
                   break;
           case 9: /* Cure Poison */
                   (void)set_poisoned(0);
                   break;
           case 10: /* Cure Critical Wounds */
                   (void)hp_player(damroll(8, 10));
                   (void)set_stun(0);
                   (void)set_cut(0);
                   break;
           case 11: /* Sense Unseen */
                   (void)set_tim_invis(p_ptr->tim_invis + randint(24) + 24);
                   break;
           case 12: /* Orb or Draining */
                   if (!get_aim_dir(&dir)) return;
                   fire_ball(GF_HOLY_FIRE, dir,
                           (damroll(3, 6) + plev +
                           (plev / ((p_ptr->pclass == CLASS_PRIEST
                           || p_ptr->pclass == CLASS_HIGH_MAGE) ? 2 : 4))),
                           ((plev < 30) ? 2 : 3));
                   break;
           case 13: /* Protection from Evil */
                   (void)set_protevil(p_ptr->protevil + randint(25) + 3 * p_ptr->lev);
                   break;
           case 14: /* Healing */
                   (void)hp_player(300);
                   (void)set_stun(0);
                   (void)set_cut(0);
                   break;
           case 15: /* Glyph of Warding */
                   warding_glyph();
                   break;
       case 16: /* Exorcism */
                   (void) dispel_undead(plev);
                   (void) dispel_demons(plev);
                   (void) turn_evil(plev);
                   break;
           case 17: /* Dispel Curse */
                   (void)remove_all_curse();
                   break;
       case 18: /* Dispel Undead + Demons */
                   (void)dispel_undead(plev * 3);
                   (void)dispel_demons(plev * 3);
                   break;
       case 19: /* 'Day of the Dove' */
                   charm_monsters(plev * 2);
                   break;
       case 20: /* Dispel Evil */
                   (void)dispel_evil(plev * 4);
                   break;
           case 21: /* Banishment */
                   if (banish_evil(100))
                   {
                           msg_print("The power of your god banishes evil!");
                   }
                   break;
           case 22: /* Holy Word */
                   (void)dispel_evil(plev * 4);
                   (void)hp_player(1000);
                   (void)set_afraid(0);
                   (void)set_poisoned(0);
                   (void)set_stun(0);
                   (void)set_cut(0);
                   break;
           case 23: /* Warding True */
                   warding_glyph();
                   glyph_creation();
                   break;
           case 24: /* Heroism */
                   (void)set_hero(p_ptr->hero + randint(25) + 25);
                   (void)hp_player(10);
                   (void)set_afraid(0);
                   break;
           case 25: /* Prayer */
                   (void)set_blessed(p_ptr->blessed + randint(48) + 48);
                   break;
           case 26:
                   bless_weapon();
                   break;
           case 27: /* Restoration */
                   (void)do_res_stat(A_STR);
                   (void)do_res_stat(A_INT);
                   (void)do_res_stat(A_WIS);
                   (void)do_res_stat(A_DEX);
                   (void)do_res_stat(A_CON);
                   (void)do_res_stat(A_CHR);
                   (void)restore_level();
                   break;
       case 28: /* Healing True */
                   (void)hp_player(2000);
                   (void)set_stun(0);
                   (void)set_cut(0);
                   break;
       case 29: /* Holy Vision */
                   identify_fully();
                   break;
       case 30: /* Divine Intervention */
                   project(0, 1, py, px, 777, GF_HOLY_FIRE, PROJECT_KILL);
                   dispel_monsters(plev * 4);
                   slow_monsters();
                   stun_monsters(plev*4);
                   confuse_monsters(plev*4);
                   turn_monsters(plev*4);
                   stasis_monsters(plev*4);
                   summon_specific(py, px, plev, SUMMON_ANGEL, TRUE, TRUE, TRUE);
                   (void)set_shero(p_ptr->shero + randint(25) + 25);
                   (void)hp_player(300);
                   if (!p_ptr->fast)
                   {   /* Haste */
                           (void)set_fast(randint(20 + (plev) ) + plev);
                   }
                   else
                   {
                           (void)set_fast(p_ptr->fast + randint(5));
                   }
                   (void)set_afraid(0);
                   break;
           case 31: /* Holy Invulnerability */
                   if (p_ptr->invuln)
                   (void)set_invuln(p_ptr->invuln + 100 + randint(500));
                   else
                   (void)set_invuln(5000);
                   break;
           default:
                   msg_format("You cast an unknown Life spell: %d.", spell);
                   msg_print(NULL);
           }
}



static void cast_air_spell(int spell)
{
        int     dir, beam;
        int     plev = p_ptr->lev;
        int     ii = 0, ij = 0;

        if (p_ptr->pclass == CLASS_MAGE) beam = plev;
        else if (p_ptr->pclass == CLASS_HIGH_MAGE) beam = plev + 10;
        else beam = plev / 2;

        switch (spell)
        {
           case 0: /* Zephyr */
                   if (!get_aim_dir(&dir)) return;
                   fire_bolt_or_beam(beam-10, GF_WIND, dir,
                        damroll(3 + ((plev - 1) / 5), 3));
                   break;
           case 1: /* Phase Door */
                   teleport_player(10);
                   break;
           case 2: /* Detect Doors and Traps */
                   (void)detect_traps();
                   (void)detect_doors();
                   (void)detect_stairs();
                   break;
           case 3: /* Luminescence */
                   (void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
                   break;
           case 4: /* Isolation */
                   (void)set_oppose_elec(p_ptr->oppose_fire + plev * 2 + randint(20));
                   break;
           case 5: /* Air Gate */
                   msg_print("Powerful winds whisk you away...");
                   fire_ball(GF_WIND, 5, damroll(2+((plev-5)/4), 2), 2);
                   teleport_player(plev * 5);
                   break;
           case 6: /* Lightning Bolt */
                   if (!get_aim_dir(&dir)) return;
                   fire_bolt_or_beam(beam, GF_ELEC, dir,
                        damroll(6+((plev-5)/4), 8));
                   break;
           case 7: /* Recharging */
                   (void)recharge(plev * 2);
                   break;
           case 8: /* Invisibility */
                   ii = p_ptr->lev/2 + randint(p_ptr->lev);
                   set_tim_invis(p_ptr->tim_invis + ii);
                   break;
           case 9: /* Identify */
                   (void)ident_spell();
                   break;
           case 10: /* Slow Monster */
                   if (!get_aim_dir(&dir)) return;
                   (void)slow_monster(dir);
                   break;
           case 11: /* High Voltage */
                if (!get_aim_dir(&dir)) return;
                   fire_ball(GF_ELEC, dir,
                        (damroll(3, 5) + plev +
                        (plev / (((p_ptr->pclass == CLASS_MAGE)
                        || (p_ptr->pclass == CLASS_HIGH_MAGE)) ? 2 : 4))),
                        ((plev < 30) ? 2 : 3));
                   break;
           case 12: /* Clearing Breeze */
                   if (!get_aim_dir(&dir)) return;
                   (void)fire_beam(GF_AWAY_ALL, dir, plev);
                   break;
           case 13: /* Essence of Wind */
                   if (!p_ptr->fast)
                   {
                           (void)set_fast(randint(20 + (plev) ) + plev);
                   }
                   else
                   {
                           (void)set_fast(p_ptr->fast + randint(5));
                   }
                   break;
           case 14: /* Detect Objects and Treasure*/
                   (void)detect_objects_normal();
                   (void)detect_treasure();
                   (void)detect_objects_gold();
                   break;
           case 15: /* Air Mantle */
                   (void)set_shield(p_ptr->shield + randint(20) + 30);
                   break;
           case 16: /* Detection True */
                   (void)detect_all();
                   break;
          case 17: /* Conjure Spirit - note the awful hack to always
                      force creation of one, even in the town */
                   (void) summon_specific(py, px, 100, SUMMON_SPIRIT, FALSE, TRUE, TRUE);
                   break;
          case 18: /* High Voltage II */
                   if (!get_aim_dir(&dir)) return;
                   fire_shower(GF_ELEC, dir, 45 + (plev), 2, (plev/15));
                   break;
          case 19: /* Dimension Door */
                   msg_print("You open a dimensional gate. Choose a destination.");
                   if (!tgt_pt(&ii,&ij)) return;
                   p_ptr->energy -= 60 - plev;
                   if (!cave_empty_bold(ij,ii) || (cave[ij][ii].info & CAVE_ICKY) ||
                           (distance(ij,ii,py,px) > plev + 2) ||
                           (!rand_int(plev * plev / 2)))
                   {
                           msg_print("You fail to exit the astral plane correctly!");
                           p_ptr->energy -= 100;
                           teleport_player(10);
                   }
                   else teleport_player_to(ij,ii);
                   break;
           case 20: /* Air Branding */
                brand_weapon(EGO_BRAND_ELEC, FALSE);
                break;
           case 21: /* Self knowledge */
           (void)self_knowledge();
                   break;
           case 22: /* Conjure Elemental - note the awful hack to always
                       force creation of one, even in the town */
                (void) summon_specific(py, px, 100, SUMMON_ELEMENTAL, FALSE, TRUE, TRUE);
                break;
           case 23: /* Teleport Level */
                   (void)teleport_player_level();
                   break;
           case 24: /* Word of Recall */
                   recall_player();
                   break;
           case 25: /* Chain Lightning */
                for (dir = 0; dir <= 9; dir++)
                        fire_beam(GF_ELEC, dir, damroll(5+(plev/10), 8));
                break;
           case 26: /* Whirlwind Attack */
                {
                        int y = 0, x = 0;
                        cave_type       *c_ptr;
                        monster_type    *m_ptr;

                        for (dir = 0; dir <= 9; dir++)
                        {
                                y = py + ddy[dir];
                                x = px + ddx[dir];
                                c_ptr = &cave[y][x];

                                /* Get the monster */
                                m_ptr = &m_list[c_ptr->m_idx];

                                /* Hack -- attack monsters */
                                if (c_ptr->m_idx && (m_ptr->ml || cave_floor_bold(y, x)))
                                        py_attack(y, x);
                        }
                }
                break;
           case 27: /* Atomic Tornado */
                  if (!get_aim_dir(&dir)) return;
                msg_print("You launch an atomic nuke!");
                fire_ball(GF_NUKE, dir, 120 + (plev), 2);
                break;
           case 28: /* Charge Weapon */
                   (void)enchant_spell(rand_int(4) + 1, rand_int(4) + 1, 0);
                   break;
           case 29: /* Charge Armour */
                   (void)enchant_spell(0, 0, rand_int(3) + 2);
                   break;
           case 30: /* Conjure Greater Elemental - note the awful hack to always
                    force creation of one, even in the town */
                (void) summon_specific(py, px, 100, SUMMON_ELEMENTAL2, FALSE, TRUE, TRUE);
                break;
           case 31: /* Globe of Invulnerability */
                   (void)set_invuln(p_ptr->invuln + randint(8) + 8);
                   break;
           default:
                   msg_format("You cast an unknown Sorcery spell: %d.", spell);
                   msg_print(NULL);
           }
}


static void cast_earth_spell(int spell)
{
        int         dir;
        int         beam;
        int         plev = p_ptr->lev;
        bool    no_trump = FALSE;

        if (p_ptr->pclass == CLASS_MAGE) beam = plev;
        else if (p_ptr->pclass == CLASS_HIGH_MAGE) beam = plev + 10;
        else beam = plev / 2;

        switch (spell)
        {
        case 0: /* Acid Spray */
                if (!get_aim_dir(&dir)) return;
                fire_bolt_or_beam(beam-10, GF_ACID, dir,
                      damroll(3 + ((plev - 1) / 5), 3));
                break;
        case 1: /* Detect Creatures */
                (void)detect_monsters_normal();
                break;
        case 2: /* Foraging */
                (void)set_food(PY_FOOD_MAX - 1);
                break;
        case 3: /* Daylight */
                (void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
                if ((p_ptr->prace == RACE_VAMPIRE) && !(p_resist_lite))
                {
                        msg_print("The daylight scorches your flesh!");
                        take_hit(damroll(2,2), "daylight");
                }
                break;
        case 4: /* Animal Taming */
                if (!get_aim_dir(&dir)) return;
                (void) charm_animal(dir, plev);
                break;
        case 5: /* Earthen Shield */
                (void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20);
                break;
        case 6: /* Earth Gate */
                msg_print("You sink below the ground...");
                fire_ball(GF_ACID, 5, damroll(2+((plev-5)/4), 2), 2);
                teleport_player(plev * 5);
                break;
        case 7: /* Cure Wounds + Poison */
                (void)set_cut(0);
                (void)set_poisoned(0);
                break;
        case 8: /* Stone to Mud */
                if (!get_aim_dir(&dir)) return;
                (void)wall_to_mud(dir);
                break;
        case 9: /* Acid Ball */
                if (!get_aim_dir(&dir)) return;
                fire_ball(GF_ACID, dir, damroll(3 + ((plev - 5) / 4), 8), 2);
                break;
        case 10: /* Nature Awareness -- downgraded */
                map_area();
                (void)detect_traps();
                (void)detect_doors();
                (void)detect_stairs();
                (void)detect_monsters_normal();
                break;
        case 11: /* Acid Bolt */
                if (!get_aim_dir(&dir)) return;
                fire_bolt_or_beam(beam-10, GF_ACID, dir,
                        damroll(5+((plev-5)/4), 8));
                break;
        case 12: /* Ray of Sunlight */
                if (!get_aim_dir(&dir)) return;
                msg_print("A line of sunlight appears.");
                lite_line(dir);
                break;
        case 13: /* Entangle */
                slow_monsters();
                break;
        case 14: /* Summon Animals */
                if (!(summon_specific(py, px, plev, SUMMON_ANIMAL_RANGER, TRUE, TRUE, TRUE)))
                        no_trump = TRUE;
                break;
        case 15: /* Herbal Healing */
                (void)hp_player(1000);
                (void)set_stun(0);
                (void)set_cut(0);
                (void)set_poisoned(0);
                break;
        case 16: /* Door Building */
                (void)door_creation();
                break;
        case 17: /* Stair Building */
                (void)stair_creation();
                break;
        case 18: /* Stone Skin */
                (void)set_shield(p_ptr->shield + randint(20) + 30);
                break;
        case 19: /* Conjure Spirit - note the awful hack to always
                    force creation of one, even in the town */
                   (void) summon_specific(py, px, 100, SUMMON_SPIRIT, FALSE, TRUE, TRUE);
                   break;
        case 20: /* Animal Friendship */
                (void) charm_animals(plev * 2);
                break;
        case 21: /* Earth Branding */
                brand_weapon(EGO_BRAND_ACID, FALSE);
                break;
        case 22: /* Wall of Stone */
                (void)wall_stone();
                break;
        case 23: /* Protection from Corrosion */
                rustproof();
                break;
        case 24: /* Earthquake */
                /* Prevent destruction of quest levels and town */
                if (!is_quest(dun_level) && dun_level)
                        earthquake(py, px, 10);
                break;
        case 25: /* Conjure Elemental - note the awful hack to always
                       force creation of one, even in the town */
                (void) summon_specific(py, px, 100, SUMMON_ELEMENTAL, FALSE, TRUE, TRUE);
                break;
        case 26: /* Rays of Acid */
                for (dir = 0; dir <= 9; dir++)
                        fire_beam(GF_ACID, dir, damroll(5+(plev/10), 8));
                break;
        case 27: /* Petrify */
                if (!get_aim_dir(&dir)) return;
                fire_bolt(GF_STONE, dir, 90 + (plev));
                break;
        case 28: /* Pool of Gravity */
                if (!get_aim_dir(&dir)) return;
                fire_ball(GF_INERTIA, dir, 100 + (plev), (plev/12)+1);
                break;
        case 29: /* Call Sunlight */
                fire_ball(GF_LITE, 0, 150, 8);
                wiz_lite();
                if ((p_ptr->prace == RACE_VAMPIRE) && !(p_resist_lite))
                {
                        msg_print("The sunlight scorches your flesh!");
                        take_hit(50, "sunlight");
                }
                break;
        case 30: /* Conjure Greater Elemental - note the awful hack to always
                    force creation of one, even in the town */
                (void) summon_specific(py, px, 100, SUMMON_ELEMENTAL2, FALSE, TRUE, TRUE);
                break;
        case 31: /* Nature's Wrath */
                (void)dispel_monsters(plev * 4);
                /* Prevent destruction of quest levels and town */
                if (!is_quest(dun_level) && dun_level)
                        earthquake(py, px, 20 + (plev / 2) );
                project(0, 1 + plev / 12, py, px,
                        100 + plev, GF_DISINTEGRATE, PROJECT_KILL | PROJECT_ITEM);
                break;
        default:
                msg_format("You cast an unknown Nature spell: %d.", spell);
                msg_print(NULL);
        }
        if (no_trump && alert_failure)
                msg_print("No animals arrive.");
}


static void cast_fire_spell(int spell)
{
        int     dir, i, beam;
        int     plev = p_ptr->lev;

        if (p_ptr->pclass == CLASS_MAGE) beam = plev;
        else if (p_ptr->pclass == CLASS_HIGH_MAGE) beam = plev + 10;
        else beam = plev / 2;

        switch (spell)
        {
        case 0: /* Burning Hands */
                if (!get_aim_dir(&dir)) return;
                fire_bolt_or_beam(beam-10, GF_FIRE, dir,
                        damroll(3 + ((plev - 1) / 5), 4));
                break;
        case 1: /* Trap / Door Destruction, was: Blink */
                (void)destroy_doors_touch();
                break;
        case 2: /* Flash of Fire */
                (void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
                break;
        case 3: /* Fire Screen */
                (void)set_oppose_fire(p_ptr->oppose_fire + plev * 2 + randint(20));
                (void)set_oppose_blind(p_ptr->tim_res_blind + plev * 2 + randint(20));
                break;
        case 4: /* Fireball */
                if (!get_aim_dir(&dir)) return;
                fire_ball(GF_FIRE, dir,
                        (damroll(3, 5) + plev +
                        (plev / (((p_ptr->pclass == CLASS_MAGE)
                        || (p_ptr->pclass == CLASS_HIGH_MAGE)) ? 2 : 4))),
                        ((plev < 30) ? 2 : 3));
                break;
        case 5: /* Fire Bolt */
                if (!get_aim_dir(&dir)) return;
                fire_bolt_or_beam(beam, GF_FIRE, dir,
                        damroll(6+((plev-5)/4), 8));
                break;
        case 6: /* Fist of Force ("Fist of Fun") */
                if (!get_aim_dir(&dir)) return;
                fire_ball(GF_DISINTEGRATE, dir,
                        damroll(8+((plev-5)/4), 8), 0);
                break;
        case 7: /* Fiery Gate */
                msg_print("A flash of fire takes you away...");
                fire_ball(GF_FIRE, 5, damroll(2+((plev-5)/4), 2), 2);
                teleport_player(plev * 5);
                break;
        case 8: /* Wonder */
                {
                /* This spell should become more useful (more
                controlled) as the player gains experience levels.
                Thus, add 1/5 of the player's level to the die roll.
                This eliminates the worst effects later on, while
                keeping the results quite random.  It also allows
                        some potent effects only at high level. */

                        int die = randint(100) + plev / 5;

                        if (!get_aim_dir(&dir)) return;
                        if (die > 100)
                                msg_print("You feel a surge of power!");
                        if (die < 8) clone_monster(dir);
                        else if (die < 14) speed_monster(dir);
                        else if (die < 26) heal_monster(dir);
                        else if (die < 31) poly_monster(dir);
                        else if (die < 36)
                                fire_bolt_or_beam(beam - 10,
                                GF_MISSILE, dir,
                                damroll(3 + ((plev - 1) / 5), 4));
                        else if (die < 41) confuse_monster(dir, plev);
                        else if (die < 46) fire_ball(GF_POIS, dir, 20 + (plev / 2), 3);
                        else if (die < 51) lite_line(dir);
                        else if (die < 56)
                                fire_bolt_or_beam(beam - 10, GF_ELEC, dir,
                                damroll(3+((plev-5)/4),8));
                        else if (die < 61)
                                fire_bolt_or_beam(beam - 10, GF_COLD, dir,
                                damroll(5+((plev-5)/4),8));
                        else if (die < 66)
                                fire_bolt_or_beam(beam, GF_ACID, dir,
                                damroll(6+((plev-5)/4),8));
                        else if (die < 71)
                                fire_bolt_or_beam(beam, GF_FIRE, dir,
                                damroll(8+((plev-5)/4),8));
                        else if (die < 76) drain_life(dir, 75);
                        else if (die < 81) fire_ball(GF_ELEC, dir, 30 + plev / 2, 2);
                        else if (die < 86) fire_ball(GF_ACID, dir, 40 + plev, 2);
                        else if (die < 91) fire_ball(GF_ICE, dir, 70 + plev, 3);
                        else if (die < 96) fire_ball(GF_FIRE, dir, 80 + plev, 3);
                        else if (die < 101) drain_life(dir, 100 + plev);
                        else if (die < 104)
                        {
                                /* Prevent destruction of quest levels and town */
                                if (!is_quest(dun_level) && dun_level)
                                        earthquake(py, px, 12);
                        }
                        else if (die < 106)
                        {
                                /* Prevent destruction of quest levels and town */
                                if (!is_quest(dun_level) && dun_level)
                                        destroy_area(py, px, 15, TRUE);
                        }
                        else if (die < 108)
                        {
                                genocide(TRUE);
                        }
                        else if (die < 110) dispel_monsters(120);
                        else /* RARE */
                        {
                                dispel_monsters(150);
                                slow_monsters();
                                sleep_monsters();
                                hp_player(300);
                        }
                        break;
                }
                break;
        case 9: /* Chaos Bolt */
                if (!get_aim_dir(&dir)) return;
                fire_bolt_or_beam(beam, GF_CHAOS, dir,
                        damroll(10+((plev-5)/4), 8));
                break;
        case 10: /* Sonic Boom */
                msg_print("BOOM! Shake the room!");
                project(0, 2+plev/10, py, px,
                        45+plev, GF_SOUND, PROJECT_KILL | PROJECT_ITEM);
                break;
        case 11: /* Doom Bolt -- always beam in 2.0.7 or later */
                if (!get_aim_dir(&dir)) return;
                fire_beam(GF_MANA, dir, damroll(11+((plev-5)/4), 8));
                break;
        case 12: /* Fireball II */
                if (!get_aim_dir(&dir)) return;
                fire_ball(GF_FIRE, dir, 55 + (plev), 2);
                break;
        case 13: /* Teleport Other */
                if (!get_aim_dir(&dir)) return;
                (void)fire_beam(GF_AWAY_ALL, dir, plev);
                break;
        case 14: /* Word of Destruction */
                /* Prevent destruction of quest levels and town */
                if (!is_quest(dun_level) && dun_level)
                        destroy_area(py, px, 15, TRUE);
                break;
        case 15: /* Conjure Spirit - note the awful hack to always
                    force creation of one, even in the town */
                (void) summon_specific(py, px, 100, SUMMON_SPIRIT, FALSE, TRUE, TRUE);
                break;
        case 16: /* Polymorph Other */
                if (!get_aim_dir(&dir)) return;
                (void)poly_monster(dir);
                break;
        case 17: /* Fire Branding */
                brand_weapon(EGO_BRAND_FIRE, FALSE);
                break;
        case 18: /* Fire Star */
                for (dir = 0; dir <= 9; dir++)
                        fire_beam(GF_FIRE, dir, damroll(5+(plev/10), 8));
                break;
        case 19: /* Arcane Binding == Charging */
                (void)recharge(40);
                break;
        case 20: /* Fiery Shower */
                if (!get_aim_dir(&dir)) return;
                fire_blast(GF_FIRE, dir, (plev/5)+1, 10, 5, 3);
                break;
        case 21: /* Alter Reality */
                alter_reality();
                break;
        case 22: /* Polymorph Self */
                polymorph_player(0);
                break;
        case 23: /* Conjure Elemental - note the awful hack to always
                    force creation of one, even in the town */
                (void) summon_specific(py, px, 100, SUMMON_ELEMENTAL, FALSE, TRUE, TRUE);
                break;
        case 24: /* Summon monster, demon */
                {
                        bool pet = (randint(3) == 1);
                        bool group = !(pet && (plev < 50));

                        if (summon_specific(py, px, (plev*3)/2, SUMMON_DEMON, group, FALSE, pet))
                        {
                                msg_print("The area fills with a stench of sulphur and brimstone.");

                                if (pet)
                                        msg_print("'What is thy bidding... Master?'");
                                else
                                        msg_print("'NON SERVIAM! Wretch! I shall feast on thy mortal soul!'");
                        }
                        break;
                }
        case 25: /* Ray of Sun */
                if (!get_aim_dir(&dir)) return;
                fire_beam(GF_LITE, dir, 200);
                break;
        case 26: /* Meteor Swarm  */
                {
                        int x, y, dx, dy, d;
                        int b = 10 + randint(10);
                        for (i = 0; i < b; i++)
                        {
                                int count = 0;

                                do
                                {
                                        count++;
                                        if (count > 1000) break;
                                        x = px - 5 + randint(10);
                                        y = py - 5 + randint(10);

                                        dx = (px > x) ? (px - x) : (x - px);
                                        dy = (py > y) ? (py - y) : (y - py);

                                        /* Approximate distance */
                                        d = (dy > dx) ? (dy + (dx >> 1)) : (dx + (dy >> 1));
                                }
                                while (in_bounds(y, x) && ((d > 5) || !player_has_los_bold(y, x)));

                                if (count > 1000) break;

                                project(0, 2, y, x, (plev * 3) / 2, GF_METEOR, PROJECT_KILL | PROJECT_JUMP | PROJECT_ITEM);
                        }
                }
                break;
        case 27: /* Call Chaos */
                call_chaos();
                break;
        case 28: /* Magic Rocket */
                if (!get_aim_dir(&dir)) return;
                msg_print("You launch a rocket!");
                fire_ball(GF_ROCKET, dir, 120 + (plev), 2);
                break;
        case 29: /* Conjure Greater Elemental - note the awful hack to always
                    force creation of one, even in the town */
                (void) summon_specific(py, px, 100, SUMMON_ELEMENTAL2, FALSE, TRUE, TRUE);
                break;
        case 30: /* Breathe Logrus */
                if (!get_aim_dir(&dir)) return;
                fire_ball(GF_CHAOS,dir,p_ptr->chp, 2);
                break;
        case 31: /* Call the Void */
                call_the_();
                break;
        default:
                msg_format("You cast an unknown Chaos spell: %d.", spell);
                msg_print(NULL);
        }
}


static void cast_death_spell(int spell)
{
        int     dir;
        int     beam;
        int     plev = p_ptr->lev;
        int     dummy = 0;
        int     i;

        if (p_ptr->pclass == CLASS_MAGE) beam = plev;
        else if (p_ptr->pclass == CLASS_HIGH_MAGE) beam = plev + 10;
        else beam = plev / 2;

        switch (spell)
        {
        case 0: /* Detect Undead & Demons -> Unlife */
                (void) detect_monsters_nonliving();
                break;
        case 1: /* Malediction */
                if (!get_aim_dir(&dir)) return;
                /* A radius-0 ball may (1) be aimed at objects etc.,
                 * and will affect them; (2) may be aimed at ANY
                 * visible monster, unlike a 'bolt' which must travel
                 * to the monster. */

                fire_ball(GF_HELL_FIRE, dir,
                        damroll(3 + ((plev - 1) / 5), 3), 0);
                if (randint(5)==1)
                {   /* Special effect first */
                        dummy = randint(1000);
                        if (dummy == 666)
                                fire_bolt(GF_DEATH_RAY, dir, plev * 200);
                        else if (dummy < 500)
                                fire_bolt(GF_TURN_ALL, dir, plev);
                        else if (dummy < 800)
                                fire_bolt(GF_OLD_CONF, dir, plev);
                        else
                                fire_bolt(GF_STUN, dir, plev);
                }
                break;
        case 2: /* Detect Evil */
                (void)detect_monsters_evil();
                break;
        case 3: /* Stinking Cloud */
                if (!get_aim_dir(&dir)) return;
                fire_ball(GF_POIS, dir, 10 + (plev / 2), 2);
                break;
        case 4: /* Black Sleep */
                if (!get_aim_dir(&dir)) return;
                (void)sleep_monster(dir);
                break;
        case 5: /* Resist Poison */
                (void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20);
                break;
        case 6: /* Horrify */
                if (!get_aim_dir(&dir)) return;
                (void)fear_monster(dir, plev);
                (void) stun_monster(dir, plev);
                break;
        case 7: /* Enslave the Undead */
                if (!get_aim_dir(&dir)) return;
                (void)control_one_undead(dir, plev);
                break;
        case 8: /* Orb of Entropy */
                if (!get_aim_dir(&dir)) return;
                fire_ball(GF_OLD_DRAIN, dir,
                        (damroll(3, 6) + plev +
                        (plev / (((p_ptr->pclass == CLASS_MAGE)
                        || (p_ptr->pclass == CLASS_HIGH_MAGE)) ? 2 : 4))),
                        ((plev < 30) ? 2 : 3));
                break;
        case 9: /* Nether Bolt */
                if (!get_aim_dir(&dir)) return;
                fire_bolt_or_beam(beam, GF_NETHER, dir,
                        damroll(6+((plev-5)/4), 8));
                break;
        case 10: /* Terror */
                turn_monsters(30+plev);
                break;
        case 11: /* Vampiric Drain */
                if (!get_aim_dir(&dir)) return;
                dummy = plev + randint(plev) * MAX(1, plev/10);   /* Dmg */
                if (drain_life(dir, dummy))
                {
                        (void)hp_player(dummy);
                        /* Gain nutritional sustenance: 150/hp drained */
                        /* A Food ration gives 5000 food points (by contrast) */
                        /* Don't ever get more than "Full" this way */
                        /* But if we ARE Gorged,  it won't cure us */
                        dummy = p_ptr->food + MIN(5000, 100 * dummy);
                        if (p_ptr->food < PY_FOOD_MAX)   /* Not gorged already */
                                (void)set_food(dummy >= PY_FOOD_MAX ? PY_FOOD_MAX-1 : dummy);
                }
                break;
        case 12: /* Poison Branding */
                brand_weapon(EGO_BRAND_POIS, FALSE);
                break;
        case 13: /* Dispel Good */
                (void)dispel_good(plev * 4);
                break;
        case 14: /* Genocide */
                (void)genocide(TRUE);
                break;
        case 15: /* Restore Life */
                (void)restore_level();
                break;
        case 16: /* Berserk */
                (void)set_shero(p_ptr->shero + randint(25) + 25);
                (void)hp_player(30);
                (void)set_afraid(0);
                break;
        case 17: /* Invoke Spirits */
                {
                        int die = randint(100) + plev / 5 + luck();
                        if (!get_aim_dir(&dir)) return;

                        msg_print("You call on the power of the dead...");
                        if (die > 100)
                                msg_print("You feel a surge of eldritch force!");

                        if (die < 8)
                        {
                                msg_print("Oh no! Mouldering forms rise from the earth around you!");
                                (void) summon_specific(py, px, dun_level, SUMMON_UNDEAD, FALSE, FALSE, FALSE);
                        }
                        else if (die < 14)
                        {
                                msg_print("An unnamable evil brushes against your mind...");
                                set_afraid(p_ptr->afraid + randint(4) + 4);
                        }
                        else if (die < 26)
                        {
                                msg_print("Your head is invaded by a horde of gibbering spectral voices...");
                                set_confused(p_ptr->confused + randint(4) + 4);
                        }
                        else if (die < 31)
                        {
                                poly_monster(dir);
                        }
                        else if (die < 36)
                        {
                                fire_bolt_or_beam(beam - 10, GF_MISSILE, dir,
                                        damroll(3 + ((plev - 1) / 5), 4));
                        }
                        else if (die < 41)
                        {
                                confuse_monster (dir, plev);
                        }
                        else if (die < 46)
                        {
                                fire_ball(GF_POIS, dir, 20 + (plev / 2), 3);
                        }
                        else if (die < 51)
                        {
                                lite_line(dir);
                        }
                        else if (die < 56)
                        {
                                fire_bolt_or_beam(beam - 10, GF_ELEC, dir,
                                        damroll(3+((plev-5)/4),8));
                        }
                        else if (die < 61)
                        {
                                fire_bolt_or_beam(beam - 10, GF_COLD, dir,
                                        damroll(5+((plev-5)/4),8));
                        }
                        else if (die < 66)
                        {
                                fire_bolt_or_beam(beam, GF_ACID, dir,
                                        damroll(6+((plev-5)/4),8));
                        }
                        else if (die < 71)
                        {
                                fire_bolt_or_beam(beam, GF_FIRE, dir,
                                        damroll(8+((plev-5)/4),8));
                        }
                        else if (die < 76)
                        {
                                drain_life(dir, 75);
                        }
                        else if (die < 81)
                        {
                                fire_ball(GF_ELEC, dir, 30 + plev / 2, 2);
                        }
                        else if (die < 86)
                        {
                                fire_ball(GF_ACID, dir, 40 + plev, 2);
                        }
                        else if (die < 91)
                        {
                                fire_ball(GF_ICE, dir, 70 + plev, 3);
                        }
                        else if (die < 96)
                        {
                                fire_ball(GF_FIRE, dir, 80 + plev, 3);
                        }
                        else if (die < 101)
                        {
                                drain_life(dir, 100 + plev);
                        }
                        else if (die < 104)
                        {
                                /* Prevent destruction of quest levels and town */
                                if (!is_quest(dun_level) && dun_level)
                                        earthquake(py, px, 12);
                        }
                        else if (die < 106)
                        {
                                /* Prevent destruction of quest levels and town */
                                if (!is_quest(dun_level) && dun_level)
                                        destroy_area(py, px, 15, TRUE);
                        }
                        else if (die < 108)
                        {
                                genocide(TRUE);
                        }
                        else if (die < 110)
                        {
                                dispel_monsters(120);
                        }
                        else
                        { /* RARE */
                                dispel_monsters(150);
                                slow_monsters();
                                sleep_monsters();
                                hp_player(300);
                        }

                        if (die < 31)
                                msg_print("Sepulchral voices chuckle. 'Soon you will join us, mortal.'");
                        break;
                }
        case 18: /* Dark Bolt */
                if (!get_aim_dir(&dir)) return;
                fire_bolt_or_beam(beam, GF_DARK, dir,
                        damroll(4+((plev-5)/4), 8));
                break;
        case 19: /* Battle Frenzy */
                (void)set_shero(p_ptr->shero + randint(25) + 25);
                (void)hp_player(30);
                (void)set_afraid(0);
                if (!p_ptr->fast)
                {
                        (void)set_fast(randint(20 + (plev / 2) ) + (plev / 2));
                }
                else
                {
                        (void)set_fast(p_ptr->fast + randint(5));
                }
                break;
        case 20: /* Vampirism True */
                if (!get_aim_dir(&dir)) return;
                for (dummy = 0; dummy < 3; dummy++)
                {
                        if (drain_life(dir, 100))
                                hp_player(100);
                }
                break;
        case 21: /* Vampiric Branding */
                brand_weapon(EGO_VAMPIRIC, FALSE);
                break;
        case 22: /* Darkness Storm */
                if (!get_aim_dir(&dir)) return;
                fire_ball(GF_DARK, dir, 120, 4);
                break;
        case 23: /* Mass Genocide */
                (void)mass_genocide(TRUE);
                break;
        case 24: /* Death Ray */
                if (!get_aim_dir(&dir)) return;
                (void)death_ray(dir, plev);
                break;
        case 25: /* Raise the Dead */
                {
                        bool pet = (randint(3) == 1);
                        bool group;
                        int type;

                        if (pet)
                        {
                                type = (plev > 47 ? SUMMON_HI_UNDEAD_NO_UNIQUES : SUMMON_UNDEAD);
                                group = (((plev > 24) && (randint(3) == 1)) ? TRUE : FALSE);
                        }
                        else
                        {
                                type = (plev > 47 ? SUMMON_HI_UNDEAD : SUMMON_UNDEAD);
                                group = TRUE;
                        }

                        if (summon_specific(py, px, (plev*3)/2, type, group, FALSE, pet))
                        {
                                msg_print("Cold winds begin to blow around you, carrying with them the stench of decay...");

                                if (pet)
                                        msg_print("Ancient, long-dead forms arise from the ground to serve you!");
                                else
                                        msg_print("'The dead arise... to punish you for disturbing them!'");
                        }

                        break;
                }
        case 26: /* Esoteria */
                if (randint(50)>plev)
                        (void) ident_spell();
                else
                        identify_fully();
                break;
        case 27: /* Word of Death */
                (void)dispel_living(plev * 3);
                break;
        case 28: /* Evocation       */
                (void)dispel_monsters(plev * 4);
                turn_monsters(plev*4);
                banish_monsters(plev*4);
                break;
        case 29: /* Hellfire */
                if (!get_aim_dir(&dir)) return;
                fire_ball(GF_HELL_FIRE, dir, 666, 3);
                take_hit(50+randint(50), "the strain of casting Hellfire");
                break;
        case 30: /* Omnicide */
                p_ptr->csp -= 100;      /* Display doesn't show mana cost (100)
                                                         * as deleted until the spell has finished. This gives a
                                                         * false impression of how high your mana is climbing.
                                                         * Therefore, 'deduct' the cost temporarily before entering the
                                                         * loop, then add it back at the end so that the rest of the
                                                         * program can deduct it properly */
                for (i = 1; i < m_max; i++)
                {
                        monster_type    *m_ptr = &m_list[i];
                        monster_race    *r_ptr = &r_info[m_ptr->r_idx];

                        /* Paranoia -- Skip dead monsters */
                        if (!m_ptr->r_idx) continue;

                        /* Hack -- Skip Unique Monsters */
                        if (r_ptr->flags1 & (RF1_UNIQUE)) continue;

                        /* Hack -- Skip Quest Monsters */
                        if (r_ptr->flags1 & RF1_QUESTOR) continue;

                        /* Delete the monster */
                        delete_monster_idx(i);

                        /* Take damage */
                        take_hit(randint(4), "the strain of casting Omnicide");

                        /* Absorb power of dead soul, up to twice max mana */
                        if (p_ptr->csp < p_ptr->msp * 2)
                                p_ptr->csp++;

                        /* Visual feedback */
                        move_cursor_relative(py, px);

                        /* Redraw */
                        p_ptr->redraw |= (PR_HP | PR_MANA);

                        /* Window stuff */
                        p_ptr->window |= (PW_PLAYER);
                        p_ptr->window |= (PW_SPELL);

                        /* Handle */
                        handle_stuff();

                        /* Fresh */
                        Term_fresh();

                        /* Delay */
                        Term_xtra(TERM_XTRA_DELAY,
                                delay_factor * delay_factor * delay_factor);
                }
                p_ptr->csp += 100;   /* Restore, ready to be deducted properly */

                break;
        case 31: /* Wraithform */
                set_shadow(p_ptr->wraith_form + randint(plev/2) + (plev/2));
                break;
        default:
                msg_format("You cast an unknown Death spell: %d.", spell);
                msg_print(NULL);
        }
}


static void cast_water_spell(int spell)
{
        int     dir, beam, i;
        int     plev = p_ptr->lev;
        int     dummy = 0;
        bool    no_trump = FALSE;
        object_type *q_ptr;
        object_type forge;

        if (p_ptr->pclass == CLASS_MAGE) beam = plev;
        else if (p_ptr->pclass == CLASS_HIGH_MAGE) beam = plev + 10;
        else beam = plev / 2;

        switch (spell)
        {
        case 0: /* Freezing Touch */
                if (!get_aim_dir(&dir)) return;
                fire_bolt_or_beam(beam-10, GF_COLD, dir,
                        damroll(3 + ((plev - 1) / 5), 4));
                break;
        case 1: /* Create Water */
             {
                q_ptr = &forge;
                i = lookup_kind(TV_POTION, SV_POTION_WATER);
                object_prep(q_ptr, i);
                drop_near(q_ptr, -1, py, px);
                break;
             }
        case 2: /* Detect Life */
                detect_monsters_living();
                break;
        case 3: /* Cold Screen */
                (void)set_oppose_cold(p_ptr->oppose_cold + plev * 2 + randint(20));
                break;
        case 4: /* Water Healing */
                if ((cave[py][px].feat == FEAT_DEEP_WATER) ||
                    (cave[py][px].feat == FEAT_SHAL_WATER))
                 {
                (void)hp_player(damroll((plev / 5) + 1, 10));
                (void)set_cut(p_ptr->cut - 5);
                 }
                break;
        case 5: /* Turn Water Creature */
                (void) turn_aquatic(plev);
                break;
        case 6: /* Summon Water Creature */
                if (!(summon_specific(py, px, plev, SUMMON_AQUATIC, FALSE, TRUE, TRUE)))
                    no_trump = TRUE;
                break;
        case 7: /* Vacuum Portal */
                msg_print("A cesspool of water engulfs you...");
                fire_ball(GF_COLD, 5, damroll(2+((plev-5)/4), 2), 2);
                teleport_player(plev * 5);
                break;
        case 8: /* Dispel Water Creature */
                (void)dispel_aquatic(plev);
                break;
        case 9: /* Mix Potions */
                (void)mix_potions();
                break;
        case 10: /* Cold Bolt */
                if (!get_aim_dir(&dir)) return;
                fire_bolt_or_beam(beam, GF_COLD, dir,
                        damroll(6+((plev-5)/4), 6));
                break;
        case 11: /* Cleansing */
                if ((cave[py][px].feat == FEAT_DEEP_WATER) ||
                    (cave[py][px].feat == FEAT_SHAL_WATER))
                {
                (void)set_cut(p_ptr->cut - 50);
                (void)set_confused(p_ptr->confused - 50);
                (void)set_poisoned(p_ptr->poisoned - 50);
                (void)set_blind(p_ptr->blind - 50);
                }
                break;
        case 12: /* Flood Area */
        {
                msg_print("The ceiling opens and water gushes forth!");
                (void)flood_area();
                (void)aggravate_monsters(1);
                break;
        }
        case 13: /* Transfer Life */
        {
                (void)corpse_to_weapon(FALSE);
                break;
        }
        case 14: /* Tidal Wave */
                if (!get_aim_dir(&dir)) return;
                fire_shower(GF_WATER, dir, damroll(3, 12), 12, (plev/10) + 1);
                break;
        case 15: /* Conjure Spirit - note the awful hack to always
                    force creation of one, even in the town */
                (void) summon_specific(py, px, 100, SUMMON_SPIRIT, FALSE, TRUE, TRUE);
                break;
        case 16: /* Swelling Touch */
              {
                int y, x;
                cave_type *c_ptr;

                /* Only works on adjacent monsters */

                if (!get_rep_dir(&dir)) break;   /* was get_aim_dir */
                y = py + ddy[dir];
                x = px + ddx[dir];
                c_ptr = &cave[y][x];

                if (!(c_ptr->m_idx))
                {
                        msg_print("There is no monster nearby.");
                        break;
                }
                dummy = plev + randint(plev) * MAX(1, plev/10);   /* Dmg */
                msg_print("You hit your opponent with a dehydrating force...");
                if (drain_life(dir, dummy))
                {
                        if (p_ptr->food < PY_FOOD_FULL)
                                /* No heal if we are "full" */
                                (void)hp_player(dummy);
                        else
                                msg_print("You were not thirsty.");
                                dummy = p_ptr->food + MIN(5000, 10 * dummy);
                        if (p_ptr->food < PY_FOOD_MAX)   /* Not gorged already */
                                (void)set_food(dummy >= PY_FOOD_MAX ? PY_FOOD_MAX-1 : dummy);
                }
              }
        case 17: /* Freezing Outburst */
                for (dir = 0; dir <= 9; dir++)
                        fire_beam(GF_COLD, dir, damroll(5+(plev/10), 8));
                break;
        case 18: /* Banish Water Creature */
                (void)banish_aquatic(plev * 2);
                break;
        case 19: /* Summon Greater Water Creature */
                if (!(summon_specific(py, px, plev * 2, SUMMON_AQUATIC, FALSE, TRUE, TRUE)))
                    no_trump = TRUE;
                break;
        case 20: /* Cold Branding */
                brand_weapon(EGO_BRAND_COLD, FALSE);
                break;
        case 21: /* Conjure Elemental - note the awful hack to always
                    force creation of one, even in the town */
                (void) summon_specific(py, px, 100, SUMMON_ELEMENTAL, FALSE, TRUE, TRUE);
                break;
        case 22: /* Cold Ray */
                if (!get_aim_dir(&dir)) return;
                if (randint(3)==1)
                fire_bolt_or_beam(beam, GF_ICE, dir, 100);
                else
                fire_bolt_or_beam(beam, GF_COLD, dir, 100);
                break;
        case 23: /* Greater Cleansing */
                if ((cave[py][px].feat == FEAT_DEEP_WATER) ||
                    (cave[py][px].feat == FEAT_SHAL_WATER))
                 {
                (void)set_cut(0);
                (void)set_confused(0);
                (void)set_poisoned(0);
                (void)set_blind(0);
                 }
                break;
        case 24: /* Mirror of Galadriel */
                (void)detect_all();
                break;
        case 25: /* Water Lore */
                (void)identify_fully();
                break;
        case 26: /* Transfer Life True */
                (void)corpse_to_weapon(TRUE);
                break;
        case 27: /* Undo Draining */
                (void)do_res_stat(rand_int(7));
                break;
        case 28: /* Whirlpool */
                if (!get_aim_dir(&dir)) return;
                fire_ball(GF_WATER, dir, 100 + (plev * 2), (plev/12)+1);
                break;
        case 29: /* Conjure Greater Elemental - note the awful hack to always
                    force creation of one, even in the town */
                (void) summon_specific(py, px, 100, SUMMON_ELEMENTAL2, FALSE, TRUE, TRUE);
                break;
        case 30: /* Call the Abyss */
                if (!get_aim_dir(&dir)) return;
                fire_shower(GF_WATER, dir, 100 + (plev * 2), (plev/12)+1, (plev/4)+1);
                break;
        case 31: /* ? */
                break;
        default:
                msg_format("You cast an unknown Water spell: %d.", spell);
                msg_print(NULL);
    }
        if (no_trump && alert_failure)
        {
                msg_print("Nobody answers to your call.");
        }
}


static void cast_arcane_spell(int spell)
{
        int     dir;
        int     beam;
        int     plev = p_ptr->lev;
        int     dummy = 0;
        bool    no_trump = FALSE;

        if (p_ptr->pclass == CLASS_MAGE) beam = plev;
        else if (p_ptr->pclass == CLASS_HIGH_MAGE) beam = plev + 10;
        else beam = plev / 2;

        switch (spell)
        {
    case 0: /* Magic Missile */
                if (!get_aim_dir(&dir)) return;
                fire_bolt_or_beam(beam-10, GF_MISSILE, dir,
                damroll(2, 6));
                break;
    case 1: /* Wizard Lock */
        if (!(get_aim_dir(&dir))) break;
        (void) wizard_lock(dir);
                break;
    case 2: /* Detect Invisibility */
              (void)detect_monsters_invis(TRUE);
                break;
    case 3: /* Detect Monsters */
                (void)detect_monsters_normal();
                break;
    case 4: /* Blink */
        teleport_player(10);
                break;
    case 5: /* Light Area */
                (void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
                break;
    case 6: /* Trap & Door Destruction */
        if (!(get_aim_dir(&dir))) return;
        (void) destroy_door(dir);
                break;
    case 7: /* Cure Light Wounds */
        (void) hp_player(damroll(2, 8));
        (void) set_cut(p_ptr->cut - 10);
                break;
    case 8: /* Detect Doors & Traps */
                (void)detect_traps();
                (void)detect_doors();
                (void)detect_stairs();
                break;
    case 9: /* Reinforce Mana */
                tap_magical_energy();
                break;
    case 10: /* Remove Curse */
                remove_curse();
                break;
    case 11: /* Detect Magic */
                (void)detect_objects_magic();
                break;
    case 12: /* Stone to Mud */
                if (!get_aim_dir(&dir)) return;
                (void)wall_to_mud(dir);
                break;
    case 13: /* Identify */
                (void)ident_spell();
                break;
    case 14: /* Extrasensory Perception */
                (void)set_tim_esp(p_ptr->tim_esp + randint(30) + 25);
                break;
    case 15: /* Cure Poison */
                (void)set_poisoned(0);
                break;
    case 16: /* Summon Lesser Servant */
        {
                bool pet = (randint(5) > 2);
                int type = (pet ? SUMMON_NO_UNIQUES : 0);

                if (summon_specific(py, px, plev, type, FALSE, FALSE, pet))
                {
                        if (!pet)
                                msg_print("The summoned creature gets angry!");
                }
                else
                {
                        no_trump = TRUE;
                }

                break;
        }
        case 17: /* Elemental Resistance */
                (void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
                (void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
                (void)set_oppose_elec(p_ptr->oppose_elec + randint(20) + 20);
                (void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20);
                break;
        case 18: /* Cure Medium Wounds */
        (void)hp_player(damroll(4, 8));
        (void)set_cut((p_ptr->cut / 2) - 50);
                break;
        case 19: /* Teleport */
        teleport_player(plev * 5);
                break;
        case 20: /* Shuffle */
                {
                        /* A limited power 'wonder' spell */

                        int die = randint(22);

                        msg_print("You shuffle the deck and draw a card...");

                   switch(die)
                   {
                        case 1:
                        {
                                msg_print("It's the Fool");
                                (void) do_dec_stat(A_INT, STAT_DEC_NORMAL);
                                (void) do_dec_stat(A_WIS, STAT_DEC_NORMAL);
                                break;
                        }
                        case 2:
                        {
                                msg_print("It's the Mage");
                                if ((p_ptr->pclass == CLASS_MAGE) ||
                                    (p_ptr->pclass == CLASS_HIGH_MAGE))
                                p_ptr->msp += (p_ptr->msp / 10);
                                p_ptr->csp = p_ptr->msp;
                                break;
                        }
                        case 3:
                        {
                                msg_print("It's the High Priestess");
                                if ((p_ptr->pclass == CLASS_PRIEST) &&
                                    (p_ptr->psex == SEX_FEMALE))
                                p_ptr->msp += (p_ptr->msp / 10);
                                p_ptr->csp = p_ptr->msp;
                                break;
                        }
                        case 4:
                        {
                                msg_print("It's the Empress");
                                if (p_ptr->psex == SEX_FEMALE)
                                (void)do_inc_stat(A_WIS);
                                break;
                        }
                        case 5:
                        {
                                msg_print("It's the Emperor");
                                if (p_ptr->psex == SEX_MALE)
                                (void)do_inc_stat(A_WIS);
                                break;
                        }
                        case 6:
                        {
                                msg_print("It's the High Priest");
                                if ((p_ptr->pclass == CLASS_PRIEST) &&
                                    (p_ptr->psex == SEX_MALE))
                                p_ptr->msp += (p_ptr->msp / 10);
                                p_ptr->csp = p_ptr->msp;
                                break;
                        }
                        case 7:
                        {
                                msg_print("It's the Lovers");
                                if (!get_aim_dir(&dir)) return;
                                (void) charm_monster(dir, MIN(p_ptr->lev, 20));
                                break;
                        }
                        case 8:
                        {
                                msg_print("It's the Chariot");
                                if (!p_ptr->fast)
                                {
                                  (void)set_fast(randint(20 + (plev) ) + plev);
                                }
                                else
                                {
                                  (void)set_fast(p_ptr->fast + randint(5));
                                }
                                break;
                        }
                        case 9:
                        {
                                msg_print("It's Strength");
                                (void)do_inc_stat(A_STR);
                                break;
                        }
                        case 10:
                        {   /* ??? Monks don't use arcane */
                                msg_print("It's the Hermit");
                                if (p_ptr->pclass == CLASS_MONK)
                                p_ptr->msp += (p_ptr->msp / 10);
                                p_ptr->csp = p_ptr->msp;
                                break;
                        }
                        case 11:
                        {
                                msg_print("It's the Wheel of Fortune.");
                                wild_magic(rand_int(32));
                                break;
                        }
                        case 12:
                        {
                                msg_print("It's Justice.");
                                set_blessed(p_ptr->blessed + p_ptr->lev);
                                break;
                        }
                        case 13:
                        {
                                msg_print("Oh no! It's the Hanged Man.");
                                (void)activate_ty_curse(FALSE);
                                break;
                        }
                        case 14:
                        {
                                msg_print("Oh, no! It's Death!");
                                for (dummy = 0; dummy < randint(3); dummy++)
                                        (void)activate_hi_summon();
                                break;
                        }
                        case 15:
                        {
                                msg_print("It's Temperance.");
                                sleep_monsters_touch();
                        }
                        case 16:
                        {
                                msg_print("Oh no! It's the Devil!");
                                (void) summon_specific(py, px, dun_level, SUMMON_DEMON, FALSE, FALSE, FALSE);
                                break;
                        }
                        case 17:
                        {
                                msg_print("It's the Tower.");
                                wall_breaker();
                                break;
                        }
                        case 18:
                        {
                                msg_print("It's the Star.");
                                for (dir = 0; dir <= 9; dir++)
                                   fire_beam(randint(5), dir, damroll(5+(plev/10), 8));
                                break;
                        }
                        case 19:
                        {
                                msg_print("It's the Moon.");
                                unlite_area(10,3);
                        }
                        case 20:
                        {
                                msg_print("It's the Sun.");
                                wiz_lite();
                                break;
                        }
                        case 21:
                        {
                                msg_print("It's the Judgement.");
                                do_cmd_rerate();
                                if (p_ptr->muta1 || p_ptr->muta2 || p_ptr->muta3)
                                {
                                        msg_print("You are cured of all mutations.");
                                        p_ptr->muta1 = p_ptr->muta2 = p_ptr->muta3 = 0;
                                        p_ptr->update |= PU_BONUS;
                                        handle_stuff();
                                }
                                break;
                        }
                        case 22:
                        {
                                msg_print("It's the World.");
                                if (p_ptr->exp < PY_MAX_EXP)
                                {
                                        s32b ee = (p_ptr->exp / 25) + 1;
                                        if (ee > 5000) ee = 5000;
                                        msg_print("You feel more experienced.");
                                        gain_exp(ee);
                                }
                                break;
                        }
                        default:
                        {
                                msg_format("Unknown arcane card %d", die);
                                break;
                        }
                }
                break;
             }
       case 21: /* Ray of Light */
                if (!get_aim_dir(&dir)) return;
        msg_print("A line of light appears.");
                lite_line(dir);
                break;
    case 22: /* Satisfy Hunger */
                (void)set_food(PY_FOOD_MAX - 1);
                break;
    case 23: /* War Branding */
                brand_weapon(EGO_SLAYING, FALSE);
                break;
    case 24: /* Recharging */
                (void)recharge(plev * 2);
                break;
    case 25: /* Teleport Level */
                (void)teleport_player_level();
                break;
    case 26: /* Starburst */
                for (dir = 0; dir <= 9; dir++)
                        fire_beam(GF_LITE, dir, damroll(5+(plev/10), 8));
                break;
    case 27: /* Teleport Away */
                if (!get_aim_dir(&dir)) return;
                (void)fire_beam(GF_AWAY_ALL, dir, plev);
                break;
    case 28: /* Elemental Ball */
                if (!get_aim_dir(&dir)) return;
        switch (randint(4))
        {
                        case 1: dummy = GF_FIRE;
                        case 2: dummy = GF_ELEC;
                        case 3: dummy = GF_COLD;
                        default: dummy = GF_ACID;
        }
        fire_ball(dummy, dir, 75 + (plev), 2);
                break;
        case 29: /* Detection */
                (void)detect_all();
                break;
        case 30: /* Word of Recall */
                recall_player();
                break;
        case 31: /* Summon Greater Servant */
        {
                bool pet = (randint(5) > 2);
                int type = (pet ? SUMMON_NO_UNIQUES : 0);
                bool group = (pet ? FALSE : TRUE);

                if (summon_specific(py, px, (plev * 3) / 2, type, group, FALSE, pet))
                {
                        if (!pet)
                                msg_print("The summoned creature gets angry!");
                }
                else
                {
                        no_trump = TRUE;
                }

                break;
        }
        default:
                msg_format("You cast an unknown Arcane spell: %d.", spell);
                msg_print(NULL);
     }

        if (no_trump && alert_failure)
        {
                msg_print("Nobody answers to your call.");
        }

}


/*
 * Cast a spell
 */
void do_cmd_cast(void)
{
        int     item, sval, spell, realm;
        int     chance;
        int     increment = 0;
        int     use_realm;
        char    silly[80];
        const cptr prayer = ((mp_ptr->spell_book == TV_LIFE_BOOK) ? "prayer" : "spell");
        object_type     *o_ptr;
        magic_type      *s_ptr;
        cptr q, s;

        /* Require spell ability */
        if (!p_ptr->realm1)
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

        if (o_ptr->tval == p_ptr->realm2+89) increment = 32;


        /* Track the object kind */
        object_kind_track(o_ptr->k_idx);

        /* Hack -- Handle stuff */
        handle_stuff();

        if (increment) realm = p_ptr->realm2;
        else realm = p_ptr->realm1;

        /* Ask for a spell */
        if (!get_spell(&spell, ((mp_ptr->spell_book == TV_LIFE_BOOK) ? "recite" : "cast"),
                sval, TRUE, (bool)(increment?TRUE:FALSE)))
        {
                if (spell == -2)
                        msg_format("You don't know any %ss in that book.", prayer);
                return;
        }


        /* Access the spell */
        use_realm = (increment?p_ptr->realm2:p_ptr->realm1);

        s_ptr = &mp_ptr->info[use_realm-1][spell];


        /* Verify "dangerous" spells */
        if (s_ptr->smana > p_ptr->csp)
        {
                /* Warning */
                msg_format("You do not have enough mana to %s this %s.",
                        ((mp_ptr->spell_book == TV_LIFE_BOOK) ? "recite" : "cast"),
                        prayer);

                /* Verify */
                if (!get_check("Attempt it anyway? ")) return;
        }


        /* Spell failure chance */
        chance = spell_chance(spell,use_realm-1);

        /* Failed spell */
        if (rand_int(100) < chance)
        {
                if (flush_failure) flush();

                msg_format("You failed to get the %s off!", prayer);
                sound(SOUND_FAIL);
                get_rnd_line("sfail.txt", silly);
                msg_format("A cloud of %s appears above you.", silly);

                if (o_ptr->tval == TV_FIRE_BOOK && (randint(100)<spell))
                {
                        msg_print("You produce a chaotic effect!");
                        wild_magic(spell);
                }
                else if (o_ptr->tval == TV_DEATH_BOOK && (randint(100)<spell))
                {
                        if ((sval == 3) && (randint(2)==1))
                        {
                                sanity_blast(0, TRUE);
                        }
                        else
                        {
                                msg_print("It hurts!");
                                take_hit(damroll((o_ptr->sval)+1,6), "a miscast Death spell");
                                if (spell>15 && randint(6)==1 && !(p_hold_life))
                                        lose_exp(spell * 250);
                        }
                }
        }

        /* Process spell */
        else
        {
                /* Spells.  */
                switch (realm)
                {
                case 1: /* * LIFE * */
                        cast_life_spell(spell);
                        break;
                case 2: /* * AIR * */
                        cast_air_spell(spell);
                        break;
                case 3: /* * EARTH * */
                        cast_earth_spell(spell);
                        break;
                case 4: /* * FIRE * */
                        cast_fire_spell(spell);
                        break;
                case 5: /* * DEATH * */
                        cast_death_spell(spell);
                        break;
                case 6: /* * WATER * */
                        cast_water_spell(spell);
                        break;
                case 7: /* * ARCANE * */
                        cast_arcane_spell(spell);
                        break;
                default:
                        msg_format("You cast a spell from an unknown realm: realm %d, spell %d.", realm, spell);
                        msg_print(NULL);
                }

                /* A spell was cast */
                if (!((increment) ?
                        (spell_worked2 & (1L << spell)) :
                (spell_worked1 & (1L << (spell)))))
                {
                        int e = s_ptr->sexp;

                        /* The spell worked */
                        if (realm == p_ptr->realm1)
                        {
                                spell_worked1 |= (1L << spell);
                        }
                        else
                        {
                                spell_worked2 |= (1L << spell);
                        }

                        /* Gain experience */
                        gain_exp(e * s_ptr->slevel);
                }
        }

        /* Take a turn */
        energy_use = 100;

        /* Time passes... */
        sc_time += randint(10);

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

                /* Damage CON or WIS (possibly permanently) */
                if (rand_int(100) < 50)
                {
                        bool was_prayer = ((p_ptr->pclass == CLASS_PRIEST) ||
                                           (p_ptr->pclass == CLASS_PALADIN))
                                                          ? TRUE : FALSE;
                        bool perm = (rand_int(100) < 25);

                        /* Message */
                        msg_print("You have damaged your health!");

                        /* Reduce constitution or wisdom */
                        if (was_prayer)
                        (void)dec_stat(A_WIS, 15 + randint(10), perm);
                        else
                        (void)dec_stat(A_CON, 15 + randint(10), perm);
                }
        }

        /* Redraw mana */
        p_ptr->redraw |= (PR_MANA);

        /* Window stuff */
        p_ptr->window |= (PW_PLAYER);
        p_ptr->window |= (PW_SPELL);
}


#if 0

/*
 * Command to ask favors from your god.
 */

void do_cmd_pray(void) {
  int level;
  cptr name;

  if (p_ptr->pgod == 0) {
    msg_print("Pray hard enough and your prayers might be answered.");
    return;
  }

  if (p_ptr->champion)
  {
    msg_print("You're in close communion with your deity anyway.");
    return;
  }

  if (!get_check("Are you sure you want to disturb your deity? ")) return;

  level = interpret_grace();
  name = deity_info[p_ptr->pgod-1].name;

  if (p_ptr->pclass == CLASS_PRIEST && magik(90)) {
    level++;
  }

  if (p_ptr->sign == SIGN_PISCES) {
    level++;
  }

  if (p_ptr->pclass == CLASS_PALADIN && magik(30)) {
    level++;
  }

  if (level < 0) level = 0;
  if (level > 10) level = 10;

  energy_use = 100;

  /* Time passes... */
  sc_time += randint(20);

  switch (level)
  {
  case 9: case 10: /* crowning ! */
    if ((cave[py][px].feat >= FEAT_ALTAR_HEAD) && (cave[py][px].feat
         <= FEAT_ALTAR_TAIL))
    crowning();
    else msg_print("Perhaps you should find an altar and pray on it.");
    break;

  case 7: case 8:
    msg_format("%s thunders: ``Thou verily art a pious follower.''", name);
    great_side_effect();
    set_grace(p_ptr->grace - 20000);
    break;

  case 6: case 5: case 4:
    msg_format("%s thunders: ``Thou hast pleaseth me, mortal.''", name);
    good_side_effect();
    set_grace(p_ptr->grace - 10000);
    break;

  case 3:
    msg_format("%s hisses: ``Leave me alone now, mortal!''", name);
    if (magik(30)) set_grace(p_ptr->grace - 1000);
    break;

  case 2:
  case 1:
    msg_format("%s quakes in rage: ``Thou art supremely insolent, mortal!''", name);
    nasty_side_effect();
    set_grace(p_ptr->grace - 10000);
    break;

  case 0:
    msg_format("%s whispers: ``Prepare to die, mortal...''", name);
    deadly_side_effect(TRUE);
    set_grace(p_ptr->grace - 20000);
    break;
  }

}

#endif

void mindcraft_info(char *p, int power)
{
    int plev = p_ptr->lev;

    strcpy(p, "");

    switch (power) {
        case 0:  break;
        case 1:  break;
        case 2:  sprintf(p, " dam %dd%d", 3 + ((plev - 1) / 4), 3 + plev/15); break;
        case 3:  sprintf(p, " power %d", 2 * plev); break;
        case 4:  strcpy(p, " heal 2d6"); break;
        case 5:  sprintf(p, " range %d", plev); break;
        case 6:  sprintf(p, " dur %d", plev); break;
        case 7:  sprintf(p, " dam 3d%d", plev);  break;
        case 8:  break;
        case 9:  if (plev > 30)
                 sprintf (p, " power %d ", plev);
                 else sprintf(p, " range %d", 4 * plev); break;
        case 10: strcpy (p, " toggle"); break;
        case 11:  if (plev > 30)
                 sprintf (p, " power %d ", 2* plev);
                 else sprintf(p, " range %d", plev); break;
        case 12: strcpy (p, " toggle"); break;
        case 13: strcpy (p, " toggle"); break;
        case 14: sprintf(p, " dam %dd8", 4+((plev-5) / 4)); break;
        case 15: sprintf(p, " dur %d", plev); break;
        case 16: break;
        case 17: strcpy (p, " toggle"); break;
        case 18: sprintf(p, " dam %d", plev * 3 / 2); break;
        case 19: sprintf(p, " dam %dd12", plev); break;
        case 20: sprintf(p, " dam %dd2", plev); break;
        case 21: break;
        case 22: sprintf(p, " range %d", plev + 2); break;
        case 23: strcpy (p, " multiple"); break;
        case 24: strcpy (p, " toggle"); break;
        case 25: sprintf(p, " dam %dd6", plev / 2); break;
        case 26: sprintf(p, " dur %d", plev); break;
        case 27: break;
        case 28: sprintf(p, " dam %d", plev * 3); break;
        case 29: sprintf(p, " dur %d", plev); break;
        case 30: strcpy (p, " toggle"); break;
        case 31: sprintf(p, " dam %d", 3 * p_ptr->chp); break;
        case 32: sprintf(p, " range %d", 2 * plev + 15); break;
        case 33: strcpy (p, " range 2"); break;
    }
}


/*
 * Allow user to choose a mindcrafter power.
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
static int get_mindcraft_power(int *sn)
{
        int             i, j, chosen = 0;
        int             num = 0, num2 = -1;
        int             y = 1;
        int             x = 20;
        int             minfail = 0;
        int             plev = p_ptr->lev;
        int             chance = 0;
        int             ask;
        int             found = 0;
        char            choice;
        char            out_val[160];
        char            comment[80];
        cptr            p = "power";
        mindcraft_power spell;
        object_type     *o_ptr = &inventory[INVEN_NECK];
        bool            flag, redraw;

        /* Assume cancelled */
        *sn = (-1);

#ifdef ALLOW_REPEAT /* TNB */

        /* Get the spell, if available */
        if (repeat_pull(sn))
        {
                /* Verify the spell */
                if (mindcraft_powers[*sn].min_lev <= plev)
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

        for (i = 0; i < MAX_MINDCRAFT_POWERS; i++)
        {
                if ((mindcraft_powers[i].min_lev <= plev) &&
                   (mindcraft_powers[i].realm == o_ptr->sval))
                {
                        num++;
                }
        }

        if (num == 0)
        {
            msg_print ("You have learned no powers from this focus yet.");
            return (FALSE);
        }

        /* Build a prompt (accept all spells) */
        (void)strnfmt(out_val, 78, "(%^ss %c-%c, *=List, ESC=exit) Use which %s? ",
                p, I2A(0), I2A(num - 1), p);

        /* Get a spell from the user */
        while (!flag && get_com(out_val, &choice))
        {
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
                                put_str("Lv Mana Fail Info", y, x + 35);

                                /* Dump the spells */
                                for (i = 0; i < MAX_MINDCRAFT_POWERS; i++)
                                {

                                        /* Access the spell */
                                        spell = mindcraft_powers[i];
                                        if (spell.min_lev > plev)   break;
                                        if (spell.realm != o_ptr->sval) continue;
                                        else found++;

                                        chance = spell.fail;

                                        /* Reduce failure rate by "effective" level adjustment */
                                        chance -= 3 * (p_ptr->lev - spell.min_lev);

                                        /* Reduce failure rate by INT/WIS adjustment */
                                        chance -= 3 * (adj_mag_stat[p_ptr->stat_ind[mp_ptr->spell_stat]] - 1);

                                        /* Not enough mana to cast */
                                        if (spell.mana_cost > p_ptr->csp)
                                        {
                                            chance += 5 * (spell.mana_cost - p_ptr->csp);
                                        }

                                        /* Extract the minimum failure rate */
                                        minfail = adj_mag_fail[p_ptr->stat_ind[mp_ptr->spell_stat]];

                                        /* Minimum failure rate */
                                        if (chance < minfail) chance = minfail;

                                        /* Stunning makes spells harder */
                                        if (p_ptr->stun > 50) chance += 25;
                                        else if (p_ptr->stun) chance += 15;

                                        /* Always a 5 percent chance of working */
                                        if (chance > 95) chance = 95;

                                        /* Get info */
                                        mindcraft_info(comment, i);

                                        /* Dump the spell --(-- */
                                        sprintf(psi_desc, "  %c) %-30s%2d %4d %3d%%%s",
                                        I2A(found-1), spell.name, spell.min_lev,
                                        spell.mana_cost, chance, comment);
                                        prt(psi_desc, y + found, x);
                                }

                                /* Clear the bottom line */
                                prt("", y + found + 1, x);
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
                ask = (isupper(choice));

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
                for (j = 0; j < MAX_MINDCRAFT_POWERS; j++)
                {
                    if (mindcraft_powers[j].realm == o_ptr->sval)
                    {
                        num2++;
                        if (i == num2)
                           chosen = j;
                    }
                }


                /* Verify it */
                if (ask)
                {
                        char tmp_val[160];

                        /* Prompt */
                        (void)strnfmt(tmp_val, 78, "Use %s? ", mindcraft_powers[chosen].name);

                        /* Belay that order */
                        if (!get_check(tmp_val)) continue;
                }

                /* Stop the loop */
                flag = TRUE;
        }

        /* Restore the screen */
        if (redraw) screen_load();

        /* Show choices */
        if (show_choices)
        {
                /* Update */
                p_ptr->window |= (PW_SPELL);

                /* Window stuff */
                window_stuff();
        }

        /* Abort if needed */
        if (!flag) return (FALSE);

        /* Save the choice */
        (*sn) = chosen;

#ifdef ALLOW_REPEAT /* TNB */

        repeat_push(*sn);

#endif /* ALLOW_REPEAT -- TNB */

        /* Success */
        return (TRUE);
}


/*
 * do_cmd_cast calls this function if the player's class
 * is 'mindcrafter'.
 */
void do_cmd_mindcraft(void)
{
        int             n = 0,  b = 0;
        int             chance;
        int             dir;
        int             minfail = 0;
        int             tempdice;
        int             plev = p_ptr->lev;
        bool            nomana = FALSE;
        bool            myrealm = FALSE;
        mindcraft_power spell;
        object_type     *o_ptr = &inventory[INVEN_NECK];

        /* not if confused */
        if (p_ptr->confused)
        {
                msg_print("You are too confused!");
                return;
        }

        if (o_ptr->tval != TV_FOCUS)
        {
            msg_print ("You must wear a focus to use the power of the mind.");
            return;
        }

        /* get power */
        if (!get_mindcraft_power(&n))  return;

        spell = mindcraft_powers[n];

        myrealm = (p_ptr->realm1 == spell.realm) ? TRUE : FALSE;

        /* Verify "dangerous" spells */
        if (spell.mana_cost > p_ptr->csp)
        {
                /* Warning */
                msg_print("You do not have enough mana to use this power.");

                /* Verify */
                if (!get_check("Attempt it anyway? ")) return;
        }

        /* Spell failure chance */
        chance = spell.fail;

        /* Reduce failure rate by "effective" level adjustment */
        chance -= 3 * (p_ptr->lev - spell.min_lev);

        /* Reduce failure rate by INT/WIS adjustment */
        chance -= 3 * (adj_mag_stat[p_ptr->stat_ind[mp_ptr->spell_stat]] - 1);

        /* Not enough mana to cast */
        if (spell.mana_cost > p_ptr->csp)
        {
                chance += 5 * (spell.mana_cost - p_ptr->csp);
        }

        /* Extract the minimum failure rate */
        minfail = adj_mag_fail[p_ptr->stat_ind[mp_ptr->spell_stat]];

        /* Minimum failure rate */
        if (chance < minfail) chance = minfail;

        /* Stunning makes spells harder */
        if (p_ptr->stun > 50) chance += 25;
        else if (p_ptr->stun) chance += 15;

        /* Always a 5 percent chance of working */
        if (chance > 95) chance = 95;

        /* Failed spell */
        if (rand_int(100) < chance)
        {
                if (flush_failure) flush();
                msg_format("You failed to concentrate hard enough!");
                sound(SOUND_FAIL);

                if (randint(100) < (chance/2))
                {
                        /* Backfire */
                        b = randint(100);
                        if (b < 5)
                        {
                                msg_print("Oh, no! Your mind has gone blank!");
                                lose_all_info();
                        }
                        else if (b < 15)
                        {
                                msg_print("Weird visions seem to dance before your eyes...");
                                set_image(p_ptr->image + 5 + randint(10));
                        }
                        else if (b < 45)
                        {
                                msg_print("Your brain is addled!");
                                set_confused(p_ptr->confused + randint(8));
                        }
                        else if (b < 90)
                        {
                                set_stun(p_ptr->stun + randint(8));
                        }
                        else
                        {
                                /* Mana storm */
                                msg_print("Your mind unleashes its power in an uncontrollable storm!");
                                project(1, 2+plev/10, py, px, plev * 2,
                                        GF_MANA,PROJECT_JUMP|PROJECT_KILL|PROJECT_GRID|PROJECT_ITEM);
                                p_ptr->csp = MAX(0, p_ptr->csp - plev * MAX(1, plev/10));
                        }
                }
        }
        else
        {
                sound(SOUND_ZAP);

                /* spell code */
                switch (n)
                {
                case 0:   /* Awareness */

                        b = detect_minds();

                        if (!b) msg_print("You feel safe.");
                        break;
                case 1:   /* Detect Invis */

                        b = detect_monsters_invis(FALSE);

                        if (!b) msg_print("You feel safe.");
                        break;
                case 2:
                        /* Neural Blast */
                        if (!get_aim_dir(&dir)) return;
                        tempdice = (myrealm) ? 5 : 3;
                        if (randint(100) < plev * 2)
                                fire_beam(GF_PSI, dir, damroll(tempdice + ((plev - 1) / 4), (3+plev/15)));
                        else
                                fire_ball(GF_PSI, dir, damroll(tempdice + ((plev - 1) / 4), (3+plev/15)), 0);
                        break;
                case 3:
                        {
                        if (!get_aim_dir(&dir)) return;
                        tempdice = (myrealm) ? 3 : 2;
                        fear_monster(dir,plev * tempdice);
                        break;
                        }
                case 4: /* Cell Adjustment */
                        {
                                (void)hp_player(damroll(plev / 2, 6));
                                (void)set_cut(p_ptr->cut - 15);
                                break;
                        }

                case 5:
                        tempdice = (myrealm) ? (plev + 10) : 10;

                        teleport_player(tempdice);
                        break;
                case 6:
                        tempdice = (myrealm) ? (plev + 10) : plev;

                        detect_heat();
                        set_tim_infra(tempdice * randint(10));
                        break;

                case 7: /* Project Force */
                        {
                        tempdice = (myrealm) ?  5 : 3;

                                if (!get_aim_dir(&dir)) return;
                                if (plev > 30) plev = 30 + (plev - 30) / 4;
                                fire_beam(GF_FORCE,dir,damroll(tempdice,plev));
                                break;
                        }
                case 8: /* Detect feats */
                        {
                            (void)detect_doors();
                            (void)detect_stairs();
                            (void)detect_traps();
                            break;
                        }
                case 9:
                        tempdice = (myrealm) ?  7 : 4;

                        /* Major displace */
                        if (plev > 29)
                                banish_monsters(plev);
                        teleport_player(plev * tempdice);
                        break;
                case 10:
                        if (!(p_ptr->mind & MIND_MENTAL_BARRIER))
                        {
                            msg_print("You activate your mental barrier.");
                            p_ptr->mind |= (MIND_MENTAL_BARRIER);
                        }
                        else
                        {
                            msg_print("You disable your mental barrier.");
                            p_ptr->mind &= ~(MIND_MENTAL_BARRIER);
                            nomana = TRUE;
                        }
                        break;
                case 11:
                        tempdice = (myrealm) ?  3 : 2;

                        /* Domination */
                        if (plev < 30)
                        {
                                if (!get_aim_dir(&dir)) return;
                                fire_ball(GF_DOMINATION, dir, plev, 0);
                        }
                        else
                        {
                                charm_monsters(plev * tempdice);
                        }
                        break;
                case 12:
                        if (!(p_ptr->mind & MIND_BIOFEEDBACK))
                        {
                            msg_print("Your pulse slows and your body prepares to resist damage.");
                            p_ptr->mind |= (MIND_BIOFEEDBACK);
                        }
                        else
                        {
                            msg_print("Your pulse returns to normal.");
                            p_ptr->mind &= ~(MIND_BIOFEEDBACK);
                            nomana = TRUE;
                        }
                        break;
                case 13:
                        if (!(p_ptr->mind & MIND_INERTIAL_BARRIER))
                        {
                        msg_print("The air around you solidifies into a protective shield.");
                            p_ptr->mind |= (MIND_INERTIAL_BARRIER);
                        }
                        else
                        {
                        msg_print("The air around you returns to normal.");
                            p_ptr->mind &= ~(MIND_INERTIAL_BARRIER);
                            nomana = TRUE;
                        }
                        break;
                case 14:
                        tempdice = (myrealm) ?  8 : 4;

                        /* Pulverize */
                        if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_SOUND, dir, damroll(tempdice+((plev-5)/4), 8),
                                (plev > 20 ? (plev-20)/8 + 1 : 0));
                        break;
                case 15:
                        tempdice = (myrealm) ?  plev : (plev + 25);

                        /* Character Armour */
                        set_shield(p_ptr->shield + tempdice);
                        break;
                case 16:
                        /* Psychometry */
                        if (plev < 40)
                                psychometry();
                        else
                                ident_spell();
                        break;
                case 17:
                        if (!(p_ptr->mind & MIND_ENERGY_CONT))
                        {
                        msg_print("You prepare to resist the elements.");
                            p_ptr->mind |= (MIND_ENERGY_CONT);
                        }
                        else
                        {
                        msg_print("You let off your elemental control.");
                            p_ptr->mind &= ~(MIND_ENERGY_CONT);
                            nomana = TRUE;
                        }
                        break;

                case 18:
                        tempdice = (myrealm) ?  5 : 3;

                        /* Mindwave */
                        msg_print("Mind-warping forces emanate from your brain!");
                        if (plev < 25)
                                project(0, 2+plev/10, py, px,
                                (plev*tempdice)/2, GF_PSI, PROJECT_KILL);
                        else
                                (void)mindblast_monsters(plev * ((plev-5+tempdice) / 10 + 1));
                        break;

                case 19: /* Detonate */
                        {
                        tempdice = (myrealm) ?  20 : 12;

                                if (!get_aim_dir(&dir)) return;
                                fire_ball(GF_METEOR,dir,damroll(tempdice,plev),2);
                                break;
                        }

                case 20:
                        {
                        tempdice = (myrealm) ?  3 : 2;

                                if (!get_aim_dir(&dir)) return;
                                fire_ball(GF_AMNESIA,dir,plev +
                                  (rand_int(2) ? randint(plev * tempdice) : 0),0);
                                break;
                        }
                case 21: /* Read Object */
                        {
                            (void)ident_spell();
                            break;
                        }
                case 22: /* D-door */
                        {

                                int i = 0, j = 0;
                                msg_print("Choose a destination.");
                                if (!tgt_pt(&i,&j)) return;
                                p_ptr->energy -= 60 - plev;
                                if (!cave_empty_bold(j,i) || (cave[j][i].info & CAVE_ICKY) ||
                                        (distance(j,i,py,px) > plev + 2) ||
                                        (!rand_int(plev * plev / 2)))
                                {
                                        msg_print("Something disrupts your concentration!");
                                        p_ptr->energy -= 100;
                                        teleport_player(20);
                                }
                                else teleport_player_to(j,i);
                                break;
                        }

                case 23:
                        /* Adrenaline */

                        tempdice = (myrealm) ?  3 : 2;

                        set_afraid(0);
                        set_stun(0);
                        hp_player(plev);
                        b = 10 + randint((plev*tempdice)/2);
                        if (plev < 35)
                                set_hero(p_ptr->hero + b);
                        else
                                set_shero(p_ptr->shero + b);

                        if (!p_ptr->fast)
                        {
                                /* Haste */
                                (void)set_fast(b);
                        }
                        else
                        {
                                (void)set_fast(p_ptr->fast + b);
                        }
                        break;
                case 24:
                        if (!(p_ptr->mind & MIND_PROB_TRAVEL))
                        {
                        msg_print("You enter the astral plane!");
                            p_ptr->mind &= (MIND_PROB_TRAVEL);
                        }
                        else
                        {
                        msg_print("You exit the astral plane.");
                            p_ptr->mind |= ~(MIND_PROB_TRAVEL);
                            nomana = TRUE;
                        }
                        break;
                case 25:

                        /* Psychic Drain */
                        if (!get_aim_dir(&dir)) return;
                        b = damroll(plev/2, 6);
                        if (fire_ball(GF_PSI_DRAIN, dir, b,  0 +
                                (plev-25)/10))
                                p_ptr->energy -= randint(150);
                        break;
                case 26: /* Shadowform */
                        {
                        tempdice = (myrealm) ?  plev : plev - 5;

                        msg_print("You leave the physical world and become a living shadow!");
                        p_ptr->wraith_form += randint(randint(plev + tempdice));
                        break;
                        }
                case 27: /* Probing */
                        probing();
                        break;
                case 28:
                        tempdice = (myrealm) ?  4 : 3;

                        /* Telekinesis */
                        msg_print("A wave of pure physical force radiates out from your body!");
                        project(0, 3+plev/10, py, px,
                                (plev * tempdice), GF_TELEKINESIS, PROJECT_KILL|PROJECT_ITEM|PROJECT_GRID);
                        break;
                case 29: /* Precognition */
                {
                        tempdice = (myrealm) ?  plev : plev-5;

                        msg_print("Images from the future begin to flash before your eyes.");
                        p_ptr->precog += randint(tempdice);
                        break;
                }
                case 30:
                        if (!(p_ptr->mind & MIND_TS_ANCHOR))
                        {
                        msg_print("You warp the timespace around you!");
                            p_ptr->mind |= (MIND_TS_ANCHOR);
                        }
                        else
                        {
                        msg_print("Your surroundings return to normal.");
                            p_ptr->mind &= ~(MIND_TS_ANCHOR);
                            nomana = TRUE;
                        }
                        break;
                case 31:
                        {
                        tempdice = (myrealm) ?  5 : 3;

                                if (!get_aim_dir(&dir)) return;
                                fire_beam(GF_FIRE,dir,damroll(tempdice,p_ptr->chp));
                                break;
                        }
                case 32:
                        {
                        tempdice = (myrealm) ?  3 : 2;

                                if (!get_aim_dir(&dir)) return;
                                fire_ball(GF_PAIN,dir,randint(plev * tempdice + 15),0);
                                break;
                        }
                case 33:  /* Super ID */
                        {
                        tempdice = (myrealm) ?  3 : 2;

                            project(0, tempdice, py, px, plev, GF_IDENTIFY, PROJECT_KILL | PROJECT_JUMP | PROJECT_ITEM);
                            break;
                        }
                default:
                        msg_print("Zap?");
                }
        }

        /* Take a turn */
        energy_use = 100;

        /* Sufficient mana */
        if (spell.mana_cost <= p_ptr->csp)
        {
                /* Use some mana */
                if (!nomana) p_ptr->csp -= spell.mana_cost;
        }

        /* Over-exert the player */
        else
        {
                int oops = spell.mana_cost - p_ptr->csp;

                /* No mana left */
                p_ptr->csp = 0;
                p_ptr->csp_frac = 0;

                /* Message */
                msg_print("You faint from the effort!");

                /* Hack -- Bypass free action */
                (void)set_paralyzed(p_ptr->paralyzed + randint(5 * oops + 1));

                /* Damage WIS (possibly permanently) */
                if (rand_int(100) < 50)
                {
                        bool perm = (rand_int(100) < 25);

                        /* Message */
                        msg_print("You have damaged your mind!");

                        /* Reduce constitution */
                        (void)dec_stat(A_WIS, 15 + randint(10), perm);
                }
        }

        /* Time passes... */
        sc_time += randint(10);

        /* Redraw mana */
        p_ptr->redraw |= (PR_MANA);

        /* Window stuff */
        p_ptr->window |= (PW_PLAYER);
        p_ptr->window |= (PW_SPELL);
}

/*
 * Issue a pet command
 */
void do_cmd_pet(void)
{
        int                     i = 0;
        int                     num;
        int                     powers[36];
        cptr                    power_desc[36];
        bool                    flag, redraw;
        int                     ask;
        char                    choice;
        char                    out_val[160];
        int                     pets = 0, pet_ctr;
        bool                    all_pets = FALSE;
        monster_type    *m_ptr;

        int mode = 0;

        byte y = 1, x = 0;
        int ctr = 0;
        char buf[160];

        num = 0;

        /* Calculate pets */
        /* Process the monsters (backwards) */
        for (pet_ctr = m_max - 1; pet_ctr >= 1; pet_ctr--)
        {
                /* Access the monster */
                m_ptr = &m_list[pet_ctr];

                if (is_pet(m_ptr)) pets++;
        }

        if (pets)
        {
                power_desc[num] = "dismiss pets";
                powers[num++] = PET_DISMISS;
        }

        power_desc[num] = "stay close";
        if (p_ptr->pet_follow_distance == PET_CLOSE_DIST) mode = num;
        powers[num++] = PET_STAY_CLOSE;

        power_desc[num] = "follow me";
        if (p_ptr->pet_follow_distance == PET_FOLLOW_DIST) mode = num;
        powers[num++] = PET_FOLLOW_ME;

        power_desc[num] = "seek and destroy";
        if (p_ptr->pet_follow_distance == PET_DESTROY_DIST) mode = num;
        powers[num++] = PET_SEEK_AND_DESTROY;

        power_desc[num] = "give me space";
        if (p_ptr->pet_follow_distance == PET_SPACE_DIST) mode = num;
        powers[num++] = PET_ALLOW_SPACE;

        power_desc[num] = "stay away";
        if (p_ptr->pet_follow_distance == PET_AWAY_DIST) mode = num;
        powers[num++] = PET_STAY_AWAY;

        if (p_ptr->pet_open_doors)
        {
                power_desc[num] = "pets may open doors";
        }
        else
        {
                power_desc[num] = "pets may not open doors";
        }
        powers[num++] = PET_OPEN_DOORS;

        if (p_ptr->pet_pickup_items)
        {
                power_desc[num] = "pets may pick up items";
        }
        else
        {
                power_desc[num] = "pets may not pick up items";
        }
        powers[num++] = PET_TAKE_ITEMS;

        /* Nothing chosen yet */
        flag = FALSE;

        /* Build a prompt (accept all spells) */
        if (num <= 26)
        {
                /* Build a prompt (accept all spells) */
                strnfmt(out_val, 78, "(Command %c-%c, *=List, ESC=exit) Select a command: ",
                        I2A(0), I2A(num - 1));
        }
        else
        {
                strnfmt(out_val, 78, "(Command %c-%c, *=List, ESC=exit) Select a command: ",
                        I2A(0), '0' + num - 27);
        }

        /* Show list */
        redraw = TRUE;

        /* Save the screen */
        Term_save();

        prt("", y++, x);

        while (ctr < num)
        {
                sprintf(buf, "%s%c) %s", (ctr == mode) ? "*" : " ", I2A(ctr), power_desc[ctr]);
                prt(buf, y + ctr, x);
                ctr++;
        }

        if (ctr < 17)
        {
                prt("", y + ctr, x);
        }
        else
        {
                prt("", y + 17, x);
        }

        /* Get a command from the user */
        while (!flag && get_com(out_val, &choice))
        {
                /* Request redraw */
                if ((choice == ' ') || (choice == '*') || (choice == '?'))
                {
                        /* Show the list */
                        if (!redraw)
                        {
                                y = 1;
                                x = 0;
                                ctr = 0;

                                /* Show list */
                                redraw = TRUE;

                                /* Save the screen */
                                Term_save();

                                prt("", y++, x);

                                while (ctr < num)
                                {
                                        sprintf(buf, "%s%c) %s", (ctr == mode) ? "*" : " ", I2A(ctr), power_desc[ctr]);
                                        prt(buf, y + ctr, x);
                                        ctr++;
                                }

                                if (ctr < 17)
                                {
                                        prt("", y + ctr, x);
                                }
                                else
                                {
                                        prt("", y + 17, x);
                                }
                        }

                        /* Hide the list */
                        else
                        {
                                /* Hide list */
                                redraw = FALSE;

                                /* Restore the screen */
                                Term_load();
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
        if (redraw) Term_load();

        /* Abort if needed */
        if (!flag)
        {
                energy_use = 0;
                return;
        }

        switch (powers[i])
        {
                case PET_DISMISS: /* Dismiss pets */
                {
                        int Dismissed = 0;

                        if (get_check("Dismiss all pets? ")) all_pets = TRUE;

                        /* Process the monsters (backwards) */
                        for (pet_ctr = m_max - 1; pet_ctr >= 1; pet_ctr--)
                        {
                                /* Access the monster */
                                m_ptr = &m_list[pet_ctr];

                                if (is_pet(m_ptr))
                                {
                                        bool delete_this = FALSE;

                                        if (all_pets)
                                                delete_this = TRUE;
                                        else
                                        {
                                                char friend_name[80], check_friend[80];
                                                monster_desc(friend_name, m_ptr, 0x80);
                                                sprintf(check_friend, "Dismiss %s? ", friend_name);

                                                if (get_check(check_friend))
                                                        delete_this = TRUE;
                                        }

                                        if (delete_this)
                                        {
                                                delete_monster_idx(pet_ctr);
                                                Dismissed++;
                                        }
                                }
                        }

                        msg_format("You have dismissed %d pet%s.", Dismissed,
                                (Dismissed == 1 ? "" : "s"));
                        break;
                }
                /* Call pets */
                case PET_STAY_CLOSE:
                {
                        p_ptr->pet_follow_distance = PET_CLOSE_DIST;
                        break;
                }
                /* "Follow Me" */
                case PET_FOLLOW_ME:
                {
                        p_ptr->pet_follow_distance = PET_FOLLOW_DIST;
                        break;
                }
                /* "Seek and destoy" */
                case PET_SEEK_AND_DESTROY:
                {
                        p_ptr->pet_follow_distance = PET_DESTROY_DIST;
                        break;
                }
                /* "Give me space" */
                case PET_ALLOW_SPACE:
                {
                        p_ptr->pet_follow_distance = PET_SPACE_DIST;
                        break;
                }
                /* "Stay away" */
                case PET_STAY_AWAY:
                {
                        p_ptr->pet_follow_distance = PET_AWAY_DIST;
                        break;
                }
                /* flag - allow pets to open doors */
                case PET_OPEN_DOORS:
                {
                        p_ptr->pet_open_doors = !p_ptr->pet_open_doors;
                        break;
                }
                /* flag - allow pets to pickup items */
                case PET_TAKE_ITEMS:
                {
                        p_ptr->pet_pickup_items = !p_ptr->pet_pickup_items;

                        /* Drop objects being carried by pets */
                        if (!p_ptr->pet_pickup_items)
                        {
                                for (pet_ctr = m_max - 1; pet_ctr >= 1; pet_ctr--)
                                {
                                        /* Access the monster */
                                        m_ptr = &m_list[pet_ctr];

                                        if (is_pet(m_ptr))
                                        {
                                                monster_drop_carried_objects(m_ptr);
                                        }
                                }
                        }

                        break;
                }
        }
}


void do_cmd_yell(void)
{
        char trash[80];

        get_rnd_line("pshout.txt", trash);
        msg_format("You shout, '%s'", trash);
        energy_use = 100;
        aggravate_monsters(0);

        return;

        sc_time += 2;
}

void do_cmd_polypowers ()
{
        int             Power = -1;
        int             num = 0, dir = 0, i = 0;

        int             powers[36];
        char            power_desc[36][80];

        bool            flag, redraw;
        int             ask, plev = p_ptr->lev;

        char            choice;

        char            out_val[160];
        monster_race    *r_ptr = &r_info[p_ptr->polymon];

        int rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);
        int x = px, y = py, k, count;
        int rad;

        /* List the powers */
        if(r_ptr->flags2 & RF2_MULTIPLY) {strcpy(power_desc[num],"Multiply");powers[num++]=90;}
        if(r_ptr->flags4 & RF4_SHRIEK) {strcpy(power_desc[num],"Agravate monsters");powers[num++]=0;}
        if(r_ptr->flags4 & RF4_ROCKET) {strcpy(power_desc[num],"Rocket");powers[num++]=1;}
        if(r_ptr->flags4 & RF4_ARROW_1) {strcpy(power_desc[num],"Arrow1");powers[num++]=2;}
        if(r_ptr->flags4 & RF4_ARROW_2) {strcpy(power_desc[num],"Arrow2");powers[num++]=3;}
        if(r_ptr->flags4 & RF4_ARROW_3) {strcpy(power_desc[num],"Arrow3");powers[num++]=4;}
        if(r_ptr->flags4 & RF4_ARROW_4) {strcpy(power_desc[num],"Arrow4");powers[num++]=5;}
        if(r_ptr->flags4 & RF4_BR_ACID) {strcpy(power_desc[num],"Breathe Acid");powers[num++]=6;}
        if(r_ptr->flags4 & RF4_BR_ELEC) {strcpy(power_desc[num],"Breathe Lightning");powers[num++]=7;}
        if(r_ptr->flags4 & RF4_BR_FIRE) {strcpy(power_desc[num],"Breathe Fire");powers[num++]=8;}
        if(r_ptr->flags4 & RF4_BR_COLD) {strcpy(power_desc[num],"Breathe Cold");powers[num++]=9;}
        if(r_ptr->flags4 & RF4_BR_POIS) {strcpy(power_desc[num],"Breathe Poison");powers[num++]=10;}
        if(r_ptr->flags4 & RF4_BR_NETH) {strcpy(power_desc[num],"Breathe Nether");powers[num++]=11;}
        if(r_ptr->flags4 & RF4_BR_LITE) {strcpy(power_desc[num],"Breathe Lite");powers[num++]=12;}
        if(r_ptr->flags4 & RF4_BR_DARK) {strcpy(power_desc[num],"Breathe Darkness");powers[num++]=13;}
        if(r_ptr->flags4 & RF4_BR_CONF) {strcpy(power_desc[num],"Breathe Confusion");powers[num++]=14;}
        if(r_ptr->flags4 & RF4_BR_SOUN) {strcpy(power_desc[num],"Breathe Sound");powers[num++]=15;}
        if(r_ptr->flags4 & RF4_BR_CHAO) {strcpy(power_desc[num],"Breathe Chaos");powers[num++]=16;}
        if(r_ptr->flags4 & RF4_BR_DISE) {strcpy(power_desc[num],"Breathe Disenchantment");powers[num++]=17;}
        if(r_ptr->flags4 & RF4_BR_NEXU) {strcpy(power_desc[num],"Breathe Nexus");powers[num++]=18;}
        if(r_ptr->flags4 & RF4_BR_TIME) {strcpy(power_desc[num],"Breathe Time");powers[num++]=19;}
        if(r_ptr->flags4 & RF4_BR_INER) {strcpy(power_desc[num],"Breathe Inertia");powers[num++]=20;}
        if(r_ptr->flags4 & RF4_BR_GRAV) {strcpy(power_desc[num],"Breathe Gravity");powers[num++]=21;}
        if(r_ptr->flags4 & RF4_BR_SHAR) {strcpy(power_desc[num],"Breathe Shards");powers[num++]=22;}
        if(r_ptr->flags4 & RF4_BR_PLAS) {strcpy(power_desc[num],"Breathe Plasma");powers[num++]=23;}
        if(r_ptr->flags4 & RF4_BR_WALL) {strcpy(power_desc[num],"Breathe Force");powers[num++]=24;}
        if(r_ptr->flags4 & RF4_BR_MANA) {strcpy(power_desc[num],"Breathe Mana");powers[num++]=25;}
        if(r_ptr->flags6 & RF6_BR_WIND) {strcpy(power_desc[num],"Breathe Wind");powers[num++]=26;}
        if(r_ptr->flags4 & RF4_BR_NUKE) {strcpy(power_desc[num],"Breathe Nuke");powers[num++]=27;}
        if(r_ptr->flags4 & RF4_BA_CHAO) {strcpy(power_desc[num],"Chaos Ball");powers[num++]=28;}
        if(r_ptr->flags4 & RF4_BR_DISI) {strcpy(power_desc[num],"Breathe Disintegration");powers[num++]=29;}
        if(r_ptr->flags5 & RF5_BA_ACID) {strcpy(power_desc[num],"Acid Ball");powers[num++]=30;}
        if(r_ptr->flags5 & RF5_BA_ELEC) {strcpy(power_desc[num],"Lightning Ball");powers[num++]=31;}
        if(r_ptr->flags5 & RF5_BA_FIRE) {strcpy(power_desc[num],"Fire Ball");powers[num++]=32;}
        if(r_ptr->flags5 & RF5_BA_COLD) {strcpy(power_desc[num],"Cold Ball");powers[num++]=33;}
        if(r_ptr->flags5 & RF5_BA_POIS) {strcpy(power_desc[num],"Poison Ball");powers[num++]=34;}
        if(r_ptr->flags5 & RF5_BA_NETH) {strcpy(power_desc[num],"Nether Ball");powers[num++]=35;}
        if(r_ptr->flags5 & RF5_BA_WATE) {strcpy(power_desc[num],"Water Ball");powers[num++]=36;}
        if(r_ptr->flags5 & RF5_BA_MANA) {strcpy(power_desc[num],"Mana Ball");powers[num++]=37;}
        if(r_ptr->flags5 & RF5_BA_DARK) {strcpy(power_desc[num],"Darkness Ball");powers[num++]=38;}
        if(r_ptr->flags4 & RF4_BA_NUKE) {strcpy(power_desc[num],"Nuke Ball");powers[num++]=39;}
        if(r_ptr->flags6 & RF6_BA_WIND) {strcpy(power_desc[num],"Wind Ball");powers[num++]=40;}
        if(r_ptr->flags5 & RF5_CAUSE_1) {strcpy(power_desc[num],"Cause light wounds");powers[num++]=42;}
        if(r_ptr->flags5 & RF5_CAUSE_2) {strcpy(power_desc[num],"Cause medium wounds");powers[num++]=43;}
        if(r_ptr->flags5 & RF5_CAUSE_3) {strcpy(power_desc[num],"Cause critical wounds");powers[num++]=44;}
        if(r_ptr->flags5 & RF5_CAUSE_4) {strcpy(power_desc[num],"Cause mortal wounds");powers[num++]=45;}
        if(r_ptr->flags5 & RF5_BO_ACID) {strcpy(power_desc[num],"Acid Bolt");powers[num++]=46;}
        if(r_ptr->flags5 & RF5_BO_ELEC) {strcpy(power_desc[num],"Lightning Bolt");powers[num++]=47;}
        if(r_ptr->flags5 & RF5_BO_FIRE) {strcpy(power_desc[num],"Fire Bolt");powers[num++]=48;}
        if(r_ptr->flags5 & RF5_BO_COLD) {strcpy(power_desc[num],"Cold Bolt");powers[num++]=49;}
        if(r_ptr->flags5 & RF5_BO_POIS) {strcpy(power_desc[num],"Poison Bolt");powers[num++]=56;}
        if(r_ptr->flags5 & RF5_BO_NETH) {strcpy(power_desc[num],"Nether Bolt");powers[num++]=50;}
        if(r_ptr->flags5 & RF5_BO_WATE) {strcpy(power_desc[num],"Water Bolt");powers[num++]=51;}
        if(r_ptr->flags5 & RF5_BO_MANA) {strcpy(power_desc[num],"Mana Bolt");powers[num++]=52;}
        if(r_ptr->flags5 & RF5_BO_PLAS) {strcpy(power_desc[num],"Plasma Bolt");powers[num++]=53;}
        if(r_ptr->flags5 & RF5_BO_ICEE) {strcpy(power_desc[num],"Ice Bolt");powers[num++]=54;}
        if(r_ptr->flags5 & RF5_MISSILE) {strcpy(power_desc[num],"Missile");powers[num++]=55;}
        if(r_ptr->flags5 & RF5_SCARE) {strcpy(power_desc[num],"Scare");powers[num++]=41;}
        if(r_ptr->flags5 & RF5_BLIND) {strcpy(power_desc[num],"Blindness");powers[num++]=57;}
        if(r_ptr->flags5 & RF5_CONF) {strcpy(power_desc[num],"Confusion");powers[num++]=58;}
        if(r_ptr->flags5 & RF5_SLOW) {strcpy(power_desc[num],"Slow");powers[num++]=59;}
        if(r_ptr->flags5 & RF5_HOLD) {strcpy(power_desc[num],"Paralyse");powers[num++]=60;}
        if(r_ptr->flags6 & RF6_HASTE) {strcpy(power_desc[num],"Haste Self");powers[num++]=61;}
        if(r_ptr->flags6 & RF6_HAND_DOOM) {strcpy(power_desc[num],"Hand of Doom");powers[num++]=62;}
        if(r_ptr->flags6 & RF6_HEAL) {strcpy(power_desc[num],"Heal");powers[num++]=63;}
        if(r_ptr->flags6 & RF6_BLINK) {strcpy(power_desc[num],"Blink");powers[num++]=64;}
        if(r_ptr->flags6 & RF6_TPORT) {strcpy(power_desc[num],"Teleport");powers[num++]=65;}
        if(r_ptr->flags6 & RF6_DOOR) {strcpy(power_desc[num],"Dimension Door");powers[num++]=66;}
        if(r_ptr->flags6 & RF6_TELE_AWAY) {strcpy(power_desc[num],"Teleport Away");powers[num++]=67;}
        if(r_ptr->flags6 & RF6_TELE_LEVEL) {strcpy(power_desc[num],"Teleport Level");powers[num++]=68;}
        if(r_ptr->flags6 & RF6_DARKNESS) {strcpy(power_desc[num],"Darkness");powers[num++]=69;}
        if(r_ptr->flags6 & RF6_TRAPS) {strcpy(power_desc[num],"Create Traps");powers[num++]=88;}
        if(r_ptr->flags6 & RF6_RAISE_DEAD) {strcpy(power_desc[num],"Raise the Dead");powers[num++]=89;}
        if(r_ptr->flags6 & RF6_S_KIN) {strcpy(power_desc[num],"Summon Kin");powers[num++]=73;}
        if(r_ptr->flags6 & RF6_S_CYBER) {strcpy(power_desc[num],"Summon Cyberdemon");powers[num++]=74;}
        if(r_ptr->flags6 & RF6_S_MONSTER) {strcpy(power_desc[num],"Summon Monster");powers[num++]=75;}
        if(r_ptr->flags6 & RF6_S_MONSTERS) {strcpy(power_desc[num],"Summon Monsters");powers[num++]=76;}
        if(r_ptr->flags6 & RF6_S_ANT) {strcpy(power_desc[num],"Summon Ants");powers[num++]=77;}
        if(r_ptr->flags6 & RF6_S_SPIDER) {strcpy(power_desc[num],"Summon Spiders");powers[num++]=78;}
        if(r_ptr->flags6 & RF6_S_HOUND) {strcpy(power_desc[num],"Summon Hound");powers[num++]=79;}
        if(r_ptr->flags6 & RF6_S_HYDRA) {strcpy(power_desc[num],"Summon Hydras");powers[num++]=80;}
        if(r_ptr->flags6 & RF6_S_ANGEL) {strcpy(power_desc[num],"Summon Angel");powers[num++]=81;}
        if(r_ptr->flags6 & RF6_S_DEMON) {strcpy(power_desc[num],"Summon Demon");powers[num++]=82;}
        if(r_ptr->flags6 & RF6_S_UNDEAD) {strcpy(power_desc[num],"Summon Undead");powers[num++]=83;}
        if(r_ptr->flags6 & RF6_S_DRAGON) {strcpy(power_desc[num],"Summon Dragon");powers[num++]=84;}
        if(r_ptr->flags6 & RF6_S_HI_UNDEAD) {strcpy(power_desc[num],"Summon High Undead");powers[num++]=85;}
        if(r_ptr->flags6 & RF6_S_HI_DRAGON) {strcpy(power_desc[num],"Summon High Dragon");powers[num++]=86;}
        if(r_ptr->flags6 & RF6_S_WRAITH) {strcpy(power_desc[num],"Summon Wraith");powers[num++]=87;}

        if(!num) {msg_print("You have no powers to use."); return;}

        /* Nothing chosen yet */
        flag = FALSE;

        /* No redraw yet */
        redraw = FALSE;

        /* Build a prompt (accept all spells) */
        if (num <= 26)
        {
                /* Build a prompt (accept all spells) */
                strnfmt(out_val, 78, "(Powers %c-%c, *=List, ESC=exit) Use which power of your monster? ",
                        I2A(0), I2A(num - 1));
        }
        else
        {
                strnfmt(out_val, 78, "(Powers %c-%c, *=List, ESC=exit) Use which power of your monster? ",
                        I2A(0), '0' + num - 27);
        }

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

                                strcpy(dummy, "");

                                /* Show list */
                                redraw = TRUE;

                                /* Save the screen */
                                Term_save();

                                prt ("", y++, x);

                                while (ctr < num && ctr < 17)
                                {
                                        sprintf(dummy, "%c) %s", I2A(ctr), power_desc[ctr]);
                                        prt(dummy, y + ctr, x);
                                        ctr++;
                                }
                                while (ctr < num)
                                {
                                        if (ctr < 26)
                                        {
                                                sprintf(dummy, " %c) %s", I2A(ctr), power_desc[ctr]);
                                        }
                                        else
                                        {
                                                sprintf(dummy, " %c) %s", '0' + ctr - 26, power_desc[ctr]);
                                        }
                                        prt(dummy, y + ctr - 17, x + 40);
                                        ctr++;
                                }
                                if (ctr < 17)
                                {
                                        prt ("", y + ctr, x);
                                }
                                else
                                {
                                        prt ("", y + 17, x);
                                }
                        }

                        /* Hide the list */
                        else
                        {
                                /* Hide list */
                                redraw = FALSE;

                                /* Restore the screen */
                                Term_load();
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
                        bell();
                        continue;
                }

                /* Save the spell index */
                Power = powers[i];

                /* Verify it */
                if (ask)
                {
                        char tmp_val[160];

                        /* Prompt */
                        strnfmt(tmp_val, 78, "Use %s? ", power_desc[i]);

                        /* Belay that order */
                        if (!get_check(tmp_val)) continue;
                }

                /* Stop the loop */
                flag = TRUE;
        }

        /* Restore the screen */
        if (redraw) Term_load();

        /* Abort if needed */
        if (!flag)
        {
                energy_use = 0;
                return;
        }

        if (r_ptr->flags2 & RF2_POWERFUL)
                rad = 1 + (p_ptr->lev/15);
        else
                rad = 1 + (p_ptr->lev/20);

        if (p_ptr->tim_polymon)
        {
                if ((Power > 6) && (Power < 26))
                msg_print ("You emit a loud belch...");
                else
                msg_format ("Your %s spell fails...", power_desc[i]);
                return;
        }
        else p_ptr->tim_polymon = 100 / ((r_ptr->freq_spell) ?
                r_ptr->freq_spell : 100);

        energy_use = 100; /* base spell mod */

        switch(Power)
        {
                case 90: /* Multiply */
                        msg_format("You multiply...");
                        get_pos_player(5, &y, &x);
                        place_monster_one(y, x, p_ptr->polymon, FALSE, TRUE, TRUE);
                        break;
                case 0: /* Shriek */
                        aggravate_monsters(-1);
                        break;
                case 1: /* Rocket */
                        msg_print("You launch a rocket...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_ROCKET, dir, p_ptr->lev * 12, 1 + (p_ptr->lev/20));
                        break;
                case 2: /* Arrow1 */
                        msg_print("You fire a light arrow...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ARROW, dir, damroll(1,6));
                        break;
                case 3: /* Arrow2 */
                        msg_print("You fire a heavy arrow...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ARROW, dir, damroll(3,6));
                        break;
                case 4: /* Arrow3 */
                        msg_print("You fire a light missile...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ARROW, dir, damroll(5,6));
                        break;
                case 5: /* Arrow4 */
                        msg_print("You fire a heavy missile...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ARROW, dir, damroll(7,6));
                        break;
                case 6: /* Br acid */
                        msg_print("You breathe acid ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_ACID, dir, (p_ptr->chp * 10)/30, rad);
                        break;
                case 7: /* Br elec */
                        msg_print("You breathe lightning ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_ELEC, dir, (p_ptr->chp * 10)/30, rad);
                        break;
                case 8: /* br fire */
                        msg_print("You breathe fire ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_FIRE, dir, (p_ptr->chp * 10)/30, rad);
                        break;
                case 9: /* br cold */
                        msg_print("You breathe cold ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_COLD, dir, (p_ptr->chp * 10)/30, rad);
                        break;
                case 10: /* br pois */
                        msg_print("You breathe poison ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_POIS, dir, (p_ptr->chp * 10)/36, rad);
                        break;
                case 11: /* br neth */
                        msg_print("You breathe nether ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_NETHER, dir, (p_ptr->chp * 10)/40, rad);
                        break;
                case 12: /* br lite */
                        msg_print("You breathe lite ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_LITE, dir, (p_ptr->chp * 10)/35, rad);
                        break;
                case 13: /* br dark */
                        msg_print("You breathe dark ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_DARK, dir, (p_ptr->chp * 10)/35, rad);
                        break;
                case 14: /* br conf */
                        msg_print("You breathe confusion ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_CONFUSION, dir, (p_ptr->chp * 10)/35, rad);
                        break;
                case 15: /* br soun */
                        msg_print("You breathe sound ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_SOUND, dir, (p_ptr->chp * 10)/35, rad);
                        break;
                case 16: /* br chao */
                        msg_print("You breathe chaos ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_CHAOS, dir, (p_ptr->chp * 10)/40, rad);
                        break;
                case 17: /* br dise */
                        msg_print("You breathe disenchantment ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_DISENCHANT, dir, (p_ptr->chp * 10)/40, rad);
                        break;
                case 18: /* br nexu */
                        msg_print("You breathe nexus ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_NEXUS, dir, (p_ptr->chp * 10)/40, rad);
                        break;
                case 19: /* br time */
                        msg_print("You breathe time ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_TIME, dir, (p_ptr->chp * 10)/60, rad);
                        break;
                case 20: /* br iner */
                        msg_print("You breathe inertia ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_INERTIA, dir, (p_ptr->chp * 10)/50, rad);
                        break;
                case 21: /* br grav */
                        msg_print("You breathe gravity ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_GRAVITY, dir, (p_ptr->chp * 10)/50, rad);
                        break;
                case 22: /* br shar */
                        msg_print("You breathe shards ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_SHARDS, dir, (p_ptr->chp * 10)/30, rad);
                        break;
                case 23: /* br plas */
                        msg_print("You breathe plasma ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_PLASMA, dir, (p_ptr->chp * 10)/60, rad);
                        break;
                case 24: /* br wall */
                        msg_print("You breathe force ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_FORCE, dir, (p_ptr->chp * 10)/50, rad);
                        break;
                case 25: /* br mana */
                        msg_print("You breathe mana ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_MANA, dir, (p_ptr->chp * 10)/36, rad);
                        break;
                case 26: /* br wind */
                        msg_print("You breathe wind ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_MANA, dir, p_ptr->lev * 5, rad);
                        break;
                case 27: /* br nuke */
                        msg_print("You breathe nuke ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_NUKE, dir, (p_ptr->chp * 10)/45, 1 + (p_ptr->lev/20));
                        break;
                case 28: /* ba chao */
                        msg_print("You cast a ball of chaos ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_CHAOS, dir, p_ptr->lev * 4, 2);
                        break;
                case 29: /* br disi */
                        msg_print("You breathe disintegration ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_DISINTEGRATE, dir, p_ptr->lev * 5, 1 + (p_ptr->lev/20));
                        break;
                case 30: /* ba acid */
                        msg_print("You cast a ball of acid ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_ACID, dir, randint(p_ptr->lev * 6)+20, 2);
                        break;
                case 31: /* ba elec */
                        msg_print("You cast a ball of lightning ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_ELEC, dir, randint(p_ptr->lev * 3)+20, 2);
                        break;
                case 32: /* ba fire */
                        msg_print("You cast a ball of fire ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_FIRE, dir, randint(p_ptr->lev * 7)+20, 2);
                        break;
                case 33: /* ba cold */
                        msg_print("You cast a ball of cold ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_COLD, dir, randint(p_ptr->lev * 3)+20, 2);
                        break;
                case 34: /* ba pois */
                        msg_print("You cast a ball of poison ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_POIS, dir, damroll(12,2), 2);
                        break;
                case 35: /* ba neth */
                        msg_print("You cast a ball of nether ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_NETHER, dir, randint(p_ptr->lev * 4)+20, 2);
                        break;
                case 36: /* ba wate */
                        msg_print("You cast a ball of water ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_WATER, dir, randint(p_ptr->lev * 4)+20, 2);
                        break;
                case 37: /* ba mana */
                        msg_print("You cast a mana storm...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_MANA, dir, randint(p_ptr->lev * 3)+20, 2);
                        break;
                case 38: /* ba dark */
                        msg_print("You cast a ball of darkness ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_DARK, dir, randint(p_ptr->lev * 3)+20, 2);
                        break;
                case 39: /* ba nuke */
                        msg_print("You cast a ball of nuke ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_NUKE, dir, p_ptr->lev * 12, 1 + (p_ptr->lev/20));
                        break;
                case 40: /* ba wind */
                        msg_print("You cast a wind storm ...");
                        if (get_aim_dir(&dir))
                               fire_ball(GF_WIND, dir, randint(p_ptr->lev * 3)+20, 2);
                        break;
                case 42: /* cause1 */
                        msg_print("You cause light wounds ...");
                        if (get_aim_dir(&dir))
                        {
                                fire_bolt(GF_MANA, dir, damroll(3, 8));
                        }
                        break;
                case 43: /* cause2 */
                        msg_print("You cause serious wounds ...");
                        if (get_aim_dir(&dir))
                        {
                                fire_bolt(GF_MANA, dir, damroll(8, 8));
                        }
                        break;
                case 44: /* cause3 */
                        msg_print("You cause critical wounds ...");
                        if (get_aim_dir(&dir))
                        {
                                fire_bolt(GF_MANA, dir, damroll(10, 15));
                        }
                        break;
                case 45: /* cause4 */
                        msg_print("You cause mortal wounds ...");
                        if (get_aim_dir(&dir))
                        {
                                fire_bolt(GF_MANA, dir, damroll(15, 15));
                        }
                        break;
                case 46: /* bo acid */
                        msg_print("You cast a bolt of acid ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ACID, dir, damroll(7, 8) + (p_ptr->lev/3));
                        break;
                case 47: /* bo elec */
                        msg_print("You cast a bolt of lightning ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ELEC, dir, damroll(4, 8) + (p_ptr->lev/3));
                        break;
                case 48: /* bo fire */
                        msg_print("You cast a bolt of fire ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_FIRE, dir, damroll(9, 8) + (p_ptr->lev/3));
                        break;
                case 49: /* bo cold */
                        msg_print("You cast a bolt of cold ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_COLD, dir, damroll(6, 8) + (p_ptr->lev/3));
                        break;
                case 56: /* bo pois */
                        msg_print("You cast a bolt of poison ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_POIS, dir, damroll(7, 8) + (p_ptr->lev/3));
                        break;
                case 50: /* bo neth */
                        msg_print("You cast a bolt of nether ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_NETHER, dir, damroll(5, 5) + (p_ptr->lev/3));
                        break;
                case 51: /* bo wate */
                        msg_print("You cast a bolt of water ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_WATER, dir, damroll(10, 10) + (p_ptr->lev/3));
                        break;
                case 52: /* bo mana */
                        msg_print("You cast a bolt of mana ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_MANA, dir, damroll(3, 8) + (p_ptr->lev/3));
                        break;
                case 53: /* bo plas */
                        msg_print("You cast a bolt of plasma ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_PLASMA, dir, damroll(8, 8) + (p_ptr->lev/3));
                        break;
                case 54: /* bo ice */
                        msg_print("You cast a bolt of ice ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_ICE, dir, damroll(6, 6) + (p_ptr->lev/3));
                        break;
                case 55: /* missile */
                        msg_print("You cast a magic missile ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_MISSILE, dir, damroll(2, 6) + (p_ptr->lev/3));
                        break;
                case 57: /* blind */
                        msg_print("You cast blindness ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_CONFUSION, dir, damroll(1, 8) + (p_ptr->lev/3));
                        break;
                case 41: /* scare */
                        msg_print("You cast scare ...");
                        if (get_aim_dir(&dir))
                                fear_monster(dir, plev);
                        break;
                case 58: /* conf */
                        msg_print("You cast a bolt of confusion ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_CONFUSION, dir, damroll(7, 8) + (p_ptr->lev/3));
                        break;
                case 59: /* slow */
                        msg_print("You cast a bolt of unspeed ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_OLD_SLOW, dir, damroll(6, 8) + (p_ptr->lev/3));
                        break;
                case 60: /* hold */
                        msg_print("You cast a bolt of paralisation ...");
                        if (get_aim_dir(&dir))
                               fire_bolt(GF_OLD_SLEEP, dir, damroll(5, 8) + (p_ptr->lev/3));
                        break;
                case 61: /* haste */
                        if (!p_ptr->fast)
                        {
                                (void)set_fast(randint(20 + (plev) ) + plev);
                        }
                        else
                        {
                                (void)set_fast(p_ptr->fast + randint(5));
                        }
                        break;
                case 62: /* hand of doom */
                        msg_print("You invoke the Hand of Doom ...");
                        if (get_aim_dir(&dir))
                        {
                                fire_bolt(GF_MANA, dir, damroll(10, 8) + (p_ptr->lev));
                        }
                        break;
                case 63: /* heal */
                        hp_player(damroll(8,5));
                        break;
                case 64: /* Blink */
                        teleport_player(10);
                        break;
                case 65: /* Teleport */
                        teleport_player(plev * 5);
                        break;
                case 66: /* tele to */
                        {
                             int ii,ij;

                             msg_print("You use the power of dimension door.");
                             if (!tgt_pt(&ii,&ij)) return;
                             p_ptr->energy -= 60 - plev;
                             if (!cave_empty_bold(ij,ii) || (cave[ij][ii].info & CAVE_ICKY) ||
                             (distance(ij,ii,py,px) > plev*20 + 2))
                             {
                                 msg_print("You fail to show the destination correctly!");
                                 p_ptr->energy -= 100;
                                 teleport_player(10);
                             }
                             else teleport_player_to(ij,ii);
                        }
                        break;
                case 67: /* tele away */
                        if (!get_aim_dir(&dir)) return;
                        (void)fire_beam(GF_AWAY_ALL, dir, plev);
                        break;
                case 68: /* tele level */
                        if (is_quest(dun_level)) {msg_print("No teleport on special levels ...");break;}
                        teleport_player_level();
                        break;
                case 69: /* darkness */
                        (void)project(-1, 3, py, px, 0, GF_DARK_WEAK, PROJECT_GRID | PROJECT_KILL);
                        /* Unlite up the room */
                        unlite_room(py, px);
                        break;
                case 88: /* create traps */
                        msg_print("You create traps ...");
                        trap_creation();
                        break;
                case 73: /* Summon kin */
                                msg_format("You magically summon some Kins.");
                                summon_kin_type = r_ptr->d_char; /* Big hack */
                                for (k = 0; k < 6; k++)
                                {
                                                count += summon_specific(y, x, rlev, SUMMON_KIN, TRUE, TRUE, TRUE);
                                }
                        break;
                case 74: /* Summon cyber */
                                msg_format("You magically summon a Cyberdemon.");
                                for (k = 0; k < 1; k++)
                                {
                                                count += summon_specific(y, x, rlev, SUMMON_CYBER, FALSE, TRUE, TRUE);
                                }
                        break;
                case 75: /* Summon monster */
                                msg_format("You magically summon a monster.");
                                for (k = 0; k < 1; k++)
                                {
                                                count += summon_specific(y, x, rlev, 0, FALSE, TRUE, TRUE);
                                }
                        break;
                case 76: /* Summon monsters */
                                msg_format("You magically summon monsters.");
                                for (k = 0; k < 6; k++)
                                {
                                                count += summon_specific(y, x, rlev, 0, TRUE, TRUE, TRUE);
                                }
                        break;
                case 77: /* Summon ant */
                                msg_format("You magically summon ants.");
                                for (k = 0; k < 6; k++)
                                {
                                                count += summon_specific(y, x, rlev, SUMMON_ANT, TRUE, TRUE, TRUE);
                                }
                        break;
                case 78: /* Summon spider */
                                msg_format("You magically summon spiders.");
                                for (k = 0; k < 6; k++)
                                {
                                                count += summon_specific(y, x, rlev, SUMMON_SPIDER, TRUE, TRUE, TRUE);
                                }
                        break;
                case 79: /* Summon hound */
                                msg_format("You magically summon hounds.");
                                for (k = 0; k < 6; k++)
                                {
                                                count += summon_specific(y, x, rlev, SUMMON_HOUND, TRUE, TRUE, TRUE);
                                }
                        break;
                case 80: /* Summon hydra */
                                msg_format("You magically summon hydras.");
                                for (k = 0; k < 6; k++)
                                {
                                                count += summon_specific(y, x, rlev, SUMMON_HYDRA, TRUE, TRUE, TRUE);
                                }
                        break;
                case 81: /* Summon angel */
                                msg_format("You magically summon an angel.");
                                for (k = 0; k < 1; k++)
                                {
                                                count += summon_specific(y, x, rlev, SUMMON_ANGEL, FALSE, TRUE, TRUE);
                                }
                        break;
                case 82: /* Summon demon */
                                msg_format("You magically summon a demon.");
                                for (k = 0; k < 1; k++)
                                {
                                                count += summon_specific(y, x, rlev, SUMMON_DEMON, FALSE, TRUE, TRUE);
                                }
                        break;
                case 83: /* Summon undead */
                                msg_format("You magically summon an undead.");
                                for (k = 0; k < 1; k++)
                                {
                                                count += summon_specific(y, x, rlev, SUMMON_UNDEAD, FALSE, TRUE, TRUE);
                                }
                        break;
                case 84: /* Summon dragon */
                                msg_format("You magically summon a dragon.");
                                for (k = 0; k < 1; k++)
                                {
                                                count += summon_specific(y, x, rlev, SUMMON_DRAGON, FALSE, TRUE, TRUE);
                                }
                        break;
                case 85: /* Summon hiundead */
                                msg_format("You magically summon greater undead.");
                                for (k = 0; k < 8; k++)
                                {
                                                count += summon_specific(y, x, rlev, SUMMON_HI_UNDEAD_NO_UNIQUES, TRUE, TRUE, TRUE);
                                }
                        break;
                case 86: /* Summon hidragon */
                                msg_format("You magically summon greater dragons.");
                                for (k = 0; k < 8; k++)
                                {
                                                count += summon_specific(y, x, rlev, SUMMON_HI_DRAGON_NO_UNIQUES, TRUE, TRUE, TRUE);
                                }
                        break;
                case 87: /* Summon wraith */
                                msg_format("You magically summon a wraith.");
                                for (k = 0; k < 8; k++)
                                {
                                                count += summon_specific(y, x, rlev, SUMMON_WRAITH, FALSE, TRUE, TRUE);
                                }
                        break;
        }
        return;
}
