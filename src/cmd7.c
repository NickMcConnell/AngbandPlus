/* File: cmd7.c */

/* Purpose: Non-Realmed Class commands */

/*
 * Copyright (c) 1999 Dark God
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */


#include "angband.h"

extern void do_cmd_rerate(void);
extern bool item_tester_hook_armour(object_type *o_ptr);

void mindcraft_info(char *p, int power)
{
    int plev = p_ptr->lev;

    strcpy(p, "");

    switch (power) {
	case 0:  break;
	case 1:  sprintf(p, " dam %dd%d", 3 + ((plev - 1) / 4), 3 + plev/15); break;
	case 2:  sprintf(p, " range %d", (plev < 25 ? 10 : plev + 2)); break;
	case 3:  sprintf(p, " range %d", plev * 5);  break;
	case 4:  break;
	case 5:  sprintf(p, " dam %dd8", 8+((plev-5)/4));  break;
	case 6:  sprintf(p, " dur %d", plev);  break;
	case 7:  break;
	case 8:  sprintf(p, " dam %d", plev * ((plev-5) / 10 + 1)); break;
	case 9:  sprintf(p, " dur 11-%d", plev + plev/2);  break;
	case 10: sprintf(p, " dam %dd6", plev/2);  break;
	case 11: sprintf(p, " dam %d", plev * (plev > 39 ? 4: 3)); break;
    }
}

void mimic_info(char *p, int power)
{
    int plev = p_ptr->lev;

    strcpy(p, "");

    switch (power) {
	case 0:  break;
        case 1:  sprintf(p, " dur %d+d20", 10 + plev); break;
        case 2:  sprintf(p, " dur 50+d%d", 50 + (2 * plev)); break;
        case 3:  sprintf(p, " dur 50+d%d", 50 + (2 * plev)); break;
        case 4:  sprintf(p, " dur 50+d%d", 50 + (2 * plev)); break;
    }
}

/*
 * Allow user to choose a magic power.
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
int get_magic_power(int *sn, magic_power *powers, int max_powers, void (*power_info)(char *p, int power))
{
	int             i;
	int             num = 0;
        int             y = 2;
	int             x = 20;
	int             minfail = 0;
	int             plev = p_ptr->lev;
	int             chance = 0;
        int             info;
	char            choice;
	char            out_val[160];
	char            comment[80];
	cptr            p = "power";
        magic_power     spell;
	bool            flag, redraw;

	/* Assume cancelled */
	*sn = (-1);

#ifdef ALLOW_REPEAT /* TNB */

	/* Get the spell, if available */
	if (repeat_pull(sn))
	{
		/* Verify the spell */
                if (powers[*sn].min_lev <= plev)
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

        for (i = 0; i < max_powers; i++)
	{
                if (powers[i].min_lev <= plev)
		{
			num++;
		}
	}

	/* Build a prompt (accept all spells) */
        strnfmt(out_val, 78, "(%^ss %c-%c, *=List, ESC=exit, %c-%c=Info) Use which %s? ",
                p, I2A(0), I2A(num - 1), toupper(I2A(0)), toupper(I2A(num - 1)), p);

        /* Save the screen */
        character_icky = TRUE;
        Term_save();

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

				/* Display a list of spells */
                                prt("", 1, x);
				prt("", y, x);
				put_str("Name", y, x + 5);
				put_str("Lv Mana Fail Info", y, x + 35);

				/* Dump the spells */
                                for (i = 0; i < max_powers; i++)
				{
					/* Access the spell */
                                        spell = powers[i];
					if (spell.min_lev > plev)   break;

					chance = spell.fail;
                                        /* Reduce failure rate by "effective" level adjustment */
                                        chance -= 3 * (p_ptr->lev - spell.min_lev);

                                        /* Reduce failure rate by INT/WIS adjustment */
                                        chance -= 3 * (adj_mag_stat[p_ptr->stat_ind[cp_ptr->spell_stat]] - 1);

					/* Not enough mana to cast */
					if (spell.mana_cost > p_ptr->csp)
					{
                                                chance += 5 * (spell.mana_cost - p_ptr->csp);
					}

                                        /* Extract the minimum failure rate */
                                        minfail = adj_mag_fail[p_ptr->stat_ind[cp_ptr->spell_stat]];

					/* Minimum failure rate */
                                        if (chance < minfail) chance = minfail;

					/* Stunning makes spells harder */
                                        if (p_ptr->stun > 50) chance += 25;
                                        else if (p_ptr->stun) chance += 15;

                                        /* Always a 5 percent chance of working */
					if (chance > 95) chance = 95;

					/* Get info */
                                        power_info(comment, i);

					/* Dump the spell --(-- */
                                        sprintf(psi_desc, "  %c) %-30s%2d %4d %3d%%%s",
                                            I2A(i), spell.name,
                                            spell.min_lev, spell.mana_cost, chance, comment);
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
				Term_load();
                                character_icky = FALSE;
			}

			/* Redo asking */
			continue;
		}

		/* Note verify */
                info = (isupper(choice));

		/* Lowercase */
                if (info) choice = tolower(choice);

		/* Extract request */
		i = (islower(choice) ? A2I(choice) : -1);

		/* Totally Illegal */
		if ((i < 0) || (i >= num))
		{
			bell();
			continue;
		}

		/* Save the spell index */
                spell = powers[i];

                /* Provides info */
                if (info)
		{
                        c_prt(TERM_L_BLUE, spell.desc, 1, 0);

                        /* Restore the screen */
                        inkey();
                        Term_load();
                        character_icky = FALSE;
                        continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}

	/* Restore the screen */
        if (redraw)
        {
                Term_load();
        }
        character_icky = FALSE;

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
	(*sn) = i;

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
	int             plev = p_ptr->lev;
        magic_power     spell;

        /* No magic */
        if (p_ptr->antimagic)
        {
                msg_print("Your anti-magic field disrupts any magic attempts.");
                return;
        }

	/* not if confused */
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}

	/* get power */
        if (!get_magic_power(&n, mindcraft_powers, MAX_MINDCRAFT_POWERS, mindcraft_info))  return;

        spell = mindcraft_powers[n];

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
	chance -= 3 * (adj_mag_stat[p_ptr->stat_ind[cp_ptr->spell_stat]] - 1);

	/* Not enough mana to cast */
	if (spell.mana_cost > p_ptr->csp)
	{
		chance += 5 * (spell.mana_cost - p_ptr->csp);
	}

	/* Extract the minimum failure rate */
	minfail = adj_mag_fail[p_ptr->stat_ind[cp_ptr->spell_stat]];

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
		case 0:   /* Precog */
			if (plev > 44)
				wiz_lite();
			else if (plev > 19)
				map_area();

			if (plev < 30)
			{
				b = detect_monsters_normal();
				if (plev > 14)  b |=  detect_monsters_invis();
				if (plev > 4)   b |=  detect_traps();
			}
			else
			{
				b = detect_all();
			}

			if ((plev > 24) && (plev < 40))
				set_tim_esp(p_ptr->tim_esp + plev);

			if (!b) msg_print("You feel safe.");
			break;
		case 1:
			/* Mindblast */
			if (!get_aim_dir(&dir)) return;
			if (randint(100) < plev * 2)
				fire_beam(GF_PSI, dir, damroll(3 + ((plev - 1) / 4), (3+plev/15)));
			else
				fire_ball(GF_PSI, dir, damroll(3 + ((plev - 1) / 4), (3+plev/15)), 0);
			break;
		case 2:
			/* Minor displace */
			if (plev < 25)
			{
				teleport_player(10);
			}
			else
			{
                                int ii,ij;

             if(dungeon_flags1 & LF1_NO_TELEPORT){msg_print("Not on special levels!");break;}

             msg_print("You open a between gate. Choose a destination.");
             if (!tgt_pt(&ii,&ij)) return;
             p_ptr->energy -= 60 - plev;
             if (!cave_empty_bold(ij,ii) || (cave[ij][ii].info & CAVE_ICKY) ||
             (distance(ij,ii,py,px) > plev + 2+(p_ptr->to_s*3)) ||
             (!rand_int(plev * plev / 2)))
             {
                 msg_print("You fail to exit the between correctly!");
                 p_ptr->energy -= 100;
                 get_pos_player(10+p_ptr->to_s/2,&ij,&ii);
             }
             cave_set_feat(py,px,FEAT_BETWEEN);
             cave_set_feat(ij,ii,FEAT_BETWEEN);
             cave[py][px].special = ii + (ij << 8);
             cave[ij][ii].special = px + (py << 8);

             break;
			}
			break;
		case 3:
			/* Major displace */
			if (plev > 29)
				banish_monsters(plev);
			teleport_player(plev * 5);
			break;
		case 4:
			/* Domination */
			if (plev < 30)
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_DOMINATION, dir, plev, 0);
			}
			else
			{
				charm_monsters(p_ptr->lev * 2);
			}
			break;
		case 5:
			/* Fist of Force  ---  not 'true' TK  */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_SOUND, dir, damroll(8+((plev-5)/4), 8),
				(plev > 20 ? (plev-20)/8 + 1 : 0));
			break;
		case 6:
			/* Character Armour */
                        set_shield(p_ptr->shield + plev, 50, 0);
			if (plev > 14) set_oppose_acid(p_ptr->oppose_acid + plev);
			if (plev > 19) set_oppose_fire(p_ptr->oppose_fire + plev);
			if (plev > 24) set_oppose_cold(p_ptr->oppose_cold + plev);
			if (plev > 29) set_oppose_elec(p_ptr->oppose_elec + plev);
			if (plev > 34) set_oppose_pois(p_ptr->oppose_pois + plev);
			break;
		case 7:
			/* Psychometry */
			if (plev < 40)
				psychometry();
			else
				ident_spell();
			break;
		case 8:
			/* Mindwave */
			msg_print("Mind-warping forces emanate from your brain!");
			if (plev < 25)
				project(0, 2+plev/10, py, px,
				(plev*3)/2, GF_PSI, PROJECT_KILL);
			else
				(void)mindblast_monsters(plev * ((plev-5) / 10 + 1));
			break;
		case 9:
			/* Adrenaline */
			set_afraid(0);
			set_stun(0);
			hp_player(plev);
			b = 10 + randint((plev*3)/2);
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
		case 10:
			/* Psychic Drain */
			if (!get_aim_dir(&dir)) return;
			b = damroll(plev/2, 6);
			if (fire_ball(GF_PSI_DRAIN, dir, b,  0 +
				(plev-25)/10))
				p_ptr->energy -= randint(150);
			break;
		case 11:
			/* Telekinesis */
			msg_print("A wave of pure physical force radiates out from your body!");
			project(0, 3+plev/10, py, px,
				plev * (plev > 39 ? 4 : 3), GF_TELEKINESIS, PROJECT_KILL|PROJECT_ITEM|PROJECT_GRID);
			break;
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
		p_ptr->csp -= spell.mana_cost;
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

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);
	p_ptr->window |= (PW_SPELL);
}

static int get_mimic_chance(int c)
{
  int chance=c;
  chance -= p_ptr->lev * 3;
  chance -= 3 * adj_mag_stat[p_ptr->stat_ind[A_DEX]];

  if (chance < 2) chance = 2;

  /* Stunning makes spells harder */
  if (p_ptr->stun > 50) chance += 25;
  else if (p_ptr->stun) chance += 15;

  /* Always a 5 percent chance of working */
  if (chance > 95) chance = 95;

  /* Return the chance */
  return (chance);
}

void do_cmd_mimic_lore()
{
        int chance;

	object_type	*o_ptr;

        if (p_ptr->blind || no_lite()) {
                msg_print("You cannot see!");
                return;
        }

        if (p_ptr->confused) {
                msg_print("You are too confused!");
                return;
        }

        if(!p_ptr->mimic_form){

        o_ptr = &inventory[INVEN_OUTER];

        if ((o_ptr->tval != TV_CLOAK) || (o_ptr->sval < 100))
        {
                msg_print("You are not wearing any cloaks of mimicry.");
                return;
        }

        chance = get_mimic_chance(((o_ptr->sval - 100) * 2) + 50);

        if (chance > 75)
        {
                msg_print("You feel uneasy with this shape-change.");
                if (!get_check("Try it anyway? "))
                {
                        return;
                }
        }

        if (randint(100) < chance)
        {
                msg_print("Your shape-change goes horribly wrong!");

                if (randint(100) < p_ptr->skill_sav)
                {
                        msg_print("You manage to wrest your body back under control.");
                }
                else
                {
                        set_mimic(30, MIMIC_ABOMINATION);

                        /* Redraw title */
                        p_ptr->redraw |= (PR_TITLE);
                        /* Recalculate bonuses */
                        p_ptr->update |= (PU_BONUS);
                }
        }
        else
        {
                set_mimic(k_info[o_ptr->k_idx].pval2, (o_ptr->sval - 100));

                /* Redraw title */
                p_ptr->redraw |= (PR_TITLE);
                /* Recalculate bonuses */
                p_ptr->update |= (PU_BONUS);
        }

        }
        else
        {
                msg_print("You are already transformed !");
                if (p_ptr->mimic_form != MIMIC_ABOMINATION)
                {
                        if (!(get_check("Turn into an abomination to cancel ? ")))
                        {
                                return;
                        }
                        set_mimic(5, MIMIC_ABOMINATION);
                        /* Redraw title */
                        p_ptr->redraw |= (PR_TITLE);
                        /* Recalculate bonuses */
                        p_ptr->update |= (PU_BONUS);
                }
        }
}

/*
 * do_cmd_cast calls this function if the player's class
 * is 'mimic'.
 */
void do_cmd_mimic(void)
{
	int             n = 0,  b = 0;
	int             chance;
	int             minfail = 0;
	int             plev = p_ptr->lev;
        magic_power     spell;

        /* No magic */
        if (p_ptr->antimagic)
        {
                msg_print("Your anti-magic field disrupts any magic attempts.");
                return;
        }

	/* not if confused */
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}

	/* get power */
        if (!get_magic_power(&n, mimic_powers, MAX_MIMIC_POWERS, mimic_info))  return;

        spell = mimic_powers[n];

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
	chance -= 3 * (adj_mag_stat[p_ptr->stat_ind[cp_ptr->spell_stat]] - 1);

	/* Not enough mana to cast */
	if (spell.mana_cost > p_ptr->csp)
	{
		chance += 5 * (spell.mana_cost - p_ptr->csp);
	}

	/* Extract the minimum failure rate */
	minfail = adj_mag_fail[p_ptr->stat_ind[cp_ptr->spell_stat]];

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
                        else
			{
				set_stun(p_ptr->stun + randint(8));
			}
		}
	}
	else
	{
		sound(SOUND_ZAP);

		/* spell code */
		switch (n)
		{
                case 0:   /* Mimic */
                        do_cmd_mimic_lore();
			break;
                case 1:   /* Invisibility */
                {
                        int ii = 10 + p_ptr->lev + randint(20) + p_ptr->to_s;
                        set_invis(p_ptr->tim_invisible + ii, 50);
                        set_tim_invis(p_ptr->tim_invisible + ii);
			break;
                }
                case 2:   /* Legs Mimicry */
                {
                        /* Extract the value and the flags */
                        u32b value = p_ptr->class_extra6 >> 16,
                             att = p_ptr->class_extra6 & 0xFFFF;

                        /* Clear useless things */
                        att &= ~CLASS_ARMS;
                        att &= ~CLASS_WALL;

                        if (att & CLASS_LEGS)
                        {
                                value += 50 + randint(50 + (2 * plev));
                        }
                        else
                        {
                                msg_print("You mimic a new pair of legs.");

                                value = 50 + randint(50 + (2 * plev));
                                att |= CLASS_LEGS;
                        }

                        if (value > 10000) value = 10000;

                        p_ptr->class_extra6 = att + (value << 16);
                        p_ptr->update |= PU_BODY;
			break;
                }
                case 3:   /* Wall Mimicry */
                {
                        /* Extract the value and the flags */
                        u32b value = p_ptr->class_extra6 >> 16,
                             att = p_ptr->class_extra6 & 0xFFFF;

                        /* Clear useless things */
                        att &= ~CLASS_ARMS;
                        att &= ~CLASS_LEGS;

                        if (att & CLASS_WALL)
                        {
                                value += 50 + randint(50 + (2 * plev));
                        }
                        else
                        {
                                msg_print("You grow an affinity for walls.");

                                value = 50 + randint(50 + (2 * plev));
                                att |= CLASS_WALL;
                        }

                        if (value > 10000) value = 10000;

                        p_ptr->class_extra6 = att + (value << 16);
                        p_ptr->update |= PU_BODY;
			break;
                }
                case 4:   /* Arms Mimicry */
                {
                        /* Extract the value and the flags */
                        u32b value = p_ptr->class_extra6 >> 16,
                             att = p_ptr->class_extra6 & 0xFFFF;

                        /* Clear useless things */
                        att &= ~CLASS_LEGS;
                        att &= ~CLASS_WALL;

                        if (att & CLASS_ARMS)
                        {
                                value += 50 + randint(50 + (2 * plev));
                        }
                        else
                        {
                                msg_print("You mimic a new pair of arms.");

                                value = 50 + randint(50 + (2 * plev));
                                att |= CLASS_ARMS;
                        }

                        if (value > 10000) value = 10000;

                        p_ptr->class_extra6 = att + (value << 16);
                        p_ptr->update |= PU_BODY;
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
		p_ptr->csp -= spell.mana_cost;
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
                        (void)dec_stat(A_DEX, 15 + randint(10), perm);
		}
	}

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);
	p_ptr->window |= (PW_SPELL);
}

/*
 * do_cmd_cast calls this function if the player's class
 * is 'beastmaster'.
 */
void do_cmd_beastmaster(void)
{
        int plev = p_ptr->lev,i;
	monster_type    *m_ptr;

	/* Process the monsters (backwards) */
        p_ptr->class_extra1 = 0;
	for (i = m_max - 1; i >= 1; i--)
	{
		/* Access the monster */
		m_ptr = &m_list[i];

                if (m_ptr->status == MSTATUS_PET)
		{
                        p_ptr->class_extra1++;
		}
        }
        if(p_ptr->class_extra1<plev*2){
                if(rand_int(80-(plev)-p_ptr->stat_use[5]-p_ptr->to_s)<20){
                        summon_specific_friendly(py, px, plev, rand_int(plev/2), FALSE);
                }
        }
        else msg_print("You can't summon more pets");

	/* Take a turn */
        if (is_magestaff()) energy_use = 80;
        else energy_use = 100;

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);
	p_ptr->window |= (PW_SPELL);
}

int alchemist_baterie = -1;
int alchemist_charge = 0;
int alchemist_num = -1;
bool alchemist_ego = FALSE;

/*
 * Hook to determine if an object is usesable with a power baterie
 */
static bool item_tester_hook_powerable(object_type *o_ptr)
{
        int i;

        /* No artifacts */
        if (artifact_p(o_ptr)) return (FALSE);

        for(i = 0; i < 9; i++)
                if((alchemist_recipes[alchemist_baterie].item[i].ctval==o_ptr->tval)&&(alchemist_recipes[alchemist_baterie].item[i].csval==o_ptr->sval)) return TRUE;

        for(i = 0; i < 9; i++)
                if(alchemist_recipes[alchemist_baterie].ego[i].which==o_ptr->tval) return TRUE;

	/* Assume not */
	return (FALSE);
}
/*
 * Hook to determine if an object is extractable in a power baterie
 */
static bool item_tester_hook_extractable(object_type *o_ptr)
{
        object_kind *k_ptr = &k_info[o_ptr->k_idx];
        u32b f1, f2, f3, f4, f5, esp;

	/* Extract the flags */
        object_flags(o_ptr, &f1, &f2, &f3, &f4, &f5, &esp);

        /* No artifacts */
        if (artifact_p(o_ptr)) return (FALSE);

        /* No cursed things */
        if (cursed_p(o_ptr)) return (FALSE);

        if(((o_ptr->tval == TV_POTION)||
           (o_ptr->tval == TV_POTION2)||
           ((o_ptr->tval == TV_WAND) && (!(f4 & TR4_RECHARGED))) ||
           ((o_ptr->tval == TV_STAFF) && (!(f4 & TR4_RECHARGED))) ||
           (o_ptr->tval == TV_RING)||
           (o_ptr->tval == TV_AMULET)||
           (o_ptr->tval == TV_SCROLL)||
           ((o_ptr->tval == TV_ROD_MAIN) && o_ptr->pval)||
           (o_ptr->tval == TV_ROD))
           && (!k_ptr->know)) return TRUE;

	/* Assume not */
	return (FALSE);
}

bool get_alchemist_target(int *i)
{
        int item, a;
        object_type *o_ptr;

	cptr q, s;

        item_tester_hook = item_tester_hook_powerable;

	/* Get an item */
        q = "Apply to which item? ";
        s = "You have no item to apply it.";
        if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return FALSE;

        *i=item;

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

	if (o_ptr->tval==TV_STAFF)
	{
		alchemist_charge = o_ptr->pval;
	}
	else if (o_ptr->tval==TV_WAND)
	{
		alchemist_charge = o_ptr->pval / o_ptr->number;
	}

        for(a = 0; a < 9; a++)
                if((alchemist_recipes[alchemist_baterie].item[a].ctval==o_ptr->tval)&&(alchemist_recipes[alchemist_baterie].item[a].csval==o_ptr->sval)) {alchemist_num = a; alchemist_ego = FALSE; return TRUE;}

        for(a = 0; a < 9; a++)
                if(alchemist_recipes[alchemist_baterie].ego[a].which==o_ptr->tval) {alchemist_num = a; alchemist_ego = TRUE; return TRUE;}

        return TRUE;
}

/* Extract a rod tip from a rod */
void rod_tip_extract(object_type *o_ptr, int item)
{
        object_type *q_ptr;
        object_type forge;

        /* Get local object */
        q_ptr = &forge;

        /* Extract the rod tip */
        object_prep(q_ptr, lookup_kind(TV_ROD, o_ptr->pval));
        q_ptr->number = 1;
        object_aware(q_ptr);
        object_known(q_ptr);
        (void)inven_carry(q_ptr, FALSE);

        /* Remove it from the rod */
        o_ptr->pval = SV_ROD_NOTHING;

        /* Window stuff */
        p_ptr->window |= (PW_INVEN);
}

static bool magic_essence()
{
        int i;
        int j = 0;

        for (i = 0; i < INVEN_WIELD; i++)
        {
                object_type *o_ptr = &inventory[i];

                /* Count the magic essenses */
                if (o_ptr->k_idx && (o_ptr->tval == TV_BATERIE) && (o_ptr->sval == SV_BATERIE_MAGIC)) j += o_ptr->number;
        }

        if (j >= p_ptr->lev)
        {
                /* Consume them */
                msg_print("All your magic essences are absorbed in the effort.");

                for (i = 0; i < INVEN_WIELD; i++)
                {
                        object_type *o_ptr = &inventory[i];

                        if (o_ptr->k_idx && (o_ptr->tval == TV_BATERIE) && (o_ptr->sval == SV_BATERIE_MAGIC))
                        {
                                inven_item_increase(i, -255);
                                inven_item_describe(i);
                                inven_item_optimize(i);
                        }
                }

                return TRUE;
        }
        else return FALSE;
}

/* Begin & finish an art */
void do_cmd_toggle_artifact(bool finish)
{
        object_type *q_ptr;
        char o_name[80];
        int item;
        cptr q, s;

        if (!finish)
        {
                bool okay = TRUE;

                msg_print("Creating an artifact will result into a permanent loss of 1 hp.");
                if (!get_check("Are you sure you want to do that?")) return;

                if (!magic_essence())
                {
                        msg_format("You need %d magic essenses.", p_ptr->lev);
                        return;
                }
                /* Restrict choices to artifactable items */
                item_tester_hook = item_tester_hook_artifactable;

                /* Get an item */
                q = "Use which item? ";
                s = "You have nothing to use.";
                if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

                /* Get the item (in the pack) */
                if (item >= 0)
                {
                        q_ptr = &inventory[item];
                }

                /* Get the item (on the floor) */
                else
                {
                        q_ptr = &o_list[0 - item];
                }

                /* Description */
                object_desc(o_name, q_ptr, FALSE, 0);

                if (artifact_p(q_ptr))
                {
                        msg_format("The %s %s already %s!",
                            o_name, ((q_ptr->number > 1) ? "are" : "is"),
                            ((q_ptr->number > 1) ? "artifacts" : "an artifact"));
                        okay = FALSE;
                }

                else if (q_ptr->name2)
                {
                        msg_format("The %s %s already %s!",
                            o_name, ((q_ptr->number > 1) ? "are" : "is"),
                            ((q_ptr->number > 1) ? "ego items" : "an ego item"));
                        okay = FALSE;
                }

                else
                {
                        if (q_ptr->number > 1)
                        {
                                msg_print("Not enough energy to enchant more than one object!");
                                msg_format("%d of your %s %s destroyed!",(q_ptr->number)-1, o_name, (q_ptr->number>2?"were":"was"));
                                q_ptr->number = 1;
                        }
                        okay = TRUE;
                }

                if(!okay) return;

                /* he/she got warned */
                p_ptr->hp_mod -= 1;

                /* Ok toggle it */
                q_ptr->art_flags4 |= TR4_ART_EXP;

                p_ptr->update |= (PU_HP);
                p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);
        }
        else
        {
                do_cmd_create_artifact();
        }
}

/*
 * do_cmd_cast calls this function if the player's class
 * is 'alchemist'.
 */
void do_cmd_alchemist(void)
{
        int item, used_up, i,ext=0, a, b = -1;
        char ch;
        byte create_q_ptr=FALSE;

	object_type	*o_ptr;
	object_type	forge;
        object_type     *q_ptr;
	object_type	forge2;
        byte carry_o_ptr = FALSE;

        cptr q, s;
        char com[80];


        /* No magic */
        if (p_ptr->antimagic)
        {
                msg_print("Your anti-magic field disrupts any magic attempts.");
                return;
        }

        q_ptr = &forge;

        alchemist_charge = 0;

        o_ptr = &inventory[INVEN_HANDS];
        if((o_ptr->tval != TV_GLOVES) || (o_ptr->sval != SV_SET_OF_LEATHER_GLOVES))
        {
                msg_print("You must wear gloves in order to do alchemy.");
                return;
        }

        if(p_ptr->lev >= 25)
                sprintf(com, "[A]dd, [E]xtract a power, [C]reate an artifact or Ac[T]ivate an Artifact ?");
        else
                sprintf(com, "[A]dd, [E]xtract a power ?");

	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}

        while (TRUE)
        {
                if (!get_com(com, &ch))
                {
                        ext = 0;
                        break;
                }
                if (ch == 'A' || ch == 'a')
                {
                        ext = 1;
                        break;
                }
                if (ch == 'E' || ch == 'e')
                {
                        ext = 2;
                        break;
                }
                if ((ch == 'C' || ch == 'c') && (p_ptr->lev >= 25))
                {
                        ext = 3;
                        break;
                }
                if ((ch == 'T' || ch == 't') && (p_ptr->lev >= 25))
                {
                        ext = 4;
                        break;
                }
        }

        /**********Add a power*********/
        if(ext==1){
        /* Restrict choices to bateries */
        item_tester_tval = TV_BATERIE;

	/* Get an item */
        q = "Use which baterie? ";
        s = "You have no bateries to use.";
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


	/* Take a turn */
	energy_use = 100;

        /* Assume the baterie will get used up */
	used_up = TRUE;

        for (a = 0; a <= MAX_ALCHEMIST_RECIPES; a++)
        {
                if (alchemist_recipes[a].sval_baterie == o_ptr->sval) break;
        }

        alchemist_baterie = a;

        used_up=get_alchemist_target(&i);

        if (used_up == TRUE){
                if (alchemist_ego)
                {
                        if(alchemist_recipes[a].ego[alchemist_num].ego_num > o_ptr->number)
                        {
                               q=format("You need at least %d bateries !",alchemist_recipes[a].ego[alchemist_num].ego_num);
                               msg_print(q);
                               used_up=FALSE;
                               goto fin_alchemist;
                        }
                        if (i >= 0)
                        {
                                q_ptr = &inventory[i];
                        }
                        else
                        {
                                q_ptr = &o_list[0 - i];
                        }
                        if(q_ptr->name2==0){
                                if(q_ptr->number>1){
                                        msg_print("You can't enchant more than one item !");
                                        used_up=FALSE;
                                        goto fin_alchemist;
                                }
                                q_ptr->name2=alchemist_recipes[a].ego[alchemist_num].ego;
                                apply_magic(q_ptr, dun_level, FALSE, FALSE, FALSE);
/*                                if(alchemist_recipes[a].ego[alchemist_num].enchant & ALCHEMIST_ENCHANT_DAM){
                                        q_ptr->to_h=rand_int(4+p_ptr->lev/5)+1;
                                        q_ptr->to_d=rand_int(4+p_ptr->lev/5)+1;
                                }
                                if(alchemist_recipes[a].ego[alchemist_num].enchant & ALCHEMIST_ENCHANT_PVAL){
                                        q_ptr->pval=rand_int(4+p_ptr->lev/5)+1;
                                }
                                if(alchemist_recipes[a].ego[alchemist_num].enchant & ALCHEMIST_ENCHANT_AC){
                                        q_ptr->to_a=rand_int(5+p_ptr->lev/5)+4;
                                }*/
                        }else{
                                msg_print("This object is already enchanted !");
                                used_up=FALSE;
                                goto fin_alchemist;
                        }
                }else{
                        if(alchemist_recipes[a].item[alchemist_num].num>o_ptr->number)
                        {
                                q=format("You need at least %d bateries !",alchemist_recipes[a].item[alchemist_num].num);
                                msg_print(q);
                                used_up=FALSE;
                                goto fin_alchemist;
                        }

                        q_ptr = &forge;
                        object_wipe(q_ptr);
                        object_prep(q_ptr, lookup_kind(alchemist_recipes[a].item[alchemist_num].etval, alchemist_recipes[a].item[alchemist_num].esval));
			if ((q_ptr->tval == TV_STAFF) || (q_ptr->tval == TV_WAND))
			{
				q_ptr->pval = alchemist_charge + 1;
			}
                        if((q_ptr->tval==TV_RING)||(q_ptr->tval==TV_AMULET))
                                apply_magic(q_ptr,max_dlv[dungeon_type],(randint(110-(max_dlv[dungeon_type]))==0)?TRUE:FALSE,
                                                                         FALSE,
                                                                         FALSE);
                        object_aware(q_ptr);
                        object_known(q_ptr);

                        q_ptr->ident |= IDENT_STOREB;

                        create_q_ptr=TRUE;
                }
        }
fin_alchemist:

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* The item was tried */
	object_tried(o_ptr);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);

        /* Hack -- allow certain bateries to be "preserved" */
	if (!used_up) return;

        /* Destroy a baterie in the pack */
	if (item >= 0)
	{
                inven_item_increase(item, -((alchemist_ego)?alchemist_recipes[a].ego[alchemist_num].ego_num:alchemist_recipes[a].item[alchemist_num].num));
                inven_item_describe(item);
	}

        /* Destroy a baterie on the floor */
	else
	{
                floor_item_increase(0 - item, -((alchemist_ego)?alchemist_recipes[a].ego[alchemist_num].ego_num:alchemist_recipes[a].item[alchemist_num].num));
		floor_item_describe(0 - item);
		floor_item_optimize(0 - item);
	}
        if(create_q_ptr==TRUE){
                                if (i >= 0)
                                {
                                        inven_item_increase(i, -1);
                                        /* reduce wand's charge */
                                        if (inventory[i].tval == TV_WAND)
                                        {
                                                inventory[i].pval -= alchemist_charge;
                                        }
                                        inven_item_describe(i);
                                        inven_item_optimize(i);
                                }
                                else
                                {
                                        floor_item_increase(0 - i, -1);
                                        /* reduce wand's charge */
                                        if (o_list[0 - i].tval == TV_WAND)
                                        {
                                                o_list[0 - i].pval -= alchemist_charge;
                                        }
                                        floor_item_describe(0 - i);
                                        floor_item_optimize(0 - i);
                                }
                inven_carry(q_ptr,TRUE);
        }
        for(item=0;item<36;item++)inven_item_optimize(item);


        /**********Extract a power*********/
        }else if(ext==2){

        item_tester_hook = item_tester_hook_extractable;

	/* Get an item */
        q = "Extract from which item? ";
        s = "You have no item to extract power from.";
        if (!get_item(&item, q, s, (USE_INVEN))) return;

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

        if (o_ptr->tval == TV_ROD_MAIN)
        {
                rod_tip_extract(o_ptr, item);
                return;
        }

        for(a=0;a<=MAX_ALCHEMIST_RECIPES;a++){
                for(b = 0; b < 9; b++)
                        if((alchemist_recipes[a].item[b].etval == o_ptr->tval)&&(alchemist_recipes[a].item[b].esval == o_ptr->sval)) goto fin_alchemist_2;
        }
        b = -1;
fin_alchemist_2:
        switch(b){
                case -1:
                {
                        object_kind *k_ptr = &k_info[o_ptr->k_idx];

                        k_ptr->know = TRUE; /* Don't try this item anymore */
                        break;
                }
                default:
			/* XXX Hack -- unstack if necessary */
			if ((o_ptr->tval==TV_STAFF) && (o_ptr->number > 1))
			{
				/* Get local object */
				q_ptr = &forge2;

				/* Obtain a local object */
				object_copy(q_ptr, o_ptr);

				/* Modify quantity */
				q_ptr->number = 1;

				/* Unstack the used item */
				o_ptr->number--;

				o_ptr = q_ptr;

				carry_o_ptr = TRUE;
			}

			if(((o_ptr->tval==TV_WAND) || (o_ptr->tval==TV_STAFF)) &&
			   (o_ptr->pval>0))
			{
				o_ptr->pval--;
                        }

			if (carry_o_ptr == TRUE)
			{
				item = inven_carry(o_ptr, FALSE);
			}

                        if(((o_ptr->tval!=TV_WAND) && (o_ptr->tval!=TV_STAFF)) ||
                           (o_ptr->number > o_ptr->pval))
                        {
                                q_ptr = &forge;
                                object_wipe(q_ptr);
                                object_prep(q_ptr, lookup_kind(alchemist_recipes[a].item[b].ctval,alchemist_recipes[a].item[b].csval));
                                q_ptr->number = 1;
                                object_aware(q_ptr);
                                object_known(q_ptr);
                                q_ptr->ident |= IDENT_STOREB;
                                create_q_ptr=TRUE;
                        }
                        break;
        }
        if (b != -1)
        {
                if(create_q_ptr==TRUE){
                        if (item >= 0)
                        {
                                inven_item_increase(item, -1);
                                inven_item_describe(item);
                                inven_item_optimize(item);
                        }
                        else
                        {
                                floor_item_increase(0 - item, -1);
                                floor_item_describe(0 - item);
                                floor_item_optimize(0 - item);
                        }
                        inven_carry(q_ptr,TRUE);
                }

                q_ptr = &forge;
                object_wipe(q_ptr);
                object_prep(q_ptr, lookup_kind(TV_BATERIE,alchemist_recipes[a].sval_baterie));
                q_ptr->number = alchemist_recipes[a].item[b].num;
                object_aware(q_ptr);
                object_known(q_ptr);
                q_ptr->ident |= IDENT_STOREB;
                inven_carry(q_ptr,TRUE);
        }

        /*******Create an artifact*******/
        }else if(ext == 3)
        {
                do_cmd_toggle_artifact(FALSE);
        }
        else if(ext == 4)
        {
                do_cmd_toggle_artifact(TRUE);
        }

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);
	p_ptr->window |= (PW_SPELL);
}

/* Helping side effect. */
void help_side_effect(void) {
  int tmp;

  tmp = randint(100);

  if (tmp <= 10) {
    if (summon_specific_friendly(py, px, dun_level+rand_spread(10, 5), damroll(4,6), FALSE)) {
      msg_print("Something materializes out of thin air.");
    } else {
      msg_print("You feel a strange tingling, but the feeling passes.");
    }
  } else if (tmp <= 20) {
    fire_explosion(py, px, GF_AWAY_ALL, 9, 0);
  } else if (tmp <= 30) {
    hp_player(damroll(p_ptr->lev, 20));
    (void)set_poisoned(0);
    (void)set_blind(0);
    (void)set_confused(0);
    (void)set_image(0);
    (void)set_stun(0);
    (void)set_cut(0);
  } else if (tmp <= 40) {
    hp_player(damroll(p_ptr->lev, 40));
    (void)set_poisoned(0);
    (void)set_blind(0);
    (void)set_confused(0);
    (void)set_image(0);
    (void)set_stun(0);
    (void)set_cut(0);
  } else if (tmp <= 50) {
    fire_explosion(py, px, GF_MAKE_GLYPH, 9, 0);
  } else if (tmp <= 60) {
    fire_explosion(py, px, GF_MISSILE, 6, 0);
  } else if (tmp <= 70) {
    fire_explosion(py, px, GF_DESTRUCTION, 8, 0);
  } else if (tmp <= 80) {
    fire_explosion(py, px, GF_MISSILE, 9, 0);
  } else if (tmp <= 90) {
    msg_print("The power of your deity makes the dungeon change!");
    alter_reality();
    hp_player(5000);
    (void)set_poisoned(0);
    (void)set_blind(0);
    (void)set_confused(0);
    (void)set_image(0);
    (void)set_stun(0);
    (void)set_cut(0);
    (void)do_res_stat(A_STR);
    (void)do_res_stat(A_CON);
    (void)do_res_stat(A_DEX);
    (void)do_res_stat(A_WIS);
    (void)do_res_stat(A_INT);
    (void)do_res_stat(A_CHR);
  } else if (tmp <= 100) {
    msg_print("Let me help thou !");
    project_hack(GF_MISSILE, p_ptr->lev + dun_level * 3);
  }
}

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

  if (!get_check("Are you sure you want to disturb your deity? ")) return;

  level = interpret_grace() - interpret_favor();
  name = deity_info[p_ptr->pgod-1].name;

  if ((PRACE_FLAGS(PR1_GOD_FRIEND)) && magik(30))
  {
          level++;
  }

  if (level < 0) level = 0;
  if (level > 10) level = 10;

  energy_use = 100;

  /* Your God can help you in hard times ... */
  if((p_ptr->chp < (p_ptr->mhp / 10)) && (level > 5))
  {
    msg_format("%s thunders: ``Thou are in great danger, thou need some help.''", name);
    help_side_effect();
  }
  else
  switch (level) {
  case 10: case 9: case 8:
    msg_format("%s thunders: ``Thou hast pleaseth me, mortal.''", name);
    great_side_effect();
    break;

  case 7:
  case 6:
    msg_format("%s hisses: ``Leave me alone now, mortal!''", name);
    if (magik(30)) set_grace(p_ptr->grace - 1000);
    break;

  case 5:
  case 4:
  case 3:
    msg_format("%s quakes in rage: ``Thou art supremely insolent, mortal!''", name);
    nasty_side_effect();
    set_grace(p_ptr->grace - 5000);
    break;

  case 2:
  case 1:
  case 0:
    msg_format("%s whispers: ``Prepare to die, mortal...''", name);
    deadly_side_effect(TRUE);
    set_grace(p_ptr->grace - 20000);
    break;
  }

  p_ptr->god_favor += 25000;
}

/*
 * Return percentage chance of spell failure.
 */
int spell_chance_random(random_spell* rspell) {
  int chance, minfail;


  /* Extract the base spell failure rate */
  chance = rspell->level + 25;

  /* Reduce failure rate by "effective" level adjustment */
  chance -= 3 * (p_ptr->lev - rspell->level);

  /* Reduce failure rate by INT/WIS adjustment */
  chance -= 3 * (adj_mag_stat[p_ptr->stat_ind[cp_ptr->spell_stat]] - 1);

  /* Not enough mana to cast */
  if (rspell->mana > p_ptr->csp) {
    chance += 5 * (rspell->mana - p_ptr->csp);
  }

  /* Extract the minimum failure rate */
    minfail = adj_mag_fail[p_ptr->stat_ind[cp_ptr->spell_stat]];

  /* Minimum failure rate */
  if (chance < minfail) chance = minfail;

  /* Stunning makes spells harder */
  if (p_ptr->stun > 50) chance += 25;
  else if (p_ptr->stun) chance += 15;

  /* Always a 5 percent chance of working */
  if (chance > 95) chance = 95;

  /* Return the chance */
  return (chance);
}



/*
 * Print a batch of spells.
 */
static void print_spell_batch(int batch, int max) {
  char buff[80];
  random_spell* rspell;
  int i;

  prt(format("      %-30s Lev Fail Mana  ", "Name"), 1, 20);

  for (i = 0; i < max; i++) {
    rspell = &random_spells[batch*10+i];

    if (rspell->untried) {
      sprintf(buff, "  %c) %-30s  (Spell untried)  ", I2A(i),
	      rspell->name);

    } else {
      sprintf(buff, "  %c) %-30s %3d %4d%% %3d  ", I2A(i), rspell->name,
              rspell->level, spell_chance_random(rspell), rspell->mana);
    }

    prt(buff, 2+i, 20);
  }
  prt("", 2+i, 20);
}



/*
 * List ten random spells and ask to pick one.
 */

static random_spell* select_spell_from_batch(int batch, bool quick) {
  char tmp[160];
  char out_val[30];
  char which;
  int mut_max = 10;
  random_spell* ret;

  character_icky = TRUE;
  Term_save();

  if (spell_num < (batch+1)*10) {
    mut_max = spell_num - batch*10;
  }

  sprintf(tmp, "(a-%c, * to list, / to rename, - to comment) Select a power: ",
	  I2A(mut_max-1));

  prt(tmp, 0, 0);

  if (quick) {
    print_spell_batch(batch, mut_max);
  }

  while (1) {
    which = inkey();

    if (which == ESCAPE) {
      ret = NULL;
      break;

    } else if (which == '*'  || which == '?' || which == ' ') {
      print_spell_batch(batch, mut_max);

    } else if (which == '\r' && mut_max == 1) {
      ret = &random_spells[batch*10];
      break;

    } else if (which == '/') {
      prt("Rename which power: ", 0, 0);
      which = tolower(inkey());

      if (islower(which) && A2I(which) <= mut_max) {
	strcpy(out_val, random_spells[batch*10+A2I(which)].name);
	if (get_string("Name this power: ", out_val, 29))
	{
	  strcpy(random_spells[batch*10+A2I(which)].name, out_val);
	}
	prt(tmp, 0, 0);
      } else {
	bell();
	prt(tmp, 0, 0);
      }

    } else if (which == '-') {
      prt("Comment which power: ", 0, 0);
      which = tolower(inkey());

      if (islower(which) && A2I(which) <= mut_max) {
	strcpy(out_val, random_spells[batch*10+A2I(which)].desc);
	if (get_string("Comment this power: ", out_val, 29))
	{
		strcpy(random_spells[batch*10+A2I(which)].desc, out_val);
	}
	prt(tmp, 0, 0);
      } else {
	bell();
	prt(tmp, 0, 0);
      }

    } else {
      which = tolower(which);
      if (islower(which) && A2I(which) < mut_max) {
        ret = &random_spells[batch*10+A2I(which)];
	break;
      } else {
	bell();
      }
    }
  }

  Term_load();
  character_icky = FALSE;

  return ret;
}


/*
 * Pick a random spell from a menu
 */

random_spell* select_spell(bool quick) {
  char tmp[160];
  char which;
  int batch_max = (spell_num-1)/10;

  if (spell_num == 0) {
    msg_print("There are no spells you can cast.");
    return NULL;
  }

  if (p_ptr->confused) {
    msg_print("You can't use your powers while confused!");
    return NULL;
  }

  character_icky = TRUE;
  Term_save();

  sprintf(tmp, "(a-%c) Select batch of powers: ", I2A(batch_max));

  prt(tmp, 0, 0);

  while (1) {
    which = inkey();

    if (which == ESCAPE) {
      Term_load();
      character_icky = FALSE;
      return NULL;

    } else if (which == '\r' && batch_max == 0) {
      Term_load();
      character_icky = FALSE;
      return select_spell_from_batch(0, quick);

    } else {
      which = tolower(which);
      if (islower(which) && A2I(which) <= batch_max) {
	Term_load();
	character_icky = FALSE;
	return select_spell_from_batch(A2I(which), quick);
      } else {
	bell();
      }
    }
  }
}


void do_cmd_powermage(void)
{
        random_spell *s_ptr;

        int dir, chance;
        int ty = 0, tx = 0;

        /* No magic */
        if (p_ptr->antimagic)
        {
                msg_print("Your anti-magic field disrupts any magic attempts.");
                return;
        }

        s_ptr = select_spell(FALSE);

        if (s_ptr == NULL) return;

        if(p_ptr->csp < s_ptr->mana)
        {
                msg_print("You do not have enough mana.");
                return;
        }

	/* Spell failure chance */
        chance = spell_chance_random(s_ptr);

	/* Failed spell */
	if (rand_int(100) < chance)
	{
                char sfail[80];

		if (flush_failure) flush();

                get_rnd_line("sfail.txt",sfail);

                msg_format("A cloud of %s appears above you.", sfail);
		sound(SOUND_FAIL);

		/* Let time pass */
		if(is_magestaff()) energy_use = 80;
		else energy_use = 100;

		/* Mana is spent anyway */
		p_ptr->csp -= s_ptr->mana;

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);
		p_ptr->window |= (PW_SPELL);
		p_ptr->redraw |= (PR_MANA);

                return;
	}


        p_ptr->csp -= s_ptr->mana;

        s_ptr->untried = FALSE;

        /* Hack -- Spell needs a target */
        if (s_ptr->proj_flags & PROJECT_BEAM || s_ptr->proj_flags & PROJECT_STOP)
        {
                if (!get_aim_dir(&dir)) return;

                /* Hack -- Use an actual "target" */
                if ((dir == 5) && target_okay()) {
                        tx = target_col;
                        ty = target_row;
                } else {
                        /* Use the given direction */
                        ty = py + ddy[dir];
                        tx = px + ddx[dir];
                }
        }

        if (s_ptr->proj_flags & PROJECT_BLAST) {
                ty = py;
                tx = px;
        }

        if(s_ptr->proj_flags & PROJECT_VIEWABLE)
        {
                project_hack(s_ptr->GF,damroll(s_ptr->dam_dice, s_ptr->dam_sides));
        }
        else
        if(s_ptr->proj_flags & PROJECT_METEOR_SHOWER)
        {
                project_meteor(s_ptr->radius, s_ptr->GF, damroll(s_ptr->dam_dice, s_ptr->dam_sides), s_ptr->proj_flags);
        }
        else
        {
                project(0, s_ptr->radius, ty, tx,
                        damroll(s_ptr->dam_dice, s_ptr->dam_sides),
                        s_ptr->GF, s_ptr->proj_flags);
        }

	/* Take a turn */
        if (is_magestaff()) energy_use = 80;
        else energy_use = 100;

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);
	p_ptr->window |= (PW_SPELL);
        p_ptr->redraw |= (PR_MANA);
}

/*
 * Old magic system
 */

/*
 * Cast a spell
 */
void cast_magic_spell(int spell, byte level)
{
        int  dir;
        int  beam;
        int  plev = p_ptr->lev;
        byte to_s = level + p_ptr->to_s;
        long dam, rad;

        /* Hack -- chance of "beam" instead of "bolt" */
        if (PRACE_FLAG(PR1_BEAM)) beam = plev + 10;
        else beam = plev / 2;


		/* Spells.  */
		switch (spell)
		{
			case 0:
			{
                                dam = apply_power_dice(4, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dam %dd%ld", 3+((plev-1)/5), dam);
                                        return;
                                }
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_MISSILE, dir,
                                                  damroll(3 + ((plev - 1) / 5), dam));
				break;
			}

			case 1:
			{
                                if (info_spell) return;
				(void)detect_monsters_normal();
				break;
			}

			case 2:
			{
                                dam = apply_power_dice(10, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " range %ld", dam);
                                        return;
                                }
                                teleport_player(dam);
				break;
			}

			case 3:
			{
                                if (info_spell) return;
                                (void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1 + to_s);
				break;
			}

			case 4:
			{
                                if (info_spell) return;
				(void)detect_treasure();
				(void)detect_objects_gold();
				break;
			}

			case 5:
			{
                                dam = apply_power_dice(8, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " heal 2d%ld", dam);
                                        return;
                                }
                                (void)hp_player(damroll(2, dam));
                                (void)set_cut(p_ptr->cut - 15 - to_s);
				break;
			}

			case 6:
			{
                                if (info_spell) return;
				(void)detect_objects_normal();
				break;
			}

			case 7:
			{
                                if (info_spell) return;
				(void)detect_traps();
				(void)detect_doors();
				(void)detect_stairs();
				break;
			}

			case 8:
			{
                                rad = apply_power_dice(2, to_s, 1);
                                dam = apply_power_dam(10 + (plev/2), to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dam %ld", dam);
                                        return;
                                }
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_POIS, dir,
                                          dam, rad);
				break;
			}

			case 9:
			{
                                dam = apply_power_dam(plev, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " power %ld", dam);
                                        return;
                                }
				if (!get_aim_dir(&dir)) return;
                                (void)confuse_monster(dir, dam);
				break;
			}

			case 10:
			{
                                dam = apply_power_dice(8, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dam %dd%ld", 3+((plev-5)/4), dam);
                                        return;
                                }
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_ELEC, dir,
                                                  damroll(3+((plev-5)/4), dam));
				break;
			}

			case 11:
			{
                                if (info_spell) return;
				(void)destroy_doors_touch();
				break;
			}

			case 12:
			{
                                if (info_spell) return;
				if (!get_aim_dir(&dir)) return;
				(void)sleep_monster(dir);
				break;
			}

			case 13:
			{
                                if (info_spell) return;
				(void)set_poisoned(0);
				break;
			}

			case 14:
			{
                                dam = apply_power_dam(plev * 5, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " range %ld", dam);
                                        return;
                                }
                                teleport_player(dam);
				break;
			}

			case 15:
			{
                                if (info_spell) return;
				if (!get_aim_dir(&dir)) return;
				msg_print("A line of blue shimmering light appears.");
				lite_line(dir);
				break;
			}

			case 16:
			{
                                dam = apply_power_dice(8, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dam %dd%ld", 5+((plev-5)/4), dam);
                                        return;
                                }
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_COLD, dir,
                                                  damroll(5+((plev-5)/4), dam));
				break;
			}

			case 17:
			{
                                if (info_spell) return;
				if (!get_aim_dir(&dir)) return;
				(void)wall_to_mud(dir);
				break;
			}

			case 18:
			{
                                if (info_spell) return;
				(void)set_food(PY_FOOD_MAX - 1);
				break;
			}

			case 19:
			{
                                if (info_spell) return;
				(void)recharge(5);
				break;
			}

			case 20:
			{
                                if (info_spell) return;
				(void)sleep_monsters_touch();
				break;
			}

			case 21:
			{
                                if (info_spell) return;
				if (!get_aim_dir(&dir)) return;
				(void)poly_monster(dir);
				break;
			}

			case 22:
			{
                                if (info_spell) return;
				(void)ident_spell();
				break;
			}

			case 23:
			{
                                if (info_spell) return;
				(void)sleep_monsters();
				break;
			}

			case 24:
			{
                                dam = apply_power_dice(8, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dam %dd%ld", 8+((plev-5)/4), dam);
                                        return;
                                }
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_FIRE, dir,
                                                  damroll(8+((plev-5)/4), dam));
				break;
			}

			case 25:
			{
                                if (info_spell) return;
				if (!get_aim_dir(&dir)) return;
				(void)slow_monster(dir);
				break;
			}

			case 26:
			{
                                rad = apply_power_dice(2, to_s, 1);
                                dam = apply_power_dam(30 + plev, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dam %ld", dam);
                                        return;
                                }
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_COLD, dir,
                                          dam, rad);
				break;
			}

			case 27:
			{
                                if (info_spell) return;
				(void)recharge(40);
				break;
			}

			case 28:
			{
                                if (info_spell) return;
				if (!get_aim_dir(&dir)) return;
				(void)teleport_monster(dir);
				break;
			}

			case 29:
			{
                                dam = apply_power_dam(plev, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dur %ld+d20", dam);
                                        return;
                                }
				if (!p_ptr->fast)
				{
                                        (void)set_fast(randint(20) + dam);
				}
				else
				{
                                        (void)set_fast(p_ptr->fast + randint(5));
				}
				break;
			}

			case 30:
			{
                                rad = apply_power_dice(2, to_s, 1);
                                dam = apply_power_dam(55 + plev, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dam %ld", dam);
                                        return;
                                }
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_FIRE, dir,
                                          dam, rad);
				break;
			}

			case 31:
			{
                                if (info_spell) return;
				destroy_area(py, px, 15, TRUE);
				break;
			}

			case 32:
			{
                                if (info_spell) return;
                                (void)genocide(TRUE);
				break;
			}

			case 33:
			{
                                if (info_spell) return;
				(void)door_creation();
				break;
			}

			case 34:
			{
                                if (info_spell) return;
				(void)stair_creation();
				break;
			}

			case 35:
			{
                                if (info_spell) return;
				(void)teleport_player_level();
				break;
			}

			case 36:
			{
                                if (info_spell) return;
				earthquake(py, px, 10);
				break;
			}

			case 37:
			{
                                if (info_spell) return;
                                if (dungeon_flags1 & LF1_NO_TELEPORT)
				{
				msg_print("No recall on special levels..");
				break;
				}

				if (!p_ptr->word_recall)
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

			case 38:
			{
                                dam = apply_power_dam(8, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dam %dd%ld", 6+((plev-5)/4), dam);
                                        return;
                                }
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_ACID, dir,
                                                  damroll(6+((plev-5)/4), dam));
				break;
			}

			case 39:
			{
                                rad = apply_power_dice(3, to_s, 1);
                                dam = apply_power_dam(20 + (plev / 2), to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dam %ld", dam);
                                        return;
                                }
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_POIS, dir,
                                          dam, rad);
				break;
			}

			case 40:
			{
                                rad = apply_power_dice(2, to_s, 1);
                                dam = apply_power_dam(40 + plev, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dam %ld", dam);
                                        return;
                                }
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_ACID, dir,
                                          dam, rad);
				break;
			}

			case 41:
			{
                                rad = apply_power_dice(3, to_s, 1);
                                dam = apply_power_dam(70 + plev, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dam %ld", dam);
                                        return;
                                }
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_COLD, dir,
                                          dam, rad);
				break;
			}

			case 42:
			{
                                rad = apply_power_dice(3, to_s, 1);
                                dam = apply_power_dam(65 + plev, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dam %ld", dam);
                                        return;
                                }
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_METEOR, dir,
                                          dam, rad);
				break;
			}

			case 43:
			{
                                rad = apply_power_dice(3, to_s, 1);
                                dam = apply_power_dam(300 + (plev * 2), to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dam %ld", dam);
                                        return;
                                }
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_MANA, dir,
                                          dam, rad);
				break;
			}

			case 44:
			{
                                if (info_spell) return;
				(void)detect_monsters_evil();
				break;
			}

			case 45:
			{
                                if (info_spell) return;
				(void)detect_objects_magic();
				break;
			}

			case 46:
			{
                                if (info_spell) return;
				recharge(100);
				break;
			}

			case 47:
			{
                                if (info_spell) return;
                                (void)genocide(TRUE);
				break;
			}

			case 48:
			{
                                if (info_spell) return;
                                (void)mass_genocide(TRUE);
				break;
			}

			case 49:
			{
                                dam = apply_power_dam(20, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dur %ld+d20", dam);
                                        return;
                                }
                                (void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + dam);
				break;
			}

			case 50:
			{
                                dam = apply_power_dam(20, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dur %ld+d20", dam);
                                        return;
                                }
                                (void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + dam);
				break;
			}

			case 51:
			{
                                dam = apply_power_dam(20, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dur %ld+d20", dam);
                                        return;
                                }
                                (void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + dam);
				break;
			}

			case 52:
			{
                                dam = apply_power_dam(20, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dur %ld+d20", dam);
                                        return;
                                }
                                (void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + dam);
				break;
			}

			case 53:
			{
                                dam = apply_power_dam(20, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dur %ld+d20", dam);
                                        return;
                                }
                                (void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + dam);
                                (void)set_oppose_elec(p_ptr->oppose_elec + randint(20) + dam);
                                (void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + dam);
                                (void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + dam);
                                (void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + dam);
				break;
			}

			case 54:
			{
                                if (info_spell) return;
                                dam = apply_power_dice(10, to_s, 2);
                                (void)hp_player(dam);
                                (void)set_hero(p_ptr->hero + randint(25) + 25 + to_s);
				(void)set_afraid(0);
				break;
			}

			case 55:
			{
                                dam = apply_power_dam(30, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dur %ld+d20", dam);
                                        return;
                                }
                                (void)set_shield(p_ptr->shield + randint(20) + dam, 50 + to_s, 0);
				break;
			}

			case 56:
			{
                                if (info_spell) return;
                                dam = apply_power_dam(30, to_s, 1);
                                (void)hp_player(dam);
                                (void)set_shero(p_ptr->shero + randint(25) + 25 + to_s);
				(void)set_afraid(0);
				break;
			}

			case 57:
			{
                                dam = apply_power_dam(30 + plev, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dur %ld+d30", dam);
                                        return;
                                }
				if (!p_ptr->fast)
				{
                                        (void)set_fast(randint(30) + dam);
				}
				else
				{
                                        (void)set_fast(p_ptr->fast + randint(10));
				}
				break;
			}

			case 58:
			{
                                dam = apply_power_dice(5, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dur %ld+d8", dam);
                                        return;
                                }
                                (void)set_invuln(p_ptr->invuln + randint(8) + dam);
				break;
			}
                        default:
                                if (info_spell) return;
                                msg_format("You cast an unknown Magic spell: %d.", spell);
                                msg_print(NULL);
                                break;
		}
}

/*
 * Pray a prayer
 */
void cast_prayer_spell(int spell, byte level)
{
        int dir;
	int plev = p_ptr->lev;
        int to_s = level + p_ptr->to_s;
        long dam, rad;

		switch (spell)
		{
			case 0:
			{
                                if (info_spell) return;
				(void)detect_monsters_evil();
				break;
			}

			case 1:
			{
                                dam = apply_power_dice(2, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " heal %ldd10", dam);
                                        return;
                                }
                                (void)hp_player(damroll(dam, 10));
                                (void)set_cut(p_ptr->cut - 10 - dam);
				break;
			}

			case 2:
			{
                                dam = apply_power_dur(12, to_s, 2);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dur %ld+d12", dam);
                                        return;
                                }
                                (void)set_blessed(p_ptr->blessed + randint(12) + dam);
				break;
			}

			case 3:
			{
                                if (info_spell) return;
				(void)set_afraid(0);
				break;
			}

			case 4:
			{
                                if (info_spell) return;
                                (void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
				break;
			}

			case 5:
			{
                                if (info_spell) return;
				(void)detect_traps();
				break;
			}

			case 6:
			{
                                if (info_spell) return;
				(void)detect_doors();
				(void)detect_stairs();
				break;
			}

			case 7:
			{
                                if (info_spell) return;
				(void)set_poisoned(p_ptr->poisoned / 2);
				break;
			}

			case 8:
			{
                                if (info_spell) return;
				if (!get_aim_dir(&dir)) return;
				(void)fear_monster(dir, plev);
				break;
			}

			case 9:
			{
                                dam = apply_power_dam(plev * 3, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " range %ld", dam);
                                        return;
                                }
                                teleport_player(dam);
				break;
			}

			case 10:
			{
                                dam = apply_power_dice(4, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " heal %ldd10", dam);
                                        return;
                                }
                                (void)hp_player(damroll(dam, 10));
				(void)set_cut((p_ptr->cut / 2) - 20);
				break;
			}

			case 11:
			{
                                dam = apply_power_dam(24, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dur %ld+d24", dam);
                                        return;
                                }
                                (void)set_blessed(p_ptr->blessed + randint(24) + dam);
				break;
			}

			case 12:
			{
                                if (info_spell) return;
				(void)sleep_monsters_touch();
				break;
			}

			case 13:
			{
                                if (info_spell) return;
				(void)set_food(PY_FOOD_MAX - 1);
				break;
			}

			case 14:
			{
                                if (info_spell) return;
				remove_curse();
				break;
			}

			case 15:
			{
                                dam = apply_power_dur(10, to_s, 2);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dur %ld+d10", dam);
                                        return;
                                }
                                (void)set_oppose_fire(p_ptr->oppose_fire + randint(10) + dam);
                                (void)set_oppose_cold(p_ptr->oppose_cold + randint(10) + dam);
				break;
			}

			case 16:
			{
                                if (info_spell) return;
				(void)set_poisoned(0);
				break;
			}

			case 17:
			{
                                rad = apply_power_dice((plev < 30)?2:3, to_s, 1);
                                dam = apply_power_dam(plev + (plev / 2), to_s, 2);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dam 3d6+%ld", dam);
                                        return;
                                }
				if (!get_aim_dir(&dir)) return;
                                fire_ball(GF_HOLY_FIRE, dir,
                                          damroll(3, 6) + dam,
                                          ((plev < 30) ? 2 : 3));
				break;
			}

			case 18:
			{
                                dam = apply_power_dice(6, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " heal %ldd10", dam);
                                        return;
                                }
                                (void)hp_player(damroll(dam, 10));
				(void)set_cut(0);
				break;
			}

			case 19:
			{
                                dam = apply_power_dam(24, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dur %ld+d24", dam);
                                        return;
                                }
                                (void)set_tim_invis(p_ptr->tim_invis + randint(24) + dam);
				break;
			}

			case 20:
			{
                                dam = apply_power_dam(3 * plev, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dur %ld+d25", dam);
                                        return;
                                }
                                (void)set_protevil(p_ptr->protevil + randint(25) + dam);
				break;
			}

			case 21:
			{
                                if (info_spell) return;
				earthquake(py, px, 10);
				break;
			}

			case 22:
			{
                                if (info_spell) return;
				map_area();
				break;
			}

			case 23:
			{
                                dam = apply_power_dice(8, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " heal %ldd10", dam);
                                        return;
                                }
                                (void)hp_player(damroll(dam, 10));
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

			case 24:
			{
                                if (info_spell) return;
				(void)turn_undead();
				break;
			}

			case 25:
			{
                                dam = apply_power_dam(48, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " heal %ld+d48", dam);
                                        return;
                                }
                                (void)set_blessed(p_ptr->blessed + randint(48) + dam);
				break;
			}

			case 26:
			{
                                dam = apply_power_dam(plev * 3, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dam %ld", dam);
                                        return;
                                }
                                (void)dispel_undead(dam);
				break;
			}

			case 27:
			{
                                dam = apply_power_dam(300, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " heal %ld", dam);
                                        return;
                                }
                                (void)hp_player(dam);
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

			case 28:
			{
                                dam = apply_power_dam(plev * 3, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dam %ld", dam);
                                        return;
                                }
                                (void)dispel_evil(dam);
				break;
			}

			case 29:
			{
                                if (info_spell) return;
				warding_glyph();
				break;
			}

			case 30:
			{
                                dam = apply_power_dam(plev * 4, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dam %ld", dam);
                                        return;
                                }
                                (void)dispel_evil(dam);
                                (void)hp_player(1000);
				(void)set_afraid(0);
				(void)set_poisoned(0);
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

			case 31:
			{
                                if (info_spell) return;
				(void)detect_monsters_normal();
				break;
			}

			case 32:
			{
                                if (info_spell) return;
				(void)detect_all();
				break;
			}

			case 33:
			{
                                if (info_spell) return;
				(void)ident_spell();
				break;
			}

			case 34:
			{
                                if (info_spell) return;
				(void)probing();
				break;
			}

			case 35:
			{
                                if (info_spell) return;
				wiz_lite();
				break;
			}

			case 36:
			{
                                dam = apply_power_dice(4, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " heal %ldd10", dam);
                                        return;
                                }
                                (void)hp_player(damroll(dam, 10));
				(void)set_cut(0);
				break;
			}

			case 37:
			{
                                dam = apply_power_dice(8, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " heal %ldd10", dam);
                                        return;
                                }
                                (void)hp_player(damroll(dam, 10));
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

			case 38:
			{
                                dam = apply_power_dam(2000, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dam %ld", dam);
                                        return;
                                }
                                (void)hp_player(dam);
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}

			case 39:
			{
                                if (info_spell) return;
				(void)do_res_stat(A_STR);
				(void)do_res_stat(A_INT);
				(void)do_res_stat(A_WIS);
				(void)do_res_stat(A_DEX);
				(void)do_res_stat(A_CON);
				(void)do_res_stat(A_CHR);
				break;
			}

			case 40:
			{
                                if (info_spell) return;
				(void)restore_level();
				break;
			}

			case 41:
			{
                                dam = apply_power_dam(plev * 4, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dam %ld", dam);
                                        return;
                                }
                                (void)dispel_undead(dam);
				break;
			}

			case 42:
			{
                                dam = apply_power_dam(plev * 4, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dam %ld", dam);
                                        return;
                                }
                                (void)dispel_evil(dam);
				break;
			}

			case 43:
			{
                                dam = apply_power_dam(100, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " power %ld", dam);
                                        return;
                                }
                                if (banish_evil(dam))
				{
					msg_print("The power of your god banishes evil!");
				}
				break;
			}

			case 44:
			{
                                if (info_spell) return;
				destroy_area(py, px, 15, TRUE);
				break;
			}

			case 45:
			{
                                dam = apply_power_dam(200, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dam %ld", dam);
                                        return;
                                }
				if (!get_aim_dir(&dir)) return;
                                drain_life(dir, dam);
				break;
			}

			case 46:
			{
                                if (info_spell) return;
				(void)destroy_doors_touch();
				break;
			}

			case 47:
			{
                                if (info_spell) return;
				(void)recharge(15);
				break;
			}

			case 48:
			{
                                if (info_spell) return;
				(void)remove_all_curse();
				break;
			}

			case 49:
			{
                                if (info_spell) return;
                                (void)enchant_spell(rand_int(4) + 1, rand_int(4) + 1, 0, 0);
				break;
			}

			case 50:
			{
                                if (info_spell) return;
                                (void)enchant_spell(0, 0, rand_int(3) + 2, 0);
				break;
			}

			case 51:
			{
                                if (info_spell) return;
                                brand_weapon(0);
				break;
			}

			case 52:
			{
                                dam = apply_power_dice(10, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " range %ld", dam);
                                        return;
                                }
                                teleport_player(dam);
				break;
			}

			case 53:
			{
                                dam = apply_power_dam(plev * 8, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " range %ld", dam);
                                        return;
                                }
                                teleport_player(dam);
				break;
			}

			case 54:
			{
                                if (info_spell) return;
				if (!get_aim_dir(&dir)) return;
				(void)teleport_monster(dir);
				break;
			}

			case 55:
			{
                                if (info_spell) return;
                                if (dungeon_flags1 & LF1_NO_TELEPORT)
				{
                                        msg_print("No teleport on special levels...");
					break;
				}
				(void)teleport_player_level();
				break;
			}

			case 56:
			{
                                if (info_spell) return;
                                if (dungeon_flags1 & LF1_NO_TELEPORT)
				{
					msg_print("No recall on special levels...");
					break;
				}

				if (p_ptr->word_recall == 0)
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

			case 57:
			{
                                if (info_spell) return;
				msg_print("The world changes!");
                                if (autosave_l)
                                {
                                    is_autosave = TRUE;
                                    msg_print("Autosaving the game...");
                                    do_cmd_save_game();
                                    is_autosave = FALSE;
                                }
                                /* Leaving */
                                p_ptr->leaving = TRUE;
				break;
			}
	       default:
                 msg_format("You cast an unknown Prayer: %d.", spell);
		 msg_print(NULL);
		}
}

/* incremental sleep spell */
/* -KMW- */
static void do_sleep_monster(void)
{
	int dir;

	if (p_ptr->lev < 15) {
		if (!get_aim_dir(&dir)) return;
		sleep_monster(dir);
	} else if (p_ptr->lev < 30)
		sleep_monsters_touch();
	else
		sleep_monsters();
}

/*
 * Multiple Monster Fear -KMW-
 */
bool fear_monsters(void)
{
	return (project_hack(GF_TURN_ALL, p_ptr->lev));
}

/*
 * Close to Player Monster Fear -KMW-
 */
bool fear_monsters_touch(void)
{
	int flg = PROJECT_KILL | PROJECT_HIDE;
        return (project(0, 1, py, px, p_ptr->lev,
	    GF_TURN_ALL, flg));
}

/* incremental fear spell */
/* -KMW- */
static void do_fear_monster(void)
{
	int dir;

	if (p_ptr->lev < 15) {
		if (!get_aim_dir(&dir)) return;
		fear_monster(dir,p_ptr->lev);
	} else if (p_ptr->lev < 30)
		fear_monsters_touch();
	else
		fear_monsters();
}

/*
* Brand some ammunition.  Used by Cubragol and a mage spell.  The spell was
* moved here from cmd6.c where it used to be for Cubragol only.  I've also
 * expanded it to do either frost, fire or venom, at random. -GJW	-KMW-
 */
void brand_ammo (int brand_type, int bolts_only)
{
	int a;
	int allowable;

	if (bolts_only)
		allowable = TV_BOLT;
	else
		allowable = TV_BOLT | TV_ARROW | TV_SHOT;

	for (a = 0; a < INVEN_PACK; a++)
	{
		object_type *o_ptr = &inventory[a];

		if ((bolts_only) && (o_ptr->tval != TV_BOLT)) continue;
		if ((!bolts_only) && (o_ptr->tval != TV_BOLT) &&
		    (o_ptr->tval != TV_ARROW) && (o_ptr->tval != TV_SHOT))
		    	continue;
		if ((!artifact_p(o_ptr)) && (!ego_item_p(o_ptr)) &&
                   (!cursed_p(o_ptr)))
		   	break;
	}

	/* Enchant the ammo (or fail) */
	if ((a < INVEN_PACK) && (rand_int(100) < 50))
	{
		object_type *o_ptr = &inventory[a];
		char *ammo_name, *aura_name, msg[48];
		int aura_type, r;

		if (brand_type == 1) r = 0;		/* fire only */
                else if (brand_type == 2) r = 99;       /* cold only */
		else r = rand_int (100);

                if (r < 50)
		{
			aura_name = "fiery";
                        aura_type = EGO_FLAME;
		}
                else
		{
			aura_name = "frosty";
                        aura_type = EGO_FROST;
		}

		if (o_ptr->tval == TV_BOLT)
			ammo_name = "bolts";
		else if (o_ptr->tval == TV_ARROW)
			ammo_name = "arrows";
		else
			ammo_name = "shots";

		sprintf (msg, "Your %s are covered in a %s aura!",
			ammo_name, aura_name);
		msg_print (msg);
		o_ptr->name2 = aura_type;

		/* Apply the ego */
		apply_magic(o_ptr, dun_level, FALSE, FALSE, FALSE);
		enchant (o_ptr, rand_int(3) + 4, ENCH_TOHIT | ENCH_TODAM);

		/* You cannot sell them for vast profit any longer */
		o_ptr->discount = 100;
	}
	else
	{
		if (flush_failure) flush();
		msg_print ("The enchantment failed.");
	}
}

/* From Kamband by Ivan Tkatchev */
void summon_monster(int sumtype)
{
        energy_use = 100;

        if (p_ptr->inside_arena) {
		msg_print("This place seems devoid of life.");
		msg_print(NULL);
		return;
	}

        if (summon_specific_friendly(py, px, dun_level +randint(5), sumtype, TRUE))
		msg_print("You summon some help.");
	else
		msg_print("You called, but no help came.");
}

/*
 * Cast an illusionist spell
 */
void cast_illusion_spell(int spell, byte level)
{
        int dir;
        int beam;
	int plev = p_ptr->lev;
        int to_s = level + p_ptr->to_s;
        long dam, rad;

		/* Hack -- chance of "beam" instead of "bolt" */
        if (PRACE_FLAG(PR1_BEAM)) beam = plev + 10;
        else beam = plev / 2;

		/* Spells.  */
		switch (spell)
		{
			case 0:
			{
                                dam = apply_power_dice(4, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dam %dd%ld", 2+((plev-1)/5), dam);
                                        return;
                                }
					/* confusion bolt -KMW- */
	 				if (!get_aim_dir(&dir)) return;
					fire_bolt_or_beam(beam-10, GF_CONFUSION, dir,
                                            damroll(2 + ((plev - 1) / 5), dam));
					break;
			}

			case 1: /* detect monsters */
			{
                                if (info_spell) return;
				(void)detect_monsters_normal();
				break;
			}

			case 2: /* phase door */
			{
                                dam = apply_power_dice(10, to_s, 2);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " range %ld", dam);
                                        return;
                                }
                                teleport_player(dam);
				break;
			}

			case 3: /* light area */
			{
                                if (info_spell) return;
                                (void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
				break;
			}

			case 4: /* treasure detection */
			{
                                if (info_spell) return;
				(void)detect_treasure();
				(void)detect_objects_gold();
				break;
			}

			case 5:
			{
                                if (info_spell) return;
					/* fear -KMW- */
					(void)do_fear_monster();
					break;
			}

			case 6: /* object detection */
			{
                                if (info_spell) return;
				(void)detect_objects_normal();
				break;
			}

			case 7: /* find hidden traps/doors */
			{
                                if (info_spell) return;
				(void)detect_traps();
				(void)detect_doors();
				(void)detect_stairs();
				break;
			}

			case 8: /* stinking cloud */
			{
                                rad = apply_power_dice(2, to_s, 1);
                                dam = apply_power_dam(10 + (plev / 2), to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dam %ld", dam);
                                        return;
                                }
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_POIS, dir,
                                          dam, rad);
				break;
			}

			case 9:
			{
                                dam = apply_power_dur(200, to_s, 5);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dur %ld+d100", dam);
                                        return;
                                }
					/* infravision */
					if (p_ptr->tim_infra == 0)
                                                set_tim_infra(p_ptr->tim_infra + dam + randint(100));
					break;
			}

			case 10:
			{
                                if (info_spell) return;
					/* sleep */
					(void)do_sleep_monster();
					break;
			}

			case 11: /* trap/door destruction */
			{
                                if (info_spell) return;
				(void)destroy_doors_touch();
				break;
			}

			case 12:
			{
                                rad = apply_power_dice(3, to_s, 1);
                                dam = apply_power_dam(10 + (plev), to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dam %ld", dam);
                                        return;
                                }
					/* fog cloud -KMW- */
					if (!get_aim_dir(&dir)) return;
                                        fire_ball(GF_POIS, dir, dam, rad);
					break;
			}

			case 13: /* cure poison */
			{
                                if (info_spell) return;
				(void)set_poisoned(0);
				break;
			}

			case 14:
			{
                                if (info_spell) return;
					/* satisfy hunger */
					(void)set_food(PY_FOOD_MAX - 1);
					break;
			}

			case 15:
			{
                                if (info_spell) return;
					/* shadow door */
					(void)door_creation();
					break;
			}

			case 16:
			{
                                dam = apply_power_dice(8, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dam %dd%ld", 5+((plev-6)/4), dam);
                                        return;
                                }
					/* shadow monster */
					if (!get_aim_dir(&dir)) return;
					fire_bolt(GF_FORCE, dir,
                                            damroll(5+((plev-6)/4), dam));
					break;
			}

			case 17: /* turn stone to mud */
			{
                                if (info_spell) return;
				if (!get_aim_dir(&dir)) return;
				(void)wall_to_mud(dir);
				break;
			}

			case 18:
			{
					/* detect invisible */
                                dam = apply_power_dam(24, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dur %ld+d24", dam);
                                        return;
                                }
                                        (void)set_tim_invis(p_ptr->tim_invis + randint(24) + dam);
					break;
			}

			case 19: /* recharge item */
			{
                                dam = apply_power_dam(plev * 2, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " power %ld", dam);
                                        return;
                                }
                                (void)recharge((plev * 2));
				break;
			}

			case 20: /* brand ammo */
			{
                                if (info_spell) return;
					(void)brand_ammo(1,0);
					break;
			}

			case 21:
			{
                                dam = apply_power_dice(5, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dam %dd%ld", 2+((plev-5)/4), dam);
                                        return;
                                }
					/* spear of light */
					if (!get_aim_dir(&dir)) return;
					msg_print("A line of blue shimmering light appears.");
					fire_beam(GF_LITE, dir,
                                            damroll(2+((plev-5)/4), dam));
					lite_line(dir);
					break;
			}

			case 22:
			{
					/* chaos */
                                rad = apply_power_dice(2, to_s, 1);
                                dam = apply_power_dam(25 + plev, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dam %ld", dam);
                                        return;
                                }
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_CHAOS, dir,
                                            dam, rad);
					break;
			}

			case 23:
			{
					/* mental barrier */
                                dam = apply_power_dam(70, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dur %ld", dam);
                                        return;
                                }
                                        set_mental_barrier(p_ptr->tim_mental_barrier + dam);
					msg_print("Your wisdom and intelligence cannot be changed!");
					break;
			}

			case 24:
			{
                                if (info_spell) return;
					/* true sight */
					map_area();
					break;
			}

			case 25: /* slow monster */
			{
                                if (info_spell) return;
				if (!get_aim_dir(&dir)) return;
				(void)slow_monster(dir);
				break;
			}

			case 26:
			{
                                rad = apply_power_dice(2, to_s, 1);
                                dam = apply_power_dam(35 + plev, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dam %ld", dam);
                                        return;
                                }
					/* shadow ball */
					if (!get_aim_dir(&dir)) return;
                                        fire_ball(GF_DARK, dir, dam, rad);
			}

			case 27:
			{
                                dam = apply_power_dice(7, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dam %dd%ld", 8+((plev-5)/4), dam);
                                        return;
                                }
					/* bolt of darkness */
					if (!get_aim_dir(&dir)) return;
					fire_bolt_or_beam(beam, GF_DARK, dir,
                                            damroll(8+((plev-5)/4), dam));
					break;
			}

			case 28:
			{
					/* shadowform */
                                dam = apply_power_dur(5, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dur %ld+d10", dam);
                                        return;
                                }
                                        (void)set_shadow(p_ptr->tim_wraith + plev + randint(10));
					break;
			}

			case 29: /* haste self */
			{
                                dam = apply_power_dam(plev, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dur %ld+d20", dam);
                                        return;
                                }
				if (!p_ptr->fast)
                                        (void)set_fast(randint(20) + dam);
				else
                                        (void)set_fast(p_ptr->fast + randint(5));
				break;
			}

			case 30:
			{
                                if (info_spell) return;
					/* prismatic wall */
					warding_glyph();
					break;
			}

			case 31:
			{
                                rad = apply_power_dice(2, to_s, 1);
                                dam = apply_power_dam(40 + plev, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dam %ld", dam);
                                        return;
                                }
					/* prismatic spray */
					if (!get_aim_dir(&dir)) return;
                                        fire_ball(GF_LITE, 5, dam, rad);
					fire_beam(GF_LITE, dir,
                                            damroll(8+((plev-5)/4), 8));
					break;
			}

			case 32:
			{
                                dam = apply_power_dam(30, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dur %ld+d30", dam);
                                        return;
                                }
					/* chromatic shield */
                                        (void)set_shield(p_ptr->shield + randint(30) + dam, 50 + to_s, 0);
					break;
			}

			case 33:
			{
                                if (info_spell) return;
					/* wizard lock */
                                        do_cmd_spike();
					break;
			}

			case 34:
			{
                                rad = apply_power_dice(5, to_s, 1);
                                dam = apply_power_dam(50 + plev, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dam %ld", dam);
                                        return;
                                }
					/* bedlam */
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_CHAOS, dir,
                                            dam, rad);
					break;
			}

			case 35:
			{
                                if (info_spell) return;
					/* word of recall */
                                        recall_player();
                                        break;
			}

			case 36:
			{
                                if (info_spell) return;
					/* detect enchantment */
					(void)detect_objects_magic();
					break;
			}

			case 37:
			{
                                if (info_spell) return;
					/* probing */
					(void)probing();
					break;
			}

			case 38:
			{
                                rad = apply_power_dice(10, to_s, 1);
                                dam = apply_power_dam(50 + plev, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dam %ld", dam);
                                        return;
                                }
					/* sunfire */
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_LITE, dir,
                                            dam, rad);
					break;
			}

			case 39: /* the bigbys that are duplicates - warding, slow etc. shoudl act all aroundplayer */
			{
                                if (info_spell) return;
					/* Bigby's Interposing Hand */
					warding_glyph();
					break;
			}

			case 40:
			{
                                dam = apply_power_dice(8, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dam %dd%ld", 2+((plev-5)/4), dam);
                                        return;
                                }
					/* Bigby's Phantom Hand */
					if (!get_aim_dir(&dir)) return;
					fire_bolt_or_beam(beam, GF_CONFUSION, dir,
                                            damroll(2+((plev-5)/4), dam));
					break;
			}

			case 41:
			{
                                dam = apply_power_dice(8, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dam %dd%ld", 6+((plev-5)/4), dam);
                                        return;
                                }
					/* Bigby's Forceful Hand */
					if (!get_aim_dir(&dir)) return;
					fire_bolt_or_beam(beam, GF_FORCE, dir,
                                            damroll(6+((plev-5)/4), dam));
					break;
			}

			case 42:
			{
                                if (info_spell) return;
					/* Bigby's Grasping Hand */
					if (!get_aim_dir(&dir)) return;
					(void)slow_monster(dir);
					break;
			}

			case 43:
			{
                                dam = apply_power_dice(8, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dam %dd%ld", 10+((plev-5)/4), dam);
                                        return;
                                }
					/* Bigby's Clenched Fist */
					if (!get_aim_dir(&dir)) return;
					fire_beam(GF_FORCE, dir,
                                            damroll(10+((plev-5)/4), dam));
					break;
			}

			case 44:
			{
                                dam = apply_power_dice(8, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dam %dd%ld", 12+((plev-5)/4), dam);
                                        return;
                                }
					/* Bigby's Crushing Hand */
					/* want to have this last two turns */
					if (!get_aim_dir(&dir)) return;
					fire_bolt(GF_GRAVITY, dir,
                                            damroll(12+((plev-5)/4), dam));
					break;
			}

			case 45:
			{
                                rad = apply_power_dice(3, to_s, 1);
                                dam = apply_power_dam(30 + (plev * 2), to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dam %ld", dam);
                                        return;
                                }
					/* force blast */
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_FORCE, 5,
                                            dam, rad);
					break;
			}

			case 46:
			{
                                rad = apply_power_dice(2, to_s, 1);
                                dam = apply_power_dam(30 + (plev * 6), to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dam %ld", dam);
                                        return;
                                }
					/* Sphere of Light */
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_LITE, dir,
                                            dam, rad);
					break;
			}

			case 47:
			{
                                rad = apply_power_dice(2, to_s, 1);
                                dam = apply_power_dam(35 + (plev * 6), to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dam %ld", dam);
                                        return;
                                }
					/* sphere of darkness */
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_DARK, dir,
                                            dam, rad);
					break;
			}

			case 48:
			{
                                rad = apply_power_dice(2, to_s, 1);
                                dam = apply_power_dam(40 + (plev * 6), to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dam %ld", dam);
                                        return;
                                }
					/* sphere of confusion */
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_CONFUSION, dir,
                                            dam, rad);
					break;
			}

			case 49:
			{
                                rad = apply_power_dice(2, to_s, 1);
                                dam = apply_power_dam(45 + (plev * 6), to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dam %ld", dam);
                                        return;
                                }
					/* sphere of chaos */
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_CHAOS, dir,
                                            dam, rad);
					break;
			}

			case 50:
			{
                                rad = apply_power_dice(2, to_s, 1);
                                dam = apply_power_dam(50 + (plev * 6), to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dam %ld", dam);
                                        return;
                                }
					/* sphere of sound */
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_SOUND, dir,
                                            dam, rad);
					break;
			}

			case 51:
			{
                                rad = apply_power_dice(2, to_s, 1);
                                dam = apply_power_dam(80 + (plev * 6), to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dam %ld", dam);
                                        return;
                                }
					/* explosion */
					if (!get_aim_dir(&dir)) return;
                                        fire_ball(GF_SHARDS, dir,
                                            dam, rad);
					break;
			}

			case 52:
			{
                                if (info_spell) return;
					/* remove fear */
					(void)set_afraid(0);
					break;
			}

			case 53:
			{
                                dam = apply_power_dur(20, to_s, 4);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dur %ld+d20", dam);
                                        return;
                                }
					/* resist light & dark */
                                        (void)set_oppose_ld(p_ptr->oppose_ld + randint(20) + dam);
					break;
			}

			case 54:
			{
                                dam = apply_power_dur(20, to_s, 4);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dur %ld+d20", dam);
                                        return;
                                }
					/* resist poison */
                                        (void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + dam);
					break;
			}

			case 55:
			{
                                dam = apply_power_dur(20, to_s, 4);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dur %ld+d20", dam);
                                        return;
                                }
					/* resist chaos & confusion */
                                        (void)set_oppose_cc(p_ptr->oppose_cc + randint(20) + dam);
					break;
			}

			case 56:
			{
                                dam = apply_power_dur(20, to_s, 4);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dur %ld+d20", dam);
                                        return;
                                }
					/* resist sound & shards */
                                        (void)set_oppose_ss(p_ptr->oppose_ss + randint(20) + dam);
					break;
			}

			case 57:
			{
                                dam = apply_power_dur(20, to_s, 4);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dur %ld+d20", dam);
                                        return;
                                }
					/* resist nexus */
                                        (void)set_oppose_nex(p_ptr->oppose_nex + randint(20) + dam);
					break;
			}

			case 58:
			{
                                dam = apply_power_dur(24, to_s, 4);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dur %ld+d24", dam);
                                        return;
                                }
					/* Invisibility */
                                        (void)set_invis(p_ptr->tim_invis + randint(24) + dam, 30);
					break;
			}

			case 59:
			{
                                rad = apply_power_dice(10, to_s, 1);
                                dam = apply_power_dam(30 + (plev * 3), to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dam %ld", dam);
                                        return;
                                }
					/* shadow monsters */
					if (!get_aim_dir(&dir)) return;
                                        fire_ball(GF_DARK, 5, dam, rad);
					fire_beam(GF_FORCE, dir,
                                            damroll(6+((plev-5)/4), 8));
					break;
			}

			case 60:
			{
                                rad = apply_power_dice(3, to_s, 1);
                                dam = apply_power_dam(40 + (plev * 5), to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dam %ld", dam);
                                        return;
                                }
					/* shadow ball */
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_DARK, dir,
                                            dam, rad);
					break;
			}

			case 61:
			{
				/* shadow sacrifices */
				int m = 0;

                                if (info_spell) return;

				m = get_quantity("How much mana?",999);
				if (!m) return;
				if (m<0) return;
				if (m > p_ptr->chp) {
					msg_print("You don't have that much life!");
					return;
				}
				if ((p_ptr->csp + m) > p_ptr->msp)
					m = p_ptr->msp - p_ptr->csp;
				p_ptr->csp += m;
				take_hit(m,"spellcasting");
				msg_print("You convert life into mana.");
				break;
			}

			case 62:
			{
                                dam = apply_power_dam(plev * 7, to_s, 1);
                                if (info_spell)
                                {
                                        sprintf(spell_txt, " dam %ld", dam);
                                        return;
                                }
					/* shadow gate */
                                        teleport_player(dam);
					break;
			}

			case 63:
			{
                                if (info_spell) return;
					/* summon shadows */
					summon_monster(SUMMON_SHADOWS);
					break;
			}

		}
}

void do_cmd_possessor()
{
        char ch, ext;

        /* No magic */
        if (p_ptr->antimagic)
        {
                msg_print("Your anti-magic field disrupts any magic attempts.");
                return;
        }

        while (TRUE)
        {
                if (!get_com("Use your [R]ace powers or your [I]ncarnating powers ?", &ch))
                {
                        ext = 0;
                        break;
                }
                if (ch == 'R' || ch == 'r')
                {
                        ext = 1;
                        break;
                }
                if (ch == 'I' || ch == 'i')
                {
                        ext = 2;
                        break;
                }
        }
        if(ext == 1)
        {
                if (p_ptr->disembodied)
                {
			msg_print("You don't currently own a body to use.");
                        return;
                }

                use_symbiotic_power(p_ptr->body_monster, TRUE, FALSE, FALSE);

                if (p_ptr->csp < 0)
                {
                        msg_print("You lose control of your body!");
                        if (!do_cmd_leave_body(FALSE))
                        {
                                cmsg_print(TERM_VIOLET, "You are forced back into your body by your cursed items, you suffer a system shock!");

                                p_ptr->chp = 1;

                                /* Display the hitpoints */
                                p_ptr->redraw |= (PR_HP);
                        }
                }
        }

        else if(ext == 2)
	{
                if(p_ptr->disembodied)
                        do_cmd_integrate_body();
                else
                        do_cmd_leave_body(TRUE);
	}

	/* Take a turn */
	energy_use = 100;
}

/*
 * Hook to determine if an object is contertible in an arrow/bolt
 */
static bool item_tester_hook_convertible(object_type *o_ptr)
{
        if((o_ptr->tval==TV_JUNK) || (o_ptr->tval==TV_SKELETON)) return TRUE;

	/* Assume not */
	return (FALSE);
}


/*
 * do_cmd_cast calls this function if the player's class
 * is 'archer'.
 */
void do_cmd_archer(void)
{
        int ext=0;
        char ch;

	object_type	forge;
        object_type     *q_ptr;

        char com[80];

        q_ptr = &forge;

        if(p_ptr->lev >= 20)
                sprintf(com, "Create [S]hots, Create [A]rrow or Create [B]olt ?");
        else if(p_ptr->lev >= 10)
                sprintf(com, "Create [S]hots or Create [A]rrow ?");
        else
                sprintf(com, "Create [S]hots ?");

	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}

        if (p_ptr->blind)
	{
                msg_print("You are blind!");
		return;
	}

        while (TRUE)
        {
                if (!get_com(com, &ch))
                {
                        ext = 0;
                        break;
                }
                if (ch == 'S' || ch == 's')
                {
                        ext = 1;
                        break;
                }
                if ((ch == 'A' || ch == 'a')&&(p_ptr->lev >= 10))
                {
                        ext = 2;
                        break;
                }
                if ((ch == 'B' || ch == 'b')&&(p_ptr->lev >= 20))
                {
                        ext = 3;
                        break;
                }
        }

        /**********Create shots*********/
        if (ext == 1)
        {
                int x,y, dir;
                cave_type *c_ptr;

                if (!get_rep_dir(&dir)) return;
                y = py + ddy[dir];
                x = px + ddx[dir];
                c_ptr = &cave[y][x];
                if (c_ptr->feat == FEAT_RUBBLE)
                {
                        /* Get local object */
                        q_ptr = &forge;

                        /* Hack -- Give the player some small firestones */
                        object_prep(q_ptr, lookup_kind(TV_SHOT, m_bonus(2, dun_level)));
                        q_ptr->number = (byte)rand_range(15,30);
                        object_aware(q_ptr);
                        object_known(q_ptr);
                        apply_magic(q_ptr, dun_level, TRUE, TRUE, (magik(20))?TRUE:FALSE);

                        (void)inven_carry(q_ptr, FALSE);

                        msg_print("You make some ammo.");

                        (void)wall_to_mud(dir);
                        p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW);
                        p_ptr->window |= (PW_OVERHEAD);
                }
        }
        /**********Create arrows*********/
        else if (ext == 2)
        {
                int item;

                cptr q, s;

                item_tester_hook = item_tester_hook_convertible;

                /* Get an item */
                q = "Convert which item? ";
                s = "You have no item to convert.";
                if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

                /* Get the item (in the pack) */
                if (item >= 0)
                {
                        q_ptr = &inventory[item];
                }

                /* Get the item (on the floor) */
                else
                {
                        q_ptr = &o_list[0 - item];
                }

                /* Get local object */
                q_ptr = &forge;

                /* Hack -- Give the player some small firestones */
                object_prep(q_ptr, lookup_kind(TV_ARROW, m_bonus(1, dun_level) + 1));
                q_ptr->number = (byte)rand_range(15,25);
                object_aware(q_ptr);
                object_known(q_ptr);
                apply_magic(q_ptr, dun_level, TRUE, TRUE, (magik(20))?TRUE:FALSE);

                msg_print("You make some ammo.");

                if (item >= 0)
                {
                        inven_item_increase(item, -1);
                        inven_item_describe(item);
                        inven_item_optimize(item);
                }
                else
                {
                        floor_item_increase(0 - item, -1);
                        floor_item_describe(0 - item);
                        floor_item_optimize(0 - item);
                }

                (void)inven_carry(q_ptr, FALSE);
        }
        /**********Create bolts*********/
        else if (ext == 3)
        {
                int item;

                cptr q, s;

                item_tester_hook = item_tester_hook_convertible;

                /* Get an item */
                q = "Convert which item? ";
                s = "You have no item to convert.";
                if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

                /* Get the item (in the pack) */
                if (item >= 0)
                {
                        q_ptr = &inventory[item];
                }

                /* Get the item (on the floor) */
                else
                {
                        q_ptr = &o_list[0 - item];
                }

                /* Get local object */
                q_ptr = &forge;

                /* Hack -- Give the player some small firestones */
                object_prep(q_ptr, lookup_kind(TV_BOLT, m_bonus(1, dun_level) + 1));
                q_ptr->number = (byte)rand_range(15,25);
                object_aware(q_ptr);
                object_known(q_ptr);
                apply_magic(q_ptr, dun_level, TRUE, TRUE, (magik(20))?TRUE:FALSE);

                msg_print("You make some ammo.");

                if (item >= 0)
                {
                        inven_item_increase(item, -1);
                        inven_item_describe(item);
                        inven_item_optimize(item);
                }
                else
                {
                        floor_item_increase(0 - item, -1);
                        floor_item_describe(0 - item);
                        floor_item_optimize(0 - item);
                }

                (void)inven_carry(q_ptr, FALSE);
        }
}

/*
 * Helper function to describe necro powers
 */
void necro_info(char *p, int power)
{
    int plev = p_ptr->lev;
    int mto_s2=p_ptr->to_s/2;

    mto_s2 = (mto_s2==0)?1:mto_s2;

    strcpy(p, "");

    switch (power) {
        case 0:
                        if (p_ptr->lev > 45)
                        {
                                sprintf(p, " dam %d", (50 + plev) * mto_s2);
                        }
                        else if (p_ptr->lev > 35)
                        {
                                sprintf(p, " dam %d", (120 + plev) * mto_s2);
                        }
                        else if (p_ptr->lev > 20)
                        {
                                sprintf(p, " dam %dd%d", 2 + (plev / 5), 8 * mto_s2);
                        }
                        else
                        {
                                sprintf(p, " dam %dd%d", 2 + (plev / 5), 4 * mto_s2);
                        }
                break;
    }
}

/*
 * Cast a Necromancy spell
 */
void do_cmd_necromancer(void)
{
	int             n = 0,  b = 0;
	int             chance;
	int             dir;
	int             minfail = 0;
	int             plev = p_ptr->lev;
        magic_power     spell;
        int to_s2=p_ptr->to_s/2;
        int mto_s2=p_ptr->to_s/2;

        mto_s2 = (mto_s2==0)?1:mto_s2;

        /* No magic */
        if (p_ptr->antimagic)
        {
                msg_print("Your anti-magic field disrupts any magic attempts.");
                return;
        }

	/* not if confused */
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}

	/* get power */
        if (!get_magic_power(&n, necro_powers, MAX_NECRO_POWERS, necro_info))  return;

        spell = necro_powers[n];

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
	chance -= 3 * (adj_mag_stat[p_ptr->stat_ind[cp_ptr->spell_stat]] - 1);

	/* Not enough mana to cast */
	if (spell.mana_cost > p_ptr->csp)
	{
		chance += 5 * (spell.mana_cost - p_ptr->csp);
	}

	/* Extract the minimum failure rate */
	minfail = adj_mag_fail[p_ptr->stat_ind[cp_ptr->spell_stat]];

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
                        if (b < 10)
			{
                                msg_print("Oh, no! You become an undead !");

                                p_ptr->class_extra3 |= CLASS_UNDEAD;
                                p_ptr->class_extra4 = 2 * p_ptr->lev;
                                msg_format("You have to kill %d monster%s to be brought back to life.", p_ptr->class_extra4, (p_ptr->class_extra4 == 1)?"":"s");

                                /* MEGA-HACK !!! */
                                calc_hitpoints();

                                /* Enforce maximum */
                                p_ptr->chp = p_ptr->mhp;
                                p_ptr->chp_frac = 0;

                                /* Display the hitpoints */
                                p_ptr->redraw |= (PR_HP);

                                /* Window stuff */
                                p_ptr->window |= (PW_PLAYER);
			}
                        else if (b < 40)
			{
                                msg_print("Suddenly you feel that you're in bad situation...");
                                summon_specific(py, px, max_dlv[dungeon_type], (p_ptr->lev >= 30)?SUMMON_HI_UNDEAD:SUMMON_UNDEAD);
			}
			else
			{
                                msg_print("Your body is damaged by the horrible forces of the spell!");
                                take_hit(damroll(10, p_ptr->lev), "using necromancy unwisely");
			}
		}
	}
	else
	{
		sound(SOUND_ZAP);

		/* spell code */
		switch (n)
		{
                case 0:   /* Call Darkness */
                        if (p_ptr->lev > 45)
                        {
                                project_hack(GF_DARK, (50 + plev) * mto_s2);
                        }
                        else if (p_ptr->lev > 35)
                        {
                                if (!get_aim_dir(&dir)) return;
                                fire_ball(GF_DARK, dir, (120 + plev) * mto_s2, 2 + (plev / 10));
                        }
                        else if (p_ptr->lev > 20)
                        {
                                if (!get_aim_dir(&dir)) return;
                                fire_beam(GF_DARK, dir, damroll(2 + (plev / 5), 8 * mto_s2));
                        }
                        else
                        {
                                if (!get_aim_dir(&dir)) return;
                                fire_bolt(GF_DARK, dir, damroll(2 + (plev / 5), 4 * mto_s2));
                        }
                        break;
                case 1:   /* Raise Death */
                        fire_ball(GF_RAISE, 0, 1, 1 + to_s2 + (plev / 10));
                        break;
                case 2:   /* Summon Undeads */
                {
                        int i;
                        bool ok = FALSE;

                        for(i = 0; i < 1 + (plev / 10) + to_s2; i++)
                        {
                                if (summon_specific_friendly(py, px, dun_level + randint(5), (plev > 38)?SUMMON_HI_UNDEAD:SUMMON_UNDEAD, FALSE))
                                        ok = TRUE;
                        }

                        if(ok)
                                msg_print("You summon some help.");
                        else
                                msg_print("You called, but no help came.");

                        break;
                }
                case 3:   /* Vampirism */
                {
                        int i;
                        if (!get_aim_dir(&dir)) return;
                        for (i = 0; i < 1 + to_s2 + (plev / 15); i++)
                        {
                                if (drain_life(dir, 100))
                                        hp_player(100);
                        }
                        break;
                }
                case 4:   /* Death */
                        if(get_check("Using the Death word will leaves you undead, with 1 DP, do you REALY want to use it ?"))
                        {
                                if (!get_aim_dir(&dir)) return;
                                fire_bolt(GF_DEATH, dir, 1);

                                p_ptr->class_extra3 |= CLASS_UNDEAD;
                                p_ptr->class_extra4 = p_ptr->lev + (rand_int(p_ptr->lev / 2) - (p_ptr->lev / 4));
                                msg_format("You have to kill %d monster%s to be brought back to life.", p_ptr->class_extra4, (p_ptr->class_extra4 == 1)?"":"s");

                                /* MEGA-HACK !!! */
                                calc_hitpoints();

                                /* Enforce 1 DP */
                                p_ptr->chp = p_ptr->mhp;
                                p_ptr->chp_frac = 0;

                                /* Display the hitpoints */
                                p_ptr->redraw |= (PR_HP);

                                /* Window stuff */
                                p_ptr->window |= (PW_PLAYER);
                        }
                        break;

		default:
			msg_print("Zap?");
		}
	}

	/* Take a turn */
        if (is_magestaff()) energy_use = 80;
        else energy_use = 100;

	/* Sufficient mana */
	if (spell.mana_cost <= p_ptr->csp)
	{
		/* Use some mana */
		p_ptr->csp -= spell.mana_cost;
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

                /* Damage CON (possibly permanently) */
		if (rand_int(100) < 50)
		{
			bool perm = (rand_int(100) < 25);

			/* Message */
                        msg_print("You have damaged your body!");

			/* Reduce constitution */
                        (void)dec_stat(A_CON, 15 + randint(10), perm);
		}
	}

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);
	p_ptr->window |= (PW_SPELL);
}

/*
 * Cast a daemon spell -SC-
 */
void cast_daemon_spell(int spell, byte level)
{
        int dir;
        int beam;
	int plev = p_ptr->lev;
        int to_s = level + p_ptr->to_s;
        long dam, rad;

	/* Hack -- chance of "beam" instead of "bolt" */
        if (PRACE_FLAG(PR1_BEAM)) beam = plev + 10;
        else beam = plev / 2;

	/* Spells.  */
	switch (spell)
	{
		case 0: /* Detect Good */
                        if (info_spell) return;
			(void)detect_monsters_good();
			break;
		case 1: /* Phase Door */
                        dam = apply_power_dice(10, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " range %ld", dam);
                                return;
                        }
                        teleport_player(dam);
			break;
		case 2: /* Resist Fire */
                        dam = apply_power_dur(10, to_s, 2);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d10", dam);
                                return;
                        }
                        set_oppose_fire(p_ptr->oppose_fire + dam + rand_int(10));
			break;
		case 3: /* Unearthly Blessing */
		{
			int dur;

                        dam = apply_power_dice(8, to_s, 4);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d8", dam);
                                return;
                        }

                        dur = randint(8) + dam;
			set_blessed(p_ptr->blessed + dur);
                        set_shield(p_ptr->shield + dur, 10 + to_s, 0);
			set_afraid(0);
			break;
		}
		case 4: /* Steal Thoughts */
                        rad = apply_power_dice(3 + (plev / 10), to_s, 1);
                        dam = apply_power_dice(2 + (plev / 5), to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ldd5", dam);
                                return;
                        }
			if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_CONFUSION, dir, damroll(dam, 5), rad);
			break;
		case 5: /* Demon Eyes */
		{
                        int dur = randint(14);

                        dam = apply_power_dur(14, to_s, 3);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d14", dam);
                                return;
                        }
                        dur += dam;

			set_tim_invis(p_ptr->tim_invis + dur);
			set_tim_esp(p_ptr->tim_esp + dur);
			break;
		}
		case 6: /* Mend Flesh */
                        dam = apply_power_dice(4, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " heal %ldd10", dam);
                                return;
                        }
                        hp_player(damroll(dam, 10));
			set_cut(0);
			break;
		case 7: /* Vision */
                        if (info_spell) return;
			map_area();
			break;
		case 8: /* Detect Angels and Demons */
                        if (info_spell) return;
			detect_monsters_xxx(RF3_DEMON);
			detect_monsters_xxx(RF3_GOOD);
			break;
		case 9: /* Protection from Good */
                        dam = apply_power_dur(5 + plev, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d20", dam);
                                return;
                        }
                        set_protgood(p_ptr->protgood + randint(20) + dam);
			break;
		case 10: /* Invisibility */
                        dam = apply_power_dur(5, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d25", dam);
                                return;
                        }
                        set_invis(p_ptr->tim_invisible + randint(25) + dam, 25);
			break;
		case 11: /* Manes Summoning */
		{
			int xx = px, yy = py, i;

                        if (info_spell) return;

			for (i=0; i<plev/5; i++)
			{
				scatter(&yy, &xx, py, px, 5, 0);
				place_monster_aux(yy, xx, test_monster_name("Manes"), FALSE, FALSE, TRUE);
			}
			break;
		}
		case 12: /* Demoncloak */
		{
                        int dur = randint(20);

                        dam = apply_power_dur(20, to_s, 7);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d20", dam);
                                return;
                        }
                        dur += dam;

			set_oppose_fire(p_ptr->oppose_fire + dur);
			set_oppose_cold(p_ptr->oppose_cold + dur);
                        set_shield(p_ptr->shield + dur, 40 + to_s, 0);
			set_lite(p_ptr->lite + dur);
			set_afraid(0);
			break;
		}
		case 13: /* Breath Fire */
                        rad = apply_power_dice(3, to_s, 1);
                        dam = apply_power_dam(40 + (plev / 6), to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
			if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_FIRE, dir, dam, rad);
			break;
		case 14: /* Fire Blade */
		{
                        int dur=randint(25);

                        dam = apply_power_dam(15, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d25", dam);
                                return;
                        }
                        dur += dam;

			set_tim_fire_aura(p_ptr->tim_fire_aura + dur);
			set_fast(dur);
			set_blessed(p_ptr->blessed + dur);
			set_hero(p_ptr->hero + dur);
			break;
		}
		case 15: /* Circle of Madness */
		{
                        rad = apply_power_dice(2, to_s, 1);
                        dam = apply_power_dam(20 + plev, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam 3*%ld", dam);
                                return;
                        }

			fire_ball(GF_CHAOS, 0, dam, rad);
			fire_ball(GF_CONF_DAM, 0, dam, rad);
			fire_ball(GF_CHARM, 0, dam, rad);
			break;
		}
		case 16: /* Bladecalm */
                        if (info_spell) return;
			break;
		case 17: /* Control Demons */
                        dam = apply_power_dam(100, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " power %ld", dam);
                                return;
                        }
                        charm_demons(dam);
			break;
		case 18: /* Revive */
                        if (info_spell) return;
                        fire_ball(GF_RAISE_DEMON, 0, 1, 2 + to_s + (plev / 5));
			break;
		case 19: /* Trap Demonsoul */
                        dam = apply_power_dice(8, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " power %dd%ld", 20+((plev-5)/4), dam);
                                return;
                        }
			if (!get_aim_dir(&dir)) return;
			fire_bolt_or_beam(beam, GF_TRAP_DEMONSOUL, dir,
                                          damroll(20+((plev-5)/4), dam));
			break;
		case 20: /* Discharge Minions */
		{
			int i, y, x;

                        if (info_spell) return;

			/* Affect all pets */
			for (i = 1; i < m_max; i++)
			{
				monster_type *m_ptr = &m_list[i];
				bool tmp;
				int dam;

				/* Skip dead monsters and non-pets */
                                if ((m_ptr->status != MSTATUS_PET)||!m_ptr->r_idx) continue;

				/* Location */
				y = m_ptr->fy;
				x = m_ptr->fx;

				dam = m_ptr->hp / 2;

				/* Kill the monster */
                                mon_take_hit(i, m_ptr->hp + 1, &tmp, " explodes.");

				/* Create the explosion */
                                project(0, 2+(m_ptr->level/20), y,
					x, dam, GF_PLASMA,
					PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL);
			}
			break;
		}
		case 21: /* Summon Demons */
                        if (info_spell) return;
                        summon_specific_friendly(py, px, (plev*3)/2, SUMMON_DEMON, FALSE);
			break;
		case 22: /* Rain of Lava */
                        rad = apply_power_dice(2, to_s, 1);
                        dam = apply_power_dam(40 + plev, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
                        fire_ball(GF_FIRE, 0, dam, rad);
                        fire_ball(GF_LAVA_FLOW, 0, 5 + (plev / 5) + to_s, rad);
			break;
		case 23: /* Kiss of the Succubus */
                        rad = apply_power_dice(3, to_s, 1);
                        dam = apply_power_dam(150 + plev, to_s, 3);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
			if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_NEXUS, dir, dam, rad);
			take_hit(50, "The Kiss of the Succubus");
			break;
		case 24: /* Immortality */
                        dam = apply_power_dam(30, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d30", dam);
                                return;
                        }
                        set_tim_res_time(p_ptr->tim_res_time + dam + randint(30));
			break;
		case 25: /* Glyph of Warding */
                        if (info_spell) return;
			warding_glyph();
			break;
		case 26: /* Lava Storm */
                        rad = apply_power_dice(4, to_s, 1);
                        dam = apply_power_dam(60 + plev, to_s, 6);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
			if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_FIRE, dir, dam, rad);
                        fire_ball(GF_LAVA_FLOW, 0, 10 + (plev / 2) + to_s, rad);
			break;
		case 27: /* Demonform */
                        dam = apply_power_dur(15, to_s, 3);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d15", dam);
                                return;
                        }
                        set_mimic(p_ptr->tim_mimic + dam + randint(15), MIMIC_DEMON_LORD);
			break;
		case 28: /* Unholy word */
                        rad = apply_power_dam(plev * 4, to_s, 1);
                        dam = apply_power_dam(1000, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " heal %ld", dam);
                                return;
                        }
                        dispel_living(rad);
                        hp_player(dam);
			set_afraid(0);
			set_poisoned(0);
			set_stun(0);
			set_cut(0);
			break;
		case 29: /* Hellfire */
                        rad = apply_power_dice(3, to_s, 1);
                        dam = apply_power_dam(250, to_s, 1);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dam %ld", dam);
                                return;
                        }
			if (!get_aim_dir(&dir)) return;
                        fire_ball(GF_HELL_FIRE, dir, dam, rad);
                        fire_ball(GF_HELL_FIRE, 10-dir, dam, rad);
			break;
		case 30: /* Armageddon */
                        if (info_spell) return;
			mass_genocide(TRUE);
			break;
		case 31: /* Shield of the Damned */
                        dam = apply_power_dur(10, to_s, 5);
                        if (info_spell)
                        {
                                sprintf(spell_txt, " dur %ld+d10", dam);
                                return;
                        }
                        set_invuln(p_ptr->invuln + randint(10) + dam);
			break;
		default:
			msg_format("You cast an unknown Daemon spell: %d.", spell);
			msg_print(NULL);
			break;
	}
}

/* Runecrafters */
static s32b rune_combine = 0;

/*
 * Hook to determine if an object is "runestone"
 */
static bool item_tester_hook_runestone(object_type *o_ptr)
{
        if (o_ptr->tval != TV_RUNE2) return FALSE;

        if (o_ptr->sval != RUNE_STONE) return FALSE;

        if (o_ptr->pval != 0) return FALSE;

        /* Assume yes */
        return (TRUE);
}
static bool item_tester_hook_runestone_full(object_type *o_ptr)
{
        if (o_ptr->tval != TV_RUNE2) return FALSE;

        if (o_ptr->sval != RUNE_STONE) return FALSE;

        if (o_ptr->pval == 0) return FALSE;

        /* Assume yes */
        return (TRUE);
}
/*
 * Hook to determine if an object is "rune-able"
 */
static bool item_tester_hook_runeable1(object_type *o_ptr)
{
        if(o_ptr->tval != TV_RUNE1) return FALSE;

        /* Assume yes */
        return (TRUE);
}
/*
 * Hook to determine if an object is "rune-able"
 */
static bool item_tester_hook_runeable2(object_type *o_ptr)
{
        if (o_ptr->tval != TV_RUNE2) return FALSE;

        if (o_ptr->sval == RUNE_STONE) return FALSE;

        if (rune_combine & (1<<o_ptr->sval)) return (FALSE);

        /* Assume yes */
        return (TRUE);
}

/* math.h(sqrt) is banned of angband so ... :) */
s32b sroot(s32b n)
{
        s32b i = n / 2;

        if (n < 2) return (n);

        while (1)
        {
                s32b err = (i - n / (i + 1)) / 2;

                if (!err) break;

                i -= err;
        }

        return ((n / i < i) ? (i - 1) : i);
}

/*
 * Damage formula, for runes
 */
void rune_calc_power(s32b *power, s32b *powerdiv)
{
        /* Not too weak power(parano‹a) */
        *power = (*power < 1)?1:*power;
        *power += 3;

        *power = 37 * sroot(*power) / 10;

        /* To reduce the high level power, while increasing the low levels */
        *powerdiv = *power / 3;
        if (*powerdiv < 1) *powerdiv = 1;

        /* Use the spell multiplicator */
        *power *= (p_ptr->to_s / 2)?(p_ptr->to_s / 2):1;
}

/*
 * Return percentage chance of runespell failure.
 */
int spell_chance_rune(rune_spell* spell)
{
        int chance, minfail;
        s32b power = spell->mana, power_rune = 0, powerdiv = 0;

        if(spell->rune2 & RUNE_POWER_SURGE)
                power_rune += 4;
        if(spell->rune2 & RUNE_ARMAGEDDON)
                power_rune += 3;
        if(spell->rune2 & RUNE_SPHERE)
                power_rune += 2;
        if(spell->rune2 & RUNE_RAY)
                power_rune += 1;

        rune_calc_power(&power, &powerdiv);

        chance = (5 * power_rune) + (power);

        /* Reduce failure rate by INT/WIS adjustment */
        chance -= 3 * (adj_mag_stat[p_ptr->stat_ind[cp_ptr->spell_stat]] - 1);

        /* Extract the minimum failure rate */
        minfail = adj_mag_fail[p_ptr->stat_ind[cp_ptr->spell_stat]];

        /* Minimum failure rate */
        if (chance < minfail) chance = minfail;

        /* Stunning makes spells harder */
        if (p_ptr->stun > 50) chance += 25;
        else if (p_ptr->stun) chance += 15;

        /* Always a 5 percent chance of working */
        if (chance > 95) chance = 95;

        /* Return the chance */
        return (chance);
}

/*
 * Combine the Runes
 */
int rune_exec(rune_spell *spell, int cost)
{
        int dir, power_rune = 0, mana_used;
        int chance;
        s32b power, powerdiv;

        int rad = 0, ty = -1, tx = -1, dam = 0, flg = 0;

        if(spell->rune2 & RUNE_POWER_SURGE)
                power_rune += 4;
        if(spell->rune2 & RUNE_ARMAGEDDON)
                power_rune += 3;
        if(spell->rune2 & RUNE_SPHERE)
                power_rune += 2;
        if(spell->rune2 & RUNE_RAY)
                power_rune += 1;

        power = spell->mana;

        if ((cost) && (power * cost / 100) > p_ptr->csp - (power_rune * (p_ptr->lev / 5)))
        {
                power = p_ptr->csp - (power_rune * (p_ptr->lev / 5));
                mana_used = power + (power_rune * (p_ptr->lev / 5));
        }
        else
        {
                mana_used = (power * cost / 100) + (power_rune * (p_ptr->lev / 5));
        }

        rune_calc_power(&power, &powerdiv);

        dam = damroll(powerdiv, power);

        if (wizard) msg_format("Rune %dd%d = dam %d", powerdiv, power, dam);

        /* Extract the base spell failure rate */
        chance = spell_chance_rune(spell);

        /* Failure ? */
	if (rand_int(100) < chance)
	{
                char sfail[80];

		if (flush_failure) flush();

                get_rnd_line("sfail.txt",sfail);

                msg_format("A cloud of %s appears above you.", sfail);
		sound(SOUND_FAIL);

                if (is_magestaff()) energy_use = 80;
                else energy_use = 100;

                /* Window stuff */
                p_ptr->window |= (PW_PLAYER);
                p_ptr->redraw |= (PR_MANA);
                return mana_used;
	}

        if(spell->rune2 & RUNE_POWER_SURGE)
        {
                flg |= PROJECT_VIEWABLE;
                ty = py;
                tx = px;
        }
        if(spell->rune2 & RUNE_ARMAGEDDON)
        {
                flg |= PROJECT_THRU;
                flg |= PROJECT_KILL;
                flg |= PROJECT_ITEM;
                flg |= PROJECT_GRID;
                flg |= PROJECT_METEOR_SHOWER;
                rad = (power / 8 == 0)?1:power / 8;
                rad = (rad > 10)?10:rad;
                ty = py;
                tx = px;
        }
        if(spell->rune2 & RUNE_SPHERE)
        {
                flg |= PROJECT_THRU;
                flg |= PROJECT_KILL;
                flg |= PROJECT_ITEM;
                flg |= PROJECT_GRID;
                rad = (power / 8 == 0)?1:power / 8;
                rad = (rad > 10)?10:rad;
                ty = py;
                tx = px;
        }
        if(spell->rune2 & RUNE_RAY)
        {
                flg |= PROJECT_THRU;
                flg |= PROJECT_KILL;
                flg |= PROJECT_BEAM;
                ty = -1;
                tx = -1;
        }
        if(spell->rune2 & RUNE_ARROW)
        {
                flg |= PROJECT_THRU;
                flg |= PROJECT_STOP;
                flg |= PROJECT_KILL;
                ty = -1;
                tx = -1;
        }
        if(spell->rune2 & RUNE_SELF)
        {
                flg |= PROJECT_THRU;
                flg |= PROJECT_STOP;
                flg |= PROJECT_KILL;
                ty = py;
                tx = px;
                unsafe = TRUE;
        }

        if((ty == -1)&&(tx == -1))
        {
                if (!get_aim_dir(&dir)) return mana_used;

                /* Use the given direction */
                tx = px + ddx[dir];
                ty = py + ddy[dir];

                /* Hack -- Use an actual "target" */
                if ((dir == 5) && target_okay())
                {
                        tx = target_col;
                        ty = target_row;
                }
        }

        if(flg & PROJECT_VIEWABLE)
        {
                project_hack(spell->type, dam);
        }
        else if(flg & PROJECT_METEOR_SHOWER)
        {
                project_meteor(rad, spell->type, dam, flg);
        }
        else project(0, rad, ty, tx, dam, spell->type, flg);

        if (unsafe) unsafe = FALSE;

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);
        p_ptr->redraw |= (PR_MANA);

        return (mana_used);
}

/* Test if all runes needed at in the player inventory */
bool test_runespell(rune_spell *spell)
{
        int i;
        object_type *o_ptr;
        bool typeok = FALSE;
        int rune2 = 0;

        for (i = 0; i < INVEN_WIELD; i++)
        {
                o_ptr = &inventory[i];

                if (!o_ptr->k_idx) continue;

                /* Does the rune1(type) match ? */
                if ((o_ptr->tval == TV_RUNE1) && (o_ptr->sval == spell->type)) typeok = TRUE;

                if ((o_ptr->tval == TV_RUNE2) && (o_ptr->sval != RUNE_STONE))
                {
                        /* Add it to the list */
                        rune2 |= 1 << o_ptr->sval;
                }
        }

        /* Need all runes to be present */
        return (typeok && ((rune2 & spell->rune2) == spell->rune2));
}

/* Ask for rune, rune2 and mana */
bool get_runespell(rune_spell *spell)
{
        int item, power_rune = 0, rune2 = 0;
        s32b power;

        int type = 0;

	object_type     *o_ptr;

	cptr q, s;

        bool OK = FALSE;

        rune_combine = 0;

        /* Restrict choices to unused runes */
        item_tester_hook = item_tester_hook_runeable1;

	/* Get an item */
        q = "Use which rune? ";
        s = "You have no rune to use.";
        if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return FALSE;

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
        type = o_ptr->sval;

        do
        {
                /* Restrict choices to unused secondary runes */
                item_tester_hook = item_tester_hook_runeable2;

                OK = !get_item(&item, q, s, (USE_INVEN | USE_FLOOR));

                if (OK) break;

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
                rune_combine |= 1 << o_ptr->sval;
                rune2 |= 1 << o_ptr->sval;
        } while (!OK);

        if (!rune2)
        {
                msg_print("You have not selected a second rune!");
                return FALSE;
        }

        power = get_quantity("Which amount of Mana?", p_ptr->csp - (power_rune * (p_ptr->lev / 5)));
        if (power < 1) power = 1;

        spell->mana = power;
        spell->type = type;
        spell->rune2 = rune2;

        return TRUE;
}

void do_cmd_rune(void)
{
        rune_spell spell;

        /* Require some mana */
        if(p_ptr->csp <= 0)
        {
                msg_print("You have no mana!");
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

        if (!get_runespell(&spell)) return;

        /* Execute at normal mana cost */
        p_ptr->csp -= rune_exec(&spell, 100);

        /* Safety :) */
        if (p_ptr->csp < 0) p_ptr->csp = 0;

	/* Take a turn */
        if (is_magestaff()) energy_use = 80;
        else energy_use = 100;

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);
        p_ptr->redraw |= (PR_MANA);
}

/*
 * Print a batch of runespells.
 */
static void print_runespell_batch(int batch, int max)
{
        char buff[80];
        rune_spell* spell;
        int i;
        s32b power, powerdiv;
        int p, dp;

        prt(format("      %-30s Fail Mana Power", "Name"), 1, 20);

        for (i = 0; i < max; i++)
        {
                spell = &rune_spells[batch * 10 + i];

                power = spell->mana;
                rune_calc_power(&power, &powerdiv);
                p = power;
                dp = powerdiv;

                sprintf(buff, "  %c) %-30s %4d%% %4d %dd%d ", I2A(i), spell->name,
                        spell_chance_rune(spell), spell->mana, dp, p);

                prt(buff, 2 + i, 20);
        }
        prt("", 2 + i, 20);
}



/*
 * List ten random spells and ask to pick one.
 */

static rune_spell* select_runespell_from_batch(int batch, bool quick, int *s_idx)
{
  char tmp[160];
  char out_val[30];
  char which;
  int mut_max = 10;
  rune_spell* ret;

  character_icky = TRUE;
  Term_save();

  if (rune_num < (batch+1)*10) {
    mut_max = rune_num - batch*10;
  }

  sprintf(tmp, "(a-%c, * to list, / to rename, - to comment) Select a power: ",
	  I2A(mut_max-1));

  prt(tmp, 0, 0);

  if (quick) {
    print_runespell_batch(batch, mut_max);
  }

  while (1) {
    which = inkey();

    if (which == ESCAPE) {
      *s_idx = -1;
      ret = NULL;
      break;

    } else if (which == '*'  || which == '?' || which == ' ') {
      print_runespell_batch(batch, mut_max);

    } else if (which == '\r' && mut_max == 1) {
      *s_idx = batch*10;
      ret = &rune_spells[batch*10];
      break;

    } else if (which == '/') {
      prt("Rename which power: ", 0, 0);
      which = tolower(inkey());

      if (islower(which) && A2I(which) <= mut_max) {
        strcpy(out_val, rune_spells[batch*10+A2I(which)].name);
        if (get_string("Name this power: ", out_val, 29))
        {
          strcpy(rune_spells[batch*10+A2I(which)].name, out_val);
        }
	prt(tmp, 0, 0);
      } else {
	bell();
	prt(tmp, 0, 0);
      }

    }
    else
    {
      which = tolower(which);
      if (islower(which) && A2I(which) < mut_max) {
        *s_idx = batch*10+A2I(which);
        ret = &rune_spells[batch*10+A2I(which)];
	break;
      } else {
	bell();
      }
    }
  }

  Term_load();
  character_icky = FALSE;

  return ret;
}


/*
 * Pick a random spell from a menu
 */

rune_spell* select_runespell(bool quick, int *s_idx)
{
  char tmp[160];
  char which;
  int batch_max = (rune_num-1)/10;

  if (rune_num == 0) {
    msg_print("There are no runespells you can cast.");
    return NULL;
  }

  character_icky = TRUE;
  Term_save();

  sprintf(tmp, "(a-%c) Select batch of powers: ", I2A(batch_max));

  prt(tmp, 0, 0);

  while (1) {
    which = inkey();

    if (which == ESCAPE) {
      Term_load();
      character_icky = FALSE;
      return NULL;

    } else if (which == '\r' && batch_max == 0) {
      Term_load();
      character_icky = FALSE;
      return select_runespell_from_batch(0, quick, s_idx);

    } else {
      which = tolower(which);
      if (islower(which) && A2I(which) <= batch_max) {
	Term_load();
        character_icky = FALSE;
        return select_runespell_from_batch(A2I(which), quick, s_idx);
      } else {
	bell();
      }
    }
  }
}

/* Cast a memorized -- note that the only limits are antimagic & conf, NOT blind */
void do_cmd_rune_cast()
{
        rune_spell *s_ptr;
        int s_idx;

        /* Require some mana */
        if(p_ptr->csp <= 0)
        {
                msg_print("You have no mana!");
                return;
        }

        /* No magic */
        if (p_ptr->antimagic)
        {
                msg_print("Your anti-magic field disrupts any magic attempts.");
                return;
        }

	/* Not when confused */
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}

        s_ptr = select_runespell(FALSE, &s_idx);

        if (s_ptr == NULL) return;

        /* Need the runes */
        if (!test_runespell(s_ptr))
        {
                msg_print("You lack some essential rune(s) for this runespell!");
		return;
        }

        /* Execute at normal mana cost */
        p_ptr->csp -= rune_exec(s_ptr, 100);

        /* Safety :) */
        if (p_ptr->csp < 0) p_ptr->csp = 0;

	/* Take a turn */
        if (is_magestaff()) energy_use = 80;
        else energy_use = 100;

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);
        p_ptr->redraw |= (PR_MANA);
}

/* Cast a runespell from a carved runestone */
void do_cmd_runestone()
{
        rune_spell s_ptr;
	object_type     *o_ptr;
	cptr q, s;
        int item;

        /* Require some mana */
        if(p_ptr->csp <= 0)
        {
                msg_print("You have no mana!");
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

        /* No magic */
        if (p_ptr->antimagic)
        {
                msg_print("Your anti-magic field disrupts any magic attempts.");
                return;
        }

        /* Restrict choices to unused runes */
        item_tester_hook = item_tester_hook_runestone_full;

	/* Get an item */
        q = "Cast from which runestone? ";
        s = "You have no runestone to cast from.";
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

        s_ptr.type = o_ptr->pval;
        s_ptr.rune2 = o_ptr->pval2;
        s_ptr.mana = o_ptr->pval3;

        /* Execute less mana */
        p_ptr->csp -= rune_exec(&s_ptr, 75);

        /* Safety :) */
        if (p_ptr->csp < 0) p_ptr->csp = 0;

	/* Take a turn */
        energy_use = 100;

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);
        p_ptr->redraw |= (PR_MANA);
}

/* Add a runespell to the list */
void do_cmd_rune_add_mem()
{
        rune_spell s_ptr;
        rune_spell *ds_ptr = &rune_spells[rune_num];

        if (rune_num >= MAX_RUNES)
        {
                msg_print("You have already learn the maximun number of runespells!");
		return;
        }

	/* Not when confused */
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}

        if (!get_runespell(&s_ptr)) return;

        ds_ptr->type = s_ptr.type;
        ds_ptr->rune2 = s_ptr.rune2;
        ds_ptr->mana = s_ptr.mana;
        strcpy(ds_ptr->name, "Unnamed Runespell");

        get_string("Name this runespell: ", ds_ptr->name, 29);

        rune_num++;

	/* Take a turn */
	energy_use = 100;

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);
        p_ptr->redraw |= (PR_MANA);
}

/* Carve a runespell onto a Runestone */
void do_cmd_rune_carve()
{
        rune_spell s_ptr;
	object_type     *o_ptr;
	cptr q, s;
        int item, i;
        char out_val[80];

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

        if (!get_check("Beware, this will destroy the involved runes, continue?")) return;

        if (!get_runespell(&s_ptr)) return;

        /* Restrict choices to unused runes */
        item_tester_hook = item_tester_hook_runestone;

	/* Get an item */
        q = "Use which runestone? ";
        s = "You have no runestone to use.";
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

        o_ptr->pval = s_ptr.type;
        o_ptr->pval2 = s_ptr.rune2;
        o_ptr->pval3 = s_ptr.mana;

	/* Start with nothing */
	strcpy(out_val, "");

	/* Use old inscription */
	if (o_ptr->note)
	{
		/* Start with the old inscription */
		strcpy(out_val, quark_str(o_ptr->note));
	}

	/* Get a new inscription (possibly empty) */
        if (get_string("Name this runestone: ", out_val, 80))
	{
		/* Save the inscription */
		o_ptr->note = quark_add(out_val);

		/* Combine the pack */
		p_ptr->notice |= (PN_COMBINE);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);
	}

        /* Delete the runes */
        for (i = 0; i < INVEN_WIELD; i++)
        {
                o_ptr = &inventory[i];

                if (o_ptr->k_idx)
                {
                        bool do_del = FALSE;

                        if ((o_ptr->tval == TV_RUNE1) && (o_ptr->sval == s_ptr.type)) do_del = TRUE;
                        if ((o_ptr->tval == TV_RUNE2) && (BIT(o_ptr->sval) & s_ptr.rune2)) do_del = TRUE;

                        if (do_del)
                        {
                                inven_item_increase(i, -1);
                                inven_item_optimize(i);
                        }
                }
        }

        /* Take a turn -- Carving takes a LONG time */
        energy_use = 400;

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);
        p_ptr->redraw |= (PR_MANA);
}


/* Remove a runespell */
void do_cmd_rune_del()
{
        rune_spell *s_ptr;
        int s_idx;
        int i;

	/* Not when confused */
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}

        s_ptr = select_runespell(FALSE, &s_idx);

        if (s_ptr == NULL) return;

        /* Delete and move */
	for (i = s_idx + 1; i < rune_num; i++)
        {
                rune_spells[i - 1].type = rune_spells[i].type;
                rune_spells[i - 1].rune2 = rune_spells[i].rune2;
                rune_spells[i - 1].mana = rune_spells[i].mana;
                strcpy(rune_spells[i - 1].name, rune_spells[i].name);
        }
        rune_num--;

	/* Take a turn */
	energy_use = 100;

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);
        p_ptr->redraw |= (PR_MANA);
}

void do_cmd_rune_add()
{
        int ext = 0;
        char ch;

        /* Select what to do */
        while (TRUE)
        {
                if (!get_com("Add to [M]emory(need runes to cast) or Carve a [R]unestone(less mana to cast)", &ch))
                {
                        ext = 0;
                        break;
                }
                if (ch == 'M' || ch == 'm')
                {
                        ext = 1;
                        break;
                }
                if (ch == 'R' || ch == 'r')
                {
                        ext = 2;
                        break;
                }
        }

        switch (ext)
        {
                /* Create a Spell in memory */
                case 1:
                        do_cmd_rune_add_mem();
                        break;
                /* Carve a Runestone */
                case 2:
                        do_cmd_rune_carve();
                        break;
        }
}

void do_cmd_runecrafter()
{
        int ext = 0;
        char ch;

        /* Select what to do */
        while (TRUE)
        {
                if (!get_com("Rune Spell:[C]reate, [D]elete, C[a]st, D[i]rectly Cast or Use [R]unestone", &ch))
                {
                        ext = 0;
                        break;
                }
                if (ch == 'C' || ch == 'c')
                {
                        ext = 1;
                        break;
                }
                if (ch == 'D' || ch == 'd')
                {
                        ext = 2;
                        break;
                }
                if (ch == 'A' || ch == 'a')
                {
                        ext = 3;
                        break;
                }
                if (ch == 'I' || ch == 'i')
                {
                        ext = 4;
                        break;
                }
                if (ch == 'R' || ch == 'r')
                {
                        ext = 5;
                        break;
                }
        }

        switch (ext)
        {
                /* Create a Spell */
                case 1:
                        do_cmd_rune_add();
                        break;
                /* Delete a Spell */
                case 2:
                        do_cmd_rune_del();
                        break;
                /* Cast a Spell */
                case 3:
                        do_cmd_rune_cast();
                        break;
                /* Directly Cast a Spell */
                case 4:
                        do_cmd_rune();
                        break;
                /* Cast a Runestone */
                case 5:
                        do_cmd_runestone();
                        break;
        }
}

void do_cmd_unbeliever_antimagic()
{
        if (p_ptr->lev < 20)
        {
                msg_print("You must be at least level 20 to be able to disrupt the magic continuum.");
                return;
        }

        if (p_ptr->class_extra6 & CLASS_ANTIMAGIC)
        {
                p_ptr->class_extra6 &= ~CLASS_ANTIMAGIC;
                msg_print("You stop disrupting the magic continuum.");
        }
        else
        {
                p_ptr->class_extra6 |= CLASS_ANTIMAGIC;
                msg_print("You start disrupting the magic continuum.");
        }

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);
}


/* Detect traps + kill traps */
void do_cmd_unbeliever()
{
        int ext = 0;
        char ch;

        /* Select what to do */
        while (TRUE)
        {
                if (!get_com("Disrupt [C]ontinuum or [D]etect Traps", &ch))
                {
                        ext = 0;
                        break;
                }
                if (ch == 'C' || ch == 'c')
                {
                        ext = 1;
                        break;
                }
                if (ch == 'D' || ch == 'd')
                {
                        ext = 2;
                        break;
                }
        }

        switch (ext)
        {
                case 1:
                        do_cmd_unbeliever_antimagic();
                        break;

                /* Detect */
                case 2:
                        if (p_ptr->lev >= 25)
                        {
                                detect_traps();
                                if (p_ptr->lev >= 35)
                                {
                                        destroy_doors_touch();
                                }
                        }
                        else
                        {
                                msg_print("You cannot use your detection abilities yet.");
                        }
                        break;
        }
}

/*
 * Summoners
 */
void do_cmd_summoner_extract()
{
	object_type     *o_ptr, forge, *q_ptr = &forge;
	cptr q, s;
        int item, r, e;
        bool partial;

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

        item_tester_tval = TV_CORPSE;

	/* Get an item */
        q = "Use which corpse? ";
        s = "You have no corpse to use.";
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

        if (r_info[o_ptr->pval2].flags1 & RF1_UNIQUE)
                partial = FALSE;
        else
                partial = get_check("Do you want to create a partial totem?");

        r = o_ptr->pval2;
        e = o_ptr->pval3;
        if (item > 0)
        {
                inven_item_increase(item, -1);
                inven_item_describe(item);
                inven_item_optimize(item);
        }
        else
        {
                floor_item_increase(-item, -1);
                floor_item_describe(-item);
                floor_item_optimize(-item);
        }

        if (magik(r_info[o_ptr->pval2].level - p_ptr->lev))
        {
                msg_print("You failled to extract a totem.");
                energy_use += 100;
                return;
        }

        object_prep(q_ptr, lookup_kind(TV_TOTEM, partial ? 1 : 2));
        q_ptr->pval = r;
        q_ptr->pval2 = 0;
        q_ptr->number = 1;
        (void)inven_carry(q_ptr, FALSE);

        msg_print("You extract a totem from the dead corpse.");
        energy_use += 100;
}

void summon_true(int r_idx, int item)
{
        int i, status, x = 1, y = 1, rx, ry = 0, chance;
        bool smash;
        monster_race *r_ptr = &r_info[r_idx];

        /* So you're a True Totem, are you? */
        /* Let's see if we're going to smash you, and whether you'll be loyal */
        if (r_ptr->flags1 & (RF1_UNIQUE))
        {
                smash = TRUE;
                chance = (p_ptr->lev * 70 / (r_ptr->level + 1));
                if (magik(chance))
                {
                        status = MSTATUS_PET;
                }
                else
                {
                        status = MSTATUS_ENEMY;
                }
        }
        else
        {
                smash = FALSE;
                chance = (r_ptr->level * 25 / p_ptr->lev);
                if (magik(chance)) smash = TRUE;

                chance = (p_ptr->lev * 130 / (r_ptr->level + 1));
                if (magik(chance))
                {
                        status = MSTATUS_PET;
                }
                else
                {
                        status = MSTATUS_ENEMY;
                }
        }

        /* Let's find a spot for you. */
        for (i = 0; i < 40; i++)
        {
                rx = (rand_int(8) - 4) + px;
                ry = (rand_int(8) - 4) + py;
                if (in_bounds(ry, rx) && cave_empty_bold(ry, rx))
                {
                        x = rx;
                        y = ry;
                        break;
                }
        }

        if (i == 40)
        {
                /* Sorry, no room. */
                msg_print("The summoning fails due to lack of room.");
                return;
        }

        /* And now we actually summon. */
        bypass_r_ptr_max_num  = TRUE;
        if (!(i = place_monster_one (y, x, r_idx, 0, 0, status)))
        {
                /* We've failed. */
                msg_print("The summoning fails.");
        }
        else
                m_list[i].status = status;
        bypass_r_ptr_max_num  = FALSE;

        /* HULK SMASH TOTEM! */
        if (smash)
        {
                /* Eliminate the totem (from the pack) */
                if (item >= 0)
                {
                        inven_item_increase(item, -1);
                        inven_item_describe(item);
                        inven_item_optimize(item);
                }

                /* Eliminate the totem (from the floor) */
                else
                {
                        floor_item_increase(0 - item, -1);
                        floor_item_describe(0 - item);
                        floor_item_optimize(0 - item);
                }
        }

        /* Finished. */
        return;
}

void do_cmd_summoner_summon()
{
        int item, x = 1, y = 1, rx, ry, m_idx = 0, i;
        cptr q,s;
        object_type *o_ptr;
        monster_type *m_ptr;

        /* Which Totem? */
        item_tester_tval = TV_TOTEM;

        q = "Summon from which Totem?";
        s = "There are no totems to summon from!";
        if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

        /* Access the item */
        if (item >= 0)
        {
                o_ptr = &inventory[item];
        }
        else
        {
                o_ptr = &o_list[0 - item];
        }

        /* Take a turn */
        energy_use = 100;

        /* True Totems have their own function. */

        if (o_ptr->sval == 2)
        {
                summon_true(o_ptr->pval, item);
                return;
        }

        /* So you're a Partial Totem, are you? */

        /* Let's find a spot for you. */

        for (i = 0; i < 40; i++)
        {
                rx = (rand_int(8) - 4) + px;
                ry = (rand_int(8) - 4) + py;
                if (in_bounds(ry, rx) && cave_empty_bold(ry, rx))
                {
                        x = rx;
                        y = ry;
                        break;
                }
        }

        if (i == 40)
        {
                /* Sorry, no room. */
                msg_print("The summoning fails due to lack of room.");
                return;
        }

        /* And now we actually summon. */
        bypass_r_ptr_max_num  = TRUE;
        m_idx = place_monster_one(y, x, o_ptr->pval, 0, 0, MSTATUS_PET);
        bypass_r_ptr_max_num  = FALSE;

        if (!m_idx)
        {
                /* We've failed. */
                msg_print("The summoning fails.");
        }

        /* Now to mark him as ours. */
        m_ptr = &m_list[m_idx];
        m_ptr->mflag |= MFLAG_PARTIAL;
}

void do_cmd_summoner(void)
{
        int ext = 0;
        char ch;

        /* Select what to do */
        while (TRUE)
        {
                if (!get_com("[E]xtract a totem, [S]ummon", &ch))
                {
                        ext = 0;
                        break;
                }
                if (ch == 'E' || ch == 'e')
                {
                        ext = 1;
                        break;
                }
                if (ch == 's' || ch == 'S')
                {
                        ext = 2;
                        break;
                }
        }

        switch (ext)
        {
                case 1:
                        do_cmd_summoner_extract();
                        break;
                case 2:
                        do_cmd_summoner_summon();
                        break;
        }
}

/*
 * Fighters may invoke The Rush.
 */
void do_cmd_blade(void)
{
    /* Are we already Rushed? */
    if (p_ptr->rush)
    {
        msg_format("You have %d turns of The Rush remaining", p_ptr->rush);
        return;
    }

    /* Are you sure? */
    if (!get_check("Are you sure you want to invoke The Rush?")) return;

    /* Let's Rush! */
    set_rush(2 + p_ptr->lev / 2 + randint(p_ptr->lev / 2));
}

/*
 * Dodge Chance Feedback.
 */
void use_ability_blade(void)
{
    if (p_ptr->dodge_chance < 5)
    {
        msg_print("You have almost no chance of dodging.");
    }
    else if (p_ptr->dodge_chance < 10)
    {
        msg_print("You have a slight chance of dodging.");
    }
    else if (p_ptr->dodge_chance < 20)
    {
        msg_print("You have a signifigant chance of dodging.");
    }
    else if (p_ptr->dodge_chance < 40)
    {
        msg_print("You have a large chance of dodging.");
    }
    else if (p_ptr->dodge_chance < 70)
    {
        msg_print("You have a high chance of dodging.");
    }
    else
    {
        msg_print("You will usually dodge succesfully.");
    };
    return;
}
