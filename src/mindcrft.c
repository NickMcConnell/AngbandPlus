/* File: mindcrft.c */

/* Purpose: Mindcrafter code */

/*
 * There is still some Mincrafter-related code in object2.c that I consider
 * to be too much trouble to extract in order to use here. -- Gumby
 *
 * Functions included here:
 *
 * mindcraft_powers[]; mindcraft_info(); get_mindcraft_power();
 * do_cmd_mindcraft();
 */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 */

#include "angband.h"

mindcraft_power mindcraft_powers[MAX_MINDCRAFT_POWERS] =
{
	/* Level,  cost,  %fail,  name */
        { 1,   1,  15, "Precognition" },          /* Detection */
        { 2,   1,  20, "Neural Blast" },          /* Psi bolt */
        { 3,   2,  25, "Minor Displacement" },    /* Phase/dimension door */
        { 7,   6,  35, "Major Displacement" },    /* Teleport Self -> Away */
        { 9,   7,  50, "Domination" },		  /* Confusion -> Charm */
        { 11,  7,  30, "Pulverize" },             /* TK Bolt -> Ball */
        { 13, 12,  50, "Character Armour" },      /* +AC & Resistances */
        { 15, 12,  60, "Psychometry" },		  /* Identify -> *ID* */
        { 18, 10,  45, "Mind Wave" },             /* Centered Ball -> LOS */
	{ 20, 12,  40, "Apportation" },		  /* Fetch an item */
        { 23, 15,  50, "Adrenaline Channeling" }, /* Haste + Heroism */
        { 25, 10,  40, "Psychic Drain" },         /* Enemy HP to SP */
        { 28, 20,  45, "Telekinetic Wave" },      /* Centered Ball -> LOS */
};


void mindcraft_info(char *p, int power)
{
    int plev = p_ptr->lev;

    strcpy(p, "");

	switch (power)
	{
		case 0:  break;
		case 1:  sprintf(p, " dam %dd%d", 3 + ((plev - 1) / 4), 3 + plev/15); break;
		case 2:  sprintf(p, " range %d", (plev < 30 ? 10 : plev + 2)); break;
		case 3:  sprintf(p, " range %d", (plev < 45 ? plev * 5 : plev)); break;
		case 4:  break;
		case 5:  sprintf(p, " dam %dd8", 10+((plev-5)/3)); break;
		case 6:  sprintf(p, " dur %d", plev); break;
		case 7:  break;
		case 8:  sprintf(p, " dam %d", plev * ((plev / 10) + 1)); break;
		case 9:  strcpy (p, " wgt 250"); break;
		case 10: sprintf(p, " dur 11-%d", plev + plev / 2); break;
		case 11: sprintf(p, " dam %dd8", plev/2);  break;
		case 12: sprintf(p, " dam %d", plev * ((plev / 10) + 2)); break;
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
	int		i, ask, cur_wgt, max_wgt;
	int		num = 0;
	int		y = 1;
	int		x = 20;
	int		minfail = 0;
	int		plev = p_ptr->lev;
	int		chance = 0;
	bool		flag, redraw;
	char		choice;
	mindcraft_power	spell;
	char		out_val[160];
	char		comment[80];
	cptr		p = "power";

	/* Assume cancelled */
	*sn = (-1);

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	for (i = 0; i < MAX_MINDCRAFT_POWERS; i++)
		if (mindcraft_powers[i].min_lev <= plev)
			num++;

	/* Build a prompt (accept all spells) */
	strnfmt(out_val, 78, "(%^ss %c-%c, *=List, ESC=exit) Use which %s? ",
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
				Term_save();

				/* Display a list of spells */
				prt("", y, x);
				put_str("Name", y, x + 5);
				put_str("Lv Mana Fail Info", y, x + 35);

				/* Dump the spells */
				for (i = 0; i < MAX_MINDCRAFT_POWERS; i++)
				{
					/* Access the spell */
					spell = mindcraft_powers[i];
					if (spell.min_lev > plev) break;

					chance = spell.fail;

					/*
					 * Reduce failure rate by
					 * "effective" level adjustment
					 */
					chance -= 3 * (p_ptr->lev - spell.min_lev);
					if (chance < 0) chance = 0;

					/*
					 * Reduce failure rate by INT/WIS
					 * adjustment
					 */
					chance -= 3 * (adj_mag_stat[p_ptr->stat_ind[mp_ptr->spell_stat]] - 1);
					if (chance < 0) chance = 0;

					/* Extract the minimum failure rate */
					minfail = adj_mag_fail[p_ptr->stat_ind[mp_ptr->spell_stat]];
				    
					/* Minimum failure rate */
					if (chance < minfail) chance = minfail;

					/* Not enough mana to cast */
					if (spell.mana_cost > p_ptr->csp)
					{
						chance += 5 * (spell.mana_cost - p_ptr->csp);
					}

					/*
					 * Wearing too much armour hurts
					 * your spellcasting. -- Gumby
					 */
					if (p_ptr->cumber_armor)
					{
						/* Weigh the armor */
						cur_wgt = 0;
						cur_wgt += inventory[INVEN_BODY].weight;
						cur_wgt += inventory[INVEN_HEAD].weight;
						cur_wgt += inventory[INVEN_ARM].weight;
						cur_wgt += inventory[INVEN_OUTER].weight;
						cur_wgt += inventory[INVEN_HANDS].weight;
						cur_wgt += inventory[INVEN_FEET].weight;

						/* Determine the weight allowance */
						max_wgt = mp_ptr->spell_weight;

						/* Heavy armor increases fail rate */
						if (((cur_wgt - max_wgt) / 10) > 0)
						{
							chance += ((cur_wgt - max_wgt) / 20);
						}
					}

					/* Stunning makes spells harder */
					if (p_ptr->stun > 50) chance += 25;
					else if (p_ptr->stun) chance += 15;

					/* Being Berserk doesn't help- G */
					if (p_ptr->shero) chance += 25;

					/* Always a 5 percent chance of working */
					if (chance > 95) chance = 95;
				    
					/* Get info */
					mindcraft_info(comment, i);
				    
					/* Dump the spell */
					sprintf(psi_desc, "  %c) %-30s%2d %4d %3d%%%s",
					I2A(i), spell.name, spell.min_lev,
					spell.mana_cost, chance, comment);
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
		spell = mindcraft_powers[i];

	        /* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Prompt */
			strnfmt(tmp_val, 78, "Use %s? ", mindcraft_powers[i].name);

			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}

	/* Restore the screen */
	if (redraw) Term_load();

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

	/* Success */
	return (TRUE);
}


/*
 * do_cmd_cast calls this function if the player's class
 * is 'mindcrafter'.
 */
void do_cmd_mindcraft(void)
{
	int		n = 0, b = 0;
	int		chance, dir, cur_wgt, max_wgt;
	int		minfail = 0;
	int		plev = p_ptr->lev;
	mindcraft_power	spell;
    
	/* not if confused */
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}
    
	/* get power */
	if (!get_mindcraft_power(&n))  return;
	
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
	if (chance < 0) chance = 0;

	/* Reduce failure rate by INT/WIS adjustment */
	chance -= 3 * (adj_mag_stat[p_ptr->stat_ind[mp_ptr->spell_stat]] - 1);
	if (chance < 0) chance = 0;

	/* Extract the minimum failure rate */
	minfail = adj_mag_fail[p_ptr->stat_ind[mp_ptr->spell_stat]];
				    
	/* Minimum failure rate */
	if (chance < minfail) chance = minfail;

	/* Not enough mana to cast */
	if (spell.mana_cost > p_ptr->csp)
	{
		chance += 5 * (spell.mana_cost - p_ptr->csp);
	}

	/* Wearing too much armour hurts your spellcasting. -- Gumby */
	if (p_ptr->cumber_armor)
	{
		/* Weigh the armor */
		cur_wgt = 0;
		cur_wgt += inventory[INVEN_BODY].weight;
		cur_wgt += inventory[INVEN_HEAD].weight;
		cur_wgt += inventory[INVEN_ARM].weight;
		cur_wgt += inventory[INVEN_OUTER].weight;
		cur_wgt += inventory[INVEN_HANDS].weight;
		cur_wgt += inventory[INVEN_FEET].weight;

		/* Determine the weight allowance */
		max_wgt = mp_ptr->spell_weight;

		/* Heavy armor increases fail rate */
		if (((cur_wgt - max_wgt) / 10) > 0)
		{
			chance += ((cur_wgt - max_wgt) / 20);
		}
	}

	/* Stunning makes spells harder */
	if (p_ptr->stun > 50) chance += 25;
	else if (p_ptr->stun) chance += 15;

	/* Being Berserk doesn't help, either. -- Gumby */
	if (p_ptr->shero) chance += 25;

	/* Always a 5 percent chance of working */
	if (chance > 95) chance = 95;

	/* Failed spell */
	if (rand_int(100) < chance)
	{
		if (flush_failure) flush();
		msg_format("You failed to concentrate hard enough!");

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
				project(1, FALSE, 2+plev/10, py, px, plev * 2,
				    GF_MANA,PROJECT_JUMP|PROJECT_KILL|PROJECT_GRID|PROJECT_ITEM, FALSE);
				p_ptr->csp = MAX(0, p_ptr->csp - plev * MAX(1, plev/10));
			}
		}
	}
	else
	{
		/* spell code */
		switch (n)
		{
			case 0: /* Precognition */
				if (plev > 44) wiz_lite();
				else if (plev > 19) map_area();

				if (plev < 35)
				{
					if (plev > 14)
					{
						b = detect_monsters_normal();
					}
					else
					{
						b = detect_monsters_mental();
					}

					if (plev > 14) b |= detect_monsters_invis();
                                        if (plev > 9)  b |= detect_traps();
					if (plev > 4)  b |= detect_doors();
                                        if (plev > 4)  b |= detect_stairs();
				}
				else
				{
					b = detect_all();
				}

				if ((plev > 24) && (plev < 40))
					set_tim_esp(p_ptr->tim_esp + plev);

				if (!b) msg_print("You sense nothing.");
				break;
			case 1: /* Neural Blast */
				if (!get_aim_dir(&dir)) return;
				if (randint(100) < plev * 2)
					fire_beam(GF_PSI, dir, damroll(3 + ((plev - 1) / 3), (3+plev/15)));
				else
					fire_ball(GF_PSI, dir, damroll(3 + ((plev - 1) / 3), (3+plev/15)), 0);
				break;
			case 2: /* Minor Displacement */
				if (plev < 30)
				{
					teleport_player(10);
				}
				else
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
				break;
			case 3: /* Major Displacement */
				if (plev < 45)
				{
					teleport_player(plev * 5);
				}
				else
				{
					if (!get_aim_dir(&dir)) return;
					(void)fire_beam(GF_AWAY_ALL, dir, plev);
				}
				break;
			case 4: /* Domination */
				if (plev < 35)
				{
					if (!get_aim_dir(&dir)) return;
					fire_ball(GF_DOMINATION, dir, plev, 0);
				}
				else
				{
					charm_monsters(p_ptr->lev * 2);
				}
				break;
			case 5: /* Pulverize */
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_SOUND, dir, damroll(10+((plev-5)/3), 8),
				    (plev > 20 ? (plev-20)/8 + 1 : 0));
				break;
			case 6: /* Character Armour */
				set_shield(p_ptr->shield + plev);
				if (plev > 14) set_oppose_acid(p_ptr->oppose_acid + plev);
				if (plev > 19) set_oppose_fire(p_ptr->oppose_fire + plev);
				if (plev > 24) set_oppose_cold(p_ptr->oppose_cold + plev);
				if (plev > 29) set_oppose_elec(p_ptr->oppose_elec + plev);
				if (plev > 34) set_oppose_pois(p_ptr->oppose_pois + plev);
				break;
			case 7: /* Psychometry */
				if (plev > 39) identify_fully();
				else if (plev > 29) ident_spell();
				else psychometry();
				break;
			case 8: /* Mind Wave */
				msg_print("Mind-warping forces emanate from your brain!");
				if (plev < 30)
					project(0, FALSE, 2 + (plev / 10), py, px,
						plev * ((plev / 10) + 1),
						GF_PSI, PROJECT_KILL, FALSE);
				else
					(void)mindblast_monsters(plev * ((plev / 10) + 1));
				break;
			case 9: /* Apportation */
				if (!get_aim_dir(&dir)) return;
				fetch(dir, 250, FALSE);
				break;
			case 10: /* Adrenaline Channeling */
				set_afraid(0);
				set_stun(0);
				hp_player(plev);

				b = 10 + randint((plev * 3) / 2);

				set_hero(p_ptr->hero + b);

				if (!p_ptr->fast) (void)set_fast(b);
				else (void)set_fast(p_ptr->fast + b);
				break;
			case 11: /* Psychic Drain */
				if (!get_aim_dir(&dir)) return;
				b = damroll(plev / 2, 8);
				if (fire_ball(GF_PSI_DRAIN, dir, b,
						0 + (plev - 25) / 10))
					p_ptr->energy -= randint(150);
				break;
			case 12: /* Telekinetic Wave */
				msg_print("A wave of pure physical force radiates out from your body!");
				project(0, FALSE, 3 + (plev / 10), py, px,
				   plev * ((plev / 10) + 2), GF_TELEKINESIS,
				   PROJECT_KILL|PROJECT_ITEM|PROJECT_GRID, FALSE);
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
