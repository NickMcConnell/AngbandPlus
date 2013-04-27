/* File: mind.c */

/* Purpose: Mindcrafter code */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

	/* Level gained, cost, %fail, name */
mindcraft_power mindcraft_powers[MINDCRAFT_MAX] =
{
	{1, 1, 15, "Neural Blast"},
	{2, 1, 20, "Precognition"},
	{3, 2, 25, "Minor Displacement"},
	{7, 6, 35, "Major Displacement"},
	{9, 7, 50, "Domination"},
	{11, 7, 30, "Pulverise"},
	{13, 12, 50, "Character Armour"},
	{15, 12, 60, "Psychometry"},
	{18, 10, 45, "Mind Wave"},
	{23, 15, 50, "Adrenaline Channeling"},
	{25, 10, 40, "Psychic Drain"},
	{28, 20, 45, "Telekinetic Wave"}
};


void mindcraft_info(char *p, int power)
{
	int plev = p_ptr->lev;

	p[0] = 0;

	switch (power)
	{
		case MINDCRAFT_NEURAL_BLAST:
		{
			strnfmt(p, 80, " dam %dd%d", 3 + ((plev - 1) / 4), 3 + plev / 15);
			break;
		}
		case MINDCRAFT_PRECOGNITION:
		{
			break;
		}
		case MINDCRAFT_MINOR_DISPLACEMENT:
		{
			strnfmt(p, 80, " range %d", (plev < 40 ? 10 : plev + 2));
			break;
		}
		case MINDCRAFT_MAJOR_DISPLACEMENT:
		{
			strnfmt(p, 80, " range %d", plev * 5);
			break;
		}
		case MINDCRAFT_DOMINATION:
		{
			break;
		}
		case MINDCRAFT_PULVERISE:
		{
			strnfmt(p, 80, " dam %dd8", 8 + ((plev - 5) / 4));
			break;
		}
		case MINDCRAFT_CHARACTER_ARMOUR:
		{
			strnfmt(p, 80, " dur %d", plev);
			break;
		}
		case MINDCRAFT_PSYCHOMETRY:
		{
			break;
		}
		case MINDCRAFT_MIND_WAVE:
		{
			strnfmt(p, 80, " dam %d", plev * ((plev - 5) / 10 + 1));
			break;
		}
		case MINDCRAFT_ADRENALINE_CHANNELING:
		{
			strnfmt(p, 80, " dur 11-%d", plev + plev / 2 + 10);
			break;
		}
		case MINDCRAFT_PSYCHIC_DRAIN:
		{
			strnfmt(p, 80, " dam %dd6", plev / 2);
			break;
		}
		case MINDCRAFT_TELEKINETIC_WAVE:
		{
			strnfmt(p, 80, " dam %d", plev * (plev > 39 ? 4 : 3));
			break;
		}
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
 *
 * What bug? -SF-
 */
static int get_mindcraft_power(int *sn)
{
	int i;
	int num = 0;
	int y = 1;
	int x = 20;
	int minfail;
	int plev = p_ptr->lev;
	int chance;
	int ask;
	char choice;
	char out_val[160];
	char comment[80];
	cptr p = "power";
	mindcraft_power spell;
	bool flag;

	/* Assume cancelled */
	*sn = (-1);

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

	/* Nothing chosen yet */
	flag = FALSE;

	for (i = 0; i < MINDCRAFT_MAX; i++)
	{
		if (mindcraft_powers[i].min_lev <= plev)
		{
			num++;
		}
	}

	/* Build a prompt (accept all spells) */
	(void)strnfmt(out_val, 78, "(%^ss %c-%c, ESC=exit) Use which %s? ",
				  p, I2A(0), I2A(num - 1), p);
	
	/* Save the screen */
	screen_save();

	/* Display a list of spells */
	prtf(x, y, "");
	put_fstr(x + 5, y, "Name");
	put_fstr(x + 35, y, "Lv Mana Fail Info");

	/* Dump the spells */
	for (i = 0; i < MINDCRAFT_MAX; i++)
	{
		/* Access the spell */
		spell = mindcraft_powers[i];
		if (spell.min_lev > plev) break;

		chance = spell.fail;

		/* Reduce failure rate by "effective" level adjustment */
		chance -= 3 * (plev - spell.min_lev);

		/* Reduce failure rate by INT/WIS adjustment */
		chance -= adj_mag_stat[p_ptr->stat[mp_ptr->spell_stat].ind] - 3;

		/* Not enough mana to cast */
		if (spell.mana_cost > p_ptr->csp)
		{
			chance += 5 * (spell.mana_cost - p_ptr->csp);
		}

		/* Extract the minimum failure rate */
		minfail = adj_mag_fail[p_ptr->stat[mp_ptr->spell_stat].ind];

		/* Minimum failure rate */
		if (chance < minfail) chance = minfail;

		/* Stunning makes spells harder */
		if (p_ptr->tim.stun > 50) chance += 25;
		else if (p_ptr->tim.stun) chance += 15;

		/* Always a 5 percent chance of working */
		if (chance > 95) chance = 95;

		/* Get info */
		mindcraft_info(comment, i);

		/* Dump the spell --(-- */
		prtf(x, y + i + 1, "  %c) %-30s%2d %4d %3d%%%s",
				I2A(i), spell.name,
				spell.min_lev, spell.mana_cost, chance, comment);
	}

	/* Clear the bottom line */
	prtf(x, y + i + 1, "");

	/* Get a spell from the user */
	while (!flag && get_com(out_val, &choice))
	{
		/* Note verify */
		ask = isupper(choice);

		/* Lowercase */
		if (ask) choice = tolower(choice);

		/* Extract request */
		i = (islower(choice) ? A2I(choice) : -1);

		/* Totally Illegal */
		if ((i < 0) || (i >= num))
		{
			bell("Illegal mindcrafter power choice!");
			continue;
		}

		/* Save the spell index */
		spell = mindcraft_powers[i];

		/* Verify it */
		if (ask)
		{
			/* Belay that order */
			if (!get_check("Use %s? ", mindcraft_powers[i].name)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}

	/* Restore the screen */
	screen_load();

	/* Show choices */
	/* Update */
	p_ptr->window |= (PW_SPELL);

	/* Window stuff */
	window_stuff();

	/* Abort if needed */
	if (!flag) return (FALSE);

	/* Save the choice */
	(*sn) = i;

	repeat_push(*sn);

	/* Success */
	return (TRUE);
}


/*
 * do_cmd_cast calls this function if the player's class
 * is 'mindcrafter'.
 */
static bool cast_mindcrafter_spell(int spell)
{
	int b;
	int dir;
	int plev = p_ptr->lev;


	/* spell code */
	switch (spell)
	{
		case MINDCRAFT_NEURAL_BLAST:
			/* Mindblast */
			if (!get_aim_dir(&dir)) return FALSE;

			if (randint1(100) < plev * 2)
				(void)fire_beam(GF_PSI, dir,
								damroll(3 + ((plev - 1) / 4), (3 + plev / 15)));
			else
				(void)fire_ball(GF_PSI, dir,
								damroll(3 + ((plev - 1) / 4), (3 + plev / 15)),
								0);
			break;
		case MINDCRAFT_PRECOGNITION:
			if (plev > 44)
				wiz_lite();
			else if (plev > 19)
				map_area();

			if (plev < 30)
			{
				b = detect_monsters_normal();
				if (plev > 14)
					b |= detect_monsters_invis();

				if (plev > 4)
				{
					b |= detect_traps(TRUE);
					b |= detect_doors();
				}
			}
			else
			{
				b = detect_all();
			}

			if ((plev > 24) && (plev < 40))
			{
				(void)inc_tim_esp(plev);
			}

			if (!b) msgf("You feel safe.");
			break;
		case MINDCRAFT_MINOR_DISPLACEMENT:
			/* Minor displace */
			if (plev < 40)
			{
				teleport_player(10);
			}
			else
			{
				msgf("You open a dimensional gate. Choose a destination.");
				return dimension_door();
			}
			break;
		case MINDCRAFT_MAJOR_DISPLACEMENT:
			/* Major displace */
			if (plev > 29) (void)banish_monsters(plev);

			teleport_player(plev * 5);
			break;
		case MINDCRAFT_DOMINATION:
			/* Domination */
			if (plev < 30)
			{
				if (!get_aim_dir(&dir)) return FALSE;

				(void)fire_ball(GF_DOMINATION, dir, plev, 0);
			}
			else
			{
				(void)charm_monsters(plev * 2);
			}
			break;
		case MINDCRAFT_PULVERISE:
			/* Fist of Force  ---  not 'true' TK */
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_ball(GF_SOUND, dir, damroll(8 + ((plev - 5) / 4), 8),
							(plev > 20 ? (plev - 20) / 8 + 1 : 0));
			break;
		case MINDCRAFT_CHARACTER_ARMOUR:
			/* Character Armour */
			(void)inc_shield(plev);
			if (plev > 16) (void)inc_oppose_acid(plev);
			if (plev > 20) (void)inc_oppose_fire(plev);
			if (plev > 24) (void)inc_oppose_cold(plev);
			if (plev > 28) (void)inc_oppose_elec(plev);
			if (plev > 32) (void)inc_oppose_pois(plev);
			break;
		case MINDCRAFT_PSYCHOMETRY:
			/* Psychometry */
			if (plev < 25)
				return psychometry();
			else
				return ident_spell();
		case MINDCRAFT_MIND_WAVE:
			/* Mindwave */
			msgf("Mind-warping forces emanate from your brain!");
			if (plev < 25)
				(void)project(0, 2 + plev / 10, p_ptr->px, p_ptr->py,
							  (plev * 3) / 2, GF_PSI, PROJECT_KILL);
			else
				(void)mindblast_monsters(plev * ((plev - 5) / 10 + 1));
			break;
		case MINDCRAFT_ADRENALINE_CHANNELING:
			/* Adrenaline */
			(void)clear_afraid();
			(void)clear_stun();

			/*
			 * Only heal when Adrenalin Channeling is not active. We check
			 * that by checking if the player isn't fast and 'heroed' atm.
			 */
			if (!p_ptr->tim.fast || !(p_ptr->tim.hero || p_ptr->tim.shero))
			{
				(void)hp_player(plev);
			}

			b = 10 + randint1((plev * 3) / 2);
			if (plev < 35)
				(void)inc_hero(b);
			else
				(void)inc_shero(b);

			/* Haste */
			(void)inc_fast(b);

			break;
		case MINDCRAFT_PSYCHIC_DRAIN:
			/* Psychic Drain */
			if (!get_aim_dir(&dir)) return FALSE;

			b = damroll(plev / 2, 6);

			/* This is always a radius-0 ball now */
			if (fire_ball(GF_PSI_DRAIN, dir, b, 0))
				p_ptr->energy -= randint1(150);
			break;
		case MINDCRAFT_TELEKINETIC_WAVE:
			/* Telekinesis */
			msgf
				("A wave of pure physical force radiates out from your body!");
			(void)project(0, 3 + plev / 10, p_ptr->px, p_ptr->py,
						  plev * (plev > 39 ? 4 : 3), GF_TELEKINESIS,
						  PROJECT_KILL | PROJECT_ITEM | PROJECT_GRID);
			break;
		default:
			msgf("Unknown Mindcrafter power!");
	}

	return TRUE;
}


/*
 * do_cmd_cast calls this function if the player's class
 * is 'mindcrafter'.
 */
void do_cmd_mindcraft(void)
{
	int n = 0;
	int chance;
	int minfail;
	int plev = p_ptr->lev;
	int old_csp = p_ptr->csp;
	mindcraft_power spell;
	bool cast;


	/* not if confused */
	if (p_ptr->tim.confused)
	{
		msgf("You are too confused!");
		return;
	}

	/* get power */
	if (!get_mindcraft_power(&n)) return;

	spell = mindcraft_powers[n];

	/* Verify "dangerous" spells */
	if (spell.mana_cost > p_ptr->csp)
	{
		/* Warning */
		msgf("You do not have enough mana to use this power.");

		/* Verify */
		if (!get_check("Attempt it anyway? ")) return;
	}

	/* Spell failure chance */
	chance = spell.fail;

	/* Reduce failure rate by "effective" level adjustment */
	chance -= 3 * (plev - spell.min_lev);

	/* Reduce failure rate by INT/WIS adjustment */
	chance -= adj_mag_stat[p_ptr->stat[mp_ptr->spell_stat].ind] - 3;

	/* Not enough mana to cast */
	if (spell.mana_cost > p_ptr->csp)
	{
		chance += 5 * (spell.mana_cost - p_ptr->csp);
	}

	/* Extract the minimum failure rate */
	minfail = adj_mag_fail[p_ptr->stat[mp_ptr->spell_stat].ind];

	/* Minimum failure rate */
	if (chance < minfail) chance = minfail;

	/* Stunning makes spells harder */
	if (p_ptr->tim.stun > 50) chance += 25;
	else if (p_ptr->tim.stun) chance += 15;

	/* Always a 5 percent chance of working */
	if (chance > 95) chance = 95;

	/* Failed spell */
	if (randint0(100) < chance)
	{
		if (flush_failure) flush();
		msgf("You failed to concentrate hard enough!");
		sound(SOUND_FAIL);

		/* Backfire */
		if (randint1(100) < (chance / 2))
		{
			int b = randint1(100);

			if (b < 5)
			{
				msgf("Oh, no! Your mind has gone blank!");
				(void)lose_all_info();
			}
			else if (b < 15)
			{
				msgf("Weird visions seem to dance before your eyes...");
				(void)inc_image(rand_range(5, 15));
			}
			else if (b < 45)
			{
				msgf("Your brain is addled!");
				(void)inc_confused(randint1(8));
			}
			else if (b < 90)
			{
				(void)inc_stun(randint1(8));
			}
			else
			{
				/* Mana storm */
				msgf
					("Your mind unleashes its power in an uncontrollable storm!");
				(void)project(1, 2 + plev / 10, p_ptr->px, p_ptr->py, plev * 2,
							  GF_MANA,
							  PROJECT_JUMP | PROJECT_KILL | PROJECT_GRID |
							  PROJECT_ITEM);
				p_ptr->csp = MAX(0, p_ptr->csp - plev * MAX(1, plev / 10));
			}
		}
	}
	else
	{
		sound(SOUND_ZAP);

		/* Cast the spell */
		cast = cast_mindcrafter_spell(n);

		if (!cast) return;
	}

	/* Take a turn */
	p_ptr->state.energy_use = 100;

	/* Sufficient mana */
	if (spell.mana_cost <= old_csp)
	{
		/* Use some mana */
		p_ptr->csp -= spell.mana_cost;

		/* Limit */
		if (p_ptr->csp < 0) p_ptr->csp = 0;
	}

	/* Over-exert the player */
	else
	{
		int oops = spell.mana_cost - old_csp;

		/* No mana left */
		p_ptr->csp = 0;
		p_ptr->csp_frac = 0;

		/* Message */
		msgf("You faint from the effort!");

		/* Hack -- Bypass free action */
		(void)inc_paralyzed(randint1(5 * oops + 1));

		/* Damage WIS (possibly permanently) */
		if (randint0(100) < 50)
		{
			bool perm = (randint0(100) < 25);

			/* Message */
			msgf("You have damaged your mind!");

			/* Reduce constitution */
			(void)dec_stat(A_WIS, rand_range(15, 25), perm);
		}
	}

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);
	p_ptr->window |= (PW_SPELL);
}
