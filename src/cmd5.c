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
 * Copy a spell from some external buffer to the player's spell list,
 * and return it.
 */
spell *add_new_spell(spell * which)
{
	spell *new_spell;

	if (spell_num == MAX_SPELLS)
	{
		mprint(MSG_URGENT,
			"Can't add spell -- spell buffer is filled up!");
		return NULL;
	}

	new_spell = &spells[spell_num];
	spell_num++;

	COPY(new_spell, which, spell);
	return new_spell;
}



/* 
 * Remove the spells at the specified indexes from the player's spell list. 
 * This is a slow and stupid algortihm reminiscent of selection sort.
 */
void remove_spells(int *index, int i_num)
{
	int i, j, new_num = spell_num;

	/* Delete the offending spells. */
	for (i = 0; i < i_num; i++)
	{

		/* Paranoia. */
		if (index[i] >= MAX_SPELLS || index[i] < 0)
			continue;

		WIPE(&spells[index[i]], spell);
		new_num--;
	}

	/* Fill in all the empty spots. */
	for (i = 0; i < spell_num; i++)
	{

		/* Hack -- check if spell is deleted. */
		if (spells[i].proj_list == NULL)
		{

			/* Find the nearest undeleted spell. */
			j = i;

			while (TRUE)
			{

				/* All done. */
				if (j == spell_num)
				{
					spell_num = new_num;
					return;
				}

				if (spells[j].proj_list != NULL)
				{
					/* Fill in the empty spot just to the left of us. */
					COPY(&spells[i], &spells[j], spell);
					WIPE(&spells[j], spell);
					break;
				}

				j++;
			}
		}
	}
}


/*
 * Print a batch of spells.
 */
static void print_spell_batch(int batch, int max)
{
	char buff[80];
	spell *rspell;
	int i, x;

	x = screen_x - 54;

	prt(format("     %-30s     Lv Mana Fail  ", "Name"), 1, x);

	for (i = 0; i < max; i++)
	{
		byte attr = TERM_WHITE;

		rspell = &spells[batch * 10 + i];

		if (rspell->unknown)
		{
			sprintf(buff, "  %c) %-30s  (Spell unknown)  ", I2A(i),
				(rspell->level > p_ptr->lev ? "Illegible" : rspell->name));
			if (rspell->level > p_ptr->lev)
				attr = TERM_L_DARK;
			else
				attr = TERM_L_BLUE;
		}
		else if (rspell->untried)
		{
			sprintf(buff, "  %c) %-30s  (Spell untried)  ", I2A(i),
				rspell->name);
			attr = TERM_L_GREEN;
		}
		else
		{
			sprintf(buff, "  %c) %-30s     %2d %4d %3d%%  ", I2A(i),
				rspell->name, rspell->level, rspell->mana,
				spell_chance(rspell));
		}

		c_prt(attr, buff, 2 + i, x);
	}

	prt("", 2 + i, x);
}


/* 
 * List ten random spells and ask to pick one. 
 */
static spell *select_spell_from_batch(int batch, bool quick)
{
	char out_val[160];
	char tmp_val[160];
	char which;
	int mut_max = 10;
	spell *ret = NULL;
	bool flag, redraw;

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	if (spell_num < (batch + 1) * 10)
	{
		mut_max = spell_num - batch * 10;
	}

	if (quick)
	{
		/* Save screen */
		screen_save();

		/* Show list */
		redraw = TRUE;
	}

	/* Get a spell from the user */
	while (!flag)
	{
		/* Display */
		if (redraw)
		{
			/* Display list */
			print_spell_batch(batch, mut_max);
		}

		/* Show choices */
		sprintf(out_val, "%c-%c,", I2A(0), I2A(mut_max - 1));

		/* Indicate redraw */
		if (!redraw) strcat(out_val, " * to see,");

		/* Extra commands */
		strcat(out_val, " / to rename, - to comment,");

		/* Finish the prompt */
		strcat(out_val, " ESC");

		/* Build the prompt */
		sprintf(tmp_val, "(%s) Select a power: ", out_val);

		/* Show the prompt */
		prt(tmp_val, 0, 0);

		/* Get a key */
		which = inkey();

		/* Parse it */
		switch (which)
		{
			case ESCAPE:
			{
				flag = TRUE;
				break;
			}
			
			case '*':
			case '?':
			case ' ':
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
				}

				break;
			}

			case '\n':
			case '\r':
			{
				if (mut_max == 1)
				{
					ret = &spells[batch * 10];
					flag = TRUE;
					break;
				}
			}

			case '/':
			{
				prt("Rename which power: ", 0, 0);
				which = tolower(inkey());
	
				if (islower(which) && A2I(which) <= mut_max)
				{
					get_string("Name this power: ",
						spells[batch * 10 + A2I(which)].name, 29);
				}
				else
				{
					bell();
				}
	
				break;
			}

			case '-':
			{
				prt("Comment which power: ", 0, 0);
				which = tolower(inkey());
	
				if (islower(which) && A2I(which) <= mut_max)
				{
					get_string("Comment this power: ",
						spells[batch * 10 + A2I(which)].desc, 29);
				}
				else
				{
					bell();
				}
	
				break;
			}

			default:
			{
				which = tolower(which);
				if (islower(which) && A2I(which) < mut_max)
				{
					ret = &spells[batch * 10 + A2I(which)];
					flag = TRUE;
				}
				else
				{
					bell();
				}

				break;
			}
		}
	}

	/* Restore the screen */
	if (redraw)
	{
		/* Load screen */
		screen_load();
	}

	/* Clear the prompt */
	prt("", 0, 0);

	return ret;
}


/* 
 * Check to see if the player has the right spellbook. 
 */
static bool check_spellbook(void)
{
	object_type *o_ptr;

	for (o_ptr = inventory; o_ptr != NULL; o_ptr = o_ptr->next)
	{
		if (o_ptr->k_idx && o_ptr->tval == TV_SPELLBOOK &&
			o_ptr->sval == cp_ptr->spell_book) return TRUE;
	}

	return FALSE;
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
	char tmp[160];
	char which;
	int batch;
	int batch_max = (spell_num - 1) / 10;

	if (spell_num == 0)
	{
		mprint(MSG_TEMP, "There are no spells you can browse.");
		return;
	}

	/* Build a prompt */
	sprintf(tmp, "(a-%c) Select batch of powers: ", I2A(batch_max));

	while (TRUE)
	{
		/* Get a command, or cancel */
		if (!get_com(tmp, &which))
		{
			return;
		}

		/* Automatically select the first batch */
		if ((which == '\n') || (which == '\r'))
		{
			if (batch_max > 0)
			{
				bell();
				continue;
			}

			batch = 0;
			break;
		}

		which = tolower(which);
		if (islower(which) && (A2I(which) <= batch_max))
		{
			batch = A2I(which);
			break;
		}
		else
		{
			bell();
		}
	}

	while (TRUE)
	{
		/* Select a spell */
		spell *s_ptr = select_spell_from_batch(batch, TRUE);

		/* Cancel */
		if (s_ptr == NULL)
			break;

		if (s_ptr->unknown)
		{
			mprint(MSG_TEMP, "Spell unknown.");
		}
		else if (s_ptr->desc[0])
		{
			mformat(MSG_TEMP, "Spell info: %s.", s_ptr->desc);
		}
		else
		{
			mprint(MSG_TEMP, "No spell info.");
		}
	}
}


/*
 * Study a book to gain a new spell/prayer
 *
 * Added building flag -KMW-
 */
void do_cmd_study(void)
{
	spell *s_ptr;

	if (!(p_ptr->new_spells))
	{
		msg_print("You cannot learn any new powers!");
		msg_print(NULL);
		return;
	}

	if (!cp_ptr->magic_innate && !check_spellbook())
	{
		msg_print("You need a spellbook to learn spells!");
		msg_print(NULL);
		return;
	}

	while (TRUE)
	{
		s_ptr = select_spell(TRUE);

		if (s_ptr == NULL)
			return;

		if (s_ptr->unknown && s_ptr->level <= p_ptr->lev)
		{
			break;
		}
		else if (!s_ptr->unknown)
		{
			msg_print("You already know that spell.");
			msg_print(NULL);
		}
		else
		{
			msg_print("That spell is incomprehensible.");
			msg_print(NULL);
		}
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Flag it as learned. */
	s_ptr->unknown = FALSE;

	/* Mention the result */
	mformat(MSG_BONUS, "You have learned the power of %s.", s_ptr->name);
	msg_print(NULL);

	/* Sound */
	sound(SOUND_STUDY);

	/* One less spell available */
	p_ptr->new_spells--;

	/* Message if needed */
	if (p_ptr->new_spells)
	{
		/* Message */
		mformat(MSG_BONUS, "You can learn %d more power%s.",
			p_ptr->new_spells, (p_ptr->new_spells != 1) ? "s" : "");
		msg_print(NULL);
	}

	/* Save the new_spells value */
	p_ptr->old_spells = p_ptr->new_spells;

	/* Redraw Study Status */
	p_ptr->redraw |= (PR_STUDY);
}


/*
 * Give a randomly-generated spell a name.
 * Note that it only describes the first effect!
 */
static void name_spell(spell * s_ptr)
{
	proj_node *pnode = s_ptr->proj_list;
	char buff[30];
	cptr buff2 = "???";

	if (pnode->proj_flags & PROJECT_STOP && pnode->radius == 0)
	{
		buff2 = "Bolt";

	}
	else if (pnode->proj_flags & PROJECT_BEAM)
	{
		buff2 = "Beam";

	}
	else if (pnode->proj_flags & PROJECT_STOP && pnode->radius > 0)
	{
		buff2 = "Ball";

	}
	else if (pnode->proj_flags & PROJECT_BLAST)
	{
		buff2 = "Blast";

	}
	else if (pnode->proj_flags & PROJECT_METEOR_SHOWER)
	{
		buff2 = "Area";

	}
	else if (pnode->proj_flags & PROJECT_VIEWABLE)
	{
		buff2 = "View";
	}

	describe_attack_fully(pnode->attack_kind, buff);
	strnfmt(s_ptr->name, 30, "%s%s - %s",
		((pnode->proj_flags & PROJECT_ETHER) ? "*" : ""), buff2, buff);
}



static const int destructive_attack_types[13] = {
	GF_KILL_WALL,
	GF_KILL_DOOR,
	GF_KILL_TRAP,
	GF_MAKE_WALL,
	GF_MAKE_DOOR,
	GF_MAKE_TRAP,
	GF_WORD_OF_DESTRUCTION,
	GF_QUAKE,
	GF_EARTHQUAKE,
	GF_WALL_TO_CHAOS,
	GF_CHAOS_DESTRUCTION,
	GF_WORD_OF_DESTRUCTION,
	GF_EARTHQUAKE
};

static const int attack_types[29] = {
	GF_ARROW,
	GF_MISSILE,
	GF_MANA,
	GF_HOLY_ORB,
	GF_WATER,
	GF_PLASMA,
	GF_METEOR,
	GF_ICE,
	GF_GRAVITY,
	GF_INERTIA,
	GF_FORCE,
	GF_TIME,
	GF_ACID,
	GF_ELEC,
	GF_FIRE,
	GF_COLD,
	GF_POIS,
	GF_LITE,
	GF_DARK,
	GF_CONFUSION,
	GF_SOUND,
	GF_SHARD,
	GF_NEXUS,
	GF_NETHER,
	GF_CHAOS,
	GF_DISENCHANT,
	GF_QUAKE,
	GF_BRAIN_SMASH,
	GF_MIND_BLAST
};


/* 
 * Generate a new random spell, add it to the list. This takes a player level
 * as an argument.
 */

void spell_generate_new(int plev)
{
	spell *rspell;
	proj_node *pnode;

	int dice, sides, chance, mana, power;

	bool destruc_gen = FALSE;
	bool simple_gen = TRUE;

	if (spell_num == MAX_SPELLS)
		return;

	rspell = &spells[spell_num];

	power = randnor(0, 10);

	dice = plev / 5;
	sides = plev * 2;
	mana = plev;

	/* Make the spell more or less powerful. */
	dice += power / 5;
	sides += power / 2;
	mana += (plev * power) / 8;

	/* Stay within reasonable bounds. */
	if (dice < 1)
		dice = 1;
	if (dice > 10)
		dice = 10;

	if (sides < 1)
		sides = 1;
	if (sides > 100)
		sides = 100;

	if (mana < 1)
		mana = 1;

	rspell->class = 0;
	rspell->level = plev;
	rspell->mana = mana;
	rspell->untried = TRUE;
	rspell->unknown = FALSE;

	/* Fill in the projection info. */
	MAKE(pnode, proj_node);
	rspell->proj_list = pnode;

	/* Spells are always maximally destructive. */
	pnode->proj_flags = PROJECT_KILL | PROJECT_ITEM | PROJECT_GRID;

	chance = randint(100);

	/* Hack -- Always start with Magic Missile or derivative at lev. 1 */
	if (plev == 1 || chance < 25)
	{
		pnode->proj_flags |= PROJECT_STOP;
		pnode->safe = TRUE;
		pnode->dam_dice = dice;
		pnode->dam_sides = sides;

	}
	else if (chance < 50)
	{
		pnode->proj_flags |= PROJECT_BEAM;
		pnode->safe = TRUE;
		pnode->dam_dice = dice;
		pnode->dam_sides = sides;

	}
	else if (chance < 76)
	{
		pnode->proj_flags |= PROJECT_STOP;
		pnode->radius = dice;
		pnode->safe = TRUE;
		pnode->dam_dice = sides;
		pnode->dam_sides = 1;

	}
	else if (chance < 83)
	{
		pnode->proj_flags |= PROJECT_BLAST;
		pnode->radius = sides / 2;
		pnode->safe = TRUE;
		pnode->dam_dice = dice;
		pnode->dam_sides = sides;

		destruc_gen = TRUE;
		simple_gen = FALSE;

	}
	else if (chance < 90)
	{
		pnode->proj_flags |= PROJECT_METEOR_SHOWER;
		pnode->safe = FALSE;
		pnode->dam_dice = dice;
		pnode->dam_sides = sides;

		destruc_gen = TRUE;

	}
	else
	{
		pnode->proj_flags |= PROJECT_VIEWABLE;
		pnode->safe = TRUE;
		pnode->dam_dice = dice;
		pnode->dam_sides = sides;
	}

	/* A 10% chance that the spell goes through walls. */
	if (randint(100) < 10)
	{
		pnode->proj_flags |= PROJECT_ETHER;
	}

	/* Both a destructive and a simple spell requested -- 
	 * pick one or the other. */
	if (destruc_gen && simple_gen)
	{
		if (magik(25))
		{
			simple_gen = FALSE;
		}
		else
		{
			destruc_gen = FALSE;
		}
	}

	/* Pick a simple spell */
	if (simple_gen)
	{
		pnode->attack_kind = attack_types[rand_int(29)];

		/* Pick a destructive spell */
	}
	else
	{
		pnode->attack_kind = destructive_attack_types[rand_int(13)];
	}

	/* Give the spell a name. */
	name_spell(rspell);
	sprintf(rspell->desc, "Damage: %dd%d, Power: %d", dice, sides, power);

	spell_num++;
}




/* 
 * Return percentage chance of spell failure. 
 */

int spell_chance(spell * rspell)
{
	int chance, minfail;


	/* Extract the base spell failure rate */
	chance = rspell->level + 25;

	/* Reduce failure rate by "effective" level adjustment */
	chance -= 3 * (p_ptr->lev - rspell->level);

	/* Reduce failure rate by INT/WIS adjustment */
	chance -= 3 * (adj_mag_stat[p_ptr->stat_ind[cp_ptr->spell_stat]] - 1);

	/* Not enough mana to cast */
	if (rspell->mana > p_ptr->csp)
	{
		chance += 5 * (rspell->mana - p_ptr->csp);
	}

	/* Extract the minimum failure rate */
	if (p_ptr->mega_spells)
	{
		minfail = 0;
	}
	else
	{
		minfail = adj_mag_fail[p_ptr->stat_ind[cp_ptr->spell_stat]];
	}

	/* Minimum failure rate */
	if (chance < minfail)
		chance = minfail;

	/* Stunning makes spells harder */
	if (p_ptr->stun > 50)
		chance += 25;
	else if (p_ptr->stun)
		chance += 15;

	/* Always a 5 percent chance of working */
	if (chance > 95)
		chance = 95;

	/* Return the chance */
	return (chance);
}


/* 
 * Pick a random spell from a menu 
 */
spell *select_spell(bool quick)
{
	char tmp[160];
	char which;
	int batch_max = (spell_num - 1) / 10;

	if (spell_num == 0)
	{
		mprint(MSG_TEMP, "There are no spells you can cast.");
		return NULL;
	}

	if (p_ptr->inside_special == SPECIAL_ARENA)
	{
		mprint(MSG_TEMP, "The arena absorbs all attempted magic!");
		msg_print(NULL);
		return NULL;
	}

	if (p_ptr->confused)
	{
		msg_print("You can't use your powers while confused!");
		return NULL;
	}

	screen_save();

	sprintf(tmp, "(a-%c) Select batch of powers: ", I2A(batch_max));

	prt(tmp, 0, 0);

	while (1)
	{
		which = inkey();

		if (which == ESCAPE)
		{
			screen_load();
			return NULL;

		}
		else if (which == '\r' && batch_max == 0)
		{
			screen_load();
			return select_spell_from_batch(0, quick);

		}
		else
		{
			which = tolower(which);
			if (islower(which) && A2I(which) <= batch_max)
			{
				screen_load();
				return select_spell_from_batch(A2I(which), quick);
			}
			else
			{
				bell();
			}
		}
	}
}


/*
 * Actually cause the spell effect.
 */
bool cause_spell_effect(spell * s_ptr)
{
	proj_node *pnode = s_ptr->proj_list;
	int dir, who;
	int ty = 0, tx = 0;
	bool succ = FALSE;

	while (pnode)
	{
		/* Hack -- Spell needs a target */
		if (pnode->proj_flags & PROJECT_BEAM ||
			pnode->proj_flags & PROJECT_STOP)
		{
			if (!get_aim_dir(&dir))
				return FALSE;

			/* Hack -- Use an actual "target" */
			if ((dir == 5) && target_okay())
			{
				tx = p_ptr->target_col;
				ty = p_ptr->target_row;
			}
			else
			{
				/* Use the given direction */
				ty = p_ptr->py + ddy[dir];
				tx = p_ptr->px + ddx[dir];
			}
		}

		if (pnode->proj_flags & PROJECT_BLAST)
		{
			ty = p_ptr->py;
			tx = p_ptr->px;
		}

		if (pnode->safe)
		{
			who = -1;
		}
		else
		{
			who = -100;
		}

		if (project(who, pnode->radius, ty, tx, damroll(pnode->dam_dice,
					pnode->dam_sides), pnode->attack_kind,
				pnode->proj_flags))
			succ = TRUE;

		pnode = pnode->next;
	}

	return succ;
}




/* 
 * Cast a spell. This is a generic function that can be used for any class
 * safely.
 */

void do_cmd_cast_power(void)
{
	int chance;

	bool failed = FALSE;

	spell *s_ptr;

	if (!cp_ptr->magic_innate && (p_ptr->blind || no_lite()))
	{
		msg_print("You cannot see!");
		return;
	}

	while (TRUE)
	{
		s_ptr = select_spell(FALSE);

		if (s_ptr == NULL)
			return;

		if (s_ptr->unknown)
		{
			mprint(MSG_TEMP, "You don't know that spell.");
			msg_print(NULL);
		}
		else
		{
			break;
		}
	}

	if (s_ptr->mana > p_ptr->csp)
	{
		mprint(MSG_WARNING,
			"You do not have enough mana to use this power.");
		if (!get_check("Attempt it anyway? "))
			return;
	}

	chance = spell_chance(s_ptr);

	p_ptr->energy_use = 100;

	if (randint(100) < chance)
	{
		if (flush_failure)
			flush();

		if (cp_ptr->magic_innate)
		{

			if (randint(100) < p_ptr->skill_sav)
			{
				mprint(MSG_BONUS,
					"You feel out of control for a moment, but the "
					"feeling passes.");
			}
			else
			{
				mprint(MSG_URGENT, "The magic escapes from your control!");
				nasty_side_effect();
			}
			failed = TRUE;

		}
		else
		{
			msg_format("A cloud of %s appears above you.",
				get_random_line("sfail.txt"));
			failed = TRUE;
		}
	}

	/* Use some mana */

	if (s_ptr->mana <= p_ptr->csp)
	{
		p_ptr->csp -= s_ptr->mana;
	}
	else
	{
		int oops = s_ptr->mana - p_ptr->csp;

		/* No mana left */
		p_ptr->csp = 0;
		p_ptr->csp_frac = 0;

		/* Message */
		mprint(MSG_WARNING, "You faint from the effort!");

		/* Hack -- Bypass free action */
		(void) set_paralyzed(p_ptr->paralyzed + randint(5 * oops + 1));

		/* Damage CON (possibly permanently) */
		if (rand_int(100) < 50)
		{
			bool perm = (rand_int(100) < 25);

			/* Message */
			mprint(MSG_URGENT, "You have damaged your health!");

			/* Reduce constitution */
			(void) dec_stat(A_CON, 15 + randint(10), perm);
		}
	}

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_SPELL | PW_PLAYER);

	if (failed)
		return;

	/* The spell is analyzed and an appropriate function is called. */
	cause_spell_effect(s_ptr);

	/* Other stuff */

	if (s_ptr->untried)
	{

		s_ptr->untried = FALSE;
		gain_exp(s_ptr->level * s_ptr->level);
	}
}





/*********************** Mutation functions. ******/

void generate_mutation(void)
{
	int which_mut;
	int which_var;
	int which_flg;
	int i = 0;
	bool looper = TRUE;

	while (looper)
	{
		which_mut = rand_int(MAX_MUTS);
		which_var = which_mut / 32;
		which_flg = 1L << (which_mut % 32);

		switch (which_var)
		{
			case 0:
				if (!(p_ptr->mutations1 & which_flg))
				{
					p_ptr->mutations1 |= which_flg;
					looper = FALSE;
				}
				break;

			case 1:
				if (!(p_ptr->mutations2 & which_flg))
				{
					p_ptr->mutations2 |= which_flg;
					looper = FALSE;
				}
				break;

			case 2:
				if (!(p_ptr->mutations3 & which_flg))
				{
					p_ptr->mutations3 |= which_flg;
					looper = FALSE;
				}
				break;
		}

		if (i > 25)
		{
			msg_print("You keep your previous form.");
			msg_print(NULL);
			return;
		}
		i++;
	}

	p_ptr->update |= PU_BONUS;
	p_ptr->redraw |= (PR_STATE | PR_MAP);
	handle_stuff();

	mprint(MSG_WARNING, mutation_names[which_mut][1]);
	msg_print(NULL);
}

/*
 * Remove a random mutation.
 */

void remove_mutation(void)
{
	int which_mut = 0;
	int which_var;
	int which_flg;
	int i = 0;
	bool looper = TRUE;

	if (p_ptr->mutations1 == 0 && p_ptr->mutations2 == 0 &&
		p_ptr->mutations3 == 0)
		return;

	while (looper)
	{
		which_mut = rand_int(MAX_MUTS);
		which_var = which_mut / 32;
		which_flg = 1L << (which_mut % 32);

		switch (which_var)
		{
			case 0:
				if (p_ptr->mutations1 & which_flg)
				{
					p_ptr->mutations1 &= ~(which_flg);
					looper = FALSE;
				}
				break;

			case 1:
				if (p_ptr->mutations2 & which_flg)
				{
					p_ptr->mutations2 &= ~(which_flg);
					looper = FALSE;
				}
				break;

			case 2:
				if (p_ptr->mutations3 & which_flg)
				{
					p_ptr->mutations3 &= ~(which_flg);
					looper = FALSE;
				}
				break;
		}

		if (i > 25)
		{
			msg_print("You keep your previous form.");
			msg_print(NULL);
			return;
		}
		i++;
	}

	p_ptr->update |= PU_BONUS;
	p_ptr->redraw |= (PR_STATE | PR_MAP);
	handle_stuff();

	mprint(MSG_WARNING, mutation_names[which_mut][2]);
	msg_print(NULL);
}



/*
 * Add the powers with the specified classification to the player's spell list.
 */

void add_powers(byte class)
{
	int i, j = 0;
	spell *s_ptr;
	spell *s2_ptr;

	for (i = 0; i < power_num; i++)
	{
		s_ptr = &powers[i];

		if (s_ptr->class == class)
		{
			s2_ptr = add_new_spell(s_ptr);

			if (s2_ptr)
			{
				s2_ptr->unknown = FALSE;
				j++;
			}
		}
	}

	if (j > 0)
	{
		mformat(MSG_BONUS, "You have gained %d new abilit%s.", j,
			(j > 1 ? "ies" : "y"));
	}
}

/*
 * Remove all spells that are classified a certain way.
 */

void remove_powers(byte class)
{
	int indexes[MAX_SPELLS];
	int i, num = 0;

	for (i = 0; i < spell_num; i++)
	{
		if (spells[i].class == class)
		{
			indexes[num] = i;
			num++;
		}
	}

	if (num)
	{
		remove_spells(indexes, num);

		mformat(MSG_WARNING, "You have lost %d of your abilities.", num);
	}
}
