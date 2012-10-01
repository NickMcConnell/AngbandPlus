/* File: cmd5.c */

/* Warrior probing.  Selection, browsing, learning, and casting of spells 
 * and prayers.  Includes definitions of all spells and prayers.  Shape-
 * shifting and making Athelas.
 *
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"


/*
 * Warriors will eventually learn to pseudo-probe monsters.  If they use 
 * the browse command, give ability information. -LM-
 */
static void warrior_probe_desc(void)
{
	/* Save screen */
	screen_save();

	/* Erase the screen */
	Term_clear();

	/* Title in light blue. */
	c_roff(TERM_L_BLUE, "Warrior Pseudo-Probing Ability:", 5, 75);
	roff("\n", 0, 0); roff("\n", 0, 0);

	/* Print out information text. */
	roff("     Warriors learn to probe monsters at level 35.  This costs nothing except a full turn.  When you reach this level, type 'm', and target the monster you would like to learn more about.  This reveals the monster's race, approximate HPs, and basic resistances.  Be warned:  the information given is not always complete...", 5, 75);
	roff("\n", 0, 0); roff("\n", 0, 0); roff("\n", 0, 0);

	/* The "exit" sign. */
	roff("    (Press any key to continue.)", 5, 75);
	roff("\n", 0, 0);

	/* Wait for it. */
	(void)inkey();

	/* Load screen */
	screen_load();
}

/* 
 * Warriors will eventually learn to pseudo-probe monsters.  This allows 
 * them to better choose between slays and brands.  They select a target, 
 * and receive (slightly incomplete) infomation about racial type, 
 * basic resistances, and HPs. -LM-
 */
static void pseudo_probe(void)
{
	char m_name[80];

	/* Acquire the target monster */
	int m_idx = p_ptr->target_who;
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	int approx_hp;


	/* If no target monster, fail. */
	if (p_ptr->target_who < 1) 
	{
		msg_print("You must actually target a monster.");
		return;
	}

	else
	{
		/* Get "the monster" or "something" */
		monster_desc(m_name, m_ptr, 0x04);

		/* Approximate monster HPs */
		approx_hp = m_ptr->hp - rand_int(m_ptr->hp / 4) + 
			rand_int(m_ptr->hp / 4);

		/* Describe the monster */
		msg_format("%^s has about %d hit points.", m_name, approx_hp);


		/* Learn some flags.  Chance of omissions. */
		if ((r_ptr->flags3 & (RF3_ANIMAL)) && (rand_int(20) != 1))
			l_ptr->flags3 |= (RF3_ANIMAL);
		if ((r_ptr->flags3 & (RF3_EVIL)) && (rand_int(10) != 1))
			l_ptr->flags3 |= (RF3_EVIL);
		if ((r_ptr->flags3 & (RF3_UNDEAD)) && (rand_int(20) != 1))
			l_ptr->flags3 |= (RF3_UNDEAD);
		if ((r_ptr->flags3 & (RF3_DEMON)) && (rand_int(20) != 1))
			l_ptr->flags3 |= (RF3_DEMON);
		if ((r_ptr->flags3 & (RF3_ORC)) && (rand_int(20) != 1))
			l_ptr->flags3 |= (RF3_ORC);
		if ((r_ptr->flags3 & (RF3_TROLL)) && (rand_int(20) != 1))
			l_ptr->flags3 |= (RF3_TROLL);
		if ((r_ptr->flags3 & (RF3_GIANT)) && (rand_int(10) != 1))
			l_ptr->flags3 |= (RF3_GIANT);
		if ((r_ptr->flags3 & (RF3_DRAGON)) && (rand_int(20) != 1))
			l_ptr->flags3 |= (RF3_DRAGON);
		if ((r_ptr->flags3 & (RF3_IM_ACID)) && (rand_int(5) != 1))
			l_ptr->flags3 |= (RF3_IM_ACID);
		if ((r_ptr->flags3 & (RF3_IM_ELEC)) && (rand_int(5) != 1))
			l_ptr->flags3 |= (RF3_IM_ELEC);
		if ((r_ptr->flags3 & (RF3_IM_FIRE)) && (rand_int(5) != 1))
			l_ptr->flags3 |= (RF3_IM_FIRE);
		if ((r_ptr->flags3 & (RF3_IM_COLD)) && (rand_int(5) != 1))
			l_ptr->flags3 |= (RF3_IM_COLD);
		if ((r_ptr->flags3 & (RF3_IM_POIS)) && (rand_int(5) != 1))
			l_ptr->flags3 |= (RF3_IM_POIS);

		/* Confirm success. */
		msg_print("You feel you know more about this monster...");

		/* Update monster recall window */
		if (p_ptr->monster_race_idx == m_ptr->r_idx)
		{
			/* Window stuff */
			p_ptr->window |= (PW_MONSTER);
		}
	}
}



/* 
 * Alter player's shape.  Taken from Sangband.
 */
void shapechange(s16b shape)
{
	char *shapedesc = "";

	/* Wonder Twin powers -- Activate! */
	p_ptr->schange = shape;
	p_ptr->update |= PU_BONUS;

	switch (shape)
	{
		case SHAPE_MOUSE:
			shapedesc = "mouse";
			break;
		case SHAPE_FERRET:
			shapedesc = "ferret";
			break;
		case SHAPE_HOUND:
			shapedesc = "hound";
			break;
		case SHAPE_GAZELLE:
			shapedesc = "gazelle";
			break;
		case SHAPE_LION:
			shapedesc = "lion";
			break;
		case SHAPE_ENT:
			shapedesc = "elder ent";
			break;
		case SHAPE_BAT:
			shapedesc = "bat";
			break;
		case SHAPE_WEREWOLF:
			shapedesc = "werewolf";
			break;
		case SHAPE_VAMPIRE:
			shapedesc = "vampire";
			break;
		case SHAPE_WYRM:
			shapedesc = "wyrm";
			break;
		default:
			msg_print("You return to your normal form.");
			break;
	}

	if (shape)
	{
		msg_format("You assume the form of a %s.", shapedesc);
		msg_print("Your equipment merges into your body.");
	}

	/* Recalculate mana. */
	p_ptr->update |= (PU_MANA);

	/* Show or hide shapechange on main window. */
	p_ptr->redraw |= (PR_SHAPE);
}


/*
 * Choose a paladin elemental attack. -LM-
 */
static void choose_ele_attack(void)
{
	int num;

	char choice;

	/* Save screen */
	screen_save();

	prt("", 14, 0);
	prt("        Choose a temporary elemental brand ", 1, 14);

	num = (p_ptr->lev - 20) / 7;

	c_prt(TERM_RED,    "        a) Fire Brand", 2, 14);
				  
	if (num >= 2) c_prt(TERM_L_WHITE,"        b) Cold Brand", 3, 14);
	else prt("", 3, 14);

	if (num >= 3) c_prt(TERM_L_DARK, "        c) Acid Brand", 4, 14);
	else prt("", 4, 14);

	if (num >= 4) c_prt(TERM_BLUE,   "        d) Elec Brand", 5, 14);
	else prt("", 5, 14);

	prt("", 6, 14);
	prt("", 7, 14);
	prt("", 8, 14);
	prt("", 9, 14);

	choice = inkey();

	if ((choice == 'a') || (choice == 'A')) 
		set_ele_attack(ATTACK_FIRE, 200);
	else if (((choice == 'b') || (choice == 'B')) && (num >= 2))
		set_ele_attack(ATTACK_COLD, 200);
	else if (((choice == 'c') || (choice == 'C')) && (num >= 3))
		set_ele_attack(ATTACK_ACID, 200);
	else if (((choice == 'd') || (choice == 'D')) && (num >= 4))
		set_ele_attack(ATTACK_ELEC, 200);
	else msg_print("You cancel the temporary branding.");

	/* Load screen */
	screen_load();
}


/* 
 * Hack -- The Athelas-creation code. -LM-
 */
void create_athelas(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
	object_type *i_ptr;
	object_type object_type_body;

	/* Get local object */
	i_ptr = &object_type_body;

	/* Hack -- Make some Athelas, identify it, and drop it near the player. */
	object_prep(i_ptr, lookup_kind(TV_FOOD, SV_FOOD_ATHELAS));

	/* Prevent money-making. */
	i_ptr->discount = 80;

	object_aware(i_ptr);
	object_known(i_ptr);
	drop_near(i_ptr, -1, py, px);
}


/*
 * Controlled teleportation.  -LM-
 * Idea from PsiAngband, through Zangband.
 */
void dimen_door(void)
{
	int ny;
	int nx;
	bool okay;
	bool old_expand_look = expand_look;

	expand_look = TRUE;
	okay = target_set_interactive(TARGET_LOOK | TARGET_GRID);
	expand_look = old_expand_look;
	if (!okay) return;

	/* grab the target coords. */
	ny = p_ptr->target_row;
	nx = p_ptr->target_col;

	/* Test for empty floor, forbid vaults or too large a
	 * distance, and insure that this spell is never certain.
	 */
	if (!cave_empty_bold(ny,nx) || (cave_info[ny][nx] & CAVE_ICKY) ||
		(distance(ny,nx,p_ptr->py,p_ptr->px) > 25) || 
		(rand_int(p_ptr->lev) == 0))
	{
		msg_print("You fail to exit the astral plane correctly!");
		p_ptr->energy -= 50;
		teleport_player(15);
	}

	/* Controlled teleport. */
	else teleport_player_to(ny,nx);
}


/*
 * Rebalance Weapon.  This is a rather powerful spell, because it can be 
 * used with any non-artifact throwing weapon, including ego-items.  It is 
 * therefore high-level, and curses weapons on failure.  Do not give Assas-
 * sins "Break Curse". -LM-
 */
static void rebalance_weapon(void)
{
	u32b f1, f2, f3;

	object_type *o_ptr;
	char o_name[120];

	/* Select the wielded melee weapon */
	o_ptr = &inventory[INVEN_WIELD];

	/* Nothing to rebalance */
	if (!o_ptr->k_idx) 
	{
		msg_print("You are not wielding any melee weapon.");
		return;
	}
	/* Artifacts not allowed. */
	if (o_ptr->name1)
	{
		msg_print("Artifacts cannot be rebalanced.");
		return;
	}

	/* Extract some flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Not a throwing weapon. */
	if (!(f1 & (TR1_THROWING))) 
	{
		msg_print("The melee weapon you are wielding is not designed for throwing.");
		return;
	}

	/* 20% chance to curse weapon. */
	else if (randint(5) == 1)
	{
		/* Description */
		object_desc(o_name, o_ptr, FALSE, 0);

		/* Light curse and lower to_h and to_d by 2 to 5 each. */
		
		o_ptr->ident |= (IDENT_CURSED);
		o_ptr->to_h -= 2 + rand_int(4);
		o_ptr->to_d -= 2 + rand_int(4);

		/* Describe */
		msg_format("Oh no!  A dreadful black aura surrounds your %s!", o_name);

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);
	}

	/* Rebalance. */
	else 
	{
		/* Grant perfect balance. */
		o_ptr->xtra1 = OBJECT_XTRA_TYPE_BALANCE;
		o_ptr->xtra2 = rand_int(OBJECT_XTRA_SIZE_BALANCE);

		/* Description */
		object_desc(o_name, o_ptr, FALSE, 0);

		/* Describe */
		msg_format("Your %s gleams steel blue!", o_name);

		/* Prevent money-making. */
		o_ptr->discount = 80;
	}
}


/*
 * Allow user to choose a spell/prayer from the given book.
 *
 * If a valid spell is chosen, saves it in '*sn' and returns TRUE
 * If the user hits escape, returns FALSE, and set '*sn' to -1
 * If there are no legal choices, returns FALSE, and sets '*sn' to -2
 *
 * The "prompt" should be "cast", "recite", "study", or "browse".
 * The "known" should be TRUE for cast/pray, FALSE for study
 */
static int get_spell(int *sn, cptr prompt, int tval, int sval, bool known)
{
	int i;

	int spell = -1;

	int first_spell, after_last_spell;

	int ver;

	bool flag, redraw, okay;
	char choice;

	magic_type *s_ptr;

	char out_val[160];

	cptr p = "";


#ifdef ALLOW_REPEAT /* TNB */

	/* Get the spell, if available */
	if (repeat_pull(sn)) 
	{
		/* Find the array index of the spellbook's first spell. */
		first_spell = mp_ptr->book_start_index[sval];
	
		/* Find the first spell in the next book. */
		after_last_spell = mp_ptr->book_start_index[sval+1];

		/* Verify the spell is in this book */
		if (((*sn) >= first_spell) && ((*sn) < after_last_spell))
		{
			/* Verify the spell is okay */
			if (spell_okay(*sn, known)) 
			{
				/* Success */
				return (TRUE);
			}
		}
	}

#endif /* ALLOW_REPEAT */

	/* Determine the magic description, for color. */
	if (mp_ptr->spell_book == TV_MAGIC_BOOK) p = "spell";
	if (mp_ptr->spell_book == TV_PRAYER_BOOK) p = "prayer";
	if (mp_ptr->spell_book == TV_DRUID_BOOK) p = "druidic lore";
	if (mp_ptr->spell_book == TV_NECRO_BOOK) p = "ritual";

	/* Assume no usable spells */
	okay = FALSE;

	/* Assume no spells available */
	(*sn) = -2;


	/* Find the array index of the spellbook's first spell. */
	first_spell = mp_ptr->book_start_index[sval];

	/* Find the first spell in the next book. */
	after_last_spell = mp_ptr->book_start_index[sval+1];

	/* Check for "okay" spells */
	for (i = first_spell; i < after_last_spell; i++)
	{
		/* Look for "okay" spells */
		if (spell_okay(i, known)) okay = TRUE;
	}

	/* No "okay" spells */
	if (!okay) return (FALSE);

	/* Assume cancelled */
	*sn = (-1);

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;


	/* Build a prompt (accept all spells) */
	strnfmt(out_val, 78, "(%^ss %c-%c, *=List, ESC=exit) %^s which %s? ",
	        p, I2A(0), I2A(after_last_spell - first_spell - 1), prompt, p);

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
				print_spells(tval, sval, 1, 14);
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
		if ((i < 0) || (i >= after_last_spell - first_spell))
		{
			bell("Illegal spell choice!");
			continue;
		}

		/* Convert spellbook number to spell index. */
		spell = i + first_spell;

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
			        prompt, spell_names[s_ptr->index],
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
	}


	/* Abort if needed */
	if (!flag) return (FALSE);

	/* Save the choice */
	(*sn) = spell;

#ifdef ALLOW_REPEAT /* TNB */
	repeat_push(*sn);

#endif /* ALLOW_REPEAT */

	/* Success */
	return (TRUE);
}


/*
 * Peruse the spells/prayers in a Book, showing "spell tips" as 
 * requested. -LM-
 *
 * Note that browsing is allowed while confused or blind,
 * and in the dark, primarily to allow browsing in stores.
 */
void do_cmd_browse(void)
{
	int item, spell, lines;

	object_type *o_ptr;

	magic_type *s_ptr;

	cptr q = "";
	cptr s = "";

	/* Forbid illiterates to read spellbooks. */
	if (!mp_ptr->spell_book)
	{
		/* Warriors will eventually learn to pseudo-probe monsters. */
		if (p_ptr->pclass == CLASS_WARRIOR) warrior_probe_desc();

		else msg_print("You cannot read books!");
		return;
	}

	/* Restrict choices to "useful" books */
	item_tester_tval = mp_ptr->spell_book;

	/* Get a realm-flavored description. */
	if (mp_ptr->spell_book == TV_MAGIC_BOOK) 
	{
		q = "Browse which magic book? ";
		s = "You have no magic books that you can read.";
	}
	if (mp_ptr->spell_book == TV_PRAYER_BOOK)
	{
		q = "Browse which holy book? ";
		s = "You have no holy books that you can read.";
	}
	if (mp_ptr->spell_book == TV_DRUID_BOOK)
	{
		q = "Browse which stone of lore? ";
		s = "You have no stones that you can read.";
	}
	if (mp_ptr->spell_book == TV_NECRO_BOOK)
	{
		q = "Browse which tome? ";
		s = "You have no tomes that you can read.";
	}

	/* Get an item */
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

	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();


	/* Save screen */
	screen_save();

	/* Display the spells */
	print_spells(o_ptr->tval, o_ptr->sval, 1, 14);

	/* Prompt for a command */
	put_str("(Browsing) Choose a spell, or ESC: ", 0, 0);


	/* Hack - Determine how far from the top of the screen the spell list 
	 * extends by counting spells, and adding space for name, etc.
	 */
	lines = mp_ptr->book_start_index[o_ptr->sval + 1] - 
		mp_ptr->book_start_index[o_ptr->sval] + 3;


	/* Keep browsing spells.  Exit browsing on cancel. */
	while(TRUE)
	{
		/* Ask for a spell, allow cancel */
		if (!get_spell(&spell, "browse", o_ptr->tval, o_ptr->sval, TRUE))
		{
			/* If cancelled, leave immediately. */
			if (spell == -1) break;

			/* Notify that there's nothing to see, and wait. */
			c_put_str(TERM_SLATE, "No spells to browse     ", 0, 11);

			/* Any key cancels if no spells are available. */
			if (inkey()) break;
		}				  

		/* Clear lines, position cursor  (really should use strlen here) */
		Term_erase(14, lines + 2, 255);
		Term_erase(14, lines + 1, 255);
		Term_erase(14, lines, 255);

		/* Access the spell */
		s_ptr = &mp_ptr->info[spell];

		/* Display that spell's information. */
		c_roff(TERM_L_BLUE, spell_tips[s_ptr->index], 16, 0);
	}

	/* Load screen */
	screen_load();
}




/*
 * Study a book to gain a new spell/prayer
 */
void do_cmd_study(void)
{
	int i, item;

	int first_spell, after_last_spell;

	magic_type *s_ptr;

	int spell = -1;

	cptr p = "";
	cptr r = "";

	cptr q = "";
	cptr s = "";

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

	/* Determine magic description. */
	if (mp_ptr->spell_book == TV_MAGIC_BOOK) p = "spell";
	if (mp_ptr->spell_book == TV_PRAYER_BOOK) p = "prayer";
	if (mp_ptr->spell_book == TV_DRUID_BOOK) p = "druidic lore";
	if (mp_ptr->spell_book == TV_NECRO_BOOK) p = "ritual";

	/* Determine spellbook description. */
	if (mp_ptr->spell_book == TV_MAGIC_BOOK) r = "magic book";
	if (mp_ptr->spell_book == TV_PRAYER_BOOK) r = "holy book";
	if (mp_ptr->spell_book == TV_DRUID_BOOK) r = "stone";
	if (mp_ptr->spell_book == TV_NECRO_BOOK) r = "tome";


	if (!(p_ptr->new_spells))
	{
		msg_format("You cannot learn any new %ss.", p, 
			(mp_ptr->spell_book == TV_DRUID_BOOK) ? "" : "s");
		return;
	}

	/* Restrict choices to "useful" books */
	item_tester_tval = mp_ptr->spell_book;

	/* Get a realm-flavored description. */
	if (mp_ptr->spell_book == TV_MAGIC_BOOK) 
	{
		q = "Study which magic book? ";
		s = "You have no magic books that you can read.";
	}
	if (mp_ptr->spell_book == TV_PRAYER_BOOK)
	{
		q = "Study which holy book? ";
		s = "You have no holy books that you can read.";
	}
	if (mp_ptr->spell_book == TV_DRUID_BOOK)
	{
		q = "Study which stone of lore? ";
		s = "You have no stones that you can read.";
	}
	if (mp_ptr->spell_book == TV_NECRO_BOOK)
	{
		q = "Study which tome? ";
		s = "You have no tomes that you can read.";
	}

	/* Get an item */
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


	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();


	/* All but Priests -- Learn a selected spell */
	if (mp_ptr->spell_book != TV_PRAYER_BOOK)
	{
		/* Ask for a spell, allow cancel */
		if (!get_spell(&spell, "study", o_ptr->tval, o_ptr->sval, FALSE) && 
			(spell == -1)) return;
	}

	/* Priest -- Learn a random prayer */
	if (mp_ptr->spell_book == TV_PRAYER_BOOK)
	{
		int k = 0;

		int gift = -1;

		/* Find the array index of the spellbook's first prayer. */
		first_spell = mp_ptr->book_start_index[o_ptr->sval];

		/* Find the first prayer in the next book. */
		after_last_spell = mp_ptr->book_start_index[o_ptr->sval+1];

		/* Pick an legal, unknown prayer at random. */
		for (spell = first_spell; spell < after_last_spell; spell++)
		{
			/* Skip non "okay" prayers */
			if (!spell_okay(spell, FALSE)) continue;

			/* Apply the randomizer */
			if ((++k > 1) && (rand_int(k) != 0)) continue;

			/* Track it */
			gift = spell;
		}

		/* Accept gift */
		spell = gift;
	}

	/* Nothing to study */
	if (spell < 0)
	{
		/* Message */
		msg_format("You cannot learn any %s%s that %s.", p, 
			(mp_ptr->spell_book == TV_DRUID_BOOK) ? " from" : "s in", r);

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

	/* Access the spell */
	s_ptr = &mp_ptr->info[spell];

	/* Mention the result */
	message_format(MSG_STUDY, 0, "You have learned the %s of %s.",
	           p, spell_names[s_ptr->index]);

	/* Sound */
	sound(SOUND_STUDY);

	/* One less spell available */
	p_ptr->new_spells--;

	/* Message if needed */
	if (p_ptr->new_spells)
	{
		/* Message */
		msg_format("You can learn %d more %s%s.", p_ptr->new_spells, p, 
			((p_ptr->new_spells != 1) && 
			(!mp_ptr->spell_book != TV_DRUID_BOOK)) ? "s" : "");
	}

	/* Save the new_spells value */
	p_ptr->old_spells = p_ptr->new_spells;

	/* Redraw Study Status */
	p_ptr->redraw |= (PR_STUDY);

}



/*
 * Cast a spell or pray a prayer.
 */
void do_cmd_cast_or_pray(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int item, spell, dir;
	int chance, beam;
	int shape = 0;

	int plev = p_ptr->lev;

	object_type *o_ptr;

	magic_type *s_ptr;

	cptr p = "";
	cptr r = "";
	cptr t = "";

	cptr q = "";
	cptr s = "";


	/* Require spell ability. */
	if (!mp_ptr->spell_book)
	{
		/* Warriors will eventually learn to pseudo-probe monsters. */
		if (p_ptr->pclass == CLASS_WARRIOR) 
		{
			if (p_ptr->lev < 35) 
				msg_print("You do not know how to probe monsters yet.");
			else if ((p_ptr->confused) || (p_ptr->image))
				msg_print("You feel awfully confused.");
			else 
			{
				/* Get a target. */
				msg_print("Target a monster to probe.");
				if (!get_aim_dir(&dir)) return;

				/* Low-level probe spell. */
				pseudo_probe();

				/* Take a turn */
				p_ptr->energy_use = 100;
			}
		}
		else msg_print("You know no magical realm.");

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

	/* Determine magic description. */
	if (mp_ptr->spell_book == TV_MAGIC_BOOK) p = "spell";
	if (mp_ptr->spell_book == TV_PRAYER_BOOK) p = "prayer";
	if (mp_ptr->spell_book == TV_DRUID_BOOK) p = "druidic lore";
	if (mp_ptr->spell_book == TV_NECRO_BOOK) p = "ritual";

	/* Determine spellbook description. */
	if (mp_ptr->spell_book == TV_MAGIC_BOOK) r = "magic book";
	if (mp_ptr->spell_book == TV_PRAYER_BOOK) r = "holy book";
	if (mp_ptr->spell_book == TV_DRUID_BOOK) r = "stone";
	if (mp_ptr->spell_book == TV_NECRO_BOOK) r = "tome";

	/* Determine method description. */
	if (mp_ptr->spell_book == TV_MAGIC_BOOK) t = "cast";
	if (mp_ptr->spell_book == TV_PRAYER_BOOK) t = "pray";
	if (mp_ptr->spell_book == TV_DRUID_BOOK) t = "use";
	if (mp_ptr->spell_book == TV_NECRO_BOOK) t = "perform";


	/* Restrict choices to spell books of the player's realm. */
	item_tester_tval = mp_ptr->spell_book;


	/* Get a realm-flavored description. */
	if (mp_ptr->spell_book == TV_MAGIC_BOOK) 
	{
		q = "Use which magic book? ";
		s = "You have no magic books that you can use.";
	}
	if (mp_ptr->spell_book == TV_PRAYER_BOOK)
	{
		q = "Use which holy book? ";
		s = "You have no holy books that you can use.";
	}
	if (mp_ptr->spell_book == TV_DRUID_BOOK)
	{
		q = "Use which stone of lore? ";
		s = "You have no stones that you can use.";
	}
	if (mp_ptr->spell_book == TV_NECRO_BOOK)
	{
		q = "Use which tome? ";
		s = "You have no tomes that you can use.";
	}

	/* Get an item */
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

	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();


	/* Ask for a spell */
	if (!get_spell(&spell, t, o_ptr->tval, o_ptr->sval, TRUE))
	{
		if (spell == -2) 
		{
			msg_format("You don't know any %s%s in that %s.", p, 
				(mp_ptr->spell_book == TV_DRUID_BOOK) ? "" : "s", r);
		}
		return;
	}


	/* Access the spell */
	s_ptr = &mp_ptr->info[spell];


	/* Verify "dangerous" spells */
	if (s_ptr->smana > p_ptr->csp)
	{
		/* Warning */
		msg_format("You do not have enough mana to %s this %s.", 
			t, p);

		/* Verify */
		if (!get_check("Attempt it anyway? ")) return;
	}


	/* Spell failure chance */
	chance = spell_chance(spell);

	/* Failed spell */
	if (rand_int(100) < chance)
	{
		if (flush_failure) flush();
		if (mp_ptr->spell_book == TV_MAGIC_BOOK) 
			msg_print("You failed to get the spell off!");
		if (mp_ptr->spell_book == TV_PRAYER_BOOK) 
			msg_print("You lost your concentration!");
		if (mp_ptr->spell_book == TV_DRUID_BOOK)
			msg_print("You lost your concentration!");
		if (mp_ptr->spell_book == TV_NECRO_BOOK)
			msg_print("You perform the ritual incorrectly!");
	}

	/* Process spell */
	else
	{
		/* Hack -- higher chance of "beam" instead of "bolt" for mages 
		 * and necros.
		 */
		beam = (((p_ptr->pclass == CLASS_MAGE) || 
			(p_ptr->pclass == CLASS_NECRO)) ? plev : (plev / 2));



		/* Spell Effects.  Spells are mostly listed by realm, each using a 
		 * block of 64 spell slots, but there are a certain number of spells 
		 * that are used by more than one realm (this allows more neat class-
		 * specific magics)
		 */
		switch (s_ptr->index)
		{
			/* Sorcerous Spells */

			case 0:	/* Magic Missile */
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_MISSILE, dir,
				                  damroll(2, 4 + plev / 10));
				break;
			}
			case 1:	/* Detect Monsters */
			{
				(void)detect_monsters_normal(DETECT_RAD_DEFAULT, TRUE);
				break;
			}
			case 2:	/* Phase Door */
			{
				teleport_player(10);
				break;
			}
			case 3:	/* Light Area */
			{
				(void)lite_area(damroll(2, 1 + (plev / 5)), (plev / 10) + 1);
				break;
			}
			case 4:	/* Combat Poison */
			{
				(void)set_poisoned(p_ptr->poisoned / 2);
				break;
			}
			case 5:	/* Cure Light Wounds */
			{
				(void)hp_player(damroll(2, plev / 4 + 5));
				(void)set_cut(p_ptr->cut - 15);
				break;
			}
			case 6:	/* Rogue Spell: Detect Treasure */
			{
			        /* Hack - 'show' effected region only with
				 * the first detect */
				(void)detect_treasure(DETECT_RAD_DEFAULT, TRUE);
				(void)detect_objects_gold(DETECT_RAD_DEFAULT, FALSE);
				break;
			}
			case 7:	/* Rogue Spell:  Detect Objects */
			{
				(void)detect_objects_normal(DETECT_RAD_DEFAULT, TRUE);
				break;
			}
			case 8:	/* Stinking Cloud */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_POIS, dir, 5 + plev / 3, 2, FALSE);
				break;
			}
			case 9:	/* Confuse Monster */
			{
				if (!get_aim_dir(&dir)) return;
				(void)confuse_monster(dir, plev + 10);
				break;
			}
			case 10:	/* Lightning Bolt */
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_ELEC, dir,
				                  damroll(2+((plev-5)/5), 8));
				break;
			}
			case 11:	/* Door Destruction */
			{
				(void)destroy_doors_touch();
				break;
			}
			case 12:	/* Sleep Monster */
			{
				if (!get_aim_dir(&dir)) return;
				(void)sleep_monster(dir, plev + 10);
				break;
			}
			case 13:	/* Cure Poison */
			{
				(void)set_poisoned(0);
				break;
			}
			case 14:	/* Teleport Self */
			{
				teleport_player(50 + plev * 2);
				break;
			}
			case 15:	/* Spear of Light */
			{
				if (!get_aim_dir(&dir)) return;
				msg_print("A line of blue shimmering light appears.");
				lite_line(dir);
				break;
			}
			case 16:	/* Recharge Item I */
			{
				(void)recharge(85);
				break;
			}
			case 17:	/* Cone of Cold */
			{
				if (!get_aim_dir(&dir)) return;
				fire_arc(GF_COLD, dir, 20 + plev, 3 + plev / 10, 45);
				break;
			}
			case 18:	/* Satisfy Hunger */
			{
				(void)set_food(PY_FOOD_MAX - 1);
				break;
			}
			case 19:	/* Magic Disarm */
			{
				if (!get_aim_dir(&dir)) return;
				(void)disarm_trap(dir);
				break;
			}
			case 20:	/* Polymorph Other */
			{
				if (!get_aim_dir(&dir)) return;
				(void)poly_monster(dir);
				break;
			}
			case 21:	/* Identify */
			{
				(void)ident_spell();
				break;
			}
			case 22:	/* Sleep Monsters */
			{
				(void)sleep_monsters(plev + 10);
				break;
			}
			case 23:	/* Fire Bolt */
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_FIRE, dir,
				                  damroll(7+((plev-5)/5), 8));
				break;
			}
			case 24:	/* Slow Monster */
			{
				if (!get_aim_dir(&dir)) return;
				(void)slow_monster(dir, plev + 10);
				break;
			}
			case 25:	/* Tap magical energy */
			{
				tap_magical_energy();
				break;
			}
			case 26:	/* Frost Ball */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_COLD, dir, 30 + plev, 2, FALSE);
				break;
			}
			case 27:	/* Recharge Item II */
			{
				(void)recharge(150);
				break;
			}
			case 28:	/* Teleport Other */
			{
				if (!get_aim_dir(&dir)) return;
				(void)teleport_monster(dir);
				break;
			}
			case 29:	/* Haste Self */
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
			case 30:	/* Fire Ball */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_FIRE, dir, 55 + plev, 2, FALSE);
				break;
			}
			case 31:	/* Hold Monsters */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_HOLD, dir, 0, 2, FALSE);
				break;
			}
			case 32:	/* Word of Destruction */
			{
				destroy_area(py, px, 15, TRUE);
				break;
			}
			case 33:	/* Resist Fire */
			{
				(void)set_oppose_fire(p_ptr->oppose_fire + randint(plev) + plev);
				break;
			}
			case 34:	/* Resist Cold */
			{
				(void)set_oppose_cold(p_ptr->oppose_cold + randint(plev) + plev);
				break;
			}
			case 35:	/* Resist Acid */
			{
				(void)set_oppose_acid(p_ptr->oppose_acid + randint(plev) + plev);
				break;
			}

			case 36:	/* Resist Poison */
			{
				(void)set_oppose_pois(p_ptr->oppose_pois + randint(plev) + plev);
				break;
			}
			case 37:	/* Resistance */
			{
				(void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20);
				(void)set_oppose_elec(p_ptr->oppose_elec + randint(20) + 20);
				(void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
				(void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
				(void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20);
				break;
			}
			case 38:	/* Door Creation */
			{
				(void)door_creation();
				break;
			}
			case 39:	/* Stair Creation */
			{
				(void)stair_creation();
				break;
			}
			case 40:	/* Teleport Level */
			{
				(void)teleport_player_level();
				break;
			}
			case 41:	/* Word of Recall */
			{
#if 1
				word_recall(rand_int(20) + 15);
#else
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
				p_ptr->redraw |= PR_STATUS;
#endif
				break;
			}
			case 42: /* Dimension Door. */
			{
				msg_print("Choose a location to teleport to.");
				msg_print(NULL);
				dimen_door();
				break;
			}
			case 43:	/* Detect Evil */
			{
				(void)detect_monsters_evil(DETECT_RAD_DEFAULT, TRUE);
				break;
			}
			case 44:	/* Detect Enchantment */
			{
				(void)detect_objects_magic(DETECT_RAD_DEFAULT, TRUE);
				break;
			}
			case 45:	/* Earthquake */
			{
				earthquake(py, px, 10, FALSE);
				break;
			}
			case 46:	/* Beguiling */
			{
				(void)slow_monsters(5 * plev / 3);
				(void)confu_monsters(5 * plev / 3);
				(void)sleep_monsters(5 * plev / 3);
				break;
			}
			case 47:	/* Starburst */
			{
				fire_sphere(GF_LITE, 0,
				          5 * plev / 2, plev / 12, 20);
				break;
			}
			case 48:	/* Clear Mind */
			{
				if (p_ptr->csp < p_ptr->msp)
				{
					p_ptr->csp += 1 + plev / 12;
					p_ptr->csp_frac = 0;
					if (p_ptr->csp > p_ptr->msp) (p_ptr->csp = p_ptr->msp);
					msg_print("You feel your head clear a little.");
					p_ptr->redraw |= (PR_MANA);
					p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
				}
				break;
			}
			case 49:	/* Shield */
			{
				if (!p_ptr->shield)
				{
					(void)set_shield(p_ptr->shield + randint(20) + 30);
				}
				else
				{
					(void)set_shield(p_ptr->shield + randint(10) + 15);
				}
				break;
			}
			case 50:	/* Recharge Item III */
			{
				recharge(220);
				break;
			}
			case 51:	/* Essence of Speed */
			{
				if (!p_ptr->fast)
				{
					(void)set_fast(randint(30) + 10 + plev);
				}
				else
				{
					(void)set_fast(p_ptr->fast + randint(10));
				}
				break;
			}
			case 52:	/* Strengthen Defenses */
			{
				if (!p_ptr->magicdef)
				{
					(void)set_extra_defences(40);
				}
				else
				{
					(void)set_extra_defences(p_ptr->magicdef + randint(20));
				}

				break;
			}
			case 53:	/* Acid Bolt */
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_ACID, dir,
				                  damroll(5+((plev-5)/5), 8));
				break;
			}
			case 54:	/* Cloud Kill */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_POIS, dir, 10 + plev, 3, FALSE);
				break;
			}
			case 55:	/* Acid Ball */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_ACID, dir, 2 * plev, 2, FALSE);
				break;
			}
			case 56:	/* Ice Storm */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_COLD, dir, 3 * plev, 3, FALSE);
				break;
			}
			case 57:	/* Meteor Swarm */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_METEOR, dir, 80 + (plev * 2), 1, FALSE);
				break;
			}
			case 58:	/* Mana Storm */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_MANA, dir, 120 + (plev * 2), 4, FALSE);
				break;
			}
			case 59:	/* Rogue Spell: Hit and Run */
			{
				p_ptr->special_attack |= (ATTACK_FLEE);
				break;
			}
			case 60:	/* Rogue Spell: Day of Misrule */
			{
				cptr p = (p_ptr->psex == SEX_FEMALE ? "Daughters" : "Sons");
				msg_format("%s of Night rejoice!  It's the Day of Misrule!", p);
				(void)set_fast(randint(30) + 30);
				(void)set_shero(p_ptr->shero + randint(30) + 30);
				break;
			}


			/* Holy Prayers */

			case 64: /* Detect Evil */
			{
				(void)detect_monsters_evil(DETECT_RAD_DEFAULT, TRUE);
				break;
			}
			case 65: /* Cure Light Wounds */
			{
				(void)hp_player(damroll(2, plev / 4 + 5));
				(void)set_cut(p_ptr->cut - 10);
				break;
			}
			case 66: /* Bless */
			{
				if (!p_ptr->blessed)
				{
					(void)set_blessed(p_ptr->blessed + randint(12) + 12);
				}
				else
				{
					(void)set_blessed(p_ptr->blessed + randint(4) + 4);
				}
				break;
			}
			case 67: /* Remove Fear */
			{
				(void)set_afraid(0);
				break;
			}
			case 68: /* Call Light */
			{
				(void)lite_area(damroll(2, 1 + (plev / 3)), (plev / 10) + 1);
				break;
			}
			case 69: /* Find Traps */
			{
				(void)detect_traps(DETECT_RAD_DEFAULT, TRUE);
				break;
			}
			case 70: /* Detect Doors/Stairs */
			{
			        /* Hack - 'show' effected region only with
				 * the first detect */
				(void)detect_doors(DETECT_RAD_DEFAULT, TRUE);
				(void)detect_stairs(DETECT_RAD_DEFAULT, FALSE);
				break;
			}
			case 71: /* Slow Poison */
			{
				(void)set_poisoned(p_ptr->poisoned / 2);
				break;
			}
			case 72: /* Cure Serious Wounds */
			{
				(void)hp_player(damroll(4, plev / 4 + 6));
				(void)set_cut((p_ptr->cut / 2) - 5);
				break;
			}
			case 73: /* Scare Monster */
			{
				if (!get_aim_dir(&dir)) return;
				(void)fear_monster(dir, (3 * plev / 2) + 10);
				break;
			}
			case 74: /* Portal */
			{
				teleport_player(2 * plev);
				break;
			}
			case 75: /* Chant */
			{
				if (!p_ptr->blessed)
				{
					(void)set_blessed(p_ptr->blessed + randint(24) + 24);
				}
				else
				{
					(void)set_blessed(p_ptr->blessed + randint(8) + 8);
				}
				break;
			}
			case 76: /* Sanctuary */
			{
				(void)sleep_monsters_touch(plev + 25);
				break;
			}
			case 77: /* Satisfy Hunger */
			{
				(void)set_food(PY_FOOD_MAX - 1);
				break;
			}
			case 78: /* Remove Curse */
			{
				if (remove_curse()) msg_print ("You feel kindly hands aiding you.");
				break;
			}
			case 79: /* Resist Heat and Cold */
			{
				(void)set_oppose_fire(p_ptr->oppose_fire + randint(10) + plev / 2);
				(void)set_oppose_cold(p_ptr->oppose_cold + randint(10) + plev / 2);
				break;
			}
			case 80: /* Neutralize Poison */
			{
				(void)set_poisoned(0);
				break;
			}
			case 81: /* Orb of Draining */
			{
				if (!get_aim_dir(&dir)) return;
				fire_sphere(GF_HOLY_ORB, dir, (damroll(3, 6) + plev / 4 +
					(plev / ((p_ptr->pclass == CLASS_PRIEST) ? 2 : 4))),
					((plev < 30) ? 1 : 2), 30);
				break;
			}
			case 82: /* Sense Invisible */
			{
				(void)set_tim_invis(p_ptr->tim_invis + randint(24) + plev);
				break;
			}
			case 83: /* Protection from Evil */
			{
				if (!p_ptr->protevil)
				{
					(void)set_protevil(p_ptr->protevil + 
						randint(24) + 3 * plev / 2);
				}
				else
				{
					(void)set_protevil(p_ptr->protevil + randint(30));
				}
				break;
			}
			case 84: /* Cure Mortal Wounds */
			{
				(void)hp_player(damroll(9, plev / 3 + 12));
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}
			case 85: /* Earthquake */
			{
				earthquake(py, px, 10, FALSE);
				break;
			}
			case 86: /* Sense Surroundings. */
			{
				/* Extended area for high-level Rangers. */
				if ((p_ptr->pclass == CLASS_RANGER) && (plev > 34))
					map_area(0, 0, TRUE);
				else 	map_area(0, 0, FALSE);
				break;
			}
			case 87: /* Turn Undead */
			{
				(void)turn_undead((3 * plev / 2) + 10);
				break;
			}
			case 88: /* Prayer */
			{
				if (!p_ptr->blessed)
				{
					(void)set_blessed(p_ptr->blessed + randint(48) + 48);
				}
				else
				{
					(void)set_blessed(p_ptr->blessed + randint(12) + 12);
				}
				break;
			}
			case 89: /* Dispel Undead */
			{
				(void)dispel_undead(randint(plev * 3));
				break;
			}
			case 90: /* Heal */
			{
				(void)hp_player(300);
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}
			case 91: /* Dispel Evil */
			{
				(void)dispel_evil(randint(plev * 3));
				break;
			}
			case 92: /* Sacred Shield */
			{
				if (!p_ptr->shield)
				{
					(void)set_shield(p_ptr->shield + randint(20) + 
						plev / 2);
				}
				else
				{
					(void)set_shield(p_ptr->shield + randint(10) + 
						plev / 4);
				}
				break;
			}
			case 93: /* Glyph of Warding */
			{
				(void)warding_glyph();
				break;
			}
			case 94: /* Holy Word */
			{
				(void)dispel_evil(randint(plev * 4));
				(void)hp_player(300);
				(void)set_afraid(0);
				(void)set_poisoned(0);
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}
			case 95: /* Blink */
			{
				teleport_player(10);
				break;
			}
			case 96: /* Teleport Self */
			{
				teleport_player(plev * 4);
				break;
			}
			case 97: /* Teleport Other */
			{
				if (!get_aim_dir(&dir)) return;
				(void)teleport_monster(dir);
				break;
			}
			case 98: /* Teleport Level */
			{
				(void)teleport_player_level();
				break;
			}
			case 99: /* Word of Recall */
			{
#if 1
				word_recall(rand_int(20) + 15);
#else
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
				p_ptr->redraw |= PR_STATUS;
#endif
				break;
			}
			case 100: /* Alter Reality */
			{
				msg_print("The world changes!");

				/* Leaving */
				p_ptr->leaving = TRUE;

				break;
			}
			case 101: /* Detect Monsters */
			{
				(void)detect_monsters_normal(DETECT_RAD_DEFAULT, TRUE);
				break;
			}
			case 102: /* Detection */
			{
				(void)detect_all(DETECT_RAD_DEFAULT, TRUE);
				break;
			}
			case 103: /* Probing */
			{
				(void)probing();
				break;
			}
			case 104: /* Perception */
			{
				(void)ident_spell();
				break;
			}
			case 105: /* Clairvoyance */
			{
				wiz_lite(FALSE);
				break;
			}
			case 106: /* Banishment */
			{
				if (banish_evil(100))
				{
					msg_print("The power of your god banishes evil!");
				}
				break;
			} 
			case 107: /* Healing */
			{
				(void)hp_player(700);
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}
			case 108: /* Sacred Knowledge */
			{
				(void)identify_fully();
				break;
			}
			case 109: /* Restoration */
			{
				(void)do_res_stat(A_STR);
				(void)do_res_stat(A_INT);
				(void)do_res_stat(A_WIS);
				(void)do_res_stat(A_DEX);
				(void)do_res_stat(A_CON);
				(void)do_res_stat(A_CHR);
				break;
			}
			case 110: /* Remembrance */
			{
				(void)restore_level();
				break;
			}
			case 111: /* Unbarring Ways */
			{
				(void)destroy_doors_touch();
				break;
			}
			case 112: /* Recharging */
			{
				(void)recharge(140);
				break;
			}
			case 113: /* Dispel Curse */
			{
				if (remove_all_curse()) 
				msg_print("A beneficent force surrounds you for a moment.");
				break;
			}
			case 114: /* Disarm Trap */
			{
				if (!get_aim_dir(&dir)) return;
				(void)disarm_trap(dir);
				break;
			}
			case 115: /* Holding */
			{
				if (!get_aim_dir(&dir)) return;

				/* Spell will hold any monster or door in one square. */
				fire_ball(GF_HOLD, dir, 0, 0, FALSE);

				break;
			}
			case 116: /* Enchant Weapon or Armour */
			{
				char answer;

				/* Query */
				msg_print("Would you like to enchant a 'W'eapon or 'A'rmour (w/a)");

				/* Interact and enchant. */
				while(1)
				{
					answer = inkey();
					if ((answer == 'W') || (answer == 'w'))
					{
						(void)enchant_spell(rand_int(4) + 1, 
						rand_int(4) + 1, 0);
						break;
					}
					else if ((answer == 'A') || (answer == 'a'))
					{
						(void)enchant_spell(0, 0, rand_int(3) + 2);
						break;
					}
					else if (answer == ESCAPE) return;
					else msg_print("Please type 'w' to enhant a weapon, 'a' to enchant armour, or ESC to cancel");
				}

				break;
			}
			case 117: /* Ball of Light */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_LITE, dir, plev * 2, 3, FALSE);
				break;
			}
			case 118: /* Holy Lance */
			{
				if (!get_aim_dir(&dir)) return;
				fire_beam(GF_HOLY_ORB, dir, 3 * plev / 2);
				break;
			}
			case 119: /* Word of Destruction */
			{
				destroy_area(py, px, 15, TRUE);
				break;
			}

			case 120: /* Annihilation */
			{
				if (!get_aim_dir(&dir)) return;
				drain_life(dir, plev * 3 + randint(100));
				break;
			}
			case 121: /* Call on Varda */
			{
				msg_print("Gilthoniel A Elbereth!");
				fire_sphere(GF_LITE, 0,
				          plev * 5, plev / 7 + 2, 20);
				(void)fear_monsters(plev * 2);
				(void)hp_player(500);
				break;
			}

			case 122: /* Paladin Prayer: Elemental Infusion */
			{
				choose_ele_attack();
				break;
			}
			case 123: /* Paladin Prayer: Sanctify for Battle */
			{
				if (!(p_ptr->special_attack & ATTACK_HOLY))
					msg_print("Your blows will strike with Holy might!");
				p_ptr->special_attack |= (ATTACK_HOLY);
				break;
			}
			case 124: /* Paladin Prayer: Horn of Wrath */
			{
				(void)hp_player(20);
				if (!p_ptr->hero)
				{
					(void)set_hero(p_ptr->hero + randint(20) + 20);
				}
				else
				{
					(void)set_hero(p_ptr->hero + randint(10) + 10);
				}
				(void)set_afraid(0);

				(void)fear_monsters(plev);
				break;
			}


			/* Nature Magics */

			case 128: /* detect life */
			{
				(void)detect_monsters_living(DETECT_RAD_DEFAULT, TRUE);
				break;
			}
			case 129:  /* call light */
			{
				(void)lite_area(damroll(2, 1 + (plev / 4)), (plev / 10) + 1);
				break;
			}
			case 130: /* foraging */
			{
				(void)set_food(PY_FOOD_MAX - 1);
				break;
			}
			case 131:  /* blink */
			{
				teleport_player(10);
				break;
			}
			case 132:  /* combat poison */
			{
				(void)set_poisoned(p_ptr->poisoned / 2);
				break;
			}
			case 133:  /* lightning spark */
			{
				if (!get_aim_dir(&dir)) return;
				fire_arc(GF_ELEC, dir, damroll(2 + (plev/8), 6), 
					(1 + plev / 5), 0);
				break;
			}
			case 134:  /* door destruction */
			{
				(void)destroy_doors_touch();
				break;
			}
			case 135:  /* turn stone to mud */
			{
				if (!get_aim_dir(&dir)) return;
				(void)wall_to_mud(dir);
				break;
			}
			case 136:  /* ray of sunlight */
			{
				if (!get_aim_dir(&dir)) return;
				msg_print("A ray of golden yellow light appears.");
				lite_line(dir);
				break;
			}
			case 137:  /* Cure poison */
			{
				(void)set_poisoned(0);
				break;
			}
			case 138:  /* frost bolt */
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam - 10, GF_COLD, dir,
				                  damroll(2 + (plev/5), 8));
				break;
			}
			case 139:  /* sleep creature */
			{
				if (!get_aim_dir(&dir)) return;
				(void)sleep_monster(dir, plev + 10);
				break;
			}
			case 140:  /* frighten creature */
			{
				if (!get_aim_dir(&dir)) return;
				(void)fear_monster(dir, plev + 10);
				break;
			}
			case 141:  /* detect trap/doors */
			{
				(void)detect_traps(DETECT_RAD_DEFAULT, TRUE);
				(void)detect_doors(DETECT_RAD_DEFAULT, TRUE);
				(void)detect_stairs(DETECT_RAD_DEFAULT, TRUE);
				break;
			}
			case 142:  /* cease small life */
			{
				(void)dispel_small_monsters(2 + plev / 5);
				break;
			}
			case 143:  /* fire bolt */
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam - 10, GF_FIRE, dir,
				                  damroll(3 + (plev/5), 8));
				break;
			}
			case 144:  /* heroism */
			{
				(void)hp_player(20);
				if (!p_ptr->hero)
				{
					(void)set_hero(p_ptr->hero + randint(20) + 20);
				}
				else
				{
					(void)set_hero(p_ptr->hero + randint(10) + 10);
				}
				(void)set_afraid(0);
				break;
			}
			case 145:  /* remove curse */
			{
				if (remove_curse()) msg_print("You feel tender hands aiding you.");
				break;
			}
			case 146:  /* acid bolt */
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam - 10, GF_ACID, dir,
				                  damroll(5 + (plev/5), 8));
				break;
			}
			case 147:  /* teleport monster */
			{
				if (!get_aim_dir(&dir)) return;
				(void)teleport_monster(dir);
				break;
			}
			case 148:  /* poison bolt */
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam - 10, GF_POIS, dir,
				                  damroll(5 + (plev/4), 8));
				break;
			}
			case 149:  /* resist poison */
			{
				(void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20);
				break;
			}
			case 150:  /* earthquake */
			{
				earthquake(py, px, 10, FALSE);
				break;
			}
			case 151:  /* resist fire & cold */
			{
				(void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
				(void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
				break;
			}
			case 152:  /* detect all */
			{
				(void)detect_all(DETECT_RAD_DEFAULT, TRUE);
				break;
			}
			case 153:  /* natural vitality */
			{
				(void)set_poisoned(p_ptr->poisoned / 2);
				(void)hp_player(damroll(2, plev / 5));
				(void)set_cut(p_ptr->cut - plev / 2);
				break;
			}
			case 154:  /* resist acid & lightning */
			{
				(void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20);
				(void)set_oppose_elec(p_ptr->oppose_elec + randint(20) + 20);
				break;
			}
			case 155:  /* wither foe */
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_MISSILE, dir,
					damroll(plev / 7, 8));			
				(void)confuse_monster(dir, plev + 10);
				(void)slow_monster(dir, plev + 10);

				break;
			}
			case 156:  /* disarm trap */
			{
				if (!get_aim_dir(&dir)) return;
				(void)disarm_trap(dir);
				break;
			}
			case 157:  /* identify */
			{
				(void)ident_spell();
				break;
			}
			case 158:  /* create athelas */
			{
				(void)create_athelas();
				break;
			}
			case 159:  /* raging storm */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_ELEC, dir, plev + randint(60 + plev * 2), 
					(1 + plev / 15), FALSE);
				break;
			}
			case 160:  /* thunderclap */
			{
				msg_print("Boom!");
				fire_sphere(GF_SOUND, 0,
				          plev + randint(40 + plev * 2), plev / 8, 20);
				break;
			}
			case 161:  /* become mouse */
			{
				shape = SHAPE_MOUSE;
				break;
			}
			case 162:  /* become ferret */
			{
				shape = SHAPE_FERRET;
				break;
			}
			case 163:  /* become hound */
			{
				shape = SHAPE_HOUND;
				break;
			}
			case 164:  /* become gazelle */
			{
				shape = SHAPE_GAZELLE;
				break;
			}
			case 165:  /* become lion */
			{
				shape = SHAPE_LION;
				break;
			}
			case 166:  /* detect evil */
			{
				(void)detect_monsters_evil(DETECT_RAD_DEFAULT, TRUE);
				break;
			}
			case 167:  /* song of frightening */
			{
				(void)fear_monsters(3 * plev / 2 + 10);
				break;
			}
			case 168:  /* sense surroundings */
			{
				map_area(0, 0, FALSE);
				break;
			}
			case 169:  /* sight beyond sight */
			{
			        /* Hack - 'show' effected region only with
				 * the first detect */
				(void)detect_monsters_normal(DETECT_RAD_DEFAULT, TRUE);
				(void)detect_monsters_invis(DETECT_RAD_DEFAULT, FALSE);
				(void)detect_traps(DETECT_RAD_DEFAULT, FALSE);
				(void)detect_doors(DETECT_RAD_DEFAULT, FALSE);
				(void)detect_stairs(DETECT_RAD_DEFAULT, FALSE);
				if (!p_ptr->tim_invis)
				{
					(void)set_tim_invis(p_ptr->tim_invis + 
						randint(24) + 24);
				}
				else
				{
					(void)set_tim_invis(p_ptr->tim_invis + 
						randint(12) + 12);
				}
				break;
			}
			case 170:  /* herbal healing */
			{
				(void)hp_player(damroll(25 + plev / 2, 12));
				(void)set_cut(0);
				(void)set_poisoned(0);
				break;
			}
			case 171:  /* blizzard */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_COLD, dir, plev + randint(50 + plev * 2), 
					1 + plev / 12, FALSE);
				break;
			}
			case 172:  /* trigger tsunami */
			{
				msg_print("You hurl mighty waves at your foes!");
				fire_sphere(GF_WATER, 0,
				          3 * plev / 2 + randint(30 + plev * 2), plev / 7, 20);
				break;
			}
			case 173:  /* volcanic eruption */
			{
				msg_print("The earth convulses and erupts in fire!");
				fire_sphere(GF_FIRE, 0, 3 * plev / 2 + randint(50 + plev * 3), 1 + plev / 15, 20);
				earthquake(py, px, plev / 5, TRUE);
				break;
			}
			case 174:  /* molten lightning */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_PLASMA, dir, 
					2 * plev + randint(50 + plev * 3), 1, FALSE);
				break;
			}
			case 175:  /* starburst. */
			{
				msg_print("Light bright beyond enduring dazzles your foes!");
				fire_sphere(GF_LITE, 0,
				          5 * plev / 2 + randint(plev * 3), plev / 10, 20);
				break;
			}
			case 176:  /* song of lulling */
			{
				msg_print("Your tranquil music enchants those nearby.");

				(void)slow_monsters(5 * plev / 3);
				(void)sleep_monsters(5 * plev / 3);
				break;
			}
			case 177:  /* song of protection */
			{
				msg_print("Your song creates a mystic shield.");
				if (!p_ptr->shield)
				{
					(void)set_shield(p_ptr->shield + randint(30) + 
						plev / 2);
				}
				else
				{
					(void)set_shield(p_ptr->shield + randint(15) + 
						plev / 4);
				}
				break;
			}
			case 178:  /* song of dispelling */
			{
				msg_print("An unbearable discord tortures your foes!");

				(void)dispel_monsters(randint(plev * 2));
				(void)dispel_evil(randint(plev * 2));
				break;
			}
			case 179:  /* song of warding */
			{
				msg_print("Your song creates a place of sanctuary.");

				(void)warding_glyph();
				break;
			}
			case 180:  /* song of renewal */
			{
				msg_print("Amidst the gloom, you envoke light and beauty; your body regains its natural vitality.");

				(void)do_res_stat(A_STR);
				(void)do_res_stat(A_INT);
				(void)do_res_stat(A_WIS);
				(void)do_res_stat(A_DEX);
				(void)do_res_stat(A_CON);
				(void)do_res_stat(A_CHR);
				(void)restore_level();
				break;
			}
			case 181:  /* time blast */
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(plev * 2, GF_TIME, dir,
				                  damroll(plev / 6, 8));
				break;
			}
			case 182:  /* essence of speed */
			{
				if (!p_ptr->fast)
				{
					(void)set_fast(randint(10) + plev / 2);
				}
				else
				{
					(void)set_fast(p_ptr->fast + randint(5));
				}
				break;
			}
			case 183:  /* infusion */
			{
				(void)recharge(125);
				break;
			}
			case 184:  /* become ent */
			{
				shape = SHAPE_ENT;
				break;
			}
			case 185:  /* regain life */
			{
				(void)restore_level();
				break;
			}
			case 186:  /* intervention of Yavanna */
			{
				(void)dispel_evil(100);
				(void)hp_player(500);
				(void)set_blessed(p_ptr->blessed + randint(100) + 100);
				(void)set_afraid(0);
				(void)set_poisoned(0);
				(void)set_stun(0);
				(void)set_cut(0);
				break;
			}
			case 187:  /* Ranger Spell:  Creature Knowledge */
			{
				msg_print("Target the monster you wish to learn about.");
				if (!get_aim_dir(&dir)) return;
				pseudo_probe();
				break;
			}


			/* Necromantic Spells */

			case 192: /* magic bolt */
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_MANA, dir, damroll(2, 5 + plev / 7));
				break;
			}
			case 193: /* detect evil */
			{
				(void)detect_monsters_evil(DETECT_RAD_DEFAULT, TRUE);
				break;
			}
			case 194: /* enhanced infravision */
			{
				set_tim_infra(p_ptr->tim_infra + 70 + randint(70));
				break;
			}
			case 195: /* break curse */
			{
				if (remove_curse()) 
					msg_print("You feel mighty hands aiding you.");
				break;
			}
			case 196: /* slow monster */
			{
				if (!get_aim_dir(&dir)) return;
				(void)slow_monster(dir, plev + 5);
				break;
			}
			case 197: /* sleep monster */
			{
				if (!get_aim_dir(&dir)) return;
				(void)sleep_monster(dir, plev + 5);
				break;
			}
			case 198: /* horrify */
			{
				if (!get_aim_dir(&dir)) return;
				(void)fear_monster(dir, plev + 15);
				break;
			}
			case 199: /* become bat */
			{
				take_hit(damroll(2, 4), "shapeshifting stress");
				shape = SHAPE_BAT;
				break;
			}
			case 200: /* door destruction */
			{
				(void)destroy_doors_touch();
				break;
			}
			case 201: /* dark bolt */
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam - 10, GF_DARK, dir,
				                  damroll((3 + plev / 7), 8));
				break;
			}
			case 202: /* noxious fumes */
			{
				fire_sphere(GF_POIS, 0, 10 + plev, 2 + plev / 12, 40);
				if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
				{
					set_poisoned(p_ptr->poisoned + rand_int(2) + 2);
				}
				break;
			}
			case 203: /* turn undead */
			{
				(void)turn_undead(3 * plev / 2);
				break;
			}
			case 204: /* turn evil */
			{
				(void)turn_evil(5 * plev / 4);
				break;
			}
			case 205: /* cure poison */
			{
				(void)set_poisoned(0);
				break;
			}
			case 206: /* dispel undead */
			{
				(void)dispel_undead(plev + 15 + randint(3 * plev / 2));
				break;
			}
			case 207: /* dispel evil */
			{
				(void)dispel_evil(plev + randint(plev));
				break;
			}
			case 208: /* see invisible */
			{
				if (!p_ptr->tim_invis)
				{
					set_tim_invis(p_ptr->tim_invis + 20 + 
						randint(plev / 2));
				}
				else
				{
					set_tim_invis(p_ptr->tim_invis + 10 + 
						randint(plev / 4));
				}
				break;
			}
			case 209: /* shadow shifting */
			{
				take_hit(damroll(1, 4), "shadow dislocation");
				teleport_player(16);
				break;
			}
			case 210: /* detect traps */
			{
				(void)detect_traps(DETECT_RAD_DEFAULT, TRUE);
				break;
			}
			case 211: /* detect doors/stairs */
			{
			        /* Hack - 'show' effected region only with
				 * the first detect */
				(void)detect_doors(DETECT_RAD_DEFAULT, TRUE);
				(void)detect_stairs(DETECT_RAD_DEFAULT, FALSE);
				break;
			}
			case 212: /* sleep monsters */
			{
				(void)sleep_monsters(plev + 5);
				break;
			}
			case 213: /* slow monsters */
			{
				(void)slow_monsters(plev + 5);
				break;
			}
			case 214: /* detect magic */
			{
				(void)detect_objects_magic(DETECT_RAD_DEFAULT, TRUE);
				break;
			}
			case 215: /* death bolt */
			{
				take_hit(damroll(1, 6), "the dark arts");
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_SPIRIT, dir,
				       damroll(2 + plev / 3, 8));
				break;
			}
			case 216: /* resist poison */
			{
				(void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + plev / 2);
				break;
			}
			case 217: /* Dispel Demons */
			{
				(void)dispel_demons(2 * plev + randint(2 * plev));
				break;
			}
			case 218: /* dark spear */
			{
				if (!get_aim_dir(&dir)) return;
				fire_beam(GF_DARK, dir, 12 + plev);
				break;
			}
			case 219: /* mana bolt */
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam, GF_MANA, dir,
				       damroll(1 + plev / 2, 8));
				break;
			}
			case 220: /* genocide */
			{
				(void)genocide();
				break;
			}
			case 221: /* dark ball */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_DARK, dir, 50 + plev * 2, 2, FALSE);
				break;
			}
			case 222: /* stench of death */
			{
				take_hit(damroll(2, 8), "the stench of Death");
				(void)dispel_living(50 + randint(plev));
				confu_monsters(plev + 10);
				if (get_aim_dir(&dir))
				{
					fire_sphere(GF_POIS, dir, plev * 2, 5 + plev / 11, 40);
				}
				break;
			}
			case 223: /* probing */
			{
				(void)probing();
				break;
			}
			case 224: /* shadow mapping */
			{
				map_area(0, 0, FALSE);
				break;
			}
			case 225: /* identify */
			{
				(void)ident_spell();
				break;
			}
			case 226: /* shadow warping */
			{
				take_hit(damroll(2, 6), "shadow dislocation");
				teleport_player(plev * 3);
				break;
			}
			case 227: /* poison ammo - for assassins only */
			{
				(void)brand_missile(0, EGO_POISON);
				break;
			}
			case 228: /* resist acid and cold */
			{
				(void)set_oppose_acid(p_ptr->oppose_pois + randint(20) + 20);
				(void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
				break;
			}
			case 229: /* heal any wound */
			{
				(void)set_cut(0);
				(void)set_stun(0);
				break;
			}
			case 230: /* protection from evil */
			{
				(void)set_protevil(p_ptr->protevil + plev / 2 + randint(plev));
				break;
			}
			case 231: /* black blessing */
			{
				(void)set_blessed(p_ptr->blessed + randint(66));
				break;
			}
			case 232: /* banish evil */
			{
				(void)banish_evil(100);
				break;
			}
			case 233: /* shadow barrier */
			{
				if (!p_ptr->shield)
				{
					(void)set_shield(p_ptr->shield + randint(20) + 10);
				}
				else
				{
					(void)set_shield(p_ptr->shield + randint(10) + 5);
				}
				break;
			}
			case 234: /* detect all monsters */
			{
			        /* Hack - 'show' effected region only with
				 * the first detect */
				(void)detect_monsters_normal(DETECT_RAD_DEFAULT, TRUE);
				(void)detect_monsters_invis(DETECT_RAD_DEFAULT, FALSE);
				break;
			}
			case 235: /* strike at life */
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_NETHER, dir,
				       damroll(3 * plev / 5, 11));
				break;
			}
			case 236: /* orb of death */
			{
				take_hit(damroll(2, 8), "Death claiming his wages");
				if (!get_aim_dir(&dir)) return;
				fire_sphere(GF_SPIRIT, dir, 15 + plev * 3, 1, 20);
				break;
			}
			case 237: /* dispel life */
			{
				(void)dispel_living(60 + randint(plev * 2));
				break;
			}
			case 238: /* vampiric drain */
			{
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_SPIRIT, dir,
				       damroll(plev / 3, 11));
				(void)hp_player(3 * plev);
				(void)set_food(p_ptr->food + 1000);
				break;
			}
			case 239: /* recharging */
			{
				(void)recharge(140);
				break;
			}
			case 240: /* become werewolf */
			{
				take_hit(damroll(2, 7), "shapeshifting stress");
				shape = SHAPE_WEREWOLF;
				break;
			}
			case 241: /* dispel curse */
			{
				if (remove_all_curse()) msg_print ("You feel mighty hands aiding you.");
				break;
			}
			case 242: /* become vampire */
			{
				take_hit(damroll(3, 6), "shapeshifting stress");
				shape = SHAPE_VAMPIRE;
				break;
			}
			case 243: /* haste self */
			{
				if (!p_ptr->fast)
				{
					(void)set_fast(10 + randint(20));
				}
				else
				{
					(void)set_fast(p_ptr->fast + randint(5));
				}
				break;
			}
			case 244: /* prepare black breath */
			{
				msg_print("Your hands start to radiate Night.");
				p_ptr->special_attack |= (ATTACK_BLKBRTH);
				break;
			}
			case 245: /* word of destruction */
			{
				destroy_area(py, px, 15, TRUE);
				break;
			}
			case 246: /* teleport away */
			{
				if (!get_aim_dir(&dir)) return;
				(void)teleport_monster(dir);
				break;
			}
			case 247: /* smash undead */
			{
				(void)dispel_undead(plev * 3 + randint(50));
				break;
			}
			case 248: /* bind undead */
			{
				(void)hold_undead();
				break;
			}
			case 249: /* darkness storm */
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_DARK, dir, 11 * plev / 2, plev / 7, FALSE);
				break;
			}
			case 250: /* Necro spell - timed ESP */
			{
				if (!p_ptr->tim_esp)
				{
					(void)set_tim_esp(30 + randint(40));
				}
				else
				{
					(void)set_tim_esp(p_ptr->tim_esp + randint(30));
				}
				break;
			}
			case 251:	/* Rogue and Assassin Spell - Slip into the Shadows */
			{
				if (!p_ptr->superstealth)
				{
					(void)set_superstealth(40);
				}
				else
				{
					(void)set_superstealth(p_ptr->superstealth + randint(20));
				}
				break;
			}
			case 252:	/* Assassin spell: Bloodwrath */
			{
				if (!p_ptr->shero)
				{
					(void)set_shero(40);
				}
				else
				{
					(void)set_shero(p_ptr->shero + randint(20));
				}

				(void)set_fast(40);

				break;
			}
			case 253:	/* Assassin Spell - Rebalance Weapon */
			{
				rebalance_weapon();
				break;
			}
		}

		/* A spell was cast or a prayer prayed */
		if (!((spell < 32) ?
		       (p_ptr->spell_worked1 & (1L << spell)) :
		       (p_ptr->spell_worked2 & (1L << (spell - 32)))))
		{
			int e = s_ptr->sexp;

			/* The spell or prayer worked */
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
		if (mp_ptr->spell_book == TV_NECRO_BOOK)
			msg_print("You collapse after the ritual!");
		else msg_print("You faint from the effort!");

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

	/* Alter shape, if necessary. */
	if (shape) shapechange(shape);


	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

}

