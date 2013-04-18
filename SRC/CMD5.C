/* File: cmd5.c */

/* Purpose: code for mage/priest spells/prayers */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"



/*
 * Determine if a spell is "okay" for the player to cast
 * A spell must be legible, learned, and not forgotten
 */
static bool spell_okay(int j)
{
    spell_type *s_ptr;

    /* Access the spell */
    s_ptr = &magic_spell[p_ptr->pclass-1][j];

    /* Spell is illegal */
    if (s_ptr->slevel > p_ptr->lev) return (FALSE);

    /* Spell is forgotten */
    if (j < 32) {
	if (spell_forgotten1 & (1L << j)) return (FALSE);
    }
    else {
	if (spell_forgotten2 & (1L << (j - 32))) return (FALSE);
    }

    /* Spell is learned */
    if (j < 32) {
	if (spell_learned1 & (1L << j)) return (TRUE);
    }
    else {
	if (spell_learned2 & (1L << (j - 32))) return (TRUE);
    }

    /* Assume unknown */
    return (FALSE);
}


/*
 * Extra information on a spell         -DRS-
 * We can use up to 14 characters of the buffer 'p'
 */
static void spell_info(char *p, int j)
{
    /* Default */
    strcpy(p, "");

#ifdef DRS_SHOW_SPELL_INFO

    /* Mage spells */
    if (cp_ptr->spell_stat == A_INT) {

	int plev = p_ptr->lev;

	/* Analyze the spell */
	switch (j) {
	    case 0: sprintf(p, " dam %dd6", 2+((plev)/12)); break;
	    case 2: strcpy(p, " range 10"); break;
	    case 5: strcpy(p, " heal 2d8"); break;
	    case 8: sprintf(p, " dam %d", 10 + (plev / 2)); break;
	    case 10: sprintf(p, " dam %dd8", (3+((plev-5)/5))); break;
	    case 13: sprintf(p, " range %d", plev * 5); break;
	    case 14: strcpy(p," dam 6d8"); break;
	    case 15: sprintf(p, " dam %dd8", (5+((plev-5)/4))); break;
	    case 20: sprintf(p, " dam %dd8", (3+((plev-5)/5))); break;
	    case 24: sprintf(p, " dam %dd8", (8+((plev-5)/3))); break;
	    case 26: sprintf(p, " dam %d", 30 + plev); break;
	    case 27: sprintf(p, " dam %dd8", (5+((plev-5)/4))); break;
	    case 30: sprintf(p, " dur %d+d20", plev); break;
	    case 31: sprintf(p, " dam %d", 55 + (plev*3)/2); break;
	    case 32: sprintf(p, " dam %dd8", (8+((plev-5)/3))); break;
	    case 35: sprintf(p, " dam %dd12", (8+((plev-5)/3))); break;
	    case 41: sprintf(p, " dam %dd8", (6+((plev-5)/4))); break;
	    case 42: sprintf(p, " dam %d", 40 + plev/2); break;
	    case 43: sprintf(p, " dam %d", 40 + plev); break;
	    case 44: sprintf(p, " dam %d", 70 + (3*plev)/2); break;
	    case 45: sprintf(p, " dam %d", 65 + (3*plev)/2); break;
	    case 46: strcpy(p, " dam 300"); break;
	    case 52: strcpy(p, " dur 20+d20"); break;
	    case 53: strcpy(p, " dur 20+d20"); break;
	    case 54: strcpy(p, " dur 20+d20"); break;
	    case 55: strcpy(p, " dur 20+d20"); break;
	    case 56: strcpy(p, " dur 20+d20"); break;
	    case 57: strcpy(p, " dur 20+d20"); break;
	    case 58: strcpy(p, " dur 25+d25"); break;
	    case 59: strcpy(p, " dur 30+d20"); break;
	    case 60: strcpy(p, " dur 25+d25"); break;
	    case 61: sprintf(p, " dur %d+d25", 30+plev); break;
	    case 62: strcpy(p, " dur 6+d8"); break;
	}
    }

    /* Priest spells */
    else {

	int plev = p_ptr->lev;

	/* See below */
	int orb = (plev / ((p_ptr->pclass == 2) ? 2 : 4));
	
	/* Analyze the spell */
	switch (j) {
	    case 1: strcpy(p, " heal 2d8"); break;
	    case 6: strcpy(p, " dam 3d3"); break;
	    case 9: sprintf(p, " range %d", 3*plev); break;
	    case 10: strcpy(p, " heal 4d8"); break;
	    case 17: sprintf(p, " %d+3d6", plev + orb); break;
	    case 18: strcpy(p, " heal 6d8"); break;
	    case 20: sprintf(p, " dur %d+d25", 3*plev); break;
	    case 23: strcpy(p, " heal 8d8"); break;
	    case 26: sprintf(p, " dam d%d", 3*plev); break;
	    case 27: strcpy(p, " heal 300"); break;
	    case 28: sprintf(p, " dam d%d", 3*plev); break;
	    case 30: strcpy(p, " heal 1000"); break;
	    case 36: strcpy(p, " heal 4d8"); break;
	    case 37: strcpy(p, " heal 8d8"); break;
	    case 38: strcpy(p, " heal 2000"); break;
	    case 41: sprintf(p, " dam d%d", 4*plev); break;
	    case 42: sprintf(p, " dam d%d", 4*plev); break;
	    case 45: strcpy(p, " dam 200"); break;
	    case 52: strcpy(p, " range 10"); break;
	    case 53: sprintf(p, " range %d", 8*plev); break;
	}
    }

#endif

}


/*
 * Print a list of spells (for browsing or casting)
 */
static void print_spells(int *spell, int num)
{
    int                 i, j, col;

    spell_type          *s_ptr;

    cptr                comment;

    char                info[80];
    char                out_val[80];

    /* Print column */
    col = 20;

    /* Title the list */
    prt("", 1, col);
    put_str("Name", 1, col + 5);
    put_str("Lv Mana Fail", 1, col + 35);

    /* Dump the spells */
    for (i = 0; i < num; i++) {

	/* Access the spell */
	j = spell[i];

	/* Access the spell */
	s_ptr = &magic_spell[p_ptr->pclass-1][j];

	/* Skip illegible spells */
	if (s_ptr->slevel >= 99) {
	    sprintf(out_val, "  %c) %-30s", 'a' + i, "(illegible)");
	    prt(out_val, 2 + i, col);
	    continue;
	}

	/* XXX XXX Could label spells above the players level */

	/* Default to no comment */
	comment = "";

	/* Get an additional comment */
	if (show_spell_info) {
	    spell_info(info, j);
	    comment = info;
	}

	/* Analyze the spell */
	if (j >= 32 ?
		 ((spell_forgotten2 & (1L << (j - 32)))) :
		 ((spell_forgotten1 & (1L << j)))) {
	    comment = " forgotten";
	}
	else if (j >= 32 ?
		 (!(spell_learned2 & (1L << (j - 32)))) :
		 (!(spell_learned1 & (1L << j)))) {
	    comment = " unknown";
	}
	else if (j >= 32 ?
		 (!(spell_worked2 & (1L << (j - 32)))) :
		 (!(spell_worked1 & (1L << j)))) {
	    comment = " untried";
	}

	/* Dump the spell --(-- */
	sprintf(out_val, "  %c) %-30s%2d %4d %3d%%%s",
		'a' + i, spell_names[cp_ptr->spell_type][j],
		s_ptr->slevel, s_ptr->smana, spell_chance(j), comment);
	prt(out_val, 2 + i, col);
    }

    /* Clear the bottom line */
    prt("", 2 + i, col);
}




/*
 * Hack -- Print a list of spells in the choice window.
 * See "print_spells()" for the basic algorithm.
 */
static void choice_spell(int *spell, int num)
{
    int                 i, j;

    spell_type          *s_ptr;

    cptr                comment;

    char                info[80];
    char                out_val[80];


    /* In-active */
    if (!use_choice_win || !term_choice) return;


    /* Activate the choice window */
    Term_activate(term_choice);

    /* Clear it */
    Term_clear();


#if 0
    /* Title the list */
    prt("", 1, col);
    put_str("Name", 1, col + 5);
    put_str("Lv Mana Fail", 1, col + 35);
#endif

    /* Dump the spells */
    for (i = 0; i < num; i++) {

	/* Access the spell */
	j = spell[i];

	/* Access the spell */
	s_ptr = &magic_spell[p_ptr->pclass-1][j];

	/* Skip illegible spells */
	if (s_ptr->slevel >= 99) {
	    /* --(-- */
	    sprintf(out_val, "%c) %-30s", 'a' + i, "(illegible)");
	    Term_putstr(0, i, -1, TERM_WHITE, out_val);
	    continue;
	}

	/* Default to no comment */
	comment = "";

	/* Get an additional comment */
	if (show_spell_info) {
	    spell_info(info, j);
	    comment = info;
	}

	/* Analyze the spell */
	if (j >= 32 ?
		 ((spell_forgotten2 & (1L << (j - 32)))) :
		 ((spell_forgotten1 & (1L << j)))) {
	    comment = " forgotten";
	}
	else if (j >= 32 ?
		 (!(spell_learned2 & (1L << (j - 32)))) :
		 (!(spell_learned1 & (1L << j)))) {
	    comment = " unknown";
	}
	else if (j >= 32 ?
		 (!(spell_worked2 & (1L << (j - 32)))) :
		 (!(spell_worked1 & (1L << j)))) {
	    comment = " untried";
	}

	/* Dump the spell --(-- */
	sprintf(out_val, "%c) %-30s%2d %4d %3d%%%s",
		'a' + i, spell_names[cp_ptr->spell_type][j],
		s_ptr->slevel, s_ptr->smana, spell_chance(j), comment);
	Term_putstr(0, i, -1, TERM_WHITE, out_val);
    }


    /* Refresh */
    Term_fresh();

    /* Activate the main screen */
    Term_activate(term_screen);
}





/*
 * Allow user to choose a spell from the given book.
 * Note -- never call this function for warriors!
 *
 * If a valid spell is chosen, saves it in '*sn' and returns TRUE
 * If the user hits escape, returns FALSE, and set '*sn' to -1
 * If there are no legal choices, returns FALSE, and sets '*sn' to -2
 */
static int cast_spell(cptr prompt, int item_val, int *sn)
{
    int                 i, use = -1, num;
    int                 spell[64];
    bool                flag, redraw, okay, ask;
    char                choice;
    cptr                q;
    u32b                j1, j2;
    inven_type          *i_ptr;
    spell_type          *s_ptr;

    char                out_str[160];
    char                tmp_str[160];


    /* Get the spell book */
    i_ptr = &inventory[item_val];

    /* Observe spells in that book */
    j1 = spell_flags[cp_ptr->spell_type][i_ptr->sval][0];
    j2 = spell_flags[cp_ptr->spell_type][i_ptr->sval][1];

    /* Extract spells */
    num = 0;
    while (j1) spell[num++] = bit_pos(&j1);
    while (j2) spell[num++] = bit_pos(&j2) + 32;


    /* Assume no usable spells */
    okay = FALSE;

    /* Assume no spells available */
    (*sn) = -2;

    /* Check for known spells */
    for (i = 0; i < num; i++) {
	if (spell_okay(spell[i])) okay = TRUE;
    }

    /* No usable spells */
    if (!okay) return (FALSE);


    /* Assume cancelled */
    *sn = (-1);

    /* Nothing chosen yet */
    flag = FALSE;

    /* No redraw yet */
    redraw = FALSE;


    /* Use the "choice" window */
    if (choice_spells) {

	/* Display the choices */
	choice_spell(spell, num);

	/* Fix the choice window later */
	p_ptr->redraw |= (PR_CHOICE);
    }


    /* Build a prompt (accept all spells) */
    sprintf(out_str, "(Spells %c-%c, *=List, <ESCAPE>=exit) %s",
	    'a', 'a' + num - 1, prompt);

    /* Get a spell from the user */
    while (!flag && get_com(out_str, &choice)) {

	/* Request redraw */
	if ((choice == ' ') || (choice == '*') || (choice == '?')) {

	    /* only do this drawing once */
	    if (!redraw) {

		/* Save the screen */
		save_screen();

		/* Remember to fix it */
		redraw = TRUE;

		/* Display a list of spells */
		print_spells(spell, num);
	    }

	    /* Ask again */
	    continue;
	}


	/* Note verify */
	ask = (isupper(choice));

	/* Extract request */
	i = ask ? (choice - 'A') : (choice - 'a');

	/* Totally Illegal */
	if ((i < 0) || (i >= num)) {
	    bell();
	    continue;
	}

	/* Save the spell index */
	use = spell[i];

	/* Check for illegal spell */
	if (!spell_okay(use)) {
	    sprintf(tmp_str, "You don't know that %s.",
		((cp_ptr->spell_stat == A_INT) ? "spell" : "prayer"));
	    msg_print(tmp_str);
	    continue;
	}

	/* Verify it */
	if (ask) {

	    /* Access the spell */
	    s_ptr = &magic_spell[p_ptr->pclass-1][use];

	    /* Prompt */
	    sprintf(tmp_str, "Cast %s (%d mana, %d%% fail)?",
		    spell_names[cp_ptr->spell_type][use], s_ptr->smana,
		    spell_chance(use));

	    /* Belay that order */
	    if (!get_check(tmp_str)) continue;
	}

	/* Stop the loop */
	flag = TRUE;
    }

    /* Restore the screen */
    if (redraw) restore_screen();


    /* Abort if needed */
    if (!flag) return (FALSE);


    /* Access the spell */
    s_ptr = &magic_spell[p_ptr->pclass-1][use];

    /* Verify if needed */
    if (s_ptr->smana > p_ptr->cmana) {

	/* Extract a message */
	if (cp_ptr->spell_stat == A_INT) {
	    q = "You summon your limited strength to cast this one! Confirm?";
	}
	else {
	    q = "The gods may think you presumptuous for this! Confirm?";
	}

	/* Allow cancel */
	if (!get_check(q)) return (FALSE);
    }

    /* Save the choice */
    (*sn) = use;

    /* Success */
    return (TRUE);
}




/*
 * Peruse the spells/prayers in a Book
 *
 * Note that *all* spells in the book are listed
 */
void do_cmd_browse(void)
{
    int                  i1, i2, num, item_val;
    int                  spell[64];
    u32b                j1, j2;
    inven_type          *i_ptr;


    /* The tval of readible books */
    int read_tval = 0;

    /* Acquire the type value of the books that the player can read, if any */
    if (cp_ptr->spell_stat == A_WIS) read_tval = TV_PRAYER_BOOK;
    else if (cp_ptr->spell_stat == A_INT) read_tval = TV_MAGIC_BOOK;


    /* This command is free */
    energy_use = 0;

    if (p_ptr->blind || no_lite()) {
	msg_print("You cannot see!");
	return;
    }

    if (p_ptr->pclass == 0) {
	msg_print("You cannot read books!");
	return;
    }

    if (p_ptr->confused) {
	msg_print("You are too confused!");
	return;
    }

    if (!find_range(read_tval, &i1, &i2)) {
	msg_print("You are not carrying any usable books!");
	return;
    }


    /* Get a book or stop checking */
    if (!get_item(&item_val, "Browse which Book?", i1, i2, FALSE)) {
	if (item_val == -2) msg_print("You have no books that you can read.");
	return;
    }


    /* Access the book */
    i_ptr = &inventory[item_val];

    /* Obtain all spells in the book */
    j1 = spell_flags[cp_ptr->spell_type][i_ptr->sval][0];
    j2 = spell_flags[cp_ptr->spell_type][i_ptr->sval][1];

    /* Build spell list */
    num = 0;
    while (j1) spell[num++] = bit_pos(&j1);
    while (j2) spell[num++] = bit_pos(&j2) + 32;


    /* Display the spells */
    save_screen();

    /* Display the spells */
    print_spells(spell, num);

    /* Wait for it */
    pause_line(0);

    /* Fix the screen */
    restore_screen();
}




/*
 * gain spells when player wants to             - jw
 */
void do_cmd_study(void)
{
    char                query;

    int                 diff_spells, new_spells;
    int                 spell[64], last_known;
    int                 i, j, ii, jj, num, col;

    u32b                spell_flag1, spell_flag2;

    spell_type          *s_ptr;

    char                buf[160];
    char                tmp_str[160];

    /* The tval of readible books */
    int read_tval = 0;

    /* Acquire the type value of the books that the player can read, if any */
    if (cp_ptr->spell_stat == A_WIS) read_tval = TV_PRAYER_BOOK;
    else if (cp_ptr->spell_stat == A_INT) read_tval = TV_MAGIC_BOOK;


    /* Assume free */
    energy_use = 0;


    if (p_ptr->pclass == 0) {
	msg_print("You cannot learn magic!");
	return;
    }

    if (p_ptr->blind || no_lite()) {
	msg_print("You cannot see!");
	return;
    }

    if (p_ptr->confused) {
	msg_print("You are too confused!");
	return;
    }

    /* No spells */
    if (!(p_ptr->new_spells)) {
	sprintf(tmp_str, "You cannot learn any new %ss!",
		(cp_ptr->spell_stat == A_INT ? "spell" : "prayer"));
	msg_print(tmp_str);
    }


    /* Count available spells */
    new_spells = p_ptr->new_spells;

    /* Count change */
    diff_spells = 0;


    /* Find the next open entry in "spell_order[]" */
    for (last_known = 0; last_known < 64; last_known++) {
	if (spell_order[last_known] == 99) break;
    }

    /* Determine which spells player can learn */
    spell_flag1 = 0L;
    spell_flag2 = 0L;

    /* Check all books */
    for (i = 0; i < inven_ctr; i++) {

	/* Only check readible books */
	if (inventory[i].tval == read_tval) {

	    /* Collect the flags */
	    spell_flag1 |= spell_flags[cp_ptr->spell_type][inventory[i].sval][0];
	    spell_flag2 |= spell_flags[cp_ptr->spell_type][inventory[i].sval][1];
	}
    }

    /* Clear bits of spells already learned */
    spell_flag1 &= ~spell_learned1;
    spell_flag2 &= ~spell_learned2;

    /* Reset */
    i = 0;

    /* Extract available spells */
    while (spell_flag1) {
	j = bit_pos(&spell_flag1);
	s_ptr = &magic_spell[p_ptr->pclass-1][j];
	if (s_ptr->slevel <= p_ptr->lev) spell[i++] = j;
    }

    /* Extract available spells */
    while (spell_flag2) {
	j = bit_pos(&spell_flag2) + 32;
	s_ptr = &magic_spell[p_ptr->pclass-1][j];
	if (s_ptr->slevel <= p_ptr->lev) spell[i++] = j;
    }


    /* Note missing books */
    if (new_spells > i) {
	msg_print("You seem to be missing a book.");
	diff_spells = new_spells - i;
	new_spells = i;
	if (new_spells == 0) return;
    }


    /* Take a turn */
    energy_use = 0;


    /* Mage-spells */
    if (cp_ptr->spell_stat == A_INT) {

	/* Save the screen */
	save_screen();

	/* Let player choose spells until done */
	while (new_spells) {

	    /* Column */
	    col = 31;

	    /* Number of spells */
	    num = (i <= 22) ? i : 22;

	    /* Title the list */
	    prt("", 1, col);
	    put_str("Name", 1, col + 5);
	    put_str("Lv Mana Fail", 1, col + 35);

	    /* List the spells */
	    for (ii = 0; ii < num; ii++) {

		/* Access the spell */
		jj = spell[ii];

		/* Access the spell */
		s_ptr = &magic_spell[p_ptr->pclass-1][jj];

		/* List the spell --(-- */
		(void)sprintf(buf, "  %c) %-30s%2d %4d %3d%%",
			      'a' + ii, spell_names[cp_ptr->spell_type][jj],
			      s_ptr->slevel, s_ptr->smana, spell_chance(jj));
		prt(buf, 2 + ii, col);
	    }


	    /* Prepare a prompt */
	    sprintf(buf, "Learn which spell (%d left)? ", new_spells);

	    /* Let player choose a spell */
	    if (!get_com(buf, &query)) break;

	    /* Analyze request */
	    j = query - 'a';

	    /* Analyze valid answers */
	    if ((j >= 0) && (j < i) && (j < 22)) {

		/* Add the spell */
		if (spell[j] < 32) {
		    spell_learned1 |= 1L << spell[j];
		}
		else {
		    spell_learned2 |= 1L << (spell[j] - 32);
		}

		/* Add the spell to the known list */
		spell_order[last_known++] = spell[j];

		/* Slide the spells */
		for (; j <= i - 1; j++) spell[j] = spell[j + 1];

		/* One less spell available */
		i--;

		/* One less spell to learn */
		new_spells--;

		/* Clear the last spell */
		prt("", j + 1, 31);

		/* Try again */
		continue;
	    }

	    /* Invalid choice */
	    bell();
	}

	/* Restore screen */
	restore_screen();
    }

    /* Priest spells */
    else {

	/* Learn a single prayer */
	if (new_spells) {

	    /* Pick a spell to learn */
	    j = rand_int(i);

	    /* Learn the spell */
	    if (spell[j] < 32) {
		spell_learned1 |= 1L << spell[j];
	    }
	    else {
		spell_learned2 |= 1L << (spell[j] - 32);
	    }

	    /* Memorize the order */
	    spell_order[last_known++] = spell[j];

	    /* Mention the result */
	    (void)sprintf(tmp_str,
			  "You have learned the prayer of %s.",
			  spell_names[cp_ptr->spell_type][spell[j]]);
	    msg_print(tmp_str);

	    /* Slide the spells */
	    for (; j <= i - 1; j++) spell[j] = spell[j + 1];

	    /* One less spell available */
	    i--;

	    /* One less spell to learn */
	    new_spells--;
	}

	/* Report on remaining prayers */
	if (new_spells) {
	    sprintf(tmp_str, "You can learn %d more prayer%s.",
		    new_spells, (new_spells == 1) ? "" : "s");
	    msg_print(tmp_str);
	}
    }

    /* Remember how many spells can be learned */
    p_ptr->new_spells = new_spells + diff_spells;

    /* Update the mana */
    p_ptr->update |= (PU_MANA);

    /* Redraw Study Status */
    p_ptr->redraw |= PR_STUDY;
}


/*
 * Hack -- fire a bolt, or a beam if lucky
 */
static void bolt_or_beam(int prob, int typ, int dir, int dam)
{
    if (rand_int(100) < prob) {
	line_spell(typ, dir, py, px, dam);
    }
    else {
	fire_bolt(typ, dir, py, px, dam);
    }
}


/*
 * Throw a magic spell                                  -RAK-
 *
 * Note that the "beam" chance is now based on "plev" and not "intelligence".
 * This will make it less common early in the game and more common later.
 */
void do_cmd_cast(void)
{
    int                 i, j, item_val, dir;
    int                 choice, chance, beam;
    int                 plev = p_ptr->lev;

    spell_type   *s_ptr;

    energy_use = 0;

    if (cp_ptr->spell_stat != A_INT) {
	msg_print("You cannot cast spells!");
	return;
    }

    if (p_ptr->blind || no_lite()) {
	msg_print("You cannot see!");
	return;
    }

    if (p_ptr->confused) {
	msg_print("You are too confused!");
	return;
    }

    if (!find_range(TV_MAGIC_BOOK, &i, &j)) {
	msg_print("You are not carrying any spell-books!");
	return;
    }


    /* Get a spell book */
    if (!get_item(&item_val, "Use which Spell Book? ", i, j, FALSE)) {
	if (item_val == -2) msg_print("You are not carrying any spell-books!");
	return;
    }


    /* Ask for a spell */
    if (!cast_spell("Cast which spell?", item_val, &choice)) {
	if (choice == -2) msg_print("You don't know any spells in that book.");
	return;
    }


    /* Access the spell */
    s_ptr = &magic_spell[p_ptr->pclass-1][choice];

    /* Spell failure chance */
    chance = spell_chance(choice);

    /* Failed spell */
    if (rand_int(100) < chance) {
	if (flush_failure) flush();
	msg_print("You failed to get the spell off!");
    }

    /* Process spell */
    else {

	/* Hack -- chance of "beam" instead of "bolt" */
	beam = ((p_ptr->pclass == 1) ? plev : (plev / 2));

	/* Spells.  */
	switch (choice + 1) {

	  case 1:
	    if (!get_dir(NULL, &dir)) return;
	    bolt_or_beam(beam-10, GF_MISSILE, dir,
			 damroll(2 + ((plev) / 12), 6));
	    break;

	  case 2:
	    (void)detect_monsters();
	    break;

	  case 3:
	    teleport_flag = TRUE;
	    teleport_dist = 10;
	    break;

	  case 4:
	    (void)lite_area(py, px,
			    damroll(2, (plev / 2)), (plev / 10) + 1);
	    break;

	  case 5:          /* treasure detection */
	    (void)detect_treasure();
	    break;

	  case 6:
	    (void)hp_player(damroll(2, 8));
	    if (p_ptr->cut > 15) {
		p_ptr->cut -= 15;
	    }
	    else {
		p_ptr->cut = 0;
	    }
	    break;

	  case 7:
	    (void)detect_object();
	    break;

	  case 8:
	    (void)detect_sdoor();
	    (void)detect_trap();
	    break;

	  case 9:
	    if (!get_dir(NULL, &dir)) return;
	    fire_ball(GF_POIS, dir, py, px,
		      10 + (plev / 2), 2);
	    break;

	  case 10:
	    if (!get_dir(NULL, &dir)) return;
	    (void)confuse_monster(dir, py, px, plev);
	    break;

	  case 11:
	    if (!get_dir(NULL, &dir)) return;
	    fire_bolt(GF_ELEC, dir, py, px, damroll(3+((plev-5)/5),8));
	    break;

	  case 12:
	    (void)td_destroy();
	    break;

	  case 13:
	    if (!get_dir(NULL, &dir)) return;
	    (void)sleep_monster(dir, py, px);
	    break;

	  case 14:
	    teleport_flag = TRUE;
	    teleport_dist = plev * 5;
	    break;

	  case 15:
	    if (!get_dir(NULL, &dir)) return;
	    msg_print("A line of blue shimmering light appears.");
	    lite_line(dir, py, px);
	    break;

	  case 16:
	    if (!get_dir(NULL, &dir)) return;
	    fire_bolt(GF_COLD, dir, py, px, damroll(5+((plev-5)/4),8));
	    break;

	  case 17:
	    if (!get_dir(NULL, &dir)) return;
	    (void)wall_to_mud(dir, py, px);
	    break;

	  case 18:
	    satisfy_hunger();
	    break;

	  case 19:
	    (void)recharge(5);
	    break;

	  case 20:
	    (void)sleep_monsters1(py, px);
	    break;

	  case 21:
	    if (!get_dir(NULL, &dir)) return;
	    line_spell(GF_COLD, dir, py, px, damroll(3+((plev-5)/5),8));
	    break;

	  case 22:
	    if (!get_dir(NULL, &dir)) return;
	    (void)poly_monster(dir, py, px);
	    break;

	  case 23:
	    if (ident_spell()) combine_pack();
	    break;

	  case 24:
	    (void)sleep_monsters2();
	    break;

	  case 25:
	    if (!get_dir(NULL, &dir)) return;
	    fire_bolt(GF_FIRE, dir, py, px, damroll(8+((plev-5)/3),8));
	    break;

	  case 26:
	    if (!get_dir(NULL, &dir)) return;
	    (void)slow_monster(dir, py, px);
	    break;

	  case 27:
	    if (!get_dir(NULL, &dir)) return;
	    fire_ball(GF_COLD, dir, py, px,
		      30 + (plev), 2);
	    break;

	  case 28:
	    if (!get_dir(NULL, &dir)) return;
	    line_spell(GF_ELEC, dir, py, px, damroll(5+((plev-5)/4),8));
	    break;

	  case 29:
	    (void)recharge(40);
	    break;

	  case 30:
	    if (!get_dir(NULL, &dir)) return;
	    (void)teleport_monster(dir, py, px);
	    break;

	  case 31:
	    if (p_ptr->fast <= 0) {
		p_ptr->fast += randint(20) + plev;
	    }
	    else {
		p_ptr->fast += randint(5);
	    }
	    break;

	  case 32:
	    if (!get_dir(NULL, &dir)) return;
	    fire_ball(GF_FIRE, dir, py, px,
		      55 + (plev*3)/2, 2);
	    break;

	  case 33:
	    if (!get_dir(NULL, &dir)) return;
	    line_spell(GF_FIRE, dir, py, px, damroll(8+ ((plev-5)/3),8));
	    break;

	  case 34:
	    destroy_area(py, px, 15, TRUE);
	    break;

	  case 35:
	    (void)genocide(TRUE);
	    break;

	  case 36:
	    if (!get_dir(NULL,&dir)) return;
	    line_spell(GF_MISSILE, dir, py, px, damroll(8+((plev-5)/3),12));
	    break;

	  case 37:         /* door creation */
	    (void)door_creation();
	    break;

	  case 38:         /* Stair creation */
	    (void)stair_creation();
	    break;

	  case 39:         /* Teleport level */
	    (void)tele_level();
	    break;

	  case 40:         /* Earthquake */
	    earthquake(py, px, 10);
	    break;

	  case 41:         /* Word of Recall */
	    if (p_ptr->word_recall == 0) {
		p_ptr->word_recall = rand_int(20) + 15;
		msg_print("The air about you becomes charged...");
	    }
	    else {
		p_ptr->word_recall = 0;
		msg_print("A tension leaves the air around you...");
	    }
	    break;

	  case 42:         /* Acid Bolt */
	    if (!get_dir(NULL, &dir)) return;
	    bolt_or_beam(beam, GF_ACID, dir,
			 damroll(6+((plev-5)/4), 8));
	    break;

	  case 43:         /* Cloud kill */
	    if (!get_dir(NULL, &dir)) return;
	    fire_ball(GF_POIS, dir, py, px,
		      20 + (plev / 2), 3);
	    break;

	  case 44:         /* Acid Ball */
	    if (!get_dir(NULL, &dir)) return;
	    fire_ball(GF_ACID, dir, py, px,
		      40 + (plev), 2);
	    break;

	  case 45:         /* Ice Storm */
	    if (!get_dir(NULL, &dir)) return;
	    fire_ball(GF_COLD, dir, py, px,
		      70 + (plev*3)/2, 3);
	    break;

	  case 46:         /* Meteor Swarm */
	    if (!get_dir(NULL, &dir)) return;
	    fire_ball(GF_METEOR, dir, py, px,
		      65 + (plev*3)/2, 3);
	    break;

	  case 47:         /* Mana Storm */
	    if (!get_dir(NULL, &dir)) return;
	    fire_ball(GF_MANA, dir, py, px, 300, 2);
	    break;

	  case 48:         /* Detect Evil */
	    (void)detect_evil();
	    break;

	  case 49:         /* Detect Enchantment */
	    (void)detect_magic();
	    break;

	  case 50:
	    recharge(100);
	    break;

	  case 51:
	    (void)genocide(TRUE);
	    break;

	  case 52:
	    (void)mass_genocide(TRUE);
	    break;

	  case 53:
	    p_ptr->oppose_fire += randint(20) + 20;
	    break;

	  case 54:
	    p_ptr->oppose_cold += randint(20) + 20;
	    break;

	  case 55:
	    p_ptr->oppose_acid += randint(20) + 20;
	    break;

	  case 56:
	    p_ptr->oppose_elec += randint(20) + 20;
	    break;

	  case 57:
	    p_ptr->oppose_pois += randint(20) + 20;
	    break;

	  case 58:
	    p_ptr->oppose_fire += randint(20) + 20;
	    p_ptr->oppose_cold += randint(20) + 20;
	    p_ptr->oppose_elec += randint(20) + 20;
	    p_ptr->oppose_pois += randint(20) + 20;
	    p_ptr->oppose_acid += randint(20) + 20;
	    break;

	  case 59:
	    hp_player(10);      /* XXX */
	    p_ptr->hero += randint(25) + 25;
	    break;

	  case 60:
	    p_ptr->shield += randint(20) + 30;
	    msg_print("A mystic shield forms around your body!");
	    break;

	  case 61:
	    hp_player(30);      /* XXX */
	    p_ptr->shero += randint(25) + 25;
	    break;

	  case 62:
	    if (p_ptr->fast <= 0) {
		p_ptr->fast += randint(30) + 30 + plev;
	    }
	    else {
		p_ptr->fast += randint(5);
	    }
	    break;

	  case 63:
	    if (randint(9)==1)
	      destroy_area(py,px,15,TRUE);
	    else
	      p_ptr->invuln += randint(8) + 8;
	    break;

	  default:
	    break;
	}

	/* A spell was cast */
	if (choice < 32) {
	    if (!(spell_worked1 & (1L << choice))) {
		spell_worked1 |= (1L << choice);
		p_ptr->exp += s_ptr->sexp << 2;
		check_experience();
	    }
	}
	else {
	    if (!(spell_worked2 & (1L << (choice - 32)))) {
		 spell_worked2 |= (1L << (choice - 32));
		p_ptr->exp += s_ptr->sexp << 2;
		check_experience();
	    }
	}
    }

    /* Take a turn */
    energy_use = 100;

    /* Use some mana */
    if (s_ptr->smana > p_ptr->cmana) {
	msg_print("You faint from the effort!");
	p_ptr->paralysis = randint((int)(5 * (s_ptr->smana - p_ptr->cmana)));
	p_ptr->cmana = 0;
	p_ptr->cmana_frac = 0;
	if (rand_int(3) == 0) {
	    msg_print("You have damaged your health!");
	    (void)dec_stat(A_CON, 15 + randint(10), (rand_int(3) == 0));
	}
    }
    else {
	p_ptr->cmana -= s_ptr->smana;
    }

    /* Update stuff */
    p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);
    
    /* Redraw mana */
    p_ptr->redraw |= (PR_MANA);
}



/*
 * Pray
 *
 * Note that "Holy Orb" damage is now based on "plev" and not "wisdom".
 * This will make it weaker early in the game and stronger later.
 */
void do_cmd_pray(void)
{
    int i, j, item_val, dir;
    int choice, chance;
    spell_type  *s_ptr;
    inven_type   *i_ptr;

    int plev = p_ptr->lev;

    energy_use = 0;

    if (cp_ptr->spell_stat != A_WIS) {
	msg_print("Pray hard enough and your prayers may be answered.");
	return;
    }

    if (p_ptr->blind || no_lite()) {
	msg_print("You cannot see!");
	return;
    }

    if (p_ptr->confused) {
	msg_print("You are too confused!");
	return;
    }

    if (!find_range(TV_PRAYER_BOOK, &i, &j)) {
	msg_print("You are not carrying any Holy Books!");
	return;
    }


    /* Choose a book */
    if (!get_item(&item_val, "Use which Holy Book? ", i, j, FALSE)) {
	if (item_val == -2) msg_print("You are not carrying any prayer books!");
	return;
    }


    /* Choose a spell */
    if (!cast_spell("Recite which prayer?", item_val, &choice)) {
	if (choice == -1) msg_print("You don't know any prayers in that book.");
	return;
    }


    /* Access the spell */
    s_ptr = &magic_spell[p_ptr->pclass-1][choice];

    /* Spell failure chance */
    chance = spell_chance(choice);

    /* Check for failure */
    if (rand_int(100) < chance) {
	if (flush_failure) flush();
	msg_print("You failed to concentrate hard enough!");
    }

    /* Success */
    else {

	switch (choice + 1) {

	  case 1:
	    (void)detect_evil();
	    break;

	  case 2:
	    (void)hp_player(damroll(2, 8));
	    if (p_ptr->cut > 10) {
		p_ptr->cut -= 10;
	    }
	    else {
		p_ptr->cut = 0;
	    }
	    break;

	  case 3:
	    bless(randint(12) + 12);
	    break;

	  case 4:
	    (void)remove_fear();
	    break;

	  case 5:
	    (void)lite_area(py, px,
			    damroll(2, (plev / 2)), (plev / 10) + 1);
	    break;

	  case 6:
	     if (!get_dir(NULL, &dir)) return;
	     fire_bolt(GF_HOLY_ORB, dir, py, px, damroll(3,3));
	    break;

	  case 7:
	    (void)detect_trap();
	    (void)detect_sdoor();
	    break;

	  case 8:
	    (void)slow_poison();
	    break;

	  case 9:
	    if (!get_dir(NULL, &dir)) return;
	    (void)fear_monster(dir, py, px, plev);
	    break;

	  case 10:
	    teleport_flag = TRUE;
	    teleport_dist = plev * 3;
	    break;

	  case 11:
	    (void)hp_player(damroll(4, 8));
	    if (p_ptr->cut > 40) {
		p_ptr->cut = (p_ptr->cut / 2) - 20;
	    }
	    else {
		p_ptr->cut = 0;
	    }
	    break;

	  case 12:
	    bless(randint(24) + 24);
	    break;

	  case 13:
	    (void)sleep_monsters1(py, px);
	    break;

	  case 14:
	    satisfy_hunger();
	    break;

	  case 15:
	    remove_curse();
	    break;

	  case 16:
	    p_ptr->oppose_fire += randint(10) + 10;
	    p_ptr->oppose_cold += randint(10) + 10;
	    break;

	  case 17:
	    (void)cure_poison();
	    break;

	  case 18:
	    if (!get_dir(NULL, &dir)) return;
	    fire_ball(GF_HOLY_ORB, dir, py, px,
		      (damroll(3,6) + plev +
		       (plev / ((p_ptr->pclass == 2) ? 2 : 4))),
		      ((plev < 30) ? 2 : 3));
	    break;

	  case 19:
	    (void)hp_player(damroll(6, 8));
	    p_ptr->cut = 0;
	    break;

	  case 20:
	    detect_inv2(randint(24) + 24);
	    break;

	  case 21:
	    (void)protect_evil();
	    break;

	  case 22:
	    earthquake(py, px, 10);
	    break;

	  case 23:
	    map_area();
	    break;

	  case 24:
	    (void)hp_player(damroll(8, 8));
	    p_ptr->cut = 0;
	    p_ptr->stun = 0;
	    break;

	  case 25:
	    (void)turn_undead();
	    break;

	  case 26:
	    bless(randint(48) + 48);
	    break;

	  case 27:
	    /* Dispel (undead) monsters */
	    (void)dispel_monsters((int)(3 * plev), FALSE, TRUE);
	    break;

	  case 28:
	    (void)hp_player(300);
	    p_ptr->stun = 0;
	    p_ptr->cut = 0;
	    break;

	  case 29:
	    /* Dispel (evil) monsters */
	    (void)dispel_monsters((int)(3 * plev), TRUE, FALSE);
	    break;

	  case 30:
	    warding_glyph();
	    break;

	  case 31:
	    /* Dispel (evil) monsters */
	    (void)dispel_monsters((int)(4 * plev), TRUE, FALSE);
	    (void)hp_player(1000);
	    (void)remove_fear();
	    (void)cure_poison();
	    p_ptr->stun = 0;
	    p_ptr->cut = 0;
	    break;

	  case 32:
	    (void)detect_monsters();
	    break;

	  case 33:
	    (void)detection();
	    break;

	  case 34:
	    if (ident_spell()) combine_pack();
	    break;

	  case 35:         /* probing */
	    (void)probing();
	    break;

	  case 36:         /* Clairvoyance */
	    wiz_lite();
	    break;

	  case 37:
	    (void)hp_player(damroll(4, 8));
	    p_ptr->cut = 0;
	    break;

	  case 38:
	    (void)hp_player(damroll(8, 8));
	    p_ptr->cut = 0;
	    p_ptr->stun = 0;
	    break;

	  case 39:
	    (void)hp_player(2000);
	    p_ptr->stun = 0;
	    p_ptr->cut = 0;
	    break;

	  case 40:         /* restoration */
	    if (res_stat(A_STR)) {
		msg_print("You feel warm all over.");
	    }
	    if (res_stat(A_INT)) {
		msg_print("You have a warm feeling.");
	    }
	    if (res_stat(A_WIS)) {
		msg_print("You feel your wisdom returning.");
	    }
	    if (res_stat(A_DEX)) {
		msg_print("You feel less clumsy.");
	    }
	    if (res_stat(A_CON)) {
		msg_print("You feel your health returning!");
	    }
	    if (res_stat(A_CHR)) {
		msg_print("You feel your looks returning.");
	    }
	    break;

	  case 41:         /* rememberance */
	    (void)restore_level();
	    break;

	  case 42:         /* dispel undead */
	    /* Dispel (undead) monsters */
	    (void)dispel_monsters((int)(4 * plev), FALSE, TRUE);
	    break;

	  case 43:         /* dispel evil */
	    /* Dispel (evil) monsters */
	    (void)dispel_monsters((int)(4 * plev), TRUE, FALSE);
	    break;

	  case 44:         /* banishment */
	    if (banish_evil(100)) {
		msg_print("The Power of your god banishes evil!");
	    }
	    break;

	  case 45:         /* word of destruction */
	    destroy_area(py, px, 15, TRUE);
	    break;

	  case 46:         /* annihilation */
	    if (!get_dir(NULL, &dir)) return;
	    drain_life(dir, py, px, 200);
	    break;

	  case 47:         /* unbarring ways */
	    (void)td_destroy();
	    break;

	  case 48:         /* recharging */
	    (void)recharge(15);
	    break;

	  case 49:         /* remove (all) curses */
	    (void)remove_all_curse();
	    break;

	  case 50:         /* enchant weapon */
	    (void)enchant_spell(rand_int(4) + 1, rand_int(4) + 1, 0);
	    break;

	  case 51:         /* enchant armor */
	    (void)enchant_spell(0, 0, rand_int(3) + 2);
	    break;

	  /* Elemental brand -- only wielded weapon */
	  case 52:

	    i_ptr = &inventory[INVEN_WIELD];

	    /* you can never modify artifacts / ego-items */
	    /* you can never modify broken / cursed items */
	    if ((i_ptr->tval) &&
		(!artifact_p(i_ptr)) && (!ego_item_p(i_ptr)) &&
		(!broken_p(i_ptr)) && (!cursed_p(i_ptr))) {

		cptr act;
		char tmp_str[100];

		if (rand_int(2)) {
		    act = " is covered in a fiery shield!";
		    i_ptr->name2 = EGO_FT;
		    i_ptr->flags1 |= (TR1_BRAND_FIRE);
		    i_ptr->flags2 |= (TR2_RES_FIRE);
		    i_ptr->flags3 |= (TR3_IGNORE_FIRE);
		}
		else {
		    act = " glows deep, icy blue!";
		    i_ptr->name2 = EGO_FB;
		    i_ptr->flags1 |= (TR1_BRAND_COLD);
		    i_ptr->flags2 |= (TR2_RES_COLD);
		    i_ptr->flags3 |= (TR3_IGNORE_COLD);
		}

		objdes(tmp_str, i_ptr, 0);

		message("Your ", 0x02);
		message(tmp_str, 0x02);
		message(act, 0);

		enchant(i_ptr, rand_int(3) + 4, ENCH_TOHIT|ENCH_TODAM);
	    }
	    else {
		if (flush_failure) flush();
		msg_print("The Branding failed.");
	    }
	    break;

	  case 53:         /* blink */
	    teleport_flag = TRUE;
	    teleport_dist = 10;
	    break;

	  case 54:         /* teleport */
	    teleport_flag = TRUE;
	    teleport_dist = plev * 8;
	    break;

	  case 55:         /* teleport away */
	    if (!get_dir(NULL, &dir)) return;
	    (void)teleport_monster(dir, py, px);
	    break;

	  case 56:         /* teleport level */
	    (void)tele_level();
	    break;

	  case 57:         /* word of recall */
	    if (p_ptr->word_recall == 0) {
		p_ptr->word_recall = rand_int(20) + 15;
		msg_print("The air about you becomes charged...");
	    }
	    else {
		p_ptr->word_recall = 0;
		msg_print("A tension leaves the air around you...");
	    }
	    break;

	  case 58:         /* alter reality */
	    new_level_flag = TRUE;
	    break;

	  default:
	    break;
	}

	/* End of prayers.                               */
	if (choice < 32) {
	    if (!(spell_worked1 & (1L << choice))) {
		spell_worked1 |= (1L << choice);
		p_ptr->exp += s_ptr->sexp << 2;
		check_experience();
	    }
	}
	else {
	    if (!(spell_worked2 & (1L << (choice - 32)))) {
		 spell_worked2 |= (1L << (choice - 32));
		 p_ptr->exp += s_ptr->sexp << 2;
		 check_experience();
	    }
	}
    }

    /* Take a turn */
    energy_use = 100;

    /* Reduce mana */
    if (s_ptr->smana > p_ptr->cmana) {
	msg_print("You faint from fatigue!");
	p_ptr->paralysis = randint((int)(5 * (s_ptr->smana - p_ptr->cmana)));
	p_ptr->cmana = 0;
	p_ptr->cmana_frac = 0;
	if (rand_int(3) == 0) {
	    msg_print("You have damaged your health!");
	    (void)dec_stat(A_CON, 15 + randint(10), (rand_int(3) == 0));
	}
    }
    else {
	p_ptr->cmana -= s_ptr->smana;
    }

    /* Update stuff */
    p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);
    
    /* Redraw mana */
    p_ptr->redraw |= (PR_MANA);
}

