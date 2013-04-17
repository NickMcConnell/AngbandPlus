/* File: cmd5.c */

/* Purpose: Spell/Prayer commands */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */


#include "angband.h"


extern void do_cmd_rerate(void);
extern void chaos_feature_shuffle(void);
extern bool item_tester_hook_armour(object_type *o_ptr);


/*
 * Allow user to choose a spell from the given book.
 *
 * If a valid spell is chosen, saves it in '*sn' and returns TRUE
 * If the user hits escape, returns FALSE, and set '*sn' to -1
 * If there are no legal choices, returns FALSE, and sets '*sn' to -2
 *
 * The "prompt" should be "cast", "recite", or "study"
 * The "known" should be TRUE for cast/pray, FALSE for study
 */
static int get_spell(int *sn, cptr prompt, int sval, bool known, int school_no)
{
	int		i;
	int		spell = -1;
	int		num = 0;
	int		ask;

	byte		spells[64];

	bool		flag, redraw, okay;
	char		choice;

	magic_type	*s_ptr;

	char		out_val[160];

	cptr p = "spell";

 #ifdef ALLOW_REPEAT /* TNB */
 
     /* Get the spell, if available */
     if (repeat_pull(sn)) {
         
         /* Verify the spell */
         if (spell_okay(*sn, known, school_no)) {
                    
             /* Success */
             return (TRUE);
         }
     }
     
 #endif /* ALLOW_REPEAT -- TNB */
 

	/* Extract spells */
	for (spell = 0; spell < 32; spell++)
	{
		/* Check for this spell */
		if ((spell_flags[sval] & (1L << spell)))
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
		if (spell_okay(spells[i], known, school_no)) okay = TRUE;
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
	strnfmt(out_val, 78, "(%^ss %c-%c, *=List, ESC=exit) %^s which %s? ",
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
				Term_save();

				/* Display a list of spells */
				print_spells(spells, num, 1, 20, school_no);
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
		spell = spells[i];

		/* Require "okay" spells */
		if (!spell_okay(spell, known, school_no))
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
			s_ptr = &mp_ptr->info[school_no][spell%32];

			/* Prompt */
			strnfmt(tmp_val, 78, "%^s %s (%d mana, %d%% fail)? ",
				prompt, spell_names[school_no][spell%32],
				s_ptr->smana, spell_chance(spell,school_no));

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
	(*sn) = spell;

 #ifdef ALLOW_REPEAT /* TNB */
 
 	repeat_push(*sn);
 
 #endif /* ALLOW_REPEAT -- TNB */
 
	/* Success */
	return (TRUE);
}

/*
 * Determine if a cantrip is "okay" for the player to cast
 */
bool cantrip_okay(int fav)
{
	byte plev = skill_set[SKILL_HEDGE].value/2;
	cantrip_type *s_ptr;

	/* Make sure we can always use at least one favour */
	if (plev == 0) plev++;

	/* Access the favour */
    s_ptr = &(cantrip_info[fav]);

	/* Cantrip is illegal */
	if (s_ptr->minskill > plev) return (FALSE);
	return (TRUE);
}

/*
 * Print a list of cantrips (for casting or learning)
 */
void print_cantrips(byte *spells, int num, int y, int x)
{
	int                     i, spell;

	byte plev = skill_set[SKILL_HEDGE].value/2;
	cantrip_type              *s_ptr;

	cptr            comment;

	char info[80];

	char            out_val[160];

	if (plev == 0) plev++;

    /* Title the list */
    prt("", y, x);
	put_str("Name", y, x + 5);
	put_str("Sk Fail Info", y, x + 40);


    /* Dump the favours */
	for (i = 0; i < num; i++)
	{
		/* Access the favour */
		spell = spells[i];

		s_ptr = &(cantrip_info[spell]);

		get_cantrip_info(info,spell);

		comment = info;

		if (s_ptr->minskill > plev)
		{
			comment = " too hard";
		}
			
		/* Dump the favour --(-- */
		sprintf(out_val, "  %c) %-35s%2d %3d%%%s",
		I2A(i), cantrip_names[spell], /* spell */
		s_ptr->minskill*2, cantrip_chance(spell), comment);
		prt(out_val, y + i + 1, x);
	}

	/* Clear the bottom line */
	prt("", y + i + 1, x);
}

/*
 * Allow user to choose a cantrip from the given charm.
 *
 * If a valid cantrip is chosen, saves it in '*sn' and returns TRUE
 * If the user hits escape, returns FALSE, and set '*sn' to -1
 * If there are no legal choices, returns FALSE, and sets '*sn' to -2
 *
 */
int get_cantrip(int *sn, int sval)
{
	int		i;
	int		spell = -1;
	int		num = 0;
	int		ask;

	byte		spells[64];

	bool		flag, redraw, okay;
	char		choice;

	cantrip_type	*s_ptr;

	char		out_val[160];

 #ifdef ALLOW_REPEAT /* TNB */
 
     /* Get the spell, if available */
     if (repeat_pull(sn)) {
         
         /* Verify the spell */
         if (cantrip_okay(*sn)) {
                    
             /* Success */
             return (TRUE);
         }
     }
     
 #endif /* ALLOW_REPEAT -- TNB */
 

	/* Extract spells */
	for (spell = 0; spell < 32; spell++)
	{
		/* Check for this spell */
		if ((cantrip_flags[sval] & (1L << spell)))
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
		if (cantrip_okay(spells[i])) okay = TRUE;
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
	strnfmt(out_val, 78, "(%c-%c, *=List, ESC=exit) Cast which cantrip? ",
		I2A(0), I2A(num - 1));

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
				Term_save();

				/* Display a list of spells */
				print_cantrips(spells, num, 1, 20);
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
		spell = spells[i];

		/* Require "okay" spells */
		if (!cantrip_okay(spell))
		{
			bell();
			msg_print("You may not cast that cantrip.");
			continue;
		}

		/* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Access the spell */
			s_ptr = &(cantrip_info[spell]);

			/* Prompt */
			strnfmt(tmp_val, 78, "cast %s (%d%% fail)? ",
				cantrip_names[spell],
				cantrip_chance(spell));

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
	(*sn) = spell;

 #ifdef ALLOW_REPEAT /* TNB */
 
 	repeat_push(*sn);
 
 #endif /* ALLOW_REPEAT -- TNB */
 
	/* Success */
	return (TRUE);
}

/*
 * Determine if a favour is "okay" for the player to cast
 */
bool favour_okay(int fav,  int sphere)
{
	byte plev = skill_set[SKILL_SHAMAN].value/2;
	favour_type *s_ptr;

	/* Make sure we can always use at least one favour */
	if (plev == 0) plev++;

	/* Access the favour */
    s_ptr = &(favour_info[sphere][fav]);

	/* Favour is too hard is illegal */
	if (s_ptr->minskill > plev) return (FALSE);
	return (TRUE);
}

/*
 * Returns cantrip chance of failure (much simpler than 'spell_chance')
 */
s16b cantrip_chance(int ctp)
{
	int             chance, minfail;
	byte plev = skill_set[SKILL_HEDGE].value/2;

	cantrip_type      *s_ptr;


	/* Access the spell */
    s_ptr = &(cantrip_info[ctp]);

	/* Extract the base spell failure rate */
	chance = s_ptr->sfail;

	/* Reduce failure rate by "effective" level adjustment */
	chance -= 3 * (plev - s_ptr->minskill);

	/* Reduce failure rate by INT/WIS adjustment */
	chance -= 3 * (adj_mag_stat[p_ptr->stat_ind[A_INT]] - 1);

	/* Extract the minimum failure rate */
	minfail = adj_mag_fail[p_ptr->stat_ind[A_INT]];

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
 * Allow user to choose a favour from the given spirit.
 *
 * If a valid favour is chosen, saves it in '*sn' and returns TRUE
 * If the user hits escape, returns FALSE, and set '*sn' to -1
 * If there are no legal choices, returns FALSE, and sets '*sn' to -2
 *
 */
static int get_favour(int *sn, int spirit,int sphere)
{
	int		i;
	int		spell = -1;
	int		num = 0;
	int		ask;

	byte		spells[64];

	bool		flag, redraw, okay;
	char		choice;

	favour_type	*s_ptr;

	char		out_val[160];


 #ifdef ALLOW_REPEAT /* TNB */
 
     /* Get the spell, if available */
     if (repeat_pull(sn)) {
         
         /* Verify the spell */
         if (favour_okay(*sn,sphere)) {
                    
             /* Success */
             return (TRUE);
         }
     }
     
 #endif /* ALLOW_REPEAT -- TNB */
 

	/* Extract spells */
	for (spell = 0; spell < 32; spell++)
	{
		/* Check for this spell */
		if ((spirits[spirit].favour_flags & (1L << spell)))
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
		if (favour_okay(spells[i],sphere)) okay = TRUE;
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
	strnfmt(out_val, 78, "(%c-%c, *=List, ESC=exit) invoke which favour? ",
		I2A(0), I2A(num - 1));

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
				Term_save();

				/* Display a list of spells */
				print_favours(spells, num, 1, 20, sphere);
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
		spell = spells[i];

		/* Require "okay" spells */
		if (!favour_okay(spell, sphere))
		{
			bell();
			msg_print("You may not invoke that favour.");
			continue;
		}

		/* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Access the spell */
			s_ptr = &(favour_info[sphere][spell]);

			/* Prompt */
			strnfmt(tmp_val, 78, "%s (%d mana, %d%% fail)? ",
				favour_names[sphere][spell],
				s_ptr->annoy_inc, favour_chance(spell,sphere));

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
	(*sn) = spell;

 #ifdef ALLOW_REPEAT /* TNB */
 
 	repeat_push(*sn);
 
 #endif /* ALLOW_REPEAT -- TNB */
 
	/* Success */
	return (TRUE);
}

/*
 * Print a list of favours (for invoking)
 */
void print_favours(byte *spells, int num, int y, int x, int sphere)
{
	int                     i, spell;

	byte plev = skill_set[SKILL_SHAMAN].value/2;
	favour_type              *s_ptr;

	cptr            comment;

	char info[80];

	char            out_val[160];

	if (plev == 0) plev++;
    if ((sphere<0 || sphere>MAX_SPHERE - 1) && cheat_wzrd)
	msg_print ("Warning! print_favours called with null sphere");

    /* Title the list */
    prt("", y, x);
	put_str("Name", y, x + 5);
	put_str("Sk Fail Info", y, x + 40);


    /* Dump the favours */
	for (i = 0; i < num; i++)
	{
		/* Access the favour */
		spell = spells[i];

		s_ptr = &(favour_info[sphere][spell]);

		get_favour_info(info,spell,sphere);

		comment = info;

		if (s_ptr->minskill > plev)
		{
			comment = " too hard";
		}
			
		/* Dump the favour --(-- */
		sprintf(out_val, "  %c) %-35s%2d %3d%%%s",
		I2A(i), favour_names[sphere][spell], /* sphere, spell */
		s_ptr->minskill*2, favour_chance(spell,sphere), comment);
		prt(out_val, y + i + 1, x);
	}

	/* Clear the bottom line */
	prt("", y + i + 1, x);
}

/*
 * Returns favour chance of failure (much simpler than 'spell_chance')
 */
s16b favour_chance(int fav,int sphere)
{
	int             chance, minfail;
	byte plev = skill_set[SKILL_SHAMAN].value/2;

	favour_type      *s_ptr;


	/* Access the spell */
    s_ptr = &(favour_info[sphere][fav]);

	/* Extract the base spell failure rate */
	chance = s_ptr->sfail;

	/* Reduce failure rate by "effective" level adjustment */
	chance -= 3 * (plev - s_ptr->minskill);

	/* Reduce failure rate by INT/WIS adjustment */
	chance -= 3 * (adj_mag_stat[p_ptr->stat_ind[A_CHR]] - 1);

	/* Extract the minimum failure rate */
	minfail = adj_mag_fail[p_ptr->stat_ind[A_CHR]];

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
 * Allow user to choose a spirit.
 *
 * If a valid spell is chosen, saves it in '*sn' and returns TRUE
 * If the user hits escape, returns FALSE, and set '*sn' to -1
 * If there are no legal choices, returns FALSE, and sets '*sn' to -2
 *
 * The "prompt" should be "form a pact with" or "call upon"
 * The "call" should be TRUE for call upon, FALSE for call a pact with
 */
int get_spirit(int *sn, cptr prompt, bool call)
{
	int		i;
	int		spirit = -1;
	int		ask;
	bool		flag, redraw, okay;
	char		choice;
	char		out_val[160];
	int valid_spirits[MAX_SPIRITS],total;

 #ifdef ALLOW_REPEAT /* TNB */
 
     /* Get the spirit, if available */
     if (repeat_pull(sn)) {
         
         /* Verify the spirit */
         if (spirit_okay(*sn, call)) {
                    
             /* Success */
             return (TRUE);
         }
     }
     
 #endif /* ALLOW_REPEAT -- TNB */
 

	/* Assume no usable spells */
	okay = FALSE;

	/* Assume no spirits available */
	(*sn) = -2;

	/* Check for "okay" spirits */
	total=0;
	for (i = 0; i < MAX_SPIRITS; i++)
	{
			/* Look for "okay" spirits */
			if (spirit_okay(i, call))
			{
				okay = TRUE;
				valid_spirits[total++] = i;
			}
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


	/* Build a prompt (accept all spirits) */
	strnfmt(out_val, 78, "(%c-%c, *=List, ESC=exit) %^s which spirit? ",
		I2A(0), I2A(total - 1), prompt);

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
				Term_save();

				/* Display a list of spirits */
				print_spirits(valid_spirits,total,1, 20);
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
		if ((i < 0) || (i >= total))
		{
			bell();
			continue;
		}

		/* Require "okay" spells */
		if (!spirit_okay(valid_spirits[i], call))
		{
			bell();
			msg_format("You may not %s that spirit.", prompt);
			continue;
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
	(*sn) = valid_spirits[i];

 #ifdef ALLOW_REPEAT /* TNB */
 
 	repeat_push(*sn);
 
 #endif /* ALLOW_REPEAT -- TNB */
 
	/* Success */
	return (TRUE);
}


/*
 * Determine if a spirit is "okay" for the player to form a pact with to or call upon
 * Set call to true to test if a spirit can be called upon
 * Set call to false to test if a spirit can form a pact
 */
bool spirit_okay(int spirit, bool call)
{

	byte plev = skill_set[SKILL_SHAMAN].value/2;
	spirit_type *s_ptr;

	if (plev == 0) plev++;

	/* Access the spell */
    s_ptr = &(spirits[spirit]);

	/* Spirit is too powerful */
	if (s_ptr->minskill > plev) return (FALSE);

	/* Spirit has a pact */
    if (s_ptr->pact)
	{
		/* Okay to call upon, not to form a pact with */
		return (call);
	}

	/* Okay to form a pact with, not to call upon */
	return (!call);
}


/*
 * Print a list of spirits (for initiating to or calling upon)
 */
void print_spirits(int *valid_spirits,int num,int y, int x)
{
	int                     i;
	byte plev = skill_set[SKILL_SHAMAN].value/2;
	spirit_type              *s_ptr;
	cptr            comment;
	char            out_val[160];
	char			full_name[80];
	if(plev == 0) plev++;
    /* Title the list */
    prt("", y, x);
	put_str("Name", y, x + 5);
	put_str("Info", y, x + 48);


    /* Dump the spirits */
	for (i = 0; i < num; i++)
	{
		/* Access the spirit */
		s_ptr = &(spirits[valid_spirits[i]]);

		/* Analyze the spirit */
		if (s_ptr->minskill > plev)
		{
			comment = " too powerful";
		}
		else if(!(s_ptr->pact))
		{
			comment = " no pact";
		}
		else if(s_ptr->annoyance > 15)
		{
			comment = " furious";
		}
		else if(s_ptr->annoyance > 8)
		{
			comment = " angry";
		}
		else if(s_ptr->annoyance > 3)
		{
			comment = " annoyed";
		}
		else if(s_ptr->annoyance > 0)
		{
			comment = " irritated";
		}
		else
		{
			comment = " placated";
		}
		/* Pre-process the spirit name and description */
		sprintf(full_name,"%s, %s",s_ptr->name,s_ptr->desc);
		/* Now insert it into the line */
		sprintf(out_val, "  %c) %-42s%s",
		I2A(i), full_name,comment);
		prt(out_val, y + i + 1, x);
	}

	/* Clear the bottom line */
	prt("", y + i + 1, x);
}


void rustproof(void)
{
	int		item;

	object_type	*o_ptr;

	char		o_name[80];

	/* Select a piece of armour */
	item_tester_hook = item_tester_hook_armour;

	/* Get an item (from equip or inven or floor) */
	if (!get_item(&item, "Rustproof which piece of armour? ", TRUE, TRUE, TRUE))
	{
		if (item == -2) msg_print("You have nothing to rustproof.");
		return;
	}

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
 * Peruse the spells/prayers in a Book
 *
 * Note that *all* spells in the book are listed
 *
 * Note that browsing is allowed while confused or blind,
 * and in the dark, primarily to allow browsing in stores.
 */
void do_cmd_browse(int item)
{
	int		sval;
	int		spell = -1;
	int		num = 0;

	byte		spells[64];

	object_type	*o_ptr;


	/* Restrict choices to "useful" books */
	item_tester_tval = TV_SORCERY_BOOK;

	/* Get an item if we do not already have one */
	if(item < 0)
	{
		/* Get an item (from inven or floor) */
		if (!get_item(&item, "Browse which book? ", FALSE, TRUE, TRUE))
		{
			if (item == -2) msg_print("You have no books that you can read.");
			return;
		}
	}

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

	item_tester_tval = TV_SORCERY_BOOK;
	if(!item_tester_okay(o_ptr))
	{
		msg_print("You can't read that.");
		item_tester_tval = 0;
		return;
	}
	item_tester_tval = 0;

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
		if ((spell_flags[sval] & (1L << spell)))
		{
			/* Collect this spell */
			spells[num++] = spell;
		}
	}


	/* Save the screen */
	Term_save();

	/* Display the spells */
	print_spells(spells, num, 1, 20, (o_ptr->tval-90));

	/* Clear the top line */
	prt("", 0, 0);

	/* Prompt user */
	put_str("[Press any key to continue]", 0, 23);

	/* Wait for key */
	(void)inkey();

	/* Restore the screen */
	Term_load();
}




/*
 * Study a book to gain a new spell/prayer
 */
void do_cmd_study(void)
{
	int	i, item, sval;
	int	spell_school = 0;
	int	spell = -1;

	cptr p = "spell";

	object_type             *o_ptr;

	if (p_ptr->blind)
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
	item_tester_tval = TV_SORCERY_BOOK;

	/* Get an item (from inven or floor) */
	if (!get_item(&item, "Study which book? ", FALSE, TRUE, TRUE))
	{
		if (item == -2) msg_print("You have no books that you can read.");
		return;
	}

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

	spell_school=o_ptr->tval - 90;

	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();

	/* Ask for a spell, allow cancel */
	if (!get_spell(&spell, "study", sval, (bool)FALSE,spell_school)
	&& (spell == -1)) return;

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


	/* Learn the spell */
	spell_learned[spell_school] |= (1L << spell);

	/* Find the next open entry in "spell_order[]" */
	for (i = 0; i < 128; i++)
	{
		/* Stop at the first empty space */
		if (spell_order[i] == 255) break;
	}

	/* Add the spell to the known list */
	spell_order[i++] = spell;

	/* Mention the result */
	msg_format("You have learned the %s of %s.",
		p, spell_names
		[spell_school][spell%32]);

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

void do_poly_wounds(void)
{

    s16b wounds = p_ptr->cut, hit_p = (p_ptr->mhp - p_ptr->chp);
    s16b change = damroll(skill_set[SKILL_TOUGH].value/2, 5);
    bool Nasty_effect = (randint(5)==1);

    if (!(wounds || hit_p || Nasty_effect)) return;

    if (Nasty_effect)
        {
            msg_print("A new wound was created!");
            take_hit(change, "a polymorphed wound");
            set_cut(change);
        }
    else
        {
            msg_print("Your wounds are polymorphed into less serious ones.");
            hp_player(change);
            set_cut((p_ptr->cut)-(change/2));
        }
}

void do_poly_self(void)
{
int effects = randint(2);
int tmp = 0;
int new_race;
int more_effects = TRUE;
char buf[1024];

msg_print("You feel a change coming over you...");

while (effects-- && more_effects)
    {
        switch (randint(12))
        {
        case 1: case 2:
            do_poly_wounds();
            break;
        case 3: case 4:
            (void) gain_chaos_feature(0);
            break;
		case 5: case 6: case 7: /* Racial polymorph! Uh oh... */
          {
            do { new_race = randint(MAX_RACES) -1; } while (new_race == p_ptr->prace);

            msg_format("You turn into a%s %s!",
                ((new_race == RACE_ELF
                  || new_race == RACE_IMP)?"n":""),
                race_info[new_race].title);

                p_ptr->prace = new_race;
                rp_ptr = &race_info[p_ptr->prace];

				/* Access the "race" pref file */
				sprintf(buf, "%s.prf", rp_ptr->title);

				/* Process that file */
				process_pref_file(buf);

				/* Access the "font" or "graf" pref file, based on "use_graphics" */
				sprintf(buf, "%s-%s.prf", (use_graphics ? "graf" : "font"), ANGBAND_SYS);

				/* Process that file */
				process_pref_file(buf);

                /* Experience factor */
                p_ptr->expfact = rp_ptr->r_exp;

            /* Calculate the height/weight for males */
            if (p_ptr->psex == SEX_MALE)
            {
                p_ptr->ht = randnor(rp_ptr->m_b_ht, rp_ptr->m_m_ht);
                p_ptr->wt = randnor(rp_ptr->m_b_wt, rp_ptr->m_m_wt);
            }

            /* Calculate the height/weight for females */
                else if (p_ptr->psex == SEX_FEMALE)
            {
                p_ptr->ht = randnor(rp_ptr->f_b_ht, rp_ptr->f_m_ht);
                p_ptr->wt = randnor(rp_ptr->f_b_wt, rp_ptr->f_m_wt);
            }


            p_ptr->redraw |= (PR_BASIC);

            p_ptr->update |= (PU_BONUS);

            handle_stuff();
          }
          lite_spot(py, px);
          more_effects = FALSE; /* Stop here! */
          break;
        case 8: /* Purposedly "leaks" into default */
            msg_print("You polymorph into an abomination!");
            while (tmp < 6)
            {
                (void)dec_stat(tmp, randint(6)+6, (randint(3)==1));
                tmp++;
            }
            if (randint(6)==1)
            {
                msg_print("You find living difficult in your present form!");
                take_hit(damroll(randint(skill_set[SKILL_TOUGH].value/2),skill_set[SKILL_TOUGH].value/2), "a lethal chaos feature");
            }
            /* No break; here! */
        default:
            chaos_feature_shuffle();
    }
    }

}

static void phlogiston (void)
{

    int max_flog = 0;
    object_type * o_ptr = &inventory[INVEN_LITE];

	/* It's a lamp */
    if ((o_ptr->tval == TV_LITE) && (o_ptr->sval == SV_LITE_LANTERN))
	{
        max_flog = FUEL_LAMP;
	}

	/* It's a torch */
    else if ((o_ptr->tval == TV_LITE) && (o_ptr->sval == SV_LITE_TORCH))
	{
        max_flog = FUEL_TORCH;
	}

	/* No torch to refill */
	else
	{
        msg_print("You are not wielding anything which uses phlogiston.");
        return;
	}

    if (o_ptr->pval >= max_flog)
    {
        msg_print("No more phlogiston can be put in this item.");
        return;
    }

	/* Refuel */
    o_ptr->pval += (max_flog / 2);

	/* Message */
    msg_print("You add phlogiston to your light item.");

	/* Comment */
    if (o_ptr->pval >= max_flog)
	{
        o_ptr->pval = max_flog;
        msg_print("Your light item is full.");
	}


	/* Recalculate torch */
	p_ptr->update |= (PU_TORCH);
}
    

static void brand_weapon(int brand_type)
{
	object_type *o_ptr;

	o_ptr = &inventory[INVEN_WIELD];

	/* you can never modify artifacts / ego-items */
    /* you can never modify cursed items */
    /* TY: You _can_ modify broken items (if you're silly enough) */
	if ((o_ptr->k_idx) &&
	    (!artifact_p(o_ptr)) && (!ego_item_p(o_ptr)) &&
        (!(o_ptr->art_name)) && (!cursed_p(o_ptr)))
	{
		cptr act = NULL;

		char o_name[80];
        object_desc(o_name, o_ptr, FALSE, 0); /* Let's get the name before
                                                it is changed... */

    switch (brand_type)
    {
        case 4:
            act = "seems very unstable now.";
            o_ptr->name2 = EGO_PLANAR;
            o_ptr->pval = randint(2);
            break;
        case 3:
            act = "thirsts for blood!";
            o_ptr->name2 = EGO_VAMPIRIC;
            break;
        case 2:
            act = "is coated with poison.";
            o_ptr->name2 = EGO_BRAND_POIS;
            break;
        case 1:
            act = "is engulfed in raw chaos!";
            o_ptr->name2 = EGO_CHAOTIC;
            break;
        default:
		if (rand_int(100) < 25)
		{
			act = "is covered in a fiery shield!";
			o_ptr->name2 = EGO_BRAND_FIRE;
		}

		else
		{
			act = "glows deep, icy blue!";
			o_ptr->name2 = EGO_BRAND_COLD;
		}
    }



		msg_format("Your %s %s", o_name, act);

		enchant(o_ptr, rand_int(3) + 4, ENCH_TOHIT | ENCH_TODAM);
	}

	else
	{
		if (flush_failure) flush();

        msg_print("The Branding failed.");
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
	    if (i-5) fire_ball(GF_SHARD, i,
				175, 2);

		for (i = 1; i < 10; i++)
	    if (i-5) fire_ball(GF_MANA, i,
				175, 3);
		
		for (i = 1; i < 10; i++)
	    if (i-5) fire_ball(GF_NUKE, i,
				175, 4);
	}
    else
    {
        msg_format("You %s the %s too close to a wall!",
            "cast",
            "spell");
        msg_print("There is a loud explosion!");
        destroy_area(py, px, 20+(skill_set[SKILL_THAUMATURGY].value/2), TRUE);
        msg_print("The dungeon collapses...");
        take_hit(100 + (randint(150)), "a suicidal Call the Void");
    }
}

 /* Fetch an item (teleport it right underneath the caster) */
 void fetch(int dir, int wgt, bool require_los)
 {
       int ty, tx, i;
       bool flag;
       cave_type *c_ptr;
   object_type *o_ptr;

   /* Check to see if an object is already there */
   if(cave[py][px].o_idx)
   {
       msg_print("You can't fetch when you're already standing on something.");
       return;
   }

   /* Use a target */
   if(dir==5 && target_okay())
   {
       tx = target_col;
       ty = target_row;
       if(distance(py, px, ty, tx)>MAX_RANGE)
       {
           msg_print("You can't fetch something that far away!");
           return;
       }
       c_ptr = &cave[ty][tx];

       if (require_los && (!player_has_los_bold(ty,tx)))
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
       } while(!c_ptr->o_idx);
   }
   o_ptr = &o_list[c_ptr->o_idx];
   if (o_ptr->weight > wgt)
   {   /* Too heavy to 'fetch' */
       msg_print("The object is too heavy.");
       return;
   }
   i = c_ptr->o_idx;
   c_ptr->o_idx = 0;
   cave[py][px].o_idx = i; /* 'move' it */
   o_ptr->iy = (byte)py;
   o_ptr->ix = (byte)px;


   note_spot(py,px);
   p_ptr->redraw |= PR_MAP;

 }




void wild_magic(int spell)
{
    int counter = 0;
    int type = SUMMON_BIZARRE1 - 1 + (randint(6));
    if (type < SUMMON_BIZARRE1) type = SUMMON_BIZARRE1;
    else if (type > SUMMON_BIZARRE6) type = SUMMON_BIZARRE6;

    switch(randint(spell) + randint(8) + 1)

    {
        case 1: case 2: case 3:
            teleport_player(10);
            break;
        case 4: case 5: case 6:
            teleport_player(100);
            break;
        case 7: case 8:
            teleport_player(200);
            break;
        case 9: case 10: case 11:
            unlite_area(10,3);
            break;
        case 12: case 13: case 14:
            lite_area(damroll(2,3),2);
            break;
        case 15:
            destroy_doors_touch();
            break;
        case 16: case 17:
            wall_breaker(50);
        case 18:
            sleep_monsters_touch(50);
            break;
        case 19: case 20:
            trap_creation();
            break;
        case 21: case 22:
            door_creation();
            break;
        case 23: case 24: case 25:
            aggravate_monsters(1);
            break;
        case 26:
            earthquake(py, px, 5);
            break;
        case 27: case 28:
            (void) gain_chaos_feature(0);
            break;
        case 29: case 30:
            apply_disenchant(0);
            break;
        case 31:
            lose_all_info();
            break;
        case 32:
            fire_ball(GF_CHAOS, 0, spell + 5, 1 + (spell/10));
            break;
        case 33:
            wall_stone();
            break;
        case 34: case 35:
            while (counter++ < 8)
            {
            (void) summon_specific(py, px, ((dun_level+dun_offset) * 3) / 2, type);
                    }
            break;
        case 36: case 37:
            activate_hi_summon();
            break;
        case 38:
            summon_reaver();
        default:
            activate_ty_curse();
    }
    return;
}


/*
 * Cast a spell
 */
void do_cmd_cast(void)
{
	int	item, sval, spell, dir;
	int	chance, beam;
	int	plev = 0;
	int	spell_school = 0, dummy = 0;
	int	i;
	int	ii = 0, ij = 0;

	bool	none_came = FALSE;
	const cptr prayer = "spell";

	object_type	*o_ptr;

	magic_type	*s_ptr;

        char	ppp[80];

        char	tmp_val[160];

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
	item_tester_tval = TV_SORCERY_BOOK;

	/* Get an item (from inven or floor) */
	if (!get_item(&item, "Use which book? ", FALSE, TRUE, TRUE))
	{
        if (item == -2) msg_format("You have no %s books!", prayer);
		return;
	}

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

	spell_school = o_ptr->tval - 90;


	/* Track the object kind */
	object_kind_track(o_ptr->k_idx);

	/* Hack -- Handle stuff */
	handle_stuff();

	/* Ask for a spell */
	if (!get_spell(&spell,"cast",
		sval, (bool)TRUE, spell_school))
	{
		if (spell == -2)
		msg_format("You don't know any %ss in that book.", prayer);
		return;
	}


	/* Access the spell */
	s_ptr = &mp_ptr->info[spell_school][spell];

	plev = spell_skill(s_ptr);

	/* Verify "dangerous" spells */
	if (s_ptr->smana > p_ptr->csp)
	{
		/* Warning */
		msg_format("You do not have enough mana to %s this %s.",
			"cast",
			prayer);

		/* Verify */
		if (!get_check("Attempt it anyway? ")) return;
	}


	/* Spell failure chance */
	chance = spell_chance(spell,spell_school);

	/* Failed spell */
	if (rand_int(100) < chance)
	{
		if (flush_failure) flush();

		msg_format("You failed to get the %s off!", prayer);

		if (o_ptr->tval == TV_THAUMATURGY_BOOK && (randint(100)<spell))
		{
			msg_print("You produce a chaotic effect!");
			wild_magic(spell);
		}
	}

	/* Process spell */
	else
	{

    beam = plev;


	/* Spells.  */
	switch (spell_school)
	{
	case 0: /* * Sorcery * */
	  switch (spell)
	  {
	   case 0: /* Detect Monsters */
			(void)detect_monsters_normal();
		       break;
	   case 1: /* Phase Door */
			teleport_player(10);
		       break;
	   case 2: /* Detect Doors and Traps */
			(void)detect_traps();
			(void)detect_doors();
			(void)detect_stairs();
		       break; 
       case 3: /* Light Area */
			(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
            break;
	   case 4: /* Confuse Monster */
			if (!get_aim_dir(&dir)) return;
            (void)confuse_monster(dir, ( plev * 3) / 2 );
			break;
	   case 5: /* Teleport Self */
            teleport_player(plev * 5);
		       break;
	   case 6: /* Sleep Monster */
			if (!get_aim_dir(&dir)) return;
			(void)sleep_monster(dir,plev);
		       break;
	   case 7: /* Recharging */
               (void)recharge(plev * 2);
		       break;
	   case 8: /* Magic Mapping */
			map_area();
		       break;
	   case 9: /* Identify */
			(void)ident_spell();
		       break;
	   case 10: /* Slow Monster */
			if (!get_aim_dir(&dir)) return;
			(void)slow_monster(dir,plev);
		       break;
	   case 11: /* Mass Sleep */
			(void)sleep_monsters(plev);
		       break;
	   case 12: /* Teleport Away */
			if (!get_aim_dir(&dir)) return;
               (void)fire_beam(GF_AWAY_ALL, dir, plev);
		       break;
	   case 13: /* Haste Self */
			if (!p_ptr->fast)
			{
				(void)set_fast(randint(20 + (plev) ) + plev);
			}
			else
			{
				(void)set_fast(p_ptr->fast + randint(5));
			}
		       break;
	   case 14: /* Detection True */
			(void)detect_all();
		       break;
	   case 15: /* Identify True */
			identify_fully();
		       break;
       case 16: /* Detect Objects and Treasure*/
			(void)detect_objects_normal();
			(void)detect_treasure();
			(void)detect_objects_gold();
		       break;
       case 17: /* Detect Enchantment */
			(void)detect_objects_magic();
		       break;
       case 18: /* Charm Monster */
                 if (!get_aim_dir(&dir)) return;
                 (void) charm_monster(dir, plev);
               break;
       case 19: /* Dimension Door */
       {
             msg_print("You open a dimensional gate. Choose a destination.");
             if (!tgt_pt(&ii,&ij)) return;
             p_ptr->energy -= 60 - plev;
             if (!cave_empty_bold(ij,ii) || (cave[ij][ii].info & CAVE_ICKY) || (cave[ij][ii].feat == FEAT_WATER) ||
             (distance(ij,ii,py,px) > plev + 2) ||
             (!rand_int(plev * plev / 2)))
             {
                 msg_print("You fail to exit the astral plane correctly!");
                 p_ptr->energy -= 100;
                 teleport_player(10);
             }
             else teleport_player_to(ij,ii);
             break;
            }

       case 20: /* Sense Minds */
            (void)set_tim_esp(p_ptr->tim_esp + randint(30) + 25);
		       break;
       case 21: /* Self knowledge */
           (void)self_knowledge();
               break;
	   case 22: /* Teleport Level */
			(void)teleport_player_level();
		       break;
	   case 23: /* Word of Recall */
			{
                if (dun_level && (p_ptr->max_dlv[cur_dungeon] > dun_level) && (cur_dungeon == recall_dungeon))
                {
                    if (get_check("Reset recall depth? "))
                    p_ptr->max_dlv[cur_dungeon] = dun_level;

                }
				if (!p_ptr->word_recall)
				{
					p_ptr->word_recall = rand_int(21) + 15;
					if (dun_level > 0)
					{
						recall_dungeon = cur_dungeon;
					}
					else
					{
						cur_dungeon = recall_dungeon;
					}
					msg_print("The air about you becomes charged...");
				}
				else
				{
					p_ptr->word_recall = 0;
					msg_print("A tension leaves the air around you...");
				}
				break;
			}
       case 24: /* Stasis */
			if (!get_aim_dir(&dir)) return;
			(void)stasis_monster(dir,plev);
		       break;
       case 25: /* Telekinesis */
         if (!get_aim_dir(&dir)) return;
         fetch(dir, plev*15, FALSE);
         break;
       case 26: /* Recharging True -- replaced by Explosive Rune */
               explosive_rune();
		       break;
	   case 27: /* Clairvoyance */
			wiz_lite();
            if (!(p_ptr->telepathy))
            {
                (void)set_tim_esp(p_ptr->tim_esp + randint(30) + 25);
            }
		       break;
	   case 28: /* Enchant Weapon */
			(void)enchant_spell(rand_int(4) + 1, rand_int(4) + 1, 0);
		       break;
	   case 29: /* Enchant Armour */
			(void)enchant_spell(0, 0, rand_int(3) + 2);
		       break;
	   case 30: /* Alchemy */
		       (void) alchemy();
		       break;
	   case 31: /* Globe of Invulnerability */
			(void)set_invuln(p_ptr->invuln + randint(8) + 8);
		       break;
	       default:
		 msg_format("You cast an unknown Sorcery spell: %d.", spell);
		 msg_print(NULL);
	   }
      break;
	case 1: /* * Thaumaturgy * */
	   switch (spell)
	   {
		case 0: /* Magic Missile */
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_MISSILE, dir,
						  damroll(3 + ((plev - 1) / 5), 4));
                break;
        case 1: /* Trap / Door destruction, was: Blink */
			(void)destroy_doors_touch();
			break;
        case 2: /* Flash of Light == Light Area */
			(void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
			break; 
        case 3: /* Touch of Confusion */
            if (!(p_ptr->confusing))
            {
                msg_print("Your hands start glowing.");
                p_ptr->confusing = TRUE;
            }
			break;
       case 4: /* Manaburst */
             if (!get_aim_dir(&dir)) return;
             fire_ball(GF_MISSILE, dir,
            (damroll(3, 5) + plev +
             (plev / 4)),
            ((plev < 30) ? 2 : 3));
          /* Shouldn't actually use GF_MANA, as it will destroy all
       * items on the floor */
             break;
        case 5: /* Fire Bolt */
			if (!get_aim_dir(&dir)) return;
			fire_bolt_or_beam(beam, GF_FIRE, dir,
				damroll(8+((plev-5)/4), 8));
			break;
        case 6: /* Fist of Force ("Fist of Fun") */
			if (!get_aim_dir(&dir)) return;
           fire_ball(GF_DISINTEGRATE, dir,
               damroll(8+((plev-5)/4), 8), 0);
            break;
		case 7: /* Teleport Self */
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
                   msg_print ("You feel a surge of power!");
               if (die < 8) clone_monster (dir);
               else if (die < 14) speed_monster (dir,plev);
               else if (die < 26) heal_monster (dir);
               else if (die < 31) poly_monster (dir,plev);
               else if (die < 36)
                   fire_bolt_or_beam (beam - 10,
                   GF_MISSILE, dir,
                   damroll(3 + ((plev - 1) / 5), 4));
               else if (die < 41) confuse_monster (dir, plev);
               else if (die < 46) fire_ball (GF_POIS, dir, 20 + (plev / 2), 3);
               else if (die < 51) lite_line (dir);
               else if (die < 56)
                   fire_bolt_or_beam (beam - 10, GF_ELEC, dir,
                   damroll(3+((plev-5)/4),8));
               else if (die < 61)
                   fire_bolt_or_beam (beam - 10, GF_COLD, dir,
                   damroll(5+((plev-5)/4),8));
               else if (die < 66)
                   fire_bolt_or_beam (beam, GF_ACID, dir,
                   damroll(6+((plev-5)/4),8));
               else if (die < 71)
                   fire_bolt_or_beam (beam, GF_FIRE, dir,
                   damroll(8+((plev-5)/4),8));
               else if (die < 76) drain_life (dir, 75);
               else if (die < 81) fire_ball (GF_ELEC, dir, 30 + plev / 2, 2);
               else if (die < 86) fire_ball (GF_ACID, dir, 40 + plev, 2);
               else if (die < 91) fire_ball (GF_ICE, dir, 70 + plev, 3);
               else if (die < 96) fire_ball (GF_FIRE, dir, 80 + plev, 3);
               else if (die < 101) drain_life (dir, 100 + plev);
               else if (die < 104) earthquake (py, px, 12);
               else if (die < 106) destroy_area (py, px, 15, TRUE);
               else if (die < 108) genocide(TRUE);
               else if (die < 110) dispel_monsters (120);
               else /* RARE */
               {
                   dispel_monsters (150);
                   slow_monsters(plev);
                   sleep_monsters(plev);
                   hp_player (300);
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
                   project(0, 2+plev/10, py, px,
               45+plev, GF_SOUND, PROJECT_KILL|PROJECT_ITEM);
                   break;
        case 11: /* Doom Bolt -- always beam in 2.0.7 or later */
				if (!get_aim_dir(&dir)) return;
                fire_beam(GF_MANA, dir, damroll(11+((plev-5)/4), 8));
			break;
		case 12: /* Fire Ball */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_FIRE, dir,
					55 + (plev), 2);
			break;
		case 13: /* Teleport Other */
           if (!get_aim_dir(&dir)) return;
               (void)fire_beam(GF_AWAY_ALL, dir, plev);
			break;
		case 14: /* Word of Destruction */
			destroy_area(py, px, 15, TRUE);
			break;
		case 15: /* Invoke chaos */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_CHAOS, dir,
					66 + (plev), (plev / 5));
			break;
        case 16: /* Polymorph Other */
			if (!get_aim_dir(&dir)) return;
			(void)poly_monster(dir,plev);
			break;
        case 17: /* Chain Lightning */
          for (dir = 0; dir <= 9; dir++)
            fire_beam(GF_ELEC, dir, damroll(5+(plev/10), 8));
           break;
        case 18: /* Arcane Binding == Charging */
			(void)recharge(40);
			break;
        case 19: /* Disintegration */
			if (!get_aim_dir(&dir)) return;
           fire_ball(GF_DISINTEGRATE, dir,
               80 + (plev), 3 + (plev/40));
               break;
            break;
        case 20: /* Alter Reality */
			msg_print("The world changes!");
                if (autosave_l)
                {
                    is_autosave = TRUE;
                    msg_print("Autosaving the game...");
                    do_cmd_save_game();
                    is_autosave = FALSE;
                }
			new_level_flag = TRUE;
			came_from=START_RANDOM;
			break;
        case 21: /* Polymorph Self */
            do_poly_self();
	    break;
        case 22: /* Chaos Branding */
		brand_weapon(1);
		break;
        case 23: /* Summon monster, demon */
		if (randint(3) == 1)
		{
			if (summon_specific(py, px, (plev*3)/2, SUMMON_DEMON))
			{
				msg_print("The area fills with a stench of sulphur and brimstone.");
				msg_print("'NON SERVIAM! Wretch! I shall feast on thy mortal soul!'");
			}
        	}
		else
		{
			if (summon_specific_friendly((int)py,(int) px, (plev*3)/2,
				SUMMON_DEMON, (bool)(plev == 50 ? TRUE : FALSE)))
			{
				msg_print("The area fills with a stench of sulphur and brimstone.");
				msg_print("'What is thy bidding... Master?'");
			}
		}
		break;
        case 24: /* Beam of Gravity */
			if (!get_aim_dir(&dir)) return;
                fire_beam(GF_GRAVITY, dir, damroll(9+((plev-5)/4), 8));
            break;
        case 25: /* Meteor Swarm  */
           {
		       int x, y, dx, dy, d, count = 0;
		       int b = 10 + randint(10); 
		       for (i = 0; i < b; i++) {
			   do {
			       count++;
			       if (count > 1000)  break;
			       x = px - 5 + randint(10);
			       y = py - 5 + randint(10);
			       dx = (px > x) ? (px - x) : (x - px);
			       dy = (py > y) ? (py - y) : (y - py);
			       /* Approximate distance */
                   d = (dy > dx) ? (dy + (dx>>1)) : (dx + (dy>>1));
               } while ((d > 5) || (!(player_has_los_bold(y, x))));
			   
			   if (count > 1000)   break;
			   count = 0;
               project(0, 2, y, x, (plev*3)/2, GF_METEOR, PROJECT_KILL|PROJECT_JUMP|PROJECT_ITEM);
		       }
		   }
	           break;
		case 26: /* Flame Strike */
			fire_ball(GF_FIRE, 0,
                150 + (2*plev), 8);
			break;
        case 27: /* Call Chaos */
            call_chaos(plev);
			break;
        case 28: /* Shard Ball */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_SHARD, dir,
					120 + (plev), 2);
			break;
        case 29: /* Mana Storm */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_MANA, dir,
				300 + (plev * 2), 4);
            break;
        case 30: /* Breathe chaos */
               if (!get_aim_dir(&dir)) return;
               fire_ball(GF_CHAOS,dir,p_ptr->chp,
                     -2);
               break;
		case 31: /* Call the Void */
			call_the_();
			break;
		default:
		  msg_format("You cast an unknown Thaumaturgy spell: %d.", spell);
		  msg_print(NULL);
	    }
	   break;
    case 2: /* Conjuration */
    switch (spell)
    {
        case 0: /* Phase Door */
			teleport_player(10);
        break;
        case 1: /* Mind Blast */
               if (!get_aim_dir(&dir)) return;
                 fire_bolt_or_beam(beam-10, GF_PSI, dir,
                              damroll(3 + ((plev - 1) / 5), 3));
        break;
        case 2: /* Tarot Draw */

           {
                /* A limited power 'wonder' spell */

               int die = die = (randint(110)) + plev / 5;
               /* get a level bonus */

            msg_print("You shuffle your Tarot deck and draw a card...");

            if (die < 7 )
            {
                msg_print("Oh no! It's the Blasted Tower!");
                for (dummy = 0; dummy < randint(3); dummy++)
                    (void)activate_hi_summon();
            }
            else if (die < 14)
            {
                msg_print("Oh no! It's the Devil!");
                (void) summon_specific(py, px, (dun_level+dun_offset), SUMMON_DEMON);
            }
            else if (die < 18 )
            {
                msg_print("Oh no! It's the Hanged Man.");
                activate_ty_curse();
            }
            else if (die < 22 )
            {
                msg_print("It's the swords of discord.");
                aggravate_monsters(1);
            }
            else if (die < 26)
            {
                msg_print("It's the Fool.");
                (void) do_dec_stat(A_INT);
                (void) do_dec_stat(A_WIS);
            }
            else if (die < 30)
            {
                msg_print("It's a picture of a strange monster.");
                if (!(summon_specific(py, px, ((dun_level+dun_offset) * 3) / 2, 32 + randint(6))))
                    none_came = TRUE;
            }
            else if (die < 33)
            {
                msg_print("It's the Moon.");
                unlite_area(10,3);
            }
            else if (die < 38)
            {
                msg_print("It's the Wheel of Fortune.");
                wild_magic((randint(32))-1);
            }
            else if (die < 40)
            {
                msg_print("It's a teleport card.");
                teleport_player(10);
            }
            else if (die <42)
            {
                msg_print("It's the Star.");
                set_blessed(p_ptr->blessed + plev);
            }
            else if (die <47)
            {
                msg_print("It's a teleport card.");
                teleport_player(100);
            }
            else if (die <52)
            {
                msg_print("It's a teleport card.");
                teleport_player(200);
            }
            else if (die <60)
            {
                msg_print("It's the Tower.");
                wall_breaker(plev);
            }
            else if (die <72)
            {
                msg_print("It's Temperance.");
                sleep_monsters_touch(plev);
            }
            else if (die <80)
            {
                msg_print("It's the Tower.");
                earthquake(py, px, 5);
            }
            else if (die<82)
            {
                msg_print("It's a picture of a friendly monster.");
                if (!(summon_specific_friendly(py, px, ((dun_level+dun_offset) * 3) / 2, SUMMON_BIZARRE1, FALSE)))
                    none_came = TRUE;
            }
            else if (die<84)
            {
                msg_print("It's a picture of a friendly monster.");
                if (!(summon_specific_friendly(py, px, ((dun_level+dun_offset) * 3) / 2, SUMMON_BIZARRE2, FALSE)))
                    none_came = TRUE;
            }
            else if (die<86)
            {
                msg_print("It's a picture of a friendly monster.");
                if (!(summon_specific_friendly(py, px, ((dun_level+dun_offset) * 3) / 2, SUMMON_BIZARRE4, FALSE)))
                    none_came = TRUE;
            }
            else if (die<88)
            {
                msg_print("It's a picture of a friendly monster.");
                if (!(summon_specific_friendly(py, px, ((dun_level+dun_offset) * 3) / 2, SUMMON_BIZARRE5, FALSE)))
                    none_came = TRUE;
            }
            else if (die<96)
            {
                msg_print("It's the Lovers.");
                if (!get_aim_dir(&dir)) return;
                (void) charm_monster(dir, MIN(plev, 20));
            }
            else if (die<101)
            {
                msg_print("It's the Hermit.");
                wall_stone();
            }
            else if (die< 111)
            {
                msg_print("It's the Judgement.");
                do_cmd_rerate();
                if (p_ptr->muta1 || p_ptr->muta2 || p_ptr->muta3)
                {
                    msg_print("You are cured of all chaos features.");
                    p_ptr->muta1 = p_ptr->muta2 = p_ptr->muta3 = 0;
                    p_ptr->update |= PU_BONUS;
                    handle_stuff();
                }
                
            }
            else if (die < 120)
            {
                msg_print("It's the Sun.");
                wiz_lite();
            }
            else
            {
                msg_print("It's the World.");
                if (p_ptr->exp < PY_MAX_EXP)
                {
                    msg_print("You feel more experienced.");
					gain_skills(100);
                }
            }

           }
        break;
        case 3: /* Reset Recall */
            {
                /* Prompt */
                sprintf(ppp, "Reset to which level (1-%d): ", p_ptr->max_dlv[cur_dungeon]);

                /* Default */
                sprintf(tmp_val, "%d", MAX(dun_level,1));

                /* Ask for a level */
                if (!get_string(ppp, tmp_val, 10)) return;

                /* Extract request */
                dummy = atoi(tmp_val);

                /* Paranoia */
                if (dummy < 1) dummy = 1;

                /* Paranoia */
                if (dummy > p_ptr->max_dlv[cur_dungeon]) dummy = p_ptr->max_dlv[cur_dungeon];

                /* Accept request */
                msg_format("Recall depth set to level %d (%d').", dummy, dummy * 50 );
            }
        break;
        case 4: /* Teleport Self */
            teleport_player(plev * 4);
        break;
        case 5: /* Dimension Door */
       {
             msg_print("You open a dimensional gate. Choose a destination.");
             if (!tgt_pt(&ii,&ij)) return;
             p_ptr->energy -= 60 - plev;
             if (!cave_empty_bold(ij,ii) || (cave[ij][ii].info & CAVE_ICKY) || (cave[ij][ii].feat == FEAT_WATER) ||
             (distance(ij,ii,py,px) > plev + 2) ||
             (!rand_int(plev * plev / 2)))
             {
                 msg_print("You fail to exit the astral plane correctly!");
                 p_ptr->energy -= 100;
                 teleport_player(10);
             }
             else teleport_player_to(ij,ii);
             break;
            }
        case 6: /* Planar Spying */
            (void)set_tim_esp(p_ptr->tim_esp + randint(30) + 25);
        break;
        case 7: /* Teleport Away */
			if (!get_aim_dir(&dir)) return;
               (void)fire_beam(GF_AWAY_ALL, dir, plev);
        break;
        case 8: /* Summon Object */
             if (!get_aim_dir(&dir)) return;
                 fetch(dir, plev*15, TRUE);
        break;
        case 9: /* Summon Animal */
        {
            msg_print ("You reach out your mind to the wilderness...");
            if (randint(5)>2)
            {
              if (!(summon_specific_friendly(py, px, plev, SUMMON_ANIMAL_RANGER, FALSE)))
                none_came = TRUE;
            }
            else
            {
                if (summon_specific(py, px, plev, SUMMON_ANIMAL))
                {
                    msg_print("The summoned animal gets angry!");
                }
                else
                {
                    none_came = TRUE;
                }
            }
        }
        break;
        case 10: /* Phantasmal Servant */
               if (summon_specific_friendly(py, px, (plev*3)/2, SUMMON_PHANTOM, FALSE))
               {
                    msg_print ("'Your wish, master?'");
                }
                else
                {
                    none_came = TRUE;
                }
        break;
        case 11: /* Summon Monster */
        {
            msg_print ("You reach out your mind...");
            if (randint(5)>2)
            {
             if (!(summon_specific_friendly(py, px, plev, SUMMON_NO_UNIQUES, FALSE)))
                none_came = TRUE;
            }
            else
            {
                if (summon_specific(py, px, plev, 0))
                {
                    msg_print("The summoned creature gets angry!");
                }
                else
                {
                    none_came = TRUE;
                }
            }
        }
        break;
        case 12: /* Conjure Elemental */
        {
            if (randint(6)>3)
            {
             if (!(summon_specific_friendly(py, px, plev, SUMMON_ELEMENTAL, FALSE)))
                none_came = TRUE;
            }
            else
            {
                if (summon_specific(py, px, plev, SUMMON_ELEMENTAL))
                {
                      msg_print("You fail to control the elemental creature!");
                }
                else
                {
                    none_came = TRUE;
                }
            }
        }

        break;
        case 13: /* Teleport Level */
			(void)teleport_player_level();
        break;
        case 14: /* Word of Recall */
			{
                if (dun_level && (p_ptr->max_dlv[cur_dungeon] > dun_level) && (cur_dungeon == recall_dungeon))
                {
                    if (get_check("Reset recall depth? "))
                    p_ptr->max_dlv[cur_dungeon] = dun_level;

                }
				if (!p_ptr->word_recall)
				{
					p_ptr->word_recall = rand_int(21) + 15;
					if (dun_level > 0)
					{
						recall_dungeon = cur_dungeon;
					}
					else
					{
						cur_dungeon = recall_dungeon;
					}
					msg_print("The air about you becomes charged...");
				}
				else
				{
					p_ptr->word_recall = 0;
					msg_print("A tension leaves the air around you...");
				}
				break;
			}
        case 15: /* Banish */
             banish_monsters(plev*4);
        break;
        case 16: /* Joker Card */
            msg_print("You concentrate on a joker card...");
            switch(randint(4))
            {
                case 1: dummy = SUMMON_BIZARRE1; break;
                case 2: dummy = SUMMON_BIZARRE2; break;
                case 3: dummy = SUMMON_BIZARRE4; break;
                case 4: dummy = SUMMON_BIZARRE5; break;

            }
            if (randint(2)==1)
            {
                if (summon_specific(py, px, plev, dummy))
                    msg_print("The summoned creature gets angry!");
                 else
                    none_came = TRUE;
                }
            else
            {
                if (!(summon_specific_friendly(py, px, plev, dummy, FALSE)))
                    none_came = TRUE;
            }
        break;
        case 17: /* Summon Spiders */
        {
            msg_print ("You reach out your mind along the planar webs...");
            if (randint(5)>2)
            {
                if (!(summon_specific_friendly(py, px, plev, SUMMON_SPIDER, TRUE)))
                    none_came = TRUE;
            }
            else
            {
                if (summon_specific(py, px, plev, SUMMON_SPIDER))
                {
                    msg_print("The summoned spiders get angry!");
                }
                else
                {
                    none_came = TRUE;
                }
            }
        }
        break;
        case 18: /* Summon Reptiles */
        {
            msg_print ("You reach out your mind to the cold, damp places...");
            if (randint(5)>2)
            {
             if (!(summon_specific_friendly(py, px, plev, SUMMON_HYDRA, TRUE)))
                none_came = TRUE;
            }
            else
            {
                if (summon_specific(py, px, plev, SUMMON_HYDRA))
                {
                    msg_print("The summoned reptile gets angry!");
                }
                else
                {
                    none_came = TRUE;
                }
            }
        }
        break;
        case 19: /* Summon Hounds */
        {
            msg_print ("You reach out your mind to the elemental planes...");
            if (randint(5)>2)
            {
             if (!(summon_specific_friendly(py, px, plev, SUMMON_HOUND, TRUE)))
                none_came = TRUE;
            }
            else
            {
                if (summon_specific(py, px, plev, SUMMON_HOUND))
                {
                    msg_print("The summoned hounds get angry!");
                }
                else
                {
                    none_came = TRUE;
                }
            }
        }

        break;
        case 20: /* Planar Branding */
            brand_weapon(4);
        break;
        case 21: /* Planar Being */
        if (randint(8)==1) dummy = 103;
        else dummy = 30;
        if (gain_chaos_feature(dummy))
            msg_print("You have turned into a Planar Being.");
        break;
        case 22: /* Death Dealing */
            (void)dispel_living(plev * 3);
        break;
        case 23: /* Summon Reaver */
        {
            msg_print ("You reach out your mind to the planes of order...");
            if (randint(10)>3)
            {
              if (!(summon_specific_friendly(py, px, plev, SUMMON_REAVER, FALSE)))
                none_came = TRUE;
            }
            else
            {
                if (summon_specific(py, px, plev, SUMMON_REAVER))
                {
                    msg_print("The summoned Black Reaver gets angry!");
                }
                else
                {
                    none_came = TRUE;
                }
            }
        }
        break;
        case 24: /* Planar Divination */
			(void)detect_all();
        break;
        case 25: /* Planar Lore */
            identify_fully();
        break;
        case 26: /* Summon Undead */
        {
            msg_print ("You reach out your mind to beyond the grave...");
            if (randint(10)>3)
            {
             if (!(summon_specific_friendly(py, px, plev, SUMMON_UNDEAD, FALSE)))
                none_came = TRUE;
            }
            else
            {
                if (summon_specific(py, px, plev, SUMMON_UNDEAD))
                {
                    msg_print("The summoned undead creature gets angry!");
                }
                else
                {
                    none_came = TRUE;
                }
            }
        }
        break;
        case 27: /* Summon Dragon */
        {
            msg_print ("You reach out your mind to the treasure troves...");
            if (randint(10)>3)
            {
             if (!(summon_specific_friendly(py, px, plev, SUMMON_DRAGON, FALSE)))
                none_came = TRUE;
            }
            else
            {
                if (summon_specific(py, px, plev, SUMMON_DRAGON))
                {
                    msg_print("The summoned dragon gets angry!");
                }
                else
                {
                    none_came = TRUE;
                }
            }
        }

        break;
        case 28: /* Mass Summon */
        {
            none_came = TRUE;
            msg_print ("You concentrate on several images at once...");
            for (dummy = 0; dummy < 3 + (plev / 10); dummy++)
            {
                if (randint(10)>3)
                {
                 if (summon_specific_friendly(py, px, plev, SUMMON_NO_UNIQUES, FALSE))
                    none_came = FALSE;
                }
                else
                {
                    if (summon_specific(py, px, plev, 0))
                    {
                        msg_print("A summoned creature gets angry!");
                        none_came = FALSE;
                    }
                }
            }
        }
        break;
        case 29: /* Summon Demon */
        {
            msg_print ("You reach out your mind to the pits of hell...");
            if (randint(10)>3)
            {
             if (!(summon_specific_friendly(py, px, plev, SUMMON_DEMON, FALSE)))
                none_came = TRUE;
            }
            else
            {
                if (summon_specific(py, px, plev, SUMMON_DEMON))
                {
                    msg_print("The summoned demon gets angry!");
                }
                else
                {
                    none_came = TRUE;
                }
            }
        }
        break;
        case 30: /* Summon Ancient Dragon */
        {
            msg_print ("You reach out your mind to the ancient caves...");
            if (randint(10)>3)
            {
             if (!(summon_specific_friendly(py, px, plev, SUMMON_HI_DRAGON_NO_UNIQUES, FALSE)))
                none_came = TRUE;
            }
            else
            {
                if (summon_specific(py, px, plev, SUMMON_HI_DRAGON))
                {
                    msg_print("The summoned ancient dragon gets angry!");
                }
                else
                {
                    none_came = TRUE;
                }
            }
        }

        break;
        case 31: /* Summon Greater Undead */
        {
            msg_print ("You reach out your mind to the darkest tombs...");
            if (randint(10)>3)
            {
             if (!(summon_specific_friendly(py, px, plev, SUMMON_HI_UNDEAD_NO_UNIQUES, FALSE)))
                none_came = TRUE;
            }
            else
            {
                if (summon_specific(py, px, plev, SUMMON_HI_UNDEAD))
                {
                    msg_print("The summoned greater undead creature gets angry!");
                }
                else
                {
                    none_came = TRUE;
                }
            }
        }
        break;
        default:
        msg_format("You cast an unknown Conjuration spell: %d.", spell);
        msg_print(NULL);
    }
    if (none_came)
    {
        msg_print("Nobody answers to your call.");
    }
    break;
	case 3: /* Necromancy */
	  switch (spell)
	  {
       case 0: /* Detect Undead & Demons -> Unlife*/
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
         if (randint(5)==1) {   /* Special effect first */
         dummy = randint(1000);
         if (dummy == 666)
           fire_bolt(GF_DEATH_RAY, dir, plev);
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
			fire_ball(GF_POIS, dir,
				10 + (plev / 2), 2);
		       break;
	   case 4: /* Black Sleep */
			if (!get_aim_dir(&dir)) return;
			(void)sleep_monster(dir,plev);
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
           (damroll(3, 6) + plev + (plev / 4)),
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
                 if (drain_life(dir, dummy)) {
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
            brand_weapon(2);
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
               int die = randint(100) + plev / 5;
               if (!get_aim_dir(&dir)) return;

              msg_print("You call on the power of the dead...");
               if (die > 100)
                 msg_print ("You feel a surge of eldritch force!");

               if (die < 8) {
               msg_print("Oh no! Mouldering forms rise from the earth around you!");
               (void) summon_specific(py, px, (dun_level+dun_offset), SUMMON_UNDEAD);
               } else if (die < 14) {
               msg_print("An unnamable evil brushes against your mind...");
               set_afraid(p_ptr->afraid + randint(4) + 4);
               } else if (die < 26) {
               msg_print("Your head is invaded by a horde of gibbering spectral voices...");
               set_confused(p_ptr->confused + randint(4) + 4);
               } else if (die < 31) {
               poly_monster (dir,plev);
               } else if (die < 36) {
               fire_bolt_or_beam (beam - 10,
                          GF_MISSILE, dir,
                          damroll(3 + ((plev - 1) / 5), 4));
               } else if (die < 41) {
               confuse_monster (dir, plev);
               } else if (die < 46) {
               fire_ball (GF_POIS, dir, 20 + (plev / 2), 3);
               } else if (die < 51) {
               lite_line (dir);
               } else if (die < 56) {
               fire_bolt_or_beam (beam - 10, GF_ELEC, dir,
                          damroll(3+((plev-5)/4),8));
               } else if (die < 61) {
               fire_bolt_or_beam (beam - 10, GF_COLD, dir,
                          damroll(5+((plev-5)/4),8));
               } else if (die < 66) {
               fire_bolt_or_beam (beam, GF_ACID, dir,
                          damroll(6+((plev-5)/4),8));
               } else if (die < 71) {
               fire_bolt_or_beam (beam, GF_FIRE, dir,
                          damroll(8+((plev-5)/4),8));
               } else if (die < 76) {
               drain_life (dir, 75);
               } else if (die < 81) {
               fire_ball (GF_ELEC, dir, 30 + plev / 2, 2);
               } else if (die < 86) {
               fire_ball (GF_ACID, dir, 40 + plev, 2);
               } else if (die < 91) {
               fire_ball (GF_ICE, dir, 70 + plev, 3);
               } else if (die < 96) {
               fire_ball (GF_FIRE, dir, 80 + plev, 3);
               } else if (die < 101) {
               drain_life (dir, 100 + plev);
               } else if (die < 104) {
               earthquake (py, px, 12);
               } else if (die < 106) {
               destroy_area (py, px, 15, TRUE);
               } else if (die < 108) {
               genocide(TRUE);
               } else if (die < 110) {
               dispel_monsters (120);
               } else { /* RARE */
               dispel_monsters (150);
               slow_monsters(plev);
               sleep_monsters(plev);
               hp_player (300);
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
            brand_weapon(3);
		       break;
       case 22: /* Darkness Storm */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_DARK, dir,
					120, 4);
		       break;
        case 23: /* Mass Genocide */
			(void)mass_genocide(TRUE);
		       break;
       case 24: /* Death Ray */
			if (!get_aim_dir(&dir)) return;
			(void)death_ray(dir, plev);
		       break;
       case 25: /* Raise the Dead */
                   if (randint(3) == 1) {
               if (summon_specific(py, px, (plev*3)/2,
                       (plev > 47 ? SUMMON_HI_UNDEAD : SUMMON_UNDEAD))) {
               msg_print("Cold winds begin to blow around you, carrying with them the stench of decay...");
               msg_print("'The dead arise... to punish you for disturbing them!'");
               }
           } else {
               if (summon_specific_friendly((int)py,(int)px, (plev*3)/2,
                       (plev > 47 ? SUMMON_HI_UNDEAD_NO_UNIQUES : SUMMON_UNDEAD),
                       (bool)(((plev > 24) && (randint(3) == 1)) ? TRUE : FALSE))) {
               msg_print("Cold winds begin to blow around you, carrying with them the stench of decay...");
               msg_print("Ancient, long-dead forms arise from the ground to serve you!");
               }
           }
           break;
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
            fire_ball(GF_HELL_FIRE, dir,
                    666, 3);
            take_hit(50+randint(50), "the strain of casting Hellfire");
            break;
        case 30: /* Omnicide */
         p_ptr->csp -= 100;  /* Display doesn't show mana cost (100)
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

			 /* Skip Quest Monsters */
			 if ((r_ptr->flags1 & RF1_GUARDIAN) || (r_ptr->flags1 & RF1_ALWAYS_GUARD)) continue;

             /* Delete the monster */
             delete_monster_idx(i,TRUE);

             /* Take damage */
             take_hit(randint(4), "the strain of casting Omnicide");

             /* Absorb power of dead soul */
             p_ptr->csp++;

             /* Visual feedback */
             move_cursor_relative(py, px);

             /* Redraw */
             p_ptr->redraw |= (PR_HP | PR_MANA);

             /* Window stuff */
             p_ptr->window |= (PW_PLAYER);
			 p_ptr->window |=(PW_SPELL);

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
		 msg_format("You cast an unknown Necromancy spell: %d.", spell);
		 msg_print(NULL);
	   }
	  break;
	default:
		  msg_format("You cast a spell from an unknown school: school %d, spell %d.", spell_school, spell);
		  msg_print(NULL);
	    }
			/* A spell was cast */
		if (!(spell_worked[spell_school] & (1L << (spell))))
		{
			int e = s_ptr->sexp;

			/* The spell worked */
	   		spell_worked[spell_school] |= (1L << spell);
		}
		/* Gain experience with spell skills and mana */
		gain_spell_exp(s_ptr);
	}

	/* Take some time - a spell of your level takes 100, lower level spells take less */
	energy_use = spell_energy((u16b)plev,(u16b)(s_ptr->minskill));

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
	p_ptr->window |= (PW_PLAYER);
	p_ptr->window |=(PW_SPELL);
}

/*
 * Cast a cantrip
 */
void do_cmd_cantrip(void)
{
	int	item, sval, spell, dir;
	int	chance, beam;
	int	plev = 0;
	int	dummy = 0;
	int	ii = 0, ij = 0;

	bool	none_came = FALSE;
	const cptr prayer = "cantrip";
	bool from_pouch = FALSE;
	bool item_break = FALSE;

	object_type	*o_ptr;

	cantrip_type	*s_ptr;

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


	/* Restrict choices to charms */
	item_tester_tval = TV_CHARM;

	/* Get an item (from inven or floor) */
	if (!get_item(&item, "Use which charm? ", TRUE, TRUE, TRUE))
	{
        if (item == -2) msg_print("You have no charms!");
		return;
	}

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];

		/* Remember if we got this from a pouch */
		if ((item >= INVEN_POUCH_1) && (item <= INVEN_POUCH_6))
		{
			from_pouch = TRUE;
		}
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
	if (!get_cantrip(&spell,sval))
	{
		if (spell == -2)
		msg_format("You don't know any %ss for that charm.", prayer);
		return;
	}

	s_ptr = &(cantrip_info[spell]);

	plev = skill_set[SKILL_HEDGE].value/2;
	if (plev == 0) plev++;

	/* Spell failure chance */
	chance = cantrip_chance(spell);

	/* Failed spell */
	if (rand_int(100) < chance)
	{
		if (flush_failure) flush();

		msg_format("You failed to cast the %s!", prayer);
		/* Charm *always* breaks if the spell fails */
		item_break = TRUE;
	}

	/* Process spell */
	else
	{

    beam = plev;


	/* Spells.  */
	  switch (spell)
	  {
        case 0: /* Zap */
               if (!get_aim_dir(&dir)) return;
                 fire_bolt_or_beam(beam-10, GF_ELEC, dir,
                              damroll(3 + ((plev - 1) / 5), 3));
        break;
        case 1: /* Wizard Lock */
            if (!(get_aim_dir(&dir))) break;
            (void) wizard_lock(dir);
        break;
        case 2: /* Detect Invisibility */
            (void)detect_monsters_invis();
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
        case 9: /* Phlogiston */
            phlogiston();
        break;
        case 10: /* Detect Treasure */
			(void)detect_treasure();
			(void)detect_objects_gold();

        break;
        case 11: /* Detect Enchantment */
			(void)detect_objects_magic();
        break;
        case 12: /* Detect Object */
			(void)detect_objects_normal();
        break;
        case 13: /* Cure Poison */
			(void)set_poisoned(0);
        break;
        case 14: /* Resist Cold */
			(void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
        break;
        case 15: /* Resist Fire */
			(void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
        break;
        case 16: /* Resist Lightning */
			(void)set_oppose_elec(p_ptr->oppose_elec + randint(20) + 20);
        break;
        case 17: /* Resist Acid */
            (void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20);
        break;
        case 18: /* Cure Medium Wounds */
            (void)hp_player(damroll(4, 8));
            (void)set_cut((p_ptr->cut / 2) - 50);
        break;
        case 19: /* Teleport */
            teleport_player(plev * 5);
        break;
        case 20: /* Stone to Mud */
			if (!get_aim_dir(&dir)) return;
			(void)wall_to_mud(dir);
        break;
        case 21: /* Ray of Light */
			if (!get_aim_dir(&dir)) return;
            msg_print("A line of light appears.");
			lite_line(dir);
        break;
        case 22: /* Satisfy Hunger */
			(void)set_food(PY_FOOD_MAX - 1);
        break;
        case 23: /* See Invisible */
			(void)set_tim_invis(p_ptr->tim_invis + randint(24) + 24);
        break;
        case 24: /* Recharging */
               (void)recharge(plev * 2);
               break;
        case 25: /* Teleport Level */
			(void)teleport_player_level();
        break;
        case 26: /* Identify */
			(void)ident_spell();
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
            fire_ball(dummy, dir,
                    75 + (plev), 2);
        break;
        case 29: /* Detection */
			(void)detect_all();
        break;
        case 30: /* Word of Recall */
			{
                if (dun_level && (p_ptr->max_dlv[cur_dungeon] > dun_level) && (cur_dungeon == recall_dungeon))
                {
                    if (get_check("Reset recall depth? "))
                    p_ptr->max_dlv[cur_dungeon] = dun_level;

                }
				if (!p_ptr->word_recall)
				{
					p_ptr->word_recall = rand_int(21) + 15;
					if (dun_level > 0)
					{
						recall_dungeon = cur_dungeon;
					}
					else
					{
						cur_dungeon = recall_dungeon;
					}
					msg_print("The air about you becomes charged...");
				}
				else
				{
					p_ptr->word_recall = 0;
					msg_print("A tension leaves the air around you...");
				}
				break;
            }
        case 31: /* Clairvoyance */
			wiz_lite();
            if (!(p_ptr->telepathy))
            {
                (void)set_tim_esp(p_ptr->tim_esp + randint(30) + 25);
            }
        break;
        default:
        msg_format("You cast an unknown cantrip: %d.", spell);
        msg_print(NULL);
    }
		/* A cantrip was cast */
		/* Gain experience with hedge skill */
		if (skill_set[SKILL_HEDGE].value < s_ptr->minskill + 50) {
			skill_exp(SKILL_HEDGE);
		}
	}

	/* Take some time - a cantrip always takes 100, unless the charm is in a pouch */
	if (from_pouch)
	{
		energy_use = 10;
	}
	else
	{
		energy_use = 100;
	}

	/* If item is going to break, give it a chance of survival at low skill levels, on
	 * the assumption that the user didn't manage to do anything to the charm.  This will
	 * allow the practice of hedge magic at low skill levels without destroying so many
	 * charms.
	 *
	 * Otherwise, give it a slight chance of breaking anyway.
	 */
	if (item_break) {
		if (rand_int(10) > skill_set[SKILL_HEDGE].value) {
			msg_print("The charm remains completely inert.");
			item_break = FALSE;
		}
	} else {
		if (rand_int(1000) < s_ptr->mana) item_break=TRUE;
	}

	if (item_break)
	{
		/* Dangerous Hack -- Destroy the item */
		msg_print("The charm crumbles, drained of magic.");

		/* Reduce and describe inventory */
		if (item >= 0)
		{
			inven_item_increase(item, -1);
			inven_item_describe(item);
			inven_item_optimize(item);
		}

		/* Reduce and describe floor item */
		else
		{
			floor_item_increase(0 - item, -1);
			floor_item_describe(0 - item);
			floor_item_optimize(0 - item);
		}
	}
	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);
	p_ptr->window |=(PW_SPELL);
}


/*
 * calculate the annoyance factor of a favour
 */
s32b favour_annoyance(favour_type *f_ptr)
{
	s32b annoy;

	/* Base annoyance is taken from the spell */
	annoy = f_ptr->annoy_inc + rand_int(10)+5;

	/* decrease based on charisma bonus for studying and skill*/
	annoy -= (adj_mag_study[p_ptr->stat_ind[A_CHR]] * (skill_set[SKILL_SHAMAN].value/20));

	/* make sure there is at least a bit */
	if (annoy <2) annoy = 2;
	
	return (annoy);
}

/*
 * annoy a spirit
 */
void annoy_spirit(spirit_type *s_ptr,u32b amount)
{
	u32b old_annoy;
	p_ptr->redraw |= (PR_SPIRIT);
	old_annoy=s_ptr->annoyance;
	s_ptr->annoyance += amount;
	
	if ((s_ptr->annoyance > 15) && (old_annoy < 16))
	{
		msg_format("%s is furious.",s_ptr->name);
	}
	else if((s_ptr->annoyance > 8) && (old_annoy < 9))
	{
		msg_format("%s gets angry.",s_ptr->name);
	}
	else if((s_ptr->annoyance > 3) && (old_annoy < 4))
	{
		msg_format("%s is getting annoyed.",s_ptr->name);
	}
	else if((s_ptr->annoyance > 0) && (old_annoy < 1))
	{
		msg_format("You have irritated %s.",s_ptr->name);
	}
}

/*
 * invoke a favour
 */
void do_cmd_invoke(void)
{
	int	spell, dir;
	int	chance, beam;
	u16b	plev = 0;
	int	favour_sphere = 0, dummy = 0;
	int	ii = 0, ij = 0;
	int spirit;
	bool	none_came = FALSE;

	spirit_type	*s_ptr;

	favour_type	*f_ptr;

	/* Not when confused */
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}

	/* Ask for a spirit */
	if (!get_spirit(&spirit,"call upon",TRUE))
	{
		if (spirit == -2)
		{
			msg_print("You must form a pact with a spirit before they will listen.");
		}
		return;
	}

	/* Get a pointer to the spirit */
	s_ptr = &(spirits[spirit]);

	/* Hack -- Handle stuff */
	handle_stuff();

	/* Ask for a favour */
	if (!get_favour(&spell,spirit, s_ptr->sphere))
	{
		if (spell == -2)
		msg_print("Strange - spirits normally know spells!");
		return;
	}


	/* Access the spell */
	favour_sphere = s_ptr->sphere;

	f_ptr = &(favour_info[favour_sphere][spell]);

	/* Access the player's skill */
	plev = skill_set[SKILL_SHAMAN].value/2;
	if (plev == 0) plev++;

	/* Spell failure chance */
	chance = favour_chance(spell,favour_sphere);

	/* Failed spell */
	if ((rand_int(100) < chance) || (s_ptr->annoyance))
	{
		if (flush_failure) flush();
		msg_format("%s refuses your call!",s_ptr->name);
		/* the call still took energy */
		energy_use = spell_energy(plev,(u16b)(f_ptr->minskill));
		/* The spirit still gets somewhat pissed off */
		annoy_spirit(s_ptr,rand_int(favour_annoyance(f_ptr)));
		/* Chance for retribution based on level of favour */
		if(rand_int(100) < spell)
		{
			msg_format("%s curses you",s_ptr->name);
			activate_ty_curse();
		}
	}
	/* Process spell */
	else
	{
		/* the call takes energy */
		energy_use = spell_energy(plev,(u16b)(f_ptr->minskill));
		/* The spirit gets pissed off */
		annoy_spirit(s_ptr,favour_annoyance(f_ptr));

    beam = plev;
	/* Spells.  */
	/* Spells.  */
	switch (favour_sphere)
	{
	case 0: /* * LIFE * */
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
				(damroll(3, 6) + plev + (plev /  4)),((plev < 30) ? 2 : 3));
		       break;
	   case 13: /* Protection from Evil */
			(void)set_protevil(p_ptr->protevil + randint(25) + 3 * plev);
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
         project(0, 1, py, px, 777, GF_HOLY_FIRE,   PROJECT_KILL);
         dispel_monsters(plev * 4);
         slow_monsters(plev * 4);
         stun_monsters(plev*4);
         confuse_monsters(plev*4);
         turn_monsters(plev*4);
         stasis_monsters(plev*4);
         (void)set_shero(p_ptr->shero + randint(25) + 25);
         (void)hp_player(300);
         if (!p_ptr->fast) {   /* Haste */
         (void)set_fast(randint(20 + (plev) ) + plev);
         } else {
         (void)set_fast(p_ptr->fast + randint(5));
         }
         (void)set_afraid(0);
         break;
	   case 31: /* Holy Invulnerability */
			(void)set_invuln(p_ptr->invuln + randint(7) + 7);
		       break;
	       default:
		 msg_format("You cast an unknown Life spell: %d.", spell);
		 msg_print(NULL);
	   }
	  break;
	case 1: /* * NATURE * */
	  switch (spell)
	  {
	   case 0: /* Detect Creatures */
			(void)detect_monsters_normal();
		       break;
	   case 1: /* First Aid */
			(void)hp_player(damroll(2, 8));
			(void)set_cut(p_ptr->cut - 15);
		       break;
	   case 2: /* Detect Doors + Traps */
			(void)detect_traps();
			(void)detect_doors();
			(void)detect_stairs();
		       break; 
	   case 3: /* Produce Food */
			(void)set_food(PY_FOOD_MAX - 1);
		       break;
       case 4: /* Daylight */
               (void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
            if ((p_ptr->prace == RACE_VAMPIRE) && !(p_ptr->resist_lite))
            {
                msg_print("The daylight scorches your flesh!");
                take_hit(damroll(2,2), "daylight");
                            }
               break;
       case 5: /* Animal Taming */
         if (!get_aim_dir(&dir)) return;
         (void) charm_animal(dir, plev);
         break;
       case 6: /* Resist Environment */
			(void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
			(void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
			(void)set_oppose_elec(p_ptr->oppose_elec + randint(20) + 20);
		       break;
       case 7: /* Cure Wounds + Poison */
            (void)set_cut(0);
			(void)set_poisoned(0);
		       break;
	   case 8: /* Stone to Mud */
			if (!get_aim_dir(&dir)) return;
			(void)wall_to_mud(dir);
		       break;
	   case 9: /* Lightning Bolt */
				if (!get_aim_dir(&dir)) return;
				fire_bolt_or_beam(beam-10, GF_ELEC, dir,
						  damroll(3+((plev-5)/4), 8));
		       break;
       case 10: /* Nature Awareness -- downgraded */
			map_area();
			(void)detect_traps();
			(void)detect_doors();
			(void)detect_stairs();
			(void)detect_monsters_normal();
            break;
	   case 11: /* Frost Bolt */
			if (!get_aim_dir(&dir)) return;
			fire_bolt_or_beam(beam-10, GF_COLD, dir,
				damroll(5+((plev-5)/4), 8));
		       break;
	   case 12: /* Ray of Sunlight */
			if (!get_aim_dir(&dir)) return;
			msg_print("A line of sunlight appears.");
			lite_line(dir);
		       break;
	   case 13: /* Entangle */
			slow_monsters(plev * 4);
		       break;
       case 14: /* Summon Animals */
             if (!(summon_specific_friendly(py, px, plev, SUMMON_ANIMAL_RANGER, TRUE)))
                none_came = TRUE;
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
       case 19: /* Resistance True */
			(void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20);
			(void)set_oppose_elec(p_ptr->oppose_elec + randint(20) + 20);
			(void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
			(void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
			(void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20);
		       break;
        case 20: /* Animal Friendship */
        (void) charm_animals(plev * 2);
         break;
	   case 21: /* Stone Tell */
		identify_fully();
		       break;
       case 22: /* Wall of Stone */
		(void)wall_stone();
		       break;
       case 23: /* Protection from Corrosion */
               rustproof();
		       break;
       case 24: /* Earthquake */
			earthquake(py, px, 10);
		       break;
       case 25: /* Whirlwind Attack */
         {
         int y = 0, x = 0;
         cave_type       *c_ptr;
         monster_type    *m_ptr;

         for (dir = 0; dir <= 9; dir++) {
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
       case 26: /* Blizzard */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_COLD, dir,
				70 + (plev), (plev/12)+1);
		       break;
	   case 27: /* Lightning Storm */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_ELEC, dir,
				90 + (plev), (plev/12)+1);
		       break;
	   case 28: /* Whirlpool */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_WATER, dir,
				100 + (plev), (plev/12)+1);
		       break;
	   case 29: /* Call Sunlight */

			fire_ball(GF_LITE, 0, 150, 8);
			wiz_lite();
            if ((p_ptr->prace == RACE_VAMPIRE) && !(p_ptr->resist_lite))
            {
                msg_print("The sunlight scorches your flesh!");
                take_hit(50, "sunlight");
            }
		       break;
	   case 30: /* Elemental Brand */
            brand_weapon(0);
		       break;
	   case 31: /* Nature's Wrath */
            (void)dispel_monsters(plev * 4);
            earthquake(py, px, 20 + (plev / 2) );
         project(0, 1+plev/12, py, px,
             100+plev, GF_DISINTEGRATE, PROJECT_KILL|PROJECT_ITEM);
		       break;
	       default:
		 msg_format("You cast an unknown Nature spell: %d.", spell);
		 msg_print(NULL);
	   }
      if (none_came)
        msg_print("No animals arrive.");
	  break;
	 default:
		  msg_format("You invoke a favour from an unknown sphere: sphere %d, spell %d.", favour_sphere, spell);
		  msg_print(NULL);
	    }
		/* Gain experience with spirit lore skill */
		if (skill_set[SKILL_SHAMAN].value < f_ptr->minskill * 2 + 50) {
			skill_exp(SKILL_SHAMAN);
		}
	}

	/* Take some time - a spell of your level takes 100, lower level spells take less */
	energy_use = spell_energy((u16b)plev,(u16b)(f_ptr->minskill));

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);
	p_ptr->window |=(PW_SPELL);
}



void mindcraft_info(char *p, int power)
{
    int plev = skill_set[SKILL_MINDCRAFTING].value/2;

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
	int                     i;

	int                     num = 0;
    int y = 1;
    int x = 20;
    int minfail = 0;
    
        int  psi = skill_set[SKILL_MINDCRAFTING].value/2;
    int chance = 0;
    
    bool            flag, redraw;
    int             ask;
	char            choice;

        mindcraft_power spell;
    
	char            out_val[160];
        char            comment[80];
    
    cptr p = "power";

	/* Assume cancelled */
	*sn = (-1);

 #ifdef ALLOW_REPEAT /* TNB */
 
     /* Get the spell, if available */
     if (repeat_pull(sn)) {
         
         /* Verify the spell */
         if (mindcraft_powers[*sn].min_lev <= psi) {
 
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
	      if (mindcraft_powers[i].min_lev <= psi)
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
			    put_str("Sk  Chi Fail Info", y, x + 35);

			    /* Dump the spells */
			    for (i = 0; i < MAX_MINDCRAFT_POWERS; i++)
				{
				    /* Access the spell */
				    spell = mindcraft_powers[i];
				    if (spell.min_lev > psi)   break;

				    chance = spell.fail;
                    /* Reduce failure rate by "effective" level adjustment */
                    chance -= 3 * ((skill_set[SKILL_MINDCRAFTING].value/2) - spell.min_lev);

                    /* Reduce failure rate by INT/WIS adjustment */
                    chance -= 3 * (adj_mag_stat[p_ptr->stat_ind[A_WIS]] - 1);

				    /* Not enough mana to cast */
				    if (spell.mana_cost > p_ptr->cchi)
					{
                        chance += 5 * (spell.mana_cost - p_ptr->cchi);
					}

                    /* Extract the minimum failure rate */
                    minfail = adj_mag_fail[p_ptr->stat_ind[A_WIS]];
				    
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
                        I2A(i), spell.name,
                        spell.min_lev*2, spell.mana_cost, chance, comment);
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
 
 #ifdef ALLOW_REPEAT /* TNB */
 
     repeat_push(*sn);
 
 #endif /* ALLOW_REPEAT -- TNB */

	/* Success */
	return (TRUE);
}


void do_cmd_mindcraft(void)
{
    int   n = 0,  b = 0;
    int chance;
    int dir;
    int minfail = 0;
    int psi = skill_set[SKILL_MINDCRAFTING].value/2;
    
    mindcraft_power   spell;
    
    /* not if confused */
    if (p_ptr->confused) {
	msg_print("You are too confused!");
	return;
    }
    
    /* get power */
    if (!get_mindcraft_power(&n))  return;
	
    spell = mindcraft_powers[n];
    
    /* Verify "dangerous" spells */
    if (spell.mana_cost > p_ptr->cchi) {
	/* Warning */
	msg_print("You do not have enough chi to use this power.");
	/* Verify */
	if (!get_check("Attempt it anyway? ")) return;
    }
    
    /* Spell failure chance */
    chance = spell.fail;
    /* Reduce failure rate by "effective" level adjustment */
    chance -= 3 * (psi - spell.min_lev);

    /* Reduce failure rate by INT/WIS adjustment */
    chance -= 3 * (adj_mag_stat[p_ptr->stat_ind[A_WIS]] - 1);

    /* Not enough mana to cast */
    if (spell.mana_cost > p_ptr->cchi)
	{
	    chance += 5 * (spell.mana_cost - p_ptr->cchi);
	}

    /* Extract the minimum failure rate */
    minfail = adj_mag_fail[p_ptr->stat_ind[A_WIS]];
				    
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

	    if (randint(100) < (chance/2))
		{    /* Backfire */
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
			{   /* Mana storm */
				msg_print("Your mind unleashes its power in an uncontrollable storm!");
				project(1, 2+psi/10, py, px,
                psi * 2, GF_MANA,PROJECT_JUMP|PROJECT_KILL|PROJECT_GRID|PROJECT_ITEM);
				p_ptr->cchi = MAX(0, p_ptr->cchi - psi * MAX(1, psi/10));
			}
	    }
	}
	else
	{
	    /* spell code */
	    switch (n)
		{
		case 0:   /* Precog */
			if (psi > 44)
			{
				wiz_lite();
			}
			else if (psi > 19)
			{
				map_area();
			}
			if (psi < 30)
			{
				b = detect_monsters_normal();
				if (psi > 14)  b |=  detect_monsters_invis();
				if (psi > 4)   b |=  detect_traps();
			}
			else
			{
				b = detect_all();
			}
			if ((psi > 24) && (psi < 40)) set_tim_esp(p_ptr->tim_esp + psi);
			if (!b)  msg_print("You feel safe.");
			break;
		case 1:   /* Mindblast */
			if (!get_aim_dir(&dir)) return;
			if (randint(100) < psi * 2)
			{
				fire_beam(GF_PSI, dir, damroll(3 + ((psi - 1) / 4), (3+psi/15)));
			}
			else
			{
				fire_ball(GF_PSI, dir, damroll(3 + ((psi - 1) / 4), (3+psi/15)), 0);
			}
			break;
		case 2:   /* Minor displace */
			if (psi < 25)
			{
				teleport_player(10);
			}
			else
			{
				int i = 0, j = 0;
				msg_print("Choose a destination.");
				if (!tgt_pt(&i,&j)) return;
				p_ptr->energy -= 60 - psi;
				if (!cave_empty_bold(j,i) || (cave[j][i].info & CAVE_ICKY) || (cave[j][i].feat == FEAT_WATER) ||
				(distance(j,i,py,px) > psi + 2) || (!rand_int(psi * psi / 2)))
				{
					msg_print("Something disrupts your concentration!");
					p_ptr->energy -= 100;
					teleport_player(20);
				}
				else
				{
					teleport_player_to(j,i);
				}
				break;
			}
			break;
		case 3:   /* Major displace */
			if (psi > 29) banish_monsters(psi);
			teleport_player(psi * 5);
			break;
		case 4:   /* Domination */
			if (psi < 30)
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_DOMINATION, dir, psi, 0);
			}
			else
			{
				charm_monsters(psi * 2);
			}
			break;
		case 5:   /* Fist of Force  ---  not 'true' TK  */
			if (!get_aim_dir(&dir)) return;
			fire_ball(GF_SOUND, dir, damroll(8+((psi-5)/4), 8),(psi > 20 ? (psi-20)/8 + 1 : 0));
			break;
		case 6:   /* Character Armour */
			set_shield(p_ptr->shield + psi);
			if (psi > 14)   set_oppose_acid(p_ptr->oppose_acid + psi);
			if (psi > 19)   set_oppose_fire(p_ptr->oppose_fire + psi);
			if (psi > 24)   set_oppose_cold(p_ptr->oppose_cold + psi);
			if (psi > 29)   set_oppose_elec(p_ptr->oppose_elec + psi);
			if (psi > 34)   set_oppose_pois(p_ptr->oppose_pois + psi);
			break;
	    case 7:   /* Psychometry */
			if (psi < 40)
			{
				psychometry();
			}
			else
			{
				ident_spell();
			}
			break;
		case 8:   /* Mindwave */
			msg_print("Mind-warping forces emanate from your brain!");
			if (psi < 25)
			{
				project(0, 2+psi/10, py, px,(psi*3)/2, GF_PSI, PROJECT_KILL);
			}
			else
			{
				(void)mindblast_monsters(psi * ((psi-5) / 10 + 1));
			}
			break;
	    case 9:   /* Adrenaline */
			set_afraid(0);
			set_stun(0);
			hp_player(psi);
			b = 10 + randint((psi*3)/2);
			if (psi < 35)
			{
				set_hero(p_ptr->hero + b);
			}
			else
			{
				set_shero(p_ptr->shero + b);
			}
			if (!p_ptr->fast)
			{   /* Haste */
				(void)set_fast(b);
			}
			else
			{
				(void)set_fast(p_ptr->fast + b);
			}
			break;
	    case 10:   /* Psychic Drain */
			if (!get_aim_dir(&dir)) return;
			b = damroll(psi/2, 6);
			if (fire_ball(GF_PSI_DRAIN, dir, b,  0 + (psi-25)/10))
			p_ptr->energy -= randint(150);
			break;
		case 11:   /* Telekinesis */
			msg_print("A wave of pure physical force radiates out from your body!");
			project(0, 3+psi/10, py, px,
            psi * (psi > 39 ? 4 : 3), GF_TELEKINESIS, PROJECT_KILL|PROJECT_ITEM|PROJECT_GRID);
			break;
	    default:
			msg_print("Zap?");
		}
		/* Get some practice */
		if (skill_set[SKILL_MINDCRAFTING].value < spell.min_lev * 2 + 50) {
			skill_exp(SKILL_MINDCRAFTING);
			skill_exp(SKILL_CHI);
		}
	}
	/* Take a turn */
	energy_use = spell_energy((u16b)psi,(u16b)(spell.min_lev));

	/* Sufficient mana */
	if (spell.mana_cost <= p_ptr->cchi)
	{
		/* Use some mana */
		p_ptr->cchi -= spell.mana_cost;
	}

	/* Over-exert the player */
	else
	{
		int oops = spell.mana_cost - p_ptr->cchi;

		/* No mana left */
		p_ptr->cchi = 0;
		p_ptr->chi_frac = 0;

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


