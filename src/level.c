/* File: level.c */

/* purpose: This file will handle the various effects of going up a level. I.e. 
 * ability to choose new talents, skills, raise stats, gain mutations and other
 * things.
 */
 
/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"


void level_reward(void)
{

	/* This returns 1 for odd levels, and 0 for even */
	int even_level	= (p_ptr->max_lev % 2);
	int b 			= randint(100);
	int choice;
	
	u32b f1, f2, f3;
	player_flags(&f1, &f2, &f3);

	/* Anarchists mutate often */
	if (cp_ptr->flags & CF_ANARCHIST)
	{
		if (b < 2)
		{
			gain_random_mutation(0);
			gain_random_mutation(0);
			gain_random_mutation(0);
			lose_mutation(0);
		}
		else if (b < 80)
		{
		gain_random_mutation(0);
		}
		else if (b < 98)
		{
		lose_mutation(0);
		}
		else 	
		{
		gain_random_mutation(0);
		lose_mutation(0);
		lose_mutation(0);
		lose_mutation(0);
		}
	}
	/* other Races / Classes mutate less */
	if (cp_ptr->flags & CF_MUTABLE || (f3 & (TR3_MUTABLE)))
	{

		if((even_level == 0) && (randint(100) < 40))
		{
			if(b < 80)
			{
			gain_random_mutation(0);
			} 
			else 
			{
			lose_mutation(0);
			}
			
		} 
	}
	
	/* Next we increase a stat on level gain. */
	/* every level. o_o */
	/* Hmmmmm. . . Two winners in less than two weeks. Not ever level do */
	/* we get a choice. */
	screen_save();
	if (f3 & (TR3_AUTOMATA))
	{
	/* Do nothing */
	}
	else if (!(p_ptr->max_lev % 3))
	{
		while(1)
		{
			char tmp[32];
			
			/* Display the stats on screen */
				   prt("  Which stat do you want to raise?", 2, 10);
				   prt(" ___________________________________", 3, 14);
				   prt(" |{{{{{{{{{** STAT GAIN **}}}}}}}}}|", 4, 14);
				   prt(" ____~~~~~~~~~~~~~~~~~~~~~~~~~~~____", 5, 14);
			cnv_stat(p_ptr->stat_max[0], tmp);
			prt(format(" |^^|    a) Str (cur %s)    |^^|", tmp), 6, 14);
			cnv_stat(p_ptr->stat_max[1], tmp);
			prt(format(" ||||    b) Int (cur %s)    ||||", tmp), 7, 14);
			cnv_stat(p_ptr->stat_max[2], tmp);
			prt(format(" ||||    c) Wis (cur %s)    ||||", tmp), 8, 14);
			cnv_stat(p_ptr->stat_max[3], tmp);
			prt(format(" ||||    d) Dex (cur %s)    ||||", tmp), 9, 14);
			cnv_stat(p_ptr->stat_max[4], tmp);
			prt(format(" ||||    e) Con (cur %s)    ||||", tmp), 10, 14);
			cnv_stat(p_ptr->stat_max[5], tmp);
			prt(format(" |^^|    f) Chr (cur %s)    |^^|", tmp), 11, 14);
				   prt(" ___________________________________", 12, 14);
				   prt(" _+++++++++++++++++++++++++++++++++_", 13, 14);
				   prt(" ___________________________________", 14, 14);
			prt("", 15, 14);
			
			/* Loop to insure proper input from the ID10T interface */
			while(1)
			{	
				/* Get the choice */
				choice = inkey();
				/* if the choice has the proper value then we can leave */
				/*this loop. If not Well we'll just stay here forever */
				if ((choice >= 'a') && (choice <= 'f')) break;
			}
	
			/* 
			 * Uhhhhhhh - I didn't understand what was happening 
			 * with the original code. (which is commented out   
			 * below), so I just typed out the options 
			 * for(n = 0; n < 6; n++)
			 *	if (n != choice - 'a')
			 *  prt("",n+2,14);
			 */
			if (get_check("Are you sure? ")) break;
			
		}
		if (choice == 'a') do_inc_stat(A_STR); 
		if (choice == 'b') do_inc_stat(A_INT);
		if (choice == 'c') do_inc_stat(A_WIS); 
		if (choice == 'd') do_inc_stat(A_DEX);
		if (choice == 'e') do_inc_stat(A_CON);
		if (choice == 'f') do_inc_stat(A_CHR);
	
	
	}	
	else 
	{
	do_inc_stat(rand_int(6));
	}
	screen_load();
}
