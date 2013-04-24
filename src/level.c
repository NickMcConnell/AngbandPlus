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

/* I'm going to have to change this function so that it's 'called'
 * every time a key is pressed, and it only does something each time 
 * the character gains a level. Until this happens consider it broken -CCC
 */

void level_reward(void)
{

	/* This returns 1 for odd levels, and 0 for even */
	int even_level	= (p_ptr->max_lev % 2);
	int b 			= randint(100);
	
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
}

/* This screen handles gaining stats, and adding points to skills */
void do_cmd_gain_level(void)
{
	char ch;
	int i;
	cptr p;
	char buf[80];
	int selected;

	/* Prompt */
	p = "['g' to gain stats, arrow keys to select stats, +/= to raise or ESC]";

	screen_save();

	/* Erase screen */
	clear_from(0);

	/* reset selected */
	selected = 0;
	
	while (p_ptr->skills[selected].skill_max == -2){selected++;}
	
	while (1)
	{

		/* Misc Info */
		display_player_misc_info();

		/* Stat info */
		display_player_stat_info();

		/* This function doesn't exist- does now */
		print_all_skills(selected);

		/* Prompt */
		Term_putstr(2, 23, -1, TERM_WHITE, p);
		
		put_str("Stat Growth:", 3, 22);
		sprintf(buf, "%2d", p_ptr->free_sgain);
		c_put_str(TERM_L_BLUE, buf, 3, 36);
		
		put_str("Skill Points:", 4, 22);
		sprintf(buf, "%2d", p_ptr->free_skpts);
		c_put_str(TERM_L_BLUE, buf, 4, 36);
		/* Hack -- hide the cursor  XXX XXX */
		Term_gotoxy(0, 26);

		/* Query */
		ch = inkey();

		/* Exit */
		if (ch == ESCAPE) break;

		/* Gain stats */
		if (ch == 'g')
		{
			screen_save();
			/* if stat growth is available */
			if (p_ptr->free_sgain > 0)
			{
				/* Raise the stats */
				for (i = 0; i < A_MAX; i++)
				{
					do_inc_stat(i);
			 	}
			 	/* reduce the amount of stat gains by one */
			 	p_ptr->free_sgain -= 1;
			}
			screen_load();
		}
		
		/* Prev skill */
		if (ch == '8')
		{
		 	if (p_ptr->skills[selected].skill_index == 1)
		 	{
		 		continue;
		 	}
		 	/* if it's an active skill - go down once */
		 	if (selected > 0 && p_ptr->skills[selected].skill_max > -2)
		 	{
		 		/* go down */
		 		selected--;		 		
		 	}
			/* if it's inactive - Keep going down */
			/* until the next active skill is reached */
			while (p_ptr->skills[selected].skill_index == 0)
			{
		  		/* go down */
		  		selected--;
			}
		 	continue;
		}

		/* Next skill */
		if (ch == '2')
		{
		 	if (p_ptr->skills[selected].skill_index == skill_count)
		 	{
		 		continue;
		 	}
			/* if it's an active skill - go up once */
			if (p_ptr->skills[selected].skill_max > -2) 
 			{
 				selected++;
			}
			/* if it's inactive - keep going down */
			/* until the next active skill is reached */
			while (p_ptr->skills[selected].skill_max == -2 
					&& selected < N_SKILLS)
			{
		  		selected++;
			}
		}
		/* Move left a col */
		/* I don't even know if this is possible! */
		if (ch == '4')
		{
		}
		/* Move right a col */
		if (ch == '6')
		{
		}

		if ((ch == '+') || (ch == '='))
		{
			/* skill is raised - Once a skill is raised it's permanant*/
			skill_up(selected); 
		}
		if ((ch == '-') || (ch == '_'))
		{
		/* decrement skill - this doesn't ever actually happen */
		}

		else{}
		
		

	}
	/* Load screen */
	screen_load();	
	
	/* Redraw Level status */
	p_ptr->redraw |= (PR_LEVEL);

	
	/* Update stuff */
	update_stuff();


}	

