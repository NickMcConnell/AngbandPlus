/* File: level.c */
/* purpose: This file will handle the various effects of going up a level. I.e. 
 * ability to choose new talents, skills, raise stats, gain mutations and other
 * things.
 */
 
/*
 * "So far," the captian of our journey into the heart of the jungle said,
 * "I can give you stuff for going up a 'level' whatever that means, or 
 * display an interface so that you can allocate skill points and raise 
 * stats"
 *
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"


static void do_raise_stats(void)
{
	int i, j, msec, align;
	
	for (i = 0; i < A_MAX; i++)
	{
		/* Attempt to increase */
		if (inc_stat(i))
		{
			for (j = 0; j < 5; j++)
			{
				
				if (i < 3) align = 28;
				else align = 53;
				msec = op_ptr->delay_factor * op_ptr->delay_factor * 2;
				
				if (j == 4)	put_str(">", 4+(i % 3), align+4+j);
				else put_str("=", 4+(i % 3), align+4+j);

				Term_fresh();
				Term_xtra(TERM_XTRA_DELAY, msec);
			
			}

			 put_str("     ", 4+(i % 3), align+4);
		}
	}
	
	/* Message */
	/* message_format(MSG_LEVEL, 0, "You feel more powerful!"); */
	msg_print ("You feel more powerful!");
}

/* This screen handles gaining stats, and adding points to skills */
void do_cmd_gain_level(bool train)
{
	char ch;
	int i;
	cptr prompt, p;
	cptr bar, barg1, barg2, barc1, barc2, bars1, bars2;
	int col, row;
	char buf[80];
	char tmp[160];

	int selected;
	int mode;

	/* Prompt */
	prompt = "['g' stat+, 'h' next tab, '2' down, '8' up, + to raise, '?' spoilers or ESC]";
	
	/* Menu bars */
	barg1= "  -------------------------";
	barg2= "/ General / Racial / Class | ......................    .....................";
	bar=   "----------------------------------------------------------------------------";
	barc1= "                           ------------------------";
	barc2= " ....................... / Melee / Ranged / Combat |   .....................";
	bars1= "                                                    ----------------------- ";
	bars2= " .......................  ....................... / Spells / Items / Other |";

	screen_save();

	/* Erase screen */
	clear_from(0);

	/* reset selected */
	selected = 0;
	
	/* reset mode */
	mode = 1;
	
	while ((p_ptr->skills[selected].skill_max == -2) ||
		   (p_ptr->skills[selected].skill_type != mode)){selected++;}
	
	while (1)
	{
		if (mode > 3) 
		{
			mode = 1;
			selected = 0;
			while (((p_ptr->skills[selected].skill_max == -2) ||
					(p_ptr->skills[selected].skill_type != mode))
					&& selected < N_SKILLS)
			{
		  		selected++;
			}
		}
		
		/* Name */
		put_str("Name", 2, 1);
		c_put_str(TERM_L_BLUE, op_ptr->full_name, 2, 8);
	
	
		/* Sex */
		put_str("Sex", 3, 1);
		c_put_str(TERM_L_BLUE, sp_ptr->title, 3, 8);
	
	
		/* Race */
		put_str("Race", 4, 1);
		c_put_str(TERM_L_BLUE, p_name + rp_ptr->name, 4, 8);
	
	
		/* Class */
		put_str("Class", 5, 1);
		c_put_str(TERM_L_BLUE, c_name + cp_ptr->name, 5, 8);
	
	
		/* Title */
		put_str("Title", 6, 1);
	
		/* Wizard */
		if (p_ptr->wizard)
		{
			p = "[=-WIZARD-=]";
		}
	
		/* Winner */
		else if (p_ptr->total_winner || (p_ptr->lev > PY_MAX_LEVEL))
		{
			p = "***WINNER***";
		}
	
		/* Normal */
		/* Display Proper "Genderized" Class Titles */
		else if (p_ptr->psex == SEX_FEMALE)
		{
			p = c_text + cp_ptr->ftitle[(p_ptr->lev - 1) / 5];
		}
		else
		{
			p = c_text + cp_ptr->mtitle[(p_ptr->lev - 1) / 5];
		}
	
		/* Dump it */
		c_put_str(TERM_L_BLUE, p, 6, 8);

		col = 22;
		row = 4;
		
		if (train)
		{
			c_prt(TERM_YELLOW, "Gold Remaining: ", 5, col + 9);
		
			sprintf(tmp, "%9ld", (long)p_ptr->au);
			prt(tmp, 5, col + 25);
			
			prt("Cost to Raise Skill Maximum: ", 6, col - 4);
			sprintf(buf, "%9ld", (long)p_ptr->lev * SKILL_INCREASE_CONSTANT);
			prt(buf, 6, col + 25);
		}
		else
		{		
			/* Print out the labels for the columns */
			c_put_str(TERM_WHITE, "   Old", 3, col+4);
			c_put_str(TERM_WHITE, "   New", 3, col+12);
	
			/* Display the stats */
			for (i = 0; i < 3; i++)
			{
				/* Reduced */
				if (p_ptr->stat_use[i] < p_ptr->stat_top[i])
				{
					/* Use lowercase stat name */
					put_str(stat_names_reduced[i], row+i, col);
				}
		
				/* Normal */
				else
				{
					/* Assume uppercase stat name */
					put_str(stat_names[i], row+i, col);
				}
		
				/* Indicate natural maximum */
				if (p_ptr->stat_max[i] == 999)
				{
					put_str("!", row+i, col+3);
				}
		
				/* 
					'Old' value - works like this, without storing as stat_use[] values are only modified, 
					when this screen is exited - show the currnet not the maximum value as the current value is
					increased also 
				*/
				cnv_stat(((p_ptr->stat_use[i])-(p_ptr->stat_add[i] * 10)), buf);    
				if (p_ptr->stat_use[i] != p_ptr->stat_top[i])
					c_put_str(TERM_YELLOW, buf, row+(i%3), col+4);
				else
					c_put_str(TERM_L_GREEN, buf, row+(i%3), col+4);
		
				/* 
					'New' value stat_cur[] is increased when do_raise_stats() is called so this is the 
					new actual value 
				*/
				cnv_stat(p_ptr->stat_cur[i], buf);	
				if (p_ptr->stat_use[i] != p_ptr->stat_top[i])
					c_put_str(TERM_YELLOW, buf, row+(i%3), col+12);
				else
					c_put_str(TERM_L_GREEN, buf, row+(i%3), col+12);
			}
	
			row = 4;
			col = 47;
			
			/* Print out the labels for the columns */
			c_put_str(TERM_WHITE, "   Old", 3, col+4);
			c_put_str(TERM_WHITE, "   New", 3, col+12);
	
			/* Display the stats */
			for (i = 3; i < A_MAX; i++)
			{
				/* Reduced */
				if (p_ptr->stat_use[i] < p_ptr->stat_top[i])
				{
					/* Use lowercase stat name */
					put_str(stat_names_reduced[i], row+(i%3), col);
				}
		
				/* Normal */
				else
				{
					/* Assume uppercase stat name */
					put_str(stat_names[i], row+(i%3), col);
				}
		
				/* Indicate natural maximum */
				if (p_ptr->stat_max[i] == 999)
				{
					put_str("!", row+(i%3), col+3);
				}
		
				/* 
					'Old' value - works like this, without storing as stat_use[] values are only modified, 
					when this screen is exited - show the currnet not the maximum value as the current value is
					increased also 
				*/
				cnv_stat(((p_ptr->stat_use[i])-(p_ptr->stat_add[i] * 10)), buf);    
				if (p_ptr->stat_use[i] != p_ptr->stat_top[i])
					c_put_str(TERM_YELLOW, buf, row+(i%3), col+4);
				else
				c_put_str(TERM_L_GREEN, buf, row+(i%3), col+4);
				/* 
					'New' value stat_cur[] is increased when do_raise_stats() is called so this is the 
					new actual value 
				*/
				cnv_stat(p_ptr->stat_cur[i], buf);	
				if (p_ptr->stat_use[i] != p_ptr->stat_top[i])
					c_put_str(TERM_YELLOW, buf, row+(i%3), col+12);
				else
					c_put_str(TERM_L_GREEN, buf, row+(i%3), col+12);
			}
		}
		
		/* This function displays skills */
		print_all_skills(selected, mode, train);


		/* Prompt */
		Term_putstr(2, 23, -1, TERM_WHITE, prompt);
		Term_putstr(2, 10, -1, TERM_WHITE, bar);
		if (mode == 1) Term_putstr(2, 8, -1, TERM_WHITE, barg1);
		if (mode == 1) Term_putstr(2, 9, -1, TERM_WHITE, barg2);
		if (mode == 2) Term_putstr(2, 8, -1, TERM_WHITE, barc1);
		if (mode == 2) Term_putstr(2, 9, -1, TERM_WHITE, barc2);
		if (mode == 3) Term_putstr(2, 8, -1, TERM_WHITE, bars1);
		if (mode == 3) Term_putstr(2, 9, -1, TERM_WHITE, bars2);
		
		if (p_ptr->free_sgain) c_put_str(TERM_RED, "Stat Growth:", 2, 22);
		else put_str("Stat Growth:", 2, 22);
		sprintf(buf, "%2d", p_ptr->free_sgain);
		if (p_ptr->free_sgain > 3) c_put_str(TERM_VIOLET, buf, 2, 36);
		else if (p_ptr->free_sgain) c_put_str(TERM_RED, buf, 2, 36);
		else put_str(buf, 2, 36);
		
		if (p_ptr->free_skpts) c_put_str(TERM_RED, "Skill Points:", 2, 47);
		else put_str("Skill Points:", 2, 47);
		sprintf(buf, "%2d", p_ptr->free_skpts);
		if (p_ptr->free_skpts > 10) c_put_str(TERM_VIOLET, buf, 2, 61);
		else if (p_ptr->free_skpts) c_put_str(TERM_RED, buf, 2, 61);
		else put_str(buf, 2, 61);
		

		/* Hack -- hide the cursor  XXX XXX */
		/* Term_gotoxy(0, 26); */


		/* Query */
		ch = inkey();

		/* Exit */
		if (ch == ESCAPE) break;
		
		if (ch == 'h')
		{
			clear_from(0);
			mode++;
			while (((p_ptr->skills[selected].skill_max == -2) ||
					(p_ptr->skills[selected].skill_type != mode))
					&& selected < N_SKILLS)
			{
		  		selected++;
			}
			if (p_ptr->skills[selected].skill_type != mode)
			{
				while (((p_ptr->skills[selected].skill_max == -2) ||
					(p_ptr->skills[selected].skill_type != mode))
					&& selected > 0)
				{
			  		selected--;
				}
			}
		}
		
		/* Gain stats */
		if (ch == 'g')
		{
			screen_save();
			/* if stat growth is available */
			if (p_ptr->free_sgain > 0)
			{
				/* Raise the stats */
				do_raise_stats();

			 	/* reduce the amount of stat gains by one */
			 	p_ptr->free_sgain -= 1;
			}
			/* Clear screen */
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
			while ((p_ptr->skills[selected].skill_index == 0) || 
					(p_ptr->skills[selected].skill_type != mode))
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
			while (((p_ptr->skills[selected].skill_max == -2) ||
					(p_ptr->skills[selected].skill_type != mode))
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
			if (train) skill_max_up(selected);
			else skill_up(selected); 

			/* Erase screen */
			clear_from(11);

			/* This function displays skills */
			print_all_skills(selected, mode, train);
		}
		if ((ch == '-') || (ch == '_'))
		{
			/* decrement skill - this doesn't ever actually happen */
		}

		/* This is the code from birth.txt to shoot to a helpfile for skills p_ptr->skills[selected].name*/
	  if (ch == '?')
		{
			strnfmt(buf, sizeof(buf), "%s#%d", "skills.txt", selected);

			screen_save();
			(void)show_file(buf, NULL, 0, 0);
			clear_from(0);
			screen_load();
		}

		else{}
		
		

	}
	/* Load screen */
	screen_load();	
	
	/* Redraw Level status */
	p_ptr->redraw |= (PR_LEVEL);

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);
	
	p_ptr->update |= (PU_HP);
	
	p_ptr->update |= (PU_MANA);

	
	/* Update stuff */
	update_stuff();


}	

