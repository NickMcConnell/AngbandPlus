/* File: learn.c */

/* Purpose: Allow player to learn(and use) monsters attacks! */
/* Also allow to learn abilities */
/* The new spellcasting system is built in this file! */
/* Items may also 'learn' abilities in this file! */

#include "angband.h"


/* Maximum number of tries for teleporting */
#define MAX_TRIES 300


int use_monster_power()
{
	int                     Power = -1;
        int                     num = 0, dir = 0 , i, j;

	int             powers[36];
	char            power_desc[36][80];

	bool            flag, redraw;
        int             ask, plev = p_ptr->lev;
        s32b dam;

        char            choice, ch;

	char            out_val[160];
        int k,count;
        int rlev = dun_level + p_ptr->lev + randint(5);
	int rad;
	int spellstat;
	monster_magics *mspell_ptr;

	/* This determines casting power */
	spellstat = (p_ptr->stat_ind[A_INT] - 5);

	/* No lower than 0. */
	if (spellstat < 0) spellstat = 0;

        /* List the powers */
	for (i = 0; i < 15; i++)
        {
                char powdesc[120];
                mspell_ptr = &monster_magic[i];
                if (mspell_ptr->type > 0)
                {
			if (mspell_ptr->type == 1)
			{
				sprintf(powdesc, "%s  Type: Bolt, %s  Pow: %d/int-5  Cost: %d", mspell_ptr->name, get_element_name(mspell_ptr->special1), mspell_ptr->power, mspell_ptr->cost);
				strcpy(power_desc[num],powdesc);
				powers[num++]=i;
			}
			else if (mspell_ptr->type == 2)
			{
				sprintf(powdesc, "%s  Type: Ball, %s  Pow: %d/int-5  Rad: %d  Cost: %d", mspell_ptr->name, get_element_name(mspell_ptr->special1), mspell_ptr->power, mspell_ptr->special2, mspell_ptr->cost);
				strcpy(power_desc[num],powdesc);
				powers[num++]=i;
			}
			else if (mspell_ptr->type == 3)
			{
				sprintf(powdesc, "%s  Type: Healing  Pow: %d/int-5  Cost: %d", mspell_ptr->name, mspell_ptr->power, mspell_ptr->cost);
				strcpy(power_desc[num],powdesc);
				powers[num++]=i;
			}
			else if (mspell_ptr->type == 4)
			{
				sprintf(powdesc, "%s  Type: Haste  Pow: %d  Cost: %d", mspell_ptr->name, mspell_ptr->power, mspell_ptr->cost);
				strcpy(power_desc[num],powdesc);
				powers[num++]=i;
			}
			else if (mspell_ptr->type == 5)
			{
				switch (mspell_ptr->special1)
				{
					case 1:
					case 2:
					case 3:
					case 6:
					{
						sprintf(powdesc, "%s  Type: Haste  Pow: %d  Cost: %d", mspell_ptr->name, mspell_ptr->power, mspell_ptr->cost);
						strcpy(power_desc[num],powdesc);
						powers[num++]=i;
						break;
					}
				
					default:
					{
						break; 
					}
				}
			}
			else if (mspell_ptr->type == 6)
			{
				sprintf(powdesc, "%s   Type: Summon  Pow: %d  Num: %d   Dur: %d   Cost: %d", mspell_ptr->name, mspell_ptr->power, mspell_ptr->special1, mspell_ptr->special2, mspell_ptr->cost);
				strcpy(power_desc[num],powdesc);
				powers[num++]=i;
			}
			else if (mspell_ptr->type == 7)
			{
				monster_race *rr_ptr = &r_info[mspell_ptr->power];
				cptr m_name = (r_name + rr_ptr->name);
				sprintf(powdesc, "%s   Type: Summon  Num: %d   Dur: %d  Cost: %d", mspell_ptr->name, mspell_ptr->special1, mspell_ptr->special2, mspell_ptr->cost);
				strcpy(power_desc[num],powdesc);
				powers[num++]=i;
			}
			else if (mspell_ptr->type == 8)
			{
				sprintf(powdesc, "%s  Type: Teleport  Pow: %d  Cost: %d", mspell_ptr->name, mspell_ptr->power, mspell_ptr->cost);
				strcpy(power_desc[num],powdesc);
				powers[num++]=i;
			}
                }
        }
        if(!num) {msg_print("You do not have any monster magics yet.");return 0;}

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt (accept all spells) */
	if (num <= 26)
	{
		/* Build a prompt (accept all spells) */
                strnfmt(out_val, 78, "(Powers %c-%c, *=List, ESC=exit) Use which monster magic? ",
			I2A(0), I2A(num - 1));
	}
	else
	{
                strnfmt(out_val, 78, "(Powers %c-%c, *=List, ESC=exit) Use which monster magic? ",
			I2A(0), '0' + num - 27);
	}

	/* Get a spell from the user */
	while (!flag && get_com(out_val, &choice))
	{
		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Show the list */
			if (!redraw)
			{
				byte y = 1, x = 0;
				int ctr = 0;
				char dummy[80];

				strcpy(dummy, "");

				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				Term_save();

				prt ("", y++, x);

				while (ctr < num && ctr < 17)
				{
					sprintf(dummy, "%c) %s", I2A(ctr), power_desc[ctr]);
					prt(dummy, y + ctr, x);
					ctr++;
				}
				while (ctr < num)
				{
					if (ctr < 26)
					{
						sprintf(dummy, " %c) %s", I2A(ctr), power_desc[ctr]);
					}
					else
					{
						sprintf(dummy, " %c) %s", '0' + ctr - 26, power_desc[ctr]);
					}
					prt(dummy, y + ctr - 17, x + 40);
					ctr++;
				}
				if (ctr < 17)
				{
					prt ("", y + ctr, x);
				}
				else
				{
					prt ("", y + 17, x);
				}
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

		if (choice == '\r' && num == 1)
		{
			choice = 'a';
		}

		if (isalpha(choice))
		{
			/* Note verify */
			ask = (isupper(choice));

			/* Lowercase */
			if (ask) choice = tolower(choice);

			/* Extract request */
			i = (islower(choice) ? A2I(choice) : -1);
		}
		else
		{
			ask = FALSE; /* Can't uppercase digits */

			i = choice - '0' + 26;
		}

		/* Totally Illegal */
		if ((i < 0) || (i >= num))
		{
			bell();
			continue;
		}

		/* Save the spell index */
		Power = powers[i];

		/* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Prompt */
			strnfmt(tmp_val, 78, "Use %s? ", power_desc[i]);

			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}

	/* Restore the screen */
	if (redraw) Term_load();

	/* Abort if needed */
	if (!flag) 
	{
		energy_use = 0;
                return num;
	}

	mspell_ptr = &monster_magic[Power];

	/* Before we cast... do we have enough mana? */
	if (p_ptr->csp < mspell_ptr->cost) 
	{
		msg_print("Not enough mana to use this monster magic.");
		return 0;
	}

	/* Actually cast the monster magic! */
	switch (mspell_ptr->type)
	{
		/* Bolt */
		case 1:
		{
			dam = mspell_ptr->power * spellstat;
			dam += multiply_divide(dam, (p_ptr->abilities[(CLASS_MONSTER_MAGE * 10)] * 50), 100);
			if(!get_aim_dir(&dir)) return;
			fire_bolt(mspell_ptr->special1, dir, dam);
			break;
		}
		/* Ball */
		case 2:
		{
			dam = mspell_ptr->power * spellstat;
			dam += multiply_divide(dam, (p_ptr->abilities[(CLASS_MONSTER_MAGE * 10)] * 50), 100);
			rad = mspell_ptr->special2;
			if(!get_aim_dir(&dir)) return;
			fire_ball(mspell_ptr->special1, dir, dam, rad);
			break;
		}
		/* Heal */
		case 3:
		{
			dam = mspell_ptr->power * spellstat;
			dam += multiply_divide(dam, (p_ptr->abilities[(CLASS_MONSTER_MAGE * 10)] * 50), 100);
			p_ptr->chp += dam;
			if (p_ptr->chp > p_ptr->mhp) p_ptr->chp = p_ptr->mhp;
			msg_print("You are healed!");
			update_and_handle();
			break;
		}
		/* Haste */
		case 4:
		{
			dam = mspell_ptr->power;
			(void)set_fast(dam);
			update_and_handle();
			break;
		}
		/* Boost */
		case 5:
		{
			dam = mspell_ptr->power;
			if (mspell_ptr->special1 == 1)
			{
				p_ptr->str_boost = mspell_ptr->power;
                                (void)set_str_boost(20); 
			}
			if (mspell_ptr->special1 == 2)
			{
				p_ptr->dex_boost = mspell_ptr->power;
                                (void)set_dex_boost(20); 
			}
			if (mspell_ptr->special1 == 3)
			{
				p_ptr->int_boost = mspell_ptr->power;
                                (void)set_int_boost(20); 
			}
			if ((mspell_ptr->special1 == 6) || (mspell_ptr->special1 == 8)) 
			{
				p_ptr->str_boost = mspell_ptr->power;
				p_ptr->dex_boost = mspell_ptr->power;
				p_ptr->int_boost = mspell_ptr->power;
				(void)set_str_boost(20);
				(void)set_dex_boost(20);
				(void)set_int_boost(20);
			}
			update_and_handle();
			break;
		}

		/* Summon Kind */
		case 6:
		{
			for (j = 0; j < mspell_ptr->special1; j++)
			{
				summon_specific_kind(py, px, mspell_ptr->power, mspell_ptr->summchar, FALSE, TRUE, mspell_ptr->special2);
			}
			break;
		}

		/* Summon Specific */
		case 7:
		{
			for (j = 0; j < mspell_ptr->special1; j++)
			{
				summon_specific_ridx(py, px, mspell_ptr->power, FALSE, TRUE, mspell_ptr->special2);
			}
			break;
		}
		/* Phase door */
		case 8:
		{
			teleport_player(mspell_ptr->power);
			break;
		}
		
		default:
		{
			break;
		}
	}	

	/* Remove some mana */
	p_ptr->csp -= mspell_ptr->cost;

	update_and_handle();

        energy_use = 100;
        return num;
}

/* Allow player to learn an ability */
/* Completely rewritten for NewAngband 1.7.0 */
void learn_ability()
{

	char            choice;
        char            abils[10][50];
        char            tmpstr[50];

        int            selecteditem = 0;
        int            x = 0;
        int            i, j;
	int	mode;

        bool           learning = TRUE;
	s16b oldabilities[MAX_ABILITIES];
	s16b old_num_abilities = p_ptr->num_abilities;
	s32b old_ability_points = p_ptr->ability_points;
	s16b old_abilities_powers[36];
	s16b old_abilities_monster_attacks[20];
	s16b old_abilities_monster_spells[20];
	bool learned[6];

        if (p_ptr->pclass == CLASS_APPRENTICE)
        {
                msg_print("This class doesn't have any abilities!");
                return;
        }

	/* Save some old stuff. */
	for (i = 0; i < MAX_ABILITIES; i++) oldabilities[i] = p_ptr->abilities[i];
	for (i = 0; i < 20; i++)
	{
		old_abilities_monster_attacks[i] = p_ptr->abilities_monster_attacks[i];
		old_abilities_monster_spells[i] = p_ptr->abilities_monster_spells[i];
	}
	for (i = 0; i < 36; i++) old_abilities_powers[i] = p_ptr->abilities_powers[i];

	for (i = 0; i < 6; i++) learned[i] = FALSE;
	if (p_ptr->boss_abilities & BOSS_HALVE_DAMAGES) learned[0] = TRUE;
	if (p_ptr->boss_abilities & BOSS_DOUBLE_DAMAGES) learned[1] = TRUE;
	if (p_ptr->boss_abilities & BOSS_DOUBLE_MAGIC) learned[2] = TRUE;
	if (p_ptr->boss_abilities & BOSS_CURSED_HITS) learned[3] = TRUE;
	if (p_ptr->boss_abilities & BOSS_RETURNING) learned[4] = TRUE;
	if (p_ptr->boss_abilities & BOSS_MAGIC_RETURNING) learned[5] = TRUE;

        /* Save what's on screen */
        Term_save();

	/* Flush messages */
	msg_print(NULL);

        /* Begin... */
	Term_erase(0, 1, 255);

	/* Start with mode 0. */
	mode = 0;

        while (learning == TRUE)
        {
		/* Mode 0 is standard abilities interface. */
		if (mode == 0)
		{
        		/* Prepare the screen */
        		for (i = 0; i < SCREEN_HGT; i++)
        		{
                		roff("\n");
        		}
			
        		c_put_str(TERM_WHITE, "ABILITIES                                                                 ", 0, 0);
			c_put_str(TERM_WHITE, "-----------", 1, 0);

			/* As of Portralis, this part has changed! :) */
			sprintf(abils[0], abilities_def[(p_ptr->pclass * 10)].name);
        		sprintf(abils[1], abilities_def[(p_ptr->pclass * 10) + 1].name);
        		sprintf(abils[2], abilities_def[(p_ptr->pclass * 10) + 2].name);
        		sprintf(abils[3], abilities_def[(p_ptr->pclass * 10) + 3].name);
        		sprintf(abils[4], abilities_def[(p_ptr->pclass * 10) + 4].name);
        		sprintf(abils[5], abilities_def[(p_ptr->pclass * 10) + 5].name);
        		sprintf(abils[6], abilities_def[(p_ptr->pclass * 10) + 6].name);
        		sprintf(abils[7], abilities_def[(p_ptr->pclass * 10) + 7].name);
        		sprintf(abils[8], abilities_def[(p_ptr->pclass * 10) + 8].name);
        		sprintf(abils[9], abilities_def[(p_ptr->pclass * 10) + 9].name);

        		for (x = 0; x < 10; x++)
        		{
        			int abilnum = (p_ptr->pclass * 10) + x;
                		int abilitynumber = x + 1;                
                		char abil[80];

                		if (abilitynumber > 9) abilitynumber = 0;
                		sprintf (abil, "%d.  %s [%d]", abilitynumber, abils[x], p_ptr->abilities[abilnum]);
                		if (p_ptr->abilities[abilnum] <= 0) c_put_str(TERM_L_DARK, abil, (3 + x), 0);
                		else c_put_str(TERM_WHITE, abil, (3 + x), 0);
        		}
        

        		sprintf(tmpstr, "%ld  ", p_ptr->ability_points);
        		c_put_str(TERM_WHITE, "Remaining AP: ", 15, 0);
        		c_put_str(TERM_L_GREEN, tmpstr, 15, 14);
        		c_put_str(TERM_WHITE, "Choose an ability to increase, 'u' to undo, ESC to exit.", 17, 0);
			if (p_ptr->prace == RACE_MONSTER) c_put_str(TERM_WHITE, "Press 'v' to invest in one of your special powers.", 18, 0);
			if (p_ptr->prace == RACE_MONSTER && p_ptr->abilities[(CLASS_MONSTER * 10) + 9] >= 10) c_put_str(TERM_WHITE, "Press 'e' to gain an elite ability.", 19, 0);

        		choice = inkey();
        		if (choice == '1')
        		{
                		int abilnum = (p_ptr->pclass * 10);
                		if (p_ptr->ability_points > 0)
                		{
                        		if (p_ptr->abilities[abilnum] <= 0 && p_ptr->num_abilities >= 30) msg_print("You may not learn any new abilities.");
                        		else
                        		{
                                		if (p_ptr->abilities[abilnum] <= 0)
						{
							p_ptr->num_abilities += 1;
							/* If it's an active ability, add it to our choices! */
							if (abilities_def[abilnum].abtype == 1)
							{
								j = 0;
								while (p_ptr->abilities_powers[j] != 0) j += 1;
								p_ptr->abilities_powers[j] = abilnum;
							}
						}
						if (p_ptr->abilities[abilnum] < 100)
						{
                                			p_ptr->abilities[abilnum] += 1;
                                			p_ptr->ability_points -= 1;
						}
						else msg_print("You may not raise an ability higher than 100.");
                        		}
                		}
        		}
        		if (choice == '2')
        		{
                		int abilnum = (p_ptr->pclass * 10) + 1;
                		if (p_ptr->ability_points > 0)
                		{
                        		if (p_ptr->abilities[abilnum] <= 0 && p_ptr->num_abilities >= 30) msg_print("You may not learn any new abilities.");
                        		else
                        		{
                                		if (p_ptr->abilities[abilnum] <= 0)
						{
							p_ptr->num_abilities += 1;
							/* If it's an active ability, add it to our choices! */
							if (abilities_def[abilnum].abtype == 1)
							{
								j = 0;
								while (p_ptr->abilities_powers[j] != 0) j += 1;
								p_ptr->abilities_powers[j] = abilnum;
							}
						}
                                		if (p_ptr->abilities[abilnum] < 100)
						{
                                			p_ptr->abilities[abilnum] += 1;
                                			p_ptr->ability_points -= 1;
						}
						else msg_print("You may not raise an ability higher than 100.");
                        		}
                		}
        		}
        		if (choice == '3')
        		{
                		int abilnum = (p_ptr->pclass * 10) + 2;
                		if (p_ptr->ability_points > 0)
                		{
                        		if (p_ptr->abilities[abilnum] <= 0 && p_ptr->num_abilities >= 30) msg_print("You may not learn any new abilities.");
                        		else
                        		{
                                		if (p_ptr->abilities[abilnum] <= 0)
						{
							p_ptr->num_abilities += 1;
							/* If it's an active ability, add it to our choices! */
							if (abilities_def[abilnum].abtype == 1)
							{
								j = 0;
								while (p_ptr->abilities_powers[j] != 0) j += 1;
								p_ptr->abilities_powers[j] = abilnum;
							}
						}
                                		if (p_ptr->abilities[abilnum] < 100)
						{
                                			p_ptr->abilities[abilnum] += 1;
                                			p_ptr->ability_points -= 1;
						}
						else msg_print("You may not raise an ability higher than 100.");
                        		}
                		}
        		}
        		if (choice == '4')
        		{
                		int abilnum = (p_ptr->pclass * 10) + 3;
                		if (p_ptr->ability_points > 0)
                		{
                        		if (p_ptr->abilities[abilnum] <= 0 && p_ptr->num_abilities >= 30) msg_print("You may not learn any new abilities.");
                        		else
                        		{
                                		if (p_ptr->abilities[abilnum] <= 0)
						{
							p_ptr->num_abilities += 1;
							/* If it's an active ability, add it to our choices! */
							if (abilities_def[abilnum].abtype == 1)
							{
								j = 0;
								while (p_ptr->abilities_powers[j] != 0) j += 1;
								p_ptr->abilities_powers[j] = abilnum;
							}
						}
                                		if (p_ptr->abilities[abilnum] < 100)
						{
                                			p_ptr->abilities[abilnum] += 1;
                                			p_ptr->ability_points -= 1;
						}
						else msg_print("You may not raise an ability higher than 100.");
                        		}
                		}
        		}
        		if (choice == '5')
        		{                
                		int abilnum = (p_ptr->pclass * 10) + 4;
                		if (p_ptr->ability_points > 0)
                		{
                        		if (p_ptr->abilities[abilnum] <= 0 && p_ptr->num_abilities >= 30) msg_print("You may not learn any new abilities.");
                        		else
                        		{
                                		if (p_ptr->abilities[abilnum] <= 0)
						{
							p_ptr->num_abilities += 1;
							/* If it's an active ability, add it to our choices! */
							if (abilities_def[abilnum].abtype == 1)
							{
								j = 0;
								while (p_ptr->abilities_powers[j] != 0) j += 1;
								p_ptr->abilities_powers[j] = abilnum;
							}
						}
                                		if (p_ptr->abilities[abilnum] < 100)
						{
                                			p_ptr->abilities[abilnum] += 1;
                                			p_ptr->ability_points -= 1;
						}
						else msg_print("You may not raise an ability higher than 100.");
                        		}
                		}
        		}
        		if (choice == '6')
        		{                
                		int abilnum = (p_ptr->pclass * 10) + 5;
                		if (p_ptr->ability_points > 0)
                		{
                        		if (p_ptr->abilities[abilnum] <= 0 && p_ptr->num_abilities >= 30) msg_print("You may not learn any new abilities.");
                        		else
                        		{
                                		if (p_ptr->abilities[abilnum] <= 0)
						{
							p_ptr->num_abilities += 1;
							/* If it's an active ability, add it to our choices! */
							if (abilities_def[abilnum].abtype == 1)
							{
								j = 0;
								while (p_ptr->abilities_powers[j] != 0) j += 1;
								p_ptr->abilities_powers[j] = abilnum;
							}
						}
                                		if (p_ptr->abilities[abilnum] < 100)
						{
                                			p_ptr->abilities[abilnum] += 1;
                                			p_ptr->ability_points -= 1;
						}
						else msg_print("You may not raise an ability higher than 100.");
                        		}
                		}
        		}
        		if (choice == '7')
        		{
                		if (p_ptr->class_level[p_ptr->pclass] >= 6)
                		{
                        		int abilnum = (p_ptr->pclass * 10) + 6;
                        		if (p_ptr->ability_points > 0)
                        		{
                                		if (p_ptr->abilities[abilnum] <= 0 && p_ptr->num_abilities >= 30) msg_print("You may not learn any new abilities.");
                                		else
                                		{
                                        		if (p_ptr->abilities[abilnum] <= 0)
							{
								p_ptr->num_abilities += 1;
								/* If it's an active ability, add it to our choices! */
								if (abilities_def[abilnum].abtype == 1)
								{
									j = 0;
									while (p_ptr->abilities_powers[j] != 0) j += 1;
									p_ptr->abilities_powers[j] = abilnum;
								}
							}
                                        		if (p_ptr->abilities[abilnum] < 100)
							{
                                				p_ptr->abilities[abilnum] += 1;
                                				p_ptr->ability_points -= 1;
							}
							else msg_print("You may not raise an ability higher than 100.");
                                		}
                        		}
                		}
                		else {
                        		msg_print("You must first reach class level 6!");
                        		inkey();
                		}
        		}
        		if (choice == '8')
        		{
                		if (p_ptr->class_level[p_ptr->pclass] >= 6)
                		{
                        		int abilnum = (p_ptr->pclass * 10) + 7;
                        		if (p_ptr->ability_points > 0)
                        		{
                                		if (p_ptr->abilities[abilnum] <= 0 && p_ptr->num_abilities >= 30) msg_print("You may not learn any new abilities.");
                                		else
                                		{
                                        		if (p_ptr->abilities[abilnum] <= 0)
							{
								p_ptr->num_abilities += 1;
								/* If it's an active ability, add it to our choices! */
								if (abilities_def[abilnum].abtype == 1)
								{
									j = 0;
									while (p_ptr->abilities_powers[j] != 0) j += 1;
									p_ptr->abilities_powers[j] = abilnum;
								}
							}
                                        		if (p_ptr->abilities[abilnum] < 100)
							{
                                				p_ptr->abilities[abilnum] += 1;
                                				p_ptr->ability_points -= 1;
							}
							else msg_print("You may not raise an ability higher than 100.");
                                		}
                        		}
                		}
                		else {
                        		msg_print("You must first reach class level 6!");
                        		inkey();
                		}
        		}
        		if (choice == '9')
        		{
                		if (p_ptr->class_level[p_ptr->pclass] >= 6)
                		{
                        		int abilnum = (p_ptr->pclass * 10) + 8;
                        		if (p_ptr->ability_points > 0)
                        		{
                                		if (p_ptr->abilities[abilnum] <= 0 && p_ptr->num_abilities >= 30) msg_print("You may not learn any new abilities.");
                                		else
                                		{
                                        		if (p_ptr->abilities[abilnum] <= 0)
							{
								p_ptr->num_abilities += 1;
								/* If it's an active ability, add it to our choices! */
								if (abilities_def[abilnum].abtype == 1)
								{
									j = 0;
									while (p_ptr->abilities_powers[j] != 0) j += 1;
									p_ptr->abilities_powers[j] = abilnum;
								}
							}
                                        		if (p_ptr->abilities[abilnum] < 100)
							{
                                				p_ptr->abilities[abilnum] += 1;
                                				p_ptr->ability_points -= 1;
							}
							else msg_print("You may not raise an ability higher than 100.");
                                		}
                        		}
                		}
                		else {
                        		msg_print("You must first reach class level 6!");
                        		inkey();
                		}
        		}
        		if (choice == '0')
        		{
                		if (p_ptr->class_level[p_ptr->pclass] >= 10)
                		{
                        		int abilnum = (p_ptr->pclass * 10) + 9;
                        		if (p_ptr->ability_points > 0)
                        		{
                                		if (p_ptr->abilities[abilnum] <= 0 && p_ptr->num_abilities >= 30) msg_print("You may not learn any new abilities.");
                                		else
                                		{
                                        		if (p_ptr->abilities[abilnum] <= 0)
							{
								p_ptr->num_abilities += 1;
								/* If it's an active ability, add it to our choices! */
								if (abilities_def[abilnum].abtype == 1)
								{
									j = 0;
									while (p_ptr->abilities_powers[j] != 0) j += 1;
									p_ptr->abilities_powers[j] = abilnum;
								}
							}
                                        		if (p_ptr->abilities[abilnum] < 100)
							{
                                				p_ptr->abilities[abilnum] += 1;
                                				p_ptr->ability_points -= 1;
							}
							else msg_print("You may not raise an ability higher than 100.");
                                		}
                        		}
                		}
                		else {
                        		msg_print("You must first reach class level 10!");
                        		inkey();
                		}
        		}
			if (choice == 'u' || choice == 'U')
        		{
                		for (i = 0; i < MAX_ABILITIES; i++) p_ptr->abilities[i] = oldabilities[i];
				for (i = 0; i < 36; i++) p_ptr->abilities_powers[i] = old_abilities_powers[i];
				for (i = 0; i < 20; i++)
				{
					p_ptr->abilities_monster_attacks[i] = old_abilities_monster_attacks[i];
					p_ptr->abilities_monster_spells[i] = old_abilities_monster_spells[i];
				}

				p_ptr->num_abilities = old_num_abilities;
				p_ptr->ability_points = old_ability_points;
				if (learned[0] == FALSE) p_ptr->boss_abilities &= BOSS_HALVE_DAMAGES;
				if (learned[1] == FALSE) p_ptr->boss_abilities &= BOSS_DOUBLE_DAMAGES;
				if (learned[2] == FALSE) p_ptr->boss_abilities &= BOSS_DOUBLE_MAGIC;
				if (learned[3] == FALSE) p_ptr->boss_abilities &= BOSS_CURSED_HITS;
				if (learned[4] == FALSE) p_ptr->boss_abilities &= BOSS_RETURNING;
				if (learned[5] == FALSE) p_ptr->boss_abilities &= BOSS_MAGIC_RETURNING;
        		}
			if (choice == 'v' || choice == 'V')
        		{
                		mode = 1;
				Term_clear();
        		}
			if (choice == 'e' || choice == 'E')
        		{
                		mode = 2;
				Term_clear();
        		}
        		if (choice == ESCAPE)
        		{
                		learning = FALSE;
        		}

        	}
		/* Mode 1 is attacks and spells for monster race. */
		if (mode == 1)
		{
			char choicechar;
			monster_race *r_ptr = &r_info[p_ptr->body_monster];

        		/* Prepare the screen */
        		for (i = 0; i < SCREEN_HGT; i++)
        		{
                		roff("\n");
        		}
        		c_put_str(TERM_WHITE, "ATTACKS AND POWERS                                                        ", 0, 0);
			c_put_str(TERM_WHITE, "--------------------", 1, 0);

			choicechar = 'a';
        		for (x = 0; x < 20; x++)
        		{               
				char aname[160];
				char abil[160];

				if (r_ptr->attack[x].type != 0 && r_ptr->attack[x].type != 2)
				{
					if (r_ptr->attack[x].name[0] != '!') sprintf(aname, "%s", r_ptr->attack[x].name);
					else
					{
						char aact[80];
						sprintf(aact, "%s", r_ptr->attack[x].act);
						aact[0] = toupper(aact[0]);
						sprintf(aname, "%s %s", get_element_name(r_ptr->attack[x].element), aact);
					}

                			sprintf (abil, "%c.  %s [%d]  ", choicechar, aname, p_ptr->abilities_monster_attacks[x]);
                			if (p_ptr->abilities_monster_attacks[x] <= 0) c_put_str(TERM_L_DARK, abil, (2 + x), 0);
                			else c_put_str(TERM_WHITE, abil, (2 + x), 0);

					choicechar++;
				}
        		}

			choicechar = 'A';
        		for (x = 0; x < 20; x++)
        		{               
				char aname[160];
				char abil[160];

				if (r_ptr->spell[x].type == 999)
				{
					char *powname;

					/* Calls lua! */
					call_lua("get_scripted_spell_name", "(dd)", "s", p_ptr->body_monster, x+1, &powname);

                			sprintf (abil, "%c.  %s [%d]  ", choicechar, powname, p_ptr->abilities_monster_spells[x]);
                			if (p_ptr->abilities_monster_spells[x] <= 0) c_put_str(TERM_L_DARK, abil, (2 + x), 33);
                			else c_put_str(TERM_WHITE, abil, (2 + x), 33);

					choicechar++;
				}
				else if (r_ptr->spell[x].type != 0)
				{
                			sprintf (abil, "%c.  %s [%d]  ", choicechar, r_ptr->spell[x].name, p_ptr->abilities_monster_spells[x]);
                			if (p_ptr->abilities_monster_spells[x] <= 0) c_put_str(TERM_L_DARK, abil, (2 + x), 33);
                			else c_put_str(TERM_WHITE, abil, (2 + x), 33);

					choicechar++;
				}
        		}
        

        		sprintf(tmpstr, "%ld  ", p_ptr->ability_points);
        		c_put_str(TERM_WHITE, "Remaining AP: ", 22, 0);
        		c_put_str(TERM_L_GREEN, tmpstr, 22, 14);
        		c_put_str(TERM_WHITE, "Choose a power, 'u' to undo, 'v' for abilities, ESC to exit.", 23, 0);

        		choice = inkey();

			if (choice >= 'a' && choice <= 't')
			{
				int whichone;

				whichone = A2I(choice);

				if (p_ptr->ability_points > 0 && r_ptr->attack[whichone].type > 0)
				{
					if (p_ptr->abilities_monster_attacks[whichone] < 100)
					{
						p_ptr->abilities_monster_attacks[whichone] += 1;
                                		p_ptr->ability_points -= 1;
					}
					else msg_print("You may not raise an ability higher than 100.");
				}
			}

			if (choice >= 'A' && choice <= 'T')
			{
				int whichone;

				whichone = A2I(choice) + 32;

				if (p_ptr->ability_points > 0 && r_ptr->spell[whichone].type > 0)
				{
					if (p_ptr->abilities_monster_spells[whichone] < 100)
					{
						p_ptr->abilities_monster_spells[whichone] += 1;
                                		p_ptr->ability_points -= 1;
					}
					else msg_print("You may not raise an ability higher than 100.");
				}
			}
        		
			if (choice == 'u' || choice == 'U')
        		{
                		for (i = 0; i < MAX_ABILITIES; i++) p_ptr->abilities[i] = oldabilities[i];
				for (i = 0; i < 36; i++) p_ptr->abilities_powers[i] = old_abilities_powers[i];
				for (i = 0; i < 20; i++)
				{
					p_ptr->abilities_monster_attacks[i] = old_abilities_monster_attacks[i];
					p_ptr->abilities_monster_spells[i] = old_abilities_monster_spells[i];
				}

				p_ptr->num_abilities = old_num_abilities;
				p_ptr->ability_points = old_ability_points;
				if (learned[0] == FALSE) p_ptr->boss_abilities &= BOSS_HALVE_DAMAGES;
				if (learned[1] == FALSE) p_ptr->boss_abilities &= BOSS_DOUBLE_DAMAGES;
				if (learned[2] == FALSE) p_ptr->boss_abilities &= BOSS_DOUBLE_MAGIC;
				if (learned[3] == FALSE) p_ptr->boss_abilities &= BOSS_CURSED_HITS;
				if (learned[4] == FALSE) p_ptr->boss_abilities &= BOSS_RETURNING;
				if (learned[5] == FALSE) p_ptr->boss_abilities &= BOSS_MAGIC_RETURNING;
        		}
			if (choice == 'v' || choice == 'V')
        		{
                		mode = 0;
				Term_clear();
        		}
        		if (choice == ESCAPE)
        		{
                		learning = FALSE;
        		}

        	}
		/* Mode 2 is elite abilities. */
		if (mode == 2)
		{
			char choicechar;
			int abp = 0;
			char abil[160];
			monster_race *r_ptr = &r_info[p_ptr->body_monster];

        		/* Prepare the screen */
        		for (i = 0; i < SCREEN_HGT; i++)
        		{
                		roff("\n");
        		}
        		c_put_str(TERM_WHITE, "ELITE ABILITIES                                                     ", 0, 0);
			c_put_str(TERM_WHITE, "-----------------", 1, 0);

			sprintf (abil, "1. Halve Damages (reduce all damages by 50%)");
                	if (!(p_ptr->boss_abilities & BOSS_HALVE_DAMAGES)) c_put_str(TERM_L_DARK, abil, 2, 0);
                	else c_put_str(TERM_WHITE, abil, 2, 0);

			sprintf (abil, "2. Double Weapon Damages (+100% damages to melee and ranged attacks)");
                	if (!(p_ptr->boss_abilities & BOSS_DOUBLE_DAMAGES)) c_put_str(TERM_L_DARK, abil, 3, 0);
                	else c_put_str(TERM_WHITE, abil, 3, 0);

			sprintf (abil, "3. Double Magic Damages (+100% damages to magic attacks)");
                	if (!(p_ptr->boss_abilities & BOSS_DOUBLE_MAGIC)) c_put_str(TERM_L_DARK, abil, 4, 0);
                	else c_put_str(TERM_WHITE, abil, 4, 0);

			/*sprintf (abil, "4. Crushing Blows (melee attacks may paralyze, confuse and reduce hit rate)");
                	if (!(p_ptr->boss_abilities & BOSS_CURSED_HITS)) c_put_str(TERM_L_DARK, abil, 5, 0);
                	else c_put_str(TERM_WHITE, abil, 5, 0);*/

			sprintf (abil, "4. Return 50% Melee Damages (return 50% of melee damages to enemies)");
                	if (!(p_ptr->boss_abilities & BOSS_RETURNING)) c_put_str(TERM_L_DARK, abil, 6, 0);
                	else c_put_str(TERM_WHITE, abil, 6, 0);

			sprintf (abil, "5. Return 50% Magic Damages (return 50% of magic damages to enemies)");
                	if (!(p_ptr->boss_abilities & BOSS_MAGIC_RETURNING)) c_put_str(TERM_L_DARK, abil, 7, 0);
                	else c_put_str(TERM_WHITE, abil, 7, 0);

			abp = p_ptr->abilities[(CLASS_MONSTER * 10) + 9] / 10;
			if (abp > 5) abp = 5;
			if (p_ptr->boss_abilities & BOSS_HALVE_DAMAGES) abp -= 1;
			if (p_ptr->boss_abilities & BOSS_DOUBLE_DAMAGES) abp -= 1;
			if (p_ptr->boss_abilities & BOSS_DOUBLE_MAGIC) abp -= 1;
			/*if (p_ptr->boss_abilities & BOSS_CURSED_HITS) abp -= 1;*/
			if (p_ptr->boss_abilities & BOSS_RETURNING) abp -= 1;
			if (p_ptr->boss_abilities & BOSS_MAGIC_RETURNING) abp -= 1;

        		sprintf(tmpstr, "%ld  ", abp);
        		c_put_str(TERM_WHITE, "Remaining: ", 15, 0);
        		c_put_str(TERM_L_GREEN, tmpstr, 15, 11);
        		c_put_str(TERM_WHITE, "Choose a power, 'u' to undo, 'v' for abilities, ESC to exit.", 16, 0);

        		choice = inkey();

			if (choice == '1')
			{
				if (abp > 0 && !(p_ptr->boss_abilities & BOSS_HALVE_DAMAGES))
				{
					abp -= 1;
					p_ptr->boss_abilities |= BOSS_HALVE_DAMAGES;
				}
			}
			if (choice == '2')
			{
				if (abp > 0 && !(p_ptr->boss_abilities & BOSS_DOUBLE_DAMAGES))
				{
					abp -= 1;
					p_ptr->boss_abilities |= BOSS_DOUBLE_DAMAGES;
				}
			}
			if (choice == '3')
			{
				if (abp > 0 && !(p_ptr->boss_abilities & BOSS_DOUBLE_MAGIC))
				{
					abp -= 1;
					p_ptr->boss_abilities |= BOSS_DOUBLE_MAGIC;
				}
			}
			/*if (choice == '4')
			{
				if (abp > 0 && !(p_ptr->boss_abilities & BOSS_CURSED_HITS))
				{
					abp -= 1;
					p_ptr->boss_abilities |= BOSS_CURSED_HITS;
				}
			}*/
			if (choice == '4')
			{
				if (abp > 0 && !(p_ptr->boss_abilities & BOSS_RETURNING))
				{
					abp -= 1;
					p_ptr->boss_abilities |= BOSS_RETURNING;
				}
			}
			if (choice == '5')
			{
				if (abp > 0 && !(p_ptr->boss_abilities & BOSS_MAGIC_RETURNING))
				{
					abp -= 1;
					p_ptr->boss_abilities |= BOSS_MAGIC_RETURNING;
				}
			}
        		
			if (choice == 'u' || choice == 'U')
        		{
                		for (i = 0; i < MAX_ABILITIES; i++) p_ptr->abilities[i] = oldabilities[i];
				for (i = 0; i < 36; i++) p_ptr->abilities_powers[i] = old_abilities_powers[i];
				for (i = 0; i < 20; i++)
				{
					p_ptr->abilities_monster_attacks[i] = old_abilities_monster_attacks[i];
					p_ptr->abilities_monster_spells[i] = old_abilities_monster_spells[i];
				}

				p_ptr->num_abilities = old_num_abilities;
				p_ptr->ability_points = old_ability_points;
				if (learned[0] == FALSE) p_ptr->boss_abilities &= BOSS_HALVE_DAMAGES;
				if (learned[1] == FALSE) p_ptr->boss_abilities &= BOSS_DOUBLE_DAMAGES;
				if (learned[2] == FALSE) p_ptr->boss_abilities &= BOSS_DOUBLE_MAGIC;
				if (learned[3] == FALSE) p_ptr->boss_abilities &= BOSS_CURSED_HITS;
				if (learned[4] == FALSE) p_ptr->boss_abilities &= BOSS_RETURNING;
				if (learned[5] == FALSE) p_ptr->boss_abilities &= BOSS_MAGIC_RETURNING;
        		}
			if (choice == 'v' || choice == 'V')
        		{
                		mode = 0;
				Term_clear();
        		}
        		if (choice == ESCAPE)
        		{
                		learning = FALSE;
        		}

        	}
	}
        Term_load();

	/* If we raised the Monster's Inner Elemental Mastery, choose an element. */
	if (p_ptr->abilities[(CLASS_MONSTER * 10) + 3] >= 1 && (p_ptr->elemlord == 0)) pick_monster_element();

        update_and_handle();
}

/* Since ability(x) is a very useful function, here's a similar one for Rings! */
bool ring(int rsval)
{
        object_type *o_ptr;
        object_type *j_ptr;

        o_ptr = &inventory[INVEN_RING];
        j_ptr = &inventory[INVEN_RING+1];

        if (o_ptr->sval == rsval || j_ptr->sval == rsval) return TRUE;
        else return FALSE;
}

/* Since ability(x) is a very useful function, here's a similar one for daggers! */
bool dagger_check()
{
        object_type *o_ptr;

        o_ptr = &inventory[INVEN_WIELD];
        if (!(o_ptr->tval == TV_WEAPON && o_ptr->itemskill == 15)) 
	{
		o_ptr = &inventory[INVEN_WIELD+1];
		if (o_ptr->tval == TV_WEAPON && o_ptr->itemskill == 15) return (TRUE);
	}
	else return (TRUE);

        return (FALSE);
}

bool axe_check()
{
        object_type *o_ptr;

        o_ptr = &inventory[INVEN_WIELD];
        if (!(o_ptr->tval == TV_WEAPON && o_ptr->itemskill == 16)) 
	{
		o_ptr = &inventory[INVEN_WIELD+1];
		if (o_ptr->tval == TV_WEAPON && o_ptr->itemskill == 16) return (TRUE);
	}
	else return (TRUE);

        return (FALSE);
}

bool is_weapon(object_type *o_ptr)
{
        if (o_ptr->tval == TV_WEAPON || o_ptr->tval == TV_ROD) return (TRUE);

        return (FALSE);
}
bool is_ammo(object_type *o_ptr)
{
        if (o_ptr->tval == TV_AMMO) return (TRUE);

        return (FALSE);
}

/* The tweak system has been rewritten for Portralis 0.3. */
void add_item_ability(object_type *o_ptr, bool crafting)
{
	int i;
	int spage = 0;
	int mode = 0;

	bool tweaking = TRUE;

	char query;
	char c;
	char tmp[80];

	/* Save what's on screen */
        Term_save();

	/* Flush messages */
	msg_print(NULL);

        /* Begin... */
	Term_erase(0, 1, 255);

	while (tweaking)
	{

		/* Mode 0: Main Menu */
		if (mode == 0)
		{

        		/* Prepare the screen */
			Term_erase(0, 1, 255);
        		for (i = 0; i < SCREEN_HGT; i++)
        		{
                		roff("\n");
        		}
        		c_put_str(TERM_WHITE, "PORTRALIS ITEM POWER UP", 0, 0);
        		c_put_str(TERM_WHITE, "-------------------------", 1, 0);

			c_put_str(TERM_WHITE, "Select a bonus to increase.", 4, 0);

			c_put_str(TERM_WHITE, "a) Stats bonus.", 6, 0);
			c_put_str(TERM_WHITE, "b) Skills bonus.", 7, 0);
			c_put_str(TERM_WHITE, "c) Attacks/Shots/Speed/Reflection.", 8, 0);
			c_put_str(TERM_WHITE, "d) Mana/Spells bonus.", 9, 0);
			c_put_str(TERM_WHITE, "e) Misc. Abilities.", 10, 0);

			
			sprintf(tmp, "%d", o_ptr->tweakpoints);
			c_put_str(TERM_WHITE, "Bonus points: ", 18, 0);
			c_put_str(TERM_L_GREEN, tmp, 18, 14);

			c_put_str(TERM_WHITE, "Make a choice. Press ESC to exit.", 20, 0);

			c = inkey();

			if (c == 'a' || c == 'A') mode = 1;
			if (c == 'b' || c == 'B') mode = 2;
			if (c == 'c' || c == 'C') mode = 3;
			if (c == 'd' || c == 'D') mode = 4;
			if (c == 'e' || c == 'E') mode = 5;

			if (c == ESCAPE) tweaking = FALSE;			
		}

		/* Mode 1: Stats menu */
		if (mode == 1)
		{

        		/* Prepare the screen */
			Term_erase(0, 1, 255);
        		for (i = 0; i < SCREEN_HGT; i++)
        		{
                		roff("\n");
        		}
        		c_put_str(TERM_WHITE, "PORTRALIS ITEM POWER UP", 0, 0);
        		c_put_str(TERM_WHITE, "-------------------------", 1, 0);

			c_put_str(TERM_WHITE, "Choose a stat to increase(costs 1 point per increase).", 4, 0);

			c_put_str(TERM_WHITE, "a) Strength     :", 6, 0);
			c_put_str(TERM_WHITE, "b) Intelligence :", 7, 0);
			c_put_str(TERM_WHITE, "c) Wisdom       :", 8, 0);
			c_put_str(TERM_WHITE, "d) Dexterity    :", 9, 0);
			c_put_str(TERM_WHITE, "e) Constitution :", 10, 0);
			c_put_str(TERM_WHITE, "f) Charisma     :", 11, 0);
			
			sprintf(tmp, "%d", o_ptr->statsbonus[A_STR]);
			c_put_str(TERM_L_GREEN, tmp, 6, 18);
			sprintf(tmp, "%d", o_ptr->statsbonus[A_INT]);
			c_put_str(TERM_L_GREEN, tmp, 7, 18);
			sprintf(tmp, "%d", o_ptr->statsbonus[A_WIS]);
			c_put_str(TERM_L_GREEN, tmp, 8, 18);
			sprintf(tmp, "%d", o_ptr->statsbonus[A_DEX]);
			c_put_str(TERM_L_GREEN, tmp, 9, 18);
			sprintf(tmp, "%d", o_ptr->statsbonus[A_CON]);
			c_put_str(TERM_L_GREEN, tmp, 10, 18);
			sprintf(tmp, "%d", o_ptr->statsbonus[A_CHR]);
			c_put_str(TERM_L_GREEN, tmp, 11, 18);

			sprintf(tmp, "%d", o_ptr->tweakpoints);
			c_put_str(TERM_WHITE, "Bonus points: ", 18, 0);
			c_put_str(TERM_L_GREEN, tmp, 18, 14);

			c_put_str(TERM_WHITE, "Make a choice. Press ESC to return to main menu.", 20, 0);

			c = inkey();

			if (c == 'a' || c == 'A')
			{
				if (o_ptr->tweakpoints >= 1)
				{
					o_ptr->statsbonus[A_STR] += 1;
					o_ptr->tweakpoints -= 1;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}
			if (c == 'b' || c == 'B')
			{
				if (o_ptr->tweakpoints >= 1)
				{
					o_ptr->statsbonus[A_INT] += 1;
					o_ptr->tweakpoints -= 1;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}
			if (c == 'c' || c == 'C')
			{
				if (o_ptr->tweakpoints >= 1)
				{
					o_ptr->statsbonus[A_WIS] += 1;
					o_ptr->tweakpoints -= 1;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}
			if (c == 'd' || c == 'D')
			{
				if (o_ptr->tweakpoints >= 1)
				{
					o_ptr->statsbonus[A_DEX] += 1;
					o_ptr->tweakpoints -= 1;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}
			if (c == 'e' || c == 'E')
			{
				if (o_ptr->tweakpoints >= 1)
				{
					o_ptr->statsbonus[A_CON] += 1;
					o_ptr->tweakpoints -= 1;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}
			if (c == 'f' || c == 'F')
			{
				if (o_ptr->tweakpoints >= 1)
				{
					o_ptr->statsbonus[A_CHR] += 1;
					o_ptr->tweakpoints -= 1;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}

			if (c == ESCAPE) mode = 0;			
		}

		/* Mode 2: Skills menu */
		if (mode == 2)
		{
			char letter;
			char skillstring[80];
			char str[80];
			int skillnum = 0;
			int row, rowname, col;

        		/* Prepare the screen */
			Term_erase(0, 1, 255);
        		for (i = 0; i < SCREEN_HGT; i++)
        		{
                		roff("\n");
        		}
        		c_put_str(TERM_WHITE, "PORTRALIS ITEM POWER UP", 0, 0);
        		c_put_str(TERM_WHITE, "-------------------------", 1, 0);

			c_put_str(TERM_WHITE, "Choose a skill to increase(costs 1 point per increase).", 3, 0);

			row = 20;
			rowname = 1;
			col = 5;

			for (i = 0 + (28 * spage); (i < (28 + (28 * spage)) && i < num_skills); i++)
			{

				if (skillnum == 0) letter = 'a';
				else if (skillnum == 1) letter = 'b';
				else if (skillnum == 2) letter = 'c';
				else if (skillnum == 3) letter = 'd';
				else if (skillnum == 4) letter = 'e';
				else if (skillnum == 5) letter = 'f';
				else if (skillnum == 6) letter = 'g';
				else if (skillnum == 7) letter = 'h';
				else if (skillnum == 8) letter = 'i';
				else if (skillnum == 9) letter = 'j';
				else if (skillnum == 10) letter = 'k';
				else if (skillnum == 11) letter = 'l';
				else if (skillnum == 12) letter = 'm';
				else if (skillnum == 13) letter = 'n';
				else if (skillnum == 14) letter = 'A';
				else if (skillnum == 15) letter = 'B';
				else if (skillnum == 16) letter = 'C';
				else if (skillnum == 17) letter = 'D';
				else if (skillnum == 18) letter = 'E';
				else if (skillnum == 19) letter = 'F';
				else if (skillnum == 20) letter = 'G';
				else if (skillnum == 21) letter = 'H';
				else if (skillnum == 22) letter = 'I';
				else if (skillnum == 23) letter = 'J';
				else if (skillnum == 24) letter = 'K';
				else if (skillnum == 25) letter = 'L';
				else if (skillnum == 26) letter = 'M';
				else if (skillnum == 27) letter = 'N';

				if (skillnum == 14)
				{
					col = 5;
					row = 43;
					rowname = 24;
				}

				sprintf(skillstring, "%c) %s:", letter, skill_names[i]);
				if (i == 10 || i == 11) c_put_str(TERM_L_DARK, skillstring, col, rowname);
				else c_put_str(TERM_WHITE, skillstring, col, rowname);
				sprintf(str, "%d", o_ptr->skillsbonus[i]);
        			c_put_str(TERM_L_GREEN, str, col, row);

				col += 1;
				skillnum += 1;
			}

			sprintf(tmp, "%d", o_ptr->tweakpoints);
			c_put_str(TERM_WHITE, "Bonus points: ", 20, 0);
			c_put_str(TERM_L_GREEN, tmp, 20, 14);

			c_put_str(TERM_WHITE, "Make a choice. Press ESC to return to main menu.", 22, 0);
			c_put_str(TERM_WHITE, "Press 'p' to access next skills page(if any).", 23, 0);

			c = inkey();

			if (c == 'a')
			{
				if (o_ptr->tweakpoints >= 1)
				{
					o_ptr->skillsbonus[0 + (28 * spage)] += 1;
					o_ptr->tweakpoints -= 1;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}
			else if (c == 'b')
			{
				if (o_ptr->tweakpoints >= 1)
				{
					o_ptr->skillsbonus[1 + (28 * spage)] += 1;
					o_ptr->tweakpoints -= 1;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}
			else if (c == 'c')
			{
				if (o_ptr->tweakpoints >= 1)
				{
					o_ptr->skillsbonus[2 + (28 * spage)] += 1;
					o_ptr->tweakpoints -= 1;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}
			else if (c == 'd')
			{
				if (o_ptr->tweakpoints >= 1)
				{
					o_ptr->skillsbonus[3 + (28 * spage)] += 1;
					o_ptr->tweakpoints -= 1;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}
			else if (c == 'e')
			{
				if (o_ptr->tweakpoints >= 1)
				{
					o_ptr->skillsbonus[4 + (28 * spage)] += 1;
					o_ptr->tweakpoints -= 1;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}
			else if (c == 'f')
			{
				if (o_ptr->tweakpoints >= 1)
				{
					o_ptr->skillsbonus[5 + (28 * spage)] += 1;
					o_ptr->tweakpoints -= 1;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}
			else if (c == 'g')
			{
				if (o_ptr->tweakpoints >= 1)
				{
					o_ptr->skillsbonus[6 + (28 * spage)] += 1;
					o_ptr->tweakpoints -= 1;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}
			else if (c == 'h')
			{
				if (o_ptr->tweakpoints >= 1)
				{
					o_ptr->skillsbonus[7 + (28 * spage)] += 1;
					o_ptr->tweakpoints -= 1;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}
			else if (c == 'i')
			{
				if (o_ptr->tweakpoints >= 1)
				{
					o_ptr->skillsbonus[8 + (28 * spage)] += 1;
					o_ptr->tweakpoints -= 1;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}
			else if (c == 'j')
			{
				if (o_ptr->tweakpoints >= 1)
				{
					o_ptr->skillsbonus[9 + (28 * spage)] += 1;
					o_ptr->tweakpoints -= 1;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}
			else if (c == 'k')
			{
				if (o_ptr->tweakpoints >= 1)
				{
					if (!(((10 + (28 * spage)) == 10)))
					{
						o_ptr->skillsbonus[10 + (28 * spage)] += 1;
						o_ptr->tweakpoints -= 1;
					}
				}
			}
			else if (c == 'l')
			{
				if (o_ptr->tweakpoints >= 1)
				{
					if (!(((11 + (28 * spage)) == 11)))
					{
						o_ptr->skillsbonus[11 + (28 * spage)] += 1;
						o_ptr->tweakpoints -= 1;
					}
				}
			}
			else if (c == 'm')
			{
				if (o_ptr->tweakpoints >= 1)
				{
					o_ptr->skillsbonus[12 + (28 * spage)] += 1;
					o_ptr->tweakpoints -= 1;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}
			else if (c == 'n')
			{
				if (o_ptr->tweakpoints >= 1)
				{
					o_ptr->skillsbonus[13 + (28 * spage)] += 1;
					o_ptr->tweakpoints -= 1;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}
			else if (c == 'A')
			{
				if (o_ptr->tweakpoints >= 1)
				{
					o_ptr->skillsbonus[14 + (28 * spage)] += 1;
					o_ptr->tweakpoints -= 1;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}
			else if (c == 'B')
			{
				if (o_ptr->tweakpoints >= 1)
				{
					o_ptr->skillsbonus[15 + (28 * spage)] += 1;
					o_ptr->tweakpoints -= 1;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}
			else if (c == 'C')
			{
				if (o_ptr->tweakpoints >= 1)
				{
					o_ptr->skillsbonus[16 + (28 * spage)] += 1;
					o_ptr->tweakpoints -= 1;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}
			else if (c == 'D')
			{
				if (o_ptr->tweakpoints >= 1)
				{
					o_ptr->skillsbonus[17 + (28 * spage)] += 1;
					o_ptr->tweakpoints -= 1;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}
			else if (c == 'E')
			{
				if (o_ptr->tweakpoints >= 1)
				{
					o_ptr->skillsbonus[18 + (28 * spage)] += 1;
					o_ptr->tweakpoints -= 1;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}
			else if (c == 'F')
			{
				if (o_ptr->tweakpoints >= 1)
				{
					o_ptr->skillsbonus[19 + (28 * spage)] += 1;
					o_ptr->tweakpoints -= 1;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}
			else if (c == 'G')
			{
				if (o_ptr->tweakpoints >= 1)
				{
					o_ptr->skillsbonus[20 + (28 * spage)] += 1;
					o_ptr->tweakpoints -= 1;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}
			else if (c == 'H')
			{
				if (o_ptr->tweakpoints >= 1)
				{
					o_ptr->skillsbonus[21 + (28 * spage)] += 1;
					o_ptr->tweakpoints -= 1;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}
			else if (c == 'I')
			{
				if (o_ptr->tweakpoints >= 1)
				{
					o_ptr->skillsbonus[22 + (28 * spage)] += 1;
					o_ptr->tweakpoints -= 1;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}
			else if (c == 'J')
			{
				if (o_ptr->tweakpoints >= 1)
				{
					o_ptr->skillsbonus[23 + (28 * spage)] += 1;
					o_ptr->tweakpoints -= 1;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}
			else if (c == 'K')
			{
				if (o_ptr->tweakpoints >= 1)
				{
					o_ptr->skillsbonus[24 + (28 * spage)] += 1;
					o_ptr->tweakpoints -= 1;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}
			else if (c == 'L')
			{
				if (o_ptr->tweakpoints >= 1)
				{
					o_ptr->skillsbonus[25 + (28 * spage)] += 1;
					o_ptr->tweakpoints -= 1;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}
			else if (c == 'M')
			{
				if (o_ptr->tweakpoints >= 1)
				{
					o_ptr->skillsbonus[26 + (28 * spage)] += 1;
					o_ptr->tweakpoints -= 1;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}
			else if (c == 'N')
			{
				if (o_ptr->tweakpoints >= 1)
				{
					o_ptr->skillsbonus[27 + (28 * spage)] += 1;
					o_ptr->tweakpoints -= 1;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}

			else if (c == 'p' || c == 'P')
			{
				spage += 1;
				if ((28 * spage) >= num_skills) spage = 0;
			}

			if (c == ESCAPE) mode = 0;			
		}

		/* Mode 3: Attacks/Speed */
		if (mode == 3)
		{

        		/* Prepare the screen */
			Term_erase(0, 1, 255);
        		for (i = 0; i < SCREEN_HGT; i++)
        		{
                		roff("\n");
        		}
        		c_put_str(TERM_WHITE, "PORTRALIS ITEM POWER UP", 0, 0);
        		c_put_str(TERM_WHITE, "-------------------------", 1, 0);

			c_put_str(TERM_WHITE, "Choose which to increase.", 4, 0);

			c_put_str(TERM_WHITE, "a) Attacks(10 pts/20 for 3rd+):", 6, 0);
			c_put_str(TERM_WHITE, "b) Shots(20 pts/40 for 2nd+)  :", 7, 0);
			c_put_str(TERM_WHITE, "c) Speed(2 pts)               :", 8, 0);
			c_put_str(TERM_WHITE, "d) Reflection(1 pt/3 points)  :", 9, 0);
			
			sprintf(tmp, "%d", o_ptr->extrablows);
			c_put_str(TERM_L_GREEN, tmp, 6, 32);
			sprintf(tmp, "%d", o_ptr->extrashots);
			c_put_str(TERM_L_GREEN, tmp, 7, 32);
			sprintf(tmp, "%d", o_ptr->speedbonus);
			c_put_str(TERM_L_GREEN, tmp, 8, 32);
			sprintf(tmp, "%d", o_ptr->reflect);
			c_put_str(TERM_L_GREEN, tmp, 9, 32);

			sprintf(tmp, "%d", o_ptr->tweakpoints);
			c_put_str(TERM_WHITE, "Bonus points: ", 18, 0);
			c_put_str(TERM_L_GREEN, tmp, 18, 14);

			c_put_str(TERM_WHITE, "Make a choice. Press ESC to return to main menu.", 20, 0);

			c = inkey();

			if (c == 'a' || c == 'A')
			{
				if (o_ptr->extrablows >= 2)
				{
					if (o_ptr->tweakpoints >= 20)
					{
						o_ptr->extrablows += 1;
						o_ptr->tweakpoints -= 20;
						if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
					}
				}
				else
				{
					if (o_ptr->tweakpoints >= 10)
					{
						o_ptr->extrablows += 1;
						o_ptr->tweakpoints -= 10;
						if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
					}
				}
			}
			if (c == 'b' || c == 'B')
			{
				if (o_ptr->extrashots >= 1)
				{
					if (o_ptr->tweakpoints >= 40)
					{
						o_ptr->extrashots += 1;
						o_ptr->tweakpoints -= 40;
						if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
					}
				}
				else
				{
					if (o_ptr->tweakpoints >= 20)
					{
						o_ptr->extrashots += 1;
						o_ptr->tweakpoints -= 20;
						if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
					}
				}
			}
			if (c == 'c' || c == 'C')
			{
				if (o_ptr->tweakpoints >= 2)
				{
					o_ptr->speedbonus += 1;
					o_ptr->tweakpoints -= 2;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}
			if (c == 'd' || c == 'D')
			{
				if (o_ptr->tweakpoints >= 1)
				{
					o_ptr->reflect += 3;
					o_ptr->tweakpoints -= 1;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}

			if (c == ESCAPE) mode = 0;			
		}

		/* Mode 4: Life/Mana */
		if (mode == 4)
		{

        		/* Prepare the screen */
			Term_erase(0, 1, 255);
        		for (i = 0; i < SCREEN_HGT; i++)
        		{
                		roff("\n");
        		}
        		c_put_str(TERM_WHITE, "PORTRALIS ITEM POWER UP", 0, 0);
        		c_put_str(TERM_WHITE, "-------------------------", 1, 0);

			c_put_str(TERM_WHITE, "Choose which to increase.", 4, 0);

			/*c_put_str(TERM_WHITE, "a) Life(1 pt/5%)  :", 6, 0);*/
			c_put_str(TERM_WHITE, "a) Mana(1 pt/3%)  :", 6, 0);
			c_put_str(TERM_WHITE, "b) Spells(1 pt/1%):", 7, 0);
			
			/*sprintf(tmp, "%d%%", o_ptr->lifebonus);
			c_put_str(TERM_L_GREEN, tmp, 6, 20);*/
			sprintf(tmp, "%d%%", o_ptr->manabonus);
			c_put_str(TERM_L_GREEN, tmp, 6, 20);
			sprintf(tmp, "%d%%", o_ptr->spellbonus);
			c_put_str(TERM_L_GREEN, tmp, 7, 20);

			sprintf(tmp, "%d", o_ptr->tweakpoints);
			c_put_str(TERM_WHITE, "Bonus points: ", 18, 0);
			c_put_str(TERM_L_GREEN, tmp, 18, 14);

			c_put_str(TERM_WHITE, "Make a choice. Press ESC to return to main menu.", 20, 0);

			c = inkey();

			/*if (c == 'a' || c == 'A')
			{
				if (o_ptr->tweakpoints >= 1)
				{
					o_ptr->lifebonus += 5;
					o_ptr->tweakpoints -= 1;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}*/
			if (c == 'a' || c == 'A')
			{
				if (o_ptr->tweakpoints >= 1)
				{
					o_ptr->manabonus += 3;
					o_ptr->tweakpoints -= 1;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}
			if (c == 'b' || c == 'B')
			{
				if (o_ptr->tweakpoints >= 2)
				{
					o_ptr->spellbonus += 1;
					o_ptr->tweakpoints -= 1;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}

			if (c == ESCAPE) mode = 0;			
		}

		/* Mode 5: Misc Abilities */
		if (mode == 5)
		{
			u32b f1, f2, f3, f4;
			object_flags(o_ptr, &f1, &f2, &f3, &f4);

        		/* Prepare the screen */
			Term_erase(0, 1, 255);
        		for (i = 0; i < SCREEN_HGT; i++)
        		{
                		roff("\n");
        		}
        		c_put_str(TERM_WHITE, "PORTRALIS ITEM POWER UP", 0, 0);
        		c_put_str(TERM_WHITE, "-------------------------", 1, 0);

			c_put_str(TERM_WHITE, "Choose which to increase.", 3, 0);

			c_put_str(TERM_WHITE, "a) Permanent light(1 pt to set to 5):", 5, 0);
			c_put_str(TERM_WHITE, "b) Sust. Strength(2 pts)            :", 6, 0);
			c_put_str(TERM_WHITE, "c) Sust. Intelligence(2 pts)        :", 7, 0);
			c_put_str(TERM_WHITE, "d) Sust. Wisdom(2 pts)              :", 8, 0);
			c_put_str(TERM_WHITE, "e) Sust. Dexterity(2 pts)           :", 9, 0);
			c_put_str(TERM_WHITE, "f) Sust. Constitution(2 pts)        :", 10, 0);
			c_put_str(TERM_WHITE, "g) Sust. Charisma(2 pts)            :", 11, 0);
			c_put_str(TERM_WHITE, "h) Resist Confusion(3 pts)          :", 12, 0);
			c_put_str(TERM_WHITE, "i) Resist Fear(2 pts)               :", 13, 0);
			c_put_str(TERM_WHITE, "j) Resist Blindness(1 pt)           :", 14, 0);
			c_put_str(TERM_WHITE, "k) Resist Paralysis(3 pts)          :", 15, 0);
			c_put_str(TERM_WHITE, "l) Resist Exp. Draining(3 pts)      :", 16, 0);
			c_put_str(TERM_WHITE, "m) Telepathy(3 pts)                 :", 17, 0);
			c_put_str(TERM_WHITE, "n) Regenerate(5 pts)                :", 18, 0);
			c_put_str(TERM_WHITE, "o) Eternal(5 pts)                   :", 19, 0);
			
			sprintf(tmp, "%d", o_ptr->light);
			c_put_str(TERM_L_GREEN, tmp, 5, 38);
			if (f2 & (TR2_SUST_STR)) sprintf(tmp, "Yes");
			else sprintf(tmp, "No");
			c_put_str(TERM_L_GREEN, tmp, 6, 38);
			if (f2 & (TR2_SUST_INT)) sprintf(tmp, "Yes");
			else sprintf(tmp, "No");
			c_put_str(TERM_L_GREEN, tmp, 7, 38);
			if (f2 & (TR2_SUST_WIS)) sprintf(tmp, "Yes");
			else sprintf(tmp, "No");
			c_put_str(TERM_L_GREEN, tmp, 8, 38);
			if (f2 & (TR2_SUST_DEX)) sprintf(tmp, "Yes");
			else sprintf(tmp, "No");
			c_put_str(TERM_L_GREEN, tmp, 9, 38);
			if (f2 & (TR2_SUST_CON)) sprintf(tmp, "Yes");
			else sprintf(tmp, "No");
			c_put_str(TERM_L_GREEN, tmp, 10, 38);
			if (f2 & (TR2_SUST_CHR)) sprintf(tmp, "Yes");
			else sprintf(tmp, "No");
			c_put_str(TERM_L_GREEN, tmp, 11, 38);
			if (f2 & (TR2_RES_CONF)) sprintf(tmp, "Yes");
			else sprintf(tmp, "No");
			c_put_str(TERM_L_GREEN, tmp, 12, 38);
			if (f2 & (TR2_RES_FEAR)) sprintf(tmp, "Yes");
			else sprintf(tmp, "No");
			c_put_str(TERM_L_GREEN, tmp, 13, 38);
			if (f2 & (TR2_RES_BLIND)) sprintf(tmp, "Yes");
			else sprintf(tmp, "No");
			c_put_str(TERM_L_GREEN, tmp, 14, 38);
			if (f4 & (TR4_SAFETY)) sprintf(tmp, "Yes");
			else sprintf(tmp, "No");
			c_put_str(TERM_L_GREEN, tmp, 15, 38);
			if (f2 & (TR2_HOLD_LIFE)) sprintf(tmp, "Yes");
			else sprintf(tmp, "No");
			c_put_str(TERM_L_GREEN, tmp, 16, 38);
			if (f3 & (TR3_TELEPATHY)) sprintf(tmp, "Yes");
			else sprintf(tmp, "No");
			c_put_str(TERM_L_GREEN, tmp, 17, 38);
			if (f3 & (TR3_REGEN)) sprintf(tmp, "Yes");
			else sprintf(tmp, "No");
			c_put_str(TERM_L_GREEN, tmp, 18, 38);
			if (f4 & (TR4_ETERNAL)) sprintf(tmp, "Yes");
			else sprintf(tmp, "No");
			c_put_str(TERM_L_GREEN, tmp, 19, 38);

			sprintf(tmp, "%d", o_ptr->tweakpoints);
			c_put_str(TERM_WHITE, "Bonus points: ", 21, 0);
			c_put_str(TERM_L_GREEN, tmp, 21, 14);

			c_put_str(TERM_WHITE, "Make a choice. Press ESC to return to main menu.", 23, 0);

			c = inkey();

			if (c == 'a' || c == 'A')
			{
				if (o_ptr->tweakpoints >= 1 && o_ptr->light < 5)
				{
					o_ptr->light = 5;
					o_ptr->tweakpoints -= 1;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}
			if (c == 'b' || c == 'B')
			{
				if (o_ptr->tweakpoints >= 2 && !(f2 & (TR2_SUST_STR)))
				{
					o_ptr->art_flags2 |= (TR2_SUST_STR);
					o_ptr->tweakpoints -= 2;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}
			if (c == 'c' || c == 'C')
			{
				if (o_ptr->tweakpoints >= 2 && !(f2 & (TR2_SUST_INT)))
				{
					o_ptr->art_flags2 |= (TR2_SUST_INT);
					o_ptr->tweakpoints -= 2;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}
			if (c == 'd' || c == 'D')
			{
				if (o_ptr->tweakpoints >= 2 && !(f2 & (TR2_SUST_WIS)))
				{
					o_ptr->art_flags2 |= (TR2_SUST_WIS);
					o_ptr->tweakpoints -= 2;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}
			if (c == 'e' || c == 'E')
			{
				if (o_ptr->tweakpoints >= 2 && !(f2 & (TR2_SUST_DEX)))
				{
					o_ptr->art_flags2 |= (TR2_SUST_DEX);
					o_ptr->tweakpoints -= 2;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}
			if (c == 'f' || c == 'F')
			{
				if (o_ptr->tweakpoints >= 2 && !(f2 & (TR2_SUST_CON)))
				{
					o_ptr->art_flags2 |= (TR2_SUST_CON);
					o_ptr->tweakpoints -= 2;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}
			if (c == 'g' || c == 'G')
			{
				if (o_ptr->tweakpoints >= 2 && !(f2 & (TR2_SUST_CHR)))
				{
					o_ptr->art_flags2 |= (TR2_SUST_CHR);
					o_ptr->tweakpoints -= 2;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}
			if (c == 'h' || c == 'H')
			{
				if (o_ptr->tweakpoints >= 3 && !(f2 & (TR2_RES_CONF)))
				{
					o_ptr->art_flags2 |= (TR2_RES_CONF);
					o_ptr->tweakpoints -= 3;
				}
			}
			if (c == 'i' || c == 'I')
			{
				if (o_ptr->tweakpoints >= 2 && !(f2 & (TR2_RES_FEAR)))
				{
					o_ptr->art_flags2 |= (TR2_RES_FEAR);
					o_ptr->tweakpoints -= 2;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}
			if (c == 'j' || c == 'J')
			{
				if (o_ptr->tweakpoints >= 1 && !(f2 & (TR2_RES_BLIND)))
				{
					o_ptr->art_flags2 |= (TR2_RES_BLIND);
					o_ptr->tweakpoints -= 1;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}
			if (c == 'k' || c == 'K')
			{
				if (o_ptr->tweakpoints >= 3 && !(f4 & (TR4_SAFETY)))
				{
					o_ptr->art_flags4 |= (TR4_SAFETY);
					o_ptr->tweakpoints -= 3;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}
			if (c == 'l' || c == 'L')
			{
				if (o_ptr->tweakpoints >= 3 && !(f2 & (TR2_HOLD_LIFE)))
				{
					o_ptr->art_flags2 |= (TR2_HOLD_LIFE);
					o_ptr->tweakpoints -= 3;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}
			if (c == 'm' || c == 'M')
			{
				if (o_ptr->tweakpoints >= 3 && !(f3 & (TR3_TELEPATHY)))
				{
					o_ptr->art_flags3 |= (TR3_TELEPATHY);
					o_ptr->tweakpoints -= 3;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}
			if (c == 'n' || c == 'N')
			{
				if (o_ptr->tweakpoints >= 5 && !(f3 & (TR3_REGEN)))
				{
					o_ptr->art_flags3 |= (TR3_REGEN);
					o_ptr->tweakpoints -= 5;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}
			if (c == 'o' || c == 'O')
			{
				if (o_ptr->tweakpoints >= 5 && !(f4 & (TR4_ETERNAL)))
				{
					o_ptr->art_flags4 |= (TR4_ETERNAL);
					o_ptr->tweakpoints -= 5;
					if (crafting && !(o_ptr->art_flags4 & (TR4_CRAFTED))) o_ptr->art_flags4 |= (TR4_CRAFTED);
				}
			}

			if (c == ESCAPE) mode = 0;			
		}
	}

	Term_load();
}

/* Soul powers */
int use_monster_soul()
{
	int                     Power = -1;
        int                     num = 0, dir = 0 , i;

	int             powers[36];
	char            power_desc[36][80];

	bool            flag, redraw;
        int             ask, plev = p_ptr->lev;

        char            choice, ch;

	char            out_val[160];
        int k,count;

        /* List the powers */
        strcpy(power_desc[num],"Capture Soul(Cost: 5   Effect: Try to capture a monster's soul.)");powers[num++]=1;
        strcpy(power_desc[num],"Soul Powers(Cost: ??   Effect: Use a soul's powers.)");powers[num++]=2;
        strcpy(power_desc[num],"Soul Bind(Effect: Enchant an item with a soul.)");powers[num++]=3;
        strcpy(power_desc[num],"Simulacrum(Cost: ??   Effect: Summon a copy of the soul's monster.)");powers[num++]=4;

        if(!num) {msg_print("You can't cast any spells from this category!");return 0;}

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt (accept all spells) */
	if (num <= 26)
	{
		/* Build a prompt (accept all spells) */
                strnfmt(out_val, 78, "(Powers %c-%c, *=List, ESC=exit) Cast which spell? ",
			I2A(0), I2A(num - 1));
	}
	else
	{
                strnfmt(out_val, 78, "(Powers %c-%c, *=List, ESC=exit) Cast which spell? ",
			I2A(0), '0' + num - 27);
	}

	/* Get a spell from the user */
	while (!flag && get_com(out_val, &choice))
	{
		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Show the list */
			if (!redraw)
			{
				byte y = 1, x = 0;
				int ctr = 0;
				char dummy[80];

				strcpy(dummy, "");

				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				Term_save();

				prt ("", y++, x);

                                while (ctr < num && ctr < 19)
				{
					sprintf(dummy, "%c) %s", I2A(ctr), power_desc[ctr]);
					prt(dummy, y + ctr, x);
					ctr++;
				}
				while (ctr < num)
				{
					if (ctr < 26)
					{
						sprintf(dummy, " %c) %s", I2A(ctr), power_desc[ctr]);
					}
					else
					{
						sprintf(dummy, " %c) %s", '0' + ctr - 26, power_desc[ctr]);
					}
                                        prt(dummy, y + ctr - 19, x + 40);
					ctr++;
				}
                                if (ctr < 19)
				{
					prt ("", y + ctr, x);
				}
				else
				{
                                        prt ("", y + 19, x);
				}
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

		if (choice == '\r' && num == 1)
		{
			choice = 'a';
		}

		if (isalpha(choice))
		{
			/* Note verify */
			ask = (isupper(choice));

			/* Lowercase */
			if (ask) choice = tolower(choice);

			/* Extract request */
			i = (islower(choice) ? A2I(choice) : -1);
		}
		else
		{
			ask = FALSE; /* Can't uppercase digits */

			i = choice - '0' + 26;
		}

		/* Totally Illegal */
		if ((i < 0) || (i >= num))
		{
			bell();
			continue;
		}

		/* Save the spell index */
		Power = powers[i];

		/* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Prompt */
			strnfmt(tmp_val, 78, "Use %s? ", power_desc[i]);

			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}

	/* Restore the screen */
	if (redraw) Term_load();

	/* Abort if needed */
	if (!flag) 
	{
		energy_use = 0;
                return num;
	}

        switch(Power)
        {
                case 1:
                        if (p_ptr->csp >= 5)
                        {
                                int ii, ij;
                                if (!tgt_pt(&ii,&ij)) return num;
                                capture_soul(ii, ij);
                                p_ptr->csp -= 5;
                                update_and_handle();
                        }
                        else msg_print("You don't have enough mana points!");
                        break;
                case 2:
                        use_soul_power();
                        update_and_handle();
                        break;
                case 3:
                        soul_bind();
                        update_and_handle();
                        break;
                case 4:
                        simulacrum();
                        update_and_handle();
                        break;

        }
        energy_use = 100;
        return num;
}


/* Draw offensive powers from a monster's soul */
void use_soul_power()
{
	int                     Power = -1;
        int                     num = 0, dir = 0 , i, j;

	int             powers[36];
	char            power_desc[36][80];

	bool            flag, redraw;
        int             ask, plev = p_ptr->lev;

	s32b dam;

        char            choice, ch;

	char            out_val[160];
        int k,count;
        int rlev = dun_level + p_ptr->lev + randint(5);
	int rad;

        int item, spellstat;
	s32b rodbonus = 0;
        object_type             *o_ptr;
        monster_race            *r_ptr;
        cptr q, s;

	/* This determines casting power */
	spellstat = (p_ptr->stat_ind[A_WIS] - 5);

	/* No lower than 0. */
	if (spellstat < 0) spellstat = 0;

	/* Prepare rods bonus. */
	o_ptr = &inventory[INVEN_WIELD];
	if (o_ptr->tval == TV_ROD)
	{
		rodbonus += damroll(o_ptr->dd, o_ptr->ds);
		rodbonus += multiply_divide(rodbonus, p_ptr->skill[17] * 3, 100);
	}
	o_ptr = &inventory[INVEN_WIELD+1];
	if (o_ptr->tval == TV_ROD)
	{
		rodbonus += damroll(o_ptr->dd, o_ptr->ds);
		rodbonus += multiply_divide(rodbonus, p_ptr->skill[17] * 3, 100);
	}
	if (p_ptr->prace == RACE_MONSTER && unarmed())
	{
		o_ptr = &inventory[INVEN_ESSENCE];
		if (o_ptr->tval == TV_ESSENCE) rodbonus += damroll(o_ptr->dd, o_ptr->ds);
	}
	
        /* Restrict choices to souls */
        item_tester_tval = TV_SOUL;

        /* Get an item */
        q = "Use which soul? ";
        s = "You have no souls!";
        if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP))) return;

        /* Get the item */
        o_ptr = &inventory[item];

	/* Is it charging? */
	if (o_ptr->timeout >= 1)
        {
                msg_print("This soul still need to recharge.");
                return;
        }

        /* Get the monster from the soul */
        r_ptr = &r_info[o_ptr->pval];

        /* List the powers */
	for (i = 0; i < 20; i++)
        {
                char spellstring[80];
                if (r_ptr->spell[i].type > 0 && r_ptr->spell[i].type < 9)
                {
                        sprintf(spellstring, "%s   Difficulty: %d", r_ptr->spell[i].name, r_ptr->spell[i].cost);        
                        /* List the powers */
                        strcpy(power_desc[num],spellstring);powers[num++]=i;
                }
        }    
        if(!num) {msg_print("No powers to use.");return 0;}

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt (accept all spells) */
	if (num <= 26)
	{
		/* Build a prompt (accept all spells) */
                strnfmt(out_val, 78, "(Powers %c-%c, *=List, ESC=exit) Use which power? ",
			I2A(0), I2A(num - 1));
	}
	else
	{
                strnfmt(out_val, 78, "(Powers %c-%c, *=List, ESC=exit) Use which power? ",
			I2A(0), '0' + num - 27);
	}

	/* Get a spell from the user */
	while (!flag && get_com(out_val, &choice))
	{
		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Show the list */
			if (!redraw)
			{
				byte y = 1, x = 0;
				int ctr = 0;
				char dummy[80];

				strcpy(dummy, "");

				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				Term_save();

				prt ("", y++, x);

				while (ctr < num && ctr < 17)
				{
					sprintf(dummy, "%c) %s", I2A(ctr), power_desc[ctr]);
					prt(dummy, y + ctr, x);
					ctr++;
				}
				while (ctr < num)
				{
					if (ctr < 26)
					{
						sprintf(dummy, " %c) %s", I2A(ctr), power_desc[ctr]);
					}
					else
					{
						sprintf(dummy, " %c) %s", '0' + ctr - 26, power_desc[ctr]);
					}
					prt(dummy, y + ctr - 17, x + 40);
					ctr++;
				}
				if (ctr < 17)
				{
					prt ("", y + ctr, x);
				}
				else
				{
					prt ("", y + 17, x);
				}
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

		if (choice == '\r' && num == 1)
		{
			choice = 'a';
		}

		if (isalpha(choice))
		{
			/* Note verify */
			ask = (isupper(choice));

			/* Lowercase */
			if (ask) choice = tolower(choice);

			/* Extract request */
			i = (islower(choice) ? A2I(choice) : -1);
		}
		else
		{
			ask = FALSE; /* Can't uppercase digits */

			i = choice - '0' + 26;
		}

		/* Totally Illegal */
		if ((i < 0) || (i >= num))
		{
			bell();
			continue;
		}

		/* Save the spell index */
		Power = powers[i];

		/* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Prompt */
			strnfmt(tmp_val, 78, "Use %s? ", power_desc[i]);

			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}

	/* Restore the screen */
	if (redraw) Term_load();

	/* Abort if needed */
	if (!flag) 
	{
		energy_use = 0;
                return;
	}

	/* Soul Guardian's spells are wisdom based! */
	{
		bool castsuccess = FALSE;
		bool risky = FALSE;
		int castpower = 5;
		if (p_ptr->stat_ind[A_WIS] >= r_ptr->spell[Power].cost) castsuccess = TRUE;
		else
		{
			char riskych;
			int caststrength = (p_ptr->stat_ind[A_WIS] - 5) * castpower;
			if (caststrength < 0) caststrength = 0;
			
			if (r_ptr->spell[Power].cost > ((caststrength * 3) * 5))
			{
				msg_print("This spell is too strong to even attempt.");
				return;
			}
                        if (r_ptr->spell[Power].cost > (caststrength * 3)) risky = TRUE;
			if (risky)
			{
				if (!get_com("Casting this spell could be dangerous. Proceed anyway? [y/n]", &riskych)) return;
				if (!(riskych == 'y' || riskych == 'Y')) return;
			}
			if (randint(caststrength) >= randint(r_ptr->spell[Power].cost)) castsuccess = TRUE;
			else
			{
				msg_print("You failed to cast the spell...");
				castsuccess = FALSE;
				if (risky)
				{
					msg_print("Your spell has become out of control!");
					msg_print("The spell backfires, and your head is hurt by a vicious trauma!");
					take_hit(r_ptr->spell[Power].cost, "Failed Casting");
					(void)set_confused(5);
                                        dec_stat(A_WIS, 5, STAT_DEC_NORMAL);                              
				}
			}
		}
		/* Failures takes a turn. */
		if (!castsuccess)
		{
			energy_use = 100;
			return;
		}
	}

        switch (r_ptr->spell[Power].type)
	{
		/* Bolt */
		case 1:
		{
			dam = (r_ptr->spell[Power].power + rodbonus) * spellstat;
			dam += multiply_divide(dam, (p_ptr->abilities[(CLASS_SOUL_GUARDIAN * 10)] * 50), 100);
			dam = dam + multiply_divide(dam, p_ptr->to_s, 100);
			if(!get_aim_dir(&dir)) return;
			fire_bolt(r_ptr->spell[Power].special1, dir, dam);
			break;
		}
		/* Ball */
		case 2:
		{
			dam = (r_ptr->spell[Power].power + rodbonus) * spellstat;
			dam += multiply_divide(dam, (p_ptr->abilities[(CLASS_SOUL_GUARDIAN * 10)] * 50), 100);
			dam = dam + multiply_divide(dam, p_ptr->to_s, 100);
			rad = r_ptr->spell[Power].special2;
			if(!get_aim_dir(&dir)) return;
			fire_ball(r_ptr->spell[Power].special1, dir, dam, rad);
			break;
		}
		/* Heal */
		case 3:
		{
			dam = (r_ptr->spell[Power].power + rodbonus) * spellstat;
			dam += multiply_divide(dam, (p_ptr->abilities[(CLASS_SOUL_GUARDIAN * 10)] * 50), 100);
			dam = dam + multiply_divide(dam, p_ptr->to_s, 100);
			p_ptr->chp += dam;
			if (p_ptr->chp > p_ptr->mhp) p_ptr->chp = p_ptr->mhp;
			msg_print("You are healed!");
			update_and_handle();
			break;
		}
		/* Haste */
		case 4:
		{
			dam = r_ptr->spell[Power].power;
			(void)set_fast(dam);
			update_and_handle();
			break;
		}
		/* Boost */
		case 5:
		{
			dam = r_ptr->spell[Power].power;
			if (r_ptr->spell[Power].special1 == 1 || r_ptr->spell[Power].special1 == 4)
			{
				p_ptr->str_boost = dam;
                                (void)set_str_boost(20); 
			}
			if (r_ptr->spell[Power].special1 == 2 || r_ptr->spell[Power].special1 == 9)
			{
				p_ptr->dex_boost = dam;
                                (void)set_dex_boost(20); 
			}
			if (r_ptr->spell[Power].special1 == 3 || r_ptr->spell[Power].special1 == 5)
			{
				p_ptr->int_boost = dam;
                                (void)set_int_boost(20);
				p_ptr->wis_boost = dam;
                                (void)set_wis_boost(20); 
			}
			if ((r_ptr->spell[Power].special1 == 6) || (r_ptr->spell[Power].special1 == 7) || (r_ptr->spell[Power].special1 == 8)) 
			{
				p_ptr->str_boost = dam;
				p_ptr->dex_boost = dam;
				p_ptr->int_boost = dam;
				p_ptr->wis_boost = dam;
				(void)set_str_boost(20);
				(void)set_dex_boost(20);
				(void)set_int_boost(20);
				(void)set_wis_boost(20);
			}
			update_and_handle();
			break;
		}

		/* Summon Kind */
		case 6:
		{
			for (j = 0; j < r_ptr->spell[Power].special1; j++)
			{
				summon_specific_kind(py, px, r_ptr->spell[Power].power, r_ptr->spell[Power].summchar, FALSE, TRUE, r_ptr->spell[Power].special2);
			}
			break;
		}

		/* Summon Specific */
		case 7:
		{
			for (j = 0; j < r_ptr->spell[Power].special1; j++)
			{
				summon_specific_ridx(py, px, r_ptr->spell[Power].power, FALSE, TRUE, r_ptr->spell[Power].special2);
			}
			break;
		}
		/* Phase door */
		case 8:
		{
			teleport_player(r_ptr->spell[Power].power);
			break;
		}
		
		default:
		{
			break;
		}
	}
        energy_use = 100;
}

bool is_elemental(int power)
{
	if (power == GF_FIRE ||
            power == GF_COLD ||
            power == GF_ELEC ||
            power == GF_ACID ||
            power == GF_POIS ||
            power == GF_LITE ||
            power == GF_DARK ||
            power == GF_WARP ||
            power == GF_WATER ||
            power == GF_WIND ||
            power == GF_EARTH ||
            power == GF_SOUND ||
            power == GF_RADIO ||
            power == GF_CHAOS ||
            power == GF_PHYSICAL ||
            power == GF_MANA ||
            power == GF_MISSILE ||
	    power == GF_FROSTFIRE ||
	    power == GF_GREY ||
            power == GF_TOXIC ||
            power == GF_MUD ||
	    power == GF_ICE ||
	    power == GF_CONFUSION) return (TRUE);
	
	return (FALSE);
}

bool is_alteration(int power)
{
	if (power == GF_REDUCE_DEF ||
            power == GF_REDUCE_HIT ||
	    power == GF_STONE_TO_MUD) return (TRUE);
	
	return (FALSE);
}

bool is_mysticism(int power)
{
	if (power == GF_OLD_HEAL || power == GF_HARM || power == GF_DRAIN_LIFE || power == GF_UNDEAD_SMITE || power == GF_DEMON_SMITE || power == GF_EVIL_SMITE || power == GF_GOOD_SMITE) return (TRUE);
	
	return (FALSE);
}

bool is_divination(int power)
{
	if (power == GF_DIVINATION) return (TRUE);
	
	return (FALSE);
}

/* Cast a spell */
void do_cmd_cast(bool wisdom)
{
	int                     Power = -1;
        int                     num = 0, dir = 0 , i,k,count;
        s32b                    finalcost;
        s32b                    spellpower;
	int			amountslots = 12;

	int             powers[36];
	char            power_desc[36][80];
        char            summname[30];

        bool            flag, redraw;
        int             ask;

        char            choice, ch, summchar, ch2;

	char            out_val[160];
	int spellstat = 0;
	object_type *o_ptr;
        magic_spells *spell_ptr;

	/* This determines casting power */
	if (wisdom) spellstat = A_WIS;
	else spellstat = A_INT;

	/* No lower than 0 */
	if (spellstat < 0) spellstat = 0;

	if (!wisdom)
	{
		if (dun_level > 0) strcpy(&ch, "C");
		else if (!get_com("[C]ast a spell, [M]ake a spell, [D]elete a spell", &ch)) return;
	}

        if (wisdom) strcpy(&ch, "C");

        if (ch == 'C' || ch == 'c')
        {
	if (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 3] >= 1)
        {
                amountslots += (1 + (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 3] / 5));
        }
        for (i = 1; i <= amountslots; i++)
        {
                char spellstring[80];
                spell_ptr = &magic_spell[i];
                if (spell_ptr->created == TRUE)
                {
			if (!(monk_synchro))
			{
                        	if (wisdom)
				{
					s32b wisdomcost;
					s32b wispercent;
					wisdomcost = spell_ptr->finalcost - p_ptr->stat_ind[A_WIS];
					if (wisdomcost < 0) wisdomcost = 0;
					if (p_ptr->stat_ind[A_WIS] >= wisdomcost) wispercent = 100;
					else if (wisdomcost > (p_ptr->stat_ind[A_WIS] * 5)) wispercent = 0;
					else
					{
						s32b proll = p_ptr->stat_ind[A_WIS];
						s32b sroll = (spell_ptr->finalcost - p_ptr->stat_ind[A_WIS]);
						int prollbonus = 0;

						prollbonus = prollbonus + (p_ptr->abilities[(CLASS_PRIEST * 10)] * 10);

						proll = proll + multiply_divide(proll, prollbonus, 100);

						wispercent = multiply_divide(100, proll, (proll + sroll));
					}

					sprintf(spellstring, "%s   Cost: %ld(%ld, %ld%%)", spell_ptr->name, spell_ptr->finalcost, wisdomcost, wispercent);
				}
				else sprintf(spellstring, "%s   Cost: %ld", spell_ptr->name, spell_ptr->finalcost);
                        	/* List the powers */
                        	strcpy(power_desc[num],spellstring);powers[num++]=i;
			}
			else
			{
				if (spell_ptr->finalcost <= (spell_ptr->finalcost - p_ptr->stat_ind[A_WIS]))
				{
					if (spell_ptr->effect[0] != 6 && spell_ptr->effect[1] != 6 && spell_ptr->effect[2] != 6 && spell_ptr->effect[3] != 6 && spell_ptr->effect[4] != 6)
					{
						if (wisdom)
						{
							s32b wisdomcost;
							s32b wispercent;
							wisdomcost = spell_ptr->finalcost - p_ptr->stat_ind[A_WIS];
							if (wisdomcost < 0) wisdomcost = 0;
							if (p_ptr->stat_ind[A_WIS] >= wisdomcost) wispercent = 100;
							else if (wisdomcost > (p_ptr->stat_ind[A_WIS] * 5)) wispercent = 0;
							else
							{
								s32b proll = p_ptr->stat_ind[A_WIS];
								s32b sroll = (spell_ptr->finalcost - p_ptr->stat_ind[A_WIS]);
								int prollbonus = 0;

								prollbonus = prollbonus + (p_ptr->abilities[(CLASS_PRIEST * 10)] * 10);

								proll = proll + multiply_divide(proll, prollbonus, 100);

								wispercent = multiply_divide(100, proll, (proll + sroll));
							}

							sprintf(spellstring, "%s   Cost: %ld(%ld, %ld%%)", spell_ptr->name, spell_ptr->finalcost, wisdomcost, wispercent);
						}
						else sprintf(spellstring, "%s   Cost: %ld", spell_ptr->name, spell_ptr->finalcost);
                        			/* List the powers */
                        			strcpy(power_desc[num],spellstring);powers[num++]=i;
					}
				}
			}
                }
        }

        if(!num) {msg_print("No spells to cast.");return 0;}

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt (accept all spells) */
	if (num <= 26)
	{
		/* Build a prompt (accept all spells) */
                strnfmt(out_val, 78, "(Spells %c-%c, *=List, ESC=exit) Cast which spell? ",
			I2A(0), I2A(num - 1));
	}
	else
	{
                strnfmt(out_val, 78, "(Spells %c-%c, *=List, ESC=exit) Cast which spell? ",
			I2A(0), '0' + num - 27);
	}

	/* Get a spell from the user */
	while (!flag && get_com(out_val, &choice))
	{
		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Show the list */
			if (!redraw)
			{
				byte y = 1, x = 0;
				int ctr = 0;
				char dummy[80];

				strcpy(dummy, "");

				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				Term_save();

				prt ("", y++, x);

                                while (ctr < num && ctr < 19)
				{
					sprintf(dummy, "%c) %s", I2A(ctr), power_desc[ctr]);
					prt(dummy, y + ctr, x);
					ctr++;
				}
				while (ctr < num)
				{
					if (ctr < 26)
					{
						sprintf(dummy, " %c) %s", I2A(ctr), power_desc[ctr]);
					}
					else
					{
						sprintf(dummy, " %c) %s", '0' + ctr - 26, power_desc[ctr]);
					}
                                        prt(dummy, y + ctr - 19, x + 40);
					ctr++;
				}
                                if (ctr < 19)
				{
					prt ("", y + ctr, x);
				}
				else
				{
                                        prt ("", y + 19, x);
				}
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

		if (choice == '\r' && num == 1)
		{
			choice = 'a';
		}

		if (isalpha(choice))
		{
			/* Note verify */
			ask = (isupper(choice));

			/* Lowercase */
			if (ask) choice = tolower(choice);

			/* Extract request */
			i = (islower(choice) ? A2I(choice) : -1);
		}
		else
		{
			ask = FALSE; /* Can't uppercase digits */

			i = choice - '0' + 26;
		}

		/* Totally Illegal */
		if ((i < 0) || (i >= num))
		{
			bell();
			continue;
		}

		/* Save the spell index */
		Power = powers[i];

		/* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Prompt */
			strnfmt(tmp_val, 78, "Use %s? ", power_desc[i]);

			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}

	/* Restore the screen */
	if (redraw) Term_load();

	/* Abort if needed */
	if (!flag) 
	{
		energy_use = 0;
                return;
	}

        spell_ptr = &magic_spell[Power];
        finalcost = spell_ptr->finalcost;
	
	/* NewAngband 1.8.0: You can use Wisdom to cast spells! */
	/* They cost no mana, but have a chance of failure. */
	/* And if you get too ambitious... something bad may happen! ;) */
	if (wisdom)
	{
		bool castsuccess = FALSE;

		wisdom_casting = TRUE;

		if (p_ptr->stat_ind[A_WIS] >= (finalcost - p_ptr->stat_ind[A_WIS])) castsuccess = TRUE;
		else
		{
			int proll = p_ptr->stat_ind[A_WIS];
			int sroll = (finalcost - p_ptr->stat_ind[A_WIS]);
			int prollbonus = 0;

			if (sroll > (proll * 5))
			{
				msg_print("This spell is too strong to attempt casting.");
				return;
			}

			prollbonus = prollbonus + (p_ptr->abilities[(CLASS_PRIEST * 10)] * 10);

			proll = proll + multiply_divide(proll, prollbonus, 100);

			if (randint(proll) >= randint(sroll)) castsuccess = TRUE;
			else msg_print("You have failed to cast the spell.");
		}
		if (!castsuccess)
		{
			update_and_handle();
			energy_use = 100; 
			return;
		}
	}

	call_lua("player_before_magic", "", "");

        for (i = 0; i < 5; i++)
        {
                if (p_ptr->csp < finalcost && !wisdom)
                {
                        msg_print("You don't have enough mana to cast this spell.");
                        return;
                }
                spellpower = spell_ptr->power[i];
                
                if (spell_ptr->effect[i] != 0)
                {
                        /* Spell kind 1 = Attack spell */
                        if (spell_ptr->effect[i] == 1)
                        {
                                if (spell_ptr->shape[i] == 1)
                                {
					s32b dam;
					int school = 0;

					if (is_elemental(spell_ptr->type[i]))
					{
						casting_elemental = TRUE;
						school = 1;
					}
					if (is_alteration(spell_ptr->type[i]))
					{
						casting_alteration = TRUE;
						school = 2;
					}
					if (is_mysticism(spell_ptr->type[i]))
					{
						casting_mysticism = TRUE;
						school = 3;
					}
					if (is_divination(spell_ptr->type[i]))
					{
						casting_divination = TRUE;
						school = 5;
					}

                                        if (dir == 0)
					{
						if (!(get_aim_dir(&dir))) return;
					}
					call_lua("spell_damages", "(ddd)", "d", spellpower, spellstat, school, &dam);
					/*ignore_spellcraft = TRUE;*/
					if (p_ptr->abilities[(CLASS_MAGE * 10) + 4] >= 1) fire_ball(spell_ptr->type[i], dir, dam, 0 + (p_ptr->abilities[(CLASS_MAGE * 10) + 4] / 5));
                                        else fire_bolt(spell_ptr->type[i], dir, dam);
					/*ignore_spellcraft = FALSE; */
					casting_elemental = FALSE;
					casting_alteration = FALSE;
					casting_conjuration = FALSE;
					casting_mysticism = FALSE;
					casting_divination = FALSE;
                                }
                                if (spell_ptr->shape[i] == 2)
                                {
					s32b dam;
					int school = 0;
					int rad;
					
					rad = spell_ptr->radius[i];
					rad += (p_ptr->abilities[(CLASS_MAGE * 10) + 4] / 5);
					if (is_elemental(spell_ptr->type[i]))
					{
						casting_elemental = TRUE;
						school = 1;
					}
					if (is_alteration(spell_ptr->type[i]))
					{
						casting_alteration = TRUE;
						school = 2;
					}
					if (is_mysticism(spell_ptr->type[i]))
					{
						casting_mysticism = TRUE;
						school = 3;
					}
					if (is_divination(spell_ptr->type[i]))
					{
						casting_divination = TRUE;
						school = 5;
					}
					
                                        if (dir == 0)
					{
						if (!(get_aim_dir(&dir))) return;
					}
					call_lua("spell_damages", "(ddd)", "d", spellpower, spellstat, school, &dam);
					/*ignore_spellcraft = TRUE;*/
                                        fire_ball(spell_ptr->type[i], dir, dam, rad);
					/*ignore_spellcraft = FALSE;*/
					casting_elemental = FALSE;
					casting_alteration = FALSE;
					casting_conjuration = FALSE;
					casting_mysticism = FALSE;
					casting_divination = FALSE;
                                }
                                if (spell_ptr->shape[i] == 3)
                                {
					s32b dam;
					int school = 0;
					int rad;
					
					rad = spell_ptr->radius[i];
					rad += (p_ptr->abilities[(CLASS_MAGE * 10) + 4] / 5);
					if (is_elemental(spell_ptr->type[i]))
					{
						casting_elemental = TRUE;
						school = 1;
					}
					if (is_alteration(spell_ptr->type[i]))
					{
						casting_alteration = TRUE;
						school = 2;
					}
					if (is_mysticism(spell_ptr->type[i]))
					{
						casting_mysticism = TRUE;
						school = 3;
					}
					if (is_divination(spell_ptr->type[i]))
					{
						casting_divination = TRUE;
						school = 5;
					}
					
                                        if (dir == 0)
					{
						if (!(get_aim_dir(&dir))) return;
					}
					call_lua("spell_damages", "(ddd)", "d", spellpower, spellstat, school, &dam);
					/*ignore_spellcraft = TRUE;*/
                                        attack_aura(spell_ptr->type[i], dam, rad);
					/*ignore_spellcraft = FALSE;*/
					casting_elemental = FALSE;
					casting_alteration = FALSE;
					casting_conjuration = FALSE;
					casting_mysticism = FALSE;
					casting_divination = FALSE;
                                }
                                if (spell_ptr->shape[i] == 4)
                                {
					s32b dam;
					int school = 0;
					int rad;
					
					rad = spell_ptr->radius[i];
					if (is_elemental(spell_ptr->type[i]))
					{
						casting_elemental = TRUE;
						school = 1;
					}
					if (is_alteration(spell_ptr->type[i]))
					{
						casting_alteration = TRUE;
						school = 2;
					}
					if (is_mysticism(spell_ptr->type[i]))
					{
						casting_mysticism = TRUE;
						school = 3;
					}
					if (is_divination(spell_ptr->type[i]))
					{
						casting_divination = TRUE;
						school = 5;
					}
					
                                        if (dir == 0)
					{
						if (!(get_aim_dir(&dir))) return;
					}
					call_lua("spell_damages", "(ddd)", "d", spellpower, spellstat, school, &dam);
					/*ignore_spellcraft = TRUE;*/
                                        chain_attack(dir, spell_ptr->type[i], dam, rad, 20);
					/*ignore_spellcraft = FALSE;*/
					casting_elemental = FALSE;
					casting_alteration = FALSE;
					casting_conjuration = FALSE;
					casting_mysticism = FALSE;
					casting_divination = FALSE;
                                }
				if (spell_ptr->shape[i] == 5)
                                {
					s32b dam;
					int school = 0;

					if (is_elemental(spell_ptr->type[i]))
					{
						casting_elemental = TRUE;
						school = 1;
					}
					if (is_alteration(spell_ptr->type[i]))
					{
						casting_alteration = TRUE;
						school = 2;
					}
					if (is_mysticism(spell_ptr->type[i]))
					{
						casting_mysticism = TRUE;
						school = 3;
					}
					if (is_divination(spell_ptr->type[i]))
					{
						casting_divination = TRUE;
						school = 5;
					}

					call_lua("spell_damages", "(ddd)", "d", spellpower, spellstat, school, &dam);
					/*ignore_spellcraft = TRUE;*/

					/* Does not actually use lua, but this is a nice, easy-to-use function. */
					lua_project(-2, 0, py, px, dam, spell_ptr->type[i], 1);

					/*ignore_spellcraft = FALSE; */
					casting_elemental = FALSE;
					casting_alteration = FALSE;
					casting_conjuration = FALSE;
					casting_mysticism = FALSE;
					casting_divination = FALSE;
                                }
                        }
                        /* Spell kind 2 = Fixed Attack spell */
                        /* Does fixed damages, not increased by your level. */
                        if (spell_ptr->effect[i] == 2)
                        {
                                if (spell_ptr->shape[i] == 1)
                                {
					if (is_elemental(spell_ptr->type[i]))
					{
						casting_elemental = TRUE;
					}
					if (is_alteration(spell_ptr->type[i]))
					{
						casting_alteration = TRUE;
					}
					if (is_mysticism(spell_ptr->type[i]))
					{
						casting_mysticism = TRUE;
					}
					if (is_divination(spell_ptr->type[i]))
					{
						casting_divination = TRUE;
					}
                                        if (dir == 0)
					{
						if (!(get_aim_dir(&dir))) return;
					}
					/*ignore_spellcraft = TRUE;*/
					if (p_ptr->abilities[(CLASS_MAGE * 10) + 4] >= 1) fire_ball(spell_ptr->type[i], dir, spellpower, 0 + (p_ptr->abilities[(CLASS_MAGE * 10) + 4] / 5));
                                        else fire_bolt(spell_ptr->type[i], dir, spellpower);
					/*ignore_spellcraft = FALSE;*/
					casting_elemental = FALSE;
					casting_alteration = FALSE;
					casting_conjuration = FALSE;
					casting_mysticism = FALSE;
					casting_divination = FALSE;
                                }
                                if (spell_ptr->shape[i] == 2)
                                {
					int rad;
					rad = spell_ptr->radius[i];
					rad += (p_ptr->abilities[(CLASS_MAGE * 10) + 4] / 5);
					if (is_elemental(spell_ptr->type[i]))
					{
						casting_elemental = TRUE;
					}
					if (is_alteration(spell_ptr->type[i]))
					{
						casting_alteration = TRUE;
					}
					if (is_mysticism(spell_ptr->type[i]))
					{
						casting_mysticism = TRUE;
					}
					if (is_divination(spell_ptr->type[i]))
					{
						casting_divination = TRUE;
					}
                                        if (dir == 0)
					{
						if (!(get_aim_dir(&dir))) return;
					}
					/*ignore_spellcraft = TRUE;*/
                                        fire_ball(spell_ptr->type[i], dir, spellpower, rad);
					/*ignore_spellcraft = FALSE;*/
					casting_elemental = FALSE;
					casting_alteration = FALSE;
					casting_conjuration = FALSE;
					casting_mysticism = FALSE;
					casting_divination = FALSE;
                                }
                                if (spell_ptr->shape[i] == 3)
                                {
					int rad;
					rad = spell_ptr->radius[i];
					rad += (p_ptr->abilities[(CLASS_MAGE * 10) + 4] / 5);
					if (is_elemental(spell_ptr->type[i]))
					{
						casting_elemental = TRUE;
					}
					if (is_alteration(spell_ptr->type[i]))
					{
						casting_alteration = TRUE;
					}
					if (is_mysticism(spell_ptr->type[i]))
					{
						casting_mysticism = TRUE;
					}
					if (is_divination(spell_ptr->type[i]))
					{
						casting_divination = TRUE;
					}
                                        if (dir == 0)
					{
						if (!(get_aim_dir(&dir))) return;
					}
					/*ignore_spellcraft = TRUE;*/
                                        attack_aura(spell_ptr->type[i], spellpower, rad);
					/*ignore_spellcraft = FALSE;*/
					casting_elemental = FALSE;
					casting_alteration = FALSE;
					casting_conjuration = FALSE;
					casting_mysticism = FALSE;
					casting_divination = FALSE;
                                }
                                if (spell_ptr->shape[i] == 4)
                                {
					if (is_elemental(spell_ptr->type[i]))
					{
						casting_elemental = TRUE;
					}
					if (is_alteration(spell_ptr->type[i]))
					{
						casting_alteration = TRUE;
					}
					if (is_mysticism(spell_ptr->type[i]))
					{
						casting_mysticism = TRUE;
					}
					if (is_divination(spell_ptr->type[i]))
					{
						casting_divination = TRUE;
					}
                                        if (dir == 0)
					{
						if (!(get_aim_dir(&dir))) return;
					}
					/*ignore_spellcraft = TRUE;*/
                                        chain_attack(dir, spell_ptr->type[i], spellpower, spell_ptr->radius[i], 20);
					/*ignore_spellcraft = FALSE;*/
					casting_elemental = FALSE;
					casting_alteration = FALSE;
					casting_conjuration = FALSE;
					casting_mysticism = FALSE;
					casting_divination = FALSE;
                                }
				if (spell_ptr->shape[i] == 5)
                                {
					if (is_elemental(spell_ptr->type[i]))
					{
						casting_elemental = TRUE;
					}
					if (is_alteration(spell_ptr->type[i]))
					{
						casting_alteration = TRUE;
					}
					if (is_mysticism(spell_ptr->type[i]))
					{
						casting_mysticism = TRUE;
					}
					if (is_divination(spell_ptr->type[i]))
					{
						casting_divination = TRUE;
					}
                                        
					/*ignore_spellcraft = TRUE;*/

					/* Does not actually use lua, but this is a nice, easy-to-use function. */
					lua_project(-2, 0, py, px, spellpower, spell_ptr->type[i], 1);

					/*ignore_spellcraft = FALSE;*/
					casting_elemental = FALSE;
					casting_alteration = FALSE;
					casting_conjuration = FALSE;
					casting_mysticism = FALSE;
					casting_divination = FALSE;
                                }
                        }
                        /* Spell kind 3 = Haste */
                        /* Raise speed by 10. Power is the duration. */
                        if (spell_ptr->effect[i] == 3)
                        {
                                (void)set_fast(spellpower);
                        }
                        /* Spell kind 4 = Stat Boost */
                        /* Raise a stat by a certain amount. */
                        /* The Shape is the duration of the spell. */
                        if (spell_ptr->effect[i] == 4)
                        {
                                switch(spell_ptr->type[i])
                                {
                                        case 1:
					{
						if (p_ptr->str_boost_dur > 0)
						{
							msg_print("You must wait until the current effect wears off.");
							return;
						}
						else
						{
                                                	p_ptr->str_boost = spellpower;
                                                	(void)set_str_boost(spell_ptr->shape[i]);
						}
                                                break;
					}
                                        case 2:
					{
						if (p_ptr->int_boost_dur > 0)
						{
							msg_print("You must wait until the current effect wears off.");
							return;
						}
						else
						{
                                                	p_ptr->int_boost = spellpower;
                                                	(void)set_int_boost(spell_ptr->shape[i]);
						}
                                                break;
					}
                                        case 3:
					{
						if (p_ptr->wis_boost_dur > 0)
						{
							msg_print("You must wait until the current effect wears off.");
							return;
						}
						else
						{
                                                	p_ptr->wis_boost = spellpower;
                                                	(void)set_wis_boost(spell_ptr->shape[i]);
						}
                                                break;
					}
                                        case 4:
					{
						if (p_ptr->dex_boost_dur > 0)
						{
							msg_print("You must wait until the current effect wears off.");
							return;
						}
						else
						{
                                                	p_ptr->dex_boost = spellpower;
                                                	(void)set_dex_boost(spell_ptr->shape[i]);
						}
                                                break;
					}
                                        case 5:
					{
						if (p_ptr->con_boost_dur > 0)
						{
							msg_print("You must wait until the current effect wears off.");
							return;
						}
						else
						{
                                                	p_ptr->con_boost = spellpower;
                                                	(void)set_con_boost(spell_ptr->shape[i]);
						}
                                                break;
					}
                                        case 6:
					{
						if (p_ptr->chr_boost_dur > 0)
						{
							msg_print("You must wait until the current effect wears off.");
							return;
						}
						else
						{
                                                	p_ptr->chr_boost = spellpower;
                                                	(void)set_chr_boost(spell_ptr->shape[i]);
						}
                                                break;
					}
                                }
                        }
                        /* Spell kind 5 = Blessing */
                        /* Doubles to_hit and AC. Power is the duration. */
                        if (spell_ptr->effect[i] == 5)
                        {
                                (void)set_blessed(spellpower);
                        }
                        /* Spell kind 6 = Healing */
                        /* Heals some hp. The power is the number of */
                        /* of hp restored per levels. */
                        if (spell_ptr->effect[i] == 6)
                        {
                                s32b heal;
					
				call_lua("spell_damages", "(ddd)", "d", spellpower, spellstat, 3, &heal);
                                msg_print("You feel better!");
                                p_ptr->chp += heal;
                                if (p_ptr->chp >= p_ptr->mhp) p_ptr->chp = p_ptr->mhp;
                                update_and_handle();
                        }
                        /* Spell kind 7 = Restore Stats */
                        if (spell_ptr->effect[i] == 7)
                        {
                                (void)do_res_stat(A_STR);
                                (void)do_res_stat(A_INT);
                                (void)do_res_stat(A_WIS);
                                (void)do_res_stat(A_DEX);
                                (void)do_res_stat(A_CON);
                                (void)do_res_stat(A_CHR);
                        }
                        /* Spell kind 8 = Restore Status */
                        if (spell_ptr->effect[i] == 8)
                        {
                                (void)set_stun(0);
                                (void)set_poisoned(0);
                                (void)set_confused(0);
                                (void)set_paralyzed(0);
                                (void)set_blind(0);
                                (void)set_afraid(0);
                                restore_level();
                                update_and_handle();                                
                        }
                        /* Spell kind 9 = Cure Bleeding */
			/* Obsolete as of now. */
                        if (spell_ptr->effect[i] == 9)
                        {
                                (void)set_cut(0);
                        }

			/* Spell kind 10 is free */                        

                        /* Spell kind 11 = Revive Monster */
                        if (spell_ptr->effect[i] == 11)
                        {
                                revive_monster();
                        }
                        /* Spell kind 12 = Restore Mana */
                        if (spell_ptr->effect[i] == 12)
                        {
				/*if (wisdom) spellpower += multiply_divide(spellpower, (p_ptr->skill[24] * 10), 100);*/
                                p_ptr->csp += spellpower;
                                if (p_ptr->csp > p_ptr->msp) p_ptr->csp = p_ptr->msp;
                                update_and_handle();
                                msg_print("You recover your mana!");                                
                        }
                        /* Spell kind 13 = Summon Kind */
                        if (spell_ptr->effect[i] == 13)
                        {
				int maxpower;

				maxpower = (p_ptr->skill[25] / 4) + (p_ptr->skill[1] / 8);
				if (maxpower < 1) maxpower = 1;
				if (spellpower > maxpower) spellpower = maxpower;

                                for (k = 0; k < spell_ptr->shape[i] + ((spell_ptr->shape[i] * (p_ptr->skill[25] * 5)) / 100); k++)
				{
                                        if (i == 0) summchar = spell_ptr->schar1;
                                        if (i == 1) summchar = spell_ptr->schar2;
                                        if (i == 2) summchar = spell_ptr->schar3;
                                        if (i == 3) summchar = spell_ptr->schar4;
                                        if (i == 4) summchar = spell_ptr->schar5;
                                        summon_specific_friendly_kind(py, px, spellpower, summchar, TRUE, spell_ptr->type[i]);
				}                                
                        }
                        /* Spell kind 14 = Summon Specific */
                        if (spell_ptr->effect[i] == 14)
                        {
                                for (k = 0; k < spell_ptr->shape[i] + ((spell_ptr->shape[i] * (p_ptr->skill[25] * 5)) / 100); k++)
				{
                                        if (i == 0) strcpy(summname, spell_ptr->sspeci1);
                                        if (i == 1) strcpy(summname, spell_ptr->sspeci2);
                                        if (i == 2) strcpy(summname, spell_ptr->sspeci3);
                                        if (i == 3) strcpy(summname, spell_ptr->sspeci4);
                                        if (i == 4) strcpy(summname, spell_ptr->sspeci5);
                                        summon_specific_friendly_name(py, px, summname, FALSE, spell_ptr->type[i]);
				}                                
                        }
                        /* Spell kind 15 = Damage fields */
                        if (spell_ptr->effect[i] == 15)
                        {
                                switch (spell_ptr->shape[i])
                                {
                                        case 1:
                                        {
                                                s32b dam;
						int rad = 0;

						if (spell_ptr->type[i] != FEAT_TREES && spell_ptr->type[i] != FEAT_WEBS) rad += (p_ptr->abilities[(CLASS_MAGE * 10) + 4] / 5);
						call_lua("spell_damages", "(ddd)", "d", spellpower, spellstat, 4, &dam);
                                                place_field(spell_ptr->type[i], spell_ptr->radius[i] + rad, px, py, dam);
                                                update_and_handle();
                                                break;
                                        }
                                        case 2:
                                        {
                                                s32b dam;
						int rad = 0;
                                                int ii, ij;

						if (spell_ptr->type[i] != FEAT_TREES && spell_ptr->type[i] != FEAT_WEBS) rad += (p_ptr->abilities[(CLASS_MAGE * 10) + 4] / 5);
						call_lua("spell_damages", "(ddd)", "d", spellpower, spellstat, 4, &dam);
                                                if (!tgt_pt(&ii,&ij)) return;
						if ((ij != py && ii != px) && (!cave_empty_bold(ij,ii) || (cave[ij][ii].info & CAVE_ICKY) || (!(cave[ij][ii].info & CAVE_MARK) && !(cave[ij][ii].info & CAVE_LITE))))
        					{
        						msg_print("You can't place a field there.");
        					}
        					else
        					{
							if (distance(ij,ii,py,px) > 10)
							{
								msg_print("This is too far!");
							}
                                                	else
							{ 
								place_field(spell_ptr->type[i], spell_ptr->radius[i] + rad, ii, ij, dam);
                                                		update_and_handle();
							}
						}
                                                break;
                                        }
                                }
                        }
                        /* Spell kind 16 = Detections */
                        if (spell_ptr->effect[i] == 16)
                        {
                                switch (spell_ptr->type[i])
                                {
                                        case 1:
                                        {
                                                detect_monsters_invis();
                                                detect_monsters_normal();
                                                break;
                                        }
                                        case 2:
                                        {
                                                detect_objects_normal();
                                                detect_objects_gold();
                                                break;
                                        }
                                        case 3:
                                        {
                                                detect_doors();
                                                break;
                                        }
                                        case 4:
                                        {
                                                detect_stairs();
                                                break;
                                        }
                                        case 5:
                                        {
                                                detect_chests();
                                                break;
                                        }

                                }
                        }
                        /* Spell kind 17 = Telepathy */
                        if (spell_ptr->effect[i] == 17)
                        {
                                (void)set_tim_esp(spellpower);
                        }
                        /* Spell kind 18 = Identify */
                        if (spell_ptr->effect[i] == 18)
                        {
                                identify_fully();
                        }
                        /* Spell kind 19 = Scan Monster */
                        if (spell_ptr->effect[i] == 19)
                        {
                                scan_targetting();
                        }
                        /* Spell kind 20 = Reveal Spell */
                        if (spell_ptr->effect[i] == 20)
                        {
                                switch (spell_ptr->shape[i])
                                {
                                        case 1:
                                        {
                                                reveal_spell(px, py, spell_ptr->radius[i] + ((spell_ptr->radius[i] * (p_ptr->skill[26] * 20)) / 100));
                                                update_and_handle();
                                                break;
                                        }
                                        case 2:
                                        {
                                                int ii, ij;
                                                if (!tgt_pt(&ii,&ij)) return;
                                                reveal_spell(ii, ij, spell_ptr->radius[i] + ((spell_ptr->radius[i] * (p_ptr->skill[26] * 20)) / 100));
                                                update_and_handle();
                                                break;
                                        }
                                }
                        }
                        /* Spell kind 21 = Misc boosts */
                        /* Raise something temporarly, such as resistance or AC. */
                        /* The Shape is the duration of the spell. */
                        if (spell_ptr->effect[i] == 21)
                        {
				if (is_alteration(spell_ptr->type[i])) spellpower += multiply_divide(spellpower, (p_ptr->skill[23] * 5), 100);
                                switch(spell_ptr->type[i])
                                {
                                        case 1:
                                                p_ptr->pres = spellpower;
                                                if (p_ptr->pres > 100) p_ptr->pres = 100;
                                                (void)set_pres(spell_ptr->shape[i]);
                                                break;
                                        case 2:
                                                p_ptr->mres = spellpower;
                                                if (p_ptr->mres > 100) p_ptr->mres = 100;
                                                (void)set_mres(spell_ptr->shape[i]);
                                                break;
                                        case 3:
                                                p_ptr->ac_boost = spellpower;
                                                (void)set_ac_boost(spell_ptr->shape[i]);
                                                break;
                                }
                        }
                        /* Spell kind 22 = Conjure Item */
                        if (spell_ptr->effect[i] == 22)
                        {
                                if (i == 0) strcpy(summname, spell_ptr->sspeci1);
                                if (i == 1) strcpy(summname, spell_ptr->sspeci2);
                                if (i == 2) strcpy(summname, spell_ptr->sspeci3);
                                if (i == 3) strcpy(summname, spell_ptr->sspeci4);
                                if (i == 4) strcpy(summname, spell_ptr->sspeci5);
                                switch(spell_ptr->type[i])
                                {
                                        case 1:
                                                conjure_specific_item(summname, spell_ptr->shape[i] + ((spell_ptr->shape[i] * (p_ptr->skill[25] * 10)) / 100), FALSE, FALSE);
                                                break;
                                        case 2:
                                                conjure_specific_item(summname, spell_ptr->shape[i] + ((spell_ptr->shape[i] * (p_ptr->skill[25] * 10)) / 100), TRUE, FALSE);
                                                break;
                                        case 3:
                                                conjure_specific_item(summname, spell_ptr->shape[i] + ((spell_ptr->shape[i] * (p_ptr->skill[25] * 10)) / 100), TRUE, TRUE);
                                                break;
                                }
                        }

                        /* Effect 23 is unused. */

			/* Spell kind 24 = Restore Fate */
                        if (spell_ptr->effect[i] == 24)
                        {
                        	p_ptr->events[29000] = 0;
				p_ptr->events[29001] = 0;
				p_ptr->events[29002] = 0;
				p_ptr->events[29003] = 0;
				p_ptr->events[29004] = 0;
				p_ptr->events[29005] = 0;
				p_ptr->events[29006] = 0;
				p_ptr->events[29007] = 0;
				p_ptr->events[29008] = 0;
				p_ptr->events[29009] = 0;
				p_ptr->events[29010] = 0;
				p_ptr->events[29011] = 0;
				msg_print("Your fate has been restored.");
                        }
			/* Spell kind 25 = Twist Fate: Monsters */
			/* Uses Shape for level, power for depth and type for elites %. */
                        if (spell_ptr->effect[i] == 25)
                        {
                        	p_ptr->events[29000] = spell_ptr->shape[i];
				p_ptr->events[29001] = spell_ptr->power[i];
				p_ptr->events[29002] = spell_ptr->type[i];
				p_ptr->events[29003] = 1;
				msg_print("You have twisted your own fate...");
                        }
			/* Spell kind 26 = Twist Fate: Items */
			/* Uses Shape for level, power for quality, type for rank. %. */
                        if (spell_ptr->effect[i] == 26)
                        {
                        	p_ptr->events[29004] = spell_ptr->shape[i];
				p_ptr->events[29005] = spell_ptr->power[i];
				p_ptr->events[29006] = 1;
				p_ptr->events[29007] = spell_ptr->type[i];
				msg_print("You have twisted your own fate...");
                        }
                	/* Spell kind 27 = Twist Fate: Random Dungeons */
			/* Uses Shape for min depth, power for max depth, type for boss level. %. */
                        if (spell_ptr->effect[i] == 27)
                        {
                        	p_ptr->events[29008] = spell_ptr->shape[i];
				p_ptr->events[29009] = spell_ptr->power[i];
				p_ptr->events[29010] = spell_ptr->type[i];
				p_ptr->events[29011] = 1;
				msg_print("You have twisted your own fate...");
                        }
			/* Spell kind 28 = Alter Position(phase door) */
			/* Power is the radius */
                        if (spell_ptr->effect[i] == 28)
                        {
                        	teleport_player(spellpower);
                        }
                }
                else
                {
                        if (!wisdom) p_ptr->csp -= finalcost;
                        update_and_handle();
                        energy_use = 100;
                        return;
                }
        }

        if (!wisdom) p_ptr->csp -= finalcost;
	wisdom_casting = FALSE;

	call_lua("player_after_magic", "", "");
        update_and_handle();
        energy_use = 100;
        return;
        }
        /* Make a spell */
        else if (ch == 'd' || ch == 'D')
        {
                if (dun_level == 0) delete_spell();
		else msg_print("You can only delete spells in town!");
        }
        else
        {
                if (dun_level == 0) spell_making();
                else msg_print("You can only create spells in town!");
        }
}

/* Allow creation of a spell! :) */
void spell_making()
{
        int i;
        int effectschoose = 0;
        int effects_costs[5];
        s32b finalcost = 0;
        int effbase = 0;
        int effcost = 0;
        int efftype = 0;
        int effnum = 1;
        int rad = 0;
        int power = 0;
        int spelltype = 1;
        s32b manacost = 0;
        int whichslot = 1;
        int effkind = 0;
        int amountslots = 12;
        int redpercent = 0;


	int Slot = -1;
        int num = 0, dir = 0, count;

	int powers[36];
	char power_desc[36][80];

        bool flag, redraw;
        int  ask;

        char choice;
	char out_val[160];




        /* Effect kinds... */
        /* 0. None         */
        /* 1. Attack spell */
        /* --------------- */

        char tmp[80];
        char tmpsumm[80];
	char query;
        char summch;
        char quantitystring[80];
        magic_spells *spell_ptr;

        if (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 3] >= 1)
        {
                amountslots += (1 + (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 3] / 5));
        }

        /*sprintf(quantitystring, "Select a slot to put the spell (1 - %d): ", amountslots);*/

	for (i = 1; i <= amountslots; i++)
        {
                char spellstring[80];
                spell_ptr = &magic_spell[i];
                sprintf(spellstring, "%s   Cost: %ld", spell_ptr->name, spell_ptr->finalcost);        
                /* List the powers */
                strcpy(power_desc[num],spellstring);powers[num++]=i;
        }

        if(!num) {msg_print("No spells available.");return 0;}

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt (accept all spells) */
	if (num <= 26)
	{
		/* Build a prompt (accept all spells) */
                strnfmt(out_val, 78, "(Spells %c-%c, *=List, ESC=exit) Select a spell slot. ",
			I2A(0), I2A(num - 1));
	}
	else
	{
                strnfmt(out_val, 78, "(Spells %c-%c, *=List, ESC=exit) Select a spell slot. ",
			I2A(0), '0' + num - 27);
	}

	/* Get a spell from the user */
	while (!flag && get_com(out_val, &choice))
	{
		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Show the list */
			if (!redraw)
			{
				byte y = 1, x = 0;
				int ctr = 0;
				char dummy[80];

				strcpy(dummy, "");

				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				Term_save();

				prt ("", y++, x);

                                while (ctr < num && ctr < 19)
				{
					sprintf(dummy, "%c) %s", I2A(ctr), power_desc[ctr]);
					prt(dummy, y + ctr, x);
					ctr++;
				}
				while (ctr < num)
				{
					if (ctr < 26)
					{
						sprintf(dummy, " %c) %s", I2A(ctr), power_desc[ctr]);
					}
					else
					{
						sprintf(dummy, " %c) %s", '0' + ctr - 26, power_desc[ctr]);
					}
                                        prt(dummy, y + ctr - 19, x + 40);
					ctr++;
				}
                                if (ctr < 19)
				{
					prt ("", y + ctr, x);
				}
				else
				{
                                        prt ("", y + 19, x);
				}
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

		if (choice == '\r' && num == 1)
		{
			choice = 'a';
		}

		if (isalpha(choice))
		{
			/* Note verify */
			ask = (isupper(choice));

			/* Lowercase */
			if (ask) choice = tolower(choice);

			/* Extract request */
			i = (islower(choice) ? A2I(choice) : -1);
		}
		else
		{
			ask = FALSE; /* Can't uppercase digits */

			i = choice - '0' + 26;
		}

		/* Totally Illegal */
		if ((i < 0) || (i >= num))
		{
			bell();
			continue;
		}

		/* Save the spell index */
		Slot = powers[i];

		/* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Prompt */
			strnfmt(tmp_val, 78, "Select %s? ", power_desc[i]);

			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}

	/* Restore the screen */
	if (redraw) Term_load();

	/* Abort if needed */
	if (!flag) 
	{
                return;
	}


        /*whichslot = get_quantity(quantitystring, amountslots);*/
	whichslot = Slot;
        spell_ptr = &magic_spell[whichslot];

	/* Save what's on screen */
        Term_save();

	/* Flush messages */
	msg_print(NULL);

        /* Begin... */
	Term_erase(0, 1, 255);

        /* Prepare the screen */
        for (i = 0; i < SCREEN_HGT; i++)
        {
                roff("\n");
        }
        c_put_str(TERM_WHITE, "PORTRALIS SPELL CREATION", 0, 0);
        c_put_str(TERM_WHITE, "--------------------------", 1, 0);



        /* No final cost yet */
        spell_ptr->finalcost = 0;
        for (i = 0;i < 5; i++)
        {
                spell_ptr->effect[i] = 0;
                spell_ptr->manacost[i] = 0;
		spell_ptr->school[i] = 0;
    		spell_ptr->shape[i] = 0;
    		spell_ptr->power[i] = 0;
    		spell_ptr->radius[i] = 0;
    		spell_ptr->type[i] = 0;
        }
        /* Not created yet...or deleted existing one. */
	sprintf(spell_ptr->name, "--- NO SPELL ---");
        spell_ptr->created = TRUE;

        /* We repeat the process for each effects! */
        while (effectschoose < 5)
        {
                /* Prepare the screen */
                for (i = 0; i < SCREEN_HGT; i++)
                {
                        roff("\n");
                }
                c_put_str(TERM_WHITE, "PORTRALIS SPELL CREATION", 0, 0);
        	c_put_str(TERM_WHITE, "--------------------------", 1, 0);
                
                /* First, select the school. */
                c_put_str(TERM_WHITE, "Select a spell school:", 4, 0);
                c_put_str(TERM_L_RED, "[1] Elemental", 7, 0);
                c_put_str(TERM_VIOLET, "[2] Alteration", 9, 0);
                c_put_str(TERM_L_BLUE, "[3] Mysticism", 11, 0);
                c_put_str(TERM_YELLOW, "[4] Conjuration", 13, 0);
                c_put_str(TERM_WHITE, "[5] Divination", 15, 0);

                /* End of scanning */
                query = inkey();

                if (query == '1')
                {
                        bool chosen = FALSE;
                        bool readytocreate = FALSE;
                        char choseneffect[30];                        
                        char choice;

                        /* Initialise some stuff */
                        effbase = 0;
                        effcost = 0;
                        efftype = 0;
                        rad = 0;
                        power = 0;
                        spelltype = 1;
                        manacost = 0;

                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "PORTRALIS SPELL CREATION", 0, 0);
        		c_put_str(TERM_WHITE, "--------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Select an effect:", 4, 0);

                        /* All right, let's prepare the list! :) */
                        if (p_ptr->elemental_effects & ELEM_MISSILE) c_put_str(TERM_WHITE, "[0] Missile", 7, 0);
                        else c_put_str(TERM_L_DARK, "[0] Missile", 7, 0);
                        if (p_ptr->elemental_effects & ELEM_FIRE) c_put_str(TERM_WHITE, "[1] Fire", 8, 0);
                        else c_put_str(TERM_L_DARK, "[1] Fire", 8, 0);
                        if (p_ptr->elemental_effects & ELEM_COLD) c_put_str(TERM_WHITE, "[2] Cold", 9, 0);
                        else c_put_str(TERM_L_DARK, "[2] Cold", 9, 0);
                        if (p_ptr->elemental_effects & ELEM_ELEC) c_put_str(TERM_WHITE, "[3] Electricity", 10, 0);
                        else c_put_str(TERM_L_DARK, "[3] Electricity", 10, 0);
                        if (p_ptr->elemental_effects & ELEM_ACID) c_put_str(TERM_WHITE, "[4] Acid", 11, 0);
                        else c_put_str(TERM_L_DARK, "[4] Acid", 11, 0);
                        if (p_ptr->elemental_effects & ELEM_POIS) c_put_str(TERM_WHITE, "[5] Poison", 12, 0);
                        else c_put_str(TERM_L_DARK, "[5] Poison", 12, 0);
                        if (p_ptr->elemental_effects & ELEM_LITE) c_put_str(TERM_WHITE, "[6] Light", 13, 0);
                        else c_put_str(TERM_L_DARK, "[6] Light", 13, 0);
                        if (p_ptr->elemental_effects & ELEM_DARK) c_put_str(TERM_WHITE, "[7] Darkness", 14, 0);
                        else c_put_str(TERM_L_DARK, "[7] Darkness", 14, 0);
                        if (p_ptr->elemental_effects & ELEM_PHYSICAL) c_put_str(TERM_WHITE, "[8] Physical", 15, 0);
                        else c_put_str(TERM_L_DARK, "[8] Physical", 15, 0);
                        if (p_ptr->elemental_effects & ELEM_RADIO) c_put_str(TERM_WHITE, "[9] Radio", 16, 0);
                        else c_put_str(TERM_L_DARK, "[9] Radio", 16, 0);
                        if (p_ptr->elemental_effects & ELEM_WATER) c_put_str(TERM_WHITE, "[a] Water", 17, 0);
                        else c_put_str(TERM_L_DARK, "[a] Water", 17, 0);
                        if (p_ptr->elemental_effects & ELEM_CHAOS) c_put_str(TERM_WHITE, "[b] Chaos", 18, 0);
                        else c_put_str(TERM_L_DARK, "[b] Chaos", 18, 0);
                        if (p_ptr->elemental_effects & ELEM_EARTH) c_put_str(TERM_WHITE, "[c] Earth", 19, 0);
                        else c_put_str(TERM_L_DARK, "[c] Earth", 19, 0);
                        if (p_ptr->elemental_effects & ELEM_SOUND) c_put_str(TERM_WHITE, "[d] Sound", 7, 20);
                        else c_put_str(TERM_L_DARK, "[d] Sound", 7, 20);
                        if (p_ptr->elemental_effects & ELEM_WARP) c_put_str(TERM_WHITE, "[e] Warp", 8, 20);
                        else c_put_str(TERM_L_DARK, "[e] Warp", 8, 20);
                        if (p_ptr->elemental_effects & ELEM_WIND) c_put_str(TERM_WHITE, "[f] Wind", 9, 20);
                        else c_put_str(TERM_L_DARK, "[f] Wind", 9, 20);
                        if (p_ptr->elemental_effects & ELEM_MANA) c_put_str(TERM_WHITE, "[g] Mana", 10, 20);
                        else c_put_str(TERM_L_DARK, "[g] Mana", 10, 20);
			if (p_ptr->elemlord != 0)
			{
				char elemname[80];
				sprintf(elemname, "[z] %s", get_element_name(p_ptr->elemlord));
				c_put_str(TERM_WHITE, elemname, 19, 20);
                        }

                        while (chosen == FALSE)
                        {
                                choice = inkey();
                                if (choice == '0')
                                {
                                        if (!(p_ptr->elemental_effects & ELEM_MISSILE)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_MISSILE;
                                                effbase = 0;
                                                effcost = 9;
                                                effkind = 1;
                                                sprintf(choseneffect, "Missile");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '1')
                                {
                                        if (!(p_ptr->elemental_effects & ELEM_FIRE)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_FIRE;
                                                effbase = 0;
                                                effcost = 3;
                                                effkind = 1;
                                                sprintf(choseneffect, "Fire");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '2')
                                {
                                        if (!(p_ptr->elemental_effects & ELEM_COLD)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_COLD;
                                                effbase = 0;
                                                effcost = 3;
                                                effkind = 1;
                                                sprintf(choseneffect, "Cold");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '3')
                                {
                                        if (!(p_ptr->elemental_effects & ELEM_ELEC)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_ELEC;
                                                effbase = 0;
                                                effcost = 3;
                                                effkind = 1;
                                                sprintf(choseneffect, "Electricity");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '4')
                                {
                                        if (!(p_ptr->elemental_effects & ELEM_ACID)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_ACID;
                                                effbase = 0;
                                                effcost = 3;
                                                effkind = 1;
                                                sprintf(choseneffect, "Acid");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '5')
                                {
                                        if (!(p_ptr->elemental_effects & ELEM_POIS)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_POIS;
                                                effbase = 0;
                                                effcost = 3;
                                                effkind = 1;
                                                sprintf(choseneffect, "Poison");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '6')
                                {
                                        if (!(p_ptr->elemental_effects & ELEM_LITE)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_LITE;
                                                effbase = 0;
                                                effcost = 3;
                                                effkind = 1;
                                                sprintf(choseneffect, "Light");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '7')
                                {
                                        if (!(p_ptr->elemental_effects & ELEM_DARK)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_DARK;
                                                effbase = 0;
                                                effcost = 3;
                                                effkind = 1;
                                                sprintf(choseneffect, "Darkness");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '8')
                                {
                                        if (!(p_ptr->elemental_effects & ELEM_PHYSICAL)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_PHYSICAL;
                                                effbase = 0;
                                                effcost = 3;
                                                effkind = 1;
                                                sprintf(choseneffect, "Physical");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '9')
                                {
                                        if (!(p_ptr->elemental_effects & ELEM_RADIO)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_RADIO;
                                                effbase = 0;
                                                effcost = 3;
                                                effkind = 1;
                                                sprintf(choseneffect, "Radio");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'a')
                                {
                                        if (!(p_ptr->elemental_effects & ELEM_WATER)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_WATER;
                                                effbase = 0;
                                                effcost = 3;
                                                effkind = 1;
                                                sprintf(choseneffect, "Water");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'b')
                                {
                                        if (!(p_ptr->elemental_effects & ELEM_CHAOS)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_CHAOS;
                                                effbase = 0;
                                                effcost = 3;
                                                effkind = 1;
                                                sprintf(choseneffect, "Chaos");
                                                chosen = TRUE;
                                        }
                                }                                
                                else if (choice == 'c')
                                {
                                        if (!(p_ptr->elemental_effects & ELEM_EARTH)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_EARTH;
                                                effbase = 0;
                                                effcost = 3;
                                                effkind = 1;
                                                sprintf(choseneffect, "Earth");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'd')
                                {
                                        if (!(p_ptr->elemental_effects & ELEM_SOUND)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_SOUND;
                                                effbase = 0;
                                                effcost = 3;
                                                effkind = 1;
                                                sprintf(choseneffect, "Sound");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'e')
                                {
                                        if (!(p_ptr->elemental_effects & ELEM_WARP)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_WARP;
                                                effbase = 0;
                                                effcost = 3;
                                                effkind = 1;
                                                sprintf(choseneffect, "Warp");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'f')
                                {
                                        if (!(p_ptr->elemental_effects & ELEM_WIND)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_WIND;
                                                effbase = 0;
                                                effcost = 3;
                                                effkind = 1;
                                                sprintf(choseneffect, "Wind");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'g')
                                {
                                        if (!(p_ptr->elemental_effects & ELEM_MANA)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_MANA;
                                                effbase = 0;
                                                effcost = 3;
                                                effkind = 1;
                                                sprintf(choseneffect, "Mana");
                                                chosen = TRUE;
                                        }
                                }
				else if (choice == 'z' && p_ptr->elemlord != 0)
                                {
                                	efftype = p_ptr->elemlord;
                                        effbase = 0;
                                        effcost = 3;
                                        effkind = 1;
                                        sprintf(choseneffect, get_element_name(p_ptr->elemlord));
                                        chosen = TRUE;
                                }
                               
                                else
                                {
                                        Term_load();
                                        return;
                                }
                        }
                        /* We've chosen AND prepared the effect! :) */
                        /* Let's create the actual spell now! :) */
                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "PORTRALIS SPELL CREATION", 0, 0);
        		c_put_str(TERM_WHITE, "--------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Selected effect:", 4, 0);
                        c_put_str(TERM_WHITE, choseneffect, 4, 17);

                        while (readytocreate == FALSE)
                        {
                                c_put_str(TERM_WHITE, "Power:", 6, 0);
                                sprintf(tmp, "%d          ", power);
                                c_put_str(TERM_L_GREEN, tmp, 6, 7);
                                c_put_str(TERM_WHITE, "Radius:", 8, 0);
                                sprintf(tmp, "%d          ", rad);
                                c_put_str(TERM_L_GREEN, tmp, 8, 8);
                                c_put_str(TERM_WHITE, "Type:", 10, 0);
                                if (spelltype == 1) sprintf(tmp, "Bolt         ");
                                else if (spelltype == 2) sprintf(tmp, "Ball         ");
                                else if (spelltype == 3) sprintf(tmp, "Circle       ");
                                else if (spelltype == 4) sprintf(tmp, "Chain        ");
				else if (spelltype == 5) sprintf(tmp, "Self         ");
                                c_put_str(TERM_L_GREEN, tmp, 10, 6);
                                c_put_str(TERM_WHITE, "Mana Cost:", 12, 0);
                                if (spelltype != 5 && spelltype != 6) manacost = (effbase + ((effcost * power) / 2));
                                else
				{
					if ((power / 2) == 0) manacost = (effbase + (effcost * power));
					else manacost = (effbase + (effcost * power)) * (power / 2);
				}
                                manacost += ((manacost * (power * 3)) / 100);
                                manacost = manacost * (rad + 1);
                                if (spelltype == 4)
                                {
                                        int a;
                                        manacost *= 5;
                                        for (a = 0; a < rad; a++)
                                        {
                                                manacost += (50 * rad);
                                        }                                        
                                }
                                if (spelltype == 5 || spelltype == 6)
                                {
                                        manacost *= 40;
                                        if (rad > 0) manacost *= rad;
                                }
				/* Magic Missiles cost reduction */
				if (efftype == GF_MISSILE && p_ptr->abilities[(CLASS_MAGE * 10) + 2] >= 1)
				{
					redpercent = 6 + (p_ptr->abilities[(CLASS_MAGE * 10) + 2] * 3);
					if (redpercent > 67) redpercent = 67;
					manacost -= multiply_divide(manacost, redpercent, 100);
				}
                                if (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 6] >= 1)
                                {
                                        redpercent = p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 6];
                                        if (redpercent > 75) redpercent = 75;
                                        manacost -= ((manacost * redpercent) / 100);
                                        manacost -= (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 6] * 3);
                                        if (manacost < 0) manacost = 0;
                                }
				
				/* Mastery of Elemental spells! */
				if (p_ptr->skill_base[22] >= 100) manacost = (manacost / 2);
				else if (p_ptr->skill_base[22] >= 50) manacost = (manacost - (manacost / 3));
				else if (p_ptr->skill_base[22] >= 25) manacost = (manacost - (manacost / 4));

				c_put_str(TERM_WHITE, "Melee/Ranged Attack does 10% of regular damages for every power points.", 14, 0);

                                sprintf(tmp, "%ld          ", manacost);
                                c_put_str(TERM_L_GREEN, tmp, 12, 11);
                                c_put_str(TERM_WHITE, "[q/w] to increase/decrease power,", 16, 0);
                                c_put_str(TERM_WHITE, "[a/s] to increase/decrease radius,", 17, 0);
                                c_put_str(TERM_WHITE, "[t] to change type,", 18, 0);
                                c_put_str(TERM_WHITE, "[c] to complete creation,", 19, 0);
                                c_put_str(TERM_WHITE, "[ESC] to exit.", 20, 0);
                                choice = inkey();
                                if (choice == 'q' || choice == 'Q') power += 1;
                                if (choice == 'w' || choice == 'W')
                                {
                                        if (power > 0) power -= 1;
                                }
                                if (choice == 'a' || choice == 'A')
                                {
                                        rad += 1;
                                        if (spelltype == 1) spelltype = 2;
                                }
                                if (choice == 's' || choice == 'S')
                                {
                                        if (rad > 0) rad -= 1;
                                        if (spelltype == 2 && rad == 0) spelltype = 1;
                                }
                                if (choice == 't' || choice == 'T')
                                {
                                        spelltype += 1;
                                        if (spelltype > 4) spelltype = 1;
                                        if (spelltype == 1 && rad > 0) spelltype += 1;
                                        if (spelltype == 1) rad = 0;
                                        if (spelltype == 2 && rad == 0) spelltype += 1;
                                }
                                if (choice == 'c' || choice == 'C') readytocreate = TRUE;
                                if (choice == 'x' || choice == 'X' || choice == ESCAPE)
                                {
                                        Term_load();
                                        return;
                                }
                        }

                }
                /* Alterations! */
                else if (query == '2')
                {
                        bool chosen = FALSE;
                        bool readytocreate = FALSE;
                        char choseneffect[30];                        
                        char choice;
                        int interfacetype;

                        /* Initialise some stuff */
                        effbase = 0;
                        effcost = 0;
                        efftype = 0;
                        rad = 0;
                        power = 0;
                        spelltype = 1;
                        manacost = 0;

                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "PORTRALIS SPELL CREATION", 0, 0);
        		c_put_str(TERM_WHITE, "--------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Select an effect:", 4, 0);

                        /* All right, let's prepare the list! :) */
                        if (p_ptr->alteration_effects & ALTER_REDUCE_HIT) c_put_str(TERM_WHITE, "[0] Reduce Hit Rate", 7, 0);
                        else c_put_str(TERM_L_DARK, "[0] Reduce Hit Rate", 7, 0);
                        if (p_ptr->alteration_effects & ALTER_REDUCE_DEF) c_put_str(TERM_WHITE, "[1] Reduce Defense", 8, 0);
                        else c_put_str(TERM_L_DARK, "[1] Reduce Defense", 8, 0);
                        if (p_ptr->alteration_effects & ALTER_REDUCE_SPEED) c_put_str(TERM_WHITE, "[2] Reduce Speed", 9, 0);
                        else c_put_str(TERM_L_DARK, "[2] Reduce Speed", 9, 0);
                        if (p_ptr->alteration_effects & ALTER_REDUCE_LEVEL) c_put_str(TERM_WHITE, "[3] Reduce Level", 10, 0);
                        else c_put_str(TERM_L_DARK, "[3] Reduce Level", 10, 0);
                        if (p_ptr->alteration_effects & ALTER_LIFE_BLAST) c_put_str(TERM_WHITE, "[4] Life Blast", 11, 0);
                        else c_put_str(TERM_L_DARK, "[4] Life Blast", 11, 0);
                        if (p_ptr->alteration_effects & ALTER_LOCK) c_put_str(TERM_WHITE, "[5] Lock", 12, 0);
                        else c_put_str(TERM_L_DARK, "[5] Lock", 12, 0);
                        if (p_ptr->alteration_effects & ALTER_HALVE_POWER) c_put_str(TERM_WHITE, "[6] Halve Power", 13, 0);
                        else c_put_str(TERM_L_DARK, "[6] Halve Power", 13, 0);
                        if (p_ptr->alteration_effects & ALTER_HALVE_MAGIC) c_put_str(TERM_WHITE, "[7] Halve Magic", 14, 0);
                        else c_put_str(TERM_L_DARK, "[7] Halve Magic", 14, 0);
                        if (p_ptr->alteration_effects & ALTER_STONE_TO_MUD) c_put_str(TERM_WHITE, "[8] Stone to Mud", 15, 0);
                        else c_put_str(TERM_L_DARK, "[8] Stone to Mud", 15, 0);
                        if (p_ptr->alteration_effects & ALTER_DEMORALIZE) c_put_str(TERM_WHITE, "[9] Demoralize", 16, 0);
                        else c_put_str(TERM_L_DARK, "[9] Demoralize", 16, 0);
                        if (p_ptr->alteration_effects & ALTER_RETROGRADE) c_put_str(TERM_WHITE, "[a] Retrograde", 17, 0);
                        else c_put_str(TERM_L_DARK, "[a] Retrograde", 17, 0);
                        if (p_ptr->alteration_effects & ALTER_EVOLVE) c_put_str(TERM_WHITE, "[b] Evolve", 18, 0);
                        else c_put_str(TERM_L_DARK, "[b] Evolve", 18, 0);
                        if (p_ptr->alteration_effects & ALTER_UNEVOLVE) c_put_str(TERM_WHITE, "[c] Un-Evolve", 19, 0);
                        else c_put_str(TERM_L_DARK, "[c] Un-Evolve", 19, 0);
                        if (p_ptr->alteration_effects & ALTER_HASTE) c_put_str(TERM_WHITE, "[d] Haste", 7, 20);
                        else c_put_str(TERM_L_DARK, "[d] Haste", 7, 20);
                        if (p_ptr->alteration_effects & ALTER_RAISE_STR) c_put_str(TERM_WHITE, "[e] Raise Strength", 8, 20);
                        else c_put_str(TERM_L_DARK, "[e] Raise Strength", 8, 20);
                        if (p_ptr->alteration_effects & ALTER_RAISE_INT) c_put_str(TERM_WHITE, "[f] Raise Intelligence", 9, 20);
                        else c_put_str(TERM_L_DARK, "[f] Raise Intelligence", 9, 20);
                        if (p_ptr->alteration_effects & ALTER_RAISE_WIS) c_put_str(TERM_WHITE, "[g] Raise Wisdom", 10, 20);
                        else c_put_str(TERM_L_DARK, "[g] Raise Wisdom", 10, 20);
                        if (p_ptr->alteration_effects & ALTER_RAISE_DEX) c_put_str(TERM_WHITE, "[h] Raise Dexterity", 11, 20);
                        else c_put_str(TERM_L_DARK, "[h] Raise Dexterity", 11, 20);
                        if (p_ptr->alteration_effects & ALTER_RAISE_CON) c_put_str(TERM_WHITE, "[i] Raise Constitution", 12, 20);
                        else c_put_str(TERM_L_DARK, "[i] Raise Constitution", 12, 20);
                        if (p_ptr->alteration_effects & ALTER_RAISE_CHR) c_put_str(TERM_WHITE, "[j] Raise Charisma", 13, 20);
                        else c_put_str(TERM_L_DARK, "[j] Raise Charisma", 13, 20);
                        if (p_ptr->alteration_effects & ALTER_POSITION) c_put_str(TERM_WHITE, "[k] Alter Position", 14, 20);
                        else c_put_str(TERM_L_DARK, "[k] Alter Position", 14, 20);
                        if (p_ptr->alteration_effects & ALTER_HASTE_OTHER) c_put_str(TERM_WHITE, "[l] Haste Other", 15, 20);
                        else c_put_str(TERM_L_DARK, "[l] Haste Other", 15, 20);
                        if (p_ptr->alteration_effects & ALTER_PHYS_RESIST) c_put_str(TERM_WHITE, "[m] Physical Resistance", 16, 20);
                        else c_put_str(TERM_L_DARK, "[m] Physical Resistance", 16, 20);
                        if (p_ptr->alteration_effects & ALTER_MAGIC_RESIST) c_put_str(TERM_WHITE, "[n] Magic Resistance", 17, 20);
                        else c_put_str(TERM_L_DARK, "[n] Magic Resistance", 17, 20);
                        if (p_ptr->alteration_effects & ALTER_STONESKIN) c_put_str(TERM_WHITE, "[o] Stoneskin", 18, 20);
                        else c_put_str(TERM_L_DARK, "[o] Stoneskin", 18, 20);
                        if (p_ptr->alteration_effects & ALTER_PARALYZE) c_put_str(TERM_WHITE, "[p] Paralyze", 19, 20);
                        else c_put_str(TERM_L_DARK, "[p] Paralyze", 19, 20);


                        while (chosen == FALSE)
                        {
                                choice = inkey();
                                if (choice == '0')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_REDUCE_HIT)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_REDUCE_HIT;
                                                effbase = 0;
                                                effcost = 3;
                                                effkind = 2;
                                                interfacetype = 1;
                                                sprintf(choseneffect, "Reduce Hit Rate");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '1')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_REDUCE_DEF)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_REDUCE_DEF;
                                                effbase = 0;
                                                effcost = 1;
                                                effkind = 2;
                                                interfacetype = 1;
                                                sprintf(choseneffect, "Reduce Defense");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '2')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_REDUCE_SPEED)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_REDUCE_SPEED;
                                                effbase = 0;
                                                effcost = 100;
                                                effkind = 2;
                                                interfacetype = 1;
                                                sprintf(choseneffect, "Reduce Speed");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '3')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_REDUCE_LEVEL)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_WEAKEN;
                                                effbase = 0;
                                                effcost = 35;
                                                effkind = 2;
                                                interfacetype = 1;
                                                sprintf(choseneffect, "Reduce Level");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '4')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_LIFE_BLAST)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_LIFE_BLAST;
                                                effbase = 0;
                                                effcost = 1;
                                                effkind = 2;
                                                interfacetype = 6;
                                                sprintf(choseneffect, "Life Blast");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '5')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_LOCK)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_LOCK;
                                                effbase = 250;
                                                effcost = 0;
                                                effkind = 2;
                                                interfacetype = 2;
                                                sprintf(choseneffect, "Lock");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '6')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_HALVE_POWER)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_HALVE_DAMAGES;
                                                effbase = 80;
                                                effcost = 0;
                                                effkind = 2;
                                                interfacetype = 2;
                                                sprintf(choseneffect, "Halve Power");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '7')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_HALVE_MAGIC)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_HALVE_MAGIC;
                                                effbase = 80;
                                                effcost = 0;
                                                effkind = 2;
                                                interfacetype = 2;
                                                sprintf(choseneffect, "Halve Magic");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '8')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_STONE_TO_MUD)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_STONE_TO_MUD;
                                                effbase = 100;
                                                effcost = 1;
                                                effkind = 1;
                                                interfacetype = 1;
                                                sprintf(choseneffect, "Stone to Mud");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '9')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_DEMORALIZE)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_FEAR_CURSE;
                                                effbase = 0;
                                                effcost = 12;
                                                effkind = 2;
                                                interfacetype = 1;
                                                sprintf(choseneffect, "Demoralize");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'a')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_RETROGRADE)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_RETROGRADE;
                                                effbase = 1000;
                                                effcost = 0;
                                                effkind = 2;
                                                interfacetype = 2;
                                                sprintf(choseneffect, "Retrograde");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'b')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_EVOLVE)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_EVOLVE;
                                                effbase = 50;
                                                effcost = 0;
                                                effkind = 2;
                                                interfacetype = 2;
                                                sprintf(choseneffect, "Evolve");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'c')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_UNEVOLVE)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_UNEVOLVE;
                                                effbase = 100;
                                                effcost = 0;
                                                effkind = 2;
                                                interfacetype = 2;
                                                sprintf(choseneffect, "Un-Evolve");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'd')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_HASTE)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 0;
                                                effbase = 0;
                                                effcost = 3;
                                                effkind = 3;
                                                interfacetype = 3;
                                                sprintf(choseneffect, "Haste");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'e')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_RAISE_STR)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 1;
                                                effbase = 0;
                                                effcost = 5;
                                                effkind = 4;
                                                interfacetype = 4;
                                                sprintf(choseneffect, "Raise Strength");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'f')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_RAISE_INT)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 2;
                                                effbase = 0;
                                                effcost = 5;
                                                effkind = 4;
                                                interfacetype = 4;
                                                sprintf(choseneffect, "Raise Intelligence");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'g')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_RAISE_WIS)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 3;
                                                effbase = 0;
                                                effcost = 5;
                                                effkind = 4;
                                                interfacetype = 4;
                                                sprintf(choseneffect, "Raise Wisdom");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'h')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_RAISE_DEX)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 4;
                                                effbase = 0;
                                                effcost = 5;
                                                effkind = 4;
                                                interfacetype = 4;
                                                sprintf(choseneffect, "Raise Dexterity");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'i')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_RAISE_CON)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 5;
                                                effbase = 0;
                                                effcost = 5;
                                                effkind = 4;
                                                interfacetype = 4;
                                                sprintf(choseneffect, "Raise Constitution");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'j')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_RAISE_CHR)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 6;
                                                effbase = 0;
                                                effcost = 5;
                                                effkind = 4;
                                                interfacetype = 4;
                                                sprintf(choseneffect, "Raise Charisma");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'k')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_POSITION)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 0;
                                                effbase = 150;
                                                effcost = 10;
                                                effkind = 28;
                                                interfacetype = 3;
                                                sprintf(choseneffect, "Alter Position");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'l')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_HASTE_OTHER)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_OLD_SPEED;
                                                effbase = 30;
                                                effcost = 0;
                                                effkind = 2;
                                                interfacetype = 2;
                                                sprintf(choseneffect, "Haste Other");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'm')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_PHYS_RESIST)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 1;
                                                effbase = 0;
                                                effcost = 1;
                                                effkind = 21;
                                                interfacetype = 5;
                                                sprintf(choseneffect, "Physical Resistance");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'n')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_MAGIC_RESIST)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 2;
                                                effbase = 0;
                                                effcost = 1;
                                                effkind = 21;
                                                interfacetype = 5;
                                                sprintf(choseneffect, "Magic Resistance");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'o')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_STONESKIN)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 3;
                                                effbase = 0;
                                                effcost = 1;
                                                effkind = 21;
                                                interfacetype = 4;
                                                sprintf(choseneffect, "Stoneskin");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'p')
                                {
                                        if (!(p_ptr->alteration_effects & ALTER_PARALYZE)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_PARALYZE;
                                                effbase = 0;
                                                effcost = 100;
                                                effkind = 2;
                                                interfacetype = 1;
                                                sprintf(choseneffect, "Paralyze");
                                                chosen = TRUE;
                                        }
                                }
                               
                                else
                                {
                                        Term_load();
                                        return;
                                }
                        }
                        /* Choose the appropriate type of interface */
                        /* Depending on the spell. */

                        /* Standard damages interface */
                        if (interfacetype == 1)
                        {

                        /* We've chosen AND prepared the effect! :) */
                        /* Let's create the actual spell now! :) */
                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "PORTRALIS SPELL CREATION", 0, 0);
        		c_put_str(TERM_WHITE, "--------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Selected effect:", 4, 0);
                        c_put_str(TERM_WHITE, choseneffect, 4, 17);

                        while (readytocreate == FALSE)
                        {
                                c_put_str(TERM_WHITE, "Power:", 6, 0);
                                sprintf(tmp, "%d          ", power);
                                c_put_str(TERM_L_GREEN, tmp, 6, 7);
                                c_put_str(TERM_WHITE, "Radius:", 8, 0);
                                sprintf(tmp, "%d          ", rad);
                                c_put_str(TERM_L_GREEN, tmp, 8, 8);
                                c_put_str(TERM_WHITE, "Type:", 10, 0);
                                if (spelltype == 1) sprintf(tmp, "Bolt  ");
                                else if (spelltype == 2) sprintf(tmp, "Ball  ");
                                else if (spelltype == 3) sprintf(tmp, "Circle");
                                else if (spelltype == 4) sprintf(tmp, "Chain ");
				else if (spelltype == 5) sprintf(tmp, "Self  ");
                                c_put_str(TERM_L_GREEN, tmp, 10, 6);
                                c_put_str(TERM_WHITE, "Mana Cost:", 12, 0);
                                manacost = (effbase + ((effcost * power))) * (rad + 1);
                                if (spelltype == 4)
                                {
                                        int a;
                                        manacost *= 5;
                                        for (a = 0; a < rad; a++)
                                        {
                                                manacost += (50 * rad);
                                        }                                        
                                }
                                if (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 7] >= 1)
                                {
                                        redpercent = p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 7];
                                        if (redpercent > 75) redpercent = 75;
                                        manacost -= ((manacost * redpercent) / 100);
                                        manacost -= (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 7] * 3);
                                        if (manacost < 0) manacost = 0;
                                }
				
				/* Mastery of Alteration spells! */
				if (p_ptr->skill_base[23] >= 100) manacost = (manacost / 2);
				else if (p_ptr->skill_base[23] >= 50) manacost = (manacost - (manacost / 3));
				else if (p_ptr->skill_base[23] >= 25) manacost = (manacost - (manacost / 4));

                                sprintf(tmp, "%ld          ", manacost);
                                c_put_str(TERM_L_GREEN, tmp, 12, 11);
                                c_put_str(TERM_WHITE, "[q/w] to increase/decrease power,", 16, 0);
                                c_put_str(TERM_WHITE, "[a/s] to increase/decrease radius,", 17, 0);
                                c_put_str(TERM_WHITE, "[t] to change type,", 18, 0);
                                c_put_str(TERM_WHITE, "[c] to complete creation,", 19, 0);
                                c_put_str(TERM_WHITE, "[ESC] to exit.", 20, 0);
                                choice = inkey();
                                if (choice == 'q' || choice == 'Q') power += 1;
                                if (choice == 'w' || choice == 'W')
                                {
                                        if (power > 0) power -= 1;
                                }
                                if (choice == 'a' || choice == 'A')
                                {
                                        rad += 1;
                                        if (spelltype == 1) spelltype = 2;
                                }
                                if (choice == 's' || choice == 'S')
                                {
                                        if (rad > 0) rad -= 1;
                                        if (spelltype == 2 && rad == 0) spelltype = 1;
                                }
                                if (choice == 't' || choice == 'T')
                                {
                                        spelltype += 1;
                                        if (spelltype > 4) spelltype = 1;
                                        if (spelltype == 1 && rad > 0) spelltype += 1;
                                        if (spelltype == 1) rad = 0;
                                        if (spelltype == 2 && rad == 0) spelltype += 1;
                                }
                                if (choice == 'c' || choice == 'C') readytocreate = TRUE;
                                if (choice == 'x' || choice == 'X' || choice == ESCAPE)
                                {
                                        Term_load();
                                        return;
                                }
                        }
                        }
                        /* 2. Powerless damages interface */
                        if (interfacetype == 2)
                        {

                        /* We've chosen AND prepared the effect! :) */
                        /* Let's create the actual spell now! :) */
                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "PORTRALIS SPELL CREATION", 0, 0);
        		c_put_str(TERM_WHITE, "--------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Selected effect:", 4, 0);
                        c_put_str(TERM_WHITE, choseneffect, 4, 17);

                        while (readytocreate == FALSE)
                        {
                                power = 100;
                                c_put_str(TERM_WHITE, "Radius:", 6, 0);
                                sprintf(tmp, "%d          ", rad);
                                c_put_str(TERM_L_GREEN, tmp, 6, 8);
                                c_put_str(TERM_WHITE, "Type:", 8, 0);
                                if (spelltype == 1) sprintf(tmp, "Bolt  ");
                                else if (spelltype == 2) sprintf(tmp, "Ball  ");
                                else if (spelltype == 3) sprintf(tmp, "Circle");
                                else if (spelltype == 4) sprintf(tmp, "Chain ");
				else if (spelltype == 5) sprintf(tmp, "Self  ");
                                c_put_str(TERM_L_GREEN, tmp, 8, 6);
                                c_put_str(TERM_WHITE, "Mana Cost:", 10, 0);
                                manacost = (effbase + ((effcost * power))) * (rad + 1);
                                if (spelltype == 4)
                                {
                                        int a;
                                        manacost *= 5;
                                        for (a = 0; a < rad; a++)
                                        {
                                                manacost += (50 * rad);
                                        }                                        
                                }
                                if (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 7] >= 1)
                                {
                                        redpercent = p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 7];
                                        if (redpercent > 75) redpercent = 75;
                                        manacost -= ((manacost * redpercent) / 100);
                                        manacost -= (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 7] * 3);
                                        if (manacost < 0) manacost = 0;
                                }
				
				/* Mastery of Alteration spells! */
				if (p_ptr->skill_base[23] >= 100) manacost = (manacost / 2);
				else if (p_ptr->skill_base[23] >= 50) manacost = (manacost - (manacost / 3));
				else if (p_ptr->skill_base[23] >= 25) manacost = (manacost - (manacost / 4));

                                sprintf(tmp, "%ld          ", manacost);
                                c_put_str(TERM_L_GREEN, tmp, 10, 11);
                                c_put_str(TERM_WHITE, "[a/s] to increase/decrease radius,", 17, 0);
                                c_put_str(TERM_WHITE, "[t] to change type,", 18, 0);
                                c_put_str(TERM_WHITE, "[c] to complete creation,", 19, 0);
                                c_put_str(TERM_WHITE, "[ESC] to exit.", 20, 0);
                                choice = inkey();
                                if (choice == 'a' || choice == 'A')
                                {
                                        rad += 1;
                                        if (spelltype == 1) spelltype = 2;
                                }
                                if (choice == 's' || choice == 'S')
                                {
                                        if (rad > 0) rad -= 1;
                                        if (spelltype == 2 && rad == 0) spelltype = 1;
                                }
                                if (choice == 't' || choice == 'T')
                                {
                                        spelltype += 1;
                                        if (spelltype > 4) spelltype = 1;
                                        if (spelltype == 1 && rad > 0) spelltype += 1;
                                        if (spelltype == 1) rad = 0;
                                        if (spelltype == 2 && rad == 0) spelltype += 1;
                                }
                                if (choice == 'c' || choice == 'C') readytocreate = TRUE;
                                if (choice == 'x' || choice == 'X' || choice == ESCAPE)
                                {
                                        Term_load();
                                        return;
                                }
                        }
                        }
                        /* 3. Power only interface */
                        /* Used for Haste. Here, the power */
                        /* is the duration. */
                        if (interfacetype == 3)
                        {

                        /* We've chosen AND prepared the effect! :) */
                        /* Let's create the actual spell now! :) */
                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "PORTRALIS SPELL CREATION", 0, 0);
        		c_put_str(TERM_WHITE, "--------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Selected effect:", 4, 0);
                        c_put_str(TERM_WHITE, choseneffect, 4, 17);

                        while (readytocreate == FALSE)
                        {
                                c_put_str(TERM_WHITE, "Duration:", 6, 0);
                                sprintf(tmp, "%d          ", power);
                                c_put_str(TERM_L_GREEN, tmp, 6, 10);
                                c_put_str(TERM_WHITE, "Mana Cost:", 8, 0);
                                manacost = (effbase + ((effcost * power))) * (rad + 1);
                                if (spelltype == 4) manacost *= 5;
                                if (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 7] >= 1)
                                {
                                        redpercent = p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 7];
                                        if (redpercent > 75) redpercent = 75;
                                        manacost -= ((manacost * redpercent) / 100);
                                        manacost -= (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 7] * 3);
                                        if (manacost < 0) manacost = 0;
                                }
				
				/* Mastery of Alteration spells! */
				if (p_ptr->skill_base[23] >= 100) manacost = (manacost / 2);
				else if (p_ptr->skill_base[23] >= 50) manacost = (manacost - (manacost / 3));
				else if (p_ptr->skill_base[23] >= 25) manacost = (manacost - (manacost / 4));

                                sprintf(tmp, "%ld          ", manacost);
                                c_put_str(TERM_L_GREEN, tmp, 8, 11);
                                c_put_str(TERM_WHITE, "[q/w] to increase/decrease duration,", 17, 0);
                                c_put_str(TERM_WHITE, "[c] to complete creation,", 18, 0);
                                c_put_str(TERM_WHITE, "[ESC] to exit.", 19, 0);
                                choice = inkey();
                                if (choice == 'q' || choice == 'Q') power += 1;
                                if (choice == 'w' || choice == 'W')
                                {
                                        if (power > 0) power -= 1;
                                }
                                if (choice == 'c' || choice == 'C') readytocreate = TRUE;
                                if (choice == 'x' || choice == 'X' || choice == ESCAPE)
                                {
                                        Term_load();
                                        return;
                                }
                        }
                        }

                        /* Power + Duration interface */
                        /* Used by the Raise spells */
                        if (interfacetype == 4)
                        {

                        /* We've chosen AND prepared the effect! :) */
                        /* Let's create the actual spell now! :) */
                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "PORTRALIS SPELL CREATION", 0, 0);
        		c_put_str(TERM_WHITE, "--------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Selected effect:", 4, 0);
                        c_put_str(TERM_WHITE, choseneffect, 4, 17);

                        spelltype = 0;
                        while (readytocreate == FALSE)
                        {
                                c_put_str(TERM_WHITE, "Power:", 6, 0);
                                sprintf(tmp, "%d          ", power);
                                c_put_str(TERM_L_GREEN, tmp, 6, 7);
                                c_put_str(TERM_WHITE, "Duration:", 8, 0);
                                sprintf(tmp, "%d          ", spelltype);
                                c_put_str(TERM_L_GREEN, tmp, 8, 10);
                                c_put_str(TERM_WHITE, "Mana Cost:", 12, 0);
                                /* Special code for the Stoneskin spell */
                                if (effkind == 21) manacost = ((effbase + ((effcost * power))) * ((spelltype / 3) + 1)) / 5;
                                else manacost = (effbase + ((effcost * power))) * ((spelltype / 2) + 1);
                                if (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 7] >= 1)
                                {
                                        redpercent = p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 7];
                                        if (redpercent > 75) redpercent = 75;
                                        manacost -= ((manacost * redpercent) / 100);
                                        manacost -= (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 7] * 3);
                                        if (manacost < 0) manacost = 0;
                                }
				
				/* Mastery of Alteration spells! */
				if (p_ptr->skill_base[23] >= 100) manacost = (manacost / 2);
				else if (p_ptr->skill_base[23] >= 50) manacost = (manacost - (manacost / 3));
				else if (p_ptr->skill_base[23] >= 25) manacost = (manacost - (manacost / 4));

                                sprintf(tmp, "%ld          ", manacost);
                                c_put_str(TERM_L_GREEN, tmp, 12, 11);
                                c_put_str(TERM_WHITE, "[q/w] to increase/decrease power,", 16, 0);
                                c_put_str(TERM_WHITE, "[a/s] to increase/decrease duration,", 17, 0);
                                c_put_str(TERM_WHITE, "[c] to complete creation,", 18, 0);
                                c_put_str(TERM_WHITE, "[ESC] to exit.", 19, 0);
                                choice = inkey();
                                if (choice == 'q' || choice == 'Q') power += 1;
                                if (choice == 'w' || choice == 'W')
                                {
                                        if (power > 0) power -= 1;
                                }
                                if (choice == 'a' || choice == 'A') spelltype += 1;
                                if (choice == 's' || choice == 'S')
                                {
                                        if (spelltype > 0) spelltype -= 1;
                                }
                                if (choice == 'c' || choice == 'C') readytocreate = TRUE;
                                if (choice == 'x' || choice == 'X' || choice == ESCAPE)
                                {
                                        Term_load();
                                        return;
                                }
                        }
                        }
                        /* Power + Duration interface */
                        /* Used by the Resistance spells */
                        /* This one has a max power of 100. */
                        if (interfacetype == 5)
                        {

                        /* We've chosen AND prepared the effect! :) */
                        /* Let's create the actual spell now! :) */
                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "PORTRALIS SPELL CREATION", 0, 0);
        		c_put_str(TERM_WHITE, "--------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Selected effect:", 4, 0);
                        c_put_str(TERM_WHITE, choseneffect, 4, 17);

                        spelltype = 0;
                        while (readytocreate == FALSE)
                        {
                                c_put_str(TERM_WHITE, "Power:", 6, 0);
                                sprintf(tmp, "%d          ", power);
                                c_put_str(TERM_L_GREEN, tmp, 6, 7);
                                c_put_str(TERM_WHITE, "Duration:", 8, 0);
                                sprintf(tmp, "%d          ", spelltype);
                                c_put_str(TERM_L_GREEN, tmp, 8, 10);
                                c_put_str(TERM_WHITE, "Mana Cost:", 12, 0);
                                manacost = (effbase + (((effcost + (power / 2)) * power))) * spelltype;
				if (power > 75) manacost += manacost * ((power - 75) + 1);
                                if (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 7] >= 1)
                                {
                                        redpercent = p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 7];
                                        if (redpercent > 75) redpercent = 75;
                                        manacost -= ((manacost * redpercent) / 100);
                                        manacost -= (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 7] * 3);
                                        if (manacost < 0) manacost = 0;
                                }
				
				/* Mastery of Alteration spells! */
				if (p_ptr->skill_base[23] >= 100) manacost = (manacost / 2);
				else if (p_ptr->skill_base[23] >= 50) manacost = (manacost - (manacost / 3));
				else if (p_ptr->skill_base[23] >= 25) manacost = (manacost - (manacost / 4));

                                sprintf(tmp, "%ld          ", manacost);
                                c_put_str(TERM_L_GREEN, tmp, 12, 11);
                                c_put_str(TERM_WHITE, "[q/w] to increase/decrease power,", 16, 0);
                                c_put_str(TERM_WHITE, "[a/s] to increase/decrease duration,", 17, 0);
                                c_put_str(TERM_WHITE, "[c] to complete creation,", 18, 0);
                                c_put_str(TERM_WHITE, "[ESC] to exit.", 19, 0);
                                choice = inkey();
                                if (choice == 'q' || choice == 'Q')
                                {
                                        if (power < 100) power += 1;
                                }
                                if (choice == 'w' || choice == 'W')
                                {
                                        if (power > 0) power -= 1;
                                }
                                if (choice == 'a' || choice == 'A') spelltype += 1;
                                if (choice == 's' || choice == 'S')
                                {
                                        if (spelltype > 0) spelltype -= 1;
                                }
                                if (choice == 'c' || choice == 'C') readytocreate = TRUE;
                                if (choice == 'x' || choice == 'X' || choice == ESCAPE)
                                {
                                        Term_load();
                                        return;
                                }
                        }
                        }
			/* Standard damages interface */
			/* But with max damages of 100. Used by the new Life Blast! */
                        if (interfacetype == 6)
                        {

                        /* We've chosen AND prepared the effect! :) */
                        /* Let's create the actual spell now! :) */
                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "PORTRALIS SPELL CREATION", 0, 0);
        		c_put_str(TERM_WHITE, "--------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Selected effect:", 4, 0);
                        c_put_str(TERM_WHITE, choseneffect, 4, 17);

                        while (readytocreate == FALSE)
                        {
                                c_put_str(TERM_WHITE, "Power:", 6, 0);
                                sprintf(tmp, "%d          ", power);
                                c_put_str(TERM_L_GREEN, tmp, 6, 7);
                                c_put_str(TERM_WHITE, "Radius:", 8, 0);
                                sprintf(tmp, "%d          ", rad);
                                c_put_str(TERM_L_GREEN, tmp, 8, 8);
                                c_put_str(TERM_WHITE, "Type:", 10, 0);
                                if (spelltype == 1) sprintf(tmp, "Bolt  ");
                                else if (spelltype == 2) sprintf(tmp, "Ball  ");
                                else if (spelltype == 3) sprintf(tmp, "Circle");
                                else if (spelltype == 4) sprintf(tmp, "Chain ");
                                c_put_str(TERM_L_GREEN, tmp, 10, 6);
                                c_put_str(TERM_WHITE, "Mana Cost:", 12, 0);
                                manacost = (effbase + ((effcost * power))) * (rad + 1);
				manacost += ((manacost * (power * 5)) / 100);
				manacost += ((manacost * (power * 5)) / 100);
				manacost += ((manacost * (power * 5)) / 100);
                                if (spelltype == 4)
                                {
                                        int a;
                                        manacost *= 5;
                                        for (a = 0; a < rad; a++)
                                        {
                                                manacost += (50 * rad);
                                        }                                        
                                }
                                if (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 7] >= 1)
                                {
                                        redpercent = p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 7];
                                        if (redpercent > 75) redpercent = 75;
                                        manacost -= ((manacost * redpercent) / 100);
                                        manacost -= (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 7] * 3);
                                        if (manacost < 0) manacost = 0;
                                }
				
				/* Mastery of Alteration spells! */
				if (p_ptr->skill_base[23] >= 100) manacost = (manacost / 2);
				else if (p_ptr->skill_base[23] >= 50) manacost = (manacost - (manacost / 3));
				else if (p_ptr->skill_base[23] >= 25) manacost = (manacost - (manacost / 4));

                                sprintf(tmp, "%ld          ", manacost);
                                c_put_str(TERM_L_GREEN, tmp, 12, 11);
                                c_put_str(TERM_WHITE, "[q/w] to increase/decrease power,", 16, 0);
                                c_put_str(TERM_WHITE, "[a/s] to increase/decrease radius,", 17, 0);
                                c_put_str(TERM_WHITE, "[t] to change type,", 18, 0);
                                c_put_str(TERM_WHITE, "[c] to complete creation,", 19, 0);
                                c_put_str(TERM_WHITE, "[ESC] to exit.", 20, 0);
                                choice = inkey();
                                if (choice == 'q' || choice == 'Q')
				{
					power += 1;
					if (power > 100) power = 100;
				}
                                if (choice == 'w' || choice == 'W')
                                {
                                        if (power > 0) power -= 1;
                                }
                                if (choice == 'a' || choice == 'A')
                                {
                                        rad += 1;
                                        if (spelltype == 1) spelltype = 2;
                                }
                                if (choice == 's' || choice == 'S')
                                {
                                        if (rad > 0) rad -= 1;
                                        if (spelltype == 2 && rad == 0) spelltype = 1;
                                }
                                if (choice == 't' || choice == 'T')
                                {
                                        spelltype += 1;
                                        if (spelltype > 4) spelltype = 1;
                                        if (spelltype == 1 && rad > 0) spelltype += 1;
                                        if (spelltype == 1) rad = 0;
                                        if (spelltype == 2 && rad == 0) spelltype += 1;
                                }
                                if (choice == 'c' || choice == 'C') readytocreate = TRUE;
                                if (choice == 'x' || choice == 'X' || choice == ESCAPE)
                                {
                                        Term_load();
                                        return;
                                }
                        }
                        }

                }
                /* Mysticism */
                else if (query == '3')
                {
                        bool chosen = FALSE;
                        bool readytocreate = FALSE;
                        char choseneffect[30];                        
                        char choice;
                        int interfacetype;

                        /* Initialise some stuff */
                        effbase = 0;
                        effcost = 0;
                        efftype = 0;
                        rad = 0;
                        power = 0;
                        spelltype = 1;
                        manacost = 0;

                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "PORTRALIS SPELL CREATION", 0, 0);
        		c_put_str(TERM_WHITE, "--------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Select an effect:", 4, 0);

                        /* All right, let's prepare the list! :) */
                        if (p_ptr->healing_effects & MYST_HEAL) c_put_str(TERM_WHITE, "[0] Heal", 7, 0);
                        else c_put_str(TERM_L_DARK, "[0] Heal", 7, 0);
                        if (p_ptr->healing_effects & MYST_RESTORE_STATS) c_put_str(TERM_WHITE, "[1] Restore Stats", 8, 0);
                        else c_put_str(TERM_L_DARK, "[1] Restore Stats", 8, 0);
                        if (p_ptr->healing_effects & MYST_RESTORE_STATUS) c_put_str(TERM_WHITE, "[2] Restore Status", 9, 0);
                        else c_put_str(TERM_L_DARK, "[2] Restore Status", 9, 0);
                        if (p_ptr->healing_effects & MYST_HARM) c_put_str(TERM_WHITE, "[3] Harm", 10, 0);
                        else c_put_str(TERM_L_DARK, "[3] Harm", 10, 0);                        
                        if (p_ptr->healing_effects & MYST_DRAIN_LIFE) c_put_str(TERM_WHITE, "[4] Drain Life", 11, 0);
                        else c_put_str(TERM_L_DARK, "[4] Drain Life", 11, 0);
                        if (p_ptr->healing_effects & MYST_REVIVE_MONSTER) c_put_str(TERM_WHITE, "[5] Revive Monster", 12, 0);
                        else c_put_str(TERM_L_DARK, "[5] Revive Monster", 12, 0);
                        if (p_ptr->healing_effects & MYST_RESTORE_MANA) c_put_str(TERM_WHITE, "[6] Restore Mana", 13, 0);
                        else c_put_str(TERM_L_DARK, "[6] Restore Mana", 13, 0);
			if (p_ptr->healing_effects & MYST_SMITE_UNDEADS) c_put_str(TERM_WHITE, "[7] Smite Undeads", 14, 0);
                        else c_put_str(TERM_L_DARK, "[7] Smite Undeads", 14, 0);
			if (p_ptr->healing_effects & MYST_SMITE_DEMONS) c_put_str(TERM_WHITE, "[8] Smite Demons", 15, 0);
                        else c_put_str(TERM_L_DARK, "[8] Smite Demons", 15, 0);
			if (p_ptr->healing_effects & MYST_SMITE_EVIL) c_put_str(TERM_WHITE, "[9] Smite Evil", 16, 0);
                        else c_put_str(TERM_L_DARK, "[9] Smite Evil", 16, 0);
			if (p_ptr->healing_effects & MYST_SMITE_GOOD) c_put_str(TERM_WHITE, "[a] Smite Good", 17, 0);
                        else c_put_str(TERM_L_DARK, "[a] Smite Good", 17, 0);
			if (p_ptr->healing_effects & MYST_WAR_BLESSING) c_put_str(TERM_WHITE, "[b] War Blessing", 18, 0);
                        else c_put_str(TERM_L_DARK, "[b] War Blessing", 18, 0);
			if (p_ptr->healing_effects & MYST_BLESSING) c_put_str(TERM_WHITE, "[c] Blessing", 19, 0);
                        else c_put_str(TERM_L_DARK, "[c] Blessing", 19, 0);

                        while (chosen == FALSE)
                        {
                                choice = inkey();
                                if (choice == '0')
                                {
                                        if (!(p_ptr->healing_effects & MYST_HEAL)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_OLD_HEAL;
                                                effbase = 0;
                                                effcost = 12;
                                                effkind = 1;
                                                interfacetype = 4;
                                                sprintf(choseneffect, "Heal");
                                                chosen = TRUE;

						/* For the Heal effect, set the default target to Self. */
						spelltype = 5;
                                        }
                                }
                                else if (choice == '1')
                                {
                                        if (!(p_ptr->healing_effects & MYST_RESTORE_STATS)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 0;
                                                effbase = 40;
                                                effcost = 0;
                                                effkind = 7;
                                                interfacetype = 3;
                                                sprintf(choseneffect, "Restore Stats");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '2')
                                {
                                        if (!(p_ptr->healing_effects & MYST_RESTORE_STATUS)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 0;
                                                effbase = 40;
                                                effcost = 0;
                                                effkind = 8;
                                                interfacetype = 3;
                                                sprintf(choseneffect, "Restore Status");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '3')
                                {
                                        if (!(p_ptr->healing_effects & MYST_HARM)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_HARM;
                                                effbase = 0;
                                                effcost = 3;
                                                effkind = 1;
                                                interfacetype = 4;
                                                sprintf(choseneffect, "Harm");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '4')
                                {
                                        if (!(p_ptr->healing_effects & MYST_DRAIN_LIFE)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_DRAIN_LIFE;
                                                effbase = 0;
                                                effcost = 24;
                                                effkind = 1;
                                                interfacetype = 1;
                                                sprintf(choseneffect, "Drain Life");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '5')
                                {
                                        if (!(p_ptr->healing_effects & MYST_REVIVE_MONSTER)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 0;
                                                effbase = 200;
                                                effcost = 0;
                                                effkind = 11;
                                                interfacetype = 3;
                                                sprintf(choseneffect, "Revive Monster");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '6')
                                {
                                        if (!(p_ptr->healing_effects & MYST_RESTORE_MANA)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 0;
                                                effbase = 0;
                                                effcost = 2;
                                                effkind = 12;
                                                interfacetype = 2;
                                                sprintf(choseneffect, "Restore Mana");
                                                chosen = TRUE;
                                        }
                                }
				else if (choice == '7')
                                {
                                        if (!(p_ptr->healing_effects & MYST_SMITE_UNDEADS)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_UNDEAD_SMITE;
                                                effbase = 0;
                                                effcost = 1;
                                                effkind = 1;
                                                interfacetype = 4;
                                                sprintf(choseneffect, "Smite Undeads");
                                                chosen = TRUE;
                                        }
                                }
				else if (choice == '8')
                                {
                                        if (!(p_ptr->healing_effects & MYST_SMITE_DEMONS)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_DEMON_SMITE;
                                                effbase = 0;
                                                effcost = 1;
                                                effkind = 1;
                                                interfacetype = 4;
                                                sprintf(choseneffect, "Smite Demons");
                                                chosen = TRUE;
                                        }
                                }
				else if (choice == '9')
                                {
                                        if (!(p_ptr->healing_effects & MYST_SMITE_EVIL)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_EVIL_SMITE;
                                                effbase = 0;
                                                effcost = 1;
                                                effkind = 1;
                                                interfacetype = 4;
                                                sprintf(choseneffect, "Smite Evil");
                                                chosen = TRUE;
                                        }
                                }
				else if (choice == 'a')
                                {
                                        if (!(p_ptr->healing_effects & MYST_SMITE_GOOD)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_GOOD_SMITE;
                                                effbase = 0;
                                                effcost = 1;
                                                effkind = 1;
                                                interfacetype = 4;
                                                sprintf(choseneffect, "Smite Good");
                                                chosen = TRUE;
                                        }
                                }
				else if (choice == 'b')
                                {
                                        if (!(p_ptr->healing_effects & MYST_WAR_BLESSING)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_WAR_BLESSING;
                                                effbase = 30;
                                                effcost = 0;
                                                effkind = 2;
                                                interfacetype = 5;
                                                sprintf(choseneffect, "War Blessing");
                                                chosen = TRUE;
                                        }
                                }
				else if (choice == 'c')
                                {
                                        if (!(p_ptr->healing_effects & MYST_BLESSING)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 0;
                                                effbase = 0;
                                                effcost = 5;
                                                effkind = 5;
                                                interfacetype = 2;
                                                sprintf(choseneffect, "Blessing");
                                                chosen = TRUE;
                                        }
                                }
                               
                                else
                                {
                                        Term_load();
                                        return;
                                }
                        }
                        /* Choose the appropriate type of interface */
                        /* Depending on the spell. */

                        /* Standard damages interface */
                        if (interfacetype == 1)
                        {

                        /* We've chosen AND prepared the effect! :) */
                        /* Let's create the actual spell now! :) */
                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "PORTRALIS SPELL CREATION", 0, 0);
        		c_put_str(TERM_WHITE, "--------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Selected effect:", 4, 0);
                        c_put_str(TERM_WHITE, choseneffect, 4, 17);

                        while (readytocreate == FALSE)
                        {
                                c_put_str(TERM_WHITE, "Power:", 6, 0);
                                sprintf(tmp, "%d          ", power);
                                c_put_str(TERM_L_GREEN, tmp, 6, 7);
                                c_put_str(TERM_WHITE, "Radius:", 8, 0);
                                sprintf(tmp, "%d          ", rad);
                                c_put_str(TERM_L_GREEN, tmp, 8, 8);
                                c_put_str(TERM_WHITE, "Type:", 10, 0);
                                if (spelltype == 1) sprintf(tmp, "Bolt  ");
                                else if (spelltype == 2) sprintf(tmp, "Ball  ");
                                else if (spelltype == 3) sprintf(tmp, "Circle");
                                else if (spelltype == 4) sprintf(tmp, "Chain ");
				else if (spelltype == 5) sprintf(tmp, "Self  ");
                                c_put_str(TERM_L_GREEN, tmp, 10, 6);
                                c_put_str(TERM_WHITE, "Mana Cost:", 12, 0);
                                manacost = (effbase + ((effcost * power) / 2)) * (rad + 1);
                                if (spelltype == 4)
                                {
                                        int a;
                                        manacost *= 5;
                                        for (a = 0; a < rad; a++)
                                        {
                                                manacost += (50 * rad);
                                        }                                        
                                }
                                
				/* Mastery of Mysticism spells! */
				if (p_ptr->skill_base[24] >= 100) manacost = (manacost / 2);
				else if (p_ptr->skill_base[24] >= 50) manacost = (manacost - (manacost / 3));
				else if (p_ptr->skill_base[24] >= 25) manacost = (manacost - (manacost / 4));

				/* Priest's Mystical power. */
				if (p_ptr->abilities[(CLASS_PRIEST * 10) + 1] >= 1)
				{

					manacost = manacost - multiply_divide(p_ptr->stat_ind[A_WIS], p_ptr->abilities[(CLASS_PRIEST * 10) + 1] * 10, 100);
					if (manacost < 0) manacost = 0;
				}

                                sprintf(tmp, "%ld          ", manacost);
                                c_put_str(TERM_L_GREEN, tmp, 12, 11);
                                c_put_str(TERM_WHITE, "[q/w] to increase/decrease power,", 16, 0);
                                c_put_str(TERM_WHITE, "[a/s] to increase/decrease radius,", 17, 0);
                                c_put_str(TERM_WHITE, "[t] to change type,", 18, 0);
                                c_put_str(TERM_WHITE, "[c] to complete creation,", 19, 0);
                                c_put_str(TERM_WHITE, "[ESC] to exit.", 20, 0);
                                choice = inkey();
                                if (choice == 'q' || choice == 'Q') power += 1;
                                if (choice == 'w' || choice == 'W')
                                {
                                        if (power > 0) power -= 1;
                                }
                                if (choice == 'a' || choice == 'A')
                                {
                                        rad += 1;
                                        if (spelltype == 1) spelltype = 2;
                                }
                                if (choice == 's' || choice == 'S')
                                {
                                        if (rad > 0) rad -= 1;
                                        if (spelltype == 2 && rad == 0) spelltype = 1;
                                }
                                if (choice == 't' || choice == 'T')
                                {
                                        spelltype += 1;
                                        if (spelltype > 4) spelltype = 1;
                                        if (spelltype == 1 && rad > 0) spelltype += 1;
                                        if (spelltype == 1) rad = 0;
                                        if (spelltype == 2 && rad == 0) spelltype += 1;
                                }
                                if (choice == 'c' || choice == 'C') readytocreate = TRUE;
                                if (choice == 'x' || choice == 'X' || choice == ESCAPE)
                                {
                                        Term_load();
                                        return;
                                }
                        }
                        }
                        /* 2. Power only interface */
                        if (interfacetype == 2)
                        {

                        /* We've chosen AND prepared the effect! :) */
                        /* Let's create the actual spell now! :) */
                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "PORTRALIS SPELL CREATION", 0, 0);
        		c_put_str(TERM_WHITE, "--------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Selected effect:", 4, 0);
                        c_put_str(TERM_WHITE, choseneffect, 4, 17);

                        while (readytocreate == FALSE)
                        {
                                c_put_str(TERM_WHITE, "Power:", 6, 0);
                                sprintf(tmp, "%d          ", power);
                                c_put_str(TERM_L_GREEN, tmp, 6, 7);
                                c_put_str(TERM_WHITE, "Mana Cost:", 8, 0);
                                manacost = (effbase + ((effcost * power)));
                                if (manacost > 10) manacost *= (manacost / 5);
				
				/* Mastery of Mysticism spells! */
				if (p_ptr->skill_base[24] >= 100) manacost = (manacost / 2);
				else if (p_ptr->skill_base[24] >= 50) manacost = (manacost - (manacost / 3));
				else if (p_ptr->skill_base[24] >= 25) manacost = (manacost - (manacost / 4));

				/* Priest's Mystical power. */
				if (p_ptr->abilities[(CLASS_PRIEST * 10) + 1] >= 1)
				{

					manacost = manacost - multiply_divide(p_ptr->stat_ind[A_WIS], p_ptr->abilities[(CLASS_PRIEST * 10) + 1] * 10, 100);
					if (manacost < 0) manacost = 0;
				}

                                sprintf(tmp, "%ld          ", manacost);
                                c_put_str(TERM_L_GREEN, tmp, 8, 11);
                                c_put_str(TERM_WHITE, "[q/w] to increase/decrease power,", 17, 0);
                                c_put_str(TERM_WHITE, "[c] to complete creation,", 18, 0);
                                c_put_str(TERM_WHITE, "[ESC] to exit.", 19, 0);
                                choice = inkey();
                                if (choice == 'q' || choice == 'Q') power += 1;
                                if (choice == 'w' || choice == 'W')
                                {
                                        if (power > 0) power -= 1;
                                }
                                if (choice == 'c' || choice == 'C') readytocreate = TRUE;
                                if (choice == 'x' || choice == 'X' || choice == ESCAPE)
                                {
                                        Term_load();
                                        return;
                                }
                        }
                        }

                        /* Empty interface */
                        /* You see the mana cost...that's all. */
                        if (interfacetype == 3)
                        {

                        /* We've chosen AND prepared the effect! :) */
                        /* Let's create the actual spell now! :) */
                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "PORTRALIS SPELL CREATION", 0, 0);
        		c_put_str(TERM_WHITE, "--------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Selected effect:", 4, 0);
                        c_put_str(TERM_WHITE, choseneffect, 4, 17);

                        spelltype = 0;
                        while (readytocreate == FALSE)
                        {
                                c_put_str(TERM_WHITE, "Mana Cost:", 12, 0);
                                manacost = (effbase + ((effcost * power)));
                                
				/* Mastery of Mysticism spells! */
				if (p_ptr->skill_base[24] >= 100) manacost = (manacost / 2);
				else if (p_ptr->skill_base[24] >= 50) manacost = (manacost - (manacost / 3));
				else if (p_ptr->skill_base[24] >= 25) manacost = (manacost - (manacost / 4));

				/* Priest's Mystical power. */
				if (p_ptr->abilities[(CLASS_PRIEST * 10) + 1] >= 1)
				{

					manacost = manacost - multiply_divide(p_ptr->stat_ind[A_WIS], p_ptr->abilities[(CLASS_PRIEST * 10) + 1] * 10, 100);
					if (manacost < 0) manacost = 0;
				}

                                sprintf(tmp, "%ld          ", manacost);
                                c_put_str(TERM_L_GREEN, tmp, 12, 11);
                                c_put_str(TERM_WHITE, "[c] to complete creation,", 18, 0);
                                c_put_str(TERM_WHITE, "[ESC] to exit.", 19, 0);
                                choice = inkey();
                                if (choice == 'c' || choice == 'C') readytocreate = TRUE;
                                if (choice == 'x' || choice == 'X' || choice == ESCAPE)
                                {
                                        Term_load();
                                        return;
                                }
                        }
                        }

			/* Standard damages interface */
			/* Uses the standard Elemental formula */
                        if (interfacetype == 4)
                        {

                        /* We've chosen AND prepared the effect! :) */
                        /* Let's create the actual spell now! :) */
                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "PORTRALIS SPELL CREATION", 0, 0);
        		c_put_str(TERM_WHITE, "--------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Selected effect:", 4, 0);
                        c_put_str(TERM_WHITE, choseneffect, 4, 17);

                        while (readytocreate == FALSE)
                        {
                                c_put_str(TERM_WHITE, "Power:", 6, 0);
                                sprintf(tmp, "%d          ", power);
                                c_put_str(TERM_L_GREEN, tmp, 6, 7);
                                c_put_str(TERM_WHITE, "Radius:", 8, 0);
                                sprintf(tmp, "%d          ", rad);
                                c_put_str(TERM_L_GREEN, tmp, 8, 8);
                                c_put_str(TERM_WHITE, "Type:", 10, 0);
                                if (spelltype == 1) sprintf(tmp, "Bolt         ");
                                else if (spelltype == 2) sprintf(tmp, "Ball         ");
                                else if (spelltype == 3) sprintf(tmp, "Circle       ");
                                else if (spelltype == 4) sprintf(tmp, "Chain        ");
				else if (spelltype == 5) sprintf(tmp, "Self         ");
                                c_put_str(TERM_L_GREEN, tmp, 10, 6);
                                c_put_str(TERM_WHITE, "Mana Cost:", 12, 0);
                                /* manacost = (effbase + ((effcost * power))) * (rad + 1);*/

				manacost = (effbase + ((effcost * power) / 2));
                                manacost += ((manacost * (power * 3)) / 100);
                                manacost = manacost * (rad + 1);

				/* Smite Undeads and Smite Demons aren't very expensive. */
				if (efftype == GF_UNDEAD_SMITE || efftype == GF_DEMON_SMITE || efftype == GF_EVIL_SMITE || efftype == GF_GOOD_SMITE) manacost = manacost / 2;

                                if (spelltype == 4)
                                {
                                        int a;
                                        manacost *= 5;
                                        for (a = 0; a < rad; a++)
                                        {
                                                manacost += (50 * rad);
                                        }                                        
                                }
                                
				/* Mastery of Mysticism spells! */
				if (p_ptr->skill_base[24] >= 100) manacost = (manacost / 2);
				else if (p_ptr->skill_base[24] >= 50) manacost = (manacost - (manacost / 3));
				else if (p_ptr->skill_base[24] >= 25) manacost = (manacost - (manacost / 4));

				/* Priest's Mystical power. */
				if (p_ptr->abilities[(CLASS_PRIEST * 10) + 1] >= 1)
				{

					manacost = manacost - multiply_divide(p_ptr->stat_ind[A_WIS], p_ptr->abilities[(CLASS_PRIEST * 10) + 1] * 10, 100);
					if (manacost < 0) manacost = 0;
				}

				if (manacost < 0) manacost = 0;

                                sprintf(tmp, "%ld          ", manacost);
                                c_put_str(TERM_L_GREEN, tmp, 12, 11);
                                c_put_str(TERM_WHITE, "[q/w] to increase/decrease power,", 16, 0);
                                c_put_str(TERM_WHITE, "[a/s] to increase/decrease radius,", 17, 0);
                                c_put_str(TERM_WHITE, "[t] to change type,", 18, 0);
                                c_put_str(TERM_WHITE, "[c] to complete creation,", 19, 0);
                                c_put_str(TERM_WHITE, "[ESC] to exit.", 20, 0);
                                choice = inkey();
                                if (choice == 'q' || choice == 'Q') power += 1;
                                if (choice == 'w' || choice == 'W')
                                {
                                        if (power > 0) power -= 1;
                                }
                                if (choice == 'a' || choice == 'A')
                                {
                                        rad += 1;
                                        if (spelltype == 1) spelltype = 2;
                                }
                                if (choice == 's' || choice == 'S')
                                {
                                        if (rad > 0) rad -= 1;
                                        if (spelltype == 2 && rad == 0) spelltype = 1;
                                }
                                if (choice == 't' || choice == 'T')
                                {
                                        spelltype += 1;
					if (efftype == GF_OLD_HEAL)
					{
                                        	if (spelltype > 5) spelltype = 1;
					}
					else
					{
						if (spelltype > 4) spelltype = 1;
					}
                                        if (spelltype == 1 && rad > 0) spelltype += 1;
                                        if (spelltype == 1) rad = 0;
                                        if (spelltype == 2 && rad == 0) spelltype += 1;
                                }
                                if (choice == 'c' || choice == 'C') readytocreate = TRUE;
                                if (choice == 'x' || choice == 'X' || choice == ESCAPE)
                                {
                                        Term_load();
                                        return;
                                }
                        }
                        }
			/* 5. Powerless damages interface */
                        if (interfacetype == 5)
                        {

                        /* We've chosen AND prepared the effect! :) */
                        /* Let's create the actual spell now! :) */
                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "PORTRALIS SPELL CREATION", 0, 0);
        		c_put_str(TERM_WHITE, "--------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Selected effect:", 4, 0);
                        c_put_str(TERM_WHITE, choseneffect, 4, 17);

                        while (readytocreate == FALSE)
                        {
                                power = 0;
                                c_put_str(TERM_WHITE, "Radius:", 6, 0);
                                sprintf(tmp, "%d          ", rad);
                                c_put_str(TERM_L_GREEN, tmp, 6, 8);
                                c_put_str(TERM_WHITE, "Type:", 8, 0);
                                if (spelltype == 1) sprintf(tmp, "Bolt  ");
                                else if (spelltype == 2) sprintf(tmp, "Ball  ");
                                else if (spelltype == 3) sprintf(tmp, "Circle");
                                else if (spelltype == 4) sprintf(tmp, "Chain ");
                                c_put_str(TERM_L_GREEN, tmp, 8, 6);
                                c_put_str(TERM_WHITE, "Mana Cost:", 10, 0);
                                manacost = (effbase + ((effcost * power))) * (rad + 1);
                                if (spelltype == 4)
                                {
                                        int a;
                                        manacost *= 5;
                                        for (a = 0; a < rad; a++)
                                        {
                                                manacost += (50 * rad);
                                        }                                        
                                }
                                
				/* Mastery of Mysticism spells! */
				if (p_ptr->skill_base[24] >= 100) manacost = (manacost / 2);
				else if (p_ptr->skill_base[24] >= 50) manacost = (manacost - (manacost / 3));
				else if (p_ptr->skill_base[24] >= 25) manacost = (manacost - (manacost / 4));

				/* Priest's Mystical power. */
				if (p_ptr->abilities[(CLASS_PRIEST * 10) + 1] >= 1)
				{

					manacost = manacost - multiply_divide(p_ptr->stat_ind[A_WIS], p_ptr->abilities[(CLASS_PRIEST * 10) + 1] * 10, 100);
					if (manacost < 0) manacost = 0;
				}

                                sprintf(tmp, "%ld          ", manacost);
                                c_put_str(TERM_L_GREEN, tmp, 10, 11);
                                c_put_str(TERM_WHITE, "[a/s] to increase/decrease radius,", 17, 0);
                                c_put_str(TERM_WHITE, "[t] to change type,", 18, 0);
                                c_put_str(TERM_WHITE, "[c] to complete creation,", 19, 0);
                                c_put_str(TERM_WHITE, "[ESC] to exit.", 20, 0);
                                choice = inkey();
                                if (choice == 'a' || choice == 'A')
                                {
                                        rad += 1;
                                        if (spelltype == 1) spelltype = 2;
                                }
                                if (choice == 's' || choice == 'S')
                                {
                                        if (rad > 0) rad -= 1;
                                        if (spelltype == 2 && rad == 0) spelltype = 1;
                                }
                                if (choice == 't' || choice == 'T')
                                {
                                        spelltype += 1;
                                        if (spelltype > 4) spelltype = 1;
                                        if (spelltype == 1 && rad > 0) spelltype += 1;
                                        if (spelltype == 1) rad = 0;
                                        if (spelltype == 2 && rad == 0) spelltype += 1;
                                }
                                if (choice == 'c' || choice == 'C') readytocreate = TRUE;
                                if (choice == 'x' || choice == 'X' || choice == ESCAPE)
                                {
                                        Term_load();
                                        return;
                                }
                        }
                        }


                }
                /* Conjurations! */
                else if (query == '4')
                {
                        bool chosen = FALSE;
                        bool readytocreate = FALSE;
                        char choseneffect[30];                        
                        char choice;
                        int interfacetype;

                        /* Initialise some stuff */
                        effbase = 0;
                        effcost = 0;
                        efftype = 0;
                        rad = 0;
                        power = 0;
                        spelltype = 1;
                        manacost = 0;

                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "PORTRALIS SPELL CREATION", 0, 0);
        		c_put_str(TERM_WHITE, "--------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Select an effect:", 4, 0);

                        /* All right, let's prepare the list! :) */
                        if (p_ptr->conjuration_effects & CONJ_SUMMON_KIND) c_put_str(TERM_WHITE, "[0] Summon Kind", 7, 0);
                        else c_put_str(TERM_L_DARK, "[0] Summon Kind", 7, 0);
                        if (p_ptr->conjuration_effects & CONJ_SUMMON_SPECIFIC) c_put_str(TERM_WHITE, "[1] Summon Specific", 8, 0);
                        else c_put_str(TERM_L_DARK, "[1] Summon Specific", 8, 0);
                        if (p_ptr->conjuration_effects & CONJ_FIRE_FIELD) c_put_str(TERM_WHITE, "[2] Fire Fields", 9, 0);
                        else c_put_str(TERM_L_DARK, "[2] Fire Fields", 9, 0);
                        if (p_ptr->conjuration_effects & CONJ_COLD_FIELD) c_put_str(TERM_WHITE, "[3] Cold Fields", 10, 0);
                        else c_put_str(TERM_L_DARK, "[3] Cold Fields", 10, 0);
                        if (p_ptr->conjuration_effects & CONJ_ELEC_FIELD) c_put_str(TERM_WHITE, "[4] Electric Fields", 11, 0);
                        else c_put_str(TERM_L_DARK, "[4] Electric Fields", 11, 0);
                        if (p_ptr->conjuration_effects & CONJ_WEBS) c_put_str(TERM_WHITE, "[5] Webs", 12, 0);
                        else c_put_str(TERM_L_DARK, "[5] Webs", 12, 0);
                        if (p_ptr->conjuration_effects & CONJ_GROW_TREES) c_put_str(TERM_WHITE, "[6] Grow Trees", 13, 0);
                        else c_put_str(TERM_L_DARK, "[6] Grow Trees", 13, 0);
                        if (p_ptr->conjuration_effects & CONJ_THORNS) c_put_str(TERM_WHITE, "[7] Thorned Vines", 14, 0);
                        else c_put_str(TERM_L_DARK, "[7] Thorned Vines", 14, 0);
                        if (p_ptr->conjuration_effects & CONJ_STORMS) c_put_str(TERM_WHITE, "[8] Storms", 15, 0);
                        else c_put_str(TERM_L_DARK, "[8] Storms", 15, 0);
                        if (p_ptr->conjuration_effects & CONJ_ITEM) c_put_str(TERM_WHITE, "[9] Conjure Item", 16, 0);
                        else c_put_str(TERM_L_DARK, "[9] Conjure Item", 16, 0);
                        if (p_ptr->conjuration_effects & CONJ_MAGIC_ITEM) c_put_str(TERM_WHITE, "[a] Conjure Magic Item", 17, 0);
                        else c_put_str(TERM_L_DARK, "[a] Conjure Magic Item", 17, 0);
                        if (p_ptr->conjuration_effects & CONJ_SPECIAL_ITEM) c_put_str(TERM_WHITE, "[b] Conjure Special Item", 18, 0);
                        else c_put_str(TERM_L_DARK, "[b] Conjure Special Item", 18, 0);


                        while (chosen == FALSE)
                        {
                                choice = inkey();
                                if (choice == '0')
                                {
                                        if (!(p_ptr->conjuration_effects & CONJ_SUMMON_KIND)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 0;
                                                effbase = 0;
                                                effcost = 3;
                                                effkind = 13;
                                                interfacetype = 1;
                                                sprintf(choseneffect, "Summon Kind");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '1')
                                {
                                        if (!(p_ptr->conjuration_effects & CONJ_SUMMON_SPECIFIC)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 0;
                                                effbase = 0;
                                                effcost = 0;
                                                effkind = 14;
                                                interfacetype = 2;
                                                sprintf(choseneffect, "Summon Specific");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '2')
                                {
                                        if (!(p_ptr->conjuration_effects & CONJ_FIRE_FIELD)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = FEAT_FIRE_FIELD;
                                                effbase = 0;
                                                effcost = 6;
                                                effkind = 15;
                                                interfacetype = 3;
                                                sprintf(choseneffect, "Fire Fields");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '3')
                                {
                                        if (!(p_ptr->conjuration_effects & CONJ_COLD_FIELD)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = FEAT_COLD_FIELD;
                                                effbase = 0;
                                                effcost = 6;
                                                effkind = 15;
                                                interfacetype = 3;
                                                sprintf(choseneffect, "Cold Fields");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '4')
                                {
                                        if (!(p_ptr->conjuration_effects & CONJ_ELEC_FIELD)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = FEAT_ELEC_FIELD;
                                                effbase = 0;
                                                effcost = 6;
                                                effkind = 15;
                                                interfacetype = 3;
                                                sprintf(choseneffect, "Electric Fields");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '5')
                                {
                                        if (!(p_ptr->conjuration_effects & CONJ_WEBS)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = FEAT_WEBS;
                                                effbase = 50;
                                                effcost = 0;
                                                effkind = 15;
                                                interfacetype = 4;
                                                sprintf(choseneffect, "Webs");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '6')
                                {
                                        if (!(p_ptr->conjuration_effects & CONJ_GROW_TREES)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = FEAT_TREES;
                                                effbase = 75;

                                                effcost = 0;
                                                effkind = 15;
                                                interfacetype = 4;
                                                sprintf(choseneffect, "Grow Trees");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '7')
                                {
                                        if (!(p_ptr->conjuration_effects & CONJ_THORNS)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = FEAT_THORNED_VINES;
                                                effbase = 0;
                                                effcost = 6;
                                                effkind = 15;
                                                interfacetype = 3;
                                                sprintf(choseneffect, "Thorned Vines");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '8')
                                {
                                        if (!(p_ptr->conjuration_effects & CONJ_STORMS)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = FEAT_STORMS;
                                                effbase = 0;
                                                effcost = 6;
                                                effkind = 15;
                                                interfacetype = 3;
                                                sprintf(choseneffect, "Storms");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '9')
                                {
                                        if (!(p_ptr->conjuration_effects & CONJ_ITEM)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 1;
                                                effbase = 0;
                                                effcost = 1;
                                                effkind = 22;
                                                interfacetype = 5;
                                                sprintf(choseneffect, "Conjure Item");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'a')
                                {
                                        if (!(p_ptr->conjuration_effects & CONJ_MAGIC_ITEM)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 2;
                                                effbase = 100;
                                                effcost = 5;
                                                effkind = 22;
                                                interfacetype = 5;
                                                sprintf(choseneffect, "Conjure Magic Item");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == 'b')
                                {
                                        if (!(p_ptr->conjuration_effects & CONJ_SPECIAL_ITEM)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 3;
                                                effbase = 500;
                                                effcost = 20;
                                                effkind = 22;
                                                interfacetype = 5;
                                                sprintf(choseneffect, "Conjure Special Item");
                                                chosen = TRUE;
                                        }
                                }

                               
                                else
                                {
                                        Term_load();
                                        return;
                                }
                        }
                        /* Choose the appropriate type of interface */
                        /* Depending on the spell. */

                        /* Summon Kind Interface */
                        if (interfacetype == 1)
                        {

                        /* We've chosen AND prepared the effect! :) */
                        /* Let's create the actual spell now! :) */
                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "PORTRALIS SPELL CREATION", 0, 0);
        		c_put_str(TERM_WHITE, "--------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Selected effect:", 4, 0);
                        c_put_str(TERM_WHITE, choseneffect, 4, 17);

                        spelltype = 0;
			efftype = 1;
                        msg_print("Enter the monster type.  ");
                        summch = inkey();
                        if (effectschoose == 0) spell_ptr->schar1 = summch;
                        if (effectschoose == 1) spell_ptr->schar2 = summch;
                        if (effectschoose == 2) spell_ptr->schar3 = summch;
                        if (effectschoose == 3) spell_ptr->schar4 = summch;
                        if (effectschoose == 4) spell_ptr->schar5 = summch;
                        c_put_str(TERM_WHITE, "PORTRALIS SPELL CREATION", 0, 0);
                        while (readytocreate == FALSE)
                        {
                                c_put_str(TERM_WHITE, "Monster Kind:", 6, 0);
                                sprintf(tmp, "%c          ", summch);
                                c_put_str(TERM_L_GREEN, tmp, 6, 14);                                
                                c_put_str(TERM_WHITE, "Power:", 8, 0);
                                sprintf(tmp, "%d          ", power);
                                c_put_str(TERM_L_GREEN, tmp, 8, 7);
                                c_put_str(TERM_WHITE, "Number:", 10, 0);
                                sprintf(tmp, "%d          ", spelltype);
                                c_put_str(TERM_L_GREEN, tmp, 10, 8);
				c_put_str(TERM_WHITE, "Duration:", 12, 0);
                                sprintf(tmp, "%d          ", efftype);
                                c_put_str(TERM_L_GREEN, tmp, 12, 10);
                                c_put_str(TERM_WHITE, "Mana Cost:", 14, 0);
                                manacost = (effbase + ((effcost * power))) * (rad + 1);
				manacost = (manacost * spelltype) + multiply_divide((manacost * spelltype), efftype * 20, 100);

                                if (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 8] >= 1)
                                {
                                        redpercent = p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 8];
                                        if (redpercent > 75) redpercent = 75;
                                        manacost -= ((manacost * redpercent) / 100);
                                        manacost -= (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 8] * 3);
                                        if (manacost < 0) manacost = 0;
                                }
				
				/* Mastery of Conjuration spells! */
				if (p_ptr->skill_base[25] >= 100) manacost = (manacost / 2);
				else if (p_ptr->skill_base[25] >= 50) manacost = (manacost - (manacost / 3));
				else if (p_ptr->skill_base[25] >= 25) manacost = (manacost - (manacost / 4));

                                sprintf(tmp, "%ld          ", manacost);
                                c_put_str(TERM_L_GREEN, tmp, 14, 11);
                                c_put_str(TERM_WHITE, "[q/w] to increase/decrease power,", 16, 0);
                                c_put_str(TERM_WHITE, "[a/s] to increase/decrease number,", 17, 0);
				c_put_str(TERM_WHITE, "[z/x] to increase/decrease duration,", 18, 0);
                                c_put_str(TERM_WHITE, "[t] to change type,", 19, 0);
                                c_put_str(TERM_WHITE, "[c] to complete creation,", 20, 0);
                                c_put_str(TERM_WHITE, "[ESC] to exit.", 21, 0);
                                choice = inkey();
                                if (choice == 'q' || choice == 'Q') power += 1;
                                if (choice == 'w' || choice == 'W')
                                {
                                        if (power > 0) power -= 1;
                                }
                                if (choice == 'a' || choice == 'A') spelltype += 1;
                                if (choice == 's' || choice == 'S')
                                {
                                        if (spelltype > 0) spelltype -= 1;
                                }
				if (choice == 'z' || choice == 'Z') efftype += 1;
                                if (choice == 'x' || choice == 'X')
                                {
                                        if (efftype > 1) efftype -= 1;
                                }
                                if (choice == 't' || choice == 'T')
                                {
                                        msg_print("Enter the monster type.  ");
                                        summch = inkey();
                                        if (effectschoose == 0) spell_ptr->schar1 = summch;
                                        if (effectschoose == 1) spell_ptr->schar2 = summch;
                                        if (effectschoose == 2) spell_ptr->schar3 = summch;
                                        if (effectschoose == 3) spell_ptr->schar4 = summch;
                                        if (effectschoose == 4) spell_ptr->schar5 = summch;
                                        c_put_str(TERM_WHITE, "PORTRALIS SPELL CREATION", 0, 0);
                                }
                                if (choice == 'c' || choice == 'C') readytocreate = TRUE;
                                if (choice == ESCAPE)
                                {
                                        Term_load();
                                        return;
                                }
                        }
                        }

                        /* Summon Specific Interface */
                        if (interfacetype == 2)
                        {

                        /* We've chosen AND prepared the effect! :) */
                        /* Let's create the actual spell now! :) */
                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "PORTRALIS SPELL CREATION", 0, 0);
        		c_put_str(TERM_WHITE, "--------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Selected effect:", 4, 0);
                        c_put_str(TERM_WHITE, choseneffect, 4, 17);

                        spelltype = 0;
			efftype = 1;
                        /* Go to the "name" field */
                        move_cursor(6, 14);

                        /* Get an input, ignore "Escape" */
                        if (askfor_aux(tmpsumm, 79))
                        {                                
                                if (effectschoose == 0) strcpy(spell_ptr->sspeci1, tmpsumm);
                                if (effectschoose == 1) strcpy(spell_ptr->sspeci2, tmpsumm);
                                if (effectschoose == 2) strcpy(spell_ptr->sspeci3, tmpsumm);
                                if (effectschoose == 3) strcpy(spell_ptr->sspeci4, tmpsumm);
                                if (effectschoose == 4) strcpy(spell_ptr->sspeci5, tmpsumm);
                        }

                        c_put_str(TERM_WHITE, "PORTRALIS SPELL CREATION", 0, 0);
                        while (readytocreate == FALSE)
                        {
                                c_put_str(TERM_WHITE, "Monster Name:", 6, 0);
                                sprintf(tmp, "%s          ", tmpsumm);
                                c_put_str(TERM_L_GREEN, tmp, 6, 14);                                
                                c_put_str(TERM_WHITE, "Number:", 8, 0);
                                sprintf(tmp, "%d          ", spelltype);
                                c_put_str(TERM_L_GREEN, tmp, 8, 8);
				c_put_str(TERM_WHITE, "Duration:", 10, 0);
                                sprintf(tmp, "%d          ", efftype);
                                c_put_str(TERM_L_GREEN, tmp, 10, 10);
                                c_put_str(TERM_WHITE, "Mana Cost:", 12, 0);
                                manacost = get_name_manacost(tmpsumm);
				manacost = (manacost * spelltype) + multiply_divide((manacost * spelltype), efftype * 20, 100);

                                if (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 8] >= 1)
                                {
                                        redpercent = p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 8];
                                        if (redpercent > 75) redpercent = 75;
                                        manacost -= ((manacost * redpercent) / 100);
                                        manacost -= (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 8] * 3);
                                        if (manacost < 0) manacost = 0;
                                }
				
				/* Mastery of Conjuration spells! */
				if (p_ptr->skill_base[25] >= 100) manacost = (manacost / 2);
				else if (p_ptr->skill_base[25] >= 50) manacost = (manacost - (manacost / 3));
				else if (p_ptr->skill_base[25] >= 25) manacost = (manacost - (manacost / 4));

                                sprintf(tmp, "%ld          ", manacost);
                                c_put_str(TERM_L_GREEN, tmp, 12, 11);                                
                                c_put_str(TERM_WHITE, "[a/s] to increase/decrease number,", 16, 0);
				c_put_str(TERM_WHITE, "[z/x] to increase/decrease duration,", 17, 0);
                                c_put_str(TERM_WHITE, "[n] to change name,", 18, 0);
                                c_put_str(TERM_WHITE, "[c] to complete creation,", 19, 0);
                                c_put_str(TERM_WHITE, "[ESC] to exit.", 20, 0);
                                choice = inkey();
                                if (choice == 'a' || choice == 'A') spelltype += 1;
                                if (choice == 's' || choice == 'S')
                                {
                                        if (spelltype > 0) spelltype -= 1;
                                }
				if (choice == 'z' || choice == 'Z') efftype += 1;
                                if (choice == 'x' || choice == 'X')
                                {
                                        if (efftype > 1) efftype -= 1;
                                }
                                if (choice == 'n' || choice == 'N')
                                {
                                        /* Go to the "name" field */
                                        move_cursor(6, 14);

                                        /* Get an input, ignore "Escape" */
                                        if (askfor_aux(tmpsumm, 79))
                                        {                                
                                                if (effectschoose == 0) strcpy(spell_ptr->sspeci1, tmpsumm);
                                                if (effectschoose == 1) strcpy(spell_ptr->sspeci2, tmpsumm);
                                                if (effectschoose == 2) strcpy(spell_ptr->sspeci3, tmpsumm);
                                                if (effectschoose == 3) strcpy(spell_ptr->sspeci4, tmpsumm);
                                                if (effectschoose == 4) strcpy(spell_ptr->sspeci5, tmpsumm);
                                        }
                                }
                                if (choice == 'c' || choice == 'C') readytocreate = TRUE;
                                if (choice == ESCAPE)
                                {
                                        Term_load();
                                        return;
                                }
                        }
                        }

                        /* Damages interface for fields! */
                        if (interfacetype == 3)
                        {

                        /* We've chosen AND prepared the effect! :) */
                        /* Let's create the actual spell now! :) */
                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "PORTRALIS SPELL CREATION", 0, 0);
        		c_put_str(TERM_WHITE, "--------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Selected effect:", 4, 0);
                        c_put_str(TERM_WHITE, choseneffect, 4, 17);

                        while (readytocreate == FALSE)
                        {
                                c_put_str(TERM_WHITE, "Power:", 6, 0);
                                sprintf(tmp, "%d          ", power);
                                c_put_str(TERM_L_GREEN, tmp, 6, 7);
                                c_put_str(TERM_WHITE, "Radius:", 8, 0);
                                sprintf(tmp, "%d          ", rad);
                                c_put_str(TERM_L_GREEN, tmp, 8, 8);
                                c_put_str(TERM_WHITE, "Type:", 10, 0);
                                if (spelltype == 1) sprintf(tmp, "Circle      ");
                                else if (spelltype == 2) sprintf(tmp, "Free Placing");
                                c_put_str(TERM_L_GREEN, tmp, 10, 6);
                                c_put_str(TERM_WHITE, "Mana Cost:", 12, 0);
                                manacost = (effbase + ((effcost * power))) * (rad + 1);
                                if (spelltype == 2) manacost *= 2;
                                if (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 8] >= 1)
                                {
                                        redpercent = p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 8];
                                        if (redpercent > 75) redpercent = 75;
                                        manacost -= ((manacost * redpercent) / 100);
                                        manacost -= (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 8] * 3);
                                        if (manacost < 0) manacost = 0;
                                }
				
				/* Mastery of Conjuration spells! */
				if (p_ptr->skill_base[25] >= 100) manacost = (manacost / 2);
				else if (p_ptr->skill_base[25] >= 50) manacost = (manacost - (manacost / 3));
				else if (p_ptr->skill_base[25] >= 25) manacost = (manacost - (manacost / 4));

                                sprintf(tmp, "%ld          ", manacost);
                                c_put_str(TERM_L_GREEN, tmp, 12, 11);
                                c_put_str(TERM_WHITE, "[q/w] to increase/decrease power,", 16, 0);
                                c_put_str(TERM_WHITE, "[a/s] to increase/decrease radius,", 17, 0);
                                c_put_str(TERM_WHITE, "[t] to change type,", 18, 0);
                                c_put_str(TERM_WHITE, "[c] to complete creation,", 19, 0);
                                c_put_str(TERM_WHITE, "[ESC] to exit.", 20, 0);
                                choice = inkey();
                                if (choice == 'q' || choice == 'Q') power += 1;
                                if (choice == 'w' || choice == 'W')
                                {
                                        if (power > 0) power -= 1;
                                }
                                if (choice == 'a' || choice == 'a') rad += 1;
                                if (choice == 's' || choice == 'S')
                                {
                                        if (rad > 0) rad -= 1;
                                }
                                if (choice == 't' || choice == 'T')
                                {
                                        spelltype += 1;
                                        if (spelltype > 2) spelltype = 1;
                                }
                                if (choice == 'c' || choice == 'C') readytocreate = TRUE;
                                if (choice == 'x' || choice == 'X' || choice == ESCAPE)
                                {
                                        Term_load();
                                        return;
                                }
                        }
                        }
                        /* Fields that do no damages! */
                        if (interfacetype == 4)
                        {

                        /* We've chosen AND prepared the effect! :) */
                        /* Let's create the actual spell now! :) */
                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "PORTRALIS SPELL CREATION", 0, 0);
        		c_put_str(TERM_WHITE, "--------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Selected effect:", 4, 0);
                        c_put_str(TERM_WHITE, choseneffect, 4, 17);

                        while (readytocreate == FALSE)
                        {
                                power = 0;
                                c_put_str(TERM_WHITE, "Radius:", 6, 0);
                                sprintf(tmp, "%d          ", rad);
                                c_put_str(TERM_L_GREEN, tmp, 6, 8);
                                c_put_str(TERM_WHITE, "Type:", 8, 0);
                                if (spelltype == 1) sprintf(tmp, "Circle      ");
                                else if (spelltype == 2) sprintf(tmp, "Free Placing");
                                c_put_str(TERM_L_GREEN, tmp, 8, 6);
                                c_put_str(TERM_WHITE, "Mana Cost:", 12, 0);
                                manacost = (effbase + ((effcost * power))) * (rad + 1);
                                if (spelltype == 2) manacost *= 2;
                                if (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 8] >= 1)
                                {
                                        redpercent = p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 8];
                                        if (redpercent > 75) redpercent = 75;
                                        manacost -= ((manacost * redpercent) / 100);
                                        manacost -= (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 8] * 3);
                                        if (manacost < 0) manacost = 0;
                                }
				
				/* Mastery of Conjuration spells! */
				if (p_ptr->skill_base[25] >= 100) manacost = (manacost / 2);
				else if (p_ptr->skill_base[25] >= 50) manacost = (manacost - (manacost / 3));
				else if (p_ptr->skill_base[25] >= 25) manacost = (manacost - (manacost / 4));

                                sprintf(tmp, "%ld          ", manacost);
                                c_put_str(TERM_L_GREEN, tmp, 12, 11);                               
                                c_put_str(TERM_WHITE, "[a/s] to increase/decrease radius,", 16, 0);
                                c_put_str(TERM_WHITE, "[t] to change type,", 17, 0);
                                c_put_str(TERM_WHITE, "[c] to complete creation,", 18, 0);
                                c_put_str(TERM_WHITE, "[ESC] to exit.", 19, 0);
                                choice = inkey();
                                if (choice == 'a' || choice == 'a') rad += 1;
                                if (choice == 's' || choice == 'S')
                                {
                                        if (rad > 0) rad -= 1;
                                }
                                if (choice == 't' || choice == 'T')
                                {
                                        spelltype += 1;
                                        if (spelltype > 2) spelltype = 1;
                                }
                                if (choice == 'c' || choice == 'C') readytocreate = TRUE;
                                if (choice == 'x' || choice == 'X' || choice == ESCAPE)
                                {
                                        Term_load();
                                        return;
                                }
                        }
                        }

                        /* Conjure Item Interface */
                        if (interfacetype == 5)
                        {

                        /* We've chosen AND prepared the effect! :) */
                        /* Let's create the actual spell now! :) */
                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "PORTRALIS SPELL CREATION", 0, 0);
        		c_put_str(TERM_WHITE, "--------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Selected effect:", 4, 0);
                        c_put_str(TERM_WHITE, choseneffect, 4, 17);

                        spelltype = 1;
                        /* Go to the "name" field */
                        move_cursor(6, 14);

                        /* Get an input, ignore "Escape" */
                        if (askfor_aux(tmpsumm, 29))
                        {                                
                                if (effectschoose == 0) strcpy(spell_ptr->sspeci1, tmpsumm);
                                if (effectschoose == 1) strcpy(spell_ptr->sspeci2, tmpsumm);
                                if (effectschoose == 2) strcpy(spell_ptr->sspeci3, tmpsumm);
                                if (effectschoose == 3) strcpy(spell_ptr->sspeci4, tmpsumm);
                                if (effectschoose == 4) strcpy(spell_ptr->sspeci5, tmpsumm);
                        }

                        c_put_str(TERM_WHITE, "PORTRALIS SPELL CREATION", 0, 0);
                        while (readytocreate == FALSE)
                        {
                                c_put_str(TERM_WHITE, "Object Name:", 6, 0);
                                sprintf(tmp, "%s          ", tmpsumm);
                                c_put_str(TERM_L_GREEN, tmp, 6, 14);                                
                                c_put_str(TERM_WHITE, "Duration:", 8, 0);
                                sprintf(tmp, "%d          ", spelltype);
                                c_put_str(TERM_L_GREEN, tmp, 8, 10);
                                c_put_str(TERM_WHITE, "Mana Cost:", 12, 0);
                                manacost = get_object_manacost(tmpsumm);
                                manacost = manacost * spelltype;
                                manacost = manacost * effcost;
                                manacost += effbase;
				manacost += ((manacost * (spelltype * 20)) / 100);
                                if (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 8] >= 1)
                                {
                                        redpercent = p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 8];
                                        if (redpercent > 75) redpercent = 75;
                                        manacost -= ((manacost * redpercent) / 100);
                                        manacost -= (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 8] * 3);
                                        if (manacost < 0) manacost = 0;
                                }
				
				/* Mastery of Conjuration spells! */
				if (p_ptr->skill_base[25] >= 100) manacost = (manacost / 2);
				else if (p_ptr->skill_base[25] >= 50) manacost = (manacost - (manacost / 3));
				else if (p_ptr->skill_base[25] >= 25) manacost = (manacost - (manacost / 4));

                                sprintf(tmp, "%ld          ", manacost);
                                c_put_str(TERM_L_GREEN, tmp, 12, 11);                                
                                c_put_str(TERM_WHITE, "[a/s] to increase/decrease duration,", 16, 0);
                                c_put_str(TERM_WHITE, "[n] to change name,", 17, 0);
                                c_put_str(TERM_WHITE, "[c] to complete creation,", 18, 0);
                                c_put_str(TERM_WHITE, "[ESC] to exit.", 19, 0);
                                choice = inkey();
                                if (choice == 'a' || choice == 'A') spelltype += 1;
                                if (choice == 's' || choice == 'S')
                                {
                                        if (spelltype > 1) spelltype -= 1;
                                }
                                if (choice == 'n' || choice == 'N')
                                {
                                        /* Go to the "name" field */
                                        move_cursor(6, 14);

                                        /* Get an input, ignore "Escape" */
                                        if (askfor_aux(tmpsumm, 29))
                                        {                                
                                                if (effectschoose == 0) strcpy(spell_ptr->sspeci1, tmpsumm);
                                                if (effectschoose == 1) strcpy(spell_ptr->sspeci2, tmpsumm);
                                                if (effectschoose == 2) strcpy(spell_ptr->sspeci3, tmpsumm);
                                                if (effectschoose == 3) strcpy(spell_ptr->sspeci4, tmpsumm);
                                                if (effectschoose == 4) strcpy(spell_ptr->sspeci5, tmpsumm);
                                        }
                                }
                                if (choice == 'c' || choice == 'C') readytocreate = TRUE;
                                if (choice == 'x' || choice == 'X' || choice == ESCAPE)
                                {
                                        Term_load();
                                        return;
                                }
                        }
                        }


                }
                /* Divinations! */
                else if (query == '5')
                {
                        bool chosen = FALSE;
                        bool readytocreate = FALSE;
                        char choseneffect[30];                        
                        char choice;
                        int interfacetype;

                        /* Initialise some stuff */
                        effbase = 0;
                        effcost = 0;
                        efftype = 0;
                        rad = 0;
                        power = 0;
                        spelltype = 1;
                        manacost = 0;

                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "PORTRALIS SPELL CREATION", 0, 0);
        		c_put_str(TERM_WHITE, "--------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Select an effect:", 4, 0);

                        /* All right, let's prepare the list! :) */
                        if (p_ptr->divination_effects & DIVI_DETECT_MONSTERS) c_put_str(TERM_WHITE, "[0] Detect Monsters", 7, 0);
                        else c_put_str(TERM_L_DARK, "[0] Detect Monsters", 7, 0);
                        if (p_ptr->divination_effects & DIVI_DETECT_OBJECTS) c_put_str(TERM_WHITE, "[1] Detect Objects", 8, 0);
                        else c_put_str(TERM_L_DARK, "[1] Detect Objects", 8, 0);
                        if (p_ptr->divination_effects & DIVI_DETECT_DOORS) c_put_str(TERM_WHITE, "[2] Detect Doors", 9, 0);
                        else c_put_str(TERM_L_DARK, "[2] Detect Doors", 9, 0);
                        if (p_ptr->divination_effects & DIVI_DETECT_STAIRS) c_put_str(TERM_WHITE, "[3] Detect Stairs", 10, 0);
                        else c_put_str(TERM_L_DARK, "[3] Detect Stairs", 10, 0);
                        if (p_ptr->divination_effects & DIVI_DETECT_CHESTS) c_put_str(TERM_WHITE, "[4] Detect Chests", 11, 0);
                        else c_put_str(TERM_L_DARK, "[4] Detect Chests", 11, 0);
                        if (p_ptr->divination_effects & DIVI_TELEPATHY) c_put_str(TERM_WHITE, "[5] Telepathy", 12, 0);
                        else c_put_str(TERM_L_DARK, "[5] Telepathy", 12, 0);
                        if (p_ptr->divination_effects & DIVI_IDENTIFY) c_put_str(TERM_WHITE, "[6] Identify", 13, 0);
                        else c_put_str(TERM_L_DARK, "[6] Identify", 13, 0);
                        if (p_ptr->divination_effects & DIVI_SCAN_MONSTER) c_put_str(TERM_WHITE, "[7] Scan Monster", 14, 0);
                        else c_put_str(TERM_L_DARK, "[7] Scan Monster", 14, 0);
                        if (p_ptr->divination_effects & DIVI_REVEAL) c_put_str(TERM_WHITE, "[8] Reveal", 15, 0);
                        else c_put_str(TERM_L_DARK, "[8] Reveal", 15, 0);
			if (p_ptr->divination_effects & DIVI_DIVINATION) c_put_str(TERM_WHITE, "[9] Divination", 16, 0);
                        else c_put_str(TERM_L_DARK, "[9] Divination", 16, 0);
			if (p_ptr->divination_effects & DIVI_RESTORE_FATE) c_put_str(TERM_WHITE, "[a] Restore Fate", 17, 0);
                        else c_put_str(TERM_L_DARK, "[a] Restore Fate", 17, 0);
			if (p_ptr->divination_effects & DIVI_FATE_MONSTERS) c_put_str(TERM_WHITE, "[b] Twist Fate: Monsters", 18, 0);
                        else c_put_str(TERM_L_DARK, "[b] Twist Fate: Monsters", 18, 0);
			if (p_ptr->divination_effects & DIVI_FATE_ITEMS) c_put_str(TERM_WHITE, "[c] Twist Fate: Items", 19, 0);
                        else c_put_str(TERM_L_DARK, "[c] Twist Fate: Items", 19, 0);
			if (p_ptr->divination_effects & DIVI_FATE_DUNGEONS) c_put_str(TERM_WHITE, "[d] Twist Fate: Random Dungeons", 7, 28);
                        else c_put_str(TERM_L_DARK, "[d] Twist Fate: Random Dungeons", 7, 28);


                        while (chosen == FALSE)
                        {
                                choice = inkey();
                                if (choice == '0')
                                {
                                        if (!(p_ptr->divination_effects & DIVI_DETECT_MONSTERS)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 1;
                                                effbase = 10;
                                                effcost = 0;
                                                effkind = 16;
                                                interfacetype = 1;
                                                sprintf(choseneffect, "Detect Monsters");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '1')
                                {
                                        if (!(p_ptr->divination_effects & DIVI_DETECT_OBJECTS)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 2;
                                                effbase = 10;
                                                effcost = 0;
                                                effkind = 16;
                                                interfacetype = 1;
                                                sprintf(choseneffect, "Detect Objects");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '2')
                                {
                                        if (!(p_ptr->divination_effects & DIVI_DETECT_DOORS)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 3;
                                                effbase = 10;
                                                effcost = 0;
                                                effkind = 16;
                                                interfacetype = 1;
                                                sprintf(choseneffect, "Detect Doors");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '3')
                                {
                                        if (!(p_ptr->divination_effects & DIVI_DETECT_STAIRS)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 4;
                                                effbase = 10;
                                                effcost = 0;
                                                effkind = 16;
                                                interfacetype = 1;
                                                sprintf(choseneffect, "Detect Stairs");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '4')
                                {
                                        if (!(p_ptr->divination_effects & DIVI_DETECT_CHESTS)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 5;
                                                effbase = 10;
                                                effcost = 0;
                                                effkind = 16;
                                                interfacetype = 1;
                                                sprintf(choseneffect, "Detect Chests");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '5')
                                {
                                        if (!(p_ptr->divination_effects & DIVI_TELEPATHY)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 0;
                                                effbase = 0;
                                                effcost = 5;
                                                effkind = 17;
                                                interfacetype = 2;
                                                sprintf(choseneffect, "Telepathy");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '6')
                                {
                                        if (!(p_ptr->divination_effects & DIVI_IDENTIFY)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 0;
                                                effbase = 10;
                                                effcost = 0;
                                                effkind = 18;
                                                interfacetype = 1;
                                                sprintf(choseneffect, "Identify");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '7')
                                {
                                        if (!(p_ptr->divination_effects & DIVI_SCAN_MONSTER)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 0;
                                                effbase = 40;
                                                effcost = 0;
                                                effkind = 19;
                                                interfacetype = 1;
                                                sprintf(choseneffect, "Scan Monster");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '8')
                                {
                                        if (!(p_ptr->divination_effects & DIVI_REVEAL)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 0;
                                                effbase = 20;
                                                effcost = 0;
                                                effkind = 20;
                                                interfacetype = 3;
                                                sprintf(choseneffect, "Reveal");
                                                chosen = TRUE;
                                        }
                                }
				else if (choice == '9')
				{
					if (!(p_ptr->divination_effects & DIVI_DIVINATION)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
						efftype = GF_DIVINATION;
                                        	effbase = 0;
                                        	effcost = 8;
                                        	effkind = 2;
                                        	interfacetype = 4;
                                        	sprintf(choseneffect, "Divination");
                                        	chosen = TRUE;
					}
				}
				else if (choice == 'a')
                                {
                                        if (!(p_ptr->divination_effects & DIVI_RESTORE_FATE)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 0;
                                                effbase = 50;
                                                effcost = 0;
                                                effkind = 24;
                                                interfacetype = 1;
                                                sprintf(choseneffect, "Restore Fate");
                                                chosen = TRUE;
                                        }
                                }
				else if (choice == 'b')
                                {
                                        if (!(p_ptr->divination_effects & DIVI_FATE_MONSTERS)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 15;
                                                effbase = 200;
                                                effcost = 0;
                                                effkind = 25;
                                                interfacetype = 5;
                                                sprintf(choseneffect, "Twist Fate: Monsters");
                                                chosen = TRUE;
                                        }
                                }
				else if (choice == 'c')
                                {
                                        if (!(p_ptr->divination_effects & DIVI_FATE_ITEMS)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 0;
                                                effbase = 300;
                                                effcost = 0;
                                                effkind = 26;
                                                interfacetype = 6;
                                                sprintf(choseneffect, "Twist Fate: Items");
                                                chosen = TRUE;
                                        }
                                }
				else if (choice == 'd')
                                {
                                        if (!(p_ptr->divination_effects & DIVI_FATE_DUNGEONS)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 0;
                                                effbase = 0;
                                                effcost = 0;
                                                effkind = 27;
                                                interfacetype = 7;
                                                sprintf(choseneffect, "Twist Fate: Random Dungeons");
                                                chosen = TRUE;
                                        }
                                }
                                else
                                {
                                        Term_load();
                                        return;
                                }
                        }
                        /* Choose the appropriate type of interface */
                        /* Depending on the spell. */

                        /* Empty interface */
                        /* You see the mana cost...that's all. */
                        if (interfacetype == 1)
                        {

                        /* We've chosen AND prepared the effect! :) */
                        /* Let's create the actual spell now! :) */
                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "PORTRALIS SPELL CREATION", 0, 0);
        		c_put_str(TERM_WHITE, "--------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Selected effect:", 4, 0);
                        c_put_str(TERM_WHITE, choseneffect, 4, 17);

                        spelltype = 0;
                        while (readytocreate == FALSE)
                        {
                                c_put_str(TERM_WHITE, "Mana Cost:", 12, 0);
                                manacost = (effbase + ((effcost * power)));

				if (p_ptr->abilities[(CLASS_DIVINER * 10) + 6] >= 1)
                                {
                                        redpercent = p_ptr->abilities[(CLASS_DIVINER * 10) + 6];
                                        if (redpercent > 75) redpercent = 75;
                                        manacost -= ((manacost * redpercent) / 100);
                                        manacost -= (p_ptr->abilities[(CLASS_DIVINER * 10) + 6] * 3);
                                        if (manacost < 0) manacost = 0;
                                }

				/* Mastery of Divination spells! */
				if (p_ptr->skill_base[26] >= 100) manacost = (manacost / 2);
				else if (p_ptr->skill_base[26] >= 50) manacost = (manacost - (manacost / 3));
				else if (p_ptr->skill_base[26] >= 25) manacost = (manacost - (manacost / 4));
				manacost -= p_ptr->skill[26];

				if (manacost < 0) manacost = 0;
                                sprintf(tmp, "%ld          ", manacost);
                                c_put_str(TERM_L_GREEN, tmp, 12, 11);
                                c_put_str(TERM_WHITE, "[c] to complete creation,", 18, 0);
                                c_put_str(TERM_WHITE, "[ESC] to exit.", 19, 0);
                                choice = inkey();
                                if (choice == 'c' || choice == 'C') readytocreate = TRUE;
                                if (choice == 'x' || choice == 'X' || choice == ESCAPE)
                                {
                                        Term_load();
                                        return;
                                }
                        }
                        }

                        /* 2. Power only interface */
                        if (interfacetype == 2)
                        {

                        /* We've chosen AND prepared the effect! :) */
                        /* Let's create the actual spell now! :) */
                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "PORTRALIS SPELL CREATION", 0, 0);
        		c_put_str(TERM_WHITE, "--------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Selected effect:", 4, 0);
                        c_put_str(TERM_WHITE, choseneffect, 4, 17);

                        while (readytocreate == FALSE)
                        {
                                c_put_str(TERM_WHITE, "Power:", 6, 0);
                                sprintf(tmp, "%d          ", power);
                                c_put_str(TERM_L_GREEN, tmp, 6, 7);
                                c_put_str(TERM_WHITE, "Mana Cost:", 8, 0);
                                manacost = (effbase + ((effcost * power)));
                                if (manacost > 10) manacost *= (manacost / 5);

				if (p_ptr->abilities[(CLASS_DIVINER * 10) + 6] >= 1)
                                {
                                        redpercent = p_ptr->abilities[(CLASS_DIVINER * 10) + 6];
                                        if (redpercent > 75) redpercent = 75;
                                        manacost -= ((manacost * redpercent) / 100);
                                        manacost -= (p_ptr->abilities[(CLASS_DIVINER * 10) + 6] * 3);
                                        if (manacost < 0) manacost = 0;
                                }

				/* Mastery of Divination spells! */
				if (p_ptr->skill_base[26] >= 100) manacost = (manacost / 2);
				else if (p_ptr->skill_base[26] >= 50) manacost = (manacost - (manacost / 3));
				else if (p_ptr->skill_base[26] >= 25) manacost = (manacost - (manacost / 4));
				manacost -= p_ptr->skill[26];

				if (manacost < 0) manacost = 0;
                                sprintf(tmp, "%ld          ", manacost);
                                c_put_str(TERM_L_GREEN, tmp, 8, 11);
                                c_put_str(TERM_WHITE, "[q/w] to increase/decrease power,", 17, 0);
                                c_put_str(TERM_WHITE, "[c] to complete creation,", 18, 0);
                                c_put_str(TERM_WHITE, "[ESC] to exit.", 19, 0);
                                choice = inkey();
                                if (choice == 'q' || choice == 'Q') power += 1;
                                if (choice == 'w' || choice == 'W')
                                {
                                        if (power > 0) power -= 1;
                                }
                                if (choice == 'c' || choice == 'C') readytocreate = TRUE;
                                if (choice == 'x' || choice == 'X' || choice == ESCAPE)
                                {
                                        Term_load();
                                        return;
                                }
                        }
                        }

                        /* Reveal */
                        if (interfacetype == 3)
                        {

                        /* We've chosen AND prepared the effect! :) */
                        /* Let's create the actual spell now! :) */
                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "PORTRALIS SPELL CREATION", 0, 0);
        		c_put_str(TERM_WHITE, "--------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Selected effect:", 4, 0);
                        c_put_str(TERM_WHITE, choseneffect, 4, 17);

                        while (readytocreate == FALSE)
                        {
                                power = 0;
                                c_put_str(TERM_WHITE, "Radius:", 6, 0);
                                sprintf(tmp, "%d          ", rad);
                                c_put_str(TERM_L_GREEN, tmp, 6, 8);
                                c_put_str(TERM_WHITE, "Type:", 8, 0);
                                if (spelltype == 1) sprintf(tmp, "Circle      ");
                                else if (spelltype == 2) sprintf(tmp, "Free Placing");
                                c_put_str(TERM_L_GREEN, tmp, 8, 6);
                                c_put_str(TERM_WHITE, "Mana Cost:", 12, 0);
                                manacost = (effbase + ((effcost * power))) * (rad + 1);
                                if (spelltype == 2) manacost *= 2;

				if (p_ptr->abilities[(CLASS_DIVINER * 10) + 6] >= 1)
                                {
                                        redpercent = p_ptr->abilities[(CLASS_DIVINER * 10) + 6];
                                        if (redpercent > 75) redpercent = 75;
                                        manacost -= ((manacost * redpercent) / 100);
                                        manacost -= (p_ptr->abilities[(CLASS_DIVINER * 10) + 6] * 3);
                                        if (manacost < 0) manacost = 0;
                                }

				/* Mastery of Divination spells! */
				if (p_ptr->skill_base[26] >= 100) manacost = (manacost / 2);
				else if (p_ptr->skill_base[26] >= 50) manacost = (manacost - (manacost / 3));
				else if (p_ptr->skill_base[26] >= 25) manacost = (manacost - (manacost / 4));
				manacost -= p_ptr->skill[26];

				if (manacost < 0) manacost = 0;
                                sprintf(tmp, "%ld          ", manacost);
                                c_put_str(TERM_L_GREEN, tmp, 12, 11);                               
                                c_put_str(TERM_WHITE, "[a/s] to increase/decrease radius,", 16, 0);
                                c_put_str(TERM_WHITE, "[t] to change type,", 17, 0);
                                c_put_str(TERM_WHITE, "[c] to complete creation,", 18, 0);
                                c_put_str(TERM_WHITE, "[ESC] to exit.", 19, 0);
                                choice = inkey();
                                if (choice == 'a' || choice == 'a') rad += 1;
                                if (choice == 's' || choice == 'S')
                                {
                                        if (rad > 0) rad -= 1;
                                }
                                if (choice == 't' || choice == 'T')
                                {
                                        spelltype += 1;
                                        if (spelltype > 2) spelltype = 1;
                                }
                                if (choice == 'c' || choice == 'C') readytocreate = TRUE;
                                if (choice == 'x' || choice == 'X' || choice == ESCAPE)
                                {
                                        Term_load();
                                        return;
                                }
                        }
                        }

			/* Standard damages interface */
                        if (interfacetype == 4)
                        {

                        /* We've chosen AND prepared the effect! :) */
                        /* Let's create the actual spell now! :) */
                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "PORTRALIS SPELL CREATION", 0, 0);
        		c_put_str(TERM_WHITE, "--------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Selected effect:", 4, 0);
                        c_put_str(TERM_WHITE, choseneffect, 4, 17);

                        while (readytocreate == FALSE)
                        {
                                c_put_str(TERM_WHITE, "Power:", 6, 0);
                                sprintf(tmp, "%d          ", power);
                                c_put_str(TERM_L_GREEN, tmp, 6, 7);
                                c_put_str(TERM_WHITE, "Radius:", 8, 0);
                                sprintf(tmp, "%d          ", rad);
                                c_put_str(TERM_L_GREEN, tmp, 8, 8);
                                c_put_str(TERM_WHITE, "Type:", 10, 0);
                                if (spelltype == 1) sprintf(tmp, "Bolt  ");
                                else if (spelltype == 2) sprintf(tmp, "Ball  ");
                                else if (spelltype == 3) sprintf(tmp, "Circle");
                                else if (spelltype == 4) sprintf(tmp, "Chain ");
                                c_put_str(TERM_L_GREEN, tmp, 10, 6);
                                c_put_str(TERM_WHITE, "Mana Cost:", 12, 0);
                                /* manacost = (effbase + ((effcost * power))) * (rad + 1);*/

				manacost = (effbase + ((effcost * power) / 2));
                                manacost += ((manacost * (power * 3)) / 100);
                                manacost = manacost * (rad + 1);

                                if (spelltype == 4)
                                {
                                        int a;
                                        manacost *= 5;
                                        for (a = 0; a < rad; a++)
                                        {
                                                manacost += (50 * rad);
                                        }                                        
                                }

				if (p_ptr->abilities[(CLASS_DIVINER * 10) + 6] >= 1)
                                {
                                        redpercent = p_ptr->abilities[(CLASS_DIVINER * 10) + 6];
                                        if (redpercent > 75) redpercent = 75;
                                        manacost -= ((manacost * redpercent) / 100);
                                        manacost -= (p_ptr->abilities[(CLASS_DIVINER * 10) + 6] * 3);
                                        if (manacost < 0) manacost = 0;
                                }
                                
				/* Mastery of Divination spells! */
				if (p_ptr->skill_base[26] >= 100) manacost = (manacost / 2);
				else if (p_ptr->skill_base[26] >= 50) manacost = (manacost - (manacost / 3));
				else if (p_ptr->skill_base[26] >= 25) manacost = (manacost - (manacost / 4));
				manacost -= p_ptr->skill[26];

				if (manacost < 0) manacost = 0;

                                sprintf(tmp, "%ld          ", manacost);
                                c_put_str(TERM_L_GREEN, tmp, 12, 11);
                                c_put_str(TERM_WHITE, "[q/w] to increase/decrease power,", 16, 0);
                                c_put_str(TERM_WHITE, "[a/s] to increase/decrease radius,", 17, 0);
                                c_put_str(TERM_WHITE, "[t] to change type,", 18, 0);
                                c_put_str(TERM_WHITE, "[c] to complete creation,", 19, 0);
                                c_put_str(TERM_WHITE, "[ESC] to exit.", 20, 0);
                                choice = inkey();
                                if (choice == 'q' || choice == 'Q') power += 1;
                                if (choice == 'w' || choice == 'W')
                                {
                                        if (power > 0) power -= 1;
                                }
                                if (choice == 'a' || choice == 'A')
                                {
                                        rad += 1;
                                        if (spelltype == 1) spelltype = 2;
                                }
                                if (choice == 's' || choice == 'S')
                                {
                                        if (rad > 0) rad -= 1;
                                        if (spelltype == 2 && rad == 0) spelltype = 1;
                                }
                                if (choice == 't' || choice == 'T')
                                {
                                        spelltype += 1;
                                        if (spelltype > 4) spelltype = 1;
                                        if (spelltype == 1 && rad > 0) spelltype += 1;
                                        if (spelltype == 1) rad = 0;
                                        if (spelltype == 2 && rad == 0) spelltype += 1;
                                }
                                if (choice == 'c' || choice == 'C') readytocreate = TRUE;
                                if (choice == 'x' || choice == 'X' || choice == ESCAPE)
                                {
                                        Term_load();
                                        return;
                                }
                        }
                        }

			/* 5. Twist Fate: Monsters interface. */
                        if (interfacetype == 5)
                        {
			s16b difference = 0;
			power = 0;
			spelltype = 0;
			efftype = 15;

                        /* We've chosen AND prepared the effect! :) */
                        /* Let's create the actual spell now! :) */
                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "PORTRALIS SPELL CREATION", 0, 0);
        		c_put_str(TERM_WHITE, "--------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Selected effect:", 4, 0);
                        c_put_str(TERM_WHITE, choseneffect, 4, 17);

                        while (readytocreate == FALSE)
                        {
                                c_put_str(TERM_WHITE, "Monster level will be affected by: ", 6, 0);
                                sprintf(tmp, "%d          ", spelltype);
                                c_put_str(TERM_L_GREEN, tmp, 6, 35);
				c_put_str(TERM_WHITE, "Monster depth will be affected by: ", 7, 0);
                                sprintf(tmp, "%d          ", power);
                                c_put_str(TERM_L_GREEN, tmp, 7, 35);
				c_put_str(TERM_WHITE, "Chance of being an elite/boss is : ", 8, 0);
                                sprintf(tmp, "%d          ", efftype);
                                c_put_str(TERM_L_GREEN, tmp, 8, 35);


                                c_put_str(TERM_WHITE, "Mana Cost:", 10, 0);

				c_put_str(TERM_WHITE, "Twisting fate is not 100% accurate, and depends on your Divination skill.", 12, 0);
				c_put_str(TERM_WHITE, "The lower you go in the dungeon, the more difficult it is.", 13, 0);
				c_put_str(TERM_WHITE, "Changes to monsters level only works on dungeon levels", 14, 0);
				c_put_str(TERM_WHITE, "that are at least half your level.", 15, 0);
                                manacost = effbase;

				/* Monster level */
				difference = 0;
				if (spelltype > 0)
				{
					difference = spelltype;
					manacost += difference * 15;
				}
				if (spelltype < 0)
				{
					difference = spelltype * (-1);
					manacost += difference * 15;
				}
				manacost += ((manacost * (difference * 5)) / 100);

				/* Monster Depth */
				difference = 0;
				if (power > 0)
				{
					difference = power;
					manacost += difference * 50;
				}
				if (power < 0)
				{
					difference = power * (-1);
					manacost += difference * 50;
				}
				manacost += ((manacost * (difference * 5)) / 100);

				/* Elites/Boss */
				difference = 0;
				if (efftype > 15)
				{
					difference = efftype - 15;
					manacost += difference * 25;
				}
				if (efftype < 15)
				{
					difference = 15 - efftype;
					manacost += difference * 300;
				}
				manacost += ((manacost * (difference * 5)) / 100);

				if (p_ptr->abilities[(CLASS_DIVINER * 10) + 6] >= 1)
                                {
                                        redpercent = p_ptr->abilities[(CLASS_DIVINER * 10) + 6];
                                        if (redpercent > 75) redpercent = 75;
                                        manacost -= ((manacost * redpercent) / 100);
                                        manacost -= (p_ptr->abilities[(CLASS_DIVINER * 10) + 6] * 3);
                                        if (manacost < 0) manacost = 0;
                                }

				/* Mastery of Divination spells! */
				if (p_ptr->skill_base[26] >= 100) manacost = (manacost / 2);
				else if (p_ptr->skill_base[26] >= 50) manacost = (manacost - (manacost / 3));
				else if (p_ptr->skill_base[26] >= 25) manacost = (manacost - (manacost / 4));
				manacost -= p_ptr->skill[26];

				if (manacost < 0) manacost = 0;
                                sprintf(tmp, "%ld          ", manacost);
                                c_put_str(TERM_L_GREEN, tmp, 10, 11);
                                c_put_str(TERM_WHITE, "[q/w] to increase/decrease level,", 17, 0);
				c_put_str(TERM_WHITE, "[a/s] to increase/decrease depth,", 18, 0);
				c_put_str(TERM_WHITE, "[z/x] to increase/decrease elites%,", 19, 0);
                                c_put_str(TERM_WHITE, "[c] to complete creation,", 20, 0);
                                c_put_str(TERM_WHITE, "[ESC] to exit.", 21, 0);
                                choice = inkey();
                                if (choice == 'q' || choice == 'Q')
				{
					if (spelltype < 200) spelltype += 1;
				}
                                if (choice == 'w' || choice == 'W')
                                {
                                        if (spelltype > -200) spelltype -= 1;
                                }
				if (choice == 'a' || choice == 'A')
				{
					if (power < 200) power += 1;
				}
                                if (choice == 's' || choice == 'S')
                                {
                                        if (power > -200) power -= 1;
                                }
				if (choice == 'z' || choice == 'Z')
				{
					if (efftype < 100) efftype += 1;
				}
                                if (choice == 'x' || choice == 'X')
                                {
                                        if (efftype > 0) efftype -= 1;
                                }
                                if (choice == 'c' || choice == 'C') readytocreate = TRUE;
                                if (choice == ESCAPE)
                                {
                                        Term_load();
                                        return;
                                }
                        }
                        }

			/* 6. Twist Fate: Items interface. */
                        if (interfacetype == 6)
                        {
			s32b basemanacost = 0;
			s16b difference = 0;
			power = 1;
			spelltype = 0;
			efftype = 0;

                        /* We've chosen AND prepared the effect! :) */
                        /* Let's create the actual spell now! :) */
                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "PORTRALIS SPELL CREATION", 0, 0);
        		c_put_str(TERM_WHITE, "--------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Selected effect:", 4, 0);
                        c_put_str(TERM_WHITE, choseneffect, 4, 17);

                        while (readytocreate == FALSE)
                        {
                                c_put_str(TERM_WHITE, "Items depth will be affected by: ", 6, 0);
                                sprintf(tmp, "%d          ", spelltype);
                                c_put_str(TERM_L_GREEN, tmp, 6, 33);
				c_put_str(TERM_WHITE, "Items quality will be: ", 7, 0);

				if (power == 1) sprintf(tmp, "Normal          ");
				if (power == 2) sprintf(tmp, "Magic           ");
				if (power == 3) sprintf(tmp, "Levelable       ");
				if (power == 4) sprintf(tmp, "Special         ");
                                c_put_str(TERM_L_GREEN, tmp, 7, 23);

				c_put_str(TERM_WHITE, "Magic items rank will be affected by: ", 8, 0);
                                sprintf(tmp, "%d          ", efftype);
                                c_put_str(TERM_L_GREEN, tmp, 8, 38);


                                c_put_str(TERM_WHITE, "Mana Cost:", 10, 0);

				c_put_str(TERM_WHITE, "Twisting fate is not 100% accurate, and depends on your Divination skill.", 12, 0);
				c_put_str(TERM_WHITE, "The lower you go in the dungeon, the more difficult it is.", 13, 0);

                                manacost = effbase;

				/* Item quality */
				if (power == 2) manacost += 2000;
				if (power == 3) manacost += 10000;
				if (power == 4) manacost += 100000;

				/* Item depth */
				difference = 0;
				if (spelltype > 0)
				{
					difference = spelltype;
					manacost += difference * 50;
				}
				if (spelltype < 0)
				{
					difference = spelltype * (-1);
					manacost -= difference * 10;
				}
				if (spelltype >= 0) manacost += ((manacost * (difference * 10)) / 100);
				else
				{
					basemanacost = manacost;
					manacost -= ((manacost * (difference)) / 100);
					if (manacost < (basemanacost / 2)) manacost = (basemanacost / 2);
				}

				/* Item rank */
				difference = 0;
				if (efftype > 0)
				{
					difference = efftype;
					manacost += difference * 50;
				}
				if (efftype < 0)
				{
					difference = efftype * (-1);
					manacost -= difference * 10;
				}
				if (efftype >= 0) manacost += ((manacost * (difference * 10)) / 100);
				else
				{
					basemanacost = manacost;
					manacost -= ((manacost * (difference)) / 100);
					if (manacost < (basemanacost / 2)) manacost = (basemanacost / 2);
				}

				if (p_ptr->abilities[(CLASS_DIVINER * 10) + 6] >= 1)
                                {
                                        redpercent = p_ptr->abilities[(CLASS_DIVINER * 10) + 6];
                                        if (redpercent > 75) redpercent = 75;
                                        manacost -= ((manacost * redpercent) / 100);
                                        manacost -= (p_ptr->abilities[(CLASS_DIVINER * 10) + 6] * 3);
                                        if (manacost < 0) manacost = 0;
                                }

				/* Mastery of Divination spells! */
				if (p_ptr->skill_base[26] >= 100) manacost = (manacost / 2);
				else if (p_ptr->skill_base[26] >= 50) manacost = (manacost - (manacost / 3));
				else if (p_ptr->skill_base[26] >= 25) manacost = (manacost - (manacost / 4));
				manacost -= p_ptr->skill[26];

				if (manacost < 0) manacost = 0;
                                sprintf(tmp, "%ld          ", manacost);
                                c_put_str(TERM_L_GREEN, tmp, 10, 11);
                                c_put_str(TERM_WHITE, "[q/w] to increase/decrease depth,", 16, 0);
				c_put_str(TERM_WHITE, "[a/s] to change items quality,", 17, 0);
				c_put_str(TERM_WHITE, "[z/x] to change items rank,", 18, 0);
                                c_put_str(TERM_WHITE, "[c] to complete creation,", 19, 0);
                                c_put_str(TERM_WHITE, "[ESC] to exit.", 20, 0);
                                choice = inkey();
                                if (choice == 'q' || choice == 'Q')
				{
					if (spelltype < 100) spelltype += 1;
				}
                                if (choice == 'w' || choice == 'W')
                                {
                                        if (spelltype > -100) spelltype -= 1;
                                }
				if (choice == 'a' || choice == 'A')
				{
					power += 1;
					if (power > 4) power = 1;
				}
                                if (choice == 's' || choice == 'S')
                                {
                                        power -= 1;
					if (power <= 0) power = 4;
                                }
				if (choice == 'z' || choice == 'Z')
				{
					if (efftype < 100) efftype += 1;
				}
                                if (choice == 'x' || choice == 'X')
                                {
                                        if (efftype > -100) efftype -= 1;
                                }
				
                                if (choice == 'c' || choice == 'C') readytocreate = TRUE;
                                if (choice == ESCAPE)
                                {
                                        Term_load();
                                        return;
                                }
                        }
                        }

			/* 7. Twist Fate: Random Dungeons interface. */
                        if (interfacetype == 7)
                        {
			s32b basemanacost = 0;
			s16b difference = 0;
			power = 1;
			spelltype = 1;
			efftype = 0;

                        /* We've chosen AND prepared the effect! :) */
                        /* Let's create the actual spell now! :) */
                        /* Perpare the screen */
                        c_put_str(TERM_WHITE, "", 0, 0);
                        for (i = 0; i < SCREEN_HGT; i++)
                        {
                                roff("\n");
                        }
                        c_put_str(TERM_WHITE, "PORTRALIS SPELL CREATION", 0, 0);
        		c_put_str(TERM_WHITE, "--------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Selected effect:", 4, 0);
                        c_put_str(TERM_WHITE, choseneffect, 4, 17);

                        while (readytocreate == FALSE)
                        {
                                c_put_str(TERM_WHITE, "Dungeon's starting depth: ", 6, 0);
                                sprintf(tmp, "%d          ", spelltype);
                                c_put_str(TERM_L_GREEN, tmp, 6, 26);

				c_put_str(TERM_WHITE, "Dungeon's ending depth  : ", 7, 0);
                                sprintf(tmp, "%d          ", power);
                                c_put_str(TERM_L_GREEN, tmp, 7, 26);
				
				c_put_str(TERM_WHITE, "Boss difficulty modifier: ", 8, 0);
                                sprintf(tmp, "%d          ", efftype);
                                c_put_str(TERM_L_GREEN, tmp, 8, 26);


                                c_put_str(TERM_WHITE, "Mana Cost:", 10, 0);

				c_put_str(TERM_WHITE, "This spell will affect any future randomly generated dungeons.", 12, 0);
				c_put_str(TERM_WHITE, "If you cast it in the wild, you'll have to reenter the outdoor", 13, 0);
				c_put_str(TERM_WHITE, "cell for the new dungeon to be generated.", 14, 0);

                                manacost = effbase;

				/* Minimum depth cost increase. */
				manacost += spelltype * 3;

				/* Maximum depth cost increase. */
				manacost += power;

				manacost += ((manacost * ((spelltype + power) * 50)) / 100);

				/* Boss Modifier */
				difference = 0;
				if (efftype > 0)
				{
					difference = efftype;
					manacost += difference * 10;
				}
				if (efftype < 0)
				{
					difference = efftype * (-1);
					manacost += difference * 10;
				}
				manacost += ((manacost * (difference * 5)) / 100);
				
				if (p_ptr->abilities[(CLASS_DIVINER * 10) + 6] >= 1)
                                {
                                        redpercent = p_ptr->abilities[(CLASS_DIVINER * 10) + 6];
                                        if (redpercent > 75) redpercent = 75;
                                        manacost -= ((manacost * redpercent) / 100);
                                        manacost -= (p_ptr->abilities[(CLASS_DIVINER * 10) + 6] * 3);
                                        if (manacost < 0) manacost = 0;
                                }

				/* Mastery of Divination spells! */
				if (p_ptr->skill_base[26] >= 100) manacost = (manacost / 2);
				else if (p_ptr->skill_base[26] >= 50) manacost = (manacost - (manacost / 3));
				else if (p_ptr->skill_base[26] >= 25) manacost = (manacost - (manacost / 4));
				manacost -= p_ptr->skill[26];

				if (manacost < 0) manacost = 0;
                                sprintf(tmp, "%ld          ", manacost);
                                c_put_str(TERM_L_GREEN, tmp, 10, 11);
                                c_put_str(TERM_WHITE, "[q/w] to increase/decrease depth,", 16, 0);
				c_put_str(TERM_WHITE, "[a/s] to change items quality,", 17, 0);
				c_put_str(TERM_WHITE, "[z/x] to change items rank,", 18, 0);
                                c_put_str(TERM_WHITE, "[c] to complete creation,", 19, 0);
                                c_put_str(TERM_WHITE, "[ESC] to exit.", 20, 0);
                                choice = inkey();
                                if (choice == 'q' || choice == 'Q')
				{
					if (spelltype < 200) spelltype += 1;
					if (power < spelltype) power = spelltype;
				}
                                if (choice == 'w' || choice == 'W')
                                {
                                        if (spelltype > 1) spelltype -= 1;
					if (power < spelltype) power = spelltype;
                                }
				if (choice == 'a' || choice == 'A')
				{
					if (power < 200) power += 1;
					if (power < spelltype) power = spelltype;
				}
                                if (choice == 's' || choice == 'S')
                                {
                                        if (power > 1) power -= 1;
					if (power < spelltype) power = spelltype;
                                }
				if (choice == 'z' || choice == 'Z')
				{
					if (efftype < 100) efftype += 1;
				}
                                if (choice == 'x' || choice == 'X')
                                {
                                        if (efftype > -100) efftype -= 1;
                                }
				
                                if (choice == 'c' || choice == 'C') readytocreate = TRUE;
                                if (choice == ESCAPE)
                                {
                                        Term_load();
                                        return;
                                }
                        }
                        }


                }




                spell_ptr->school[effectschoose] = 1;
                spell_ptr->power[effectschoose] = power;
                spell_ptr->radius[effectschoose] = rad;
                spell_ptr->type[effectschoose] = efftype;
                spell_ptr->shape[effectschoose] = spelltype;
                spell_ptr->effect[effectschoose] = effkind;
                spell_ptr->manacost[effectschoose] = manacost;
                if (effectschoose < 5)
                {
                        if (!get_com("Do you want to add another effect? [y/n]", &query))
                        {
                                Term_load();
                                return;
                        }
                        if (query == 'y' || query == 'Y')
                        {
                                effectschoose += 1;
                                effnum += 1;
                        }
                        else effectschoose = 5;
                }
        }


        /* We're done with the creation! */
        /* Now, let's proceed to the final part! :) */
        /* Perpare the screen */
        c_put_str(TERM_WHITE, "", 0, 0);
        for (i = 0; i < SCREEN_HGT; i++)
        {
                roff("\n");
        }
        c_put_str(TERM_WHITE, "PORTRALIS SPELL CREATION", 0, 0);
        c_put_str(TERM_WHITE, "--------------------------", 1, 0);
        for (i = 0; i < 5; i++)
        {
                finalcost = finalcost + spell_ptr->manacost[i];
        }
        finalcost = finalcost * effnum;
        c_put_str(TERM_WHITE, "Final Cost:", 8, 0);
        sprintf(tmp, "%ld          ", finalcost);
        c_put_str(TERM_L_GREEN, tmp, 8, 12);
        c_put_str(TERM_WHITE, "Name:", 4, 0);

        /* Go to the "name" field */
        move_cursor(4, 6);

        /* Get an input, ignore "Escape" */
        if (askfor_aux(tmp, 29)) strcpy(spell_ptr->name, tmp);

        c_put_str(TERM_WHITE, "Press 'c' to create spell, or any key to exit.", 15, 0);

        query = inkey();
        if (query == 'c' || query == 'C')
        {
                msg_print("Spell created!");
                spell_ptr->finalcost = finalcost;
                spell_ptr->created = TRUE;
        }
	else
	{
		int k;
		for (k = 0; k < 5; k++)
		{
			spell_ptr->school[k] = 0;
                	spell_ptr->power[k] = 0;
                	spell_ptr->radius[k] = 0;
                	spell_ptr->type[k] = 0;
                	spell_ptr->shape[k] = 0;
                	spell_ptr->effect[k] = 0;
                	spell_ptr->manacost[k] = 0;
		}
		sprintf(spell_ptr->name, "--- NO SPELL ---");
		spell_ptr->created = TRUE;
	}
		
        Term_load();
	update_and_handle();
}

/* Returns the mana cost of a specific creature */
int get_name_manacost(char name[80])
{
        int x;
        int manacost = 0;
        bool cansummon = FALSE;
	
        for (x = 0; x <= max_r_idx; x++)
        {
                monster_race *r_ptr = &r_info[x];
                if (strstr((r_name + r_ptr->name), name) && !(r_ptr->flags1 & (RF1_UNIQUE)))
                {
                        cansummon = TRUE;
                        manacost = r_ptr->level * 6;
                        manacost = manacost * ((r_ptr->level / 20) + 1);
			manacost = manacost * r_ptr->rarity;
                        return (manacost);
                }
        }
        if (cansummon == FALSE) return (0);
}

/* Delete a spell */
void delete_spell()
{
	int                     Power = -1;
        int                     num = 0, i;

	int             powers[36];
	char            power_desc[36][80];

	bool            flag, redraw;
        int             ask;

        char            choice;

	char            out_val[160];
        magic_spells *spell_ptr;

        for (i = 1; i < 29; i++)
        {
                char spellstring[80];
                spell_ptr = &magic_spell[i];
                if (spell_ptr->created == TRUE)
                {
                        sprintf(spellstring, "%s   Cost: %ld", spell_ptr->name, spell_ptr->finalcost);        
                        /* List the powers */
                        strcpy(power_desc[num],spellstring);powers[num++]=i;
                }
        }

        if(!num) {msg_print("No spells to delete.");return 0;}

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt (accept all spells) */
	if (num <= 26)
	{
		/* Build a prompt (accept all spells) */
                strnfmt(out_val, 78, "(Spells %c-%c, *=List, ESC=exit) Delete which spell? ",
			I2A(0), I2A(num - 1));
	}
	else
	{
                strnfmt(out_val, 78, "(Spells %c-%c, *=List, ESC=exit) Delete which spell? ",
			I2A(0), '0' + num - 27);
	}

	/* Get a spell from the user */
	while (!flag && get_com(out_val, &choice))
	{
		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Show the list */
			if (!redraw)
			{
				byte y = 1, x = 0;
				int ctr = 0;
				char dummy[80];

				strcpy(dummy, "");

				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				Term_save();

				prt ("", y++, x);

                                while (ctr < num && ctr < 19)
				{
					sprintf(dummy, "%c) %s", I2A(ctr), power_desc[ctr]);
					prt(dummy, y + ctr, x);
					ctr++;
				}
				while (ctr < num)
				{
					if (ctr < 26)
					{
						sprintf(dummy, " %c) %s", I2A(ctr), power_desc[ctr]);
					}
					else
					{
						sprintf(dummy, " %c) %s", '0' + ctr - 26, power_desc[ctr]);
					}
                                        prt(dummy, y + ctr - 19, x + 40);
					ctr++;
				}
                                if (ctr < 19)
				{
					prt ("", y + ctr, x);
				}
				else
				{
                                        prt ("", y + 19, x);
				}
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

		if (choice == '\r' && num == 1)
		{
			choice = 'a';
		}

		if (isalpha(choice))
		{
			/* Note verify */
			ask = (isupper(choice));

			/* Lowercase */
			if (ask) choice = tolower(choice);

			/* Extract request */
			i = (islower(choice) ? A2I(choice) : -1);
		}
		else
		{
			ask = FALSE; /* Can't uppercase digits */

			i = choice - '0' + 26;
		}

		/* Totally Illegal */
		if ((i < 0) || (i >= num))
		{
			bell();
			continue;
		}

		/* Save the spell index */
		Power = powers[i];

		/* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Prompt */
                        strnfmt(tmp_val, 78, "Delete %s? ", power_desc[i]);

			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}

	/* Restore the screen */
	if (redraw) Term_load();

	/* Abort if needed */
	if (!flag) 
	{
		energy_use = 0;
                return num;
	}

        spell_ptr = &magic_spell[Power];
        for (i = 0; i < 5; i++)
        {
                spell_ptr->effect[i] = 0;
                spell_ptr->manacost[i] = 0;
		spell_ptr->school[i] = 0;
    		spell_ptr->shape[i] = 0;
    		spell_ptr->power[i] = 0;
    		spell_ptr->radius[i] = 0;
    		spell_ptr->type[i] = 0;
        }
	sprintf(spell_ptr->name, "--- NO SPELL ---");
        spell_ptr->created = TRUE;
        msg_print("Spell deleted.");
}

/* Erase all created spells! */
void delete_all_spells()
{
        int x, i;
        magic_spells *spell_ptr;

        for (x = 0; x < 29; x++)
        {
                spell_ptr = &magic_spell[x];
                for (i = 0; i < 5; i++)
                {
                        spell_ptr->effect[i] = 0;
                        spell_ptr->manacost[i] = 0;
			spell_ptr->school[i] = 0;
    			spell_ptr->shape[i] = 0;
    			spell_ptr->power[i] = 0;
    			spell_ptr->radius[i] = 0;
    			spell_ptr->type[i] = 0;
                }
		sprintf(spell_ptr->name, "--- NO SPELL ---");
                spell_ptr->created = TRUE;
        }
}        

/* Returns the mana cost of a specific item */
int get_object_manacost(char name[30])
{
        int x;
        int manacost = 0;
        bool cansummon = FALSE;

        for (x = 1; x <= max_k_idx; x++)
        {
                object_kind *k_ptr = &k_info[x];
                if (strstr((k_name + k_ptr->name), name) && !(k_ptr->flags4 & (TR4_SPECIAL_GENE)) && !(k_ptr->flags3 & (TR3_INSTA_ART)))
                {
                        cansummon = TRUE;
                        /* With this formula, junk is VERY cheap to conjure */
                        /* while great stuff is very expensive */
                        /*manacost = k_ptr->locale[0] / 5;
                        manacost += manacost * ((k_ptr->locale[0] / 11) * 4);
                        manacost = manacost / 10;*/

			manacost = k_ptr->level;

                        if (manacost < 1) manacost = 1;
                        return (manacost);
                }
        }
        if (cansummon == FALSE) return (0);
}

void conjure_specific_item(char name[30], int dur, bool magic, bool special)
{
        int x;

        for (x = 1; x <= max_k_idx; x++)
        {
                object_kind *k_ptr = &k_info[x];
                if (strstr((k_name + k_ptr->name), name) && !(k_ptr->flags4 & (TR4_SPECIAL_GENE)) && !(k_ptr->flags3 & (TR3_INSTA_ART)) && !(strstr((k_name + k_ptr->name), "Broken")))
                {
                        conjure_item(k_ptr->tval, k_ptr->sval, dur, magic, special);
                        return;
                }
        }
        msg_print("Invalid item.");
        return;
}

/* Activate an item. */
/* Now calls Lua. */
void activate_item(object_type *o_ptr)
{
	int                     Power = -1;
        int                     num = 0, dir = 0 , i, j;

	int             powers[36];
	char            power_desc[36][80];

	bool            flag, redraw;
        int             ask;

	s32b dam;

        char            choice, ch;
	bool 		artifact = FALSE;

	char            out_val[160];
        int k,count;
	object_type *q_ptr;

        cptr q, s;

	/* Is it charging? */
	if (o_ptr->timeout >= 1)
        {
                msg_print("This items still needs to recharge.");
                return;
        }

        /* List the powers */
	for (i = 0; i < 20; i++)
        {
                char spellstring[80];
		
                if (o_ptr->spell[i].type > 0)
                {
                        sprintf(spellstring, "%s   Recharge: %d turns", o_ptr->spell[i].name, o_ptr->spell[i].cost);        
                        /* List the powers */
                        strcpy(power_desc[num],spellstring);powers[num++]=i;
                }
        }

        if(!num) {msg_print("No powers to use.");return 0;}

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt (accept all spells) */
	if (num <= 26)
	{
		/* Build a prompt (accept all spells) */
                strnfmt(out_val, 78, "(Powers %c-%c, *=List, ESC=exit) Use which power? ",
			I2A(0), I2A(num - 1));
	}
	else
	{
                strnfmt(out_val, 78, "(Powers %c-%c, *=List, ESC=exit) Use which power? ",
			I2A(0), '0' + num - 27);
	}

	/* Get a spell from the user */
	while (!flag && get_com(out_val, &choice))
	{
		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Show the list */
			if (!redraw)
			{
				byte y = 1, x = 0;
				int ctr = 0;
				char dummy[80];

				strcpy(dummy, "");

				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				Term_save();

				prt ("", y++, x);

				while (ctr < num && ctr < 17)
				{
					sprintf(dummy, "%c) %s", I2A(ctr), power_desc[ctr]);
					prt(dummy, y + ctr, x);
					ctr++;
				}
				while (ctr < num)
				{
					if (ctr < 26)
					{
						sprintf(dummy, " %c) %s", I2A(ctr), power_desc[ctr]);
					}
					else
					{
						sprintf(dummy, " %c) %s", '0' + ctr - 26, power_desc[ctr]);
					}
					prt(dummy, y + ctr - 17, x + 40);
					ctr++;
				}
				if (ctr < 17)
				{
					prt ("", y + ctr, x);
				}
				else
				{
					prt ("", y + 17, x);
				}
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

		if (choice == '\r' && num == 1)
		{
			choice = 'a';
		}

		if (isalpha(choice))
		{
			/* Note verify */
			ask = (isupper(choice));

			/* Lowercase */
			if (ask) choice = tolower(choice);

			/* Extract request */
			i = (islower(choice) ? A2I(choice) : -1);
		}
		else
		{
			ask = FALSE; /* Can't uppercase digits */

			i = choice - '0' + 26;
		}

		/* Totally Illegal */
		if ((i < 0) || (i >= num))
		{
			bell();
			continue;
		}

		/* Save the spell index */
		Power = powers[i];

		/* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Prompt */
			strnfmt(tmp_val, 78, "Use %s? ", power_desc[i]);

			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}

	/* Restore the screen */
	if (redraw) Term_load();

	/* Abort if needed */
	if (!flag) 
	{
		energy_use = 0;
                return;
	}

	o_ptr->timeout = o_ptr->spell[Power].cost;
	if (o_ptr->timeout < 0) o_ptr->timeout = 0;

	call_lua("item_activation", "(Od)", "", o_ptr, Power);
}

/* Pick one of your spells, returns it's id. */
int pick_spell()
{
	int                     Power = -1;
        int                     num = 0, i;
	int			amountslots = 12;

	int             powers[36];
	char            power_desc[36][80];

        bool            flag, redraw;
        int             ask;

        char            choice;

	char            out_val[160];

        magic_spells *spell_ptr;

	if (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 3] >= 1)
        {
                amountslots += (1 + (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 3] / 5));
        }
        for (i = 1; i <= amountslots; i++)
        {
                char spellstring[80];
                spell_ptr = &magic_spell[i];
                if (spell_ptr->created == TRUE)
                {
                        sprintf(spellstring, "%s   Cost: %ld", spell_ptr->name, spell_ptr->finalcost);        
                        /* List the powers */
                        strcpy(power_desc[num],spellstring);powers[num++]=i;
                }
        }

        if(!num) {msg_print("No spells to choose.");return 0;}

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt (accept all spells) */
	if (num <= 26)
	{
		/* Build a prompt (accept all spells) */
                strnfmt(out_val, 78, "(Spells %c-%c, *=List, ESC=exit) Choose a spell. ",
			I2A(0), I2A(num - 1));
	}
	else
	{
                strnfmt(out_val, 78, "(Spells %c-%c, *=List, ESC=exit) Choose a spell. ",
			I2A(0), '0' + num - 27);
	}

	/* Get a spell from the user */
	while (!flag && get_com(out_val, &choice))
	{
		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Show the list */
			if (!redraw)
			{
				byte y = 1, x = 0;
				int ctr = 0;
				char dummy[80];

				strcpy(dummy, "");

				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				Term_save();

				prt ("", y++, x);

                                while (ctr < num && ctr < 19)
				{
					sprintf(dummy, "%c) %s", I2A(ctr), power_desc[ctr]);
					prt(dummy, y + ctr, x);
					ctr++;
				}
				while (ctr < num)
				{
					if (ctr < 26)
					{
						sprintf(dummy, " %c) %s", I2A(ctr), power_desc[ctr]);
					}
					else
					{
						sprintf(dummy, " %c) %s", '0' + ctr - 26, power_desc[ctr]);
					}
                                        prt(dummy, y + ctr - 19, x + 40);
					ctr++;
				}
                                if (ctr < 19)
				{
					prt ("", y + ctr, x);
				}
				else
				{
                                        prt ("", y + 19, x);
				}
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

		if (choice == '\r' && num == 1)
		{
			choice = 'a';
		}

		if (isalpha(choice))
		{
			/* Note verify */
			ask = (isupper(choice));

			/* Lowercase */
			if (ask) choice = tolower(choice);

			/* Extract request */
			i = (islower(choice) ? A2I(choice) : -1);
		}
		else
		{
			ask = FALSE; /* Can't uppercase digits */

			i = choice - '0' + 26;
		}

		/* Totally Illegal */
		if ((i < 0) || (i >= num))
		{
			bell();
			continue;
		}

		/* Save the spell index */
		Power = powers[i];

		/* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Prompt */
			strnfmt(tmp_val, 78, "Use %s? ", power_desc[i]);

			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}

	/* Restore the screen */
	if (redraw) Term_load();

	/* Abort if needed */
	if (!flag) 
	{
		energy_use = 0;
                return num;
	}

	return (Power);
}

/* Used for twisting spells! */
/* Mode 1 is to return monster level change. */
/* Mode 2 is for depth change. */
/* Mode 3 is elites percent. */
s16b fate_monsters(int mode)
{
	int ppower;
	int dpower;

	ppower = (p_ptr->stat_ind[A_INT] + p_ptr->stat_ind[A_WIS] + (p_ptr->skill[26] * 3) + p_ptr->skill[1]) + ((p_ptr->skill[26] / 5) * p_ptr->abilities[(CLASS_DIVINER * 10) + 7]);
	dpower = dun_level * 5;

	if (mode == 1)
	{
		if (dun_level >= (p_ptr->lev / 2))
		{
			if ((randint(ppower) >= randint(dpower)) || dun_level <= p_ptr->abilities[(CLASS_DIVINER * 10) + 7]) return (p_ptr->events[29000]);
			else return (0);
		}
	}
	if (mode == 2)
	{
		if ((randint(ppower) >= randint(dpower)) || dun_level <= p_ptr->abilities[(CLASS_DIVINER * 10) + 7]) return (p_ptr->events[29001]);
		else return (0);
	}
	if (mode == 3)
	{
		if (p_ptr->events[29003] == 0) return (15);
		else
		{
			if ((randint(ppower) >= randint(dpower)) || dun_level <= p_ptr->abilities[(CLASS_DIVINER * 10) + 7]) return (p_ptr->events[29002]);
			else return (15);
		}
	}

	/* Default */
	return (0);
}

/* Used for twisting spells! */
/* Mode 1 is to return items level change. */
/* Mode 2 is for quality change. */
/* Mode 3 is rank modifier. */
s16b fate_items(int mode)
{
	int ppower;
	int dpower;

	ppower = (p_ptr->stat_ind[A_INT] + p_ptr->stat_ind[A_WIS] + (p_ptr->skill[26] * 3) + p_ptr->skill[1]) + ((p_ptr->skill[26] / 5) * p_ptr->abilities[(CLASS_DIVINER * 10) + 7]);
	dpower = dun_level * 5;

	if (mode == 1)
	{
		if ((randint(ppower) >= randint(dpower)) || dun_level <= p_ptr->abilities[(CLASS_DIVINER * 10) + 7]) return (p_ptr->events[29004]);
		else return (0);
	}
	if (mode == 2)
	{
		if ((randint(ppower) >= randint(dpower)) || dun_level <= p_ptr->abilities[(CLASS_DIVINER * 10) + 7]) return (p_ptr->events[29005]);
		else return (0);
	}
	if (mode == 3)
	{
		if ((randint(ppower) >= randint(dpower)) || dun_level <= p_ptr->abilities[(CLASS_DIVINER * 10) + 7]) return (p_ptr->events[29007]);
		else return (0);
	}

	/* Default */
	return (0);
}

/* Special attacks of monsters! */
void use_monster_special_attack(s16b m_idx)
{
        monster_race    *r_ptr;
	int             i;
	int		num = 0;
	int             ac,pt;
	int		powers[36];

	char		powdesc[160];
	char            power_desc[36][160];
	bool            blinked = FALSE, touched = FALSE;

	int Power = -1;
	bool            flag, redraw;
        int             ask;
	char            choice;
	char            out_val[160];

        r_ptr = &r_info[p_ptr->body_monster];
		
	/* List the powers */
	i = 0;
	while (i < 20 && r_ptr->attack[i].type > 0) 
	{
		if (r_ptr->attack[i].type == 1 || (r_ptr->attack[i].type == 3 && !(combatfeat)))
		{
			sprintf(powdesc, "%s", get_monster_attack_name(m_idx, i));
			strcpy(power_desc[num],powdesc);
			powers[num++]=i;
		}
		i++;
	}

        if(!num) {msg_print("No special attacks available.");return;}

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt (accept all spells) */
	if (num <= 26)
	{
		/* Build a prompt (accept all spells) */
        	strnfmt(out_val, 78, "(Attacks %c-%c, *=List, ESC=exit) Use which attack? ",
		I2A(0), I2A(num - 1));
	}
	else
	{
        	strnfmt(out_val, 78, "(Attacks %c-%c, *=List, ESC=exit) Use which attack? ",
		I2A(0), '0' + num - 27);
	}

	/* Get a spell from the user */
	while (!flag && get_com(out_val, &choice))
	{
		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Show the list */
			if (!redraw)
			{
				byte y = 1, x = 0;
				int ctr = 0;
				char dummy[80];

				strcpy(dummy, "");

				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				Term_save();

				prt ("", y++, x);

				while (ctr < num && ctr < 17)
				{
					sprintf(dummy, "%c) %s", I2A(ctr), power_desc[ctr]);
					prt(dummy, y + ctr, x);
					ctr++;
				}
				while (ctr < num)
				{
					if (ctr < 26)
					{
						sprintf(dummy, " %c) %s", I2A(ctr), power_desc[ctr]);
					}
					else
					{
						sprintf(dummy, " %c) %s", '0' + ctr - 26, power_desc[ctr]);
					}
					prt(dummy, y + ctr - 17, x + 40);
					ctr++;
				}
				if (ctr < 17)
				{
					prt ("", y + ctr, x);
				}
				else
				{
					prt ("", y + 17, x);
				}
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

		if (choice == '\r' && num == 1)
		{
			choice = 'a';
		}

		if (isalpha(choice))
		{
			/* Note verify */
			ask = (isupper(choice));

			/* Lowercase */
			if (ask) choice = tolower(choice);

			/* Extract request */
			i = (islower(choice) ? A2I(choice) : -1);
		}
		else
		{
			ask = FALSE; /* Can't uppercase digits */

			i = choice - '0' + 26;
		}

		/* Totally Illegal */
		if ((i < 0) || (i >= num))
		{
			bell();
			continue;
		}

		/* Save the spell index */
		Power = powers[i];

		/* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Prompt */
			strnfmt(tmp_val, 78, "Use %s? ", power_desc[i]);

			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}

	/* Restore the screen */
	if (redraw) Term_load();

	/* Abort if needed */
	if (!flag) 
	{
		energy_use = 0;
                return;
	}

	/* Actually use the power! */
	if (r_ptr->attack[i].type == 1)
	{
		s32b dam;
		int dir;
		int rad;

		p_ptr->events[29016] = damroll(r_ptr->attack[i].ddice, r_ptr->attack[i].dside);
		if (unarmed()) call_lua("monk_damages", "", "l", &dam);
		else
		{
			if (!combatfeat) choose_current_weapon();
			call_lua("weapon_damages", "", "l", &dam);
		}

		if (!(get_rep_dir(&dir))) return;

		dam += multiply_divide(dam, p_ptr->abilities_monster_attacks[i] * 20, 100);

		rad = 0 + (p_ptr->abilities_monster_attacks[i] / 10);

		melee_attack = TRUE;
		no_magic_return = TRUE;
		p_ptr->events[29018] = i+1;
		chain_attack(dir, r_ptr->attack[i].element, dam, rad, 1);
		p_ptr->events[29018] = 0;
		no_magic_return = FALSE;
		melee_attack = FALSE;

		update_and_handle();
		energy_use = 100;
	}

	if (r_ptr->attack[i].type == 3)
	{
		call_lua("monster_ranged_attacks", "(d)", "", i+1);
	}
}

char *get_monster_attack_name(int m_idx, int num)
{
	char attackname[160];
	char aname[80];
	char extra[80];
	monster_race *r_ptr = &r_info[m_idx];
	bool scaled = FALSE;

	if (r_ptr->flags7 & (RF7_SCALED)) scaled = TRUE;

	/* Generate the name. */
	/* If it's just "!", let's make a NICE name! ;) */
	if (r_ptr->attack[num].name[0] != '!') sprintf(aname, "%s", r_ptr->attack[num].name);
	else
	{
		char aact[80];
		sprintf(aact, "%s", r_ptr->attack[num].act);
		aact[0] = toupper(aact[0]);
		sprintf(aname, "%s %s", get_element_name(r_ptr->attack[num].element), aact);
	}

	if (scaled)
	{
		if (r_ptr->attack[num].type == 1) sprintf(extra, "(%s melee attack)", get_element_name(r_ptr->attack[num].element));
		else if (r_ptr->attack[num].type == 3) sprintf(extra, "(%s ranged attack)", get_element_name(r_ptr->attack[num].element));
		else sprintf(extra, "(Special attack)");
	}
	else
	{
		if (r_ptr->attack[num].type == 1) sprintf(extra, "(%s melee attack, base dam: %dd%d)", get_element_name(r_ptr->attack[num].element), r_ptr->attack[num].ddice, r_ptr->attack[num].dside);
		else if (r_ptr->attack[num].type == 3) sprintf(extra, "(%s ranged attack, base dam: %dd%d)", get_element_name(r_ptr->attack[num].element), r_ptr->attack[num].ddice, r_ptr->attack[num].dside);
		else sprintf(extra, "(Special attack)");
	}

	sprintf(attackname, "%s %s", aname, extra);

        return(attackname);
}

char *get_monster_attack_name_short(int m_idx, int num)
{
	char attackname[160];
	char aname[80];
	char extra[80];
	monster_race *r_ptr = &r_info[m_idx];
	bool scaled = FALSE;

	if (r_ptr->flags7 & (RF7_SCALED)) scaled = TRUE;

	/* Generate the name. */
	/* If it's just "!", let's make a NICE name! ;) */
	if (r_ptr->attack[num].name[0] != '!') sprintf(aname, "%s", r_ptr->attack[num].name);
	else
	{
		char aact[80];
		sprintf(aact, "%s", r_ptr->attack[num].act);
		aact[0] = toupper(aact[0]);
		sprintf(aname, "%s %s", get_element_name(r_ptr->attack[num].element), aact);
	}

	if (scaled)
	{
		if (r_ptr->attack[num].type == 1) sprintf(extra, "(%s, Melee)", get_element_name(r_ptr->attack[num].element));
		else if (r_ptr->attack[num].type == 3) sprintf(extra, "(%s, Ranged)", get_element_name(r_ptr->attack[num].element));
		else sprintf(extra, "(Special attack)");
	}
	else
	{
		if (r_ptr->attack[num].type == 1) sprintf(extra, "(%s, %d(+%d/%d lev)d%d(+%d/%d lev), Melee)", get_element_name(r_ptr->attack[num].element), r_ptr->attack[num].ddice, r_ptr->attack[num].ddscale, r_ptr->attack[num].ddscalefactor, r_ptr->attack[num].dside, r_ptr->attack[num].dsscale, r_ptr->attack[num].dsscalefactor);
		else if (r_ptr->attack[num].type == 3) sprintf(extra, "(%s, %d(+%d/%d lev)d%d(+%d/%d lev), Ranged)", get_element_name(r_ptr->attack[num].element), r_ptr->attack[num].ddice, r_ptr->attack[num].ddscale, r_ptr->attack[num].ddscalefactor, r_ptr->attack[num].dside, r_ptr->attack[num].dsscale, r_ptr->attack[num].dsscalefactor);
		else sprintf(extra, "(Special attack)");
	}

	sprintf(attackname, "%s %s", aname, extra);

        return(attackname);
}

char *get_monster_attack_name_damages(monster_type *m_ptr, int num)
{
	char attackname[160];
	char aname[80];
	char extra[80];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	s32b mindam;
	s32b maxdam;
	bool scaled = FALSE;
	int scaledlevel;

	if (r_ptr->flags7 & (RF7_SCALED))
	{
		scaled = TRUE;
		if (p_ptr->max_plv > r_ptr->level) scaledlevel = p_ptr->max_plv;
		else scaledlevel = r_ptr->level;
	}

	/* Generate the name. */
	/* If it's just "!", let's make a NICE name! ;) */
	if (r_ptr->attack[num].name[0] != '!') sprintf(aname, "%s", r_ptr->attack[num].name);
	else
	{
		char aact[80];
		sprintf(aact, "%s", r_ptr->attack[num].act);
		aact[0] = toupper(aact[0]);
		sprintf(aname, "%s %s", get_element_name(r_ptr->attack[num].element), aact);
	}

	/* Calculate damages. */
	if (r_ptr->attack[num].type == 1)
	{
		if (scaled) mindam = damroll(multiply_divide(scaledlevel, r_ptr->attack[num].ddice, 100), 1);
		else mindam = damroll(r_ptr->attack[num].ddice, 1);
		mindam *= (m_ptr->skill_attack + 1);
		mindam += multiply_divide(mindam, ((m_ptr->str - 5) * 5), 100);
		mindam += multiply_divide(mindam, m_ptr->str, 100);

		/* Bosses may get higher damages! */
		if (m_ptr->abilities & (BOSS_DOUBLE_DAMAGES)) mindam *= 2;
                if (m_ptr->abilities & (CURSE_HALVE_DAMAGES)) mindam -= mindam / 2;
                else if (m_ptr->abilities & (CURSE_LOWER_POWER)) mindam -= mindam / 4;

		if (scaled) maxdam = maxroll(multiply_divide(scaledlevel, r_ptr->attack[num].ddice, 100), multiply_divide(scaledlevel, r_ptr->attack[num].dside, 100));
		else maxdam = maxroll(r_ptr->attack[num].ddice, r_ptr->attack[num].dside);
		maxdam *= (m_ptr->skill_attack + 1);
		maxdam += multiply_divide(maxdam, ((m_ptr->str - 5) * 5), 100);
		maxdam += multiply_divide(maxdam, m_ptr->str, 100);

		/* Bosses may get higher damages! */
		if (m_ptr->abilities & (BOSS_DOUBLE_DAMAGES)) maxdam *= 2;
                if (m_ptr->abilities & (CURSE_HALVE_DAMAGES)) maxdam -= maxdam / 2;
                else if (m_ptr->abilities & (CURSE_LOWER_POWER)) maxdam -= maxdam / 4;
	}
	else if (r_ptr->attack[num].type == 3)
	{
		if (scaled) mindam = damroll(multiply_divide(scaledlevel, r_ptr->attack[num].ddice, 100), 1);
		else mindam = damroll(r_ptr->attack[num].ddice, 1);
		mindam *= (m_ptr->skill_ranged + 1);
		mindam += multiply_divide(mindam, ((m_ptr->dex - 5) * 5), 100);
		mindam += multiply_divide(mindam, m_ptr->dex, 100);


		/* Bosses may get higher damages! */
		if (m_ptr->abilities & (BOSS_DOUBLE_DAMAGES)) mindam *= 2;
                if (m_ptr->abilities & (CURSE_HALVE_DAMAGES)) mindam -= mindam / 2;
                else if (m_ptr->abilities & (CURSE_LOWER_POWER)) mindam -= mindam / 4;

		if (scaled) maxdam = maxroll(multiply_divide(scaledlevel, r_ptr->attack[num].ddice, 100), multiply_divide(scaledlevel, r_ptr->attack[num].dside, 100));
		else maxdam = maxroll(r_ptr->attack[num].ddice, r_ptr->attack[num].dside);
		maxdam *= (m_ptr->skill_ranged + 1);
		maxdam += multiply_divide(maxdam, ((m_ptr->dex - 5) * 5), 100);
		maxdam += multiply_divide(maxdam, m_ptr->dex, 100);

		/* Bosses may get higher damages! */
		if (m_ptr->abilities & (BOSS_DOUBLE_DAMAGES)) maxdam *= 2;
                if (m_ptr->abilities & (CURSE_HALVE_DAMAGES)) maxdam -= maxdam / 2;
                else if (m_ptr->abilities & (CURSE_LOWER_POWER)) maxdam -= maxdam / 4;
	}

	if (r_ptr->attack[num].type == 1) sprintf(extra, "(%s, %ld - %ld, Melee)", get_element_name(r_ptr->attack[num].element), mindam, maxdam);
	else if (r_ptr->attack[num].type == 3) sprintf(extra, "(%s, %ld - %ld, Ranged)", get_element_name(r_ptr->attack[num].element), mindam, maxdam);
	else sprintf(extra, "(Special attack)");

	sprintf(attackname, "%s %s", aname, extra);

        return(attackname);
}

/* Pick a song, return it's id. */
int pick_song(int reduction)
{
	int                     Power = -1;
        int                     num = 0, i;

	int             powers[36];
	char            power_desc[36][80];

        bool            flag, redraw;
        int             ask;

        char            choice;

	char            out_val[160];

        music_songs *song_ptr;

        for (i = 1; i <= 15; i++)
        {
                char songstring[80];
		song_ptr = &music_song[i];
                if (song_ptr->created == TRUE)
                {
			s16b finalcost;
			char songtype[80];
			finalcost = song_ptr->cost - reduction;
			if (finalcost < 0) finalcost = 0;

			if (song_ptr->type == 1) sprintf(songtype, "Passive");
			else if (song_ptr->type == 2) sprintf(songtype, "Instant");
			else if (song_ptr->type == 3) sprintf(songtype, "Effect");
			else sprintf(songtype, "Unknown");
                        sprintf(songstring, "%s (%s, Power: %d, Rad: %d, Charisma: %d)", song_ptr->name, songtype, song_ptr->power, song_ptr->radius, finalcost);        
                        strcpy(power_desc[num],songstring);powers[num++]=i;
                }
		else
		{
			sprintf(songstring, "--- Unused ---");        
                        strcpy(power_desc[num],songstring);powers[num++]=i;
		}
        }

        if(!num) {msg_print("No songs to choose.");return 0;}

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt (accept all spells) */
	if (num <= 26)
	{
		/* Build a prompt (accept all spells) */
                strnfmt(out_val, 78, "(Songs %c-%c, *=List, ESC=exit) Choose a song. ",
			I2A(0), I2A(num - 1));
	}
	else
	{
                strnfmt(out_val, 78, "(Songs %c-%c, *=List, ESC=exit) Choose a song. ",
			I2A(0), '0' + num - 27);
	}

	/* Get a spell from the user */
	while (!flag && get_com(out_val, &choice))
	{
		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Show the list */
			if (!redraw)
			{
				byte y = 1, x = 0;
				int ctr = 0;
				char dummy[80];

				strcpy(dummy, "");

				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				Term_save();

				prt ("", y++, x);

                                while (ctr < num && ctr < 19)
				{
					sprintf(dummy, "%c) %s", I2A(ctr), power_desc[ctr]);
					prt(dummy, y + ctr, x);
					ctr++;
				}
				while (ctr < num)
				{
					if (ctr < 26)
					{
						sprintf(dummy, " %c) %s", I2A(ctr), power_desc[ctr]);
					}
					else
					{
						sprintf(dummy, " %c) %s", '0' + ctr - 26, power_desc[ctr]);
					}
                                        prt(dummy, y + ctr - 19, x + 40);
					ctr++;
				}
                                if (ctr < 19)
				{
					prt ("", y + ctr, x);
				}
				else
				{
                                        prt ("", y + 19, x);
				}
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

		if (choice == '\r' && num == 1)
		{
			choice = 'a';
		}

		if (isalpha(choice))
		{
			/* Note verify */
			ask = (isupper(choice));

			/* Lowercase */
			if (ask) choice = tolower(choice);

			/* Extract request */
			i = (islower(choice) ? A2I(choice) : -1);
		}
		else
		{
			ask = FALSE; /* Can't uppercase digits */

			i = choice - '0' + 26;
		}

		/* Totally Illegal */
		if ((i < 0) || (i >= num))
		{
			bell();
			continue;
		}

		/* Save the spell index */
		Power = powers[i];

		/* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Prompt */
			strnfmt(tmp_val, 78, "Use %s? ", power_desc[i]);

			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}

	/* Restore the screen */
	if (redraw) Term_load();

	/* Abort if needed */
	if (!flag) 
	{
		energy_use = 0;
                return num;
	}

	return (Power);
}

void browse_spells()
{
	int i;
	char query;
	char choice;
	bool looping = TRUE;

	 /* Save what's on screen */
        Term_save();

	/* Flush messages */
	msg_print(NULL);

	while (looping)
	{

	/* Begin... */
	Term_erase(0, 1, 255);

	/* Prepare the screen */
        for (i = 0; i < SCREEN_HGT; i++)
        {
        	roff("\n");
        }
        c_put_str(TERM_WHITE, "PORTRALIS SPELLS LIST", 0, 0);
        c_put_str(TERM_WHITE, "-----------------------", 1, 0);
                
        /* First, select the school. */
        c_put_str(TERM_WHITE, "Select a spell school:", 4, 0);
        c_put_str(TERM_L_RED, "[1] Elemental", 7, 0);
        c_put_str(TERM_VIOLET, "[2] Alteration", 9, 0);
        c_put_str(TERM_L_BLUE, "[3] Mysticism", 11, 0);
        c_put_str(TERM_YELLOW, "[4] Conjuration", 13, 0);
        c_put_str(TERM_WHITE, "[5] Divination", 15, 0);

	c_put_str(TERM_WHITE, "Press ESC to return to game.", 22, 0);

        /* End of scanning */
        query = inkey();

        if (query == '1')
        {
        	/* Perpare the screen */
                c_put_str(TERM_WHITE, "", 0, 0);
                for (i = 0; i < SCREEN_HGT; i++)
                {
                	roff("\n");
                }
                c_put_str(TERM_WHITE, "PORTRALIS SPELLS LIST", 0, 0);
        	c_put_str(TERM_WHITE, "-----------------------", 1, 0);

                /* All right, let's prepare the list! :) */
                if (p_ptr->elemental_effects & ELEM_MISSILE) c_put_str(TERM_WHITE, "[0] Missile", 7, 0);
                else c_put_str(TERM_L_DARK, "[0] Missile", 7, 0);
                if (p_ptr->elemental_effects & ELEM_FIRE) c_put_str(TERM_WHITE, "[1] Fire", 8, 0);
                else c_put_str(TERM_L_DARK, "[1] Fire", 8, 0);
                if (p_ptr->elemental_effects & ELEM_COLD) c_put_str(TERM_WHITE, "[2] Cold", 9, 0);
                else c_put_str(TERM_L_DARK, "[2] Cold", 9, 0);
                if (p_ptr->elemental_effects & ELEM_ELEC) c_put_str(TERM_WHITE, "[3] Electricity", 10, 0);
                else c_put_str(TERM_L_DARK, "[3] Electricity", 10, 0);
                if (p_ptr->elemental_effects & ELEM_ACID) c_put_str(TERM_WHITE, "[4] Acid", 11, 0);
                else c_put_str(TERM_L_DARK, "[4] Acid", 11, 0);
                if (p_ptr->elemental_effects & ELEM_POIS) c_put_str(TERM_WHITE, "[5] Poison", 12, 0);
                else c_put_str(TERM_L_DARK, "[5] Poison", 12, 0);
                if (p_ptr->elemental_effects & ELEM_LITE) c_put_str(TERM_WHITE, "[6] Light", 13, 0);
                else c_put_str(TERM_L_DARK, "[6] Light", 13, 0);
                if (p_ptr->elemental_effects & ELEM_DARK) c_put_str(TERM_WHITE, "[7] Darkness", 14, 0);
                else c_put_str(TERM_L_DARK, "[7] Darkness", 14, 0);
                if (p_ptr->elemental_effects & ELEM_PHYSICAL) c_put_str(TERM_WHITE, "[8] Physical", 15, 0);
                else c_put_str(TERM_L_DARK, "[8] Physical", 15, 0);
                if (p_ptr->elemental_effects & ELEM_RADIO) c_put_str(TERM_WHITE, "[9] Radio", 16, 0);
                else c_put_str(TERM_L_DARK, "[9] Radio", 16, 0);
                if (p_ptr->elemental_effects & ELEM_WATER) c_put_str(TERM_WHITE, "[a] Water", 17, 0);
                else c_put_str(TERM_L_DARK, "[a] Water", 17, 0);
                if (p_ptr->elemental_effects & ELEM_CHAOS) c_put_str(TERM_WHITE, "[b] Chaos", 18, 0);
                else c_put_str(TERM_L_DARK, "[b] Chaos", 18, 0);
                if (p_ptr->elemental_effects & ELEM_EARTH) c_put_str(TERM_WHITE, "[c] Earth", 19, 0);
                else c_put_str(TERM_L_DARK, "[c] Earth", 19, 0);
                if (p_ptr->elemental_effects & ELEM_SOUND) c_put_str(TERM_WHITE, "[d] Sound", 7, 20);
                else c_put_str(TERM_L_DARK, "[d] Sound", 7, 20);
                if (p_ptr->elemental_effects & ELEM_WARP) c_put_str(TERM_WHITE, "[e] Warp", 8, 20);
                else c_put_str(TERM_L_DARK, "[e] Warp", 8, 20);
                if (p_ptr->elemental_effects & ELEM_WIND) c_put_str(TERM_WHITE, "[f] Wind", 9, 20);
                else c_put_str(TERM_L_DARK, "[f] Wind", 9, 20);
                if (p_ptr->elemental_effects & ELEM_MANA) c_put_str(TERM_WHITE, "[g] Mana", 10, 20);
                else c_put_str(TERM_L_DARK, "[g] Mana", 10, 20);
		if (p_ptr->elemlord != 0)
		{
			char elemname[80];
			sprintf(elemname, "[z] %s", get_element_name(p_ptr->elemlord));
			c_put_str(TERM_WHITE, elemname, 19, 20);
                }

		c_put_str(TERM_WHITE, "Press any keys to return to main spells menu.", 22, 0);

		choice = inkey();
	}
	/* Alterations! */
        else if (query == '2')
        {
                /* Perpare the screen */
                c_put_str(TERM_WHITE, "", 0, 0);
                for (i = 0; i < SCREEN_HGT; i++)
                {
                        roff("\n");
                }
                c_put_str(TERM_WHITE, "PORTRALIS SPELLS LIST", 0, 0);
        	c_put_str(TERM_WHITE, "-----------------------", 1, 0);

                /* All right, let's prepare the list! :) */
                if (p_ptr->alteration_effects & ALTER_REDUCE_HIT) c_put_str(TERM_WHITE, "[0] Reduce Hit Rate", 7, 0);
                else c_put_str(TERM_L_DARK, "[0] Reduce Hit Rate", 7, 0);
                if (p_ptr->alteration_effects & ALTER_REDUCE_DEF) c_put_str(TERM_WHITE, "[1] Reduce Defense", 8, 0);
                else c_put_str(TERM_L_DARK, "[1] Reduce Defense", 8, 0);
                if (p_ptr->alteration_effects & ALTER_REDUCE_SPEED) c_put_str(TERM_WHITE, "[2] Reduce Speed", 9, 0);
                else c_put_str(TERM_L_DARK, "[2] Reduce Speed", 9, 0);
                if (p_ptr->alteration_effects & ALTER_REDUCE_LEVEL) c_put_str(TERM_WHITE, "[3] Reduce Level", 10, 0);
                else c_put_str(TERM_L_DARK, "[3] Reduce Level", 10, 0);
                if (p_ptr->alteration_effects & ALTER_LIFE_BLAST) c_put_str(TERM_WHITE, "[4] Life Blast", 11, 0);
                else c_put_str(TERM_L_DARK, "[4] Life Blast", 11, 0);
                if (p_ptr->alteration_effects & ALTER_LOCK) c_put_str(TERM_WHITE, "[5] Lock", 12, 0);
                else c_put_str(TERM_L_DARK, "[5] Lock", 12, 0);
                if (p_ptr->alteration_effects & ALTER_HALVE_POWER) c_put_str(TERM_WHITE, "[6] Halve Power", 13, 0);
                else c_put_str(TERM_L_DARK, "[6] Halve Power", 13, 0);
                if (p_ptr->alteration_effects & ALTER_HALVE_MAGIC) c_put_str(TERM_WHITE, "[7] Halve Magic", 14, 0);
                else c_put_str(TERM_L_DARK, "[7] Halve Magic", 14, 0);
                if (p_ptr->alteration_effects & ALTER_STONE_TO_MUD) c_put_str(TERM_WHITE, "[8] Stone to Mud", 15, 0);
                else c_put_str(TERM_L_DARK, "[8] Stone to Mud", 15, 0);
                if (p_ptr->alteration_effects & ALTER_DEMORALIZE) c_put_str(TERM_WHITE, "[9] Demoralize", 16, 0);
                else c_put_str(TERM_L_DARK, "[9] Demoralize", 16, 0);
                if (p_ptr->alteration_effects & ALTER_RETROGRADE) c_put_str(TERM_WHITE, "[a] Retrograde", 17, 0);
                else c_put_str(TERM_L_DARK, "[a] Retrograde", 17, 0);
                if (p_ptr->alteration_effects & ALTER_EVOLVE) c_put_str(TERM_WHITE, "[b] Evolve", 18, 0);
                else c_put_str(TERM_L_DARK, "[b] Evolve", 18, 0);
                if (p_ptr->alteration_effects & ALTER_UNEVOLVE) c_put_str(TERM_WHITE, "[c] Un-Evolve", 19, 0);
                else c_put_str(TERM_L_DARK, "[c] Un-Evolve", 19, 0);
                if (p_ptr->alteration_effects & ALTER_HASTE) c_put_str(TERM_WHITE, "[d] Haste", 7, 20);
                else c_put_str(TERM_L_DARK, "[d] Haste", 7, 20);
                if (p_ptr->alteration_effects & ALTER_RAISE_STR) c_put_str(TERM_WHITE, "[e] Raise Strength", 8, 20);
                else c_put_str(TERM_L_DARK, "[e] Raise Strength", 8, 20);
                if (p_ptr->alteration_effects & ALTER_RAISE_INT) c_put_str(TERM_WHITE, "[f] Raise Intelligence", 9, 20);
                else c_put_str(TERM_L_DARK, "[f] Raise Intelligence", 9, 20);
                if (p_ptr->alteration_effects & ALTER_RAISE_WIS) c_put_str(TERM_WHITE, "[g] Raise Wisdom", 10, 20);
                else c_put_str(TERM_L_DARK, "[g] Raise Wisdom", 10, 20);
                if (p_ptr->alteration_effects & ALTER_RAISE_DEX) c_put_str(TERM_WHITE, "[h] Raise Dexterity", 11, 20);
                else c_put_str(TERM_L_DARK, "[h] Raise Dexterity", 11, 20);
                if (p_ptr->alteration_effects & ALTER_RAISE_CON) c_put_str(TERM_WHITE, "[i] Raise Constitution", 12, 20);
                else c_put_str(TERM_L_DARK, "[i] Raise Constitution", 12, 20);
                if (p_ptr->alteration_effects & ALTER_RAISE_CHR) c_put_str(TERM_WHITE, "[j] Raise Charisma", 13, 20);
                else c_put_str(TERM_L_DARK, "[j] Raise Charisma", 13, 20);
                if (p_ptr->alteration_effects & ALTER_POSITION) c_put_str(TERM_WHITE, "[k] Alter Position", 14, 20);
                else c_put_str(TERM_L_DARK, "[k] Alter Position", 14, 20);
                if (p_ptr->alteration_effects & ALTER_HASTE_OTHER) c_put_str(TERM_WHITE, "[l] Haste Other", 15, 20);
                else c_put_str(TERM_L_DARK, "[l] Haste Other", 15, 20);
                if (p_ptr->alteration_effects & ALTER_PHYS_RESIST) c_put_str(TERM_WHITE, "[m] Physical Resistance", 16, 20);
                else c_put_str(TERM_L_DARK, "[m] Physical Resistance", 16, 20);
                if (p_ptr->alteration_effects & ALTER_MAGIC_RESIST) c_put_str(TERM_WHITE, "[n] Magic Resistance", 17, 20);
                else c_put_str(TERM_L_DARK, "[n] Magic Resistance", 17, 20);
                if (p_ptr->alteration_effects & ALTER_STONESKIN) c_put_str(TERM_WHITE, "[o] Stoneskin", 18, 20);
                else c_put_str(TERM_L_DARK, "[o] Stoneskin", 18, 20);
                if (p_ptr->alteration_effects & ALTER_PARALYZE) c_put_str(TERM_WHITE, "[p] Paralyze", 19, 20);
                else c_put_str(TERM_L_DARK, "[p] Paralyze", 19, 20);

		c_put_str(TERM_WHITE, "Press any keys to return to main spells menu.", 22, 0);

		choice = inkey();
	}
        /* Mysticism */
        else if (query == '3')
        {
        	/* Perpare the screen */
                c_put_str(TERM_WHITE, "", 0, 0);
                for (i = 0; i < SCREEN_HGT; i++)
                {
                        roff("\n");
                }
                c_put_str(TERM_WHITE, "PORTRALIS SPELLS LIST", 0, 0);
        	c_put_str(TERM_WHITE, "-----------------------", 1, 0);

                /* All right, let's prepare the list! :) */
                if (p_ptr->healing_effects & MYST_HEAL) c_put_str(TERM_WHITE, "[0] Heal", 7, 0);
                else c_put_str(TERM_L_DARK, "[0] Heal", 7, 0);
                if (p_ptr->healing_effects & MYST_RESTORE_STATS) c_put_str(TERM_WHITE, "[1] Restore Stats", 8, 0);
                else c_put_str(TERM_L_DARK, "[1] Restore Stats", 8, 0);
                if (p_ptr->healing_effects & MYST_RESTORE_STATUS) c_put_str(TERM_WHITE, "[2] Restore Status", 9, 0);
                else c_put_str(TERM_L_DARK, "[2] Restore Status", 9, 0);
                if (p_ptr->healing_effects & MYST_HARM) c_put_str(TERM_WHITE, "[3] Harm", 10, 0);
                else c_put_str(TERM_L_DARK, "[3] Harm", 10, 0);                        
                if (p_ptr->healing_effects & MYST_DRAIN_LIFE) c_put_str(TERM_WHITE, "[4] Drain Life", 11, 0);
                else c_put_str(TERM_L_DARK, "[4] Drain Life", 11, 0);
                if (p_ptr->healing_effects & MYST_REVIVE_MONSTER) c_put_str(TERM_WHITE, "[5] Revive Monster", 12, 0);
                else c_put_str(TERM_L_DARK, "[5] Revive Monster", 12, 0);
                if (p_ptr->healing_effects & MYST_RESTORE_MANA) c_put_str(TERM_WHITE, "[6] Restore Mana", 13, 0);
                else c_put_str(TERM_L_DARK, "[6] Restore Mana", 13, 0);
		if (p_ptr->healing_effects & MYST_SMITE_UNDEADS) c_put_str(TERM_WHITE, "[7] Smite Undeads", 14, 0);
                else c_put_str(TERM_L_DARK, "[7] Smite Undeads", 14, 0);
		if (p_ptr->healing_effects & MYST_SMITE_DEMONS) c_put_str(TERM_WHITE, "[8] Smite Demons", 15, 0);
                else c_put_str(TERM_L_DARK, "[8] Smite Demons", 15, 0);
		if (p_ptr->healing_effects & MYST_SMITE_EVIL) c_put_str(TERM_WHITE, "[9] Smite Evil", 16, 0);
                else c_put_str(TERM_L_DARK, "[9] Smite Evil", 16, 0);
		if (p_ptr->healing_effects & MYST_SMITE_GOOD) c_put_str(TERM_WHITE, "[a] Smite Good", 17, 0);
                else c_put_str(TERM_L_DARK, "[a] Smite Good", 17, 0);
		if (p_ptr->healing_effects & MYST_WAR_BLESSING) c_put_str(TERM_WHITE, "[b] War Blessing", 18, 0);
                else c_put_str(TERM_L_DARK, "[b] War Blessing", 18, 0);
		if (p_ptr->healing_effects & MYST_BLESSING) c_put_str(TERM_WHITE, "[c] Blessing", 19, 0);
                else c_put_str(TERM_L_DARK, "[c] Blessing", 19, 0);

		c_put_str(TERM_WHITE, "Press any keys to return to main spells menu.", 22, 0);

		choice = inkey();
	}
        /* Conjurations! */
        else if (query == '4')
        {
                /* Perpare the screen */
                c_put_str(TERM_WHITE, "", 0, 0);
                for (i = 0; i < SCREEN_HGT; i++)
                {
                        roff("\n");
                }
                c_put_str(TERM_WHITE, "PORTRALIS SPELLS LIST", 0, 0);
        	c_put_str(TERM_WHITE, "-----------------------", 1, 0);

                /* All right, let's prepare the list! :) */
                if (p_ptr->conjuration_effects & CONJ_SUMMON_KIND) c_put_str(TERM_WHITE, "[0] Summon Kind", 7, 0);
                else c_put_str(TERM_L_DARK, "[0] Summon Kind", 7, 0);
                if (p_ptr->conjuration_effects & CONJ_SUMMON_SPECIFIC) c_put_str(TERM_WHITE, "[1] Summon Specific", 8, 0);
                else c_put_str(TERM_L_DARK, "[1] Summon Specific", 8, 0);
                if (p_ptr->conjuration_effects & CONJ_FIRE_FIELD) c_put_str(TERM_WHITE, "[2] Fire Fields", 9, 0);
                else c_put_str(TERM_L_DARK, "[2] Fire Fields", 9, 0);
                if (p_ptr->conjuration_effects & CONJ_COLD_FIELD) c_put_str(TERM_WHITE, "[3] Cold Fields", 10, 0);
                else c_put_str(TERM_L_DARK, "[3] Cold Fields", 10, 0);
                if (p_ptr->conjuration_effects & CONJ_ELEC_FIELD) c_put_str(TERM_WHITE, "[4] Electric Fields", 11, 0);
                else c_put_str(TERM_L_DARK, "[4] Electric Fields", 11, 0);
                if (p_ptr->conjuration_effects & CONJ_WEBS) c_put_str(TERM_WHITE, "[5] Webs", 12, 0);
                else c_put_str(TERM_L_DARK, "[5] Webs", 12, 0);
                if (p_ptr->conjuration_effects & CONJ_GROW_TREES) c_put_str(TERM_WHITE, "[6] Grow Trees", 13, 0);
                else c_put_str(TERM_L_DARK, "[6] Grow Trees", 13, 0);
                if (p_ptr->conjuration_effects & CONJ_THORNS) c_put_str(TERM_WHITE, "[7] Thorned Vines", 14, 0);
                else c_put_str(TERM_L_DARK, "[7] Thorned Vines", 14, 0);
                if (p_ptr->conjuration_effects & CONJ_STORMS) c_put_str(TERM_WHITE, "[8] Storms", 15, 0);
                else c_put_str(TERM_L_DARK, "[8] Storms", 15, 0);
                if (p_ptr->conjuration_effects & CONJ_ITEM) c_put_str(TERM_WHITE, "[9] Conjure Item", 16, 0);
                else c_put_str(TERM_L_DARK, "[9] Conjure Item", 16, 0);
                if (p_ptr->conjuration_effects & CONJ_MAGIC_ITEM) c_put_str(TERM_WHITE, "[a] Conjure Magic Item", 17, 0);
                else c_put_str(TERM_L_DARK, "[a] Conjure Magic Item", 17, 0);
                if (p_ptr->conjuration_effects & CONJ_SPECIAL_ITEM) c_put_str(TERM_WHITE, "[b] Conjure Special Item", 18, 0);
                else c_put_str(TERM_L_DARK, "[b] Conjure Special Item", 18, 0);

		c_put_str(TERM_WHITE, "Press any keys to return to main spells menu.", 22, 0);

		choice = inkey();
	}
        /* Divinations! */
        else if (query == '5')
        {
        	/* Perpare the screen */
                c_put_str(TERM_WHITE, "", 0, 0);
                for (i = 0; i < SCREEN_HGT; i++)
                {
                        roff("\n");
                }
                c_put_str(TERM_WHITE, "PORTRALIS SPELLS LIST", 0, 0);
        	c_put_str(TERM_WHITE, "-----------------------", 1, 0);

                /* All right, let's prepare the list! :) */
                if (p_ptr->divination_effects & DIVI_DETECT_MONSTERS) c_put_str(TERM_WHITE, "[0] Detect Monsters", 7, 0);
                else c_put_str(TERM_L_DARK, "[0] Detect Monsters", 7, 0);
                if (p_ptr->divination_effects & DIVI_DETECT_OBJECTS) c_put_str(TERM_WHITE, "[1] Detect Objects", 8, 0);
                else c_put_str(TERM_L_DARK, "[1] Detect Objects", 8, 0);
                if (p_ptr->divination_effects & DIVI_DETECT_DOORS) c_put_str(TERM_WHITE, "[2] Detect Doors", 9, 0);
                else c_put_str(TERM_L_DARK, "[2] Detect Doors", 9, 0);
                if (p_ptr->divination_effects & DIVI_DETECT_STAIRS) c_put_str(TERM_WHITE, "[3] Detect Stairs", 10, 0);
                else c_put_str(TERM_L_DARK, "[3] Detect Stairs", 10, 0);
                if (p_ptr->divination_effects & DIVI_DETECT_CHESTS) c_put_str(TERM_WHITE, "[4] Detect Chests", 11, 0);
                else c_put_str(TERM_L_DARK, "[4] Detect Chests", 11, 0);
                if (p_ptr->divination_effects & DIVI_TELEPATHY) c_put_str(TERM_WHITE, "[5] Telepathy", 12, 0);
                else c_put_str(TERM_L_DARK, "[5] Telepathy", 12, 0);
                if (p_ptr->divination_effects & DIVI_IDENTIFY) c_put_str(TERM_WHITE, "[6] Identify", 13, 0);
                else c_put_str(TERM_L_DARK, "[6] Identify", 13, 0);
                if (p_ptr->divination_effects & DIVI_SCAN_MONSTER) c_put_str(TERM_WHITE, "[7] Scan Monster", 14, 0);
                else c_put_str(TERM_L_DARK, "[7] Scan Monster", 14, 0);
                if (p_ptr->divination_effects & DIVI_REVEAL) c_put_str(TERM_WHITE, "[8] Reveal", 15, 0);
                else c_put_str(TERM_L_DARK, "[8] Reveal", 15, 0);
		if (p_ptr->divination_effects & DIVI_DIVINATION) c_put_str(TERM_WHITE, "[9] Divination", 16, 0);
                else c_put_str(TERM_L_DARK, "[9] Divination", 16, 0);
		if (p_ptr->divination_effects & DIVI_RESTORE_FATE) c_put_str(TERM_WHITE, "[a] Restore Fate", 17, 0);
                else c_put_str(TERM_L_DARK, "[a] Restore Fate", 17, 0);
		if (p_ptr->divination_effects & DIVI_FATE_MONSTERS) c_put_str(TERM_WHITE, "[b] Twist Fate: Monsters", 18, 0);
                else c_put_str(TERM_L_DARK, "[b] Twist Fate: Monsters", 18, 0);
		if (p_ptr->divination_effects & DIVI_FATE_ITEMS) c_put_str(TERM_WHITE, "[c] Twist Fate: Items", 19, 0);
                else c_put_str(TERM_L_DARK, "[c] Twist Fate: Items", 19, 0);
		if (p_ptr->divination_effects & DIVI_FATE_DUNGEONS) c_put_str(TERM_WHITE, "[d] Twist Fate: Random Dungeons", 7, 28);
                else c_put_str(TERM_L_DARK, "[d] Twist Fate: Random Dungeons", 7, 28);

		c_put_str(TERM_WHITE, "Press any keys to return to main spells menu.", 22, 0);

		choice = inkey();
        }

	if (query == ESCAPE) looping = FALSE;

	}

	Term_load();
	update_and_handle();
}