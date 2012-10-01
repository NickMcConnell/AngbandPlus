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
                char spellstring[80];
                mspell_ptr = &monster_magic[i];
                if (mspell_ptr->type > 0)
                {
                        sprintf(spellstring, "%s   Cost: %d", mspell_ptr->name, mspell_ptr->cost);        
                        /* List the powers */
                        strcpy(power_desc[num],spellstring);powers[num++]=i;
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

        /* Before we cast... do we have enough mana? */
	if (p_ptr->csp < mspell_ptr->cost) 
	{
		msg_print("Not enough mana to use this monster magic.");
		return 0;
	}

	mspell_ptr = &monster_magic[Power];
	/* Actually cast the monster magic! */
	switch (mspell_ptr->type)
	{
		/* Bolt */
		case 1:
		{
			dam = mspell_ptr->power * spellstat;
			dam += ((dam * (p_ptr->abilities[(CLASS_MONSTER_MAGE * 10)] * 20)) / 100);
			if(!get_aim_dir(&dir)) return;
			fire_bolt(mspell_ptr->special1, dir, dam);
			break;
		}
		/* Ball */
		case 2:
		{
			dam = mspell_ptr->power * spellstat;
			dam += ((dam * (p_ptr->abilities[(CLASS_MONSTER_MAGE * 10)] * 20)) / 100);
			rad = mspell_ptr->special2;
			if(!get_aim_dir(&dir)) return;
			fire_ball(mspell_ptr->special1, dir, dam, rad);
			break;
		}
		/* Heal */
		case 3:
		{
			dam = mspell_ptr->power * spellstat;
			dam += ((dam * (p_ptr->abilities[(CLASS_MONSTER_MAGE * 10)] * 20)) / 100);
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

        bool           learning = TRUE;

        if (p_ptr->pclass == CLASS_APPRENTICE && p_ptr->prace != RACE_MONSTER)
        {
                msg_print("This class doesn't have any abilities!");
                return;
        }

        /* Save what's on screen */
        Term_save();

	/* Flush messages */
	msg_print(NULL);

        /* Begin... */
	Term_erase(0, 1, 255);

        while (learning == TRUE)
        {

        /* Prepare the screen */
        for (i = 0; i < SCREEN_HGT; i++)
        {
                roff("\n");
        }
        c_put_str(TERM_WHITE, "ABILITIES                                                                 ", 0, 0);
        c_put_str(TERM_WHITE, "-----------", 1, 0);

	/* As of Portralis, this part has changed! :) */
	if (p_ptr->prace == RACE_MONSTER && p_ptr->pclass == 0)
	{
		sprintf(abils[0], "Improved Attack");
                sprintf(abils[1], "Improved Defense");
                sprintf(abils[2], "Improved Magic");
                sprintf(abils[3], "-");
                sprintf(abils[4], "-");
                sprintf(abils[5], "-");
                sprintf(abils[6], "-");
                sprintf(abils[7], "-");
                sprintf(abils[8], "-");
                sprintf(abils[9], "-");
	}
	else
	{
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
	}

        
        if (p_ptr->prace != RACE_MONSTER)
        {
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
        }
        else
        {
                for (x = 0; x < 3; x++)
                {
                        int abilnum = (p_ptr->pclass * 10) + x;
                        int abilitynumber = x + 1;                
                        char abil[80];

                        if (abilitynumber > 9) abilitynumber = 0;
                        sprintf (abil, "%d.  %s [%d]", abilitynumber, abils[x], p_ptr->abilities[abilnum]);
                        if (p_ptr->abilities[abilnum] <= 0) c_put_str(TERM_L_DARK, abil, (3 + x), 0);
                        else c_put_str(TERM_WHITE, abil, (3 + x), 0);
                }
        }        

        sprintf(tmpstr, "%ld  ", p_ptr->ability_points);
        c_put_str(TERM_WHITE, "Remaining AP: ", 15, 0);
        c_put_str(TERM_L_GREEN, tmpstr, 15, 14);
        c_put_str(TERM_WHITE, "Choose an ability to increase, ESC to exit.", 17, 0);

        choice = inkey();
        if (choice == '1')
        {
                int abilnum = (p_ptr->pclass * 10);
                if (p_ptr->ability_points > 0)
                {
                        if (p_ptr->abilities[abilnum] <= 0 && p_ptr->num_abilities >= 20) msg_print("You may not learn any new abilities.");
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
                                p_ptr->abilities[abilnum] += 1;
                                p_ptr->ability_points -= 1;
                        }
                }
        }
        if (choice == '2')
        {
                int abilnum = (p_ptr->pclass * 10) + 1;
                if (p_ptr->ability_points > 0)
                {
                        if (p_ptr->abilities[abilnum] <= 0 && p_ptr->num_abilities >= 20) msg_print("You may not learn any new abilities.");
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
                                p_ptr->abilities[abilnum] += 1;
                                p_ptr->ability_points -= 1;
                        }
                }
        }
        if (choice == '3')
        {
                int abilnum = (p_ptr->pclass * 10) + 2;
                if (p_ptr->ability_points > 0)
                {
                        if (p_ptr->abilities[abilnum] <= 0 && p_ptr->num_abilities >= 20) msg_print("You may not learn any new abilities.");
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
                                p_ptr->abilities[abilnum] += 1;
                                p_ptr->ability_points -= 1;
                        }
                }
        }
        if (choice == '4')
        {
                int abilnum = (p_ptr->pclass * 10) + 3;
                if (p_ptr->prace == RACE_MONSTER)
                {
                        Term_load();
                        return;
                }
                if (p_ptr->ability_points > 0)
                {
                        if (p_ptr->abilities[abilnum] <= 0 && p_ptr->num_abilities >= 20) msg_print("You may not learn any new abilities.");
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
                                p_ptr->abilities[abilnum] += 1;
                                p_ptr->ability_points -= 1;
                        }
                }
        }
        if (choice == '5')
        {                
                int abilnum = (p_ptr->pclass * 10) + 4;
                if (p_ptr->prace == RACE_MONSTER)
                {
                        Term_load();
                        return;
                }
                if (p_ptr->ability_points > 0)
                {
                        if (p_ptr->abilities[abilnum] <= 0 && p_ptr->num_abilities >= 20) msg_print("You may not learn any new abilities.");
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
                                p_ptr->abilities[abilnum] += 1;
                                p_ptr->ability_points -= 1;
                        }
                }
        }
        if (choice == '6')
        {                
                int abilnum = (p_ptr->pclass * 10) + 5;
                if (p_ptr->prace == RACE_MONSTER)
                {
                        Term_load();
                        return;
                }
                if (p_ptr->ability_points > 0)
                {
                        if (p_ptr->abilities[abilnum] <= 0 && p_ptr->num_abilities >= 20) msg_print("You may not learn any new abilities.");
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
                                p_ptr->abilities[abilnum] += 1;
                                p_ptr->ability_points -= 1;
                        }
                }
        }
        if (choice == '7')
        {
                if (p_ptr->prace == RACE_MONSTER)
                {
                        Term_load();
                        return;
                }
                if (p_ptr->class_level[p_ptr->pclass] >= 6)
                {
                        int abilnum = (p_ptr->pclass * 10) + 6;
                        if (p_ptr->ability_points > 0)
                        {
                                if (p_ptr->abilities[abilnum] <= 0 && p_ptr->num_abilities >= 20) msg_print("You may not learn any new abilities.");
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
                                        p_ptr->abilities[abilnum] += 1;
                                        p_ptr->ability_points -= 1;
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
                if (p_ptr->prace == RACE_MONSTER)
                {
                        Term_load();
                        return;
                }
                if (p_ptr->class_level[p_ptr->pclass] >= 6)
                {
                        int abilnum = (p_ptr->pclass * 10) + 7;
                        if (p_ptr->ability_points > 0)
                        {
                                if (p_ptr->abilities[abilnum] <= 0 && p_ptr->num_abilities >= 20) msg_print("You may not learn any new abilities.");
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
                                        p_ptr->abilities[abilnum] += 1;
                                        p_ptr->ability_points -= 1;
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
                if (p_ptr->prace == RACE_MONSTER)
                {
                        Term_load();
                        return;
                }
                if (p_ptr->class_level[p_ptr->pclass] >= 6)
                {
                        int abilnum = (p_ptr->pclass * 10) + 8;
                        if (p_ptr->ability_points > 0)
                        {
                                if (p_ptr->abilities[abilnum] <= 0 && p_ptr->num_abilities >= 20) msg_print("You may not learn any new abilities.");
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
                                        p_ptr->abilities[abilnum] += 1;
                                        p_ptr->ability_points -= 1;
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
                if (p_ptr->prace == RACE_MONSTER)
                {
                        Term_load();
                        return;
                }
                if (p_ptr->class_level[p_ptr->pclass] >= 10)
                {
                        int abilnum = (p_ptr->pclass * 10) + 9;
                        if (p_ptr->ability_points > 0)
                        {
                                if (p_ptr->abilities[abilnum] <= 0 && p_ptr->num_abilities >= 20) msg_print("You may not learn any new abilities.");
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
                                        p_ptr->abilities[abilnum] += 1;
                                        p_ptr->ability_points -= 1;
                                }
                        }
                }
                else {
                        msg_print("You must first reach class level 10!");
                        inkey();
                }
        }
        if (choice == ESCAPE)
        {
                learning = FALSE;
        }

        }
        Term_load();
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

/* Since ability(x) is a very useful function, here's a similar one for Bows! */
s16b examine_bow()
{
        object_type *o_ptr;

        o_ptr = &inventory[INVEN_BOW];

        return (o_ptr->sval - 10);
}

/* Since ability(x) is a very useful function, here's a similar one for daggers! */
bool dagger_check()
{
        object_type *o_ptr;

        o_ptr = &inventory[INVEN_WIELD];
        if (o_ptr->tval != TV_DAGGER) 
	{
		o_ptr = &inventory[INVEN_WIELD+1];
		if (o_ptr->tval == TV_DAGGER) return (TRUE);
	}
	else return (TRUE);

        return (FALSE);
}

bool axe_check()
{
        object_type *o_ptr;

        o_ptr = &inventory[INVEN_WIELD];
        if (o_ptr->tval != TV_AXE) 
	{
		o_ptr = &inventory[INVEN_WIELD+1];
		if (o_ptr->tval == TV_AXE) return (TRUE);
	}
	else return (TRUE);

        return (FALSE);
}

bool is_weapon(object_type *o_ptr)
{
        if (o_ptr->tval == TV_SWORD || o_ptr->tval == TV_HAFTED || o_ptr->tval == TV_POLEARM ||
        o_ptr->tval == TV_ROD || o_ptr->tval == TV_MSTAFF || o_ptr->tval == TV_SWORD_DEVASTATION || o_ptr->tval == TV_DAGGER || o_ptr->tval == TV_AXE ||
        o_ptr->tval == TV_HELL_STAFF || o_ptr->tval == TV_VALKYRIE_SPEAR || o_ptr->tval == TV_ZELAR_WEAPON) return (TRUE);

        return (FALSE);
}
bool is_ammo(object_type *o_ptr)
{
        if (o_ptr->tval == TV_ARROW || o_ptr->tval == TV_BOLT || o_ptr->tval == TV_SHOT) return (TRUE);

        return (FALSE);
}

int add_item_ability(object_type *o_ptr)
{
	int                     Power = -1;
        int                     num = 0, dir = 0 , i;
        int     magictype;

	int             powers[36];
	char            power_desc[36][80];

	bool            flag, redraw;
        int             ask, plev = p_ptr->lev;

        char            choice, ch;

	char            out_val[160];
        int k,count;

        /* List the powers */
        {strcpy(power_desc[num],"Random Resistance 3000 golds");powers[num++]=0;}
        if (o_ptr->tval == TV_BOOTS) {strcpy(power_desc[num],"Increase speed 200000 golds");powers[num++]=1;}
        if (is_weapon(o_ptr)) {strcpy(power_desc[num],"Extra attacks 200000 golds");powers[num++]=2;}
        {strcpy(power_desc[num],"Invisibility 150000 golds");powers[num++]=7;}
        {strcpy(power_desc[num],"Telepathy 20000 golds");powers[num++]=8;}
        {strcpy(power_desc[num],"Permanent Light 5000 golds");powers[num++]=10;}
        if (is_weapon(o_ptr) || is_ammo(o_ptr)) {strcpy(power_desc[num],"Add Random Brand(or change damages of existing one)");powers[num++]=11;}
	if (o_ptr->tval == TV_SOFT_ARMOR || o_ptr->tval == TV_HARD_ARMOR || o_ptr->tval == TV_DRAG_ARMOR) {strcpy(power_desc[num],"Life 40000 golds");powers[num++]=19;}
        {strcpy(power_desc[num],"Indestructible 500 golds");powers[num++]=20;}
        {strcpy(power_desc[num],"Regeneration 50000 golds");powers[num++]=21;}
        if (o_ptr->tval == TV_SHIELD) {strcpy(power_desc[num],"Reflecting 25000 golds");powers[num++]=22;}
        {strcpy(power_desc[num],"Eternal 45000 golds");powers[num++]=23;}
        if (o_ptr->tval == TV_HELM || o_ptr->tval == TV_CROWN) {strcpy(power_desc[num],"Safety 50000 golds");powers[num++]=24;}
        if(!num) {msg_print("No powers to use.");return 0;}

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt (accept all spells) */
	if (num <= 26)
	{
		/* Build a prompt (accept all spells) */
                strnfmt(out_val, 78, "(Powers %c-%c, *=List, ESC=exit) Add which ability? ",
			I2A(0), I2A(num - 1));
	}
	else
	{
                strnfmt(out_val, 78, "(Powers %c-%c, *=List, ESC=exit) Add which ability? ",
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

        switch(Power)
        {
                case 0:
                        if (p_ptr->au >= 3000)
                        {
				int totalresists;
				int maxresists = o_ptr->pval * 30;
				totalresists = (o_ptr->fireres + o_ptr->coldres +
				o_ptr->elecres + o_ptr->acidres + o_ptr->poisres +
				o_ptr->lightres + o_ptr->darkres + o_ptr->warpres +
				o_ptr->waterres + o_ptr->windres + o_ptr->earthres +
				o_ptr->soundres + o_ptr->radiores + o_ptr->chaosres +
				o_ptr->physres + o_ptr->manares);
				if (totalresists >= maxresists)
				{
					msg_print("This item cannot gain more resistances with current pval.");
				}
				else
				{ 
                                	msg_print("Your item gained a resistance!");
                                	p_ptr->au -= 3000;
                                	random_resistance(o_ptr);
                                	update_and_handle();
				}
                        }
                        else msg_print("You don't have enough money...");
                        break;
                case 1:
                        if (p_ptr->au >= 200000)
                        {
                                msg_print("Your item gained a new ability!");
                                p_ptr->au -= 200000;
                                o_ptr->art_flags1 |= TR1_SPEED;
                                update_and_handle();
                        }
                        else msg_print("You don't have enough money...");
                        break;
                case 2:
                        if (p_ptr->au >= 200000)
                        {
                                msg_print("Your item gained a new ability!");
                                p_ptr->au -= 200000;
                                o_ptr->art_flags1 |= TR1_BLOWS;
                                update_and_handle();
                        }
                        else msg_print("You don't have enough money...");
                        break;
                case 7:
                        if (p_ptr->au >= 150000)
                        {
                                msg_print("Your item gained a new ability!");
                                p_ptr->au -= 22000;
                                o_ptr->art_flags2 |= TR2_INVIS;
                                update_and_handle();
                        }
                        else msg_print("You don't have enough money...");
                        break;
                case 8:
                        if (p_ptr->au >= 20000)
                        {
                                msg_print("Your item gained a new ability!");
                                p_ptr->au -= 20000;
                                o_ptr->art_flags3 |= TR3_TELEPATHY;
                                update_and_handle();
                        }
                        else msg_print("You don't have enough money...");
                        break;
                case 10:
                        if (p_ptr->au >= 5000)
                        {
                                msg_print("Your item gained a new ability!");
                                p_ptr->au -= 5000;
                                o_ptr->art_flags3 |= TR3_LITE;
                                update_and_handle();
                        }
                        else msg_print("You don't have enough money...");
                        break;
                case 11:
		{
			int brandamount, brandmax;
			s32b goldcost;
			char bquan[80];
			char goldcostmsg[80];
			char ch;
			brandmax = o_ptr->pval * 100;
			sprintf(bquan, "How much power? (Max: %d) ", brandmax);
			brandamount = get_quantity(bquan, brandmax);
			goldcost = brandamount * 50;
			goldcost += ((goldcost * brandamount) / 100);
			sprintf(goldcostmsg, "This will cost %ld golds. [y/n]", goldcost);
			if (!get_com(goldcostmsg, &ch)) return;
			if (ch == 'y' || ch == 'Y')
			{
				if (p_ptr->au >= goldcost)
				{
                        		msg_print("Your item gained a new ability!");
					if (!(o_ptr->brandtype)) o_ptr->brandtype = randint(12);
					o_ptr->branddam = brandamount;
                        		p_ptr->au -= goldcost;
                        		update_and_handle();
				}
                        	else msg_print("You don't have enough money...");
			}
                        break;
		}
                case 19:
                        if (p_ptr->au >= 40000)
                        {
                                msg_print("Your item gained a new ability!");
                                p_ptr->au -= 40000;
                                o_ptr->art_flags2 |= TR2_LIFE;
                                update_and_handle();
                        }
                        else msg_print("You don't have enough money...");
                        break;
                case 20:
                        if (p_ptr->au >= 500)
                        {
                                msg_print("Your item gained a new ability!");
                                p_ptr->au -= 500;
                                o_ptr->art_flags4 |= TR4_INDESTRUCTIBLE;
                                update_and_handle();
                        }
                        else msg_print("You don't have enough money...");
                        break;
                case 21:
                        if (p_ptr->au >= 50000)
                        {
                                msg_print("Your item gained a new ability!");
                                p_ptr->au -= 50000;
                                o_ptr->art_flags3 |= TR3_REGEN;
                                update_and_handle();
                        }
                        else msg_print("You don't have enough money...");
                        break;
                case 22:
                        if (p_ptr->au >= 25000)
                        {
                                msg_print("Your item gained a new ability!");
                                p_ptr->au -= 25000;
                                o_ptr->art_flags2 |= TR2_REFLECT;
                                update_and_handle();
                        }
                        else msg_print("You don't have enough money...");
                        break;
                case 23:
                        if (p_ptr->au >= 45000)
                        {
                                msg_print("Your item gained a new ability!");
                                p_ptr->au -= 45000;
                                o_ptr->art_flags4 |= TR4_ETERNAL;
                                update_and_handle();
                        }
                        else msg_print("You don't have enough money...");
                        break;
                case 24:
                        if (p_ptr->au >= 50000)
                        {
                                msg_print("Your item gained a new ability!");
                                p_ptr->au -= 50000;
                                o_ptr->art_flags4 |= TR4_SAFETY;
                                update_and_handle();
                        }
                        else msg_print("You don't have enough money...");
                        break;

        }
        energy_use = 100;
        return num;
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
        object_type             *o_ptr;
        monster_race            *r_ptr;
        cptr q, s;

	/* This determines casting power */
	spellstat = (p_ptr->stat_ind[A_WIS] - 5);

	/* No lower than 0. */
	if (spellstat < 0) spellstat = 0;

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
                if (r_ptr->spell[i].type > 0)
                {
			if (!(r_ptr->spell[i].type == 5 && (r_ptr->spell[Power].type == 4 || r_ptr->spell[Power].type == 5 || r_ptr->spell[Power].type == 7)))
			{
				int chance;
                        	sprintf(spellstring, "%s   Difficulty: %d", r_ptr->spell[i].name, r_ptr->spell[i].cost);        
                        	/* List the powers */
                        	strcpy(power_desc[num],spellstring);powers[num++]=i;
			}
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
		int castpower = 5 + (p_ptr->abilities[(CLASS_PRIEST * 10) + 7] / 10);
		if (p_ptr->stat_ind[A_WIS] >= r_ptr->spell[Power].cost) castsuccess = TRUE;
		else
		{
			int caststrength = (p_ptr->stat_ind[A_WIS] - 5) * castpower;
			caststrength += ((caststrength * (p_ptr->abilities[(CLASS_PRIEST * 10) + 7] * 10)) / 100);
                        if (r_ptr->spell[Power].cost > (caststrength * (3 + (p_ptr->abilities[(CLASS_PRIEST * 10) + 7] / 20)))) risky = TRUE;
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
			dam = r_ptr->spell[Power].power * spellstat;
			dam += ((dam * (p_ptr->abilities[(CLASS_SOUL_GUARDIAN * 10)] * 20)) / 100);
			if(!get_aim_dir(&dir)) return;
			fire_bolt(r_ptr->spell[Power].special1, dir, dam);
			break;
		}
		/* Ball */
		case 2:
		{
			dam = r_ptr->spell[Power].power * spellstat;
			dam += ((dam * (p_ptr->abilities[(CLASS_SOUL_GUARDIAN * 10)] * 20)) / 100);
			rad = r_ptr->spell[Power].special2;
			if(!get_aim_dir(&dir)) return;
			fire_ball(r_ptr->spell[Power].special1, dir, dam, rad);
			break;
		}
		/* Heal */
		case 3:
		{
			dam = r_ptr->spell[Power].power * spellstat;
			dam += ((dam * (p_ptr->abilities[(CLASS_SOUL_GUARDIAN * 10)] * 20)) / 100);
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
			if (r_ptr->spell[Power].special1 == 1)
			{
				p_ptr->str_boost = dam;
                                (void)set_str_boost(20); 
			}
			if (r_ptr->spell[Power].special1 == 2)
			{
				p_ptr->dex_boost = dam;
                                (void)set_dex_boost(20); 
			}
			if (r_ptr->spell[Power].special1 == 3)
			{
				p_ptr->int_boost = dam;
                                (void)set_int_boost(20); 
			}
			if ((r_ptr->spell[Power].special1 == 6) || (r_ptr->spell[Power].special1 == 8)) 
			{
				p_ptr->str_boost = dam;
				p_ptr->dex_boost = dam;
				p_ptr->int_boost = dam;
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

static bool is_elemental(int power)
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
            power == GF_TOXIC) return (TRUE);
	
	return (FALSE);
}

static bool is_alteration(int power)
{
	if (power == GF_REDUCE_DEF ||
            power == GF_REDUCE_HIT ||
            power == GF_REDUCE_SPEED ||
            power == GF_EVOLVE ||
            power == GF_WEAKEN ||
            power == GF_OLD_SPEED ||
            power == GF_PARALYZE ||
            power == GF_UNEVOLVE) return (TRUE);
	
	return (FALSE);
}

static bool is_healing(int power)
{
	if (power == GF_OLD_HEAL) return (TRUE);
	
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
        bool            empower = FALSE;
        int             ask;

        char            choice, ch, summchar, ch2;

	char            out_val[160];
	int spellstat = 0;
        magic_spells *spell_ptr;

	/* This determines casting power */
	if (wisdom) spellstat = (p_ptr->stat_ind[A_WIS] - 5);
	else spellstat = (p_ptr->stat_ind[A_INT] - 5);

	/* No lower than 0 */
	if (spellstat < 0) spellstat = 0;

	if (!wisdom) if (!get_com("[C]ast a spell, [M]ake a spell, [D]elete a spell", &ch)) return;

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
                        sprintf(spellstring, "%s   Cost: %d", spell_ptr->name, spell_ptr->finalcost);        
                        /* List the powers */
                        strcpy(power_desc[num],spellstring);powers[num++]=i;
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
                return num;
	}

        spell_ptr = &magic_spell[Power];
        finalcost = spell_ptr->finalcost;
        if (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 5] >= 1)
        {
                get_com("Empower the spell? [y/n]", &ch2);
                if (ch2 == 'y' || ch2 == 'Y')
                {
                        empower = TRUE;
                        finalcost = finalcost * 3;
                }
        }
	
	/* NewAngband 1.8.0: You can use Wisdom to cast spells! */
	/* They cost no mana, but have a chance of failure. */
	/* And if you get too ambitious... something bad may happen! ;) */
	if (wisdom)
	{
		bool castsuccess = FALSE;
		bool risky = FALSE;
		int castpower = 5 + (p_ptr->abilities[(CLASS_PRIEST * 10) + 7] / 10);
		if (p_ptr->stat_ind[A_WIS] >= finalcost) castsuccess = TRUE;
		else
		{
			int caststrength = (p_ptr->stat_ind[A_WIS] - 5) * castpower;
			caststrength += ((caststrength * (p_ptr->abilities[(CLASS_PRIEST * 10) + 7] * 10)) / 100);
                        if (finalcost > (caststrength * (3 + (p_ptr->abilities[(CLASS_PRIEST * 10) + 7] / 20)))) risky = TRUE;
			if (randint(caststrength) >= randint(finalcost)) castsuccess = TRUE;
			else
			{
				msg_print("You failed to cast the spell...");
				castsuccess = FALSE;
				if (risky)
				{
					msg_print("Your spell has become out of control!");
					msg_print("The spell backfires, and your head is hurt by a vicious trauma!");
					take_hit(finalcost, "Failed Casting");
					(void)set_confused(5);
                                        dec_stat(A_WIS, 5, STAT_DEC_NORMAL);                              
				}
			}
		}
		if (!castsuccess)
		{
			update_and_handle();
			energy_use = 100; 
			return;
		}
	}

        for (i = 0; i < 5; i++)
        {
                if (p_ptr->csp < finalcost && !wisdom)
                {
                        msg_print("You don't have enough mana to cast this spell.");
                        return;
                }
                spellpower = spell_ptr->power[i];
                if (empower == TRUE) spellpower += ((spellpower * (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 5] * 5)) / 100);
                if (spell_ptr->effect[i] != 0)
                {
                        /* Spell kind 1 = Attack spell */
                        if (spell_ptr->effect[i] == 1)
                        {
                                /* For physical spells... */
                                nevermiss = TRUE;
                                if (spell_ptr->shape[i] == 1)
                                {
					s32b dam;
					dam = (spellpower * spellstat) * (p_ptr->to_s);
					if (is_elemental(spell_ptr->type[i])) dam += ((dam * (p_ptr->skill[22] * 20)) / 100);
					if (is_alteration(spell_ptr->type[i])) dam += ((dam * (p_ptr->skill[23] * 5)) / 100);
					if (is_healing(spell_ptr->type[i])) dam += ((dam * (p_ptr->skill[24] * 20)) / 100);
                                        if (dir == 0) get_aim_dir(&dir);
                                        fire_bolt(spell_ptr->type[i], dir, dam);
                                }
                                if (spell_ptr->shape[i] == 2)
                                {
					s32b dam;
					dam = (spellpower * spellstat) * (p_ptr->to_s);
					if (is_elemental(spell_ptr->type[i])) dam += ((dam * (p_ptr->skill[22] * 20)) / 100);
					if (is_alteration(spell_ptr->type[i])) dam += ((dam * (p_ptr->skill[23] * 5)) / 100);
					if (is_healing(spell_ptr->type[i])) dam += ((dam * (p_ptr->skill[24] * 20)) / 100);
                                        if (dir == 0) get_aim_dir(&dir);
                                        fire_ball(spell_ptr->type[i], dir, dam, spell_ptr->radius[i]);
                                }
                                if (spell_ptr->shape[i] == 3)
                                {
					s32b dam;
					dam = (spellpower * spellstat) * (p_ptr->to_s);
					if (is_elemental(spell_ptr->type[i])) dam += ((dam * (p_ptr->skill[22] * 20)) / 100);
					if (is_alteration(spell_ptr->type[i])) dam += ((dam * (p_ptr->skill[23] * 5)) / 100);
					if (is_healing(spell_ptr->type[i])) dam += ((dam * (p_ptr->skill[24] * 20)) / 100);
                                        attack_aura(spell_ptr->type[i], dam, spell_ptr->radius[i]);
                                }
                                if (spell_ptr->shape[i] == 4)
                                {
					s32b dam;
					dam = (spellpower * spellstat) * (p_ptr->to_s);
					if (is_elemental(spell_ptr->type[i])) dam += ((dam * (p_ptr->skill[22] * 20)) / 100);
					if (is_alteration(spell_ptr->type[i])) dam += ((dam * (p_ptr->skill[23] * 5)) / 100);
					if (is_healing(spell_ptr->type[i])) dam += ((dam * (p_ptr->skill[24] * 20)) / 100);
                                        if (dir == 0) get_rep_dir(&dir);
                                        chain_attack(dir, spell_ptr->type[i], dam, spell_ptr->radius[i], 20);
                                }
                                if (spell_ptr->shape[i] == 5)
                                {
                                        s32b dam = 0;
                                        object_type *o_ptr = &inventory[INVEN_WIELD];
                                        if (dir == 0) get_rep_dir(&dir);
                                        if (o_ptr)
                                        {
						call_lua("weapon_damages", "", "l", &dam);
                                        }
                                        dam += p_ptr->to_d;
                                        if (dam < 0) dam = 0;
					if (is_elemental(spell_ptr->type[i])) dam += ((dam * (p_ptr->skill[22] * 20)) / 100);
					if (is_alteration(spell_ptr->type[i])) dam += ((dam * (p_ptr->skill[23] * 5)) / 100);
					if (is_healing(spell_ptr->type[i])) dam += ((dam * (p_ptr->skill[24] * 20)) / 100);
                                        chain_attack(dir, spell_ptr->type[i], (dam * spellpower), spell_ptr->radius[i], 1);
                                }
                                nevermiss = FALSE;
                        }
                        /* Spell kind 2 = Fixed Attack spell */
                        /* Does fixed damages, not increased by your level. */
                        if (spell_ptr->effect[i] == 2)
                        {
                                if (spell_ptr->shape[i] == 1)
                                {
					if (is_elemental(spell_ptr->type[i])) spellpower += ((spellpower * (p_ptr->skill[22] * 20)) / 100);
					if (is_alteration(spell_ptr->type[i])) spellpower += ((spellpower * (p_ptr->skill[23] * 5)) / 100);
                                        if (dir == 0) get_aim_dir(&dir);
                                        fire_bolt(spell_ptr->type[i], dir, spellpower);
                                }
                                if (spell_ptr->shape[i] == 2)
                                {
					if (is_elemental(spell_ptr->type[i])) spellpower += ((spellpower * (p_ptr->skill[22] * 20)) / 100);
					if (is_alteration(spell_ptr->type[i])) spellpower += ((spellpower * (p_ptr->skill[23] * 5)) / 100);
                                        if (dir == 0) get_aim_dir(&dir);
                                        fire_ball(spell_ptr->type[i], dir, spellpower, spell_ptr->radius[i]);
                                }
                                if (spell_ptr->shape[i] == 3)
                                {
					if (is_elemental(spell_ptr->type[i])) spellpower += ((spellpower * (p_ptr->skill[22] * 20)) / 100);
					if (is_alteration(spell_ptr->type[i])) spellpower += ((spellpower * (p_ptr->skill[23] * 5)) / 100);
                                        attack_aura(spell_ptr->type[i], spellpower, spell_ptr->radius[i]);
                                }
                                if (spell_ptr->shape[i] == 4)
                                {
					if (is_elemental(spell_ptr->type[i])) spellpower += ((spellpower * (p_ptr->skill[22] * 20)) / 100);
					if (is_alteration(spell_ptr->type[i])) spellpower += ((spellpower * (p_ptr->skill[23] * 5)) / 100);
                                        if (dir == 0) get_rep_dir(&dir);
                                        chain_attack(dir, spell_ptr->type[i], spellpower, spell_ptr->radius[i], 20);
                                }
                        }
                        /* Spell kind 3 = Haste */
                        /* Raise speed by 10. Power is the duration. */
                        if (spell_ptr->effect[i] == 3)
                        {
				if (is_alteration(spell_ptr->type[i])) spellpower += ((spellpower * (p_ptr->skill[23] * 5)) / 100);
                                (void)set_fast(spellpower);
                        }
                        /* Spell kind 4 = Stat Boost */
                        /* Raise a stat by a certain amount. */
                        /* The Shape is the duration of the spell. */
                        if (spell_ptr->effect[i] == 4)
                        {
				if (is_alteration(spell_ptr->type[i])) spellpower += ((spellpower * (p_ptr->skill[23] * 5)) / 100);
                                switch(spell_ptr->type[i])
                                {
                                        case 1:
                                                p_ptr->str_boost = spellpower;
                                                (void)set_str_boost(spell_ptr->shape[i]);
                                                break;
                                        case 2:
                                                p_ptr->int_boost = spellpower;
                                                (void)set_int_boost(spell_ptr->shape[i]);
                                                break;
                                        case 3:
                                                p_ptr->wis_boost = spellpower;
                                                (void)set_wis_boost(spell_ptr->shape[i]);
                                                break;
                                        case 4:
                                                p_ptr->dex_boost = spellpower;
                                                (void)set_dex_boost(spell_ptr->shape[i]);
                                                break;
                                        case 5:
                                                p_ptr->con_boost = spellpower;
                                                (void)set_con_boost(spell_ptr->shape[i]);
                                                break;
                                        case 6:
                                                p_ptr->chr_boost = spellpower;
                                                (void)set_chr_boost(spell_ptr->shape[i]);
                                                break;
                                }
                        }
                        /* Spell kind 5 = Blessing */
                        /* Doubles to_hit and AC. Power is the duration. */
                        if (spell_ptr->effect[i] == 5)
                        {
				if (is_alteration(spell_ptr->type[i])) spellpower += ((spellpower * (p_ptr->skill[23] * 5)) / 100);
                                (void)set_blessed(spellpower);
                        }
                        /* Spell kind 6 = Healing */
                        /* Heals some hp. The power is the number of */
                        /* of hp restored per levels. */
                        if (spell_ptr->effect[i] == 6)
                        {
                                s32b heal;
                                heal = (spellstat * spellpower) * (p_ptr->to_s);
				heal += ((heal * (p_ptr->skill[24] * 10)) / 100);
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
				if (wisdom) spellpower += ((spellpower * (p_ptr->skill[24] * 10)) / 100);
                                p_ptr->csp += spellpower;
                                if (p_ptr->csp > p_ptr->msp) p_ptr->csp = p_ptr->msp;
                                update_and_handle();
                                msg_print("You recover your mana!");                                
                        }
                        /* Spell kind 13 = Summon Kind */
                        if (spell_ptr->effect[i] == 13)
                        {
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
                                                dam = (spellstat * spellpower) * (p_ptr->to_s);
						dam += ((dam * (p_ptr->skill[25] * 20)) / 100);                                                                
                                                place_field(spell_ptr->type[i], spell_ptr->radius[i], px, py, dam);
                                                update_and_handle();
                                                break;
                                        }
                                        case 2:
                                        {
                                                s32b dam;
                                                int ii, ij;
                                                dam = (spellstat * spellpower) * (p_ptr->to_s);
						dam += ((dam * (p_ptr->skill[25] * 20)) / 100);                                                                
                                                if (!tgt_pt(&ii,&ij)) return num;
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
								place_field(spell_ptr->type[i], spell_ptr->radius[i], ii, ij, dam);
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
                                                detect_traps();
                                                break;
                                        }

                                }
                        }
                        /* Spell kind 17 = Telepathy */
                        if (spell_ptr->effect[i] == 17)
                        {
				spellpower += ((spellpower * (p_ptr->skill[26] * 20)) / 100);
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
                                                if (!tgt_pt(&ii,&ij)) return num;
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
				if (is_alteration(spell_ptr->type[i])) spellpower += ((spellpower * (p_ptr->skill[23] * 5)) / 100);
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
                        /* Spell kind 23 = Satisfy Hunger */
                        if (spell_ptr->effect[i] == 23)
                        {
                        	if (p_ptr->prace == RACE_VAMPIRE || p_ptr->prace == RACE_SKELETON)
                        	msg_print("This spell is useless on you!");
                        	else set_food(PY_FOOD_MAX - 1);        
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
        update_and_handle();
        energy_use = 100;
        return;
        }
        /* Make a spell */
        else if (ch == 'd' || ch == 'D')
        {
                delete_spell();
        }
        else
        {
                if (dun_level == 0) spell_making();
                else msg_print("You can only create spells in the town!");
        }
}

/* Allow creation of a spell! :) */
void spell_making()
{
        int i;
        int effectschoose = 0;
        int effects_costs[5];
        int finalcost = 0;
        int effbase = 0;
        int effcost = 0;
        int efftype = 0;
        int effnum = 1;
        int rad = 0;
        int power = 0;
        int spelltype = 1;
        int manacost = 0;
        int whichslot = 1;
        int effkind = 0;
        int amountslots = 12;
        int redpercent = 0;

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
        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
        c_put_str(TERM_WHITE, "---------------------------", 1, 0);

        if (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 3] >= 1)
        {
                amountslots += (1 + (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 3] / 5));
        }

        sprintf(quantitystring, "Select a slot to put the spell (1 - %d): ", amountslots);

        whichslot = get_quantity(quantitystring, amountslots);
        spell_ptr = &magic_spell[whichslot];

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
                c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                c_put_str(TERM_WHITE, "---------------------------", 1, 0);
                
                /* First, select the school. */
                c_put_str(TERM_WHITE, "Select a spell school:", 4, 0);
                c_put_str(TERM_L_RED, "[1] Elemental", 7, 0);
                c_put_str(TERM_VIOLET, "[2] Alteration", 9, 0);
                c_put_str(TERM_L_BLUE, "[3] Healing", 11, 0);
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
                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                        c_put_str(TERM_WHITE, "---------------------------", 1, 0);
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
                                                effcost = 1;
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
                                                effcost = 1;
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
                                                effcost = 1;
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
                                                effcost = 1;
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
                                                effcost = 1;
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
                                                effcost = 1;
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
                                                effcost = 1;
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
                                                effcost = 1;
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
                                                effcost = 1;
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
                                                effcost = 1;
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
                                                effcost = 1;
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
                                                effcost = 1;
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
                                                effcost = 1;
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
                                                effcost = 1;
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
                                                effbase = 40;
                                                effcost = 1;
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
                                                effcost = 1;
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
                                                effcost = 2;
                                                effkind = 1;
                                                sprintf(choseneffect, "Mana");
                                                chosen = TRUE;
                                        }
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
                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                        c_put_str(TERM_WHITE, "---------------------------", 1, 0);
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
                                else if (spelltype == 5) sprintf(tmp, "Melee Attack ");
                                c_put_str(TERM_L_GREEN, tmp, 10, 6);
                                c_put_str(TERM_WHITE, "Mana Cost:", 12, 0);
                                if (spelltype != 5) manacost = (effbase + ((effcost * power) / 2));
                                else manacost = (effbase + ((effcost * power)));
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
                                if (spelltype == 5)
                                {
                                        manacost *= 40;
                                        if (rad > 0) manacost *= rad;
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
				if (p_ptr->skill[22] >= 100) manacost = (manacost / 2);
				else if (p_ptr->skill[22] >= 50) manacost = (manacost - (manacost / 3));
				else if (p_ptr->skill[22] >= 25) manacost = (manacost - (manacost / 4));

                                sprintf(tmp, "%d          ", manacost);
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
                                        if (spelltype > 5) spelltype = 1;
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
                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                        c_put_str(TERM_WHITE, "---------------------------", 1, 0);
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
                        if (p_ptr->alteration_effects & ALTER_WAR_BLESSING) c_put_str(TERM_WHITE, "[8] War Blessing", 15, 0);
                        else c_put_str(TERM_L_DARK, "[8] War Blessing", 15, 0);
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
                        if (p_ptr->alteration_effects & ALTER_BLESSING) c_put_str(TERM_WHITE, "[k] Blessing", 14, 20);
                        else c_put_str(TERM_L_DARK, "[k] Blessing", 14, 20);
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
                                        if (!(p_ptr->alteration_effects & ALTER_WAR_BLESSING)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_WAR_BLESSING;
                                                effbase = 30;
                                                effcost = 0;
                                                effkind = 2;
                                                interfacetype = 2;
                                                sprintf(choseneffect, "War Blessing");
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
                                                effcost = 50;
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
                                        if (!(p_ptr->alteration_effects & ALTER_BLESSING)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 0;
                                                effbase = 0;
                                                effcost = 5;
                                                effkind = 5;
                                                interfacetype = 3;
                                                sprintf(choseneffect, "Blessing");
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
                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                        c_put_str(TERM_WHITE, "---------------------------", 1, 0);
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
				if (p_ptr->skill[23] >= 100) manacost = (manacost / 2);
				else if (p_ptr->skill[23] >= 50) manacost = (manacost - (manacost / 3));
				else if (p_ptr->skill[23] >= 25) manacost = (manacost - (manacost / 4));

                                sprintf(tmp, "%d          ", manacost);
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
                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                        c_put_str(TERM_WHITE, "---------------------------", 1, 0);
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
                                if (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 7] >= 1)
                                {
                                        redpercent = p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 7];
                                        if (redpercent > 75) redpercent = 75;
                                        manacost -= ((manacost * redpercent) / 100);
                                        manacost -= (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 7] * 3);
                                        if (manacost < 0) manacost = 0;
                                }
				/* Mastery of Alteration spells! */
				if (p_ptr->skill[23] >= 100) manacost = (manacost / 2);
				else if (p_ptr->skill[23] >= 50) manacost = (manacost - (manacost / 3));
				else if (p_ptr->skill[23] >= 25) manacost = (manacost - (manacost / 4));

                                sprintf(tmp, "%d          ", manacost);
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
                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                        c_put_str(TERM_WHITE, "---------------------------", 1, 0);
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
				if (p_ptr->skill[23] >= 100) manacost = (manacost / 2);
				else if (p_ptr->skill[23] >= 50) manacost = (manacost - (manacost / 3));
				else if (p_ptr->skill[23] >= 25) manacost = (manacost - (manacost / 4));

                                sprintf(tmp, "%d          ", manacost);
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
                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                        c_put_str(TERM_WHITE, "---------------------------", 1, 0);
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
				if (p_ptr->skill[23] >= 100) manacost = (manacost / 2);
				else if (p_ptr->skill[23] >= 50) manacost = (manacost - (manacost / 3));
				else if (p_ptr->skill[23] >= 25) manacost = (manacost - (manacost / 4));

                                sprintf(tmp, "%d          ", manacost);
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
                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                        c_put_str(TERM_WHITE, "---------------------------", 1, 0);
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
                                manacost = (effbase + (((effcost + (power / 10)) * power))) * spelltype;
                                if (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 7] >= 1)
                                {
                                        redpercent = p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 7];
                                        if (redpercent > 75) redpercent = 75;
                                        manacost -= ((manacost * redpercent) / 100);
                                        manacost -= (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 7] * 3);
                                        if (manacost < 0) manacost = 0;
                                }
				/* Mastery of Alteration spells! */
				if (p_ptr->skill[23] >= 100) manacost = (manacost / 2);
				else if (p_ptr->skill[23] >= 50) manacost = (manacost - (manacost / 3));
				else if (p_ptr->skill[23] >= 25) manacost = (manacost - (manacost / 4));

                                sprintf(tmp, "%d          ", manacost);
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
                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                        c_put_str(TERM_WHITE, "---------------------------", 1, 0);
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
				if (p_ptr->skill[23] >= 100) manacost = (manacost / 2);
				else if (p_ptr->skill[23] >= 50) manacost = (manacost - (manacost / 3));
				else if (p_ptr->skill[23] >= 25) manacost = (manacost - (manacost / 4));

                                sprintf(tmp, "%d          ", manacost);
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
                /* Healings! */
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
                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                        c_put_str(TERM_WHITE, "---------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Select an effect:", 4, 0);

                        /* All right, let's prepare the list! :) */
                        if (p_ptr->healing_effects & HEAL_HEAL) c_put_str(TERM_WHITE, "[0] Heal", 7, 0);
                        else c_put_str(TERM_L_DARK, "[0] Heal", 7, 0);
                        if (p_ptr->healing_effects & HEAL_RESTORE_STATS) c_put_str(TERM_WHITE, "[1] Restore Stats", 8, 0);
                        else c_put_str(TERM_L_DARK, "[1] Restore Stats", 8, 0);
                        if (p_ptr->healing_effects & HEAL_RESTORE_STATUS) c_put_str(TERM_WHITE, "[2] Restore Status", 9, 0);
                        else c_put_str(TERM_L_DARK, "[2] Restore Status", 9, 0);
                        if (p_ptr->healing_effects & HEAL_CURE_BLEEDING) c_put_str(TERM_WHITE, "[3] Cure Bleeding", 10, 0);
                        else c_put_str(TERM_L_DARK, "[3] Cure Bleeding", 10, 0);                        
                        if (p_ptr->healing_effects & HEAL_HEAL_OTHERS) c_put_str(TERM_WHITE, "[4] Heal Others", 11, 0);
                        else c_put_str(TERM_L_DARK, "[4] Heal Others", 11, 0);
                        if (p_ptr->healing_effects & HEAL_REVIVE_MONSTER) c_put_str(TERM_WHITE, "[5] Revive Monster", 12, 0);
                        else c_put_str(TERM_L_DARK, "[5] Revive Monster", 12, 0);
                        if (p_ptr->healing_effects & HEAL_RESTORE_MANA) c_put_str(TERM_WHITE, "[6] Restore Mana", 13, 0);
                        else c_put_str(TERM_L_DARK, "[6] Restore Mana", 13, 0);
                        if (p_ptr->healing_effects & HEAL_SATISFY_HUNGER) c_put_str(TERM_WHITE, "[7] Satisfy Hunger", 14, 0);
                        else c_put_str(TERM_L_DARK, "[7] Satisfy Hunger", 14, 0);

                        while (chosen == FALSE)
                        {
                                choice = inkey();
                                if (choice == '0')
                                {
                                        if (!(p_ptr->healing_effects & HEAL_HEAL)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 0;
                                                effbase = 0;
                                                effcost = 1;
                                                effkind = 6;
                                                interfacetype = 2;
                                                sprintf(choseneffect, "Heal");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '1')
                                {
                                        if (!(p_ptr->healing_effects & HEAL_RESTORE_STATS)) msg_print("You haven't learned this effect yet.");
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
                                        if (!(p_ptr->healing_effects & HEAL_RESTORE_STATUS)) msg_print("You haven't learned this effect yet.");
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
                                        if (!(p_ptr->healing_effects & HEAL_CURE_BLEEDING)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 0;
                                                effbase = 40;
                                                effcost = 0;
                                                effkind = 9;
                                                interfacetype = 3;
                                                sprintf(choseneffect, "Cure Bleeding");
                                                chosen = TRUE;
                                        }
                                }                                
                                else if (choice == '4')
                                {
                                        if (!(p_ptr->healing_effects & HEAL_HEAL_OTHERS)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = GF_OLD_HEAL;
                                                effbase = 0;
                                                effcost = 1;
                                                effkind = 1;
                                                interfacetype = 1;
                                                sprintf(choseneffect, "Heal Others");
                                                chosen = TRUE;
                                        }
                                }
                                else if (choice == '5')
                                {
                                        if (!(p_ptr->healing_effects & HEAL_REVIVE_MONSTER)) msg_print("You haven't learned this effect yet.");
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
                                        if (!(p_ptr->healing_effects & HEAL_RESTORE_MANA)) msg_print("You haven't learned this effect yet.");
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
                                        if (!(p_ptr->healing_effects & HEAL_SATISFY_HUNGER)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 0;
                                                effbase = 80;
                                                effcost = 0;
                                                effkind = 23;
                                                interfacetype = 3;
                                                sprintf(choseneffect, "Satisfy Hunger");
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
                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                        c_put_str(TERM_WHITE, "---------------------------", 1, 0);
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
                                if (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 4] >= 1)
                                {
                                        redpercent = p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 4];
                                        if (redpercent > 75) redpercent = 75;
                                        manacost -= ((manacost * redpercent) / 100);
                                        manacost -= (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 4] * 3);
                                        if (manacost < 0) manacost = 0;
                                }
				/* Mastery of Healing spells! */
				if (p_ptr->skill[24] >= 100) manacost = (manacost / 2);
				else if (p_ptr->skill[24] >= 50) manacost = (manacost - (manacost / 3));
				else if (p_ptr->skill[24] >= 25) manacost = (manacost - (manacost / 4));

                                sprintf(tmp, "%d          ", manacost);
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
                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                        c_put_str(TERM_WHITE, "---------------------------", 1, 0);
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
                                if (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 4] >= 1)
                                {
                                        redpercent = p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 4];
                                        if (redpercent > 75) redpercent = 75;
                                        manacost -= ((manacost * redpercent) / 100);
                                        manacost -= (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 4] * 3);
                                        if (manacost < 0) manacost = 0;
                                }
				/* Mastery of Healing spells! */
				if (p_ptr->skill[24] >= 100) manacost = (manacost / 2);
				else if (p_ptr->skill[24] >= 50) manacost = (manacost - (manacost / 3));
				else if (p_ptr->skill[24] >= 25) manacost = (manacost - (manacost / 4));

                                sprintf(tmp, "%d          ", manacost);
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
                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                        c_put_str(TERM_WHITE, "---------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Selected effect:", 4, 0);
                        c_put_str(TERM_WHITE, choseneffect, 4, 17);

                        spelltype = 0;
                        while (readytocreate == FALSE)
                        {
                                c_put_str(TERM_WHITE, "Mana Cost:", 12, 0);
                                manacost = (effbase + ((effcost * power)));
                                if (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 4] >= 1)
                                {
                                        redpercent = p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 4];
                                        if (redpercent > 75) redpercent = 75;
                                        manacost -= ((manacost * redpercent) / 100);
                                        manacost -= (p_ptr->abilities[(CLASS_JUSTICE_WARRIOR * 10) + 4] * 3);
                                        if (manacost < 0) manacost = 0;
                                }
				/* Mastery of Healing spells! */
				if (p_ptr->skill[24] >= 100) manacost = (manacost / 2);
				else if (p_ptr->skill[24] >= 50) manacost = (manacost - (manacost / 3));
				else if (p_ptr->skill[24] >= 25) manacost = (manacost - (manacost / 4));

                                sprintf(tmp, "%d          ", manacost);
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
                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                        c_put_str(TERM_WHITE, "---------------------------", 1, 0);
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
                                                effcost = 2;
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
                                                effcost = 2;
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
                                                effcost = 2;
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
                                                effcost = 2;
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
                                                effcost = 2;
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
                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                        c_put_str(TERM_WHITE, "---------------------------", 1, 0);
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
                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
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
                                if ((efftype / 2) <= 0) manacost = manacost * spelltype;
                                else manacost = (manacost * spelltype) * (efftype / 2);
				/* Dragons of all kind, and greater demons are costly. */
				if (summch == 'd' || summch == 'D' || summch == 'U')
				{
					manacost *= 5;
				}
                                if (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 8] >= 1)
                                {
                                        redpercent = p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 8];
                                        if (redpercent > 75) redpercent = 75;
                                        manacost -= ((manacost * redpercent) / 100);
                                        manacost -= (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 8] * 3);
                                        if (manacost < 0) manacost = 0;
                                }
				/* Mastery of Conjuration spells! */
				if (p_ptr->skill[25] >= 100) manacost = (manacost / 2);
				else if (p_ptr->skill[25] >= 50) manacost = (manacost - (manacost / 3));
				else if (p_ptr->skill[25] >= 25) manacost = (manacost - (manacost / 4));

                                sprintf(tmp, "%d          ", manacost);
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
                                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
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
                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                        c_put_str(TERM_WHITE, "---------------------------", 1, 0);
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

                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
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
				if ((efftype / 2) <= 0) manacost = manacost * spelltype;
                                else manacost = (manacost * spelltype) * (efftype / 2);
                                if (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 8] >= 1)
                                {
                                        redpercent = p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 8];
                                        if (redpercent > 75) redpercent = 75;
                                        manacost -= ((manacost * redpercent) / 100);
                                        manacost -= (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 8] * 3);
                                        if (manacost < 0) manacost = 0;
                                }
				/* Mastery of Conjuration spells! */
				if (p_ptr->skill[25] >= 100) manacost = (manacost / 2);
				else if (p_ptr->skill[25] >= 50) manacost = (manacost - (manacost / 3));
				else if (p_ptr->skill[25] >= 25) manacost = (manacost - (manacost / 4));

                                sprintf(tmp, "%d          ", manacost);
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
                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                        c_put_str(TERM_WHITE, "---------------------------", 1, 0);
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
				if (p_ptr->skill[25] >= 100) manacost = (manacost / 2);
				else if (p_ptr->skill[25] >= 50) manacost = (manacost - (manacost / 3));
				else if (p_ptr->skill[25] >= 25) manacost = (manacost - (manacost / 4));

                                sprintf(tmp, "%d          ", manacost);
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
                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                        c_put_str(TERM_WHITE, "---------------------------", 1, 0);
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
				if (p_ptr->skill[25] >= 100) manacost = (manacost / 2);
				else if (p_ptr->skill[25] >= 50) manacost = (manacost - (manacost / 3));
				else if (p_ptr->skill[25] >= 25) manacost = (manacost - (manacost / 4));

                                sprintf(tmp, "%d          ", manacost);
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
                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                        c_put_str(TERM_WHITE, "---------------------------", 1, 0);
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

                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
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
                                if (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 8] >= 1)
                                {
                                        redpercent = p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 8];
                                        if (redpercent > 75) redpercent = 75;
                                        manacost -= ((manacost * redpercent) / 100);
                                        manacost -= (p_ptr->abilities[(CLASS_HIGH_MAGE * 10) + 8] * 3);
                                        if (manacost < 0) manacost = 0;
                                }
				/* Mastery of Conjuration spells! */
				if (p_ptr->skill[25] >= 100) manacost = (manacost / 2);
				else if (p_ptr->skill[25] >= 50) manacost = (manacost - (manacost / 3));
				else if (p_ptr->skill[25] >= 25) manacost = (manacost - (manacost / 4));

                                sprintf(tmp, "%d          ", manacost);
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
                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                        c_put_str(TERM_WHITE, "---------------------------", 1, 0);
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
                        if (p_ptr->divination_effects & DIVI_DETECT_TRAPS) c_put_str(TERM_WHITE, "[4] Detect Traps", 11, 0);
                        else c_put_str(TERM_L_DARK, "[4] Detect Traps", 11, 0);
                        if (p_ptr->divination_effects & DIVI_TELEPATHY) c_put_str(TERM_WHITE, "[5] Telepathy", 12, 0);
                        else c_put_str(TERM_L_DARK, "[5] Telepathy", 12, 0);
                        if (p_ptr->divination_effects & DIVI_IDENTIFY) c_put_str(TERM_WHITE, "[6] Identify", 13, 0);
                        else c_put_str(TERM_L_DARK, "[6] Identify", 13, 0);
                        if (p_ptr->divination_effects & DIVI_SCAN_MONSTER) c_put_str(TERM_WHITE, "[7] Scan Monster", 14, 0);
                        else c_put_str(TERM_L_DARK, "[7] Scan Monster", 14, 0);
                        if (p_ptr->divination_effects & DIVI_REVEAL) c_put_str(TERM_WHITE, "[8] Reveal", 15, 0);
                        else c_put_str(TERM_L_DARK, "[8] Reveal", 15, 0);



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
                                        if (!(p_ptr->divination_effects & DIVI_DETECT_TRAPS)) msg_print("You haven't learned this effect yet.");
                                        else
                                        {
                                                efftype = 5;
                                                effbase = 10;
                                                effcost = 0;
                                                effkind = 16;
                                                interfacetype = 1;
                                                sprintf(choseneffect, "Detect Traps");
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
                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                        c_put_str(TERM_WHITE, "---------------------------", 1, 0);
                        c_put_str(TERM_WHITE, "Selected effect:", 4, 0);
                        c_put_str(TERM_WHITE, choseneffect, 4, 17);

                        spelltype = 0;
                        while (readytocreate == FALSE)
                        {
                                c_put_str(TERM_WHITE, "Mana Cost:", 12, 0);
                                manacost = (effbase + ((effcost * power)));
				/* Mastery of Divination spells! */
				if (p_ptr->skill[26] >= 100) manacost = (manacost / 2);
				else if (p_ptr->skill[26] >= 50) manacost = (manacost - (manacost / 3));
				else if (p_ptr->skill[26] >= 25) manacost = (manacost - (manacost / 4));
				manacost -= p_ptr->skill[26];
				if (manacost < 0) manacost = 0;
                                sprintf(tmp, "%d          ", manacost);
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
                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                        c_put_str(TERM_WHITE, "---------------------------", 1, 0);
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
				/* Mastery of Divination spells! */
				if (p_ptr->skill[26] >= 100) manacost = (manacost / 2);
				else if (p_ptr->skill[26] >= 50) manacost = (manacost - (manacost / 3));
				else if (p_ptr->skill[26] >= 25) manacost = (manacost - (manacost / 4));
				manacost -= p_ptr->skill[26];
				if (manacost < 0) manacost = 0;
                                sprintf(tmp, "%d          ", manacost);
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
                        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
                        c_put_str(TERM_WHITE, "---------------------------", 1, 0);
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
				/* Mastery of Divination spells! */
				if (p_ptr->skill[26] >= 100) manacost = (manacost / 2);
				else if (p_ptr->skill[26] >= 50) manacost = (manacost - (manacost / 3));
				else if (p_ptr->skill[26] >= 25) manacost = (manacost - (manacost / 4));
				manacost -= p_ptr->skill[26];
				if (manacost < 0) manacost = 0;
                                sprintf(tmp, "%d          ", manacost);
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
        c_put_str(TERM_WHITE, "NEWANGBAND SPELL CREATION", 0, 0);
        c_put_str(TERM_WHITE, "---------------------------", 1, 0);
        for (i = 0; i < 5; i++)
        {
                finalcost = finalcost + spell_ptr->manacost[i];
        }
        finalcost = finalcost * effnum;
        c_put_str(TERM_WHITE, "Final Cost:", 8, 0);
        sprintf(tmp, "%d          ", finalcost);
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
			/* Dragons and demons are more expensive. */
			if ((r_ptr->flags3 & (RF3_DRAGON)) || (r_ptr->flags3 & (RF3_DEMON)))
			{
				manacost *= 3;
			}
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
                        sprintf(spellstring, "%s   Cost: %d", spell_ptr->name, spell_ptr->finalcost);        
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
                        manacost = k_ptr->locale[0] / 5;
                        manacost += manacost * ((k_ptr->locale[0] / 11) * 4);
                        manacost = manacost / 10;                         
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
