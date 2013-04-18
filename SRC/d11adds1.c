/* File: d11adds1.c */

/*
 * Copyright (c) 2001 Stefan "Dunkelelf" Jurisch
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 */

/* Purpose: Additions for the special D11-Angband Version */

#include "angband.h"


/*
 * Static Variables
 */

/* For Attribute Training */
char attribs1[7][30] =
{
	" a) Strength     (STR) ",
	" b) Intelligence (INT) ",
	" c) Wisdom       (WIS) ",
	" d) Dexterity    (DEX) ",
	" e) Constitution (CON) ",
	" f) Charisma     (CHR) ",
};

char attribs2[7][13] =
{
	"Strength",
	"Intelligence",
	"Wisdom",
	"Dexterity",
	"Constitution",
	"Charisma",
};

char train_no_success[4][70] =
{
	"",
	"Oh no! You were not able to gain more",
	"What a pity! You had no success training your",
	"Don't worry! Next time you will do better training your",
};

char train_min_success[4][70] =
{
	"",
	"Okay. You gained some more",
	"That's right. Now you have some more",
	"It's okay. You have successfully trained your",
};

char train_norm_success[4][70] =
{
	"",
	"All right! That was a success training you",
	"That's it. You have very successfully trained your",
	"Okay! Good training. You gained some more",
};

char train_max_success[4][70] =
{
	"",
	"Great! That's a really succes gaining more",
	"Superb! Now you have really more",
	"Mighty! This is the meaning of training",
};

/* For Special Drow Abilities */
char abilities[7][70] =
{
	" a) Cure Light Wounds     ",
	" b) Dimension Door        ",
	" c) Sense Traps and Doors ",
	" d) Feel Monster Presence ",
	" e) Cure Poison           ",
	" f) Teleport              ",
};

char abilities_desc[7][70] =
{
	"cure some of your wounds",
	"open a dimension door",
	"sense traps and doors",
	"feel the presence of monsters",
	"cure your body from poison",
	"teleport yourself away",
};


s16b ab_fail[7];	/* Fail-rates of the Drow Powers */
s16b ab_level[7] =	/* Level of the Drow Powers */
{
	1,
	3,
	5,
	7,
	9,
	11,
};
s16b ab_turns[7] =	/* useable every x Turns */
{
	500,
	500,
	500,
	1000,
	1000,
	1500,
};
s16b level_backup = 0;

/*
 * Train attribute after level gain
 */
extern int train_attribute(void)
{
	int i;					/* Counter for Loops */
	char in;
	int modify;
	int max_level;
	int training_efficiency;
	int no_effect, min_effect, max_effect;
	int dice;
	s16b stat_new;
	char msg_part[70];

	/* Ask, if it is possible to train (training-value > 0) */
	if (p_ptr->training > 0)
	{

		/* Init Variables */
		in = '\0';
		modify = 0;
		max_level = p_ptr->max_depth;
		training_efficiency = 0;
		no_effect = 10;
		min_effect = 60;
		max_effect = 100;

		/* Save the screen */
		screen_save();

		/* Show Attributes on Screen */
		for (i = 0; i < 6; i++)
			c_put_str(TERM_GREEN, attribs1[i], (i + 1), 30);

		/* Ask, which attribute should be trained */
		c_put_str(TERM_WHITE, "Which attribute do you want to train? (a-f):", 0, 0);

		/* Key-Loop. do not end, as long as the key is not between a - f or ESC */
		while (TRUE)
		{
			in = inkey(); /* Wait for keypress */
			if ((in < 'a' || in > 'f' || in == '\0') && in != ESCAPE)
				continue;
			else
			    break;
		}

		/* Reload the screen */
		screen_load();

		/* After keypress erase the message */
		Term_erase(0, 0, 255);

		/* If Escape is pressed, get out of this routine */
		if (in == ESCAPE)
			return 0;

		/* Set stat-number of stat wich will be tried to train */
		modify = in - 'a';

		/* Here we get the value, when the training results in maximum effect (3 points)
		in general (at game start) there are the following training effects on the value of 1d100:
		1-10:	no effect - the training had no success
		11-60:	minimum effect - the training brought the minimum of 1 point to the attribute
		61-99:	optimal effect - the training brought the optimal access of 2 points
		100:	maximal effect - the great success! very good training.
		the maximum effect value decreases with the maximal depth in levels the player has advanced to,
		because the deeper the player goes the harder is the training for the body.
		when training on level 20, he has a chance of 20% (80 or more on 1d100) to increase a stat
		by 3 points. the minimal value for that is a 33% chance (78-100 on 1d100) */
		/* Set the value for the maximal efficiency */
		if (max_level < 40)
			max_effect = 100 - (max_level / 2);
		else
		    max_effect = 80;

		/* roll the dice */
		dice = randint(100);

		/* test for the efficiency
		this will be done by 3 following if-tasks, where the efficiency will be increased by 1 every
		time, a question is true. */
		/* first step - starting with 0 (no effect) */
		if (dice > 10)
			training_efficiency++;
		/* second step - till now efficiency is 1 */
		if (dice > min_effect)
			training_efficiency++;
		/* third step - now efficiency is 2 */
		if (dice >= max_effect)
			training_efficiency++;

		/* Now we modify the chosen stat
		for keeping all simply, the temporary stats will be modified in this way, too. */
		stat_new = modify_stat_value(p_ptr->stat_max[modify], training_efficiency);
		p_ptr->stat_max[modify] = stat_new;
		stat_new = modify_stat_value(p_ptr->stat_cur[modify], training_efficiency);
		p_ptr->stat_cur[modify] = stat_new;
		stat_new = modify_stat_value(p_ptr->stat_top[modify], training_efficiency);
		p_ptr->stat_top[modify] = stat_new;
		stat_new = modify_stat_value(p_ptr->stat_use[modify], training_efficiency);
		p_ptr->stat_use[modify] = stat_new;

		/* Generate and print a (no) success message */
		switch (training_efficiency)
		{
		case 0:
			strcpy(msg_part, train_no_success[randint(3)]);
			break;

		case 1:
			strcpy(msg_part, train_min_success[randint(3)]);
			break;

		case 2:
			strcpy(msg_part, train_norm_success[randint(3)]);
			break;

		case 3:
			strcpy(msg_part, train_max_success[randint(3)]);
			break;
		}

		msg_format("%s %s (+%d).", msg_part, attribs2[modify], training_efficiency);

		/* Finally we delete the Status of Training ability */
		p_ptr->training--;
	}
	else	/* No Training possible */
		msg_format("Sorry! You are not able to train any attribute.");


	/* Some Stuff before finishing. Then finish! */

	/* Update some stuff */
	p_ptr->update |= (PU_BONUS | PU_HP | PU_TRAIN | PU_UPDATE_VIEW);

	/* Redraw some stuff */
	p_ptr->redraw |= (PR_STATS | PR_TRAIN);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

	/* Handle stuff */
	handle_stuff();


	return 0;
}

/*
 * Better Word of Recall (Choose a recall depth)
 */
int input_recall_depth(void)
{
	int recall_depth;
	char prompt[80];
	char buf[80];

	/* Set some Variables */
	recall_depth = 0;
	sprintf(prompt, "Recall to which depth? (1-%d) [%d]: ", p_ptr->max_depth, p_ptr->max_depth);
	strcpy(buf, "");

	/* Input the recall depth (no input means max depth */
	if (!get_string(prompt, buf, 7)) return (p_ptr->max_depth);

	/* Extract the depth from input */
	recall_depth = atoi(buf);

	/* A letter means max depth */
	if (isalpha(buf[0]))
		recall_depth = p_ptr->max_depth;

	/* Return the depth */
	return recall_depth;
}

/*
 * Drow special Powers (initiated with SHIFT-U)
 *
 * The following special Drow-Powers are available:
 * a) Cure Light Wounds:		every 500 Turns / possible at Character-Level 1
 * b) Phase Door:				every 500 Turns / possible at Character-Level 3
 * c) sense traps and doors:	every 500 Turns / possible at Character-Level 5
 * d) Feel monster presence:	every 1000 Turns / possible at Character-Level 7
 * e) Cure Poison:				every 1000 Turns / possible at Character-Level 9
 * f) Teleport:					every 1500 Turns / possible at Character-Level 11
 */

/*
 * Use a special Drow Power
 */
void do_cmd_use_drowpower(void)
{
	int i;
	int col;
	char buf[70];
	cptr status;
	char in;
	int useable;
	cptr r;
	int rr;
	int fail;

	in = '\0';

	screen_save(); /* Save the Screen */

	/* Use of special Powers only as drow/drider */
	r = p_name + rp_ptr->name;
	if ((strcmp(r, "Drow") != 0) && (strcmp(r, "Drider") != 0))
	{
		screen_load();
		msg_format("You are not able to use special Drow Powers.");
		return;
	}

	/* Dump the Special Powers Choice List on the Screen */
	c_put_str(TERM_WHITE, " Special Drow Power        Lv Fail Status       ", 1, 20);
	for (i = 0; i < 6; i++)
	{
		if (ab_level[i] > p_ptr->lev)
		{
			col = TERM_L_RED;
			status =     "difficult   ";
		}
		else
			if (p_ptr->ab_turn_count[i] >= ab_turns[i])
			{
				col = TERM_L_GREEN;
				status = "useable     ";
			}
			else
			{
				col = TERM_L_BLUE;
				status = "regenerating";
			}

		/* Create Text row and print */
		sprintf(buf, "%s %2d  %2d%% %s ", abilities[i], ab_level[i], ab_fail[i], status);
		c_put_str(col, buf, (i + 2), 20);
	}

	/* Ask, which Power should be used */
	c_put_str(TERM_WHITE, "Which Power do you want to use? (a-f):", 0, 0);

	/* Key-Loop. do not end, as long as the key is not between a - f or ESC*/
	while (TRUE)
	{
		in = inkey(); /* Wait for keypress */
		if ((in < 'a' || in > 'f' || in == '\0') && in != ESCAPE)
			continue;
		else
		    break;
	}

	/* Erase the Term */
	Term_erase(0, 0, 255);

	/* If Escape is pressed, get out of this routine */
	if (in == ESCAPE)
	{
		screen_load();
		return;
	}

	/*
	* Here starts the handling of the Powers
	*/
	useable = in - 'a' + 1;

	/* Cannot use power yet */
	if (useable > p_ptr->abilities)
	{
		screen_load();
		msg_format("You are not able to use this special Drow Power yet! %d:%d", useable, p_ptr->abilities);
		return;
	}

	/* Cannot use power because charging */
	if (p_ptr->ab_turn_count[useable - 1] < ab_turns[useable - 1])
	{
		screen_load();
		c_put_str(TERM_WHITE, "You try and try and try... but the special Power does not activate.", 0, 0);
		return;
	}

	/* Use of power failed */
	fail = randint(100);
	if (fail < ab_fail[useable - 1])
	{
		screen_load();
		msg_format("You tried but you failed to %s.", abilities_desc[useable - 1]);
		return;
	}

	/* Handle the powers here */
	switch (in)
	{
	case 'a': /* Cure light wounds */
		{
			(void)hp_player(damroll(2, 10));
			(void)set_cut(p_ptr->cut - 10);
			break;
		}

	case 'b': /* DimDoor */
		{
			teleport_player(10);
			break;
		}

	case 'c': /* Detect traps and doors */
		{
			(void)detect_traps();
			(void)detect_doors();
			(void)detect_stairs();
			break;
		}

	case 'd': /* detect monsters */
		{
			(void)detect_monsters_normal();
			break;
		}

	case 'e': /* Cure Poison */
		{
			(void)set_poisoned(0);
			break;
		}

	case 'f': /* Teleport */
		{
			teleport_player(p_ptr->lev * 5);
			break;
		}

	}

	/* Set Ability Counter to '0' and give a success message  */
	screen_load();
	p_ptr->ab_turn_count[useable - 1] = 0;
	msg_format("You %s.", abilities_desc[useable - 1]);

	return;
}

/*
 * Modify the Drow Powers
 */
void modify_drowpowers(void)
{
	int i;

	/* Gain an ability if possible */
	if (p_ptr->lev >= ab_level[p_ptr->abilities])
	{
		p_ptr->abilities++;
		/* Paranoia!!! No more abilities than 6!!! */
		if (p_ptr->abilities > 6)
			p_ptr->abilities = 6;
	}

	/* possibly lose an ability */
	if (p_ptr->lev < ab_level[p_ptr->abilities - 1])
	{
		p_ptr->abilities--;
		/* Paranoia: abilities cannot be less than 1!!! */
		if (p_ptr->abilities < 1)
			p_ptr->abilities = 1;
	}

	/* Set fail rates to maximum */
	for (i = 0; i < 7; i++)
		ab_fail[i] = MAX_AB_FAIL;

	/* and calculate the actual ones */
	for (i = 0; i < p_ptr->abilities; i++)
	{
		if (p_ptr->lev >= ab_level[i])
		{
			ab_fail[i] = MAX_AB_FAIL - ((p_ptr->lev - ab_level[i] + 1) * 3);
			if (ab_fail[i] < MIN_AB_FAIL)
				ab_fail[i] = MIN_AB_FAIL;
		}
	}
}

/*
 * Update the Drow Powers
 */
void update_abilities(void)
{
	int i;

	/* Modify/Set the drow powers at game start */
	if (!level_backup)
	{
		level_backup = p_ptr->lev;
		modify_drowpowers();
	}

	/* Count up the drow power turn counters */
	for (i = 0; i < 7; i++)
	{
		if (p_ptr->ab_turn_count[i] < (ab_turns[i] + 1))
			p_ptr->ab_turn_count[i]++;
	}

	/* Modify the drow powers when a level was gained */
	if (p_ptr->lev != level_backup)
	{
		level_backup = p_ptr->lev;
		modify_drowpowers();
	}
}

