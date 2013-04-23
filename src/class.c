/* File: class.c */

/* Purpose: Class ability code / was mind.c */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

class_power class_powers[CLASS_ABILITY_MAX] =
{
	/* Level, tech type, Stat to use, cost, fail stat, %fail, name */
	{ 1,  	1,  	1, 	 1, 	A_WIS, 	15, 	"Neural Blast" },          /* ~MM */
	{ 2,  	1,  	1, 	 1, 	A_WIS, 	20, 	"Precognition" },          /* Det. monsters/traps */
	{ 3,  	1,  	1, 	 2, 	A_WIS, 	25, 	"Minor Displacement" },    /* Blink */
	{ 7,  	1,  	1, 	 6, 	A_WIS,	35, 	"Major Displacement" },    /* Tele. Self / All */
	{ 9,  	1,  	1, 	 7, 	A_WIS, 	50, 	"Domination" },
	{ 11, 	1,  	1, 	 7, 	A_WIS, 	30, 	"Pulverise" },             /* Telekinetic "bolt" */
	{ 13, 	1,  	1, 	12, 	A_WIS, 	50, 	"Character Armour" },      /* Psychic/physical defenses */
	{ 15, 	1,  	1, 	12, 	A_WIS, 	60, 	"Psychometry" },
	{ 18, 	1,  	1, 	10, 	A_WIS, 	45, 	"Mind Wave" },             /* Ball -> LOS */
	{ 23, 	1,  	1, 	15, 	A_WIS, 	50, 	"Adrenaline Channeling" },
	{ 25, 	1,  	1, 	10, 	A_WIS, 	40, 	"Psychic Drain" },         /* Convert enemy HP to mana */
	{ 28, 	1,  	1, 	20, 	A_WIS, 	45, 	"Telekinetic Wave" },      /* Ball -> LOS */
	{ 1,  	2,  	3, 	10, 	A_CON, 	25, 	"Training" },      	   /* Restore Physical Stats */
	{ 8,  	3,  	3, 	10, 	A_WIS, 	 9, 	"Berserk" },      	   /* Berserk */
	{ 1,  	4,  	2, 	 6, 	A_INT, 	20, 	"Make Ammunition" },       /* Shots, Arrows and Bolts */
	{ 1,   	5, 	2, 	 2, 	A_INT, 	10, 	"Refuel" },       	   /* Torches and Lamps */
	{ 2,   	5, 	2, 	 3, 	A_INT, 	14, 	"Dart" },       	   /* MM */
	{ 5,   	5, 	2, 	 5, 	A_INT, 	24, 	"Speed Potion" },
	{ 6,   	5, 	2, 	10, 	A_INT, 	50, 	"Create Weapon" },
	{ 8,   	5, 	2, 	10, 	A_INT, 	30, 	"Healing Potion" },
	{ 10,  	5, 	2, 	 7, 	A_INT, 	20, 	"Blast Wave" },       	   /* Ball */      	 
	{ 15,   5, 	2, 	15, 	A_INT, 	40, 	"Protective Potion" },
	{ 18,	5,	2,	20,	A_INT,	50,	"Recharge Magic" },
	{ 20,  	5, 	2, 	25, 	A_INT, 	75, 	"Create Weapon II" },
	{ 25,  	5, 	2, 	30, 	A_INT, 	60, 	"Restoring Potion" },
	{ 30,	5,	2,	40,	A_INT,	80,	"Improve weapon" },
	{ 30,	5,	2,	45,	A_INT,	75,	"Improve armour" },
	{ 35,	5,	2,	50,	A_INT,	65,	"Fission beam" },
	{ 40,	5,	2,	60,	A_INT,	70,	"Explosives" },
	{ 48,	5,	2,	100,	A_INT,	90,	"Nuke" },		   /* Ball */
	{ 5,   	6, 	2, 	 5, 	A_INT, 	24, 	"Speed Potion" },
	{ 8,   	6, 	2, 	10, 	A_INT, 	30, 	"Healing Potion" },
	{ 15,   6, 	2, 	15, 	A_INT, 	40, 	"Protective Potion" },
	{ 18,	6,	2,	20,	A_INT,	50,	"Recharge Magic" },
	{ 25,  	6, 	2, 	30, 	A_INT, 	60, 	"Restoring Potion" },
	{ 5,  	7, 	1, 	10, 	A_WIS, 	10, 	"Turn Undead" },
	{ 7,  	8, 	1, 	20, 	A_WIS, 	10, 	"Healing Touch" },
};


void class_ability_info(char *p, int power)
{
	int plev = p_ptr->lev;

	strcpy(p, "");

	switch (power)
	{
		case MINDCRAFT_NEURAL_BLAST:
			sprintf(p, " dam %dd%d", 3 + ((plev - 1) / 4), 3 + plev / 15);
			break;
		case MINDCRAFT_MINOR_DISPLACEMENT:
			sprintf(p, " range %d", (plev < 25 ? 2 : plev + 2));
			break;
		case MINDCRAFT_MAJOR_DISPLACEMENT:
			sprintf(p, " range %d", plev * 5);
			break;
		case MINDCRAFT_PULVERISE:
			sprintf(p, " dam %dd8", 8 + ((plev - 5) / 4));
			break;
		case MINDCRAFT_CHARACTER_ARMOUR:
			sprintf(p, " dur %d", plev);
			break;
		case MINDCRAFT_MIND_WAVE:
			sprintf(p, " dam %d", plev * ((plev - 5) / 10 + 1));
			break;
		case MINDCRAFT_ADRENALINE_CHANNELING:
			sprintf(p, " dur 11-%d", plev + plev / 2 + 10);
			break;
		case MINDCRAFT_PSYCHIC_DRAIN:
			sprintf(p, " dam %dd6", plev / 2);
			break;
		case MINDCRAFT_TELEKINETIC_WAVE:
			sprintf(p, " dam %d", plev * (plev > 39 ? 4: 3));
			break;
		case BERSERK_BERSERK:
			sprintf(p, " dur 20+1d%d", plev);
			break;
		case TECH_DART:
			sprintf(p, " dam %dd4", 3 + ((plev - 1) / 4));
			break;
		case TECH_BOMB:
			strcpy (p, " dam 5d5");
			break;
		case TECH_FISSION_BEAM:
			sprintf(p, " dam %dd6", 6 + plev/4);
			break;
		case TECH_MEGA_NUKE:
			strcpy (p, " dam 30d30");
			break;
		case PALADIN_HEAL_TOUCH:
			sprintf(p, " heal %d", plev * 2);
		default:
			break;
	}
}


/*
 * Allow user to choose a class power.
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
 *
 * What bug? -SF-
 */
static int get_class_power(int *sn)
{
	int             i;
	int             num = 0;
	int             y = 1;
	int             x = 20;
	int             minfail;
	int             plev = p_ptr->lev;
	int             chance;
	int             ask;
	int		excess;
	char            choice;
	char            out_val[160];
	char            comment[80];
	cptr            p = "power";
	class_power 	spell;
	bool            flag, redraw;

	cptr		thing;

	/* Assume cancelled */
	*sn = (-1);

	/* Get the spell, if available */
	if (repeat_pull(sn))
	{
		/* Verify the spell */
		if (class_powers[*sn].min_lev <= plev)
		{
			/* Success */
			return (TRUE);
		}
	}

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	for (i = mp_ptr->class_first; i < CLASS_ABILITY_MAX; i++)
	{
		if ((class_powers[i].min_lev <= plev) && (class_powers[i].tech_type == mp_ptr->tech_type))
		{
			num++;
		}
	}
	
	if (num == 0)
	{
		msg_print("You don't have any class abilities. (yet)");
		return (FALSE);
	}
	
	/* Build a prompt (accept all spells) */
	(void)strnfmt(out_val, 78, "(%^ss %c-%c, *=List, ESC=exit) Use which %s? ",
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
				screen_save();

				/* Display a list of spells */
				prt("", y, x);
				put_str("Name", y, x + 5);
				put_str("Lv Cost Fail Info", y, x + 35);

				/* Dump the spells */
				for (i = mp_ptr->class_first; i < CLASS_ABILITY_MAX; i++)
				{
					if (class_powers[i].use_stat == 1)
					{
						thing = p_ptr->csp;
					}
					else if (class_powers[i].use_stat == 2)
					{
						thing = p_ptr->crp;
					}
					else
					{
						thing = p_ptr->chp;
					}
					
					/* Access the spell */
					spell = class_powers[i];
					if ((spell.min_lev > plev) || (class_powers[i].tech_type != mp_ptr->tech_type)) break;

					chance = spell.fail;

					/* Reduce failure rate by "effective" level adjustment */
					chance -= 3 * (plev - spell.min_lev);

					/* Reduce failure rate by stat adjustment */
					chance -= 3 * (adj_mag_stat[p_ptr->stat_ind[class_powers[i].fail_stat]] - 1);

					/* Not enough mana to cast */
					if (spell.cost > thing)
					{
						excess = thing;
						chance += 5 * (spell.cost - excess);
					}

					/* Extract the minimum failure rate */
					minfail = adj_mag_fail[p_ptr->stat_ind[class_powers[i].fail_stat]];

					/* Minimum failure rate */
					if (chance < minfail) chance = minfail;

					/* Stunning makes spells harder */
					if (p_ptr->stun > 50) chance += 25;
					else if (p_ptr->stun) chance += 15;

					/* Always a 5 percent chance of working */
					if (chance > 95) chance = 95;

					/* Get info */
					class_ability_info(comment, i);

					/* Dump the spell --(-- */
					sprintf(psi_desc, "  %c) %-30s%2d %4d %3d%%%s",
					        I2A(i - mp_ptr->class_first), spell.name,
					        spell.min_lev, spell.cost, chance, comment);
					prt(psi_desc, y + i + 1 - mp_ptr->class_first, x);
				}

				/* Clear the bottom line */
				prt("", y + i + 1 - mp_ptr->class_first, x);
			}

			/* Hide the list */
			else
			{
				/* Hide list */
				redraw = FALSE;

				/* Restore the screen */
				screen_load();
			}

			/* Redo asking */
			continue;
		}

		/* Note verify */
		ask = isupper(choice);

		/* Lowercase */
		if (ask) choice = tolower(choice);

		/* Extract request */
		i = (islower(choice) ? A2I(choice) : -1);

		i += (mp_ptr->class_first);
		/* Totally Illegal */
		if ((i < mp_ptr->class_first) || (i >= num + mp_ptr->class_first))
		{
			bell();
			continue;
		}

		/* Save the spell index */
		spell = class_powers[i];


		/* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Prompt */
			(void)strnfmt(tmp_val, 78, "Use %s? ", class_powers[i].name);

			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}

	/* Restore the screen */
	if (redraw) screen_load();

	/* Show choices */
	/* Update */
	p_ptr->window |= (PW_SPELL);

	/* Window stuff */
	window_stuff();
	
	/* Abort if needed */
	if (!flag) return (FALSE);

	/* Save the choice */
	(*sn) = i;

	repeat_push(*sn);

	/* Success */
	return (TRUE);
}

/*
 * do_cmd_class calls this function if the player's class
 * is 'archer'.
 * From Hengband and modified.
 */
static bool do_cmd_archer(void)
{
	int ext=0;
	char ch;

	object_type	forge;
	object_type     *q_ptr;

	char com[80];

	q_ptr = &forge;

	if(p_ptr->lev >= 20)

		sprintf(com, "Create [S]hots, Create [A]rrow or Create [B]olt ?");

	else if(p_ptr->lev >= 10)

		sprintf(com, "Create [S]hots or Create [A]rrow ?");

	else

		sprintf(com, "Create [S]hots ?");


	if (p_ptr->confused)
	{

		msg_print("You are too confused!");

		return FALSE;
	}

	if (p_ptr->blind)
	{

		msg_print("You are blind!");

		return FALSE;
	}

	while (TRUE)
	{
		if (!get_com(com, &ch))
		{
			return FALSE;
		}
		if (ch == 'S' || ch == 's')
		{
			ext = 1;
			break;
		}
		if ((ch == 'A' || ch == 'a')&&(p_ptr->lev >= 10))
		{
			ext = 2;
			break;
		}
		if ((ch == 'B' || ch == 'b')&&(p_ptr->lev >= 20))
		{
			ext = 3;
			break;
		}
	}

	/**********Create shots*********/
	if (ext == 1)
	{
		int x,y, dir, i;
		cave_type *c_ptr;

		msg_print(" Rubble?");
		if (!tgt_pt(&x, &y)) return FALSE;
		if (!in_bounds2(y, x)) return FALSE;
		if (distance(y, x, p_ptr->py, p_ptr->px) >  2) return FALSE;
		c_ptr = &cave[y][x];
		if (c_ptr->feat == FEAT_RUBBLE)
		{
			i = 50;
			msg_print(" You use the rubble");
			(void)project(0, 0, y, x, 0, GF_KILL_WALL, PROJECT_GRID);
		}
		else
		{
			i = 24;
		}
			/* Get local object */
			q_ptr = &forge;

			/* Hack -- Give the player some small firestones */
			object_prep(q_ptr, lookup_kind(TV_SHOT, randint1(p_ptr->lev)*3/50));

			q_ptr->number = (byte)rand_range(i/2,i);
			object_aware(q_ptr);
			object_known(q_ptr);
			apply_magic(q_ptr, p_ptr->lev, p_ptr->lev, FALSE);
			q_ptr->discount = 99;

			(void)inven_carry(q_ptr);

			msg_print("You make some ammo.");

			p_ptr->update |= (PU_VIEW | PU_FLOW);
			p_ptr->window |= (PW_OVERHEAD);
	}
	/**********Create arrows*********/
	else if (ext == 2)
	{

		/* Get local object */
		q_ptr = &forge;

		/* Hack -- Give the player some arrows */
		object_prep(q_ptr, lookup_kind(TV_ARROW, randint1(p_ptr->lev)*3/50));
		q_ptr->number = (byte)rand_range(5,10);
		object_aware(q_ptr);
		object_known(q_ptr);
		apply_magic(q_ptr, p_ptr->lev, p_ptr->lev, FALSE);

		q_ptr->discount = 99;

		msg_print("You make some ammo.");

		(void)inven_carry(q_ptr);
	}
	/**********Create bolts*********/
	else if (ext == 3)
	{

		/* Get local object */
		q_ptr = &forge;

		/* Hack -- Give the player some bolts */
		object_prep(q_ptr, lookup_kind(TV_BOLT, randint1(p_ptr->lev)*3/50));
		q_ptr->number = (byte)rand_range(4,8);
		object_aware(q_ptr);
		object_known(q_ptr);
		apply_magic(q_ptr, p_ptr->lev, p_ptr->lev, FALSE);

		q_ptr->discount = 99;

		msg_print("You make some ammo.");

		(void)inven_carry(q_ptr);
	}
	return TRUE;
}

/* Make a potion */
static bool make_potion(int type)
{
	int sv;
	int plev = p_ptr->lev;
	
	int chance = randint1(50) + plev/10;
	
	object_type	forge;
	object_type     *q_ptr;
	q_ptr = &forge;
		
	if (p_ptr->pclass == CLASS_WITCH)
	{
		chance += 5;
	}
	if (p_ptr->pclass == CLASS_TECH_WAR)
	{
		chance -= 5;
	}
	
	switch (type)
	{
	case 0:
		if (chance <= 25)
		{
			sv = 4;
		}
		else 
		{
			sv = 29;
		}
		break;
	case 1:
		if (chance <= 5)
		{
			sv = 15;
		}
		else if (chance <= 7)
		{
			sv = 13;
		}
		else if (chance <= 10)
		{
			sv = 16;
		}
		else if (chance <= 13)
		{
			sv = 17;
		}
		else if (chance <= 16)
		{
			sv = 18;
		}
		else if (chance <= 19)
		{
			sv = 19;
		}
		else if (chance <= 21)
		{
			sv = 20;
		}
		else if (chance <= 26)
		{
			sv = 21;
		}
		else if (chance <= 31)
		{
			sv = 47;
		}
		else if (chance <= 36)
		{
			sv = 46;
		}
		else if (chance <= 41)
		{
			sv = 45;
		}
		else if (chance <= 46)
		{
			sv = 44;
		}
		else if (chance <= 51)
		{
			sv = 43;
		}
		else if (chance <= 55)
		{
			sv = 42;
		}
		else if (chance <= 58)
		{
			sv = 41;
		}
		else
		{
			sv = 55;
		}
		break;
	case 2:
		if (chance <= 4)
		{
			sv = 6;
		}
		else if (chance <= 10)
		{
			sv = 5;
		}
		else if (chance <= 21)
		{
			sv = 34;
		}
		else if (chance <= 31)
		{
			sv = 35;
		}
		else if (chance <= 40)
		{
			sv = 36;
		}
		else if (chance <= 48)
		{
			sv = 61;
		}
		else if (chance <= 55)
		{
			sv = 37;
		}
		else if (chance <= 59)
		{
			sv = 38;
		}
		else
		{
			sv = 39;
		}
		break;
	case 3:
		if (chance <= 7)
		{
			sv = 9;
		}
		else if (chance <= 18)
		{
			sv = 30;
		}
		else if (chance <= 28)
		{
			sv = 31;
		}
		else if (chance <= 38)
		{
			sv = 32;
		}
		else if (chance <= 46)
		{
			sv = 33;
		}
		else if (chance <= 58)
		{
			sv = 60;
		}
		else
		{
			sv = 62;
		}
		break;
	}
	
	object_prep(q_ptr, lookup_kind(TV_POTION, sv));
	q_ptr->discount = 99;
	msg_print("You make a potion.");
	(void)inven_carry(q_ptr);
}

/* Make a weapon */
static bool make_weapon(int boost)
{
	int sv = 0;
	int plev = p_ptr->lev;
	int type = 20 + randint1(3);
	object_type	forge;
	object_type     *q_ptr;
	q_ptr = &forge;
		
	/* Note: Attempt twice */
	switch (type)
	{
	case 21:
		sv = randint1(9) + boost * 9;
		if (p_ptr->pclass == CLASS_TECH_WAR)
			{
				sv += randint1(3) - 1;
			}
		if ((sv == 9) || (sv == 10))
		{
			sv = randint1(9) + boost * 9;
			if (p_ptr->pclass == CLASS_TECH_WAR)
				{
					sv += randint1(3) - 1;
				}
			if ((sv == 9) || (sv == 10))
			{
				msg_print(" You fail. ");
				return;
			}
		}
		break;
	case 22:
		sv = randint1(14) + boost * 13;
		if (p_ptr->pclass == CLASS_TECH_WAR)
		{
			sv += randint1(3) - 1;
		}
		if ((sv == 18) || (sv == 19) || (sv == 21) || (sv == 23) || (sv == 24) || (sv == 27))
		{
			sv = randint1(14) + boost * 13;
			if (p_ptr->pclass == CLASS_TECH_WAR)
			{
				sv += randint1(3) - 1;
			}
			if ((sv == 18) || (sv == 19) || (sv == 21) || (sv == 23) || (sv == 24) || (sv == 27))
			{
				msg_print(" You fail. ");
				return;
			}
		}
		break;
	case 23:
		sv = randint1(15) + boost * 14;
		if (p_ptr->pclass == CLASS_TECH_WAR)
		{
			sv += randint1(3) - 1;
		}
		if (sv == 3)
		{
			sv = randint1(15) + boost * 14;
			if (p_ptr->pclass == CLASS_TECH_WAR)
			{
				sv += randint1(3) - 1;
			}
			if (sv == 3)
			{
				msg_print(" You fail. ");
				return;
			}
		}
		break;
	}
	
	object_prep(q_ptr, lookup_kind(type, sv));
	q_ptr->discount = 99;
	msg_print("You make a weapon.");
	(void)inven_carry(q_ptr);
}

/*
 * do_cmd_cast calls this function if the player's class
 * is 'mindcrafter'.
 */
static bool cast_class_spell(int spell)
{
	int b;
	int dir;
	int plev = p_ptr->lev;
	int die;


	/* spell code */
	switch (spell)
	{
	case MINDCRAFT_NEURAL_BLAST:
		/* Mindblast */
		if (!get_aim_dir(&dir)) return FALSE;

		if (randint1(100) < plev * 2)
			(void)fire_beam(GF_PSI, dir, damroll(3 + ((plev - 1) / 4), (3 + plev / 15)));
		else
			(void)fire_ball(GF_PSI, dir, damroll(3 + ((plev - 1) / 4), (3 + plev / 15)), 0);
		break;
	case MINDCRAFT_PRECOGNITION:
		if (plev > 44)
			wiz_lite();
		else if (plev > 19)
			map_area();

		if (plev < 30)
		{
			b = detect_monsters_normal();
			if (plev > 14)
				b |= detect_monsters_invis();

			if (plev > 4)
			{
				b |= detect_traps();
				b |= detect_doors();
			}
		}
		else
		{
			b = detect_all();
		}

		if ((plev > 24) && (plev < 40))
		{
			(void)set_tim_esp(p_ptr->tim_esp + plev);
		}
		
		if (!b) msg_print("You feel safe.");
		break;
	case MINDCRAFT_MINOR_DISPLACEMENT:
		/* Minor displace */
		if (plev < 40)
		{
			dimension_door(0);
		}
		else
		{
			msg_print("You open a dimensional gate. Choose a destination.");
			return dimension_door(1);
		}
		break;
	case MINDCRAFT_MAJOR_DISPLACEMENT:
		/* Major displace */
		if (plev > 29) (void)banish_monsters(plev);

		teleport_player(plev * 5);
		break;
	case MINDCRAFT_DOMINATION:
		/* Domination */
		if (plev < 30)
		{
			if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_ball(GF_DOMINATION, dir, plev, 0);
		}
		else
		{
			(void)charm_monsters(plev * 2);
		}
		break;
	case MINDCRAFT_PULVERISE:
		/* Fist of Force  ---  not 'true' TK */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_TELEKINESIS, dir, damroll(8 + ((plev - 5) / 4), 8),
		          (plev > 20 ? (plev - 20) / 8 + 1 : 0));
		break;
	case MINDCRAFT_CHARACTER_ARMOUR:
		/* Character Armour */
		(void)set_shield(p_ptr->shield + plev);
		if (plev > 14) (void)set_oppose_acid(p_ptr->oppose_acid + plev);
		if (plev > 19) (void)set_oppose_fire(p_ptr->oppose_fire + plev);
		if (plev > 24) (void)set_oppose_cold(p_ptr->oppose_cold + plev);
		if (plev > 29) (void)set_oppose_elec(p_ptr->oppose_elec + plev);
		if (plev > 34) (void)set_oppose_pois(p_ptr->oppose_pois + plev);
		break;
	case MINDCRAFT_PSYCHOMETRY:
		/* Psychometry */
		if (plev < 25)
			return psychometry();
		else
			return ident_spell();
	case MINDCRAFT_MIND_WAVE:
		/* Mindwave */
		msg_print("Mind-warping forces emanate from your brain!");
		if (plev < 25)
			(void)project(0, 2 + plev / 10, p_ptr->py, p_ptr->px,
			        (plev * 3) / 2, GF_PSI, PROJECT_KILL);
		else
			(void)mindblast_monsters(plev * ((plev - 5) / 10 + 1));
		break;
	case MINDCRAFT_ADRENALINE_CHANNELING:
		/* Adrenaline */
		(void)set_afraid(0);
		(void)set_stun(0);

		/*
		 * Only heal when Adrenalin Channeling is not active. We check
		 * that by checking if the player isn't fast and 'heroed' atm.
		 */
		if (!p_ptr->fast || !(p_ptr->hero || p_ptr->shero))
		{
			(void)hp_player(plev);
		}

		b = 10 + randint1((plev * 3) / 2);
		if (plev < 35)
			(void)set_hero(p_ptr->hero + b);
		else
			(void)set_shero(p_ptr->shero + b);

		if (!p_ptr->fast)
		{
			/* Haste */
			(void)set_fast(b);
		}
		else
		{
			(void)set_fast(p_ptr->fast + b);
		}
		break;
	case MINDCRAFT_PSYCHIC_DRAIN:
		/* Psychic Drain */
		if (!get_aim_dir(&dir)) return FALSE;

		b = damroll(plev / 2, 6);

		/* This is always a radius-0 ball now */
		if (fire_ball(GF_PSI_DRAIN, dir, b, 0))
			p_ptr->energy -= randint1(150);
		break;
	case MINDCRAFT_TELEKINETIC_WAVE:
		/* Telekinesis */
		msg_print("A wave of pure physical force radiates out from your body!");
		(void)project(0, 3 + plev / 10, p_ptr->py, p_ptr->px, 
	   plev * (plev > 39 ? 4 : 3), GF_TELEKINESIS, PROJECT_KILL | PROJECT_ITEM | PROJECT_GRID);
		break;
	case WARRIOR_TRAINING:
		die = (randint1(50) + plev);
		msg_print("You train hard.");
		if (die > 40)
			{ (void)do_res_stat(A_STR);
			}
		if (die > 55)
			{ (void)do_res_stat(A_CON);
			}
		if (die > 70)
			{ (void)restore_level();
			}
		if (die > 85)
			{ (void)do_res_stat(A_DEX);
			}
		break;
	case BERSERK_BERSERK:
		msg_print("RAAAGH!");
		if (!p_ptr->shero)
		{
			(void)hp_player(30);
		}
		(void)set_afraid(0);
		(void)set_shero(p_ptr->shero + 20 + randint1(plev));
		break;
	case CREATE_MISSILE:
		(void)do_cmd_archer();
		break;
	case TECH_REFUEL:
		phlogiston();
		break;
	case TECH_DART:
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_bolt(GF_MISSILE, dir, damroll(3 + ((plev - 1) / 5), 4));
		break;
	case TECH_SPEED_POTION:
	case WITCH_SPEED_POTION:
		make_potion(0);
		break;
	case TECH_CREATE_WEAPON:
		make_weapon(0);
		break;
	case TECH_HEAL_POTION:
	case WITCH_HEAL_POTION:
		make_potion(2);
		break;
	case TECH_BOMB:
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_ROCKET, dir, damroll(5, 5), -plev/10 +1);
		break;
	case TECH_PROTECT_POTION:
	case WITCH_PROTECT_POTION:
		make_potion(3);
		break;
	case TECH_RECHARGE:
	case WITCH_RECHARGE:
		return recharge(plev * 3);
		break;
	case TECH_CREATE_WEAPON2:
		make_weapon(1);
		break;
	case TECH_RESTORE_POTION:
	case WITCH_RESTORE_POTION:
		make_potion(1);
		break;
	case TECH_IMPROVE_WEAPON:
		if ((p_ptr->pclass == CLASS_TECH_WAR) && (plev >= 45))
		{
			brand_weapon(10);
		}
		else
		{
			return enchant_spell(randint1(4), randint1(4), 0);
		}
		break;
	case TECH_IMPROVE_ARMOUR:
		return enchant_spell(0, 0, randint1(4));
		break;
	case TECH_FISSION_BEAM:
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_beam(GF_NUKE, dir, damroll(6 + plev/4, 6));
		break;
	case TECH_NITRO9:
	{
		object_type	forge;
		object_type     *q_ptr;
		q_ptr = &forge;
		object_prep(q_ptr, lookup_kind(TV_POTION, 22));
		q_ptr->discount = 99;
		msg_print("You make an explosive.");
		(void)inven_carry(q_ptr);
		break;
	}
	case TECH_MEGA_NUKE:
		(void)fire_ball(GF_NUKE, 0, damroll(30, 30), 6);
		break;
	case PRIEST_TURN_UNDEAD:
		(void)dispel_undead(plev / 2);
		(void)turn_undead();
		break;
	case PALADIN_HEAL_TOUCH:
		(void)set_stun(0);
		(void)set_cut(0);
		(void)set_poisoned(0);
		(void)hp_player(plev*2);
		break;
	default:
		msg_format("Unknown Class power!", spell);
	}

	return TRUE;
}


/*
 * do_cmd_cast calls this function if the player's class
 * is 'mindcrafter'.
 */
void do_cmd_class(void)
{
	int             n = 0;
	int             chance;
	int             minfail;
	int             plev = p_ptr->lev;
	int             old_csp = p_ptr->csp;
	int             old_crp = p_ptr->crp;
	int             old_chp = p_ptr->chp;
	int		excess, stat;
	cptr		thing, thing2;
	class_power 	spell;
	bool            cast;


	/* not if confused */
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}
	
	/* get power */
	if (!get_class_power(&n)) return;

	spell = class_powers[n];

	if (class_powers[n].use_stat == 1)
	{
		thing = p_ptr->csp;
		thing2 = old_csp;
	}
	else if (class_powers[n].use_stat == 2)
	{
		thing = p_ptr->crp;
		thing2 = old_crp;
	}
	else
	{
		thing = p_ptr->chp;
		thing2 = old_chp;
	}

	/* Verify "dangerous" spells */
	if (spell.cost > thing)
	{
		if (class_powers[n].use_stat == 2)
		{
			msg_print("You need more resources");
			return;
		}
		/* Warning */
		msg_print("You do not have enough points to use this power.");

		/* Verify */
		if (!get_check("Attempt it anyway? ")) return;
	}

	/* Spell failure chance */
	chance = spell.fail;

	/* Reduce failure rate by "effective" level adjustment */
	chance -= 3 * (plev - spell.min_lev);

	/* Reduce failure rate by stat adjustment */
	chance -= 3 * (adj_mag_stat[p_ptr->stat_ind[class_powers[n].fail_stat]] - 1);

	/* Not enough mana to cast */
	if (spell.cost > thing)
	{
		excess = thing;
		chance += 5 * (spell.cost - excess);
	}

	/* Extract the minimum failure rate */
	minfail = adj_mag_fail[p_ptr->stat_ind[class_powers[n].fail_stat]];

	/* Minimum failure rate */
	if (chance < minfail) chance = minfail;

	/* Stunning makes spells harder */
	if (p_ptr->stun > 50) chance += 25;
	else if (p_ptr->stun) chance += 15;

	/* Always a 5 percent chance of working */
	if (chance > 95) chance = 95;

	/* Failed spell */
	if (randint0(100) < chance)
	{
		if (flush_failure) flush();
		msg_format("You failed to concentrate hard enough!");
		sound(SOUND_FAIL);

		/* Backfire */
		if ((randint1(100) < (chance / 2)) && (p_ptr->pclass == CLASS_MINDCRAFTER))
		{
			int b = randint1(100);

			if (b < 5)
			{
				msg_print("Oh, no! Your mind has gone blank!");
				(void)lose_all_info();
			}
			else if (b < 15)
			{
				msg_print("Weird visions seem to dance before your eyes...");
				(void)set_image(p_ptr->image + rand_range(5, 15));
			}
			else if (b < 45)
			{
				msg_print("Your brain is addled!");
				(void)set_confused(p_ptr->confused + randint1(8));
			}
			else if (b < 90)
			{
				(void)set_stun(p_ptr->stun + randint1(8));
			}
			else
			{
				/* Mana storm */
				msg_print("Your mind unleashes its power in an uncontrollable storm!");
				(void)project(1, 2 + plev / 10, p_ptr->py, p_ptr->px, plev * 2,
					GF_MANA, PROJECT_JUMP | PROJECT_KILL | PROJECT_GRID | PROJECT_ITEM);
				p_ptr->csp = MAX(0, p_ptr->csp - plev * MAX(1, plev / 10));
			}
		}
	}
	else
	{
		sound(SOUND_ZAP);

		/* Cast the spell */
		cast = cast_class_spell(n);

		if (!cast) return;
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Sufficient mana */
	if (spell.cost <= thing2)
	{
		/* Use some mana */
		thing -= spell.cost;

		/* Limit */
		if (thing < 0) thing = 0;
	}

	/* Over-exert the player */
	else
	{
		int excess = thing2;
		int oops = spell.cost - excess;

		/* No mana left */
		thing = 0;
//		p_ptr->csp_frac = 0;

		/* Message */
		msg_print("You faint from the effort!");

		/* Hack -- Bypass free action */
		(void)set_paralyzed(p_ptr->paralyzed + randint1(5 * oops + 1));

		/* Damage stat (possibly permanently) */
		if (randint0(100) < 50)
		{
			bool perm = (randint0(100) < 25);

			/* Message */
			msg_print("You have damaged yourself!");
			
			stat = class_powers[n].fail_stat;

			/* Reduce constitution */
			(void)dec_stat(stat, rand_range(15, 25), perm);
		}
	}
	
	/* Use the points */
	stat = thing;
	if (class_powers[n].use_stat == 1)
	{
		p_ptr->csp = stat;
	}
	else if (class_powers[n].use_stat == 2)
	{
		p_ptr->crp = stat;
	}
	else
	{
		p_ptr->chp = stat;
	}

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);
	p_ptr->redraw |= (PR_RESOURCE);
	p_ptr->redraw |= (PR_HP);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);
	p_ptr->window |= (PW_SPELL);
}

