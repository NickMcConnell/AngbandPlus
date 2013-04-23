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

int *index;

int mindcrafter[] = { 	MINDCRAFT_NEURAL_BLAST, 
			MINDCRAFT_PRECOGNITION, 
			MINDCRAFT_MINOR_DISPLACEMENT, 
			MINDCRAFT_MINOR_TELEKINESIS,
			MINDCRAFT_MAJOR_DISPLACEMENT,
			MINDCRAFT_DOMINATION,
			MINDCRAFT_PULVERISE,
			MINDCRAFT_CHARACTER_ARMOUR,
			MINDCRAFT_PSYCHOMETRY,
			MINDCRAFT_MIND_WAVE,
			MINDCRAFT_CHARM,
			MINDCRAFT_ADRENALINE_CHANNELING,
			MINDCRAFT_PSYCHIC_DRAIN,
			MINDCRAFT_TELEKINETIC_WAVE,
			CLASS_ABILITY_MAX
			};

int warrior[] = { 	WARRIOR_TRAINING,
			SPIN_ATTACK,
			CLASS_ABILITY_MAX
			};
		
int berserker[] = { 	WARRIOR_TRAINING,
			BERSERK_BERSERK,
			SPIN_ATTACK,
			CLASS_ABILITY_MAX
			};

int ranger[] = { 	WARRIOR_TRAINING,
			CREATE_MISSILE,
			CLASS_ABILITY_MAX
			};
		
int paladin[] = { 	WARRIOR_TRAINING,
			PALADIN_HEAL_TOUCH,
			CLASS_ABILITY_MAX
			};

int darkknight[] = { 	WARRIOR_TRAINING,
			DEATH_BLOOD_FIRE,
			CLASS_ABILITY_MAX
			};
			
int chaoswar[] = { 	WARRIOR_TRAINING,
			CLASS_ABILITY_MAX
			};
		
int priest[] = { 	PRIEST_TURN_UNDEAD,
			CLASS_ABILITY_MAX
			};
			
int necromancer[] = {	DEATH_BLOOD_FIRE,
			CLASS_ABILITY_MAX
			};
			
int shaman[] = {	CLASS_ABILITY_MAX
			};
		
int archer[] = { 	WARRIOR_TRAINING,
			CREATE_MISSILE,
			CLASS_ABILITY_MAX
			};

int witch[] = { 	TECH_SPEED_POTION,
			TECH_HEAL_POTION,
			TECH_PROTECT_POTION,
			TECH_RECHARGE,
			TECH_RESTORE_POTION,
			CLASS_ABILITY_MAX
			};

int tech[] = { 		TECH_REFUEL,
			TECH_DART,
			TECH_SPEED_POTION,
			TECH_CREATE_WEAPON,
			TECH_HEAL_POTION,
			TECH_BOMB,
			TECH_PROTECT_POTION,
			TECH_RECHARGE,
			TECH_CREATE_WEAPON2,
			TECH_RESTORE_POTION,
			TECH_IMPROVE_WEAPON,
			TECH_IMPROVE_ARMOUR,
			TECH_FISSION_BEAM,
			TECH_NITRO9,
			TECH_MEGA_NUKE,
			CLASS_ABILITY_MAX
			};

int techwar[] = { 	TECH_REFUEL,
			WARRIOR_TRAINING,
			TECH_DART,
			TECH_SPEED_POTION,
			TECH_CREATE_WEAPON,
			TECH_HEAL_POTION,
			TECH_BOMB,
			TECH_PROTECT_POTION,
			TECH_RECHARGE,
			TECH_CREATE_WEAPON2,
			TECH_RESTORE_POTION,
			SPIN_ATTACK,
			TECH_IMPROVE_WEAPON,
			TECH_IMPROVE_ARMOUR,
			TECH_FISSION_BEAM,
			TECH_NITRO9,
			TECH_MEGA_NUKE,
			CLASS_ABILITY_MAX
			};

int techmage[] = { 	TECH_REFUEL,
			TECH_DART,
			MAGE_FOCUS_MANA,
			TECH_SPEED_POTION,
			TECH_CREATE_WEAPON,
			TECH_HEAL_POTION,
			TECH_BOMB,
			TECH_PROTECT_POTION,
			TECH_RECHARGE,
			TECH_CREATE_WEAPON2,
			TECH_RESTORE_POTION,
			TECH_IMPROVE_WEAPON,
			TECH_IMPROVE_ARMOUR,
			TECH_FISSION_BEAM,
			MAGE_SENSE,
			TECH_NITRO9,
			TECH_MEGA_NUKE,
			CLASS_ABILITY_MAX
			};
			
int techthief[] = { 	TECH_REFUEL,
			TECH_DART,
			TECH_SPEED_POTION,
			TECH_CREATE_WEAPON,
			TECH_HEAL_POTION,
			TECH_BOMB,
			TECH_PROTECT_POTION,
			ROGUE_SENSE_INVIS,
			TECH_RECHARGE,
			TECH_CREATE_WEAPON2,
			TECH_RESTORE_POTION,
			TECH_IMPROVE_WEAPON,
			TECH_IMPROVE_ARMOUR,
			ROGUE_BE_INVIS,
			TECH_FISSION_BEAM,
			TECH_NITRO9,
			TECH_MEGA_NUKE,
			CLASS_ABILITY_MAX
			};
			
int mage[] = {		MAGE_FOCUS_MANA,
			MAGE_SENSE,
			CLASS_ABILITY_MAX
			};
			
int warmage[] = {	WARRIOR_TRAINING,
			MAGE_FOCUS_MANA,
			SPIN_ATTACK,
			MAGE_SENSE,
			CLASS_ABILITY_MAX
			};
			
int rogue[] = {		ROGUE_SENSE_INVIS,
			ROGUE_BE_INVIS,
			CLASS_ABILITY_MAX
			};
			
int roguemage[] = {	MAGE_FOCUS_MANA,
			ROGUE_SENSE_INVIS,
			ROGUE_BE_INVIS,
			MAGE_SENSE,
			CLASS_ABILITY_MAX
			};

int ninja[] = {		WARRIOR_TRAINING,
			ROGUE_SENSE_INVIS,
			ROGUE_BE_INVIS,
			CLASS_ABILITY_MAX
			};
			
int monk[] = {		WARRIOR_TRAINING,
			ROGUE_SENSE_INVIS,
			CLASS_ABILITY_MAX
			};
			
int blank[] = { 	CLASS_ABILITY_MAX
			};
		
		
class_power class_powers[CLASS_ABILITY_MAX] =
{
	/* Level, Stat to use, cost, fail stat, %fail, name */
	{ 1,  	  	SP, 	 1, 	A_WIS, 	15, 	"Neural Blast" },          /* ~MM */
	{ 2,  	  	SP, 	 1, 	A_WIS, 	20, 	"Precognition" },          /* Det. monsters/traps */
	{ 3,  	 	SP, 	 2, 	A_WIS, 	25, 	"Minor Displacement" },    /* Blink */
	{ 5,		SP,	 2,	A_WIS,	30,	"Minor Telekinesis" },
	{ 7,  	  	SP, 	 6, 	A_WIS,	35, 	"Major Displacement" },    /* Tele. Self / All */
	
	{ 9,  	  	SP, 	 7, 	A_WIS, 	50, 	"Domination" },
	{ 11, 	  	SP, 	 7, 	A_WIS, 	30, 	"Pulverise" },             /* Telekinetic "bolt" */
	{ 13, 	 	SP, 	12, 	A_WIS, 	50, 	"Character Armour" },      /* Psychic/physical defenses */
	{ 15, 	  	SP, 	12, 	A_WIS, 	60, 	"Psychometry" },
	{ 18, 	  	SP, 	10, 	A_WIS, 	45, 	"Mind Wave" },             /* Ball -> LOS */
	
	{ 20,		SP,	20,	A_WIS,	55,	"Charm" },
	{ 23, 	  	SP, 	15, 	A_WIS, 	50, 	"Adrenaline Channeling" },
	{ 25, 	  	SP, 	10, 	A_WIS, 	40, 	"Psychic Drain" },         /* Convert enemy HP to mana */
	{ 28, 	  	SP, 	20, 	A_WIS, 	45, 	"Telekinetic Wave" },      /* Ball -> LOS */	
	{ 1,  	  	FP,    200, 	A_CON, 	25, 	"Training" },      	   /* Restore Physical Stats */
	
	{ 8,  	  	FP,    100, 	A_WIS, 	 9, 	"Berserk" },      	   /* Berserk */
	{ 1,  	  	RP, 	 6, 	A_INT, 	20, 	"Make Ammunition" },       /* Shots, Arrows or Bolts */
	{ 1,   	 	RP, 	 2, 	A_INT, 	10, 	"Refuel" },       	   /* Torches and Lamps */
	{ 2,   	 	RP, 	 3, 	A_INT, 	14, 	"Dart" },       	   /* MM */
	{ 5,   	 	RP, 	 5, 	A_INT, 	24, 	"Speed Potion" },
	
	{ 6,   	 	RP, 	10, 	A_INT, 	50, 	"Create Weapon" },
	{ 8,   	 	RP, 	10, 	A_INT, 	30, 	"Healing Potion" },
	{ 10,  	 	RP, 	 7, 	A_INT, 	20, 	"Blast Wave" },       	   /* Wave */      	 
	{ 15,    	RP, 	15, 	A_INT, 	40, 	"Protective Potion" },
	{ 18,		RP,	20,	A_INT,	50,	"Recharge Magic" },
	
	{ 20,  	 	RP, 	25, 	A_INT, 	75, 	"Create Weapon II" },
	{ 25,  	 	RP, 	30, 	A_INT, 	60, 	"Restoring Potion" },
	{ 30,		RP,	40,	A_INT,	80,	"Improve weapon" },
	{ 30,		RP,	45,	A_INT,	75,	"Improve armour" },
	{ 35,		RP,	50,	A_INT,	65,	"Fission beam" },

	{ 40,		RP,	60,	A_INT,	70,	"Explosives" },
	{ 48,		RP,    100,	A_INT,	90,	"Nuke" },		   /* Ball */
	{ 5,  	 	SP, 	10, 	A_WIS, 	10, 	"Turn Undead" },
	{ 7,  	 	SP, 	20, 	A_WIS, 	10, 	"Healing Touch" },
	{ 3,		FP,	25,	A_INT,	33,	"Gather Mana" },

	{ 27,		FP,	150,	A_DEX,  40,	"Spin Attack" },
	{ 36,		FP,	25,	A_INT,	76,	"Mage Sense" },
	{ 17,		FP,	22,	A_WIS,	27,	"Sense Invisible" },
	{ 32,		SP,	20,	A_WIS,	56,	"Invisibility" },
	{ 22,		HP,	50,	A_INT,	66,	"Blood Fire" }
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
		case MINDCRAFT_MINOR_TELEKINESIS:
			sprintf(p, " max wgt %d", plev * 15 / 10); 
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
		case MINDCRAFT_CHARM:
			strcpy (p, " dur 50-100");
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
			break;
		case MAGE_FOCUS_MANA:
			sprintf(p, " +1d%d SP", plev);
			break;
		case DEATH_BLOOD_FIRE:
			sprintf(p, " dam %d", 100 + 3 * plev);
			break;
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

	switch (p_ptr->pclass)
	{
		case CLASS_MINDCRAFTER:
		{
			index = &mindcrafter[0];
			break;
		}
		case CLASS_WARRIOR:
		{
			index = &warrior[0];
			break;
		}
		case CLASS_BERSERK:
		{
			index = &berserker[0];
			break;
		}
		case CLASS_RANGER:
		{
			index = &ranger[0];
			break;
		}
		case CLASS_ARCHER:
		{
			index = &archer[0];
			break;
		}
		case CLASS_PALADIN:
		{
			index = &paladin[0];
			break;
		}
		case CLASS_WITCH:
		{
			index = &witch[0];
			break;
		}
		case CLASS_TECH_FULL:
		case CLASS_TECH_CLERIC:
		{
			index = &tech[0];
			break;
		}
		case CLASS_TECH_WAR:
		{
			index = &techwar[0];
			break;
		}
		case CLASS_TECH_MAGE:
		{
			index = &techmage[0];
			break;
		}
		case CLASS_TECH_THIEF:
		{
			index = &techthief[0];
			break;
		}
		case CLASS_MAGE_FIRE:
		case CLASS_MAGE_WATER:
		case CLASS_MAGE_EARTH:
		case CLASS_MAGE_AIR:
		case CLASS_WIZARD:
		case CLASS_HIGH_MAGE:
		{
			index = &mage[0];
			break;
		}
		case CLASS_WARRIOR_MAGE:
		{
			index = &warmage[0];
			break;
		}
		case CLASS_ROGUE:
		case CLASS_ASSASSIN:
		{
			index = &rogue[0];
			break;
		}
		case CLASS_NINJA:
		{
			index = &ninja[0];
			break;
		}
		case CLASS_THIEF_MAGE:
		{
			index = &roguemage[0];
			break;
		}
		case CLASS_MONK:
		{
			index = &monk[0];
			break;
		}
		case CLASS_DARK_KNIGHT:
		{
			index = &darkknight[0];
			break;
		}
		case CLASS_CHAOS_WARRIOR:
		{
			index = &chaoswar[0];
			break;
		}
		case CLASS_NECROMANCER:
		{
			index = &necromancer[0];
			break;
		}
		case CLASS_SHAMAN:
		{
			index = &shaman[0];
			break;
		}

		default:
		{
			index = &blank[0];
			break;
		}
	}

	/* Get the spell, if available */
	if (repeat_pull(sn))
	{
		/* Verify the spell */
		if (class_powers[index[*sn]].min_lev <= plev)
		{
			/* Success */
			return (TRUE);
		}
	}

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	i = 0;

	do
	{
		if (class_powers[index[i]].min_lev <= plev)
		{
			num++;
		}
	} while (index[i++] != CLASS_ABILITY_MAX);
	
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

				i = 0;
				/* Dump the spells */
				do
				{
					if (class_powers[index[i]].use_stat == SP)
					{
						thing = p_ptr->csp;
					}
					else if (class_powers[index[i]].use_stat == RP)
					{
						thing = p_ptr->crp;
					}
					else if (class_powers[index[i]].use_stat == FP)
					{
						thing = p_ptr->fatigue;
					}
					else
					{
						thing = p_ptr->chp;
					}
					
					/* Access the spell */
					spell = class_powers[index[i]];
					if (spell.min_lev > plev) break;

					chance = spell.fail;

					/* Reduce failure rate by "effective" level adjustment */
					chance -= 3 * (plev - spell.min_lev);

					/* Reduce failure rate by stat adjustment */
					chance -= 3 * (adj_mag_stat[change_form(class_powers[index[i]].fail_stat)] - 1);

					/* Not enough mana to cast */
					if ((spell.cost > thing) && (class_powers[index[i]].use_stat != FP))
					{
						excess = thing;
						chance += 5 * (spell.cost - excess);
					}

					/* Extract the minimum failure rate */
					minfail = adj_mag_fail[change_form(class_powers[index[i]].fail_stat)];

					/* Minimum failure rate */
					if (chance < minfail) chance = minfail;

					/* Stunning makes spells harder */
					if (p_ptr->stun > 50) chance += 25;
					else if (p_ptr->stun) chance += 15;

					/* Always a 5 percent chance of working */
					if (chance > 95) chance = 95;

					/* Get info */
					class_ability_info(comment, index[i]);

					/* Dump the spell --(-- */
					sprintf(psi_desc, "  %c) %-30s%2d %4d %3d%%%s",
					        I2A(i), spell.name,
					        spell.min_lev, spell.cost, chance, comment);
					prt(psi_desc, y + i, x);

				} while (index[i++] != CLASS_ABILITY_MAX);

				/* Clear the bottom line */
				prt("", y + i, x);
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
		
		/* Totally Illegal */
		if ((i < 0) || (i >= num))
		{
			bell();
			continue;
		}
		
		/* Save the spell index */
		spell = class_powers[index[i]];

		/* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Prompt */
			(void)strnfmt(tmp_val, 78, "Use %s? ", spell.name);

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
	(*sn) = index[i];

	repeat_push(index[i]);

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
	int dir = 0;
	int plev = p_ptr->lev;
	int die;
	int mod = 0;

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
	case MINDCRAFT_MINOR_TELEKINESIS:
		fetch(dir, plev * 15, FALSE);
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
	case MINDCRAFT_CHARM:
		/*  Spot the useless bit ;)*/
		if (!p_ptr->boost_chr)
		{
			(void)set_boost_chr(p_ptr->boost_chr + rand_range(50, 100));
		}
		else
		{
			(void)set_boost_chr(p_ptr->boost_chr + randint1(plev));
		}
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
		/* Restores physical stats */
		if (p_ptr->pclass == CLASS_WARRIOR)
			mod = 1;
		die = (randint1(30 + 20 * mod) + plev);
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
		make_potion(0);
		break;
	case TECH_CREATE_WEAPON:
		make_weapon(0);
		break;
	case TECH_HEAL_POTION:
		make_potion(2);
		break;
	case TECH_BOMB:
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_ROCKET, dir, damroll(5, 5), -plev/10 +1);
		break;
	case TECH_PROTECT_POTION:
		make_potion(3);
		break;
	case TECH_RECHARGE:
		return recharge(plev * 3);
		break;
	case TECH_CREATE_WEAPON2:
		make_weapon(1);
		break;
	case TECH_RESTORE_POTION:
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
		object_prep(q_ptr, lookup_kind(TV_POTION, SV_POTION_DETONATIONS));
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
	case MAGE_FOCUS_MANA:
		p_ptr->csp += randint1(plev);
		p_ptr->csp_frac = 0;
		msg_print(" You focus nearby mana. ");
		p_ptr->redraw |= (PR_MANA);
		p_ptr->window |= (PW_PLAYER);
		p_ptr->window |= (PW_SPELL);
		
		break;
	case SPIN_ATTACK:
	{
		int y = 0, x = 0;
		int px = p_ptr->px;
		int py = p_ptr->py;
		cave_type *c_ptr;
		monster_type *m_ptr;
		
		for (dir != ( 0 || 5 ); dir <= 9; dir++)
		{
			y = py + ddy[dir];
			x = px + ddx[dir];
	
			/* paranoia */
			if (!in_bounds2(y, x)) continue;
			c_ptr = area(y, x);
		
			/* Get the monster */
			m_ptr = &m_list[c_ptr->m_idx];
		
			/* Hack -- attack monsters */
			if (c_ptr->m_idx && (m_ptr->ml || cave_floor_grid(c_ptr)))
				py_attack(y, x);
		}
	}
		break;
	case MAGE_SENSE:
		detect_objects_magic();
		break;
	case ROGUE_SENSE_INVIS:
		if (p_ptr->pclass == CLASS_ROGUE)
			mod = 1;
		if (randint1(plev - 20 * mod) >> plev)
			(void)set_image(p_ptr->image + 2);
		(void)detect_monsters_invis();
		break;
	case ROGUE_BE_INVIS:
		(void)set_invisible(p_ptr->tim_nonvis + rand_range(50, 100));
		break;
	case DEATH_BLOOD_FIRE:
		if (!get_aim_dir(&dir)) return FALSE;
		(void)fire_ball(GF_MISSILE, dir, 100 + 3 * plev, 3);
		break;
	default:
		msg_format("Unknown Class Power!: %i.", spell);
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
	int             old_fatigue = p_ptr->fatigue;
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

	if (class_powers[n].use_stat == SP)
	{
		thing = p_ptr->csp;
		thing2 = old_csp;
	}
	else if (class_powers[n].use_stat == RP)
	{
		thing = p_ptr->crp;
		thing2 = old_crp;
	}
	else if (class_powers[n].use_stat == FP)
	{
		thing = p_ptr->fatigue;
		thing2 = old_fatigue;
	}
	else
	{
		thing = p_ptr->chp;
		thing2 = old_chp;
	}

	/* Verify "dangerous" spells */
	if ((spell.cost > thing) && (class_powers[n].use_stat != FP))
	{
		if (class_powers[n].use_stat == RP)
		{
			msg_print("You need more resources.");
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
	chance -= 3 * (adj_mag_stat[change_form(class_powers[n].fail_stat)] - 1);

	/* Not enough mana to cast */
	if ((spell.cost > thing) && (class_powers[n].use_stat != FP))
	{
		excess = thing;
		chance += 5 * (spell.cost - excess);
	}

	/* Extract the minimum failure rate */
	minfail = adj_mag_fail[change_form(class_powers[n].fail_stat)];

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
	if ((spell.cost <= thing2) && (class_powers[n].use_stat != FP))
	{
		/* Use some mana */
		thing -= spell.cost;

		/* Limit */
		if (thing < 0) thing = 0;
	}
	else if (class_powers[n].use_stat == FP)
	{
		thing += spell.cost;
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
	if (class_powers[n].use_stat == SP)
	{
		p_ptr->csp = stat;
	}
	else if (class_powers[n].use_stat == RP)
	{
		p_ptr->crp = stat;
	}
	else if (class_powers[n].use_stat == FP)
	{
		p_ptr->fatigue = stat;
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