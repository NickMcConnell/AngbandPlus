/* File: mutation.c */

/* Purpose: Mutation effects (and racial powers) */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"



/*
 * Hack - to allow bias for collecting
 * certain mutations dependent upon your race
 */
#define M1_HYPN_GAZE	 2
#define M1_SHRIEK		13
#define M1_POLYMORPH	17
#define M2_HORNS		39
#define M2_TENTACLES	48


/* Does the player have the given mutation? */
bool player_has_mut(int mutation)
{
	if (mutation < MUT_PER_SET)
	{
		return (p_ptr->muta1 & mutations[mutation].which ? TRUE : FALSE);
	}
	else if (mutation < MUT_PER_SET * 2)
	{
		return (p_ptr->muta2 & mutations[mutation].which ? TRUE : FALSE);
	}
	else
	{
		return (p_ptr->muta3 & mutations[mutation].which ? TRUE : FALSE);
	}
} 
 

/*
 * Select a random mutation.
 *
 * Note that if choose_mut is non-zero (specified
 * via p_ptr->command_arg in debugging mode) we
 * select the specified mutation.
 *
 * This function is really silly.
 * We should use the (depth) rarity method used
 * for everything else to select the mutation
 */
static bool select_mutation(int choose_mut, bool gain, int *mutation)
{
	u32b flag;

	int attempts_left;
	
	int num = -1;


	/* Sanity check */
	if (choose_mut < 0 || choose_mut > 193) choose_mut = 0;

	attempts_left = (choose_mut ? 1 : (gain ? 20 : 2000));

	while (attempts_left--)
	{
		switch (choose_mut ? choose_mut : randint1(193))
		{
		case 1: case 2: case 3: case 4:
			/* Spit acid */
			num = 0;
			break;
		case 5: case 6: case 7:
			/* Breathe fire */
			num = 1;
			break;
		case 8: case 9:
			/* Hypnotic gaze */
			num = 2;
			break;
		case 10: case 11:
			/* Telekinesis */
			num = 3;
			break;
		case 12: case 13: case 14:
			/* Vteleport */
			num = 4;
			break;
		case 15: case 16:
			/* Mind blast */
			num = 5;
			break;
		case 17: case 18:
			/* Radiation */
			num = 6;
			break;
		case 19: case 20:
			/* Vampirism */
			num = 7;
			break;
		case 21: case 22: case 23:
			/* Smell metal */
			num = 8;
			break;
		case 24: case 25: case 26: case 27:
			/* Smell monsters */
			num = 9;
			break;
		case 28: case 29: case 30:
			/* Blink */
			num = 10;
			break;
		case 31: case 32:
			/* Eat rock */
			num = 11;
			break;
		case 33: case 34:
			/* Swap position */
			num = 12;
			break;
		case 35: case 36: case 37:
			/* Shriek */
			num = 13;
			break;
		case 38: case 39: case 40:
			/* Illuminate */
			num = 14;
			break;
		case 41: case 42:
			/* Detect curse */
			num = 15;
			break;
		case 43: case 44: case 45:
			/* Berserk */
			num = 16;
			break;
		case 46:
			/* Polymorph */
			num = 17;
			break;
		case 47: case 48:
			/* Midas */
			num = 18;
			break;
		case 49:
			/* Mold */
			num = 19;
			break;
		case 50: case 51: case 52:
			/* Resist Elements */
			num = 20;
			break;
		case 53: case 54: case 55:
			/* Earthquake */
			num = 21;
			break;
		case 56:
			/* Eat magic */
			num = 22;
			break;
		case 57: case 58:
			/* Weigh magic */
			num = 23;
			break;
		case 59:
			/* Sterilize */
			num = 24;
			break;
		case 60: case 61:
			/* Panic hit */
			num = 25;
			break;
		case 62: case 63: case 64:
			/* Dazzle */
			num = 26;
			break;
		case 65: case 66: case 67:
			/* Laser eye */
			num = 27;
			break;
		case 68: case 69:
			/* Recall */
			num = 28;
			break;
		case 70:
			/* Banish */
			num = 29;
			break;
		case 71: case 72:
			/* Cold touch */
			num = 30;
			break;
		case 73: case 74:
			/* Throw */
			num = 31;
			break;
		case 75:
			/* Berserk */
			num = 32;
			break;
		case 76:
			/* Fear */
			num = 33;
			break;
		case 77:
			/* Teleport */
			num = 34;
			break;
		case 78:
			/* Ethanol */
			num = 35;
			break;
		case 79:
			/* Hallucinate */
			num = 36;
			break;
		case 80:
			/* Flatulent */
			num = 37;
			break;
		case 81: case 82:
			/* Scorpion */
			num = 38; 
			break;
		case 83: case 84:
			/* Horns */
			num = 39;
			break;
		case 85: case 86:
			/* Beak */
			num = 40;
			break;
		case 87: case 88:
			/* Demons */
			num = 41;
			break;
		case 89:
			/* Mana */
			num = 42;
			break;
		case 90: case 91:
			/* Speed flux */
			num = 43;
			break;
		case 92: case 93:
			/* Banish */
			num = 44;
			break;
		case 94:
			/* Eat lite */
			num = 45;
			break;
		case 95: case 96:
			/* Trunk */
			num = 46;
			break;
		case 97:
			/* Animal */
			num = 47;
			break;
		case 98:
			/* Tentacles */
			num = 48;
			break;
		case 99:
			/* Raw Chaos */
			num = 49;
			break;
		case 100: case 101: case 102:
			/* Normal */
			num = 50;
			break;
		case 103:
			/* Wraith */
			num = 51;
			break;
		case 104:
			/* Poly wound */
			num = 52;
			break;
		case 105:
			/* Disease */
			num = 53;
			break;
		case 106:
			/* Dragon */
			num = 54;
			break;
		case 107: case 108:
			/* Esp */
			num = 55;
			break;
		case 109:
			/* Sick */
			num = 56;
			break;
		case 110: case 111:
			/* Chaos warriors already have a chaos deity */
			if (p_ptr->pclass != CLASS_CHAOS_WARRIOR)
			{
				/* Patron */
				num = 57;
			}
			break;
		case 112:
			/* Shadow walk */
			num = 58;
			break;
		case 113: case 114:
			/* Warn */
			num = 59;
			break;
		case 115:
			/* Invuln */
			num = 60;
			break;
		case 116: case 117:
			/* Healing */
			num = 61;
			break;
		case 118:
			/* HP2SP */
			num = 62;
			break;
		case 119:
			/* Disarm */
			num = 63;
			break;
		case 120: case 121: case 122:
			/* Strong */
			num = 64;
			break;
		case 123: case 124: case 125:
			/* Weak */
			num = 65;
			break;
		case 126: case 127: case 128:
			/* Smart */
			num = 66;
			break;
		case 129: case 130: case 131:
			/* Dumb */
			num = 67;
			break;
		case 132: case 133:
			/* Con */
			num = 68;
			break;
		case 134: case 135:
			/* Fat */
			num = 69;
			break;
		case 136: case 137:
			/* Frail */
			num = 70;
			break;
		case 138: case 139: case 140:
			/* Rot */
			num = 71;
			break;
		case 141: case 142:
			/* Squeak */
			num = 72;
			break;
		case 143: case 144:
			/* Blank face */
			num = 73;
			break;
		case 145:
			/* Illusion face */
			num = 74;
			break;
		case 146: case 147: case 148:
			/* eyes */
			num = 75;
			break;
		case 149: case 150:
			/* Res magic */
			num = 76;
			break;
		case 151: case 152: case 153:
			/* Noise */
			num = 77;
			break;
		case 154: case 155: case 156:
			/* Infra */
			num = 78;
			break;
		case 157: case 158:
			/* Legs fast */
			num = 79;
			break;
		case 159: case 160:
			/* Legs slow */
			num = 80;
			break;
		case 161: case 162:
			/* Aura elec */
			num = 81;
			break;
		case 163: case 164:
			/* Aura fire */
			num = 82;
			break;
		case 165: case 166: case 167:
			/* Warts */
			num = 83;
			break;
		case 168: case 169: case 170:
			/* Scales */
			num = 84;
			break;
		case 171: case 172:
			/* Iron */
			num = 85;
			break;
		case 173: case 174:
			/* Wings */
			num = 86;
			break;
		case 175: case 176: case 177:
			/* Res fear */
			num = 87;
			break;
		case 178: case 179:
			/* Regen */
			num = 88;
			break;
		case 180: case 181:
			/* ESP */
			num = 89;
			break;
		case 182: case 183: case 184:
			/* Limber */
			num = 90;
			break;
		case 185: case 186: case 187:
			/* Arthritis */
			num = 91;
			break;
		case 188:
			/* Bad luck */
			num = 92;
			break;
		case 189:
			/* Bad element */
			num = 93;
			break;
		case 190: case 191: case 192:
			/* Stealth */
			num = 94;
			break;
		case 193:
			/* Good luck */
			num = 95;
			break;
		default:
			num = -1;
		}

		/* Have we picked anything? */
		if (num >= 0)
		{
			/* Can we gain / lose the mutation as desired? */
			if (num < MUT_PER_SET)
			{
				flag = p_ptr->muta1;
			}
			else if (num < MUT_PER_SET * 2)
			{
				flag = p_ptr->muta2;
			}
			else
			{
				flag = p_ptr->muta3;
			}

			/* Save the mutation we are using */
			*mutation = num;
			
			return ((flag & mutations[num].which ? FALSE : TRUE) == gain);
		}
	}

	return FALSE;
}


/*
 * Gain a mutation
 */
bool gain_mutation(int choose_mut)
{
	const mutation_type *mut_ptr;
	
	u32b muta_which;

	int num;
	
	/* Choose a mutation */
	if (!select_mutation(choose_mut, TRUE, &num))
	{
		msg_print("You feel normal.");
		return FALSE;
	}
	else
	{
		chg_virtue(V_CHANCE, 1);

		if (p_ptr->prace == RACE_VAMPIRE &&
		  !(p_ptr->muta1 & MUT1_HYPN_GAZE) &&
		   (randint1(10) < 7))
		{
			num = M1_HYPN_GAZE;
		}

		else if (p_ptr->prace == RACE_IMP &&
			!(p_ptr->muta2 & MUT2_HORNS) &&
			(randint1(10) < 7))
		{
			num = M2_HORNS;
		}

		else if (p_ptr->prace == RACE_YEEK &&
			!(p_ptr->muta1 & MUT1_SHRIEK) &&
			(randint1(10) < 7))
		{
			num = M1_SHRIEK;
		}

		else if (p_ptr->prace == RACE_BEASTMAN &&
			!(p_ptr->muta1 & MUT1_POLYMORPH) &&
			(randint1(10) < 2))
		{
			num = M1_POLYMORPH;
		}

		else if (p_ptr->prace == RACE_MIND_FLAYER &&
			!(p_ptr->muta2 & MUT2_TENTACLES) &&
			(randint1(10) < 7))
		{
			num = M2_TENTACLES;
		}
		
		/* Point to the mutation */
		mut_ptr = &mutations[num];

		muta_which = mut_ptr->which;

		msg_print("You mutate!");
		msg_print(mut_ptr->gain_text);
		
		/* Gain the mutation */
		if (num < MUT_PER_SET)
		{
			p_ptr->muta1 |= muta_which;
		}
		else if (num < MUT_PER_SET * 2)
		{
			p_ptr->muta2 |= muta_which;
		}
		else
		{
			p_ptr->muta3 |= muta_which;
		}

		/* Some mutations cancel others */
		if (num >= MUT_PER_SET * 2)
		{
			if (muta_which == MUT3_PUNY)
			{
				if (p_ptr->muta3 & MUT3_HYPER_STR)
				{
					msg_print("You no longer feel super-strong!");
					p_ptr->muta3 &= ~(MUT3_HYPER_STR);
				}
			}
			else if (muta_which == MUT3_HYPER_STR)
			{
				if (p_ptr->muta3 & MUT3_PUNY)
				{
					msg_print("You no longer feel puny!");
					p_ptr->muta3 &= ~(MUT3_PUNY);
				}
			}
			else if (muta_which == MUT3_MORONIC)
			{
				if (p_ptr->muta3 & MUT3_HYPER_INT)
				{
					msg_print("Your brain is no longer a living computer.");
					p_ptr->muta3 &= ~(MUT3_HYPER_INT);
				}
			}
			else if (muta_which == MUT3_HYPER_INT)
			{
				if (p_ptr->muta3 & MUT3_MORONIC)
				{
					msg_print("You are no longer moronic.");
					p_ptr->muta3 &= ~(MUT3_MORONIC);
				}
			}
			else if (muta_which == MUT3_IRON_SKIN)
			{
				if (p_ptr->muta3 & MUT3_SCALES)
				{
					msg_print("You lose your scales.");
					p_ptr->muta3 &= ~(MUT3_SCALES);
				}
				if (p_ptr->muta3 & MUT3_FLESH_ROT)
				{
					msg_print("Your flesh rots no longer.");
					p_ptr->muta3 &= ~(MUT3_FLESH_ROT);
				}
				if (p_ptr->muta3 & MUT3_WART_SKIN)
				{
					msg_print("You lose your warts.");
					p_ptr->muta3 &= ~(MUT3_WART_SKIN);
				}
			}
			else if ((muta_which == MUT3_WART_SKIN) ||
			         (muta_which == MUT3_SCALES) ||
			         (muta_which == MUT3_FLESH_ROT))
			{
				if (p_ptr->muta3 & MUT3_IRON_SKIN)
				{
					msg_print("Your skin is no longer made of steel.");
					p_ptr->muta3 &= ~(MUT3_IRON_SKIN);
				}
			}
			else if (muta_which == MUT3_FEARLESS)
			{
				if (p_ptr->muta2 & MUT2_COWARDICE)
				{
					msg_print("You are no longer cowardly.");
					p_ptr->muta2 &= ~(MUT2_COWARDICE);
				}
			}
			else if (muta_which == MUT3_FLESH_ROT)
			{
				if (p_ptr->muta3 & MUT3_REGEN)
				{
					msg_print("You stop regenerating.");
					p_ptr->muta3 &= ~(MUT3_REGEN);
				}
			}
			else if (muta_which == MUT3_REGEN)
			{
				if (p_ptr->muta3 & MUT3_FLESH_ROT)
				{
					msg_print("Your flesh stops rotting.");
					p_ptr->muta3 &= ~(MUT3_FLESH_ROT);
				}
			}
			else if (muta_which == MUT3_LIMBER)
			{
				if (p_ptr->muta3 & MUT3_ARTHRITIS)
				{
					msg_print("Your joints stop hurting.");
					p_ptr->muta3 &= ~(MUT3_ARTHRITIS);
				}
			}
			else if (muta_which == MUT3_ARTHRITIS)
			{
				if (p_ptr->muta3 & MUT3_LIMBER)
				{
					msg_print("You no longer feel limber.");
					p_ptr->muta3 &= ~(MUT3_LIMBER);
				}
			}
		}
		else if (num >= MUT_PER_SET)
		{
			if (muta_which == MUT2_COWARDICE)
			{
				if (p_ptr->muta3 & MUT3_FEARLESS)
				{
					msg_print("You no longer feel fearless.");
					p_ptr->muta3 &= ~(MUT3_FEARLESS);
				}
			}
			if (muta_which == MUT2_BEAK)
			{
				if (p_ptr->muta2 & MUT2_TRUNK)
				{
					msg_print("Your nose is no longer elephantine.");
					p_ptr->muta2 &= ~(MUT2_TRUNK);
				}
			}
			if (muta_which == MUT2_TRUNK)
			{
				if (p_ptr->muta2 & MUT2_BEAK)
				{
					msg_print("You no longer have a hard beak.");
					p_ptr->muta2 &= ~(MUT2_BEAK);
				}
			}
		}

		mutant_regenerate_mod = calc_mutant_regenerate_mod();
		p_ptr->update |= PU_BONUS;
		handle_stuff();
		return TRUE;
	}
}

/*
 * Lose a mutation
 */
bool lose_mutation(int choose_mut)
{
	int num;
	
	u32b muta_which;
	const mutation_type *mut_ptr;

	
	if (!select_mutation(choose_mut, FALSE, &num))
	{
		return FALSE;
	}
	else
	{
		/* Point to the mutation */
		mut_ptr = &mutations[num];
		
		muta_which = mut_ptr->which;
		
		msg_print(mut_ptr->lose_text);
		
		if (num < MUT_PER_SET)
		{
			p_ptr->muta1 &= ~(muta_which);
		}
		else if (num < MUT_PER_SET * 2)
		{
			p_ptr->muta2 &= ~(muta_which);
		}
		else
		{
			p_ptr->muta3 &= ~(muta_which);
		}

		p_ptr->update |= PU_BONUS;
		handle_stuff();
		mutant_regenerate_mod = calc_mutant_regenerate_mod();
		return TRUE;
	}
}


/*
 * Print out a description of the current mutations
 */
void dump_mutations(FILE *OutFile)
{
	const mutation_type *mut_ptr;
	
	int i;

	if (!OutFile) return;

	/* Run through the mutations */
	for (i = 0; i < MUT_PER_SET * 3; i++)
	{
		mut_ptr = &mutations[i];

		if (player_has_mut(i))
		{
			fprintf(OutFile, "%s\n", mut_ptr->desc_text);
		}
	}
}


/*
 * List mutations we have...
 */
void do_cmd_knowledge_mutations(void)
{
	FILE *fff;
	char file_name[1024];


	/* Open a temporary file */
	fff = my_fopen_temp(file_name, sizeof(file_name));

	/* Failure */
	if (!fff) return;

	/* Dump the mutations to file */
	if (fff) dump_mutations(fff);

	/* Close the file */
	my_fclose(fff);

	/* Display the file contents */
	(void)show_file(file_name, "Mutations", 0, 0);

	/* Remove the file */
	(void)fd_kill(file_name);
}


/* This works by masking off the lowest order set bits one at a time */
static int count_bits(u32b x)
{
	int n = 0;

	if (x) do
	{
		n++;
	}
	while (0 != (x = x & (x - 1)));

	return (n);
}


int count_mutations(void)
{
	return (count_bits(p_ptr->muta1) +
	        count_bits(p_ptr->muta2) +
	        count_bits(p_ptr->muta3));
}


/*
 * Return the modifier to the regeneration rate
 * (in percent)
 */
int calc_mutant_regenerate_mod(void)
{
	int regen;
	int mod = 10;
	int count = count_mutations();

	/*
	 * Beastman get 10 "free" mutations and
	 * only 5% decrease per additional mutation
	 */
	if (p_ptr->prace == RACE_BEASTMAN)
	{
		count -= 10;
		mod = 5;
	}

	/* No negative modifier */
	if (count <= 0) return 100;

	regen = 100 - count * mod;

	/* Max. 90% decrease in regeneration speed */
	if (regen < 10) regen = 10;

	return (regen);
}


/*
 * Use an activatable mutation power
 */
void mutation_power_aux(const mutation_type *mut_ptr)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	int     dir = 0;
	int     lvl = p_ptr->lev;
	cptr    q, s;


	if (!(racial_aux(mut_ptr->level, mut_ptr->cost, mut_ptr->stat,
		 mut_ptr->diff))) return;

	if (mut_ptr->which == MUT1_SPIT_ACID)
	{
		msg_print("You spit acid...");
		if (get_aim_dir(&dir))
		{
			(void)fire_ball(GF_ACID, dir, lvl, 1 + (lvl / 30));
		}
	}

	else if (mut_ptr->which == MUT1_BR_FIRE)
	{
		msg_print("You breathe fire...");
		if (get_aim_dir(&dir))
		{
			(void)fire_ball(GF_FIRE, dir, lvl * 2, 1 + (lvl / 20));
		}
	}

	else if (mut_ptr->which == MUT1_HYPN_GAZE)
	{
		msg_print("Your eyes look mesmerizing...");
		if (get_aim_dir(&dir))
		{
			(void)charm_monster(dir, lvl);
		}
	}

	else if (mut_ptr->which == MUT1_TELEKINES)
	{
		msg_print("You concentrate...");
		if (get_aim_dir(&dir))
		{
			fetch(dir, lvl * 10, TRUE);
		}
	}

	else if (mut_ptr->which == MUT1_VTELEPORT)
	{
		msg_print("You concentrate...");
		teleport_player(10 + 4 * lvl);
	}
	else if (mut_ptr->which == MUT1_MIND_BLST)
	{
		msg_print("You concentrate...");
		if (get_aim_dir(&dir))
		{
			(void)fire_bolt(GF_PSI, dir, damroll(3 + ((lvl - 1) / 5), 3));
		}
		else
		{
			/* Is this statement needed? */
			return;
		}
	}

	else if (mut_ptr->which == MUT1_RADIATION)
	{
		msg_print("Radiation flows from your body!");
		(void)fire_ball(GF_NUKE, 0, (lvl * 2), 3 + (lvl / 20));
	}

	else if (mut_ptr->which == MUT1_VAMPIRISM)
	{
		int x, y, dummy;
		cave_type *c_ptr;

		/* Handle player fear */
		if (p_ptr->afraid)
		{
			/* Message */
			msg_print("You are too afraid!");
			return;
		}

		/* Only works on adjacent monsters */
		if (!get_rep_dir(&dir)) return;
		y = py + ddy[dir];
		x = px + ddx[dir];

		/* paranoia */
		if (!in_bounds2(y, x)) return;

		c_ptr = area(y, x);

		if (!(c_ptr->m_idx))
		{
			msg_print("You bite into thin air!");
			return;
		}
		msg_print("You grin and bare your fangs...");

		dummy = lvl * 2;

		if (drain_gain_life(dir, dummy))
		{
			/* Gain nutritional sustenance: 150/hp drained */
			/* A Food ration gives 5000 food points (by contrast) */
			/* Don't ever get more than "Full" this way */
			/* But if we ARE Gorged,  it won't cure us */
			dummy = p_ptr->food + MIN(5000, 100 * dummy);
			if (p_ptr->food < PY_FOOD_MAX)   /* Not gorged already */
				(void)set_food(dummy >= PY_FOOD_MAX ? PY_FOOD_MAX-1 : dummy);
		}
		else
			msg_print("Yechh. That tastes foul.");
	}

	else if (mut_ptr->which == MUT1_SMELL_MET)
	{
		(void)detect_treasure();
	}

	else if (mut_ptr->which == MUT1_SMELL_MON)
	{
		(void)detect_monsters_normal();
	}

	else if (mut_ptr->which == MUT1_BLINK)
	{
		teleport_player(10);
	}

	else if (mut_ptr->which == MUT1_EAT_ROCK)
	{
		int x, y, ox, oy;
		cave_type *c_ptr;
				
		if (!get_rep_dir(&dir)) return;
		y = py + ddy[dir];
		x = px + ddx[dir];

		/* paranoia */
		if (!in_bounds2(y, x)) return;

		c_ptr = area(y, x);

		if (cave_floor_grid(c_ptr))
		{
			msg_print("You bite into thin air!");
			return;
		}
		else if (((c_ptr->feat >= FEAT_PERM_EXTRA) &&
			(c_ptr->feat <= FEAT_PERM_SOLID)) ||
			(c_ptr->feat == FEAT_MOUNTAIN))
		{
			msg_print("Ouch!  This wall is harder than your teeth!");
			return;
		}
		else if (c_ptr->m_idx)
		{
			msg_print("There's something in the way!");
			return;
		}
		else if (c_ptr->feat == FEAT_TREES)
		{
			msg_print("You don't like the woody taste!");
			return;
		}
		else
		{
			if ((c_ptr->feat >= FEAT_CLOSED) &&
				(c_ptr->feat <= FEAT_RUBBLE))
			{
				(void)set_food(p_ptr->food + 3000);
			}
			else if ((c_ptr->feat >= FEAT_MAGMA) &&
				(c_ptr->feat <= FEAT_QUARTZ_K))
			{
				(void)set_food(p_ptr->food + 5000);
			}
			else
			{
				msg_print("This granite is very filling!");
				(void)set_food(p_ptr->food + 10000);
			}
		}
		(void)wall_to_mud(dir);

		/* Save old location */
		oy = py;
		ox = px;

		/* Process fields under the player. */
		field_hook(&area(py, px)->fld_idx,
			 FIELD_ACT_PLAYER_LEAVE, NULL);

		/* Move the player */
		py = y;
		px = x;
		
		/* Move the player */
		p_ptr->py = y;
		p_ptr->px = x;

		if (!p_ptr->depth)
		{
			/* Scroll wilderness */
			p_ptr->wilderness_x = px;
			p_ptr->wilderness_y = py;
			move_wild();
		}
		
		lite_spot(py, px);
		lite_spot(oy, ox);
		
		/* Process fields under the player. */
		field_hook(&area(py, px)->fld_idx, FIELD_ACT_PLAYER_ENTER, NULL);

		verify_panel();

		p_ptr->update |= (PU_VIEW | PU_FLOW | PU_MON_LITE);
		p_ptr->update |= (PU_DISTANCE);
		p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);
	}

	else if (mut_ptr->which == MUT1_SWAP_POS)
	{
		if (get_aim_dir(&dir))
		{
			(void)teleport_swap(dir);
		}
	}

	else if (mut_ptr->which == MUT1_SHRIEK)
	{
		(void)fire_ball(GF_SOUND, 0, 2 * lvl, 8);
		(void)aggravate_monsters(0);
	}

	else if (mut_ptr->which == MUT1_ILLUMINE)
	{
		(void)lite_area(damroll(2, (lvl / 2)), (lvl / 10) + 1);
	}

	else if (mut_ptr->which	== MUT1_DET_CURSE)
	{
		int i;

		for (i = 0; i < INVEN_TOTAL; i++)
		{
			object_type *o_ptr = &inventory[i];

			if (!o_ptr->k_idx) continue;
			if (!cursed_p(o_ptr)) continue;

			o_ptr->feeling = FEEL_CURSED;
		}
	}

	else if (mut_ptr->which == MUT1_BERSERK)
	{
		if (!p_ptr->shero)
		{
			(void)hp_player(30);
		}
		
		(void)set_shero(p_ptr->shero + rand_range(25, 50));
		(void)set_afraid(0);
	}

	else if (mut_ptr->which == MUT1_POLYMORPH)
	{
				do_poly_self();
	}

	else if (mut_ptr->which == MUT1_MIDAS_TCH)
	{
		(void)alchemy();
	}

	/* Summon pet molds around the player */
	else if (mut_ptr->which == MUT1_GROW_MOLD)
	{
		int i;

		for (i = 0; i < 8; i++)
		{
			(void)summon_specific(-1, py, px, lvl, SUMMON_BIZARRE1, FALSE, TRUE, TRUE);
		}
	}

	else if (mut_ptr->which == MUT1_RESIST)
	{
		int num = lvl / 10;
		int dur = rand_range(20, 40);

		if (randint0(5) < num)
		{
			(void)set_oppose_acid(p_ptr->oppose_acid + dur);
			num--;
		}
		if (randint0(4) < num)
		{
			(void)set_oppose_elec(p_ptr->oppose_elec + dur);
			num--;
		}
		if (randint0(3) < num)
		{
			(void)set_oppose_fire(p_ptr->oppose_fire + dur);
			num--;
		}
		if (randint0(2) < num)
		{
			(void)set_oppose_cold(p_ptr->oppose_cold + dur);
			num--;
		}
		if (num)
		{
			(void)set_oppose_pois(p_ptr->oppose_pois + dur);
			num--;
		}
	}

	else if (mut_ptr->which == MUT1_EARTHQUAKE)
	{
		(void)earthquake(py, px, 10);
	}

	else if (mut_ptr->which == MUT1_EAT_MAGIC)
	{
		object_type * o_ptr;
		int lev, item;

		item_tester_hook = item_tester_hook_recharge;

		/* Get an item */
		q = "Drain which item? ";
		s = "You have nothing to drain.";
		if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

		if (item >= 0)
		{
			o_ptr = &inventory[item];
		}
		else
		{
			o_ptr = &o_list[0 - item];
		}

		lev = get_object_level(o_ptr);

		if (o_ptr->tval == TV_ROD)
		{
			if (o_ptr->pval > 0)
			{
				msg_print("You can't absorb energy from a discharged rod.");
			}
			else
			{
				p_ptr->csp += 2 * lev;
				o_ptr->pval = 500;
			}
		}
		else
		{
			if (o_ptr->pval > 0)
			{
				p_ptr->csp += o_ptr->pval * lev;
				o_ptr->pval = 0;
			}
			else
			{
				msg_print("There's no energy there to absorb!");
			}
			o_ptr->ident |= IDENT_EMPTY;
		}

		if (p_ptr->csp > p_ptr->msp)
		{
			p_ptr->csp = p_ptr->msp;
		}

		p_ptr->notice |= (PN_COMBINE | PN_REORDER);
		p_ptr->window |= (PW_INVEN);
	}

	else if (mut_ptr->which == MUT1_WEIGH_MAG)
	{
		report_magics();
	}

	/* Fake a population explosion. */
	else if (mut_ptr->which == MUT1_STERILITY)
	{
		msg_print("You suddenly have a headache!");
		take_hit(rand_range(17, 34), "the strain of forcing abstinence");
		num_repro += MAX_REPRO;
	}

	else if (mut_ptr->which == MUT1_PANIC_HIT)
	{
		int x, y;

		if (!get_rep_dir(&dir)) return;
		y = py + ddy[dir];
		x = px + ddx[dir];

		/* paranoia */
		if (!in_bounds2(y, x)) return;

		if (area(y, x)->m_idx)
		{
			py_attack(y, x);
			teleport_player(30);
		}
		else
		{
			msg_print("You don't see any monster in this direction");
			msg_print(NULL);
		}
	}

	else if (mut_ptr->which == MUT1_DAZZLE)
	{
		(void)stun_monsters(lvl * 4);
		(void)confuse_monsters(lvl * 4);
		(void)turn_monsters(lvl * 4);
	}

	else if (mut_ptr->which == MUT1_LASER_EYE)
	{
		if (get_aim_dir(&dir))
			(void)fire_beam(GF_LITE, dir, 2 * lvl);
	}

	else if (mut_ptr->which == MUT1_RECALL)
	{
		word_of_recall();
	}

	else if (mut_ptr->which == MUT1_BANISH)
	{
		int x, y;
		cave_type *c_ptr;
		monster_type *m_ptr;
		monster_race *r_ptr;

		if (!get_rep_dir(&dir)) return;
		y = py + ddy[dir];
		x = px + ddx[dir];

		/* paranoia */
		if (!in_bounds2(y, x)) return;

		c_ptr = area(y, x);

		if (!c_ptr->m_idx)
		{
			msg_print("You sense no evil there!");
			return;
		}

		m_ptr = &m_list[c_ptr->m_idx];
		r_ptr = &r_info[m_ptr->r_idx];

		if ((r_ptr->flags3 & RF3_EVIL) &&
		    !(r_ptr->flags1 & RF1_QUESTOR) &&
		    !(r_ptr->flags1 & RF1_UNIQUE))
		{
			/* Delete the monster, rather than killing it. */
			delete_monster_idx(c_ptr->m_idx);
			msg_print("The evil creature vanishes in a puff of sulfurous smoke!");
		}
		else
		{
			msg_print("Your invocation is ineffectual!");
		}
	}				
	else if (mut_ptr->which == MUT1_COLD_TOUCH)
	{
		int x, y;
		cave_type *c_ptr;

		if (!get_rep_dir(&dir)) return;
		y = py + ddy[dir];
		x = px + ddx[dir];

		/* paranoia */
		if (!in_bounds2(y, x)) return;
		
		c_ptr = area(y, x);
		
		if (!c_ptr->m_idx)
		{
			msg_print("You wave your hands in the air.");
			return;
		}
		(void)fire_bolt(GF_COLD, dir, 2 * lvl);
	}
	
	/* Gives a multiplier of 2 at first, up to 3 at level 30 */
	else if (mut_ptr->which == MUT1_LAUNCHER)
	{
		do_cmd_throw_aux(2 + lvl / 30);
	}
}


/*
 * Proces the random mutations
 */
void mutation_random_aux(const mutation_type *mut_ptr)
{
	if (!one_in_(mut_ptr->chance * 100)) return;
	
	if (mut_ptr->which == MUT2_BERS_RAGE)
	{
		disturb(FALSE);
		msg_print("RAAAAGHH!");
		msg_print("You feel a fit of rage coming over you!");
		(void)set_shero(p_ptr->shero + 10 + randint1(p_ptr->lev));
	}

	else if (mut_ptr->which == MUT2_COWARDICE)
	{
		if (!(p_ptr->resist_fear || p_ptr->hero || p_ptr->shero))
		{
			disturb(FALSE);
			msg_print("It's so dark... so scary!");
			(void)set_afraid(p_ptr->afraid + rand_range(13, 40));
		}
	}			

	else if (mut_ptr->which == MUT2_RTELEPORT)
	{
		if (!p_ptr->resist_nexus && !p_ptr->muta1 & MUT1_VTELEPORT &&
		    !p_ptr->anti_tele)
		{
			disturb(FALSE);				
			
			/* Teleport player */
			msg_print("Your position suddenly seems very uncertain...");
			msg_print(NULL);
			teleport_player(40);
		}
	}
	
	else if (mut_ptr->which == MUT2_ALCOHOL)
	{
		if (!p_ptr->resist_confu && !p_ptr->resist_chaos)
		{
			disturb(FALSE);
			p_ptr->redraw |= PR_EXTRA;
			msg_print("You feel a SSSCHtupor cOmINg over yOu... *HIC*!");
		}			
		
		if (!p_ptr->resist_confu)
		{
			(void)set_confused(p_ptr->confused + rand_range(15, 35));
		}

		if (!p_ptr->resist_chaos)
		{
			if (one_in_(20))
			{
				msg_print(NULL);
				if (one_in_(3)) (void)lose_all_info();
				else wiz_dark();
				teleport_player(100);
				wiz_dark();
				msg_print("You wake up somewhere with a sore head...");
				msg_print("You can't remember a thing, or how you got here!");
			}
			else
			{
				if (one_in_(3))
				{
					msg_print("Thishcischs GooDSChtuff!");
					(void)set_image(p_ptr->image + rand_range(150, 300));
				}
 			}
		}
	}				
	
	else if (mut_ptr->which == MUT2_HALLU)
	{
		if (!p_ptr->resist_chaos)
		{
			disturb(FALSE);
			p_ptr->redraw |= PR_EXTRA;
			(void)set_image(p_ptr->image + rand_range(20, 70));
		}
	}			
				
	else if (mut_ptr->which == MUT2_FLATULENT)
	{
		disturb(FALSE);

		msg_print("BRRAAAP! Oops.");
		msg_print(NULL);
		(void)fire_ball(GF_POIS, 0, p_ptr->lev, 3);
	}

	else if ((mut_ptr->which == MUT2_PROD_MANA) && !p_ptr->anti_magic)
	{
		int dire = 0;
		disturb(FALSE);
		msg_print("Magical energy flows through you! You must release it!");
		flush();
		msg_print(NULL);
		(void)get_hack_dir(&dire);
		(void)fire_ball(GF_MANA, dire, p_ptr->lev * 2, 3);
	}				

	else if ((mut_ptr->which == MUT2_ATT_DEMON) && !p_ptr->anti_magic)
	{
		bool pet = (one_in_(6));

		if (summon_specific((pet ? -1 : 0), p_ptr->py, p_ptr->px,
				 p_ptr->depth, SUMMON_DEMON, TRUE, FALSE, pet))
		{
			msg_print("You have attracted a demon!");
			disturb(FALSE);
		}
	}

	else if (mut_ptr->which == MUT2_SPEED_FLUX)
	{
		disturb(FALSE);
		if (one_in_(2))
		{
			msg_print("You feel less energetic.");
			if (p_ptr->fast > 0)
			{
				(void)set_fast(0);
			}
			else
			{
				(void)set_slow(p_ptr->slow + rand_range(10, 40));
			}
		}
		else
		{
			msg_print("You feel more energetic.");
			if (p_ptr->slow > 0)
			{
				(void)set_slow(0);
			}
			else
			{
				(void)set_fast(p_ptr->fast + rand_range(10, 40));
			}
		}
		msg_print(NULL);
	}

	else if (mut_ptr->which == MUT2_BANISH_ALL)
	{
		disturb(FALSE);
		msg_print("You suddenly feel almost lonely.");
		(void)banish_monsters(100);
		msg_print(NULL);
	}

	else if (mut_ptr->which == MUT2_EAT_LIGHT)
 	{
		object_type *o_ptr;
		cave_type *c_ptr = area(p_ptr->py, p_ptr->px);

		msg_print("A shadow passes over you.");
		msg_print(NULL);

		/* Absorb light from the current possition */
		if (c_ptr->info & CAVE_GLOW)
		{
			(void)hp_player(10);
		}

		o_ptr = &inventory[INVEN_LITE];

		/* Absorb some fuel in the current lite */
		if (o_ptr->tval == TV_LITE)
		{
			/* Use some fuel (except on artifacts) */
			if (!(o_ptr->flags3 & TR3_INSTA_ART) && (o_ptr->pval > 0))
			{
				/* Heal the player a bit */
				(void)hp_player(o_ptr->pval / 20);

				/* Decrease life-span of lite */
				o_ptr->pval /= 2;

				msg_print("You absorb energy from your light!");

				/* Notice interesting fuel steps */
				notice_lite_change(o_ptr);
			}
		}

		/*
		 * Unlite the area (radius 10) around player and
		 * do 50 points damage to every affected monster
		 */
		(void)unlite_area(50, 10);
	}	
			
	else if ((mut_ptr->which == MUT2_ATT_ANIMAL) && !p_ptr->anti_magic)
	{
		bool pet = (one_in_(3));

		if (summon_specific((pet ? -1 : 0), p_ptr->py, p_ptr->px,
			 p_ptr->depth, SUMMON_ANIMAL, TRUE, FALSE, pet))
		{
			msg_print("You have attracted an animal!");
			disturb(FALSE);
		}
	}

	else if ((mut_ptr->which == MUT2_RAW_CHAOS) && !p_ptr->anti_magic)
	{
		disturb(FALSE);
		msg_print("You feel the world warping around you!");
		msg_print(NULL);
		(void)fire_ball(GF_CHAOS, 0, p_ptr->lev, 8);
	}

	else if (mut_ptr->which == MUT2_NORMALITY)
	{
		if (!lose_mutation(0))
		{
			msg_print("You feel oddly normal.");
		}
	}

	else if ((mut_ptr->which == MUT2_WRAITH) && !p_ptr->anti_magic)
	{
		disturb(FALSE);
		msg_print("You feel insubstantial!");
		msg_print(NULL);
		(void)set_wraith_form(p_ptr->wraith_form +
			 rand_range(p_ptr->lev / 2, p_ptr->lev));
	}

	else if (mut_ptr->which == MUT2_POLY_WOUND)
	{
		do_poly_wounds();
	}

	else if (mut_ptr->which == MUT2_WASTING)
	{
		int which_stat = randint0(6);
		int sustained = FALSE;

		switch (which_stat)
		{
		case A_STR:
			if (p_ptr->sustain_str) sustained = TRUE;
			break;
		case A_INT:
			if (p_ptr->sustain_int) sustained = TRUE;
			break;
		case A_WIS:
			if (p_ptr->sustain_wis) sustained = TRUE;
			break;
		case A_DEX:
			if (p_ptr->sustain_dex) sustained = TRUE;
			break;
		case A_CON:
			if (p_ptr->sustain_con) sustained = TRUE;
			break;
		case A_CHR:
			if (p_ptr->sustain_chr) sustained = TRUE;
			break;
		default:
			msg_print("Invalid stat chosen!");
			sustained = TRUE;
		}

		if (!sustained)
		{
			disturb(FALSE);
			msg_print("You can feel yourself wasting away!");
			msg_print(NULL);
#if 0
			(void)dec_stat(which_stat, rand_range(6, 12), one_in_(3));
#else
			(void)dec_stat(which_stat, rand_range(6, 12), 0);
#endif
		}
	}

	else if ((mut_ptr->which == MUT2_ATT_DRAGON) && !p_ptr->anti_magic)
	{
		bool pet = (one_in_(5));

		if (summon_specific((pet ? -1 : 0), p_ptr->py, p_ptr->px,
			 p_ptr->depth, SUMMON_DRAGON, TRUE, FALSE, pet))
		{
			msg_print("You have attracted a dragon!");
			disturb(FALSE);
		}
	}

	else if ((mut_ptr->which == MUT2_WEIRD_MIND) && !p_ptr->anti_magic)
	{
		if (p_ptr->tim_esp > 0)
		{
			msg_print("Your mind feels cloudy!");
			(void)set_tim_esp(0);
		}
		else
		{
			msg_print("Your mind expands!");
			(void)set_tim_esp(p_ptr->lev);
		}
	}

	else if ((mut_ptr->which == MUT2_NAUSEA) && !p_ptr->slow_digest)
	{
		disturb(FALSE);
		msg_print("Your stomach roils, and you lose your lunch!");
		msg_print(NULL);
		(void)set_food(PY_FOOD_WEAK);
	}

	else if ((mut_ptr->which == MUT2_WALK_SHAD) && !p_ptr->anti_magic)
	{
		alter_reality();
	}

	else if (mut_ptr->which == MUT2_WARNING)
	{
		int danger_amount = 0;
		int monster;

		for (monster = 0; monster < m_max; monster++)
		{
			monster_type    *m_ptr = &m_list[monster];
			monster_race    *r_ptr = &r_info[m_ptr->r_idx];

			/* Paranoia -- Skip dead monsters */
			if (!m_ptr->r_idx) continue;
		
			if (r_ptr->level >= p_ptr->lev)
			{
				danger_amount += r_ptr->level - p_ptr->lev + 1;
			}
		}

		if (danger_amount > 100)
			msg_print("You feel utterly terrified!");
		else if (danger_amount > 50)
			msg_print("You feel terrified!");
		else if (danger_amount > 20)
			msg_print("You feel very worried!");
		else if (danger_amount > 10)
			msg_print("You feel paranoid!");
		else if (danger_amount > 5)
			msg_print("You feel almost safe.");
		else
			msg_print("You feel lonely.");
	}

	else if ((mut_ptr->which == MUT2_INVULN) && !p_ptr->anti_magic)
	{
		disturb(FALSE);
		msg_print("You feel invincible!");
		msg_print(NULL);
		(void)set_invuln(p_ptr->invuln + rand_range(8, 16));
	}
				
	else if (mut_ptr->which == MUT2_SP_TO_HP)
	{
		int wounds = p_ptr->mhp - p_ptr->chp;		

		if (wounds > 0)
		{
			int healing = p_ptr->csp;
			
			if (healing > wounds)
			{
				healing = wounds;
			}
	
			(void)hp_player(healing);
			p_ptr->csp -= healing;
		}
	}

	else if ((mut_ptr->which == MUT2_HP_TO_SP) && !p_ptr->anti_magic)
	{
		int wounds = p_ptr->msp - p_ptr->csp;
		
		if (wounds > 0)
		{
			int healing = p_ptr->chp;

			if (healing > wounds)
			{
				healing = wounds;
			}
		
			p_ptr->csp += healing;
			take_hit(healing, "blood rushing to the head");
		}
	}

	else if (mut_ptr->which == MUT2_DISARM)
	{
		object_type *o_ptr;

		disturb(FALSE);
		msg_print("You trip over your own feet!");
		take_hit(randint1(p_ptr->wt / 6), "tripping");

		msg_print(NULL);
		o_ptr = &inventory[INVEN_WIELD];
			if ((o_ptr->k_idx) && !cursed_p(o_ptr))
		{
			msg_print("You drop your weapon!");
			inven_drop(INVEN_WIELD, 1);
		}
	}
}

/* Constan mutation effects */
void mutation_effect(void)
{
	/* Hyper Strength */
	if (p_ptr->muta3 & MUT3_HYPER_STR)
	{
		p_ptr->stat_add[A_STR] += 4;
	}

	/* Puny */
	if (p_ptr->muta3 & MUT3_PUNY)
	{
		p_ptr->stat_add[A_STR] -= 4;
	}

	/* Living computer */
	if (p_ptr->muta3 & MUT3_HYPER_INT)
	{
		p_ptr->stat_add[A_INT] += 4;
		p_ptr->stat_add[A_WIS] += 4;
	}

	/* Moronic */
	if (p_ptr->muta3 & MUT3_MORONIC)
	{
		p_ptr->stat_add[A_INT] -= 4;
		p_ptr->stat_add[A_WIS] -= 4;
	}

	if (p_ptr->muta3 & MUT3_RESILIENT)
	{
		p_ptr->stat_add[A_CON] += 4;
	}

	if (p_ptr->muta3 & MUT3_XTRA_FAT)
	{
		p_ptr->stat_add[A_CON] += 2;
		p_ptr->pspeed -= 2;
	}

	if (p_ptr->muta3 & MUT3_ALBINO)
	{
		p_ptr->stat_add[A_CON] -= 4;
	}

	if (p_ptr->muta3 & MUT3_FLESH_ROT)
	{
		p_ptr->stat_add[A_CON] -= 2;
		p_ptr->stat_add[A_CHR] -= 1;
		p_ptr->regenerate = FALSE;
		/* Cancel innate regeneration */
	}

	if (p_ptr->muta3 & MUT3_SILLY_VOI)
	{
		p_ptr->stat_add[A_CHR] -= 4;
	}

	if (p_ptr->muta3 & MUT3_BLANK_FAC)
	{
		p_ptr->stat_add[A_CHR] -= 1;
	}

	if (p_ptr->muta3 & MUT3_XTRA_EYES)
	{
		p_ptr->skill_fos += 15;
		p_ptr->skill_srh += 15;
	}

	if (p_ptr->muta3 & MUT3_MAGIC_RES)
	{
		p_ptr->skill_sav += (15 + (p_ptr->lev / 5));
	}

	if (p_ptr->muta3 & MUT3_XTRA_NOIS)
	{
		p_ptr->skill_stl -= 3;
	}

	if (p_ptr->muta3 & MUT3_INFRAVIS)
	{
		p_ptr->see_infra += 3;
	}

	if (p_ptr->muta3 & MUT3_XTRA_LEGS)
	{
		p_ptr->pspeed += 3;
	}

	if (p_ptr->muta3 & MUT3_SHORT_LEG)
	{
		p_ptr->pspeed -= 3;
	}

	if (p_ptr->muta3 & MUT3_ELEC_TOUC)
	{
		p_ptr->sh_elec = TRUE;
	}

	if (p_ptr->muta3 & MUT3_FIRE_BODY)
	{
		p_ptr->sh_fire = TRUE;
		p_ptr->lite = TRUE;
	}

	if (p_ptr->muta3 & MUT3_WART_SKIN)
	{
		p_ptr->stat_add[A_CHR] -= 2;
		p_ptr->to_a += 5;
		p_ptr->dis_to_a += 5;
	}

	if (p_ptr->muta3 & MUT3_SCALES)
	{
		p_ptr->stat_add[A_CHR] -= 1;
		p_ptr->to_a += 10;
		p_ptr->dis_to_a += 10;
	}

	if (p_ptr->muta3 & MUT3_IRON_SKIN)
	{
		p_ptr->stat_add[A_DEX] -= 1;
		p_ptr->to_a += 25;
		p_ptr->dis_to_a += 25;
	}

	if (p_ptr->muta3 & MUT3_WINGS)
	{
		p_ptr->ffall = TRUE;
	}

	if (p_ptr->muta3 & MUT3_FEARLESS)
	{
		p_ptr->resist_fear = TRUE;
	}

	if (p_ptr->muta3 & MUT3_REGEN)
	{
		p_ptr->regenerate = TRUE;
	}

	if (p_ptr->muta3 & MUT3_ESP)
	{
		p_ptr->telepathy = TRUE;
	}

	if (p_ptr->muta3 & MUT3_LIMBER)
	{
		p_ptr->stat_add[A_DEX] += 3;
	}

	if (p_ptr->muta3 & MUT3_ARTHRITIS)
	{
		p_ptr->stat_add[A_DEX] -= 3;
	}

	if (p_ptr->muta3 & MUT3_MOTION)
	{
		p_ptr->free_act = TRUE;
		p_ptr->skill_stl += 1;
	}

	if (p_ptr->muta3 & MUT3_ILL_NORM)
	{
		p_ptr->stat_add[A_CHR] = 0;
	}
}
