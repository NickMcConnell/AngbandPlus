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
			case 1:  case 2:  case 3:  case 4:
				/* Spit acid */
				num = 0;
				break;
			case 5:  case 6:  case 7:
				/* Breathe fire */
				num = 1;
				break;
			case 8:  case 9:
				/* Hypnotic gaze */
				num = 2;
				break;
			case 10:  case 11:
				/* Telekinesis */
				num = 3;
				break;
			case 12:  case 13:  case 14:
				/* Vteleport */
				num = 4;
				break;
			case 15:  case 16:
				/* Mind blast */
				num = 5;
				break;
			case 17:  case 18:
				/* Radiation */
				num = 6;
				break;
			case 19:  case 20:
				/* Vampirism */
				num = 7;
				break;
			case 21:  case 22:  case 23:
				/* Smell metal */
				num = 8;
				break;
			case 24:  case 25:  case 26:  case 27:
				/* Smell monsters */
				num = 9;
				break;
			case 28:  case 29:  case 30:
				/* Blink */
				num = 10;
				break;
			case 31:  case 32:
				/* Eat rock */
				num = 11;
				break;
			case 33:  case 34:
				/* Swap position */
				num = 12;
				break;
			case 35:  case 36:  case 37:
				/* Shriek */
				num = 13;
				break;
			case 38:  case 39:  case 40:
				/* Illuminate */
				num = 14;
				break;
			case 41:  case 42:
				/* Detect curse */
				num = 15;
				break;
			case 43:  case 44:  case 45:
				/* Berserk */
				num = 16;
				break;
			case 46:
				/* Polymorph */
				num = 17;
				break;
			case 47:  case 48:
				/* Midas */
				/* Only rich characters can get this */
				if (p_ptr->au >= p_ptr->lev * 1000L)
				{
					num = 18;
				}
				break;
			case 49:
				/* Mold */
				num = 19;
				break;
			case 50:  case 51:  case 52:
				/* Resist Elements */
				num = 20;
				break;
			case 53:  case 54:  case 55:
				/* Earthquake */
				num = 21;
				break;
			case 56:
				/* Eat magic */
				num = 22;
				break;
			case 57:  case 58:
				/* Weigh magic */
				num = 23;
				break;
			case 59:
				/* Sterilize */
				num = 24;
				break;
			case 60:  case 61:
				/* Panic hit */
				num = 25;
				break;
			case 62:  case 63:  case 64:
				/* Dazzle */
				num = 26;
				break;
			case 65:  case 66:  case 67:
				/* Laser eye */
				num = 27;
				break;
			case 68:  case 69:
				/* Recall */
				num = 28;
				break;
			case 70:
				/* Banish */
				num = 29;
				break;
			case 71:  case 72:
				/* Cold touch */
				num = 30;
				break;
			case 73:  case 74:
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
			case 81:  case 82:
				/* Scorpion */
				num = 38;
				break;
			case 83:  case 84:
				/* Horns */
				num = 39;
				break;
			case 85:  case 86:
				/* Beak */
				num = 40;
				break;
			case 87:  case 88:
				/* Demons */
				num = 41;
				break;
			case 89:
				/* Mana */
				num = 42;
				break;
			case 90:  case 91:
				/* Speed flux */
				num = 43;
				break;
			case 92:  case 93:
				/* Banish */
				num = 44;
				break;
			case 94:
				/* Eat lite */
				num = 45;
				break;
			case 95:  case 96:
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
			case 100:  case 101:  case 102:
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
			case 107:  case 108:
				/* Esp */
				num = 55;
				break;
			case 109:
				/* Sick */
				num = 56;
				break;
			case 110:  case 111:
				/* Chaos warriors already have a chaos deity */
				if (p_ptr->rp.pclass != CLASS_CHAOS_WARRIOR)
				{
					/* Patron */
					num = 57;
				}
				break;
			case 112:
				/* Shadow walk */
				num = 58;
				break;
			case 113:  case 114:
				/* Warn */
				num = 59;
				break;
			case 115:
				/* Invuln */
				num = 60;
				break;
			case 116:  case 117:
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
			case 120:  case 121:  case 122:
				/* Strong */
				num = 64;
				break;
			case 123:  case 124:  case 125:
				/* Weak */
				num = 65;
				break;
			case 126:  case 127:  case 128:
				/* Smart */
				num = 66;
				break;
			case 129:  case 130:  case 131:
				/* Dumb */
				num = 67;
				break;
			case 132:  case 133:
				/* Con */
				num = 68;
				break;
			case 134:  case 135:
				/* Fat */
				num = 69;
				break;
			case 136:  case 137:
				/* Frail */
				num = 70;
				break;
			case 138:  case 139:  case 140:
				/* Rot */
				num = 71;
				break;
			case 141:  case 142:
				/* Squeak */
				/* Restricted to chars with several muts already */
				if (count_mutations() >= 3)
				{
					num = 72;
				}
				break;
			case 143:  case 144:
				/* Blank face */
				num = 73;
				break;
			case 145:
				/* Illusion face */
				num = 74;
				break;
			case 146:  case 147:  case 148:
				/* eyes */
				num = 75;
				break;
			case 149:  case 150:
				/* Res magic */
				num = 76;
				break;
			case 151:  case 152:  case 153:
				/* Noise */
				num = 77;
				break;
			case 154:  case 155:  case 156:
				/* Infra */
				num = 78;
				break;
			case 157:  case 158:
				/* Legs fast */
				num = 79;
				break;
			case 159:  case 160:
				/* Legs slow */
				num = 80;
				break;
			case 161:  case 162:
				/* Aura elec */
				num = 81;
				break;
			case 163:  case 164:
				/* Aura fire */
				num = 82;
				break;
			case 165:  case 166:  case 167:
				/* Warts */
				num = 83;
				break;
			case 168:  case 169:  case 170:
				/* Scales */
				num = 84;
				break;
			case 171:  case 172:
				/* Iron */
				num = 85;
				break;
			case 173:  case 174:
				/* Wings */
				num = 86;
				break;
			case 175:  case 176:  case 177:
				/* Res fear */
				num = 87;
				break;
			case 178:  case 179:
				/* Regen */
				num = 88;
				break;
			case 180:  case 181:
				/* ESP */
				num = 89;
				break;
			case 182:  case 183:  case 184:
				/* Limber */
				num = 90;
				break;
			case 185:  case 186:  case 187:
				/* Arthritis */
				num = 91;
				break;
			case 188:
				/* Bad luck */
				num = 92;
				break;
			case 189:
				/* Bad element */
				/* Restricted to chars with several muts already */
				if (count_mutations() >= 3)
				{
					num = 93;
				}
				break;
			case 190:  case 191:  case 192:
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

		/* Lower chance of getting an unusable mutation */
		if (num >= 0 && mutations[num].level > p_ptr->lev &&
				!one_in_(mutations[num].level - p_ptr->lev + 1))
		{
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
		msgf("You feel normal.");
		return FALSE;
	}
	else
	{
		chg_virtue(V_CHANCE, 1);

		if (p_ptr->rp.prace == RACE_VAMPIRE &&
			!(p_ptr->muta1 & MUT1_HYPN_GAZE) && (randint1(10) < 7))
		{
			num = M1_HYPN_GAZE;
		}

		else if (p_ptr->rp.prace == RACE_IMP &&
				 !(p_ptr->muta2 & MUT2_HORNS) && (randint1(10) < 7))
		{
			num = M2_HORNS;
		}

		else if (p_ptr->rp.prace == RACE_YEEK &&
				 !(p_ptr->muta1 & MUT1_SHRIEK) && (randint1(10) < 7))
		{
			num = M1_SHRIEK;
		}

		else if (p_ptr->rp.prace == RACE_BEASTMAN &&
				 !(p_ptr->muta1 & MUT1_POLYMORPH) && (randint1(10) < 2))
		{
			num = M1_POLYMORPH;
		}

		else if (p_ptr->rp.prace == RACE_MIND_FLAYER &&
				 !(p_ptr->muta2 & MUT2_TENTACLES) && (randint1(10) < 7))
		{
			num = M2_TENTACLES;
		}

		/* Point to the mutation */
		mut_ptr = &mutations[num];

		muta_which = mut_ptr->which;

		msgf("You mutate!");
		msgf(mut_ptr->gain_text);

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
					msgf("You no longer feel super-strong!");
					p_ptr->muta3 &= ~(MUT3_HYPER_STR);
				}
			}
			else if (muta_which == MUT3_HYPER_STR)
			{
				if (p_ptr->muta3 & MUT3_PUNY)
				{
					msgf("You no longer feel puny!");
					p_ptr->muta3 &= ~(MUT3_PUNY);
				}
			}
			else if (muta_which == MUT3_MORONIC)
			{
				if (p_ptr->muta3 & MUT3_HYPER_INT)
				{
					msgf("Your brain is no longer a living computer.");
					p_ptr->muta3 &= ~(MUT3_HYPER_INT);
				}
			}
			else if (muta_which == MUT3_HYPER_INT)
			{
				if (p_ptr->muta3 & MUT3_MORONIC)
				{
					msgf("You are no longer moronic.");
					p_ptr->muta3 &= ~(MUT3_MORONIC);
				}
			}
			else if (muta_which == MUT3_IRON_SKIN)
			{
				if (p_ptr->muta3 & MUT3_SCALES)
				{
					msgf("You lose your scales.");
					p_ptr->muta3 &= ~(MUT3_SCALES);
				}
				if (p_ptr->muta3 & MUT3_FLESH_ROT)
				{
					msgf("Your flesh rots no longer.");
					p_ptr->muta3 &= ~(MUT3_FLESH_ROT);
				}
				if (p_ptr->muta3 & MUT3_WART_SKIN)
				{
					msgf("You lose your warts.");
					p_ptr->muta3 &= ~(MUT3_WART_SKIN);
				}
			}
			else if ((muta_which == MUT3_WART_SKIN) ||
					 (muta_which == MUT3_SCALES) ||
					 (muta_which == MUT3_FLESH_ROT))
			{
				if (p_ptr->muta3 & MUT3_IRON_SKIN)
				{
					msgf("Your skin is no longer made of steel.");
					p_ptr->muta3 &= ~(MUT3_IRON_SKIN);
				}
			}
			else if (muta_which == MUT3_FEARLESS)
			{
				if (p_ptr->muta2 & MUT2_COWARDICE)
				{
					msgf("You are no longer cowardly.");
					p_ptr->muta2 &= ~(MUT2_COWARDICE);
				}
			}
			else if (muta_which == MUT3_FLESH_ROT)
			{
				if (p_ptr->muta3 & MUT3_REGEN)
				{
					msgf("You stop regenerating.");
					p_ptr->muta3 &= ~(MUT3_REGEN);
				}
			}
			else if (muta_which == MUT3_REGEN)
			{
				if (p_ptr->muta3 & MUT3_FLESH_ROT)
				{
					msgf("Your flesh stops rotting.");
					p_ptr->muta3 &= ~(MUT3_FLESH_ROT);
				}
			}
			else if (muta_which == MUT3_LIMBER)
			{
				if (p_ptr->muta3 & MUT3_ARTHRITIS)
				{
					msgf("Your joints stop hurting.");
					p_ptr->muta3 &= ~(MUT3_ARTHRITIS);
				}
			}
			else if (muta_which == MUT3_ARTHRITIS)
			{
				if (p_ptr->muta3 & MUT3_LIMBER)
				{
					msgf("You no longer feel limber.");
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
					msgf("You no longer feel fearless.");
					p_ptr->muta3 &= ~(MUT3_FEARLESS);
				}
			}
			if (muta_which == MUT2_BEAK)
			{
				if (p_ptr->muta2 & MUT2_TRUNK)
				{
					msgf("Your nose is no longer elephantine.");
					p_ptr->muta2 &= ~(MUT2_TRUNK);
				}
			}
			if (muta_which == MUT2_TRUNK)
			{
				if (p_ptr->muta2 & MUT2_BEAK)
				{
					msgf("You no longer have a hard beak.");
					p_ptr->muta2 &= ~(MUT2_BEAK);
				}
			}
		}

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

		msgf(mut_ptr->lose_text);

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
		return TRUE;
	}
}


/*
 * Print out a description of the current mutations
 */
void dump_mutations(FILE *fff)
{
	const mutation_type *mut_ptr;

	int i;

	if (!fff) return;

	/* Run through the mutations */
	for (i = 0; i < MUT_PER_SET * 3; i++)
	{
		mut_ptr = &mutations[i];

		if (player_has_mut(i))
		{
			froff(fff, "%s\n", mut_ptr->desc_text);
		}
	}
}


/*
 * List mutations we have...
 */
bool do_cmd_knowledge_mutations(int dummy)
{
	FILE *fff;
	char file_name[1024];
	
	/* Hack - ignore parameter */
	(void) dummy;

	/* Open a temporary file */
	fff = my_fopen_temp(file_name, sizeof(file_name));

	/* Failure */
	if (!fff) return (FALSE);

	/* Dump the mutations to file */
	if (fff) dump_mutations(fff);

	/* Close the file */
	my_fclose(fff);

	/* Display the file contents */
	(void)show_file(file_name, "Mutations", 0, 0);

	/* Remove the file */
	(void)fd_kill(file_name);
	
	return (FALSE);
}



int count_mutations(void)
{
	return (count_bits(p_ptr->muta1) +
			count_bits(p_ptr->muta2) + count_bits(p_ptr->muta3));
}


/*
 * Use an activatable mutation power
 */
void mutation_power_aux(const mutation_type *mut_ptr)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	int dir = 0;
	int lvl = p_ptr->lev;
	cptr q, s;


	if (!(racial_aux(mut_ptr->level, mut_ptr->cost, mut_ptr->stat,
					 mut_ptr->diff))) return;

	if (mut_ptr->which == MUT1_SPIT_ACID)
	{
		msgf("You spit acid...");
		if (get_aim_dir(&dir))
		{
			(void)fire_ball(GF_ACID, dir, lvl, 1 + (lvl / 30));
		}
	}

	else if (mut_ptr->which == MUT1_BR_FIRE)
	{
		msgf("You breathe fire...");
		if (get_aim_dir(&dir))
		{
			(void)fire_ball(GF_FIRE, dir, lvl * 2, 1 + (lvl / 20));
		}
	}

	else if (mut_ptr->which == MUT1_HYPN_GAZE)
	{
		msgf("Your eyes look mesmerizing...");
		if (get_aim_dir(&dir))
		{
			(void)charm_monster(dir, lvl);
		}
	}

	else if (mut_ptr->which == MUT1_TELEKINES)
	{
		msgf("You concentrate...");
		if (get_aim_dir(&dir))
		{
			fetch(dir, lvl * 10, TRUE);
		}
	}

	else if (mut_ptr->which == MUT1_VTELEPORT)
	{
		msgf("You concentrate...");
		teleport_player(10 + 4 * lvl);
	}
	else if (mut_ptr->which == MUT1_MIND_BLST)
	{
		msgf("You concentrate...");
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
		msgf("Radiation flows from your body!");
		(void)fire_ball(GF_NUKE, 0, (lvl * 2), 3 + (lvl / 20));
	}

	else if (mut_ptr->which == MUT1_VAMPIRISM)
	{
		int x, y, dummy;
		cave_type *c_ptr;

		/* Handle player fear */
		if (p_ptr->tim.afraid)
		{
			/* Message */
			msgf("You are too afraid!");
			return;
		}

		/* Only works on adjacent monsters */
		if (!get_rep_dir(&dir)) return;
		y = py + ddy[dir];
		x = px + ddx[dir];

		/* paranoia */
		if (!in_bounds2(x, y)) return;

		c_ptr = area(x, y);

		if (!(c_ptr->m_idx))
		{
			msgf("You bite into thin air!");
			return;
		}
		msgf("You grin and bare your fangs...");

		dummy = lvl * 2;

		if (drain_gain_life(dir, dummy))
		{
			/* Gain nutritional sustenance: 150/hp drained */
			/* A Food ration gives 5000 food points (by contrast) */
			/* Don't ever get more than "Full" this way */
			/* But if we ARE Gorged,  it won't cure us */
			dummy = p_ptr->food + MIN(5000, 100 * dummy);
			if (p_ptr->food < PY_FOOD_MAX)	/* Not gorged already */
				(void)set_food(dummy >= PY_FOOD_MAX ? PY_FOOD_MAX - 1 : dummy);
		}
		else
			msgf("Yechh. That tastes foul.");
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
		if (!in_bounds2(x, y)) return;

		c_ptr = area(x, y);

		if (cave_floor_grid(c_ptr))
		{
			msgf("You bite into thin air!");
			return;
		}
		else if (cave_perma_grid(c_ptr) || (c_ptr->feat == FEAT_MOUNTAIN))
		{
			msgf("Ouch!  This wall is harder than your teeth!");
			return;
		}
		else if (c_ptr->m_idx)
		{
			msgf("There's something in the way!");
			return;
		}
		else if (c_ptr->feat == FEAT_TREES)
		{
			msgf("You don't like the woody taste!");
			return;
		}
		else
		{
			if ((c_ptr->feat >= FEAT_CLOSED) && (c_ptr->feat <= FEAT_RUBBLE))
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
				msgf("This granite is very filling!");
				(void)set_food(p_ptr->food + 10000);
			}
		}
		(void)wall_to_mud(dir);

		/* Save old location */
		oy = py;
		ox = px;

		/* Move the player */
		py = y;
		px = x;

		/* Move the player */
		p_ptr->py = y;
		p_ptr->px = x;

		/* Notice movement */
		Term_move_player();

		if (!p_ptr->depth)
		{
			/* Scroll wilderness */
			p_ptr->wilderness_x = px;
			p_ptr->wilderness_y = py;
			move_wild();
		}

		lite_spot(px, py);
		lite_spot(ox, oy);

		/* Process fields under the player. */
		field_script(area(px, py), FIELD_ACT_PLAYER_ENTER, "");

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

	else if (mut_ptr->which == MUT1_DET_CURSE)
	{
		object_type *o_ptr;

		OBJ_ITT_START (p_ptr->inventory, o_ptr)
		{
			if (!o_ptr->k_idx) continue;
			if (!cursed_p(o_ptr)) continue;

			o_ptr->feeling = FEEL_CURSED;
		}
		OBJ_ITT_END;
	}

	else if (mut_ptr->which == MUT1_BERSERK)
	{
		if (!p_ptr->tim.shero)
		{
			(void)hp_player(30);
		}

		(void)inc_shero(rand_range(25, 50));
		(void)clear_afraid();
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
			(void)summon_specific(-1, px, py, lvl, SUMMON_BIZARRE1, FALSE,
								  TRUE, TRUE);
		}
	}

	else if (mut_ptr->which == MUT1_RESIST)
	{
		int num = lvl / 10;
		int dur = rand_range(20, 40);

		if (randint0(5) < num)
		{
			(void)inc_oppose_acid(dur);
			num--;
		}
		if (randint0(4) < num)
		{
			(void)inc_oppose_elec(dur);
			num--;
		}
		if (randint0(3) < num)
		{
			(void)inc_oppose_fire(dur);
			num--;
		}
		if (randint0(2) < num)
		{
			(void)inc_oppose_cold(dur);
			num--;
		}
		if (num)
		{
			(void)inc_oppose_pois(dur);
			num--;
		}
	}

	else if (mut_ptr->which == MUT1_EARTHQUAKE)
	{
		(void)earthquake(px, py, 10);
	}

	else if (mut_ptr->which == MUT1_EAT_MAGIC)
	{
		object_type *o_ptr;
		int lev;

		item_tester_hook = item_tester_hook_recharge;

		/* Get an item */
		q = "Drain which item? ";
		s = "You have nothing to drain.";

		o_ptr = get_item(q, s, (USE_INVEN | USE_FLOOR));

		/* Not a valid item */
		if (!o_ptr) return;

		lev = get_object_level(o_ptr);

		if (o_ptr->tval == TV_ROD)
		{
			if (o_ptr->pval > 0)
			{
				msgf("You can't absorb energy from a discharged rod.");
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
				msgf("There's no energy there to absorb!");
			}
			o_ptr->info |= OB_EMPTY;
		}

		if (p_ptr->csp > p_ptr->msp)
		{
			p_ptr->csp = p_ptr->msp;
		}

		/* Notice changes */
		notice_inven();
	}

	else if (mut_ptr->which == MUT1_WEIGH_MAG)
	{
		report_magics();
	}

	/* Fake a population explosion. */
	else if (mut_ptr->which == MUT1_STERILITY)
	{
		msgf("You suddenly have a headache!");
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
		if (!in_bounds2(x, y)) return;

		if (area(x, y)->m_idx)
		{
			py_attack(x, y);
			teleport_player(30);
		}
		else
		{
			msgf("You don't see any monster in this direction");
			message_flush();
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
		if (!in_bounds2(x, y)) return;

		c_ptr = area(x, y);

		if (!c_ptr->m_idx)
		{
			msgf("You sense no evil there!");
			return;
		}

		m_ptr = &m_list[c_ptr->m_idx];
		r_ptr = &r_info[m_ptr->r_idx];

		if (FLAG(r_ptr, RF_EVIL) &&
			!FLAG(r_ptr, RF_QUESTOR) && !FLAG(r_ptr, RF_UNIQUE))
		{
			/* Delete the monster, rather than killing it. */
			delete_monster_idx(c_ptr->m_idx);
			msgf
				("The evil creature vanishes in a puff of sulfurous smoke!");
		}
		else
		{
			msgf("Your invocation is ineffectual!");
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
		if (!in_bounds2(x, y)) return;

		c_ptr = area(x, y);

		if (!c_ptr->m_idx)
		{
			msgf("You wave your hands in the air.");
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
		msgf("RAAAAGHH!");
		msgf("You feel a fit of rage coming over you!");
		(void)inc_shero(10 + randint1(p_ptr->lev));
	}

	else if (mut_ptr->which == MUT2_COWARDICE)
	{
		if (!((FLAG(p_ptr, TR_RES_FEAR)) ||
				p_ptr->tim.hero || p_ptr->tim.shero))
		{
			disturb(FALSE);
			msgf("It's so dark... so scary!");
			(void)inc_afraid(rand_range(13, 40));
		}
	}

	else if (mut_ptr->which == MUT2_RTELEPORT)
	{
		if (!(FLAG(p_ptr, TR_RES_NEXUS)) &&
			!(p_ptr->muta1 & MUT1_VTELEPORT) &&
			!(FLAG(p_ptr, TR_NO_TELE)))
		{
			disturb(FALSE);

			/* Teleport player */
			msgf("Your position suddenly seems very uncertain...");
			message_flush();
			teleport_player(40);
		}
	}

	else if (mut_ptr->which == MUT2_ALCOHOL)
	{
		if (!(FLAG(p_ptr, TR_RES_CONF)) &&
			!(FLAG(p_ptr, TR_RES_CHAOS)))
		{
			disturb(FALSE);
			p_ptr->redraw |= PR_EXTRA;
			msgf("You feel a SSSCHtupor cOmINg over yOu... *HIC*!");
		}

		if (!(FLAG(p_ptr, TR_RES_CONF)))
		{
			(void)inc_confused(rand_range(15, 35));
		}

		if (!(FLAG(p_ptr, TR_RES_CHAOS)))
		{
			if (one_in_(20))
			{
				message_flush();
				if (one_in_(3)) (void)lose_all_info();
				else
					wiz_dark();
				teleport_player(100);
				wiz_dark();
				msgf("You wake up somewhere with a sore head...");
				msgf("You can't remember a thing, or how you got here!");
			}
			else
			{
				if (one_in_(3))
				{
					msgf("Thishcischs GooDSChtuff!");
					(void)inc_image(rand_range(150, 300));
				}
			}
		}
	}

	else if (mut_ptr->which == MUT2_HALLU)
	{
		if (!(FLAG(p_ptr, TR_RES_CHAOS)))
		{
			disturb(FALSE);
			p_ptr->redraw |= PR_EXTRA;
			(void)inc_image(rand_range(20, 70));
		}
	}

	else if (mut_ptr->which == MUT2_FLATULENT)
	{
		disturb(FALSE);

		msgf("BRRAAAP! Oops.");
		message_flush();
		(void)fire_ball(GF_POIS, 0, p_ptr->lev, 3);
	}

	else if ((mut_ptr->which == MUT2_PROD_MANA) &&
		!(FLAG(p_ptr, TR_NO_MAGIC)))
	{
		int dire = 0;
		disturb(FALSE);
		msgf("Magical energy flows through you! You must release it!");
		flush();
		message_flush();
		(void)get_hack_dir(&dire);
		(void)fire_ball(GF_MANA, dire, p_ptr->lev * 2, 3);
	}

	else if ((mut_ptr->which == MUT2_ATT_DEMON) &&
		!(FLAG(p_ptr, TR_NO_MAGIC)))
	{
		bool pet = (one_in_(6));

		if (summon_specific((pet ? -1 : 0), p_ptr->px, p_ptr->py,
							p_ptr->depth, SUMMON_DEMON, TRUE, FALSE, pet))
		{
			msgf("You have attracted a demon!");
			disturb(FALSE);
		}
	}

	else if (mut_ptr->which == MUT2_SPEED_FLUX)
	{
		disturb(FALSE);
		if (one_in_(2))
		{
			msgf("You feel less energetic.");
			if (p_ptr->tim.fast > 0)
			{
				(void)clear_fast();
			}
			else
			{
				(void)inc_slow(rand_range(10, 40));
			}
		}
		else
		{
			msgf("You feel more energetic.");
			if (p_ptr->tim.slow > 0)
			{
				(void)clear_slow();
			}
			else
			{
				(void)inc_fast(rand_range(10, 40));
			}
		}
		message_flush();
	}

	else if (mut_ptr->which == MUT2_BANISH_ALL)
	{
		disturb(FALSE);
		msgf("You suddenly feel almost lonely.");
		(void)banish_monsters(100);
		message_flush();
	}

	else if (mut_ptr->which == MUT2_EAT_LIGHT)
	{
		object_type *o_ptr;
		cave_type *c_ptr = area(p_ptr->px, p_ptr->py);

		msgf("A shadow passes over you.");
		message_flush();

		/* Absorb light from the current possition */
		if (c_ptr->info & CAVE_GLOW)
		{
			(void)hp_player(10);
		}

		o_ptr = &p_ptr->equipment[EQUIP_LITE];

		/* Absorb some fuel in the current lite */
		if (o_ptr->tval == TV_LITE)
		{
			/* Use some fuel (except on artifacts) */
			if (!(FLAG(o_ptr, TR_INSTA_ART)) && (o_ptr->timeout > 0))
			{
				/* Heal the player a bit */
				(void)hp_player(o_ptr->timeout / 20);

				/* Decrease life-span of lite */
				o_ptr->timeout /= 2;

				msgf("You absorb energy from your light!");

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

	else if ((mut_ptr->which == MUT2_ATT_ANIMAL) &&
		!(FLAG(p_ptr, TR_NO_MAGIC)))
	{
		bool pet = (one_in_(3));

		if (summon_specific((pet ? -1 : 0), p_ptr->px, p_ptr->py,
							p_ptr->depth, SUMMON_ANIMAL, TRUE, FALSE, pet))
		{
			msgf("You have attracted an animal!");
			disturb(FALSE);
		}
	}

	else if ((mut_ptr->which == MUT2_RAW_CHAOS) &&
		!(FLAG(p_ptr, TR_NO_MAGIC)))
	{
		disturb(FALSE);
		msgf("You feel the world warping around you!");
		message_flush();
		(void)fire_ball(GF_CHAOS, 0, p_ptr->lev, 8);
	}

	else if (mut_ptr->which == MUT2_NORMALITY)
	{
		if (!lose_mutation(0))
		{
			msgf("You feel oddly normal.");
		}
	}

	else if ((mut_ptr->which == MUT2_WRAITH) &&
		!(FLAG(p_ptr, TR_NO_MAGIC)))
	{
		disturb(FALSE);
		msgf("You feel insubstantial!");
		message_flush();
		(void)inc_wraith_form(rand_range(p_ptr->lev / 2, p_ptr->lev));
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
				if (FLAG(p_ptr, TR_SUST_STR)) sustained = TRUE;
				break;
			case A_INT:
				if (FLAG(p_ptr, TR_SUST_INT)) sustained = TRUE;
				break;
			case A_WIS:
				if (FLAG(p_ptr, TR_SUST_WIS)) sustained = TRUE;
				break;
			case A_DEX:
				if (FLAG(p_ptr, TR_SUST_DEX)) sustained = TRUE;
				break;
			case A_CON:
				if (FLAG(p_ptr, TR_SUST_CON)) sustained = TRUE;
				break;
			case A_CHR:
				if (FLAG(p_ptr, TR_SUST_CHR)) sustained = TRUE;
				break;
			default:
				msgf("Invalid stat chosen!");
				sustained = TRUE;
		}

		if (!sustained)
		{
			disturb(FALSE);
			msgf("You can feel yourself wasting away!");
			message_flush();
			(void)dec_stat(which_stat, rand_range(6, 12), 0);
		}
	}

	else if ((mut_ptr->which == MUT2_ATT_DRAGON) &&
		!(FLAG(p_ptr, TR_NO_MAGIC)))
	{
		bool pet = (one_in_(5));

		if (summon_specific((pet ? -1 : 0), p_ptr->px, p_ptr->py,
							p_ptr->depth, SUMMON_DRAGON, TRUE, FALSE, pet))
		{
			msgf("You have attracted a dragon!");
			disturb(FALSE);
		}
	}

	else if ((mut_ptr->which == MUT2_WEIRD_MIND) &&
		!(FLAG(p_ptr, TR_NO_MAGIC)))
	{
		if (p_ptr->tim.esp > 0)
		{
			msgf("Your mind feels cloudy!");
			(void)clear_tim_esp();
		}
		else
		{
			msgf("Your mind expands!");
			(void)inc_tim_esp(p_ptr->lev);
		}
	}

	else if ((mut_ptr->which == MUT2_NAUSEA) &&
				!(FLAG(p_ptr, TR_SLOW_DIGEST)))
	{
		disturb(FALSE);
		msgf("Your stomach roils, and you lose your lunch!");
		message_flush();
		(void)set_food(PY_FOOD_WEAK);
	}

	else if ((mut_ptr->which == MUT2_WALK_SHAD) &&
		!(FLAG(p_ptr, TR_NO_MAGIC)))
	{
		alter_reality();
	}

	else if (mut_ptr->which == MUT2_WARNING)
	{
		int danger_amount = 0;
		int monster;

		for (monster = 0; monster < m_max; monster++)
		{
			monster_type *m_ptr = &m_list[monster];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			/* Paranoia -- Skip dead monsters */
			if (!m_ptr->r_idx) continue;

			if (r_ptr->level >= p_ptr->lev)
			{
				danger_amount += r_ptr->level - p_ptr->lev + 1;
			}
		}

		if (danger_amount > 100)
			msgf("You feel utterly terrified!");
		else if (danger_amount > 50)
			msgf("You feel terrified!");
		else if (danger_amount > 20)
			msgf("You feel very worried!");
		else if (danger_amount > 10)
			msgf("You feel paranoid!");
		else if (danger_amount > 5)
			msgf("You feel almost safe.");
		else
			msgf("You feel lonely.");
	}

	else if ((mut_ptr->which == MUT2_INVULN) &&
		!(FLAG(p_ptr, TR_NO_MAGIC)))
	{
		disturb(FALSE);
		msgf("You feel invincible!");
		message_flush();
		(void)inc_invuln(rand_range(8, 16));
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

	else if ((mut_ptr->which == MUT2_HP_TO_SP) &&
		!(FLAG(p_ptr, TR_NO_MAGIC)))
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
		msgf("You trip over your own feet!");
		take_hit(randint1(p_ptr->rp.wt / 6), "tripping");

		message_flush();
		o_ptr = &p_ptr->equipment[EQUIP_WIELD];
		if ((o_ptr->k_idx) && !cursed_p(o_ptr))
		{
			msgf("You drop your weapon!");
			inven_drop(o_ptr, 1);
		}
	}
}

/* 
 * Constant mutation effects 
 *
 * Note that the commented out effects are actually handled in player_flags().
 */
void mutation_effect(void)
{
	if (p_ptr->muta1 & MUT1_HYPN_GAZE)
	{
		p_ptr->stat[A_WIS].add -= 1;
	}

	if (p_ptr->muta1 & MUT1_TELEKINES)
	{
		p_ptr->stat[A_CON].add -= 1;
	}

	if (p_ptr->muta1 & MUT1_MIND_BLST)
	{
		p_ptr->stat[A_STR].add -= 1;
	}

	if (p_ptr->muta1 & MUT1_RADIATION)
	{
		p_ptr->stat[A_CON].add -= 1;
	}

	if (p_ptr->muta1 & MUT1_SHRIEK)
	{
		p_ptr->stat[A_CHR].add -= 1;
	}
	
	if (p_ptr->muta1 & MUT1_ILLUMINE)
	{
		p_ptr->skills[SKILL_STL] -= 1;
	}

	if (p_ptr->muta1 & MUT1_BERSERK)
	{
		p_ptr->stat[A_WIS].add -= 1;
	}

	if (p_ptr->muta1 & MUT1_MIDAS_TCH)
	{
		p_ptr->stat[A_CHR].add -= 1;
		p_ptr->stat[A_WIS].add -= 1;
	}

	if (p_ptr->muta1 & MUT1_RESIST)
	{
		p_ptr->skills[SKILL_SAV] -= 10;
	}

	if (p_ptr->muta1 & MUT1_EARTHQUAKE)
	{
		p_ptr->stat[A_WIS].add -= 1;
	}

	if (p_ptr->muta1 & MUT1_STERILITY)
	{
		p_ptr->stat[A_INT].add -= 1;
	}

	if (p_ptr->muta1 & MUT1_DAZZLE)
	{
		p_ptr->skills[SKILL_STL] -= 1;
	}

	if (p_ptr->muta1 & MUT1_LASER_EYE)
	{
		p_ptr->skills[SKILL_SNS] -= 10;
	}

	if (p_ptr->muta1 & MUT1_COLD_TOUCH)
	{
		p_ptr->stat[A_DEX].add -= 1;
	}

	if (p_ptr->muta1 & MUT1_LAUNCHER)
	{
		p_ptr->stat[A_DEX].add -= 1;
	}

	if (p_ptr->muta2 & MUT2_SCOR_TAIL)
	{
		p_ptr->stat[A_CHR].add -= 2;
	}

	if (p_ptr->muta2 & MUT2_TRUNK)
	{
		p_ptr->stat[A_CHR].add -= 1;
	}

	if (p_ptr->muta2 & MUT2_TENTACLES)
	{
		p_ptr->stat[A_DEX].add += 1;
		p_ptr->stat[A_CHR].add -= 2;
	}

	if (p_ptr->muta2 & MUT2_WRAITH)
	{
		p_ptr->stat[A_CON].add -= 3;
	}

	if (p_ptr->muta2 & MUT2_INVULN)
	{
		p_ptr->stat[A_WIS].add -= 2;
	}


	/* Hyper Strength */
	if (p_ptr->muta3 & MUT3_HYPER_STR)
	{
		p_ptr->stat[A_STR].add += 4;
		p_ptr->stat[A_INT].add -= 1;
		p_ptr->stat[A_WIS].add -= 1;
	}

	/* Puny */
	if (p_ptr->muta3 & MUT3_PUNY)
	{
		p_ptr->stat[A_STR].add -= 4;
		p_ptr->stat[A_DEX].add += 2;
	}

	/* Living computer */
	if (p_ptr->muta3 & MUT3_HYPER_INT)
	{
		p_ptr->stat[A_INT].add += 4;
		p_ptr->stat[A_WIS].add += 4;
		/* p_ptr->flags[3] |= TR3_HURT_ELEC */
	}

	/* Moronic */
	if (p_ptr->muta3 & MUT3_MORONIC)
	{
		p_ptr->stat[A_INT].add -= 4;
		p_ptr->stat[A_WIS].add -= 4;
		/* p_ptr->flags[1] |= TR1_RES_FEAR */
		/* p_ptr->flags[1] |= TR1_RES_CONF */
	}

	if (p_ptr->muta3 & MUT3_RESILIENT)
	{
		p_ptr->stat[A_CON].add += 4;
	}

	if (p_ptr->muta3 & MUT3_XTRA_FAT)
	{
		p_ptr->stat[A_CON].add += 2;
		p_ptr->pspeed -= 2;
	}

	if (p_ptr->muta3 & MUT3_ALBINO)
	{
		p_ptr->stat[A_CON].add -= 4;
		/* p_ptr->flags[1] |= TR1_RES_DARK */
	}

	if (p_ptr->muta3 & MUT3_FLESH_ROT)
	{
		p_ptr->stat[A_CON].add -= 2;
		p_ptr->stat[A_CHR].add -= 1;
		/* p_ptr->flags[2] &= ~(TR2_REGEN); */
		/* Cancel innate regeneration */
	}

	if (p_ptr->muta3 & MUT3_SILLY_VOI)
	{
		p_ptr->stat[A_CHR].add -= 4;
	}

	if (p_ptr->muta3 & MUT3_BLANK_FAC)
	{
		p_ptr->stat[A_CHR].add -= 1;
		/* p_ptr->Flags3 |= TR2_SEE_INVIS; */
	}

	if (p_ptr->muta3 & MUT3_XTRA_EYES)
	{
		p_ptr->skills[SKILL_FOS] += 15;
		p_ptr->skills[SKILL_SNS] += 15;
		p_ptr->stat[A_CHR].add -= 1;
	}

	if (p_ptr->muta3 & MUT3_MAGIC_RES)
	{
		p_ptr->skills[SKILL_SAV] += (15 + (p_ptr->lev / 5));
	}

	if (p_ptr->muta3 & MUT3_XTRA_NOIS)
	{
		p_ptr->skills[SKILL_STL] -= 3;
	}

	if (p_ptr->muta3 & MUT3_INFRAVIS)
	{
		p_ptr->see_infra += 3;
	}

	if (p_ptr->muta3 & MUT3_XTRA_LEGS)
	{
		p_ptr->pspeed += 3;
		p_ptr->stat[A_DEX].add -= 1;
	}

	if (p_ptr->muta3 & MUT3_SHORT_LEG)
	{
		p_ptr->stat[A_CON].add += 1;
		p_ptr->pspeed -= 3;
	}

	if (p_ptr->muta3 & MUT3_ELEC_TOUC)
	{
		p_ptr->stat[A_CON].add -= 1;
		/* SET_FLAG(p_ptr, TR_SH_ELEC) */;
	}

	if (p_ptr->muta3 & MUT3_FIRE_BODY)
	{
		p_ptr->stat[A_DEX].add -= 1;
		/* SET_FLAG(p_ptr, TR_SH_FIRE) */;
		/* SET_FLAG(p_ptr, TR_LITE) */;
	}

	if (p_ptr->muta3 & MUT3_WART_SKIN)
	{
		p_ptr->stat[A_CHR].add -= 2;
		p_ptr->to_a += 5;
		p_ptr->dis_to_a += 5;
	}

	if (p_ptr->muta3 & MUT3_SCALES)
	{
		p_ptr->stat[A_CHR].add -= 1;
		p_ptr->to_a += 10;
		p_ptr->dis_to_a += 10;
	}

	if (p_ptr->muta3 & MUT3_IRON_SKIN)
	{
		p_ptr->stat[A_DEX].add -= 3;
		p_ptr->to_a += 25;
		p_ptr->dis_to_a += 25;
	}

	if (p_ptr->muta3 & MUT3_WINGS)
	{
		p_ptr->stat[A_CON].add -= 1;
		p_ptr->stat[A_CHR].add += 3;
		/* SET_FLAG(p_ptr, TR_FEATHER) */;
	}

	if (p_ptr->muta3 & MUT3_FEARLESS)
	{
		/* SET_FLAG(p_ptr, TR_RES_FEAR) */;
	}

	if (p_ptr->muta3 & MUT3_REGEN)
	{
		/* SET_FLAG(p_ptr, TR_REGEN) */;
	}

	if (p_ptr->muta3 & MUT3_ESP)
	{
		p_ptr->stat[A_CON].add -= 1;
		/* SET_FLAG(p_ptr, TR_TELEPATHY) */;
	}

	if (p_ptr->muta3 & MUT3_LIMBER)
	{
		p_ptr->stat[A_DEX].add += 3;
		p_ptr->stat[A_STR].add -= 1;
	}

	if (p_ptr->muta3 & MUT3_ARTHRITIS)
	{
		p_ptr->stat[A_DEX].add -= 3;
	}

	if (p_ptr->muta3 & MUT3_MOTION)
	{
		/* SET_FLAG(p_ptr, TR_FREE_ACT) */;
		p_ptr->skills[SKILL_STL] += 1;
	}

	if (p_ptr->muta3 & MUT3_ILL_NORM)
	{
		p_ptr->stat[A_CHR].add = 0;
	}
}
