/* File: feature.c */

/*
 * Copyright (c) 2006 Jeff Greene, Diego Gonzalez
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"


/*
 * Obtains the name of a feature.
 * Set add_prefix to TRUE if you want the name prefixed by an article.
 * Set get_mimic to TRUE if you want the fake name of the feature, if any.
 */
void feature_desc(char *buf, size_t max, u16b feat, bool add_prefix, bool get_mimic)
{
	const char *pref = "";
	char *name;
	char *pbuf;

	/* Get mimic feature, if requested */
	if (get_mimic) feat = f_info[feat].f_mimic;

	/* Paranoia */
	if (!f_info[feat].name)
	{
		my_strcpy(buf, "unknown feature", max);

		return;
	}

	/* Get the name */
	name = f_name + f_info[feat].name;

	/* Found special mark -- No prefix */
	if (name[0] == '~')
	{
		++name;
	}
	/* Want prefix */
	else if (add_prefix)
	{
		/* Check the first letter of the name for the right article */
		if (my_is_vowel((unsigned char)name[0])) pref = "an ";

		else pref = "a ";

		/* Ignore prefix if there is one already */
		if (prefix(name, "the ")) pref = "";

		/* Hack -- Handle shops */
		if (feat_ff1_match(feat, FF1_SHOP))
		{
			pref = "the entrance to the ";
		}
	}

	/* Hack -- Make room for the trailing null character */
	if (max > 0) --max;

	/* Go to the beginning of the buffer */
	pbuf = buf;

	/* Hack -- Append the prefix quickly */
	while (*pref && ((size_t)(pbuf - buf) < max)) *pbuf++ = *pref++;

	/* Hack -- Append the name quickly */
	while (*name && ((size_t)(pbuf - buf) < max)) *pbuf++ = *name++;

	/* Terminate the buffer */
	*pbuf = '\0';
}


/*
 * Provide adjustments to combat chances based on the player position and
 * nativity.
 *
 * If being_attacked is TRUE, we assume that chance is a monster's chance to
 * hit the player.
 * If being_attacked is FALSE, we assume that chance is the player's chance to
 * hit a monster.
 */
int feat_adjust_combat_for_player(int chance, bool being_attacked)
{
	feature_type *f_ptr = &f_info[cave_feat[p_ptr->py][p_ptr->px]];
	feature_lore *f_l_ptr = &f_l_list[cave_feat[p_ptr->py][p_ptr->px]];

	int bonus;

	/*No adjustments when the player is flying. */
	if (p_ptr->timed[TMD_FLYING]) return chance;

	/* Native player adjustment to combat */
	if (is_player_native(p_ptr->py, p_ptr->px))
	{
		/* Raw bonus to hit */
		bonus = f_ptr->native_to_hit_adj - 100;

		/*Mark the feature lore if the player understands*/
		if ((player_can_observe()) && (f_l_ptr->f_l_native_to_hit_adj < MAX_UCHAR))
		{
			f_l_ptr->f_l_native_to_hit_adj++;
		}
	}

	/*Non_native player adjustment to combat*/
	else
	{
		/* Raw bonus to hit */
		bonus = f_ptr->non_native_to_hit_adj - 100;

		/*Mark the feature lore if the player understands*/
		if ((player_can_observe()) && (f_l_ptr->f_l_non_native_to_hit_adj < MAX_UCHAR))
		{
			f_l_ptr->f_l_non_native_to_hit_adj++;
		}
	}

	/*
	 * Invert the bonus if the player is being attacked (we are adjusting
	 * monster's chance to hit)
	 */
	if (being_attacked) bonus = -bonus;

	/* Adjust chance to hit */
	chance = (chance * (100 + bonus)) / 100;

	return (chance);
}


/*
 * Provide adjustments to combat chances based on the player position and
 * nativity.
 *
 * If being_attacked is TRUE, we assume that chance is the player's chance to
 * hit the monster.
 * If being_attacked is FALSE, we assume that chance is the monster's chance to
 * hit the player
 */
int feat_adjust_combat_for_monster(const monster_type *m_ptr, int chance,
	bool being_attacked)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	feature_type *f_ptr = &f_info[cave_feat[m_ptr->fy][m_ptr->fx]];
	feature_lore *f_l_ptr = &f_l_list[cave_feat[m_ptr->fy][m_ptr->fx]];

	int bonus;

	/* Flying monsters get no change */
	if (m_ptr->mflag & (MFLAG_FLYING))
	{
		return (chance);
	}
	/* Monster_native adjustment to combat */
	else if (is_monster_native(m_ptr->fy, m_ptr->fx, r_ptr))
	{
		/* Raw bonus to hit */
		bonus = f_ptr->native_to_hit_adj - 100;

		/* Mark the monster lore */
		if((m_ptr->ml) && player_can_observe())
		{
			u32b native = f_info[cave_feat[m_ptr->fy][m_ptr->fx]].f_flags3;
			native &= r_ptr->r_native;
			l_ptr->r_l_native |= native;
		}

		/* Mark the feature lore if the player understands */
		if ((player_can_observe()) && (f_l_ptr->f_l_native_to_hit_adj < MAX_UCHAR))
		{
			f_l_ptr->f_l_native_to_hit_adj++;
		}
	}
	/* Non_native monster adjustment to combat */
	else
	{
		/* Raw bonus to hit */
		bonus = f_ptr->non_native_to_hit_adj - 100;

		/*Mark the feature lore if the player understands*/
		if ((player_can_observe()) && (f_l_ptr->f_l_non_native_to_hit_adj < MAX_UCHAR))
		{
			f_l_ptr->f_l_non_native_to_hit_adj++;
		}
	}

	/*
	 * Invert the bonus if the monster is being attacked (we are adjusting
	 * player's chance to hit)
	 */
	if (being_attacked) bonus = -bonus;

	/* Adjust chance to hit */
	chance = (chance * (100 + bonus)) / 100;

	return (chance);
}


/*
 * Find a secret at the specified location and change it according to
 * the state.
 */
void find_secret(int y, int x)
{
	feature_type *f_ptr;
	feature_lore *f_l_ptr;

	cptr text;

	/* Get feature */
	f_ptr = &f_info[cave_feat[y][x]];
	f_l_ptr = &f_l_list[cave_feat[y][x]];

	if (f_l_ptr->f_l_sights < MAX_UCHAR) f_l_ptr->f_l_sights++;

	/* Get the feature description */
	text = (f_text + f_ptr->f_text);

	if (player_has_los_bold(y, x) && strlen(text))
	{
		/* You have found something */
		msg_format("%s",text);
	}

	/* Change the location */
	cave_alter_feat(y, x, FS_SECRET);

	/* Disturb */
	disturb(0, 0);
}




/*
 * Take a feature, determine what that feature becomes
 * through applying the given action.
 */
u16b feat_state(u16b feat, int action)
{
	int i;

	/* Permanent stuff never gets changed */
	if (feat_ff1_match(feat, FF1_PERMANENT)) return (feat);

	/* Get the new feature */
	for (i = 0; i < MAX_FEAT_STATES; i++)
	{
		/* Found the action */
		if (f_info[feat].state[i].fs_action == action)
		{
			/* Return the new state */
			return (f_info[feat].state[i].fs_result);
		}
	}

	/* Try with the default action */
	return (f_info[feat].defaults);
}

/*
 * Determine the power of the given action. If the action isn't found we
 * return the value of the f_power field.
 */
u16b feat_state_power(u16b feat, int action)
{
	int i;

	/* Search the action  */
	for (i = 0; i < MAX_FEAT_STATES; i++)
	{
		/* Found the action */
		if (f_info[feat].state[i].fs_action == action)
		{
			/* Return the transition power */
			return (f_info[feat].state[i].fs_power);
		}
	}

	/* Try with the default power */
	return (f_info[feat].f_power);
}


/*
 * Determine the power of the given action.
 * Returns 0 if the feature doesn't have an explicit K: directive
 * for the given action
 */
u16b feat_state_explicit_power(u16b feat, int action)
{
	int i;

	/* Search the action  */
	for (i = 0; i < MAX_FEAT_STATES; i++)
	{
		/* Found the action */
		if (f_info[feat].state[i].fs_action == action)
		{
			/* Return the transition power */
			return (f_info[feat].state[i].fs_power);
		}
	}

	/* Not found */
	return (0);
}


/*
 * Fire a bolt at the player
 * Stop if we hit a monster, but this function should only fire with a clear shot.
 * Affect monsters and the player
 */
static void other_beam_or_bolt(int y, int x, int typ, int dam, bool beam)
{

	u32b flg = PROJECT_STOP | PROJECT_KILL | PROJECT_PLAY | PROJECT_EFCT| PROJECT_ITEM;

	if (beam) flg |= PROJECT_BEAM;

	/* Target the player with a bolt attack */
	(void)project(SOURCE_EFFECT, 0, y, x, p_ptr->py, p_ptr->px, dam, typ, flg, 0 , 0);
}

/*
 * Fire an orb at the player from a trap/effect
 * Affect monsters and the player
 */
static bool other_orb_or_ball(int y, int x, int typ, int rad, int dam, bool orb)
{
	int source_diameter = (orb ? (10 + rad * 10) : 0);

	/* Add the ball bitflags */
	u32b flg = PROJECT_BOOM | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_WALL | PROJECT_PLAY;

	/* Limit radius to nine (up to 256 grids affected) */
	if (rad > 9) rad = 9;

	/* Cast a ball */
	return (project(SOURCE_EFFECT, rad, y, x, p_ptr->py, p_ptr->px, dam, typ, flg,
	                0, source_diameter));
}


/*
 * Handle a smart trap firing at the player.
 * Note that the variables y and x are only be used for a real trap.
 */
u16b fire_trap_smart(int f_idx, int y, int x, byte mode)
{

	/*Careful, in MODE-DESCRIBE, this will be undefined*/
	effect_type *x_ptr = &x_list[cave_x_idx[y][x]];

	/* Get feature */
	feature_type *f_ptr = &f_info[f_idx];
	feature_lore *f_l_ptr = &f_l_list[f_idx];

	/*paranoia*/
	if (!_feat_ff2_match(f_ptr, FF2_TRAP_SMART)) return (FALSE);

	/* Reveal the trap.  */
	if (mode == MODE_ACTION)
	{
		bool did_message = FALSE;

		char feat_name[80];

		feature_desc(feat_name, sizeof(feat_name), f_idx, FALSE, TRUE);

		/* Reveal it*/
		if (x_ptr->x_flags & (EF1_HIDDEN))
		{

			msg_format("A %s appears!", feat_name);

			/*Make sure the next message makes sense*/
			did_message = TRUE;

			/*No longer hidden*/
			x_ptr->x_flags &= ~(EF1_HIDDEN);

			/* Memorize */
 			cave_info[y][x] |= (CAVE_MARK);

			/*Light it up*/
			light_spot(y, x);
		}

		/* We have seen this feature */
		f_ptr->f_everseen = TRUE;

		/* Print the message*/
		if (did_message) msg_format("It %s", (f_text + f_ptr->f_text));
		else msg_format("The %s %s", feat_name, (f_text + f_ptr->f_text));

		/*Count in the feature lore the number of times set off*/
		if (f_l_ptr->f_l_power < MAX_UCHAR)
		{
			f_l_ptr->f_l_power++;
		}

		/* Disturb the player */
 		disturb(0, 0);
	}

	/* Don't describe if not set off*/
	if ((mode == MODE_DESCRIBE) && (!f_l_ptr->f_l_power))
	{
		text_out("  The effects of this trap are unknown.");
		return(FALSE);
	}

	/* Analyze XXX XXX XXX */
	switch (f_ptr->f_power)
	{
		/*Bolts*/
		case 1:  /*Magic Missle Trap*/
		case 3:  /*Arrow Trap*/
		{

			if (mode == MODE_DESCRIBE)
			{
				text_out("  If there is a clear line of sight to the player, this rune fires");
				if (f_ptr->f_power == 1) text_out(" a magic missle");
				else if (f_ptr->f_power == 3) text_out("  an arrow");
				text_out(" at the player");
				if (f_l_ptr->f_l_power > 20)
				{
					text_out(format(" as often as every %dd%d turns", f_ptr->x_timeout_set, f_ptr->x_timeout_rand));
				}
				text_out(".");
				return(TRUE);
			}
			if (mode == MODE_ACTION)
			{

				other_beam_or_bolt(y, x, f_ptr->x_gf_type, x_ptr->x_power, FALSE);
				return (TRUE);
			}
			else if (mode == MODE_FLAGS) return (EF1_SM_TRAP_BOLT);
			break;
		}
		case 6:  /*Lightning Trap*/
		{

			if (mode == MODE_DESCRIBE)
			{
				text_out("  When the player is in line of sight, this rune fires");
				if (f_ptr->f_power == 6) text_out(" a bolt of lightning");
				text_out(" at the player");
				if (f_l_ptr->f_l_power > 20)
				{
					text_out(format(" as often as every %dd%d turns", f_ptr->x_timeout_set, f_ptr->x_timeout_rand));
				}
				text_out(".");
				return(TRUE);

			}
			if (mode == MODE_ACTION)
			{

				other_beam_or_bolt(y, x, f_ptr->x_gf_type, x_ptr->x_power, TRUE);
				return (TRUE);
			}
			else if (mode == MODE_FLAGS) return (EF1_SM_TRAP_BEAM);
			break;
		}
		/*Orbs*/
		case 2:   /*Holy Orb Trap*/
		{

			if (mode == MODE_DESCRIBE)
			{
				text_out("  When the player is in line of sight, this rune fires");
				if (f_ptr->f_power == 2) text_out(" a holy orb");
				text_out(" at the player");
				if (f_l_ptr->f_l_power > 20)
				{
					text_out(format(" as often as every %dd%d turns", f_ptr->x_timeout_set, f_ptr->x_timeout_rand));
				}
				text_out(".");
				return(TRUE);

			}
			if (mode == MODE_ACTION)
			{
				other_orb_or_ball(y, x, f_ptr->x_gf_type, 2, x_ptr->x_power, TRUE);
				return (TRUE);
			}
			else if (mode == MODE_FLAGS) return (EF1_SM_TRAP_BALL);
			break;
		}
		/*Ball spells*/
		case 4:   /* poison ball trap*/
		case 5:   /* fire ball trap*/
		case 7:   /* frost ball trap*/
		case 8:   /* acid ball trap*/
		case 9:   /* nether ball trap*/
		case 10:  /* nexus ball trap*/
		case 13:  /* Shards Trap*/
		case 17:  /* Time Trap*/
		case 20:  /* Meteor Trap*/
		case 21:  /* Plasma Trap*/
		case 22:  /* Disenchantment Trap*/
		case 23:  /* Static Trap */
		{

			if (mode == MODE_DESCRIBE)
			{
				text_out("  When the player is in line of sight, this rune fires");

				if (f_ptr->f_power == 4) text_out(" a poison ball");
				else if (f_ptr->f_power == 5) text_out(" a fire ball");
				else if (f_ptr->f_power == 7) text_out(" a frost ball");
				else if (f_ptr->f_power == 8) text_out(" an acid ball");
				else if (f_ptr->f_power == 9) text_out(" a nether ball");
				else if (f_ptr->f_power == 10) text_out(" a nexus ball");
				else if (f_ptr->f_power == 13) text_out(" a ball of shards");
				else if (f_ptr->f_power == 17) text_out(" a time ball");
				else if (f_ptr->f_power == 20) text_out(" a meteor");
				else if (f_ptr->f_power == 21) text_out(" a ball of plasma");
				else if (f_ptr->f_power == 22) text_out(" a ball of disenchantment");
				else if (f_ptr->f_power == 23) text_out(" a ball of static");
				text_out(" at the player");

				if (f_l_ptr->f_l_power > 20)
				{
					text_out(format(" as often as every %dd%d turns", f_ptr->x_timeout_set, f_ptr->x_timeout_rand));
				}
				text_out(".");
				return(TRUE);
			}
			if (mode == MODE_ACTION)
			{
				other_orb_or_ball(y, x, f_ptr->x_gf_type, 2, x_ptr->x_power, FALSE);

				return (TRUE);
			}
			else if (mode == MODE_FLAGS) return (EF1_SM_TRAP_BALL);
			break;
		}
		/* LOS */
		case 11: 	/*Confusion trap*/
		case 12:	/*Sound trap*/
		case 14:   /* Gravity Trap*/
		case 15:   /*Aggravate Monsters Trap*/
		case 16:   /* Inertia Trap*/
		case 18:   /* Light Trap*/
		case 19:   /* Darnkess Trap*/
		{

			if (mode == MODE_DESCRIBE)
			{
				text_out("  When the player is in line of sight, this rune");

				if (f_ptr->f_power == 11) text_out(" attempts to confuse every being in sight");
				else if (f_ptr->f_power == 12) text_out(" surrounds the area with a deafening blast");
				else if (f_ptr->f_power == 14) text_out(" hits the area with a blast of gravity");
				else if (f_ptr->f_power == 15) text_out(" aggravates all creatures within line of sight");
				else if (f_ptr->f_power == 16) text_out(" hits the area with a blast of inertia");
				else if (f_ptr->f_power == 18) text_out(" releases a blinding light in the surrounding area");
				else if (f_ptr->f_power == 19) text_out(" covers the surrounding area with darkness");

				if (f_l_ptr->f_l_power > 20)
				{
					text_out(format(" as often as every %dd%d turns", f_ptr->x_timeout_set, f_ptr->x_timeout_rand));
				}
				text_out(".");
				return(TRUE);
			}
			if (mode == MODE_ACTION)
			{
				bool hurt_player = TRUE;

				/* Special handling of light trap */
				if (f_ptr->f_power == 18)
				{
					light_room(p_ptr->py, p_ptr->px);

				}


				/* Special handling of darkness trap */
				else if (f_ptr->f_power == 19)
				{
					unlight_room(p_ptr->py, p_ptr->px);

				}

				/*Special handling of confusion, it isn't supposed to damage the player*/
				else if (f_ptr->f_power == 11)
				{
					hurt_player = FALSE;

					if (allow_player_confusion())
					{
						(void)inc_timed(TMD_CONFUSED, rand_int(4) + 4 + x_ptr->x_power / 10, TRUE);
					}
				}

				/* Mass aggravation does nothing to the player*/
				else if (f_ptr->f_power == 15)
				{
					hurt_player = FALSE;
				}

				/*Affect the player for all others*/
				if (hurt_player) project_p(SOURCE_TRAP, p_ptr->py, p_ptr->px, x_ptr->x_power, f_ptr->x_gf_type, NULL);

				/* affect all monsters within LOS*/
				(void)project_los(y, x, x_ptr->x_power, f_ptr->x_gf_type);
				return (TRUE);
			}
			else if (mode == MODE_FLAGS) return (EF1_SM_TRAP_LOS);
			break;
		}


		/*Oops!*/
		default:
			msg_print("unknown trap type");

			break;
	}

	return(TRUE);
}


/*
 * Handle player hitting a real trap.
 * Note that the variables y and x can only be used for a real trap.
 */
void hit_trap(int f_idx, int y, int x, byte mode)
{
	int dice,sides, reps, dam, i, num;

	cptr name = "a trap";

	/* Get feature */
	const feature_type *f_ptr = &f_info[f_idx];
	feature_lore *f_l_ptr = &f_l_list[f_idx];

	/*paranoia*/
	if (!_feat_ff2_match(f_ptr, FF2_TRAP_PASSIVE)) return;

	if (mode == MODE_ACTION)
	{

		/* Get the trap effect */
		effect_type *x_ptr = &x_list[cave_x_idx[y][x]];

		if (p_ptr->timed[TMD_FLYING])
		{
			char feat_name[80];

			/* Get the feature name */
			feature_desc(feat_name, sizeof(feat_name), f_idx, FALSE, TRUE);

			msg_format("You float over the %s.", feat_name);

			/* Disturb the player */
			 disturb(0, 0);

			/*We are done here*/
			return;
		}


 		/* Make it visible if necessary. Note paranoia check */
 		if ((x_ptr->x_f_idx == f_idx) &&
 			(x_ptr->x_flags & (EF1_HIDDEN)))
		{
			/* Now visible */
 			x_ptr->x_flags &= ~(EF1_HIDDEN);

			/* Memorize */
 			note_spot(y, x);

 			/* Redraw */
			light_spot(y, x);
		}

		/*Count in the feature lore the number of times set off*/
		if (f_l_ptr->f_l_power < MAX_UCHAR)
		{
			f_l_ptr->f_l_power++;

			/* Disturb the player */
 			disturb(0, 0);
		}
	}

	/* Don't describe if not set off*/
	if ((mode == MODE_DESCRIBE) && (!f_l_ptr->f_l_power))
	{
		text_out("  The effects of this trap are unknown.");
		return;
	}

	/* Analyze XXX XXX XXX */
	switch (f_ptr->f_power)
	{
		case 0:
 		{
			int dice2 = 2;
			int sides2 = 3;
			dice = 2;
			sides = 6;
			reps = 5;

			if (mode == MODE_DESCRIBE)
			{
				text_out(format("  This pit will cause you %dd%d damage.", dice, sides));
				text_out(format("  Daggers will cut you up to %d times for %dd%d turns", reps, dice2, sides2));
				text_out(" each as you fall.");
				return;
			}

			if (mode == MODE_ACTION)
			{
				if (p_ptr->state.ffall)
				{
					msg_print("You float gently to the floor of the pit.");
					msg_print("You carefully avoid setting off the daggers.");
				}

				else
				{
					/* activate the ordinary daggers. */
					msg_print("Daggers pierce you everywhere!");

					/* Base damage */
					dam = damroll(dice, sides);

					msg_print("You are impaled!");

					for (i = 0; i < randint(reps); i++)
					{
						dam += damroll(dice2, sides2);
					}

					(void)set_cut(p_ptr->timed[TMD_CUT] + randint(dam));

					/* Take the damage */
					take_hit(dam, name);
				}
			}
			break;
		}

		case 1:
		{
			dice = 2;
			sides = 6;

			if (game_mode == GAME_NPPMORIA) sides = 8;

			if (mode == MODE_DESCRIBE)
			{
				text_out(format("  This trap door will cause you %dd%d damage when you fall through to the", dice, sides));
				text_out(" next level of the dungeon.");
				return;
			}

			if (mode == MODE_ACTION)
			{

				msg_print("You fall through a trap door!");
				if (p_ptr->state.ffall)
				{
					msg_print("You float gently down to the next level.");
				}
				else
				{
					dam = damroll(dice, sides);
					take_hit(dam, name);
				}

				/* New depth */
				dungeon_change_level(p_ptr->depth + 1);
			}

			break;

		}

		case 2:
		{
			dice = 2;
			sides = 6;

			if (mode == MODE_DESCRIBE)
			{
				text_out(format("  This pit will cause you %dd%d damage.", dice, sides));
				return;
			}
			if (mode == MODE_ACTION)
			{

				msg_print("You fall into a pit!");
				if (p_ptr->state.ffall)
				{
					msg_print("You float gently to the bottom of the pit.");
				}
				else
				{
					dam = damroll(dice, sides);
					take_hit(dam, name);
				}
			}
			break;
		}

		case 3:
		{

			dice = 2;
			sides = 6;


			if (mode == MODE_DESCRIBE)
			{
				text_out(format("  This spiked pit will cause you %dd%d damage when you fall into it.", dice, sides));
				text_out(format("  50% the time, The spikes will cut you up to (%dd%d) * 2 turns.", dice, sides));
				return;
			}

			if (mode == MODE_ACTION)
			{

				msg_print("You fall into a spiked pit!");

				if (p_ptr->state.ffall)
				{
					msg_print("You float gently to the floor of the pit.");
					msg_print("You carefully avoid touching the spikes.");
				}

				else
				{
					/* Base damage */
					dam = damroll(dice, sides);

					/* Extra spike damage */
					if (rand_int(100) < 50)
					{
						msg_print("You are impaled!");

						dam = dam * 2;
						(void)set_cut(p_ptr->timed[TMD_CUT] + randint(dam));
					}

					/* Take the damage */
					take_hit(dam, name);
				}
			}
			break;
		}

		case 4:
		{
			int percentage = 50;

			dice = 2;
			sides = 6;


			if (mode == MODE_DESCRIBE)
			{
				text_out(format("  This poison spiked pit will cause you %dd%d damage when you fall into it.", dice, sides));
				text_out(format("  %d percent of the time, the spikes will cut you up to (%dd%d) * 2 turns", percentage, dice, sides));
				text_out(format("  as well as poison you up to (%dd%d) * 4 turns.", dice, sides));
				return;
			}

			if (mode == MODE_ACTION)
			{

				msg_print("You fall into a poison spiked pit!");

				if (p_ptr->state.ffall)
				{
					msg_print("You float gently to the floor of the pit.");
					msg_print("You carefully avoid touching the poison spikes.");
				}

				else
				{
					/* Base damage */
					dam = damroll(dice, sides);

					/* Extra spike damage */
					if (rand_int(100) < percentage)
					{
						msg_print("You are impaled on poisonous spikes!");

						dam = dam * 2;
						(void)set_cut(p_ptr->timed[TMD_CUT] + randint(dam));

						if (p_ptr->state.resist_pois || p_ptr->timed[TMD_OPP_POIS] || p_ptr->state.immune_pois)
						{
							msg_print("The poison does not affect you!");
						}
						else
						{
							dam = dam * 2;
							(void)inc_timed(TMD_POISONED, randint(dam), TRUE);
						}
					}

					/* Take the damage */
					take_hit(dam, name);
				}
			}

			break;
		}

		case 5:
		{
			int sum_base = 2;
			int sum_plus = 3;


			if (mode == MODE_DESCRIBE)
			{
				text_out(format("  This trap will attempt to summon between %d and %d creatures.",
								(sum_base + 1), (sum_base + sum_plus)));
				return;
			}

			if (mode == MODE_ACTION)
			{

				msg_print("You are enveloped in a cloud of smoke!");
				cave_info[y][x] &= ~(CAVE_MARK);

				/* Destroy the trap */
				delete_effect_idx(cave_x_idx[y][x]);

				/* Forget the trap */
				cave_info[y][x] &= ~(CAVE_MARK);

				num = sum_base + randint(sum_plus);
				for (i = 0; i < num; i++)
				{
					(void)summon_specific(y, x, p_ptr->depth, 0, MPLACE_OVERRIDE);
				}
			}
			break;
		}

		case 6:
		{
			int dist = 100;

			if (mode == MODE_DESCRIBE)
			{
				text_out(format("  This trap will teleport you up to %d squares away.", dist));
				return;
			}
			if (mode == MODE_ACTION)
			{

				msg_print("You hit a teleport trap!");
				teleport_player(dist, FALSE);
			}
			break;
		}

		case 7:
		{
			dice = 4;
			sides = 6;

			if (mode == MODE_DESCRIBE)
			{
				text_out(format("  This fire trap will envelope you in flames for %dd%d damage.", dice, sides));
				return;
			}
			if (mode == MODE_ACTION)
			{
				msg_print("You are enveloped in flames!");
				dam = damroll(dice, sides);
				fire_dam(dam, "a fire trap");
			}
			break;
		}

		case 8:
		{
			dice = 4;
			sides = 6;

			if (mode == MODE_DESCRIBE)
			{
				text_out(format("  This acid trap will splash you with acid for %dd%d damage.", dice, sides));
				return;
			}
			if (mode == MODE_ACTION)
			{
				msg_print("You are splashed with acid!");
				dam = damroll(dice, sides);
				acid_dam(dam, "an acid trap");
			}
			break;
		}

		case 9:
		{
			int duration = 20;
			dice = 1;
			sides = 4;

			if (mode == MODE_DESCRIBE)
			{
				text_out(format("  This trap fires a small dart that can cause you to be slowed for %d + %dd turns,",
			   			 duration, duration));
				text_out(format(" and cause %dd%d damage.", dice, sides));
				return;
			}
			if (mode == MODE_ACTION)
			{
				if (check_hit(125))
				{
					msg_print("A small dart hits you!");
					dam = damroll(dice, sides);
					take_hit(dam, name);
					(void)inc_timed(TMD_SLOW, rand_int(duration) + duration, TRUE);
				}
				else
				{
					msg_print("A small dart barely misses you.");
				}
			}
			break;
		}

		case 10:
		{
			dice = 1;
			sides = 4;

			if (mode == MODE_DESCRIBE)
			{
				text_out("  This trap fires a small dart that can drain your strength,");
				text_out(format(" and cause %dd%d damage.", dice, sides));
				return;
			}
			if (mode == MODE_ACTION)
			{

				if (check_hit(125))
				{
					msg_print("A small dart hits you!");
					dam = damroll(dice, sides);
					take_hit(dam, name);
					(void)do_dec_stat(A_STR);
				}
				else
				{
					msg_print("A small dart barely misses you.");
				}
			}
			break;
		}

		case 11:
		{
			dice = 1;
			sides = 4;

			if (mode == MODE_DESCRIBE)
			{
				text_out("  This trap fires a small dart that can drain your dexterity,");
				text_out(format(" and cause %dd%d damage.", dice, sides));
				return;
			}
			if (mode == MODE_ACTION)
			{

				if (check_hit(125))
				{
					msg_print("A small dart hits you!");
					dam = damroll(1, 4);
					take_hit(dam, name);
					(void)do_dec_stat(A_DEX);
				}
				else
				{
					msg_print("A small dart barely misses you.");
				}
			}
			break;
		}

		case 12:
		{
			dice = 1;
			sides = 4;

			if (mode == MODE_DESCRIBE)
			{
				text_out("  This trap fires a small dart that can drain your constitution,");
				text_out(format(" and cause %dd%d damage.", dice, sides));
				return;
			}
			if (mode == MODE_ACTION)
			{

				if (check_hit(125))
				{
					msg_print("A small dart hits you!");
					dam = damroll(1, 4);
					take_hit(dam, name);
					(void)do_dec_stat(A_CON);
				}
				else
				{
					msg_print("A small dart barely misses you.");
				}
			}
			break;
		}

		case 13:
		{
			int base = 25;
			int rand_base = 50;

			if (game_mode == GAME_NPPMORIA) base = 50;

			if (mode == MODE_DESCRIBE)
			{
				text_out(format("  This trap releases black gas that can blind you for %d + %dd turns.", base, rand_base));
				return;
			}
			if (mode == MODE_ACTION)
			{

				msg_print("You are surrounded by a black gas!");
				if (!p_ptr->state.resist_blind)
				{
					(void)inc_timed(TMD_BLIND,rand_int(rand_base) + base, TRUE);
				}
			}
			break;
		}

		case 14:
		{
			int base = 10;
			int rand_base = 20;

			if (game_mode == GAME_NPPMORIA)
			{
				base = 15;
				rand_base = 15;
			}


			if (mode == MODE_DESCRIBE)
			{
				text_out(format("  This trap releases gas that can confuse you for %d + %dd turns.", base, rand_base));
				return;
			}
			if (mode == MODE_ACTION)
			{

				msg_print("You are surrounded by a gas of scintillating colors!");
				if (allow_player_confusion())
				{
					(void)inc_timed(TMD_CONFUSED, rand_int(rand_base) + base, TRUE);
				}
			}
			break;
		}

		case 15:
		{
			int base = 10;
			int rand_base = 30;

			if (game_mode == GAME_NPPMORIA)
			{
				base = 2;
				rand_base = 10;
			}

			if (mode == MODE_DESCRIBE)
			{
				text_out(format("  This trap releases green gas that can poison you for %d + %dd turns.", base, rand_base));
				return;
			}
			if (mode == MODE_ACTION)
			{


				msg_print("You are surrounded by a pungent green gas!");
				if (!p_ptr->state.resist_pois && !p_ptr->timed[TMD_OPP_POIS] && !p_ptr->state.immune_pois)
				{
					(void)inc_timed(TMD_POISONED, rand_int(rand_base) + base, TRUE);
				}
			}
			break;
		}

		case 16:
		{
			int base = 5;
			int rand_base = 20;

			if (game_mode == GAME_NPPMORIA)
			{
				base = 4;
				rand_base = 10;
			}

			if (mode == MODE_DESCRIBE)
			{
				text_out(format("  This trap releases a white mist that can paralyze you for %d + %dd turns.",
									base, rand_base));
				return;
			}
			if (mode == MODE_ACTION)
			{
				msg_print("You are surrounded by a strange white mist!");
				if (!p_ptr->state.free_act)
				{
					(void)inc_timed(TMD_PARALYZED, rand_int(rand_base) + base, TRUE);
				}
			}
			break;
		}

		/*Oops!*/
		default:
			msg_print("unknown trap type");

			break;
	}

	if (mode == MODE_ACTION) disturb(0,0);
}


/*
 * Let an feature appear near a location.
 *
 * The initial location is assumed to be "in_bounds_fully()".
 *
 */
void feat_near(int feat, int y, int x)
{
	int d, s;

	int bs, bn;
	int by, bx;
	int dy, dx;
	int ty, tx;


	bool flag = FALSE;

	/* Score */
	bs = -1;

	/* Picker */
	bn = 0;

	/* Default */
	by = y;
	bx = x;

	/* Scan local grids */
	for (dy = -3; dy <= 3; dy++)
	{
		/* Scan local grids */
		for (dx = -3; dx <= 3; dx++)
		{
			/* Calculate actual distance */
			d = (dy * dy) + (dx * dx);

			/* Ignore distant grids */
			if (d > 10) continue;

			/* Location */
			ty = y + dy;
			tx = x + dx;

			/* Skip illegal grids */
			if (!in_bounds_fully(ty, tx)) continue;

			/* Require line of sight */
			if (!los(y, x, ty, tx)) continue;

			/* Prevent overwriting permanents */
			if (f_info[cave_feat[ty][tx]].f_flags1 & (FF1_PERMANENT)) continue;

			/* Don't like non-floor space */
			if (!(f_info[cave_feat[ty][tx]].f_flags1 & (FF1_FLOOR))) continue;

			/* Don't like objects */
			if (cave_o_idx[ty][tx]) continue;

			/* Calculate score */
			s = 1000 - (d - cave_feat[ty][tx]);

			/* Skip bad values */
			if (s < bs) continue;

			/* New best value */
			if (s > bs) bn = 0;

			/* Apply the randomizer to equivalent values */
			if ((++bn >= 2) && (rand_int(bn) != 0)) continue;

			/* Keep score */
			bs = s;

			/* Track it */
			by = ty;
			bx = tx;

			/* Okay */
			flag = TRUE;
		}
	}

	/* Give it to the floor */
	if (flag) cave_set_feat(by, bx, feat);
}


/*
 * Count which terrain has been seen on this level.
 * Clear the everseen flag for the next level.
 *
 */
void count_feat_everseen(void)
{
	int i;

	/* Delete the existing objects */
	for (i = 1; i < z_info->f_max; i++)
	{
		feature_type *f_ptr = &f_info[i];
		feature_lore *f_l_ptr = &f_l_list[i];

		/* Skip features not seen */
		if (!f_ptr->f_everseen) continue;

		/*Count the number of levels this character has been seen*/
		if (f_l_ptr->f_l_sights < MAX_UCHAR) f_l_ptr->f_l_sights ++;

		/*Clear it for the next level*/
		f_ptr->f_everseen = FALSE;
	}
}


/*
 * Helper function to find a smart trap
 */
static bool vault_trap_smart(int f_idx)
{
	feature_type *f_ptr = &f_info[f_idx];

	/* Decline non-traps */
	if (!_feat_ff1_match(f_ptr, FF1_TRAP)) return (FALSE);

	/* Decline invisible traps */
	if (_feat_ff3_match(f_ptr, FF3_PICK_TRAP)) return (FALSE);

	/* Decline passive traps */
	if (_feat_ff2_match(f_ptr, FF2_TRAP_PASSIVE)) return (FALSE);

	/* Decline traps set by player */
	if (_feat_ff2_match(f_ptr, FF2_TRAP_MON)) return (FALSE);

	/* Okay */
	return (TRUE);
}


/*
 * Helper function to find a passive trap
 */
static bool vault_trap_passive(int f_idx)
{
	feature_type *f_ptr = &f_info[f_idx];

	/* Decline non-traps */
	if (!_feat_ff1_match(f_ptr, FF1_TRAP)) return (FALSE);

	/* Decline invisible traps */
	if (_feat_ff3_match(f_ptr, FF3_PICK_TRAP)) return (FALSE);

	/* Decline passive traps */
	if (_feat_ff2_match(f_ptr, FF2_TRAP_SMART)) return (FALSE);

	/* Decline traps set by player */
	if (_feat_ff2_match(f_ptr, FF2_TRAP_MON)) return (FALSE);

	/* Okay */
	return (TRUE);
}


/*
 * Helper function to find any dungeon trap
 */
static bool vault_trap_all(int f_idx)
{
	feature_type *f_ptr = &f_info[f_idx];

	/* Decline non-traps */
	if (!_feat_ff1_match(f_ptr, FF1_TRAP)) return (FALSE);

	/* Decline invisible traps */
	if (_feat_ff3_match(f_ptr, FF3_PICK_TRAP)) return (FALSE);

	/* Decline traps set by player */
	if (_feat_ff2_match(f_ptr, FF2_TRAP_MON)) return (FALSE);

	/* Okay */
	return (TRUE);
}


/*
 * Helper function for "jammed doors"
 */
static bool vault_jammed_door(int f_idx)
{
	feature_type *f_ptr = &f_info[f_idx];

	/* Decline non-door */
	if (!(f_ptr->f_flags1 & (FF1_DOOR))) return (FALSE);

	/* Jammed doors */
	if (f_ptr->f_flags3 & (FF3_DOOR_JAMMED)) return (TRUE);

	/* Decline everything else */
	return (FALSE);
}


/*
 * Helper function for "secret doors"
 */
static bool vault_secret_door(int f_idx)
{
	feature_type *f_ptr = &f_info[f_idx];

	/* Decline non-door */
	if (!(f_ptr->f_flags1 & (FF1_DOOR))) return (FALSE);

	/* Secret doors */
	if (f_ptr->f_flags1 & (FF1_SECRET)) return (TRUE);

	/* Decline everything else */
	return (FALSE);
}


/*
 * Helper function for "closed doors"
 */
static bool vault_closed_door(int f_idx)
{
	feature_type *f_ptr = &f_info[f_idx];

	/* Decline non-door */
	if (!(f_ptr->f_flags1 & (FF1_DOOR))) return (FALSE);

	/* Closed doors */
	if (f_ptr->f_flags3 & (FF3_DOOR_CLOSED)) return (TRUE);

	/* Decline everything else */
	return (FALSE);
}


/*
 * Helper function for "opened doors"
 */
static bool vault_open_door(int f_idx)
{
	feature_type *f_ptr = &f_info[f_idx];

	/* Decline non-door */
	if (!(f_ptr->f_flags1 & (FF1_DOOR))) return (FALSE);

	/* Open doors */
	if (f_ptr->f_flags3 & (FF3_DOOR_OPEN)) return (TRUE);

	/* Decline everything else */
	return (FALSE);
}


/*
 * Helper function for "broken doors"
 */
static bool vault_broken_door(int f_idx)
{
	feature_type *f_ptr = &f_info[f_idx];

	/* Decline non-door */
	if (!(f_ptr->f_flags1 & (FF1_DOOR))) return (FALSE);

	/* Broken doors */
	if (f_ptr->f_flags3 & (FF3_DOOR_BROKEN)) return (TRUE);

	/* Decline everything else */
	return (FALSE);
}


/*
 * Helper function for "locked doors"
 */
bool vault_locked_door(int f_idx)
{
	feature_type *f_ptr = &f_info[f_idx];

	/* Decline non-door */
	if (!(f_ptr->f_flags1 & (FF1_DOOR))) return (FALSE);

	/* Locked doors */
	if (f_ptr->f_flags3 & (FF3_DOOR_LOCKED)) return (TRUE);

	/* Decline everything else */
	return (FALSE);
}


/*
 * Helper function for boring "closed doors"
 */
static bool vault_boring_closed_door(int f_idx)
{
	feature_type *f_ptr = &f_info[f_idx];

	/* Decline non-door */
	if (!_feat_ff1_match(f_ptr, FF1_DOOR)) return (FALSE);

	/* Decline non-closed doors */
	if (!_feat_ff3_match(f_ptr, FF3_DOOR_CLOSED)) return (FALSE);

	/* Decline interesting closed doors */
	if (_feat_ff3_match(f_ptr, FF3_DOOR_LOCKED | FF3_DOOR_JAMMED))
	{
		return (FALSE);
	}

	/* Accept */
	return (TRUE);
}


/*
 * Apply a "feature restriction function" to the "feature allocation table"
 */
errr get_feat_num_prep(void)
{
	int i;

	/* Get the entry */
	alloc_entry *table = alloc_feat_table;

	/* Scan the allocation table */
	for (i = 0; i < alloc_feat_size; i++)
	{
		/* Accept objects which pass the restriction, if any */
		if (!get_feat_num_hook || (*get_feat_num_hook)(table[i].index))
		{
			/* Accept this object */
			table[i].prob2 = table[i].prob1;
		}

		/* Do not use this object */
		else
		{
			/* Decline this object */
			table[i].prob2 = 0;
		}
	}

	/* Success */
	return (0);
}


/*
 * Choose an feature type that seems "appropriate" to the given level
 *
 * This function uses the "prob2" field of the "feature allocation table",
 * and various local information, to calculate the "prob3" field of the
 * same table, which is then used to choose an "appropriate" feature, in
 * a relatively efficient manner.
 *
 * It is (slightly) more likely to acquire a feature of the given level
 * than one of a lower level.  This is done by choosing several features
 * appropriate to the given level and keeping the "hardest" one.
 *
 * Note that if no features are "appropriate", then this function will
 * fail, and return zero, but this should *almost* never happen.
 * Happening all the time for trapped doors.
 */
s16b get_feat_num(int level)
{
	int i, j, p;

	int f_idx;

	long value, total;

	alloc_entry *table = alloc_feat_table;

	/* Save the level */
	int old_level = level;

	/* Boost level */
	if (level > 0)
	{
		/* Occasional "boost" */
		if (one_in_(40))
		{

			/* 10-20 levels boost */
			/*level += (10 + rand_int(11));*/

			/* 25-40 levels boost */
			level += (25 + rand_int(16));
		}
	}

	/* Reset total */
	total = 0L;

	/* Process probabilities */
	for (i = 0; i < alloc_feat_size; i++)
	{

		/* Default */
		table[i].prob3 = 0;

		/* Features are not sorted by depth */
		if (table[i].level > level) break;

		/* Get the index */
		f_idx = table[i].index;

		/* Limit the power of OOD features */
		 if ((f_info[f_idx].f_level > (old_level + 10)) &&
			 (!feat_ff2_match(f_idx, FF2_LAKE | FF2_RIVER) ||
			(f_info[f_idx].dam_non_native > (p_ptr->mhp / 2)))) continue;

		/*
		 * Mega-Hack -- No summoning traps in themed levels,
		 * labyrinth levels, wilderness levels or arena levels
		 */
		if (feat_ff2_match(f_idx, FF2_TRAP_PASSIVE) &&
			(f_info[f_idx].f_power == 5) && (*dun_cap->limited_level_summoning)())
		{
			continue;
		}

		/* Hack -- no up stairs on certain levels */
		if (feat_ff1_match(f_idx, FF1_LESS))
		{
			/* No up stairs in the town */
			if (!p_ptr->depth) continue;

			/* No up shafts in level 1 */
			if ((p_ptr->depth < 2) && feat_ff2_match(f_idx, FF2_SHAFT)) continue;
		}

		/* Hack -- no chasm/trap doors/down stairs on certain levels */
		if (feat_ff1_match(f_idx, FF1_MORE))
		{
			/* Cache the quest type at the current depth */
			int what_quest_type = quest_check(p_ptr->depth);

			/* Verify if the feature is a down shaft */
			bool is_shaft = (feat_ff2_match(f_idx, FF2_SHAFT) != 0);

			/* Check max depth */
			if (p_ptr->depth >= (MAX_DEPTH - (is_shaft ? 2: 1))) continue;

			/* Stairs have special rules */
			if (feat_ff1_match(f_idx, FF1_STAIRS))
			{
				if (no_down_stairs(p_ptr->depth)) continue;

				/* Shafts shouldn't pierce quest levels */
				if (is_shaft && quest_check(p_ptr->depth + 1)) continue;
			}
			/* Trap doors don't get along with quests */
			else if (what_quest_type) continue;
		}

		/* Accept */
		table[i].prob3 = table[i].prob2;

		/* Total */
		total += table[i].prob3;
	}

	/* No legal features */
	if (total <= 0) return (0);


	/* Pick a feature */
	value = rand_int(total);

	/* Find the feature */
	for (i = 0; i < alloc_feat_size; i++)
	{
		/* Found the entry */
		if (value < table[i].prob3) break;

		/* Decrement */
		value = value - table[i].prob3;
	}

	/* Power boost */
	p = rand_int(100);

	/* Try for a "better" object once (50%) or twice (10%) */
	if (p < 60)
	{
		/* Save old */
		j = i;

		/* Pick a object */
		value = rand_int(total);

		/* Find the monster */
		for (i = 0; i < alloc_feat_size; i++)
		{
			/* Found the entry */
			if (value < table[i].prob3) break;

			/* Decrement */
			value = value - table[i].prob3;
		}

		/* Keep the "best" one */
		if (table[i].level < table[j].level) i = j;
	}

	/* Try for a "better" object twice (10%) */
	if (p < 10)
	{
		/* Save old */
		j = i;

		/* Pick a object */
		value = rand_int(total);

		/* Find the object */
		for (i = 0; i < alloc_feat_size; i++)
		{
			/* Found the entry */
			if (value < table[i].prob3) break;

			/* Decrement */
			value = value - table[i].prob3;
		}

		/* Keep the "best" one */
		if (table[i].level < table[j].level) i = j;
	}

	/* Result */
	return (table[i].index);
}


/*
 * Hack -- pick and create a trap.  Allow specification of smart or dumb traps.
 *
 * Any call to this function should check to limit traps, such as the check for "trap doors" on quest levels.
 */
u16b pick_trap(int y, int x, byte mode)
{
	u16b feat = 0;

	/* No smart traps in Moria */
	if (game_mode == GAME_NPPMORIA) mode = EFFECT_TRAP_DUMB;

	/* Set hook*/

	if (mode == EFFECT_TRAP_SMART) get_feat_num_hook = vault_trap_smart;
	else if (mode == EFFECT_TRAP_DUMB) get_feat_num_hook = vault_trap_passive;
	else get_feat_num_hook = vault_trap_all;

	get_feat_num_prep();

	/* Pick a trap */
	while (1)
	{
		/* Pick the trap */
		feat = get_feat_num(effective_depth(p_ptr->depth));

		/*Special handling of trap doors*/
		if (feat == FEAT_TRAP_DOOR)
		{
			/* HACK - no trap doors on quest levels  */
			if (quest_check(p_ptr->depth)) continue;

			/* Hack -- no trap doors on the deepest level */
			if (p_ptr->depth >= MAX_DEPTH-1) continue;
		}

		/*Hack - no summoning traps on themed levels*/
		if ((feat == FEAT_TRAP_SUMMON) && (feeling >= LEV_THEME_HEAD)) continue;

		/* Done */
		break;
	}

	/* Clear hook */
	get_feat_num_hook = NULL;

	get_feat_num_prep();

	/* More paranoia */
	return (feat);

}


u16b get_secret_door_num(void)
{
	u16b feat;

	/* Set the hook */
	get_feat_num_hook = vault_secret_door;

	get_feat_num_prep();

	/* Get the door */
	feat = get_feat_num(effective_depth(p_ptr->depth));

	/* Clear the hook */
	get_feat_num_hook = NULL;

	get_feat_num_prep();

	return (feat);
}


/*
 * Place a secret door at the given location
 */
void place_secret_door(int y, int x)
{
	u16b feat = get_secret_door_num();

	/* More paranoia */
	if (!feat) return;

	/* Set the door */
	cave_set_feat(y, x, feat);
}


/*
 * Place a random type of closed door at the given location.
 */
void place_closed_door(int y, int x)
{
	u16b feat;

	/* Set the hook */
	get_feat_num_hook = vault_closed_door;

	get_feat_num_prep();

	/* Get the door */
	feat = get_feat_num(effective_depth(p_ptr->depth));

	/* Clear the hook */
	get_feat_num_hook = NULL;

	get_feat_num_prep();

	/* More paranoia */
	if (!feat) return;

	/* Set the door */
	cave_set_feat(y, x, feat);
}


/*
 * Place a random type of boring closed door at the given location.
 */
void place_boring_closed_door(int y, int x)
{
	u16b feat;

	/* Set the hook */
	get_feat_num_hook = vault_boring_closed_door;

	get_feat_num_prep();

	/* Get the door */
	feat = get_feat_num(effective_depth(p_ptr->depth));

	/* Clear the hook */
	get_feat_num_hook = NULL;

	get_feat_num_prep();

	/* More paranoia */
	if (!feat) return;

	/* Set the door */
	cave_set_feat(y, x, feat);
}


/*
 * Place a random type of closed door at the given location.
 */
void place_open_door(int y, int x)
{
	u16b feat;

	/* Set the hook */
	get_feat_num_hook = vault_open_door;

	get_feat_num_prep();

	/* Get the door */
	feat = get_feat_num(effective_depth(p_ptr->depth));

	/* Clear the hook */
	get_feat_num_hook = NULL;

	get_feat_num_prep();

	/* More paranoia */
	if (!feat) return;

	/* Set the door */
	cave_set_feat(y, x, feat);
}


/*
 * Place a random type of closed door at the given location.
 */
void place_broken_door(int y, int x)
{
	u16b feat;

	/* Set the hook */
	get_feat_num_hook = vault_broken_door;

	get_feat_num_prep();

	/* Get the door */
	feat = get_feat_num(effective_depth(p_ptr->depth));

	/* Clear the hook */
	get_feat_num_hook = NULL;

	get_feat_num_prep();

	/* More paranoia */
	if (!feat) return;

	/* Set the door */
	cave_set_feat(y, x, feat);
}


/*
 * Place a random type of closed door at the given location.
 */
void place_locked_door(int y, int x)
{
	u16b feat;

	/* Set the hook */
	get_feat_num_hook = vault_locked_door;

	get_feat_num_prep();

	/* Get the door */
	feat = get_feat_num(effective_depth(p_ptr->depth));

	/* Clear the hook */
	get_feat_num_hook = NULL;

	get_feat_num_prep();

	/* More paranoia */
	if (!feat) return;

	/* Set the door */
	cave_set_feat(y, x, feat);
}


/*
 * Place a random type of jammed door at the given location.
 */
void place_jammed_door(int y, int x)
{
	u16b feat;

	/* Set the hook */
	get_feat_num_hook = vault_jammed_door;

	get_feat_num_prep();

	/* Get the door */
	feat = get_feat_num(effective_depth(p_ptr->depth));

	/* Clear the hook */
	get_feat_num_hook = NULL;

	get_feat_num_prep();

	/* More paranoia */
	if (!feat) return;

	/* Set the door */
	cave_set_feat(y, x, feat);
}


/*
 * Place a random type of door at the given location.
 */
void place_random_door(int y, int x)
{
	int tmp;

	/* Choose an object */
	tmp = rand_int(1000);

	/* Open doors (300/1000) */
	if (tmp < 300)
	{
		/* Create open door */
		place_open_door(y, x);
	}

	/* Broken doors (100/1000) */
	else if (tmp < 400)
	{
		/* Create broken door */
		place_broken_door(y, x);
	}

	/* Secret doors (200/1000) */
	else if (tmp < 600)
	{
		/* Create secret door */
		place_secret_door(y, x);
	}

	/* Closed, locked, or stuck doors (400/1000) */
	else
	{
		/* Create closed door */
		place_closed_door(y, x);
	}
}


void lore_do_probe_feature(int f_idx)
{
	feature_type *f_ptr = &f_info[f_idx];
	feature_lore *f_l_ptr = &f_l_list[f_idx];

	int i;

	i = randint (3);

	/*learn 1 out of three terrain flags.....*/
	switch (i)
	{
		case 1:
		{
			f_l_ptr->f_l_flags1 = f_ptr->f_flags1;
			break;
		}
		case 2:
		{
			f_l_ptr->f_l_flags2 = f_ptr->f_flags2;
			break;
		}

		default:
		{
			f_l_ptr->f_l_flags3 = f_ptr->f_flags3;
			break;
		}
	}

	/* Hack -- Maximal sightings 50% of the time*/
	if (one_in_(2)) f_l_ptr->f_l_sights = MAX_UCHAR;

	/* Observe all transitions another 50% of the time*/
	else
	{
		for (i = 0; i < MAX_FEAT_STATES; i++)
		{
			/*There isn't an action here*/
			if (f_ptr->state[i].fs_action == FS_FLAGS_END) continue;

			/* Hack -- we have seen this transition */
			f_l_ptr->f_l_state[i] = MAX_UCHAR;
		}

		/* Hack -- Maximal info */
		f_l_ptr->f_l_defaults = MAX_UCHAR;
	}

	/*Other 33% of the time, learn all combat, movement and stealth*/
	i = randint (3);

	/*learn either movement, damage to non-native, or stealth.....*/
	switch (i)
	{
		case 1:
		{
			f_l_ptr->f_l_dam_non_native = MAX_UCHAR;
			break;
		}
		case 2:
		{
			f_l_ptr->f_l_native_moves = MAX_UCHAR;
			f_l_ptr->f_l_non_native_moves = MAX_UCHAR;
			f_l_ptr->f_l_stealth_adj = MAX_UCHAR;
			break;
		}
		default:
		{
			f_l_ptr->f_l_native_to_hit_adj = MAX_UCHAR;
			f_l_ptr->f_l_non_native_to_hit_adj = MAX_UCHAR;
			break;
		}

	}
}


/*
 * Learn everything about a feature (by cheating)
 */
static void cheat_feature_lore(int f_idx, feature_lore *f_l_ptr)
{
	const feature_type *f_ptr = &f_info[f_idx];

	int i;

	/* Hack -- Maximal sightings */
	f_l_ptr->f_l_sights = MAX_UCHAR;

	/* Hack -- Maximal info */
	f_l_ptr->f_l_defaults = MAX_UCHAR;

	/* Observe "maximal" attacks */
	for (i = 0; i < MAX_FEAT_STATES; i++)
	{
		/*There isn't an action here*/
		if (f_ptr->state[i].fs_action == FS_FLAGS_END) continue;

		/* Hack -- we have seen this transition */
		f_l_ptr->f_l_state[i] = MAX_UCHAR;
	}

	/* Hack -- maximal uses of power */
	f_l_ptr->f_l_power = MAX_UCHAR;

	/*Hack -- Have seen all changes to movement, stealth, and combat*/
	f_l_ptr->f_l_dam_non_native = MAX_UCHAR;
	f_l_ptr->f_l_native_moves = MAX_UCHAR;
	f_l_ptr->f_l_non_native_moves = MAX_UCHAR;
	f_l_ptr->f_l_native_to_hit_adj = MAX_UCHAR;
	f_l_ptr->f_l_non_native_to_hit_adj = MAX_UCHAR;
	f_l_ptr->f_l_stealth_adj = MAX_UCHAR;

	/* Hack -- know all the flags */
	f_l_ptr->f_l_flags1 = f_ptr->f_flags1;
	f_l_ptr->f_l_flags2 = f_ptr->f_flags2;
	f_l_ptr->f_l_flags3 = f_ptr->f_flags3;

}


/*
 * Return a string that describes the type of a feature
 */
static cptr get_feature_type(const feature_lore *f_l_ptr)
{
	if (f_l_ptr->f_l_flags1 & FF1_SHOP)		return (" shop entrance");
	else if (f_l_ptr->f_l_flags1 & FF1_DOOR)	return (" door");
	else if (f_l_ptr->f_l_flags1 & FF1_TRAP)	return (" trap");
	else if (f_l_ptr->f_l_flags1 & FF1_STAIRS)	return (" staircase");
	else if (f_l_ptr->f_l_flags1 & FF1_GLYPH)	return (" glyph");
	else if (f_l_ptr->f_l_flags1 & FF1_FLOOR)	return (" floor");
	else if (f_l_ptr->f_l_flags1 & FF1_WALL)	return (" wall");
	else if (f_l_ptr->f_l_flags3 & FF3_TREE)	return (" tree");
	else if (f_l_ptr->f_l_flags2 & FF2_EFFECT)	return (" effect");

	/*Default*/
	else return (" feature");
}


/* Describe and print on the screen the type of feature */
static void describe_feature_type(const feature_lore *f_l_ptr)
{
	text_out(get_feature_type(f_l_ptr));
}


/*
 * Return TRUE if the given string start with a vowel
 * Leading whitespace is ignored
 */
static bool begins_with_vowel(cptr str)
{
	int i;

	/* Ignore whitespace */
	for (i = 0; str[i] == ' '; i++) ;

	/* Check */
	return (my_is_vowel(str[i]));
}


static void describe_feature_basic(int f_idx, const feature_lore *f_l_ptr)
{
	const feature_type *f_ptr = &f_info[f_idx];
	cptr flags[50];
	cptr type;
	int i = 0;
	u16b n = 0;

	text_out("This is a");

	/* Collect some flags */
	if (f_l_ptr->f_l_flags2 & FF2_SHALLOW)		if (n < N_ELEMENTS(flags)) flags[n++] = " shallow";
	if (f_l_ptr->f_l_flags2 & FF2_DEEP)			if (n < N_ELEMENTS(flags)) flags[n++] = " deep";

	if ((f_l_ptr->f_l_flags2 & FF2_GLOW) &&
	     (!(f_l_ptr->f_l_flags1 & FF1_SHOP)))	if (n < N_ELEMENTS(flags)) flags[n++] = " glowing";

	if (f_l_ptr->f_l_flags3 & FF3_LAVA)			if (n < N_ELEMENTS(flags)) flags[n++] = " lava";
	if (f_l_ptr->f_l_flags3 & FF3_ICE)			if (n < N_ELEMENTS(flags)) flags[n++] = " icy";
	if (f_l_ptr->f_l_flags3 & FF3_OIL)			if (n < N_ELEMENTS(flags)) flags[n++] = " oil";
	if (f_l_ptr->f_l_flags3 & FF3_FIRE)			if (n < N_ELEMENTS(flags)) flags[n++] = " fire";
	if (f_l_ptr->f_l_flags3 & FF3_SAND)			if (n < N_ELEMENTS(flags)) flags[n++] = " sandy";
	if (f_l_ptr->f_l_flags3 & FF3_FOREST)		if (n < N_ELEMENTS(flags)) flags[n++] = " forest";
	if (f_l_ptr->f_l_flags3 & FF3_WATER)		if (n < N_ELEMENTS(flags)) flags[n++] = " water";
	if (f_l_ptr->f_l_flags3 & FF3_ACID)			if (n < N_ELEMENTS(flags)) flags[n++] = " acid";
	if (f_l_ptr->f_l_flags3 & FF3_MUD)			if (n < N_ELEMENTS(flags)) flags[n++] = " mud";

	/* Print the collected flags */
	for (i = 0; i < n; i++)
	{
		/* Append a 'n' to 'a' if the first flag begins with a vowel */
		if ((i == 0) && begins_with_vowel(flags[i])) text_out("n");

		text_out_c(TERM_L_BLUE, flags[i]);
	}

	/* Get the feature type name */
	type = get_feature_type(f_l_ptr);

	/* Append a 'n' to 'a' if the type name begins with a vowel */
	if ((n == 0) && begins_with_vowel(type)) text_out("n");

	/* Describe the feature type */
	text_out(type);

	/* Describe location */
	if (f_ptr->f_flags1 & FF2_EFFECT)
	{
		/* Do nothing */
	}

	else if (f_l_ptr->f_l_flags1 & FF1_SHOP)
	{
		text_out_c(TERM_L_GREEN, " that is found in the town");
	}
	else if ((f_l_ptr->f_l_flags2 & FF2_TRAP_MON) ||
	(f_l_ptr->f_l_flags1 & FF1_GLYPH))
	{
		text_out_c(TERM_L_GREEN, " that is set by the player");
	}
	else if (f_l_ptr->f_l_sights > 10)
	{
		text_out(" that");

		if (f_l_ptr->f_l_sights > 20)
		{
			if (f_ptr->f_rarity >= 4) text_out(" rarely");
			else if (f_ptr->f_rarity >= 2) text_out(" occasionally");
			else text_out(" commonly");
		}

		if (f_ptr->f_level == 0)
		{
			text_out_c(TERM_L_GREEN, " appears in both the town and dungeon");
		}
		else if (f_ptr->f_level == 1)
		{
			text_out_c(TERM_L_GREEN, " appears throughout the dungeon");
		}
		else
		{
			text_out_c(TERM_L_GREEN, " appears");

			text_out_c(TERM_L_GREEN, format(" at depths of %d feet and below",
			                            f_ptr->f_level * 50));
		}

	}

	/* Little Information Yet */
	else
	{
		text_out_c(TERM_L_GREEN, " that is found in the dungeon");
	}

	/* End this sentence */
	text_out(".");

	/* More misc info */
	if (f_l_ptr->f_l_flags1 & FF1_DROP)
	{
		text_out("  This");
		/*Describe the feature type*/
		describe_feature_type(f_l_ptr);
		text_out(" can hold objects.");
	}
	if (f_l_ptr->f_l_flags1 & FF1_HAS_GOLD)
	{
		text_out("  This");
		describe_feature_type(f_l_ptr);
		text_out(" may be hiding treasure.");
	}
	if (f_l_ptr->f_l_flags1 & FF1_HAS_ITEM)
	{
		text_out("  This");
		describe_feature_type(f_l_ptr);
		text_out(" may be hiding an object.");
	}
}


static void describe_feature_move_see_cast(int f_idx, const feature_lore *f_l_ptr)
{

	int vn, n;
	cptr vp[6];

	/* Collect special abilities. */
	vn = 0;

	if (f_l_ptr->f_l_flags1 & FF1_MOVE)
	{
		if feat_ff1_match(f_idx, FF1_FLOOR)	vp[vn++] = "move over";
		else vp[vn++] = "move through";
	}
	if (f_l_ptr->f_l_flags1 & FF1_LOS) vp[vn++] = "see through";
	if (f_l_ptr->f_l_flags1 & FF1_RUN) vp[vn++] = "run past";
	if (f_l_ptr->f_l_flags1 & FF1_PROJECT)
	{
		vp[vn++] = "cast magic through";
		vp[vn++] = "fire projectiles through";
	}

	/* Describe special abilities. */
	if (vn)
	{
		/* Intro */
		text_out("  You ");

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) text_out("can ");
			else if (n < vn-1) text_out(", ");
			else text_out(" and ");

			/* Dump */
			text_out_c(TERM_GREEN, vp[n]);
		}

		/* End */
		text_out(" this");
		describe_feature_type(f_l_ptr);
		text_out(".");
	}

	if (f_l_ptr->f_l_flags2 & FF2_CAN_FLY)
	{
		text_out("  Creatures who have the ability to do so can fly over this");
		describe_feature_type(f_l_ptr);
		text_out(".");
	}
	if (f_l_ptr->f_l_flags2 & FF2_COVERED)
	{
		text_out("  Native creatures can hide in this");
		describe_feature_type(f_l_ptr);
		text_out(".");
	}
}


static void describe_feature_stairs(const feature_lore *f_l_ptr)
{
	text_out("  This");

	if (f_l_ptr->f_l_flags2 & FF2_SHAFT)	text_out_c(TERM_L_BLUE, " shaft");
	else text_out_c(TERM_L_BLUE, " staircase");

	text_out(" will take you");

	if (f_l_ptr->f_l_flags1 & FF1_LESS)		text_out_c(TERM_L_BLUE, " up");
	if (f_l_ptr->f_l_flags1 & FF1_MORE)		text_out_c(TERM_L_BLUE, " down");

	if (f_l_ptr->f_l_flags2 & FF2_SHAFT)	text_out_c(TERM_L_BLUE, " two levels.");
	else text_out_c(TERM_L_BLUE, " one level.");
}


static void describe_feature_trap(int f_idx, const feature_lore *f_l_ptr)
{
	/*Describe passive traps the player can set off*/
	if (f_l_ptr->f_l_flags2 & FF2_TRAP_PASSIVE) hit_trap(f_idx, 0, 0, MODE_DESCRIBE);

	/*Describe passive traps the player can set off*/
	if (f_l_ptr->f_l_flags2 & FF2_TRAP_MON) apply_monster_trap(f_idx, 0, 0, MODE_DESCRIBE);

	/*Describe smart traps */
	if (f_l_ptr->f_l_flags2 & FF2_TRAP_SMART)fire_trap_smart(f_idx, 0, 0, MODE_DESCRIBE);
}


static void describe_feature_interaction(int f_idx, const feature_lore *f_l_ptr)
{
	const feature_type *f_ptr = &f_info[f_idx];

	int vn, n, i;
	cptr vp[15];

	u32b filtered_flag1 = f_l_ptr->f_l_flags1;

	/* Collect special abilities. */
	vn = 0;

	/*
	 * First get rid of redundant messages, if they are to be
	 * described later in describe_feature_transitions.
	 */
	for (i = 0; i < MAX_FEAT_STATES; i++)
	{
		/* There isn't a recorded action here */
		if (f_ptr->state[i].fs_action == FS_FLAGS_END) continue;

		/* Have we seen it yet? */
		if (f_l_ptr->f_l_state[i] == 0) continue;

		switch (f_ptr->state[i].fs_action)
		{

			/* Make sure we don't list something redundantly and more specifically later*/
			case 	FS_OPEN:		{filtered_flag1 &= ~(FF1_CAN_OPEN); 	continue;}
			case	FS_CLOSE:		{filtered_flag1 &= ~(FF1_CAN_CLOSE); 	continue;}
			case	FS_BASH:		{filtered_flag1 &= ~(FF1_CAN_BASH); 	continue;}
			case	FS_SPIKE:		{filtered_flag1 &= ~(FF1_CAN_SPIKE); 	continue;}
			case	FS_TUNNEL:		{filtered_flag1 &= ~(FF1_CAN_TUNNEL); 	continue;}

			/*All others*/
			default: continue;
		}
	}

	if (filtered_flag1 & FF1_CAN_OPEN) vp[vn++] = "open";
	if (filtered_flag1 & FF1_CAN_CLOSE) vp[vn++] = "close";
	if (filtered_flag1 & FF1_CAN_BASH) vp[vn++] = "bash";
	if (filtered_flag1 & FF1_CAN_SPIKE) vp[vn++] = "spike";
	if (filtered_flag1 & FF1_CAN_DISARM) vp[vn++] = "disarm";
	if (filtered_flag1 & FF1_CAN_TUNNEL) vp[vn++] = "tunnel into";

	/* Describe special abilities. */
	if (vn)
	{
		/* Intro */
		text_out("  You ");

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) text_out("can ");
			else if (n < vn-1) text_out(", ");
			else text_out(" and ");

			/* Dump */
			text_out_c(TERM_YELLOW, vp[n]);
		}

		/* End */
		text_out(" this");
		describe_feature_type(f_l_ptr);
		text_out(".");
	}
}


static void describe_feature_vulnerabilities(const feature_lore *f_l_ptr)
{

	int vn, n;
	cptr vp[15];

	/* Collect special abilities. */
	vn = 0;

	if (f_l_ptr->f_l_flags2 & FF2_HURT_ROCK) vp[vn++] = "stone-to-mud";
	if (f_l_ptr->f_l_flags2 & FF2_HURT_FIRE) vp[vn++] = "fire";
	if (f_l_ptr->f_l_flags2 & FF2_HURT_FIRE) vp[vn++] = "lava";
	if (f_l_ptr->f_l_flags2 & FF2_HURT_FIRE) vp[vn++] = "plasma";
	if (f_l_ptr->f_l_flags2 & FF2_HURT_COLD) vp[vn++] = "cold";
	if (f_l_ptr->f_l_flags2 & FF2_HURT_COLD) vp[vn++] = "ice";
	if (f_l_ptr->f_l_flags2 & FF2_HURT_ACID) vp[vn++] = "acid";
	if (f_l_ptr->f_l_flags2 & FF2_HURT_ELEC) vp[vn++] = "electricity";
	if (f_l_ptr->f_l_flags2 & FF2_HURT_WATER) vp[vn++] = "water";
	if (f_l_ptr->f_l_flags3 & FF3_HURT_BOIL_WATER) vp[vn++] = "boiling water";
	if (f_l_ptr->f_l_flags3 & FF3_HURT_BOIL_WATER) vp[vn++] = "steam";
	if (f_l_ptr->f_l_flags3 & FF3_HURT_POIS) vp[vn++] = "poison";

	/* Describe special abilities. */
	if (vn)
	{
		/* Intro */
		text_out("  This");

		describe_feature_type(f_l_ptr);

		text_out(" is affected ");

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) text_out("by ");
			else if (n < vn-1) text_out(", ");
			else text_out(" and ");

			/* Dump */
			text_out_c(TERM_L_RED, vp[n]);
		}

		/* End */
		text_out(".");
	}
}


/*
 * Returns true if special language is used
 * returns false if the standard output should follow this.
 * This function assumes it is the start of the sentence.
 * Returning false follows this function by "causes this feature to change to"
 */

static bool describe_transition_action(int action)
{

	switch (action)
	{

		case 	FS_SECRET:		{text_out_c(TERM_YELLOW, "  Once discovered, this feature is revealed as ");return (TRUE);}
		case 	FS_OPEN:		{text_out_c(TERM_YELLOW, "  Opening"); 		break;}
		case	FS_CLOSE:		{text_out_c(TERM_YELLOW, "  Closing"); 		break;}
		case	FS_BASH:		{text_out_c(TERM_YELLOW, "  Bashing"); 		break;}
		case	FS_SPIKE:		{text_out_c(TERM_YELLOW, "  Spiking"); 		break;}
		case	FS_TUNNEL:		{text_out_c(TERM_YELLOW, "  Tunneling"); 		break;}
		case	FS_TRAP:		{text_out_c(TERM_YELLOW, "  Creating a trap"); break;}
		case	FS_GLYPH:		{text_out_c(TERM_YELLOW, "  This feature can change into a "); return (TRUE);}
		case	FS_FLOOR:		{text_out_c(TERM_YELLOW, "  A plain floor"); 	break;}
		case	FS_BRIDGE:		{text_out_c(TERM_YELLOW, "  Bridging"); 		break;}
		case	FS_HIT_TRAP:	{text_out_c(TERM_YELLOW, "  De-activating this trap"); break;}
		case	FS_HURT_ROCK:	{text_out_c(TERM_YELLOW, "  Stone-to-mud"); 	break;}
		case	FS_HURT_FIRE:	{text_out_c(TERM_YELLOW, "  Fire, smoke, lava or plasma"); break;}
		case	FS_HURT_COLD:	{text_out_c(TERM_YELLOW, "  Cold or ice"); 	break;}
		case	FS_HURT_ACID:	{text_out_c(TERM_YELLOW, "  Acid"); 			break;}
		case	FS_HURT_ELEC:	{text_out_c(TERM_YELLOW, "  Electricity"); 	break;}
		case	FS_HURT_WATER:	{text_out_c(TERM_YELLOW, "  Water"); 			break;}
		case	FS_HURT_BWATER:	{text_out_c(TERM_YELLOW, "  Boiling water"); 	break;}
		case	FS_HURT_POIS:	{text_out_c(TERM_YELLOW, "  Poison"); 			break;}
		case	FS_TREE:		{text_out_c(TERM_YELLOW, "  Forest creation"); break;}
		case   	FS_NEED_TREE: 	{text_out_c(TERM_YELLOW, "  Forest destruction"); break;}

		/*Paranoia*/
		default:				{text_out_c(TERM_RED, "  ERROR - Unspecified Action"); break;}
	}

	return (FALSE);
}


static void describe_feature_transitions(int f_idx, const feature_lore *f_l_ptr)
{
	feature_type *f_ptr = &f_info[f_idx];

	char feature_name[80];

	int i;

	bool other_trans = FALSE;

	/*Handle permanent features*/
	if (f_l_ptr->f_l_flags1 & FF1_PERMANENT)
	{
		text_out("  This is a permanent feature.");
	}

	/*Mention the mimic, if known*/
	if (f_ptr->f_mimic != f_idx)
	{
		/* Remember we have a transition we are reporting */
		other_trans = TRUE;

		/*Describe it*/
		text_out("  Until discovered, this feature appears as ");
		feature_desc(feature_name, sizeof(feature_name), f_ptr->f_mimic, TRUE, FALSE);
		text_out(format("%s.", feature_name));
	}

	/* Search the action */
	for (i = 0; i < MAX_FEAT_STATES; i++)
	{
		/* There isn't a recorded action here */
		if (f_ptr->state[i].fs_action == FS_FLAGS_END) continue;

		/* The feature isn't changing */
		if (f_ptr->state[i].fs_result == f_idx)
		{
			if (f_l_ptr->f_l_flags3 & (FF3_PICK_DOOR))
			{
				text_out(" Discovering this ");
				describe_feature_type(f_l_ptr);
				text_out(" reveals a closed door.");
			}

			continue;
		}

		/* Have we seen it yet? */
		if (f_l_ptr->f_l_state[i] == 0) continue;

		/* Remember we have a transition we are reporting */
		other_trans = TRUE;

		/* Describe it, followed by standard output */
		if(!describe_transition_action(f_ptr->state[i].fs_action))
		{
			text_out(" changes this");
			describe_feature_type(f_l_ptr);
			text_out(" to ");
		}

		feature_desc(feature_name, sizeof(feature_name), f_ptr->state[i].fs_result, TRUE, FALSE);
		text_out(format("%s.", feature_name));
	}

	/*Mention the default if it is different*/
	if ((f_l_ptr->f_l_defaults > 0) && (f_ptr->defaults != f_idx))
	{
		/*Describe this transition, handle differently if we have described a transition above*/
		if (other_trans)
		{
			text_out("  For all other effects, this");
		}
		else text_out("  This");
		describe_feature_type(f_l_ptr);
		text_out(" changes to ");
		feature_desc(feature_name, sizeof(feature_name), f_ptr->defaults, TRUE, FALSE);
		text_out(format("%s.", feature_name));
	}
}


static void describe_feature_damage(int f_idx, const feature_lore *f_l_ptr)
{
	feature_type *f_ptr = &f_info[f_idx];

	cptr action = "hurts";

	/* No damage, or no damage seen yet */
	if (f_ptr->dam_non_native == 0) return;
	if (!f_l_ptr->f_l_dam_non_native) return;

	/* Specify the damage type from certain features */
	if (_feat_ff3_match(f_ptr, FF3_ICE))
	{
		action = "freezes";
	}
	else if (_feat_ff3_match(f_ptr, FF3_LAVA | FF3_FIRE))
	{
		action = "burns";
	}

	/* Intro */
	text_out("  This");
	describe_feature_type(f_l_ptr);
	text_out(format(" %s any non-native creature", action));

	/* Slightly more information when the player has seen it several times */
	if (f_l_ptr->f_l_dam_non_native > 10)
	{
		text_out(format(" for %d damage", f_ptr->dam_non_native));
	}

	text_out(" who stays on this feature for one turn at normal speed.");
}


static void describe_feature_movement_effects(int f_idx, const feature_lore *f_l_ptr)
{
	feature_type *f_ptr = &f_info[f_idx];

	if (feat_ff2_match(f_idx, FF2_EFFECT)) return;

	/*Describe movement by native creatures*/
	if ((f_ptr->native_energy_move != BASE_ENERGY_MOVE) && (f_l_ptr->f_l_native_moves > 0))
	{
		int percent_movement = (ABS(BASE_ENERGY_MOVE - f_ptr->native_energy_move) * 100) / BASE_ENERGY_MOVE;

		text_out("  A creature native to this terrain uses");

		/*More information for who have observed movement more*/
		if (f_l_ptr->f_l_native_moves > 20)
		{
			if (f_ptr->native_energy_move > BASE_ENERGY_MOVE)
			{
				text_out_c(TERM_BLUE, format(" %d percent more", percent_movement));
			}
			else text_out_c(TERM_BLUE, format(" %d percent less", percent_movement));
		}
		else
		{
			if (percent_movement  > 15) text_out_c(TERM_BLUE, " significantly");

			if (f_ptr->native_energy_move > BASE_ENERGY_MOVE)  text_out_c(TERM_BLUE, " more");
			else text_out_c(TERM_BLUE, " less");

		}

		text_out(" energy moving into this terrain.");
	}

	/*Describe movement by non-native creatures*/
	if ((f_ptr->non_native_energy_move != BASE_ENERGY_MOVE) && (f_l_ptr->f_l_non_native_moves > 0))
	{
		int percent_movement = (ABS(BASE_ENERGY_MOVE - f_ptr->non_native_energy_move) * 100) / BASE_ENERGY_MOVE;

		text_out("  A creature who is not native to this terrain uses");

		/*More information for who have observed movement more*/
		if (f_l_ptr->f_l_non_native_moves > 20)
		{
			if (f_ptr->non_native_energy_move > BASE_ENERGY_MOVE)
			{
				text_out_c(TERM_BLUE, format(" %d percent more", percent_movement));
			}
			else text_out_c(TERM_BLUE, format(" %d percent less", percent_movement));
		}
		else
		{
			if (percent_movement  > 15) text_out_c(TERM_BLUE, " significantly");

			if (f_ptr->non_native_energy_move > BASE_ENERGY_MOVE)  text_out_c(TERM_BLUE, " more");
			else text_out_c(TERM_BLUE, " less");

		}

		text_out(" energy moving into this terrain.");
	}
}


static void describe_feature_combat_effects(int f_idx, const feature_lore *f_l_ptr)
{
	feature_type *f_ptr = &f_info[f_idx];

	if (feat_ff2_match(f_idx, FF2_EFFECT)) return;

	/* Describe movement by native creatures */
	if ((f_ptr->native_to_hit_adj != 100) && (f_l_ptr->f_l_native_to_hit_adj > 0))
	{
		text_out("  A native creature is");

		/* More information for who have observed movement more */
		if (f_l_ptr->f_l_native_to_hit_adj > 100)
		{
			if (f_ptr->native_to_hit_adj  > 100)
			{
				text_out_c(TERM_BLUE, " %d percent more", (f_ptr->native_to_hit_adj  - 100));
			}
			else text_out_c(TERM_BLUE, " %d percent less",
				(100 - f_ptr->native_to_hit_adj));

		}
		else
		{
			if (ABS(f_ptr->native_to_hit_adj  - 100) > 15) text_out_c(TERM_BLUE, " significantly");

			if (f_ptr->native_to_hit_adj  > 100)  text_out_c(TERM_BLUE, " more");
			else text_out_c(TERM_BLUE, " less");

		}

		text_out(" successful in attacking and defending while fighting in this terrain.");
	}

	/*Describe movement by non_native creatures*/
	if ((f_ptr->non_native_to_hit_adj != 100) && (f_l_ptr->f_l_non_native_to_hit_adj > 0))
	{
		text_out("  A non-native creature is");

		/*More information for who have observed movement more*/
		if (f_l_ptr->f_l_non_native_to_hit_adj > 100)
		{
			if (f_ptr->non_native_to_hit_adj > 100)
			{
				text_out_c(TERM_BLUE, format(" %d percent more", (f_ptr->non_native_to_hit_adj - 100)));
			}
			else text_out_c(TERM_BLUE, format(" %d percent less",
				(100 - f_ptr->non_native_to_hit_adj)));

		}
		else
		{
			if (ABS(f_ptr->non_native_to_hit_adj - 100) > 15) text_out_c(TERM_BLUE, " significantly");

			if (f_ptr->non_native_to_hit_adj > BASE_ENERGY_MOVE)  text_out_c(TERM_BLUE, " more");
			else text_out_c(TERM_BLUE, " less");

		}

		text_out(" successful in attacking and defending while fighting in this terrain.");
	}
}


static void describe_feature_stealth_effects(int f_idx, const feature_lore *f_l_ptr)
{
	feature_type *f_ptr = &f_info[f_idx];

	if (feat_ff2_match(f_idx, FF2_EFFECT)) return;

	/*Describe stealth effects*/
	if ((f_ptr->f_stealth_adj != 0) && (f_l_ptr->f_l_stealth_adj > 0))
	{

		text_out("  Walking through this terrain");

		if (f_l_ptr->f_l_stealth_adj > 100)
		{

			if (ABS(f_ptr->f_stealth_adj) >= 3) text_out(" significantly");
			else if (ABS(f_ptr->f_stealth_adj) == 2) text_out(" considerably");
			else if (ABS(f_ptr->f_stealth_adj) == 1) text_out(" somewhat");
		}

		if (f_ptr->f_stealth_adj  > 0)  text_out(" improves");
		else text_out(" reduces");

		text_out(" the player's stealth.");
	}
}


/*
 * This section describes dynamic features.
 * The description for each feature needs to be described on a case-by-case basis.
 * This function should be consistent with process_dynamic_terrain_aux for its output.
 */
static void describe_feature_dynamic(int f_idx, const feature_lore *f_l_ptr)
{
	feature_type *f_ptr = &f_info[f_idx];

	/*
	 * Hack - describe Ethereal WallTeleport player
	 * TODO - figure out how to remember this in feature_lore.
	 */
	if (f_idx == FEAT_ETHEREAL_WALL)
	{
		text_out("  This wall can teleport you across the dungeon, or explode in a burst of light.");
	}

	/* TODO - figure out how to remember this in feature_lore.*/
	if (f_idx == FEAT_WALL_INSCRIPTION)
	{
		text_out("  This wall can cast a spell at the player,");
		text_out(" or reveal some useful information about the current dungeon level.");

		return;
	}

	/* Has not been observed */
	if (!(f_l_ptr->f_l_flags3 & (FF3_DYNAMIC))) return;

	/* Dynamic fire can spread smoke and fire */
	if (_feat_ff3_match(f_ptr, FF3_FIRE))
	{
		text_out("  This terrain can spread fire and smoke to adjacent terrains.");

		return;
	}

	if (f_idx == FEAT_GEYSER)
	{
		text_out("  The geyser can explode in a burst of boiling water!");

		/* Done */
		return;
	}

	if (f_idx == FEAT_FSOIL_DYNAMIC)
	{
		text_out("  This feature can slowly spread across the dungeon.");

		/* Done */
		return;
	}

	/* Sniper flowers */
	if (f_idx == FEAT_PUTRID_FLOWER)
	{
		text_out("  This flower can fire spikes or spit poison at you.");

		/* Done */
		return;
	}

	/* Silent watchers */
	if (f_idx == FEAT_SILENT_WATCHER)
	{
		text_out("  The silent watcher can aggravate nearly monsters.");

		/* Done */
		return;
	}

	/* Dynamic lava can spread fire */
	if (_feat_ff3_match(f_ptr, TERRAIN_MASK) == (ELEMENT_LAVA))
	{
		text_out("  This terrain can spread fire to adjacent terrains.");

		return;
	}
}

/*
 * Hack -- display feature information using "roff()"
 *
 *
 * This function should only be called with the cursor placed at the
 * left edge of the screen or line, on a cleared line, in which the output is
 * to take place.  One extra blank line is left after the recall.
 */
void describe_feature(int f_idx, bool spoilers)
{
	feature_lore lore;

	feature_lore save_mem;

	/* Get the race and lore */
	const feature_type *f_ptr = &f_info[f_idx];
	feature_lore *f_l_ptr = &f_l_list[f_idx];

	/* Cheat -- know everything */
	if (cheat_know)
	{
		/* XXX XXX XXX */

		/* Hack -- save memory */
		COPY(&save_mem, f_l_ptr, feature_lore);
	}

	/* Hack -- create a copy of the monster-memory */
	COPY(&lore, f_l_ptr, feature_lore);

	/* Assume some "obvious" flags */
	lore.f_l_flags1 |= (f_ptr->f_flags1 & FF1_OBVIOUS_MASK);
	lore.f_l_flags2 |= (f_ptr->f_flags2 & FF2_OBVIOUS_MASK);
	lore.f_l_flags3 |= (f_ptr->f_flags3 & FF3_OBVIOUS_MASK);

	/* Cheat -- know everything*/
	if (cheat_know || spoilers)
	{
		cheat_feature_lore(f_idx, &lore);
	}

	/* Describe the movement and level of the monster */
	describe_feature_basic(f_idx, &lore);

	/* Describe the movement, LOS, and projection */
	describe_feature_move_see_cast(f_idx, &lore);

	/* Describe stairs */
	if (lore.f_l_flags1 & FF1_STAIRS) describe_feature_stairs(&lore);

	/* Describe trap */
	if (lore.f_l_flags1 & FF1_TRAP) describe_feature_trap(f_idx, &lore);

	describe_feature_interaction(f_idx, &lore);

	describe_feature_vulnerabilities(&lore);

	describe_feature_transitions(f_idx, &lore);

	describe_feature_damage(f_idx, &lore);

	describe_feature_movement_effects(f_idx, &lore);

	describe_feature_combat_effects(f_idx, &lore);

	describe_feature_stealth_effects(f_idx, &lore);

	describe_feature_dynamic(f_idx, &lore);

	/* All done */
	text_out("\n");
}


/*
 * Hack -- Display the "name" and "attr/chars" of a feature
 */
void feature_roff_top(int f_idx)
{
	feature_type *f_ptr = &f_info[f_idx];

	byte a1, a2;
	char c1, c2;

	char name[80];

	/* Get the chars */
	c1 = f_ptr->d_char;
	c2 = f_ptr->x_char;

	/* Get the attrs */
	a1 = f_ptr->d_attr;
	a2 = f_ptr->x_attr;

	/* Clear the top line */
	Term_erase(0, 0, 255);

	/* Reset the cursor */
	Term_gotoxy(0, 0);

	/* Get the feature name */
	feature_desc(name, sizeof(name), f_idx, FALSE, TRUE);

	/* Dump the name */
	Term_addstr(-1, TERM_WHITE, name);

	/* Append the "standard" attr/char info */
	Term_addstr(-1, TERM_WHITE, " ('");
	Term_addch(a1, c1);
	Term_addstr(-1, TERM_WHITE, "')");

	/* Append the "optional" attr/char info */
	Term_addstr(-1, TERM_WHITE, "/('");
	Term_addch(a2, c2);
	if (use_bigtile && (a2 & 0x80)) Term_addch(255, -1);
	Term_addstr(-1, TERM_WHITE, "'):");
}


/*
 * Hack -- describe the given feature at the top of the screen
 */
void screen_feature_roff(int f_idx)
{
	/* Flush messages */
	message_flush();

	/* Begin recall */
	Term_erase(0, 1, 255);

	/* Output to the screen */
	text_out_hook = text_out_to_screen;

	/* Recall feature */
	describe_feature(f_idx, FALSE);

	/* Describe feature */
	feature_roff_top(f_idx);
}


/*
 * Hack -- describe the given feature in the current "term" window
 */
void display_feature_roff(int f_idx)
{
	int y;

	/* Erase the window */
	for (y = 0; y < Term->hgt; y++)
	{
		/* Erase the line */
		Term_erase(0, y, 255);
	}

	/* Begin recall */
	Term_gotoxy(0, 1);

	/* Output to the screen */
	text_out_hook = text_out_to_screen;

	/* Recall feature */
	describe_feature(f_idx, FALSE);

	/* Describe feature  */
	feature_roff_top(f_idx);
}


/*
 * Reset the dyna_g array to its initial state. To be used every time a level
 * is generated
 */
void wipe_dynamic_terrain(void)
{
	/* Wipe the dyna_g entries */
	dyna_cnt = 0;
	dyna_next = 0;

	/* Turn dyna_full off */
	dyna_full = FALSE;
	dyna_center_y = 255;
	dyna_center_x = 255;
}


/*
 * Returns a pointer to a dyna_g entry given its coordinates.
 * Returns NULL if the entry was not found.
 */
dynamic_grid_type *get_dynamic_terrain(byte y, byte x)
{
	int i;

	/* Search in dyna_g */
	for (i = 0; i < dyna_next; i++)
	{
		/* Get the grid info */
		dynamic_grid_type *g_ptr = &dyna_g[i];

		/* Ignore removed grids */
		if (!(g_ptr->flags & (DF1_OCCUPIED))) continue;

		/* Found the coordinates */
		if ((g_ptr->y == y) && (g_ptr->x == x)) return (g_ptr);
	}

	/* Failure */
	return (NULL);
}


/*
 * Some dynamic features use a timer. Returns the value of that timer.
 * Every tick is equal to one call to process_dynamic_terrain.
 * Returns 0 if the feature isn't timed.
 */
static byte calculate_turn_count(u16b feat)
{
	/* Growing trees */
	if (feat == FEAT_FSOIL_DYNAMIC)
	{
		return (25 + rand_int(30));
	}

	/* Geysers */
	if (feat == FEAT_GEYSER)
	{
		return (20 + rand_int(50));
	}

	/* Sniper flowers */
	if (feat == FEAT_PUTRID_FLOWER)
	{
		return (15 + rand_int(20));
	}

	/* Silent watchers */
	if (feat == FEAT_SILENT_WATCHER)
	{
		return (5 + rand_int(10));
	}

	/* Default */
	return (0);
}


/*
 * Add a new grid to the dyna_g array given its coordinates.
 * Returns TRUE on success
 */
bool add_dynamic_terrain(byte y, byte x)
{
	int i;
	dynamic_grid_type *g_ptr;

	/* We don't have more space in dyna_g */
	if (dyna_cnt >= DYNA_MAX)
	{
		/*
		 * Turn on dyna_full. From now on and until the generation of
		 * a new level, the contents of dyna_g will be updated every
		 * certain number of steps.
		 */
		dyna_full = TRUE;

		/* Hack - Force the rebuild of dyna_g */
		dyna_center_y = 255;
		dyna_center_x = 255;

		/* Failure */
		return (FALSE);
	}

	/*
	 * dyna_full is on, and the grid is too far away from the player.
	 * Ignore that grid.
	 */
	if (dyna_full && character_dungeon &&
		(distance(y, x, p_ptr->py, p_ptr->px) > MAX_SIGHT))
	{
		/* Failure */
		return (FALSE);
	}

	/* We can add the grid at the end of dyna_g without problems */
	if (dyna_next < DYNA_MAX)
	{
		i = dyna_next++;
	}
	/* We reached the end of dyna_g. Find a gap between entries */
	else
	{
		/* Scan dyna_g */
		for (i = 0; i < DYNA_MAX; i++)
		{
			/* We found an unused entry */
			if (!(dyna_g[i].flags & (DF1_OCCUPIED))) break;
		}
	}

	/* Get the new entry */
	g_ptr = &dyna_g[i];

	/* Fill in the grid info */
	g_ptr->y = y;
	g_ptr->x = x;
	g_ptr->flags = (DF1_OCCUPIED | DF1_NEW_BORN);
	g_ptr->counter = calculate_turn_count(cave_feat[y][x]);

	/* One grid more */
	++dyna_cnt;

	/* Success */
	return (TRUE);
}


/*
 * Remove a grid from the dyna_g array given its coordinates
 */
void remove_dynamic_terrain(byte y, byte x)
{
	/* Get the dyna_g entry of that grid */
	dynamic_grid_type *g_ptr = get_dynamic_terrain(y, x);

	/* Got one? */
	if (g_ptr)
	{
		/* Clear its fields */
		WIPE(g_ptr, dynamic_grid_type);

		/* Decrease the grid count */
		if (dyna_cnt > 0) --dyna_cnt;
	}
}


/*
 * Size of the "visited" array
 */
#define MAX_VISITED ((MAX_SIGHT + 10) * 2 + 1)


/*
 * Reset the dyna_g array and fill it with as many dynamic features close
 * to the player we can find (features out of the MAX_SIGHT range are ignored).
 * To be used when dyna_full is on.
 */
static void collect_dynamic_terrain(void)
{
	int y, x, i, j, d;

	int last_index, grid_count;
	int this_cycle = 0, next_cycle = 1;
	byte grid_table[2][2][8 * MAX_SIGHT];

	bool visited[MAX_VISITED][MAX_VISITED];
	int y2v, x2v;

	dynamic_grid_type *g_ptr;

	/* Remember the center of this update */
	dyna_center_y = p_ptr->py;
	dyna_center_x = p_ptr->px;

	/* Wipe the contents of the dyna_g array. We keep timed terrain */
	for (i = j = 0; i < dyna_next; i++)
	{
		/* Get the grid info */
		g_ptr = &dyna_g[i];

		/* Removed entry. Ignore */
		if (!(g_ptr->flags & (DF1_OCCUPIED))) continue;

		/* Non-timed terrain. Ignore */
		if (!g_ptr->counter) continue;

		/* Far timed terrain is removed */
		if (distance(g_ptr->y, g_ptr->x, p_ptr->py, p_ptr->px) > MAX_SIGHT) continue;

		/* Copy the timed terrain into its final position */
		if (i > j)
		{
			COPY(&dyna_g[j], g_ptr, dynamic_grid_type);
		}

		/* We have one timed terrain more */
		++j;
	}

	/* The number of collected timed features is the new size of dyna_g */
	dyna_next = dyna_cnt = j;

	/* dyna_g is full of timed features. */
	if (dyna_cnt >= DYNA_MAX) return;

	/* Mark timed terrain */
	for (i = 0; i < dyna_cnt; i++)
	{
		/* Get the grid info */
		g_ptr = &dyna_g[i];

		/* Mark the timed terrain (see later) */
		cave_info[g_ptr->y][g_ptr->x] |= (CAVE_TEMP);
	}

	/* No visited grids yet */
	for (y = 0; y < MAX_VISITED; y++)
	{
		for (x = 0; x < MAX_VISITED; x++)
		{
			visited[y][x] = FALSE;
		}
	}

	/*
	 * These variables translate dungeon coordinates to the coordinates
	 * used in the "visited" array. Note that the center of the update
	 * is translated to the physical center of the "visited" array.
	 */
	y2v = (MAX_VISITED / 2) - dyna_center_y;
	x2v = (MAX_VISITED / 2) - dyna_center_x;

	/* Add the center grid to the table */
	grid_table[this_cycle][0][0] = dyna_center_y;
	grid_table[this_cycle][1][0] = dyna_center_x;
	grid_count = 1;

	/* Mark the center grid as visited */
	visited[MAX_VISITED / 2][MAX_VISITED / 2] = TRUE;

	/* Add the center grid to dyna_g if necessary */
	if (cave_ff3_match(dyna_center_y, dyna_center_x, FF3_DYNAMIC))
	{
		/* Get the grid info */
		g_ptr = &dyna_g[dyna_cnt++];

		/* Fill in the grid info (partially) */
		g_ptr->y = dyna_center_y;
		g_ptr->x = dyna_center_x;
	}

	/* Put the nearby dynamic features in dyna_g */
	while (TRUE)
	{
		/* Get the number of input grids */
		last_index = grid_count;

		/* No more grids */
		if (!last_index) break;

		/* Reset the number of output grids */
		grid_count = 0;

		/* Process the input grids */
		for (i = 0; i < last_index; i++)
		{
			/* Get the coordinates of the next input grid */
			y = grid_table[this_cycle][0][i];
			x = grid_table[this_cycle][1][i];

			/* Check the adjacent grids */
			for (d = 0; d < 8; d++)
			{
				/* Get coordinates */
				int y2 = y + ddy_ddd[d];
				int x2 = x + ddx_ddd[d];
				int vy, vx, dx, dy, dist;

				/* Ignore annoying locations */
				if (!in_bounds(y2, x2)) continue;

				/* Translate coordinates to use the "visited" array */
				vy = y2v + y2;
				vx = x2v + x2;

				/* Ignore grids already visited */
				if (visited[vy][vx]) continue;

				/* Mark the grid as visited */
				visited[vy][vx] = TRUE;

				/*
				 * IMPORTANT: ignore any dynamic feature out of line
				 * of fire.
				 * Reasons:
				 * 1. Avoid possible dynamic-terrain-scum tactics.
				 * Example: throwing a fire ball into an oil lake
				 * followed by teleportation, etc.
				 * 2. Efficiency (only the grids near to the player
				 * are processed).
				 */
				if (!player_can_fire_bold(y2, x2)) continue;

				/* Calculate the distance to the center */

				/* Vertical distance */
				dy = y2 - dyna_center_y;
				if (dy < 0) dy = -dy;

				/* Horizontal distance */
				dx = x2 - dyna_center_x;
				if (dx < 0) dx = -dx;

				/* Final distance */
				dist = (dy > dx) ? (dy + (dx>>1)): (dx + (dy>>1));

				/* Out of range */
				if (dist > MAX_SIGHT) continue;

				/* Put the grid in dyna_g, if possible */
				/*
				 * We do this when:
				 * 1. It's a dynamic grid.
				 * 2. It isn't a timed grid already added.
				 * 3. We have space in dyna_g
				 */
				if (cave_ff3_match(y2, x2, FF3_DYNAMIC) &&
					!(cave_info[y2][x2] & (CAVE_TEMP)) &&
					(dyna_cnt < DYNA_MAX))
				{
					/* Claim a dyna_g entry */
					g_ptr = &dyna_g[dyna_cnt++];

					/* Fill in the grid info (partially) */
					g_ptr->y = y2;
					g_ptr->x = x2;
				}

				/* Add this grid to the table */
				grid_table[next_cycle][0][grid_count] = y2;
				grid_table[next_cycle][1][grid_count] = x2;

				/* One more output grid */
				++grid_count;
			}
		}

		/* Swap the table for the next iteration */
		if (this_cycle == 0)
		{
			this_cycle = 1;
			next_cycle = 0;
		}
		else
		{
			this_cycle = 0;
			next_cycle = 1;
		}
	}

	/* Complete the required info */
	for (i = 0; i < dyna_cnt; i++)
	{
		/* Get the grid */
		g_ptr = &dyna_g[i];

		/* Old timed terrain */
		if (cave_info[g_ptr->y][g_ptr->x] & (CAVE_TEMP))
		{
			/* We just clear the mark. Grid info is preserved */
			cave_info[g_ptr->y][g_ptr->x] &= ~(CAVE_TEMP);
		}
		/* Regular terrain (or new timed terrain) */
		else
		{
			/* Set some flags for the collected grids */
			g_ptr->flags = (DF1_OCCUPIED | DF1_NEW_BORN);

			/* Reset the counter */
			g_ptr->counter = calculate_turn_count(cave_feat[g_ptr->y][g_ptr->x]);
		}
	}

	/* Set the new maximum size of dyna_g */
	dyna_next = dyna_cnt;
}


/*
 * Apply the effect of *one* dynamic feature.
 * We mark the terrain lore if the dynamic terrain change is observed.
 * This function should be consistent with describe_terrain_dynamic in the feature description.
 */
static void process_dynamic_terrain_aux(dynamic_grid_type *g_ptr)
{

	/* Get coordinates */
	int y = g_ptr->y;
	int x = g_ptr->x;

	/* Get the feature */
	u16b feat = cave_feat[y][x];
	u16b feat2;

	/*We need to remember is the player saw this.*/
	feature_lore *f_l_ptr = &f_l_list[feat];

	/* Dynamic fire eventually burns its adjacent grids */
	if (feat_ff3_match(feat, FF3_FIRE))
	{
		int d;
		int dam = f_info[feat].dam_non_native;
		bool can_burn = FALSE;
		bool can_smoke = FALSE;

		if (!one_in_(3)) return;

		/* Scan adjacent grids */
		for (d = 0; d < 8; d++)
		{
			/* Get the coordinates */
			int yy = y + ddy_ddd[d];
			int xx = x + ddx_ddd[d];

			/* Ignore annoying locations*/
			if (!in_bounds(yy, xx)) continue;

			/* Get the feature */
			feat2 = cave_feat[yy][xx];

			/* Feature can burn */
			if (feat_ff2_match(feat2, FF2_HURT_FIRE))
			{
				u32b flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;

				/* A grid can be affected. Keep the fire alive */
				can_burn = TRUE;

				/* Prevent scumming */
				if (!player_can_fire_bold(yy, xx)) continue;

				/* Oil burns faster */
				if (feat_ff3_match(feat2, FF3_OIL))
				{
					/* But not always */
					if (!one_in_(5)) continue;
				}
				else
				{
					/* Don't want to burn forests too fast */
					if (!one_in_(35)) continue;
				}

				/* Burn/melt the feature */
				project(SOURCE_OTHER, 0, y, x, yy, xx, dam, GF_FIRE, flg, 0, 0);

				/*Mark the lore if the player observed this*/
				if (player_can_see_bold(yy, xx)) f_l_ptr->f_l_flags3 |= (FF3_DYNAMIC);
			}
			/* We can create smoke */
			else if ((cave_info[yy][xx] & (CAVE_MOVE | CAVE_LOS)) == (CAVE_MOVE | CAVE_LOS))
			{
				/* Remember this fact */
				can_smoke = TRUE;

				/* But not always make smoke */
				if (!one_in_(15)) continue;

				/* Create smoke */
 				set_effect_lingering_cloud(FEAT_SMOKE, yy, xx, 100, SOURCE_OTHER, 0);

				/*Mark the lore if the player observed this*/
				if (player_can_see_bold(yy, xx)) f_l_ptr->f_l_flags3 |= (FF3_DYNAMIC);
			}
		}

		/* Smokers are removed sometimes to speed things up */
		if (!can_burn && (!can_smoke || one_in_(2))) remove_dynamic_terrain(y, x);

		/* Done */
		return;
	}

	/* Boiling water geysers can soak the dungeon with boiling water */
	if (feat == FEAT_GEYSER)
	{
		u32b flg = PROJECT_BOOM | PROJECT_ITEM | PROJECT_GRID |
			PROJECT_KILL | PROJECT_PLAY;

		int dam = f_info[feat].dam_non_native;

		/* Set the timer again */
		g_ptr->counter = calculate_turn_count(feat);

		/* Check line of fire */
		if (!player_can_fire_bold(y, x)) return;

		/* Show a message */
		if (player_can_see_bold(y, x))
		{
			msg_print("The geyser explodes in a burst of boiling water!");

			/*Mark the lore*/
			f_l_ptr->f_l_flags3 |= (FF3_DYNAMIC);

			disturb(0, 0);
		}

		/* Splash! */
		project(SOURCE_OTHER, 2 + rand_int(2), y, x, y, x, dam, GF_BWATER, flg, 0, 0);

		/* Done */
		return;
	}

	/*
	 * A very bad imitation of the Fangorn forest.
	 */
	if (feat == FEAT_FSOIL_DYNAMIC)
	{
		bool skip = FALSE;

		/* Ignore locations out of line of fire */
		if (!player_can_fire_bold(y, x)) skip = TRUE;

		/* Ignore locations occupied by monsters/players */
		else if (cave_m_idx[y][x]) skip = TRUE;

		/* Ignore locations occupied by objects */
		else if (cave_o_idx[y][x]) skip = TRUE;

		/* Don't grow too fast */
		else if (!one_in_(50)) skip = TRUE;

		/* Cancel tree generation */
		if (skip)
		{
			/* Set the timer again */
			g_ptr->counter = calculate_turn_count(feat);

			/* Done */
			return;
		}

		/* Message */
		if (player_can_see_bold(y, x))
		{
			msg_print("The forest seems to be alive.");

			/*Mark the lore*/
			f_l_ptr->f_l_flags3 |= (FF3_DYNAMIC);

			disturb(0, 0);
		}

		/* Create a new tree */
		/*
		 * This can be done safely because the branches are passable
		 * and can hold objects, so any entity adjacent to the trunk
		 * remains untouched.
		 */
		cave_set_feat(y, x, FEAT_TREE);

		/* Done */
		return;
	}

	/* Animate waves */
	if (feat_ff3_match(feat, FF3_WATER))
	{
		int d = -1, k;
		int yy, xx;
		bool kill = FALSE;
		int freq = 10000;

		/* Crests don't live too much time */
		if (strstr(f_name + f_info[feat].name, "crest of a wave") != NULL)
		{
			kill = TRUE;
		}
		/* Other waves are often inactive */
		else
		{
			k = rand_int(200);

			if (k >= 10) return;
		}

		/* Pick a direction for the waves */
		k = turn % freq;

		/* To the right */
		if (k < (freq / 2))
		{
		       	int directions[] = {3, 6, 9};
			d = directions[rand_int(3)];
		}

		/* To the left */
		else
		{
			int directions[] = {1, 4, 7};
			d = directions[rand_int(3)];
		}

		/* Transform an adjacent grid if possible */
		if (d != -1)
		{
			/* Get coordinates */
			yy = y + ddy[d];
			xx = x + ddx[d];

			/* The grid must be affected by water  */
			if (in_bounds(yy, xx) && cave_ff2_match(yy, xx, FF2_HURT_WATER))
			{
				/* Transform */
				cave_alter_feat(yy, xx, FS_HURT_WATER);

				/* Remove the wave must of the time */
				if (!kill && !one_in_(10)) kill = TRUE;
			}
		}

		/* A wave disappears eventually */
		if (kill && feat_ff2_match(feat, FF2_HURT_FIRE))
		{
			/* Get the next feature to avoid messages */
			feat = feat_state(feat, FS_HURT_FIRE);

			/*
			 * Turn the wave into plain water, or a crest into a
			 * wave
			 */
			cave_set_feat(y, x, feat);
		}

		/* Done */
		return;
	}

	/* Sniper flowers */
	if (feat == FEAT_PUTRID_FLOWER)
	{
		u32b flg = PROJECT_PLAY | PROJECT_KILL | PROJECT_STOP;

		int dam;
		byte gf_type;

		/* Set the timer again */
		g_ptr->counter = calculate_turn_count(feat);

		/* Check line of fire and los */
		if (!player_can_fire_bold(y, x) || !player_has_los_bold(y, x)) return;

		/* Select damage type */
		gf_type = (one_in_(10) ? GF_POIS: GF_ARROW);

		/* Show a message */
		if (player_can_see_bold(y, x))
		{
			if (gf_type == GF_ARROW)
			{
				msg_print("The putrid flower aims at you with spikes!");
			}
			else if (gf_type == GF_POIS)
			{
				msg_print("The putrid flower spits poison in your face!");
			}

			/*Mark the lore*/
			f_l_ptr->f_l_flags3 |= (FF3_DYNAMIC);
		}

		/* Message for when the player can't see */
		else
		{
			if (gf_type == GF_ARROW)
			{
				msg_print("It aims at you with spikes!");
			}
			else if (gf_type == GF_POIS)
			{
				msg_print("It spits poison in your face!");
			}
		}

		/* Tell the player */
		disturb(0, 0);

		/* Calculate damage */
		dam = 2 * effective_depth(p_ptr->depth) / 3;

		if (dam < 1) dam = 1;

		/* Fire a bolt to the player  */
		project(SOURCE_OTHER, 0, y, x, p_ptr->py, p_ptr->px, dam, gf_type, flg, 0, 0);

		/* Done */
		return;
	}

	/* Silent watchers */
	if (feat == FEAT_SILENT_WATCHER)
	{
		/* Set the timer again */
		g_ptr->counter = calculate_turn_count(feat);

		/* Player must be near to the silent watcher */
		if (!player_has_los_bold(y, x) || (distance(y, x, p_ptr->py, p_ptr->px) > 4)) return;

		/* Message */
		msg_c_format(MSG_NOTICE, "The silent watcher howls in madness!");

		if (player_can_see_bold(y, x))
		{
			/*Mark the lore*/
			f_l_ptr->f_l_flags3 |= (FF3_DYNAMIC);
		}

		/* Stop resting/running */
		disturb(0, 0);

		/* Call monsters */
		aggravate_monsters(SOURCE_OTHER);

		/* Done */
		return;
	}

	/* Dynamic lava eventually burns its adjacent grids */
	if (feat_ff3_match(feat, TERRAIN_MASK) == (ELEMENT_LAVA))
	{
		int d;
		int dam = f_info[feat].dam_non_native;
		bool can_burn = FALSE;

		/* Flavor */
		if (!one_in_(2)) return;

		/* Scan adjacent grids */
		for (d = 0; d < 8; d++)
		{
			/* Get the coordinates */
			int yy = y + ddy_ddd[d];
			int xx = x + ddx_ddd[d];

			/* Ignore annoying locations*/
			if (!in_bounds(yy, xx)) continue;

			/* Get the feature */
			feat2 = cave_feat[yy][xx];

			/* Feature can burn */
			if (feat_ff2_match(feat2, FF2_HURT_FIRE))
			{
				u32b flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;

				/* A grid can be affected. Keep the lava alive */
				can_burn = TRUE;

				/* Prevent scumming */
				if (!player_can_fire_bold(yy, xx)) continue;

				/* Flavor */
				if (!one_in_(4)) continue;

				/* Burn/melt the feature */
				project(SOURCE_OTHER, 0, y, x, yy, xx, dam, GF_FIRE, flg, 0, 0);

				/* Mark the lore if the player observed this */
				if (player_can_see_bold(yy, xx)) f_l_ptr->f_l_flags3 |= (FF3_DYNAMIC);
			}
		}

		/* Remove the dynamic mark if the dungeon cannot be affected, to speed things up */
		if (!can_burn) remove_dynamic_terrain(y, x);

		/* Done */
		return;
	}
}


/*
 * Traverse the dynamic features stored in dyna_g and apply their effects.
 */
void process_dynamic_terrain(void)
{
	int i, j;
	u16b max;
	bool full = FALSE;
	dynamic_grid_type *g_ptr;

	/* We have a level saturated with dynamic features, check rebuild */
	if (dyna_full)
	{
		/* Get approximate distance to the center of the last update */
		int dist_y = ABS(p_ptr->py - dyna_center_y);
		int dist_x = ABS(p_ptr->px - dyna_center_x);
		int dist = MAX(dist_y, dist_x);

		/* Too far from the previous center, rebuild */
		if (dist > 5) full = TRUE;
	}

	/*
	 * Rebuild dyna_g. We put in only the nearest dynamic features to the
	 * player.
	 */
	if (full) collect_dynamic_terrain();

	/* Common case. We don't have any dynamic features */
	if (dyna_cnt == 0)
	{
		/* Reset the maximum size of dyna_g, for efficiency */
		dyna_next = 0;

		/* Done */
		return;
	}

	/*
	 * Important. We make a local copy of dyna_next, since certain dynamic
	 * features can create other dynamic features when we apply the effect
	 */
	max = dyna_next;

	/*
	 * We clear the NEW_BORN flag. All dynamic features at this point
	 * are valid.
	 */
	for (i = 0; i < max; i++)
	{
		dyna_g[i].flags &= ~(DF1_NEW_BORN);
	}

	/* Process the stored dynamic features */
	for (i = 0; i < max; i++)
	{
		/* Get the grid info */
		g_ptr = &dyna_g[i];

		/* Removed grid, ignore */
		if (!(g_ptr->flags & (DF1_OCCUPIED))) continue;

		/*
		 * IMPORTANT: ignore any new dynamic feature created by another
		 * dynamic feature in a previous iteration.
		 * These new features will be processed in the next call to
		 * this function.
		 */
		if (g_ptr->flags & (DF1_NEW_BORN)) continue;

		/* Active timed terrain */
		/*
		 * Note: the effect of a timed feature is always applied,
		 * even when the player isn't in line of fire to track the
		 * value of the counter properly.
		 */
		if (g_ptr->counter > 0)
		{
			/* Decrement the counter */
			--g_ptr->counter;

			/* Do we need to apply the effect? */
			if (g_ptr->counter > 0) continue;
		}
		/* Regular terrain (or inactive timed terrain) */
		/*
		 * IMPORTANT: ignore any dynamic feature out of line of fire.
		 * Reasons:
		 * 1. Avoid possible dynamic-terrain-scum tactics.
		 * Example: throwing a fire ball into an oil lake followed by
		 * teleportation, etc.
		 * 2. Efficiency (only the grids near to the player are
		 * processed).
		 */
		else if (!player_can_fire_bold(g_ptr->y, g_ptr->x)) continue;

		/* Actually, apply the effect */
		process_dynamic_terrain_aux(g_ptr);
	}

	/* Compact the dyna_g array, if necessary */
	if (dyna_next > (dyna_cnt + 30))
	{
		/* Remove any gaps in dyna_g */
		for (i = j = 0; i < dyna_next; i++)
		{
			/* Found a gap, update only one of the indexes */
			if (!(dyna_g[i].flags & (DF1_OCCUPIED))) continue;

			/* Copy the grid info to its final position */
			if (i > j)
			{
				COPY(&dyna_g[j], &dyna_g[i], dynamic_grid_type);
			}

			/* One dynamic feature more */
			++j;
		}

		/* Set the value of dyna_next to the new compacted size */
		dyna_next = j;
	}
}


#define MAX_RACES 10

/*
 * Randomly select and return a monster race. The race is guaranteed to be one
 * of the most powerful races in the current level.
 * Return 0 if an error occurs
 */
s16b select_powerful_race(void)
{
	int i, j, n = 0;
	s16b r_idx;
	monster_race *r_ptr;
	/* The most powerful races in the level. Note dummy entry at the end */
	s16b races[MAX_RACES + 1];
	/* The weight of each one of these races */
	int rarities[MAX_RACES];
	bool *marked;

	/* Player cannot read clearly */
	if (p_ptr->timed[TMD_CONFUSED] || p_ptr->timed[TMD_IMAGE])
	{
		/* Pick a random monster race */
		while (TRUE)
		{
			/* Do it */
			r_idx = randint(z_info->r_max - 1);

			/* Get the race */
			r_ptr = &r_info[r_idx];

			/* Ignore empty races */
			if (!r_ptr->r_speed) continue;

			/* Ignore player ghosts (no name) */
			if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) continue;

			/* Found one */
			break;
		}

		/* Done */
		return (r_idx);
	}

	/* No monsters */
	if (mon_cnt == 0) return (0);

	marked = C_ZNEW(z_info->r_max, bool);

	/* Find the most powerful monsters */
	for (i = 0; i < mon_max; i++)
	{
		/* Get the race */
		r_idx = mon_list[i].r_idx;

		/* Ignore dead monsters */
		if (!r_idx) continue;

		/* Find if the race was already stored */
		if (marked[r_idx]) continue;

		marked[r_idx] = TRUE;

		r_ptr = &r_info[r_idx];

		/*
		 * Store the race ordering by monster power
		 * Note the use of dummy entry at the end to discard weaker races
		 */
		for (j = n; j > 0; j--)
		{
			/* Get the race */
			monster_race *r2_ptr = &r_info[races[j-1]];

			/* Monsters are ordered by monster power */
			if (r_ptr->mon_power <= r2_ptr->mon_power) break;

			/* Move the race (perhaps discarding it) */
			races[j] = races[j-1];
		}

		/* Store the race (maybe in the dummy entry, thus discarding it) */
		races[j] = r_idx;

		/* Check bounds and increment the race count if possible */
		if (n < MAX_RACES) ++n;
	}

	FREE(marked);

#if 0
	for (i = 0; i < n; i++)
	{
		r_ptr = &r_info[races[i]];

		msg_format("DEBUG: %50s - %lu.", r_name + r_ptr->name, r_ptr->mon_power);
	}
#endif

	/* Paranoia */
	if (n == 0) return (0);

	/* Determine the weight of each race */
	for (i = 0; i < n; i++)
	{
		/* Get the race */
		monster_race *r_ptr = &r_info[races[i]];

		/* Uniques are more "pickable" */
		if (r_ptr->flags1 & (RF1_UNIQUE)) rarities[i] = 5;

		/* Powerful monsters */
		else if (r_ptr->mon_power > mon_power_ave[effective_depth(p_ptr->depth)][CREATURE_NON_UNIQUE]) rarities[i] = 2;

		/* Normal monsters */
		else rarities[i] = 1;
	}

	/* Pick a monster race */
	r_idx = races[pick_random_item(rarities, n)];

	/* Done */
	return (r_idx);
}


/*
 * Format a inscription that spoils the presence of a monster of the given race in the level
 * and put it on the given buffer. max is the size of the buffer
 */
void format_monster_inscription(s16b r_idx, char inscr[], size_t max)
{
	int i;
	monster_type *m_ptr;
	monster_type m_body;
	monster_race *r_ptr;
	char name[80];

	/* Empty race or no monster found */
	if (r_idx < 1)
	{
		my_strcpy(inscr, "It doesn't say anything useful.", max);

		return;
	}

	/* Find a monster of that race */
	for (i = 0; i < mon_max; i++)
	{
		/* Get the monster */
		m_ptr = &mon_list[i];

		/* Found it? */
		if (m_ptr->r_idx == r_idx) break;
	}

	/* Not found */
	if (i >= mon_max)
	{
		/* Make a fake monster */
		m_ptr = &m_body;

		/* Clear the monster */
		WIPE(m_ptr, monster_type);

		/* Hack -- Set the race */
		m_ptr->r_idx = r_idx;

		/* Hack -- Always visible */
		m_ptr->ml = TRUE;

		/* Hack -- Set some dummy location */
		m_ptr->fy = p_ptr->py;
		m_ptr->fx = p_ptr->px;
	}

	/* Get the race */
	r_ptr = &r_info[r_idx];

	/* Sleeping monsters */
	if (m_ptr->m_timed[MON_TMD_SLEEP])
	{
		/* Get the monster name */
		monster_desc(name, sizeof(name), m_ptr, 0x180);

		/* Format */
		if (one_in_(2)) strnfmt(inscr, max, "Sssshhhh! %s is asleep!", name);

		else strnfmt(inscr, max, "Hurry! before %s wakes up!", name);

	}
	/* Special message for dragons */
	else if ((r_ptr->flags3 & (RF3_DRAGON)) && (r_ptr->d_char == 'D') && one_in_(2))
	{
		/* Get the monster name */
		monster_desc(name, sizeof(name), m_ptr, 0x180);

		/* Format */
		strnfmt(inscr, max, "It says: beware of the claws of %s!", name);
	}
	/* Special message for demons */
	else if ((r_ptr->flags3 & (RF3_DEMON)) && (r_ptr->d_char == 'U') && one_in_(2))
	{
		/* Get the monster name */
		monster_desc(name, sizeof(name), m_ptr, 0x188);

		/* Format */
		strnfmt(inscr, max, "It says: %s came from hell to hunt you!", name);
	}
	/* Normal case (1) */
	else if (one_in_(3))
	{
		/* Get the monster name */
		monster_desc(name, sizeof(name), m_ptr, 0x180);

		/* Format */
		strnfmt(inscr, max, "Its says: %s will attack you without mercy!", name);
	}
	/* Normal case (2) */
	else if (one_in_(2))
	{
		/* Get the monster name */
		monster_desc(name, sizeof(name), m_ptr, 0x188);

		/* Format */
		strnfmt(inscr, max, "It says: %s guards this dungeon!", name);
	}
	/* Normal case (3) */
	else
	{
		/* Get the monster name */
		monster_desc(name, sizeof(name), m_ptr, 0x188);

		/* Format */
		strnfmt(inscr, max, "It says: You will be face to face with %s!", name);
	}
}


/*
 * Display a message spoiling the presence of the most powerful monsters in the current level
 * x_idx is the index of the effect that triggered this action
 * IMPORTANT: the given effect can be deleted by this function
 */
void decipher_strange_inscription(int x_idx)
{
	/* Get the effect */
	effect_type *x_ptr = &x_list[x_idx];
	char name[200];

	/* Initial message */
	if (!x_ptr->x_r_idx)
	{
		msg_print("You try to decipher the inscription.");
	}
	else
	{
		msg_print("You try to decipher another part of the inscription.");
	}

	/* Hurt the player in rare occasions */
	if ((effective_depth(p_ptr->depth) > 10) && one_in_(75))
	{
		/* Get the feature under the effect */
		u16b feat = cave_feat[x_ptr->x_cur_y][x_ptr->x_cur_x];
		/* Hurt the player for 20% of his/her max HP */
		int dam = p_ptr->mhp / 5;
		int gf_type;

		if (one_in_(2))
		{
			/* Message */
			msg_c_format(MSG_NOTICE, "It was a chaotic spell!");

			/* Set type */
			gf_type = GF_CHAOS;
		}
		else
		{
			/* Message */
			msg_c_format(MSG_NOTICE, "It was a deadly spell!");

			/* Set type */
			gf_type = GF_NETHER;
		}

		/* Pause */
		message_flush();

		/* Paranoia */
		if (dam < 1) dam = 1;

		/* Get the feature name */
		feature_desc(name, sizeof(name), feat, TRUE, TRUE);

		/* Hurt the player */
		project_p(SOURCE_EFFECT, p_ptr->py, p_ptr->px, dam, gf_type,
			format("a spell inscribed on %s", name));

		/* Remove effect */
		delete_effect_idx(x_idx);

		/* Done */
		return;
	}

	/* Pick a race */
	x_ptr->x_r_idx = select_powerful_race();

	/* We are printing thrash  */
	if ((p_ptr->timed[TMD_CONFUSED] || p_ptr->timed[TMD_IMAGE]) && one_in_(2)) msg_print("You don't trust your eyes.");

	/* Format the message */
	format_monster_inscription(x_ptr->x_r_idx, name, sizeof(name));

	/* Hack -- Remember that this effect tried to show something without success */
	if (!x_ptr->x_r_idx) x_ptr->x_r_idx = -1;

	/* Show it */
	msg_c_format(MSG_NOTICE, name);
	message_flush();

	/* Destroy the effect sometimes */
	if (!DEBUG_MODE_ACTIVATED && one_in_(3))
	{
		/* Get the effect name */
		feature_desc(name, sizeof(name), x_ptr->x_f_idx, FALSE, TRUE);

		/* Message */
		msg_format("The %s falls apart!", name);

		/* Remove effect */
		delete_effect_idx(x_idx);
	}
}


/*
 * Hurt the player/aggravate monsters
 * This function can kill the player
 */
void hit_silent_watcher(int y, int x)
{
	u32b flg = PROJECT_PLAY | PROJECT_KILL | PROJECT_STOP;

	/* Calculate damage */
	int dam = (300 * MAX(1, effective_depth(p_ptr->depth))) / 100;

	/* Message */
	msg_c_format(MSG_NOTICE, "The silent watcher screams and curses at you!");

	/* Aggravate */
	aggravate_monsters(SOURCE_OTHER);

	/* Fire a bolt to the player  */
	project(SOURCE_OTHER, 0, y, x, p_ptr->py, p_ptr->px, dam, GF_NETHER, flg, 0, 0);

}


/*
 * Certain walls have special behavior when they are touched. This function
 * execute such actions (when do_action is TRUE) or check the presence of such
 * walls (when do_action is FALSE)
 * Return TRUE if an interesting wall was detected in the given location
 */
bool hit_wall(int y, int x, bool do_action)
{
	u16b feat = cave_feat[y][x];
	u16b dam = f_info[feat].dam_non_native;
	char name[80];

	/*We need to remember is the player saw this.*/
	feature_lore *f_l_ptr = &f_l_list[feat];

	/* Feature is dangerous for the player */
	if ((dam > 0) && !is_player_native(y, x))
	{
		cptr kb_str;
		int gf_type;

		/* Check player immunity to the feature */
		get_spell_type_from_feature(feat, &gf_type, NULL);
		if (is_player_immune(gf_type)) return (FALSE);

		/* Done */
		if (!do_action) return (TRUE);

		/* Get the name */
		feature_desc(name, sizeof(name), feat, TRUE, TRUE);

		/* Format the killer string */
		kb_str = format("touching %s", name);

		/* Take the hit */
		take_terrain_hit(dam, feat, kb_str);

		return (TRUE);
	}

	/* Touching silent watchers is dangerous */
	if (feat == FEAT_SILENT_WATCHER)
	{
		/* Done */
		if (!do_action) return (TRUE);

		if (player_can_see_bold(y, x))
		{
			/*Mark the lore*/
			f_l_ptr->f_l_flags3 |= (FF3_DYNAMIC);
		}

		/* Hurt the player */
		hit_silent_watcher(y, x);

		return (TRUE);
	}

	/* Teleport player */
	if (feat == FEAT_ETHEREAL_WALL)
	{
		/* Done */
		if (!do_action) return (TRUE);

		/* Hurt the player sometimes */
		if (one_in_(10))
		{
			/* Set flags */
			u32b flg = PROJECT_PLAY | PROJECT_GRID | PROJECT_KILL |
				PROJECT_ITEM | PROJECT_BOOM | PROJECT_WALL;

			/* Calculate damage (not centered on player) */
			int dam = (800 * MAX(1, effective_depth(p_ptr->depth))) / 100;

			/* Message */
			msg_c_format(MSG_NOTICE, "The wall explodes in a burst of light!");

			/* Remove the wall */
			cave_alter_feat(y, x, FS_TUNNEL);

			/* Burst of light */
			project(SOURCE_OTHER, 2, y, x, y, x, dam, GF_LIGHT, flg, 0, 0);
		}
		/* It works most of the time */
		else
		{
			/* Message */
			msg_print("A blast of bright light teleports you away!");

			/* Blind the player */
			if (!p_ptr->state.resist_blind && !p_ptr->state.resist_light)
			{
				/* Become blind */
				(void)inc_timed(TMD_BLIND, 10 + randint(10), TRUE);
			}

			/* Teleport */
			teleport_player(100 + rand_int(effective_depth(p_ptr->depth)), FALSE);
		}

		return (TRUE);
	}

	/* Process effects */
	if (cave_x_idx[y][x])
	{
		/* Get the first effect index */
		s16b x_idx = cave_x_idx[y][x];

		/* Traverse the effects */
		while (x_idx)
		{
			/* Get the effect */
			effect_type *x_ptr = &x_list[x_idx];

			/* Prepare the next effect */
			x_idx = x_ptr->next_x_idx;

			/* Found an inscription */
			if (x_ptr->x_type == EFFECT_INSCRIPTION)
			{
				/* Done */
				if (!do_action) return (TRUE);

				/* Can't see  */
				if (p_ptr->timed[TMD_BLIND] || no_light())
				{
					/* Message */
					msg_print("You can not see!");
				}
				else
				{
					/* Reveal monsters */
					decipher_strange_inscription((int)(x_ptr - x_list));
				}

				/* Done */
				return (TRUE);
			}
		}
	}

	return (FALSE);
}


/*
 * Clear level_flag and then rescan the current level searching for elemental features.
 * Update level_flag for each element found.
 */
void update_level_flag(void)
{
	int y, x;

	/* Reset the global flags */
	level_flag = 0;

	/* Debug */
	if (cheat_room)
	{
		msg_c_format(MSG_NOTICE, "Updating level flags.");
		disturb(0, 0);
	}

	/* Scan the dungeon */
	for (y = 1; y < (p_ptr->cur_map_hgt - 1); y++)
	{
		for (x = 1; x < (p_ptr->cur_map_wid - 1); x++)
		{
			/* Cache the feature */
			u16b feat = cave_feat[y][x];

			/* Is it an elemental feature? */
			if (feat_ff3_match(feat, TERRAIN_MASK))
			{
				/* Update the global flags */
				level_flag |= get_level_flag(feat);
			}
		}
	}
}


/*
 * Return ONLY ONE of the LF1_* flags that represents the given feature.
 * Return 0 if there isn't none.
 */
u32b get_level_flag(u16b feat)
{
	/* Get the elemental flags */
	u32b element_flags = feat_ff3_match(feat, TERRAIN_MASK);

	/* Analyze the type of the flags */
	switch (element_flags)
	{
		/* Special case. Boiling mud (hybrid element) */
		case ELEMENT_BMUD:
		{
			/* This flag actually doesn't match any of the ELEMENT_* flags */
			return (LF1_BMUD);
		}
		/* Special case. Boiling water (hybrid element) */
		case ELEMENT_BWATER:
		{
			/* This flag actually doesn't match any of the ELEMENT_* flags */
			return (LF1_BWATER);
		}
		/* Just don't do anything for other flags */
		default:
		{
			return (element_flags);
		}

	}
}


/*
 * Return ALL the LF1_* flags that represents the nativity settings of the
 * given monster race.
 * Return 0 if there isn't none.
 */
u32b get_level_flag_from_race(monster_race *r_ptr)
{
	/* Get the native flags */
	u32b element_flags = (r_ptr->r_native & (TERRAIN_MASK));

	/* Special case. Boiling mud (hybrid element) */
	if ((element_flags & ELEMENT_BMUD) == (ELEMENT_BMUD))
	{
		/* Just add the pseudo flag */
		/* Note that LF1_LAVA and LF1_MUD still are in the flags */
		element_flags |= LF1_BMUD;
	}

	/* Special case. Boiling mud (hybrid element) */
	if ((element_flags & ELEMENT_BWATER) == (ELEMENT_BWATER))
	{
		/* Just add the pseudo flag */
		/* Note that LF1_LAVA and LF1_WATER still are in the flags */
		element_flags |= LF1_BWATER;
	}

	/* Done */
	return (element_flags);
}


/*
 * Paste the name of the element given in flag into buf.
 * max is the maximum size of buf.
 * flag must contain ONLY ONE of the LF1_* flags.
 */
void describe_one_level_flag(char *buf, size_t max, u32b flag)
{
	/* Default name */
	const char *name = "unknown";

	/* Analyze the flag */
	switch (flag)
	{
		case LF1_FIRE: name = "fire"; break;
		case LF1_ACID: name = "acid"; break;
		case LF1_WATER: name = "water"; break;
		case LF1_MUD: name = "mud"; break;
		case LF1_LAVA: name = "lava"; break;
		case LF1_ICE: name = "ice"; break;
		case LF1_FOREST: name = "forest"; break;
		case LF1_OIL: name = "oil"; break;
		case LF1_SAND: name = "sand"; break;
		case LF1_BWATER: name = "boiling water"; break;
		case LF1_BMUD: name = "boiling mud"; break;
	}

	/* Copy the name */
	my_strcpy(buf, name, max);
}


/*
 * Show several messages describing all the LF1_* flags contained in the
 * given set of flags
 */
void debug_all_level_flags(u32b all_flags)
{
	int i;
	u32b flag = 1;

	/* Parse the bits of the given flags */
	for (i = 0; i < 32; i++, flag <<= 1)
	{
		/* The current flags is present in the set? */
		if (all_flags & flag)
		{
			char buf[80];

			/* Get the name */
			describe_one_level_flag(buf, sizeof(buf), flag);

			/* Message */
			msg_c_format(MSG_NOTICE, "The %s flag is present.", buf);
		}
	}
}


