/* File: mspells1.c */

/* Purpose: Monster spells (attack player) */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"


/*
 * And now for Intelligent monster attacks (including spells).
 *
 * Original idea and code by "DRS" (David Reeves Sward).
 * Major modifications by "BEN" (Ben Harrison).
 *
 * Give monsters more intelligent attack/spell selection based on
 * observations of previous attacks on the player, and/or by allowing
 * the monster to "cheat" and know the player status.
 *
 * Maintain an idea of the player status, and use that information
 * to occasionally eliminate "ineffective" spell attacks.  We could
 * also eliminate ineffective normal attacks, but there is no reason
 * for the monster to do this, since he gains no benefit.
 * Note that MINDLESS monsters are not allowed to use this code.
 * And non-INTELLIGENT monsters only use it partially effectively.
 *
 * Actually learn what the player resists, and use that information
 * to remove attacks or spells before using them.  This will require
 * much less space, if I am not mistaken.  Thus, each monster gets a
 * set of 32 bit flags, "smart*", build from the various "SM*_*" flags.
 *
 * This has the added advantage that attacks and spells are related.
 * The "smart_learn" option means that the monster "learns" the flags
 * that should be set, and "smart_cheat" means that he "knows" them.
 * So "smart_cheat" means that the "smart*" field is always up to date,
 * while "smart_learn" means that the "smart*" field is slowly learned.
 * Both of them have the same effect on the "choose spell" routine.
 */



/*
 * Internal probability routine
 */
static bool int_outof(monster_race *r_ptr, int prob)
{
	/* Non-Smart monsters are half as "smart" */
	if (!(r_ptr->flags2 & RF2_SMART)) prob = prob / 2;

	/* Roll the dice */
	return (randint0(100) < prob);
}



/*
 * Remove the "bad" spells from a spell list
 */
static void remove_bad_spells(int m_idx, u32b *f4p, u32b *f5p, u32b *f6p, u32b *fap)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	u32b f4 = (*f4p);
	u32b f5 = (*f5p);
	u32b f6 = (*f6p);
	u32b fa = (*fap);

	u32b smart1 = 0L;
	u32b smart2 = 0L;


	/* Too stupid to know anything */
	if (r_ptr->flags2 & RF2_STUPID) return;


	/* Must be cheating or learning */
	if (!smart_cheat && !smart_learn) return;


	/* Update acquired knowledge */
	if (smart_learn)
	{
		/* Hack -- Occasionally forget player status */
		if (randint0(100) < 1)
		{
			m_ptr->smart1 &= (SM1_CLONED | SM1_PET | SM1_FRIENDLY);
			m_ptr->smart2 = 0L;
		}

		/* Use the memorized flags */
		smart1 = m_ptr->smart1;
		smart2 = m_ptr->smart2;
	}


	/* Cheat if requested */
	if (smart_cheat)
	{
		/* Know basic info */
		if (p_ptr->resist_acid) smart1 |= (SM1_RES_ACID);
		if (p_ptr->oppose_acid) smart1 |= (SM1_OPP_ACID);
		if (p_ptr->immune_acid) smart1 |= (SM1_IMM_ACID);
		if (p_ptr->resist_elec) smart1 |= (SM1_RES_ELEC);
		if (p_ptr->oppose_elec) smart1 |= (SM1_OPP_ELEC);
		if (p_ptr->immune_elec) smart1 |= (SM1_IMM_ELEC);
		if (p_ptr->resist_fire) smart1 |= (SM1_RES_FIRE);
		if (p_ptr->oppose_fire) smart1 |= (SM1_OPP_FIRE);
		if (p_ptr->immune_fire) smart1 |= (SM1_IMM_FIRE);
		if (p_ptr->resist_cold) smart1 |= (SM1_RES_COLD);
		if (p_ptr->oppose_cold) smart1 |= (SM1_OPP_COLD);
		if (p_ptr->immune_cold) smart1 |= (SM1_IMM_COLD);

		/* Know poison info */
		if (p_ptr->resist_pois) smart1 |= (SM1_RES_POIS);
		if (p_ptr->oppose_pois) smart1 |= (SM1_OPP_POIS);

		/* Know special resistances */
		if (p_ptr->resist_neth) smart1 |= (SM1_RES_NETH);
		if (p_ptr->evil_equip || prace_is_(RACE_GHOST)) smart2 |= (SM2_IMM_NETH);
		if (p_ptr->resist_lite) smart1 |= (SM1_RES_LITE);
		if (p_ptr->resist_dark) smart1 |= (SM1_RES_DARK);
		if (p_ptr->evil_equip || WRAITH_FORM()) smart2 |= (SM2_IMM_DARK);
		if (p_ptr->resist_fear) smart1 |= (SM1_RES_FEAR);
		if (p_ptr->resist_conf) smart1 |= (SM1_RES_CONF);
		if (p_ptr->resist_chaos) smart1 |= (SM1_RES_CHAOS);
		if (p_ptr->resist_disen) smart1 |= (SM1_RES_DISEN);
		if (p_ptr->resist_blind) smart1 |= (SM1_RES_BLIND);
		if (p_ptr->resist_stone) smart1 |= (SM1_RES_STONE);
		if (p_ptr->resist_sound) smart1 |= (SM1_RES_SOUND);
		if (p_ptr->resist_shard) smart1 |= (SM1_RES_SHARD);
		if (p_ptr->resist_water) smart2 |= (SM2_RES_WATER);
		if (p_ptr->zoshonel_protect) smart2 |= (SM2_IMM_PLASMA);
		if (p_ptr->resist_time) smart2 |= (SM2_RES_TIME);
		if (p_ptr->anti_tele || p_ptr->earth_spike) smart2 |= (SM2_IMM_TELE);
		if ((rp_ptr->r_flags & PRF_DEMON) || (cp_ptr->c_flags & PCF_DEMON)) smart2 |= (SM2_IMM_DRAIN);
		if ((rp_ptr->r_flags & PRF_UNDEAD) || (cp_ptr->c_flags & PCF_UNDEAD)) smart2 |= (SM2_IMM_DRAIN);
		if (p_ptr->wind_guard)
		{
			smart2 |= (SM2_IMM_AVOID0);
			if (p_ptr->stat_use[A_INT] >= (18 + 150)) smart2 |= (SM2_IMM_AVOID1);
			if (p_ptr->stat_use[A_INT] >= (18 + 200)) smart2 |= (SM2_IMM_AVOID2);
		}
		if (p_ptr->reflect) smart1 |= (SM1_IMM_REFLECT);

		/* Know bizarre "resistances" */
		if (p_ptr->free_act) smart1 |= (SM1_IMM_FREE);
		if (!p_ptr->msp) smart1 |= (SM1_IMM_MANA);
	}


	/* Nothing known */
	if (!smart1 && !smart2) return;


	if (smart1 & SM1_IMM_ACID)
	{
		f4 &= ~(RF4_BR_ACID);
		f5 &= ~(RF5_BA_ACID | RF5_BO_ACID);
		fa &= ~(RFA_GNOME);
	}
	else if ((smart1 & (SM1_OPP_ACID)) && (smart1 & (SM1_RES_ACID)))
	{
		if (int_outof(r_ptr, 80)) f4 &= ~(RF4_BR_ACID);
		if (int_outof(r_ptr, 80)) f5 &= ~(RF5_BA_ACID);
		if (int_outof(r_ptr, 80)) f5 &= ~(RF5_BO_ACID);
		if (int_outof(r_ptr, 80)) fa &= ~(RFA_GNOME);
	}
	else if ((smart1 & (SM1_OPP_ACID)) || (smart1 & (SM1_RES_ACID)))
	{
		if (int_outof(r_ptr, 30)) f4 &= ~(RF4_BR_ACID);
		if (int_outof(r_ptr, 30)) f5 &= ~(RF5_BA_ACID);
		if (int_outof(r_ptr, 30)) f5 &= ~(RF5_BO_ACID);
		if (int_outof(r_ptr, 30)) fa &= ~(RFA_GNOME);
	}


	if (smart1 & (SM1_IMM_ELEC))
	{
		f4 &= ~(RF4_BR_ELEC);
		f5 &= ~(RF5_BA_ELEC | RF5_BO_ELEC);
		fa &= ~(RFA_THUNDERBIRD);
	}
	else if ((smart1 & (SM1_OPP_ELEC)) && (smart1 & (SM1_RES_ELEC)))
	{
		if (int_outof(r_ptr, 80)) f4 &= ~(RF4_BR_ELEC);
		if (int_outof(r_ptr, 80)) f5 &= ~(RF5_BA_ELEC);
		if (int_outof(r_ptr, 80)) f5 &= ~(RF5_BO_ELEC);
		if (int_outof(r_ptr, 80)) fa &= ~(RFA_THUNDERBIRD);
	}
	else if ((smart1 & (SM1_OPP_ELEC)) || (smart1 & (SM1_RES_ELEC)))
	{
		if (int_outof(r_ptr, 30)) f4 &= ~(RF4_BR_ELEC);
		if (int_outof(r_ptr, 30)) f5 &= ~(RF5_BA_ELEC);
		if (int_outof(r_ptr, 30)) f5 &= ~(RF5_BO_ELEC);
		if (int_outof(r_ptr, 30)) fa &= ~(RFA_THUNDERBIRD);
	}


	if (smart1 & (SM1_IMM_FIRE))
	{
		f4 &= ~(RF4_BR_FIRE);
		f5 &= ~(RF5_BA_FIRE | RF5_BO_FIRE);
		fa &= ~(RFA_SALAMANDER);
	}
	else if ((smart1 & (SM1_OPP_FIRE)) && (smart1 & (SM1_RES_FIRE)))
	{
		if (int_outof(r_ptr, 80)) f4 &= ~(RF4_BR_FIRE);
		if (int_outof(r_ptr, 80)) f5 &= ~(RF5_BA_FIRE);
		if (int_outof(r_ptr, 80)) f5 &= ~(RF5_BO_FIRE);
		if (int_outof(r_ptr, 80)) fa &= ~(RFA_SALAMANDER);
	}
	else if ((smart1 & (SM1_OPP_FIRE)) || (smart1 & (SM1_RES_FIRE)))
	{
		if (int_outof(r_ptr, 30)) f4 &= ~(RF4_BR_FIRE);
		if (int_outof(r_ptr, 30)) f5 &= ~(RF5_BA_FIRE);
		if (int_outof(r_ptr, 30)) f5 &= ~(RF5_BO_FIRE);
		if (int_outof(r_ptr, 30)) fa &= ~(RFA_SALAMANDER);
	}


	if (smart1 & (SM1_IMM_COLD))
	{
		f4 &= ~(RF4_BR_COLD);
		f5 &= ~(RF5_BA_COLD | RF5_BO_COLD | RF5_BO_ICEE);
		fa &= ~(RFA_FENRER);
	}
	else if ((smart1 & (SM1_OPP_COLD)) && (smart1 & (SM1_RES_COLD)))
	{
		if (int_outof(r_ptr, 80)) f4 &= ~(RF4_BR_COLD);
		if (int_outof(r_ptr, 80)) f5 &= ~(RF5_BA_COLD);
		if (int_outof(r_ptr, 80)) f5 &= ~(RF5_BO_COLD);
		if (int_outof(r_ptr, 80)) f5 &= ~(RF5_BO_ICEE);
		if (int_outof(r_ptr, 80)) fa &= ~(RFA_FENRER);
	}
	else if ((smart1 & (SM1_OPP_COLD)) || (smart1 & (SM1_RES_COLD)))
	{
		if (int_outof(r_ptr, 30)) f4 &= ~(RF4_BR_COLD);
		if (int_outof(r_ptr, 30)) f5 &= ~(RF5_BA_COLD);
		if (int_outof(r_ptr, 30)) f5 &= ~(RF5_BO_COLD);
		if (int_outof(r_ptr, 20)) f5 &= ~(RF5_BO_ICEE);
		if (int_outof(r_ptr, 30)) fa &= ~(RFA_FENRER);
	}


	if ((smart1 & (SM1_OPP_POIS)) && (smart1 & (SM1_RES_POIS)))
	{
		if (int_outof(r_ptr, 80)) f4 &= ~(RF4_BR_POIS);
		if (int_outof(r_ptr, 80)) f5 &= ~(RF5_BA_POIS);
		if (int_outof(r_ptr, 60)) f4 &= ~(RF4_BA_NUKE);
		if (int_outof(r_ptr, 60)) f4 &= ~(RF4_BR_NUKE);
	}
	else if ((smart1 & (SM1_OPP_POIS)) || (smart1 & (SM1_RES_POIS)))
	{
		if (int_outof(r_ptr, 30)) f4 &= ~(RF4_BR_POIS);
		if (int_outof(r_ptr, 30)) f5 &= ~(RF5_BA_POIS);
	}


	if (smart2 & (SM2_IMM_NETH))
	{
		f4 &= ~(RF4_BR_NETH);
		f5 &= ~(RF5_BA_NETH | RF5_BO_NETH);
	}
	else if (smart1 & (SM1_RES_NETH))
	{
		if (int_outof(r_ptr, 20)) f4 &= ~(RF4_BR_NETH);
		if (int_outof(r_ptr, 50)) f5 &= ~(RF5_BA_NETH);
		if (int_outof(r_ptr, 50)) f5 &= ~(RF5_BO_NETH);
	}

	if (smart1 & (SM1_RES_LITE))
	{
		if (int_outof(r_ptr, 50)) f4 &= ~(RF4_BR_LITE);
		if (int_outof(r_ptr, 50)) f5 &= ~(RF5_BA_LITE);
	}

	if (smart2 & (SM2_IMM_DARK))
	{
		f4 &= ~(RF4_BR_DARK);
		f5 &= ~(RF5_BA_DARK);
	}
	else if (smart1 & (SM1_RES_DARK))
	{
		if (int_outof(r_ptr, 50)) f4 &= ~(RF4_BR_DARK);
		if (int_outof(r_ptr, 50)) f5 &= ~(RF5_BA_DARK);
	}

	if (smart1 & (SM1_RES_FEAR))
	{
		f5 &= ~(RF5_SCARE);
	}

	if (smart1 & (SM1_RES_CONF))
	{
		f5 &= ~(RF5_CONF);
		if (int_outof(r_ptr, 50)) f4 &= ~(RF4_BR_CONF);
	}

	if (smart1 & (SM1_RES_CHAOS))
	{
		if (int_outof(r_ptr, 20)) f4 &= ~(RF4_BR_CHAO);
		if (int_outof(r_ptr, 50)) f4 &= ~(RF4_BA_CHAO);
	}

	if (smart1 & (SM1_RES_DISEN))
	{
		if (int_outof(r_ptr, 40)) f4 &= ~(RF4_BR_DISE);
	}

	if (smart1 & (SM1_RES_BLIND))
	{
		f5 &= ~(RF5_BLIND);
	}

	if (smart1 & (SM1_RES_STONE))
	{
		fa &= ~(RFA_STONE_GAZE);
		if (int_outof(r_ptr, 50)) f4 &= ~(RF4_BR_STON);
		if (int_outof(r_ptr, 50)) fa &= ~(RFA_PETRO_CLOUD);
	}

	if (smart1 & (SM1_RES_SOUND))
	{
		if (int_outof(r_ptr, 50)) f4 &= ~(RF4_BR_SOUN);
	}

	if (smart1 & (SM1_RES_SHARD))
	{
		if (int_outof(r_ptr, 30)) f4 &= ~(RF4_ROCKET);
		if (int_outof(r_ptr, 40)) f4 &= ~(RF4_BR_SHAR);
		if (int_outof(r_ptr, 50)) fa &= ~(RFA_SAND_STORM);
	}

	if (smart2 & (SM2_RES_WATER))
	{
		if (int_outof(r_ptr, 50)) f5 &= ~(RF5_BA_WATE);
		if (int_outof(r_ptr, 50)) f5 &= ~(RF5_BO_WATE);
	}

	if (smart2 & (SM2_IMM_PLASMA))
	{
		f4 &= ~(RF4_BR_PLAS);
		f5 &= ~(RF5_BO_PLAS);
	}

	if (smart2 & (SM2_RES_TIME))
	{
		if (int_outof(r_ptr, 50)) f4 &= ~(RF4_BR_TIME);
	}

	if (smart2 & (SM2_IMM_TELE))
	{
		f6 &= ~(RF6_TELE_TO | RF6_TELE_AWAY | RF6_TELE_LEVEL);
	}

	/* Lancelot Only */
	/* if (smart2 & (SM2_IMM_DRAIN)) {} */

	if (smart2 & (SM2_IMM_AVOID2))
	{
		f4 &= ~(RF4_ROCKET | RF4_SHOOT);
		f5 &= ~(RF5_BO_ACID | RF5_BO_ELEC | RF5_BO_FIRE | RF5_BO_COLD |
		        RF5_BO_NETH | RF5_BO_WATE | RF5_BO_MANA | RF5_BO_PLAS |
		        RF5_BO_ICEE | RF5_MISSILE);
	}
	else if (smart2 & (SM2_IMM_AVOID1))
	{
		f4 &= ~(RF4_SHOOT);
		f5 &= ~(RF5_BO_ACID | RF5_BO_ELEC | RF5_BO_FIRE | RF5_BO_COLD |
		        RF5_BO_NETH | RF5_BO_WATE | RF5_BO_MANA | RF5_BO_PLAS |
		        RF5_BO_ICEE | RF5_MISSILE);
	}
	else if (smart2 & (SM2_IMM_AVOID0))
	{
		f4 &= ~(RF4_SHOOT);
	}


	if (smart1 & (SM1_IMM_REFLECT))
	{
		if (int_outof(r_ptr, 150)) f5 &= ~(RF5_BO_COLD);
		if (int_outof(r_ptr, 150)) f5 &= ~(RF5_BO_FIRE);
		if (int_outof(r_ptr, 150)) f5 &= ~(RF5_BO_ACID);
		if (int_outof(r_ptr, 150)) f5 &= ~(RF5_BO_ELEC);
		if (int_outof(r_ptr, 150)) f5 &= ~(RF5_BO_NETH);
		if (int_outof(r_ptr, 150)) f5 &= ~(RF5_BO_WATE);
		if (int_outof(r_ptr, 150)) f5 &= ~(RF5_BO_MANA);
		if (int_outof(r_ptr, 150)) f5 &= ~(RF5_BO_PLAS);
		if (int_outof(r_ptr, 150)) f5 &= ~(RF5_BO_ICEE);
		if (int_outof(r_ptr, 150)) f5 &= ~(RF5_MISSILE);
	}

	if (smart1 & (SM1_IMM_FREE))
	{
		f5 &= ~(RF5_HOLD | RF5_SLOW);
	}

	if (smart1 & (SM1_IMM_MANA))
	{
		f5 &= ~(RF5_DRAIN_MANA);
	}

	if (f6 & RF6_SPECIAL)
	{
		switch (m_ptr->r_idx)
		{
		case MON_OZ:
			if (smart2 & (SM2_IMM_DARK))
			{
				f6 &= ~(RF6_SPECIAL);
			}
			else if (smart1 & (SM1_RES_DARK))
			{
				if (int_outof(r_ptr, 50)) f6 &= ~(RF6_SPECIAL);
			}
			break;
		case MON_MARTYM:
			if (smart2 & (SM2_RES_WATER))
			{
				if (int_outof(r_ptr, 50)) f6 &= ~(RF6_SPECIAL);
			}
			break;
		case MON_ANDORAS:
			if (smart1 & (SM1_IMM_ELEC))
			{
				f6 &= ~(RF6_SPECIAL);
			}
			else if ((smart1 & (SM1_OPP_ELEC)) && (smart1 & (SM1_RES_ELEC)))
			{
				if (int_outof(r_ptr, 80)) f6 &= ~(RF6_SPECIAL);
			}
			else if ((smart1 & (SM1_OPP_ELEC)) || (smart1 & (SM1_RES_ELEC)))
			{
				if (int_outof(r_ptr, 30)) f6 &= ~(RF6_SPECIAL);
			}
			break;
		case MON_LANCELOT:
			if (smart2 & (SM2_IMM_DRAIN))
			{
				f6 &= ~(RF6_SPECIAL);
			}
			break;
		}
	}

	/* XXX XXX XXX No spells left? */
	/* if (!f4 && !f5 && !f6 && !fa) ... */

	(*f4p) = f4;
	(*f5p) = f5;
	(*f6p) = f6;
	(*fap) = fa;
}


/*
 * Determine if there is a space near the player in which
 * a summoned creature can appear
 */
bool summon_possible(int who, int y1, int x1)
{
	int y, x;
	bool not_powerful = !(r_info[m_list[who].r_idx].flags2 & RF2_POWERFUL);

	/* Start at the player's location, and check 2 grids in each dir */
	for (y = y1 - 2; y <= y1 + 2; y++)
	{
		for (x = x1 - 2; x <= x1 + 2; x++)
		{
			/* Ignore illegal locations */
			if (!in_bounds(y, x)) continue;

			/* Only check a circular area */
			if (distance(y1, x1, y, x)>2) continue;

			/* Hack: no summon on anti-magic field */
			if (not_powerful && is_anti_magic_grid(who, y, x)) continue;

			/* Require empty floor grid in line of sight */
			if ((cave_empty_bold(y, x) || (cave[y][x].feat == FEAT_TREES)) && los(y1, x1, y, x) && los(y, x, y1, x1)) return (TRUE);
		}
	}

	return FALSE;
}


static bool raise_possible(monster_type *m_ptr)
{
	int xx, yy;
	int y = m_ptr->fy;
	int x = m_ptr->fx;
	s16b this_o_idx, next_o_idx = 0;
	cave_type *c_ptr;
	monster_race *z_ptr;

	for (xx = x - 5; xx <= x + 5; xx++)
	{
		for (yy = y - 5; yy <= y + 5; yy++)
		{
			if (distance(y, x, yy, xx) > 5) continue;
			if (!los(y, x, yy, xx)) continue;

			c_ptr = &cave[yy][xx];
			/* Scan the pile of objects */
			for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
			{
				/* Acquire object */
				object_type *o_ptr = &o_list[this_o_idx];

				/* Acquire next object */
				next_o_idx = o_ptr->next_o_idx;

				/* Known to be worthless? */
				if (o_ptr->tval == TV_CORPSE)
				{
					z_ptr = &r_info[o_ptr->pval];
					if (monster_has_hostile_alignment(m_ptr, z_ptr)) continue;

					return TRUE;
				}
			}
		}
	}
	return FALSE;
}


/*
 * Originally, it was possible for a friendly to shoot another friendly.
 * Change it so a "clean shot" means no equally friendly monster is
 * between the attacker and target.
 */
/*
 * Determine if a bolt spell will hit the player.
 *
 * This is exactly like "projectable", but it will
 * return FALSE if a monster is in the way.
 * no equally friendly monster is
 * between the attacker and target.
 */
bool clean_shot(int y1, int x1, int y2, int x2, bool friend, bool strict)
{
	/* Must be the same as projectable() */

	int i, y, x;

	int grid_n = 0;
	u16b grid_g[512];

	/* Check the projection path */
	grid_n = project_path(grid_g, MAX_RANGE, y1, x1, y2, x2, 0);

	/* No grid is ever projectable from itself */
	if (!grid_n) return (FALSE);

	/* Final grid */
	y = GRID_Y(grid_g[grid_n-1]);
	x = GRID_X(grid_g[grid_n-1]);

	/* May not end in an unrequested grid */
	if ((y != y2) || (x != x2)) return (FALSE);

	for (i = 0; i < grid_n; i++)
	{
		y = GRID_Y(grid_g[i]);
		x = GRID_X(grid_g[i]);

		if ((cave[y][x].m_idx > 0) && !((y == y2) && (x == x2)))
		{
			monster_type *m_ptr = &m_list[cave[y][x].m_idx];
			if ((friend == is_pet(m_ptr)) || strict)
			{
				return (FALSE);
			}
		}
		/* Pets may not shoot through the character - TNB */
		if ((y == py) && (x == px))
		{
			if (friend) return (FALSE);
		}
	}

	return (TRUE);
}

/*
 * Cast a bolt at the player
 * Stop if we hit a monster
 * Affect monsters and the player
 */
static void bolt(int y, int x, int m_idx, int typ, int dam_hp)
{
	u32b flg = PROJECT_STOP | PROJECT_KILL | PROJECT_PLAYER | PROJECT_REFLECTABLE | PROJECT_AVOIDABLE;
	int mod_elem_mode = MODIFY_ELEM_MODE_MAGIC;

	switch (typ)
	{
	case GF_PHYSICAL:
	case GF_BLUNT:
	case GF_EDGED:
		flg &= ~(PROJECT_REFLECTABLE);
		mod_elem_mode = MODIFY_ELEM_MODE_FIRE;
		break;
	}

	/* Target the player with a bolt attack */
	(void)project(m_idx, 0, y, x, dam_hp, typ, flg, mod_elem_mode);
}

static void beam(int y, int x, int m_idx, int typ, int dam_hp)
{
	u32b flg = PROJECT_BEAM | PROJECT_KILL | PROJECT_THRU | PROJECT_PLAYER;
	int mod_elem_mode = MODIFY_ELEM_MODE_MAGIC;

	switch (typ)
	{
	case GF_PHYSICAL:
	case GF_BLUNT:
	case GF_EDGED:
		mod_elem_mode = MODIFY_ELEM_MODE_FIRE;
		break;
	}

	/* Target the player with a bolt attack */
	(void)project(m_idx, 0, y, x, dam_hp, typ, flg, mod_elem_mode);
}


/*
 * Cast a breath (or ball) attack at the player
 * Pass over any monsters that may be in the way
 * Affect grids, objects, monsters, and the player
 */
static void breath(int y, int x, int m_idx, int typ, int dam_hp, int rad, bool breath, bool no_reduce)
{
	u32b flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_PLAYER;
	int mod_elem_mode = MODIFY_ELEM_MODE_MAGIC;

	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	/* Determine the radius of the blast */
	if ((rad < 1) && breath) rad = (r_ptr->flags2 & (RF2_POWERFUL)) ? 3 : 2;

	/* Handle breath attacks */
	if (breath) rad = 0 - rad;

	switch (typ)
	{
	case GF_ROCKET:
		flg |= (PROJECT_STOP | PROJECT_AVOIDABLE);
		mod_elem_mode = MODIFY_ELEM_MODE_FIRE;
		break;
	case GF_MIND_BLAST:
	case GF_BRAIN_SMASH:
	case GF_CAUSE_1:
	case GF_CAUSE_2:
	case GF_CAUSE_3:
	case GF_CAUSE_4:
	case GF_HAND_DOOM:
		flg |= (PROJECT_HIDE);
		break;
	}

	if (no_reduce) flg |= PROJECT_NO_REDUCE;

	/* Target the player with a ball attack */
	(void)project(m_idx, rad, y, x, dam_hp, typ, flg, mod_elem_mode);
}


static void special_blow(int y, int x, int m_idx, int typ, int dam_hp)
{
	u32b flg = PROJECT_HIDE | PROJECT_STOP | PROJECT_KILL | PROJECT_GRID | PROJECT_PLAYER;

	project_length = 2;

	/* Target the player with a special blow */
	(void)project(m_idx, 0, y, x, dam_hp, typ, flg, MODIFY_ELEM_MODE_MAGIC);
}


u32b get_curse(int power, object_type *o_ptr)
{
	u32b new_curse;

	while(1)
	{
		new_curse = (1 << (randint0(MAX_CURSE)+4));
		if (power == 2)
		{
			if (!(new_curse & TRC_HEAVY_MASK)) continue;
		}
		else if (power == 1)
		{
			if (new_curse & TRC_SPECIAL_MASK) continue;
		}
		else if (power == 0)
		{
			if (new_curse & TRC_HEAVY_MASK) continue;
		}
		if (((o_ptr->tval < TV_BOW) || (o_ptr->tval > TV_SWORD)) && (new_curse == TRC_LOW_MELEE)) continue;
		if (((o_ptr->tval < TV_BOOTS) || (o_ptr->tval > TV_HARD_ARMOR)) && (new_curse == TRC_LOW_AC)) continue;
		break;
	}
	return new_curse;
}

void curse_equipment(int chance, int heavy_chance)
{
	bool        changed = FALSE;
	int         curse_power = 0;
	u32b        new_curse;
	u32b        oflgs[TR_FLAG_SIZE];
	object_type *o_ptr = &inventory[INVEN_RARM + randint0(12)];
	char o_name[MAX_NLEN];

	if (randint1(100) > chance) return;

	if (!o_ptr->k_idx) return;

	object_flags(o_ptr, oflgs);

	object_desc(o_name, o_ptr, (OD_OMIT_PREFIX | OD_NAME_ONLY));

	/* Extra, biased saving throw for blessed items */
	if (have_flag(oflgs, TR_BLESSED) && (randint1(888) > chance))
	{
#ifdef JP
		msg_format("%sは呪いを跳ね返した！", o_name,
#else
		msg_format("Your %s resist%s cursing!", o_name,
#endif

			((o_ptr->number > 1) ? "" : "s"));
		/* Hmmm -- can we wear multiple items? If not, this is unnecessary */
		return;
	}

	if ((randint1(100) <= heavy_chance) &&
		(object_is_artifact(o_ptr) || object_is_ego(o_ptr)))
	{
		if (!(o_ptr->curse_flags & TRC_HEAVY_CURSE))
			changed = TRUE;
		o_ptr->curse_flags |= TRC_HEAVY_CURSE;
		o_ptr->curse_flags |= TRC_CURSED;
		curse_power++;
	}
	else
	{
		if (!object_is_cursed(o_ptr))
			changed = TRUE;
		o_ptr->curse_flags |= TRC_CURSED;
	}
	if (heavy_chance >= 50) curse_power++;

	new_curse = get_curse(curse_power, o_ptr);
	if (!(o_ptr->curse_flags & new_curse))
	{
		changed = TRUE;
		o_ptr->curse_flags |= new_curse;
	}

	if (changed)
	{
#ifdef JP
		msg_format("悪意に満ちた黒いオーラが%sをとりまいた...", o_name);
#else
		msg_format("There is a malignant black aura surrounding %s...", o_name);
#endif

		o_ptr->feeling = FEEL_NONE;
	}
	p_ptr->update |= (PU_BONUS);
}


/*
 * Return TRUE if a spell is good for hurting the player (directly).
 */
static bool spell_attack(u16b spell)
{
	/* All RF4 spells hurt (except for shriek and dispel) */
	if (spell < 128 && spell > 98) return (TRUE);

	/* Various "ball" spells */
	if (spell >= 128 && spell <= 128 + 8) return (TRUE);

	/* "Cause wounds" and "bolt" spells */
	if (spell >= 128 + 12 && spell < 128 + 27) return (TRUE);

	/* Hand of Doom */
	if (spell == 160 + 1) return (TRUE);

	/* Godly Spear */
	if (spell == 160 + 11) return (TRUE);

	/* Additional attacks (1) */
	if (spell >= 288 && spell <= 288 + 9) return (TRUE);

	/* Additional attacks (2) */
	if (spell >= 288 + 12 && spell <= 288 + 20) return (TRUE);

	/* Additional attacks (3) */
	if (spell >= 288 + 23 && spell <= 288 + 24) return (TRUE);

	/* Doesn't hurt */
	return (FALSE);
}


/*
 * Return TRUE if a spell is good for escaping.
 */
static bool spell_escape(u16b spell)
{
	/* Blink or Teleport */
	if (spell == 160 + 4 || spell == 160 + 5) return (TRUE);

	/* Teleport the player away */
	if (spell == 160 + 9 || spell == 160 + 10) return (TRUE);

	/* Isn't good for escaping */
	return (FALSE);
}

/*
 * Return TRUE if a spell is good for annoying the player.
 */
static bool spell_annoy(u16b spell)
{
	/* Shriek */
	if (spell == 96 + 0) return (TRUE);

	/* Shift own element */
	if (spell == 96 + 1) return (TRUE);

	/* Brain smash, et al (added curses) */
	if (spell >= 128 + 9 && spell <= 128 + 14) return (TRUE);

	/* Scare, confuse, blind, slow, paralyze */
	if (spell >= 128 + 27 && spell <= 128 + 31) return (TRUE);

	/* Teleport to */
	if (spell == 160 + 8) return (TRUE);

	/* Teleport level */
	if (spell == 160 + 10) return (TRUE);

	/* Darkness, make traps, cause amnesia */
	if (spell >= 160 + 12 && spell <= 160 + 14) return (TRUE);

	/* Doesn't annoy */
	return (FALSE);
}

/*
 * Return TRUE if a spell summons help.
 */
static bool spell_summon(u16b spell)
{
	/* All summon spells */
	if (spell >= 160 + 16 && spell <= 160 + 31) return (TRUE);

	if (spell >= 288 + 21 && spell <= 288 + 22) return (TRUE);

	/* Doesn't summon */
	return (FALSE);
}


/*
 * Return TRUE if a spell raise-dead.
 */
static bool spell_raise(u16b spell)
{
	/* All raise-dead spells */
	if (spell == 160 + 15) return (TRUE);

	/* Doesn't summon */
	return (FALSE);
}


/*
 * Return TRUE if a spell is good in a tactical situation.
 */
static bool spell_tactic(u16b spell)
{
	/* Blink */
	if (spell == 160 + 4) return (TRUE);

	/* Not good */
	return (FALSE);
}

/*
 * Return TRUE if a spell makes invulnerable.
 */
static bool spell_invulner(u16b spell)
{
	/* Invulnerability */
	if (spell == 160 + 3) return (TRUE);

	/* No invulnerability */
	return (FALSE);
}

/*
 * Return TRUE if a spell hastes.
 */
static bool spell_haste(u16b spell)
{
	/* Haste self */
	if (spell == 160 + 0) return (TRUE);

	/* Not a haste spell */
	return (FALSE);
}


/*
 * Return TRUE if a spell stops time.
 */
static bool spell_stop_time(u16b spell)
{
	/* Stop the Time */
	if (spell == 160 + 6) return (TRUE);

	/* Not a time stop spell */
	return (FALSE);
}


/*
 * Return TRUE if a spell special.
 */
static bool spell_special(u16b spell)
{
	/* Special */
	if (spell == 160 + 7) return (TRUE);

	/* Not a special spell */
	return (FALSE);
}


/*
 * Return TRUE if a spell penetrating invulnerability.
 */
static bool spell_inv_pen(u16b spell)
{
	/* Godly Spear */
	if (spell == 160 + 11) return (TRUE);

	/* *Element* attacks */
	if (spell >= 288 && spell <= 288 + 7) return (TRUE);

	/* *Element* beam */
	if (spell == 288 + 24) return (TRUE);

	/* Not a penetrating spell */
	return (FALSE);
}


/*
 * Return TRUE if a spell is good for healing.
 */
static bool spell_heal(u16b spell)
{
	/* Heal */
	if (spell == 160 + 2) return (TRUE);

	/* No healing */
	return (FALSE);
}


/*
 * Return TRUE if a spell is good for dispel.
 */
static bool spell_dispel(u16b spell)
{
	/* Dispel */
	if (spell == 96 + 2) return (TRUE);

	/* No dispel */
	return (FALSE);
}


/*
 * Return TRUE if a spell is good for erasing elements.
 */
static bool spell_erase(u16b spell)
{
	/* Erasing elements */
	if (spell == 288 + 10) return (TRUE);

	/* Can't erase elements */
	return (FALSE);
}


/*
 * Return TRUE if a spell is good for change player's element.
 */
static bool spell_ch_elem(u16b spell)
{
	/* Change player's element */
	if (spell == 288 + 11) return (TRUE);

	/* Can't change player's element */
	return (FALSE);
}


/*
 * Return TRUE if a spell is inate spell.
 */
bool spell_is_inate(u16b spell)
{
	/* Set RF4 */
	if (spell == (96 + 0)) return TRUE;
	if ((spell >= (96 + 3)) && (spell <= (96 + 27))) return TRUE;
	if (spell == (96 + 29)) return TRUE;
	if (spell == (96 + 31)) return TRUE;

	/* Set RF5 - nothing */

	/* Set RF6 */
	if (spell == (160 + 7)) return TRUE;

	/* Set RFA */
	if ((spell >= (288 + 4)) && (spell <= (288 + 7))) return TRUE;
	if (spell == (288 + 18)) return TRUE;

	/* This spell is not "inate" */
	return FALSE;
}


/*
 * Check should monster cast dispel spell.
 */
static bool dispel_check(int m_idx)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	/* Invulnabilty */
	if (p_ptr->invuln) return (TRUE);

	/* Wraith form */
	if (p_ptr->wraith_form)
	{
		if (!p_ptr->wraith_form_perm) return (TRUE);
	}

	/* Shield */
	if (p_ptr->shield) return (TRUE);

	/* Magic defence */
	if (p_ptr->magicdef) return (TRUE);

	/* Multi Shadow */
	if (p_ptr->multishadow) return (TRUE);

	/* Berserk Strength */
	if (p_ptr->shero)
	{
		if (!((inventory[INVEN_RARM].k_idx && (inventory[INVEN_RARM].name1 == ART_BERSERK)) ||
		      (inventory[INVEN_LARM].k_idx && (inventory[INVEN_LARM].name1 == ART_BERSERK))))
			return (TRUE);
	}

	/* Elemental resistances */
	if (r_ptr->flags4 & RF4_BR_ACID)
	{
		if (!p_ptr->immune_acid && p_ptr->oppose_acid) return (TRUE);
	}

	if (r_ptr->flags4 & RF4_BR_FIRE)
	{
		if (!p_ptr->immune_fire && p_ptr->oppose_fire) return (TRUE);
	}

	if (r_ptr->flags4 & RF4_BR_ELEC)
	{
		if (!p_ptr->immune_elec && p_ptr->oppose_elec) return (TRUE);
	}

	if (r_ptr->flags4 & RF4_BR_COLD)
	{
		if (!p_ptr->immune_cold && p_ptr->oppose_cold) return (TRUE);
	}

	if (r_ptr->flags4 & (RF4_BR_POIS | RF4_BR_NUKE))
	{
		if (p_ptr->oppose_pois) return (TRUE);
	}

	/* Teleportation resistance */
	if (r_ptr->flags6 & (RF6_TELE_TO | RF6_TELE_AWAY | RF6_TELE_LEVEL))
	{
		if (p_ptr->earth_spike) return (TRUE);
	}

	/* Avoidance to arrows */
	if (p_ptr->wind_guard)
	{
		if (r_ptr->flags4 & RF4_SHOOT) return (TRUE);
		if (((r_ptr->flags4 & (RF4_BOLT_MASK & ~(RF4_ROCKET | RF4_SHOOT_GUN))) ||
		     (r_ptr->flags5 & RF5_BOLT_MASK) ||
		     (r_ptr->flags6 & RF6_BOLT_MASK) ||
		     (r_ptr->flagsa & RFA_BOLT_MASK)) &&
		     (p_ptr->stat_use[A_INT] >= (18 + 150)))
		{
			return (TRUE);
		}
		if ((r_ptr->flags4 & RF4_ROCKET) &&
		    (p_ptr->stat_use[A_INT] >= (18 + 200)))
		{
			return (TRUE);
		}
	}

	/* Elemental Brands */
	if ((p_ptr->special_attack & ATTACK_ACID) && !(r_ptr->flagsr & RFR_RES_ACID)) return (TRUE);
	if ((p_ptr->special_attack & ATTACK_FIRE) && !(r_ptr->flagsr & RFR_RES_FIRE)) return (TRUE);
	if ((p_ptr->special_attack & ATTACK_ELEC) && !(r_ptr->flagsr & RFR_RES_ELEC)) return (TRUE);
	if ((p_ptr->special_attack & ATTACK_COLD) && !(r_ptr->flagsr & RFR_RES_COLD)) return (TRUE);
	if ((p_ptr->special_attack & ATTACK_POIS) && !(r_ptr->flagsr & RFR_RES_POIS)) return (TRUE);

	/* Speed */
	if (p_ptr->pspeed < 145)
	{
		if (p_ptr->fast) return (TRUE);
	}

	if (p_ptr->riding && (m_list[p_ptr->riding].mspeed < 135))
	{
		if (MON_FAST(&m_list[p_ptr->riding])) return (TRUE);
	}

	/* Opposite element */
	if (p_ptr->opposite_pelem)
	{
		if (get_cur_pelem() == get_dominant_feature_elem(&cave[py][px])) return (TRUE);
	}

	/* Aura of resurrection */
	if (p_ptr->tim_resurrection) return (TRUE);

	/* Protection of Zoshonel */
	if (p_ptr->zoshonel_protect) return (TRUE);

	/* No need to cast dispel spell */
	return (FALSE);
}


#define ERASE_ELEM_MODE_NONE   0x00
#define ERASE_ELEM_MODE_PLAYER 0x01
#define ERASE_ELEM_MODE_CASTER 0x02

static byte erase_elem_target_mode = ERASE_ELEM_MODE_NONE;

/*
 * Have a monster choose a spell from a list of "useful" spells.
 *
 * Note that this list does NOT include spells that will just hit
 * other monsters, and the list is restricted when the monster is
 * "desperate".  Should that be the job of this function instead?
 *
 * Stupid monsters will just pick a spell randomly.  Smart monsters
 * will choose more "intelligently".
 *
 * Use the helper functions above to put spells into categories.
 *
 * This function may well be an efficiency bottleneck.
 */
static int choose_attack_spell(int m_idx, u16b spells[], u16b num)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	u16b escape[96], escape_num = 0;
	u16b attack[96], attack_num = 0;
	u16b summon[96], summon_num = 0;
	u16b tactic[96], tactic_num = 0;
	u16b annoy[96], annoy_num = 0;
	u16b invul[96], invul_num = 0;
	u16b haste[96], haste_num = 0;
	u16b stop_time[96], stop_time_num = 0;
	u16b special[96], special_num = 0;
	u16b inv_pen[96], inv_pen_num = 0;
	u16b raise[96], raise_num = 0;
	u16b heal[96], heal_num = 0;
	u16b dispel[96], dispel_num = 0;
	u16b erase[96], erase_num = 0;
	u16b ch_elem[96], ch_elem_num = 0;

	int i;

	bool do_erase_elem = FALSE;

	/* Stupid monsters choose randomly */
	if (r_ptr->flags2 & (RF2_STUPID))
	{
		/* Pick at random */
		return (spells[randint0(num)]);
	}

	/* Categorize spells */
	for (i = 0; i < num; i++)
	{
		/* Escape spell? */
		if (spell_escape(spells[i])) escape[escape_num++] = spells[i];

		/* Attack spell? */
		if (spell_attack(spells[i])) attack[attack_num++] = spells[i];

		/* Summon spell? */
		if (spell_summon(spells[i])) summon[summon_num++] = spells[i];

		/* Tactical spell? */
		if (spell_tactic(spells[i])) tactic[tactic_num++] = spells[i];

		/* Annoyance spell? */
		if (spell_annoy(spells[i])) annoy[annoy_num++] = spells[i];

		/* Invulnerability spell? */
		if (spell_invulner(spells[i])) invul[invul_num++] = spells[i];

		/* Haste spell? */
		if (spell_haste(spells[i])) haste[haste_num++] = spells[i];

		/* Time-stop spell? */
		if (spell_stop_time(spells[i])) stop_time[stop_time_num++] = spells[i];

		/* Special spell? */
		if (spell_special(spells[i])) special[special_num++] = spells[i];

		/* Invulnerability-penetrating attack spell? */
		if (spell_inv_pen(spells[i])) inv_pen[inv_pen_num++] = spells[i];

		/* Raise-dead spell? */
		if (spell_raise(spells[i])) raise[raise_num++] = spells[i];

		/* Heal spell? */
		if (spell_heal(spells[i])) heal[heal_num++] = spells[i];

		/* Dispel spell? */
		if (spell_dispel(spells[i])) dispel[dispel_num++] = spells[i];

		/* Erase-elements spell? */
		if (spell_erase(spells[i])) erase[erase_num++] = spells[i];

		/* Change-element spell? */
		if (spell_ch_elem(spells[i])) ch_elem[ch_elem_num++] = spells[i];
	}

	/* Mega-Hack - Precaching the comparation for element-operation */
	if (erase_num)
	{
		cave_type tmp_grid;
		feature_type *f_ptr;
		int cur_elem;

		erase_elem_target_mode = ERASE_ELEM_MODE_NONE;

		if (get_cur_pelem() == get_dominant_feature_elem(&cave[py][px]))
		{
			tmp_grid = cave[py][px];
			f_ptr = &f_info[tmp_grid.mimic ? tmp_grid.mimic : f_info[tmp_grid.feat].mimic];
			for (cur_elem = MIN_ELEM; cur_elem < ELEM_NUM; cur_elem++)
				tmp_grid.elem[cur_elem] = f_ptr->elem[cur_elem] + get_cur_weather_elem(cur_elem);
			if (get_cur_pelem() != get_dominant_feature_elem(&tmp_grid))
			{
				erase_elem_target_mode |= ERASE_ELEM_MODE_PLAYER;
				do_erase_elem = TRUE;
			}
		}
		if (get_opposite_elem(get_cur_melem(m_ptr)) == get_dominant_feature_elem(&cave[m_ptr->fy][m_ptr->fx]))
		{
			tmp_grid = cave[m_ptr->fy][m_ptr->fx];
			f_ptr = &f_info[tmp_grid.mimic ? tmp_grid.mimic : f_info[tmp_grid.feat].mimic];
			for (cur_elem = MIN_ELEM; cur_elem < ELEM_NUM; cur_elem++)
				tmp_grid.elem[cur_elem] = f_ptr->elem[cur_elem] + get_cur_weather_elem(cur_elem);
			if (get_opposite_elem(get_cur_melem(m_ptr)) != get_dominant_feature_elem(&tmp_grid))
			{
				erase_elem_target_mode |= ERASE_ELEM_MODE_CASTER;
				do_erase_elem = TRUE;
			}
		}

		/* エレメント消去の対象はどちらかしか取らない */
		if (do_erase_elem
			&& (erase_elem_target_mode & ERASE_ELEM_MODE_PLAYER)
			&& (erase_elem_target_mode & ERASE_ELEM_MODE_CASTER))
		{
			if (one_in_(2)) erase_elem_target_mode &= ~(ERASE_ELEM_MODE_PLAYER);
			else erase_elem_target_mode &= ~(ERASE_ELEM_MODE_CASTER);
		}
	}

	/*** Try to pick an appropriate spell type ***/

	/* Stop the Time */
	if (stop_time_num && (randint0(100) < 15) && !stop_the_time_monster)
	{
		/* Choose time stop spell */
		return (stop_time[randint0(stop_time_num)]);
	}

	/* special */
	if (special_num)
	{
		bool success = FALSE;
		switch (m_ptr->r_idx)
		{
			case MON_OZ:
			case MON_OZMA:
			case MON_VOLAC:
			case MON_MARTYM:
			case MON_BARBAS:
			case MON_BALZEPHO:
			case MON_ANDORAS:
			case MON_LANCELOT:
			case MON_DENIM:
				if ((randint0(100) < 50) && (m_ptr->cdis <= 2) && clean_shot(m_ptr->fy, m_ptr->fx, py, px, FALSE, TRUE))
				{
					success = TRUE;
				}
				break;

			case MON_SISTEENA:
			case MON_SELYE:
			case MON_SHELLEY:
			case MON_OLIVIA:
				if (!MON_OPPOSITE_ELEM(m_ptr))
				{
					if (cave[m_ptr->fy][m_ptr->fx].elem[get_cur_melem(m_ptr)] < 80)
						success = TRUE;
				}
				break;

			default:
				if (randint0(100) < 50) success = TRUE;
				break;
		}
		if (success) return (special[randint0(special_num)]);
	}

	/* Still hurt badly, couldn't flee, attempt to heal */
	if (m_ptr->hp < m_ptr->maxhp / 3 && one_in_(2))
	{
		/* Choose heal spell if possible */
		if (heal_num) return (heal[randint0(heal_num)]);
	}

	/* Hurt badly or afraid, attempt to flee */
	if (((m_ptr->hp < m_ptr->maxhp / 3) || MON_MONFEAR(m_ptr)) && one_in_(2))
	{
		/* Choose escape spell if possible */
		if (escape_num) return (escape[randint0(escape_num)]);
	}

	/* special */
	if (special_num)
	{
		bool success = FALSE;
		switch (m_ptr->r_idx)
		{
		default:
			break;
		}
		if (success) return (special[randint0(special_num)]);
	}

	/* Player is close and we have attack spells, blink away */
	if ((distance(py, px, m_ptr->fy, m_ptr->fx) < 4) && (attack_num || (r_ptr->flags6 & RF6_TRAPS)) && (randint0(100) < 75) && !stop_the_time_monster)
	{
		/* Choose tactical spell */
		if (tactic_num) return (tactic[randint0(tactic_num)]);
	}

	/* Summon if possible (sometimes) */
	if (summon_num && (randint0(100) < 40))
	{
		/* Choose summon spell */
		return (summon[randint0(summon_num)]);
	}

	/* dispel */
	if (dispel_num && one_in_(2))
	{
		/* Choose dispel spell if possible */
		if (dispel_check(m_idx))
		{
			return (dispel[randint0(dispel_num)]);
		}
	}

	/* Raise-dead if possible (sometimes) */
	if (raise_num && (randint0(100) < 40) && raise_possible(m_ptr))
	{
		/* Choose raise-dead spell */
		return (raise[randint0(raise_num)]);
	}

	/* Attack spell (most of the time) */
	if (p_ptr->invuln)
	{
		if (inv_pen_num && (randint0(100) < 50))
		{
			/* Choose invulnerability-penetrating attack spell */
			return (inv_pen[randint0(inv_pen_num)]);
		}
		else if (attack_num && (randint0(100) < 40))
		{
			/* Choose attack spell */
			return (attack[randint0(attack_num)]);
		}
	}
	else if (attack_num && (randint0(100) < 85))
	{
		/* Choose attack spell */
		return (attack[randint0(attack_num)]);
	}

	/* Try another tactical spell (sometimes) */
	if (tactic_num && (randint0(100) < 50) && !stop_the_time_monster)
	{
		/* Choose tactic spell */
		return (tactic[randint0(tactic_num)]);
	}

	/* Hack - Element erasing spell (sometimes) on player's grid */
	if (do_erase_elem && (randint0(100) < 50))
	{
		/* Choose erase-elements spell */
		return (erase[randint0(erase_num)]);
	}

	/* Hack - Change element spell (sometimes) */
	if (ch_elem_num && (get_cur_pelem() == get_dominant_feature_elem(&cave[py][px]))
		&& !p_ptr->opposite_pelem && (randint0(100) < 50))
	{
		/* Choose erase-elements spell */
		return (ch_elem[randint0(ch_elem_num)]);
	}

	/* Cast globe of invulnerability if not already in effect */
	if (invul_num && !MON_INVULNER(m_ptr) && (randint0(100) < 50))
	{
		/* Choose Globe of Invulnerability */
		return (invul[randint0(invul_num)]);
	}

	/* We're hurt (not badly), try to heal */
	if ((m_ptr->hp < m_ptr->maxhp * 3 / 4) && (randint0(100) < 25))
	{
		/* Choose heal spell if possible */
		if (heal_num) return (heal[randint0(heal_num)]);
	}

	/* Haste self if we aren't already somewhat hasted (rarely) */
	if (haste_num && (randint0(100) < 20) && !MON_FAST(m_ptr))
	{
		/* Choose haste spell */
		return (haste[randint0(haste_num)]);
	}

	/* Annoy player (most of the time) */
	if (annoy_num && (randint0(100) < 80))
	{
		/* Choose annoyance spell */
		return (annoy[randint0(annoy_num)]);
	}

	/* Choose no spell */
	return (0);
}


/*
 * Check path
 */
bool mspell_check_path(monster_type *m_ptr, int *yp, int *xp, int dist,
                       int *do_disi_type, u32b *f4p, u32b *f5p, u32b *f6p, u32b *fap)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	bool have_br_disi = (r_ptr->flags4 & RF4_BR_DISI) ? TRUE : FALSE;
	bool have_ba_disi = (r_ptr->flagsa & RFA_BA_DISI) ? TRUE : FALSE;

	/* Check path */
	if (projectable(m_ptr->fy, m_ptr->fx, *yp, *xp))
	{
		/* Breath disintegration to the enemy in the wall */
		if ((!cave_floor_bold(*yp, *xp)) && one_in_(2))
		{
			if (have_br_disi && have_ba_disi)
			{
				if (one_in_(2)) *do_disi_type = DO_DISI_TYPE_BREATH;
				else *do_disi_type = DO_DISI_TYPE_BALL;
			}
			else if (have_br_disi) *do_disi_type = DO_DISI_TYPE_BREATH;
			else if (have_ba_disi) *do_disi_type = DO_DISI_TYPE_BALL;
		}
	}

	/* Check path to next grid */
	else
	{
		if (have_br_disi)
		{
			if ((dist < MAX_RANGE / 2) && in_disintegration_range(m_ptr->fy, m_ptr->fx, *yp, *xp) &&
			    (one_in_(10) || (projectable(*yp, *xp, m_ptr->fy, m_ptr->fx) && one_in_(2))))
			{
				*do_disi_type = DO_DISI_TYPE_BREATH;
				return TRUE;
			}
		}

		if (have_ba_disi)
		{
			if (dist <= MAX_RANGE)
			{
				int ty = m_ptr->fy;
				int tx = m_ptr->fx;
				u16b path_g[128];
				int path_n, ny, nx, i;

				path_n = project_path(path_g, MAX_RANGE, ty, tx, *yp, *xp, 0L);
				for (i = 0; i < path_n; i++)
				{
					ny = GRID_Y(path_g[i]);
					nx = GRID_X(path_g[i]);

					if (!cave_floor_bold(ny, nx)) break;

					ty = ny;
					tx = nx;
				}

				if ((distance(ty, tx, *yp, *xp) <= 4) && in_disintegration_range(ty, tx, *yp, *xp) &&
				    (one_in_(10) || (projectable(*yp, *xp, m_ptr->fy, m_ptr->fx) && one_in_(2))))
				{
					*do_disi_type = DO_DISI_TYPE_BALL;
					return TRUE;
				}
			}
		}

		{
			int i;
			int tonari;
			static int tonari_y[4][8] = {{-1,-1,-1,0,0,1,1,1},
			                             {-1,-1,-1,0,0,1,1,1},
			                             {1,1,1,0,0,-1,-1,-1},
			                             {1,1,1,0,0,-1,-1,-1}};
			static int tonari_x[4][8] = {{-1,0,1,-1,1,-1,0,1},
			                             {1,0,-1,1,-1,1,0,-1},
			                             {-1,0,1,-1,1,-1,0,1},
			                             {1,0,-1,1,-1,1,0,-1}};

			if (m_ptr->fy < *yp && m_ptr->fx < *xp) tonari = 0;
			else if (m_ptr->fy < *yp) tonari = 1;
			else if (m_ptr->fx < *xp) tonari = 2;
			else tonari = 3;

			for (i = 0; i < 8; i++)
			{
				int next_x = *xp + tonari_x[tonari][i];
				int next_y = *yp + tonari_y[tonari][i];
				cave_type *c_ptr;

				/* Access the next grid */
				c_ptr = &cave[next_y][next_x];

				/* Skip door, rubble, wall */
				if ((c_ptr->feat >= FEAT_DOOR_HEAD) && (c_ptr->feat <= FEAT_PERM_SOLID)) continue;

				/* Skip tree */
				if (c_ptr->feat == FEAT_TREES) continue;

				/* Skip mountain */
				if (c_ptr->feat == FEAT_MOUNTAIN) continue;

				if (projectable(m_ptr->fy, m_ptr->fx, next_y, next_x))
				{
					*yp = next_y;
					*xp = next_x;
					return TRUE;
				}
			}
		}

		if (m_ptr->target_y && m_ptr->target_x)
		{
			*yp = m_ptr->target_y;
			*xp = m_ptr->target_x;
			*f4p &= (RF4_INDIRECT_MASK);
			*f5p &= (RF5_INDIRECT_MASK);
			*f6p &= (RF6_INDIRECT_MASK);
			*fap &= (RFA_INDIRECT_MASK);
			return TRUE;
		}

		/* No spells */
		return FALSE;
	}

	return TRUE;
}


/*
 * Creatures can cast spells, shoot missiles, and breathe.
 *
 * Returns "TRUE" if a spell (or whatever) was (successfully) cast.
 *
 * XXX XXX XXX This function could use some work, but remember to
 * keep it as optimized as possible, while retaining generic code.
 *
 * Verify the various "blind-ness" checks in the code.
 *
 * XXX XXX XXX Note that several effects should really not be "seen"
 * if the player is blind.  See also "effects.c" for other "mistakes".
 *
 * Perhaps monsters should breathe at locations *near* the player,
 * since this would allow them to inflict "partial" damage.
 *
 * Perhaps smart monsters should decline to use "bolt" spells if
 * there is a monster in the way, unless they wish to kill it.
 *
 * Note that, to allow the use of the "track_target" option at some
 * later time, certain non-optimal things are done in the code below,
 * including explicit checks against the "direct" variable, which is
 * currently always true by the time it is checked, but which should
 * really be set according to an explicit "projectable()" test, and
 * the use of generic "x,y" locations instead of the player location,
 * with those values being initialized with the player location.
 *
 * It will not be possible to "correctly" handle the case in which a
 * monster attempts to attack a location which is thought to contain
 * the player, but which in fact is nowhere near the player, since this
 * might induce all sorts of messages about the attack itself, and about
 * the effects of the attack, which the player might or might not be in
 * a position to observe.  Thus, for simplicity, it is probably best to
 * only allow "faulty" attacks by a monster if one of the important grids
 * (probably the initial or final grid) is in fact in view of the player.
 * It may be necessary to actually prevent spell attacks except when the
 * monster actually has line of sight to the player.  Note that a monster
 * could be left in a bizarre situation after the player ducked behind a
 * pillar and then teleported away, for example.
 *
 * Note that certain spell attacks do not use the "project()" function
 * but "simulate" it via the "direct" variable, which is always at least
 * as restrictive as the "project()" function.  This is necessary to
 * prevent "blindness" attacks and such from bending around walls, etc,
 * and to allow the use of the "track_target" option in the future.
 *
 * Note that this function attempts to optimize the use of spells for the
 * cases in which the monster has no spells, or has spells but cannot use
 * them, or has spells but they will have no "useful" effect.  Note that
 * this function has been an efficiency bottleneck in the past.
 *
 * Note the special "MFLAG_NICE" flag, which prevents a monster from using
 * any spell attacks until the player has had a single chance to move.
 */
bool make_attack_spell(int m_idx)
{
	int             k, thrown_spell = 0, rlev, failrate;
	u16b            spell[96], num = 0;
	u32b            f4, f5, f6, fa;
	monster_type    *m_ptr = &m_list[m_idx];
	monster_race    *r_ptr = &r_info[m_ptr->r_idx];
	char            m_name[80];
	char            m_poss[80];
	char            ddesc[80];
	bool            no_inate = FALSE;
	int             do_disi_type = DO_DISI_TYPE_NONE;
	int             dam = 0;
	u32b mode = 0L;
	int s_num_6 = (easy_band ? 2 : 6);
	int s_num_4 = (easy_band ? 1 : 4);

	/* Target location */
	int y = py;
	int x = px;

	/* Summon count */
	int count = 0;

	/* Extract the blind-ness */
	bool blind = (p_ptr->blind ? TRUE : FALSE);

	/* Extract the "see-able-ness" */
	bool seen = (!blind && m_ptr->ml);

	bool mon_anti_magic = (MON_SILENT(m_ptr) || m_ptr->silent_song);

	bool can_use_lite_area = FALSE;

	/* Cannot cast spells when confused */
	if (MON_CONFUSED(m_ptr))
	{
		reset_target(m_ptr);
		return (FALSE);
	}

	/* Cannot cast spells when nice */
	if (m_ptr->mflag & MFLAG_NICE) return (FALSE);
	if (!is_hostile(m_ptr)) return (FALSE);


	/* Sometimes forbid inate attacks (breaths) */
	if (randint0(100) >= (r_ptr->freq_spell * 2)) no_inate = TRUE;

	/* XXX XXX XXX Handle "track_target" option (?) */

	/* Nearby anti-magic monsters */
	if (!mon_anti_magic)
	{
		mon_anti_magic = is_anti_magic_grid(m_idx, m_ptr->fy, m_ptr->fx);
	}

	/* Extract the racial spell flags */
	f4 = r_ptr->flags4;
	f5 = r_ptr->flags5;
	f6 = r_ptr->flags6;
	fa = r_ptr->flagsa;

	/* Check range */
	if ((m_ptr->ddis > MAX_RANGE) && !m_ptr->target_y) return (FALSE);

	if (!mspell_check_path(m_ptr, &y, &x, m_ptr->ddis, &do_disi_type, &f4, &f5, &f6, &fa)) return FALSE;

	reset_target(m_ptr);

	/* Extract the monster level */
	rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);

	/* Forbid inate attacks sometimes */
	if (no_inate)
	{
		f4 &= ~(RF4_NOMAGIC_MASK);
		f5 &= ~(RF5_NOMAGIC_MASK);
		f6 &= ~(RF6_NOMAGIC_MASK);
		fa &= ~(RFA_NOMAGIC_MASK);
	}

	if (!p_ptr->csp)
	{
		f5 &= ~(RF5_DRAIN_MANA);
	}

	if (f6 & RF6_DARKNESS)
	{
		if ((p_ptr->pclass == CLASS_VAMPIRE) &&
		    !(r_ptr->flags3 & (RF3_UNDEAD | RF3_HURT_LITE)))
			can_use_lite_area = TRUE;

		if (!(r_ptr->flags2 & RF2_STUPID))
		{
			if ((p_ptr->pclass == CLASS_VAMPIRE) && !can_use_lite_area) f6 &= ~(RF6_DARKNESS);
		}
	}

	if (mon_anti_magic && !(r_ptr->flags2 & RF2_STUPID))
	{
		f4 &= (RF4_NOMAGIC_MASK);
		f5 &= (RF5_NOMAGIC_MASK);
		f6 &= (RF6_NOMAGIC_MASK);
		fa &= (RFA_NOMAGIC_MASK);
	}

	/* Hack -- allow "desperate" spells */
	if ((r_ptr->flags2 & (RF2_SMART)) &&
		(m_ptr->hp < m_ptr->maxhp / 10) &&
		(randint0(100) < 50))
	{
		/* Require intelligent spells */
		f4 &= (RF4_INT_MASK);
		f5 &= (RF5_INT_MASK);
		f6 &= (RF6_INT_MASK);
		fa &= (RFA_INT_MASK);

		/* No spells left */
		if (!f4 && !f5 && !f6 && !fa) return (FALSE);
	}

	/* Remove the "ineffective" spells */
	remove_bad_spells(m_idx, &f4, &f5, &f6, &fa);

	if (p_ptr->inside_arena)
	{
		f4 &= ~(RF4_SUMMON_MASK);
		f5 &= ~(RF5_SUMMON_MASK);
		f6 &= ~(RF6_SUMMON_MASK);
		fa &= ~(RFA_SUMMON_MASK);
	}

	/* No spells left */
	if (!f4 && !f5 && !f6 && !fa) return (FALSE);

	/* Check for a clean bolt shot */
	if (!(r_ptr->flags2 & RF2_STUPID) &&
	    !clean_shot(m_ptr->fy, m_ptr->fx, py, px, FALSE, FALSE))
	{
		/* Remove spells that will only hurt friends */
		f4 &= ~(RF4_BOLT_MASK);
		f5 &= ~(RF5_BOLT_MASK);
		f6 &= ~(RF6_BOLT_MASK);
		fa &= ~(RFA_BOLT_MASK);
	}

	/* Check for a possible summon */
	if (!(r_ptr->flags2 & RF2_STUPID) &&
	    !(summon_possible(m_idx, y, x)))
	{
		/* Remove summoning spells */
		f4 &= ~(RF4_SUMMON_MASK);
		f5 &= ~(RF5_SUMMON_MASK);
		f6 &= ~(RF6_SUMMON_MASK);
		fa &= ~(RFA_SUMMON_MASK);
	}

	/* No spells left */
	switch (do_disi_type)
	{
	case DO_DISI_TYPE_BREATH:
		if (!(f4 & RF4_BR_DISI)) return FALSE;
		break;
	case DO_DISI_TYPE_BALL:
		if (!(fa & RFA_BA_DISI)) return FALSE;
		break;
	default:
		if (!f4 && !f5 && !f6 && !fa) return FALSE;
		break;
	}

	/* Extract the "inate" spells */
	for (k = 0; k < 32; k++)
	{
		if (f4 & (1L << k)) spell[num++] = k + 32 * 3;
	}

	/* Extract the "normal" spells */
	for (k = 0; k < 32; k++)
	{
		if (f5 & (1L << k)) spell[num++] = k + 32 * 4;
	}

	/* Extract the "bizarre" spells */
	for (k = 0; k < 32; k++)
	{
		if (f6 & (1L << k)) spell[num++] = k + 32 * 5;
	}

	/* Extract the "additional" spells */
	for (k = 0; k < 32; k++)
	{
		if (fa & (1L << k)) spell[num++] = k + 32 * 9;
	}

	/* No spells left */
	if (!num) return (FALSE);

	/* Stop if player is dead or gone */
	if (!p_ptr->playing || p_ptr->is_dead) return (FALSE);

	/* Stop if player is leaving */
	if (p_ptr->leaving) return (FALSE);

	/* Get the monster name (or "it") */
	monster_desc(m_name, m_ptr, 0x00);

	/* Get the monster possessive ("his"/"her"/"its") */
	monster_desc(m_poss, m_ptr, MD_PRON_VISIBLE | MD_POSSESSIVE);

	/* Hack -- Get the "died from" name */
	monster_desc(ddesc, m_ptr, MD_IGNORE_HALLU | MD_ASSUME_VISIBLE | MD_INDEF_VISIBLE);

	switch (do_disi_type)
	{
	case DO_DISI_TYPE_BREATH:
		thrown_spell = 96+31;
		break;
	case DO_DISI_TYPE_BALL:
		thrown_spell = 288+23;
		break;
	default:
		{
			int attempt = 10;
			while (attempt--)
			{
				thrown_spell = choose_attack_spell(m_idx, spell, num);
				if (thrown_spell) break;
			}
		}

		/* Abort if no spell was chosen */
		if (!thrown_spell) return FALSE;
		break;
	}

	/* Calculate spell failure rate */
	failrate = 25 - (rlev + 3) / 4;

	/* Hack -- Stupid monsters will never fail (for jellies and such) */
	if (r_ptr->flags2 & RF2_STUPID) failrate = 0;

	/* Check for spell failure (inate attacks never fail) */
	if (!spell_is_inate(thrown_spell) && ((MON_STUNNED(m_ptr) && one_in_(2)) || (randint0(100) < failrate) || mon_anti_magic))
	{
		disturb(1, 0);
		/* Message */
#ifdef JP
		msg_format("%^sは呪文を唱えようとしたが失敗した。", m_name);
#else
		msg_format("%^s tries to cast a spell, but fails.", m_name);
#endif

		return (TRUE);
	}


	/* Cast the spell. */
	switch (thrown_spell)
	{
		/* RF4_SHRIEK */
		case 96+0:
		{
			disturb(1, 0);
#ifdef JP
			msg_format("%^sがかん高い金切り声をあげた。", m_name);
#else
			msg_format("%^s makes a high pitched shriek.", m_name);
#endif

			aggravate_monsters(m_idx);
			break;
		}

		/* RF4_SHIFT_ELEM */
		case 96+1:
		{
			s16b old_elem = m_ptr->elem;

			/* Message */
			if (!seen)
			{
#ifdef JP
				msg_format("%^sが何かをつぶやいた。", m_name);
#else
				msg_format("%^s mumbles.", m_name);
#endif

			}
			else
			{
#ifdef JP
				msg_format("%sはシフト・エレメントの呪文を唱えた。", m_name);
#else
				msg_format("%^s casts shifting own element.", m_name);
#endif

			}

			do
			{
				m_ptr->elem = randint0(ELEM_NUM);
			}
			while (m_ptr->elem == old_elem);
			break;
		}

		/* RF4_DISPEL */
		case 96+2:
		{
			if ((x != px) || (y != py)) return (FALSE);
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かを力強くつぶやいた。", m_name);
			else msg_format("%^sが魔力消去の呪文を念じた。", m_name);
#else
			if (blind) msg_format("%^s mumbles powerfully.", m_name);
			else msg_format("%^s invokes a dispel magic.", m_name);
#endif
			dispel_player();

			if (p_ptr->riding) dispel_monster_status(p_ptr->riding);

			break;
		}

		/* RF4_ROCKET */
		case 96+3:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かを射った。", m_name);
#else
			if (blind) msg_format("%^s shoots something.", m_name);
#endif

#ifdef JP
			else msg_format("%^sがロケットを発射した。", m_name);
#else
			else msg_format("%^s fires a rocket.", m_name);
#endif
			sound(SOUND_ROCKET);

			dam = ((m_ptr->hp / 4) > 800 ? 800 : (m_ptr->hp / 4));
			breath(y, x, m_idx, GF_ROCKET, dam, 2, FALSE, FALSE);
			update_smart_learn(m_idx, DRS_SHARD);
			update_smart_learn(m_idx, DRS_AVOID2);
			break;
		}

		/* RF4_SHOOT */
		case 96+4:
		{
			if ((x != px) || (y != py)) return (FALSE);
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが奇妙な音を発した。", m_name);
#else
			if (blind) msg_format("%^s makes a strange noise.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが矢を放った。", m_name);
#else
			else msg_format("%^s fires an arrow.", m_name);
#endif
			sound(SOUND_SHOOT);

			dam = damroll(r_ptr->blow[0].d_dice, r_ptr->blow[0].d_side);
			bolt(y, x, m_idx, GF_EDGED, dam);
			update_smart_learn(m_idx, DRS_AVOID0);
			break;
		}

		/* RF4_SHOOT_GUN */
		case 96+5:
		{
			if ((x != px) || (y != py)) return (FALSE);
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが鋭い音を発した。", m_name);
#else
			if (blind) msg_format("%^s makes a sharp noise.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが銃を撃った。", m_name);
#else
			else msg_format("%^s shoots a gun.", m_name);
#endif
			sound(SOUND_SHOOT_GUN);

			dam = damroll(r_ptr->blow[0].d_dice, r_ptr->blow[0].d_side);
			beam(y, x, m_idx, GF_BLUNT, dam);
			break;
		}

		/* RF4_XXX3 */
		case 96+6:
		{
			/* XXX XXX XXX */
			break;
		}

		/* RF4_XXX4 */
		case 96+7:
		{
			/* XXX XXX XXX */
			break;
		}

		/* RF4_BR_ACID */
		case 96+8:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
			if (blind) msg_format("%^s breathes.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが酸のブレスを吐いた。", m_name);
#else
			else msg_format("%^s breathes acid.", m_name);
#endif
			sound(SOUND_BREATH);

			dam = ((m_ptr->hp / 3) > 1600 ? 1600 : (m_ptr->hp / 3));
			breath(y, x, m_idx, GF_ACID, dam, 0, TRUE, FALSE);
			update_smart_learn(m_idx, DRS_ACID);
			break;
		}

		/* RF4_BR_ELEC */
		case 96+9:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
			if (blind) msg_format("%^s breathes.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが稲妻のブレスを吐いた。", m_name);
#else
			else msg_format("%^s breathes lightning.", m_name);
#endif
			sound(SOUND_BREATH);

			dam = ((m_ptr->hp / 3) > 1600 ? 1600 : (m_ptr->hp / 3));
			breath(y, x, m_idx, GF_ELEC, dam,0, TRUE, FALSE);
			update_smart_learn(m_idx, DRS_ELEC);
			break;
		}

		/* RF4_BR_FIRE */
		case 96+10:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
			if (blind) msg_format("%^s breathes.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが火炎のブレスを吐いた。", m_name);
#else
			else msg_format("%^s breathes fire.", m_name);
#endif
			sound(SOUND_BREATH);

			dam = ((m_ptr->hp / 3) > 1600 ? 1600 : (m_ptr->hp / 3));
			breath(y, x, m_idx, GF_FIRE, dam,0, TRUE, FALSE);
			update_smart_learn(m_idx, DRS_FIRE);
			break;
		}

		/* RF4_BR_COLD */
		case 96+11:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
			if (blind) msg_format("%^s breathes.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが冷気のブレスを吐いた。", m_name);
#else
			else msg_format("%^s breathes frost.", m_name);
#endif
			sound(SOUND_BREATH);

			dam = ((m_ptr->hp / 3) > 1600 ? 1600 : (m_ptr->hp / 3));
			breath(y, x, m_idx, GF_COLD, dam,0, TRUE, FALSE);
			update_smart_learn(m_idx, DRS_COLD);
			break;
		}

		/* RF4_BR_POIS */
		case 96+12:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
			if (blind) msg_format("%^s breathes.", m_name);
#endif

#ifdef JP
			else msg_format("%^sがガスのブレスを吐いた。", m_name);
#else
			else msg_format("%^s breathes gas.", m_name);
#endif
			sound(SOUND_BREATH);

			dam = ((m_ptr->hp / 3) > 800 ? 800 : (m_ptr->hp / 3));
			breath(y, x, m_idx, GF_POIS, dam, 0, TRUE, FALSE);
			update_smart_learn(m_idx, DRS_POIS);
			break;
		}


		/* RF4_BR_NETH */
		case 96+13:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
			if (blind) msg_format("%^s breathes.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが地獄のブレスを吐いた。", m_name);
#else
			else msg_format("%^s breathes nether.", m_name);
#endif
			sound(SOUND_BREATH);

			dam = ((m_ptr->hp / 6) > 550 ? 550 : (m_ptr->hp / 6));
			breath(y, x, m_idx, GF_NETHER, dam,0, TRUE, FALSE);
			update_smart_learn(m_idx, DRS_NETH);
			break;
		}

		/* RF4_BR_LITE */
		case 96+14:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
			if (blind) msg_format("%^s breathes.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが閃光のブレスを吐いた。", m_name);
#else
			else msg_format("%^s breathes light.", m_name);
#endif
			sound(SOUND_BREATH);

			dam = ((m_ptr->hp / 6) > 400 ? 400 : (m_ptr->hp / 6));
			breath(y, x, m_idx, GF_LITE, dam,0, TRUE, FALSE);
			update_smart_learn(m_idx, DRS_LITE);
			break;
		}

		/* RF4_BR_DARK */
		case 96+15:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
			if (blind) msg_format("%^s breathes.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが暗黒のブレスを吐いた。", m_name);
#else
			else msg_format("%^s breathes darkness.", m_name);
#endif
			sound(SOUND_BREATH);

			dam = ((m_ptr->hp / 6) > 400 ? 400 : (m_ptr->hp / 6));
			breath(y, x, m_idx, GF_DARK, dam,0, TRUE, FALSE);
			update_smart_learn(m_idx, DRS_DARK);
			break;
		}

		/* RF4_BR_CONF */
		case 96+16:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
			if (blind) msg_format("%^s breathes.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが混乱のブレスを吐いた。", m_name);
#else
			else msg_format("%^s breathes confusion.", m_name);
#endif
			sound(SOUND_BREATH);

			dam = ((m_ptr->hp / 6) > 450 ? 450 : (m_ptr->hp / 6));
			breath(y, x, m_idx, GF_CONFUSION, dam,0, TRUE, FALSE);
			update_smart_learn(m_idx, DRS_CONF);
			break;
		}

		/* RF4_BR_SOUN */
		case 96+17:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
			if (blind) msg_format("%^s breathes.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが轟音のブレスを吐いた。", m_name);
#else
			else msg_format("%^s breathes sound.", m_name);
#endif
			sound(SOUND_BREATH);

			dam = ((m_ptr->hp / 6) > 450 ? 450 : (m_ptr->hp / 6));
			breath(y, x, m_idx, GF_SOUND, dam,0, TRUE, FALSE);
			update_smart_learn(m_idx, DRS_SOUND);
			break;
		}

		/* RF4_BR_CHAO */
		case 96+18:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
			if (blind) msg_format("%^s breathes.", m_name);
#endif

#ifdef JP
			else msg_format("%^sがカオスのブレスを吐いた。", m_name);
#else
			else msg_format("%^s breathes chaos.", m_name);
#endif
			sound(SOUND_BREATH);

			dam = ((m_ptr->hp / 6) > 600 ? 600 : (m_ptr->hp / 6));
			breath(y, x, m_idx, GF_CHAOS, dam,0, TRUE, FALSE);
			update_smart_learn(m_idx, DRS_CHAOS);
			break;
		}

		/* RF4_BR_DISE */
		case 96+19:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
			if (blind) msg_format("%^s breathes.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが劣化のブレスを吐いた。", m_name);
#else
			else msg_format("%^s breathes disenchantment.", m_name);
#endif
			sound(SOUND_BREATH);

			dam = ((m_ptr->hp / 6) > 500 ? 500 : (m_ptr->hp / 6));
			breath(y, x, m_idx, GF_DISENCHANT, dam,0, TRUE, FALSE);
			update_smart_learn(m_idx, DRS_DISEN);
			break;
		}

		/* RF4_BR_STON */
		case 96+20:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
			if (blind) msg_format("%^s breathes.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが石化のブレスを吐いた。", m_name);
#else
			else msg_format("%^s breathes stone.", m_name);
#endif
			sound(SOUND_BREATH);

			dam = ((m_ptr->hp / 3) > 250 ? 250 : (m_ptr->hp / 3));
			breath(y, x, m_idx, GF_STONE, dam,0, TRUE, FALSE);
			update_smart_learn(m_idx, DRS_STONE);
			break;
		}

		/* RF4_BR_TIME */
		case 96+21:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
			if (blind) msg_format("%^s breathes.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが時間逆転のブレスを吐いた。", m_name);
#else
			else msg_format("%^s breathes time.", m_name);
#endif
			sound(SOUND_BREATH);

			dam = ((m_ptr->hp / 3) > 150 ? 150 : (m_ptr->hp / 3));
			breath(y, x, m_idx, GF_TIME, dam,0, TRUE, FALSE);
			update_smart_learn(m_idx, DRS_TIME);
			break;
		}

		/* RF4_BR_INER */
		case 96+22:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
			if (blind) msg_format("%^s breathes.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが遅鈍のブレスを吐いた。", m_name);
#else
			else msg_format("%^s breathes inertia.", m_name);
#endif
			sound(SOUND_BREATH);

			dam = ((m_ptr->hp / 6) > 200 ? 200 : (m_ptr->hp / 6));
			breath(y, x, m_idx, GF_INERTIA, dam,0, TRUE, FALSE);
			break;
		}

		/* RF4_BR_GRAV */
		case 96+23:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
			if (blind) msg_format("%^s breathes.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが重力のブレスを吐いた。", m_name);
#else
			else msg_format("%^s breathes gravity.", m_name);
#endif
			sound(SOUND_BREATH);

			dam = ((m_ptr->hp / 3) > 200 ? 200 : (m_ptr->hp / 3));
			breath(y, x, m_idx, GF_GRAVITY, dam,0, TRUE, FALSE);
			break;
		}

		/* RF4_BR_SHAR */
		case 96+24:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
			if (blind) msg_format("%^s breathes.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが破片のブレスを吐いた。", m_name);
#else
			else msg_format("%^s breathes shards.", m_name);
#endif
			sound(SOUND_BREATH);

			dam = ((m_ptr->hp / 6) > 500 ? 500 : (m_ptr->hp / 6));
			breath(y, x, m_idx, GF_SHARDS, dam,0, TRUE, FALSE);
			update_smart_learn(m_idx, DRS_SHARD);
			break;
		}

		/* RF4_BR_PLAS */
		case 96+25:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
			if (blind) msg_format("%^s breathes.", m_name);
#endif

#ifdef JP
			else msg_format("%^sがプラズマのブレスを吐いた。", m_name);
#else
			else msg_format("%^s breathes plasma.", m_name);
#endif
			sound(SOUND_BREATH);

			dam = ((m_ptr->hp / 6) > 150 ? 150 : (m_ptr->hp / 6));
			breath(y, x, m_idx, GF_PLASMA, dam,0, TRUE, FALSE);
			update_smart_learn(m_idx, DRS_PLASMA);
			break;
		}

		/* RF4_BR_WALL */
		case 96+26:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
			if (blind) msg_format("%^s breathes.", m_name);
#endif

#ifdef JP
			else msg_format("%^sがフォースのブレスを吐いた。", m_name);
#else
			else msg_format("%^s breathes force.", m_name);
#endif
			sound(SOUND_BREATH);

			dam = ((m_ptr->hp / 6) > 200 ? 200 : (m_ptr->hp / 6));
			breath(y, x, m_idx, GF_FORCE, dam,0, TRUE, FALSE);
			break;
		}

		/* RF4_BR_MANA */
		case 96+27:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
			if (blind) msg_format("%^s breathes.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが魔力のブレスを吐いた。", m_name);
#else
			else msg_format("%^s breathes mana.", m_name);
#endif
			sound(SOUND_BREATH);

			dam = ((m_ptr->hp / 3) > 250 ? 250 : (m_ptr->hp / 3));
			breath(y, x, m_idx, GF_MANA, dam,0, TRUE, FALSE);
			break;
		}

		/* RF4_BA_NUKE */
		case 96+28:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かをつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが放射能球を放った。", m_name);
#else
			else msg_format("%^s casts a ball of radiation.", m_name);
#endif

			dam = (rlev + damroll(10, 6)) * ((r_ptr->flags2 & RF2_POWERFUL) ? 2 : 1);
			breath(y, x, m_idx, GF_NUKE, dam, 2, FALSE, FALSE);
			update_smart_learn(m_idx, DRS_POIS);
			break;
		}

		/* RF4_BR_NUKE */
		case 96+29:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
			if (blind) msg_format("%^s breathes.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが放射性廃棄物のブレスを吐いた。", m_name);
#else
			else msg_format("%^s breathes toxic waste.", m_name);
#endif
			sound(SOUND_BREATH);

			dam = ((m_ptr->hp / 3) > 800 ? 800 : (m_ptr->hp / 3));
			breath(y, x, m_idx, GF_NUKE, dam,0, TRUE, FALSE);
			update_smart_learn(m_idx, DRS_POIS);
			break;
		}

		/* RF4_BA_CHAO */
		case 96+30:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが恐ろしげにつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles frighteningly.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが純粋なカオスを放った。", m_name);/*nuke me*/
#else
			else msg_format("%^s invokes a raw chaos.", m_name);
#endif

			dam = ((r_ptr->flags2 & RF2_POWERFUL) ? (rlev * 3) : (rlev * 2))+ damroll(10, 10);
			breath(y, x, m_idx, GF_CHAOS, dam, 4, FALSE, FALSE);
			update_smart_learn(m_idx, DRS_CHAOS);
			break;
		}

		/* RF4_BR_DISI */
		case 96+31:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
			if (blind) msg_format("%^s breathes.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが分解のブレスを吐いた。", m_name);
#else
			else msg_format("%^s breathes disintegration.", m_name);
#endif
			sound(SOUND_BREATH);

			dam = ((m_ptr->hp / 6) > 150 ? 150 : (m_ptr->hp / 6));
			breath(y, x, m_idx, GF_DISINTEGRATE, dam,0, TRUE, FALSE);
			break;
		}



		/* RF5_BA_ACID */
		case 128+0:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かをつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles.", m_name);
#endif

#ifdef JP
			else msg_format("%^sがアシッドクラウドの呪文を唱えた。", m_name);
#else
			else msg_format("%^s casts an acid cloud.", m_name);
#endif

			dam = (randint1(rlev * 3) + 15) * ((r_ptr->flags2 & RF2_POWERFUL) ? 2 : 1);
			breath(y, x, m_idx, GF_ACID, dam, 2, FALSE, TRUE);
			update_smart_learn(m_idx, DRS_ACID);
			break;
		}

		/* RF5_BA_ELEC */
		case 128+1:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かをつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles.", m_name);
#endif

#ifdef JP
			else msg_format("%^sがサンダーフレアの呪文を唱えた。", m_name);
#else
			else msg_format("%^s casts a thunder flare.", m_name);
#endif

			dam = (randint1(rlev * 3 / 2) + 8) * ((r_ptr->flags2 & RF2_POWERFUL) ? 2 : 1);
			breath(y, x, m_idx, GF_ELEC, dam, 2, FALSE, TRUE);
			update_smart_learn(m_idx, DRS_ELEC);
			break;
		}

		/* RF5_BA_FIRE */
		case 128+2:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かをつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles.", m_name);
#endif

#ifdef JP
			else msg_format("%^sがファイアストームの呪文を唱えた。", m_name);
#else
			else msg_format("%^s casts a fire storm.", m_name);
#endif

			dam = (randint1(rlev * 7 / 2) + 10) * ((r_ptr->flags2 & RF2_POWERFUL) ? 2 : 1);
			breath(y, x, m_idx, GF_FIRE, dam, 2, FALSE, TRUE);
			update_smart_learn(m_idx, DRS_FIRE);
			break;
		}

		/* RF5_BA_COLD */
		case 128+3:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かをつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles.", m_name);
#endif

#ifdef JP
			else msg_format("%^sがアイスブラストの呪文を唱えた。", m_name);
#else
			else msg_format("%^s casts an ice blast.", m_name);
#endif

			dam = (randint1(rlev * 3 / 2) + 10) * ((r_ptr->flags2 & RF2_POWERFUL) ? 2 : 1);
			breath(y, x, m_idx, GF_COLD, dam, 2, FALSE, TRUE);
			update_smart_learn(m_idx, DRS_COLD);
			break;
		}

		/* RF5_BA_POIS */
		case 128+4:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かをつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが悪臭雲の呪文を唱えた。", m_name);
#else
			else msg_format("%^s casts a stinking cloud.", m_name);
#endif

			dam = damroll(12, 2) * ((r_ptr->flags2 & RF2_POWERFUL) ? 2 : 1);
			breath(y, x, m_idx, GF_POIS, dam, 2, FALSE, FALSE);
			update_smart_learn(m_idx, DRS_POIS);
			break;
		}

		/* RF5_BA_NETH */
		case 128+5:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かをつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが地獄球の呪文を唱えた。", m_name);
#else
			else msg_format("%^s casts a nether ball.", m_name);
#endif

			dam = 50 + damroll(10, 10) + (rlev * ((r_ptr->flags2 & RF2_POWERFUL) ? 2 : 1));
			breath(y, x, m_idx, GF_NETHER, dam, 2, FALSE, FALSE);
			update_smart_learn(m_idx, DRS_NETH);
			break;
		}

		/* RF5_BA_WATE */
		case 128+6:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かをつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが流れるような身振りをした。", m_name);
#else
			else msg_format("%^s gestures fluidly.", m_name);
#endif

#ifdef JP
			msg_print("あなたは渦巻きに飲み込まれた。");
#else
			msg_print("You are engulfed in a whirlpool.");
#endif

			dam = ((r_ptr->flags2 & RF2_POWERFUL) ? randint1(rlev * 3) : randint1(rlev * 2)) + 50;
			breath(y, x, m_idx, GF_WATER, dam, 4, FALSE, FALSE);
			update_smart_learn(m_idx, DRS_WATER);
			break;
		}

		/* RF5_BA_MANA */
		case 128+7:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かを力強くつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles powerfully.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが魔力の嵐の呪文を念じた。", m_name);
#else
			else msg_format("%^s invokes a mana storm.", m_name);
#endif

			dam = (rlev * 4) + 50 + damroll(10, 10);
			breath(y, x, m_idx, GF_MANA, dam, 4, FALSE, FALSE);
			break;
		}

		/* RF5_BA_DARK */
		case 128+8:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かを力強くつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles powerfully.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが暗黒の嵐の呪文を念じた。", m_name);
#else
			else msg_format("%^s invokes a darkness storm.", m_name);
#endif

			dam = (rlev * 4) + 50 + damroll(10, 10);
			breath(y, x, m_idx, GF_DARK, dam, 4, FALSE, FALSE);
			update_smart_learn(m_idx, DRS_DARK);
			break;
		}

		/* RF5_DRAIN_MANA */
		case 128+9:
		{
			if ((x != px) || (y != py)) return (FALSE);
			disturb(1, 0);
			if (p_ptr->csp)
			{
				int r1;

				/* Basic message */
#ifdef JP
				msg_format("%^sに精神エネルギーを吸い取られてしまった！", m_name);
#else
				msg_format("%^s draws psychic energy from you!", m_name);
#endif


				/* Attack power */
				r1 = (randint1(rlev) / 2) + 1;

				/* Full drain */
				if (r1 >= p_ptr->csp)
				{
					r1 = p_ptr->csp;
					p_ptr->csp = 0;
					p_ptr->csp_frac = 0;
				}

				/* Partial drain */
				else
				{
					p_ptr->csp -= r1;
				}

				/* Redraw mana */
				p_ptr->redraw |= (PR_MANA);

				/* Window stuff */
				p_ptr->window |= (PW_PLAYER);
				p_ptr->window |= (PW_SPELL);

				/* Heal the monster */
				if (m_ptr->hp < m_ptr->maxhp)
				{
					/* Heal */
					m_ptr->hp += (6 * r1);
					if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

					/* Redraw (later) if needed */
					if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);
					if (p_ptr->riding == m_idx) p_ptr->redraw |= (PR_UHEALTH);

					/* Special message */
					if (seen)
					{
#ifdef JP
						msg_format("%^sは気分が良さそうだ。", m_name);
#else
						msg_format("%^s appears healthier.", m_name);
#endif

					}
				}
			}
			update_smart_learn(m_idx, DRS_MANA);
			break;
		}

		/* RF5_MIND_BLAST */
		case 128+10:
		{
			if ((x != px) || (y != py)) return (FALSE);
			disturb(1, 0);
			if (!seen)
			{
#ifdef JP
				msg_print("何かがあなたの精神に念を放っているようだ。");
#else
				msg_print("You feel something focusing on your mind.");
#endif

			}
			else
			{
#ifdef JP
				msg_format("%^sがあなたの瞳をじっとにらんでいる。", m_name);
#else
				msg_format("%^s gazes deep into your eyes.", m_name);
#endif

			}

			dam = damroll(7, 7);
			breath(y, x, m_idx, GF_MIND_BLAST, dam, 0, FALSE, FALSE);
			break;
		}

		/* RF5_BRAIN_SMASH */
		case 128+11:
		{
			if ((x != px) || (y != py)) return (FALSE);
			disturb(1, 0);
			if (!seen)
			{
#ifdef JP
				msg_print("何かがあなたの精神に念を放っているようだ。");
#else
				msg_print("You feel something focusing on your mind.");
#endif

			}
			else
			{
#ifdef JP
				msg_format("%^sがあなたの瞳をじっと見ている。", m_name);
#else
				msg_format("%^s looks deep into your eyes.", m_name);
#endif

			}

			dam = damroll(12, 12);
			breath(y, x, m_idx, GF_BRAIN_SMASH, dam, 0, FALSE, FALSE);
			break;
		}

		/* RF5_CAUSE_1 */
		case 128+12:
		{
			if ((x != px) || (y != py)) return (FALSE);
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かをつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles.", m_name);
#endif

#ifdef JP
			else msg_format("%^sがあなたを指さして呪った。", m_name);
#else
			else msg_format("%^s points at you and curses.", m_name);
#endif

			dam = damroll(3, 8);
			breath(y, x, m_idx, GF_CAUSE_1, dam, 0, FALSE, FALSE);
			break;
		}

		/* RF5_CAUSE_2 */
		case 128+13:
		{
			if ((x != px) || (y != py)) return (FALSE);
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かをつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles.", m_name);
#endif

#ifdef JP
			else msg_format("%^sがあなたを指さして恐ろしげに呪った。", m_name);
#else
			else msg_format("%^s points at you and curses horribly.", m_name);
#endif

			dam = damroll(8, 8);
			breath(y, x, m_idx, GF_CAUSE_2, dam, 0, FALSE, FALSE);
			break;
		}

		/* RF5_CAUSE_3 */
		case 128+14:
		{
			if ((x != px) || (y != py)) return (FALSE);
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かを大声で叫んだ。", m_name);
#else
			if (blind) msg_format("%^s mumbles loudly.", m_name);
#endif

#ifdef JP
			else msg_format("%^sがあなたを指さして恐ろしげに呪文を唱えた！", m_name);
#else
			else msg_format("%^s points at you, incanting terribly!", m_name);
#endif

			dam = damroll(10, 15);
			breath(y, x, m_idx, GF_CAUSE_3, dam, 0, FALSE, FALSE);
			break;
		}

		/* RF5_CAUSE_4 */
		case 128+15:
		{
			if ((x != px) || (y != py)) return (FALSE);
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが「死ね！」と叫んだ。", m_name);
#else
			if (blind) msg_format("%^s screams the word 'DIE!'", m_name);
#endif

#ifdef JP
			else msg_format("%^sがあなたを指さして「死ね！」と叫んだ。", m_name);
#else
			else msg_format("%^s points at you, screaming the word DIE!", m_name);
#endif

			dam = damroll(15, 15);
			breath(y, x, m_idx, GF_CAUSE_4, dam, 0, FALSE, FALSE);
			break;
		}

		/* RF5_BO_ACID */
		case 128+16:
		{
			if ((x != px) || (y != py)) return (FALSE);
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かをつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles.", m_name);
#endif

#ifdef JP
			else msg_format("%^sがアシッド・ボルトの呪文を唱えた。", m_name);
#else
			else msg_format("%^s casts a acid bolt.", m_name);
#endif

			dam = (damroll(7, 8) + (rlev / 3)) * ((r_ptr->flags2 & RF2_POWERFUL) ? 2 : 1);
			bolt(y, x, m_idx, GF_ACID, dam);
			update_smart_learn(m_idx, DRS_ACID);
			update_smart_learn(m_idx, DRS_REFLECT);
			update_smart_learn(m_idx, DRS_AVOID1);
			break;
		}

		/* RF5_BO_ELEC */
		case 128+17:
		{
			if ((x != px) || (y != py)) return (FALSE);
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かをつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles.", m_name);
#endif

#ifdef JP
			else msg_format("%^sがサンダー・ボルトの呪文を唱えた。", m_name);
#else
			else msg_format("%^s casts a lightning bolt.", m_name);
#endif

			dam = (damroll(4, 8) + (rlev / 3)) * ((r_ptr->flags2 & RF2_POWERFUL) ? 2 : 1);
			bolt(y, x, m_idx, GF_ELEC, dam);
			update_smart_learn(m_idx, DRS_ELEC);
			update_smart_learn(m_idx, DRS_REFLECT);
			update_smart_learn(m_idx, DRS_AVOID1);
			break;
		}

		/* RF5_BO_FIRE */
		case 128+18:
		{
			if ((x != px) || (y != py)) return (FALSE);
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かをつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles.", m_name);
#endif

#ifdef JP
			else msg_format("%^sがファイア・ボルトの呪文を唱えた。", m_name);
#else
			else msg_format("%^s casts a fire bolt.", m_name);
#endif

			dam = (damroll(9, 8) + (rlev / 3)) * ((r_ptr->flags2 & RF2_POWERFUL) ? 2 : 1);
			bolt(y, x, m_idx, GF_FIRE, dam);
			update_smart_learn(m_idx, DRS_FIRE);
			update_smart_learn(m_idx, DRS_REFLECT);
			update_smart_learn(m_idx, DRS_AVOID1);
			break;
		}

		/* RF5_BO_COLD */
		case 128+19:
		{
			if ((x != px) || (y != py)) return (FALSE);
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かをつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles.", m_name);
#endif

#ifdef JP
			else msg_format("%^sがアイス・ボルトの呪文を唱えた。", m_name);
#else
			else msg_format("%^s casts a frost bolt.", m_name);
#endif

			dam = (damroll(6, 8) + (rlev / 3)) * ((r_ptr->flags2 & RF2_POWERFUL) ? 2 : 1);
			bolt(y, x, m_idx, GF_COLD, dam);
			update_smart_learn(m_idx, DRS_COLD);
			update_smart_learn(m_idx, DRS_REFLECT);
			update_smart_learn(m_idx, DRS_AVOID1);
			break;
		}

		/* RF5_BA_LITE */
		case 128+20:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かを力強くつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles powerfully.", m_name);
#endif

#ifdef JP
			else msg_format("%^sがスターバーストの呪文を念じた。", m_name);
#else
			else msg_format("%^s invokes a starburst.", m_name);
#endif

			dam = (rlev * 4) + 50 + damroll(10, 10);
			breath(y, x, m_idx, GF_LITE, dam, 4, FALSE, FALSE);
			update_smart_learn(m_idx, DRS_LITE);
			break;
		}

		/* RF5_BO_NETH */
		case 128+21:
		{
			if ((x != px) || (y != py)) return (FALSE);
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かをつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが地獄の矢の呪文を唱えた。", m_name);
#else
			else msg_format("%^s casts a nether bolt.", m_name);
#endif

			dam = 30 + damroll(5, 5) + (rlev * 4) / ((r_ptr->flags2 & RF2_POWERFUL) ? 2 : 3);
			bolt(y, x, m_idx, GF_NETHER, dam);
			update_smart_learn(m_idx, DRS_NETH);
			update_smart_learn(m_idx, DRS_REFLECT);
			update_smart_learn(m_idx, DRS_AVOID1);
			break;
		}

		/* RF5_BO_WATE */
		case 128+22:
		{
			if ((x != px) || (y != py)) return (FALSE);
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かをつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles.", m_name);
#endif

#ifdef JP
			else msg_format("%^sがウォーター・ボルトの呪文を唱えた。", m_name);
#else
			else msg_format("%^s casts a water bolt.", m_name);
#endif

			dam = damroll(10, 10) + (rlev * 3 / ((r_ptr->flags2 & RF2_POWERFUL) ? 2 : 3));
			bolt(y, x, m_idx, GF_WATER, dam);
			update_smart_learn(m_idx, DRS_WATER);
			update_smart_learn(m_idx, DRS_REFLECT);
			update_smart_learn(m_idx, DRS_AVOID1);
			break;
		}

		/* RF5_BO_MANA */
		case 128+23:
		{
			if ((x != px) || (y != py)) return (FALSE);
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かをつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが魔力の矢の呪文を唱えた。", m_name);
#else
			else msg_format("%^s casts a mana bolt.", m_name);
#endif

			dam = randint1(rlev * 7 / 2) + 50;
			bolt(y, x, m_idx, GF_MANA, dam);
			update_smart_learn(m_idx, DRS_REFLECT);
			update_smart_learn(m_idx, DRS_AVOID1);
			break;
		}

		/* RF5_BO_PLAS */
		case 128+24:
		{
			if ((x != px) || (y != py)) return (FALSE);
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かをつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles.", m_name);
#endif

#ifdef JP
			else msg_format("%^sがプラズマ・ボルトの呪文を唱えた。", m_name);
#else
			else msg_format("%^s casts a plasma bolt.", m_name);
#endif

			dam = 10 + damroll(8, 7) + (rlev * 3 / ((r_ptr->flags2 & RF2_POWERFUL) ? 2 : 3));
			bolt(y, x, m_idx, GF_PLASMA, dam);
			update_smart_learn(m_idx, DRS_PLASMA);
			update_smart_learn(m_idx, DRS_REFLECT);
			update_smart_learn(m_idx, DRS_AVOID1);
			break;
		}

		/* RF5_BO_ICEE */
		case 128+25:
		{
			if ((x != px) || (y != py)) return (FALSE);
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かをつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが極寒の矢の呪文を唱えた。", m_name);
#else
			else msg_format("%^s casts an ice bolt.", m_name);
#endif

			dam = damroll(6, 6) + (rlev * 3 / ((r_ptr->flags2 & RF2_POWERFUL) ? 2 : 3));
			bolt(y, x, m_idx, GF_ICE, dam);
			update_smart_learn(m_idx, DRS_COLD);
			update_smart_learn(m_idx, DRS_REFLECT);
			update_smart_learn(m_idx, DRS_AVOID1);
			break;
		}

		/* RF5_MISSILE */
		case 128+26:
		{
			if ((x != px) || (y != py)) return (FALSE);
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かをつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles.", m_name);
#endif

#ifdef JP
			else msg_format("%^sがマジック・ミサイルの呪文を唱えた。", m_name);
#else
			else msg_format("%^s casts a magic missile.", m_name);
#endif

			dam = damroll(2, 6) + (rlev / 3);
			bolt(y, x, m_idx, GF_MISSILE, dam);
			update_smart_learn(m_idx, DRS_REFLECT);
			update_smart_learn(m_idx, DRS_AVOID1);
			break;
		}

		/* RF5_SCARE */
		case 128+27:
		{
			if ((x != px) || (y != py)) return (FALSE);
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かをつぶやくと、恐ろしげな音が聞こえた。", m_name);
#else
			if (blind) msg_format("%^s mumbles, and you hear scary noises.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが恐ろしげな幻覚を作り出した。", m_name);
#else
			else msg_format("%^s casts a fearful illusion.", m_name);
#endif

			if (p_ptr->resist_fear)
			{
#ifdef JP
				msg_print("しかし恐怖に侵されなかった。");
#else
				msg_print("You refuse to be frightened.");
#endif

			}
			else if (randint0(100 + rlev/2) < p_ptr->skill_sav)
			{
#ifdef JP
				msg_print("しかし恐怖に侵されなかった。");
#else
				msg_print("You refuse to be frightened.");
#endif

			}
			else
			{
				(void)set_afraid(p_ptr->afraid + randint0(4) + 4);
			}
			update_smart_learn(m_idx, DRS_FEAR);
			break;
		}

		/* RF5_BLIND */
		case 128+28:
		{
			if ((x != px) || (y != py)) return (FALSE);
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かをつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが呪文を唱えてあなたの目をくらました！", m_name);
#else
			else msg_format("%^s casts a spell, burning your eyes!", m_name);
#endif

			if (p_ptr->resist_blind)
			{
#ifdef JP
				msg_print("しかし効果がなかった！");
#else
				msg_print("You are unaffected!");
#endif

			}
			else if (randint0(100 + rlev/2) < p_ptr->skill_sav)
			{
#ifdef JP
				msg_print("しかし効力を跳ね返した！");
#else
				msg_print("You resist the effects!");
#endif

			}
			else
			{
				(void)set_blind(12 + randint0(4));
			}
			update_smart_learn(m_idx, DRS_BLIND);
			break;
		}

		/* RF5_CONF */
		case 128+29:
		{
			if ((x != px) || (y != py)) return (FALSE);
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かをつぶやくと、頭を悩ます音がした。", m_name);
#else
			if (blind) msg_format("%^s mumbles, and you hear puzzling noises.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが誘惑的な幻覚を作り出した。", m_name);
#else
			else msg_format("%^s creates a mesmerising illusion.", m_name);
#endif

			if (p_ptr->resist_conf)
			{
#ifdef JP
				msg_print("しかし幻覚にはだまされなかった。");
#else
				msg_print("You disbelieve the feeble spell.");
#endif

			}
			else if (randint0(100 + rlev/2) < p_ptr->skill_sav)
			{
#ifdef JP
				msg_print("しかし幻覚にはだまされなかった。");
#else
				msg_print("You disbelieve the feeble spell.");
#endif

			}
			else
			{
				(void)set_confused(p_ptr->confused + randint0(4) + 4);
			}
			update_smart_learn(m_idx, DRS_CONF);
			break;
		}

		/* RF5_SLOW */
		case 128+30:
		{
			if ((x != px) || (y != py)) return (FALSE);
			disturb(1, 0);
#ifdef JP
			msg_format("%^sがあなたの筋力を吸い取ろうとした！", m_name);
#else
			msg_format("%^s drains power from your muscles!", m_name);
#endif

			if (p_ptr->free_act)
			{
#ifdef JP
				msg_print("しかし効果がなかった！");
#else
				msg_print("You are unaffected!");
#endif

			}
			else if (randint0(100 + rlev/2) < p_ptr->skill_sav)
			{
#ifdef JP
				msg_print("しかし効力を跳ね返した！");
#else
				msg_print("You resist the effects!");
#endif

			}
			else
			{
				(void)set_slow(p_ptr->slow + randint0(4) + 4, FALSE);
			}
			update_smart_learn(m_idx, DRS_FREE);
			break;
		}

		/* RF5_HOLD */
		case 128+31:
		{
			if ((x != px) || (y != py)) return (FALSE);
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かをつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles.", m_name);
#endif

#ifdef JP
			else msg_format("%^sがあなたの目をじっと見つめた！", m_name);
#else
			else msg_format("%^s stares deep into your eyes!", m_name);
#endif

			if (p_ptr->free_act)
			{
#ifdef JP
				msg_print("しかし効果がなかった！");
#else
				msg_print("You are unaffected!");
#endif

			}
			else if (randint0(100 + rlev/2) < p_ptr->skill_sav)
			{
#ifdef JP
				msg_format("しかし効力を跳ね返した！");
#else
				msg_format("You resist the effects!");
#endif

			}
			else
			{
				(void)set_paralyzed(p_ptr->paralyzed + randint0(4) + 4);
			}
			update_smart_learn(m_idx, DRS_FREE);
			break;
		}

		/* RF6_HASTE */
		case 160+0:
		{
			disturb(1, 0);
			if (blind)
			{
#ifdef JP
				msg_format("%^sが何かをつぶやいた。", m_name);
#else
				msg_format("%^s mumbles.", m_name);
#endif

			}
			else
			{
#ifdef JP
				msg_format("%^sが自分の体に念を送った。", m_name, m_poss);
#else
				msg_format("%^s concentrates on %s body.", m_name, m_poss);
#endif

			}

			/* Allow quick speed increases to base+10 */
			if (set_monster_fast(m_idx, MON_FAST(m_ptr) + 100))
			{
#ifdef JP
				msg_format("%^sの動きが速くなった。", m_name);
#else
				msg_format("%^s starts moving faster.", m_name);
#endif
			}
			break;
		}

		/* RF6_HAND_DOOM */
		case 160+1:
		{
			if ((x != px) || (y != py)) return (FALSE);
			disturb(1, 0);
#ifdef JP
			msg_format("%^sが<破滅の手>を放った！", m_name);
#else
			msg_format("%^s invokes the Hand of Doom!", m_name);
#endif
			dam = randint0(100 + rlev / 2);
			breath(y, x, m_idx, GF_HAND_DOOM, dam, 0, FALSE, FALSE);
			break;
		}

		/* RF6_HEAL */
		case 160+2:
		{
			disturb(1, 0);

			/* Message */
			if (blind)
			{
#ifdef JP
				msg_format("%^sが何かをつぶやいた。", m_name);
#else
				msg_format("%^s mumbles.", m_name);
#endif

			}
			else
			{
#ifdef JP
				msg_format("%^sが自分の傷に集中した。", m_name);
#else
				msg_format("%^s concentrates on %s wounds.", m_name, m_poss);
#endif

			}

			/* Heal some */
			m_ptr->hp += (rlev * 6);

			/* Fully healed */
			if (m_ptr->hp >= m_ptr->maxhp)
			{
				/* Fully healed */
				m_ptr->hp = m_ptr->maxhp;

				/* Message */
				if (seen)
				{
#ifdef JP
					msg_format("%^sは完全に治った！", m_name);
#else
					msg_format("%^s looks completely healed!", m_name);
#endif

				}
				else
				{
#ifdef JP
					msg_format("%^sは完全に治ったようだ！", m_name);
#else
					msg_format("%^s sounds completely healed!", m_name);
#endif

				}
			}

			/* Partially healed */
			else
			{
				/* Message */
				if (seen)
				{
#ifdef JP
					msg_format("%^sは体力を回復したようだ。", m_name);
#else
					msg_format("%^s looks healthier.", m_name);
#endif

				}
				else
				{
#ifdef JP
					msg_format("%^sは体力を回復したようだ。", m_name);
#else
					msg_format("%^s sounds healthier.", m_name);
#endif

				}
			}

			/* Redraw (later) if needed */
			if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);
			if (p_ptr->riding == m_idx) p_ptr->redraw |= (PR_UHEALTH);

			/* Cancel fear */
			if (MON_MONFEAR(m_ptr))
			{
				/* Cancel fear */
				(void)set_monster_monfear(m_idx, 0);

				/* Message */
#ifdef JP
				if (seen) msg_format("%^sは勇気を取り戻した。", m_name, m_poss);
#else
				if (seen) msg_format("%^s recovers %s courage.", m_name, m_poss);
#endif

			}
			break;
		}

		/* RF6_INVULNER */
		case 160+3:
		{
			disturb(1, 0);

			/* Message */
			if (!seen)
			{
#ifdef JP
				msg_format("%^sが何かを力強くつぶやいた。", m_name);
#else
				msg_format("%^s mumbles powerfully.", m_name);
#endif

			}
			else
			{
#ifdef JP
				msg_format("%sは無傷の球の呪文を唱えた。", m_name);
#else
				msg_format("%^s casts a Globe of Invulnerability.", m_name);
#endif

			}

			if (!MON_INVULNER(m_ptr)) (void)set_monster_invulner(m_idx, randint1(4) + 4, FALSE);
			break;
		}

		/* RF6_BLINK */
		case 160+4:
		{
			disturb(1, 0);
#ifdef JP
			msg_format("%^sが瞬時に消えた。", m_name);
#else
			msg_format("%^s blinks away.", m_name);
#endif

			teleport_away(m_idx, 10);
			p_ptr->update |= (PU_MONSTERS);
			break;
		}

		/* RF6_TPORT */
		case 160+5:
		{
			int i, oldfy, oldfx;
			u32b flgs[TR_FLAG_SIZE];
			object_type *o_ptr;

			oldfy = m_ptr->fy;
			oldfx = m_ptr->fx;

			disturb(1, 0);
#ifdef JP
			msg_format("%^sがテレポートした。", m_name);
#else
			msg_format("%^s teleports away.", m_name);
#endif

			teleport_away(m_idx, MAX_SIGHT * 2 + 5);

			if (los(py, px, oldfy, oldfx) && !stop_the_time_monster)
			{
				for (i=INVEN_RARM;i<INVEN_TOTAL;i++)
				{
					o_ptr = &inventory[i];
					if(!object_is_cursed(o_ptr))
					{
						object_flags(o_ptr, flgs);

						if (have_flag(flgs, TR_TELEPORT) || (p_ptr->muta1 & MUT1_VTELEPORT))
						{
#ifdef JP
							if(get_check_strict("ついていきますか？", CHECK_OKAY_CANCEL))
#else
							if(get_check_strict("Do you follow it? ", CHECK_OKAY_CANCEL))
#endif
							{
								if (one_in_(3))
								{
									teleport_player(200);
#ifdef JP
									msg_print("失敗！");
#else
									msg_print("Failed!");
#endif
								}
								else teleport_player_to(m_ptr->fy, m_ptr->fx, TRUE, TRUE);
								p_ptr->energy_need += ENERGY_NEED();
							}
							break;
						}
					}
				}
			}
			break;
		}

		/* RF6_STOP_TIME */
		case 160+6:
		{
			disturb(1, 0);
			if (!process_stop_the_time(randint1(2)+2, TRUE)) return (FALSE);
			break;
		}

		/* RF6_SPECIAL */
		case 160+7:
		{
			disturb(1, 0);
			switch (m_ptr->r_idx)
			{
			case MON_OZ:
				if ((x != px) || (y != py)) return (FALSE);
				if ((m_ptr->cdis > 2) || !clean_shot(m_ptr->fy, m_ptr->fx, py, px, FALSE, TRUE)) return FALSE;
				disturb(1, 0);
#ifdef JP
				msg_format("%s「貴様のツラは見飽きたよ…。終わりにしようぜ！ブラックプリズン！」", m_name);
#else
				msg_format("%^s mumbles powerfully.", m_name);
#endif

				dam = 3 * rlev + 100;
				special_blow(y, x, m_idx, GF_DARK, dam);
				update_smart_learn(m_idx, DRS_DARK);
				if (p_ptr->free_act) update_smart_learn(m_idx, DRS_FREE);
				else if (randint0(100 + rlev/2) >= p_ptr->skill_sav)
					(void)set_paralyzed(p_ptr->paralyzed + randint0(4) + 4);
				break;

			case MON_SISTEENA:
#ifdef JP
				msg_format("%s「風神を統べる偉大なるハーネラよ…、我らに力強き風の庇護を与えたまえ！」", m_name);
#else
				msg_format("%^s mumbles powerfully.", m_name);
#endif

				inc_area_elem(m_idx, ELEM_WIND, 99, 2, FALSE);
				break;

			case MON_SELYE:
#ifdef JP
				msg_format("%s「火神を統べる偉大なるゾショネルよ…、我らに熱き炎の庇護を与えたまえ！」", m_name);
#else
				msg_format("%^s mumbles powerfully.", m_name);
#endif

				inc_area_elem(m_idx, ELEM_FIRE, 99, 2, FALSE);
				break;

			case MON_OZMA:
				if ((x != px) || (y != py)) return (FALSE);
				if ((m_ptr->cdis > 2) || !clean_shot(m_ptr->fy, m_ptr->fx, py, px, FALSE, TRUE)) return FALSE;
				disturb(1, 0);
#ifdef JP
				msg_format("%s「根性だけは認めてあげるわ…。でも、そこまでよ、デーモンローズ！」", m_name);
#else
				msg_format("%^s mumbles powerfully.", m_name);
#endif

				dam = 3 * rlev + 100;
				special_blow(y, x, m_idx, GF_PURE_FIRE, dam);
				if (p_ptr->resist_conf) update_smart_learn(m_idx, DRS_CONF);
				else if (randint0(100 + rlev/2) >= p_ptr->skill_sav)
					(void)set_confused(p_ptr->confused + randint0(4) + 4);
				break;

			case MON_SHELLEY:
#ifdef JP
				msg_format("%s「大地神を統べる偉大なるバーサよ…、我らに優しき大地の庇護を与えたまえ！」", m_name);
#else
				msg_format("%^s mumbles powerfully.", m_name);
#endif

				inc_area_elem(m_idx, ELEM_EARTH, 99, 2, FALSE);
				break;

			case MON_VOLAC:
				if ((x != px) || (y != py)) return (FALSE);
				if ((m_ptr->cdis > 2) || !clean_shot(m_ptr->fy, m_ptr->fx, py, px, FALSE, TRUE)) return FALSE;
				disturb(1, 0);
#ifdef JP
				msg_format("%s「ロスローリアンの力をなめてもらっては困るのだよ…。ライアットバーン！」", m_name);
#else
				msg_format("%^s mumbles powerfully.", m_name);
#endif

				dam = 3 * rlev + 100;
				special_blow(y, x, m_idx, GF_HOLY_FIRE, dam);
				break;

			case MON_MARTYM:
				if ((x != px) || (y != py)) return (FALSE);
				if ((m_ptr->cdis > 2) || !clean_shot(m_ptr->fy, m_ptr->fx, py, px, FALSE, TRUE)) return FALSE;
				disturb(1, 0);
#ifdef JP
				msg_format("%s「このオレとサシで戦おうなんざ、10年早ぇんだよ！バカめッ！フローヴェノム！」", m_name);
#else
				msg_format("%^s mumbles powerfully.", m_name);
#endif

				dam = 3 * rlev + 100;
				special_blow(y, x, m_idx, GF_WATER, dam);
				update_smart_learn(m_idx, DRS_WATER);
				break;

			case MON_BARBAS:
				if ((x != px) || (y != py)) return (FALSE);
				if ((m_ptr->cdis > 2) || !clean_shot(m_ptr->fy, m_ptr->fx, py, px, FALSE, TRUE)) return FALSE;
				disturb(1, 0);
#ifdef JP
				msg_format("%s「このオレの奥義が見たいか！？いい度胸だ。デスアベンジャー(1)！」", m_name);
#else
				msg_format("%^s mumbles powerfully.", m_name);
#endif

				dam = 3 * rlev + 100;
				special_blow(y, x, m_idx, GF_PURE_EARTH, dam);
				if (!p_ptr->is_dead) knock_back(m_idx, y, x, 200);
				break;

			case MON_OLIVIA:
#ifdef JP
				msg_format("%s「水神を統べる偉大なるグルーザよ…、我らに清らかな水の庇護を与えたまえ！」", m_name);
#else
				msg_format("%^s mumbles powerfully.", m_name);
#endif

				inc_area_elem(m_idx, ELEM_AQUA, 99, 2, FALSE);
				break;

			case MON_BALZEPHO:
				if ((x != px) || (y != py)) return (FALSE);
				if ((m_ptr->cdis > 2) || !clean_shot(m_ptr->fy, m_ptr->fx, py, px, FALSE, TRUE)) return FALSE;
				disturb(1, 0);
#ifdef JP
				msg_format("%s「真の騎士の誇りを貴様に見せてやろう！くらえッ！フレイミングデス！」", m_name);
#else
				msg_format("%^s mumbles powerfully.", m_name);
#endif

				dam = 3 * rlev + 100;
				special_blow(y, x, m_idx, GF_PURE_FIRE, dam);
				break;

			case MON_ANDORAS:
				if ((x != px) || (y != py)) return (FALSE);
				if ((m_ptr->cdis > 2) || !clean_shot(m_ptr->fy, m_ptr->fx, py, px, FALSE, TRUE)) return FALSE;
				disturb(1, 0);
#ifdef JP
				msg_format("%s「フン！命知らずの愚か者めッ！我が奥義を防げるかな？いくぞッ！サンダーブレイド！」", m_name);
#else
				msg_format("%^s mumbles powerfully.", m_name);
#endif

				dam = 3 * rlev + 100;
				special_blow(y, x, m_idx, GF_ELEC, dam);
				update_smart_learn(m_idx, DRS_ELEC);
				break;

			case MON_LANCELOT:
				if ((x != px) || (y != py)) return (FALSE);
				if ((m_ptr->cdis > 2) || !clean_shot(m_ptr->fy, m_ptr->fx, py, px, FALSE, TRUE)) return FALSE;
				disturb(1, 0);
#ifdef JP
				msg_format("%s「ローディスに逆らう愚か者め…。我が奥義を受けてみよ！アポカリプス！」", m_name);
#else
				msg_format("%^s mumbles powerfully.", m_name);
#endif

				dam = 3 * rlev + 100;
				special_blow(y, x, m_idx, GF_OLD_DRAIN, dam);
				update_smart_learn(m_idx, DRS_DRAIN);
				break;

			case MON_DENIM:
				if ((x != px) || (y != py)) return (FALSE);
				if ((m_ptr->cdis > 2) || !clean_shot(m_ptr->fy, m_ptr->fx, py, px, FALSE, TRUE)) return FALSE;
				disturb(1, 0);
#ifdef JP
				msg_format("%s「僕は負けるわけにはいかないッ！くらえ！神鳴明王剣！」", m_name);
#else
				msg_format("%^s mumbles powerfully.", m_name);
#endif

				dam = 3 * rlev + 100;
				special_blow(y, x, m_idx, GF_GODLY_SPEAR, dam);
				break;

			default:
				if (r_ptr->d_char == 'B')
				{
					disturb(1, 0);
					if (one_in_(3) || (x != px) || (y != py))
					{
#ifdef JP
						if (seen) msg_format("%^sは突然視界から消えた!", m_name);
#else
						if (seen) msg_format("%^s suddenly go out of your sight!", m_name);
#endif
						teleport_away(m_idx, 10);
						p_ptr->update |= (PU_MONSTERS);
					}
					else
					{
						int get_damage = 0;

						dam = damroll(4, 8);
#ifdef JP
						msg_format("%^sがあなたを掴んで空中から投げ落した。", m_name);
#else
						msg_format("%^s holds you, and drops from the sky.", m_name);
#endif
						teleport_player_to(m_ptr->fy, m_ptr->fx, FALSE, FALSE);

						if (!p_ptr->leaving)
						{
							sound(SOUND_FALL);

							if (cave[py][px].feat != FEAT_AIR)
							{
								if (p_ptr->ffall)
								{
#ifdef JP
									msg_print("あなたは静かに着地した。");
#else
									msg_print("You float gently down to the ground.");
#endif
								}
								else
								{
#ifdef JP
									msg_print("あなたは地面に叩きつけられた。");
#else
									msg_print("You crashed into the ground.");
#endif
									dam += damroll(6, 8);
								}
							}
						}

						dam = modify_dam_by_elem(m_idx, 0, dam, GF_MISSILE, MODIFY_ELEM_MODE_MELEE);

						/* Mega hack -- this special action deals damage to the player. Therefore the code of "eyeeye" is necessary.
						   -- henkma
						 */
						get_damage = take_hit(DAMAGE_NOESCAPE, dam, m_name);
						if (p_ptr->tim_eyeeye && get_damage > 0 && !p_ptr->is_dead && !p_ptr->leaving)
						{
#ifdef JP
							msg_format("攻撃が%s自身を傷つけた！", m_name);
#else
							char m_name_self[80];

							/* hisself */
							monster_desc(m_name_self, m_ptr, MD_PRON_VISIBLE | MD_POSSESSIVE | MD_OBJECTIVE);

							msg_format("The attack of %s has wounded %s!", m_name, m_name_self);
#endif
							project(0, 0, m_ptr->fy, m_ptr->fx, get_damage, GF_MISSILE, PROJECT_KILL, MODIFY_ELEM_MODE_MAGIC);
							set_tim_eyeeye(p_ptr->tim_eyeeye-5, TRUE);
						}
					}
					break;
				}

				/* Something is wrong */
				else return FALSE;
			}
			break;
		}

		/* RF6_TELE_TO */
		case 160+8:
		{
			if ((x != px) || (y != py)) return (FALSE);
			disturb(1, 0);
#ifdef JP
			msg_format("%^sがあなたを引き戻した。", m_name);
#else
			msg_format("%^s commands you to return.", m_name);
#endif

			if (p_ptr->earth_spike)
#ifdef JP
				msg_print("しかし効力を跳ね返した！");
#else
				msg_print("You resist the effects!");
#endif
			else teleport_player_to(m_ptr->fy, m_ptr->fx, TRUE, FALSE);
			update_smart_learn(m_idx, DRS_TELE);
			break;
		}

		/* RF6_TELE_AWAY */
		case 160+9:
		{
			if ((x != px) || (y != py)) return (FALSE);
			disturb(1, 0);
#ifdef JP
			msg_format("%^sにテレポートさせられた。", m_name);
#else
			msg_format("%^s teleports you away.", m_name);
#endif

			if (p_ptr->earth_spike)
#ifdef JP
				msg_print("しかし効力を跳ね返した！");
#else
				msg_print("You resist the effects!");
#endif
			else teleport_player(100);
			update_smart_learn(m_idx, DRS_TELE);
			break;
		}

		/* RF6_TELE_LEVEL */
		case 160+10:
		{
			if ((x != px) || (y != py)) return (FALSE);
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何か奇妙な言葉をつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles strangely.", m_name);
#endif

#ifdef JP
			else msg_format("%^sがあなたの足を指さした。", m_name);
#else
			else msg_format("%^s gestures at your feet.", m_name);
#endif

			if ((randint0(100 + rlev/2) < p_ptr->skill_sav) || p_ptr->earth_spike)
			{
#ifdef JP
				msg_print("しかし効力を跳ね返した！");
#else
				msg_print("You resist the effects!");
#endif

			}
			else teleport_level(0);
			update_smart_learn(m_idx, DRS_TELE);
			break;
		}

		/* RF6_GODLY_SPEAR */
		case 160+11:
		{
			if ((x != px) || (y != py)) return (FALSE);
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かをつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが神の槍を放った。", m_name);
#else
			else msg_format("%^s throws a godly spear.", m_name);
#endif

			dam = (r_ptr->flags2 & RF2_POWERFUL) ? (randint1(rlev * 2) + 150) : (randint1(rlev * 3 / 2) + 100);
			beam(y, x, m_idx, GF_GODLY_SPEAR, dam);
			break;
		}

		/* RF6_DARKNESS */
		case 160+12:
		{
			if ((x != px) || (y != py)) return (FALSE);
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かをつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles.", m_name);
#endif

#ifdef JP
			else if (can_use_lite_area) msg_format("%^sが辺りを明るく照らした。", m_name);
			else msg_format("%^sが暗闇の中で手を振った。", m_name);
#else
			else if (can_use_lite_area) msg_format("%^s cast a spell to light up.", m_name);
			else msg_format("%^s gestures in shadow.", m_name);
#endif

			if (can_use_lite_area) (void)lite_area(0, 3);
			else (void)unlite_area(0, 3);
			break;
		}

		/* RF6_TRAPS */
		case 160+13:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かをつぶやいて邪悪に微笑んだ。", m_name);
#else
			if (blind) msg_format("%^s mumbles, and then cackles evilly.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが呪文を唱えて邪悪に微笑んだ。", m_name);
#else
			else msg_format("%^s casts a spell and cackles evilly.", m_name);
#endif

			(void)trap_creation(y, x);
			break;
		}

		/* RF6_FORGET */
		case 160+14:
		{
			if ((x != px) || (y != py)) return (FALSE);
			disturb(1, 0);
#ifdef JP
			msg_format("%^sがあなたの記憶を消去しようとしている。", m_name);
#else
			msg_format("%^s tries to blank your mind.", m_name);
#endif


			if (randint0(100 + rlev/2) < p_ptr->skill_sav)
			{
#ifdef JP
				msg_print("しかし効力を跳ね返した！");
#else
				msg_print("You resist the effects!");
#endif

			}
			else if (lose_all_info())
			{
#ifdef JP
				msg_print("記憶が薄れてしまった。");
#else
				msg_print("Your memories fade away.");
#endif

			}
			break;
		}

		/* RF6_RAISE_DEAD */
		case 160+15:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かをつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが死者復活の呪文を唱えた。", m_name);
#else
			else msg_format("%^s casts a spell to revive corpses.", m_name);
#endif
			animate_dead(m_idx, m_ptr->fy, m_ptr->fx);
			break;
		}

		/* RF6_SUMMON_KIN */
		case 160+16:
		{
			disturb(1, 0);
#ifdef JP
			if (blind)
				msg_format("%^sが何かをつぶやいた。", m_name);
			else
				msg_format("%^sは魔法で%sを召喚した。",
				m_name,
				((r_ptr->flags1 & RF1_UNIQUE) || (r_ptr->flags7 & RF7_UNIQUE2) ?
				"手下" : "仲間"));
#else
			if (blind)
				msg_format("%^s mumbles.", m_name);
			else
				msg_format("%^s magically summons %s %s.",
				m_name, m_poss,
				((r_ptr->flags1 & RF1_UNIQUE) || (r_ptr->flags7 & RF7_UNIQUE2) ?
				"minions" : "kin"));
#endif

			switch (m_ptr->r_idx)
			{
			case MON_THORONDOR:
			case MON_GWAIHIR:
			case MON_MENELDOR:
				{
					int num = 4 + randint1(3);
					for (k = 0; k < num; k++)
					{
						count += summon_specific(m_idx, y, x, rlev, SUMMON_EAGLES, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE));
					}
				}
				break;
			case MON_ELEM_L_FIRE:
				{
					int num = 4 + randint1(3);
					for (k = 0; k < num; k++)
					{
						count += summon_specific(m_idx, y, x, rlev, SUMMON_FIRE, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE));
					}
				}
				break;
			case MON_ELEM_L_AQUA:
				{
					int num = 4 + randint1(3);
					for (k = 0; k < num; k++)
					{
						count += summon_specific(m_idx, y, x, rlev, SUMMON_AQUA, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE));
					}
				}
				break;
			case MON_ELEM_L_EARTH:
				{
					int num = 4 + randint1(3);
					for (k = 0; k < num; k++)
					{
						count += summon_specific(m_idx, y, x, rlev, SUMMON_EARTH, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE));
					}
				}
				break;
			case MON_ELEM_L_WIND:
				{
					int num = 4 + randint1(3);
					for (k = 0; k < num; k++)
					{
						count += summon_specific(m_idx, y, x, rlev, SUMMON_WIND, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE));
					}
				}
				break;
			default:
				{
					summon_kin_type = r_ptr->d_char;

					for (k = 0; k < 4; k++)
					{
						count += summon_specific(m_idx, y, x, rlev, SUMMON_KIN, (PM_ALLOW_GROUP));
					}
				}
				break;
			}

#ifdef JP
			if (blind && count) msg_print("多くのものが間近に現れた音がする。");
#else
			if (blind && count) msg_print("You hear many things appear nearby.");
#endif


			break;
		}

		/* RF6_S_XXX1 */
		case 160+17:
		{
			break;
		}

		/* RF6_S_MONSTER */
		case 160+18:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かをつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが魔法で仲間を召喚した！", m_name);
#else
			else msg_format("%^s magically summons help!", m_name);
#endif

			for (k = 0; k < 1; k++)
			{
				count += summon_specific(m_idx, y, x, rlev, 0, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE));
			}
#ifdef JP
			if (blind && count) msg_print("何かが間近に現れた音がする。");
#else
			if (blind && count) msg_print("You hear something appear nearby.");
#endif

			break;
		}

		/* RF6_S_MONSTERS */
		case 160+19:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かをつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが魔法でモンスターを召喚した！", m_name);
#else
			else msg_format("%^s magically summons monsters!", m_name);
#endif

			for (k = 0; k < s_num_6; k++)
			{
				count += summon_specific(m_idx, y, x, rlev, 0, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE));
			}
#ifdef JP
			if (blind && count) msg_print("多くのものが間近に現れた音がする。");
#else
			if (blind && count) msg_print("You hear many things appear nearby.");
#endif

			break;
		}

		/* RF6_S_ANT */
		case 160+20:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かをつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが魔法でアリを召喚した。", m_name);
#else
			else msg_format("%^s magically summons ants.", m_name);
#endif

			for (k = 0; k < s_num_6; k++)
			{
				count += summon_specific(m_idx, y, x, rlev, SUMMON_ANT, PM_ALLOW_GROUP);
			}
#ifdef JP
			if (blind && count) msg_print("多くのものが間近に現れた音がする。");
#else
			if (blind && count) msg_print("You hear many things appear nearby.");
#endif

			break;
		}

		/* RF6_S_SPIDER */
		case 160+21:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かをつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが魔法でクモを召喚した。", m_name);
#else
			else msg_format("%^s magically summons spiders.", m_name);
#endif

			for (k = 0; k < s_num_6; k++)
			{
				count += summon_specific(m_idx, y, x, rlev, SUMMON_SPIDER, PM_ALLOW_GROUP);
			}
#ifdef JP
			if (blind && count) msg_print("多くのものが間近に現れた音がする。");
#else
			if (blind && count) msg_print("You hear many things appear nearby.");
#endif

			break;
		}

		/* RF6_S_HOUND */
		case 160+22:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かをつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが魔法でハウンドを召喚した。", m_name);
#else
			else msg_format("%^s magically summons hounds.", m_name);
#endif

			for (k = 0; k < s_num_4; k++)
			{
				count += summon_specific(m_idx, y, x, rlev, SUMMON_HOUND, PM_ALLOW_GROUP);
			}
#ifdef JP
			if (blind && count) msg_print("多くのものが間近に現れた音がする。");
#else
			if (blind && count) msg_print("You hear many things appear nearby.");
#endif

			break;
		}

		/* RF6_S_BEAST */
		case 160+23:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かをつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles.", m_name);
#endif

			else if (m_ptr->r_idx == MON_GAMP)
#ifdef JP
				msg_format("%^sが魔法で魔獣の親友を召喚した。", m_name);
#else
				msg_format("%^s magically summons friends of beasts.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが魔法で魔獣を召喚した。", m_name);
#else
			else msg_format("%^s magically summons beasts.", m_name);
#endif

			if (m_ptr->r_idx == MON_GAMP)
			{
				int attempt;
				s16b friend_beasts_r_idx[] =
				{
					MON_OBDA, MON_BELDA, MON_ZANGA, MON_BANGA,
					MON_OBDA_JR, MON_BELDA_JR,
				};

				for (k = 0; k < 2; k++)
				{
					attempt = 5000;
					while (attempt)
					{
						if (summon_named_creature(m_idx, y, x, friend_beasts_r_idx[randint0((sizeof friend_beasts_r_idx) / (sizeof (s16b)))], mode))
						{
							count++;
							break;
						}
						attempt--;
					}
				}
			}
			else
			{
				for (k = 0; k < s_num_4; k++)
				{
					count += summon_specific(m_idx, y, x, rlev, SUMMON_BEAST, PM_ALLOW_GROUP);
				}
			}
#ifdef JP
			if (blind && count) msg_print("多くのものが間近に現れた音がする。");
#else
			if (blind && count) msg_print("You hear many things appear nearby.");
#endif

			break;
		}

		/* RF6_S_ANGEL */
		case 160+24:
		{
			int num = 1;

			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かをつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが魔法で天使を召喚した！", m_name);
#else
			else msg_format("%^s magically summons an angel!", m_name);
#endif

			if ((r_ptr->flags1 & RF1_UNIQUE) && !easy_band)
			{
				num += r_ptr->level/40;
			}

			for (k = 0; k < num; k++)
			{
				count += summon_specific(m_idx, y, x, rlev, SUMMON_ANGEL, PM_ALLOW_GROUP);
			}

			if (count < 2)
			{
#ifdef JP
				if (blind && count) msg_print("何かが間近に現れた音がする。");
#else
				if (blind && count) msg_print("You hear something appear nearby.");
#endif
			}
			else
			{
#ifdef JP
				if (blind) msg_print("多くのものが間近に現れた音がする。");
#else
				if (blind) msg_print("You hear many things appear nearby.");
#endif
			}

			break;
		}

		/* RF6_S_DEMON */
		case 160+25:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かをつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが魔法で地獄の強敵を召喚した！", m_name);
#else
			else msg_format("%^s magically summons a hellish adversary!", m_name);
#endif

			for (k = 0; k < 1; k++)
			{
				count += summon_specific(m_idx, y, x, rlev, SUMMON_DEMON, PM_ALLOW_GROUP);
			}
#ifdef JP
			if (blind && count) msg_print("何かが間近に現れた音がする。");
#else
			if (blind && count) msg_print("You hear something appear nearby.");
#endif

			break;
		}

		/* RF6_S_UNDEAD */
		case 160+26:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かをつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが魔法でアンデッドの強敵を召喚した！", m_name);
#else
			else msg_format("%^s magically summons an undead adversary!", m_name);
#endif

			for (k = 0; k < 1; k++)
			{
				count += summon_specific(m_idx, y, x, rlev, SUMMON_UNDEAD, PM_ALLOW_GROUP);
			}
#ifdef JP
			if (blind && count) msg_print("何かが間近に現れた音がする。");
#else
			if (blind && count) msg_print("You hear something appear nearby.");
#endif

			break;
		}

		/* RF6_S_DRAGON */
		case 160+27:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かをつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが魔法でドラゴンを召喚した！", m_name);
#else
			else msg_format("%^s magically summons a dragon!", m_name);
#endif

			for (k = 0; k < 1; k++)
			{
				count += summon_specific(m_idx, y, x, rlev, SUMMON_DRAGON, PM_ALLOW_GROUP);
			}
#ifdef JP
			if (blind && count) msg_print("何かが間近に現れた音がする。");
#else
			if (blind && count) msg_print("You hear something appear nearby.");
#endif

			break;
		}

		/* RF6_S_HI_UNDEAD */
		case 160+28:
		{
			disturb(1, 0);

			if ((m_ptr->r_idx == MON_ANGMAR) && ((r_info[MON_NAZGUL].cur_num+2) < r_info[MON_NAZGUL].max_num))
			{
				int cy = y;
				int cx = x;

#ifdef JP
				if (blind) msg_format("%^sが何かをつぶやいた。", m_name);
#else
				if (blind) msg_format("%^s mumbles.", m_name);
#endif

#ifdef JP
				else msg_format("%^sが魔法で指輪の幽鬼を召喚した！", m_name);
#else
				else msg_format("%^s magically summons regions of Nazgul!", m_name);
#endif

				for (k = 0; k < 30; k++)
				{
					if (!summon_possible(m_idx, cy, cx) || !cave_floor_bold(cy, cx))
					{
						int j;
						for (j = 100; j > 0; j--)
						{
							scatter(&cy, &cx, y, x, 2, 0);
							if (cave_floor_bold(cy, cx)) break;
						}
						if (!j) break;
					}
					if (!cave_floor_bold(cy, cx)) continue;

					if (summon_named_creature(m_idx, cy, cx, MON_NAZGUL, mode))
					{
						y = cy;
						x = cx;
						count++;
					}
				}
			}
			else
			{
#ifdef JP
				if (blind) msg_format("%^sが何かをつぶやいた。", m_name);
#else
				if (blind) msg_format("%^s mumbles.", m_name);
#endif

#ifdef JP
				else msg_format("%^sが魔法で強力なアンデッドを召喚した！", m_name);
#else
				else msg_format("%^s magically summons greater undead!", m_name);
#endif

				for (k = 0; k < s_num_6; k++)
				{
					count += summon_specific(m_idx, y, x, rlev, SUMMON_HI_UNDEAD, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE));
				}
			}
			if (blind && count)
			{
#ifdef JP
				msg_print("間近で何か多くのものが這い回る音が聞こえる。");
#else
				msg_print("You hear many creepy things appear nearby.");
#endif

			}
			break;
		}

		/* RF6_S_HI_DRAGON */
		case 160+29:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かをつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが魔法で古代ドラゴンを召喚した！", m_name);
#else
			else msg_format("%^s magically summons ancient dragons!", m_name);
#endif

			for (k = 0; k < s_num_4; k++)
			{
				count += summon_specific(m_idx, y, x, rlev, SUMMON_HI_DRAGON, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE));
			}
			if (blind && count)
			{
#ifdef JP
				msg_print("多くの力強いものが間近に現れた音が聞こえる。");
#else
				msg_print("You hear many powerful things appear nearby.");
#endif

			}
			break;
		}

		/* RF6_S_TEMPLES */
		case 160+30:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かをつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが暗黒騎士を召喚した！", m_name);
#else
			else msg_format("%^s magically summons Temple Knights!", m_name);
#endif



			for (k = 0; k < s_num_4; k++)
			{
				count += summon_specific(m_idx, y, x, rlev, SUMMON_TEMPLES, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE));
			}
			if (blind && count)
			{
#ifdef JP
				msg_print("多くの力強いものが間近に現れた音が聞こえる。");
#else
				msg_print("You hear many powerful things appear nearby.");
#endif

			}
			break;
		}

		/* RF6_S_UNIQUE */
		case 160+31:
		{
			bool uniques_are_summoned = FALSE;
			int non_unique_type = SUMMON_HI_UNDEAD;

			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かをつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが魔法で特別な強敵を召喚した！", m_name);
#else
			else msg_format("%^s magically summons special opponents!", m_name);
#endif

			for (k = 0; k < s_num_4; k++)
			{
				count += summon_specific(m_idx, y, x, rlev, SUMMON_UNIQUE, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE));
			}

			if (count) uniques_are_summoned = TRUE;

			if ((m_ptr->sub_align & (SUB_ALIGN_GOOD | SUB_ALIGN_EVIL)) == (SUB_ALIGN_GOOD | SUB_ALIGN_EVIL))
				non_unique_type = 0;
			else if (m_ptr->sub_align & SUB_ALIGN_GOOD)
				non_unique_type = SUMMON_ANGEL;

			for (k = count; k < s_num_4; k++)
			{
				count += summon_specific(m_idx, y, x, rlev, non_unique_type, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE));
			}

			if (blind && count)
			{
#ifdef JP
				msg_format("多くの%sが間近に現れた音が聞こえる。", uniques_are_summoned ? "力強いもの" : "もの");
#else
				msg_format("You hear many %s appear nearby.", uniques_are_summoned ? "powerful things" : "things");
#endif
			}
			break;
		}

		/* RFA_FIRE_STORM */
		case 288+0:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かを力強くつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles powerfully.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが*火炎*の嵐の呪文を念じた。", m_name);
#else
			else msg_format("%^s invokes a *fire* storm.", m_name);
#endif

			dam = (rlev * 4) + 50 + damroll(10, 10);
			breath(y, x, m_idx, GF_PURE_FIRE, dam, 4, FALSE, FALSE);
			m_ptr->energy_need += ENERGY_NEED();
			break;
		}

		/* RFA_AQUA_STORM */
		case 288+1:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かを力強くつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles powerfully.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが*水*の嵐の呪文を念じた。", m_name);
#else
			else msg_format("%^s invokes a *aqua* storm.", m_name);
#endif

			dam = (rlev * 4) + 50 + damroll(10, 10);
			breath(y, x, m_idx, GF_PURE_AQUA, dam, 4, FALSE, FALSE);
			m_ptr->energy_need += ENERGY_NEED();
			break;
		}

		/* RFA_EARTH_STORM */
		case 288+2:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かを力強くつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles powerfully.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが*大地*の嵐の呪文を念じた。", m_name);
#else
			else msg_format("%^s invokes a *earth* storm.", m_name);
#endif

			dam = (rlev * 4) + 50 + damroll(10, 10);
			breath(y, x, m_idx, GF_PURE_EARTH, dam, 4, FALSE, FALSE);
			m_ptr->energy_need += ENERGY_NEED();
			break;
		}

		/* RFA_WIND_STORM */
		case 288+3:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かを力強くつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles powerfully.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが*風*の嵐の呪文を念じた。", m_name);
#else
			else msg_format("%^s invokes a *wind* storm.", m_name);
#endif

			dam = (rlev * 4) + 50 + damroll(10, 10);
			breath(y, x, m_idx, GF_PURE_WIND, dam, 4, FALSE, FALSE);
			m_ptr->energy_need += ENERGY_NEED();
			break;
		}

		/* RFA_BR_PURE_FIRE */
		case 288+4:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
			if (blind) msg_format("%^s breathes.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが*火炎*のブレスを吐いた。", m_name);
#else
			else msg_format("%^s breathes *fire*.", m_name);
#endif
			sound(SOUND_BREATH);

			dam = ((m_ptr->hp / 3) > 700 ? 700 : (m_ptr->hp / 3));
			breath(y, x, m_idx, GF_PURE_FIRE, dam, 0, TRUE, FALSE);
			m_ptr->energy_need += 2 * ENERGY_NEED();
			break;
		}

		/* RFA_BR_PURE_AQUA */
		case 288+5:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
			if (blind) msg_format("%^s breathes.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが*水*のブレスを吐いた。", m_name);
#else
			else msg_format("%^s breathes *aqua*.", m_name);
#endif
			sound(SOUND_BREATH);

			dam = ((m_ptr->hp / 3) > 700 ? 700 : (m_ptr->hp / 3));
			breath(y, x, m_idx, GF_PURE_AQUA, dam, 0, TRUE, FALSE);
			m_ptr->energy_need += 2 * ENERGY_NEED();
			break;
		}

		/* RFA_BR_PURE_EARTH */
		case 288+6:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
			if (blind) msg_format("%^s breathes.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが*大地*のブレスを吐いた。", m_name);
#else
			else msg_format("%^s breathes *earth*.", m_name);
#endif
			sound(SOUND_BREATH);

			dam = ((m_ptr->hp / 3) > 700 ? 700 : (m_ptr->hp / 3));
			breath(y, x, m_idx, GF_PURE_EARTH, dam, 0, TRUE, FALSE);
			m_ptr->energy_need += 2 * ENERGY_NEED();
			break;
		}

		/* RFA_BR_PURE_WIND */
		case 288+7:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かのブレスを吐いた。", m_name);
#else
			if (blind) msg_format("%^s breathes.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが*風*のブレスを吐いた。", m_name);
#else
			else msg_format("%^s breathes *wind*.", m_name);
#endif
			sound(SOUND_BREATH);

			dam = ((m_ptr->hp / 3) > 700 ? 700 : (m_ptr->hp / 3));
			breath(y, x, m_idx, GF_PURE_WIND, dam, 0, TRUE, FALSE);
			m_ptr->energy_need += 2 * ENERGY_NEED();
			break;
		}

		/* RFA_PETRO_CLOUD */
		case 288+8:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かをつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles.", m_name);

#endif

#ifdef JP
			else msg_format("%^sがペトロクラウドの呪文を唱えた。", m_name);
#else
			else msg_format("%^s casts a petrocloud.", m_name);
#endif

			dam = 100 + rlev * 3;
			breath(y, x, m_idx, GF_STONE, dam, 3, FALSE, TRUE);
			update_smart_learn(m_idx, DRS_STONE);
			break;
		}

		/* RFA_SAND_STORM */
		case 288+9:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かを力強くつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles powerfully.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが神砂嵐の呪文を念じた。", m_name);
#else
			else msg_format("%^s invokes a godly sand storm.", m_name);
#endif

			dam = (rlev * 4) + 50 + damroll(10, 10);
			breath(y, x, m_idx, GF_SHARDS, dam, 4, FALSE, FALSE);
			update_smart_learn(m_idx, DRS_SHARD);
			break;
		}

		/* RFA_ERASE_ELEM */
		case 288+10:
		{
#ifdef JP
			if (blind) msg_format("%^sが何かを力強くつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles powerfully.", m_name);
#endif

#ifdef JP
			else msg_format("%^sがエレメント消去の呪文を念じた。", m_name);
#else
			else msg_format("%^s invokes element erasing.", m_name);
#endif

			if (erase_elem_target_mode & ERASE_ELEM_MODE_PLAYER)
				breath(y, x, m_idx, GF_ERASE_ELEM, 0, 3, FALSE, FALSE);
			else if (erase_elem_target_mode & ERASE_ELEM_MODE_CASTER)
				breath(m_ptr->fy, m_ptr->fx, m_idx, GF_ERASE_ELEM, 0, 3, FALSE, FALSE);
			erase_elem_target_mode = ERASE_ELEM_MODE_NONE;
			break;
		}

		/* RFA_CHANGE_ELEM */
		case 288+11:
		{
#ifdef JP
			if (blind) msg_format("%^sが何かをつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles.", m_name);
#endif

#ifdef JP
			else msg_format("%^sがエレメント・チェンジの呪文を唱えた。", m_name);
#else
			else msg_format("%^s casts changing elements.", m_name);
#endif

			if (!p_ptr->opposite_pelem) set_opposite_pelem(12 + randint1(12));
			break;
		}

		/* RFA_SALAMANDER */
		case 288+12:
		{
			disturb(1, 0);

#ifdef JP
			msg_format("%^sはあなたを焼き払うように精霊サラマンダーに命じた！", m_name);
#else
			msg_format("%^s orders the Salamander to burn you!.", m_name);
#endif

			if (mon_cast_call_the_elemental(m_idx, y, x, rlev, (r_ptr->flags2 & (RF2_POWERFUL)) ? 2 : 1, rlev, 5, GF_FIRE))
				update_smart_learn(m_idx, DRS_FIRE);
			break;
		}

		/* RFA_FENRER */
		case 288+13:
		{
			disturb(1, 0);

#ifdef JP
			msg_format("%^sはあなたを氷漬けにするように精霊フェンリルに命じた！", m_name);
#else
			msg_format("%^s orders the Fenrer to freeze you!.", m_name);
#endif

			if (mon_cast_call_the_elemental(m_idx, y, x, rlev, (r_ptr->flags2 & (RF2_POWERFUL)) ? 2 : 1, rlev, 5, GF_COLD))
				update_smart_learn(m_idx, DRS_COLD);
			break;
		}

		/* RFA_GNOME */
		case 288+14:
		{
			disturb(1, 0);

#ifdef JP
			msg_format("%^sはあなたを溶解させるように精霊ノームに命じた！", m_name);
#else
			msg_format("%^s orders the Gnome to melt you!.", m_name);
#endif

			if (mon_cast_call_the_elemental(m_idx, y, x, rlev, (r_ptr->flags2 & (RF2_POWERFUL)) ? 2 : 1, rlev, 5, GF_ACID))
				update_smart_learn(m_idx, DRS_ACID);
			break;
		}

		/* RFA_THUNDERBIRD */
		case 288+15:
		{
			disturb(1, 0);

#ifdef JP
			msg_format("%^sはあなたに雷撃を落とすように精霊サンダーバードに命じた！", m_name);
#else
			msg_format("%^s orders the Thunderbird to hit ligntning on you!.", m_name);
#endif

			if (mon_cast_call_the_elemental(m_idx, y, x, rlev, (r_ptr->flags2 & (RF2_POWERFUL)) ? 2 : 1, rlev, 5, GF_ELEC))
				update_smart_learn(m_idx, DRS_ELEC);
			break;
		}

		/* RFA_IGNIS_FATUUS */
		case 288+16:
		{
			disturb(1, 0);

#ifdef JP
			msg_format("%^sはあなたを神聖な力で消し去るように精霊イグニスファタスに命じた！", m_name);
#else
			msg_format("%^s orders the Ignis Fatuus to erase you by holy force!.", m_name);
#endif

			(void)mon_cast_call_the_elemental(m_idx, y, x, 0, (r_ptr->flags2 & (RF2_POWERFUL)) ? 5 : 4, rlev / 4, 3, GF_HOLY_FIRE);
			break;
		}

		/* RFA_DARK_LORE */
		case 288+17:
		{
			disturb(1, 0);

#ifdef JP
			msg_format("%^sはあなたを邪悪な力で葬り去るように精霊ファントムに命じた！", m_name);
#else
			msg_format("%^s orders the Phantom to slay you by evil force!.", m_name);
#endif

			(void)mon_cast_call_the_elemental(m_idx, y, x, 0, (r_ptr->flags2 & (RF2_POWERFUL)) ? 5 : 4, rlev / 4, 3, GF_HELL_FIRE);
			break;
		}

		/* RFA_STONE_GAZE */
		case 288+18:
		{
			if ((x != px) || (y != py)) return (FALSE);
#ifdef JP
			if (blind) msg_format("%^sの凍るような視線を感じる。", m_name);
#else
			if (blind) msg_format("%^s looks chilly.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが邪眼で周囲を見回した！", m_name);
#else
			else msg_format("%^s looks around with stone gaze!", m_name);
#endif

			if (stone_gaze(m_idx))
				update_smart_learn(m_idx, DRS_STONE);
			break;
		}

		/* RFA_HOLY_ORB */
		case 288+19:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かをつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles.", m_name);

#endif

#ifdef JP
			else msg_format("%^sが聖なる光球の呪文を唱えた。", m_name);
#else
			else msg_format("%^s casts a holy orb.", m_name);
#endif

			dam = damroll(3, 6) + rlev + rlev / 2;
			breath(y, x, m_idx, GF_HOLY_FIRE, dam, (rlev > 29) ? 3 : 2, FALSE, FALSE);
			break;
		}

		/* RFA_DARK_FIRE */
		case 288+20:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かをつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles.", m_name);

#endif

#ifdef JP
			else msg_format("%^sが闇の焔の呪文を唱えた。", m_name);
#else
			else msg_format("%^s casts a petit hell fire.", m_name);
#endif

			dam = damroll(3, 6) + rlev + rlev / 2;
			breath(y, x, m_idx, GF_HELL_FIRE, dam, (rlev > 29) ? 3 : 2, FALSE, FALSE);
			break;
		}

		/* RFA_S_ZENOBIAN */
		case 288+21:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かをつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが神聖騎士を召喚した！", m_name);
#else
			else msg_format("%^s magically summons White Knights!", m_name);
#endif



			for (k = 0; k < s_num_4; k++)
			{
				count += summon_specific(m_idx, y, x, rlev, SUMMON_ZENOBIAN_FORCES, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE));
			}
			if (blind && count)
			{
#ifdef JP
				msg_print("多くの力強いものが間近に現れた音が聞こえる。");
#else
				msg_print("You hear many powerful things appear nearby.");
#endif

			}
			break;
		}

		/* RFA_S_HI_DEMON */
		case 288+22:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かをつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが魔法で上級デーモンを召喚した！", m_name);
#else
			else msg_format("%^s magically summons major demons!", m_name);
#endif

			for (k = 0; k < s_num_4; k++)
			{
				count += summon_specific(m_idx, y, x, rlev, SUMMON_HI_DEMON, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE));
			}
			if (blind && count)
			{
#ifdef JP
				msg_print("多くの力強いものが間近に現れた音が聞こえる。");
#else
				msg_print("You hear many powerful things appear nearby.");
#endif

			}
			break;
		}

		/* RFA_BA_DISI */
		case 288+23:
		{
			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かをつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles.", m_name);

#endif

#ifdef JP
			else msg_format("%^sが分解のエネルギーを放った。", m_name);
#else
			else msg_format("%^s invokes disintegration energy.", m_name);
#endif

			dam = rlev + 70;
			breath(y, x, m_idx, GF_DISINTEGRATE, dam, 4, FALSE, FALSE);
			break;
		}

		/* RFA_PURE_ELEM_BEAM */
		case 288+24:
		{
			int pure_elem_typ = GF_GODLY_SPEAR;
			cptr pure_elem_desc = "(?)";

			if ((x != px) || (y != py)) return (FALSE);

			switch (get_cur_melem(m_ptr))
			{
			case ELEM_FIRE:
				pure_elem_typ = GF_PURE_FIRE;
#ifdef JP
				pure_elem_desc = "*火炎*";
#else
				pure_elem_desc = "*fire*";
#endif
				break;
			case ELEM_AQUA:
				pure_elem_typ = GF_PURE_AQUA;
#ifdef JP
				pure_elem_desc = "*水*";
#else
				pure_elem_desc = "*aqua*";
#endif
				break;
			case ELEM_EARTH:
				pure_elem_typ = GF_PURE_EARTH;
#ifdef JP
				pure_elem_desc = "*大地*";
#else
				pure_elem_desc = "*earth*";
#endif
				break;
			case ELEM_WIND:
				pure_elem_typ = GF_PURE_WIND;
#ifdef JP
				pure_elem_desc = "*風*";
#else
				pure_elem_desc = "*wind*";
#endif
				break;
			}

			disturb(1, 0);
#ifdef JP
			if (blind) msg_format("%^sが何かをつぶやいた。", m_name);
#else
			if (blind) msg_format("%^s mumbles.", m_name);
#endif

#ifdef JP
			else msg_format("%^sが%sのビームを放った。", m_name, pure_elem_desc);
#else
			else msg_format("%^s fires a %s beam.", m_name, pure_elem_desc);
#endif

			dam = (r_ptr->flags2 & RF2_POWERFUL) ? (randint1(rlev * 2) + 150) : (randint1(rlev * 3 / 2) + 100);
			beam(y, x, m_idx, pure_elem_typ, dam);
			break;
		}
	}

	/* Remember what the monster did to us */
	if (seen)
	{
		/* Inate spell */
		if (thrown_spell < 32 * 4)
		{
			r_ptr->r_flags4 |= (1L << (thrown_spell - 32 * 3));
			if (r_ptr->r_cast_spell < MAX_UCHAR) r_ptr->r_cast_spell++;
		}

		/* Bolt or Ball */
		else if (thrown_spell < 32 * 5)
		{
			r_ptr->r_flags5 |= (1L << (thrown_spell - 32 * 4));
			if (r_ptr->r_cast_spell < MAX_UCHAR) r_ptr->r_cast_spell++;
		}

		/* Special spell */
		else if (thrown_spell < 32 * 6)
		{
			r_ptr->r_flags6 |= (1L << (thrown_spell - 32 * 5));
			if (r_ptr->r_cast_spell < MAX_UCHAR) r_ptr->r_cast_spell++;
		}

		/* Additional spell */
		else if (thrown_spell < 32 * 10)
		{
			r_ptr->r_flagsa |= (1L << (thrown_spell - 32 * 9));
			if (r_ptr->r_cast_spell < MAX_UCHAR) r_ptr->r_cast_spell++;
		}
	}


	/* Always take note of monsters that kill you */
	if (p_ptr->is_dead && (r_ptr->r_deaths < MAX_SHORT) && !p_ptr->inside_arena)
	{
		r_ptr->r_deaths++;
	}

	if (p_ptr->action == ACTION_ELEMSCOPE) p_ptr->redraw |= (PR_MAP);

	/* A spell was cast */
	return (TRUE);
}
