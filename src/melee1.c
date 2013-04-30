/* File: melee1.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this cofxright and statement
 * are included in all such copies.  Other cofxrights may also apply.
 *
 * UnAngband (c) 2001-6 Andrew Doull. Modifications to the Angband 2.9.1
 * source code are released under the Gnu Public License. See www.fsf.org
 * for current GPL license details. Addition permission granted to
 * incorporate modifications in all Angband variants as defined in the
 * Angband variants FAQ. See rec.games.roguelike.angband for FAQ.
 */

#include "angband.h"



/*
 * Critical blows by monsters can inflict cuts and stuns.
 */
static int monster_critical(int dice, int sides, int dam, int effect)
{
	int max = 0;
	int bonus;
	int total = dice * sides;

	/* Special case -- wounding/battering attack */
	if ((effect == GF_WOUND) || (effect == GF_BATTER))
	{
		/* Must do at least 70% of perfect */
		if (dam < total * 7 / 10) return (0);

		max = 1;
	}

	/* Standard attack */
	else
	{
		/* Weak blows rarely work */
		if ((rand_int(20) >= dam) || (!rand_int(3))) return (0);

		/* Must do at least 90% of perfect */
		if (dam < total * 9 / 10) return (0);
	}


	/* Perfect damage */
	if (dam == total) max++;

	/* Get bonus to critical damage (never greater than 6) */
	bonus = MIN(6, dam / 8);

	/* Critical damage  (never greater than 6 + max) */
	return (randint(bonus) + max);
}


/*
 * Monster scale
 *
 * Scale a levelling monster based on monster depth vs actual depth.
 */
bool monster_scale(monster_race *n_ptr, int m_idx, int depth)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	int d1, d2, scale;
	int i, boost;
	int n = 0;
	
	u32b flag[4];

	/* Paranoia */
	if ((r_ptr->flags9 & (RF9_LEVEL_MASK)) == 0) return (FALSE);

	/* Hack -- ensure distinct monster types */
	depth = depth - ((depth - r_ptr->level) % 5);

	/* Hack -- leader monsters */
	if (m_ptr->mflag & (MFLAG_LEADER)) depth += 5;
	
	/* Hack -- maximum power increase */
	if (depth > r_ptr->level + 15) depth = r_ptr->level + 15;

	/* This is the reverse of the algorithmic level
	   computation in eval_r_power in init1.c.
	   We apply this because monster power increases
	   much faster per level deeper in the dungeon.
	 */

	/* Compute effective power for monsters at this depth */
	d1 = (depth <= 40 ? depth :
			(depth <= 70 ? depth * 3 - 80 : 
			(depth <= 90 ? depth * 6 - 290
			: depth * 20 - 1550)));

	/* Compute effective power for monster's old depth */
	d2 = (r_ptr->level == 0 ? 1 : (r_ptr->level <= 40 ? r_ptr->level :
			(r_ptr->level <= 70 ? r_ptr->level * 3 - 80 : 
			(r_ptr->level <= 90 ? r_ptr->level * 6 - 290
			: r_ptr->level * 20 - 1550))));

	/* We only care about significant multipliers but scale up by multiple of 100 */
	scale = ((d1 * 100) / d2);

	/* Paranoia */
	if (scale <= 100) return (FALSE);

	/* Copy real monster to fake */
	COPY(n_ptr, r_ptr, monster_race);

	/* Set experience */
	n_ptr->mexp = r_ptr->mexp * scale / 100;

	/* Apply only one level flag */
	if (n_ptr->flags9 & (RF9_LEVEL_AGE)) flag[n++] = RF9_LEVEL_AGE;
	/* if (n_ptr->flags9 & (RF9_LEVEL_CLASS)) flag[n++] = RF9_LEVEL_CLASS;*/
	if (n_ptr->flags9 & (RF9_LEVEL_SPEED)) flag[n++] = RF9_LEVEL_SPEED;
	if (n_ptr->flags9 & (RF9_LEVEL_POWER)) flag[n++] = RF9_LEVEL_POWER;
	if (n_ptr->flags9 & (RF9_LEVEL_SIZE)) flag[n++] = RF9_LEVEL_SIZE;

	/* Clear all but one flag */
	if (n > 1)
	{
		/* Clear all flags */
		n_ptr->flags9 &= ~RF9_LEVEL_MASK;
		
		/* Add one back in based on m_idx */
		n_ptr->flags9 |= flag[m_idx % n];
	}
	
	/* Set level to depth */
	n_ptr->level = depth;

	/* Scale up for classes */
	if (n_ptr->flags9 & (RF9_LEVEL_CLASS))
	{
		int fake_m_idx = m_idx / n;
		bool add_ranged = FALSE;
		n = 0;
		
		if (!(fake_m_idx % 2) && (r_ptr->level <= depth - (++n * 5)))
		{
			n_ptr->flags2 |= (RF2_ARMOR);

			/* Increase effectiveness of some blows */
			for (i = 0; i < 4; i++)
			{
				if (!n_ptr->blow[i].d_side)
				{
					/* Give warrior some random ranged attack */
					n_ptr->blow[i].method = RBM_ARROW + (m_ptr->r_idx % 5);
					n_ptr->blow[i].effect = i > 0 ? n_ptr->blow[0].effect : GF_HURT;
					n_ptr->blow[i].d_dice = MAX(d1 < 50 ? d1 / 5 : d1 / (d1 / 10), 1);
					n_ptr->blow[i].d_side = d1 < 50 ? 5 : (d1 / 10);
					
					n_ptr->freq_innate = MAX(n_ptr->freq_innate, 25 + depth / 10);
					add_ranged = TRUE;
					
					break;
				}
				else if ((n_ptr->blow[i].method >= RBM_ARROW) && (n_ptr->blow[i].method < RBM_ARROW + 5))
				{
					n_ptr->blow[i].d_dice = MAX(d1 < 50 ? d1 / 5 : d1 / (d1 / 10), n_ptr->blow[i].d_dice);
					n_ptr->blow[i].d_side = MAX(d1 < 50 ? 5 : (d1 / 10), n_ptr->blow[i].d_side);
					break;
				}
				
				if (n_ptr->blow[i].effect == GF_HURT)
				{
					if ((m_idx % 11) < 4) n_ptr->blow[i].effect = GF_BATTER;
					else n_ptr->blow[i].effect = GF_WOUND;
				}
			}
			
			scale = scale * 4 / 5;
		}
		if (!(fake_m_idx % 3) && (r_ptr->level <= depth - (++n * 5)))
		{
			n_ptr->flags2 |= (RF2_PRIEST);
			
			/* Give priest some power */
			n_ptr->mana = MAX(n_ptr->mana, d1 / 10);
			n_ptr->spell_power = MAX(n_ptr->spell_power, depth / 4);
			n_ptr->freq_spell = MAX(n_ptr->freq_spell, 25 + depth / 10);
			
			/* Give priest some basic spells */
			if ((m_ptr->r_idx % 7) < (depth / 15)) n_ptr->flags6 |= (RF6_HEAL);
			if ((m_ptr->r_idx % 9) < (depth / 12)) n_ptr->flags6 |= (RF6_CURE);
			if ((m_ptr->r_idx % 11) < (depth / 10)) n_ptr->flags6 |= (RF6_BLESS);
			if ((m_ptr->r_idx % 13) < (depth / 8)) n_ptr->flags6 |= (RF6_BLINK);
			
			/* Pick a 'priestly' attack spell */
			switch(m_ptr->r_idx % 5)
			{
				case 0: n_ptr->flags6 |= (RF6_WOUND); break;
				case 1: n_ptr->flags6 |= (RF6_CURSE); break;
				case 2: n_ptr->flags5 |= (RF5_HOLY_ORB); break;
				case 3: n_ptr->flags5 |= (RF5_BOLT_ELEC); break;
				case 4: n_ptr->flags5 |= (RF5_BALL_LITE); break;
			}
			
			/* And maybe another */
			if (depth > 40) switch((m_ptr->r_idx * 7) % 5)
			{
				case 0: n_ptr->flags6 |= (RF6_WOUND); break;
				case 1: n_ptr->flags6 |= (RF6_CURSE); break;
				case 2: n_ptr->flags5 |= (RF5_HOLY_ORB); break;
				case 3: n_ptr->flags5 |= (RF5_BOLT_ELEC); break;
				case 4: n_ptr->flags5 |= (RF5_BALL_LITE); break;
			}
			
			scale = scale * 2 / 3;
		}
		if (!(fake_m_idx % 5) && (r_ptr->level <= depth - (++n * 5)))
		{
			n_ptr->flags2 |= (RF2_MAGE);
			
			/* Give priest some power */
			n_ptr->mana = MAX(n_ptr->mana, d1 / 12);
			n_ptr->spell_power = MAX(n_ptr->spell_power, depth / 3);
			n_ptr->freq_spell = MAX(n_ptr->freq_spell, 25 + depth / 10);
			
			/* Give mage some basic spells */
			if (((m_ptr->r_idx * 11) % 7) < (depth / 15)) n_ptr->flags6 |= (RF6_BLINK);
			if (((m_ptr->r_idx * 13) % 9) < (depth / 12)) n_ptr->flags6 |= (RF6_ADD_MANA);
			if (((m_ptr->r_idx * 17) % 11) < (depth / 10)) n_ptr->flags6 |= (RF6_TPORT);
			if (((m_ptr->r_idx * 19) % 13) < (depth / 8)) n_ptr->flags6 |= (RF6_SHIELD);
			
			/* Pick a 'magely' attack spell */
			switch((m_ptr->r_idx * 13) % 12)
			{
				case 0: if ((n_ptr->flags6 & (RF6_DARKNESS)) && (depth > 20)) { n_ptr->flags5 |= (RF5_BALL_DARK); break; }
				case 1: n_ptr->flags5 |= (RF5_BOLT_ACID); break;
				case 2: if ((n_ptr->flags6 & (RF6_CONF)) && (depth > 30)) { n_ptr->flags5 |= (RF5_BALL_CONFU); break; }
				case 3: n_ptr->flags5 |= (RF5_BOLT_COLD); break;
				case 4: if ((n_ptr->flags6 & (RF6_ILLUSION)) && (depth > 50)) { n_ptr->flags5 |= (RF5_BALL_CHAOS); break; }
				case 5: n_ptr->flags5 |= (RF5_BOLT_FIRE); break;
				case 6: if ((n_ptr->flags6 & (RF6_HOLD)) && (depth > 40)) { n_ptr->flags5 |= (RF5_BOLT_NETHR); break; }
				case 7: n_ptr->flags5 |= (RF5_BOLT_ELEC); break;
				case 8: if ((n_ptr->flags3 & (RF3_EVIL)) && (depth > 60)) { n_ptr->flags5 |= (RF5_BALL_NETHR); break; }
				case 9: n_ptr->flags5 |= (RF5_BOLT_POIS); break;
				case 10: if ((n_ptr->flags6 & (RF6_SLOW)) && (depth > 60)) { n_ptr->flags5 |= (RF5_BALL_SOUND); break; }
				case 11: n_ptr->flags5 |= (RF5_BOLT_MANA); break;
			}
			
			/* And another one */
			if (depth > 30) switch((m_ptr->r_idx * 17) % 12)
			{
				case 0: if ((n_ptr->flags5 & (RF5_BOLT_POIS))/* && (depth > 20) */) { n_ptr->flags5 |= (RF5_BALL_POIS); break; }
				case 1: n_ptr->flags5 |= (RF5_BOLT_ACID); break;
				case 2: if ((n_ptr->flags5 & (RF5_BOLT_ELEC))/* && (depth > 30) */) { n_ptr->flags5 |= (RF5_BALL_WIND); break; }
				case 3: n_ptr->flags5 |= (RF5_BOLT_COLD); break;
				case 4: if ((n_ptr->flags5 & (RF5_BOLT_ACID)) && (depth > 50)) { n_ptr->flags5 |= (RF5_BALL_ACID); break; }
				case 5: n_ptr->flags5 |= (RF5_BOLT_FIRE); break;
				case 6: if ((n_ptr->flags5 & (RF5_BOLT_FIRE)) && (depth > 40)) { n_ptr->flags5 |= (RF5_BALL_FIRE); break; }
				case 7: n_ptr->flags5 |= (RF5_BOLT_ELEC); break;
				case 8: if ((n_ptr->flags5 & (RF5_BOLT_COLD)) && (depth > 50)) { n_ptr->flags5 |= (RF5_BALL_COLD); break; }
				case 9: n_ptr->flags5 |= (RF5_BOLT_POIS); break;
				case 10: if ((n_ptr->flags5 & (RF5_BOLT_MANA)) && (depth > 60)) { n_ptr->flags5 |= (RF5_BALL_MANA); break; }
				case 11: n_ptr->flags5 |= (RF5_BOLT_MANA); break;
			}
			
			/* And maybe one more */
			if (depth > 50) switch((m_ptr->r_idx * 19) % 15)
			{
				case 0: n_ptr->flags5 |= (RF5_BALL_LITE); break;
				case 1: n_ptr->flags5 |= (RF5_BALL_DARK); break;
				case 2: n_ptr->flags5 |= (RF5_BALL_CONFU); break;
				case 3: n_ptr->flags5 |= (RF5_BALL_SOUND); break;
				case 4: n_ptr->flags5 |= (RF5_BALL_SHARD); break;
				case 5: n_ptr->flags5 |= (RF5_BALL_STORM); break;
				case 6: n_ptr->flags5 |= (RF5_BALL_NETHR); break;
				case 7: n_ptr->flags5 |= (RF5_BALL_WATER); break;
				case 8: n_ptr->flags5 |= (RF5_BALL_CHAOS); break;
				case 9: n_ptr->flags5 |= (RF5_BALL_ELEC); break;
				case 10: n_ptr->flags5 |= (RF5_BOLT_PLAS); break;
				case 11: n_ptr->flags5 |= (RF5_BOLT_ICE); break;
				case 12: n_ptr->flags5 |= (RF5_BEAM_ELEC); break;
				case 13: n_ptr->flags5 |= (RF5_BEAM_ICE); break;
				case 14: n_ptr->flags5 |= (RF5_ARC_FORCE); break;
			}

			scale = scale / 2;
		}
		if (!(fake_m_idx % 7) && (r_ptr->level <= depth - (++n * 5)))
		{
			n_ptr->flags2 |= (RF2_SNEAKY);

			/* Increase effectiveness of some blows */
			for (i = 0; i < 4; i++)
			{
				if ((!n_ptr->blow[i].d_side) && (!add_ranged))
				{
					/* Give thief some random ranged attack */
					n_ptr->blow[i].method = RBM_DAGGER + (m_ptr->r_idx % 3);
					n_ptr->blow[i].effect = i > 0 ? n_ptr->blow[0].effect : GF_HURT;
					n_ptr->blow[i].d_dice = MAX(d1 < 50 ? d1 / 5 : d1 / (d1 / 10), 1);
					n_ptr->blow[i].d_side = d1 < 50 ? 5 : (d1 / 10);
					
					n_ptr->freq_innate = MAX(n_ptr->freq_innate, 25 + depth / 10);
					add_ranged = TRUE;
				}
				else if ((!n_ptr->blow[i].d_side) && (i * 15 < depth))
				{
					n_ptr->blow[i].method = RBM_TOUCH;
					if (((m_idx + (i * 13)) % 11) < 4)
					{
						n_ptr->blow[i].effect = depth < 20 ? GF_EAT_GOLD : GF_EAT_ITEM;					
					}
					else n_ptr->blow[i].effect = GF_POIS;
					
					n_ptr->blow[i].d_dice = MAX(d1 < 50 ? d1 / 5 : d1 / (d1 / 10), 1);
					n_ptr->blow[i].d_side = d1 < 50 ? 5 : (d1 / 10);					
				}
				else if ((n_ptr->blow[i].method >= RBM_DAGGER) && (n_ptr->blow[i].method < RBM_DAGGER + 3))
				{
					n_ptr->blow[i].d_dice = MAX(d1 < 50 ? d1 / 5 : d1 / (d1 / 10), n_ptr->blow[i].d_dice);
					n_ptr->blow[i].d_side = MAX(d1 < 50 ? 5 : (d1 / 10), n_ptr->blow[i].d_side);
				}
			}
			
			/* Give thief some power */
			n_ptr->mana = MAX(n_ptr->mana, d1 / 20);
			n_ptr->spell_power = MAX(n_ptr->spell_power, depth / 6);
			n_ptr->freq_spell = MAX(n_ptr->freq_spell, 25 + depth / 10);
			
			/* Give thief some basic spells */
			if (((m_ptr->r_idx * 13) % 7) < (depth / 15)) n_ptr->flags6 |= (RF6_BLINK);
			if (((m_ptr->r_idx * 17) % 9) < (depth / 12)) n_ptr->flags6 |= (RF6_TPORT);
			if (((m_ptr->r_idx * 19) % 11) < (depth / 10)) n_ptr->flags6 |= (RF6_INVIS);
			
			scale = scale * 4 / 5;
		}
		
		if ((!(fake_m_idx % 11) && (r_ptr->level <= depth - (++n * 5))) || (!n))
		{
			n_ptr->flags2 |= (RF2_ARCHER);
			
			/* Increase effectiveness of some blows */
			for (i = 0; i < 4; i++)
			{
				if ((!n_ptr->blow[i].d_side) && ((i * 15 < depth) || !(add_ranged)))
				{
					/* Give thief some random ranged attack */
					n_ptr->blow[i].method = RBM_ARROW + (m_ptr->r_idx % 2);
					n_ptr->blow[i].effect = i > 0 ? n_ptr->blow[0].effect : GF_HURT;
					n_ptr->blow[i].d_dice = MAX(d1 < 50 ? d1 / 5 : d1 / (d1 / 10), 1);
					n_ptr->blow[i].d_side = d1 < 50 ? 5 : (d1 / 10);
					
					n_ptr->freq_innate = MAX(n_ptr->freq_innate, 25 + depth / 10);
					add_ranged = TRUE;
				}
				else if ((n_ptr->blow[i].method >= RBM_ARROW) && (n_ptr->blow[i].method < RBM_ARROW + 2))
				{
					n_ptr->blow[i].d_dice = MAX(d1 < 50 ? d1 / 5 : d1 / (d1 / 10), n_ptr->blow[i].d_dice);
					n_ptr->blow[i].d_side = MAX(d1 < 50 ? 5 : (d1 / 10), n_ptr->blow[i].d_side);
				}
			}
			
			scale = scale * 4 / 5;
		}
	}
	
	/* Recheck scale */
	if (scale < 100) return (TRUE);

	/* Scale up for speed */
	if (((n_ptr->flags9 & (RF9_LEVEL_SPEED)) != 0)
		|| (((n_ptr->flags9 & (RF9_LEVEL_CLASS)) != 0) && (r_ptr->flags2 & (RF2_SNEAKY | RF2_ARCHER))))
	{
		/* We rely on speed giving us an overall scale improvement */
		/* Note breeders are more dangerous on speed overall */

		/* Boost speed first */
		boost = scale / 100;

		/* Limit to +25 faster */
		if (boost > 25) boost = 25;

		/* Increase fake speed */
		n_ptr->speed += boost;

		/* Reduce scale by actual improvement in energy */
		scale = scale * extract_energy[r_ptr->speed] / extract_energy[n_ptr->speed];

		/* Fast breeders more dangerous so reduce speed improvement */
		if ((n_ptr->flags2 & (RF2_MULTIPLY)) != 0) n_ptr->speed -= boost / 2;

		/* Hack -- faster casters run out of mana faster */
		else if (n_ptr->mana && (boost > 10)) n_ptr->flags6 |= RF6_ADD_MANA;

	}
	
	/* Recheck scale */
	if (scale < 100) return (TRUE);

	/* Scale up for size */
	if (((n_ptr->flags9 & (RF9_LEVEL_SIZE)) != 0)
		|| (((n_ptr->flags9 & (RF9_LEVEL_CLASS)) != 0) && (r_ptr->flags2 & (RF2_ARMOR))))
	{
		/* Boost attack damage partially */
		if (scale > 133)
			for (i = 0; i < 4; i++)
		{
			if (!n_ptr->blow[i].d_side) continue;

			boost = (scale * n_ptr->blow[i].d_dice / 25) / (n_ptr->blow[i].d_side * 3);

			if (boost > 19 * n_ptr->blow[i].d_dice) boost = 19 * n_ptr->blow[i].d_dice;

			if (boost > 1) n_ptr->blow[i].d_dice += boost - 1;
		}

		/* Boost power slightly */
		if (scale > 133)
		{
			n_ptr->power = (r_ptr->power * scale * 3) / 4;
		}
	}

	/* Recheck scale */
	if (scale < 100) return (TRUE);

	/* Scale up for power */
	if (((n_ptr->flags9 & (RF9_LEVEL_POWER)) != 0)
		|| (((n_ptr->flags9 & (RF9_LEVEL_CLASS)) != 0) && (r_ptr->flags2 & (RF2_MAGE))))
	{
		/* Boost speed first */
		boost = scale / 400;

		/* Limit to +15 faster */
		if (boost > 15) boost = 15;

		/* Increase fake speed */
		n_ptr->speed += boost;

		/* Reduce scale by actual improvement in energy */
		scale = scale * extract_energy[r_ptr->speed] / extract_energy[n_ptr->speed];

		/* Boost attack damage partially */
		if (scale > 200)
			for (i = 0; i < 4; i++)
		{
			if (!n_ptr->blow[i].d_side) continue;

			boost = (scale * n_ptr->blow[i].d_dice / 50) / n_ptr->blow[i].d_side;

			if (boost > 9 * n_ptr->blow[i].d_dice) boost = 9 * n_ptr->blow[i].d_dice;

			if (boost > 1) n_ptr->blow[i].d_dice += boost - 1;
		}

		/* Boost power */
		if (scale > 100)
		{
			scale = (scale - 100) / 2 + 100; 
			n_ptr->power = (r_ptr->power * scale) / 100;
		}
	}

	/* Recheck scale */
	if (scale < 100) return (TRUE);

	/* Scale up for age or not done */
	if (scale > 100)
	{
		/* Boost armour class next */
		boost = r_ptr->ac * scale / 200;

		/* Limit armour class improvement */
		if (boost > 50 + n_ptr->ac / 3) boost = 50 + n_ptr->ac / 3;

		/* Improve armour class */
		n_ptr->ac += boost;

		/* Reduce scale by actual scaled improvement in armour class */
		scale = scale * r_ptr->ac / n_ptr->ac;

		/* Boost speed next */
		boost = scale / 200;

		/* Limit to +15 faster */
		if (boost > 15) boost = 15;

		/* Increase fake speed */
		n_ptr->speed += boost;

		/* Reduce scale by actual improvement in energy */
		scale = scale * extract_energy[r_ptr->speed] / extract_energy[n_ptr->speed];

		/* Boost attack damage partially */
		if (scale > 200)
			for (i = 0; i < 4; i++)
		{
			if (!n_ptr->blow[i].d_side) continue;

			boost = (scale * n_ptr->blow[i].d_dice / 50) / n_ptr->blow[i].d_side;

			if (boost > 9 * n_ptr->blow[i].d_dice) boost = 9 * n_ptr->blow[i].d_dice;

			if (boost > 1) n_ptr->blow[i].d_dice += boost - 1;
		}

		/* Boost power */
		if (scale > 100)
		{
			scale = (scale - 100) / 2 + 100; 
			n_ptr->power = (r_ptr->power * scale) / 100;

			/* Not done? */
			if (scale > 100)
			{
				/* Boost hit points -- unlimited */
				n_ptr->hside = n_ptr->hside * scale / 100;
			}
		}
	}

	return (TRUE);
}



/*
 * Determine if a monster attack against the player succeeds.
 * Always miss 5% of the time, Always hit 5% of the time.
 * Otherwise, match monster power against player armor.
 */
static bool check_hit(int power, int level, int who, bool ranged)
{
	int k, ac;
	int blocking = 0;

	/* Object  */
	object_type *o_ptr = &inventory[INVEN_ARM];

	/* Is player blocking? */
	if (p_ptr->blocking > 1)
	{
		/* Base blocking */
		blocking = p_ptr->to_h;

		/* No shield / secondary weapon */
		if (!o_ptr->k_idx)
		{
			if (inventory[INVEN_WIELD].k_idx)
			{
				o_ptr = &inventory[INVEN_WIELD];
			}
			else if (inventory[INVEN_HANDS].k_idx)
			{
				o_ptr = &inventory[INVEN_HANDS];
			}
		}

		/* Modify by object */
		if (o_ptr->k_idx)
		{
			/* Adjust by ac factor */
			blocking += o_ptr->ac + o_ptr->to_a;

			/* Adjust by to hit factor */
			blocking += o_ptr->to_h;
		}

		/* Modify by style */
		if (!p_ptr->heavy_wield)
		{
			int i;
			
			for (i = 0; i < z_info->w_max; i++)
			{
				if (w_info[i].class != p_ptr->pclass) continue;

				if (w_info[i].level > p_ptr->lev) continue;

				/* Check for styles */
				if (w_info[i].styles==0 
					|| w_info[i].styles & (p_ptr->cur_style & (WS_WIELD_FLAGS)) & (1L << p_ptr->pstyle))
				{
					switch (w_info[i].benefit)
					{
						case WB_HIT:
						case WB_AC:
							blocking += (p_ptr->lev - w_info[i].level) /2;
							break;
					}
				}
			}
		}
		
		/* Player condition */
		if ((p_ptr->blind) || (p_ptr->confused) || (p_ptr->image) || (p_ptr->shero))
			blocking /= 2;
	}

	/* Determine monster to hit chance */
	if (who > SOURCE_MONSTER_START)
	{
		monster_type *m_ptr = &m_list[who];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Monster never misses */
		if (r_ptr->flags9 & (RF9_NEVER_MISS)) return (TRUE);

		/* Calculate the "attack quality".  Blind monsters are greatly hindered. Stunned monsters are hindered. */
		power = (power + (m_ptr->blind ? level * 1 : (m_ptr->stunned ? level * 2 : level * 3)));

		/* Apply monster stats */
		if (m_ptr->mflag & (MFLAG_CLUMSY)) power -= 5;
		else if (m_ptr->mflag & (MFLAG_SKILLFUL)) power += 5;

		/* Apply temporary conditions */
		if (m_ptr->bless) power += 10;
		if (m_ptr->berserk) power += 24;

		/* Blind monsters almost always miss at ranged combat */
		if ((ranged) && (m_ptr->blind)) power /= 10;
	}
	else
	{
		/* Level counts for full amount */
		power += level * 3;
	}

	/* Percentile dice */
	k = rand_int(100);

	/* Hack -- Always miss or hit */
	if (k < 10) return (k < 5);

	/* Total armor */
	ac = p_ptr->ac + p_ptr->to_a;

	/* Modify for blocking */
	ac += blocking;

	/* Some items and effects count for more at range */
	if (ranged)
	{
		object_type *o_ptr = &inventory[INVEN_ARM];

		/* No secondary weapon or shield, use primary weapon */
		if (!o_ptr->k_idx) o_ptr = &inventory[INVEN_WIELD];

		/* Count for double */
		if (o_ptr->k_idx)
		{
			/* Shield or secondary weapon counts for double? */
			ac += o_ptr->ac;
			ac += o_ptr->to_a;
		}

		/* Shield counts for double */
		if (p_ptr->shield) ac += 50;
	}

	/* Some rooms make the player vulnerable */
	if (room_has_flag(p_ptr->py, p_ptr->px, ROOM_CURSED)) ac /= 2;

	/* Power and Level compete against Armor */
	if (power > 0)
	{
		/* Armour or blocking protects */
		if (randint(power) > ((ac * 3) / 4)) return (TRUE);

		/* Give blocking message to encourage blocking */
		if (randint(power) < (blocking)) msg_print("You block the attack.");
	}

	/* Assume miss */
	return (FALSE);
}






/*
 * Hack -- possible "insult" messages
 */
static cptr desc_insult[] =
{
	"insults you!",
	"insults your mother!",
	"gives you the finger!",
	"humiliates you!",
	"defiles you!",
	"dances around you!",
	"makes obscene gestures!",
	"moons you!!!"
};



/*
 * Hack -- possible "moan" messages
 */
static cptr desc_moan[] =
{
	"seems sad about something.",
	"asks if you have seen his dogs.",
	"tells you to get off his land.",
	"mumbles something about mushrooms."
};


/*
 * Hack -- possible "sing" messages
 */
static cptr desc_sing[] =
{
	"starts to read you a poem.",
	"sings a happy tune.",
	"picks a flower for you.",
	"skips in a circle."
};


/*
 * Describe attack
 */
void attack_desc(int who, int what, int target, int method, int damage, bool *do_cut, bool *do_stun)
{
	char m_name[80];
	char t_name[80];

	cptr prefix = "";
	cptr suffix = "";

	/* Describe source */
	if ((who >= SOURCE_MONSTER_START) || (who == SOURCE_PLAYER_ALLY))
	{
		/* Get the monster name (or "it") */
		monster_desc(m_name, sizeof(m_name), who > SOURCE_MONSTER_START ? who : what, 0x00);
	}
	else if (who <= SOURCE_PLAYER_START)
	{
		my_strcpy(m_name,"you", sizeof(m_name));
	}
	else
	{
		my_strcpy(m_name,"it", sizeof(m_name));
	}

	/* Describe target */
	if ((who == SOURCE_MONSTER_START) && (what > SOURCE_MONSTER_START))
	{
		/* Get the monster reflexive ("himself"/"herself"/"itself") */
		monster_desc(t_name, sizeof(t_name), who, 0x23);
	}
	else if (target > 0)
	{
		/* Get the monster name (or "it") */
		monster_desc(t_name, sizeof(t_name), target, 0x00);
	}
	else if (target < 0)
	{
		my_strcpy(t_name,"you", sizeof(t_name));
	}
	else
	{
		my_strcpy(t_name,"it", sizeof(t_name));
	}

	/* Describe the attack method */
	switch (method)
	{
		case RBM_HIT:
		{
			/* Handle special effect types */
			if (*do_cut)
			{
				if      (damage >= 30) prefix = "gouges ";
				else if (damage >= 20) prefix = "slashes ";
				else if (damage >= 5)  prefix = "cuts ";
				else                prefix = "scratches ";

				/* Usually don't stun */
				if (!rand_int(5)) *do_stun = TRUE;

				*do_cut = TRUE;
			}
			else if (*do_stun)
			{
				if      (damage >= 30) prefix = "bludgeons ";
					else if (damage >= 20) prefix = "batters ";
					else if (damage >= 5)  prefix = "bashes ";
					else                prefix = "hits ";

					/* Usually don't cut */
					if (!rand_int(5)) *do_cut = TRUE;

					*do_stun = TRUE;
			}
			else
			{
				prefix = "hits ";
				*do_cut = TRUE;
				*do_stun = TRUE;
			}

			break;
		}

		case RBM_TOUCH:
		{
			prefix = "touches ";
			break;
		}

		case RBM_PUNCH:
		{
			prefix = "punches ";
			*do_stun = TRUE;
			break;
		}

		case RBM_KICK:
		{
			prefix = "kicks ";
			*do_stun = TRUE;
			break;
		}

		case RBM_CLAW:
		{
			if      (damage >= 25) prefix = "slashes ";
			else if (damage >=  5) prefix = "claws ";
			else                prefix = "scratches ";
			*do_cut = TRUE;
			break;
		}

		case RBM_BITE:
		{
			if (damage >= 5) prefix = "bites ";
			else          prefix = "nips ";
			*do_cut = TRUE;
			break;
		}

		case RBM_PECK:
		{
			prefix = "pecks ";
			*do_stun  = TRUE;
			break;
		}

		case RBM_STING:
		{
			prefix = "stings ";
			break;
		}

		case RBM_VOMIT:
		{
			prefix = "vomits on ";
			break;
		}

		case RBM_BUTT:
		{
			if (damage >= rand_range(10, 20)) prefix = "tramples ";
			else                           prefix = "butts ";
			*do_stun  = TRUE;
			break;
		}

		case RBM_CRUSH:
		{
			if (damage >= 10) prefix = "crushes ";
			else           prefix = "squeezes ";
			*do_stun  = TRUE;
			break;
		}

		case RBM_ENGULF:
		{
			if (damage >= randint(50)) prefix = "envelops ";
			else                    prefix = "engulfs ";
			break;
		}

		case RBM_CRAWL:
		{
			prefix = "crawls on ";
			break;
		}

		case RBM_DROOL:
		{
			prefix = "drools on ";
			break;
		}

		case RBM_SLIME:
		{
			prefix = "slimes ";
			break;
		}

		case RBM_SPIT:
		{
			prefix = "spits on ";
			break;
		}

		case RBM_GAZE:
		{
			if      (damage >= rand_range(20, 30))
			{
				prefix = "glares at ";
				suffix = " terribly";
			}
			else if (damage >= rand_range(5, 30))
				prefix = "gazes upon ";
			else prefix = "gazes at ";
			break;
		}

		case RBM_WAIL:
		{
			prefix = "wails horribly";
			my_strcpy(t_name, "", sizeof(t_name));
			break;
		}

		case RBM_HOWL:
		{
			prefix = "howls";
			my_strcpy(t_name, "", sizeof(t_name));
			break;
		}

		case RBM_SHRIEK:
		{
			prefix = "shrieks horribly";
			my_strcpy(t_name, "", sizeof(t_name));
			break;
		}

		case RBM_SPORE:
		{
			prefix = "releases a cloud of spores";
			my_strcpy(t_name, "", sizeof(t_name));
			break;
		}

		case RBM_LASH:
		{
			prefix = "lashes ";
			suffix = " with a whip";
			break;
		}

		case RBM_BEG:
		{
			prefix = "begs ";
			suffix = " for money";
			break;
		}

		case RBM_INSULT:
		{
			if (target < 0) msg_format("%^s %s", m_name, desc_insult[rand_int(8)]);
			return;
		}

		case RBM_MOAN:
		{
			if (target < 0) msg_format("%^s %s", m_name, desc_moan[rand_int(4)]);
			return;
		}

		case RBM_SING:
		{
			if (target < 0) msg_format("%^s %s", m_name, desc_sing[rand_int(4)]);
			return;
		}

		case RBM_AURA:
		case RBM_AURA_MINOR:
		{
			prefix = "intensifies ";
			suffix = " aura";

			/* Get the monster possessive ("his"/"her"/"its") */
			if (who > SOURCE_MONSTER_START) monster_desc(t_name, sizeof(t_name), who, 0x22);
			else my_strcpy (t_name,"its", sizeof(t_name));

			break;
		}

		case RBM_TRAP:
		{
			prefix = "traps ";
			break;
		}

		case RBM_BOULDER:
		{
			prefix = "throws a boulder at ";
			break;
		}

		case RBM_BREATH:
		{
			prefix = "breathes at";
			break;
		}

		case RBM_EXPLODE:
		{
			prefix = "explodes";
			my_strcpy(t_name, "", sizeof(t_name));
			break;
		}

		case RBM_ARROW:
		{
			prefix = "shoots ";
			suffix = " with an arrow";
			break;
		}

		case RBM_XBOLT:
		{
			prefix = "shoots ";
			suffix = " with a crossbow bolt";
			break;
		}

		case RBM_SPIKE:
		{
			prefix = "shoots ";
			suffix = " with a spike";
			break;
		}

		case RBM_DART:
		{
			prefix = "throws a dart at ";
			break;
		}

		case RBM_SHOT:
		{
			prefix = "hurls a shot at ";
			break;
		}

		case RBM_FLASK:
		{
			prefix = "throws a fizzing flask at ";
			break;
		}

		case RBM_DAGGER:
		{
			prefix = "throws a dagger at ";
			break;
		}

		default:
		{
			prefix = "casts a spell at ";
		}
	}

	/* Message */
	if ((target < 0) && (method == RBM_SLIME))
	{
		msg_format("%^s %syou%s!", m_name, prefix, suffix);

		/* Slime player */
		if (f_info[cave_feat[p_ptr->py][p_ptr->px]].flags1 & (FF1_FLOOR))
			feat_near(p_ptr->py, p_ptr->px, FEAT_FLOOR_SLIME_T);
	}
	else if ((target < 0) && (damage > p_ptr->chp / 3))
	{
		msg_format("%^s %s%s%s!", m_name, prefix, t_name, suffix);

		/* Player loses blood */
		if (f_info[cave_feat[p_ptr->py][p_ptr->px]].flags1 & (FF1_FLOOR))
			feat_near(p_ptr->py, p_ptr->px, FEAT_FLOOR_BLOOD_T);
	}
	else /* XXX Have option to hide monster fighting messages */
	{
		msg_format("%^s %s%s%s.", m_name, prefix, t_name, suffix);
	}
}

/*
 * Get attack power based on effect.
 */
static int attack_power(int effect)
{
	int power = 0;

	/* Extract the attack "power". Elemental attacks upgraded. */
	switch (effect)
	{
		case GF_HURT: power = 60; break;
		case GF_WOUND: power = 60; break;
		case GF_BATTER: power = 60; break;
		case GF_SHATTER: power = 60; break;

		case GF_UN_BONUS:	power = 20; break;
		case GF_UN_POWER:	power = 15; break;
		case GF_LOSE_MANA: power = 45; break;
		case GF_EAT_GOLD:	power =  5; break;
		case GF_EAT_ITEM:	power =  5; break;
		case GF_EAT_FOOD:	power =  45; break;
		case GF_EAT_LITE:	power =  45; break;
		case GF_HUNGER: power = 45; break;

		case GF_POIS:	power =  25; break;
		case GF_ACID:		power = 50; break;
		case GF_ELEC:		power = 50; break;
		case GF_FIRE:		power = 50; break;
		case GF_COLD:		power = 50; break;

		case GF_BLIND:	power =  5; break;
		case GF_CONFUSION:	power = 10; break;
		case GF_TERRIFY:	power = 10; break;
		case GF_PARALYZE:	power =  5; break;
		case GF_HALLU:     power = 10; break;
		case GF_DISEASE:	power = 10; break;

		case GF_LOSE_STR:	power =  0; break;
		case GF_LOSE_DEX:	power =  0; break;
		case GF_LOSE_CON:	power =  0; break;
		case GF_LOSE_INT:	power =  0; break;
		case GF_LOSE_WIS:	power =  0; break;
		case GF_LOSE_CHR:	power =  0; break;
		case GF_LOSE_ALL:	power =  2; break;

		case GF_EXP_10:	power =  5; break;
		case GF_EXP_20:	power =  5; break;
		case GF_EXP_40:	power =  5; break;
		case GF_EXP_80:	power =  5; break;

		/* Need to add extra flavours in here */
	}
	return power;
}


/*
 * Determine if a monster attack against another monster succeeds.
 * Always miss 5% of the time, Always hit 5% of the time.
 * Otherwise, match monster power against monster armor.
 */
bool mon_check_hit(int m_idx, int effect, int level, int who, bool ranged)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_type *n_ptr = &m_list[who];
	monster_race *r_ptr = &r_info[n_ptr->r_idx];

	int k, ac;
	int power = attack_power(effect);
	
	/* Monster never misses */
	if (r_ptr->flags9 & (RF9_NEVER_MISS)) return (TRUE);

	/* Calculate the "attack quality".  Blind monsters are greatly hindered. Stunned monsters are hindered. */
	power = (power + (n_ptr->blind ? level * 1 : (n_ptr->stunned ? level * 2 : level * 3)));

	/* Apply monster stats */
	if (n_ptr->mflag & (MFLAG_CLUMSY)) power -= 5;
	else if (n_ptr->mflag & (MFLAG_SKILLFUL)) power += 5;

	/* Apply temporary conditions */
	if (n_ptr->bless) power += 10;
	if (n_ptr->berserk) power += 24;

	/* Blind monsters almost always miss at ranged combat */
	if ((ranged) && (n_ptr->blind)) power /= 10;

	/* Monsters can evade */
	if (mon_evade(m_idx, 
		(ranged ? 5 : 3) + (m_ptr->confused 
				 || m_ptr->stunned ? 1 : 3),
		9, "")) return (FALSE);

	/* Huge monsters are hard to hit. */
	if (r_info[m_ptr->r_idx].flags3 & (RF3_HUGE))
	{
		/* Easier for climbers, flyers and other huge monsters */
		if (((n_ptr->mflag & (MFLAG_OVER)) == 0)
			 && (r_ptr->flags3 & (RF3_HUGE)) &&
			 rand_int(100) < ((r_ptr->flags2 & (RF3_GIANT)) ? 35 : 70)) return (FALSE);
	}

	/* Hack -- make armoured monsters much more effective against monster arrows. */
	if ((ranged) && (r_info[m_ptr->r_idx].flags2 & (RF2_ARMOR)))
	{
		/* XXX Tune this to make warriors useful against archers. */
		if (rand_int(100) < 40) return (FALSE);
	}

	/* Percentile dice */
	k = rand_int(100);

	/* Hack -- Always miss or hit */
	if (k < 10) return (k < 5);

	/* Total armor */
	ac = calc_monster_ac(m_idx, ranged);
	
	/* Power and Level compete against Armor */
	if (power > 0)
	{
		/* Armour or blocking protects */
		if (randint(power) > ((ac * 3) / 4)) return (TRUE);
	}

	/* Assume miss */
	return (FALSE);
}


/*
 * Attack the player via physical attacks.
 * TODO: join with other (monster?) attack routines
 */
bool make_attack_normal(int m_idx)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	int ap_cnt;

	int tmp, ac, rlev;
	bool do_cut, do_stun, touched;

	char m_name[80];

	char ddesc[80];

	bool blinked = FALSE;

	bool shrieked = FALSE;

	/* Not allowed to attack */
	if (r_ptr->flags1 & (RF1_NEVER_BLOW)) return (FALSE);

	/* Player in stastis */
	if (p_ptr->stastis) return (FALSE);


	/* Total armor */
	ac = p_ptr->ac + p_ptr->to_a;

	/* Extract the effective monster level */
	rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);


	/* Get the monster name (or "it") */
	monster_desc(m_name, sizeof(m_name), m_idx, 0);

	/* Get the "died from" information (i.e. "a goblin") */
	monster_desc(ddesc, sizeof(ddesc), m_idx, 0x88);


	/* Scan through all four blows */
	for (ap_cnt = 0; ap_cnt < 4; ap_cnt++)
	{
		bool visible = FALSE;
		bool obvious = FALSE;

		int damage = 0;

		/* Extract the attack infomation */
		int effect = r_ptr->blow[ap_cnt].effect;
		int method = r_ptr->blow[ap_cnt].method;
		int d_dice = r_ptr->blow[ap_cnt].d_dice;
		int d_side = r_ptr->blow[ap_cnt].d_side;
	
		if (cheat_xtra) msg_format("base dice in this blow: dice %d, sides %d", d_dice, d_side);

		/* Hack -- no more attacks */
		if (!method) break;

		/* Handle "leaving" */
		if (p_ptr->leaving) break;

		/* Extract visibility (before blink) */
		if (m_ptr->ml) visible = TRUE;

		/* Skip 'tricky' attacks */
		if (method > RBM_MAX_NORMAL) continue;

		/* Assume no cut or stun or touched */
		do_cut = FALSE;
		do_stun = FALSE;
		touched = FALSE;

		if (cheat_xtra) msg_format("dice (2) in this blow: dice %d, sides %d", d_dice, d_side);

		/* Apply monster stats */
		if (d_side > 1)
		{
			/* Apply monster stats */
			if (m_ptr->mflag & (MFLAG_WEAK)) d_side = MIN(d_side - 2, d_side * 9 / 10);
			else if (m_ptr->mflag & (MFLAG_STRONG)) d_side = MAX(d_side + 2, d_side * 11 / 10);

			if (d_side <= 0) d_side = 1;
		}

		if (cheat_xtra) msg_format("dice (3) in this blow: dice %d, sides %d", d_dice, d_side);

		/* Roll out the damage */
		damage = damroll(d_dice, d_side);

		/* See if we notice attack */
		switch (method)
		{
			case RBM_HIT:
			case RBM_TOUCH:
			case RBM_PUNCH:
			case RBM_KICK:
			case RBM_CLAW:
			case RBM_BITE:
			case RBM_PECK:
			case RBM_STING:
			case RBM_VOMIT:
			case RBM_BUTT:
			case RBM_CRUSH:
			case RBM_ENGULF:
			case RBM_CRAWL:
			case RBM_DROOL:
			case RBM_LASH:
				touched = TRUE;
				break;
			case RBM_SHRIEK:
				shrieked = TRUE;
				break;
		}

		/* Monster hits player */
		if (!effect || check_hit(attack_power(effect), rlev, m_idx, FALSE))
		{
			/* Always disturbing */
			disturb(1, 1);

			/* Hack -- Apply "protection from evil" */
			if ((p_ptr->protevil > 0) &&
			    (r_ptr->flags3 & (RF3_EVIL)) &&
			    (p_ptr->lev >= rlev) &&
			    ((rand_int(100) + p_ptr->lev) > 50))
			{
				/* Remember the Evil-ness */
				if (m_ptr->ml)
				{
					l_ptr->flags3 |= (RF3_EVIL);
				}

				/* Message */
				msg_format("%^s is repelled.", m_name);

				/* Hack -- Next attack */
				continue;
			}

			/* Hack -- save passing effect for batter and wound */
			if (effect == GF_BATTER) do_stun = TRUE;
			else if (effect == GF_WOUND) do_cut = TRUE;

			/* Get the attack string */
			attack_desc(m_idx, ap_cnt, -1, method, damage, &do_cut, &do_stun);

			/* Check for usage */
			if (rand_int(100)<damage)
			{
				int slot;

				/* Pick a (possibly empty) inventory slot */
				switch (randint(6))
				{
					case 1: slot = INVEN_BODY; break;
					case 2: slot = INVEN_ARM; break;
					case 3: slot = INVEN_OUTER; break;
					case 4: slot = INVEN_HANDS; break;
					case 5: slot = INVEN_HEAD; break;
					default: slot = INVEN_FEET; break;
				}

				/* Object used? */
				object_usage(slot);
			}

			/* Player armor reduces total damage */
			damage -= (damage * ((p_ptr->ac + p_ptr->to_a < 150) ? p_ptr->ac + p_ptr->to_a: 150) / 250);

			if (cheat_xtra) msg_format("base damage dealt by monster in this blow: %d, dice %d, sides %d", damage, d_dice, d_side);

			if (effect)
			{
				/* New result routine */
				if (project_p(m_idx, ap_cnt, p_ptr->py, p_ptr->px, damage, effect))
				{
					obvious = TRUE;

					if ((effect == GF_EAT_ITEM) || (effect == GF_EAT_GOLD)) blinked = TRUE;
				}

				/* Apply teleport & other effects */
				if (project_t(m_idx, ap_cnt, p_ptr->py, p_ptr->px, damage, effect))
				{
					obvious = TRUE;

					if ((effect == GF_EAT_ITEM) || (effect == GF_EAT_GOLD)) blinked = TRUE;
				}

			}
			else
			{
				obvious = TRUE;
			}

			/* Hack -- only one of cut or stun */
			if (do_cut && do_stun)
			{
				/* Cancel cut */
				if (rand_int(100) < 50)
				{
					do_cut = FALSE;
				}

				/* Cancel stun */
				else
				{
					do_stun = FALSE;
				}
			}

			/* Handle cut */
			if (do_cut)
			{
				int k;

				/* Critical hit (zero if non-critical) */
				tmp = monster_critical(d_dice, d_side, damage, effect);

				/* Roll for damage */
				switch (tmp)
				{
					case 0: k = 0; break;
					case 1: k = randint(5); break;
					case 2: k = randint(5) + 5; break;
					case 3: k = randint(20) + 20; break;
					case 4: k = randint(50) + 50; break;
					case 5: k = randint(100) + 100; break;
					case 6: k = 300; break;
					default: k = 500; break;
				}

				/* Apply the cut */
				if (k) (void)set_cut(p_ptr->cut + k);
			}

			/* Handle stun */
			if (do_stun)
			{
				int k;

				/* Critical hit (zero if non-critical) */
				tmp = monster_critical(d_dice, d_side, damage, effect);

				/* Roll for damage */
				switch (tmp)
				{
					case 0: k = 0; break;
					case 1: k = randint(5); break;
					case 2: k = randint(8) + 8; break;
					case 3: k = randint(15) + 15; break;
					case 4: k = randint(25) + 25; break;
					case 5: k = randint(35) + 35; break;
					case 6: k = randint(45) + 45; break;
					default: k = 100; break;
				}

				/* Apply the stun */
				if (k) (void)set_stun(p_ptr->stun ? p_ptr->stun + randint(MIN(k,10)) : k);
			}
		}

		/* Monster missed player */
		else if (touched)
		{
			/* Visible monsters */
			if (m_ptr->ml)
			{
				/* Disturbing */
				disturb(1, 0);

				/* Message */
				if (!p_ptr->psleep) msg_format("%^s misses you.", m_name);
			}
		}


		/* Analyze "visible" monsters only */
		if (visible)
		{
			/* Count "obvious" attacks (and ones that cause damage) */
			if (obvious || damage || (l_ptr->blows[ap_cnt] > 10))
			{
				/* Count attacks of this type */
				if (l_ptr->blows[ap_cnt] < MAX_UCHAR)
				{
					l_ptr->blows[ap_cnt]++;
				}
			}
		}
	}

	/* Blink away */
	if (blinked)
	{
		/* Hack -- haste and flee with the loot */
		if ((!m_ptr->monfear) && (rand_int(3)))
		{
			/* Set monster fast */
			set_monster_haste(m_idx, 3 + rand_int(3), FALSE);

			/* Make monster 'panic' -- move away */
			set_monster_fear(m_ptr, m_ptr->hp / 3, TRUE);
		}
		else
		{
			msg_print("There is a puff of smoke!");
			teleport_hook = NULL;
			teleport_away(m_idx, MAX_SIGHT * 2 + 5);
		}
	}

	/* Shriek */
	if (shrieked)
	{
		aggravate_monsters(m_idx);
	}

	/* Always notice cause of death */
	if (p_ptr->is_dead && (l_ptr->deaths < MAX_SHORT))
	{
		l_ptr->deaths++;
	}


	/* Assume we attacked */
	return (TRUE);
}


/*
 * Using an input value for average damage, and another that controls
 * variability, return the actual base damage of a monster's attack
 * spell.  The larger the value for "control", the less likely the damage
 * will vary greatly.
 */
static int get_dam(byte power, int attack)
{
	int dam = 0;
	int spread;

	int control, av_dam;

	/* Determine mana cost */
	if (attack >= 224) return (FALSE);
	else if (attack >= 192)
	{
		av_dam = power * spell_info_RF7[attack-192][COL_SPELL_DAM_MULT];
		av_dam /= MAX(1, spell_info_RF7[attack-192][COL_SPELL_DAM_DIV]);
		control = spell_info_RF7[attack-192][COL_SPELL_DAM_VAR];
	}
	else if (attack >= 160)
	{
		av_dam = power * spell_info_RF6[attack-160][COL_SPELL_DAM_MULT];
		av_dam /= MAX(1, spell_info_RF6[attack-160][COL_SPELL_DAM_DIV]);
		control = spell_info_RF6[attack-160][COL_SPELL_DAM_VAR];
	}
	else if (attack >= 128)
	{
		av_dam = power * spell_info_RF5[attack-128][COL_SPELL_DAM_MULT];
		av_dam /= MAX(1, spell_info_RF5[attack-128][COL_SPELL_DAM_DIV]);
		control = spell_info_RF5[attack-128][COL_SPELL_DAM_VAR];
	}
	else if (attack >=  96)
	{
		av_dam = power * spell_info_RF4[attack- 96][COL_SPELL_DAM_MULT];
		av_dam /= MAX(1, spell_info_RF4[attack- 96][COL_SPELL_DAM_DIV]);
		control = spell_info_RF4[attack- 96][COL_SPELL_DAM_VAR];
	}
	else return (FALSE);

	/*Hack - handle Wound spell differently */
	if (attack == 160+20)
	{
		if (power > 75) av_dam = 225 + power - 75;
	}


	/*No point in going through this to return 0*/
	if (av_dam < 1) return (FALSE);

	/* Damage may never differ by more than 50% from the average */
	if (control < 4) control = 4;

	/*
	 * Get the allowable spread (two standard deviations, or 100,
	 * whichever is less).
	 */
	spread = MIN(100, av_dam * 2 / control);

	/* Loop until damage is within the allowable spread */
	while (TRUE)
	{
		/* Randomize damage (average, standard deviation) */
		dam = Rand_normal(av_dam, av_dam / control);

		/* Forbid too great a variation */
		if (dam > av_dam + spread) continue;
		if (dam < av_dam - spread) continue;

		/* Accept */
		break;
	}

	/* Return randomized damage */
	return (dam);
}


/*
 * Using an input value for average damage, and another that controls
 * variability, return the actual base damage of a monster's attack
 * spell.  The larger the value for "control", the less likely the damage
 * will vary greatly.
 */
int get_breath_dam(s16b hit_points, int gf_type, bool powerful)
{
	int dam, max_dam;

	switch (gf_type)
	{
		case GF_ACID:
		case GF_ELEC:
		case GF_FIRE:
		case GF_COLD:
		{
			dam = hit_points / 3;
			max_dam = 1600;
			break;
		}
		case GF_POIS:
		{
			dam = hit_points / 3;
			max_dam = 800;
			break;
		}
		case GF_PLASMA:
		{
			if (powerful)
			{
				dam = hit_points / 3;
				max_dam = 400;
			}
			else
			{
				dam = hit_points / 6;
				max_dam = 150;
			}
			break;
		}
		case GF_LITE:
		case GF_DARK:
		case GF_CONFUSION:
		case GF_TERRIFY:
		{
			dam = hit_points / 6;
			max_dam = 400;
			break;
		}
		case GF_WIND:
		case GF_SOUND:
		{
			if (powerful)
			{
				dam = hit_points / 4;
				max_dam = 500;
			}
			else
			{
				dam = hit_points / 9;
				max_dam = 150;
			}
			break;
		}
		case GF_SHARD:
		{
			dam = hit_points / 6;
			max_dam = 500;
			break;
		}
		case GF_INERTIA:
		{
			if (powerful)
			{
				dam = hit_points / 4;
				max_dam = 400;
			}
			else
			{
				dam = hit_points / 8;
				max_dam = 150;
			}
			break;
		}
		case GF_GRAVITY:
		{
			if (powerful)
			{
				dam = hit_points / 4;
				max_dam = 300;
			}
			else
			{
				dam = hit_points / 8;
				max_dam = 150;
			}
			break;
		}
		case GF_FORCE:
		{
			if (powerful)
			{
				dam = hit_points / 3;
				max_dam = 400;
			}
			else
			{
				dam = hit_points / 6;
				max_dam = 150;
			}
			break;
		}
		case GF_NEXUS:
		{
			dam = hit_points / 6;
			max_dam = 450;
			break;
		}
		case GF_HOLY_ORB:
		case GF_NETHER:
		{
			dam = hit_points / 6;
			max_dam = 550;
			break;
		}
		case GF_CHAOS:
		case GF_DISENCHANT:
		{
			dam = hit_points / 6;
			max_dam = 500;
			break;
		}
		case GF_DISEASE:
		case GF_TIME:
		{
			dam = hit_points / 3;
			if (powerful) max_dam = 400;
			else max_dam = 150;
			break;
		}
		case GF_MANA:
		{
			dam = hit_points / 3;
			if (powerful) max_dam = 400;
			else max_dam = 250;
			break;
		}

		/*Whoops!*/
		default: return (FALSE);
	}

	/* Don't exceed max damage */
	if (dam > max_dam) dam = max_dam;

	/* Return breath damage */
	return (dam);
}


#define FLG_MON_BEAM (PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | \
			PROJECT_PLAY | PROJECT_MAGIC)
#define FLG_MON_SHOT (PROJECT_STOP | PROJECT_KILL | PROJECT_PLAY | PROJECT_GRID)
#define FLG_MON_BALL_SHOT (PROJECT_BOOM | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | \
			PROJECT_PLAY | PROJECT_WALL)
#define FLG_MON_BOLT (PROJECT_STOP | PROJECT_KILL | PROJECT_PLAY | PROJECT_GRID | PROJECT_MAGIC)
#define FLG_MON_BALL (PROJECT_BOOM | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | \
			PROJECT_PLAY | PROJECT_WALL | PROJECT_MAGIC)
#define FLG_MON_AREA (PROJECT_BOOM | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | \
			PROJECT_PLAY | PROJECT_WALL | PROJECT_AREA | PROJECT_MAGIC)
#define FLG_MON_8WAY (PROJECT_8WAY | PROJECT_BOOM | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | \
			PROJECT_PLAY | PROJECT_WALL | PROJECT_AREA | PROJECT_MAGIC)
#define FLG_MON_CLOUD (PROJECT_BOOM | PROJECT_GRID | PROJECT_ITEM | PROJECT_PLAY | \
			PROJECT_HIDE | PROJECT_WALL | PROJECT_MAGIC)
#define FLG_MON_ARC  (PROJECT_ARC | PROJECT_BOOM | PROJECT_GRID | PROJECT_ITEM | \
	           PROJECT_KILL | PROJECT_PLAY | PROJECT_WALL | PROJECT_MAGIC)
#define FLG_MON_DIRECT (PROJECT_JUMP | PROJECT_KILL | PROJECT_PLAY | PROJECT_MAGIC)


/*
 * Cast a bolt at the target
 * Stop if we hit a monster
 * Affect monsters and the player
 */
static void mon_bolt(int who, int what, int y, int x, int typ, int dam, cptr result)
{
	/* Message */
	if (result) msg_print(result);

	if ((who > SOURCE_MONSTER_START) || (who == SOURCE_PLAYER_ALLY))
	{
		monster_type *m_ptr = &m_list[who > SOURCE_MONSTER_START ? who : what];
		int fy = m_ptr->fy;
		int fx = m_ptr->fx;

		/* Aim at target with a bolt attack */
		(void)project(who, what, 0, fy, fx, y, x, dam, typ, FLG_MON_BOLT, 0 , 0);
	}
	else
	{
		/* Affect single grid with a bolt attack */
		(void)project(who, what, 0, y, x, y, x, dam, typ, FLG_MON_BOLT, 0 , 0);
	}
}


/*
 * Cast a bolt at the target
 * Stop if we hit a monster
 * Affect monsters and the player
 * Can miss the first target
 * Not treated as magic
 */
static void mon_shot(int who, int what, int y, int x, int typ, int dam, bool hit, cptr result)
{
	/* Message */
	if (result) msg_print(result);

	if ((who > SOURCE_MONSTER_START) || (who == SOURCE_PLAYER_ALLY))
	{
		monster_type *m_ptr = &m_list[who > SOURCE_MONSTER_START ? who : what];
		int fy = m_ptr->fy;
		int fx = m_ptr->fx;

		/* Aim at target with a bolt attack */
		(void)project(who, what, 0, fy, fx, y, x, dam, typ, FLG_MON_SHOT | (hit ? 0L : PROJECT_MISS), 0 , 0);
	}
	else
	{
		/* Affect single grid with a bolt attack */
		(void)project(who, what, 0, y, x, y, x, dam, typ, FLG_MON_BOLT, 0 , 0);
	}
}


/*
 * Cast a beam at the target, sometimes with limited range.
 * Do not stop if we hit a monster
 * Affect grids, monsters, and the player
 */
static void mon_beam(int who, int what, int y, int x, int typ, int dam, int range, cptr result)
{
	/* Message */
	if (result) msg_print(result);

	if ((who > SOURCE_MONSTER_START) || (who == SOURCE_PLAYER_ALLY))
	{
		monster_type *m_ptr = &m_list[who > SOURCE_MONSTER_START ? who : what];
		int fy = m_ptr->fy;
		int fx = m_ptr->fx;

		/* Aim at target with a beam attack */
		(void)project(who, what, range, fy, fx, y, x, dam, typ, FLG_MON_BEAM,0 ,0);
	}
	else
	{
		/* Affect single grid with a beam attack */
		(void)project(who, what, range, y, x, y, x, dam, typ, FLG_MON_BEAM,0 ,0);
	}
}



/*
 * Cast an 8 way spell at the target
 * Pass over any monsters that may be in the way
 * Affect grids, objects, monsters, and (specifically) the player
 */
static void mon_8way(int who, int what, int y, int x, int typ, int dam, int rad, cptr result)
{
	/* Message */
	if (result) msg_print(result);

	if ((who > SOURCE_MONSTER_START) || (who == SOURCE_PLAYER_ALLY))
	{
		monster_type *m_ptr = &m_list[who > SOURCE_MONSTER_START ? who : what];
		int fy = m_ptr->fy;
		int fx = m_ptr->fx;

		/* Aim at target with an 8-way attack */
		(void)project(who, what, rad, fy, fx, y, x, dam, typ, FLG_MON_8WAY, 0, 0);
	}
	else
	{
		/* Affect grids in radius with a ball attack */
		(void)project(who, what, rad, y, x, y, x, dam, typ, FLG_MON_8WAY, 0, 0);
	}
}


/*
 * Cast a ball spell at the target
 * Pass over any monsters that may be in the way
 * Affect grids, objects, monsters, and (specifically) the player
 * Can miss the first target
 * Damage consistent over distance
 */
static void mon_area(int who, int what, int y, int x, int typ, int dam, int rad, cptr result)
{
	/* Message */
	if (result) msg_print(result);

	if ((who > SOURCE_MONSTER_START) || (who == SOURCE_PLAYER_ALLY))
	{
		monster_type *m_ptr = &m_list[who > SOURCE_MONSTER_START ? who : what];
		int fy = m_ptr->fy;
		int fx = m_ptr->fx;

		/* Aim at target with a ball attack */
		(void)project(who, what, rad, fy, fx, y, x, dam, typ, FLG_MON_AREA, 0, 0);
	}
	else
	{
		/* Affect grids in radius with a ball attack */
		(void)project(who, what, rad, y, x, y, x, dam, typ, FLG_MON_AREA, 0, 0);
	}
}


/*
 * Cast a ball shot at the target
 * Affect grids, objects, monsters, and (specifically) the player
 * Not treated as magic
 * Stop at first target
 */
static void mon_ball_minor_shot(int who, int what, int y, int x, int typ, int dam, int rad, bool hit, cptr result)
{
	/* Message */
	if (result) msg_print(result);

	if ((who > SOURCE_MONSTER_START) || (who == SOURCE_PLAYER_ALLY))
	{
		monster_type *m_ptr = &m_list[who > SOURCE_MONSTER_START ? who : what];
		int fy = m_ptr->fy;
		int fx = m_ptr->fx;

		/* Aim at target with a ball attack */
		(void)project(who, what, rad, fy, fx, y, x, dam, typ, FLG_MON_BALL_SHOT | PROJECT_STOP | (hit ? 0L : PROJECT_MISS), 0, 0);
	}
	else
	{
		/* Affect grids in radius with a ball attack */
		(void)project(who, what, rad, y, x, y, x, dam, typ, FLG_MON_BALL_SHOT, 0, 0);
	}
}


/*
 * Cast a ball spell at the target
 * Affect grids, objects, monsters, and (specifically) the player
 * Stop at first target
 */
static void mon_ball_minor(int who, int what, int y, int x, int typ, int dam, int rad, bool hit, cptr result)
{
	/* Message */
	if (result) msg_print(result);

	if ((who > SOURCE_MONSTER_START) || (who == SOURCE_PLAYER_ALLY))
	{
		monster_type *m_ptr = &m_list[who > SOURCE_MONSTER_START ? who : what];
		int fy = m_ptr->fy;
		int fx = m_ptr->fx;

		/* Aim at target with a ball attack */
		(void)project(who, what, rad, fy, fx, y, x, dam, typ, FLG_MON_BALL | PROJECT_STOP | (hit ? 0L : PROJECT_MISS), 0, 0);
	}
	else
	{
		/* Affect grids in radius with a ball attack */
		(void)project(who, what, rad, y, x, y, x, dam, typ, FLG_MON_BALL, 0, 0);
	}
}


/*
 * Cast a ball spell at the target
 * Pass over any monsters that may be in the way
 * Affect grids, objects, monsters, and (specifically) the player
 * Can miss the first target
 */
static void mon_ball(int who, int what, int y, int x, int typ, int dam, int rad, bool hit, cptr result)
{
	/* Message */
	if (result) msg_print(result);

	if ((who > SOURCE_MONSTER_START) || (who == SOURCE_PLAYER_ALLY))
	{
		monster_type *m_ptr = &m_list[who > SOURCE_MONSTER_START ? who : what];
		int fy = m_ptr->fy;
		int fx = m_ptr->fx;

		/* Aim at target with a ball attack */
		(void)project(who, what, rad, fy, fx, y, x, dam, typ, FLG_MON_BALL | (hit ? 0L : PROJECT_MISS), 0, 0);
	}
	else
	{
		/* Affect grids in radius with a ball attack */
		(void)project(who, what, rad, y, x, y, x, dam, typ, FLG_MON_BALL, 0, 0);
	}
}


/*
 * Release a cloud, which is a ball centered on the monster that does not
 * affect other monsters (mostly to avoid annoying messages).
 *
 * Consider being less graphics-intensive.
 */
void mon_cloud(int who, int what, int y, int x, int typ, int dam, int rad, cptr result)
{
	/* Message */
	if (result) msg_print(result);

	if ((who > SOURCE_MONSTER_START) || (who == SOURCE_PLAYER_ALLY))
	{
		monster_type *m_ptr = &m_list[who > SOURCE_MONSTER_START ? who : what];
		int fy = m_ptr->fy;
		int fx = m_ptr->fx;

		/* Surround the target with a cloud */
		(void)project(who, what, rad, fy, fx, fy, fx, dam, typ, FLG_MON_CLOUD, 0, 0);
	}
	else
	{
		/* Affect grids in radius with a cloud */
		(void)project(who, what, rad, y, x, y, x, dam, typ, FLG_MON_CLOUD, 0, 0);
	}
}



/*
 * Breathe or cast an arc-shaped spell at the player.
 * Use an arc spell of specified range and width.
 * Affect grids, objects, monsters, and (specifically) the player
 *
 * Monster breaths do not lose strength with distance at the same rate
 * that normal arc spells do.  If the monster is "powerful", they lose
 * less strength; otherwise, they lose more.
 */
static void mon_arc(int who, int what, int y, int x, int typ, int dam, int rad, int degrees_of_arc, cptr result)
{
	/* Message */
	if (result) msg_print(result);

	/* Arc if a monster */
	if ((who > SOURCE_MONSTER_START) || (who == SOURCE_PLAYER_ALLY))
	{
		monster_type *m_ptr = &m_list[who > SOURCE_MONSTER_START ? who : what];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		int fy = m_ptr->fy;
		int fx = m_ptr->fx;

		/* Diameter of source of energy is at least 20. */
		int diameter_of_source = 20;

		/* XXX XXX -- POWERFUL monster breaths lose less damage with range. */
		int degree_factor = (r_ptr->flags2 & (RF2_POWERFUL)) ? 120 : 60;

		/* Narrow arcs lose relatively little energy over distance. */
		if (degrees_of_arc < degree_factor)
		{
			if (degrees_of_arc <= 6) diameter_of_source = rad * 10;
			else diameter_of_source = diameter_of_source * degree_factor /
				degrees_of_arc;
		}

		/* Radius of zero means no fixed limit. */
		if (rad == 0) rad = MAX_SIGHT;

		/* Target the player with an arc-shaped attack. */
		(void)project(who, what, rad, fy, fx, y, x, dam, typ, FLG_MON_ARC, degrees_of_arc,
			(byte)diameter_of_source);
	}
	/* Ball if not a monster */
	else
	{
		/* Affect grids in radius with a ball */
		(void)project(who, what, rad, y, x, y, x, dam, typ, FLG_MON_BALL, 0, 0);
	}
}



/*
 * Monster attempts to make a ranged melee attack.
 * TODO: join with other (monster?) attack routines
 *
 * Use by aura and trail effects and eating part
 * of the monster.
 */
void mon_blow_ranged(int who, int what, int x, int y, int method, int range, int flg, cptr result)
{
	monster_lore *l_ptr;
	monster_race *r_ptr;

	int fy, fx;

	bool obvious, known;

	int i;
			
	if (who > SOURCE_MONSTER_START)
	{
		monster_type *m_ptr = &m_list[who];
		r_ptr = &r_info[m_ptr->r_idx];
		l_ptr = &l_list[m_ptr->r_idx];

		known = m_ptr->ml;

		fy = m_ptr->fy;
		fx = m_ptr->fx;
	}
	else if ((who == SOURCE_SELF) || (who == SOURCE_PLAYER_ALLY))
	{
		monster_type *m_ptr = &m_list[what];
		r_ptr = &r_info[m_ptr->r_idx];
		l_ptr = &l_list[m_ptr->r_idx];

		known = m_ptr->ml;

		fy = m_ptr->fy;
		fx = m_ptr->fx;
	}	
	else if (who == SOURCE_PLAYER_EAT_MONSTER)
	{
		r_ptr = &r_info[what];
		l_ptr = &l_list[what];

		fy = p_ptr->py;
		fx = p_ptr->px;
		
		known = TRUE;
	}
	else
	{
		return;
	}

	/* Scan through all four blows */
	for (i = 0; i < 4; i++)
	{
		/* End of attacks */
		if (!(r_ptr->blow[i].method)) break;

		/* Skip if not spores */
		if (r_ptr->blow[i].method != method) continue;

		/* Message */
		if (result) msg_print(result);

		/* Target the player with a ranged attack */
		obvious = project(who, what, range, fy, fx, y, x, damroll(r_ptr->blow[i].d_side,
					r_ptr->blow[i].d_dice),r_ptr->blow[i].effect, flg, 0, 0);

		/* Analyze "visible" monsters only */
		if (known)
		{
			/* Count "obvious" attacks */
			if (obvious || (l_ptr->blows[i] > 10))
			{
				/* Count attacks of this type */
				if (l_ptr->blows[i] < MAX_UCHAR)
				{
					l_ptr->blows[i]++;
				}
			}
		}
	}
}


/*
 * Monsters can concentrate light or conjure up darkness.
 * 
 * Also, druidic type monsters use water and trees to boost
 * their recovery of hit points and mana.
 *
 * Return TRUE if the monster did anything.
 * 
 * Note we force 'destroy' to FALSE to keep druidic monsters
 * more interesting. This means they don't destroy the
 * resources that they concentrate - forcing the player to
 * consider how to do so.
 */
static int mon_concentrate_power(int y, int x, int spower, bool use_los, bool destroy, bool concentrate_hook(const int y, const int x, const bool modify))
{
	int damage, radius, lit_grids;
	
	/* Radius of darkness-creation varies depending on spower */
	radius = MIN(6, 3 + spower / 20);

	/* Check to see how much we would gain (use a radius of 6) */
	lit_grids = concentrate_power(y, x, 6,
		FALSE, use_los, concentrate_hook);

	/* We have enough juice to make it worthwhile (make a hasty guess) */
	if (lit_grids >= rand_range(40, 60))
	{
		/* Actually concentrate the light */
		(void)concentrate_power(y, x, radius,
			destroy, use_los, concentrate_hook);

		/* Calculate damage (60 grids => break-even point) */
		damage = lit_grids * spower / 20;

		/* Limit damage, but allow fairly high values */
		if (damage > 9 * spower / 2) damage = 9 * spower / 2;

		/* We did something */
		return (damage);
	}

	/* We decided not to do anything */
	return (0);
}




/*
 * Monster attempts to make a ranged (non-melee) attack.
 * TODO: join with other (monster?) attack routines
 *
 * Determine if monster can attack at range, then see if it will.  Use
 * the helper function "choose_attack_spell()" to pick a physical ranged
 * attack, magic spell, or summon.  Execute the attack chosen.  Process
 * its effects, and update character knowledge of the monster.
 *
 * Perhaps monsters should breathe at locations *near* the player,
 * since this would allow them to inflict "partial" damage.
 * 
 * XXX We currently only call this routine with the following values of
 * 'who'.
 * 
 * who > SOURCE_MONSTER_START: 	source is monster
 * who == SOURCE_FEATURE:		source is feature
 * who == SOURCE_PLAYER_TRAP:	source is player trap
 */
bool make_attack_ranged(int who, int attack, int y, int x)
{
	int k, rlev, spower, rad, what;

	monster_type *m_ptr, *n_ptr;
	monster_race *r_ptr, *s_ptr;
	monster_lore *l_ptr, *k_ptr;

	char m_name[80];
	char m_poss[80];

	char t_name[80];
	char t_poss[80];
	char t_nref[80]; /* Not reflexive if required */

	cptr result = NULL;

	int target = cave_m_idx[y][x];

	/* Summoner */
	int summoner = 0;
	
	/* Summon count */
	int count = 0;

	/* Summon level */
	int summon_lev = 0;

	/* Is the player blind */
	bool blind = (p_ptr->blind ? TRUE : FALSE);

	bool seen;	/* Source seen */
	bool known;	/* Either source or target seen */
	bool powerful;
	bool normal;
	bool direct;
	
	u32b allies = 0L;

	/* Some summons override cave ecology */
	bool old_cave_ecology = cave_ecology.ready;

	/* Hack -- don't summon on surface */
	bool surface = p_ptr->depth == min_depth(p_ptr->dungeon);

	/* Hack -- Confused monsters get a random target */
	if ((who > SOURCE_MONSTER_START) && (target != who) && (m_list[who].confused) && (m_list[who].confused > rand_int(33)))
	{
		int dir = randint(8);
		int path_n;
		u16b path_g[256];
		int i, ty, tx;

		/* Get the monster */
		m_ptr = &m_list[who];

		/* Predict the "target" location */
		ty = m_ptr->fy + 99 * ddy[dir];
		tx = m_ptr->fx + 99 * ddx[dir];

		/* Calculate the path */
		path_n = project_path(path_g, MAX_SIGHT, m_ptr->fy, m_ptr->fx, &ty, &tx, 0);

		/* Hack -- Handle stuff */
		handle_stuff();

		/* Hack -- default is to start at monster and go in random dir */
		y = m_ptr->fy + ddy[dir];
		x = m_ptr->fx + ddx[dir];

		/* Project along the path */
		for (i = 0; i < path_n; ++i)
		{
			int ny = GRID_Y(path_g[i]);
			int nx = GRID_X(path_g[i]);

			/* Hack -- Stop before hitting walls */
			if (!cave_project_bold(ny, nx)) break;

			/* Advance */
			x = nx;
			y = ny;

			/* Handle monster */
			if (cave_m_idx[y][x]) break;
		}
	}

	/* Describe target - target is itself */
	if ((who > 0) && (who == target))
	{
		n_ptr = &m_list[who];
		s_ptr = &r_info[n_ptr->r_idx];
		k_ptr = &l_list[who];

		/* Get the monster name (or "it") */
		monster_desc(t_nref, sizeof(t_name), target, 0x00);

		/* Get the monster reflexive ("himself"/"herself"/"itself") */
		monster_desc(t_name, sizeof(t_nref), who, 0x23);

		/* Get the monster possessive ("his"/"her"/"its") */
		monster_desc(t_poss, sizeof(t_poss), who, 0x22);
	}
	/* Describe target - target is another monster */
	else if (target > 0)
	{
		n_ptr = &m_list[target];
		s_ptr = &r_info[n_ptr->r_idx];
		k_ptr = &l_list[target];

		/* Get the monster name (or "it") */
		monster_desc(t_name, sizeof(t_name), target, 0x00);

		/* Get the monster name (or "it") */
		monster_desc(t_nref, sizeof(t_nref), target, 0x00);

		/* Get the monster possessive ("the goblin's") */
		monster_desc(t_poss, sizeof(t_poss), target, 0x02);
	}
	/* Describe target - target is the player */
	else if (target < 0)
	{
		n_ptr = &m_list[0];
		s_ptr = &r_info[0];
		k_ptr = &l_list[0];

		my_strcpy(t_name,"you", sizeof(t_name));
		my_strcpy(t_nref,"you", sizeof(t_nref));
		my_strcpy(t_poss,"your", sizeof(t_poss));
	}
	/* Describe target - target is an empty grid/feature */
	else
	{
		n_ptr = &m_list[0];
		s_ptr = &r_info[0];
		k_ptr = &l_list[0];

		my_strcpy(t_name,f_name + f_info[cave_feat[y][x]].name, sizeof(t_name));
		my_strcpy(t_nref,f_name + f_info[cave_feat[y][x]].name, sizeof(t_nref));
		my_strcpy(t_poss,"the ", sizeof(t_poss));
		my_strcat(t_poss,f_name + f_info[cave_feat[y][x]].name, sizeof(t_poss));
		my_strcat(t_poss,"'s",sizeof(t_poss));
	}
		
	/* Describe caster - player traps and features only */
	if (who < SOURCE_MONSTER_START)
	{
		m_ptr = &m_list[0];
		l_ptr = &l_list[0];
		r_ptr = &r_info[0];

		what = cave_feat[y][x];
		
		/* Feature is seen */
		if (play_info[y][x] & (PLAY_SEEN))
		{
			my_strcpy(m_name,f_name + f_info[what].name, sizeof(m_name));
			my_strcpy(m_poss,"the ", sizeof(m_poss));
			my_strcpy(m_poss,f_name + f_info[what].name, sizeof(m_poss));
			my_strcat(m_poss,"'s",sizeof(t_poss));
		}
		else
		{
			my_strcpy(m_name,"it", sizeof(m_name));
			my_strcpy(m_poss,"its", sizeof(m_poss));
		}
		
		/* Hack -- Message text depends on feature description */
		seen = FALSE;

		/* Assume "normal" target */
		normal = (target < 0);

		/* Assume "projectable" */
		direct = TRUE;

		/* Check if known */
		known = player_can_see_bold(y,x);

		/* Fake the monster level */
		rlev = f_info[cave_feat[y][x]].power;

		/* Fake the spell power */
		spower = 2 + p_ptr->depth / 10;

		/* Fake the powerfulness */
		powerful = (p_ptr->depth > 50 ? TRUE : FALSE);

		/* No summoner */
		summoner = 0;
		
		/* Fake the summoning level */
		summon_lev = p_ptr->depth + 3;
	}
	else
	{
		/* Monsters can fail spells and run out of mana*/
		int failrate, manacost, ammo;

		m_ptr = &m_list[who];
		l_ptr = &l_list[m_ptr->r_idx];
		r_ptr = &r_info[m_ptr->r_idx];

		/* Hack -- never make spell attacks if hidden */
		if (m_ptr->mflag & (MFLAG_HIDE)) return (FALSE);

		/* Get the monster name (or "it") */
		monster_desc(m_name, sizeof(m_name), who, 0x00);

		/* Get the monster possessive ("his"/"her"/"its") */
		monster_desc(m_poss, sizeof(m_poss), who, 0x22);

		/* Extract the "see-able-ness" */
		seen = (!blind && m_ptr->ml);

		/* Extract the monster level.  Must be at least 1. */
		rlev = MAX(1, r_ptr->level);

		/* Extract the powerfulness */
		powerful = (r_ptr->flags2 & (RF2_POWERFUL) ? TRUE : FALSE);
		
		/* Extract the summoner */
		summoner = m_list[who].r_idx;

		/* Extract the summoning level.  Must be at least 1. */
		summon_lev = MAX(1, (r_ptr->level + p_ptr->depth) / 2 - 1);

		/* Determine mana cost */
		if (attack >= 224) return (FALSE);
		else if (attack >= 192) manacost = spell_info_RF7[attack-192][COL_SPELL_MANA_COST];
		else if (attack >= 160) manacost = spell_info_RF6[attack-160][COL_SPELL_MANA_COST];
		else if (attack >= 128) manacost = spell_info_RF5[attack-128][COL_SPELL_MANA_COST];
		else if (attack >=  96) manacost = spell_info_RF4[attack- 96][COL_SPELL_MANA_COST];
		else return (FALSE);		/* Spend the mana */

		m_ptr->mana -= manacost;

		/* Use ammunition */
		ammo = find_monster_ammo(who, attack - 96, FALSE);

		/* Reduce ammunition */
		if (ammo > 0)
		{
			floor_item_increase(ammo, -1);
			floor_item_optimize(ammo);
		}
		/* Out of ammunition */
		else if (ammo < 0)
		{
			/* Message -- only stupid monsters get this and not really applicable to them*/
			/*if (m_ptr->ml) msg_format("%^s is out of ammunition.", m_name);*/

			return (TRUE);
		}

		/* Calculate spell failure rate */
		failrate = 25 - (rlev + 3) / 4;

		/* Stunned monsters always have a chance to fail */
		if ((m_ptr->stunned) && (failrate < 10)) failrate = 10;

		/* Hack -- Stupid monsters will never fail (for jellies and such) */
		if (r_ptr->flags2 & (RF2_STUPID)) failrate = 0;

		/* Apply monster intelligence stat */
		if (m_ptr->mflag & (MFLAG_STUPID)) failrate = failrate * 3 / 2;
		else if (m_ptr->mflag & (MFLAG_SMART)) failrate /= 2;

		/* Check for spell failure (breath/shot attacks never fail) */
		if ((attack >= 128) && (rand_int(100) < failrate) && ((m_ptr->mflag != (MFLAG_ALLY)) == 0))
		{
			/* Message */
			msg_format("%^s tries to cast a spell, but fails.", m_name);

			return (TRUE);
		}

		/* Extract the monster's spell power.  Must be at least 1. */
		spower = MAX(1, r_ptr->spell_power);

		/* Monster has cast a spell */
		m_ptr->mflag &= ~(MFLAG_CAST | MFLAG_SHOT | MFLAG_BREATH);

		if (target > 0)
		{
			known = ((m_ptr->ml || n_ptr->ml));

			/* Not "normal" target */
			normal = FALSE;

			/* Check "projectable" */
			direct = ((projectable(m_ptr->fy, m_ptr->fx, y, x, 0)) != PROJECT_NO);

		}
		else if (target < 0)
		{
			/* Always known if target */
			known = TRUE;

			/* Assume "normal" target */
			normal = TRUE;

			/* Check "projectable" */
			direct = ((projectable(m_ptr->fy, m_ptr->fx, y, x, 0)) != PROJECT_NO);

		}
		else
		{
			known = (m_ptr->ml && player_can_see_bold(y,x));

			/* Assume "normal" target */
			normal = TRUE;

			/* Check "projectable" */
			direct = ((projectable(m_ptr->fy, m_ptr->fx, y, x, 0)) != PROJECT_NO);
		}

		/* Attacking itself */
		if (who == target)
		{
			what = who;
			who = SOURCE_SELF;
		}

		/* Allies */
		else if (m_ptr->mflag & (MFLAG_ALLY))
		{
			what = who;
			who = SOURCE_PLAYER_ALLY;
		}
		
		/* Notice attack */
		else
		{
			what = attack;
		}
	}


	/*** Execute the ranged attack chosen. ***/
	switch (attack)
	{

		/* RF4_BLOW_1 */
		/* RF4_BLOW_2 */
		/* RF4_BLOW_3 */
		/* RF4_BLOW_4 */
		case 96+0:
		case 96+1:
		case 96+2:
		case 96+3:
		{
			int method, effect, d_dice, d_side;
			int dam = 0, power = 0;

			bool do_cut = FALSE;
			bool do_stun = FALSE;
			bool hit = TRUE;

			/* Requires a monster */
			if ((who < SOURCE_MONSTER_START) && (who != SOURCE_PLAYER_ALLY)) break;

			/* Get the blow */
			effect = r_ptr->blow[attack-96].effect;
			method = r_ptr->blow[attack-96].method;
			d_dice = r_ptr->blow[attack-96].d_dice;
			d_side = r_ptr->blow[attack-96].d_side;

			/* Get the damage */
			if ((d_dice) && (d_side)) dam = damroll(d_dice, d_side);

			/* Hack -- save passing effect for batter and wound */
			if (effect == GF_BATTER) do_stun = TRUE;
			else if (effect == GF_WOUND) do_cut = TRUE;

			/* Apply monster stats */
			if (m_ptr->mflag & (MFLAG_CLUMSY)) power -= 5;
			else if (m_ptr->mflag & (MFLAG_SKILLFUL)) power += 5;

			/* Apply temporary conditions */
			if (m_ptr->bless) power += 10;
			if (m_ptr->berserk) power += 24;

			/* Player has chance of being missed by various ranged attacks */
			if (target < 0)
			{
				hit = check_hit(attack_power(effect), rlev - m_ptr->cdis, who, TRUE);
			}
			else if (target > 0)
			{
				hit = mon_check_hit(target, effect, rlev - m_ptr->cdis, who, TRUE);
			}

			/* Get attack */
			if ((!blind) && (m_ptr->ml)) attack_desc(who, what, target, method, dam, &do_cut, &do_stun);

			/* Already displayed result */
			result = NULL;

			/* Only do if damage */
			if (dam) switch (method)
			{
				case RBM_SPIT:	mon_shot(who, what, y, x, effect, dam, hit, result); break;
				case RBM_GAZE:	msg_print(result);(void)project(who, what, 0, m_ptr->fy, m_ptr->fx, y, x, dam, effect, FLG_MON_DIRECT, 0, 0);  break;
				case RBM_WAIL: msg_print(result);(void)project(who, what, 4, m_ptr->fy, m_ptr->fx, m_ptr->fy, m_ptr->fx, dam, effect, FLG_MON_BALL | PROJECT_HIDE, 0, 0);  break;
				case RBM_HOWL: msg_print(result);(void)project(who, what, 2, m_ptr->fy, m_ptr->fx, m_ptr->fy, m_ptr->fx, dam, effect, FLG_MON_BALL | PROJECT_HIDE, 0, 0);  break;
				case RBM_SHRIEK: msg_print(result); (void)project(who, what, 6, m_ptr->fy, m_ptr->fx, y, x, dam, effect, FLG_MON_ARC | PROJECT_HIDE, 20, 20); aggravate_monsters(who); break;
				case RBM_SPORE:	mon_ball_minor_shot(who, what, y, x, effect, dam, 1, hit, result); break;
				case RBM_LASH:  mon_beam(who, what, y, x, effect, dam, 2, result); break;
				case RBM_BEG:	msg_print(result);(void)project(who, what, 0, m_ptr->fy, m_ptr->fx, y, x, dam, effect, FLG_MON_DIRECT, 0, 0);  break;
				case RBM_INSULT: msg_print(result);(void)project(who, what, 0, m_ptr->fy, m_ptr->fx, y, x, dam, effect, FLG_MON_DIRECT, 0, 0);  break;
				case RBM_SING:  msg_print(result);(void)project(who, what, 0, m_ptr->fy, m_ptr->fx, y, x, dam, effect, FLG_MON_DIRECT, 0, 0);  break;
				case RBM_TRAP:  msg_print(result);(void)project(who, what, 0, m_ptr->fy, m_ptr->fx, y, x, dam, effect, FLG_MON_DIRECT, 0, 0);  break;
				case RBM_BOULDER: mon_shot(who, what, y, x, effect, dam, hit, result); break;
				case RBM_AURA:	msg_print(result);(void)project(who, what, 2, m_ptr->fy, m_ptr->fx, m_ptr->fy, m_ptr->fx, dam, effect, FLG_MON_CLOUD, 0, 0);  break;
				case RBM_AURA_MINOR:	msg_print(result);(void)project(who, what, 1, m_ptr->fy, m_ptr->fx, m_ptr->fy, m_ptr->fx, dam, effect, FLG_MON_CLOUD, 0, 0);  break;
				case RBM_SELF:	msg_print(result);(void)project(SOURCE_SELF, who, 0, m_ptr->fy, m_ptr->fx, m_ptr->fy, m_ptr->fx, dam, effect, FLG_MON_DIRECT, 0, 0); break;
				case RBM_ADJACENT: msg_print(result);(void)project(SOURCE_SELF, who, (rlev / 10) + 1, m_ptr->fy, m_ptr->fx, m_ptr->fy, m_ptr->fx, dam, effect, FLG_MON_BALL | PROJECT_HIDE, 0, 0);  break;
				case RBM_HANDS: mon_beam(who, what, y, x, effect, dam, 3, result); break;
				case RBM_MISSILE: mon_bolt(who, what, y, x, effect, dam, result); break;
				case RBM_BOLT_MINOR: mon_bolt(who, what, y, x, effect, dam, result); break;
				case RBM_BOLT_10: (rand_int(100) < 10 ? mon_beam(who, what, y, x, effect, dam, 10, result) : mon_bolt(who, what, y, x, effect, dam, result)); break;
				case RBM_BOLT: mon_bolt(who, what, y, x, effect, dam, result); break;
				case RBM_BEAM: mon_beam(who, what, y, x, effect, dam, 10, result); break;
				case RBM_BLAST: mon_ball(who, what, y, x, effect, dam, 0, TRUE, result); break;
				case RBM_WALL: mon_beam(who, what, y, x, effect, dam, 12, result); break;
				case RBM_BALL_MINOR: mon_ball_minor(who, what, y, x, effect, dam, 2, FALSE, result); break;
				case RBM_BALL: mon_ball(who, what, y, x, effect, dam, 2, TRUE, result); break;
				case RBM_BALL_II: mon_ball(who, what, y, x, effect, dam, 3, TRUE, result); break;
				case RBM_BALL_III: mon_ball(who, what, y, x, effect, dam, 4, TRUE, result); break;
				case RBM_CLOUD: mon_area(who, what, y, x, effect, dam, 3, result); break;
				case RBM_STORM: mon_area(who, what, y, x, effect, dam, 3, result); break;
				case RBM_BREATH: mon_arc(who, what, y, x, effect, MIN(dam, m_ptr->hp / d_side), 0, (powerful ? 40 : 20), result); break;
				case RBM_AREA: (void)project(who, what, (rlev / 10) + 1, m_ptr->fy, m_ptr->fx, m_ptr->fy, m_ptr->fx, dam, effect, FLG_MON_BALL | PROJECT_AREA, 0, 0);  break;
				case RBM_AIM_AREA: (void)project(who, what, (rlev / 10) + 1, m_ptr->fy, m_ptr->fx, m_ptr->fy, m_ptr->fx, dam, effect, FLG_MON_BALL | PROJECT_AREA, 0, 0);  break;
				case RBM_LOS: (void)project(who, what, 0, m_ptr->fy, m_ptr->fx, y, x, dam, effect, FLG_MON_DIRECT, 0, 0);  break;
				case RBM_LINE: mon_beam(who, what, y, x, effect, dam, 8, result); break;
				case RBM_AIM: (void)project(who, what, 0, m_ptr->fy, m_ptr->fx, y, x, dam, effect, FLG_MON_DIRECT, 0, 0);  break;
				case RBM_ORB: mon_area(who, what, y, x, effect, dam, 2, result); break;
				case RBM_STAR: mon_beam(who, what, y, x, effect, dam, 10, result); break;
				case RBM_SPHERE: mon_area(who, what, y, x, effect, dam, 4, result); break;
				case RBM_PANEL: (void)project(who, what, 0, m_ptr->fy, m_ptr->fx, y, x, dam, effect, FLG_MON_DIRECT | PROJECT_WALL, 0, 0);  break;
				case RBM_LEVEL: (void)project(who, what, 0, m_ptr->fy, m_ptr->fx, y, x, dam, effect, FLG_MON_DIRECT | PROJECT_WALL, 0, 0);  break;
				case RBM_CROSS: mon_beam(who, what, y, x, effect, dam, 10, result); break;
				case RBM_STRIKE: mon_ball(who, what, y, x, effect, dam, (rlev / 10) + 2, TRUE, result); break;
				case RBM_EXPLODE: (void)project(SOURCE_SELF, who, 2, m_ptr->fy, m_ptr->fx, m_ptr->fy, m_ptr->fx, damroll(5,8), GF_EXPLODE, FLG_MON_BALL, 0, 0); break;
				case RBM_ARROW: mon_shot(who, what, y, x, effect, dam, hit, result); break;
				case RBM_XBOLT: mon_shot(who, what, y, x, effect, dam, hit, result); break;
				case RBM_SPIKE: mon_shot(who, what, y, x, effect, dam, hit, result); break;
				case RBM_DART: mon_shot(who, what, y, x, effect, dam, hit, result); break;
				case RBM_SHOT: mon_shot(who, what, y, x, effect, dam, hit, result); break;
				case RBM_ARC_20: mon_arc(who, what, y, x, effect, dam, 0, (powerful ? 40 : 20), result); break;
				case RBM_ARC_30: mon_arc(who, what, y, x, effect, dam, 0, (powerful ? 50 : 30), result); break;
				case RBM_ARC_40: mon_arc(who, what, y, x, effect, dam, 0, (powerful ? 60 : 40), result); break;
				case RBM_ARC_50: mon_arc(who, what, y, x, effect, dam, 0, 50, result); break;
				case RBM_ARC_60: mon_arc(who, what, y, x, effect, dam, 0, 60, result); break;
				case RBM_FLASK:	mon_ball_minor_shot(who, what, y, x, effect, dam, 1, hit, result); break;
				case RBM_8WAY: mon_8way(who, what, y, x, effect, dam, 2, result); break;
				case RBM_8WAY_II: mon_8way(who, what, y, x, effect, dam, 3, result); break;
				case RBM_8WAY_III: mon_8way(who, what, y, x, effect, dam, 4, result); break;
				case RBM_SWARM: for (k = 0; k < (rlev / 20) + 2; k++) mon_ball_minor(who, what, y, x, effect, dam, 2, TRUE, result); break;
				case RBM_DAGGER: mon_shot(who, what, y, x, effect, dam, hit, result); break;
				case RBM_SCATTER: for (k = 0; k < (rlev / 10) + 3; k++) { scatter(&y, &x, m_ptr->fy, m_ptr->fx, 5, 0); (void)project(who, what, 0, m_ptr->fy, m_ptr->fx, y, x, dam, effect, FLG_MON_DIRECT, 0, 0); } break;
				default: mon_beam(who, what, y, x, effect, dam, 2, result); /* For all hurt huge attacks */
			}

			/* Hack -- only one of cut or stun */
			if (do_cut && do_stun)
			{
				/* Cancel cut */
				if (rand_int(100) < 50)
				{
					do_cut = FALSE;
				}

				/* Cancel stun */
				else
				{
					do_stun = FALSE;
				}
			}

			/* Handle cut */
			if ((do_cut) && (target < 0))
			{
				int k, tmp;

				/* Critical hit (zero if non-critical) */
				tmp = monster_critical(d_dice, d_side, dam, effect);

				/* Roll for damage */
				switch (tmp)
				{
					case 0: k = 0; break;
					case 1: k = randint(5); break;
					case 2: k = randint(5) + 5; break;
					case 3: k = randint(20) + 20; break;
					case 4: k = randint(50) + 50; break;
					case 5: k = randint(100) + 100; break;
					case 6: k = 300; break;
					default: k = 500; break;
				}

				/* Apply the cut */
				if ((k) && !(p_ptr->stastis)) (void)set_cut(p_ptr->cut + k);
			}

			/* Handle stun */
			if ((do_stun) && (target < 0))
			{
				int k, tmp;

				/* Critical hit (zero if non-critical) */
				tmp = monster_critical(d_dice, d_side, dam, effect);

				/* Roll for damage */
				switch (tmp)
				{
					case 0: k = 0; break;
					case 1: k = randint(5); break;
					case 2: k = randint(8) + 8; break;
					case 3: k = randint(15) + 15; break;
					case 4: k = randint(25) + 25; break;
					case 5: k = randint(35) + 35; break;
					case 6: k = randint(45) + 45; break;
					default: k = 100; break;
				}

				/* Apply the stun */
				if ((k) && !(p_ptr->stastis)) (void)set_stun(p_ptr->stun ? p_ptr->stun + randint(MIN(k,10)) : k);
			}
			break;
		}

		/* RF6_ADD_AMMO */
		case 96+4:
		{
			int ammo = 0;

			/* Requires a monster */
			if ((who < SOURCE_MONSTER_START) && (who != SOURCE_PLAYER_ALLY)) break;

			/* Grow ammunition */
			ammo = find_monster_ammo(who > SOURCE_MONSTER_START ? who : what, -1, TRUE);

			if (ammo)
			{
				switch(o_list[ammo].tval)
				{
					case TV_EGG: msg_format("%^s grows more spores.", m_name); break;
					case TV_JUNK: msg_format("%^s grows more rocks.", m_name); break;
					case TV_ARROW: msg_format("%^s grows more arrows.", m_name); break;
					case TV_BOLT: msg_format("%^s grows more bolts.", m_name); break;
					case TV_SPIKE: msg_format("%^s grows more spikes.", m_name); break;
					case TV_POLEARM: msg_format("%^s grows more darts.", m_name); break;
					case TV_SHOT: msg_format("%^s grows more shots.", m_name); break;
				}
			}
			break;
		}

		/* RF4_QUAKE --- Earthquake */
		case 96+5:
		{
			if (target < 0) disturb(1,0);

			/* Centre on caster */
			if ((who > SOURCE_MONSTER_START) || (who == SOURCE_PLAYER_ALLY))
			{
				y = m_ptr->fy;
				x = m_ptr->fx;
			}

			earthquake(y, x, 8);
			break;
		}

		/* RF4_EXPLODE --- used by chests */
		case 96+6:
		{
			if (target < 0) disturb(1,0);

			if (known) msg_format("%^s explodes.", m_name);

			/* Centre on caster */
			if ((who > SOURCE_MONSTER_START) || (who == SOURCE_PLAYER_ALLY))
			{
				y = m_ptr->fy;
				x = m_ptr->fx;
				
				what = who;
				who = SOURCE_SELF;
			}

			/* Target everyone (including caster) with a ball attack */
			(void)project(who, what, 2, y, x, y, x, damroll(5,8), GF_EXPLODE, FLG_MON_BALL, 0, 0);

			break;
		}

		/* RF4_AURA */
		case 96+7:
		{
			int rad = (powerful ? 2 : 1);

			if (target < 0) disturb(1,0);

			/* Centre on caster */
			if ((who > SOURCE_MONSTER_START) || (who == SOURCE_PLAYER_ALLY))
			{
				y = m_ptr->fy;
				x = m_ptr->fx;
			}

			/* The target is attacked by a ball attack */
			mon_blow_ranged(who, attack, y, x, RBM_AURA, rad, FLG_MON_CLOUD, result);

			break;
		}

		/* RF4_BRTH_ACID */
		case 96+8:
		{
			if (target < 0) disturb(1, 0);
			sound(MSG_BR_ACID);
			if (blind) result = format("%^s breathes.", m_name);
			else result = format("%^s breathes acid at %s.", m_name, t_name);

			/*
			 * Breaths are 40-degree arcs for POWERFUL monsters,
			 * 20 degrees for others.
			 */
			mon_arc(who, what, y, x, GF_ACID, get_breath_dam(m_ptr->hp, GF_ACID, powerful),
			        0, (powerful ? 40 : 20), result);
			break;
		}

		/* RF4_BRTH_ELEC */
		case 96+9:
		{
			if (target < 0) disturb(1, 0);
			sound(MSG_BR_ELEC);
			if (blind) result = format("%^s breathes.", m_name);
			else result = format("%^s breathes lightning at %s.", m_name, t_name);

			mon_arc(who, what, y, x, GF_ELEC, get_breath_dam(m_ptr->hp, GF_ELEC, powerful),
			        0, (powerful ? 40 : 20), result);
			break;
		}

		/* RF4_BRTH_FIRE */
		case 96+10:
		{
			if (target < 0) disturb(1, 0);
			sound(MSG_BR_FIRE);
			if (blind) result = format("%^s breathes.", m_name);
			else result = format("%^s breathes fire at %s.", m_name, t_name);

			mon_arc(who, what, y, x, GF_FIRE, get_breath_dam(m_ptr->hp, GF_FIRE, powerful),
			        0, (powerful ? 40 : 20), result);
			break;
		}

		/* RF4_BRTH_COLD */
		case 96+11:
		{
			if (target < 0) disturb(1, 0);
			sound(MSG_BR_FROST);
			if (blind) result = format("%^s breathes.", m_name);
			else result = format("%^s breathes frost at %s.", m_name, t_name);

			mon_arc(who, what, y, x, GF_COLD, get_breath_dam(m_ptr->hp, GF_COLD, powerful),
			       0, (powerful ? 40 : 20), result);
			break;
		}

		/* RF4_BRTH_POIS */
		case 96+12:
		{
			if (target < 0) disturb(1, 0);
			sound(MSG_BR_GAS);
			if (blind) result = format("%^s breathes.", m_name);
			else result = format("%^s breathes gas at %s.", m_name, t_name);

			mon_arc(who, what, y, x, GF_POIS, get_breath_dam(m_ptr->hp, GF_POIS, powerful),
			        0, (powerful ? 50 : 30), result);
			break;
		}

		/* RF4_BRTH_PLAS */
		case 96+13:
		{
			if (target < 0) disturb(1, 0);
			sound(MSG_BR_PLASMA);
			if (blind) result = format("%^s breathes.", m_name);
			else result = format("%^s breathes plasma at %s.", m_name, t_name);

			mon_arc(who, what, y, x, GF_PLASMA, get_breath_dam(m_ptr->hp, GF_PLASMA, powerful),
				   0, (powerful ? 40 : 20), result);
			break;
		}

		/* RF4_BRTH_LITE */
		case 96+14:
		{
			if (target < 0) disturb(1, 0);
			sound(MSG_BR_LIGHT);
			if (blind) result = format("%^s breathes.", m_name);
			else result = format("%^s breathes light at %s.", m_name, t_name);

			mon_arc(who, what, y, x, GF_LITE, get_breath_dam(m_ptr->hp, GF_LITE, powerful),
			        0, (powerful ? 40 : 20), result);
			break;
		}

		/* RF4_BRTH_DARK */
		case 96+15:
		{
			if (target < 0) disturb(1, 0);
			sound(MSG_BR_DARK);
			if (blind) result = format("%^s breathes.", m_name);
			else result = format("%^s breathes darkness at %s.", m_name, t_name);

			mon_arc(who, what, y, x, GF_DARK, get_breath_dam(m_ptr->hp, GF_DARK, powerful),
					 0, (powerful ? 40 : 20), result);
			break;
		}

		/* RF4_BRTH_CONFU */
		case 96+16:
		{
			if (target < 0) disturb(1, 0);
			sound(MSG_BR_CONF);
			if (blind) result = format("%^s breathes.", m_name);
			else result = format("%^s breathes confusion at %s.", m_name, t_name);

			mon_arc(who, what, y, x, GF_CONFUSION, get_breath_dam(m_ptr->hp, GF_CONFUSION, powerful),
			       0, (powerful ? 40 : 20), result);
			break;
		}

		/* RF4_BRTH_SOUND */
		case 96+17:
		{
			if (target < 0) disturb(1, 0);
			sound(MSG_BR_SOUND);
			if (blind) result = format("%^s breathes.", m_name);
			else result = format("%^s breathes sound at %s.", m_name, t_name);

			mon_arc(who, what, y, x, GF_SOUND, get_breath_dam(m_ptr->hp, GF_SOUND, powerful),
			       0, (powerful ? 50 : 30), result);
			break;
		}

		/* RF4_BRTH_SHARD */
		case 96+18:
		{
			if (target < 0) disturb(1, 0);
			sound(MSG_BR_SHARDS);
			if (blind) result = format("%^s breathes.", m_name);
			else result = format("%^s breathes shards at %s.", m_name, t_name);
			mon_arc(who, what, y, x, GF_SHARD, get_breath_dam(m_ptr->hp, GF_SHARD, powerful),
			        0, (powerful ? 40 : 20), result);
			break;
		}

		/* RF4_BRTH_INERT */
		case 96+19:
		{
			if (target < 0) disturb(1, 0);
			sound(MSG_BR_INERTIA);
			if (blind) result = format("%^s breathes.", m_name);
			else result = format("%^s breathes inertia at %s.", m_name, t_name);
			mon_arc(who, what, y, x, GF_INERTIA, get_breath_dam(m_ptr->hp, GF_INERTIA, powerful),
				   0, (powerful ? 40 : 20), result);
			break;
		}

		/* RF4_BRTH_GRAV */
		case 96+20:
		{
			if (target < 0) disturb(1, 0);
			sound(MSG_BR_GRAVITY);
			if (blind) result = format("%^s breathes.", m_name);
			else result = format("%^s breathes gravity at %s.", m_name, t_name);
			mon_arc(who, what, y, x, GF_GRAVITY, get_breath_dam(m_ptr->hp, GF_GRAVITY, powerful),
				   0, (powerful ? 40 : 20), result);
			break;
		}

		/* RF4_BRTH_WIND */
		case 96+21:
		{
			if (target < 0) disturb(1, 0);
			if (blind) result = format("%^s breathes.", m_name);
			else result = format("%^s breathes wind at %s.", m_name, t_name);
			mon_arc(who, what, y, x, GF_WIND, get_breath_dam(m_ptr->hp, GF_WIND, powerful),
				   0, (powerful ? 40 : 20), result);
			break;
		}

		/* RF4_BRTH_FORCE */
		case 96+22:
		{
			if (target < 0) disturb(1, 0);
			sound(MSG_BR_FORCE);
			if (blind) result = format("%^s breathes.", m_name);
			else result = format("%^s breathes force at %s.", m_name, t_name);
			mon_arc(who, what, y, x, GF_FORCE, get_breath_dam(m_ptr->hp, GF_FORCE, powerful),
				   0, (powerful ? 40 : 20), result);
			break;
		}

		/* RF4_BRTH_NEXUS */
		case 96+23:
		{
			if (target < 0) disturb(1, 0);
			sound(MSG_BR_NEXUS);
			if (blind) result = format("%^s breathes.", m_name);
			else result = format("%^s breathes nexus at %s.", m_name, t_name);
			mon_arc(who, what, y, x, GF_NEXUS, get_breath_dam(m_ptr->hp, GF_GRAVITY, powerful),
			       0, (powerful ? 40 : 20), result);
			break;
		}

		/* RF4_BRTH_NETHR */
		case 96+24:
		{
			if (target < 0) disturb(1, 0);
			sound(MSG_BR_NETHER);
			if (blind) result = format("%^s breathes.", m_name);
			else result = format("%^s breathes nether at %s.", m_name, t_name);
			mon_arc(who, what, y, x, GF_NETHER, get_breath_dam(m_ptr->hp, GF_NETHER, powerful),
			       0, (powerful ? 40 : 20), result);
			break;
		}

		/* RF4_BRTH_CHAOS */
		case 96+25:
		{
			if (target < 0) disturb(1, 0);
			sound(MSG_BR_CHAOS);
			if (blind) result = format("%^s breathes.", m_name);
			else result = format("%^s breathes chaos at %s.", m_name, t_name);
			mon_arc(who, what, y, x, GF_CHAOS, get_breath_dam(m_ptr->hp, GF_CHAOS, powerful),
			       0, (powerful ? 40 : 20), result);
			break;
		}

		/* RF4_BRTH_DISEN */
		case 96+26:
		{
			if (target < 0) disturb(1, 0);
			sound(MSG_BR_DISENCHANT);
			if (blind) result = format("%^s breathes.", m_name);
			else result = format("%^s breathes disenchantment at %s.", m_name, t_name);
			mon_arc(who, what, y, x, GF_DISENCHANT, get_breath_dam(m_ptr->hp, GF_DISENCHANT, powerful),
			       0, (powerful ? 40 : 20), result);
			break;
		}

		/* RF4_BRTH_TIME */
		case 96+27:
		{
			if (target < 0) disturb(1, 0);
			sound(MSG_BR_TIME);
			if (blind) result = format("%^s breathes.", m_name);
			else result = format("%^s breathes time at %s.", m_name, t_name);
			mon_arc(who, what, y, x, GF_TIME, get_breath_dam(m_ptr->hp, GF_TIME, powerful),
				   0, (powerful ? 40 : 20), result);
			break;
		}

		/* RF4_BRTH_MANA */
		case 96+28:
		{
			if (target < 0) disturb(1, 0);
			if (blind) result = format("%^s breathes.", m_name);
			else result = format("%^s breathes raw mana at %s.", m_name, t_name);
			mon_arc(who, what, y, x, GF_MANA, get_breath_dam(m_ptr->hp, GF_MANA, powerful),
				    0, (powerful ? 40 : 20), result);
			break;
		}

		/* RF4_BRTH_HOLY */
		case 96+29:
		{
			if (target < 0) disturb(1, 0);
			if (blind) result = format("%^s breathes.", m_name);
			else result = format("%^s breathes holiness at %s.", m_name, t_name);
			mon_arc(who, what, y, x, GF_HOLY_ORB, get_breath_dam(m_ptr->hp, GF_HOLY_ORB, powerful),
				    0, (powerful ? 40 : 20), result);
			break;
		}

		/* RF4_BRTH_FEAR */
		case 96+30:
		{
			if (target < 0) disturb(1, 0);
			if (blind) result = format("%^s breathes.", m_name);
			else result = format("%^s breathes terror at %s.", m_name, t_name);
			mon_arc(who, what, y, x, GF_TERRIFY, get_breath_dam(m_ptr->hp, GF_TERRIFY, powerful),
				    0, (powerful ? 40 : 20), result);
			break;
		}

		/* RF4_BRTH_DISEA */
		case 96+31:
		{
			if (target < 0) disturb(1, 0);
			if (blind) result = format("%^s breathes.", m_name);
			else result = format("%^s breathes pestilence at %s.", m_name, t_name);
			mon_arc(who, what, y, x, GF_DISEASE, get_breath_dam(m_ptr->hp, GF_DISEASE, powerful),
				    0, (powerful ? 40 : 20), result);
			break;
		}

		/* RF5_BALL_ACID */
		case 128+0:
		{
			if (target < 0) disturb(1, 0);
			if (spower < 10)
			{
				if (blind) result = format("%^s mumbles.", m_name);
				else result = format("%^s casts an acid ball at %s.", m_name, t_name);
				rad = 1;
			}
			else if (spower < 40)
			{
				if (blind) result = format("%^s murmurs deeply.", m_name);
				else result = format("%^s casts an acid ball at %s.", m_name, t_name);
				rad = 2;
			}
			else
			{
				if (blind) result = format("%^s chants powerfully.", m_name);
				else result = format("%^s invokes a storm of acid around %s.", m_name, t_name);
				if (spower < 80) rad = 3;
				else rad = 4;
			}
			mon_ball(who, what, y, x, GF_ACID, get_dam(spower, attack), rad, TRUE, result);
			break;
		}

		/* RF5_BALL_ELEC */
		case 128+1:
		{
			if (target < 0) disturb(1, 0);
			if (spower < 10)
			{
				if (blind) result = format("%^s mumbles.", m_name);
				else result = format("%^s casts a ball of electricity at %s.", m_name, t_name);
				rad = 1;
			}
			else if (spower < 40)
			{
				if (blind) result = format("%^s murmurs deeply.", m_name);
				else result = format("%^s casts a ball of electricity at %s.", m_name, t_name);
				rad = 2;
			}

			/* Electricity is the most variable of all attacks at high level. */
			else
			{
				if (blind) result = format("%^s chants powerfully.", m_name);

				if (rand_int(3) != 0)
				{
				result = format("%^s invokes a storm of electricity around %s.", m_name, t_name);
					if (spower < 80) rad = 3;
					else rad = 4;
					spower = 3 * spower / 4;
				}
				else
				{
					result = format("%^s calls a massive stroke of lightning down upon %s!", m_name, t_name);
					rad = 0;
					spower = 3 * spower / 2;
				}
			}
			mon_ball(who, what, y, x, GF_ELEC, get_dam(spower, attack), rad, TRUE, result);
			break;
		}

		/* RF5_BALL_FIRE */
		case 128+2:
		{
			if (target < 0) disturb(1, 0);
			if (spower < 10)
			{
				if (blind) result = format("%^s mumbles.", m_name);
				else result = format("%^s casts a ball of fire at %s.", m_name, t_name);
				rad = 1;
			}
			else if (spower < 40)
			{
				if (blind) result = format("%^s murmurs deeply.", m_name);
				else result = format("%^s casts a ball of fire at %s.", m_name, t_name);
				rad = 2;
			}
			else if (spower < 80)
			{
				if (blind) result = format("%^s chants powerfully.", m_name);
				else result = format("%^s invokes a firestorm at %s.", m_name, t_name);
				rad = 3;
			}
			else
			{
				if (blind) result = format("%^s intones in rising wrath.", m_name);
				else result = format("%^s conjures up a maelstrom of fire upon %s!", m_name, t_name);
				rad = 4;
			}
			mon_ball(who, what, y, x, GF_FIRE, get_dam(spower, attack), rad, TRUE, result);
			break;
		}

		/* RF5_BALL_COLD */
		case 128+3:
		{
			if (target < 0) disturb(1, 0);
			if (spower < 10)
			{
				if (blind) result = format("%^s mumbles.", m_name);
				else result = format("%^s casts a frost ball at %s.", m_name, t_name);
				rad = 1;
			}
			else if (spower < 40)
			{
				if (blind) result = format("%^s murmurs deeply.", m_name);
				else result = format("%^s casts a frost ball at %s.", m_name, t_name);
				rad = 2;
			}
			else
			{
				if (blind) result = format("%^s chants powerfully.", m_name);
				else result = format("%^s invokes a storm of frost around %s.", m_name, t_name);
				if (spower < 80) rad = 3;
				else rad = 4;
			}
			mon_ball(who, what, y, x, GF_COLD, get_dam(spower, attack), rad, TRUE, result);
			break;
		}

		/* RF5_BALL_POIS */
		case 128+4:
		{
			if (target < 0) disturb(1, 0);
			if (spower < 10)
			{
				if (blind) result = format("%^s mumbles.", m_name);
				else result = format("%^s casts a stinking cloud at %s.", m_name, t_name);
				rad = 2;
			}
			else if (spower < 40)
			{
				if (blind) result = format("%^s murmurs deeply.", m_name);
				else result = format("%^s casts a venomous cloud at %s.", m_name, t_name);
				rad = 3;
			}
			else
			{
				if (blind) result = format("%^s chants powerfully.", m_name);
				else result = format("%^s invokes a storm of poison at %s.", m_name, t_name);
				if (spower < 80) rad = 4;
				else rad = 5;
			}
			mon_ball(who, what, y, x, GF_POIS, get_dam(spower, attack), rad, TRUE, result);
			break;
		}

		/* RF5_BALL_LITE */
		case 128+5:
		{
			if (target < 0) disturb(1, 0);
			
			/* Sometimes try to concentrate light */
			if ((rand_int(100) < 50) && ((who <= 0) || (generic_los(y, x, m_ptr->fy, m_ptr->fx, CAVE_XLOF))))
			{
				/* Check to see if doing so would be worthwhile */
				int damage = mon_concentrate_power(y, x, spower, TRUE, FALSE, concentrate_light_hook);

				/* We decided to concentrate light */
				if (damage)
				{
					/* Message */
					if (blind) result = format("%^s mumbles.", m_name);
					else result = format("%^s concentrates light... and releases it.", m_name);

					/* Fire bolt */
					mon_bolt(who, what, y, x, GF_LITE, damage, result);

					/* Done */
					break;
				}
			}
			
			if (spower < 10)
			{
				if (blind) result = format("%^s mumbles.", m_name);
				else result = format("%^s casts a sphere of light at %s.", m_name, t_name);
				rad = 1;
			}
			else if (spower < 40)
			{
				if (blind) result = format("%^s murmurs deeply.", m_name);
				else result = format("%^s invokes an explosion of light around %s.", m_name, t_name);
				rad = 2;
			}
			else
			{
				if (blind) result = format("%^s chants powerfully.", m_name);
				else result = format("%^s invokes a powerful explosion of light around %s.", m_name, t_name);
				rad = 3;
			}
			mon_ball(who, what, y, x, GF_LITE, get_dam(spower, attack), rad, TRUE, result);
			break;
		}

		/* RF5_BALL_DARK */
		case 128+6:
		{
			if (target < 0) disturb(1, 0);
			
			/* Sometimes try to concentrate light */
			if ((rand_int(100) < 50) && ((who <= 0) || (generic_los(y, x, m_ptr->fy, m_ptr->fx, CAVE_XLOF))))
			{
				/* Check to see if doing so would be worthwhile */
				int damage = mon_concentrate_power(y, x, spower, TRUE, TRUE, concentrate_light_hook);

				/* We decided to concentrate light */
				if (damage)
				{
					/* Message */
					if (blind) result = format("%^s mumbles.", m_name);
					else result = format("%^s conjures up darkness... and releases it.", m_name);

					/* Fire bolt */
					mon_bolt(who, what, y, x, GF_LITE, damage, result);

					/* Done */
					break;
				}
			}

			if (spower < 20)
			{
				if (blind) result = format("%^s mumbles.", m_name);
				else result = format("%^s casts a ball of darkness at %s.", m_name, t_name);
				rad = 1;
			}
			else if (spower < 70)
			{
			if (blind) result = format("%^s murmurs deeply.", m_name);
				else result = format("%^s casts a storm of darkness at %s.", m_name, t_name);
				rad = 2;
			}
			else
			{
				if (blind) result = format("%^s chants powerfully.", m_name);
				else result = format("%^s invokes a powerful darkness storm around %s.", m_name, t_name);
				if (spower < 110) rad = 3;
				else rad = 4;
			}
			mon_ball(who, what, y, x, GF_DARK, get_dam(spower, attack), rad, TRUE, result);
			break;
		}

		/* RF5_BALL_CONFU */
		case 128+7:
		{
			if (target < 0) disturb(1, 0);
			if (spower < 10)
			{
				if (blind) result = format("%^s mumbles.", m_name);
				else result = format("%^s casts a ball of confusion at %s.", m_name, t_name);
				rad = 1;
			}
			else if (spower < 40)
			{
				if (blind) result = format("%^s murmurs deeply.", m_name);
				else result = format("%^s casts a storm of confusion at %s.", m_name, t_name);
				rad = 2;
			}
			else
			{
				if (blind) result = format("%^s chants powerfully.", m_name);
				else result = format("%^s invokes a powerful storm of confusion around %s.", m_name, t_name);
				rad = 3;
			}
			mon_ball(who, what, y, x, GF_CONFUSION, get_dam(spower, attack), rad, TRUE, result);
			break;
		}

		/* RF5_BALL_SOUND */
		case 128+8:
		{
			if (target < 0) disturb(1, 0);
			if (spower < 10)
			{
				if (blind) result = format("%^s mumbles.", m_name);
				else result = format("%^s calls up a blast of sound around %s.", m_name, t_name);
				rad = 1;
			}
			else if (spower < 40)
			{
				if (blind) result = format("%^s murmurs deeply.", m_name);
				else result = format("%^s invokes a thunderclap around %s.", m_name, t_name);
				rad = 2;
			}
			else
			{
				if (blind) result = format("%^s chants powerfully.", m_name);
				else result = format("%^s unleashes a cacophony of sound around %s.", m_name, t_name);
				rad = 3;
			}
			mon_ball(who, what, y, x, GF_SOUND, get_dam(spower, attack), rad, TRUE, result);
			break;
		}

		/* RF5_BALL_SHARD */
		case 128+9:
		{
			if (target < 0) disturb(1, 0);
			if (spower < 10)
			{
				if (blind) result = format("%^s mumbles.", m_name);
				else result = format("%^s calls up up a blast of shards around %s.", m_name, t_name);
				rad = 1;
			}
			else if (spower < 50)
			{
				if (blind) result = format("%^s murmurs deeply.", m_name);
				else result = format("%^s calls up a whirlwind of shards around %s.", m_name, t_name);
				rad = 2;
			}
			else
			{
				if (blind) result = format("%^s chants powerfully.", m_name);
				else result = format("%^s invokes a storm of knives around %s!", m_name, t_name);
				rad = 3;
			}
			mon_ball(who, what, y, x, GF_SHARD, get_dam(spower, attack), rad, TRUE, result);
			break;
		}

		/* RF5_BALL_WIND */
		case 128+10:
		{
			if (target < 0) disturb(1, 0);

			if (spower < 22)
			{
				if (blind) result = format("%^s mumbles.", m_name);
				else result = format("%^s gestures fluidly.", m_name);
				msg_format("%^s %s surrounded by a dust devil.", t_nref, target < 0 ? "are" : "is");
				rad = 2;
			}
			else if (spower < 40)
			{
				if (blind) result = format("%^s mumbles.", m_name);
				else result = format("%^s gestures fluidly.", m_name);
				msg_format("%^s %s engulfed in a twister.", t_nref, target < 0 ? "are" : "is");
				rad = 3;
			}
			else
			{
				if (blind) result = format("%^s chants powerfully.", m_name);
				else result = format("%^s gestures fluidly.", m_name);
				msg_format("%^s %s lost in a raging tornado!", t_nref, target < 0 ? "are" : "is");
				rad = 5;
			}
			mon_ball(who, what, y, x, GF_WIND, get_dam(spower, attack), rad, TRUE, result);
			break;
		}

		/* RF5_BALL_STORM */
		case 128+11:
		{
			if (target < 0) disturb(1, 0);

			if (spower < 22)
			{
				if (blind) result = format("%^s mumbles.", m_name);
				else result = format("%^s gestures fluidly.", m_name);
				msg_format("%^s %s surrounded by a little storm.", t_nref, target < 0 ? "are" : "is");
				rad = 2;
			}
			else if (spower < 40)
			{
				if (blind) result = format("%^s mumbles.", m_name);
				else result = format("%^s gestures fluidly.", m_name);
				msg_format("%^s %s engulfed in a whirlpool.", t_nref, target < 0 ? "are" : "is");
				rad = 3;
			}
			else
			{
				if (blind) result = format("%^s chants powerfully.", m_name);
				else result = format("%^s gestures fluidly.", m_name);
				msg_format("%^s %s lost in a raging tempest of wind and water!", t_nref, target < 0 ? "are" : "is");
				rad = 5;
			}
			mon_ball(who, what, y, x, GF_WATER, get_dam(spower, attack), rad, TRUE, result);
			break;
		}

		/* RF5_BALL_NETHR */
		case 128+12:
		{
			if (target < 0) disturb(1, 0);
			if (spower < 22)
			{
				if (blind) result = format("%^s whispers nastily.", m_name);
				else result = format("%^s casts an orb of nether at %s.", m_name, t_name);
				rad = 1;
			}
			else if (spower < 40)
			{
				if (blind) result = format("%^s murmurs a deadly word.", m_name);
				else result = format("%^s casts a nether ball at %s.", m_name, t_name);
				rad = 2;
			}
			else
			{
				if (blind) result = format("%^s intones with deadly menace.", m_name);
				else result = format("%^s calls up a storm of nether magics around %s.", m_name, t_name);
			rad = 3;
			}
			mon_ball(who, what, y, x, GF_NETHER, get_dam(spower, attack), rad, TRUE, result);
			break;
		}

		/* RF5_BALL_CHAOS */
		case 128+13:
		{
			if (target < 0) disturb(1, 0);
			if (spower < 13)
			{
				if (blind) result = format("%^s mumbles.", m_name);
				else result = format("%^s casts a sphere of chaos at %s.", m_name, t_name);
				rad = 1;
			}
			else if (spower < 40)
			{
				if (blind) result = format("%^s murmurs deeply.", m_name);
				else result = format("%^s casts an exlosion of raw chaos at %s.", m_name, t_name);
				rad = 2;
			}
			else
			{
				if (blind) result = format("%^s chants powerfully.", m_name);
				else result = format("%^s invokes a storm of chaos around %s.", m_name, t_name);
				rad = 3;
			}
			mon_ball(who, what, y, x, GF_CHAOS, get_dam(spower, attack), rad, TRUE, result);
			break;
		}

		/* RF5_BALL_MANA */
		case 128+14:
		{
			if (target < 0) disturb(1, 0);
			if (spower < 25)
			{
				if (blind) result = format("%^s mumbles.", m_name);
				else result = format("%^s casts a mana burst at %s.", m_name, t_name);
				rad = 1;
			}
			else if (spower < 50)
			{
				if (blind) result = format("%^s murmurs deeply.", m_name);
				else result = format("%^s casts a mana ball at %s.", m_name, t_name);
				rad = 2;
			}
			else
			{
				if (blind) result = format("%^s chants powerfully.", m_name);
				else result = format("%^s invokes a storm of mana around %s.", m_name, t_name);
				rad = 3;
			}
			mon_ball(who, what, y, x, GF_MANA, get_dam(spower, attack), rad, TRUE, result);

			break;
		}

		/* RF5_BALL_WATER */
		case 128+15:
		{
			if (target < 0) disturb(1, 0);
			if (spower < 15)
			{
				if (blind) result = format("%^s mumbles.", m_name);
				else result = format("%^s casts a small water ball at %s.", m_name, t_name);
				rad = 1;
			}
			else if (spower < 40)
			{
				if (blind) result = format("%^s murmurs deeply.", m_name);
				else result = format("%^s casts a water ball at %s.", m_name, t_name);
				rad = 2;
			}
			else
			{
				if (blind) result = format("%^s chants powerfully.", m_name);
				else result = format("%^s invokes a storm of water around %s.", m_name, t_name);
				if (spower < 120) rad = 3;
				else rad = 4;
			}
			mon_ball(who, what, y, x, GF_WATER, get_dam(spower, attack), rad, TRUE, result);
			break;
		}

		/* RF5_BOLT_ACID */
		case 128+16:
		{
			if (target < 0) disturb(1, 0);
			if (spower < 50)
			{
				if (blind) result = format("%^s mumbles.", m_name);
				else result = format("%^s casts an acid bolt at %s.", m_name, t_name);
			}
			else
			{
				if (blind) result = format("%^s murmurs deeply.", m_name);
				else result = format("%^s casts a bolt of acid at %s.", m_name, t_name);
			}
			mon_bolt(who, what, y, x, GF_ACID, get_dam(spower, attack), result);
			break;
		}

		/* RF5_BOLT_ELEC */
		case 128+17:
		{
			if (target < 0) disturb(1, 0);
			if (spower < 50)
			{
				if (blind) result = format("%^s mumbles.", m_name);
				else result = format("%^s casts a bolt of electricity at %s.", m_name, t_name);
			}
			else
			{
				if (blind) result = format("%^s murmurs deeply.", m_name);
				else result = format("%^s casts a bolt of lightning at %s.", m_name, t_name);
			}
			mon_bolt(who, what, y, x, GF_ELEC, get_dam(spower, attack), result);
			break;
		}

		/* RF5_BOLT_FIRE */
		case 128+18:
		{
			if (target < 0) disturb(1, 0);
			if (spower < 50)
			{
				if (blind) result = format("%^s mumbles.", m_name);
				else result = format("%^s casts a fire bolt at %s.", m_name, t_name);
			}
			else
			{
				if (blind) result = format("%^s murmurs deeply.", m_name);
				else result = format("%^s throws a fiery sphere at %s.", m_name, t_name);
			}
			mon_bolt(who, what, y, x, GF_FIRE, get_dam(spower, attack), result);
			break;
		}

		/* RF5_BOLT_COLD */
		case 128+19:
		{
			if (target < 0) disturb(1, 0);
			if (spower < 50)
			{
				if (blind) result = format("%^s mumbles.", m_name);
				else result = format("%^s casts a frost bolt at %s.", m_name, t_name);
			}
			else
			{
				if (blind) result = format("%^s murmurs deeply.", m_name);
				else result = format("%^s casts a frost bolt at %s.", m_name, t_name);
			}
			mon_bolt(who, what, y, x, GF_COLD, get_dam(spower, attack), result);
			break;
		}

		/* RF5_BOLT_POIS */
		case 128+20:
		{
			disturb(1, 0);
			if (spower < 50)
			{
				if (blind) result = format("%^s mumbles.", m_name);
				else result = format("%^s casts a poison bolt at %s.", m_name, t_name);
			}
			else
			{
				if (blind) result = format("%^s murmurs deeply.", m_name);
				else result = format("%^s casts a bolt of venom at %s.", m_name, t_name);
			}
			mon_bolt(who, what, y, x, GF_POIS, get_dam(spower, attack), result);
			break;
		}

		/* RF5_BOLT_PLAS */
		case 128+21:
		{
			disturb(1, 0);
			if (spower < 50)
			{
				if (blind) result = format("%^s mumbles.", m_name);
				else result = format("%^s casts a plasma bolt at %s.", m_name, t_name);
			}
			else
			{
				if (blind) result = format("%^s murmurs deeply.", m_name);
				else result = format("%^s casts a bolt of plasma at %s.", m_name, t_name);
			}
			mon_bolt(who, what, y, x, GF_PLASMA, get_dam(spower, attack), result);
			break;
		}

		/* RF5_BOLT_ICE */
		case 128+22:
		{
			disturb(1, 0);
			if (spower < 50)
			{
				if (blind) result = format("%^s mumbles.", m_name);
				else result = format("%^s casts an ice bolt at %s.", m_name, t_name);
			}
			else
			{
				if (blind) result = format("%^s murmurs deeply.", m_name);
				else result = format("%^s casts a bolt of ice at %s.", m_name, t_name);
			}
			mon_bolt(who, what, y, x, GF_ICE, get_dam(spower, attack), result);
			break;
		}

		/* RF5_BOLT_WATER */
		case 128+23:
		{
			disturb(1, 0);
			if (spower < 50)
			{
				if (blind) result = format("%^s mumbles.", m_name);
				else result = format("%^s casts a water bolt at %s.", m_name, t_name);
			}
			else
			{
				if (blind) result = format("%^s murmurs deeply.", m_name);
				else result = format("%^s casts a water bolt at %s.", m_name, t_name);
			}
			mon_bolt(who, what, y, x, GF_WATER, get_dam(spower, attack), result);
			break;
		}

		/* RF5_BOLT_NETHR */
		case 128+24:
		{
			disturb(1, 0);
			if (spower < 40)
			{
				if (blind) result = format("%^s whispers nastily.", m_name);
				else result = format("%^s casts a nether bolt at %s.", m_name, t_name);
			}
			else
			{
				if (blind) result = format("%^s murmurs a deadly word.", m_name);
				else result = format("%^s hurls a black bolt of nether at %s.", m_name, t_name);
			}
			mon_bolt(who, what, y, x, GF_NETHER, get_dam(spower, attack), result);
			break;
		}

		/* RF5_BOLT_MANA */
		case 128+25:
		{
			disturb(1, 0);
			if ((spower < 5) || (spower <= rlev / 10))
			{
				if (blind) result = format("%^s mumbles.", m_name);
				else result = format("%^s casts a magic missile at %s.", m_name, t_name);
			}
			else
			{
				if (blind) result = format("%^s murmurs deeply.", m_name);
				else result = format("%^s casts a mana bolt at %s.", m_name, t_name);
			}
			mon_bolt(who, what, y, x, GF_MANA, get_dam(spower, attack), result);
			break;
		}

		/* RF5_HOLY_ORB */
		case 128+26:
		{
			disturb(1, 0);
			if (spower < 40)
			{
				if (blind) result = format("%^s mumbles.", m_name);
				else result = format("%^s casts an orb of draining at %s.", m_name, t_name);
				rad = 1;
			}
			else if (spower < 90)
			{
				if (blind) result = format("%^s murmurs deeply.", m_name);
				else result = format("%^s casts a powerful orb of draining at %s.", m_name, t_name);
				rad = 2;
			}
			else
			{
				if (blind) result = format("%^s chants powerfully.", m_name);
				else result = format("%^s casts a large orb of holy might at %s.", m_name, t_name);
				rad = 3;
			}
			mon_ball(who, what, y, x, GF_HOLY_ORB, get_dam(spower, attack), rad, TRUE, result);
		}

		/* RF5_BEAM_ELEC */
		case 128+27:
		{
			disturb(1, 0);
			if (blind) msg_print("You feel a crackling in the air.");
			else result = format("%^s shoots a spark of lightning at %s.", m_name, t_name);

			mon_beam(who, what, y, x, GF_ELEC, get_dam(spower, attack), 10, result);
			break;
		}

		/* RF5_BEAM_ICE */
		case 128+28:
		{
			disturb(1, 0);
			if (spower < 50)
			{
				if (blind) result = format("%^s mumbles.", m_name);
				else result = format("%^s casts an icy lance at %s.", m_name, t_name);
			}
			else
			{
				if (blind) result = format("%^s murmurs deeply.", m_name);
				else result = format("%^s casts an icy lance at %s.", m_name, t_name);
			}
			mon_beam(who, what, y, x, GF_ICE, get_dam(spower, attack), 12, result);
			break;
		}

		/* RF5_BEAM_NETHR */
		case 128+29:
		{
			disturb(1, 0);
			if (spower < 25)
			{
				if (blind) result = format("%^s whispers nastily.", m_name);
				else result = format("%^s casts a beam of nether at %s.", m_name, t_name);
			}
			else if (spower < 50)
			{
				if (blind) result = format("%^s murmurs a deadly word.", m_name);
				else result = format("%^s hurls a nether lance at %s.", m_name, t_name);
			}
			else
			{
				if (blind) result = format("%^s intones with deadly menace.", m_name);
				else result = format("%^s unleashes a ray of death at %s.", m_name, t_name);
			}
			mon_beam(who, what, y, x, GF_NETHER, get_dam(spower, attack), 10, result);
			break;
		}

		/* RF5_ARC_HFIRE */
		case 128+30:
		{
			disturb(1, 0);
			/* Must be powerful to get an arc. */
			if (spower > 50)
			{
				if (blind) result = format("%^s speaks a word of peril.", m_name);
				else result = format("%^s invokes a hellfire blast around %s!", m_name, t_name);

				/* Absolutely formidable close up, less so at range. */
				mon_arc(who, what, y, x, GF_HELLFIRE, get_dam(7 * spower, 6), 6, 60, result);
			}
			else
			{
				if (blind) result = format("%^s murmurs darkly.", m_name);
				else result = format("%^s gestures, and %s %s enveloped in hellfire.", m_name, t_name, target < 0 ? "are" : "is");
				mon_ball(who, what, y, x, GF_HELLFIRE, get_dam(5 * spower, 6), 3, TRUE, result);
			}

			break;
		}

		/* RF5_ARC_FORCE */
		case 128+31:
		{
			disturb(1, 0);
			if (blind) result = format("%^s mutters.", m_name);
			else result = format("%^s calls up a wall of force around %s.", m_name, t_name);
			mon_arc(who, what, y, x, GF_FORCE, get_dam(3 * spower, 10), 8, 60, result);
			break;
		}

		/* RF6_HASTE */
		case 160+0:
		{
			if (target == 0) break;

			if (known)
			{
				disturb(1,0);

				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s concentrates on %s body.", m_name, t_poss);
			}

			if (target > 0)
			{
				/* Add to the monster haste counter */
				set_monster_haste(target, n_ptr->hasted + rlev + rand_int(rlev), n_ptr->ml);
			}
			else if (target < 0)
			{
				if (p_ptr->stastis) /* No effect */ ; 
				else if (p_ptr->fast) set_fast(p_ptr->fast + rand_int(rlev/3));
				else set_fast(p_ptr->fast + rlev);
			}

			break;
		}

		/* RF6_ADD_MANA */
		case 160+1:
		{
			if (target == 0) break;

			/* Druids/shamans sometimes add mana from water */
			if ((who > 0) && ((r_ptr->flags2 & (RF2_MAGE)) != 0) && ((r_ptr->flags2 & (RF2_PRIEST)) != 0))
			{
				if (rand_int(100) < 50)
				{
					/* Check to see if doing so would be worthwhile */
					int power = mon_concentrate_power(y, x, spower, FALSE, FALSE, concentrate_water_hook);
	
					/* We decided to concentrate light */
					if (power)
					{
						/* Message */
						if (blind) msg_format("%^s mumbles.", m_name);
						else msg_format("%^s absorbs mana from the surrounding water.", m_name);
	
						/* Big boost to mana */
						n_ptr->mana += (power / 5) + 1;
						if (n_ptr->mana > s_ptr->mana) n_ptr->mana = s_ptr->mana;
	
						/* Done */
						break;
					}
				}
				
				/* Benefit less from gaining mana any other way */
				spower /= 2;
			}
			
			if (known)
			{
				disturb(1,0);

				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s gathers power for %s.", m_name, t_name);
			}

			if (target > 0)
			{
				/* Increase current mana.  Do not exceed maximum. */
				n_ptr->mana += (spower / 15) + 1;
				if (n_ptr->mana > s_ptr->mana) n_ptr->mana = s_ptr->mana;
			}
			else if (target < 0)
			{
				/* Player mana is worth more */
				if (p_ptr->stastis) /* No effect */ ; 
				else if (p_ptr->csp < p_ptr->msp)
				{
					p_ptr->csp = p_ptr->csp + damroll((spower / 15) + 1,5);
					if (p_ptr->csp >= p_ptr->msp)
					{
						p_ptr->csp = p_ptr->msp;
						p_ptr->csp_frac = 0;
					}

					p_ptr->redraw |= (PR_MANA);
					p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
				}
			}
			break;
		}

		/* RF6_HEAL */
		case 160+2:
		{
			int gain, cost;

			if (target == 0) break;

			/* Druids/shamans sometimes add health from trees/plants */
			if ((who > 0) && (target == who) && ((r_ptr->flags2 & (RF2_MAGE)) != 0) && ((r_ptr->flags2 & (RF2_PRIEST)) != 0) && (rand_int(100) < 50))
			{
				/* Check to see if doing so would be worthwhile */
				int power = mon_concentrate_power(y, x, spower, FALSE, FALSE, concentrate_life_hook);

				/* We decided to concentrate life */
				if (power)
				{
					/* Message */
					if (blind) msg_format("%^s mumbles.", m_name);
					else msg_format("%^s absorbs life from the surrounding plants.", m_name);

					/* Big boost to mana */
					spower = MAX(spower, power);
				}
			}
			else if (known)
			{
				disturb(1,0);

				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s concentrates on %s wounds.", m_name, t_poss);
			}

			/* Heal up monsters */
			if (target > 0)
			{
				/* We regain lost hitpoints (up to spower * 3) */
				gain = MIN(n_ptr->maxhp - n_ptr->hp, spower * 3);
	
				/* We do not gain more than mana * 15 HPs at a time */
				gain = MIN(gain, m_ptr->mana * 15);

				/* Regain some hitpoints */
				n_ptr->hp += gain;

				/* Lose some mana (high-level monsters are more efficient) */
				cost = 1 + gain / (5 + 2 * rlev / 5);

				/* Reduce mana (do not go negetive) */
				m_ptr->mana -= MIN(cost, m_ptr->mana);

				/* Fully healed */
				if (n_ptr->hp >= n_ptr->maxhp)
				{
					/* Fully healed */
					n_ptr->hp = n_ptr->maxhp;

					/* Message */
					if (known)
					{
						if ((!blind) && (n_ptr->ml)) msg_format("%^s looks very healthy!",  t_nref);
						else msg_format("%^s sounds very healthy!", t_nref);
					}
				}

				/* Partially healed */
				else
				{
					/* Message */
					if (known)
					{
						if ((!blind) && (n_ptr->ml)) msg_format("%^s looks healthier.",  t_nref);
						else msg_format("%^s sounds healthier.", t_nref);
					}
				}

				/* Redraw (later) if needed */
				if ((p_ptr->health_who == target) && (n_ptr->ml))
					p_ptr->redraw |= (PR_HEALTH);

				/* Cancel fear */
				if (n_ptr->monfear)
				{
					/* Cancel fear */
					set_monster_fear(m_ptr, 0, FALSE);

					/* Message */
					if (n_ptr->ml)
						msg_format("%^s recovers %s courage.", t_nref, t_poss);
				}

				/* Recalculate combat range later */
				m_ptr->min_range = 0;
			}

			/* Heal up player */
			else if (target < 0)
			{
				/* We regain lost hitpoints */
				if (!p_ptr->stastis) hp_player(damroll(spower, 8));

				/* Lose some mana (high-level monsters are more efficient) */
				cost = 1 + spower * 3 / (5 + 2 * rlev / 5);

				/* Reduce mana (do not go negetive) */
				m_ptr->mana -= MIN(cost, m_ptr->mana);
			}

			break;
		}

		/* RF6_CURE */
		case 160+3:
		{
			if (target == 0) break;

			if (known)
			{
				disturb(1,0);

				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s concentrates on %s ailments.", m_name, t_poss);
			}

			/* Cure a monster */
			if (target > 0)
			{
				/* Cancel stunning */
				if (n_ptr->stunned)
				{
					/* Cancel stunning */
					n_ptr->stunned = 0;

					/* Message */
					if (n_ptr->ml) msg_format("%^s is no longer stunned.", t_nref);
				}

				/* Cancel fear */
				if (n_ptr->monfear)
				{
					/* Cancel fear */
					n_ptr->monfear = 0;

					/* Message */
					if (n_ptr->ml) msg_format("%^s recovers %s courage.", t_nref, t_poss);
				}

				/* Cancel confusion */
				if (n_ptr->confused)
				{
					/* Cancel fear */
					n_ptr->confused = 0;

					/* Message */
					if (n_ptr->ml) msg_format("%^s is no longer confused.", t_nref);
				}

				/* Cancel cuts */
				if (n_ptr->cut)
				{
					/* Cancel fear */
					n_ptr->cut = 0;

					/* Message */
					if (n_ptr->ml) msg_format("%^s is no longer bleeding.", t_nref);
				}

				/* Cancel confusion */
				if (n_ptr->poisoned)
				{
					/* Cancel fear */
					n_ptr->poisoned = 0;

					/* Message */
					if (n_ptr->ml) msg_format("%^s is no longer poisoned.", t_nref);
				}

				/* Cancel blindness */
				if (n_ptr->blind)
				{
					/* Cancel fear */
					n_ptr->blind = 0;

					/* Message */
					if (n_ptr->ml) msg_format("%^s is no longer blind.", t_nref);
				}

				/* Redraw (later) if needed */
				if (p_ptr->health_who == who) p_ptr->redraw |= (PR_HEALTH);
			}

			/* Cure the player */
			else if (target < 0)
			{
				k = 0;

				/* Hack -- always cure paralyzation first */
				if ((p_ptr->paralyzed) && !(p_ptr->stastis))
				{
					set_paralyzed(0);
					break;
				}

				/* Count ailments */
				if (p_ptr->cut) k++;
				if (p_ptr->stun) k++;
				if (p_ptr->poisoned) k++;
				if (p_ptr->blind) k++;
				if (p_ptr->afraid) k++;
				if (p_ptr->confused) k++;
				if (p_ptr->image) k++;
				if (p_ptr->food < PY_FOOD_WEAK) k++;

				/* High level monsters restore stats and experience if nothing worse ails the player */
				if ((rlev >= 30) && (!k))
				{
					if (p_ptr->stat_cur[A_STR] != p_ptr->stat_max[A_STR]) k++;
					if (p_ptr->stat_cur[A_INT] != p_ptr->stat_max[A_INT]) k++;
					if (p_ptr->stat_cur[A_WIS] != p_ptr->stat_max[A_WIS]) k++;
					if (p_ptr->stat_cur[A_DEX] != p_ptr->stat_max[A_DEX]) k++;
					if (p_ptr->stat_cur[A_CON] != p_ptr->stat_max[A_CON]) k++;
					if (p_ptr->stat_cur[A_CHR] != p_ptr->stat_max[A_CHR]) k++;
					if (p_ptr->stat_cur[A_AGI] != p_ptr->stat_max[A_AGI]) k++;
					if (p_ptr->stat_cur[A_SIZ] != p_ptr->stat_max[A_SIZ]) k++;
					if (p_ptr->exp < p_ptr->max_exp) k++;
				}

				/* Stastis */
				if (p_ptr->stastis) /* No effect */ ; 

				/* Pick a random ailment */
				else if (k)
				{
					/* Check what to restore - note paranoia checks */
					if ((p_ptr->cut) && (k) && (rand_int(k--))) { set_cut(0); set_stun(0); }
					else if ((p_ptr->stun) && (k) && (rand_int(k--))) { set_stun(0); set_cut(0); }
					else if ((p_ptr->poisoned) && (k) && (rand_int(k--))) set_poisoned(0);
					else if ((p_ptr->blind) && (k) && (rand_int(k--))) set_blind(0);
					else if ((p_ptr->afraid) && (k) && (rand_int(k--))) set_afraid(0);
					else if ((p_ptr->confused) && (k) && (rand_int(k--))) set_confused(0);
					else if ((p_ptr->image) && (k) && (rand_int(k--))) set_image(0);
					else if ((p_ptr->food < PY_FOOD_WEAK) && (k) && (rand_int(k--))) set_food(PY_FOOD_MAX -1);
					else if ((rlev < 30) || (k <= 0)) /* Nothing */ ;
					else if ((p_ptr->stat_cur[A_STR] != p_ptr->stat_max[A_STR]) && (k) && (rand_int(k--))) do_res_stat(A_STR);
					else if ((p_ptr->stat_cur[A_INT] != p_ptr->stat_max[A_INT]) && (k) && (rand_int(k--))) do_res_stat(A_INT);
					else if ((p_ptr->stat_cur[A_WIS] != p_ptr->stat_max[A_WIS]) && (k) && (rand_int(k--))) do_res_stat(A_WIS);
					else if ((p_ptr->stat_cur[A_DEX] != p_ptr->stat_max[A_DEX]) && (k) && (rand_int(k--))) do_res_stat(A_DEX);
					else if ((p_ptr->stat_cur[A_CON] != p_ptr->stat_max[A_CON]) && (k) && (rand_int(k--))) do_res_stat(A_CON);
					else if ((p_ptr->stat_cur[A_CHR] != p_ptr->stat_max[A_CHR]) && (k) && (rand_int(k--))) do_res_stat(A_CHR);
					else if ((p_ptr->stat_cur[A_AGI] != p_ptr->stat_max[A_AGI]) && (k) && (rand_int(k--))) do_res_stat(A_AGI);
					else if ((p_ptr->stat_cur[A_SIZ] != p_ptr->stat_max[A_SIZ]) && (k) && (rand_int(k--))) do_res_stat(A_SIZ);
					else if ((p_ptr->exp < p_ptr->max_exp) && (k) && (rand_int(k--))) restore_level();
				}

				/* Hack -- everything else cured, feed a hungry player */
				else if (p_ptr->food < PY_FOOD_ALERT) set_food(PY_FOOD_MAX -1);
			}

			break;
		}

		/* RF6_BLINK */
		case 160+4:
		{
			if (target > 0)
			{
				if ((known) || (direct)) disturb(1, 0);

				/* Get the target name (using "A"/"An") again. */
				monster_desc(t_name, sizeof(t_name), target, 0x08);

				teleport_away(target, 10);

				/*
				 * If it comes into view from around a corner (unlikely)
				 * give a message and learn about the casting
				 */
				if (!direct && m_ptr->ml)
				{
					/* Get the target name (using "A"/"An") again. */
					monster_desc(t_name, sizeof(t_name), target, 0x08);
										
					seen = TRUE;
					msg_format("%^s blinks into view.", t_nref);
				}

				/* Normal message */
				else if (direct)
				{
					msg_format("%^s blinks away.", t_nref);
				}

				/* Telepathic message */
				else if (known)
				{
					msg_format("%^s blinks.", t_nref);
				}
			}
			break;
		}

		/* RF6_TPORT */
		case 160+5:
		{
			if (target > 0)
			{
				if ((known) || (direct)) disturb(1, 0);

				/* Get the target name (using "A"/"An") again. */
				monster_desc(t_name, sizeof(t_name), target, 0x08);

				teleport_away(target, MAX_SIGHT * 2 + 5);

				/*
				 * If it comes into view from around a corner (unlikely)
				 * give a message and learn about the casting
				 */
				if (!direct && m_ptr->ml)
				{
					/* Get the target name (using "A"/"An") again. */
					monster_desc(t_name, sizeof(t_name), target, 0x08);

					seen = TRUE;
					msg_format("%^s teleports into view.", t_nref);
				}

				/* Normal message */
				else if (direct)
				{
					msg_format("%^s teleports away.", t_nref);
				}

				/* Telepathic message */
				else if (known)
				{
					msg_format("%^s teleports.", t_nref);
				}
			}
			break;
		}

		/* RF6_INVIS */
		case 160+6:
		{
			if (target == 0) break;

			if (known)
			{
				disturb(1,0);

				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s concentrates on %s appearance.", m_name, t_poss);
			}

			if (target > 0)
			{
				/* Add to the monster invisibility counter */
				n_ptr->tim_invis += n_ptr->tim_invis + rlev + rand_int(rlev);

				/* Notify player */
				if (n_ptr->ml)
				{
					/* Get the target name (using "A"/"An") again. */
					monster_desc(t_name, sizeof(t_name), target, 0x08);

					update_mon(target, FALSE);

					if (!n_ptr->ml)
					{
						msg_format("%^s disppears!", t_nref);
					}
				}
			}

			break;
		}

		/* RF6_TELE_SELF_TO */
		case 160+7:
		{
			if ((who > SOURCE_MONSTER_START) || (who == SOURCE_PLAYER_ALLY))
			{
				int old_cdis = m_ptr->cdis;

				/* Move monster near player (also updates "m_ptr->ml"). */
				teleport_towards(m_ptr->fy, m_ptr->fx, y, x);

				/* Monster is now visible, but wasn't before. */
				if ((!seen) && (m_ptr->ml))
				{
					/* Get the name (using "A"/"An") again. */
					monster_desc(m_name, sizeof(m_name), who > SOURCE_MONSTER_START ? who : what, 0x08);

					/* Message */
					msg_format("%^s suddenly appears.", m_name);
				}

				/* Monster was visible before, but isn't now. */
				else if ((seen) && (!m_ptr->ml))
				{
					/* Message */
					msg_format("%^s blinks away.", m_name);
				}

				/* Monster is visible both before and after. */
				else if ((seen) && (m_ptr->ml))
				{
					if (distance(m_ptr->fy, m_ptr->fx, p_ptr->py, p_ptr->px) < (old_cdis - 1))
					{
						msg_format("%^s blinks toward you.", m_name);
					}
					else
					{
						msg_format("%^s blinks.", m_name);
					}
				}

				/* Have we seen them at any point?  If so, we will learn about the spell. */
				if (m_ptr->ml) seen = TRUE;
			}

			break;
		}

		/* RF6_TELE_TO */
		case 160+8:
		{
			if ((who > SOURCE_MONSTER_START) && (target < 0))
			{
				if (!direct) break;
				disturb(1, 0);

				msg_format("%^s commands you to return.", m_name);

				if ((p_ptr->cur_flags4 & (TR4_ANCHOR)) || (room_has_flag(p_ptr->py, p_ptr->px, ROOM_ANCHOR)))
				{
					msg_print("You remain anchored in place.");
					if (!(room_has_flag(p_ptr->py, p_ptr->px, ROOM_ANCHOR))) player_can_flags(who, 0x0L, 0x0L, 0x0L, TR4_ANCHOR);
				}
				else
				{
					player_not_flags(who, 0x0L, 0x0L, 0x0L, TR4_ANCHOR);
					teleport_player_to(m_ptr->fy, m_ptr->fx);
				}
			}
		}

		/* RF6_TELE_AWAY */
		case 160+9:
		{
			if (target < 0)
			{
				if (!direct) break;
				disturb(1, 0);

				if ((p_ptr->cur_flags4 & (TR4_ANCHOR)) || (room_has_flag(p_ptr->py, p_ptr->px, ROOM_ANCHOR)))
				{
					msg_format("%^s fails to teleport you away.", m_name);
					if (!(room_has_flag(p_ptr->py, p_ptr->px, ROOM_ANCHOR))) player_can_flags(who, 0x0L, 0x0L, 0x0L, TR4_ANCHOR);
				}
				else
				{
					player_not_flags(who, 0x0L, 0x0L, 0x0L, TR4_ANCHOR);
					teleport_hook = NULL;
					teleport_player(100);
				}
			}
			else if (target > 0)
			{
				if (!(s_ptr->flags9 & (RF9_RES_TPORT)))
				{
					disturb(1, 0);
					msg_format("%^s teleports %s away.", m_name, t_name);
					teleport_hook = NULL;
					teleport_away(target, MAX_SIGHT * 2 + 5);
				}
			}
			break;
		}

		/* RF6_TELE_LEVEL */
		case 160+10:
		{
			if (target < 0)
			{
				if (!direct) break;
				disturb(1, 0);
				if ((blind) && (known)) msg_format("%^s mumbles strangely.", m_name);
				else if (known) msg_format("%^s gestures at your feet.", m_name);

				if ((p_ptr->cur_flags2 & (TR2_RES_NEXUS)) || (p_ptr->cur_flags4 & (TR4_ANCHOR)))
				{
					msg_print("You are unaffected!");

					/* Always notice */
					if ((p_ptr->cur_flags2 & (TR2_RES_NEXUS))) player_can_flags(who, 0x0L,TR2_RES_NEXUS,0x0L,0x0L);
					if ((p_ptr->cur_flags4 & (TR4_ANCHOR))) player_can_flags(who, 0x0L,0x0L,0x0L,TR4_ANCHOR);
				}
				else if (rand_int(100) < p_ptr->skill_sav)
				{
					msg_print("You resist the effects!");
					
					if (who > 0) update_smart_save(who, TRUE);
				}
				else
				{
					teleport_player_level();

					/* Always notice */
					if (!player_not_flags(who, 0x0L,TR2_RES_NEXUS,0x0L,TR4_ANCHOR) && who)
					{
						update_smart_save(who, FALSE);
					}
				}
			}
			break;
		}

		/* RF6_WRAITHFORM */
		case 160+11:
		{
			if (target == 0) break;

			if (known)
			{
				disturb(1,0);

				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s concentrates on %s body.", m_name, m_poss);
			}

			if (target > 0)
			{
				/* Notify player */
				if ((n_ptr->ml) && !(n_ptr->tim_passw))
				{
					msg_format("%^s becomes more insubstantial!", t_nref);
				}

				/* Add to the monster haste counter */
				n_ptr->tim_passw += n_ptr->tim_passw + rlev + rand_int(rlev);
			}
			else if (target < 0)
			{
				/* Nothing, yet */
			}

			break;
		}

		/* RF6_DARKNESS */
		case 160+12:
		{
			if (!direct) break;
			if (target < 0) disturb(1, 0);

			if (who > SOURCE_MONSTER_START)
			{
				if ((blind) && (known)) msg_format("%^s mumbles.", m_name);
				else if (known) msg_format("%^s gestures in shadow.", m_name);
			}

			/* Hack -- Message */
			if (!((blind) && (known)) && (target < 0))
			{
				msg_print("Darkness surrounds you.");
			}

			/* Hook into the "project()" function */
			(void)project(who, what, 3, y, x, y, x, 0, GF_DARK_WEAK, FLG_MON_BALL, 0, 0);

			/* Lite up the room */
			unlite_room(y, x);

			break;
		}

		/* RF6_TRAPS */
		case 160+13:
		{
			int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;

			if (!direct) break;
			if (target < 0) disturb(1, 0);

			sound(MSG_CREATE_TRAP);
			if (who > SOURCE_MONSTER_START)
			{
				if (((blind) && (known)) && (target < 0)) msg_format("%^s mumbles, and then cackles evilly.", m_name);
				else if ((target < 0) || ((target ==0) && (known))) msg_format("%^s casts a spell and cackles evilly.", m_name);
				else if (known) msg_format("%^s casts a spell at %s and cackles evilly.",m_name,t_name);
			}

			(void)project(who, what, 1, y, x, y, x, 0, GF_MAKE_TRAP, flg, 0, 0);

			break;
		}

		/* RF6_FORGET */
		case 160+14:
		{
			if (!direct) break;
			if (target >=0) break;
			disturb(1, 0);
			msg_format("%^s tries to blank your mind.", m_name);

			if (p_ptr->stastis) /* No effect */ ; 
			else if (rand_int(100) < (powerful ? p_ptr->skill_sav * 2 / 3 : p_ptr->skill_sav))
			{
				msg_print("You resist the effects!");
				
				if (who > 0) update_smart_save(who, TRUE);
			}
			else
			{
				(void)set_amnesia(p_ptr->amnesia + rlev / 8 + 4 + rand_int(4));
				
				msg_print("Your memories fade.");

				if (who > 0) update_smart_save(who, FALSE);
			}
			break;
		}

		/* RF6_DRAIN_MANA */
		case 160+15:
		{
			int r1 = 0;

			if (!direct) break;
			if (target == 0) break;

			if ((target < 0) && !(p_ptr->stastis))
			{
				if (p_ptr->csp)
				{	
					/* Disturb if legal */
					disturb(1, 0);
	
					/* Basic message */
					msg_format("%^s draws psychic energy from you!", m_name);
	
					/* Attack power */
					r1 = (randint(spower) / 20) + 1;
	
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
					p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
				}
				else break;
			}
			else if (target > 0)
			{
				if (n_ptr->mana > 0)
				{
					/* Basic message */
					msg_format("%^s draws psychic energy from %s.", m_name, t_name);
	
					r1 = (randint(spower) / 20) + 1;
	
					/* Full drain */
					r1 = MIN(r1, n_ptr->mana);
					
					n_ptr->mana -= r1;
				}
				else break;
			}

			/* Replenish monster mana */
			if ((who > SOURCE_MONSTER_START) && (m_ptr->mana < r_ptr->mana) && (r1))
			{
				if ( r1 > r_ptr->mana - m_ptr->mana)
				{
					 r1 -= r_ptr->mana - m_ptr->mana;
					 m_ptr->mana = r_ptr->mana;
				}
				else
				{
					 m_ptr->mana += r1;
					 r1 = 0;
				}
			}

			/* Heal the monster with remaining energy */
			if ((who > SOURCE_MONSTER_START) && (m_ptr->hp < m_ptr->maxhp) && (r1))
			{
				/* Heal */
				m_ptr->hp += (30 * (r1 + 1));
				if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

				/* Redraw (later) if needed */
				if (p_ptr->health_who == who) p_ptr->redraw |= (PR_HEALTH);

				/* Special message */
				if (seen)
				{
					msg_format("%^s appears healthier.", m_name);
				}
			}
			
			/* Inform allies of new player mana */
			if ((who > 0) && (target > 0))
			{
				if (p_ptr->csp)
				{
					update_smart_forget(who, SM_IMM_MANA);
				}
				else
				{
					update_smart_learn(who, SM_IMM_MANA);
				}
			}
			break;
		}

		/* RF6_CURSE */
		case 160+16:
		{
			if (!direct) break;
			if (target < 0) disturb(1, 0);

			if ((blind) && (known)) msg_format("%^s curses %s.", m_name, t_name);
			else msg_format("%^s points at %s and curses.", m_name, t_name);

			(void)project(who, what, 0, m_ptr->fy, m_ptr->fx, y, x, get_dam(spower, attack), GF_CURSE, FLG_MON_DIRECT, 0, 0);

			break;
		}

		/* RF6_DISPEL */
		case 160+17:
		{
			if (!direct) break;
			if (target < 0) disturb(1, 0);
			if ((who > SOURCE_MONSTER_START) && (!seen))
			{
				if (target < 0) msg_print("There is a static feeling in the air.");
			}
			else if (who > SOURCE_MONSTER_START)
			{
				msg_format("%^s dispels %s magic.", m_name, t_name);
			}

			(void)project(who, what, 0, m_ptr->fy, m_ptr->fx, y, x, rlev, GF_DISPEL, FLG_MON_DIRECT, 0, 0);

			break;
		}

		/* RF6_MIND_BLAST */
		case 160+18:
		{
			if (!direct) break;
			if (target < 0) disturb(1, 0);
			if ((target < 0) && (!seen)) msg_print("You feel something focusing on your mind.");
			else msg_format("%^s gazes deep into %s eyes.", m_name, t_poss);

			if ((target < 0) && !(p_ptr->stastis))
			{
				if (rand_int(100) < p_ptr->skill_sav)
				{
					msg_print("You resist the effects!");
					
					if (who > 0) update_smart_save(who, TRUE);
				}
				else
				{
					msg_print("Your mind is blasted by psionic energy.");
					if ((p_ptr->cur_flags2 & (TR2_RES_CONFU)) == 0)
					{
						(void)set_confused(p_ptr->confused + rand_int(4) + 4);
						if (!player_not_flags(who, 0x0L, TR2_RES_CONFU, 0x0L, 0x0L) && who)
						{
							update_smart_save(who, FALSE);
						}
					}
					else
					{
						if (!player_can_flags(who, 0x0L, TR2_RES_CONFU, 0x0L, 0x0L) && who)
						{
							update_smart_save(who, FALSE);
						}
					}
					
					/* Apply damage directly */
					project_p(who, what, y, x, get_dam(spower, attack), GF_HURT);
				}
			}
			else if (target > 0)
			{
				if (!(r_ptr->flags2 & (RF2_EMPTY_MIND)))
				{
					if (known) msg_format ("&^s mind is blasted by psionic energy.",t_poss);

					/* Hack --- Use GF_CONFUSION */
					project_m(who, what, y, x, get_dam(spower, attack), GF_CONFUSION);

					/* Hack --- Use GF_CONFUSION */
					project_t(who, what, y, x, get_dam(spower, attack), GF_CONFUSION);
				}
				else if (n_ptr->ml)
				{
					k_ptr->flags2 |= (RF2_EMPTY_MIND);
				}
			}
			break;
		}

		/* RF6_ILLUSION */
		case 160+19:
		{
			if (!direct) break;
			if (target < 0) disturb(1, 0);

			if (known) sound(MSG_CAST_FEAR);
			if (who > SOURCE_MONSTER_START)
			{
				if (((blind) && (known)) && (target < 0)) msg_format("%^s mumbles, and you hear deceptive noises.", m_name);
				else if ((blind) && (known)) msg_format("%^s mumbles.",m_name);
				else if ((target < 0) || ((target ==0) && (known))) msg_format("%^s casts a deceptive illusion.", m_name);
				else if (known) msg_format("%^s casts a deceptive illusion at %s.",m_name,t_name);
			}

			if ((target < 0) && !(p_ptr->stastis))
			{
				if (p_ptr->cur_flags2 & (TR2_RES_CHAOS))
				{
					if (powerful && (rand_int(100) > p_ptr->skill_sav))
					{
						msg_format("%^s power overcomes your resistance.", m_poss);

						(void)set_image(p_ptr->image + rand_int(4) + 1);
					}
					else msg_print("You refuse to be deceived.");

					/* Sometimes notice */
					player_can_flags(who, 0x0L,TR2_RES_CHAOS,0x0L,0x0L);
				}
				else if (rand_int(100) < (powerful ? p_ptr->skill_sav * 2 / 3 : p_ptr->skill_sav))
				{
					msg_print("You refuse to be deceived.");

					if (who > 0) update_smart_save(who, TRUE);				
				}
				else
				{
					(void)set_image(p_ptr->image + rand_int(4) + 4);

					/* Always notice */
					if (!player_not_flags(who, 0x0L,TR2_RES_CHAOS,0x0L,0x0L) && who)
					{
						update_smart_save(who, FALSE);
					}
				}
			}
			else if (target > 0)
			{
				/* Hack --- Use GF_HALLU */
				project_m(who, what, y, x, rlev, GF_HALLU);

				/* Hack --- Use GF_HALLU */
				project_t(who, what, y, x, rlev, GF_HALLU);
			}
			break;
		}

		/* RF6_WOUND */
		case 160+20:
		{
			if (!direct) break;
			if (target < 0) disturb(1, 0);

			if (spower < 4)
			{
				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s points at %s and curses.", m_name, t_name);
				k = 1;
			}
			else if (spower < 10)
			{
				if (blind) msg_format("%^s mumbles deeply.", m_name);
				else msg_format("%^s points at %s and curses horribly.", m_name, t_name);
				k = 2;
			}
			else if (spower < 20)
			{
				if (blind) msg_format("%^s murmurs loudly.", m_name);
				else msg_format("%^s points at %s, incanting terribly.", m_name, t_name);
				k = 3;
			}
			else if (spower < 35)
			{
				if (blind) msg_format("%^s cries out wrathfully.", m_name);
				else msg_format("%^s points at %s, screaming words of peril!", m_name, t_name);
				k = 4;
			}
			else
			{
				if (blind) msg_format("%^s screams the word 'DIE!'", m_name);
				else msg_format("%^s points at %s, screaming the word DIE!", m_name, t_name);
				k = 5;
			}

			if ((target < 0) && !(p_ptr->stastis))
			{
				if (rand_int(rlev / 2 + 70) < p_ptr->skill_sav)
				{
					msg_format("You resist the effects%c",
					      (spower < 30 ?  '.' : '!'));

					if (who > 0) update_smart_save(who, TRUE);				
				}
				else
				{
					/*
					 * Inflict damage. Note this spell has a hack
					 * that handles damage differently in get_dam.
					 */
					project_p(who, what, y, x, get_dam(spower, attack), GF_HURT);

					/* Cut the player depending on strength of spell. */
					if (k == 1) (void)set_cut(p_ptr->cut + 8 + damroll(2, 4));
					if (k == 2) (void)set_cut(p_ptr->cut + 23 + damroll(3, 8));
					if (k == 3) (void)set_cut(p_ptr->cut + 46 + damroll(4, 12));
					if (k == 4) (void)set_cut(p_ptr->cut + 95 + damroll(8, 15));
					if (k == 5) (void)set_cut(1200);
					
					if (who > 0) update_smart_save(who, FALSE);
				}
			}
			else if (target > 0)
			{
				int cut = 0;

				/* Cut the monster depending on strength of spell. */
				if (k == 1) cut = 8 + damroll(2, 4);
				if (k == 2) cut = 23 + damroll(3, 8);
				if (k == 3) cut = 46 + damroll(4, 12);
				if (k == 4) cut = 95 + damroll(8, 15);
				if (k == 5) cut = 1200;

				/* Adjust for monster level */
				cut /= r_ptr->level / 10 + 1;

				if (who < 0)
				{
					/* Hack --- Use GF_DRAIN_LIFE */
					project_m(who, what, y, x, damroll(8,8), GF_DRAIN_LIFE);

					/* Hack --- Use GF_DRAIN_LIFE */
					project_t(who, what, y, x, damroll(8,8), GF_DRAIN_LIFE);

					/* Hack -- player can cut monsters */
					if ((s_ptr->flags3 & (RF3_NONLIVING)) == 0) n_ptr->cut = MIN(255, n_ptr->cut + cut);
				}
				else
				{
					/* Hack -- monsters only do damage, not cuts, to each other */
					project_m(who, what, y, x, damroll(8,8) + cut, GF_DRAIN_LIFE);

					/* Hack -- monsters only do damage, not cuts, to each other */
					project_t(who, what, y, x, damroll(8,8) + cut, GF_DRAIN_LIFE);
				}
			}

			break;
		}

		/* RF6_BLESS */
		case 160+21:
		{
			if (target == 0) break;

			if (known)
			{
				disturb(1,0);

				if (blind)
				{
					msg_format("%^s mumbles.", m_name);
				}
				else if (who != SOURCE_SELF)
				{
					msg_format("%^s blesses %s.", m_name, t_name);
				}
				else
				{
					msg_format("%^s invokes %s diety.", m_name, m_poss);
				}
			}

			if (target > 0)
			{
				/* Notify player */
				if ((n_ptr->ml) && !(n_ptr->bless))
				{
					msg_format("%^s appears righteous!", t_nref);
				}

				/* Add to the monster bless counter */
				n_ptr->bless += n_ptr->bless + rlev + rand_int(rlev);
			}
			else if ((target < 0) && !(p_ptr->stastis))
			{
				/* Set blessing */
				set_blessed(p_ptr->blessed + rlev);
			}

			break;
		}

		/* RF6_BERSERK */
		case 160+22:
		{
			if (target == 0) break;

			if (known)
			{
				disturb(1,0);

				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s whips %s up into a frenzy.", m_name, t_name);
			}

			if (target > 0)
			{
				/* Notify player */
				if ((n_ptr->ml) && !(n_ptr->berserk))
				{
					msg_format("%^s goes berserk!", t_nref);
				}

				/* Add to the monster haste counter */
				n_ptr->berserk += n_ptr->berserk + rlev + rand_int(rlev);
			}
			else if ((target < 0) && !(p_ptr->stastis))
			{
				/* Set blessing */
				set_shero(p_ptr->shero + rlev);
			}

			break;
		}

		/* RF6_SHIELD */
		case 160+23:
		{
			if (target == 0) break;

			if (known)
			{
				disturb(1,0);

				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s concentrates on the air around %s.", m_name, t_name);
			}

			if (target > 0)
			{
				/* Notify player */
				if ((n_ptr->ml) && !(n_ptr->shield))
				{
					msg_format("%^s becomes magically shielded.", t_nref);
				}

				/* Add to the monster shield counter */
				n_ptr->shield += n_ptr->shield + rlev + rand_int(rlev);
			}
			else if ((target < 0) && !(p_ptr->stastis))
			{
				/* Set blessing */
				set_shield(p_ptr->shield + rlev);
			}

			break;
		}

		/* RF6_OPPOSE_ELEM */
		case 160+24:
		{
			if (target == 0) break;

			if (known)
			{
				disturb(1,0);

				if (blind) msg_format("%^s mumbles.", m_name);
				else msg_format("%^s concentrates on %s body.", m_name, t_poss);
			}

			if (target > 0)
			{
				/* Notify player */
				if ((n_ptr->ml) && !(n_ptr->oppose_elem))
				{
					msg_format("%^s becomes temporarily resistant to the elements.", t_nref);
				}

				/* Add to the monster haste counter */
				n_ptr->oppose_elem += n_ptr->oppose_elem + rlev + rand_int(rlev);
			}
			else if ((target < 0) && !(p_ptr->stastis))
			{
				/* Set opposition */
				set_oppose_acid(p_ptr->oppose_acid + rlev);
				set_oppose_cold(p_ptr->oppose_cold + rlev);
				set_oppose_elec(p_ptr->oppose_elec + rlev);
				set_oppose_fire(p_ptr->oppose_fire + rlev);
				set_oppose_pois(p_ptr->oppose_pois + rlev);
			}

			break;
		}

		/* RF6_HUNGER */
		case 160+25:
		{
			if (!direct) break;
			if (target == 0) break;

			if (blind) msg_format("%^s %s commanded to feel hungry.", t_nref, target < 0 ? "are" : "is");
			else msg_format("%^s gestures at %s, and commands that %s feel%s hungry.", m_name, t_name, t_name, target < 0 ? "" : "s");

			if (target < 0)
			{
				if (p_ptr->stastis) /* No effect */;
				else if (rand_int(rlev / 2 + 70) > p_ptr->skill_sav)
				{
					/* Reduce food abruptly.  */
					(void)set_food(p_ptr->food - (p_ptr->food/4));
					
					if (who > 0) update_smart_save(who, FALSE);
				}
				else
				{
					msg_print ("You resist the effects!");

					if (who > 0) update_smart_save(who, TRUE);
				}
			}
			else if ((target > 0) && ((r_info[n_ptr->r_idx].flags3 & (RF3_NONLIVING)) == 0))
			{
				/* Hack -- use strength to reflect hunger */
				if (n_ptr->mflag & (MFLAG_STRONG)) m_ptr->mflag &= ~(MFLAG_STRONG);
				else if (m_ptr->mflag & (MFLAG_WEAK))
				{
					/* Hack --- Use GF_HURT */
					project_m(who, what, y, x, get_dam(spower, attack), GF_HURT);
				}
				else m_ptr->mflag |= (MFLAG_WEAK);
			}

			break;
		}

		/* RF6_PROBE */
		case 160+26:
		{
			if (!direct) break;
			if ((who <= 0) || (target >= 0)) break;

			msg_format("%^s probes your weaknesses.", m_name);

			if (who > SOURCE_MONSTER_START)
			{
				update_smart_cheat(who);
			}

			break;
		}

		/* RF6_SCARE */
		case 160+27:
		{
			if (!direct) break;
			if (target < 0) disturb(1, 0);

			if (known) sound(MSG_CAST_FEAR);
			if (((blind) && (known)) && (target < 0)) msg_format("%^s mumbles, and you hear scary noises.", m_name);
			else if ((blind) && (known)) msg_format("%^s mumbles.",m_name);
			else if ((target < 0) || ((target ==0) && (known))) msg_format("%^s casts a fearful illusion.", m_name);
			else if (known) msg_format("%^s casts a fearful illusion at %s.",m_name,t_name);

			if ((target < 0) && !(p_ptr->stastis))
			{
				if ((p_ptr->cur_flags2 & (TR2_RES_FEAR)) != 0)
				{
					if (powerful && (rand_int(100) > p_ptr->skill_sav))
					{
						msg_format("%^s power overcomes your resistance.", m_poss);

						(void)set_afraid(p_ptr->afraid + rand_int(4) + 1);
						
						/* Always notice */
						if (!player_can_flags(who, 0x0L,TR2_RES_FEAR,0x0L,0x0L) && who)
						{
							update_smart_save(who, FALSE);
						}
					}
					else
					{
						msg_print("You refuse to be frightened.");

						/* Always notice */
						if (!player_can_flags(who, 0x0L,TR2_RES_FEAR,0x0L,0x0L) && who)
						{
							update_smart_save(who, TRUE);
						}
					}
				}
				else if (rand_int(100) < (powerful ? p_ptr->skill_sav * 2 / 3 : p_ptr->skill_sav))
				{
					msg_print("You refuse to be frightened.");
					if (who > 0) update_smart_save(who, TRUE);
				}
				else
				{
					(void)set_afraid(p_ptr->afraid + rand_int(4) + 4);

					/* Always notice */
					if (!player_not_flags(who, 0x0L,TR2_RES_FEAR,0x0L,0x0L) && who)
					{
						update_smart_save(who, FALSE);
					}
				}
			}
			else if (target > 0)
			{
				/* Hack --- Use GF_TERRIFY */
				project_m(who, what, y, x, rlev, GF_TERRIFY);

				/* Hack --- Use GF_TERRIFY */
				project_t(who, what, y, x, rlev, GF_TERRIFY);
			}
			break;
		}

		/* RF6_BLIND */
		case 160+28:
		{
			if (!direct) break;
			if (target < 0) disturb(1, 0);

			if ((blind) && (known)) msg_format("%^s mumbles.", m_name);
			else if (known) msg_format("%^s casts a spell, burning %s eyes.", m_name, t_poss);

			if ((target < 0) && !(p_ptr->stastis))
			{
				if ((p_ptr->cur_flags2 & (TR2_RES_BLIND)) != 0)
				{
					if (powerful && (rand_int(100) > p_ptr->skill_sav))
					{
						msg_format("%^s power overcomes your resistance.", m_poss);

						(void)set_blind(p_ptr->blind + rand_int(6) + 1);

						/* Always notice */
						if (!player_can_flags(who, 0x0L,TR2_RES_BLIND,0x0L,0x0L) && who)
						{
							update_smart_save(who, FALSE);
						}
					}
					else
					{
						msg_print("You are unaffected!");
	
						/* Always notice */
						if (!player_can_flags(who, 0x0L,TR2_RES_BLIND,0x0L,0x0L) && who)
						{
							update_smart_save(who, TRUE);
						}
					}
				}
				else if (rand_int(100) < (powerful ? p_ptr->skill_sav * 2 / 3 : p_ptr->skill_sav))
				{
					msg_print("You resist the effects!");
					
					if (who > 0) update_smart_save(who, TRUE);
				}
				else
				{
					(void)set_blind(p_ptr->blind + 12 + rlev / 4 + rand_int(4));

					/* Always notice */
					if (!player_not_flags(who, 0x0L,TR2_RES_BLIND,0x0L,0x0L) && who)
					{
						update_smart_save(who, FALSE);
					}
				}
			}
			else if (target > 0)
			{
				/* Hack --- Use GF_CONF_WEAK */
				project_m(who, what, y, x, rlev, GF_CONF_WEAK);

				/* Hack --- Use GF_CONF_WEAK */
				project_t(who, what, y, x, rlev, GF_CONF_WEAK);
			}
			break;
		}

		/* RF6_CONF */
		case 160+29:
		{
			if (!direct) break;
			if (target < 0) disturb(1, 0);

			if (((blind) && (known)) && (target < 0)) msg_format("%^s mumbles, and you hear puzzling noises.", m_name);
			else if ((blind) && (known)) msg_format ("%^s mumbles.",m_name);
			else if ((target < 0) || ((target == 0) && (known))) msg_format("%^s casts a mesmerising illusion.", m_name);
			else if (known) msg_format("%^s creates a mesmerising illusion for %s.", m_name, t_name);

			if ((target < 0) && !(p_ptr->stastis))
			{
				if ((p_ptr->cur_flags2 & (TR2_RES_CONFU)) != 0)
				{
					if (powerful && (rand_int(100) > p_ptr->skill_sav))
					{
						msg_format("%^s power overcomes your resistance.", m_poss);

						(void)set_confused(p_ptr->confused + rand_int(5) + 1);

						/* Always notice */
						if (!player_can_flags(who, 0x0L,TR2_RES_CONFU,0x0L,0x0L) && who)
						{
							update_smart_save(who, FALSE);
						}
					}
					else
					{
						msg_print("You disbelieve the feeble spell.");
	
						/* Always notice */
						if (!player_can_flags(who, 0x0L,TR2_RES_CONFU,0x0L,0x0L) && who)
						{
							update_smart_save(who, TRUE);
						}
					}
				}
				else if (rand_int(100) < (powerful ? p_ptr->skill_sav * 2 / 3 : p_ptr->skill_sav))
				{
					msg_print("You disbelieve the feeble spell.");
					if (who > 0) update_smart_save(who, TRUE);
				}
				else
				{
					(void)set_confused(p_ptr->confused + rlev / 8 + 4 + rand_int(4));

					/* Always notice */
					if (!player_not_flags(who, 0x0L,TR2_RES_CONFU,0x0L,0x0L) && who)
					{
						update_smart_save(who, FALSE);
					}
				}
			}
			else if (target > 0)
			{
				/* Hack --- Use GF_CONF_WEAK */
				project_m(who, what, y, x, rlev, GF_CONF_WEAK);

				/* Hack --- Use GF_CONF_WEAK */
				project_t(who, what, y, x, rlev, GF_CONF_WEAK);
			}
			break;
		}

		/* RF6_SLOW */
		case 160+30:
		{
			if (!direct) break;
			if (target < 0) disturb(1, 0);

			if (((blind) && (known)) && (target < 0)) msg_format("%^s drains power from your muscles.", m_name);
			else if ((blind) && (known)) msg_format ("%^s mumbles.",m_name);
			else if (known) msg_format("%^s drains power from %s muscles.", m_name, t_poss);

			if ((target < 0) && !(p_ptr->stastis))
			{
				if (((p_ptr->cur_flags3 & (TR3_FREE_ACT)) != 0) || (p_ptr->free_act))
				{
					if (powerful && (rand_int(100) > p_ptr->skill_sav))
					{
						msg_format("%^s power overcomes your resistance.", m_poss);

						(void)set_slow(p_ptr->slow + rand_int(3) + 1);

						/* Player is temporarily resistant */
						if (p_ptr->free_act)
						{
							update_smart_save(who, FALSE);
						}
						/* Always notice */
						else if (!player_can_flags(who, 0x0L,0x0L,TR3_FREE_ACT,0x0L) && who)
						{
							update_smart_save(who, FALSE);
						}
					}
					else
					{
						msg_print("You are unaffected!");
						
						/* Player is temporarily resistant */
						if (p_ptr->free_act)
						{
							update_smart_save(who, TRUE);
						}
						/* Always notice */
						else if (!player_can_flags(who, 0x0L,0x0L,TR3_FREE_ACT,0x0L) && who)
						{
							update_smart_save(who, TRUE);
						}
					}
				}
				else if (rand_int(100) < (powerful ? p_ptr->skill_sav * 2 / 3 : p_ptr->skill_sav))
				{
					msg_print("You resist the effects!");
					if (who > 0) update_smart_save(who, TRUE);
				}
				else
				{
					(void)set_slow(p_ptr->slow + rand_int(4) + 4 + rlev / 25);

					/* Always notice */
					if (!player_not_flags(who, 0x0L,0x0L,TR3_FREE_ACT,0x0L) && who)
					{
						update_smart_save(who, FALSE);
					}
				}
			}
			else if (target > 0)
			{
				/* Hack --- Use GF_SLOW_WEAK */
				project_m(who, what, y, x, rlev, GF_SLOW_WEAK);

				/* Hack --- Use GF_SLOW_WEAK */
				project_t(who, what, y, x, rlev, GF_SLOW_WEAK);
			}
			break;
		}

		/* RF6_HOLD */
		case 160+31:
		{
			if (!direct) break;
			if (target < 0) disturb(1, 0);

			if ((blind) && (known)) msg_format ("%^s mumbles.",m_name);
			else if (known) msg_format("%^s stares deeply into %s muscles.", m_name, t_poss);

			if ((target < 0) && !(p_ptr->stastis))
			{
				if (((p_ptr->cur_flags3 & (TR3_FREE_ACT)) != 0) || (p_ptr->free_act))
				{
					if (powerful && (rand_int(100) > p_ptr->skill_sav))
					{
						msg_format("%^s power overcomes your resistance.", m_poss);

						if (!p_ptr->slow) (void)set_slow(p_ptr->slow + rand_int(4) + 1);
						else set_paralyzed(p_ptr->paralyzed + 1);

						/* Player is temporarily resistant */
						if (p_ptr->free_act)
						{
							update_smart_save(who, FALSE);
						}
						/* Always notice */
						else if (!player_can_flags(who, 0x0L,0x0L,TR3_FREE_ACT,0x0L) && who)
						{
							update_smart_save(who, FALSE);
						}						
					}
					else
					{
						msg_print("You are unaffected!");
						
						/* Player is temporarily resistant */
						if (p_ptr->free_act)
						{
							update_smart_save(who, TRUE);
						}
						/* Always notice */
						else if (!player_can_flags(who, 0x0L,0x0L,TR3_FREE_ACT,0x0L) && who)
						{
							update_smart_save(who, TRUE);
						}
					}
				}
				else if (rand_int(100) < (powerful ? p_ptr->skill_sav * 2 / 3 : p_ptr->skill_sav))
				{
					msg_print("You resist the effects!");
					if (who > 0) update_smart_save(who, TRUE);
				}
				else
				{
					(void)set_paralyzed(p_ptr->paralyzed + rand_int(4) + 4);

					/* Always notice */
					if (!player_not_flags(who, 0x0L,0x0L,TR3_FREE_ACT,0x0L) && who)
					{
						update_smart_save(who, FALSE);
					}
				}
			}
			else if (target > 0)
			{
				/* Hack --- Use GF_SLEEP */
				project_m(who, what, y, x, rlev, GF_SLEEP);

				/* Hack --- Use GF_SLEEP */
				project_t(who, what, y, x, rlev, GF_SLEEP);
			}
			break;
		}

		/* RF7_S_KIN */
		case 192 + 0:
		{
			if (surface) break;
			disturb(1, 0);

			if ((who > SOURCE_MONSTER_START) || (who == SOURCE_PLAYER_ALLY) || (who == SOURCE_SELF))
			{
				if (((blind) && (known)) && (target < 0)) msg_format("%^s cries out for help.", m_name);
				else if (known) msg_format("%^s magically summons %s %s.", m_name, m_poss,
						((r_ptr->flags1) & RF1_UNIQUE ?
						 "minions" : "kin"));
				else msg_print("You hear distant cries for help.");

				/* Hack -- Set the letter of the monsters to summon */
				summon_char_type = r_ptr->d_char;

				/* Hack -- prevent summoning for a short while */
				m_ptr->summoned = 20;
			}
			else
			{
				/* MegaHack -- Determine letter later */
				summon_char_type = '\0';
			}

			/* Count them for later */
			for (k = 0; k < 6; k++)
			{
				count += summon_specific(y, x, summoner, rlev - 1, SUMMON_KIN, TRUE, allies);
			}

			break;
		}

		/* RF7_R_KIN */
		case 192 + 1:
		{
			/* Override cave ecology */
			cave_ecology.ready = FALSE;

			if (surface) break;
			disturb(1, 0);
			if ((who > SOURCE_MONSTER_START) || (who == SOURCE_PLAYER_ALLY) || (who == SOURCE_SELF))
			{
				if (((blind) && (known)) && (target < 0)) result = format("%^s whispers.", m_name);
				else if (known) result = format("%^s magically reanimates %s %s from %s.", m_name, m_poss,
						((r_ptr->flags1) & RF1_UNIQUE ? "minions" : "kin"),
						((r_ptr->flags3) & RF3_NONLIVING ? "spare parts" : "death"));
				else result = "You hear distant whispering.";

				/* Hack -- Set the letter of the monsters to summon */
				summon_char_type = r_ptr->d_char;
			}
			else
			{
				/* MegaHack -- Determine letter later */
				summon_char_type = '\0';
			}

			/* Raise the dead */
			mon_ball(who, what, y, x, GF_RAISE_DEAD, 0, 3, TRUE, result);
			break;
		}


		/* RF7_A_DEAD */
		case 192 + 2:
		{
			/* Override cave ecology */
			cave_ecology.ready = FALSE;

			if (surface) break;
			disturb(1, 0);
			if ((who > SOURCE_MONSTER_START) || (who == SOURCE_PLAYER_ALLY) || (who == SOURCE_SELF))
			{
				if (((blind) && (known)) && (target < 0)) result = format("%^s whispers.", m_name);
				else if (known) result = format("%^s animates dead bodies near %s.", m_name, t_name);
				else result = "You hear distant whispering.";
			}

			/* Animate dead */
			mon_ball(who, what, y, x, GF_ANIM_DEAD, 0, 3, TRUE, result);
			break;
		}


		/* RF7_S_MONSTER */
		case 192 + 3:
		{
			if (surface) break;
			disturb(1, 0);
			if ((who > SOURCE_MONSTER_START) || (who == SOURCE_PLAYER_ALLY) || (who == SOURCE_SELF))
			{
				if ((blind) && (known)) msg_format("%^s chants.", m_name);
				else if (known) msg_format("%^s magically summons help!", m_name);
				else msg_print("You hear distant chanting.");

				/* Hack -- prevent summoning for a short while */
				m_ptr->summoned = 20;
			}

			/* Count them for later */
			for (k = 0; k < 1; k++)
			{
				count += summon_specific(y, x, summoner, rlev - 1, 0, TRUE, allies);
			}
			break;
		}

		/* RF7_S_MONSTERS */
		case 192 + 4:
		{
			if (surface) break;
			disturb(1, 0);
			if ((who > SOURCE_MONSTER_START) || (who == SOURCE_PLAYER_ALLY) || (who == SOURCE_SELF))
			{
				if ((blind) && (known)) msg_format("%^s chants.", m_name);
				else if (known) msg_format("%^s magically summons monsters.", m_name);
				else msg_print("You hear distant chanting.");

				/* Hack -- prevent summoning for a short while */
				m_ptr->summoned = 20;
			}

			/* Count them for later */
			for (k = 0; k < 4; k++)
			{
				count += summon_specific(y, x, summoner, rlev - 1, 0, TRUE, allies);
			}
			break;
		}

		/* RF7_R_MONSTER */
		case 192 + 5:
		{
			/* Override cave ecology */
			cave_ecology.ready = FALSE;

			if (surface) break;
			disturb(1, 0);
			if ((who > SOURCE_MONSTER_START) || (who == SOURCE_PLAYER_ALLY) || (who == SOURCE_SELF))
			{
				if ((blind) && (known)) result = format("%^s chants.", m_name);
				else if (known) result = format("%^s magically reanimates a monster!", m_name);
				else result = "You hear distant chanting.";

				/* Hack -- prevent summoning for a short while */
				m_ptr->summoned = 20;
			}

			/* Raise the dead */
			mon_ball(who, what, y, x, GF_RAISE_DEAD, 0, 3, TRUE, result);
			break;
		}

		/* RF7_R_MONSTERS */
		case 192 + 6:
		{
			/* Override cave ecology */
			cave_ecology.ready = FALSE;

			if (surface) break;
			disturb(1, 0);
			if ((who > SOURCE_MONSTER_START) || (who == SOURCE_PLAYER_ALLY) || (who == SOURCE_SELF))
			{
				if ((blind) && (known)) msg_format("%^s chants.", m_name);
				else if (known) msg_format("%^s magically reanimates monsters.", m_name);
				else msg_print("You hear distant chanting.");

				/* Hack -- prevent summoning for a short while */
				m_ptr->summoned = 20;
			}

			/* All in line of sight */
			
			break;
		}

		/* RF7_S_PLANT */
		case 192 + 7:
		{
			if (surface) break;
			disturb(1, 0);
			if ((who > SOURCE_MONSTER_START) || (who == SOURCE_PLAYER_ALLY) || (who == SOURCE_SELF))
			{
				if ((blind) && (known)) msg_format("%^s chants in a rustling tongue.", m_name);
				else if (known) msg_format("%^s magically summons plants.", m_name);
				else msg_print("You hear distant rustling.");

				/* Hack -- prevent summoning for a short while */
				m_ptr->summoned = 20;
			}

			/* Count them for later */
			for (k = 0; k < 4; k++)
			{
				count += summon_specific(y, x, summoner, rlev - 1, SUMMON_PLANT, TRUE, allies);
			}
			break;
		}

		/* RF7_S_INSECT */
		case 192 + 8:
		{
			if (surface) break;
			disturb(1, 0);
			if ((who > SOURCE_MONSTER_START) || (who == SOURCE_PLAYER_ALLY) || (who == SOURCE_SELF))
			{
				if ((blind) && (known)) msg_format("%^s chants in a chittering tongue.", m_name);
				else if (known) msg_format("%^s magically summons insects.", m_name);
				else msg_print("You hear distant chittering.");

				/* Hack -- prevent summoning for a short while */
				m_ptr->summoned = 20;
			}

			/* Count them for later */
			for (k = 0; k < 3; k++)
			{
				count += summon_specific(y, x, summoner, rlev - 1, SUMMON_INSECT, TRUE, allies);
			}
			break;
		}

		/* RF7_S_ANIMALS */
		case 192 + 9:
		{
			if (surface) break;
			disturb(1, 0);
			if ((who > SOURCE_MONSTER_START) || (who == SOURCE_PLAYER_ALLY) || (who == SOURCE_SELF))
			{
				if ((blind) && (known)) msg_format("%^s chants.", m_name);
				else if (known) msg_format("%^s magically summons animals.", m_name);
				else msg_print("You hear distant chanting.");

				/* Hack -- This lets vampires summon animals while not letting undead live with non-undead animals in general */
				if ((r_ptr->flags3 & (RF3_UNDEAD)) && ((r_ptr->flags7 & (RF7_S_ANIMAL)) == 0)) summon_flag_type = (RF8_HAS_SKELETON);

				/* Hack -- Set the skin flags to summon */
				else summon_flag_type = (r_ptr->flags8 & (RF8_SKIN_MASK));

				/* Mega Hack -- Other racial preferences for animals */
				if (!summon_flag_type)
				{
					/* Everyone likes lions, tigers, wolves */
					summon_flag_type |= RF8_HAS_FUR;

					/* Surface dwellers like birds */
					if ((r_ptr->flags9 & (RF9_RACE_MASK)) && ! (r_ptr->flags3 & (RF3_RACE_MASK))) 
						summon_flag_type |= RF8_HAS_FEATHER;

					/* Dungeon dwellers like reptiles, fish and worse */
					else summon_flag_type |= RF8_HAS_SCALE;
				}

				/* Hack -- prevent summoning for a short while */
				m_ptr->summoned = 20;
			}
			else
			{
				/* MegaHack -- Determine letter later */
				summon_flag_type = 0L;
			}

			/* Count them for later */
			for (k = 0; k < 3; k++)
			{
				count += summon_specific(y, x, summoner, rlev - 1, SUMMON_ANIMAL, TRUE, allies);
			}
			break;
		}

		/* RF7_S_HOUND */
		case 192 + 10:
		{
			if (surface) break;
			disturb(1, 0);
			if ((who > SOURCE_MONSTER_START) || (who == SOURCE_PLAYER_ALLY) || (who == SOURCE_SELF))
			{
				if ((blind) && (known)) msg_format("%^s howls.", m_name);
				else if (known) msg_format("%^s magically summons hounds.", m_name);
				else msg_print("You hear distant howling.");

				/* Hack -- prevent summoning for a short while */
				m_ptr->summoned = 20;
			}

			/* Count them for later */
			for (k = 0; k < 2; k++)
			{
				count += summon_specific(y, x, summoner, rlev - 1, SUMMON_HOUND, TRUE, allies);
			}
			break;
		}

		/* RF7_S_SPIDER */
		case 192 + 11:
		{
			if (surface) break;
			disturb(1, 0);
			if ((who > SOURCE_MONSTER_START) || (who == SOURCE_PLAYER_ALLY) || (who == SOURCE_SELF))
			{
				if ((blind) && (known)) msg_format("%^s chants in a chittering tongue.", m_name);
				else if (known) msg_format("%^s magically summons spiders.", m_name);
				else msg_print("You hear distant chittering.");

				/* Hack -- prevent summoning for a short while */
				m_ptr->summoned = 20;
			}

			/* Count them for later */
			for (k = 0; k < 4; k++)
			{
				count += summon_specific(y, x, summoner, rlev - 1, SUMMON_SPIDER, TRUE, allies);
			}
			break;
		}

		/* RF7_S_CLASS */
		case 192 + 12:
		{
			if (surface) break;
			disturb(1, 0);
			if ((who > SOURCE_MONSTER_START) || (who == SOURCE_PLAYER_ALLY) || (who == SOURCE_SELF))
			{
				if ((blind) && (known)) msg_format("%^s chants.", m_name);
				else if (known) msg_format("%^s magically summons allies.", m_name);
				else msg_print("You hear distant chanting.");

				/* Hack -- Set the class flags to summon */
				summon_flag_type = (r_ptr->flags2 & (RF2_CLASS_MASK));

				/* Hack -- prevent summoning for a short while */
				m_ptr->summoned = 20;
			}
			else
			{
				/* MegaHack -- Determine letter later */
				summon_flag_type = 0L;
			}

			/* Count them for later */
			for (k = 0; k < 3; k++)
			{
				count += summon_specific(y, x, summoner, rlev - 1, SUMMON_CLASS, TRUE, allies);
			}
			break;
		}

		/* RF7_S_RACE */
		case 192 + 13:
		{
			if (surface) break;
			disturb(1, 0);
			if ((who > SOURCE_MONSTER_START) || (who == SOURCE_PLAYER_ALLY) || (who == SOURCE_SELF))
			{
				if ((blind) && (known)) msg_format("%^s chants.", m_name);
				else if (known) msg_format("%^s magically summons allies.", m_name);
				else msg_print("You hear distant chanting.");

				/* Hack -- Set the class flags to summon */
				summon_flag_type = (r_ptr->flags3 & (RF3_RACE_MASK));

				/* Mega Hack -- Combine two flags XXX */
				summon_flag_type |= (r_ptr->flags9 & (RF9_RACE_MASK));

				/* Hack -- prevent summoning for a short while */
				m_ptr->summoned = 20;
			}
			else
			{
				/* MegaHack -- Determine letter later */
				summon_flag_type = 0L;
			}

			/* Count them for later */
			for (k = 0; k < 3; k++)
			{
				count += summon_specific(y, x, summoner, rlev - 1, SUMMON_RACE, TRUE, allies);
			}
			break;
		}

		/* RF7_S_GROUP */
		case 192 + 14:
		{
			if (surface) break;
			disturb(1, 0);

			summon_group_type = 0;

			if ((who > SOURCE_MONSTER_START) || (who == SOURCE_PLAYER_ALLY) || (who == SOURCE_SELF))
			{
				if ((blind) && (known)) msg_format("%^s chants.", m_name);
				else if (known) msg_format("%^s magically summons allies.", m_name);
				else msg_print("You hear distant chanting.");

				summon_group_type = r_ptr->grp_idx;

				/* Hack -- prevent summoning for a short while */
				m_ptr->summoned = 20;
			}

			/* Count them for later */
			for (k = 0; k < 3; k++)
			{
				count += summon_specific(y, x, summoner, rlev - 1, SUMMON_GROUP, TRUE, allies);
			}
			break;
		}

		/* RF7_S_FRIEND */
		case 192 + 15:
		{
			int summon_type = SUMMON_FRIEND;

			if (surface) break;
			disturb(1, 0);

			if ((who > SOURCE_MONSTER_START) || (who == SOURCE_PLAYER_ALLY) || (who == SOURCE_SELF))
			{
				if (((blind) && (known)) && (target < 0)) msg_format("%^s calls out for help.", m_name);
				else if (known) msg_format("%^s magically summons a friend.", m_name);
				else msg_print("You hear distant cries for help.");

				/* Mega Hack -- uniques summon other uniques with the same d_char and d_attr */
				if (r_ptr->flags1 & (RF1_UNIQUE))
				{
					summon_char_type = r_ptr->d_char;
					summon_attr_type = r_ptr->d_attr;
					summon_type = SUMMON_UNIQUE_FRIEND;
				}
				else
				{
					summon_race_type = m_ptr->r_idx;
				}

				/* Hack -- prevent summoning for a short while */
				m_ptr->summoned = 20;
			}
			else
			{
				/* MegaHack -- Determine race later */
				summon_race_type = 0;
			}

			/* Count them for later */
			for (k = 0; k < 1; k++)
			{
				count += summon_specific(y, x, summoner, rlev, summon_type, TRUE, allies);
			}

			break;
		}

		/* RF7_S_FRIENDS */
		case 192 + 16:
		{
			int summon_type = SUMMON_FRIEND;

			if (surface) break;
			disturb(1, 0);

			if ((who > SOURCE_MONSTER_START) || (who == SOURCE_PLAYER_ALLY) || (who == SOURCE_SELF))
			{
				if (((blind) && (known)) && (target < 0)) msg_format("%^s calls out for help.", m_name);
				else if (known) msg_format("%^s magically summons friends.", m_name);
				else msg_print("You hear distant cries for help.");

				/* Mega Hack -- uniques summon other uniques with the same d_char and d_attr */
				if (r_ptr->flags1 & (RF1_UNIQUE))
				{
					summon_char_type = r_ptr->d_char;
					summon_attr_type = r_ptr->d_attr;
					summon_type = SUMMON_UNIQUE_FRIEND;
				}
				else
				{
					summon_race_type = m_ptr->r_idx;
				}

				/* Hack -- prevent summoning for a short while */
				m_ptr->summoned = 20;
			}
			else
			{
				/* MegaHack -- Determine race later */
				summon_race_type = 0;
			}

			/* Count them for later */
			for (k = 0; k < 6; k++)
			{
				count += summon_specific(y, x, summoner, rlev, summon_type, TRUE, allies);
			}

			break;
		}

		/* RF7_S_ORC */
		case 192 + 17:
		{
			if (surface) break;
			disturb(1, 0);
			if ((who > SOURCE_MONSTER_START) || (who == SOURCE_PLAYER_ALLY) || (who == SOURCE_SELF))
			{
				if ((blind) && (known)) msg_format("%^s beats on a drum.", m_name);
				else if (known) msg_format("%^s magically summons orcs.", m_name);
				else msg_print("You hear distant drums.");

				/* Hack -- prevent summoning for a short while */
				m_ptr->summoned = 20;
			}

			/* Count them for later */
			for (k = 0; k < 4; k++)
			{
				count += summon_specific(y, x, summoner, rlev - 1, SUMMON_ORC, TRUE, allies);
			}
			break;
		}

		/* RF7_S_TROLL */
		case 192 + 18:
		{
			if (surface) break;
			disturb(1, 0);
			if ((who > SOURCE_MONSTER_START) || (who == SOURCE_PLAYER_ALLY) || (who == SOURCE_SELF))
			{
				if ((blind) && (known)) msg_format("%^s beats on a drum.", m_name);
				else if (known) msg_format("%^s magically summons trolls.", m_name);
				else msg_print("You hear distant drums.");

				/* Hack -- prevent summoning for a short while */
				m_ptr->summoned = 20;
			}

			/* Count them for later */
			for (k = 0; k < 4; k++)
			{
				count += summon_specific(y, x, summoner, rlev - 1, SUMMON_TROLL, TRUE, allies);
			}
			break;
		}

		/* RF7_S_GIANT */
		case 192 + 19:
		{
			if (surface) break;
			disturb(1, 0);
			if ((who > SOURCE_MONSTER_START) || (who == SOURCE_PLAYER_ALLY) || (who == SOURCE_SELF))
			{
				if ((blind) && (known)) msg_format("%^s chants in a thundering voice.", m_name);
				else if (known) msg_format("%^s magically summons giants.", m_name);
				else msg_print("You hear distant thunder.");

				/* Hack -- prevent summoning for a short while */
				m_ptr->summoned = 20;
			}

			/* Count them for later */
			for (k = 0; k < 4; k++)
			{
				count += summon_specific(y, x, summoner, rlev - 1, SUMMON_GIANT, TRUE, allies);
			}
			break;
		}

		/* RF7_S_DRAGON */
		case 192 + 20:
		{
			disturb(1, 0);
			sound(MSG_SUM_DRAGON);
			if ((who > SOURCE_MONSTER_START) || (who == SOURCE_PLAYER_ALLY) || (who == SOURCE_SELF))
			{
				if ((blind) && (known)) msg_format("%^s chants in a roaring voice.", m_name);
				else if (known) msg_format("%^s magically summons a dragon.", m_name);
				else msg_print("You hear distant roars.");

				/* Hack -- prevent summoning for a short while */
				m_ptr->summoned = 20;
			}

			for (k = 0; k < 1; k++)
			{
				count += summon_specific(m_ptr->fy, m_ptr->fx, summoner,
					rlev - 1, SUMMON_DRAGON, TRUE, allies);
			}

			break;
		}

		/* RF7_S_HI_DRAGON */
		case 192 + 21:
		{
			if (surface) break;
			disturb(1, 0);
			sound(MSG_SUM_HI_DRAGON);
			if ((who > SOURCE_MONSTER_START) || (who == SOURCE_PLAYER_ALLY) || (who == SOURCE_SELF))
			{
				if ((blind) && (known)) msg_format("%^s chants in a cacophonous voice.", m_name);
				else if (known) msg_format("%^s magically summons ancient dragons!", m_name);
				else msg_print("You hear cacophonous roars.");

				/* Hack -- prevent summoning for a short while */
				m_ptr->summoned = 20;
			}

			for (k = 0; k < 4; k++)
			{
				count += summon_specific(y, x, summoner, rlev - 1, SUMMON_HI_DRAGON, TRUE, allies);
			}
			break;
		}

		/* RF7_A_ELEMENT */
		case 192 + 22:
		{
			/* Override cave ecology */
			cave_ecology.ready = FALSE;

			if (surface) break;
			disturb(1, 0);

			if ((who > SOURCE_MONSTER_START) || (who == SOURCE_PLAYER_ALLY) || (who == SOURCE_SELF))
			{
				if ((blind) && (known)) result = format("%^s chants in a rumbling voice.", m_name);
				else if (known) result = format("%^s magically animates the elements around %s.", m_name, t_name);
				else result = "You hear distant rumbles.";
			}

			/* Animate elements */
			mon_ball(who, what, y, x, GF_ANIM_ELEMENT, 0, 3, TRUE, result);
			break;
		}

		/* RF7_A_OBJECT */
		case 192 + 23:
		{
			/* Override cave ecology */
			cave_ecology.ready = FALSE;

			if (surface) break;
			disturb(1, 0);

			if ((who > SOURCE_MONSTER_START) || (who == SOURCE_PLAYER_ALLY) || (who == SOURCE_SELF))
			{
				if ((blind) && (known)) result = format("%^s chants in a clanking.", m_name);
				else if (known) result = format("%^s magically animates the objects around %s.", m_name, t_name);
				else result = "You hear distant clanking.";
			}

			/* Animate objects */
			mon_ball(who, what, y, x, GF_ANIM_OBJECT, 0, 3, TRUE, result);
			break;
		}

		/* RF7_S_DEMON */
		case 192 + 24:
		{
			if (surface) break;
			disturb(1, 0);
			sound(MSG_SUM_DEMON);
			if ((who > SOURCE_MONSTER_START) || (who == SOURCE_PLAYER_ALLY) || (who == SOURCE_SELF))
			{
				if ((blind) && (known)) msg_format("%^s chants in an infernal voice.", m_name);
				else if (known) msg_format("%^s magically summons a hellish adversary!", m_name);
				else msg_print("You hear infernal chanting.");

				/* Hack -- prevent summoning for a short while */
				m_ptr->summoned = 20;
			}

			for (k = 0; k < 1; k++)
			{
				count += summon_specific(y, x, summoner, rlev - 1, SUMMON_DEMON, TRUE, allies);
			}
			break;
		}

		/* RF7_S_HI_DEMON */
		case 192 + 25:
		{
			if (surface) break;
			disturb(1, 0);
			sound(MSG_SUM_HI_DEMON);

			if ((who > SOURCE_MONSTER_START) || (who == SOURCE_PLAYER_ALLY) || (who == SOURCE_SELF))
			{
				if ((blind) && (known)) msg_format("%^s chants in an infernal voice.", m_name);
				else if (known) msg_format("%^s magically summons greater demons!", m_name);
				else msg_print("You hear an infernal chorus.");

				/* Hack -- prevent summoning for a short while */
				m_ptr->summoned = 20;
			}

			for (k = 0; k < 4; k++)
			{
				count += summon_specific(y, x, summoner, rlev - 1, SUMMON_HI_DEMON, TRUE, allies);
			}
			break;
		}

		/* Summon a dead unique */
		/* RF7_R_UNIQUE */
		case 192 + 26:
		{
			int summon_type = RAISE_UNIQUE;

			disturb(1, 0);
			sound(MSG_SUM_UNIQUE);
			if ((who > SOURCE_MONSTER_START) || (who == SOURCE_PLAYER_ALLY) || (who == SOURCE_SELF))
			{
				if ((blind) && (known)) msg_format("%^s chants in a powerful voice.", m_name);
				else if (known) msg_format("%^s magically raises one of your former opponents!", m_name);
				else msg_print("You hear powerful, invocative chanting.");

				if (r_ptr->flags7 & (RF7_S_HI_UNIQUE)) summon_type = RAISE_HI_UNIQUE;

				/* Hack -- prevent summoning for a short while */
				m_ptr->summoned = 20;
			}

			for (k = 0; k < 1; k++)
			{
				count += summon_specific(y, x, summoner, rlev - 1, summon_type, TRUE, allies);
			}
			break;
		}

		/* Summon Uniques */
		/* RF7_S_UNIQUE */
		case 192 + 27:
		{
			disturb(1, 0);
			sound(MSG_SUM_UNIQUE);
			if ((who > SOURCE_MONSTER_START) || (who == SOURCE_PLAYER_ALLY) || (who == SOURCE_SELF))
			{
				if ((blind) && (known)) msg_format("%^s chants in a powerful voice.", m_name);
				else if (known) msg_format("%^s magically summons special opponents!", m_name);
				else msg_print("You hear powerful, invocative chanting.");

				/* Hack -- prevent summoning for a short while */
				m_ptr->summoned = 20;
			}

			for (k = 0; k < 3; k++)
			{
				count += summon_specific(y, x, summoner, rlev - 1, SUMMON_UNIQUE, TRUE, allies);
			}
			break;
		}

		/* Summon Uniques */
		/* RF7_S_HI_UNIQUE */
		case 192 + 28:
		{
			disturb(1, 0);
			sound(MSG_SUM_UNIQUE);
			if ((who > SOURCE_MONSTER_START) || (who == SOURCE_PLAYER_ALLY) || (who == SOURCE_SELF))
			{
				if ((blind) && (known)) msg_format("%^s chants in a powerful voice.", m_name);
				else if (known) msg_format("%^s magically summons legendary opponents!", m_name);
				else msg_print("You hear powerful, invocative chanting.");

				/* Hack -- prevent summoning for a short while */
				m_ptr->summoned = 20;
			}

			for (k = 0; k < 3; k++)
			{
				count += summon_specific(y, x, summoner, rlev - 1, SUMMON_HI_UNIQUE, TRUE, allies);
			}
			break;
		}

		/* RF7_S_UNDEAD */
		case 192 + 29:
		{
			if (surface) break;
			disturb(1, 0);
			sound(MSG_SUM_UNDEAD);

			if ((who > SOURCE_MONSTER_START) || (who == SOURCE_PLAYER_ALLY) || (who == SOURCE_SELF))
			{
				if ((blind) && (known)) msg_format("%^s whispers.", m_name);
				else if (known) msg_format("%^s magically summons an undead adversary!", m_name);
				else msg_print("You hear distant whispering.");
			}

			for (k = 0; k < 1; k++)
			{
				count += summon_specific(y, x, summoner, rlev - 1, SUMMON_UNDEAD, TRUE, allies);
			}
			break;
		}

		/* RF7_S_HI_UNDEAD */
		case 192 + 30:
		{
			if (surface) break;
			disturb(1, 0);
			sound(MSG_SUM_HI_UNDEAD);

			if ((who > SOURCE_MONSTER_START) || (who == SOURCE_PLAYER_ALLY) || (who == SOURCE_SELF))
			{
				summoner = m_list[who > SOURCE_MONSTER_START ? who : what].r_idx;

				if ((blind) && (known)) msg_format("%^s whispers.", m_name);
				else if (known) msg_format("%^s magically summons greater undead!", m_name);
				else msg_print("You hear loud and imperious whispering.");

				/* Hack -- prevent summoning for a short while */
				m_ptr->summoned = 20;
			}

			for (k = 0; k < 4; k++)
			{
				count += summon_specific(y, x, summoner, rlev - 1, SUMMON_HI_UNDEAD, TRUE, allies);
			}

			break;
		}

		/* Summon the Ringwraiths */
		/* RF7_S_WRAITH */
		case 192 + 31:
		{
			disturb(1, 0);
			sound(MSG_SUM_WRAITH);

			if ((who > SOURCE_MONSTER_START) || (who == SOURCE_PLAYER_ALLY) || (who == SOURCE_SELF))
			{
				if ((blind) && (known)) msg_format("%^s whispers.", m_name);
				else if (known) msg_format("%^s magically summons mighty undead opponents!", m_name);
				else msg_print("You hear thunderous, echoing whispers.");

				/* Hack -- prevent summoning for a short while */
				m_ptr->summoned = 20;
			}

			for (k = 0; k < 6; k++)
			{
				count += summon_specific(y, x, summoner, rlev - 1, SUMMON_WRAITH, TRUE, allies);
			}

			for (k = 0; k < 6; k++)
			{
				count += summon_specific(y, x, summoner, rlev - 1, SUMMON_HI_UNDEAD, TRUE, allies);
			}

			break;
		}

		/* Paranoia */
		default:
		{
			if ((who > SOURCE_MONSTER_START) || (who == SOURCE_PLAYER_ALLY) || (who == SOURCE_SELF))
				msg_print("A monster tried to cast a spell that has not yet been defined.");
			else
				msg_print("Something tried to cast a spell that has not yet been defined.");
		}
	}

	/* Restore cave ecology */
	cave_ecology.ready = old_cave_ecology;

	/* Hack - Inform a blind player about monsters appearing nearby */
	if (blind && count && (target < 0))
	{
		if (count == 1)
		{
			msg_print("You hear something appear nearby.");
		}
		else if (count < 4)
		{
			msg_print("You hear several things appear nearby.");
		}
		else
		{
			msg_print("You hear many things appear nearby.");
		}
	}

	/* Monster updates */
	if (who > SOURCE_MONSTER_START)
	{
		/* Mark minimum desired range for recalculation */
		m_ptr->min_range = 0;

		/* Remember what the monster did to us */
		if (seen)
		{
			/* Innate spell */
			if (attack < 32*4)
			{
				l_ptr->flags4 |= (1L << (attack - 32*3));
				if (l_ptr->cast_innate < MAX_UCHAR) l_ptr->cast_innate++;

				/* Hack -- always notice if powerful */
				if (r_ptr->flags2 & (RF2_POWERFUL)) l_ptr->flags2 |= (RF2_POWERFUL);

				/* Hack -- blows */
				if (attack - 32*3 < 4)
				{
					/* Count attacks of this type */
					if (l_ptr->blows[attack - 32*3] < MAX_UCHAR)
					{
						l_ptr->blows[attack - 32*3]++;
					}
				}
			}

			/* Bolt or Ball */
			else if (attack < 32*5)
			{
				l_ptr->flags5 |= (1L << (attack - 32*4));
				if (l_ptr->cast_spell < MAX_UCHAR) l_ptr->cast_spell++;
			}

			/* Special spell */
			else if (attack < 32*6)
			{
				l_ptr->flags6 |= (1L << (attack - 32*5));
				if (l_ptr->cast_spell < MAX_UCHAR) l_ptr->cast_spell++;

				/* Hack -- always notice if powerful */
				if (r_ptr->flags2 & (RF2_POWERFUL)) l_ptr->flags2 |= (RF2_POWERFUL);
			}

			/* Summon spell */
			else if (attack < 32*7)
			{
				l_ptr->flags7 |= (1L << (attack - 32*6));
				if (l_ptr->cast_spell < MAX_UCHAR) l_ptr->cast_spell++;
			}
		}

		if (seen && p_ptr->wizard)
			msg_format("%^s has %i mana remaining.", m_name, m_ptr->mana);

		/* Always take note of monsters that kill you */
		if (p_ptr->is_dead && (l_ptr->deaths < MAX_SHORT))
		{
			l_ptr->deaths++;
		}
	}

	/* A spell was cast */
 	return (TRUE);
}


/*
 * Determine if monster resists a blow.  -LM-
 *
 * Return TRUE if evasion was successful.
 */
bool mon_evade(int m_idx, int chance, int out_of, cptr r)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	char m_name[80];

	cptr p;

	int roll;
	
	if ((r_ptr->flags9 & (RF9_EVASIVE)) == 0) return (FALSE);

	roll = rand_int(out_of);
	
	/* Get "the monster" or "it" */
	monster_desc(m_name, sizeof(m_name), m_idx, 0x40);

	switch(roll % 4)
	{
		case 0: p = "dodges"; break;
		case 1: p = "evades"; break;
		case 2: p = "side steps"; break;
		default: p = "ducks"; break;
	}

	/* Hack -- evasive monsters may ignore trap */
	if ((!m_ptr->blind) && (!m_ptr->csleep)
		&& (roll < chance))
	{
		if (m_ptr->ml)
		{
			/* Message */
			message_format(MSG_MISS, 0, "%^s %s%s!", m_name, p, r);

			/* Note that monster is evasive */
			l_ptr->flags9 |= (RF9_EVASIVE);
		}
		return (TRUE);
	}

	return (FALSE);
}


/*
 * Determine if monster resists a blow.  -LM-
 *
 * Return TRUE if blow was avoided.
 */
bool mon_resist_object(int m_idx, const object_type *o_ptr)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	int resist = 0;
	bool learn = FALSE;

	cptr note = "";

	char m_name[80];
	char o_name[80];

	/* Get "the monster" or "it" */
	monster_desc(m_name, sizeof(m_name), m_idx, 0x40);

	/* Describe object */
	if (o_ptr->k_idx) object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);
	else (my_strcpy(o_name,"attack", sizeof(o_name)));

	/*
	 * Handle monsters that resist blunt and/or edged weapons. We include
	 * martial arts as a blunt attack, as well as any unusual thrown objects.
	 * We include resist magic for instances where we try to attack monsters
	 * with spell based weapons.
	 */
	switch (o_ptr->tval)
	{
		case TV_SPELL:
		{
			/* Resist */
			if ((r_ptr->flags9 & (RF9_RES_MAGIC)) != 0)
			{
				resist = 60;

				if (((l_ptr->flags9 & (RF9_RES_MAGIC)) == 0) &&
					(m_ptr->ml))
				{
					l_ptr->flags9 |= (RF9_RES_MAGIC);
					learn = TRUE;
				}
			}

			/* Take note */
			note = "glances off of";
			break;
		}

		case TV_ARROW:
		case TV_BOLT:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_DIGGING:
		case TV_SPIKE:
		{
			/* Immunity */
			if ((r_ptr->flags9 & (RF9_IM_EDGED)) != 0)
			{
				/* Resist */
				resist = 85;

				/* Learn */
				if (((l_ptr->flags9 & (RF9_IM_EDGED)) == 0) &&
					(m_ptr->ml))
				{
					l_ptr->flags9 |= (RF9_IM_EDGED);
					learn = TRUE;
				}
			}
			/* Resist */
			else if ((r_ptr->flags9 & (RF9_RES_EDGED)) != 0)
			{
				resist = 60;

				if (((l_ptr->flags9 & (RF9_RES_EDGED)) == 0) &&
					(m_ptr->ml))
				{
					l_ptr->flags9 |= (RF9_RES_EDGED);
					learn = TRUE;
				}
			}

			/* Take note */
			note = "glances off of";
			break;
		}

		default:
		{
			/* Immunity */
			if ((r_ptr->flags9 & (RF9_IM_BLUNT)) != 0)
			{
				resist = 85;

				if (((l_ptr->flags9 & (RF9_IM_BLUNT)) == 0) &&
					(m_ptr->ml))
				{
					l_ptr->flags9 |= (RF9_IM_BLUNT);
					learn = TRUE;
				}
			}
			/* Resist */
			else if ((r_ptr->flags9 & (RF9_RES_BLUNT)) != 0)
			{
				resist = 60;

				if (((l_ptr->flags9 & (RF9_RES_BLUNT)) == 0) &&
					(m_ptr->ml))
				{
					l_ptr->flags9 |= (RF9_RES_BLUNT);
					learn = TRUE;
				}
			}

			/* Take note */
			if (strchr("GvE", r_ptr->d_char))
				note = "passes harmlessly through";
			else
				note = "bounces off of";

			break;
		}
	}

	/* Hack -- more accurate weapons reduce resistance */
	resist -= o_ptr->to_h;

	/* Try for a miss */
	if (resist > rand_int(100))
	{
		/* Monster is fully visible */
		if (m_ptr->ml)
		{
			/* Take note of new resist */
			if (learn)
			{
				if (resist >= 80)
					msg_format("Your %s does almost no damage to %s!",
						o_name, m_name);
				else if (resist >= 70)
					msg_format("Your %s does very little damage to %s!",
						o_name, m_name);
				else if (resist >= 50)
					msg_format("Your %s does little damage to %s!",
						o_name, m_name);
				else
					msg_format("Your %s is resisted by %s.",
						o_name, m_name);
			}

			/* Note already known resistance */
			else
			{
				msg_format("Your %s %s %s.", o_name, note, m_name);
			}
		}

		/* Can't hurt me! */
		return (TRUE);
	}

	/* Can hurt me */
	return (FALSE);
}



/*
 * Handle monster hitting a real trap.
 * TODO: join with other (monster?) attack routines
 * TODO: this is probably the easiest place for the refactoring
 */
void mon_hit_trap(int m_idx, int y, int x)
{
	feature_type *f_ptr;
	monster_type *m_ptr = &m_list[m_idx];

	int feat = cave_feat[y][x];

	bool fear;
	bool magic = TRUE;

	char m_name[80];

	/* Hack --- don't activate unknown invisible traps */
	if (cave_feat[y][x] == FEAT_INVIS) return;

	/* Get feature */
	f_ptr = &f_info[cave_feat[y][x]];

	/* Get "the monster" or "it" */
	monster_desc(m_name, sizeof(m_name), m_idx, 0);

	/* Hack --- trapped doors */
	/* XXX XXX Dangerous */
	while (!(f_ptr->spell) && !(f_ptr->blow.method) && (f_ptr->flags1 & (FF1_TRAP)))
	{
		pick_trap(y,x);

		/* Error */
		if (cave_feat[y][x] == feat) break;

		feat = cave_feat[y][x];

		/* Get feature */
		f_ptr = &f_info[feat];

	}

	/* Use covered or bridged if necessary */
	if ((f_ptr->flags2 & (FF2_COVERED)) || (f_ptr->flags2 & (FF2_BRIDGED)))
	{
		f_ptr = &f_info[f_ptr->mimic];
	}

	/* Hack -- monster falls onto trap */
	if ((m_ptr->fy!=y)|| (m_ptr->fx !=x))
	{
		/* Move monster */
		monster_swap(m_ptr->fy, m_ptr->fx, y, x);
	}

	/* Hack -- evasive monsters may ignore trap */
	if (mon_evade(m_idx, (m_ptr->stunned || m_ptr->confused) ? 50 : 80, 100, " a trap"))
	{
		if (f_ptr->flags1 & (FF1_SECRET))
		{
			/* Discover */
			cave_alter_feat(y,x,FS_SECRET);
		}

		return;
	}

	/* Apply the object */
	else if ((cave_o_idx[y][x]) && (f_ptr->flags1 & (FF1_HIT_TRAP)))
	{
		object_type *o_ptr = &o_list[cave_o_idx[y][x]];

		char o_name[80];

		int power = 0;

		switch (o_ptr->tval)
		{
			case TV_BOW:
			{
				object_type *j_ptr;
				u32b f1,f2,f3,f4;

				int i, shots = 1;

				/* Get bow */
				j_ptr = o_ptr;

				/* Get bow flags */
				object_flags(o_ptr,&f1,&f2,&f3,&f4);

				/* Apply extra shots */
				if (f1 & (TR1_SHOTS)) shots += j_ptr->pval;

				/* Test for hit */
				for (i = 0; i < shots; i++)
				{
					if (j_ptr->next_o_idx)
					{
						int ammo = j_ptr->next_o_idx;
						object_type *i_ptr;
						object_type object_type_body;

						/* Use ammo instead of bow */
						o_ptr = &o_list[ammo];

						/* Describe ammo */
						object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 0);

						/* Did we hit? */
						if (test_hit_fire((j_ptr->to_h + o_ptr->to_h)* BTH_PLUS_ADJ + f_ptr->power,  calc_monster_ac(m_idx, TRUE), TRUE))
						{
							int k, mult;

							mult = bow_multiplier(j_ptr->sval);

							/* Apply extra might */
							if (f1 & (TR1_MIGHT)) mult += j_ptr->pval;

							k = damroll(o_ptr->dd, o_ptr->ds);
							k *= mult;

							k = tot_dam_aux(o_ptr, k, m_ptr, TRUE);

							k += critical_shot(o_ptr->weight, o_ptr->to_h + j_ptr->to_h, k);
							k += o_ptr->to_d + j_ptr->to_d;

							/* No negative damage */
							if (k < 0) k = 0;

							/* Damage, check for fear and death */
							if (!mon_resist_object(m_idx, o_ptr)) (void)mon_take_hit(cave_m_idx[y][x], k, &fear, NULL);

						}
						else
						{
							/* Trap description */
							if (m_ptr->ml) msg_format("%^s narrowly misses %s.",o_name, m_name);
						}

						/* Get local object */
						i_ptr = &object_type_body;

						/* Obtain a local object */
						object_copy(i_ptr, o_ptr);

						/* Modify quantity */
						i_ptr->number = 1;

						/* Apply additional effect from activation */
						if (auto_activate(o_ptr))
						{
							/* Make item strike */
							process_item_blow(SOURCE_PLAYER_ACT_ARTIFACT, o_ptr->name1, o_ptr, y, x);
						}

						/* Apply additional effect from coating*/
						else if (coated_p(o_ptr))
						{
							/* Make item strike */
							process_item_blow(SOURCE_PLAYER_COATING, lookup_kind(o_ptr->xtra1, o_ptr->xtra2), o_ptr, y, x);
						}

						/* Drop nearby - some chance of breakage */
						drop_near(i_ptr,y,x,breakage_chance(i_ptr));

						/* Decrease the item */
						floor_item_increase(ammo, -1);
						floor_item_optimize(ammo);

						break;
					}
					else
					{
						/* Disarm */
						cave_alter_feat(y,x,FS_DISARM);
					}
				}
			}

			/* Similar to hitting a regular trap below, but (hack) damage increased by current player level. */
			case TV_SPELL:
			{
				/* Player floats on terrain */
				if (player_ignore_terrain(feat)) return;

				if (f_ptr->spell)
				{
   		   			make_attack_ranged(SOURCE_PLAYER_TRAP,f_ptr->spell,y,x);
				}
				else if (f_ptr->blow.method)
				{
					int dam = damroll(f_ptr->blow.d_side,f_ptr->blow.d_dice * (((p_ptr->lev + 9) / 10) + 1) );

					/* Apply the blow */
					project_p(SOURCE_PLAYER_TRAP, feat, p_ptr->py, p_ptr->px, dam, f_ptr->blow.effect);
					project_t(SOURCE_PLAYER_TRAP, feat, p_ptr->py, p_ptr->px, dam, f_ptr->blow.effect);
				}
				
				/* Drop through to use a charge */
			}

			case TV_WAND:
			case TV_STAFF:
			{
				if (o_ptr->charges > 0)
				{
					/* Get item effect */
					get_spell(&power, "use", o_ptr, FALSE);

					/* XXX Hack -- new unstacking code */
					o_ptr->stackc++;

					/* No spare charges */	
					if (o_ptr->stackc >= o_ptr->number)
					{
						/* Use a charge off the stack */
						o_ptr->charges--;

						/* Reset the stack count */
						o_ptr->stackc = 0;
					}

					/* XXX Hack -- unstack if necessary */
					if ((o_ptr->number > 1) &&
					((!object_charges_p(o_ptr) && (o_ptr->charges == 2) && (o_ptr->stackc > 1)) ||
					  (!object_charges_p(o_ptr) && (rand_int(o_ptr->number) <= o_ptr->stackc) &&
					  (o_ptr->stackc != 1) && (o_ptr->charges > 2))))
					{
						object_type *i_ptr;
						object_type object_type_body;

						/* Get local object */
						i_ptr = &object_type_body;

						/* Obtain a local object */
						object_copy(i_ptr, o_ptr);

						/* Modify quantity */
						i_ptr->number = 1;

						/* Reset stack counter */
						i_ptr->stackc = 0;
 
				 		/* Unstack the used item */
				 		o_ptr->number--;

						/* Reduce the charges on the new item */
						if (o_ptr->stackc > 1)
						{
							i_ptr->charges-=2;
							o_ptr->stackc--;
						}
						else if (!o_ptr->stackc)
						{
							i_ptr->charges--;
							o_ptr->charges++;
							o_ptr->stackc = o_ptr->number-1;
						}

						(void)floor_carry(y,x,i_ptr);
					}
				}
				else
				{
					/* Disarm if runs out */
					cave_alter_feat(y,x,FS_DISARM);
				}

				break;
			}

			case TV_ROD:
			case TV_DRAG_ARMOR:
			{
				if (!((o_ptr->timeout) && ((!o_ptr->stackc) || (o_ptr->stackc >= o_ptr->number))))
				{
					int tmpval;

					/* Store pval */
					tmpval = o_ptr->timeout;

					/* Time rod out */
					o_ptr->timeout = o_ptr->charges;

					/* Get item effect */
					get_spell(&power, "use", o_ptr, FALSE);

					/* Has a power */
					/* Hack -- check if we are stacking rods */
					if ((o_ptr->timeout > 0) && (!(tmpval) || stack_force_times))
					{
						/* Hack -- one more rod charging */
						if (o_ptr->timeout) o_ptr->stackc++;

						/* Reset stack count */
						if (o_ptr->stackc == o_ptr->number) o_ptr->stackc = 0;

						/* Hack -- always use maximum timeout */
						if (tmpval > o_ptr->timeout) o_ptr->timeout = tmpval;
					}

					/* XXX Hack -- unstack if necessary */
					if ((o_ptr->number > 1) && (o_ptr->timeout > 0))
					{
						object_type *i_ptr;
						object_type object_type_body;

						/* Get local object */
						i_ptr = &object_type_body;

						/* Obtain a local object */
						object_copy(i_ptr, o_ptr);

						/* Modify quantity */
						i_ptr->number = 1;

						/* Clear stack counter */
						i_ptr->stackc = 0;

						/* Restore "charge" */
						o_ptr->timeout = tmpval;

						/* Unstack the used item */
						o_ptr->number--;

						/* Reset the stack if required */
						if (o_ptr->stackc == o_ptr->number) o_ptr->stackc = 0;

						(void)floor_carry(y,x,i_ptr);
					}
				}
				break;
			}

			case TV_POTION:
			case TV_FLASK:
			case TV_FOOD:
				magic = FALSE;
				/* Drop through */
			case TV_SCROLL:
			case TV_LITE:
			{
				/* Hack -- boring food */
				if ((o_ptr->tval == TV_FOOD) && (o_ptr->sval >= SV_FOOD_MIN_FOOD))
				{
					/* Disarm */
					cave_alter_feat(y,x,FS_DISARM);
				}
				else
				{
					/* Get item effect */
					get_spell(&power, "use", o_ptr, FALSE);

					/* Decrease the item */
					floor_item_increase(cave_o_idx[y][x], -1);
					floor_item_optimize(cave_o_idx[y][x]);

					/* Disarm if runs out */
					if (!cave_o_idx[y][x]) cave_alter_feat(y,x,FS_DISARM);
				}

				break;
			}

			case TV_RUNESTONE:
			{
				u32b runes = p_ptr->cur_runes;

				int num = 0;
				s16b book[26];

				/* Hack -- use current rune */
				p_ptr->cur_runes = (2 << (o_ptr->sval-1));

				/* Fill the book with spells */
				fill_book(o_ptr,book,&num);

				/* Unhack */
				p_ptr->cur_runes = runes;

				/* Get a power */
				power = book[rand_int(num)];

				/* Decrease the item */
				floor_item_increase(cave_o_idx[y][x], -1);
				floor_item_optimize(cave_o_idx[y][x]);

				/* Disarm if runs out */
				if (!cave_o_idx[y][x]) cave_alter_feat(y,x,FS_DISARM);

				break;
			}

			case TV_SWORD:
			case TV_POLEARM:
			case TV_HAFTED:
			{
				object_type *i_ptr;
				object_type object_type_body;

				/* Describe ammo */
				object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 0);

				/* Test for hit */
				if (test_hit_norm(o_ptr->to_h * BTH_PLUS_ADJ + f_ptr->power, calc_monster_ac(m_idx, FALSE), TRUE))
				{
					int k;

					k = damroll(o_ptr->dd, o_ptr->ds);

					k = tot_dam_aux(o_ptr, k, m_ptr, TRUE);

					k += critical_norm(o_ptr->weight, 2 * o_ptr->to_h, k);
					k += o_ptr->to_d;

					/* Armour reduces total damage */
					k -= (k * ((p_ptr->ac < 150) ? p_ptr->ac : 150) / 250);

					/* No negative damage */
					if (k < 0) k = 0;

					/* Damage, check for fear and death */
					if (!mon_resist_object(m_idx, o_ptr)) (void)mon_take_hit(cave_m_idx[y][x], k, &fear, NULL);

				}
				else
				{
					/* Trap description */
					if (m_ptr->ml) msg_format("%^s narrowly misses %s.",o_name, m_name);					
				}

				/* Get local object */
				i_ptr = &object_type_body;

				/* Obtain a local object */
				object_copy(i_ptr, o_ptr);

				/* Modify quantity */
				i_ptr->number = 1;

				/* Apply additional effect from activation */
				if (auto_activate(o_ptr))
				{
					/* Make item strike */
					process_item_blow(SOURCE_PLAYER_ACT_ARTIFACT, o_ptr->name1, o_ptr, y, x);
				}

				/* Apply additional effect from coating*/
				else if (coated_p(o_ptr))
				{				
					/* Make item strike */
					process_item_blow(SOURCE_PLAYER_COATING, lookup_kind(o_ptr->xtra1, o_ptr->xtra2), o_ptr, y, x);
				}

				/* Drop nearby - some chance of breakage */
				drop_near(i_ptr,y,x,breakage_chance(i_ptr));

				/* Decrease the item */
				floor_item_increase(cave_o_idx[y][x], -1);
				floor_item_optimize(cave_o_idx[y][x]);

				/* Disarm if runs out */
				if (!cave_o_idx[y][x]) cave_alter_feat(y,x,FS_DISARM);

				break;
			}

			default:
			{
				/* Disarm */
				cave_alter_feat(y,x,FS_DISARM);
			}
		}

		/* Has a power */
		if (power > 0)
		{
			spell_type *s_ptr = &s_info[power];

			int ap_cnt;

			/* Object is used */
			if (k_info[o_ptr->k_idx].used < MAX_SHORT) k_info[o_ptr->k_idx].used++;

			/* Scan through all four blows */
			for (ap_cnt = 0; ap_cnt < 4; ap_cnt++)
			{
				int damage = 0;

				/* Extract the attack infomation */
				int effect = s_ptr->blow[ap_cnt].effect;
				int method = s_ptr->blow[ap_cnt].method;
				int d_dice = s_ptr->blow[ap_cnt].d_dice;
				int d_side = s_ptr->blow[ap_cnt].d_side;
				int d_plus = s_ptr->blow[ap_cnt].d_plus;

				/* Hack -- no more attacks */
				if (!method) break;

				if  ((magic) && ((r_info[m_ptr->r_idx].flags9 & (RF9_RES_MAGIC)) != 0)
					&& (rand_int(100) < 20 + p_ptr->depth / 2))
				{
					msg_format("%^s is unaffected.", m_name);

					if ((m_ptr->ml) && !(l_list[m_ptr->r_idx].flags9 & (RF9_RES_MAGIC)))
					{
						l_list[m_ptr->r_idx].flags9 |= (RF9_RES_MAGIC);
					}

					continue;
				}

				/* Mega hack -- dispel evil/undead objects */
				if (!d_side)
				{
					d_plus += 25 * d_dice;
				}

				/* Roll out the damage */
				if ((d_dice) && (d_side))
				{
					damage = damroll(d_dice, d_side) + d_plus;
				}
				else
				{
					damage = d_plus;
				}

				(void)project_m(SOURCE_PLAYER_TRAP, o_ptr->k_idx,y,x,damage, effect);
				(void)project_f(SOURCE_PLAYER_TRAP, o_ptr->k_idx,y,x,damage, effect);
				(void)project_t(SOURCE_PLAYER_TRAP, o_ptr->k_idx,y,x,damage, effect);
			}
		}
	}

	/* Regular traps */
	else
	{
		if (f_ptr->spell)
		{
	      		make_attack_ranged(SOURCE_FEATURE,f_ptr->spell,y,x);
		}
		else if (f_ptr->blow.method)
		{
			int damage = damroll(f_ptr->blow.d_side,f_ptr->blow.d_dice);
   
			/* Apply the blow */
			project_m(SOURCE_FEATURE, feat, y, x, damage, f_ptr->blow.effect);

			/* Apply the blow */
			project_t(SOURCE_FEATURE, feat, y, x, damage, f_ptr->blow.effect);
		}

		/* Get feature */
		f_ptr = &f_info[cave_feat[p_ptr->py][p_ptr->px]];

		if (f_ptr->flags1 & (FF1_HIT_TRAP))
		{
			/* Modify the location hit by the trap */
			cave_alter_feat(y,x,FS_HIT_TRAP);
		}
		else if (f_ptr->flags1 & (FF1_SECRET))
		{
			/* Discover */
			cave_alter_feat(y,x,FS_SECRET);
		}
	}
}




#if 0

/*
 * The running algorithm  -CJS-
 *
 * Modified for monsters. This lets them navigate corridors 'quickly'
 * and correctly turn corners. We use the monster run algorithm to
 * move now.
 *
 * We should be careful not to to let a monster run past the player
 * unless they are fleeing.
 *
 * We should be careful to 'disturb' the monster if they are in a room
 * and the player becomes visible or moves while visible.
 *
 * Because monsters are effectively running, we should be careful to
 * update the run parameters if they are 'staggering', and not let
 * them run while confused.
 *
 */




/*
 * Hack -- allow quick "cycling" through the legal directions
 */
static byte cycle[] =
{ 1, 2, 3, 6, 9, 8, 7, 4, 1, 2, 3, 6, 9, 8, 7, 4, 1 };

/*
 * Hack -- map each direction into the "middle" of the "cycle[]" array
 */
static byte chome[] =
{ 0, 8, 9, 10, 7, 0, 11, 6, 5, 4 };



/*
 * Initialize the running algorithm for a new direction.
 *
 * Diagonal Corridor -- allow diaginal entry into corridors.
 *
 * Blunt Corridor -- If there is a wall two spaces ahead and
 * we seem to be in a corridor, then force a turn into the side
 * corridor, must be moving straight into a corridor here. ???
 *
 * Diagonal Corridor    Blunt Corridor (?)
 *       # #		  #
 *       #x#		 @x#
 *       @p.		  p
 */
static void mon_run_init(int dir)
{
	int fy = m_ptr->fy;
	int fx = m_ptr->fx;

	int i, row, col;

	bool deepleft, deepright;
	bool shortleft, shortright;

	/* Save the direction */
	m_ptr->run_cur_dir = dir;

	/* Assume running straight */
	m_ptr->run_old_dir = dir;

	/* Assume looking for open area */
	m_ptr->mflag |= MFLAG_RUN_OPEN_AREA;

	/* Assume not looking for breaks */
	m_ptr->mflag &= ~(MFLAG_RUN_BREAK_RIGHT);
	m_ptr->mflag &= ~(MFLAG_RUN_BREAK_LEFT);

	/* Assume no nearby walls */
	deepleft = deepright = FALSE;
	shortright = shortleft = FALSE;

	/* Find the destination grid */
	row = fy + ddy[dir];
	col = fx + ddx[dir];

	/* Extract cycle index */
	i = chome[dir];

	/* Check for nearby wall */
	if (see_wall(cycle[i+1], fy, fx))
	{
		m_ptr->mflag |= MFLAG_RUN_BREAK_LEFT;
		shortleft = TRUE;
	}

	/* Check for distant wall */
	else if (see_wall(cycle[i+1], row, col))
	{
		m_ptr->mflag |= MFLAG_RUN_BREAK_LEFT;
		deepleft = TRUE;
	}

	/* Check for nearby wall */
	if (see_wall(cycle[i-1], fy, fx))
	{
		m_ptr->mflag |= MFLAG_RUN_BREAK_RIGHT;
		shortright = TRUE;
	}

	/* Check for distant wall */
	else if (see_wall(cycle[i-1], row, col))
	{
		m_ptr->mflag |= MFLAG_RUN_BREAK_RIGHT;
		deepright = TRUE;
	}

	/* Looking for a break */
	if ((m_ptr->mflag & (MFLAG_RUN_BREAK_LEFT)) && (m_ptr->mflag & (MFLAG_RUN_BREAK_RIGHT)))
	{
		/* Not looking for open area */
		m_ptr->mflag &= ~(MFLAG_RUN_OPEN_AREA);

		/* Hack -- allow angled corridor entry */
		if (dir & 0x01)
		{
			if (deepleft && !deepright)
			{
				m_ptr->run_old_dir = cycle[i - 1];
			}
			else if (deepright && !deepleft)
			{
				m_ptr->run_old_dir = cycle[i + 1];
			}
		}

		/* Hack -- allow blunt corridor entry */
		else if (see_wall(cycle[i], row, col))
		{
			if (shortleft && !shortright)
			{
				m_ptr->run_old_dir = cycle[i - 2];
			}
			else if (shortright && !shortleft)
			{
				m_ptr->run_old_dir = cycle[i + 2];
			}
		}
	}
}


/*
 * Update the current "run" path
 *
 * Return TRUE if the running should be stopped
 */
static bool run_test(void)
{
	int fy = m_ptr->fy;
	int fx = m_ptr->fx;

	int prev_dir;
	int new_dir;
	int check_dir = 0;

	int row, col;
	int i, max;
	int option, option2;

	int feat;

	bool notice;

	/* No options yet */
	option = 0;
	option2 = 0;

	/* Where we came from */
	prev_dir = m_ptr->run_old_dir;


	/* Range of newly adjacent grids */
	max = (prev_dir & 0x01) + 1;


	/* Look at every newly adjacent square. */
	for (i = -max; i <= max; i++)
	{
		s16b this_o_idx, next_o_idx = 0;


		/* New direction */
		new_dir = cycle[chome[prev_dir] + i];

		/* New location */
		row = fy + ddy[new_dir];
		col = fx + ddx[new_dir];


		/* Visible monsters abort running */
		if (cave_m_idx[row][col] > 0)
		{
			return (TRUE);
		}

		/* Analyze unknown grids and floors */
		if (cave_floor_bold(row, col))
		{
			/* Looking for open area */
			if (m_ptr->mflag & (MFLAG_RUN_OPEN_AREA))
			{
				/* Nothing */
			}

			/* The first new direction. */
			else if (!option)
			{
				option = new_dir;
			}

			/* Three new directions. Stop running. */
			else if (option2)
			{
				return (TRUE);
			}

			/* Two non-adjacent new directions.  Stop running. */
			else if (option != cycle[chome[prev_dir] + i - 1])
			{
				return (TRUE);
			}

			/* Two new (adjacent) directions (case 1) */
			else if (new_dir & 0x01)
			{
				check_dir = cycle[chome[prev_dir] + i - 2];
				option2 = new_dir;
			}

			/* Two new (adjacent) directions (case 2) */
			else
			{
				check_dir = cycle[chome[prev_dir] + i + 1];
				option2 = option;
				option = new_dir;
			}
		}

		/* Obstacle, while looking for open area */
		else
		{
			if (m_ptr->mflag & (MFLAG_RUN_OPEN_AREA))
			{
				if (i < 0)
				{
					/* Break to the right */
					m_ptr->mflag |= MFLAG_RUN_BREAK_RIGHT;
				}

				else if (i > 0)
				{
					/* Break to the left */
					m_ptr->mflag |= MFLAG_RUN_BREAK_LEFT;
				}
			}
		}
	}


	/* Looking for open area */
	if (m_ptr->mflag & (MFLAG_RUN_OPEN_AREA)run_open_area)
	{
		/* Hack -- look again */
		for (i = -max; i < 0; i++)
		{
			new_dir = cycle[chome[prev_dir] + i];

			row = fy + ddy[new_dir];
			col = fx + ddx[new_dir];

			/* Get feature */
			feat = cave_feat[row][col];

			/* Get mimiced feature */
			feat = f_info[feat].mimic;

			/* Unknown grid or non-wall */
			/* Was: cave_floor_bold(row, col) */
			if (!(play_info[row][col] & (PLAY_MARK)) ||
			    (!(f_info[feat].flags1 & (FF1_WALL))) )
			{
				/* Looking to break right */
				if ((m_ptr->mflag & (MFLAG_RUN_BREAK_RIGHT)))
				{
					return (TRUE);
				}
			}


			/* Obstacle */
			else
			{
				/* Looking to break left */
				if ((m_ptr->mflag & (MFLAG_RUN_BREAK_LEFT)))
				{
					return (TRUE);
				}
			}
		}

		/* Hack -- look again */
		for (i = max; i > 0; i--)
		{
			new_dir = cycle[chome[prev_dir] + i];

			row = fy + ddy[new_dir];
			col = fx + ddx[new_dir];

			/* Get feature */
			feat = cave_feat[row][col];

			/* Get mimiced feature */
			feat = f_info[feat].mimic;

			/* Unknown grid or non-wall */
			/* Was: cave_floor_bold(row, col) */
			if (!(play_info[row][col] & (PLAY_MARK)) ||
			    (!(f_info[feat].flags1 & (FF1_WALL))))
			{
				/* Looking to break left */
				if ((m_ptr->mflag & (MFLAG_RUN_BREAK_LEFT)))
				{
					return (TRUE);
				}
			}

			/* Obstacle */
			else
			{
				/* Looking to break right */
				if ((m_ptr->mflag & (MFLAG_RUN_BREAK_RIGHT)))
				{
					return (TRUE);
				}
			}
		}
	}


	/* Not looking for open area */
	else
	{
		/* No options */
		if (!option)
		{
			return (TRUE);
		}

		/* One option */
		else if (!option2)
		{
			/* Primary option */
			m_ptr->run_cur_dir = option;

			/* No other options */
			m_ptr->run_old_dir = option;
		}

		/* Two options, examining corners */
		else if (run_use_corners && !run_cut_corners)
		{
			/* Primary option */
			m_ptr->run_cur_dir = option;

			/* Hack -- allow curving */
			m_ptr->run_old_dir = option2;
		}

		/* Two options, pick one */
		else
		{
			/* Get next location */
			row = fy + ddy[option];
			col = fx + ddx[option];

			/* Don't see that it is closed off. */
			/* This could be a potential corner or an intersection. */
			if (!see_wall(option, row, col) ||
			    !see_wall(check_dir, row, col))
			{
				/* Can not see anything ahead and in the direction we */
				/* are turning, assume that it is a potential corner. */
				if (run_use_corners &&
				    see_nothing(option, row, col) &&
				    see_nothing(option2, row, col))
				{
					m_ptr->run_cur_dir = option;
					m_ptr->run_old_dir = option2;
				}

				/* STOP: we are next to an intersection or a room */
				else
				{
					return (TRUE);
				}
			}

			/* This corner is seen to be enclosed; we cut the corner. */
			else if (run_cut_corners)
			{
				m_ptr->run_cur_dir = option2;
				m_ptr->run_old_dir = option2;
			}

			/* This corner is seen to be enclosed, and we */
			/* deliberately go the long way. */
			else
			{
				m_ptr->run_cur_dir = option;
				m_ptr->run_old_dir = option2;
			}
		}
	}


	/* About to hit a known wall, stop */
	if (see_wall(m_ptr->run_cur_dir, fy, fx))
	{
		return (TRUE);
	}


	/* Failure */
	return (FALSE);
}



/*
 * Take one step along the current "run" path
 *
 * Called with a real direction to begin a new run, and with zero
 * to continue a run in progress.
 */
void mon_run_step(int dir)
{
	/* Start run */
	if (dir)
	{
		/* Initialize */
		run_init(dir);
	}

	/* Continue run */
	else
	{
		/* Update run */
		if (run_test())
		{
			/* Done */
			return;
		}
	}

}


#endif

