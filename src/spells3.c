/* File: spells3.c */

/*
 * Functions added by will_asher for DaJAngband.
 
 * (except for the wonder spell, this applies only to the wonder spell:)
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statxement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"



/*
 * Paladin Truce spell
 * (avoids using project() so I don't have to add another GF_XXX)
 *
 * Currently you can make a truce with sleeping monsters, and you get no
 * indication whether sleeping monsters have accepted the truce or not.
 * You'll probably want to re-think this..
 */
bool truce(void)
{
	int i, y, x, tmp, dur, talk, dishonor = 0;
	char m_name[80];
	bool something = FALSE;
	bool sleeping;

	/* Scan monsters */
	for (i = 1; i < mon_max; i++)
	{
		/* get the monster and its level */
		monster_type *m_ptr = &mon_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];
		int rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;
		
		/* monster is temporarily dead */
		if (m_ptr->temp_death) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* affect only monsters in line of sight */
		if (!player_has_los_bold(y, x)) continue;

		/* affect only living monsters */
		if ((r_ptr->flags3 & (RF3_UNDEAD)) || (r_ptr->flags3 & (RF3_NON_LIVING)) ||
			(r_ptr->flags7 & (RF7_NONMONSTER)))
			continue;

		/* smart monsters of any symbol can be affected */
		if (r_ptr->flags2 & (RF2_SMART)) /* */;
		/* only certain types of monsters are affected */
		else if (!(strchr("dhkptxyAKOPTY@", r_ptr->d_char))) continue;

		/* evil knights have a chance to be affected */
		if ((m_ptr->evil) && (strchr("K", r_ptr->d_char)))
		{
			dishonor = rlev + 20;
		}
		/* some others of low level can be affected */
		else if ((m_ptr->evil) && (rlev <= p_ptr->lev/2))
		{
			dishonor = rlev + 40;
		}
		/* all other evil monsters are very rarely affected */
		else if (m_ptr->evil)
		{
			dishonor = (rlev*4) + 60;
		}
		/* nonevil monsters unlikely to resist (more likely to work on knights and light fairies) */
		else if (strchr("K", r_ptr->d_char)) dishonor = rlev / 3;
		else if (rlev > 4) dishonor = (rlev-2) / 2;
		else dishonor = 1;
		/* uniques */
		if (r_ptr->flags1 & (RF1_UNIQUE)) dishonor += 50;
		else if (r_ptr->maxpop == 1) dishonor += 30;
		/* non-agressive light fairies */
		if ((p_ptr->nice) && (r_ptr->flags3 & (RF3_CLIGHT))) dishonor = dishonor/2;
		/* common race strengthens truce (hacky, but I don't really care) */
		if (((p_ptr->prace == 5) || (p_ptr->prace == 3)) && 
			(strchr("h", r_ptr->d_char))) dishonor -= 20;
		if (((p_ptr->prace == 12) || (p_ptr->prace == 15)) && 
			(r_ptr->flags3 & (RF3_CLIGHT))) dishonor -= 20;

		/* effectiveness based on charisma and luck (not level) */
		talk = (adj_chr_charm[p_ptr->stat_ind[A_CHR]] * 2) + randint(goodluck+1);
		/* cap (for those with maxxed charisma) */
		if (talk > 50) talk = 50 + ((talk - 47)/4);

		/* get monster name */
		monster_desc(m_name, sizeof(m_name), m_ptr, 0);

		/* is the monster sleeping? */
		if ((m_ptr->csleep) && (!m_ptr->roaming)) sleeping = TRUE;
		else sleeping = FALSE;

		/* saving throw */
		if (dishonor >= 1) dishonor = randint(dishonor * 2);
		if (dishonor > talk)
		{
			if ((!m_ptr->evil) && (m_ptr->ml) && (!sleeping))
				msg_format("%^s refuses", m_name);
			continue;
		}
		/* (else monster is affected) */

		/* begin truce */
		dur = (talk*2) - dishonor;
		dur += randint(talk/2);

		/* minimum duration for low-level monsters */
		if ((p_ptr->lev > rlev * 2 + 1) && (dur < p_ptr->lev - rlev*2 + 1))
			dur = p_ptr->lev - rlev*2 + 1;

		/* message (only if monster is awake and visible) */
		if ((!sleeping) && (m_ptr->ml))
		{
			if (m_ptr->truce) msg_format("%^s agrees to extend the truce.", m_name);
			else if (dur < 12) msg_format("%^s agrees to a short truce.", m_name);
			else msg_format("%^s agrees to a temporary truce.", m_name);
		}

		/* Increase truce duration */
		tmp = m_ptr->truce + dur;

		/* Set truce */
		m_ptr->truce = (tmp < 200) ? tmp : 200;

		/* a monster was affected */
		something = TRUE;
	}

	return something;
}



/*
 * Mist of Amnesia
 * (a mist shouldn't just be an LOS effect)
 */
bool mass_amnesia(int power, int ey, int ex)
{
	int i, y, x, erlev, do_sleep;
	char m_name[80];
	bool something = FALSE;
	bool grenade = FALSE;

	/* only grenades send coordinates to this function */
	if ((ey) && (ex)) grenade = TRUE;

	if (!grenade)
	{
		if (project_los(GF_AMNESIA, power)) something = TRUE;
	}

	/* Scan monsters */
	for (i = 1; i < mon_max; i++)
	{
		/* get the monster */
		monster_type *m_ptr = &mon_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];
		monster_lore *l_ptr = &l_list[m_ptr->r_idx];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;
		
		/* monster is temporarily dead */
		if (m_ptr->temp_death) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* affect only monsters within 14 spaces away who aren't in line of sight */
		/* (spell has already affected LOS monsters) */
		if ((!grenade) && ((player_has_los_bold(y, x)) || (m_ptr->cdis > 14))) continue;

		/* grenades of smoke have shorter range than the amnesia spell */
		else if (grenade)
		{
			int maxdis = 8;
			if (los(ey, ex, y, x)) maxdis += 3 + (goodluck+1)/3;
			if (distance(ey, ex, y, x) > maxdis) continue;
		}

		/* effective monster level: */
		erlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);
		if ((r_ptr->flags1 & (RF1_UNIQUE)) && (r_ptr->level > 80)) erlev = erlev * 2;
		else if ((r_ptr->flags1 & (RF1_UNIQUE)) && (r_ptr->level < 22))  erlev += 9;
		else if (r_ptr->flags1 & (RF1_UNIQUE)) erlev += 10 + randint((erlev/2) - 10);
		
		/* get monster name */
		monster_desc(m_name, sizeof(m_name), m_ptr, 0);

		/* doesn't affect non-living monsters */
		if ((r_ptr->flags3 & (RF3_UNDEAD)) || (r_ptr->flags3 & (RF3_NON_LIVING)))
		{
			if (m_ptr->ml)
			{
				/* Memorize a flag */
				if (r_ptr->flags3 & (RF3_NON_LIVING)) l_ptr->flags3 |= (RF3_NON_LIVING);
				if (r_ptr->flags3 & (RF3_UNDEAD)) l_ptr->flags3 |= (RF3_UNDEAD);
				/*  */
				msg_format("%^s is unaffected!", m_name);
			}
		}
		/* Attempt a saving throw (power should be slightly more than plev * 2) */
		else if (r_ptr->level > randint((power - 10) < 1 ? 1 : (power - 10)) + 20)
		{
			/* resist */
			if (m_ptr->ml) msg_format("%^s resists the spell!", m_name);
		}
		else
		{
			/* Forget the player later */
			if (m_ptr->ml) msg_format("%^s forgets what it was doing.", m_name);
			/* (more random amount than LOS effect, lower average) */
			do_sleep = 300 + randint(r_ptr->sleep * 7);
			if (do_sleep > 2000) do_sleep = 2000;

			/* no, forget the player now */
			if (!m_ptr->csleep) m_ptr->csleep = do_sleep;
			else m_ptr->csleep += do_sleep/2;

			/* monster is roaming not sleeping */
			m_ptr->roaming = 1;

			/* something happened */
			something = TRUE;
		}
	}

	return something;
}



/*
 * The black realm vampiric drain spell.
 * Get actual amount of damage, damage the monster, then
 * figure amount to heal the caster based on the damage.
 * (previously you could shoot the spell at a wall and still
 *  get healed.)
 */
int do_vampiric_drain(int dir, int damage)
{
	int healmuch, ty, tx, x, y, i;

	monster_type *m_ptr;
	monster_race *r_ptr;
	monster_lore *l_ptr;
	char m_name[80];
	bool fear = FALSE;

	int path_n;
	u16b path_g[256];

	/* Easy if we have a specific target */
	if ((dir == 5) && target_okay())
	{
		ty = p_ptr->target_row;
		tx = p_ptr->target_col;

		/* get monster info */
		m_ptr = &mon_list[cave_m_idx[ty][tx]];
		r_ptr = &r_info[m_ptr->r_idx];
		l_ptr = &l_list[m_ptr->r_idx];
		monster_desc(m_name, sizeof(m_name), m_ptr, 0);
	}
	/* find target monster (taken from do_cmd_fire() ) */
	else
	{
		/* Start at the player */
		y = p_ptr->py;
		x = p_ptr->px;

		/* Predict the "target" location */
		ty = p_ptr->py + 99 * ddy[dir];
		tx = p_ptr->px + 99 * ddx[dir];

		/* Calculate the path */
		path_n = project_path(path_g, MAX_RANGE, p_ptr->py, p_ptr->px, ty, tx, 0);

		/* Project along the path */
		for (i = 0; i < path_n; ++i)
		{
			int ny = GRID_Y(path_g[i]);
			int nx = GRID_X(path_g[i]);

			if ((!cave_floor_bold(ny, nx)) && (cave_m_idx[ny][nx] > 0)) /* okay*/;
			else if (!cave_floor_bold(ny, nx)) break;

			/* Advance */
			x = nx;
			y = ny;

			/* found the target */
			if (cave_m_idx[y][x] > 0)
			{
				/* target the monster */
				ty = y;
				tx = x;
				/* get monster info */
				m_ptr = &mon_list[cave_m_idx[ty][tx]];
				r_ptr = &r_info[m_ptr->r_idx];
				l_ptr = &l_list[m_ptr->r_idx];
				monster_desc(m_name, sizeof(m_name), m_ptr, 0);
				break;
			}
		}
	}

	/* didn't find any monster */
	if (!m_ptr) return 0;

	/* these monsters aren't affected */
	/* (so it doesn't do the caster any good) */
	if ((r_ptr->flags3 & (RF3_NON_LIVING)) || 
		(r_ptr->flags3 & (RF3_UNDEAD)) ||
		(r_ptr->flags3 & (RF3_SILVER)))
	{
		if (r_ptr->flags3 & (RF3_UNDEAD))
		{
			if (m_ptr->ml) l_ptr->flags3 |= (RF3_UNDEAD);
		}
		if (r_ptr->flags3 & (RF3_NON_LIVING))
		{
			if (m_ptr->ml) l_ptr->flags3 |= (RF3_NON_LIVING);
		}
		if (r_ptr->flags3 & (RF3_SILVER))
		{
			if (m_ptr->ml) l_ptr->flags3 |= (RF3_SILVER);
		}

		msg_format("%^s is unaffected", m_name);
		return 0;
	}

	/* check for death before killing the monster */
	/* (so we know how much the caster can drain) */
	if (m_ptr->hp < damage)
	{
		cptr note_dies = " dies.";
		damage = m_ptr->hp;
		/* now kill it */
		mon_take_hit(cave_m_idx[ty][tx], damage+1, &fear, note_dies, TRUE);
	}
	else
	{
		/* Hurt the monster, check for fear (we know the monster isn't dead) */
		mon_take_hit(cave_m_idx[ty][tx], damage, &fear, NULL, TRUE);

		/* notice fear */
		if ((fear) && (m_ptr->ml))
		{
			/* Message */
			message_format(MSG_FLEE, m_ptr->r_idx,
			               "%^s flees in terror!", m_name);
		}
	}

	/* amount to heal the caster */
	healmuch = damage + 10 + goodluck;
	if (damage >= 24) healmuch += randint((damage/2)-10);

	return healmuch;
}



/*
 * Hook to specify an item which can be blessed
 */
static bool item_tester_hook_bless(const object_type *o_ptr)
{
	/* Extract the flags */
	u32b f1, f2, f3, f4;
	object_flags(o_ptr, &f1, &f2, &f3, &f4);

	/* anything which can be wielded as a weapon */
	/* (cannot bless most throwing weapons or ammo) */
	if (wield_slot(o_ptr) == INVEN_WIELD) return (TRUE);

	/* anything which can be activated (even elemental rings) */
	if (f3 & (TR3_ACTIVATE)) return (TRUE);

	switch (o_ptr->tval)
	{
		/* bows and lights */
		case TV_BOW:
        case TV_LITE:

		/* any armor */
        case TV_BOOTS:
        case TV_GLOVES:
        case TV_HELM:
        case TV_CROWN:
        case TV_SHIELD:
        case TV_CLOAK:
        case TV_SOFT_ARMOR:
        case TV_HARD_ARMOR:
        case TV_DRAG_ARMOR:
		{
			return (TRUE);
		}
		/* lowers fail rate of magic devices */
		case TV_WAND:
		case TV_ROD:
		case TV_STAFF:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}



/*
 * Hook to specify an item which can be enhanced by the alchemy spell
 */
bool item_tester_hook_bigwand(const object_type *o_ptr)
{
	/* Extract the flags (to check for activation) */
	u32b f1, f2, f3, f4;
	object_flags(o_ptr, &f1, &f2, &f3, &f4);

	if (o_ptr->tval == TV_WAND)
	{
		/* can waste an enhancement on a wand which can't be enhanced */
		if (!object_aware_p(o_ptr)) return (TRUE);
		/* check type */
		switch (o_ptr->sval)
		{
			/* most, but not all wands can be enhanced */
			case SV_WAND_HEAL_MONSTER:
			case SV_WAND_LITE:
			case SV_WAND_SLEEP_MONSTER:
			case SV_WAND_SLOW_MONSTER:
			case SV_WAND_CONFUSE_MONSTER:
			case SV_WAND_FEAR_MONSTER:
			case SV_WAND_POLYMORPH:
			case SV_WAND_DRAIN_LIFE:
			case SV_WAND_STINKING_CLOUD:
			case SV_WAND_MAGIC_MISSILE:
			case SV_WAND_ACID_BOLT:
			case SV_WAND_ELEC_BOLT:
			case SV_WAND_FIRE_BOLT:
			case SV_WAND_COLD_BOLT:
			case SV_WAND_ACID_BALL:
			case SV_WAND_ELEC_BALL:
			case SV_WAND_FIRE_BALL:
			case SV_WAND_COLD_BALL:
			case SV_WAND_DRAGON_FIRE:
			case SV_WAND_DRAGON_COLD:
			case SV_WAND_DRAGON_BREATH:
			case SV_WAND_STORMS:
				 return (TRUE);
		}
	}

	/* min level to enhance staves */
	if (p_ptr->lev < 20) return (FALSE);

	/* not many staff types can be enhanced (used to be none) */
	if (o_ptr->tval == TV_STAFF)
	{
		/* can waste an enhancement on a rod which can't be enhanced */
		if (!object_aware_p(o_ptr)) return (TRUE);
		/* check type */
		switch (o_ptr->sval)
		{
			case SV_STAFF_DARKNESS:
			case SV_STAFF_STARLITE:
			case SV_STAFF_CURE_LIGHT:
			case SV_STAFF_SLEEP_MONSTERS:
			case SV_STAFF_SLOW_MONSTERS:
			case SV_STAFF_STRIKING:
				 return (TRUE);
		}
	}

	/* only a high level alchemist can enhance rods, rings and artifacts */
	if (p_ptr->lev < 30) return (FALSE);
	if (o_ptr->tval == TV_ROD)
	{
		/* can waste an enhancement on a rod which can't be enhanced */
		if (!object_aware_p(o_ptr)) return (TRUE);
		/* check type */
		switch (o_ptr->sval)
		{
			case SV_ROD_RECALL:
			case SV_ROD_ILLUMINATION:
			case SV_ROD_LITE:
			case SV_ROD_SLEEP_MONSTER:
			case SV_ROD_SLOW_MONSTER:
			case SV_ROD_POLYMORPH:
			case SV_ROD_DRAIN_LIFE:
			case SV_ROD_ACID_BOLT:
			case SV_ROD_ELEC_BOLT:
			case SV_ROD_FIRE_BOLT:
			case SV_ROD_COLD_BOLT:
			case SV_ROD_ACID_BALL:
			case SV_ROD_ELEC_BALL:
			case SV_ROD_FIRE_BALL:
			case SV_ROD_COLD_BALL:
				 return (TRUE);
		}
	}

	/* only a few of the weaker activations can be enhanced */
	if ((o_ptr->name1) && (f3 & (TR3_ACTIVATE)))
	{
		artifact_type *a_ptr = &a_info[o_ptr->name1];
		switch (a_ptr->activation)
		{
			case ACT_MISSILE:
			case ACT_FIRE1:
			case ACT_FROST1:
			case ACT_LIGHTNING_BOLT:
			case ACT_ACID1:
			case ACT_STINKING_CLOUD:
			case ACT_FROST2:
			case ACT_FROST4:
			case ACT_FROST3:
			case ACT_FIRE2:
			case ACT_HOLY_FIRE:
			case ACT_DRAIN_LIFE1:
			case ACT_DRAIN_LIFE2:
			case ACT_CURE_WOUNDS:
			case ACT_WOR:
			case ACT_CONFUSE:
			case ACT_MANA_BOLT:
				 return (TRUE);
		}
	}

	/* elemental rings */
	if (o_ptr->tval == TV_RING)
	{
		if (!object_aware_p(o_ptr))
		{
			if (f3 & (TR3_ACTIVATE)) return (TRUE);
			else return (FALSE);
		}
		switch (o_ptr->sval)
		{
			case SV_RING_ACID:
			case SV_RING_FLAMES:
			case SV_RING_ICE:
			case SV_RING_LIGHTNING:
				 return (TRUE);
		}
	}

	/* else */ return (FALSE);
}


/*
 * The enhance wand/rod alchemy spell.
 * works similar to priest's bless weapon spell
 */
bool enhance_wand(int power)
{
	int item, lev, gain;
	object_type *o_ptr;
	char o_name[80];
	s16b amount;
	cptr q, s;

	item_tester_hook = item_tester_hook_bigwand;

	/* Get an item */
	if (p_ptr->lev < 20) q = "Enhance which wand? ";
	else q = "Enhance which magic item? ";
	s = "You have nothing to enhance.";  
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return (FALSE);

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}

	/* Description */
	object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 1);

	/* should never happen */
	if (o_ptr->enhancenum > o_ptr->number) o_ptr->enhancenum = o_ptr->number;

	/* current enhancement per item */
	if (o_ptr->enhancenum) amount = o_ptr->enhance/o_ptr->enhancenum;
	else amount = 0;

	/* max enhancements */
	if ((amount >= 10) || ((o_ptr->enhance >= 5) && (artifact_p(o_ptr))))
	{
		msg_format("%s %s has already been enhanced.",
		       ((item >= 0) ? "Your" : "The"), o_name);
		return (FALSE);
	}

	/* Extract the item diffuculty (now separated from item level) */
	if (o_ptr->tval == TV_WAND) lev = k_info[o_ptr->k_idx].extra;
	/* items other than wands are harder to enhance */
	else if (o_ptr->tval == TV_STAFF) lev = (k_info[o_ptr->k_idx].extra * 5) / 4;
	else if (o_ptr->tval == TV_ROD) lev = (k_info[o_ptr->k_idx].extra * 3) / 2;
	else lev = (k_info[o_ptr->k_idx].extra * 5) / 3;

	/* minimum resistance */
	if ((lev < 20) && ((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_STAFF)))
		lev += (23 - lev) / 3;
	if ((lev < 25) && (!((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_STAFF)))) 
        lev += (26 - lev) / 2;

	/* amount of enhancement */
	gain = power - lev;

	/* weak first enhancement weakens other enhancements */
	if (o_ptr->enhance) gain = (gain + 1) / 2;

	/* cap enhancement on artifacts */
	if ((artifact_p(o_ptr)) && (gain > 25 + goodluck)) gain = 25 + goodluck;

	/* minimum gain (spell failed) */
	if (gain < 4) gain = 0;

	/* enhance the item */
	if (gain) o_ptr->enhance += gain;
	if ((gain >= 10) && (o_ptr->number > o_ptr->enhancenum)) o_ptr->enhancenum += 1;
	else if (!o_ptr->enhancenum) o_ptr->enhancenum = 1;

	if (o_ptr->enhance >= 50) /* rarely this powerful */
	{
		msg_format("%s %s is Mightily Enhanced!",
		       ((item >= 0) ? "Your" : "The"), o_name);
	}
	else if ((o_ptr->enhance >= 25) && (gain))
	{
		msg_format("%s %s is greatly enhanced!",
		       ((item >= 0) ? "Your" : "The"), o_name);
	}
	else if ((o_ptr->enhance >= 10) && (gain))
	{
		msg_format("You succeeded in enhancing %s %s.",
		       ((item >= 0) ? "Your" : "The"), o_name);
	}
	else if (gain)
	{
		msg_format("You succeeded in enhancing %s %s, but it's a weak enhancement.",
		       ((item >= 0) ? "Your" : "The"), o_name);
	}
	else /* failed */
	{
		msg_print("The enhancement failed.");
	}

	return TRUE;
}

/*
 * The bless object spell. (not always a weapon)
 * has a chance of nullifing the BAD_WEAP flag
 */
bool bless_weapon(int power)
{
	int item;
	cptr q, s;
	u32b f1, f2, f3, f4;
	int something, resistb, plus;
	bool weapon, bow, lite, mdevice = FALSE;
	bool barmor = FALSE;

	object_type *o_ptr;
	char o_name[80];

	item_tester_hook = item_tester_hook_bless;

	/* Get an item */
	q = "Bless which item? ";
	s = "You have nothing to bless.";  
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return (FALSE);

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}

	/* Description */
	object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 1);

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3, &f4);

	/* Describe */
	msg_format("A white light touches %s %s",
	           ((item >= 0) ? "Your" : "The"), o_name);
	           
	something = 0;
	resistb = 1;
	if (!o_ptr->blessed)
	{
	   if (f3 & (TR3_BAD_WEAP)) resistb = 20 + badluck/2 + o_ptr->pval*2;
	   if (f2 & (TR2_CORRUPT)) resistb += 15;
    }
    /* this spell can directly remove curses now, but only for priests */
    if (cursed_p(o_ptr))
    {
	   if ((f3 & (TR3_HEAVY_CURSE)) && (resistb < 15)) resistb = 15;
	   else if (f3 & (TR3_HEAVY_CURSE)) resistb += 10;
	   else /* light curse */ resistb += 5;
    }
	
	/* what kind of object is it? */
	weapon = (wield_slot(o_ptr) == INVEN_WIELD);
	bow = (wield_slot(o_ptr) == INVEN_BOW);
	lite = (wield_slot(o_ptr) == INVEN_LITE);
	if ((!weapon) && (wield_slot(o_ptr) >= INVEN_BODY) && (wield_slot(o_ptr) <= INVEN_FEET))
		barmor = TRUE;
	if ((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_ROD) || (o_ptr->tval == TV_RING)) mdevice = TRUE;

	/* Narsil is only thing with both CORRUPT and BLESSED */
	/* and it's easier to un-CORRUPT */
	if ((f2 & (TR2_CORRUPT)) && (f2 & (TR3_BLESSED))) resistb = 1;

	/* fail on artifacts with heavy curses or BAD_WEAP flag */
	/* can work on other artifacts */
    if ((broken_p(o_ptr)) || ((artifact_p(o_ptr)) && (resistb > 9) && (power + (goodluck/2) < 47)))
    {
        if (artifact_p(o_ptr)) msg_print("The powerful evil resists enchantment");
        else msg_print("The blessing fails");
        /* blessing failed but the attempt still uses mana */
	    return TRUE;
    }

    /* sometimes some trace of the curse remains */
    if ((f3 & (TR3_HEAVY_CURSE)) && (!cursed_p(o_ptr)) && (randint(100) < 19)) resistb += (randint(2) * 5);

    /* can bless egos or artifacts, but they */
    /* have a chance to resist, especially if they don't have GOOD_WEAP */
    if ((artifact_p(o_ptr)) && (!f3 & (TR3_GOOD_WEAP)) && (resistb < 30)) resistb = 30;
    else if ((ego_item_p(o_ptr)) && (!f3 & (TR3_GOOD_WEAP)) && (resistb < 15)) resistb = 15;
    else if ((artifact_p(o_ptr)) && (resistb < 20)) resistb = 20;
	else if ((o_ptr->tval == TV_DRAG_ARMOR) && (resistb < 15)) resistb = 15;
    else if ((ego_item_p(o_ptr)) && (resistb < 10)) resistb = 10;

	/* Narsil is only thing with CORRUPT and BLESSED and is easier to un-CORRUPT */
	if ((f2 & (TR2_CORRUPT)) && (f2 & (TR3_BLESSED))) resistb = (resistb*3)/5;
    
    /* paladins not as good at removing curses and evil alignment */
    if ((!cp_ptr->flags & CF_BLESS_WEAPON) && (resistb + 5 < 20)) resistb = 20;
    else if (!cp_ptr->flags & CF_BLESS_WEAPON) resistb += 5;
    
    /* blessing doesn't always work on lites or devices */
    if ((lite) && (resistb < 2)) resistb += (randint(5) * 5);
    else if (((lite) || (mdevice)) && (resistb < 15)) resistb = 15;
    
    /* everything should have some small chance to be blessed (max power = plev) */
    if (resistb > 49) resistb = 48 + randint(2);

	/* Attempt to overcome BAD_WEAP and curses */
    if (resistb > power)
    {
        if (artifact_p(o_ptr)) msg_print("The artifact remains unaffected");
        else if (f3 & (TR3_BAD_WEAP)) msg_print("The evil enchantment resists blessing");
        else if (lite) msg_print("You fail to make the light brighter");
        else /* ego or device */ msg_print("The object's magic resists blessing");
        something = 2;
    }
	/* bless */
	else if (o_ptr->blessed <= 1)
    {
       if ((f3 & (TR3_BAD_WEAP)) && (!o_ptr->blessed))
       {
          msg_format("The evil enchantment on %s %s is lifted!",
           ((item >= 0) ? "Your" : "The"), o_name);
       }
       if (lite) o_ptr->blessed = 500 + (power * 100);
       else o_ptr->blessed = 500 + power * 50;
       something = 1;

	   /* only priests can uncurse HEAVY_CURSE objects */
	   if ((cp_ptr->flags & CF_BLESS_WEAPON) && (cursed_p(o_ptr))) 
           uncurse_object(o_ptr);

       /* remove evil egos ("The evil enchantment is lifted") */
       /* (but not MORGUL or NAZGUL) */
       if ((o_ptr->name2 == EGO_WITCHCRAFT) || (o_ptr->name2 == EGO_DEMON_MIGHT) ||
          (o_ptr->name2 == EGO_BLOODLUST))
       {
           o_ptr->name2 = 0;
           /* remove random stuff with ego */
			o_ptr->randsus = 0;
			o_ptr->randsus2 = 0;
			o_ptr->randres = 0;
			o_ptr->randres2 = 0;
			o_ptr->randres3 = 0;
			o_ptr->randpow = 0;
			o_ptr->randpow2 = 0;
			o_ptr->randslay = 0;
			o_ptr->randslay2 = 0;
			o_ptr->randslay3 = 0;
			o_ptr->randbon = 0;
			o_ptr->randbon2 = 0;
			o_ptr->randplu = 0;
			o_ptr->randplu2 = 0;
			o_ptr->randdrb = 0;
			o_ptr->randdrb2 = 0;
			o_ptr->randimm = 0;
			o_ptr->randlowr = 0;
			o_ptr->randlowr2 = 0;
			o_ptr->randact = 0;
			o_ptr->esprace = 0;
       }
    }
    else /* extend if already blessed */
    {
       if (lite) o_ptr->blessed += (power * (35 + goodluck));
       else o_ptr->blessed += (power * (25 + (goodluck/2)));
    }

    /* usually only enchants if you are at least L35 */
    if (p_ptr->lev > 34 - goodluck/4)
    {
		/* priests don't get as much combat enchantment as paladins do */
       if ((cp_ptr->flags & CF_BLESS_WEAPON) && (goodluck < 19))
	   {
           power -= (5 - goodluck/4);
	   }

       plus = 1;
       if (power > 19) plus = power / 10;
       if ((plus > 3) && (goodluck < 8)) plus = 2 + randint(plus - 2);

       if ((o_ptr->blessed) && ((weapon) || (bow)))
       {
	      /* Enchant */
	      if (enchant(o_ptr, plus, ENCH_TOHIT, FALSE)) something = 1;
	      if (enchant(o_ptr, plus, ENCH_TODAM, FALSE)) something = 1;
       }
       else if ((barmor) && (o_ptr->blessed)) /* armor */
       {
	      if (enchant(o_ptr, plus, ENCH_TOAC, FALSE)) something = 1;
       }
    }

	/* nothing happened */
	if (something != 1)
	{
		/* Flush */
		if (flush_failure) flush();

		/* Message (if didn't already get a message) */
		if (something == 0) msg_format("%s %s seems unchanged.",
	           ((item >= 0) ? "Your" : "The"), o_name);
	           
	    return TRUE;
	}
	
	return TRUE;
}



/*
 * Hook to specify an item which can be mimmiced by the alchemy spell
 * (almost the same as the bigwand hook, but cannot mimmic artifacts)
 */
bool item_tester_hook_mimmic(const object_type *o_ptr)
{
	/* can never mimmic an unaware item */
	if (!object_aware_p(o_ptr)) return (FALSE);

	if (o_ptr->tval == TV_WAND)
	{
		/* check type */
		switch (o_ptr->sval)
		{
			/* most, but not all wands can be enhanced */
			case SV_WAND_HEAL_MONSTER:
			case SV_WAND_LITE:
			case SV_WAND_SLEEP_MONSTER:
			case SV_WAND_SLOW_MONSTER:
			case SV_WAND_CONFUSE_MONSTER:
			case SV_WAND_FEAR_MONSTER:
			case SV_WAND_POLYMORPH:
			case SV_WAND_DRAIN_LIFE:
			case SV_WAND_STINKING_CLOUD:
			case SV_WAND_MAGIC_MISSILE:
			case SV_WAND_ACID_BOLT:
			case SV_WAND_ELEC_BOLT:
			case SV_WAND_FIRE_BOLT:
			case SV_WAND_COLD_BOLT:
			case SV_WAND_ACID_BALL:
			case SV_WAND_ELEC_BALL:
			case SV_WAND_FIRE_BALL:
			case SV_WAND_COLD_BALL:
			case SV_WAND_DRAGON_FIRE:
			case SV_WAND_DRAGON_COLD:
			case SV_WAND_DRAGON_BREATH:
			case SV_WAND_STORMS:
				 return (TRUE);
		}
	}

	/* only a high level alchemist can mimmic items other than wands */
	if (p_ptr->lev < 30) return (FALSE);
	if (o_ptr->tval == TV_ROD)
	{
		/* check type */
		switch (o_ptr->sval)
		{
			case SV_ROD_RECALL:
			case SV_ROD_ILLUMINATION:
			case SV_ROD_LITE:
			case SV_ROD_SLEEP_MONSTER:
			case SV_ROD_SLOW_MONSTER:
			case SV_ROD_POLYMORPH:
			case SV_ROD_DRAIN_LIFE:
			case SV_ROD_ACID_BOLT:
			case SV_ROD_ELEC_BOLT:
			case SV_ROD_FIRE_BOLT:
			case SV_ROD_COLD_BOLT:
			case SV_ROD_ACID_BALL:
			case SV_ROD_ELEC_BALL:
			case SV_ROD_FIRE_BALL:
			case SV_ROD_COLD_BALL:
				 return (TRUE);
		}
	}
	/* (unlike the enhance spell, artifacts and rings cannot be mimmiced) */

	/* else */ return (FALSE);
}


/*
 * Use the mimmiced object or check which object you are currently mimmicking.
 * justcheck = FALSE is called by the mimmic_zap spell
 * justcheck = TRUE is called by mimmic_wand() and always returns false
 */
bool zap_mimmic(bool justcheck)
{
	int sval, dir;
	bool isrod;

	/* ident is unused but needed to call wand effect */
	bool ident;

	/* haven't chosen an object to mimmic */
	if (!p_ptr->mimmic)
	{
		msg_print("You have not chosen an object to mimmic.");
		if (justcheck) return FALSE;
	
		/* spell uses no mana when you choose 'no' to the question */
		if (get_check("Mimmic a random wand? (answering 'no' cancels the spell) "))
		{
			/* random low-level wand (weaker than wonder: no ball effects but also no bad effects) */
			p_ptr->mimmic = 7 + rand_int(11);
			/* skip drain life (need to chose a wand to mimmic for this spell to be powerful) */
			if (p_ptr->mimmic == 12) p_ptr->mimmic += 1;
		}
		else return FALSE;
		/* could combine these into one spell, but then you'd have to ask */
		/* mimmic a new wand?, check on current mimmicked wand?, */
		/* or zap mimmicked wand? */
		/* if (!mimmic_wand()) return FALSE; */
	}

	/* sval of the wand to mimmic */
	if (p_ptr->mimmic < 100)
	{
		sval = p_ptr->mimmic;
		isrod = FALSE;
	}
	/* or -100 if it's a rod */
	else
	{
		sval = p_ptr->mimmic - 100;
		isrod = TRUE;
	}

	/* just checking what we've chosen to mimmic as a reminder */
	if (justcheck)
	{
		int kidx;
		object_kind *k_ptr;
		cptr mname;

		/* get the object kind (without having a real object) */
		if (isrod) kidx = lookup_kind(TV_ROD, sval);
		else kidx = lookup_kind(TV_WAND, sval);
		k_ptr = &k_info[kidx];

		/* describe without using object_desc() function */
		/* because we don't have an o_ptr */
		mname = (k_name + k_ptr->name);
		if (p_ptr->menhance)
		{
			if (isrod) msg_format("You are currently mimmicing a rod of %s enhanced by %d.", mname, p_ptr->menhance);
			else msg_format("You are currently mimmicing a wand of %s enhanced by %d.", mname, p_ptr->menhance);
		}
		else
		{
			if (isrod) msg_format("You are currently mimmicing a rod of %s.", mname);
			else msg_format("You are currently mimmicing a wand of %s.", mname);
		}

#if notsureifIwantthis
		if (p_ptr->lev >= 30)
		{
			if (get_check("Cancel this and mimmic a random wand instead? "))
			{
				/* cancel */
				p_ptr->mimmic = 0;
				p_ptr->menhance = 0;
			}
		}
#endif

		/* no mana cost for checking */
		return FALSE;
	}

	/* Get a direction, allow cancel */
	if (!get_aim_dir(&dir)) return FALSE;

	/* use the fake object */
	/* (both of these functions would always return TRUE */
	/*  because you can't mimmic a rod of identify) */
	if (isrod) rod_effect(sval, &ident, p_ptr->menhance, dir);
	else wand_effect(sval, &ident, p_ptr->menhance, dir);

	/* enhancement still gets used up with use (but slower) */
	if ((randint(100) < 33) && (p_ptr->menhance))
	{
		if (p_ptr->menhance > 2) p_ptr->menhance -= randint(2);
		else p_ptr->menhance -= 1;
	}

	return TRUE;
}


/*
 * Mimmic wand alchemy spell
 * any wand or rod which can be enhanced by the enhance wand spell
 * can be mimmiced by this spell (but not rings or artifacts)
 */
bool mimmic_wand(void)
{
	cptr q, s;
	int item;

	object_type *o_ptr;
	char o_name[80];
	
	/* spell uses no mana when you choose 'no' to the question */
	if (!get_check("Mimmic a new object? (y=yes, n=check current mimmiced object) "))
	{
		/* just checks on which wand is currently mimmicked */
		(void)zap_mimmic(TRUE);
		return FALSE;
	}

	item_tester_hook = item_tester_hook_mimmic;

	/* Get an item */
	if (p_ptr->lev < 30) q = "Mimmic which wand? ";
	else q = "Mimmic which wand or rod? ";
	s = "You have nothing that this spell can mimmic.";  
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return (FALSE);

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}
	
	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}
	
	/* can't mimmic an unaware object */
	if (!object_aware_p(o_ptr))
	{
		msg_print("You can't mimmic an object if you don't know what it does!");
		return FALSE;
	}

	/* save sval of wand or rod to mimmic */
	if (o_ptr->tval == TV_WAND)
	{ 
		p_ptr->mimmic = o_ptr->sval;

		/* use a charge */
		if (o_ptr->charges) o_ptr->charges--;
	}
	/* +100 means it's a rod */
	else /*(o_ptr->tval == TV_ROD)*/
	{
		object_kind *k_ptr = &k_info[o_ptr->k_idx];
		p_ptr->mimmic += 100;

		/* drain the charge */
		o_ptr->timeout += k_ptr->pval;
	}

	/* if mimmicked object is enhanced, then enhance the mimmiced effect */
	p_ptr->menhance = o_ptr->enhance;

	/* Description */
	object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 1);
	msg_format("You are mimmicing %s", o_name);

	/* done */
	return TRUE;
}


/** next few functions were moved here from x-spell.c **/

/* This spell should become more useful (more
 * controlled) as the player gains experience levels.
 * Thus, add 1/5 of the player's level to the die roll.
 * This eliminates the worst effects later on, while
 * keeping the results quite random.  It also allows
 * some potent effects only at high level.
 */
void spell_wonder(int dir)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
	int plev = p_ptr->lev;
	int die = randint(100) + plev / 5;
	int beam = p_ptr->lev / 2;
	if (cp_ptr->flags & CF_BEAM) beam = p_ptr->lev;

	if (die > 100)
		msg_print("You feel a surge of power!");
	if (die < 8) clone_monster(dir);
	else if (die < 14) speed_monster(dir);
	else if (die < 26) heal_monster(dir, damroll(4, 6));
	else if (die < 31) poly_monster(dir, plev + 1);
	else if (die < 36)
		fire_bolt_or_beam(beam - 10, GF_MISSILE, dir,
		                  damroll(3 + ((plev - 1) / 5), 4));
	else if (die < 41) confuse_monster(dir, (plev + 2 + adj_chr_charm[p_ptr->stat_ind[A_CHR]]));
	else if (die < 46) fire_ball(GF_POIS, dir, 20 + (plev / 2), 3);
	else if (die < 51) lite_line(dir, 10-(plev/5));
	else if (die < 56)
		fire_beam(GF_ELEC, dir, damroll(3+((plev-5)/6), 6));
	else if (die < 61)
		fire_bolt_or_beam(beam-10, GF_COLD, dir,
		                  damroll(5+((plev-5)/4), 8));
	else if (die < 66)
		fire_bolt_or_beam(beam, GF_ACID, dir,
		                  damroll(6+((plev-5)/4), 8));
	else if (die < 71)
		fire_bolt_or_beam(beam, GF_FIRE, dir,
		                  damroll(8+((plev-5)/4), 8));
	else if (die < 76) drain_life(dir, 75);
	else if (die < 81) fire_ball(GF_ELEC, dir, 30 + plev / 2, 2);
	else if (die < 86) fire_ball(GF_ACID, dir, 40 + plev, 2);
	else if (die < 91) fire_ball(GF_ICE, dir, 70 + plev, 3);
	else if (die < 96) fire_ball(GF_FIRE, dir, 80 + plev, 3);
	else if (die < 101) drain_life(dir, 100 + plev);
	else if (die < 104) earthquake(py, px, 12, 0, 0, FALSE);
	else if (die < 106) destroy_area(py, px, 15, TRUE);
	else if (die < 108) banishment();
	else if (die < 110) dispel_monsters(120);
	else /* RARE */
	{
		dispel_monsters(150);
		slow_monsters(60);
		sleep_monsters(60);
		hp_player(300);
	}
}


/* affect self tourist spell */
void spell_affect_self(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
	int plev = p_ptr->lev;
	int die, dist;
	bool controlled;

    die = randint(70) + randint(p_ptr->luck) + randint(plev/6); /* healing / damage */
    if (spellswitch == 30) die += 4 + randint(6);
    if (die < 40) take_hit(randint((plev * 3)/2), "a backfiring spell");
    if (die > 45) (void)hp_player(randint(plev));
    if (die > 65) (void)hp_player(randint(plev));
    if (die > 90) (void)hp_player(randint(plev));
    die = randint(70) + randint(p_ptr->luck) + randint(plev/6); /* chance of teleport */
    if (die < 25)
    {
        msg_print("Your position feels uncertain.");
        dist = damroll(2, plev/3 + 1);
        if (die < 15) dist = damroll(plev/2, 4);
        
	    controlled = FALSE;
		/* controlled teleport (random if you don't chose a target) */
		if (p_ptr->telecontrol)
		{
            if (control_tport(110 - plev, dist + 30)) controlled = TRUE;
            if (!controlled) msg_print("You fail to control the teleportation.");
        }

        if (!controlled) teleport_player(dist);
    }
    die = randint(70) + randint(p_ptr->luck) + randint(plev/6); /* chance of bad timed effect */
    if (spellswitch == 30) die += 3 + randint(7);
    if (die < 5) (void)inc_timed(TMD_CHARM, randint(16) + 12);
    else if (die < 10) (void)inc_timed(TMD_CONFUSED, randint(19) + 11);
    else if (die < 15) (void)inc_timed(TMD_BLIND, randint(25) + 25);
    else if (die < 23) (void)inc_timed(TMD_AMNESIA, randint(19) + 11);
    else if (die < 27) (void)inc_timed(TMD_IMAGE, randint(19) + 11);
    else if (die < 33) (void)inc_timed(TMD_POISONED, randint(30) + 20);
    else if (die < 37)
    {
		   msg_print("You feel your life slipping away!");
		   lose_exp(100 + randint(die * 101));
    }
    die = randint(65) + randint(p_ptr->luck) + randint(plev/5); /* curing */
    if (spellswitch == 30) die += 3 + randint(7);
    if (die > 100)
    {
	     if (p_ptr->silver > PY_SILVER_HEALTHY) p_ptr->silver -= 7;
	     if (p_ptr->silver < PY_SILVER_HEALTHY) p_ptr->silver = PY_SILVER_HEALTHY;
    }
    if (die > 95)
    {
         if (p_ptr->slime > PY_SLIME_HEALTHY) p_ptr->slime -= 10;
	     if (p_ptr->slime < PY_SLIME_HEALTHY) p_ptr->slime = PY_SLIME_HEALTHY;
    }
    if (die > 90) (void)clear_timed(TMD_CHARM);
    if (die > 80) (void)clear_timed(TMD_AMNESIA);
    if (die > 75)
    {
         (void)clear_timed(TMD_CONFUSED);
         (void)clear_timed(TMD_BLIND);
         (void)clear_timed(TMD_FRENZY);
    }
    if (die > 60) (void)clear_timed(TMD_STUN);
    if (die > 55) (void)clear_timed(TMD_CUT);
    if (die > 50) (void)clear_timed(TMD_POISONED);
    die = randint(67) + randint(p_ptr->luck) + randint(plev/6); /* nourishment */
    if (spellswitch == 30) die += 3 + randint(7);
    if (die > 74) (void)set_food(p_ptr->food + ((randint(plev + die)) * 6)); 
    if (die < 26) (void)set_food(p_ptr->food - ((randint(plev + die)) * 11));  
    die = randint(99) + randint(plev/5); /* speed adjustment */
    if ((die < 34) && (!p_ptr->timed[TMD_SUST_SPEED]))
    {
         int adjust = (randint(25) - 11);
         if (randint(100) < plev) adjust += 2;
         if (randint(501) < plev) adjust += randint(3);
         if ((adjust == 0) && (randint(60) < plev)) adjust = randint(13);
         else if (adjust == 0) adjust = (randint(9) - 10);
         p_ptr->spadjust = adjust;
	    if (p_ptr->spadjust > 0) (void)set_timed(TMD_ADJUST, randint(25) + plev);
		if (p_ptr->spadjust < 0) (void)set_timed(TMD_ADJUST, (plev + 25) - randint(plev));
    }
    die = randint(70) + randint(p_ptr->luck) + randint(plev/6); /* chance to cancel good effects */
    if (spellswitch == 30) die += 10;
    if (die < 12)
    {
         msg_print("You feel rather mundane.");
         (void)clear_timed(TMD_IMAGE);
         (void)clear_timed(TMD_PROTEVIL);
         (void)clear_timed(TMD_HERO);
         (void)clear_timed(TMD_SHERO);
         (void)clear_timed(TMD_BLESSED);
         (void)clear_timed(TMD_SANCTIFY);
         (void)clear_timed(TMD_SHADOW);
         p_ptr->silver += 1;
    }
    else if (die < 16)
    {
         msg_print("You feel vulnerable.");
         (void)clear_timed(TMD_SHIELD);
         (void)clear_timed(TMD_WSHIELD);
         (void)clear_timed(TMD_OPP_POIS);
         (void)clear_timed(TMD_OPP_ACID);
         (void)clear_timed(TMD_OPP_COLD);
         (void)clear_timed(TMD_OPP_FIRE);
         (void)clear_timed(TMD_OPP_ELEC);
         if (!p_ptr->timed[TMD_SUST_SPEED]) (void)clear_timed(TMD_FAST);
         if ((p_ptr->spadjust) && (!p_ptr->timed[TMD_SUST_SPEED])) p_ptr->spadjust -= 1;
    }
    else if (die < 20)
    {
       msg_print("Your eyes hurt for a moment.");
       (void)inc_timed(TMD_BLIND, randint(3));
       (void)clear_timed(TMD_TSIGHT);
       (void)clear_timed(TMD_SINVIS);
       (void)clear_timed(TMD_SINFRA);
       (void)clear_timed(TMD_BRAIL);
       p_ptr->slime += 1;
    }
    if ((die < 25) && (!p_ptr->timed[TMD_SUST_SPEED])) (void)inc_timed(TMD_SLOW, randint(25) + 15);
    die = randint(70) + randint(p_ptr->luck) + randint(plev/5); /* chance for buffs */
    if (spellswitch == 30) die += 4 + randint(6);
    if (die > 100) (void)inc_timed(TMD_SANCTIFY, randint(die / 2) + (25));
    else if ((die > 95) && (!p_ptr->peace)) (void)inc_timed(TMD_HERO, randint(die / 2) + (25));
    else if ((die > 90) && (!p_ptr->peace)) (void)inc_timed(TMD_SHERO, randint(die / 2) + (25));
    else if (die > 85) (void)inc_timed(TMD_BLESSED, randint(die) + (25));
    else if (die > 80) (void)inc_timed(TMD_WSHIELD, randint(die) + (25));
    else if (die > 75) (void)inc_timed(TMD_SHADOW, randint(die) + (25));
    else if (die > 70) (void)inc_timed(TMD_PROTEVIL, randint(die / 2) + (25));
    die = randint(70) + randint(p_ptr->luck) + randint(plev/5); /* chance for resistances */
    if (spellswitch == 30) die += 3 + randint(7);
    if (die > 98)
    {
		 int time = randint(20) + 20;
         (void)inc_timed(TMD_OPP_POIS, time);
         (void)inc_timed(TMD_OPP_ACID, time);
         (void)inc_timed(TMD_OPP_ELEC, time);
         (void)inc_timed(TMD_OPP_COLD, time);
         (void)inc_timed(TMD_OPP_FIRE, time);
    }
    else if (die > 90)
    {
	     int time = randint(20) + 20;
         (void)inc_timed(TMD_OPP_ACID, time);
         (void)inc_timed(TMD_OPP_ELEC, time);
    }
    else if (die > 82)
    {
	     int time = randint(20) + 20;
         (void)inc_timed(TMD_OPP_COLD, time);
         (void)inc_timed(TMD_OPP_FIRE, time);
    }
    else if (die > 74) (void)inc_timed(TMD_OPP_POIS, randint(21) + 21);
    die = randint(68) + randint(p_ptr->luck) + randint(plev/5); /* chance for enhanced sight / detection */
    if (spellswitch == 30) die += 3 + randint(7);
    if (die > 99) (void)inc_timed(TMD_ESP, randint(die * 2) + (25));
    else if (die > 89) (void)inc_timed(TMD_TSIGHT, randint(die * 2) + (25));
    else if (die > 85) (void)inc_timed(TMD_BRAIL, randint(die * 2) + (25));
    else if (die > 75) (void)inc_timed(TMD_SINVIS, randint(die * 2) + (25));
    else if (die > 65) (void)inc_timed(TMD_SINFRA, randint(die * 2) + (25));
    else if ((die > 55) && (!p_ptr->timed[TMD_2ND_THOUGHT])) (void)detect_monsters_normal(1);
    else if ((die > 50) && (!p_ptr->timed[TMD_2ND_THOUGHT])) (void)detect_monsters_invis();
    else if ((die > 45) && (!p_ptr->timed[TMD_2ND_THOUGHT])) (void)detect_traps();
    else if ((die > 42) && (!p_ptr->timed[TMD_2ND_THOUGHT])) (void)detect_doorstairs(TRUE);
    else if ((die > 36) && (!p_ptr->timed[TMD_2ND_THOUGHT])) (void)detect_doorstairs(FALSE);
    else if ((die > 32) && (!p_ptr->timed[TMD_2ND_THOUGHT])) (void)detect_objects_magic();
    else if ((die > 27) && (!p_ptr->timed[TMD_2ND_THOUGHT])) (void)detect_treasure();
    else if ((die > 22) && (!p_ptr->timed[TMD_2ND_THOUGHT])) (void)detect_objects_normal(FALSE);

    die = randint(100) + randint(plev/6); /* chance for luck modification */
	if ((die > 98) && (p_ptr->luck < 37) && (randint(100) < 60))
	{
		p_ptr->luck += randint(2);
		msg_print("You feel lucky.");
	}
	else if ((die > 94) && (p_ptr->luck < 37) && (p_ptr->luck < p_ptr->maxluck - 1))
	{
		if (p_ptr->luck < p_ptr->maxluck - 5) p_ptr->luck += 1 + randint(p_ptr->maxluck - p_ptr->luck);
		else p_ptr->luck += 2;
		if (p_ptr->luck < 20) msg_print("You feel less unlucky.");
		else msg_print("You feel like you've found something that was lost.");
	}
	else if ((die < 3) && (p_ptr->luck > 4)) p_ptr->luck -= 2;
	else if ((die < 10) && (p_ptr->luck > 4)) p_ptr->luck -= 1;
	if ((die < 10) && (p_ptr->luck > 4)) msg_print("You feel unlucky.");
}

/* adjust curse tourist spell */
void spell_adjust_curse(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
	int plev = p_ptr->lev;
	int die, dir;

    die = randint(99) + plev/5 + goodluck/2 - badluck/2;
    /* adjust when called by bizzare effects spell */
    if (spellswitch == 30) die += 1 + randint(9 + goodluck/3); 
    
    if (die < 4)
    {
         /* makes weapon a morgul weapon or else curse armor */
         if (!curse_weapon(5)) curse_armor(FALSE);
    }
    else if (die < 25)
    {
         if (die < 10) curse_weapon(4);
         else if (die < 15) curse_weapon(3);
         else if (die < 21) curse_weapon(2);
         else curse_weapon(1);
    }
    else if (die < 35)
    {
         curse_armor(TRUE);
    }
    else if (die < 75)
    {
         remove_curse();
    }
    else if (die < 85)
    {
         remove_curse();
         msg_print("You fire a small ball of destroy cursed objects.");
		 if (!get_aim_dir(&dir)) return;
		 fire_ball(GF_HOLY_ORB, dir, 0, (plev/15));
    }
    else if (die < 95)
    {
         remove_curse();
		 if (!get_aim_dir(&dir)) return;
         msg_print("You fire a ball of destroy cursed objects.");
		 fire_ball(GF_HOLY_ORB, dir, randint(plev/5), (plev/7));
    }
    else if (die < 105)
    {
         remove_all_curse();
    }
    else 
    {
         remove_all_curse();
		 if (!get_aim_dir(&dir)) return;
         msg_print("You fire a ball of destroy cursed objects.");
		 fire_ball(GF_HOLY_ORB, dir, randint(plev/4), (plev/8));
    }
}

/* affect surroundings tourist spell */
void spell_affect_other(int dir)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
	int plev = p_ptr->lev;
	int die, olot;
	int thirty = 0;
	int lottery = randint(2);
	if (randint(100) < (plev/4)) lottery += 1;
	olot = lottery;
	if (randint(100) < (plev+2)/2) lottery += 1;
    if (spellswitch == 30) thirty = 1;

    /* damage effects */
    for (; lottery > 0; lottery -= 1)
    {
        die = randint(99) + randint(plev/5);
        if (thirty == 1) die += 3 + randint(7);
		/* most of this part replaced by luck attack spell */
        if (die > 98) project_los(GF_CHAOS, randint(plev * (1 + olot)));
        else if (die > 92) project_los(GF_NEXUS, randint(plev * (1 + olot)));
        else if (die > 85) project_los(GF_WATER, randint(plev * (1 + olot)));
    }
    
    /* other monster effects */
    if ((olot > 1) && (plev < 30)) olot -= 1;
    for (;olot > 0; olot -= 1)
    {
        die = randint(99) + randint(plev/4);
        if (thirty == 1) die += 3 + randint(7);
        if (die < 10) aggravate_monsters(0);
        else if (die < 19) project_los(GF_OLD_HEAL, 45 + randint(45) + (badluck*2) - plev/2);
        else if (die < 28) 
        {
             /* make sure it doesn't clone more than once */
             if (olot == 1)
             {
                 project_los(GF_OLD_CLONE, plev);
             }
             else
             {
                 spellswitch = 13; /* blink monsters */
                 project_los(GF_AWAY_ALL, 2 + randint(15));
             }
        }
        else if (die < 33) 
        {
           spellswitch = 13; /* blink monsters */
           project_los(GF_AWAY_ALL, 2 + randint(15));
        }
        else if (die < 38) 
        {
           (void)summon_chosen();
        }
        else if (die < 48) project_los(GF_OLD_CONF, 45 + plev);
        else if (die < 58) project_los(GF_OLD_SLEEP, 10 + plev);
        else if (die < 68) project_los(GF_TURN_ALL, 55 + plev);
        else if (die < 78) project_los(GF_OLD_SLOW, 45 + plev);
        else if (die < 88) project_los(GF_AWAY_UNDEAD, plev + 10); /* banish unnatural */
        else if (die < 98) project_los(GF_AWAY_EVIL, plev + 10); /* banish evil */
        else if (die < 108) project_los(GF_AWAY_ALL, plev + 10); /* mass banishment */
        else /* die > 107 */ banishment();
    }
    
    if (randint(100) < 50) /* destruct/create effects */
    {
       die = randint(99) + randint(plev/5);
       if (thirty == 1) die += 3 + randint(7);
       if (die < 8)
       {
          fire_ball(GF_MAKE_TRAP, 1, 9, randint(3)); /* lots of traps.. */
          fire_ball(GF_MAKE_TRAP, 2, 9, randint(3));
          fire_ball(GF_MAKE_TRAP, 3, 9, randint(3));
          fire_ball(GF_MAKE_TRAP, 4, 9, randint(3));
          fire_ball(GF_MAKE_TRAP, 5, 9, randint(3));
          fire_ball(GF_MAKE_TRAP, 6, 9, randint(3));
          fire_ball(GF_MAKE_TRAP, 7, 9, randint(3));
          fire_ball(GF_MAKE_TRAP, 8, 9, randint(3));
          fire_ball(GF_MAKE_TRAP, 9, 9, randint(3));
       }
       else if (die < 16)
       {
          fire_ball(GF_KILL_DOOR, 1, 0, 4); /* trap/door destruction */
          fire_ball(GF_KILL_DOOR, 2, 0, 4); 
          fire_ball(GF_KILL_DOOR, 3, 0, 4); 
          fire_ball(GF_KILL_DOOR, 4, 0, 4); 
          fire_ball(GF_KILL_DOOR, 5, 0, 4); 
          fire_ball(GF_KILL_DOOR, 6, 0, 4); 
          fire_ball(GF_KILL_DOOR, 7, 0, 4); 
          fire_ball(GF_KILL_DOOR, 8, 0, 4); 
          fire_ball(GF_KILL_DOOR, 9, 0, 4); 
       }
       else if (die < 24)
       {
          fire_ball(GF_KILL_WALL, 1, (plev*2), randint(3)); /* destroy walls */
          fire_ball(GF_KILL_WALL, 2, (plev*2), randint(3)); 
          fire_ball(GF_KILL_WALL, 3, (plev*2), randint(3)); 
          fire_ball(GF_KILL_WALL, 4, (plev*2), randint(3)); 
          fire_ball(GF_KILL_WALL, 5, (plev*2), randint(3)); 
          fire_ball(GF_KILL_WALL, 6, (plev*2), randint(3)); 
          fire_ball(GF_KILL_WALL, 7, (plev*2), randint(3)); 
          fire_ball(GF_KILL_WALL, 8, (plev*2), randint(3)); 
          fire_ball(GF_KILL_WALL, 9, (plev*2), randint(3)); 
       }
       else if (die < 40)
       {
          fire_ball(GF_KILL_WALL, randint(8), 0, 1); 
          fire_ball(GF_KILL_WALL, randint(8), 0, 1); 
          fire_ball(GF_KILL_WALL, randint(8), 0, 1); 
          fire_ball(GF_KILL_WALL, randint(8), 0, 1); 
          fire_ball(GF_MAKE_DOOR, 1, 0, 1); 
          fire_ball(GF_MAKE_DOOR, 2, 0, 1); 
          fire_ball(GF_MAKE_DOOR, 3, 0, 1); 
          fire_ball(GF_MAKE_DOOR, 4, 0, 1); 
          fire_ball(GF_MAKE_DOOR, 5, 0, 1); 
          fire_ball(GF_MAKE_DOOR, 6, 0, 1); 
          fire_ball(GF_MAKE_DOOR, 7, 0, 1); 
          fire_ball(GF_MAKE_DOOR, 8, 0, 1); 
          fire_ball(GF_MAKE_DOOR, 9, 0, 1); 
          fire_ball(GF_MAKE_DOOR, randint(8), 0, 3); 
       }
       else if (die < 48)
       {
          if (randint(100) < 90) fire_ball(GF_WIZLOCK, 1, 0, 1); 
          if (randint(100) < 90) fire_ball(GF_WIZLOCK, 2, 0, 1); 
          if (randint(100) < 90) fire_ball(GF_WIZLOCK, 3, 0, 1); 
          if (randint(100) < 90) fire_ball(GF_WIZLOCK, 4, 0, 1); 
          if (randint(100) < 80) fire_ball(GF_WIZLOCK, 5, 0, 1); 
          if (randint(100) < 80) fire_ball(GF_WIZLOCK, 6, 0, 1); 
          if (randint(100) < 80) fire_ball(GF_WIZLOCK, 7, 0, 1); 
          if (randint(100) < 80) fire_ball(GF_WIZLOCK, 8, 0, 1); 
          if (randint(100) < 80) fire_ball(GF_WIZLOCK, 9, 0, 1); 
       }
       else if (die < 53)
       {
          fire_ball(GF_MANA, 1, 1, 5); /* destroy objects */
          fire_ball(GF_MANA, 2, 1, 5); 
          fire_ball(GF_MANA, 3, 1, 5); 
          fire_ball(GF_MANA, 4, 1, 5); 
          fire_ball(GF_MANA, 5, 1, 5); 
          fire_ball(GF_MANA, 6, 1, 5); 
          fire_ball(GF_MANA, 7, 1, 5); 
          fire_ball(GF_MANA, 8, 1, 5); 
          fire_ball(GF_MANA, 9, 1, 5); 
       }
       else if (die < 58)
       {
          fire_ball(GF_HOLY_ORB, 1, 0, 5); /* destroy cursed objects */
          fire_ball(GF_HOLY_ORB, 2, 0, 5); 
          fire_ball(GF_HOLY_ORB, 3, 0, 5); 
          fire_ball(GF_HOLY_ORB, 4, 0, 5); 
          fire_ball(GF_HOLY_ORB, 5, 0, 5); 
          fire_ball(GF_HOLY_ORB, 6, 0, 5); 
          fire_ball(GF_HOLY_ORB, 7, 0, 5); 
          fire_ball(GF_HOLY_ORB, 8, 0, 5); 
          fire_ball(GF_HOLY_ORB, 9, 0, 5); 
       }
       else if (die < 70)
	   {
		   int quake = 85;
		   if (randint(plev + 50) > 46) quake -= randint(plev/2);
		   earthquake(py, px, randint(13), quake, 1, FALSE);
	   }
       else if (die < 78) destroy_area(py, px, 4 + randint(14), TRUE);
       else if (die > 100)
       {
			msg_print("The world changes!");
			/* Leaving (alter reality) */
			p_ptr->leaving = TRUE;
       }
    }
    
    /* mapping & light/dark effects */
    die = randint(99) + randint(plev/5);
    if (thirty == 1) die += 3 + randint(7);
    if (die < 30) (void)lite_area(4, (plev / (4 + randint(5))) + 1);
    else if (die < 50) /* call dark */
    {
       /* most of the time use GF_DARK instead of DARK_WEAK */
       if (randint(100) < 75 + goodluck/2) (void)unlite_area(4, (plev / (4 + randint(5))) + 1, TRUE);
	   else (void)unlite_area(4, (plev / (4 + randint(5))) + 1, FALSE);
    }
    if (die == 109) wiz_lite(TRUE);
    else if (die > 103) wiz_lite(FALSE);
    else if (die > 98) map_area(200);
	else if ((die < 78) && (rand_int(100) < 20)) 
		map_area(40 + goodluck*2 - badluck + randint(goodluck + 4));
    
    /* other effects (may add more later if I think of them) */
    die = randint(99) + randint(plev/5);
    if (thirty == 1) die += 3 + randint(7);
    if (die > 95)
	{
	   do_telekinesis(9 + goodluck/2);
	}
	else if (die > 90) /* create good object */
	{
         int where = randint(100);
	     if (where < 25) acquirement(py + randint(4), px + randint(4), 1, FALSE);
         else if (where < 50) acquirement(py + randint(4), px - randint(4), 1, FALSE);
         else if (where < 75) acquirement(py - randint(4), px + randint(4), 1, FALSE);
         else acquirement(py - randint(4), px - randint(4), 1, FALSE);
    }
	else if (die > 85)
	{
         int where = randint(100);
         int aquirenum = randint(2) + 1;
         spellswitch = 5;  /* creates object that's not good or great */
         if (where < 25) acquirement(py + randint(4), px + randint(4), aquirenum, FALSE);
         else if (where < 50) acquirement(py + randint(4), px - randint(4), aquirenum, FALSE);
         else if (where < 75) acquirement(py - randint(4), px + randint(4), aquirenum, FALSE);
         else acquirement(py - randint(4), px - randint(4), aquirenum, FALSE);
         spellswitch = 0;
    }
}



/*
 * Summon a creature of the specified type
 * copied from wizard2.c
 * This function is rather dangerous? why?
 */
void do_call_help(int r_idx)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int i, x, y;
	monster_race *r_ptr = &r_info[r_idx];

	/* Paranoia */
	if (!r_idx) return;
	if (r_idx >= z_info->r_max-1) return;
	
	/* extinct */
	if ((r_ptr->curpop + r_ptr->cur_num >= r_ptr->maxpop) && (r_ptr->maxpop)) return;

	/* Try 10 times */
	for (i = 0; i < 10; i++)
	{
		int d = 1;

		/* Pick a location */
		scatter(&y, &x, py, px, d, 0);

		/* Require empty grids */
		if (!cave_can_occupy_bold(y, x)) continue;

		/* Place it (don't allow groups) */
		if (place_monster_aux(y, x, r_idx, TRUE, FALSE)) break;
	}
}


/*
 * choose a monster appropriate for an invasion
 */
static bool invasion_okay(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];
	
	if ((p_ptr->depth > 3) && ((r_ptr->level < 2) || (r_ptr->level < p_ptr->depth / 6))) 
		return (FALSE);
	
	/* dL1 monsters can come up the stairs to the town now, but forbid breeders */
	if ((!p_ptr->depth) && (r_ptr->flags2 & (RF2_MULTIPLY))) return (FALSE);

	/* NEVER_MOVE and WATER_ONLY monsters don't go up/down stairs */
	if ((r_ptr->flags1 & (RF1_NEVER_MOVE)) || (r_ptr->flags7 & (RF7_WATER_ONLY))) 
		return (FALSE);
	
    /* restrict to appropriate theme */	
	if (!theme_okay(r_idx, 1, FALSE))
	
	/* letting these monsters generate on the nearest stairs automatically is mean */
	/* so only do it on deeper levels */
	if (((r_ptr->flags2 & (RF2_PASS_WALL)) || (r_ptr->flags2 & (RF2_INVISIBLE)) ||
		(r_ptr->flags2 & (RF2_KILL_WALL))) && (p_ptr->depth < 56 - badluck - rand_int(31))) 
			return (FALSE);

	/* Require certain types */
	if ((p_ptr->depth < 9) && (strchr("k", r_ptr->d_char))) return (TRUE);
	if ((p_ptr->depth < 34) && (strchr("JRt", r_ptr->d_char))) return (TRUE);
	if (strchr("CDEFIJQbdjkmt,", r_ptr->d_char)) return (FALSE);
	if ((p_ptr->depth < 30) && (strchr("Xv%", r_ptr->d_char))) return (FALSE);

	return (TRUE);
}


/* invasion trap (creates some monsters on a nearby staircase) 
 * (possibly later stagger this so monsters appear one at a time)
 * mode == 0 check to see if we can do the invasion trap
 * mode == 1 do the invasion trap
 * mode == -1 check to see it we can place a randomly generated monster on a nearby staircase
 * mode == 2 place a randomly generated monster on a nearby staircase
 */
bool invasion(int mode)
{
	int y, x, k, some, i = 0;
    int stx = 0, sty = 0, stx2 = 0, sty2 = 0;
	int x1, x2, y1, y2;
	int py = p_ptr->py;
	int px = p_ptr->px;
	bool groupmon = FALSE;
	bool loner = FALSE;
	bool sameroom = FALSE;
	int r_idx;
	int ilvl = p_ptr->depth;
	monster_race *r_ptr;

	/* Pick an area */
	y1 = py - 21;
	y2 = py + 21;
	x1 = px - 25;
	x2 = px + 25;

	if ((mode == -1) || (mode == 2))
	{
		y1 = py - 28;
		y2 = py + 28;
		x1 = px - 32;
		x2 = px + 32;
	}

	if (y1 < 0) y1 = 0;
	if (x1 < 0) x1 = 0;
	
	/* Scan the dungeon */
	for (y = y1; y < y2; y++)
	{
		for (x = x1; x < x2; x++)
		{
			if (!in_bounds_fully(y, x)) continue;

			/* look for stairs */
			if ((cave_feat[y][x] == FEAT_LESS) || (cave_feat[y][x] == FEAT_MORE))
			{
				/* only place a random monster on a visible staircase if we can place it exactly */
				if (((mode == -1) || (mode == 2)) && (player_has_los_bold(y, x)) &&
					(!cave_can_occupy_bold(y, x))) continue;
				/* mode < 1 just checks to see if we can do the invasion */
				if (mode < 1) return TRUE;
				
				if (stx > 0) /* if we've already found stairs check which one is closer */
				{
					int d = distance(p_ptr->py, p_ptr->px, y, x);
					int db = distance(p_ptr->py, p_ptr->px, sty, stx);
					if ((d < db) && ((!(mode == 2)) || (randint(100) < 45)))
					{
						stx = x;
						sty = y;
					}
					break;
				}
				/* save the stairs */
				stx = x;
				sty = y;
			}
		}
	}
	if (stx < 1) return FALSE;
				
	if (cave_feat[sty][stx] == FEAT_MORE) ilvl += randint(2);
	/* This will occationally allow town monsters to wander into the dungeon */
	if ((cave_feat[sty][stx] == FEAT_LESS) && (randint(100) < 12)) ilvl--;
	/* loner prevents whole groups of urchins going into the dungeon */
	if (ilvl == 0) loner = TRUE;
	/* remember if the stairs are in the same room as the PC */
	if ((player_has_los_bold(sty, stx)) || (in_same_room(p_ptr->py, p_ptr->px, sty, stx)))
		sameroom = TRUE;

	/* Require appropriate monsters */
	get_mon_num_hook = invasion_okay;

	/* Prepare allocation table */
	get_mon_num_prep();
		
	while (i++ < 999)
	{
		r_idx = get_mon_num(ilvl, FALSE);
		
		if (!r_idx) continue;

		/* check for maximum population */
		r_ptr = &r_info[r_idx];
		
		/* maximum population */
		if ((r_ptr->curpop + r_ptr->cur_num >= r_ptr->maxpop) && (r_ptr->maxpop)) continue;

		if ((r_ptr->flags1 & (RF1_ESCORT)) ||
			(r_ptr->flags2 & (RF2_ESCORT1)) ||
			(r_ptr->flags1 & (RF1_FRIENDS)) ||
			(r_ptr->flags2 & (RF2_FRIEND1))) groupmon = TRUE;
		else groupmon = FALSE;
		
		if ((mode == 2) && (!p_ptr->depth) && (r_ptr->level > p_ptr->max_depth/6 + 1)) continue;
		
#if notimedinvasion /* all monsters placed at once */
		/* don't randomly place a group all at once in view of the PC */
		if ((mode == 2) && (groupmon) && (sameroom)) continue;
#else
		if ((mode == 2) && (sameroom) &&
			((r_ptr->flags1 & (RF1_ESCORT)) || (r_ptr->flags2 & (RF2_ESCORT1)))) 
				continue;
#endif
		
		break;
	}

	/* Remove restriction */
	get_mon_num_hook = NULL;

	/* Prepare allocation table */
	get_mon_num_prep();

	/* no need for timed invasions when the stairs are out of line of sight */
	/* or if only one monster is appearing */
	if ((loner) || ((mode == 2) && ((!sameroom) || (!groupmon))))
	{
		if (loner) (void)place_monster_aux(sty, stx, r_idx, FALSE, FALSE);
		else (void)place_monster_aux(sty, stx, r_idx, FALSE, TRUE);
		return TRUE;
	}
		
	if (groupmon) some = 1;
	else some = 4 + randint(2);

#if notimedinvasion /* all monsters placed at once */
	/* place the 1st monster (allow groups) */
	(void)place_monster_aux(sty, stx, r_idx, FALSE, TRUE);

    /* summon kin of the 1st monster */
	summoner = r_idx;
	for (k = 0; k < some; k++)
	{
		/* Hack -- tweak for ghouls */
		if ((randint(100) < 58) && ((r_idx == 396) || (r_idx == 383))) summon_kin_type = 'z';
		/* Hack -- Set the letter of the monsters to summon */
		else summon_kin_type = r_ptr->d_char;
		summon_specific(sty, stx, ilvl, SUMMON_KIN);
	}
#else /* one monster emerges from the stairs at a time */

	/* place the 1st monster (don't allow groups) */
	(void)place_monster_aux(sty, stx, r_idx, FALSE, FALSE);
	if (groupmon) some = 4 + randint(5);
	if ((r_ptr->flags1 & (RF1_ESCORT)) || (r_ptr->flags2 & (RF2_ESCORT1))) some += 1;

	summoner = r_idx;
	if (((r_idx == 396) || (r_idx == 383)) && (randint(100) < 58)) summon_kin_type = 'z';
	/* Hack -- Set the letter of the monsters to summon */
	else summon_kin_type = r_ptr->d_char;
	/* just summon one for now, the rest will come later */
	/* summon_nogroups(sty, stx, ilvl, SUMMON_KIN); */
	/* invoke the others to come */
	(void)inc_timed(TMD_INVASION, some);
	invasionridx = r_idx;
	invasiony = sty;
	invasionx = stx;
	/* mode 2 is monster groups of the same race only */
	if (mode == 2) invasionmode = 2;
	else invasionmode = 1;
	
#endif
	
	if (player_can_see_bold(sty, stx)) msg_print("More monsters emerge from the stairs.");
	else msg_print("You hear the movement of monsters nearby.");
	return TRUE;
}


/* line of sight polymorph trap
 */
bool lospolytrap(int mode, int mnum)
{
	int i, x, y, d, tmp;
	monster_type *n_ptr;
	monster_race *r_ptr;
	char m_name[80];
	int oldhp, oldmax, oldmin, oldlev;
	bool change = FALSE;

	/* check for nearby monsters */
	for (i = 1; i < mon_max, mnum > 0; i++)
	{
		/* get target monster info */
		n_ptr = &mon_list[i];
		r_ptr = &r_info[n_ptr->r_idx];

		/* Paranoia -- Skip dead monsters and NONMONSTERs */
		if (!n_ptr->r_idx) continue;
		if (r_ptr->flags7 & (RF7_NONMONSTER)) continue;
		
		/* monster is temporarily dead (raise it now?) */
		if (n_ptr->temp_death) continue;
		
		/* uniques can't be polymorphed (but semi-uniques can) */
		if (r_ptr->flags1 & (RF1_UNIQUE)) continue;

		/* Location of target */
		y = n_ptr->fy;
		x = n_ptr->fx;

		/* Require line of sight */
		d = distance(p_ptr->py, p_ptr->px, y, x);
		if ((d > MAX_RANGE - 1) || (!los(p_ptr->py, p_ptr->px, y, x))) continue;

		/* Get the monster name (BEFORE polymorphing) */
		monster_desc(m_name, sizeof(m_name), n_ptr, 0);

		/* these monsters resist (but don't prevent the trap from casting the spell) */
		if (r_ptr->Rchaos >= 2)
		{
			msg_format("%^s is unaffected.", m_name);
			mnum -= 1;
			continue;
		}

		/* los polymorph trap allows the monsters no saving throw */
		/* because it prefers to change them into tougher monsters unlike the PC-cast version */

		/* Pick a new monster race */
		tmp = poly_r_idx(n_ptr->r_idx, 1);
		
		/* try again once */
		if (tmp == n_ptr->r_idx) tmp = poly_r_idx(n_ptr->r_idx + 1, 1);

		/* Handle polymorph */
		if (tmp != n_ptr->r_idx)
		{
			msg_format("%^s changes!", m_name);

			/* "Kill" the "old" monster */
			delete_monster_idx(cave_m_idx[y][x], FALSE);

			/* Create a new monster (no groups) */
			(void)place_monster_aux(y, x, tmp, FALSE, FALSE);
			/* (fully heals, project() polymorph doesn't do this anymore) */
			
			change = TRUE;
			mnum -= 1;
		}
		else mnum -= 1;
	}
	return change;
}


/* 
 * returns number of monsters in line of sight
 * mode is not yet used, but I imagine this function may be useful for other things in the future.
 * (currently the only mode is for the LOS polymorph trap.)
 */
int anylosmon(int mode)
{
	int i, x, y, d, num = 0;
	monster_type *n_ptr;
	monster_race *r_ptr;

	/* check for nearby monsters */
	for (i = 1; i < mon_max; i++)
	{
		/* get target monster info */
		n_ptr = &mon_list[i];
		r_ptr = &r_info[n_ptr->r_idx];

		/* Paranoia -- Skip dead monsters and NONMONSTERs */
		if (!n_ptr->r_idx) continue;
		if (r_ptr->flags7 & (RF7_NONMONSTER)) continue;
		
		/* monster is temporarily dead (raise it now?) */
		if (n_ptr->temp_death) continue;
		
		/* uniques can't be polymorphed (but semi-uniques can) */
		if (r_ptr->flags1 & (RF1_UNIQUE)) /* && (mode == )) */ continue;

		/* Location of target */
		y = n_ptr->fy;
		x = n_ptr->fx;

		/* Require line of sight */
		d = distance(p_ptr->py, p_ptr->px, y, x);
		if ((d > MAX_RANGE - 1) || (!los(p_ptr->py, p_ptr->px, y, x))) continue;
		
		num += 1;
	}
	return num;
}


/*
 * For the monster spells: HEAL_OTHR and HEAL_KIN (kinonly)
 * (simulation of monster-cast project_los using GF_OLD_HEAL)
 * (doesn't actually use project() at all)
 *
 * if healmon is 0 or -1, then it only tests to see 
 * if there are nearby monsters to heal. (-1 is for stupid monsters)
 *
 * XXXTODO: If an undead monster has HEAL_KIN, it should raise any
 * undead monsters which are temporarily dead. (onlyundead is currently unused)
 */
bool heal_monsters(int healmon, monster_type *m_ptr, bool kinonly)
{
	int i, x, y, d, cy, cx;
	char ally;
	int kinkind = 0;
	char n_name[80];
	char n_poss[80];
	monster_type *n_ptr;

	int flg = PROJECT_JUMP | PROJECT_KILL | PROJECT_HIDE;

	bool healed = FALSE;
	bool onlygood = FALSE;
	bool onlyliving = FALSE, onlyundead = FALSE;

	/* what race is the caster? */
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
    cptr rname = (r_name + r_ptr->name);

	ally = r_ptr->d_char;
	if (r_ptr->flags3 & (RF3_TROLL)) kinkind = 1;
	if (strchr("pKt", r_ptr->d_char)) kinkind = 2; /* all humans */
	if (strchr("oO", r_ptr->d_char)) kinkind = 3; /* orcs and ogres considered kin */
	if (r_ptr->flags3 & (RF3_CLIGHT)) kinkind = 4; /* creatures of light */
	/* hack: paladins and templar knights don't heal evil monsters */
	if ((strstr(rname, "paladin")) || (strstr(rname, "templar"))) onlygood = TRUE;
	/* special case: Humbaba heals all animals */
	if (strstr(rname, "Humbaba")) kinkind = 5;
	/* positive energy vortex */
	if (strstr(rname, "positive energy v")) onlyliving = TRUE;
	/* location of caster */
	cy = m_ptr->fy;
	cx = m_ptr->fx;

	/* Affect all (nearby) monsters */
	for (i = 1; i < mon_max; i++)
	{
		bool iskin = FALSE;
		bool endfear = FALSE;
		/* get target monster info */
		n_ptr = &mon_list[i];
		r_ptr = &r_info[n_ptr->r_idx];
		/* get target monster name (and possesive) */
		monster_desc(n_name, sizeof(n_name), n_ptr, 0);
		monster_desc(n_poss, sizeof(n_poss), n_ptr, 0x22);

		/* Paranoia -- Skip dead monsters and NONMONSTERs */
		if (!n_ptr->r_idx) continue;
		if (r_ptr->flags7 & (RF7_NONMONSTER)) continue;
		
		/* monster is temporarily dead (raise it now?) */
		if (m_ptr->temp_death) continue;

		/* Location of target */
		y = n_ptr->fy;
		x = n_ptr->fx;

		/* Require projectable from caster */
		d = distance(cy, cx, y, x);
		if ((d > MAX_RANGE - 1) || (!projectable(cy, cx, y, x))) continue;

		/* hurts undead */
		if ((r_ptr->flags3 & (RF3_UNDEAD)) && (!kinonly) && (healmon > 0))
		{
			/* Hurt the monster */
			n_ptr->hp -= (healmon/2);

			/* Dead monster */
			if (n_ptr->hp < 0)
			{
				/* Generate treasure, etc */
				monster_death(cave_m_idx[y][x]);

				/* (healing it makes it less likely to come back) */
				n_ptr->ninelives += 2;

				/* Delete the monster */
				delete_monster_idx(cave_m_idx[y][x], TRUE);

				/* Give detailed messages if destroyed */
				if (n_ptr->ml) msg_format("%^s is destroyed", n_name);
			}

			/* Damaged monster */
			else
			{
				/* Give detailed messages if visible */
				if (n_ptr->ml) msg_format("%^s is damaged", n_name);
			}
			continue;
		}
		
		/* positive energy vortex only heals living monsters */
		if ((onlyliving) && (r_ptr->flags3 & (RF3_NON_LIVING))) continue;

		/* don't bother if monster doesn't need healing */
		/* (but stupid monsters will cast it even if no one needs healing) */
		if ((n_ptr->hp == n_ptr->maxhp) && (healmon > -1)) continue;

		/* Heal_kin spell only heals similar monsters */
		if (kinonly)
		{
			int kinkindt = 0;
			if (r_ptr->flags3 & (RF3_CLIGHT)) kinkindt = 4;
			/* light fairies also heal non-evil animals with HEAL_KIN */
			if ((!n_ptr->evil) && (r_ptr->flags3 & (RF3_ANIMAL))) kinkindt = 4;
			/* centaurs also considered kin with light fairies */
			if ((!n_ptr->evil) && (strchr("Y", r_ptr->d_char))) kinkindt = 4;
			if (kinkind == kinkindt) iskin = TRUE;
			if (r_ptr->flags3 & (RF3_TROLL)) kinkindt = 1;
			if (strchr("pKt", r_ptr->d_char)) kinkindt = 2; /* all humans */
			if (strchr("oO", r_ptr->d_char)) kinkindt = 3; /* orcs and ogres considered kin */
			/* is the target monster kin? */
			if (kinkind == kinkindt) iskin = TRUE;
			/* Humbaba heals all animals */
			if ((r_ptr->flags3 & (RF3_ANIMAL)) && (kinkind == 5)) iskin = TRUE;
			/* same symbol is always kin */
			if (r_ptr->d_char == ally) iskin = TRUE;
			if (!iskin) continue;
		}

		/* hack: templar knights don't heal evil monsters */
		if ((n_ptr->evil) && (onlygood)) continue;

		/* (healmon < 1) == not actually casting the spell yet */
		/* TRUE == there is at least one monster nearby to heal */
		/* (but not if the only monster to heal is itself) */
		if ((healmon < 1) && (n_ptr != m_ptr)) return TRUE;
		else if (healmon < 1) continue;

		/* Wake up (usually) */
		if ((p_ptr->nice) && (!n_ptr->evil) &&
           (goodluck) && (randint(100) < 50) &&
	       ((r_ptr->flags3 & (RF3_CLIGHT)) ||
	       (r_ptr->flags3 & (RF3_ANIMAL))))
	    {
            /* don't wake up */
        }   
		else if ((goodluck < 14) && (!n_ptr->charmed) &&
			(!(r_ptr->sleep == 255)))
        {
			n_ptr->csleep = 0;
			n_ptr->roaming = 0;
		}

#ifdef roomrunes /* healing tree */
		if ((room_runes(cy, cx) == 24) && (!(r_ptr->flags3 & (RF3_NON_LIVING))))
		{
			if (n_ptr->maxhp/4 >= 800) n_ptr->hp += healmon + n_ptr->maxhp/5;
			else if (n_ptr->maxhp/4 > 640) n_ptr->hp += healmon + (n_ptr->maxhp/4 + n_ptr->maxhp/5)/2;
			else n_ptr->hp += healmon + n_ptr->maxhp/4;
		}
		else
#endif
		/* Heal */
			n_ptr->hp += healmon;
		healed = TRUE;

		/* Message */
		if ((n_ptr->hp < n_ptr->maxhp) && (n_ptr->ml))
			msg_format("%^s looks healthier", n_name);

		/* No overflow */
		if (n_ptr->hp >= n_ptr->maxhp)
		{
			n_ptr->hp = n_ptr->maxhp;
			/* alternate message */
			if (n_ptr->ml) msg_format("%^s looks fully healthy", n_name);
		}

		/* Redraw (later) if needed */
		if (p_ptr->health_who == cave_m_idx[y][x]) p_ptr->redraw |= (PR_HEALTH);

		/* always end fear if healed by an ally or "kin" */
		if ((kinonly) || (r_ptr->d_char == ally)) endfear = TRUE;
		/* Cancel fear */
		if (randint(100) < 60 + badluck - goodluck) endfear = TRUE;
		if ((n_ptr->monfear) && (endfear))
		{
			/* Cancel fear */
			n_ptr->monfear = 0;

			/* Message */
			if (n_ptr->ml) msg_format("%^s recovers %s courage.", n_name, n_poss);
		}
	}

	/* Result */
	return (healed);
}


/*
 * An "item_tester_hook" for grenade oil sources
 */
static bool item_tester_refill_lantern(const object_type *o_ptr)
{
	/* Get flags */
	u32b f1, f2, f3, f4;
	object_flags(o_ptr, &f1, &f2, &f3, &f4);
	
	/* grenades use TV_FLASK also (normal flasks of oil only have PTHROW) */
    if (f3 & TR3_THROWN) return (FALSE);

	/* Flasks of oil are okay */
	if (o_ptr->tval == TV_FLASK) return (TRUE);

	/* ego lanerns are okay even if they're empty (allow everburning lanterns) */
	if ((o_ptr->tval == TV_LITE) && (o_ptr->sval == SV_LITE_LANTERN) && 
		(ego_item_p(o_ptr))) return (TRUE);

	/* other Non-empty lanterns are okay */
	if ((o_ptr->tval == TV_LITE) &&
	    (o_ptr->sval == SV_LITE_LANTERN) &&
	    (o_ptr->timeout > 0))
			return (TRUE);

	/* Assume not okay */
	return (FALSE);
}


/*
 * Create a grenade (alchemy spell)
 * Copied from wiz_create_item() and do_cmd_refill_lamp()
 */
bool craft_grenade(void)
{
	object_type *i_ptr;
	object_type object_type_body;
	object_type *o_ptr;

	int k_idx;
	int item, die, bonus;

	cptr q, s;
	
	/* Restrict the choices */
	item_tester_hook = item_tester_refill_lantern;

	/* Get an item */
	q = "Craft a grenade with which source of oil? ";
	s = "You have no sources of oil.";
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return FALSE;

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}

	/* an ego lantern will make an ego grenade */
	if (ego_item_p(o_ptr)) bonus = 200;
	else bonus = 0;

	/* Decrease the item (from the pack) */
	if (item >= 0)
	{
		inven_item_increase(item, -1);
		inven_item_describe(item);
		inven_item_optimize(item);
	}
	/* Decrease the item (from the floor) */
	else
	{
		floor_item_increase(0 - item, -1);
		floor_item_describe(0 - item);
		floor_item_optimize(0 - item);
	}

	/* Determine quality of grenade to make */
	die = p_ptr->lev + goodluck + randint(p_ptr->lev * 2 + goodluck + 40);

	/* default */
	k_idx = lookup_kind(TV_FLASK, SV_OIL_GRENADE);

	/* junk grenade */
	if (die < 50) k_idx = lookup_kind(TV_FLASK, SV_JUNK_GRENADE);

	/* chance of an ego grenade (or combat bonuses) */
	if ((die > 100) && (bonus < 100)) bonus = die - 100;

	/* small chance of powerful grenade */
	if (die > 175)
	{
		k_idx = lookup_kind(TV_FLASK, SV_FIREBOMB);
		if (bonus < 200) bonus = bonus * 2 / 3;
	}

	/* Return if failed */
	if (!k_idx)
	{
		msg_print("oops, someone's killed this spell in the code or object.txt.");
		return FALSE;
	}

	/* Get local object */
	i_ptr = &object_type_body;

	/* Create the item */
	object_prep(i_ptr, k_idx);
	
	/* apply magic (manually) */

	/* roll for ego grenade */
	if (rand_int(100) < bonus - 10)
	{
		ego_item_type *e_ptr;
		int dieb, maxdieb, nbonus = bonus; 
		if (nbonus > 150) nbonus = 150;
		maxdieb = 78 + goodluck*2 + nbonus/10 + p_ptr->lev/4;
		if (maxdieb > 131) maxdieb = 111 + goodluck; /* maximum goodluck is 21 (vrare) */
		dieb = rand_int(maxdieb);
		if (dieb < 11) i_ptr->name2 = EGO_HURT_ANIMAL; /* 11 */
		else if (dieb < 20) i_ptr->name2 = EGO_HURT_DEMON; /* 9 */
		else if (dieb < 29) i_ptr->name2 = EGO_HURT_UNDEAD; /* 9 */
		else if (dieb < 40) i_ptr->name2 = EGO_HURT_GIANT; /* 11 */
		else if (dieb < 53) i_ptr->name2 = EGO_AMMO_VENOM; /* 12 */
		else if (dieb < 66) i_ptr->name2 = EGO_XPLODE_PANIC; /* 13 */
		else if (dieb < 80) i_ptr->name2 = EGO_XPLODE_ESCAPE; /* 14 */
		else if (dieb < 94) i_ptr->name2 = EGO_XPLODE_DESTRUCT; /* 14 */
		else if (dieb < 108) i_ptr->name2 = EGO_XPLODE_CHAOS; /* 14 */
		else if (dieb < 120) i_ptr->name2 = EGO_AMMO_ELEC; /* 12 */
		else /*if (dieb < 132)*/ i_ptr->name2 = EGO_AMMO_HOLY; /* 12 (vrare max) */

		/* ego combat bonuses */
		e_ptr = &e_info[i_ptr->name2];
		i_ptr->to_h += randint(e_ptr->max_to_h);
		i_ptr->to_d += randint(e_ptr->max_to_d);
	}

	/* give some combat bonuses */
	if ((rand_int(20) < bonus + 2) || (rand_int(400) < goodluck) || (i_ptr->name2)) 
	{
		if ((rand_int(150 - p_ptr->lev) > goodluck + 5) || (i_ptr->name2))
		{
			if ((bonus > 105) && (i_ptr->name2)) bonus = randint(61) + 45;
			else if (bonus > 120) bonus = randint(30) + 90;
			else if (bonus > 90) bonus = 90;
		
			if (bonus > 105) bonus = 105;
		}

		i_ptr->to_h += randint(bonus/15) + randint(5);
		i_ptr->to_d += randint(bonus/15) + randint(5);
	}

	/* place the object */
	drop_near(i_ptr, -1, p_ptr->py, p_ptr->px);

	return (TRUE);
}


/*
 * Create a treasure map for the tourist to find.
 * Copied from wiz_create_item()
 */
void treasure_map(void)
{
	int i, x, y, d, dis, min;
	bool look = TRUE;

	int py = p_ptr->py;
	int px = p_ptr->px;

	object_type *i_ptr;
	object_type object_type_body;

	int k_idx;

	/* Get object base type (tval 4, sval 2) */
	k_idx = lookup_kind(TV_SPECIAL, SV_TREASURE);

	/* Return if failed */
	if (!k_idx) return;

	/* Get local object */
	i_ptr = &object_type_body;

	/* Create the item */
	object_prep(i_ptr, k_idx);
	
	/* no need to apply magic to a treasure map */
	
	/* Look for a place to "hide" the map */
	/* coped from teleportation code */
	look = TRUE;
	dis = (76 - randint(p_ptr->lev + goodluck));

	/* Initialize */
	y = py;
	x = px;

	/* Minimum distance */
	min = dis / 2;

	while (look)
	{
		/* Try several locations */
		for (i = 0; i < 500; i++)
		{
			/* Pick a (possibly illegal) location */
			while (1)
			{
				y = rand_spread(py, dis);
				x = rand_spread(px, dis);
				d = distance(py, px, y, x);
				if ((d >= min) && (d <= dis)) break;
			}

			/* Ignore illegal locations */
			if (!in_bounds_fully(y, x)) continue;

			/* Require "occupy" floor space (allows pits & rubble) */
			if (!cave_can_occupy_bold(y, x)) continue;

			/* don't put it with another object */
			if (cave_o_idx[y][x] != 0) continue;

			/* Might create the map inside a vault */
			/* if (cave_info[y][x] & (CAVE_ICKY)) continue; */

			/* This grid looks good */
			look = FALSE;

			/* Stop looking */
			break;
		}

		/* Increase the maximum distance */
		dis = dis * 2;

		/* Decrease the minimum distance */
		min = min / 2;
    }

	/* Drop the object from heaven */
	drop_near(i_ptr, -1, y, x);

	/* All done */
	if (los(py, px, y, x)) msg_print("X marks the spot. (actually it's a '?')");
	else msg_print("The map is hidden, the hunt is on!");
}



/*
 * Activate Experience drain on an item
 * (activated by attacking, shooting, or casting)
 * (was 55 instead of 10 + odd)
 * (odd is 45 except in melee when it is 40 + (damage/5))
 */
void rxp_drain(int odd)
{
     /* sentient equipment can make exp drain kick in more or less often */
     if (goodweap > badweap) odd -= 10;
     else if (badweap > goodweap) odd -= 5;
     else if ((badweap > 0) && (badweap == goodweap)) odd += 10;

     if ((randint(100) < odd + badluck - goodluck) && (p_ptr->exp > 0))
     {
        int drainmuch = randint(11) + randint(p_ptr->lev) + randint(badluck*2);
        if (cp_ptr->spell_book == TV_DARK_BOOK) drainmuch += randint(6);
		p_ptr->exp -= drainmuch;
		if (goodluck > 16) p_ptr->max_exp -= drainmuch/10;
		else if (goodluck > 10) p_ptr->max_exp -= drainmuch/4;
		else if (goodluck > 4) p_ptr->max_exp -= drainmuch/3;
		else if (badluck > 6) p_ptr->max_exp -= (drainmuch * 2) / 3;
		else p_ptr->max_exp -= drainmuch/2;
	    check_experience();
     }
     else if ((badluck > 0) && (p_ptr->exp > 0))
     {
        int drainmuch = randint(badluck) + 1;
		p_ptr->exp -= drainmuch;
		p_ptr->max_exp -= 1;
	    check_experience();
     }
}

/* drain charges caused by a curse */
static void annoying_static(void)
{
	int k, i, rstatic = (goodluck+1)/2; /* static resistance */
	int drained = 0;
	object_type *o_ptr;
	char o_name[80];

	/* first point of resist is worth 26, others worth 20 */
	if (p_ptr->resist_static > 1) rstatic += ((p_ptr->resist_static-1) * 20) + 26;
	else if (p_ptr->resist_static == 1) rstatic += 26;

	/* cap resistance */
	if (rstatic > 95) rstatic = 90;
	
    	/* Find an item */
		for (k = 0; k < 10; k++)
		{
			/* Pick an item */
			i = rand_int(INVEN_PACK);

			/* Obtain the item */
			o_ptr = &inventory[i];

			/* Skip non-objects */
			if (!o_ptr->k_idx) continue;

			/* Drain charged wands/staves */
			if (((o_ptr->tval == TV_STAFF) ||
			    (o_ptr->tval == TV_WAND)) && (o_ptr->charges))
			{
				if (p_ptr->resist_static) rstatic += (k+1)/2;

				if (randint(100) < rstatic)
				{
					/* Description */
					object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 1);
								
					if (p_ptr->resist_static) msg_format("You prevent your %s from being drained!", o_name);
					/* resisted even without static resistance (from luck, rare) */
					else msg_format("You get lucky and your %s escapes being drained!", o_name);
					/* second resist roll to prevent it draining something else */
					if (randint(100) < rstatic + 1) break;
				}
				else
				{
					drained = o_ptr->charges;
					if ((p_ptr->resist_static) && (randint(100) < 55))
					{
						drained -= p_ptr->resist_static;
						if (drained < 1) drained = 1;
					}
						/* Uncharge */
					o_ptr->charges -= drained;
				}
			}

			if (drained)
			{
				msg_print("Energy drains from your pack!");

				/* Combine / Reorder the pack */
				p_ptr->notice |= (PN_COMBINE | PN_REORDER);

				/* Window stuff */
				p_ptr->window |= (PW_INVEN);

				/* Affect only a single inventory slot */
				break;
			}
		}
}

/* new item drawback: R_ANNOY */
void do_something_annoying(object_type *o_ptr)
{
	char o_name[80];
	/* curse gets worse based on your max depth (was rand_int(60 + ...) ) */
	int die = p_ptr->max_depth/4 + rand_int(56 + (p_ptr->max_depth+3)/4 + badluck);

	/* Description */
	object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 1);
	
	if (die < 5)
	{
		msg_print("You suddenly get a crick in your neck.");
		inc_timed(TMD_STUN, 4 + randint(4 + badluck/2));
		if (disturb_minor) disturb(0, 0);
	}
	else if (die < 16)
	{
		msg_format("Your %s bites you!", o_name);
		take_hit(damroll(1, 6), "an annoying curse");
		if ((!p_ptr->resist_charm) && (!p_ptr->peace))
			inc_timed(TMD_FRENZY, 8 + randint(8 + badluck));
		if (disturb_minor) disturb(0, 0);
	}
	else if (die < 20)
	{
		msg_print("You trip over a rock and cut yourself on a broken flask.");
		inc_timed(TMD_CUT, 13 + randint(13 + badluck));
		if (disturb_minor) disturb(0, 0);
	}
	else if (die < 25)
	{
		msg_format("Your %s creates some traps!", o_name);
		disturb(0, 0);
		trap_creation(1);
	}
	else if (die < 28)
	{
		msg_format("Your %s makes an annoying squeak which breaks your focus.", o_name);
		(void)clear_timed(TMD_SKILLFUL);
		(void)clear_timed(TMD_SHADOW);
		(void)clear_timed(TMD_SUPER_ROGUE);
		(void)clear_timed(TMD_BRAIL);
		(void)clear_timed(TMD_TSIGHT);
		(void)clear_timed(TMD_SNIPER);
		(void)clear_timed(TMD_HERO);
		(void)clear_timed(TMD_SHERO);
		(void)clear_timed(TMD_SANCTIFY);
		(void)clear_timed(TMD_MDETECTION);
		(void)clear_timed(TMD_CLEAR_MIND);
		(void)clear_timed(TMD_LASTING_CURE);
		inc_timed(TMD_FRENZY, 3 + randint(2 + badluck/2));
		if (disturb_minor) disturb(0, 0);
	}
	else if (die < 32) /* 4 */
	{
		/* shhh, don't let the PC know there's an imaginary monster summoned */
		imaginary_friend(0);
	}
	else if (die < 37)
	{
		msg_format("Your %s curses you!", o_name);
		inc_timed(TMD_CURSE, 80 + randint(80 + badluck));
		if (disturb_minor) disturb(0, 0);
	}
	else if (die < 43)
	{
		msg_format("Your %s makes you forget what you were doing.", o_name);
		inc_timed(TMD_AMNESIA, 60 + randint(50 + badluck));
		if (disturb_minor) disturb(0, 0);
	}
	else if (die < 50)
	{
		disturb(0, 0);
		msg_format("Your %s shouts 'Hello Sailor' loud enough rouse a sleeping behemoth!", o_name);
		aggravate_monsters(0);
	}
	else if (die < 53) /* 3 */
	{
		int healmoo = damroll(4, 9);
		if (p_ptr->depth >= 22) healmoo = healmoo * randint(p_ptr->depth/11);
        if (fire_spread(GF_OLD_HEAL, healmoo, 8))
			msg_format("Your %s magically heals nearby monsters!", o_name);
	}
	else if (die < 62) /* 9 */
	{
		int die2, fdep = p_ptr->depth;
		if ((fdep < 10) && (p_ptr->max_depth > 14)) fdep = (p_ptr->max_depth*2)/3;
		else if (fdep < 9) fdep = 9;
        die2 = fdep/3 + randint((fdep*2)/3 + badluck);
		msg_format("Your %s summons a pest.", o_name);
		disturb(1, 0);
		if (die2 < 5) do_call_help(67); /* yellow worm mass */
		else if (die2 < 7) do_call_help(46); /* garden gnome */
		else if (die2 < 10) do_call_help(109); /* rabid rat */
		else if (die2 < 12) do_call_help(94); /* harpy */
		else if (die2 < 14) do_call_help(104); /* hobgoblin */
		else if (die2 < 16) do_call_help(776); /* cute fluffy bunny */
		else if (die2 < 18) do_call_help(169); /* giant fruit fly */
		else if (die2 < 20) do_call_help(138); /* gnome thief */
		else if (die2 < 22) do_call_help(198); /* shrieking worm mass */
		else if (die2 < 24) do_call_help(176); /* tengu */
		else if (die2 < 26) do_call_help(201); /* slime blob */
		else if (die2 < 28) do_call_help(249); /* mirkwood spider */
		else if (die2 < 31) do_call_help(837); /* blue icky thing */
		else if (die2 < 33) do_call_help(219); /* ochre jelly */
		else if (die2 < 35) do_call_help(263); /* winged monkey */
		else if (die2 < 38) do_call_help(277); /* rabid wolf */
		else if (die2 < 40) do_call_help(281); /* imp */
		else if (die2 < 42) do_call_help(305); /* neekerbreeker */
		else if (die2 < 44) do_call_help(308); /* minidrake */
		else if (die2 < 46) do_call_help(344); /* skull wisp */
		else if (die2 < 48) do_call_help(351); /* giant horned bullfrog */
		else if (die2 < 50) do_call_help(951); /* grindylow */
		else if (die2 < 52) do_call_help(436); /* doombat */
		else if (die2 < 54) do_call_help(331); /* acid hound */
		else if (die2 < 57) do_call_help(358); /* giant firefly */
		else if (die2 < 59) do_call_help(395); /* pooka */
		else if (die2 < 62) do_call_help(403); /* wereworm */
		else if (die2 < 64) do_call_help(513); /* small black reaper */
		else if (die2 < 66) do_call_help(511); /* singing vyrm */
		else if (die2 < 69) do_call_help(482); /* undying troll */
		else if (die2 < 72) do_call_help(466); /* giant lightning bugs */
		else if (die2 < 74) do_call_help(522); /* bone soldier */
		else if (die2 < 76) do_call_help(675); /* sleepy willow tree */
		else if (die2 < 78) do_call_help(497); /* plague zombie */
		else if (die2 < 80) do_call_help(545); /* master mind flayer */
		else if (die2 < 82) do_call_help(741); /* gnawing bug */
		else if (die2 < 84) do_call_help(569); /* animated staff of summoning */
		else if (die2 < 88) do_call_help(840); /* giant cockroach */
		else if (die2 < 90) do_call_help(555); /* furies */
		else if (die2 < 93) do_call_help(631); /* doppleganger */
		else if (die2 < 97) do_call_help(668); /* time hound */
		else if (die2 < 100) do_call_help(637); /* undead beholder */
		else do_call_help(822); /* pandemonium fiend */
	}
	else if (die < 66) /* 4 */
	{
		msg_print("You suddenly get a nasty crick in your neck.");
		(void)clear_timed(TMD_SNIPER);
		(void)clear_timed(TMD_CLEAR_MIND);
		inc_timed(TMD_STUN, 8 + randint(6 + badluck));
		if (disturb_minor) disturb(0, 0);
	}
	else if (die < 70) /* 4 */
	{
		msg_format("Your %s plays a catchy tune, putting you in a good mood.", o_name);
		inc_timed(TMD_CHARM, 40 + randint(40 + badluck));
		if (disturb_minor) disturb(0, 0);
	}
	else if (die < 73) /* 3 */
	{
		disturb(0, 0);
		/* spontaneously create a trap underneath you (prevents trap doors) */
		cave_set_feat(p_ptr->py, p_ptr->px, FEAT_INVIS);
		pick_trap(p_ptr->py, p_ptr->px, TRUE);

		/* ...and hit the trap */
		hit_trap(p_ptr->py, p_ptr->px, TRUE);
	}
	else if (die < 76) /* 3 */
	{
		disturb(0, 0);
		msg_format("Your %s grows a pair of tentacles and pokes you in the eyes.", o_name);
		take_hit(damroll(1, 3), "an annoying curse");
		if ((!p_ptr->resist_charm) && (!p_ptr->peace))
			inc_timed(TMD_FRENZY, 9 + randint(9 + badluck));
		if (!p_ptr->timed[TMD_SAFET_GOGGLES])
			inc_timed(TMD_BLIND, 3 + randint(3 + badluck/3));
	}
	else if (die < 79) /* 3 */
	{
		int traprad = 2, chance = 10;
		if (die > 77) traprad = 3;
		if (die < 74) chance = 11; /* slightly less traps */
		msg_format("Your %s creates some traps!", o_name);
		disturb(0, 0);
		/* for each grid within 2-3 spaces, it has a 1/3 chance to create a trap */
		fire_spread(GF_MAKE_TRAP, chance, traprad);
	}
	else if (die < 82) /* 3 */
	{
		disturb(0, 0);
		msg_format("Your %s is hungry!", o_name);
		annoying_static();
	}
	else if (die == 91) /* 1 */
	{
		disturb(0, 0);
		msg_format("Your %s curses your armor!", o_name);
		/* (less nasty version of curse armor effect) */
		curse_armor(TRUE);
	}
	else if (die < 94) /* 11 */
	{
		msg_format("Your %s bites you!", o_name);
		take_hit(damroll(2, 6), "an annoying curse");
		if ((!p_ptr->resist_charm) && (!p_ptr->peace))
			inc_timed(TMD_FRENZY, 33 + randint(33 + badluck));
		if (!(p_ptr->resist_pois || p_ptr->timed[TMD_OPP_POIS]))
			inc_timed(TMD_POISONED, randint(20) + 20 + badluck);
		inc_timed(TMD_CUT, 13 + randint(13 + badluck));
		if (disturb_minor) disturb(0, 0);
	}
	else if (die == 103) /* 104 is maximum for maxdepth 99 not including bad luck */
	{
		disturb(0, 0);
		msg_format("Your %s decides to be really nasty this time.", o_name);
		msg_format("Your %s bites you and casts some spells at the same time!", o_name);
		take_hit(damroll(2, 8), "an annoying curse");
		if ((!p_ptr->resist_charm) && (!p_ptr->peace))
			inc_timed(TMD_FRENZY, 33 + randint(33 + badluck));
		if (!(p_ptr->resist_pois || p_ptr->timed[TMD_OPP_POIS]))
			inc_timed(TMD_POISONED, randint(20) + 20 + badluck);
		inc_timed(TMD_CUT, 16 + randint(13 + badluck));
		inc_timed(TMD_STUN, 12 + randint(6 + badluck));
		if (!p_ptr->resist_chaos)
			inc_timed(TMD_IMAGE, 29 + randint(31 + badluck));
		if (!p_ptr->resist_charm)
			inc_timed(TMD_CHARM, 29 + randint(31 + badluck));

		/* spontaneously create a trap underneath you (prevents trap doors) */
		cave_set_feat(p_ptr->py, p_ptr->px, FEAT_INVIS);
		pick_trap(p_ptr->py, p_ptr->px, TRUE);

		/* ...and hit the trap */
		hit_trap(p_ptr->py, p_ptr->px, FALSE);
	}
	else /* 9 (if max is 104 (maxdepth99)) */
	{
		disturb(0, 0);
		msg_format("Your %s injects something funky into your veins.", o_name);
		inc_timed(TMD_IMAGE, 65 + badluck + randint(65 + badluck));
	}
}

