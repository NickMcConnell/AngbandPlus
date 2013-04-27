#include "angband.h"
#include <assert.h>

int hit_chance_innate(int to_h, int ac)
{
	int chance = p_ptr->skills.thn + to_h * BTH_PLUS_ADJ;
	int odds;
	
	odds = 95*(chance - ac*3/4)*1000/(chance*100);
	if (p_ptr->personality == PERS_LAZY) odds = (19*odds+10)/20;
	if (odds < 50) odds = 50;
	return (odds+5)/10;
}

int hit_chance(int hand, int to_h, int ac)
{
	int chance = p_ptr->skills.thn + (p_ptr->weapon_info[hand].to_h + to_h) * BTH_PLUS_ADJ;
	int odds;
	
	chance = chance * p_ptr->weapon_info[hand].dual_wield_pct / 1000;
	odds = 95*(chance - ac*3/4)*1000/(chance*100);
	if (p_ptr->personality == PERS_LAZY) odds = (19*odds+10)/20;
	if (odds < 50) odds = 50;
	return (odds+5)/10;
}

int bow_hit_chance(int sval, int to_h, int ac)
{
	int chance;
	int odds;

	if (sval == SV_LIGHT_XBOW || sval == SV_HEAVY_XBOW)
		chance = (p_ptr->skills.thb + (p_ptr->weapon_exp[0][sval] / 400 + to_h) * BTH_PLUS_ADJ);
	else
		chance = (p_ptr->skills.thb + ((p_ptr->weapon_exp[0][sval] - (WEAPON_EXP_MASTER / 2)) / 200 + to_h) * BTH_PLUS_ADJ);

	odds = 95*(chance - ac*3/4)*1000/(chance*100);
	if (p_ptr->personality == PERS_LAZY) odds = (19*odds+10)/20;
	if (odds < 50) odds = 50;
	return (odds+5)/10;
}

/**********************************************************************
 * Display Weapon Information to the Player
 **********************************************************************/
static void _display_weapon_slay(int base_mult, int slay_mult, bool force, int blows, 
								 int dd, int ds, int to_d, cptr name, int color, int row, int col)
{
	char buf[80];
	int mult, min, max;

	mult = slay_mult;
	if (force)
		mult = mult * 3/2 + 100;
	mult = mult * base_mult / 100;

	min = blows * (mult*dd/100 + to_d);
	max = blows * (mult*dd*ds/100 + to_d);

	sprintf(buf, " %-7.7s", name);
	c_put_str(color, buf, row, col);

	sprintf(buf, ": %d (%d-%d) [%d.%02dx]", 
					(min + max)/2, min, max,
					mult/100, mult%100);
	put_str(buf, row, col+8);
}

int display_weapon_info(int hand, int row, int col)
{
	object_type *o_ptr = equip_obj(p_ptr->weapon_info[hand].slot);
	char o_name[MAX_NLEN];
	char buf[80];
	u32b flgs[TR_FLAG_SIZE];
	int dd;
	int ds;
	int to_d = 0;
	int to_h = 0;
	int mult;
	critical_t crit = {0};
	int num_blow = NUM_BLOWS(hand);
	int r,c;
	bool force = FALSE;
	int result = row;

	if (p_ptr->weapon_info[hand].wield_how == WIELD_NONE) return row;
	if (!o_ptr) return row;

	dd = o_ptr->dd + p_ptr->weapon_info[hand].to_dd;
	ds = o_ptr->ds + p_ptr->weapon_info[hand].to_ds;
	if (object_is_known(o_ptr))
	{
		to_d = o_ptr->to_d;
		to_h = o_ptr->to_h;
	}

	weapon_flags_known(hand, flgs);
	if ( (have_flag(flgs, TR_FORCE_WEAPON) || p_ptr->tim_force) 
	  && (p_ptr->csp > o_ptr->dd*o_ptr->ds/5) )
	{
		force = TRUE;
	}

	if (weaponmaster_get_toggle() == TOGGLE_SHIELD_BASH && object_is_shield(o_ptr))
	{
		dd = 3 + p_ptr->weapon_info[hand].to_dd;
		ds = k_info[o_ptr->k_idx].ac + p_ptr->weapon_info[hand].to_ds;
		if (object_is_known(o_ptr))
		{
			to_h = o_ptr->to_a;
			to_d = o_ptr->to_a;
			to_h += 2*o_ptr->to_h;
			to_d += 2*o_ptr->to_d;
		}
	}

	mult = 100;
	switch (o_ptr->name1)
	{
	case ART_VORPAL_BLADE:
	case ART_CHAINSWORD:
	case ART_MURAMASA:
		mult = mult * 5 / 3;
		break;
	default:
		if (have_flag(flgs, TR_VORPAL))
			mult = mult * 11 / 9;
	}

	if (!have_flag(flgs, TR_ORDER))
	{
		const int ct = 10 * 1000;
		int i;
		/* Compute Average Effects of Criticals by sampling */
		for (i = 0; i < ct; i++)
		{
			critical_t tmp = critical_norm(o_ptr->weight, to_h, p_ptr->weapon_info[hand].to_h, 0, hand);
			if (tmp.desc)
			{
				crit.mul += tmp.mul;
				crit.to_d += tmp.to_d;
			}
			else
				crit.mul += 100;
		}
		crit.mul = crit.mul / ct;
		crit.to_d = crit.to_d * 100 / ct;
	}
	else
		crit.mul = 100;


	/* First Column */
	r = row;
	c = col;
	object_desc(o_name, o_ptr, OD_OMIT_INSCRIPTION);
	sprintf(buf, " Hand #%d: %-49.49s", hand+1, o_name);
	c_put_str(TERM_YELLOW, buf, r++, c);

	sprintf(buf, " %-7.7s: %d.%d lbs", "Weight", o_ptr->weight/10, o_ptr->weight%10);
	put_str(buf, r++, c);

	sprintf(buf, " %-7.7s: %d + %d = %d", "To Hit", to_h, p_ptr->weapon_info[hand].to_h, to_h + p_ptr->weapon_info[hand].to_h);
	put_str(buf, r++, c);

	sprintf(buf, " %-7.7s: %d + %d = %d", "To Dam", to_d, p_ptr->weapon_info[hand].to_d, to_d + p_ptr->weapon_info[hand].to_d);
	put_str(buf, r++, c);
	
	sprintf(buf, " %-7.7s: %d", "Blows", num_blow);
	put_str(buf, r++, c);

	if (p_ptr->weapon_info[hand].dual_wield_pct < 1000)
	{
		sprintf(buf, " %-7.7s: %d.%d%%", "Skill", p_ptr->weapon_info[hand].dual_wield_pct/ 10, p_ptr->weapon_info[hand].dual_wield_pct % 10);
		put_str(buf, r++, c);
	}

	mult = mult * crit.mul / 100;
	to_d = to_d + crit.to_d/100 + p_ptr->weapon_info[hand].to_d;

	sprintf(buf, " %-7.7s", "Damage");
	c_put_str(TERM_L_GREEN, buf, r++, c);

	if (!have_flag(flgs, TR_ORDER))
	{
		sprintf(buf, " %-7.7s: %d.%02dx + %d.%02d", "Crits",
						crit.mul/100, crit.mul%100, crit.to_d/100, crit.to_d%100);
		put_str(buf, r++, c);
	}

	_display_weapon_slay(mult, 100, FALSE, num_blow, dd, ds, to_d, "Normal", TERM_WHITE, r++, c);
	if (force)
		_display_weapon_slay(mult, 100, force, num_blow, dd, ds, to_d, "Force", TERM_L_BLUE, r++, c);

	if (p_ptr->tim_slay_sentient)
		_display_weapon_slay(mult, 200, force, num_blow, dd, ds, to_d, "Sent.", TERM_YELLOW, r++, c);
	
	if (have_flag(flgs, TR_KILL_ANIMAL)) 
		_display_weapon_slay(mult, 400, force, num_blow, dd, ds, to_d, "Animals", TERM_YELLOW, r++, c);
	else if (have_flag(flgs, TR_SLAY_ANIMAL))
		_display_weapon_slay(mult, 250, force, num_blow, dd, ds, to_d, "Animals", TERM_YELLOW, r++, c);
	
	if (have_flag(flgs, TR_KILL_EVIL))   
		_display_weapon_slay(mult, 350, force, num_blow, dd, ds, to_d, "Evil", TERM_YELLOW, r++, c);
	else if (have_flag(flgs, TR_SLAY_EVIL))
		_display_weapon_slay(mult, 200, force, num_blow, dd, ds, to_d, "Evil", TERM_YELLOW, r++, c);

	if (have_flag(flgs, TR_KILL_HUMAN))
		_display_weapon_slay(mult, 400, force, num_blow, dd, ds, to_d, "Human", TERM_YELLOW, r++, c);
	else if (have_flag(flgs, TR_SLAY_HUMAN))
		_display_weapon_slay(mult, 250, force, num_blow, dd, ds, to_d, "Human", TERM_YELLOW, r++, c);

	if (have_flag(flgs, TR_KILL_UNDEAD))
		_display_weapon_slay(mult, 500, force, num_blow, dd, ds, to_d, "Undead", TERM_YELLOW, r++, c);
	else if (have_flag(flgs, TR_SLAY_UNDEAD))
		_display_weapon_slay(mult, 300, force, num_blow, dd, ds, to_d, "Undead", TERM_YELLOW, r++, c);
	
	if (have_flag(flgs, TR_KILL_DEMON))  
		_display_weapon_slay(mult, 500, force, num_blow, dd, ds, to_d, "Demons", TERM_YELLOW, r++, c);
	else if (have_flag(flgs, TR_SLAY_DEMON))
		_display_weapon_slay(mult, 300, force, num_blow, dd, ds, to_d, "Demons", TERM_YELLOW, r++, c);

	if (have_flag(flgs, TR_KILL_ORC))  
		_display_weapon_slay(mult, 500, force, num_blow, dd, ds, to_d, "Orcs", TERM_YELLOW, r++, c);
	else if (have_flag(flgs, TR_SLAY_ORC))
		_display_weapon_slay(mult, 300, force, num_blow, dd, ds, to_d, "Orcs", TERM_YELLOW, r++, c);

	if (have_flag(flgs, TR_KILL_TROLL))  
		_display_weapon_slay(mult, 500, force, num_blow, dd, ds, to_d, "Trolls", TERM_YELLOW, r++, c);
	else if (have_flag(flgs, TR_SLAY_TROLL))
		_display_weapon_slay(mult, 300, force, num_blow, dd, ds, to_d, "Trolls", TERM_YELLOW, r++, c);

	if (have_flag(flgs, TR_KILL_GIANT))  
		_display_weapon_slay(mult, 500, force, num_blow, dd, ds, to_d, "Giants", TERM_YELLOW, r++, c);
	else if (have_flag(flgs, TR_SLAY_GIANT))
		_display_weapon_slay(mult, 300, force, num_blow, dd, ds, to_d, "Giants", TERM_YELLOW, r++, c);

	if (have_flag(flgs, TR_KILL_DRAGON))  
		_display_weapon_slay(mult, 500, force, num_blow, dd, ds, to_d, "Dragons", TERM_YELLOW, r++, c);
	else if (have_flag(flgs, TR_SLAY_DRAGON))
		_display_weapon_slay(mult, 300, force, num_blow, dd, ds, to_d, "Dragons", TERM_YELLOW, r++, c);
	
	if (have_flag(flgs, TR_BRAND_ACID))
		_display_weapon_slay(mult, 250, force, num_blow, dd, ds, to_d, "Acid", TERM_RED, r++, c);

	if (have_flag(flgs, TR_BRAND_ELEC))
		_display_weapon_slay(mult, 250, force, num_blow, dd, ds, to_d, "Elec", TERM_RED, r++, c);

	if (have_flag(flgs, TR_BRAND_FIRE))
		_display_weapon_slay(mult, 250, force, num_blow, dd, ds, to_d, "Fire", TERM_RED, r++, c);

	if (have_flag(flgs, TR_BRAND_COLD))
		_display_weapon_slay(mult, 250, force, num_blow, dd, ds, to_d, "Cold", TERM_RED, r++, c);

	if (have_flag(flgs, TR_BRAND_POIS))
		_display_weapon_slay(mult, 250, force, num_blow, dd, ds, to_d, "Poison", TERM_RED, r++, c);

	result = MAX(result, r);

	/* Second Column */
	r = row;
	c = col + 60;

	c_put_str(TERM_L_GREEN, "Accuracy", r++, c);
	put_str(" AC Hit", r++, c);

	sprintf(buf, "%3d %2d%%", 25, hit_chance(hand, to_h, 25));
	put_str(buf, r++, c);

	sprintf(buf, "%3d %2d%%", 50, hit_chance(hand, to_h, 50));
	put_str(buf, r++, c);

	sprintf(buf, "%3d %2d%%", 75, hit_chance(hand, to_h, 75));
	put_str(buf, r++, c);

	sprintf(buf, "%3d %2d%%", 100, hit_chance(hand, to_h, 100));
	put_str(buf, r++, c);

	sprintf(buf, "%3d %2d%%", 125, hit_chance(hand, to_h, 125));
	put_str(buf, r++, c);

	sprintf(buf, "%3d %2d%%", 150, hit_chance(hand, to_h, 150));
	put_str(buf, r++, c);

	sprintf(buf, "%3d %2d%%", 175, hit_chance(hand, to_h, 175));
	put_str(buf, r++, c);

	sprintf(buf, "%3d %2d%%", 200, hit_chance(hand, to_h, 200));
	put_str(buf, r++, c);

	result = MAX(result, r);
	return result;
}

/**********************************************************************
 * Innate Attacks
 **********************************************************************/
static cptr _effect_name(int which)
{
	switch (which)
	{
	case 0: case GF_MISSILE: return "Normal";
	case GF_ACID: return "Acid";
	case GF_ELEC: return "Elec";
	case GF_FIRE: return "Fire";
	case GF_COLD: return "Cold";
	case GF_POIS: return "Poison";
	case GF_NETHER: return "Nether";
	case GF_SHARDS: return "Shards";
	case GF_DISENCHANT: return "Disench";
	case GF_TIME: return "Time";
	case GF_OLD_DRAIN: return "Drain";
	case GF_OLD_CONF: return "Confuse";
	case GF_STUN: return "Stun";
	case GF_DRAIN_MANA: return "Drain Mana";
	}
	return "Unknown";
}

int display_innate_attack_info(int which, int row, int col)
{
	innate_attack_ptr a = &p_ptr->innate_attacks[which];
	int min, max, min_base, max_base, min2, max2;
	critical_t crit = {0};
	const int ct = 10 * 1000;
	int i;
	int to_h = p_ptr->to_h_m + a->to_h;
	int to_d = p_ptr->to_d_m + a->to_d;
	int dd = a->dd + p_ptr->innate_attack_info.to_dd;
	char buf[100];
	int mult = 100;
	int r, c;
	int result = row;

	/* First Column */
	r = row;
	c = col;
	sprintf(buf, " %-7.7s: Your %s (%dd%d)", "Attack", a->name, dd, a->ds);
	c_put_str(TERM_YELLOW, buf, r++, c);

	if (a->weight)
	{
		sprintf(buf, " %-7.7s: %d.%d lbs", "Weight", a->weight/10, a->weight%10);
		put_str(buf, r++, c);
	}

	sprintf(buf, " %-7.7s: %d + %d = %d", "To Hit", a->to_h, p_ptr->to_h_m, to_h);
	put_str(buf, r++, c);

	sprintf(buf, " %-7.7s: %d + %d = %d", "To Dam", a->to_d, p_ptr->to_d_m, to_d);
	put_str(buf, r++, c);
	
	sprintf(buf, " %-7.7s: %d", "Blows", a->blows);
	put_str(buf, r++, c);

	sprintf(buf, " %-7.7s", "Damage");
	c_put_str(TERM_L_GREEN, buf, r++, c);

	for (i = 0; i < ct; i++)
	{
		critical_t tmp = critical_norm(a->weight, to_h, 0, 0, HAND_NONE);
		if (tmp.desc)
		{
			crit.mul += tmp.mul;
			crit.to_d += tmp.to_d;
		}
		else
			crit.mul += 100;
	}
	crit.mul = crit.mul / ct;
	crit.to_d = crit.to_d * 100 / ct;
	sprintf(buf, " %-7.7s: %d.%02dx + %d.%02d", "Crits",
					crit.mul/100, crit.mul%100, crit.to_d/100, crit.to_d%100);
	put_str(buf, r++, c);
	crit.to_d /= 100;
	mult = mult * crit.mul / 100;
	to_d = to_d + crit.to_d;

	min_base = mult * dd / 100;
	min = min_base + to_d;
	min2 = 2*(min_base + a->to_d) + p_ptr->to_d_m;
	max_base = mult * dd * a->ds / 100;
	max = max_base + to_d;
	max2 = 2*(max_base + a->to_d) + p_ptr->to_d_m;
	
	if (a->effect[0] == GF_OLD_CONF) /* Hack for Umber Hulk ... */
	{
		c_put_str(TERM_L_BLUE, "Confuses", r++, c+10);
	}
	else
	{
		sprintf(buf, " %-7.7s: %d (%d-%d)",
				_effect_name(a->effect[0]),
				a->blows * (min + max)/2,
				a->blows * min,
				a->blows * max
		);
		c_put_str(TERM_WHITE, buf, r++, c);
	}

	for (i = 1; i < MAX_INNATE_EFFECTS; i++)
	{
		int p = a->effect_chance[i];
		char xtra[255];
		if (!a->effect[i]) break;
		if (!p)
			sprintf(xtra, "");
		else
			sprintf(xtra, " (%d%%)", p);

		switch (a->effect[i])
		{
		case GF_OLD_SLOW: 
			c_put_str(TERM_L_BLUE, format("Slows%s", xtra), r++, c+10);
			break;
		case GF_OLD_CONF: 
			c_put_str(TERM_L_BLUE, format("Confuses%s", xtra), r++, c+10);
			break;
		case GF_OLD_SLEEP: 
			c_put_str(TERM_L_BLUE, format("Sleeps%s", xtra), r++, c+10);
			break;
		case GF_STASIS:
			c_put_str(TERM_L_BLUE, format("Paralyzes%s", xtra), r++, c+10);
			break;
		case GF_DRAIN_MANA:
			c_put_str(TERM_L_BLUE, format("Drains Mana%s", xtra), r++, c+10);
			break;
		case GF_STUN:
			c_put_str(TERM_L_BLUE, format("Stuns%s", xtra), r++, c+10);
			break;
		default:
			sprintf(buf, " %-7.7s: %d (%d-%d)",
					_effect_name(a->effect[i]),
					a->blows * (min2 + max2)/2,
					a->blows * min2,
					a->blows * max2
			);
			c_put_str(TERM_RED, buf, r++, c);
		}
	}
	result = MAX(result, r);

	/* Second Column */
	r = row;
	c = col + 40;
	c_put_str(TERM_L_GREEN, "Accuracy", r++, c);
	put_str(" AC Hit", r++, c);

	sprintf(buf, "%3d %2d%%", 25, hit_chance_innate(to_h, 25));
	put_str(buf, r++, c);

	sprintf(buf, "%3d %2d%%", 50, hit_chance_innate(to_h, 50));
	put_str(buf, r++, c);

	sprintf(buf, "%3d %2d%%", 75, hit_chance_innate(to_h, 75));
	put_str(buf, r++, c);

	sprintf(buf, "%3d %2d%%", 100, hit_chance_innate(to_h, 100));
	put_str(buf, r++, c);

	sprintf(buf, "%3d %2d%%", 125, hit_chance_innate(to_h, 125));
	put_str(buf, r++, c);

	sprintf(buf, "%3d %2d%%", 150, hit_chance_innate(to_h, 150));
	put_str(buf, r++, c);

	sprintf(buf, "%3d %2d%%", 175, hit_chance_innate(to_h, 175));
	put_str(buf, r++, c);

	sprintf(buf, "%3d %2d%%", 200, hit_chance_innate(to_h, 200));
	put_str(buf, r++, c);

	result = MAX(result, r);
	return result;
}

/**********************************************************************
 * Ranged Attacks
 **********************************************************************/
static int _bow_mult_base(int sval)
{
	switch (sval)
	{
	case SV_SLING: return 2;
	case SV_SHORT_BOW: return 3;
	case SV_LONG_BOW: return 3;
	case SV_NAMAKE_BOW: return 3;
	case SV_LIGHT_XBOW: return 4;
	case SV_HEAVY_XBOW: return 4;
	}
	return 0;
}

/* Multiplier scaled by 100 */
static int _bow_mult(int sval)
{
	int mult = _bow_mult_base(sval);

	if (p_ptr->xtra_might) mult++;
	mult += p_ptr->shooter_info.to_mult;

	if (weaponmaster_is_(WEAPONMASTER_CROSSBOWS))
	{
		int idx = p_ptr->stat_ind[A_STR] + 4;
		if (idx > 40-3)
			idx = 40-3;
		mult = mult * (100 + (int)(adj_str_td[idx]) - 128);
	}
	else
		mult = mult * (100 + (int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128);

	return mult;
}

static void _display_missile_slay(int base_mult, int slay_mult, int shots, 
								  int dd, int ds, int to_d, int to_d_xtra, 
								  cptr name, int color, int row, int col)
{
	char buf[80];
	int mult, min, max;

	mult = slay_mult;
	mult = mult * base_mult / 100;

	if (p_ptr->concent) 
	{
		min = shots * (boost_concentration_damage(mult*(dd + to_d)/100) + to_d_xtra) / 100;
		max = shots * (boost_concentration_damage(mult*(dd*ds + to_d)/100) + to_d_xtra) / 100;
	}
	else
	{
		min = shots * (mult*(dd + to_d)/100 + to_d_xtra) / 100;
		max = shots * (mult*(dd*ds + to_d)/100 + to_d_xtra) / 100;
	}

	sprintf(buf, " %-8.8s", name);
	c_put_str(color, buf, row, col);

	sprintf(buf, ": %d (%d-%d) [%d.%02dx]", 
					(min + max)/2, min, max,
					mult/100, mult%100);
	put_str(buf, row, col+9);
}


static int _shooter_info_aux(object_type *bow, object_type *arrow, int row, int col, int ct)
{
	int          result = row;
	char         o_name[MAX_NLEN];
	char		 buf[255];
	u32b         flgs[TR_FLAG_SIZE];
	int          mult;
	int          to_h = 0;
	int          to_d = 0;
	int          to_h_bow = 0;
	int          to_d_bow = 0;
	int          to_h_xtra = p_ptr->shooter_info.dis_to_h;
	int          to_d_xtra = p_ptr->shooter_info.dis_to_d;
	int          dd = arrow->dd;
	int          ds = arrow->ds;
	critical_t   crit = {0}; 
	int			 num_fire = 0;
	int          r,c;

	missile_flags_known(arrow, flgs);
	mult = _bow_mult(bow->sval);

	if (p_ptr->shooter_info.num_fire)
		num_fire = p_ptr->shooter_info.num_fire * 100 * 100 / bow_energy(bow->sval);

	if (object_is_known(bow))
	{
		to_h_bow = bow->to_h;
		to_d_bow = bow->to_d;
		if (weaponmaster_is_(WEAPONMASTER_CROSSBOWS) && p_ptr->lev >= 15)
			to_d_bow += 1 + p_ptr->lev/10;
	} 

	if (object_is_known(arrow))
	{
		to_h = arrow->to_h;
		to_d = arrow->to_d;
	}

	if (p_ptr->big_shot)
		dd *= 2;

	{
		const int ct = 10 * 1000;
		int i;
		/* Compute Average Effects of Criticals by sampling */
		for (i = 0; i < ct; i++)
		{
			critical_t tmp = critical_shot(arrow->weight, arrow->to_h);
			if (tmp.desc)
			{
				crit.mul += tmp.mul;
				crit.to_d += tmp.to_d;
			}
			else
				crit.mul += 100;
		}
		crit.mul = crit.mul / ct;
		crit.to_d = crit.to_d * 100 / ct;
	}

	r = row;
	c = col;
	object_desc(o_name, arrow, OD_OMIT_INSCRIPTION);
	sprintf(buf, " Ammo #%-2d: %-54.54s", ct, o_name);
	c_put_str(TERM_UMBER, buf, r++, c);

	sprintf(buf, " %-8.8s: %d.%d lbs", "Weight", arrow->weight/10, arrow->weight%10);
	put_str(buf, r++, c);

	sprintf(buf, " %-8.8s: %d + %d = %d", "To Hit", to_h, to_h_bow + to_h_xtra, to_h + to_h_bow + to_h_xtra);
	put_str(buf, r++, c);

	sprintf(buf, " %-8.8s: %d + %d = %d (%s)", "To Dam", to_d, to_d_bow + to_d_xtra, to_d + to_d_bow + to_d_xtra, "Multiplier Applies");
	put_str(buf, r++, c);

	sprintf(buf, " %-8.8s", "Damage");
	c_put_str(TERM_L_GREEN, buf, r++, c);

	if (crit.to_d)
	{
		sprintf(buf, " %-8.8s: %d.%02dx + %d.%02d", "Crits",
						crit.mul/100, crit.mul%100, crit.to_d/100, crit.to_d%100);
	}
	else
	{
		sprintf(buf, " %-8.8s: %d.%02dx", "Crits",
						crit.mul/100, crit.mul%100);
	}
	put_str(buf, r++, c);

	to_d = to_d + to_d_bow;
	mult = mult * crit.mul / 100;
	to_d_xtra = to_d_xtra + crit.to_d/100;

	_display_missile_slay(mult, 100, num_fire, dd, ds, to_d, to_d_xtra, "Normal", TERM_WHITE, r++, c);

	if (have_flag(flgs, TR_KILL_ANIMAL)) 
		_display_missile_slay(mult, 270, num_fire, dd, ds, to_d, to_d_xtra, "Animals", TERM_YELLOW, r++, c);
	else if (have_flag(flgs, TR_SLAY_ANIMAL))
		_display_missile_slay(mult, 170, num_fire, dd, ds, to_d, to_d_xtra, "Animals", TERM_YELLOW, r++, c);
	
	if (have_flag(flgs, TR_KILL_EVIL))   
		_display_missile_slay(mult, 250, num_fire, dd, ds, to_d, to_d_xtra, "Evil", TERM_YELLOW, r++, c);
	else if (have_flag(flgs, TR_SLAY_EVIL))
		_display_missile_slay(mult, 150, num_fire, dd, ds, to_d, to_d_xtra, "Evil", TERM_YELLOW, r++, c);

	if (have_flag(flgs, TR_KILL_HUMAN))
		_display_missile_slay(mult, 270, num_fire, dd, ds, to_d, to_d_xtra, "Human", TERM_YELLOW, r++, c);
	else if (have_flag(flgs, TR_SLAY_HUMAN))
		_display_missile_slay(mult, 170, num_fire, dd, ds, to_d, to_d_xtra, "Human", TERM_YELLOW, r++, c);

	if (have_flag(flgs, TR_KILL_UNDEAD))
		_display_missile_slay(mult, 300, num_fire, dd, ds, to_d, to_d_xtra, "Undead", TERM_YELLOW, r++, c);
	else if (have_flag(flgs, TR_SLAY_UNDEAD))
		_display_missile_slay(mult, 200, num_fire, dd, ds, to_d, to_d_xtra, "Undead", TERM_YELLOW, r++, c);
	
	if (have_flag(flgs, TR_KILL_DEMON))  
		_display_missile_slay(mult, 300, num_fire, dd, ds, to_d, to_d_xtra, "Demons", TERM_YELLOW, r++, c);
	else if (have_flag(flgs, TR_SLAY_DEMON))
		_display_missile_slay(mult, 200, num_fire, dd, ds, to_d, to_d_xtra, "Demons", TERM_YELLOW, r++, c);

	if (have_flag(flgs, TR_KILL_ORC))  
		_display_missile_slay(mult, 300, num_fire, dd, ds, to_d, to_d_xtra, "Orcs", TERM_YELLOW, r++, c);
	else if (have_flag(flgs, TR_SLAY_ORC))
		_display_missile_slay(mult, 200, num_fire, dd, ds, to_d, to_d_xtra, "Orcs", TERM_YELLOW, r++, c);

	if (have_flag(flgs, TR_KILL_TROLL))  
		_display_missile_slay(mult, 300, num_fire, dd, ds, to_d, to_d_xtra, "Trolls", TERM_YELLOW, r++, c);
	else if (have_flag(flgs, TR_SLAY_TROLL))
		_display_missile_slay(mult, 200, num_fire, dd, ds, to_d, to_d_xtra, "Trolls", TERM_YELLOW, r++, c);

	if (have_flag(flgs, TR_KILL_GIANT))  
		_display_missile_slay(mult, 300, num_fire, dd, ds, to_d, to_d_xtra, "Giants", TERM_YELLOW, r++, c);
	else if (have_flag(flgs, TR_SLAY_GIANT))
		_display_missile_slay(mult, 200, num_fire, dd, ds, to_d, to_d_xtra, "Giants", TERM_YELLOW, r++, c);

	if (have_flag(flgs, TR_KILL_DRAGON))  
		_display_missile_slay(mult, 300, num_fire, dd, ds, to_d, to_d_xtra, "Dragons", TERM_YELLOW, r++, c);
	else if (have_flag(flgs, TR_SLAY_DRAGON))
		_display_missile_slay(mult, 200, num_fire, dd, ds, to_d, to_d_xtra, "Dragons", TERM_YELLOW, r++, c);
	
	if (have_flag(flgs, TR_BRAND_ACID))
		_display_missile_slay(mult, 170, num_fire, dd, ds, to_d, to_d_xtra, "Acid", TERM_RED, r++, c);

	if (have_flag(flgs, TR_BRAND_ELEC))
		_display_missile_slay(mult, 170, num_fire, dd, ds, to_d, to_d_xtra, "Elec", TERM_RED, r++, c);

	if (have_flag(flgs, TR_BRAND_FIRE))
		_display_missile_slay(mult, 170, num_fire, dd, ds, to_d, to_d_xtra, "Fire", TERM_RED, r++, c);

	if (have_flag(flgs, TR_BRAND_COLD))
		_display_missile_slay(mult, 170, num_fire, dd, ds, to_d, to_d_xtra, "Cold", TERM_RED, r++, c);

	if (have_flag(flgs, TR_BRAND_POIS))
		_display_missile_slay(mult, 170, num_fire, dd, ds, to_d, to_d_xtra, "Poison", TERM_RED, r++, c);

	result = MAX(result, r);

	/* Second Column */
	r = row;
	c = col + 66;

	to_h = to_h + to_h_bow + to_h_xtra;

	c_put_str(TERM_L_GREEN, " AC Hit", r++, c);

	sprintf(buf, "%3d %2d%%", 50, bow_hit_chance(bow->sval, to_h, 50));
	put_str(buf, r++, c);

	sprintf(buf, "%3d %2d%%", 100, bow_hit_chance(bow->sval, to_h, 100));
	put_str(buf, r++, c);

	sprintf(buf, "%3d %2d%%", 150, bow_hit_chance(bow->sval, to_h, 150));
	put_str(buf, r++, c);

	sprintf(buf, "%3d %2d%%", 175, bow_hit_chance(bow->sval, to_h, 175));
	put_str(buf, r++, c);

	sprintf(buf, "%3d %2d%%", 200, bow_hit_chance(bow->sval, to_h, 200));
	put_str(buf, r++, c);

	result = MAX(result, r);

	return result;
}

int display_shooter_info(int row, int col)
{
	object_type *bow_ptr = NULL;
	int          slot = equip_find_object(TV_BOW, SV_ANY);
	char         o_name[MAX_NLEN];
	char		 buf[255];
	int          mult;
	int			 num_fire = 0;
	int          to_h = 0;
	int          to_d = 0;
	int          r, c, i, j, w, h;

	Term_get_size(&w, &h);
	
	if (!slot || prace_is_(RACE_MON_JELLY))
	{
		sprintf(buf, " Shooting: %-49.49s", "Nothing");
		c_put_str(TERM_YELLOW, buf, row, col);
		return row+1;
	}

	bow_ptr = equip_obj(slot);
	assert(bow_ptr);

	mult = _bow_mult(bow_ptr->sval);

	if (p_ptr->shooter_info.num_fire)
		num_fire = p_ptr->shooter_info.num_fire * 100 * 100 / bow_energy(bow_ptr->sval);

	if (object_is_known(bow_ptr))
	{
		to_h = bow_ptr->to_h;
		to_d = bow_ptr->to_d;
		if (weaponmaster_is_(WEAPONMASTER_CROSSBOWS) && p_ptr->lev >= 15)
			to_d += 1 + p_ptr->lev/10;
	}

	/* Header */
	r = row;
	c = col;
	object_desc(o_name, bow_ptr, OD_OMIT_INSCRIPTION);
	sprintf(buf, " Shooting: %-70.70s", o_name);
	c_put_str(TERM_YELLOW, buf, r++, c);
	
	sprintf(buf, " %-8.8s: %d'", "Range", (bow_range(bow_ptr->sval) + 1) * 10);
	put_str(buf, r++, c);

	sprintf(buf, " %-8.8s: %d.%02d", "Shots", num_fire/100, num_fire%100);
	put_str(buf, r++, c);

	sprintf(buf, " %-8.8s: %d.%02dx", "Mult", mult/100, mult%100);
	put_str(buf, r++, c);

	sprintf(buf, " %-8.8s: %d + %d = %d", "To Hit", to_h, p_ptr->shooter_info.dis_to_h, to_h + p_ptr->shooter_info.dis_to_h);
	put_str(buf, r++, c);

	sprintf(buf, " %-8.8s: %d (%s)", "To Dam", to_d, "Multiplier Applies");
	put_str(buf, r++, c);

	sprintf(buf, " %-8.8s: %d (%s)", "Xtra Dam", p_ptr->shooter_info.dis_to_d, "Multiplier Does Not Apply");
	put_str(buf, r++, c);

	r++;

	/* Ammo */
	j = 0;
	for (i = 0; i < INVEN_PACK && r < h; i++)
	{
		if (inventory[i].tval == p_ptr->shooter_info.tval_ammo)
		{
			r = _shooter_info_aux(bow_ptr, &inventory[i], r, c, ++j) + 1;
		}
	}

	return r;
}
