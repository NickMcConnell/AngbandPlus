#define XTRA1_C
/* File: misc.c */

/* Purpose: misc code */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

/*
 * Converts a number (0 to 365) into a date
 */
static void day_to_date(int day,char *date)
{
	cptr mon[13]={"","Jan","Feb","Mar","Apr","May","Jun",
		"Jul","Aug","Sep","Oct","Nov","Dec"};
	s16b days[13]={0,31,60,91, 121,152,182, 213,244,274,305,335,366};

	byte i;

	day = YEARDAY(day);

	for (i = 1; i < 13; i++)
	{
		if (day < days[i])
	{
			sprintf(date, "%2d %s ", day-days[i-1]+1, mon[i]);
		return;
	}
	}
	/* Something's gone horribly wrong */
	sprintf(date, "??????");
		return;
	}

/*
 * A vstrnfmt_aux wrapper around day_to_date().
 */
void day_to_date_f1(char *buf, uint max, cptr UNUSED fmt, va_list *vp)
{
	int day = va_arg(*vp, int);
	if (max > strlen("10 Jan "))
	{
		day_to_date(day, buf);
	}
	else if (alert_failure)
	{
		msg_format("Only given %d characters for day_to_date(), %d needed!",
			max, strlen("10 Jan ")+1);
	}
}

/*
 * Converts stat num into a six-char (right justified) string
 */
static void cnv_stat(char *out_val, int val)
{
	if (val < 19)
	{
		sprintf(out_val, "    %2d", val);
		}
	else if (val < 8119)
		{
		sprintf(out_val, " %2d/%02d", (val-18)/100+18, (val-18)%100);
		}
	else
	{
		strcpy(out_val, " XX/XX");
	}
}

void cnv_stat_f1(char *buf, uint max, cptr UNUSED fmt, va_list *vp)
{
	int val = va_arg(*vp, int);

	/* Paranoia. */
	if (max < 7) return;

	cnv_stat(buf, val);
}


/*
 * Modify a stat value by a "modifier", return new value
 *
 * Stats go up: 3,4,...,17,18,18/10,18/20,...,18/220
 * Or even: 18/13, 18/23, 18/33, ..., 18/220
 *
 * Stats go down: 18/220, 18/210,..., 18/10, 18, 17, ..., 3
 * Or even: 18/13, 18/03, 18, 17, ..., 3
 */
s16b modify_stat_value(int value, int amount)
{
	int    i;

	/* Reward */
	if (amount > 0)
	{
		/* Apply each point */
		for (i = 0; i < amount; i++)
		{
			/* One point at a time */
			if (value < 18) value++;

			/* Ten "points" at a time */
			else value += 10;
		}
	}

	/* Penalty */
	else if (amount < 0)
	{
		/* Apply each point */
		for (i = 0; i < (0 - amount); i++)
		{
			/* Ten points at a time */
			if (value >= 18+10) value -= 10;

			/* Hack -- prevent weirdness */
			else if (value > 18) value = 18;

			/* One point at a time */
			else if (value > 3) value--;
		}
	}

	/* Return new value */
	return (value);
}


/*
 * Translate a negative co-ordinate into one relative to the far edge of the
 * screen.
 */
static int PURE get_y(int i)
{
	const redraw_type *t = screen_coords+i;
	if (t->y >= 0) return t->y;
	else return Term->hgt + t->y;
}

static int PURE get_x(int i)
{
	const redraw_type *t = screen_coords+i;
	if (t->x >= 0) return t->x;
	else return Term->wid + t->x;
}

/* Shorten "put it where the table says it should go" for y,x functions. */
#define GET_YX(T) get_y(T), get_x(T), screen_coords[T].l

static void prt_equippy(void)
{
	mc_put_lfmt(GET_YX(XY_EQUIPPY), "%v", equippy_f0);
}

/*
 * Print character stat in given row, column
 */
static void prt_stat(int stat)
{
	char attr, colon;
	cptr name;

	/* Choose a colour (white >18/219, yellow reduced, light green normal. */
	if (p_ptr->stat_use[stat] > 18+219) attr = 'w';
	else if (p_ptr->stat_cur[stat] < p_ptr->stat_max[stat]) attr = 'y';
	else attr = 'G';

	/* Choose a name style (reduced or normal) */
	if (p_ptr->stat_cur[stat] < p_ptr->stat_max[stat])
		name = stat_names_reduced[stat];
	else
		name = stat_names[stat];

	/* Indicate natural maximum */
	if (p_ptr->stat_max[stat] == 18+100)
	{
		colon = '!';
	}
	else
	{
		colon = ':';
	}

	/* Display name and number */
	mc_put_lfmt(GET_YX(XY_STAT+stat), "%.3s%c  $%c%v",
		name, colon, attr, cnv_stat_f1, p_ptr->stat_use[stat]);
}

/*
 * Prints current gold
 */
static void prt_gold(void)
{
	mc_put_lfmt(GET_YX(XY_GOLD), "AU $G%9ld", (long)p_ptr->au);
}

/*
 * Prints current time
 */
static void prt_time(void)
{
	int minute = ((turn % ((10L * TOWN_DAWN)) * 1440) / ((10L * TOWN_DAWN)));
	int hour = ((minute/60)+6)%24; /* 0 to 23 */
	int day = (turn + (10L * TOWN_DAWN / 4)) / (10L * TOWN_DAWN) + 1;

	/* Only keep loose minutes */
	minute = minute % 60;

	mc_put_lfmt(GET_YX(XY_TIME), "%2d:%02d %v",
		hour, minute, day_to_date_f1, day+p_ptr->startdate);
}


/*
 * Prints current AC
 */
static void prt_ac(void)
{
	mc_put_lfmt(GET_YX(XY_AC), "AC:    $G%5d", p_ptr->dis_ac+p_ptr->dis_to_a);
}

/*
 * Prints energy cost of last turn
 */
static void prt_energy(void)
{
	mc_put_lfmt(GET_YX(XY_ENERGY), "LE:     $G%4d", old_energy_use);
}

/*
 * Prints Cur/Max hit points
 */
static void prt_hp(void)
{
	prt_nums("HP:", GET_YX(XY_HP), p_ptr->chp, p_ptr->mhp);
}


/*
 * Prints players max/cur spell points and chi points
 */
static void prt_sp(void)
{
	prt_nums("SP:", GET_YX(XY_SP), p_ptr->csp, p_ptr->msp);
	prt_nums("CH:", GET_YX(XY_CHI), p_ptr->cchi, p_ptr->mchi);
}

/*
 * Displays spirit relationships
 *
 * Colour code:
 * Black = No relationship
 * Light Dark = Too powerful
 * Red = Furious or angry
 * Yellow = Annoyed or irriated
 * Green = Placated
 */
static void prt_spirit(void)
{
	int i, j;
	const int plev = MAX(1, skill_set[SKILL_SHAMAN].value/2);

	char life[25] = "Life", *ls = life+4;
	char wild[25] = "Wild", *ws = wild+4, **s;

	for (i = j = 0; i < MAX_SPIRITS; i++)
	{
		spirit_type *s_ptr = spirits+i;

		/* Add to the appropriate string. */
		if (i % 2) s = &ws;
		else s = &ls;

		if(!(s_ptr->pact))
		{
			*s += sprintf(*s, " $d#");
		}
		else if (s_ptr->minskill > plev)
		{
			*s += sprintf(*s, " $D#");
		}
		else if(s_ptr->annoyance > 8)
		{
			*s += sprintf(*s, " $r%c", I2A(j++));
		}
		else if(s_ptr->annoyance > 0)
		{
			*s += sprintf(*s, " $y%c", I2A(j++));
		}
		else
		{
			*s += sprintf(*s, " $G%c", I2A(j++));
		}
	}
	mc_put_lfmt(GET_YX(XY_LIFE_SPIRIT), "%s", life);
	mc_put_lfmt(GET_YX(XY_WILD_SPIRIT), "%s", wild);
}

/*
 * Prints depth in stat area
 */
static void prt_depth(void)
{
	s16b level = dun_level + dun_offset;
	cptr descr[2][2] = {
	{"(Lev %d)", "%d ft"},
	{"/(Lev %d)\\", "/%d ft\\"}
	};
	if (depth_in_feet) level *= 50;

	if (dun_level)
	{
		bool tower = (dun_defs[cur_dungeon].flags & DF_TOWER) != 0;
		mc_put_lfmt(GET_YX(XY_DEPTH), descr[!!tower][!!depth_in_feet], level);
	}

	else if (wild_grid[wildy][wildx].dungeon < MAX_CAVES)
	{
		mc_put_lfmt(GET_YX(XY_DEPTH), "%s", 
			dun_name+dun_defs[wild_grid[wildy][wildx].dungeon].shortname);
	}
	else
	{
		mc_put_lfmt(GET_YX(XY_DEPTH), "Wild (%d,%d)",wildx,wildy);
	}
}



/*
 * Prints Searching, Resting, Paralysis, or 'count' status
 * Display is always exactly 10 characters wide (see below)
 *
 * This function was a major bottleneck when resting, so a lot of
 * the text formatting code was optimized in place below.
 */
static void prt_state(void)
{
	char text[16];
	cptr s;
	int rep = (command_rep >= 1000) ? command_rep/100*100 : command_rep;

	/* Paralysed */
	if ((s = prt_flag(TIMED_PARALYZED))) ;

	/* Repeated command. */
	else if (rep)
	{
		/* Timed resting */
		if (command_cmd == 'R')
		{
			/* Resting until recovered HP/SP. */
			if (rep == -2)
			{
				s = "Rest *****";
			}
			/* Resting until completely healed. */
			else if (rep == -3)
			{
				s = "Rest &&&&&";
			}
			else
			{
				sprintf(text, "Rest %5d", rep);
				s = text;
			}
		}
		else
		{
			sprintf(text, "Rep%s %3d", (rep > 1000) ? "." : "eat", rep);
			s = text;
		}
	}

	/* Sneaking */
	else if (p_ptr->sneaking)
	{
		s = "Sneaking";
	}

	/* Nothing interesting */
	else
	{
		s = "";
	}

	/* Display the info (or blanks) */
	mc_put_lfmt(GET_YX(XY_STATE), "%s", s);
}


/*
 * Prints the speed of a character. -CJS-
 */
static void prt_speed(void)
{
	int i = p_ptr->pspeed - 110;
	cptr change;

	/* Fast */
	if (i > 0) change = "$GFast";

	/* Slow */
	else if (i < 0) change = "$USlow";

	/* Normal" */
	else change = "$d";

	/* Display the speed */
	mc_put_lfmt(GET_YX(XY_SPEED), "%-14v", vstrnfmt_fn, "%s (%+d)", change, i);
}

/*
 * Displays temporary resistance in preference to studying ability as there's
 * not much call for libraries in the dungeon.
 */

static void prt_study(void)
{
	char buf[16], *t = buf;

	if (p_ptr->oppose_acid || p_ptr->oppose_elec || p_ptr->oppose_fire ||
		p_ptr->oppose_cold || p_ptr->oppose_pois)
	{
		t += sprintf(t, "$%cA", atchar[OPPOSE_COL(p_ptr->oppose_acid)]);
		t += sprintf(t, "$%cE", atchar[OPPOSE_COL(p_ptr->oppose_elec)]);
		t += sprintf(t, "$%cF", atchar[OPPOSE_COL(p_ptr->oppose_fire)]);
		t += sprintf(t, "$%cC", atchar[OPPOSE_COL(p_ptr->oppose_cold)]);
		t += sprintf(t, "$%cP", atchar[OPPOSE_COL(p_ptr->oppose_pois)]);
	}
	else if (p_ptr->new_spells)
	{
		sprintf(t, "Study");
	}
	else
	{
		*t = '\0';
	}
	mc_put_lfmt(GET_YX(XY_STUDY), "%s", buf);
}


/*
 * Redraw the "monster health bar" -DRS-
 * Rather extensive modifications by -BEN-
 *
 * The "monster health bar" provides visual feedback on the "health"
 * of the monster currently being "tracked".  There are several ways
 * to "track" a monster, including targetting it, attacking it, and
 * affecting it (and nobody else) with a ranged attack.
 *
 * Display the monster health bar (affectionately known as the
 * "health-o-meter").  Clear health bar if nothing is being tracked.
 * Auto-track current target monster when bored.  Note that the
 * health-bar stops tracking any monster that "disappears".
 *
 * Modified to give "character" as well as colour info by Dean Anderson
 */
static void health_redraw(void)
{

#ifdef DRS_SHOW_HEALTH_BAR

	cptr str, smb;
	int pct, len = 0;

	monster_type *m_ptr = &m_list[health_who];

	/* Default to almost dead */
	char attr = 'r';
	smb = "**********";

	/* Not tracking */
	if (!health_who)
	{
		/* Erase the health bar */
		str = "            ";
	}

	/* Tracking an unseen monster */
	else if (!m_list[health_who].ml)
	{
		/* Indicate that the monster health is "unknown" */
		str = "[----------]";
	}

	/* Tracking a hallucinatory monster */
	else if (p_ptr->image)
	{
		/* Indicate that the monster health is "unknown" */
		str = "[----------]";
	}

	/* Tracking a dead monster (???) */
	else if (!m_list[health_who].hp < 0)
	{
		/* Indicate that the monster health is "unknown" */
		str = "[----------]";
	}

	/* Tracking a visible monster */
	else
	{
		str = "[----------]";

		/* Extract the "percent" of health */
		pct = 100L * m_ptr->hp / m_ptr->maxhp;

		/* Badly wounded */
		if (pct >= 10) attr = 'R';

		/* Wounded */
		if (pct >= 25) attr = 'o';

		/* Somewhat Wounded */
		if (pct >= 60) attr = 'y';

		/* Healthy */
		if (pct >= 100) attr = 'G';

		/* Afraid */
		if (m_ptr->monfear) {
			attr = 'v';
			smb = "AFRAID****";
		}
		/* Asleep */
		if (m_ptr->csleep) {
			attr = 'B';
			smb = "SLEEPING**";
		}
		if (m_ptr->smart & SM_ALLY) {
			attr = 'U';
			smb = "ALLY******";
		}
		/* Convert percent into "health" */
		len = (pct < 10) ? 1 : (pct < 90) ? (pct / 10 + 1) : 10;
	}

	/* Dump the current "health" (use '*' symbols) */
	mc_put_lfmt(GET_YX(XY_INFO), "%c$%c%.*s$w%s",
		str[0], attr, len, smb, str+len+1);

#endif

}


/*
 * Calculate number of spells player should have, and forget,
 * or remember, spells until that number is properly reflected.
 *
 * Note that this function induces various "status" messages,
 * which must be bypasses until the character is created.
 */
static void calc_spells(void)
{
	const s16b old_new_spells = p_ptr->new_spells;

	int i, j, k;
	int num_allowed, num_known;
	magic_type *s_ptr;


	cptr p = "spell";


	/* Hack -- wait for creation */
	if (!character_generated) return;

	/* Hack -- handle "xtra" mode */
	if (character_xtra) return;


	/* Extract total allowed spells */
	num_allowed = ((adj_mag_study[p_ptr->stat_ind[A_INT]] *
					skill_set[SKILL_MANA].value) / 4);

	/* Ensure at least one */
	num_allowed++;


	/* Assume none known */
	num_known = 0;

	/* Count the number of spells we know */
	for (i=0; i<MAX_SCHOOL*MAX_SPELLS_PER_BOOK; i++)
	{
		/* Get the spell. */
		s_ptr = num_to_spell(i);

		/* Not a spell. */
		if (!s_ptr) continue;

		/* Count known spells */
		if (s_ptr->flags & MAGIC_LEARNED) num_known++;
	}

	/* See how many spells we must forget or may learn */
	p_ptr->new_spells = num_allowed - num_known;

	/* Forget spells which are too hard */
	for (i = 127; i >= 0; i--)
	{
		/* Access the spell */
		j = spell_order[i];

		/* Skip non-spells */
		if (j >= 255) continue;

		/* Get the spell. */
		s_ptr = num_to_spell(j);

		/* Skip spells we are allowed to know */
		if (s_ptr->min <= spell_skill(s_ptr)) continue;

		/* Forget it (if learned) */
		if (s_ptr->flags & MAGIC_LEARNED)
		{
			/* Mark as forgotten */
			s_ptr->flags |= MAGIC_FORGOT;

			/* No longer known */
			s_ptr->flags &= ~MAGIC_LEARNED;

			/* Message */
			msg_format("You have forgotten the %s of %s.", p, s_ptr->name);

			/* One more can be learned */
			p_ptr->new_spells++;
		}
	}


	/* Forget spells if we know too many spells */
	for (i = 127; i >= 0; i--)
	{
		/* Stop when possible */
		if (p_ptr->new_spells >= 0) break;


		/* Get the (i+1)th spell learned */
		j = spell_order[i];
		s_ptr = num_to_spell(j);

		/* Skip unknown spells */
		if (j >= 255) continue;

		/* Get the spell. */
		s_ptr = num_to_spell(j);

		/* Forget it (if learned) */
		if (s_ptr->flags & MAGIC_LEARNED)
		{
			/* Mark as forgotten */
			s_ptr->flags |= MAGIC_FORGOT;

			/* No longer known */
			s_ptr->flags &= ~MAGIC_LEARNED;

			/* Message */
			msg_format("You have forgotten the %s of %s.", p, s_ptr->name);

			/* One more can be learned */
			p_ptr->new_spells++;
		}
	}


	/* Check for spells to remember */
	for (i = 0; i < 128; i++)
	{
		/* None left to remember */
		if (p_ptr->new_spells <= 0) break;

		/* Get the next spell we learned */
		j = spell_order[i];

		/* Skip unknown spells */
		if (j >= 255) break;

		/* Get the spell. */
		s_ptr = num_to_spell(j);

		/* Skip spells we cannot remember */
		if (s_ptr->min > spell_skill(s_ptr)) continue;

		/* First set of spells */
		if (s_ptr->flags & MAGIC_FORGOT)
		{
			/* No longer forgotten */
			s_ptr->flags &= ~MAGIC_FORGOT;

			/* Known once more */
			s_ptr->flags |= MAGIC_LEARNED;

			/* Message */
			msg_format("You have remembered the %s of %s.", p, s_ptr->name);

			/* One less can be learned */
			p_ptr->new_spells--;
		}
	}


	/* Count spells that can be learned */
	for (j = k = 0; j < 128; j++)
	{
		/* Access the spell */
		s_ptr = num_to_spell(j);

		/* Skip spells we cannot remember */
		if (s_ptr->min > spell_skill(s_ptr)) continue;

		/* Skip spells we already know */
		if (s_ptr->flags & MAGIC_LEARNED) continue;

		/* Count it */
		k++;
	}


	/* Cannot learn more spells than exist */
	if (p_ptr->new_spells > k) p_ptr->new_spells = k;



	/* Spell count changed */
	if (old_new_spells != p_ptr->new_spells)
	{

		/* Message if needed and allowed. */
		if (p_ptr->new_spells)
		{
			/* Message */
			msg_format("You can learn %d more %s%s.", p_ptr->new_spells, p,
				(p_ptr->new_spells != 1) ? "s" : "");
		}

		/* Redraw Study Status */
		p_ptr->redraw |= (PR_STUDY);
	}
}

/*
 * Calculated the AC bonus from a given empty armour slot.
 * This does not check that the player is eligible for such bonuses
 * in the first place.
 */
static int mystic_armour(int slot)
{
	/* Not empty. */
	if (inventory[slot].k_idx) return 0;

	switch (slot)
	{
		/* Run the formula for the slot. */
		case INVEN_WIELD: return 5;
		case INVEN_BODY: return (skill_set[SKILL_MA].value * 3) / 4;
		case INVEN_OUTER: return MAX(0, ((skill_set[SKILL_MA].value - 26) / 6));
		case INVEN_ARM: return MAX(0, ((skill_set[SKILL_MA].value - 16) / 6));
		case INVEN_HEAD: return MAX(0, (skill_set[SKILL_MA].value - 4) / 6);
		case INVEN_HANDS: return (skill_set[SKILL_MA].value / 4);
		case INVEN_FEET: return (skill_set[SKILL_MA].value / 6);
		default: return 0;
	}
}

/*
 * Return whether a given object inhibits spellcasting.
 */
bool PURE cumber_glove(object_ctype *o_ptr)
{
	u32b f[3];

	/* Only gloves can inhibit spellcasting. */
	if (wield_slot(o_ptr) != INVEN_HANDS) return FALSE;

	object_flags(o_ptr, f, f+1, f+2);

	/* Free action helps spellcasting. */
	if (f[1] & (TR2_FREE_ACT)) return FALSE;

	/* A dexterity bonus helps spellcasting. */
	if ((f[0] & (TR1_DEX)) && (o_ptr->pval > 0)) return FALSE;

	/* Other gloves harm spellcasting. */
	return TRUE;
}


/*
 * Return whether a given object inhibits mindcrafting.
 */
bool PURE cumber_helm(object_ctype *o_ptr)
{
	u32b f[3];

	/* Only helmets can inhibit mindcrafting. */
	if (wield_slot(o_ptr) != INVEN_HEAD) return FALSE;

	object_flags(o_ptr, f, f+1, f+2);

	/* Telepathy helps mindcraft. */
	if (f[2] & (TR3_TELEPATHY)) return FALSE;

	/* A wisdom bonus helps mindcraft. */
	if ((f[0] & (TR1_WIS)) && (o_ptr->pval > 0)) return FALSE;

	/* Other helmets harm mindcraft. */
	return TRUE;
}

#define MAX_SPELL_WEIGHT 300 /* Max weight for spellcasting */

/*
 * Calculate maximum mana.  You do not need to know any spells.
 * Note that mana is lowered by heavy (or inappropriate) armor.
 *
 * This function induces status messages.
 *
 * This function also calculates maximum chi
 */
static void calc_mana(void)
{
	int msp, levels, cur_wgt, max_wgt;
	int     mchi;

	const bool old_cumber_armor = p_ptr->cumber_armor;
	const bool old_cumber_glove = p_ptr->cumber_glove;
	const bool old_cumber_helm = p_ptr->cumber_helm;

	object_type *o_ptr;



	levels = skill_set[SKILL_MANA].value;


	/* Hack -- no negative mana */
	if (levels < 1) levels = 1;

	/* Extract total mana */
	msp = (adj_mag_mana[p_ptr->stat_ind[A_INT]]) * levels / 4;

	/* Hack -- add one mana */
	msp++;

	/* Assume player is not encumbered by gloves */
	p_ptr->cumber_glove = FALSE;

	/* Get the gloves */
	o_ptr = &inventory[INVEN_HANDS];

	/* Normal gloves hurt wizard spells */
	if (cumber_glove(o_ptr))
	{
		/* Encumbered */
		p_ptr->cumber_glove = TRUE;

		/* Reduce mana */
		msp = (3 * msp) / 4;
	}


	/* Assume player not encumbered by armor */
	p_ptr->cumber_armor = FALSE;

	/* Weigh the armor */
	cur_wgt = 0;
	cur_wgt += inventory[INVEN_BODY].weight;
	cur_wgt += inventory[INVEN_HEAD].weight;
	cur_wgt += inventory[INVEN_ARM].weight;
	cur_wgt += inventory[INVEN_OUTER].weight;
	cur_wgt += inventory[INVEN_HANDS].weight;
	cur_wgt += inventory[INVEN_FEET].weight;

	/* Determine the weight allowance */
	max_wgt = MAX_SPELL_WEIGHT;

	/* Heavy armor penalizes mana */
	if (((cur_wgt - max_wgt) / 10) > 0)
	{
		/* Encumbered */
		p_ptr->cumber_armor = TRUE;

		/* Reduce mana */
		msp -= ((cur_wgt - max_wgt) / 10);
	}


	/* Mana can never be negative */
	if (msp < 0) msp = 0;


	/* Maximum mana has changed */
	if (p_ptr->msp != msp)
	{

		/* XXX XXX XXX New mana maintenance */

		/* Enforce maximum */
		if (p_ptr->csp >= msp)
		{
			p_ptr->csp = msp;
			p_ptr->csp_frac = 0;
		}

		/* Save new mana */
		p_ptr->msp = msp;

		/* Display mana later */
		p_ptr->redraw |= (PR_MANA);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);
		p_ptr->window |=(PW_SPELL);
	}

	/* Now repeat the whole process for chi */
	levels = skill_set[SKILL_CHI].value;


	/* Hack -- no negative chi */
	if (levels < 1) levels = 1;

	/* Extract total chi */
	mchi = (adj_mag_mana[p_ptr->stat_ind[A_WIS]]) * levels / 4;

	/* Hack -- add one chi */
	mchi++;

	/* Get the helmet */
	o_ptr = &inventory[INVEN_HEAD];

	/* Normal helms hurt mindcrafting */
	if (cumber_helm(o_ptr))
	{
		/* Encumbered */
		p_ptr->cumber_helm = TRUE;

		/* Reduce mana */
		mchi = (3 * mchi) / 4;
	}
	/* Special helms do not. */
	else
	{
		p_ptr->cumber_helm = FALSE;
	}


	/* Chi can never be negative */
	if (mchi < 0) mchi = 0;

	/* Maximum chi has changed */
	if (p_ptr->mchi != mchi)
	{

		/* XXX XXX XXX Chi maintenance */

		/* Enforce maximum */
		if (p_ptr->cchi >= mchi)
		{
			p_ptr->cchi = mchi;
			p_ptr->chi_frac = 0;
		}

		/* Save new chi */
		p_ptr->mchi = mchi;

		/* Display chi later */
		p_ptr->redraw |= (PR_MANA);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);
		p_ptr->window |=(PW_SPELL);
	}

	/* Hack -- handle "xtra" mode */
	if (character_xtra) return;

	/* Take note when "glove state" changes */
	if (old_cumber_glove != p_ptr->cumber_glove)
	{
		/* Message */
		if (p_ptr->cumber_glove)
		{
			msg_print("Your covered hands feel unsuitable for spellcasting.");
		}
		else
		{
			msg_print("Your hands feel more suitable for spellcasting.");
		}
	}

	/* Take note when "helm state" changes */
	if (old_cumber_helm != p_ptr->cumber_helm)
	{
		/* Message */
		if (p_ptr->cumber_helm)
		{
			msg_print("Your covered head feels unsuitable for mindcrafting.");
		}
		else
		{
			msg_print("Your head feels more suitable for mindcrafting.");
		}
	}

	/* Take note when "armor state" changes */
	if (old_cumber_armor != p_ptr->cumber_armor)
	{
		/* Message */
		if (p_ptr->cumber_armor)
		{
			msg_print("The weight of your armor encumbers your movement.");
		}
		else
		{
			msg_print("You feel able to move more freely.");
		}
	}
}



/*
 * Calculate the players (maximal) hit points
 * Adjust current hitpoints if necessary
 */
static void calc_hitpoints(void)
{
	int bonus, mhp;

	/* Un-inflate "half-hitpoint bonus per level" value */
	bonus = ((int)(adj_con_mhp[p_ptr->stat_ind[A_CON]]) - 128);

	/* Calculate hitpoints */
	mhp = player_hp[skill_set[SKILL_TOUGH].value-1]/2 +
		(bonus*skill_set[SKILL_TOUGH].value/4);

	/* Always have at least one hitpoint per level */
	if (mhp < skill_set[SKILL_TOUGH].value) mhp = skill_set[SKILL_TOUGH].value;

	/* Factor in the hero / superhero settings */
	if (p_ptr->hero) mhp += 10;
	if (p_ptr->shero) mhp += 30;

	/* New maximum hitpoints */
	if (p_ptr->mhp != mhp)
	{
		/* XXX XXX XXX New hitpoint maintenance */

		/* Enforce maximum */
		if (p_ptr->chp >= mhp)
		{
			p_ptr->chp = mhp;
			p_ptr->chp_frac = 0;
		}

		/* Save the new max-hitpoints */
		p_ptr->mhp = mhp;

		/* Display hitpoints (later) */
		p_ptr->redraw |= (PR_HP);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);
	}
}



/*
 * Extract and set the current "lite radius"
 *
 * SWD: Experimental modification: multiple light sources have additive effect.
 *
 */
static void calc_torch(void)
{
	const s16b old_cur_lite = p_ptr->cur_lite;

	int i;
	object_type *o_ptr;
	u32b f1, f2, f3;

	/* Assume no light */
	p_ptr->cur_lite = 0;

	/* Loop through all wielded items */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		o_ptr = &inventory[i];

		/* Skip empty slots */
		if (!o_ptr->k_idx) continue;

		/* Examine actual lites */
		if ((i == INVEN_LITE) && (o_ptr->k_idx) && (o_ptr->tval == TV_LITE)
			&& (allart_p(o_ptr) || o_ptr->pval > 0))
		{
			p_ptr->cur_lite += k_info[o_ptr->k_idx].extra;
		}

		/* Extract the flags */
		object_flags(o_ptr, &f1, &f2, &f3);

		/* Check for extra light. */
		if (f3 & TR3_LITE) p_ptr->cur_lite++;
	}

	/* max radius is 5 without rewriting other code -- */
	/* see cave.c:update_lite() and defines.h:LITE_MAX */
	if (p_ptr->cur_lite > 5) p_ptr->cur_lite = 5;

	/* check if the player doesn't have a lite source, */
	/* but does glow as an intrinsic.                  */
	if (p_ptr->cur_lite == 0 && p_ptr->lite) p_ptr->cur_lite = 1;

	/* end experimental mods */

	/* Reduce lite when running if requested */
	if (command_cmd == '.' && view_reduce_lite)
	{
		/* Reduce the lite radius if needed */
		if (p_ptr->cur_lite > 1) p_ptr->cur_lite = 1;
	}

	/* Notice changes in the "lite radius" */
	if (old_cur_lite != p_ptr->cur_lite)
	{
		/* Update the lite */
		p_ptr->update |= (PU_LITE);

		/* Update the monsters */
		p_ptr->update |= (PU_MONSTERS);
	}
}



/*
 * Computes current weight limit.
 */
static int weight_limit(void)
{
	int i;

	/* Weight limit based only on strength */
	i = adj_str_wgt[p_ptr->stat_ind[A_STR]] * 100;

	/* Return the result */
	return (i);
}


/* Calculate the skill used by a certain weapon. */

int PURE wield_skill(object_ctype *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_DIGGING:
		case TV_SWORD:
		{
			return k_info[o_ptr->k_idx].extra;
		}
		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
		case TV_BOW:
		{
			return SKILL_MISSILE;
		}
		/* An empty object will have a tval of 0. */
		case 0:
		{
			return SKILL_CLOSE;
		}
		default:
		{
			return MAX_SKILLS;
		}
	}
}

/*
 * Test whether the worn armour weighs too much for effective martial arts.
 */
static bool ma_heavy_armor(void)
{

	int arm_wgt = 0;
	arm_wgt += inventory[INVEN_BODY].weight;
	arm_wgt += inventory[INVEN_HEAD].weight;
	arm_wgt += inventory[INVEN_ARM].weight;
	arm_wgt += inventory[INVEN_OUTER].weight;
	arm_wgt += inventory[INVEN_HANDS].weight;
	arm_wgt += inventory[INVEN_FEET].weight;

	return (arm_wgt > 100 + skill_set[SKILL_MA].value * 2);
}


/*
 * Calculate the martial arts AC bonus from empty slots, set the AC for
 * each appropriately and set p_ptr->ma_armour to the total.
 */
static void calc_ma_armour(void)
{
	int i;
	bool ma_good = ma_empty_hands() && !p_ptr->ma_cumber_armour;

	/* Give a small AC bonus to hands which are free to deflect blows. */
	if (ma_good)
	{
	}

	for (i=INVEN_WIELD, p_ptr->ma_armour = 0; i<=INVEN_FEET; i++)
	{
		/* Armour there, so do nothing. */
		if (inventory[i].k_idx);

		/* Add in the bonus AC. */
		else if (ma_good)
		{
			inventory[i].to_a = mystic_armour(i);
			p_ptr->ma_armour += inventory[i].to_a;
		}

		/* No bonus AC. */
		else
		{
			inventory[i].to_a = 0;
		}
	}

	/* Display the changes. */
	p_ptr->redraw |= PR_ARMOR;
	p_ptr->window |= PW_EQUIP | PW_PLAYER;
}


/*
 * Is this flag associated with an integer (rather than a boolean)?
 */
static bool PURE is_pval_flag(int set, int flag)
{
	/* Choose the appropriate mask for the set. */
	u32b mask = (set == TR0) ? TR0_PVAL_MASK : (set == TR1) ? TR1_PVAL_MASK : 0;

	/* Compare it with the flag. */
	return ((1L<<flag) & mask) != 0;
}

/*
 * Add the flags on an object to the flags table.
 */
static void calc_bonuses_object(s16b (*flags)[32], object_ctype *o_ptr)
{
	u32b f[3], a;
	int i, j;

	/* Weapons don't contribute to the player's offensive power here. */
	const bool is_weapon = (o_ptr == inventory+INVEN_WIELD ||
		o_ptr == inventory+INVEN_BOW);

	const bool known = object_known_p(o_ptr);

	assert(flags+4 && o_ptr); /* Caller */

	object_flags(o_ptr, f, f+1, f+2);

	/* Add the flags, setting to the pval or TRUE as appropriate. */
	for (i = 0, a = 1; i < 32; i++, a *= 2)
	{
		for (j = TR1; j <= TR3; j++)
		{
			/* Absent flags. */
			if (~f[j-TR1] & a) continue;

			if (is_pval_flag(j, i))
			{
				flags[j][i] += o_ptr->pval;
			}
			else
			{
				flags[j][i] = TRUE;
			}
		}
	}

	/* Add some other object variables as appropriate. */
	flags[TR0][iilog(TR0_AC)] += o_ptr->ac + o_ptr->to_a;
	flags[TR0][iilog(TR0_DIS_AC)] += o_ptr->ac;
	if (known) flags[TR0][iilog(TR0_DIS_TO_A)] += o_ptr->to_a;
	if (!is_weapon) flags[TR0][iilog(TR0_TO_H)] += o_ptr->to_h;
	if (!is_weapon) flags[TR0][iilog(TR0_TO_D)] += o_ptr->to_d;
	if (!is_weapon && known) flags[TR0][iilog(TR0_DIS_TO_H)] += o_ptr->to_h;
	if (!is_weapon && known) flags[TR0][iilog(TR0_DIS_TO_D)] += o_ptr->to_d;
}

static race_bonus muta_bonuses[] =
{
	{MUT_HYPER_STR, 0, 0, TR1, iilog(TR1_STR), 4},
	{MUT_PUNY, 0, 0, TR1, iilog(TR1_STR), -4},
	{MUT_HYPER_INT, 0, 0, TR1, iilog(TR1_INT), 4},
	{MUT_HYPER_INT, 0, 0, TR1, iilog(TR1_WIS), 4},
	{MUT_MORONIC, 0, 0, TR1, iilog(TR1_INT), -4},
	{MUT_MORONIC, 0, 0, TR1, iilog(TR1_WIS), -4},
	{MUT_RESILIENT, 0, 0, TR1, iilog(TR1_CON), 4},
	{MUT_XTRA_FAT, 0, 0, TR1, iilog(TR1_CON), 2},
	{MUT_XTRA_FAT, 0, 0, TR1, iilog(TR1_SPEED), -2},
	{MUT_ALBINO, 0, 0, TR1, iilog(TR1_CON), -4},
	{MUT_FLESH_ROT, 0, 0, TR1, iilog(TR1_CON), -2},
	{MUT_FLESH_ROT, 0, 0, TR1, iilog(TR1_CHR), -1},
	{MUT_FLESH_ROT, 0, 0, TR3, iilog(TR3_REGEN), FALSE},
	{MUT_SILLY_VOI, 0, 0, TR1, iilog(TR1_CHR), -4},
	{MUT_BLANK_FAC, 0, 0, TR1, iilog(TR1_CHR), -1},
	{MUT_XTRA_EYES, 0, 0, TR1, iilog(TR1_SEARCH), 3},
	{MUT_MAGIC_RES, 0, 0, TR0, iilog(TR0_SAVE_SK), 0},
	{MUT_XTRA_NOIS, 0, 0, TR1, iilog(TR1_STEALTH), -3},
	{MUT_INFRAVIS, 0, 0, TR1, iilog(TR1_INFRA), 3},
	{MUT_XTRA_LEGS, 0, 0, TR1, iilog(TR1_SPEED), 3},
	{MUT_SHORT_LEG, 0, 0, TR1, iilog(TR1_SPEED), -3},
	{MUT_ELEC_TOUC, 0, 0, TR3, iilog(TR3_SH_ELEC), TRUE},
	{MUT_FIRE_BODY, 0, 0, TR3, iilog(TR3_SH_FIRE), TRUE},
	{MUT_FIRE_BODY, 0, 0, TR3, iilog(TR3_LITE), TRUE},
	{MUT_WART_SKIN, 0, 0, TR1, iilog(TR1_CHR), -2},
	{MUT_WART_SKIN, 0, 0, TR0, iilog(TR0_AC), 5},
	{MUT_WART_SKIN, 0, 0, TR0, iilog(TR0_DIS_TO_A), 5},
	{MUT_SCALES, 0, 0, TR1, iilog(TR1_CHR), -1},
	{MUT_SCALES, 0, 0, TR0, iilog(TR0_AC), 10},
	{MUT_SCALES, 0, 0, TR0, iilog(TR0_DIS_TO_A), 10},
	{MUT_IRON_SKIN, 0, 0, TR1, iilog(TR1_DEX), -1},
	{MUT_IRON_SKIN, 0, 0, TR0, iilog(TR0_AC), 25},
	{MUT_IRON_SKIN, 0, 0, TR0, iilog(TR0_DIS_TO_A), 25},
	{MUT_WINGS, 0, 0, TR3, iilog(TR3_FEATHER), TRUE},
	{MUT_FEARLESS, 0, 0, TR2, iilog(TR2_RES_FEAR), TRUE},
	{MUT_REGEN, 0, 0, TR3, iilog(TR3_REGEN), TRUE},
	{MUT_ESP, 0, 0, TR3, iilog(TR3_TELEPATHY), TRUE},
	{MUT_LIMBER, 0, 0, TR1, iilog(TR1_DEX), 3},
	{MUT_ARTHRITIS, 0, 0, TR1, iilog(TR1_DEX), -3},
	{MUT_MOTION, 0, 0, TR1, iilog(TR1_STEALTH), 1},
	{MUT_MOTION, 0, 0, TR2, iilog(TR2_FREE_ACT), TRUE},
	{MUT_SUS_STATS, 0, 0, TR2, iilog(TR2_SUST_CON), TRUE},
	{MUT_SUS_STATS, SKILL_RACIAL, 20, TR2, iilog(TR2_SUST_STR), TRUE},
	{MUT_SUS_STATS, SKILL_RACIAL, 40, TR2, iilog(TR2_SUST_DEX), TRUE},
	{MUT_SUS_STATS, SKILL_RACIAL, 60, TR2, iilog(TR2_SUST_WIS), TRUE},
	{MUT_SUS_STATS, SKILL_RACIAL, 80, TR2, iilog(TR2_SUST_INT), TRUE},
	{MUT_SUS_STATS, SKILL_RACIAL, 100, TR2, iilog(TR2_SUST_CHR), TRUE},
	{MUT_ILL_NORM, 0, 0, TR0, iilog(TR0_ILL_NORM), 0},
};

/*
 * Hack - add in the permanent effects of a mutation which behaves in an unusual
 * way.
 * Return TRUE if this is
 */
static bool calc_bonuses_weird(s16b (*flags)[32], int flag, int val)
{
	switch (flag)
	{
		case iilog(TR0_SAVE_SK):
		{
			flags[TR0][iilog(TR0_SAVE)] +=
				(15 + skill_set[SKILL_RACIAL].value/10);
			return TRUE;
		}
		case iilog(TR0_ILL_NORM):
		{
			/* Cancel out the race/template bonuses and any other mutations. */
			flags[TR1][iilog(TR1_CHR)] = -p_ptr->stat_add[A_CHR];
			return TRUE;
		}
		case iilog(TR0_AC_SK):
		{
			flags[TR0][iilog(TR0_AC)] += 20 + skill_set[SKILL_RACIAL].value/10;
			flags[TR0][iilog(TR0_DIS_TO_A)] +=
				20 + skill_set[SKILL_RACIAL].value/10;
			return TRUE;
		}
		case iilog(TR0_SPEED_SK):
		{
			flags[TR1][iilog(TR1_SPEED)] -= skill_set[SKILL_MA].value/20;
			flags[TR1][iilog(TR1_SPEED)] += skill_set[SKILL_RACIAL].value/20;
			return TRUE;
		}
		case iilog(TR0_RES_ELDRITCH):
		{
			/* Hack - use a negative value to represent skill-based value. */
			if (val < 0) val = skill_set[SKILL_SAVE].value/2 + 24;

			/* Save the resistance. */
			flags[TR0][flag] = val;

			return TRUE;
		}
		/* Not a "weird" bonus. */
		default:
		{
			return FALSE;
		}
	}
}

/*
 * Give a sufficiently skilled player the permanent effects of a race/mutation.
 */
static void calc_bonuses_muta_aux(s16b (*flags)[32], race_bonus *ptr)
{
	s16b *this = &flags[ptr->set][ptr->flag];

	/* Not skilled enough yet. */
	if (ptr->min > skill_set[ptr->skill].value) return;

	/* Weird stuff. */
	if (ptr->set == TR0 && calc_bonuses_weird(flags, ptr->flag, ptr->value))
	{
	}

	/* Numerical modifier. */
	else if (is_pval_flag(ptr->set, ptr->flag))
	{
		*this += ptr->value;
	}
	/* Boolean flag. */
	else
	{
		*this = ptr->value;
	}
}

/*
 * Add in the permanent effects of various mutations.
 */
static void calc_bonuses_muta(s16b (*flags)[32])
{
	race_bonus *ptr;
	FOR_ALL_IN(muta_bonuses, ptr)
	{
		/* Check for mutation. */
		if (p_has_mutation(ptr->type))
		{
			/* Check skills and add in the mutation. */
			calc_bonuses_muta_aux(flags, ptr);
		}
	}
}

/*
 * Add the intrinsic flags the player has by race to the flags table.
 */
static void calc_bonuses_race(s16b (*flags)[32])
{
	race_bonus *ptr;
	int i, j;

	/* Binary flags which are always present are simple. */
	for (i = 0; i < 4; i++)
	{
		for (j = 0; j < 32; j++)
		{
			if (rp_ptr->flags[i] & (1L << j)) flags[i][j] = TRUE;
		}
	}

	/* No other bonuses present. */
	if (!rp_ptr->bonuses) return;

	for (ptr = rp_ptr->bonus; ptr < rp_ptr->bonus+rp_ptr->bonuses; ptr++)
	{
		/* Check skills and add in the bonus. */
		calc_bonuses_muta_aux(flags, ptr);
	}
}

/*
 * Add both above sets of flags to the flag table.
 */
static void get_bonus_flags(s16b (*flags)[32])
{
	calc_bonuses_race(flags);
	calc_bonuses_muta(flags);
}

/*
 * Check a single flag. Doesn't currently check objects.
 */
bool PURE player_has_flag(int set, u32b flag)
{
	s16b flags[4][32];

	get_bonus_flags(flags);

	return flags[set][iilog(flag)] != 0;
}

/*
 * Obtain the "flags" for the player as if he was an item
 */
void player_flags(u32b *f1, u32b *f2, u32b *f3)
{
	int f,s;
	s16b flags[4][32];
	u32b *fn[3];

	fn[0] = f1;
	fn[1] = f2;
	fn[2] = f3;

	/* Clear */
	(*f1) = (*f2) = (*f3) = 0L;
	WIPE(flags, flags);

	/* Acquire the flags. */
	get_bonus_flags(flags);

	/* Copy them to the variables. */
	for (s = 1; s < 4; s++)
	{
		for (f = 0; f < 32; f++)
		{
			if (flags[s][f]) *fn[s-1] |= 1L << f;
		}
	}
}

/*
 * Give the player a bonus for various abilities in the flags table.
 */
static void calc_bonuses_add(s16b (*flags)[32])
{
	assert(flags+4); /* Caller */

	/* Affect stats */
	p_ptr->stat_add[A_STR] += flags[1][iilog(TR1_STR)];
	p_ptr->stat_add[A_INT] += flags[1][iilog(TR1_INT)];
	p_ptr->stat_add[A_WIS] += flags[1][iilog(TR1_WIS)];
	p_ptr->stat_add[A_DEX] += flags[1][iilog(TR1_DEX)];
	p_ptr->stat_add[A_CON] += flags[1][iilog(TR1_CON)];
	p_ptr->stat_add[A_CHR] += flags[1][iilog(TR1_CHR)];

	/* Affect stealth */
	p_ptr->skill_stl += flags[1][iilog(TR1_STEALTH)];

	/* Affect searching ability (factor of five) */
	p_ptr->skill_srh += flags[1][iilog(TR1_SEARCH)] * 5;

	/* Affect searching frequency (factor of five) */
	p_ptr->skill_fos += flags[1][iilog(TR1_SEARCH)] * 5;

	/* Affect infravision */
	p_ptr->see_infra += flags[1][iilog(TR1_INFRA)];

	/* Affect digging (factor of 20) */
	p_ptr->skill_dig += flags[1][iilog(TR1_TUNNEL)] * 20;

	/* Affect saving throw. */
	p_ptr->skill_sav += flags[0][iilog(TR0_SAVE)];

	/* Affect speed */
	p_ptr->pspeed += flags[1][iilog(TR1_SPEED)];

	/* Affect blows */
	p_ptr->num_blow += flags[1][iilog(TR1_BLOWS)] * 60;

	/* Affect sanity blast saving throw. */
	p_ptr->resist_eldritch = flags[0][iilog(TR0_RES_ELDRITCH)];

	/* Hack -- cause earthquakes */
	if (flags[1][iilog(TR1_IMPACT)]) p_ptr->impact = TRUE;

	/* Boost shots */
	if (flags[3][iilog(TR3_XTRA_SHOTS)]) p_ptr->num_fire+=60;

	/* Various flags */
	p_ptr->no_cut = (!!flags[0][iilog(TR0_NO_CUT)]);
	p_ptr->no_stun = (!!flags[0][iilog(TR0_NO_STUN)]);
	p_ptr->heal_nether = (!!flags[0][iilog(TR0_HEAL_NETHER)]);
	p_ptr->immune_dark = (!!flags[0][iilog(TR0_IM_DARK)]);
	p_ptr->hurt_light = (!!flags[0][iilog(TR0_HURT_LIGHT)]);
	p_ptr->weak_wraith = (!!flags[0][iilog(TR0_WEAK_WRAITH)]);
	if (flags[3][iilog(TR3_AGGRAVATE)]) p_ptr->aggravate = TRUE;
	if (flags[3][iilog(TR3_TELEPORT)]) p_ptr->teleport = TRUE;
	if (flags[3][iilog(TR3_DRAIN_EXP)]) p_ptr->exp_drain = TRUE;
	if (flags[3][iilog(TR3_BLESSED)]) p_ptr->bless_blade = TRUE;
	if (flags[3][iilog(TR3_XTRA_MIGHT)]) p_ptr->xtra_might = TRUE;
	if (flags[3][iilog(TR3_SLOW_DIGEST)]) p_ptr->slow_digest = TRUE;
	if (flags[3][iilog(TR3_REGEN)]) p_ptr->regenerate = TRUE;
	if (flags[3][iilog(TR3_TELEPATHY)]) p_ptr->telepathy = TRUE;
	if (flags[3][iilog(TR3_LITE)]) p_ptr->lite = TRUE;
	if (flags[3][iilog(TR3_SEE_INVIS)]) p_ptr->see_inv = TRUE;
	if (flags[3][iilog(TR3_FEATHER)]) p_ptr->ffall = TRUE;
	if (flags[2][iilog(TR2_FREE_ACT)]) p_ptr->free_act = TRUE;
	if (flags[2][iilog(TR2_HOLD_LIFE)]) p_ptr->hold_life = TRUE;
	if (flags[3][iilog(TR3_WRAITH)])
		p_ptr->wraith_form = MAX(p_ptr->wraith_form, 20);

	/* Immunity flags */
	if (flags[2][iilog(TR2_IM_FIRE)]) p_ptr->immune_fire = TRUE;
	if (flags[2][iilog(TR2_IM_ACID)]) p_ptr->immune_acid = TRUE;
	if (flags[2][iilog(TR2_IM_COLD)]) p_ptr->immune_cold = TRUE;
	if (flags[2][iilog(TR2_IM_ELEC)]) p_ptr->immune_elec = TRUE;

	/* Resistance flags */
	if (flags[2][iilog(TR2_RES_ACID)]) p_ptr->resist_acid = TRUE;
	if (flags[2][iilog(TR2_RES_ELEC)]) p_ptr->resist_elec = TRUE;
	if (flags[2][iilog(TR2_RES_FIRE)]) p_ptr->resist_fire = TRUE;
	if (flags[2][iilog(TR2_RES_COLD)]) p_ptr->resist_cold = TRUE;
	if (flags[2][iilog(TR2_RES_POIS)]) p_ptr->resist_pois = TRUE;
	if (flags[2][iilog(TR2_RES_FEAR)]) p_ptr->resist_fear = TRUE;
	if (flags[2][iilog(TR2_RES_CONF)]) p_ptr->resist_conf = TRUE;
	if (flags[2][iilog(TR2_RES_SOUND)]) p_ptr->resist_sound = TRUE;
	if (flags[2][iilog(TR2_RES_LITE)]) p_ptr->resist_lite = TRUE;
	if (flags[2][iilog(TR2_RES_DARK)]) p_ptr->resist_dark = TRUE;
	if (flags[2][iilog(TR2_RES_CHAOS)]) p_ptr->resist_chaos = TRUE;
	if (flags[2][iilog(TR2_RES_DISEN)]) p_ptr->resist_disen = TRUE;
	if (flags[2][iilog(TR2_RES_SHARDS)]) p_ptr->resist_shard = TRUE;
	if (flags[2][iilog(TR2_RES_NEXUS)]) p_ptr->resist_nexus = TRUE;
	if (flags[2][iilog(TR2_RES_BLIND)]) p_ptr->resist_blind = TRUE;
	if (flags[2][iilog(TR2_RES_NETHER)]) p_ptr->resist_neth = TRUE;

	if (flags[2][iilog(TR2_REFLECT)]) p_ptr->reflect = TRUE;
	if (flags[3][iilog(TR3_SH_FIRE)]) p_ptr->sh_fire = p_ptr->lite = TRUE;
	if (flags[3][iilog(TR3_SH_ELEC)]) p_ptr->sh_elec = TRUE;
	if (flags[3][iilog(TR3_NO_MAGIC)]) p_ptr->anti_magic = TRUE;
	if (flags[3][iilog(TR3_NO_TELE)]) p_ptr->anti_tele = TRUE;

	/* Sustain flags */
	if (flags[2][iilog(TR2_SUST_STR)]) p_ptr->sustain[A_STR] = TRUE;
	if (flags[2][iilog(TR2_SUST_INT)]) p_ptr->sustain[A_INT] = TRUE;
	if (flags[2][iilog(TR2_SUST_WIS)]) p_ptr->sustain[A_WIS] = TRUE;
	if (flags[2][iilog(TR2_SUST_DEX)]) p_ptr->sustain[A_DEX] = TRUE;
	if (flags[2][iilog(TR2_SUST_CON)]) p_ptr->sustain[A_CON] = TRUE;
	if (flags[2][iilog(TR2_SUST_CHR)]) p_ptr->sustain[A_CHR] = TRUE;

	/* Add the armour class */
	p_ptr->ac += flags[0][iilog(TR0_AC)];

	/* The base armor class is always known */
	p_ptr->dis_ac += flags[0][iilog(TR0_DIS_AC)];

	/* Apply the mental bonuses to armor class, if known */
	p_ptr->dis_to_a += flags[0][iilog(TR0_DIS_TO_A)];

	/* Apply the bonuses to hit/damage */
	p_ptr->to_h += flags[0][iilog(TR0_TO_H)];
	p_ptr->to_d += flags[0][iilog(TR0_TO_D)];

	/* Apply the mental bonuses tp hit/damage, if known */
	p_ptr->dis_to_h += flags[0][iilog(TR0_DIS_TO_H)];
	p_ptr->dis_to_d += flags[0][iilog(TR0_DIS_TO_D)];
}


/*
 * Calculate the players current "state", taking into account
 * not only race intrinsics, but also objects being worn
 * and temporary spell effects.
 *
 * See also calc_mana() and calc_hitpoints().
 *
 * Take note of the new "speed code", in particular, a very strong
 * player will start slowing down as soon as he reaches 150 pounds,
 * but not until he reaches 450 pounds will he be half as fast as
 * a normal kobold.  This both hurts and helps the player, hurts
 * because in the old days a player could just avoid 300 pounds,
 * and helps because now carrying 300 pounds is not very painful.
 *
 * The "weapon" and "bow" do *not* add to the bonuses to hit or to
 * damage, since that would affect non-combat things.  These values
 * are actually added in later, at the appropriate place.
 *
 * This function induces various "status" messages.
 */
static void calc_bonuses(void)
{
	int i, j, hold;

	int old_speed;

	int old_telepathy;
	int old_see_inv;

	int old_dis_ac;
	int old_dis_to_a;

	const bool old_heavy_wield = p_ptr->heavy_wield;
	const bool old_heavy_shoot = p_ptr->heavy_shoot;

	object_type *o_ptr;

	const bool old_ma_cumber_armour = p_ptr->ma_cumber_armour;

	s16b flags[4][32];


	/* Save the old speed */
	old_speed = p_ptr->pspeed;

	/* Save the old vision stuff */
	old_telepathy = p_ptr->telepathy;
	old_see_inv = p_ptr->see_inv;

	/* Save the old armor class */
	old_dis_ac = p_ptr->dis_ac;
	old_dis_to_a = p_ptr->dis_to_a;


	/* Get weapon type */
	o_ptr = &inventory[INVEN_WIELD];

	/* Can we assign a weapon skill? */
	p_ptr->wield_skill = wield_skill(&inventory[INVEN_WIELD]);
	if (p_ptr->wield_skill >= MAX_SKILLS)
	{
		msg_print("Unknown weapon type wielded - "
			"defaulting to close combat skill.");
		p_ptr->wield_skill = SKILL_CLOSE;
	}


	/* Clear the stat modifiers */
	for (i = 0; i < 6; i++) p_ptr->stat_add[i] = 0;


	/* Clear the Displayed/Real armor class */
	p_ptr->dis_ac = p_ptr->ac = 0;

	/* Clear the Displayed/Real Bonuses */
	p_ptr->dis_to_h = p_ptr->to_h = 0;
	p_ptr->dis_to_d = p_ptr->to_d = 0;
	p_ptr->dis_to_a = 0;


	/* Clear all the flags */
	p_ptr->aggravate = FALSE;
	p_ptr->teleport = FALSE;
	p_ptr->exp_drain = FALSE;
	p_ptr->bless_blade = FALSE;
	p_ptr->xtra_might = FALSE;
	p_ptr->impact = FALSE;
	p_ptr->see_inv = FALSE;
	p_ptr->free_act = FALSE;
	p_ptr->slow_digest = FALSE;
	p_ptr->regenerate = FALSE;
	p_ptr->ffall = FALSE;
	p_ptr->hold_life = FALSE;
	p_ptr->telepathy = FALSE;
	p_ptr->lite = FALSE;
	C_WIPE(p_ptr->sustain, A_MAX, bool);
	p_ptr->resist_acid = FALSE;
	p_ptr->resist_elec = FALSE;
	p_ptr->resist_fire = FALSE;
	p_ptr->resist_cold = FALSE;
	p_ptr->resist_pois = FALSE;
	p_ptr->resist_conf = FALSE;
	p_ptr->resist_sound = FALSE;
	p_ptr->resist_lite = FALSE;
	p_ptr->resist_dark = FALSE;
	p_ptr->resist_chaos = FALSE;
	p_ptr->resist_disen = FALSE;
	p_ptr->resist_shard = FALSE;
	p_ptr->resist_nexus = FALSE;
	p_ptr->resist_blind = FALSE;
	p_ptr->resist_neth = FALSE;
	p_ptr->resist_fear = FALSE;
	p_ptr->reflect = FALSE;
	p_ptr->sh_fire = FALSE;
	p_ptr->sh_elec = FALSE;
	p_ptr->anti_magic = FALSE;
	p_ptr->anti_tele = FALSE;

	p_ptr->immune_acid = FALSE;
	p_ptr->immune_elec = FALSE;
	p_ptr->immune_fire = FALSE;
	p_ptr->immune_cold = FALSE;

	/* Start with "normal" speed */
	p_ptr->pspeed = 110;

	/* No blows have been counted yet. */
	p_ptr->num_blow = 0;

	/* No shots have been counted yet. */
	p_ptr->num_fire = 0;

	/* Reset the "ammo" tval */
	p_ptr->tval_ammo = 0;



	/* Base infravision (purely racial) */
	p_ptr->see_infra = rp_ptr->infra;


	/* Base skill -- disarming */
	p_ptr->skill_dis = skill_set[SKILL_DISARM].value;
	/* Base skill -- magic devices */
	p_ptr->skill_dev = skill_set[SKILL_DEVICE].value;
	/* Base skill -- saving throw */
	p_ptr->skill_sav = skill_set[SKILL_SAVE].value;
	/* Base skill -- stealth */
	p_ptr->skill_stl = skill_set[SKILL_STEALTH].value/10;
	/* Base skill -- searching ability */
	p_ptr->skill_srh = skill_set[SKILL_SEARCH].value/2;
	/* Base skill -- searching frequency */
	p_ptr->skill_fos = skill_set[SKILL_PERCEPTION].value/2;
	/* Base skill -- combat (normal) */
	p_ptr->skill_thn = skill_set[p_ptr->wield_skill].value*3;
	/* Base skill -- combat (shooting) */
	p_ptr->skill_thb = skill_set[SKILL_MISSILE].value*3;
	/* Base skill -- combat (throwing) */
	p_ptr->skill_tht = skill_set[SKILL_MISSILE].value*3;
	/* Base skill -- digging */
	p_ptr->skill_dig = 0;

	/* Extract the race-based flags. */
	WIPE(flags, flags);
	calc_bonuses_race(flags);

	/* Hack -- apply racial/template stat maxes */
	if (maximise_mode)
	{
		/* Apply the racial modifiers */
		for (i = 0; i < 6; i++)
		{
			/* Modify the stats for "race" */
			p_ptr->stat_add[i] += (rp_ptr->r_adj[i] + cp_ptr->c_adj[i]);
		}
	}

	/* Mystic get extra ac for armour _not worn_ */
	p_ptr->ma_cumber_armour = ma_heavy_armor();

	/* Calculate any martial arts AC now, so the effect can be counted below. */
	if (!p_ptr->ma_cumber_armour || !old_ma_cumber_armour)
	{
		calc_ma_armour();
		p_ptr->update &= ~(PU_MA_ARMOUR);
	}

	/* Extract the mutation-based flags. */
	calc_bonuses_muta(flags);

	/* Scan the usable inventory */
	for (o_ptr = inventory+INVEN_WIELD; o_ptr < inventory+INVEN_TOTAL; o_ptr++)
	{
		/* Skip non-objects */
		if (!o_ptr->k_idx && !o_ptr->to_a) continue;

		/* Obtain the flags. */
		calc_bonuses_object(flags, o_ptr);
	}

	/* Copy the flags to p_ptr. */
	calc_bonuses_add(flags);


	/* Calculate stats */
	for (i = 0; i < 6; i++)
	{
		int top, use, ind;


		/* Extract the new "stat_use" value for the stat */
		top = modify_stat_value(p_ptr->stat_max[i], p_ptr->stat_add[i]);

		/* Notice changes */
		if (p_ptr->stat_top[i] != top)
		{
			/* Save the new value */
			p_ptr->stat_top[i] = top;

			/* Redisplay the stats later */
			p_ptr->redraw |= (PR_STATS);

			/* Window stuff */
			p_ptr->window |= (PW_PLAYER);
		}


		/* Extract the new "stat_use" value for the stat */
		use = modify_stat_value(p_ptr->stat_cur[i], p_ptr->stat_add[i]);

		if ((i == A_CHR) && (p_has_mutation(MUT_ILL_NORM)))
		{
			/* 10 to 18/90 charisma, guaranteed, based on level */
			if (use < 8 + 2 * (skill_set[SKILL_RACIAL].value/2))
			{
				use = 8 + 2 * (skill_set[SKILL_RACIAL].value/2);
			}
		}

		/* Notice changes */
		if (p_ptr->stat_use[i] != use)
		{
			/* Save the new value */
			p_ptr->stat_use[i] = use;

			/* Redisplay the stats later */
			p_ptr->redraw |= (PR_STATS);

			/* Window stuff */
			p_ptr->window |= (PW_PLAYER);
		}


		/* Calculate the index for the stat in use (see defines.h) */
		ind = ind_stat(use);

		/* Notice changes */
		if (p_ptr->stat_ind[i] != ind)
		{
			/* Save the new index */
			p_ptr->stat_ind[i] = ind;

			/* Change in CON affects Hitpoints */
			if (i == A_CON)
			{
				p_ptr->update |= (PU_HP);
			}

			/* Change in INT may affect Mana/Spells */
			else if (i == A_INT)
			{
				p_ptr->update |= (PU_MANA | PU_SPELLS);
			}

			/* Window stuff */
			p_ptr->window |= (PW_PLAYER);
		}
	}


	/* Apply temporary "stun" */
	if (p_ptr->stun > 50)
	{
		p_ptr->to_h -= 20;
		p_ptr->dis_to_h -= 20;
		p_ptr->to_d -= 20;
		p_ptr->dis_to_d -= 20;
	}
	else if (p_ptr->stun)
	{
		p_ptr->to_h -= 5;
		p_ptr->dis_to_h -= 5;
		p_ptr->to_d -= 5;
		p_ptr->dis_to_d -= 5;
	}


	/* Invulnerability */
	if (p_ptr->invuln)
	{
		p_ptr->ac += 100;
		p_ptr->dis_to_a += 100;
	}

	/* wraith_form */
	if (p_ptr->wraith_form)
	{
		p_ptr->ac += 100;
		p_ptr->dis_to_a += 100;
		p_ptr->reflect = TRUE;
	}

	/* Temporary blessing */
	if (p_ptr->blessed)
	{
		p_ptr->ac += 5;
		p_ptr->dis_to_a += 5;
		p_ptr->to_h += 10;
		p_ptr->dis_to_h += 10;
	}

	/* Temprory shield */
	if (p_ptr->shield)
	{
		p_ptr->ac += 50;
		p_ptr->dis_to_a += 50;
	}

	/* Temporary "Hero" */
	if (p_ptr->hero)
	{
		p_ptr->to_h += 12;
		p_ptr->dis_to_h += 12;
	}

	/* Temporary "Beserk" */
	if (p_ptr->shero)
	{
		p_ptr->to_h += 24;
		p_ptr->dis_to_h += 24;
		p_ptr->ac -= 10;
		p_ptr->dis_to_a -= 10;
	}

	/* Temporary "fast" */
	if (p_ptr->fast)
	{
		p_ptr->pspeed += 10;
	}

	/* Temporary "slow" */
	if (p_ptr->slow)
	{
		p_ptr->pspeed -= 10;
	}

	/* Most martial artists gain speed with skill. */
	if (!p_ptr->ma_cumber_armour)
	{
		p_ptr->pspeed += (skill_set[SKILL_MA].value / 20);
	}

	if (p_ptr->tim_esp)
	{
		p_ptr->telepathy = TRUE;
	}

	/* Temporary see invisible */
	if (p_ptr->tim_invis)
	{
		p_ptr->see_inv = TRUE;
	}

	/* Temporary infravision boost */
	if (p_ptr->tim_infra)
	{
		p_ptr->see_infra++;
	}

	/* Negative infravision does not make sense. */
	if (p_ptr->see_infra < 0) p_ptr->see_infra = 0;

	/* Hack -- Res Chaos -> Res Conf */
	if (p_ptr->resist_chaos)
	{
		p_ptr->resist_conf = TRUE;
	}

	/* Hack -- Hero/Shero -> Res fear */
	if (p_ptr->hero || p_ptr->shero)
	{
		p_ptr->resist_fear = TRUE;
	}


	/* Hack -- Telepathy Change */
	if (p_ptr->telepathy != old_telepathy)
	{
		p_ptr->update |= (PU_MONSTERS);
	}

	/* Hack -- See Invis Change */
	if (p_ptr->see_inv != old_see_inv)
	{
		p_ptr->update |= (PU_MONSTERS);
	}


	/* Extract the current weight (in tenth pounds) */
	j = total_weight;

	/* Extract the "weight limit" (in tenth pounds) */
	i = weight_limit();

	/* XXX XXX XXX Apply "encumbrance" from weight */
	if (j > i/2) p_ptr->pspeed -= ((j - (i/2)) / (i / 10));

	/* Bloating slows the player down (a little) */
	if (p_ptr->food >= PY_FOOD_MAX) p_ptr->pspeed -= 10;

	/* Sneaking slows the player down */
	if (p_ptr->sneaking)
	{
		p_ptr->pspeed -= 15;
	}
	else /* But not sneaking cripples stealth skill */
	{
		p_ptr->skill_stl=p_ptr->skill_stl/2;
	}

	/* Display the speed (if needed) */
	if (p_ptr->pspeed != old_speed) p_ptr->redraw |= (PR_SPEED);


	/* Actual Modifier Bonuses (Un-inflate stat bonuses) */
	p_ptr->ac += ((int)(adj_dex_ta[p_ptr->stat_ind[A_DEX]]) - 128);
	p_ptr->to_d += ((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128);
	p_ptr->to_h += ((int)(adj_dex_th[p_ptr->stat_ind[A_DEX]]) - 128);
	p_ptr->to_h += ((int)(adj_str_th[p_ptr->stat_ind[A_STR]]) - 128);

	/* Displayed Modifier Bonuses (Un-inflate stat bonuses) */
	p_ptr->dis_to_a += ((int)(adj_dex_ta[p_ptr->stat_ind[A_DEX]]) - 128);
	p_ptr->dis_to_d += ((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128);
	p_ptr->dis_to_h += ((int)(adj_dex_th[p_ptr->stat_ind[A_DEX]]) - 128);
	p_ptr->dis_to_h += ((int)(adj_str_th[p_ptr->stat_ind[A_STR]]) - 128);


	/* Redraw armor (if needed) */
	if ((p_ptr->dis_ac != old_dis_ac) || (p_ptr->dis_to_a != old_dis_to_a))
	{
		/* Redraw */
		p_ptr->redraw |= (PR_ARMOR);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);
	}


	/* Obtain the "hold" value */
	hold = adj_str_hold[p_ptr->stat_ind[A_STR]];


	/* Examine the "current bow" */
	o_ptr = &inventory[INVEN_BOW];


	/* Assume not heavy */
	p_ptr->heavy_shoot = FALSE;

	/* It is hard to carholdry a heavy bow */
	if (hold < o_ptr->weight / 10)
	{
		/* Hard to wield a heavy bow */
		p_ptr->to_h += 2 * (hold - o_ptr->weight / 10);
		p_ptr->dis_to_h += 2 * (hold - o_ptr->weight / 10);

		/* Heavy Bow */
		p_ptr->heavy_shoot = TRUE;

		/* Too heavy to use quickly. */
		p_ptr->num_fire = 60;
	}

	/* Compute "extra shots" if needed */
	else if (o_ptr->k_idx)
	{
		/* Take note of required "tval" for missiles (should this rely
		 * on !p_ptr->heavy_shoot?). */
		p_ptr->tval_ammo = ammunition_type(o_ptr);

		/* Add additional shots for player skill, depending on weapon. */
		switch (o_ptr->k_idx)
		{
			case OBJ_SLING:
			{
				if (skill_set[SKILL_MISSILE].value==100)
					p_ptr->num_fire += 120;
				else if (skill_set[SKILL_MISSILE].value>22)
					p_ptr->num_fire += (skill_set[SKILL_MISSILE].value*4/3-30);
				break;
			}

			case OBJ_SHORT_BOW:
			case OBJ_LONG_BOW:
			{
				if (skill_set[SKILL_MISSILE].value==100)
					p_ptr->num_fire += 180;
				else if (skill_set[SKILL_MISSILE].value>15)
					p_ptr->num_fire += (skill_set[SKILL_MISSILE].value*2-30);
				break;
			}

			case OBJ_LIGHT_CROSSBOW:
			case OBJ_HEAVY_CROSSBOW:
			{
				/* At 100 skill, a crossbow gives 1 1/6 extra shots anyway. */
				if (skill_set[SKILL_MISSILE].value>30)
					p_ptr->num_fire += (skill_set[SKILL_MISSILE].value-30);
				break;
			}
		}

		/* Require at least one shot */
		if (p_ptr->num_fire < 60) p_ptr->num_fire = 60;
	}



	/* Examine the "main weapon" */
	o_ptr = &inventory[INVEN_WIELD];


	/* Assume not heavy */
	p_ptr->heavy_wield = FALSE;

	/* It is hard to hold a heavy weapon */
	if (hold < o_ptr->weight / 10)
	{
		/* Hard to wield a heavy weapon */
		p_ptr->to_h += 2 * (hold - o_ptr->weight / 10);
		p_ptr->dis_to_h += 2 * (hold - o_ptr->weight / 10);

		/* Heavy weapon */
		p_ptr->heavy_wield = TRUE;

		/* Too heavy to use quickly. */
		p_ptr->num_blow = 60;
	}

	/* Normal weapons */
	else if (o_ptr->k_idx)
	{
		/* Enforce a minimum "weight" (tenth pounds) */
		int div = MAX(35, o_ptr->weight);

		/* Index by strength vs weight (bounded). */
		int strx = MIN(11, (adj_str_blow[p_ptr->stat_ind[A_STR]] * 3 / div));

		/* Index by dexterity */
		int dexx = adj_dex_blow[p_ptr->stat_ind[A_DEX]];

		/* Use the blows table */
		int blow = blows_table[strx][dexx] * 2;

		/* Add in the "bonus blows" */
		p_ptr->num_blow += blow;

		/* Level bonus for warriors (0-3) */
		if (skill_set[p_ptr->wield_skill].value==100)
			p_ptr->num_blow += 180;
		else if (skill_set[p_ptr->wield_skill].value>15)
			p_ptr->num_blow += (skill_set[p_ptr->wield_skill].value*2-30);

		/* Require at least one blow */
		if (p_ptr->num_blow < 60) p_ptr->num_blow = 60;


		/* Boost digging skill by weapon weight */
		p_ptr->skill_dig += (o_ptr->weight / 10);
	}


	/* Different calculation for mystics with empty hands */
	else
	{
		int ma_blow = 0;

		if (skill_set[SKILL_MA].value == 100)
			ma_blow = 420;
		else if (skill_set[SKILL_MA].value > 59)
			ma_blow = skill_set[SKILL_MA].value * 6 - 210;
		else if (skill_set[SKILL_MA].value > 10)
			ma_blow = skill_set[SKILL_MA].value * 3 - 30;

			if (p_ptr->ma_cumber_armour)
				ma_blow /= 2;

			p_ptr->num_blow += 60 + ma_blow;

			if (!p_ptr->ma_cumber_armour)
			{
				p_ptr->to_h += (skill_set[SKILL_MA].value / 6);
				p_ptr->to_d += (skill_set[SKILL_MA].value / 6);

				p_ptr->dis_to_h += (skill_set[SKILL_MA].value / 6);
				p_ptr->dis_to_d += (skill_set[SKILL_MA].value / 6);
			}
	}

	p_ptr->to_h += (skill_set[p_ptr->wield_skill].value/10);
	p_ptr->to_d += (skill_set[p_ptr->wield_skill].value/10);

	p_ptr->dis_to_h += (skill_set[p_ptr->wield_skill].value/10);
	p_ptr->dis_to_d += (skill_set[p_ptr->wield_skill].value/10);


	/* Affect Skill -- stealth (bonus one) */
	p_ptr->skill_stl += 1;

	/* Affect Skill -- disarming (DEX and INT) */
	p_ptr->skill_dis += adj_dex_dis[p_ptr->stat_ind[A_DEX]];
	p_ptr->skill_dis += adj_int_dis[p_ptr->stat_ind[A_INT]];

	/* Affect Skill -- magic devices (INT) */
	p_ptr->skill_dev += adj_int_dev[p_ptr->stat_ind[A_INT]];

	/* Affect Skill -- saving throw (WIS) */
	p_ptr->skill_sav += adj_wis_sav[p_ptr->stat_ind[A_WIS]];

	/* Affect Skill -- digging (STR) */
	p_ptr->skill_dig += adj_str_dig[p_ptr->stat_ind[A_STR]];

	/* Limit Skill -- stealth from 0 to 30 */
	if (p_ptr->skill_stl > 30) p_ptr->skill_stl = 30;
	if (p_ptr->skill_stl < 0) p_ptr->skill_stl = 0;

	/* Limit Skill -- digging from 1 up */
	if (p_ptr->skill_dig < 1) p_ptr->skill_dig = 1;

	/* Limit Skill -- number of attacks from 60 to 600 per 60 rounds*/
	if (p_ptr->num_blow < 60) p_ptr->num_blow = 60;
	if (p_ptr->num_blow > 600) p_ptr->num_blow = 600;

	/* Limit Skill -- number of shots from 60 to 600 per 60 rounds*/
	if (p_ptr->num_fire < 60) p_ptr->num_fire = 60;
	if (p_ptr->num_fire > 600) p_ptr->num_fire = 600;

	if ((p_ptr->anti_magic) && (p_ptr->skill_sav < 95))
		p_ptr->skill_sav = 95;

	/* Hack -- handle "xtra" mode */
	if (character_xtra) return;

	/* Take note when "heavy bow" changes */
	if (old_heavy_shoot != p_ptr->heavy_shoot)
	{
		/* Message */
		if (p_ptr->heavy_shoot)
		{
			msg_print("You have trouble wielding such a heavy bow.");
		}
		else if (inventory[INVEN_BOW].k_idx)
		{
			msg_print("You have no trouble wielding your bow.");
		}
		else
		{
			msg_print("You feel relieved to put down your heavy bow.");
		}
	}


	/* Take note when "heavy weapon" changes */
	if (old_heavy_wield != p_ptr->heavy_wield)
	{
		/* Message */
		if (p_ptr->heavy_wield)
		{
			msg_print("You have trouble wielding such a heavy weapon.");
		}
		else if (inventory[INVEN_WIELD].k_idx)
		{
			msg_print("You have no trouble wielding your weapon.");
		}
		else
		{
			msg_print("You feel relieved to put down your heavy weapon.");
		}
	}

	if (old_ma_cumber_armour != p_ptr->ma_cumber_armour)
	{
		/* Message */
		if (p_ptr->ma_cumber_armour)
			msg_print("The weight of your armor disrupts your balance.");
		else
			msg_print("You regain your balance.");
	}
}



/*
 * Handle "p_ptr->notice"
 */
void notice_stuff(void)
{
	/* Notice stuff */
	if (!p_ptr->notice) return;

	/* Squelch floor things. */
	if (p_ptr->notice & PN_FSQUELCH)
	{
		p_ptr->notice &= ~(PN_FSQUELCH);
		squelch_grid();
	}

	/* Squelch inventory things. */
	if (p_ptr->notice & PN_ISQUELCH)
	{
		p_ptr->notice &= ~(PN_ISQUELCH);
		squelch_inventory();
	}

	/* Combine the pack */
	if (p_ptr->notice & (PN_COMBINE))
	{
		p_ptr->notice &= ~(PN_COMBINE);
		combine_pack();
	}

	/* Reorder the pack */
	if (p_ptr->notice & (PN_REORDER))
	{
		p_ptr->notice &= ~(PN_REORDER);
		reorder_pack();
	}
}


/*
 * Is cave[y][x] in a room?
 *
 * This actually looks for empty spaces connected to this square by right
 * angles.
 */
static bool is_room_func_p(int y, int x)
{
	uint i,adj;

	int xs[] = { 1, 1,-1,-1, 1,  1, 0,-1, 0, 1};
	int ys[] = { 1,-1,-1, 1, 1,  0, 1, 0,-1, 0};

	/* Hack - assume that ineligible non-floor squares are excluded
	 * elsewhere.
	 */
	if (!cave_floor_bold(y,x)) return TRUE;

	for (i = adj = 0; i < N_ELEMENTS(xs); i++)
	{
		if (!cave_floor_bold(y+ys[i], x+xs[i]))
		{
			adj = 0;
		}
		/* (1,1) and (1,0) being floor squares does not make a room. */
		else if (adj++ && i != 5)
		{
			return TRUE;
		}
	}
	return FALSE;
}

/*
 * Check that every room square is next to another room square, to prevent
 * corners from making it easy to abuse the AI.
 */
static bool is_isolated_room_p(int y, int x)
{
	/* An array which rotates around 0,0 anticlockwise. */
	int xs[] = { 1, 1, 0,-1,-1,-1, 0, 1};
	int ys[] = { 0, 1, 1, 1, 0,-1,-1, 0};
	uint i;

	/* Paranoia - avoid looking out of bounds. */
	if (!in_bounds(y, x)) return FALSE;

	for (i = 0; i < N_ELEMENTS(xs); i++)
	{
		if (cave[y+ys[i]][x+xs[i]].info & CAVE_ROOM) return FALSE;
	}
	return TRUE;
}

/*
 * Set CAVE_ROOM as appropriate using the above routines.
 */
static void calc_rooms(void)
{
	int y,x;
	for (y = 0; y < cur_hgt; y++)
	{
		for (x = 0; x < cur_wid; x++)
		{
			if (is_room_func_p(y,x))
			{
				cave[y][x].info |= (CAVE_ROOM);
			}
			else
			{
				cave[y][x].info &= ~(CAVE_ROOM);
			}
		}
	}
	for (y = 1; y < cur_hgt-1; y++)
	{
		for (x = 1; x < cur_wid-1; x++)
		{
			if (is_isolated_room_p(y,x))
			{
				cave[y][x].info &= ~(CAVE_ROOM);
			}
		}
	}
}

/*
 * Handle "p_ptr->update"
 */
void update_stuff(void)
{
	bool quiet;

	/* Update stuff */
	if (!p_ptr->update) return;

	/*
	 * If quiet is set, this carries out the changes but does not comment
	 * on them.
	 */
	if (p_ptr->update & (PU_QUIET))
	{
		p_ptr->update &= ~(PU_QUIET);
		no_msg_print = quiet = TRUE;
	}
	else
	{
		quiet = FALSE;
	}

	if (p_ptr->update & (PU_BONUS))
	{
		p_ptr->update &= ~(PU_BONUS);
		calc_bonuses();
	}

	if (p_ptr->update & (PU_MA_ARMOUR))
	{
		p_ptr->update &= ~(PU_MA_ARMOUR);
		calc_ma_armour();
	}

	if (p_ptr->update & (PU_TORCH))
	{
		p_ptr->update &= ~(PU_TORCH);
		calc_torch();
	}

	if (p_ptr->update & (PU_HP))
	{
		p_ptr->update &= ~(PU_HP);
		calc_hitpoints();
	}

	if (p_ptr->update & (PU_MANA))
	{
		p_ptr->update &= ~(PU_MANA);
		calc_mana();
	}

	if (p_ptr->update & (PU_SPELLS))
	{
		p_ptr->update &= ~(PU_SPELLS);
		calc_spells();
	}

	/* Hack - allow messages again. */
	no_msg_print = FALSE;

	/* Only making temporary changes, no screen updates */
	if (quiet) return;


	/* Character is not ready yet, no screen updates */
	if (!character_generated) return;


	/* Character is in "icky" mode, no screen updates */
	if (screen_is_icky()) return;


	if (p_ptr->update & (PU_UN_LITE))
	{
		p_ptr->update &= ~(PU_UN_LITE);
		forget_lite();
	}

	if (p_ptr->update & (PU_UN_VIEW))
	{
		p_ptr->update &= ~(PU_UN_VIEW);
		forget_view();
	}


	if (p_ptr->update & (PU_VIEW))
	{
		p_ptr->update &= ~(PU_VIEW);
		update_view();
	}

	if (p_ptr->update & (PU_LITE))
	{
		p_ptr->update &= ~(PU_LITE);
		update_lite();
	}

	if (p_ptr->update & (PU_ROOM))
	{
		p_ptr->update &= ~(PU_ROOM);
		calc_rooms();
	}

	if (p_ptr->update & (PU_FLOW))
	{
		p_ptr->update &= ~(PU_FLOW);
		update_flow();
	}


	if (p_ptr->update & (PU_DISTANCE))
	{
		p_ptr->update &= ~(PU_DISTANCE);
		p_ptr->update &= ~(PU_MONSTERS);
		update_monsters(TRUE);
	}

	if (p_ptr->update & (PU_MONSTERS))
	{
		p_ptr->update &= ~(PU_MONSTERS);
		update_monsters(FALSE);
	}
}


/*
 * Handle "p_ptr->redraw"
 */
void redraw_stuff(void)
{
	/* Redraw stuff */
	if (!p_ptr->redraw) return;


	/* Character is not ready yet, no screen updates */
	if (!character_generated) return;


	/* Character is in "icky" mode, no screen updates */
	if (screen_is_icky()) return;



	/* Hack -- clear the top line. */
	if (p_ptr->redraw & (PR_WIPE_0))
	{
		p_ptr->redraw &= ~(PR_WIPE_0);
		prt("", 0, 0);
	}

	/* Hack -- clear the rest of the screen */
	if (p_ptr->redraw & (PR_WIPE_1))
	{
		p_ptr->redraw &= ~(PR_WIPE_1);
		clear_from(1);
	}


	if (p_ptr->redraw & (PR_MAP))
	{
		prt_map(TRUE);
		p_ptr->redraw &= ~(PR_MAP | PR_PANEL);
	}

	if (p_ptr->redraw & (PR_PANEL))
	{
		prt_map(FALSE);
		p_ptr->redraw &= ~(PR_PANEL);
	}

	if (p_ptr->redraw & (PR_TIMERS))
	{
		p_ptr->redraw &= ~(PR_TIMERS);
		prt_timers();
	}

	if (p_ptr->redraw & (PR_EQUIPPY))
	{
		p_ptr->redraw &= ~(PR_EQUIPPY);
		prt_equippy(); /* To draw / delete equippy chars */
	}

	if (p_ptr->redraw & (PR_TIME))
	{
		prt_time();
		p_ptr->redraw &= ~(PR_TIME);
	}

	if (p_ptr->redraw & (PR_STATS))
	{
		p_ptr->redraw &= ~(PR_STATS);
		prt_stat(A_STR);
		prt_stat(A_INT);
		prt_stat(A_WIS);
		prt_stat(A_DEX);
		prt_stat(A_CON);
		prt_stat(A_CHR);
	}

	if (p_ptr->redraw & (PR_ARMOR))
	{
		p_ptr->redraw &= ~(PR_ARMOR);
		prt_ac();
	}

	if (p_ptr->redraw & (PR_ENERGY))
	{
		p_ptr->redraw &= ~(PR_ENERGY);
		prt_energy();
	}

	if (p_ptr->redraw & (PR_HP))
	{
		p_ptr->redraw &= ~(PR_HP);
		prt_hp();
	}

	if (p_ptr->redraw & (PR_MANA))
	{
		p_ptr->redraw &= ~(PR_MANA);
		prt_sp();
	}

	if (p_ptr->redraw & (PR_SPIRIT))
	{
		p_ptr->redraw &= ~(PR_SPIRIT);
		prt_spirit();
	}

	if (p_ptr->redraw & (PR_GOLD))
	{
		p_ptr->redraw &= ~(PR_GOLD);
		prt_gold();
	}

	if (p_ptr->redraw & (PR_DEPTH))
	{
		p_ptr->redraw &= ~(PR_DEPTH);
		prt_depth();
	}

	if (p_ptr->redraw & (PR_HEALTH))
	{
		p_ptr->redraw &= ~(PR_HEALTH);
		health_redraw();
	}

	if (p_ptr->redraw & (PR_STATE))
	{
		p_ptr->redraw &= ~(PR_STATE);
		prt_state();
	}

	if (p_ptr->redraw & (PR_SPEED))
	{
		p_ptr->redraw &= ~(PR_SPEED);
		prt_speed();
	}

	if (p_ptr->redraw & (PR_STUDY))
	{
		p_ptr->redraw &= ~(PR_STUDY);
		prt_study();
	}
}



/*
 * Return whether PW_INVEN is interesting
 */
static bool win_inven_good(void)
{
	int i;

	/* Boring without an inventory */
	for (i = 0; i < INVEN_PACK; i++)
	{
		/* There is some inventory */
		if (inventory[i].k_idx) return TRUE;
	}

	/* There is no inventory */
	return FALSE;
}

/*
 * Display the inventory window.
 */
static void win_inven_display(void)
{
	display_inven(FALSE);
}

/*
 * Return whether PW_EQUIP is interesting
 */
static bool win_equip_good(void)
{
	int i;

	/* Boring without equipment */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		/* There is some equipment */
		if (inventory[i].k_idx) return TRUE;
	}

	/* There is no equipment */
	return FALSE;
}

/*
 * Display the equipment window.
 */
static void win_equip_display(void)
{
	display_inven(TRUE);
}

/*
 * Return whether PW_SPELL is interesting
 */
static bool win_spell_good(void)
{
	/* Boring without any powers */
	return (book_info[BK_MIND].info[0].min <=
		skill_set[SKILL_MINDCRAFTING].value/2);
}




/*
 * Display the first player screen in a window.
 */
static void win_player_display(void)
{
	/* Display player */
	display_player(DPLAY_PLAYER);
}


/*
 * Display the third player screen in a window.
 */
static void win_player_skills_display(void)
{
	/* Display player */
	display_player(DPLAY_SKILLS);
}


/*
 * Return whether PW_MESSAGE is interesting
 */
static bool win_message_good(void)
{
	/* Boring without any messages */
	return (message_num() != 0);
}

/*
 * Display old messages in a window.
 */
static void win_message_display(void)
{
	int i, h;

	/* Get size */
	Term_get_size(&i, &h);

	/* Dump messages */
	for (i = 0; i < h; i++)
	{
		/* Dump the message on the appropriate line */
		mc_put_str((h-1)-i, 0, message_str(i));
	}
}



/*
 * Return whether PW_OVERHEAD is interesting
 */
static bool win_overhead_good(void)
{
	/* The map's always different on the surface. */
	if (!dun_level) return TRUE;

	/* The map doesn't fit on the main display. */
	if (windows[0].term->wid < cur_wid) return TRUE;
	if (windows[0].term->hgt < cur_hgt) return TRUE;

	/* This is shown on the main display. */
	return FALSE;
}

/*
 * Display the overhead map in a window.
 */
static void win_overhead_display(void)
{
	int x;

	/* Redraw map */
	display_map(0, 0, 0, &x);

	/* Hack - also give the world map if the player is in a town. */
	if (!dun_level)
	{
		display_wild_map(x+3);
	}
}


/*
 * PW_SHOPS is interesting if the player is in a town with shops.
 */
static bool win_shops_good(void)
{
	if (!dun_level && is_town_p(wildy,wildx))
	{
		return shops_good(cur_town);
	}
	else
	{
		return FALSE;
	}
}

/*
 * Give a list of shops in the current town.
 */
static void win_shops_display(void)
{
	shops_display(cur_town);
}


/*
 * Return whether PW_MONSTER is interesting
 */
static bool win_monster_good(void)
{
	/* Boring without a selected monster */
	return (monster_race_idx != 0);
}

/*
 * Display monster recall in a window.
 */
static void win_monster_display(void)
{
	/* Display monster race info */
	if (monster_race_idx) screen_roff(monster_race_idx);
}

/*
 * Return whether PW_OBJECT is interesting
 */
static bool win_object_good(void)
{
	/* Boring with a remembered object */
	return (object_kind_idx != 0);
}

/*
 * Display object kind recall in a window.
 */
static void win_object_display(void)
{
	/* Display object kind info */
	if (object_kind_idx) display_koff(object_kind_idx);
}



/*
 * Return whether PW_OBJECT_DETAILS is interesting.
 */
static bool win_object_details_good(void)
{
	object_ctype *o_ptr = tracked_o_ptr;

	/* Non-objects are boring. */
	if (!o_ptr || !(o_ptr->k_idx)) return FALSE;

	/* Invisible floor objects are boring. */
	if (find_object(o_ptr) == OUP_FLOOR && !los(py, px, o_ptr->iy, o_ptr->ix))
	{
		return FALSE;
	}

	/* Other objects are interesting. */
	return TRUE;
}

static void win_object_details_display(void)
{
	object_ctype *o_ptr = tracked_o_ptr;

	/* Never display non-objects. */
	if (!o_ptr || !(o_ptr->k_idx)) return;

	/* Never display invisible floor objects */
	if (find_object(o_ptr) == OUP_FLOOR && !los(py, px, o_ptr->iy, o_ptr->ix))
	{
		return;
	}

	/* Describe fully. */
	identify_fully_aux(o_ptr, 2);

	/* Put the name at the top. */
	mc_put_fmt(0, 0, "%v %v", get_symbol_f2, object_attr(o_ptr),
		object_char(o_ptr), object_desc_f3, o_ptr, TRUE, 3);
}

/*
 * Return whether PW_HELP is interesting.
 */
static bool win_help_good(void)
{
	/* There's a help hook present (should check for actual help). */
	return (cur_help_str() != NULL);
}

/*
 * Display some help text.
 */
static void win_help_display(void)
{
	cptr s = cur_help_str();

	/* Nothing to show. */
	if (!s) return;

	display_help_page(s);
}

/*
 * Return whether PW_VISIBLE is interesting
 */
static bool win_visible_good(void)
{
	int i;

	/* Hallucinating players find anything interesting. */
	if (p_ptr->image) return TRUE;

	/* Boring without any visible monsters. */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Skip unseen monsters */
		if (!m_ptr->ml) continue;

		/* A visible monster. */
		return TRUE;
	}

	/* No visible monsters. */
	return FALSE;
}

typedef struct monster_list_entry monster_list_entry;

struct monster_list_entry
{
	s16b r_idx; /* Monster race index */
	byte amount;
};


/*
 * Sorting hook -- Comp function -- see below
 *
 * We use "u" to point to array of monster indexes,
 * and "v" to select the type of sorting to perform on "u".
 */
static bool ang_mon_sort_comp_hook(vptr u, vptr v, int a, int b)
{
	monster_list_entry *who = (monster_list_entry*)(u);

	u16b *why = (u16b*)(v);

	int r1 = who[a].r_idx;
	int r2 = who[b].r_idx;

	int z1, z2;

	/* Sort by player kills */
	if (*why >= 4)
	{
		/* Extract player kills */
		z1 = r_info[r1].r_pkills;
		z2 = r_info[r2].r_pkills;

		/* Compare player kills */
		if (z1 < z2) return (TRUE);
		if (z1 > z2) return (FALSE);
	}

	/* Sort by total kills */
	if (*why >= 3)
	{
		/* Extract total kills */
		z1 = r_info[r1].r_tkills;
		z2 = r_info[r2].r_tkills;

		/* Compare total kills */
		if (z1 < z2) return (TRUE);
		if (z1 > z2) return (FALSE);
	}

	/* Sort by monster level */
	if (*why >= 2)
	{
		/* Extract levels */
		z1 = r_info[r1].level;
		z2 = r_info[r2].level;

		/* Compare levels */
		if (z1 < z2) return (TRUE);
		if (z1 > z2) return (FALSE);
	}

	/* Sort by monster experience */
	if (*why >= 1)
	{
		/* Extract experience */
		z1 = r_info[r1].mexp;
		z2 = r_info[r2].mexp;

		/* Compare experience */
		if (z1 < z2) return (TRUE);
		if (z1 > z2) return (FALSE);
	}

	/* Compare indexes */
	return (r1 <= r2);
}

/*
 * Sorting hook -- Swap function -- see below
 *
 * We use "u" to point to array of monster indexes,
 * and "v" to select the type of sorting to perform.
 */
static void ang_mon_sort_swap_hook(vptr u, vptr UNUSED v, int a, int b)
{
	monster_list_entry *who = (monster_list_entry*)(u);

	monster_list_entry holder;

	/* Swap */
	holder = who[a];
	who[a] = who[b];
	who[b] = holder;
}

/*
 * Dump a monster description to the screen.
 */
static void dump_race(int w, int h, int num, char attr, monster_list_entry *ptr)
{
	int x = num/(h-1)*w, y = num%(h-1)+1;

	monster_race *r_ptr = r_info+ptr->r_idx;
	int total = ptr->amount;
	byte flags = (total == 1) ? 0 : MDF_NUMBER;

	/* Dump the monster name. */
	mc_put_fmt(y, x, "%v $%c%.*v", get_symbol_f2, r_ptr->x_attr,
		r_ptr->x_char, attr, w-3, monster_desc_aux_f3, r_ptr, total, flags);
}


/*
 * Display the visible monster list in a window.
 */
static void win_visible_display(void)
{
	int i, j;
	int c = 0;
	int items = 0;

	monster_list_entry *who;

	/* XXX Hallucination - no monster list */
	if (p_ptr->image)
	{
		mc_put_str(0, 0, "You see a lot of pretty colours.");

		return;
	}

	/* Allocate the "who" array */
	C_MAKE(who, m_max, monster_list_entry);

	/* Count up the number visible in each race */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];

		bool found = FALSE;

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Skip unseen monsters */
		if (!m_ptr->ml) continue;

		/* Increase for this race */
		if (items)
		{
			for (j = 0; j < items; j++)
			{
				if (who[j].r_idx == m_ptr->r_idx)
				{
					who[j].amount++;

					found = TRUE;

					break;
				}
			}
		}

		if (!found)
		{
			who[items].r_idx = m_ptr->r_idx;
			who[items].amount = 1;

			items++;
		}

		/* Increase total Count */
		c++;
	}

	/* Are monsters visible? */
	if (items)
	{
		int w, h, num;
		u16b why = 1;

		/* First, sort the monsters by expereince*/
		ang_sort(who, &why, items, ang_mon_sort_comp_hook,
			ang_mon_sort_swap_hook);

		/* Then, display them */
		(void)Term_get_size(&w, &h);

		/* Find the optimal width of one entry. */
		w = MAX(26, w/((items+h-2)/(h-1)));

		mc_put_fmt(0, 0, "You can see %d monster%s.", c, (c > 1 ? "s:" : ":"));

		/* Print the monsters in reverse order */
		for (i = items - 1, num = 0; i >= 0; i--, num++)
		{
			monster_race *r_ptr = r_info+who[i].r_idx;

			/* Default Colour */
			char attr = 'w';

			/* Uniques */
			if (r_ptr->flags1 & RF1_UNIQUE)
			{
				attr = 'R';
			}

			/* Have we ever killed one? */
			if (r_ptr->r_tkills)
			{
				if (r_ptr->level > dun_depth)
				{
					attr = 'v';

					if (r_ptr->flags1 & RF1_UNIQUE)
					{
						attr = 'r';
					}
				}
			}
			else
			{
				if (!r_ptr->flags1 & RF1_UNIQUE) attr = 's';
			}

			dump_race(w, h, num, attr, who+i);
		}
	}
	else
	{
		mc_put_str(0, 0, "You see no monsters.");
	}

	/* XXX XXX Free the "who" array */
	FREE(who);
}


/*
 * Check whether the floor display is "interesting".
 * True if the player character can see some floor at his feet.
 */
static bool win_floor_good(void)
{
	/* No floor to stand on. */
	if (!character_dungeon) return FALSE;

	/* An imaginary floor to stand on. */
	if (p_ptr->image) return TRUE;

	/* Not a real square. */
	if (!in_bounds2(tracked_co_ord.y, tracked_co_ord.x)) return FALSE;

	/* A visible wall. */
	return (cave[tracked_co_ord.y][tracked_co_ord.x].info & CAVE_MARK ||
		player_can_see_bold(tracked_co_ord.y, tracked_co_ord.x));
}


/*
 * Display a list of objects on the floor.
 *
 * The correct code here depends unpleasantly on that of target_set_aux().
 */
static void win_floor_display(void)
{
	cptr verb;

	if (tracked_co_ord.x == px && tracked_co_ord.y == py)
		verb = "are standing on";
	else
		verb = "see";

	if (p_ptr->image)
	{
		mc_put_fmt(0, 0, "You %s something strange.\n", verb);
	}
	else if (in_bounds2(tracked_co_ord.y, tracked_co_ord.x))
	{
		int y = 0;
		cave_type *c_ptr = &cave[tracked_co_ord.y][tracked_co_ord.x];
		monster_type *m_ptr = m_list+c_ptr->m_idx;
		monster_race *r_ptr = r_info+m_ptr->r_idx;
		object_type *o_ptr = o_list+c_ptr->o_idx;

		mc_put_fmt(y++, 0, "You %s %v.\n", verb, feature_desc_f2,
			c_ptr->feat, FDF_INDEF);

		if (c_ptr->m_idx && m_ptr->ml)
		{
			mc_put_fmt(y++, 0, "%v %v", get_symbol_f2, r_ptr->x_attr,
				r_ptr->x_char, monster_desc_f2, m_ptr, 0x0C);
		}

		for (; y < Term->hgt && o_ptr != o_list;
			o_ptr = o_list+o_ptr->next_o_idx)
		{
			if (!o_ptr->marked) continue;

			mc_put_fmt(y++, 0, "%v %v", get_symbol_f2, object_attr(o_ptr),
				object_char(o_ptr), object_desc_f3, o_ptr, TRUE, 3);
		}
	}
}


/*
 * The list of display functions.
 * This has an extra element to store PW_NONE, although it should not be set
 * by normal mechanisms.
 */
display_func_type display_func[NUM_DISPLAY_FUNCS+1] =
{
	{PW_INVEN, "inventory", win_inven_good, win_inven_display},
	{PW_EQUIP, "equipment", win_equip_good, win_equip_display},
	{PW_SPELL, "spell list", win_spell_good, display_spell_list},
	{PW_PLAYER, "character", func_true, win_player_display},
	{PW_PLAYER_SKILLS, "skills", func_true, win_player_skills_display},
	{PW_VISIBLE, "nearby monsters", win_visible_good, win_visible_display},
	{PW_MESSAGE, "messages", win_message_good, win_message_display},
	{PW_OVERHEAD, "overhead view", win_overhead_good, win_overhead_display},
	{PW_MONSTER, "monster recall", win_monster_good, win_monster_display},
	{PW_SHOPS, "shop names", win_shops_good, win_shops_display},
	{PW_OBJECT, "object recall", win_object_good, win_object_display},
	{PW_OBJECT_DETAILS, "object details", win_object_details_good,
		win_object_details_display},
	{PW_FLOOR, "floor information", win_floor_good, win_floor_display},
	{PW_HELP, "help", win_help_good, win_help_display},
	{PW_NONE, "", func_false, func_nothing},
};

/*
 * Choose what to display in a window after a "rotate" request, i.e.
 * the first "interesting" display after the current one which has a priority
 * greater than 0.
 *
 * If there are no suitable displays, it returns a blank display.
 */
static int window_rotate(const window_type *w_ptr)
{
	int c, n;
	for (c = 1; c < NUM_DISPLAY_FUNCS; c++)
	{
		n = (w_ptr->current+c)%NUM_DISPLAY_FUNCS;

		/* Don't stop at a priority of 0. */
		if (w_ptr->rep[n] == 0) continue;

		/* Stop at any other interesting display. */
		if ((*display_func[n].good)()) return n;
	}
	return DISPLAY_NONE;
}

/*
 * Choose what to display in a window in a normal window_stuff() call according
 * to the following criteria:
 *
 * 1. Never show displays with a priority+ of 0
 * 2. Prefer "interesting" displays to "boring" ones.
 * 3. Prefer high priority ones to low priority ones.
 * 4. Prefer changing windows to unchanging ones.
 * 5. Prefer the current display, and others following it in order.
 *
 * + If a display is being changes at this invocation, the "rep"
 * field is used to determine its priority. If not, the "pri" field
 * is used.
 */
static int window_best(const window_type *w_ptr, const u32b rep_mask)
{
	/* n is set if n_good != 0. */
	int i, UNREAD(n), i_good, n_good;

	/*
	 * Find an appropriate display by turning the qualities above
	 * which add to a display's goodness into numbers and comparing
	 * them. As n_good is initially less than 4, it can only fail to be
	 * replaced if the every display has a priority of 0.
	 */
	for (n_good = 0, i = 0; i < NUM_DISPLAY_FUNCS; i++)
	{
		display_func_type *d_ptr = display_func+i;

		/* Decide whether display i is better than display n. */
		if (rep_mask & d_ptr->flag)
			i_good = w_ptr->rep[i]*4;
		else
			i_good = w_ptr->pri[i]*4; /* 3 */

		if (!i_good) continue; /* 1 (hack) */
		if (i >= w_ptr->current) i_good++; /* 5 */
		if (rep_mask & d_ptr->flag) i_good += 2; /* 4 */
		if ((*d_ptr->good)()) i_good += 64; /* 2 */

		/* If display i is better, set n to i. */
		if (i_good > n_good)
		{
			n = i;
			n_good = i_good;
		}
	}

	/* If no positive priorities are found, do nothing. */
	if (!n_good) n = DISPLAY_NONE;

	return n;
}

/*
 * Display the indicated display in the indicated window.
 */
static void show_display_func(window_type *w_ptr, int display)
{
	assert(w_ptr && w_ptr->term); /* Caller. */

	/* Access the term. */
	Term_activate(w_ptr->term);

	/* Draw the display. */
	clear_from(0);
	(*display_func[display].display)();
	Term_fresh();
}

/*
 * Handle "p_ptr->window"
 *
 * This makes the assumption that the main display will always be displayed
 * on term_screen. This is enforced elsewhere, but is not a necessary
 * assumption as the main display is not always of su
 */
void window_stuff(void)
{
	int m, n;

	u32b old_window = p_ptr->window & WINDOW_STUFF_MASK;

	/* Remember the original term (i.e. term_screen) */
	term *old = Term;

	bool rotate = (old_window & PW_ROTATE) != 0;

	/* Nothing to do */
	if (!old_window) return;

	/* Only process this display once. */
	p_ptr->window &= ~(old_window);

	/* Scan windows */
	for (m = 0; m < ANGBAND_TERM_MAX; m++)
	{
		window_type *w_ptr = &windows[m];

		/* Skip non-existant windows */
		if (!w_ptr->term) continue;

		/* Hack - skip window containing main display */
		if (w_ptr->term == term_screen) continue;

		if (rotate)
		{
			n = window_rotate(w_ptr);
		}
		else
		{
			n = window_best(w_ptr, old_window);
		}

		/* If different display is to be shown, show it. */
		if (n != w_ptr->current || old_window & display_func[n].flag)
		{
			display_func_type *d_ptr = display_func+n;

			/* Set the current display. */
			w_ptr->current = n;

			/* Return to this routine next time. */
			if (d_ptr->flag != PW_NONE) p_ptr->window |= PW_RETURN;

			/* Show the current display. */
			show_display_func(w_ptr, n);
		}
	}

	/* Restore the original terminal. */
	Term_activate(old);
}


/*
 * An entirely mis-named function. This causes all window flags with the same
 * window and non-boring quality to be rotated when window_stuff() is next
 * called.
 */
void toggle_inven_equip(void)
{
	/* Rotate the displays. */
	p_ptr->window |= PW_ROTATE;
}

/*
 * Update various things in response to non-keypress events.
 */
void event_stuff(void)
{
	/* Call window_stuff() if the event affects it. */
	if (p_ptr->window & WINDOW_STUFF_MASK & ~(PW_RETURN))
	{
		window_stuff();
	}
}

/*
 * Redraw the current term via a display function.
 */
void resize_window(void)
{
	window_type *w_ptr;

	/* Hack - don't call during birth or level creation. */
	if (!character_dungeon) return;

	/* Paranoia - never call for term_screen */
	if (Term == term_screen) return;


	/* Hack -- react to changes (does this ever do anything?) */
	Term_xtra(TERM_XTRA_REACT, 0);

	/* Hack - find the window. */
	FOR_ALL_IN(windows, w_ptr) if (w_ptr->term == Term) break;

	/* Paranoia - resizing an abnormal term. */
	if (w_ptr == END_PTR(windows)) return;

	/* Show the current display. */
	show_display_func(w_ptr, w_ptr->current);
}



/*
 * Handle "p_ptr->update" and "p_ptr->redraw"
 */
void handle_stuff(void)
{
	/* Update stuff */
	if (p_ptr->update) update_stuff();

	/* Redraw stuff */
	if (p_ptr->redraw) redraw_stuff();
}


bool ma_empty_hands(void)
{
	return !(inventory[INVEN_WIELD].k_idx);
}

/*
 * Update the maxima for each skill, and possibly give chaos patron rewards
 */
static void update_skill_maxima(void)
{
	player_skill *ptr;
	const bool want_gifts = p_has_mutation(MUT_CHAOS_GIFT);

	/* Assume no chaos effect for now. */
	int chaos = 0;

	/* Broo might get a gift as they get better */
	ptr = &skill_set[SKILL_RACIAL];
	if (!chaos && player_has_flag(TR0, TR0_CHAOS) &&
		(ptr->value > ptr->max_value))
	{
		int chance = MIN(90, MAX(10, ptr->value));
		if (!chaos && percent(chance)) chaos = SKILL_RACIAL;
	}
	/* Anyone with a thaumaturgy skill has been calling out to chaos */
	ptr = &skill_set[SKILL_THAUMATURGY];
	if (!chaos && ptr->value > ptr->max_value)
	{
		int chance = MIN(90, MAX(10, ptr->value));
		if (!chaos && rand_int(100) < chance) chaos = SKILL_THAUMATURGY;
	}

	/* Now update all the maxima */
	for (ptr = skill_set; ptr < skill_set+MAX_SKILLS; ptr++)
	{
		int chance = MIN(90, MAX(10, ptr->value));

		/* No increase. */
		if (ptr->value <= ptr->max_value) continue;

		/* Accept the increase. */
		ptr->max_value = ptr->value;

		/* Sometimes get an extra reward. */
		if (!chaos && want_gifts && rand_int(500) < chance)
		{
			chaos = ptr-skill_set;
		}
	}

	/* Give a chaos reward, if allowed. */
	if (chaos && chaos_patrons) gain_level_reward(0, skill_set[chaos].value);
}

/*
 * Chance of succeeding on the nth skill check after touching a new object.
 * Based on a sine wave for want of a better shape.
 */
static int object_skill_table[] =
{
	100, 100, 99, 97, 95, 92, 89, 85, 81, 76,
	70, 65, 59, 52, 45, 38, 31, 23, 16, 8
};

/*
 * Not encountering new objects leads to boredom (you should be exploring, not
 * studying) and a reduction in skill gain.
 */
static bool PURE object_skill_fail(void)
{
	/* 0% chance. */
	if (object_skill_count > (int)N_ELEMENTS(object_skill_table)) return TRUE;

	/* Use the table above. */
	return !percent(object_skill_table[object_skill_count++]);
}

/* Test whether a skill can be tested on the current level */
bool PURE skill_check_possible(player_skill *sk_ptr)
{
	/* Never on the surface or too shallow a level. */
	return (dun_level && dun_depth >= (sk_ptr->value - sk_ptr->base) * 2 / 3);
}

/* Give experience to a skill after usage */
void skill_exp(int index)
{
	player_skill *sk_ptr = &skill_set[index];

	/* No experience is gained on the surface */
	if (!dun_level) return;

	if (cheat_skll)
	{
		msg_format("Check %s skill, values %d/%d, exp %d/%d.",
			sk_ptr->name, sk_ptr->value, sk_ptr->base,
			sk_ptr->experience, sk_ptr->exp_to_raise);
	}

	if (!skill_check_possible(sk_ptr))
	{
		if (cheat_skll)
		{
			msg_format("You are not tense enough to improve your %s skill.",
				sk_ptr->name);
		}
		return;
	}

	if (object_skill_fail())
	{
		if (cheat_skll)
		{
			msg_format("You are not inquisitive enough to "
				"improve your %s skill.", sk_ptr->name);
		}
		return;
	}

	/* Debugging message */
	if (cheat_skll)
	{
		msg_format("Skill check for %s.",sk_ptr->name);
	}

	/* Means a player with an expfact of 100 has a 1-1 mapping */
	sk_ptr->experience += 100; 

	if(sk_ptr->experience >= sk_ptr->exp_to_raise * rp_ptr->r_exp)
	{

		/* Debugging message */
		if (cheat_skll)
		{
			msg_format("%s tested.",sk_ptr->name);
		}

		sk_ptr->experience-=sk_ptr->exp_to_raise * rp_ptr->r_exp;
		if(((byte)rand_int(100)>=(sk_ptr->value)) && (sk_ptr->value < 100))
		{
			bool more;
			sk_ptr->value++;

			/* Update other skill-related variables. */
			switch (index)
			{
				case SKILL_TOUGH: p_ptr->update |= PU_HP; break;
				case SKILL_MANA: p_ptr->update |= PU_SPELLS | PU_MANA; break;
				case SKILL_CHI: p_ptr->update |= PU_MANA; break;
				case SKILL_MA: p_ptr->update |= PU_MA_ARMOUR; break;
			}
			update_stuff();

			more = skill_check_possible(sk_ptr);

			msg_format("%s %c%d%%->%d%%%c",sk_ptr->increase,
				more ? '(' : '[',sk_ptr->value-1, sk_ptr->value,
				more ? ')' : ']');

			p_ptr->window |= PW_PLAYER_SKILLS; /* Window stuff */

			/* Update the maxima and possibly give rewards */
			update_skill_maxima();
		}
	}
}

/*
 * Determine where an object is in terms of OUP_* flags.
 */
int find_object(object_ctype *o_ptr)
{
	int slot;
	/* Floor item. */
	if (o_ptr >= o_list && o_ptr < o_list+MAX_O_IDX)
	{
		/* Here. */
		if (o_ptr->iy == py && o_ptr->ix == px) return OUP_FLOOR;

		/* Somewhere else. */
		else return 0;
	}
	/* Inventory item. */
	else if (o_ptr >= inventory && o_ptr < inventory+INVEN_TOTAL)
	{
		slot = o_ptr-inventory;
		if (slot <= INVEN_PACK) return OUP_INVEN;
		if (slot >= INVEN_POUCH_1 && slot <= INVEN_POUCH_6) return OUP_POUCH;
		if (slot >= INVEN_WIELD && slot <= INVEN_FEET) return OUP_EQUIP;
	}

	/* Unknown. */
	return 0;
}

/*
 * Update everything which may change after one or more objects in a class has
 * been changed.
 */
void update_objects(int where)
{
	if (where & OUP_FLOOR)
	{
		/* Squelch the item if needed. */
		p_ptr->notice |= PN_FSQUELCH;

		/* Display the floor under the player, as the object may be there. */
		cave_track(py, px);
	}
	if (where & OUP_INVEN)
	{
		/* Put the object in the correct position. */
		p_ptr->notice |= PN_COMBINE | PN_REORDER | PN_ISQUELCH;

		/* Correct the speed, for if the weight has changed. */
		p_ptr->update |= PU_BONUS;

		/* Display the inventory window. */
		p_ptr->window |= PW_INVEN;
	}
	/* Pouches are . */
	if (where & OUP_POUCH)
	{
		/* Correct the speed, for if the weight has changed. */
		p_ptr->update |= PU_BONUS;

		/* Display the equipment window. */
		p_ptr->window |= PW_EQUIP;
	}
	if (where & OUP_EQUIP)
	{
		/* Update various item bonuses. */
		p_ptr->update |= PU_BONUS;

		/* Update separately for cumber_*() and armour weight. */
		p_ptr->update |= PU_MANA;

		/* Display the equipment window. */
		p_ptr->window |= PW_EQUIP;

		/* Display the player window, as some changes may not do this. (?) */
		p_ptr->window |= PW_PLAYER;
	}
}

/*
 * Update everything which needs to be updated after an object changes.
 */
void update_object(object_type *o_ptr)
{
	/* Hack - Give the changed object a valid stack number if appropriate.
	 * There may be a better place to put this. */
	if (!o_ptr->stack) set_stack_number(o_ptr);

	update_objects(find_object(o_ptr));
}

