/* File: info.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

/*
 * Obtain the "flags" for an item
 */
static void object_flags_aux(int mode, object_type *o_ptr, u32b *f1, u32b *f2, u32b *f3)
{
	bool mental = (o_ptr->ident & IDENT_MENTAL);

	object_kind *k_ptr;

	/* Check artifact knowledge status */
	if (artifact_p(o_ptr))
	{
		artifact_type *a_ptr = &a_info[o_ptr->a_idx];

		if (artifact_known_p(a_ptr)) mental = TRUE;
	}

	if (mode != OBJECT_INFO_FULL)
	{
		/* Clear */
		(*f1) = (*f2) = (*f3) = 0L;

		/* Must be identified */
		if (!object_known_p(o_ptr)) 
		{
			/* Hack - know the light radius of lite items */
			if (o_ptr->a_idx) 
			{
				(*f3) = (a_info[o_ptr->a_idx].flags3 & (TR3_LITE_MASK));
			}
			else (*f3) = (k_info[o_ptr->k_idx].flags3 & (TR3_LITE_MASK)); 

			return;
		}
	}

	if (mode != OBJECT_INFO_RANDOM)
	{
		k_ptr = &k_info[o_ptr->k_idx];

		/* Base object */
		(*f1) = k_ptr->flags1;
		(*f2) = k_ptr->flags2;
		(*f3) = k_ptr->flags3;

		if (mode == OBJECT_INFO_FULL)
		{
			/* Artifact */
			if (artifact_p(o_ptr))
			{
				artifact_type *a_ptr = &a_info[o_ptr->a_idx];

				(*f1) = a_ptr->flags1;
				(*f2) = a_ptr->flags2;
				(*f3) = a_ptr->flags3;
			}
		}

		/* Ego-item */
		if (o_ptr->e_idx)
		{
			ego_item_type *e_ptr = &e_info[o_ptr->e_idx];

			(*f1) |= e_ptr->flags1;
			(*f2) |= e_ptr->flags2;
			(*f3) |= e_ptr->flags3;
		}

		if (mode == OBJECT_INFO_KNOWN)
		{
			if (o_ptr->e_idx)
			{
				ego_item_type *e_ptr = &e_info[o_ptr->e_idx];

				/* Obvious flags (pval) */
				(*f1) = (e_ptr->flags1);
			}

			/* Obvious artifact flags */
			if (o_ptr->a_idx)
			{
				artifact_type *a_ptr = &a_info[o_ptr->a_idx];

				/* Obvious flags (pval, elemental ignores) */
				(*f1) = (a_ptr->flags1 & (TR1_PVAL_MASK));

				(*f3) = (a_ptr->flags3 & (TR3_IGNORE_ELEM | TR3_IGNORE_NON_ELEM));

				/* Always know the radius of light sources */
				(*f3) |= (a_ptr->flags3 & (TR3_LITE_MASK));

			}
		}
	}

	if (mode != OBJECT_INFO_FULL)
	{
		bool spoil = FALSE;

#ifdef SPOIL_ARTIFACTS
		/* Full knowledge for some artifacts */
		if (artifact_p(o_ptr)) spoil = TRUE;
#endif /* SPOIL_ARTIFACTS */

#ifdef SPOIL_EGO_ITEMS
		/* Full knowledge for some ego-items */
		if (ego_item_p(o_ptr)) spoil = TRUE;
#endif /* SPOIL_ARTIFACTS */

		/* Need full knowledge or spoilers */
		if (!spoil && !mental) return;

		/* Artifact */
		if (o_ptr->a_idx)
		{
			artifact_type *a_ptr = &a_info[o_ptr->a_idx];

			(*f1) = a_ptr->flags1;
			(*f2) = a_ptr->flags2;
			(*f3) = a_ptr->flags3;

			if (mode == OBJECT_INFO_RANDOM)
			{
				/* Hack - remove ALL flags, until random artifacts return */
				(*f1) = 0;
				(*f2) = 0;
				(*f3) = 0;
			}
		}

		/* Full knowledge for *identified* objects */
		if (!mental) return;
	}

	/* Extra powers */
	switch (o_ptr->xtra1)
	{
		case OBJECT_XTRA_TYPE_SUSTAIN:
		{
			(*f1) |= (OBJECT_XTRA_BASE_SUSTAIN << o_ptr->xtra2);
			break;
		}

		case OBJECT_XTRA_TYPE_POWER:
		{
			(*f3) |= (OBJECT_XTRA_BASE_POWER << o_ptr->xtra2);
			break;
		}
	}
}

/*
 * Obtain the "flags" for an item
 */
void object_flags(object_type *o_ptr, u32b *f1, u32b *f2, u32b *f3)
{
	object_flags_aux(OBJECT_INFO_FULL, o_ptr, f1, f2, f3);
}

/*
 * Obtain the "flags" for an item which are known to the player
 */
static void object_flags_known(object_type *o_ptr, u32b *f1, u32b *f2, u32b *f3)
{
	object_flags_aux(OBJECT_INFO_KNOWN, o_ptr, f1, f2, f3);
}

/*
 * Obtain the resistances from an item 
 */
byte object_resist(object_type *o_ptr, int res_type)
{
	object_kind *k_ptr = &k_info[o_ptr->k_idx];
	artifact_type *a_ptr = &a_info[o_ptr->a_idx];
	ego_item_type *e_ptr = &e_info[o_ptr->e_idx];

	int i = 0;

	/* Random abilities for ego items */
	if (ego_item_p(o_ptr))
	{
		if (((o_ptr->xtra1 == OBJECT_XTRA_TYPE_MID_RESIST) ||
			(o_ptr->xtra1 == OBJECT_XTRA_TYPE_MID_RESIST)) &&
			(o_ptr->xtra2 == res_type))
			i = 25;
	}

	return (k_ptr->res[res_type] + a_ptr->res[res_type] + e_ptr->res[res_type] + i);
}

/*
 * Obtain the resistances from an item 
 */
byte object_resist_known(object_type *o_ptr, int res_type)
{
	object_kind *k_ptr = &k_info[o_ptr->k_idx];
	artifact_type *a_ptr = &a_info[o_ptr->a_idx];
	ego_item_type *e_ptr = &e_info[o_ptr->e_idx];

	int res = 0;

	if (object_known_p(o_ptr)) 
	{
		res = k_ptr->res[res_type] + e_ptr->res[res_type];
		if (artifact_known_p(a_ptr)) res += a_ptr->res[res_type];
		/* Known random ego-item resists */
		if (ego_item_p(o_ptr) && (o_ptr->ident & IDENT_MENTAL))
		if (((o_ptr->xtra1 == OBJECT_XTRA_TYPE_MID_RESIST) ||
			(o_ptr->xtra1 == OBJECT_XTRA_TYPE_MID_RESIST)) &&
			(o_ptr->xtra2 == res_type))
			res += 25;
	}

	return (res);
}

/*
 * Obtain the slays from an item 
 * The highest slay amout the possibilities.
 */
void weapon_slays(object_type *o_ptr, byte *slays)
{
	int j;
	byte slay;

	object_kind *k_ptr = &k_info[o_ptr->k_idx];
	artifact_type *a_ptr = &a_info[o_ptr->a_idx];
	ego_item_type *e_ptr = &e_info[o_ptr->e_idx];
	weapon_prefix_type *wpx_ptr = &wpx_info[o_ptr->prefix_idx];

	/* Extract slays */
	for (j = 0 ; j < SL_MAX ; j++)
	{
		slay = k_ptr->slays[j];

		if (a_ptr->slays[j] > slay) slay = a_ptr->slays[j];
		if (e_ptr->slays[j] > slay) slay = e_ptr->slays[j];
		if ((o_ptr->tval == TV_SWORD) || (o_ptr->tval == TV_HAFTED) || 
			(o_ptr->tval == TV_POLEARM))
		{
			if (wpx_ptr->slays[j] > slay) slay = wpx_ptr->slays[j];
		}

		slays[j] = slay;
	}
}

/*
 * Obtain the slays from an item 
 * The highest slay amout the possibilities.
 */
void weapon_slays_known(object_type *o_ptr, byte *slays)
{
	int j;
	byte slay;

	object_kind *k_ptr = &k_info[o_ptr->k_idx];
	artifact_type *a_ptr = &a_info[o_ptr->a_idx];
	ego_item_type *e_ptr = &e_info[o_ptr->e_idx];
	weapon_prefix_type *wpx_ptr = &wpx_info[o_ptr->prefix_idx];

	/* Extract slays */
	for (j = 0 ; j < SL_MAX ; j++)
	{
		slay = k_ptr->slays[j];

		if (object_known_p(o_ptr))
		{
			if (artifact_known_p(a_ptr) && (a_ptr->slays[j] > slay)) slay = a_ptr->slays[j];
			if (e_ptr->slays[j] > slay) slay = e_ptr->slays[j];
		}

		if ((o_ptr->tval == TV_SWORD) || (o_ptr->tval == TV_HAFTED) || 
			(o_ptr->tval == TV_POLEARM))
		{
			if (wpx_ptr->slays[j] > slay) slay = wpx_ptr->slays[j];
		}

		slays[j] = slay;
	}
}

/*
 * Determine the "Activation" (if any) for an artifact
 */
static bool item_activation(char *text, int *time1, int *time2, object_type *o_ptr)
{
	int j;
	u16b act = object_activation(o_ptr);

	/* No activation */
	if (!act) return (FALSE);

	/* Paranoia */
	if (act >= POW_MAX) return (FALSE);

	for (j = (POW_MAX - 1); j > 0; j--) 
	{
		if (power_info[j].index == act) break;
	}

	/* Display that spell's information. */
	if (power_info[j].desc != NULL) strcpy(text, power_info[j].desc);

	/* Artifact activations */
	if (o_ptr->a_idx)
	{
		artifact_type *a_ptr = &a_info[o_ptr->a_idx];

		/* Some artifacts can be activated */
		*time1 = a_ptr->time;
		*time2 = a_ptr->randtime;
	}
	else (*time1 = *time2 = 0);

	return (TRUE);
}

/*
 * Hack -- pass color info around this file
 */
static byte likert_color = TERM_WHITE;

/*
 * Returns a "rating" of x depending on y
 */
static cptr likert(int x, int y)
{
	/* Paranoia */
	if (y <= 0) y = 1;

	/* Negative value */
	if (x < 0)
	{
		likert_color = TERM_RED;
		return ("Very Bad");
	}

	/* Analyze the value */
	switch ((x / y))
	{
		case 0:
		case 1:
		{
			likert_color = TERM_RED;
			return ("Bad");
		}
		case 2:
		{
			likert_color = TERM_RED;
			return ("Poor");
		}
		case 3:
		case 4:
		{
			likert_color = TERM_YELLOW;
			return ("Fair");
		}
		case 5:
		{
			likert_color = TERM_YELLOW;
			return ("Good");
		}
		case 6:
		{
			likert_color = TERM_YELLOW;
			return ("Very Good");
		}
		case 7:
		case 8:
		{
			likert_color = TERM_L_GREEN;
			return ("Excellent");
		}
		case 9:
		case 10:
		case 11:
		case 12:
		case 13:
		{
			likert_color = TERM_L_GREEN;
			return ("Superb");
		}
		case 14:
		case 15:
		case 16:
		case 17:
		{
			likert_color = TERM_L_GREEN;
			return ("Heroic");
		}
		default:
		{
			likert_color = TERM_L_GREEN;
			return ("Legendary");
		}
	}
}

/*
 * Player's Racial Powers - Big Hack.
 */
static void display_player_race_power(int y, int x)
{
	int turns = 0;
	int line = 0;

	switch (rsp_ptr[(p_ptr->max_lev) / 5]->power)
	{
		case 1:
		{
			Term_putstr(x , y + line++, -1, TERM_L_BLUE, "Detect Evil");
			turns = 50;
			break;
		}
		case 2:
		{
			Term_putstr(x , y + line++, -1, TERM_L_BLUE, "Call Light");
			Term_putstr(x , y + line++, -1, TERM_L_BLUE, "(dam. 2d2)");
			turns = 50;
			break;
		}
		case 3:
		{
			Term_putstr(x , y + line++, -1, TERM_L_BLUE, "Spear of Light");
			Term_putstr(x , y + line++, -1, TERM_L_BLUE, "(dam. 6d8)");
			turns = 25;
			break;
		}
		case 4:
		{
			Term_putstr(x , y + line++, -1, TERM_L_BLUE, "Orb of Draining");
			Term_putstr(x , y + line++, -1, TERM_L_BLUE, "(dam. 3d6+50)");
			turns = 15;
			break;
		}
		case 5:
		{
			Term_putstr(x , y + line++, -1, TERM_L_BLUE, "Prot. from Evil");
			Term_putstr(x , y + line++, -1, TERM_L_BLUE, "(dur. d25+30)");
			turns = 150;
			break;
		}
		case 6:
		{
			Term_putstr(x , y + line++, -1, TERM_L_BLUE, "Blink");
			Term_putstr(x , y + line++, -1, TERM_L_BLUE, "(radius 10)");
			turns = 20;
			break;
		}
		case 7:
		{
			Term_putstr(x , y + line++, -1, TERM_L_BLUE, "Fire Bolt");
			Term_putstr(x , y + line++, -1, TERM_L_BLUE, "(dam. 10d8)");
			turns = 20;
			break;
		}
		case 8:
		{
			Term_putstr(x , y + line++, -1, TERM_L_BLUE, "Fire Ball");
			Term_putstr(x , y + line++, -1, TERM_L_BLUE, "(dam. 85 rad 2)");
			turns = 20;
			break;
		}
		case 9:
		{
			Term_putstr(x , y + line++, -1, TERM_L_BLUE, "Plasma Ball");
			Term_putstr(x , y + line++, -1, TERM_L_BLUE, "(dam. 4d7+100)");
			Term_putstr(x , y + line++, -1, TERM_L_BLUE, " rad 3)");
			turns = 20;
			break;
		}
		default:
		{
			/* Paranoia */
			return;
		}
	}

	if (line) Term_putstr(x, y + line , -1, TERM_L_BLUE, format("Every %d Turns", turns));
}

/*
 * Prints some "extra" information on the screen.
 *
 * Space includes rows 3-9 cols 24-79
 * Space includes rows 10-17 cols 1-79
 * Space includes rows 19-22 cols 1-79
 */
static void display_player_xtra_info(void)
{
	int col;
	int hit, dam;
	int base, plus;
	int i;

	object_type *o_ptr;

	char buf[160];

	/* Upper middle */
	col = 26;

	/* Left */
	col = 1;

	/* Level */
	Term_putstr(col, 10, -1, TERM_WHITE, "Level");
	if (p_ptr->lev >= p_ptr->max_lev)
	{
		Term_putstr(col+8, 10, -1, TERM_L_GREEN,
		            format("%10d", p_ptr->lev));
	}
	else
	{
		Term_putstr(col+8, 10, -1, TERM_YELLOW,
		            format("%10d", p_ptr->lev));
	}

	/* Current Experience */
	Term_putstr(col, 11, -1, TERM_WHITE, "Cur Exp");
	if (p_ptr->exp >= p_ptr->max_exp)
	{
		Term_putstr(col+8, 11, -1, TERM_L_GREEN,
		            format("%10ld", p_ptr->exp));
	}
	else
	{
		Term_putstr(col+8, 11, -1, TERM_YELLOW,
		            format("%10ld", p_ptr->exp));
	}

	/* Maximum Experience */
	Term_putstr(col, 12, -1, TERM_WHITE, "Max Exp");
	Term_putstr(col+8, 12, -1, TERM_L_GREEN,
	            format("%10ld", p_ptr->max_exp));

	/* Advance Experience */
	Term_putstr(col, 13, -1, TERM_WHITE, "Adv Exp");
	if (p_ptr->lev < PY_MAX_LEVEL)
	{
		s32b advance = (player_exp[p_ptr->lev - 1] *
		                p_ptr->expfact / 100L);
		Term_putstr(col+8, 13, -1, TERM_L_GREEN,
		            format("%10ld", advance));
	}
	else
	{
		Term_putstr(col+8, 13, -1, TERM_L_GREEN,
		            format("%10s", "********"));
	}

	/* Gold */
	Term_putstr(col, 15, -1, TERM_WHITE, "Gold");
	Term_putstr(col+8, 15, -1, TERM_L_GREEN,
	            format("%10ld", p_ptr->au));

	/* Burden */
	sprintf(buf, "%ld.%ld lbs",
	        p_ptr->total_weight / 10,
	        p_ptr->total_weight % 10);
	Term_putstr(col, 16, -1, TERM_WHITE, "Burden");
	Term_putstr(col+8, 16, -1, TERM_L_GREEN,
	            format("%10s", buf));

	/* Left Middle */
	col = 21;

	/* Armor */
	base = p_ptr->dis_ac;
	plus = p_ptr->dis_to_a;

	/* Total Armor */
	sprintf(buf, "[%d,%+d]", base, plus);
	Term_putstr(col, 10, -1, TERM_WHITE, "Armor");
	Term_putstr(col+5, 10, -1, TERM_L_BLUE, format("%13s", buf));

	/* Base skill */
	hit = p_ptr->dis_to_h;
	dam = p_ptr->dis_to_d;

	/* Basic fighting */
	sprintf(buf, "(%+d,%+d)", hit, dam);
	Term_putstr(col, 11, -1, TERM_WHITE, "Fight");
	Term_putstr(col+5, 11, -1, TERM_L_BLUE, format("%13s", buf));

	/* Melee weapon */
	o_ptr = &inventory[INVEN_WIELD];

	/* Base skill */
	hit = p_ptr->dis_to_h;
	dam = p_ptr->dis_to_d;

	/* Apply weapon bonuses */
	if (object_known_p(o_ptr)) hit += actual_to_h(o_ptr);
	if (object_known_p(o_ptr)) dam += actual_to_d(o_ptr);

	/* Melee attacks */
	sprintf(buf, "(%+d,%+d)", hit, dam);
	Term_putstr(col, 12, -1, TERM_WHITE, "Melee");
	Term_putstr(col+5, 12, -1, TERM_L_BLUE, format("%13s", buf));

	/* Range weapon */
	o_ptr = &inventory[INVEN_BOW];

	/* Base skill */
	hit = p_ptr->dis_to_h;
	dam = 0;

	/* Apply weapon bonuses */
	if (object_known_p(o_ptr)) hit += actual_to_h(o_ptr);
	if (object_known_p(o_ptr)) dam += actual_to_d(o_ptr);

	/* Range attacks */
	sprintf(buf, "(%+d,%+d)", hit, dam);
	Term_putstr(col, 13, -1, TERM_WHITE, "Shoot");
	Term_putstr(col+5, 13, -1, TERM_L_BLUE, format("%13s", buf));

	/* Blows */
	sprintf(buf, "%d/turn", p_ptr->num_blow);
	Term_putstr(col, 15, -1, TERM_WHITE, "Blows");
	Term_putstr(col+5, 15, -1, TERM_L_BLUE, format("%13s", buf));

	/* Shots */
	sprintf(buf, "%d/turn", p_ptr->num_fire);
	Term_putstr(col, 16, -1, TERM_WHITE, "Shots");
	Term_putstr(col+5, 16, -1, TERM_L_BLUE, format("%13s", buf));

	/* Right Middle */
	col=41;

	/* Infra */
	sprintf(buf, "%d ft", p_ptr->see_infra * 10);
	Term_putstr(col, 10, -1, TERM_WHITE, "Infra");
	Term_putstr(col+5, 10, -1, TERM_L_BLUE, format("%9s", buf));

	/* Racial Ability */
	put_str("Racial Ability", 12, col);
	if ((rp_ptr->special) && (rsp_ptr[(p_ptr->max_lev)/5]->power > 0))
	{
		display_player_race_power(13, col);
	}
	else
	Term_putstr(col, 13, -1, TERM_L_BLUE, format("None", buf));

	/* Bottom */
	col = 1;

	/* History */
	for (i = 0; i < 5; i++)
	{
		put_str(p_ptr->history[i], i + 18, col);
	}
}

/*
 * Prints some skill information on the screen.
 */
static void display_player_skill_info(void)
{
	int col;
	int xskill[SK_MAX];

	cptr desc;

	object_type *o_ptr;

	/* Right */
	col = 57;

	/* Fighting Skill (with current weapon) */
	o_ptr = &inventory[INVEN_WIELD];
	xskill[SK_THN] = p_ptr->skill[SK_THN] + p_ptr->to_h + actual_to_h(o_ptr);

	/* Shooting Skill (with current bow) */
	o_ptr = &inventory[INVEN_BOW];
	xskill[SK_THB] = p_ptr->skill[SK_THB] + p_ptr->to_h + actual_to_h(o_ptr);;

	/* Throwing Skill */
	xskill[SK_THT] = p_ptr->skill[SK_THT] + p_ptr->to_h;

	/* Basic abilities */
	xskill[SK_DIS] = p_ptr->skill[SK_DIS];
	xskill[SK_BYP] = p_ptr->skill[SK_BYP];
	xskill[SK_DEV] = p_ptr->skill[SK_DEV];
	xskill[SK_SAV] = p_ptr->skill[SK_SAV];
	xskill[SK_STL] = p_ptr->skill[SK_STL];
	xskill[SK_SRH] = p_ptr->skill[SK_SRH];
	xskill[SK_FOS] = p_ptr->skill[SK_FOS];
	xskill[SK_DIG] = p_ptr->skill[SK_DIG];
	xskill[SK_ALC] = p_ptr->skill[SK_ALC];

	put_str("Saving Throw", 10, col);
	desc = likert(xskill[SK_SAV] - 10, 5);  
	c_put_str(likert_color, format("%9s", desc), 10, col+13);

	put_str("Stealth", 11, col);
	desc = likert(xskill[SK_STL], 1);
	c_put_str(likert_color, format("%9s", desc), 11, col+13);

	put_str("Fighting", 12, col);
	desc = likert(xskill[SK_THN], 4);
	c_put_str(likert_color, format("%9s", desc), 12, col+13);

	put_str("Shooting", 13, col);
	desc = likert(xskill[SK_THB], 4);
	c_put_str(likert_color, format("%9s", desc), 13, col+13);

	put_str("Throwing", 14, col);
	desc = likert(xskill[SK_THT], 4);
	c_put_str(likert_color, format("%9s", desc), 14, col+13);

	put_str("Disarming", 15, col);
	desc = likert(xskill[SK_DIS], 8);
	c_put_str(likert_color, format("%9s", desc), 15, col+13);

	put_str("Bypass Trap", 16, col);
	desc = likert(xskill[SK_BYP], 5);
	c_put_str(likert_color, format("%9s", desc), 16, col+13); 

	put_str("Magic Device", 17, col);
	desc = likert(xskill[SK_DEV], 6);
	c_put_str(likert_color, format("%9s", desc), 17, col+13);

	put_str("Searching", 18, col);
	desc = likert(xskill[SK_SRH], 6);
	c_put_str(likert_color, format("%9s", desc), 18, col+13);

	put_str("Search Freq.", 19, col);
	desc = likert(xskill[SK_FOS], 6);
	c_put_str(likert_color, format("%9s", desc), 19, col+13);

	put_str("Digging", 20, col);
	desc = likert(xskill[SK_DIG], 10);
	c_put_str(likert_color, format("%9s", desc), 20, col+13);

	put_str("Alchemy", 21, col);
	desc = likert(xskill[SK_ALC], 7);
	c_put_str(likert_color, format("%9s", desc), 21, col+13);
}

/*
 * Obtain the "flags" for the player as if he was an item
 */
void player_flags(u32b *f1, u32b *f2, u32b *f3)
{
	/* Clear */
	(*f1) = (*f2) = (*f3) = 0L;

	/* Add racial flags */
	(*f1) |= rp_ptr->flags1;
	(*f2) |= rp_ptr->flags2;
	(*f3) |= rp_ptr->flags3;

	if (rp_ptr->special)
	{
		(*f1) |= rsp_ptr[(p_ptr->max_lev)/5]->flags1;
		(*f2) |= rsp_ptr[(p_ptr->max_lev)/5]->flags2;
		(*f3) |= rsp_ptr[(p_ptr->max_lev)/5]->flags3;
	}

	if (cp_ptr->flags & CF_BRAVERY_30)
	{
		if (p_ptr->lev >= 30) (*f2) |= (TR2_BRAVERY);
	}
}

/*
 * Equippy chars
 */
void display_player_equippy(int y, int x)
{
	int i;

	byte a;
	char c;

	object_type *o_ptr;

	/* Dump equippy chars */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; ++i)
	{
		/* Object */
		o_ptr = &inventory[i];

		/* Get attr/char for display */
		a = object_attr(o_ptr);
		c = object_char(o_ptr);

		/* Clear the part of the screen */
		if (!o_ptr->k_idx)
		{
			c = ' ';
			a = TERM_WHITE;
		}

		/* Dump */
		Term_putch(x+i-INVEN_WIELD, y, a, c);
	}
}

typedef struct c_flag_desc
{
	byte		set;

	u32b		r_flag;
	u32b		i_flag;
	cptr		desc;
} c_flag_desc;

/*
 * Hack -- see below
 */
static const c_flag_desc flag_list[3][12] =
{
	{
		{2, TR2_NO_BLIND,			 0, "No  Blind:"},
		{2, TR2_FREE_ACT,			 0, "Free Actn:"},
		{2, TR2_HOLD_LIFE,			 0, "Hold Life:"},
		{2, TR2_BRAVERY,			 0, "  Bravery:"},
		{2, TR2_NO_DISEASE,			 0, "No Dsease:"},
		{2, TR2_NO_STUN,			 0, "No   Stun:"},
		{2, TR2_NO_POISON,		     0, "No Poison:"},
		{2, TR2_NO_CUT,				 0, "No    Cut:"},
		{2, TR2_NO_CONF,			 0, "No   Conf:"},
		{3, TR3_SLOW_DIGEST,		 0, "S. Digest:"},
		{3, TR3_FEATHER,			 0, "Fthr Fall:"},
		{99,0, 0,						"          "}
	},
	{
		{3, TR3_GLOW,	 TR3_LITE_MASK, "    Light:"},
		{3, TR3_REGEN,				 0, "    Regen:"},
		{3, TR3_TELEPATHY,			 0, "Telepathy:"},
		{3, TR3_SEE_INVIS,			 0, "See Invis:"},
		{3, TR3_INVIS,				 0, "Invisible:"},
		{3, TR3_LUCK,				 0,	"     Luck:"},
		{1, TR1_STEALTH,			 0, "  Stealth:"},
		{1, TR1_SEARCH,				 0, "   Search:"},
		{1, TR1_INFRA,				 0, "Infra-vsn:"},
		{1, TR1_TUNNEL,				 0, "Tunneling:"},
		{1, TR1_SPEED,				 0, "    Speed:"},
		{99,0, 0,						"          "}
	},
	{
		{1, TR1_BLOWS,				 0, "    Blows:"},
		{1, TR1_SHOTS,				 0, "    Shots:"},
		{1, TR1_MANA,				 0, "     Mana:"},
		{1, TR1_HEALTH,				 0, "   Health:"},
		{1, TR1_MIGHT,				 0, "    Might:"},
		{3,	TR3_DISRUPT,			 0, "  Disrupt:"},
		{3, TR3_DRAIN_ITEM,			 0, "Drn Items:"},
		{3, TR3_DRAIN_EXP,			 0, "Drn   Exp:"},
		{3, TR3_TELEPORT,			 0, " Teleport:"},
		{3, TR3_AGGRAVATE,			 0, "Aggravate:"},
		{99,0, 0,						"          "},
		{99,0, 0,						"          "}
	}
};

/*
 * Display the player flags on the top of the screen
 */
static void display_player_flag_info(void)
{
	int x, y, i, n;
	int row, col;

	cptr name;

	u32b f[4];

	object_type *o_ptr;

	byte attr1, attr2;

	/* Three columns */
	for (x = 0; x < 3; x++)
	{
		/* Reset */
		row = 1;
		col = 2 + (26 * x);

		/* Twelve rows */
		for (y = 0; y < 12; y++)
		{
			if (flag_list[x][y].set == 99) break;
				
			/* Extract name */
			name = flag_list[x][y].desc;

			/* Header */
			c_put_str(TERM_WHITE, name, row, col);

			if (flag_list[x][y].set > 0)
			{
				/* Check equipment */
				for (n = 10, i = INVEN_WIELD; i < INVEN_TOTAL; ++i, ++n)
				{
					attr1 = TERM_SLATE;
					attr2 = TERM_WHITE;

					/* Object */
					o_ptr = &inventory[i];

					/* different colour for unlit lanterns */
					if ((o_ptr->tval == TV_LITE) && (!o_ptr->timeout)) attr2 = TERM_SLATE;

					/* Known flags */
					object_flags_known(o_ptr, &f[1], &f[2], &f[3]);

					/* Color columns by parity */
					if (i % 2) attr1 = TERM_L_WHITE;

					/* Non-existant objects */
					if (!o_ptr->k_idx) attr1 = TERM_L_DARK;

					/* Hack -- Check immunities */
					if (f[flag_list[x][y].set] & flag_list[x][y].i_flag)
					{
						c_put_str(attr2, "*", row, col+n);
					}

					/* Check flags */
					else if (f[flag_list[x][y].set] & flag_list[x][y].r_flag)
					{
						c_put_str(attr2, "+", row, col+n);
					}

					/* Default */
					else
					{
						c_put_str(attr1, ".", row, col+n);
					}
				}

				/* Player flags */
				player_flags(&f[1], &f[2], &f[3]);

				/* Default */
				c_put_str(TERM_SLATE, ".", row, col+n);

				/* Check flags */
				if (f[flag_list[x][y].set] & flag_list[x][y].i_flag)
				{
					c_put_str(TERM_WHITE, "*", row, col+n);
				}
				else if (f[flag_list[x][y].set] & flag_list[x][y].r_flag) 
				{
					c_put_str(TERM_WHITE, "+", row, col+n);
				}

				n++;

				/* Temporary Flags */
				f[1]=p_ptr->tim_flag1;
				f[2]=p_ptr->tim_flag2;
				f[3]=p_ptr->tim_flag3;

				/* Default */
				c_put_str(TERM_SLATE, ".", row, col+n);

				/* Check flags */
				if (f[flag_list[x][y].set] & flag_list[x][y].i_flag)
				{
					c_put_str(TERM_WHITE, "*", row, col+n);
				}
				else if (f[flag_list[x][y].set] & flag_list[x][y].r_flag) 
				{
					c_put_str(TERM_WHITE, "+", row, col+n);
				}
			}

			/* Advance */
			row++;
		}
	}
}

/*
 * Display the player resistances on the bottom of the screen
 */
static void display_player_resists_info(void)
{
	int i, j, k, n, x;
	int row, col;
	object_type *o_ptr;

	byte attr1, attr2;

	c_put_str(TERM_WHITE, "Resistances:", 13, 2);

	for (i = 0, j = 0; i < RS_MAX; i++)
	{
		x = (j / (RS_MAX / 3));
		col = 2 + (26 * x);
		row = 14;

		/* Display */
		if (p_ptr->dis_res[i])
		{
			prt(format("%s:", resist_names_short[i]), row + (j % (RS_MAX / 3)), col);

			/* Check equipment */
			for (n = 10, k = INVEN_WIELD; k < INVEN_TOTAL; ++k, ++n)
			{
				attr1 = TERM_SLATE;
				attr2 = TERM_WHITE;

				/* Object */
				o_ptr = &inventory[k];

				/* different colour for unlit lanterns */
				if ((o_ptr->tval == TV_LITE) && (!o_ptr->timeout)) attr2 = TERM_SLATE;

				/* Color columns by parity */
				if (k % 2) attr1 = TERM_L_WHITE;

				/* Non-existant objects */
				if (!o_ptr->k_idx) attr1 = TERM_L_DARK;

				/* Check flags */
				if (object_resist_known(o_ptr, i))
				{
					c_put_str(attr2, "+", row + (j % (RS_MAX / 3)), col + n);
				}

				/* Default */
				else
				{
					c_put_str(attr1, ".", row + (j % (RS_MAX / 3)), col + n);
				}
			}

			/* Default */
			c_put_str(TERM_SLATE, ".", row + (j % (RS_MAX / 3)), col + n);

			/* Basic Resistance */
			if ((((rp_ptr->res[i] + cp_ptr->res[i]) * p_ptr->lev) / 50) > 0)
			{
				c_put_str(TERM_WHITE, "+", row + (j % (RS_MAX / 3)), col + n);
			}

			n++;

			/* Temporary Resist */

			/* Default */
			c_put_str(TERM_SLATE, ".", row + (j % (RS_MAX / 3)), col + n);

			/* Check flags */
			if (p_ptr->tim_res[i])
			{
				c_put_str(TERM_WHITE, "+", row + (j % (RS_MAX / 3)), col + n);
			}

			/* Resistance value */
			if (p_ptr->tim_res[i]) attr1 = TERM_L_BLUE;
			else if (p_ptr->dis_res[i] > resist_caps[i].normal) 
			{
				if (resist_caps[i].normal > rp_ptr->res[i]) attr1 = TERM_VIOLET;
				else attr1 = TERM_GREEN;
			}
			else if (p_ptr->dis_res[i] == resist_caps[i].normal) attr1 = TERM_GREEN;
			else if (p_ptr->dis_res[i] >= resist_caps[i].normal / 2) attr1 = TERM_L_GREEN;
			else attr1 = TERM_YELLOW;

			c_put_str(attr1, format("%d%%", p_ptr->dis_res[i]), row + (j % (RS_MAX / 3)), col + 6);

			j++;
		}
	}

	for (i = 0; i < 3; i++)
	{
		/* Footer */
		if (cp_ptr->flags & CF_MUSIC) 
			c_put_str(TERM_WHITE, "abcdefghijklm@*", 20, 12 + (26 * i));
		else c_put_str(TERM_WHITE, "abcdefghijkl@*", 20, 12 + (26 * i));

		/* Equippy */
		display_player_equippy(21, 12 + (26 * i));
	}
}	

/*
 * Special display, part 2a
 */
static void display_player_misc_info(void)
{
	int col;
	cptr p;

	char buf[80];

	/* Name */
	put_str("Name", 1, 1);
	c_put_str(TERM_L_BLUE, op_ptr->full_name, 1, 8);

	/* Sex */
	put_str("Sex", 2, 1);
	c_put_str(TERM_L_BLUE, sp_ptr->title, 2, 8);

	/* Race */
	put_str("Race", 3, 1);
	if (!rp_ptr->special) p=p_name + rp_ptr->name;
		else p=rsp_ptr[(p_ptr->max_lev)/5]->name;
	c_put_str(TERM_L_BLUE, p, 3, 8);

	/* Class */
	put_str("Class", 4, 1);
	c_put_str(TERM_L_BLUE, c_name + cp_ptr->name, 4, 8);

	/* Title */
	put_str("Title", 5, 1);

	/* Winner */
	if (p_ptr->total_winner || (p_ptr->lev > PY_MAX_LEVEL))
	{
		p = "***WINNER***";
	}

	/* Normal */
	else
	{
#ifndef PREVENT_LOAD_C_TEXT
		p = c_text+cp_ptr->title[(p_ptr->lev-1)/5];
#else /* PREVENT_LOAD_C_TEXT */
		p = " ";
#endif /* PREVENT_LOAD_C_TEXT */
	}

	/* Dump it */
	c_put_str(TERM_L_BLUE, p, 5, 8);

	/* Hit Points */
	put_str("HP", 6, 1);
	sprintf(buf, "%d/%d", p_ptr->chp, p_ptr->mhp);
	c_put_str(TERM_L_BLUE, buf, 6, 8);

	/* Spell Points */
	put_str("SP", 7, 1);
	sprintf(buf, "%d/%d", p_ptr->csp, p_ptr->msp);
	c_put_str(TERM_L_BLUE, buf, 7, 8);

	col = 21;

	/* Age */
	Term_putstr(col, 2, -1, TERM_WHITE, "Age");
	Term_putstr(col+14, 2, -1, TERM_L_BLUE, format("%4d", (int)p_ptr->age));

	/* Height */
	Term_putstr(col, 3, -1, TERM_WHITE, "Height");
	Term_putstr(col+14, 3, -1, TERM_L_BLUE, format("%4d", (int)p_ptr->ht));

	/* Weight */
	Term_putstr(col, 4, -1, TERM_WHITE, "Weight");
	Term_putstr(col+14, 4, -1, TERM_L_BLUE, format("%4d", (int)p_ptr->wt));

	/* Status */
	Term_putstr(col, 5, -1, TERM_WHITE, "Status");
	Term_putstr(col+14, 5, -1, TERM_L_BLUE, format("%4d", (int)p_ptr->sc));

   	/* Deepest recall level */
   	Term_putstr(col, 7, -1, TERM_WHITE, "Max Depth");
   	if (!depth_in_feet) 
	{
		Term_putstr(col+14, 7, -1, TERM_L_BLUE, format("%4d", (int)p_ptr->max_depth));
	}
	else 
	{
		Term_putstr(col+11, 7, -1, TERM_L_BLUE, format("%4d Ft", (int)p_ptr->max_depth * 50));
	}
}

/*
 * Stat display used in birth display, part 2b
 */
static void display_player_stat_birth(void)
{
	int i, row, col, n;

	char buf[80];

	/* Row */
	row = 2;

	/* Column */
	col = 41;

	/* Print out the labels for the columns */
	c_put_str(TERM_WHITE, "CB", row-1, col + 6);
	c_put_str(TERM_WHITE, "Base", row-1, col + 10);
	c_put_str(TERM_WHITE, "RB", row-1, col + 16);
	c_put_str(TERM_WHITE, "Stat", row-1, col + 20);

	/* Display the stats */
	for (i = 0; i < A_MAX; i++)
	{
		/* Assume uppercase stat name */
		put_str(stat_names[i], row+i, col);

		/* Indicate natural maximum */
		if (p_ptr->stat_max[i] == 20)
		{
			put_str("!", row+i, col+3);
		}

		/* Class Bonus */
		sprintf(buf, "(%+d)", cp_ptr->c_adj[i]);
		c_put_str(TERM_SLATE, buf, row+i, col + 5);

		/* Internal "natural" maximum value */
		sprintf(buf, "%2d", p_ptr->stat_max[i]);
		c_put_str(TERM_L_BLUE, buf, row+i, col + 11);

		/* Race Bonus */
		if (!rp_ptr->special) n=rp_ptr->r_adj[i];
		else n=rsp_ptr[(p_ptr->max_lev)/5]->r_adj[i]+rp_ptr->r_adj[i];
		sprintf(buf, "%+2d", n);
		c_put_str(TERM_L_BLUE, buf, row+i, col+16);

		/* Resulting "modified" maximum value */
		sprintf(buf, "%2d", p_ptr->stat_top[i]);
		c_put_str(TERM_L_GREEN, buf, row+i, col+21);
	}
}

/*
 * Stat display used in birth display, part 2b
 */
static void display_player_stat_dump(void)
{
	int i, row, col, n;

	char buf[80];

	/* Row */
	row = 2;

	/* Column */
	col = 41;

	/* Print out the labels for the columns */
	c_put_str(TERM_WHITE, "CB", row-1, col + 6);
	c_put_str(TERM_WHITE, "Base", row-1, col + 10);
	c_put_str(TERM_WHITE, "RB", row-1, col + 16);
	c_put_str(TERM_WHITE, "EB", row-1, col + 20);
	c_put_str(TERM_WHITE, "Stat", row-1, col + 24);

	/* Display the stats */
	for (i = 0; i < A_MAX; i++)
	{
		/* Assume uppercase stat name */
		put_str(stat_names[i], row+i, col);

		/* Indicate natural maximum */
		if (p_ptr->stat_max[i] == 20)
		{
			put_str("!", row+i, col+3);
		}

		/* Class Bonus */
		sprintf(buf, "(%+d)", cp_ptr->c_adj[i]);
		c_put_str(TERM_SLATE, buf, row+i, col + 5);

		/* Internal "natural" maximum value */
		sprintf(buf, "%2d", p_ptr->stat_max[i]);
		c_put_str(TERM_L_BLUE, buf, row+i, col + 11);

		/* Race Bonus */
		if (!rp_ptr->special) n=rp_ptr->r_adj[i];
		else n=rsp_ptr[(p_ptr->max_lev)/5]->r_adj[i]+rp_ptr->r_adj[i];
		sprintf(buf, "%+2d", n);
		c_put_str(TERM_L_BLUE, buf, row+i, col+16);

		/* Equipment Bonus */
		sprintf(buf, "%+3d", p_ptr->stat_add[i]);
		c_put_str(TERM_L_BLUE, buf, row+i, col+19);

		/* Resulting "modified" maximum value */
		sprintf(buf, "%2d", p_ptr->stat_top[i]);
		c_put_str(TERM_L_GREEN, buf, row+i, col+25);
	}
}

/*
 * Special display, part 2c
 *
 * How to print out the modifications and sustains.
 * Positive mods with no sustain will be light green.
 * Positive mods with a sustain will be dark green.
 * Sustains (with no modification) will be a dark green 's'.
 * Negative mods (from a curse) will be red.
 * Huge mods (>9), like from MICoMorgoth, will be a '*'
 * No mod, no sustain, will be a slate '.'
 */
static void display_player_stat_info(void)
{
	int i, n, row, col, stat;

	char buf[80];

	object_type *o_ptr;
	u32b f1, f2, f3;
	u32b ignore_f2, ignore_f3;
	u32b hack_f1;

	byte a;
	char c;

	/* Row */
	row = 2;

	/* Column */
	col = 41;

	/* Header */
	if (cp_ptr->flags & CF_MUSIC) c_put_str(TERM_WHITE, "abcdefghijklm@", row-1, col+11);
	else c_put_str(TERM_WHITE, "abcdefghijkl@", row-1, col+11);
	c_put_str(TERM_WHITE, " Base", row-1, col+5);
	c_put_str(TERM_WHITE, " Stat", row-1, col+26);

	/* Display the stats */
	for (i = 0; i < A_MAX; i++)
	{
		/* Reduced */
		if (p_ptr->stat_use[i] < p_ptr->stat_top[i])
		{
			/* Use lowercase stat name */
			put_str(stat_names_reduced[i], row+i, col);
		}

		/* Normal */
		else
		{
			/* Assume uppercase stat name */
			put_str(stat_names[i], row+i, col);
		}

		/* Indicate natural maximum */
		if (p_ptr->stat_max[i] == 20)
		{
			put_str("!", row+i, col+3);
		}

		/* Internal "natural" maximum value */
		sprintf(buf, "%2d", p_ptr->stat_max[i]);
		c_put_str(TERM_L_GREEN, buf, row+i, col+7);

		/* Resulting "modified" maximum value */
		sprintf(buf, "%2d", p_ptr->stat_top[i]);
		c_put_str(TERM_L_GREEN, buf, row+i, col+28);

		/* Only display stat_use if not maximal */
		if (p_ptr->stat_use[i] < p_ptr->stat_top[i])
		{
			byte a = ((p_ptr->stat_use[i] > 0) && (p_ptr->stat_cur[i] > 0)) 
				? TERM_YELLOW : TERM_RED;

			sprintf(buf, "%2d", p_ptr->stat_use[i]);
			c_put_str(a, buf, row+i, col+33);
		}
	}
		
	/* Process equipment */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; ++i)
	{
		/* Get the object */
		o_ptr = &inventory[i];

		/* Get the "known" flags */
		object_flags_known(o_ptr, &f1, &f2, &f3);

		/* Hack -- assume stat modifiers are known */
		object_flags(o_ptr, &hack_f1, &ignore_f2, &ignore_f3);
		hack_f1 &= TR1_PVAL_MASK;
		f1 |= hack_f1;

		/* Initialize color based of sign of pval. */
		for (stat = 0; stat < A_MAX; stat++)
		{
			/* Default */
			a = TERM_SLATE;
			c = '.';

			/* Boost */
			if (f1 & (1<<stat))
			{
				/* Default */
				c = '*';

				/* Good */
				if (o_ptr->pval > 0)
				{
					/* Good */
					a = TERM_L_GREEN;

					/* Label boost */
					if (o_ptr->pval < 10) c = I2D(o_ptr->pval);
				}

				/* Bad */
				if (o_ptr->pval < 0)
				{
					/* Bad */
					a = TERM_RED;

					/* Label boost */
					if (o_ptr->pval > -10) c = I2D(-(o_ptr->pval));
				}
			}

			/* Sustain */
			if (f1 & (1 << (stat + 26)))
			{
				/* Dark green */
				a = TERM_GREEN;

				/* Convert '.' to 's' */
				if (c == '.') c = 's';
			}

			/* Hack - unlit lanterns */
			if ((o_ptr->tval == TV_LITE) && (!o_ptr->timeout)) a = TERM_SLATE;

			/* Dump proper character */
			Term_putch(col+11, row+stat, a, c);
		}

		/* Advance */
		col++;
	}

	/* Player flags */
	player_flags(&f1, &f2, &f3);

	/* Check stats */
	for (stat = 0; stat < A_MAX; ++stat)
	{
		/* Default */
		a = TERM_SLATE;
		c = '.';

		/* Race bonus */
		if (!rp_ptr->special) n = rp_ptr->r_adj[stat];
		else n = rsp_ptr[(p_ptr->max_lev) / 5]->r_adj[stat] + rp_ptr->r_adj[stat];
	
		/* Good */
		if (n > 0)
		{
			/* Good */
			a = TERM_L_GREEN;

				/* Label boost */
				if (n < 10) c = I2D(n);
				else c = '*';
		}

		/* Bad */
		if (n < 0)
		{
			/* Bad */
			a = TERM_RED;

				/* Label boost */
			if (n > -10) c = I2D(-(n));
			else c = '*';
		}

		/* Sustain */
		if (f1 & (1<<(stat+26)))
		{
			/* Dark green */
			a = TERM_GREEN;

			/* Convert '.' to 's' */
			if (c == '.') c = 's';
		}

		/* Dump */
		Term_putch(col+11, row+stat, a, c);
	}

	/* Column */
	col = 41;

	/* Footer */
	if (cp_ptr->flags & CF_MUSIC) c_put_str(TERM_WHITE, "abcdefghijklm@", row+6, col+11);
	else c_put_str(TERM_WHITE, "abcdefghijkl@", row+6, col+11);

	/* Equippy */
	display_player_equippy(row+7, col+11);
}

/*
 * Display the character on the screen (several different modes)
 *
 * The top two lines, and the bottom line (or two) are left blank.
 *
 * Mode 0 = display with skills/history
 * Mode 1 = display with resistances / equipment flags
 * Mode 98 = special display with equipment bonuses for char dumps
 * Mode 99 = special display with class bonuses for birth 
 */
void display_player(byte mode)
{
	/* Erase screen */

	clear_from(0);

	/* Resistances */
	if (mode == 1)
	{
		display_player_flag_info();
		display_player_resists_info();
	}

	/* Standard */
	else
	{
		if (mode == 99) display_player_stat_birth();
		else if (mode == 98) display_player_stat_dump();
		else display_player_stat_info();
		display_player_misc_info();

		/* Extra info */
		display_player_xtra_info();
		display_player_skill_info();
	}
}

/*
 * Prints some status information on the screen.
 */
void display_player_status(void)
{
	int h,w;
	int col = 0;
	int row = 0;

	(void)Term_get_size(&w, &h);

	/* Erase screen */
	clear_from(0);

	/* Left column */
	put_str("Current Effects:", row++, col);

	/* Shield */
	if (p_ptr->shield)
	{
		c_put_str(TERM_L_GREEN, "Shield", row, col);
		row++;
	}

	/* Blessing */
	if (p_ptr->blessed)
	{
		c_put_str(TERM_L_GREEN, "Blessed", row, col);
		row++;
	}

	/* Heroism */
	if (p_ptr->hero)
	{
		c_put_str(TERM_L_GREEN, "Heroism", row, col);
		row++;
	}

	/* Stability */
	if (p_ptr->stability)
	{
		c_put_str(TERM_L_GREEN, "Stability", row, col);
		row++;
	}

	/* Berserk */
	if (p_ptr->rage)
	{
		c_put_str(TERM_L_GREEN, "Berserk", row, col);
		row++;
	}

	/* Protection from evil */
	if (p_ptr->protevil)
	{
		c_put_str(TERM_L_GREEN, "Prot. Evil", row, col);
		row++;
	}

	/* Resilient */
	if (p_ptr->resilient)
	{
		c_put_str(TERM_L_GREEN, "Resilient", row, col);
		row++;
	}

	/* Fast */
	if (p_ptr->fast)
	{
		c_put_str(TERM_L_GREEN, "Fast", row, col);
		row++;
	}

	/* Absorb hit */
	if (p_ptr->absorb)
	{
		c_put_str(TERM_L_GREEN, "Absorb Hit", row, col);
		row++;
	}

	/* Right column */
	if (row > 1) col = 14; /* use left column if no good effects */
	row = 1;

	/* Blindness */
	if (p_ptr->blind) 
	{
		c_put_str(TERM_L_RED, "Blind", row, col);
		row++;
	}

	/* Confusion */
	if (p_ptr->confused > PY_CONF_INSANE)
	{
		c_put_str(TERM_L_RED, "Insane", row, col);
		row++;
	}
	else if (p_ptr->confused > PY_CONF_BEFUDDLE)
	{
		c_put_str(TERM_L_RED, "Befuddled", row, col);
		row++;
	}
	else if (p_ptr->confused > PY_CONF_CONFUSE)
	{
		c_put_str(TERM_L_RED, "Confused", row, col);
		row++;
	}
	else if (p_ptr->confused)
	{
		c_put_str(TERM_L_RED, "Perplexed", row, col);
		row++;
	}

	/* Poison */
	if (p_ptr->poisoned)
	{
		c_put_str(TERM_L_RED, "Poisoned", row, col);
		row++;
	}

	/* Diseased */
	if (p_ptr->diseased)
	{
		c_put_str(TERM_L_RED, "Diseased", row, col);
		row++;
	}

	/* Fear */
	if (p_ptr->afraid > PY_FEAR_PANIC)
	{
		c_put_str(TERM_L_RED, "Panic", row, col);
		row++;
	}
	else if (p_ptr->afraid > PY_FEAR_TERROR)
	{
		c_put_str(TERM_L_RED, "Terrified", row, col);
		row++;
	}
	else if (p_ptr->afraid > PY_FEAR_AFRAID)
	{
		c_put_str(TERM_L_RED, "Afraid", row, col);
		row++;
	}
	else if (p_ptr->afraid)
	{
		c_put_str(TERM_L_RED, "Wary", row, col);
		row++;
	}

	/* Paralyzation */
	if (p_ptr->paralyzed) 
	{
		c_put_str(TERM_L_RED, "Paralyzed", row, col);
		row++;
	}

	/* Hallucination */
	if (p_ptr->image)
	{
		c_put_str(TERM_L_RED, "Hallucination", row, col);
		row++;
	}
	
	/* Slow */
	if (p_ptr->slow)
	{
		c_put_str(TERM_L_RED, "Slowed", row, col);
		row++;
	}

	/* Stunning */
	if (p_ptr->stun > PY_STUN_KO)
	{
		c_put_str(TERM_L_RED, "Knocked Out", row, col);
		row++;
	}
	else if (p_ptr->stun > PY_STUN_HEAVY)
	{
		c_put_str(TERM_L_RED, "Heavy Stun", row, col);
		row++;
	}
	else if (p_ptr->stun)
	{
		c_put_str(TERM_L_RED, "Stunned", row, col);
		row++;
	}

	/* Cuts */
	if (p_ptr->cut > PY_CUT_MORTAL)
	{
		c_put_str(TERM_L_RED, "Mortal wound", row, col);
		row++;
	}
	else if (p_ptr->cut > PY_CUT_DEEP)
	{
		c_put_str(TERM_L_RED, "Deep gash   ", row, col);
		row++;
	}
	else if (p_ptr->cut > PY_CUT_SEVERE)
	{
		c_put_str(TERM_L_RED, "Severe cut  ", row, col);
		row++;
	}
	else if (p_ptr->cut > PY_CUT_NASTY)
	{
		c_put_str(TERM_L_RED, "Nasty cut   ", row, col);
		row++;
	}
	else if (p_ptr->cut > PY_CUT_BAD)
	{
		c_put_str(TERM_L_RED, "Bad cut     ", row, col);
		row++;
	}
	else if (p_ptr->cut > PY_CUT_LIGHT)
	{
		c_put_str(TERM_L_RED, "Light cut   ", row, col);
		row++;
	}
	else if (p_ptr->cut)
	{
		c_put_str(TERM_L_RED, "Graze", row, col);
		row++;
	}

	if (!col && (row == 1))
	put_str("None", row++, col);

}

/*
 * Pair together a constant flag with a textual description.
 *
 * Note that it sometimes more efficient to actually make an array
 * of textual names, where entry 'N' is assumed to be paired with
 * the flag whose value is "1L << N", but that requires hard-coding.
 */
typedef struct o_flag_desc
{
	u32b flag;
	cptr desc;
} o_flag_desc;

/*
 * These are used for "+3 to STR, DEX", etc. These are separate from
 * the other pval affected traits to simplify the case where an object
 * affects all stats.  In this case, "All stats" is used instead of
 * listing each stat individually.
 */
static const o_flag_desc stat_flags_desc[A_MAX] =
{
	{ TR1_STR,        "strength" },
	{ TR1_INT,        "intelligence" },
	{ TR1_WIS,        "wisdom" },
	{ TR1_DEX,        "dexterity" },
	{ TR1_CON,        "constitution" },
	{ TR1_CHR,        "charisma" }
};

/*
 * Besides stats, these are the other player traits
 * which may be affected by an object's pval
 */
static const o_flag_desc pval_flags1_desc[] =
{
	{ TR1_STEALTH,    "stealth" },
	{ TR1_SEARCH,     "searching" },
	{ TR1_INFRA,      "infravision" },
	{ TR1_TUNNEL,     "tunneling" },
	{ TR1_SPEED,      "speed" },
	{ TR1_BLOWS,      "attacks" },
	{ TR1_SHOTS,      "shots" },
	{ TR1_MIGHT,      "might" },
	{ TR1_MANA,       "mana" },
	{ TR1_HEALTH,     "health" }
};

/*
 * Elemental brands for weapons
 */
static const o_flag_desc weapon_flags_desc[] =
{
	{ TR2_WOUNDING,		"cause bleeding" },
	{ TR2_TERROR,		"terrify the enemy" },
	{ TR2_IMPACT,		"cause an earthquake" }
};

/*
 * Slay names 
 */
cptr slay_names[SL_MAX] =
{
	"vs. evil",
	"vs. chaotic",
	"vs. animals",
	"vs. plants",
	"vs. undead",
	"vs. demons",
	"vs. humanoids",
	"vs. people",
	"vs. faeries",
	"vs. dragons",
	"vs. lycanthropes",
	"acid brand",
	"lightning brand",
	"fire brand",
	"cold brand",
	"poison brand",
	"light brand",
	"dark brand"
};

/*
 * Sustain stats -  these are given their "own" line in the
 * spoiler file, mainly for simplicity
 */
static const o_flag_desc sustain_flags_desc[] =
{
	{ TR1_SUST_STR,   "strength" },
	{ TR1_SUST_INT,   "intelligence" },
	{ TR1_SUST_WIS,   "wisdom" },
	{ TR1_SUST_DEX,   "dexterity" },
	{ TR1_SUST_CON,   "constitution" },
	{ TR1_SUST_CHR,   "charisma" }
};


static const o_flag_desc imm_flags2_desc[] =
{
	{ TR2_FREE_ACT,		"paralyzation" },
	{ TR2_HOLD_LIFE,	"experience drain" },
	{ TR2_BRAVERY,		"fear" },
	{ TR2_NO_BLIND,		"blindness" },
	{ TR2_NO_DISEASE,	"disease" },
	{ TR2_NO_STUN,		"stunning" },
	{ TR2_NO_POISON,	"poisoning" },
	{ TR2_NO_CUT,		"cuts" },
	{ TR2_NO_CONF,		"confusion" }
};

/*
 * Miscellaneous magic given by an object's "flags3" field
 */
static const o_flag_desc misc_flags3_desc[] =
{
	{ TR3_SLOW_DIGEST,	"slow digestion" },
	{ TR3_FEATHER,		"feather falling" },
	{ TR3_GLOW,			"brighter light" },
	{ TR3_REGEN,		"regeneration" },
	{ TR3_TELEPATHY,	"telepathy" },
	{ TR3_SEE_INVIS,	"see invisible" },
	{ TR3_INVIS,		"invisibility" },
	{ TR3_LUCK,			"luck finding items" },
};

/*
 * "Bad" magic given by an object's "flags3" field
 *
 * Note that cursed artifacts and objects with permanent light
 * are handled "directly" -- see analyze_misc_magic()
 */
static const o_flag_desc bad_flags3_desc[] =
{
	{ TR3_DISRUPT,		"spellcasting disruption" },
	{ TR3_TELEPORT,		"random teleportation" },
	{ TR3_AGGRAVATE,	"aggravation" },
	{ TR3_DRAIN_EXP,	"experience drain" },
	{ TR3_DRAIN_ITEM,	"item drain" }
};

/*
 * This function does most of the actual "analysis". Given a set of bit flags
 * (which will be from one of the flags fields from the object in question),
 * a "flag description structure", a "description list", and the number of
 * elements in the "flag description structure", this function sets the
 * "description list" members to the appropriate descriptions contained in
 * the "flag description structure".
 *
 * The possibly updated description pointer is returned.
 */

static cptr *spoiler_flag_aux(const u32b art_flags, const o_flag_desc *flag_x_ptr,
                              cptr *desc_x_ptr, const int n_elmnts)
{
	int i;

	for (i = 0; i < n_elmnts; ++i)
	{
		if (art_flags & flag_x_ptr[i].flag)
		{
			*desc_x_ptr++ = flag_x_ptr[i].desc;
		}
	}

	return desc_x_ptr;
}

/*
 * Does an item ignore the damage from fire? 
 */
static bool spoiler_ignore_damage(bool spoil, object_type *o_ptr, int res_type, int slay_type)
{
	u32b f1, f2, f3;
	byte slays[SL_MAX];
	int j;

	/* Resistance */
	if (spoil) j = object_resist(o_ptr, res_type);
	else j = object_resist_known(o_ptr, res_type);
	if (j > RST_IGNORE_ELEM) return TRUE;

	/* Brand */
	if (spoil) weapon_slays(o_ptr, slays);
	else weapon_slays_known(o_ptr, slays);

	if (slays[slay_type] > 10) return TRUE;

	/* Ignores all elements */
	if (spoil) object_flags(o_ptr, &f1, &f2, &f3);
	else object_flags_known(o_ptr, &f1, &f2, &f3);

	if (f3 & TR3_IGNORE_ELEM) return TRUE;

	return FALSE;
}

/*  
 * This is a special function that lists what can damage an item 
 */
static cptr *spoiler_damage_aux(bool spoil, object_type *o_ptr, cptr *desc_x_ptr)
{
	u32b f1, f2, f3;

	/* Extract the flags */
	if (spoil) object_flags(o_ptr, &f1, &f2, &f3);
	else object_flags_known(o_ptr, &f1, &f2, &f3);

	if (hates_acid(o_ptr) && !spoiler_ignore_damage(spoil, o_ptr, RS_ACD, SL_BRAND_ACID))
		*desc_x_ptr++ = "acid";
	if (hates_elec(o_ptr) && !spoiler_ignore_damage(spoil, o_ptr, RS_ELC, SL_BRAND_ELEC))
		*desc_x_ptr++ = "electricity";
	if (hates_fire(o_ptr) && !spoiler_ignore_damage(spoil, o_ptr, RS_FIR, SL_BRAND_FIRE))
		*desc_x_ptr++ = "fire";
	if (hates_cold(o_ptr) && !spoiler_ignore_damage(spoil, o_ptr, RS_CLD, SL_BRAND_COLD))
		*desc_x_ptr++ = "cold";
	if (hates_rust(o_ptr) && !(f3 & TR3_IGNORE_NON_ELEM)) *desc_x_ptr++ = "rusting attacks";
	if (hates_rot(o_ptr) && !(f3 & TR3_IGNORE_NON_ELEM)) *desc_x_ptr++ = "rotting attacks";

	if (wearable_p(o_ptr))
	{
		if ((o_ptr->tval != TV_RING) && (o_ptr->tval != TV_AMULET) && 
			(o_ptr->tval != TV_LITE) && (o_ptr->tval != TV_LITE_SPECIAL) && 
			(o_ptr->tval != TV_MUSIC) && (!(f3 & TR3_IGNORE_DISEN))) 
			*desc_x_ptr++ = "disenchantment";
	}

	return desc_x_ptr;
}

/*
 * Hack -- Display the "name" and "attr/chars" of a monster race
 */
static void obj_top(object_type *o_ptr, bool real)
{
	char name[80];

	if (real) object_desc(name, o_ptr, TRUE, 1);
	else object_desc_store(name, o_ptr, FALSE, 0);

	/* Clear the top line */
	Term_erase(0, 0, 255);

	/* Reset the cursor */
	Term_gotoxy(0, 0);

	/* Dump the name */
	Term_addstr(-1, TERM_L_BLUE, name);
}

/*
 * Display an object at the top of the screen
 */
void screen_object(object_type *o_ptr, bool real)
{
	/* Set text_out hook */
	text_out_hook = text_out_to_screen;

	/* Load screen */
	screen_save();

	/* Begin recall */
	Term_gotoxy(0, 1);

	/* Actually display the item */
	list_object(o_ptr, OBJECT_INFO_KNOWN);

	/* Display object history */
	if (real) display_object_history(o_ptr);

	/* Display item name */
	obj_top(o_ptr, real);

	(void) inkey();
	
	/* Load screen */
	screen_load();
}

/*
 * This function displays lists of properties
 */
static bool outlist(cptr header, const cptr *list)
{
	/* Ignore an empty list */
	if (*list == NULL) return (FALSE);

	/* Create header (if one was given) */
	if (header && (header[0]))
	{
		text_out(header);
		text_out(" ");
	}

	/* Now begin the tedious task */
	while (1)
	{
		/* Print the current item */
		text_out(*list);

		/*
		 * If there is an item following this one, pad with separator and a space
		 */
		if (list[1])
		{
			/* If there are two items, use a comma. */
			if (list[2]) text_out(", ");
			/* Otherwise, use "and" */
			else text_out(" and ");
		}

		/* Advance, with break */
		if (!*++list) break;
	}

	/* End the current list */
	text_out(".  ");

	/* Something was printed */
	return (TRUE);
}

/* 
 * Create a spoiler file entry for an artifact 
 */
void list_object(object_type *o_ptr, int mode)
{
	bool spoil = (mode == OBJECT_INFO_FULL) ? TRUE : FALSE;
	bool random = (mode == OBJECT_INFO_RANDOM) ? TRUE : FALSE;

	const u32b all_stats = (TR1_STR | TR1_INT | TR1_WIS |
							TR1_DEX | TR1_CON | TR1_CHR);
	const u32b all_sustains = (TR1_SUST_STR | TR1_SUST_INT | TR1_SUST_WIS |
							   TR1_SUST_DEX | TR1_SUST_CON | TR1_SUST_CHR);
	
	bool any_res = FALSE;  /* Printed any resistance */
	bool any_slay = FALSE; /* Printed any slay */
	bool anything = FALSE; /* Printed anything at all */

	int i, j;
	int prev_idx, prev;
	char buf[80];
	cptr list[40];
	cptr *list_ptr;

	byte slays[SL_MAX];

	bool do_act;

	u32b f1, f2, f3;

	/* Extract the flags */
	object_flags_aux(mode, o_ptr, &f1, &f2, &f3);

	/* Hack - Never show ignores for artifacts, rods or talismans (they're assumed) */
	if (artifact_p(o_ptr)) f3 &= ~(TR3_IGNORE_ELEM | TR3_IGNORE_NON_ELEM);

	/* Light sources */
	if (!random && ((o_ptr->tval == TV_LITE_SPECIAL) || (o_ptr->tval == TV_LITE)))
	{
		byte rad = 0;

		if (f3 & TR3_LITE1) rad = 1;
		if (f3 & TR3_LITE2) rad = 2;
		if (f3 & TR3_LITE3) rad = 3;
		if (f3 & TR3_LITE4) rad = 4;

		if (rad) 
		{
			if (o_ptr->tval == TV_LITE_SPECIAL) text_out("Permanent light-source (");
			else text_out("Refuelable light-source (");

			text_out_c(TERM_YELLOW, format("radius %d", rad));
			text_out(").  ");
			anything = TRUE;
		}
	}

	/* Quest items */
	if (!random && (o_ptr->tval == TV_QUEST))
	{
		text_out("An item of legend, it serves no practical use, but is of much value to collectors.  ");
	}

	/* Pval-affected flags */
	if (f1)
	{
		list_ptr = list;

		/* First, check to see if the pval affects all stats */
		if ((f1 & all_stats) == all_stats)
		{
			*list_ptr++ = "all stats";
		}

		/* Are any stats affected? */
		else if (f1 & all_stats)
		{
			list_ptr = spoiler_flag_aux(f1, stat_flags_desc, list_ptr,
											N_ELEMENTS(stat_flags_desc));
		}

		/* And now the "rest" */
		list_ptr = spoiler_flag_aux(f1, pval_flags1_desc, list_ptr,
										N_ELEMENTS(pval_flags1_desc));

		/* Terminate the description list */
		*list_ptr = NULL;

		/* Print the Pval */
		if (!(*list == NULL))
		{
			if (o_ptr->pval > 0) 
			{
				text_out_c(TERM_L_GREEN,format("+%d ", o_ptr->pval));
				text_out("to ");
			}
			else if (o_ptr->pval < 0) 
			{
				text_out_c(TERM_RED,format("%d ", o_ptr->pval));
				text_out("to ");
			}
			else text_out("It modifies ");
		
			anything |= outlist(NULL, list);
		}
	}

	/* Sustains */
	if (f1 & all_sustains)
	{
		list_ptr = list;

		/* Simplify things if an item sustains all stats */
		if ((f1 & all_sustains) == all_sustains)
		{
			*list_ptr++ = "all stats";
		}

		/* Should we bother? */
		else if ((f1 & all_sustains))
		{
			list_ptr = spoiler_flag_aux(f1, sustain_flags_desc, list_ptr,
											N_ELEMENTS(sustain_flags_desc));
		}

		/* Terminate the description list */
		*list_ptr = NULL;

		/* Sustains */
		anything |= outlist("It sustains", list);
	}

	/* Resistances */
	for (i = 0, prev_idx = -1, prev = 0; i < RS_MAX; i++)
	{
		/* Extract the flags */
		if (spoil) j = object_resist(o_ptr, i);
		else j = object_resist_known(o_ptr, i);;

		/* In case of random abilities, only display random resistances */
		if (random)
		{
			if (!(((o_ptr->xtra1 == OBJECT_XTRA_TYPE_MID_RESIST) ||
			(o_ptr->xtra1 == OBJECT_XTRA_TYPE_MID_RESIST)) &&
			(o_ptr->xtra2 == i))) j = 0;
		}

		if (j)
		{
			/* Print the previous resistance */
			if (prev_idx >= 0)
			{
				if (!any_res)
				{
					text_out("It increases resistances: ");
					any_res = TRUE;
					anything = TRUE;
				}
				else text_out(", ");

				text_out_c(TERM_ORANGE, format("+%d%% ", prev));
				text_out(format("%s", resist_names[prev_idx]));
			}

			prev_idx = i;
			prev = j;
		}
	}
	/* Print the final resistance */
	if (prev_idx >= 0)
	{
		if (!any_res)
		{
			text_out("It increases resistances: ");
			any_res = TRUE;
			anything = TRUE;
		}
		else text_out(" and ");

		text_out_c(TERM_ORANGE, format("+%d%% ", prev));
		text_out(format("%s.  ", resist_names[prev_idx]));
	}
	
	/* Weapon flags */
	if (f2)
	{
		list_ptr = list;

		list_ptr = spoiler_flag_aux(f2, weapon_flags_desc, list_ptr,
	                              N_ELEMENTS(weapon_flags_desc));

		/* Terminate the description list */
		*list_ptr = NULL;
		
		/* Weapon flags */
		anything |= outlist("A hit from this weapon can", list);

		/* Note that blessed weapons have special treatment */
		if (f2 & TR2_BLESSED) 
			text_out("It has been blessed by the gods.  ");

	}

	/* Slays/Brands */
	if (spoil) weapon_slays(o_ptr, slays);
	else weapon_slays_known(o_ptr, slays);

	/* Slays */
	for (i = 0, prev_idx = -1; i < SL_MAX; i++)
	{
		if (!random && slays[i])
		{
			/* Print the previous slay */
			if (prev_idx >= 0)
			{
				if (!any_slay)
				{
					any_slay = TRUE;
					anything = TRUE;
					text_out("It modifies damage: ");
				}
				else text_out(", ");

				if (slays[prev_idx] % 10) 
					text_out_c(TERM_L_RED, 
					format("x%d.%d", slays[prev_idx] / 10, slays[prev_idx] % 10));
				else text_out_c(TERM_L_RED, format("x%d", slays[prev_idx] / 10));
				text_out(format(" %s",  slay_names[prev_idx]));
			}

			prev_idx = i;
		}
	}
	/* Print the final slay */
	if (prev_idx >= 0)
	{
		if (!any_slay)
		{
			any_slay = TRUE;
			anything = TRUE;
			text_out("It modifies damage: ");
		}
		else text_out(" and ");

		if (slays[prev_idx] % 10) 
			text_out_c(TERM_L_RED, 
			format("x%d.%d", slays[prev_idx] / 10, slays[prev_idx] % 10));
		else text_out_c(TERM_L_RED, format("x%d", slays[prev_idx] / 10));
		text_out(format(" %s.  ",  slay_names[prev_idx]));
	}

	/* Condition Immunity flags */
	if (f2)
	{
		list_ptr = list;

		list_ptr = spoiler_flag_aux(f2, imm_flags2_desc, list_ptr,
	                              N_ELEMENTS(imm_flags2_desc));

		/* Terminate the description list */
		*list_ptr = NULL;
		
		/* Weapon flags */
		anything |= outlist("It provides immunity to the effects of", list);
	}

	/* Miscellenious Abilities */
	if (f3)
	{
		list_ptr = list;

		/*
		 * Special flags
		 */
		list_ptr = spoiler_flag_aux(f3, misc_flags3_desc, list_ptr,
		                             N_ELEMENTS(misc_flags3_desc));

		/* Terminate the description list */
		*list_ptr = NULL;
	
		/* Other flags */
		anything |= outlist("It gives its wielder", list);
	}

	/* Negative effects */
	if (f3)
	{
		list_ptr = list;

		/*
		 * Special flags
		 */
		list_ptr = spoiler_flag_aux(f3, bad_flags3_desc, list_ptr,
		                             N_ELEMENTS(bad_flags3_desc));

		/* Terminate the description list */
		*list_ptr = NULL;
	
		/* Other flags */
		anything |= outlist("it burdens its wielder with", list);
	}

	/* Hack - curse spoiler items */
	if (spoil)
	{
		if (f3 & (TR3_LIGHT_CURSE)) o_ptr->ident |= (IDENT_CURSED);
	}

	/*
	 * Handle cursed objects here to avoid redundancies such as noting
	 * that a permanently cursed object is heavily cursed as well as
	 * being "lightly cursed".
	 */
	if (!random && cursed_p(o_ptr))
	{
		if (f3 & (TR3_PERMA_CURSE))
		{
			text_out_c(TERM_RED, "It is permanently cursed.  ");
			anything = TRUE;
		}
		else if (f3 & (TR3_HEAVY_CURSE))
		{
			text_out_c(TERM_RED, "It is heavily cursed.  ");
			anything = TRUE;
		}
		else if (object_known_p(o_ptr) || (o_ptr->discount == INSCRIP_CURSED))
		{
			text_out_c(TERM_RED, "It is cursed.  ");
			anything = TRUE;
		}
	}

	/* Can be damaged by */
	if (!random)
	{
		list_ptr = list;

		/*
		 * Special flags
		 */
		list_ptr = spoiler_damage_aux(spoil, o_ptr, list_ptr);

		/* Terminate the description list */
		*list_ptr = NULL;
	
		/* Other flags */
		if (spoil || (object_known_p(o_ptr) && 
			(!o_ptr->a_idx || (a_info[o_ptr->a_idx].status & A_STATUS_KNOWN) || 
			(a_info[o_ptr->a_idx].status & A_STATUS_MEMORY))))
		{
			if (outlist("It is vulnerable to damage by", list));
			else text_out_c(TERM_WHITE, "It cannot be damaged by any means.  ");
			anything = TRUE;
		}
		else anything |= outlist("It might be vulnerable to damage by", list);
	}

	/* Activations */
	if (!random && item_activation(buf, &i, &j, o_ptr))
	{
		do_act = TRUE;

		/* If spoil mode, always print activation */
		if (spoil)
		{
			/* Do  nothing */
		}
		/* Unknown activation, print nothing */
		else if (!(object_known_p(o_ptr) || object_aware_p(o_ptr)) ||
			((artifact_p(o_ptr)) && !(a_info[o_ptr->a_idx].status & A_STATUS_ACTIVATE)))
		{
			do_act = FALSE;
		}
		/* Check if the activation is fully known (actifact only) */
		else if ((artifact_p(o_ptr)))
		{
			artifact_type *a_ptr = &a_info[o_ptr->a_idx];

			/* Times unknown */
			if (!artifact_known_p(a_ptr))
			{
				i = 0;
				j = 0;
			}
		}
		/* Hack - recharge times for rods/talismans */
		else if ((o_ptr->tval == TV_ROD) || (o_ptr->tval == TV_TALISMAN))
		{
			i = o_ptr->pval;
		}

		if (do_act)
		{
			/* Activation */
			if (artifact_p(o_ptr)) 
			{
				switch (o_ptr->tval)
				{
					case TV_BOW:
					case TV_DIGGING:
					case TV_SWORD:
					case TV_HAFTED:
					case TV_POLEARM:
					{
						text_out("When wielded and activated, it ");
						break;
					}
					default: text_out("When worn and activated, it "); break;
				}
			}
			else
			{ 
				switch (o_ptr->tval)
				{
					case TV_DRAG_ARMOR:		text_out("When worn and activated, it "); break;
					case TV_TALISMAN:		text_out("When invoked, it "); break;
					case TV_ROD:			text_out("When zapped, it "); break;
					case TV_FOOD:			text_out("When eaten, it "); break;
					case TV_POTION:			text_out("When quaffed, it "); break;
					case TV_SCROLL:			text_out("When read, it "); break;
					case TV_WAND:			text_out("When aimed, it "); break;
					case TV_STAFF: default: text_out("When used, it "); break;
				}
			}

			text_out(format("%s", buf));
			if ((i > 0) && (j > 0)) 
			{
				text_out(", recharging every ");
				text_out_c(TERM_L_GREEN, format ("%d", i));
				text_out("+");
				text_out_c(TERM_L_GREEN, format ("d%d", j));
				text_out(" turns.  ");
			}
			else if (i > 1)
			{
				text_out(", recharging every ");
				text_out_c(TERM_L_GREEN, format ("%d", i));
				text_out(" turns.  ");
			}
			else if (i > 0)
			{
				text_out(", recharging every turn.");
			}
			else text_out(".  ");
			anything = TRUE;
		}
	}

	/* Unknown extra powers (ego-item with random extras or artifact) */
	if (!spoil && !random && (object_known_p(o_ptr) && ((o_ptr->xtra1) || artifact_p(o_ptr))))
	{
		bool hidden = TRUE;
	
		if (!artifact_p(o_ptr) && (o_ptr->ident & IDENT_MENTAL)) hidden = FALSE;

		if (artifact_p(o_ptr))
		{
			artifact_type *a_ptr = &a_info[o_ptr->a_idx];

			if artifact_known_p(a_ptr) hidden = FALSE;
		}
	    
		if (hidden)
		{
			if (anything) text_out("\n");
			text_out_c(TERM_VIOLET,"It may have undiscovered powers.  ");
			anything = TRUE;
		}
	}

	if (!random && !spoil && (cp_ptr->flags & CF_APPRAISE))
	{
		i = object_value(o_ptr);

		if (i)
		{
			text_out("\nYou think it values at around ");
			text_out_c(TERM_ORANGE, format("%d", i));
			text_out(" gp.  ");
		}
		else text_out("\nIt appears to be totally worthless.  ");

		anything = TRUE;
	}
	
	/* New line: show alchemy combination if known */
	if ((!random) && (!spoil) && o_ptr->tval == TV_POTION)
	{
		char line[80];
		bool alch_title_out = FALSE;

		/* Target of mix */
		if ((potion_alch[o_ptr->sval].known1) || (potion_alch[o_ptr->sval].known2))
		{
			if (!alch_title_out)
			{
				text_out_c(TERM_YELLOW,"\nKnown alchemical combinations:");
				alch_title_out = TRUE;
			}
			alchemy_describe(line, o_ptr->sval);
			text_out_c(TERM_L_WHITE, format("\n%s",line));
		}
 
		/* Components */
		for (i = 0; i < SV_POTION_MAX; i++)
		{
			if ((potion_alch[i].sval1 == o_ptr->sval && 
				potion_alch[i].known1) || (potion_alch[i].sval2 == o_ptr->sval && 
				potion_alch[i].known2))
			{
				if (!alch_title_out)
				{
					text_out_c(TERM_YELLOW,"\nKnown alchemical combinations:");
					alch_title_out = TRUE;
				}
				alchemy_describe(line, i);
				text_out_c(TERM_L_WHITE, format("\n%s",line));
			}
		}
	
		anything |= alch_title_out;
	}

	/* Nothing was printed */
	if (!random && !anything) text_out("You know nothing worth noting about this item.  ");

	/* End */
	if (!random) text_out("\n");
	else if (anything) text_out("\n");
}

/* Print the depth of an history item */
static void history_depth(s16b depth)
{
	if (depth == 0)
	{
		text_out_c(TERM_YELLOW, "in the town\n");
	}
	else if (depth_in_feet)
	{
		text_out_c(TERM_YELLOW, format("at a depth of %d ft\n", depth * 50));
	}
	else 
	{
		text_out_c(TERM_YELLOW, format("on dungeon level %d\n", depth));
	}
}

/*
 * Display item history 
 */
void display_object_history(object_type *o_ptr)
{
	char intro[10];

	if (o_ptr->number > 1) strcpy(intro, "They were");
	else strcpy (intro, "It was");

	switch (o_ptr->origin_nature)
	{
		case ORIGIN_NONE: case ORIGIN_MIXED:
		{
			/* Don't display anything */
 			break;
		}
		case ORIGIN_BIRTH:
		{
			text_out_c(TERM_YELLOW, format("%s ", intro));
			text_out_c(TERM_YELLOW, "an inheritance from your family.\n");
			break;
		}
		case ORIGIN_STORE:
		{
			text_out_c(TERM_YELLOW, format("%s ", intro));
			text_out_c(TERM_YELLOW, "bought in a store.\n");
			break;
		}
		case ORIGIN_MORGOTH:
		{
			text_out_c(TERM_YELLOW, "It is one of your prizes for victory!.\n");
 			break;
		}
		case ORIGIN_CHEAT:
		{
			text_out_c(TERM_YELLOW, "-- Created by debug option --\n");
 			break;
		}
		case ORIGIN_FLOOR:
		{
			text_out_c(TERM_YELLOW, format("%s ", intro));
			text_out_c(TERM_YELLOW, "lying on the floor ");
			history_depth(o_ptr->origin_dlvl);
 			break;
		}
		case ORIGIN_ACQUIRE:
		{
			text_out_c(TERM_YELLOW, format("%s ", intro));
			text_out_c(TERM_YELLOW, "conjured forth by magic ");
			history_depth(o_ptr->origin_dlvl);
 			break;
		}
		case ORIGIN_DROP_UNKNOWN:
		{
			text_out_c(TERM_YELLOW, format("%s ", intro));
			text_out_c(TERM_YELLOW, "dropped by an unknown monster ");
			history_depth(o_ptr->origin_dlvl);
 			break;
		}
		case ORIGIN_REWARD:
		{
			text_out_c(TERM_YELLOW, format("%s ", intro));
			text_out_c(TERM_YELLOW, "a reward for your exploits ");
			history_depth(o_ptr->origin_dlvl);
			break;
		}
		case ORIGIN_DROP_KNOWN:
		{
			cptr name = monster_name_idx(o_ptr->origin_r_idx, o_ptr->origin_s_idx, o_ptr->origin_u_idx);

			text_out_c(TERM_YELLOW, format("%s ", intro));
			text_out_c(TERM_YELLOW, "dropped by ");
			if (!o_ptr->origin_u_idx)
			{
				/* Indefinite monsters need an indefinite article */
				text_out_c(TERM_YELLOW, format("%s ", is_a_vowel(name[0]) ? "an" : "a"));
			}
			text_out_c(TERM_YELLOW, format ("%s ", name));
			history_depth(o_ptr->origin_dlvl);
 			break;
		}
	}
}

/*
 * Check whether the history is interesting 
 */
bool history_interesting(object_type *o_ptr)
{
	/* Empty slots are always boring */
	if (!o_ptr->k_idx) return FALSE;

	/* Items with no, or mixed, origins are always boring */
	if (!o_ptr->origin_nature || (o_ptr->origin_nature == ORIGIN_MIXED)) return FALSE;

	/* Artifacts are always interesting */
	if (artifact_p(o_ptr)) return TRUE;

	/* Ego items are interesting if they're good */
	if (ego_item_p(o_ptr) && (object_value(o_ptr) > 0)) return TRUE;

	/* Objects dropped by uniques are always interesting */
	if (o_ptr->origin_u_idx) return TRUE;

	/* Cheat items are always interesting */
	if (o_ptr->origin_nature == ORIGIN_CHEAT) return TRUE;

	/* Some other origins usually are boring */
	if ((o_ptr->origin_nature == ORIGIN_BIRTH) || (o_ptr->origin_nature == ORIGIN_STORE))
		return FALSE;

	/* Objects OOD by more than ten levels are interesting */
	if ((o_ptr->origin_dlvl + 10) < k_info[o_ptr->k_idx].level) return TRUE;

	return FALSE;
}

/* The possible actions */

#define KEEP_O		1
#define	KEEP_J		2
#define LOSE_M_EGO	3
#define MIX			4

/* 
 * Try to stack item histories 
 */
void stack_histories(object_type *o_ptr, object_type *j_ptr)
{
	int action = MIX;

	/* Histories are identical anyway */
	if ((o_ptr->origin_nature == j_ptr->origin_nature) &&
		 (o_ptr->origin_dlvl == j_ptr->origin_dlvl) && 
		 (o_ptr->origin_r_idx == j_ptr->origin_r_idx) &&
  		 (o_ptr->origin_s_idx == j_ptr->origin_s_idx) &&
  		 (o_ptr->origin_u_idx == j_ptr->origin_u_idx))
	{
		action = KEEP_O;
	}

	/* Allow yourself to lose "ego" monster names */
	else if ((o_ptr->origin_nature == j_ptr->origin_nature) &&
			 (o_ptr->origin_dlvl == j_ptr->origin_dlvl) && 
			 (o_ptr->origin_r_idx == j_ptr->origin_r_idx) &&
  			 (o_ptr->origin_u_idx == j_ptr->origin_u_idx))
	{
		action = LOSE_M_EGO;
	}

	/* If the first object has an exceptionally interesting history */
	else if ((o_ptr->origin_u_idx) ||
	    (o_ptr->origin_dlvl > k_info[o_ptr->k_idx].level + 10))
	{
		/* Use the first item's history */
		action = KEEP_O;
	}

	/* If the second object has an exceptionally interesting history */
	else if ((j_ptr->origin_u_idx) ||
	    (j_ptr->origin_dlvl > k_info[j_ptr->k_idx].level + 10))
	{
		/* Use the second item's history */
		action = KEEP_J;
	}

	/* Do it */
	switch (action)
	{
		case (KEEP_O):
		{
			/* Nothing to do */
			break;
		}
		case (KEEP_J):
		{
			/* Copy over the second object's history */
			o_ptr->origin_nature = j_ptr->origin_nature;
			o_ptr->origin_dlvl = j_ptr->origin_dlvl;
			o_ptr->origin_r_idx = j_ptr->origin_r_idx;
  			o_ptr->origin_s_idx = j_ptr->origin_s_idx;
  			o_ptr->origin_u_idx = j_ptr->origin_u_idx;
			break;
		}
		case (LOSE_M_EGO):
		{
			o_ptr->origin_s_idx = 0;
			break;
		}
		case (MIX): default:
		{
			o_ptr->origin_nature = ORIGIN_MIXED;
			break;
		}
	}
}

/*
 * Hack -- display an object kind in the current window
 *
 * Include list of usable spells for readable books
 */
void display_koff(object_type *o_ptr)
{
	int y;

	char o_name[80];

	/* Erase the window */
	for (y = 0; y < Term->hgt; y++)
	{
		/* Erase the line */
		Term_erase(0, y, 255);
	}

	/* No info */
	if (!o_ptr->k_idx) return;

	/* Mention the object name */
	Term_putstr(0, 0, -1, TERM_WHITE, o_name);

	/* Display spells in readable books */
	if (o_ptr->tval == TV_MUSIC)
	{
		if (cp_ptr->flags & CF_MUSIC)
		{
			print_spells(o_ptr->sval, TRUE, o_ptr->pval, 1, 14);
			return;
		}
	}
	if (o_ptr->tval == TV_MAGIC_BOOK)
	{
		if (cp_ptr->spell_book[o_ptr->sval])
		{
			/* Print spells */
			print_spells(o_ptr->sval, FALSE, 0, 1, 14);
			return;
		}
	}

	/* Set text_out hook */
	text_out_hook = text_out_to_screen;

	text_out("\n");

	/* Actually display the item */
	list_object(o_ptr, OBJECT_INFO_KNOWN);

	/* Display object history */
	if (term_obj_real) display_object_history(o_ptr);

	/* Display item name */
	obj_top(o_ptr, term_obj_real);
}
