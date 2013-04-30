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
		case 0:	case 1:
		{
			likert_color = TERM_RED;
			return ("Bad");
		}
		case 2:
		{
			likert_color = TERM_RED;
			return ("Poor");
		}
		case 3:	case 4:
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
		case 7:	case 8:
		{
			likert_color = TERM_L_GREEN;
			return ("Excellent");
		}
		case 9:	case 10: case 11: case 12: case 13:
		{
			likert_color = TERM_L_GREEN;
			return ("Superb");
		}
		case 14: case 15: case 16: case 17:
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
 * Prints some "extra" information on the screen.
 *
 * Space includes rows 3-9 cols 24-79
 * Space includes rows 10-17 cols 1-79
 * Space includes rows 19-22 cols 1-79
 */
static void display_player_xtra_info(void)
{
	int col;
	int base, plus;
	byte attr;

	char buf[160];

	/* Left */
	col = 1;

	/* Wounds */
	Term_putstr(col, 8, -1, TERM_WHITE, "Wounds");

	if ((p_ptr->wound_vigor) && (p_ptr->wound_wit) && (p_ptr->wound_grace))
	{
		Term_putstr(col+15, 8, -1, TERM_VIOLET, ("All"));
	}
	else if ((p_ptr->wound_vigor) && (p_ptr->wound_wit))
	{
		Term_putstr(col+7, 8, -1, TERM_RED, ("Vigor & Wit"));
	}
	else if ((p_ptr->wound_vigor) && (p_ptr->wound_grace))
	{
		Term_putstr(col+7, 8, -1, TERM_RED, ("Vigor&Grace"));
	}
	else if ((p_ptr->wound_wit) && (p_ptr->wound_grace))
	{
		Term_putstr(col+7, 8, -1, TERM_RED, ("Wit & Grace"));
	}
	else if (p_ptr->wound_vigor)
	{
		Term_putstr(col+13, 8, -1, TERM_YELLOW, ("Vigor"));
	}
	else if (p_ptr->wound_wit)
	{
		Term_putstr(col+15, 8, -1, TERM_YELLOW, ("Wit"));
	}
	else if (p_ptr->wound_grace)
	{
		Term_putstr(col+13, 8, -1, TERM_YELLOW, ("Grace"));
	}
	else
	{
		Term_putstr(col+14, 8, -1, TERM_L_GREEN, ("None"));
	}

	int current_lore = p_ptr->lore - p_ptr->lore_uses;
	if (current_lore < 0) current_lore = 0;

	int current_reserves = p_ptr->reserves - p_ptr->reserves_uses;
	if (current_reserves < 0) current_reserves = 0;

	int current_escapes = p_ptr->escapes - p_ptr->escapes_uses;
	if (current_escapes < 0) current_escapes = 0;

	/* Hit Points */
	Term_putstr(col, 9, -1, TERM_WHITE, "Hits");
	if (p_ptr->chp >= p_ptr->mhp) attr = TERM_L_GREEN;
	else if (p_ptr->chp == 0) attr = TERM_RED;
	else attr = TERM_YELLOW;
	Term_putstr(col+8, 9, -1, attr, format("%4d /%4d", (int)p_ptr->chp, (int)p_ptr->mhp));

	/* Spell Points */
	if (p_ptr->msp > 0)
	{
		Term_putstr(col, 10, -1, TERM_WHITE, "Mana");
		if (p_ptr->chp >= p_ptr->msp) attr = TERM_L_GREEN;
		else if (p_ptr->csp == 0) attr = TERM_RED;
		else attr = TERM_YELLOW;
		Term_putstr(col+8, 10, -1, attr, format("%4d /%4d", (int)p_ptr->csp, (int)p_ptr->msp));
	}

	/* Lore */
	int temp_lore_bonus = 0;

	/* Temp Lore bonus when near a bookshelf & inside a room */
	if (cave_info[p_ptr->py][p_ptr->px] & (CAVE_ROOM))
	{
		if (t_list[cave_t_idx[p_ptr->py + 1][p_ptr->px]].w_idx == WG_SHELF) temp_lore_bonus = 3;
		if (t_list[cave_t_idx[p_ptr->py - 1][p_ptr->px]].w_idx == WG_SHELF) temp_lore_bonus = 3;
		if (t_list[cave_t_idx[p_ptr->py][p_ptr->px + 1]].w_idx == WG_SHELF) temp_lore_bonus = 3;
		if (t_list[cave_t_idx[p_ptr->py][p_ptr->px - 1]].w_idx == WG_SHELF) temp_lore_bonus = 3;
		if (t_list[cave_t_idx[p_ptr->py + 1][p_ptr->px]].w_idx == WG_SHELF_EMPTY) temp_lore_bonus = 3;
		if (t_list[cave_t_idx[p_ptr->py - 1][p_ptr->px]].w_idx == WG_SHELF_EMPTY) temp_lore_bonus = 3;
		if (t_list[cave_t_idx[p_ptr->py][p_ptr->px + 1]].w_idx == WG_SHELF_EMPTY) temp_lore_bonus = 3;
		if (t_list[cave_t_idx[p_ptr->py][p_ptr->px - 1]].w_idx == WG_SHELF_EMPTY) temp_lore_bonus = 3;
		if (t_list[cave_t_idx[p_ptr->py + 1][p_ptr->px]].w_idx == WG_SHELF_OPEN_DOOR) temp_lore_bonus = 3;
		if (t_list[cave_t_idx[p_ptr->py - 1][p_ptr->px]].w_idx == WG_SHELF_OPEN_DOOR) temp_lore_bonus = 3;
		if (t_list[cave_t_idx[p_ptr->py][p_ptr->px + 1]].w_idx == WG_SHELF_OPEN_DOOR) temp_lore_bonus = 3;
		if (t_list[cave_t_idx[p_ptr->py][p_ptr->px - 1]].w_idx == WG_SHELF_OPEN_DOOR) temp_lore_bonus = 3;
		if (t_list[cave_t_idx[p_ptr->py + 1][p_ptr->px]].w_idx == WG_SHELF_CLOSED_DOOR) temp_lore_bonus = 3;
		if (t_list[cave_t_idx[p_ptr->py - 1][p_ptr->px]].w_idx == WG_SHELF_CLOSED_DOOR) temp_lore_bonus = 3;
		if (t_list[cave_t_idx[p_ptr->py][p_ptr->px + 1]].w_idx == WG_SHELF_CLOSED_DOOR) temp_lore_bonus = 3;
		if (t_list[cave_t_idx[p_ptr->py][p_ptr->px - 1]].w_idx == WG_SHELF_CLOSED_DOOR) temp_lore_bonus = 3;
		if (t_list[cave_t_idx[p_ptr->py + 1][p_ptr->px]].w_idx == WG_SHELF_SECRET_DOOR) temp_lore_bonus = 3;
		if (t_list[cave_t_idx[p_ptr->py - 1][p_ptr->px]].w_idx == WG_SHELF_SECRET_DOOR) temp_lore_bonus = 3;
		if (t_list[cave_t_idx[p_ptr->py][p_ptr->px + 1]].w_idx == WG_SHELF_SECRET_DOOR) temp_lore_bonus = 3;
		if (t_list[cave_t_idx[p_ptr->py][p_ptr->px - 1]].w_idx == WG_SHELF_SECRET_DOOR) temp_lore_bonus = 3;
	}

	/* Temp Lore bonus when on a Circle of Knowledge */
	if (t_list[cave_t_idx[p_ptr->py][p_ptr->px]].w_idx == WG_CIRCLE_OF_KNOWLEDGE) temp_lore_bonus += 7;

	if (p_ptr->lore > 0)
	{
		if (p_stat(A_INT) + p_stat(A_WIS) + temp_lore_bonus >= 30)
		{
			Term_putstr(col, 11, -1, TERM_WHITE, "*Lore*");
		}
		else
		{
			Term_putstr(col, 11, -1, TERM_WHITE, "Lore");
		}
		if (current_lore >= p_ptr->lore) attr = TERM_L_GREEN;
		else if (current_lore == 0) attr = TERM_RED;
		else attr = TERM_YELLOW;
		Term_putstr(col+8, 11, -1, attr, format("%4d /%4d", (int)current_lore, (int)p_ptr->lore));
	}

	/* Reserves */
	if (p_ptr->reserves > 0)
	{
		Term_putstr(col, 12, -1, TERM_WHITE, "Reserves");
		if (current_reserves >= p_ptr->reserves) attr = TERM_L_GREEN;
		else if (current_reserves == 0) attr = TERM_RED;
		else attr = TERM_YELLOW;
		Term_putstr(col+8, 12, -1, attr, format("%4d /%4d", (int)current_reserves, (int)p_ptr->reserves));
	}

	/* Escapes */
	if (p_ptr->escapes > 0)
	{
		Term_putstr(col, 13, -1, TERM_WHITE, "Escapes");
		if (current_escapes >= p_ptr->escapes) attr = TERM_L_GREEN;
		else if (current_escapes == 0) attr = TERM_RED;
		else attr = TERM_YELLOW;
		Term_putstr(col+8, 13, -1, attr, format("%4d /%4d", (int)current_escapes, (int)p_ptr->escapes));
	}

	/* Left Middle */
	col = 22;

	/* Infra */
	sprintf(buf, "%d squares", p_ptr->see_infra);
	Term_putstr(col, 8, -1, TERM_WHITE, "Infra");
	Term_putstr(col + 9, 8, -1, TERM_L_BLUE, format("%9s", buf));

	/* Spell and Device Range */
	sprintf(buf, "%d squares", p_ptr->spell_range);
	Term_putstr(col, 9, -1, TERM_WHITE, "Spells");
	Term_putstr(col + 9, 9, -1, TERM_L_BLUE, format("%8s", buf));

	/* Armor */
	base = p_ptr->dis_ac;
	plus = p_ptr->dis_to_a;

	/* Total Armor */
	sprintf(buf, "[%d,%+d]", base, plus);
	Term_putstr(col, 12, -1, TERM_WHITE, "Armor");
	Term_putstr(col + 5, 12, -1, TERM_L_BLUE, format("%13s", buf));

	/* Melee weapon
	o_ptr = &inventory[INVEN_WIELD]; */

	/* Base skill
	hit = p_ptr->dis_to_h; */

	/* Apply weapon bonuses
	if (object_known_p(o_ptr)) hit += object_to_h(o_ptr); */

	/* Blows */
	sprintf(buf, "%d/turn", p_ptr->num_blow);
	Term_putstr(col, 13, -1, TERM_WHITE, "Blows");
	Term_putstr(col + 5, 13, -1, TERM_L_BLUE, format("%13s", buf));

	/* Melee damage */
	sprintf(buf, "%dd%d", p_ptr->dd, p_ptr->ds);
	Term_putstr(col, 14, -1, TERM_WHITE, "Melee Damage");
	Term_putstr(col + 12, 14, -1, TERM_L_BLUE, format("%6s", buf));

	/* Range weapon
	o_ptr = &inventory[INVEN_BOW]; */

	/* Base skill
	hit = p_ptr->dis_to_h; */

	/* Apply weapon bonuses
	if (object_known_p(o_ptr)) hit += object_to_h(o_ptr); */

	/* Range attacks
	sprintf(buf, "(%+d)", hit);
	Term_putstr(col, 12, -1, TERM_WHITE, "Shoot");
	Term_putstr(col + 5, 12, -1, TERM_L_BLUE, format("%13s", buf)); */

	/* Shots */
	sprintf(buf, "%d/turn", p_ptr->num_fire);
	Term_putstr(col, 15, -1, TERM_WHITE, "Shots");
	Term_putstr(col + 5, 15, -1, TERM_L_BLUE, format("%13s", buf));

	/* Thrown Damage */
	sprintf(buf, "%d*", p_ptr->num_fire);
	Term_putstr(col, 16, -1, TERM_WHITE, "Thrown Damage");
	if (p_stat(A_STR) >= 29)
	{
		if ((p_ptr->rage) && (p_ptr->mighty_throw)) Term_putstr(col + 16, 16, -1, TERM_L_BLUE, format ("24*"));
		else if ((p_ptr->rage) || (p_ptr->mighty_throw)) Term_putstr(col + 16, 16, -1, TERM_L_BLUE, format ("12*"));
		else Term_putstr(col + 16, 16, -1, TERM_L_BLUE, format ("6*"));
	}
	else if (p_stat(A_STR) >= 25)	
	{
		if ((p_ptr->rage) && (p_ptr->mighty_throw)) Term_putstr(col + 16, 16, -1, TERM_L_BLUE, format ("20*"));
		else if ((p_ptr->rage) || (p_ptr->mighty_throw)) Term_putstr(col + 16, 16, -1, TERM_L_BLUE, format ("10*"));
		else Term_putstr(col + 16, 16, -1, TERM_L_BLUE, format ("5*"));
	}
	else if (p_stat(A_STR) >= 21)	
	{
		if ((p_ptr->rage) && (p_ptr->mighty_throw)) Term_putstr(col + 16, 16, -1, TERM_L_BLUE, format ("16*"));
		else if ((p_ptr->rage) || (p_ptr->mighty_throw)) Term_putstr(col + 16, 16, -1, TERM_L_BLUE, format ("8*"));
		else Term_putstr(col + 16, 16, -1, TERM_L_BLUE, format ("4*"));
	}
	else if (p_stat(A_STR) >= 17)	
	{
		if ((p_ptr->rage) && (p_ptr->mighty_throw)) Term_putstr(col + 16, 16, -1, TERM_L_BLUE, format ("12*"));
		else if ((p_ptr->rage) || (p_ptr->mighty_throw)) Term_putstr(col + 16, 16, -1, TERM_L_BLUE, format ("6*"));
		else Term_putstr(col + 16, 16, -1, TERM_L_BLUE, format ("3*"));
	}
	else if (p_stat(A_STR) >= 13)	
	{
		if ((p_ptr->rage) && (p_ptr->mighty_throw)) Term_putstr(col + 16, 16, -1, TERM_L_BLUE, format ("8*"));
		else if ((p_ptr->rage) || (p_ptr->mighty_throw)) Term_putstr(col + 16, 16, -1, TERM_L_BLUE, format ("4*"));
		else Term_putstr(col + 16, 16, -1, TERM_L_BLUE, format ("2*"));
	}
	else
	{
		if ((p_ptr->rage) && (p_ptr->mighty_throw)) Term_putstr(col + 16, 16, -1, TERM_L_BLUE, format ("4*"));
		else if ((p_ptr->rage) || (p_ptr->mighty_throw)) Term_putstr(col + 16, 16, -1, TERM_L_BLUE, format ("2*"));
		else Term_putstr(col + 16, 16, -1, TERM_L_BLUE, format ("1*"));
	}

	/* Right Middle */
	col=41;

	/* Racial Ability
	put_str("Racial Ability", 11, col);
	display_player_race_power(12, col); */

	/* Bottom */
	col = 1;

	/* Output to the screen and wrap at column 55 */
	text_out_wrap = 55;
	text_out_indent = 1;
 
	/* Print history */
	Term_gotoxy(text_out_indent, 18);
	text_out_to_screen(TERM_WHITE, p_ptr->history);

	/* Reset text_out() vars */
	text_out_wrap = 0;
	text_out_indent = 0;
}

static void print_deity_bonuses(int row, int bonus1, int bonus2, int multiplier, int color)
{
	if (multiplier == 1)
	{
		if (bonus1 == DEITY_STR) Term_putstr(43, row, -1, color, "STR++");
		else if (bonus1 == DEITY_INT) Term_putstr(43, row, -1, color, "MEM++");
		else if (bonus1 == DEITY_WIS) Term_putstr(43, row, -1, color, "WIS++");
		else if (bonus1 == DEITY_DEX) Term_putstr(43, row, -1, color, "DEX++");
		else if (bonus1 == DEITY_CON) Term_putstr(43, row, -1, color, "CON++");
		else if (bonus1 == DEITY_CHR) Term_putstr(43, row, -1, color, "PRE++");
		else if (bonus1 == DEITY_BERSERK) Term_putstr(43, row, -1, color, "Ber++");
		else if (bonus1 == DEITY_ESCAPE) Term_putstr(43, row, -1, color, "Esc++");
		else if (bonus1 == DEITY_AC) Term_putstr(43, row, -1, color, "Arm++");
		else if (bonus1 == DEITY_RANGE) Term_putstr(43, row, -1, color, "Rng++");
		else if (bonus1 == DEITY_STEALTH) Term_putstr(43, row, -1, color, "Ste++");

		if (bonus1 == bonus2) Term_putstr(48, row, -1, color, "+");
		else if (bonus2 == DEITY_STR) Term_putstr(49, row, -1, color, "STR+");
		else if (bonus2 == DEITY_INT) Term_putstr(49, row, -1, color, "MEM+");
		else if (bonus2 == DEITY_WIS) Term_putstr(49, row, -1, color, "WIS+");
		else if (bonus2 == DEITY_DEX) Term_putstr(49, row, -1, color, "DEX+");
		else if (bonus2 == DEITY_CON) Term_putstr(49, row, -1, color, "CON+");
		else if (bonus2 == DEITY_CHR) Term_putstr(49, row, -1, color, "PRE+");
		else if (bonus2 == DEITY_BERSERK) Term_putstr(49, row, -1, color, "Ber+");
		else if (bonus2 == DEITY_ESCAPE) Term_putstr(49, row, -1, color, "Esc+");
		else if (bonus2 == DEITY_AC) Term_putstr(49, row, -1, color, "Arm+");
		else if (bonus2 == DEITY_RANGE) Term_putstr(49, row, -1, color, "Rng+");
		else if (bonus2 == DEITY_STEALTH) Term_putstr(49, row, -1, color, "Ste+");
	}
	else if (multiplier == 2)
	{
		if (bonus1 == DEITY_STR) Term_putstr(43, row, -1, color, "STR+++");
		else if (bonus1 == DEITY_INT) Term_putstr(43, row, -1, color, "MEM+++");
		else if (bonus1 == DEITY_WIS) Term_putstr(43, row, -1, color, "WIS+++");
		else if (bonus1 == DEITY_DEX) Term_putstr(43, row, -1, color, "DEX+++");
		else if (bonus1 == DEITY_CON) Term_putstr(43, row, -1, color, "CON+++");
		else if (bonus1 == DEITY_CHR) Term_putstr(43, row, -1, color, "PRE+++");
		else if (bonus1 == DEITY_BERSERK) Term_putstr(43, row, -1, color, "Ber+++");
		else if (bonus1 == DEITY_ESCAPE) Term_putstr(43, row, -1, color, "Esc+++");
		else if (bonus1 == DEITY_AC) Term_putstr(43, row, -1, color, "Arm+++");
		else if (bonus1 == DEITY_RANGE) Term_putstr(43, row, -1, color, "Rng+++");
		else if (bonus1 == DEITY_STEALTH) Term_putstr(43, row, -1, color, "Ste+++");

		if (bonus1 == bonus2) Term_putstr(49, row, -1, color, "+++");
		else if (bonus2 == DEITY_STR) Term_putstr(49, row, -1, color, "STR+++");
		else if (bonus2 == DEITY_INT) Term_putstr(49, row, -1, color, "MEM+++");
		else if (bonus2 == DEITY_WIS) Term_putstr(49, row, -1, color, "WIS+++");
		else if (bonus2 == DEITY_DEX) Term_putstr(49, row, -1, color, "DEX+++");
		else if (bonus2 == DEITY_CON) Term_putstr(49, row, -1, color, "CON+++");
		else if (bonus2 == DEITY_CHR) Term_putstr(49, row, -1, color, "PRE+++");
		else if (bonus2 == DEITY_BERSERK) Term_putstr(49, row, -1, color, "Ber+++");
		else if (bonus2 == DEITY_ESCAPE) Term_putstr(49, row, -1, color, "Esc+++");
		else if (bonus2 == DEITY_AC) Term_putstr(49, row, -1, color, "Arm+++");
		else if (bonus2 == DEITY_RANGE) Term_putstr(49, row, -1, color, "Rng+++");
		else if (bonus2 == DEITY_STEALTH) Term_putstr(49, row, -1, color, "Ste+++");
	}
	else if (multiplier == -1)
	{
		if (bonus1 == DEITY_STR) Term_putstr(43, row, -1, color, "STR--");
		else if (bonus1 == DEITY_INT) Term_putstr(43, row, -1, color, "MEM--");
		else if (bonus1 == DEITY_WIS) Term_putstr(43, row, -1, color, "WIS--");
		else if (bonus1 == DEITY_DEX) Term_putstr(43, row, -1, color, "DEX--");
		else if (bonus1 == DEITY_CON) Term_putstr(43, row, -1, color, "CON--");
		else if (bonus1 == DEITY_CHR) Term_putstr(43, row, -1, color, "PRE--");
		else if (bonus1 == DEITY_BERSERK) Term_putstr(43, row, -1, color, "Ber--");
		else if (bonus1 == DEITY_ESCAPE) Term_putstr(43, row, -1, color, "Esc--");
		else if (bonus1 == DEITY_AC) Term_putstr(43, row, -1, color, "Arm--");
		else if (bonus1 == DEITY_RANGE) Term_putstr(43, row, -1, color, "Rng--");
		else if (bonus1 == DEITY_STEALTH) Term_putstr(43, row, -1, color, "Ste--");

		if (bonus1 == bonus2) Term_putstr(48, row, -1, color, "-");
		else if (bonus2 == DEITY_STR) Term_putstr(49, row, -1, color, "STR-");
		else if (bonus2 == DEITY_INT) Term_putstr(49, row, -1, color, "MEM-");
		else if (bonus2 == DEITY_WIS) Term_putstr(49, row, -1, color, "WIS-");
		else if (bonus2 == DEITY_DEX) Term_putstr(49, row, -1, color, "DEX-");
		else if (bonus2 == DEITY_CON) Term_putstr(49, row, -1, color, "CON-");
		else if (bonus2 == DEITY_CHR) Term_putstr(49, row, -1, color, "PRE-");
		else if (bonus2 == DEITY_BERSERK) Term_putstr(49, row, -1, color, "Ber-");
		else if (bonus2 == DEITY_ESCAPE) Term_putstr(49, row, -1, color, "Esc-");
		else if (bonus2 == DEITY_AC) Term_putstr(49, row, -1, color, "Arm-");
		else if (bonus2 == DEITY_RANGE) Term_putstr(49, row, -1, color, "Rng-");
		else if (bonus2 == DEITY_STEALTH) Term_putstr(49, row, -1, color, "Ste-");
	}
}

/*
 * Prints some skill information on the screen.
 */
static void display_player_skill_info(void)
{
	int col;
	int xskill[SK_MAX];
	int melee_hit = 0;
	int shooting_hit = 0;
	int thrown_hit = 0;

	cptr desc;

	object_type *o_ptr;

	/* Right */
	col = 56;

	/* Fighting Skill (with current weapon) */
	o_ptr = &inventory[INVEN_WIELD];
	xskill[SK_THN] = p_ptr->skill[SK_THN] + p_ptr->to_h_melee + object_to_h(o_ptr);

	/* Base skill */
	melee_hit = p_ptr->dis_to_h_melee;

	/* Apply weapon bonuses */
	if (object_known_p(o_ptr)) melee_hit += object_to_h(o_ptr);

	/* Melee attacks
	sprintf(buf, "(%+d, %dd%d)", hit, p_ptr->dd, p_ptr->ds);
	Term_putstr(col, 10, -1, TERM_WHITE, "Melee");
	Term_putstr(col + 5, 10, -1, TERM_L_BLUE, format("%13s", buf)); */

	/* Shooting Skill (with current bow) */
	o_ptr = &inventory[INVEN_BOW];
	xskill[SK_THB] = p_ptr->skill[SK_THB] + p_ptr->to_h_shooting + object_to_h(o_ptr);

	/* Base skill */
	shooting_hit = p_ptr->dis_to_h_shooting;

	/* Apply weapon bonuses */
	if (object_known_p(o_ptr)) shooting_hit += object_to_h(o_ptr);

	/* Range attacks
	sprintf(buf, "(%+d)", hit);
	Term_putstr(col, 12, -1, TERM_WHITE, "Shoot");
	Term_putstr(col + 5, 12, -1, TERM_L_BLUE, format("%13s", buf)); */

	/* Throwing Skill */
	xskill[SK_THT] = p_ptr->skill[SK_THT] + p_ptr->to_h_throwing;

	/* Base skill */
	thrown_hit = p_ptr->dis_to_h_throwing;

	/* Basic abilities */
	xskill[SK_DIS] = p_ptr->skill[SK_DIS];
	xskill[SK_DEV] = p_ptr->skill[SK_DEV];
	xskill[SK_SAV] = p_ptr->skill[SK_SAV];
	xskill[SK_STL] = p_ptr->skill[SK_STL];
	xskill[SK_PER] = p_ptr->skill[SK_PER];
	xskill[SK_MOB] = p_ptr->skill[SK_MOB];
	xskill[SK_ALC] = p_ptr->skill[SK_ALC];
	xskill[SK_MAP] = p_ptr->skill[SK_MAP];

	put_str("Fighting", 11, col);
	desc = likert(xskill[SK_THN], 2);
	c_put_str(likert_color, format("%4d (%+2d)", xskill[SK_THN], melee_hit), 11, col+13);
	/* if (cheat_wizard) c_put_str(likert_color, format("%9d", xskill[SK_THN]), 11, col+13);
	else c_put_str(likert_color, format("%9s", desc), 11, col+13); */

	put_str("Shooting", 12, col);
	desc = likert(xskill[SK_THB], 2);
	c_put_str(likert_color, format("%4d (%+2d)", xskill[SK_THB], shooting_hit), 12, col+13);
	/* if (cheat_wizard) c_put_str(likert_color, format("%9d", xskill[SK_THB]), 12, col+13);
	else c_put_str(likert_color, format("%9s", desc), 12, col+13); */

	put_str("Throwing", 13, col);
	desc = likert(xskill[SK_THT], 2);
	c_put_str(likert_color, format("%4d (%+2d)", xskill[SK_THT], thrown_hit), 13, col+13);
	/* if (cheat_wizard) c_put_str(likert_color, format("%9d", xskill[SK_THT]), 13, col+13);
	else c_put_str(likert_color, format("%9s", desc), 13, col+13); */

	put_str("Magic Device", 14, col);
	desc = likert(xskill[SK_DEV], 5);
	c_put_str(likert_color, format("%9d%%", xskill[SK_DEV]), 14, col+12);

	put_str("Saving Throw", 15, col);
	desc = likert(xskill[SK_SAV], 5);  
	c_put_str(likert_color, format("%9d%%", xskill[SK_SAV]), 15, col+12);
	/* if (cheat_wizard) c_put_str(likert_color, format("%9d", xskill[SK_SAV]), 21, col+13);
	else c_put_str(likert_color, format("%9s", desc), 21, col+13); */

	/* desc = likert(xskill[SK_DEV], 5);
	if (cheat_wizard) c_put_str(likert_color, format("%9d", xskill[SK_DEV]), 19, col+13);
	else c_put_str(likert_color, format("%9s", desc), 19, col+13); */

	/* put_str("Jumping", 14, col);
	desc = likert(xskill[SK_MOB], 10);
	if (cheat_wizard) c_put_str(likert_color, format("%9d", xskill[SK_MOB]), 14, col+13);
	else c_put_str(likert_color, format("%9s", desc), 14, col+13); */

	put_str("Stealth", 17, col);
	desc = likert(xskill[SK_STL], 1);
	if (cheat_wizard) c_put_str(likert_color, format("%9d", xskill[SK_STL]), 17, col+13);
	else c_put_str(likert_color, format("%9s", desc), 17, col+13);

	put_str("Perception", 18, col);
	if (p_ptr->alertness)
	{
		int perception = xskill[SK_PER];
		if (perception > 100) perception = 100;
		int alert = xskill[SK_PER] + (p_ptr->alertness * 25);
		if (alert > 100) alert = 100;
		desc = likert(perception, 5);
		c_put_str(likert_color, format("%3d%% (%2d%%)", perception, alert), 18, col+12);
	}
	else
	{
		desc = likert(xskill[SK_PER], 5);
		c_put_str(likert_color, format("%9d%%", xskill[SK_PER]), 18, col+12);
	}

	put_str("Jumping", 19, col);
	desc = likert(xskill[SK_MOB], 5);
	c_put_str(likert_color, format("%9d%%", xskill[SK_MOB]), 19, col+12);

	put_str("Alchemy", 20, col);
	desc = likert(xskill[SK_ALC], 5);
	c_put_str(likert_color, format("%9d%%", xskill[SK_ALC]), 20, col+12);

	put_str("Navigation", 21, col);
	desc = likert(xskill[SK_MAP], 5);
	c_put_str(likert_color, format("%9d%%", xskill[SK_MAP]), 21, col+12);
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
static const c_flag_desc flag_list[3][13] =
{
	{
		{2, TR2_NO_BLIND,			 0, "No  Blind:"},
		{2, TR2_FREE_ACT,			 0, "Free Actn:"},
		{2, TR2_HOLD_LIFE,			 0, "Hold Life:"},
		{2, TR2_BRAVERY,			 0, "  Bravery:"},
		{2, TR2_NO_DISEASE,			 0, "No Dsease:"},
		{2, TR2_NO_STUN,			 0, "  No Stun:"},
		{2, TR2_NO_POISON,		    	 0, "No Poison:"},
		{2, TR2_NO_CUT,				 0, "   No Cut:"},
		{2, TR2_NO_CONF,			 0, "  No Conf:"},
		{3, TR3_REGEN,				 0, "    Regen:"},
		{3, TR3_FEATHER,			 0, "Fthr Fall:"},
		{3, TR3_GLOW,	 TR3_LITE_MASK,	 "    Light:"},
		{99,0, 0,						"          "}
	},
	{
		{3, TR3_TELEPATHY,			 0, "Telepathy:"},
		{3, TR3_SEE_INVIS,			 0, "See Invis:"},
		{3, TR3_INVIS,				 0, "Invisible:"},
		{3, TR3_LUCK,				 0, "     Luck:"},
		{1, TR1_STEALTH,			 0, "  Stealth:"},
		{1, TR1_JUMPING,			 0, "  Jumping:"},
		{1, TR1_PERCEPTION,			 0, " Percptn.:"},
		{1, TR1_INFRA,				 0, "Infra-vsn:"},
		{1, TR1_AMBUSH,				 0, "   Ambush:"},
		{1, TR1_BLOWS,				 0, "    Blows:"},
		{1, TR1_MYSTIC_RANGE,			 0, "    Range:"},
		{3, TR3_MIGHTY_THROW,			 0, "Mgt Throw:"},
		{99,0, 0,						"          "}
	},
	{
		{1, TR1_SPEED,				 0, "    Speed:"},
		{1, TR1_MANA,				 0, "     Mana:"},
		{1, TR1_HEALTH,				 0, "   Health:"},
		{1, TR1_SP_DUR,				 0, " Spl. Dur:"},
		{1, TR1_SP_DAM,				 0, " Spl. Dam:"},
		{1, TR1_SP_INF,				 0, " Spl. Inf:"},
		{3, TR3_TAINT,				 0, "    Taint:"},
		{3,	TR3_DISRUPT,			 0, "  Disrupt:"},
		{3, TR3_DRAIN_ITEM,			 0, "Drn Items:"},
		{3, TR3_DRAIN_EXP,			 0, "  Drn Exp:"},
		{3, TR3_TELEPORT,			 0, " Teleport:"},
		{3, TR3_AGGRAVATE,			 0, "Aggravate:"},
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
		for (y = 0; y < 13; y++)
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

	for (i = 0, j = 0; i < RS_MAX; i++)
	{
		x = (j / ((RS_MAX + 2) / 3));
		col = 2 + (26 * x);
		row = 14;

		/* Display */
		if (p_ptr->dis_res[i])
		{
			prt(format("%s:", resist_names_short[i]), row + (j % ((RS_MAX + 2) / 3)), col);

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
					c_put_str(attr2, "+", row + (j % ((RS_MAX + 2) / 3)), col + n);
				}

				/* Default */
				else
				{
					c_put_str(attr1, ".", row + (j % ((RS_MAX + 2) / 3)), col + n);
				}
			}

			/* Default */
			c_put_str(TERM_SLATE, ".", row + (j % ((RS_MAX + 2) / 3)), col + n);

			/* Basic Resistance */
			if ((((rp_ptr->res[i] + cp_ptr->res[i]) * p_ptr->lev) / 50) > 0)
			{
				c_put_str(TERM_WHITE, "+", row + (j % ((RS_MAX + 2) / 3)), col + n);
			}

			n++;

			/* Temporary Resist */

			/* Default */
			c_put_str(TERM_SLATE, ".", row + (j % ((RS_MAX + 2) / 3)), col + n);

			/* Check flags */
			if (p_ptr->tim_res[i])
			{
				c_put_str(TERM_WHITE, "+", row + (j % ((RS_MAX + 2) / 3)), col + n);
			}

			/* Resistance value */
			if (p_ptr->tim_res[i]) attr1 = TERM_L_BLUE;
			else if (p_ptr->dis_res[i] > resist_caps[i].normal) 
			{
				if (resist_caps[i].normal > (rp_ptr->res[i] + cp_ptr->res[i])) attr1 = TERM_VIOLET;
				else attr1 = TERM_GREEN;
			}
			else if (p_ptr->dis_res[i] == resist_caps[i].normal) attr1 = TERM_GREEN;
			else if (p_ptr->dis_res[i] >= resist_caps[i].normal / 2) attr1 = TERM_L_GREEN;
			else attr1 = TERM_YELLOW;

			c_put_str(attr1, format("%d%%", p_ptr->dis_res[i]), row + (j % ((RS_MAX + 2) / 3)), col + 6);

			j++;
		}
	}

	for (i = 0; i < 3; i++)
	{
		/* Footer */
		c_put_str(TERM_WHITE, "abcdefghijkl@*", 20, 12 + (26 * i));

		/* Equippy */
		display_player_equippy(21, 12 + (26 * i));
	}
}	

/*
 * Normal display, part 2a
 */
static void display_player_misc_info(void)
{
	int col = 0;
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
	if (p_ptr->shape == SHAPE_HARPY) p="Harpy";
	else if (p_ptr->shape == SHAPE_ANGEL) p="Angel";
	else if (p_ptr->shape == SHAPE_APE) p="Ape";
	else if (p_ptr->shape == SHAPE_NAGA) p="Naga";
	else if (p_ptr->shape == SHAPE_STATUE) p="Statue";
	else if (p_ptr->shape == SHAPE_FAUN) p="Faun";
	else if (p_ptr->shape == SHAPE_GOBLIN) p="Goblin";
	else if (p_ptr->shape == SHAPE_GHOUL) p="Ghoul";
	else if (!rp_ptr->special) p=p_name + rp_ptr->name;
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

	/* Deity */
	put_str("Deity", 6, 1);

	if (p_ptr->obsession_status >= 4) Term_putstr(col+8, 6, -1, TERM_RED, ("Beleth"));
	else if (p_ptr->conflict_status >= 4) Term_putstr(col+8, 6, -1, TERM_YELLOW, ("Discordia"));
	else if (p_ptr->purity_status >= 4) Term_putstr(col+8, 6, -1, TERM_L_GREEN, ("Eostre"));
	else if (p_ptr->transformation_status >= 4) Term_putstr(col+8, 6, -1, TERM_GREEN, ("Cyrridven"));
	else if (p_ptr->deceit_status >= 4) Term_putstr(col+8, 6, -1, TERM_VIOLET, ("Laverna"));
	else Term_putstr(col+8, 6, -1, TERM_L_BLUE, ("None"));

	col = 22;

	/* Level */
	int attr;
	Term_putstr(col, 1, -1, TERM_WHITE, "Level");
	if (p_ptr->lev >= p_ptr->max_lev) attr = TERM_L_BLUE;
	else attr = TERM_BLUE;
	if (p_ptr->max_lev > 9)
	{
		Term_putstr(col+12, 1, -1, attr, format("%3d/%2d", (int)p_ptr->lev, (int)p_ptr->max_lev));
	}
	else
	{
		Term_putstr(col+14, 1, -1, attr, format("%2d/%1d", (int)p_ptr->lev, (int)p_ptr->max_lev));
	}

	/* Current Experience */
	Term_putstr(col, 2, -1, TERM_WHITE, "Cur Exp");
	if (p_ptr->lev >= p_ptr->max_lev) attr = TERM_L_BLUE;
	else attr = TERM_BLUE;
	Term_putstr(col+8, 2, -1, attr, format("%10d", (int)p_ptr->exp));

	/* Maximum Experience */
	Term_putstr(col, 3, -1, TERM_WHITE, "Max Exp");
	Term_putstr(col+8, 3, -1, TERM_L_BLUE, format("%10d", (int)p_ptr->max_exp));

	/* Advance Experience
	Term_putstr(col, 4, -1, TERM_WHITE, "Adv Exp");
	if (p_ptr->lev < PY_MAX_LEVEL)
	{
		s32b advance = ((player_exp[p_ptr->lev - 1] * p_ptr->expfact) / 100L);
		Term_putstr(col + 8, 4, -1, TERM_L_BLUE, format("%10ld", advance));
	}
	else Term_putstr(col+8, 4, -1, TERM_L_BLUE, format("%10s", "********")); */

   	/* Deepest recall level */
   	Term_putstr(col, 4, -1, TERM_WHITE, "Cur Depth");
   	if (!depth_in_feet) 
	{
		Term_putstr(col+14, 4, -1, TERM_L_BLUE, format("%4d", (int)p_ptr->max_depth));
	}
	else 
	{
		Term_putstr(col+11, 4, -1, TERM_L_BLUE, format("%4d Ft", (int)p_ptr->max_depth * 50));
	}

   	/* Minimum depth */
   	Term_putstr(col, 5, -1, TERM_WHITE, "Min Depth");
   	if (!depth_in_feet) 
	{
		Term_putstr(col+14, 5, -1, TERM_L_BLUE, format("%4d", (int)p_ptr->min_depth));
	}
	else 
	{
		Term_putstr(col+11, 5, -1, TERM_L_BLUE, format("%4d Ft", (int)p_ptr->min_depth * 50));
	}

	/* Gold */
	Term_putstr(col, 6, -1, TERM_WHITE, "Gold");
	Term_putstr(col + 8, 6, -1, TERM_L_BLUE,format("%10ld", p_ptr->au));

	/* Burden */
	if (p_ptr->total_weight % 10)
		sprintf(buf, "%ld.%ld/%ld", p_ptr->total_weight / 10L, p_ptr->total_weight % 10L,
		adj_str_wgt[p_stat(A_STR)] * 6L);
	else 
		sprintf(buf, "%ld/%ld", p_ptr->total_weight / 10L, adj_str_wgt[p_stat(A_STR)] * 6L);
	Term_putstr(col, 11, -1, TERM_WHITE, "Burden");
	Term_putstr(col + 8, 11, -1, TERM_L_BLUE, format("%10s", buf));

	col = 43;

	/* Deity bonuses */
	int multi = 0;
	int roww = 11;

	if ((p_ptr->obsession_status > 1) && (!(p_ptr->taint)))
	{
		if ((p_ptr->obsession_status == 5) || (p_ptr->obsession_status == 6)) multi = 2;
		else if ((p_ptr->obsession_status == 7) || (p_ptr->obsession_status == 8)) multi = -1;
		else multi = 1;

		Term_putstr(col, roww, -1, TERM_RED, "Beleth");
		roww++;
		print_deity_bonuses(roww, p_ptr->obsession_bonus_a, p_ptr->obsession_bonus_b, multi, TERM_RED);
		roww++;
	}
	if ((p_ptr->conflict_status > 1) && (!(p_ptr->taint)))
	{
		if ((p_ptr->conflict_status == 5) || (p_ptr->conflict_status == 6)) multi = 2;
		else if ((p_ptr->conflict_status == 7) || (p_ptr->conflict_status == 8)) multi = -1;
		else multi = 1;

		Term_putstr(col, roww, -1, TERM_YELLOW, "Discordia");
		roww++;
		print_deity_bonuses(roww, p_ptr->conflict_bonus_a, p_ptr->conflict_bonus_b, multi, TERM_YELLOW);
		roww++;
	}
	if ((p_ptr->purity_status > 1) && (!(p_ptr->taint)))
	{
		if ((p_ptr->purity_status == 5) || (p_ptr->purity_status == 6)) multi = 2;
		else if ((p_ptr->purity_status == 7) || (p_ptr->purity_status == 8)) multi = -1;
		else multi = 1;

		Term_putstr(col, roww, -1, TERM_L_GREEN, "Eostre");
		roww++;
		print_deity_bonuses(roww, p_ptr->purity_bonus_a, p_ptr->purity_bonus_b, multi, TERM_L_GREEN);
		roww++;
	}
	if ((p_ptr->transformation_status > 1) && (!(p_ptr->taint)))
	{
		if ((p_ptr->transformation_status == 5) || (p_ptr->transformation_status == 6)) multi = 2;
		else if ((p_ptr->transformation_status == 7) || (p_ptr->transformation_status == 8)) multi = -1;
		else multi = 1;

		Term_putstr(col, roww, -1, TERM_GREEN, "Cyrridven");
		roww++;
		print_deity_bonuses(roww, p_ptr->transformation_bonus_a, p_ptr->transformation_bonus_b, multi, TERM_GREEN);
		roww++;
	}
	if ((p_ptr->deceit_status > 1) && (!(p_ptr->taint)))
	{
		if ((p_ptr->deceit_status == 5) || (p_ptr->deceit_status == 6)) multi = 2;
		else if ((p_ptr->deceit_status == 7) || (p_ptr->deceit_status == 8)) multi = -1;
		else multi = 1;

		Term_putstr(col, roww, -1, TERM_VIOLET, "Laverna");
		roww++;
		print_deity_bonuses(roww, p_ptr->deceit_bonus_a, p_ptr->deceit_bonus_b, multi, TERM_VIOLET);
		roww++;
	}
}


/*
 * Special display used for autorollers, part 2a
 */
static void display_player_basic_info(void)
{
	cptr p;

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
	col = 55;

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
	int special_bonus = 0;

	char buf[80];

	/* Row */
	row = 2;

	/* Column */
	col = 45;

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

		/*** Hack: wounds and shapes affect race bonus ***/
		special_bonus = 0;

		if (i == 0)
		{
			if (p_ptr->wound_vigor == 2) special_bonus += -1;
			if (p_ptr->wound_vigor == 4) special_bonus += -1;

			if (p_ptr->shape == SHAPE_APE) special_bonus += 5;
			if (p_ptr->shape == SHAPE_NAGA) special_bonus += 2;
			if (p_ptr->shape == SHAPE_GOBLIN) special_bonus -= 2;
			if (p_ptr->shape == SHAPE_GHOUL) special_bonus -= 1;
		}
		else if (i == 1)
		{
			if (p_ptr->wound_wit == 2) special_bonus += -1;
			if (p_ptr->wound_wit == 4) special_bonus += -1;

			if (p_ptr->shape == SHAPE_APE) special_bonus -= 5;
			if (p_ptr->shape == SHAPE_NAGA) special_bonus += 5;
			if (p_ptr->shape == SHAPE_FAUN) special_bonus -= 2;
			if (p_ptr->shape == SHAPE_GOBLIN) special_bonus -= 2;
			if (p_ptr->shape == SHAPE_GHOUL) special_bonus -= 1;
		}
		else if (i == 2)
		{
			if (p_ptr->wound_wit == 3) special_bonus += -1;
			if (p_ptr->wound_wit == 4) special_bonus += -1;

			if (p_ptr->shape == SHAPE_HARPY) special_bonus -= 5;
			if (p_ptr->shape == SHAPE_ANGEL) special_bonus += 5;
			if (p_ptr->shape == SHAPE_FAUN) special_bonus += 2;
			if (p_ptr->shape == SHAPE_GOBLIN) special_bonus -= 2;
			if (p_ptr->shape == SHAPE_GHOUL) special_bonus -= 1;
		}
		else if (i == 3)
		{
			if (p_ptr->wound_grace == 2) special_bonus += -1;
			if (p_ptr->wound_grace == 4) special_bonus += -1;

			if (p_ptr->shape == SHAPE_HARPY) special_bonus += 5;
			if (p_ptr->shape == SHAPE_ANGEL) special_bonus += 2;
			if (p_ptr->shape == SHAPE_GOBLIN) special_bonus += 2;
			if (p_ptr->shape == SHAPE_GHOUL) special_bonus -= 1;
		}
		else if (i == 4)
		{
			if (p_ptr->wound_vigor == 3) special_bonus += -1;
			if (p_ptr->wound_vigor == 4) special_bonus += -1;

			if (p_ptr->shape == SHAPE_APE) special_bonus += 2;
			if (p_ptr->shape == SHAPE_STATUE) special_bonus += 5;
			if (p_ptr->shape == SHAPE_FAUN) special_bonus += 2;
			if (p_ptr->shape == SHAPE_GOBLIN) special_bonus -= 2;
			if (p_ptr->shape == SHAPE_GHOUL) special_bonus -= 1;
		}
		else if (i == 5)
		{
			if (p_ptr->wound_grace == 3) special_bonus += -1;
			if (p_ptr->wound_grace == 4) special_bonus += -1;

			if (p_ptr->shape == SHAPE_HARPY) special_bonus -= 5;
			if (p_ptr->shape == SHAPE_ANGEL) special_bonus += 5;
			if (p_ptr->shape == SHAPE_APE) special_bonus -= 2;
			if (p_ptr->shape == SHAPE_STATUE) special_bonus += 2;
			if (p_ptr->shape == SHAPE_GOBLIN) special_bonus -= 2;
			if (p_ptr->shape == SHAPE_GHOUL) special_bonus -= 1;
		}

		/*** Hack: goddesses give bonuses or penalties ***/
		/* Blessed by or a follower of Beleth */
		if ((p_ptr->obsession_status >= 2) && (p_ptr->obsession_status <= 4))
		{
			if (p_ptr->obsession_bonus_a == i) special_bonus+= 2;
			if (p_ptr->obsession_bonus_b == i) special_bonus+= 1;
		}
		/* Beleth is happy or very happy */
		if ((p_ptr->obsession_status >= 5) && (p_ptr->obsession_status <= 6))
		{
			if (p_ptr->obsession_bonus_a == i) special_bonus+= 3;
			if (p_ptr->obsession_bonus_b == i) special_bonus+= 3;
		}
		/* Beleth is angry or very angry */
		if (p_ptr->obsession_status >= 7)
		{
			if (p_ptr->obsession_bonus_a == i) special_bonus+= -2;
			if (p_ptr->obsession_bonus_b == i) special_bonus+= -1;
		}

		/* Blessed by or a follower of Discordia */
		if ((p_ptr->conflict_status >= 2) && (p_ptr->conflict_status <= 4))
		{
			if (p_ptr->conflict_bonus_a == i) special_bonus+= 2;
			if (p_ptr->conflict_bonus_b == i) special_bonus+= 1;
		}
		/* Discordia is happy or very happy */
		if ((p_ptr->conflict_status >= 5) && (p_ptr->conflict_status <= 6))
		{
			if (p_ptr->conflict_bonus_a == i) special_bonus+= 3;
			if (p_ptr->conflict_bonus_b == i) special_bonus+= 3;
		}
		/* Discordia is angry or very angry */
		if (p_ptr->conflict_status >= 7)
		{
			if (p_ptr->conflict_bonus_a == i) special_bonus+= -2;
			if (p_ptr->conflict_bonus_b == i) special_bonus+= -1;
		}

		/* Blessed by or a follower of Eostre */
		if ((p_ptr->purity_status >= 2) && (p_ptr->purity_status <= 4))
		{
			if (p_ptr->purity_bonus_a == i) special_bonus+= 2;
			if (p_ptr->purity_bonus_b == i) special_bonus+= 1;
		}
		/* Eostre is happy or very happy */
		if ((p_ptr->purity_status >= 5) && (p_ptr->purity_status <= 6))
		{
			if (p_ptr->purity_bonus_a == i) special_bonus+= 3;
			if (p_ptr->purity_bonus_b == i) special_bonus+= 3;
		}
		/* Eostre is angry or very angry */
		if (p_ptr->purity_status >= 7)
		{
			if (p_ptr->purity_bonus_a == i) special_bonus+= -2;
			if (p_ptr->purity_bonus_b == i) special_bonus+= -1;
		}

		/* Blessed by or a follower of Cyrridven */
		if ((p_ptr->transformation_status >= 2) && (p_ptr->transformation_status <= 4))
		{
			if (p_ptr->transformation_bonus_a == i) special_bonus+= 2;
			if (p_ptr->transformation_bonus_b == i) special_bonus+= 1;
		}
		/* Cyrridven is happy or very happy */
		if ((p_ptr->transformation_status >= 5) && (p_ptr->transformation_status <= 6))
		{
			if (p_ptr->transformation_bonus_a == i) special_bonus+= 3;
			if (p_ptr->transformation_bonus_b == i) special_bonus+= 3;
		}
		/* Cyrridven is angry or very angry */
		if (p_ptr->transformation_status >= 7)
		{
			if (p_ptr->transformation_bonus_a == i) special_bonus+= -2;
			if (p_ptr->transformation_bonus_b == i) special_bonus+= -1;
		}

		/* Blessed by or a follower of Laverna */
		if ((p_ptr->deceit_status >= 2) && (p_ptr->deceit_status <= 4))
		{
			if (p_ptr->deceit_bonus_a == i) special_bonus+= 2;
			if (p_ptr->deceit_bonus_b == i) special_bonus+= 1;
		}
		/* Laverna is happy or very happy */
		if ((p_ptr->deceit_status >= 5) && (p_ptr->deceit_status <= 6))
		{
			if (p_ptr->deceit_bonus_a == i) special_bonus+= 3;
			if (p_ptr->deceit_bonus_b == i) special_bonus+= 3;
		}
		/* Laverna is angry or very angry */
		if (p_ptr->deceit_status >= 7)
		{
			if (p_ptr->deceit_bonus_a == i) special_bonus+= -2;
			if (p_ptr->deceit_bonus_b == i) special_bonus+= -1;
		}

		/* Finally print the race bonus */
		sprintf(buf, "%+2d", n + special_bonus);
		c_put_str(TERM_L_BLUE, buf, row+i, col+16);

		/* Equipment Bonus */
		sprintf(buf, "%+3d", p_ptr->stat_add[i] - special_bonus);
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
	col = 45;

	/* Header */
	c_put_str(TERM_WHITE, "abcdefghijkl@", row-1, col+11);
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

		/*** Hack: wounds and shapes affect race bonus ***/
		if (stat == 0)
		{
			if (p_ptr->wound_vigor == 2) n += -1;
			if (p_ptr->wound_vigor == 4) n += -1;

			if (p_ptr->shape == SHAPE_APE) n += 5;
			if (p_ptr->shape == SHAPE_NAGA) n += 2;
		}
		else if (stat == 1)
		{
			if (p_ptr->wound_wit == 2) n += -1;
			if (p_ptr->wound_wit == 4) n += -1;

			if (p_ptr->shape == SHAPE_APE) n -= 5;
			if (p_ptr->shape == SHAPE_NAGA) n += 5;
		}
		else if (stat == 2)
		{
			if (p_ptr->wound_wit == 3) n += -1;
			if (p_ptr->wound_wit == 4) n += -1;

			if (p_ptr->shape == SHAPE_HARPY) n -= 5;
			if (p_ptr->shape == SHAPE_ANGEL) n += 5;
		}
		else if (stat == 3)
		{
			if (p_ptr->wound_grace == 2) n += -1;
			if (p_ptr->wound_grace == 4) n += -1;

			if (p_ptr->shape == SHAPE_HARPY) n += 5;
			if (p_ptr->shape == SHAPE_ANGEL) n += 2;
		}
		else if (stat == 4)
		{
			if (p_ptr->wound_vigor == 3) n += -1;
			if (p_ptr->wound_vigor == 4) n += -1;

			if (p_ptr->shape == SHAPE_APE) n += 2;
			if (p_ptr->shape == SHAPE_STATUE) n += 5;
		}
		else if (stat == 5)
		{
			if (p_ptr->wound_grace == 3) n += -1;
			if (p_ptr->wound_grace == 4) n += -1;

			if (p_ptr->shape == SHAPE_HARPY) n -= 5;
			if (p_ptr->shape == SHAPE_ANGEL) n += 5;
			if (p_ptr->shape == SHAPE_APE) n -= 2;
			if (p_ptr->shape == SHAPE_STATUE) n += 2;
		}

		/*** Hack: goddesses give bonuses or penalties ***/
		/* Blessed by or a follower of Beleth */
		if ((p_ptr->obsession_status >= 2) && (p_ptr->obsession_status <= 4))
		{
			if (p_ptr->obsession_bonus_a == stat) n+= 2;
			if (p_ptr->obsession_bonus_b == stat) n+= 1;
		}
		/* Beleth is happy or very happy */
		if ((p_ptr->obsession_status >= 5) && (p_ptr->obsession_status <= 6))
		{
			if (p_ptr->obsession_bonus_a == stat) n+= 3;
			if (p_ptr->obsession_bonus_b == stat) n+= 3;
		}
		/* Beleth is angry or very angry */
		if (p_ptr->obsession_status >= 7)
		{
			if (p_ptr->obsession_bonus_a == stat) n+= -2;
			if (p_ptr->obsession_bonus_b == stat) n+= -1;
		}

		/* Blessed by or a follower of Discordia */
		if ((p_ptr->conflict_status >= 2) && (p_ptr->conflict_status <= 4))
		{
			if (p_ptr->conflict_bonus_a == stat) n+= 2;
			if (p_ptr->conflict_bonus_b == stat) n+= 1;
		}
		/* Discordia is happy or very happy */
		if ((p_ptr->conflict_status >= 5) && (p_ptr->conflict_status <= 6))
		{
			if (p_ptr->conflict_bonus_a == stat) n+= 3;
			if (p_ptr->conflict_bonus_b == stat) n+= 3;
		}
		/* Discordia is angry or very angry */
		if (p_ptr->conflict_status >= 7)
		{
			if (p_ptr->conflict_bonus_a == stat) n+= -2;
			if (p_ptr->conflict_bonus_b == stat) n+= -1;
		}

		/* Blessed by or a follower of Eostre */
		if ((p_ptr->purity_status >= 2) && (p_ptr->purity_status <= 4))
		{
			if (p_ptr->purity_bonus_a == stat) n+= 2;
			if (p_ptr->purity_bonus_b == stat) n+= 1;
		}
		/* Eostre is happy or very happy */
		if ((p_ptr->purity_status >= 5) && (p_ptr->purity_status <= 6))
		{
			if (p_ptr->purity_bonus_a == stat) n+= 3;
			if (p_ptr->purity_bonus_b == stat) n+= 3;
		}
		/* Eostre is angry or very angry */
		if (p_ptr->purity_status >= 7)
		{
			if (p_ptr->purity_bonus_a == stat) n+= -2;
			if (p_ptr->purity_bonus_b == stat) n+= -1;
		}

		/* Blessed by or a follower of Cyrridven */
		if ((p_ptr->transformation_status >= 2) && (p_ptr->transformation_status <= 4))
		{
			if (p_ptr->transformation_bonus_a == stat) n+= 2;
			if (p_ptr->transformation_bonus_b == stat) n+= 1;
		}
		/* Cyrridven is happy or very happy */
		if ((p_ptr->transformation_status >= 5) && (p_ptr->transformation_status <= 6))
		{
			if (p_ptr->transformation_bonus_a == stat) n+= 3;
			if (p_ptr->transformation_bonus_b == stat) n+= 3;
		}
		/* Cyrridven is angry or very angry */
		if (p_ptr->transformation_status >= 7)
		{
			if (p_ptr->transformation_bonus_a == stat) n+= -2;
			if (p_ptr->transformation_bonus_b == stat) n+= -1;
		}

		/* Blessed by or a follower of Laverna */
		if ((p_ptr->deceit_status >= 2) && (p_ptr->deceit_status <= 4))
		{
			if (p_ptr->deceit_bonus_a == stat) n+= 2;
			if (p_ptr->deceit_bonus_b == stat) n+= 1;
		}
		/* Laverna is happy or very happy */
		if ((p_ptr->deceit_status >= 5) && (p_ptr->deceit_status <= 6))
		{
			if (p_ptr->deceit_bonus_a == stat) n+= 3;
			if (p_ptr->deceit_bonus_b == stat) n+= 3;
		}
		/* Laverna is angry or very angry */
		if (p_ptr->deceit_status >= 7)
		{
			if (p_ptr->deceit_bonus_a == stat) n+= -2;
			if (p_ptr->deceit_bonus_b == stat) n+= -1;
		}

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
	col = 45;

	/* Footer */
	c_put_str(TERM_WHITE, "abcdefghijkl@", row+6, col+11);

	/* Equippy */
	display_player_equippy(row+7, col+11);
}

/*
 * Display the character on the screen (several different modes)
 *
 * The top two lines, and the bottom line (or two) are left blank.
 */
void display_player(byte mode)
{
	/* Erase screen */
	clear_from(0);

	/* Resistances */
	if (mode == CSCREEN_RESISTS)
	{
		display_player_flag_info();
		display_player_resists_info();

		return;
	}

	/* Auto-roller (top-left corner only */
	if (mode == CSCREEN_ROLLER)
	{
		display_player_basic_info();

		return;
	}

	/* Birth - no line for equipment bonuses */
	if (mode == CSCREEN_BIRTH) display_player_stat_birth();
	/* Dump - numberical display */
	else if (mode == CSCREEN_DUMP) display_player_stat_dump();
	/* Standard */
	else display_player_stat_info();

	display_player_misc_info();

	/* Extra info */
	display_player_xtra_info();
	display_player_skill_info();
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
	{ TR1_INT,        "memory" },
	{ TR1_WIS,        "wisdom" },
	{ TR1_DEX,        "dexterity" },
	{ TR1_CON,        "constitution" },
	{ TR1_CHR,        "presence" }
};

/*
 * Besides stats, these are the other player traits
 * which may be affected by an object's pval
 */
static const o_flag_desc pval_flags1_desc[] =
{
	{ TR1_STEALTH,    "stealth" },
	{ TR1_PERCEPTION, "perception" },
	{ TR1_INFRA,      "infravision" },
	{ TR1_SPEED,      "speed" },
	{ TR1_BLOWS,      "attacks" },
	{ TR1_SHOTS,      "shots" },
	{ TR1_MIGHT,      "might" },
	{ TR1_MANA,       "mana" },
	{ TR1_SP_DUR,     "spell duration" },
	{ TR1_SP_DAM,     "spell damage" },
	{ TR1_SP_INF,     "spell influence" },
	{ TR1_HEALTH,     "health" },
	{ TR1_MELEE,     "melee" },
	{ TR1_ARCHERY,     "archery skill" },
	{ TR1_ESCAPES,     "escapes" },
	{ TR1_THROW_SKILL,     "throwing" },
	{ TR1_JUMPING,     "jumping" },
	{ TR1_MYSTIC_RANGE,     "range with spells and weapons" },
	{ TR1_AMBUSH,     "ambush chance" }
};

/*
 * Elemental brands for weapons
 */
static const o_flag_desc weapon_flags_desc[] =
{
	{ TR2_WOUNDING,		"hit from this weapon can cause bleeding" },
	{ TR2_TERROR,		"hit from this weapon can terrify the enemy" },
	{ TR2_IMPACT,		"hit from this weapon can cause an earthquake" },
	{ TR2_DEADLY_CRIT,	"critical hit from this weapon does triple damage" }
};

/*
 * Slay names 
 */
cptr slay_names[SL_MAX] =
{
	"vs. evil",
	"vs. creatures of Aether",
	"vs. creatures of Skultgard",
	"vs. creatures of Thornwild",
	"vs. creatures of Chaos",
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
	{ TR1_SUST_INT,   "memory" },
	{ TR1_SUST_WIS,   "wisdom" },
	{ TR1_SUST_DEX,   "dexterity" },
	{ TR1_SUST_CON,   "constitution" },
	{ TR1_SUST_CHR,   "presence" }
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
	{ TR3_PRO_CHAOS,	"protection from lesser creatures of Chaos" },
	{ TR3_PRO_THORNWILD,	"protection from lesser creatures of Thornwild" },
	{ TR3_PRO_SKULTGARD,	"protection from lesser creatures of Skultgard" },
	{ TR3_PRO_AETHER,	"protection from lesser creatures of Aether" },
	{ TR3_FEATHER,		"feather falling" },
	{ TR3_GLOW,			"brighter light" },
	{ TR3_REGEN,		"regeneration" },
	{ TR3_TELEPATHY,	"telepathy" },
	{ TR3_SEE_INVIS,	"see invisible" },
	{ TR3_INVIS,		"invisibility" },
	{ TR3_LUCK,		"luck finding items" },
	{ TR3_MIGHTY_THROW,	"double thrown range and thrown weapon damage" },
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
	{ TR3_DRAIN_ITEM,	"item drain" },
	{ TR3_TAINT,		"a taint" }
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
static bool spoiler_ignore_damage(bool spoil, const object_type *o_ptr, int res_type, int slay_type)
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
static cptr *spoiler_damage_aux(bool spoil, const object_type *o_ptr, cptr *desc_x_ptr)
{
	u32b f1, f2, f3;

	/* Extract the flags */
	if (spoil) object_flags(o_ptr, &f1, &f2, &f3);
	else object_flags_known(o_ptr, &f1, &f2, &f3);

	if (object_hates_acid(o_ptr) && !spoiler_ignore_damage(spoil, o_ptr, RS_ACD, SL_BRAND_ACID))
		*desc_x_ptr++ = "acid";
	if (object_hates_elec(o_ptr) && !spoiler_ignore_damage(spoil, o_ptr, RS_ELC, SL_BRAND_ELEC))
		*desc_x_ptr++ = "electricity";
	if (object_hates_fire(o_ptr) && !spoiler_ignore_damage(spoil, o_ptr, RS_FIR, SL_BRAND_FIRE))
		*desc_x_ptr++ = "fire";
	if (object_hates_cold(o_ptr) && !spoiler_ignore_damage(spoil, o_ptr, RS_CLD, SL_BRAND_COLD))
		*desc_x_ptr++ = "cold";
	if (object_hates_rust(o_ptr) && !(f3 & TR3_IGNORE_NON_ELEM)) *desc_x_ptr++ = "rusting attacks";
	if (object_hates_rot(o_ptr) && !(f3 & TR3_IGNORE_NON_ELEM)) *desc_x_ptr++ = "rotting attacks";

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
static void obj_top(const object_type *o_ptr, bool real)
{
	char name[80];

	if (real) object_desc(name, sizeof(name), o_ptr, FALSE, 2);
	else object_desc_store(name, sizeof(name), o_ptr, FALSE, 0);

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
void screen_object(const object_type *o_ptr, bool real)
{
	/* Set text_out hook */
	text_out_hook = text_out_to_screen;

	/* Load screen */
	screen_save();

	/* Begin recall */
	Term_gotoxy(0, 1);

	/* Actually display the item */
	list_object(o_ptr, OBJECT_INFO_KNOWN);

	/* Real objects (not recall) */
	if (real) 
	{
		/* Weapon Analysis */
		if (weapon_p(o_ptr) || (o_ptr->tval == TV_DIGGING)) analyze_weapon(o_ptr);
		if ((o_ptr->tval == TV_BODY_ARMOR) || (o_ptr->tval == TV_DRAG_ARMOR)) analyze_armor(o_ptr);
		if (o_ptr->tval == TV_ARROW) analyze_ammo(o_ptr);

		/* Display object history */
		display_object_history(o_ptr);
	}

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
 * Determine the "Activation" (if any) for an artifact
 */
bool query_activation(char *text, size_t max, int *time1, int *time2, const object_type *o_ptr)
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
	if (power_info[j].desc != NULL) my_strcpy(text, power_info[j].desc, max);

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
 * Display information about weapons
 */
void analyze_weapon(const object_type *o_ptr)
{
	int i;
	int dam = 0;
	int penalty = 0;
	int str_needed = 0, dex_needed = 0, max_blows;

	byte ds = object_ds(o_ptr);
	byte dd = object_dd(o_ptr);
	byte blows = calc_blows(o_ptr, FALSE);

	byte slays[SL_MAX];
	bool any_slay = FALSE;

	text_out("\n");

	/* How much STR you need */
	for (i = 0; i < 31; i++)
	{
		if (adj_str_hold[i] >= object_weight(o_ptr) / 10)
		{
			str_needed = i;
			break;
		}
	}

	/* Blessed weapon? */
	bool blessed_weapon = TRUE;
	if ((o_ptr->tval == TV_SWORD) || (o_ptr->tval == TV_POLEARM)) blessed_weapon = FALSE;
	if (o_ptr->e_idx == EGO_BLESS_BLADE) blessed_weapon = TRUE;
	if (o_ptr->e_idx == EGO_SLAY_EVIL) blessed_weapon = TRUE;
	if (o_ptr->e_idx == EGO_HA) blessed_weapon = TRUE;
	if (o_ptr->a_idx == ART_AEGLOS) blessed_weapon = TRUE;
	if (o_ptr->a_idx == ART_OROME) blessed_weapon = TRUE;
	if (o_ptr->a_idx == ART_EONWE) blessed_weapon = TRUE;
	if (o_ptr->a_idx == ART_LAWGIVER) blessed_weapon = TRUE;
	if (o_ptr->a_idx == ART_WRATH) blessed_weapon = TRUE;
	if (o_ptr->a_idx == ART_ULMO) blessed_weapon = TRUE;
	if (o_ptr->a_idx == ART_POSEIDON) blessed_weapon = TRUE;

	/* Find out the effective weapon weight */
	int wgt_val = (object_weight(o_ptr) / 10);
	if (wgt_val > 29) wgt_val = 29;

	if (!((cp_ptr->flags & CF_BLESS_WEAPON) && (!blessed_weapon)))
	{
		if ((cp_ptr->flags & CF_BETTER_BLOWS) && (wgt_val > 12)) wgt_val--;
		if (cp_ptr->flags & CF_BETTER_BLOWS) wgt_val--;
	}

	if (cp_ptr->flags & CF_WORSE_BLOWS) wgt_val++;

	if (wgt_val < 0) wgt_val = 0;
	if (wgt_val > 29) wgt_val = 29;

	/* Maximum number of blows? How much DEX needed for that? */
	max_blows = weapon_wgt_blows[wgt_val];
	for (i = 0; i < 31; i++)
	{
		if (adj_dex_blows[i] == max_blows)
		{
			dex_needed = i;
			break;
		}
	}

	/* Penalties for too low STR */
	if (str_needed > p_stat(A_STR))
	{
		penalty = (object_weight(o_ptr) / 10) - adj_str_hold[p_stat(A_STR)];
	}

	/* If you cannot use the weapon fully competently, display info about needed stats */
	if (str_needed > p_stat(A_STR))
	{
		text_out_c(TERM_L_RED, "You need ");
		text_out_c(TERM_L_RED, format("%d ", str_needed));
		if (dex_needed > p_stat(A_DEX))
		{
			text_out_c(TERM_L_RED, "STR to use this weapon effectively and ");
			text_out_c(TERM_L_RED, format("%d ", dex_needed));
			text_out_c(TERM_L_RED, "DEX to get the full ");
			text_out_c(TERM_L_RED, format("%d ", max_blows));
			text_out_c(TERM_L_RED, "blows with it!\n\n");
		}
		else
		{
			text_out_c(TERM_L_RED, "STR to use this weapon effectively!\n\n");
		}
	}
	else if (dex_needed > p_stat(A_DEX))
	{
		text_out_c(TERM_L_RED, "You need ");
		text_out_c(TERM_L_RED, format("%d ", dex_needed));
		text_out_c(TERM_L_RED, "DEX to get the full ");
		text_out_c(TERM_L_RED, format("%d ", max_blows));
		text_out_c(TERM_L_RED, "blows with this weapon!\n\n");
	}

	/* Print damage */
	ds = (ds * 2) / (penalty + 2);
	dd = (dd * 2) / (penalty + 2);
	dam = ((ds + 1) * 5 * dd);

	text_out("Using this weapon, you are, in your current condition, able to score ");
	text_out_c(TERM_L_GREEN, format("%d ", blows));
	if ((blows > 1) && (p_ptr->fencing) && (o_ptr->tval == TV_SWORD))
	{
		text_out("blows per round, and ");
		text_out_c(TERM_L_GREEN, format("%d ", p_ptr->fencing));
		if (p_ptr->fencing == 1) text_out("extra blow ");
		else text_out("extra blows ");
		text_out("against visible persons and humanoids because of your superior fencing skill. Each blow will do an average damage of ");
	}
	else if (blows > 1) text_out("blows per round. Each blow will do an average damage of ");
	else if ((p_ptr->fencing) && (o_ptr->tval == TV_SWORD))
	{
		text_out("blow per round, and ");
		text_out_c(TERM_L_GREEN, format("%d ", p_ptr->fencing));
		if (p_ptr->fencing == 1) text_out("extra blow ");
		else text_out("extra blows ");
		text_out("against visible persons and humanoids because of your superior fencing skill. Each blow will do an average damage of ");
	}
	else text_out("blow per round, averaging a damage of ");

	/* First, the slays */
	weapon_slays_known(o_ptr, slays);

	/* Slays */
	for (i = 0; i < SL_MAX; i++)
	{
		if (slays[i] > 10)
		{
			/* Adjust damage for slay - note the complicated rounding */
			int dam_slay = dam + (((dam * (slays[i] - 10) + 90) / 100) * 10);
			any_slay = TRUE;

			if (dam_slay % 10)
				text_out_c(TERM_L_GREEN, format("%d.%d", dam_slay / 10, dam_slay % 10));
			else text_out_c(TERM_L_GREEN, format("%d", dam_slay / 10));
			text_out(format(" %s, ",  slay_names[i]));
		}
	}

	/* Regular damage */
	if (dam % 10) text_out_c(TERM_L_GREEN, format("%d.%d", dam / 10, dam % 10));
	else text_out_c(TERM_L_GREEN, format("%d", dam / 10));
		
	if (any_slay) text_out(" vs. unaffected monsters.\n"); 
	else text_out(" vs. any monster.\n"); 

	text_out("\nIf you have more than one enemy in melee range, you will attack one extra monster at random.\n");

	/* Ambush chance */
	int ambush_remainder = 0;
	int ambush = (p_ptr->to_h_melee + object_to_h(o_ptr)) +5;
	if (cp_ptr->flags & CF_AMBUSH)
	{
		ambush += (p_ptr->lev +10)/2;
		ambush_remainder = 5 * ((p_ptr->lev +10) % 2);
	}

	ambush += p_ptr->ambush_bonus;

	text_out("\nWith this weapon, you have a ");
	text_out_c(TERM_L_GREEN, format("%d", ambush));
	if (ambush_remainder) text_out_c(TERM_L_GREEN, format (".%d", ambush_remainder));
	text_out_c(TERM_L_GREEN, "%");
	text_out(" bonus to your critical hit chance against sleeping, scared, confused, or blind monsters.\n");
}

/*
 * Display information about body armor
 */
void analyze_armor(const object_type *o_ptr)
{
	int i;
	int str_needed = 0;

	/* How much STR you need */
	for (i = 0; i < 31; i++)
	{
		if (adj_str_armor[i] >= object_weight(o_ptr) / 10)
		{
			str_needed = i;
			break;
		}
	}

	if (str_needed > p_stat(A_STR))
	{
		text_out_c(TERM_L_RED, "\nYou would need ");
		text_out_c(TERM_L_RED, format("%d ", str_needed));
		text_out_c(TERM_L_RED, "STR to get DEX bonuses to Jumping in this armor!\n\n");
	}
	else
	{
		text_out(format("\nYou get full DEX bonuses to Jumping in this armour. (STR %d+ required.)\n\n", str_needed));
	}
}

/*
 * Display information about arrows
 */
void analyze_ammo(const object_type *o_ptr)
{
	int i;
	int dam = 0;

	object_type *j_ptr = &inventory[INVEN_BOW];

	byte ds = object_ds(o_ptr);
	byte dd = object_dd(o_ptr);

	byte slays[SL_MAX];
	byte slays_bow[SL_MAX];
	bool any_slay = FALSE;

	/* Wielding a bow */
	if ((j_ptr->k_idx) && object_known_p(j_ptr))
	{
		text_out("\nFired from you current bow, this arrow will hit targets up to ");
		text_out_c(TERM_L_GREEN, format("%d", (bow_range(j_ptr) + o_ptr->pval)));
		text_out(" squares away, inflicting an average damage of ");

		dam = ((ds + 1) * 5 * dd) * bow_might(j_ptr);

		/* First, the slays */
		weapon_slays_known(o_ptr, slays);
		weapon_slays_known(j_ptr, slays_bow);

		/* Slays */
		for (i = 0; i < SL_MAX; i++)
		{
			/* Add bow slays (ensuring that they're added correctly */
			if (slays_bow[i] > 10)
			{
				if (slays[i] > 10) slays[i] -= 10;
				slays[i] += slays_bow[i];
			}

			if (slays[i] > 10)
			{
				/* Adjust damage for slay - note the complicated rounding */
				int dam_slay = dam + (((dam * (slays[i] - 10) + 90) / 100) * 10);
				any_slay = TRUE;

				if (dam_slay % 10)
					text_out_c(TERM_L_GREEN, format("%d.%d", dam_slay / 10, dam_slay % 10));
				else text_out_c(TERM_L_GREEN, format("%d", dam_slay / 10));
				text_out(format(" %s, ",  slay_names[i]));
			}
		}

		/* Regular damage */
		if (dam % 10) text_out_c(TERM_L_GREEN, format("%d.%d", dam / 10, dam % 10));
		else text_out_c(TERM_L_GREEN, format("%d", dam / 10));
			
		if (any_slay) text_out(" vs. unaffected monsters. "); 
		else text_out(" vs. any monster. ");
	}

	text_out("It has a ");
	text_out_c(TERM_L_GREEN, format("%d%%", k_info[o_ptr->k_idx].breakage));
	text_out(" chance of breaking on contact.\n");

	/* Ambush chance */
	int ambush_remainder = 0;
	int ambush = (p_ptr->to_h_shooting + object_to_h(o_ptr) + object_to_h(j_ptr)) +5;

	if (cp_ptr->flags & CF_AMBUSH)
	{
		ambush += (p_ptr->lev +10)/2;
		ambush_remainder = 5 * ((p_ptr->lev +10) % 2);
	}

	ambush += p_ptr->ambush_bonus;

	text_out("\nWith this arrow, fired from your current bow, you have a ");
	text_out_c(TERM_L_GREEN, format("%d", ambush));
	if (ambush_remainder) text_out_c(TERM_L_GREEN, format (".%d", ambush_remainder));
	text_out_c(TERM_L_GREEN, "%");
	text_out(" bonus to your critical hit chance against sleeping, scared, confused, or blind monsters.\n");
}

/* 
 * Create a spoiler file entry for an artifact 
 */
void list_object(const object_type *o_ptr, int mode)
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
	if (spoil) object_flags(o_ptr, &f1, &f2, &f3);
	else if (random) object_flags_random(o_ptr, &f1, &f2, &f3);
	else object_flags_known(o_ptr, &f1, &f2, &f3);

	/* Quest items */
	if (!random && (o_ptr->tval == TV_QUEST))
	{
		text_out("An item of legend, it serves no practical use, but is of much value to collectors.  ");
	}

	/* Light sources */
	if (!random && ((o_ptr->tval == TV_LITE_SPECIAL) || (o_ptr->tval == TV_LITE)))
	{
		byte rad = 0;

		if (f3 & TR3_LITE1) rad = 1;
		if (f3 & TR3_LITE2) rad = 2;
		if (f3 & TR3_LITE3) rad = 3;
		if (f3 & TR3_LITE4) rad = 4;

		/* if ((rad) && (o_ptr->ident & IDENT_KNOWN)) */

		if (object_known_p(o_ptr) || object_aware_p(o_ptr))
		{
			if (o_ptr->tval == TV_LITE_SPECIAL) text_out("Permanent light-source (");
			else text_out("Light-source (");

			text_out_c(TERM_YELLOW, format("radius %d", rad + p_ptr->phlogiston));
			text_out(").  Its light protects you from seeing illusions in the Halls of Mist.  ");
			anything = TRUE;
		}
		else if (rad)
		{
			if (o_ptr->tval == TV_LITE_SPECIAL) text_out("Permanent light-source.  Its light protects you from seeing illusions in the Halls of Mist.  ");
			else text_out("Light-source.  Its light protects you from seeing illusions in the Halls of Mist.  ");

			anything = TRUE;
		}
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
				text_out_c(TERM_L_GREEN, format("+%d ", o_ptr->pval));
				text_out("to ");
			}
			else if (o_ptr->pval < 0) 
			{
				text_out_c(TERM_RED, format("%d ", o_ptr->pval));
				text_out("to ");
			}
			else
			{
				text_out("It modifies ");
			}
		
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
			(o_ptr->xtra1 == OBJECT_XTRA_TYPE_HIGH_RESIST)) &&
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
		anything |= outlist("A", list);

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

		anything |= outlist("It provides protection from the effects of", list);
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

	/*
	 * Handle cursed objects here to avoid redundancies such as noting
	 * that a permanently cursed object is heavily cursed as well as
	 * being "lightly cursed".
	 *
	 * Hack - curse spoiler items.
	 */
	if (!random && (cursed_p(o_ptr) || (spoil && (f3 & TR3_LIGHT_CURSE))))
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
	if (!random && query_activation(buf, sizeof(buf), &i, &j, o_ptr))
	{
		do_act = TRUE;

		/* If spoil mode, always print activation */
		if (spoil)
		{
			/* Do  nothing */
		}
		/* Unknown activation, print nothing */
		else if (!(object_known_p(o_ptr) || object_aware_p(o_ptr)) ||
			(o_ptr->a_idx && !(a_info[o_ptr->a_idx].status & A_STATUS_ACTIVATE)))
		{
			do_act = FALSE;
		}
		/* Check if the activation is fully known (actifact only) */
		else if (o_ptr->a_idx)
		{
			artifact_type *a_ptr = &a_info[o_ptr->a_idx];

			/* Times unknown */
			if (!artifact_known_p(a_ptr))
			{
				i = 0;
				j = 0;
			}
		}
		/* Recharge times for rods/talismans */
		else if ((o_ptr->tval == TV_ROD) || (o_ptr->tval == TV_TALISMAN))
		{
			i = o_ptr->pval;
		}

		if (do_act)
		{
			/* Activation */
			if (o_ptr->a_idx) 
			{
				switch (o_ptr->tval)
				{
					case TV_BOW:
					case TV_DIGGING:
					case TV_SWORD:
					case TV_BLUNT:
					case TV_POLEARM:
					{
						text_out("When wielded and activated, it ");
						break;
					}
					case TV_MAGIC_BOOK:
					{
						text_out("\n\nThis tome contains the Ritual ");
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
					case TV_POWDER:			text_out("When you throw it at a monster and it hits, it "); break;
					case TV_FLASK:			text_out("When you refuel a lantern with it, it "); break;
					case TV_MAGIC_BOOK:		text_out("\n\nThis tome contains the Ritual "); break;
					case TV_STAFF: default:		text_out("When used, it "); break;
				}
			}

			text_out(format("%s", buf));

			/* Show fail rate */
			if ((o_ptr->tval == TV_TALISMAN) || (o_ptr->tval == TV_ROD) || (o_ptr->tval == TV_WAND)
				|| (o_ptr->tval == TV_STAFF) || (o_ptr->tval ==  TV_DRAG_ARMOR) || ((o_ptr->a_idx) && (!(o_ptr->tval == TV_MAGIC_BOOK))))
			{
				int lev = k_info[o_ptr->k_idx].level;

				int chance;
				int fail_rate;

				/* Base chance of success */
				chance = p_ptr->skill[SK_DEV];
			
				/* Count the first 10 item levels twice */
				chance = chance - ((lev > 10) ? 10 : lev);

				/* Count the first 4 item levels three times */
				chance = chance - ((lev > 4) ? 4 : lev);

				/* Confusion hurts skill */
				if (p_ptr->confused) chance = chance / 2;
			
				/* Stunning hurts skill */
				if (p_ptr->stun > PY_STUN_HEAVY) chance = chance / 2;
				else if (p_ptr->stun) chance = (chance * 2) / 3;
			
				/* High level objects are harder */
				if (lev > 80) chance -= 57;
				else chance -= ((lev > 51) ? 30 + (lev/3) : lev - ((lev-10)/10));

				/* Calculate fail rate */
				if (chance < 2) fail_rate = 85;
				else if (chance == 2) fail_rate = 70;
				else if (chance == 3) fail_rate = 60;
				else fail_rate = (200/chance);

				if (fail_rate > 85) fail_rate = 85;

				/* Print fail rate */
				text_out(", with a ");
				text_out_c(TERM_YELLOW, format ("%d%%", fail_rate));
				text_out(" fail rate");
			}

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
				text_out(", recharging every turn.  ");
			}
			else text_out(".  ");

			anything = TRUE;

			/*				
			 * If item power has damage, radius, or beam chance, show them
			 */
			int plev = p_ptr->lev;
			int beam, dlev, llev;

			switch (o_ptr->tval)
			{
				case TV_STAFF:		beam = 0; dlev = 20; llev = 20; break;
				case TV_WAND:		beam = 20; dlev = plev; llev = 20; break;
				case TV_ROD:		beam = 0; dlev = 10; llev = 10; break;
				case TV_TALISMAN:	beam = 10; dlev = plev; llev = plev; break;
				case TV_FOOD:		beam = 0; dlev = plev; llev = 20; break;
				case TV_SCROLL:		beam = 0; dlev = 20; llev = 30; break;
				default:		beam = 0; dlev = plev; plev = plev; break;
			}

			switch (object_activation(o_ptr))
			{
				case POW_POWDER_HEAL:
					text_out("On a critical hit, the powder cloud will spread.  The powder may also be poured (dropped) on a broken magic circle to complete a Circle of Lifeforce.  ");
					text_out("\n\nCircle of Lifeforce restores and sustains STR, DEX, CON, and experience points.  It also cures and protects from the effects of disease.  Undead and demons have difficulties moving (80% fail) or attacking in melee (50% fail) on a Circle of Lifeforce.  ");
					break;
				case POW_POWDER_CONFUSING:
					text_out("On a critical hit, the powder cloud will spread.  The powder may also be poured (dropped) on a broken magic circle to complete either a Circle of Illusions or Nexus, chosen at random.  ");
					text_out("\n\nCircle of Illusions increases spell range by 7 squares and makes your influence spells (not devices) harder to resist.  Animals, persons, and humanoids have difficulties moving (80% fail) or attacking in melee (50% fail) on a Circle of Illusions.  ");
					text_out("\n\nCircle of Nexus restores and sustains MEM, WIS, and PRE.  All teleportation effects other than Phase Door will take you to the Circle of Nexus.  Any teleporting monster (for whatever reason) will also end up there.  ");
					break;
				case POW_POWDER_SLEEPING:
					text_out("On a critical hit, the powder cloud will spread.  The powder may also be poured (dropped) on a broken magic circle to complete a Circle of Illusions.  ");
					text_out("\n\nCircle of Illusions increases spell range by 7 squares and makes your influence spells (not devices) harder to resist.  Animals, persons, and humanoids have difficulties moving (80% fail) or attacking in melee (50% fail) on a Circle of Illusions.  ");
					break;
				case POW_POWDER_CALMING:
					text_out("On a critical hit, the powder cloud will spread.  The powder may also be poured (dropped) on a broken magic circle to complete either a Circle of Lifeforce or Illusions, chosen at random.  ");
					text_out("\n\nCircle of Lifeforce restores and sustains STR, DEX, CON, and experience points.  It also cures and protects from the effects of disease.  Undead and demons have difficulties moving (80% fail) or attacking in melee (50% fail) on a Circle of Lifeforce.  ");
					text_out("\n\nCircle of Illusions increases spell range by 7 squares and makes your influence spells (not devices) harder to resist.  Animals, persons, and humanoids have difficulties moving (80% fail) or attacking in melee (50% fail) on a Circle of Illusions.  ");
					break;
				case POW_POWDER_TRANSFORMING:
					text_out("On a critical hit, the powder cloud will spread.  The powder may also be poured (dropped) on a broken magic circle to complete either a Circle of Recall or Summoning, chosen at random.  ");
					text_out("\n\nCircle of Recall will call you back when your hitpoints are at 20% of maximum or lower.  ");
					text_out("\n\nCircle of Summoning summons a group of monsters when completed.  ");
					break;
				case POW_POWDER_FLASH:
					text_out("Damage ");
					text_out_c(TERM_L_GREEN, "3d16");
					text_out(".  On a critical hit, the powder cloud will spread.  The powder may also be poured (dropped) on a broken magic circle to complete either a Circle of Lifeforce or Knowledge, chosen at random.  ");
					text_out("\n\nCircle of Lifeforce restores and sustains STR, DEX, CON, and experience points.  It also cures and protects from the effects of disease.  Undead and demons have difficulties moving (80% fail) or attacking in melee (50% fail) on a Circle of Lifeforce.  ");
					text_out("\n\nCircle of Knowledge gives +7 to Lore calculations.  When your MEM+WIS+bonus reaches 20, 30, 40, 50, or 60 you gain either more Lore points or better proficiencies.  ");
					break;
				case POW_POWDER_DARKNESS:
					text_out("Damage ");
					text_out_c(TERM_L_GREEN, "3d16");
					text_out(".  On a critical hit, the powder cloud will spread.  The powder may also be poured (dropped) on a broken magic circle to complete either a Circle of Summoning or Nexus, chosen at random.  ");
					text_out("\n\nCircle of Summoning summons a group of monsters when completed.  ");
					text_out("\n\nCircle of Nexus restores and sustains MEM, WIS, and PRE.  All teleportation effects other than Phase Door will take you to the Circle of Nexus.  Any teleporting monster (for whatever reason) will also end up there.  ");
					break;
				case POW_POWDER_POISONING:
					text_out("Damage ");
					text_out_c(TERM_L_GREEN, "3d14");
					text_out(".  On a critical hit, the powder cloud will spread.  The powder may also be poured (dropped) on a broken magic circle to complete a Circle of Summoning.  ");
					text_out("\n\nCircle of Summoning summons a group of monsters when completed.  ");
					break;
				case POW_POWDER_BURNING:
					text_out("Damage ");
					text_out_c(TERM_L_GREEN, "3d16");
					text_out(".  On a critical hit, the powder cloud will spread.  The powder may also be poured (dropped) on a broken magic circle to complete a Circle of Knowledge.  ");
					text_out("\n\nCircle of Knowledge gives +7 to Lore calculations.  When your MEM+WIS+bonus reaches 20, 30, 40, 50, or 60 you gain either more Lore points or better proficiencies.  ");
					break;
				case POW_POWDER_FREEZING:
					text_out("Damage ");
					text_out_c(TERM_L_GREEN, "3d16");
					text_out(".  On a critical hit, the powder cloud will spread.  The powder may also be poured (dropped) on a broken magic circle to complete a Circle of Nexus.  ");
					text_out("\n\nCircle of Nexus restores and sustains MEM, WIS, and PRE.  All teleportation effects other than Phase Door will take you to the Circle of Nexus.  Any teleporting monster (for whatever reason) will also end up there.  ");
					break;
				case POW_POWDER_INCINERATION:
					text_out("Damage ");
					text_out_c(TERM_L_GREEN, "8d30");
					text_out(", radius ");
					text_out_c(TERM_L_GREEN, format ("2"));
					text_out(".  On a critical hit, the powder cloud will spread further.  The powder may also be poured (dropped) on a broken magic circle to complete either a Circle of Knowledge or Permanence, chosen at random.  ");
					text_out("\n\nCircle of Knowledge gives +7 to Lore calculations.  When your MEM+WIS+bonus reaches 20, 30, 40, or 50, or 60 you gain either more Lore points or better proficiencies.  ");
					text_out("\n\nCircle of Permanence will make any positive temporary status effects started inside the circle last until you leave the dungeon level.  ");
					break;
				case POW_POWDER_ICE_BLAST:
					text_out("Damage ");
					text_out_c(TERM_L_GREEN, "8d30");
					text_out(", radius ");
					text_out_c(TERM_L_GREEN, format ("2"));
					text_out(".  On a critical hit, the powder cloud will spread further.  The powder may also be poured (dropped) on a broken magic circle to complete a Circle of Permanence.  ");
					text_out("\n\nCircle of Permanence will make any positive temporary status effects started inside the circle last until you leave the dungeon level.  ");
					break;
				case POW_POWDER_ENERGY:
					text_out("Damage ");
					text_out_c(TERM_L_GREEN, "10d60");
					text_out(", radius ");
					text_out_c(TERM_L_GREEN, format ("2"));
					text_out(".  On a critical hit, the powder cloud will spread further.  The powder may also be poured (dropped) on a broken magic circle to complete a Circle of Recall.  ");
					text_out("\n\nCircle of Recall will call you back when your hitpoints are at 20% of maximum or lower.  ");
					break;
				case POW_OIL_LANTERN:
					text_out("When you throw it at a monster and it hits, it burns the monster.  ");
					text_out("Damage ");
					text_out_c(TERM_L_GREEN, "2d4");
					text_out(".  On a critical hit, the oil slick will spread.  "); break;
				case POW_OIL_BURNING:
					text_out("When you throw it at a monster and it hits, it burns the monster.  ");
					text_out("Damage ");
					text_out_c(TERM_L_GREEN, "3d10");
					text_out(".  On a critical hit, the oil slick will spread.  "); break;
				case POW_DETONATE:
					text_out("It is a powerful thrown weapon.  ");
					text_out("Damage ");
					text_out_c(TERM_L_GREEN, "25d25");
					text_out(".  "); break;
				case POW_BOLT_MISSILE:
					text_out("Damage ");
					text_out_c(TERM_L_GREEN, format ("2d6"));
					text_out(".  "); break;
				case POW_BOLT_ELEC:
					text_out("Damage ");
					text_out_c(TERM_L_GREEN, format ("%dd6", (3 + ((dlev - 5) / 4))));
					beam = beam -10;
					if (beam > 0)
					{	text_out(", beam chance ");
						text_out_c(TERM_L_GREEN, format ("%d%%", beam));
					}
					text_out(".  ");
					break;
				case POW_BOLT_COLD_1:
					text_out("Damage ");
					text_out_c(TERM_L_GREEN, format ("%dd6", 5 + ((dlev - 5) / 4)));
					beam = beam -10;
					if (beam > 0)
					{	text_out(", beam chance ");
						text_out_c(TERM_L_GREEN, format ("%d%%", beam));
					}
					text_out(".  ");
					break;
				case POW_BOLT_COLD_2:
					text_out("Damage ");
					text_out_c(TERM_L_GREEN, format ("%dd6", 10 + ((dlev - 5) / 4)));
					beam = beam -10;
					if (beam > 0)
					{	text_out(", beam chance ");
						text_out_c(TERM_L_GREEN, format ("%d%%", beam));
					}
					text_out(".  "); break;
				case POW_BOLT_ACID_1:
					text_out("Damage ");
					text_out_c(TERM_L_GREEN, format ("%dd6", 3 + (dlev / 4)));
					if (beam > 0)
					{	text_out(", beam chance ");
						text_out_c(TERM_L_GREEN, format ("%d%%", beam));
					}
					text_out(".  "); break;
				case POW_BOLT_ACID_2:
					text_out("Damage ");
					text_out_c(TERM_L_GREEN, format ("%dd7", 3 + ((dlev - 5) / 4)));
					if (beam > 0)
					{	text_out(", beam chance ");
						text_out_c(TERM_L_GREEN, format ("%d%%", beam));
					}
					text_out(".  "); break;
				case POW_BOLT_FIRE_1:
					text_out("Damage ");
					text_out_c(TERM_L_GREEN, format ("10d6"));
					if (beam > 0)
					{	text_out(", beam chance ");
						text_out_c(TERM_L_GREEN, format ("%d%%", beam));
					}
					text_out(".  "); break;
				case POW_BOLT_FIRE_2:
					text_out("Damage ");
					text_out_c(TERM_L_GREEN, format ("%dd6", 5 + (dlev / 5)));
					if (beam > 0)
					{	text_out(", beam chance ");
						text_out_c(TERM_L_GREEN, format ("%d%%", beam));
					}
					text_out(".  "); break;
				case POW_BOLT_FIRE_3:
					text_out("Damage ");
					text_out_c(TERM_L_GREEN, format ("%dd7", 7 + ((dlev - 5) / 3)));
					if (beam > 0)
					{	text_out(", beam chance ");
						text_out_c(TERM_L_GREEN, format ("%d%%", beam));
					}
					text_out(".  "); break;
				case POW_BOLT_FORCE_2:
					text_out("Damage ");
					text_out_c(TERM_L_GREEN, format ("%dd8", 3 + (dlev / 5)));
					if (beam > 0)
					{	text_out(", beam chance ");
						text_out_c(TERM_L_GREEN, format ("%d%%", beam));
					}
					text_out(".  "); break;
				case POW_HARPOON:
					text_out("Damage ");
					text_out_c(TERM_L_GREEN, format ("%dd6", 1 + (dlev/4)));
					text_out(".  "); break;
				case POW_BALL_STUN:
					text_out("Damage ");
					text_out_c(TERM_L_GREEN, ("1d6"));
					text_out(", radius ");
					text_out_c(TERM_L_GREEN, format ("2"));
					text_out(".  "); break;
				case POW_BOLT_LITE:
				case POW_BOLT_DARK:
					text_out("Damage ");
					text_out_c(TERM_L_GREEN, format ("4d7"));
					text_out(".  "); break;
				case POW_BOLT_WATER:
					text_out("Damage ");
					text_out_c(TERM_L_GREEN, format ("5d6"));
					text_out(".  "); break;
				case POW_BOLT_MANA:
					text_out("Damage ");
					text_out_c(TERM_L_GREEN, format ("%dd6", 6 + ((dlev - 5) / 4)));
					if (beam > 0)
					{	text_out(", beam chance ");
						text_out_c(TERM_L_GREEN, format ("%d%%", beam));
					}
					text_out(".  "); break;
				case POW_BEAM_WEAK_LITE:
					text_out("Damage ");
					text_out_c(TERM_L_GREEN, format ("9d8"));
					text_out(".  "); break;
				case POW_BALL_POISON:
					text_out("Damage ");
					text_out_c(TERM_L_GREEN, format ("15"));
					text_out(", radius ");
					text_out_c(TERM_L_GREEN, format ("2"));
					text_out(".  "); break;
				case POW_BALL_ACID:
				case POW_BALL_ELEC_1:
					text_out("Damage ");
					text_out_c(TERM_L_GREEN, format ("%d", 35 + (dlev *2)));
					text_out(", radius ");
					text_out_c(TERM_L_GREEN, format ("2"));
					text_out(".  "); break;
				case POW_BALL_ELEC_2:
					text_out("Damage ");
					text_out_c(TERM_L_GREEN, format ("210"));
					text_out(", radius ");
					text_out_c(TERM_L_GREEN, format ("3"));
					text_out(".  "); break;
				case POW_BALL_FIRE_1:
					text_out("Damage ");
					text_out_c(TERM_L_GREEN, format ("80"));
					text_out(", radius ");
					text_out_c(TERM_L_GREEN, format ("2"));
					text_out(".  "); break;
				case POW_BALL_FIRE_2:
					text_out("Damage ");
					text_out_c(TERM_L_GREEN, format ("%d", 50 + (dlev)));
					text_out(", radius ");
					text_out_c(TERM_L_GREEN, format ("%d", (dlev < 40) ? 2 : 3));
					text_out(".  "); break;
				case POW_BALL_FIRE_3:
					text_out("Damage ");
					text_out_c(TERM_L_GREEN, format ("%d", 90 + (dlev * 2)));
					text_out(", radius ");
					text_out_c(TERM_L_GREEN, format ("%d", (dlev < 40) ? 3 : 4));
					text_out(".  "); break;
				case POW_BALL_COLD_1:
					text_out("Damage ");
					text_out_c(TERM_L_GREEN, format ("%d", 30 + (dlev)));
					text_out(", radius ");
					text_out_c(TERM_L_GREEN, format ("%d", (dlev < 35) ? 2 : 3));
					text_out(".  "); break;
				case POW_BALL_COLD_2:
					text_out("Damage ");
					text_out_c(TERM_L_GREEN, format ("%d", 60 + (dlev * 2)));
					text_out(", radius ");
					text_out_c(TERM_L_GREEN, format ("%d", (dlev < 35) ? 3 : 4));
					text_out(".  "); break;
				case POW_BALL_COLD_3:
					text_out("Damage ");
					text_out_c(TERM_L_GREEN, format ("180"));
					text_out(", radius ");
					text_out_c(TERM_L_GREEN, format ("4"));
					text_out(".  "); break;
				case POW_STAR_BEAM_W_LITE:
					text_out("Damage ");
					text_out_c(TERM_L_GREEN, format ("9d8"));
					text_out(".  "); break;
				case POW_STAR_BALL_ELEC:
					text_out("Damage ");
					text_out_c(TERM_L_GREEN, format ("140"));
					text_out(", radius ");
					text_out_c(TERM_L_GREEN, format ("3"));
					text_out(".  "); break;
				case POW_DRAIN_LIFE_1:
					text_out("Damage ");
					text_out_c(TERM_L_GREEN, format ("%d", (50 + dlev)));
					text_out(".  "); break;
				case POW_DRAIN_LIFE_2:
					text_out("Damage ");
					text_out_c(TERM_L_GREEN, format ("110"));
					text_out(".  "); break;
				case POW_DRAIN_LIFE_3:
					text_out("Damage ");
					text_out_c(TERM_L_GREEN, format ("180"));
					text_out(".  "); break;
				case POW_DISPEL_ALL:
					text_out("Damage ");
					text_out_c(TERM_L_GREEN, format ("%d", (dlev * 6)));
					text_out(".  "); break;
				case POW_DISPEL_UNDEAD_1:
					text_out("Damage ");
					text_out_c(TERM_L_GREEN, format ("1d%d", (dlev * 3)));
					text_out(".  "); break;
				case POW_DISPEL_EVIL_3:
					text_out("Damage ");
					text_out_c(TERM_L_GREEN, format ("1d%d", (dlev * 3)));
					text_out(".  "); break;
				case POW_HOLY_1:
					text_out("Damage ");
					text_out_c(TERM_L_GREEN, format ("1d%d", (dlev * 6)));
					text_out(".  "); break;
				case POW_LIGHT_AREA_2: 
				case POW_DARK_AREA:
					text_out("Damage ");
					text_out_c(TERM_L_GREEN, format ("2d%d", (dlev/2)));
					text_out(", radius ");
					text_out_c(TERM_L_GREEN, format ("%d", (dlev/10) + 1));
					text_out(".  "); break;
				case POW_STONE_TO_MUD:
					text_out("Damage is ");
					text_out_c(TERM_L_GREEN, format ("100"));
					text_out(" against rock monsters, or against a ghost inside the exploding wall square.  ");
					text_out("Burst radius is ");
					text_out_c(TERM_L_GREEN, format ("3"));
					text_out(" and damage ");
					text_out_c(TERM_L_GREEN, format ("50"));
					text_out(", ");
					text_out_c(TERM_L_GREEN, format ("33"));
					text_out(", or ");
					text_out_c(TERM_L_GREEN, format ("25"));
					text_out(", depending on the distance from the center of explosion. The explosion damages both you and monsters.");
					break;
				case POW_SHRPOISON:
					if (cp_ptr->flags & CF_SHROOM_MAGIC)
					{
						text_out("When eaten by a Shaman, it makes you breath poison.  ");
						text_out("Damage ");
						text_out_c(TERM_L_GREEN, format ("150+1d%d", (dlev*2)));
						text_out(".  ");
					}
					break;
				case POW_SHRBLIND:
					if (cp_ptr->flags & CF_SHROOM_MAGIC)
					{
						text_out("When eaten by a Shaman, it attempts to blind all monsters in line of sight.  ");
					}
					break;
				case POW_SHRSCARE:
					if (cp_ptr->flags & CF_SHROOM_MAGIC)
					{
						text_out("When eaten by a Shaman, it attempts to scare all monsters in line of sight.  ");
					}
					break;
				case POW_SHRCONFUSE:
					if (cp_ptr->flags & CF_SHROOM_MAGIC)
					{
						text_out("When eaten by a Shaman, it attempts to confuse all monsters in line of sight.  ");
					}
					break;
				case POW_SHRHALLUCINATE:
					if (cp_ptr->flags & CF_SHROOM_MAGIC)
					{
						text_out("When eaten by a Shaman, it provides temporary see invisible.  ");
					}
					break;
				case POW_SHRPARALYZE:
					if (cp_ptr->flags & CF_SHROOM_MAGIC)
					{
						text_out("When eaten by a Shaman, it attempts to paralyze all monsters in line of sight.  ");
					}
					break;
				case POW_SHRNAIVITY:
					if (cp_ptr->flags & CF_SHROOM_MAGIC)
					{
						text_out("When eaten by a Shaman, it restores mana to full level.  ");
					}
					break;
				case POW_SHRSTUPIDITY:
					if (cp_ptr->flags & CF_SHROOM_MAGIC)
					{
						text_out("When eaten by a Shaman, it makes you go berserk.  ");
					}
					break;
				case POW_SHRAMNESIA:
					if (cp_ptr->flags & CF_SHROOM_MAGIC)
					{
						text_out("When eaten by a Shaman, it makes you invisible.  ");
					}
					break;
				case POW_SHRDISEASE:
					if (cp_ptr->flags & CF_SHROOM_MAGIC)
					{
						text_out("When eaten by a Shaman, it makes you breath nether.  ");
						text_out("Damage ");
						text_out_c(TERM_L_GREEN, format ("250+1d%d", (dlev*2)));
						text_out(".  ");
					}
					break;
				case POW_SHRHEAL_1:
					if (cp_ptr->flags & CF_SHROOM_MAGIC)
					{
						text_out("When eaten by a Shaman, it reduces cuts and heals you a moderate amount.  ");
					}
					break;
				case POW_SHRHEAL_2:
					if (cp_ptr->flags & CF_SHROOM_MAGIC)
					{
						text_out("When eaten by a Shaman, it reduces cuts and heals you a large amount.  ");
					}
					break;
				case POW_DRAGON_BLACK:
				case POW_DRAGON_BLUE:
				case POW_DRAGON_WHITE:
				case POW_DRAGON_RED:
						text_out("Damage ");
						text_out_c(TERM_L_GREEN, format ("125+1d%d", (dlev*2)));
						text_out(", radius ");
						text_out_c(TERM_L_GREEN, format ("2"));
						text_out(".  "); break;
				case POW_DRAGON_GREEN:
						text_out("Damage ");
						text_out_c(TERM_L_GREEN, format ("150+1d%d", (dlev*2)));
						text_out(", radius ");
						text_out_c(TERM_L_GREEN, format ("2"));
						text_out(".  "); break;
				case POW_DRAGON_GOLD:
				case POW_DRAGON_SILVER:
						text_out("Damage ");
						text_out_c(TERM_L_GREEN, format ("100+1d%d", (dlev*2)));
						text_out(", radius ");
						text_out_c(TERM_L_GREEN, format ("2"));
						text_out(".  "); break;
				case POW_DRAGON_MH:
				case POW_DRAGON_SPIRIT:
				case POW_DRAGON_SHADOW:
				case POW_DRAGON_ETHER:
						text_out("Damage ");
						text_out_c(TERM_L_GREEN, format ("250+1d%d", (dlev*2)));
						text_out(", radius ");
						text_out_c(TERM_L_GREEN, format ("2"));
						text_out(".  "); break;
				case POW_DRAGON_CHAOS:
				case POW_DRAGON_TIME:
						text_out("Damage ");
						text_out_c(TERM_L_GREEN, format ("350+1d%d", (dlev*2)));
						text_out(", radius ");
						text_out_c(TERM_L_GREEN, format ("2"));
						text_out(".  "); break;
				case POW_DRAGON_POWER:
						text_out("Damage ");
						text_out_c(TERM_L_GREEN, format ("400+1d%d", (dlev*2)));
						text_out(", radius ");
						text_out_c(TERM_L_GREEN, format ("2"));
						text_out(".  "); break;
				case POW_RITUAL_STR:
					if (cp_ptr->flags & CF_RITUAL_EXPERT) text_out("You may use the ritual by dropping the book on a Circle of Permanence.  The book is consumed in the process.  The ritual's power is MEM+WIS-1d20. For each full 5 power points, gain a point of Strength.  ");
					else text_out("You may use the ritual by dropping the book on a Circle of Permanence.  The book is consumed in the process.  The ritual's power is MEM+WIS-1d20. For each full 10 power points, gain a point of Strength.  ");
					break;
				case POW_RITUAL_INT:
					if (cp_ptr->flags & CF_RITUAL_EXPERT) text_out("You may use the ritual by dropping the book on a Circle of Recall.  The book is consumed in the process.  The ritual's power is MEM+WIS-1d20. For each full 5 power points, gain a point of Memory.  ");
					else text_out("You may use the ritual by dropping the book on a Circle of Recall.  The book is consumed in the process.  The ritual's power is MEM+WIS-1d20. For each full 10 power points, gain a point of Memory.  ");
					break;
				case POW_RITUAL_WIS:
					if (cp_ptr->flags & CF_RITUAL_EXPERT) text_out("You may use the ritual by dropping the book on a Circle of Recall.  The book is consumed in the process.  The ritual's power is MEM+WIS-1d20. For each full 5 power points, gain a point of Wisdom.  ");
					else text_out("You may use the ritual by dropping the book on a Circle of Recall.  The book is consumed in the process.  The ritual's power is MEM+WIS-1d20. For each full 10 power points, gain a point of Wisdom.  ");
					break;
				case POW_RITUAL_DEX:
					if (cp_ptr->flags & CF_RITUAL_EXPERT) text_out("You may use the ritual by dropping the book on a Circle of Nexus.  The book is consumed in the process.  The ritual's power is MEM+WIS-1d20. For each full 5 power points, gain a point of Dexterity.  ");
					else text_out("You may use the ritual by dropping the book on a Circle of Nexus.  The book is consumed in the process.  The ritual's power is MEM+WIS-1d20. For each full 10 power points, gain a point of Dexterity.  ");
					break;
				case POW_RITUAL_CON:
					if (cp_ptr->flags & CF_RITUAL_EXPERT) text_out("You may use the ritual by dropping the book on a Circle of Lifeforce.  The book is consumed in the process.  The ritual's power is MEM+WIS-1d20. For each full 5 power points, gain a point of Constitution.  ");
					else text_out("You may use the ritual by dropping the book on a Circle of Lifeforce.  The book is consumed in the process.  The ritual's power is MEM+WIS-1d20. For each full 10 power points, gain a point of Constitution.  ");
					break;
				case POW_RITUAL_CHR:
					if (cp_ptr->flags & CF_RITUAL_EXPERT) text_out("You may use the ritual by dropping the book on a Circle of Illusions.  The book is consumed in the process.  The ritual's power is MEM+WIS-1d20. For each full 5 power points, gain a point of Presence.  ");
					else text_out("You may use the ritual by dropping the book on a Circle of Illusions.  The book is consumed in the process.  The ritual's power is MEM+WIS-1d20. For each full 10 power points, gain a point of Presence.  ");
					break;
				case POW_RITUAL_AUGMENT_BODY:
					if (cp_ptr->flags & CF_RITUAL_EXPERT) text_out("You may use the ritual by dropping the book on a Circle of Permanence.  The book is consumed in the process.  The ritual's power is MEM+WIS-1d20. For each full 5 power points, increase your physical stats by one.  ");
					else text_out("You may use the ritual by dropping the book on a Circle of Permanence.  The book is consumed in the process.  The ritual's power is MEM+WIS-1d20. For each full 10 power points, increase your physical stats by one.  ");
					break;
				case POW_RITUAL_MIND_OVER_BODY:
					if (cp_ptr->flags & CF_RITUAL_EXPERT) text_out("You may use the ritual by dropping the book on a Circle of Knowledge.  The book is consumed in the process.  The ritual's power is MEM+WIS-1d20. For each full 5 power points, increase your mental stats by 2, and decrease your physical stats by 2.  ");
					else text_out("You may use the ritual by dropping the book on a Circle of Knowledge.  The book is consumed in the process.  The ritual's power is MEM+WIS-1d20. For each full 10 power points, increase your mental stats by 2, and decrease your physical stats by 2.  ");
					break;
				case POW_RITUAL_FORTIFICATION:
					if (cp_ptr->flags & CF_RITUAL_EXPERT) text_out("You may use the ritual by dropping the book on a Circle of Lifeforce.  The book is consumed in the process.  The ritual's power is MEM+WIS-1d20. For each full 5 power points, gain a +3 bonus to all resistances, and increase resistance caps by the same amount.  ");
					else text_out("You may use the ritual by dropping the book on a Circle of Lifeforce.  The book is consumed in the process.  The ritual's power is MEM+WIS-1d20. For each full 10 power points, gain a +3 bonus to all resistances, and increase resistance caps by the same amount.  ");
					break;
				case POW_RITUAL_NIGHT_SIGHT:
					if (cp_ptr->flags & CF_RITUAL_EXPERT) text_out("You may use the ritual by dropping the book on a Circle of Illusions.  The book is consumed in the process.  The ritual's power is MEM+WIS-1d20. For each full 5 power points, gain a permanent one square increase to your infravision range.  ");
					else text_out("You may use the ritual by dropping the book on a Circle of Illusions.  The book is consumed in the process.  The ritual's power is MEM+WIS-1d20. For each full 10 power points, gain a permanent one square increase to your infravision range.  ");
					break;
				case POW_RITUAL_ACQUIRE_ARMOR:
					if (cp_ptr->flags & CF_RITUAL_EXPERT) text_out("You may use the ritual by dropping the book on a Circle of Summoning.  The book is consumed in the process.  The ritual's power is MEM+WIS-1d20. For each full 5 power points, summon a powerful armour.  ");
					else text_out("You may use the ritual by dropping the book on a Circle of Summoning.  The book is consumed in the process.  The ritual's power is MEM+WIS-1d20. For each full 10 power points, summon a powerful armour.  ");
					break;
				case POW_RITUAL_CREATE_POWDER:
					if (cp_ptr->flags & CF_RITUAL_EXPERT) text_out("You may use the ritual by dropping the book on a Circle of Nexus.  The book is consumed in the process.  The ritual's power is MEM+WIS-1d20. For each full 5 power points, create 10 piles of powder vials.  ");
					else text_out("You may use the ritual by dropping the book on a Circle of Nexus.  The book is consumed in the process.  The ritual's power is MEM+WIS-1d20. For each full 10 power points, create 10 piles of powder vials.  ");
					break;
				case POW_RITUAL_CURE_WOUND:
					if (cp_ptr->flags & CF_RITUAL_EXPERT) text_out("You may use the ritual by dropping the book on a Circle of Lifeforce.  The book is consumed in the process.  The ritual's power is MEM+WIS-1d20. For each full 5 power points, cure a Wound in this order: Vigor, Wit, Grace.  ");
					else text_out("You may use the ritual by dropping the book on a Circle of Lifeforce.  The book is consumed in the process.  The ritual's power is MEM+WIS-1d20. For each full 10 power points, cure a Wound in this order: Vigor, Wit, Grace.  ");
					break;
				case POW_RITUAL_ACQUIRE_WEAPON:
					if (cp_ptr->flags & CF_RITUAL_EXPERT) text_out("You may use the ritual by dropping the book on a Circle of Summoning.  The book is consumed in the process.  The ritual's power is MEM+WIS-1d20. For each full 5 power points, summon a powerful weapon.  ");
					else text_out("You may use the ritual by dropping the book on a Circle of Summoning.  The book is consumed in the process.  The ritual's power is MEM+WIS-1d20. For each full 10 power points, summon a powerful weapon.  ");
					break;
				case POW_RITUAL_FORBIDDEN_LORE:
					if (cp_ptr->flags & CF_RITUAL_EXPERT) text_out("You may use the ritual by dropping the book on a Circle of Knowledge.  The book is consumed in the process.  The ritual's power is MEM+WIS-1d20. For each full 5 power points, you gain at least one experience level.  ");
					else text_out("You may use the ritual by dropping the book on a Circle of Knowledge.  The book is consumed in the process.  The ritual's power is MEM+WIS-1d20. For each full 10 power points, you gain at least one experience level.  ");
					break;
			}
		}
	}

	/* Unknown extra powers (ego-item with random extras or artifact) */
	if (!spoil && !random && (object_known_p(o_ptr) && ((o_ptr->xtra1) || o_ptr->a_idx)))
	{
		bool hidden = TRUE;
	
		if (!o_ptr->a_idx && (o_ptr->ident & IDENT_MENTAL)) hidden = FALSE;

		if (o_ptr->a_idx)
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

		if ((i >= p_ptr->min_depth * 500) && (i > 0))
		{
			text_out("\n\nYou may sell it in the town. You think it values at around ");
			text_out_c(TERM_ORANGE, format("%d", i / 10));
			text_out(" gp.  ");

			anything = TRUE;
		}
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
			alchemy_describe(line, sizeof(line), o_ptr->sval);
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
				alchemy_describe(line, sizeof(line), i);
				text_out_c(TERM_L_WHITE, format("\n%s", line));
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
		text_out_c(TERM_YELLOW, "in the town.\n");
	}
	else if (depth_in_feet)
	{
		text_out_c(TERM_YELLOW, format("at a depth of %d ft.\n", depth * 50));
	}
	else 
	{
		text_out_c(TERM_YELLOW, format("on dungeon level %d.\n", depth));
	}
}

/*
 * Display item history 
 */
void display_object_history(const object_type *o_ptr)
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
			text_out_c(TERM_YELLOW, "It is one of your prizes for victory!\n");
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
		case ORIGIN_CHEST:
		{
			text_out_c(TERM_YELLOW, format("%s ", intro));
			text_out_c(TERM_YELLOW, "found in a chest ");
			history_depth(o_ptr->origin_dlvl);
 			break;
		}
		case ORIGIN_RACK:
		{
			text_out_c(TERM_YELLOW, format("%s ", intro));
			text_out_c(TERM_YELLOW, "found in a rack ");
			history_depth(o_ptr->origin_dlvl);
 			break;
		}
		case ORIGIN_SHELF:
		{
			text_out_c(TERM_YELLOW, format("%s ", intro));
			text_out_c(TERM_YELLOW, "found in a bookshelf ");
			history_depth(o_ptr->origin_dlvl);
 			break;
		}
		case ORIGIN_CLOSET:
		{
			text_out_c(TERM_YELLOW, format("%s ", intro));
			text_out_c(TERM_YELLOW, "found in a closet ");
			history_depth(o_ptr->origin_dlvl);
 			break;
		}
		case ORIGIN_FOREST:
		{
			text_out_c(TERM_YELLOW, format("%s ", intro));
			text_out_c(TERM_YELLOW, "found in a forest ");
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
bool history_interesting(const object_type *o_ptr)
{
	/* Empty slots are always boring */
	if (!o_ptr->k_idx) return FALSE;

	/* Items with no, or mixed, origins are always boring */
	if (!o_ptr->origin_nature || (o_ptr->origin_nature == ORIGIN_MIXED)) return FALSE;

	/* Artifacts are always interesting */
	if (o_ptr->a_idx) return TRUE;

	/* Ego items are interesting if they're good */
	if (o_ptr->e_idx && (object_value(o_ptr) > 0)) return TRUE;

	/* Objects dropped by uniques are always interesting */
	if (o_ptr->origin_u_idx) return TRUE;

	/* Cheat items are always interesting */
	if (o_ptr->origin_nature == ORIGIN_CHEAT) return TRUE;

	/* Some other origins usually are boring */
	if ((o_ptr->origin_nature == ORIGIN_BIRTH) || (o_ptr->origin_nature == ORIGIN_STORE))
		return FALSE;

	/* Objects OOD by more than ten levels are interesting */
	if ((o_ptr->origin_dlvl + INTEREST_OFFSET) < k_info[o_ptr->k_idx].level) return TRUE;

	return FALSE;
}

/*
 * Hack -- display an object kind in the current window
 *
 * Include list of usable spells for readable books
 */
void display_koff(const object_type *o_ptr)
{
	int y, x;
	
	/* Erase the window */
	for (y = 0; y < Term->hgt; y++)
	{
		/* Erase the line */
		Term_erase(0, y, 255);
	}

	/* No info */
	if (!o_ptr->k_idx) return;

	/* Begin recall */
	Term_gotoxy(0, 1);

	/* Center spellbooks in window */
	x = (Term->wid - 66) / 2;
	if (x < 0) x = 0;

	/* Display spells in readable books */
	if (o_ptr->tval == TV_MAGIC_BOOK)
	{
		if (cp_ptr->spell_book[o_ptr->sval])
		{
			/* Print spells */
			print_spells(o_ptr->sval, FALSE, 0, 1, x);
			return;
		}
	}

	/* Set text_out hook */
	text_out_hook = text_out_to_screen;

	/* Actually display the item */
	list_object(o_ptr, OBJECT_INFO_KNOWN);

	/* Things that need to appear for real objects */
	if (term_obj_real)
	{
		/* Weapon Analysis */
		if (weapon_p(o_ptr) || (o_ptr->tval == TV_DIGGING)) analyze_weapon(o_ptr);
		if (o_ptr->tval == TV_ARROW) analyze_ammo(o_ptr);

		/* Display object history */
		display_object_history(o_ptr);
	}

	/* Display item name */
	obj_top(o_ptr, term_obj_real);
}

