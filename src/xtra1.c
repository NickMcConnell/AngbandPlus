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
 * Converts stat num into a six-char (right justified) string
 */
void cnv_stat(int val, char *out_val)
{
	/* Above 18 */
	if (val > 18)
	{
		int bonus = (val - 18);

		if (bonus >= 220)
		{
			sprintf(out_val, "18/%3s", "***");
		}
		else if (bonus >= 100)
		{
			sprintf(out_val, "18/%03d", bonus);
		}
		else
		{
			sprintf(out_val, " 18/%02d", bonus);
		}
	}

	/* From 3 to 18 */
	else
	{
		sprintf(out_val, "    %2d", val);
	}
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
 * Print character info at given row, column in a 13 char field
 */
static void prt_field(cptr info, int row, int col)
{
	/* Dump 13 spaces to clear */
	c_put_str(TERM_WHITE, "             ", row, col);

	/* Dump the info itself */
	c_put_str(TERM_L_BLUE, info, row, col);
}


/*
 * Extract day, hour, min
 */
void extract_day_hour_min(int *day, int *hour, int *min)
{
	s32b len = 10 * TOWN_DAWN;
	s32b tick = turn % len + len / 4;

	/* We cannot choose undeads on birth, so initial turn is 1 */
	*day = (turn + (10 * TOWN_DAWN / 4))/ len + 1;
	*hour = (24 * tick / len) % 24;
	*min = (1440 * tick / len) % 60;
}

/*
 * Print time
 */
void prt_time(void)
{
	int day, hour, min;

	/* Dump 13 spaces to clear */
	c_put_str(TERM_WHITE, "             ", ROW_DAY, COL_DAY);

	extract_day_hour_min(&day, &hour, &min);

	/* Dump the info itself */
	c_put_str(TERM_WHITE, format(
#ifdef JP
		"%2d日目",
#else
		"Day %-2d",
#endif
		day), ROW_DAY, COL_DAY);
	
	c_put_str(TERM_WHITE, format("%2d:%02d", hour, min), ROW_DAY, COL_DAY+7);
}


cptr map_name(void)
{
	if (p_ptr->inside_quest && (p_ptr->inside_quest < MIN_RANDOM_QUEST)
	    && (quest[p_ptr->inside_quest].flags & QUEST_FLAG_PRESET))
#ifdef JP
		return "クエスト";
#else
		return "Quest";
#endif
	else if (!dun_level && p_ptr->town_num)
		return town[p_ptr->town_num].name;
	else if (!dun_level)
#ifdef JP
		return "地上";
#else
		return "Surface";
#endif
	else
#ifdef JP
		return "アングバンド";
#else
		return "Angband";
#endif
}


/*
 * Print character stat in given row, column
 */
static void prt_stat(int stat)
{
	char tmp[32];

	/* Display "injured" stat */
	if (p_ptr->stat_cur[stat] < p_ptr->stat_max[stat])
	{
		put_str(stat_names_reduced[stat], ROW_STAT + stat, 0);
		cnv_stat(p_ptr->stat_use[stat], tmp);
		c_put_str(TERM_YELLOW, tmp, ROW_STAT + stat, COL_STAT + 6);
	}

	/* Display "healthy" stat */
	else
	{
		put_str(stat_names[stat], ROW_STAT + stat, 0);
		cnv_stat(p_ptr->stat_use[stat], tmp);
		c_put_str(TERM_L_GREEN, tmp, ROW_STAT + stat, COL_STAT + 6);
	}

	/* Indicate natural maximum */
	if (p_ptr->stat_max[stat] == 18+100)
	{
#ifdef JP
		/* 日本語にかぶらないように表示位置を変更 */
		put_str("!", ROW_STAT + stat, 5);
#else
		put_str("!", ROW_STAT + stat, 3);
#endif

	}
}


/*
 *  Data structure for status bar
 */
#define BAR_HALLUCINATION 0
#define BAR_BLINDNESS 1
#define BAR_PARALYZE 2
#define BAR_CONFUSE 3
#define BAR_POISONED 4
#define BAR_AFRAID 5
#define BAR_WRAITH 6
#define BAR_PROTEVIL 7
#define BAR_MAGICDEFENSE 8
#define BAR_STONESKIN 9
#define BAR_REGMAGIC 10
#define BAR_ULTIMATE 11
#define BAR_INVULN 12
#define BAR_RESACID 13
#define BAR_RESELEC 14
#define BAR_RESFIRE 15
#define BAR_RESCOLD 16
#define BAR_RESPOIS 17
#define BAR_SHFIRE 18
#define BAR_BLESSED 19
#define BAR_HEROISM 20
#define BAR_BERSERK 21
#define BAR_ATTKCONF 22
#define BAR_SENSEUNSEEN 23
#define BAR_TELEPATHY 24
#define BAR_REGENERATION 25
#define BAR_INFRAVISION 26
#define BAR_RECALL 27
#define BAR_RADAREYE 28
#define BAR_SHCOLD 29
#define BAR_EXMIGHT 30
#define BAR_BUILDUP 31
#define BAR_ANTITELE 32
#define BAR_ANTIMAGIC 33
#define BAR_EYEFOREYE 34
#define BAR_RESDARK 35
#define BAR_RESNETH 36
#define BAR_SENTENCE 37
#define BAR_RUNESWORD 38
#define BAR_ANTIMULTI 39
#define BAR_VAMPIRIC 40
#define BAR_BRAND 41


static struct {
	byte attr;
	cptr sstr;
	cptr lstr;
} bar[]
#ifdef JP
= {
	{TERM_VIOLET, "幻", "幻覚"},
	{TERM_L_DARK, "盲", "盲目"},
	{TERM_RED, "痺", "麻痺"},
	{TERM_VIOLET, "乱", "混乱"},
	{TERM_GREEN, "毒", "毒"},
	{TERM_BLUE, "恐", "恐怖"},
	{TERM_L_DARK, "幽", "幽体"},
	{TERM_SLATE, "邪", "防邪"},
	{TERM_YELLOW, "魔", "魔法鎧"},
	{TERM_WHITE, "石", "石肌"},
	{TERM_SLATE, "防", "魔法防御"},
	{TERM_YELLOW, "究", "究極"},
	{TERM_YELLOW, "無", "無敵"},
	{TERM_GREEN, "酸", "耐酸"},
	{TERM_BLUE, "電", "耐電"},
	{TERM_RED, "火", "耐火"},
	{TERM_SLATE, "冷", "耐冷"},
	{TERM_GREEN, "毒", "耐毒"},
	{TERM_L_RED, "オ", "火オーラ"},
	{TERM_WHITE, "祝", "祝福"},
	{TERM_WHITE, "勇", "勇"},
	{TERM_RED, "狂", "狂乱"},
	{TERM_RED, "乱", "混乱打撃"},
	{TERM_L_BLUE, "視", "透明視"},
	{TERM_ORANGE, "テ", "テレパシ"},
	{TERM_L_BLUE, "回", "回復"},
	{TERM_L_RED, "赤", "赤外"},
	{TERM_WHITE, "帰", "帰還"},
	{TERM_L_GREEN, "レ", "レーダー"},
	{TERM_WHITE, "オ", "氷オーラ"},
	{TERM_YELLOW, "腕", "腕力強化"},
	{TERM_RED, "肉", "肉体強化"},
	{TERM_ORANGE, "テ", "反テレポ"},
	{TERM_RED, "魔", "反魔法"},
	{TERM_SLATE, "目", "目には目を"},
	{TERM_L_DARK, "暗", "耐暗黒"},
	{TERM_L_DARK, "獄", "耐地獄"},
	{TERM_SLATE, "宣", "宣告"},
	{TERM_L_DARK, "剣", "魔剣化"},
	{TERM_L_DARK, "殖", "反増殖"},
	{TERM_RED, "吸", "吸血打撃"},
	{TERM_L_BLUE, "属", "属性打撃"},
	{0, NULL, NULL}
};
#else
= {
	{TERM_VIOLET, "Hu", "Hullc"},
	{TERM_L_DARK, "Bl", "Blind"},
	{TERM_RED, "Pa", "Paralyzed"},
	{TERM_VIOLET, "Cf", "Confused"},
	{TERM_GREEN, "Po", "Poisoned"},
	{TERM_BLUE, "Af", "Afraid"},
	{TERM_L_DARK, "Wr", "Wraith"},
	{TERM_SLATE, "Ev", "PrtEvl"},
	{TERM_YELLOW, "Md", "MgcArm"},
	{TERM_WHITE, "Ss", "StnSkn"},
	{TERM_SLATE, "Rm", "ResMag"},
	{TERM_YELLOW, "Ul", "Ultima"},
	{TERM_YELLOW, "Iv", "Invuln"},
	{TERM_GREEN, "Ac", "Acid"},
	{TERM_BLUE, "El", "Elec"},
	{TERM_RED, "Fi", "Fire"},
	{TERM_SLATE, "Co", "Cold"},
	{TERM_GREEN, "Po", "Pois"},
	{TERM_L_RED, "SFi", "SFire"},
	{TERM_WHITE, "Bs", "Bless"},
	{TERM_WHITE, "He", "Hero"},
	{TERM_RED, "Br", "Berserk"},
	{TERM_RED, "TCf", "TchCnf"},
	{TERM_L_BLUE, "Se", "SInv"},
	{TERM_ORANGE, "Te", "Telepa"},
	{TERM_L_BLUE, "Rg", "Regen"},
	{TERM_L_RED, "If", "Infr"},
	{TERM_WHITE, "Rc", "Recall"},
	{TERM_L_GREEN, "Rd", "Rader"},
	{TERM_WHITE, "SCo", "SCold"},
	{TERM_YELLOW, "EMi", "ExMight"},
	{TERM_RED, "Bu", "BuildUp"},
	{TERM_ORANGE, "AT", "AntiTele"},
	{TERM_RED, "AM", "AntiMagic"},
	{TERM_SLATE, "Ey", "Eye-Eye"},
	{TERM_L_DARK, "Dk", "Dark"},
	{TERM_L_DARK, "Nt", "Nether"},
	{TERM_SLATE, "Rv", "Revenge"},
	{TERM_L_DARK, "Rs", "RuneSword"},
	{TERM_L_DARK, "AMl", "AntiMulti"},
	{TERM_RED, "Vm", "Vampiric"},
	{TERM_L_BLUE, "Brn", "XtraBrand"},
	{0, NULL, NULL}
};
#endif

#define ADD_FLG(FLG) (bar_flags[FLG / 32] |= (1L << (FLG % 32)))
#define IS_FLG(FLG) (bar_flags[FLG / 32] & (1L << (FLG % 32)))


/*
 *  Show status bar
 */
static void prt_status(void)
{
	u32b bar_flags[2];
	int wid, hgt, row_statbar, max_col_statbar;
	int i, col = 0, num = 0;
	int space = 2;

	Term_get_size(&wid, &hgt);
	row_statbar = hgt + ROW_STATBAR;
	max_col_statbar = wid + MAX_COL_STATBAR;

	Term_erase(0, row_statbar, max_col_statbar);

	bar_flags[0] = 0L;
	bar_flags[1] = 0L;

	/* Hallucinating */
	if (p_ptr->image) ADD_FLG(BAR_HALLUCINATION);

	/* Blindness */
	if (p_ptr->blind) ADD_FLG(BAR_BLINDNESS);

	/* Paralysis */
	if (p_ptr->paralyzed) ADD_FLG(BAR_PARALYZE);

	/* Confusion */
	if (p_ptr->confused) ADD_FLG(BAR_CONFUSE);

	/* Posioned */
	if (p_ptr->poisoned) ADD_FLG(BAR_POISONED);

	/* Times see-invisible */
	if (p_ptr->tim_invis) ADD_FLG(BAR_SENSEUNSEEN);

	/* Timed esp */
	if (p_ptr->tim_esp) ADD_FLG(BAR_TELEPATHY);

	/* Timed regenerate */
	if (p_ptr->tim_regen) ADD_FLG(BAR_REGENERATION);

	/* Timed infra-vision */
	if (p_ptr->tim_infra) ADD_FLG(BAR_INFRAVISION);

	/* Timed radar-eye */
	if (p_ptr->tim_radar) ADD_FLG(BAR_RADAREYE);

	/* Protection from evil */
	if (p_ptr->protevil) ADD_FLG(BAR_PROTEVIL);

	/* Invulnerability */
	if (p_ptr->invuln) ADD_FLG(BAR_INVULN);

	/* Wraith form */
	if (p_ptr->wraith_form) ADD_FLG(BAR_WRAITH);

	/* Heroism */
	if (p_ptr->hero) ADD_FLG(BAR_HEROISM);

	/* Super Heroism / berserk */
	if (p_ptr->shero) ADD_FLG(BAR_BERSERK);

	/* Blessed */
	if (p_ptr->blessed) ADD_FLG(BAR_BLESSED);

	/* Shield */
	if (p_ptr->magicdef) ADD_FLG(BAR_MAGICDEFENSE);

	if (p_ptr->shield) ADD_FLG(BAR_STONESKIN);

	/* Oppose Acid */
	if (p_ptr->oppose_acid) ADD_FLG(BAR_RESACID);

	/* Oppose Lightning */
	if (p_ptr->oppose_elec) ADD_FLG(BAR_RESELEC);

	/* Oppose Fire */
	if (p_ptr->oppose_fire) ADD_FLG(BAR_RESFIRE);

	/* Oppose Cold */
	if (p_ptr->oppose_cold) ADD_FLG(BAR_RESCOLD);

	/* Oppose Poison */
	if (p_ptr->oppose_pois) ADD_FLG(BAR_RESPOIS);

	/* Word of Recall */
	if (p_ptr->word_recall) ADD_FLG(BAR_RECALL);

	/* Afraid */
	if (p_ptr->afraid) ADD_FLG(BAR_AFRAID);

	/* Confusing Hands */
	if (p_ptr->confusing) ADD_FLG(BAR_ATTKCONF);

	if (p_ptr->resist_magic) ADD_FLG(BAR_REGMAGIC);

	/* Ultimate-resistance */
	if (p_ptr->musou) ADD_FLG(BAR_ULTIMATE);

	/* Fire Aura */
	if (p_ptr->tim_sh_fire) ADD_FLG(BAR_SHFIRE);

	/* Extra Might */
	if (p_ptr->tim_might) ADD_FLG(BAR_EXMIGHT);

	/* Extra Brand */
	if (p_ptr->tim_brand) ADD_FLG(BAR_BRAND);

	/* Calcurate length */
	for (i = 0; bar[i].sstr; i++)
	{
		if (IS_FLG(i))
		{
			col += strlen(bar[i].lstr) + 1;
			num++;
		}
	}

	/* If there are not excess spaces for long strings, use short one */
	if (col - 1 > max_col_statbar)
	{
		space = 0;
		col = 0;

		for (i = 0; bar[i].sstr; i++)
		{
			if (IS_FLG(i))
			{
				col += strlen(bar[i].sstr);
			}
		}

		/* If there are excess spaces for short string, use more */
		if (col - 1 <= max_col_statbar - (num-1))
		{
			space = 1;
			col += num - 1;
		}
	}


	/* Centering display column */
	col = (max_col_statbar - col) / 2;

	/* Display status bar */
	for (i = 0; bar[i].sstr; i++)
	{
		if (IS_FLG(i))
		{
			cptr str;
			if (space == 2) str = bar[i].lstr;
			else str = bar[i].sstr;

			c_put_str(bar[i].attr, str, row_statbar, col);
			col += strlen(str);
			if (space > 0) col++;
			if (col > max_col_statbar) break;
		}
	}
}



/*
 * Prints "title", including "wizard" or "winner" as needed.
 */
static void prt_title(void)
{
	cptr p;

	/* Wizard */
	if (wizard)
	{
#ifdef JP
		p = "[ウィザード]";
#else
		p = "[=-WIZARD-=]";
#endif

	}

	/* Winner */
	else if (total_winner || (p_ptr->lev > PY_MAX_LEVEL))
	{
#ifdef JP
		p = "***勝利者***";

#else
		p = "***WINNER***";
#endif

	}

	/* Normal */
	else
	{
		p = player_title[p_ptr->pclass][(p_ptr->lev - 1) / 5];
	}

	prt_field(p, ROW_TITLE, COL_TITLE);
}


/*
 * Prints level
 */
static void prt_level(void)
{
	char tmp[32];

#ifdef JP
	sprintf(tmp, "%5d", p_ptr->lev);
#else
	sprintf(tmp, "%6d", p_ptr->lev);
#endif


	if (p_ptr->lev >= p_ptr->max_plv)
	{
#ifdef JP
		put_str("レベル ", ROW_LEVEL, 0);
		c_put_str(TERM_L_GREEN, tmp, ROW_LEVEL, COL_LEVEL + 7);
#else
		put_str("LEVEL ", ROW_LEVEL, 0);
		c_put_str(TERM_L_GREEN, tmp, ROW_LEVEL, COL_LEVEL + 6);
#endif

	}
	else
	{
#ifdef JP
		put_str("xレベル", ROW_LEVEL, 0);
		c_put_str(TERM_YELLOW, tmp, ROW_LEVEL, COL_LEVEL + 7);
#else
		put_str("Level ", ROW_LEVEL, 0);
		c_put_str(TERM_YELLOW, tmp, ROW_LEVEL, COL_LEVEL + 6);
#endif

	}
}


/*
 * Display the experience
 */
static void prt_exp(void)
{
	char out_val[32];

	if (!exp_need)
	{
#ifdef JP
		(void)sprintf(out_val, "%7ld", (long)p_ptr->exp);
#else
		(void)sprintf(out_val, "%8ld", (long)p_ptr->exp);
#endif
	}
	else
	{
		if (p_ptr->lev >= PY_MAX_LEVEL)
		{
			(void)sprintf(out_val, "********");
		}		
		else
		{
#ifdef JP
			(void)sprintf(out_val, "%7ld", (long)(player_exp[p_ptr->lev - 1] * p_ptr->expfact / 100L) - p_ptr->exp);
#else
			(void)sprintf(out_val, "%8ld", (long)(player_exp[p_ptr->lev - 1] * p_ptr->expfact / 100L) - p_ptr->exp);
#endif
		}
	}

	if (p_ptr->exp >= p_ptr->max_exp)
	{
#ifdef JP
		put_str("経験 ", ROW_EXP, 0);
		c_put_str(TERM_L_GREEN, out_val, ROW_EXP, COL_EXP + 5);
#else
		put_str("EXP ", ROW_EXP, 0);
		c_put_str(TERM_L_GREEN, out_val, ROW_EXP, COL_EXP + 4);
#endif

	}
	else
	{
#ifdef JP
		put_str("x経験", ROW_EXP, 0);
		c_put_str(TERM_YELLOW, out_val, ROW_EXP, COL_EXP + 5);
#else
		put_str("Exp ", ROW_EXP, 0);
		c_put_str(TERM_YELLOW, out_val, ROW_EXP, COL_EXP + 4);
#endif

	}
}


/*
 * Prints current gold
 */
static void prt_gold(void)
{
	char tmp[32];

#ifdef JP
	put_str("＄ ", ROW_GOLD, COL_GOLD);
#else
	put_str("AU ", ROW_GOLD, COL_GOLD);
#endif

	sprintf(tmp, "%9ld", (long)p_ptr->au);
	c_put_str(TERM_L_GREEN, tmp, ROW_GOLD, COL_GOLD + 3);
}



/*
 * Prints current AC
 */
static void prt_ac(void)
{
	char tmp[32];

#ifdef JP
/* AC の表示方式を変更している */
	put_str(" ＡＣ(     )", ROW_AC, COL_AC);
	sprintf(tmp, "%5d", p_ptr->dis_ac + p_ptr->dis_to_a);
	c_put_str(TERM_L_GREEN, tmp, ROW_AC, COL_AC + 6);
#else
	put_str("Cur AC ", ROW_AC, COL_AC);
	sprintf(tmp, "%5d", p_ptr->dis_ac + p_ptr->dis_to_a);
	c_put_str(TERM_L_GREEN, tmp, ROW_AC, COL_AC + 7);
#endif

}


/*
 * Prints Cur/Max hit points
 */
static void prt_hp(void)
{
	/* ヒットポイントの表示方法を変更 */
	char tmp[32];

	byte color;

	/* タイトル */
	put_str("HP", ROW_CURHP, COL_CURHP);

	/* 現在のヒットポイント */
	sprintf(tmp, "%4d", p_ptr->chp);

	if (p_ptr->chp >= p_ptr->mhp)
	{
		color = TERM_L_GREEN;
	}
	else if (p_ptr->chp > (p_ptr->mhp * hitpoint_warn) / 10)
	{
		color = TERM_YELLOW;
	}
	else
	{
		color = TERM_RED;
	}

	c_put_str(color, tmp, ROW_CURHP, COL_CURHP+3);

	/* 区切り */
	put_str( "/", ROW_CURHP, COL_CURHP + 7 );

	/* 最大ヒットポイント */
	sprintf(tmp, "%4d", p_ptr->mhp);
	color = TERM_L_GREEN;

	c_put_str(color, tmp, ROW_CURHP, COL_CURHP + 8 );
}


/*
 * Prints players max/cur spell points
 */
static void prt_sp(void)
{
	/* マジックポイントの表示方法を変更している */
	char tmp[32];
	byte color;

	/* Do not show mana unless it matters */
	if (!mp_ptr->spell_type) return;

	/* タイトル */
#ifdef JP
	put_str("MP", ROW_CURSP, COL_CURSP);
#else
	put_str("SP", ROW_CURSP, COL_CURSP);
#endif

	/* 現在のマジックポイント */
	sprintf(tmp, "%4d", p_ptr->csp);

	if (p_ptr->csp >= p_ptr->msp)
	{
		color = TERM_L_GREEN;
	}
	else if (p_ptr->csp > (p_ptr->msp * spellpoint_warn) / 10)
	{
		color = TERM_YELLOW;
	}
	else
	{
		color = TERM_RED;
	}

	c_put_str(color, tmp, ROW_CURSP, COL_CURSP+3);

	/* 区切り */
	put_str( "/", ROW_CURSP, COL_CURSP + 7 );

	/* 最大マジックポイント */
	sprintf(tmp, "%4d", p_ptr->msp);
	color = TERM_L_GREEN;

	c_put_str(color, tmp, ROW_CURSP, COL_CURSP + 8);
}


/*
 * Prints depth in stat area
 */
static void prt_depth(void)
{
	char depths[32];
	int wid, hgt, row_depth, col_depth;
	byte attr = TERM_WHITE;

	Term_get_size(&wid, &hgt);
	col_depth = wid + COL_DEPTH;
	row_depth = hgt + ROW_DEPTH;

	if (p_ptr->inside_arena)
	{
#ifdef JP
		strcpy(depths, "アリーナ");
#else
		strcpy(depths, "Arena");
#endif
	}
	else if (p_ptr->inside_quest && quest[p_ptr->inside_quest].type != QUEST_TYPE_RANDOM)
	{
#ifdef JP
		strcpy(depths, "クエスト");
#else
		strcpy(depths, "Quest");
#endif
	}
	else if (!dun_level)
	{
		if (p_ptr->town_num)
			strcpy(depths, town[p_ptr->town_num].name);
		else
#ifdef JP
			strcpy(depths, "荒野");
#else
			strcpy(depths, "Wilderness");
#endif
	}
	else
	{
#ifdef JP
		if (depth_in_feet) (void)sprintf(depths, "%d ft", dun_level * 50);
		else (void)sprintf(depths, "地下 %d 階", dun_level);
#else
		if (depth_in_feet) (void)sprintf(depths, "%d ft", dun_level * 50);
		else (void)sprintf(depths, "Lev %d", dun_level);
#endif

		/* Get color of level based on feeling  -JSV- */
		switch (p_ptr->feeling)
		{
		case  0: attr = TERM_SLATE;   break; /* Unknown */
		case  1: attr = TERM_L_BLUE;  break; /* Special */
		case  2: attr = TERM_VIOLET;  break; /* Horrible visions */
		case  3: attr = TERM_RED;     break; /* Very dangerous */
		case  4: attr = TERM_L_RED;   break; /* Very bad feeling */
		case  5: attr = TERM_ORANGE;  break; /* Bad feeling */
		case  6: attr = TERM_YELLOW;  break; /* Nervous */
		case  7: attr = TERM_L_UMBER; break; /* Luck is turning */
		case  8: attr = TERM_L_WHITE; break; /* Don't like */
		case  9: attr = TERM_WHITE;   break; /* Reasonably safe */
		case 10: attr = TERM_WHITE;   break; /* Boring place */
		}
	}

	/* Right-Adjust the "depth", and clear old values */
	c_prt(attr, format("%12s", depths), row_depth, col_depth);
}


/*
 * Prints status of hunger
 */
static void prt_hunger(void)
{
	/* Fainting / Starving */
	if (p_ptr->food < PY_FOOD_FAINT)
	{
#ifdef JP
		c_put_str(TERM_RED, "衰弱  ", ROW_HUNGRY, COL_HUNGRY);
#else
		c_put_str(TERM_RED, "Weak  ", ROW_HUNGRY, COL_HUNGRY);
#endif

	}

	/* Weak */
	else if (p_ptr->food < PY_FOOD_WEAK)
	{
#ifdef JP
		c_put_str(TERM_ORANGE, "衰弱  ", ROW_HUNGRY, COL_HUNGRY);
#else
		c_put_str(TERM_ORANGE, "Weak  ", ROW_HUNGRY, COL_HUNGRY);
#endif

	}

	/* Hungry */
	else if (p_ptr->food < PY_FOOD_ALERT)
	{
#ifdef JP
		c_put_str(TERM_YELLOW, "空腹  ", ROW_HUNGRY, COL_HUNGRY);
#else
		c_put_str(TERM_YELLOW, "Hungry", ROW_HUNGRY, COL_HUNGRY);
#endif

	}

	/* Normal */
	else if (p_ptr->food < PY_FOOD_FULL)
	{
		c_put_str(TERM_L_GREEN, "      ", ROW_HUNGRY, COL_HUNGRY);
	}

	/* Full */
	else if (p_ptr->food < PY_FOOD_MAX)
	{
#ifdef JP
		c_put_str(TERM_L_GREEN, "満腹  ", ROW_HUNGRY, COL_HUNGRY);
#else
		c_put_str(TERM_L_GREEN, "Full  ", ROW_HUNGRY, COL_HUNGRY);
#endif

	}

	/* Gorged */
	else
	{
#ifdef JP
		c_put_str(TERM_GREEN, "食過ぎ", ROW_HUNGRY, COL_HUNGRY);
#else
		c_put_str(TERM_GREEN, "Gorged", ROW_HUNGRY, COL_HUNGRY);
#endif

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
	byte attr = TERM_WHITE;

	char text[5];

	/* Resting */
	if (resting)
	{
		int i;

		/* Start with blank */
		strcpy(text, "    ");

		/* Extensive (timed) rest */
		if (resting >= 1000)
		{
			i = resting / 100;
			text[3] = '0';
			text[2] = '0';
			text[1] = '0' + (i % 10);
			text[0] = '0' + (i / 10);
		}

		/* Long (timed) rest */
		else if (resting >= 100)
		{
			i = resting;
			text[3] = '0' + (i % 10);
			i = i / 10;
			text[2] = '0' + (i % 10);
			text[1] = '0' + (i / 10);
		}

		/* Medium (timed) rest */
		else if (resting >= 10)
		{
			i = resting;
			text[3] = '0' + (i % 10);
			text[2] = '0' + (i / 10);
		}

		/* Short (timed) rest */
		else if (resting > 0)
		{
			i = resting;
			text[3] = '0' + (i);
		}

		/* Rest until healed */
		else if (resting == -1)
		{
			text[0] = text[1] = text[2] = text[3] = '*';
		}

		/* Rest until done */
		else if (resting == -2)
		{
			text[0] = text[1] = text[2] = text[3] = '&';
		}
	}

	/* Repeating */
	else if (command_rep)
	{
		if (command_rep > 999)
		{
			sprintf(text, "%2d00", command_rep / 100);
		}
		else
		{
			(void)sprintf(text, "  %2d", command_rep);
		}
	}

	/* Searching */
	else if (p_ptr->searching)
	{
#ifdef JP
		strcpy(text, "探索");
#else
		strcpy(text, "Srch");
#endif

	}

	/* Nothing interesting */
	else
	{
		strcpy(text, "    ");
	}

	/* Display the info (or blanks) */
	c_put_str(attr, format("%5.5s", text), ROW_STATE, COL_STATE);
}


/*
 * Prints the speed of a character.			-CJS-
 */
static void prt_speed(void)
{
	int i = p_ptr->pspeed;

	byte attr = TERM_WHITE;
	char buf[32] = "";
	int wid, hgt, row_speed, col_speed;

	Term_get_size(&wid, &hgt);
	col_speed = wid + COL_SPEED;
	row_speed = hgt + ROW_SPEED;

	/* Hack -- Visually "undo" the Search Mode Slowdown */
	if (p_ptr->searching) i += 10;

	/* Fast */
	if (i > 110)
	{
		if (p_ptr->fast && !p_ptr->slow) attr = TERM_YELLOW;
		else if (!p_ptr->fast && p_ptr->slow) attr = TERM_VIOLET;
		else attr = TERM_L_GREEN;
#ifdef JP
		sprintf(buf, "加速 (+%d)", (i - 110));
#else
		sprintf(buf, "Fast (+%d)", (i - 110));
#endif
	}

	/* Slow */
	else if (i < 110)
	{
		if (p_ptr->fast && !p_ptr->slow) attr = TERM_YELLOW;
		else if (!p_ptr->fast && p_ptr->slow) attr = TERM_VIOLET;
		else attr = TERM_L_UMBER;
#ifdef JP
		sprintf(buf, "減速 (-%d)", (110 - i));
#else
		sprintf(buf, "Slow (-%d)", (110 - i));
#endif
	}

	/* Display the speed */
	c_put_str(attr, format("%-10s", buf), row_speed, col_speed);
}


static void prt_study(void)
{
	int wid, hgt, row_study, col_study;

	Term_get_size(&wid, &hgt);
	col_study = wid + COL_STUDY;
	row_study = hgt + ROW_STUDY;

	if (p_ptr->new_spells)
	{
#ifdef JP
		put_str("学習 ", row_study, col_study);
#else
		put_str("Study", row_study, col_study);
#endif

	}
	else
	{
		put_str("     ", row_study, col_study);
	}
}


static void prt_cut(void)
{
	int c = p_ptr->cut;

	if (c > 1000)
	{
#ifdef JP
		c_put_str(TERM_L_RED, "致命傷      ", ROW_CUT, COL_CUT);
#else
		c_put_str(TERM_L_RED, "Mortal wound", ROW_CUT, COL_CUT);
#endif

	}
	else if (c > 200)
	{
#ifdef JP
		c_put_str(TERM_RED, "ひどい深手  ", ROW_CUT, COL_CUT);
#else
		c_put_str(TERM_RED, "Deep gash   ", ROW_CUT, COL_CUT);
#endif

	}
	else if (c > 100)
	{
#ifdef JP
		c_put_str(TERM_RED, "重傷        ", ROW_CUT, COL_CUT);
#else
		c_put_str(TERM_RED, "Severe cut  ", ROW_CUT, COL_CUT);
#endif

	}
	else if (c > 50)
	{
#ifdef JP
		c_put_str(TERM_ORANGE, "大変な傷    ", ROW_CUT, COL_CUT);
#else
		c_put_str(TERM_ORANGE, "Nasty cut   ", ROW_CUT, COL_CUT);
#endif

	}
	else if (c > 25)
	{
#ifdef JP
		c_put_str(TERM_ORANGE, "ひどい傷    ", ROW_CUT, COL_CUT);
#else
		c_put_str(TERM_ORANGE, "Bad cut     ", ROW_CUT, COL_CUT);
#endif

	}
	else if (c > 10)
	{
#ifdef JP
		c_put_str(TERM_YELLOW, "軽傷        ", ROW_CUT, COL_CUT);
#else
		c_put_str(TERM_YELLOW, "Light cut   ", ROW_CUT, COL_CUT);
#endif

	}
	else if (c)
	{
#ifdef JP
		c_put_str(TERM_YELLOW, "かすり傷    ", ROW_CUT, COL_CUT);
#else
		c_put_str(TERM_YELLOW, "Graze       ", ROW_CUT, COL_CUT);
#endif

	}
	else
	{
		put_str("            ", ROW_CUT, COL_CUT);
	}
}



static void prt_stun(void)
{
	int s = p_ptr->stun;

	if (s > 100)
	{
#ifdef JP
		c_put_str(TERM_RED, "意識不明瞭  ", ROW_STUN, COL_STUN);
#else
		c_put_str(TERM_RED, "Knocked out ", ROW_STUN, COL_STUN);
#endif

	}
	else if (s > 50)
	{
#ifdef JP
		c_put_str(TERM_ORANGE, "ひどく朦朧  ", ROW_STUN, COL_STUN);
#else
		c_put_str(TERM_ORANGE, "Heavy stun  ", ROW_STUN, COL_STUN);
#endif

	}
	else if (s)
	{
#ifdef JP
		c_put_str(TERM_ORANGE, "朦朧        ", ROW_STUN, COL_STUN);
#else
		c_put_str(TERM_ORANGE, "Stun        ", ROW_STUN, COL_STUN);
#endif

	}
	else
	{
		put_str("            ", ROW_STUN, COL_STUN);
	}
}



/*
 * Redraw the "monster health bar"	-DRS-
 * Rather extensive modifications by	-BEN-
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
 */
static void health_redraw(void)
{

#ifdef DRS_SHOW_HEALTH_BAR

	/* Not tracking */
	if (!p_ptr->health_who)
	{
		/* Erase the health bar */
		Term_erase(COL_INFO, ROW_INFO, 12);
	}

	/* Tracking an unseen monster */
	else if (!m_list[p_ptr->health_who].ml)
	{
		/* Indicate that the monster health is "unknown" */
		Term_putstr(COL_INFO, ROW_INFO, 12, TERM_WHITE, "[----------]");
	}

	/* Tracking a hallucinatory monster */
	else if (p_ptr->image)
	{
		/* Indicate that the monster health is "unknown" */
		Term_putstr(COL_INFO, ROW_INFO, 12, TERM_WHITE, "[----------]");
	}

	/* Tracking a dead monster (???) */
	else if (!m_list[p_ptr->health_who].hp < 0)
	{
		/* Indicate that the monster health is "unknown" */
		Term_putstr(COL_INFO, ROW_INFO, 12, TERM_WHITE, "[----------]");
	}

	/* Tracking a visible monster */
	else
	{
		int pct, len;

		monster_type *m_ptr = &m_list[p_ptr->health_who];

		/* Default to almost dead */
		byte attr = TERM_RED;

		/* Extract the "percent" of health */
		pct = 100L * m_ptr->hp / m_ptr->maxhp;

		/* Badly wounded */
		if (pct >= 10) attr = TERM_L_RED;

		/* Wounded */
		if (pct >= 25) attr = TERM_ORANGE;

		/* Somewhat Wounded */
		if (pct >= 60) attr = TERM_YELLOW;

		/* Healthy */
		if (pct >= 100) attr = TERM_L_GREEN;

		/* Afraid */
		if (m_ptr->monfear) attr = TERM_VIOLET;

		/* Asleep */
		if (m_ptr->csleep) attr = TERM_BLUE;

		/* Invulnerable */
		if (m_ptr->invulner) attr = TERM_WHITE;

		/* Convert percent into "health" */
		len = (pct < 10) ? 1 : (pct < 90) ? (pct / 10 + 1) : 10;

		/* Default to "unknown" */
		Term_putstr(COL_INFO, ROW_INFO, 12, TERM_WHITE, "[----------]");

		/* Dump the current "health" (use '*' symbols) */
		Term_putstr(COL_INFO + 1, ROW_INFO, len, attr, "**********");
	}

#endif

}



/*
 * Display basic info (mostly left of map)
 */
static void prt_frame_basic(void)
{
	int i;

	/* Race and Class */
	prt_field(rp_ptr->title, ROW_RACE, COL_RACE);
	prt_field(cp_ptr->title, ROW_CLASS, COL_CLASS);

	/* Title */
	prt_title();

	/* Level/Experience */
	prt_level();
	prt_exp();

	/* All Stats */
	for (i = 0; i < 6; i++) prt_stat(i);

	/* Armor */
	prt_ac();

	/* Hitpoints */
	prt_hp();

	/* Spellpoints */
	prt_sp();

	/* Gold */
	prt_gold();

	/* Current depth */
	prt_depth();

	/* Special */
	health_redraw();
}


/*
 * Display extra info (mostly below map)
 */
static void prt_frame_extra(void)
{
	/* Cut/Stun */
	prt_cut();
	prt_stun();

	/* Food */
	prt_hunger();

	/* State */
	prt_state();

	/* Speed */
	prt_speed();

	/* Study spells */
	prt_study();

	/* Status Bar */
	prt_status();
}


/*
 * Hack -- display inventory in sub-windows
 */
static void fix_inven(void)
{
	int j;

	/* Scan windows */
	for (j = 0; j < 8; j++)
	{
		term *old = Term;

		/* No window */
		if (!angband_term[j]) continue;

		/* No relevant flags */
		if (!(window_flag[j] & (PW_INVEN))) continue;

		/* Activate */
		Term_activate(angband_term[j]);

		/* Display inventory */
		display_inven();

		/* Fresh */
		Term_fresh();

		/* Restore */
		Term_activate(old);
	}
}



/*
 * Hack -- display equipment in sub-windows
 */
static void fix_equip(void)
{
	int j;

	/* Scan windows */
	for (j = 0; j < 8; j++)
	{
		term *old = Term;

		/* No window */
		if (!angband_term[j]) continue;

		/* No relevant flags */
		if (!(window_flag[j] & (PW_EQUIP))) continue;

		/* Activate */
		Term_activate(angband_term[j]);

		/* Display equipment */
		display_equip();

		/* Fresh */
		Term_fresh();

		/* Restore */
		Term_activate(old);
	}
}


/*
 * Hack -- display equipment in sub-windows
 */
static void fix_spell(void)
{
	int j;

	/* Scan windows */
	for (j = 0; j < 8; j++)
	{
		term *old = Term;

		/* No window */
		if (!angband_term[j]) continue;

		/* No relevant flags */
		if (!(window_flag[j] & (PW_SPELL))) continue;

		/* Activate */
		Term_activate(angband_term[j]);

		/* Display spell list */
		display_spell_list();

		/* Fresh */
		Term_fresh();

		/* Restore */
		Term_activate(old);
	}
}


/*
 * Hack -- display character in sub-windows
 */
static void fix_player(int mode)
{
	int j;

	/* Scan windows */
	for (j = 0; j < 8; j++)
	{
		term *old = Term;

		/* No window */
		if (!angband_term[j]) continue;

		/* No relevant flags */
		if (!(window_flag[j] & (PW_PLAYER | PW_STATS))) continue;

		/* Activate */
		Term_activate(angband_term[j]);

		/* Update playtime */
		update_playtime();

		/* Display player */
		display_player(mode);

		/* Fresh */
		Term_fresh();

		/* Restore */
		Term_activate(old);
	}
}



/*
 * Hack -- display recent messages in sub-windows
 *
 * XXX XXX XXX Adjust for width and split messages
 */
static void fix_message(void)
{
	int j, i;
	int w, h;
	int x, y;

	/* Scan windows */
	for (j = 0; j < 8; j++)
	{
		term *old = Term;

		/* No window */
		if (!angband_term[j]) continue;

		/* No relevant flags */
		if (!(window_flag[j] & (PW_MESSAGE))) continue;

		/* Activate */
		Term_activate(angband_term[j]);

		/* Get size */
		Term_get_size(&w, &h);

		/* Dump messages */
		for (i = 0; i < h; i++)
		{
			/* Dump the message on the appropriate line */
			Term_putstr(0, (h - 1) - i, -1, (byte)((i < p_ptr->now_message) ? TERM_WHITE : TERM_SLATE), message_str(i));

			/* Cursor */
			Term_locate(&x, &y);

			/* Clear to end of line */
			Term_erase(x, y, 255);
		}

		/* Fresh */
		Term_fresh();

		/* Restore */
		Term_activate(old);
	}
}


/*
 * Hack -- display overhead view in sub-windows
 *
 * Note that the "player" symbol does NOT appear on the map.
 */
static void fix_overhead(void)
{
	int j;

	int cy, cx;

	/* Scan windows */
	for (j = 0; j < 8; j++)
	{
		term *old = Term;

		/* No window */
		if (!angband_term[j]) continue;

		/* No relevant flags */
		if (!(window_flag[j] & (PW_OVERHEAD))) continue;

		/* Activate */
		Term_activate(angband_term[j]);

		/* Redraw map */
		display_map(&cy, &cx);

		/* Fresh */
		Term_fresh();

		/* Restore */
		Term_activate(old);
	}
}


/*
 * Hack -- display dungeon view in sub-windows
 */
static void fix_dungeon(void)
{
	int j;

	/* Scan windows */
	for (j = 0; j < 8; j++)
	{
		term *old = Term;

		/* No window */
		if (!angband_term[j]) continue;

		/* No relevant flags */
		if (!(window_flag[j] & (PW_DUNGEON))) continue;

		/* Activate */
		Term_activate(angband_term[j]);

		/* Redraw dungeon view */
		display_dungeon();

		/* Fresh */
		Term_fresh();

		/* Restore */
		Term_activate(old);
	}
}


/*
 * Hack -- display monster recall in sub-windows
 */
static void fix_monster(void)
{
	int j;

	/* Scan windows */
	for (j = 0; j < 8; j++)
	{
		term *old = Term;

		/* No window */
		if (!angband_term[j]) continue;

		/* No relevant flags */
		if (!(window_flag[j] & (PW_MONSTER))) continue;

		/* Activate */
		Term_activate(angband_term[j]);

		/* Display monster race info */
		if (p_ptr->monster_race_idx) display_roff(p_ptr->monster_race_idx);

		/* Fresh */
		Term_fresh();

		/* Restore */
		Term_activate(old);
	}
}


/*
 * Hack -- display object recall in sub-windows
 */
static void fix_object(void)
{
	int j;

	/* Scan windows */
	for (j = 0; j < 8; j++)
	{
		term *old = Term;

		/* No window */
		if (!angband_term[j]) continue;

		/* No relevant flags */
		if (!(window_flag[j] & (PW_OBJECT))) continue;

		/* Activate */
		Term_activate(angband_term[j]);

		/* Display monster race info */
		if (p_ptr->object_kind_idx) display_koff(p_ptr->object_kind_idx);

		/* Fresh */
		Term_fresh();

		/* Restore */
		Term_activate(old);
	}
}


/*
 * Hack -- display objects on floor in sub-windows
 */
static void fix_floor(void)
{
	int j;

	/* Scan windows */
	for (j = 0; j < 8; j++)
	{
		term *old = Term;

		/* No window */
		if (!angband_term[j]) continue;

		/* No relevant flags */
		if (!(window_flag[j] & (PW_FLOOR))) continue;

		/* Activate */
		Term_activate(angband_term[j]);

		/* Display objects */
		clear_from(0);
		show_floor(look_y, look_x);

		/* Fresh */
		Term_fresh();

		/* Restore */
		Term_activate(old);
	}
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
	int			i, j, k, levels;
	int			num_allowed, num_known;

	magic_type		*s_ptr;
	int use_realm1 = p_ptr->realm1 - 1;
	int use_realm2 = p_ptr->realm2 - 1;
	int which;


#ifdef JP
	cptr p = ((mp_ptr->spell_type == ST_SPELL) ? "呪文" : "祈り");
#else
	cptr p = ((mp_ptr->spell_type == ST_SPELL) ? "spell" : "prayer");
#endif


	/* Hack -- must be literate */
	if (!mp_ptr->spell_type) return;

	/* Hack -- wait for creation */
	if (!character_generated) return;

	/* Hack -- handle "xtra" mode */
	if (character_xtra) return;


	/* Determine the number of spells allowed */
	levels = p_ptr->lev - mp_ptr->spell_first + 1;

	/* Hack -- no negative spells */
	if (levels < 0) levels = 0;

	/* Extract total allowed spells */
	num_allowed = (adj_mag_study[p_ptr->stat_ind[mp_ptr->spell_stat]] * levels / 2);


	/* Assume none known */
	num_known = 0;

	/* Count the number of spells we know */
	for (j = 0; j < 64; j++)
	{
		/* Count known spells */
		if ((j < 32) ?
		    (spell_learned1 & (1L << j)) :
		    (spell_learned2 & (1L << (j - 32))))
		{
			num_known++;
		}
	}

	/* See how many spells we must forget or may learn */
	p_ptr->new_spells = num_allowed - num_known;


	/* Forget spells which are too hard */
	for (i = 63; i >= 0; i--)
	{
		/* Efficiency -- all done */
		if (!spell_learned1 && !spell_learned2) break;

		/* Access the spell */
		j = spell_order[i];

		/* Skip non-spells */
		if (j >= 99) continue;


		/* Get the spell */
		if (j < 32)
			s_ptr = &mp_ptr->info[use_realm1][j];
		else
			s_ptr = &mp_ptr->info[use_realm2][j%32];

		/* Skip spells we are allowed to know */
		if (s_ptr->slevel <= p_ptr->lev) continue;

		/* Is it known? */
		if ((j < 32) ?
		    (spell_learned1 & (1L << j)) :
		    (spell_learned2 & (1L << (j - 32))))
		{
			/* Mark as forgotten - no longer known */
			if (j < 32)
			{
				spell_forgotten1 |= (1L << j);
				spell_learned1 &= ~(1L << j);
				which = use_realm1;
			}
			else
			{
				spell_forgotten2 |= (1L << (j - 32));
				spell_learned2 &= ~(1L << (j - 32));
				which = use_realm2;
			}

			/* Message */
#ifdef JP
			msg_format("%sの%sを忘れてしまった。",
				   spell_names[which][j%32], p );
#else
			msg_format("You have forgotten the %s of %s.", p,
			spell_names[which][j%32]);
#endif


			/* One more can be learned */
			p_ptr->new_spells++;
		}
	}


	/* Forget spells if we know too many spells */
	for (i = 63; i >= 0; i--)
	{
		/* Stop when possible */
		if (p_ptr->new_spells >= 0) break;

		/* Efficiency -- all done */
		if (!spell_learned1 && !spell_learned2) break;

		/* Get the (i+1)th spell learned */
		j = spell_order[i];

		/* Skip unknown spells */
		if (j >= 99) continue;

		/* Forget it (if learned) */
		if ((j < 32) ?
		    (spell_learned1 & (1L << j)) :
		    (spell_learned2 & (1L << (j - 32))))
		{
			/* Mark as forgotten - no longer known */
			if (j < 32)
			{
				spell_forgotten1 |= (1L << j);
				spell_learned1 &= ~(1L << j);
				which = use_realm1;
			}
			else
			{
				spell_forgotten2 |= (1L << (j - 32));
				spell_learned2 &= ~(1L << (j - 32));
				which = use_realm2;
			}

			/* Message */
#ifdef JP
			msg_format("%sの%sを忘れてしまった。",
				   spell_names[which][j%32], p );
#else
			msg_format("You have forgotten the %s of %s.", p,
				   spell_names[which][j%32]);
#endif


			/* One more can be learned */
			p_ptr->new_spells++;
		}
	}


	/* Check for spells to remember */
	for (i = 0; i < 64; i++)
	{
		/* None left to remember */
		if (p_ptr->new_spells <= 0) break;

		/* Efficiency -- all done */
		if (!spell_forgotten1 && !spell_forgotten2) break;

		/* Get the next spell we learned */
		j = spell_order[i];

		/* Skip unknown spells */
		if (j >= 99) break;

		/* Access the spell */
		if (j < 32)
			s_ptr = &mp_ptr->info[use_realm1][j];
		else
			s_ptr = &mp_ptr->info[use_realm2][j % 32];

		/* Skip spells we cannot remember */
		if (s_ptr->slevel > p_ptr->lev) continue;

		/* First set of spells */
		if ((j < 32) ?
		    (spell_forgotten1 & (1L << j)) :
		    (spell_forgotten2 & (1L << (j - 32))))
		{
			/* No longer forgotten - known once more */
			if (j < 32)
			{
				spell_forgotten1 &= ~(1L << j);
				spell_learned1 |= (1L << j);
				which = use_realm1;
			}
			else
			{
				spell_forgotten2 &= ~(1L << (j - 32));
				spell_learned2 |= (1L << (j - 32));
				which = use_realm2;
			}

			/* Message */
#ifdef JP
			msg_format("%sの%sを思い出した。",
				   spell_names[which][j%32], p );
#else
			msg_format("You have remembered the %s of %s.",
				   p, spell_names[which][j%32]);
#endif


			/* One less can be learned */
			p_ptr->new_spells--;
		}
	}


	/* Assume no spells available */
	k = 0;

	/* Count spells that can be learned */
	for (j = 0; j < (p_ptr->realm2 != REALM_NONE ? 64 : 32); j++)
	{
		/* Skip out of spellbooks */
		if (j % 32 >= MAX_SPELLS) continue;

		/* Access the spell */
		if (j < 32)
			s_ptr = &mp_ptr->info[use_realm1][j];
		else
			s_ptr = &mp_ptr->info[use_realm2][j % 32];

		/* Skip spells we cannot remember */
		if (s_ptr->slevel > p_ptr->lev) continue;

		/* Skip spells we already know */
		if ((j < 32) ?
		    (spell_learned1 & (1L << j)) :
		    (spell_learned2 & (1L << (j - 32))))
		{
			continue;
		}

		/* Count it */
		k++;
	}

	/* Cannot learn more spells than exist */
	if (p_ptr->new_spells > k) p_ptr->new_spells = k;

	/* Spell count changed */
	if (p_ptr->old_spells != p_ptr->new_spells)
	{
		/* Message if needed */
		if (p_ptr->new_spells)
		{
			/* Message */
#ifdef JP
			if( p_ptr->new_spells < 10 ){
				msg_format("あと %d つの%sを学べる。", p_ptr->new_spells, p);
			}else{
				msg_format("あと %d 個の%sを学べる。", p_ptr->new_spells, p);
			}
#else
			msg_format("You can learn %d more %s%s.",
				   p_ptr->new_spells, p,
				   (p_ptr->new_spells != 1) ? "s" : "");
#endif

		}

		/* Save the new_spells value */
		p_ptr->old_spells = p_ptr->new_spells;

		/* Redraw Study Status */
		p_ptr->redraw |= (PR_STUDY);
	}
}


/*
 * Calculate maximum mana.  You do not need to know any spells.
 * Note that mana is lowered by heavy (or inappropriate) armor.
 *
 * This function induces status messages.
 */
static void calc_mana(void)
{
	int		msp, levels, cur_wgt, max_wgt;

	object_type	*o_ptr;


	/* Hack -- Must be literate */
	if (!mp_ptr->spell_type) return;

	/* Extract "effective" player level */
	levels = (p_ptr->lev - mp_ptr->spell_first) + 1;

	/* Hack -- no negative mana */
	if (levels < 1)
	{
		msp = 0;
	}
	else
	{
		/* Extract total mana */
		msp = adj_mag_mana[p_ptr->stat_ind[mp_ptr->spell_stat]] * (levels + 3) / 3;

		/* Hack -- usually add 5 mana */
		if (msp) msp += 5;

		/* Hack: Dual classess have a -25% mana bonus */
		if (msp &&
			((p_ptr->pclass == CLASS_PALADIN) ||
			(p_ptr->pclass == CLASS_WARRIOR_MAGE)))
			msp -= msp / 4;

		/* Only mages are affected */
		if (mp_ptr->spell_type == ST_SPELL)
		{
			u32b f1, f2, f3;

			/* Assume player is not encumbered by gloves */
			p_ptr->cumber_glove = FALSE;

			/* Get the gloves */
			o_ptr = &inventory[INVEN_HANDS];

			/* Examine the gloves */
			object_flags(o_ptr, &f1, &f2, &f3);

			/* Normal gloves hurt mage-type spells */
			if (o_ptr->k_idx &&
				!(f2 & (TR2_FREE_ACT)) &&
				!(f1 & (TR1_MAGIC_MASTERY)) &&
				!((f1 & (TR1_DEX)) && (o_ptr->pval > 0)))
			{
				/* Encumbered */
				p_ptr->cumber_glove = TRUE;

				/* Reduce mana */
				msp = (3 * msp) / 4;
			}
		}


		/* Assume player not encumbered by armor */
		p_ptr->cumber_armor = FALSE;

		/* Weigh the armor */
		cur_wgt = 0;
		cur_wgt += inventory[INVEN_BODY].weight;
		cur_wgt += inventory[INVEN_HEAD].weight;
		if (!is_two_handed()) cur_wgt += inventory[INVEN_ARM].weight;
		cur_wgt += inventory[INVEN_OUTER].weight;
		cur_wgt += inventory[INVEN_HANDS].weight;
		cur_wgt += inventory[INVEN_FEET].weight;

		/* Determine the weight allowance */
		max_wgt = mp_ptr->spell_weight;

		/* Heavy armor penalizes mana by a percentage.  -LM- */
		if (((cur_wgt - max_wgt) / 10) > 0)
		{
			/* Encumbered */
			p_ptr->cumber_armor = TRUE;

			/* Subtract a percentage of maximum mana. */
			switch (p_ptr->pclass)
			{
				/* For these classes, mana is halved if armour
				 * is 30 pounds over their weight limit. */
				case CLASS_MAGE:
				{
					msp -= msp * (cur_wgt - max_wgt) / 600;
					break;
				}

				/* Mana halved if armour is 40 pounds over weight limit. */
				case CLASS_PRIEST:
				{
					msp -= msp * (cur_wgt - max_wgt) / 800;
					break;
				}

				/* Mana halved if armour is 60 pounds over weight limit. */
				case CLASS_PALADIN:
				case CLASS_WARRIOR_MAGE:
				{
					msp -= msp * (cur_wgt - max_wgt) / 1200;
					break;
				}
			}
		}
	}

	/* Mana can never be negative - paranoia - */
	if (msp < 0) msp = 0;

	/* Maximum mana has changed */
	if (p_ptr->msp != msp)
	{
		/* Enforce maximum */
		if (p_ptr->csp >= msp)
		{
			p_ptr->csp = msp;
			p_ptr->csp_frac = 0;
		}

#ifdef JP
		/* XTRA HACK LVUP */
		/* レベルアップの時は上昇量を表示する */
		if ((level_up == 1) && (msp > p_ptr->msp))
		{
			msg_format("最大マジック・ポイントが %d 増加した！",
				   (msp - p_ptr->msp));
		}
#endif
		/* Save new mana */
		p_ptr->msp = msp;

		/* Display mana later */
		p_ptr->redraw |= (PR_MANA);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER | PW_STATS);
		p_ptr->window |= (PW_SPELL);
	}


	/* Hack -- handle "xtra" mode */
	if (character_xtra) return;

	/* Take note when "glove state" changes */
	if (p_ptr->old_cumber_glove != p_ptr->cumber_glove)
	{
		/* Message */
		if (p_ptr->cumber_glove)
		{
#ifdef JP
			msg_print("手が覆われて呪文が唱えにくい感じがする。");
#else
			msg_print("Your covered hands feel unsuitable for spellcasting.");
#endif
		}
		else
		{
#ifdef JP
			msg_print("この手の状態なら、ぐっと呪文が唱えやすい感じだ。");
#else
			msg_print("Your hands feel more suitable for spellcasting.");
#endif
		}

		/* Save it */
		p_ptr->old_cumber_glove = p_ptr->cumber_glove;
	}


	/* Take note when "armor state" changes */
	if (p_ptr->old_cumber_armor != p_ptr->cumber_armor)
	{
		/* Message */
		if (p_ptr->cumber_armor)
		{
#ifdef JP
			msg_print("防具の重さで動きが鈍くなってしまっている。");
#else
			msg_print("The weight of your armor encumbers your movement.");
#endif
		}
		else
		{
#ifdef JP
			msg_print("ぐっと楽に体を動かせるようになった。");
#else
			msg_print("You feel able to move more freely.");
#endif
		}

		/* Save it */
		p_ptr->old_cumber_armor = p_ptr->cumber_armor;
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
	mhp = player_hp[p_ptr->lev - 1] + (bonus * p_ptr->lev / 4);

	/* Always have at least one hitpoint per level */
	if (mhp < p_ptr->lev + 1) mhp = p_ptr->lev + 1;

	/* Factor in the hero / superhero settings */
	if (p_ptr->hero) mhp += 10;
	if (p_ptr->shero) mhp += 30;

	/* New maximum hitpoints */
	if (p_ptr->mhp != mhp)
	{
		/* Enforce maximum */
		if (p_ptr->chp >= mhp)
		{
			p_ptr->chp = mhp;
			p_ptr->chp_frac = 0;
		}

#ifdef JP
/* XTRA HACK LVUP */
		/* レベルアップの時は上昇量を表示する */
		if ((level_up == 1) && (mhp > p_ptr->mhp))
		{
			msg_format("最大ヒット・ポイントが %d 増加した！",
				   (mhp - p_ptr->mhp) );
		}
#endif
		/* Save the new max-hitpoints */
		p_ptr->mhp = mhp;

		/* Display hitpoints (later) */
		p_ptr->redraw |= (PR_HP);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER | PW_STATS);
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
	int i;
	object_type *o_ptr;
	u32b f1, f2, f3;

	/* Assume no light */
	p_ptr->cur_lite = 0;

	/* Loop through all wielded items */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		o_ptr = &inventory[i];

		/* Examine actual lites */
		if ((i == INVEN_LITE) && (o_ptr->k_idx) && (o_ptr->tval == TV_LITE))
		{
			/* Torches (with fuel) provide some lite */
			if (o_ptr->sval == SV_LITE_TORCH)
			{
				if (o_ptr->xtra3 > 0) p_ptr->cur_lite += 1;
				continue;
			}

			/* Lanterns (with fuel) provide more lite */
			if (o_ptr->sval == SV_LITE_LANTERN)
			{
				if (o_ptr->xtra3 > 0) p_ptr->cur_lite += 2;
				continue;
			}

			/* Feanorian lamps provide more lite */
			if (o_ptr->sval == SV_LITE_FEANOR)
			{
				p_ptr->cur_lite += 2;
				continue;
			}

			/* Artifact Lites provide permanent, bright, lite */
			if (artifact_p(o_ptr))
			{
				p_ptr->cur_lite += 3;
				continue;
			}

			/* Provide permanent lite */
			p_ptr->cur_lite += 2;
		}
		else
		{
			/* Skip empty slots */
			if (!o_ptr->k_idx) continue;

			/* Extract the flags */
			object_flags(o_ptr, &f1, &f2, &f3);

			/* does this item glow? */
			if (f3 & TR3_LITE) p_ptr->cur_lite++;
		}

	}

	/* max radius is 14 without rewriting other code -- */
	/* see cave.c:update_lite() and defines.h:LITE_MAX */
	if (p_ptr->cur_lite > 14) p_ptr->cur_lite = 14;

	/* check if the player doesn't have a lite source, */
	/* but does glow as an intrinsic.                  */
	if (p_ptr->cur_lite == 0 && p_ptr->lite) p_ptr->cur_lite = 1;

	/* Varda gives you extra light */
	if (p_ptr->valar_patron == VAR_VARDA) p_ptr->cur_lite *= 3;

	/* end experimental mods */

	/* Reduce lite when running if requested */
	if (running && view_reduce_lite)
	{
		/* Reduce the lite radius if needed */
		if (p_ptr->cur_lite > 1) p_ptr->cur_lite = 1;
	}

	/* Notice changes in the "lite radius" */
	if (p_ptr->old_lite != p_ptr->cur_lite)
	{
		/* Update the lite */
		p_ptr->update |= (PU_LITE);

		/* Update the monsters */
		p_ptr->update |= (PU_MONSTERS);

		/* Remember the old lite */
		p_ptr->old_lite = p_ptr->cur_lite;
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

/* Calculate all class-based bonuses and penalties to melee Skill.  Oangband
 * recognizes that it takes a great deal of training to get critical hits with
 * a large, heavy weapon - training that many classes simply do not have the
 * time or inclination for.  -LM-
 */
static sint add_special_melee_skill(byte pclass, s16b weight, object_type *o_ptr)
{
	int add_skill = 0;

	switch (pclass)
	{
		/* Warrior.  Can use 15 lb weapons without penalty at level 1, and 45 lb weapons without penalty at 50th level. */
		case CLASS_WARRIOR:
		{
			add_skill = 25 + p_ptr->lev - (weight / 6);
			if (add_skill > 0) add_skill = 0;
			if (add_skill < -10) add_skill = -10;
			break;
		}

		/* Mage/High Mage.  Can use 6 lb weapons without penalty at level 1, and 16 lb weapons without penalty at 50th level. */
		case CLASS_MAGE:
		{
			add_skill = 20 + (2 * p_ptr->lev / 3) - (weight / 3);
			if (add_skill > 0) add_skill = 0;
			if (add_skill < -30) add_skill = -30;
			break;
		}

		/* Priest.  Can use 12 lb weapons without penalty at level 1, and 22 lb weapons without penalty at 50th level. */
		case CLASS_PRIEST:
		{
			add_skill = 30 + (1 * p_ptr->lev / 2) - (weight / 4);
			if (add_skill > 0) add_skill = 0;
			if (add_skill < -25) add_skill = -25;
			break;
		}

		/* Archer.  Can use 12 lb weapons without penalty at level 1, and 25 lb
		*weapons without penalty at 50th level. */
		case CLASS_ARCHER:
		{
			add_skill = 25 + (1 * p_ptr->lev / 2) - (weight / 5);
			if (add_skill > 0) add_skill = 0;
			if (add_skill < -20) add_skill = -20;
			break;
		}

		/* Paladin/Chaos warrior/Warrior mage.  Can use 15 lb weapons without
		* penalty at level 1, and 45 lb weapons without penalty at 50th level. */
		case CLASS_PALADIN:
		case CLASS_WARRIOR_MAGE:
		{
			add_skill = 25 + p_ptr->lev - (weight / 6);
			if (add_skill > 0) add_skill = 0;
			if (add_skill < -10) add_skill = -10;
			break;
		}
	}

	return (add_skill);
}


/*
 * Calculate the players current "state", taking into account
 * not only race/class intrinsics, but also objects being worn
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
void calc_bonuses(void)
{
	int             i, j, hold;
	int             old_speed;
	int             old_telepathy;
	int             old_see_inv;
	int             old_dis_ac;
	int             old_dis_to_a;
	bool            old_wraith_form;
	int             extra_blows;
	int             extra_shots;
	int				to_h = 0;
	int				dis_to_h = 0;
	int				to_d = 0;
	int				dis_to_d = 0;
	object_type     *o_ptr;
	u32b            f1, f2, f3;


	/* Save the old speed */
	old_speed = p_ptr->pspeed;

	/* Save the old vision stuff */
	old_telepathy = p_ptr->telepathy;
	old_see_inv = p_ptr->see_inv;

	/* Save the old armor class */
	old_dis_ac = p_ptr->dis_ac;
	old_dis_to_a = p_ptr->dis_to_a;

	/* Save the old wraith from */
	old_wraith_form = p_ptr->wraith_form;

	/* Clear extra blows/shots */
	extra_blows = extra_shots = 0;

	/* Clear the stat modifiers */
	for (i = 0; i < 6; i++) p_ptr->stat_add[i] = 0;


	/* Clear the Displayed/Real armor class */
	p_ptr->dis_ac = p_ptr->ac = 0;

	/* Clear the Displayed/Real Bonuses */
	p_ptr->dis_to_h = p_ptr->to_h = 0;
	p_ptr->dis_to_d = p_ptr->to_d = 0;
	p_ptr->dis_to_b = p_ptr->to_b = 0;
	p_ptr->dis_to_a = p_ptr->to_a = 0;

	/* Start with "normal" speed */
	p_ptr->pspeed = 110;

	/* Start with a single blow per turn */
	p_ptr->num_blow[0] = 1;
	p_ptr->num_blow[1] = 1;

	/* Start with a single shot per turn */
	p_ptr->num_fire = 100;

	/* Reset the "xtra" tval */
	p_ptr->tval_xtra = 0;

	/* Reset the "ammo" tval */
	p_ptr->tval_ammo = 0;

	/* Clear all the flags */
	p_ptr->aggravate = FALSE;
	p_ptr->teleport = FALSE;
	p_ptr->exp_drain = FALSE;
	p_ptr->bless_blade = FALSE;
	p_ptr->xtra_might = FALSE;
	p_ptr->impact = FALSE;
	p_ptr->pass_wall = FALSE;
	p_ptr->kill_wall = FALSE;
	p_ptr->dec_mana = FALSE;
	p_ptr->see_inv = FALSE;
	p_ptr->free_act = FALSE;
	p_ptr->slow_digest = FALSE;
	p_ptr->regenerate = FALSE;
	p_ptr->ffall = FALSE;
	p_ptr->hold_life = FALSE;
	p_ptr->wraith_form = FALSE;
	p_ptr->telepathy = FALSE;
	p_ptr->lite = FALSE;
	p_ptr->warning = FALSE;
	p_ptr->sustain_str = FALSE;
	p_ptr->sustain_int = FALSE;
	p_ptr->sustain_wis = FALSE;
	p_ptr->sustain_con = FALSE;
	p_ptr->sustain_dex = FALSE;
	p_ptr->sustain_chr = FALSE;
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
	p_ptr->sh_cold = FALSE;
	p_ptr->anti_magic = FALSE;
	p_ptr->anti_tele = FALSE;

	p_ptr->immune_acid = FALSE;
	p_ptr->immune_elec = FALSE;
	p_ptr->immune_fire = FALSE;
	p_ptr->immune_cold = FALSE;


	/* Base infravision (purely racial) */
	p_ptr->see_infra = rp_ptr->infra;


	/* Base skill -- disarming */
	p_ptr->skill_dis = rp_ptr->r_dis + cp_ptr->c_dis;

	/* Base skill -- magic devices */
	p_ptr->skill_dev = rp_ptr->r_dev + cp_ptr->c_dev;

	/* Base skill -- saving throw */
	p_ptr->skill_sav = rp_ptr->r_sav + cp_ptr->c_sav;

	/* Base skill -- stealth */
	p_ptr->skill_stl = rp_ptr->r_stl + cp_ptr->c_stl;

	/* Base skill -- searching ability */
	p_ptr->skill_srh = rp_ptr->r_srh + cp_ptr->c_srh;

	/* Base skill -- searching frequency */
	p_ptr->skill_fos = rp_ptr->r_fos + cp_ptr->c_fos;

	/* Base skill -- combat (normal) */
	p_ptr->skill_thn = rp_ptr->r_thn + cp_ptr->c_thn;

	/* Base skill -- combat (shooting) */
	p_ptr->skill_thb = rp_ptr->r_thb + cp_ptr->c_thb;

	/* Base skill -- combat (throwing) */
	p_ptr->skill_tht = rp_ptr->r_thb + cp_ptr->c_thb;

	/* Base skill -- digging */
	p_ptr->skill_dig = 0;

	/***** Classes *****/
	switch (p_ptr->pclass)
	{
		case CLASS_WARRIOR:
			if (p_ptr->lev > 29) p_ptr->resist_fear = TRUE;
			if (p_ptr->lev > 44) p_ptr->regenerate = TRUE;
			break;
		case CLASS_PALADIN:
			if (p_ptr->lev > 39) p_ptr->resist_fear = TRUE;
			break;
	}

	/***** Races ****/
	switch (p_ptr->prace)
	{
		case RACE_ELF:
			if (p_ptr->lev >= 20) p_ptr->see_inv = TRUE;
			break;
		case RACE_HOBBIT:
			p_ptr->sustain_dex = TRUE;
			break;
		case RACE_DWARF:
			p_ptr->resist_blind = TRUE;
			break;
		case RACE_HALF_ORC:
			p_ptr->resist_dark = TRUE;
			break;
		case RACE_DUNADAN:
			p_ptr->sustain_con = TRUE;
			p_ptr->regenerate = TRUE;  /* Dunadans heal fast... */
			break;
		case RACE_HIGH_ELF:
			p_ptr->see_inv = TRUE;
			break;
		case RACE_BARBARIAN:
			p_ptr->resist_fear = TRUE;
			break;
	}

	/***** Valar *****/
	switch (p_ptr->valar_patron)
	{
		case VAR_MANWE:
			p_ptr->resist_elec = TRUE;
			p_ptr->free_act = TRUE;
			if (p_ptr->lev >= 30) p_ptr->immune_elec = TRUE;
			break;
		case VAR_ULMO:
			p_ptr->resist_fire = TRUE;
			if (p_ptr->lev >= 25) p_ptr->resist_sound = TRUE;
			break;
		case VAR_AULE:
			/* Nothing */
			break;
		case VAR_OROME:
			p_ptr->pspeed += 1 + (p_ptr->lev / 7);
			break;
		case VAR_MANDOS:
			p_ptr->hold_life = TRUE;
			p_ptr->see_inv = TRUE;
			break;
		case VAR_IRMO:
			/* Nothing */
			break;
		case VAR_TULKAS:
			p_ptr->to_d += 1 + (p_ptr->lev / 5);
			p_ptr->dis_to_d += 1 + (p_ptr->lev / 5);
			break;
		case VAR_VARDA:
			p_ptr->lite = TRUE;
			break;
		case VAR_YAVANNA:
			p_ptr->resist_acid = TRUE;
			if (p_ptr->lev >= 30) p_ptr->reflect = TRUE;
			break;
		case VAR_NIENNA:
			p_ptr->warning = TRUE;
			if (p_ptr->lev >= 25) p_ptr->resist_conf = TRUE;
			break;
		case VAR_ESTE:
			p_ptr->sustain_con = TRUE;
			if (p_ptr->lev >= 20) p_ptr->resist_blind = TRUE;
			break;
		case VAR_VAIRE:
			p_ptr->resist_cold = TRUE;
			p_ptr->ac += 1 + (p_ptr->lev / 3);
			p_ptr->dis_ac += 1 + (p_ptr->lev / 3);
			break;
		case VAR_VANA:
			p_ptr->regenerate = TRUE;
			p_ptr->slow_digest = TRUE;
			break;
		case VAR_NESSA:
			p_ptr->skill_stl += 1 + (p_ptr->lev / 7);
			break;
	}

	/* Apply the racial modifiers */
	for (i = 0; i < 6; i++)
	{
		/* Modify the stats for "race" */
		p_ptr->stat_add[i] += (rp_ptr->r_adj[i] + cp_ptr->c_adj[i]);
	}


	/* I'm adding the mutations here for the lack of a better place... */
	if (p_ptr->muta)
	{
		/* Hyper Strength */
		if (p_ptr->muta & MUT_HYPER_STR)
		{
			p_ptr->stat_add[A_STR] += 4;
		}

		/* Puny */
		if (p_ptr->muta & MUT_PUNY)
		{
			p_ptr->stat_add[A_STR] -= 4;
		}

		/* Living computer */
		if (p_ptr->muta & MUT_HYPER_INT)
		{
			p_ptr->stat_add[A_INT] += 4;
			p_ptr->stat_add[A_WIS] += 4;
		}

		/* Moronic */
		if (p_ptr->muta & MUT_MORONIC)
		{
			p_ptr->stat_add[A_INT] -= 4;
			p_ptr->stat_add[A_WIS] -= 4;
		}

		if (p_ptr->muta & MUT_RESILIENT)
		{
			p_ptr->stat_add[A_CON] += 4;
		}

		if (p_ptr->muta & MUT_XTRA_FAT)
		{
			p_ptr->stat_add[A_CON] += 2;
			p_ptr->pspeed -= 2;
		}

		if (p_ptr->muta & MUT_ALBINO)
		{
			p_ptr->stat_add[A_CON] -= 4;
		}

		if (p_ptr->muta & MUT_FLESH_ROT)
		{
			p_ptr->stat_add[A_CON] -= 2;
			p_ptr->stat_add[A_CHR] -= 1;
			p_ptr->regenerate = FALSE;
			/* Cancel innate regeneration */
		}

		if (p_ptr->muta & MUT_SILLY_VOI)
		{
			p_ptr->stat_add[A_CHR] -= 4;
		}

		if (p_ptr->muta & MUT_XTRA_EYES)
		{
			p_ptr->skill_fos += 15;
			p_ptr->skill_srh += 15;
		}

		if (p_ptr->muta & MUT_XTRA_LEGS)
		{
			p_ptr->pspeed += 3;
		}

		if (p_ptr->muta & MUT_SHORT_LEG)
		{
			p_ptr->pspeed -= 3;
		}

		if (p_ptr->muta & MUT_ELEC_TOUC)
		{
			p_ptr->sh_elec = TRUE;
			p_ptr->resist_elec = TRUE;
		}

		if (p_ptr->muta & MUT_FIRE_BODY)
		{
			p_ptr->sh_fire = TRUE;
			p_ptr->resist_fire = TRUE;
			p_ptr->lite = TRUE;
		}

		if (p_ptr->muta & MUT_SCALES)
		{
			p_ptr->stat_add[A_CHR] -= 1;
			p_ptr->to_a += 10;
			p_ptr->dis_to_a += 10;
		}

		if (p_ptr->muta & MUT_IRON_SKIN)
		{
			p_ptr->stat_add[A_DEX] -= 1;
			p_ptr->to_a += 25;
			p_ptr->dis_to_a += 25;
		}

		if (p_ptr->muta & MUT_WINGS)
		{
			p_ptr->ffall = TRUE;
		}

		if (p_ptr->muta & MUT_FEARLESS)
		{
			p_ptr->resist_fear = TRUE;
		}

		if (p_ptr->muta & MUT_REGEN)
		{
			p_ptr->regenerate = TRUE;
		}

		if (p_ptr->muta & MUT_ESP)
		{
			p_ptr->telepathy = TRUE;
		}

		if (p_ptr->muta & MUT_LIMBER)
		{
			p_ptr->stat_add[A_DEX] += 3;
		}

		if (p_ptr->muta & MUT_ARTHRITIS)
		{
			p_ptr->stat_add[A_DEX] -= 3;
		}

		if (p_ptr->muta & MUT_MOTION)
		{
			p_ptr->free_act = TRUE;
			p_ptr->to_h += 5;
			p_ptr->dis_to_h +=5;
			p_ptr->to_b += 5;
			p_ptr->dis_to_b +=5;
			p_ptr->skill_stl += 1;
		}

		if (p_ptr->muta & MUT_ILL_NORM)
		{
			p_ptr->stat_add[A_CHR] = 0;
		}
	}

	/* Extra Might */
	if (p_ptr->tim_might)
	{
		p_ptr->stat_add[A_STR] += 4;
	}

	/* Scan the usable inventory */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Extract the item flags */
		object_flags(o_ptr, &f1, &f2, &f3);

		/* Affect stats */
		if (f1 & (TR1_STR)) p_ptr->stat_add[A_STR] += (s16b) o_ptr->pval;
		if (f1 & (TR1_INT)) p_ptr->stat_add[A_INT] += (s16b) o_ptr->pval;
		if (f1 & (TR1_WIS)) p_ptr->stat_add[A_WIS] += (s16b) o_ptr->pval;
		if (f1 & (TR1_DEX)) p_ptr->stat_add[A_DEX] += (s16b) o_ptr->pval;
		if (f1 & (TR1_CON)) p_ptr->stat_add[A_CON] += (s16b) o_ptr->pval;
		if (f1 & (TR1_CHR)) p_ptr->stat_add[A_CHR] += (s16b) o_ptr->pval;

		/* Affect magic devices */
		if (f1 & (TR1_MAGIC_MASTERY)) p_ptr->skill_dev += (o_ptr->pval * 8);

		/* Affect stealth */
		if (f1 & (TR1_STEALTH)) p_ptr->skill_stl += (s16b) o_ptr->pval;

		/* Affect searching ability (factor of five) */
		if (f1 & (TR1_SEARCH)) p_ptr->skill_srh += (o_ptr->pval * 5);

		/* Affect searching frequency (factor of five) */
		if (f1 & (TR1_SEARCH)) p_ptr->skill_fos += (o_ptr->pval * 5);

		/* Affect infravision */
		if (f1 & (TR1_INFRA)) p_ptr->see_infra += (s16b) o_ptr->pval;

		/* Affect digging (factor of 20) */
		if (f1 & (TR1_TUNNEL)) p_ptr->skill_dig += (o_ptr->pval * 20);

		/* Affect speed */
		if (f1 & (TR1_SPEED)) p_ptr->pspeed += (s16b) o_ptr->pval;

		/* Affect blows */
		if ((i != INVEN_WIELD) &&
			!((i == INVEN_ARM) && is_two_handed()))
			if (f1 & (TR1_BLOWS)) extra_blows += o_ptr->pval;

		/* Hack -- cause earthquakes */
		if (f1 & (TR1_IMPACT)) p_ptr->impact = TRUE;

		/* Boost shots */
		if (f3 & (TR3_XTRA_SHOTS)) extra_shots++;

		/* Various flags */
		if (f3 & (TR3_AGGRAVATE))   p_ptr->aggravate = TRUE;
		if (f3 & (TR3_TELEPORT))    p_ptr->teleport = TRUE;
		if (f3 & (TR3_DRAIN_EXP))   p_ptr->exp_drain = TRUE;
		if (f3 & (TR3_BLESSED))     p_ptr->bless_blade = TRUE;
		if (f3 & (TR3_XTRA_MIGHT))  p_ptr->xtra_might = TRUE;
		if (f3 & (TR3_SLOW_DIGEST)) p_ptr->slow_digest = TRUE;
		if (f3 & (TR3_REGEN))       p_ptr->regenerate = TRUE;
		if (f3 & (TR3_TELEPATHY))   p_ptr->telepathy = TRUE;
		if (f3 & (TR3_LITE))        p_ptr->lite = TRUE;
		if (f3 & (TR3_SEE_INVIS))   p_ptr->see_inv = TRUE;
		if (f3 & (TR3_FEATHER))     p_ptr->ffall = TRUE;
		if (f2 & (TR2_FREE_ACT))    p_ptr->free_act = TRUE;
		if (f2 & (TR2_HOLD_LIFE))   p_ptr->hold_life = TRUE;
		if (f3 & (TR3_WRAITH))      p_ptr->wraith_form = TRUE;
		if (f3 & (TR3_WARNING))
			if (!o_ptr->inscription || !(my_strchr(quark_str(o_ptr->inscription), '$')))
			{
			     p_ptr->warning = TRUE;
			}
		if (f3 & (TR3_DEC_MANA))    p_ptr->dec_mana = TRUE;

		/* Immunity flags */
		if (f2 & (TR2_IM_FIRE)) p_ptr->immune_fire = TRUE;
		if (f2 & (TR2_IM_ACID)) p_ptr->immune_acid = TRUE;
		if (f2 & (TR2_IM_COLD)) p_ptr->immune_cold = TRUE;
		if (f2 & (TR2_IM_ELEC)) p_ptr->immune_elec = TRUE;

		/* Resistance flags */
		if (f2 & (TR2_RES_ACID))   p_ptr->resist_acid = TRUE;
		if (f2 & (TR2_RES_ELEC))   p_ptr->resist_elec = TRUE;
		if (f2 & (TR2_RES_FIRE))   p_ptr->resist_fire = TRUE;
		if (f2 & (TR2_RES_COLD))   p_ptr->resist_cold = TRUE;
		if (f2 & (TR2_RES_POIS))   p_ptr->resist_pois = TRUE;
		if (f2 & (TR2_RES_FEAR))   p_ptr->resist_fear = TRUE;
		if (f2 & (TR2_RES_CONF))   p_ptr->resist_conf = TRUE;
		if (f2 & (TR2_RES_SOUND))  p_ptr->resist_sound = TRUE;
		if (f2 & (TR2_RES_LITE))   p_ptr->resist_lite = TRUE;
		if (f2 & (TR2_RES_DARK))   p_ptr->resist_dark = TRUE;
		if (f2 & (TR2_RES_CHAOS))  p_ptr->resist_chaos = TRUE;
		if (f2 & (TR2_RES_DISEN))  p_ptr->resist_disen = TRUE;
		if (f2 & (TR2_RES_SHARDS)) p_ptr->resist_shard = TRUE;
		if (f2 & (TR2_RES_NEXUS))  p_ptr->resist_nexus = TRUE;
		if (f2 & (TR2_RES_BLIND))  p_ptr->resist_blind = TRUE;
		if (f2 & (TR2_RES_NETHER)) p_ptr->resist_neth = TRUE;

		if (f2 & (TR2_REFLECT))  p_ptr->reflect = TRUE;
		if (f3 & (TR3_SH_FIRE))  p_ptr->sh_fire = TRUE;
		if (f3 & (TR3_SH_ELEC))  p_ptr->sh_elec = TRUE;
		if (f3 & (TR3_SH_COLD))  p_ptr->sh_cold = TRUE;
		if (f3 & (TR3_NO_MAGIC)) p_ptr->anti_magic = TRUE;
		if (f3 & (TR3_NO_TELE))  p_ptr->anti_tele = TRUE;

		/* Sustain flags */
		if (f2 & (TR2_SUST_STR)) p_ptr->sustain_str = TRUE;
		if (f2 & (TR2_SUST_INT)) p_ptr->sustain_int = TRUE;
		if (f2 & (TR2_SUST_WIS)) p_ptr->sustain_wis = TRUE;
		if (f2 & (TR2_SUST_DEX)) p_ptr->sustain_dex = TRUE;
		if (f2 & (TR2_SUST_CON)) p_ptr->sustain_con = TRUE;
		if (f2 & (TR2_SUST_CHR)) p_ptr->sustain_chr = TRUE;

		/* Modify the base armor class */
		p_ptr->ac += o_ptr->ac;

		/* The base armor class is always known */
		p_ptr->dis_ac += o_ptr->ac;

		/* Apply the bonuses to armor class */
		p_ptr->to_a += o_ptr->to_a;

		/* Apply the mental bonuses to armor class, if known */
		if (object_known_p(o_ptr)) p_ptr->dis_to_a += o_ptr->to_a;

		/* Hack -- do not apply "weapon" bonuses */
		if (i == INVEN_WIELD) continue;
		if ((i == INVEN_ARM) && is_two_handed()) continue;

		/* Hack -- do not apply "bow" bonuses */
		if (i == INVEN_BOW) continue;

		/* Apply the bonuses to hit/damage */
		to_h += o_ptr->to_h;
		to_d += o_ptr->to_d;

		/* Apply the mental bonuses tp hit/damage, if known */
		if (object_known_p(o_ptr)) dis_to_h += o_ptr->to_h;
		if (object_known_p(o_ptr)) dis_to_d += o_ptr->to_d;
	}

	/* Hack -- aura of fire also provides light */
	if (p_ptr->sh_fire) p_ptr->lite = TRUE;

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
			p_ptr->window |= (PW_PLAYER | PW_STATS);
		}


		/* Extract the new "stat_use" value for the stat */
		use = modify_stat_value(p_ptr->stat_cur[i], p_ptr->stat_add[i]);

		if ((i == A_CHR) && (p_ptr->muta & MUT_ILL_NORM))
		{
			/* 10 to 18/90 charisma, guaranteed, based on level */
			if (use < 8 + 2 * p_ptr->lev)
			{
				use = 8 + 2 * p_ptr->lev;
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
			p_ptr->window |= (PW_PLAYER | PW_STATS);
		}


		/* Values: 3, 4, ..., 17 */
		if (use <= 18) ind = (use - 3);

		/* Ranges: 18/00-18/09, ..., 18/210-18/219 */
		else if (use <= 18+219) ind = (15 + (use - 18) / 10);

		/* Range: 18/220+ */
		else ind = (37);

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
				if (mp_ptr->spell_stat == A_INT)
				{
					p_ptr->update |= (PU_MANA | PU_SPELLS);
				}
			}

			/* Change in WIS may affect Mana/Spells */
			else if (i == A_WIS)
			{
				if (mp_ptr->spell_stat == A_WIS)
				{
					p_ptr->update |= (PU_MANA | PU_SPELLS);
				}
			}

			/* Window stuff */
			p_ptr->window |= (PW_PLAYER | PW_STATS);
		}
	}


	/* Apply temporary "stun" */
	if (p_ptr->stun > 50)
	{
		p_ptr->to_h -= 20;
		p_ptr->dis_to_h -= 20;
		p_ptr->to_b -= 20;
		p_ptr->dis_to_b -= 20;
		p_ptr->to_d -= 20;
		p_ptr->dis_to_d -= 20;
	}
	else if (p_ptr->stun)
	{
		p_ptr->to_h -= 5;
		p_ptr->dis_to_h -= 5;
		p_ptr->to_b -= 5;
		p_ptr->dis_to_b -= 5;
		p_ptr->to_d -= 5;
		p_ptr->dis_to_d -= 5;
	}

	/* Invulnerability */
	if (p_ptr->invuln)
	{
		p_ptr->to_a += 100;
		p_ptr->dis_to_a += 100;
	}

	/* wraith_form */
	if (p_ptr->tim_wraith)
	{
		p_ptr->wraith_form = TRUE;
	}

	if (p_ptr->wraith_form)
	{
		p_ptr->reflect = TRUE;
	}

	/* Temporary blessing */
	if (p_ptr->blessed)
	{
		p_ptr->to_a += 5;
		p_ptr->dis_to_a += 5;
		p_ptr->to_h += 10;
		p_ptr->dis_to_h += 10;
		p_ptr->to_b += 10;
		p_ptr->dis_to_b += 10;
	}

	/* Temporary shield */
	if (p_ptr->shield || p_ptr->magicdef ||
	    p_ptr->wraith_form || p_ptr->musou)
	{
		p_ptr->to_a += 50;
		p_ptr->dis_to_a += 50;
	}

	/* Temporary magic defence */
	if (p_ptr->magicdef)
	{
		p_ptr->resist_blind = TRUE;
		p_ptr->resist_conf = TRUE;
		p_ptr->reflect = TRUE;
		p_ptr->free_act = TRUE;
		p_ptr->ffall = TRUE;
	}

	/* Musou */
	if (p_ptr->musou)
	{
		p_ptr->see_inv = TRUE;
		p_ptr->free_act = TRUE;
		p_ptr->slow_digest = TRUE;
		p_ptr->regenerate = TRUE;
		p_ptr->ffall = TRUE;
		p_ptr->hold_life = TRUE;
		p_ptr->telepathy = TRUE;
		p_ptr->lite = TRUE;
		p_ptr->sustain_str = TRUE;
		p_ptr->sustain_int = TRUE;
		p_ptr->sustain_wis = TRUE;
		p_ptr->sustain_con = TRUE;
		p_ptr->sustain_dex = TRUE;
		p_ptr->sustain_chr = TRUE;
		p_ptr->resist_acid = TRUE;
		p_ptr->resist_elec = TRUE;
		p_ptr->resist_fire = TRUE;
		p_ptr->resist_cold = TRUE;
		p_ptr->resist_pois = TRUE;
		p_ptr->resist_conf = TRUE;
		p_ptr->resist_sound = TRUE;
		p_ptr->resist_lite = TRUE;
		p_ptr->resist_dark = TRUE;
		p_ptr->resist_chaos = TRUE;
		p_ptr->resist_disen = TRUE;
		p_ptr->resist_shard = TRUE;
		p_ptr->resist_nexus = TRUE;
		p_ptr->resist_blind = TRUE;
		p_ptr->resist_neth = TRUE;
		p_ptr->resist_fear = TRUE;
		p_ptr->reflect = TRUE;
		p_ptr->sh_fire = TRUE;
		p_ptr->sh_elec = TRUE;
		p_ptr->sh_cold = TRUE;
	}

	/* Temporary "Hero" */
	if (p_ptr->hero)
	{
		p_ptr->to_h += 12;
		p_ptr->dis_to_h += 12;
		p_ptr->to_b += 12;
		p_ptr->dis_to_b += 12;
	}

	/* Temporary "Beserk" */
	if (p_ptr->shero)
	{
		p_ptr->to_h += 24;
		p_ptr->dis_to_h += 24;
		p_ptr->to_h -= 10;
		p_ptr->dis_to_h -= 10;
		p_ptr->to_a -= 10;
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

	/* Temporary "telepathy" */
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

	/* Temporary regeneration */
	if (p_ptr->tim_regen)
	{
		p_ptr->regenerate = TRUE;
	}

	/* Temporary fire aura */
	if (p_ptr->tim_sh_fire)
	{
		p_ptr->sh_fire = TRUE;
	}

	/* Temporary elec aura */
	if (p_ptr->tim_sh_elec)
	{
		p_ptr->sh_elec = TRUE;
	}

	/* Temporary cold aura */
	if (p_ptr->tim_sh_cold)
	{
		p_ptr->sh_cold = TRUE;
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
	j = p_ptr->total_weight;

	/* Extract the "weight limit" (in tenth pounds) */
	i = weight_limit();

	/* XXX XXX XXX Apply "encumbrance" from weight */
	if (j > i/2) p_ptr->pspeed -= ((j - (i/2)) / (i / 10));

	/* Bloating slows the player down (a little) */
	if (p_ptr->food >= PY_FOOD_MAX) p_ptr->pspeed -= 10;

	/* Searching slows the player down */
	if (p_ptr->searching) p_ptr->pspeed -= 10;

	/* Display the speed (if needed) */
	if (p_ptr->pspeed != old_speed) p_ptr->redraw |= (PR_SPEED);


	/* Actual Modifier Bonuses (Un-inflate stat bonuses) */
	p_ptr->to_a += ((int)(adj_dex_ta[p_ptr->stat_ind[A_DEX]]) - 128);
	to_d += ((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128);
	to_h += ((int)(adj_dex_th[p_ptr->stat_ind[A_DEX]]) - 128);

	/* Displayed Modifier Bonuses (Un-inflate stat bonuses) */
	p_ptr->dis_to_a += ((int)(adj_dex_ta[p_ptr->stat_ind[A_DEX]]) - 128);
	dis_to_d += ((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128);
	dis_to_h += ((int)(adj_dex_th[p_ptr->stat_ind[A_DEX]]) - 128);

	/* Redraw armor (if needed) */
	if ((p_ptr->dis_ac != old_dis_ac) || (p_ptr->dis_to_a != old_dis_to_a))
	{
		/* Redraw */
		p_ptr->redraw |= (PR_ARMOR);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER | PW_STATS);
	}


	/* Obtain the "hold" value */
	hold = adj_str_hold[p_ptr->stat_ind[A_STR]];


	/* Examine the "current bow" */
	o_ptr = &inventory[INVEN_BOW];


	/* Assume not heavy */
	p_ptr->heavy_shoot = FALSE;

	/* It is hard to carry a heavy bow */
	if (hold < o_ptr->weight / 10)
	{
		/* Hard to wield a heavy bow */
		p_ptr->to_b += 2 * (hold - o_ptr->weight / 10);
		p_ptr->dis_to_b += 2 * (hold - o_ptr->weight / 10);

		/* Heavy Bow */
		p_ptr->heavy_shoot = TRUE;
	}


	/* Compute "extra shots" if needed */
	if (o_ptr->k_idx)
	{
		/* Analyze the launcher */
		switch (o_ptr->sval)
		{
			case SV_SLING:
			{
				p_ptr->tval_ammo = TV_SHOT;
				break;
			}

			case SV_SHORT_BOW:
			case SV_LONG_BOW:
			{
				p_ptr->tval_ammo = TV_ARROW;
				break;
			}

			case SV_LIGHT_XBOW:
			case SV_HEAVY_XBOW:
			{
				p_ptr->tval_ammo = TV_BOLT;
				break;
			}
		}

		/* Apply special flags */
		if (o_ptr->k_idx && !p_ptr->heavy_shoot)
		{
			/* Extra shots */
			p_ptr->num_fire += (extra_shots * 100);

			/* Warriors are good at crossbow shooting */
			if ((p_ptr->pclass == CLASS_WARRIOR) &&
			   (p_ptr->tval_ammo == TV_BOLT))
			{
				p_ptr->num_fire += (p_ptr->lev * 2);
			}

			/* Warrior-mages are also good at shooting as Rangers */
			if ((p_ptr->pclass == CLASS_WARRIOR_MAGE) &&
				((p_ptr->tval_ammo == TV_SHOT) || (p_ptr->tval_ammo == TV_ARROW)))
			{
				p_ptr->num_fire += (p_ptr->lev * 2);
			}

			/* Hack -- Archers love shooting */
			if (p_ptr->pclass == CLASS_ARCHER)
			{
				if (p_ptr->tval_ammo == TV_ARROW) p_ptr->num_fire += (p_ptr->lev * 4);
				else if (p_ptr->tval_ammo == TV_BOLT)
					p_ptr->num_fire += (p_ptr->lev * 3);
				else if (p_ptr->tval_ammo == TV_SHOT)
					p_ptr->num_fire += (p_ptr->lev * 2);
			}
		}
	}

	/* Final shooting to-hit bonus (Dex and armors) */
	p_ptr->to_b += to_h;
	p_ptr->dis_to_b += dis_to_h;

	/* Assume not heavy */
	p_ptr->heavy_wield = FALSE;

	for (i = 0; i < 2; i++)
	{
		if ((i == 1) && !is_two_handed()) break;

		/* Examine the "main weapon" */
		o_ptr = &inventory[INVEN_WIELD + i];

		/* It is hard to hold a heavy weapon */
		if (hold < o_ptr->weight / 10)
		{
			/* Hard to wield a heavy weapon */
			p_ptr->to_h += 2 * (hold - o_ptr->weight / 10);
			p_ptr->dis_to_h += 2 * (hold - o_ptr->weight / 10);

			/* Heavy weapon */
			p_ptr->heavy_wield = TRUE;

			/* The player gets to swing a heavy weapon only once. -LM- */
			p_ptr->num_blow[i] = 1;
		}

		/* Normal weapons */
		if (o_ptr->k_idx && !p_ptr->heavy_wield)
		{
			int str_index, dex_index;
			int num, wgt, mul, div;

			/* Extract the item flags */
			object_flags(o_ptr, &f1, &f2, &f3);

			num = cp_ptr->num; wgt = cp_ptr->wgt; mul = cp_ptr->mul;

			/* Enforce a minimum "weight" (tenth pounds) */
			div = ((o_ptr->weight < wgt) ? wgt : o_ptr->weight);

			/* Access the strength vs weight */
			str_index = ((adj_str_blow[p_ptr->stat_ind[A_STR]] * mul / 10) / div);

			/* When doing two handed combat, you needs more strength. */
			if (is_two_handed()) str_index = str_index * 3 / 4;

			/* Maximal value */
			if (str_index > 11) str_index = 11;

			/* Index by dexterity */
			dex_index = (adj_dex_blow[p_ptr->stat_ind[A_DEX]]);

			/* Maximal value */
			if (dex_index > 11) dex_index = 11;

			/* Use the blows table */
			p_ptr->num_blow[i] = blows_table[str_index][dex_index];

			/* Maximal value */
			if (p_ptr->num_blow[i] > num) p_ptr->num_blow[i] = num;

			/* Warrior gets one extra attack */
			if (p_ptr->pclass == CLASS_WARRIOR) p_ptr->num_blow[i]++;

			/* Add in the "bonus blows" */
			p_ptr->num_blow[i] += extra_blows;
			if (f1 & (TR1_BLOWS)) p_ptr->num_blow[i] += (s16b)(o_ptr->pval);

			/* Require at least one blow */
			if (p_ptr->num_blow[i] < 1) p_ptr->num_blow[i] = 1;

			/* Boost digging skill by weapon weight */
			p_ptr->skill_dig += (o_ptr->weight / 10);
		}

		/* If not wielding a weapon. */
		else if (!o_ptr->k_idx)
		{
			switch(p_ptr->pclass)
			{
				case CLASS_WARRIOR:
					p_ptr->num_blow[i] = 2;
					break;
				default:
					p_ptr->num_blow[i] = 1;
			}
		}
	}

	/* Add all other class-specific adjustments to melee Skill. -LM- */
	p_ptr->skill_thn += add_special_melee_skill(p_ptr->pclass, o_ptr->weight, o_ptr);

	/* Assume okay */
	p_ptr->icky_wield = FALSE;
#ifndef TINYANGBAND
	/* Extra bonus for warriors... */
	if (p_ptr->pclass == CLASS_WARRIOR)
	{
		to_h += (p_ptr->lev / 5);
		to_d += (p_ptr->lev / 5);

		dis_to_h += (p_ptr->lev / 5);
		dis_to_d += (p_ptr->lev / 5);
	}
#endif
	/* Priest weapon penalty for non-blessed edged weapons */
	if (p_ptr->pclass == CLASS_PRIEST)
	{
		for (i = 0; i < 2; i++)
		{
			o_ptr = &inventory[INVEN_WIELD + i];
			object_flags(o_ptr, &f1, &f2, &f3);

			if (!(f3 & (TR3_BLESSED)) &&
				 ((o_ptr->tval == TV_SWORD) || (o_ptr->tval == TV_POLEARM)))
			{
				/* Reduce the real bonuses */
				p_ptr->to_h -= 2;
				p_ptr->to_d -= 2;

				/* Reduce the mental bonuses */
				p_ptr->dis_to_h -= 2;
				p_ptr->dis_to_d -= 2;

				/* Icky weapon */
				p_ptr->icky_wield = TRUE;
			}
		}
	}

	/* Add all to_hit and to_dam bonuses. */
	if (is_two_handed())
	{
		to_d /= 2;
		to_h /= 2;
		dis_to_d /= 2;
		dis_to_h /= 2;

		/* When doing two handed combat, get penalty to to-hit. */
		to_h -= 10;
		dis_to_h -= 10;
	}

	p_ptr->to_d += to_d;
	p_ptr->to_h += to_h;
	p_ptr->dis_to_d += dis_to_d;
	p_ptr->dis_to_h += dis_to_h;

	/* Specified weapons give extra bonuses when doing two handed combat. */
	if (is_two_handed())
	{
		object_type *o1_ptr = &inventory[INVEN_WIELD];
		object_type *o2_ptr = &inventory[INVEN_ARM];

		if ((o2_ptr->tval == TV_SWORD) && (o2_ptr->sval == SV_MAIN_GAUCHE))
		{
			p_ptr->to_h += 5;
			p_ptr->dis_to_h += 5;
			p_ptr->to_a += 5;
			p_ptr->dis_to_a += 5;
		}

		p_ptr->redraw |= (PR_ARMOR | PR_SPEED);
	}

	/* Penalty by curses */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Extract the item flags */
		object_flags(o_ptr, &f1, &f2, &f3);

		if ((f3 & TR3_CURSED) || (f3 & TR3_HEAVY_CURSE) ||
			(f3 & TR3_PERMA_CURSE))
		{
			switch(i)
			{
			case INVEN_WIELD:
			case INVEN_RIGHT:
			case INVEN_LEFT:
				p_ptr->to_h -= 5;
				p_ptr->dis_to_h -= 5;
				break;
			case INVEN_ARM:
				if (is_two_handed())
				{
					p_ptr->to_h -= 5;
					p_ptr->dis_to_h -= 5;
				}
				else
				{
					p_ptr->to_a -= 5;
					p_ptr->dis_to_a -= 5;
				}
				break;
			case INVEN_BOW:
				p_ptr->to_b -= 5;
				p_ptr->dis_to_b -= 5;
				break;
			default:
				p_ptr->to_a -= 5;
				p_ptr->dis_to_a -= 5;
			}
		}
	}

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

	/* Affect Skill -- disarming (Level, by Class) */
	p_ptr->skill_dis += (cp_ptr->x_dis * p_ptr->lev / 10);

	/* Affect Skill -- magic devices (Level, by Class) */
	p_ptr->skill_dev += (cp_ptr->x_dev * p_ptr->lev / 10);

	/* Affect Skill -- saving throw (Level, by Class) */
	p_ptr->skill_sav += (cp_ptr->x_sav * p_ptr->lev / 10);

	/* Affect Skill -- stealth (Level, by Class) */
	p_ptr->skill_stl += (cp_ptr->x_stl * p_ptr->lev / 10);

	/* Affect Skill -- search ability (Level, by Class) */
	p_ptr->skill_srh += (cp_ptr->x_srh * p_ptr->lev / 10);

	/* Affect Skill -- search frequency (Level, by Class) */
	p_ptr->skill_fos += (cp_ptr->x_fos * p_ptr->lev / 10);

	/* Affect Skill -- combat (normal) (Level, by Class) */
	p_ptr->skill_thn += (cp_ptr->x_thn * p_ptr->lev / 50);

	/* Affect Skill -- combat (shooting) (Level, by Class) */
	p_ptr->skill_thb += (cp_ptr->x_thb * p_ptr->lev / 50);

	/* Affect Skill -- combat (throwing) (Level, by Class) */
	p_ptr->skill_tht += (cp_ptr->x_thb * p_ptr->lev / 50);


	/* Limit Skill -- stealth from 0 to 30 */
	if (p_ptr->skill_stl > 30) p_ptr->skill_stl = 30;
	if (p_ptr->skill_stl < 0) p_ptr->skill_stl = 0;

	/* Limit Skill -- digging from 1 up */
	if (p_ptr->skill_dig < 1) p_ptr->skill_dig = 1;

	if ((p_ptr->anti_magic || p_ptr->magicdef || p_ptr->musou) && (p_ptr->skill_sav < 95)) p_ptr->skill_sav = 95;

	/* Hack -- handle "xtra" mode */
	if (character_xtra) return;

	/* wraith_form */
	if (old_wraith_form != p_ptr->wraith_form)
	{
		notice_wraith_form(p_ptr->wraith_form);
	}

	/* Take note when "heavy bow" changes */
	if (p_ptr->old_heavy_shoot != p_ptr->heavy_shoot)
	{
		/* Message */
		if (p_ptr->heavy_shoot)
		{
#ifdef JP
			msg_print("こんな重い弓を装備しているのは大変だ。");
#else
			msg_print("You have trouble wielding such a heavy bow.");
#endif

		}
		else if (inventory[INVEN_BOW].k_idx)
		{
#ifdef JP
			msg_print("この弓なら装備していても辛くない。");
#else
			msg_print("You have no trouble wielding your bow.");
#endif

		}
		else
		{
#ifdef JP
			msg_print("重い弓を装備からはずして体が楽になった。");
#else
			msg_print("You feel relieved to put down your heavy bow.");
#endif

		}

		/* Save it */
		p_ptr->old_heavy_shoot = p_ptr->heavy_shoot;
	}


	/* Take note when "heavy weapon" changes */
	if (p_ptr->old_heavy_wield != p_ptr->heavy_wield)
	{
		/* Message */
		if (p_ptr->heavy_wield)
		{
#ifdef JP
			msg_print("こんな重い武器を装備しているのは大変だ。");
#else
			msg_print("You have trouble wielding such a heavy weapon.");
#endif

		}
		else if (inventory[INVEN_WIELD].k_idx)
		{
#ifdef JP
			msg_print("この武器なら装備していても辛くない。");
#else
			msg_print("You have no trouble wielding your weapon.");
#endif

		}
		else
		{
#ifdef JP
			msg_print("重い武器を装備からはずして体が楽になった。");
#else
			msg_print("You feel relieved to put down your heavy weapon.");
#endif

		}

		/* Save it */
		p_ptr->old_heavy_wield = p_ptr->heavy_wield;
	}


	/* Take note when "illegal weapon" changes */
	if (p_ptr->old_icky_wield != p_ptr->icky_wield)
	{
		/* Message */
		if (p_ptr->icky_wield)
		{
#ifdef JP
			msg_print("今の武器はどうも自分にふさわしくない気がする。");
#else
			msg_print("You do not feel comfortable with your weapon.");
#endif
		}
		else if (inventory[INVEN_WIELD].k_idx)
		{
#ifdef JP
			msg_print("今の武器は自分にふさわしい気がする。");
#else
			msg_print("You feel comfortable with your weapon.");
#endif
		}
		else
		{
#ifdef JP
			msg_print("武器を装備からはずしたら随分と気が楽になった。");
#else
			msg_print("You feel more comfortable after removing your weapon.");
#endif
		}

		/* Save it */
		p_ptr->old_icky_wield = p_ptr->icky_wield;
	}

	p_ptr->align = friend_align;
}



/*
 * Handle "p_ptr->notice"
 */
void notice_stuff(void)
{
	/* Notice stuff */
	if (!p_ptr->notice) return;


	/* Actually do auto-destroy */
	if (p_ptr->notice & (PN_AUTODESTROY))
	{
		p_ptr->notice &= ~(PN_AUTODESTROY);
		autopick_delayed_alter();
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
 * Handle "p_ptr->update"
 */
void update_stuff(void)
{
	/* Update stuff */
	if (!p_ptr->update) return;


	if (p_ptr->update & (PU_BONUS))
	{
		p_ptr->update &= ~(PU_BONUS);
		calc_bonuses();
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


	/* Character is not ready yet, no screen updates */
	if (!character_generated) return;


	/* Character is in "icky" mode, no screen updates */
	if (character_icky) return;


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


	if (p_ptr->update & (PU_FLOW))
	{
		p_ptr->update &= ~(PU_FLOW);
		update_flow();
	}

	if (p_ptr->update & (PU_DISTANCE))
	{
		p_ptr->update &= ~(PU_DISTANCE);

		/* Still need to call update_monsters(FALSE) after update_mon_lite() */ 
		/* p_ptr->update &= ~(PU_MONSTERS); */

		update_monsters(TRUE);
	}

	if (p_ptr->update & (PU_MON_LITE))
	{
		p_ptr->update &= ~(PU_MON_LITE);
		update_mon_lite();
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
	if (character_icky) return;



	/* Hack -- clear the screen */
	if (p_ptr->redraw & (PR_WIPE))
	{
		p_ptr->redraw &= ~(PR_WIPE);
		msg_print(NULL);
		Term_clear();
	}


	if (p_ptr->redraw & (PR_MAP))
	{
		p_ptr->redraw &= ~(PR_MAP);
		prt_map();
	}


	if (p_ptr->redraw & (PR_BASIC))
	{
		p_ptr->redraw &= ~(PR_BASIC);
		p_ptr->redraw &= ~(PR_MISC | PR_TITLE | PR_STATS | PR_STATUS);
		p_ptr->redraw &= ~(PR_LEV | PR_EXP | PR_GOLD);
		p_ptr->redraw &= ~(PR_ARMOR | PR_HP | PR_MANA);
		p_ptr->redraw &= ~(PR_DEPTH | PR_HEALTH);
		prt_frame_basic();
		prt_time();
	}

	if (p_ptr->redraw & (PR_EQUIPPY))
	{
		p_ptr->redraw &= ~(PR_EQUIPPY);
		print_equippy(); /* To draw / delete equippy chars */
	}

	if (p_ptr->redraw & (PR_MISC))
	{
		p_ptr->redraw &= ~(PR_MISC);
		prt_field(rp_ptr->title, ROW_RACE, COL_RACE);
		prt_field(cp_ptr->title, ROW_CLASS, COL_CLASS);
	}

	if (p_ptr->redraw & (PR_TITLE))
	{
		p_ptr->redraw &= ~(PR_TITLE);
		prt_title();
	}

	if (p_ptr->redraw & (PR_LEV))
	{
		p_ptr->redraw &= ~(PR_LEV);
		prt_level();
	}

	if (p_ptr->redraw & (PR_EXP))
	{
		p_ptr->redraw &= ~(PR_EXP);
		prt_exp();
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

	if (p_ptr->redraw & (PR_STATUS))
	{
		p_ptr->redraw &= ~(PR_STATUS);
		prt_status();
	}

	if (p_ptr->redraw & (PR_ARMOR))
	{
		p_ptr->redraw &= ~(PR_ARMOR);
		prt_ac();
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


	if (p_ptr->redraw & (PR_EXTRA))
	{
		p_ptr->redraw &= ~(PR_EXTRA);
		p_ptr->redraw &= ~(PR_CUT | PR_STUN);
		p_ptr->redraw &= ~(PR_HUNGER);
		p_ptr->redraw &= ~(PR_STATE | PR_SPEED | PR_STUDY | PR_STATUS);
		prt_frame_extra();
	}

	if (p_ptr->redraw & (PR_CUT))
	{
		p_ptr->redraw &= ~(PR_CUT);
		prt_cut();
	}

	if (p_ptr->redraw & (PR_STUN))
	{
		p_ptr->redraw &= ~(PR_STUN);
		prt_stun();
	}

	if (p_ptr->redraw & (PR_HUNGER))
	{
		p_ptr->redraw &= ~(PR_HUNGER);
		prt_hunger();
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
 * Handle "p_ptr->window"
 */
void window_stuff(void)
{
	int j;

	u32b mask = 0L;


	/* Nothing to do */
	if (!p_ptr->window) return;

	/* Scan windows */
	for (j = 0; j < 8; j++)
	{
		/* Save usable flags */
		if (angband_term[j]) mask |= window_flag[j];
	}

	/* Apply usable flags */
	p_ptr->window &= mask;

	/* Nothing to do */
	if (!p_ptr->window) return;


	/* Display inventory */
	if (p_ptr->window & (PW_INVEN))
	{
		p_ptr->window &= ~(PW_INVEN);
		fix_inven();
	}

	/* Display equipment */
	if (p_ptr->window & (PW_EQUIP))
	{
		p_ptr->window &= ~(PW_EQUIP);
		fix_equip();
	}

	/* Display spell list */
	if (p_ptr->window & (PW_SPELL))
	{
		p_ptr->window &= ~(PW_SPELL);
		fix_spell();
	}

	/* Display player */
	if (p_ptr->window & (PW_PLAYER))
	{
		p_ptr->window &= ~(PW_PLAYER);
		fix_player(0);
	}

	/* Display stats and resistances */
	if (p_ptr->window & (PW_STATS))
	{
		p_ptr->window &= ~(PW_STATS);
		fix_player(2);
	}

	/* Display overhead view */
	if (p_ptr->window & (PW_MESSAGE))
	{
		p_ptr->window &= ~(PW_MESSAGE);
		fix_message();
	}

	/* Display overhead view */
	if (p_ptr->window & (PW_OVERHEAD))
	{
		p_ptr->window &= ~(PW_OVERHEAD);
		fix_overhead();
	}

	/* Display overhead view */
	if (p_ptr->window & (PW_DUNGEON))
	{
		p_ptr->window &= ~(PW_DUNGEON);
		fix_dungeon();
	}

	/* Display monster recall */
	if (p_ptr->window & (PW_MONSTER))
	{
		p_ptr->window &= ~(PW_MONSTER);
		fix_monster();
	}

	/* Display object recall */
	if (p_ptr->window & (PW_OBJECT))
	{
		p_ptr->window &= ~(PW_OBJECT);
		fix_object();
	}

	/* Display objects on floor */
	if (p_ptr->window & (PW_FLOOR))
	{
		p_ptr->window &= ~(PW_FLOOR);
		fix_floor();
	}
}


/*
 * Handle "p_ptr->update" and "p_ptr->redraw" and "p_ptr->window"
 */
void handle_stuff(void)
{
	/* Update stuff */
	if (p_ptr->update) update_stuff();

	/* Redraw stuff */
	if (p_ptr->redraw) redraw_stuff();

	/* Window stuff */
	if (p_ptr->window) window_stuff();
}

/* Count the number of random quests chosen */
int number_of_quests(void)
{
	int i, j;

	/* Clear the counter */
	i = 0;

	for (j = MIN_RANDOM_QUEST; j < MAX_RANDOM_QUEST + 1; j++)
	{
		if (quest[j].status != QUEST_STATUS_UNTAKEN)
		{
			/* Increment count of quests taken. */
			i++;
		}
	}

	/* Return the number of quests taken */
	return (i);
}

