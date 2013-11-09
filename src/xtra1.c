
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
 *  Whether daytime or not
 */
bool is_daytime(void)
{
	s32b len = TURNS_PER_TICK * TOWN_DAWN;
	if ((turn % len) < (len / 2))
		return TRUE;
	else
		return FALSE;
}

/*
 * Extract day, hour, min
 */
void extract_day_hour_min(int *day, int *hour, int *min)
{
	s32b len = TURNS_PER_TICK * TOWN_DAWN;
	s32b tick = turn % len + len / 4;

	*day = (turn + (TURNS_PER_TICK * TOWN_DAWN /4))/ len + 1;
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
	if (p_ptr->inside_quest && quest_is_fixed(p_ptr->inside_quest)
	    && (quest[p_ptr->inside_quest].flags & QUEST_FLAG_PRESET))
#ifdef JP
		return "クエスト";
#else
		return "Quest";
#endif
	else if (p_ptr->wild_mode)
#ifdef JP
		return "地上";
#else
		return "Surface";
#endif
	else if (p_ptr->inside_arena)
#ifdef JP
		return "アリーナ";
#else
		return "Arena";
#endif
	else if (!dun_level && p_ptr->town_num)
		return town[p_ptr->town_num].name;
	else
		return d_name+d_info[dungeon_type].name;
}

/*
 * Print dungeon
 */
static void prt_dungeon(void)
{
	cptr dungeon_name;
	int col;

	/* Dump 13 spaces to clear */
	c_put_str(TERM_WHITE, "             ", ROW_DUNGEON, COL_DUNGEON);

	dungeon_name = map_name();

	col = COL_DUNGEON + 6 - strlen(dungeon_name)/2;
	if (col < 0) col = 0;

	/* Dump the info itself */
	c_put_str(TERM_L_UMBER, format("%s",dungeon_name),
		  ROW_DUNGEON, col);
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
	if (p_ptr->stat_max[stat] == STAT_MAX_MAX)
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
#define BAR_CUT_BASE       0
#define BAR_STUN_BASE      7
#define BAR_HALLUCINATION 11
#define BAR_BLINDNESS     12
#define BAR_PARALYZE      13
#define BAR_CONFUSE       14
#define BAR_POISONED      15
#define BAR_AFRAID        16
#define BAR_STONING       17
#define BAR_WRAITH        18
#define BAR_PROTEVIL      19
#define BAR_MAGICDEFENSE  20
#define BAR_STONESKIN     21
#define BAR_MULTISHADOW   22
#define BAR_INVULN        23
#define BAR_RESACID       24
#define BAR_RESELEC       25
#define BAR_RESFIRE       26
#define BAR_RESCOLD       27
#define BAR_RESPOIS       28
#define BAR_RESTIME       29
#define BAR_DUSTROBE      30
#define BAR_SHFIRE        31
#define BAR_SHELEC        32
#define BAR_SHCOLD        33
#define BAR_SHHOLY        34
#define BAR_EYEEYE        35
#define BAR_BLESSED       36
#define BAR_HEROISM       37
#define BAR_BERSERK       38
#define BAR_ATTKEVIL      39
#define BAR_ATTKFIRE      40
#define BAR_ATTKCOLD      41
#define BAR_ATTKELEC      42
#define BAR_ATTKACID      43
#define BAR_ATTKPOIS      44
#define BAR_ATTKCONF      45
#define BAR_SENSEUNSEEN   46
#define BAR_TELEPATHY     47
#define BAR_INFRAVISION   48
#define BAR_RECALL        49
#define BAR_ALTER         50
#define BAR_INH_FLOOD     51
#define BAR_REVERTED      52
#define BAR_CHARGESPELL   53
#define BAR_EARTHSPIKE    54
#define BAR_WINDGUARD     55
#define BAR_RESURRECTION  56
#define BAR_INC_BLOW      57
#define BAR_DEC_BLOW      58
#define BAR_ZOSHONEL      59
#define BAR_MERMAID       60


static struct {
	byte attr;
	cptr sstr;
	cptr lstr;
} bar[]
#ifdef JP
= {
	{TERM_YELLOW, "傷", "かすり傷"},
	{TERM_YELLOW, "傷", "軽傷"},
	{TERM_ORANGE, "傷", "ひどい傷"},
	{TERM_ORANGE, "傷", "大変な傷"},
	{TERM_RED, "傷", "重傷"},
	{TERM_RED, "傷", "ひどい深手"},
	{TERM_L_RED, "傷", "致命傷"},
	{TERM_ORANGE, "朦", "朦朧"},
	{TERM_ORANGE, "朦", "ひどく朦朧"},
	{TERM_L_RED, "朦", "気絶寸前"},
	{TERM_RED, "朦", "意識不明瞭"},
	{TERM_VIOLET, "幻", "幻覚"},
	{TERM_L_DARK, "盲", "盲目"},
	{TERM_RED, "痺", "麻痺"},
	{TERM_VIOLET, "乱", "混乱"},
	{TERM_GREEN, "毒", "毒"},
	{TERM_BLUE, "恐", "恐怖"},
	{TERM_SLATE, "石", "石化"},
	{TERM_L_DARK, "幽", "幽体"},
	{TERM_SLATE, "邪", "防邪"},
	{TERM_YELLOW, "魔", "魔法鎧"},
	{TERM_WHITE, "石", "石肌"},
	{TERM_L_BLUE, "分", "分身"},
	{TERM_YELLOW, "無", "無敵"},
	{TERM_GREEN, "酸", "耐酸"},
	{TERM_BLUE, "電", "耐電"},
	{TERM_RED, "火", "耐火"},
	{TERM_SLATE, "冷", "耐冷"},
	{TERM_GREEN, "毒", "耐毒"},
	{TERM_L_BLUE, "時", "耐時間"},
	{TERM_L_UMBER, "破", "破オーラ"},
	{TERM_L_RED, "オ", "火オーラ"},
	{TERM_L_BLUE, "オ", "電オーラ"},
	{TERM_WHITE, "オ", "冷オーラ"},
	{TERM_WHITE, "聖", "聖オーラ"},
	{TERM_VIOLET, "目", "目には目"},
	{TERM_WHITE, "祝", "祝福"},
	{TERM_WHITE, "勇", "勇"},
	{TERM_RED, "狂", "狂乱"},
	{TERM_L_DARK, "邪", "邪剣"},
	{TERM_L_RED, "火", "魔剣火"},
	{TERM_WHITE, "冷", "魔剣冷"},
	{TERM_L_BLUE, "電", "魔剣電"},
	{TERM_SLATE, "酸", "魔剣酸"},
	{TERM_L_GREEN, "毒", "魔剣毒"},
	{TERM_RED, "乱", "混乱打撃"},
	{TERM_L_BLUE, "視", "透明視"},
	{TERM_ORANGE, "テ", "テレパシ"},
	{TERM_L_RED, "赤", "赤外"},
	{TERM_WHITE, "帰", "帰還"},
	{TERM_WHITE, "現", "現実変容"},
	{TERM_BLUE, "禁", "洪水禁止"},
	{TERM_YELLOW, "逆", "反転"},
	{TERM_WHITE, "力", "魔力"},
	{TERM_L_UMBER, "地", "大地の楔"},
	{TERM_L_GREEN, "風", "風の護り"},
	{TERM_WHITE, "復", "復活"},
	{TERM_WHITE, "増", "攻撃増加"},
	{TERM_L_DARK, "減", "攻撃減少"},
	{TERM_L_RED, "ゾ", "ゾショネル"},
	{TERM_BLUE, "水", "水中行動"},
	{0, NULL, NULL}
};
#else
= {
	{TERM_YELLOW, "Cut", "Graze"},
	{TERM_YELLOW, "Cut", "LightCut"},
	{TERM_ORANGE, "Cut", "BadCut"},
	{TERM_ORANGE, "Cut", "NastyCut"},
	{TERM_RED, "Cut", "SevereCut"},
	{TERM_RED, "Cut", "DeepGash"},
	{TERM_L_RED, "Cut", "MortalWound"},
	{TERM_ORANGE, "Stu", "Stun"},
	{TERM_ORANGE, "Stu", "HeavyStun"},
	{TERM_L_RED, "Stu", "NearlyFaint"},
	{TERM_RED, "Stu", "KnockedOut"},
	{TERM_VIOLET, "Hu", "Hullc"},
	{TERM_L_DARK, "Bl", "Blind"},
	{TERM_RED, "Pa", "Paralyzed"},
	{TERM_VIOLET, "Cf", "Confused"},
	{TERM_GREEN, "Po", "Poisoned"},
	{TERM_BLUE, "Af", "Afraid"},
	{TERM_SLATE, "St", "Stoning"},
	{TERM_L_DARK, "Wr", "Wraith"},
	{TERM_SLATE, "Ev", "PrtEvl"},
	{TERM_YELLOW, "Md", "MgcArm"},
	{TERM_WHITE, "Ss", "StnSkn"},
	{TERM_L_BLUE, "Ms", "MltShdw"},
	{TERM_YELLOW, "Iv", "Invuln"},
	{TERM_GREEN, "Ac", "Acid"},
	{TERM_BLUE, "El", "Elec"},
	{TERM_RED, "Fi", "Fire"},
	{TERM_SLATE, "Co", "Cold"},
	{TERM_GREEN, "Po", "Pois"},
	{TERM_L_BLUE, "Ti", "Time"},
	{TERM_L_UMBER, "SSh", "SShar"},
	{TERM_L_RED, "SFi", "SFire"},
	{TERM_L_BLUE, "SEl", "SElec"},
	{TERM_WHITE, "SCo", "SCold"},
	{TERM_WHITE, "Ho", "Holy"},
	{TERM_VIOLET, "Ee", "EyeEye"},
	{TERM_WHITE, "Bs", "Bless"},
	{TERM_WHITE, "He", "Hero"},
	{TERM_RED, "Br", "Berserk"},
	{TERM_L_DARK, "BEv", "BEvil"},
	{TERM_L_RED, "BFi", "BFire"},
	{TERM_WHITE, "BCo", "BCold"},
	{TERM_L_BLUE, "BEl", "BElec"},
	{TERM_SLATE, "BAc", "BAcid"},
	{TERM_L_GREEN, "BPo", "BPois"},
	{TERM_RED, "TCf", "TchCnf"},
	{TERM_L_BLUE, "Se", "SInv"},
	{TERM_ORANGE, "Te", "Telepa"},
	{TERM_L_RED, "If", "Infr"},
	{TERM_WHITE, "Rc", "Recall"},
	{TERM_WHITE, "Al", "Alter"},
	{TERM_BLUE, "Inh", "NoFlood"},
	{TERM_YELLOW, "Rv", "Reverted"},
	{TERM_WHITE, "CS", "Spell"},
	{TERM_L_UMBER, "Es", "EarthSpike"},
	{TERM_L_GREEN, "Wg", "WindGuard"},
	{TERM_WHITE, "Rs", "Resurrect"},
	{TERM_WHITE, "Inc", "IncBlow"},
	{TERM_L_DARK, "Dec", "DecBlow"},
	{TERM_L_RED, "Zo", "Zoshonel"},
	{TERM_BLUE, "Iw", "InWater"},
	{0, NULL, NULL}
};
#endif

/* Hack -- dividing by 32 is into 5-bit shift */
/* Hack -- MOD by 32 is into 5-bit mask */
#define ADD_FLG(FLG) (bar_flags[((FLG) >> 5)] |= (1L << ((FLG) & 0x1f)))
#define IS_FLG(FLG) (bar_flags[((FLG) >> 5)] & (1L << ((FLG) & 0x1f)))


/*
 *  Show status bar
 */
static void prt_status(void)
{
	u32b bar_flags[3];
	int wid, hgt, row_statbar, max_col_statbar;
	int i, col = 0, num = 0;
	int space = 2;
	int cut_lev = cut_level(p_ptr->cut);
	int stun_lev = stun_level(p_ptr->stun);

	Term_get_size(&wid, &hgt);
	row_statbar = hgt + ROW_STATBAR;
	max_col_statbar = wid + MAX_COL_STATBAR;

	Term_erase(0, row_statbar, max_col_statbar);

	C_WIPE(bar_flags, 3, u32b);

	/* Cut */
	if (cut_lev > 0) ADD_FLG(BAR_CUT_BASE + cut_lev - 1);

	/* Stun */
	if (stun_lev > 0) ADD_FLG(BAR_STUN_BASE + stun_lev - 1);

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

	/* Stoning */
	if (p_ptr->stoning) ADD_FLG(BAR_STONING);

	/* Times see-invisible */
	if (p_ptr->tim_invis) ADD_FLG(BAR_SENSEUNSEEN);

	/* Timed esp */
	if (p_ptr->tim_esp) ADD_FLG(BAR_TELEPATHY);

	/* Timed infra-vision */
	if (p_ptr->tim_infra) ADD_FLG(BAR_INFRAVISION);

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

	/* Alter realiry */
	if (p_ptr->alter_reality) ADD_FLG(BAR_ALTER);

	/* Inhibiting using of great flood */
	if (p_ptr->inhibit_flood) ADD_FLG(BAR_INH_FLOOD);

	/* Afraid */
	if (p_ptr->afraid) ADD_FLG(BAR_AFRAID);

	/* Resist time */
	if (p_ptr->tim_res_time) ADD_FLG(BAR_RESTIME);

	if (p_ptr->multishadow) ADD_FLG(BAR_MULTISHADOW);

	/* Confusing Hands */
	if (p_ptr->special_attack & ATTACK_CONFUSE) ADD_FLG(BAR_ATTKCONF);

	if (p_ptr->dustrobe) ADD_FLG(BAR_DUSTROBE);

	if (p_ptr->special_attack & ATTACK_EVIL) ADD_FLG(BAR_ATTKEVIL);

	/* Magical Weapon  */
	if (p_ptr->special_attack & ATTACK_FIRE) ADD_FLG(BAR_ATTKFIRE);
	if (p_ptr->special_attack & ATTACK_COLD) ADD_FLG(BAR_ATTKCOLD);
	if (p_ptr->special_attack & ATTACK_ELEC) ADD_FLG(BAR_ATTKELEC);
	if (p_ptr->special_attack & ATTACK_ACID) ADD_FLG(BAR_ATTKACID);
	if (p_ptr->special_attack & ATTACK_POIS) ADD_FLG(BAR_ATTKPOIS);

	/* tim stealth */
	if (p_ptr->tim_sh_fire) ADD_FLG(BAR_SHFIRE);
	if (p_ptr->tim_sh_elec) ADD_FLG(BAR_SHELEC);
	if (p_ptr->tim_sh_cold) ADD_FLG(BAR_SHCOLD);

	/* Holy aura */
	if (p_ptr->tim_sh_holy) ADD_FLG(BAR_SHHOLY);

	/* An Eye for an Eye */
	if (p_ptr->tim_eyeeye) ADD_FLG(BAR_EYEEYE);

	/* Element is reverted */
	if (p_ptr->opposite_pelem) ADD_FLG(BAR_REVERTED);

	/* Chargespell */
	if (p_ptr->chargespell) ADD_FLG(BAR_CHARGESPELL);

	/* Earth Spike */
	if (p_ptr->earth_spike) ADD_FLG(BAR_EARTHSPIKE);

	/* Wind Guard */
	if (p_ptr->wind_guard) ADD_FLG(BAR_WINDGUARD);

	/* Resurrection */
	if (p_ptr->tim_resurrection) ADD_FLG(BAR_RESURRECTION);

	/* Timed blows */
	if (p_ptr->tim_inc_blow) ADD_FLG(BAR_INC_BLOW);
	if (p_ptr->tim_dec_blow) ADD_FLG(BAR_DEC_BLOW);

	/* Protection of Zoshonel */
	if (p_ptr->zoshonel_protect) ADD_FLG(BAR_ZOSHONEL);

	/* Mermaid in water */
	if (p_ptr->mermaid_in_water) ADD_FLG(BAR_MERMAID);

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
	cptr p = "";
	char str[14];

	/* Wizard */
	if (p_ptr->wizard)
	{
#ifdef JP
		/* 英日切り替え機能 称号 */
		p = "[ウィザード]";
#else
		p = "[=-WIZARD-=]";
#endif

	}

	/* Legendary Ogre */
	else if (!r_info[MON_FILARHH].max_num)
	{
#ifdef JP
		p = "伝説のオウガ";
#else
		p = "**TrueOgre**";
#endif

	}

	/* Winner */
	else if (p_ptr->total_winner || (p_ptr->lev > PY_MAX_LEVEL))
	{
		if ((p_ptr->arena_number > MAX_ARENA_MONS+4) && (p_ptr->arena_number < 99))
		{
#ifdef JP
			/* 英日切り替え機能 称号 */
			p = "*真・勝利者*";

#else
			p = "*TRUEWINNER*";
#endif
		}
		else
		{
#ifdef JP
			/* 英日切り替え機能 称号 */
			p = "***勝利者***";

#else
			p = "***WINNER***";
#endif
		}
	}

	/* Normal */
	else
	{
		my_strcpy(str, player_title[p_ptr->pclass][(p_ptr->cexp_info[p_ptr->pclass].clev - 1) / 5], sizeof(str));
		p = str;
	}

	prt_field(p, ROW_TITLE, COL_TITLE);
}


/*
 * Prints racial level
 */
static void prt_level(void)
{
	char tmp[32];

	sprintf(tmp, "%5d", p_ptr->lev);


	if (p_ptr->lev >= p_ptr->max_plv)
	{
#ifdef JP
		put_str("レベル ", ROW_LEVEL, 0);
#else
		put_str("LEVEL  ", ROW_LEVEL, 0);
#endif
		c_put_str(TERM_L_GREEN, tmp, ROW_LEVEL, COL_LEVEL + 7);
	}
	else
	{
#ifdef JP
		put_str("xレベル", ROW_LEVEL, 0);
#else
		put_str("Level  ", ROW_LEVEL, 0);
#endif
		c_put_str(TERM_YELLOW, tmp, ROW_LEVEL, COL_LEVEL + 7);
	}
}


/*
 * Display the racial experience
 */
static void prt_exp(void)
{
	char out_val[32];

	(void)sprintf(out_val, "%8ld", (long)p_ptr->exp);


	if (p_ptr->exp >= p_ptr->max_exp)
	{
		put_str("EX  ", ROW_EXP, 0);
		c_put_str(TERM_L_GREEN, out_val, ROW_EXP, COL_EXP + 4);
	}
	else
	{
#ifdef JP
		put_str("xEX ", ROW_EXP, 0);
#else
		put_str("ex  ", ROW_EXP, 0);
#endif
		c_put_str(TERM_YELLOW, out_val, ROW_EXP, COL_EXP + 4);
	}
}


/*
 * Prints class level
 */
static void prt_clevel(void)
{
	char tmp[32];
	cexp_info_type *cexp_ptr = &p_ptr->cexp_info[p_ptr->pclass];

	sprintf(tmp, "%5d", cexp_ptr->clev);


	if (cexp_ptr->clev >= cexp_ptr->max_clev)
	{
#ifdef JP
		put_str("C-Lv   ", ROW_CLEVEL, 0);
#else
		put_str("CLEVEL ", ROW_CLEVEL, 0);
#endif
		c_put_str(TERM_L_GREEN, tmp, ROW_CLEVEL, COL_CLEVEL + 7);
	}
	else
	{
#ifdef JP
		put_str("xC-Lv  ", ROW_CLEVEL, 0);
#else
		put_str("CLevel ", ROW_CLEVEL, 0);
#endif
		c_put_str(TERM_YELLOW, tmp, ROW_CLEVEL, COL_CLEVEL + 7);
	}
}


/*
 * Display the class experience
 */
static void prt_cexp(void)
{
	char out_val[32];
	cexp_info_type *cexp_ptr = &p_ptr->cexp_info[p_ptr->pclass];

	(void)sprintf(out_val, "%8ld", (long)cexp_ptr->cexp);


	if (cexp_ptr->cexp >= cexp_ptr->max_cexp)
	{
		put_str("CE  ", ROW_CEXP, 0);
		c_put_str(TERM_L_GREEN, out_val, ROW_CEXP, COL_CEXP + 4);
	}
	else
	{
#ifdef JP
		put_str("xCE ", ROW_CEXP, 0);
#else
		put_str("ce  ", ROW_CEXP, 0);
#endif
		c_put_str(TERM_YELLOW, out_val, ROW_CEXP, COL_CEXP + 4);
	}
}


/*
 * Prints current gold
 */
static void prt_gold(void)
{
	char tmp[32];

#ifdef JP
	put_str("$  ", ROW_GOLD, COL_GOLD);
#else
	put_str("AU ", ROW_GOLD, COL_GOLD);
#endif

	sprintf(tmp, "%9ld", (long)p_ptr->au_sum);
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
	put_str("AC (      )", ROW_AC, COL_AC);
	sprintf(tmp, "%6d", p_ptr->dis_ac + p_ptr->dis_to_a);
	c_put_str(TERM_L_GREEN, tmp, ROW_AC, COL_AC + 4);
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

	put_str("HP", ROW_CURHP, COL_CURHP);

	/* 現在のヒットポイント */
	sprintf(tmp, "%5d", p_ptr->chp);

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

	c_put_str(color, tmp, ROW_CURHP, COL_CURHP+2);

	/* 区切り */
	put_str( "/", ROW_CURHP, COL_CURHP + 7 );

	/* 最大ヒットポイント */
	sprintf(tmp, "%4d", p_ptr->mhp);
	color = TERM_L_GREEN;

	c_put_str(color, " ", ROW_CURHP, COL_CURHP + 12);
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

	if (p_ptr->pclass == CLASS_GUNNER) return;

#ifdef JP
	put_str("MP", ROW_CURSP, COL_CURSP);
#else
	put_str("SP", ROW_CURSP, COL_CURSP);
#endif

	/* 現在のマジックポイント */
	sprintf(tmp, "%5d", p_ptr->csp);

	if (p_ptr->csp >= p_ptr->msp)
	{
		color = TERM_L_GREEN;
	}
	else if (p_ptr->csp > (p_ptr->msp * hitpoint_warn) / 10)
	{
		color = TERM_YELLOW;
	}
	else
	{
		color = TERM_RED;
	}

	c_put_str(color, tmp, ROW_CURSP, COL_CURSP+2);

	/* 区切り */
	put_str( "/", ROW_CURSP, COL_CURSP + 7 );

	/* 最大マジックポイント */
	sprintf(tmp, "%4d", p_ptr->msp);
	color = TERM_L_GREEN;

	c_put_str(color, " ", ROW_CURSP, COL_CURSP + 12);
	c_put_str(color, tmp, ROW_CURSP, COL_CURSP + 8);
}


/*
 * Prints depth in stat area
 */
static void prt_depth(void)
{
	char depths[32];
	int wid, hgt, row_depth, col_depth;

	Term_get_size(&wid, &hgt);
	col_depth = wid + COL_DEPTH;
	row_depth = hgt + ROW_DEPTH;

	if (!dun_level)
	{
#ifdef JP
		strcpy(depths, "地上");
#else
		strcpy(depths, "Surf.");
#endif
	}
	else if (p_ptr->inside_quest && !dungeon_type)
	{
#ifdef JP
		strcpy(depths, "地上");
#else
		strcpy(depths, "Quest");
#endif

	}
	else if (depth_in_feet)
	{
#ifdef JP
		(void)sprintf(depths, "%d ft", dun_level * 50);
#else
		(void)sprintf(depths, "%d ft", dun_level * 50);
#endif

	}
	else
	{
#ifdef JP
		(void)sprintf(depths, "%d 階", dun_level);
#else
		(void)sprintf(depths, "Lev %d", dun_level);
#endif

	}

	/* Right-Adjust the "depth", and clear old values */
	prt(format("%8s", depths), row_depth, col_depth);
}


/*
 * Prints weather
 */
static void prt_weather(void)
{
	char buf[10];
	int wid, hgt, row_weather, col_weather;
	byte attr = TERM_WHITE;

	/* Name table: access keys are RAIN and TEMP/2 */
	static cptr weather_names_table[4][2] =
	{
		{"快晴", "快晴"},
		{"曇り", "曇り"},
		{"雨", "雪"},
		{"豪雨", "豪雪"},
	};

	Term_get_size(&wid, &hgt);
	col_weather = wid + COL_WEATHER;
	row_weather = hgt + ROW_WEATHER;

	switch (weather_level(weather[WEATHER_TEMP]))
	{
	case 0:
		attr = TERM_L_RED;
		break;

	case 1:
		attr = TERM_YELLOW;
		break;

	case 2:
		attr = TERM_WHITE;
		break;

	case 3:
		attr = TERM_L_BLUE;
		break;
	}

	if ((weather_level(weather[WEATHER_RAIN]) == 3) && (weather_level(weather[WEATHER_WIND]) == 3))
	{
		if (weather_level(weather[WEATHER_TEMP]) > 1) strcpy(buf, "暴風雪");
		else strcpy(buf, "暴風雨");
	}
	else
	{
		sprintf(buf, "%s %s",
			weather_names_table[weather_level(weather[WEATHER_RAIN])][weather_level(weather[WEATHER_TEMP]) / 2],
			weather_table[WEATHER_WIND][weather_level(weather[WEATHER_WIND])].title);
	}

	/* Right-Adjust the "weather", and clear old values */
	c_put_str(TERM_WHITE, format("         ", buf), row_weather, col_weather);
	c_put_str(attr, format("%9s", buf), row_weather, col_weather);
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

	/* Repeating */
	if (command_rep)
	{
		if (command_rep > 999)
		{
#ifdef JP
			(void)sprintf(text, "%2d00", command_rep / 100);
#else
			(void)sprintf(text, "%2d00", command_rep / 100);
#endif

		}
		else
		{
#ifdef JP
			(void)sprintf(text, "  %2d", command_rep);
#else
			(void)sprintf(text, "  %2d", command_rep);
#endif

		}
	}

	/* Action */
	else
	{
		switch(p_ptr->action)
		{
			case ACTION_SEARCH:
			{
#ifdef JP
				strcpy(text, "探索");
#else
				strcpy(text, "Sear");
#endif
				break;
			}
			case ACTION_REST:
			{
				int i;

				/* Start with "Rest" */
#ifdef JP
				strcpy(text, "    ");
#else
				strcpy(text, "    ");
#endif


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
				break;
			}
			case ACTION_SING:
			{
#ifdef JP
				strcpy(text, "歌  ");
#else
				strcpy(text, "Sing");
#endif
				break;
			}
			case ACTION_STEALTH:
			{
#ifdef JP
				strcpy(text, "忍足");
#else
				strcpy(text, "Stlh");
#endif
				break;
			}
			case ACTION_ELEMSCOPE:
			{
#ifdef JP
				strcpy(text, "精視");
#else
				strcpy(text, "Elem");
#endif
				break;
			}
			case ACTION_AURA:
			{
#ifdef JP
				strcpy(text, "闘気");
#else
				strcpy(text, "Aura");
#endif
				break;
			}
			default:
			{
				strcpy(text, "    ");
				break;
			}
		}
	}

	/* Display the info (or blanks) */
	c_put_str(attr, format("%5.5s",text), ROW_STATE, COL_STATE);
}


/*
 * Prints the speed of a character.			-CJS-
 */
static void prt_speed(void)
{
	int i = p_ptr->pspeed;
	bool is_fast = p_ptr->fast;

	byte attr = TERM_WHITE;
	char buf[32] = "";
	int wid, hgt, row_speed, col_speed;

	Term_get_size(&wid, &hgt);
	col_speed = wid + COL_SPEED;
	row_speed = hgt + ROW_SPEED;

	/* Hack -- Visually "undo" the Search Mode Slowdown */
	if (p_ptr->action == ACTION_SEARCH) i += 10;

	/* Fast */
	if (i > 110)
	{
		if (p_ptr->riding)
		{
			if (m_list[p_ptr->riding].fast && !m_list[p_ptr->riding].slow) attr = TERM_L_BLUE;
			else if (m_list[p_ptr->riding].slow && !m_list[p_ptr->riding].fast) attr = TERM_VIOLET;
			else attr = TERM_GREEN;
		}
		else if (is_fast && !p_ptr->slow) attr = TERM_YELLOW;
		else if (p_ptr->slow && !is_fast) attr = TERM_VIOLET;
		else attr = TERM_L_GREEN;
#ifdef JP
		sprintf(buf, "%s(+%d)", (p_ptr->riding ? "乗馬" : "加速"), (i - 110));
#else
		sprintf(buf, "Fast(+%d)", (i - 110));
#endif

	}

	/* Slow */
	else if (i < 110)
	{
		if (p_ptr->riding)
		{
			if (m_list[p_ptr->riding].fast && !m_list[p_ptr->riding].slow) attr = TERM_L_BLUE;
			else if (m_list[p_ptr->riding].slow && !m_list[p_ptr->riding].fast) attr = TERM_VIOLET;
			else attr = TERM_RED;
		}
		else if (is_fast && !p_ptr->slow) attr = TERM_YELLOW;
		else if (p_ptr->slow && !is_fast) attr = TERM_VIOLET;
		else attr = TERM_L_UMBER;
#ifdef JP
		sprintf(buf, "%s(-%d)", (p_ptr->riding ? "乗馬" : "減速"), (110 - i));
#else
		sprintf(buf, "Slow(-%d)", (110 - i));
#endif
	}
	else if (p_ptr->riding)
	{
		attr = TERM_GREEN;
#ifdef JP
		strcpy(buf, "乗馬中");
#else
		strcpy(buf, "Riding");
#endif
	}

	/* Display the speed */
	c_put_str(attr, format("%-9s", buf), row_speed, col_speed);
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
		int pct, pct2, len;

		monster_type *m_ptr = &m_list[p_ptr->health_who];

		/* Default to almost dead */
		byte attr = TERM_RED;

		/* Extract the "percent" of health */
		pct = 100L * m_ptr->hp / m_ptr->maxhp;
		pct2 = 100L * m_ptr->hp / m_ptr->max_maxhp;

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

		/* Stoning */
		if (m_ptr->stoning) attr = TERM_SLATE;

		/* Invulnerable */
		if (m_ptr->invulner) attr = TERM_WHITE;

		/* Convert percent into "health" */
		len = (pct2 < 10) ? 1 : (pct2 < 90) ? (pct2 / 10 + 1) : 10;

		/* Default to "unknown" */
		Term_putstr(COL_INFO, ROW_INFO, 12, TERM_WHITE, "[----------]");

		/* Dump the current "health" (use '*' symbols) */
		Term_putstr(COL_INFO + 1, ROW_INFO, len, attr, "**********");
	}

#endif

}



static void riding_health_redraw(void)
{

#ifdef DRS_SHOW_HEALTH_BAR

	/* Not tracking */
	if (!p_ptr->riding)
	{
		/* Erase the health bar */
		Term_erase(COL_RIDING_INFO, ROW_RIDING_INFO, 12);
	}

	/* Tracking a hallucinatory monster */
	else if (p_ptr->image)
	{
		/* Indicate that the monster health is "unknown" */
		Term_putstr(COL_RIDING_INFO, ROW_RIDING_INFO, 12, TERM_WHITE, "[----------]");
	}

	/* Tracking a dead monster (???) */
	else if (!m_list[p_ptr->health_who].hp < 0)
	{
		/* Indicate that the monster health is "unknown" */
		Term_putstr(COL_RIDING_INFO, ROW_RIDING_INFO, 12, TERM_WHITE, "[----------]");
	}

	/* Tracking a visible monster */
	else
	{
		int pct, pct2, len;

		monster_type *m_ptr = &m_list[p_ptr->riding];

		/* Default to almost dead */
		byte attr = TERM_RED;

		/* Extract the "percent" of health */
		pct = 100L * m_ptr->hp / m_ptr->maxhp;
		pct2 = 100L * m_ptr->hp / m_ptr->max_maxhp;

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
		len = (pct2 < 10) ? 1 : (pct2 < 90) ? (pct2 / 10 + 1) : 10;

		/* Default to "unknown" */
		Term_putstr(COL_RIDING_INFO, ROW_RIDING_INFO, 12, TERM_WHITE, "[----------]");

		/* Dump the current "health" (use '*' symbols) */
		Term_putstr(COL_RIDING_INFO + 1, ROW_RIDING_INFO, len, attr, "**********");
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
	if (!(cp_ptr->c_flags & PCF_REINCARNATE))
	{
		char str[14];
		my_strcpy(str, rp_ptr->title, sizeof(str));
		prt_field(str, ROW_RACE, COL_RACE);
	}
	else
	{
		prt_field("             ", ROW_RACE, COL_RACE);
	}
/*	prt_field(cp_ptr->title, ROW_CLASS, COL_CLASS); */


	/* Title */
	prt_title();

	/* Level/Experience */
	prt_level();
	prt_exp();
	prt_clevel();
	prt_cexp();

	/* All Stats */
	for (i = 0; i < A_MAX; i++) prt_stat(i);

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

	/* Current weather */
	prt_weather();

	/* Special */
	health_redraw();
	riding_health_redraw();
}


/*
 * Display extra info (mostly below map)
 */
static void prt_frame_extra(void)
{
	/* Food */
	prt_hunger();

	/* State */
	prt_state();

	/* Speed */
	prt_speed();

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
static void fix_player(void)
{
	int j;

	/* Scan windows */
	for (j = 0; j < 8; j++)
	{
		term *old = Term;

		/* No window */
		if (!angband_term[j]) continue;

		/* No relevant flags */
		if (!(window_flag[j] & (PW_PLAYER))) continue;

		/* Activate */
		Term_activate(angband_term[j]);

		update_playtime();

		/* Display player */
		display_player(0);

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
			Term_putstr(0, (h - 1) - i, -1, (byte)((i < now_message) ? TERM_WHITE : TERM_SLATE), message_str((s16b)i));

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
		int wid, hgt;

		/* No window */
		if (!angband_term[j]) continue;

		/* No relevant flags */
		if (!(window_flag[j] & (PW_OVERHEAD))) continue;

		/* Activate */
		Term_activate(angband_term[j]);

		/* Full map in too small window is useless  */
		Term_get_size(&wid, &hgt);
		if (wid > COL_MAP + 2 && hgt > ROW_MAP + 2)
		{
			/* Redraw map */
			display_map(&cy, &cx);

			/* Fresh */
			Term_fresh();
		}

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
 * Calculate maximum mana.  You do not need to know any spells.
 * Note that mana is lowered by heavy (or inappropriate) armor.
 *
 * This function induces status messages.
 */
static void calc_mana(void)
{
	int		cur_wgt, max_wgt, i, j;
	s32b msp;

	object_type	*o_ptr;

	/* Gunner has no mana */
	if (p_ptr->pclass == CLASS_GUNNER)
	{
		p_ptr->msp = 0;
		p_ptr->csp = 0;
		p_ptr->csp_frac = 0;

		p_ptr->cumber_glove = FALSE;
		p_ptr->cumber_armor = FALSE;
		p_ptr->old_cumber_glove = p_ptr->cumber_glove;
		p_ptr->old_cumber_armor = p_ptr->cumber_armor;

		return;
	}

	/* Calculate mana */
	msp = p_ptr->player_gsp;
	for (i = 0; i < p_ptr->lev; i++) msp += p_ptr->race_sp[i];

	for (i = 0; i < MAX_CLASS; i++)
	{
		if (p_ptr->cexp_info[i].clev > 0)
		{
			for (j = 0; j < p_ptr->cexp_info[i].clev; j++) msp += p_ptr->class_sp[i][j];
		}
	}

	for (i = 0; i < 2; i++) msp = msp * (100 + p_ptr->inc_msp[i]) / 100;

	if (mp_ptr->spell_book)
		msp += ((int)(adj_mag_mana[p_ptr->stat_ind[mp_ptr->spell_stat]]) - 128) * p_ptr->lev / 10;

	/* Only mages are affected */
	if (mp_ptr->spell_xtra & MAGIC_GLOVE_REDUCE_MANA)
	{
		u32b flgs[TR_FLAG_SIZE];

		/* Assume player is not encumbered by gloves */
		p_ptr->cumber_glove = FALSE;

		/* Get the gloves */
		o_ptr = &inventory[INVEN_HANDS];

		/* Examine the gloves */
		object_flags(o_ptr, flgs);

		/* Normal gloves hurt mage-type spells */
		if (o_ptr->k_idx &&
			!((o_ptr->tval == TV_GLOVES) && (o_ptr->sval == SV_SET_OF_GLOVES)) &&
		    !have_flag(flgs, TR_FREE_ACT) &&
		    !have_flag(flgs, TR_MAGIC_MASTERY) &&
		    !(have_flag(flgs, TR_DEX) && (o_ptr->to_stat[A_DEX] > 0)))
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
	if (inventory[INVEN_RARM].tval > TV_SWORD) cur_wgt += inventory[INVEN_RARM].weight;
	if (inventory[INVEN_LARM].tval > TV_SWORD) cur_wgt += inventory[INVEN_LARM].weight;
	cur_wgt += inventory[INVEN_BODY].weight;
	cur_wgt += inventory[INVEN_HEAD].weight;
	cur_wgt += inventory[INVEN_OUTER].weight;
	cur_wgt += inventory[INVEN_HANDS].weight;
	cur_wgt += inventory[INVEN_FEET].weight;

	/* Subtract a percentage of maximum mana. */
	switch (p_ptr->pclass)
	{
		/* For these classes, mana is halved if armour 
		 * is 30 pounds over their weight limit. */
		case CLASS_WIZARD:
		case CLASS_SIRENE:
		case CLASS_PRIEST:
		case CLASS_LICH:
		case CLASS_HIGHWITCH:
		case CLASS_ARCHMAGE:
		{
			if (inventory[INVEN_RARM].tval <= TV_SWORD) cur_wgt += inventory[INVEN_RARM].weight;
			if (inventory[INVEN_LARM].tval <= TV_SWORD) cur_wgt += inventory[INVEN_LARM].weight;
			break;
		}

		/* Mana halved if armour is 40 pounds over weight limit. */
		case CLASS_WARLOCK:
		case CLASS_WITCH:
		case CLASS_CLERIC:
		{
			if (inventory[INVEN_RARM].tval <= TV_SWORD) cur_wgt += inventory[INVEN_RARM].weight*2/3;
			if (inventory[INVEN_LARM].tval <= TV_SWORD) cur_wgt += inventory[INVEN_LARM].weight*2/3;
			break;
		}

		case CLASS_NINJA:
		case CLASS_EXORCIST:
		{
			if (inventory[INVEN_RARM].tval <= TV_SWORD) cur_wgt += inventory[INVEN_RARM].weight/2;
			if (inventory[INVEN_LARM].tval <= TV_SWORD) cur_wgt += inventory[INVEN_LARM].weight/2;
			break;
		}

		/* Mana halved if armour is 50 pounds over weight limit. */
		case CLASS_BEASTTAMER:
		case CLASS_SWORDMASTER:
		case CLASS_DRAGONTAMER:
		case CLASS_NINJAMASTER:
		case CLASS_CRESCENT:
		{
			if (inventory[INVEN_RARM].tval <= TV_SWORD) cur_wgt += inventory[INVEN_RARM].weight/3;
			if (inventory[INVEN_LARM].tval <= TV_SWORD) cur_wgt += inventory[INVEN_LARM].weight/3;
			break;
		}

		/* Mana halved if armour is 60 pounds over weight limit. */
		case CLASS_DRAGOON:
		case CLASS_VALKYRIE:
		case CLASS_TEMPLEKNIGHT:
		case CLASS_WHITEKNIGHT:
		case CLASS_LORD:
		case CLASS_FREYA:
		case CLASS_VAMPIRE:
		{
			if (inventory[INVEN_RARM].tval <= TV_SWORD) cur_wgt += inventory[INVEN_RARM].weight/5;
			if (inventory[INVEN_LARM].tval <= TV_SWORD) cur_wgt += inventory[INVEN_LARM].weight/5;
			break;
		}

		/* For new classes created, but not yet added to this formula. */
		default:
		{
			break;
		}
	}

	/* Determine the weight allowance */
	max_wgt = mp_ptr->spell_weight;

	/* Heavy armor penalizes mana by a percentage.  -LM- */
	if (((cur_wgt - max_wgt) > 0) && (mp_ptr->spell_book))
	{
		/* Encumbered */
		p_ptr->cumber_armor = TRUE;

		/* Subtract a percentage of maximum mana. */
		switch (p_ptr->pclass)
		{
			/* For these classes, mana is halved if armour 
			 * is 30 pounds over their weight limit. */
			case CLASS_NINJA:
			case CLASS_WIZARD:
			case CLASS_SIRENE:
			case CLASS_PRIEST:
			case CLASS_LICH:
			case CLASS_HIGHWITCH:
			case CLASS_ARCHMAGE:
			case CLASS_NINJAMASTER:
			{
				msp -= msp * (cur_wgt - max_wgt) / 600;
				break;
			}

			/* Mana halved if armour is 40 pounds over weight limit. */
			case CLASS_WARLOCK:
			case CLASS_WITCH:
			case CLASS_CLERIC:
			{
				msp -= msp * (cur_wgt - max_wgt) / 800;
				break;
			}

			case CLASS_EXORCIST:
			{
				msp -= msp * (cur_wgt - max_wgt) / 900;
				break;
			}

			/* Mana halved if armour is 50 pounds over weight limit. */
			case CLASS_BEASTTAMER:
			case CLASS_SWORDMASTER:
			case CLASS_DRAGONTAMER:
			{
				msp -= msp * (cur_wgt - max_wgt) / 1000;
				break;
			}

			/* Mana halved if armour is 60 pounds over weight limit. */
			case CLASS_SOLDIER:
			case CLASS_KNIGHT:
			case CLASS_BERSERKER:
			case CLASS_TERRORKNIGHT:
			case CLASS_DRAGOON:
			case CLASS_AMAZONESS:
			case CLASS_VALKYRIE:
			case CLASS_ARCHER:
			case CLASS_ANGELKNIGHT:
			case CLASS_TEMPLEKNIGHT:
			case CLASS_WHITEKNIGHT:
			case CLASS_LORD:
			case CLASS_GENERAL:
			case CLASS_FREYA:
			case CLASS_CRESCENT:
			case CLASS_VAMPIRE:
			{
				msp -= msp * (cur_wgt - max_wgt) / 1200;
				break;
			}

			/* For new classes created, but not yet added to this formula. */
			default:
			{
				msp -= msp * (cur_wgt - max_wgt) / 800;
				break;
			}
		}
	}

	/* Mana can never be negative */
	if (msp < 0) msp = 0;

	/* Prevent overflow */
	if (msp > 30000) msp = 30000;


	/* Maximum mana has changed */
	if (p_ptr->msp != msp)
	{
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
		p_ptr->window |= (PW_SPELL);
	}


	/* Hack -- handle "xtra" mode */
	if (character_xtra) return;

	/* Take note when "glove state" changes */
	if (p_ptr->old_cumber_glove != p_ptr->cumber_glove)
	{
		/* Message */
		if (realm_choices[p_ptr->pclass])
		{
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
		}

		/* Save it */
		p_ptr->old_cumber_glove = p_ptr->cumber_glove;
	}


	/* Take note when "armor state" changes */
	if (p_ptr->old_cumber_armor != p_ptr->cumber_armor)
	{
		/* Message */
		if (realm_choices[p_ptr->pclass])
		{
			if (p_ptr->cumber_armor)
			{
#ifdef JP
				msg_print("装備の重さで動きが鈍くなってしまっている。");
#else
				msg_print("The weight of your equipment encumbers your movement.");
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
	int bonus, i, j;
	s32b mhp;

	/* Un-inflate "half-hitpoint bonus per level" value */
	bonus = ((int)(adj_con_mhp[p_ptr->stat_ind[A_CON]]) - 128) * p_ptr->lev / 5;

	/* Calculate hitpoints */
	mhp = p_ptr->player_ghp;

	for (i = 0; i < p_ptr->lev; i++) mhp += p_ptr->race_hp[i];

	for (i = 0; i < MAX_CLASS; i++)
	{
		if (p_ptr->cexp_info[i].clev > 0)
		{
			for (j = 0; j < p_ptr->cexp_info[i].clev; j++) mhp += p_ptr->class_hp[i][j];
		}
	}

	if (p_ptr->ogre_equip) mhp *= 5;
	if (p_ptr->zoshonel_protect) mhp = mhp * 6 / 5;
	if (p_ptr->action == ACTION_AURA) mhp = mhp * 6 / 5;

	mhp += bonus;

	/* Always have at least one hitpoint per level */
	if (mhp < p_ptr->lev + 1) mhp = p_ptr->lev + 1;

	/* Factor in the hero / superhero settings */
	if (p_ptr->hero) mhp += 10;
	if (p_ptr->shero) mhp += 30;

	/* Prevent overflow */
	if (mhp > 30000) mhp = 30000;

	/* New maximum hitpoints */
	if (p_ptr->mhp != mhp)
	{
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
	int i;
	object_type *o_ptr;
	u32b flgs[TR_FLAG_SIZE];

	/* Assume no light */
	p_ptr->cur_lite = 0;

	/* Loop through all wielded items */
	for (i = INVEN_RARM; i < INVEN_TOTAL; i++)
	{
		o_ptr = &inventory[i];

		/* Examine actual lites */
		if ((i == INVEN_LITE) && (o_ptr->k_idx) && (o_ptr->tval == TV_LITE))
		{
			if (o_ptr->name2 == EGO_LITE_DARKNESS)
			{
				if (o_ptr->sval == SV_LITE_TORCH)
				{
					p_ptr->cur_lite -= 1;
				}

				/* Lanterns (with fuel) provide more lite */
				else if (o_ptr->sval == SV_LITE_LANTERN)
				{
					p_ptr->cur_lite -= 2;
				}

				else if (o_ptr->sval == SV_LITE_FEANOR)
				{
					p_ptr->cur_lite -= 3;
				}
			}
			/* Torches (with fuel) provide some lite */
			else if ((o_ptr->sval == SV_LITE_TORCH) && (o_ptr->xtra4 > 0))
			{
				p_ptr->cur_lite += 1;
			}

			/* Lanterns (with fuel) provide more lite */
			else if ((o_ptr->sval == SV_LITE_LANTERN) && (o_ptr->xtra4 > 0))
			{
				p_ptr->cur_lite += 2;
			}

			else if ((o_ptr->sval == SV_LITE_FEANOR) || ((o_ptr->sval == SV_LITE_EMPTY) && have_flag(o_ptr->art_flags, TR_LITE)))
			{
				p_ptr->cur_lite += 2;
			}

			/* Artifact Lites provide permanent, bright, lite */
			else if (artifact_p(o_ptr) || (o_ptr->sval == SV_LITE_MAGICAL_LAMP))
			{
				p_ptr->cur_lite += 3;
			}

			if (o_ptr->name2 == EGO_LITE_SHINE) p_ptr->cur_lite++;
		}
		else
		{
			/* Skip empty slots */
			if (!o_ptr->k_idx) continue;

			/* Extract the flags */
			object_flags(o_ptr, flgs);

			/* does this item glow? */
			if (have_flag(flgs, TR_LITE))
			{
				if ((o_ptr->name2 == EGO_DARK) || (o_ptr->name1 == ART_NIGHT)) p_ptr->cur_lite--;
				else p_ptr->cur_lite++;
			}
		}

	}

	/* max radius is 14 (was 5) without rewriting other code -- */
	/* see cave.c:update_lite() and defines.h:LITE_MAX */

	/*
	 * check if the player doesn't have light radius, 
	 * but does weakly glow as an intrinsic.
	 */
	if (p_ptr->cur_lite <= 0 && p_ptr->lite) p_ptr->cur_lite++;

	if (p_ptr->cur_lite > 14) p_ptr->cur_lite = 14;
	if (p_ptr->cur_lite < 0) p_ptr->cur_lite = 0;

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



static void calc_gold(void)
{
	int i;
	s32b diff;

	/* Some gold lost */
	if (p_ptr->au_sum < p_ptr->old_au_sum)
	{
		diff = p_ptr->old_au_sum - p_ptr->au_sum;

		/* Low class golds are first lost */
		for (i = 0; i <= MAX_GOLD; i++)
		{
			if (p_ptr->au[i] >= diff)
			{
				p_ptr->au[i] -= diff;
				break;
			}
			else
			{
				diff -= p_ptr->au[i];
				p_ptr->au[i] = 0;
			}
		}
	}

	/* Get some gold */
	else if (p_ptr->au_sum > p_ptr->old_au_sum)
	{
		p_ptr->au[SV_GOLD_NOTE] += p_ptr->au_sum - p_ptr->old_au_sum;
	}

	/* Recalculate */
	p_ptr->au_sum = 0;
	for (i = MAX_GOLD; i >= 0; i--)
	{
		/* Prevent overflow */
		if ((PY_MAX_GOLD - p_ptr->au_sum) < p_ptr->au[i]) p_ptr->au[i] = (PY_MAX_GOLD - p_ptr->au_sum);
		if (p_ptr->au[i] < 0) p_ptr->au[i] = 0;
		p_ptr->au_sum += p_ptr->au[i];
	}

	/* Save old value */
	p_ptr->old_au_sum = p_ptr->au_sum;
}



/*
 * Computes current weight limit.
 */
static int weight_limit(void)
{
	int i;

	/* Weight limit based only on strength */
	i = adj_str_wgt[p_ptr->stat_ind[A_STR]] * 100;
	if (p_ptr->pclass == CLASS_TERRORKNIGHT) i = i*3/2;

	/* Return the result */
	return (i);
}


bool buki_motteruka(int i)
{
	return ((inventory[i].k_idx && inventory[i].tval >= TV_DIGGING && inventory[i].tval <= TV_SWORD) ? TRUE : FALSE);
}


#ifdef JP
#undef strchr
#define strchr strchr_j
#endif


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
	bool            old_telepathy;
	bool            old_esp_dragon;
	bool            old_see_inv;
	bool            old_wraith_form;
	int             old_dis_ac;
	int             old_dis_to_a;
	int             extra_blows[2];
	int             extra_shots;
	object_type     *o_ptr;
	object_kind     *k_ptr;
	u32b            flgs[TR_FLAG_SIZE];
	bool            omoi = FALSE;
	bool            down_saving = FALSE;
	bool            easy_2weapon = FALSE;
	bool            resist_magic = FALSE;
	byte            empty_hands_status;
	bool            dual_bare_hand = FALSE;
	cexp_info_type  *cexp_ptr = &p_ptr->cexp_info[p_ptr->pclass];


	/* Save the old speed */
	old_speed = p_ptr->pspeed;

	/* Save the old vision stuff */
	old_telepathy = p_ptr->telepathy;
	old_esp_dragon = p_ptr->esp_dragon;
	old_see_inv = p_ptr->see_inv;
	old_wraith_form = WRAITH_FORM();

	/* Save the old armor class */
	old_dis_ac = p_ptr->dis_ac;
	old_dis_to_a = p_ptr->dis_to_a;


	/* Clear extra blows/shots */
	extra_blows[0] = extra_blows[1] = extra_shots = 0;

	/* Clear the stat modifiers */
	for (i = 0; i < A_MAX; i++) p_ptr->stat_add[i] = 0;


	/* Clear the Extra Dice Bonuses */
	p_ptr->to_dd[0] = p_ptr->to_ds[0] = 0;
	p_ptr->to_dd[1] = p_ptr->to_ds[1] = 0;

	/* Clear the Displayed/Real armor class */
	p_ptr->dis_ac = p_ptr->ac = 0;

	/* Clear the Displayed/Real Bonuses */
	p_ptr->dis_to_h[0] = p_ptr->to_h[0] = 0;
	p_ptr->dis_to_h[1] = p_ptr->to_h[1] = 0;
	p_ptr->dis_to_d[0] = p_ptr->to_d[0] = 0;
	p_ptr->dis_to_d[1] = p_ptr->to_d[1] = 0;
	p_ptr->dis_to_h_b = p_ptr->to_h_b = 0;
	p_ptr->dis_to_a = p_ptr->to_a = 0;
	p_ptr->to_h_m = 0;
	p_ptr->to_d_m = 0;

	p_ptr->to_m_chance = 0;

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

	/* Reset the anti-magic field radius */
	p_ptr->anti_magic_field = 0;

	/* Clear all the flags */
	p_ptr->cursed = 0L;
	p_ptr->dis_xtra_might = p_ptr->xtra_might = FALSE;
	p_ptr->impact[0] = FALSE;
	p_ptr->impact[1] = FALSE;
	p_ptr->pass_wall = FALSE;
	p_ptr->wraith_form_perm = FALSE;
	p_ptr->kill_wall = FALSE;
	p_ptr->dec_mana = FALSE;
	p_ptr->easy_spell = FALSE;
	p_ptr->heavy_spell = FALSE;
	p_ptr->ogre_equip = FALSE;
	p_ptr->smell_equip = FALSE;
	p_ptr->evil_equip = FALSE;
	p_ptr->see_inv = FALSE;
	p_ptr->free_act = FALSE;
	p_ptr->slow_digest = FALSE;
	p_ptr->no_digest = FALSE;
	p_ptr->regenerate = FALSE;
	p_ptr->regenerate_mana = FALSE;
	p_ptr->can_swim = FALSE;
	p_ptr->ffall = FALSE;
	p_ptr->hold_life = FALSE;
	p_ptr->mermaid_in_water = FALSE;
	p_ptr->telepathy = FALSE;
	p_ptr->esp_dragon = FALSE;
	p_ptr->lite = FALSE;
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
	p_ptr->resist_stone = FALSE;
	p_ptr->resist_blind = FALSE;
	p_ptr->resist_neth = FALSE;
	p_ptr->resist_time = FALSE;
	p_ptr->resist_fear = FALSE;
	p_ptr->resist_water = FALSE;
	p_ptr->weak_fire = FALSE;
	p_ptr->weak_aqua = FALSE;
	p_ptr->weak_wind = FALSE;
	p_ptr->weak_earth = FALSE;
	p_ptr->reflect = FALSE;
	p_ptr->sh_fire = FALSE;
	p_ptr->sh_elec = FALSE;
	p_ptr->sh_cold = FALSE;
	p_ptr->anti_magic = FALSE;
	p_ptr->anti_tele = FALSE;
	p_ptr->fear_field = FALSE;
	p_ptr->warning = FALSE;
	p_ptr->dis_mighty_throw = p_ptr->mighty_throw = FALSE;
	p_ptr->see_dark_grid = FALSE;

	p_ptr->immune_acid = FALSE;
	p_ptr->immune_elec = FALSE;
	p_ptr->immune_fire = FALSE;
	p_ptr->immune_cold = FALSE;

	p_ptr->ryoute = FALSE;
	p_ptr->migite = FALSE;
	p_ptr->hidarite = FALSE;
	p_ptr->no_flowed = FALSE;

	/* Initialize alignment (LNC) */
	p_ptr->align = p_ptr->align_self + friend_align_lnc;

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

	empty_hands_status = empty_hands();

	if (buki_motteruka(INVEN_RARM) && (empty_hands_status & EMPTY_HAND_LARM) &&
		((inventory[INVEN_RARM].weight > 99) || (inventory[INVEN_RARM].tval == TV_POLEARM)) &&
		(!p_ptr->riding || (p_ptr->pet_extra_flags & PF_RYOUTE))) p_ptr->ryoute = TRUE;
	if (buki_motteruka(INVEN_RARM) || !buki_motteruka(INVEN_LARM)) p_ptr->migite = TRUE;
	if (buki_motteruka(INVEN_LARM)) p_ptr->hidarite = TRUE;

	switch (p_ptr->pclass)
	{
		case CLASS_TERRORKNIGHT:
			p_ptr->sustain_str = TRUE;
			p_ptr->sustain_dex = TRUE;
			p_ptr->sustain_con = TRUE;
			p_ptr->hold_life = TRUE;
			if (cexp_ptr->clev > 14)
			{
				p_ptr->regenerate = TRUE;
				p_ptr->free_act = TRUE;
				p_ptr->fear_field = TRUE;
			}
			if (cexp_ptr->clev > 24)
			{
				p_ptr->resist_fear = TRUE;
				p_ptr->anti_magic_field += 3;
			}
			if (cexp_ptr->clev > 34)
			{
				p_ptr->resist_conf = TRUE;
				p_ptr->anti_magic_field++;
			}
			if (cexp_ptr->clev > 44)
			{
				p_ptr->resist_neth = TRUE;
				p_ptr->anti_magic_field++;
			}
			break;
		case CLASS_SWORDMASTER:
			easy_2weapon = TRUE;
			for (i = 0; i < 2; i++)
			{
				/* Examine the "main weapon" */
				o_ptr = &inventory[INVEN_RARM + i];
				k_ptr = &k_info[o_ptr->k_idx];
				if (!buki_motteruka(INVEN_RARM + i)) continue;
				if (!(weapon_type_bit(get_weapon_type(k_ptr)) & (WT_BIT_SMALL_SWORD | WT_BIT_KATANA | WT_BIT_SWORD | WT_BIT_GREAT_SWORD)))
					easy_2weapon = FALSE;
			}
			break;
		case CLASS_DRAGOON:
			if (cexp_ptr->clev > 24) p_ptr->esp_dragon = TRUE;
			break;
		case CLASS_NINJA:
		case CLASS_NINJAMASTER:
			p_ptr->see_dark_grid = TRUE;
			if (heavy_armor())
			{
				p_ptr->pspeed -= cexp_ptr->clev / 10;
				p_ptr->skill_stl -= cexp_ptr->clev / 10;
			}
			break;
		case CLASS_EXORCIST:
			if (cexp_ptr->clev > 39) p_ptr->resist_fear = TRUE;
			break;
		case CLASS_WITCH:
			if (cexp_ptr->clev > 9) p_ptr->ffall = TRUE;
			break;
		case CLASS_VAMPIRE:
			p_ptr->resist_dark = TRUE;
			p_ptr->ffall = TRUE;
			p_ptr->lite = TRUE;
			if (cexp_ptr->clev > 39) p_ptr->resist_lite = TRUE;
			if (!is_daytime())
			{
				p_ptr->regenerate = TRUE;
				p_ptr->resist_fear = TRUE;
				for (i = 0; i < A_MAX; i++) p_ptr->stat_add[i] += cexp_ptr->clev / 10;
				if (cexp_ptr->clev > 14) p_ptr->fear_field = TRUE;
				if (cexp_ptr->clev > 39) resist_magic = TRUE;
			}
			else if (cexp_ptr->clev < 40)
			{
				for (i = 0; i < A_MAX; i++) p_ptr->stat_add[i] -= 220;
				p_ptr->to_a += 200;
				p_ptr->dis_to_a += 200;
				p_ptr->pspeed -= 15;
				p_ptr->skill_stl -= 20 - cexp_ptr->clev / 10;
			}
		case CLASS_LICH:
			p_ptr->resist_cold = TRUE;
			p_ptr->resist_pois = TRUE;
			p_ptr->resist_neth = TRUE;
			p_ptr->free_act = TRUE;
			p_ptr->see_inv = TRUE;
			p_ptr->hold_life = TRUE;
			break;
		case CLASS_ANGELKNIGHT:
			p_ptr->ffall = TRUE;
			if (cexp_ptr->clev > 9) p_ptr->resist_fear = TRUE;
			if (cexp_ptr->clev > 19) p_ptr->sustain_wis = TRUE;
			if (cexp_ptr->clev > 29) p_ptr->resist_conf = TRUE;
			if (cexp_ptr->clev > 39) p_ptr->telepathy = TRUE;
			break;
		case CLASS_HIGHWITCH:
			if (cexp_ptr->clev > 9) p_ptr->ffall = TRUE;
			break;
		case CLASS_GUNNER:
			p_ptr->resist_sound = TRUE;
			p_ptr->resist_shard = TRUE;
			p_ptr->anti_tele = TRUE;
			p_ptr->skill_dig += (cexp_ptr->clev / 8);
			p_ptr->see_dark_grid = TRUE;
			break;
		case CLASS_LORD:
			if (p_ptr->action == ACTION_AURA)
			{
				p_ptr->lite = TRUE;
				p_ptr->regenerate = TRUE;
				if (cexp_ptr->clev > 29) p_ptr->telepathy = TRUE;
			}
			break;
		case CLASS_CRESCENT:
			if (cexp_ptr->clev > 24)
			{
				p_ptr->anti_magic_field += 3;
			}
			if (cexp_ptr->clev > 34)
			{
				p_ptr->anti_magic_field++;
			}
			if (cexp_ptr->clev > 39)
			{
				if (get_weapon_type(&k_info[inventory[INVEN_BOW].k_idx]) == WT_BOW) p_ptr->xtra_might = p_ptr->dis_xtra_might = TRUE;
			}
			if (cexp_ptr->clev > 44)
			{
				p_ptr->anti_magic_field++;
			}
			break;
	}

	if (p_ptr->pclass != CLASS_TERRORKNIGHT)
	{
		if (p_ptr->cexp_info[CLASS_TERRORKNIGHT].clev > 24)
		{
			p_ptr->regenerate = TRUE;
			p_ptr->free_act = TRUE;
			p_ptr->fear_field = TRUE;
		}
		if (p_ptr->cexp_info[CLASS_TERRORKNIGHT].clev > 44)
		{
			p_ptr->resist_fear = TRUE;
			p_ptr->anti_magic_field += 3;
		}
	}

	if (p_ptr->pclass != CLASS_SWORDMASTER)
	{
		if (p_ptr->cexp_info[CLASS_SWORDMASTER].clev > 49)
		{
			easy_2weapon = TRUE;
			for (i = 0; i < 2; i++)
			{
				/* Examine the "main weapon" */
				o_ptr = &inventory[INVEN_RARM + i];
				k_ptr = &k_info[o_ptr->k_idx];
				if (!buki_motteruka(INVEN_RARM + i)) continue;
				if (!(weapon_type_bit(get_weapon_type(k_ptr)) & (WT_BIT_SMALL_SWORD | WT_BIT_KATANA | WT_BIT_SWORD | WT_BIT_GREAT_SWORD)))
					easy_2weapon = FALSE;
			}
		}
	}

	if (p_ptr->pclass != CLASS_DRAGOON)
		if (p_ptr->cexp_info[CLASS_DRAGOON].clev > 44) p_ptr->esp_dragon = TRUE;

	if ((p_ptr->cexp_info[CLASS_WITCH].clev > 29) || (p_ptr->cexp_info[CLASS_HIGHWITCH].clev > 29)) p_ptr->ffall = TRUE;

	if ((p_ptr->cexp_info[CLASS_NINJA].clev > 49) || (p_ptr->cexp_info[CLASS_GUNNER].clev > 49) || (p_ptr->cexp_info[CLASS_NINJAMASTER].clev > 29)) p_ptr->see_dark_grid = TRUE;

	if ((rp_ptr->r_flags & PRF_NO_DIGEST) || (cp_ptr->c_flags & PCF_NO_DIGEST)) p_ptr->no_digest = TRUE;

	p_ptr->to_a += cp_ptr->c_to_a;
	p_ptr->dis_to_a += cp_ptr->c_to_a;

	/* Base skill -- speed */
	p_ptr->pspeed += rp_ptr->r_spd + cp_ptr->c_spd;

	/* Affect speed (Level, by Class) */
	p_ptr->pspeed += ((p_ptr->gx_spd * (p_ptr->lev + cexp_ptr->clev)) / (p_ptr->max_plv + cexp_ptr->max_clev)) / 50;

	/***** Races ****/
	switch (p_ptr->prace)
	{
		case RACE_HAWKMAN:
			p_ptr->ffall = TRUE;
			if (p_ptr->lev > 14) p_ptr->resist_fear = TRUE;
			break;
		case RACE_LIZARDMAN:
			p_ptr->resist_acid = TRUE;
			p_ptr->resist_cold = TRUE;
			break;
		case RACE_FAIRY:
			p_ptr->resist_lite = TRUE;
			p_ptr->ffall = TRUE;
			break;
		case RACE_GREMLIN:
			p_ptr->resist_dark = TRUE;
			p_ptr->resist_neth = TRUE;
			p_ptr->ffall = TRUE;
			break;
		case RACE_SKELETON:
			p_ptr->resist_pois = TRUE;
			p_ptr->resist_shard = TRUE;
			p_ptr->see_inv = TRUE;
			p_ptr->hold_life = TRUE;
			if (p_ptr->lev > 9) p_ptr->resist_cold = TRUE;
			break;
		case RACE_GHOST:
			p_ptr->resist_cold = TRUE;
			p_ptr->resist_pois = TRUE;
			p_ptr->resist_neth = TRUE;
			p_ptr->free_act = TRUE;
			p_ptr->see_inv = TRUE;
			p_ptr->hold_life = TRUE;
			p_ptr->ffall = TRUE;
			p_ptr->pass_wall = TRUE;
			if (p_ptr->lev > 34) p_ptr->telepathy = TRUE;
			break;
		case RACE_PUMPKINHEAD:
			p_ptr->resist_chaos = TRUE;
			p_ptr->resist_disen = TRUE;
			p_ptr->free_act = TRUE;
			p_ptr->hold_life = TRUE;
			p_ptr->slow_digest = TRUE;
			if (p_ptr->lev > 19) p_ptr->telepathy = TRUE;
			break;
		case RACE_GOBLIN:
			p_ptr->resist_dark = TRUE;
			break;
		case RACE_GORGON:
			p_ptr->resist_stone = TRUE;
			if (p_ptr->lev > 9) p_ptr->resist_cold = TRUE;
			if (p_ptr->lev > 19) p_ptr->resist_acid = TRUE;
			if (p_ptr->lev > 29) p_ptr->resist_chaos = TRUE;
			if (p_ptr->lev > 39) p_ptr->resist_pois = TRUE;
			if (p_ptr->lev > 49) p_ptr->resist_neth = TRUE;
			break;
		case RACE_MERMAID:
			p_ptr->resist_acid = TRUE;
			p_ptr->resist_water = TRUE;
			p_ptr->can_swim = TRUE;
			if (p_ptr->lev > 24) p_ptr->resist_conf = TRUE;
			break;
		default:
			/* Do nothing */
			;
	}

	/* Temporary shield */
	if (p_ptr->shield || p_ptr->magicdef)
	{
		p_ptr->to_a += 50;
		p_ptr->dis_to_a += 50;
	}

	if (p_ptr->shield)
	{
		p_ptr->resist_stone = TRUE;
	}
	if (p_ptr->tim_sh_fire)
	{
		p_ptr->sh_fire = TRUE;
	}
	if (p_ptr->tim_sh_elec)
	{
		p_ptr->sh_elec = TRUE;
	}
	if (p_ptr->tim_sh_cold)
	{
		p_ptr->sh_cold = TRUE;
	}
	if (p_ptr->tim_res_time)
	{
		p_ptr->resist_time = TRUE;
	}
	if (p_ptr->zoshonel_protect)
	{
		p_ptr->immune_fire = TRUE;
		p_ptr->sh_fire = TRUE;
		p_ptr->stat_add[A_STR] += 4;
		p_ptr->stat_add[A_DEX] += 4;
		p_ptr->pspeed += 4;
	}

	if (prace_is_(RACE_PUMPKINHEAD)) p_ptr->cursed |= (TRC_AGGRAVATE);

	if (easy_band)
	{
		p_ptr->resist_blind = TRUE;
		p_ptr->resist_conf  = TRUE;
		p_ptr->hold_life = TRUE;
		if (p_ptr->pclass != CLASS_NINJA) p_ptr->lite = TRUE;
	}

	if (p_ptr->riding)
	{
		if (!(r_info[m_list[p_ptr->riding].r_idx].flags2 & RF2_PASS_WALL))
			p_ptr->pass_wall = FALSE;
		if (r_info[m_list[p_ptr->riding].r_idx].flags2 & RF2_KILL_WALL)
			p_ptr->pass_wall = TRUE;
	}

	if (p_ptr->kill_wall) p_ptr->pass_wall = TRUE;

	/* Hack -- apply racial/class stat maxes */
	/* Apply the racial modifiers */
	for (i = 0; i < A_MAX; i++)
	{
		/* Modify the stats for "race" */
		p_ptr->stat_add[i] += rp_ptr->r_adj[i];
	}


	/* I'm adding the mutations here for the lack of a better place... */
	if (p_ptr->muta3)
	{
		/* Hyper Strength */
		if (p_ptr->muta3 & MUT3_HYPER_STR)
		{
			p_ptr->stat_add[A_STR] += 4;
		}

		/* Puny */
		if (p_ptr->muta3 & MUT3_PUNY)
		{
			p_ptr->stat_add[A_STR] -= 4;
		}

		/* Living computer */
		if (p_ptr->muta3 & MUT3_HYPER_INT)
		{
			p_ptr->stat_add[A_INT] += 4;
			p_ptr->stat_add[A_WIS] += 4;
		}

		/* Moronic */
		if (p_ptr->muta3 & MUT3_MORONIC)
		{
			p_ptr->stat_add[A_INT] -= 4;
			p_ptr->stat_add[A_WIS] -= 4;
		}

		if (p_ptr->muta3 & MUT3_RESILIENT)
		{
			p_ptr->stat_add[A_CON] += 4;
		}

		if (p_ptr->muta3 & MUT3_XTRA_FAT)
		{
			p_ptr->stat_add[A_CON] += 2;
			p_ptr->pspeed -= 2;
		}

		if (p_ptr->muta3 & MUT3_ALBINO)
		{
			p_ptr->stat_add[A_CON] -= 4;
		}

		if (p_ptr->muta3 & MUT3_FLESH_ROT)
		{
			p_ptr->stat_add[A_CON] -= 2;
			p_ptr->stat_add[A_CHR] -= 1;
			p_ptr->regenerate = FALSE;
			/* Cancel innate regeneration */
		}

		if (p_ptr->muta3 & MUT3_SILLY_VOI)
		{
			p_ptr->stat_add[A_CHR] -= 4;
		}

		if (p_ptr->muta3 & MUT3_BLANK_FAC)
		{
			p_ptr->stat_add[A_CHR] -= 1;
		}

		if (p_ptr->muta3 & MUT3_XTRA_EYES)
		{
			p_ptr->skill_fos += 15;
			p_ptr->skill_srh += 15;
		}

		if (p_ptr->muta3 & MUT3_MAGIC_RES)
		{
			p_ptr->skill_sav += (15 + (p_ptr->lev / 5));
		}

		if (p_ptr->muta3 & MUT3_XTRA_NOIS)
		{
			p_ptr->skill_stl -= 3;
		}

		if (p_ptr->muta3 & MUT3_INFRAVIS)
		{
			p_ptr->see_infra += 3;
		}

		if (p_ptr->muta3 & MUT3_XTRA_LEGS)
		{
			p_ptr->pspeed += 3;
		}

		if (p_ptr->muta3 & MUT3_SHORT_LEG)
		{
			p_ptr->pspeed -= 3;
		}

		if (p_ptr->muta3 & MUT3_ELEC_TOUC)
		{
			p_ptr->sh_elec = TRUE;
		}

		if (p_ptr->muta3 & MUT3_FIRE_BODY)
		{
			p_ptr->sh_fire = TRUE;
			p_ptr->lite = TRUE;
		}

		if (p_ptr->muta3 & MUT3_WART_SKIN)
		{
			p_ptr->stat_add[A_CHR] -= 2;
			p_ptr->to_a += 5;
			p_ptr->dis_to_a += 5;
		}

		if (p_ptr->muta3 & MUT3_SCALES)
		{
			p_ptr->stat_add[A_CHR] -= 1;
			p_ptr->to_a += 10;
			p_ptr->dis_to_a += 10;
		}

		if (p_ptr->muta3 & MUT3_IRON_SKIN)
		{
			p_ptr->stat_add[A_DEX] -= 1;
			p_ptr->to_a += 25;
			p_ptr->dis_to_a += 25;
		}

		if (p_ptr->muta3 & MUT3_WINGS)
		{
			p_ptr->ffall = TRUE;
		}

		if (p_ptr->muta3 & MUT3_FEARLESS)
		{
			p_ptr->resist_fear = TRUE;
		}

		if (p_ptr->muta3 & MUT3_REGEN)
		{
			p_ptr->regenerate = TRUE;
		}

		if (p_ptr->muta3 & MUT3_ESP)
		{
			p_ptr->telepathy = TRUE;
		}

		if (p_ptr->muta3 & MUT3_LIMBER)
		{
			p_ptr->stat_add[A_DEX] += 3;
		}

		if (p_ptr->muta3 & MUT3_ARTHRITIS)
		{
			p_ptr->stat_add[A_DEX] -= 3;
		}

		if (p_ptr->muta3 & MUT3_MOTION)
		{
			p_ptr->free_act = TRUE;
			p_ptr->skill_stl += 1;
		}

		if (p_ptr->muta3 & MUT3_ILL_NORM)
		{
			p_ptr->stat_add[A_CHR] = 0;
		}
	}

	if (p_ptr->chargespell)
	{
		p_ptr->stat_add[A_INT] += 4;
		p_ptr->stat_add[A_WIS] += 4;
		p_ptr->regenerate_mana = TRUE;
	}

	/* Scan the usable inventory */
	for (i = INVEN_RARM; i < INVEN_TOTAL; i++)
	{
		int bonus_to_h, bonus_to_d;

		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Extract the item flags */
		object_flags(o_ptr, flgs);

		p_ptr->cursed |= (o_ptr->curse_flags & (0xFFFFFFF0L));

		/* Affect stats */
		if (have_flag(flgs, TR_STR)) p_ptr->stat_add[A_STR] += o_ptr->to_stat[A_STR];
		if (have_flag(flgs, TR_INT)) p_ptr->stat_add[A_INT] += o_ptr->to_stat[A_INT];
		if (have_flag(flgs, TR_WIS)) p_ptr->stat_add[A_WIS] += o_ptr->to_stat[A_WIS];
		if (have_flag(flgs, TR_DEX)) p_ptr->stat_add[A_DEX] += o_ptr->to_stat[A_DEX];
		if (have_flag(flgs, TR_CON)) p_ptr->stat_add[A_CON] += o_ptr->to_stat[A_CON];
		if (have_flag(flgs, TR_CHR)) p_ptr->stat_add[A_CHR] += o_ptr->to_stat[A_CHR];

		if (have_flag(flgs, TR_MAGIC_MASTERY)) p_ptr->skill_dev += 8 * o_ptr->to_misc[OB_MAGIC_MASTERY];

		/* Affect stealth */
		if (have_flag(flgs, TR_STEALTH)) p_ptr->skill_stl += o_ptr->to_misc[OB_STEALTH];

		if (have_flag(flgs, TR_SEARCH))
		{
			/* Affect searching ability (factor of five) */
			p_ptr->skill_srh += (o_ptr->to_misc[OB_SEARCH] * 5);

			/* Affect searching frequency (factor of five) */
			p_ptr->skill_fos += (o_ptr->to_misc[OB_SEARCH] * 5);
		}

		/* Affect infravision */
		if (have_flag(flgs, TR_INFRA)) p_ptr->see_infra += o_ptr->to_misc[OB_INFRA];

		/* Affect digging (factor of 20) */
		if (have_flag(flgs, TR_TUNNEL)) p_ptr->skill_dig += (o_ptr->to_misc[OB_TUNNEL] * 20);

		/* Affect speed */
		if (have_flag(flgs, TR_SPEED)) p_ptr->pspeed += o_ptr->to_misc[OB_SPEED];

		/* Affect blows */
		if (have_flag(flgs, TR_BLOWS))
		{
			if ((i == INVEN_RARM || i == INVEN_RIGHT) && !(p_ptr->ryoute || (empty_hands_status & EMPTY_HAND_RARM))) extra_blows[0] += o_ptr->to_misc[OB_BLOWS];
			else if((i == INVEN_LARM || i == INVEN_LEFT) && !(p_ptr->ryoute || (empty_hands_status & EMPTY_HAND_RARM))) extra_blows[1] += o_ptr->to_misc[OB_BLOWS];
			else {extra_blows[0] += o_ptr->to_misc[OB_BLOWS]; extra_blows[1] += o_ptr->to_misc[OB_BLOWS];}
		}

		/* Affect anti-magic field radius */
		if (have_flag(flgs, TR_ANTI_MAGIC)) p_ptr->anti_magic_field += o_ptr->to_misc[OB_ANTI_MAGIC];

		/* Hack -- cause earthquakes */
		if (have_flag(flgs, TR_IMPACT)) p_ptr->impact[(i == INVEN_RARM) ? 0 : 1] = TRUE;

		/* Boost shots */
		if (have_flag(flgs, TR_XTRA_SHOTS)) extra_shots++;

		/* Various flags */
		if (have_flag(flgs, TR_AGGRAVATE))   p_ptr->cursed |= TRC_AGGRAVATE;
		if (have_flag(flgs, TR_DRAIN_EXP))   p_ptr->cursed |= TRC_DRAIN_EXP;
		if (have_flag(flgs, TR_TY_CURSE))    p_ptr->cursed |= TRC_TY_CURSE;
		if (have_flag(flgs, TR_DEC_MANA))    p_ptr->dec_mana = TRUE;
		if (have_flag(flgs, TR_XTRA_MIGHT))
		{
			p_ptr->xtra_might = TRUE;
			if (object_known_p(o_ptr)) p_ptr->dis_xtra_might = TRUE;
		}
		if (have_flag(flgs, TR_SLOW_DIGEST)) p_ptr->slow_digest = TRUE;
		if (have_flag(flgs, TR_REGEN))       p_ptr->regenerate = TRUE;
		if (have_flag(flgs, TR_REGEN_MANA))  p_ptr->regenerate_mana = TRUE;
		if (have_flag(flgs, TR_TELEPATHY))   p_ptr->telepathy = TRUE;
		if (have_flag(flgs, TR_SEE_INVIS))   p_ptr->see_inv = TRUE;
		if (have_flag(flgs, TR_FEATHER))     p_ptr->ffall = TRUE;
		if (have_flag(flgs, TR_FREE_ACT))    p_ptr->free_act = TRUE;
		if (have_flag(flgs, TR_HOLD_LIFE))   p_ptr->hold_life = TRUE;
		if (have_flag(flgs, TR_FEAR_FIELD))  p_ptr->fear_field = TRUE;
		if (have_flag(flgs, TR_WRAITH))      p_ptr->wraith_form_perm = TRUE;
		if (have_flag(flgs, TR_WARNING)){
			if (!o_ptr->inscription || !(strchr(quark_str(o_ptr->inscription),'$')))
			  p_ptr->warning = TRUE;
		}

		if (have_flag(flgs, TR_TELEPORT))
		{
			if (cursed_p(o_ptr)) p_ptr->cursed |= TRC_TELEPORT;
			else
			{
				cptr insc = quark_str(o_ptr->inscription);

				if (o_ptr->inscription &&
				    (strchr(insc, '.') || strchr(insc, '%')))
				{
					/*
					 * {.} will stop random teleportation.
					 * {%} includes '.' after conversion.
					 */
				}
				else
				{
					/* Controlled random teleportation */
					p_ptr->cursed |= TRC_TELEPORT_SELF;
				}
			}
		}

		/* Immunity flags */
		if (have_flag(flgs, TR_IM_FIRE)) p_ptr->immune_fire = TRUE;
		if (have_flag(flgs, TR_IM_ACID)) p_ptr->immune_acid = TRUE;
		if (have_flag(flgs, TR_IM_COLD)) p_ptr->immune_cold = TRUE;
		if (have_flag(flgs, TR_IM_ELEC)) p_ptr->immune_elec = TRUE;

		/* Resistance flags */
		if (have_flag(flgs, TR_RES_ACID))   p_ptr->resist_acid = TRUE;
		if (have_flag(flgs, TR_RES_ELEC))   p_ptr->resist_elec = TRUE;
		if (have_flag(flgs, TR_RES_FIRE))   p_ptr->resist_fire = TRUE;
		if (have_flag(flgs, TR_RES_COLD))   p_ptr->resist_cold = TRUE;
		if (have_flag(flgs, TR_RES_POIS))   p_ptr->resist_pois = TRUE;
		if (have_flag(flgs, TR_RES_FEAR))   p_ptr->resist_fear = TRUE;
		if (have_flag(flgs, TR_RES_CONF))   p_ptr->resist_conf = TRUE;
		if (have_flag(flgs, TR_RES_SOUND))  p_ptr->resist_sound = TRUE;
		if (have_flag(flgs, TR_RES_LITE))   p_ptr->resist_lite = TRUE;
		if (have_flag(flgs, TR_RES_DARK))   p_ptr->resist_dark = TRUE;
		if (have_flag(flgs, TR_RES_CHAOS))  p_ptr->resist_chaos = TRUE;
		if (have_flag(flgs, TR_RES_DISEN))  p_ptr->resist_disen = TRUE;
		if (have_flag(flgs, TR_RES_SHARDS)) p_ptr->resist_shard = TRUE;
		if (have_flag(flgs, TR_RES_STONE))  p_ptr->resist_stone = TRUE;
		if (have_flag(flgs, TR_RES_BLIND))  p_ptr->resist_blind = TRUE;
		if (have_flag(flgs, TR_RES_NETHER)) p_ptr->resist_neth = TRUE;

		if (have_flag(flgs, TR_REFLECT))   p_ptr->reflect = TRUE;
		if (have_flag(flgs, TR_SH_FIRE))   p_ptr->sh_fire = TRUE;
		if (have_flag(flgs, TR_SH_ELEC))   p_ptr->sh_elec = TRUE;
		if (have_flag(flgs, TR_SH_COLD))   p_ptr->sh_cold = TRUE;
		if (have_flag(flgs, TR_NO_MAGIC))  p_ptr->anti_magic = TRUE;
		if (have_flag(flgs, TR_NO_TELE))   p_ptr->anti_tele = TRUE;
		if (have_flag(flgs, TR_RES_MAGIC)) resist_magic = TRUE;

		/* Sustain flags */
		if (have_flag(flgs, TR_SUST_STR)) p_ptr->sustain_str = TRUE;
		if (have_flag(flgs, TR_SUST_INT)) p_ptr->sustain_int = TRUE;
		if (have_flag(flgs, TR_SUST_WIS)) p_ptr->sustain_wis = TRUE;
		if (have_flag(flgs, TR_SUST_DEX)) p_ptr->sustain_dex = TRUE;
		if (have_flag(flgs, TR_SUST_CON)) p_ptr->sustain_con = TRUE;
		if (have_flag(flgs, TR_SUST_CHR)) p_ptr->sustain_chr = TRUE;

		if (o_ptr->name2 == EGO_RING_RES_TIME) p_ptr->resist_time = TRUE;
		if (o_ptr->name2 == EGO_RING_THROW)
		{
			p_ptr->mighty_throw = TRUE;
			if (object_known_p(o_ptr)) p_ptr->dis_mighty_throw = TRUE;
		}
		if (have_flag(flgs, TR_EASY_SPELL)) p_ptr->easy_spell = TRUE;
		if (o_ptr->name2 == EGO_AMU_FOOL) p_ptr->heavy_spell = TRUE;
		if (o_ptr->name2 == EGO_AMU_NAIVETY) down_saving = TRUE;

		if (o_ptr->curse_flags & TRC_LOW_MAGIC)
		{
			if (o_ptr->curse_flags & TRC_HEAVY_CURSE)
			{
				p_ptr->to_m_chance += 10;
			}
			else
			{
				p_ptr->to_m_chance += 3;
			}
		}

		/* Modify the base armor class */
		p_ptr->ac += o_ptr->ac;

		/* The base armor class is not always known */
		if (object_known_p(o_ptr))
		{
			p_ptr->dis_ac += o_ptr->ac;
		}
		else
		{
			k_ptr = &k_info[o_ptr->k_idx];
			p_ptr->dis_ac += k_ptr->ac;
		}

		/* Apply the bonuses to armor class */
		p_ptr->to_a += o_ptr->to_a;

		/* Apply the mental bonuses to armor class, if known */
		if (object_known_p(o_ptr)) p_ptr->dis_to_a += o_ptr->to_a;

		if (o_ptr->curse_flags & TRC_LOW_MELEE)
		{
			int slot = i - INVEN_RARM;
			if (slot < 2)
			{
				if (o_ptr->curse_flags & TRC_HEAVY_CURSE)
				{
					p_ptr->to_h[slot] -= 15;
					if (o_ptr->ident & IDENT_MENTAL) p_ptr->dis_to_h[slot] -= 15;
				}
				else
				{
					p_ptr->to_h[slot] -= 5;
					if (o_ptr->ident & IDENT_MENTAL) p_ptr->dis_to_h[slot] -= 5;
				}
			}
			else
			{
				if (o_ptr->curse_flags & TRC_HEAVY_CURSE)
				{
					p_ptr->to_h_b -= 15;
					if (o_ptr->ident & IDENT_MENTAL) p_ptr->dis_to_h_b -= 15;
				}
				else
				{
					p_ptr->to_h_b -= 5;
					if (o_ptr->ident & IDENT_MENTAL) p_ptr->dis_to_h_b -= 5;
				}
			}
		}

		if (o_ptr->curse_flags & TRC_LOW_AC)
		{
			if (o_ptr->curse_flags & TRC_HEAVY_CURSE)
			{
				p_ptr->to_a -= 30;
				if (o_ptr->ident & IDENT_MENTAL) p_ptr->dis_to_a -= 30;
			}
			else
			{
				p_ptr->to_a -= 10;
				if (o_ptr->ident & IDENT_MENTAL) p_ptr->dis_to_a -= 10;
			}
		}

		/* Hack -- do not apply "weapon" bonuses */
		if (i == INVEN_RARM && buki_motteruka(i)) continue;
		if (i == INVEN_LARM && buki_motteruka(i)) continue;

		/* Hack -- do not apply "bow" bonuses */
		if (i == INVEN_BOW) continue;

		bonus_to_h = o_ptr->to_h;
		bonus_to_d = o_ptr->to_d;

		/* To Bow and Natural attack */

		/* Apply the bonuses to hit/damage */
		p_ptr->to_h_b += bonus_to_h;
		p_ptr->to_h_m += bonus_to_h;
		p_ptr->to_d_m += bonus_to_d;

		/* Apply the mental bonuses tp hit/damage, if known */
		if (object_known_p(o_ptr)) p_ptr->dis_to_h_b += bonus_to_h;

		/* To Melee */
		if ((i == INVEN_LEFT || i == INVEN_RIGHT) && !p_ptr->ryoute)
		{
			/* Apply the bonuses to hit/damage */
			p_ptr->to_h[i-INVEN_RIGHT] += bonus_to_h;
			p_ptr->to_d[i-INVEN_RIGHT] += bonus_to_d;

			/* Apply the mental bonuses tp hit/damage, if known */
			if (object_known_p(o_ptr))
			{
				p_ptr->dis_to_h[i-INVEN_RIGHT] += bonus_to_h;
				p_ptr->dis_to_d[i-INVEN_RIGHT] += bonus_to_d;
			}
		}
		else if (p_ptr->migite && p_ptr->hidarite)
		{
			/* Apply the bonuses to hit/damage */
			p_ptr->to_h[0] += (bonus_to_h > 0) ? (bonus_to_h+1)/2 : bonus_to_h;
			p_ptr->to_h[1] += (bonus_to_h > 0) ? bonus_to_h/2 : bonus_to_h;
			p_ptr->to_d[0] += (bonus_to_d > 0) ? (bonus_to_d+1)/2 : bonus_to_d;
			p_ptr->to_d[1] += (bonus_to_d > 0) ? bonus_to_d/2 : bonus_to_d;

			/* Apply the mental bonuses tp hit/damage, if known */
			if (object_known_p(o_ptr))
			{
				p_ptr->dis_to_h[0] += (bonus_to_h > 0) ? (bonus_to_h+1)/2 : bonus_to_h;
				p_ptr->dis_to_h[1] += (bonus_to_h > 0) ? bonus_to_h/2 : bonus_to_h;
				p_ptr->dis_to_d[0] += (bonus_to_d > 0) ? (bonus_to_d+1)/2 : bonus_to_d;
				p_ptr->dis_to_d[1] += (bonus_to_d > 0) ? bonus_to_d/2 : bonus_to_d;
			}
		}
		else
		{
			/* Apply the bonuses to hit/damage */
			p_ptr->to_h[0] += bonus_to_h;
			p_ptr->to_d[0] += bonus_to_d;

			/* Apply the mental bonuses tp hit/damage, if known */
			if (object_known_p(o_ptr))
			{
				p_ptr->dis_to_h[0] += bonus_to_h;
				p_ptr->dis_to_d[0] += bonus_to_d;
			}
		}
	}

	if (inventory[INVEN_OUTER].k_idx && (inventory[INVEN_OUTER].tval == TV_CLOAK) && (inventory[INVEN_OUTER].sval == SV_RAINCOAT))
		p_ptr->resist_water = TRUE;

	if (inventory[INVEN_OUTER].k_idx && (inventory[INVEN_OUTER].name2 == EGO_BAT))
		p_ptr->see_dark_grid = TRUE;

	if (inventory[INVEN_OUTER].k_idx && (inventory[INVEN_OUTER].name2 == EGO_NO_ELEM))
	{
		p_ptr->no_elem = TRUE;
		init_realm_table();
	}

	if (inventory[INVEN_NECK].k_idx && (inventory[INVEN_NECK].name1 == ART_AMU_FIRE))
		p_ptr->weak_aqua = TRUE;

	if (inventory[INVEN_NECK].k_idx && (inventory[INVEN_NECK].name1 == ART_AMU_AQUA))
	{
		p_ptr->resist_water = TRUE;
		p_ptr->weak_fire = TRUE;
	}

	if (inventory[INVEN_NECK].k_idx && (inventory[INVEN_NECK].name1 == ART_AMU_WIND))
		p_ptr->weak_earth = TRUE;

	if (inventory[INVEN_NECK].k_idx && (inventory[INVEN_NECK].name1 == ART_AMU_EARTH))
		p_ptr->weak_wind = TRUE;

	if (p_ptr->cursed & TRC_TELEPORT) p_ptr->cursed &= ~(TRC_TELEPORT_SELF);

	/* Hack -- aura of fire also provides light */
	if (p_ptr->sh_fire) p_ptr->lite = TRUE;

	if (p_ptr->anti_magic_field > MAX_RANGE) p_ptr->anti_magic_field = MAX_RANGE;
	if (p_ptr->anti_magic_field < 0) p_ptr->anti_magic_field = 0;

	/*** Mega-Hack - scan the artifacts ***/

	/* Equipments of Death */
	if (((inventory[INVEN_RARM].k_idx && (inventory[INVEN_RARM].name1 == ART_DAGDA))
	        || (inventory[INVEN_LARM].k_idx && (inventory[INVEN_LARM].name1 == ART_DAGDA)))
	    && ((inventory[INVEN_LEFT].k_idx && (inventory[INVEN_LEFT].name1 == ART_EVIL_RING))
	        || (inventory[INVEN_RIGHT].k_idx && (inventory[INVEN_RIGHT].name1 == ART_EVIL_RING)))
	    && (inventory[INVEN_BODY].k_idx && (inventory[INVEN_BODY].name1 == ART_DEATH))
	    && (inventory[INVEN_HEAD].k_idx && (inventory[INVEN_HEAD].name1 == ART_SKULL_MASK)))
	{
		p_ptr->to_a += 100;
		p_ptr->dis_to_a += 100;
		p_ptr->evil_equip = TRUE;
	}

	/* Equipments of Smell */

	if (((inventory[INVEN_RARM].k_idx && (inventory[INVEN_RARM].name1 == ART_WARRIOR_SWORD))
	        || (inventory[INVEN_LARM].k_idx && (inventory[INVEN_LARM].name1 == ART_WARRIOR_SWORD)))
	    && (inventory[INVEN_BODY].k_idx && (inventory[INVEN_BODY].name1 == ART_WARRIOR_ARMOR))
	    && (inventory[INVEN_HEAD].k_idx && (inventory[INVEN_HEAD].name1 == ART_WARRIOR_HELM))
	    && (inventory[INVEN_HANDS].k_idx && (inventory[INVEN_HANDS].name1 == ART_WARRIOR_GLOVE)))
	{
		p_ptr->smell_equip = TRUE;
	}

	/* Equipments of Ogre */
	if ((((inventory[INVEN_RARM].k_idx && (inventory[INVEN_RARM].name1 == ART_OGRE_BLADE))
	          && (inventory[INVEN_LARM].k_idx && (inventory[INVEN_LARM].name1 == ART_OGRE_SHIELD)))
	       || ((inventory[INVEN_LARM].k_idx && (inventory[INVEN_LARM].name1 == ART_OGRE_BLADE))
	          && (inventory[INVEN_RARM].k_idx && (inventory[INVEN_RARM].name1 == ART_OGRE_SHIELD))))
	    && (inventory[INVEN_BODY].k_idx && (inventory[INVEN_BODY].name1 == ART_OGRE_ARMOR))
	    && (inventory[INVEN_HEAD].k_idx && (inventory[INVEN_HEAD].name1 == ART_OGRE_HELM)))
	{
		p_ptr->to_h[0] += 50;
		p_ptr->to_h[1] += 50;
		p_ptr->dis_to_h[0] += 50;
		p_ptr->dis_to_h[1] += 50;
		p_ptr->to_d[0] += 50;
		p_ptr->to_d[1] += 50;
		p_ptr->dis_to_d[0] += 50;
		p_ptr->dis_to_d[1] += 50;
		for (i = 0; i < A_MAX; i++) p_ptr->stat_add[i] += 220;
		p_ptr->ogre_equip = TRUE;
	}

	if ((inventory[INVEN_RARM].k_idx && (inventory[INVEN_RARM].name1 == ART_BERSERK)) ||
	    (inventory[INVEN_LARM].k_idx && (inventory[INVEN_LARM].name1 == ART_BERSERK)))
	{
		p_ptr->shero = 1;
	}

	if (((inventory[INVEN_RARM].tval == TV_SWORD) && (inventory[INVEN_RARM].sval == SV_DARK_SWORD)) || 
		((inventory[INVEN_LARM].tval == TV_SWORD) && (inventory[INVEN_LARM].sval == SV_DARK_SWORD)))
	{
		if ((p_ptr->cexp_info[CLASS_TERRORKNIGHT].clev > 34) || (p_ptr->pclass == CLASS_VAMPIRE))
		{
			p_ptr->cursed &= ~(TRC_DRAIN_EXP);
			p_ptr->cursed &= ~(TRC_TY_CURSE);
		}
	}

	if (p_ptr->cexp_info[CLASS_LICH].clev > 49) p_ptr->cursed &= ~(TRC_TY_CURSE);

	p_ptr->skull_mask_hates = FALSE;

	if (inventory[INVEN_HEAD].k_idx && (inventory[INVEN_HEAD].name1 == ART_SKULL_MASK))
	{
		if (realm_choices[p_ptr->pclass])
		{
			p_ptr->cursed |= (TRC_AGGRAVATE | TRC_TY_CURSE);
			p_ptr->skull_mask_hates = TRUE;
		}
	}

	/* Always calculate HP & mana */
	p_ptr->update |= (PU_HP | PU_MANA);

	/* Calculate stats */
	for (i = 0; i < A_MAX; i++)
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

		if ((i == A_CHR) && (p_ptr->muta3 & MUT3_ILL_NORM))
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
			p_ptr->window |= (PW_PLAYER);
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

			/* Change in INT may affect Mana/Spells */
			if (i == A_INT)
			{
				if (mp_ptr->spell_stat == A_INT)
				{
					p_ptr->update |= (PU_SPELLS);
				}
			}

			/* Change in WIS may affect Mana/Spells */
			else if (i == A_WIS)
			{
				if (mp_ptr->spell_stat == A_WIS)
				{
					p_ptr->update |= (PU_SPELLS);
				}
			}

			/* Change in WIS may affect Mana/Spells */
			else if (i == A_CHR)
			{
				if (mp_ptr->spell_stat == A_CHR)
				{
					p_ptr->update |= (PU_SPELLS);
				}
			}

			/* Window stuff */
			p_ptr->window |= (PW_PLAYER);
		}
	}


	/* Apply temporary "stun" */
	switch (stun_level(p_ptr->stun))
	{
	case 0:
		break;

	case 1:
		p_ptr->to_h[0] -= 5;
		p_ptr->to_h[1] -= 5;
		p_ptr->to_h_b -= 5;
		p_ptr->to_h_m -= 5;
		p_ptr->dis_to_h[0] -= 5;
		p_ptr->dis_to_h[1] -= 5;
		p_ptr->dis_to_h_b -= 5;
		p_ptr->to_d[0] -= 5;
		p_ptr->to_d[1] -= 5;
		p_ptr->to_d_m -= 5;
		p_ptr->dis_to_d[0] -= 5;
		p_ptr->dis_to_d[1] -= 5;
		break;

	default:
		p_ptr->to_h[0] -= 20;
		p_ptr->to_h[1] -= 20;
		p_ptr->to_h_b  -= 20;
		p_ptr->to_h_m  -= 20;
		p_ptr->dis_to_h[0] -= 20;
		p_ptr->dis_to_h[1] -= 20;
		p_ptr->dis_to_h_b  -= 20;
		p_ptr->to_d[0] -= 20;
		p_ptr->to_d[1] -= 20;
		p_ptr->to_d_m -= 20;
		p_ptr->dis_to_d[0] -= 20;
		p_ptr->dis_to_d[1] -= 20;
		break;
	}

	/* wraith_form */
	if (WRAITH_FORM())
	{
		p_ptr->reflect = TRUE;
	}

	/* Temporary blessing */
	if (p_ptr->blessed)
	{
		p_ptr->to_a += 5;
		p_ptr->dis_to_a += 5;
		p_ptr->to_h[0] += 10;
		p_ptr->to_h[1] += 10;
		p_ptr->to_h_b  += 10;
		p_ptr->to_h_m  += 10;
		p_ptr->dis_to_h[0] += 10;
		p_ptr->dis_to_h[1] += 10;
		p_ptr->dis_to_h_b += 10;
	}

	if (p_ptr->magicdef)
	{
		p_ptr->resist_blind = TRUE;
		p_ptr->resist_conf = TRUE;
		p_ptr->reflect = TRUE;
		p_ptr->free_act = TRUE;
		p_ptr->ffall = TRUE;
	}

	/* Temporary "Hero" */
	if (p_ptr->hero)
	{
		p_ptr->to_h[0] += 12;
		p_ptr->to_h[1] += 12;
		p_ptr->to_h_b  += 12;
		p_ptr->to_h_m  += 12;
		p_ptr->dis_to_h[0] += 12;
		p_ptr->dis_to_h[1] += 12;
		p_ptr->dis_to_h_b  += 12;
	}

	/* Temporary "Beserk" */
	if (p_ptr->shero)
	{
		p_ptr->to_h[0] += 12;
		p_ptr->to_h[1] += 12;
		p_ptr->to_h_b  -= 12;
		p_ptr->to_h_m  += 12;
		p_ptr->to_d[0] += 3+(p_ptr->lev/5);
		p_ptr->to_d[1] += 3+(p_ptr->lev/5);
		p_ptr->to_d_m  += 3+(p_ptr->lev/5);
		p_ptr->dis_to_h[0] += 12;
		p_ptr->dis_to_h[1] += 12;
		p_ptr->dis_to_h_b  -= 12;
		p_ptr->dis_to_d[0] += 3+(p_ptr->lev/5);
		p_ptr->dis_to_d[1] += 3+(p_ptr->lev/5);
		p_ptr->to_a -= 10;
		p_ptr->dis_to_a -= 10;
		p_ptr->skill_stl -= 7;
		p_ptr->skill_dev -= 20;
		p_ptr->skill_sav -= 30;
		p_ptr->skill_srh -= 15;
		p_ptr->skill_fos -= 15;
		p_ptr->skill_tht -= 20;
		p_ptr->skill_dig += 30;
	}

	if (p_ptr->zoshonel_protect)
	{
		int from_int = p_ptr->stat_use[A_INT] / 10;
		int from_wis = p_ptr->stat_use[A_WIS] / 10;

		p_ptr->to_h[0] += from_wis;
		p_ptr->to_h[1] += from_wis;
		p_ptr->to_h_b  += from_wis;
		p_ptr->to_h_m  += from_wis;
		p_ptr->to_d[0] += from_int;
		p_ptr->to_d[1] += from_int;
		p_ptr->to_d_m  += from_int;
		p_ptr->dis_to_h[0] += from_wis;
		p_ptr->dis_to_h[1] += from_wis;
		p_ptr->dis_to_h_b  += from_wis;
		p_ptr->dis_to_d[0] += from_int;
		p_ptr->dis_to_d[1] += from_int;
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

	/* Stoning */
	if (p_ptr->stoning)
	{
		p_ptr->pspeed -= p_ptr->stoning / 5;
		p_ptr->to_a += p_ptr->stoning / 5;
		p_ptr->dis_to_a += p_ptr->stoning / 5;
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
		p_ptr->see_infra+=3;
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

	if (p_ptr->esp_dragon != old_esp_dragon)
	{
		p_ptr->update |= (PU_MONSTERS);
	}

	/* Hack -- See Invis Change */
	if (p_ptr->see_inv != old_see_inv)
	{
		p_ptr->update |= (PU_MONSTERS);
	}

	/* Hack -- Telepathy Change */
	if (WRAITH_FORM() != old_wraith_form)
	{
		p_ptr->redraw |= (PR_MAP);
	}

	/* Bloating slows the player down (a little) */
	if (!p_ptr->no_digest && (p_ptr->food >= PY_FOOD_MAX)) p_ptr->pspeed -= 10;

	if (!buki_motteruka(INVEN_RARM) && !buki_motteruka(INVEN_LARM))
	{
		int attack_var = skill_lev_var[p_ptr->skill_exp[SKILL_MARTIAL_ARTS]/10];
		p_ptr->to_h[0] += attack_var * 4 - 8;
		p_ptr->dis_to_h[0] += attack_var * 4 - 8;
		p_ptr->to_d[0] += attack_var * 2 - 2;
		p_ptr->dis_to_d[0] += attack_var * 2 - 2;
	}

	if (buki_motteruka(INVEN_RARM) && buki_motteruka(INVEN_LARM))
	{
		int attack_var = skill_lev_var[p_ptr->skill_exp[SKILL_NITOURYU]/10];
		int penalty1 = (100 - attack_var * attack_var) - (130 - inventory[INVEN_RARM].weight) / 8;
		int penalty2 = (100 - attack_var * attack_var) - (130 - inventory[INVEN_LARM].weight) / 8;
		if (easy_2weapon)
		{
			if (penalty1 > 0) penalty1 /= 2;
			if (penalty2 > 0) penalty2 /= 2;
		}
		else if ((inventory[INVEN_LARM].tval == TV_SWORD) && (inventory[INVEN_LARM].sval == SV_MAIN_GAUCHE))
		{
			penalty1 = MAX(0, penalty1 - 10);
			penalty2 = MAX(0, penalty2 - 10);
		}
		if (inventory[INVEN_RARM].tval == TV_POLEARM) penalty1 += 10;
		if (inventory[INVEN_LARM].tval == TV_POLEARM) penalty2 += 10;
		p_ptr->to_h[0] -= penalty1;
		p_ptr->to_h[1] -= penalty2;
		p_ptr->dis_to_h[0] -= penalty1;
		p_ptr->dis_to_h[1] -= penalty2;
	}

	/* Extract the current weight (in tenth pounds) */
	j = p_ptr->total_weight;

	/* Extract the "weight limit" (in tenth pounds) */
	i = weight_limit();

	if (p_ptr->riding)
	{
		int speed = m_list[p_ptr->riding].mspeed;
		if (m_list[p_ptr->riding].mspeed > 110)
		{
			p_ptr->pspeed = 110 + ((speed - 110)*((skill_lev_var[p_ptr->skill_exp[SKILL_RIDING]/10] * 1000)*3 + p_ptr->lev*160L - 10000L)/(22000L));
			if (p_ptr->pspeed < 110) p_ptr->pspeed = 110;
		}
		else
		{
			p_ptr->pspeed = speed;
		}
		if (m_list[p_ptr->riding].fast) p_ptr->pspeed += 10;
		if (m_list[p_ptr->riding].slow) p_ptr->pspeed -= 10;
		if (r_info[m_list[p_ptr->riding].r_idx].flags7 & RF7_CAN_FLY) p_ptr->ffall = TRUE;
		if (r_info[m_list[p_ptr->riding].r_idx].flags7 & (RF7_CAN_SWIM | RF7_AQUATIC)) p_ptr->can_swim = TRUE;

		if ((skill_lev_var[p_ptr->skill_exp[SKILL_RIDING]/10] * 1000) < 2000) j += (p_ptr->wt*3*(2000 - (skill_lev_var[skill_exp_level(p_ptr->skill_exp[SKILL_RIDING])] * 1000)))/2000;

		i = 3000 + r_info[m_list[p_ptr->riding].r_idx].level * 50;
	}

	if (p_ptr->earth_spike) p_ptr->ffall = FALSE;

	/* XXX XXX XXX Apply "encumbrance" from weight */
	if (j > i/2) p_ptr->pspeed -= ((j - (i/2)) / (i / 10));

	/* Searching slows the player down */
	if (p_ptr->action == ACTION_SEARCH) p_ptr->pspeed -= 10;

	/* Stealth walking slows the player down */
	if (p_ptr->action == ACTION_STEALTH)
	{
		p_ptr->pspeed -= 10;
		p_ptr->skill_stl += 10;
	}

	/* Lord Aura gain some power */
	if (p_ptr->action == ACTION_AURA)
	{
		int clev = p_ptr->cexp_info[CLASS_LORD].clev;
		
		p_ptr->skill_sav += clev;
		p_ptr->to_a += 10 + clev / 5;
		p_ptr->dis_to_a += 10 + clev / 5;
		for (i = 0; i < 2; i++)
		{
			p_ptr->to_d[i] += 10 + clev / 5;
			p_ptr->dis_to_d[i] += 10 + clev / 5;
		}
		if (clev > 30) p_ptr->pspeed += 5 + clev / 10;
	}

	/* Actual Modifier Bonuses (Un-inflate stat bonuses) */
	p_ptr->to_a += ((int)(adj_dex_ta[p_ptr->stat_ind[A_DEX]]) - 128);
	p_ptr->to_d[0] += ((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128);
	p_ptr->to_d[1] += ((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128);
	p_ptr->to_d_m  += ((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128);
	p_ptr->to_h[0] += ((int)(adj_dex_th[p_ptr->stat_ind[A_DEX]]) - 128);
	p_ptr->to_h[1] += ((int)(adj_dex_th[p_ptr->stat_ind[A_DEX]]) - 128);
	p_ptr->to_h_b  += ((int)(adj_dex_th[p_ptr->stat_ind[A_DEX]]) - 128);
	p_ptr->to_h_m  += ((int)(adj_dex_th[p_ptr->stat_ind[A_DEX]]) - 128);
	p_ptr->to_h[0] += ((int)(adj_str_th[p_ptr->stat_ind[A_STR]]) - 128);
	p_ptr->to_h[1] += ((int)(adj_str_th[p_ptr->stat_ind[A_STR]]) - 128);
	p_ptr->to_h_b  += ((int)(adj_str_th[p_ptr->stat_ind[A_STR]]) - 128);
	p_ptr->to_h_m  += ((int)(adj_str_th[p_ptr->stat_ind[A_STR]]) - 128);

	/* Displayed Modifier Bonuses (Un-inflate stat bonuses) */
	p_ptr->dis_to_a += ((int)(adj_dex_ta[p_ptr->stat_ind[A_DEX]]) - 128);
	p_ptr->dis_to_d[0] += ((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128);
	p_ptr->dis_to_d[1] += ((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128);
	p_ptr->dis_to_h[0] += ((int)(adj_dex_th[p_ptr->stat_ind[A_DEX]]) - 128);
	p_ptr->dis_to_h[1] += ((int)(adj_dex_th[p_ptr->stat_ind[A_DEX]]) - 128);
	p_ptr->dis_to_h_b  += ((int)(adj_dex_th[p_ptr->stat_ind[A_DEX]]) - 128);
	p_ptr->dis_to_h[0] += ((int)(adj_str_th[p_ptr->stat_ind[A_STR]]) - 128);
	p_ptr->dis_to_h[1] += ((int)(adj_str_th[p_ptr->stat_ind[A_STR]]) - 128);
	p_ptr->dis_to_h_b  += ((int)(adj_str_th[p_ptr->stat_ind[A_STR]]) - 128);


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
		p_ptr->to_h_b  += 2 * (hold - o_ptr->weight / 10);
		p_ptr->dis_to_h_b  += 2 * (hold - o_ptr->weight / 10);

		/* Heavy Bow */
		p_ptr->heavy_shoot = TRUE;
	}


	/* Compute "extra shots" if needed */
	if (o_ptr->k_idx)
	{
		/* Analyze the launcher */
		switch (o_ptr->sval)
		{
		case SV_PISTOL:
			p_ptr->tval_ammo = TV_BULLET;
			break;

		case SV_ASSAULT_RIFLE:
		case SV_SNIPER_RIFLE:
		case SV_RUNEGUN:
			p_ptr->tval_ammo = TV_ROUND;
			break;

		case SV_SHOTGUN:
			p_ptr->tval_ammo = TV_SHELL;
			break;

		case SV_ROCKET_LAUNCHER:
			p_ptr->tval_ammo = TV_ROCKET;
			break;

		case SV_SHORT_BOW:
		case SV_LONG_BOW:
		case SV_RUNEBOW:
			p_ptr->tval_ammo = TV_ARROW;
			break;

		case SV_BOWGUN:
		case SV_CROSSBOW:
			p_ptr->tval_ammo = TV_BOLT;
			break;
		}

		/* Apply special flags */
		if (o_ptr->k_idx && !p_ptr->heavy_shoot)
		{
			int attack_var = skill_lev_var[p_ptr->weapon_exp[get_weapon_type(&k_info[inventory[INVEN_BOW].k_idx])]/10];

			/* Extra shots */
			p_ptr->num_fire += (extra_shots * 100);

			/* Analyze the launcher */
			switch (o_ptr->sval)
			{
			case SV_PISTOL:
				p_ptr->num_fire += attack_var * 50;
				break;

			case SV_SNIPER_RIFLE:
			case SV_SHOTGUN:
			case SV_ROCKET_LAUNCHER:
			case SV_BOWGUN:
			case SV_CROSSBOW:
				p_ptr->num_fire += attack_var * 100 / 3;
				break;

			case SV_ASSAULT_RIFLE:
			case SV_RUNEGUN:
				p_ptr->num_fire += attack_var * 25;
				break;

			case SV_SHORT_BOW:
			case SV_LONG_BOW:
			case SV_RUNEBOW:
				p_ptr->num_fire += (attack_var - 1) * 50;
				break;
			}

			if (p_ptr->pclass == CLASS_GUNNER)
			{
				if ((p_ptr->tval_ammo >= TV_BULLET) && (p_ptr->tval_ammo <= TV_ROCKET))
				{
					p_ptr->to_h_b += 5;
					p_ptr->dis_to_h_b += 5;
				}
			}
		}
	}

	if (p_ptr->ryoute)
		hold *= 2;

	for (i = 0; i < 2; i++)
	{
		/* Examine the "main weapon" */
		o_ptr = &inventory[INVEN_RARM+i];
		k_ptr = &k_info[o_ptr->k_idx];

		object_flags(o_ptr, flgs);

		/* Assume not heavy */
		p_ptr->heavy_wield[i] = FALSE;
		p_ptr->icky_wield[i] = 0;
		p_ptr->riding_wield[i] = FALSE;

		p_ptr->inc_msp[i] = 0;

		if (!buki_motteruka(INVEN_RARM + i))
		{
			p_ptr->num_blow[i] = 1;
			continue;
		}

		/* It is hard to hold a heavy weapon */
		if (hold < o_ptr->weight / 10)
		{
			/* Hard to wield a heavy weapon */
			p_ptr->to_h[i] += 2 * (hold - o_ptr->weight / 10);
			p_ptr->dis_to_h[i] += 2 * (hold - o_ptr->weight / 10);

			/* Heavy weapon */
			p_ptr->heavy_wield[i] = TRUE;
		}
		else if (p_ptr->ryoute && (hold < o_ptr->weight/5)) omoi = TRUE;

		if ((i == 1) && (o_ptr->tval == TV_SWORD) && (o_ptr->sval == SV_MAIN_GAUCHE))
		{
			p_ptr->to_a += 5;
			p_ptr->dis_to_a += 5;
			if ((p_ptr->pclass == CLASS_SWORDMASTER) && (get_weapon_type(k_ptr) == WT_KATANA))
			{
				p_ptr->to_a += 5;
				p_ptr->dis_to_a += 5;
				p_ptr->to_d[0] += 5;
				p_ptr->dis_to_d[0] += 5;
			}
		}

		/* Normal weapons */
		if (o_ptr->k_idx && !p_ptr->heavy_wield[i])
		{
			int str_index, dex_index;

			int num = 0, wgt = 0, mul = 0, div = 0;
			int skill_level = p_ptr->weapon_exp[get_weapon_type(&k_info[o_ptr->k_idx])]/10;
			int attack_var = skill_lev_var[skill_level];

			/* Analyze the class */
			switch (p_ptr->pclass)
			{
				case CLASS_SOLDIER:
					num = 5; wgt = 70; mul = 3; break;

				case CLASS_KNIGHT:
				case CLASS_BERSERKER:
				case CLASS_DRAGOON:
				case CLASS_ANGELKNIGHT:
				case CLASS_LORD:
				case CLASS_GENERAL:
				case CLASS_FREYA:
				case CLASS_VAMPIRE:
					num = 6; wgt = 70; mul = 5; break;

				case CLASS_TERRORKNIGHT:
					num = 6; wgt = 70; mul = 6; break;

				case CLASS_BEASTTAMER:
				case CLASS_VALKYRIE:
				case CLASS_DRAGONTAMER:
				case CLASS_TEMPLEKNIGHT:
				case CLASS_WHITEKNIGHT:
					num = 5; wgt = 70; mul = 4; break;

				case CLASS_SWORDMASTER:
					num = 5; wgt = 50; mul = 4; break;

				case CLASS_NINJA:
					num = 4; wgt = 20; mul = 1; break;

				case CLASS_NINJAMASTER:
					num = 5; wgt = 50; mul = 1; break;

				case CLASS_WIZARD:
				case CLASS_SIRENE:
				case CLASS_PRIEST:
				case CLASS_LICH:
				case CLASS_HIGHWITCH:
				case CLASS_GUNNER:
				case CLASS_ARCHMAGE:
					num = 3; wgt = 100; mul = 2; break;

				case CLASS_WARLOCK:
					num = 4; wgt = 100; mul = 4; break;

				case CLASS_EXORCIST:
					num = 5; wgt = 100; mul = 4; break;

				case CLASS_AMAZONESS:
					num = 5; wgt = 70; mul = 3; break;

				case CLASS_ARCHER:
				case CLASS_WITCH:
				case CLASS_CRESCENT:
					num = 4; wgt = 100; mul = 3; break;

				case CLASS_CLERIC:
					num = 5; wgt = 100; mul = 3; break;

			}

			/* Enforce a minimum "weight" (tenth pounds) */
			div = ((o_ptr->weight < wgt) ? wgt : o_ptr->weight);

			/* Access the strength vs weight */
			str_index = (adj_str_blow[p_ptr->stat_ind[A_STR]] * mul / div);

			if (p_ptr->ryoute && !omoi) str_index++;
			if ((p_ptr->pclass == CLASS_NINJA) || (p_ptr->pclass == CLASS_NINJAMASTER)) str_index = MAX(0, str_index-1);

			/* Maximal value */
			if (str_index > 11) str_index = 11;

			/* Index by dexterity */
			dex_index = (adj_dex_blow[p_ptr->stat_ind[A_DEX]]);
			dex_index += (attack_var - 4);

			/* Skill level affects */
			switch (skill_level)
			{
			case SKILL_LEVEL_BEGINNER:
				if (num > 1) num--;
				break;
			case SKILL_LEVEL_MASTER:
				num++;
				break;
			}

			/* Maximal value & minimal value */
			if (dex_index < 0) dex_index = 0;
			if (dex_index > 11) dex_index = 11;

			/* Use the blows table */
			p_ptr->num_blow[i] = blows_table[str_index][dex_index];

			/* Maximal value */
			if (p_ptr->num_blow[i] > num) p_ptr->num_blow[i] = num;

			/* Add in the "bonus blows" */
			p_ptr->num_blow[i] += extra_blows[i];


			if ((p_ptr->pclass == CLASS_KNIGHT) || (p_ptr->pclass == CLASS_BERSERKER) || (p_ptr->pclass == CLASS_GENERAL))
				p_ptr->num_blow[i] += (cexp_ptr->clev / 40);
			else if (p_ptr->pclass == CLASS_TERRORKNIGHT)
			{
				p_ptr->num_blow[i] += (cexp_ptr->clev / 23);
			}


			/* Require at least one blow */
			if (p_ptr->num_blow[i] < 1) p_ptr->num_blow[i] = 1;

			/* Boost digging skill by weapon weight */
			p_ptr->skill_dig += (o_ptr->weight / 10);
		}

		switch (p_ptr->pclass)
		{
		case CLASS_KNIGHT:
		case CLASS_GENERAL:
			if (weapon_type_bit(get_weapon_type(k_ptr)) & (WT_BIT_SMALL_SWORD | WT_BIT_KATANA | WT_BIT_SWORD | WT_BIT_GREAT_SWORD | WT_BIT_SPEAR | WT_BIT_LANCE))
			{
				p_ptr->to_d[i] += 5;
				p_ptr->dis_to_d[i] += 5;
			}
			break;

		case CLASS_BERSERKER:
			if (get_weapon_type(k_ptr) == WT_AXE)
			{
				p_ptr->to_d[i] += 5;
				p_ptr->dis_to_d[i] += 5;
			}
			break;

		case CLASS_TERRORKNIGHT:
			p_ptr->to_h[i] += cexp_ptr->clev/5;
			p_ptr->to_d[i] += cexp_ptr->clev/6;
			p_ptr->dis_to_h[i] += cexp_ptr->clev/5;
			p_ptr->dis_to_d[i] += cexp_ptr->clev/6;
			if (!p_ptr->hidarite || p_ptr->ryoute)
			{
				p_ptr->to_h[i] += cexp_ptr->clev/5;
				p_ptr->to_d[i] += cexp_ptr->clev/6;
				p_ptr->dis_to_h[i] += cexp_ptr->clev/5;
				p_ptr->dis_to_d[i] += cexp_ptr->clev/6;
			}
			if (get_weapon_type(k_ptr) == WT_HAMMER)
			{
				p_ptr->to_d[i] += 10;
				p_ptr->dis_to_d[i] += 10;
			}
			break;

		case CLASS_BEASTTAMER:
			if (get_weapon_type(k_ptr) == WT_WHIP)
			{
				p_ptr->to_dd[i] += 2;
				p_ptr->to_ds[i] += 2;
			}
			break;

		case CLASS_SWORDMASTER:
			if (weapon_type_bit(get_weapon_type(k_ptr)) & (WT_BIT_SMALL_SWORD | WT_BIT_KATANA | WT_BIT_SWORD | WT_BIT_GREAT_SWORD))
			{
				p_ptr->to_d[i] += 5;
				p_ptr->dis_to_d[i] += 5;
				if ((o_ptr->tval == TV_SWORD) && (o_ptr->sval == SV_KATANA))
				{
					p_ptr->to_dd[i]++;
					p_ptr->to_ds[i]++;
				}
			}
			else p_ptr->icky_wield[i] = -1;
			break;

		case CLASS_LORD:
		case CLASS_VAMPIRE:
			if (weapon_type_bit(get_weapon_type(k_ptr)) & (WT_BIT_SMALL_SWORD | WT_BIT_SWORD | WT_BIT_GREAT_SWORD))
			{
				p_ptr->to_d[i] += 5;
				p_ptr->dis_to_d[i] += 5;
				if (o_ptr->tval == TV_SWORD)
				{
					p_ptr->to_dd[i]++;
					p_ptr->to_ds[i]++;
				}
			}
			else p_ptr->icky_wield[i] = -1;
			break;

		case CLASS_NINJA:
		case CLASS_NINJAMASTER:
			if (get_weapon_type(k_ptr) == WT_CLAW)
			{
				p_ptr->to_h[i] += 10;
				p_ptr->dis_to_h[i] += 10;
				p_ptr->to_dd[i] += 2;
				p_ptr->to_ds[i] += 2;
			}
			else if (get_weapon_type(k_ptr) == WT_SMALL_SWORD)
			{
				p_ptr->to_h[i] += 5;
				p_ptr->dis_to_h[i] += 5;
			}
			break;

		case CLASS_WIZARD:
		case CLASS_WARLOCK:
		case CLASS_ARCHMAGE:
		case CLASS_WITCH:
		case CLASS_SIRENE:
		case CLASS_LICH:
		case CLASS_HIGHWITCH:
			if ((!p_ptr->s_ptr->w_eff[get_weapon_type(k_ptr)]) && (p_ptr->weapon_exp[get_weapon_type(k_ptr)]/10 < SKILL_EXP_SKILLED))
			{
				/* Icky weapon */
				p_ptr->icky_wield[i] += 10;
				p_ptr->inc_msp[i] -= 20;
			}

			/* Lich weapon penalty for blessed weapons */
			if ((p_ptr->pclass == CLASS_LICH) || (p_ptr->pclass == CLASS_VAMPIRE))
			{
				if (have_flag(flgs, TR_BLESSED))
				{
					int div = have_flag(flgs, TR_UNHOLY) ? 2 : 1;

					/* Reduce the real bonuses */
					p_ptr->to_h[i] -= 200 / div;
					p_ptr->to_d[i] -= 200 / div;

					/* Reduce the mental bonuses */
					p_ptr->dis_to_h[i] -= 200 / div;
					p_ptr->dis_to_d[i] -= 200 / div;

					/* Icky weapon */
					p_ptr->icky_wield[i] += 25 / div;
				}
				if (have_flag(flgs, TR_UNHOLY))
				{
					p_ptr->inc_msp[i] += 10;
				}
			}
			break;

		case CLASS_EXORCIST:
		case CLASS_CLERIC:
		case CLASS_PRIEST:
		case CLASS_ANGELKNIGHT:
			/* Priest weapon penalty for non-blessed edged weapons */
			if ((p_ptr->pclass != CLASS_ANGELKNIGHT) && !have_flag(flgs, TR_BLESSED) &&
			    ((o_ptr->tval == TV_SWORD) || (o_ptr->tval == TV_POLEARM)))
			{
				/* Reduce the real bonuses */
				p_ptr->to_h[i] -= 2;
				p_ptr->to_d[i] -= 2;

				/* Reduce the mental bonuses */
				p_ptr->dis_to_h[i] -= 2;
				p_ptr->dis_to_d[i] -= 2;

				/* Icky weapon */
				p_ptr->icky_wield[i] += 25;
			}
			if (have_flag(flgs, TR_UNHOLY))
			{
				int div = have_flag(flgs, TR_BLESSED) ? 2 : 1;

				/* Reduce the real bonuses */
				p_ptr->to_h[i] -= 2 / div;
				p_ptr->to_d[i] -= 2 / div;

				/* Reduce the mental bonuses */
				p_ptr->dis_to_h[i] -= 2 / div;
				p_ptr->dis_to_d[i] -= 2 / div;

				/* Icky weapon */
				p_ptr->icky_wield[i] += 25 / div;
			}
			break;

		case CLASS_VALKYRIE:
		case CLASS_FREYA:
			if (weapon_type_bit(get_weapon_type(k_ptr)) & (WT_BIT_SPEAR | WT_BIT_LANCE))
			{
				p_ptr->to_d[i] += 5;
				p_ptr->dis_to_d[i] += 5;
			}
			break;

		case CLASS_ARCHER:
		case CLASS_GUNNER:
		case CLASS_CRESCENT:
			if (!p_ptr->s_ptr->w_eff[get_weapon_type(&k_info[o_ptr->k_idx])])
			{
				p_ptr->to_h_b -= 10;
				p_ptr->dis_to_h_b -= 10;

				/* Icky weapon */
				p_ptr->icky_wield[i] = -1;
			}
			break;

		case CLASS_TEMPLEKNIGHT:
			if (have_flag(flgs, TR_UNHOLY))
			{
				p_ptr->to_dd[i] += 2;
			}
			if (have_flag(flgs, TR_BLESSED))
			{
				/* Reduce the real bonuses */
				p_ptr->to_h[i] -= 2;
				p_ptr->to_d[i] -= 2;

				/* Reduce the mental bonuses */
				p_ptr->dis_to_h[i] -= 2;
				p_ptr->dis_to_d[i] -= 2;

				/* Icky weapon */
				p_ptr->icky_wield[i] += 25;
			}
			break;

		case CLASS_WHITEKNIGHT:
			if (have_flag(flgs, TR_BLESSED))
			{
				p_ptr->to_dd[i] += 2;
			}
			if (have_flag(flgs, TR_UNHOLY))
			{
				/* Reduce the real bonuses */
				p_ptr->to_h[i] -= 2;
				p_ptr->to_d[i] -= 2;

				/* Reduce the mental bonuses */
				p_ptr->dis_to_h[i] -= 2;
				p_ptr->dis_to_d[i] -= 2;

				/* Icky weapon */
				p_ptr->icky_wield[i] += 25;
			}
			break;
		}

		/* Hafted weapon's SP bonus for some classes */
		if (o_ptr->tval == TV_HAFTED)
		{
			switch (p_ptr->pclass)
			{
			case CLASS_WIZARD:
			case CLASS_WARLOCK:
			case CLASS_ARCHMAGE:
			case CLASS_WITCH:
			case CLASS_SIRENE:
			case CLASS_CLERIC:
			case CLASS_PRIEST:
			case CLASS_LICH:
			case CLASS_HIGHWITCH:
				switch (o_ptr->sval)
				{
				case SV_QUARTERSTAFF:
				case SV_SCIPPLAYS_STAFF:
				case SV_WIZSTAFF:
				case SV_RUNESTAFF:
					p_ptr->inc_msp[i] += 5;
					break;
				case SV_FAN:
				case SV_RUNEFAN:
					if (p_ptr->pclass == CLASS_SIRENE) p_ptr->inc_msp[i] += 20;
					break;
				case SV_LIFE_STAFF:
				case SV_CLEAR_STAFF:
					if (p_ptr->pclass != CLASS_LICH) p_ptr->inc_msp[i] += 5;
					break;
				}
				break;
			}
		}

		if (p_ptr->riding)
		{
			k_ptr = &k_info[o_ptr->k_idx];

			if (get_weapon_type(k_ptr) == WT_LANCE)
			{
				p_ptr->to_h[i] += 15;
				p_ptr->dis_to_h[i] += 15;
				p_ptr->to_dd[i] += 2;
				if ((p_ptr->pclass == CLASS_KNIGHT) || (p_ptr->pclass == CLASS_GENERAL))
				{
					p_ptr->to_dd[i]++;
					p_ptr->to_ds[i]++;
				}
				else if ((p_ptr->pclass == CLASS_VALKYRIE) || (p_ptr->pclass == CLASS_FREYA))
				{
					p_ptr->to_dd[i]++;
					p_ptr->to_ds[i]++;
					p_ptr->to_h[i] += 5;
					p_ptr->dis_to_h[i] += 5;
				}
			}
			else if ((p_ptr->pclass == CLASS_BEASTTAMER) && (get_weapon_type(k_ptr) == WT_WHIP))
			{
				/* Nothing */
			}
			else if (!(have_flag(flgs, TR_RIDING)))
			{
				int penalty;
				if ((p_ptr->pclass == CLASS_BEASTTAMER) || (p_ptr->pclass == CLASS_DRAGONTAMER) || (p_ptr->cexp_info[CLASS_BEASTTAMER].clev > 49) || (p_ptr->cexp_info[CLASS_DRAGONTAMER].clev > 49))
				{
					penalty = 5;
				}
				else
				{
					penalty = r_info[m_list[p_ptr->riding].r_idx].level - (skill_lev_var[p_ptr->skill_exp[SKILL_RIDING]/10] * 1000) / 80;
					penalty += 30;
					if (penalty < 30) penalty = 30;
				}
				p_ptr->to_h[i] -= penalty;
				p_ptr->dis_to_h[i] -= penalty;

				/* Riding weapon */
				p_ptr->riding_wield[i] = TRUE;
			}
		}
	}

	if (p_ptr->riding)
	{
		int penalty = 0;

		p_ptr->riding_ryoute = FALSE;
		if (p_ptr->ryoute || !empty_hands_status) p_ptr->riding_ryoute = TRUE;

		if ((p_ptr->pclass == CLASS_BEASTTAMER) || (p_ptr->pclass == CLASS_DRAGONTAMER) || (p_ptr->cexp_info[CLASS_BEASTTAMER].clev > 49) || (p_ptr->cexp_info[CLASS_DRAGONTAMER].clev > 49))
		{
			p_ptr->to_a += 10;
			if (p_ptr->tval_ammo != TV_ARROW) penalty = 5;
		}
		else
		{
			penalty = r_info[m_list[p_ptr->riding].r_idx].level - (skill_lev_var[p_ptr->skill_exp[SKILL_RIDING]/10] * 1000) / 80;
			penalty += 30;
			if (penalty < 30) penalty = 30;
		}
		if (p_ptr->tval_ammo == TV_BOLT) penalty *= 2;
		p_ptr->to_h_b -= penalty;
		p_ptr->dis_to_h_b -= penalty;
	}

	/* Non-riding players only */
	else if (empty_hands_status & EMPTY_HAND_RARM)
	{
		/* Different calculation for Terror-Knights with empty hands */
		if (p_ptr->pclass == CLASS_TERRORKNIGHT)
		{
			int blow_base = cexp_ptr->clev / 3 + adj_dex_blow_bare_hand[p_ptr->stat_ind[A_DEX]];

			if (empty_hands_status & EMPTY_HAND_LARM) blow_base += 8;

			p_ptr->num_blow[0] = 1;

			if (blow_base > 29) p_ptr->num_blow[0]++;
			if (blow_base > 59) p_ptr->num_blow[0]++;

			p_ptr->to_h[0] += (cexp_ptr->clev / 3);
			p_ptr->dis_to_h[0] += (cexp_ptr->clev / 3);

			p_ptr->to_d[0] += (cexp_ptr->clev / 6);
			p_ptr->dis_to_d[0] += (cexp_ptr->clev / 6);
		}

		p_ptr->num_blow[0] += extra_blows[0];

		p_ptr->num_blow[0] += skill_lev_var[p_ptr->skill_exp[SKILL_MARTIAL_ARTS]/10] - 1;
		if (empty_hands_status & EMPTY_HAND_LARM) dual_bare_hand = TRUE;
	}

	monk_armour_aux = FALSE;

	if (heavy_armor())
	{
		monk_armour_aux = TRUE;
	}

	for (i = 0; i < 2; i++)
	{
		o_ptr = &inventory[INVEN_RARM + i];

		if (buki_motteruka(INVEN_RARM+i))
		{
			int attack_var = skill_lev_var[p_ptr->weapon_exp[get_weapon_type(&k_info[o_ptr->k_idx])]/10];

			p_ptr->to_h[i] += attack_var * 4 - 8;
			p_ptr->dis_to_h[i] += attack_var * 4 - 8;
			p_ptr->to_d[i] += attack_var * 2 - 2;
			p_ptr->dis_to_d[i] += attack_var * 2 - 2;
		}
		else
		{
			switch (p_ptr->pclass)
			{
			case CLASS_ARCHER:
			case CLASS_GUNNER:
			case CLASS_CRESCENT:
				if (o_ptr->k_idx && (o_ptr->tval == TV_SHIELD) && (o_ptr->weight >= 70))
				{
					p_ptr->to_h_b -= 40;
					p_ptr->dis_to_h_b -= 40;

					/* Icky weapon */
					p_ptr->icky_wield[i] = -1;
				}
				break;
			}
		}

		if (p_ptr->tim_inc_blow) p_ptr->num_blow[i]++;
		if (p_ptr->tim_dec_blow && (p_ptr->num_blow[i] > 1)) p_ptr->num_blow[i]--;
		if ((p_ptr->action == ACTION_AURA) && (p_ptr->cexp_info[CLASS_LORD].clev > 40)) p_ptr->num_blow[i]++;

		/* Hack - If SP bonus < 0, mark as "icky" with no failrate penalty */
		if ((p_ptr->inc_msp[i] < 0) && !p_ptr->icky_wield[i])
			p_ptr->icky_wield[i] = -1;
	}

	if (character_dungeon)
	{
		if (prace_is_(RACE_MERMAID))
		{
			if (IS_MERMAID_IN_WATER() && !p_ptr->is_dead)
			{
				p_ptr->mermaid_in_water = TRUE;
				p_ptr->pspeed += p_ptr->lev / 10 + 5;
			}

			/* Redraw status bar */
			p_ptr->redraw |= (PR_STATUS);
		}
	}

	if (p_ptr->pspeed > 209) p_ptr->pspeed = 209;
	if (p_ptr->pspeed < 11) p_ptr->pspeed = 11;

	/* Display the speed (if needed) */
	if (p_ptr->pspeed != old_speed) p_ptr->redraw |= (PR_SPEED);

	/* Redraw armor (if needed) */
	if ((p_ptr->dis_ac != old_dis_ac) || (p_ptr->dis_to_a != old_dis_to_a))
	{
		/* Redraw */
		p_ptr->redraw |= (PR_ARMOR);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);
	}


	if ((p_ptr->ryoute && !omoi) || dual_bare_hand)
	{
		int bonus_to_h=0, bonus_to_d=0;
		bonus_to_d = ((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128)/2;
		bonus_to_h = ((int)(adj_str_th[p_ptr->stat_ind[A_STR]]) - 128) + ((int)(adj_dex_th[p_ptr->stat_ind[A_DEX]]) - 128);

		p_ptr->to_h[0] += MAX(bonus_to_h,1);
		p_ptr->dis_to_h[0] += MAX(bonus_to_h,1);
		p_ptr->to_d[0] += MAX(bonus_to_d,1);
		p_ptr->dis_to_d[0] += MAX(bonus_to_d,1);
	}

	if (empty_hands_status == (EMPTY_HAND_RARM | EMPTY_HAND_LARM)) p_ptr->ryoute = FALSE;

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
	p_ptr->skill_dis += ((p_ptr->gx_dis * (p_ptr->lev + cexp_ptr->clev)) / (p_ptr->max_plv + cexp_ptr->max_clev)) / 10;

	/* Affect Skill -- magic devices (Level, by Class) */
	p_ptr->skill_dev += ((p_ptr->gx_dev * (p_ptr->lev + cexp_ptr->clev)) / (p_ptr->max_plv + cexp_ptr->max_clev)) / 10;

	/* Affect Skill -- saving throw (Level, by Class) */
	p_ptr->skill_sav += ((p_ptr->gx_sav * (p_ptr->lev + cexp_ptr->clev)) / (p_ptr->max_plv + cexp_ptr->max_clev)) / 10;

	/* Affect Skill -- stealth (Level, by Class) */
	p_ptr->skill_stl += ((p_ptr->gx_stl * (p_ptr->lev + cexp_ptr->clev)) / (p_ptr->max_plv + cexp_ptr->max_clev)) / 10;

	/* Affect Skill -- search ability (Level, by Class) */
	p_ptr->skill_srh += ((p_ptr->gx_srh * (p_ptr->lev + cexp_ptr->clev)) / (p_ptr->max_plv + cexp_ptr->max_clev)) / 10;

	/* Affect Skill -- search frequency (Level, by Class) */
	p_ptr->skill_fos += ((p_ptr->gx_fos * (p_ptr->lev + cexp_ptr->clev)) / (p_ptr->max_plv + cexp_ptr->max_clev)) / 10;

	/* Affect Skill -- combat (normal) (Level, by Class) */
	p_ptr->skill_thn += ((p_ptr->gx_thn * (p_ptr->lev + cexp_ptr->clev)) / (p_ptr->max_plv + cexp_ptr->max_clev)) / 10;

	/* Affect Skill -- combat (shooting) (Level, by Class) */
	p_ptr->skill_thb += ((p_ptr->gx_thb * (p_ptr->lev + cexp_ptr->clev)) / (p_ptr->max_plv + cexp_ptr->max_clev)) / 10;

	/* Affect Skill -- combat (throwing) (Level, by Class) */
	p_ptr->skill_tht += ((p_ptr->gx_thb * (p_ptr->lev + cexp_ptr->clev)) / (p_ptr->max_plv + cexp_ptr->max_clev)) / 10;


	/* Limit Skill -- digging from 1 up */
	if (p_ptr->skill_dig < 1) p_ptr->skill_dig = 1;

	/* Limit Skill -- Do not exceed "Mithical [99]" */
	if (p_ptr->skill_dis > (SKILL_LIKERT_MYTHICAL_MAX * SKILL_DIV_XDIS)) p_ptr->skill_dis = SKILL_LIKERT_MYTHICAL_MAX * SKILL_DIV_XDIS;
	if (p_ptr->skill_dev > (SKILL_LIKERT_MYTHICAL_MAX * SKILL_DIV_XDEV)) p_ptr->skill_dev = SKILL_LIKERT_MYTHICAL_MAX * SKILL_DIV_XDEV;
	if (p_ptr->skill_sav > (SKILL_LIKERT_MYTHICAL_MAX * SKILL_DIV_XSAV)) p_ptr->skill_sav = SKILL_LIKERT_MYTHICAL_MAX * SKILL_DIV_XSAV;
	if (p_ptr->skill_stl > (SKILL_LIKERT_MYTHICAL_MAX * SKILL_DIV_XSTL)) p_ptr->skill_stl = SKILL_LIKERT_MYTHICAL_MAX * SKILL_DIV_XSTL;
	if (p_ptr->skill_srh > (SKILL_LIKERT_MYTHICAL_MAX * SKILL_DIV_XSRH)) p_ptr->skill_srh = SKILL_LIKERT_MYTHICAL_MAX * SKILL_DIV_XSRH;
	if (p_ptr->skill_fos > (SKILL_LIKERT_MYTHICAL_MAX * SKILL_DIV_XFOS)) p_ptr->skill_fos = SKILL_LIKERT_MYTHICAL_MAX * SKILL_DIV_XFOS;
	if (p_ptr->skill_thn > (SKILL_LIKERT_MYTHICAL_MAX * SKILL_DIV_XTHN)) p_ptr->skill_thn = SKILL_LIKERT_MYTHICAL_MAX * SKILL_DIV_XTHN;
	if (p_ptr->skill_thb > (SKILL_LIKERT_MYTHICAL_MAX * SKILL_DIV_XTHB)) p_ptr->skill_thb = SKILL_LIKERT_MYTHICAL_MAX * SKILL_DIV_XTHB;
	if (p_ptr->skill_tht > (SKILL_LIKERT_MYTHICAL_MAX * SKILL_DIV_XTHB)) p_ptr->skill_thb = SKILL_LIKERT_MYTHICAL_MAX * SKILL_DIV_XTHB;

	if (p_ptr->magicdef)
	{
		if (resist_magic && (p_ptr->skill_sav < ((95 + 90) + 2 * p_ptr->lev))) p_ptr->skill_sav = ((95 + 90) + 2 * p_ptr->lev);
		else if (p_ptr->skill_sav < (95 + p_ptr->lev)) p_ptr->skill_sav = 95 + p_ptr->lev;
	}
	else if (p_ptr->anti_magic)
	{
		if (resist_magic && (p_ptr->skill_sav < (2 * (90 + p_ptr->lev)))) p_ptr->skill_sav = 2 * (90 + p_ptr->lev);
		else if (p_ptr->skill_sav < (90 + p_ptr->lev)) p_ptr->skill_sav = 90 + p_ptr->lev;
	}
	else if (resist_magic)
	{
		if (p_ptr->skill_sav < (90 + p_ptr->lev)) p_ptr->skill_sav = 90 + p_ptr->lev;
	}

	if (down_saving) p_ptr->skill_sav /= 2;

	/* Hack -- handle "xtra" mode */
	if (character_xtra) return;

	/* Take note when "skull_mask_hates" changes */
	if (p_ptr->old_skull_mask_hates != p_ptr->skull_mask_hates)
	{
		/* Message */
		if (p_ptr->skull_mask_hates)
		{
#ifdef JP
			msg_print("兜に嫌われているような気がする。");
#else
			msg_print("You feel the helm hates you.");
#endif

		}
		else if (inventory[INVEN_HEAD].k_idx && (inventory[INVEN_HEAD].name1 == ART_SKULL_MASK))
		{
#ifdef JP
			msg_print("兜に嫌われている感じがなくなった。");
#else
			msg_print("You no longer feel the helm does not hate you.");
#endif

		}

		/* Save it */
		p_ptr->old_skull_mask_hates = p_ptr->skull_mask_hates;
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

	for(i = 0 ; i < 2 ; i++)
	{
		/* Take note when "heavy weapon" changes */
		if (p_ptr->old_heavy_wield[i] != p_ptr->heavy_wield[i])
		{
			/* Message */
			if (p_ptr->heavy_wield[i])
			{
#ifdef JP
				msg_print("こんな重い武器を装備しているのは大変だ。");
#else
				msg_print("You have trouble wielding such a heavy weapon.");
#endif

			}
			else if (buki_motteruka(INVEN_RARM+i))
			{
#ifdef JP
				msg_print("これなら装備していても辛くない。");
#else
				msg_print("You have no trouble wielding your weapon.");
#endif

			}
			else if (p_ptr->heavy_wield[1-i])
			{
#ifdef JP
				msg_print("まだ武器が重い。");
#else
				msg_print("You have still trouble wielding a heavy weapon.");
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
			p_ptr->old_heavy_wield[i] = p_ptr->heavy_wield[i];
		}

		/* Take note when "heavy weapon" changes */
		if (p_ptr->old_riding_wield[i] != p_ptr->riding_wield[i])
		{
			/* Message */
			if (p_ptr->riding_wield[i])
			{
#ifdef JP
				msg_print("この武器は乗馬中に使うにはむかないようだ。");
#else
				msg_print("This weapon is not suitable for use while riding.");
#endif

			}
			else if (!p_ptr->riding)
			{
#ifdef JP
				msg_print("この武器は徒歩で使いやすい。");
#else
				msg_print("This weapon was not suitable for use while riding.");
#endif

			}
			else if (buki_motteruka(INVEN_RARM+i))
			{
#ifdef JP
				msg_print("これなら乗馬中にぴったりだ。");
#else
				msg_print("This weapon is suitable for use while riding.");
#endif

			}
			/* Save it */
			p_ptr->old_riding_wield[i] = p_ptr->riding_wield[i];
		}

		/* Take note when "illegal weapon" changes */
		if ((p_ptr->old_icky_wield[i] != 0) != (p_ptr->icky_wield[i] != 0))
		{
			/* Message */
			if (p_ptr->icky_wield[i])
			{
#ifdef JP
				msg_print("今の装備はどうも自分にふさわしくない気がする。");
#else
				msg_print("You do not feel comfortable with your equipment.");
#endif
			}
			else if (buki_motteruka(INVEN_RARM+i))
			{
#ifdef JP
				msg_print("今の装備は自分にふさわしい気がする。");
#else
				msg_print("You feel comfortable with your equipment.");
#endif

			}
			else
			{
#ifdef JP
				msg_print("装備をはずしたら随分と気が楽になった。");
#else
				msg_print("You feel more comfortable after removing your equipment.");
#endif

			}

			/* Save it */
			if (p_ptr->old_icky_wield[i] != p_ptr->icky_wield[i])
				p_ptr->old_icky_wield[i] = p_ptr->icky_wield[i];
		}
	}

	if (p_ptr->riding && (p_ptr->old_riding_ryoute != p_ptr->riding_ryoute))
	{
		/* Message */
		if (p_ptr->riding_ryoute)
		{
#ifdef JP
			msg_print("両手がふさがっていて馬を操れない。");
#else
			msg_print("You are using both hand for fighting, and you can't control a riding pet.");
#endif
		}
		else
		{
#ifdef JP
			msg_print("手が空いて馬を操れるようになった。");
#else
			msg_print("You began to control riding pet with one hand.");
#endif
		}

		p_ptr->old_riding_ryoute = p_ptr->riding_ryoute;
	}

	if (((p_ptr->pclass == CLASS_NINJA) || (p_ptr->pclass == CLASS_NINJAMASTER)) && (monk_armour_aux != monk_notify_aux))
	{
		if (heavy_armor())
		{
#ifdef JP
			msg_print("装備が重くてバランスを取れない。");
#else
			msg_print("The weight of your armor disrupts your balance.");
#endif
		}
		else
#ifdef JP
			msg_print("バランスがとれるようになった。");
#else
			msg_print("You regain your balance.");
#endif

		monk_notify_aux = monk_armour_aux;
	}

	/* Determine player alignment (LNC) by equipment */
	if (inventory[INVEN_OUTER].k_idx)
	{
		o_ptr = &inventory[INVEN_OUTER];
		if ((o_ptr->name2 == EGO_ARCADIA) && (o_ptr->sval == SV_CLOAK_OF_IVORY_TOWER))
			p_ptr->align += 100;
		else if (o_ptr->name2 == EGO_ARCADIA)
			p_ptr->align += 50;
		else if (o_ptr->sval == SV_CLOAK_OF_IVORY_TOWER)
			p_ptr->align -= 50;
	}

	if (inventory[INVEN_RARM].k_idx)
	{
		o_ptr = &inventory[INVEN_RARM];
		if ((o_ptr->tval == TV_SWORD) && ((o_ptr->sval == SV_YOUTOU) || (o_ptr->sval == SV_DARK_SWORD)))
			p_ptr->align -= 100;
		else if (have_flag(flgs, TR_UNHOLY))
			p_ptr->align -= 30;
		else if (have_flag(flgs, TR_BLESSED))
			p_ptr->align += 30;
	}
	
	if (inventory[INVEN_LARM].k_idx)
	{
		o_ptr = &inventory[INVEN_LARM];
		if ((o_ptr->tval == TV_SWORD) && ((o_ptr->sval == SV_YOUTOU) || (o_ptr->sval == SV_DARK_SWORD)))
			p_ptr->align -= 100;
		else if (have_flag(flgs, TR_UNHOLY))
			p_ptr->align -= 30;
		else if (have_flag(flgs, TR_BLESSED))
			p_ptr->align += 30;
	}

	/* Limit player alignment (LNC) */
	if (p_ptr->align > 300) p_ptr->align = 300;
	if (p_ptr->align < -300) p_ptr->align = -300;

	if ((p_ptr->pass_wall && !p_ptr->kill_wall) || WRAITH_FORM()) p_ptr->no_flowed = TRUE;

	if (character_dungeon)
	{
		/* If you cannot levitate, fall into air */
		if (!p_ptr->ffall)
		{
			if (!p_ptr->leaving && (cave[py][px].feat == FEAT_AIR)) fall_into_air();
		}
	}
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
		delayed_auto_destroy();
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

	if (p_ptr->update & (PU_GOLD))
	{
		p_ptr->update &= ~(PU_GOLD);
		calc_gold();
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
		p_ptr->redraw &= ~(PR_MISC | PR_TITLE | PR_STATS);
		p_ptr->redraw &= ~(PR_LEV | PR_EXP | PR_CLEV | PR_CEXP | PR_GOLD);
		p_ptr->redraw &= ~(PR_ARMOR | PR_HP | PR_MANA);
		p_ptr->redraw &= ~(PR_DEPTH | PR_WEATHER | PR_HEALTH | PR_UHEALTH);
		prt_frame_basic();
		prt_time();
		prt_dungeon();
		prt_weather();
	}

	if (p_ptr->redraw & (PR_DUNGEON))
	{
		p_ptr->redraw &= ~(PR_DUNGEON);
	}

	if (p_ptr->redraw & (PR_EQUIPPY))
	{
		p_ptr->redraw &= ~(PR_EQUIPPY);
		print_equippy(); /* To draw / delete equippy chars */
	}

	if (p_ptr->redraw & (PR_MISC))
	{
		p_ptr->redraw &= ~(PR_MISC);
		if (!(cp_ptr->c_flags & PCF_REINCARNATE))
			prt_field(rp_ptr->title, ROW_RACE, COL_RACE);
		else
			prt_field("             ", ROW_RACE, COL_RACE);
		/* prt_field(cp_ptr->title, ROW_CLASS, COL_CLASS); */

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

	if (p_ptr->redraw & (PR_CLEV))
	{
		p_ptr->redraw &= ~(PR_CLEV);
		prt_clevel();
	}

	if (p_ptr->redraw & (PR_CEXP))
	{
		p_ptr->redraw &= ~(PR_CEXP);
		prt_cexp();
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

	if (p_ptr->redraw & (PR_WEATHER))
	{
		p_ptr->redraw &= ~(PR_WEATHER);
		prt_weather();
	}

	if (p_ptr->redraw & (PR_HEALTH))
	{
		p_ptr->redraw &= ~(PR_HEALTH);
		health_redraw();
	}

	if (p_ptr->redraw & (PR_UHEALTH))
	{
		p_ptr->redraw &= ~(PR_UHEALTH);
		riding_health_redraw();
	}


	if (p_ptr->redraw & (PR_EXTRA))
	{
		p_ptr->redraw &= ~(PR_EXTRA);
		p_ptr->redraw &= ~(PR_HUNGER);
		p_ptr->redraw &= ~(PR_STATE | PR_SPEED | PR_STATUS);
		prt_frame_extra();
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
		fix_player();
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


byte empty_hands(void)
{
	s16b kaerichi = 0x00;

	if (!(inventory[INVEN_RARM].k_idx)) kaerichi |= EMPTY_HAND_RARM;
	if (!(inventory[INVEN_LARM].k_idx)) kaerichi |= EMPTY_HAND_LARM;
	return kaerichi;
}


bool heavy_armor(void)
{
	int ninja_level = p_ptr->cexp_info[CLASS_NINJA].clev + p_ptr->cexp_info[CLASS_NINJAMASTER].clev / 2;
	u16b monk_arm_wgt = 0;

	/* Weight the armor */
	if(inventory[INVEN_RARM].tval > TV_SWORD) monk_arm_wgt += inventory[INVEN_RARM].weight;
	if(inventory[INVEN_LARM].tval > TV_SWORD) monk_arm_wgt += inventory[INVEN_LARM].weight;
	monk_arm_wgt += inventory[INVEN_BODY].weight;
	monk_arm_wgt += inventory[INVEN_HEAD].weight;
	monk_arm_wgt += inventory[INVEN_OUTER].weight;
	monk_arm_wgt += inventory[INVEN_HANDS].weight;
	monk_arm_wgt += inventory[INVEN_FEET].weight;

	return (monk_arm_wgt > (100 + (ninja_level * 4)));
}

int number_of_quests(void)
{
	int i, j;

	/* Clear the counter */
	i = 0;

	for (j = MIN_RANDOM_QUEST; j <= MAX_RANDOM_QUEST_ASTRAL; j++)
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
