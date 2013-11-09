/* File: autopick.c */

/* Purpose: Object Auto-picker/Destroyer */

/*
 * Copyright (c) 2002  Mogami
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

#define MAX_LINELEN 1024

/*
 * Macros for Keywords
 */
#define FLG_ALL              0
#define FLG_UNIDENTIFIED     1
#define FLG_IDENTIFIED       2
#define FLG_STAR_IDENTIFIED  3
#define FLG_COLLECTING       4
#define FLG_BOOSTED          5
#define FLG_MORE_THAN        6
#define FLG_DICE             7
#define FLG_MORE_BONUS       8
#define FLG_MORE_BONUS2      9
#define FLG_ARTIFACT        10
#define FLG_EGO             11
#define FLG_GOOD            12
#define FLG_NAMELESS        13
#define FLG_AVERAGE         14
#define FLG_WORTHLESS       15
#define FLG_RARE            16
#define FLG_COMMON          17
#define FLG_UNAWARE         18
#define FLG_WANTED          19
#define FLG_UNIQUE          20
#define FLG_WALSTANIAN      21
#define FLG_GARGASTAN       22
#define FLG_BACRUM          23
#define FLG_ZENOBIAN        24
#define FLG_LODIS           25
#define FLG_METAL           26

#define FLG_ITEMS           30
#define FLG_WEAPONS         31
#define FLG_ARMORS          32
#define FLG_MISSILES        33
#define FLG_SHOTS           34
#define FLG_DEVICES         35
#define FLG_LIGHTS          36
#define FLG_JUNKS           37
#define FLG_ARCHER          38
#define FLG_SPELLBOOKS      39
#define FLG_SHIELDS         40
#define FLG_LAUNCHERS       41
#define FLG_RINGS           42
#define FLG_AMULETS         43
#define FLG_SUITS           44
#define FLG_CLOAKS          45
#define FLG_HELMS           46
#define FLG_GLOVES          47
#define FLG_BOOTS           48

#define FLG_SMALL_SWORDS    49
#define FLG_KATANAS         50
#define FLG_SWORDS          51
#define FLG_GREAT_SWORDS    52
#define FLG_AXES            53
#define FLG_SPEARS          54
#define FLG_LANCES          55
#define FLG_CLAWS           56
#define FLG_SCYTHES         57
#define FLG_WHIPS           58
#define FLG_HAMMERS         59
#define FLG_STAFFS          60
#define FLG_FANS            61
#define FLG_BOWS            62
#define FLG_GUNS            63


#ifdef JP

static char KEY_ALL[] = "すべての";
static char KEY_UNIDENTIFIED[] = "未鑑定の";
static char KEY_IDENTIFIED[] = "鑑定済みの";
static char KEY_STAR_IDENTIFIED[] = "*鑑定*済みの";
static char KEY_COLLECTING[] = "収集中の";
static char KEY_BOOSTED[] = "ダイス目の違う";
static char KEY_MORE_THAN[] = "ダイス目";
static char KEY_DICE[] = "以上の";
static char KEY_MORE_BONUS[] = "修正値";
static char KEY_MORE_BONUS2[] = "以上の";
static char KEY_ARTIFACT[] = "アーティファクト";
static char KEY_EGO[] = "エゴ";
static char KEY_GOOD[] = "上質の";
static char KEY_NAMELESS[] = "無銘の";
static char KEY_AVERAGE[] = "並の";
static char KEY_WORTHLESS[] = "無価値の";
static char KEY_RARE[] = "レアな";
static char KEY_COMMON[] = "ありふれた";
static char KEY_UNAWARE[] = "未判明の";
static char KEY_WANTED[] = "賞金首の";
static char KEY_UNIQUE[] = "ユニーク・モンスターの";
static char KEY_WALSTANIAN[] = "ウォルスタ人の";
static char KEY_GARGASTAN[] = "ガルガスタン人の";
static char KEY_BACRUM[] = "バクラム人の";
static char KEY_ZENOBIAN[] = "ゼノビア人の";
static char KEY_LODIS[] = "ローディス人の";
static char KEY_METAL[] = "金属製の";

static char KEY_ITEMS[] = "アイテム";
static char KEY_WEAPONS[] = "武器";
static char KEY_ARMORS[] = "防具";
static char KEY_MISSILES[] = "矢";
static char KEY_SHOTS[] = "弾丸";
static char KEY_DEVICES[] = "魔法アイテム";
static char KEY_LIGHTS[] = "光源";
static char KEY_JUNKS[] = "がらくた";
static char KEY_ARCHER[] = "材料";
static char KEY_SPELLBOOKS[] = "魔法書";
static char KEY_SHIELDS[] = "盾";
static char KEY_LAUNCHERS[] = "射撃武器";
static char KEY_RINGS[] = "指輪";
static char KEY_AMULETS[] = "アミュレット";
static char KEY_SUITS[] = "鎧";
static char KEY_CLOAKS[] = "クローク";
static char KEY_HELMS[] = "兜";
static char KEY_GLOVES[] = "籠手";
static char KEY_BOOTS[] = "靴";

static char KEY_SMALL_SWORDS[] = "小剣";
static char KEY_KATANAS[] = "カタナ";
static char KEY_SWORDS[] = "剣";
static char KEY_GREAT_SWORDS[] = "大剣";
static char KEY_AXES[] = "斧";
static char KEY_SPEARS[] = "槍";
static char KEY_LANCES[] = "乗馬槍";
static char KEY_CLAWS[] = "爪";
static char KEY_SCYTHES[] = "鎌";
static char KEY_WHIPS[] = "鞭";
static char KEY_HAMMERS[] = "ハンマー";
static char KEY_STAFFS[] = "杖";
static char KEY_FANS[] = "扇";
static char KEY_BOWS[] = "弓";
static char KEY_GUNS[] = "銃";

#else 

static char KEY_ALL[] = "all";
static char KEY_UNIDENTIFIED[] = "unidentified";
static char KEY_IDENTIFIED[] = "identified";
static char KEY_STAR_IDENTIFIED[] = "*identified*";
static char KEY_COLLECTING[] = "collecting";
static char KEY_BOOSTED[] = "dice boosted";
static char KEY_MORE_THAN[] = "more than";
static char KEY_DICE[] = " dice";
static char KEY_MORE_BONUS[] = "more bonus than";
static char KEY_MORE_BONUS2[] = "";
static char KEY_ARTIFACT[] = "artifact";
static char KEY_EGO[] = "ego";
static char KEY_GOOD[] = "good";
static char KEY_NAMELESS[] = "nameless";
static char KEY_AVERAGE[] = "average";
static char KEY_WORTHLESS[] = "worthless";
static char KEY_RARE[] = "rare";
static char KEY_COMMON[] = "common";
static char KEY_UNAWARE[] = "unaware";
static char KEY_WANTED[] = "wanted";
static char KEY_UNIQUE[] = "unique monster's";
static char KEY_WALSTANIAN[] = "walstanian";
static char KEY_GARGASTAN[] = "gargastan";
static char KEY_BACRUM[] = "bacrum";
static char KEY_ZENOBIAN[] = "zenobian";
static char KEY_LODIS[] = "lodis";
static char KEY_METAL[] = "metal";

static char KEY_ITEMS[] = "items";
static char KEY_WEAPONS[] = "weapons";
static char KEY_ARMORS[] = "armors";
static char KEY_MISSILES[] = "missiles";
static char KEY_SHOTS[] = "shots";
static char KEY_DEVICES[] = "magical devices";
static char KEY_LIGHTS[] = "lights";
static char KEY_JUNKS[] = "junks";
static char KEY_ARCHER[] = "ammo-materials";
static char KEY_SPELLBOOKS[] = "spellbooks";
static char KEY_SHIELDS[] = "shields";
static char KEY_LAUNCHERS[] = "launchers";
static char KEY_RINGS[] = "rings";
static char KEY_AMULETS[] = "amulets";
static char KEY_SUITS[] = "suits";
static char KEY_CLOAKS[] = "cloaks";
static char KEY_HELMS[] = "helms";
static char KEY_GLOVES[] = "gloves";
static char KEY_BOOTS[] = "boots";

static char KEY_SMALL_SWORDS[] = "small swords";
static char KEY_KATANAS[] = "katanas";
static char KEY_SWORDS[] = "swords";
static char KEY_GREAT_SWORDS[] = "great swords";
static char KEY_AXES[] = "axes";
static char KEY_SPEARS[] = "spears";
static char KEY_LANCES[] = "lances";
static char KEY_CLAWS[] = "claws";
static char KEY_SCYTHES[] = "scythes";
static char KEY_WHIPS[] = "whips";
static char KEY_HAMMERS[] = "hammers";
static char KEY_STAFFS[] = "staffs";
static char KEY_FANS[] = "fans";
static char KEY_BOWS[] = "bows";
static char KEY_GUNS[] = "guns";


#endif /* JP */

#define MATCH_KEY(KEY) (!strncmp(ptr, KEY, sizeof(KEY)-1)\
     ? (ptr += sizeof(KEY)-1, (' '==*ptr) ? ptr++ : 0, TRUE) : FALSE)
#define MATCH_KEY2(KEY) (!strncmp(ptr, KEY, sizeof(KEY)-1)\
     ? (prev_ptr = ptr, ptr += sizeof(KEY)-1, (' '==*ptr) ? ptr++ : 0, TRUE) : FALSE)

#ifdef JP
#define ADD_KEY(KEY) strcat(ptr, KEY)
#else
#define ADD_KEY(KEY) (strcat(ptr, KEY), strcat(ptr, " "))
#endif
#define ADD_KEY2(KEY) strcat(ptr, KEY)

#define ADD_FLG(FLG) (entry->flag[FLG / 32] |= (1L << (FLG % 32)))
#define REM_FLG(FLG) (entry->flag[FLG / 32] &= ~(1L << (FLG % 32)))
#define ADD_FLG_NOUN(FLG) (ADD_FLG(FLG), prev_flg = FLG)
#define IS_FLG(FLG) (entry->flag[FLG / 32] & (1L << (FLG % 32)))

#ifdef JP
	static char kanji_colon[] = "：";
#endif


/*
 * Reconstruct preference line from entry
 */
cptr autopick_line_from_entry(autopick_type *entry)
{
	char buf[MAX_LINELEN];
	char *ptr;
	bool sepa_flag = TRUE;

	*buf = '\0';
	if (!(entry->action & DO_DISPLAY)) strcat(buf, "(");
	if (entry->action & DO_QUERY_AUTOPICK) strcat(buf, ";");
	if (entry->action & DO_AUTODESTROY) strcat(buf, "!");
	if (entry->action & DONT_AUTOPICK) strcat(buf, "~");

	ptr = buf;

	if (IS_FLG(FLG_ALL)) ADD_KEY(KEY_ALL);
	if (IS_FLG(FLG_COLLECTING)) ADD_KEY(KEY_COLLECTING);
	if (IS_FLG(FLG_UNIDENTIFIED)) ADD_KEY(KEY_UNIDENTIFIED);
	if (IS_FLG(FLG_IDENTIFIED)) ADD_KEY(KEY_IDENTIFIED);
	if (IS_FLG(FLG_STAR_IDENTIFIED)) ADD_KEY(KEY_STAR_IDENTIFIED);
	if (IS_FLG(FLG_UNAWARE)) ADD_KEY(KEY_UNAWARE);
	if (IS_FLG(FLG_BOOSTED)) ADD_KEY(KEY_BOOSTED);

	if (IS_FLG(FLG_MORE_THAN))
	{
		ADD_KEY(KEY_MORE_THAN);
		strcat(ptr, format("%d", entry->dice));
		ADD_KEY(KEY_DICE);
	}

	if (IS_FLG(FLG_MORE_BONUS))
	{
		ADD_KEY(KEY_MORE_BONUS);
		strcat(ptr, format("%d", entry->bonus));
		ADD_KEY(KEY_MORE_BONUS2);
	}

	if (IS_FLG(FLG_WANTED)) ADD_KEY(KEY_WANTED);
	if (IS_FLG(FLG_UNIQUE)) ADD_KEY(KEY_UNIQUE);
	if (IS_FLG(FLG_WALSTANIAN)) ADD_KEY(KEY_WALSTANIAN);
	if (IS_FLG(FLG_GARGASTAN)) ADD_KEY(KEY_GARGASTAN);
	if (IS_FLG(FLG_BACRUM)) ADD_KEY(KEY_BACRUM);
	if (IS_FLG(FLG_ZENOBIAN)) ADD_KEY(KEY_ZENOBIAN);
	if (IS_FLG(FLG_LODIS)) ADD_KEY(KEY_LODIS);
	if (IS_FLG(FLG_METAL)) ADD_KEY(KEY_METAL);
	if (IS_FLG(FLG_WORTHLESS)) ADD_KEY(KEY_WORTHLESS);
	if (IS_FLG(FLG_GOOD)) ADD_KEY(KEY_GOOD);
	if (IS_FLG(FLG_NAMELESS)) ADD_KEY(KEY_NAMELESS);
	if (IS_FLG(FLG_AVERAGE)) ADD_KEY(KEY_AVERAGE);
	if (IS_FLG(FLG_RARE)) ADD_KEY(KEY_RARE);
	if (IS_FLG(FLG_COMMON)) ADD_KEY(KEY_COMMON);
	if (IS_FLG(FLG_EGO)) ADD_KEY(KEY_EGO);

	if (IS_FLG(FLG_ARTIFACT)) ADD_KEY(KEY_ARTIFACT);

	if (IS_FLG(FLG_ITEMS)) ADD_KEY2(KEY_ITEMS);
	else if (IS_FLG(FLG_WEAPONS)) ADD_KEY2(KEY_WEAPONS);
	else if (IS_FLG(FLG_ARMORS)) ADD_KEY2(KEY_ARMORS);
	else if (IS_FLG(FLG_MISSILES)) ADD_KEY2(KEY_MISSILES);
	else if (IS_FLG(FLG_SHOTS)) ADD_KEY2(KEY_SHOTS);
	else if (IS_FLG(FLG_DEVICES)) ADD_KEY2(KEY_DEVICES);
	else if (IS_FLG(FLG_LIGHTS)) ADD_KEY2(KEY_LIGHTS);
	else if (IS_FLG(FLG_JUNKS)) ADD_KEY2(KEY_JUNKS);
	else if (IS_FLG(FLG_ARCHER)) ADD_KEY2(KEY_ARCHER);
	else if (IS_FLG(FLG_SPELLBOOKS)) ADD_KEY2(KEY_SPELLBOOKS);
	else if (IS_FLG(FLG_SMALL_SWORDS)) ADD_KEY2(KEY_SMALL_SWORDS);
	else if (IS_FLG(FLG_KATANAS)) ADD_KEY2(KEY_KATANAS);
	else if (IS_FLG(FLG_SWORDS)) ADD_KEY2(KEY_SWORDS);
	else if (IS_FLG(FLG_GREAT_SWORDS)) ADD_KEY2(KEY_GREAT_SWORDS);
	else if (IS_FLG(FLG_AXES)) ADD_KEY2(KEY_AXES);
	else if (IS_FLG(FLG_SPEARS)) ADD_KEY2(KEY_SPEARS);
	else if (IS_FLG(FLG_LANCES)) ADD_KEY2(KEY_LANCES);
	else if (IS_FLG(FLG_CLAWS)) ADD_KEY2(KEY_CLAWS);
	else if (IS_FLG(FLG_SCYTHES)) ADD_KEY2(KEY_SCYTHES);
	else if (IS_FLG(FLG_WHIPS)) ADD_KEY2(KEY_WHIPS);
	else if (IS_FLG(FLG_HAMMERS)) ADD_KEY2(KEY_HAMMERS);
	else if (IS_FLG(FLG_STAFFS)) ADD_KEY2(KEY_STAFFS);
	else if (IS_FLG(FLG_FANS)) ADD_KEY2(KEY_FANS);
	else if (IS_FLG(FLG_BOWS)) ADD_KEY2(KEY_BOWS);
	else if (IS_FLG(FLG_GUNS)) ADD_KEY2(KEY_GUNS);
	else if (IS_FLG(FLG_SHIELDS)) ADD_KEY2(KEY_SHIELDS);
	else if (IS_FLG(FLG_LAUNCHERS)) ADD_KEY2(KEY_LAUNCHERS);
	else if (IS_FLG(FLG_RINGS)) ADD_KEY2(KEY_RINGS);
	else if (IS_FLG(FLG_AMULETS)) ADD_KEY2(KEY_AMULETS);
	else if (IS_FLG(FLG_SUITS)) ADD_KEY2(KEY_SUITS);
	else if (IS_FLG(FLG_CLOAKS)) ADD_KEY2(KEY_CLOAKS);
	else if (IS_FLG(FLG_HELMS)) ADD_KEY2(KEY_HELMS);
	else if (IS_FLG(FLG_GLOVES)) ADD_KEY2(KEY_GLOVES);
	else if (IS_FLG(FLG_BOOTS)) ADD_KEY2(KEY_BOOTS);

	/* You don't need sepalator after adjective */
	/* 'artifact' is not true adjective */
	else if (!IS_FLG(FLG_ARTIFACT))
		sepa_flag = FALSE;

	if (entry->name && entry->name[0])
	{
		int i, j = 0;

		if (sepa_flag) strcat(buf, ":");

		i = strlen(buf);
		while (entry->name[j] && i < MAX_LINELEN - 2 - 1)
		{
#ifdef JP
			if (iskanji(entry->name[j]))
				buf[i++] = entry->name[j++];
#endif
			buf[i++] = entry->name[j++];
		}
		buf[i] = '\0';
	}

	if (entry->insc)
	{
		int i, j = 0;
		strcat(buf, "#");
		i = strlen(buf);

		while (entry->insc[j] && i < MAX_LINELEN - 2)
		{
#ifdef JP
			if (iskanji(entry->insc[j]))
				buf[i++] = entry->insc[j++];
#endif
			buf[i++] = entry->insc[j++];
		}
		buf[i] = '\0';
	}

	return string_make(buf);
}


/*
 * A function to create new entry
 */
static bool autopick_new_entry(autopick_type *entry, cptr str)
{
	cptr insc;
	int i;
	byte act = 0;
	char buf[MAX_LINELEN];
	cptr prev_ptr, ptr, old_ptr;
	int prev_flg;

	if (str[1] == ':') switch (str[0])
	{
	case '?': case '%':
	case 'A': case 'P': case 'C':
		return FALSE;
	}

	entry->flag[0] = entry->flag[1] = 0L;
	entry->dice = 0;

	act = DO_AUTOPICK | DO_DISPLAY;
	while (1)
	{
		if ((act & DO_AUTOPICK) && *str == '!')
		{
			act &= ~DO_AUTOPICK;
			act |= DO_AUTODESTROY;
			str++;
		}
		else if ((act & DO_AUTOPICK) && *str == '~')
		{
			act &= ~DO_AUTOPICK;
			act |= DONT_AUTOPICK;
			str++;
		}
		else if ((act & DO_AUTOPICK) && *str == ';')
		{
			act &= ~DO_AUTOPICK;
			act |= DO_QUERY_AUTOPICK;
			str++;
		}
		else if ((act & DO_DISPLAY) && *str == '(')
		{
			act &= ~DO_DISPLAY;
			str++;
		}
		else
			break;
	}

	/* don't mind upper or lower case */
	insc = NULL;
	for (i = 0; *str; i++)
	{
		char c = *str++;
#ifdef JP
		if (iskanji(c))
		{
			buf[i++] = c;
			buf[i] = *str++;
			continue;
		}
#endif
		/* Auto-inscription? */
		if (c == '#')
		{
			buf[i] = '\0';
			insc = str;
			break;
		}

		if (isupper(c)) c = tolower(c);

		buf[i] = c;
	}
	buf[i] = '\0';
	
	/* Skip empty line */
	if (*buf == 0) return FALSE;

	ptr = prev_ptr = buf;
	old_ptr = NULL;

	while (old_ptr != ptr)
	{
		/* Save current location */
		old_ptr = ptr;

		if (MATCH_KEY(KEY_ALL)) ADD_FLG(FLG_ALL);
		if (MATCH_KEY(KEY_COLLECTING)) ADD_FLG(FLG_COLLECTING);
		if (MATCH_KEY(KEY_UNIDENTIFIED)) ADD_FLG(FLG_UNIDENTIFIED);
		if (MATCH_KEY(KEY_IDENTIFIED)) ADD_FLG(FLG_IDENTIFIED);
		if (MATCH_KEY(KEY_STAR_IDENTIFIED)) ADD_FLG(FLG_STAR_IDENTIFIED);
		if (MATCH_KEY(KEY_BOOSTED)) ADD_FLG(FLG_BOOSTED);

		/*** Weapons whose dd*ds is more than nn ***/
		if (MATCH_KEY2(KEY_MORE_THAN))
		{
			int k = 0;
			entry->dice = 0;

			/* Drop leading spaces */
			while (' ' == *ptr) ptr++;

			/* Read number */
			while ('0' <= *ptr && *ptr <= '9')
			{
				entry->dice = 10 * entry->dice + (*ptr - '0');
				ptr++;
				k++;
			}

			if (k > 0 && k <= 2)
			{
				(void)MATCH_KEY(KEY_DICE);
				ADD_FLG(FLG_MORE_THAN);
			}
			else
				ptr = prev_ptr;
		}

		/*** Items whose magical bonus is more than n ***/
		if (MATCH_KEY2(KEY_MORE_BONUS))
		{
			int k = 0;
			entry->bonus = 0;

			/* Drop leading spaces */
			while (' ' == *ptr) ptr++;

			/* Read number */
			while ('0' <= *ptr && *ptr <= '9')
			{
				entry->bonus = 10 * entry->bonus + (*ptr - '0');
				ptr++;
				k++;
			}

			if (k > 0 && k <= 2)
			{
				(void)MATCH_KEY(KEY_MORE_BONUS2);
				ADD_FLG(FLG_MORE_BONUS);
			}
			else
				ptr = prev_ptr;
		}

		if (MATCH_KEY(KEY_WORTHLESS)) ADD_FLG(FLG_WORTHLESS);
		if (MATCH_KEY(KEY_EGO)) ADD_FLG(FLG_EGO);
		if (MATCH_KEY(KEY_GOOD)) ADD_FLG(FLG_GOOD);
		if (MATCH_KEY(KEY_NAMELESS)) ADD_FLG(FLG_NAMELESS);
		if (MATCH_KEY(KEY_AVERAGE)) ADD_FLG(FLG_AVERAGE);
		if (MATCH_KEY(KEY_RARE)) ADD_FLG(FLG_RARE);
		if (MATCH_KEY(KEY_COMMON)) ADD_FLG(FLG_COMMON);
		if (MATCH_KEY(KEY_UNAWARE)) ADD_FLG(FLG_UNAWARE);
		if (MATCH_KEY(KEY_WANTED)) ADD_FLG(FLG_WANTED);
		if (MATCH_KEY(KEY_UNIQUE)) ADD_FLG(FLG_UNIQUE);
		if (MATCH_KEY(KEY_WALSTANIAN)) ADD_FLG(FLG_WALSTANIAN);
		if (MATCH_KEY(KEY_GARGASTAN)) ADD_FLG(FLG_GARGASTAN);
		if (MATCH_KEY(KEY_BACRUM)) ADD_FLG(FLG_BACRUM);
		if (MATCH_KEY(KEY_ZENOBIAN)) ADD_FLG(FLG_ZENOBIAN);
		if (MATCH_KEY(KEY_LODIS)) ADD_FLG(FLG_LODIS);
		if (MATCH_KEY(KEY_METAL)) ADD_FLG(FLG_METAL);
	}

	/* Not yet found any noun */
	prev_flg = -1;

	if (MATCH_KEY2(KEY_ARTIFACT)) ADD_FLG_NOUN(FLG_ARTIFACT);

	if (MATCH_KEY2(KEY_ITEMS)) ADD_FLG_NOUN(FLG_ITEMS);
	else if (MATCH_KEY2(KEY_WEAPONS)) ADD_FLG_NOUN(FLG_WEAPONS);
	else if (MATCH_KEY2(KEY_ARMORS)) ADD_FLG_NOUN(FLG_ARMORS);
	else if (MATCH_KEY2(KEY_MISSILES)) ADD_FLG_NOUN(FLG_MISSILES);
	else if (MATCH_KEY2(KEY_SHOTS)) ADD_FLG_NOUN(FLG_SHOTS);
	else if (MATCH_KEY2(KEY_DEVICES)) ADD_FLG_NOUN(FLG_DEVICES);
	else if (MATCH_KEY2(KEY_LIGHTS)) ADD_FLG_NOUN(FLG_LIGHTS);
	else if (MATCH_KEY2(KEY_JUNKS)) ADD_FLG_NOUN(FLG_JUNKS);
	else if (MATCH_KEY2(KEY_ARCHER)) ADD_FLG_NOUN(FLG_ARCHER);
	else if (MATCH_KEY2(KEY_SPELLBOOKS)) ADD_FLG_NOUN(FLG_SPELLBOOKS);
	else if (MATCH_KEY2(KEY_SMALL_SWORDS)) ADD_FLG_NOUN(FLG_SMALL_SWORDS);
	else if (MATCH_KEY2(KEY_KATANAS)) ADD_FLG_NOUN(FLG_KATANAS);
	else if (MATCH_KEY2(KEY_SWORDS)) ADD_FLG_NOUN(FLG_SWORDS);
	else if (MATCH_KEY2(KEY_GREAT_SWORDS)) ADD_FLG_NOUN(FLG_GREAT_SWORDS);
	else if (MATCH_KEY2(KEY_AXES)) ADD_FLG_NOUN(FLG_AXES);
	else if (MATCH_KEY2(KEY_SPEARS)) ADD_FLG_NOUN(FLG_SPEARS);
	else if (MATCH_KEY2(KEY_LANCES)) ADD_FLG_NOUN(FLG_LANCES);
	else if (MATCH_KEY2(KEY_CLAWS)) ADD_FLG_NOUN(FLG_CLAWS);
	else if (MATCH_KEY2(KEY_SCYTHES)) ADD_FLG_NOUN(FLG_SCYTHES);
	else if (MATCH_KEY2(KEY_WHIPS)) ADD_FLG_NOUN(FLG_WHIPS);
	else if (MATCH_KEY2(KEY_HAMMERS)) ADD_FLG_NOUN(FLG_HAMMERS);
	else if (MATCH_KEY2(KEY_STAFFS)) ADD_FLG_NOUN(FLG_STAFFS);
	else if (MATCH_KEY2(KEY_FANS)) ADD_FLG_NOUN(FLG_FANS);
	else if (MATCH_KEY2(KEY_BOWS)) ADD_FLG_NOUN(FLG_BOWS);
	else if (MATCH_KEY2(KEY_GUNS)) ADD_FLG_NOUN(FLG_GUNS);
	else if (MATCH_KEY2(KEY_SHIELDS)) ADD_FLG_NOUN(FLG_SHIELDS);
	else if (MATCH_KEY2(KEY_LAUNCHERS)) ADD_FLG_NOUN(FLG_LAUNCHERS);
	else if (MATCH_KEY2(KEY_RINGS)) ADD_FLG_NOUN(FLG_RINGS);
	else if (MATCH_KEY2(KEY_AMULETS)) ADD_FLG_NOUN(FLG_AMULETS);
	else if (MATCH_KEY2(KEY_SUITS)) ADD_FLG_NOUN(FLG_SUITS);
	else if (MATCH_KEY2(KEY_CLOAKS)) ADD_FLG_NOUN(FLG_CLOAKS);
	else if (MATCH_KEY2(KEY_HELMS)) ADD_FLG_NOUN(FLG_HELMS);
	else if (MATCH_KEY2(KEY_GLOVES)) ADD_FLG_NOUN(FLG_GLOVES);
	else if (MATCH_KEY2(KEY_BOOTS)) ADD_FLG_NOUN(FLG_BOOTS);

	/* Last 'keyword' must be at the correct location */
	if (*ptr == ':')
		ptr++;
#ifdef JP
	else if (ptr[0] == kanji_colon[0] && ptr[1] == kanji_colon[1])
		ptr += 2;
#endif
	else if (*ptr == '\0')
		; /* nothing to do */
	else
	{
		/* Noun type? */
		if (prev_flg != -1)
		{
			entry->flag[(prev_flg / 32)] &= ~(1L << (prev_flg % 32));
			ptr = prev_ptr;
		}
	}

	/* Save this auto-picker entry line */
	entry->name = string_make(ptr);
	entry->action = act;
	entry->insc = string_make(insc);

	return TRUE;
}

/*
 * A function to delete entry
 */
static void autopick_free_entry(autopick_type *entry)
{
	string_free(entry->name);
	string_free(entry->insc);
}


/*
 * A function for Auto-picker/destroyer
 * Examine whether the object matches to the entry
 */
static bool is_autopick_aux(object_type *o_ptr, autopick_type *entry, cptr o_name)
{
	int j;
	cptr ptr = entry->name;
	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	/*** Unidentified ***/
	if (IS_FLG(FLG_UNIDENTIFIED)
	    && (object_is_known(o_ptr) || (o_ptr->ident & IDENT_SENSE)))
		return FALSE;

	/*** Identified ***/
	if (IS_FLG(FLG_IDENTIFIED) && !object_is_known(o_ptr))
		return FALSE;

	/*** *Identified* ***/
	if (IS_FLG(FLG_STAR_IDENTIFIED) &&
	    (!object_is_known(o_ptr) || !(o_ptr->ident & IDENT_MENTAL)))
		return FALSE;

	/*** Dice boosted (weapon of slaying) ***/
	if (IS_FLG(FLG_BOOSTED))
	{
		switch( o_ptr->tval )
		{
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_DIGGING:
			if (object_is_known(o_ptr) && ((o_ptr->dd != k_ptr->dd) || (o_ptr->ds != k_ptr->ds)))
				break;
			else
				return FALSE;
		default:
			return FALSE;
		}
	}

	/*** Weapons which dd*ds is more than nn ***/
	if (IS_FLG(FLG_MORE_THAN))
	{
		if (object_is_known(o_ptr))
		{
			if (o_ptr->dd * o_ptr->ds < entry->dice)
				return FALSE;
		}
		else
		{
			if (k_ptr->dd * k_ptr->ds < entry->dice)
				return FALSE;
		}
	}

	/*** Object which bonus is more than nn ***/
	if (IS_FLG(FLG_MORE_BONUS))
	{
		int i;
		bool qualify = FALSE;

		if (!object_is_known(o_ptr)) return FALSE;

		for (i = 0; (i < A_MAX) && !qualify; i++)
		{
			if (o_ptr->to_stat[i] >= entry->bonus) qualify = TRUE;
		}
		for (i = 0; (i < OB_MAX) && !qualify; i++)
		{
			if (o_ptr->to_misc[i] >= entry->bonus) qualify = TRUE;
		}

		if (o_ptr->to_h < entry->bonus &&
		    o_ptr->to_d < entry->bonus &&
		    o_ptr->to_a < entry->bonus &&
		    !qualify)
			return FALSE;
	}

	/*** Worthless items ***/
	if (IS_FLG(FLG_WORTHLESS) && object_value(o_ptr) > 0)
		return FALSE;

	/*** Artifact object ***/
	if (IS_FLG(FLG_ARTIFACT))
	{
		if (!object_is_known(o_ptr) || !object_is_artifact(o_ptr))
			return FALSE;
	}

	/*** Ego object ***/
	if (IS_FLG(FLG_EGO))
	{
		if (!object_is_known(o_ptr) || !object_is_ego(o_ptr))
			return FALSE;
	}

	/*** Good ***/
	if (IS_FLG(FLG_GOOD))
	{
		if (!object_is_equipment(o_ptr)) return FALSE;

		/* Identified */
		if (object_is_known(o_ptr))
		{
			/* Artifacts and Ego objects are not okay */
			if (o_ptr->inscription || object_is_artifact(o_ptr) || object_is_ego(o_ptr))
				return FALSE;

			/* Average are not okay */
			if (o_ptr->to_a <= 0 && (o_ptr->to_h + o_ptr->to_d) <= 0)
				return FALSE;
		}

		/* Pseudo-identified */
		else if (o_ptr->ident & IDENT_SENSE)
		{
			switch (o_ptr->feeling)
			{
			case FEEL_GOOD:
				/* It's good */
				break;

			default:
				/* It's not good */
				return FALSE;
			}
		}

		/* Unidentified */
		else
		{
			/* Not known to be good */
			return FALSE;
		}
	}

	/*** Nameless ***/
	if (IS_FLG(FLG_NAMELESS))
	{
		if (!object_is_equipment(o_ptr)) return FALSE;

		/* Identified */
		if (object_is_known(o_ptr))
		{
			/* Artifacts and Ego objects are not okay */
			if (o_ptr->inscription || object_is_artifact(o_ptr) || object_is_ego(o_ptr))
				return FALSE;

		}

		/* Pseudo-identified */
		else if (o_ptr->ident & IDENT_SENSE)
		{
			switch (o_ptr->feeling)
			{
			case FEEL_AVERAGE:
			case FEEL_GOOD:
			case FEEL_BROKEN:
			case FEEL_CURSED:
				/* It's nameless */
				break;

			default:
				/* It's not nameless */
				return FALSE;
			}
		}

		/* Unidentified */
		else
		{
			/* Not known to be nameless */
			return FALSE;
		}
	}

	/*** Unaware items ***/
	if (IS_FLG(FLG_UNAWARE) && object_is_aware(o_ptr))
		return FALSE;

	/*** Average ***/
	if (IS_FLG(FLG_AVERAGE))
	{
		if (!object_is_equipment(o_ptr)) return FALSE;

		/* Identified */
		if (object_is_known(o_ptr))
		{
			/* Artifacts and Ego objects are not okay */
			if (o_ptr->inscription || object_is_artifact(o_ptr) || object_is_ego(o_ptr))
				return FALSE;

			/* Cursed or broken objects are not okay */
			if (object_is_cursed(o_ptr) || object_is_broken(o_ptr))
				return FALSE;

			/* Good are not okay */
			if (o_ptr->to_a > 0 || (o_ptr->to_h + o_ptr->to_d) > 0)
				return FALSE;
		}

		/* Pseudo-identified */
		else if (o_ptr->ident & IDENT_SENSE)
		{
			switch (o_ptr->feeling)
			{
			case FEEL_AVERAGE:
				/* It's average */
				break;

			default:
				/* It's not average */
				return FALSE;
			}
		}

		/* Unidentified */
		else
		{
			/* Not known to be average */
			return FALSE;
		}
	}

	/*** Rere equpiments ***/
	if (IS_FLG(FLG_RARE) && !object_is_rare(o_ptr))
		return FALSE;

	/*** Common equpiments ***/
	if (IS_FLG(FLG_COMMON) && object_is_rare(o_ptr))
		return FALSE;

	/*** Wanted monster's corpse/skeletons ***/
	if (IS_FLG(FLG_WANTED) &&
	    (o_ptr->tval != TV_CORPSE || !object_is_shoukinkubi(o_ptr)))
		return FALSE;

	/*** Unique monster's corpse/skeletons/statues ***/
	if (IS_FLG(FLG_UNIQUE) &&
	    ((o_ptr->tval != TV_CORPSE && o_ptr->tval != TV_STATUE) ||
	     !(r_info[o_ptr->pval].flags1 & RF1_UNIQUE)))
		return FALSE;

	/*** Walstanian Unique's statues ***/
	if (IS_FLG(FLG_WALSTANIAN) &&
	    ((o_ptr->tval != TV_STATUE) ||
	     !(r_info[o_ptr->pval].flags2 & RF2_WALSTANIAN) ||
	     !(r_info[o_ptr->pval].flags1 & RF1_UNIQUE)))
		return FALSE;

	/*** Gargastan Unique's statues ***/
	if (IS_FLG(FLG_GARGASTAN) &&
	    ((o_ptr->tval != TV_STATUE) ||
	     !(r_info[o_ptr->pval].flags2 & RF2_GARGASTAN) ||
	     !(r_info[o_ptr->pval].flags1 & RF1_UNIQUE)))
		return FALSE;

	/*** Bacrum Unique's statues ***/
	if (IS_FLG(FLG_BACRUM) &&
	    ((o_ptr->tval != TV_STATUE) ||
	     !(r_info[o_ptr->pval].flags2 & RF2_BACRUM) ||
	     !(r_info[o_ptr->pval].flags1 & RF1_UNIQUE)))
		return FALSE;

	/*** Zenobian Unique's statues ***/
	if (IS_FLG(FLG_ZENOBIAN) &&
	    ((o_ptr->tval != TV_STATUE) ||
	     !(r_info[o_ptr->pval].flags2 & RF2_ZENOBIAN) ||
	     !(r_info[o_ptr->pval].flags1 & RF1_UNIQUE)))
		return FALSE;

	/*** Lodis Unique's statues ***/
	if (IS_FLG(FLG_LODIS) &&
	    ((o_ptr->tval != TV_STATUE) ||
	     !(r_info[o_ptr->pval].flags2 & RF2_LODIS) ||
	     !(r_info[o_ptr->pval].flags1 & RF1_UNIQUE)))
		return FALSE;

	/*** Metal items ***/
	if (IS_FLG(FLG_METAL) && !object_is_metal(o_ptr))
		return FALSE;

	/*** Items ***/
	if (IS_FLG(FLG_WEAPONS))
	{
		switch(o_ptr->tval)
		{
		case TV_BOW: case TV_HAFTED: case TV_POLEARM:
		case TV_SWORD: case TV_DIGGING:
			break;
		default: return FALSE;
		}
	}
	else if (IS_FLG(FLG_ARMORS))
	{
		switch(o_ptr->tval)
		{
		case TV_BOOTS: case TV_GLOVES: case TV_CLOAK: case TV_CROWN:
		case TV_HELM: case TV_SHIELD: case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
			break;
		default: return FALSE;
		}
	}
	else if (IS_FLG(FLG_MISSILES))
	{
		switch(o_ptr->tval)
		{
		case TV_BOLT: case TV_ARROW:
			break;
		default: return FALSE;
		}
	}
	else if (IS_FLG(FLG_SHOTS))
	{
		switch(o_ptr->tval)
		{
		case TV_BULLET: case TV_ROUND: case TV_SHELL: case TV_ROCKET:
			break;
		default: return FALSE;
		}
	}
	else if (IS_FLG(FLG_DEVICES))
	{
		switch(o_ptr->tval)
		{
		case TV_SCROLL: case TV_STAFF: case TV_WAND: case TV_ROD:
			break;
		default: return FALSE;
		}
	}
	else if (IS_FLG(FLG_LIGHTS))
	{
		if (!(o_ptr->tval == TV_LITE))
			return FALSE;
	}
	else if (IS_FLG(FLG_JUNKS))
	{
		switch(o_ptr->tval)
		{
		case TV_SKELETON: case TV_BOTTLE:
		case TV_JUNK: case TV_STATUE:
			break;
		default: return FALSE;
		}
	}
	else if (IS_FLG(FLG_ARCHER))
	{
		if (!object_is_convertible(o_ptr))
			return FALSE;
	}
	else if (IS_FLG(FLG_SPELLBOOKS))
	{
		if (!(o_ptr->tval >= TV_MAGERY_BOOK))
			return FALSE;
	}
	else if (IS_FLG(FLG_SMALL_SWORDS))
	{
		if (get_weapon_type(k_ptr) != WT_SMALL_SWORD) return FALSE;
	}
	else if (IS_FLG(FLG_KATANAS))
	{
		if (get_weapon_type(k_ptr) != WT_KATANA) return FALSE;
	}
	else if (IS_FLG(FLG_SWORDS))
	{
		if (get_weapon_type(k_ptr) != WT_SWORD) return FALSE;
	}
	else if (IS_FLG(FLG_GREAT_SWORDS))
	{
		if (get_weapon_type(k_ptr) != WT_GREAT_SWORD) return FALSE;
	}
	else if (IS_FLG(FLG_AXES))
	{
		if (get_weapon_type(k_ptr) != WT_AXE) return FALSE;
	}
	else if (IS_FLG(FLG_SPEARS))
	{
		if (get_weapon_type(k_ptr) != WT_SPEAR) return FALSE;
	}
	else if (IS_FLG(FLG_LANCES))
	{
		if (get_weapon_type(k_ptr) != WT_LANCE) return FALSE;
	}
	else if (IS_FLG(FLG_CLAWS))
	{
		if (get_weapon_type(k_ptr) != WT_CLAW) return FALSE;
	}
	else if (IS_FLG(FLG_SCYTHES))
	{
		if (get_weapon_type(k_ptr) != WT_SCYTHE) return FALSE;
	}
	else if (IS_FLG(FLG_WHIPS))
	{
		if (get_weapon_type(k_ptr) != WT_WHIP) return FALSE;
	}
	else if (IS_FLG(FLG_HAMMERS))
	{
		if (get_weapon_type(k_ptr) != WT_HAMMER) return FALSE;
	}
	else if (IS_FLG(FLG_STAFFS))
	{
		if (get_weapon_type(k_ptr) != WT_STAFF) return FALSE;
	}
	else if (IS_FLG(FLG_FANS))
	{
		if (get_weapon_type(k_ptr) != WT_FAN) return FALSE;
	}
	else if (IS_FLG(FLG_BOWS))
	{
		if (get_weapon_type(k_ptr) != WT_BOW) return FALSE;
	}
	else if (IS_FLG(FLG_GUNS))
	{
		if (get_weapon_type(k_ptr) != WT_GUN) return FALSE;
	}
	else if (IS_FLG(FLG_SHIELDS))
	{
		if (!(o_ptr->tval == TV_SHIELD))
			return FALSE;
	}
	else if (IS_FLG(FLG_LAUNCHERS))
	{
		if (!(o_ptr->tval == TV_BOW))
			return FALSE;
	}
	else if (IS_FLG(FLG_RINGS))
	{
		if (!(o_ptr->tval == TV_RING))
			return FALSE;
	}
	else if (IS_FLG(FLG_AMULETS))
	{
		if (!(o_ptr->tval == TV_AMULET))
			return FALSE;
	}
	else if (IS_FLG(FLG_SUITS))
	{
		if (!(o_ptr->tval == TV_HARD_ARMOR ||
		      o_ptr->tval == TV_SOFT_ARMOR))
			return FALSE;
	}
	else if (IS_FLG(FLG_CLOAKS))
	{
		if (!(o_ptr->tval == TV_CLOAK))
			return FALSE;
	}
	else if (IS_FLG(FLG_HELMS))
	{
		if (!(o_ptr->tval == TV_CROWN || o_ptr->tval == TV_HELM))
			return FALSE;
	}
	else if (IS_FLG(FLG_GLOVES))
	{
		if (!(o_ptr->tval == TV_GLOVES))
			return FALSE;
	}
	else if (IS_FLG(FLG_BOOTS))
	{
		if (!(o_ptr->tval == TV_BOOTS))
			return FALSE;
	}

	/* Keyword don't match */
	if (*ptr == '^')
	{
		ptr++;
		if (strncmp(o_name, ptr, strlen(ptr))) return FALSE;
	}
	else
	{
		if (!my_strstr(o_name, ptr)) return FALSE;
	}

	/* TRUE when it need not to be 'collecting' */
	if (!IS_FLG(FLG_COLLECTING)) return TRUE;

	/* Check if there is a same item */
	for (j = 0; j < INVEN_PACK; j++)
	{
		/*
		 * 'Collecting' means the item must be absorbed 
		 * into an inventory slot.
		 * But an item can not be absorbed into itself!
		 */
		if ((&inventory[j] != o_ptr) &&
		    object_similar(&inventory[j], o_ptr))
			return TRUE;
	}

	/* Not collecting */
	return FALSE;
}


/*
 * A function for Auto-picker/destroyer
 * Examine whether the object matches to the list of keywords or not.
 */
int is_autopick(object_type *o_ptr)
{
	int i;
	char o_name[MAX_NLEN];

	if (o_ptr->tval == TV_GOLD) return -1;

	object_desc(o_name, o_ptr, (OD_NO_FLAVOR | OD_OMIT_PREFIX | OD_NO_PLURAL));

	/* Force to be lower case string */
	for (i = 0; o_name[i]; i++)
	{
#ifdef JP
		if (iskanji(o_name[i]))
			i++;
		else
#endif
		if (isupper(o_name[i]))
			o_name[i] = tolower(o_name[i]);
	}

	for (i=0; i < max_autopick; i++)
	{
		autopick_type *entry = &autopick_list[i];

		if (is_autopick_aux(o_ptr, entry, o_name)) return i;
	}

	/* No matching entry */
	return -1;
}


/*
 * Automatically destroy items in this grid.
 */
static bool is_opt_confirm_destroy(object_type *o_ptr)
{
	if (!destroy_items) return FALSE;

	/* Known to be worthless? */
	if (leave_worth)
		if (object_value(o_ptr) > 0) return FALSE;

	if (leave_equip)
		if ((o_ptr->tval >= TV_BULLET) && (o_ptr->tval <= TV_HARD_ARMOR)) return FALSE;

	if (leave_chest)
		if ((o_ptr->tval == TV_CHEST) && o_ptr->pval) return FALSE;

	if (leave_wanted)
	{
		if (o_ptr->tval == TV_CORPSE
		    && object_is_shoukinkubi(o_ptr)) return FALSE;
	}

	if (leave_corpse)
		if (o_ptr->tval == TV_CORPSE) return FALSE;

	if (leave_special)
	{
		if ((p_ptr->pclass == CLASS_ARCHER) || (p_ptr->pclass == CLASS_CRESCENT))
		{
			if (object_is_convertible(o_ptr))
				return FALSE;
		}

		else if (p_ptr->pclass == CLASS_GUNNER)
		{
			if (object_is_metal(o_ptr))
				return FALSE;
		}

		else if ((p_ptr->pclass == CLASS_LICH) || (p_ptr->pclass == CLASS_VAMPIRE))
		{
			if ((o_ptr->tval == TV_SCROLL) && (o_ptr->sval == SV_SCROLL_UNHOLY_WEAPON))
				return FALSE;
		}
	}

	if (leave_junk)
		if ((o_ptr->tval == TV_SKELETON) || (o_ptr->tval == TV_BOTTLE) || (o_ptr->tval == TV_JUNK) || (o_ptr->tval == TV_STATUE)) return FALSE;

	if (o_ptr->tval == TV_GOLD) return FALSE;

	return TRUE;
}


/*
 *  Hack - AUX of auto inscription
 */
bool auto_inscribe_object(object_type *o_ptr, int idx)
{
	/* Auto-inscription or Re-inscribe for resistances {%} */
	if ((idx < 0 || !autopick_list[idx].insc) && !o_ptr->inscription)
		return FALSE;

	if (!o_ptr->inscription)
		o_ptr->inscription = quark_add(autopick_list[idx].insc);

	return TRUE;
}


/*
 *  Auto inscription
 */
void auto_inscribe_item(int item, int idx)
{
	object_type *o_ptr;

	/* Get the item (in the pack) */
	if (item >= 0) o_ptr = &inventory[item];

	/* Get the item (on the floor) */
	else o_ptr = &o_list[0 - item];

	if (!auto_inscribe_object(o_ptr, idx)) return;

	if (item > INVEN_PACK)
	{
		/* Redraw inscription */
		p_ptr->window |= (PW_EQUIP);

		/* {.} and {$} effect p_ptr->warning and TRC_TELEPORT_SELF */
		p_ptr->update |= (PU_BONUS);
	}
	else if (item >= 0)
	{
		/* Redraw inscription */
		p_ptr->window |= (PW_INVEN);
	}
}


/*
 * Automatically destroy an item if it is to be destroyed
 */
bool auto_destroy_item(int item, int autopick_idx)
{
	bool destroy = FALSE;
	object_type *o_ptr;

	/* Don't destroy equipped items */
	if (item > INVEN_PACK) return FALSE;

	/* Get the item (in the pack) */
	if (item >= 0) o_ptr = &inventory[item];

	/* Get the item (on the floor) */
	else o_ptr = &o_list[0 - item];

	/* Easy-Auto-Destroyer */
	if (is_opt_confirm_destroy(o_ptr)) destroy = TRUE;

	/* Protected by auto-picker */
	if (autopick_idx >= 0 &&
	    !(autopick_list[autopick_idx].action & DO_AUTODESTROY))
		destroy = FALSE;

	if (!always_pickup)
	{
		/* Auto-picker/destroyer */
		if (autopick_idx >= 0 &&
		    (autopick_list[autopick_idx].action & DO_AUTODESTROY))
			destroy = TRUE;
	}

	/* Not to be destroyed */
	if (!destroy) return FALSE;

	/* Now decided to destroy */

	disturb(0,0);

	/* Artifact? */
	if (!can_player_destroy_object(o_ptr))
	{
		char o_name[MAX_NLEN];

		/* Describe the object (with {terrible/special}) */
		object_desc(o_name, o_ptr, 0);

		/* Message */
#ifdef JP
		msg_format("%sは破壊不能だ。", o_name);
#else
		msg_format("You cannot auto-destroy %s.", o_name);
#endif

		/* Done */
		return TRUE;
	}

	/* Destroy Later */
	o_ptr->marked |= OM_AUTODESTROY;
	p_ptr->notice |= PN_AUTODESTROY;

	return TRUE;
}


/*
 *  Auto-destroy marked item
 */
static void delayed_auto_destroy_aux(int item)
{
	object_type *o_ptr;

	/* Get the item (in the pack) */
	if (item >= 0) o_ptr = &inventory[item];

	/* Get the item (on the floor) */
	else o_ptr = &o_list[0 - item];

	if (o_ptr->k_idx && (o_ptr->marked & OM_AUTODESTROY))
	{
		char o_name[MAX_NLEN];

		/* Describe the object (with {terrible/special}) */
		object_desc(o_name, o_ptr, 0);

		/* Eliminate the item (from the pack) */
		if (item >= 0)
		{
			inven_item_increase(item, -(o_ptr->number));
			inven_item_optimize(item);
		}

		/* Eliminate the item (from the floor) */
		else
		{
			delete_object_idx(0 - item);
		}

		/* Print a message */
#ifdef JP
		msg_format("%sを自動破壊します。", o_name);
#else
		msg_format("Auto-destroying %s.", o_name);
#endif
	}
}


/*
 *  Auto-destroy marked item in inventory and on floor
 */
void delayed_auto_destroy(void)
{
	int item;

	/* 
	 * Scan inventory in reverse order to prevent
	 * skipping after inven_item_optimize()
	 */
	for (item = INVEN_TOTAL - 1; item >= 0 ; item--)
		delayed_auto_destroy_aux(item);

	/* Scan the pile of objects */
	item = cave[py][px].o_idx;
	while (item)
	{
		int next = o_list[item].next_o_idx;
		delayed_auto_destroy_aux(-item);
		item = next;
	}
}


/*
 * Automatically pickup/destroy items in this grid.
 */
void auto_pickup_items(cave_type *c_ptr)
{
	s16b this_o_idx, next_o_idx = 0;

	/* Scan the pile of objects */
	for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		int idx, old_idx;

		/* Acquire object */
		object_type *o_ptr = &o_list[this_o_idx];

		/* Acquire next object */
		next_o_idx = o_ptr->next_o_idx;

		old_idx = is_autopick(o_ptr);

		/* Item index for floor -1,-2,-3,...  */
		auto_inscribe_item((-this_o_idx), old_idx);

		/* Sense the object */
		sense_floor_object(this_o_idx);

		idx = is_autopick(o_ptr);

		if (old_idx >= 0 &&
			(autopick_list[old_idx].action & (DO_AUTOPICK | DO_QUERY_AUTOPICK)))
		{
			disturb(0,0);

			if (!inven_carry_okay(o_ptr))
			{
				char o_name[MAX_NLEN];

				/* Describe the object */
				object_desc(o_name, o_ptr, 0);

				/* Message */
#ifdef JP
				msg_format("ザックには%sを入れる隙間がない。", o_name);
#else
				msg_format("You have no room for %s.", o_name);
#endif
				/* Hack - remember that the item has given a message here. */
				o_ptr->marked |= OM_NOMSG;

				continue;
			}
			else if (autopick_list[old_idx].action & DO_QUERY_AUTOPICK)
			{
				char out_val[MAX_NLEN+20];
				char o_name[MAX_NLEN];

				if (o_ptr->marked & OM_NO_QUERY)
				{
					/* Already answered as 'No' */
					continue;
				}

				/* Describe the object */
				object_desc(o_name, o_ptr, 0);

#ifdef JP
				sprintf(out_val, "%sを拾いますか? ", o_name);
#else
				sprintf(out_val, "Pick up %s? ", o_name);
#endif

				if (!get_check(out_val))
				{
					/* Hack - remember that the item has given a message here. */
					o_ptr->marked |= (OM_NOMSG | OM_NO_QUERY);
					continue;
				}

			}
			py_pickup_aux(this_o_idx);

			continue;
		}
		
		if (idx >= 0 &&
			(autopick_list[idx].action & (DO_AUTOPICK | DO_QUERY_AUTOPICK)))
		{
			disturb(0,0);

			if (!inven_carry_okay(o_ptr))
			{
				char o_name[MAX_NLEN];

				/* Describe the object */
				object_desc(o_name, o_ptr, 0);

				/* Message */
#ifdef JP
				msg_format("ザックには%sを入れる隙間がない。", o_name);
#else
				msg_format("You have no room for %s.", o_name);
#endif
				/* Hack - remember that the item has given a message here. */
				o_ptr->marked |= OM_NOMSG;

				continue;
			}
			else if (autopick_list[idx].action & DO_QUERY_AUTOPICK)
			{
				char out_val[MAX_NLEN+20];
				char o_name[MAX_NLEN];

				if (o_ptr->marked & OM_NO_QUERY)
				{
					/* Already answered as 'No' */
					continue;
				}

				/* Describe the object */
				object_desc(o_name, o_ptr, 0);

#ifdef JP
				sprintf(out_val, "%sを拾いますか? ", o_name);
#else
				sprintf(out_val, "Pick up %s? ", o_name);
#endif

				if (!get_check(out_val))
				{
					/* Hack - remember that the item has given a message here. */
					o_ptr->marked |= (OM_NOMSG | OM_NO_QUERY);
					continue;
				}

			}
			py_pickup_aux(this_o_idx);

			continue;
		}
		
		/*
		 * Do auto-destroy;
		 * When always_pickup is 'yes', we disable
		 * auto-destroyer from autopick function, and do only
		 * easy-auto-destroyer.
		 */
		else
		{
			if (auto_destroy_item((-this_o_idx), idx))
				continue;
		}
	}
}


/*
 * Initialize auto-picker preference
 */
void init_autopicker(void)
{
	static const char easy_autopick_inscription[] = "(:=g";
	autopick_type entry;
	int i;

	/* Clear old entries */
	for( i = 0; i < max_autopick; i++)
		autopick_free_entry(&autopick_list[i]);

	max_autopick = 0;

	/* There is always one entry "=g" */
	autopick_new_entry(&entry, easy_autopick_inscription);
	autopick_list[max_autopick++] = entry;
}



/*
 *  Process line for auto picker/destroyer.
 */
errr process_pickpref_file_line(char *buf)
{
	autopick_type entry;
	int i;

	if (max_autopick == MAX_AUTOPICK)
		return 1;

	/* Nuke illegal char */
	for(i = 0; buf[i]; i++)
	{
#ifdef JP
		if (iskanji(buf[i]))
		{
			i++;
			continue;
		}
#endif
		if (isspace(buf[i]) && buf[i] != ' ')
			break;
	}
	buf[i] = 0;

	if (!autopick_new_entry(&entry, buf)) return 0;

	/* Already has the same entry? */ 
	for (i = 0; i < max_autopick; i++)
		if (!strcmp(entry.name, autopick_list[i].name)
		   && entry.flag[0] == autopick_list[i].flag[0]
		   && entry.flag[1] == autopick_list[i].flag[1]
		   && entry.dice == autopick_list[i].dice
		   && entry.bonus == autopick_list[i].bonus) return 0;

	autopick_list[max_autopick++] = entry;
	return 0;
}
