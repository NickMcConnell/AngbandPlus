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
#define FLG_COLLECTING       1
#define FLG_UNIDENTIFIED     2
#define FLG_IDENTIFIED       3
#define FLG_STAR_IDENTIFIED  4
#define FLG_BOOSTED          5
#define FLG_MORE_THAN        6
#define FLG_DICE             7
#define FLG_MORE_BONUS       8
#define FLG_MORE_BONUS2      9
#define FLG_WORTHLESS       10
#define FLG_ARTIFACT        11
#define FLG_EGO             12
#define FLG_NAMELESS        13
#define FLG_UNAWARE         14
#define FLG_WANTED          15
#define FLG_UNIQUE          16
#define FLG_METAL           17
#define FLG_UNREADABLE      18
#define FLG_FIRST           19
#define FLG_SECOND          20
#define FLG_THIRD           21

#define FLG_ITEMS           22
#define FLG_WEAPONS         23
#define FLG_ARMORS          24
#define FLG_MISSILES        25
#define FLG_SHOTS           26
#define FLG_DEVICES         27
#define FLG_LIGHTS          28
#define FLG_JUNKS           29
#define FLG_SPELLBOOKS      30
#define FLG_HAFTED          31

#define FLG_SMALL_SWORDS    32
#define FLG_KATANAS         33
#define FLG_SWORDS          34
#define FLG_GREAT_SWORDS    35
#define FLG_AXES            36
#define FLG_SPEARS          37
#define FLG_LANCES          38
#define FLG_CLAWS           39
#define FLG_SCYTHES         40
#define FLG_WHIPS           41
#define FLG_HAMMERS         42
#define FLG_STAFFS          43
#define FLG_FANS            44
#define FLG_BOWS            45
#define FLG_GUNS            46
#define FLG_DIGGERS         47

#define FLG_SHIELDS         48
#define FLG_LAUNCHERS       49
#define FLG_RINGS           50
#define FLG_AMULETS         51
#define FLG_SUITS           52
#define FLG_CLOAKS          53
#define FLG_HELMS           54
#define FLG_GLOVES          55
#define FLG_BOOTS           56

#ifdef JP

#define KEY_ALL "すべての"

#ifdef MAC_MPW
/*
 * MEGA HACK -- MPWのバグ除け。
 * pre-process中に「収」の字の2バイト目が勝手に消えてしまう。
 */
#define KEY_COLLECTING "\x8e\xfb集中の"
#else
#define KEY_COLLECTING "収集中の"
#endif

#define KEY_UNIDENTIFIED "未鑑定の"
#define KEY_IDENTIFIED "鑑定済みの"
#define KEY_STAR_IDENTIFIED "*鑑定*済みの"
#define KEY_BOOSTED "ダイス目の違う"
#define KEY_MORE_THAN  "ダイス目"
#define KEY_DICE  "以上の"
#define KEY_MORE_BONUS  "修正値"
#define KEY_MORE_BONUS2  "以上の"
#define KEY_WORTHLESS "無価値の"
#define KEY_ARTIFACT "アーティファクト"
#define KEY_EGO "エゴ"
#define KEY_NAMELESS "無銘の"
#define KEY_UNAWARE "未判明の"
#define KEY_WANTED "賞金首の"
#define KEY_UNIQUE "ユニーク・モンスターの"
#define KEY_METAL "金属製の"
#define KEY_UNREADABLE "読めない"
#define KEY_FIRST "1冊目の"
#define KEY_SECOND "2冊目の"
#define KEY_THIRD "3冊目の"
#define KEY_ITEMS "アイテム"
#define KEY_WEAPONS "武器"
#define KEY_ARMORS "防具"
#define KEY_MISSILES "矢"
#define KEY_SHOTS "弾丸"
#define KEY_DEVICES "魔法アイテム"
#define KEY_LIGHTS "光源"
#define KEY_JUNKS "がらくた"
#define KEY_SPELLBOOKS "魔法書"
#define KEY_HAFTED "鈍器"

#define KEY_SMALL_SWORDS "小剣"
#define KEY_KATANAS      "カタナ"
#define KEY_SWORDS       "剣"
#define KEY_GREAT_SWORDS "大剣"
#define KEY_AXES         "斧"
#define KEY_SPEARS       "槍"
#define KEY_LANCES       "乗馬槍"
#define KEY_CLAWS        "爪"
#define KEY_SCYTHES      "鎌"
#define KEY_WHIPS        "鞭"
#define KEY_HAMMERS      "ハンマー"
#define KEY_STAFFS       "杖"
#define KEY_FANS         "扇"
#define KEY_NUNCHAKUS    "ヌンチャク"
#define KEY_BOWS         "弓"
#define KEY_GUNS         "銃"
#define KEY_DIGGERS      "掘削道具"

#define KEY_SHIELDS "盾"
#define KEY_LAUNCHERS "射撃武器"
#define KEY_RINGS "指輪"
#define KEY_AMULETS "アミュレット"
#define KEY_SUITS "鎧"
#define KEY_CLOAKS "クローク"
#define KEY_HELMS "兜"
#define KEY_GLOVES "籠手"
#define KEY_BOOTS "靴"

#else 

#define KEY_ALL "all"
#define KEY_COLLECTING "collecting"
#define KEY_UNIDENTIFIED "unidentified"
#define KEY_IDENTIFIED "identified"
#define KEY_STAR_IDENTIFIED "*identified*"
#define KEY_BOOSTED "dice boosted"
#define KEY_MORE_THAN  "more than"
#define KEY_DICE  " dice"
#define KEY_MORE_BONUS  "more bonus than"
#define KEY_MORE_BONUS2  ""
#define KEY_WORTHLESS "worthless"
#define KEY_ARTIFACT "artifact"
#define KEY_EGO "ego"
#define KEY_NAMELESS "nameless"
#define KEY_UNAWARE "unaware"
#define KEY_WANTED "wanted"
#define KEY_UNIQUE "unique monster's"
#define KEY_METAL "metal"
#define KEY_UNREADABLE "unreadable"
#define KEY_FIRST "first"
#define KEY_SECOND "second"
#define KEY_THIRD "third"
#define KEY_ITEMS "items"
#define KEY_WEAPONS "weapons"
#define KEY_ARMORS "armors"
#define KEY_MISSILES "missiles"
#define KEY_SHOTS "shots"
#define KEY_DEVICES "magical devices"
#define KEY_LIGHTS "lights"
#define KEY_JUNKS "junks"
#define KEY_SPELLBOOKS "spellbooks"
#define KEY_HAFTED "hafted weapons"

#define KEY_SMALL_SWORDS "small swords"
#define KEY_KATANAS      "katanas"
#define KEY_SWORDS       "swords"
#define KEY_GREAT_SWORDS "great swords"
#define KEY_AXES         "axes"
#define KEY_SPEARS       "spears"
#define KEY_LANCES       "lances"
#define KEY_CLAWS        "claws"
#define KEY_SCYTHES      "scythes"
#define KEY_WHIPS        "whips"
#define KEY_HAMMERS      "hammers"
#define KEY_STAFFS       "staffs"
#define KEY_FANS         "fans"
#define KEY_NUNCHAKUS    "nunchakus"
#define KEY_BOWS         "bows"
#define KEY_GUNS         "guns"
#define KEY_DIGGERS      "diggers"

#define KEY_SHIELDS "shields"
#define KEY_LAUNCHERS "launchers"
#define KEY_RINGS "rings"
#define KEY_AMULETS "amulets"
#define KEY_SUITS "suits"
#define KEY_CLOAKS "cloaks"
#define KEY_HELMS "helms"
#define KEY_GLOVES "gloves"
#define KEY_BOOTS "boots"

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

/* Hack -- dividing by 32 is into 5-bit shift */
/* Hack -- MOD by 32 is into 5-bit mask */
#define ADD_FLG(FLG) (entry->flag[((FLG) >> 5)] |= (1L << ((FLG) & 0x1f)))
#define REM_FLG(FLG) (entry->flag[((FLG) >> 5)] &= ~(1L << ((FLG) & 0x1f)))
#define ADD_FLG_NOUN(FLG) (ADD_FLG(FLG), prev_flg = (FLG))
#define IS_FLG(FLG) (entry->flag[((FLG) >> 5)] & (1L << ((FLG) & 0x1f)))

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

	if (IS_FLG(FLG_UNREADABLE)) ADD_KEY(KEY_UNREADABLE);
	if (IS_FLG(FLG_FIRST)) ADD_KEY(KEY_FIRST);
	if (IS_FLG(FLG_SECOND)) ADD_KEY(KEY_SECOND);
	if (IS_FLG(FLG_THIRD)) ADD_KEY(KEY_THIRD);
	if (IS_FLG(FLG_WANTED)) ADD_KEY(KEY_WANTED);
	if (IS_FLG(FLG_UNIQUE)) ADD_KEY(KEY_UNIQUE);
	if (IS_FLG(FLG_METAL)) ADD_KEY(KEY_METAL);
	if (IS_FLG(FLG_WORTHLESS)) ADD_KEY(KEY_WORTHLESS);
	if (IS_FLG(FLG_NAMELESS)) ADD_KEY(KEY_NAMELESS);
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
	else if (IS_FLG(FLG_SPELLBOOKS)) ADD_KEY2(KEY_SPELLBOOKS);
	else if (IS_FLG(FLG_HAFTED)) ADD_KEY2(KEY_HAFTED);
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
	else if (IS_FLG(FLG_DIGGERS)) ADD_KEY2(KEY_DIGGERS);
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
		if (MATCH_KEY(KEY_NAMELESS)) ADD_FLG(FLG_NAMELESS);
		if (MATCH_KEY(KEY_UNAWARE)) ADD_FLG(FLG_UNAWARE);
		if (MATCH_KEY(KEY_WANTED)) ADD_FLG(FLG_WANTED);
		if (MATCH_KEY(KEY_UNIQUE)) ADD_FLG(FLG_UNIQUE);
		if (MATCH_KEY(KEY_METAL)) ADD_FLG(FLG_METAL);
		if (MATCH_KEY(KEY_UNREADABLE)) ADD_FLG(FLG_UNREADABLE);
		if (MATCH_KEY(KEY_FIRST)) ADD_FLG(FLG_FIRST);
		if (MATCH_KEY(KEY_SECOND)) ADD_FLG(FLG_SECOND);
		if (MATCH_KEY(KEY_THIRD)) ADD_FLG(FLG_THIRD);
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
	else if (MATCH_KEY2(KEY_SPELLBOOKS)) ADD_FLG_NOUN(FLG_SPELLBOOKS);
	else if (MATCH_KEY2(KEY_HAFTED)) ADD_FLG_NOUN(FLG_HAFTED);
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
	else if (MATCH_KEY2(KEY_DIGGERS)) ADD_FLG_NOUN(FLG_DIGGERS);
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
			/* A noun type keyword didn't end correctly */
			/* Hack -- dividing by 32 is into 5-bit shift */
			/* Hack -- MOD by 32 is into 5-bit mask */
			entry->flag[(prev_flg >> 5)] &= ~(1L << (prev_flg & 0x1f));
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
	    && (object_known_p(o_ptr) || (o_ptr->ident & IDENT_SENSE)))
		return FALSE;

	/*** Identified ***/
	if (IS_FLG(FLG_IDENTIFIED) && !object_known_p(o_ptr))
		return FALSE;

	/*** *Identified* ***/
	if (IS_FLG(FLG_STAR_IDENTIFIED) &&
	    (!object_known_p(o_ptr) || !(o_ptr->ident & IDENT_MENTAL)))
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
			if (object_known_p(o_ptr) && ((o_ptr->dd != k_ptr->dd) || (o_ptr->ds != k_ptr->ds)))
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
		if (object_known_p(o_ptr))
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

		if (!object_known_p(o_ptr)) return FALSE;

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
		if (!object_known_p(o_ptr) || (!o_ptr->name1 && !o_ptr->art_name))
			return FALSE;
	}

	/*** Ego object ***/
	if (IS_FLG(FLG_EGO))
	{
		if (!object_known_p(o_ptr) || !o_ptr->name2)
			return FALSE;
	}

	/*** Nameless ***/
	if (IS_FLG(FLG_NAMELESS))
	{
		switch (o_ptr->tval)
		{
		case TV_STONE: case TV_TAROT: case TV_SCRATCH_CARD:
		case TV_BULLET: case TV_ROUND: case TV_SHELL: case TV_ROCKET:
		case TV_ARROW: case TV_BOLT: case TV_BOW:
		case TV_DIGGING: case TV_HAFTED: case TV_POLEARM: case TV_SWORD: 
		case TV_BOOTS: case TV_GLOVES: case TV_HELM: case TV_CROWN:
		case TV_SHIELD: case TV_CLOAK:
		case TV_SOFT_ARMOR: case TV_HARD_ARMOR:
		case TV_LITE: case TV_AMULET: case TV_RING: case TV_CARD:
			if ((!object_known_p(o_ptr) || o_ptr->inscription
			     || o_ptr->name1 || o_ptr->name2 || o_ptr->art_name))
				return FALSE;
			break;
		default:
			/* don't match */
			return FALSE;
		}
	}

	/*** Unaware items ***/
	if (IS_FLG(FLG_UNAWARE) && object_aware_p(o_ptr))
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

	/*** Metal items ***/
	if (IS_FLG(FLG_METAL) && !item_tester_hook_metal(o_ptr))
		return FALSE;

	/*** Unreadable spellbooks ***/
	if (IS_FLG(FLG_UNREADABLE) &&
	    (o_ptr->tval < TV_MAGERY_BOOK ||
	     check_book_realm(o_ptr->tval, o_ptr->sval)))
		return FALSE;

	/*** First rank spellbooks ***/
	if (IS_FLG(FLG_FIRST) &&
	    (o_ptr->tval < TV_MAGERY_BOOK || 0 != o_ptr->sval))
		return FALSE;

	/*** Second rank spellbooks ***/
	if (IS_FLG(FLG_SECOND) &&
	    (o_ptr->tval < TV_MAGERY_BOOK || 1 != o_ptr->sval))
		return FALSE;

	/*** Third rank spellbooks ***/
	if (IS_FLG(FLG_THIRD) && 
	    (o_ptr->tval < TV_MAGERY_BOOK || 2 != o_ptr->sval))
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
	else if (IS_FLG(FLG_SPELLBOOKS))
	{
		if (!(o_ptr->tval >= TV_MAGERY_BOOK))
			return FALSE;
	}
	else if (IS_FLG(FLG_HAFTED))
	{
		if (!(o_ptr->tval == TV_HAFTED))
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
#ifdef JP
		if (!strstr_j(o_name, ptr)) return FALSE;
#else
		if (!strstr(o_name, ptr)) return FALSE;
#endif
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

	object_desc(o_name, o_ptr, FALSE, 3);

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
		if (p_ptr->pclass == CLASS_ARCHER)
		{
			if (o_ptr->tval == TV_SKELETON ||
			    (o_ptr->tval == TV_CORPSE && o_ptr->sval == SV_SKELETON))
				return FALSE;
		}

		else if (p_ptr->pclass == CLASS_GUNNER)
		{
			if (item_tester_hook_metal(o_ptr))
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
		object_desc(o_name, o_ptr, TRUE, 3);

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
		object_desc(o_name, o_ptr, TRUE, 3);

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
		int idx;

		/* Acquire object */
		object_type *o_ptr = &o_list[this_o_idx];

		/* Acquire next object */
		next_o_idx = o_ptr->next_o_idx;

		idx = is_autopick(o_ptr);

		/* Item index for floor -1,-2,-3,...  */
		auto_inscribe_item((-this_o_idx), idx);

		/* Sense the object */
		sense_floor_object(this_o_idx);

		if (idx >= 0 &&
			(autopick_list[idx].action & (DO_AUTOPICK | DO_QUERY_AUTOPICK)))
		{
			disturb(0,0);

			if (!inven_carry_okay(o_ptr))
			{
				char o_name[MAX_NLEN];

				/* Describe the object */
				object_desc(o_name, o_ptr, TRUE, 3);

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
				object_desc(o_name, o_ptr, TRUE, 3);

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
