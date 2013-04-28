/* PosBand -- A variant of Angband roguelike
 *
 * Copyright (c) 2004 Ben Harrison, Robert Ruehlmann and others
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 * 
 * NPPAngband Copyright (c) 2003-2004 Jeff Greene
 * PosBand Copyright (c) 2004-2005 Alexander Ulyanov
 */

/* quest.h: quest-related definitions */

#ifndef QUEST_H_INCLUDED
#define QUEST_H_INCLUDED

/* Used as values for the "chest_or_quest" variable */

#define OPEN_CHEST			1
#define QUEST_ITEM			2

#define CHEST_LEVEL			130
#define QUEST_LEVEL			131

/*
 * Quest types
 */
enum
{
        QUEST_FIXED = 1,
        QUEST_FIXED_U,
        QUEST_GUILD,
        QUEST_UNIQUE,
        QUEST_VAULT
};

/*
 * Quest reward types
 */
enum
{
        REWARD_GOLD = 1,
        REWARD_GOOD_ITEM,
        REWARD_GREAT_ITEM,
        REWARD_TAILORED
};

/*
 * Quest item name parts
 */
#define QUEST_NAME_1	11
#define QUEST_NAME_2	14
#define QUEST_NAME_3	11

/*
 * Structure for the "quests"
 */
struct quest_type
{
	u32b name;		/* Name (offset) */

	byte type;		/* Quest Type */
	byte reward;		/* Quest Reward */

	byte active_level;	/* Equals dungeon level if not completed, 0 if completed */
	byte base_level;	/* The dungeon level on which the quest was assigned*/

	s16b mon_idx;		/* Monster race/unique */

	s16b cur_num;		/* Number killed */
	s16b max_num;		/* Number required */

	bool started;		/* Has the player start the quest */
};

extern bool chest_or_quest;
extern quest_type *q_info;
extern char *q_name;

/* quest.c */
void plural_aux(char *name, size_t max);
cptr describe_quest(s16b level, int mode);
void display_guild(void);
void guild_purchase(void);
byte quest_check(int lev);
int quest_num(int lev);
int quest_item_slot(void);
void quest_fail(void);

#endif /* QUEST_H_INCLUDED */
