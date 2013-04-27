/* File: quest1.c */

/* Purpose: Quest creation */

/*
 * Copyright (c) 1989, 2003 James E. Wilson, Robert A. Koeneke,
 *                          Robert Ruehlmann, Steven Fuerst
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

/*
 * Changes copyright (c) 2008 Mangojuice, licenced under the GPL.
 */

#include "angband.h"

#include "wild.h"

/*
 * Maximum number of tries for selection of a proper quest monster
 */
#define MAX_TRIES 20

const store_type *curr_build;
static int curr_scale;
static int curr_place_num;

static int thresh = 30;

/*
 * Selects a random location near the current one,
 * and creates a quest stair place there.
 */
static void pick_quest_location(int *best_x, int *best_y, int *p_num)
{
	int score, i;
	int best_score = 500;
	int x, y, bx, by, pn;
	int j;
	wild_type *w_ptr;

	/* Avoid compiler warnings */
	bx = by = 0;

	for (i = 0; i < thresh; i++)
	{
		x = 4+rand_range(place[curr_place_num].x - thresh, place[curr_place_num].x + thresh);
		y = 4+rand_range(place[curr_place_num].y - thresh, place[curr_place_num].y + thresh);

		/* Bounds enforcement */
		if (x < 0) x = 0;
		if (y < 0) y = 0;
		if (x >= max_wild-1) x = max_wild - 2;
		if (y >= max_wild-1) y = max_wild - 2;

		score = distance(x, y, place[curr_place_num].x+4, place[curr_place_num].y+4);

		if (score < 5)
		{
			/* Too close */
			continue;
		}

		/* Make sure this place is not already a fixed place, and not in water or lava or acid, or on the road */
		w_ptr = &wild[y][x];
		if (w_ptr->trans.place) score = 500;
		if (w_ptr->trans.info & (WILD_INFO_WATER | WILD_INFO_LAVA | WILD_INFO_ACID | WILD_INFO_ROAD))
			score = 500;

		/* Not too close to another quest dungeon */
		for (j = 0; j < place_count; j++)
		{
			if (place[j].type != PL_QUEST_STAIR) continue;
			if (distance(x, y, place[j].x, place[j].y) < 3)
				score = 500;
		}

		if (score < best_score)
		{
			best_score = score;
			*best_x = x;
			*best_y = y;
			bx = x;
			by = y;
		}
	}

	if (best_score == 500) /* Try to not crash, just recurse... but warn */
	{
		/* Debug */
		get_check ("Couldn't find suitable place in pick_quest_location, recursing.");
		thresh++;
		pick_quest_location(best_x, best_y, p_num);
		return;
	}

	/* Create the place */
	pn = place_pop();

	/* Paranoia */
	if (pn == -1)
	{
		get_check ("Ready to quit?");
		quit ("Couldn't pick a new place in quest generation");
	}

	*p_num = pn;

	/* Save x and y */
	place[pn].x = bx;
	place[pn].y = by;

	/* Do other common setup stuff */
	place[pn].store = NULL;
	MAKE(place[pn].dungeon, dun_type);
	place[pn].type = PL_QUEST_STAIR;
	place[pn].numstores = 0;
	place[pn].xsize = 1;
	place[pn].ysize = 1;
	place[pn].data = 0;
	place[pn].monst_type = TOWN_MONST_MONST;

	/* Default name */
	strcpy (place[pn].name, "Quest");
}

/*
 * Selects a single bit from a flag set
 * that contains possibly multiple bits
 */
static u32b select_bit(u32b flags)
{
	int i;
	int bits=0;
	int n;

	if (flags == 0) return 0;

	for (i = 0; i < 32; i++)
	{
		if (flags & (1 << i)) bits++;
	}

	n = randint1(bits);

	for (i = 0; i < 32; i++)
	{
		if (flags & (1 << i))
		{
			bits--;
			if (!bits) return (1 << i);
		}
	}

	/* Should never get here */
	return (0);
}

/*
 * Wipe a quest
 */
static void quest_wipe(int i)
{
	quest_type *q_ptr = &quest[i];

	q_ptr->status = QUEST_STATUS_UNTAKEN;
	q_ptr->flags = 0x00;
	q_ptr->type = QUEST_TYPE_NONE;

	/* No artificial quest item */
	q_ptr->item = 0;

	/* No quest-giver */
	q_ptr->place = 0;
	q_ptr->shop = 0;

	/* No reward */
	q_ptr->reward = 0;

	/* Types of creation and trigger hooks */
	q_ptr->c_type = QC_NONE;
	q_ptr->x_type = QX_NONE;

	/* Timeout */
	q_ptr->timeout = 0;

	/* No name */
	q_ptr->name[0] = '\0';

	/*
	 * Do not need to clear the extra data
	 * - it is ignored since q_ptr->type = QUEST_TYPE_NONE
	 */
}

/* Current location in scan for completed quests */
static s16b q_cnt = 0;

/*
 * Acquires and returns the index of a "free" quest.
 *
 * This routine should almost never fail, but in case it does,
 * we must be sure to handle "failure" of this routine.
 */
u16b q_pop(void)
{
	int i;

	/* Initial allocation */
	if (q_max < z_info->q_max)
	{
		/* Get next space */
		i = q_max;

		/* Count quests */
		q_max++;

		/* Use this quest */
		return (i);
	}

	/* Recycle finished quests */
	for (i = 1; i < q_max; i++)
	{
		quest_type *q_ptr;

		/* Make sure we have a linear algorithm */
		q_cnt++;

		/* loop back to start */
		if (q_cnt >= q_max) q_cnt = 0;

		/* Acquire quest */
		q_ptr = &quest[q_cnt];

		/* Skip live quests */
		if (q_ptr->status != QUEST_STATUS_FINISHED) continue;

		/* Skip find_place quests as these can be completed as a group */
		if (q_ptr->type == QUEST_TYPE_FIND_PLACE) continue;

		/* Clear the old data */
		quest_wipe(q_cnt);

		/* Use this field */
		return (q_cnt);
	}

	/* Warn the player */
	msgf("Too many quests!");

	/* Oops */
	return (0);
}


/*
 * Make a quest for killing n monsters of a certain type on a certain level
 */
static u16b insert_dungeon_monster_quest(u16b r_idx, u16b num, u16b level)
{
	int q_num;
	quest_type *q_ptr;
	monster_race *r_ptr = &r_info[r_idx];

	/* get a new quest */
	q_num = q_pop();

	/* Paranoia */
	if (!q_num) return (-1);

	q_ptr = &quest[q_num];

	/* Store in information */
	q_ptr->type = QUEST_TYPE_DUNGEON;

	/* We need to place the monster(s) when the dungeon is made */
	q_ptr->c_type = QC_DUN_MONST;

	/* We need to trigger when the monsters are killed */
	if (FLAG(r_ptr, RF_UNIQUE))
	{
		q_ptr->x_type = QX_KILL_UNIQUE;
	}
	else
	{
		q_ptr->x_type = QX_KILL_MONST;
	}

	if (num != 1)
	{
		char buf[80];
		strcpy(buf, mon_race_name(r_ptr));
		plural_aux(buf);

		/* XXX XXX Create quest name */
		(void)strnfmt(q_ptr->name, 128, "Kill the %d %s that are guarding dungeon level %d.", (int)num, buf, level);
	}
	else if (FLAG(r_ptr, RF_UNIQUE))
	{
		/* XXX XXX Create quest name */
		(void)strnfmt(q_ptr->name, 128, "Kill %s, who is guarding dungeon level %d.", mon_race_name(r_ptr), level);
	}
	else
	{
		/* XXX XXX Create quest name */
		(void)strnfmt(q_ptr->name, 128, "Kill one %s, who is guarding dungeon level %d.", mon_race_name(r_ptr), level);
	}

	/* Save the quest data */
	q_ptr->data.dun.r_idx = r_idx;
	q_ptr->data.dun.level = level;
	q_ptr->data.dun.cur_num = 0;
	q_ptr->data.dun.max_num = num;
	q_ptr->data.dun.num_mon = 0;
	q_ptr->level = level;
	q_ptr->reward = r_ptr->level * r_ptr->level * 5 * num;

	/* Return number of quest */
	return (q_num);
}


/*
 * Create the quests for the Serpent and Oberon
 */
static void insert_winner_quest(u16b r_idx, u16b num, u16b level)
{
	/* Normal monster quest */
	u16b q_idx = insert_dungeon_monster_quest(r_idx, num, level);

	/* Winner result of quest */
	quest[q_idx].x_type = QX_KILL_WINNER;

	quest[q_idx].status = QUEST_STATUS_UNTAKEN;

	/* Exception: these need to be given in certain ironman games, or you can't win. */
	if (vanilla_town || ironman_downward)
	{
	  quest[q_idx].flags |= QUEST_FLAG_KNOWN;
	  quest[q_idx].status = QUEST_STATUS_TAKEN;
	}
}

static u16b insert_boss_quest(u16b r_idx, int x, int y, int p_num)
{
	int q_num, offset, hero_idx;
	quest_type *q_ptr;
	monster_race *r_ptr = &r_info[r_idx];
	cptr town_name, town_dir;
	char buf[80];
	char pron[4];
	char pron2[4];
	char buf2[80];
	char buf3[80];
	char buf4[80];
	char buf5[80];
	dun_type *d_ptr;
	u32b d_type;

	/* get a new quest */
	q_num = q_pop();

	/* Paranoia */
	if (!q_num) return (-1);

	q_ptr = &quest[q_num];

	q_ptr->type = QUEST_TYPE_FIXED_BOSS;

	/* Completion when the level is wiped out */
	q_ptr->x_type = QX_CLEAR_LEVEL;

	q_ptr->status = QUEST_STATUS_UNTAKEN;
	q_ptr->level = r_ptr->level;

	/* Adjust for quests that have a hero in them */
	offset = damroll(4,2)-3;
	hero_idx = create_hero(r_idx, offset, TRUE);

	if (hero_idx)
	{
		q_ptr->level += offset;
		r_idx = hero_idx;
	}

	q_ptr->reward = r_ptr->level * r_ptr->level * 30;

	/* Mark boss as questor if unique */
	if (FLAG(r_ptr, RF_UNIQUE) || FLAG(r_ptr, RF_UNIQUE_7))
		SET_FLAG(r_ptr, RF_QUESTOR);

	/* If the target is unique, quest ends when it dies... must
	   also make sure not to have the target killed prematurely. */
	if (FLAG(r_ptr, RF_UNIQUE) || hero_okay(r_idx))
	{
		q_ptr->x_type = QX_KILL_UNIQUE;
	}
	else if (FLAG(r_ptr, RF_UNIQUE_7))
	{
		q_ptr->x_type = QX_KILL_MONST;
	}

	/* Get name of closest town + direction away from it */
	town_name = describe_quest_location(&town_dir, x, y, FALSE);

	if (FLAG(r_ptr, RF_UNIQUE))
		strcpy(buf, mon_race_name(r_ptr));
	else
	{
		strnfmt(buf, 80, "A %s", mon_race_name(r_ptr));
		if (is_a_vowel(buf[2]))
			strnfmt(buf, 80, "An %s", mon_race_name(r_ptr));
	}

	if (FLAG(r_ptr, RF_MALE))
	{
		strcpy(pron, "his");
		strcpy(pron2, "him");
	}
	else if (FLAG(r_ptr, RF_FEMALE))
	{
		strcpy(pron, "her");
		strcpy(pron2, "her");
	}
	else
	{
		strcpy(pron, "its");
		strcpy(pron2, "it");
	}

	get_rnd_line("quest1.txt", 0, buf2);
	get_rnd_line("quest2.txt", 0, buf3);
	get_rnd_line("quest3.txt", 0, buf4);
	get_rnd_line("quest4.txt", 0, buf5);

	/* Create quest name */
	if (FLAG(r_ptr, RF_UNIQUE) || FLAG(r_ptr, RF_UNIQUE_7))
		(void)strnfmt(q_ptr->name, 256, "%s and %s %s have been %s the area %s %s.  Find %s %s and %s %s.",
				  buf, pron, buf4, buf2, town_dir, town_name, pron, buf3, buf5, pron2);
	else
		(void)strnfmt(q_ptr->name, 256, "%s and %s %s have been %s the area %s %s.  Find their %s and %s them.",
				  buf, pron, buf4, buf2, town_dir, town_name, buf3, buf5);

	d_type = DUN_TYPE_PURE_CASTLE;
	if (strchr("fqrBCK", r_ptr->d_char))
		d_type = DUN_TYPE_FOREST;
	else if (strchr("abcwFIJRS", r_ptr->d_char))
		d_type = DUN_TYPE_DESERT;
	else if (strchr("nszGHMQ", r_ptr->d_char))
		d_type = DUN_TYPE_SWAMP;

	q_ptr->data.fix.d_type = d_type;
	q_ptr->data.fix.d_flags = DF_MEDIUM;
	q_ptr->data.fix.seed = randint0(0x10000000);
	q_ptr->data.fix.x = x;
	q_ptr->data.fix.y = y;
	q_ptr->data.fix.min_level = r_ptr->level - 5;
	q_ptr->data.fix.data.boss.r_idx = r_idx;

	/* Farming can be an issue here.  But since this will often be uniques, don't make it too restrictive.  */
	q_ptr->data.fix.attempts = 6;

	/* Set the quest number */
	place[p_num].quest_num = q_num;

	/* Set up the dungeon */
	d_ptr = place[p_num].dungeon;

	/* Paranoia */
	if (d_ptr == NULL)
	{
		quit ("Couldn't allocate dungeon.");
	}

	pick_dungeon(d_ptr, d_type);

	/* One level always */
	d_ptr->min_level = d_ptr->max_level = r_ptr->level;

	/* Always medium size */
	d_ptr->flags &= ~(DF_BIG | DF_SMALL);
	d_ptr->flags |= q_ptr->data.fix.d_flags;

	return(q_num);
}

static u16b insert_clearout_quest(u16b d_idx, int level, int x, int y, int p_num)
{
	int q_num;
	quest_type *q_ptr;
	dun_gen_type *dg_ptr = &dungeons[d_idx];
	dun_type *d_ptr;
	int n;
	cptr town_name, town_dir;
	char buf[80];

	/* get a new quest */
	q_num = q_pop();

	/* Paranoia */
	if (!q_num) return (-1);

	q_ptr = &quest[q_num];

	q_ptr->type = QUEST_TYPE_FIXED_CLEAROUT;

	/* Completion when the dungeon is cleared */
	q_ptr->x_type = QX_CLEAR_LEVEL;

	q_ptr->status = QUEST_STATUS_UNTAKEN;
	q_ptr->level = level + 2;

	n = 1+rand_range(level/25, level/18);

	/* Enforce bounds */
	if (n + level > 100) n = (100 - level);

	q_ptr->reward = level * level * 7 * n;

	/* Get name of closest town + direction away from it */
	town_name = describe_quest_location(&town_dir, x, y, FALSE);

	/* Describe the type of dungeon */
	strcpy(buf, dungeon_type_name(dg_ptr->habitat));
	buf[0] = tolower(buf[0]);

	(void)strnfmt(q_ptr->name, 256, "Clear %i %s of a certain %s %s %s.",
				  n, (n > 1 ? "levels" : "level"), buf, town_dir, town_name);

	q_ptr->data.fix.d_type = (1 << (d_idx - 1));
	q_ptr->data.fix.d_flags = 0;
	q_ptr->data.fix.seed = randint0(0x10000000);
	q_ptr->data.fix.x = x;
	q_ptr->data.fix.y = y;
	q_ptr->data.fix.min_level = level;
	q_ptr->data.fix.data.clearout.cleared = 0;
	q_ptr->data.fix.data.clearout.levels = n;

	/* This should not be a quest you can bail on. */
	q_ptr->data.fix.attempts = 10000;

	/* Set the quest number */
	place[p_num].quest_num = q_num;

	/* Set up the dungeon */
	d_ptr = place[p_num].dungeon;

	/* Paranoia */
	if (d_ptr == NULL)
	{
		quit ("Couldn't allocate dungeon.");
	}

	/* Use dungeon type d_idx */
	pick_dungeon(d_ptr, 1 << d_idx);

	/* Set up level bounds */
	d_ptr->min_level = level;
	d_ptr->max_level = level + n - 1;

	return(q_num);
}

static u16b insert_den_quest(u16b mg_idx, int level, int x, int y, int p_num)
{
	int q_num, n;
	quest_type *q_ptr;
	monster_group_type *mg_ptr = &mg_info[mg_idx];
	cptr town_name, town_dir;
	char buf[80];
	char buf2[80];
	char buf3[80];
	char buf4[80];
	char buf5[80];
	dun_type * d_ptr;

	/* get a new quest */
	q_num = q_pop();

	/* Paranoia */
	if (!q_num) return (-1);

	q_ptr = &quest[q_num];

	q_ptr->type = QUEST_TYPE_FIXED_DEN;

	/* Completion when all levels are cleared */
	q_ptr->x_type = QX_CLEAR_LEVEL;

	q_ptr->status = QUEST_STATUS_UNTAKEN;
	q_ptr->level = level;

	n = rand_range(mg_ptr->min_levels, mg_ptr->max_levels);

	/* Paranoia */
	n = MAX(n, 1);

	q_ptr->reward = level * level * 9 * n;

	/* Get name of closest town + direction away from it */
	town_name = describe_quest_location(&town_dir, x, y, FALSE);

	get_rnd_line("quest1.txt", 0, buf2);
	get_rnd_line("quest2.txt", 0, buf3);
	get_rnd_line("quest4.txt", 0, buf5);
	get_rnd_line("quest5.txt", 0, buf);

	if (n > 1) strnfmt(buf4, 80, "  (%i levels)", n);
	else buf4[0] = 0;

	/* Create quest name */
	(void)strnfmt(q_ptr->name, 256, "%s %s have been %s the area %s %s.  Discover their %s and %s them all.%s",
					buf, mg_ptr->name, buf2, town_dir, town_name, buf3, buf5, buf4);

	q_ptr->data.fix.d_type = select_bit(mg_ptr->d_type);
	q_ptr->data.fix.d_flags = mg_ptr->d_flags;
	q_ptr->data.fix.seed = randint0(0x10000000);
	q_ptr->data.fix.x = x;
	q_ptr->data.fix.y = y;
	q_ptr->data.fix.min_level = level;
	q_ptr->data.fix.data.den.mg_idx = mg_idx;
	q_ptr->data.fix.data.den.cleared = 0;
	q_ptr->data.fix.data.den.levels = n;

	/* Eminently farmable.  Few attempts. */
	q_ptr->data.fix.attempts = 4;

	/* Set the quest number */
	place[p_num].quest_num = q_num;

	/* Set up the dungeon */
	d_ptr = place[p_num].dungeon;

	/* Paranoia */
	if (d_ptr == NULL)
	{
		quit ("Couldn't allocate dungeon.");
	}

	/* Pick a dungeon matching the group */
	pick_dungeon(d_ptr, mg_ptr->d_type);

	/* Levels */
	d_ptr->min_level = level;
	d_ptr->max_level = level+n-1;

	/* Apply flags from the group */
	d_ptr->flags |= mg_ptr->d_flags;

	return(q_num);
}

static u16b insert_kill_quest(u16b r_idx, int x, int y, int p_num)
{
	int q_num, offset;
	int hero_idx;
	quest_type *q_ptr;
	monster_race *r_ptr = &r_info[r_idx];
	cptr town_name, town_dir;
	char buf[80];
	char pron[4];
	char pron2[4];
	char buf2[80];
	char buf3[80];
	char buf5[80];
	dun_type * d_ptr;
	u32b d_type = 0;

	/* get a new quest */
	q_num = q_pop();

	/* Paranoia */
	if (!q_num) return (-1);

	q_ptr = &quest[q_num];

	q_ptr->type = QUEST_TYPE_FIXED_KILL;

	/* Completion when the monster is killed */
	q_ptr->x_type = QX_KILL_MONST;

	q_ptr->status = QUEST_STATUS_UNTAKEN;
	q_ptr->level = r_ptr->level - 5;

	/* Adjust for quests that have a hero in them */
	offset = damroll(4,2)-3;
	hero_idx = create_hero(r_idx, offset, TRUE);

	if (hero_idx)
	{
		q_ptr->level += 3 + offset;
		r_idx = hero_idx;
	}

	q_ptr->reward = r_ptr->mexp * 2;

	/* Get name of closest town + direction away from it */
	town_name = describe_quest_location(&town_dir, x, y, FALSE);

	/* Mark boss as questor if unique */
	if (FLAG(r_ptr, RF_UNIQUE) || FLAG(r_ptr, RF_UNIQUE_7))
		SET_FLAG(r_ptr, RF_QUESTOR);

	if (FLAG(r_ptr, RF_UNIQUE))
		strcpy(buf, mon_race_name(r_ptr));
	else
	{
		strnfmt(buf, 80, "A %s", mon_race_name(r_ptr));
		if (is_a_vowel(buf[2]))
			strnfmt(buf, 80, "An %s", mon_race_name(r_ptr));
	}

	if (FLAG(r_ptr, RF_MALE))
	{
		strcpy(pron, "his");
		strcpy(pron2, "him");
	}
	else if (FLAG(r_ptr, RF_FEMALE))
	{
		strcpy(pron, "her");
		strcpy(pron2, "her");
	}
	else
	{
		strcpy(pron, "its");
		strcpy(pron2, "it");
	}

	get_rnd_line("quest1.txt", 0, buf2);
	get_rnd_line("quest2.txt", 0, buf3);
	get_rnd_line("quest4.txt", 0, buf5);

	/* Create quest name */
	(void)strnfmt(q_ptr->name, 256, "%s has been %s the area %s %s.  Track %s to %s %s and %s %s.",
				  buf, buf2, town_dir, town_name, pron2, pron, buf3, buf5, pron2);

	d_type = DUN_TYPE_PURE_CASTLE;
	if (strchr("fqrBCK", r_ptr->d_char))
		d_type = DUN_TYPE_FOREST;
	else if (strchr("abcwFIJRS", r_ptr->d_char))
		d_type = DUN_TYPE_DESERT;
	else if (strchr("nszGHMQ", r_ptr->d_char))
		d_type = DUN_TYPE_SWAMP;

	q_ptr->data.fix.d_type = d_type;
	q_ptr->data.fix.d_flags = DF_SMALL;
	q_ptr->data.fix.seed = randint0(0x10000000);
	q_ptr->data.fix.x = x;
	q_ptr->data.fix.y = y;
	q_ptr->data.fix.min_level = r_ptr->level - 5;
	q_ptr->data.fix.data.kill.r_idx = r_idx;

	/* Rather easy to avoid waking the monster up & plundering, so prevent scumming. */
	q_ptr->data.fix.attempts = 3;

	/* Set the quest number */
	place[p_num].quest_num = q_num;

	/* Set up the dungeon */
	d_ptr = place[p_num].dungeon;

	/* Paranoia */
	if (d_ptr == NULL)
	{
		quit ("Couldn't allocate dungeon.");
	}

	/* Always use the "Pure Castle" dungeon for this quest type */
	pick_dungeon(d_ptr, d_type);

	/* One level always */
	d_ptr->min_level = d_ptr->max_level = r_ptr->level;

	/* Apply flags */
	d_ptr->flags |= q_ptr->data.fix.d_flags;

	return(q_num);
}

static bool ambig [NUM_DUNGEON];

/*
 * Returns TRUE if the place pl_ptr
 * would be given the same description as another place.
 */
static bool ambiguous (int place_num)
{
	static bool init = FALSE;
	int i, j;
	char * town_name;
	char town_dir[20];
	char buf1[80];
	char buf2[80];
	place_type * pl_ptr;
	place_type * pl_ptr2;

	if (init) return ambig[place_num];

	init = TRUE;

	for (i = 0; i < NUM_DUNGEON; i++)
	{
		ambig[i] = FALSE;
		pl_ptr = &place[NUM_TOWNS+i];

		for (j = 0; j < i; j++)
		{
			pl_ptr2 = &place[NUM_TOWNS+j];

			strnfmt(buf1, 80, "%s %s of %s", dungeon_type_name(pl_ptr->dungeon->habitat), town_dir,
				describe_quest_location(town_dir, pl_ptr->x, pl_ptr->y, FALSE));

			strnfmt(buf2, 80, "%s %s of %s", dungeon_type_name(pl_ptr2->dungeon->habitat), town_dir,
				describe_quest_location(town_dir, pl_ptr2->x, pl_ptr2->y, FALSE));

			if (streq(buf1, buf2))
			{
				ambig[i] = TRUE;
				ambig[j] = TRUE;
			}
		}
	}

}

/*
 * Look for an appropriate dungeon for a given level
 */
static u16b find_good_dungeon(int level, bool repeat)
{
	int i;

	int score, best_score = 0;

	int best_place = 0;

	place_type *pl_ptr;

	for (i = 0; i < place_count; i++)
	{
		pl_ptr = &place[i];

		/* Want dungeons */
		if (pl_ptr->type != PL_DUNGEON) continue;

		/* Reuse this dungeon? (relevant for quest_find_place) */
		if (!repeat && pl_ptr->quest_num) continue;

		/* Does the dungeon have an ambiguous description?  If so, skip it. */
		if (repeat && ambiguous(i)) continue;

		/* Get difference in levels */
		score = pl_ptr->dungeon->max_level - level;

		/* Do not allow dungeons with max_level higher than level. */
		if (score < 0) continue;

		/* The bigger the difference, the less likely a high score is */
		score = randint1(127 - score);

		if (score > best_score)
		{
			best_score = score;
			best_place = i;
		}
	}

	/* Best match to reward level */
	return (best_place);
}

/* Save the quest giver (current town + building) */
void set_quest_giver(quest_type *q_ptr)
{
	place_type *pl_ptr = &place[p_ptr->place_num];

	/* Remember quest giver for later */
	q_ptr->place = p_ptr->place_num;
	q_ptr->shop = GET_ARRAY_INDEX(pl_ptr->store, curr_build);

	/* We know of this quest */
	q_ptr->flags |= QUEST_FLAG_KNOWN;
}


static int insert_artifact_quest(u16b a_idx)
{
	artifact_type *a_ptr = &a_info[a_idx];

	place_type *pl_ptr;

	cptr town_name, town_dir;

	quest_type *q_ptr;

	int q_num;

	char buf[80];

	/* Skip "empty" artifacts */
	if (!a_ptr->name) return (-1);

	/* Cannot make an artifact twice */
	if (a_ptr->cur_num) return (-1);

	/* No quest items */
	if (FLAG(a_ptr, TR_QUESTITEM)) return (-1);

	/* get a new quest */
	q_num = q_pop();

	/* Paranoia */
	if (!q_num) return (-1);

	q_ptr = &quest[q_num];

	/* Store in information */
	q_ptr->type = QUEST_TYPE_FIND_ITEM;

	/* We have taken the quest */
	q_ptr->status = QUEST_STATUS_TAKEN;

	/* We need to place the artifact in the dungeon */
	q_ptr->c_type = QC_DUN_ARTIFACT;

	/* Finished when the player identifies it */
	q_ptr->x_type = QX_KNOW_ARTIFACT;

	/* Find an available dungeon to place it in */

	/* Save the quest data */
	q_ptr->data.fit.a_idx = a_idx;

	/* Find a place for this artifact. */
	q_ptr->data.fit.place = find_good_dungeon(a_ptr->level, TRUE);

	/* Where is it? */
	pl_ptr = &place[q_ptr->data.fit.place];

	/* Get name of closest town + direction away from it */
	town_name = describe_quest_location(&town_dir, pl_ptr->x, pl_ptr->y, FALSE);

	strcpy(buf, dungeon_type_name(pl_ptr->dungeon->habitat));
	buf[0] = tolower(buf[0]);

	/* XXX XXX Create quest name */
	if (is_a_vowel(buf[0])) {
		(void)strnfmt(q_ptr->name, 256, "Find the relic %s, which is hidden in an %s, %s %s.",
				  a_name + a_ptr->name, buf, town_dir, town_name);
	} else {
		(void)strnfmt(q_ptr->name, 256, "Find the relic %s, which is hidden in a %s, %s %s.",
				  a_name + a_ptr->name, buf, town_dir, town_name);
	}

	/* Set level.  +3 because of the effort needed for this type of quest. */
	q_ptr->level = a_info[a_idx].level + 3;

	/* Artifact is now a quest item */
	/* We do this even before the quest is known, so it doesn't become irrelevant later. */
	SET_FLAG(a_ptr, TR_QUESTITEM);

	/* Done */
	return (q_num);
}




/* This function returns TRUE if nr1 and nr2 share a divider > 1 */
/* Uses the classic Euclidean algorithm, recursive and fast. */
static bool share_divider(int nr1, int nr2)
{
	int a = nr1, b = nr2;

	if (nr1 < nr2)
	{
		a = nr2;
		b = nr1;
	}

	while (b != 0)
	{
		int m = a % b;
		a = b;
		b = m;
	}

	if (a > 1 || a == 0) return (TRUE);
	else return (FALSE);
}


/*
 * Supply an artifact-idx that has not been found yet.  Do this on a weighted
 * basis.  The product of the artifact's depth and level should be lower than
 * some random number.  This ensures that low depth/level artifacts are more
 * easily chosen
 */
static u16b find_random_artifact(void)
{
	u16b a_idx;
	int min = 999999, max = 0;
	int rand, step;

	artifact_type *a_ptr;

	/* Loop through the artifacts */
	for (a_idx = 0; a_idx < z_info->a_max; a_idx++)
	{
		a_ptr = &a_info[a_idx];

		/* Skip "empty" artifacts */
		if (!a_ptr->name) continue;

		/* Cannot make an artifact twice */
		if (a_ptr->cur_num) continue;

		/* No quest items */
		if (FLAG(a_ptr, TR_QUESTITEM)) continue;

		/* Hack: no quests for artifact lights */
		if (a_ptr->tval == TV_LITE) continue;

		/* keep track of the lowest level * rarity */
		min = MIN(min, a_ptr->level * a_ptr->rarity);

		/* keep track of the highest level * rarity */
		max = MAX(max, a_ptr->level * a_ptr->rarity);
	}

	/* All the artifacts have been found! */
	if (!max) return (0);

	/* Find the selection condition */
	rand = rand_range(min, max);

	/*
	 * Select a step to go through the artifact array. That step should not
	 * share a divider with z_info->a_max to ensure that all artifacts are
	 * tried.  (basic group theory)
	 */
	do step = randint1(z_info->a_max);
	while (share_divider(step, z_info->a_max));

	/* Randomly loop through the artifacts */
	do
	{
		/* Finf the next artifact */
		a_idx = (a_idx + step) % z_info->a_max;

		/* Make a pointer to the artifact */
		a_ptr = &a_info[a_idx];

		/* Skip "empty" artifacts */
		if (!a_ptr->name) continue;

		/* Cannot make an artifact twice */
		if (a_ptr->cur_num) continue;

		/* No quest items */
		if (FLAG(a_ptr, TR_QUESTITEM)) continue;

		/* Hack: no quests for artifact lights */
		if (a_ptr->tval == TV_LITE) continue;

		/* deliver this artifact maybe */
		if (a_ptr->level * a_ptr->rarity <= rand) return (a_idx);
	}
	/* No termnation needed because there is a garantee to find an artifact */
	while (TRUE);

	return (0);
}

static bool request_find_item(int dummy)
{
	quest_type *q_ptr;
	int q_idx;

	/* Hack - ignore parameter */
	(void) dummy;

	/* Try to find a artifact to quest for */
	q_idx = insert_artifact_quest(find_random_artifact());

	if (q_idx == -1)
	{
		msgf("You have found all the relics and still want more?  Impossible.");

		message_flush();

		/* No available quests, unfortunately. */
		return (FALSE);
	}

	q_ptr = &quest[q_idx];

	/* Display a helpful message. */
	msgf("%s", q_ptr->name);

	message_flush();

	/* Remember who gave us the quest */
	set_quest_giver(q_ptr);

	/* Exit */
	return (TRUE);
}

/*
 * Tests whether a monster is suitable as a quest target.
 */
bool monster_quest(int r_idx)
{
	int i;
	monster_race * r_ptr = &r_info[r_idx];

	/* No bounty quests for multiplying monsters */
	if (FLAG(r_ptr, RF_MULTIPLY)) return (FALSE);

	/* No bounty to kill friendly monsters */
	if (FLAG(r_ptr, RF_FRIENDLY)) return (FALSE);

	/* Allow monsters only if they don't match a disallowed theme */
	if (!silly_monsters && FLAG(r_ptr, RF_SILLY)) return (FALSE);
	if (!cthulhu_monsters && FLAG(r_ptr, RF_CTH)) return (FALSE);
	if (!amber_monsters && FLAG(r_ptr, RF_AMBER)) return (FALSE);

	/* Only "hard" monsters for quests */
	if (FLAG(r_ptr, RF_NEVER_MOVE) || FLAG(r_ptr, RF_FRIENDS)) return (FALSE);
	if (r_ptr->d_char == 't' || FLAG(r_ptr, RF_WILD_TOWN)) return (FALSE);

	/* No uniques that are already dead */
	if ((FLAG(r_ptr, RF_UNIQUE) || FLAG(r_ptr, RF_UNIQUE_7))
			&& (r_ptr->cur_num >= r_ptr->max_num))
		{
			return (FALSE);
		}

	/* No monsters that are quest-reserved */
	if (FLAG(r_ptr, RF_QUESTOR)) return (FALSE);

	/* For all the quests */
	for (i = 0; i < q_max; i++)
	{
		/* If there is already a quest for this monster then give up */
		if ((quest[i].type == QUEST_TYPE_BOUNTY ||
			 quest[i].type == QUEST_TYPE_DEFEND) &&
			&r_info[quest[i].data.bnt.r_idx] == r_ptr) return (FALSE);
		if ((quest[i].type == QUEST_TYPE_DUNGEON) &&
			&r_info[quest[i].data.dun.r_idx] == r_ptr) return (FALSE);

	}

	return (TRUE);
}

static char races[] = "WTyYuUoOpPAdDhHkLzVn";
static char uraces[] = "qQwER#asSfGjJVBM";

/*
 * Tests is a monster is suitable as a "boss" monster
 */
static bool monster_boss(int r_idx)
{
	int i;
	int len;
	monster_race * r_ptr = &r_info[r_idx];

	/* Reject quest-reserved monsters */
	if (FLAG(r_ptr, RF_QUESTOR)) return (FALSE);

	/* No bounty quests for multiplying monsters */
	if (FLAG(r_ptr, RF_MULTIPLY)) return (FALSE);

	/* No bounty to kill friendly monsters */
	if (FLAG(r_ptr, RF_FRIENDLY)) return (FALSE);

	/* For all the quests */
	for (i = 0; i < q_max; i++)
	{
		/* If there is already a quest for this monster then give up */
		if ((quest[i].type == QUEST_TYPE_BOUNTY ||
			 quest[i].type == QUEST_TYPE_DEFEND) &&
			&r_info[quest[i].data.bnt.r_idx] == r_ptr) return (FALSE);
		if ((quest[i].type == QUEST_TYPE_DUNGEON) &&
			&r_info[quest[i].data.dun.r_idx] == r_ptr) return (FALSE);

	}

	/* Allow monsters only if they don't match a disallowed theme */
	if (!silly_monsters && FLAG(r_ptr, RF_SILLY)) return (FALSE);
	if (!cthulhu_monsters && FLAG(r_ptr, RF_CTH)) return (FALSE);
	if (!amber_monsters && FLAG(r_ptr, RF_AMBER)) return (FALSE);

	/* HACK: Do not make Wormtongue a quest monster */
	if (mon_name_cont(r_ptr, "Wormtongue")) return (FALSE);

	/* Allow anything that comes with escorts */
	if (FLAG(r_ptr, RF_ESCORT)) return (TRUE);
	if (FLAG(r_ptr, RF_ESCORTS)) return (TRUE);

	/* Allow anything that can summon its kin. */
	if (FLAG(r_ptr, RF_S_KIN)) return (TRUE);

	/* Allow certain monsters based on their race. */
	len = strlen(races);
	for (i = 0; i <= len; i++)
	{
		if (r_ptr->d_char == races[i]) return (TRUE);
	}

	/* Allow certain uniques */
	if (!FLAG(r_ptr, RF_UNIQUE)) return (FALSE);

	len = strlen(uraces);
	for (i = 0; i <= len; i++)
	{
		if (r_ptr->d_char == uraces[i]) return (TRUE);
	}

	return (FALSE);
}

/*
 * Checks that the given monster has lower-level "kin".
 */
static bool has_followers(int r_idx)
{
	int i;
	monster_race *r_ptr, *r2_ptr = &r_info[r_idx];
	int cnt = 0;

	for (i = 0; i < z_info->r_max; i++)
	{
		if (i == r_idx) continue;

		r_ptr = &r_info[i];

		/* Skip uniques */
		if (FLAG(r_ptr, RF_UNIQUE)) continue;

		/* Skip town monsters */
		if (!FLAG(r_ptr, RF_DUN)) continue;

		/* Skip theme-disallowed monsters */
		if (FLAG(r_ptr, RF_SILLY) && !silly_monsters) continue;
		if (FLAG(r_ptr, RF_AMBER) && !amber_monsters) continue;
		if (FLAG(r_ptr, RF_CTH) && !cthulhu_monsters) continue;

		if (r_ptr->d_char == r2_ptr->d_char &&
			r_ptr->level < r2_ptr->level)
			return TRUE;
	}

	return FALSE;
}
/*
 * Check to make sure the monster fits the "theme" for this guild.
 */
bool monster_quest_guild_theme(const monster_race *r_ptr)
{
	/* Aquatic monsters never okay */
	if (FLAG(r_ptr, RF_AQUATIC)) return (FALSE);

	switch(curr_build->type)
	{
		case BUILD_CASTLE0:
		case BUILD_CASTLE2:
			/* Anything okay */
			break;
		case BUILD_CASTLE1:
			/* Non-uniques are USUALLY rejected.  */
			if (!FLAG(r_ptr, RF_UNIQUE)) return (one_in_(25));
			break;
		case BUILD_WARRIOR_GUILD:
			/* Warrior guild: orcs, trolls, dragons, and giants. */
			if (!FLAG(r_ptr, RF_ORC) && !FLAG(r_ptr, RF_TROLL) && !FLAG(r_ptr, RF_DRAGON)
				&& !FLAG(r_ptr, RF_GIANT)) return (FALSE);
			break;
		case BUILD_MAGE_GUILD:
			/* Relatively frequent spellcasters. */
			if (r_ptr->freq_spell < 10) return (FALSE);
		case BUILD_CATHEDRAL:
			/* Cathedrals: undead & demons always okay, intelligent evil okay. */
			if (!FLAG(r_ptr, RF_UNDEAD) && !FLAG(r_ptr, RF_DEMON) &&
				  !(FLAG(r_ptr, RF_SMART) && FLAG(r_ptr, RF_EVIL))) return (FALSE);
			break;
		case BUILD_THIEVES_GUILD:
			/* Thieves guild: Good monsters okay, o.w. 'p' and 'h' only */
			if (FLAG(r_ptr, RF_GOOD)) break;
			if (r_ptr->d_char != 'p' && r_ptr->d_char != 'h') return (FALSE);
			break;
		case BUILD_RANGER_GUILD:
			/* Ranger guild: unnatural monsters */
			if (!FLAG(r_ptr, RF_DEMON) && !FLAG(r_ptr, RF_DRAGON) &&
				!FLAG(r_ptr, RF_UNDEAD) && !FLAG(r_ptr, RF_TROLL) && !FLAG(r_ptr, RF_ORC)) return (FALSE);
			break;
		case BUILD_FARM:
			/* Farm: Animals only */
			if (!FLAG(r_ptr, RF_ANIMAL)) return (FALSE);
			break;
		default:
			break;
	}
	return (TRUE);
}



static int insert_bounty_quest(u16b r_idx, u16b num)
{
	quest_type *q_ptr;

	int q_num;

	monster_race *r_ptr = &r_info[r_idx];

	/* get a new quest */
	q_num = q_pop();

	/* Paranoia */
	if (!q_num) return (-1);

	q_ptr = &quest[q_num];

	/* Bounty quest */
	q_ptr->type = QUEST_TYPE_BOUNTY;

	/* We have taken the quest */
	q_ptr->status = QUEST_STATUS_TAKEN;

	if (num != 1)
	{
		char buf[80];
		strcpy(buf, mon_race_name(r_ptr));
		plural_aux(buf);

		/* XXX XXX Create quest name */
		(void)strnfmt(q_ptr->name, 128, "Kill %d %s.", num, buf);
	}
	else if (FLAG(r_ptr, RF_UNIQUE))
	{
		/* XXX XXX Create quest name */
		(void)strnfmt(q_ptr->name, 128, "Kill %s.", mon_race_name(r_ptr));
	}
	else
	{
		/* XXX XXX Create quest name */
		(void)strnfmt(q_ptr->name, 128, "Kill %d %s.", num, mon_race_name(r_ptr));
	}

	/* We need to place the monster(s) when the dungeon is made */
	q_ptr->c_type = QC_DUN_MONST;

	/* We need to trigger when the monsters are killed */
	if (FLAG(r_ptr, RF_UNIQUE))
	{
		q_ptr->x_type = QX_KILL_UNIQUE;
	}
	else
	{
		q_ptr->x_type = QX_KILL_MONST;
	}

	/* Save the quest data */
	q_ptr->data.bnt.r_idx = r_idx;
	q_ptr->data.bnt.cur_num = 0;
	q_ptr->data.bnt.max_num = num;
	q_ptr->reward = r_ptr->level * r_ptr->level * num * 2;

	/* bonus reward for uniques */
	if (num == 1) q_ptr->reward *= 10;

	/* difficulty level is monster level, or higher if there are many */
	q_ptr->level = r_ptr->level + (num > 10 ? 1 : 0);

	/* Done */
	return (q_num);
}

static bool request_bounty(int dummy)
{
	int i;

	u16b num;
	u16b best_r_idx = 1;
	int best_level = 1;
	int q_idx;

	int r_idx;
	monster_race *r_ptr;

	quest_type *q_ptr;

	/* Hack - ignore parameter */
	(void) dummy;

	/* Get monster */
	for (i = 0; i < MAX_TRIES; i++)
	{
		/*
		 * Random monster of specified depth.
		 */
		r_idx = get_filter_mon_num(curr_scale, monster_quest);

		r_ptr = &r_info[r_idx];

		/* Require the monster to match the guild's theme */
		if (!monster_quest_guild_theme(r_ptr)) continue;


		/* Save the index if the monster is deeper than current monster */
		if (!best_r_idx || (r_info[r_idx].level > best_level))
		{
			best_r_idx = r_idx;
			best_level = r_info[r_idx].level;
		}

		/* Accept monsters that are a few levels out of depth */
		if (best_level > curr_scale + 10) break;
	}

	if (best_r_idx == 1 && curr_scale >= 100)
	{
		msgf("Sorry, I don't need any bounties today.");
		message_flush();
		return(FALSE);
	} else if (best_r_idx == 1) {
		/* We must be trying a scale that's too low.  Try again at a harder scale. */
		curr_scale += 5;
		return(request_bounty(dummy));
	}

	r_ptr = &r_info[best_r_idx];

	/* Get the number of monsters */
	if (FLAG(r_ptr, RF_UNIQUE))
	{
		num = 1;
	}
	else if (FLAG(r_ptr, RF_UNIQUE_7))
	{
		num = randint1(r_ptr->max_num - r_ptr->cur_num);
	}
	else
	{
		num = (10 + randint0(15)) / r_ptr->rarity;
	}

	/* Generate the quest */
	q_idx = insert_bounty_quest(best_r_idx, num);

	if (q_idx == -1)
	{
		msgf("Sorry, I don't need any bounties today.");

		message_flush();

		/* No available quests, unfortunately. */
		return (FALSE);
	}

	q_ptr = &quest[q_idx];

	/* Display a helpful message. */
	msgf("%s", q_ptr->name);

	message_flush();

	/* Remember who gave us the quest */
	set_quest_giver(q_ptr);

	/* Exit */
	return (TRUE);
}


static quest_type *insert_find_place_quest(void)
{
	place_type *pl_ptr;

	quest_type *q_ptr;

	cptr town_name, town_dir;

	u16b place_num;

	int q_num;

	/* Find a dungeon appropriate for this player */
	place_num = find_good_dungeon(2 * p_ptr->lev, FALSE);

	/* All dungeons have been found */
	if (!place_num)	return (NULL);

	/* Get a new quest */
	q_num = q_pop();

	/* Paranoia */
	if (!q_num) return (NULL);

	q_ptr = &quest[q_num];

	/* Store in information */
	q_ptr->type = QUEST_TYPE_FIND_PLACE;

	/* We have taken the quest */
	q_ptr->status = QUEST_STATUS_TAKEN;

	/* We don't need any special creation operation */
	q_ptr->c_type = QC_NONE;

	/* Finished when the player finds it */
	q_ptr->x_type = QX_WILD_ENTER;

	/* Get the place */
	pl_ptr = &place[place_num];

	pl_ptr->quest_num = q_num;

	/* Get name of closest town + direction away from it */
	town_name = describe_quest_location(&town_dir, pl_ptr->x, pl_ptr->y, FALSE);

	/* XXX XXX Create quest name */
	(void)strnfmt(q_ptr->name, 128, "Find a certain lost ruin, which is hidden %s %s.",
				  town_dir, town_name);

	q_ptr->data.fpl.place = place_num;

	/* Set the reward level */
	q_ptr->reward = 100 * distance(pl_ptr->x, pl_ptr->y, p_ptr->wilderness_x / 16,
					p_ptr->wilderness_y / 16);

	/* Done */
	return (q_ptr);
}

static bool request_find_place(int dummy)
{
	quest_type *q_ptr;

	/* Hack - ignore parameter */
	(void) dummy;

	/*Generate a quest to find an unknown dungeon */
	q_ptr = insert_find_place_quest();

	if (!q_ptr)
	{
		msgf("You've found all the ruins that I know of.");

		message_flush();

		/* No available quests, unfortunately. */
		return (FALSE);
	}

	/* Show it on the screen? */

	/* Display a helpful message. */
	msgf("%s", q_ptr->name);

	message_flush();

	/* Remember who gave us the quest */
	set_quest_giver(q_ptr);

	/* Exit */
	return (TRUE);
}


/*
 * The following functions are used to determine if the given monster
 * is appropriate for inclusion in a quest of the given type.
 *
 * The general selections are not allowed to include "unique" monsters.
 */


/*
 * Hack - Monster validation macro
 *
 * Line 1 -- forbid town monsters
 * Line 2 -- forbid uniques
 * Line 3 -- forbid aquatic monsters
 */
#define quest_monster_okay(I) \
	(!FLAG(&r_info[I], RF_WILD_TOWN) && \
	 !FLAG(&r_info[I], RF_UNIQUE) && \
	 !FLAG(&r_info[I], RF_AQUATIC))


#ifdef UNUSED_FUNC
/*
 * Helper monster selection function
 */
static bool quest_aux_simple(int r_idx)
{
	/* Okay */
	return (quest_monster_okay(r_idx));
}

#endif /* UNUSED_FUNC */


/*
 * Helper function for selecting undead
 */
static bool quest_aux_undead(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Validate the monster */
	if (!quest_monster_okay(r_idx)) return (FALSE);

	/* Require Undead */
	if (!FLAG(r_ptr, RF_UNDEAD)) return (FALSE);

	/* Okay */
	return (TRUE);
}


/*
 * Helper function for selecting orcs
 */
static bool quest_aux_orc(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Validate the monster */
	if (!quest_monster_okay(r_idx)) return (FALSE);

	/* Require orc */
	if (!(FLAG(r_ptr, RF_ORC))) return (FALSE);

	/* Decline undead */
	if (FLAG(r_ptr, RF_UNDEAD)) return (FALSE);

	/* Okay */
	return (TRUE);
}


/*
 * Helper function for selecting trolls
 */
static bool quest_aux_troll(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Validate the monster */
	if (!quest_monster_okay(r_idx)) return (FALSE);

	/* Require troll */
	if (!(FLAG(r_ptr, RF_TROLL))) return (FALSE);

	/* Decline undead */
	if (FLAG(r_ptr, RF_UNDEAD)) return (FALSE);

	/* Okay */
	return (TRUE);
}


/*
 * Helper function for selecting giants
 */
static bool quest_aux_giant(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Validate the monster */
	if (!quest_monster_okay(r_idx)) return (FALSE);

	/* Require giant */
	if (!(FLAG(r_ptr, RF_GIANT))) return (FALSE);

	/* Decline undead */
	if (FLAG(r_ptr, RF_UNDEAD)) return (FALSE);

	/* Okay */
	return (TRUE);
}

/*
 * Helper function for selecting dragons
 */
static bool quest_aux_dragon(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Validate the monster */
	if (!quest_monster_okay(r_idx)) return (FALSE);

	/* Require dragon */
	if (!(FLAG(r_ptr, RF_DRAGON))) return (FALSE);

	/* Decline undead */
	if (FLAG(r_ptr, RF_UNDEAD)) return (FALSE);

	/* Okay */
	return (TRUE);
}

static int pick_quest_type(quest_aux_type *l_ptr, int level)
{
	int tmp, total;

	quest_aux_type *n_ptr;

	int i;

	/* Calculate the total possibilities */
	for (i = 0, total = 0; TRUE; i++)
	{
		n_ptr = &l_ptr[i];

		/* Note end */
		if (!n_ptr->hook_func) break;

		/* Ignore excessive depth */
		if (n_ptr->level > level) continue;

		/* Count this possibility */
		total += n_ptr->chance * MAX_DEPTH / (level - n_ptr->level + 5);
	}

	if (!total) return (-1);

	/* Pick a random type */
	tmp = randint0(total);

	/* Find this type */
	for (i = 0, total = 0; TRUE; i++)
	{
		n_ptr = &l_ptr[i];

		/* Note end */
		if (!n_ptr->hook_func) break;

		/* Ignore excessive depth */
		if (n_ptr->level > level) continue;

		/* Count this possibility */
		total += n_ptr->chance * MAX_DEPTH / (level - n_ptr->level + 5);

		/* Found the type */
		if (tmp < total) break;
	}

	return (i);
}



static quest_aux_type camp_types[] =
{
	{quest_aux_undead, 10, 1, "undead"},
	{quest_aux_orc, 20, 2, "orc"},
	{quest_aux_troll, 40, 4, "troll"},
	{quest_aux_giant, 60, 1, "giant"},
	{quest_aux_dragon, 80, 1, "dragon"},
	{NULL, 0, 0, NULL},
};



/*
 * Pick a quest to use
 */
void pick_wild_quest(int *xsize, int *ysize, byte *flags)
{
	/* Hack - don't worry too much now, we only have one type of quest */

	/* Random size */
	*xsize = randint1(5);
	*ysize = randint1(5);

	/* On normal terrain */
	*flags = Q_GEN_PICKY;
}

/*
 * Look to see if a wilderness block is able to have
 * a quest overlayed on top.
 */
bool quest_blank(int x, int y, int xsize, int ysize, int place_num, byte flags)
{
	int i, j;
	wild_gen2_type *w_ptr;
	place_type *pl_ptr = &place[place_num];

	/* Hack - Population check */
	if (randint0(256) > wild[y][x].trans.pop_map) return (FALSE);

	for (i = x - 1; i < x + xsize + 2; i++)
	{
		for (j = y - 1; j < y + ysize + 2; j++)
		{
			/* Hack - Not next to boundary */
			if ((i <= 0) || (i >= max_wild - 1) ||
				(j <= 0) || (j >= max_wild - 1))
			{
				return (FALSE);
			}

			w_ptr = &wild[j][i].trans;

			/* No place already */
			if (w_ptr->place) return (FALSE);

			/* Picky quests require "normal terrain" */
			if (flags & Q_GEN_PICKY)
			{
				/* No water or lava or acid */
				if (w_ptr->
					info & (WILD_INFO_WATER | WILD_INFO_LAVA | WILD_INFO_ACID))
				{
					return (FALSE);
				}
			}

			/* Ocean quests must be on water */
			if (flags & Q_GEN_OCEAN)
			{
				/* Not on Ocean? */
				if (w_ptr->hgt_map >= (256 / SEA_FRACTION)) return (FALSE);
			}
			else
			{
				/* Otherwise, Ocean is not allowed */
				if (w_ptr->hgt_map < (256 / SEA_FRACTION)) return (FALSE);
			}
		}
	}

	/* Look to see if another town / quest is too close */
	for (i = 1; i < place_num; i++)
	{
		if (distance(place[i].x, place[i].y, x, y) < MIN_DIST_QUEST)
		{
			/* Too close? */
			return (FALSE);
		}
	}

	/* Save size */
	pl_ptr->xsize = xsize;
	pl_ptr->ysize = ysize;

	/* Ok then */
	return (TRUE);
}


/*
 * Create a quest in the wilderness
 */
bool create_quest(int x, int y, int place_num)
{
	int i, j;
	int q_num, qtype;
	cptr town_name, town_dir;

	wild_type *w_ptr = &wild[y][x];

	place_type *pl_ptr = &place[place_num];

	quest_type *q_ptr;

	/* Select type of monster to place in the camp */
	qtype = pick_quest_type(camp_types, (255 - w_ptr->trans.law_map) / 3);

	/* Is the area too easy for the quests? */
	if (qtype == -1) return (FALSE);

	/* Get a new quest */
	q_num = q_pop();

	/* Paranoia */
	if (!q_num) return (FALSE);

	/* Get a random seed for later */
	pl_ptr->seed = randint0(0x10000000);

	/* Quest */
	pl_ptr->type = PL_QUEST_PIT;
	pl_ptr->monst_type = TOWN_MONST_MONST;
	pl_ptr->x = x;
	pl_ptr->y = y;
	pl_ptr->quest_num = q_num;

	/* Data value is used as a counter of "active" blocks */
	pl_ptr->data = 0;

	if ((!pl_ptr->xsize) || (!pl_ptr->ysize)) quit("Zero quest size");

	/* Link wilderness to quest */
	for (i = 0; i < pl_ptr->xsize; i++)
	{
		for (j = 0; j < pl_ptr->ysize; j++)
		{
			w_ptr = &wild[y + j][x + i];

			/*
			 * Add quest to wilderness
			 * Note: only 255 can be stored currently.
			 */
			w_ptr->trans.place = (byte)place_num;

			/* Increment "active block" counter */
			pl_ptr->data++;
		}
	}

	/* Set up quest */
	q_ptr = &quest[q_num];

	/* Store in information */
	q_ptr->type = QUEST_TYPE_WILD;

	/* We don't need a special generator */
	q_ptr->c_type = QC_NONE;

	/* We need to trigger when the player enters the wilderness block */
	q_ptr->x_type = QX_WILD_ENTER;

	/* Get name and direction of closest town to quest */
	town_name = describe_quest_location(&town_dir, pl_ptr->x, pl_ptr->y, FALSE);

	/* Create quest name */
	(void)strnfmt(q_ptr->name, 128, "Defeat the %s camp %s %s.",
				  camp_types[qtype].name, town_dir, town_name);

	/* Save the quest data */
	q_ptr->data.wld.place = place_num;
	q_ptr->data.wld.data = qtype;
	/* q_ptr->data.wld.depth = (255 - w_ptr->trans.law_map) / 3; */

	return (TRUE);
}


/*
 * Draw the quest onto its region
 */
void draw_quest(place_type *pl_ptr)
{
	int x, y, n;
	int i, j;

	wild_type *w_ptr = &wild[pl_ptr->y][pl_ptr->x];

	quest_type *q_ptr = &quest[pl_ptr->quest_num];

	cave_type *c_ptr;

	/* Object theme */
	obj_theme theme;

	int depth = w_ptr->done.mon_gen;

	/* Paranoia */
	if (pl_ptr->region) quit("Quest already has region during creation.");

	/* Get region */
	create_region(pl_ptr, pl_ptr->xsize * WILD_BLOCK_SIZE,
						 pl_ptr->ysize * WILD_BLOCK_SIZE,
						 REGION_OVER);

	/* Hack - do not increment refcount here - let allocate_block do that */

	/* Hack -- Use the "simple" RNG */
	Rand_quick = TRUE;

	/* Hack -- Induce consistant quest layout */
	Rand_value = pl_ptr->seed;

	/* Apply the monster restriction */
	get_mon_num_prep(camp_types[q_ptr->data.wld.data].hook_func);

	/* Set theme for weapons / armour */
	theme.treasure = 0;
	theme.combat = 100;
	theme.magic = 0;
	theme.tools = 0;

	init_match_theme(theme);

	/* Prepare allocation table */
	get_obj_num_prep(kind_is_theme);

	/* Pick number random spots within region */
	n = (pl_ptr->xsize * pl_ptr->ysize) / 4;

	while (n != 0)
	{
		/* Decrement counter */
		n--;

		/* Get spot */
		x = randint0(pl_ptr->xsize * 2);
		y = randint0(pl_ptr->ysize * 2);

		current_object_source.type = OM_FLOOR;
		/* Hack: The description doesn't care which quest pit it was. */
        current_object_source.place_num = 1;
		current_object_source.depth = 0;
		current_object_source.data = 0;

		/* Place ground */
		for (i = 0; i < 8; i++)
		{
			for (j = 0; j < 8; j++)
			{
				/* Get location */
				c_ptr = cave_p(x * 8 + i, y * 8 + j);

				/* Draw a roughly circular blob */
				if (randint0(distance(0, 0, i, j)) < 4)
				{
					if (one_in_(3))
					{
						c_ptr->feat = FEAT_PEBBLES;
					}
					else
					{
						c_ptr->feat = FEAT_DIRT;
					}

					/* Place monsters on spots */
					if (one_in_(QUEST_CAMP_MON))
					{
						/* Pick a race to clone */
						u16b r_idx;

						/* Pick a race to clone */
						r_idx = get_mon_num(depth);

						/* Use it */
						c_ptr->m_idx = r_idx;
					}

					/* Place weapons + armour around the spots */
					if (one_in_(QUEST_CAMP_OBJ))
					{
						c_ptr->o_idx = get_obj_num(depth, depth / 3);
					}
				}
			}
		}
	}


	/* Set theme for junk */
	theme.treasure = 5;
	theme.combat = 0;
	theme.magic = 0;
	theme.tools = 5;

	init_match_theme(theme);

	/* Clear allocation table */
	get_obj_num_prep(kind_is_theme);

	/* Scatter stuff over the region */
	for (i = 0; i < pl_ptr->xsize * WILD_BLOCK_SIZE; i++)
	{
		for (j = 0; j < pl_ptr->ysize * WILD_BLOCK_SIZE; j++)
		{
			/* Only on some squares */
			if (!one_in_(QUEST_CAMP_SCATTER)) continue;

			/* Get location */
			c_ptr = cave_p(i, j);

			/* Not on allocated squares */
			if (c_ptr->feat) continue;

			if (one_in_(3))
			{
				c_ptr->feat = FEAT_PEBBLES;
			}
			else
			{
				c_ptr->feat = FEAT_DIRT;
			}

			/* Place monsters on spots */
			if (one_in_(QUEST_CAMP_MON))
			{
				u16b r_idx;

				/* Pick a race to clone */
				r_idx = get_mon_num(depth);

				/* Use it */
				c_ptr->m_idx = r_idx;

				/* Place junk under monsters */
				if (one_in_(QUEST_CAMP_OBJ))
				{
					c_ptr->o_idx = get_obj_num(depth, 0);
				}
			}
		}
	}

	/* Activate quest + we know about the quest */
	q_ptr->flags |= (QUEST_FLAG_ACTIVE);

	/* Hack - we now take this quest */
	if (q_ptr->status == QUEST_STATUS_UNTAKEN)
	{
		q_ptr->status = QUEST_STATUS_TAKEN;
	}

	/* Mega-hack Give a message if we "discover" it */
	quest_discovery();

	/* We know about it now */
	q_ptr->flags |= QUEST_FLAG_KNOWN;

	/* Hack XXX XXX (No quest-giving store yet) */

	/* Hack -- use the "complex" RNG */
	Rand_quick = FALSE;

	/* Remove the monster restriction */
	get_mon_num_prep(NULL);
}

static int create_quest_dungeon(store_type * st_ptr, int lev)
{
	int r_idx;
	monster_race * r_ptr;
	int i;
	int best_level = 0;
	int best_r_idx = 0;
	int num;

	/* Save building */
	curr_build = st_ptr;

	for (i = 0; i < MAX_TRIES; i++)
	{
		r_idx = get_filter_mon_num(lev, monster_quest);
		r_ptr = &r_info[r_idx];

		if (!monster_quest_guild_theme(r_ptr)) continue;

		if (!best_r_idx || r_ptr->level > best_level)
		{
			best_r_idx = r_idx;
			best_level = r_ptr->level;
		}

		/* 5 levels OOD is enough */
		if (best_level > lev+5)
			break;
	}

	if (!best_r_idx)
	{
		/* Never generated one.  Level must be too low. */
		if (lev < 96) return (create_quest_dungeon(st_ptr, lev+5));
	}

	r_ptr = &r_info[best_r_idx];

	/* Get the number of monsters */
	if (FLAG(r_ptr, RF_UNIQUE))
	{
		num = 1;
	}
	else if (FLAG(r_ptr, RF_UNIQUE_7))
	{
		num = randint1(r_ptr->max_num - r_ptr->cur_num);
	}
	else
	{
		num = (5 + randint0(p_ptr->max_lev / 2)) / r_ptr->rarity;
	}

	/* Force bounds */
	lev = MIN(lev, 99);
	lev = MAX(lev, 1);

	/* Generate the quest */
	return(insert_dungeon_monster_quest(best_r_idx, num, lev));
}


static int create_quest_clearout(store_type * st_ptr, int x, int y, int p_num)
{
	int i;
	int tot = 0;
	int choice;
	int d_idx=0;

	/* Quest level */
	int lev = damroll(5,5)-5+((256 - wild[y][x].trans.law_map)/4);

	lev = MIN(lev, 70);
	lev = MAX(lev, 1);

	/* Hack: Ignore input */
	(void)st_ptr;


	for (i = 0; i < NUM_DUN_TYPES_BASIC; i++)
	{
		/* Is this type going to generate levels at the required depth? */
		if (dungeons[i].min_level > lev || dungeons[i].max_level < lev) continue;

		tot++;
	}

	/* Paranoia */
	if (tot == 0)
	{
		msgf ("Failed to find suitable dungeon type in create_quest_clearout");
		return (-1);
	}

	choice = randint1(tot);

	for (i = 0; i < NUM_DUN_TYPES_BASIC; i++)
	{
		/* Is this type going to generate levels at the required depth? */
		if (dungeons[i].min_level > lev || dungeons[i].max_level < lev) continue;

		choice--;
		if (choice == 0)
		{
			d_idx = i;
			break;
		}
	}

	return(insert_clearout_quest(d_idx, lev, x, y, p_num));
}


static int create_quest_kill(store_type * st_ptr, int x, int y, int p_num)
{
	int r_idx;
	monster_race * r_ptr;
	int i;
	int best_level = 0;
	int best_r_idx = 0;

	/* Quest level */
	int lev = damroll(5,6)-10+((256 - wild[y][x].trans.law_map)/4);

	lev = MIN(lev, 90);
	lev = MAX(lev, 1);

	/* Save building */
	curr_build = st_ptr;

	while (TRUE)
	{
		for (i = 0; i < MAX_TRIES; i++)
		{
			/* Choose only "hard" monsters */
			r_idx = get_filter_mon_num(lev, monster_quest);
			r_ptr = &r_info[r_idx];

			if (!monster_quest_guild_theme(r_ptr)) continue;

			/* Hack: don't let Wormtongue be in a quest */
			if (mon_name_cont(r_ptr, "Wormtongue")) continue;

			/* To avoid too many uniques being quest targets, do a random check */
			if (FLAG(r_ptr, RF_UNIQUE) && (randint1(100) > 110 - r_ptr->level)) continue;

			if (!best_r_idx || r_ptr->level > best_level)
			{
				best_r_idx = r_idx;
				best_level = r_ptr->level;
			}

			/* 5 extra levels OOD is enough */
			if (best_level > lev+10)
				break;
		}

		/* Success */
		if (best_r_idx) break;

		/* Have to stop */
		if (lev > 94) break;

		lev += 5;
	}

	r_ptr = &r_info[best_r_idx];

	/* Generate the quest */

	/* Hack: no kill quests for ideal bosses; make boss quests instead. */
	if (FLAG(r_ptr, RF_UNIQUE) && (FLAG(r_ptr, RF_ESCORT) || FLAG(r_ptr, RF_ESCORTS)))
		return (insert_boss_quest(best_r_idx, x, y, p_num));

	return(insert_kill_quest(best_r_idx, x, y, p_num));
}

static u16b store_type_to_mg_flags(int type)
{
	switch(type)
	{
		case BUILD_CASTLE0:
			return MGF_CASTLE0;
		case BUILD_CASTLE1:
			return MGF_CASTLE1;
		case BUILD_CASTLE2:
			return MGF_CASTLE2;
		case BUILD_WARRIOR_GUILD:
			return MGF_WARRIOR_GUILD;
		case BUILD_THIEVES_GUILD:
			return MGF_THIEVES_GUILD;
		case BUILD_RANGER_GUILD:
			return MGF_RANGER_GUILD;
		case BUILD_MAGE_GUILD:
			return MGF_MAGE_GUILD;
		case BUILD_CATHEDRAL:
			return MGF_CATHEDRAL;
		case BUILD_INN:
			return MGF_INN;
		case BUILD_FARM:
			return MGF_FARM;
		default:
			return 0x0000;
	}
}


static int create_quest_den(store_type * st_ptr, int x, int y, int p_num)
{
	monster_group_type *mg_ptr;
	int i;
	s32b tot = 0;
	int mg_idx;
	s32b choice;

	/* Quest level */
	int lev = damroll(5,5)-8+((256 - wild[y][x].trans.law_map)/4);

	lev = MIN(lev, 90);
	lev = MAX(lev, 1);

	while (TRUE)
	{
		/* Gather total weights */
		for (i = 0; i < z_info->mg_max; i++)
		{
			mg_ptr = &mg_info[i];

			/* Only use groups that match this building */
			if (!(store_type_to_mg_flags(st_ptr->type) & mg_ptr->flags)) continue;

			/* lev must be in the range */
			if (lev < mg_ptr->min_level - mg_ptr->adj_level) continue;
			if (lev > mg_ptr->max_level - mg_ptr->adj_level) continue;

			tot += mg_ptr->weight;
		}

		if (tot == 0)
		{
			/* No den quests available for this store at this level.  Try another
			   level. */
			lev += rand_range(-5,5);
			lev = MIN(lev, 90);
			lev = MAX(lev, 1);
			continue;
		}

		/* Success */
		break;
	}

	choice = randint1(tot);

	mg_idx = -1;

	/* Find the group we chose */
	for (i = 0; i < z_info->mg_max; i++)
	{
		mg_ptr = &mg_info[i];

		/* Only use groups that match this building */
		if (!(store_type_to_mg_flags(st_ptr->type) & mg_ptr->flags)) continue;

		/* lev must be in the range */
		if (lev < mg_ptr->min_level - mg_ptr->adj_level) continue;
		if (lev > mg_ptr->max_level - mg_ptr->adj_level) continue;

		choice -= mg_ptr->weight;
		if (choice <= 0)
		{
			/* This is it */
			mg_idx = i;
			break;
		}
	}

	/* Paranoia */
	if (mg_idx == -1) quit ("Error in selecting monster group in create_quest_den");

	return (insert_den_quest(mg_idx, lev, x, y, p_num));
}

static int create_quest_boss(store_type * st_ptr, int x, int y, int p_num)
{
	int r_idx;
	monster_race * r_ptr;
	int i;
	int best_level = 0;
	int best_r_idx = 0;

	/* Quest level */
	int lev = damroll(5,6)-10+((256 - wild[y][x].trans.law_map)/4);

	lev = MIN(lev, 90);
	lev = MAX(lev, 1);

	/* Save building */
	curr_build = st_ptr;

	while (TRUE)
	{
		for (i = 0; i < MAX_TRIES; i++)
		{
			r_idx = get_filter_mon_num(lev, monster_boss);

			r_ptr = &r_info[r_idx];

			if (!monster_quest_guild_theme(r_ptr))
			{
				continue;
			}

			/* To avoid too many uniques being quest targets, do a random check */
			if (FLAG(r_ptr, RF_UNIQUE) && (randint1(100) > 110 - r_ptr->level))
			{
				continue;
			}

			if (!best_r_idx || r_ptr->level > best_level)
			{
				best_r_idx = r_idx;
				best_level = r_ptr->level;
			}

			/* 5 levels OOD is enough */
			if (best_level > lev+5)
				break;
		}

		/* Success */
		if (best_r_idx) break;

		/* Can't try again */
		if (lev > 94) break;

		lev += 5;
	}

	/* Hack: If this "boss" is the weakest of its race, make this a kill quest instead. */
	if (has_followers(best_r_idx))
		return (insert_boss_quest(best_r_idx, x, y, p_num));
	else
		return (insert_kill_quest(best_r_idx, x, y, p_num));
}

static int create_quest_bounty(store_type * st_ptr, int lev)
{
	int r_idx;
	monster_race * r_ptr;
	int i;
	int best_level = 0;
	int best_r_idx = 0;
	int num;

	/* Save building */
	curr_build = st_ptr;

	for (i = 0; i < MAX_TRIES; i++)
	{
		r_idx = get_filter_mon_num(lev, monster_quest);
		r_ptr = &r_info[r_idx];

		if (!monster_quest_guild_theme(r_ptr)) continue;

		if (!best_r_idx || r_ptr->level > best_level)
		{
			best_r_idx = r_idx;
			best_level = r_ptr->level;
		}

		/* 5 levels OOD is enough */
		if (best_level > lev+5)
			break;
	}

	if (!best_r_idx)
	{
		/* Never generated one.  Level must be too low. */
		if (lev < 96) return (create_quest_bounty(st_ptr, lev+5));
	}

	r_ptr = &r_info[best_r_idx];

	/* Get the number of monsters */
	if (FLAG(r_ptr, RF_UNIQUE))
	{
		num = 1;
	}
	else if (FLAG(r_ptr, RF_UNIQUE_7))
	{
		num = randint1(r_ptr->max_num - r_ptr->cur_num);
	}
	else
	{
		num = (5 + randint0(p_ptr->max_lev / 2)) / r_ptr->rarity;
	}

	/* Generate the quest */
	return(insert_bounty_quest(best_r_idx, num));
}


static int create_quest_artifact(store_type * st_ptr, int lev)
{
	int best_a_idx = find_random_artifact();
	int a_idx;
	int i;
	int score = 0;
	int best_score = ABS(lev - a_info[best_a_idx].level);

	/* Ignore input */
	(void)st_ptr;

	/* Try several random artifacts */
	for (i = 0; i < 20; i++)
	{
		a_idx = find_random_artifact();

		/* Compare to "best" */
		score = ABS(lev - a_info[a_idx].level);
		if (score < best_score)
		{
			best_score = score;
			best_a_idx = a_idx;
		}
	}

	return(insert_artifact_quest(best_a_idx));
}

/*
 * Creates a new quest for the given store at the specified
 * level, and returns the index of the inserted quest.
 *
 * Should *REALLY* never fail; if it does, we will quit.
 */
static int create_quest_one(store_type * st_ptr)
{
	int q_idx;
	int n;
	int x, y, p_num;

	/*
	 * Pick n, which determines what type of quest we try to
	 * create.
	 *  n = 1:  Dungeon guardian quest.
	 *  n = 2:  Simple dungeon clearout.
	 *  n = 3:  Single monster lair clearout.
	 *  n = 4/5:  Themed monster den.
	 *  n = 6:  Unique monster / Boss monster stronghold clearout.
	 *  n = 7:  Monster bounty.
	 *  n = 8:  Find an artifact.
	 */

	switch(st_ptr->type)
	{
		case BUILD_CASTLE0:
		case BUILD_CASTLE2:
			n = randint1(8);
			if (n >= 7) n = 6;
			break;
		case BUILD_CASTLE1:
			n = randint1(10);
			if (n >= 9) n = 6;
			break;
		case BUILD_WARRIOR_GUILD:
			n = rand_range(2,7);
			break;
		case BUILD_THIEVES_GUILD:
			n = rand_range(2,7);
			if (n == 7) n = 8;
			break;
		case BUILD_MAGE_GUILD:
			n = rand_range(3,7);
			if (n == 7) n = 8;
			break;
		case BUILD_CATHEDRAL:
			n = rand_range(1,6);
			break;
		case BUILD_RANGER_GUILD:
			n = rand_range(2,8);
			if (n == 8) n = 4;
			break;
		default:
			msgf ("Unknown quest building type.");
			n = randint1(7);
			break;
	}

	/* Acquire quest location, if it will be needed */
	if (n > 1 && n < 7)
	{
		pick_quest_location(&x, &y, &p_num);
	}

	switch(n)
	{
		case 1:
			q_idx = create_quest_dungeon(st_ptr, randint1(85));
			break;
		case 2:
			q_idx = create_quest_clearout(st_ptr, x, y, p_num);
			break;
		case 3:
			q_idx = create_quest_kill(st_ptr, x, y, p_num);
			break;
		case 4:
		case 5:
			q_idx = create_quest_den(st_ptr, x, y, p_num);
			break;
		case 6:
			q_idx = create_quest_boss(st_ptr, x, y, p_num);
			break;
		case 7:
			q_idx = create_quest_bounty(st_ptr, randint1(75));
			break;
		case 8:
			q_idx = create_quest_artifact(st_ptr, damroll(10,10)-9);
			break;
		default:
			q_idx = -1;
			break;
	}

	if (q_idx == -1)
	{
		get_check("ERROR!  Unable to create quest.");
		quit("Unable to create quest.");
	}

	quest[q_idx].status = QUEST_STATUS_UNTAKEN;

	return (q_idx);
}

/*
 * Create a sequenced list of quests for
 * the given building.
 *
 * We are guaranteed that st_ptr->type >= BUILD_CASTLE0,
 * but not that st_ptr is actually a quest building.
 *
 */
static void create_quest_list(int place_num, int store_num)
{
	int q_num;
	int num_quests = 0;
	int i;
	store_type *st_ptr = &place[place_num].store[store_num];

	/* Determine number of quests to create */
	switch (st_ptr->type)
	{
		case BUILD_CATHEDRAL:
		case BUILD_WARRIOR_GUILD:
		case BUILD_THIEVES_GUILD:
		case BUILD_MAGE_GUILD:
		case BUILD_RANGER_GUILD:
			num_quests = 2;
			break;
		case BUILD_CASTLE2:
		case BUILD_CASTLE0:
			num_quests = 3;
			break;
		case BUILD_CASTLE1:
			num_quests = 4;
			break;
		default:
			/* Non-quest building */
			return;
	}

	/* Add 0-2 quests */
	num_quests += randint0(3);

	/* Create the quests */
	for (i = 0; i < num_quests; i++)
	{
		q_num = create_quest_one(st_ptr);

		/* Remember the quest giver */
		quest[q_num].place = place_num;
		quest[q_num].shop = store_num;
	}
}

/*
 * Set up building quests
 */
void init_build_quests(void)
{
	int i, j;
	
	clear_row(0);
	prtf (0, 0, "Please wait: initializing quests");
	Term_fresh();
	
	/* Go through all guilds and castles, creating a list of quests for each. */
	for (i = 0; i < place_count; i++)
	{
		curr_place_num = i;

		if (!place[i].numstores) continue;

		for (j = 0; j < place[i].numstores; j++)
		{
			store_type *st_ptr = &place[i].store[j];

			/* not a quest building */
			if (st_ptr->type < BUILD_CASTLE0) continue;
			create_quest_list(i, j);
		}

		if (place[i].type != PL_TOWN_FRACT && place[i].type != PL_TOWN_OLD) continue;

		prtf (32+i, 0, ".");
		Term_fresh();
		
	}
}

/*
 * Initial quest setup
 *
 * IMPORTANT: There should be no random choices made in this function, for
 * the sake of having competition_mode work properly. 
 */
void init_player_quests(void)
{
	int i;

	/* Reset number of quests */
	q_max = 1;

	/* Clear all the quests */
	for (i = 0; i < z_info->q_max; i++)
	{
		quest_wipe(i);
	}

	/* Add the winner quests */

	if (amber_monsters) {
		/* Hack XXX XXX Oberon, hard coded */
		insert_winner_quest(QW_OBERON, 1, 99);

		/* Hack XXX XXX Serpent, hard coded */
		insert_winner_quest(QW_SERPENT, 1, 100);
	} else {
		int m[2];
		int n[2];
		int i, j;

		/* MEGA-hack: when "amber monsters" option is off,
		   make Sauron and Morgoth the winner quests again.. */
		insert_winner_quest(QW_SAURON, 1, 99);
		insert_winner_quest(QW_MORGOTH, 1, 100);

		/* BUT, if we do this, make Sauron and Morgoth as tough
		   as Oberon and the Serpent. */
		m[0] = QW_SAURON;
		n[0] = QW_OBERON;
		m[1] = QW_MORGOTH;
		n[1] = QW_SERPENT;

		for (i = 0; i < 2; i++) {
			r_info[m[i]].hdice = r_info[n[i]].hdice;
			r_info[m[i]].hside = r_info[n[i]].hside;
			r_info[m[i]].ac = r_info[n[i]].ac;
			r_info[m[i]].sleep = r_info[n[i]].sleep;
			r_info[m[i]].aaf = r_info[n[i]].aaf;
			r_info[m[i]].speed = r_info[n[i]].speed;
			r_info[m[i]].mexp = r_info[n[i]].mexp;
			r_info[m[i]].extra = r_info[n[i]].extra;
			for (j = 0; j < 9; j++) {
				r_info[m[i]].flags[j] = r_info[n[i]].flags[j];
			}
			r_info[m[i]].level = r_info[n[i]].level;
			r_info[m[i]].rarity = r_info[n[i]].rarity;
			for (j = 0; j < 4; j++) {
				r_info[m[i]].blow[j].method = r_info[n[i]].blow[j].method;
				r_info[m[i]].blow[j].effect = r_info[n[i]].blow[j].effect;
				r_info[m[i]].blow[j].d_dice = r_info[n[i]].blow[j].d_dice;
				r_info[m[i]].blow[j].d_side = r_info[n[i]].blow[j].d_side;
			}
			r_info[m[i]].obj_drop.tools = r_info[n[i]].obj_drop.tools;
			r_info[m[i]].obj_drop.magic = r_info[n[i]].obj_drop.magic;
			r_info[m[i]].obj_drop.combat = r_info[n[i]].obj_drop.combat;
			r_info[m[i]].obj_drop.treasure = r_info[n[i]].obj_drop.treasure;
		}

	}

}


/*
 * Initialise the quests
 */
errr init_quests(void)
{
	int i;

	/* Make the quest array */
	C_MAKE(quest, z_info->q_max, quest_type);

	/* Reset number of quests */
	q_max = 1;

	/* Wipe the quests */
	for (i = 0; i < z_info->q_max; i++)
	{
		quest_wipe(i);
	}

	return (0);
}

/*
 * Wipe QUESTOR flag off everything that shouldn't inherently have it.
 * Wipe QUESTITEM flag too.
 */
void wipe_all_quest_flags(void)
{
	int i;
	for (i = 0; i < z_info->a_max; i++)
	{
		/* Reserve the Iron Crown of Chaos, Grond, and the Stormbringer */
		if (i == ART_GROND || i == ART_MORGOTH || i == ART_STORMBRINGER)
			continue;
		
		/* Remove QUESTITEM flag */
		a_info[i].flags[2] &= ~TR2_QUESTITEM;
	}
	
	for (i = 0; i < z_info->r_max; i++)
	{
		/* Reserve Oberon and the Serpent */
		if (i == QW_OBERON || i == QW_SERPENT)
			continue;
		
		/* Remove QUESTOR flag */
		r_info[i].flags[0] &= ~RF0_QUESTOR;
	}
}
	
	
	
