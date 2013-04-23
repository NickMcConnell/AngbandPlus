#define MONSTER2_C
/* File: monster.c */

/* Purpose: misc code for monsters */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

#define MAX_HORROR 20
#define MAX_FUNNY 22
#define MAX_COMMENT 5

static cptr horror_desc[MAX_HORROR] =
{
	"abominable",
	"abysmal",
	"appalling",
	"baleful",
	"blasphemous",

	"disgusting",
	"dreadful",
	"filthy",
	"grisly",
	"hideous",

	"hellish",
	"horrible",
	"infernal",
	"loathsome",
	"nightmarish",

	"repulsive",
	"sacrilegious",
	"terrible",
	"unclean",
	"unspeakable",
};

static cptr funny_desc[MAX_FUNNY] =
{
	"silly",
	"hilarious",
	"absurd",
	"insipid",
	"ridiculous",

	"laughable",
	"ludicrous",
	"far-out",
	"groovy",
	"postmodern",

	"fantastic",
	"dadaistic",
	"cubistic",
	"cosmic",
	"awesome",

	"incomprehensible",
	"fabulous",
	"amazing",
	"incredible",
	"chaotic",

	"wild",
	"preposterous",
};

static cptr funny_comments[MAX_COMMENT] =
{
	"Wow, cosmic, man!",
	"Rad!",
	"Groovy!",
	"Cool!",
	"Far out!"
};

/* Check that a grid is available for monster placement. */
#define space_okay(y,x,dis) (cave_naked_bold(y,x) && distance(y,x,py,px) > dis)

/* How many "many" is */
#define MANY_SPACES 128

/*
 * Find a random space. First we find out whether there are a large number of
 * empty spaces, a small number, or none at all. If there are a large number,
 * we pick spaces completely at random until we find one of them. If there are
 * a small number, we pick a random space from amongst them. If there are none
 * at all, we do nothing.
 */
static bool find_space(int *y, int *x, int dis)
{
	int i;
	s16b empty_space[MANY_SPACES][2];
	u16b max_tries = (full_grid == MAX_FULL_GRID) ? 1000 : 1000000;
	/* Hack - we use the special value of full_grid = MAX_FULL_GRID-1 to
	 * start the thorough search. We don't try otherwise.
	 */

	if (full_grid < MAX_FULL_GRID)
	{
		for ((*x) = 0, i = 0; (*x) < cur_wid && i < MANY_SPACES; (*x)++)
		{
			for ((*y) = 0; (*y) < cur_hgt && i < MANY_SPACES; (*y)++)
			{
				if (space_okay((*y),(*x), dis))
				{
					empty_space[i][0] = (*y);
					empty_space[i++][1] = (*x);
				}
			}
		}
	}
	else
	{
		i = MANY_SPACES;
	}
	switch (i)
	{
		case 0: /* No empty spaces at all */
		/* Remember that we've given up */
		full_grid = dis;

		if (cheat_xtra || cheat_hear)
		{
			msg_print("Warning! Could not allocate a new monster. Small level?");
		}
		return FALSE;
		case MANY_SPACES: /* Lots of empty spaces, so the RNG should find one soon enough. */
		for (i = 0; i < max_tries; i++)
		{
			/* Pick a location */
			(*y) = rand_int(cur_hgt);
			(*x) = rand_int(cur_wid);

			/* Return it if good */
			if (space_okay((*y), (*x), dis)) return TRUE;
		}
		/* The RNG just ignored them! */
		if (full_grid < MAX_FULL_GRID)
		{
			if (alert_failure || cheat_hear)
				msg_print("Warning! RNG not detecting empty spaces!");
		}
		/* We're having trouble placing monsters, so start searching for them instead. */
		else
		{
			if (cheat_hear)
				msg_print("Failed to find a space, so trying again.");
			full_grid--;
			return find_space(y, x, dis);
		}
		/* Fall through anyway in order to get a space. */
		default: /* Between 1 and MANY_SPACE-1 spaces, so pick one at random */
		if (cheat_hear)
		{
			msg_format("Looking for one of %d spaces", i);
		}
		i = rand_int(i);
		(*y)=empty_space[i][0];
		(*x)=empty_space[i][1];
		return TRUE;
	}
}

/*
 * Prepare the ghost.
 */
static void set_ghost_aux(cptr gb_name, int ghost_race, int lev)
{
	monster_race *r_ptr;

	monster_race *rt_ptr;

	int i;

	cptr g_name, gr_name;

	/* Paranoia - unwanted races should be disabled, not deleted. */
	if (ghost_race >= 0 && ghost_race < MAX_RACES)
	{
		gr_name=race_info[ghost_race].title;
	}
	else
	{
		gr_name = "Alien";
	}

	/* Pick a level to obtain the ghost for. */
	lev = (lev/4)*4 + rand_int(12);

	/* Find a ghost for this level. */
	for (r_ptr = r_info, rt_ptr = NULL; r_ptr < r_info+MAX_R_IDX; r_ptr++)
	{
		/* Not a ghost. */
		if (!(r_ptr->flags3 & RF3_PLAYER_GHOST)) continue;

		/* Too deep. */
		if (r_ptr->level > lev) continue;

		/* Remember the new deepest ghost. */
		if (!rt_ptr || r_ptr->level > rt_ptr->level) rt_ptr = r_ptr;
	}

	/* None were found, so complain and use a default race. */
	if (!rt_ptr)
	{
		rt_ptr = r_info+MON_NEWT;
		if (alert_failure)
			msg_format("Failed to allocate a ghost for level %d!", lev);
	}

	/* Now write the ghost. */
	r_ptr = &r_info[MON_PLAYER_GHOST];

	/* Combine the file defaults in r_ptr with the prototype defaults in rt_ptr. */

	/* First, the name. */
	if (strstr(r_name+rt_ptr->name, "RACE"))
	{
		/* Replace RACE with the actual race. */
		cptr r = r_name+rt_ptr->name;
		cptr s = strstr(r, "RACE");
		g_name = format("%s, %.*s%s%s", gb_name, s-r, r, gr_name, s+4);
	}
	else
	{
		/* Simply copy the strings. */
		g_name = format("%s, %s", gb_name, r_name+rt_ptr->name);
	}

	/* Hack - assume that the names are always in increasing order. */
	i = r_ptr[1].name - r_ptr->name - 1;

	/* Copy to r_name (r_ptr->level stores its maximum length). */
	sprintf(r_name+r_ptr->name, "%.*s", i, g_name);

	/* The hit dice in rt_ptr are a multiple of the base values. */
	i = (int)(r_ptr->hdice) * rt_ptr->hdice;
	if (i < 256) r_ptr->hdice *= rt_ptr->hdice;
	i = (int)(r_ptr->hside) * rt_ptr->hside;
	if (i < 256) r_ptr->hside *= rt_ptr->hside;

	/* The experience is similar, but is boosted by 100 for finer control. */
	r_ptr->mexp = MAX(r_ptr->mexp * rt_ptr->mexp / 100, 0);

	/* The ghost doesn't yet exist. */
	r_ptr->cur_num = 0;
	r_ptr->max_num = 1;

	/* Most other fields are simply copied. */
	r_ptr->level = rt_ptr->level;
	r_ptr->ac = rt_ptr->ac;
	r_ptr->sleep = rt_ptr->sleep;
	r_ptr->aaf = rt_ptr->aaf;
	r_ptr->speed = rt_ptr->speed;
	r_ptr->freq_spell = rt_ptr->freq_spell;
	r_ptr->flags1 = rt_ptr->flags1;
	r_ptr->flags2 = rt_ptr->flags2;
	r_ptr->flags3 = rt_ptr->flags3;
	r_ptr->flags4 = rt_ptr->flags4;
	r_ptr->flags5 = rt_ptr->flags5;
	r_ptr->flags6 = rt_ptr->flags6;
	r_ptr->gfx.da = rt_ptr->gfx.da;
	r_ptr->gfx.dc = rt_ptr->gfx.dc;
	r_ptr->gfx.xa = rt_ptr->gfx.xa;
	r_ptr->gfx.xc = rt_ptr->gfx.xc;
	r_ptr->num_blows = rt_ptr->num_blows;
	C_COPY(r_ptr->blow, rt_ptr->blow, 4, monster_blow);

	/* Remove the RF3_ORC and RF3_TROLL flags if necessary. */
	switch (race_info[ghost_race].grace)
	{
		case RACE_ORC: r_ptr->flags3 &= ~(RF3_TROLL); break;
		case RACE_TROLL: r_ptr->flags3 &= ~(RF3_ORC); break;
		default: r_ptr->flags3 &= ~(RF3_TROLL | RF3_ORC);
	}
}

/*
 * Hack -- Prepare the "ghost" race
 *
 * We are given a "name" of the form "Bob" (or "Bob, the xxx"), and
 * a race/class (by index), and a level (usually the dungeon level),
 * and a special "town" flag (which chooses the major ghost "type").
 *
 * Note that "town" ghosts are always level 1 to 50, and other ghosts
 * are always level 1 to 100 (or deeper?)
 *
 * Currently we save the current "ghost race info" in the savefile.
 * Note that ghosts from pre-2.7.7 savefiles are always ignored.
 *
 * Eventually we should probably save the ghost in such a way as
 * to allow it to be "re-extracted" from a small amount of info,
 * such as the "base name", the "race", the "class", the base "hp",
 * the "level", the "town" flag, and the "random seed".  This would
 * make the savefile impervious to changes in the race format.
 *
 * Thus we would need to save "pn", "hp", "gr", "gc", and "lev",
 * plus the "town" flag, plus a random seed of some form.  Note that
 * we already save the "pn" value, followed by a "comma" and "title",
 * and we have the "lev" field as the actual ghost level.  But it is
 * probably best to ignore this storage method for a few versions.
 *
 * We "could" extract the "hp" from the ghost name and current hp's.
 * We "could" extract the "town" flag from the ghost race symbol.
 *
 * Note that each new ghost needs a new "random seed".  And actually,
 * we do not really need a "full" random seed, we could just use a
 * random value from which random numbers can be extracted.  (?)
 */
static void set_ghost(cptr pname, int hp, int ghost_race, int lev)
{
	char gb_name[32];

	int i;

	monster_race *r_ptr = &r_info[MON_PLAYER_GHOST];

	/* Prepare to copy until the first comma. */
	for (i = 0; pname[i] && pname[i] != ','; i++);

	/* Force a name (if no real name was provided). */
	if (i < 2)
	{
		pname = "Nobody";
		i = strlen(pname);
	}

	/* Avoid overflow. */
	if (i >= (int)sizeof(gb_name)) i = -1+sizeof(gb_name);

	/* Copy the name across. */
	else sprintf(gb_name, "%.*s", i, pname);

	/* Capitalize the name */
	gb_name[0] = TOUPPER(gb_name[0]);

	/* Extract the default experience */
	r_ptr->mexp = lev * 5 + 5;

	/* Hack -- Break up the hitpoints */
	for (i = 1; i * i < hp; i++) ;

	/* Extract the basic hit dice and sides */
	r_ptr->hdice = r_ptr->hside = i;

	/* Prepare the ghost */
	set_ghost_aux(gb_name, ghost_race, lev);
}



/*
 * Places a ghost somewhere.
 */
bool place_ghost(void)
{
	int y, x, hp, grace, dummy[1];

	monster_race *r_ptr = &r_info[MON_PLAYER_GHOST];

	FILE *fp;

	bool b;

	char                name[100];

	/* Hack -- no ghosts on the surface. */
	if (!dun_level) return (FALSE);

	/* Already have a ghost */
	if (r_ptr->cur_num >= r_ptr->max_num)
	{
		if (cheat_wzrd) msg_print("Player-Ghost already present!");
		if (cheat_wzrd) msg_format("cur_num = %d, max_num = %d",
			r_ptr->cur_num, r_ptr->max_num);
		return (FALSE);
	}

	/* Dungeon -- Use Dungeon Level */

	/* Choose and open the bones file */
	fp = my_fopen_path(ANGBAND_DIR_BONE, format("bone.%03d", dun_depth), "r");

	/* No bones file to use */
	if (!fp) return (FALSE);

	/* Scan the file */
	b = (fscanf(fp, "%99[^\n]\n%d\n%d\n%d", name, &hp, &grace, dummy) == 4);


	/* Close the file */
	fclose(fp);

	/* Catch errors */
	if (!b)
	{
		msg_print("Warning -- corrupt 'ghost' file!");
		return (FALSE);
	}

	/* Hack -- pick a nice (far away) location */
	if (!find_space(&y, &x, MAX_SIGHT+5)) return FALSE;

	/* Set up the ghost */
	set_ghost(name, hp, grace, dun_depth);

	/* Try to create the ghost in the dungeon. */
	b = !!place_monster_one(y, x, MON_PLAYER_GHOST, FALSE,FALSE, FALSE);

	/* Wizard mode message */
	if (b && cheat_wzrd) msg_print("WIZARD: Ghost placed");

	/* Tell the caller if a ghost was created. */
	return b;
}


/*
 * Delete a monster by index.
 *
 * When a monster is deleted, all of its objects are deleted.
 */
void delete_monster_idx(int i,bool visibly)
{
	int x, y;

	monster_type *m_ptr = &m_list[i];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	s16b this_o_idx, next_o_idx = 0;


	/* Get location */
	y = m_ptr->fy;
	x = m_ptr->fx;


	/* Hack -- Reduce the racial counter */
	r_ptr->cur_num--;

	/* Hack -- count the number of "reproducers" */
	if (r_ptr->flags2 & (RF2_MULTIPLY)) num_repro--;


	/* Hack -- remove target monster */
	if (i == target_who) target_who = 0;

	/* Hack -- remove tracked monster */
	if (i == health_who) health_track(0);


	/* Monster is gone */
	cave[y][x].m_idx = 0;


	/* A monster may now be able to be generated here. */
	full_grid = MAX(full_grid, distance(y,x,py,px));

	/* Delete objects */
	for (this_o_idx = m_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Acquire object */
		o_ptr = &o_list[this_o_idx];

		/* Acquire next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Hack -- efficiency */
		o_ptr->held_m_idx = 0;

		/* Delete the object */
		delete_dun_object(o_ptr);
	}


	/* Wipe the Monster */
	WIPE(m_ptr, monster_type);

	/* Count monsters */
	m_cnt--;


	/* Visual update */
	if (visibly) lite_spot(y, x);
}


/*
 * Delete the monster, if any, at a given location
 */
void delete_monster(int y, int x)
{
	cave_type *c_ptr;

	/* Paranoia */
	if (!in_bounds(y, x)) return;

	/* Check the grid */
	c_ptr = &cave[y][x];

	/* Delete the monster (if any) */
	if (c_ptr->m_idx) delete_monster_idx(c_ptr->m_idx,TRUE);
}


/*
 * Try to increase the size of m_list to accommodate current requirements.
 * Return FALSE if this failed for one reason or another.
 */
bool grow_m_list(void)
{
#ifdef USE_DYNAMIC_LISTS
	monster_type *new;

	/* This function returns FALSE on out of memory errors, so need not crash. */
	vptr (*old_rpanic_aux)(huge) = rpanic_aux;

	uint new_max = z_info->m_max*2;

	size_t new_size = new_max*sizeof(monster_type);

	/* Can't store m_list's new size. */
	if (new_max <= z_info->m_max || new_max > MAX_SHORT) return FALSE;

	/* Can't request m_list's new size. */
	if (new_size <= sizeof(monster_type) * z_info->m_max) return FALSE;

	/* Failure is safe here. */
	rpanic_aux = rpanic_none;

	/* Try to allcoate the new memory. */
	C_MAKE(new, new_max, monster_type);

	/* Restore rpanic_aux. */
	rpanic_aux = old_rpanic_aux;

	/* Handle success. */
	if (new)
	{
		/* Let cheaters know the level is really full. */
		if (cheat_hear) msg_format("Monster list grown from %u to %u.",
			z_info->m_max, new_max);

		/* Start using the new m_list. */
		C_COPY(new, m_list, z_info->m_max, monster_type);
		FREE(m_list);
		m_list = new;
		z_info->m_max = new_max;
	}

	return (new != NULL);
#else /* USE_DYNAMIC_LISTS */

	/* Not allowed to grow the array. */
	return FALSE;

#endif /* USE_DYNAMIC_LISTS */
}

/*
 * Actually remove monsters during compacting.
 */
static void compact_monsters_purge(int size)
{
	int num, cnt, i;
	int cur_lev, cur_dis, chance;

	assert(size > 0); /* See caller(s). */

	msg_print("Compacting monsters...");

	/* Compact at least 'size' objects */
	for (num = 0, cnt = 1; num < size; cnt++)
	{
		/* Get more vicious each iteration */
		cur_lev = 5 * cnt;

		/* Get closer each iteration */
		cur_dis = 5 * (20 - cnt);

		/* Check all the monsters */
		for (i = 1; i < m_max; i++)
		{
			monster_type *m_ptr = &m_list[i];

			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			/* Paranoia -- skip "dead" monsters */
			if (!m_ptr->r_idx) continue;

			/* Hack -- High level monsters start out "immune" */
			if (r_ptr->level > cur_lev) continue;

			/* Ignore nearby monsters */
			if ((cur_dis > 0) && (m_ptr->cdis < cur_dis)) continue;

			/* Saving throw chance */
			chance = 90;

			/* Try not to compact Unique Monsters */
			if (r_ptr->flags1 & (RF1_UNIQUE)) chance = 99;

			/* Only compact "Quest" Monsters in emergencies */
			if ((r_ptr->flags1 & RF1_GUARDIAN) && (cnt < 1000)) chance = 100;

			/* All monsters get a saving throw */
			if (rand_int(100) < chance) continue;

			/* Delete the monster */
			delete_monster_idx(i,TRUE);

			/* Count the monster */
			num++;
		}
	}
}

/*
 * Move an object from index i1 to index i2 in the object list
 */
static void compact_monsters_aux(int i1, int i2)
{
	int y, x;

	cave_type *c_ptr;

	monster_type *m_ptr;

	s16b this_o_idx, next_o_idx = 0;


	/* Do nothing */
	if (i1 == i2) return;


	/* Old monster */
	m_ptr = &m_list[i1];

	/* Location */
	y = m_ptr->fy;
	x = m_ptr->fx;

	/* Cave grid */
	c_ptr = &cave[y][x];

	/* Update the cave */
	c_ptr->m_idx = i2;

	/* Repair objects being carried by monster */
	for (this_o_idx = m_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Acquire object */
		o_ptr = &o_list[this_o_idx];

		/* Acquire next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Reset monster pointer */
		o_ptr->held_m_idx = i2;
	}

	/* Hack -- Update the target */
	if (target_who == i1) target_who = i2;

	/* Hack -- Update the health bar */
	if (health_who == i1) health_track(i2);

	/* Structure copy */
	COPY(&m_list[i2], &m_list[i1], monster_type);

	/* Wipe the hole */
	WIPE(&m_list[i1], monster_type);
}

/*
 * Remove dead (or purged) monsters from the monster list.
 */
static void compact_monsters_excise(void)
{
	int i;

	/* Excise dead monsters (backwards!) */
	for (i = m_max - 1; i >= 1; i--)
	{
		/* Get the i'th monster */
		monster_type *m_ptr = &m_list[i];

		/* Skip real monsters */
		if (m_ptr->r_idx) continue;

		/* Move last monster into open hole */
		compact_monsters_aux(m_max - 1, i);

		/* Compress "m_max" */
		m_max--;
	}
}

/*
 * Compact and Reorder the monster list
 *
 * This function can be very dangerous, use with caution!
 *
 * When actually "compacting" monsters, we base the saving throw
 * on a combination of monster level, distance from player, and
 * current "desperation".
 *
 * After "compacting" (if needed), we "reorder" the monsters into a more
 * compact order, and we reset the allocation info, and the "live" array.
 */
void compact_monsters(int size)
{
	/* Try to increase the space available. */
	if (size > 0 && !grow_m_list())
	{
		/* Purge monsters if impossible. */
		compact_monsters_purge(size);
	}

	/* Remove dead monsters from the list. */
	compact_monsters_excise();
}

/* Take out non-pets */
void remove_non_pets(void)
{
	int i;
	for(i=m_max-1;i>=0;i--)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Skip pets */
		if (m_ptr->smart & SM_ALLY) continue;

		/* Reduce current live count if monster is alive */
		if (m_ptr->r_idx)
		{
			m_cnt--;

			/* Reduce multiplier count if necessary */
			if(r_ptr->flags2 & RF2_MULTIPLY) num_repro--;

			/* Hack -- Reduce the racial counter */
			r_ptr->cur_num--;
		}

		/* Move last monster into open hole */
		compact_monsters_aux(m_max-1,i);

		/* Compress m_max */
		m_max--;
	}
}

/*
 * Acquires and returns the index of a "free" monster.
 *
 * This routine should almost never fail, but it *can* happen.
 */
s16b m_pop(void)
{
	int i;


	/* Normal allocation */
	if (m_max < MAX_M_IDX)
	{
		/* Access the next hole */
		i = m_max;

		/* Expand the array */
		m_max++;

		/* Count monsters */
		m_cnt++;

		/* Return the index */
		return (i);
	}


	/* Recycle dead monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr;

		/* Acquire monster */
		m_ptr = &m_list[i];

		/* Skip live monsters */
		if (m_ptr->r_idx) continue;

		/* Count monsters */
		m_cnt++;

		/* Use this monster */
		return (i);
	}


	/* Warn the player (except during dungeon creation) */
	if (character_dungeon) msg_print("Too many monsters!");

	/* Try not to crash */
	return (0);
}


/*
 * Prevent town monsters and dungeon monsters from being in the same place.
 */
static bool PURE get_mon_depth(int depth, int r_idx)
{
	return (!depth == !r_info[r_idx].level);
}

/*
 * Prevent dead uniques from being generated.
 */
static bool PURE get_mon_dead(int r_idx)
{
	monster_race *r_ptr = r_info+r_idx;

	/* Non-unique. */
	if (~r_ptr->flags1 & RF1_UNIQUE) return TRUE;

	/* Alive and not yet generated. */
	if (r_ptr->cur_num < r_ptr->max_num) return TRUE;

	/* Not available. */
	return FALSE;
}

/*
 * Prevent HURT_LITE monsters from being generated on the surface during the
 * day.
 */
static bool PURE get_mon_toobright(int UNUSED p, int r_idx)
{
	monster_race *r_ptr = r_info+r_idx;

	/* Not on the surface. */
	if (dun_level) return TRUE;

	/* Not light. */
	if (!daytime_p()) return TRUE;

	/* Doesn't hate light. */
	if (~r_ptr->flags3 & RF3_HURT_LITE) return TRUE;

	/* Not available. */
	return FALSE;
}

/*
 * alloc_race_table[] functions.
 *
 * The following tables should be the only ones which modify the prob2 field
 * of alloc_race_table[].
 *
 * get_mon_num_init() sets it to a default state which it should have after
 * the completion of any given "action".
 * get_mon_num_filter() modifies the existing state by removing monsters
 * from it which are no longer deemed suitable.
 */

/*
 * Remove any special monster generation restrictions.
 *
 * This removes any existing restriction from the monster list, and adds
 * three simple ones:
 *
 * 1. No town monsters are generated outside the towns or other monsters inside.
 * 2. No dead uniques are generated.
 * 3. No monsters with HURT_LITE are generated on the surface during the day.
 */
static void get_mon_num_init(void)
{
	int i;

	/* Scan the allocation table */
	for (i = 0; i < alloc_race_size; i++)
	{
		/* Get the entry */
		alloc_entry *entry = &alloc_race_table[i];

		/* Bad depth. */
		if (!get_mon_depth(dun_depth, entry->index)) entry->prob2 = 0;

		/* Not allowed during the day. */
		else if (!get_mon_toobright(0, entry->index)) entry->prob2 = 0;

		/* Accept the monster. */
		else entry->prob2 = entry->prob1;
	}
}



/*
 * Remove monsters from the "monster allocation table" if they fail an
 * arbitrary filter, which may use the p parameter.
 */
static void get_mon_num_filter(bool (*hook)(int, int), int p)
{
	int i;

	assert(hook); /* Caller. */

	/* Scan the allocation table */
	for (i = 0; i < alloc_race_size; i++)
	{
		/* Get the entry */
		alloc_entry *entry = &alloc_race_table[i];

		/* Reject monsters if they fail the restriction. */
		if (!(*hook)(p, entry->index))
		{
			/* Decline this monster */
			entry->prob2 = 0;
		}
	}
}

/*
 * Initialise the monster with 0 or 1 filter.
 */
void get_mon_num_prep(bool (*hook)(int, int), int p)
{
	get_mon_num_init();

	if (hook)
	{
		get_mon_num_filter(hook, p);
	}
}

/*
 * Choose a monster race that seems "appropriate" to the given level
 *
 * This function uses the "prob2" field of the "monster allocation table",
 * and various local information, to calculate the "prob3" field of the
 * same table, which is then used to choose an "appropriate" monster, in
 * a relatively efficient manner.
 *
 * Note that "town" monsters will *only* be created in the town, and
 * "normal" monsters will *never* be created in the town, unless the
 * "level" is "modified", for example, by polymorph or summoning.
 *
 * There is a small chance (1/50) of "boosting" the given depth by
 * a small amount (up to four levels), except in the town.
 *
 * It is (slightly) more likely to acquire a monster of the given level
 * than one of a lower level.  This is done by choosing several monsters
 * appropriate to the given level and keeping the "hardest" one.
 *
 * Note that if no monsters are "appropriate", then this function will
 * fail, and return zero, but this should *almost* never happen.
 */
s16b get_mon_num(int level)
{
	int i, p;

	long value, total;

	alloc_entry *table = alloc_race_table;


	/* Boost the level */
	if (level > 0)
	{
		/* Maximum boost ranges from 4 at dlvl 1 to 10 at dlvl 12 and above. */
		int d = MIN(level / 4 + 2, 5);

		/* Occasional "nasty" monster */
		if (one_in(NASTY_MON)) level += d;
		if (one_in(NASTY_MON)) level += d;
	}

	/* Process probabilities */
	for (total = i = 0; i < alloc_race_size; i++)
	{
		alloc_entry *entry = table+i;

		/* Monsters are sorted by depth */
		if (entry->level > level) break;

		/* Hack - remove dead/existing uniques here as get_mon_num() can
		 * change get_mon_dead() itself. This only really needs to be updated as
		 * monsters are created and destroyed. */
		entry->prob3 = (get_mon_dead(entry->index)) ? entry->prob2 : 0;

		/* Total */
		total += entry->prob3;
	}

	/* No legal monsters */
	if (!total) return (0);

	/* Power boost */
	p = rand_int(100);
	i = (p < 40) ? 1 : (p < 90) ? 2 : 3;

	/* Roll up to three times for a monster. */
	for (value = 0; i; i--)
	{
		long v = rand_int(total);
		if (v > value) value = v;
	}

	/* Find the monster */
	for (i = 0; i < alloc_race_size; i++)
	{
		/* Decrement */
		value -= table[i].prob3;

		/* Found the entry */
		if (value < 0) return table[i].index;
	}

	/* Paranoia - this should not be possible. */
	return 0;
}



/* Add a string at t (within buf) if there is enough room. */
#define MDA_ADD(X) \
{ \
	if (t-buf+strlen(X)+1 < max) \
	{ \
		strcpy(t, (X)); \
		t += strlen(X); \
	} \
}

/*
 * Describe a name for a monster of a given type of monster, pluralising where
 * asked.
 *
 * num def   indef number string       The table on the left gives the format
 *   1 FALSE FALSE FALSE  "Newt"        of its output.
 *  10 FALSE FALSE FALSE  "Newts"
 *  10 FALSE FALSE TRUE   "10 Newts"
 *   1 FALSE TRUE  -      "a Newt"
 *  10 FALSE TRUE  -      "some Newts"
 *  10 TRUE  -     FALSE  "the Newts"
 *  10 TRUE  -     TRUE   "the 10 Newts"
 */
static void monster_desc_aux(char *buf, uint max, cptr name, int num,
	byte flags)
{
	cptr artstr = "";
	char tmp[20];
	cptr s;
	char *t;
	byte reject = 0;

	if (num == 1) reject |= 1<<MCI_PLURAL;
	else flags |= MDF_MANY;

	/* MDF_INDEF inhibits MDF_NUMBER, but is overriden by MDF_DEF below. */
	if (flags & MDF_INDEF && ~flags & MDF_DEF) flags &= ~(MDF_NUMBER);

	/* None needed. */
	if (!(flags & (MDF_DEF | MDF_INDEF | MDF_NUMBER | MDF_YOUR)))
	{
		reject |= 1<<MCI_ARTICLE;
	}
	else if (flags & MDF_DEF)
	{
		artstr = "the";
	}
	else if (flags & MDF_INDEF && flags & MDF_MANY)
	{
		artstr = "some";
	}
	else if (flags & MDF_INDEF)
	{
		/* CM_ACT | MCI_ARTICLE, to be turned into English later... */
		sprintf(tmp, ".%c", CM_ACT | MCI_ARTICLE);
		artstr = tmp;
	}
	else if (flags & MDF_YOUR)
	{
		artstr = "your";
	}
	if (flags & MDF_NUMBER)
	{
		/* Add a number to the end of the article. */
		sprintf(tmp, "%s%s%d", artstr, (artstr[0]) ? " " : "", num);
		artstr = tmp;
	}

	for (s = name, t = buf; *s && t < buf+max-1; s++)
	{
		if (*s & 0xE0)
		{
			*t++ = *s;
		}
		else if (find_cm(*s) == CM_NORM);
		else if (find_cm(*s) != CM_ACT)
		{
			s = find_next_good_flag(s, reject, ~reject)-1;
		}
		else if (find_ci(*s) == MCI_ARTICLE)
		{
			MDA_ADD(artstr);
		}
	}

	/* Finish off. */
	*t = '\0';

	/* Turn any article strings into normal characters. */
	convert_articles(buf);
}

/*
 * Process the above as a vstrnfmt_aux function.
 *
 * Format:
 * "%v", monster_desc_aux_f3, (monster_race*)r_ptr, (int)num, (byte)flags
 * or:
 * "%.*v", (int)len, monster_desc_aux_f3, r_ptr, num, flags
 * or even:
 * "%v", monster_desc_aux_f3, (cptr)name, (int)num, (byte)flags
 *
 * Note that max is expected to be >= 2*len, which is true for normal values
 * of each.
 */
void monster_desc_aux_f3(char *buf, uint max, cptr fmt, va_list *vp)
{
	/* Extract the arguments. */
	vptr *r_ptr = va_arg(*vp, vptr);
	int num = va_arg(*vp, int);
	uint flags = va_arg(*vp, uint);
	cptr name;

	/* Use a length of MNAME_MAX if unspecified. */
	if (!strchr(fmt, '.'))
	{
		max = MIN(MNAME_MAX, max);
	}

	/*
	 * The first argument is either a monster_race * or a cptr.
	 * The former is easy to spot as it will be a pointer into r_info.
	 * This means that the anything else is assumed to be a cptr.
	 */
	if ((monster_race*)r_ptr >= r_info &&
		(monster_race*)r_ptr < r_info+MAX_R_IDX)
	{
		name = r_name+((monster_race*)(r_ptr))->name;
	}
	else
	{
		name = (cptr)r_ptr;
	}

	/* Create the name now the arguments are known. */
	monster_desc_aux(buf, max, name, num, flags);
}

/*
 * Build a string describing a specific monster in some way.
 *
 * We can correctly describe monsters based on their visibility.
 * We can force all monsters to be treated as visible or invisible.
 * We can build nominatives, objectives, possessives, or reflexives.
 * We can selectively pronominalize hidden, visible, or all monsters.
 * We can use definite or indefinite descriptions for hidden monsters.
 * We can use definite or indefinite descriptions for visible monsters.
 *
 * Pronominalization involves the gender whenever possible and allowed,
 * so that by cleverly requesting pronominalization / visibility, you
 * can get messages like "You hit someone.  She screams in agony!".
 *
 * Reflexives are acquired by requesting Objective plus Possessive.
 *
 * If no m_ptr arg is given (?), the monster is assumed to be hidden,
 * unless the "Assume Visible" mode is requested.
 *
 * If no r_ptr arg is given, it is extracted from m_ptr and r_info
 * If neither m_ptr nor r_ptr is given, the monster is assumed to
 * be neuter, singular, and hidden (unless "Assume Visible" is set),
 * in which case you may be in trouble... :-)
 *
 * I am assuming that no monster name is more than 70 characters long,
 * so that "char desc[80];" is sufficiently large for any result.
 *
 * Mode Flags:
 *   0x01 --> Objective (or Reflexive)
 *   0x02 --> Possessive (or Reflexive)
 *   0x04 --> Use indefinites for hidden monsters ("something")
 *   0x08 --> Use indefinites for visible monsters ("a kobold")
 *   0x10 --> Pronominalize hidden monsters
 *   0x20 --> Pronominalize visible monsters
 *   0x40 --> Assume the monster is hidden
 *   0x80 --> Assume the monster is visible
 *
 * Useful Modes:
 *   0x00 --> Full nominative name ("the kobold") or "it"
 *   0x04 --> Full nominative name ("the kobold") or "something"
 *   0x80 --> Genocide resistance name ("the kobold")
 *   0x88 --> Killing name ("a kobold")
 *   0x22 --> Possessive, genderized if visable ("his") or "its"
 *   0x23 --> Reflexive, genderized if visable ("himself") or "itself"
 */
static void monster_desc(char *buf, monster_type *m_ptr, int mode, int size)
{
	monster_race *r_ptr = (m_ptr) ? &r_info[m_ptr->r_idx] : r_info;

	cptr name, suffix_ = "";

	char        silly_name[80];

	/* Can we "see" it (exists + forced, or visible + not unforced) */
	bool seen = (m_ptr && ((mode & 0x80) || (!(mode & 0x40) && m_ptr->ml)));

	/* Sexed Pronouns (seen and allowed, or unseen and allowed) */
	bool pron = (m_ptr && ((seen) ? (mode & 0x20) : (mode & 0x10)));

	/* First, try using pronouns, or describing hidden monsters */
	if (!seen || pron)
	{
		/* an encoding of the monster "sex" */
		int kind = 0x00;

		/* Extract the gender (if applicable) */
		if (r_ptr->flags1 & (RF1_FEMALE)) kind = 0x20;
		else if (r_ptr->flags1 & (RF1_MALE)) kind = 0x10;

		/* Ignore the gender (if desired) */
		if (!m_ptr || !pron) kind = 0x00;


		/* Brute force: split on the possibilities */
		switch (kind | (mode & 0x07))
		{
			/* Neuter, or unknown */
			case 0x00: name = "it"; break;
			case 0x01: name = "it"; break;
			case 0x02: name = "its"; break;
			case 0x03: name = "itself"; break;
			case 0x04: name = "something"; break;
			case 0x05: name = "something"; break;
			case 0x06: name = "something's"; break;
			case 0x07: name = "itself"; break;

			/* Male (assume human if vague) */
			case 0x10: name = "he"; break;
			case 0x11: name = "him"; break;
			case 0x12: name = "his"; break;
			case 0x13: name = "himself"; break;
			case 0x14: name = "someone"; break;
			case 0x15: name = "someone"; break;
			case 0x16: name = "someone's"; break;
			case 0x17: name = "himself"; break;

			/* Female (assume human if vague) */
			case 0x20: name = "she"; break;
			case 0x21: name = "her"; break;
			case 0x22: name = "her"; break;
			case 0x23: name = "herself"; break;
			case 0x24: name = "someone"; break;
			case 0x25: name = "someone"; break;
			case 0x26: name = "someone's"; break;
			case 0x27: name = "herself"; break;
			default: name = "it";
		}
	}


	/* Handle visible monsters, "reflexive" request */
	else if ((mode & 0x02) && (mode & 0x01))
	{
		/* The monster is visible, so use its gender */
		if (r_ptr->flags1 & (RF1_FEMALE)) name = "herself";
		else if (r_ptr->flags1 & (RF1_MALE)) name = "himself";
		else name = "itself";
	}


	/* Handle all other visible monster requests */
	else
	{
		byte flags = 0;

		/* Are we hallucinating? (Idea from Nethack...) */
		if (p_ptr->image)
		{
			if(randint(2)==1)
			{
				monster_race * hallu_race;
				do {
					hallu_race = &r_info[rand_int(MAX_R_IDX)];
				}
				while (hallu_race->flags1 & RF1_UNIQUE ||
					is_fake_monster(hallu_race));
				name = r_name+hallu_race->name;
			}
			else
			{
				/*
				 * Hack - get_rnd_line() only returns a plain string, so add an
				 * article here. Luckily, monster_desc() only deals with
				 * singular monsters...
				 */
				strnfmt(silly_name, sizeof(silly_name), "%c%c %c%v",
					CM_TRUE | MCI_ARTICLE, CM_ACT | MCI_ARTICLE,
					CM_NORM | MCI_ARTICLE, get_rnd_line_f1, "silly.txt");
				name = silly_name;
			}
		}
		else
		{
			name = r_name+r_ptr->name;
		}

		/* It could be a Unique */
		if ((r_ptr->flags1 & (RF1_UNIQUE)) && !(p_ptr->image))
		{
			/* Start with the name (thus nominative and objective) */
		}

		/* It could be an indefinite monster */
		else if (mode & 0x08)
		{
			/* Indefinite monsters need an indefinite article */
			flags |= MDF_INDEF;
		}

		/* It could be a normal, definite, monster */
		else
		{
			/* Definite monsters need a definite article */
			if (m_ptr->smart & (SM_ALLY))
				flags |= MDF_YOUR;
			else
				flags |= MDF_DEF;
		}

		/* Handle the Possessive as a special afterthought */
		if (mode & 0x02)
		{
			/* XXX Check for trailing "s" */

			/* Simply append "apostrophe" and "s" */
			suffix_ = "'s";
		}

		/* Create the name now the flags are known. */
		strnfmt(buf, size, "%v", monster_desc_aux_f3, name, 1, flags);
		name = buf;
	}
	/* Copy to buf, avoiding overflow. */
	{
		int n = strlen(name);
		int s = strlen(suffix_);

		n = MIN(n, MAX(0, size-s-1));
		s = MIN(s, MAX(0, size-1));

		strnfmt(buf, size, "%.*s%.*s", n, name, s, suffix_);
	}
}

/*
 * Call monster_desc() as a vstrnfmt_aux function.
 *
 * Format:
 * "%v", monster_desc_f2, (monster_type*)m_ptr, (int)mode
 * or:
 * "%.*v", (int)len, monster_desc_aux_f3, m_ptr, mode
 *
 */
void monster_desc_f2(char *buf, uint max, cptr fmt, va_list *vp)
{
	monster_type *m_ptr = va_arg(*vp, monster_type *);
	int mode = va_arg(*vp, int);

	/* Use a length of MNAME_MAX if unspecified. */
	if (!strchr(fmt, '.'))
	{
		max = MIN(MNAME_MAX, max);
	}

	/* Copy the string to buf. */
	monster_desc(buf, m_ptr, mode, max);
}



/*
 * Learn about a monster (by "probing" it)
 */
void lore_do_probe(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Hack -- Memorize some flags */
	r_ptr->r_flags1 = r_ptr->flags1;
	r_ptr->r_flags2 = r_ptr->flags2;
	r_ptr->r_flags3 = r_ptr->flags3;

	/* Update monster recall window */
	if (monster_race_idx == r_idx)
	{
		/* Window stuff */
		p_ptr->window |= (PW_MONSTER);
	}
}


/*
 * Take note that the given monster just dropped some treasure
 *
 * Note that learning the "GOOD"/"GREAT" flags gives information
 * about the treasure (even when the monster is killed for the first
 * time, such as uniques, and the treasure has not been examined yet).
 *
 * This "indirect" method is used to prevent the player from learning
 * exactly how much treasure a monster can drop from observing only
 * a single example of a drop.  This method actually observes how much
 * gold and items are dropped, and remembers that information to be
 * described later by the monster recall code.
 */
void lore_treasure(int r_idx, int num_item, int num_gold)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Note the number of things dropped */
	if (num_item > r_ptr->r_drop_item) r_ptr->r_drop_item = num_item;
	if (num_gold > r_ptr->r_drop_gold) r_ptr->r_drop_gold = num_gold;

	/* Hack -- memorize the good/great flags */
	if (r_ptr->flags1 & (RF1_DROP_GOOD)) r_ptr->r_flags1 |= (RF1_DROP_GOOD);
	if (r_ptr->flags1 & (RF1_DROP_GREAT)) r_ptr->r_flags1 |= (RF1_DROP_GREAT);

	/* Update monster recall window */
	if (monster_race_idx == r_idx)
	{
		/* Window stuff */
		p_ptr->window |= (PW_MONSTER);
	}
}



static void sanity_blast (monster_type * m_ptr, bool necro)
{
	bool happened = FALSE;
	int power = 100;

	if (!necro)
	{
		monster_race * r_ptr = &r_info[m_ptr->r_idx];
		power = (r_ptr->level)+10;

		if (!(r_ptr->flags1 & RF1_UNIQUE))
		{
			if (r_ptr->flags1 & RF1_FRIENDS)
			power /= 2;
		}
		else power *= 2;

		if (!hack_mind)
			return; /* No effect yet, just loaded... */

		if (!(m_ptr->ml))
			return; /* Cannot see it for some reason */

		if (!(r_ptr->flags2 & RF2_ELDRITCH_HORROR))
			return; /* oops */



		if ((m_ptr->smart & SM_ALLY) && (randint(8)!=1))
			return; /* Pet eldritch horrors are safe most of the time */

		if (randint(power)<p_ptr->skill_sav)
		{
			skill_exp(SKILL_SAVE);
			return; /* Save, no adverse effects */
		}
		else
		{
			C_TNEW(m_name, MNAME_MAX, char);

			strnfmt(m_name, MNAME_MAX, "%v", monster_desc_f2, m_ptr, 0);

			if (p_ptr->image)
			{
			/* Something silly happens... */
				msg_format("You behold the %s visage of %s!",
					funny_desc[(randint(MAX_FUNNY))-1], m_name);
				if (randint(3)==1)
				{
					msg_print(funny_comments[randint(MAX_COMMENT)-1]);
					p_ptr->image = (p_ptr->image + randint(r_ptr->level));
				}
				TFREE(m_name);
				return; /* Never mind; we can't see it clearly enough */
			}

			/* Something frightening happens... */
			msg_format("You behold the %s visage of %s!",
				horror_desc[(randint(MAX_HORROR))-1], m_name);

			TFREE(m_name);

			r_ptr->r_flags2 |= RF2_ELDRITCH_HORROR;

			/* Some characters get a second chance to resist. */
			if (percent(p_ptr->resist_eldritch)) return;
		}
	}
	else
	{
		msg_print("Your sanity is shaken by reading the Necronomicon!");
	}
	if (randint(power)<p_ptr->skill_sav) /* Mind blast */
	{
		if (!p_ptr->resist_conf)
		{
			(void)add_flag(TIMED_CONFUSED, rand_int(4) + 4);
		}
		if ((!p_ptr->resist_chaos) && (randint(3)==1))
		{
			(void) add_flag(TIMED_IMAGE, rand_int(250) + 150);
		}
		return;
	}


	if (randint(power)<p_ptr->skill_sav) /* Lose int & wis */
	{
		do_dec_stat (A_INT);
		do_dec_stat (A_WIS);
		return;
	}


	if (randint(power)<p_ptr->skill_sav) /* Brain smash */
	{
				if (!p_ptr->resist_conf)
				{
					(void)add_flag(TIMED_CONFUSED, rand_int(4) + 4);
				}
				if (!p_ptr->free_act)
				{
					(void)add_flag(TIMED_PARALYZED, rand_int(4) + 4);
				}
				while (rand_int(100) > p_ptr->skill_sav)
					(void)do_dec_stat(A_INT);
				while (rand_int(100) > p_ptr->skill_sav)
					(void)do_dec_stat(A_WIS);
				if (!p_ptr->resist_chaos)
				{
					(void) add_flag(TIMED_IMAGE, rand_int(250) + 150);
				}
		return;
	}

	if (randint(power)<p_ptr->skill_sav) /* Permanent lose int & wis */
	{
		if (dec_stat(A_INT, 10, TRUE)) happened = TRUE;
		if (dec_stat(A_WIS, 10, TRUE)) happened = TRUE;
		if (happened)
			msg_print("You feel much less sane than before.");
		return;
	}


	if (randint(power)<p_ptr->skill_sav) /* Amnesia */
	{

		if (lose_all_info())
			msg_print("You forget everything in your utmost terror!");
		return;
	}




	{
		int i = 0, j, badmut[4];

		/* Add mutations the player can gain. */
		badmut[i++] = MUT_MORONIC;
		if (!p_ptr->resist_fear) badmut[i++] = MUT_COWARDICE;
		if (!p_ptr->resist_chaos) badmut[i++] = MUT_HALLU;
		badmut[i++] = MUT_BERS_RAGE;

		/* Remove mutations the player already has. */
		for (j = i-1; j >= 0; j--)
		{
			if (p_has_mutation(badmut[j]))
			{
				if (j < --i) badmut[j] = badmut[i];
			}
		}

		/* The poor bastard already has all possible insanities! */
		if (!i) return;

		/* Choose one at random. */
		i = badmut[rand_int(i)];

		/* Attempt to gain it. */
		gain_chaos_feature(i);

	}

	p_ptr->update |= PU_BONUS;
	handle_stuff();

}


/*
 * This function updates the monster record of the given monster
 *
 * This involves extracting the distance to the player, checking
 * for visibility (natural, infravision, see-invis, telepathy),
 * updating the monster visibility flag, redrawing or erasing the
 * monster when the visibility changes, and taking note of any
 * "visual" features of the monster (cold-blooded, invisible, etc).
 *
 * The only monster fields that are changed here are "cdis" (the
 * distance from the player), "los" (clearly visible to player),
 * and "ml" (visible to the player in any way).
 *
 * There are a few cases where the calling routine knows that the
 * distance from the player to the monster has not changed, and so
 * we have a special parameter "full" to request distance computation.
 * This lets many calls to this function run very quickly.
 *
 * Note that every time a monster moves, we must call this function
 * for that monster, and update distance.  Note that every time the
 * player moves, we must call this function for every monster, and
 * update distance.  Note that every time the player "state" changes
 * in certain ways (including "blindness", "infravision", "telepathy",
 * and "see invisible"), we must call this function for every monster.
 *
 * The routines that actually move the monsters call this routine
 * directly, and the ones that move the player, or notice changes
 * in the player state, call "update_monsters()".
 *
 * Routines that change the "illumination" of grids must also call
 * this function, since the "visibility" of some monsters may be
 * based on the illumination of their grid.
 *
 * Note that this function is called once per monster every time the
 * player moves, so it is important to optimize it for monsters which
 * are far away.  Note the optimization which skips monsters which
 * are far away and were completely invisible last turn.
 *
 * Note the optimized "inline" version of the "distance()" function.
 *
 * Note that only monsters on the current panel can be "visible",
 * and then only if they are (1) in line of sight and illuminated
 * by light or infravision, or (2) nearby and detected by telepathy.
 *
 * The player can choose to be disturbed by several things, including
 * "disturb_move" (monster which is viewable moves in some way), and
 * "disturb_near" (monster which is "easily" viewable moves in some
 * way).  Note that "moves" includes "appears" and "disappears".
 *
 * Note the new "xtra" field which encodes several state flags such
 * as "detected last turn", and "detected this turn", and "currently
 * in line of sight", all of which are used for visibility testing.
 */
void update_mon(int m_idx, bool full)
{
	monster_type *m_ptr = &m_list[m_idx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	/* The current monster location */
	int fy = m_ptr->fy;
	int fx = m_ptr->fx;

	bool old_ml = m_ptr->ml;

	/* Seen at all */
	bool flag = FALSE;

	/* Seen by vision */
	bool easy = FALSE;

	/* Seen by telepathy */
	bool hard = FALSE;

	/* Various extra flags */
	bool do_empty_mind = FALSE;
	bool do_weird_mind = FALSE;
	bool do_invisible = FALSE;
	bool do_cold_blood = FALSE;


	/* Calculate distance */
	if (full)
	{
		int d, dy, dx;

		/* Distance components */
		dy = (py > fy) ? (py - fy) : (fy - py);
		dx = (px > fx) ? (px - fx) : (fx - px);

		/* Approximate distance */
		d = (dy > dx) ? (dy + (dx>>1)) : (dx + (dy>>1));

		/* Save the distance (in a byte) */
		m_ptr->cdis = (d < 255) ? d : 255;
	}


	/* Process "distant" monsters */
	if (m_ptr->cdis > MAX_SIGHT)
	{
		/* Ignore unseen monsters */
		if (!m_ptr->ml) return;

	/* Detected */
	if (m_ptr->mflag & (MFLAG_MARK)) flag = TRUE;
	}


	/* Process "nearby" monsters on the current "panel" */
	else if (panel_contains(fy, fx))
	{
		cave_type *c_ptr = &cave[fy][fx];

		/* Normal line of sight, and player is not blind */
		if ((c_ptr->info & (CAVE_VIEW)) && (!p_ptr->blind))
		{
			/* Use "infravision" */
			if (m_ptr->cdis <= (byte)(p_ptr->see_infra))
			{
				/* Infravision only works on "warm" creatures */
				/* Below, we will need to know that infravision failed */
				if (r_ptr->flags2 & (RF2_COLD_BLOOD)) do_cold_blood = TRUE;

				/* Infravision works */
				if (!do_cold_blood) easy = flag = TRUE;
			}

			/* Use "illumination" */
			if (c_ptr->info & (CAVE_LITE | CAVE_GLOW))
			{
				/* Take note of invisibility */
				if (r_ptr->flags2 & (RF2_INVISIBLE)) do_invisible = TRUE;

				/* Visible, or detectable, monsters get seen */
				if (!do_invisible || p_ptr->see_inv) easy = flag = TRUE;
			}
		}

		/* Telepathy can see all "nearby" monsters with "minds" */
		if (p_ptr->telepathy)
		{
			/* Empty mind, no telepathy */
			if (r_ptr->flags2 & (RF2_EMPTY_MIND))
			{
				do_empty_mind = TRUE;
			}

			/* Weird mind, occasional telepathy */
			else if (r_ptr->flags2 & (RF2_WEIRD_MIND))
			{
				do_weird_mind = TRUE;
				if (rand_int(100) < 10) hard = flag = TRUE;
			}

			/* Normal mind, allow telepathy */
			else
			{
				hard = flag = TRUE;
			}
		}

		/* Apply "detection" spells */
		if (m_ptr->mflag & (MFLAG_MARK)) flag = TRUE;

		/* Hack -- Wizards have "perfect telepathy" */
		if (cheat_wzrd) flag = TRUE;
	}


	/* The monster is now visible */
	if (flag)
	{
		/* It was previously unseen */
		if (!m_ptr->ml)
		{
			/* Mark as visible */
			m_ptr->ml = TRUE;

			/* Draw the monster */
			lite_spot(fy, fx);

			/* Update health bar as needed */
			if (health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);

			/* Update monster list window */
			p_ptr->window |= (PW_VISIBLE);

			/* Hack -- Count "fresh" sightings */
			if (r_ptr->r_sights < MAX_UCHAR) r_ptr->r_sights++;

			/* Disturb on appearance */
			if (disturb_move)
			{   if (disturb_allies || !(m_ptr->smart & (SM_ALLY)))
					disturb(0);
				}
		}

		/* Apply telepathy */
		if (hard)
		{
			/* Hack -- Memorize mental flags */
			if (r_ptr->flags2 & (RF2_SMART)) r_ptr->r_flags2 |= (RF2_SMART);
			if (r_ptr->flags2 & (RF2_STUPID)) r_ptr->r_flags2 |= (RF2_STUPID);
		}

		/* Memorize various observable flags */
		if (do_empty_mind) r_ptr->r_flags2 |= (RF2_EMPTY_MIND);
		if (do_weird_mind) r_ptr->r_flags2 |= (RF2_WEIRD_MIND);
		if (do_cold_blood) r_ptr->r_flags2 |= (RF2_COLD_BLOOD);
		if (do_invisible) r_ptr->r_flags2 |= (RF2_INVISIBLE);
	}

	/* The monster is not visible */
	else
	{
		/* It was previously seen */
		if (m_ptr->ml)
		{
			/* Mark as not visible */
			m_ptr->ml = FALSE;

			/* Erase the monster */
			lite_spot(fy, fx);

			/* Update health bar as needed */
			if (health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);

			/* Update monster list window */
			p_ptr->window |= (PW_VISIBLE);

			/* Disturb on disappearance*/
			if (disturb_move)
			{
				if (disturb_allies || !(m_ptr->smart & (SM_ALLY)))
					disturb(0);
				}
		}
	}


	/* The monster is now easily visible */
	if (easy)
	{

	if (m_ptr->ml != old_ml)
	{
			if (r_ptr->flags2 & RF2_ELDRITCH_HORROR)
			{
				sanity_blast(m_ptr, FALSE);
			}
	}

		/* Change */
		if (!(m_ptr->mflag & (MFLAG_VIEW)))
		{
			/* Mark as easily visible */
			m_ptr->mflag |= (MFLAG_VIEW);

			/* Disturb on appearance */
			if (disturb_near)
			{
				if (disturb_allies || !(m_ptr->smart & (SM_ALLY)))
					disturb(0);
				}

		}
	}

	/* The monster is not easily visible */
	else
	{
		/* Change */
		if (m_ptr->mflag & (MFLAG_VIEW))
		{
			/* Mark as not easily visible */
			m_ptr->mflag &= ~(MFLAG_VIEW);

			/* Disturb on disappearance */
			if (disturb_near)
			{
				if (disturb_allies || !(m_ptr->smart & (SM_ALLY)))
					disturb(0);
			}
		}
	}
}




/*
 * This function simply updates all the (non-dead) monsters (see above).
 */
void update_monsters(bool full)
{
	int          i;

	/* Update each (live) monster */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Update the monster */
		update_mon(i, full);
	}
}




/*
 * Return TRUE if this is a living monster (used in various places).
 */
bool live_monster_p(monster_race *r_ptr)
{
	if (r_ptr->flags3 & (RF3_UNDEAD)) return FALSE;
	if (r_ptr->flags2 & (RF2_PHANTOM)) return FALSE;
	if (r_ptr->flags3 & (RF3_DEMON)) return FALSE;
	if (r_ptr->flags3 & (RF3_CTHULOID)) return FALSE;
	if (r_ptr->flags3 & (RF3_NONLIVING)) return FALSE;
	if (strchr("Egv", r_ptr->gfx.dc)) return FALSE;

	return TRUE;
}

/*
 * Return TRUE if this is a living monster (less restrictive version).
 */
bool live_monster_wide_p(monster_race *r_ptr)
{
	if (r_ptr->flags3 & (RF3_UNDEAD)) return FALSE;
	if (r_ptr->flags2 & (RF2_PHANTOM)) return FALSE;
	if (r_ptr->flags3 & (RF3_NONLIVING)) return FALSE;
	return TRUE;
}

/*
 * Attempt to place a monster of the given race at the given location.
 *
 * To give the player a sporting chance, any monster that appears in
 * line-of-sight and is extremely dangerous can be marked as
 * "FORCE_SLEEP", which will cause them to be placed with low energy,
 * which often (but not always) lets the player move before they do.
 *
 * This routine refuses to place out-of-depth "FORCE_DEPTH" monsters.
 *
 * XXX XXX XXX Use special "here" and "dead" flags for unique monsters,
 * remove old "cur_num" and "max_num" fields.
 *
 * XXX XXX XXX Actually, do something similar for artifacts, to simplify
 * the "preserve" mode, and to make the "what artifacts" flag more useful.
 *
 * If "force" is set, this routine will not prevent any monster from being
 * generated on a legal square. This can break certain reasonable assumptions
 * made elsewhere.
 *
 * This is the only function which may place a monster in the dungeon,
 * except for the savefile loading code.
 */
monster_type *place_monster_one(int y, int x, int r_idx, bool slp, bool charm, bool force)
{
	cave_type *c_ptr;

	monster_type *m_ptr;

	monster_race *r_ptr = &r_info[r_idx];


	/* Verify location */
	if (!in_bounds(y, x)) return NULL;

	/* Require empty space */
	if (!cave_empty_bold(y, x)) return NULL;

	/* Hack -- no creation on glyph of warding */
	if (cave[y][x].feat == FEAT_GLYPH) return NULL;
	if (cave[y][x].feat == FEAT_MINOR_GLYPH) return NULL;

	/* Nor on the Pattern */
	if ((cave[y][x].feat >= FEAT_PATTERN_START)
		&& (cave[y][x].feat <= FEAT_PATTERN_XTRA2))
			return NULL;

	/* Paranoia */
	if (!r_idx) return NULL;

	/* Paranoia */
	if (!r_ptr->name) return NULL;


	/* Hack -- "unique" monsters must be "unique" */
	if ((r_ptr->flags1 & (RF1_UNIQUE)) && (r_ptr->cur_num >= r_ptr->max_num) && !force)
	{
		/* Cannot create */
		return NULL;
	}

	/*
	 * Check quest monsters
	 * Heino Vander Sanden
	 */
	if ((r_ptr->flags1 & RF1_GUARDIAN) && !force)
	{
		quest_type *q_ptr = get_quest();
		if (!q_ptr)
		{
			/* Not a quest level */
			return NULL;
		}
		if (r_idx != q_ptr->r_idx)
		{
			/* Not your turn yet */
			return NULL;
		}

		if (r_ptr->cur_num >= (q_ptr->max_num - q_ptr->cur_num))
		{
			/* Too many already */
			return NULL;
		}
	}


	/* Hack - ghosts give a constant rating boost. */
	if (r_idx == MON_PLAYER_GHOST)
	{
		rating += 10;
	}
	/* In depth normal monsters have no effect on rating. */
	else if (r_ptr->level <= dun_depth)
	{
		/* Note a unique anyway. */
		if (r_ptr->flags1 & (RF1_UNIQUE))
		{
			/* Unique monsters induce message */
			if (cheat_hear) msg_format("Unique (%v).",
				monster_desc_aux_f3, r_ptr, 1, 0);
		}
	}
	/* Deep unique monsters */
	else if (r_ptr->flags1 & (RF1_UNIQUE))
	{
		/* Message for cheaters */
		if (cheat_hear) msg_format("Deep Unique (%v).",
			monster_desc_aux_f3, r_ptr, 1, 0);

		/* Boost rating by twice delta-depth */
		rating += (r_ptr->level - (dun_depth)) * 2;
	}

	/* Deep normal monsters */
	else
	{
		/* Message for cheaters */
		if (cheat_hear) msg_format("Deep Monster (%v).",
			monster_desc_aux_f3, r_ptr, 1, 0);

		/* Boost rating by delta-depth */
		rating += (r_ptr->level - (dun_depth));
	}

	/* Access the location */
	c_ptr = &cave[y][x];

	/* Make a new monster */
	c_ptr->m_idx = m_pop();

	/* Mega-Hack -- catch "failure" */
	if (!c_ptr->m_idx) return NULL;


	/* Get a new monster record */
	m_ptr = &m_list[c_ptr->m_idx];

	/* Save the race */
	m_ptr->r_idx = r_idx;

	/* Place the monster at the location */
	m_ptr->fy = y;
	m_ptr->fx = x;

	/* monster is first generation */
	m_ptr->generation = 1;


	/* No "damage" yet */
	m_ptr->stunned = 0;
	m_ptr->confused = 0;
	m_ptr->monfear = 0;
	m_ptr->pl_cdam = m_ptr->pl_mdam = 0;

		/* Friendly? */

		if (charm){
					m_ptr->smart |= SM_ALLY;
				}

	/* Assume no sleeping */
	m_ptr->csleep = 0;

	/* Enforce sleeping if needed */
	if (slp && r_ptr->sleep)
	{
		int val = r_ptr->sleep;
		m_ptr->csleep = ((val * 2) + randint(val * 10));
	}


	/* Unknown distance */
	m_ptr->cdis = 0;

	/* No flags */
	m_ptr->mflag = 0;

	/* Not visible */
	m_ptr->ml = FALSE;


	/* Assign maximal hitpoints */
	if (r_ptr->flags1 & (RF1_FORCE_MAXHP))
	{
		m_ptr->maxhp = maxroll(r_ptr->hdice, r_ptr->hside);
	}
	else
	{
		m_ptr->maxhp = damroll(r_ptr->hdice, r_ptr->hside);
	}

	/* And start out fully healthy */
	m_ptr->hp = m_ptr->maxhp;

	/* Give uniques the stated speed. */
	if (r_ptr->flags1 & (RF1_UNIQUE))
	{
		/* Extract the monster base speed */
		m_ptr->mspeed = r_ptr->speed;
	}
	/* Hack -- small racial variety for other monsters. */
	else
	{
		/* Allow some small variation per monster */
		int j = TURN_ENERGY/extract_energy[r_ptr->speed];
		j = r_ptr->speed + rand_spread(0, j);
		m_ptr->mspeed = MIN(MAX(j, 0), N_ELEMENTS(extract_energy)-1);
	}



	/* Give a random starting energy (should this be a TURN_ENERGY thing?) */
	m_ptr->energy = rand_int(100)+900;

	/* Force monster to wait for player */
	if (r_ptr->flags1 & (RF1_FORCE_SLEEP))
	{
		/* Monster is still being nice */
		m_ptr->mflag |= (MFLAG_NICE);

		/* Must repair monsters */
		repair_mflag_nice = TRUE;
	}

	/* Prevent the monster from moving on the game turn of creation. */
	m_ptr->mflag |= (MFLAG_BORN);


	/* Update the monster */
	update_mon(c_ptr->m_idx, TRUE);


	/* Hack -- Count the monsters on the level */
	r_ptr->cur_num++;


	/* Hack -- Count the number of "reproducers" */
	if (r_ptr->flags2 & (RF2_MULTIPLY)) num_repro++;


	/* Hack -- Notice new multi-hued monsters */
	if (r_ptr->flags1 & (RF1_ATTR_MULTI)) shimmer_monsters = TRUE;


	/* Success */
	return m_ptr;
}


/*
 * Maximum size of a group of monsters
 */
#define GROUP_MAX 32


/*
 * Attempt to place a "group" of monsters around the given location
 */
static bool place_monster_group(int y, int x, int r_idx, bool slp, bool charm)
{
	monster_race *r_ptr = &r_info[r_idx];

	int old, n, i;
	int total = 0, extra = 0;

	int hack_n = 0;

	byte hack_y[GROUP_MAX];
	byte hack_x[GROUP_MAX];


	/* Pick a group size */
	total = randint(13);

	/* Hard monsters, small groups */
	if (r_ptr->level > (dun_depth))
	{
		extra = r_ptr->level - (dun_depth);
		extra = 0 - randint(extra);
	}

	/* Easy monsters, large groups */
	else if (r_ptr->level < (dun_depth))
	{
		extra = (dun_depth) - r_ptr->level;
		extra = randint(extra);
	}

	/* Hack -- limit group reduction */
	if (extra > 12) extra = 12;

	/* Modify the group size */
	total += extra;

	/* Minimum size */
	if (total < 1) total = 1;

	/* Maximum size */
	if (total > GROUP_MAX) total = GROUP_MAX;


	/* Save the rating */
	old = rating;

	/* Start on the monster */
	hack_n = 1;
	hack_x[0] = x;
	hack_y[0] = y;

	/* Puddle monsters, breadth first, up to total */
	for (n = 0; (n < hack_n) && (hack_n < total); n++)
	{
		/* Grab the location */
		int hx = hack_x[n];
		int hy = hack_y[n];

		/* Check each direction, up to total */
		for (i = 0; (i < 8) && (hack_n < total); i++)
		{
			int mx = hx + ddx_ddd[i];
			int my = hy + ddy_ddd[i];

			/* Walls and Monsters block flow */
			if (!cave_empty_bold(my, mx)) continue;

			/* Attempt to place another monster */
			if (place_monster_one(my, mx, r_idx, slp, charm, FALSE))
			{
				/* Add it to the "hack" set */
				hack_y[hack_n] = my;
				hack_x[hack_n] = mx;
				hack_n++;
			}
		}
	}

	/* Hack -- restore the rating */
	rating = old;


	/* Success */
	return (TRUE);
}


/*
 * Hack -- help pick an escort type
 */
static bool place_monster_okay(int place_monster_idx, int r_idx)
{
	monster_race *r_ptr = &r_info[place_monster_idx];

	monster_race *z_ptr = &r_info[r_idx];

	/* Require similar "race" */
	if (z_ptr->gfx.dc != r_ptr->gfx.dc) return (FALSE);

	/* Skip more advanced monsters */
	if (z_ptr->level > r_ptr->level) return (FALSE);

	/* Skip unique monsters */
	if (z_ptr->flags1 & (RF1_UNIQUE)) return (FALSE);

	/* Paranoia -- Skip identical monsters */
	if (place_monster_idx == r_idx) return (FALSE);

	/* Okay */
	return (TRUE);
}


/*
 * Attempt to place a monster of the given race at the given location
 *
 * Note that certain monsters are now marked as requiring "friends".
 * These monsters, if successfully placed, and if the "grp" parameter
 * is TRUE, will be surrounded by a "group" of identical monsters.
 *
 * Note that certain monsters are now marked as requiring an "escort",
 * which is a collection of monsters with similar "race" but lower level.
 *
 * Some monsters induce a fake "group" flag on their escorts.
 *
 * Note the "bizarre" use of non-recursion to prevent annoying output
 * when running a code profiler.
 *
 * Note the use of the new "monster allocation table" code to restrict
 * the "get_mon_num()" function to "legal" escort types.
 */
monster_type *place_monster_aux(int y, int x, int r_idx, bool slp, bool grp, bool charm, bool force)
{
	int i;
	monster_type *m_ptr;
	monster_race *r_ptr = &r_info[r_idx];


	/* Place one monster, or fail */
	m_ptr = place_monster_one(y, x, r_idx, slp, charm, force);
	if (!m_ptr) return NULL;

	/* Escorts for certain monsters */
	if (r_ptr->flags1 & (RF1_ESCORT))
	{
		/* Try to place several "escorts" */
		for (i = 0; i < 50; i++)
		{
			int nx, ny, z, d = 3;

			/* Pick a location */
			if (!scatter(&ny, &nx, y, x, d, cave_empty_bold_p)) break;


			/* Prepare allocation table for the escort. */
			get_mon_num_prep(place_monster_okay, r_idx);


			/* Pick a random race */
			z = get_mon_num(r_ptr->level);


			/* Prepare normal allocation table */
			get_mon_num_prep(NULL, 0);


			/* Handle failure */
			if (!z) break;

			/* Place a single escort */
				(void)place_monster_one(ny, nx, z, slp, charm, FALSE);

			/* Place a "group" of escorts if needed */
			if ((r_info[z].flags1 & (RF1_FRIENDS)) ||
				(r_ptr->flags1 & (RF1_ESCORTS)))
			{
				/* Place a group of monsters */
				(void)place_monster_group(ny, nx, z, slp, charm);
			}
		}
	}

	/* Require the "group" flag */
	if (!grp) return m_ptr;


	/* Friends for certain monsters */
	if (r_ptr->flags1 & (RF1_FRIENDS))
	{
		/* Attempt to place a group */
		(void)place_monster_group(y, x, r_idx, slp, charm);
	}

	/* Success */
	return m_ptr;
}


/*
 * Hack -- attempt to place a monster at the given location
 *
 * Attempt to find a monster appropriate to the "level"
 */
bool place_monster(int y, int x, int level, bool slp, bool grp)
{
	int r_idx;

	/* Pick a monster */
	r_idx = get_mon_num(level);

	/* Handle failure */
	if (!r_idx) return (FALSE);

	/* Attempt to place the monster */
	if (place_monster_aux(y, x, r_idx, slp, grp, FALSE, FALSE)) return (TRUE);

	/* Oops */
	return (FALSE);
}


/*
 * Put Quest monster in dungeon
 * Heino Vander Sanden
 */
bool put_quest_monster(int r_idx)
{
	int y, x;

	/*
	 * Safety check to make sure it is allowed
	 * This is really just paranoia, but it means that if a unique is
	 * somehow killed before its time then it is resurrected rather than
	 * forcing an infinite loop
	 */
	if(r_info[r_idx].max_num == 0)
	{
		r_info[r_idx].max_num++;
		msg_print("Resurrecting guardian to fix corrupted savefile...");
	}

	/* Place the monster somewhere, or fail. */
	return (find_space(&y, &x, 15) &&
		place_monster_aux(y, x, r_idx, 0,0,0, FALSE));
}

#ifdef MONSTER_HORDES
/*
 * Return TRUE if a horde of a monster can be created.
 */
static bool PURE get_mon_horde(int UNUSED p, int r_idx)
{
	u32b f1 = r_info[r_idx].flags1;

	/* Don't create a horde of Smeagol. */
	if (f1 & RF1_UNIQUE) return FALSE;

	/* Don't create a horde of escorted monsters. */
	if (f1 & RF1_ESCORTS) return FALSE;

	/* Good enough. */
	return TRUE;
}

/*
 * Create a horde of monsters.
 * Return FALSE if no monsters were generated.
 */
static bool alloc_horde_aux(int y, int x, int level)
{
	monster_type *m_ptr;
	int i, r_idx;

	get_mon_num_filter(get_mon_horde, 0);

	/* Pick a monster */
	r_idx = get_mon_num(level);

	/* Couldn't find one. */
	if (!r_idx) return (FALSE);

	/* Attempt to place the monster */
	m_ptr = place_monster_aux(y, x, r_idx, FALSE, FALSE, FALSE, FALSE);

	if (m_ptr)
	{
		for (i = rand_range(6, 15); i; i--)
		{
			summon_specific(m_ptr->fy, m_ptr->fx, level,
				SUMMON_CHAR(r_info[r_idx].gfx.dc) | SUMMON_NO_UNIQUES);
		}

		return TRUE;
	}

	/* Couldn't find a place. */
	return FALSE;
}

/*
 * Create a horde and tidy up afterwards.
 */
bool alloc_horde(int y, int x, int level)
{
	bool rc = alloc_horde_aux(y, x, level);

	get_mon_num_init();

	return rc;
}
#endif

/* Forward declare. */
static bool summon_specific_okay(int summon_specific_type, int r_idx);

/*
 * Attempt to allocate a random monster in the dungeon.
 *
 * Place the monster at least "dis" distance from the player.
 *
 * Use "slp" to choose the initial "sleep" status
 *
 * Use "level" for the monster level
 */
static bool alloc_monster_aux(int dis, int level, bool slp, int bias)
{
	bool rc;
	int x, y;

	/* If we already know the grid is full, don't try anything */
	if (full_grid <= dis) return FALSE;

	/* Find a legal, distant, unoccupied, space */
	if (!find_space(&y, &x, dis)) return FALSE;

	/* Prepare allocation table ("okay" monster) */
	if (bias) get_mon_num_prep(summon_specific_okay, bias);

#ifdef MONSTER_HORDES
	if (rand_int(5000) < level)
	{
		/* Hordes never sleep. (?) */
		rc = alloc_horde_aux(y, x, level);

		/* Feedbaok. */
		if (rc && cheat_hear) msg_print("Monster horde.");
	}
	else
#endif /* MONSTER_HORDES */
	{
		/* Place an appropriate monster (or group). */
		rc = place_monster(y, x, level, slp, TRUE);
	}

	/* Restore the normal monster table. */
	if (bias) get_mon_num_prep(NULL, 0);

	/* Nope */
	return rc;
}

/*
 * Attempt to allocate a random monster in the dungeon.
 * Try to use the current dungeon bias 40% of the time. Otherwise, or if that
 * attempt fails, use the full monster list.
 */
void alloc_monster(int dis, int level, bool slp)
{
	if (dun_bias && percent(40))
	{
		if (alloc_monster_aux(dis, level+5, slp, dun_bias)) return;
	}

	alloc_monster_aux(dis, level, slp, 0);
}



/* Remove the flags from a SUMMON_* value. */
#define UNFLAG(X) \
	(X & ~(SUMMON_NO_UNIQUES))

/*
 * Hack -- help decide if a monster race is "okay" to summon
 */
static bool summon_specific_okay(int summon_specific_type, int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Explicitly forbid uniques. */
	if ((summon_specific_type & SUMMON_NO_UNIQUES) &&
		(r_ptr->flags1 & RF1_UNIQUE)) return (FALSE);

	/* Hack - summon by symbol. */
	if (summon_specific_type & SUMMON_BY_CHAR)
	{
		return (r_ptr->gfx.dc == (summon_specific_type & 0x00FF));
	}
	else if (summon_specific_type & SUMMON_BY_FLAG)
	{
		u32b *rflag, flag = 1L << (summon_specific_type % 32);
		int set = (summon_specific_type / 32) % 8;
		switch (set)
		{
			case 1: rflag = &(r_ptr->flags1); break;
			case 2: rflag = &(r_ptr->flags2); break;
			case 3: rflag = &(r_ptr->flags3); break;
			default:
			{
				/* Paranoia - no other summon types are possible. */
				if (alert_failure)
					msg_format("Strange summon type %lx", summon_specific_type);
				return FALSE;
			}
		}

		return !!(*rflag & flag);
	}

	/* Check our requirements */
	else switch (UNFLAG(summon_specific_type))
	{
		case UNFLAG(SUMMON_HOUND):
		{
			return ((r_ptr->gfx.dc == 'C') || (r_ptr->gfx.dc == 'Z'));
		}

		case UNFLAG(SUMMON_HI_UNDEAD):
		{
			return !!strchr("LVW", r_ptr->gfx.dc);
		}

		case UNFLAG(SUMMON_ANIMAL_RANGER):
		{
			return ((r_ptr->flags3 & (RF3_ANIMAL)) &&
				(strchr("abcflqrwBCIJKMRS", r_ptr->gfx.dc)) &&
				!(r_ptr->flags3 & (RF3_DRAGON))&&
				!(r_ptr->flags3 & (RF3_EVIL)) &&
				!(r_ptr->flags3 & (RF3_UNDEAD))&&
				!(r_ptr->flags3 & (RF3_DEMON)) &&
				!(r_ptr->flags3 & (RF3_CTHULOID)) &&
				!(r_ptr->flags4 || r_ptr->flags5 || r_ptr->flags6));
		}

		case UNFLAG(SUMMON_REAVER):
		{
			return (r_idx == MON_BLACK_REAVER);
		}

		case UNFLAG(SUMMON_LIVING):
		{
			return live_monster_p(r_ptr);
		}
		case 0: /* No restrictions. */
		{
			return TRUE;
		}
		default: /* An unknown restriction. */
		{
			if (alert_failure)
			{
				msg_format("Odd summon type %d requested!",
					summon_specific_type);
			}
			return FALSE;
		}
	}
}

/*
 * Return TRUE if a square is suitable for monster generation.
 */
static bool PURE cave_summon_bold_p(int y, int x)
{
	int feat = cave[y][x].feat;

	if (!cave_empty_bold(y, x)) return FALSE;

	if (feat == FEAT_GLYPH || feat == FEAT_MINOR_GLYPH) return FALSE;

	if (feat >= FEAT_PATTERN_START && feat <= FEAT_PATTERN_XTRA2) return FALSE;

	return TRUE;
}


/*
 * Place a monster (of the specified "type") near the given
 * location.  Return TRUE iff a monster was actually summoned.
 *
 * We will attempt to place the monster up to 10 times before giving up.
 *
 * Note: SUMMON_UNIQUE and SUMMON_GOO (XXX) will summon Unique's
 * Note: SUMMON_HI_UNDEAD and SUMMON_HI_DRAGON may summon Unique's
 * Note: None of the other summon codes will ever summon Unique's.
 *
 * This function has been changed.  We now take the "monster level"
 * of the summoning monster as a parameter, and use that, along with
 * the current dungeon level, to help determine the level of the
 * desired monster.  Note that this is an upper bound, and also
 * tends to "prefer" monsters of that level.  Currently, we use
 * the average of the dungeon and monster levels, and then add
 * five to allow slight increases in monster power.
 *
 * Note that we use the new "monster allocation table" creation code
 * to restrict the "get_mon_num()" function to the set of "legal"
 * monsters, making this function much faster and more reliable.
 *
 * Note that this function may not succeed, though this is very rare.
 */
bool summon_specific_aux(int y1, int x1, int lev, int type, bool Group_ok, bool charm)
{
	int x, y, r_idx;

	/* Hack - disallow friendly uniques. */
	if (charm) type |= SUMMON_NO_UNIQUES;

	if (!scatter(&y, &x, y1, x1, 1, cave_summon_bold_p) &&
		!scatter(&y, &x, y1, x1, 2, cave_summon_bold_p))
	{
		return FALSE;
	}

	if (type)
	{
		/* Prepare allocation table ("okay" monster) */
		get_mon_num_prep(summon_specific_okay, type);
	}


	/* Pick a monster, using the level calculation */
	r_idx = get_mon_num(((dun_depth) + lev) / 2 + 5);

	/* Prepare normal allocation table */
	get_mon_num_prep(NULL, 0);


	/* Handle failure */
	if (!r_idx) return (FALSE);


	/* Attempt to place the monster (awake, allow groups) */
	return !!place_monster_aux(y, x, r_idx, FALSE, Group_ok, charm, FALSE);
}

/*
 * Two wrappers around the above.
 * The first is used for hostile summoning, and always allows groups to be
 * created.
 */
bool summon_specific(int y1, int x1, int lev, int type)
{
	return summon_specific_aux(y1, x1, lev, type, TRUE, FALSE);
}

/*
 * The second is used for non-hostile summoning. It takes a Group_ok option,
 * and never allows uniques to be summoned.
 */
bool summon_specific_friendly(int y1, int x1, int lev, int type, bool Group_ok)
{
	return summon_specific_aux(y1, x1, lev, type, Group_ok, TRUE);
}




/*
 * Let the given monster attempt to reproduce.
 *
 * Note that "reproduction" REQUIRES empty space.
 */
bool multiply_monster(monster_type *m_ptr, bool charm, bool clone)
{
	monster_type *m2_ptr;

	int y, x;

	if (!scatter(&y, &x, m_ptr->fy, m_ptr->fx, 1, cave_empty_bold_p))
		return FALSE;

	/* Create a new monster (awake, no groups) */
	m2_ptr = place_monster_aux(y, x, m_ptr->r_idx, FALSE, FALSE, charm, FALSE);

	/* Nothing was created. */
	if (!m2_ptr) return FALSE;

	if (clone) m2_ptr->smart |= SM_CLONED;

	/* Both resulting monsters are next generation */
	m_ptr->generation++;
	m2_ptr->generation = m_ptr->generation;

	/* Result */
	return TRUE;
}




static cptr pain_races[4] = {"jmvQ", "CZ", "FIKMRSXabclqrst", NULL};

static int pain_states[7] = {10, 20, 35, 50, 75, 95, 100};

static cptr pain_str[4][7] =
{
	{"jerks limply" , "writhes in agony", "writhes about", "quivers in pain",
		"squelches", "flinches", "barely notices"},
	{"yelps feebly" , "writhes in agony", "howls in agony", "howls in pain",
		"yelps in pain", "snarls with pain", "shrugs off the attack"},
	{"cries out feebly", "writhes in agony", "shrieks in agony",
		"shrieks in pain", "squeals in pain", "grunts with pain",
		"ignores the attack"},
	{"cries out feebly", "writhes in agony", "screams in agony",
		"screams in pain", "cries out in pain", "grunts with pain",
		"shrugs off the attack"},
};

/*
 * Dump a message describing a monster's reaction to damage
 *
 * Technically should attempt to treat "Beholder"'s as jelly's
 */
void message_pain(monster_type *m_ptr, int dam)
{
	cptr str;

	/* Notice non-damage */
	if (dam == 0) 
	{
		str = "is unharmed";
	}
	/* Obtain a string based on the percentage of HP just lost. */
	else
	{
		long newhp = (long)(m_ptr->hp);
		long oldhp = newhp + (long)(dam);
		long tmp = (newhp * 100L) / oldhp;
		int percentage = (int)(tmp);
		uint race, pain;
		char d_char = r_info[m_ptr->r_idx].gfx.dc;

		/* Find the message set by race, the last being a default set. */
		for (race = 0; race < N_ELEMENTS(pain_races)-1; race++)
			if (strchr(pain_races[race], d_char)) break;

		/* percentage <= 100 anyway, but... */
		for (pain = 0; pain < N_ELEMENTS(pain_states)-1; pain++)
			if (percentage <= pain_states[pain]) break;

		str = pain_str[race][pain];
	}

	/* Print the message. */
	msg_format("%^v %s.", monster_desc_f2, m_ptr, 0, str);

}



/*
 * Learn about an "observed" resistance.
 */
void update_smart_learn(monster_type *m_ptr, int what)
{

#ifdef DRS_SMART_OPTIONS

	monster_race *r_ptr = &r_info[m_ptr->r_idx];


	/* Not allowed to learn */
	if (!smart_learn) return;

	/* Too stupid to learn anything */
	if (r_ptr->flags2 & (RF2_STUPID)) return;

	/* Not intelligent, only learn sometimes */
	if (!(r_ptr->flags2 & (RF2_SMART)) && (rand_int(100) < 50)) return;


	/* XXX XXX XXX */

	/* Analyze the knowledge */
	switch (what)
	{
		case DRS_ACID:
		if (p_ptr->resist_acid) m_ptr->smart |= (SM_RES_ACID);
		if (p_ptr->oppose_acid) m_ptr->smart |= (SM_OPP_ACID);
		if (p_ptr->immune_acid) m_ptr->smart |= (SM_IMM_ACID);
		break;

		case DRS_ELEC:
		if (p_ptr->resist_elec) m_ptr->smart |= (SM_RES_ELEC);
		if (p_ptr->oppose_elec) m_ptr->smart |= (SM_OPP_ELEC);
		if (p_ptr->immune_elec) m_ptr->smart |= (SM_IMM_ELEC);
		break;

		case DRS_FIRE:
		if (p_ptr->resist_fire) m_ptr->smart |= (SM_RES_FIRE);
		if (p_ptr->oppose_fire) m_ptr->smart |= (SM_OPP_FIRE);
		if (p_ptr->immune_fire) m_ptr->smart |= (SM_IMM_FIRE);
		break;

		case DRS_COLD:
		if (p_ptr->resist_cold) m_ptr->smart |= (SM_RES_COLD);
		if (p_ptr->oppose_cold) m_ptr->smart |= (SM_OPP_COLD);
		if (p_ptr->immune_cold) m_ptr->smart |= (SM_IMM_COLD);
		break;

		case DRS_POIS:
		if (p_ptr->resist_pois) m_ptr->smart |= (SM_RES_POIS);
		if (p_ptr->oppose_pois) m_ptr->smart |= (SM_OPP_POIS);
		break;


		case DRS_NETH:
		if (p_ptr->resist_neth) m_ptr->smart |= (SM_RES_NETH);
		break;

		case DRS_LITE:
		if (p_ptr->resist_lite) m_ptr->smart |= (SM_RES_LITE);
		break;

		case DRS_DARK:
		if (p_ptr->resist_dark) m_ptr->smart |= (SM_RES_DARK);
		break;

		case DRS_FEAR:
		if (p_ptr->resist_fear) m_ptr->smart |= (SM_RES_FEAR);
		break;

		case DRS_CONF:
		if (p_ptr->resist_conf) m_ptr->smart |= (SM_RES_CONF);
		break;

		case DRS_CHAOS:
		if (p_ptr->resist_chaos) m_ptr->smart |= (SM_RES_CHAOS);
		break;

		case DRS_DISEN:
		if (p_ptr->resist_disen) m_ptr->smart |= (SM_RES_DISEN);
		break;

		case DRS_BLIND:
		if (p_ptr->resist_blind) m_ptr->smart |= (SM_RES_BLIND);
		break;

		case DRS_NEXUS:
		if (p_ptr->resist_nexus) m_ptr->smart |= (SM_RES_NEXUS);
		break;

		case DRS_SOUND:
		if (p_ptr->resist_sound) m_ptr->smart |= (SM_RES_SOUND);
		break;

		case DRS_SHARD:
		if (p_ptr->resist_shard) m_ptr->smart |= (SM_RES_SHARD);
		break;


		case DRS_FREE:
		if (p_ptr->free_act) m_ptr->smart |= (SM_IMM_FREE);
		break;

		case DRS_MANA:
		if (!p_ptr->msp) m_ptr->smart |= (SM_IMM_MANA);
		break;

		case DRS_REFLECT:
		if (p_ptr->reflect) m_ptr-> smart |= (SM_IMM_REFLECT);
	}

#endif /* DRS_SMART_OPTIONS */

}


