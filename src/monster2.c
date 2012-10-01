/* File: monster2.c */

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

static bool summoning_specific = FALSE;

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

/* Obsolete function */
int get_wilderness_flag(void)
{
	/*int x = p_ptr->wilderness_x;
	int y = p_ptr->wilderness_y;

	if (dun_level)
		return (RF8_DUNGEON);
	else
                return (1L << wf_info[wild_map[y][x].feat].terrain_idx);*/
}


/*
 * Delete a monster by index.
 *
 * When a monster is deleted, all of its objects are deleted.
 */
void delete_monster_idx(int i)
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
        r_ptr->on_saved = FALSE;

	/* Hack -- count the number of "reproducers" */
	if (r_ptr->flags2 & (RF2_MULTIPLY)) num_repro--;


	/* Hack -- remove target monster */
	if (i == target_who) target_who = 0;

	/* Hack -- remove tracked monster */
	if (i == health_who) health_track(0);


	/* Monster is gone */
	cave[y][x].m_idx = 0;


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
		delete_object_idx(this_o_idx);
	}


	/* Wipe the Monster */
	WIPE(m_ptr, monster_type);

	/* Count monsters */
	m_cnt--;


	/* Visual update */
	lite_spot(y, x);
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
	if (c_ptr->m_idx) delete_monster_idx(c_ptr->m_idx);
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
	int		i, num, cnt;
	int		cur_lev, cur_dis, chance;


	/* Message (only if compacting) */
	if (size) msg_print("Compacting monsters...");


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

			/* Only compact "Quest" Monsters in emergencies */
			if ((r_ptr->flags1 & (RF1_QUESTOR)) && (cnt < 1000)) chance = 100;

			/* Try not to compact Unique Monsters */
			if (r_ptr->flags1 & (RF1_UNIQUE)) chance = 99;

			/* All monsters get a saving throw */
			if (rand_int(100) < chance) continue;

			/* Delete the monster */
			delete_monster_idx(i);

			/* Count the monster */
			num++;
		}
	}


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
 * Delete/Remove all the monsters when the player leaves the level
 *
 * This is an efficient method of simulating multiple calls to the
 * "delete_monster()" function, with no visual effects.
 */
void wipe_m_list(void)
{
	int i;

	/* Delete all the monsters */
	for (i = m_max - 1; i >= 1; i--)
	{
		monster_type *m_ptr = &m_list[i];

		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Mega-Hack -- preserve Unique's XXX XXX XXX */

                /* Hack -- Reduce the racial counter */
                r_ptr->cur_num--;

		/* Monster is gone */
		cave[m_ptr->fy][m_ptr->fx].m_idx = 0;

		/* Wipe the Monster */
		WIPE(m_ptr, monster_type);
	}

	/* Reset "m_max" */
	m_max = 1;

	/* Reset "m_cnt" */
	m_cnt = 0;

	/* Hack -- reset "reproducer" count */
	num_repro = 0;

	/* Hack -- no more target */
	target_who = 0;

	/* Hack -- no more tracking */
	health_track(0);
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
	if (m_max < max_m_idx)
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
 * Apply a "monster restriction function" to the "monster allocation table"
 */
errr get_mon_num_prep(void)
{
	int i;

	/* Scan the allocation table */
	for (i = 0; i < alloc_race_size; i++)
	{
		/* Get the entry */
		alloc_entry *entry = &alloc_race_table[i];

		/* Accept monsters which pass the restriction, if any */
		if ((!get_mon_num_hook || (*get_mon_num_hook)(entry->index)) &&
			(!get_mon_num2_hook || (*get_mon_num2_hook)(entry->index)))
		{
			/* Accept this monster */
			entry->prob2 = entry->prob1;
		}

		/* Do not use this monster */
		else
		{
			/* Decline this monster */
			entry->prob2 = 0;
		}
	}

	/* Success */
	return (0);
}

/*
 * Some dungeon types restrict the possible monsters.
 * Return TRUE is the monster is OK and FALSE otherwise
 */
bool restrict_monster_to_dungeon(monster_race *r_ptr)
{
        dungeon_info_type *d_ptr = &d_info[dungeon_type];
        byte a;

        /* Some percent of monsters are alloways accepted */
        if (magik(100 - d_ptr->special_percent)) return TRUE;

        if(d_ptr->mode == DUNGEON_MODE_AND)
        {
                if (d_ptr->mflags1)
		{
                        if(!(d_ptr->mflags1 & r_ptr->flags1))
                                return FALSE;
		}
                if (d_ptr->mflags2)
		{
                        if(!(d_ptr->mflags2 & r_ptr->flags2))
                                return FALSE;
		}
                if (d_ptr->mflags3)
		{
                        if(!(d_ptr->mflags3 & r_ptr->flags3))
                                return FALSE;
		}
                if (d_ptr->mflags4)
		{
                        if(!(d_ptr->mflags4 & r_ptr->flags4))
                                return FALSE;
		}
                if (d_ptr->mflags5)
		{
                        if(!(d_ptr->mflags5 & r_ptr->flags5))
                                return FALSE;
		}
                if (d_ptr->mflags6)
		{
                        if(!(d_ptr->mflags6 & r_ptr->flags6))
                                return FALSE;
		}
                if (d_ptr->mflags7)
		{
                        if(!(d_ptr->mflags7 & r_ptr->flags7))
                                return FALSE;
		}
                if (d_ptr->mflags8)
		{
                        if(!(d_ptr->mflags8 & r_ptr->flags8))
                                return FALSE;
		}
                if (d_ptr->mflags9)
		{
                        if(!(d_ptr->mflags9 & r_ptr->flags9))
                                return FALSE;
		}
                for(a = 0; a < 5; a++)
                        if(d_ptr->r_char[a] && (d_ptr->r_char[a] != r_ptr->d_char)) return FALSE;
        }
        else if(d_ptr->mode == DUNGEON_MODE_NAND)
        {
                byte ok[9 + 5], i = 0, j = 0;

                if (d_ptr->mflags1)
		{
                        i++;
                        if(d_ptr->mflags1 & r_ptr->flags1)
                                ok[0] = 1;
		}
                if (d_ptr->mflags2)
		{
                        i++;
                        if(d_ptr->mflags2 & r_ptr->flags2)
                                ok[1] = 1;
		}
                if (d_ptr->mflags3)
		{
                        i++;
                        if(d_ptr->mflags3 & r_ptr->flags3)
                                ok[2] = 1;
		}
                if (d_ptr->mflags4)
		{
                        i++;
                        if(d_ptr->mflags4 & r_ptr->flags4)
                                ok[3] = 1;
		}
                if (d_ptr->mflags5)
		{
                        i++;
                        if(d_ptr->mflags5 & r_ptr->flags5)
                                ok[4] = 1;
		}
                if (d_ptr->mflags6)
		{
                        i++;
                        if(d_ptr->mflags6 & r_ptr->flags6)
                                ok[5] = 1;
		}
                if (d_ptr->mflags7)
		{
                        i++;
                        if(d_ptr->mflags7 & r_ptr->flags7)
                                ok[6] = 1;
		}
                if (d_ptr->mflags8)
		{
                        i++;
                        if(d_ptr->mflags8 & r_ptr->flags8)
                                ok[7] = 1;
		}
                if (d_ptr->mflags9)
		{
                        i++;
                        if(d_ptr->mflags9 & r_ptr->flags9)
                                ok[8] = 1;
		}

                for(a = 0; a < 5; a++)
                {
                        if(d_ptr->r_char[a])
                        {
                                i++;
                                if (d_ptr->r_char[a] != r_ptr->d_char) ok[9 + a] = 1;
                        }
                }

                j = ok[0] + ok[1] + ok[2] + ok[3] + ok[4] + ok[5] + ok[6] + ok[7] + ok[8] + ok[9] + ok[10] + ok[11] + ok[12] + ok[13];

                if(i == j) return FALSE;
        }
        else if(d_ptr->mode == DUNGEON_MODE_OR)
        {
                byte ok = FALSE, i;
                s32b flag;

                for(i = 0; i < 32; i++)
                {
                        flag = d_ptr->mflags1 & (1 << i);
                        if(r_ptr->flags1 & flag) ok = TRUE;

                        flag = d_ptr->mflags2 & (1 << i);
                        if(r_ptr->flags2 & flag) ok = TRUE;

                        flag = d_ptr->mflags3 & (1 << i);
                        if(r_ptr->flags3 & flag) ok = TRUE;

                        flag = d_ptr->mflags4 & (1 << i);
                        if(r_ptr->flags4 & flag) ok = TRUE;

                        flag = d_ptr->mflags5 & (1 << i);
                        if(r_ptr->flags5 & flag) ok = TRUE;

                        flag = d_ptr->mflags6 & (1 << i);
                        if(r_ptr->flags6 & flag) ok = TRUE;

                        flag = d_ptr->mflags7 & (1 << i);
                        if(r_ptr->flags7 & flag) ok = TRUE;

                        flag = d_ptr->mflags8 & (1 << i);
                        if(r_ptr->flags8 & flag) ok = TRUE;

                        flag = d_ptr->mflags9 & (1 << i);
                        if(r_ptr->flags9 & flag) ok = TRUE;
                }
                for(a = 0; a < 5; a++)
                        if(d_ptr->r_char[a] == r_ptr->d_char) ok = TRUE;

                return ok;
        }
        else if(d_ptr->mode == DUNGEON_MODE_NOR)
        {
                byte ok = TRUE, i;
                s32b flag;

                for(i = 0; i < 32; i++)
                {
                        flag = d_ptr->mflags1 & (1 << i);
                        if(r_ptr->flags1 & flag) ok = FALSE;

                        flag = d_ptr->mflags2 & (1 << i);
                        if(r_ptr->flags2 & flag) ok = FALSE;

                        flag = d_ptr->mflags3 & (1 << i);
                        if(r_ptr->flags3 & flag) ok = FALSE;

                        flag = d_ptr->mflags4 & (1 << i);
                        if(r_ptr->flags4 & flag) ok = FALSE;

                        flag = d_ptr->mflags5 & (1 << i);
                        if(r_ptr->flags5 & flag) ok = FALSE;

                        flag = d_ptr->mflags6 & (1 << i);
                        if(r_ptr->flags6 & flag) ok = FALSE;

                        flag = d_ptr->mflags7 & (1 << i);
                        if(r_ptr->flags7 & flag) ok = FALSE;

                        flag = d_ptr->mflags8 & (1 << i);
                        if(r_ptr->flags8 & flag) ok = FALSE;

                        flag = d_ptr->mflags9 & (1 << i);
                        if(r_ptr->flags9 & flag) ok = FALSE;
                }
                for(a = 0; a < 5; a++)
                        if(d_ptr->r_char[a] == r_ptr->d_char) ok = FALSE;
                return ok;
        }

        return TRUE;
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
	int			i, j, p;

	int			r_idx;

	long		value, total;
	bool mustbecursed;

	monster_race	*r_ptr;

	alloc_entry		*table = alloc_race_table;


	/* Boost the level */
	if (level > 0)
	{
                /* No matter what, depth has an influence on the level */
                /* It MAY not have anything to do, so there is a SMALL */
                /* chance to find that level 127 Kobold on dungeon level */
                /* 127... :) */
                /*if (rand_int(100) <= 92 && p_ptr->lev >= 4)
                {
                        level += dun_level / 4;
                        level += p_ptr->lev / 4;
                }*/

		/* Occasional "nasty" monster */
		if (rand_int(NASTY_MON) == 0)
		{
			/* Pick a level bonus */
			int d = level / 4 + 2;

			/* Boost the level */
			level += ((d < 5) ? d : 5);
		}

		/* Occasional "nasty" monster */
		/*if (rand_int(NASTY_MON) == 0)
		{
			int d = level / 4 + 2;

			level += ((d < 5) ? d : 5);
		}*/
	}

	/* If not in a quest level, Divination's Twist Fate could affect this. */
	if (!(p_ptr->inside_quest) && p_ptr->events[29003] == 1)
	{
		level += fate_monsters(2);
		if (level < 1) level = 1;
	}

	/* If you are cursed, there's one chance out of 10 that a "Nightmare" monster */
	/* Will spawn.*/
	mustbecursed = FALSE;
	if (p_ptr->cursed > 0 && !(p_ptr->inside_quest) && dun_level > 0 && randint(100) <= (10 + p_ptr->cursed / 10))
	{
		mustbecursed = TRUE;
		level = 30000;
	}


	/* Reset total */
	total = 0L;

	/* Process probabilities */
	for (i = 0; i < alloc_race_size; i++)
	{
		/* Access the "r_idx" of the chosen monster */
		r_idx = table[i].index;

		/* Access the actual race */
		r_ptr = &r_info[r_idx];

		/* Monsters are sorted by depth */
		if (table[i].level > level && (r_ptr->cursed == 0)) break;

		/* Default */
		table[i].prob3 = 0;

		/* Hack -- "unique" monsters must be "unique" */
		if ((r_ptr->flags1 & (RF1_UNIQUE)) &&
		    (r_ptr->cur_num >= r_ptr->max_num))
		{
			continue;
		}

		/* Hack -- don't create questors */
		if (r_ptr->flags1 & RF1_QUESTOR)
		{
			continue;
		}

		/* Depth Monsters never appear out of depth */
		if ((r_ptr->flags1 & (RF1_FORCE_DEPTH)) && (r_ptr->level > dun_level))
		{
			continue;
		}

                /* Depth Monsters never appear out of their depth */
                if ((r_ptr->flags9 & (RF9_ONLY_DEPTH)) && (r_ptr->level != dun_level))
		{
			continue;
		}

		/* Cursed monsters shouldn't appear if you're not cursed. */
		if (r_ptr->cursed > 0 && (p_ptr->cursed <= 0 || p_ptr->inside_quest || dun_level == 0)) continue;

		/* But sometimes, it HAS to be a cursed monster. */
		if ((mustbecursed) && r_ptr->cursed == 0) continue;

		/* NO_GENERIC means monsters with dungeon id 0 will never appear. */
		if ((dun_level) && (d_info[dungeon_type].flags1 & DF1_NO_GENERIC))
		{
			if (r_ptr->dunnum == 0) continue;
		}

		/* Dungeon restrictions. */
		/* If event is set to 4, bypass any restrictions. */
		if (p_ptr->events[29021] != 4)
		{
			/* If in an UNDEAD dungeon, only generate undead monsters! */
			if (((dun_level) && (d_info[dungeon_type].flags1 & DF1_UNDEAD)) || p_ptr->events[29021] == 1)
			{
				if (!(r_ptr->flags3 & (RF3_UNDEAD)) && !(r_ptr->cursed > 0)) continue;
			}
			/* If in a DEMON dungeon, only generate demon monsters! */
			if (((dun_level) && (d_info[dungeon_type].flags1 & DF1_DEMON)) || p_ptr->events[29021] == 2)
			{
				if (!(r_ptr->flags3 & (RF3_DEMON)) && !(r_ptr->cursed > 0)) continue;
			}
			/* If in a DRAGON dungeon, only generate dragon monsters! */
			if (((dun_level) && (d_info[dungeon_type].flags1 & DF1_DRAGON)) || p_ptr->events[29021] == 3)
			{
				if (!(r_ptr->flags3 & (RF3_DRAGON)) && !(r_ptr->cursed > 0)) continue;
			}
			/* If in a RANDOM_ONLY dungeon, only generate random monsters! */
			if (((dun_level) && (d_info[dungeon_type].flags1 & DF1_RANDOM_ONLY)))
			{
				if (!(r_ptr->flags7 & (RF7_RANDOM)) && !(r_ptr->cursed > 0)) continue;
			}
			/* If in an ICE dungeon, only generate "ICE" monsters. */
			/* We can also place monsters with at least 25% Cold resistance. */
			/* If NOT an ICE dungeon, then don't place ICE monsters. */
			if ((dun_level) && (d_info[dungeon_type].flags1 & DF1_ICE))
			{
				if (!(r_ptr->flags7 & (RF7_ICE)) && !(r_ptr->cursed > 0))
				{
					if (r_ptr->resistances[GF_COLD] < 25) continue;
				}
			}
			if ((dun_level) && ((r_ptr->flags7 & (RF7_ICE)) && !(d_info[dungeon_type].flags1 & DF1_ICE)))
			{
				continue;
			}
		}

		/* Check if we're in the proper town and proper dungeon! */
		if (p_ptr->towns[p_ptr->town_num] != 0)
		{
			if ((p_ptr->towns[p_ptr->town_num] != r_ptr->townnum) && (r_ptr->townnum != 0))
			{
				continue;
			}
		}
		else if ((p_ptr->town_num != r_ptr->townnum) && (r_ptr->townnum != 0))
		{
			continue;
		}

		if ((p_ptr->recall_dungeon != r_ptr->dunnum) && (r_ptr->dunnum != 0))
		{
			continue;
		}

                /* Some dungeon types restrict the possible monsters */
                if(!restrict_monster_to_dungeon(r_ptr) && dun_level) continue;

		/* Accept */
		table[i].prob3 = table[i].prob2;

		/* Total */
		total += table[i].prob3;
	}

	/* No legal monsters */
	if (total <= 0) return (0);


	/* Pick a monster */
	value = rand_int(total);

	/* Find the monster */
	for (i = 0; i < alloc_race_size; i++)
	{
		/* Found the entry */
		if (value < table[i].prob3) break;

		/* Decrement */
		value = value - table[i].prob3;
	}


	/* Power boost */
	p = rand_int(100);

	/* Try for a "harder" monster once (50%) or twice (10%) */
	/*if (p < 60)*/

	/* Modified so it always tries at least once for a better monster. */
	/* Raised the chance it will try twice. */
	if ((p < 50) || !(mustbecursed))
	{

		/* Save old */
		j = i;

		/* Pick a monster */
		value = rand_int(total);

		/* Find the monster */
		for (i = 0; i < alloc_race_size; i++)
		{
			/* Found the entry */
			if (value < table[i].prob3) break;

			/* Decrement */
			value = value - table[i].prob3;
		}
		
		/* Keep the "best" one */
		if (table[i].level < table[j].level) i = j;
	}

	/* Try for a "harder" monster twice (10%) */
	if (p < 50 && !(mustbecursed))
	{
		/* Save old */
		j = i;

		/* Pick a monster */
		value = rand_int(total);
		
		/* Find the monster */
		for (i = 0; i < alloc_race_size; i++)
		{
			/* Found the entry */
			if (value < table[i].prob3) break;

			/* Decrement */
			value = value - table[i].prob3;
		}
		
		/* Keep the "best" one */
		if (table[i].level < table[j].level) i = j;
	}

	/* Past 40, try every 10 floors. */
	if (dun_level >= 40)
	{
		int harderloop;

		harderloop = (dun_level-30);

		/* No more than 5 loops. */
		if (harderloop > 25) harderloop = 25;

		for (p = 0; p < (harderloop / 5); p++)
		{
			/* Save old */
			j = i;

			/* Pick a monster */
			value = rand_int(total);
		
			/* Find the monster */
			for (i = 0; i < alloc_race_size; i++)
			{
				/* Found the entry */
				if (value < table[i].prob3) break;

				/* Decrement */
				value = value - table[i].prob3;
			}
		
			/* Keep the "best" one */
			if (table[i].level < table[j].level) i = j;
		}
	}

	/* Result */
	return (table[i].index);
}





/*
 * Build a string describing a monster in some way.
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
void monster_desc(char *desc, monster_type *m_ptr, int mode)
{
	cptr            res;
	monster_race    *r_ptr = &r_info[m_ptr->r_idx];
	/*cptr            name = (r_name + r_ptr->name);*/
	cptr		name;
	char            silly_name[80];
	bool            seen, pron;

	name = r_ptr->name_char;

	/* Are we hallucinating? (Idea from Nethack...) */
	if (p_ptr->image)
	{
		if (randint(2) == 1)
		{
			monster_race *hallu_race;

			do
			{
				hallu_race = &r_info[randint(max_r_idx-2)];
			}
			while (hallu_race->flags1 & RF1_UNIQUE);

			strcpy(silly_name, (r_name + hallu_race->name));
		}
		else
		{
			get_rnd_line("silly.txt", silly_name);
		}

		/* Better not strcpy it, or we could corrupt r_info... */
		name = silly_name;
	}

	/* Can we "see" it (exists + forced, or visible + not unforced) */
	seen = (m_ptr && ((mode & 0x80) || (!(mode & 0x40) && m_ptr->ml)));

	/* Sexed Pronouns (seen and allowed, or unseen and allowed) */
	pron = (m_ptr && ((seen && (mode & 0x20)) || (!seen && (mode & 0x10))));


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


		/* Assume simple result */
		res = "it";

		/* Brute force: split on the possibilities */
		switch (kind + (mode & 0x07))
		{
			/* Neuter, or unknown */
			case 0x00: res = "it"; break;
			case 0x01: res = "it"; break;
			case 0x02: res = "its"; break;
			case 0x03: res = "itself"; break;
			case 0x04: res = "something"; break;
			case 0x05: res = "something"; break;
			case 0x06: res = "something's"; break;
			case 0x07: res = "itself"; break;

			/* Male (assume human if vague) */
			case 0x10: res = "he"; break;
			case 0x11: res = "him"; break;
			case 0x12: res = "his"; break;
			case 0x13: res = "himself"; break;
			case 0x14: res = "someone"; break;
			case 0x15: res = "someone"; break;
			case 0x16: res = "someone's"; break;
			case 0x17: res = "himself"; break;

			/* Female (assume human if vague) */
			case 0x20: res = "she"; break;
			case 0x21: res = "her"; break;
			case 0x22: res = "her"; break;
			case 0x23: res = "herself"; break;
			case 0x24: res = "someone"; break;
			case 0x25: res = "someone"; break;
			case 0x26: res = "someone's"; break;
			case 0x27: res = "herself"; break;
		}

		/* Copy the result */
		(void)strcpy(desc, res);
	}


	/* Handle visible monsters, "reflexive" request */
	else if ((mode & 0x02) && (mode & 0x01))
	{
		/* The monster is visible, so use its gender */
		if (r_ptr->flags1 & (RF1_FEMALE)) strcpy(desc, "herself");
		else if (r_ptr->flags1 & (RF1_MALE)) strcpy(desc, "himself");
		else strcpy(desc, "itself");
	}


	/* Handle all other visible monster requests */
	else
	{
		/* It could be a Unique */
		if ((r_ptr->flags1 & (RF1_UNIQUE)) && !(p_ptr->image))
		{
			/* Start with the name (thus nominative and objective) */
			(void)strcpy(desc, name);
		}

		/* It could be an indefinite monster */
		else if (mode & 0x08)
		{
			/* XXX Check plurality for "some" */

			/* Indefinite monsters need an indefinite article */
			(void)strcpy(desc, is_a_vowel(name[0]) ? "an " : "a ");
			(void)strcat(desc, name);
		}

		/* It could be a normal, definite, monster */
		else
		{
			/* Definite monsters need a definite article */
			if (is_pet(m_ptr))
                                (void)strcpy(desc, "Your ");
			else
				(void)strcpy(desc, "the ");

			(void)strcat(desc, name);
		}

		/* Handle the Possessive as a special afterthought */
		if (mode & 0x02)
		{
			/* XXX Check for trailing "s" */

			/* Simply append "apostrophe" and "s" */
			(void)strcat(desc, "'s");
		}
	}
}

void monster_race_desc(char *desc, int r_idx)
{
        monster_race *r_ptr = &r_info[r_idx];
	cptr            name = (r_name + r_ptr->name);

		/* It could be a Unique */
                if (r_ptr->flags1 & RF1_UNIQUE)
		{
			/* Start with the name (thus nominative and objective) */
			(void)strcpy(desc, name);
		}

		/* It could be a normal, definite, monster */
		else
		{
			/* Definite monsters need a definite article */
			(void)strcpy(desc, is_a_vowel(name[0]) ? "an " : "a ");

			(void)strcat(desc, name);
		}
}



/*
 * Learn about a monster (by "probing" it)
 */
void lore_do_probe(int m_idx)
{
	monster_type *m_ptr = &m_list[m_idx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	/* Hack -- Memorize some flags */
	r_ptr->r_flags1 = r_ptr->flags1;
	r_ptr->r_flags2 = r_ptr->flags2;
	r_ptr->r_flags3 = r_ptr->flags3;

	/* Update monster recall window */
	if (monster_race_idx == m_ptr->r_idx)
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
void lore_treasure(int m_idx, int num_item, int num_gold)
{
	monster_type *m_ptr = &m_list[m_idx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	/* Note the number of things dropped */
	if (num_item > r_ptr->r_drop_item) r_ptr->r_drop_item = num_item;
	if (num_gold > r_ptr->r_drop_gold) r_ptr->r_drop_gold = num_gold;

	/* Hack -- memorize the good/great flags */
	if (r_ptr->flags1 & (RF1_DROP_GOOD)) r_ptr->r_flags1 |= (RF1_DROP_GOOD);
	if (r_ptr->flags1 & (RF1_DROP_GREAT)) r_ptr->r_flags1 |= (RF1_DROP_GREAT);

	/* Update monster recall window */
	if (monster_race_idx == m_ptr->r_idx)
	{
		/* Window stuff */
		p_ptr->window |= (PW_MONSTER);
	}
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
		if (p_ptr->telepathy && !(p_ptr->inside_quest) && !(c_ptr->info & (CAVE_ICKY)))
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
		else if (p_ptr->telepathy && ((p_ptr->inside_quest) || (c_ptr->info & (CAVE_ICKY))))
		{
			if ((c_ptr->info & (CAVE_VIEW)))
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
		}

		/* Apply "detection" spells */
		if (m_ptr->mflag & (MFLAG_MARK)) flag = TRUE;

		/* Hack -- Wizards have "perfect telepathy" */
		if (wizard) flag = TRUE;
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

			/* Hack -- Count "fresh" sightings */
			if (r_ptr->r_sights < MAX_SHORT) r_ptr->r_sights++;

			/* Disturb on appearance */
            if (disturb_move)
            {   if (disturb_pets || !is_pet(m_ptr))
                    disturb(1, 0);
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

			/* Disturb on disappearance*/
            if (disturb_move)
            {
                if (disturb_pets || !is_pet(m_ptr))
                    disturb(1, 0);
                }
		}
	}


	/* The monster is now easily visible */
	if (easy)
	{

		/* Change */
		if (!(m_ptr->mflag & (MFLAG_VIEW)))
		{
			/* Mark as easily visible */
			m_ptr->mflag |= (MFLAG_VIEW);

			/* Disturb on appearance */
            if (disturb_near)
            {
                if (disturb_pets || !is_pet(m_ptr))
                    disturb(1, 0);
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
                if (disturb_pets || !is_pet(m_ptr))
                    disturb(1, 0);
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
 * This is the only function which may place a monster in the dungeon,
 * except for the savefile loading code.
 */
bool place_monster_one(int y, int x, int r_idx, bool slp, bool charm, int dur)
{
        int             i, tempint;
	int		curseelitemod;

        char            dummy[5];

	cave_type		*c_ptr;

	monster_type	*m_ptr;

	monster_race	*r_ptr = &r_info[r_idx];

	cptr		name = (r_name + r_ptr->name);

	bool scaled = FALSE;
	int scaledlevel;
	

	/* Verify location */
	if (!in_bounds(y, x)) return (FALSE);

	/* Require empty space */
	if (!cave_empty_bold(y, x)) return (FALSE);

	/* Hack -- no creation on glyph of warding */
	if (cave[y][x].feat == FEAT_GLYPH) return (FALSE);
	if (cave[y][x].feat == FEAT_MINOR_GLYPH) return (FALSE);

        /* Nor on the between */
        if (cave[y][x].feat == FEAT_BETWEEN) return (FALSE);

        /* Nor on the altars */
        if ((cave[y][x].feat >= FEAT_ALTAR_HEAD)
         && (cave[y][x].feat <= FEAT_ALTAR_TAIL))
                return (FALSE);

	/* Nor on the Pattern */
	if ((cave[y][x].feat >= FEAT_PATTERN_START)
	 && (cave[y][x].feat <= FEAT_PATTERN_XTRA2))
		return (FALSE);

	/* Paranoia */
	if (!r_idx) return (FALSE);

	/* Paranoia */
	if (!r_ptr->name) return (FALSE);

#if 0
        /* No Uniques on saved levels(this would cause too much confusion) */
        if((d_info[dungeon_type].flags1 & DF1_PERSISTENT) && (r_ptr->flags1 & RF1_UNIQUE)) return (FALSE);
#endif

        /* Don't allow undefined ghosts */
        if ((r_idx >= GHOST_R_IDX_HEAD)&&(r_idx <= GHOST_R_IDX_TAIL))
                if(ghost_file[r_idx - GHOST_R_IDX_HEAD][0] == 0)
                        return FALSE;

	if (!monster_can_cross_terrain(cave[y][x].feat, r_ptr))
	{
		return FALSE;
	}

        /* Unallow some uniques to be generated outside of their quests/special levels/dungeons */
        if ((r_ptr->flags9 & RF9_SPECIAL_GENE) && !hack_allow_special && !summoning_specific)
        {
                return FALSE;
        }

	/* Random monsters should only be placed in random dungeons. */
	if ((r_ptr->flags7 & (RF7_RANDOM)) && !(d_info[dungeon_type].flags1 & DF1_RANDOM_ONLY) && !summoning_specific) return FALSE;

	/* Ice monsters should only be placed in ice dungeons. */
	if ((r_ptr->flags7 & (RF7_ICE)) && !(d_info[dungeon_type].flags1 & DF1_ICE) && !summoning_specific) return FALSE;

	/* Cursed monsters cannot appear unless the player is cursed enough. */
	if (r_ptr->cursed > 0 && !summoning_specific)
	{
		if ((p_ptr->cursed < r_ptr->cursed) || (p_ptr->inside_quest) || dun_level == 0 || (charm))
		{
			return FALSE;
		}
	}

	/* Hack -- "unique" monsters must be "unique" */
        if ((r_ptr->flags1 & (RF1_UNIQUE)) && (r_ptr->max_num == -1))
	{
		/* Cannot create */
		return (FALSE);
	}
	

        /* The monster is already on an unique level */
        if (r_ptr->on_saved) return (FALSE);

	/* Hack -- "unique" monsters must be "unique" */
	if ((r_ptr->flags1 & (RF1_UNIQUE)) && (r_ptr->cur_num >= r_ptr->max_num))
	{
		/* Cannot create */
		return (FALSE);
	}

	/* Depth monsters may NOT be created out of depth */
	if ((r_ptr->flags1 & (RF1_FORCE_DEPTH)) && (dun_level < r_ptr->level))
	{
		/* Cannot create */
		return (FALSE);
	}


	/* Powerful monster */
	if (r_ptr->level > dun_level)
	{
		/* Unique monsters */
		if (r_ptr->flags1 & (RF1_UNIQUE))
		{
			/* Message for cheaters */
                        if ((cheat_hear)) msg_format("Deep Unique (%s).", name);

			/* Boost rating by twice delta-depth */
			rating += (r_ptr->level - dun_level) * 2;
		}

		/* Normal monsters */
		else
		{
			/* Message for cheaters */
                        if ((cheat_hear)) msg_format("Deep Monster (%s).", name);

			/* Boost rating by delta-depth */
			rating += (r_ptr->level - dun_level);
		}
	}

	/* Note the monster */
	else if (r_ptr->flags1 & (RF1_UNIQUE))
	{
		/* Unique monsters induce message */
                if ((cheat_hear)) msg_format("Unique (%s).", name);
	}

	/* This is a scaled enemy. It's stats scales with the player's level. */
	if (r_ptr->flags7 & (RF7_SCALED))
	{
		scaled = TRUE;
		if (p_ptr->max_plv > r_ptr->level) scaledlevel = p_ptr->max_plv;
		else scaledlevel = r_ptr->level;
	}


	/* Access the location */
	c_ptr = &cave[y][x];

	/* Make a new monster */
        c_ptr->m_idx = m_pop();
	hack_m_idx_ii = c_ptr->m_idx;

	/* Mega-Hack -- catch "failure" */
	if (!c_ptr->m_idx) return (FALSE);


	/* Get a new monster record */
	m_ptr = &m_list[c_ptr->m_idx];

	/* Save the race */
	m_ptr->r_idx = r_idx;

	/* Place the monster at the location */
	m_ptr->fy = y;
	m_ptr->fx = x;


	/* No "damage" yet */
	m_ptr->stunned = 0;
	m_ptr->confused = 0;
	m_ptr->monfear = 0;

        /* Not a boss and no abilities yet... */
        m_ptr->boss = 0;
        
        /* Not hit by Sealing Light */
        m_ptr->seallight = 0;

	/* Not hasted nor boosted */
	m_ptr->hasted = 0;
	m_ptr->boosted = 0;

	/* Not summoned */
	m_ptr->summoned = 0;

	/* No lives */
	m_ptr->lives = 0;

	/* Worth experience. */
	m_ptr->no_experience = FALSE;

        /* No objects yet */
        m_ptr->hold_o_idx = 0;

	/* Stats are set to 0 */
	m_ptr->str = 0;
	m_ptr->dex = 0;
	m_ptr->mind = 0;
	m_ptr->skill_attack = 0;
	m_ptr->skill_ranged = 0;
	m_ptr->skill_magic = 0;
	m_ptr->mana = 0;

        /* Not an animated monster...never will. */
        m_ptr->animated = FALSE;
        m_ptr->animdam_d = 0;
        m_ptr->animdam_s = 0;

	/* Friendly? */
        if ( charm || (r_ptr->flags7 & RF7_PET) )
	{
		set_pet(m_ptr, TRUE);
	}

	if (p_ptr->cursed > 0 && !(p_ptr->inside_quest) && dun_level > 0)
	{
		curseelitemod = p_ptr->cursed / 3;
		if (curseelitemod > 85) curseelitemod = 85;
	}
	else curseelitemod = 0;

        /* Will this monster be an elite? */
	if (!(p_ptr->inside_quest) && p_ptr->events[29003] == 1)
	{
		if (randint(100) <= fate_monsters(3) && !(r_ptr->flags1 & (RF1_UNIQUE))) m_ptr->boss = 1;
	}
        else if (randint(100) <= (15 + curseelitemod) && !(r_ptr->flags1 & (RF1_UNIQUE))) m_ptr->boss = 1;
        /* Even worse, will it be a boss? */
        if (m_ptr->boss == 1 && randint(100) >= 80) m_ptr->boss = 2;
        /* No bosses in towns... */
        if (dun_level == 0) m_ptr->boss = 0;
        /* Friends aren't bosses! */
        if (is_pet(m_ptr)) m_ptr->boss = 0;
	/* Some monsters are never bosses... */
	if (r_ptr->flags7 & (RF7_NEVER_BOSS)) m_ptr->boss = 0;
	/* Force a boss */
	if (p_ptr->events[29026] > 0) m_ptr->boss = p_ptr->events[29026];
	/* Cursed monsters aren't bosses. They're bad enough like that. */
	if (r_ptr->cursed > 0) m_ptr->boss = 0;

	/* Assume no sleeping */
	m_ptr->csleep = 0;

	/* Enforce sleeping if needed */
        if (slp && r_ptr->sleep && m_ptr->boss == 0)
	{
		int val = r_ptr->sleep;
		m_ptr->csleep = ((val * 2) + randint(val * 10));
	}

        /* Generate the monster's inventory(if any) */
	if (!(p_ptr->inside_quest && !(r_ptr->flags1 & (RF1_UNIQUE))))
        {
                bool good = (r_ptr->flags1 & (RF1_DROP_GOOD)) ? TRUE : FALSE;
                bool great = (r_ptr->flags1 & (RF1_DROP_GREAT)) ? TRUE : FALSE;

                bool do_gold = (!(r_ptr->flags1 & (RF1_ONLY_ITEM)));
                bool do_item = (!(r_ptr->flags1 & (RF1_ONLY_GOLD)));

                int j;

                int force_coin = get_coin_type(r_ptr);

                int dump_item = 0;
                int dump_gold = 0;
                object_type forge;
                object_type *q_ptr;

                int number = 0;

                if (m_ptr->boss == 1)
                {
                        number += 1;
                        good = TRUE;
                        great = TRUE;
                }
                if (m_ptr->boss == 2)
                {
                        number += 4;
                        good = TRUE;
                        great = TRUE;
                }

                /* Average dungeon and monster levels */
		if (scaled)
		{
			if (r_ptr->flags7 & (RF7_SECRET_BOSS)) object_level = scaledlevel * 2;
			else object_level = scaledlevel + (scaledlevel / 2);
		}
                else object_level = (dun_level + r_ptr->level) / 2;

                /* Determine how much we can drop */
                if ((r_ptr->flags1 & (RF1_DROP_60)) && (rand_int(100) < 60)) number++;
                if ((r_ptr->flags1 & (RF1_DROP_90)) && (rand_int(100) < 90)) number++;
                if  (r_ptr->flags1 & (RF1_DROP_1D2)) number += damroll(1, 2);
                if  (r_ptr->flags1 & (RF1_DROP_2D2)) number += damroll(2, 2);
                if  (r_ptr->flags1 & (RF1_DROP_3D2)) number += damroll(3, 2);
                if  (r_ptr->flags1 & (RF1_DROP_4D2)) number += damroll(4, 2);

                /* Friendly monsters do NOT get items! */
                if (is_pet(m_ptr)) number = 0;

                /* Hack -- handle creeping coins */
                coin_type = force_coin;

                /* Drop some objects */
                for (j = 0; j < number; j++)
                {
                        int o_idx;
                        object_type *o_ptr;

                        /* Get local object */
                        q_ptr = &forge;

                        /* Wipe the object */
                        object_wipe(q_ptr);

                        /* Make Gold */
                        if (do_gold && (!do_item || (rand_int(100) < 50)))
                        {
                                /* Make some gold */
                                if (!make_gold(q_ptr)) continue;

                                /* XXX XXX XXX */
                                dump_gold++;
                        }

                        /* Make Object */
                        else
                        {
                                /* Make an object */
                                if (!make_object(q_ptr, good, great)) continue;

                                /* XXX XXX XXX */
                                dump_item++;
                        }

                        /* Get new object */
                        o_idx = o_pop();

                        if(o_idx)
                        {
                                /* Get the item */
                                o_ptr = &o_list[o_idx];

                                /* Ugly hack for Molotov Cocktails... */
                                if (o_ptr->tval == TV_POTION && o_ptr->sval == 68) o_ptr->sval == 2;

                                /* Structure copy */
                                object_copy(o_ptr, q_ptr);

                                /* Build a stack */
                                o_ptr->next_o_idx = m_ptr->hold_o_idx;

                                o_ptr->held_m_idx = c_ptr->m_idx;
                                o_ptr->ix = 0;
                                o_ptr->iy = 0;

                                m_ptr->hold_o_idx = o_idx;
                        }
                }

                /* Reset the object level */
                object_level = dun_level;

                /* Reset "coin" type */
                coin_type = 0;
        }

	/* Unknown distance */
	m_ptr->cdis = 0;

	/* No flags */
	m_ptr->mflag = 0;

	/* Not visible */
	m_ptr->ml = FALSE;

        /* Give the monster a level */
	if (scaled) tempint = scaledlevel;
        else tempint = dun_level;

	if (r_ptr->cursed > 0 && tempint < r_ptr->cursed) tempint = r_ptr->cursed;
	if (m_ptr->boss >= 1 || r_ptr->cursed > 0 || (scaled)) m_ptr->level = tempint;
        else m_ptr->level = randint(tempint / 2) + (tempint / 2);
	if (scaled)
	{
		if (r_ptr->cursed > 0 && m_ptr->level < scaledlevel) m_ptr->level = scaledlevel;
	}
	else
	{
		if (r_ptr->cursed > 0 && m_ptr->level < r_ptr->level) m_ptr->level = r_ptr->level;
	}
        if (p_ptr->lev >= 4 || (scaled)) m_ptr->level += randint(3);
        /* Higher levels for lower depths...and higher player levels! */
        if (p_ptr->lev >= 4 || (scaled))
        {
                m_ptr->level += (tempint / 4);
		if (scaled && p_ptr->lev < tempint) m_ptr->level += tempint / 4;
                else m_ptr->level += p_ptr->lev / 4;
        }
	/* However, a fixed level will override everything. */
	if (r_ptr->fixedlevel > 0) m_ptr->level = r_ptr->fixedlevel;

        /* Bosses are 5 levels higher than the current dungeon level */
        if (m_ptr->boss == 2) m_ptr->level += 5;

	/* Possible modifier. */
	m_ptr->level += p_ptr->events[29025];

	/* Your worst nightmare! */
	if (r_ptr->cursed > 0) m_ptr->level += p_ptr->cursed / 4;

	/* If not in a quest level, Divination's Twist Fate could affect this. */
	if (!(p_ptr->inside_quest) && p_ptr->events[29003] == 1 && !(charm))
	{
		m_ptr->level += fate_monsters(1);
		if (m_ptr->level < 1) m_ptr->level = 1;
	}

        if (m_ptr->level < (dun_level / 2)) m_ptr->level = dun_level;
        if (is_pet(m_ptr) && m_ptr->r_idx == 1080) m_ptr->level = 1;
        if (m_ptr->level <= 0) m_ptr->level = 1;
        /* if (m_ptr->level > tempint && m_ptr->boss == 0) m_ptr->level = tempint; */
        if (m_ptr->level <= 0) m_ptr->level = 1;

	/* Summoned? */
	m_ptr->summoned = dur;
	if (m_ptr->summoned > 0) m_ptr->no_experience = TRUE;
	if (m_ptr->summoned < 0)
	{
		m_ptr->summoned = 0;
		m_ptr->no_experience = TRUE;
	}

	/* Lives */
	m_ptr->lives = r_ptr->lives;

	/* To keep up Nightmares difficulty! :) */
	if (r_ptr->cursed > 0) m_ptr->lives += multiply_divide(m_ptr->lives, m_ptr->level * 3, 100);

	/* Scaled enemies can gain extra lives. */
	if (scaled)
	{
		int baselives = m_ptr->lives;

		
		/* Increment by 5% per levels. */
		baselives = baselives + multiply_divide(baselives, (scaledlevel * 5), 100);

		/* Add some lives. */
		if (scaledlevel >= 20)
		{
			baselives = baselives + ((scaledlevel - 10) / 10);
		}

		m_ptr->lives = baselives;
	}

	/* Determine the monster's hp. */
	{
		int a;
		s32b hpperlevels;

		if (r_ptr->flags1 & (RF1_FORCE_MAXHP))
		{
			if (scaled) hpperlevels = maxroll(r_ptr->hdice * scaledlevel, r_ptr->hside * scaledlevel);
			else hpperlevels = maxroll(r_ptr->hdice, r_ptr->hside);
		}
		else
		{
			if (scaled) hpperlevels = damroll(r_ptr->hdice * scaledlevel, r_ptr->hside * scaledlevel);
			else hpperlevels = damroll(r_ptr->hdice, r_ptr->hside);
		}

		hpperlevels += multiply_divide(hpperlevels, (m_ptr->level * 5), 100);

		/* Three times more hp for elites... */
        	if (m_ptr->boss == 1) hpperlevels *= 3;
        	/* SIX TIMES more hp for bosses!!! */
        	if (m_ptr->boss == 2 || r_ptr->cursed > 0) hpperlevels *= 6;

		/* Assume 0 max hp. */
		m_ptr->maxhp = 0;

		for (a = 0; a < m_ptr->level; a++)
		{
        		m_ptr->maxhp += hpperlevels;
			if (m_ptr->maxhp > 2000000000 || m_ptr->maxhp < 0)
			{
				m_ptr->maxhp = 2000000000;
				break;
			}
		}
	}

	/* And start out fully healthy */
	m_ptr->hp = m_ptr->maxhp;

	/* Now, determine the stats! */
	if (scaled)
	{
		m_ptr->str = multiply_divide(scaledlevel, r_ptr->str, 100) + multiply_divide(multiply_divide(scaledlevel, r_ptr->str, 100), ((m_ptr->level - 1) * 5), 100);
		m_ptr->dex = multiply_divide(scaledlevel, r_ptr->dex, 100) + multiply_divide(multiply_divide(scaledlevel, r_ptr->dex, 100), ((m_ptr->level - 1) * 5), 100);
		m_ptr->mind = multiply_divide(scaledlevel, r_ptr->mind, 100) + multiply_divide(multiply_divide(scaledlevel, r_ptr->mind, 100), ((m_ptr->level - 1) * 5), 100);
		m_ptr->skill_attack = multiply_divide(scaledlevel, r_ptr->skill_attack, 100) + multiply_divide(multiply_divide(scaledlevel, r_ptr->skill_attack, 100), ((m_ptr->level - 1) * 5), 100);
		m_ptr->skill_ranged = multiply_divide(scaledlevel, r_ptr->skill_ranged, 100) + multiply_divide(multiply_divide(scaledlevel, r_ptr->skill_ranged, 100), ((m_ptr->level - 1) * 5), 100);
		m_ptr->skill_magic = multiply_divide(scaledlevel, r_ptr->skill_magic, 100) + multiply_divide(multiply_divide(scaledlevel, r_ptr->skill_magic, 100), ((m_ptr->level - 1) * 5), 100);
	}
	else
	{
		m_ptr->str = r_ptr->str + multiply_divide(r_ptr->str, ((m_ptr->level - 1) * 5), 100);
		m_ptr->dex = r_ptr->dex + multiply_divide(r_ptr->dex, ((m_ptr->level - 1) * 5), 100);
		m_ptr->mind = r_ptr->mind + multiply_divide(r_ptr->mind, ((m_ptr->level - 1) * 5), 100);
		m_ptr->skill_attack = r_ptr->skill_attack + multiply_divide(r_ptr->skill_attack, ((m_ptr->level - 1) * 5), 100);
		m_ptr->skill_ranged = r_ptr->skill_ranged + multiply_divide(r_ptr->skill_ranged, ((m_ptr->level - 1) * 5), 100);
		m_ptr->skill_magic = r_ptr->skill_magic + multiply_divide(r_ptr->skill_magic, ((m_ptr->level - 1) * 5), 100);
	}

	if (m_ptr->boss == 1)
	{
		m_ptr->str += multiply_divide(m_ptr->str, (m_ptr->level / 3), 100);
		m_ptr->dex += multiply_divide(m_ptr->dex, (m_ptr->level / 3), 100);
		m_ptr->mind += multiply_divide(m_ptr->mind, (m_ptr->level / 3), 100);
		m_ptr->skill_attack += multiply_divide(m_ptr->skill_attack, (m_ptr->level / 3), 100);
		m_ptr->skill_ranged += multiply_divide(m_ptr->skill_ranged, (m_ptr->level / 3), 100);
		m_ptr->skill_magic += multiply_divide(m_ptr->skill_magic, (m_ptr->level / 3), 100);
	}

	if (m_ptr->boss == 2 || r_ptr->cursed > 0)
	{
		m_ptr->str += multiply_divide(m_ptr->str, m_ptr->level + r_ptr->cursed, 100);
		m_ptr->dex += multiply_divide(m_ptr->dex, m_ptr->level + r_ptr->cursed, 100);
		m_ptr->mind += multiply_divide(m_ptr->mind, m_ptr->level + r_ptr->cursed, 100);
		m_ptr->skill_attack += multiply_divide(m_ptr->skill_attack, m_ptr->level + r_ptr->cursed, 100);
		m_ptr->skill_ranged += multiply_divide(m_ptr->skill_ranged, m_ptr->level + r_ptr->cursed, 100);
		m_ptr->skill_magic += multiply_divide(m_ptr->skill_magic, m_ptr->level + r_ptr->cursed, 100);
	}

	if (m_ptr->str > 2000000000 || m_ptr->str < 0) m_ptr->str = 2000000000;
	if (m_ptr->dex > 2000000000 || m_ptr->dex < 0) m_ptr->dex = 2000000000;
	if (m_ptr->mind > 2000000000 || m_ptr->mind < 0) m_ptr->mind = 2000000000;
	if (m_ptr->skill_attack > 2000000000 || m_ptr->skill_attack < 0) m_ptr->skill_attack = 2000000000;
	if (m_ptr->skill_ranged > 2000000000 || m_ptr->skill_ranged < 0) m_ptr->skill_ranged = 2000000000;
	if (m_ptr->skill_magic > 2000000000 || m_ptr->skill_magic < 0) m_ptr->skill_magic = 2000000000;

        /* Hit rate! */
        m_ptr->hitrate = ((m_ptr->dex - 5) * 10) + 2;
	m_ptr->hitrate += ((m_ptr->str - 5) / 2);
	m_ptr->hitrate += multiply_divide(m_ptr->hitrate, ((m_ptr->dex - 5) * 5), 100);

        /* Defense! */
	if (scaled) m_ptr->defense = multiply_divide(scaledlevel, r_ptr->ac, 100) + multiply_divide(r_ptr->ac, ((m_ptr->level - 1) * 5), 100);
	else m_ptr->defense = r_ptr->ac + multiply_divide(r_ptr->ac, ((m_ptr->level - 1) * 5), 100);

	/* Mana! */
	m_ptr->mana = (m_ptr->mind - 5) * 10;
	if (m_ptr->mana < 0) m_ptr->mana = 0;

	if (m_ptr->boss == 1) m_ptr->defense += multiply_divide(m_ptr->defense, (m_ptr->level * 5), 100);
	if (m_ptr->boss == 2 || r_ptr->cursed > 0) m_ptr->defense += multiply_divide(m_ptr->defense, (m_ptr->level * 10), 100);

        /* If the monster is an elite or a boss, give it an ability... */
        if (m_ptr->boss == 1) get_boss_ability(m_ptr, 1);
        if (m_ptr->boss == 2)
        {
                if (dun_level <= 10) get_boss_ability(m_ptr, 1);
                else get_boss_ability(m_ptr, (dun_level / 10));
        }

	/* Extract the monster base speed */
	m_ptr->mspeed = r_ptr->speed;
	
        /* Level will now boost the speed a little... */
	{
		int a, b;

		b = (m_ptr->level / 5);

		for (a = 0; a < b; a++)
		{
        		m_ptr->mspeed += 1;
			if (m_ptr->mspeed > 180)
			{
				m_ptr->mspeed = 180;
				break;
			}
		}
	}

	/* Give a random starting energy */
	/*m_ptr->energy = (byte)rand_int(100);*/
	m_ptr->energy = 100;

	/* Force monster to wait for player */
	if (r_ptr->flags1 & (RF1_FORCE_SLEEP))
	{
		/* Monster is still being nice */
		m_ptr->mflag |= (MFLAG_NICE);

		/* Must repair monsters */
		repair_monsters = TRUE;
	}

	/* Hack -- see "process_monsters()" */
	if (c_ptr->m_idx < hack_m_idx)
	{
		/* Monster is still being born */
		m_ptr->mflag |= (MFLAG_BORN);
	}


	/* Update the monster */
	update_mon(c_ptr->m_idx, TRUE);


	/* Hack -- Count the monsters on the level */
	r_ptr->cur_num++;

        /* Unique monsters on saved levels should be "marked" */
        if ((r_ptr->flags1 & RF1_UNIQUE) && get_dungeon_save(dummy))
        {
                r_ptr->on_saved = TRUE;
        }

	/* Hack -- Count the number of "reproducers" */
	if (r_ptr->flags2 & (RF2_MULTIPLY)) num_repro++;


	/* Hack -- Notice new multi-hued monsters */
	if (r_ptr->flags1 & (RF1_ATTR_MULTI)) shimmer_monsters = TRUE;

	/* Call a lua script. */
	if (r_ptr->event_spawn > 0)
	{
		call_lua("monster_spawn", "(dd)", "", c_ptr->m_idx, r_ptr->event_spawn);
	}

	/* Success */
	return (TRUE);
}

s16b place_monster_one_return(int y, int x, int r_idx, bool slp, bool charm, int monlevel, int dur)
{
        int             i, tempint;
	int		curseelitemod;
        char            dummy[5];

	cave_type		*c_ptr;

	monster_type	*m_ptr;

	monster_race	*r_ptr = &r_info[r_idx];

	cptr		name = (r_name + r_ptr->name);

	bool scaled = FALSE;
	int scaledlevel;


	/* Verify location */
        if (!in_bounds(y, x)) return 0;

	/* Require empty space */
        if (!cave_empty_bold(y, x)) return 0;

	/* Hack -- no creation on glyph of warding */
        if (cave[y][x].feat == FEAT_GLYPH) return 0;
        if (cave[y][x].feat == FEAT_MINOR_GLYPH) return 0;

        /* Nor on the between */
        if (cave[y][x].feat == FEAT_BETWEEN) return 0;

	/* Nor on the Pattern */
	if ((cave[y][x].feat >= FEAT_PATTERN_START)
	 && (cave[y][x].feat <= FEAT_PATTERN_XTRA2))
                return 0;

	/* Paranoia */
        if (!r_idx) return 0;

	/* Paranoia */
        if (!r_ptr->name) return 0;

	if (!monster_can_cross_terrain(cave[y][x].feat, r_ptr))
	{
		return FALSE;
	}

	/* Hack -- "unique" monsters must be "unique" */
	if ((r_ptr->flags1 & (RF1_UNIQUE)) && (r_ptr->cur_num >= r_ptr->max_num))
	{
		/* Cannot create */
                return 0;
	}

	/* Depth monsters may NOT be created out of depth */
	if ((r_ptr->flags1 & (RF1_FORCE_DEPTH)) && (dun_level < r_ptr->level))
	{
		/* Cannot create */
                return 0;
	}

        /* The monster is already on an unique level */
        if (r_ptr->on_saved) return (FALSE);


	/* Powerful monster */
	if (r_ptr->level > dun_level)
	{
		/* Unique monsters */
		if (r_ptr->flags1 & (RF1_UNIQUE))
		{
			/* Message for cheaters */
                        if ((cheat_hear)) msg_format("Deep Unique (%s).", name);

			/* Boost rating by twice delta-depth */
			rating += (r_ptr->level - dun_level) * 2;
		}

		/* Normal monsters */
		else
		{
			/* Message for cheaters */
                        if ((cheat_hear)) msg_format("Deep Monster (%s).", name);

			/* Boost rating by delta-depth */
			rating += (r_ptr->level - dun_level);
		}
	}

	/* Note the monster */
	else if (r_ptr->flags1 & (RF1_UNIQUE))
	{
		/* Unique monsters induce message */
                if ((cheat_hear)) msg_format("Unique (%s).", name);
	}

	/* This is a scaled enemy. It's stats scales with the player's level. */
	if (r_ptr->flags7 & (RF7_SCALED))
	{
		scaled = TRUE;
		if (p_ptr->max_plv > r_ptr->level) scaledlevel = p_ptr->max_plv;
		else scaledlevel = r_ptr->level;
	}


	/* Access the location */
	c_ptr = &cave[y][x];

	/* Make a new monster */
	c_ptr->m_idx = m_pop();
	hack_m_idx_ii = c_ptr->m_idx;

	/* Mega-Hack -- catch "failure" */
        if (!c_ptr->m_idx) return 0;


	/* Get a new monster record */
	m_ptr = &m_list[c_ptr->m_idx];

	/* Save the race */
	m_ptr->r_idx = r_idx;

	/* Place the monster at the location */
	m_ptr->fy = y;
	m_ptr->fx = x;


	/* No "damage" yet */
	m_ptr->stunned = 0;
	m_ptr->confused = 0;
	m_ptr->monfear = 0;

        /* Not an elite or a boss yet... */
        m_ptr->boss = 0;

        /* Not an animated monster...never will. */
        m_ptr->animated = FALSE;
        m_ptr->animdam_d = 0;
        m_ptr->animdam_s = 0;

	/* Friendly? */
        if ( charm || (r_ptr->flags7 & RF7_PET) )
	{
		set_pet(m_ptr, TRUE);
	}

	if (p_ptr->cursed > 0 && !(p_ptr->inside_quest) && dun_level > 0)
	{
		curseelitemod = p_ptr->cursed / 3;
		if (curseelitemod > 85) curseelitemod = 85;
	}
	else curseelitemod = 0;

        /* Will this monster be an elite? */
        if (!(p_ptr->inside_quest) && p_ptr->events[29003] == 1)
	{
		if (randint(100) <= fate_monsters(3) && !(r_ptr->flags1 & (RF1_UNIQUE))) m_ptr->boss = 1;
	}
        else if (randint(100) <= (15 + curseelitemod) && !(r_ptr->flags1 & (RF1_UNIQUE))) m_ptr->boss = 1;
        /* Even worse, will it be a boss? */
        if (m_ptr->boss == 1 && randint(100) >= 80) m_ptr->boss = 2;
        /* No bosses in town... */
        if (dun_level == 0) m_ptr->boss = 0;
        /* Friends aren't bosses! */
        if (is_pet(m_ptr)) m_ptr->boss = 0;
	/* Some monsters are never bosses... */
	if (r_ptr->flags7 & (RF7_NEVER_BOSS)) m_ptr->boss = 0;
	/* Force a boss */
	if (p_ptr->events[29026] > 0) m_ptr->boss = p_ptr->events[29026];
	/* Cursed monsters aren't bosses. They're bad enough like that. */
	if (r_ptr->cursed > 0) m_ptr->boss = 0;

	/* Assume no sleeping */
	m_ptr->csleep = 0;

	/* Enforce sleeping if needed */
        if (slp && r_ptr->sleep && m_ptr->boss == 0)
	{
		int val = r_ptr->sleep;
		m_ptr->csleep = ((val * 2) + randint(val * 10));
	}

        /* Generate the monster's inventory(if any) */
        {
                bool good = (r_ptr->flags1 & (RF1_DROP_GOOD)) ? TRUE : FALSE;
                bool great = (r_ptr->flags1 & (RF1_DROP_GREAT)) ? TRUE : FALSE;

                bool do_gold = (!(r_ptr->flags1 & (RF1_ONLY_ITEM)));
                bool do_item = (!(r_ptr->flags1 & (RF1_ONLY_GOLD)));

                int j;

                int force_coin = get_coin_type(r_ptr);

                int dump_item = 0;
                int dump_gold = 0;
                object_type forge;
                object_type *q_ptr;

                int number = 0;

                if (m_ptr->boss == 1)
                {
                        number += 1;
                        good = TRUE;
                        great = TRUE;
                }
                if (m_ptr->boss == 2)
                {
                        number += 4;
                        good = TRUE;
                        great = TRUE;
                }


                /* Average dungeon and monster levels */
		if (scaled)
		{
			if (r_ptr->flags7 & (RF7_SECRET_BOSS)) object_level = scaledlevel * 2;
			else object_level = scaledlevel + (scaledlevel / 2);
		}
                else object_level = (dun_level + r_ptr->level) / 2;

                /* Determine how much we can drop */
                if ((r_ptr->flags1 & (RF1_DROP_60)) && (rand_int(100) < 60)) number++;
                if ((r_ptr->flags1 & (RF1_DROP_90)) && (rand_int(100) < 90)) number++;
                if  (r_ptr->flags1 & (RF1_DROP_1D2)) number += damroll(1, 2);
                if  (r_ptr->flags1 & (RF1_DROP_2D2)) number += damroll(2, 2);
                if  (r_ptr->flags1 & (RF1_DROP_3D2)) number += damroll(3, 2);
                if  (r_ptr->flags1 & (RF1_DROP_4D2)) number += damroll(4, 2);

                /* Friendly monsters do NOT get items! */
                if (is_pet(m_ptr)) number = 0;

                /* Hack -- handle creeping coins */
                coin_type = force_coin;

                /* Drop some objects */
                for (j = 0; j < number; j++)
                {
                        int o_idx;
                        object_type *o_ptr;

                        /* Get local object */
                        q_ptr = &forge;

                        /* Wipe the object */
                        object_wipe(q_ptr);

                        /* Make Gold */
                        if (do_gold && (!do_item || (rand_int(100) < 50)))
                        {
                                /* Make some gold */
                                if (!make_gold(q_ptr)) continue;

                                /* XXX XXX XXX */
                                dump_gold++;
                        }

                        /* Make Object */
                        else
                        {
                                /* Make an object */
                                if (!make_object(q_ptr, good, great)) continue;

                                /* XXX XXX XXX */
                                dump_item++;
                        }

                        /* Get new object */
                        o_idx = o_pop();

                        if(o_idx)
                        {
                                /* Get the item */
                                o_ptr = &o_list[o_idx];

                                /* Structure copy */
                                object_copy(o_ptr, q_ptr);

                                /* Build a stack */
                                o_ptr->next_o_idx = m_ptr->hold_o_idx;

                                o_ptr->held_m_idx = c_ptr->m_idx;
                                o_ptr->ix = 0;
                                o_ptr->iy = 0;

                                m_ptr->hold_o_idx = o_idx;
                        }
                }

                /* Reset the object level */
                object_level = dun_level;

                /* Reset "coin" type */
                coin_type = 0;
        }


	/* Unknown distance */
	m_ptr->cdis = 0;

	/* No flags */
	m_ptr->mflag = 0;

	/* Not visible */
	m_ptr->ml = FALSE;

        /* Give the monster a level */
	if (scaled) tempint = scaledlevel;
        else tempint = dun_level;

	if (r_ptr->cursed > 0 && tempint < r_ptr->cursed) tempint = r_ptr->cursed;
	if (m_ptr->boss >= 1 || r_ptr->cursed > 0 || (scaled)) m_ptr->level = tempint;
        else m_ptr->level = randint(tempint / 2) + (tempint / 2);
	if (scaled)
	{
		if (r_ptr->cursed > 0 && m_ptr->level < scaledlevel) m_ptr->level = scaledlevel;
	}
	else
	{
		if (r_ptr->cursed > 0 && m_ptr->level < r_ptr->level) m_ptr->level = r_ptr->level;
	}
        if (p_ptr->lev >= 4 || (scaled)) m_ptr->level += randint(3);
        /* Higher levels for lower depths...and higher player levels! */
        if (p_ptr->lev >= 4 || (scaled))
        {
                m_ptr->level += (tempint / 4);
		if (scaled && p_ptr->lev < tempint) m_ptr->level += tempint / 4;
                else m_ptr->level += p_ptr->lev / 4;
        }
	/* However, a fixed level will override everything. */
	if (r_ptr->fixedlevel > 0) m_ptr->level = r_ptr->fixedlevel;

        /* Bosses are 5 levels higher than the current dungeon level */
        if (m_ptr->boss == 2) m_ptr->level += 5;

	/* Possible modifier. */
	m_ptr->level += p_ptr->events[29025];

	/* Your worst nightmare! */
	if (r_ptr->cursed > 0) m_ptr->level += p_ptr->cursed / 4;

	/* If not in a quest level, Divination's Twist Fate could affect this. */
	if (!(p_ptr->inside_quest) && p_ptr->events[29003] == 1 && !(charm))
	{
		m_ptr->level += fate_monsters(1);
		if (m_ptr->level < 1) m_ptr->level = 1;
	}

        if (m_ptr->level < (dun_level / 2)) m_ptr->level = dun_level;
        if (is_pet(m_ptr) && m_ptr->r_idx == 1080) m_ptr->level = 1;
        if (m_ptr->level <= 0) m_ptr->level = 1;
        /* if (m_ptr->level > tempint && m_ptr->boss == 0) m_ptr->level = tempint; */
        if (m_ptr->level <= 0) m_ptr->level = 1;

	/* Summoned? */
	m_ptr->summoned = dur;
	if (m_ptr->summoned > 0) m_ptr->no_experience = TRUE;
	if (m_ptr->summoned < 0)
	{
		m_ptr->summoned = 0;
		m_ptr->no_experience = TRUE;
	}

	/* Lives */
	m_ptr->lives = r_ptr->lives;

	/* To keep up Nightmares difficulty! :) */
	if (r_ptr->cursed > 0) m_ptr->lives += multiply_divide(m_ptr->lives, m_ptr->level * 3, 100);

	/* Scaled enemies can gain extra lives. */
	if (scaled)
	{
		int baselives = m_ptr->lives;

		/* Increment by 5% per levels. */
		baselives = baselives + multiply_divide(baselives, (scaledlevel * 5), 100);

		/* Add some lives. */
		if (scaledlevel >= 20)
		{
			baselives = baselives + ((scaledlevel - 10) / 10);
		}

		m_ptr->lives = baselives;
	}

	/* Determine the monster's hp. */
	{
		int a;
		s32b hpperlevels;

		if (r_ptr->flags1 & (RF1_FORCE_MAXHP))
		{
			if (scaled) hpperlevels = maxroll(r_ptr->hdice * scaledlevel, r_ptr->hside * scaledlevel);
			else hpperlevels = maxroll(r_ptr->hdice, r_ptr->hside);
		}
		else
		{
			if (scaled) hpperlevels = damroll(r_ptr->hdice * scaledlevel, r_ptr->hside * scaledlevel);
			else hpperlevels = damroll(r_ptr->hdice, r_ptr->hside);
		}

		hpperlevels += multiply_divide(hpperlevels, (m_ptr->level * 5), 100);

		/* Three times more hp for elites... */
        	if (m_ptr->boss == 1) hpperlevels *= 3;
        	/* SIX TIMES more hp for bosses!!! */
        	if (m_ptr->boss == 2 || r_ptr->cursed > 0) hpperlevels *= 6;

		/* Assume 0 max hp. */
		m_ptr->maxhp = 0;

		for (a = 0; a < m_ptr->level; a++)
		{
        		m_ptr->maxhp += hpperlevels;
			if (m_ptr->maxhp > 2000000000 || m_ptr->maxhp < 0)
			{
				m_ptr->maxhp = 2000000000;
				break;
			}
		}
	}

	/* And start out fully healthy */
	m_ptr->hp = m_ptr->maxhp;

	/* Now, determine the stats! */
	if (scaled)
	{
		m_ptr->str = multiply_divide(scaledlevel, r_ptr->str, 100) + multiply_divide(multiply_divide(scaledlevel, r_ptr->str, 100), ((m_ptr->level - 1) * 5), 100);
		m_ptr->dex = multiply_divide(scaledlevel, r_ptr->dex, 100) + multiply_divide(multiply_divide(scaledlevel, r_ptr->dex, 100), ((m_ptr->level - 1) * 5), 100);
		m_ptr->mind = multiply_divide(scaledlevel, r_ptr->mind, 100) + multiply_divide(multiply_divide(scaledlevel, r_ptr->mind, 100), ((m_ptr->level - 1) * 5), 100);
		m_ptr->skill_attack = multiply_divide(scaledlevel, r_ptr->skill_attack, 100) + multiply_divide(multiply_divide(scaledlevel, r_ptr->skill_attack, 100), ((m_ptr->level - 1) * 5), 100);
		m_ptr->skill_ranged = multiply_divide(scaledlevel, r_ptr->skill_ranged, 100) + multiply_divide(multiply_divide(scaledlevel, r_ptr->skill_ranged, 100), ((m_ptr->level - 1) * 5), 100);
		m_ptr->skill_magic = multiply_divide(scaledlevel, r_ptr->skill_magic, 100) + multiply_divide(multiply_divide(scaledlevel, r_ptr->skill_magic, 100), ((m_ptr->level - 1) * 5), 100);
	}
	else
	{
		m_ptr->str = r_ptr->str + multiply_divide(r_ptr->str, ((m_ptr->level - 1) * 5), 100);
		m_ptr->dex = r_ptr->dex + multiply_divide(r_ptr->dex, ((m_ptr->level - 1) * 5), 100);
		m_ptr->mind = r_ptr->mind + multiply_divide(r_ptr->mind, ((m_ptr->level - 1) * 5), 100);
		m_ptr->skill_attack = r_ptr->skill_attack + multiply_divide(r_ptr->skill_attack, ((m_ptr->level - 1) * 5), 100);
		m_ptr->skill_ranged = r_ptr->skill_ranged + multiply_divide(r_ptr->skill_ranged, ((m_ptr->level - 1) * 5), 100);
		m_ptr->skill_magic = r_ptr->skill_magic + multiply_divide(r_ptr->skill_magic, ((m_ptr->level - 1) * 5), 100);
	}

	if (m_ptr->boss == 1)
	{
		m_ptr->str += multiply_divide(m_ptr->str, (m_ptr->level / 3), 100);
		m_ptr->dex += multiply_divide(m_ptr->dex, (m_ptr->level / 3), 100);
		m_ptr->mind += multiply_divide(m_ptr->mind, (m_ptr->level / 3), 100);
		m_ptr->skill_attack += multiply_divide(m_ptr->skill_attack, (m_ptr->level / 3), 100);
		m_ptr->skill_ranged += multiply_divide(m_ptr->skill_ranged, (m_ptr->level / 3), 100);
		m_ptr->skill_magic += multiply_divide(m_ptr->skill_magic, (m_ptr->level / 3), 100);
	}

	if (m_ptr->boss == 2 || r_ptr->cursed > 0)
	{
		m_ptr->str += multiply_divide(m_ptr->str, m_ptr->level + r_ptr->cursed, 100);
		m_ptr->dex += multiply_divide(m_ptr->dex, m_ptr->level + r_ptr->cursed, 100);
		m_ptr->mind += multiply_divide(m_ptr->mind, m_ptr->level + r_ptr->cursed, 100);
		m_ptr->skill_attack += multiply_divide(m_ptr->skill_attack, m_ptr->level + r_ptr->cursed, 100);
		m_ptr->skill_ranged += multiply_divide(m_ptr->skill_ranged, m_ptr->level + r_ptr->cursed, 100);
		m_ptr->skill_magic += multiply_divide(m_ptr->skill_magic, m_ptr->level + r_ptr->cursed, 100);
	}
	if (m_ptr->str > 2000000000 || m_ptr->str < 0) m_ptr->str = 2000000000;
	if (m_ptr->dex > 2000000000 || m_ptr->dex < 0) m_ptr->dex = 2000000000;
	if (m_ptr->mind > 2000000000 || m_ptr->mind < 0) m_ptr->mind = 2000000000;
	if (m_ptr->skill_attack > 2000000000 || m_ptr->skill_attack < 0) m_ptr->skill_attack = 2000000000;
	if (m_ptr->skill_ranged > 2000000000 || m_ptr->skill_ranged < 0) m_ptr->skill_ranged = 2000000000;
	if (m_ptr->skill_magic > 2000000000 || m_ptr->skill_magic < 0) m_ptr->skill_magic = 2000000000;

        /* Hit rate! */
        m_ptr->hitrate = ((m_ptr->dex - 5) * 10) + 2;
	m_ptr->hitrate += ((m_ptr->str - 5) / 2);
	m_ptr->hitrate += multiply_divide(m_ptr->hitrate, ((m_ptr->dex - 5) * 5), 100);

        /* Defense! */
	if (scaled) m_ptr->defense = multiply_divide(scaledlevel, r_ptr->ac, 100) + multiply_divide(r_ptr->ac, ((m_ptr->level - 1) * 5), 100);
	else m_ptr->defense = r_ptr->ac + multiply_divide(r_ptr->ac, ((m_ptr->level - 1) * 5), 100);

	/* Mana! */
	m_ptr->mana = (m_ptr->mind - 5) * 10;
	if (m_ptr->mana < 0) m_ptr->mana = 0;

	if (m_ptr->boss == 1) m_ptr->defense += multiply_divide(m_ptr->defense, (m_ptr->level * 5), 100);
	if (m_ptr->boss == 2 || r_ptr->cursed > 0) m_ptr->defense += multiply_divide(m_ptr->defense, (m_ptr->level * 10), 100);


        /* If the monster is an elite or a boss, give it an ability... */
        if (m_ptr->boss == 1) get_boss_ability(m_ptr, 1);
        if (m_ptr->boss == 2)
        {
                if (dun_level <= 10) get_boss_ability(m_ptr, 1);
                else get_boss_ability(m_ptr, (dun_level / 10));
        }

	/* Extract the monster base speed */
	m_ptr->mspeed = r_ptr->speed;

        /* Level will now boost the speed a little... */
        {
		int a, b;

		b = (m_ptr->level / 5);

		for (a = 0; a < b; a++)
		{
        		m_ptr->mspeed += 1;
			if (m_ptr->mspeed > 180)
			{
				m_ptr->mspeed = 180;
				break;
			}
		}
	}

	/* Give a random starting energy */
	/*m_ptr->energy = (byte)rand_int(100);*/
	m_ptr->energy = 100;

	/* Force monster to wait for player */
	if (r_ptr->flags1 & (RF1_FORCE_SLEEP))
	{
		/* Monster is still being nice */
		m_ptr->mflag |= (MFLAG_NICE);

		/* Must repair monsters */
		repair_monsters = TRUE;
	}

	/* Hack -- see "process_monsters()" */
	if (c_ptr->m_idx < hack_m_idx)
	{
		/* Monster is still being born */
		m_ptr->mflag |= (MFLAG_BORN);
	}


	/* Update the monster */
	update_mon(c_ptr->m_idx, TRUE);


	/* Hack -- Count the monsters on the level */
	r_ptr->cur_num++;

        /* Unique monsters on saved levels should be "marked" */
        if ((r_ptr->flags1 & RF1_UNIQUE) && get_dungeon_save(dummy))
        {
                r_ptr->on_saved = TRUE;
        }

	/* Hack -- Count the number of "reproducers" */
	if (r_ptr->flags2 & (RF2_MULTIPLY)) num_repro++;


	/* Hack -- Notice new multi-hued monsters */
	if (r_ptr->flags1 & (RF1_ATTR_MULTI)) shimmer_monsters = TRUE;

	/* Call a lua script. */
	if (r_ptr->event_spawn > 0)
	{
		call_lua("monster_spawn", "(dd)", "", c_ptr->m_idx, r_ptr->event_spawn);
	}

	/* Success */
        return c_ptr->m_idx;
}


/*
 * Maximum size of a group of monsters
 */
#define GROUP_MAX	32


/*
 * Attempt to place a "group" of monsters around the given location
 */
static bool place_monster_group(int y, int x, int r_idx, bool slp, bool charm, int dur)
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
	if (r_ptr->level > dun_level)
	{
		extra = r_ptr->level - dun_level;
		extra = 0 - randint(extra);
	}

	/* Easy monsters, large groups */
	else if (r_ptr->level < dun_level)
	{
		extra = dun_level - r_ptr->level;
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
			if (place_monster_one(my, mx, r_idx, slp, charm, dur))
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
static int place_monster_idx = 0;

/*
 * Hack -- help pick an escort type
 */
static bool place_monster_okay(int r_idx)
{
	monster_race *r_ptr = &r_info[place_monster_idx];

	monster_race *z_ptr = &r_info[r_idx];

	/* Hack - Escorts have to have the same dungeon flag */
	if (monster_dungeon(place_monster_idx) != monster_dungeon(r_idx)) return (FALSE);

	/* Require similar "race" */
	if (z_ptr->d_char != r_ptr->d_char) return (FALSE);

	/* Skip more advanced monsters */
	if (z_ptr->level > r_ptr->level) return (FALSE);

	/* Skip unique monsters */
	if (z_ptr->flags1 & (RF1_UNIQUE)) return (FALSE);

	/* Skip monsters with escorts. */
	if ((z_ptr->flags1 & (RF1_ESCORT)) || (z_ptr->flags1 & (RF1_ESCORTS))) return (FALSE);

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
bool place_monster_aux(int y, int x, int r_idx, bool slp, bool grp, bool charm, int dur)
{
	int             i;
	monster_race    *r_ptr = &r_info[r_idx];
	bool (*old_get_mon_num_hook)(int r_idx);

        /* Treat the Mirror Images differently... */
        if (r_idx == 1077) (void)place_monster_one_image(y, x, r_idx, slp, charm, dur);

	/* Place one monster, or fail */
        else if (!place_monster_one(y, x, r_idx, slp, charm, dur)) return (FALSE);

	/* Require the "group" flag */
	if (!grp) return (TRUE);


	/* Friends for certain monsters */
	if (r_ptr->flags1 & (RF1_FRIENDS))
	{
		/* Attempt to place a group */
        (void)place_monster_group(y, x, r_idx, slp, charm, dur);
	}


	/* Escorts for certain monsters */
	if (r_ptr->flags1 & (RF1_ESCORT))
	{
		old_get_mon_num_hook = get_mon_num_hook;

		/* Set the escort index */
		place_monster_idx = r_idx;

		/* Set the escort hook */
		get_mon_num_hook = place_monster_okay;

		/* Prepare allocation table */
		get_mon_num_prep();

		/* Try to place several "escorts" */
		for (i = 0; i < 50; i++)
		{
			int nx, ny, z, d = 3;

			/* Pick a location */
			scatter(&ny, &nx, y, x, d, 0);

			/* Require empty grids */
			if (!cave_empty_bold(ny, nx)) continue;

			set_mon_num2_hook(ny, nx);

			/* Prepare allocation table */
			get_mon_num_prep();

			/* Pick a random race */
			z = get_mon_num(r_ptr->level);

			/* Reset restriction */
			get_mon_num2_hook = NULL;

			/* Prepare allocation table */
			get_mon_num_prep();

			/* Handle failure */
			if (!z) break;

			/* Place a single escort */
                        (void)place_monster_one(ny, nx, z, slp, charm, dur);

			/* Place a "group" of escorts if needed */
			if ((r_info[z].flags1 & (RF1_FRIENDS)) ||
			    (r_ptr->flags1 & (RF1_ESCORTS)))
			{
				/* Place a group of monsters */
				(void)place_monster_group(ny, nx, z, slp, charm, dur);
			}
		}

		/* Reset restriction */
		get_mon_num_hook = old_get_mon_num_hook;

		/* Prepare allocation table */
		get_mon_num_prep();
	}

	/* Success */
	return (TRUE);
}


/*
 * Hack -- attempt to place a monster at the given location
 *
 * Attempt to find a monster appropriate to the "monster_level"
 */
bool place_monster(int y, int x, bool slp, bool grp, int dur)
{
	int r_idx;

	/* Set monster restriction */
	set_mon_num2_hook(y, x);

	/* Prepare allocation table */
	get_mon_num_prep();

	/* Pick a monster */
        r_idx = get_mon_num(monster_level);

	/* Reset restriction */
	get_mon_num2_hook = NULL;

	/* Prepare allocation table */
	get_mon_num_prep();

	/* Handle failure */
	if (!r_idx) return (FALSE);

	/* Attempt to place the monster */
	if (place_monster_aux(y, x, r_idx, slp, grp, FALSE, dur)) return (TRUE);

	/* Oops */
	return (FALSE);
}



/*
 * XXX XXX XXX Player Ghosts are such a hack, they have been completely
 * removed until Angband 2.8.0, in which there will actually be a small
 * number of "unique" monsters which will serve as the "player ghosts".
 * Each will have a place holder for the "name" of a deceased player,
 * which will be extracted from a "bone" file, or replaced with a
 * "default" name if a real name is not available.  Each ghost will
 * appear exactly once and will not induce a special feeling.
 *
 * Possible methods:
 *   (s) 1 Skeleton
 *   (z) 1 Zombie
 *   (M) 1 Mummy
 *   (G) 1 Polterguiest, 1 Spirit, 1 Ghost, 1 Shadow, 1 Phantom
 *   (W) 1 Wraith
 *   (V) 1 Vampire, 1 Vampire Lord
 *   (L) 1 Lich
 *
 * Possible change: Lose 1 ghost, Add "Master Lich"
 *
 * Possible change: Lose 2 ghosts, Add "Wraith", Add "Master Lich"
 *
 * Possible change: Lose 4 ghosts, lose 1 vampire lord
 *
 * Note that ghosts should never sleep, should be very attentive, should
 * have maximal hitpoints, drop only good (or great) items, should be
 * cold blooded, evil, undead, immune to poison, sleep, confusion, fear.
 *
 * Base monsters:
 *   Skeleton
 *   Zombie
 *   Mummy
 *   Poltergeist
 *   Spirit
 *   Ghost
 *   Vampire
 *   Wraith
 *   Vampire Lord
 *   Shadow
 *   Phantom
 *   Lich
 *
 * This routine will simply extract ghost names from files, and
 * attempt to allocate a player ghost somewhere in the dungeon,
 * note that normal allocation may also attempt to place ghosts,
 * so we must work with some form of default names.
 *
 * XXX XXX XXX
 */



#ifdef MONSTER_HORDES

bool alloc_horde(int y, int x)
{
        int r_idx = 0;
        monster_race * r_ptr;
	monster_type * m_ptr;
	int attempts = 1000;

	set_mon_num2_hook(y, x);

	/* Prepare allocation table */
	get_mon_num_prep();

	while (--attempts)
	{
		/* Pick a monster */
		r_idx = get_mon_num(monster_level);

		/* Handle failure */
		if (!r_idx) return (FALSE);

		r_ptr = &r_info[r_idx];

		if (!(r_ptr->flags1 & (RF1_UNIQUE))
		 && !(r_ptr->flags1 & (RF1_ESCORTS)))
			break;
	}

	get_mon_num2_hook = NULL;

	/* Prepare allocation table */
	get_mon_num_prep();

	if (attempts < 1) return FALSE;

	attempts = 1000;

	while (--attempts)
	{
		/* Attempt to place the monster */
		if (place_monster_aux(y, x, r_idx, FALSE, FALSE, FALSE, 0)) break;
	}

	if (attempts < 1) return FALSE;


	m_ptr = &m_list[hack_m_idx_ii];

	summon_kin_type = r_ptr->d_char;

	for (attempts = randint(10) + 5; attempts; attempts--)
	{
		summon_specific_kind(m_ptr->fy, m_ptr->fx, r_ptr->level, r_ptr->d_char, FALSE, FALSE, 0);
	}

	return TRUE;
}

#endif /* MONSTER_HORDES */

/*
 * Attempt to allocate a random monster in the dungeon.
 *
 * Place the monster at least "dis" distance from the player.
 *
 * Use "slp" to choose the initial "sleep" status
 *
 * Use "monster_level" for the monster level
 */
bool alloc_monster(int dis, bool slp)
{
	int			y, x;
	int         attempts_left = 10000;

	/* Find a legal, distant, unoccupied, space */
	while (attempts_left)
	{
		/* Pick a location */
		y = rand_int(cur_hgt);
		x = rand_int(cur_wid);

		/* Require empty floor grid (was "naked") */
		if (!cave_empty_bold(y, x)) continue;

		/* Accept far away grids */
		if (distance(y, x, py, px) > dis) break;

		attempts_left--;
	}

	if (!attempts_left)
	{
                if (cheat_xtra || cheat_hear)
		{
			msg_print("Warning! Could not allocate a new monster. Small level?");
		}

		return (FALSE);
	}


#ifdef MONSTER_HORDES
	if (randint(5000) <= dun_level)
	{
		if (alloc_horde(y, x))
		{
                        if ((cheat_hear)) msg_print("Monster horde.");
			return (TRUE);
		}
	}
	else
	{
#endif /* MONSTER_HORDES */

		/* Attempt to place the monster, allow groups */
		if (place_monster(y, x, slp, TRUE, 0)) return (TRUE);

#ifdef MONSTER_HORDES
	}
#endif /* MONSTER_HORDES */

	/* Nope */
	return (FALSE);
}




/*
 * Hack -- the "type" of the current "summon specific"
 */
static int summon_specific_type = 0;

/*
 * Hack -- The monste that summons
 */
monster_type *summoner_monster = NULL;


/*
 * Hack -- help decide if a monster race is "okay" to summon
 */
static bool summon_specific_okay(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Hack - Only summon dungeon monsters */
	if (!monster_dungeon(r_idx)) return (FALSE);

	/* Result */
	return (TRUE);
}


/*
 * Place a monster (of the specified "type") near the given
 * location.  Return TRUE if a monster was actually summoned.
 *
 * We will attempt to place the monster up to 10 times before giving up.
 *
 * Note: SUMMON_UNIQUE and SUMMON_WRAITH (XXX) will summon Unique's
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
bool summon_specific(int y1, int x1, int lev, int type, int dur)
{
	int i, x, y, r_idx;
	bool Group_ok = TRUE;
	bool (*old_get_mon_num_hook)(int r_idx);

        summoner_monster = NULL;

	/* Look for a location */
	for (i = 0; i < 20; ++i)
	{
		/* Pick a distance */
		int d = (i / 15) + 1;

		/* Pick a location */
		scatter(&y, &x, y1, x1, d, 0);

		/* Require "empty" floor grid */
		if (!cave_empty_bold(y, x)) continue;

		/* Hack -- no summon on glyph of warding */
		if (cave[y][x].feat == FEAT_GLYPH) continue;
		if (cave[y][x].feat == FEAT_MINOR_GLYPH) continue;

                /* Nor on the between */
                if (cave[y][x].feat == FEAT_BETWEEN) return (FALSE);

		/* ... nor on the Pattern */
		if ((cave[y][x].feat >= FEAT_PATTERN_START) &&
		    (cave[y][x].feat <= FEAT_PATTERN_XTRA2))
			continue;

		/* Okay */
		break;
	}

	/* Failure */
	if (i == 20) return (FALSE);


	/* Save the "summon" type */
	summon_specific_type = type;

	/* Backup the old hook */
	old_get_mon_num_hook = get_mon_num_hook;

	/* Require "okay" monsters */
	get_mon_num_hook = summon_specific_okay;

	/* Prepare allocation table */
	get_mon_num_prep();


	/* Pick a monster, using the level calculation */
	r_idx = get_mon_num((dun_level + lev) / 2 + 5);

#ifdef R_IDX_TESTING_HACK
	r_idx = 356;
#endif

	/* Reset restriction */
	get_mon_num_hook = old_get_mon_num_hook;

	/* Prepare allocation table */
	get_mon_num_prep();


	/* Handle failure */
	if (!r_idx) return (FALSE);

	/* Attempt to place the monster (awake, allow groups) */
	if (!place_monster_aux(y, x, r_idx, FALSE, Group_ok, FALSE, 0)) return (FALSE);


	/* Success */
	return (TRUE);
}



bool summon_specific_friendly(int y1, int x1, int lev, int type, bool Group_ok, int dur)
{
	int i, x, y, r_idx;
	bool (*old_get_mon_num_hook)(int r_idx);

        summoner_monster = NULL;

	/* Look for a location */
	for (i = 0; i < 20; ++i)
	{
		/* Pick a distance */
		int d = (i / 15) + 1;

		/* Pick a location */
		scatter(&y, &x, y1, x1, d, 0);

		/* Require "empty" floor grid */
		if (!cave_empty_bold(y, x)) continue;

		/* Hack -- no summon on glyph of warding */
		 if (cave[y][x].feat == FEAT_GLYPH) continue;
		 if (cave[y][x].feat == FEAT_MINOR_GLYPH) continue;

                /* Nor on the between */
                if (cave[y][x].feat == FEAT_BETWEEN) return (FALSE);

		/* ... nor on the Pattern */
		if ((cave[y][x].feat >= FEAT_PATTERN_START) &&
		    (cave[y][x].feat <= FEAT_PATTERN_XTRA2))
			continue;

		/* Okay */
		break;
	}

	/* Failure */
	if (i == 20) return (FALSE);

	old_get_mon_num_hook = get_mon_num_hook;

	/* Save the "summon" type */
	summon_specific_type = type;

	/* Require "okay" monsters */
	get_mon_num_hook = summon_specific_okay;

	/* Prepare allocation table */
	get_mon_num_prep();

	/* Pick a monster, using the level calculation */
        r_idx = get_mon_num((dun_level + lev) / 2 + 5);

#ifdef R_IDX_TESTING_HACK
	r_idx = 356;
#endif
	/* Reset restriction */
	get_mon_num_hook = old_get_mon_num_hook;

	/* Prepare allocation table */
	get_mon_num_prep();

	/* Handle failure */
	if (!r_idx) return (FALSE);

	/* Attempt to place the monster (awake, allow groups) */
	if (!place_monster_aux(y, x, r_idx, FALSE, Group_ok, TRUE, dur)) return (FALSE);

	/* Success */
	return (TRUE);
}


/*
 * Swap the players/monsters (if any) at two locations XXX XXX XXX
 */
void monster_swap(int y1, int x1, int y2, int x2)
{
	int m1, m2;

	monster_type *m_ptr;
        cave_type *c_ptr1,*c_ptr2;

        c_ptr1 = &cave[y1][x1];
        c_ptr2 = &cave[y2][x2];

	/* Monsters */
        m1 = c_ptr1->m_idx;
        m2 = c_ptr2->m_idx;


	/* Update grids */
        c_ptr1->m_idx = m2;
        c_ptr2->m_idx = m1;


	/* Monster 1 */
	if (m1 > 0)
	{
		m_ptr = &m_list[m1];

		/* Move monster */
		m_ptr->fy = y2;
		m_ptr->fx = x2;

		/* Update monster */
		update_mon(m1, TRUE);
	}

	/* Player 1 */
	else if (m1 < 0)
	{
		/* Move player */
                py = y2;
                px = x2;

		/* Check for new panel (redraw map) */
		verify_panel();

		/* Update stuff */
		p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW);

		/* Update the monsters */
		p_ptr->update |= (PU_DISTANCE);

		/* Window stuff */
		/* It's probably not a good idea to recalculate the
		 * overhead view each turn.

		 p_ptr->window |= (PW_OVERHEAD);
		*/
	}

	/* Monster 2 */
	if (m2 > 0)
	{
		m_ptr = &m_list[m2];

		/* Move monster */
		m_ptr->fy = y1;
		m_ptr->fx = x1;

		/* Update monster */
		update_mon(m2, TRUE);
	}

	/* Player 2 */
	else if (m2 > 0)
	{
		/* Move player */
                py = y1;
                px = x1;

		/* Check for new panel (redraw map) */
		verify_panel();

		/* Update stuff */
		p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW);

		/* Update the monsters */
		p_ptr->update |= (PU_DISTANCE);

		/* Window stuff */
		/* It's probably not a good idea to recalculate the
		 * overhead view each turn.

		 p_ptr->window |= (PW_OVERHEAD);
		*/
	}


	/* Redraw */
	lite_spot(y1, x1);
	lite_spot(y2, x2);
}


/*
 * Hack -- help decide if a monster race is "okay" to summon
 */
static bool mutate_monster_okay(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	bool okay = FALSE;

	/* Hack - Only summon dungeon monsters */
	if (!monster_dungeon(r_idx)) return (FALSE);

        okay = ((r_ptr->d_char == summon_kin_type) && !(r_ptr->flags1 & (RF1_UNIQUE))
                && (r_ptr->level >= dun_level));

        return okay;
}


/*
 * Let the given monster attempt to reproduce.
 *
 * Note that "reproduction" REQUIRES empty space.
 */
bool multiply_monster(int m_idx, bool charm, bool clone)
{
	monster_type	*m_ptr = &m_list[m_idx];
        monster_race    *r_ptr = &r_info[m_ptr->r_idx];

        int                     i, y, x, new_race;

	bool result = FALSE;

        if(no_breeds)
        {
                msg_print("It tries to breed but he fails!");
                return FALSE;
        }

	/* Try up to 18 times */
	for (i = 0; i < 18; i++)
	{
		int d = 1;


		/* Pick a location */
		scatter(&y, &x, m_ptr->fy, m_ptr->fx, d, 0);

		/* Require an "empty" floor grid */
		if (!cave_empty_bold(y, x)) continue;

                new_race = m_ptr->r_idx;

                /* It can mutate into a nastier monster */
                if((rand_int(100)<7) && (!clone))
                {
                        bool (*old_get_mon_num_hook)(int r_idx);

                        /* Backup the old hook */
                        old_get_mon_num_hook = get_mon_num_hook;

                        /* Require "okay" monsters */
                        get_mon_num_hook = mutate_monster_okay;

                        /* Prepare allocation table */
                        get_mon_num_prep();

                        summon_kin_type = r_ptr->d_char;

                        /* Pick a monster, using the level calculation */
                        new_race = get_mon_num(dun_level + 5);

                        /* Reset restriction */
                        get_mon_num_hook = old_get_mon_num_hook;

                        /* Prepare allocation table */
                        get_mon_num_prep();
                }

		/* Create a new monster (awake, no groups) */
                result = place_monster_aux(y, x, new_race, FALSE, FALSE, charm, -1);

		/* Done */
		break;
	}

	if (clone && result) m_list[hack_m_idx_ii].smart |= SM_CLONED;

	/* Result */
	return (result);
}





/*
 * Dump a message describing a monster's reaction to damage
 *
 * Technically should attempt to treat "Beholder"'s as jelly's
 */
void message_pain(int m_idx, s32b dam)
{
	long            oldhp, newhp, tmp;
	int             percentage;
	monster_type    *m_ptr = &m_list[m_idx];
	monster_race    *r_ptr = &r_info[m_ptr->r_idx];
	char            m_name[80];

#if 0
	if (!(player_can_see_bold(m_ptr->fy, m_ptr->fx)))
		return;
#endif

	/* Get the monster name */
	monster_desc(m_name, m_ptr, 0);

	/* Notice non-damage */
	if (dam == 0)
	{
		msg_format("%^s is unharmed.", m_name);
		return;
	}

	/* Note -- subtle fix -CFT */
	newhp = (long)(m_ptr->hp);
	oldhp = newhp + (long)(dam);
	tmp = (newhp * 100L) / oldhp;
	percentage = (int)(tmp);


	/* Jelly's, Mold's, Vortex's, Quthl's */
	if (strchr("jmvQ", r_ptr->d_char))
	{
		if (percentage > 95)
			msg_format("%^s barely notices.", m_name);
		else if (percentage > 75)
			msg_format("%^s flinches.", m_name);
		else if (percentage > 50)
			msg_format("%^s squelches.", m_name);
		else if (percentage > 35)
			msg_format("%^s quivers in pain.", m_name);
		else if (percentage > 20)
			msg_format("%^s writhes about.", m_name);
		else if (percentage > 10)
			msg_format("%^s writhes in agony.", m_name);
		else
			msg_format("%^s jerks limply.", m_name);
	}

	/* Dogs and Hounds */
	else if (strchr("CZ", r_ptr->d_char))
	{
		if (percentage > 95)
			msg_format("%^s shrugs off the attack.", m_name);
		else if (percentage > 75)
			msg_format("%^s snarls with pain.", m_name);
		else if (percentage > 50)
			msg_format("%^s yelps in pain.", m_name);
		else if (percentage > 35)
			msg_format("%^s howls in pain.", m_name);
		else if (percentage > 20)
			msg_format("%^s howls in agony.", m_name);
		else if (percentage > 10)
			msg_format("%^s writhes in agony.", m_name);
		else
			msg_format("%^s yelps feebly.", m_name);
	}

	/* One type of monsters (ignore,squeal,shriek) */
	else if (strchr("FIKMRSXabclqrst", r_ptr->d_char))
	{
		if (percentage > 95)
			msg_format("%^s ignores the attack.", m_name);
		else if (percentage > 75)
			msg_format("%^s grunts with pain.", m_name);
		else if (percentage > 50)
			msg_format("%^s squeals in pain.", m_name);
		else if (percentage > 35)
			msg_format("%^s shrieks in pain.", m_name);
		else if (percentage > 20)
			msg_format("%^s shrieks in agony.", m_name);
		else if (percentage > 10)
			msg_format("%^s writhes in agony.", m_name);
		else
			msg_format("%^s cries out feebly.", m_name);
	}

	/* Another type of monsters (shrug,cry,scream) */
	else
	{
		if (percentage > 95)
			msg_format("%^s shrugs off the attack.", m_name);
		else if (percentage > 75)
			msg_format("%^s grunts with pain.", m_name);
		else if (percentage > 50)
			msg_format("%^s cries out in pain.", m_name);
		else if (percentage > 35)
			msg_format("%^s screams in pain.", m_name);
		else if (percentage > 20)
			msg_format("%^s screams in agony.", m_name);
		else if (percentage > 10)
			msg_format("%^s writhes in agony.", m_name);
		else
			msg_format("%^s cries out feebly.", m_name);
	}
}



/*
 * Learn about an "observed" resistance.
 */
void update_smart_learn(int m_idx, int what)
{
#ifdef DRS_SMART_OPTIONS

	monster_type *m_ptr = &m_list[m_idx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];


	/* Not allowed to learn */
	if (!smart_learn) return;

	/* Too stupid to learn anything */
	if (r_ptr->flags2 & (RF2_STUPID)) return;

	/* Not intelligent, only learn sometimes */
	if (!(r_ptr->flags2 & (RF2_SMART)) && (rand_int(100) < 50)) return;


	/* XXX XXX XXX */

	/* Analyze the knowledge */
	/*switch (what)
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
	}*/

#endif /* DRS_SMART_OPTIONS */

}


/*
 * Place the player in the dungeon XXX XXX
 */
s16b player_place(int y, int x)
{
	/* Paranoia XXX XXX */
	if (cave[y][x].m_idx != 0) return (0);

	/* Save player location */
	py = y;
	px = x;

	/* Success */
	return (-1);
}

/*
 * Drop all items carried by a monster
 */
void monster_drop_carried_objects(monster_type *m_ptr)
{
	s16b this_o_idx, next_o_idx = 0;
	object_type forge;
	object_type *o_ptr;
	object_type *q_ptr;


	/* Drop objects being carried */
	for (this_o_idx = m_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		/* Acquire object */
		o_ptr = &o_list[this_o_idx];

		/* Acquire next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Paranoia */
		o_ptr->held_m_idx = 0;

		/* Get local object */
		q_ptr = &forge;

		/* Copy the object */
		object_copy(q_ptr, o_ptr);

		/* Delete the object */
		delete_object_idx(this_o_idx);

		/* Drop it */
		drop_near(q_ptr, -1, m_ptr->fy, m_ptr->fx);
	}

	/* Forget objects */
	m_ptr->hold_o_idx = 0;
}      

void apply_monster_level_hp(monster_type *m_ptr)
{
	bool scaled = FALSE;
	int scaledlevel = 0;
        monster_race    *r_ptr = &r_info[m_ptr->r_idx];

	if (r_ptr->flags7 & (RF7_SCALED))
	{
		scaled = TRUE;
		if (p_ptr->lev > r_ptr->level) scaledlevel = p_ptr->lev;
		else scaledlevel = r_ptr->level;
	}


	/* Determine the monster's hp. */
	{
		int a;
		s32b hpperlevels;

		if (r_ptr->flags1 & (RF1_FORCE_MAXHP))
		{
			if (scaled) hpperlevels = maxroll(r_ptr->hdice * scaledlevel, r_ptr->hside * scaledlevel);
			else hpperlevels = maxroll(r_ptr->hdice, r_ptr->hside);
		}
		else
		{
			if (scaled) hpperlevels = damroll(r_ptr->hdice * scaledlevel, r_ptr->hside * scaledlevel);
			else hpperlevels = damroll(r_ptr->hdice, r_ptr->hside);
		}

		hpperlevels += multiply_divide(hpperlevels, (m_ptr->level * 5), 100);

		/* Three times more hp for elites... */
        	if (m_ptr->boss == 1) hpperlevels *= 3;
        	/* SIX TIMES more hp for bosses!!! */
        	if (m_ptr->boss == 2 || r_ptr->cursed > 0) hpperlevels *= 6;

		/* Assume 0 max hp. */
		m_ptr->maxhp = 0;

		for (a = 0; a < m_ptr->level; a++)
		{
        		m_ptr->maxhp += hpperlevels;
			if (m_ptr->maxhp > 2000000000 || m_ptr->maxhp < 0)
			{
				m_ptr->maxhp = 2000000000;
				break;
			}
		}
	}

	/* And start out fully healthy */
	m_ptr->hp = m_ptr->maxhp;

	/* Now, determine the stats! */
	if (scaled)
	{
		m_ptr->str = multiply_divide(scaledlevel, r_ptr->str, 100) + multiply_divide(multiply_divide(scaledlevel, r_ptr->str, 100), ((m_ptr->level - 1) * 5), 100);
		m_ptr->dex = multiply_divide(scaledlevel, r_ptr->dex, 100) + multiply_divide(multiply_divide(scaledlevel, r_ptr->dex, 100), ((m_ptr->level - 1) * 5), 100);
		m_ptr->mind = multiply_divide(scaledlevel, r_ptr->mind, 100) + multiply_divide(multiply_divide(scaledlevel, r_ptr->mind, 100), ((m_ptr->level - 1) * 5), 100);
		m_ptr->skill_attack = multiply_divide(scaledlevel, r_ptr->skill_attack, 100) + multiply_divide(multiply_divide(scaledlevel, r_ptr->skill_attack, 100), ((m_ptr->level - 1) * 5), 100);
		m_ptr->skill_ranged = multiply_divide(scaledlevel, r_ptr->skill_ranged, 100) + multiply_divide(multiply_divide(scaledlevel, r_ptr->skill_ranged, 100), ((m_ptr->level - 1) * 5), 100);
		m_ptr->skill_magic = multiply_divide(scaledlevel, r_ptr->skill_magic, 100) + multiply_divide(multiply_divide(scaledlevel, r_ptr->skill_magic, 100), ((m_ptr->level - 1) * 5), 100);
	}
	else
	{
		m_ptr->str = r_ptr->str + multiply_divide(r_ptr->str, ((m_ptr->level - 1) * 5), 100);
		m_ptr->dex = r_ptr->dex + multiply_divide(r_ptr->dex, ((m_ptr->level - 1) * 5), 100);
		m_ptr->mind = r_ptr->mind + multiply_divide(r_ptr->mind, ((m_ptr->level - 1) * 5), 100);
		m_ptr->skill_attack = r_ptr->skill_attack + multiply_divide(r_ptr->skill_attack, ((m_ptr->level - 1) * 5), 100);
		m_ptr->skill_ranged = r_ptr->skill_ranged + multiply_divide(r_ptr->skill_ranged, ((m_ptr->level - 1) * 5), 100);
		m_ptr->skill_magic = r_ptr->skill_magic + multiply_divide(r_ptr->skill_magic, ((m_ptr->level - 1) * 5), 100);
	}

	if (m_ptr->boss == 1)
	{
		m_ptr->str += multiply_divide(m_ptr->str, (m_ptr->level / 3), 100);
		m_ptr->dex += multiply_divide(m_ptr->dex, (m_ptr->level / 3), 100);
		m_ptr->mind += multiply_divide(m_ptr->mind, (m_ptr->level / 3), 100);
		m_ptr->skill_attack += multiply_divide(m_ptr->skill_attack, (m_ptr->level / 3), 100);
		m_ptr->skill_ranged += multiply_divide(m_ptr->skill_ranged, (m_ptr->level / 3), 100);
		m_ptr->skill_magic += multiply_divide(m_ptr->skill_magic, (m_ptr->level / 3), 100);
	}

	if (m_ptr->boss == 2 || r_ptr->cursed > 0)
	{
		m_ptr->str += multiply_divide(m_ptr->str, m_ptr->level + r_ptr->cursed, 100);
		m_ptr->dex += multiply_divide(m_ptr->dex, m_ptr->level + r_ptr->cursed, 100);
		m_ptr->mind += multiply_divide(m_ptr->mind, m_ptr->level + r_ptr->cursed, 100);
		m_ptr->skill_attack += multiply_divide(m_ptr->skill_attack, m_ptr->level + r_ptr->cursed, 100);
		m_ptr->skill_ranged += multiply_divide(m_ptr->skill_ranged, m_ptr->level + r_ptr->cursed, 100);
		m_ptr->skill_magic += multiply_divide(m_ptr->skill_magic, m_ptr->level + r_ptr->cursed, 100);
	}
	if (m_ptr->str > 2000000000 || m_ptr->str < 0) m_ptr->str = 2000000000;
	if (m_ptr->dex > 2000000000 || m_ptr->dex < 0) m_ptr->dex = 2000000000;
	if (m_ptr->mind > 2000000000 || m_ptr->mind < 0) m_ptr->mind = 2000000000;
	if (m_ptr->skill_attack > 2000000000 || m_ptr->skill_attack < 0) m_ptr->skill_attack = 2000000000;
	if (m_ptr->skill_ranged > 2000000000 || m_ptr->skill_ranged < 0) m_ptr->skill_ranged = 2000000000;
	if (m_ptr->skill_magic > 2000000000 || m_ptr->skill_magic < 0) m_ptr->skill_magic = 2000000000;

        /* Hit rate! */
        m_ptr->hitrate = ((m_ptr->dex - 5) * 10) + 2;
	m_ptr->hitrate += ((m_ptr->str - 5) / 2);
	m_ptr->hitrate += multiply_divide(m_ptr->hitrate, ((m_ptr->dex - 5) * 5), 100);

        /* Defense! */
	if (scaled) m_ptr->defense = multiply_divide(scaledlevel, r_ptr->ac, 100) + multiply_divide(r_ptr->ac, ((m_ptr->level - 1) * 5), 100);
	else m_ptr->defense = r_ptr->ac + multiply_divide(r_ptr->ac, ((m_ptr->level - 1) * 5), 100);

	/* Mana! */
	m_ptr->mana = (m_ptr->mind - 5) * 10;
	if (m_ptr->mana < 0) m_ptr->mana = 0;

	if (m_ptr->boss == 1) m_ptr->defense += multiply_divide(m_ptr->defense, (m_ptr->level * 5), 100);
	if (m_ptr->boss == 2 || r_ptr->cursed > 0) m_ptr->defense += multiply_divide(m_ptr->defense, (m_ptr->level * 10), 100);
}

void get_boss_ability(monster_type *m_ptr, int number)
{
        while(number > 0)
        {
                int tempint;
		monster_race *r_ptr = &r_info[m_ptr->r_idx];
                tempint = randint(80);
                if (tempint >= 70 && !(m_ptr->abilities & (BOSS_IMMUNE_MAGIC)))
                {
                        m_ptr->abilities |= (BOSS_IMMUNE_WEAPONS);
                }
                else if (tempint >= 60 && !(m_ptr->abilities & (BOSS_IMMUNE_WEAPONS)) && !(r_ptr->resistances[GF_PHYSICAL] == 100))
                {
                        m_ptr->abilities |= (BOSS_IMMUNE_MAGIC);
                }
                else if (tempint >= 50)
                {
                        m_ptr->abilities |= (BOSS_DOUBLE_DAMAGES);
                }
                else if (tempint >= 40)
                {
                        m_ptr->abilities |= (BOSS_HALVE_DAMAGES);
                }
                else if (tempint >= 30)
                {
                        m_ptr->abilities |= (BOSS_CURSED_HITS);
                }
                else if (tempint >= 20)
                {
                        m_ptr->abilities |= (BOSS_DOUBLE_MAGIC);
                }
                else if (tempint >= 10)
                {
                        m_ptr->abilities |= (BOSS_RETURNING);
                }
                else
                {
                        m_ptr->abilities |= (BOSS_MAGIC_RETURNING);
                }
                number -= 1;
        }
}

/* Place a monster, but not an elite, nor a boss. Used by Leaders! */
bool place_monster_one_no_boss(int y, int x, int r_idx, bool slp, bool charm, int dur)
{
        int             i, tempint;

        char            dummy[5];

	cave_type		*c_ptr;

	monster_type	*m_ptr;

	monster_race	*r_ptr = &r_info[r_idx];

	cptr		name = (r_name + r_ptr->name);

	/* Verify location */
	if (!in_bounds(y, x)) return (FALSE);

	/* Require empty space */
	if (!cave_empty_bold(y, x)) return (FALSE);

	/* Hack -- no creation on glyph of warding */
	if (cave[y][x].feat == FEAT_GLYPH) return (FALSE);
	if (cave[y][x].feat == FEAT_MINOR_GLYPH) return (FALSE);

        /* Nor on the between */
        if (cave[y][x].feat == FEAT_BETWEEN) return (FALSE);

        /* Nor on the altars */
        if ((cave[y][x].feat >= FEAT_ALTAR_HEAD)
         && (cave[y][x].feat <= FEAT_ALTAR_TAIL))
                return (FALSE);

	/* Nor on the Pattern */
	if ((cave[y][x].feat >= FEAT_PATTERN_START)
	 && (cave[y][x].feat <= FEAT_PATTERN_XTRA2))
		return (FALSE);

	/* Paranoia */
	if (!r_idx) return (FALSE);

	/* Paranoia */
	if (!r_ptr->name) return (FALSE);

#if 0
        /* No Uniques on saved levels(this would cause too much confusion) */
        if((d_info[dungeon_type].flags1 & DF1_PERSISTENT) && (r_ptr->flags1 & RF1_UNIQUE)) return (FALSE);
#endif

        /* Don't allow undefined ghosts */
        if ((r_idx >= GHOST_R_IDX_HEAD)&&(r_idx <= GHOST_R_IDX_TAIL))
                if(ghost_file[r_idx - GHOST_R_IDX_HEAD][0] == 0)
                        return FALSE;

	if (!monster_can_cross_terrain(cave[y][x].feat, r_ptr))
	{
		return FALSE;
	}

        /* Unallow some uniques to be generated outside of their quests/special levels/dungeons */
        if ((r_ptr->flags9 & RF9_SPECIAL_GENE) && !hack_allow_special && !summoning_specific)
        {
                return FALSE;
        }

	/* Random monsters should only be placed in random dungeons. */
	if ((r_ptr->flags7 & (RF7_RANDOM)) && !(d_info[dungeon_type].flags1 & DF1_RANDOM_ONLY)) return FALSE;

	/* Ice monsters should only be placed in ice dungeons. */
	if ((r_ptr->flags7 & (RF7_ICE)) && !(d_info[dungeon_type].flags1 & DF1_ICE)) return FALSE;

	/* Cursed monsters cannot appear unless the player is cursed enough. */
	if (r_ptr->cursed > 0)
	{
		if ((p_ptr->cursed < r_ptr->cursed) || (p_ptr->inside_quest) || dun_level == 0 || (charm))
		{
			return FALSE;
		}
	}

	/* Hack -- "unique" monsters must be "unique" */
        if ((r_ptr->flags1 & (RF1_UNIQUE)) && (r_ptr->max_num == -1))
	{
		/* Cannot create */
		return (FALSE);
	}

        /* The monster is already on an unique level */
        if (r_ptr->on_saved) return (FALSE);

	/* Hack -- "unique" monsters must be "unique" */
	if ((r_ptr->flags1 & (RF1_UNIQUE)) && (r_ptr->cur_num >= r_ptr->max_num))
	{
		/* Cannot create */
		return (FALSE);
	}

	/* Depth monsters may NOT be created out of depth */
	if ((r_ptr->flags1 & (RF1_FORCE_DEPTH)) && (dun_level < r_ptr->level))
	{
		/* Cannot create */
		return (FALSE);
	}


	/* Powerful monster */
	if (r_ptr->level > dun_level)
	{
		/* Unique monsters */
		if (r_ptr->flags1 & (RF1_UNIQUE))
		{
			/* Message for cheaters */
                        if ((cheat_hear)) msg_format("Deep Unique (%s).", name);

			/* Boost rating by twice delta-depth */
			rating += (r_ptr->level - dun_level) * 2;
		}

		/* Normal monsters */
		else
		{
			/* Message for cheaters */
                        if ((cheat_hear)) msg_format("Deep Monster (%s).", name);

			/* Boost rating by delta-depth */
			rating += (r_ptr->level - dun_level);
		}
	}

	/* Note the monster */
	else if (r_ptr->flags1 & (RF1_UNIQUE))
	{
		/* Unique monsters induce message */
                if ((cheat_hear)) msg_format("Unique (%s).", name);
	}


	/* Access the location */
	c_ptr = &cave[y][x];

	/* Make a new monster */
        c_ptr->m_idx = m_pop();
	hack_m_idx_ii = c_ptr->m_idx;

	/* Mega-Hack -- catch "failure" */
	if (!c_ptr->m_idx) return (FALSE);


	/* Get a new monster record */
	m_ptr = &m_list[c_ptr->m_idx];

	/* Save the race */
	m_ptr->r_idx = r_idx;

	/* Place the monster at the location */
	m_ptr->fy = y;
	m_ptr->fx = x;


	/* No "damage" yet */
	m_ptr->stunned = 0;
	m_ptr->confused = 0;
	m_ptr->monfear = 0;

        /* Not a boss and no abilities yet... */
        m_ptr->boss = 0;

        /* Not hit by Sealing Light */
        m_ptr->seallight = 0;

	/* Not hasted nor boosted */
	m_ptr->hasted = 0;
	m_ptr->boosted = 0;

	/* Not summoned */
	m_ptr->summoned = 0;

	/* No lives */
	m_ptr->lives = 0;

	/* Worth experience. */
	m_ptr->no_experience = FALSE;

        /* No objects yet */
        m_ptr->hold_o_idx = 0;

        /* Not an animated monster...never will. */
        m_ptr->animated = FALSE;
        m_ptr->animdam_d = 0;
        m_ptr->animdam_s = 0;

	/* Friendly? */
        if ( charm || (r_ptr->flags7 & RF7_PET) )
	{
		set_pet(m_ptr, TRUE);                
	}

        /* NOT a boss! */
        m_ptr->boss = 0;

	/* Not hasted nor boosted */
	m_ptr->hasted = 0;
	m_ptr->boosted = 0;

	/* Assume no sleeping */
	m_ptr->csleep = 0;

	/* Enforce sleeping if needed */
        if (slp && r_ptr->sleep && m_ptr->boss == 0)
	{
		int val = r_ptr->sleep;
		m_ptr->csleep = ((val * 2) + randint(val * 10));
	}

        /* Generate the monster's inventory(if any) */
        {
                bool good = (r_ptr->flags1 & (RF1_DROP_GOOD)) ? TRUE : FALSE;
                bool great = (r_ptr->flags1 & (RF1_DROP_GREAT)) ? TRUE : FALSE;

                bool do_gold = (!(r_ptr->flags1 & (RF1_ONLY_ITEM)));
                bool do_item = (!(r_ptr->flags1 & (RF1_ONLY_GOLD)));

                int j;

                int force_coin = get_coin_type(r_ptr);

                int dump_item = 0;
                int dump_gold = 0;
                object_type forge;
                object_type *q_ptr;

                int number = 0;

                if (m_ptr->boss == 1)
                {
                        number += 1;
                        good = TRUE;
                        great = TRUE;
                }
                if (m_ptr->boss == 2)
                {
                        number += 4;
                        good = TRUE;
                        great = TRUE;
                }

                /* Average dungeon and monster levels */
                object_level = (dun_level + r_ptr->level) / 2;

                /* Determine how much we can drop */
                if ((r_ptr->flags1 & (RF1_DROP_60)) && (rand_int(100) < 60)) number++;
                if ((r_ptr->flags1 & (RF1_DROP_90)) && (rand_int(100) < 90)) number++;
                if  (r_ptr->flags1 & (RF1_DROP_1D2)) number += damroll(1, 2);
                if  (r_ptr->flags1 & (RF1_DROP_2D2)) number += damroll(2, 2);
                if  (r_ptr->flags1 & (RF1_DROP_3D2)) number += damroll(3, 2);
                if  (r_ptr->flags1 & (RF1_DROP_4D2)) number += damroll(4, 2);

                /* Friendly monsters do NOT get items! */
                if (is_pet(m_ptr)) number = 0;

                /* Hack -- handle creeping coins */
                coin_type = force_coin;

                /* Drop some objects */
                for (j = 0; j < number; j++)
                {
                        int o_idx;
                        object_type *o_ptr;

                        /* Get local object */
                        q_ptr = &forge;

                        /* Wipe the object */
                        object_wipe(q_ptr);

                        /* Make Gold */
                        if (do_gold && (!do_item || (rand_int(100) < 50)))
                        {
                                /* Make some gold */
                                if (!make_gold(q_ptr)) continue;

                                /* XXX XXX XXX */
                                dump_gold++;
                        }

                        /* Make Object */
                        else
                        {
                                /* Make an object */
                                if (!make_object(q_ptr, good, great)) continue;

                                /* XXX XXX XXX */
                                dump_item++;
                        }

                        /* Get new object */
                        o_idx = o_pop();

                        if(o_idx)
                        {
                                /* Get the item */
                                o_ptr = &o_list[o_idx];

                                /* Structure copy */
                                object_copy(o_ptr, q_ptr);

                                /* Build a stack */
                                o_ptr->next_o_idx = m_ptr->hold_o_idx;

                                o_ptr->held_m_idx = c_ptr->m_idx;
                                o_ptr->ix = 0;
                                o_ptr->iy = 0;

                                m_ptr->hold_o_idx = o_idx;
                        }
                }

                /* Reset the object level */
                object_level = dun_level;

                /* Reset "coin" type */
                coin_type = 0;
        }

	/* Unknown distance */
	m_ptr->cdis = 0;

	/* No flags */
	m_ptr->mflag = 0;

	/* Not visible */
	m_ptr->ml = FALSE;

        /* Give the monster a level */
        tempint = dun_level;
	if (r_ptr->cursed > 0 && tempint < r_ptr->cursed) tempint = r_ptr->cursed;
	if (m_ptr->boss >= 1 || r_ptr->cursed > 0) m_ptr->level = tempint;
        else m_ptr->level = randint(tempint / 2) + (tempint / 2);
	if (r_ptr->cursed > 0 && m_ptr->level < r_ptr->level) m_ptr->level = r_ptr->level;
        if (p_ptr->lev >= 4) m_ptr->level += randint(3);
        /* Higher levels for lower depths...and higher player levels! */
        if (p_ptr->lev >= 4)
        {
                m_ptr->level += (tempint / 4);
                m_ptr->level += p_ptr->lev / 4;
        }
	/* However, a fixed level will override everything. */
	if (r_ptr->fixedlevel > 0) m_ptr->level = r_ptr->fixedlevel;

        /* Bosses are 5 levels higher than the current dungeon level */
        if (m_ptr->boss == 2) m_ptr->level += 5;

	/* Possible modifier. */
	m_ptr->level += p_ptr->events[29025];

	/* Your worst nightmare! */
	if (r_ptr->cursed > 0) m_ptr->level += p_ptr->cursed / 4;

	/* If not in a quest level, Divination's Twist Fate could affect this. */
	if (!(p_ptr->inside_quest) && p_ptr->events[29003] == 1 && !(charm))
	{
		m_ptr->level += fate_monsters(1);
		if (m_ptr->level < 1) m_ptr->level = 1;
	}

        if (m_ptr->level < (dun_level / 2)) m_ptr->level = dun_level;
        if (is_pet(m_ptr) && m_ptr->r_idx == 1080) m_ptr->level = 1;
        if (m_ptr->level <= 0) m_ptr->level = 1;
        /* if (m_ptr->level > tempint && m_ptr->boss == 0) m_ptr->level = tempint; */
        if (m_ptr->level <= 0) m_ptr->level = 1;

	/* Summoned? */
	m_ptr->summoned = dur;
	if (m_ptr->summoned > 0) m_ptr->no_experience = TRUE;
	if (m_ptr->summoned < 0)
	{
		m_ptr->summoned = 0;
		m_ptr->no_experience = TRUE;
	}

	/* Lives */
	m_ptr->lives = r_ptr->lives;

	/* To keep up Nightmares difficulty! :) */
	if (r_ptr->cursed > 0) m_ptr->lives += multiply_divide(m_ptr->lives, m_ptr->level * 3, 100);

	/* Determine the monster's hp. */
	{
		int a;
		s32b hpperlevels;

		if (r_ptr->flags1 & (RF1_FORCE_MAXHP))
		{
			hpperlevels = maxroll(r_ptr->hdice, r_ptr->hside);
		}
		else
		{
			hpperlevels = damroll(r_ptr->hdice, r_ptr->hside);
		}

		hpperlevels += multiply_divide(hpperlevels, (m_ptr->level * 5), 100);

		/* Three times more hp for elites... */
        	if (m_ptr->boss == 1) hpperlevels *= 3;
        	/* SIX TIMES more hp for bosses!!! */
        	if (m_ptr->boss == 2 || r_ptr->cursed > 0) hpperlevels *= 6;

		/* Assume 0 max hp. */
		m_ptr->maxhp = 0;

		for (a = 0; a < m_ptr->level; a++)
		{
        		m_ptr->maxhp += hpperlevels;
			if (m_ptr->maxhp > 2000000000 || m_ptr->maxhp < 0)
			{
				m_ptr->maxhp = 2000000000;
				break;
			}
		}
	}

	/* And start out fully healthy */
	m_ptr->hp = m_ptr->maxhp;

	/* Now, determine the stats! */
	m_ptr->str = r_ptr->str + multiply_divide(r_ptr->str, ((m_ptr->level - 1) * 5), 100);
	m_ptr->dex = r_ptr->dex + multiply_divide(r_ptr->dex, ((m_ptr->level - 1) * 5), 100);
	m_ptr->mind = r_ptr->mind + multiply_divide(r_ptr->mind, ((m_ptr->level - 1) * 5), 100);
	m_ptr->skill_attack = r_ptr->skill_attack + multiply_divide(r_ptr->skill_attack, ((m_ptr->level - 1) * 5), 100);
	m_ptr->skill_ranged = r_ptr->skill_ranged + multiply_divide(r_ptr->skill_ranged, ((m_ptr->level - 1) * 5), 100);
	m_ptr->skill_magic = r_ptr->skill_magic + multiply_divide(r_ptr->skill_magic, ((m_ptr->level - 1) * 5), 100);

	if (m_ptr->boss == 1)
	{
		m_ptr->str += multiply_divide(m_ptr->str, (m_ptr->level / 3), 100);
		m_ptr->dex += multiply_divide(m_ptr->dex, (m_ptr->level / 3), 100);
		m_ptr->mind += multiply_divide(m_ptr->mind, (m_ptr->level / 3), 100);
		m_ptr->skill_attack += multiply_divide(m_ptr->skill_attack, (m_ptr->level / 3), 100);
		m_ptr->skill_ranged += multiply_divide(m_ptr->skill_ranged, (m_ptr->level / 3), 100);
		m_ptr->skill_magic += multiply_divide(m_ptr->skill_magic, (m_ptr->level / 3), 100);
	}

	if (m_ptr->boss == 2 || r_ptr->cursed > 0)
	{
		m_ptr->str += multiply_divide(m_ptr->str, m_ptr->level + r_ptr->cursed, 100);
		m_ptr->dex += multiply_divide(m_ptr->dex, m_ptr->level + r_ptr->cursed, 100);
		m_ptr->mind += multiply_divide(m_ptr->mind, m_ptr->level + r_ptr->cursed, 100);
		m_ptr->skill_attack += multiply_divide(m_ptr->skill_attack, m_ptr->level + r_ptr->cursed, 100);
		m_ptr->skill_ranged += multiply_divide(m_ptr->skill_ranged, m_ptr->level + r_ptr->cursed, 100);
		m_ptr->skill_magic += multiply_divide(m_ptr->skill_magic, m_ptr->level + r_ptr->cursed, 100);
	}
	if (m_ptr->str > 2000000000 || m_ptr->str < 0) m_ptr->str = 2000000000;
	if (m_ptr->dex > 2000000000 || m_ptr->dex < 0) m_ptr->dex = 2000000000;
	if (m_ptr->mind > 2000000000 || m_ptr->mind < 0) m_ptr->mind = 2000000000;
	if (m_ptr->skill_attack > 2000000000 || m_ptr->skill_attack < 0) m_ptr->skill_attack = 2000000000;
	if (m_ptr->skill_ranged > 2000000000 || m_ptr->skill_ranged < 0) m_ptr->skill_ranged = 2000000000;
	if (m_ptr->skill_magic > 2000000000 || m_ptr->skill_magic < 0) m_ptr->skill_magic = 2000000000;

        /* Hit rate! */
        m_ptr->hitrate = ((m_ptr->dex - 5) * 10) + 2;
	m_ptr->hitrate += ((m_ptr->str - 5) / 2);
	m_ptr->hitrate += multiply_divide(m_ptr->hitrate, ((m_ptr->dex - 5) * 5), 100);

        /* Defense! */
	m_ptr->defense = r_ptr->ac + multiply_divide(r_ptr->ac, ((m_ptr->level - 1) * 5), 100);

	/* Mana! */
	m_ptr->mana = (m_ptr->mind - 5) * 10;
	if (m_ptr->mana < 0) m_ptr->mana = 0;

	if (m_ptr->boss == 1) m_ptr->defense += ((m_ptr->defense * (m_ptr->level * 5)) / 100);
	if (m_ptr->boss == 2) m_ptr->defense += ((m_ptr->defense * (m_ptr->level * 10)) / 100);

        /* If the monster is an elite or a boss, give it an ability... */
        if (m_ptr->boss == 1) m_ptr->defense += multiply_divide(m_ptr->defense, (m_ptr->level * 5), 100);
	if (m_ptr->boss == 2 || r_ptr->cursed > 0) m_ptr->defense += multiply_divide(m_ptr->defense, (m_ptr->level * 10), 100);

	/* Extract the monster base speed */
	m_ptr->mspeed = r_ptr->speed;
	
        /* Level will now boost the speed a little... */
        {
		int a, b;

		b = (m_ptr->level / 5);

		for (a = 0; a < b; a++)
		{
        		m_ptr->mspeed += 1;
			if (m_ptr->mspeed > 180)
			{
				m_ptr->mspeed = 180;
				break;
			}
		}
	}

	/* Give a random starting energy */
	/*m_ptr->energy = (byte)rand_int(100);*/
	m_ptr->energy = 100;

	/* Force monster to wait for player */
	if (r_ptr->flags1 & (RF1_FORCE_SLEEP))
	{
		/* Monster is still being nice */
		m_ptr->mflag |= (MFLAG_NICE);

		/* Must repair monsters */
		repair_monsters = TRUE;
	}

	/* Hack -- see "process_monsters()" */
	if (c_ptr->m_idx < hack_m_idx)
	{
		/* Monster is still being born */
		m_ptr->mflag |= (MFLAG_BORN);
	}


	/* Update the monster */
	update_mon(c_ptr->m_idx, TRUE);


	/* Hack -- Count the monsters on the level */
	r_ptr->cur_num++;

        /* Unique monsters on saved levels should be "marked" */
        if ((r_ptr->flags1 & RF1_UNIQUE) && get_dungeon_save(dummy))
        {
                r_ptr->on_saved = TRUE;
        }

	/* Hack -- Count the number of "reproducers" */
	if (r_ptr->flags2 & (RF2_MULTIPLY)) num_repro++;


	/* Hack -- Notice new multi-hued monsters */
	if (r_ptr->flags1 & (RF1_ATTR_MULTI)) shimmer_monsters = TRUE;

	/* Call a lua script. */
	if (r_ptr->event_spawn > 0)
	{
		call_lua("monster_spawn", "(dd)", "", c_ptr->m_idx, r_ptr->event_spawn);
	}


	/* Success */
	return (TRUE);
}

/* Place monster return, but no boss! */
s16b place_monster_one_return_no_boss(int y, int x, int r_idx, bool slp, bool charm, int petlevel, s32b pethp, s32b petmaxhp, int dur)
{
        int             i;
        char            dummy[5];

	cave_type		*c_ptr;

	monster_type	*m_ptr;

	monster_race	*r_ptr = &r_info[r_idx];

	cptr		name = (r_name + r_ptr->name);


	/* Verify location */
        if (!in_bounds(y, x)) return 0;

	/* Require empty space */
        if (!cave_empty_bold(y, x)) return 0;

	/* Hack -- no creation on glyph of warding */
        if (cave[y][x].feat == FEAT_GLYPH) return 0;
        if (cave[y][x].feat == FEAT_MINOR_GLYPH) return 0;

        /* Nor on the between */
        if (cave[y][x].feat == FEAT_BETWEEN) return 0;

	/* Nor on the Pattern */
	if ((cave[y][x].feat >= FEAT_PATTERN_START)
	 && (cave[y][x].feat <= FEAT_PATTERN_XTRA2))
                return 0;

	/* Paranoia */
        if (!r_idx) return 0;

	/* Paranoia */
        if (!r_ptr->name) return 0;

	if (!monster_can_cross_terrain(cave[y][x].feat, r_ptr))
	{
		return FALSE;
	}

	/* Hack -- "unique" monsters must be "unique" */
	if ((r_ptr->flags1 & (RF1_UNIQUE)) && (r_ptr->cur_num >= r_ptr->max_num))
	{
		/* Cannot create */
                return 0;
	}

	/* Depth monsters may NOT be created out of depth */
	if ((r_ptr->flags1 & (RF1_FORCE_DEPTH)) && (dun_level < r_ptr->level))
	{
		/* Cannot create */
                return 0;
	}

        /* The monster is already on an unique level */
        if (r_ptr->on_saved) return (FALSE);


	/* Powerful monster */
	if (r_ptr->level > dun_level)
	{
		/* Unique monsters */
		if (r_ptr->flags1 & (RF1_UNIQUE))
		{
			/* Message for cheaters */
                        if ((cheat_hear)) msg_format("Deep Unique (%s).", name);

			/* Boost rating by twice delta-depth */
			rating += (r_ptr->level - dun_level) * 2;
		}

		/* Normal monsters */
		else
		{
			/* Message for cheaters */
                        if ((cheat_hear)) msg_format("Deep Monster (%s).", name);

			/* Boost rating by delta-depth */
			rating += (r_ptr->level - dun_level);
		}
	}

	/* Note the monster */
	else if (r_ptr->flags1 & (RF1_UNIQUE))
	{
		/* Unique monsters induce message */
                if ((cheat_hear)) msg_format("Unique (%s).", name);
	}


	/* Access the location */
	c_ptr = &cave[y][x];

	/* Make a new monster */
	c_ptr->m_idx = m_pop();
	hack_m_idx_ii = c_ptr->m_idx;

	/* Mega-Hack -- catch "failure" */
        if (!c_ptr->m_idx) return 0;


	/* Get a new monster record */
	m_ptr = &m_list[c_ptr->m_idx];

	/* Save the race */
	m_ptr->r_idx = r_idx;

	/* Place the monster at the location */
	m_ptr->fy = y;
	m_ptr->fx = x;


	/* No "damage" yet */
	m_ptr->stunned = 0;
	m_ptr->confused = 0;
	m_ptr->monfear = 0;

        /* NOT a boss! */
        m_ptr->boss = 0;

	/* Not hasted nor boosted */
	m_ptr->hasted = 0;
	m_ptr->boosted = 0;

	/* Not summoned */
	m_ptr->summoned = 0;

	/* No lives */
	m_ptr->lives = 0;

	/* Worth experience. */
	m_ptr->no_experience = FALSE;

        /* Not hit by Sealing Light */
        m_ptr->seallight = 0;

        /* Not an animated monster...never will. */
        m_ptr->animated = FALSE;
        m_ptr->animdam_d = 0;
        m_ptr->animdam_s = 0;

	/* Friendly? */
        if ( charm || (r_ptr->flags7 & RF7_PET) )
	{
		set_pet(m_ptr, TRUE);
	}

	/* Assume no sleeping */
	m_ptr->csleep = 0;

	/* Enforce sleeping if needed */
        if (slp && r_ptr->sleep && m_ptr->boss == 0)
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

        /* Give the monster a level */
        m_ptr->level = petlevel;

	/* Summoned? */
	m_ptr->summoned = dur;
	if (m_ptr->summoned > 0) m_ptr->no_experience = TRUE;
	if (m_ptr->summoned < 0)
	{
		m_ptr->summoned = 0;
		m_ptr->no_experience = TRUE;
	}

	/* Lives */
	m_ptr->lives = r_ptr->lives;

	/* To keep up Nightmares difficulty! :) */
	if (r_ptr->cursed > 0) m_ptr->lives += multiply_divide(m_ptr->lives, m_ptr->level * 3, 100);

        /* Give the hp */
        m_ptr->maxhp = petmaxhp;

	/* And start out fully healthy */
        m_ptr->hp = pethp;

	/* Extract the monster base speed */
	m_ptr->mspeed = r_ptr->speed;

	/* Now, determine the stats! */
	m_ptr->str = r_ptr->str + multiply_divide(r_ptr->str, ((m_ptr->level - 1) * 5), 100);
	m_ptr->dex = r_ptr->dex + multiply_divide(r_ptr->dex, ((m_ptr->level - 1) * 5), 100);
	m_ptr->mind = r_ptr->mind + multiply_divide(r_ptr->mind, ((m_ptr->level - 1) * 5), 100);
	m_ptr->skill_attack = r_ptr->skill_attack + multiply_divide(r_ptr->skill_attack, ((m_ptr->level - 1) * 5), 100);
	m_ptr->skill_ranged = r_ptr->skill_ranged + multiply_divide(r_ptr->skill_ranged, ((m_ptr->level - 1) * 5), 100);
	m_ptr->skill_magic = r_ptr->skill_magic + multiply_divide(r_ptr->skill_magic, ((m_ptr->level - 1) * 5), 100);

	if (m_ptr->str > 2000000000 || m_ptr->str < 0) m_ptr->str = 2000000000;
	if (m_ptr->dex > 2000000000 || m_ptr->dex < 0) m_ptr->dex = 2000000000;
	if (m_ptr->mind > 2000000000 || m_ptr->mind < 0) m_ptr->mind = 2000000000;
	if (m_ptr->skill_attack > 2000000000 || m_ptr->skill_attack < 0) m_ptr->skill_attack = 2000000000;
	if (m_ptr->skill_ranged > 2000000000 || m_ptr->skill_ranged < 0) m_ptr->skill_ranged = 2000000000;
	if (m_ptr->skill_magic > 2000000000 || m_ptr->skill_magic < 0) m_ptr->skill_magic = 2000000000;

        /* Hit rate! */
        m_ptr->hitrate = ((m_ptr->dex - 5) * 10) + 2;
	m_ptr->hitrate += ((m_ptr->str - 5) / 2);
	m_ptr->hitrate += multiply_divide(m_ptr->hitrate, ((m_ptr->dex - 5) * 5), 100);

        /* Defense! */
	m_ptr->defense = r_ptr->ac + multiply_divide(r_ptr->ac, ((m_ptr->level - 1) * 5), 100);

	/* Mana! */
	m_ptr->mana = (m_ptr->mind - 5) * 10;
	if (m_ptr->mana < 0) m_ptr->mana = 0;

        /* Level will now boost the speed a little... */
        {
		int a, b;

		b = (m_ptr->level / 5);

		for (a = 0; a < b; a++)
		{
        		m_ptr->mspeed += 1;
			if (m_ptr->mspeed > 180)
			{
				m_ptr->mspeed = 180;
				break;
			}
		}
	}

	/* Give a random starting energy */
	/*m_ptr->energy = (byte)rand_int(100);*/
	m_ptr->energy = 100;

	/* Force monster to wait for player */
	if (r_ptr->flags1 & (RF1_FORCE_SLEEP))
	{
		/* Monster is still being nice */
		m_ptr->mflag |= (MFLAG_NICE);

		/* Must repair monsters */
		repair_monsters = TRUE;
	}

        /* Give the imprint, so they are controllable! */
        /* Bone Walls however aren't imprinted... */
        if (m_ptr->r_idx != 1116) m_ptr->imprinted = TRUE;

        /* Yeah, all right, they're also our friends! */
        m_ptr->friend = TRUE;

	/* Hack -- see "process_monsters()" */
	if (c_ptr->m_idx < hack_m_idx)
	{
		/* Monster is still being born */
		m_ptr->mflag |= (MFLAG_BORN);
	}


	/* Update the monster */
	update_mon(c_ptr->m_idx, TRUE);


	/* Hack -- Count the monsters on the level */
	r_ptr->cur_num++;

        /* Unique monsters on saved levels should be "marked" */
        if ((r_ptr->flags1 & RF1_UNIQUE) && get_dungeon_save(dummy))
        {
                r_ptr->on_saved = TRUE;
        }

	/* Hack -- Count the number of "reproducers" */
	if (r_ptr->flags2 & (RF2_MULTIPLY)) num_repro++;


	/* Hack -- Notice new multi-hued monsters */
	if (r_ptr->flags1 & (RF1_ATTR_MULTI)) shimmer_monsters = TRUE;

	/* Call a lua script. */
	if (r_ptr->event_spawn > 0)
	{
		call_lua("monster_spawn", "(dd)", "", c_ptr->m_idx, r_ptr->event_spawn);
	}

	/* Success */
        return c_ptr->m_idx;
}

/* You should get the idea by now... ;) */
bool place_monster_aux_no_boss(int y, int x, int r_idx, bool slp, bool grp, bool charm, int dur)
{
	int             i;
	monster_race    *r_ptr = &r_info[r_idx];
	bool (*old_get_mon_num_hook)(int r_idx);


	/* Place one monster, or fail */
        if (!place_monster_one_no_boss(y, x, r_idx, slp, charm, dur)) return (FALSE);


	/* Require the "group" flag */
	if (!grp) return (TRUE);


	/* Friends for certain monsters */
	if (r_ptr->flags1 & (RF1_FRIENDS))
	{
		/* Attempt to place a group */
        (void)place_monster_group(y, x, r_idx, slp, charm, dur);
	}


	/* Escorts for certain monsters */
	if (r_ptr->flags1 & (RF1_ESCORT))
	{
		old_get_mon_num_hook = get_mon_num_hook;

		/* Set the escort index */
		place_monster_idx = r_idx;

		/* Set the escort hook */
		get_mon_num_hook = place_monster_okay;

		/* Prepare allocation table */
		get_mon_num_prep();

		/* Try to place several "escorts" */
		for (i = 0; i < 50; i++)
		{
			int nx, ny, z, d = 3;

			/* Pick a location */
			scatter(&ny, &nx, y, x, d, 0);

			/* Require empty grids */
			if (!cave_empty_bold(ny, nx)) continue;

			set_mon_num2_hook(ny, nx);

			/* Prepare allocation table */
			get_mon_num_prep();

			/* Pick a random race */
			z = get_mon_num(r_ptr->level);

			/* Reset restriction */
			get_mon_num2_hook = NULL;

			/* Prepare allocation table */
			get_mon_num_prep();

			/* Handle failure */
			if (!z) break;

			/* Place a single escort */
                        (void)place_monster_one_no_boss(ny, nx, z, slp, charm, dur);

			/* Place a "group" of escorts if needed */
			if ((r_info[z].flags1 & (RF1_FRIENDS)) ||
			    (r_ptr->flags1 & (RF1_ESCORTS)))
			{
				/* Place a group of monsters */
				(void)place_monster_group(ny, nx, z, slp, charm, dur);
			}
		}

		/* Reset restriction */
		get_mon_num_hook = old_get_mon_num_hook;

		/* Prepare allocation table */
		get_mon_num_prep();
	}

	/* Success */
	return (TRUE);
}

/* The Simulacrum version of place_monster */
/* VERY similar, except they aren't friends, nor are they imprinted */
s16b place_monster_one_simulacrum(int y, int x, int r_idx, bool slp, bool charm, int petlevel, s32b pethp, int dur)
{
        int             i;
        char            dummy[5];

	cave_type		*c_ptr;

	monster_type	*m_ptr;

	monster_race	*r_ptr = &r_info[r_idx];

	cptr		name = (r_name + r_ptr->name);


	/* Verify location */
        if (!in_bounds(y, x)) return 0;

	/* Require empty space */
        if (!cave_empty_bold(y, x)) return 0;

	/* Hack -- no creation on glyph of warding */
        if (cave[y][x].feat == FEAT_GLYPH) return 0;
        if (cave[y][x].feat == FEAT_MINOR_GLYPH) return 0;

        /* Nor on the between */
        if (cave[y][x].feat == FEAT_BETWEEN) return 0;

	/* Nor on the Pattern */
	if ((cave[y][x].feat >= FEAT_PATTERN_START)
	 && (cave[y][x].feat <= FEAT_PATTERN_XTRA2))
                return 0;

	/* Paranoia */
        if (!r_idx) return 0;

	/* Paranoia */
        if (!r_ptr->name) return 0;

	if (!monster_can_cross_terrain(cave[y][x].feat, r_ptr))
	{
		return FALSE;
	}

	/* Hack -- "unique" monsters must be "unique" */
        /*if ((r_ptr->flags1 & (RF1_UNIQUE)) && (r_ptr->cur_num >= r_ptr->max_num))
	{
                return 0;
        }*/

	/* Depth monsters may NOT be created out of depth */
	if ((r_ptr->flags1 & (RF1_FORCE_DEPTH)) && (dun_level < r_ptr->level))
	{
		/* Cannot create */
                return 0;
	}

        /* The monster is already on an unique level */
        if (r_ptr->on_saved) return (FALSE);


	/* Powerful monster */
	if (r_ptr->level > dun_level)
	{
		/* Unique monsters */
		if (r_ptr->flags1 & (RF1_UNIQUE))
		{
			/* Message for cheaters */
                        if ((cheat_hear)) msg_format("Deep Unique (%s).", name);

			/* Boost rating by twice delta-depth */
			rating += (r_ptr->level - dun_level) * 2;
		}

		/* Normal monsters */
		else
		{
			/* Message for cheaters */
                        if ((cheat_hear)) msg_format("Deep Monster (%s).", name);

			/* Boost rating by delta-depth */
			rating += (r_ptr->level - dun_level);
		}
	}

	/* Note the monster */
	else if (r_ptr->flags1 & (RF1_UNIQUE))
	{
		/* Unique monsters induce message */
                if ((cheat_hear)) msg_format("Unique (%s).", name);
	}


	/* Access the location */
	c_ptr = &cave[y][x];

	/* Make a new monster */
	c_ptr->m_idx = m_pop();
	hack_m_idx_ii = c_ptr->m_idx;

	/* Mega-Hack -- catch "failure" */
        if (!c_ptr->m_idx) return 0;


	/* Get a new monster record */
	m_ptr = &m_list[c_ptr->m_idx];

	/* Save the race */
	m_ptr->r_idx = r_idx;

	/* Place the monster at the location */
	m_ptr->fy = y;
	m_ptr->fx = x;


	/* No "damage" yet */
	m_ptr->stunned = 0;
	m_ptr->confused = 0;
	m_ptr->monfear = 0;

        /* NOT a boss! */
        m_ptr->boss = 0;

	/* Not hasted nor boosted */
	m_ptr->hasted = 0;
	m_ptr->boosted = 0;

	/* Not summoned */
	m_ptr->summoned = 0;

	/* No lives */
	m_ptr->lives = 0;

	/* Worth experience. */
	m_ptr->no_experience = FALSE;

        /* Not hit by Sealing Light */
        m_ptr->seallight = 0;

        /* Not an animated monster...never will. */
        m_ptr->animated = FALSE;
        m_ptr->animdam_d = 0;
        m_ptr->animdam_s = 0;

	/* Friendly? */
        if ( charm || (r_ptr->flags7 & RF7_PET) )
	{
		set_pet(m_ptr, TRUE);
	}

	/* Assume no sleeping */
	m_ptr->csleep = 0;

	/* Enforce sleeping if needed */
        if (slp && r_ptr->sleep && m_ptr->boss == 0)
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

        /* Give the monster a level */
        m_ptr->level = petlevel;

	/* Summoned? */
	m_ptr->summoned = dur;
	if (m_ptr->summoned > 0) m_ptr->no_experience = TRUE;
	if (m_ptr->summoned < 0)
	{
		m_ptr->summoned = 0;
		m_ptr->no_experience = TRUE;
	}

	/* Lives */
	m_ptr->lives = r_ptr->lives;

	/* To keep up Nightmares difficulty! :) */
	if (r_ptr->cursed > 0) m_ptr->lives += multiply_divide(m_ptr->lives, m_ptr->level * 3, 100);

        /* Give the hp */
        m_ptr->maxhp = pethp;

	/* And start out fully healthy */
        m_ptr->hp = m_ptr->maxhp;

	/* Extract the monster base speed */
	m_ptr->mspeed = r_ptr->speed;

	/* Now, determine the stats! */
	m_ptr->str = r_ptr->str + multiply_divide(r_ptr->str, ((m_ptr->level - 1) * 5), 100);
	m_ptr->dex = r_ptr->dex + multiply_divide(r_ptr->dex, ((m_ptr->level - 1) * 5), 100);
	m_ptr->mind = r_ptr->mind + multiply_divide(r_ptr->mind, ((m_ptr->level - 1) * 5), 100);
	m_ptr->skill_attack = r_ptr->skill_attack + multiply_divide(r_ptr->skill_attack, ((m_ptr->level - 1) * 5), 100);
	m_ptr->skill_ranged = r_ptr->skill_ranged + multiply_divide(r_ptr->skill_ranged, ((m_ptr->level - 1) * 5), 100);
	m_ptr->skill_magic = r_ptr->skill_magic + multiply_divide(r_ptr->skill_magic, ((m_ptr->level - 1) * 5), 100);

	if (m_ptr->str > 2000000000 || m_ptr->str < 0) m_ptr->str = 2000000000;
	if (m_ptr->dex > 2000000000 || m_ptr->dex < 0) m_ptr->dex = 2000000000;
	if (m_ptr->mind > 2000000000 || m_ptr->mind < 0) m_ptr->mind = 2000000000;
	if (m_ptr->skill_attack > 2000000000 || m_ptr->skill_attack < 0) m_ptr->skill_attack = 2000000000;
	if (m_ptr->skill_ranged > 2000000000 || m_ptr->skill_ranged < 0) m_ptr->skill_ranged = 2000000000;
	if (m_ptr->skill_magic > 2000000000 || m_ptr->skill_magic < 0) m_ptr->skill_magic = 2000000000;

        /* Hit rate! */
        m_ptr->hitrate = ((m_ptr->dex - 5) * 10) + 2;
	m_ptr->hitrate += ((m_ptr->str - 5) / 2);
	m_ptr->hitrate += multiply_divide(m_ptr->hitrate, ((m_ptr->dex - 5) * 5), 100);

        /* Defense! */
	m_ptr->defense = r_ptr->ac + multiply_divide(r_ptr->ac, ((m_ptr->level - 1) * 5), 100);

	/* Mana! */
	m_ptr->mana = (m_ptr->mind - 5) * 10;
	if (m_ptr->mana < 0) m_ptr->mana = 0;

        /* Level will now boost the speed a little... */
        {
		int a, b;

		b = (m_ptr->level / 5);

		for (a = 0; a < b; a++)
		{
        		m_ptr->mspeed += 1;
			if (m_ptr->mspeed > 180)
			{
				m_ptr->mspeed = 180;
				break;
			}
		}
	}

	/* Give a random starting energy */
	/*m_ptr->energy = (byte)rand_int(100);*/
	m_ptr->energy = 100;

	/* Force monster to wait for player */
	if (r_ptr->flags1 & (RF1_FORCE_SLEEP))
	{
		/* Monster is still being nice */
		m_ptr->mflag |= (MFLAG_NICE);

		/* Must repair monsters */
		repair_monsters = TRUE;
	}

	/* Hack -- see "process_monsters()" */
	if (c_ptr->m_idx < hack_m_idx)
	{
		/* Monster is still being born */
		m_ptr->mflag |= (MFLAG_BORN);
	}


	/* Update the monster */
	update_mon(c_ptr->m_idx, TRUE);


	/* Hack -- Count the monsters on the level */
	r_ptr->cur_num++;

        /* Unique monsters on saved levels should be "marked" */
        if ((r_ptr->flags1 & RF1_UNIQUE) && get_dungeon_save(dummy))
        {
                r_ptr->on_saved = TRUE;
        }

	/* Hack -- Count the number of "reproducers" */
	if (r_ptr->flags2 & (RF2_MULTIPLY)) num_repro++;


	/* Hack -- Notice new multi-hued monsters */
	if (r_ptr->flags1 & (RF1_ATTR_MULTI)) shimmer_monsters = TRUE;

	/* Call a lua script. */
	if (r_ptr->event_spawn > 0)
	{
		call_lua("monster_spawn", "(dd)", "", c_ptr->m_idx, r_ptr->event_spawn);
	}

	/* Success */
        return c_ptr->m_idx;
}

bool summon_specific_friendly_kind(int y1, int x1, int lev, char kind, bool Group_ok, int dur)
{
	int i, x, y, r_idx;        

        summoner_monster = NULL;

	/* Look for a location */
	for (i = 0; i < 20; ++i)
	{
		/* Pick a distance */
		int d = (i / 15) + 1;

		/* Pick a location */
		scatter(&y, &x, y1, x1, d, 0);

		/* Require "empty" floor grid */
		if (!cave_empty_bold(y, x)) continue;

		/* Hack -- no summon on glyph of warding */
		 if (cave[y][x].feat == FEAT_GLYPH) continue;
		 if (cave[y][x].feat == FEAT_MINOR_GLYPH) continue;

                /* Nor on the between */
                if (cave[y][x].feat == FEAT_BETWEEN) return (FALSE);

		/* ... nor on the Pattern */
		if ((cave[y][x].feat >= FEAT_PATTERN_START) &&
		    (cave[y][x].feat <= FEAT_PATTERN_XTRA2))
			continue;

		/* Okay */
		break;
	}

	/* Failure */
	if (i == 20) return (FALSE);

	/* Prepare allocation table */
	get_mon_num_prep();

	/* Pick a monster, using the level calculation */
        r_idx = get_mon_num_kind(lev, kind);

        /* Do we have a valid r_idx? */
        if (r_idx == 0) return (FALSE);

	/* Prepare allocation table */
	get_mon_num_prep();

	/* Handle failure */
	if (!r_idx) return (FALSE);

	/* Attempt to place the monster (awake, allow groups) */
	if (!place_monster_aux(y, x, r_idx, FALSE, Group_ok, TRUE, dur)) return (FALSE);

	/* Success */
	return (TRUE);
}

int get_mon_num_kind(int lev, char kind)
{
        int choosemon = 0;
        int x;
        bool cansummon = FALSE;
        bool okaysignal = FALSE; /* Yes, I like this name! :) */
	char s[80];

        /* Do a first check to see if it's actually possible */
        /* to summon something, to avoid enless loops */
	for (x = 1; x < max_r_idx; x++)
        {
                monster_race *r_ptr;

                r_ptr = &r_info[x];

                /* Make sure it's the right kind */
                /* And not UNIQUE */
                if ((r_ptr->d_char == kind) && (r_ptr->level >= 1) && !(r_ptr->flags1 & (RF1_UNIQUE)) && !(r_ptr->flags9 & (RF9_SPECIAL_GENE)) && !(r_ptr->flags7 & (RF7_TOWNSFOLK)) && !(r_ptr->flags7 & (RF7_GUARD)))
                {
                        /* Check for the depth */
                        if (r_ptr->level <= lev) cansummon = TRUE;
                }
        }
        if (cansummon == FALSE) return (0);

        while (!okaysignal)
        {
                monster_race *r_ptr;
                /* Pick up a monster... */
                choosemon = randint((max_r_idx - 1));
		
                r_ptr = &r_info[choosemon];

                /* Make sure it's the right kind */
                /* And not UNIQUE */
                if ((r_ptr->d_char == kind) && (r_ptr->level >= 1) && !(r_ptr->flags1 & (RF1_UNIQUE)) && !(r_ptr->flags9 & (RF9_SPECIAL_GENE)) && !(r_ptr->flags7 & (RF7_TOWNSFOLK)) && !(r_ptr->flags7 & (RF7_GUARD)) && choosemon != 0)
                {
                        /* Check for the depth */
                        if (r_ptr->level <= lev) okaysignal = TRUE;
                }

        }
        return (choosemon);
}

int get_mon_num_rflag(int lev, u32b rflag)
{
        int choosemon = 0;
        int x;
        bool cansummon = FALSE;
        bool okaysignal = FALSE; /* Yes, I like this name! :) */
	char s[80];

        /* Do a first check to see if it's actually possible */
        /* to summon something, to avoid enless loops */
	for (x = 1; x < max_r_idx; x++)
        {
                monster_race *r_ptr;

                r_ptr = &r_info[x];

                /* Make sure it's the right kind */
                /* And not UNIQUE */
                if ((r_ptr->flags3 == rflag) && !(r_ptr->flags1 & (RF1_UNIQUE)))
                {
                        /* Check for the depth */
                        if (r_ptr->level <= lev) cansummon = TRUE;
                }
        }
        if (cansummon == FALSE) return (0);

        while (!okaysignal)
        {
                monster_race *r_ptr;
                /* Pick up a monster... */
                choosemon = randint((max_r_idx - 1));
		
                r_ptr = &r_info[choosemon];

                /* Make sure it's the right kind */
                /* And not UNIQUE */
                if ((r_ptr->flags3 == rflag) && !(r_ptr->flags1 & (RF1_UNIQUE)) && choosemon != 0)
                {
                        /* Check for the depth */
                        if (r_ptr->level <= lev) okaysignal = TRUE;
                }

        }
        return (choosemon);
}

bool summon_specific_friendly_name(int y1, int x1, char name[30], bool Group_ok, int dur)
{
	int i, x, y, r_idx;

        summoner_monster = NULL;

	/* Look for a location */
	for (i = 0; i < 20; ++i)
	{
		/* Pick a distance */
		int d = (i / 15) + 1;

		/* Pick a location */
		scatter(&y, &x, y1, x1, d, 0);

		/* Require "empty" floor grid */
		if (!cave_empty_bold(y, x)) continue;

		/* Hack -- no summon on glyph of warding */
		 if (cave[y][x].feat == FEAT_GLYPH) continue;
		 if (cave[y][x].feat == FEAT_MINOR_GLYPH) continue;

                /* Nor on the between */
                if (cave[y][x].feat == FEAT_BETWEEN) return (FALSE);

		/* ... nor on the Pattern */
		if ((cave[y][x].feat >= FEAT_PATTERN_START) &&
		    (cave[y][x].feat <= FEAT_PATTERN_XTRA2))
			continue;

		/* Okay */
		break;
	}

	/* Failure */
	if (i == 20) return (FALSE);

	/* Prepare allocation table */
	get_mon_num_prep();

	/* Pick a monster, using the level calculation */
        r_idx = get_mon_num_name(name);

        /* Do we have a valid r_idx? */
        if (r_idx == 0) return (FALSE);

	/* Prepare allocation table */
	get_mon_num_prep();

	/* Handle failure */
	if (!r_idx) return (FALSE);

	/* Attempt to place the monster (awake, allow groups) */
	if (!place_monster_aux(y, x, r_idx, FALSE, Group_ok, TRUE, dur)) return (FALSE);

	/* Success */
	return (TRUE);
}

int get_mon_num_name(char name[30])
{
        int x;
        bool cansummon = FALSE;

        for (x = 0; x < max_r_idx; x++)
        {
                monster_race *r_ptr;

                r_ptr = &r_info[x];

                /* Make sure it's the right name */
                /* And not UNIQUE */
                if (strstr((r_name + r_ptr->name), name) && !(r_ptr->flags1 & (RF1_UNIQUE)))
                {
                        cansummon = TRUE;
                        return (x);
                }
        }
        /* Default */
        return (0);
}

/* Place an 'image' monster. Used by Mirror Image ability. */
/* It should only be used with monster 1077, but can theorically be used */
/* by any kinds of monsters. */
bool place_monster_one_image(int y, int x, int r_idx, bool slp, bool charm, int dur)
{
        int             i;

        char            dummy[5];

	cave_type		*c_ptr;

	monster_type	*m_ptr;

	monster_race	*r_ptr = &r_info[r_idx];

	cptr		name = (r_name + r_ptr->name);

	/* Verify location */
	if (!in_bounds(y, x)) return (FALSE);

	/* Require empty space */
	if (!cave_empty_bold(y, x)) return (FALSE);

	/* Hack -- no creation on glyph of warding */
	if (cave[y][x].feat == FEAT_GLYPH) return (FALSE);
	if (cave[y][x].feat == FEAT_MINOR_GLYPH) return (FALSE);

        /* Nor on the between */
        if (cave[y][x].feat == FEAT_BETWEEN) return (FALSE);

        /* Nor on the altars */
        if ((cave[y][x].feat >= FEAT_ALTAR_HEAD)
         && (cave[y][x].feat <= FEAT_ALTAR_TAIL))
                return (FALSE);

	/* Nor on the Pattern */
	if ((cave[y][x].feat >= FEAT_PATTERN_START)
	 && (cave[y][x].feat <= FEAT_PATTERN_XTRA2))
		return (FALSE);

	/* Paranoia */
	if (!r_idx) return (FALSE);

	/* Paranoia */
	if (!r_ptr->name) return (FALSE);

#if 0
        /* No Uniques on saved levels(this would cause too much confusion) */
        if((d_info[dungeon_type].flags1 & DF1_PERSISTENT) && (r_ptr->flags1 & RF1_UNIQUE)) return (FALSE);
#endif

        /* Don't allow undefined ghosts */
        if ((r_idx >= GHOST_R_IDX_HEAD)&&(r_idx <= GHOST_R_IDX_TAIL))
                if(ghost_file[r_idx - GHOST_R_IDX_HEAD][0] == 0)
                        return FALSE;

	if (!monster_can_cross_terrain(cave[y][x].feat, r_ptr))
	{
		return FALSE;
	}

        /* Unallow some uniques to be generated outside of their quests/special levels/dungeons */
        if ((r_ptr->flags9 & RF9_SPECIAL_GENE) && !hack_allow_special && !summoning_specific)
        {
                return FALSE;
        }

	/* Random monsters should only be placed in random dungeons. */
	if ((r_ptr->flags7 & (RF7_RANDOM)) && !(d_info[dungeon_type].flags1 & DF1_RANDOM_ONLY)) return FALSE;

	/* Ice monsters should only be placed in ice dungeons. */
	if ((r_ptr->flags7 & (RF7_ICE)) && !(d_info[dungeon_type].flags1 & DF1_ICE)) return FALSE;

	/* Cursed monsters cannot appear unless the player is cursed enough. */
	if (r_ptr->cursed > 0)
	{
		if ((p_ptr->cursed < r_ptr->cursed) || (p_ptr->inside_quest) || dun_level == 0 || (charm))
		{
			return FALSE;
		}
	}

	/* Hack -- "unique" monsters must be "unique" */
        if ((r_ptr->flags1 & (RF1_UNIQUE)) && (r_ptr->max_num == -1))
	{
		/* Cannot create */
		return (FALSE);
	}

        /* The monster is already on an unique level */
        if (r_ptr->on_saved) return (FALSE);

	/* Hack -- "unique" monsters must be "unique" */
	if ((r_ptr->flags1 & (RF1_UNIQUE)) && (r_ptr->cur_num >= r_ptr->max_num))
	{
		/* Cannot create */
		return (FALSE);
	}

	/* Depth monsters may NOT be created out of depth */
	if ((r_ptr->flags1 & (RF1_FORCE_DEPTH)) && (dun_level < r_ptr->level))
	{
		/* Cannot create */
		return (FALSE);
	}


	/* Powerful monster */
	if (r_ptr->level > dun_level)
	{
		/* Unique monsters */
		if (r_ptr->flags1 & (RF1_UNIQUE))
		{
			/* Message for cheaters */
                        if ((cheat_hear)) msg_format("Deep Unique (%s).", name);

			/* Boost rating by twice delta-depth */
			rating += (r_ptr->level - dun_level) * 2;
		}

		/* Normal monsters */
		else
		{
			/* Message for cheaters */
                        if ((cheat_hear)) msg_format("Deep Monster (%s).", name);

			/* Boost rating by delta-depth */
			rating += (r_ptr->level - dun_level);
		}
	}

	/* Note the monster */
	else if (r_ptr->flags1 & (RF1_UNIQUE))
	{
		/* Unique monsters induce message */
                if ((cheat_hear)) msg_format("Unique (%s).", name);
	}


	/* Access the location */
	c_ptr = &cave[y][x];

	/* Make a new monster */
        c_ptr->m_idx = m_pop();
	hack_m_idx_ii = c_ptr->m_idx;

	/* Mega-Hack -- catch "failure" */
	if (!c_ptr->m_idx) return (FALSE);


	/* Get a new monster record */
	m_ptr = &m_list[c_ptr->m_idx];

	/* Save the race */
	m_ptr->r_idx = r_idx;

	/* Place the monster at the location */
	m_ptr->fy = y;
	m_ptr->fx = x;


	/* No "damage" yet */
	m_ptr->stunned = 0;
	m_ptr->confused = 0;
	m_ptr->monfear = 0;

        /* Not a boss and no abilities... */
        m_ptr->boss = 0;

	/* Not hasted nor boosted */
	m_ptr->hasted = 0;
	m_ptr->boosted = 0;

	/* Not summoned */
	m_ptr->summoned = 0;

	/* No lives */
	m_ptr->lives = 0;

	/* Worth experience. */
	m_ptr->no_experience = FALSE;

        /* Not hit by Sealing Light */
        m_ptr->seallight = 0;

        /* No objects yet */
        m_ptr->hold_o_idx = 0;

        /* Not an animated monster...never will. */
        m_ptr->animated = FALSE;
        m_ptr->animdam_d = 0;
        m_ptr->animdam_s = 0;

	/* Friendly? */
        if ( charm || (r_ptr->flags7 & RF7_PET) )
	{
		set_pet(m_ptr, TRUE);                
	}

	/* Assume no sleeping */
	m_ptr->csleep = 0;

	/* Enforce sleeping if needed */
        if (slp && r_ptr->sleep && m_ptr->boss == 0)
	{
		int val = r_ptr->sleep;
		m_ptr->csleep = ((val * 2) + randint(val * 10));
	}

        /* Generate the monster's inventory(if any) */
        {
                bool good = (r_ptr->flags1 & (RF1_DROP_GOOD)) ? TRUE : FALSE;
                bool great = (r_ptr->flags1 & (RF1_DROP_GREAT)) ? TRUE : FALSE;

                bool do_gold = (!(r_ptr->flags1 & (RF1_ONLY_ITEM)));
                bool do_item = (!(r_ptr->flags1 & (RF1_ONLY_GOLD)));

                int j;

                int force_coin = get_coin_type(r_ptr);

                int dump_item = 0;
                int dump_gold = 0;
                object_type forge;
                object_type *q_ptr;

                int number = 0;

                /* Average dungeon and monster levels */
                object_level = (dun_level + r_ptr->level) / 2;

                /* Determine how much we can drop */
                if ((r_ptr->flags1 & (RF1_DROP_60)) && (rand_int(100) < 60)) number++;
                if ((r_ptr->flags1 & (RF1_DROP_90)) && (rand_int(100) < 90)) number++;
                if  (r_ptr->flags1 & (RF1_DROP_1D2)) number += damroll(1, 2);
                if  (r_ptr->flags1 & (RF1_DROP_2D2)) number += damroll(2, 2);
                if  (r_ptr->flags1 & (RF1_DROP_3D2)) number += damroll(3, 2);
                if  (r_ptr->flags1 & (RF1_DROP_4D2)) number += damroll(4, 2);

                /* Friendly monsters do NOT get items! */
                if (is_pet(m_ptr)) number = 0;

                /* Hack -- handle creeping coins */
                coin_type = force_coin;

                /* Drop some objects */
                for (j = 0; j < number; j++)
                {
                        int o_idx;
                        object_type *o_ptr;

                        /* Get local object */
                        q_ptr = &forge;

                        /* Wipe the object */
                        object_wipe(q_ptr);

                        /* Make Gold */
                        if (do_gold && (!do_item || (rand_int(100) < 50)))
                        {
                                /* Make some gold */
                                if (!make_gold(q_ptr)) continue;

                                /* XXX XXX XXX */
                                dump_gold++;
                        }

                        /* Make Object */
                        else
                        {
                                /* Make an object */
                                if (!make_object(q_ptr, good, great)) continue;

                                /* XXX XXX XXX */
                                dump_item++;
                        }

                        /* Get new object */
                        o_idx = o_pop();

                        if(o_idx)
                        {
                                /* Get the item */
                                o_ptr = &o_list[o_idx];

                                /* Structure copy */
                                object_copy(o_ptr, q_ptr);

                                /* Build a stack */
                                o_ptr->next_o_idx = m_ptr->hold_o_idx;

                                o_ptr->held_m_idx = c_ptr->m_idx;
                                o_ptr->ix = 0;
                                o_ptr->iy = 0;

                                m_ptr->hold_o_idx = o_idx;
                        }
                }

                /* Reset the object level */
                object_level = dun_level;

                /* Reset "coin" type */
                coin_type = 0;
        }

	/* Unknown distance */
	m_ptr->cdis = 0;

	/* No flags */
	m_ptr->mflag = 0;

	/* Not visible */
	m_ptr->ml = FALSE;

        /* The image has your level. */
        m_ptr->level = p_ptr->lev;

	/* Summoned? */
	m_ptr->summoned = dur;
	if (m_ptr->summoned > 0) m_ptr->no_experience = TRUE;
	if (m_ptr->summoned < 0)
	{
		m_ptr->summoned = 0;
		m_ptr->no_experience = TRUE;
	}

	/* Lives */
	m_ptr->lives = r_ptr->lives;

	/* To keep up Nightmares difficulty! :) */
	if (r_ptr->cursed > 0) m_ptr->lives += multiply_divide(m_ptr->lives, m_ptr->level * 3, 100);

        /* The image has the same hp as you, more if you invest in the */
        /* Mirror Images ability. */
        m_ptr->maxhp = p_ptr->mhp;
	m_ptr->maxhp *= p_ptr->abilities[(CLASS_MAGE * 10) + 5];

	/* And start out fully healthy */
	m_ptr->hp = m_ptr->maxhp;

        /* Hit rate! */
        m_ptr->hitrate = 0;

	/* Stats */
	m_ptr->str = 0;
	m_ptr->dex = 0;
	m_ptr->mind = 0;
	m_ptr->skill_attack = 0;
	m_ptr->skill_ranged = 0;
	m_ptr->skill_magic = 0;
	m_ptr->mana = 0;

        /* Defense! */
        m_ptr->defense = r_ptr->ac + ((r_ptr->ac * ((m_ptr->level - 1) * 5)) / 100);

	/* Mana! */
	m_ptr->mana = (m_ptr->mind - 5) * 10;
	if (m_ptr->mana < 0) m_ptr->mana = 0;

	/* Extract the monster base speed */
	m_ptr->mspeed = r_ptr->speed;
	
        /* Level will now boost the speed a little... */
        {
		int a, b;

		b = (m_ptr->level / 5);

		for (a = 0; a < b; a++)
		{
        		m_ptr->mspeed += 1;
			if (m_ptr->mspeed > 180)
			{
				m_ptr->mspeed = 180;
				break;
			}
		}
	}

	/* Give a random starting energy */
	/*m_ptr->energy = (byte)rand_int(100);*/
	m_ptr->energy = 100;

	/* Force monster to wait for player */
	if (r_ptr->flags1 & (RF1_FORCE_SLEEP))
	{
		/* Monster is still being nice */
		m_ptr->mflag |= (MFLAG_NICE);

		/* Must repair monsters */
		repair_monsters = TRUE;
	}

	/* Hack -- see "process_monsters()" */
	if (c_ptr->m_idx < hack_m_idx)
	{
		/* Monster is still being born */
		m_ptr->mflag |= (MFLAG_BORN);
	}


	/* Update the monster */
	update_mon(c_ptr->m_idx, TRUE);


	/* Hack -- Count the monsters on the level */
	r_ptr->cur_num++;

        /* Unique monsters on saved levels should be "marked" */
        if ((r_ptr->flags1 & RF1_UNIQUE) && get_dungeon_save(dummy))
        {
                r_ptr->on_saved = TRUE;
        }

	/* Hack -- Count the number of "reproducers" */
	if (r_ptr->flags2 & (RF2_MULTIPLY)) num_repro++;


	/* Hack -- Notice new multi-hued monsters */
	if (r_ptr->flags1 & (RF1_ATTR_MULTI)) shimmer_monsters = TRUE;

	/* Call a lua script. */
	if (r_ptr->event_spawn > 0)
	{
		call_lua("monster_spawn", "(dd)", "", c_ptr->m_idx, r_ptr->event_spawn);
	}

	/* Success */
	return (TRUE);
}

/* Place an animated monster! */
s16b place_monster_animated(int y, int x, int r_idx, bool slp, bool charm, s32b basehp, s32b hit_bonus, int d_d, int d_s)
{
        int             i;
        char            dummy[5];

	cave_type		*c_ptr;

	monster_type	*m_ptr;

	monster_race	*r_ptr = &r_info[r_idx];

	cptr		name = (r_name + r_ptr->name);


	/* Verify location */
        if (!in_bounds(y, x)) return 0;

	/* Require empty space */
        if (!cave_empty_bold(y, x)) return 0;

	/* Hack -- no creation on glyph of warding */
        if (cave[y][x].feat == FEAT_GLYPH) return 0;
        if (cave[y][x].feat == FEAT_MINOR_GLYPH) return 0;

        /* Nor on the between */
        if (cave[y][x].feat == FEAT_BETWEEN) return 0;

	/* Nor on the Pattern */
	if ((cave[y][x].feat >= FEAT_PATTERN_START)
	 && (cave[y][x].feat <= FEAT_PATTERN_XTRA2))
                return 0;

	/* Paranoia */
        if (!r_idx) return 0;

	/* Paranoia */
        if (!r_ptr->name) return 0;

	if (!monster_can_cross_terrain(cave[y][x].feat, r_ptr))
	{
		return FALSE;
	}

	/* Hack -- "unique" monsters must be "unique" */
	if ((r_ptr->flags1 & (RF1_UNIQUE)) && (r_ptr->cur_num >= r_ptr->max_num))
	{
		/* Cannot create */
                return 0;
	}

	/* Depth monsters may NOT be created out of depth */
	if ((r_ptr->flags1 & (RF1_FORCE_DEPTH)) && (dun_level < r_ptr->level))
	{
		/* Cannot create */
                return 0;
	}

        /* The monster is already on an unique level */
        if (r_ptr->on_saved) return (FALSE);


	/* Powerful monster */
	if (r_ptr->level > dun_level)
	{
		/* Unique monsters */
		if (r_ptr->flags1 & (RF1_UNIQUE))
		{
			/* Message for cheaters */
                        if ((cheat_hear)) msg_format("Deep Unique (%s).", name);

			/* Boost rating by twice delta-depth */
			rating += (r_ptr->level - dun_level) * 2;
		}

		/* Normal monsters */
		else
		{
			/* Message for cheaters */
                        if ((cheat_hear)) msg_format("Deep Monster (%s).", name);

			/* Boost rating by delta-depth */
			rating += (r_ptr->level - dun_level);
		}
	}

	/* Note the monster */
	else if (r_ptr->flags1 & (RF1_UNIQUE))
	{
		/* Unique monsters induce message */
                if ((cheat_hear)) msg_format("Unique (%s).", name);
	}


	/* Access the location */
	c_ptr = &cave[y][x];

	/* Make a new monster */
	c_ptr->m_idx = m_pop();
	hack_m_idx_ii = c_ptr->m_idx;

	/* Mega-Hack -- catch "failure" */
        if (!c_ptr->m_idx) return 0;


	/* Get a new monster record */
	m_ptr = &m_list[c_ptr->m_idx];

	/* Save the race */
	m_ptr->r_idx = r_idx;

	/* Place the monster at the location */
	m_ptr->fy = y;
	m_ptr->fx = x;


	/* No "damage" yet */
	m_ptr->stunned = 0;
	m_ptr->confused = 0;
	m_ptr->monfear = 0;

        /* NOT a boss! */
        m_ptr->boss = 0;

	/* Not hasted nor boosted */
	m_ptr->hasted = 0;
	m_ptr->boosted = 0;

	/* Not summoned */
	m_ptr->summoned = 0;

	/* No lives */
	m_ptr->lives = 0;

	/* Worth experience. */
	m_ptr->no_experience = FALSE;

        /* Not hit by Sealing Light */
        m_ptr->seallight = 0;

        /* This IS an animated monster! */
        m_ptr->animated = TRUE;
        m_ptr->animdam_d = d_d;
        m_ptr->animdam_s = d_s;

	/* Friendly? */
        if ( charm || (r_ptr->flags7 & RF7_PET) )
	{
		set_pet(m_ptr, TRUE);
	}

	/* Assume no sleeping */
	m_ptr->csleep = 0;

	/* Enforce sleeping if needed */
        if (slp && r_ptr->sleep && m_ptr->boss == 0)
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

        /* Give the monster a level */
        m_ptr->level = p_ptr->lev + ((p_ptr->abilities[(CLASS_MAGE * 10) + 9] - 1) * 2);

        /* Give the hp */
        m_ptr->maxhp = (basehp * 100) * p_ptr->abilities[(CLASS_MAGE * 10) + 9];

	/* And start out fully healthy */
        m_ptr->hp = m_ptr->maxhp;

	/* Lives */
	m_ptr->lives = r_ptr->lives;

	/* To keep up Nightmares difficulty! :) */
	if (r_ptr->cursed > 0) m_ptr->lives += multiply_divide(m_ptr->lives, m_ptr->level * 3, 100);

	/* Extract the monster base speed */
	m_ptr->mspeed = r_ptr->speed;

	/* Now, determine the stats! */
	m_ptr->str = 10 + p_ptr->abilities[(CLASS_MAGE * 10) + 9];
	m_ptr->dex = 10 + p_ptr->abilities[(CLASS_MAGE * 10) + 9];
	m_ptr->mind = 10 + p_ptr->abilities[(CLASS_MAGE * 10) + 9];
	m_ptr->skill_attack = 10 + p_ptr->abilities[(CLASS_MAGE * 10) + 9];
	m_ptr->skill_ranged = 10 + p_ptr->abilities[(CLASS_MAGE * 10) + 9];
	m_ptr->skill_magic = 10 + p_ptr->abilities[(CLASS_MAGE * 10) + 9];

	if (m_ptr->str > 2000000000 || m_ptr->str < 0) m_ptr->str = 2000000000;
	if (m_ptr->dex > 2000000000 || m_ptr->dex < 0) m_ptr->dex = 2000000000;
	if (m_ptr->mind > 2000000000 || m_ptr->mind < 0) m_ptr->mind = 2000000000;
	if (m_ptr->skill_attack > 2000000000 || m_ptr->skill_attack < 0) m_ptr->skill_attack = 2000000000;
	if (m_ptr->skill_ranged > 2000000000 || m_ptr->skill_ranged < 0) m_ptr->skill_ranged = 2000000000;
	if (m_ptr->skill_magic > 2000000000 || m_ptr->skill_magic < 0) m_ptr->skill_magic = 2000000000;

        /* Hit rate! */
        m_ptr->hitrate = ((m_ptr->dex - 5) * 10) + 2;
	m_ptr->hitrate += ((m_ptr->str - 5) / 2);
	m_ptr->hitrate = m_ptr->hitrate + ((m_ptr->hitrate * ((m_ptr->dex - 5) * 5)) / 100);
	m_ptr->hitrate += multiply_divide(m_ptr->hitrate, ((m_ptr->dex - 5) * 5), 100);
	m_ptr->hitrate += hit_bonus;
	m_ptr->hitrate *= p_ptr->abilities[(CLASS_MAGE * 10) + 9];

        /* Defense! */
	m_ptr->defense = r_ptr->ac + multiply_divide(r_ptr->ac, ((m_ptr->level - 1) * 5), 100);

	/* Mana! */
	m_ptr->mana = (m_ptr->mind - 5) * 10;
	if (m_ptr->mana < 0) m_ptr->mana = 0;

        /* Level will now boost the speed a little... */
        {
		int a, b;

		b = (m_ptr->level / 5);

		for (a = 0; a < b; a++)
		{
        		m_ptr->mspeed += 1;
			if (m_ptr->mspeed > 180)
			{
				m_ptr->mspeed = 180;
				break;
			}
		}
	}

	/* Give a random starting energy */
	/*m_ptr->energy = (byte)rand_int(100);*/
	m_ptr->energy = 100;

	/* Force monster to wait for player */
	if (r_ptr->flags1 & (RF1_FORCE_SLEEP))
	{
		/* Monster is still being nice */
		m_ptr->mflag |= (MFLAG_NICE);

		/* Must repair monsters */
		repair_monsters = TRUE;
	}

        /* Give the imprint, so they are controllable! */
        /* Bone Walls however aren't imprinted... */
        if (m_ptr->r_idx != 1116) m_ptr->imprinted = TRUE;

        /* Yeah, all right, they're also our friends! */
        m_ptr->friend = TRUE;

	/* Hack -- see "process_monsters()" */
	if (c_ptr->m_idx < hack_m_idx)
	{
		/* Monster is still being born */
		m_ptr->mflag |= (MFLAG_BORN);
	}


	/* Update the monster */
	update_mon(c_ptr->m_idx, TRUE);


	/* Hack -- Count the monsters on the level */
	r_ptr->cur_num++;

        /* Unique monsters on saved levels should be "marked" */
        if ((r_ptr->flags1 & RF1_UNIQUE) && get_dungeon_save(dummy))
        {
                r_ptr->on_saved = TRUE;
        }

	/* Hack -- Count the number of "reproducers" */
	if (r_ptr->flags2 & (RF2_MULTIPLY)) num_repro++;


	/* Hack -- Notice new multi-hued monsters */
	if (r_ptr->flags1 & (RF1_ATTR_MULTI)) shimmer_monsters = TRUE;

	/* Call a lua script. */
	if (r_ptr->event_spawn > 0)
	{
		call_lua("monster_spawn", "(dd)", "", c_ptr->m_idx, r_ptr->event_spawn);
	}

	/* Success */
        return c_ptr->m_idx;
}

bool summon_specific_kind(int y1, int x1, int lev, char kind, bool Group_ok, bool friendly, int dur)
{
	int i, x, y, r_idx;        

        summoner_monster = NULL;

	/* Look for a location */
	for (i = 0; i < 20; ++i)
	{
		/* Pick a distance */
		int d = (i / 15) + 1;

		/* Pick a location */
		scatter(&y, &x, y1, x1, d, 0);

		/* Require "empty" floor grid */
		if (!cave_empty_bold(y, x)) continue;

		/* Hack -- no summon on glyph of warding */
		 if (cave[y][x].feat == FEAT_GLYPH) continue;
		 if (cave[y][x].feat == FEAT_MINOR_GLYPH) continue;

                /* Nor on the between */
                if (cave[y][x].feat == FEAT_BETWEEN) return (FALSE);

		/* ... nor on the Pattern */
		if ((cave[y][x].feat >= FEAT_PATTERN_START) &&
		    (cave[y][x].feat <= FEAT_PATTERN_XTRA2))
			continue;

		/* Okay */
		break;
	}

	/* Failure */
	if (i == 20) return (FALSE);

	/* Prepare allocation table */
	get_mon_num_prep();

	/* Pick a monster, using the level calculation */
        r_idx = get_mon_num_kind(lev, kind);

        /* Do we have a valid r_idx? */
        if (r_idx == 0) return (FALSE);

	/* Prepare allocation table */
	get_mon_num_prep();

	/* Handle failure */
	if (!r_idx) return (FALSE);

	/* Attempt to place the monster (awake, allow groups) */
	if (friendly)
	{ 
		if (!place_monster_aux(y, x, r_idx, FALSE, Group_ok, TRUE, dur)) return (FALSE);
	}
	else if (!place_monster_aux(y, x, r_idx, FALSE, Group_ok, FALSE, dur)) return (FALSE);

	/* Success */
	return (TRUE);
}

bool summon_specific_ridx(int y1, int x1, int ridx, bool Group_ok, bool friendly, int dur)
{
	int i, x, y, r_idx;        

        summoner_monster = NULL;

	/* Look for a location */
	for (i = 0; i < 20; ++i)
	{
		/* Pick a distance */
		int d = (i / 15) + 1;

		/* Pick a location */
		scatter(&y, &x, y1, x1, d, 0);

		/* Require "empty" floor grid */
		if (!cave_empty_bold(y, x)) continue;

		/* Hack -- no summon on glyph of warding */
		 if (cave[y][x].feat == FEAT_GLYPH) continue;
		 if (cave[y][x].feat == FEAT_MINOR_GLYPH) continue;

                /* Nor on the between */
                if (cave[y][x].feat == FEAT_BETWEEN) return (FALSE);

		/* ... nor on the Pattern */
		if ((cave[y][x].feat >= FEAT_PATTERN_START) &&
		    (cave[y][x].feat <= FEAT_PATTERN_XTRA2))
			continue;

		/* Okay */
		break;
	}

	/* Failure */
	if (i == 20) return (FALSE);

	/* Prepare allocation table */
	get_mon_num_prep();

	/* Pick a monster, using the level calculation */
        r_idx = ridx;

        /* Do we have a valid r_idx? */
        if (r_idx == 0) return (FALSE);

	/* Prepare allocation table */
	get_mon_num_prep();

	/* Handle failure */
	if (!r_idx) return (FALSE);
	
	/* Attempt to place the monster (awake, allow groups) */
	summoning_specific = TRUE;
	if (friendly) 
	{ 
		if (!place_monster_aux(y, x, r_idx, FALSE, Group_ok, TRUE, dur))
		{
			summoning_specific = FALSE;
			return (FALSE);
		}
	}
	else if (!place_monster_aux(y, x, r_idx, FALSE, Group_ok, FALSE, dur))
	{
		summoning_specific = FALSE;
		return (FALSE);
	}

	summoning_specific = FALSE;

	/* Success */
	return (TRUE);
}

bool summon_specific_rflag(int y1, int x1, int lev, u32b rflag, bool Group_ok, bool friendly, int dur)
{
	int i, x, y, r_idx;        

        summoner_monster = NULL;

	/* Look for a location */
	for (i = 0; i < 20; ++i)
	{
		/* Pick a distance */
		int d = (i / 15) + 1;

		/* Pick a location */
		scatter(&y, &x, y1, x1, d, 0);

		/* Require "empty" floor grid */
		if (!cave_empty_bold(y, x)) continue;

		/* Hack -- no summon on glyph of warding */
		 if (cave[y][x].feat == FEAT_GLYPH) continue;
		 if (cave[y][x].feat == FEAT_MINOR_GLYPH) continue;

                /* Nor on the between */
                if (cave[y][x].feat == FEAT_BETWEEN) return (FALSE);

		/* ... nor on the Pattern */
		if ((cave[y][x].feat >= FEAT_PATTERN_START) &&
		    (cave[y][x].feat <= FEAT_PATTERN_XTRA2))
			continue;

		/* Okay */
		break;
	}

	/* Failure */
	if (i == 20) return (FALSE);

	/* Prepare allocation table */
	get_mon_num_prep();

	/* Pick a monster, using the level calculation */
        r_idx = get_mon_num_rflag(lev, rflag);

        /* Do we have a valid r_idx? */
        if (r_idx == 0) return (FALSE);

	/* Prepare allocation table */
	get_mon_num_prep();

	/* Handle failure */
	if (!r_idx) return (FALSE);

	/* Attempt to place the monster (awake, allow groups) */
	if (friendly)
	{ 
		if (!place_monster_aux(y, x, r_idx, FALSE, Group_ok, TRUE, dur)) return (FALSE);
	}
	else if (!place_monster_aux(y, x, r_idx, FALSE, Group_ok, FALSE, dur)) return (FALSE);

	/* Success */
	return (TRUE);
}

/* Don't change the hp. Just the stats. */
void apply_monster_level_stats(monster_type *m_ptr)
{
	bool scaled = FALSE;
	int scaledlevel;
        monster_race    *r_ptr = &r_info[m_ptr->r_idx];

	if (r_ptr->flags7 & (RF7_SCALED))
	{
		scaled = TRUE;
		if (p_ptr->lev > r_ptr->level) scaledlevel = p_ptr->lev;
		else scaledlevel = r_ptr->level;
	}
	

	/* Now, determine the stats! */
	if (scaled)
	{
		m_ptr->str = multiply_divide(scaledlevel, r_ptr->str, 100) + multiply_divide(multiply_divide(scaledlevel, r_ptr->str, 100), ((m_ptr->level - 1) * 5), 100);
		m_ptr->dex = multiply_divide(scaledlevel, r_ptr->dex, 100) + multiply_divide(multiply_divide(scaledlevel, r_ptr->dex, 100), ((m_ptr->level - 1) * 5), 100);
		m_ptr->mind = multiply_divide(scaledlevel, r_ptr->mind, 100) + multiply_divide(multiply_divide(scaledlevel, r_ptr->mind, 100), ((m_ptr->level - 1) * 5), 100);
		m_ptr->skill_attack = multiply_divide(scaledlevel, r_ptr->skill_attack, 100) + multiply_divide(multiply_divide(scaledlevel, r_ptr->skill_attack, 100), ((m_ptr->level - 1) * 5), 100);
		m_ptr->skill_ranged = multiply_divide(scaledlevel, r_ptr->skill_ranged, 100) + multiply_divide(multiply_divide(scaledlevel, r_ptr->skill_ranged, 100), ((m_ptr->level - 1) * 5), 100);
		m_ptr->skill_magic = multiply_divide(scaledlevel, r_ptr->skill_magic, 100) + multiply_divide(multiply_divide(scaledlevel, r_ptr->skill_magic, 100), ((m_ptr->level - 1) * 5), 100);
	}
	else
	{
		m_ptr->str = r_ptr->str + multiply_divide(r_ptr->str, ((m_ptr->level - 1) * 5), 100);
		m_ptr->dex = r_ptr->dex + multiply_divide(r_ptr->dex, ((m_ptr->level - 1) * 5), 100);
		m_ptr->mind = r_ptr->mind + multiply_divide(r_ptr->mind, ((m_ptr->level - 1) * 5), 100);
		m_ptr->skill_attack = r_ptr->skill_attack + multiply_divide(r_ptr->skill_attack, ((m_ptr->level - 1) * 5), 100);
		m_ptr->skill_ranged = r_ptr->skill_ranged + multiply_divide(r_ptr->skill_ranged, ((m_ptr->level - 1) * 5), 100);
		m_ptr->skill_magic = r_ptr->skill_magic + multiply_divide(r_ptr->skill_magic, ((m_ptr->level - 1) * 5), 100);
	}

	if (m_ptr->boss == 1)
	{
		m_ptr->str += multiply_divide(m_ptr->str, (m_ptr->level / 3), 100);
		m_ptr->dex += multiply_divide(m_ptr->dex, (m_ptr->level / 3), 100);
		m_ptr->mind += multiply_divide(m_ptr->mind, (m_ptr->level / 3), 100);
		m_ptr->skill_attack += multiply_divide(m_ptr->skill_attack, (m_ptr->level / 3), 100);
		m_ptr->skill_ranged += multiply_divide(m_ptr->skill_ranged, (m_ptr->level / 3), 100);
		m_ptr->skill_magic += multiply_divide(m_ptr->skill_magic, (m_ptr->level / 3), 100);
	}

	if (m_ptr->boss == 2 || r_ptr->cursed > 0)
	{
		m_ptr->str += multiply_divide(m_ptr->str, m_ptr->level + r_ptr->cursed, 100);
		m_ptr->dex += multiply_divide(m_ptr->dex, m_ptr->level + r_ptr->cursed, 100);
		m_ptr->mind += multiply_divide(m_ptr->mind, m_ptr->level + r_ptr->cursed, 100);
		m_ptr->skill_attack += multiply_divide(m_ptr->skill_attack, m_ptr->level + r_ptr->cursed, 100);
		m_ptr->skill_ranged += multiply_divide(m_ptr->skill_ranged, m_ptr->level + r_ptr->cursed, 100);
		m_ptr->skill_magic += multiply_divide(m_ptr->skill_magic, m_ptr->level + r_ptr->cursed, 100);
	}
	if (m_ptr->str > 2000000000 || m_ptr->str < 0) m_ptr->str = 2000000000;
	if (m_ptr->dex > 2000000000 || m_ptr->dex < 0) m_ptr->dex = 2000000000;
	if (m_ptr->mind > 2000000000 || m_ptr->mind < 0) m_ptr->mind = 2000000000;
	if (m_ptr->skill_attack > 2000000000 || m_ptr->skill_attack < 0) m_ptr->skill_attack = 2000000000;
	if (m_ptr->skill_ranged > 2000000000 || m_ptr->skill_ranged < 0) m_ptr->skill_ranged = 2000000000;
	if (m_ptr->skill_magic > 2000000000 || m_ptr->skill_magic < 0) m_ptr->skill_magic = 2000000000;

        /* Hit rate! */
        m_ptr->hitrate = ((m_ptr->dex - 5) * 10) + 2;
	m_ptr->hitrate += ((m_ptr->str - 5) / 2);
	m_ptr->hitrate += multiply_divide(m_ptr->hitrate, ((m_ptr->dex - 5) * 5), 100);

        /* Defense! */
	if (scaled) m_ptr->defense = multiply_divide(scaledlevel, r_ptr->ac, 100) + multiply_divide(r_ptr->ac, ((m_ptr->level - 1) * 5), 100);
	else m_ptr->defense = r_ptr->ac + multiply_divide(r_ptr->ac, ((m_ptr->level - 1) * 5), 100);

	/* Mana! */
	m_ptr->mana = (m_ptr->mind - 5) * 10;
	if (m_ptr->mana < 0) m_ptr->mana = 0;

	if (m_ptr->boss == 1) m_ptr->defense += multiply_divide(m_ptr->defense, (m_ptr->level * 5), 100);
	if (m_ptr->boss == 2 || r_ptr->cursed > 0) m_ptr->defense += multiply_divide(m_ptr->defense, (m_ptr->level * 10), 100);
}

/* Compare your current monster with an evolution. */
void evolution_compare(int r_idx, bool compare, bool recall)
{
	int mode;
	int i;
	bool comparing = TRUE;
	monster_race *r_ptr;
	monster_race *b_ptr;
	char c;
	char str[80];
	bool scaled = FALSE;
	int scaledlevel;
	int colorterm;

	/* Initialise monster race pointers. */
	r_ptr = &r_info[r_idx];
	if (recall) b_ptr = &r_info[r_idx];
	else b_ptr = &r_info[p_ptr->body_monster];

	if (recall && b_ptr->cursed > 0)
	{
		msg_print("You have no clues about the powers of this monster.");
		return;
	}

	if (r_ptr->flags7 & (RF7_SCALED))
	{
		scaled = TRUE;
		if (p_ptr->max_plv > r_ptr->level) scaledlevel = p_ptr->max_plv;
		else scaledlevel = r_ptr->level;
		colorterm = TERM_INDIGO;
	}
	else colorterm = TERM_L_GREEN;

	/* Save what's on screen */
        Term_save();

	/* Flush messages */
	msg_print(NULL);

        /* Begin... */
	Term_erase(0, 1, 255);

	/* Start with mode 0. */
	mode = 0;

	while (comparing)
	{
		/* Mode 0: Base stats comparison. */
		if (mode == 0)
		{
			/* Prepare the screen */
			Term_erase(0, 1, 255);
        		for (i = 0; i < SCREEN_HGT; i++)
        		{
                		roff("\n");
        		}
			
        		c_put_str(TERM_WHITE, "MONSTER STATS                                                               ", 0, 0);
			c_put_str(TERM_WHITE, "------------------", 1, 0);

			/* Current monster */

			if (compare)
			{
				c_put_str(TERM_WHITE, "Current Monster: ", 2, 0);
				c_put_str(colorterm, b_ptr->name_char, 2, 17);
			}
			else
			{
				c_put_str(TERM_WHITE, "Monster: ", 2, 0);
				c_put_str(colorterm, b_ptr->name_char, 2, 9);
			}
			c_put_str(TERM_WHITE, "Depth: ", 3, 0);
			if (scaled) sprintf(str, "Unknown");
			else sprintf(str, "%d", b_ptr->level);
			c_put_str(colorterm, str, 3, 7);
			c_put_str(TERM_WHITE, "Type: ", 4, 0);
			if (b_ptr->body_parts[BODY_WEAPON]) sprintf(str, "Humanoid Monster");
			else sprintf(str, "Essence Monster");
			c_put_str(colorterm, str, 4, 6);

			c_put_str(TERM_WHITE, "--- BASE STATS ---", 5, 0);
			if (recall)
			{
				if (scaled)
				{
					c_put_str(TERM_WHITE, "Base Hp  : ", 6, 0);
					sprintf(str, "Unknown");
					c_put_str(TERM_INDIGO, str, 6, 11);
				}
				else
				{
					if (b_ptr->flags1 & (RF1_FORCE_MAXHP))
					{
						c_put_str(TERM_WHITE, "Base Hp  : ", 6, 0);
						sprintf(str, "%ld", maxroll(b_ptr->hdice, b_ptr->hside));
						c_put_str(TERM_L_GREEN, str, 6, 11);
					}
					else
					{
						c_put_str(TERM_WHITE, "Hp Roll  : ", 6, 0);
						sprintf(str, "%dd%d", b_ptr->hdice, b_ptr->hside);
						c_put_str(TERM_L_GREEN, str, 6, 11);
					}
				}
			}
			else
			{
				c_put_str(TERM_WHITE, "Hp Bonus : ", 6, 0);
				sprintf(str, "%ld", maxroll(b_ptr->hdice, b_ptr->hside) / 2);
				c_put_str(TERM_L_GREEN, str, 6, 11);
			}

			if (scaled)
			{
				c_put_str(TERM_WHITE, "Strength : ", 7, 0);
				sprintf(str, "Unknown");
				c_put_str(TERM_INDIGO, str, 7, 11);
				c_put_str(TERM_WHITE, "Dexterity: ", 8, 0);
				sprintf(str, "Unknown");
				c_put_str(TERM_INDIGO, str, 8, 11);
				c_put_str(TERM_WHITE, "Mind     : ", 9, 0);
				sprintf(str, "Unknown");
				c_put_str(TERM_INDIGO, str, 9, 11);

				if (recall)
				{
					c_put_str(TERM_WHITE, "Attack Skill: ", 7, 20);
					sprintf(str, "Unknown");
					c_put_str(TERM_INDIGO, str, 7, 34);
					c_put_str(TERM_WHITE, "Ranged Skill: ", 8, 20);
					sprintf(str, "Unknown");
					c_put_str(TERM_INDIGO, str, 8, 34);
					c_put_str(TERM_WHITE, "Magic Skill : ", 9, 20);
					sprintf(str, "Unknown");
					c_put_str(TERM_INDIGO, str, 9, 34);
				}
			}
			else
			{
				c_put_str(TERM_WHITE, "Strength : ", 7, 0);
				sprintf(str, "%ld", b_ptr->str);
				c_put_str(TERM_L_GREEN, str, 7, 11);
				c_put_str(TERM_WHITE, "Dexterity: ", 8, 0);
				sprintf(str, "%ld", b_ptr->dex);
				c_put_str(TERM_L_GREEN, str, 8, 11);
				c_put_str(TERM_WHITE, "Mind     : ", 9, 0);
				sprintf(str, "%ld", b_ptr->mind);
				c_put_str(TERM_L_GREEN, str, 9, 11);

				if (recall)
				{
					c_put_str(TERM_WHITE, "Attack Skill: ", 7, 20);
					sprintf(str, "%ld", r_ptr->skill_attack);
					c_put_str(TERM_L_GREEN, str, 7, 34);
					c_put_str(TERM_WHITE, "Ranged Skill: ", 8, 20);
					sprintf(str, "%ld", r_ptr->skill_ranged);
					c_put_str(TERM_L_GREEN, str, 8, 34);
					c_put_str(TERM_WHITE, "Magic Skill : ", 9, 20);
					sprintf(str, "%ld", r_ptr->skill_magic);
					c_put_str(TERM_L_GREEN, str, 9, 34);
				}
			}

			c_put_str(TERM_WHITE, "--- MISC STATS ---", 10, 0);

			if (scaled)
			{
				c_put_str(TERM_WHITE, "Base AC  : ", 11, 0);
				sprintf(str, "Unknown");
				c_put_str(TERM_INDIGO, str, 11, 11);
				c_put_str(TERM_WHITE, "Speed    : ", 12, 0);
				if (r_ptr->flags7 & (RF7_VERY_FAST)) sprintf(str, "%d {VERY FAST}", b_ptr->speed - 110);
				else if (r_ptr->flags7 & (RF7_FAST)) sprintf(str, "%d {FAST}", b_ptr->speed - 110);
				else sprintf(str, "%d", b_ptr->speed - 110);
				c_put_str(TERM_INDIGO, str, 12, 11);
				c_put_str(TERM_WHITE, "Attacks  : ", 13, 0);
				sprintf(str, "%d", b_ptr->attacks);
				c_put_str(TERM_INDIGO, str, 13, 11);
				c_put_str(TERM_WHITE, "Spells   : ", 14, 0);
				sprintf(str, "%d", b_ptr->spells);
				c_put_str(TERM_INDIGO, str, 14, 11);
			}
			else
			{
				c_put_str(TERM_WHITE, "Base AC  : ", 11, 0);
				sprintf(str, "%ld", b_ptr->ac);
				c_put_str(TERM_L_GREEN, str, 11, 11);
				c_put_str(TERM_WHITE, "Speed    : ", 12, 0);
				if (r_ptr->flags7 & (RF7_VERY_FAST)) sprintf(str, "%d {VERY FAST}", b_ptr->speed - 110);
				else if (r_ptr->flags7 & (RF7_FAST)) sprintf(str, "%d {FAST}", b_ptr->speed - 110);
				else sprintf(str, "%d", b_ptr->speed - 110);
				c_put_str(TERM_L_GREEN, str, 12, 11);
				c_put_str(TERM_WHITE, "Attacks  : ", 13, 0);
				sprintf(str, "%d", b_ptr->attacks);
				c_put_str(TERM_L_GREEN, str, 13, 11);
				c_put_str(TERM_WHITE, "Spells   : ", 14, 0);
				sprintf(str, "%d", b_ptr->spells);
				c_put_str(TERM_L_GREEN, str, 14, 11);
			}

			if (scaled)
			{
				if (recall)
				{
					c_put_str(TERM_WHITE, "Lives    : ", 11, 20);
					sprintf(str, "Unknown");
					c_put_str(TERM_INDIGO, str, 11, 31);
				}
			}
			else
			{
				if (recall)
				{
					c_put_str(TERM_WHITE, "Lives    : ", 11, 20);
					sprintf(str, "%ld", b_ptr->lives);
					c_put_str(TERM_L_GREEN, str, 11, 31);
				}
			}

			c_put_str(TERM_WHITE, "---  COUNTERS  ---", 15, 0);
			{
				char *countername;
				c_put_str(TERM_WHITE, "Type     : ", 16, 0);
				call_lua("get_counter_name", "(d)", "s", b_ptr->countertype, &countername);
				c_put_str(colorterm, countername, 16, 11);
				c_put_str(TERM_WHITE, "Attempt  : ", 17, 0);
				sprintf(str, "%d%%", b_ptr->counterchance);
				c_put_str(colorterm, str, 17, 11);
			}

			c_put_str(TERM_WHITE, "---    MISC    ---", 18, 0);
			{
				char *line1;
				char *line2;
				char *line3;
				char *line4;
				
				if (recall)
				{
					call_lua("get_misc_monster_info", "(dd)", "s", r_idx, 1, &line1);
					c_put_str(colorterm, line1, 19, 0);
					call_lua("get_misc_monster_info", "(dd)", "s", r_idx, 2, &line2);
					c_put_str(colorterm, line2, 20, 0);
					call_lua("get_misc_monster_info", "(dd)", "s", r_idx, 3, &line3);
					c_put_str(colorterm, line3, 21, 0);
					call_lua("get_misc_monster_info", "(dd)", "s", r_idx, 4, &line4);
					c_put_str(colorterm, line4, 22, 0);
				}
				else
				{
					call_lua("get_misc_monster_info", "(dd)", "s", p_ptr->body_monster, 1, &line1);
					c_put_str(colorterm, line1, 19, 0);
					call_lua("get_misc_monster_info", "(dd)", "s", p_ptr->body_monster, 2, &line2);
					c_put_str(colorterm, line2, 20, 0);
					call_lua("get_misc_monster_info", "(dd)", "s", p_ptr->body_monster, 3, &line3);
					c_put_str(colorterm, line3, 21, 0);
					call_lua("get_misc_monster_info", "(dd)", "s", p_ptr->body_monster, 4, &line4);
					c_put_str(colorterm, line4, 22, 0);
				}
			}

			/* New monster. */
			if (compare)
			{
				c_put_str(TERM_WHITE, "New Monster: ", 2, 40);
				c_put_str(TERM_L_GREEN, r_ptr->name_char, 2, 53);
				c_put_str(TERM_WHITE, "Depth: ", 3, 40);
				sprintf(str, "%d", r_ptr->level);
				c_put_str(TERM_L_GREEN, str, 3, 47);
				c_put_str(TERM_WHITE, "Type: ", 4, 40);
				if (r_ptr->body_parts[BODY_WEAPON]) sprintf(str, "Humanoid Monster");
				else sprintf(str, "Essence Monster");
				c_put_str(TERM_L_GREEN, str, 4, 46);

				c_put_str(TERM_WHITE, "--- BASE STATS ---", 5, 40);
				c_put_str(TERM_WHITE, "Hp Bonus : ", 6, 40);
				sprintf(str, "%ld", maxroll(r_ptr->hdice, r_ptr->hside) / 2);
				c_put_str(TERM_L_GREEN, str, 6, 51);
				c_put_str(TERM_WHITE, "Strength : ", 7, 40);
				sprintf(str, "%ld", r_ptr->str);
				c_put_str(TERM_L_GREEN, str, 7, 51);
				c_put_str(TERM_WHITE, "Dexterity: ", 8, 40);
				sprintf(str, "%ld", r_ptr->dex);
				c_put_str(TERM_L_GREEN, str, 8, 51);
				c_put_str(TERM_WHITE, "Mind     : ", 9, 40);
				sprintf(str, "%ld", r_ptr->mind);
				c_put_str(TERM_L_GREEN, str, 9, 51);

				c_put_str(TERM_WHITE, "--- MISC STATS ---", 10, 40);

				c_put_str(TERM_WHITE, "Base AC  : ", 11, 40);
				sprintf(str, "%ld", r_ptr->ac);
				c_put_str(TERM_L_GREEN, str, 11, 51);
				c_put_str(TERM_WHITE, "Speed    : ", 12, 40);
				if (r_ptr->flags7 & (RF7_VERY_FAST)) sprintf(str, "%d {VERY FAST}", r_ptr->speed - 110);
				else if (r_ptr->flags7 & (RF7_FAST)) sprintf(str, "%d {FAST}", r_ptr->speed - 110);
				else sprintf(str, "%d", r_ptr->speed - 110);
				c_put_str(TERM_L_GREEN, str, 12, 51);
				c_put_str(TERM_WHITE, "Attacks  : ", 13, 40);
				sprintf(str, "%d", r_ptr->attacks);
				c_put_str(TERM_L_GREEN, str, 13, 51);
				c_put_str(TERM_WHITE, "Spells   : ", 14, 40);
				sprintf(str, "%d", r_ptr->spells);
				c_put_str(TERM_L_GREEN, str, 14, 51);
				c_put_str(TERM_WHITE, "---  COUNTERS  ---", 15, 40);
				{
					char *countername;
					c_put_str(TERM_WHITE, "Type     : ", 16, 40);
					call_lua("get_counter_name", "(d)", "s", r_ptr->countertype, &countername);
					c_put_str(TERM_L_GREEN, countername, 16, 51);
					c_put_str(TERM_WHITE, "Attempt  : ", 17, 40);
					sprintf(str, "%d%%", r_ptr->counterchance);
					c_put_str(TERM_L_GREEN, str, 17, 51);
				}
				c_put_str(TERM_WHITE, "---    MISC    ---", 18, 0);
				{
					char *line1;
					char *line2;
					char *line3;
					char *line4;
				
					call_lua("get_misc_monster_info", "(dd)", "s", r_idx, 1, &line1);
					c_put_str(TERM_L_GREEN, line1, 19, 40);
					call_lua("get_misc_monster_info", "(dd)", "s", r_idx, 2, &line2);
					c_put_str(TERM_L_GREEN, line2, 20, 40);
					call_lua("get_misc_monster_info", "(dd)", "s", r_idx, 3, &line3);
					c_put_str(TERM_L_GREEN, line3, 21, 40);
					call_lua("get_misc_monster_info", "(dd)", "s", r_idx, 4, &line4);
					c_put_str(TERM_L_GREEN, line4, 22, 40);
				}
			}
		}
		/* Mode 1: Attacks comparison. */
		if (mode == 1)
		{
			int x;
			char attackname[160];
			/* Prepare the screen */
			Term_erase(0, 1, 255);
        		for (i = 0; i < SCREEN_HGT; i++)
        		{
                		roff("\n");
        		}
			
        		c_put_str(TERM_WHITE, "MONSTER ATTACKS                                                                 ", 0, 0);
			c_put_str(TERM_WHITE, "--------------------", 1, 0);

			/* Current monster */

			if (compare)
			{
				c_put_str(TERM_WHITE, "Current Monster: ", 2, 0);
				c_put_str(colorterm, b_ptr->name_char, 2, 17);
			}
			else
			{
				c_put_str(TERM_WHITE, "Monster: ", 2, 0);
				c_put_str(colorterm, b_ptr->name_char, 2, 9);
			}

			c_put_str(TERM_WHITE, "--- ATTACKS ---", 3, 0);
			for (x = 0; x < 20; x++)
			{
				if (b_ptr->attack[x].type > 0)
				{
					if (recall) sprintf(attackname, "%s", get_monster_attack_name_short(r_idx, x));
					else sprintf(attackname, "%s", get_monster_attack_name_short(p_ptr->body_monster, x));
					if (scaled) c_put_str(TERM_INDIGO, attackname, 4+x, 0);
					else c_put_str(TERM_WHITE, attackname, 4+x, 0);
				}
			}
			

			/* New monster. */
			if (compare)
			{
				c_put_str(TERM_WHITE, "New Monster: ", 2, 40);
				c_put_str(TERM_L_GREEN, r_ptr->name_char, 2, 53);

				c_put_str(TERM_WHITE, "--- ATTACKS ---", 3, 40);
				for (x = 0; x < 20; x++)
				{
					if (r_ptr->attack[x].type > 0)
					{
						sprintf(attackname, "%s", get_monster_attack_name_short(r_idx, x));
						c_put_str(TERM_WHITE, attackname, 4+x, 40);
					}
				}
			}
		}
		/* Mode 2: Spells comparison. */
		if (mode == 2)
		{
			int x;
			char spellname[160];
			/* Prepare the screen */
			Term_erase(0, 1, 255);
        		for (i = 0; i < SCREEN_HGT; i++)
        		{
                		roff("\n");
        		}
			
        		c_put_str(TERM_WHITE, "MONSTER SPELLS                                                                  ", 0, 0);
			c_put_str(TERM_WHITE, "-------------------", 1, 0);

			/* Current monster */

			if (compare)
			{
				c_put_str(TERM_WHITE, "Current Monster: ", 2, 0);
				c_put_str(colorterm, b_ptr->name_char, 2, 17);
			}
			else
			{
				c_put_str(TERM_WHITE, "Monster: ", 2, 0);
				c_put_str(colorterm, b_ptr->name_char, 2, 9);
			}

			c_put_str(TERM_WHITE, "--- SPELLS ---", 3, 0);
			for (x = 0; x < 20; x++)
        		{
				if (scaled)
				{
					if (b_ptr->spell[x].type > 0)
					{
						char extra[160];
						if (b_ptr->spell[x].type == 1)
						{
							sprintf(extra, "(%s)", get_element_name(b_ptr->spell[x].special1));
                					sprintf(spellname, "%s %s", b_ptr->spell[x].name, extra);
						}
						else if (b_ptr->spell[x].type == 2)
						{
							sprintf(extra, "(%s, rad ??)", get_element_name(b_ptr->spell[x].special1), b_ptr->spell[x].special2);
                					sprintf(spellname, "%s %s", b_ptr->spell[x].name, extra);
						}
						else if (b_ptr->spell[x].type == 3 || b_ptr->spell[x].type == 4 || b_ptr->spell[x].type == 5 || b_ptr->spell[x].type == 6 || b_ptr->spell[x].type == 7 || b_ptr->spell[x].type == 8)
						{
                					sprintf(spellname, "%s", b_ptr->spell[x].name);
						}
						else if (b_ptr->spell[x].type == 9)
						{
							char *powname;

							/* Calls lua! */
							if (recall) call_lua("get_scripted_spell_name", "(dd)", "s", r_idx, x+1, &powname);
							else call_lua("get_scripted_spell_name", "(dd)", "s", p_ptr->body_monster, x+1, &powname);

                					sprintf(spellname, "%s", powname);
						}
						else sprintf(spellname, "%s", b_ptr->spell[x].name);

						c_put_str(TERM_INDIGO, spellname, 4+x, 0);
					}
				}
				else
				{
					if (b_ptr->spell[x].type > 0)
					{
						char extra[160];
						if (b_ptr->spell[x].type == 1)
						{
							sprintf(extra, "(%d, %s)", b_ptr->spell[x].power, get_element_name(b_ptr->spell[x].special1));
                					sprintf(spellname, "%s %s", b_ptr->spell[x].name, extra);
						}
						else if (b_ptr->spell[x].type == 2)
						{
							sprintf(extra, "(%d, %s, rad %d)", b_ptr->spell[x].power, get_element_name(b_ptr->spell[x].special1), b_ptr->spell[x].special2);
                					sprintf(spellname, "%s %s", b_ptr->spell[x].name, extra);
						}
						else if (b_ptr->spell[x].type == 3 || b_ptr->spell[x].type == 4)
						{
							sprintf(extra, "(Power %d)", b_ptr->spell[x].power);
                					sprintf(spellname, "%s %s", b_ptr->spell[x].name, extra);
						}
						else if (b_ptr->spell[x].type == 5)
						{
							if (b_ptr->spell[x].special1 == 1 || b_ptr->spell[x].special1 == 4) sprintf(extra, "(Str, %d)", b_ptr->spell[x].power);
							else if (b_ptr->spell[x].special1 == 2 || b_ptr->spell[x].special1 == 9) sprintf(extra, "(Dex, %d)", b_ptr->spell[x].power);
							else if (b_ptr->spell[x].special1 == 3 || b_ptr->spell[x].special1 == 5) sprintf(extra, "(Int/Wis, %d)", b_ptr->spell[x].power);
							else sprintf(extra, "(Str/Dex/Int/Wis, %d)", b_ptr->spell[x].power);
                					sprintf(spellname, "%s %s", b_ptr->spell[x].name, extra);
						}
						else if (b_ptr->spell[x].type == 6)
						{
							sprintf(extra, "(lvl %d, Num: %d)", b_ptr->spell[x].power, b_ptr->spell[x].special1);
                					sprintf(spellname, "%s %s", b_ptr->spell[x].name, extra);
						}
						else if (b_ptr->spell[x].type == 7)
						{
							sprintf(extra, "(Num: %d)", b_ptr->spell[x].special1);
                					sprintf(spellname, "%s %s", b_ptr->spell[x].name, extra);
						}
						else if (b_ptr->spell[x].type == 8)
						{
							sprintf(extra, "(Dist %d)", b_ptr->spell[x].power);
                					sprintf(spellname, "%s %s", b_ptr->spell[x].name, extra);
						}
						else if (b_ptr->spell[x].type == 9)
						{
							char *powname;

							/* Calls lua! */
							if (recall) call_lua("get_scripted_spell_name", "(dd)", "s", r_idx, x+1, &powname);
							else call_lua("get_scripted_spell_name", "(dd)", "s", p_ptr->body_monster, x+1, &powname);

                					sprintf(spellname, "%s", powname);
						}
						else sprintf(spellname, "%s", b_ptr->spell[x].name);

						c_put_str(TERM_WHITE, spellname, 4+x, 0);
					}
				}
        		}
			

			/* New monster. */
			if (compare)
			{
				c_put_str(TERM_WHITE, "New Monster: ", 2, 40);
				c_put_str(TERM_L_GREEN, r_ptr->name_char, 2, 53);

				c_put_str(TERM_WHITE, "--- SPELLS ---", 3, 40);
				for (x = 0; x < 20; x++)
        			{
					if (r_ptr->spell[x].type > 0)
					{
						char extra[160];
						if (r_ptr->spell[x].type == 1)
						{
							sprintf(extra, "(%d, %s)", r_ptr->spell[x].power, get_element_name(r_ptr->spell[x].special1));
                					sprintf(spellname, "%s %s", r_ptr->spell[x].name, extra);
						}
						else if (r_ptr->spell[x].type == 2)
						{
							sprintf(extra, "(%d, %s, rad %d)", r_ptr->spell[x].power, get_element_name(r_ptr->spell[x].special1), r_ptr->spell[x].special2);
                					sprintf(spellname, "%s %s", r_ptr->spell[x].name, extra);
						}
						else if (r_ptr->spell[x].type == 3 || r_ptr->spell[x].type == 4)
						{
							sprintf(extra, "(Power %d)", r_ptr->spell[x].power);
                					sprintf(spellname, "%s %s", r_ptr->spell[x].name, extra);
						}
						else if (r_ptr->spell[x].type == 5)
						{
							if (r_ptr->spell[x].special1 == 1 || r_ptr->spell[x].special1 == 4) sprintf(extra, "(Str, %d)", r_ptr->spell[x].power);
							else if (r_ptr->spell[x].special1 == 2 || r_ptr->spell[x].special1 == 9) sprintf(extra, "(Dex, %d)", r_ptr->spell[x].power);
							else if (r_ptr->spell[x].special1 == 3 || r_ptr->spell[x].special1 == 5) sprintf(extra, "(Int/Wis, %d)", r_ptr->spell[x].power);
							else sprintf(extra, "(Str/Dex/Int/Wis, %d)", r_ptr->spell[x].power);
                					sprintf(spellname, "%s %s", r_ptr->spell[x].name, extra);
						}
						else if (r_ptr->spell[x].type == 6)
						{
							sprintf(extra, "(lvl %d, Num: %d)", r_ptr->spell[x].power, r_ptr->spell[x].special1);
                					sprintf(spellname, "%s %s", r_ptr->spell[x].name, extra);
						}
						else if (r_ptr->spell[x].type == 7)
						{
							sprintf(extra, "(Num: %d)", r_ptr->spell[x].special1);
                					sprintf(spellname, "%s %s", r_ptr->spell[x].name, extra);
						}
						else if (r_ptr->spell[x].type == 8)
						{
							sprintf(extra, "(Dist %d)", r_ptr->spell[x].power);
                					sprintf(spellname, "%s %s", r_ptr->spell[x].name, extra);
						}
						else if (r_ptr->spell[x].type == 9)
						{
							char *powname;

							/* Calls lua! */
							call_lua("get_scripted_spell_name", "(dd)", "s", p_ptr->body_monster, x+1, &powname);

                					sprintf(spellname, "%s", powname);
						}
						else sprintf(spellname, "%s", r_ptr->spell[x].name);

						c_put_str(TERM_WHITE, spellname, 4+x, 40);
					}
        			}
			}
		}
		/* Mode 3: Resistances comparison. */
		if (mode == 3)
		{
			int x;
			int respos;
			char resname[160];
			char resamt[5];
			/* Prepare the screen */
			Term_erase(0, 1, 255);
        		for (i = 0; i < SCREEN_HGT; i++)
        		{
                		roff("\n");
        		}
			
        		c_put_str(TERM_WHITE, "MONSTER RESISTANCES                                                               ", 0, 0);
			c_put_str(TERM_WHITE, "------------------------", 1, 0);

			/* Current monster */

			if (compare)
			{
				c_put_str(TERM_WHITE, "Current Monster: ", 2, 0);
				c_put_str(colorterm, b_ptr->name_char, 2, 17);
			}
			else
			{
				c_put_str(TERM_WHITE, "Monster: ", 2, 0);
				c_put_str(colorterm, b_ptr->name_char, 2, 9);
			}

			c_put_str(TERM_WHITE, "--- RESISTANCES ---", 3, 0);
			respos = 0;
			for (x = 0; x < MAX_RESIST; x++)
			{
				if (b_ptr->resistances[x] > 0 || b_ptr->resistances[x] < 0)
				{
					sprintf(resname, "%s:", get_element_name(x), b_ptr->resistances[x]);
					c_put_str(TERM_WHITE, resname, 4+respos, 0);
					sprintf(resamt, "%d", b_ptr->resistances[x]);
					c_put_str(colorterm, resamt, 4+respos, 17);
					respos++;
				}
			}
			if (b_ptr->flags3 & (RF3_NO_STUN))
			{
				c_put_str(TERM_WHITE, "Resists Paralysis", 4+respos, 0);
				respos++;
			}
			if (b_ptr->flags3 & (RF3_NO_FEAR))
			{
				c_put_str(TERM_WHITE, "Resists Fear", 4+respos, 0);
				respos++;
			}
			if (b_ptr->flags3 & (RF3_NO_CONF))
			{
				c_put_str(TERM_WHITE, "Resists Confusion", 4+respos, 0);
				respos++;
			}
			if (b_ptr->flags3 & (RF3_NO_SLEEP))
			{
				c_put_str(TERM_WHITE, "Resists Sleep", 4+respos, 0);
				respos++;
			}
			

			/* New monster. */
			if (compare)
			{
				c_put_str(TERM_WHITE, "New Monster: ", 2, 40);
				c_put_str(TERM_L_GREEN, r_ptr->name_char, 2, 53);

				c_put_str(TERM_WHITE, "--- RESISTANCES ---", 3, 40);
				respos = 0;
				for (x = 0; x < MAX_RESIST; x++)
				{
					if (r_ptr->resistances[x] > 0 || r_ptr->resistances[x] < 0)
					{
						sprintf(resname, "%s:", get_element_name(x), r_ptr->resistances[x]);
						c_put_str(TERM_WHITE, resname, 4+respos, 40);
						sprintf(resamt, "%d", r_ptr->resistances[x]);
						c_put_str(TERM_L_GREEN, resamt, 4+respos, 57);
						respos++;
					}
				}
				if (r_ptr->flags3 & (RF3_NO_STUN))
				{
					c_put_str(TERM_WHITE, "Resists Paralysis", 4+respos, 40);
					respos++;
				}
				if (r_ptr->flags3 & (RF3_NO_FEAR))
				{
					c_put_str(TERM_WHITE, "Resists Fear", 4+respos, 40);
					respos++;
				}
				if (r_ptr->flags3 & (RF3_NO_CONF))
				{
					c_put_str(TERM_WHITE, "Resists Confusion", 4+respos, 40);
					respos++;
				}
				if (r_ptr->flags3 & (RF3_NO_SLEEP))
				{
					c_put_str(TERM_WHITE, "Resists Sleep", 4+respos, 40);
					respos++;
				}
			}
		}

		c_put_str(TERM_WHITE, "Press 'p' for previous mode, or any keys for next. Press ESC to exit.", 23, 0);
		c = inkey();

		switch (c)
		{
			case ESCAPE:
			{
				comparing = FALSE;
				break;
			}

			case 'p':
			case 'P':
			{
				mode -= 1;
				if (mode < 0) mode = 3;
				break;
			}
			
			default:
			{
				mode += 1;
				if (mode > 3) mode = 0;
				break;
			}
		}
	}

	Term_load();
}

/* Based on above function. */
/* A new and MUCH better Scan Monster! :) */
void scan_monster(monster_type *m_ptr)
{
	int mode;
	int i;
	bool comparing = TRUE;
	monster_race *r_ptr;
	char c;
	char str[80];
	bool scaled = FALSE;
	int scaledlevel;
	int colorterm;

	/* Initialise monster race pointers. */
	r_ptr = &r_info[m_ptr->r_idx];

	/*if (r_ptr->flags7 & (RF7_SCALED))
	{
		evolution_compare(m_ptr->r_idx, FALSE, TRUE);
		return;
	}*/

	if (r_ptr->cursed > 0)
	{
		msg_print("This monster cannot be scanned!");
		return;
	}

	if (r_ptr->flags7 & (RF7_SCALED))
	{
		scaled = TRUE;
		if (p_ptr->max_plv > r_ptr->level) scaledlevel = p_ptr->max_plv;
		else scaledlevel = r_ptr->level;
		colorterm = TERM_INDIGO;
	}
	else colorterm = TERM_L_GREEN;

	/* Save what's on screen */
        Term_save();

	/* Flush messages */
	msg_print(NULL);

        /* Begin... */
	Term_erase(0, 1, 255);

	/* Start with mode 0. */
	mode = 0;

	while (comparing)
	{
		/* Mode 0: Base stats comparison. */
		if (mode == 0)
		{
			/* Prepare the screen */
			Term_erase(0, 1, 255);
        		for (i = 0; i < SCREEN_HGT; i++)
        		{
                		roff("\n");
        		}
			
        		c_put_str(TERM_WHITE, "MONSTER STATS                                                               ", 0, 0);
			c_put_str(TERM_WHITE, "------------------", 1, 0);

			/* Current monster */
			c_put_str(TERM_WHITE, "Monster: ", 2, 0);
			c_put_str(colorterm, r_ptr->name_char, 2, 9);

			c_put_str(TERM_WHITE, "Level: ", 3, 0);
			sprintf(str, "%d", m_ptr->level);
			c_put_str(colorterm, str, 3, 7);

			if (scaled)
			{
				c_put_str(TERM_WHITE, "Depth: ", 3, 13);
				sprintf(str, "Unknown");
				c_put_str(TERM_INDIGO, str, 3, 20);
			}
			else
			{
				c_put_str(TERM_WHITE, "Depth: ", 3, 13);
				sprintf(str, "%d", r_ptr->level);
				c_put_str(TERM_L_GREEN, str, 3, 20);
			}
			c_put_str(TERM_WHITE, "Type: ", 4, 0);
			if (r_ptr->body_parts[BODY_WEAPON]) sprintf(str, "Humanoid Monster");
			else sprintf(str, "Essence Monster");
			c_put_str(colorterm, str, 4, 6);

			c_put_str(TERM_WHITE, "--- BASE STATS ---", 5, 0);
			c_put_str(TERM_WHITE, "Hp       : ", 6, 0);
			sprintf(str, "%ld/%ld", m_ptr->hp, m_ptr->maxhp);
			c_put_str(colorterm, str, 6, 11);
			c_put_str(TERM_WHITE, "Strength : ", 7, 0);
			sprintf(str, "%ld", m_ptr->str);
			c_put_str(colorterm, str, 7, 11);
			c_put_str(TERM_WHITE, "Dexterity: ", 8, 0);
			sprintf(str, "%ld", m_ptr->dex);
			c_put_str(colorterm, str, 8, 11);
			c_put_str(TERM_WHITE, "Mind     : ", 9, 0);
			sprintf(str, "%ld", m_ptr->mind);
			c_put_str(colorterm, str, 9, 11);

			c_put_str(TERM_WHITE, "Attack Skill: ", 7, 20);
			sprintf(str, "%ld", m_ptr->skill_attack);
			c_put_str(colorterm, str, 7, 34);
			c_put_str(TERM_WHITE, "Ranged Skill: ", 8, 20);
			sprintf(str, "%ld", m_ptr->skill_ranged);
			c_put_str(colorterm, str, 8, 34);
			c_put_str(TERM_WHITE, "Magic Skill : ", 9, 20);
			sprintf(str, "%ld", m_ptr->skill_magic);
			c_put_str(colorterm, str, 9, 34);

			c_put_str(TERM_WHITE, "--- MISC STATS ---", 10, 0);

			c_put_str(TERM_WHITE, "Defense  : ", 11, 0);
			sprintf(str, "%ld", m_ptr->defense);
			c_put_str(colorterm, str, 11, 11);
			c_put_str(TERM_WHITE, "Speed    : ", 12, 0);
			if (r_ptr->flags7 & (RF7_VERY_FAST)) sprintf(str, "%d {VERY FAST}", m_ptr->mspeed - 110);
			else if (r_ptr->flags7 & (RF7_FAST)) sprintf(str, "%d {FAST}", m_ptr->mspeed - 110);
			else sprintf(str, "%d", m_ptr->mspeed - 110);
			c_put_str(colorterm, str, 12, 11);
			c_put_str(TERM_WHITE, "Attacks  : ", 13, 0);
			sprintf(str, "%d", r_ptr->attacks);
			c_put_str(colorterm, str, 13, 11);
			c_put_str(TERM_WHITE, "Spells   : ", 14, 0);
			sprintf(str, "%d", r_ptr->spells);
			c_put_str(colorterm, str, 14, 11);

			c_put_str(TERM_WHITE, "Hit Rate : ", 11, 20);
			sprintf(str, "%ld", m_ptr->hitrate);
			c_put_str(colorterm, str, 11, 31);
			c_put_str(TERM_WHITE, "Lives    : ", 12, 20);
			sprintf(str, "%ld", m_ptr->lives);
			c_put_str(colorterm, str, 12, 31);
			c_put_str(TERM_WHITE, "---  COUNTERS  ---", 15, 0);
			{
				char *countername;
				c_put_str(TERM_WHITE, "Type     : ", 16, 0);
				call_lua("get_counter_name", "(d)", "s", r_ptr->countertype, &countername);
				c_put_str(colorterm, countername, 16, 11);
				c_put_str(TERM_WHITE, "Attempt  : ", 17, 0);
				sprintf(str, "%d%%", r_ptr->counterchance);
				c_put_str(colorterm, str, 17, 11);
			}

			c_put_str(TERM_WHITE, "---    MISC    ---", 18, 0);
			{
				char *line1;
				char *line2;
				char *line3;
				char *line4;
				
				call_lua("get_misc_monster_info", "(dd)", "s", p_ptr->body_monster, 1, &line1);
				c_put_str(colorterm, line1, 19, 0);
				call_lua("get_misc_monster_info", "(dd)", "s", p_ptr->body_monster, 2, &line2);
				c_put_str(colorterm, line2, 20, 0);
				call_lua("get_misc_monster_info", "(dd)", "s", p_ptr->body_monster, 3, &line3);
				c_put_str(colorterm, line3, 21, 0);
				call_lua("get_misc_monster_info", "(dd)", "s", p_ptr->body_monster, 4, &line4);
				c_put_str(colorterm, line4, 22, 0);
			}

			c_put_str(TERM_WHITE, "--- ABILITIES/AILMENTS ---", 3, 40);
			{
				int l = 0;

				if (m_ptr->abilities & (BOSS_IMMUNE_WEAPONS))
				{
					c_put_str(TERM_L_BLUE, "Immune To Weapons/Physical attacks", 4 + l, 40);
					l++;
				}
				if (m_ptr->abilities & (BOSS_IMMUNE_MAGIC))
				{
					c_put_str(TERM_L_BLUE, "Immune To Magical attacks", 4 + l, 40);
					l++;
				}
				if (m_ptr->abilities & (BOSS_DOUBLE_DAMAGES))
				{
					c_put_str(TERM_L_BLUE, "Double damages with Melee/Ranged attacks.", 4 + l, 40);
					l++;
				}
				if (m_ptr->abilities & (BOSS_DOUBLE_MAGIC))
				{
					c_put_str(TERM_L_BLUE, "Double damages with Magic attacks.", 4 + l, 40);
					l++;
				}
				if (m_ptr->abilities & (BOSS_HALVE_DAMAGES))
				{
					c_put_str(TERM_L_BLUE, "50% resistance to all damages.", 4 + l, 40);
					l++;
				}
				if (m_ptr->abilities & (BOSS_CURSED_HITS))
				{
					c_put_str(TERM_L_BLUE, "Melee attacks blinds, confuse and scare.", 4 + l, 40);
					l++;
				}
				if (m_ptr->abilities & (BOSS_RETURNING))
				{
					c_put_str(TERM_L_BLUE, "Returns 50% normal melee attacks damages.", 4 + l, 40);
					l++;
				}
				if (m_ptr->abilities & (BOSS_MAGIC_RETURNING))
				{
					c_put_str(TERM_L_BLUE, "Returns 50% magic attacks damages.", 4 + l, 40);
					l++;
				}
				if (m_ptr->abilities & (CURSE_LOCK))
				{
					c_put_str(TERM_ORANGE, "Magic spells are locked.", 4 + l, 40);
					l++;
				}
				if (m_ptr->abilities & (CURSE_HALVE_DAMAGES))
				{
					c_put_str(TERM_ORANGE, "Melee/Ranged damages are halved.", 4 + l, 40);
					l++;
				}
				if (m_ptr->abilities & (CURSE_HALVE_MAGIC))
				{
					c_put_str(TERM_ORANGE, "Magic damages are halved.", 4 + l, 40);
					l++;
				}
				if (m_ptr->abilities & (CURSE_DAMAGES_CURSE))
				{
					c_put_str(TERM_ORANGE, "Cursed with Damages Curse.", 4 + l, 40);
					l++;
				}
				if (m_ptr->abilities & (MUTILATE_LEGS))
				{
					c_put_str(TERM_ORANGE, "Cannot move.", 4 + l, 40);
					l++;
				}
				if (m_ptr->abilities & (MUTILATE_ARMS))
				{
					c_put_str(TERM_ORANGE, "Cannot attack.", 4 + l, 40);
					l++;
				}
				if (m_ptr->abilities & (TAUNTED))
				{
					c_put_str(TERM_ORANGE, "Taunted.", 4 + l, 40);
					l++;
				}
				if (m_ptr->abilities & (PIERCING_SPELLS))
				{
					c_put_str(TERM_ORANGE, "Vulnerable to your chosen element.", 4 + l, 40);
					l++;
				}
				if (m_ptr->abilities & (WAR_BLESSED))
				{
					c_put_str(TERM_YELLOW, "Blessed with War Blessing.", 4 + l, 40);
					l++;
				}
				if (m_ptr->abilities & (MORALE_BOOST))
				{
					c_put_str(TERM_YELLOW, "Morale Boost.", 4 + l, 40);
					l++;
				}
			}
		}
		/* Mode 1: Attacks comparison. */
		if (mode == 1)
		{
			int x;
			char attackname[160];
			/* Prepare the screen */
			Term_erase(0, 1, 255);
        		for (i = 0; i < SCREEN_HGT; i++)
        		{
                		roff("\n");
        		}
			
        		c_put_str(TERM_WHITE, "MONSTER ATTACKS                                                                 ", 0, 0);
			c_put_str(TERM_WHITE, "--------------------", 1, 0);

			/* Current monster */
			c_put_str(TERM_WHITE, "Monster: ", 2, 0);
			c_put_str(colorterm, r_ptr->name_char, 2, 9);

			c_put_str(TERM_WHITE, "--- ATTACKS ---", 3, 0);
			for (x = 0; x < 20; x++)
			{
				if (r_ptr->attack[x].type > 0)
				{
					sprintf(attackname, "%s", get_monster_attack_name_damages(m_ptr, x));
					c_put_str(TERM_WHITE, attackname, 4+x, 0);
				}
			}
		}
		/* Mode 2: Spells comparison. */
		if (mode == 2)
		{
			int x;
			char spellname[160];
			/* Prepare the screen */
			Term_erase(0, 1, 255);
        		for (i = 0; i < SCREEN_HGT; i++)
        		{
                		roff("\n");
        		}
			
        		c_put_str(TERM_WHITE, "MONSTER SPELLS                                                                  ", 0, 0);
			c_put_str(TERM_WHITE, "-------------------", 1, 0);

			/* Current monster */
			c_put_str(TERM_WHITE, "Monster: ", 2, 0);
			c_put_str(colorterm, r_ptr->name_char, 2, 9);

			c_put_str(TERM_WHITE, "--- SPELLS ---", 3, 0);
			for (x = 0; x < 20; x++)
        		{
				if (r_ptr->spell[x].type > 0)
				{
					char extra[160];
					if (r_ptr->spell[x].type == 1)
					{
						s32b dam;
						s32b dambonus;
						s32b mindstat = m_ptr->mind - 5;
						if (mindstat <= 0) mindstat = 1;
						if (r_ptr->spell[x].special3 == 1) dam = r_ptr->spell[x].power;
						else
						{
							if (scaled) dam = multiply_divide(scaledlevel, r_ptr->spell[x].power, 100) * mindstat;
							else dam = (r_ptr->spell[x].power * mindstat);
							dambonus = multiply_divide(dam, m_ptr->skill_magic * 10, 100);
							dam = dam + dambonus;
							if (m_ptr->abilities & (BOSS_DOUBLE_MAGIC)) dam *= 2;
						}
						sprintf(extra, "(%ld, %s)", dam, get_element_name(r_ptr->spell[x].special1));
                				sprintf(spellname, "%s %s", r_ptr->spell[x].name, extra);
					}
					else if (r_ptr->spell[x].type == 2)
					{
						s32b dam;
						s32b dambonus;
						s32b mindstat = m_ptr->mind - 5;
						int rad = r_ptr->spell[x].special2;
						if (mindstat <= 0) mindstat = 1;
						if (r_ptr->spell[x].special3 == 1) dam = r_ptr->spell[x].power;
						else
						{
							if (scaled) dam = multiply_divide(scaledlevel, r_ptr->spell[x].power, 100) * mindstat;
							else dam = (r_ptr->spell[x].power * mindstat);
							dambonus = multiply_divide(dam, m_ptr->skill_magic * 10, 100);
							dam = dam + dambonus;
							if (m_ptr->abilities & (BOSS_DOUBLE_MAGIC)) dam *= 2;
						}
						rad += m_ptr->skill_magic / 30;
						sprintf(extra, "(%ld, %s, rad %d)", dam, get_element_name(r_ptr->spell[x].special1), rad);
                				sprintf(spellname, "%s %s", r_ptr->spell[x].name, extra);
					}
					else if (r_ptr->spell[x].type == 3)
					{
						s32b dam;
						s32b dambonus;
						s32b mindstat = m_ptr->mind - 5;
						if (mindstat <= 0) mindstat = 1;
						if (r_ptr->spell[x].special3 == 1) dam = r_ptr->spell[x].power;
						else
						{
							if (scaled) dam = multiply_divide(scaledlevel, r_ptr->spell[x].power, 100) * mindstat;
							else dam = (r_ptr->spell[x].power * mindstat);
							dambonus = multiply_divide(dam, m_ptr->skill_magic * 10, 100);
							dam = dam + dambonus;
							if (m_ptr->abilities & (BOSS_DOUBLE_MAGIC)) dam *= 2;
						}
						sprintf(extra, "(Power %ld)", dam);
                				sprintf(spellname, "%s %s", r_ptr->spell[x].name, extra);
					}
					else if (r_ptr->spell[x].type == 4)
					{
						sprintf(extra, "(Power %d)", r_ptr->spell[x].power);
                				sprintf(spellname, "%s %s", r_ptr->spell[x].name, extra);
					}
					else if (r_ptr->spell[x].type == 5)
					{
						int pow;
						if (scaled) pow = multiply_divide(scaledlevel, r_ptr->spell[x].power, 100);
						else pow = r_ptr->spell[x].power;
						
						if (r_ptr->spell[x].special1 == 1 || r_ptr->spell[x].special1 == 4) sprintf(extra, "(Str, %d)", pow);
						else if (r_ptr->spell[x].special1 == 2 || r_ptr->spell[x].special1 == 9) sprintf(extra, "(Dex, %d)", pow);
						else if (r_ptr->spell[x].special1 == 3 || r_ptr->spell[x].special1 == 5) sprintf(extra, "(Int/Wis, %d)", pow);
						else sprintf(extra, "(Str/Dex/Int/Wis, %d)", pow);
                				sprintf(spellname, "%s %s", r_ptr->spell[x].name, extra);
					}
					else if (r_ptr->spell[x].type == 6)
					{
						int pow;

						if (scaled) pow = multiply_divide(scaledlevel, r_ptr->spell[x].power, 100);
						else pow = r_ptr->spell[x].power;

						sprintf(extra, "(lvl %d, Num: %d)", pow, r_ptr->spell[x].special1);
                				sprintf(spellname, "%s %s", r_ptr->spell[x].name, extra);
					}
					else if (r_ptr->spell[x].type == 7)
					{
						sprintf(extra, "(Num: %d)", r_ptr->spell[x].special1);
                				sprintf(spellname, "%s %s", r_ptr->spell[x].name, extra);
					}
					else if (r_ptr->spell[x].type == 8)
					{
						sprintf(extra, "(Dist %d)", r_ptr->spell[x].power);
                				sprintf(spellname, "%s %s", r_ptr->spell[x].name, extra);
					}
					else if (r_ptr->spell[x].type == 9)
					{
						char *powname;

						/* Calls lua! */
						call_lua("get_scripted_spell_name", "(dd)", "s", p_ptr->body_monster, x+1, &powname);

                				sprintf(spellname, "%s", powname);
					}
					else sprintf(spellname, "%s", r_ptr->spell[x].name);

					c_put_str(TERM_WHITE, spellname, 4+x, 0);
				}
        		}
		}
		/* Mode 3: Resistances comparison. */
		if (mode == 3)
		{
			int x;
			int respos;
			char resname[160];
			char resamt[5];
			/* Prepare the screen */
			Term_erase(0, 1, 255);
        		for (i = 0; i < SCREEN_HGT; i++)
        		{
                		roff("\n");
        		}
			
        		c_put_str(TERM_WHITE, "MONSTER RESISTANCES                                                               ", 0, 0);
			c_put_str(TERM_WHITE, "------------------------", 1, 0);

			/* Current monster */
			c_put_str(TERM_WHITE, "Monster: ", 2, 0);
			c_put_str(colorterm, r_ptr->name_char, 2, 9);

			c_put_str(TERM_WHITE, "--- RESISTANCES ---", 3, 0);
			respos = 0;
			for (x = 0; x < MAX_RESIST; x++)
			{
				if (r_ptr->resistances[x] > 0 || r_ptr->resistances[x] < 0)
				{
					sprintf(resname, "%s:", get_element_name(x), r_ptr->resistances[x]);
					c_put_str(TERM_WHITE, resname, 4+respos, 0);
					sprintf(resamt, "%d", r_ptr->resistances[x]);
					c_put_str(colorterm, resamt, 4+respos, 17);
					respos++;
				}
			}
			if (r_ptr->flags3 & (RF3_NO_STUN))
			{
				c_put_str(TERM_WHITE, "Resists Paralysis", 4+respos, 0);
				respos++;
			}
			if (r_ptr->flags3 & (RF3_NO_FEAR))
			{
				c_put_str(TERM_WHITE, "Resists Fear", 4+respos, 0);
				respos++;
			}
			if (r_ptr->flags3 & (RF3_NO_CONF))
			{
				c_put_str(TERM_WHITE, "Resists Confusion", 4+respos, 0);
				respos++;
			}
			if (r_ptr->flags3 & (RF3_NO_SLEEP))
			{
				c_put_str(TERM_WHITE, "Resists Sleep", 4+respos, 0);
				respos++;
			}
			
		}

		c_put_str(TERM_WHITE, "Press 'p' for previous mode, or any keys for next. Press ESC to exit.", 23, 0);
		c = inkey();

		switch (c)
		{
			case ESCAPE:
			{
				comparing = FALSE;
				break;
			}

			case 'p':
			case 'P':
			{
				mode -= 1;
				if (mode < 0) mode = 3;
				break;
			}
			
			default:
			{
				mode += 1;
				if (mode > 3) mode = 0;
				break;
			}
		}
	}

	Term_load();
}

/* This function shouldn't exist at all, but it's difficult to get objects names in lua. */
/* For now, it will exist. I'll remove it eventually once I get workable object names */
/* in lua. */
void boss_of_global_object(int r_idx)
{
	char o_name[80];
	object_type *o_ptr = &global_object;
	monster_race *r_ptr = &r_info[r_idx];

	object_desc(o_name, o_ptr, TRUE, 0);
	sprintf(r_ptr->name_char, "Boss of %s", o_name);
}

/* Count the number of racial kills of a specific monster kind. */
int get_race_kills(char mr)
{
	int i;
	int killcount = 0;

	for (i = 1; i < max_r_idx; i++)
        {
                monster_race *r_ptr;

                r_ptr = &r_info[i];

		if (r_ptr->d_char == mr && r_ptr->level > 0 && r_ptr->r_pkills > 0) killcount += 1;
	}

	return (i);
}
