/* File: monster2.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"
#include "randname.h"



/*
 * Delete a monster by index.
 * When a monster is deleted, all of its objects are deleted.
 *
 * This functions does not always delete RETURNS monsters
 * (monsters which come back from the dead), it only marks them
 * as temporarily dead.
 * If cancomeback is FALSE, then the monster cannot come back from the dead.
 *
 * return_monsters() is called every 40 game turns.  That function
 * decrements m_ptr->temp_death, or if temp_death == 1, it brings the
 * monster back to life. (40 game turns = 4 normal speed player turns)
 */
void delete_monster_idx(int i, bool cancomeback)
{
	int x, y, deadlong;

	monster_type *m_ptr = &mon_list[i];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	bool comeback = FALSE;

	s16b this_o_idx, next_o_idx = 0;

	/* for RETURNS monsters */
    int drlev = r_ptr->level / 8;
	if (drlev < 2) drlev = 2;
	
	/* will the monster come back from the dead? (if it has RETURNS) */
    if ((strchr("f", r_ptr->d_char)) && (cancomeback))
	{
		/* any cats that have the RETURNS flag have nine lives */
        if (m_ptr->ninelives < 9) comeback = TRUE;
	}
	else if ((r_ptr->flags2 & (RF2_RETURNS)) && (cancomeback))
	{
		int remonchance = 2;
		if (badluck) remonchance += rand_int(2 + (badluck / 2));
		if (rand_int(100) < drlev)
		{
			if (drlev > 3) remonchance += randint(drlev-2);
			else remonchance += 1;
		}
		/* town version less likely to come back */
		if (!p_ptr->depth) remonchance = 1;
		/* imaginary monsters less likely to come back */
		if (m_ptr->extra2) remonchance = 1;
		/* if remonchance == 2 (normal) then */
        /* always if first death, 2/3 chance if 2nd death, then 50% chance.. */
        if (rand_int(m_ptr->ninelives + 2) < remonchance) comeback = TRUE;
	}

	/* Monsters that come back from the dead */
	if ((r_ptr->flags2 & (RF2_RETURNS)) && (comeback))
	{
		/* come back to life slower if it has come back before */
        if (m_ptr->ninelives) deadlong = 5 + rand_int(10 + m_ptr->ninelives);
		else deadlong = 3 + rand_int(11);
        /* higher temp_death makes it take longer to come back to life */
		m_ptr->temp_death = deadlong + m_ptr->ninelives - randint(drlev) - ((badluck+1)/3);
		/* ensure a positive number */
		if (m_ptr->temp_death < 3) m_ptr->temp_death = 2 + ((goodluck + 1) / 4);
		/* make sure it doesn't take too long to come back */
		if (m_ptr->temp_death > 10) m_ptr->temp_death = 10 + rand_int(m_ptr->temp_death-8);
		if ((p_ptr->depth >= r_ptr->level + 5) && (m_ptr->ninelives) &&
			(m_ptr->temp_death > 5 + m_ptr->ninelives))
			m_ptr->temp_death = 5 + m_ptr->ninelives + rand_int(m_ptr->temp_death - (4 + m_ptr->ninelives));

		/* count how many times the monster has died */
		if ((strchr("f", r_ptr->d_char)) && (!m_ptr->ninelives) &&
			(randint(100) < 55 + goodluck - badluck))
		{
			/* cats have nine lives, but they might have died a couple times before */
			/* (but must have m_ptr->ninelives == 0 when it's created) */
            m_ptr->ninelives += 1 + randint(5);
		}
		else
		{
			m_ptr->ninelives += 1;
		}
		
		/* make it dissapear */
        m_ptr->ml = FALSE;
		m_ptr->heard = FALSE;
	}

	/* Get location */
	y = m_ptr->fy;
	x = m_ptr->fx;

	/* remove CAVE_WALL from dead BLOCK_LOS monsters */
	/* (as long as they're not in a wall) */
	if (r_ptr->flags7 & (RF7_BLOCK_LOS))
	{
		if (!((cave_feat[y][x] >= FEAT_SECRET) ||
			((cave_feat[y][x] >= FEAT_DOOR_CLOSE) &&
			(cave_feat[y][x] <= FEAT_DOOR_STUCK))))
				cave_info[y][x] &= ~(CAVE_WALL);
	}

	/* Hack -- Reduce the racial counter */
	/* (only if it's not going to come back to life) */
	if (!m_ptr->temp_death) r_ptr->cur_num--;

	/* Hack -- count the number of "reproducers" */
	/* (hopefully we don't have any breeders with the RETURNS flag but allow just in case) */
	if ((r_ptr->flags2 & (RF2_MULTIPLY)) &&	(!m_ptr->temp_death)) 
		num_repro--;


	/* Hack -- remove target monster */
	if (p_ptr->target_who == i) target_set_monster(0);

	/* Hack -- remove tracked monster */
	if (p_ptr->health_who == i) health_track(0);

	/* monster is no longer holding the PC */
	if (p_ptr->held_m_idx == i)
	{
		p_ptr->held_m_idx = 0;
		clear_timed(TMD_BEAR_HOLD);
	}

#ifdef roomrunes
	/* If a room rune is destroyed, reset saved rune locations */
	if (m_ptr->r_idx > 1400) save_runes();
#endif

	/* Monster is gone */
	cave_m_idx[y][x] = 0;


	/* Delete objects */
	for (this_o_idx = m_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Hack -- efficiency */
		o_ptr->held_m_idx = 0;

		/* Delete the object */
		delete_object_idx(this_o_idx);
	}

    if (!m_ptr->temp_death)
	{
		/* Wipe the Monster */
		(void)WIPE(m_ptr, monster_type);

		/* Count monsters */
		mon_cnt--;
	}

	/* Visual update */
	lite_spot(y, x);
}


/*
 * Delete the monster, if any, at a given location
 */
void delete_monster(int y, int x, bool cancomeback)
{
	/* Paranoia */
	if (!in_bounds(y, x)) return;

	/* Delete the monster (if any) */
	if (cave_m_idx[y][x] > 0) delete_monster_idx(cave_m_idx[y][x], cancomeback);
}


/*
 * Move an object from index i1 to index i2 in the object list
 */
static void compact_monsters_aux(int i1, int i2)
{
	int y, x;

	monster_type *m_ptr;

	s16b this_o_idx, next_o_idx = 0;


	/* Do nothing */
	if (i1 == i2) return;


	/* Old monster */
	m_ptr = &mon_list[i1];

	/* Location */
	y = m_ptr->fy;
	x = m_ptr->fx;

	/* Update the cave */
	cave_m_idx[y][x] = i2;

	/* Repair objects being carried by monster */
	for (this_o_idx = m_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Reset monster pointer */
		o_ptr->held_m_idx = i2;
	}

	/* Hack -- Update the target */
	if (p_ptr->target_who == i1) p_ptr->target_who = i2;

	/* Hack -- Update the health bar */
	if (p_ptr->health_who == i1) p_ptr->health_who = i2;

	/* Hack -- move monster */
	COPY(&mon_list[i2], &mon_list[i1], monster_type);

	/* Hack -- wipe hole */
	(void)WIPE(&mon_list[i1], monster_type);
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
	int i, num, cnt;

	int cur_lev, cur_dis, chance;


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
		for (i = 1; i < mon_max; i++)
		{
			monster_type *m_ptr = &mon_list[i];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			/* Paranoia -- skip "dead" monsters */
			if (!m_ptr->r_idx) continue;

			/* Hack -- High level monsters start out "immune" */
			if (r_ptr->level > cur_lev) continue;

			/* Ignore nearby monsters */
			if ((cur_dis > 0) && (m_ptr->cdis < cur_dis)) continue;

			/* Saving throw chance */
			chance = 90;
			
			/* This monster is already temporarily dead, so it dies easier */
			if (m_ptr->temp_death) chance = 34;

			/* Only compact "Quest" Monsters in emergencies */
			if ((r_ptr->flags1 & (RF1_QUESTOR)) && (cnt < 1000)) chance = 100;

			/* Try not to compact Unique Monsters */
			if (r_ptr->flags1 & (RF1_UNIQUE)) chance = 99;
			if (r_ptr->maxpop == 1) chance = 98;

			/* All monsters get a saving throw */
			if (rand_int(100) < chance) continue;

			/* Delete the monster */
			delete_monster_idx(i, FALSE);

			/* Count the monster */
			num++;
		}
	}


	/* Excise dead monsters (backwards!) */
	for (i = mon_max - 1; i >= 1; i--)
	{
		/* Get the i'th monster */
		monster_type *m_ptr = &mon_list[i];

		/* Skip real monsters */
		if (m_ptr->r_idx) continue;

		/* Move last monster into open hole */
		compact_monsters_aux(mon_max - 1, i);

		/* Compress "mon_max" */
		mon_max--;
	}
}


/*
 * Delete/Remove all the monsters when the player leaves the level
 *
 * This is an efficient method of simulating multiple calls to the
 * "delete_monster()" function, with no visual effects.
 */
void wipe_mon_list(void)
{
	int i;

	/* Delete all the monsters */
	for (i = mon_max - 1; i >= 1; i--)
	{
		monster_type *m_ptr = &mon_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Mega-Hack -- preserve Uniques XXX XXX XXX */

		/* Hack -- Reduce the racial counter */
		r_ptr->cur_num--;

		/* Monster is gone */
		cave_m_idx[m_ptr->fy][m_ptr->fx] = 0;

		/* Wipe the Monster */
		(void)WIPE(m_ptr, monster_type);
	}

	/* Reset "mon_max" */
	mon_max = 1;

	/* Reset "mon_cnt" */
	mon_cnt = 0;

	/* Hack -- reset "reproducer" count */
	num_repro = 0;

	/* Hack -- no more target */
	target_set_monster(0);

	/* Hack -- no more tracking */
	health_track(0);

	/* no monster is holding the player */
	p_ptr->held_m_idx = 0;
	clear_timed(TMD_BEAR_HOLD);
}


/*
 * Delete/Remove all imaginary monsters when the PC stops hallucenating
 */
void wipe_images(void)
{
	int i;

	/* scan monsters */
	for (i = mon_max - 1; i >= 1; i--)
	{
		monster_type *m_ptr = &mon_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* imaginary monsters vanish */
		/* extra2 == 5 is hallucenatory monster, there are now other types of illusion monsters */
		/* == 8 is dissapated hallucenatory monsters */
		if ((m_ptr->extra2 == 5) || (m_ptr->extra2 == 8))
		{
			delete_monster_idx(i, FALSE);
		}
	}
}


/*
 * Get and return the index of a "free" monster.
 *
 * This routine should almost never fail, but it *can* happen.
 */
s16b mon_pop(void)
{
	int i;


	/* Normal allocation */
	if (mon_max < z_info->m_max)
	{
		/* Get the next hole */
		i = mon_max;

		/* Expand the array */
		mon_max++;

		/* Count monsters */
		mon_cnt++;

		/* Return the index */
		return (i);
	}


	/* Recycle dead monsters */
	for (i = 1; i < mon_max; i++)
	{
		monster_type *m_ptr;

		/* Get the monster */
		m_ptr = &mon_list[i];

		/* Skip live monsters */
		if (m_ptr->r_idx) continue;

		/* Count monsters */
		mon_cnt++;

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
		if (!get_mon_num_hook || (*get_mon_num_hook)(entry->index))
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
 * Choose an especially appropriate monster for the theme.
 * because certain monsters should be significantly more common
 * in their home.
 * Changed to make sure no monsters appear more than 10 levels 
 * out of depth on the early levels.
 * On the shallow levels, this will now occationally pick a monster 
 * that isn't normally considered theme-appropriate to prevent other
 * monsters appearing grossly out of depth.
 */
s16b get_mon_force_theme(int level)
{
	s16b choose;
	int die = randint(100);
	int randlevel, olev = level;
	monster_race *r_ptr;

	/* randomize level (not as much as it did before) */
	if (level > 30) randlevel = (level-22) + rand_int(51);
	else randlevel = 2 + randint(36 + badluck - goodluck);
	if ((rand_int(100) < 15) && (level > 20)) level = ((level*2) + randlevel)/3;

	/* randomize even more */
	if (die < 6) level -= randint(6);
	if (die > 95) level += randint(6);
	/* cap */
	if (level > olev + 8 + rand_int(8) - goodluck/2) level = olev + 2 + randint(6);
	if (level < olev - 10) level = olev - (3 + randint(6));
	
	die = rand_int(100);

	switch (p_ptr->theme)
	{
		case 1: /* fairy forest */
		{
			if (die < 14)
			{
				if (level < 20) choose = 191; /* brownie(11) */
				else if (level < 85) choose = 401; /* wild unicorn(27) */
				else choose = 665; /* white unicorn(50) */
			}
			else if (die < 28)
			{
				if ((level < 22) && (randint(100) < 93)) choose = 190; /* gnome(11) */
				else if (level < 27) choose = 952; /* naiad(21) */
				else if (level < 39) choose = 849; /* dryad(30) */
				else choose = 845; /* gnoyem (44, theme only) */
			}
			else if (die < 43)
			{
				if (level < 36) choose = 200; /* bright sprite(12) */
				else if (level < 49) choose = 598; /* trooping fairy(41) */
				else if (level < 59) choose = 650; /* dark fairy king(47) */
				else if (level < 65) choose = 598; /* trooping fairy(41) */
				else choose = 712; /* dark fairy fool(68) */
			}
			else if (die < 57)
			{
				if (level < 5) choose = 27; /* confused baby bright sprite(1) */
				else if (level < 33) choose = 239; /* white gnome(15) */
				else if (level < 54) choose = 554; /* extril(39) */
				else choose = 680; /* dark fairy queen(56) */
			}
			else if (die < 71)
			{
				if (level < 13) choose = 63; /* sprite(3) */
				else if (level < 19) choose = 284; /* centaur barbarian(17) */
				else if (level < 30) choose = 285; /* centaur barbarian groups(22) */
				else choose = 458; /* centaur warrior(32) */
			}
			else if (die < 85)
			{
				if (level < 23)
				{
					int dieb = randint(100);
					if (dieb < 35) choose = 138; /* gnome thief(8) */
					else if (dieb < 70) choose = 139; /* clear sprite(8) */
					else choose = 151; /* leprechaun(9) */
				}
				else if (level < 72) choose = 395; /* pooka(27) */
				else choose = 723; /* rayem(69) */
			}
			else /* (die > 85) */
			{
				if (level < 17) choose = 110; /* poison sprite(6) */
				else if (level < 29) choose = 995; /* pixie (helper) */
				else if (level < 39) choose = 490; /* grey unicorn(34) */
				else choose = 604; /* black unicorn */
			}
			break;
		}
		case 2: /* cold forest */
		{
			if (die < 14)
			{
				if ((level < 15) && (randint(100) < 47)) choose = 32; /* kobold(2) */
				else if (level < 15) choose = 62; /* kobold shaman(3) */
				else if (level < 18) choose = 116; /* kobold lord(6) */
				else if (level < 29) choose = 320; /* ranger(20) */
				else choose = 442; /* frost giant(30) */
			}
			else if (die < 28)
			{
				if (level < 4) choose = 26; /* small kobold(1) */
				else if (level < 15) choose = 116; /* kobold lord(6) */
				else if (level < 30) choose = 283; /* forest troll(17) */
				else choose = 468; /* ice troll(33) */
			}
			else if (die < 42)
			{
				if (level < 5) choose = 79; /* kobold archer(4) */
				else if (level < 25) choose = 154; /* yeti(9) */
				else if (level < 75) choose = 387; /* owlbear(26) */
				else choose = 718; /* great ice wyrm(63) */
			}
			else if (die < 57)
			{
				if (level < 29) choose = 182; /* white wolf(11) */
				else choose = 471; /* slow elemental(33) */
			}
			else if (die < 71)
			{
				if (level < 4) choose = 92; /* goblin(5) */
				else if (level < 11) choose = 273; /* hill orc(8) */
				else if (level < 33) choose = 273; /* uruk(16) */
				else choose = 530; /* ice elemental(37) */
			}
			else if (die < 85)
			{
				if (level < 29) choose = 174; /* bugbear(10) */
				else choose = 460; /* windigo(32) */
			}
			else /* (die > 85) */
			{
				if (level < 10) choose = 108; /* wood spider(6) */
				else if (level < 65) choose = 318; /* sasquatch(20) */
				else choose = 570; /* wendigo(40) */
			}
			break;
		}
		case 3: /* icky place */
		{
			if (die < 14)
			{
				if (level < 26) choose = 835; /* bubbering icky thing(3, theme only) */
				else choose = 406; /* brown pudding(28) */
			}
			else if (die < 29)
			{
				if (level < 15) choose = 112; /* small slime(6) */
				else if (level < 27) choose = 266; /* giant slime blob(16) */
				else if (level < 85) choose = 403; /* wereworm(27) */
				else choose = 730; /* great bile worm(67) */
			}
			else if (die < 43)
			{
				if (level < 16) choose = 836; /* bloodshot icky thing(9, theme only) */
				else if (level < 64) choose = 301; /* gelatinous cube(18) */
				else choose = 711; /* bile devil(61) */
			}
			else if (die < 55)
			{
				if (level < 6) choose = 835; /* bubbering icky thing(3, theme only) */
				else choose = 837; /* blue icky thing(16, theme only) */
			}
			else if (die < 71)
			{
				if (level < 4) choose = 112; /* small slime(6) */
				else if (level < 39) choose = 201; /* slime blob(12) */
				else choose = 603; /* giant ameoba slime(42) */
			}
			else if (die < 85)
			{
				if (level < 5) choose = 25; /* disgusting worm(1) */
				else if (level < 36) choose = 254; /* grey ooze(15) */
				else choose = 535; /* black pudding(37) */
			}
			else /* (die > 85) */
			{
				if ((level < 9) && (randint(100) < 46)) choose = 59; /* photoplasm(3) */
				else if ((level < 10) && (randint(100) < 85)) choose = 119; /* rot jelly(6) */
				else if (level < 6) choose = 59; /* photoplasm(3) */
				else if (level < 30) choose = 219; /* ochre jelly(13) */
				else choose = 470; /* ooze elemental(33) */
			}
			break;
		}
		case 4: /* volcano */
		{
			if (die < 14)
			{
				if ((level < 16) && (randint(100) < 36)) choose = 65; /* naga hatchling(3) */
				else if (level < 19) choose = 70; /* black naga(4) */
				else if (level < 30) choose = 358; /* giant firefly(24) */
				else if (level < 43) choose = 436; /* doombat(30) */
				else choose = 629; /* effretti(45) */
			}
			else if (die < 27)
			{
				if (level < 16) choose = 87; /* red worm mass(5) */
				else if (level < 37) choose = 348; /* fire vortex(23) */
				else choose = 538; /* plasma vortex(37) */
			}
			else if (die < 41)
			{
				if (level < 24) choose = 131; /* red naga(7) */
				else if (level < 60) choose = 449; /* golden naga(31) */
				else choose = 714; /* greater fire elemental(62) */
			}
			else if (die < 56)
			{
				if (level < 6) choose = 65; /* naga hatchling(3) */
				else if (level < 27) choose = 222; /* mordor orc(13) */
				else if (level < 27) choose = 222; /* mordor orc(13) */
				else if (level < 38) choose = 438; /* ifrit(30) */
				else choose = 562; /* fire elemental(39) */
			}
			else if (die < 71)
			{
				if (level < 7) choose = 87; /* red worm mass(5) */
				else if ((level < 14) && (randint(100) < 56)) choose = 163; /* baby red(9) */
				else if (level < 10) choose = 121; /* baby salamander(7) */
				else if (level < 28) choose = 335; /* salamander(21) */
				else if (level < 36) choose = 441; /* chimaera(30) */
				else choose = 529; /* magma elemental(37) */
			}
			else if (die < 85)
			{
				if (level < 16) choose = 121; /* baby salamander(7) */
				else if (level < 27) choose = 321; /* 3-headed hydra(20) */
				else if (level < 34) choose = 335; /* salamander(21) */
				else if ((level > 37) && (level < 56) && (randint(100) < 46)) 
                        choose = 576; /* 9-headed hydra(40) */
				else if (level < 75) choose = 509; /* hellhound(35) */
				else choose = 767; /* hellhound in groups(78) */
			}
			else /* (die > 85) */
			{
				if (level < 10) choose = 87; /* baby salamander(7) */
				else if (level < 30) choose = 337; /* red dragon bat(22) */
				else if (level < 81) choose = 478; /* fire giant(33) */
				else choose = 731; /* great hell wyrm(67) */
			}
			break;
		}
#if 0 /* removed / combined with DWARF_MINE */
		case 5: /* earthy cave */
		{
			if (die < 15)
			{
				if (level < 25) choose = 68; /* rock lizard(4) */
				else if (level < 44) choose = 439; /* xreek(30) */
				else if (level < 105) choose = 628; /* xaren(45) */
				else choose = 796; /* giant xreek(105) */
			}
			else if (die < 29)
			{
				if (level < 17) choose = 117; /* well lizard(6) */
				else if (level < 30) choose = 310; /* zhelung(19) */
				else if (level < 42) choose = 456; /* blinking zhelung(32) */
				else choose = 605; /* ancient zhelung(43) */
			}
			else if (die < 43)
			{
				if (level < 5) choose = 68; /* rock lizard(4) */
				else if (level < 25) choose = 240; /* lurker(14) */
				else if (level < 63) choose = 412; /* stone giant(28) */
				else choose = 717; /* greater earth elemental(62) */
			}
			else if (die < 57)
			{
				if (level < 14) choose = 93; /* rock mole(5) */
				else if (level < 36) choose = 271; /* umber hulk(16) */
				else choose = 572; /* xorn(40) */
			}
			else if (die < 71)
			{
				if (level < 7) choose = 88; /* rasti(6) */
				else if (level < 25) choose = 278; /* merret(17) */
				else if (level < 35) choose = 279; /* merret in groups(30) */
				else choose = 565; /* earth elemental(39) */
			}
			else if (die < 85)
			{
				if (level < 8) choose = 93; /* rock mole(5) */
				else if (level < 31) choose = 245; /* rubble mimmic(18) */
				else if (level < 45) choose = 475; /* crystal drake(33) */
				else choose = 640; /* great crystal drake(49) */
			}
			else /* (die >= 85) */
			{
				if (level < 9) choose = 103; /* large brown bat(6) */
				else if ((level < 34) || (randint(100) < 3)) choose = 309; /* stone golem(19) */
				else choose = 529; /* magma elemental(37) */
			}
			break;
		}
#endif
		case 6: /* giants vs titans (air) */
		{
			if (die < 16)
			{
				if (level < 30) choose = 73; /* kestral(4) */
				else if ((level < 75) && (randint(100) < 45-level/3)) choose = 480; /* invisible stalker(34) */
				else if ((level > 64) && (randint(100) < 50)) choose = 737; /* titan(70) */
				else if (level < 52) choose = 480; /* invisible stalker(34) */
				else choose = 719; /* great storm wyrm(63) */
			}
			else if (die < 32)
			{
				if (level < 15) choose = 878; /* flurry(3) */
				else if ((level < 29) && (randint(100) < 45)) choose = 299; /* gargoyle(18) */
				else if ((level < 36) && (randint(100) < 55)) choose = 300; /* group gargoyle(22) */
				else if (level < 36) choose = 347; /* cold vortex(23) */
				else if ((level > 62) && (randint(100) < 25)) choose = 732; /* great wyrm of thunder(67) */
				else if ((level > 58) && (randint(100) < 30)) choose = 719; /* great storm wyrm(63) */
				else if ((level > 45) && (randint(100) < 25)) choose = 681; /* chaos vortex(53) */
				else choose = 534; /* shardstorm(37) */
			}
			else if (die < 48)
			{
				if (level < 35) choose = 141; /* nighthawk(8) */
				else if ((level > 79) && (randint(100) < 70)) choose = 766; /* greater titan(90) */
				else if ((level >= 40) && (level < 96) && (randint(100) < 55)) choose = 614; /* valkyrie(43) */
				else if (level > 74) choose = 780; /* sky dragon(85) */
				else if (randint(100) < 51) choose = 537; /* nexus vortex(37) */
				else choose = 538; /* plasma vortex(37) */
			}
			else if (die < 65)
			{
				if (level < 6) choose = 73; /* kestral(4) */
				else if (randint(100) < 25 - level/10) choose = 176; /* tengu(10) */
				else if ((level < 20) || ((level < 60) && (randint(100) < 25 - level/11))) choose = 876; /* storm cloud(12) */
				else if ((level < 56) || (randint(100) < 25)) choose = 877; /* arrowhawk(24) */
				else choose = 716; /* greater air elemental(62) */
			}
			else if (die < 82)
			{
				if (randint(100) < 23 - level/10) choose = 185; /* dust vortex(11) */
				else if ((level < 20) && (randint(100) < 45)) choose = 172; /* goyley(10) */
				else if ((level < 22) && (randint(100) < 90)) choose = 199; /* groyle(12) */
				else if (level < 24) choose = 250; /* wind shade(14) */
				else if ((level < 72) && (randint(100) < 90-level)) choose = 575; /* cloud giant(40) */
				else if (level < 31) choose = 371; /* hill giant(25) */
				else choose = 627; /* storm giant(45) */
			}
			else
			{
				if ((level < 34) && (randint(100) < 50)) choose = 256; /* griffon(15) */
				else if (level < 32) choose = 113; /* fog cloud(6) */
				else if ((level < 41) && (randint(100) < 12)) choose = 507; /* margoyle(35) */
				else if ((level > 50) && (level < 81) && (randint(100) < 35)) choose = 692; /* lesser titan(56) */
				else if (((level < 103) && (randint(100) < 70)) || (level < 48)) choose = 564; /* air elemental(39) */
				else choose = 716; /* greater air elemental(62) */
			}
			break;
		}
		case 7: /* full moon */
		{
			if (die < 14)
			{
				if (level < 9) choose = 41; /* crow(2) */
				else if (level < 19) choose = 120; /* raven(7) */
				else if (level < 43) choose = 327; /* shadow cat(21) */
				else choose = 616; /* wereraven(46) */
			}
			else if (die < 28)
			{
				if (level < 27) choose = 220; /* grim(13) */
				else if ((level < 32) && (randint(100) < 49)) choose = 403; /* wereworm(27) */
				else choose = 848; /* black stalker cat(33) */
			}
			else if (die < 42)
			{
				if (level < 6) choose = 109; /* rabid rat(6) */
				else if (level < 38) choose = 233; /* wererat(14) */
				else choose = 843; /* weremumak(39, theme only) */
			}
			else if (die < 57)
			{
				if (level < 12) choose = 120; /* raven(7) */
				else if (level < 45) choose = 333; /* werewolf(22) */
				else if ((level < 55) && (randint(100) < 90-level)) choose = 333; /* werewolf(22) */
				else choose = 844; /* greater werewolf(50, theme only) */
			}
			else if (die < 71)
			{
				if ((level < 5) || ((level < 16) && (randint(100) < 10))) 
                    choose = 40; /* alleycat(2) */
				else if (level < 12) choose = 34; /* glass cat(8) */
				else if ((level > 23) && (randint(100) < level+52)) choose = 373; /* werebear(25) */
				else choose = 275; /* black cat(16) */
			}
			else if (die < 85)
			{
				if (level < 12) choose = 46; /* novice rogue(2) */
				else if (level < 19) choose = 100; /* novice rogue groups(6) */
				else if (level < 30) choose = 354; /* lightning rogue(23) */
				else if ((level < 40) && (randint(100) < 95-(level-25))) choose = 459; /* assassin(32) */
				else choose = 508; /* master assassin(35) */
			}
			else
			{
				if (level < 15) choose = 72; /* clear cat(4) */
				else if (level < 31) choose = 281; /* imp(17) */
				else if (level < 46) choose = 489; /* phantom(34) */
				else choose = 631; /* doppleganger(45) */
			}
			break;
		}
		case 8: /* haunted temple ruins */
		{
			if (die < 16)
			{
				if (level < 23) choose = 81; /* grey skeleton(4) */
				else if (level < 33) choose = 393; /* yellow skeleton(27) */
				else if (level < 50) choose = 510; /* lich(35) */
				else if ((randint(100) < (level-50)*5) && (level > 55)) choose = 726; /* archlich(65) */
				else if (level < 70) choose = 685; /* demilich(54) */
			}
			else if (die < 32)
			{
				if (level < 13) choose = 111; /* human zombie(6) */
				else if (level < 30) choose = 353; /* gory ghost(23) */
				else if ((level < 37) && (randint(100) < 5 + (36-level)*15)) choose = 467; /* solo barrow wight(33) */
				else choose = 549; /* barrow wight(38) */
			}
			else if (die < 48)
			{
				if (level < 14) choose = 184; /* potion mimmic(11) */
				else if ((level > 32) && (randint(100) < level+10))	choose = 586; /* nether wraith(40) */
				else if (((level > 30) && (randint(100) < level+5)) || (level > 34)) \
					choose = 559; /* black wraith(39) */
				else if (level < 35) choose = 381; /* white wraith(26) */
				else choose = 559; /* black wraith(39) */
			}
			else if (die < 64)
			{
				if (level < 16) choose = 236; /* flesh golem(14) */
				else if (level < 32) choose = 402; /* vampire(27) */
				else if (level < 55) choose = 601; /* vampire lord(42) */
				else choose = 686; /* elder vampire(54) */
			}
			else if (die < 80)
			{
				if (level < 14) choose = 126; /* lost soul(7) */
				else if (level < 18) choose = 339; /* human mummy(22) */
				else if ((level < 56) && (randint(100) < 7)) choose = 359; /* vampire bat(24) */
				else if (level < 96) choose = 411; /* ghost(28) */
				else if (randint(100) < 55) choose = 706; /* armored death(60) */
				else choose = 703; /* skull druj(59) */
			}
			else if (die < 91)
			{
				if (level < 9) choose = 77; /* kobold skeleton(4) */
				else if ((level < 14) || (randint(100) < 12)) choose = 197; /* greater poltergeist(11) */
				else if (randint(100) < 110-level*2) choose = 287; /* guardian naga(17) */
				else if (level < 26) choose = 339; /* human mummy(22) */
				else if (level < 50) choose = 483; /* black skeleton(34) */
			}
			else
			{
				if (level < 7) choose = 64; /* poltergeist(3) */
				else if (level < 16) choose = 234; /* scroll mimmic(14) */
				else if (level < 21) choose = 328; /* skull vendor(21) */
				else if ((level > 35) && (randint(100) < 27)) choose = 606; /* master lich(42) */
				else if ((level > 42) && (randint(100) < 27)) choose = 659; /* gorgon(49) */
				else if (level < 50) choose = 422; /* regenerating zombie(29) */
				else if (randint(100) < 35) choose = 689; /* dracolich(58) */
				else if (randint(100) < 40) choose = 637; /* undead beholder(56) */
				else if ((level > 65) && (randint(100) < 6)) choose = 738; /* greater rotting quylthulg(71) */
				else choose = 703; /* skull druj(59) */
			}
			break;
		}
		case 9: /* swamp */
		{
			if (die < 14)
			{
				if (level < 17) choose = 211; /* horned bullfrog(11) */
				else if ((level < 80) && (randint(100) < 90-level)) choose = 326; /* warty bullfrog(21) */
				else if (level < 89) choose = 351; /* giant horned bullfrog(23) */
				else if (level < 92) choose = 763; /* baby behemoth(88) */
				else choose = 790; /* behemoth(101) */
			}
			else if (die < 28)
			{
				if (level < 5) choose = 38; /* large orange frog(2) */
				else if (level < 24) choose = 217; /* large spotted frog(13) */
				else if (level < 41) choose = 420; /* giant python(27) */
				else choose = 841; /* marsh corpse(41, theme only) */
			}
			else if (die < 43)
			{
				if (level < 25) choose = 218; /* small null(13) */
				else if (level < 41) choose = 424; /* large null(28) */
				else choose = 624; /* giant null(44) */
			}
			else if (die < 57)
			{
				if (level < 6) choose = 94; /* harpy(5) */
				else if (level < 45) choose = 241; /* crockodile(14) */
				else choose = 674; /* nulfraz(50) */
			}
			else if (die < 71)
			{
				if (level < 18) choose = 230; /* mosquito(14) */
				else if (level < 23) choose = 231; /* alternate mosquito(20) */
				else choose = 842; /* swamp bat(25, theme only) */
			}
			else if (die < 84)
			{
				if (level < 8) choose = 122; /* acid blob(7) */
				else if (level < 62) choose = 295; /* null(18) */
				else if (level < 64) choose = 563; /* water elemental(39) */
				else choose = 715; /* greater water elemental(62) */
			}
			else
			{
				if (level < 23) choose = 94; /* harpy(5) */
				else if (randint(100) < 26) choose = 950; /* water moccasin(14) */
				else if (level < 86) choose = 421; /* hag(29) */
				else if (level < 118) choose = 765; /* young behemoth(90) */
				else choose = 804; /* giant behemoth(120) */
			}
			break;
		}
		case 10: /* dwarf mine */
		{
			if (die < 14)
			{
				if (level < 15) choose = 92; /* goblin(5) */
				else if ((level < 22) && (randint(100) < 11)) choose = 187; /* fire orc(11) */
				else if (level < 22) choose = 124; /* cave orc(7) */
				else if (level < 35) choose = 388; /* cave ogre(26) */
				else if (level < 43) choose = 536; /* rock troll(37) */
				else choose = 625; /* rock troll groups(44) */
			}
			else if (die < 28)
			{
				if ((level < 10) && (randint(100) < (10-level)*24)) choose = 118; /* creeping silver coins(6) */
				else if (level < 11) choose = 167; /* baby grey dragon(10) */
				else if ((level > 14) && (level < 32) && (randint(100) < 22)) choose = 310; /* zhelung(19) */
				else if (level < 19) choose = 223; /* creeping mithril coins(13) */
				else if (level < 44) choose = 830; /* blood diamond(21) */
				else choose = 838; /* ironwood tree(48, theme only) */
			}
			else if (die < 43)
			{
				if ((level < 14) && (randint(100) < (14-level)*27)) choose = 206; /* black dwarf(12) */
				else if ((level > 32) && (level < 39) && (randint(100) < (level-32)*5)) choose = 846; /* the summoning dark(41, theme only) */
				else if (level < 39) choose = 207; /* black dwarf groups(16) */
				else if (randint(100) < 22) choose = 613; /* ancient grey dragon(45) */
				else choose = 846; /* the summoning dark(41, theme only) */
			}
			else if (die < 57)
			{
				if (level < 5) choose = 55; /* scruffy looking hobbit(3) */
				else if (level < 28) choose = 215; /* mine mirage(13) */
				else if ((level < 32) && (randint(100) < (32-level)*10)) choose = 851; /* waiting dark(28,THEME_ONLY) */
				else if ((level < 66) && (randint(100) < 45)) choose = 486; /* mithril golem(34) */
				else if ((level > 59) && (randint(100) < 75)) choose = 727; /* silver idol(66) */
				else if ((level > 52) && (randint(100) < 56 + level/3)) choose = 782; /* enveloping dark(58) */
				else choose = 852; /* the following dark(35,THEME_ONLY) */
			}
			else if (die < 71)
			{
				if (level < 6) choose = 103; /* large brown bat(6) */
				else if ((level > 12) && (level < 26) && (randint(100) < 16)) choose = 225; /* behir(15) */
				else if ((randint(100) < (100-level*2)) || (level < 16)) choose = 237; /* mine vyrm(14) */
				else if ((randint(100) < (102-level*2)) || (level < 27)) choose = 329; /* earth hound(20) */
				else if (level < 32) choose = 439; /* xreek(30) */
				else if (randint(100) < (100-level)) choose = 565; /* earth elemental(39) */
				else if (level < 46) choose = 529; /* magma elemental(37) */
				else choose = 666; /* lesser balrog(50) */
			}
			else if (die < 85)
			{
				if (level < 7) choose = 93; /* rock mole(5) */
				else if ((level < 40) && (randint(100) < 11)) choose = 851; /* waiting dark(28,THEME_ONLY) */
				else if (level < 29) choose = 833; /* black dwarf miner(18) */
				else if ((level > 57) && (randint(100) < 28 + level/2)) choose = 717; /* greater earth elemental(62) */
				else choose = 839; /* grag high priest(32) */
			}
			else 
			{
				if (level < 10) choose = 117; /* well lizard(6) */
				else if ((level < 22) || (randint(100) > level+65)) choose = 303; /* black dwarf priest(19) */
				else if ((level > 34) && (level < 59) && (randint(100) < 20)) choose = 572; /* xorn(40) */
				else if ((level > 39) && (randint(100) < 16)) choose = 628; /* xaren(45) */
				else if (level < 84) choose = 350; /* black dwarf grag(24) */
				else choose = 756; /* greater balrog(79) */
			}
			break;
		}
		case 11: /* bug cave */
		{
			if (die < 14)
			{
				if (level < 9) choose = 33; /* centipede(2) */
				else if (level < 43) choose = 181; /* millipede(11) */
				else choose = 840; /* giant cockroach(46, theme only) */
			}
			else if (die < 28)
			{
				if (level < 21) choose = 106; /* giant gnat(6) */
				else if (level < 32) choose = 392; /* giant lightning bug(27) */
				else if (level < 64) choose = 466; /* giant lightning bug groups(33) */
				else choose = 741; /* gnawing bug(70) */
			}
			else if (die < 42)
			{
				if (level < 6) choose = 39; /* cave spider(2) */
				else if (level < 40) choose = 226; /* drider(13) */
				else choose = 609; /* drider of Achrya(43) */
			}
			else if (die < 56)
			{
				if (level < 10) choose = 130; /* dark elf(7) */
				else if (level < 36) choose = 204; /* dark elf priest(12) */
				else if (level < 49) choose = 592; /* priest of Achrya(41) */
				else choose = 654; /* high priest of Achrya(48) */
			}
			else if (die < 70)
			{
				if ((level < 14) || (randint(100) < 5)) choose = 179; /* giant spider(10) */
				else if (level < 45) choose = 255; /* giant tarantula(15) */
				else choose = 653; /* elder aranea(48) */
			}
			else if (die < 85)
			{
				if ((level < 5) && (randint(100) < 85)) choose = 56; /* giant black ant(3) */
				else if (level < 8) choose = 75; /* multi-hued centipede(4) */
				else if (level < 26) choose = 280; /* giant scorpion(17) */
				else if (randint(100) < 100-level) choose = 434; /* soldier ant(30) */
				else choose = 501; /* fire ant(35) */
			}
			else
			{
				if ((level < 15) && (randint(100) < level+2)) choose = 133; /* wolf spider(8) */
				else if (level < 15) choose = 149; /* nesting spider(9) */
				else if (level < 35) choose = 265; /* giant tick(16) */
				else choose = 548; /* aranea(38) */
			}
			break;
		}
		case 12: /* domain of the grepse */
		{
			if (die < 21)
			{
				if (level < 16) choose = 69; /* silver mouse(4) */
				else if ((level < 29) || (randint(100) < 4)) choose = 308; /* minidrake(19) */
				else if ((level < 49) || (randint(100) < 4)) choose = 457; /* limbo worm(32) */
				else choose = 661; /* wemu vyrm(49) */
			}
			else if (die < 39)
			{
				if (level < 19) choose = 105; /* clear eye(6) */
				else if (level < 36) choose = 345; /* soothing moss(23) */
				else if (level < 54) choose = 552; /* silver moss(38) */
				else if (level < 93) choose = 675; /* sleepy willow tree(55) */
				else choose = 769; /* disenchantree(73) */
			}
			else if (die < 60)
			{
				if (level < 31) choose = 171; /* silverworm(9) */
				else if ((level < 76) || (randint(100) < 105-level)) choose = 511; /* singing vyrm(35) */
				else choose = 679; /* grepse devil(60) */
			}
			else if (die < 80)
			{
				if (level < 33) choose = 214; /* mirage(13) */
				else if (level < 93) choose = 543; /* will o' the wisp(38) */
				else choose = 768; /* greater silver wemu(72) */
			}
			else
			{
				if ((level < 38) || (randint(100) < 130-(level*2))) choose = 269; /* vyrm(16) */
				else choose = 585; /* rastlig(45) */
			}
			break;
		}
		case 13: /* nightmare */
		{
			if (die < 14) 
			{
				if (rand_int(100) < 104-(level*6)) choose = 64; /* poltergeist(2) */
				else if ((level < 15) && (randint(100) < 85)) choose = 861; /* erlbold runt(8) */
				else if ((level < 18) || (randint(100) < 30-level)) choose = 810; /* tan dust bunny(14) */
				else if (level < 30) choose = 863; /* erlbold hob(19) */
				else if (level < 41) choose = 421; /* hag(29) */
				else choose = 623; /* headless horseman(45) */
			}
			else if (die < 29)
			{
				if (level < 27) choose = 197; /* greater poltergeist(11) */
				if (level < 40) choose = 865; /* erlbold hag(33) */
				else if (randint(100) < (level/2)-21) choose = 695; /* dread in groups(49) */
				else if (level < 80) choose = 553; /* dread(39) */
				else choose = 725; /* jabberwock(67) */
			}
			else if (die < 43)
			{
				if (level < 18) choose = 183; /* illusionist(11) */
				else if (level < 30) choose = 328; /* skull vendor(21) */
				else if (randint(100) < 75) choose = 463; /* nightmare doctor(32) */
				else if (level < 64) choose = 611; /* screaming reaper(43) */
				else choose = 710; /* dullahan(61) */
			}
			else if (die < 57)
			{
				if (level < 21) choose = 862; /* city erlbold(17) */
				else if ((level > 40) && (randint(100) < level/4)) choose = 631; /* doppleganger(45) */
				else if ((level > 40) && (randint(100) < level/4)) choose = 652; /* winged horror(48) */
				else choose = 864; /* erlbold ogre(24) */
			}
			else if (die < 70)
			{
				if ((level < 40) && (randint(100) < 12)) choose = 264; /* elete winged monkey(18) */
				else if (level < 21) choose = 263; /* winged monkey(16) */
				else if (level < 40) choose = 346; /* snakeman(23) */
				else choose = 555; /* furie(41) */
			}
			else if (die < 83)
			{
				r_ptr = &r_info[869]; /* check for Dominick(25) */
				if (level < 5) choose = 15; /* village witch(0) */
				else if ((level > 10) && (level < 37) && (randint(100) < 16) &&
					(r_ptr->curpop + r_ptr->cur_num < r_ptr->max_num)) choose = 869; /* Dominick */
				else if ((level < 20) && (randint(100) < 16)) choose = 221; /* witch(13) */
				else if (level < 36) choose = 344; /* skull wisp(23) */
				else choose = 566; /* night mare(39) */
			}
			else
			{
				if (level > 93) level = 93;
				if ((level > 27) && (level < 67) && (randint(100) < 14))
					choose = 433; /* snakeman sorcerer(30) */
				else if ((level > 36) && (level < 74) && (randint(100) < 14))
					choose = 570; /* wendigo(40) */
				else if ((level < 70) || (randint(100) < 98-level)) choose = 386; /* bogeyman(26) */
				else choose = 743; /* horned reaper(72) */
			}
			break;
		}
		case 14: /* hell hall */
		{
			if (die < 16)
			{
				if (level < 21) choose = 115; /* manes(6) */
				else if (randint(100) < 39-level) choose = 384; /* small black reaper(26) */
				else if (level < 44) choose = 513; /* small black reaper groups(36) */
				else if (level < 76) choose = 648; /* marilith(47) */
				else choose = 755; /* pit fiend(77) */
			}
			else if (die < 33)
			{
				if ((level < 27) && (randint(100) < 77)) choose = 140; /* impsprite(8) */
				else if (level < 33) choose = 281; /* imp(17) */
				else if ((level < 53) || (randint(100) < 10)) choose = 593; /* hezrou(41) */
				else choose = 688; /* barbazu(55) */
			}
			else if (die < 49)
			{
				if (level < 31) choose = 144; /* lemure(9) */
				else if (level < 45) choose = 520; /* demonologist(36) */
				else choose = 660; /* horned devil(49) */
			}
			else if (die < 67)
			{
				if (level < 35) choose = 244; /* homunculus(15) */
				else if ((level < 63) || (randint(100) < 11)) choose = 573; /* vrock(40) */
				else choose = 713; /* horned devil lord(62) */
			}
			else if (die < 83)
			{
				if (level < 39) choose = 272; /* quasit(16) */
				else if ((level < 59) || (randint(100) < 11)) choose = 630; /* nalfeshnee(45) */
				else choose = 707; /* bone devil(60) */
			}
			else
			{
				if (level < 20) choose = 291; /* evil eye(18) */
				else if ((level < 51) || (randint(100) < 10)) choose = 518; /* bodak(36) */
				else choose = 673; /* barbed devil(52) */
			}
			break;
		}
		case 15: /* barracks */
		{
			if (die < 20)
			{
				if (level < 6) choose = 14; /* battle-scarred veteran(0) */
				else if ((level < 18) || (randint(100) < 80-level*3)) choose = 99; /* novice warrior groups(6) */
				else if ((level < 34) || (randint(100) < 26)) choose = 355; /* hardened warrior(23) */
				else if ((level < 40) || (randint(100) < 6 + level/4)) choose = 522; /* bone soldier(34) */
				else if ((level > 52) && (randint(100) < level/3)) choose = 688; /* barbazu(55) */
				else choose = 602; /* olog(42) */
			}
			else if (die < 40)
			{
				if (level < 9) choose = 42; /* novice warrior(2) */
				else if ((level < 16) || (randint(100) < 80-level*3)) choose = 222; /* mordor orc(13) */
				else if ((level < 24) || (randint(100) < 65-(level-20)*4)) choose = 258; /* black ogre(18) */
				else if ((level < 66) && (randint(100) < 21)) choose = 414; /* black knight(28) */
				else if ((level > 25) && (randint(100) < 65-level)) choose = 435; /* ogre chieftain(30) */
				else if ((level > 31) && (randint(100) < 38-level/2)) choose = 487; /* centaur cheiftain(34) */
				else if ((level > 41) && (randint(100) < level/2)) choose = 619; /* knight templar(44) */
				else if ((level > 75) && (randint(100) < level/4)) choose = 755; /* pit fiend(77) */
				else choose = 414; /* black knight(28) */
			}
			else if (die < 55)
			{
				if (level < 9) choose = 60; /* novice archer(2) */
				else if ((level < 21) || (randint(100) < 86-level*3)) choose = 253; /* half orc(15) */
				else if ((level > 23) && (randint(100) < 24-level/4)) choose = 778; /* sniper(27) */
				else if ((level < 57) && (randint(100) < 51)) choose = 364; /* green knight(24) */
				else if ((level > 47) && (randint(100) < 32-level/4)) choose = 707; /* horned devil(49) */
				else if (level < 58) choose = 376; /* rohirrim defector(25) */
				else choose = 706; /* armored death(60) */
			}
			else if (die < 70)
			{
				if ((level > 14) && (level < 40) && (randint(100) < 16)) choose = 287; /* guardian naga(17) */
				else if ((level > 14) && (level < 56) && (randint(100) < (71-level)/2))
					choose = 302; /* orc captain(18) */
				else if ((level > 29) && (level < 93) && (randint(100) < 12))
					choose = 458; /* centaur warrior(32) */
				else if ((level > 29) && (level < 67) && (randint(100) < 16)) choose = 859; /* haradrim warriors(32) */
				else if ((level > 39) && (randint(100) < 10)) choose = 614; /* valkyrie(43) */
				else choose = get_mon_match(level, 5, FALSE); /* missile launcher */
			}
			else if (die < 82)
			{
				if (level < 9) choose = 42; /* novice warrior(2) */
				else if (level < 18) choose = 209; /* swordsman(12) */
				else if ((level < 36) && (randint(100) < 33)) choose = 307; /* warhorse(19) */
				else if ((level > 34) && (randint(100) < 49-level)) choose = 507; /* rock troll(37) */
				else if ((level < 36) || (randint(100) < 156-level*3)) choose = 312; /* firepower mercenary(20) */
				else if ((level > 41) && (randint(100) < level/2)) choose = 625; /* rock troll groups(44) */
				else choose = 557; /* death knight(39) */
			}
			else
			{
				if ((level < 14) || (randint(100) < 76-level)) choose = 209; /* swordsman(12) */
				else if ((level < 33) || (randint(100) < 22)) choose = 273; /* uruk(16) */
				else if (level < 38) choose = 507; /* margoyle(35) */
				else if ((level < 40) || ((level < 69) && (randint(100) < 25))) 
					choose = 568; /* margoyle groups(40) */
				else choose = 600; /* berserker(42) */
			}
			break;
		}
	}
	return choose;
}


/*
 * Require monster appropriate for monster temple
 * (monster temples on themed levels are handled separately)
 */
static bool temple_okay(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* monster temple monsters are supposed to be tough */
	if ((r_ptr->level < p_ptr->depth - 4) && (!(r_ptr->flags1 & (RF1_UNIQUE))))
		return (FALSE);
	else if (r_ptr->level < (p_ptr->depth * 2) / 3) return (FALSE);

	/* monster temple monsters */
	if (r_ptr->flags7 & RF7_TEMPLE) return (TRUE);

	/* assume not okay */
	return (FALSE);
}


/*
 * Return if a monster is appropriate for the current themed level
 * mode 2 has bigger chance to allow out of theme monsters
 * mode 1 has a chance to allow (almost) any monster.
 * mode 0 always forces appropriate monsters.
 */
bool theme_okay(int r_idx, int mode, bool vault)
{
	monster_race *r_ptr = &r_info[r_idx];
	int force;

	/* a monster temple is like a mini themed level */
	if ((!p_ptr->theme) && (r_ptr->flags7 & (RF7_THEME_ONLY)) &&
		(get_mon_num_hook == temple_okay)) return TRUE;
	/* theme-only monsters are never okay when there's no theme */
	else if ((!p_ptr->theme) && (r_ptr->flags7 & (RF7_THEME_ONLY))) return FALSE;
	/* all other monsters are okay when there's no theme */
	else if (!p_ptr->theme) return TRUE;

		/* theme monsters */
	/* the cold forest (yeti, windigo, other wild forest/hill stuff) */
	if ((p_ptr->theme == 1) && (r_ptr->flags7 & (RF7_CFOREST))) return TRUE;
	/* the fairy forest (light fairies, and some other forest stuff) */
	else if ((p_ptr->theme == 2) && (r_ptr->flags7 & (RF7_FFOREST))) return TRUE;
	/* the icky place (acid) */
	else if ((p_ptr->theme == 3) && (r_ptr->flags7 & (RF7_ICKY_PLACE))) return TRUE;
	/* volcano (fire/mordor) */
	else if ((p_ptr->theme == 4) && (r_ptr->flags7 & (RF7_VOLCANO))) return TRUE;
#if 0 /* removed */
	/* earth */
	else if ((p_ptr->theme == 5) && (r_ptr->flags7 & (RF7_EARTHY_CAVE))) return TRUE;
#endif
	/* air / giants & titans */
	else if ((p_ptr->theme == 6) && (r_ptr->flags7 & (RF7_WINDY_CAVE))) return TRUE;
	/* full moon (werebeasts, magic cats,.. / lightning) */
	else if ((p_ptr->theme == 7) && (r_ptr->flags7 & (RF7_FULL_MOON))) return TRUE;
	/* haunted castle (mostly undead...) */
	else if ((p_ptr->theme == 8) && (r_ptr->flags7 & (RF7_CASTLE))) return TRUE;
	/* swamp (water) */
	else if ((p_ptr->theme == 9) && (r_ptr->flags7 & (RF7_SWAMP))) return TRUE;
	/* dwarf mine (earth) */
	else if ((p_ptr->theme == 10) && (r_ptr->flags7 & (RF7_DWARF_MINE)))
    {
		/* Earth cave: dwarves, gnomes & orcs no longer considered in-theme very deep */
		if ((strchr("hoy", r_ptr->d_char)) && (p_ptr->depth > 105) && (randint(100) < 76)) 
			/*return FALSE skip */;
		else return TRUE;
	}
	/* bug cave */
	else if ((p_ptr->theme == 11) && (r_ptr->flags7 & (RF7_BUG_CAVE))) return TRUE;
	/* silver (grepse & certain others) */
	else if ((p_ptr->theme == 12) && (r_ptr->flags7 & (RF7_GREPSE))) return TRUE;
	/* nightmare / dark fairy city */
	else if ((p_ptr->theme == 13) && (r_ptr->flags7 & (RF7_DARK_CITY))) return TRUE;
	/* hell hall (demons & devils) */
	else if ((p_ptr->theme == 14) && (r_ptr->flags7 & (RF7_HELL_HALL))) return TRUE;
	/* army barracks */
	else if ((p_ptr->theme == 15) && (r_ptr->flags7 & (RF7_BARRACKS))) return TRUE;

	/* Hacky: allow water elementals from the swamp to appear in the cold forest */
	if ((p_ptr->theme == 1) && (r_ptr->flags7 & (RF7_SWAMP)) &&
		((r_ptr->flags7 & (RF7_WATER_HIDE)) || (r_ptr->flags7 & (RF7_WATER_ONLY))) &&
		(strchr("X%", r_ptr->d_char))) return TRUE;

	/* THEME_ONLY always requires correct theme for its flag */
	if (r_ptr->flags7 & (RF7_THEME_ONLY)) return FALSE;
	/* mode 0 always requires correct theme */
	if (mode == 0) return FALSE;

		/* theme conflicts */
	/* volcano monsters never in cold forest */
	if ((p_ptr->theme == 1) && (r_ptr->flags7 & (RF7_VOLCANO))) return FALSE;
	/* cold forest monsters never in volcano */
	if ((p_ptr->theme == 4) && (r_ptr->flags7 & (RF7_CFOREST))) return FALSE;
	/* no demons or grepse in fairy forest */
	if ((p_ptr->theme == 2) && (r_ptr->flags3 & (RF3_DEMON))) return FALSE;
	if ((p_ptr->theme == 2) && (r_ptr->flags3 & (RF3_SILVER))) return FALSE;
	/* no creatures of light in the HELL_HALL */
	if ((p_ptr->theme == 14) && (r_ptr->flags3 & (RF3_CLIGHT))) return FALSE;
	/* no light fairies in the domain of the grepse */
	if ((p_ptr->theme == 12) && (strchr("y", r_ptr->d_char))) return FALSE;
	/* no light fairies in the dark fairy city */
	if ((p_ptr->theme == 13) && (strchr("y", r_ptr->d_char)) &&
		(r_ptr->flags3 & (RF3_CLIGHT))) return FALSE;
	/* no trees or centaurs in dwarf mine or volcano */
	if ((((p_ptr->theme == 10) && (randint(100) < 80)) || (p_ptr->theme == 4)) && 
		(strchr("EY", r_ptr->d_char))) return FALSE;

	force = 15;
	/* (some themes have different odds to accept non-theme monsters) */
	if (p_ptr->theme == 10) force = 13; /* DWARF_MINE */
	/* mode == 2 means we're using a room theme with no level theme */
	if (mode == 2) force = 25;
	if (vault) force -= 2;
	/* if no conflicts, then doesn't always require correct theme flag */
	if (rand_int(100) < force) return TRUE;

	/* don't accept */
	return FALSE;
}


/*
 * Hack -- help pick an escort type
 * These were moved here from further down in the file.
 */
static int place_monster_idx = 0;

/*
 * Hack -- help pick an escort type
 */
static bool place_monster_okay(int r_idx)
{
	monster_race *r_ptr = &r_info[place_monster_idx];
	monster_race *z_ptr = &r_info[r_idx];
    cptr rname = (r_name + r_ptr->name);
    cptr zname = (r_name + z_ptr->name);
    bool exception = FALSE;
	
	/* Make exceptions for certain escort leaders */
	 /* Azog can have troll escorts sometimes */
	if ((strstr(rname, "King of the orcs")) && (strstr(zname, "troll")) && 
		(randint(100) < 9)) exception = TRUE;
	 /* The Great Goblin was allies with wolves */
	 /* (checks for 'C' AND wolf to prevent wolf spiders or jackals) */
	if ((strstr(rname, "Great Goblin")) && (strstr(zname, "wolf")) && 
		(strchr("C", z_ptr->d_char)) && (randint(100) < 10)) exception = TRUE;
	 /* orcs sometimes ally with wargs */
	if ((strchr("o", r_ptr->d_char)) && (strstr(zname, "warg")) && 
		(randint(100) < 5)) exception = TRUE;
	 /* The Sire of all werewolves may lead any werebeast */
	if ((strstr(rname, "Draugluin")) && (strstr(zname, "were")) && 
		(randint(100) < 55)) exception = TRUE;
	 /* An artifact mentions Gothmog's troll guard (don't allow weakest trolls) */
	if ((strstr(rname, "Gothmog")) && (strstr(zname, "troll")) && 
		(z_ptr->level >= 25) && (randint(100) < 11)) exception = TRUE;
	 /* allow the icky king to have slimes & puddings */
	if ((strstr(rname, "Icky King")) && (strchr("S", z_ptr->d_char)) && 
		(randint(100) < 25)) exception = TRUE;
	/* Wulf may have other outlaw/warrior escorts ("K" with "p" escorts) */
	if ((strstr(rname, "Wulf")) && 
		((strstr(zname, "swordsm")) || (strstr(zname, "hardened")) || 
		(strstr(zname, "warhorse")) || (strstr(zname, "lightning ro"))) && 
		(randint(100) < 34)) exception = TRUE;
	 /* The White Witch: wolves, demons, less commonly dwarves and cold monsters */
	if ((strstr(rname, "White Witch")) && ((strstr(zname, "warg")) ||
		(strstr(zname, "white wolf")) || (z_ptr->flags3 & (RF3_DEMON))) &&
		(randint(100) < 90)) exception = TRUE;
	if ((strstr(rname, "White Witch")) && (z_ptr->flags7 & (RF7_CFOREST)) &&
		(randint(100) < 35)) exception = TRUE;
	if ((strstr(rname, "White Witch")) && (strstr(zname, "dwarf")) &&
		(randint(100) < 13)) exception = TRUE;
	
	/* (usually) Require similar "race" */
	if ((z_ptr->d_char != r_ptr->d_char) && (!exception)) return (FALSE);

		/* 'T' symbol complications.. */
    /* Hack: Tom, Bert & Bill shouldn't have gargoyle escorts */
    if ((strstr(rname, "the Stone Troll")) && (strstr(zname, "oyle"))) return (FALSE);
	/* Hack: Rogrog should only rarely have gargoyle escorts */
	if ((strstr(rname, "Rogrog")) && (strstr(zname, "oyle")) && (randint(100) < 80)) 
		return (FALSE);
	/* Ramvodo the gargoyle king should have mostly gargoyle escorts */
	if ((strstr(rname, "Gargoyle")) && (!strstr(zname, "oyle")) && (randint(100) < 75)) 
		return (FALSE);
	
	/* Skip more advanced monsters */
	if (z_ptr->level > r_ptr->level) return (FALSE);

	/* Skip unique monsters */
	if (z_ptr->flags1 & (RF1_UNIQUE)) return (FALSE);

	/* Paranoia -- Skip identical monsters */
	if (place_monster_idx == r_idx) return (FALSE);

	/* NEVER_MOVE monsters don't make good escorts */
	if ((z_ptr->flags1 & (RF1_NEVER_MOVE)) && (randint(100) < 85)) return (FALSE);

	/* Okay */
	return (TRUE);
}


/*
 * Hack -- the "type" of the current "summon specific"
 */
static int summon_specific_type = 0;

/*
 * Hack -- help decide if a monster race is "okay" to summon
 */
static bool summon_specific_okay(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];
    cptr rname = (r_name + r_ptr->name);
	bool okay = FALSE;

    /* get summoner race also */
	monster_race *sr_ptr = &r_info[summoner];

	/* Extract the racial spell flags */
	u32b f4, f5, f6;
	f4 = r_ptr->flags4;
	f5 = r_ptr->flags5;
	f6 = r_ptr->flags6;
	
	/* if summoner is hurt by fire, exclude fire monsters */
    if (sr_ptr->Rfire < 0)
	{
		/* exclude fire monsters */
		if ((r_ptr->flags4 & (RF4_BR_FIRE)) ||
			(strstr(rname, "fire")) || (strstr(rname, "plasma")) ||
			(strstr(rname, "tyerr")) || (strstr(rname, "salamander")) || 
			(strstr(rname, "ifrit")) ||	(strstr(rname, "effretti")) || 
			(strstr(rname, "bodak")) ||	(strstr(rname, "horned devil")) || 
			(strstr(rname, "bronze idol")) || (strstr(rname, "lava")))
			return FALSE;
	}
	/* This should probably have something for monsters hurt by cold as well */

	/* is summoner hates water, exclude water monsters */
    if (sr_ptr->Rwater < -1)
	{
		/* exclude water monsters */
		if ((strstr(rname, "water")) || (r_ptr->flags7 & (RF7_WATER_HIDE)) ||
			(r_ptr->flags7 & (RF7_WATER_ONLY)) || (r_ptr->d_char == 'N') ||
               (r_ptr->flags5 & (RF5_BA_WATE))) 
			return FALSE;
	}
	/* if summoner is evil or hurt by light, */
	/* exclude light-breathers and creatures of light */
    if ((sr_ptr->flags3 & (RF3_EVIL)) || (sr_ptr->Rlite < -1))
	{
		/* exclude light breathers and creatures of light */
		if ((r_ptr->flags4 & (RF4_BR_LITE)) || (r_ptr->flags3 & (RF3_CLIGHT)))
			return FALSE;
	}
	/* if summoner is hurt by dark, exclude darkness breathers */
	if (sr_ptr->Rdark < 0)
	{
		if (r_ptr->flags4 & (RF4_BR_DARK)) return FALSE;
	}

	/* Hack -- no specific type specified (for S_MONSTER(S) spell) */
	if (!summon_specific_type) return (TRUE);

	/* Check standard requirements */
	switch (summon_specific_type)
	{
		case SUMMON_ANIMAL:
		{
			okay = ((r_ptr->flags3 & (RF3_ANIMAL)) &&
			        !(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		case SUMMON_SPIDER:
		{
			okay = ((r_ptr->d_char == 'x') &&
			        !(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		case SUMMON_HOUND:
		{
			okay = (((r_ptr->d_char == 'C') || (r_ptr->d_char == 'Z')) &&
			        !(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		case SUMMON_HYDRA:
		{
			okay = ((r_ptr->d_char == 'M') &&
			        !(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		case SUMMON_APE:
		{
			okay = ((r_ptr->d_char == 'A') &&
			        !(r_ptr->flags1 & (RF1_UNIQUE)));
			        
			/* The Wicked witch of the West prefers winged monkeys */
			if ((summoner == 311) && (!strstr(rname, "winged")) &&
				(randint(100) < 81)) okay = FALSE;
			break;
		}

		case SUMMON_DEMON:
		{
			okay = ((r_ptr->flags3 & (RF3_DEMON)) &&
			        !(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		case SUMMON_UNDEAD: 
		{
			okay = ((r_ptr->flags3 & (RF3_UNDEAD)) &&
			        !(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		case SUMMON_DRAGON:
		{
			okay = ((r_ptr->flags3 & (RF3_DRAGON)) &&
			        !(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		case SUMMON_SILVER:
		{
			okay = ((r_ptr->flags3 & (RF3_SILVER)) &&
			        !(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		case SUMMON_NMARE: /* nightmares */
		{
			okay = ((r_ptr->flags3 & (RF3_NIGHTMARE)) &&
			        !(r_ptr->flags1 & (RF1_UNIQUE)));
			if ((r_ptr->level >= sr_ptr->level) && (randint(100) < 99-badluck/2)) okay = FALSE;
			break;
		}

		/* dwarvish curses (grag & grag high priest are only two who get this spell) */
		case SUMMON_DARK: 
		{
			cptr dname = (r_name + r_ptr->name);
			/* check a few things to make sure we get the right monsters */
			if ((strchr("&u", r_ptr->d_char)) && (strstr(dname, "dark")) &&
				((r_ptr->d_attr == TERM_BLUE) || (r_ptr->d_attr == TERM_L_BLUE)) &&
				(r_ptr->flags7 & (RF7_DWARF_MINE))) okay = TRUE;
			if (r_ptr->flags1 & (RF1_UNIQUE)) okay = FALSE;
			break;
		}

		case SUMMON_ARMY: /* military monsters (may allow uniques) */
		{
			int luck = 2 + (goodluck*4/5) - badluck;
			if ((r_ptr->level > sr_ptr->level) && (sr_ptr->level > p_ptr->depth - 2))
				luck += (r_ptr->level - sr_ptr->level) * 2;
			else if (r_ptr->level > sr_ptr->level) luck += r_ptr->level - sr_ptr->level;
			if (p_ptr->depth > sr_ptr->level + 8) luck -= (p_ptr->depth - sr_ptr->level)/4;
			if (p_ptr->depth > sr_ptr->level + 8) luck -= (p_ptr->depth - sr_ptr->level)/4;

			if (r_ptr->flags7 & (RF7_ARMY)) okay = TRUE;
			if ((r_ptr->flags1 & (RF1_UNIQUE)) && (rand_int(21) < luck)) okay = FALSE;
			else if ((r_ptr->level > sr_ptr->level + 2) && (rand_int(25) < luck)) okay = FALSE;
			break;
		}

		case SUMMON_KIN:
		{
			okay = ((r_ptr->d_char == summon_kin_type) &&
			        !(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}
		
		case CHOOSE_DISGUISE:
		{
			okay = ((r_ptr->d_char == sr_ptr->d_char) &&
			        !(r_ptr->flags1 & (RF1_UNIQUE)));
			/* 'p' monsters can disguise as 't' monsters (as long as it's not a town monster) */
			if ((sr_ptr->d_char == 'p') && (r_ptr->d_char == 't') && (r_ptr->level)) 
				okay == TRUE;
			/* no cross-dressing */
			if (((sr_ptr->flags1 & (RF1_MALE)) && (r_ptr->flags1 & (RF1_FEMALE))) ||
				((r_ptr->flags1 & (RF1_MALE)) && (sr_ptr->flags1 & (RF1_FEMALE)))) okay = FALSE;
			if (r_ptr->level >= sr_ptr->level * 2 / 3) okay = FALSE;
			if (r_ptr->maxpop < 10) okay = FALSE;
			break;
		}

		case SUMMON_HI_UNDEAD:
		{
			okay = ((r_ptr->d_char == 'L') ||
			        (r_ptr->d_char == 'V') ||
			        (r_ptr->d_char == 'W'));
			break;
		}

		case SUMMON_HI_DRAGON:
		{
			/* I don't think non-unique dragons should summon Ancalagon or Glaurung */
			if ((!(sr_ptr->flags1 & (RF1_UNIQUE))) && (sr_ptr->d_char == 'D') &&
				(r_ptr->flags1 & (RF1_UNIQUE)) && (r_ptr->level > 70)) return FALSE;

			okay = (r_ptr->d_char == 'D');
			break;
		}

		case SUMMON_HI_DEMON:
		{
			okay = ((r_ptr->d_char == 'U') ||
			        (r_ptr->d_char == '&'));
			break;
		}

		/* require correct color becuase there are now other unique 'W's */
		case SUMMON_WRAITH:
		{
			okay = ((r_ptr->d_char == 'W') &&
					(r_ptr->d_attr == TERM_RED) &&
			        (r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

		case SUMMON_UNIQUE:
		{
			okay = (r_ptr->flags1 & (RF1_UNIQUE)) ? TRUE : FALSE;
			if (r_ptr->maxpop == 1) okay = TRUE;
			/* sometimes allow S_UNIQUE to summon 'almost-uniques' */
			if (((strstr(rname, "king")) || (strstr(rname, "queen")) ||
				(strstr(rname, "fool")) || (strstr(rname, "lord")) ||
				(strstr(rname, "captain")) || (strstr(rname, "spectral")) ||
				(strstr(rname, "chief")) || (strstr(rname, "Beruthi"))) && 
                (r_ptr->level > sr_ptr->level - 25) && (rand_int(100) < 20 + badluck))
				okay = TRUE;
			break;
		}
	}

	/* Result */
	return (okay);
}


/*
 * A separate function to choose a monster to place in the town 
 */
s16b get_mon_num_town(void)
{
	int i, j, p, erlev;
	int r_idx;
	long value, total;
	bool night;
	int plev = p_ptr->lev;
	int maxd = p_ptr->max_depth;

	monster_race *r_ptr;
	alloc_entry *table = alloc_race_table;
	
	/* night or day */
    if ((turn % (10L * TOWN_DAWN)) < ((10L * TOWN_DAWN) / 2)) night = FALSE;
	else night = TRUE;

	/* Reset total */
	total = 0L;

	/* Process probabilities */
	for (i = 0; i < alloc_race_size; i++)
	{
        cptr dname;
        /* Default */
		table[i].prob3 = 0;
		
		/* Get the "r_idx" of the chosen monster */
		r_idx = table[i].index;

		/* Get the actual race */
		r_ptr = &r_info[r_idx];
		dname = (r_name + r_ptr->name);

		/* effective monster level */
		erlev = r_ptr->level;

		/* Only TOWNOK dungeon monsters allowed in the town */
        if ((!(r_ptr->flags7 & (RF7_TOWNOK))) && (erlev > 0)) continue;

		/* poltergeists have TOWNOK (but never in the daytime) */
		if ((r_ptr->flags2 & (RF2_INVISIBLE)) && (!night)) continue;
		else if (r_ptr->flags2 & (RF2_INVISIBLE)) erlev += 2;
		/* ravens have TOWNOK */
		if (strstr(dname, "raven")) erlev += 3;
		
		/* Make sure all TOWNOK have some chance of appearing in the town */
        if (erlev > 43) erlev = 43;
		
		/* deeper TOWNOK monsters should only appear when the PC is tough */
        if ((erlev > 25) && (plev <= erlev + 5)) continue;
		
		/* TOWNOK werebeasts only allowed on a full moon */
        if ((strstr(dname, "were")) && (!(p_ptr->theme == 7))) continue;
		
		/* limits:  this prevents dungeon monsters (except dL1 monsters) */
		/*  in the town at the very beginning of the game  */
		if ((erlev > 1) && ((plev <= erlev + 1) || (maxd <= erlev + 1))) continue;

		/* Hack -- "unique" monsters must be "unique" */
		if (((r_ptr->flags1 & (RF1_UNIQUE)) || (r_ptr->maxpop == 1)) &&
		    (r_ptr->curpop + r_ptr->cur_num >= r_ptr->max_num)) continue;

		/* Accept */
		table[i].prob3 = table[i].prob2;

		/* FULL_MOON theme occationally allowed in town */
		if ((p_ptr->theme == 7) && (night) && 
			(r_ptr->flags7 & (RF7_FULL_MOON)))
		{
			/* no increasing the rarity */;
		}
		/* dungeon monsters normally less common in the town than town monsters */
		else if ((night) || (erlev <= 3) || 
			((p_ptr->theme == 7) && (r_ptr->flags7 & (RF7_FULL_MOON))))
		{
			if (r_ptr->level > 0) table[i].prob3 = (table[i].prob3 + 2) / 3;
		}
		/* (most dungeon monsters rarely come out in the daytime) */
		else if (r_ptr->level > 0) table[i].prob3 = (table[i].prob3 + 4) / 5;

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

	/* Try for a "harder" monster (rare in the town) */
	if ((p < 1 + badluck) && (plev > 5))
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

	/* Result */
	return (table[i].index);
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
 * DJA 1.2.0 introduced the TOWKOK flag which makes exceptions to this
 * rule, allowing dungeon monsters to appear in the town (but never at
 * the very beginning of the game).
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
s16b get_mon_num(int level, bool vault)
{
	int i, j, p;
	int r_idx;
	long value, total;
	bool boosted = FALSE;
	bool summoned = FALSE;
	bool faketheme = FALSE;
	int levelb = level; /* level before boost */
	int truetheme = p_ptr->theme;

	monster_race *r_ptr;
	alloc_entry *table = alloc_race_table;

	/* escorts and summoned monsters */
	if ((get_mon_num_hook == place_monster_okay) || 
		(get_mon_num_hook == summon_specific_okay) ||
		(get_mon_num_hook == match_okay))
		summoned = TRUE;

	/* paranoia */
	if (level <= 0)
	{
		if ((!p_ptr->depth) && (p_ptr->max_depth > 4)) level = p_ptr->max_depth/4;
		else if (!p_ptr->depth) level = 1;
		else level = p_ptr->depth;
		levelb = level;
	}

	/* Boost the level */
	/* Occasional "nasty" monster */
	if (rand_int(NASTY_MON) == 0)
	{
		/* Pick a level bonus */
		int d = level / 4 + 2;

		/* Boost the level */
		level += ((d < 5) ? d : 5);
			
		boosted = TRUE;
	}

	/* Occasional "nasty" monster (NASTY_MON = 50, set in line304 in defines.h) */
	if (rand_int(NASTY_MON) == 0)
	{
		/* Pick a level bonus */
		int d = level / 4 + 2;
		if ((level > 50) && (d < 20)) d = 20;
		if ((level > 31) && (d < 15)) d = 15;
		if ((level > 14) && (d < 10)) d = 10;

		/* occationally allow way out of depth monsters */
           if (d > 5)
		{
		    d = 2 + randint(d-2);
		    /* 1/NASTY_MON to withdrawl cap on d */
		    if ((d > 5) && (rand_int(NASTY_MON) > 0)) d = 5;
        }

		/* Boost the level */
		level += d;
			
		boosted = TRUE;
	}
	
	/* (allow deeper monsters on themed levels, especially on very deep levels) */
	/* vault monsters are already boosted */
	if ((!boosted) && (!vault) && (p_ptr->theme) && (randint(100) < 20)) 
	{
		int booo = rand_int(7);
		level += booo; 
		if (booo > 2) boosted = TRUE; 
	}
	if ((p_ptr->depth > 80) && (p_ptr->theme) && (randint(100) < 12))
	{
		if ((!boosted) && (!vault)) level += randint(8);
		else level += randint(2);
    }

	/* more common slight increase (7% with NASTY_MON == 50) */
	if ((!boosted) && (!vault) && (!p_ptr->theme) &&
		(rand_int(100) < NASTY_MON / 7) && (p_ptr->depth > 2)) level += randint(2);
	/* rare decrease (4% with NASTY_MON == 50), can happen on a themed level */
	else if ((!boosted) && (!vault) && (level > 9) && (p_ptr->depth < 99) &&
		(rand_int(100) < (99-NASTY_MON) / 11)) level -= randint(2);
	
	/* never boost summoned monsters */
	if (get_mon_num_hook == summon_specific_okay) level = levelb;

    /* don't boost off the scales */
    if (level > MAX_DEPTH - 1) level = MAX_DEPTH - 1;
	/* don't boost much on very shallow levels */
	if ((p_ptr->depth < 6) && (level > levelb + 3)) level = levelb + rand_int(3);
	else if ((p_ptr->depth < 10) && (level > levelb + 5)) level = levelb + rand_int(6);

	/* Reset total */
	total = 0L;
	
	/* sometimes choose a monster according to room theme if no level theme */
	/* (vaults and designed rooms only: uses mode 2 in theme_okay() ) */
	if ((room_design_theme) && (!p_ptr->theme))	
	{
		p_ptr->theme = room_design_theme; /* resets at end of function */
		faketheme = TRUE;
	}

	/* Process probabilities */
	for (i = 0; i < alloc_race_size; i++)
	{
		/* Monsters are sorted by depth */
		if (table[i].level > level) break;

		/* Default */
		table[i].prob3 = 0;
		
		/* I'm still getting way out of depth stuff on themed levels, so add this: */
		/* don't allow significantly out of depth non-theme appropriate monsters */
		if ((table[i].level > p_ptr->depth+4) && (!faketheme) && (!vault) && 
			(!theme_okay(r_idx, 0, FALSE)))
		{
			continue; /* */
		}
		/* don't allow too out of depth except in a vault */
        else if ((table[i].level > levelb+2) && (!vault) && (p_ptr->theme) && 
                 (p_ptr->depth < 96))
        {
			int blahhh = rand_int(11);
			if (p_ptr->depth < 10) blahhh = rand_int(6);
			/* small chance to allow much more OOD */
			if ((blahhh = 10) && (randint(100) < 6)) blahhh = 10 + rand_int(15);
            if ((blahhh < table[i].level - levelb) &&
                (rand_int(500) > 80 - p_ptr->depth/2)) continue;
        }
		/* sometimes reject monsters which are too easy for the level */
		/* not common unless the monster is way out of depth or it's in a vault */
		else if ((table[i].level < p_ptr->depth - 30) || ((vault) && (table[i].level < p_ptr->depth - 15)))
		{
			/* was int rejectchance = (p_ptr->depth - table[i].level - 25)/2; */
			int rejectchance = (p_ptr->depth - table[i].level - 20)/2;

			if ((vault) && (table[i].level < p_ptr->depth/5)) rejectchance += 10;
			if ((table[i].level < 9) && ((p_ptr->depth > 55) || (vault))) rejectchance += 10;

			if ((rejectchance > 40) && (!vault)) rejectchance = 40;
			if (rand_int(100) < rejectchance) continue;
		}
		/* very shallow monsters never in deep vaults */
		if ((table[i].level < p_ptr->depth/10) && (levelb >= 50) && (vault)) continue;

		/* Get the "r_idx" of the chosen monster */
		r_idx = table[i].index;

		/* Get the actual race */
		r_ptr = &r_info[r_idx];

		/* Hack -- No town monsters in dungeon */
		/* (except if very shallow and appropriate to themed level) */
		if ((p_ptr->theme) && (theme_okay(r_idx, 0, vault)) && (!faketheme) &&
			(p_ptr->depth < 8) && (!vault)) /* okay */; /* was (p_ptr->depth < 20) */
		else if (table[i].level <= 0) continue;

		/* town and HELPER monsters can be randomly generated on themed */
		/* levels but don't allow them to be summoned or placed in vaults */
		if (((vault) || (summoned)) && 
			((r_ptr->flags3 & (RF3_HELPER)) || (table[i].level <= 0))) continue;

		/* Hack -- "unique" monsters must be "unique" */
		if (((r_ptr->flags1 & (RF1_UNIQUE)) || (r_ptr->maxpop == 1)) &&
		    (r_ptr->curpop + r_ptr->cur_num >= r_ptr->max_num)) continue;

		/* Depth Monsters never appear out of depth */
		if ((r_ptr->flags1 & (RF1_FORCE_DEPTH)) && (r_ptr->level > p_ptr->depth))
			continue;

		/* Accept */
		table[i].prob3 = table[i].prob2;

		/* don't check themed level when there's a get_mon_num_hook */
		/* (but don't summon theme-only monsters when not on a themed level) */
		/* (this allows THEME_ONLY monsters in non-themed nests & pits) */
		if ((!get_mon_num_hook) || ((r_ptr->flags7 & (RF7_THEME_ONLY)) && (summoned)))
		{
			/* always common when appropriate for current themed level */
			/* prob is defined as 100/r_ptr->rarity in init2.c */
			/* so common monsters have a prob of 100(1) or 50(2) */
			if ((p_ptr->theme) && (theme_okay(r_idx, 0, vault)) && (table[i].prob2))
			{
				int minus = (r_ptr->rarity*5);
                if (minus > 80) minus = 80;
                if (table[i].prob3 < 75) table[i].prob3 = 84 - minus;
				/* else prob3 is 100 */
			}
			/* don't allow if not appropriate */
			/* can't just say 'continue' because prob3 has already been set */
			else if ((!faketheme) && (!theme_okay(r_idx, 1, vault))) table[i].prob3 = 0;
			/* mode 2 (bigger chance to accept out of theme) if roomtheme with no level theme */
			else if ((faketheme) && (!theme_okay(r_idx, 2, vault))) table[i].prob3 = 0;
		}

		/* Total */
		total += table[i].prob3;
	}
	
	/* reset */
	p_ptr->theme = truetheme;

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

	/* on a themed level: if it's already out of depth or very close to equal depth, */
    /* then don't try for a deeper monster */
	if ((p_ptr->theme) && (table[i].level >= levelb - 2)) return (table[i].index);

	/* Power boost */
	p = rand_int(100);

	/* Try for a "harder" monster once (50%) or twice (10%) */
	if (p < 60)
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
	if (p < 10)
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

	/* Result */
	return (table[i].index);
}

/* get the general type of monster you heard */
/* (I don't know how I could turn this into a switch?) */
cptr get_hear_race(monster_race *r_ptr)
{
	if (strchr("A", r_ptr->d_char)) return "an ape";
	if (strchr("acFIS", r_ptr->d_char)) return "a bug";
	if (strchr("B", r_ptr->d_char)) return "a bird";
	if (strchr("b", r_ptr->d_char)) return "a bat";
	if (strchr("C", r_ptr->d_char)) return "a canine";
	if (strchr("dD", r_ptr->d_char)) return "a dragon";
	if (strchr("E", r_ptr->d_char)) return "a moving tree"; /* (NEVER_MOVE monsters are silent) */
	if (strchr("e", r_ptr->d_char)) return "an eye";
	if (strchr("f", r_ptr->d_char)) return "a feline (you must have insanely good ears)";
	if (strchr("GW", r_ptr->d_char)) return "something ghostly";
	if (strchr("g", r_ptr->d_char)) return "a golem";
	if (strchr("HMq", r_ptr->d_char)) return "a beast";
	if (strchr("h", r_ptr->d_char)) return "a humanoid monster";
	if (strchr("i", r_ptr->d_char)) return "a dark fairy or imp";
	if (strchr("J", r_ptr->d_char)) return "a snake";
	if (strchr("j", r_ptr->d_char)) return "a slime"; /* (most moving j's are slimes) */
	if (strchr("K", r_ptr->d_char)) return "a knight";
	if (strchr("k", r_ptr->d_char)) return "a kobold";
	if (strchr("L", r_ptr->d_char)) return "a lich";
	if (strchr("l", r_ptr->d_char)) return "a lizard";
	if (strchr("N", r_ptr->d_char)) return "a null";
	if (strchr("n", r_ptr->d_char)) return "a naga";
	if (strchr("O", r_ptr->d_char)) return "an ogre";
	if (strchr("o", r_ptr->d_char)) return "orcs"; /* almost never alone */
	if (strchr("P", r_ptr->d_char)) return "a giant";
	if (strchr("pt", r_ptr->d_char)) return "someone";
	if (strchr("R", r_ptr->d_char)) return "an amphibian";
	if (strchr("r", r_ptr->d_char)) return "a rodent";
	if (strchr("s", r_ptr->d_char)) return "a skeleton";
	if (strchr("T", r_ptr->d_char)) return "a troll";
	if (strchr("U", r_ptr->d_char)) return "a devil";
	if (strchr("u&", r_ptr->d_char)) return "a demon";
	if (strchr("V", r_ptr->d_char)) return "a vampire";
	if (strchr("w", r_ptr->d_char)) return "a worm";
	if (strchr("X%", r_ptr->d_char)) return "an elemental";
	if (strchr("x", r_ptr->d_char)) return "a gargoyle";
	if (strchr("Y", r_ptr->d_char)) return "the sound of hooves";
	if (strchr("y", r_ptr->d_char)) return "a fairy";
	if (strchr("Z", r_ptr->d_char)) return "a hound";
	if (strchr("z", r_ptr->d_char)) return "a zombie";
	/* m,$ and mimmics */
	return "an unknown monster";
}

/*
 * Display visible monsters in a window (the monster list)
 */
void display_monlist(void)
{
	int i, max;
	int line = 1, x = 0;
	int cur_x;
	unsigned total_count = 0, disp_count = 0;
	unsigned count_asleep = 0, count_normal = 0;
	unsigned count_snolos = 0, count_nolos = 0;
	bool uniq, messy;
	int effridx;

	byte attr;

	char *m_name;
	char buf[80];
	monster_type *m_ptr;
	monster_race *r_ptr;

	u16b *race_count;
	u16b *race_count_nolos;
	u16b *race_count_snolos;
	u16b *race_count_asleep;
	int unknown = 0;
	int hearmon = 0;
	cptr heartype;


	/* Clear the term if in a subwindow, set x otherwise */
	if (Term != angband_term[0])
	{
		clear_from(0);
		max = Term->hgt - 1;
	}
	else
	{
		x = 13;
		max = Term->hgt - 2;
	}

	/* Allocate the array */
	C_MAKE(race_count, z_info->r_max, u16b);
	C_MAKE(race_count_nolos, z_info->r_max, u16b);
	C_MAKE(race_count_snolos, z_info->r_max, u16b);
	C_MAKE(race_count_asleep, z_info->r_max, u16b);

	uniq = FALSE;
	messy = FALSE;
	if (p_ptr->timed[TMD_IMAGE]) messy = TRUE;

	/* Scan the monster list */
	for (i = 1; i < mon_max; i++)
	{
		bool isnlos = FALSE;
		int randimg = 40;
		m_ptr = &mon_list[i];
		r_ptr = &r_info[m_ptr->r_idx];

		/* Paranoia -- skip "dead" monsters */
		if (!m_ptr->r_idx) continue;
		
		/* monster is temporarily dead */
		if (m_ptr->temp_death) continue;

		/* Only visible monsters */
		if ((!m_ptr->ml) && (!m_ptr->heard)) continue;

		/* ordinary trees aren't really monsters.. */
		if (r_ptr->flags7 & (RF7_NONMONSTER)) continue;
		
		/* monster is disguised as another monster */
		if ((r_ptr->flags2 & (RF2_DISGUISE)) && (m_ptr->disguised))
		{
			r_ptr = &r_info[m_ptr->disguised];
			effridx = m_ptr->disguised;
		}
		else effridx = m_ptr->r_idx;
		
        /* display flags should be correct even when hallucenating */
        if (m_ptr->csleep) m_ptr->display_sleep = TRUE;
        else m_ptr->display_sleep = FALSE;
#ifdef newhallu
		randimg = 200;
#endif
		
		/* don't just use MFLAG_VIEW, because that excludes anything */
        /* outside of light radius even if it's in line of sight */
		if (m_ptr->mflag & (MFLAG_VIEW)) isnlos = TRUE;
		if (projectable(m_ptr->fy, m_ptr->fx, p_ptr->py, p_ptr->px)) isnlos = TRUE;

		/* heard but not seen- can't tell monster race */
		if ((!m_ptr->ml) && (m_ptr->heard))
		{
			/* don't increment for more of the same race (if you show the race) */
			if ((unknown == 1) && (get_hear_race(r_ptr) == heartype)) /* */;
			else unknown++;
			hearmon++;

			/* get the race type for only the first heard unseen monster */
			if (unknown == 1) heartype = get_hear_race(r_ptr);
		}
        /* halucenation messes things up */
        else if ((messy) && (randint(randimg) < 30 + (badluck/2)))
        {
           int mess = randint(4);
           int friedx = randint(682) + 17;
           if (mess == 1)
           {
              race_count_asleep[friedx] += 1;
              count_asleep += 1;
           }
           else if (mess == 2)
           {
              race_count[friedx] += 1;
              count_normal += 1;
           }
           else if (mess == 3)
           {
              race_count_snolos[friedx] += 1;
              count_snolos += 1;
           }
           else if (mess == 4)
           {
              race_count_nolos[friedx] += 1;
              count_nolos += 1;
           }
		   /* sometimes see more monsters than there really are */
		   if (randint(100) < 9) i -= 1;
        }
		/* Bump the count for this race, and the total count */
		/* separate race count for los / no los / asleep */
        else if ((!isnlos) && (m_ptr->csleep))
	    {
            race_count_snolos[effridx]++;
            count_snolos++;
        }
        else if (!isnlos)
	    {
            race_count_nolos[effridx]++;
            count_nolos++;
        }
        else if (m_ptr->csleep)
        {
            race_count_asleep[effridx]++;
            count_asleep++;
        }
        else
        {
            race_count[effridx]++;
            count_normal++;
        }
        if ((r_ptr->flags1 & RF1_UNIQUE) && (!messy)) uniq = TRUE;

		total_count++;
	}

	/* Note no visible monsters */
	if (!total_count)
	{
		/* Clear display and print note */
		c_prt(TERM_SLATE, "You see no monsters.", 0, 0);
		if (Term == angband_term[0])
		    Term_addstr(-1, TERM_WHITE, "  (Press any key to continue.)");

		/* Free up memory */
		FREE(race_count);
		FREE(race_count_asleep);
		FREE(race_count_nolos);
		FREE(race_count_snolos);

		/* Done */
		return;
	}

    /* if (count_asleep) */
	if (((count_asleep) || (count_snolos) || (count_nolos)) && (!messy))
    {
		/* Reset position */
		cur_x = x;

        my_strcpy(buf, " (darker shade means unaware of you, blue means out of line of sight) ", sizeof(buf));

        /* Print and bump line counter */
		c_prt(TERM_L_WHITE, buf, line, cur_x);
		line++;
    }

    if ((uniq) && (!messy))
    {
		/* Reset position */
		cur_x = x;

        my_strcpy(buf, " (uniques in red if in sight, or brown if out of LOS) ", sizeof(buf));

        /* Print and bump line counter */
		c_prt(TERM_L_WHITE, buf, line, cur_x);
		line++;
    }

	/* Go over (line of sight and awake) */
	/* for (i = 1; (i < z_info->r_max) && (line < max); i++) */
	/* sort from highest to lowest */
	for (i = z_info->r_max-1; (i > 0) && (line < max); i--)
	{
		/* Reset position */
		cur_x = x;

        /* no monsters in los who are aware of you */
        if (!count_normal) break;
        
		/* No monsters of this race are visible (and awake) */
		if (!race_count[i]) continue;

		/* Note that these have been displayed */
		disp_count += race_count[i];

		/* Get monster race and name */
		r_ptr = &r_info[i];
		m_name = r_name + r_ptr->name;

		/* Display uniques in a special colour */
		if (r_ptr->flags1 & RF1_UNIQUE)
			attr = TERM_L_RED;
		else
			attr = TERM_WHITE;

		/* Build the monster name */
		if (race_count[i] == 1)
			my_strcpy(buf, m_name, sizeof(buf));
		else
			strnfmt(buf, sizeof(buf), "%s (x%d) ", m_name, race_count[i]);

		/* Display the pict */
		Term_putch(cur_x++, line, r_ptr->x_attr, r_ptr->x_char);
		if (use_bigtile) Term_putch(cur_x++, line, 255, -1);
		Term_putch(cur_x++, line, TERM_WHITE, ' ');

		/* Print and bump line counter */
		c_prt(attr, buf, line, cur_x);
		line++;
	}
        
	/* Go over (line of sight and unaware of the player) */
	/* for (i = 1; (i < z_info->r_max) && (line < max); i++) */
	/* sort from highest to lowest */
	for (i = z_info->r_max-1; (i > 0) && (line < max); i--)
	{
		/* Reset position */
		cur_x = x;

        /* no sleeping (or roaming) monsters */
        if (!count_asleep) break;
        
        /* No monsters of this race are visible and unaware */
		if (!race_count_asleep[i]) continue;

		/* Note that these have been displayed */
		disp_count += race_count_asleep[i];

		/* Get monster race and name */
		r_ptr = &r_info[i];
		m_name = r_name + r_ptr->name;

		/* Display uniques in a special colour */
		if (r_ptr->flags1 & RF1_UNIQUE)
			attr = TERM_RED;
		else
			attr = TERM_L_WHITE;

		/* Build the monster name */
		if (race_count_asleep[i] == 1)
			my_strcpy(buf, m_name, sizeof(buf));
		else
			strnfmt(buf, sizeof(buf), "%s (x%d) ", m_name, race_count_asleep[i]);

		/* Display the pict */
		Term_putch(cur_x++, line, r_ptr->x_attr, r_ptr->x_char);
		if (use_bigtile) Term_putch(cur_x++, line, 255, -1);
		Term_putch(cur_x++, line, TERM_WHITE, ' ');

		/* Print and bump line counter */
		c_prt(attr, buf, line, cur_x);
		line++;
	}

	/* Go over (for not in LOS monsters) */
	/* for (i = 1; (i < z_info->r_max) && (line < max); i++) */
	/* sort from highest to lowest */
	for (i = z_info->r_max-1; (i > 0) && (line < max); i--)
	{
		/* Reset position */
		cur_x = x;

        /* no monsters out of LOS and aware */
        if (!count_nolos) break;
        
		/* No monsters of this race are visible */
		if (!race_count_nolos[i]) continue;

		/* Note that these have been displayed */
		disp_count += race_count_nolos[i];

		/* Get monster race and name */
		r_ptr = &r_info[i];
		m_name = r_name + r_ptr->name;

		/* Display uniques in a special colour */
		if (r_ptr->flags1 & RF1_UNIQUE)
			attr = TERM_L_UMBER;
		else
			attr = TERM_L_BLUE;

		/* Build the monster name */
		if (race_count_nolos[i] == 1)
			my_strcpy(buf, m_name, sizeof(buf));
		else
			strnfmt(buf, sizeof(buf), "%s (x%d) ", m_name, race_count_nolos[i]);

		/* Display the pict */
		Term_putch(cur_x++, line, r_ptr->x_attr, r_ptr->x_char);
		if (use_bigtile) Term_putch(cur_x++, line, 255, -1);
		Term_putch(cur_x++, line, TERM_WHITE, ' ');

		/* Print and bump line counter */
		c_prt(attr, buf, line, cur_x);
		line++;
	}

	/* Go over (for out of LOS and not aware) */
	/* for (i = 1; (i < z_info->r_max) && (line < max); i++) */
	/* sort from highest to lowest */
	for (i = z_info->r_max-1; (i > 0) && (line < max); i--)
	{
		/* Reset position */
		cur_x = x;

        /* no monsters out of LOS and unaware */
        if (!count_snolos) break;

        /* no message */
        
		/* No monsters of this race are visible */
		if (!race_count_snolos[i]) continue;

		/* Note that these have been displayed */
		disp_count += race_count_snolos[i];

		/* Get monster race and name */
		r_ptr = &r_info[i];
		m_name = r_name + r_ptr->name;

		/* Display uniques in a special colour */
		if (r_ptr->flags1 & RF1_UNIQUE)
			attr = TERM_UMBER;
		else
			attr = TERM_BLUE;
		

		/* Build the monster name */
		if (race_count_snolos[i] == 1)
			my_strcpy(buf, m_name, sizeof(buf));
		else
			strnfmt(buf, sizeof(buf), "%s (x%d) ", m_name, race_count_snolos[i]);

		/* Display the pict */
		Term_putch(cur_x++, line, r_ptr->x_attr, r_ptr->x_char);
		if (use_bigtile) Term_putch(cur_x++, line, 255, -1);
		Term_putch(cur_x++, line, TERM_WHITE, ' ');

		/* Print and bump line counter */
		c_prt(attr, buf, line, cur_x);
		line++;
	}

	/* can hear unseen monsters */
	if (unknown)
	{
		if (unknown == 1) strnfmt(buf, sizeof buf, " and you hear %s out of sight.", heartype);
		else strnfmt(buf, sizeof buf, " and you hear %d unknown monsters.", unknown);

		/* don't print "... and X others" for monsters you hear */
		total_count = total_count - hearmon;

        /* Print and bump line counter */
		c_prt(TERM_L_BLUE, buf, line, cur_x);
		line++;
	}

	/* Print "and others" message if we've run out of space */
	if (disp_count != total_count)
	{
		strnfmt(buf, sizeof buf, "  ...and %d others.", total_count - disp_count);
		c_prt(TERM_WHITE, buf, line, x);
	}

	/* Otherwise clear a line at the end, for main-term display */
	else
	{
		prt("", line, x);
	}

	/* Message */
	prt(format("You can see %d monster%s:",
	           total_count, (total_count == 1 ? "" : "s")), 0, 0);

	if (Term == angband_term[0])
		Term_addstr(-1, TERM_WHITE, "  (Press any key to continue.)");

	/* Free the race counters */
	FREE(race_count);
	FREE(race_count_asleep);
	FREE(race_count_nolos);
	FREE(race_count_snolos);
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
 * I am assuming that no monster name is more than 65 characters long,
 * so that "char desc[80];" is sufficiently large for any result, even
 * when the "offscreen" notation is added.
 *
 * Note that the possessive for certain unique monsters will look
 * really silly, as in "Morgoth, King of Darkness's".  We should
 * perhaps add a flag to remove any descriptives in the name.
 *
 * Note that "offscreen" monsters will get a special "(offscreen)"
 * notation in their name if they are visible but offscreen.  This
 * may look silly with possessives, as in "the rat's (offscreen)".
 * Perhaps the "offscreen" descriptor should be abbreviated.
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
 *   0x80 --> Banishment resistance name ("the kobold")
 *   0x88 --> Killing name ("a kobold")
 *   0x22 --> Possessive, genderized if visable ("his") or "its"
 *   0x23 --> Reflexive, genderized if visable ("himself") or "itself"
 */
void monster_desc(char *desc, size_t max, const monster_type *m_ptr, int mode)
{
	cptr res;
	cptr name;
	bool seen, pron;
	int randimg = 40;

	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	
	/* monster disguised as another monster */
	if ((r_ptr->flags2 & (RF2_DISGUISE)) && (m_ptr->disguised))
		r_ptr = &r_info[m_ptr->disguised];

#ifdef newhallu
	randimg = 200;
#endif
	/* Hallucenation */
    if ((p_ptr->timed[TMD_IMAGE]) && (randint(randimg) < 35 + ((badluck+3)/4)))
	{
       r_ptr = &r_info[randint(682) + 17];
	}

	name = (r_name + r_ptr->name);

	/* NPC adventurers */
	if (strstr(name, "high level adventuerer"))
	{
		switch (m_ptr->tinvis)
		{
			case 1: name = "high level warrior";
			case 2: name = "high level ranger";
			case 3: name = "high level priest";
			case 4: name = "high level mage";
			case 5: name = "high level rogue";
			case 6: name = "high level paladin";
			case 7: name = "high level archer";
		}
	}
	/* NPC adventurers */
	if (strstr(name, "mid level adventuerer"))
	{
		switch (m_ptr->tinvis)
		{
			case 1: name = "mid level warrior";
			case 2: name = "mid level ranger";
			case 3: name = "mid level priest";
			case 4: name = "mid level mage";
			case 5: name = "mid level rogue";
			case 6: name = "mid level paladin";
			case 7: name = "mid level archer";
		}
	}
	/* NPC adventurers */
	if (strstr(name, "low level adventuerer"))
	{
		switch (m_ptr->tinvis)
		{
			case 1: name = "low level warrior";
			case 2: name = "low level ranger";
			case 3: name = "low level priest";
			case 4: name = "low level mage";
			case 5: name = "low level rogue";
			case 6: name = "low level paladin";
			case 7: name = "low level archer";
		}
	}

	/* Can we "see" it (forced, or not hidden + visible) */
	seen = ((mode & (0x80)) || (!(mode & (0x40)) && m_ptr->ml));

	/* Sexed Pronouns (seen and forced, or unseen and allowed) */
	pron = ((seen && (mode & (0x20))) || (!seen && (mode & (0x10))));


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
		my_strcpy(desc, res, max);
	}

	/* Handle visible monsters, "reflexive" request */
	else if ((mode & 0x02) && (mode & 0x01))
	{
		/* The monster is visible, so use its gender */
		if (r_ptr->flags1 & (RF1_FEMALE)) my_strcpy(desc, "herself", max);
		else if (r_ptr->flags1 & (RF1_MALE)) my_strcpy(desc, "himself", max);
		else my_strcpy(desc, "itself", max);
	}

	/* Handle all other visible monster requests */
	else
	{
		/* It could be a Unique */
		if ((r_ptr->flags1 & (RF1_UNIQUE)) || (r_ptr->maxpop == 1))
		{
			/* Start with the name (thus nominative and objective) */
			my_strcpy(desc, name, max);
		}
		
		/* pseudo-unique */
		else if (m_ptr->champ)
		{
			/*char champname; */
			my_strcpy(desc, m_ptr->champion_name, max);
			my_strcat(desc, " the ", max);
			my_strcat(desc, name, max);
			/*my_strcpy(desc, champname, max);*/
		}

		/* It could be an indefinite monster */
		else if (mode & 0x08)
		{
			/* XXX Check plurality for "some" */

			/* Indefinite monsters need an indefinite article */
			my_strcpy(desc, is_a_vowel(name[0]) ? "an " : "a ", max);
			my_strcat(desc, name, max);
		}

		/* It could be a normal, definite, monster */
		else
		{
			/* Definite monsters need a definite article */
			my_strcpy(desc, "the ", max);
			my_strcat(desc, name, max);
		}

		/* Handle the Possessive as a special afterthought */
		if (mode & 0x02)
		{
			/* XXX Check for trailing "s" */

			/* Simply append "apostrophe" and "s" */
			my_strcat(desc, "'s", max);
		}

		/* Mention "offscreen" monsters XXX XXX */
		if (!panel_contains(m_ptr->fy, m_ptr->fx))
		{
			/* Append special notation */
			my_strcat(desc, " (offscreen)", max);
		}
	}
}



/*
 * Learn about a monster (by "probing" it)
 */
void lore_do_probe(int m_idx)
{
	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];


	/* Hack -- Memorize some flags, DJA: changed to include spells */
	l_ptr->flags1 = r_ptr->flags1;
	l_ptr->flags2 = r_ptr->flags2;
	l_ptr->flags3 = r_ptr->flags3;
	l_ptr->flags4 = r_ptr->flags4; /* breath / projectiles */
	/* spells */
	if (randint(100) < 34) l_ptr->flags5 = r_ptr->flags5;
	if (randint(100) < 34) l_ptr->flags6 = r_ptr->flags6;

	/* this monster race has been probed (reveals more info in recall) */
	l_ptr->xtra1 = 1;

	/* Update monster recall window */
	if (p_ptr->monster_race_idx == m_ptr->r_idx)
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
	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];


	/* Note the number of things dropped */
	if (num_item > l_ptr->drop_item) l_ptr->drop_item = num_item;
	if (num_gold > l_ptr->drop_gold) l_ptr->drop_gold = num_gold;

	/* Hack -- memorize the good/great flags */
	if (r_ptr->flags1 & (RF1_DROP_GOOD)) l_ptr->flags1 |= (RF1_DROP_GOOD);
	if (r_ptr->flags1 & (RF1_DROP_GREAT)) l_ptr->flags1 |= (RF1_DROP_GREAT);

	/* Update monster recall window */
	if (p_ptr->monster_race_idx == m_ptr->r_idx)
	{
		/* Window stuff */
		p_ptr->window |= (PW_MONSTER);
	}
}


/* a little hook for monsters slowed by water */
static bool monster_is_slowed_by_water(int ridx)
{
	monster_race *r_ptr = &r_info[ridx];
		
	/* nulls are water monsters */
	/* though some are too big to hide in shallow water */
	if (strchr("N", r_ptr->d_char)) return FALSE;
	
	/* Most PASS_DOOR monsters aren't incorporeal, but some are */
	if ((r_ptr->flags2 & (RF2_PASS_DOOR)) &&
		(strchr("GLVW", r_ptr->d_char))) return FALSE;

#if nolongerneeded
	/* This is the temporary piece for until I finish */
	/* getting r_ptr->mrsize coded */
	/* (monsters too big to get slowed by water) */
	if (strchr("DEOPTd", r_ptr->d_char)) return FALSE;
#endif

	/* isn't water monster, doesn't fly, not very big, */
	/* not incorporeal */
	if ((!(r_ptr->flags2 & (RF2_FLY))) && 
		(!(r_ptr->flags7 & (RF7_WATER_HIDE))) &&
		(!(r_ptr->flags7 & (RF7_WATER_ONLY))) &&
		(!(r_ptr->flags2 & (RF2_PASS_WALL))) &&
		(r_ptr->mrsize < 6))
		return TRUE;
		
	return FALSE;
}

/* mode 0: keywords or UNIQUE flag
 * mode 1: UNIQUE flag, psuedo-uniques, or keywords
 *  (tourist celebrity watch spell uses mode 1)
 * mode 2: escorts only
 * mode 3: escorts or keywords (unused)
 * mode 4: escorts, keywords, or UNIQUE flag
 */
bool is_a_leader(const monster_type *m_ptr, int mode)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	cptr dname = (r_name + r_ptr->name);
	bool uniqflg = FALSE, psuniq = FALSE;
	bool escflg = FALSE, keywords = FALSE;
	/* define what we're looking for in a leader */
	switch (mode)
	{
		case 0: { keywords = TRUE; uniqflg = TRUE; break; }
		case 1: { uniqflg = TRUE; psuniq = TRUE; keywords = TRUE; break; }
		case 2: { escflg = TRUE; break; }
		case 3: { escflg = TRUE; keywords = TRUE; break; }
		case 4: { uniqflg = TRUE; escflg = TRUE; keywords = TRUE; break; }
	}
	
	if (keywords)
	{
		/* check for leader keywords */
		if ((strstr(dname, "lord")) || (strstr(dname, "chieftain")) ||
			/* the scruffy-loo(king) hobbit is not a leader... */
			((strstr(dname, "king")) && (!strstr(dname, "looking"))) || 
            (strstr(dname, "captain")))
			return TRUE;
	}
	/* pseudo-uniques */
	if ((psuniq) && (m_ptr->champ)) return TRUE;
	/* real uniques */
	if (((r_ptr->flags1 & (RF1_UNIQUE)) || (r_ptr->maxpop == 1)) && 
		(uniqflg)) return TRUE;
	
    /* Escorts make you a leader */	
	if ((escflg) && ((r_ptr->flags1 & RF1_ESCORT) || 
		(r_ptr->flags1 & RF1_ESCORTS) || (r_ptr->flags2 & RF2_ESCORT1)))
		return TRUE;
		
	return FALSE;
}

/* find out if a given monster has a particular melee attack
 * mode 1 == thieves
 * mode 2 == poison
 * mode 3 == grabbing
 * (may add other modes as I need them)
 */
bool check_melee(const monster_type *m_ptr, int mode)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	int ap_cnt;

	/* Scan through all blows */
	for (ap_cnt = 0; ap_cnt < MONSTER_BLOW_MAX; ap_cnt++)
	{
		int effect = r_ptr->blow[ap_cnt].effect;
		/* should thieves include EAT_FOOD? */
		if ((mode == 1) && ((effect == RBE_EAT_GOLD) || (effect == RBE_EAT_ITEM)))
			return TRUE;

		/* poisoners */
		if ((mode == 2) && (effect == RBE_POISON)) return TRUE;
		
		/* grabbers */
		if ((mode == 2) && (effect == RBE_BHOLD)) return TRUE;
	}

	/*  */
	return FALSE;
}

/* chance to recognise illusions (and monsters disguising as other monsters) */
/* (unlikely each time, but there's a chance every turn that the monster is in easy view) */
static void discern_illusion(m_idx)
{
	int diffc, discern;
	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];
	
	/* not an illusion or disguised monster */
	if ((!m_ptr->extra2) && (!m_ptr->disguised)) return;
	/* can't recognise illusions when you're hallucenating or blind */
	if ((m_ptr->extra2 == 5) || (p_ptr->timed[TMD_IMAGE]) || 
		(p_ptr->timed[TMD_BLIND])) return; 

	/* saving throw + (alertness*2) + luck mods */
	discern = adj_wis_sav[p_ptr->stat_ind[A_WIS]] + (p_ptr->skills[SKILL_FOS]*2) + goodluck - badluck;

	/* true sight recognises illusions */
	if ((p_ptr->timed[TMD_TSIGHT]) && (m_ptr->extra2)) discern = discern * 20;
	/* but doesn't help quite as much against disguises */
	else if ((p_ptr->timed[TMD_TSIGHT]) || (p_ptr->timed[TMD_2ND_THOUGHT])) 
		discern = discern * 3;

	/* tourist awareness of thieves spell */
	if ((p_ptr->timed[TMD_PROT_THIEF]) && (check_melee(m_ptr, 1))) discern = discern * 3;

	/* you've recognised this same monster's disguise before */
	if ((m_ptr->meet >= 5) && (m_ptr->meet <= 10) && (!m_ptr->extra2)) 
		discern = discern * 3;

	/* smaller bonuses */
	else if ((l_ptr->flags2 & RF2_DISGUISE) && (!m_ptr->extra2)) discern = discern * 8 / 7;
	/* OPP_SILV helps against illusions but not as much against disguises (OPP_SILV gives resist_charm among other things) */
	if ((p_ptr->timed[TMD_OPP_SILV]) && (m_ptr->extra2)) discern = discern * 5 / 4;
	else if (p_ptr->resist_charm) discern = discern * 10 / 9;
	/* SKILLFULL helps against disguises but not illusions */
	if ((p_ptr->timed[TMD_SKILLFUL]) && (!m_ptr->extra2)) discern = discern * 8 / 7;
	if (p_ptr->timed[TMD_BLESSED]) discern = discern * 8 / 7;
	/* these only help against illusions */
	if ((p_ptr->timed[TMD_CLEAR_MIND]) && (m_ptr->extra2)) discern = discern * 8 / 7;
	if ((p_ptr->timed[TMD_SAFET_GOGGLES]) && (m_ptr->extra2)) discern = discern * 11 / 10;
	
	/* penalties */
	if (p_ptr->timed[TMD_FATIGUE]) discern = discern * 4 / 5;
	if (p_ptr->timed[TMD_BEAR_HOLD]) discern = discern * 4 / 5;
	if (p_ptr->timed[TMD_CONFUSED]) discern = discern/2;
	if (p_ptr->timed[TMD_CURSE]) discern = discern/2;
	if ((p_ptr->timed[TMD_AFRAID]) || (p_ptr->timed[TMD_TERROR])) discern = discern/2;
	if ((p_ptr->timed[TMD_CHARM]) || (p_ptr->timed[TMD_FRENZY]) ||
		(p_ptr->timed[TMD_SHERO])) discern = discern/2;

#ifdef roomrunes
	/* pink elephant room rune */
	if ((p_ptr->roomeffect == 11) || (room_runes(m_ptr->fy, m_ptr->fx) == 11)) 
		discern = discern/4;
#endif

	/* different types of illusions may be easier or harder to discern */
	if (m_ptr->extra2 == 2) diffc = 10000;
	else if (m_ptr->extra2 == 1) diffc = 9000;
	else if (m_ptr->extra2 == 4) diffc = 7500;
	else diffc = 6000;
	if ((!m_ptr->extra2) && (r_ptr->flags2 & (RF2_DISGUISE))) diffc = r_ptr->stealth * 1500;
	else diffc += r_ptr->stealth * 20;

	/* better chance the first time you see it */
	if (!m_ptr->meet) diffc -= 200;

	/* roll to discern */
	if (randint(diffc) < discern)
	{
		char dr_name[80];
		char m_name[80];

		/* dissapate the illusion */
		if (m_ptr->extra2)
		{
   			/* Extract monster name  */
			monster_desc(dr_name, sizeof(dr_name), m_ptr, 0x04);
			
			/* 1XP for discerning a semi-real illusion */
			if (m_ptr->extra2 <= 2) gain_exp(1);

			if (m_ptr->extra2 == 5) m_ptr->extra2 = 8;
			else m_ptr->extra2 = 9;
			msg_format("You discern that %s is an illusion.", dr_name);
		}
		/* or remove the disguise */
		if ((r_ptr->flags2 & (RF2_DISGUISE)) && (m_ptr->disguised))
		{
			/* Extract monster name -before removing disguise */
			monster_desc(dr_name, sizeof(dr_name), m_ptr, 0x04);

			m_ptr->disguised = 0;
			m_ptr->ml = TRUE;
			l_ptr->flags2 |= (RF2_DISGUISE);

			/* chance to recognise more than the disguise */
            /* (true sight doesn't help as much with this) */			
			if (p_ptr->timed[TMD_TSIGHT]) discern = discern / 10;
			if (randint(diffc) < discern * 20)
			{
				/* If it's always evil then the monster lore tells you that it's evil */
				if (r_ptr->flags3 & (RF3_EVIL)) l_ptr->flags3 |= (RF3_EVIL);
				else if (m_ptr->evil)
				{
					/* can tell (later) that the monster is evil */
					m_ptr->meet = 2;
              
					if (r_ptr->flags2 & (RF2_S_EVIL2)) l_ptr->flags2 |= (RF2_S_EVIL2);
					else if (r_ptr->flags2 & (RF2_S_EVIL1)) l_ptr->flags2 |= (RF2_S_EVIL1);
				}
				/* can tell (later) that the monster is not evil */
				else m_ptr->meet = 3;
			}
			/* scale up to remember that you've recognised this monster's disguise before */
			/* (later it'll be easier to recognise if he puts on a new disguise) */
			m_ptr->meet += 5;

			/* and after removing disguise */
			monster_desc(m_name, sizeof(m_name), m_ptr, 0);
			
			msg_format("You recognise that %s was really %s in disguise!", dr_name, m_name);
			/* stop acting like the monster he's disguised as */
			/* (see end of disguise_mimmics() */
			if (m_ptr->mspeed < r_ptr->speed)
			{
				int die = rand_int(100);
				m_ptr->mspeed = r_ptr->speed;
				/* racial variety in speed */
				if ((die < 15) && (!(r_ptr->flags1 & (RF1_UNIQUE)))) 
					m_ptr->mspeed += 2;
				else if ((die < 30) && (!(r_ptr->flags1 & (RF1_UNIQUE))))
					m_ptr->mspeed -= 2;
			}
			update_mon(m_idx, 0);
		}
	}
}

/* check to see if this monster is detected by the PC's racial ESP
 * and return the range at which it can be detected.
 */
int check_rtelep(int m_idx)
{
	int espcheck = 0;
	int i, rtr = 0; /* Rtelep range */
	bool thranduil = FALSE;
	bool sting = FALSE;
	bool ratagast = FALSE;
    /* get the monster and its race */
	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	cptr dname = (r_name + r_ptr->name);
	
	/* first sight supresses ESP */
    if (p_ptr->timed[TMD_2ND_THOUGHT]) return 0;

	/* Check what we can sense */
	/* Scan the equipment */
	for (i = INVEN_WIELD; i < END_EQUIPMENT; i++)
	{
		object_type *o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;
		
		/* 70 == detect everything at short range (8 may be too short...) */
		if ((o_ptr->esprace == 70) && (!rtr)) rtr = 8;
		else if (o_ptr->esprace == 70) rtr += 3;
        
		/* special for Thranduil (senses several monster types) */
		if (o_ptr->esprace == 75) thranduil = TRUE;
		/* special for Sting */
		if (o_ptr->esprace == 15) sting = TRUE;
		/* special for Ratagast */
		if (o_ptr->esprace == 71) ratagast = TRUE;
		/* that's all for this time through */
	}

	/* traditional telepathy */
	if (p_ptr->telepathy)
	{
		monster_lore *l_ptr = &l_list[m_ptr->r_idx];
		if (r_ptr->flags2 & (RF2_EMPTY_MIND))
		{
			l_ptr->flags2 |= (RF2_EMPTY_MIND);
		}
		else if (r_ptr->flags2 & (RF2_WEIRD_MIND))
		{
			int dchance = 3 + ((goodluck+1)/2);
			if (r_ptr->flags2 & (RF2_SMART)) dchance += 2;
			if (r_ptr->flags2 & (RF2_STUPID)) dchance -= 1;
			/* One in ten individuals are easy to sense with ESP */
			if ((m_idx % 10) == 5) dchance += 20;
				
			if (rand_int(100) < dchance)
			{
				if (los(p_ptr->py, p_ptr->px, m_ptr->fy, m_ptr->fx)) rtr = 19;
				else if (!rtr) rtr = 16;
				else rtr += 2;
				/* Memorize mental flags */
				l_ptr->flags2 |= (RF2_WEIRD_MIND);
				if (r_ptr->flags2 & (RF2_SMART)) l_ptr->flags2 |= (RF2_SMART);
				if (r_ptr->flags2 & (RF2_STUPID)) l_ptr->flags2 |= (RF2_STUPID);
			}
		}
		else /* normal mind */
		{
			if (los(p_ptr->py, p_ptr->px, m_ptr->fy, m_ptr->fx)) rtr = 20;
            else if (!rtr) rtr = 18;
			else rtr += 2;
		}
	}
	
	/* ESP by race */
    if (r_ptr->flags3 & (RF3_ORC)) espcheck = 1;
	if (r_ptr->flags3 & (RF3_TROLL)) espcheck = 2;
	if (r_ptr->flags3 & (RF3_GIANT)) espcheck = 3;
	if (r_ptr->flags3 & (RF3_UNDEAD)) espcheck = 4;
	if (r_ptr->flags3 & (RF3_DRAGON)) espcheck = 5;
	if (r_ptr->flags3 & (RF3_DEMON)) espcheck = 6;
	if (r_ptr->flags3 & (RF3_BUG)) espcheck = 8;
	if (r_ptr->flags3 & (RF3_SILVER)) espcheck = 9;
	
	/* light fairies, not all creatures of light */
	if ((strchr("y", r_ptr->d_char)) || (strstr(dname, "unicorn")) ||
		(strstr(dname, "eirrinel")))
		espcheck = 7;
	
	/* 10 senses most but not all animals */
	/* (doesn't include 'H' animals or unicorns, probably a few others) */
	if ((r_ptr->flags3 & (RF3_ANIMAL)) && 
		((strchr("ABCJRZbflqr", r_ptr->d_char)) ||
		(strstr(dname, "warhorse"))))
		espcheck = 10;
		
	/* constructs */
	if (strchr("gm", r_ptr->d_char)) espcheck = 11;
	
	/* dwarves (maybe we should have a dwarf flag..) */
	if ((strstr(dname, "dwarf")) || (strstr(dname, "grag")) ||
		(strstr(dname, "Mim")) || (strstr(dname, "Dwarf")) ||
		(strstr(dname, "Nibelung")) || 
		/* ghosts of dwarvish supersition get sensed along with dwarves  */
		(strstr(dname, "ing dark"))) espcheck = 12;
		
	/* dark elves and other servants of Achrya */	
	if (((strchr("@", r_ptr->d_char)) &&
	/* exclude doppleganger, but include Achrya and her servants */
		(!strstr(dname, "dopple"))) || 
		(strstr(dname, "achrya")) || (strstr(dname, "Achrya")) ||
		(strstr(dname, "drider"))) espcheck = 13;
	
	/** thranduil **/
    /* orcs, bugs, (most) animals, tree monsters, centaurs, unicorns, */
	/* light fairies, also picks up druids and forest trolls */
    if ((thranduil) && ((espcheck == 1) || (espcheck == 8) ||
		(espcheck == 10) || (strchr("EYy", r_ptr->d_char)) ||
		(strstr(dname, "forest")) || (strstr(dname, "druid"))))
	{ 
		if (rtr < 14) rtr = 14;
		else rtr += 3;
	}
		
	/* Sting detects orcs and spiders, but not as long a range */
    if ((sting) && ((r_ptr->flags3 & (RF3_ORC)) ||
		(strchr("x", r_ptr->d_char))))
	{
		if (rtr < 12) rtr = 12;
		else rtr += 3;
	}
	
	/* Ratagast senses all evil wizards */
    /* (..this is insane..  ..for 1 artifact??..) */
    if ((ratagast) && ((strstr(dname, "Saruman")) ||
		((strstr(dname, "mage")) && (m_ptr->evil)) || 
		(strstr(dname, "the Blue")) || (strstr(dname, "necromancer")) ||
		(strstr(dname, "demonologist")) ||
		(strstr(dname, "Dunlending")) || (strstr(dname, "witch")) ||
		(strstr(dname, "Witch")) || (strstr(dname, "Mouth of")) ||
		((strstr(dname, "hag")) && (!strchr("o", r_ptr->d_char))) || /* exclude Shagrat from hags */
		((r_ptr->d_attr == TERM_RED) && (r_ptr->flags1 & (RF1_UNIQUE)) && (strchr("W", r_ptr->d_char))) ||
		(strstr(dname, "priest of")) || (strstr(dname, "nightmare doc")) ||
		(strstr(dname, "sorcerer")) || (strstr(dname, "shaman"))))
    {
		if (rtr < 14) rtr = 14;
		else rtr += 3;
	}
	
	/* tourist celebrity watch spell */
	if ((p_ptr->timed[TMD_CELEB_WATCH]) && (is_a_leader(m_ptr, 1)))
    {
		if (rtr < 15) rtr = 15;
		else rtr += 3;
	}
		
	/* surely not sensed */
    if ((!espcheck) && (!rtr)) return 0;

	/* 2nd time through */
	for (i = INVEN_WIELD; i < END_EQUIPMENT; i++)
	{
		object_type *o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;
		
		/* check items with racial ESP */
		if ((o_ptr->esprace == espcheck) && (rtr < 15)) rtr = 15;
		else if (o_ptr->esprace == espcheck) rtr += 2;
	}
	
	return rtr;
}


/*
 * update_mon() helper:
 * Check PC alertness vs monster stealth (and other factors)
 * to decide if the PC notices the monster.
 *
 * mode == 0 normal useage called by update_mon()
 * mode == 1 called by detect monsters spell
 */
bool alertness_check(monster_type *m_ptr, int mode, bool darkvs)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	int d = m_ptr->cdis;
	int mstealth = r_ptr->stealth;
	int palert = p_ptr->palert;
	int roomeff = 0;
	int detect_alert = p_ptr->lev/2 + 20;
	bool easy = TRUE;
	bool isthief = check_melee(m_ptr, 1);
	/* chance realm classes have more influence from luck */
	if (cp_ptr->spell_book == TV_LUCK_BOOK) 
		detect_alert = p_ptr->lev/2 + 18 + goodluck - ((badluck+1)/2);
		
	/* if monster is disguised as another monster, he tries to act like the other monster */
	if ((r_ptr->flags2 & (RF2_DISGUISE)) && (m_ptr->disguised))
	{
		monster_race *dr_ptr = &r_info[m_ptr->disguised];
		mstealth = dr_ptr->stealth;
	}

	/* if called from a detection spell, it shouldn't depend as much on alertness */
	if ((mode == 1) && (palert < detect_alert)) palert = (detect_alert + palert)/2;
	/* negative stealth applies only to hearing monsters */
	if (mstealth < 0) mstealth = 0;
	
	/* monsters size 6 or larger have no stealth if they are in your line of sight */
	/* (and visible) */
	if ((r_ptr->mrsize >= 6) && (player_has_los_bold(m_ptr->fy, m_ptr->fx))) mstealth = 0;
#ifdef roomrunes
	roomeff = room_runes(m_ptr->fy, m_ptr->fx);
	if ((roomeff == 1) && (!detect_alert)) mstealth = 0; /* room_runes 1 is war -no stealth */
	/* room_runes 16 is silence -added stealth (only for monsters which are already stealthy) */
	if ((roomeff == 16) && (!detect_alert) && (mstealth > 2) && (mstealth < 6)) 
		mstealth += 1;
#endif

	/* most of the stealth of a CHAR_MULTI monster is their disguise */
	if ((r_ptr->flags1 & (RF1_CHAR_MULTI)) && (m_ptr->disguised))
	{
		/* Rchaos helps recognise mimmics */
		if ((mstealth < 5) && (!p_ptr->resist_chaos)) mstealth = 5;
		else if (mstealth < 2) mstealth += 2;
	}
	/* WATER_HIDE monsters get stealth bonus when in water */
	if (((r_ptr->flags7 & (RF7_WATER_HIDE)) || (r_ptr->flags7 & (RF7_WATER_ONLY))) &&
		(cave_feat[m_ptr->fy][m_ptr->fx] == FEAT_WATER))
	{
		if (mstealth < 2) mstealth += 3;
		else if (mstealth < 5) mstealth = 5;
		else if (mstealth == 4) mstealth += randint(2);
	}
	/* WATER_ONLY monsters can't hide out of water */
	else if ((r_ptr->flags7 & (RF7_WATER_ONLY)) && (mstealth > 1))
	{
		if (mstealth < 5) mstealth = 1;
		else mstealth -= 3;
	}
	/* earth elementals get stealth bonus when in a wall */
	if ((strchr("X%", r_ptr->d_char)) && (r_ptr->flags2 & (RF2_PASS_WALL)) &&
		(!cave_floor_bold(m_ptr->fy, m_ptr->fx)))
	{
		if (mstealth < 3) mstealth = 2 + randint(2);
		else if (mstealth < 4)  mstealth += randint(2);
	}
	/* some monsters hide better in their natural habitat (elementals, fairies, and animals) */
	if ((r_ptr->flags3 & RF3_ANIMAL) || (strchr("X%iy", r_ptr->d_char)))
	{
		if ((r_ptr->flags7 & (RF7_CFOREST)) && (p_ptr->theme == 1)) mstealth += 1;
		if ((r_ptr->flags7 & (RF7_FFOREST)) && (p_ptr->theme == 2)) mstealth += 1;
		if ((r_ptr->flags7 & (RF7_SWAMP)) && (p_ptr->theme == 9)) mstealth += 1;
	}

	/* scale stealth for monsters that scale with depth */
	if ((r_ptr->flags3 & (RF3_SCALE)) && (mstealth >= 1))
	{
		if ((mstealth > 1) && (p_ptr->depth >= r_ptr->level + 50)) mstealth += randint(2);
		else if (p_ptr->depth >= r_ptr->level + 25) mstealth += rand_int(3);
		else if (p_ptr->depth >= r_ptr->level + 10) mstealth += rand_int(2);
	}

	/* tourist awareness of thieves spell */
	if ((p_ptr->timed[TMD_PROT_THIEF]) && (isthief)) mstealth = 1;

	/* cap & scale up */
	if (mstealth > 6) mstealth = 6;
	mstealth = (mstealth * 11) + 1; /* scale is now 1 to 67 */
	/* luck factor */
	if (goodluck > 5) mstealth -= goodluck/2;
	else if (goodluck > 3) mstealth -= 1;
	if (badluck > 3) mstealth += badluck/2;
        
	/* distance factor */
	if ((r_ptr->stealth > 1) && (d < 15)) mstealth += d * 2;
	if ((d > 14) && (r_ptr->stealth > 2)) mstealth += (r_ptr->stealth*10);
	else if ((d > 14) && (r_ptr->stealth == 2)) mstealth += 28;
	else if (d > 14) mstealth += 16;
	else if ((d > 3) && (r_ptr->stealth > 3)) mstealth += (d-3) * (r_ptr->stealth - 2);
	else if (d > 3) mstealth += (d-3);

	if ((m_ptr->stunned) || (m_ptr->confused) || (m_ptr->charmed) || (!p_ptr->depth)) 
		mstealth -= 16;
	
	/* Monster gives off light */
	if (r_ptr->flags2 & (RF2_MONGLOW)) mstealth -= 12;
                
	/* monster hasn't been noticed yet (it is still hiding) */
	/* darkvision makes it hard to hide in the shadows */
	/* this helps dwarves who have darkvision but horrible alertness */
	if ((!m_ptr->monseen) && (r_ptr->stealth > 2) && (!darkvs)) mstealth += 20;
	else if ((!m_ptr->monseen) && (r_ptr->stealth > 2)) mstealth += 9;

	/* much easier to notice if you've noticed it before */
	if ((m_ptr->monseen > 3) && (d < 5) && (!m_ptr->monfear)) mstealth -= m_ptr->monseen * 15;
	else if ((m_ptr->monseen > 0) && (d < 7) && (!m_ptr->monfear))
	{
		if (m_ptr->monseen > 2) mstealth -= 40;
		else mstealth -= m_ptr->monseen * 14;
	}
	else if (m_ptr->monseen > 2) mstealth -= m_ptr->monseen * 4;
	else if ((m_ptr->monseen > 0) || (m_ptr->heard)) mstealth -= 12;
        
	/* sleeping monsters can't actively hide (unless they are disguised) */
	if ((!((r_ptr->flags1 & (RF1_CHAR_MULTI)) && (m_ptr->disguised))) &&
		((m_ptr->csleep) && (!m_ptr->roaming))) mstealth -= 12;

	/* NEVER_MOVE monsters can't try to hide once they've been noticed */
	if ((m_ptr->monseen > 0) && (r_ptr->flags1 & (RF1_NEVER_MOVE))) mstealth -= 24;

	/* Monsters with 0 stealth never hide unless disguised */
 	if ((r_ptr->stealth < 1) && (!m_ptr->disguised)) mstealth = 1;

	/* prevent randint(negative mstealth) because that makes it crash */
	if (mstealth < 1) mstealth = 1;

	/* examples which assume monseen is 0 and luck is 0: */
	/* monster of stealth 1 at a distance of 8 now has mstealth of 16 */
	/* monster of stealth 1 at a distance of 15 now has mstealth of 27 */
	/* monster of stealth 3 at a distance of 15 now has mstealth of 71 */
	/* monster of stealth 5 at a distance of 15 now has mstealth of 113 */
	/* monster of stealth 2 at a distance of 9 now has mstealth of 54 */
	/* monster of stealth 3 at a distance of 7 now has mstealth of 59 */
	/* monster of stealth 4 at a distance of 6 now has mstealth of 70 */
	/* monster of stealth 5 at a distance of 5 now has mstealth of 79 */
	/* monster of stealth 6 at a distance of 4 now has mstealth of 86 */
	/* monster of stealth 5 at a distance of 1 now has mstealth of 53 */
	/* monster of stealth 6 at a distance of 8 now has mstealth of 110 */
	/* does the PC notice the monster? */
	if ((d > 2) || ((m_ptr->disguised) && (m_ptr->monseen < 2)))
	{
		/* give monsters a few chances to avoid being noticed */
		if ((mstealth > (palert + 50)) && (m_ptr->monseen < 1)) easy = FALSE;
		if ((mstealth > (palert * 2)) && (randint(mstealth) > palert + 4)) easy = FALSE;
		if (randint(mstealth) > palert) easy = FALSE;
		if (randint(mstealth) > palert + rp_ptr->r_fos) easy = FALSE;
		if ((palert > mstealth) && (r_ptr->stealth > 1) && 
			(randint(palert * 20) < (mstealth/2 + d + badluck/2 - goodluck/2))) easy = FALSE;
	}
	else if (d == 2)
	{
		/* only one chance to escape notice when very close */
		if (randint(mstealth) > palert) easy = FALSE;
	}
	/* d == 1: always notice if adjacent and not disguised */
        
	return easy;
}

/*
 * update_mon() helper:
 * Check PC alertness (and other factors) to decide if the PC
 * notices signs of an invisible monster.
 * (easy is always false when this function is called)
 */
bool notice_invisible_check(monster_type *m_ptr)
{
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	bool easy = FALSE;
	int d = m_ptr->cdis;
	int mstealth = r_ptr->stealth;
    int roomeff = 0;

#ifdef roomrunes
	roomeff = room_runes(m_ptr->fy, m_ptr->fx);
	/* room_runes 16 is silence -added stealth (only for monsters which are already stealthy) */
	if ((roomeff == 16) && (mstealth > 2) && (mstealth < 6)) 
		mstealth += 1;
#endif
	
	/* stealth + distance + 48 for invisibility */
	mstealth = 48 + (mstealth * 5) + (d * 2);
    /* luck factor */
	if (goodluck > 4) mstealth -= (goodluck/4 + randint(goodluck/2));
	if (badluck > 4) mstealth += (badluck/4 + rand_int(badluck/3));
	/* Ghosts are harder to notice */
	if ((r_ptr->flags3 & (RF3_UNDEAD)) || (r_ptr->flags3 & (RF3_SILVER)))
		mstealth += 18 + badluck/2;
		
	/* much easier to notice an invisible monster which is giving off light */
    if (r_ptr->flags2 & (RF2_MONGLOW)) mstealth -= 12;

	if ((d > 3) && (r_ptr->stealth > 3)) mstealth += (d-3) * 2;
	else if (d > 3) mstealth += (d-3);
	else if (d < 2) mstealth -= 10;
	if ((m_ptr->monseen > 0) && (r_ptr->flags1 & (RF1_NEVER_MOVE))) mstealth -= (5 + m_ptr->monseen);
	if (m_ptr->monseen > 0) mstealth -= m_ptr->monseen;

	/* minimum stealth for invisibile monsters */
	if (mstealth < 30) mstealth = 30;

	/* impossible to notice invisible monsters further than 6 spaces away */
	if ((d < 7) && (r_ptr->stealth < 5))
	{
		if (randint(p_ptr->palert) > mstealth)
		{
			easy = TRUE;
			msg_format("You notice the signs of an invisible monster!");
		}
	}
	return easy;
}


/*
 * This function updates the monster record of the given monster
 *
 * This involves extracting the distance to the player (if requested),
 * and then checking for visibility (natural, stealth, see-invis,
 * telepathy), updating the monster visibility flag, redrawing (or
 * erasing) the monster when its visibility changes, and taking note
 * of any interesting monster flags (cold-blooded, invisible, etc).
 *
 * Note the new "mflag" field which encodes several monster state flags,
 * including "view" for when the monster is currently in line of sight,
 * and "mark" for when the monster is currently visible via detection.
 *
 * The only monster fields that are changed here are "cdis" (the
 * distance from the player), "ml" (visible to the player), and
 * "mflag" (to maintain the "MFLAG_VIEW" flag).
 *
 * Note the special "update_monsters()" function which can be used to
 * call this function once for every monster.
 *
 * Note the "full" flag which requests that the "cdis" field be updated,
 * this is only needed when the monster (or the player) has moved.
 *
 * full parameter is now an int: full == 3 means the monster has just
 * moved. ..because there are certain ways which even NEVER_MOVE
 * monsters can be moved (like teleother).
 *
 * Every time a monster moves, we must call this function for that
 * monster, and update the distance, and the visibility.  Every time
 * the player moves, we must call this function for every monster, and
 * update the distance, and the visibility.  Whenever the player "state"
 * changes in certain ways ("blindness", "telepathy",
 * and "see invisible"), we must call this function for every monster,
 * and update the visibility.
 *
 * Routines that change the "illumination" of a grid must also call this
 * function for any monster in that grid, since the "visibility" of some
 * monsters may be based on the illumination of their grid.
 *
 * Note that this function is called once per monster every time the
 * player moves.  When the player is running, this function is one
 * of the primary bottlenecks, along with "update_view()" and the
 * "process_monsters()" code, so efficiency is important.
 *
 * Note the optimized "inline" version of the "distance()" function.
 *
 * A monster is "visible" to the player if (1) it has been detected
 * by the player, (2) it is close to the player and the player has
 * telepathy, or (3) it is close to the player, and in line of sight
 * of the player, and it is "illuminated" by some combination of
 * torch light, or permanent light (invisible monsters
 * are only affected by "light" if the player can see invisible).
 *
 * Stealthy monsters who are visible and in line of sight can still 
 * escape notice.
 *
 * Monsters which are not on the current panel may be "visible" to
 * the player, and their descriptions will include an "offscreen"
 * reference.  Currently, offscreen monsters cannot be targetted
 * or viewed directly, but old targets will remain set.  XXX XXX
 *
 * The player can choose to be disturbed by several things, including
 * "disturb_move" (monster which is viewable moves in some way), and
 * "disturb_near" (monster which is "easily" viewable moves in some
 * way).  Note that "moves" includes "appears" and "disappears".
 */
void update_mon(int m_idx, int full)
{
	bool do_invisible = FALSE;
	bool darkvs, msilent, lore, nevermove, easybut = FALSE;
	bool alreadyseen = FALSE;
	int tempinv, hearcheck, discernmod, hearstealth;
    int d, rtelep = 0;
	u32b f4, f5, f6;

	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	/* Current location */
	int fy = m_ptr->fy;
	int fx = m_ptr->fx;

	/* Seen at all */
	bool flag = FALSE;

	/* Seen by vision (in LOS) */
	bool easy = FALSE;

	int py = p_ptr->py;
	int px = p_ptr->px;
	cptr mon_text = (r_text + r_ptr->text);

	/* monster is temporarily dead */
	if (m_ptr->temp_death) return;
	
	/* racial ESP */
	rtelep = (check_rtelep(m_idx));
	
	/* temporary invisibility */
	tempinv = m_ptr->tinvis;
	/* statues use m_ptr->tinvis for something else */
	if ((r_ptr->flags7 & (RF7_NONMONSTER)) && (r_ptr->flags3 & (RF3_NON_LIVING))) 
		tempinv = 0;
	/* remember where the trees are in the town */
	if ((r_ptr->flags7 & (RF7_NONMONSTER)) && (r_ptr->flags7 & (RF7_BLOCK_LOS)) &&
		(!p_ptr->depth)) m_ptr->ml = TRUE;
#if shouldntneedthis
	/* paranoia: make sure BLOCK_LOS monsters have CAVE_WALL */
	if ((r_ptr->flags7 & (RF7_BLOCK_LOS)) && (!(cave_info[fy][fx] & (CAVE_WALL))))
	{
		cave_info[fy][fx] |= (CAVE_WALL);
	}
#endif

	/* Compute distance */
	if (full)
	{
		/* Distance components */
		int dy = (py > fy) ? (py - fy) : (fy - py);
		int dx = (px > fx) ? (px - fx) : (fx - px);

		/* Approximate distance */
		d = (dy > dx) ? (dy + (dx>>1)) : (dx + (dy>>1));

		/* Restrict distance */
		if (d > 255) d = 255;

		/* Save the distance */
		m_ptr->cdis = d;
	}

	/* Extract distance */
	else
	{
		/* Extract the distance */
		d = m_ptr->cdis;
	}

	/* paranoia: prevent non-adjacent monsters holding the player */
	if ((p_ptr->timed[TMD_BEAR_HOLD]) && (p_ptr->held_m_idx == m_idx) && (m_ptr->cdis > 2))
	{
		p_ptr->held_m_idx = 0;
		msg_print("You pull free.");
		clear_timed(TMD_BEAR_HOLD);
	}

	/* Extract the racial spell flags to check for certain things */
	f4 = r_ptr->flags4;
	f5 = r_ptr->flags5;
	f6 = r_ptr->flags6;

	/* !monster detection detects all monsters */
	/* 23 will probably be standard detection distance in the future */
	if ((p_ptr->timed[TMD_MDETECTION]) && (m_ptr->cdis <= 23)) flag = TRUE;

	/* Detected */
	if (m_ptr->mflag & (MFLAG_MARK)) flag = TRUE;


	/* Nearby */
	if (d <= MAX_SIGHT)
	{
		/* rtelep covers all kinds of ESP (value of rtelep = range of ESP) */
		/* (we have already checked if this monster is sensed) */
		if (d <= rtelep)
		{
			/* Detectable */
			flag = TRUE;
					
			/* monster moved into esp range */
			if ((!m_ptr->ml) && (disturb_espmove)) disturb(1, 0);
		}

		/* Normal line of sight, and not blind */
		if (player_has_los_bold(fy, fx) && !p_ptr->timed[TMD_BLIND])
		{
			bool pccansee = player_can_see_bold(fy, fx);
			/* large monsters in a lit space in LOS */
			if ((r_ptr->mrsize >= 6) && (cave_info[fy][fx] & (CAVE_GLOW))) pccansee = TRUE;
			/* large monsters now only need to be next to a lit space to be illuminated */
			if ((r_ptr->mrsize >= 6) && (next_to(fy, fx, 2, 0)) && (r_ptr->stealth < 3))
				pccansee = TRUE;
				
			/* Use "illumination" */
			if (pccansee)
			{
				/* Handle "invisible" monsters */
				if ((r_ptr->flags2 & (RF2_INVISIBLE)) || (tempinv))
				{
					/* Take note */
					do_invisible = TRUE;

					/* See invisible */
					if (p_ptr->see_inv) easy = TRUE;
				}
				/* Handle "normal" monsters */
				else
				{
					/* Easy to see */
					easy = TRUE;
				}
			}
			/* in line of sight & not blind, but out of torchlight range */
			/* MONGLOW monsters give off their own light */
			/* (but not enough to illuminate any square but their own) */
			/* One of the fountain statues also gives off light */
			else if ((r_ptr->flags2 & (RF2_MONGLOW)) || ((strstr(mon_text, "fountain special")) &&
				(m_ptr->tinvis == 4)))
			{
				/* Handle "invisible" monsters */
				if ((r_ptr->flags2 & (RF2_INVISIBLE)) || (tempinv))
				{
					/* Take note */
					do_invisible = TRUE;

					/* See invisible */
					if (p_ptr->see_inv) easy = TRUE;
				}
				/* Handle "normal" MONGLOW monsters- easy to see */
				else easy = TRUE;
				/* INVISIBLE (or stealthy) MONGLOW monsters still give off light */
				easybut = TRUE;
				if (!m_ptr->disguised) m_ptr->disguised = 1;
			}

			/* Visible */
			if ((flag) || (easy))
			{
				/* Memorize flags */
				if ((do_invisible) && (!tempinv)) l_ptr->flags2 |= (RF2_INVISIBLE);
				else if (do_invisible) l_ptr->flags6 |= (RF6_INVIS);
			}
		}
	}

	/* Temporary invisibility decrements in process_monster() */
	
	/* make sure all invisible monsters are marked as invisible for darkvision check */
	if ((r_ptr->flags2 & (RF2_INVISIBLE)) || (tempinv)) do_invisible = TRUE;

    /* Darkvision: If monster is in line of sight, the player is not blind, */ 
    /* and the monster is not invisible, then the player can see it */
    /* (even if the space is not lit) */
    /* (should darkvision let you see walls without light?) */
	darkvs = FALSE;
    if (((p_ptr->darkvis) || (p_ptr->timed[TMD_DARKVIS])) &&
	   (player_has_los_bold(fy, fx)) && (!p_ptr->timed[TMD_BLIND]))
	{   
		if ((p_ptr->see_inv) || (!do_invisible))
		{
			easy = TRUE;
			darkvs = TRUE;
		}
    }

	/* True sight allows the PC to see through all disguises */
	if (((r_ptr->flags1 & (RF1_CHAR_MULTI)) || (r_ptr->flags2 & (RF2_DISGUISE))) && 
		(m_ptr->disguised) && (p_ptr->timed[TMD_TSIGHT]) && (easy)) m_ptr->disguised = 0;
		
	/* easy to see before considering the disguise */
	if ((r_ptr->flags1 & (RF1_CHAR_MULTI)) && (m_ptr->disguised) && (easy))
		easybut = TRUE;

	/* if a monster would otherwise be seen easily, check monster stealth */
	if (easy) easy = (alertness_check(m_ptr, 0, darkvs));
	/* an extremely alert character may notice signs of an invisible monster */
	else if ((!easy) && (full == 2) && 
		((r_ptr->flags2 & (RF2_INVISIBLE)) || (tempinv)))
	{
		easy = (notice_invisible_check(m_ptr));
	}

    /* once it's noticed by the player it's easier to see afterwords */
    if ((easy) && (m_ptr->monseen < 2)) m_ptr->monseen = 2;

    /* water hide monsters can hide in water even if you can still see them */
	if (((r_ptr->flags7 & (RF7_WATER_HIDE)) || (r_ptr->flags7 & (RF7_WATER_ONLY))) &&
		(cave_feat[fy][fx] == FEAT_WATER) && (m_ptr->cdis > 1))
	{
		 if ((m_ptr->monseen > 2) || (randint(170) < r_ptr->stealth*5))
			 m_ptr->monseen -= 1;
		/* water monsters go back underwater to protect themselves from ranged attacks */
		 if (m_ptr->monseen > 0) m_ptr->monseen -= 1;
	}
    /* monster has been seen but is hiding again */
	else if ((!easy) && (m_ptr->monseen > 2)) m_ptr->monseen -= 1;
    else if ((!easy) && (m_ptr->monseen > 1) && (randint(200) < r_ptr->stealth*5))
          m_ptr->monseen -= 1;
    if (m_ptr->monseen > 5) m_ptr->monseen -= 1;

    /* is it still visible after monster stealth is checked? */
    if (easy) flag = TRUE;

	/*** DJA: hearing out-of-sight monsters ***/
	/* maybe should add a 'move silently' stat for monsters separate from 'hide' stealth */
	/* was: hearcheck = (p_ptr->palert * 4) + (goodluck/2); */
	hearcheck = (p_ptr->palert * 5) + (goodluck+1)/2;
	/* hear better when you're blind */
	if (p_ptr->timed[TMD_BLIND]) hearcheck += 100;
	/* not attentive when you're running */
	if (p_ptr->running) hearcheck = (hearcheck*2)/3;
	/* hear it better when you're listening for it */
	else if (m_ptr->heard) hearcheck += 100;
	/* */
	if (p_ptr->listening) hearcheck += 200;

	/* PASS_WALL and NEVER_MOVE monsters are considered completely silent */
	/* (unless their stealth is 0: poltergeists, green glutton ghosts, and earth elementals) */
	msilent = FALSE; /* assume not completely silent */
	if ((r_ptr->flags2 & (RF2_PASS_WALL)) && (r_ptr->stealth > 0)) msilent = TRUE;
	if ((r_ptr->flags1 & (RF1_NEVER_MOVE)) && (!(r_ptr->flags4 & (RF4_SHRIEK))))
		msilent = TRUE;
	/* poltergeists make a lot of noise */
	if ((r_ptr->flags4 & (RF4_THROW)) && (r_ptr->flags2 & (RF2_PASS_WALL)))
	{
		hearcheck += 35; /* was 30 */
		/* noisy even when they don't move */
		if (full) full = 2;
	}
	/* sleeping monsters are (nearly) silent */
	if ((m_ptr->csleep) && (!m_ptr->roaming)) hearcheck = hearcheck/3;
	/* undiscovered mimmics are silent */
	if ((r_ptr->flags1 & (RF1_CHAR_MULTI)) && (m_ptr->disguised)) msilent = TRUE;
	/* don't hear offscreen monsters */
	if (!panel_contains(fy, fx)) msilent = TRUE;
	/* alertness isn't high enough */
	if ((r_ptr->stealth == 2) && (p_ptr->palert < 18)) msilent = TRUE;
	if ((r_ptr->stealth == 3) && (p_ptr->palert < 25)) msilent = TRUE;
	if ((r_ptr->stealth == 4) && (p_ptr->palert < 50)) msilent = TRUE;
	else if (r_ptr->stealth == 4) hearcheck -= 25;
	/* rune of silence */
	if (room_runes(m_ptr->fy, m_ptr->fx) == 16) msilent = TRUE;
	
	/* ensure no randint(<1) (because of negative r_ptr->stealth) */
	hearstealth = 2500 + (r_ptr->stealth * 500) + (d * 25);
    if (hearstealth < d + 1) hearstealth = d + 1;

	/* if monster is nearby and not already visible there's a chance to hear it */
	/* the town is considered safe, so don't bother if monster level is 0 */
	if ((!flag) && (full == 2) && (r_ptr->stealth < 5) && (d <= MAX_SIGHT) &&
	    (r_ptr->level) && (!msilent) && (randint(hearstealth) < hearcheck))
	{
		bool eardisturb = FALSE;

		/* decide whether to disturb */
		if (disturb_move) eardisturb = TRUE;
		/* don't disturb when running, resting, or digging */
		if ((p_ptr->resting) || (p_ptr->running) || (p_ptr->command_rep)) eardisturb = FALSE;
		/* ..unless it's near and in line of sight */
		if ((disturb_near) && (player_has_los_bold(fy, fx)) && (d < 15)) eardisturb = TRUE;

		/* disturb on hearing */
		if ((!m_ptr->heard) && (eardisturb))
		{
			/* (no message if no disturb) */
			msg_format("You hear a monster nearby but out of sight.");
			disturb(1, 0);
		}

        /* you heard it, now you're listening for it */
        m_ptr->heard = TRUE;

        /* you heard it, now you're watching for it */
        if (m_ptr->monseen < 1) m_ptr->monseen = 1;

		/* (can't examine the monster) */
    }
    /* don't hear it anymore */
    else if ((full) && (m_ptr->heard)) m_ptr->heard = FALSE;

	if (m_ptr->heard)
	{
		/* Draw the monster (always grey color) */
		lite_spot(fy, fx);

		/* (is listed in monster list but only tells type, not exact race) */
		p_ptr->window |= PW_MONLIST;
	}

	/* check if the monster never moves (check more than just NEVER_MOVE flag) */
	if ((l_ptr->flags1 & (RF1_NEVER_MOVE)) &&
		(!(r_ptr->flags6 & (RF6_BLINK))) &&
		(!(r_ptr->flags6 & (RF6_TPORT)))) nevermove = TRUE;
	else nevermove = FALSE;
	/* full == 2 means monster has just moved because there are ways that */
	/* even NEVER_MOVE monsters can move (like teleother). */
	if (full == 2) nevermove = FALSE;
	/* mark monsters that were already seen */
	if (m_ptr->ml) alreadyseen = TRUE;

	/* mimmics */
	if (((r_ptr->flags1 & (RF1_CHAR_MULTI)) || (!easy)) && (m_ptr->disguised) && 
		(easybut) && (!flag) && (!character_xtra))
	{
		/* Show whatever the monster is disguised as */
		/* (no disturb because you don't know it's a monster yet) */
		/* Hack -- Detect the monster */
		repair_mflag_show = TRUE;
		m_ptr->mflag |= (MFLAG_SHOW);

		lite_spot(fy, fx);
		/* Monsters disguised as objects should show on object list */
		p_ptr->window |= (PW_OBJLIST);
	}
	/* flag == true: If it's (easy) or detected as a monster, then it's no longer disguised */
	else if ((!(r_ptr->flags2 & (RF2_DISGUISE))) && (m_ptr->disguised) && (flag)) 
		m_ptr->disguised = 0;

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
			if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);

			/* Hack -- Count "fresh" sightings */
			if (l_ptr->sights < MAX_SHORT) l_ptr->sights++;

			/* Disturb on appearance */
			if ((disturb_move) && (!(r_ptr->flags7 & (RF7_NONMONSTER))))
			{
				/* unaware town monsters shouldn't disturb */
                if ((r_ptr->level == 0) && ((m_ptr->csleep) || (p_ptr->lev > 20))) /* skip */;
                else disturb(1, 0);
			}

			/* Window stuff */
			p_ptr->window |= PW_MONLIST;
		}

		/* everyone knows ordinary trees don't move.. */
		if ((r_ptr->flags7 & (RF7_NONMONSTER)) && (r_ptr->flags1 & (RF1_NEVER_MOVE)))
		{
			monster_lore *l_ptr = &l_list[m_ptr->r_idx];
			l_ptr->flags1 |= (RF1_NEVER_MOVE);
		}
	}

	/* The monster is not visible */
	else
	{
		/* It was previously seen */
		/* if you know that a monster never moves, remember where it is */
		if ((m_ptr->ml) && (!nevermove))
		{
			/* Mark as not visible */
			m_ptr->ml = FALSE;

			/* Erase the monster */
			lite_spot(fy, fx);

			/* Update health bar as needed */
			if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);

			/* Disturb on disappearance (why?) */
			/* if ((disturb_move) && (!(r_ptr->flags7 & (RF7_NONMONSTER)))) disturb(1, 0); */

			/* Window stuff */
			p_ptr->window |= PW_MONLIST;
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

#if idontrememberwhatthisisfor
			/* Disturb on appearance */
			if ((disturb_near) && ((!m_ptr->roaming) || (m_ptr->roaming < 20))) disturb(1, 0);
#endif
			
			/* nevermove monsters that you already know about shouldn't disturb */
			if ((nevermove) && (alreadyseen)) /* skip */;
			/* unaware town monsters shouldn't disturb */
			else if ((r_ptr->level == 0) && ((m_ptr->csleep) || (p_ptr->lev >= 20))) /* skip */;
			/* Disturb on appearance (even if roaming) */
			else if ((disturb_near) && (!(r_ptr->flags7 & (RF7_NONMONSTER))))
                disturb(1, 0);

			/* Window stuff */
			p_ptr->window |= PW_MONLIST;
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

			/* Window stuff */
			p_ptr->window |= PW_MONLIST;
		}
	}

	/* don't mark nevermove monster as in LOS when they aren't */
	/* (don't get why the easy flag doesn't work for this) */
	if ((nevermove) && (!player_has_los_bold(fy, fx)))
	{
		/* Change */
		if (m_ptr->mflag & (MFLAG_VIEW))
		{
			/* Mark as not easily visible */
			m_ptr->mflag &= ~(MFLAG_VIEW);

			/* Window stuff */
			p_ptr->window |= PW_MONLIST;
		}
	}

	
    /* monster now has LOS to the PC */
    if ((m_ptr->cdis <= MAX_RANGE) && (projectable(m_ptr->fy, m_ptr->fx, py, px)))
	{
        if (!(m_ptr->mflag & (MFLAG_MLOS)))
        {
			/* Mark as having LOS to the player */
			m_ptr->mflag |= (MFLAG_MLOS);
			
			/* only note if awake and sensed by telepathy */
			if ((disturb_espmove) && (flag) && (!easy) && (!m_ptr->csleep) &&
				(!(r_ptr->flags7 & (RF7_NONMONSTER))))
			{
               disturb(1, 0);
#if nomessage
               /* get monster name */
		       char m_name[80];
               monster_desc(m_name, sizeof(m_name), m_ptr, 0x04);
                       
               /* should there be a message here? */
               msg_format("You sense that %s can see you.", m_name);
#endif
            }
        }
    }
    else if (m_ptr->mflag & (MFLAG_MLOS))
    {
		/* Mark as not having LOS to the PC */
		m_ptr->mflag &= ~(MFLAG_MLOS);
    }

	/* check monster's awareness of you */
	/* asleep but displayed as awake in monster list */
	if ((m_ptr->csleep) && (!m_ptr->display_sleep))
	{
	   /* update monster list */
	   p_ptr->window |= PW_MONLIST;
    }
    /* awake but displayed as asleep in monster list */
	if ((!m_ptr->csleep) && (m_ptr->display_sleep))
	{
	   /* update monster list */
	   p_ptr->window |= PW_MONLIST;
    }
    
	/* meet == 11 is a magic code, so reset it */
    if ((easy) && (m_ptr->meet > 10)) m_ptr->meet = 0;
    
    /* chance to recognise illusions */
    if ((easy) && (m_ptr->extra2 < 9)) discern_illusion(m_idx);

	/* done if you've met the monster before or if you're not meeting it now */
	if ((m_ptr->meet) || (!easy)) return;

   /* "Nice to meet you" (First impression) */
   if ((easy) && (m_ptr->cdis < 10))
   {
       /* bluff */
	   int bluff = r_ptr->stealth;
	   if (bluff < 0) bluff = 0;
	   if (r_ptr->level <= 20) bluff += 1;
	   else bluff += r_ptr->level / 20;
	   
	   if (r_ptr->flags2 & (RF2_SMART)) bluff += randint((bluff+1)/2) + 1;
	   if ((r_ptr->flags2 & (RF2_STUPID)) && (bluff > 1)) bluff = bluff / 2;
	   if (m_ptr->disguised) bluff = bluff * 2;
	   
	   bluff -= (l_ptr->sights)/10;
	   if (bluff < 1) bluff = 1;
	   
	   /* Meeting the monster for the first time */
       m_ptr->meet = 1;
      
       /*** Can you discern if the monster is evil? ***/

       /* discernment based on wisdom */
       discernmod = (adj_mag_study[p_ptr->stat_ind[A_WIS]] / 10);

       /* paladins almost always recognise evil */
       if ((cp_ptr->spell_book == TV_PRAYER_BOOK) && (!cp_ptr->flags & CF_ZERO_FAIL))
       {
          discernmod += 21;
       }
       /* aligned classes recognise alignment easier */
       /* (priests and druids but not healers) */
       else if ((cp_ptr->flags & CF_BLESS_WEAPON) ||
            (cp_ptr->spell_book == TV_DARK_BOOK))
       {
            /* wielding equipment with alignment that matches yours */
            if ((magicmod == 4) || (magicmod == 6)) discernmod += 14;
            else discernmod += 7;
       }
       /* small bonus for healers */
       else if (cp_ptr->spell_book == TV_PRAYER_BOOK)
       {
            discernmod += 3;
       }

	   /* telepathy helps */
	   if (p_ptr->telepathy) discernmod += 7 + (goodluck/2);

       /* chance realm casters get modifier from luck */
       if (cp_ptr->spell_book == TV_LUCK_BOOK)
       {
          if (goodluck > 2) discernmod += goodluck/3;
          if (badluck > 2) discernmod -= badluck/3;
       }
       /* everyone else gets much weaker modifier from luck */
       else
       {
           if (goodluck >= 5) discernmod += goodluck/5;
           if (badluck >= 5) discernmod -= badluck/5;
       }

       lore = TRUE;

       /* always recognise if you know the race is always evil */
       if (l_ptr->flags3 & (RF3_EVIL)) discernmod += 90;
       /* easier to recognise if you know it's sometimes evil */
       else if (l_ptr->flags2 & (RF2_S_EVIL2)) discernmod += 4;
       else if (l_ptr->flags2 & (RF2_S_EVIL1)) discernmod += 2;
       else lore = FALSE;
   
       if (!lore)
       {
          /* always evil monsters are easier to recognise */
          if (r_ptr->flags3 & (RF3_EVIL)) discernmod += 2;
          /* occationally evil monsters are harder to recognise */
          /* if you don't know that they're sometimes evil */
          else if (r_ptr->flags2 & (RF2_S_EVIL2)) discernmod -= 5;
          else if (r_ptr->flags2 & (RF2_S_EVIL1)) discernmod -= 9;
       }
   
       /* discern */
       if (bluff + randint(24) < discernmod)
       {
          /* If it's always evil then the monster lore tells you that it's evil */
          if (r_ptr->flags3 & (RF3_EVIL))
          {
             /* remember */
             l_ptr->flags3 |= (RF3_EVIL);

             /* don't need a separate message telling you it's evil */
          }
          else if (m_ptr->evil)
          {
			  /* can tell (later) that the monster is evil */
			  m_ptr->meet = 2;
              
			  if (r_ptr->flags2 & (RF2_S_EVIL2)) l_ptr->flags2 |= (RF2_S_EVIL2);
			  else if (r_ptr->flags2 & (RF2_S_EVIL1)) l_ptr->flags2 |= (RF2_S_EVIL1);
          }
          /* (much harder to tell for sure that a monster is NOT evil) */
          else if (bluff + randint(50) < discernmod)
          {
              /* can tell (later) that the monster is not evil */
              m_ptr->meet = 3;
          }
       }
       /* if you fail this first chance, a detect evil spell or slay evil weapon */
       /* are the only ways to find out if an individual monster is evil or not */
   }
}




/*
 * This function simply updates all the (non-dead) monsters (see above).
 */
void update_monsters(bool full)
{
	int i;

	/* Update each (live) monster */
	for (i = 1; i < mon_max; i++)
	{
		monster_type *m_ptr = &mon_list[i];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* monster is temporarily dead */
        if (m_ptr->temp_death) continue;

		/* Update the monster */
		if (full) update_mon(i, 1);
		else update_mon(i, 0);
	}
}




/*
 * Make a monster carry an object
 */
s16b monster_carry(int m_idx, object_type *j_ptr)
{
	s16b o_idx;
	s16b this_o_idx, next_o_idx = 0;
	monster_type *m_ptr = &mon_list[m_idx];


	/* Scan objects already being held for combination */
	for (this_o_idx = m_ptr->hold_o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Check for combination */
		if (object_similar(o_ptr, j_ptr))
		{
			/* Combine the items */
			object_absorb(o_ptr, j_ptr);

			/* Result */
			return (this_o_idx);
		}
	}


	/* Make an object */
	o_idx = o_pop();

	/* Success */
	if (o_idx)
	{
		object_type *o_ptr;

		/* Get new object */
		o_ptr = &o_list[o_idx];

		/* Copy object */
		object_copy(o_ptr, j_ptr);

		/* Forget mark */
		o_ptr->marked = FALSE;

		/* Forget location */
		o_ptr->iy = o_ptr->ix = 0;

		/* Link the object to the monster */
		o_ptr->held_m_idx = m_idx;

		/* Link the object to the pile */
		o_ptr->next_o_idx = m_ptr->hold_o_idx;

		/* Link the monster to the object */
		m_ptr->hold_o_idx = o_idx;
	}

	/* Result */
	return (o_idx);
}

#ifdef roomrunes
/* gets any magic room effects for the room containing the grid in question.
 */
s16b room_runes(int y, int x)
{
	int ry1, rx1, ry2, rx2, ry3, rx3;
	/* when checking roomeffect on entering the dungeon level, oy and ox are 0 */
	/* see check_roomeffect() */
	if ((!y) || (!x)) return 0;
	/* no room runes on this level */
	if (!r_rune1) return 0;
	
	/* convert room rune locations */
	ry1 = r_rune1/1000;
	rx1 = r_rune1 - (ry1 * 1000);
	ry2 = r_rune2/1000;
	rx2 = r_rune2 - (ry2 * 1000);
	ry3 = r_rune3/1000;
	rx3 = r_rune3 - (ry3 * 1000);
	/* check in_same_room() */
	if (in_same_room(y, x, ry1, rx1)) return r_rune1typ;
	if (in_same_room(y, x, ry2, rx2)) return r_rune2typ;
	if (in_same_room(y, x, ry3, rx3)) return r_rune3typ;
	
	return 0;
}

/* save the location of room runes */
/* (so we won't have to go through the monster list every time room_runes() is called) */
bool save_runes(void)
{
	int i;
	bool anyrunes = FALSE;
	/* reset rune locations */
	r_rune1 = 0;
	r_rune2 = 0;
	r_rune3 = 0;
	r_rune1typ = 0;
	r_rune2typ = 0;
	r_rune3typ = 0;
	/* browse monsters */
	for (i = 1; i < mon_max; i++)
	{
		monster_type *m_ptr = &mon_list[i];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;
		
		/* monsters greater than index 1400 are magic room runes (not monsters at all) */
		if (m_ptr->r_idx > 1400)
		{
			/* save the location of room runes */
			if (!r_rune1) 
			{
				r_rune1 = (m_ptr->fy * 1000) + m_ptr->fx;
				r_rune1typ = m_ptr->r_idx - 1400;
			}
			else if (!r_rune2)
			{
				r_rune2 = (m_ptr->fy * 1000) + m_ptr->fx;
				r_rune2typ = m_ptr->r_idx - 1400;
			}
			else if (!r_rune3)
			{
				r_rune3 = (m_ptr->fy * 1000) + m_ptr->fx;
				r_rune3typ = m_ptr->r_idx - 1400;
			}
			/* enforce no more than 3 per level */
			else delete_monster_idx(i, FALSE);
			anyrunes = TRUE;
		}
	}
	return anyrunes;
}

/* check any magic effects of the room which the PC is in */
void check_roomeffect(int oy, int ox)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
	int oeff = room_runes(oy, ox); /* old room effect (0 if just entering the level) */
	int neff = room_runes(py, px); /* new room effect */
	
	/* no change */
	if ((neff == oeff) && (neff == p_ptr->roomeffect)) return;

	/* save the new room effect */
	p_ptr->roomeffect = neff;

	p_ptr->update |= (PU_BONUS);
	/* roomeffect 9 is the enveloping dark */
	if ((p_ptr->roomeffect == 9) || (oeff == 9)) p_ptr->update |= (PU_TORCH);

	/* sundial */
	if ((neff == 10) && (!(oeff == 10)))
	{
		bool night = TRUE;
		msg_print("This room magically shows the sky above ground instead of the rock ceiling.");
		/* night or day */
		if ((turn % (10L * TOWN_DAWN)) < ((10L * TOWN_DAWN) / 2)) night = FALSE;
		if ((night) && (p_ptr->theme == 7)) msg_print("You look up and see the full moon.");
		else if (night) msg_print("You look up and see the moon and stars.");
		else msg_print("It is a strange but welcome sight to see real daylight deep in the dungeon.");
	}
	/* message */
	if ((neff == 1) && (!(oeff == 1))) msg_print("Scenes of endless combat are painted on the walls of this room.");
	if ((neff == 2) && (!(oeff == 2))) msg_print("This room is littered with dead rats. It smells disgusting.");
	if ((neff == 3) && (!(oeff == 3)))
	{
		/* famine's rune automatically removes satiation */
		if (p_ptr->food >= PY_FOOD_MAX) p_ptr->food = PY_FOOD_MAX - 1;
		msg_print("You suddenly feel hungry, then you notice a corpse of skin and bones.");
	}
	if ((neff == 4) && (!(oeff == 4))) msg_print("It is especially hot in here. The walls drip lava.");
	if ((neff == 5) && (!(oeff == 5))) msg_print("It is especially cold in here. You notice icicles.");
	if ((neff == 6) && (!(oeff == 6))) msg_print("Slime drips from the walls and spatters the floor here.");
	if ((neff == 7) && (!(oeff == 7))) msg_print("Lightning flashes across the ceiling here as if you were outdoors.");
	if ((neff == 8) && (!(oeff == 8))) msg_print("It is extremely windy in this room.");
	if ((neff == 9) && (!(oeff == 9)))
	{
		char o_name[80];
		object_type *o_ptr = &inventory[INVEN_LITE];
		object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 0);
		if (o_ptr) msg_format("Your %s seems dimmer in this room.", o_name);
		else msg_print("The darkness in this room seems blacker than black.");
	}
	if ((neff == 11) && (!(oeff == 11)))
	{ 
		if (p_ptr->timed[TMD_IMAGE]) msg_print("Woah. Your hallucenations are especially intense here.");
		msg_print("You have a momentary hallucenation as you enter this room.");
	}
	if ((neff == 12) && (!(oeff == 12))) msg_print("Your magic items pulse in this room.");
	if ((neff == 13) && (!(oeff == 13)))
	{ 
		if (cp_ptr->spell_book == TV_DARK_BOOK) msg_print("You feel your necromancy is more powerful here.");
		else msg_print("This room gives you a creepy feeling. You imagine ghostly skulls in every shadow.");
	}
	if ((neff == 14) && (!(oeff == 14))) msg_print("This room is full of static. Your hair sticks out.");
	if ((neff == 15) && (!(oeff == 15))) 
	{ 
		msg_print("Recently healed wounds reopen as you enter this room.");
		if ((badluck) && (p_ptr->depth >= 18)) (void)inc_timed(TMD_CUT, p_ptr->depth/6);
		else (void)inc_timed(TMD_CUT, 2);
	}
	if ((neff == 16) && (!(oeff == 16))) msg_print("It is utterly silent in here.");
	if ((neff == 17) && (!(oeff == 17))) msg_print("The walls of this room appear to be solid titanium.");
	/* if ((neff == 18) && (!(oeff == 18))) msg_print("."); (can't think of a good message for rising dead rune) */
	if ((neff == 19) && (!(oeff == 19))) msg_print("Everything in this room seems to blink in and out of existence.");
}
#endif /* roomrunes */

/*
 * Swap the players/monsters (if any) at two locations XXX XXX XXX
 * (this is used whenever a monster or the PC moves in any way)
 */
void monster_swap(int y1, int x1, int y2, int x2)
{
	int m1, m2, oy, ox;
	bool pcmove = FALSE;

	monster_type *m_ptr;
	monster_race *r_ptr;
	monster_race *ar_ptr;

	/* Monsters */
	m1 = cave_m_idx[y1][x1];
	m2 = cave_m_idx[y2][x2];


	/* Update grids */
	cave_m_idx[y1][x1] = m2;
	cave_m_idx[y2][x2] = m1;

	/* if the PC moves, DEMON_WARD ends */
	if ((m1 < 0) || (m2 < 0))
	{ 
		clear_timed(TMD_DEMON_WARD);
		pcmove = TRUE;
		/* save old location */
		if (m1 < 0) { oy = y1; ox = x1; }
		else if (m2 < 0) { oy = y2; ox = x2; }
	}

	/* Monster 1 */
	if (m1 > 0)
	{
		m_ptr = &mon_list[m1];
		r_ptr = &r_info[m_ptr->r_idx];

		/* Move monster */
		m_ptr->fy = y2;
		m_ptr->fx = x2;

		/* handle monsters that block line of sight */
		if (r_ptr->flags7 & (RF7_BLOCK_LOS))
		{
			cave_info[y2][x2] |= (CAVE_WALL);
			/* allow for PASS_WALL monsters that block LOS */
			if ((cave_feat[y1][x1] >= FEAT_SECRET) ||
				((cave_feat[y1][x1] >= FEAT_DOOR_CLOSE) &&
				(cave_feat[y1][x1] <= FEAT_DOOR_STUCK)))
			{
				/* don't remove CAVE_WALL */
			}
			else
			{
				cave_info[y1][x1] &= ~(CAVE_WALL);
			}
		}
		
		if (monster_is_slowed_by_water(m_ptr->r_idx))
		{
	        /* moving into a space with water */
    	    if ((cave_feat[y2][x2] == FEAT_WATER) &&
				(!(cave_feat[y1][x1] == FEAT_WATER)))
			{
				m_ptr->mspeed -= 2;
			}
	        /* moving out of a space with water */
    	    if ((cave_feat[y1][x1] == FEAT_WATER) &&
				(!(cave_feat[y2][x2] == FEAT_WATER)))
			{
				m_ptr->mspeed += 2;
			}
		}

		/* Update monster */
		update_mon(m1, 3);
	}

	/* Player 1 */
	else if (m1 < 0)
	{
		/* Move player */
		p_ptr->py = y2;
		p_ptr->px = x2;

		/* Update the panel */
		p_ptr->update |= (PU_PANEL | PU_UPDATE_FLOW);

		/* Update the visuals (and monster distances) */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_DISTANCE);

		/* Update bonuses (water slows you down) */
		if ((cave_feat[y1][x1] == FEAT_WATER) ||
			(cave_feat[y2][x2] == FEAT_WATER))
 			p_ptr->update |= (PU_BONUS);

		/* Window stuff */
		p_ptr->window |= (PW_OVERHEAD | PW_MAP);
	}

	/* Monster 2 */
	if (m2 > 0)
	{
		m_ptr = &mon_list[m2];
		ar_ptr = &r_info[m_ptr->r_idx];

		/* Move monster */
		m_ptr->fy = y1;
		m_ptr->fx = x1;

		/* handle monsters that block line of sight */
		if (ar_ptr->flags7 & (RF7_BLOCK_LOS))
		{
			cave_info[y2][x2] |= (CAVE_WALL);
			/* allow for PASS_WALL monsters that block LOS */
			if ((cave_feat[y1][x1] >= FEAT_SECRET) ||
				((cave_feat[y1][x1] >= FEAT_DOOR_CLOSE) &&
				(cave_feat[y1][x1] <= FEAT_DOOR_STUCK)))
			{
				/* don't remove CAVE_WALL */
			}
			/* allow for two BLOCK_LOS monsters switching with each other */
			/* (although I don't think this should ever happen..) */
			else if ((m1 > 0) && (ar_ptr->flags7 & (RF7_BLOCK_LOS)))
			{
				/* don't remove CAVE_WALL */
			}
			else
			{
				cave_info[y1][x1] &= ~(CAVE_WALL);
			}
		}
		
		if (monster_is_slowed_by_water(m_ptr->r_idx))
		{
	        /* moving into a space with water */
    	    if ((cave_feat[y1][x1] == FEAT_WATER) &&
				(!(cave_feat[y2][x2] == FEAT_WATER)))
			{
				m_ptr->mspeed -= 2;
			}
	        /* moving out of a space with water */
    	    if ((cave_feat[y2][x2] == FEAT_WATER) &&
				(!(cave_feat[y1][x1] == FEAT_WATER)))
			{
				m_ptr->mspeed += 2;
			}
		}

		/* Update monster */
		update_mon(m2, 3);
	}

	/* Player 2 */
	else if (m2 < 0)
	{
		/* Move player */
		p_ptr->py = y1;
		p_ptr->px = x1;

		/* Update the panel and flow */
		p_ptr->update |= (PU_PANEL | PU_UPDATE_FLOW);

		/* Update the visuals (and monster distances) */
		p_ptr->update |= (PU_UPDATE_VIEW | PU_DISTANCE);

		/* Update bonuses (water slows you down) */
		if ((cave_feat[y1][x1] == FEAT_WATER) ||
			(cave_feat[y2][x2] == FEAT_WATER))
 			p_ptr->update |= (PU_BONUS);

		/* Window stuff */
		p_ptr->window |= (PW_OVERHEAD | PW_MAP);
	}
	
#ifdef roomrunes
	/* */
	if (pcmove) check_roomeffect(oy, ox);
#endif

	/* Notice */
	note_spot(y1, x1);
	note_spot(y2, x2);

	/* Redraw */
	lite_spot(y1, x1);
	lite_spot(y2, x2);
}


/*
 * Place the player in the dungeon XXX XXX
 */
s16b player_place(int y, int x)
{
	/* Paranoia XXX XXX */
	if (cave_m_idx[y][x] != 0) return (0);


	/* Save player location */
	p_ptr->py = y;
	p_ptr->px = x;

	/* Mark cave grid */
	cave_m_idx[y][x] = -1;

	/* Success */
	return (-1);
}


/*
 * Place a copy of a monster in the dungeon XXX XXX (when loading a savefile)
 */
s16b monster_place(int y, int x, monster_type *n_ptr)
{
	s16b m_idx;
	monster_type *m_ptr;
	monster_race *r_ptr;

	/* Get a new record */
	m_idx = mon_pop();

	/* Oops */
	if (m_idx)
	{
		/* Make a new monster */
		m_ptr = &mon_list[m_idx];

		/* Copy the monster XXX */
		COPY(m_ptr, n_ptr, monster_type);

		/* Paranoia XXX a temporarily dead monster may be in the same space as a */
		/* living monster (just can't come back to life in the same space). */
		if ((cave_m_idx[y][x] != 0) && (!m_ptr->temp_death)) return (0);

		/* Location */
		m_ptr->fy = y;
		m_ptr->fx = x;
		
		/* don't actually put the monster in the dungeon if it is temporarily dead */
        if (m_ptr->temp_death) return (m_idx);

		/* place the monster in the dungeon */
		cave_m_idx[y][x] = m_idx;

		/* Update the monster */
		update_mon(m_idx, 1);

		/* Get the new race */
		r_ptr = &r_info[m_ptr->r_idx];

		/* Hack -- Notice new multi-hued monsters */
		if (r_ptr->flags1 & (RF1_ATTR_MULTI)) shimmer_monsters = TRUE;

		/* Hack -- Count the number of "reproducers" */
		if (r_ptr->flags2 & (RF2_MULTIPLY)) num_repro++;

		/* Count racial occurances */
		r_ptr->cur_num++;
	}

	/* Result */
	return (m_idx);
}


/*
 * Count the number of empty spaces next to a grid
 *  Mostly from next_to_corr() in generate.c except
 * that it counts any non-wall spaces, not only empty corridors.
 */
int next_to_floor(int y1, int x1)
{
	int i, y, x, k = 0;

	/* Scan adjacent grids */
	for (i = 0; i < 8; i++)
	{
		/* Extract the location */
		y = y1 + ddy_ddd[i];
		x = x1 + ddx_ddd[i];

		/* Skip non floors */
		if (!cave_floor_bold(y, x)) continue;

		/* Count these grids */
		k++;
	}

	/* Return the number of empty spaces */
	return (k);
}


/*
 * Find the index of the object_kind with the given tval and sval.
 * used for mimmics - didn't use normal lookup_kind function because
 * I didn't want people getting the "No object (blah, blah)" message
 * when there was nothing wrong.
 */
s16b lookup_mimmic_kind(int tval, int sval)
{
	int k;

	/* Look for it */
	for (k = 1; k < z_info->k_max; k++)
	{
		object_kind *k_ptr = &k_info[k];

		/* Found a match */
		if ((k_ptr->tval == tval) && (k_ptr->sval == sval)) return (k);
	}

	/* Oops */
	return (0);
}

/* mimmics and DISGUISE monsters put on their disguises */
void disguise_mimmics(monster_type *n_ptr)
{
	int dkind = 0;
	monster_race *r_ptr = &r_info[n_ptr->r_idx];
	int nodisguise = 15;
	if (r_ptr->stealth > 3) nodisguise = 55;
	/* This monster is disguised (mimmics) */
	if ((r_ptr->flags1 & (RF1_CHAR_MULTI)) && (strchr("!,-_~=?#%:.$", r_ptr->d_char)))
	{
		if (strchr("!", r_ptr->d_char)) /* potion mimmics */
		{
			/* in case it doesn't get a valid kind on 1st try  */
			while (!dkind)
			{
	            int dk = 23 + rand_int(36);
				if (dk > 45) dk += 1; /* no sval 46 */
				dkind = lookup_mimmic_kind(75, dk);
			}
			/* disguised = index of object it is disguised as */
			n_ptr->disguised = dkind;
		}
		else if (strchr(",", r_ptr->d_char)) /* mushroom mimmic (currently none) */
		{
			/* in case it doesn't get a valid kind on 1st try  */
			while (!dkind)
			{
	            int dk = 0 + rand_int(19);
				dkind = lookup_mimmic_kind(80, dk);
			}
			n_ptr->disguised = dkind;
		}
		else if (strchr("-", r_ptr->d_char)) /* wand mimmics */
		{
			while (!dkind)
			{
	            int dk = 0 + rand_int(25);
				dkind = lookup_mimmic_kind(65, dk);
			}
			n_ptr->disguised = dkind;
		}
		else if (strchr("_", r_ptr->d_char)) /* staff mimmics */
		{
			while (!dkind)
			{
				int dk = 0 + rand_int(33);
				dkind = lookup_mimmic_kind(55, dk);
			}
			n_ptr->disguised = dkind;
		}
		else if (strchr("$", r_ptr->d_char)) /* coin mimmics */
		{
			if (p_ptr->depth < 16) n_ptr->disguised = lookup_mimmic_kind(100, 1 + rand_int(5));
			else if (p_ptr->depth < 25) n_ptr->disguised = lookup_mimmic_kind(100, 1 + rand_int(p_ptr->depth-10));
			else n_ptr->disguised = lookup_mimmic_kind(100, 6 + rand_int(14));
		}
		else if (strchr("~", r_ptr->d_char)) /* chest mimmics */
		{
			while (!dkind)
			{
				int dk = 1 + rand_int(5);
				if (dk > 3) dk += 1; /* no sval 4 */
				dkind = lookup_mimmic_kind(7, dk);
			}
			n_ptr->disguised = dkind;
		}
		else if (strchr("=", r_ptr->d_char)) /* ring mimmics */
		{
			while (!dkind)
			{
				int dk = 6 + rand_int(22);
				if (dk > 12) dk += 3; /* no 13-15 svals  */
				dkind = lookup_mimmic_kind(45, dk);
			}
			n_ptr->disguised = dkind;
		}
		else if (strchr("?", r_ptr->d_char)) /* scroll mimmics */
		{
			while (!dkind)
			{
				int dk = 7 + rand_int(36);
				/* no scroll sval for these numbers */
				/* if ((dk == 19) || (dk == 23) || (dk == 26) || (dk == 31))
				dk = 7 + rand_int(12); */
				dkind = lookup_mimmic_kind(70, dk);
			}
            n_ptr->disguised = dkind;
		}
		/* disguised as a wall or floor ('#', '.', ':' or '%') */
        else n_ptr->disguised = 1;
	}
	/* non-uniques with the DISGUISE flag don't always use a disguise */
	else if ((!(r_ptr->flags1 & (RF1_UNIQUE))) && (randint(100) < nodisguise))
	{
		n_ptr->disguised = 0;
		return;
	}
	/* monster that disguises as another monster */
	else if ((r_ptr->flags2 & (RF2_DISGUISE)) && (!n_ptr->extra2))
	{
		monster_race *dr_ptr; /* disguise race */
		int dr_idx, tries = 0;
		summon_specific_type = CHOOSE_DISGUISE;
		summoner = n_ptr->r_idx; /* chooses similar to summon kin except enforces lower level */

		/* Require "okay" monsters *//* Prepare allocation table */
		get_mon_num_hook = summon_specific_okay;
		get_mon_num_prep();

		/* keeps trying until it gets a valid r_idx */
		while (tries < 90)
		{
			tries++;

			dr_idx = get_mon_num(r_ptr->level - 1, FALSE);

			/* Handle failure */
			if (!dr_idx) continue;

			dr_ptr = &r_info[dr_idx];
		
			/* maximum population */
			if ((dr_ptr->curpop + dr_ptr->cur_num >= dr_ptr->maxpop) && (dr_ptr->maxpop)) continue;

			/* otherwise accept */
			break;
		}

		/* Remove restriction *//* Prepare allocation table */
		get_mon_num_hook = NULL;
		get_mon_num_prep();
		
		n_ptr->disguised = dr_idx;
		/* tries to act like the monster he's disguised as so as not to give himself away too soon */
		if (dr_ptr->speed < n_ptr->mspeed - 4)
			n_ptr->mspeed = (dr_ptr->speed + r_ptr->speed + 2)/2;
		
		/* msg_format("gets here: %d", n_ptr->disguised); *//*(testing) */
		return;
	}
	else n_ptr->disguised = 0;
}


/* give a psuedo-unique monster a name */
void champ_name(monster_type *m_ptr)
{
	char buf[40];
	char word[12];
	int tolkyc = 9, latw = 0;
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	cptr name = (r_name + r_ptr->name);

	if (strchr("@", r_ptr->d_char)) tolkyc = 99;
	if (strchr("BEKpt", r_ptr->d_char)) tolkyc = 50;
	if (strchr("CGHPTUVWahfusxz%&", r_ptr->d_char)) tolkyc = 28;
	if (strchr("DOdilmn", r_ptr->d_char)) tolkyc = 16;

	/* chance of using the scroll name word list */
	if (strchr("@BECDGHKPOUdfilptu&", r_ptr->d_char)) latw = 16;

	if (r_ptr->flags1 & RF1_FEMALE)
	{
		if ((tolkyc) && (tolkyc < 90)) tolkyc += 5;
		if (latw) latw += 4;
	}
	
	/* use RANDNAME_TOLKIEN for elves, and some people, and occationally others */
	if (randint(100) < tolkyc)
			randname_make(RANDNAME_TOLKIEN, 5, 11, word, sizeof word);
	else if (randint(100) < latw)
			randname_make(RANDNAME_SCROLL, 5, 11, word, sizeof word);
	/* otherwise use RANDNAME_SILLY */
	else randname_make(RANDNAME_SILLY, 4, 11, word, sizeof word);
	/* I have no idea what this line is for */
	word[0] = toupper((unsigned char) word[0]);

	strnfmt(buf, sizeof(buf), "%s", word);

	/* save the name */
	strnfmt(m_ptr->champion_name, sizeof(m_ptr->champion_name), buf);
	/* strnfmt(m_ptr->champion_name, sizeof(m_ptr->champion_name), " the ");
	strnfmt(m_ptr->champion_name, sizeof(m_ptr->champion_name), name); */
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
 * DAJ: and return_monsters() in dungeon.c which brings temporarily dead
 * monsters back to life.
 *
 *   spccode is almost always 0. exceptions:
 * spccode == 1 means it's a tree or statue in a vault design so
 * disregard whether it blocks a path or not.
 *  (normally NONMONSTERs are not placed if it would block a path.)
 * spccode == 3 enforce awake
 *  (slp == false) means monsters generated over time, usually but not always awake)
 * spccode == 4 supress "deep monster" cheat_hear messages for a monster pit
 *  (so cheat_hear users don't get ~30 of the messages in a row)
 *      spccode > 20 means mark the monster as an illusory monster:
 * --spccode == 22 means imaginary monster created by hallucentations
 * --spccode == 23 means illusory monster summoned by summon illusion(s) monster spell
 * --spccode == 24 means illusory monster created by illusion_clone monster spell
 * --spccode == 25 means illusory monster (other, for later)
 */
static bool place_monster_one(int y, int x, int r_idx, bool slp, bool group, int spccode)
{
	int i;
	byte isroam, weakenme = 0;
	bool roamflag, spawnroam = FALSE;
	int evilchance, drops = 0;

	monster_race *r_ptr;
	monster_type *n_ptr;
	monster_type monster_type_body;
	cptr name;

	/* Paranoia */
	if (!in_bounds_fully(y, x)) return (FALSE);

	/* Paranoia */
	if (!r_idx) return (FALSE);

	/* Race */
	r_ptr = &r_info[r_idx];

	/* WATER_ONLY monsters are only generated in water */
	/* checks this elseware, and should be allowed if caused by polymorph */
	if (p_ptr->leaving) /* leaving is true only while the level is being generated */
	{
		bool vault = FALSE;
		if (cave_info[y][x] & (CAVE_ICKY)) vault = TRUE;
		if ((r_ptr->flags7 & (RF7_WATER_ONLY)) &&
			(!(cave_feat[y][x] == FEAT_WATER)))
		{
			if ((p_ptr->theme) && (theme_okay(r_idx, 0, vault)) && 
				((randint(100) < 35) || (vault)))
               place_puddle(y, x, FALSE);
			else return (FALSE);
		}
	}

	/* NONMONSTERs shouldn't get in the way */
	if ((r_ptr->flags7 & (RF7_NONMONSTER)) && (!(spccode == 1)))
	{
		int mtspaces = next_to_floor(y, x);
        /* maybe find a better location */
		if ((mtspaces < 3) || (possible_doorway(y, x, TRUE)))
		{
			int spoty, spotx;
			/* abort if there's no good space for a tree nearby */
			if (!get_nearby(y, x, &spoty, &spotx, 6)) return (FALSE);
			y = spoty;
			x = spotx;
		}
    }

	/* pass_door monsters (silver moss) can multiply into a door space */
	if ((((cave_feat[y][x] >= FEAT_DOOR_CLOSE) &&
          (cave_feat[y][x] <= FEAT_DOOR_STUCK)) ||
         /* (cave_feat[y][x] == FEAT_SECRET) || */
		  (cave_feat[y][x] == FEAT_SMRUBBLE)) &&
		  (cave_m_idx[y][x] == 0) &&
		  (r_ptr->flags2 & (RF2_PASS_DOOR))) /* okay */;

	/* Require empty space */
	else if (!cave_can_occupy_bold(y, x)) return (FALSE);

	/* Hack -- no creation on glyph of warding */
	if (cave_feat[y][x] == FEAT_GLYPH) return (FALSE);

	/* Paranoia */
	if (!r_ptr->name) return (FALSE);

	/* Name */
	name = (r_name + r_ptr->name);

	/* Hack - "unique" monsters must be unique (unless it's imaginary) */
	if (((r_ptr->flags1 & (RF1_UNIQUE)) || (r_ptr->maxpop == 1)) && 
		(r_ptr->curpop + r_ptr->cur_num >= r_ptr->max_num) && (spccode < 20))
			return (FALSE);

	/* Depth monsters may NOT be created out of depth */
	if ((r_ptr->flags1 & (RF1_FORCE_DEPTH)) && (p_ptr->depth < r_ptr->level) &&
		(spccode < 20))	return (FALSE);

	/* Powerful monster */
	if ((r_ptr->level > p_ptr->depth) && (spccode < 20))
	{
		/* Unique monsters */
		if (r_ptr->flags1 & (RF1_UNIQUE))
		{
			/* Message for cheaters */
			if (cheat_hear) msg_format("Deep Unique (%s).", name);

			/* Boost rating by twice delta-depth */
			/* was (r_ptr->level - p_ptr->depth) * 2; */
			rating += (r_ptr->level - p_ptr->depth + 1) / 2; /* round up */
		}
		/* Normal monsters */
		else if (!(r_ptr->flags7 & (RF7_NONMONSTER)))
		{
			/* Message for cheaters */
			if ((cheat_hear) && (!(spccode == 4))) msg_format("Deep Monster (%s).", name);

			/* Boost rating by delta-depth */
			/* was (r_ptr->level - p_ptr->depth); */
			rating += (r_ptr->level - p_ptr->depth + 1) / 4;
		}
	}

	/* Note the monster */
	else if ((r_ptr->flags1 & (RF1_UNIQUE)) && (spccode < 20))
	{
		/* Unique monsters induce message */
		if (cheat_hear) msg_format("Unique (%s).", name);
		
		/* in depth uniques raise rating slightly */
        if (r_ptr->level > p_ptr->depth - 4) rating += 1;
	}
	else if ((r_ptr->flags1 & (RF1_CHAR_MULTI)) && (spccode < 20))
	{
		/* mimmics induce messages now (only if you really want them to) */
		if ((cheat_hear) && (cheat_xtra)) msg_format("Mimmic (%s).", name);
	}
	
	/* only sometimes count drops if the monster doesn't always drop */
    if ((r_ptr->flags1 & (RF1_DROP_30)) && (rand_int(100) < 5)) drops += 1;
	if ((r_ptr->flags1 & (RF1_DROP_60)) && (rand_int(100) < 16)) drops += 1;
	if ((r_ptr->flags1 & (RF1_DROP_90)) && (rand_int(100) < 34)) drops += 1;
	if (r_ptr->flags1 & (RF1_DROP_1D2)) drops += 1;
	if (r_ptr->flags1 & (RF1_DROP_2D2)) drops += 2;
	if (r_ptr->flags1 & (RF1_DROP_3D2)) drops += 3;
	if (r_ptr->flags1 & (RF1_DROP_4D2)) drops += 4;
	/* level rating is now primarily about treasure, danger rating is separate */
	if (drops) /* ((drops) && (p_ptr->depth >= 20)) */
	{
		int mrat = 0;
		if (drops > 2) mrat = drops/3;
		if (r_ptr->flags1 & (RF1_DROP_GREAT))
		{
			if (drops > 3) mrat += 3;
			else if (drops > 1) mrat += 2;
            else mrat += 1;
		}
		else if (r_ptr->flags1 & (RF1_DROP_GOOD))
		{
			if (drops > 3) mrat += 2;
            else mrat += 1;
		}
		if (r_ptr->flags1 & (RF1_ONLY_ITEM)) mrat += 1;
		else if ((mrat) && (r_ptr->flags1 & (RF1_ONLY_GOLD))) mrat -= 1;
		
		/* boost the rating according to a monster's drops */
        if (spccode < 20) rating += mrat;
	}

	/* Get local monster */
	n_ptr = &monster_type_body;

	/* Clean out the monster */
	(void)WIPE(n_ptr, monster_type);

	/* Save the race */
	n_ptr->r_idx = r_idx;


	/* likeliness of being awake without noticing the PC */
	/* currently no monster has both ROAM1 and ROAM2 but could use that to stack */
	/* slp is only FALSE when monster is being generated over time after dungeon creation */
	isroam = 1;
	roamflag = FALSE;
	if ((r_ptr->flags2 & (RF2_ROAM2)) || (r_ptr->flags2 & (RF2_ROAM1))) roamflag = TRUE;
	if (r_ptr->flags2 & (RF2_ROAM1)) isroam += 25;
	if (r_ptr->flags2 & (RF2_ROAM2)) isroam += 74;
	/* townspeople shouldn't just stand there */
	if ((r_ptr->flags2 & (RF2_ROAM2)) && (r_ptr->level == 0)) isroam += 16;
	else if (r_ptr->level == 0) isroam += 55;
	/* monsters who are immune to sleep shouldn't sleep */
	if ((r_ptr->flags3 & (RF3_NO_SLEEP)) && (!roamflag)) isroam += 15;
	else if (r_ptr->flags3 & (RF3_NO_SLEEP)) isroam += 5;
	/* non-aggressive monsters */
	if ((r_ptr->sleep == 255) && (isroam > 70)) isroam += 6;
	else if (r_ptr->sleep == 255) isroam += 25;
    /* spawned smonsters shouldn't always automatically be aware of the player */
    else if ((!slp) && (rand_int(100) < 33))
    {
        isroam += 20;
        spawnroam = TRUE;
    }
	/* some types of monsters rarely roam */
	if ((strchr("eglLsVwWXz$.", r_ptr->d_char)) && (!roamflag)) isroam = isroam/3;
	/* some types of monsters never roam */
	if ((!r_ptr->sleep) && (!roamflag)) isroam = 0;
	if ((strchr(",dDEjmQvZ%?!=_~", r_ptr->d_char)) && (!roamflag)) isroam = 0;
	if (spccode == 3) isroam = 0;

	/* water monsters never roam when in water */
	if (((r_ptr->flags7 & (RF7_WATER_ONLY)) || (r_ptr->flags7 & (RF7_WATER_HIDE))) &&
		(cave_feat[y][x] == FEAT_WATER))
	{
		isroam = 0;
	}

	/* is it roaming? */
	if (randint(100) < isroam)
    {
        n_ptr->roaming = 1;
        /* roams as part of a group */
        if (group) n_ptr->roaming = 11;
        /* spawned smonsters shouldn't always automatically be aware of the player */
        if (spawnroam) slp = TRUE;
    }
	else n_ptr->roaming = 0;

	/* Enforce sleeping if needed */
	/* (old: all monsters start out asleep unless it was random generation over time */
	/* or unless they have 0 in the monster alertness field in monster.txt) */
	/* now: n_ptr->csleep means hasn't noticed the player yet, not nessesarily sleeping */
	/* if n_ptr->csleep > 0 and n_ptr->roaming = 0 then monster is asleep */
	if ((slp) && ((r_ptr->sleep) || (n_ptr->roaming)))
	{
		int val = r_ptr->sleep;
		if (val < 1) val = 1;
		
		/* town monsters ignore you most of the time */
		if (!r_ptr->level) n_ptr->csleep = (val * 6) + randint(val * 8);
		/* (town versions of dungeon monsters) */
		else if (!p_ptr->depth) n_ptr->csleep = (val * 4) + randint(val * 9);
		else n_ptr->csleep = (val * 2) + randint(val * 10);
	}

	/* BLOCK_LOS monsters get the CAVE_WALL flag */
	if (r_ptr->flags7 & (RF7_BLOCK_LOS))
	{
		cave_info[y][x] |= (CAVE_WALL);
	}

	/* Assign maximal hitpoints */
	if (r_ptr->flags1 & (RF1_FORCE_MAXHP))
	{
		n_ptr->maxhp = maxroll(r_ptr->hdice, r_ptr->hside);
	}
	else
	{
		n_ptr->maxhp = damroll(r_ptr->hdice, r_ptr->hside);
	}

	/* And start out fully healthy */
	n_ptr->hp = n_ptr->maxhp;
	n_ptr->ninelives = 0;
	n_ptr->temp_death = 0;

	/* Extract the monster base speed */
	n_ptr->mspeed = r_ptr->speed;

	/* if monster is imaginary, mark it as imaginary */
	if (spccode > 20) /* spccode == 22 is hallucenatory monster */
	{
		int itype = 5;
		int telev = p_ptr->lev;
		int semireal = 3 + p_ptr->depth/10 + (badluck+1)/3;
		if (telev < 10) telev = 10;
		if ((spccode == 23) || /* summoned by summon illusion(s) monster spell */
			(spccode == 24))   /* summoned by ILLUSION_CLONE monster spell */
		{
			/* get summoner race */
			monster_race *sr_ptr = &r_info[summoner];
			semireal = 8 + sr_ptr->level/8 + badluck/2;
		}

		/* chance for semi-real illusion */
		if (rand_int(100) < semireal)
		{
			itype = 1;
			if (spccode == 24) itype = 2;
		}
		else if (spccode == 24) itype = 4;
		else if (spccode == 23) itype = 3;

		/* marks the monster as illusion (including type of illusion) */
		n_ptr->extra2 = itype; 
		/* imaginary monsters die easily */
		if ((itype == 3) || (itype == 5))
		{
			n_ptr->maxhp = (n_ptr->maxhp+3) / 4; /* 25%HP */
			if (n_ptr->maxhp > telev * 6) n_ptr->maxhp = telev * 6;
			if (n_ptr->maxhp > 150) n_ptr->maxhp = 140 + randint(55);
		}
		else if (itype == 4)
		{
			n_ptr->maxhp = (n_ptr->maxhp+9) / 5; /* 20%HP */
			/* illusory clones shouldn't have too many HP */
			if (n_ptr->maxhp > telev * 4) n_ptr->maxhp = telev * 4;
			if (n_ptr->maxhp > 100) n_ptr->maxhp = 90 + randint(45);
		}
		else if (itype == 2)
		{
			n_ptr->maxhp = n_ptr->maxhp * 3 / 10; /* 30%HP */
			if (n_ptr->maxhp > telev * 10) n_ptr->maxhp = telev * 10;
			if (n_ptr->maxhp > 180) n_ptr->maxhp = 170 + randint(45);
		}
		else if (itype == 1)
		{
			n_ptr->maxhp = (n_ptr->maxhp * 7 + 13) / 20; /* 35%HP */
			if (n_ptr->maxhp > telev * 15) n_ptr->maxhp = telev * 15;
			if (n_ptr->maxhp > 200) n_ptr->maxhp = 200 + randint(40);
		}
		n_ptr->hp = n_ptr->maxhp;
	}

	/* Some monster races are sometimes evil but not always */
	evilchance = 0;
	/* uniques never have S_EVIL1 or S_EVIL2 */
	/* sometimes evil (should never be invisible except on very high levels) */
	if (r_ptr->flags2 & (RF2_S_EVIL1)) evilchance = 22;
	/* usually evil */
	else if (r_ptr->flags2 & (RF2_S_EVIL2)) evilchance = 78;
	
	/* if it's invisible, it's evil more often so it'll be easier to find */
	if ((evilchance) && (r_ptr->flags2 & (RF2_INVISIBLE)))
    {
        if (p_ptr->depth < 60) evilchance += (61 - p_ptr->depth)/3 + 1;
    }

	/* mark the monster as eviil or not */
	if ((randint(100) < evilchance) || (r_ptr->flags3 & (RF3_EVIL))) n_ptr->evil = 1;
	else n_ptr->evil = 0;
    
    /* nonevil monsters take less notice of you */
    if ((evilchance) && (n_ptr->csleep) && (!n_ptr->evil))
    {
        if (n_ptr->csleep < r_ptr->sleep * 6) n_ptr->csleep += r_ptr->sleep * 2 + 1;
    }
    else if ((evilchance) && (n_ptr->csleep))
    {
        if (n_ptr->csleep > r_ptr->sleep * 8) n_ptr->csleep -= r_ptr->sleep * 2;
    }

    /* you've never met this monster */
	n_ptr->meet = 0;
	/* disguise mimmics */
	disguise_mimmics(n_ptr);
	
	/* hack: statues use tinvis to remember their individual descriptions */
	/* ( see describe_monster_desc() in monster1.c ) */
	if ((r_ptr->flags7 & (RF7_NONMONSTER)) && (r_ptr->flags3 & (RF3_NON_LIVING)))
	{
		/* sleep is irrevelent for NONMONSTERs so they can use this flag for something else */
		/* (slp means it's a ruined statue) */
		if ((slp) && (r_ptr->Rwater > 1)) n_ptr->tinvis = 8 + rand_int(2);
        else if (slp) n_ptr->tinvis = 26;
        /* r_ptr->Rwater > 1 means it's a fountain */
		else if (r_ptr->Rwater > 1) n_ptr->tinvis = 1 + rand_int(7); /* 1-7 */
		/* Statues in DARK_CITY are of goblins and ugly/weird things */
		else if (p_ptr->theme == 13) 
		{
			/* BLOCK_LOS statues and non-BLOCK_LOS have different descriptions */
            if (r_ptr->flags7 & (RF7_BLOCK_LOS)) n_ptr->tinvis = 18 + rand_int(8); /* 18-25 */
            else n_ptr->tinvis = 19 + rand_int(7); /* 19-25 */
		}
		/* (small) Statues in DWARF_MINE are usually of dwarves */
		else if ((p_ptr->theme == 10) && (!(r_ptr->flags7 & (RF7_BLOCK_LOS))) &&
			(randint(100) < 70)) n_ptr->tinvis = 1 + rand_int(8); /* 1-8 */
		/* Statues in the CASTLE and BARRACKS are of historical figures */
		else if ((p_ptr->theme == 8) || (p_ptr->theme == 15))
		{
			/* BLOCK_LOS statues and non-BLOCK_LOS have different descriptions */
            if (r_ptr->flags7 & (RF7_BLOCK_LOS)) n_ptr->tinvis = 1 + rand_int(7); /* 1-7 */
            else n_ptr->tinvis = 1 + rand_int(14); /* 1-14 */
		}
		/* Statues other places could be any type of statue */
		else if (r_ptr->flags7 & (RF7_BLOCK_LOS)) n_ptr->tinvis = randint(23);
        else n_ptr->tinvis = randint(22);
	}
	/* to establish which class the adventurer is */
	else if (strstr(name, "adventurer"))
	{
		n_ptr->tinvis = randint(8);
	}

	/* Hack -- small racial variety */
	if (!((r_ptr->flags1 & (RF1_UNIQUE)) || (r_ptr->maxpop == 1)))
	{
		/* Allow some small variation per monster */
		i = extract_energy[r_ptr->speed] / 10;
		if (i) n_ptr->mspeed += rand_spread(0, i);
		
#ifdef roomrunes
		/* Werebeast in a sundial room on a full moon can see the actual full moon */
		if ((r_ptr->Rsilver < -1) && (room_runes(y, x) == 10) && (p_ptr->theme == 7))
		{
			if (n_ptr->mspeed < r_ptr->speed + 2) n_ptr->mspeed += 2;
			else if (n_ptr->mspeed == r_ptr->speed + 2) n_ptr->mspeed++;
		}
#endif
	}

	/* a few monsters scale with depth */
	if ((r_ptr->flags3 & (RF3_SCALE)) && (p_ptr->depth > r_ptr->level + 5))
	{
		int maxspeedscale, scalebase = p_ptr->depth - (r_ptr->level + 4);
		/* scale HP */
		if (scalebase > 38) n_ptr->maxhp += 100 + scalebase*2 + randint(scalebase * 2 + 10);
		else if (scalebase > 30) n_ptr->maxhp += 85 + scalebase + randint(scalebase * 2);
		else if (scalebase > 25) n_ptr->maxhp += 50 + scalebase + randint(scalebase * 2);
		else if (scalebase > 20) n_ptr->maxhp += 35 + randint(scalebase * 2);
		else if (scalebase > 10) n_ptr->maxhp += 11 + randint(scalebase + 5);
		else if (scalebase > 4) n_ptr->maxhp += 5 + randint(scalebase + 1);
		else n_ptr->maxhp += randint(scalebase + 1);
		n_ptr->hp = n_ptr->maxhp;
		
		/* scale speed (never more than 129) */
		if (n_ptr->mspeed + 12 >= 129) maxspeedscale = 129 - n_ptr->mspeed;
		else maxspeedscale = 12;
		if ((scalebase > 4) && (n_ptr->mspeed < r_ptr->speed + 6)) n_ptr->mspeed += rand_int(2);
		if ((scalebase > 8) && (n_ptr->mspeed < r_ptr->speed + 6)) n_ptr->mspeed += 1;
		if ((scalebase > 12) && (n_ptr->mspeed < r_ptr->speed + 6)) n_ptr->mspeed += rand_int(2);
		if ((scalebase > 16) && (n_ptr->mspeed < r_ptr->speed + 8)) n_ptr->mspeed += 1;
		if ((scalebase > 20) && (n_ptr->mspeed < r_ptr->speed + 8)) n_ptr->mspeed += 1;
		if ((scalebase > 25) && (n_ptr->mspeed < r_ptr->speed + 9)) n_ptr->mspeed += randint(2);
		if ((scalebase > 30) && (n_ptr->mspeed < r_ptr->speed + 10)) n_ptr->mspeed += 1;
		if ((scalebase > 37) && (n_ptr->mspeed < r_ptr->speed + 11)) n_ptr->mspeed += randint(2);
		if ((scalebase > 44) && (n_ptr->mspeed < r_ptr->speed + 12)) n_ptr->mspeed += randint(2);

		/* scale alertness */
		if ((scalebase > 30) && (n_ptr->csleep > 13)) n_ptr->csleep -= 5 + randint(n_ptr->csleep - 10);
		else if ((scalebase > 30) && (n_ptr->csleep > 8)) n_ptr->csleep -= 6;
		else if ((scalebase > 20) && (n_ptr->csleep > 15)) n_ptr->csleep -= (4 + randint((n_ptr->csleep - 8)/2) );
		else if ((scalebase > 20) && (n_ptr->csleep > 9)) n_ptr->csleep -= (4 + randint(3));
		else if ((scalebase > 10) && (n_ptr->csleep > 8)) n_ptr->csleep -= 5;
	}
	/* scale nonmonsters a little bit also (to make them easier to get them out of the way early on) */
	if (r_ptr->flags7 & (RF7_NONMONSTER))
	{
		if ((p_ptr->depth < 20) && (n_ptr->maxhp > maxroll(r_ptr->hdice, r_ptr->hside)/3))
			n_ptr->maxhp = maxroll(r_ptr->hdice, r_ptr->hside)/4;
		else if ((p_ptr->depth < 35) && (n_ptr->maxhp > maxroll(r_ptr->hdice, r_ptr->hside)/2))
			n_ptr->maxhp = maxroll(r_ptr->hdice, r_ptr->hside)/3;
		else if ((p_ptr->depth >= 80) && (n_ptr->maxhp < (maxroll(r_ptr->hdice, r_ptr->hside)*3)/4))
			n_ptr->maxhp = maxroll(r_ptr->hdice, r_ptr->hside);
		else if ((p_ptr->depth > 55) && (n_ptr->maxhp < (maxroll(r_ptr->hdice, r_ptr->hside)*2)/3))
			n_ptr->maxhp = (maxroll(r_ptr->hdice, r_ptr->hside)*2)/3;
		else if ((p_ptr->depth >= 40) && (n_ptr->maxhp < maxroll(r_ptr->hdice, r_ptr->hside)/3))
			n_ptr->maxhp = maxroll(r_ptr->hdice, r_ptr->hside)/3;
		n_ptr->hp = n_ptr->maxhp;
	}

	/* this speed adjustment is handled only here and */
	/* in monster_swap() */
	if ((monster_is_slowed_by_water(n_ptr->r_idx)) &&
		(cave_feat[y][x] == FEAT_WATER))
	{
		n_ptr->mspeed -= 2;
	}

	/* Give a random starting energy */
	n_ptr->energy = (byte)rand_int(100);
	
	/* TOWNOK dungeon monsters in the town are (usually) created as */
	/* easier versions (partly to justify the lack of an XP reward) */
	if ((r_ptr->level > 0) && (!p_ptr->depth) &&
		(r_ptr->flags7 & (RF7_TOWNOK)))
	{
		int erlev = r_ptr->level * 3;
		int erlevb = r_ptr->level * 7;
		if (erlev < 10) erlev = 10;
		if (erlevb < 25) erlevb = 25;
		if (p_ptr->lev > erlevb) weakenme = 0;
		else if (p_ptr->lev > erlev) weakenme = 1;
		else weakenme = 2;
	}

	if ((spccode >= 20) || (weakenme) || (r_ptr->flags2 & (RF2_MULTIPLY)) ||
		(r_ptr->flags1 & (RF1_UNIQUE)) || (r_ptr->maxpop == 1)) 
			/* can't be a pseudo-unique */;
	/* pseudo-unique 'champion' */
	else if (r_ptr->champ > 0)
	{
		int champcc = r_ptr->champ;
		if (r_ptr->champ == 30) champcc = 25;
		if (r_ptr->champ == 50) champcc = 40;
		if (r_ptr->champ == 500) champcc = 500;
		if (r_ptr->champ == 800) champcc = 750;
		if (r_ptr->champ == 850) champcc = 800;
		if (r_ptr->champ == 880) champcc = 850;
		if (r_ptr->level > p_ptr->depth) champcc += 50 + (r_ptr->level - p_ptr->depth);
		if (randint(champcc) == 1)
		{
			int die = rand_int(100 - badluck);
			/* give the monster a name */
			champ_name(n_ptr);
			/* and some bonuses */
			if (r_ptr->flags1 & (RF1_FORCE_MAXHP))
			{
				if (n_ptr->maxhp < 500) n_ptr->maxhp = n_ptr->maxhp * 3 / 2;
				else if (n_ptr->maxhp < 601) n_ptr->maxhp = 745 + randint(10);
				else n_ptr->maxhp = n_ptr->maxhp * 5 / 4;
			}
			else if (maxroll(r_ptr->hdice, r_ptr->hside) > n_ptr->maxhp + 500)
				n_ptr->maxhp = ((2 * maxroll(r_ptr->hdice, r_ptr->hside)) + n_ptr->maxhp) / 3;
			else n_ptr->maxhp = maxroll(r_ptr->hdice, r_ptr->hside) + r_ptr->level/2;
			n_ptr->hp = n_ptr->maxhp;
			if (n_ptr->mspeed < r_ptr->speed) n_ptr->mspeed += 1 + randint(3);
			else if (n_ptr->mspeed < r_ptr->speed + 1 + randint(5)) n_ptr->mspeed += 2;
			if (n_ptr->csleep > r_ptr->sleep * 6) n_ptr->csleep -= r_ptr->sleep * 2;
			/* mark it as a pseudo-unique */
			n_ptr->champ = 1;
			/* make it more unique */
			if ((die < 1) && (r_ptr->level < p_ptr->depth - 2))
			{
				n_ptr->champ = 11; /* a little of all of the below bonuses */
				if (n_ptr->mspeed < r_ptr->speed + 5) n_ptr->mspeed++;
				if (n_ptr->csleep > r_ptr->sleep * 5) n_ptr->csleep -= (r_ptr->sleep + 1);
			}
			else if (die < 12) n_ptr->champ = 5; /* cast faster & higher ac */
			else if (die < 24) n_ptr->champ = 2; /* cast faster */
			else if (die < 33) n_ptr->champ = 6; /* more melee damage b (bigger +dam) */
			else if (die < 42) n_ptr->champ = 4; /* more melee damage a (also slight +ac) */
			else if (die < 54) n_ptr->champ = 3; /* higher ac */
			else if (die < 64) /* additional speed & alterness boost */
			{
				if (n_ptr->mspeed < r_ptr->speed + 3) n_ptr->mspeed += 1 + randint(3);
				else if (n_ptr->mspeed < r_ptr->speed + 5 + rand_int(3)) n_ptr->mspeed += 1;
				if (n_ptr->csleep > r_ptr->sleep * 4) n_ptr->csleep -= r_ptr->sleep * 2;
				n_ptr->champ = 7;
			}
			/* (Do these things when it comes into effect cause it isn't held in m_ptr) */
			if (cheat_hear) msg_format("Pseudo-unique (%s).", name);
		}
	}
	
	
    if (weakenme == 2) /* for dungeon monsters in the town */
    {
		/* monster (almost) never has more than its race's average amount of HP */
        if (n_ptr->maxhp > ((r_ptr->hdice * r_ptr->hside) + r_ptr->hdice)/2 + badluck)
		{
			int omax = n_ptr->maxhp;
			n_ptr->maxhp = ((r_ptr->hdice * r_ptr->hside) + r_ptr->hdice)/2;
			if ((badluck) && (n_ptr->maxhp + badluck <= omax)) 
				n_ptr->maxhp += randint(badluck);
			n_ptr->hp = n_ptr->maxhp;
		}
		/* reduced speed */
		if (n_ptr->mspeed > 103) n_ptr->mspeed -= 5;
		else if (n_ptr->mspeed > 99) n_ptr->mspeed = 99;
		/* less alert */
		if (n_ptr->csleep) n_ptr->csleep += 40;
		else n_ptr->csleep += 10;
	}
	/* don't weaken TOWNOK monsters as much if PC is tough */
	else if (weakenme == 1)
    {
		/* monster never has much more than its race's average amouint of HP */
        if (n_ptr->maxhp > (((r_ptr->hdice * r_ptr->hside) + r_ptr->hdice) * 2) / 3 + badluck)
		{
			int omax = n_ptr->maxhp;
			n_ptr->maxhp = (((r_ptr->hdice * r_ptr->hside) + r_ptr->hdice) * 2) / 3;
			if ((badluck) && (n_ptr->maxhp + badluck <= omax)) 
				n_ptr->maxhp += randint(badluck);
			n_ptr->hp = n_ptr->maxhp;
		}
		/* reduced speed */
		if (n_ptr->mspeed >= 115) n_ptr->mspeed -= 4;
		else if (n_ptr->mspeed >= 110) n_ptr->mspeed -= 3;
		else if (n_ptr->mspeed >= 103) n_ptr->mspeed -= 2;
		/* less alert */
		if (n_ptr->csleep) n_ptr->csleep += 20;
		else n_ptr->csleep += 4;
	}

	/* Force monster to wait for player */
	if ((r_ptr->flags1 & (RF1_FORCE_SLEEP)) || (weakenme) ||
		(distance(y, x, p_ptr->py, p_ptr->px) < 3))
	{
		/* Monster is still being nice */
		n_ptr->mflag |= (MFLAG_NICE);

		/* Optimize -- Repair flags */
		repair_mflag_nice = TRUE;
	}

	/* Place the monster in the dungeon */
	if (!monster_place(y, x, n_ptr)) return (FALSE);

	/* Success */
	return (TRUE);
}


/*
 * Maximum size of a group of monsters
 */
#define GROUP_MAX	31


/*
 * Attempt to place a "group" of monsters around the given location
 */
static bool place_monster_group(int y, int x, int r_idx, bool slp)
{
	monster_race *r_ptr = &r_info[r_idx];

	int old, n, i;
	int tag = 0;
	int biggroup = 11;
	int total, extra = 0;
	int hack_n;

	byte hack_y[GROUP_MAX];
	byte hack_x[GROUP_MAX];


	/* Pick a group size */
	total = randint(13);
	/* slightly smaller groups in caverns */
	if ((p_ptr->speclev == 1) || (p_ptr->speclev == 2)) total = randint(10);
	if (r_ptr->flags2 & RF2_FRIEND1) biggroup = 9;

	/* Hard monsters, small groups */
	if (r_ptr->level > p_ptr->depth)
	{
		extra = r_ptr->level - p_ptr->depth;
		extra = randint(extra);

		/* Hack -- limit group reduction */
		if (extra > 12) extra = 12;

		extra = 0 - extra;
	}

	/* Easy monsters, large groups */
	else if (r_ptr->level < p_ptr->depth-1)
	{
		extra = p_ptr->depth - r_ptr->level;
		if ((r_ptr->flags2 & RF2_FRIEND1) && (!(r_ptr->flags3 & RF3_SCALE))) 
			extra = randint((extra+1)/2);
		/* slightly smaller groups in caverns */
		else if ((p_ptr->speclev == 1) || (p_ptr->speclev == 2)) 
			extra = randint((extra*3)/4);
		else extra = randint(extra);
	}

	/* Modify the group size */
	total += extra;

	/* Minimum size */
	if (total < 1) total = 1;

	/* Maximum size (31, set just above this function) */
	if (total > GROUP_MAX) total = GROUP_MAX;

	/* Pairs (usually) */
	if ((r_ptr->flags1 & RF1_FRIEND) && (total > 2))
	{
       	if ((extra > 9) && (randint(100) < 60)) total = 4;
       	else if ((extra > 7) && (randint(100) < 25)) total = 3;
       	else total = 2;
    }

	/* small groups (max max is 16) */
	if ((r_ptr->flags2 & RF2_FRIEND1) && (total > 6))
	{
       	/* reduction */
        if ((total > 7) && (randint(100) < 9) && (goodluck)) total = (total / 2) + rand_int(3);
       	else if ((total > 7) && (randint(100) < 41)) total = ((total * 3) / 4);
       	else if (total > 8) total = 4 + randint(total / 2);
       	else total = 3 + randint(3);
       	/* caps */
	    if ((r_ptr->level > p_ptr->depth) && (total > 6)) total = 5 + rand_int(3);
	    else if ((r_ptr->level > p_ptr->depth - 4) && (total > 8)) total = 6 + randint(3);
	    else if ((r_ptr->level > p_ptr->depth - 8) && (total > 10)) total = 7 + randint(4);
	    else if ((total > 11) && (badluck < 3)) total = 8 + randint(total-9);
	    else if (total > 15) total = 12 + randint(4);
	}
    /* group monsters shouldn't be all by themselves */
	else if ((r_ptr->flags1 & RF1_FRIENDS) || (r_ptr->flags2 & RF2_FRIEND1))
	{
	    if (total < 2) total = 2;
	}

	/* prevent huge groups in vaults */
	if ((cave_info[y][x] & (CAVE_ICKY)) && (total >= biggroup))
	{
		if (total > biggroup+7)
        {
			total -= (total-biggroup-2)/2;
			if (total > biggroup+7) total = biggroup+7;
		}
		else total -= (total-biggroup-1)/2;
	}
	else if ((cave_info[y][x] & (CAVE_ICKY)) && (total >= biggroup-2)) total--;

	/* Save the rating */
	old = rating;

	/* Start on the monster */
	hack_n = 1;
	hack_x[0] = x;
	hack_y[0] = y;

	/* maybe add a tribe shaman to the group */
	/* (or something else which hangs out with that type of monster group) */
	if ((r_ptr->flags3 & RF3_ORC) && (p_ptr->depth > 9))
	{
		int die = randint(150 - badluck);
		if ((die < 4) && (p_ptr->depth > 24)) tag = 388; /* cave ogre L26 */
		else if ((die < 5) && (p_ptr->depth > 13)) tag = 193; /* ogre L11 */
		else if ((die < 9) && (p_ptr->depth > 14)) tag = 235; /* warg L14 */
		else if ((die < 11) && (p_ptr->depth > 22)) tag = 370; /* stone troll L24 */
		else if (die < 48) tag = 155; /* orc shaman */
		else if ((die < 55) && (p_ptr->depth > 12)) tag = 187; /* fire orc */
	}
	else if ((strchr("x", r_ptr->d_char)) && (total > 2))
	{
		int die = randint(70 + p_ptr->depth);
		if (die > 135) tag = 547; /* riverflow gargoyle */
		else if (die > 97) tag = 556; /* margoyle shaman */
	}
	else if ((strchr("O", r_ptr->d_char)) && (total > 2))
	{
		int die = randint(70 + p_ptr->depth);
		if (die > 80) tag = 397; /* ogre shaman */
	}
	else if (strchr("T", r_ptr->d_char))
	{
		int die = randint(70 + p_ptr->depth);
		if ((die > 90) && (p_ptr->depth > 29)) tag = 482; /* undying troll */
	}
	else if (r_idx == 263) /* winged monkeys */
	{
		int die = randint(90 + (p_ptr->depth/3) + badluck);
		/* much bigger chance if in-theme */
		if (p_ptr->theme == 13)	die = 55 + randint(45 + (p_ptr->depth/2) + badluck);
		if (die > 93) tag = 264; /* elete winged monkey (otherwise THEME_ONLY) */
	}
	else if (r_idx == 407) /* bats of gorgoroth */
	{
		int die = randint(75 + (p_ptr->depth/3) + badluck);
		if (die > 96) tag = 652; /* winged horror (only deep) */
	}
	/* haradrim with mumaks */
	else if (r_idx == 859)
	{
		if (randint(70 + p_ptr->depth*2) > 60) tag = 860; /* captain */
		else tag = 506;
	}

	/* place trees differently (this helps keep trees from blocking paths) */
	if (r_ptr->flags7 & (RF7_NONMONSTER))
	{
		int spotx, spoty, yb, xb, tries = 0;
		int hitwall = 0;
		for (n = 0; n < total; n++, tries++)
		{
			/* get a spot */
			if (get_nearby(y, x, &spoty, &spotx, 6))
			{
				/* enforce success */
				if (place_monster_one(spoty, spotx, r_idx, slp, TRUE, 0))
				{
					/* Scan adjacent grids */
					for (i = 0; i < 8; i++)
					{
						/* Extract the location */
						yb = y + ddy_ddd[i];
						xb = x + ddx_ddd[i];
				
						/* next to a wall */
						if (cave_feat[yb][xb] >= FEAT_MAGMA) hitwall += 1;
					}
				}
				else if (tries < 90) n--; /* failed to place */
				/* if we're in a wide open cavern, maybe make more trees */
				if ((p_ptr->speclev == 1) && (hitwall < 2) && 
					(total - n < 3) && (randint(100) < 35)) n--;
			}
			/* try again in case of failure (but not forever) */
			else if (tries < 90) n--;
		}
		return TRUE;
	}

	/* Puddle monsters, breadth first, up to total */
	for (n = 0; (n < hack_n) && (hack_n < total); n++)
	{
		/* Grab the location */
		int hx = hack_x[n];
		int hy = hack_y[n];
		bool mumaks = FALSE;

		/* Check each direction, up to total */
		for (i = 0; (i < 8) && (hack_n < total); i++)
		{
			int mx = hx + ddx_ddd[i];
			int my = hy + ddy_ddd[i];

			/* Walls and Monsters block flow */
			if (!cave_can_occupy_bold(my, mx)) continue;
				
			/* haradrim often have more than just one mumak */
            if ((r_idx == 859) && (randint(100) < 11) && (!mumaks))
            {
				int tagb = tag;
                if (tag == 860) tagb = 608;
                if (place_monster_one(my, mx, tagb, slp, TRUE, 0))
				{
					/* Add it to the "hack" set */
					hack_y[hack_n] = my;
					hack_x[hack_n] = mx;
					hack_n++;
					if (tagb == 608) mumaks = TRUE;
					continue;
				}
    	    }

			/* maybe add a tribe shaman to the group */
			if ((hack_n == (total - 1)) && (tag))
			{
				if (place_monster_one(my, mx, tag, slp, TRUE, 0))
				{
					/* Add it to the "hack" set */
					hack_y[hack_n] = my;
					hack_x[hack_n] = mx;
					hack_n++;
					continue;
				}
			}

			/* Attempt to place another monster */
			if (place_monster_one(my, mx, r_idx, slp, TRUE, 0))
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
	
	/* if OOD or near OOD, tags (shamans) increase rating */
    if ((r_ptr->level > p_ptr->depth) && (tag)) rating += randint(2);
    else if ((r_ptr->level > p_ptr->depth - 3) && (tag)) rating += rand_int(2);

	/* Success */
	return (TRUE);
}


/*
 * Attempt to place a monster of the given race at the given location
 *
 * Note that certain monsters are now marked as requiring "friends".
 * These monsters, if successfully placed, and if the "grp" parameter
 * is TRUE, will be surrounded by a group of identical monsters.
 *
 * Note that certain monsters are now marked as requiring an "escort",
 * which is a collection of monsters with similar "race" but lower level.
 * Some monsters induce a fake "group" flag on their escorts.
 *
 * Note the bizarre use of non-recursion to prevent annoying output
 * when running a code profiler.
 *
 * Note the use of the new "monster allocation table" code to restrict
 * the "get_mon_num()" function to "legal" escort types.
 * 
 * see place_monster_one for spccodes.
 */
bool place_monster_aux_real(int y, int x, int r_idx, bool slp, int grp, int spccode)
{
	int i, place;
	monster_race *r_ptr = &r_info[r_idx];

	/* maximum population (monster is extinct) */
	/* should rarely happen: in all functions that get_mon_num is called, */
	/* it retries when it chooses an extinct monster. */
	if ((r_ptr->curpop + r_ptr->cur_num >= r_ptr->maxpop) && (r_ptr->maxpop) &&
	/* debug summon specific command allows you to bypass maximum population */
		(!(p_ptr->noscore & NOSCORE_DEBUG))) return (FALSE);

	/* Place one monster, or fail */
	if (!place_monster_one(y, x, r_idx, slp, FALSE, spccode)) return (FALSE);

	/* Require the "group" flag */
	if (!grp) return (TRUE);


	/* Friends for certain monsters */
	if ((r_ptr->flags1 & (RF1_FRIENDS)) || (r_ptr->flags1 & (RF1_FRIEND)) || 
		(r_ptr->flags2 & (RF2_FRIEND1)) || (grp == 2))
	{
		/* Attempt to place a group (if allowed) */
		(void)place_monster_group(y, x, r_idx, slp);
	}


	/* Escorts for certain monsters */
	if ((r_ptr->flags1 & (RF1_ESCORT)) || (r_ptr->flags2 & (RF2_ESCORT1)))
	{
 		/* how many escorts to try to place */
		if ((r_ptr->flags2 & (RF2_ESCORT1)) && (r_ptr->level < p_ptr->depth - 9)) 
			place = 11 + randint(9);
		else if ((r_ptr->flags2 & (RF2_ESCORT1)) && (r_ptr->level < p_ptr->depth - 4)) 
			place = 8 + randint(8);
		else if (r_ptr->flags2 & (RF2_ESCORT1)) place = 7 + randint(6);
		else if (r_ptr->level > p_ptr->depth) place = 34 + randint(10);
		else place = 44 + randint(6);
 
		/* Try to place several escorts */
		for (i = 0; i < place; i++)
		{
			int nx, ny, z, d = 3;
			if ((d == 3) && (i > 40) && (randint(100) < 10)) d++;

			/* Pick a location */
			scatter(&ny, &nx, y, x, d, 0);

			/* Require empty grids */
			if (!cave_can_occupy_bold(ny, nx))
			{
				int spoty, spotx;
				if (get_nearby(ny, nx, &spoty, &spotx, 4))
				{
					ny = spoty;
					nx = spotx;
				}
				else
				{
					/* try again if less total tries */
					if ((place < 34) && (randint(100) < 70)) i -= 1;
					continue;
				}
			}

			/* Set the escort index */
			place_monster_idx = r_idx;

			/* Set the escort hook */
			get_mon_num_hook = place_monster_okay;

			/* Prepare allocation table */
			get_mon_num_prep();


			/* Pick a random race */
			z = get_mon_num(r_ptr->level, FALSE);


			/* Remove restriction */
			get_mon_num_hook = NULL;

			/* Prepare allocation table */
			get_mon_num_prep();

			/* Handle failure */
			if (!z) break;

			/* Place a single escort */
			(void)place_monster_one(ny, nx, z, slp, TRUE, 0);

			/* Place a "group" of escorts if needed */
			if ((r_info[z].flags1 & (RF1_FRIENDS)) ||
			    (r_ptr->flags1 & (RF1_ESCORTS)) ||
                ((r_info[z].flags2 & (RF2_FRIEND1)) && (randint(100) < 22)))
			{
				/* Place a group of monsters */
				(void)place_monster_group(ny, nx, z, slp);
			}
		}
	}


	/* Success */
	return (TRUE);
}


/* call the real function with an added parameter (and convert grp to int) */
bool place_monster_aux(int y, int x, int r_idx, bool slp, bool grp)
{
	int grpb = 0;
	if (grp) grpb = 1;
	if (!place_monster_aux_real(y, x, r_idx, slp, grpb, 0)) return (FALSE);
	else return (TRUE);
	/* return place_monster_aux_real(y, x, r_idx, slp, grp, 0); */
}
/* call the real function with an added parameter (and convert grp to int) */
bool place_monster_aux_awake(int y, int x, int r_idx, bool slp, bool grp)
{
	int grpb = 0;
	if (grp) grpb = 1;
	if (!place_monster_aux_real(y, x, r_idx, slp, grpb, 3)) return (FALSE);
	else return (TRUE);
	/* return place_monster_aux_real(y, x, r_idx, slp, grp, 0); */
}
/* call the real function with an added parameter (and convert grp to int) */
bool place_monster_aux_pit(int y, int x, int r_idx, bool slp, bool grp)
{
	int grpb = 0;
	if (grp) grpb = 1;
	if (!place_monster_aux_real(y, x, r_idx, slp, grpb, 4)) return (FALSE);
	else return (TRUE);
	/* return place_monster_aux_real(y, x, r_idx, slp, grp, 0); */
}
/* call the real function with an added parameter (and convert grp to int) */
bool place_monster_aux_illu(int y, int x, int r_idx, bool slp, bool grp, int spccode)
{
	int grpb = 0;
	if (grp) grpb = 1;
	if (!place_monster_aux_real(y, x, r_idx, slp, grpb, spccode)) return (FALSE);
	else return (TRUE);
	/* return place_monster_aux_real(y, x, r_idx, slp, grp, 0); */
}

/*
 * Hack -- attempt to place a monster at the given location
 *
 * Attempt to find a monster appropriate to the "monster_level"
 */
bool place_monster(int y, int x, bool slp, bool grp, bool vault)
{
	int r_idx, tries = 0;
	monster_race *r_ptr;
	
	/* paranoia */
	if ((p_ptr->depth > 0) && (monster_level <= 0)) monster_level = p_ptr->depth;

	/* keeps trying until it gets a valid r_idx */
	while (tries < 90)
	{
		tries++;
		/* separate function now to choose a town monster */
		/* (monster_level is always 0 when initially placing town residents) */
		if (!p_ptr->depth)
		{
			/* Pick a monster */
			r_idx = get_mon_num_town();
		}
		/* occationally force an especially appropriate themed monster */
		/* this will rarely cause an unusually out-of-depth monster */
		/* or very low level monster in a deep depth */
		else if ((p_ptr->theme) && (rand_int(100) < 6))
		{
			r_idx = get_mon_force_theme(monster_level);
		}
		else
		{
			/* Pick a monster */
			r_idx = get_mon_num(monster_level, vault);
		}

		/* Handle failure */
		if (!r_idx) continue;

		r_ptr = &r_info[r_idx];

		/* maximum population */
		if ((r_ptr->curpop + r_ptr->cur_num >= r_ptr->maxpop) && (r_ptr->maxpop)) continue;

		/* don't create WATER_ONLY monsters in a space without water */
		/* (check this here to allow for 2nd tries to get_mon_num) */
		if ((r_ptr->flags7 & (RF7_WATER_ONLY)) &&
			(!(cave_feat[y][x] == FEAT_WATER)))
		{
			/* (this would be naiads in an FFOREST or water moccasins in a SWAMP only) */
			/* pretend that the puddle isn't in a vault even if it is */
			/* (so it'll be big enough to hold all the water moccasins in the group) */
			/* (if leaving is true, then this chance to create a puddle happens in place_monster_one) */
            if ((p_ptr->theme) && (theme_okay(r_idx, 0, vault)) && 
				(randint(100) < 34) && (!p_ptr->leaving))
	               place_puddle(y, x, FALSE);
            /* try again */
			else continue;
		}

		/* otherwise accept */
		break;
	}

	/* Handle failure */
	if (!r_idx) return FALSE;

	/* Attempt to place the monster */
	if (place_monster_aux(y, x, r_idx, slp, grp)) return (TRUE);

	/* Oops */
	return (FALSE);
}



/*
 * XXX XXX XXX Player Ghosts are such a hack, they have been completely
 * removed.
 *
 * An idea for reintroducing them is to create a small number of
 * "unique" monsters which will serve as the "player ghosts".
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
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x;
	int	attempts_left = 10000;

	/* Find a legal, distant, unoccupied, space */
	while (--attempts_left)
	{
		/* Pick a location */
		y = rand_int(DUNGEON_HGT);
		x = rand_int(DUNGEON_WID);

		/* Require "naked" floor grid */
		if (!cave_can_occupy_bold(y, x)) continue;
		
		/* don't place monsters in shop doorways (need this when we have trees as monsters) */
		/* (trees are no longer placed as monsters) */
        if ((cave_feat[y][x] >= FEAT_SHOP_HEAD) && (cave_feat[y][x] <= FEAT_SHOP_TAIL))
           continue;

		/* Accept far away grids */
		if (distance(y, x, py, px) > dis) break;
	}

	if (!attempts_left)
	{
		if (cheat_xtra || cheat_hear)
		{
			msg_print("Warning! Could not allocate a new monster.");
		}

		return FALSE;
	}
	
	/* Attempt to place the monster, allow groups */
	if (place_monster(y, x, slp, TRUE, FALSE)) return (TRUE);

	/* Nope */
	return (FALSE);
}



/*
 * Place a monster (of the specified "type") near the given
 * location.  Return TRUE if a monster was actually summoned.
 *
 * We will attempt to place the monster up to 10 times before giving up.
 *
 * Note: SUMMON_UNIQUE and SUMMON_WRAITH (XXX) will summon Uniques
 * Note: SUMMON_HI_UNDEAD and SUMMON_HI_DRAGON may summon Uniques
 * Note: None of the other summon codes will ever summon Uniques.
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
 *
 * mode == 0 is normal
 * mode == 1 doesn't allow groups.
 * mode == 2 turns type into r_idx of monster to summon (really hacky, also no groups)
 * mode > 20 passes illusory monster spccode to place_monster_one()
 */
static bool summon_specific_really(int y1, int x1, int lev, int type, int mode)
{
	int i, x, y, r_idx, tries = 0;
	monster_race *r_ptr;
	int sumlev;
	bool nwater = FALSE;

	/* Look for a location */
	for (i = 0; i < 22; ++i)
	{
		/* Pick a distance */
		int d = (i / 15) + 1;

		/* Pick a location */
		scatter(&y, &x, y1, x1, d, 0);

		/* Only check a circular area */
		/* (be sure to use the same criteria as summon_possible() ) */
		if (distance(y1, x1, y, x) > 2) continue;

		/* Require empty floor grid */
		if (!cave_can_occupy_bold(y, x)) continue;

		/* Require line of sight */
		if (!los(y1, x1, y, x))

		/* Hack -- no summon on glyph of warding */
		if (cave_feat[y][x] == FEAT_GLYPH) continue;

		/* Okay */
		break;
	}

	/* Failure */
	if (i == 22) return (FALSE);
	
	if (mode == 2) /* mode 2 summons a specific r_idx (no groups) */
	{
		if (!place_monster_aux_awake(y, x, type, FALSE, FALSE)) return (FALSE);
		else return (TRUE);
	}
	if (mode == 24) /* mode 24 is for ILLUSION_CLONE spell */
	{
		if (!place_monster_aux_illu(y, x, summoner, FALSE, FALSE, mode)) return (FALSE);
		else return (TRUE);
	}

	/* Save the "summon" type */
	summon_specific_type = type;


	/* Require "okay" monsters */
	get_mon_num_hook = summon_specific_okay;

	/* Prepare allocation table */
	get_mon_num_prep();

	/* keeps trying until it gets a valid r_idx */
	while (tries < 90)
	{
		tries++;

		/* Pick a monster, using the level calculation */
		if (lev > p_ptr->depth) sumlev = (p_ptr->depth + lev) / 2 + 4;
		else if (lev+1 < p_ptr->depth) sumlev = (p_ptr->depth + lev) / 2 + 2;
		else sumlev = lev;
		r_idx = get_mon_num(sumlev, FALSE);

		/* Handle failure */
		if (!r_idx) continue;

		r_ptr = &r_info[r_idx];
		
		/* maximum population */
		if ((r_ptr->curpop + r_ptr->cur_num >= r_ptr->maxpop) && (r_ptr->maxpop)) continue;

		/* don't create WATER_ONLY monsters in a space without water */
		/* (check this here to allow for 2nd tries to get_mon_num) */
		if ((r_ptr->flags7 & (RF7_WATER_ONLY)) &&
			(!(cave_feat[y][x] == FEAT_WATER)))
		{
			/* try again */
			continue;
		}
		
		/* forbid invisibile illusions (because that doesn't make any sense) */
		if ((mode > 20) && (r_ptr->flags2 & (RF2_INVISIBLE))) continue;

		/* otherwise accept */
		break;
	}

	/* Remove restriction */
	get_mon_num_hook = NULL;

	/* Prepare allocation table */
	get_mon_num_prep();


	/* Handle failure */
	if (!r_idx) return (FALSE);

	if (mode == 1)
	{
       /* Attempt to place the monster (don't allow groups) */
	   if (!place_monster_aux_awake(y, x, r_idx, FALSE, FALSE)) return (FALSE);
    }
	else if (mode > 20) /* for illusion monsters */
	{
		if (mode > 40)
		{
			/* Attempt to place the monster (don't allow groups) */
			if (!place_monster_aux_illu(y, x, r_idx, FALSE, FALSE, mode-20)) return (FALSE);
		}
		else
		{
			/* Attempt to place the monster (allow groups) */
			if (!place_monster_aux_illu(y, x, r_idx, FALSE, FALSE, mode)) return (FALSE);
		}
    }
    else
	{
       /* Attempt to place the monster (awake, allow groups) */
	   if (!place_monster_aux_awake(y, x, r_idx, FALSE, TRUE)) return (FALSE);
    }

	/* Success */
	return (TRUE);
}

/* just call the real function */
bool summon_specific(int y1, int x1, int lev, int type)
{
   return summon_specific_really(y1, x1, lev, type, 0);
}

/*
 * cheap way to make TMD_WITCH not allow groups
 * (too much trouble to change all the calls to summon_specific to add a TRUE or FALSE)
 */
bool summon_nogroups(int y1, int x1, int lev, int type)
{
   return summon_specific_really(y1, x1, lev, type, 1);
}

/*
 * summons illusion monsters
 */
bool summon_illusion(int y1, int x1, int lev, int type, int spccode)
{
	return summon_specific_really(y1, x1, lev, type, spccode);
}

/*
 * summons a specific r_idx (lev is ignored)
 */
bool summon_really_specific(int y1, int x1, int lev, int ridx)
{
   return summon_specific_really(y1, x1, lev, ridx, 2);
}


/*
 * Let the given monster attempt to reproduce.
 *
 * Note that "reproduction" REQUIRES empty space.
 */
bool multiply_monster(int m_idx)
{
	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	int i, y, x;

	bool result = FALSE;
	
	/* only happens when cloning: no MULTIPLY monsters have a max population */
	if ((r_ptr->curpop + r_ptr->cur_num >= r_ptr->maxpop) && (r_ptr->maxpop)) return FALSE;

	/* Try up to 18 times */
	for (i = 0; i < 18; i++)
	{
		int d = 1;

		/* Pick a location */
		scatter(&y, &x, m_ptr->fy, m_ptr->fx, d, 0);

		/* pass_door monsters (silver moss) can multiply into a door space */
		if ((((cave_feat[y][x] >= FEAT_DOOR_CLOSE) &&
		          (cave_feat[y][x] <= FEAT_DOOR_STUCK)) ||
		          (cave_feat[y][x] == FEAT_SECRET) ||
				  (cave_feat[y][x] == FEAT_SMRUBBLE)) &&
  				  (r_ptr->flags2 & (RF2_PASS_DOOR))) /* okay */;

		/* Require an "empty" floor grid */
		else if (!cave_can_occupy_bold(y, x)) continue;

		/* Create a new monster (awake, no groups) */
		result = place_monster_aux(y, x, m_ptr->r_idx, FALSE, FALSE);

		/* Done */
		break;
	}

	/* Result */
	return (result);
}



/*
 * Dump a message describing a monster's reaction to damage
 */
void message_pain(int m_idx, int dam)
{
	long oldhp, newhp, tmp;
	int percentage;

	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	char m_name[80];

	/* Get the monster name */
	monster_desc(m_name, sizeof(m_name), m_ptr, 0);

	/* Notice non-damage */
	if (dam == 0)
	{
		if (!(r_ptr->flags7 & (RF7_NONMONSTER))) msg_format("%^s is unharmed.", m_name);
		return;
	}
	
	/* NONMONSTERs don't feel pain */
	if (r_ptr->flags7 & (RF7_NONMONSTER))
	{
		msg_format("%^s is damaged.", m_name);
		return;
	}

	/* Note -- subtle fix -CFT */
	newhp = (long)(m_ptr->hp);
	oldhp = newhp + (long)(dam);
	tmp = (newhp * 100L) / oldhp;
	percentage = (int)(tmp);


	/* Jelly's, Mold's, Vortex's, Quthl's */
	if (strchr("jmvQJwe,$", r_ptr->d_char))
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
    
   	/* monsters which don't feel pain (ignores) (undead, golems, and stupid trees) */
	else if ((strchr("szgW", r_ptr->d_char)) || 
		((strchr("E", r_ptr->d_char)) && (r_ptr->flags2 & (RF2_STUPID))))
	{
		if (percentage > 95)
			msg_format("%^s ignores the attack.", m_name);
		else if (percentage > 75)
			msg_format("%^s is slightly damaged.", m_name);
		else if (percentage > 50)
			msg_format("%^s is damaged.", m_name);
		else if (percentage > 35)
			msg_format("%^s is severely damaged.", m_name);
		else if (percentage > 20)
			msg_format("%^s has deep gashes but doesn't notice.", m_name);
		else if (percentage > 10)
			msg_format("%^s ignores the loss of its limbs.", m_name);
		else
			msg_format("%^s is literally falling apart.", m_name);
	}

	/* One type of monsters (ignore,squeal,shriek) */
	else if (strchr("FIMRSXabclqrt", r_ptr->d_char))
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

	monster_type *m_ptr = &mon_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	/* Not allowed to learn */
	if (!adult_ai_learn) return;

	/* Too stupid to learn anything */
	if (r_ptr->flags2 & (RF2_STUPID)) return;

	/* Not intelligent, only learn sometimes */
	if (!(r_ptr->flags2 & (RF2_SMART)) && (rand_int(100) < 50)) return;


	/* XXX XXX XXX */

	/* Analyze the knowledge */
	switch (what)
	{
		case DRS_FREE:
		{
			if (p_ptr->free_act) m_ptr->smart |= (SM_IMM_FREE);
			break;
		}

		case DRS_MANA:
		{
			if ((cp_ptr->flags & CF_HEAVY_BONUS) || (cp_ptr->flags & CF_KNIGHT) || (!p_ptr->msp))
			   m_ptr->smart |= (SM_IMM_MANA);
			break;
		}

		case DRS_RES_ACID:
		{
			if (p_ptr->resist_acid) m_ptr->smart |= (SM_RES_ACID);
			if (p_ptr->timed[TMD_OPP_ACID]) m_ptr->smart |= (SM_OPP_ACID);
			if (p_ptr->immune_acid) m_ptr->smart |= (SM_IMM_ACID);
			break;
		}

		case DRS_RES_ELEC:
		{
			if (p_ptr->resist_elec) m_ptr->smart |= (SM_RES_ELEC);
			if (p_ptr->timed[TMD_OPP_ELEC]) m_ptr->smart |= (SM_OPP_ELEC);
			if (p_ptr->immune_elec) m_ptr->smart |= (SM_IMM_ELEC);
			break;
		}

		case DRS_RES_FIRE:
		{
			if (p_ptr->resist_fire) m_ptr->smart |= (SM_RES_FIRE);
			if (p_ptr->timed[TMD_OPP_FIRE]) m_ptr->smart |= (SM_OPP_FIRE);
			if (p_ptr->immune_fire) m_ptr->smart |= (SM_IMM_FIRE);
			break;
		}

		case DRS_RES_COLD:
		{
			if (p_ptr->resist_cold) m_ptr->smart |= (SM_RES_COLD);
			if (p_ptr->timed[TMD_OPP_COLD]) m_ptr->smart |= (SM_OPP_COLD);
			if (p_ptr->immune_cold) m_ptr->smart |= (SM_IMM_COLD);
			break;
		}

		case DRS_RES_POIS:
		{
			if (p_ptr->resist_pois) m_ptr->smart |= (SM_RES_POIS);
			if (p_ptr->timed[TMD_OPP_POIS]) m_ptr->smart |= (SM_OPP_POIS);
			break;
		}

		case DRS_RES_FEAR:
		{
			if (p_ptr->resist_fear) m_ptr->smart |= (SM_RES_FEAR);
			break;
		}

		case DRS_RES_CHARM:
		{
		    /* if (p_ptr->resist_charm) m_ptr->smart |= (SM_RES_CHARM); */
			break;
		}
		
		case DRS_RES_LITE:
		{
			if (p_ptr->resist_lite) m_ptr->smart |= (SM_RES_LITE);
			break;
		}

		case DRS_RES_DARK:
		{
			if (p_ptr->resist_dark) m_ptr->smart |= (SM_RES_DARK);
			break;
		}

		case DRS_RES_BLIND:
		{
			if (p_ptr->resist_blind) m_ptr->smart |= (SM_RES_BLIND);
			break;
		}

		case DRS_RES_CONFU:
		{
			if (p_ptr->resist_confu) m_ptr->smart |= (SM_RES_CONFU);
			break;
		}

		case DRS_RES_SOUND:
		{
			if (p_ptr->resist_sound) m_ptr->smart |= (SM_RES_SOUND);
			break;
		}

		case DRS_RES_SHARD:
		{
			if (p_ptr->resist_shard) m_ptr->smart |= (SM_RES_SHARD);
			break;
		}

		case DRS_RES_NEXUS:
		{
			if (p_ptr->resist_nexus) m_ptr->smart |= (SM_RES_NEXUS);
			break;
		}

		case DRS_RES_NETHR:
		{
			if (p_ptr->resist_nethr) m_ptr->smart |= (SM_RES_NETHR);
			break;
		}

		case DRS_RES_CHAOS:
		{
			if (p_ptr->resist_chaos) m_ptr->smart |= (SM_RES_CHAOS);
			break;
		}

		case DRS_RES_DISEN:
		{
			if (p_ptr->resist_disen) m_ptr->smart |= (SM_RES_DISEN);
			break;
		}
	}

#endif /* DRS_SMART_OPTIONS */

}



/*
 * Helper function to chose imaginary monsters
 */
static bool image_okay(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* mode 1 sometimes allows (almost) anything, */
	/* but generally we want appropriate monsters */
	if (!theme_okay(r_idx, 1, FALSE)) return (FALSE);

	/* mimmics and breeders don't make good imaginary monsters */
	if (r_ptr->flags1 & (RF1_CHAR_MULTI)) return (FALSE);
	if (r_ptr->flags2 & (RF2_MULTIPLY)) return (FALSE);

	/* often reject never_move monsters */
	if ((r_ptr->flags1 & (RF1_NEVER_MOVE)) && (rand_int(100) < 70))
		return (FALSE);
	if ((r_ptr->flags7 & (RF7_WATER_ONLY)) && (rand_int(100) < 75))
		return (FALSE);
	if ((r_ptr->flags1 & (RF1_UNIQUE)) && (rand_int(100) < 50))
		return (FALSE);

	/* Okay */
	return (TRUE);
}

/* 
 * summon an imaginary monster 
 * mode 0 is usual, mode 1 is for the room of the pink elephant
 */
void imaginary_friend(int mode)
{
	int sy, sx, ii, ifi;
	int iridx, ibonus = randint(4) + 3;

	/* sometimes allow a scarier illusion */
	if (randint(100) < 10) ibonus = randint(34) + 6;

	/* Require appropriate monsters */
	get_mon_num_hook = image_okay;

	/* Prepare allocation table */
	get_mon_num_prep();
	
	/* Get a monster type */
	iridx = get_mon_num(p_ptr->depth + ibonus, FALSE);

	/* Remove restriction */
	get_mon_num_hook = NULL;

	/* Prepare allocation table */
	get_mon_num_prep();

	if (iridx)
	{
		monster_race *r_ptr;

		if (mode == 1)
		{
#ifdef roomrunes /* mode 1 should never be called if this isn't defined */
			/* mode 1 requires the imaginary monster to be created in a room of the pink elephant */
			int tries = 0;
			while (1)
			{
				get_nearby(p_ptr->py, p_ptr->px, &sy, &sx, 3);
				if (room_runes(sy, sx) == 11) break;
				if (tries > 500) return; /* fail */
				tries++;
			}
			place_monster_aux_real(sy, sx, iridx, FALSE, 0, 22);
#else
			return;
#endif
		}
		else
		{
			/* create an imaginary monster */
			if (get_nearby(p_ptr->py, p_ptr->px, &sy, &sx, 3))
				place_monster_aux_real(sy, sx, iridx, FALSE, 0, 22);
		}

		r_ptr = &r_info[iridx];

		/* give it some imaginary friends (up to 5) */
		if ((r_ptr->flags1 & RF1_FRIENDS) || (r_ptr->flags2 & RF2_FRIEND1))
		{
			ifi = randint(4) + 1;
			for (ii = 0; ii < ifi; ii++)
			{
				if (get_nearby(sy, sx, &sy, &sx, 4))
					place_monster_aux_real(sy, sx, iridx, FALSE, 0, 22);
			}
		}
	}
}

