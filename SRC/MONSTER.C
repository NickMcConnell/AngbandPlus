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



/*
 * Actually remove a monster record.
 * Always call "delete_monster()" first.
 *
 * Currently, only called by "process_monsters()" and
 * "tighten_m_list()" and "wipe_m_list()".
 *
 * Note the careful use of the "m_idx" field in cave grids.  This is
 * necessary to prevent problems during "polymorph" attacks and when
 * one monster "eats" another and even if one monster dies and another
 * "blinks" into the grid it used to occupy.  See below.
 */
void remove_monster_idx(int i)
{
    monster_type *m_ptr = &m_list[i];
    monster_lore *l_ptr = &l_list[m_ptr->r_idx];


    /* One less of this monster on this level */
    l_ptr->cur_num--;

    /* Hack -- remove target monster */
    if (i == target_who) target_who = 0;

    /* Hack -- remove tracked monster */
    if (i == health_who) health_track(0);

    /* One less monster */
    m_max--;

    /* Do structure dumping */
    if (i != m_max) {

	int ny = m_list[m_max].fy;
	int nx = m_list[m_max].fx;

	/* Hack -- prepare to slide the monster */
	if (cave[ny][nx].m_idx == m_max) cave[ny][nx].m_idx = i;

	/* Hack -- Sliding target monster */
	if (target_who == (int)(m_max)) target_who = i;

	/* Hack -- Sliding tracked monster */
	if (health_who == (int)(m_max)) health_track(i);

	/* Structure copy the final monster onto the dead monster */
	m_list[i] = m_list[m_max];
    }

    /* Wipe the monster record */
    m_list[m_max] = m_list[0];
}


/*
 * Delete a monster by index.
 *
 * The actual "removal" is done by "remove_monster_idx()" which is
 * only called from "process_monsters()".  This prevents any chance
 * of nasty dangling pointer references.
 *
 * But note that until that happens, there will be monsters floating
 * around marked as "dead".  In order to avoid constant checks on
 * "m_ptr->dead", we mark the cave grid as empty below.
 *
 * Note that compaction should probably not be attempted during the
 * "process_monsters()" function, since it will fail.  This is handled
 * by only allowing about 30 monster reproductions per game round, by
 * use of the "tighten_m_list()" function called from "dungeon.c".
 */
void delete_monster_idx(int i)
{
    monster_type *m_ptr = &m_list[i];

    int fy = m_ptr->fy;
    int fx = m_ptr->fx;

    cave_type *c_ptr = &cave[fy][fx];


    /* Monster is dead */
    m_ptr->dead = TRUE;


    /* Hack -- Monster is gone */
    c_ptr->m_idx = 0;

    /* Hack -- Visual update */
    lite_spot(fy, fx);
}


/*
 * Delete the monster, if any, at a given location
 */
void delete_monster(int y, int x)
{
    cave_type *c_ptr;

    /* Paranoia */
    if (!in_bounds(y,x)) return;

    /* Check the grid */
    c_ptr = &cave[y][x];

    /* Delete the monster (if any) */
    if (c_ptr->m_idx > MIN_M_IDX) delete_monster_idx(c_ptr->m_idx);
}


/*
 * Attempt to Compact some monsters.
 *
 * This will usually only work correctly from "tighten_m_list()"
 *
 * XXX Base the saving throw on a combination of monster level,
 * distance from player, and current "desperation".
 */
static void compact_monsters(void)
{
    int           i, cur_dis, num = 0;

    msg_print("Compacting monsters...");

    /* Start 66 (that is, 72-6) units away */
    cur_dis = 72;

    /* Keep going until someone is deleted */
    while (!num) {

	/* Nothing to compact (!!!) */
	if (cur_dis < 0) return;

	/* Come closer to the player */
	cur_dis -= 6;

	/* Check all the monsters */
	for (i = MIN_M_IDX; i < m_max; i++) {

	    monster_type *m_ptr = &m_list[i];
	    monster_race *r_ptr = &r_list[m_ptr->r_idx];

	    /* Paranoia -- skip "dead" monsters */
	    if (m_ptr->dead) continue;

	    /* Ignore nearby monsters */
	    if (m_ptr->cdis < cur_dis) continue;

	    /* Never compact "Quest" Monsters */
	    if (r_ptr->rflags1 & RF1_QUESTOR) continue;

	    /* XXX Try not to compact Unique Monsters */
	    /* if ((r_ptr->rflags1 & RF1_UNIQUE) && ???) continue; */

	    /* All monsters get a saving throw */
	    if (rand_int(3) != 0) continue;

	    /* Delete the monster */
	    delete_monster_idx(i);

	    /* Count the monster */
	    num++;
	}
    }
}


/*
 * Require some breathing space.  This function should never
 * be called during the main loop of "process_monsters()".
 * Thus it is called at the top and bottom of that function.
 * Note that running out of space is NOT fatal, just annoying.
 */
void tighten_m_list()
{
    int i;

    /* Require some free records */
    if (m_max + 30 > MAX_M_IDX) {

	/* Compact some monsters */
	compact_monsters();

	/* Hack -- Remove dead monsters (backwards!) */
	for (i = m_max - 1; i >= MIN_M_IDX; i--) {

	    /* Get the i'th monster */
	    monster_type *m_ptr = &m_list[i];

	    /* Hack -- Remove dead monsters. */
	    if (m_ptr->dead) remove_monster_idx(i);
	}
    }
}



/*
 * Returns a new monster index (or zero)
 */
int m_pop(void)
{
    /* Normal allocation */
    if (m_max < MAX_M_IDX) return (m_max++);

    /* Warning */
    msg_print("Unable to create new monster!");

    /* Hack -- try not to crash */
    return (0);
}







/*
 * Delete/Remove all the monsters when the player leaves the level
 */
void wipe_m_list()
{
    int i;

    /* Delete all the monsters */
    for (i = MIN_M_IDX; i < m_max; i++) {

	monster_type *m_ptr = &m_list[i];

	/* Paranoia -- skip dead monsters */
	if (m_ptr->dead) continue;

	/* Delete the monster */
	delete_monster_idx(i);
    }

    /* Remove all the monsters (backwards!) */
    for (i = m_max - 1; i >= MIN_M_IDX; i--) {

	/* Remove the monster */
	remove_monster_idx(i);
    }

    /* Paranoia */
    m_max = MIN_M_IDX;
}



/*
 * Forward declare
 */
typedef struct _race_entry race_entry;


/*
 * An entry for the monster allocator below
 */
struct _race_entry {
    u16b r_idx;         /* Object kind index */
    byte locale;        /* Base dungeon level */
    byte chance;        /* Rarity of occurance */
};


/*
 * Return a monster race index suitable to the requested dungeon level.
 *
 * There is a small chance (1/50) of using a monster from even deeper
 * than the requested level (up to four levels deeper), in which case
 * the monster will almost definitely be from that level.
 *
 * It is slightly more likely to acquire monsters of the given level
 * that of lower level monsters.  This is done by choosing two monsters
 * and using the "hardest" of the two monsters.
 *
 * The new distribution makes a level n monster occur approx 2/n% of
 * the time on level n, and 1/n*n% are 1st level.
 *
 * Only two functions (this one and the next, which is almost identical)
 * use the "r_level" array, and they both assume that the "r_list" array
 * is sorted by level, which may or may not actually be true.
 *
 * This version (2.7.0) enforces the "rarity" information for monsters.
 * But note that several functions bypass us and use the race list directly,
 * and they also assume that the list is sorted.  XXX XXX XXX
 *
 * XXX XXX XXX This code currently assumes that the monster list is sorted,
 * and even then assumes that "later" monsters are "harder".
 *
 * The old "get_nmons_num()" function was only used in "greater vaults" to
 * acquire extremely out of depth monsters.  We now use this function always.
 *
 * This function still feels a little bit "hacky" to me...
 *
 * Note that monster race zero is now "illegal".
 */

/*
 * Returns the array number of a random object
 * Uses the locale/chance info for distribution.
 */
int get_mon_num(int level)
{
    int         i, try, r_idx;

    monster_race *r_ptr;


    /* Number of entries in the "k_sort" table */
    static u16b size = 0;

    /* The actual table of entries */
    static race_entry *table = NULL;

    /* Number of entries at each locale */
    static u16b t_lev[256];


    /* Initialize the table */
    if (!size) {

	u16b aux[256];

	/* Clear the level counter and the aux array */
	for (i = 0; i < 256; i++) t_lev[i] = aux[i] = 0;

	/* Scan the monsters (not the ghost) */
	for (i = 1; i < MAX_R_IDX-1; i++) {

	    /* Get the i'th race */
	    r_ptr = &r_list[i];

	    /* Process "real" monsters */
	    if (r_ptr->rarity) {

		/* Count the total entries */
		size++;

		/* Count the entries at each level */
		t_lev[r_ptr->level]++;
	    }
	}

	/* Combine the "t_lev" entries */
	for (i = 1; i < 256; i++) t_lev[i] += t_lev[i-1];

	/* Allocate the table */
	C_MAKE(table, size, race_entry);

	/* Scan the monsters (not the ghost) */
	for (i = 1; i < MAX_R_IDX-1; i++) {

	    /* Get the i'th race */
	    r_ptr = &r_list[i];

	    /* Count valid pairs */
	    if (r_ptr->rarity) {

		int r, x, y, z;

		/* Extract the level/rarity */
		x = r_ptr->level;
		r = r_ptr->rarity;

		/* Skip entries preceding our locale */
		y = (x > 0) ? t_lev[x-1] : 0;

		/* Skip previous entries at this locale */
		z = y + aux[x];

		/* Load the table entry */
		table[z].r_idx = i;
		table[z].locale = x;
		table[z].chance = r;

		/* Another entry complete for this locale */
		aux[x]++;
	    }
	}
    }


    /* Pick a monster */
    while (1) {

	/* Town level is easy */
	if (level == 0) {

	    /* Pick a level 0 entry */
	    i = rand_int(t_lev[0]);
	}

	/* Other levels */
	else {

	    /* Obtain "desired" level */
	    try = level;

	    /* Occasionally Make a Nasty Monster */
	    if (rand_int(NASTY_MON) == 0) {

		/* Pick a level bonus */
		int d = try / 4 + 2;

		/* Require a harder level */
		try += ((d < 5) ? d : 5);
	    }

	    /* Only examine legal levels */
	    if (try > MAX_R_LEV) try = MAX_R_LEV;

	    /* Pick any entry at or below the given level */
	    i = rand_int(t_lev[try]);

	    /* Always try for a "harder" monster */
	    if (TRUE) {

		/* Pick another object at or below the given level */
		int j = rand_int(t_lev[try]);

		/* Keep it if it is "better" */
		if (table[i].locale < table[j].locale) i = j;
	    }

	    /* Hack -- Never make town monsters */
	    if (table[i].locale == 0) continue;
	}

	/* Play the "chance game" */
	if (rand_int(table[i].chance) != 0) continue;


	/* Access the "r_idx" of the chosen monster */
	r_idx = table[i].r_idx;

	/* Access the actual race */
	r_ptr = &r_list[r_idx];

	/* Uniques never appear out of "requested" depth */
	if ((r_ptr->level > level) && (r_ptr->rflags1 & RF1_UNIQUE)) {
	    continue;
	}

	/* Depth Monsters never appear out of depth */
	if ((r_ptr->level > dun_level) && (r_ptr->rflags1 & RF1_FORCE_DEPTH)) {
	    continue;
	}

	/* Sounds good */
	return (r_idx);
    }

    /* Paranoia */
    return (0);
}



/*
 * Places a monster at given location
 *
 * Refuses to place out-of-depth Quest Monsters.
 *
 * To give the player a sporting chance, any monster that appears in
 * line-of-sight and can cast spells or breathe, should be asleep.
 * This is an extension of Um55's sleeping dragon code...  This has
 * been changed, since all "good" uniques can be avoided by any player
 * with adequate stealth.  Now "breathers" start out with low energy,
 * which often (but not always) lets the player move before they do.
 *
 * Note that once a monster has been killed 30000 times during a single
 * life, that monster will no longer be created during that life.
 *
 * Note that monster race "zero" is no longer a valid race index!
 *
 * Unique kobolds, Liches, orcs, Ogres, Trolls, yeeks, and demons
 * (but not skeletons/drujs) get a "following" of escorts.  -DGK-
 *
 * XXX XXX XXX Hack --
 * Note that we assume that the higher a monster race index is,
 * the higher the monster level.  We use this to choose the hardest
 * possible s for the unique by traversing the array backwards.
 */
int place_monster(int y, int x, int r_idx, int slp)
{
    int                 i, z, ny, nx, count;

    cave_type           *c_ptr;

    monster_type        *m_ptr;

    monster_race        *r_ptr = &r_list[r_idx];
    monster_lore        *l_ptr = &l_list[r_idx];


    /* Verify location */
    if (!in_bounds(y,x)) return (FALSE);

    /* Require empty space */
    if (!empty_grid_bold(y, x)) return (FALSE);


    /* Paranoia -- prevent illegal monster race index */
    if (r_idx == 0) return (FALSE);


    /* Depth monsters may NOT be created out of depth */
    if ((r_ptr->rflags1 & RF1_FORCE_DEPTH) && (dun_level < r_ptr->level)) {

	/* Cannot create */
	return (FALSE);
    }


    /* Monster limit per level */
    if (l_ptr->cur_num >= l_ptr->max_num) {

	/* Cannot create */
	return (FALSE);
    }

    /* Monster limit per life */
    if (l_ptr->pkills >= 30000) {

	/* Cannot create */
	return (FALSE);
    }


    /* Note the monster */
    if (cheat_hear && (r_ptr->rflags1 & RF1_UNIQUE)) {
	msg_print(format("Unique (%s)", r_ptr->name));
    }

    /* Powerful monster */
    if (r_ptr->level > dun_level) {

	/* Uniques get rating based on "out of depth" amount */
	if (r_ptr->rflags1 & RF1_UNIQUE) {
	    rating += (r_ptr->level - dun_level);
	}

	/* Normal monsters are worth "half" as much */
	else {
	    rating += (r_ptr->level - dun_level) / 2;
	}
    }


    /* Access the location */
    c_ptr = &cave[y][x];

    /* Make a new monster */
    c_ptr->m_idx = m_pop();

    /* Hack -- catch failure */
    if (c_ptr->m_idx == 0) return (FALSE);


    /* Get a new monster record */
    m_ptr = &m_list[c_ptr->m_idx];

    /* Save the race */
    m_ptr->r_idx = r_idx;

    /* Place the monster at the location */
    m_ptr->fy = y;
    m_ptr->fx = x;


    /* Count the monsters on the level */
    l_ptr->cur_num++;


    /* Assign maximal hitpoints */
    if (r_ptr->rflags1 & RF1_FORCE_MAXHP) {
	m_ptr->maxhp = maxroll(r_ptr->hdice, r_ptr->hside);
    }
    else {
	m_ptr->maxhp = damroll(r_ptr->hdice, r_ptr->hside);
    }

    /* And start out fully healthy */
    m_ptr->hp = m_ptr->maxhp;

    /* Extract the monster base speed */
    m_ptr->mspeed = r_ptr->speed;

    /* Allow some small variation per monster */
    i = extract_energy[r_ptr->speed] / 10;
    if (i) m_ptr->mspeed += rand_spread(0, i);

    /* Give a random starting energy */
    m_ptr->energy = rand_int(100);

    /* No "damage" yet */
    m_ptr->stunned = 0;
    m_ptr->confused = 0;
    m_ptr->monfear = 0;

    /* Not dead */
    m_ptr->dead = FALSE;
    
    /* Update the monster (correctly) */
    m_ptr->ml = FALSE;
    update_mon(c_ptr->m_idx, TRUE);


    /* Update the monster sleep info */
    if (slp) {
	if (r_ptr->sleep == 0) {
	    m_ptr->csleep = 0;
	}
	else {
	    m_ptr->csleep = ((int)r_ptr->sleep * 2) +
			     randint((int)r_ptr->sleep * 10);
	}
    }

    /* Wake up... */
    else {
	m_ptr->csleep = 0;
    }


    /* Hack -- Reduce risk of "instant death by breath weapons" */
    if (r_ptr->rflags1 & RF1_FORCE_SLEEP) {

	/* Start out with minimal energy */
	m_ptr->energy = rand_int(10);
    }


    /* Escorts for certain unique monsters */
    if (r_ptr->rflags1 & RF1_UNIQUE) {

	/* Unique monsters with escorts */
	if (strchr("kLoOTyI&", r_ptr->r_char)) {

	    /* XXX XXX XXX XXX Hack -- Find some escorts */
	    for (z = MAX_R_IDX-1; z > 0; z--) {

		monster_race *z_ptr = &r_list[z];

		/* Skip Unique Monsters */
		if (z_ptr->rflags1 & RF1_UNIQUE) continue;
		
		/* Find a similar, lower level, non-unique, monster */
		if ((z_ptr->r_char == r_ptr->r_char) &&
		    (z_ptr->level <= r_ptr->level)) {

		    /* Try up to 50 nearby places */
		    for (count = 0; count < 50; count++) {

			int d = 3;

			/* Pick a place */
			while (1) {
			    ny = rand_spread(y, d);
			    nx = rand_spread(x, d);
			    if (!in_bounds(ny,nx)) continue;
			    if (distance(y, x, ny, nx) > d) continue;
			    if (los(y, x, ny, nx)) break;
			}

			/* Use first empty space */
			if (empty_grid_bold(ny, nx)) break;
		    }

		    /* Hack -- Nowhere to go */
		    if (count == 50) break;

		    /* Certain monsters come in groups */
		    if ((z_ptr->r_char == 'k') ||
			(z_ptr->r_char == 'y') ||
			(z_ptr->r_char == '&') ||
			(z_ptr->rflags1 & RF1_FRIENDS)) {
			place_group(ny, nx, z, slp);
		    }

		    /* Otherwise, just use a single escort */
		    else {
			place_monster(ny, nx, z, slp);
		    }
		}
	    }
	}
    }

    /* Success */
    return TRUE;
}



 
/*
 * Ghost generation info
 */
 
static cptr ghost_race_names[] = {
    "Human", "Elf", "Elf", "Hobbit", "Gnome",
    "Dwarf", "Orc", "Troll", "Human", "Elf",
    "Pixie","Yeek","Crystal Dragon","Copper Dragon",
    "Bronze Dragon","Gold Dragon","Pseudo Dragon"
};

static cptr ghost_class_names[] = {
    "Warrior", "Mage", "Priest",
    "Rogue", "Ranger", "Paladin"
};

static byte ghost_class_colors[] = {
    TERM_L_BLUE, TERM_RED, TERM_L_GREEN,
    TERM_BLUE, TERM_GREEN, TERM_WHITE
};

static int ghost_race;
static int ghost_class;

static char gb_name[32];



/*
 * Set a "blow" record for the ghost
 */
static void ghost_blow(int i, int m, int e, int d, int s)
{
    monster_race *g = &r_list[MAX_R_IDX-1];

    /* Save the data */
    g->blow[i].method = m;
    g->blow[i].effect = e;
    g->blow[i].d_dice = d;
    g->blow[i].d_side = s;
}


/*
 * Prepare the "ghost" race (method 1)
 */
static void set_ghost_aux_1(void)
{
    monster_race *g = &r_list[MAX_R_IDX-1];

    int i, d1, d2;
    
    int lev = g->level;

    int gr = ghost_race;
    int gc = ghost_class;

    cptr gr_name = ghost_race_names[gr];
    cptr gc_name = ghost_class_names[gc];
	
    
    /* XXX Indent */
    if (TRUE) {
    
	/* A wanderer from the town */
	sprintf(ghost_name, "%s, the %s %s",
		gb_name, gr_name, gc_name);


	/* Use a "player" symbol */
	g->r_char = 'p';

	/* Use a "player" color */
	g->r_attr = ghost_class_colors[gc];


	/* Open doors, bash doors */
	g->rflags1 |= (RF2_OPEN_DOOR | RF2_BASH_DOOR);


	/* Treasure drops */
	g->rflags1 |= (RF1_DROP_60 | RF1_DROP_90);

	/* Treasure drops */
	if (lev >= 10) g->rflags1 |= (RF1_DROP_1D2);
	if (lev >= 20) g->rflags1 |= (RF1_DROP_2D2);
	if (lev >= 30) g->rflags1 |= (RF1_DROP_4D2);

	/* Treasure drops */
	if (lev >= 40) g->rflags1 &= ~(RF1_DROP_4D2);
	if (lev >= 40) g->rflags1 |= (RF1_DROP_GREAT);


	/* Extract an "immunity power" */
	i = (lev / 5) + randint(5);
	
	/* Immunity (by level) */
	switch ((i > 12) ? 12 : i) {
	
	    case 12:
		g->rflags3 |= (RF3_IM_POIS);
		
	    case 11:
	    case 10:
		g->rflags3 |= (RF3_IM_ACID);
		
	    case 9:
	    case 8:
	    case 7:
		g->rflags3 |= (RF3_IM_FIRE);
		
	    case 6:
	    case 5:
	    case 4:
		g->rflags3 |= (RF3_IM_COLD);
		
	    case 3:
	    case 2:
	    case 1:
		g->rflags3 |= (RF3_IM_ELEC);
	}


	/* Extract some spells */
	switch (gc) {

	  case 0:       /* Warrior */
	    break;

	  case 1:       /* Mage */
	    g->freq_inate = g->freq_spell = 100 / 2;
	    g->rflags4 |= (RF4_ARROW_1);
	    g->rflags5 |= (RF5_SLOW | RF5_CONF);
	    g->rflags6 |= (RF6_BLINK);
	    if (lev > 5) g->rflags5 |= (RF5_BA_POIS);
	    if (lev > 7) g->rflags5 |= (RF5_BO_ELEC);
	    if (lev > 10) g->rflags5 |= (RF5_BO_COLD);
	    if (lev > 12) g->rflags6 |= (RF6_TPORT);
	    if (lev > 15) g->rflags5 |= (RF5_BO_ACID);
	    if (lev > 20) g->rflags5 |= (RF5_BO_FIRE);
	    if (lev > 25) g->rflags5 |= (RF5_BA_COLD);
	    if (lev > 25) g->rflags6 |= (RF6_HASTE);
	    if (lev > 30) g->rflags5 |= (RF5_BA_FIRE);
	    if (lev > 40) g->rflags5 |= (RF5_BO_MANA);
	    break;

	  case 2:       /* Priest */
	    g->freq_inate = g->freq_spell = 100 / 4;
	    g->rflags5 |= (RF5_CAUSE_1 | RF5_SCARE);
	    if (lev > 5) g->rflags6 |= (RF6_HEAL);
	    if (lev > 10) g->rflags5 |= (RF5_BLIND);
	    if (lev > 12) g->rflags5 |= (RF5_CAUSE_2);
	    if (lev > 18) g->rflags5 |= (RF5_HOLD);
	    if (lev > 25) g->rflags5 |= (RF5_CONF);
	    if (lev > 30) g->rflags5 |= (RF5_CAUSE_3);
	    if (lev > 35) g->rflags5 |= (RF5_DRAIN_MANA);
	    break;

	  case 3:       /* Rogue */
	    g->freq_inate = g->freq_spell = 100 / 6;
	    g->rflags6 |= (RF6_BLINK);
	    if (lev > 10) g->rflags5 |= (RF5_CONF);
	    if (lev > 18) g->rflags5 |= (RF5_SLOW);
	    if (lev > 25) g->rflags6 |= (RF6_TPORT);
	    if (lev > 30) g->rflags5 |= (RF5_HOLD);
	    if (lev > 35) g->rflags6 |= (RF6_TELE_TO);
	    break;

	  case 4:       /* Ranger */
	    g->freq_inate = g->freq_spell = 100 / 6;
	    g->rflags4 |= (RF4_ARROW_1);
	    if (lev > 5) g->rflags5 |= (RF5_BA_POIS);
	    if (lev > 7) g->rflags5 |= (RF5_BO_ELEC);
	    if (lev > 10) g->rflags5 |= (RF5_BO_COLD);
	    if (lev > 18) g->rflags5 |= (RF5_BO_ACID);
	    if (lev > 25) g->rflags5 |= (RF5_BO_FIRE);
	    if (lev > 30) g->rflags5 |= (RF5_BA_COLD);
	    if (lev > 35) g->rflags5 |= (RF5_BA_FIRE);
	    break;

	  case 5:       /* Paladin */
	    g->freq_inate = g->freq_spell = 100 / 8;
	    g->rflags5 |= (RF5_CAUSE_1 | RF5_SCARE);
	    if (lev > 5) g->rflags6 |= (RF6_HEAL);
	    if (lev > 10) g->rflags5 |= (RF5_BLIND);
	    if (lev > 12) g->rflags5 |= (RF5_CAUSE_2);
	    if (lev > 18) g->rflags5 |= (RF5_HOLD);
	    if (lev > 25) g->rflags5 |= (RF5_CONF);
	    if (lev > 30) g->rflags5 |= (RF5_CAUSE_3);
	    if (lev > 35) g->rflags5 |= (RF5_DRAIN_MANA);
	    break;
	}


	/* Racial properties */
	if (gr == 6) g->rflags3 |= (RF3_ORC);
	if (gr == 7) g->rflags3 |= (RF3_TROLL);


	/* Armor class */
	g->ac = 15 + randint(15);
	
	/* Non mage/priest gets extra armor */
	if ((gc != 1) && (gc != 2)) g->ac += randint(60);


	/* Default speed (normal) */
	g->speed = 110;

	/* High level mages are fast... */
	if ((gc == 1) && (lev >= 20)) g->speed += 10;

	/* High level rogues are fast... */
	if ((gc == 3) && (lev >= 30)) g->speed += 10;


	/* Base damage */
	d1 = 1; d2 = lev + 5;

	/* Break up the damage */
	while ((d1 * 8) < d2) {
	    d1 = d1 * 2;
	    d2 = d2 / 2;
	}


	/* Extract attacks */
	switch (gc) {

	  case 0:       /* Warrior */

	    /* Sometimes increase damage */
	    if (lev >= 30) d2 = d2 * 2;
	    
	    /* Normal attacks (four) */
	    ghost_blow(0, RBM_HIT, RBE_HURT, d1, d2);
	    ghost_blow(1, RBM_HIT, RBE_HURT, d1, d2);
	    ghost_blow(2, RBM_HIT, RBE_HURT, d1, d2);
	    ghost_blow(3, RBM_HIT, RBE_HURT, d1, d2);
	    
	    break;

	  case 1:       /* Mage */

	    /* Sometimes increase damage */
	    if (lev >= 30) d2 = d2 * 3 / 2;
	    
	    /* Normal attacks (one) */
	    ghost_blow(0, RBM_HIT, RBE_HURT, d1, d2);

	    break;

	  case 2:       /* Priest */
	  
	    /* Sometimes increase damage */
	    if (lev >= 30) d2 = d2 * 3 / 2;
	    
	    /* Normal attacks (two) */
	    ghost_blow(0, RBM_HIT, RBE_HURT, d1, d2);
	    ghost_blow(0, RBM_HIT, RBE_HURT, d1, d2);

	    break;

	  case 3:       /* Rogue */

	    /* Sometimes increase damage */
	    if (lev >= 30) d2 = d2 * 2;
	    
	    /* Normal attacks */
	    ghost_blow(0, RBM_HIT, RBE_HURT, d1, d2);
	    ghost_blow(1, RBM_HIT, RBE_HURT, d1, d2);

	    /* Special attacks -- Touch to steal */
	    ghost_blow(2, RBM_TOUCH, RBE_EAT_ITEM, 0, 0);
	    ghost_blow(3, RBM_TOUCH, RBE_EAT_ITEM, 0, 0);
	    
	    break;

	  case 4:       /* Ranger */

	    /* Sometimes increase damage */
	    if (lev >= 30) d2 = d2 * 2;

	    /* Normal attacks (three) */
	    ghost_blow(0, RBM_HIT, RBE_HURT, d1, d2);
	    ghost_blow(1, RBM_HIT, RBE_HURT, d1, d2);
	    ghost_blow(2, RBM_HIT, RBE_HURT, d1, d2);

	    break;

	  case 5:       /* Paladin */

	    /* Sometimes increase damage */
	    if (lev >= 30) d2 = d2 * 2;
	    
	    /* Normal attacks (three) */
	    ghost_blow(0, RBM_HIT, RBE_HURT, d1, d2);
	    ghost_blow(1, RBM_HIT, RBE_HURT, d1, d2);
	    ghost_blow(2, RBM_HIT, RBE_HURT, d1, d2);

	    break;
	}
    }
}



/*
 * Prepare the ghost -- method 2
 */
static void set_ghost_aux_2(void)
{
    monster_race *g = &r_list[MAX_R_IDX-1];

    int lev = g->level;

    int gr = ghost_race;

    cptr gr_name = ghost_race_names[gr];
	

    /* The ghost is cold blooded */
    g->rflags2 |= (RF2_COLD_BLOOD);

    /* The ghost is undead */
    g->rflags3 |= (RF3_UNDEAD);

    /* The ghost is immune to poison */
    g->rflags3 |= (RF3_IM_POIS);
    

    /* Make a ghost */
    switch ((lev / 4) + randint(3)) {

      case 1:
      case 2:
      case 3:
	sprintf(ghost_name, "%s, the Skeleton %s", gb_name, gr_name);
	g->r_char = 's';
	g->r_attr = TERM_WHITE;
	g->rflags2 |= (RF2_OPEN_DOOR | RF2_BASH_DOOR);
	g->rflags3 |= (RF3_IM_COLD);
	if (gr == 6) g->rflags3 |= (RF3_ORC);
	if (gr == 7) g->rflags3 |= (RF3_TROLL);
	g->ac = 26;
	g->speed = 110;
	ghost_blow(0, RBM_HIT, RBE_HURT, 1, 6);
	ghost_blow(1, RBM_HIT, RBE_HURT, 1, 6);
	break;

      case 4:
      case 5:
	sprintf(ghost_name, "%s, the Zombified %s", gb_name, gr_name);
	g->r_char = 'z';
	g->r_attr = TERM_GRAY;
	g->rflags1 |= (RF1_DROP_60 | RF1_DROP_90);
	g->rflags2 |= (RF2_OPEN_DOOR | RF2_BASH_DOOR);
	if (gr == 6) g->rflags3 |= (RF3_ORC);
	if (gr == 7) g->rflags3 |= (RF3_TROLL);
	g->ac = 30;
	g->speed = 110;
	g->hside *= 2;
	ghost_blow(0, RBM_HIT, RBE_HURT, 1, 9);
	break;

      case 6:
      case 7:
	sprintf(ghost_name, "%s, the Mummified %s", gb_name, gr_name);
	g->r_char = 'M';
	g->r_attr = TERM_GRAY;
	g->rflags1 |= (RF1_DROP_1D2);
	g->rflags2 |= (RF2_OPEN_DOOR | RF2_BASH_DOOR);
	if (gr == 6) g->rflags3 |= (RF3_ORC);
	if (gr == 7) g->rflags3 |= (RF3_TROLL);
	g->ac = 35;
	g->speed = 110;
	g->hside *= 2;
	g->mexp = (g->mexp * 3) / 2;
	ghost_blow(0, RBM_HIT, RBE_HURT, 2, 8);
	ghost_blow(1, RBM_HIT, RBE_HURT, 2, 8);
	ghost_blow(2, RBM_HIT, RBE_HURT, 2, 8);
	break;

      case 8:
	sprintf(ghost_name, "%s, the Poltergeist", gb_name);
	g->r_char = 'G';
	g->r_attr = TERM_WHITE;
	g->rflags1 |= (RF1_RAND_50 | RF1_RAND_25 | RF1_DROP_1D2);
	g->rflags2 |= (RF2_INVISIBLE | RF2_PASS_WALL);
	g->rflags3 |= (RF3_IM_COLD);
	g->ac = 20;
	g->speed = 130;
	g->mexp = (g->mexp * 3) / 2;
	ghost_blow(0, RBM_HIT, RBE_HURT, 1, 6);
	ghost_blow(1, RBM_HIT, RBE_HURT, 1, 6);
	ghost_blow(2, RBM_TOUCH, RBE_TERRIFY, 0, 0);
	ghost_blow(3, RBM_TOUCH, RBE_TERRIFY, 0, 0);
	break;

      case 9:
      case 10:
	sprintf(ghost_name, "%s, the Spirit", gb_name);
	g->r_char = 'G';
	g->r_attr = TERM_WHITE;
	g->rflags1 |= (RF1_DROP_1D2);
	g->rflags2 |= (RF2_INVISIBLE | RF2_PASS_WALL);
	g->rflags3 |= (RF3_IM_COLD);
	g->ac = 20;
	g->speed = 110;
	g->hside *= 2;
	g->mexp = g->mexp * 3;
	ghost_blow(0, RBM_TOUCH, RBE_LOSE_WIS, 1, 6);
	ghost_blow(1, RBM_TOUCH, RBE_LOSE_DEX, 1, 6);
	ghost_blow(2, RBM_HIT, RBE_HURT, 3, 6);
	ghost_blow(3, RBM_WAIL, RBE_TERRIFY, 0, 0);
	break;

      case 11:
	sprintf(ghost_name, "%s, the Ghost", gb_name);
	g->r_char = 'G';
	g->r_attr = TERM_WHITE;
	g->rflags1 |= (RF1_DROP_1D2);
	g->rflags2 |= (RF2_INVISIBLE | RF2_PASS_WALL);
	g->rflags3 |= (RF3_IM_COLD);
	g->rflags5 |= (RF5_BLIND | RF5_HOLD | RF5_DRAIN_MANA);
	g->freq_inate = g->freq_spell = 100 / 15;
	g->ac = 40;
	g->speed = 120;
	g->hside *= 2;
	g->mexp = (g->mexp * 7) / 2;
	ghost_blow(0, RBM_WAIL, RBE_TERRIFY, 0, 0);
	ghost_blow(1, RBM_TOUCH, RBE_EXP_20, 0, 0);
	ghost_blow(2, RBM_CLAW, RBE_LOSE_INT, 1, 6);
	ghost_blow(3, RBM_CLAW, RBE_LOSE_WIS, 1, 6);
	break;

      case 12:
	sprintf(ghost_name, "%s, the Vampire", gb_name);
	g->r_char = 'V';
	g->r_attr = TERM_VIOLET;
	g->rflags1 |= (RF1_DROP_2D2);
	g->rflags2 |= (RF2_OPEN_DOOR | RF2_BASH_DOOR);
	g->rflags3 |= (RF3_HURT_LITE);
	g->rflags5 |= (RF5_SCARE | RF5_HOLD | RF5_CAUSE_2);
	g->rflags6 |= (RF6_TELE_TO);
	g->freq_inate = g->freq_spell = 100 / 8;
	g->ac = 40;
	g->speed = 110;
	g->hside *= 3;
	g->mexp = g->mexp * 3;
	ghost_blow(0, RBM_HIT, RBE_HURT, 3, 8);
	ghost_blow(1, RBM_HIT, RBE_HURT, 3, 8);
	ghost_blow(2, RBM_BITE, RBE_EXP_40, 0, 0);
	break;

      case 13:
	sprintf(ghost_name, "%s, the Wraith", gb_name);
	g->r_char = 'W';
	g->r_attr = TERM_WHITE;
	g->rflags1 |= (RF1_DROP_2D2 | RF1_DROP_4D2);
	g->rflags2 |= (RF2_OPEN_DOOR | RF2_BASH_DOOR);
	g->rflags3 |= (RF3_IM_COLD | RF3_HURT_LITE);
	g->rflags5 |= (RF5_BLIND | RF5_SCARE | RF5_HOLD);
	g->rflags5 |= (RF5_CAUSE_3 | RF5_BO_NETH);
	g->freq_inate = g->freq_spell = 100 / 7;
	g->ac = 60;
	g->speed = 120;
	g->hside *= 3;
	g->mexp = g->mexp * 5;
	ghost_blow(0, RBM_HIT, RBE_HURT, 3, 8);
	ghost_blow(1, RBM_HIT, RBE_HURT, 3, 8);
	ghost_blow(2, RBM_TOUCH, RBE_EXP_20, 0, 0);
	break;

      case 14:
	sprintf(ghost_name, "%s, the Vampire Lord", gb_name);
	g->r_char = 'V';
	g->r_attr = TERM_BLUE;
	g->rflags1 |= (RF1_DROP_1D2 | RF1_DROP_GREAT);
	g->rflags2 |= (RF2_OPEN_DOOR | RF2_BASH_DOOR);
	g->rflags3 |= (RF3_HURT_LITE);
	g->rflags5 |= (RF5_SCARE | RF5_HOLD | RF5_CAUSE_3 | RF5_BO_NETH);
	g->rflags6 |= (RF6_TELE_TO);
	g->freq_inate = g->freq_spell = 100 / 8;
	g->ac = 80;
	g->speed = 110;
	g->hside *= 2;
	g->hdice *= 2;
	g->mexp = g->mexp * 20;
	ghost_blow(0, RBM_HIT, RBE_HURT, 3, 8);
	ghost_blow(1, RBM_HIT, RBE_HURT, 3, 8);
	ghost_blow(2, RBM_HIT, RBE_HURT, 3, 8);
	ghost_blow(3, RBM_BITE, RBE_EXP_80, 0, 0);
	break;

      case 15:
	sprintf(ghost_name, "%s, the Ghost", gb_name);
	g->r_char = 'G';
	g->r_attr = TERM_WHITE;
	g->rflags1 |= (RF1_DROP_2D2 | RF1_DROP_GREAT);
	g->rflags2 |= (RF2_INVISIBLE | RF2_PASS_WALL);
	g->rflags3 |= (RF3_IM_COLD);
	g->rflags5 |= (RF5_BLIND | RF5_CONF | RF5_HOLD | RF5_DRAIN_MANA);
	g->freq_inate = g->freq_spell = 100 / 5;
	g->ac = 90;
	g->speed = 130;
	g->hside *= 3;
	g->mexp = g->mexp * 20;
	ghost_blow(0, RBM_WAIL, RBE_TERRIFY, 0, 0);
	ghost_blow(1, RBM_TOUCH, RBE_EXP_20, 0, 0);
	ghost_blow(2, RBM_CLAW, RBE_LOSE_INT, 1, 6);
	ghost_blow(3, RBM_CLAW, RBE_LOSE_WIS, 1, 6);
	break;

      case 17:
	sprintf(ghost_name, "%s, the Lich", gb_name);
	g->r_char = 'L';
	g->r_attr = TERM_ORANGE;
	g->rflags1 |= (RF1_DROP_2D2 | RF1_DROP_1D2 | RF1_DROP_GREAT);
	g->rflags2 |= (RF2_SMART | RF2_OPEN_DOOR | RF2_BASH_DOOR);
	g->rflags3 |= (RF3_IM_COLD);
	g->rflags5 |= (RF5_BLIND | RF5_SCARE | RF5_CONF | RF5_HOLD);
	g->rflags5 |= (RF5_DRAIN_MANA | RF5_BA_FIRE | RF5_BA_COLD);
	g->rflags5 |= (RF5_CAUSE_3 | RF5_CAUSE_4 | RF5_BRAIN_SMASH);
	g->rflags6 |= (RF6_BLINK | RF6_TPORT | RF6_TELE_TO | RF6_S_UNDEAD);
	g->freq_inate = g->freq_spell = 100 / 3;
	g->ac = 120;
	g->speed = 120;
	g->hside *= 3;
	g->hdice *= 2;
	g->mexp = g->mexp * 50;
	ghost_blow(0, RBM_TOUCH, RBE_LOSE_DEX, 2, 12);
	ghost_blow(1, RBM_TOUCH, RBE_LOSE_DEX, 2, 12);
	ghost_blow(2, RBM_TOUCH, RBE_UN_POWER, 0, 0);
	ghost_blow(3, RBM_TOUCH, RBE_EXP_40, 0, 0);
	break;

      default:
	sprintf(ghost_name, "%s, the Ghost", gb_name);
	g->r_char = 'G';
	g->r_attr = TERM_WHITE;
	g->rflags1 |= (RF1_DROP_1D2 | RF1_DROP_2D2 | RF1_DROP_GREAT);
	g->rflags2 |= (RF2_SMART | RF2_INVISIBLE | RF2_PASS_WALL);
	g->rflags3 |= (RF3_IM_COLD);
	g->rflags5 |= (RF5_BLIND | RF5_CONF | RF5_HOLD | RF5_BRAIN_SMASH);
	g->rflags5 |= (RF5_DRAIN_MANA | RF5_BA_NETH | RF5_BO_NETH);
	g->rflags6 |= (RF6_TELE_TO | RF6_TELE_LEVEL);
	g->freq_inate = g->freq_spell = 100 / 2;
	g->ac = 130;
	g->speed = 130;
	g->hside *= 2;
	g->hdice *= 2;
	g->mexp = g->mexp * 30;
	ghost_blow(0, RBM_WAIL, RBE_TERRIFY, 0, 0);
	ghost_blow(1, RBM_TOUCH, RBE_EXP_20, 0, 0);
	ghost_blow(2, RBM_CLAW, RBE_LOSE_INT, 1, 6);
	ghost_blow(3, RBM_CLAW, RBE_LOSE_WIS, 1, 6);
	break;
    }
}



/*
 * Hack -- Prepare the "ghost" race
 *
 * XXX Note that g->name always points to "ghost_name"
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
static void set_ghost(cptr pn, int hp, int gr, int gc, int lev, bool town)
{
    int i;

    monster_race *g = &r_list[MAX_R_IDX-1];


    /* Extract the basic ghost name */
    strcpy(gb_name, pn);

    /* Find the first comma, or end of string */
    for (i = 0; (i < 16) && (gb_name[i]) && (gb_name[i] != ','); i++);

    /* Terminate the name */
    gb_name[i] = '\0';
	
    /* Force a name */
    if (gb_name[0]) strcpy(gb_name, "Nobody");
    
    /* Capitalize the name */
    if (islower(gb_name[0])) gb_name[0] = toupper(gb_name[0]);


    /* Clear the normal flags */
    g->rflags1 = g->rflags2 = g->rflags3 = 0L;

    /* Clear the spell flags */
    g->rflags4 = g->rflags5 = g->rflags6 = 0L;
    
    
    /* Clear the attacks */
    ghost_blow(0, 0, 0, 0, 0);
    ghost_blow(1, 0, 0, 0, 0);
    ghost_blow(2, 0, 0, 0, 0);
    ghost_blow(3, 0, 0, 0, 0);
    

    /* The ghost never sleeps */
    g->sleep = 0;

    /* The ghost is very attentive */
    g->aaf = 100;


    /* Save the level */
    g->level = lev;

    /* Extract the default experience */
    g->mexp = lev * 5 + 5;


    /* Hack -- Break up the hitpoints */
    for (i = 1; i * i < hp; i++);

    /* Extract the basic hit dice and sides */
    g->hdice = g->hside = i;


    /* Unique monster */
    g->rflags1 |= (RF1_UNIQUE);

    /* Only carry good items */
    g->rflags1 |= (RF1_ONLY_ITEM | RF1_DROP_GOOD);

    /* The ghost is always evil */
    g->rflags3 |= (RF3_EVIL);

    /* Cannot be slept or confused */
    g->rflags3 |= (RF3_NO_SLEEP | RF3_NO_CONF);


    /* Save the race and class */
    ghost_race = gr;
    ghost_class = gc;


    /* Prepare the ghost (method 1) */
    if (town) {

	/* Method 1 */
	set_ghost_aux_1();
    }

    /* Prepare the ghost (method 2) */
    else {

	/* Method 2 */
	set_ghost_aux_2();
    }
}



/*
 * Places a ghost somewhere.
 */
int place_ghost()
{
    int                 y, x, hp, level, gr, gc;

    cave_type           *c_ptr;
    monster_type        *m_ptr;

    monster_race        *r_ptr = &r_list[MAX_R_IDX-1];
    monster_lore        *l_ptr = &l_list[MAX_R_IDX-1];

    FILE                *fp;

    bool                err = FALSE;
    bool                town = FALSE;
    
    char                name[100];
    char                tmp[1024];


    /* Hack -- no ghosts in the town */
    if (!dun_level) return (FALSE);

    /* Already have a ghost */
    if (l_ptr->cur_num >= l_ptr->max_num) return (FALSE);


    /* Town -- Use Player Level */
    if (!dun_level) {

#if 0
	/* Assume minimum level */
	if (p_ptr->lev < 5) return (FALSE);

	/* And even then, it only happens sometimes */
	/* if (14 > randint((dun_level / 2) + 11)) return (FALSE); */

	/* Only a 10% chance */
	if (rand_int(10) != 0) return (FALSE);

	/* Level is player level */
	level = p_ptr->lev;
#endif

	/* Hack -- no chance */
	return (FALSE);
    }

    /* Dungeon -- Use Dungeon Level */
    else {

	/* And even then, it only happens sometimes */
	if (14 > randint((dun_level / 2) + 11)) return (FALSE);

	/* Only a 33% chance */
	if (rand_int(3) != 0) return (FALSE);

	/* Level is dungeon level */
	level = dun_level;
    }


    /* Choose a bones file */
    sprintf(tmp, "%s%sbone.%d", ANGBAND_DIR_BONE, PATH_SEP, level);

    /* Open the bones file */
    fp = my_tfopen(tmp, "r");

    /* No bones file to use */
    if (!fp) return (FALSE);

    /* Scan the file */
    err = (fscanf(fp, "%[^\n]\n%d\n%d\n%d", name, &hp, &gr, &gc) != 4);

    /* Close the file */
    fclose(fp);

    /* Delete the bones file */
    unlink(tmp);

    /* Catch errors */
    if (err) {
	msg_print("Warning -- deleted corrupt 'ghost' file!");
	return (FALSE);
    }


    /* Extract "town" flag */
    if (level == p_ptr->lev) town = TRUE;

    /* Set up the ghost */
    set_ghost(name, hp, gr, gc, level, town);


    /* Note for wizard (special ghost name) */
    if (cheat_hear) msg_print("Unique (Ghost)");

    /* Hack -- pick a nice (far away) location */
    while (1) {

	/* Pick a location */
	y = randint(cur_hgt - 2);
	x = randint(cur_wid - 2);

	/* Require "naked" floor grid */
	if (!naked_grid_bold(y,x)) continue;

	/* Accept far away grids */
	if (distance(py, px, y, x) > MAX_SIGHT + 5) break;
    }


    /*** Place the Ghost by Hand (so no-one else does it accidentally) ***/

    /* Access the location */
    c_ptr = &cave[y][x];

    /* Make a new monster */
    c_ptr->m_idx = m_pop();

    /* XXX XXX XXX Failed? */
    if (c_ptr->m_idx == 0) return (FALSE);


    /* Access the monster */
    m_ptr = &m_list[c_ptr->m_idx];

    m_ptr->fy = y;
    m_ptr->fx = x;

    /* Hack -- the monster is a ghost */
    m_ptr->r_idx = MAX_R_IDX-1;


    /* Count the ghosts (only one) */
    l_ptr->cur_num++;


    /* Assign the max hitpoints */
    m_ptr->maxhp = maxroll(r_ptr->hdice, r_ptr->hside);

    /* Start out fully healed */
    m_ptr->hp = m_ptr->maxhp;

    /* Extract the base speed */
    m_ptr->mspeed = r_ptr->speed;

    /* Pick a random energy */
    m_ptr->energy = rand_int(100);

    /* Not dead */
    m_ptr->dead = FALSE;
    
    /* Not stunned or sleeping */
    m_ptr->stunned = 0;
    m_ptr->csleep = 0;
    m_ptr->monfear = 0;


    /* Mega-Hack -- update the graphic info */
    l_ptr->l_attr = r_ptr->r_attr;
    l_ptr->l_char = r_ptr->r_char;


    /* Update the monster (correctly) */
    m_ptr->ml = FALSE;
    update_mon(c_ptr->m_idx, TRUE);

    /* Success */
    return (TRUE);
}


/*
 * Let the given monster attempt to reproduce.
 * Note that "reproduction" REQUIRES empty space.
 */
int multiply_monster(int m_idx)
{
    int        i, y, x;

    monster_type *m_ptr = &m_list[m_idx];

    int x1 = m_ptr->fx;
    int y1 = m_ptr->fy;

    int result = FALSE;


    /* Hack -- prevent insane reproduction */
    if (m_ptr->cdis > 20) return (FALSE);


    /* Try up to 18 times */
    for (i = 0; i < 18; i++) {

	int d = 1;

	/* Pick a location */
	while (1) {
	    y = rand_spread(y1, d);
	    x = rand_spread(x1, d);
	    if (!in_bounds2(y, x)) continue;
	    if (distance(y1, x1, y, x) > d) continue;
	    if (los(y1, x1, y, x)) break;
	}

	/* Require an "empty" floor grid */
	if (!empty_grid_bold(y, x)) continue;

	/* Create a new monster */
	result = place_monster(y, x, m_ptr->r_idx, FALSE);

	/* Failed to create! */
	if (!result) return FALSE;

	/* Get the new monster */
	m_ptr = &m_list[cave[y][x].m_idx];

	/* Return the visibility of the created monster */
	return (m_ptr->ml);
    }


    /* Nobody got made */
    return (FALSE);
}






/*
 * Place a group of monsters near the given location
 * Hack -- note the use of "old" to prevent a group of out of
 * depth monsters from driving the rating through the roof
 */
void place_group(int y, int x, int r_idx, int slp)
{
    monster_race *r_ptr = &r_list[r_idx];

    int extra = 0;

    int old = rating;


    /* reduce size of group if out-of-depth */
    if (r_ptr->level > (unsigned) dun_level) {
	extra = 0 - randint(r_ptr->level - dun_level);
    }

    /* if monster is deeper than normal, then travel in bigger packs -CFT */
    else if (r_ptr->level < (unsigned) dun_level) {
	extra = randint(dun_level - r_ptr->level);
    }

    /* put an upper bounds on it... -CFT */
    if (extra > 12) extra = 12;

    switch (randint(13) + extra) {
      case 25:
	place_monster(y, x - 3, r_idx, slp);
      case 24:
	place_monster(y, x + 3, r_idx, slp);
      case 23:
	place_monster(y - 3, x, r_idx, slp);
      case 22:
	place_monster(y + 3, x, r_idx, slp);
      case 21:
	place_monster(y - 2, x + 1, r_idx, slp);
      case 20:
	place_monster(y + 2, x - 1, r_idx, slp);
      case 19:
	place_monster(y + 2, x + 1, r_idx, slp);
      case 18:
	place_monster(y - 2, x - 1, r_idx, slp);
      case 17:
	place_monster(y + 1, x + 2, r_idx, slp);
      case 16:
	place_monster(y - 1, x - 2, r_idx, slp);
      case 15:
	place_monster(y + 1, x - 2, r_idx, slp);
      case 14:
	place_monster(y - 1, x + 2, r_idx, slp);
      case 13:
	place_monster(y, x - 2, r_idx, slp);
      case 12:
	place_monster(y, x + 2, r_idx, slp);
      case 11:
	place_monster(y + 2, x, r_idx, slp);
      case 10:
	place_monster(y - 2, x, r_idx, slp);
      case 9:
	place_monster(y + 1, x + 1, r_idx, slp);
      case 8:
	place_monster(y + 1, x - 1, r_idx, slp);
      case 7:
	place_monster(y - 1, x - 1, r_idx, slp);
      case 6:
	place_monster(y - 1, x + 1, r_idx, slp);
      case 5:
	place_monster(y, x + 1, r_idx, slp);
      case 4:
	place_monster(y, x - 1, r_idx, slp);
      case 3:
	place_monster(y + 1, x, r_idx, slp);
      case 2:
	place_monster(y - 1, x, r_idx, slp);
	rating = old;
      default:
	place_monster(y, x, r_idx, slp);
    }
}


/*
 * Allocates some random monsters   -RAK-       
 * Place the monsters at least "dis" distance from the player.
 * Use "slp" to choose the initial "sleep" status
 */
void alloc_monster(int num, int dis, int slp)
{
    int                 y, x, i, r_idx;

    monster_race        *r_ptr;


    /* Place the monsters */
    for (i = 0; i < num; i++) {

	/* Find a legal, distant, unoccupied, space */
	while (1) {

	    /* Pick a location */
	    y = randint(cur_hgt - 2);
	    x = randint(cur_wid - 2);

	    /* Require "naked" floor grid */
	    if (!naked_grid_bold(y,x)) continue;

	    /* Accept far away grids */
	    if (distance(y, x, py, px) > dis) break;
	}

	/* Get a monster of the current level */
	r_idx = get_mon_num(dun_level);

	/* Get that race */
	r_ptr = &r_list[r_idx];
	
	/* Place a group of monsters */ 
	if (r_ptr->rflags1 & RF1_FRIENDS) {
	    place_group(y, x, r_idx, slp);
	}

	/* Place a single monster */    
	else {
	    place_monster(y, x, r_idx, slp);
	}
    }
}


/*
 * Summon a monster of the given level near the given location.
 * We return TRUE if anything was summoned.
 */
int summon_monster(int y1, int x1, int lev)
{
    int                 i, y, x, r_idx;

    monster_race        *r_ptr;


    /* Try twenty locations */
    for (i = 0; i < 20; i++) {

	int d = (i / 15) + 1;

	/* Pick a nearby location */
	while (1) {
	    y = rand_spread(y1, d);
	    x = rand_spread(x1, d);
	    if (!in_bounds(y, x)) continue;
	    if (distance(y1, x1, y, x) > d) continue;
	    if (los(y1, x1, y, x)) break;
	}

	/* Require "empty" floor grids */
	if (!empty_grid_bold(y, x)) continue;

	/* Pick a monster race */
	r_idx = get_mon_num(lev);

	/* Get the race */
	r_ptr = &r_list[r_idx];
	
#if 0
	/* where are they?????? */
	if (r_idx>548) 
	  msg_print (format("Mine %d %s",r_idx,r_ptr->name)); 
	else msg_print (format("Theirs %d %s",r_idx,r_ptr->name));
#endif

	/* Place the monster */
	if (r_ptr->rflags1 & RF1_FRIENDS) {
	    place_group(y, x, r_idx, FALSE);
	}
	else {
	    place_monster(y, x, r_idx, FALSE);
	}

	/* Success */
	return (TRUE);
    }

    /* Nothing summoned */
    return (FALSE);
}



/*
 * Check if monster race "m" is "okay" for summon type "type"
 */
static bool summon_specific_okay(int m, int type)
{
    monster_race *r_ptr = &r_list[m];

    bool okay = FALSE;


    /* Check our requirements */
    switch (type) {

	    case SUMMON_ANT:
		okay = ((r_ptr->r_char == 'a') &&
			!(r_ptr->rflags1 & RF1_UNIQUE));
		break;

	    case SUMMON_SPIDER:
		okay = ((r_ptr->r_char == 'S') &&
			!(r_ptr->rflags1 & RF1_UNIQUE));
		break;

	    case SUMMON_HOUND:
		okay = (((r_ptr->r_char == 'C') || (r_ptr->r_char == 'Z')) &&
			!(r_ptr->rflags1 & RF1_UNIQUE));
		break;

	    case SUMMON_REPTILE:
		okay = ((r_ptr->r_char == 'R') &&
			!(r_ptr->rflags1 & RF1_UNIQUE));
		break;

	    case SUMMON_ANGEL:
		okay = ((r_ptr->r_char == 'A') &&
			!(r_ptr->rflags1 & RF1_UNIQUE));
		break;

	    case SUMMON_DEMON:
		okay = ((r_ptr->rflags3 & RF3_DEMON) &&
		    !(r_ptr->rflags1 & RF1_UNIQUE));
		break;

	    case SUMMON_UNDEAD:
		okay = ((r_ptr->rflags3 & RF3_UNDEAD) &&
			!(r_ptr->rflags1 & RF1_UNIQUE));
		break;

	    case SUMMON_DRAGON:
		okay = ((r_ptr->rflags3 & RF3_DRAGON) &&
			!(r_ptr->rflags1 & RF1_UNIQUE));
		break;

	    case SUMMON_HI_UNDEAD:
		okay = ((r_ptr->r_char == 'L') ||
			(r_ptr->r_char == 'V') ||
			(r_ptr->r_char == 'W'));
		break;

	    case SUMMON_HI_DRAGON:
		okay = (r_ptr->r_char == 'D');
		break;

	    case SUMMON_WRAITH:
		okay = ((r_ptr->r_char == 'W') &&
			(r_ptr->rflags1 & RF1_UNIQUE));
		break;

	    case SUMMON_UNIQUE:
		okay = (r_ptr->rflags1 & RF1_UNIQUE);
		break;
    }

    /* Return the result */
    return (okay);
}


/*
 * Place a monster (of the specified "type") near the given
 * location.  Return TRUE iff a monster was actually summoned.
 *
 * We will attempt to place the monster up to 10 times before giving up.
 *
 * Note: SUMMON_UNIQUE and SUMMON_WRAITH (XXX) require "Unique-ness"
 * Note: SUMMON_GUNDEAD and SUMMON_ANCIENTD may summon "Unique's"
 * Note: Other summon will never summon Unique monsters.
 *
 * This function has been changed.  We now take the "monster level"
 * of the summoning monster as a parameter, and use that, along with
 * the current dungeon level, to help determine the level
 * of the desired monster.  Note that this is an upper bound, and
 * also tends to "prefer" monsters of that level.
 *
 * Currently, we use the average of the dungeon and monster levels,
 * and then add five to allow slight increases in monster power.
 */
int summon_specific(int y1, int x1, int lev, int type)
{
    int         r_idx, i, x, y;

    monster_race        *r_ptr;


    /* Find an acceptable monster race */
    for (r_idx = i = 0; !r_idx && (i < 30000); i++) {

	/* Choose a monster */
	monster_level = (dun_level + lev) / 2 + 5;
	r_idx = get_mon_num(monster_level);
	monster_level = dun_level;

	/* Refuse "undesired" races */
	if (!summon_specific_okay(r_idx, type)) r_idx = 0;
    }

    /* No race found (!) */
    if (!r_idx) return (FALSE);

    /* Get that race */
    r_ptr = &r_list[r_idx];


    /* Try to place it */
    for (i = 0; i < 20; ++i) {

	/* Pick a distance */
	int d = (i / 15) + 1;

	/* Pick a location */
	while (1) {
	    y = rand_spread(y1, d);
	    x = rand_spread(x1, d);
	    if (!in_bounds(y, x)) continue;
	    if (distance(y1, x1, y, x) > d) continue;
	    if (los(y1, x1, y, x)) break;
	}

	/* Require "empty" floor grid */
	if (!empty_grid_bold(y, x)) continue;

	/* Place a group of monsters */ 
	if (r_ptr->rflags1 & RF1_FRIENDS) {
	    place_group(y, x, r_idx, FALSE);
	}

	/* Place a single monster */    
	else {
	    place_monster(y, x, r_idx, FALSE);
	}

	/* Success */
	return (TRUE);
    }


    /* Failure */
    return (FALSE);
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
 * If no r_ptr arg is given, it is extracted from m_ptr and r_list
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
 *   0x23 --> Reflexive, genderized if visable (
 */
void monster_desc(char *desc, monster_type *m_ptr, int mode)
{
    cptr res;
    monster_race *r_ptr = &r_list[m_ptr->r_idx];

    /* Can we "see" it (exists + forced, or visible + not unforced) */
    int seen = m_ptr && ((mode & 0x80) || (!(mode & 0x40) && m_ptr->ml));

    /* Sexed Pronouns (seen and allowed, or unseen and allowed) */
    int pron = m_ptr && ((seen && (mode & 0x20)) || (!seen && (mode & 0x10)));


    /* First, try using pronouns, or describing hidden monsters */
    if (!seen || pron) {

	/* an encoding of the monster "sex" */
	int kind = 0x00;

	/* Extract the gender (if applicable) */
	if (r_ptr->rflags1 & RF1_FEMALE) kind = 0x20;
	else if (r_ptr->rflags1 & RF1_MALE) kind = 0x10;

	/* Ignore the gender (if desired) */
	if (!m_ptr || !pron) kind = 0x00;


	/* Assume simple result */
	res = "it";

	/* Brute force: split on the possibilities */
	switch (kind + (mode & 0x07)) {

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
    else if ((mode & 0x02) && (mode & 0x01)) {

	/* The monster is visible, so use its gender */
	if (r_ptr->rflags1 & RF1_FEMALE) strcpy(desc, "herself");
	else if (r_ptr->rflags1 & RF1_MALE) strcpy(desc, "himself");
	else strcpy(desc, "itself");
    }


    /* Handle all other visible monster requests */
    else {

	/* It could be a Unique */
	if (r_ptr->rflags1 & RF1_UNIQUE) {

	    /* Start with the name (thus nominative and objective) */
	    (void)strcpy(desc, r_ptr->name);
	}

	/* It could be an indefinite monster */
	else if (mode & 0x08) {

	    /* XXX Check plurality for "some" */

	    /* Indefinite monsters need an indefinite article */
	    (void)strcpy(desc, is_a_vowel(r_ptr->name[0]) ? "an " : "a ");
	    (void)strcat(desc, r_ptr->name);
	}

	/* It could be a normal, definite, monster */
	else {

	    /* Definite monsters need a definite article */
	    (void)strcpy(desc, "the ");
	    (void)strcat(desc, r_ptr->name);
	}

	/* Handle the Possessive as a special afterthought */
	if (mode & 0x02) {

	    /* XXX Check for trailing "s" */

	    /* Simply append "apostrophe" and "s" */
	    (void)strcat(desc, "'s");
	}
    }
}



