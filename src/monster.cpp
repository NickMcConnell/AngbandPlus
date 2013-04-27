// File: monster.c
// Purpose: misc code for monsters

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "utumno.h"



static bool spawning = FALSE;


/*
 * Set up a monster as a certain race
 */
void CMonster::setup(void)
{
    CMonsterRace *r_ptr = get_r_ptr();

    // Assign maximum hitpoints
    if (r_ptr->flags1 & RF1_FORCE_MAXHP) {
        SetMHP(maxroll(r_ptr->hdice, r_ptr->hside));
    }
    else {
        SetMHP(damroll(r_ptr->hdice, r_ptr->hside));
    }

    // Start out fully healed
    SetCHP(GetMHP());
    SetCHPFrac(0);


    // Hack -- Reduce risk of "instant death by breath weapons"
    if (r_ptr->flags1 & RF1_FORCE_SLEEP) {
        // Start out busy
        set_busy(100);
    }

    // Give a random starting energy
    else {
        set_busy(rand_int(100));
    }


    // No bad conditions
    set_fast(0);
    set_slow(0);
    set_confused(0);
    set_afraid(0);
    set_stun(0);

    // No knowledge
    set_cdis(0);
    set_los(FALSE);
    set_visible(FALSE);
    detect = 0;

    // Was it spawned?
    set_spawned(spawning);

    // No inventory
    i_ptr = NULL;
}


// Blank out a monster
CMonster::CMonster(int race)
{
    // Set the race
    r_idx = race;

    csleep = 0;
    busy = 0;
    fast = 0;
    slow = 0;
    confused = 0;
    afraid = 0;
    stun = 0;
    cdis = 0;
    los = 0;
    visible = 0;
    spawned = 0;
    i_ptr = NULL;
    detect = 0;
    action = ACTION_NOTHING;
    setup();
}

// Get the monster race pointer
CMonsterRace *CMonster::get_r_ptr(void)
{
    return &r_info[r_idx];
}


// Monster list routines
void delete_monster(CMonster *m_ptr)
{
    CMonsterRace *r_ptr = m_ptr->get_r_ptr();

    // Hack -- Reduce the racial counter
    r_ptr->cur_num--;

    // Hack -- count the number of "reproducers"
    if (r_ptr->flags2 & RF2_MULTIPLY) num_repro--;

    // Monster is gone
    cave[m_ptr->GetY()][m_ptr->GetX()].m_ptr = NULL;

    // Dealloc its memory
    delete m_ptr;
}


/*
 * Delete the monster, if any, at a given location
 */
void delete_monster(int y, int x)
{
    CGrid *g_ptr;

    /* Paranoia */
    if (!in_bounds(y, x)) return;

    /* Check the grid */
    g_ptr = &cave[y][x];

    /* Delete the monster (if any) */
    if (g_ptr->m_ptr) delete_monster(g_ptr->m_ptr);
}




/*
 * Choose a monster race that seems "appropriate" to the given level
 *
 * This function uses the "allocation table" built in "init.c".
 *
 * Note that "town" monsters will *only* be created in the town,
 * and "normal" monsters will *never* be created in the town.
 *
 * There is a small chance (1/50) of "boosting" the given depth by
 * a small amount (up to four levels), except in the town.
 *
 * It is (slightly) more likely to acquire a monster of the given level
 * than one of a lower level.  This is done by choosing several monsters
 * appropriate to the given level and keeping the "hardest" one.
 *
 * Note that we only pick 10000 different monsters before failing.
 * This may prevent "summon unique monsters" from crashing.
 *
 * As far as I can tell, this routine will never return a monster
 * which cannot be placed in the dungeon at the current time.  But
 * if we are given an overly restrictive hook, we may not terminate.
 *
 * XXX XXX XXX Note that the "get_mon_num()" function may fail, especially
 * if given an abusive "restriction", in which case it will return zero.
 */
s16b get_mon_num(int level)
{
    int i, p, k;

    int r_idx;

    CMonsterRace *r_ptr;

    /* Obtain the table */
    s16b *t_lev = alloc_race_index;
    race_entry *table = alloc_race_table;


    /* Hack -- sometimes "boost" level */
    if (level > 0) {
        /* Occasional "nasty" monster */
        if (one_in(NASTY_MON)) {
            /* Pick a level bonus */
            int d = level / 4 + 2;

            /* Boost the level */
            level += ((d < 5) ? d : 5);
        }

        /* Occasional "nasty" monster */
        if (one_in(NASTY_MON)) {
            /* Pick a level bonus */
            int d = level / 4 + 2;

            /* Boost the level */
            level += ((d < 5) ? d : 5);
        }

        /* Only examine legal levels */
        if (level > MAX_DEPTH - 1) level = MAX_DEPTH - 1;
    }


    /* Hack -- Pick a monster */
    for (k = 0; k < 10000; k++)
    {
        /* Town level is easy */
        if (level <= 0) {
            /* Pick a level 0 entry */
            i = rand_int(t_lev[0]);
        }

        /* Other levels */
        else {
            /* Roll for rerolls */
            p = rand_int(100);

            /* Pick any "appropriate" monster */
            i = rand_int(t_lev[level]);

            /* Try for a "harder" monster twice (10%) */
            if (p < 10)
            {
                /* Pick another monster at or below the given level */
                int j = rand_int(t_lev[level]);

                /* Keep it if it is "better" */
                if (table[i].locale < table[j].locale) i = j;
            }

            /* Try for a "harder" monster once (50%) */
            if (p < 60)
            {
                /* Pick another monster at or below the given level */
                int j = rand_int(t_lev[level]);

                /* Keep it if it is "better" */
                if (table[i].locale < table[j].locale) i = j;
            }

            /* Hack -- Never make town monsters */
            if (table[i].locale == 0) continue;
        }


        /* Access the "r_idx" of the chosen monster */
        r_idx = table[i].r_idx;


        /* Hack -- apply the hook (if needed) */
        if (get_mon_num_hook && (!(*get_mon_num_hook)(r_idx))) continue;


        /* Access the actual race */
        r_ptr = &r_info[r_idx];


        /* Hack -- "unique" monsters must be "unique" */
        if ((r_ptr->flags1 & RF1_UNIQUE) &&
            (r_ptr->cur_num >= r_ptr->max_num))
        {
            continue;
        }


        /* Depth Monsters never appear out of depth */
        if ((r_ptr->flags1 & RF1_FORCE_DEPTH) && (r_ptr->level > dun_level)) {
            continue;
        }


        /* Hack -- Roll for "rarity" */
        if (rand_int(table[i].chance) != 0) continue;


        /* Use that monster */
        return (r_idx);
    }


    /* Oops */
    return 0;
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
void CMonster::get_desc(char *desc, int mode)
{
    char *res;
    CMonsterRace *r_ptr = get_r_ptr();
    char *name = r_name + r_ptr->name;
    bool seen, pron;


    /* Can we "see" it (exists + forced, or visible + not unforced) */
    seen = ((mode & 0x80) || (!(mode & 0x40) && is_visible()));

    /* Sexed Pronouns (seen and allowed, or unseen and allowed) */
    pron = ((seen && (mode & 0x20)) || (!seen && (mode & 0x10)));


    /* First, try using pronouns, or describing hidden monsters */
    if (!seen || pron) {
        /* an encoding of the monster "sex" */
        int kind = 0x00;

        /* Extract the gender (if applicable) */
        if (r_ptr->flags1 & RF1_FEMALE) kind = 0x20;
        else if (r_ptr->flags1 & RF1_MALE) kind = 0x10;

        /* Ignore the gender (if desired) */
        if (!pron) kind = 0x00;


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
        strcpy(desc, res);
    }


    /* Handle visible monsters, "reflexive" request */
    else if ((mode & 0x02) && (mode & 0x01)) {
        /* The monster is visible, so use its gender */
        if (r_ptr->flags1 & RF1_FEMALE) strcpy(desc, "herself");
        else if (r_ptr->flags1 & RF1_MALE) strcpy(desc, "himself");
        else strcpy(desc, "itself");
    }


    /* Handle all other visible monster requests */
    else
    {
        /* It could be a Unique */
        if (r_ptr->flags1 & RF1_UNIQUE)
        {
            /* Start with the name (thus nominative and objective) */
            (void)strcpy(desc, name);
        }

        /* It could be an indefinite monster */
        else if (mode & 0x08) {
            /* XXX Check plurality for "some" */

            /* Indefinite monsters need an indefinite article */
            strcpy(desc, is_a_vowel(name[0]) ? "an " : "a ");
            strcat(desc, name);
        }

        /* It could be a normal, definite, monster */
        else {
            /* Definite monsters need a definite article */
            strcpy(desc, "the ");
            strcat(desc, name);
        }

        /* Handle the Possessive as a special afterthought */
        if (mode & 0x02) {
            /* XXX Check for trailing "s" */

            /* Simply append "apostrophe" and "s" */
            strcat(desc, "'s");
        }
    }
}




/*
 * Learn about a monster (by "probing" it)
 */
void CMonster::lore_do_probe(void)
{
    CMonsterRace *r_ptr = get_r_ptr();

    // Hack -- Memorize some flags
    r_ptr->r_flags1 = r_ptr->flags1;
    r_ptr->r_flags2 = r_ptr->flags2;
    r_ptr->r_flags3 = r_ptr->flags3;
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
void CMonster::lore_treasure(int num_item, int num_gold)
{
    CMonsterRace *r_ptr = get_r_ptr();

    /* Note the number of things dropped */
    if (num_item > r_ptr->r_drop_item) r_ptr->r_drop_item = num_item;
    if (num_gold > r_ptr->r_drop_gold) r_ptr->r_drop_gold = num_gold;

    /* Hack -- memorize the good/great flags */
    if (r_ptr->flags1 & RF1_DROP_GOOD) r_ptr->r_flags1 |= RF1_DROP_GOOD;
    if (r_ptr->flags1 & RF1_DROP_GREAT) r_ptr->r_flags1 |= RF1_DROP_GREAT;
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
 * we have a special parameter to request distance computation.
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
 * Note that monsters can only be visible if (1) in line of sight and
 * illuminated by light or infravision, or (2) nearby and detected by
 * telepathy.
 */
void CMonster::update(void)
{
    CMonsterRace *r_ptr = get_r_ptr();

    int d, dx, dy;

    // The current monster location
    int fx = GetX();
    int fy = GetY();

    // Seen at all
    bool flag = FALSE;

    // Seen by vision
    bool easy = FALSE;

    // Seen by telepathy
    bool hard = FALSE;

    // Various extra flags
    bool do_empty_mind = FALSE;
    bool do_weird_mind = FALSE;
    bool do_invisible = FALSE;
    bool do_cold_blood = FALSE;


    // Distance components
    dx = (p_ptr->GetX() > fx) ? (p_ptr->GetX() - fx) : (fx - p_ptr->GetX());
    dy = (p_ptr->GetY() > fy) ? (p_ptr->GetY() - fy) : (fy - p_ptr->GetY());

    // Approximate distance
    d = (dy > dx) ? (dy + (dx>>1)) : (dx + (dy>>1));

    // Save the distance (in a byte)
    set_cdis((d < 255) ? d : 255);


    // Detected monsters
    if (detect) flag = TRUE;

    /* Process "distant" monsters */
    else if (get_cdis() > MAX_SIGHT) {
        /* Ignore unseen monsters */
        if (!is_visible()) return;
    }

    /* Process "nearby" monsters */
    else {
        CGrid *g_ptr = &cave[fy][fx];

        /* Normal line of sight, and player is not blind */
        if ((g_ptr->flags & MAP_VIEW) &&  !p_ptr->GetBlind()) {
            /* Use "infravision" */
            if (get_cdis() <= (byte)(p_ptr->get_see_infra())) {
                /* Infravision only works on "warm" creatures */
                /* Below, we will need to know that infravision failed */
                if (r_ptr->flags2 & RF2_COLD_BLOOD) do_cold_blood = TRUE;

                /* Infravision works */
                if (!do_cold_blood) easy = flag = TRUE;
            }

            /* Use "illumination" */
            if (g_ptr->is_glowing()) {
                /* Take note of invisibility */
                if (r_ptr->flags2 & RF2_INVISIBLE) do_invisible = TRUE;

                /* Visible, or detectable, monsters get seen */
                if (!do_invisible || p_ptr->get_see_inv()) easy = flag = TRUE;
            }
        }

        /* Telepathy can see all "nearby" monsters with "minds" */
        if (p_ptr->get_telepathy()) {
            /* Empty mind, no telepathy */
            if (r_ptr->flags2 & RF2_EMPTY_MIND) {
                do_empty_mind = TRUE;
            }

            /* Weird mind, occasional telepathy */
            else if (r_ptr->flags2 & RF2_WEIRD_MIND) {
                do_weird_mind = TRUE;
                if (percent(50)) hard = flag = TRUE;
            }

            /* Normal mind, allow telepathy */
            else {
                hard = flag = TRUE;
            }

            /* Apply telepathy */
            if (hard) {
                /* Hack -- Memorize mental flags */
                if (r_ptr->flags2 & RF2_SMART) r_ptr->r_flags2 |= RF2_SMART;
                if (r_ptr->flags2 & RF2_STUPID) r_ptr->r_flags2 |= RF2_STUPID;
            }
        }

        /* Hack -- Wizards have "perfect telepathy" */
        if (wizard) flag = TRUE;
    }

    /* The monster is now visible */
    if (flag) {
        /* It was previously unseen */
        if (!is_visible()) {
            /* Mark as visible */
            set_visible(TRUE);

            /* Hack -- Count "fresh" sightings */
            if (r_ptr->r_sights < MAX_SHORT) r_ptr->r_sights++;
        }

        /* Memorize various observable flags */
        if (do_empty_mind) r_ptr->r_flags2 |= RF2_EMPTY_MIND;
        if (do_weird_mind) r_ptr->r_flags2 |= RF2_WEIRD_MIND;
        if (do_cold_blood) r_ptr->r_flags2 |= RF2_COLD_BLOOD;
        if (do_invisible) r_ptr->r_flags2 |= RF2_INVISIBLE;
    }

    /* The monster is not visible */
    else {
        /* It was previously seen */
        if (is_visible()) {
            /* Mark as not visible */
            set_visible(FALSE);
        }
    }


    /* The monster is now easily visible */
    if (easy) {
        /* Change */
        if (!get_los()) {
            /* Mark as easily visible */
            set_los(TRUE);
        }
    }

    /* The monster is not easily visible */
    else {
        /* Change */
        if (get_los()) {
            /* Mark as not easily visible */
            set_los(FALSE);
        }
    }
}




/*
 * This function simply updates all the (non-dead) monsters (see above).
 */
void update_monsters(void)
{
    CMonster *m_ptr;
    int x, y;

    // Go through every tile
    for (x = 0; x < cur_wid; x++) {
        for (y = 0; y < cur_hgt; y++) {
            m_ptr = cave[y][x].m_ptr;

            // If there is no monster, skip
            if (!m_ptr) continue;

            // Update the monster
            m_ptr->update();
        }
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
 */
static bool place_monster_one(int y, int x, int r_idx, bool slp)
{
    CGrid *g_ptr;
    CMonster *m_ptr;
    CMonsterRace *r_ptr = &r_info[r_idx];


    /* Verify location */
    if (!in_bounds(y,x)) return (FALSE);

    /* Require empty space */
    if (!empty_grid_bold(y, x)) return (FALSE);

    /* Hack -- no creation on glyph of warding */
    if (cave[y][x].get_feat() == CF_GLYPH) return (FALSE);


    /* Paranoia */
    if (!r_idx) return (FALSE);

    /* Paranoia */
    if (!r_ptr->name) return (FALSE);


    /* Hack -- "unique" monsters must be "unique" */
    if ((r_ptr->flags1 & RF1_UNIQUE) && (r_ptr->cur_num >= r_ptr->max_num)) {
        /* Cannot create */
        return FALSE;
    }


    /* Depth monsters may NOT be created out of depth */
    if ((r_ptr->flags1 & RF1_FORCE_DEPTH) && (dun_level < r_ptr->level)) {
        /* Cannot create */
        return FALSE;
    }


    // Access the location
    g_ptr = &cave[y][x];

    // Make a new monster of the proper race
    g_ptr->m_ptr = new CMonster(r_idx);

    // Get the monster
    m_ptr = g_ptr->m_ptr;

    /* Place the monster at the location */
    m_ptr->SetLocation(x, y);


    /* Hack -- Count the monsters on the level */
    r_ptr->cur_num++;


    /* Hack -- count the number of "reproducers" */
    if (r_ptr->flags2 & RF2_MULTIPLY) num_repro++;

    /* Update the monster */
    m_ptr->update();


    /* Assume no sleeping */
    m_ptr->set_csleep(0);

    /* Enforce sleeping if needed */
    if (slp && r_ptr->sleep) {
        int val = r_ptr->sleep;
        m_ptr->set_csleep((val * 2) + randint(val * 10));
    }


    /* Success */
    return TRUE;
}


// Maximum size of a group of monsters
const int GROUP_MAX = 32;


/*
 * Attempt to place a "group" of monsters around the given location
 */
static bool place_monster_group(int y, int x, int r_idx, bool slp)
{
    CMonsterRace *r_ptr = &r_info[r_idx];
    int n, i;
    int total = 0, extra = 0;

    int hack_n = 0;

    byte hack_y[GROUP_MAX];
    byte hack_x[GROUP_MAX];


    // Pick a group size
    total = randint(13);

    // Hard monsters, small groups
    if (r_ptr->level > dun_level) {
        extra = r_ptr->level - dun_level;
        extra = 0 - randint(extra);
    }

    /* Easy monsters, large groups */
    else if (r_ptr->level < dun_level) {
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


    /* Start on the monster */
    hack_n = 1;
    hack_x[0] = x;
    hack_y[0] = y;

    /* Puddle monsters, breadth first, up to total */
    for (n = 0; (n < hack_n) && (hack_n < total); n++) {
        /* Grab the location */
        int hx = hack_x[n];
        int hy = hack_y[n];

        /* Check each direction, up to total */
        for (i = 0; (i < 8) && (hack_n < total); i++) {
            int mx = hx + ddx[ddd[i]];
            int my = hy + ddy[ddd[i]];

            /* Walls and Monsters block flow */
            if (!empty_grid_bold(my, mx)) continue;

            /* Attempt to place another monster */
            if (place_monster_one(my, mx, r_idx, slp)) {
                /* Add it to the "hack" set */
                hack_y[hack_n] = my;
                hack_x[hack_n] = mx;
                hack_n++;
            }
        }
    }


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
    CMonsterRace *r_ptr = &r_info[place_monster_idx];
    CMonsterRace *z_ptr = &r_info[r_idx];

    /* Require similar type */
    if (z_ptr->type != r_ptr->type) return FALSE;

    /* Skip more advanced monsters */
    if (z_ptr->level > r_ptr->level) return FALSE;

    /* Skip unique monsters */
    if (z_ptr->flags1 & RF1_UNIQUE) return FALSE;

    /* Paranoia -- Skip identical monsters */
    if (place_monster_idx == r_idx) return FALSE;

    /* Okay */
    return TRUE;
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
 */
bool place_monster_aux(int y, int x, int r_idx, byte flags)
{
    int i;

    CMonsterRace *r_ptr = &r_info[r_idx];


    /* Place one monster, or fail */
    if (!place_monster_one(y, x, r_idx, flags & PM_FORCE_SLEEP)) return FALSE;


    /* Require the "group" flag */
    if (!(flags & PM_ALLOW_GROUP)) return TRUE;


    /* Friends for certain monsters */
    if (r_ptr->flags1 & RF1_FRIENDS) {
        /* Attempt to place a group */
        (void)place_monster_group(y, x, r_idx, flags & PM_FORCE_SLEEP);
    }


    /* Escorts for certain monsters */
    if (r_ptr->flags1 & RF1_ESCORT) {
        /* Try to place several "escorts" */
        for (i = 0; i < 50; i++) {
            int nx, ny, z, d = 3;

            /* Pick a location */
            scatter(&ny, &nx, y, x, d, 0);

            /* Require empty grids */
            if (!empty_grid_bold(ny, nx)) continue;

            /* Set the escort index */
            place_monster_idx = r_idx;

            /* Set the escort hook */
            get_mon_num_hook = place_monster_okay;

            /* Pick a random race */
            z = get_mon_num(r_ptr->level);

            /* Remove restriction */
            get_mon_num_hook = NULL;

            /* Handle failure */
            if (!z) break;

            /* Place a single escort */
            place_monster_one(ny, nx, z, flags & PM_FORCE_SLEEP);

            /* Place a "group" of escorts if needed */
            if ((r_info[z].flags1 & RF1_FRIENDS) ||
                (r_ptr->flags1 & RF1_ESCORTS))
            {
                /* Place a group of monsters */
                (void)place_monster_group(ny, nx, z, flags & PM_FORCE_SLEEP);
            }
        }
    }


    /* Success */
    return (TRUE);
}


/*
 * Attempt to place a monster at the given location appropriate to the
 * level mlev
 */
bool place_monster(int y, int x, int mlev, byte flags)
{
    int r_idx;

    // Pick a monster
    r_idx = get_mon_num(mlev);

    // Handle failure
    if (!r_idx) return FALSE;

    // Attempt to place the monster
    if (place_monster_aux(y, x, r_idx, flags)) return TRUE;

    // Oops
    return FALSE;
}




/*
 * Attempt to allocate a random monster in the dungeon.
 * Place the monster at least "dis" distance from the player.
 * Use "slp" to choose the initial "sleep" status
 */
bool alloc_monster(int dis, int slp, int mlev)
{
    int y, x;
    byte flags;

    /* Find a legal, distant, unoccupied, space */
    while (1) {
        /* Pick a location */
        y = rand_int(cur_hgt);
        x = rand_int(cur_wid);

        /* Require "naked" floor grid */
        if (!naked_grid_bold(y, x)) continue;

        /* Accept far away grids */
        if (distance(x, y, p_ptr->GetX(), p_ptr->GetY()) > dis) break;
    }

    // Attempt to place the monster, allow groups
    flags = PM_ALLOW_GROUP;
    if (slp) flags |= PM_FORCE_SLEEP;
    if (place_monster(y, x, mlev, flags)) return TRUE;

    // Nope
    return FALSE;
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
    CMonsterRace *r_ptr = &r_info[r_idx];


    // Hack -- no specific type specified
    if (!summon_specific_type) return TRUE;


    // Check our requirements
    switch (summon_specific_type) {
        case SUMMON_ANT:
            if (r_ptr->flags1 & RF1_UNIQUE) return FALSE;
            return (r_ptr->type == TYPE_INSECT_ANT);

        case SUMMON_SPIDER:
            if (r_ptr->flags1 & RF1_UNIQUE) return FALSE;
            return (r_ptr->type == TYPE_INSECT_SPIDER);

        case SUMMON_HOUND:
            if (r_ptr->flags1 & RF1_UNIQUE) return FALSE;
            if (r_ptr->type == TYPE_MAMMAL_DOG) return TRUE;
            if (r_ptr->type == TYPE_ELEMENT_HOUND) return TRUE;
            return FALSE;

        case SUMMON_HYDRA:
            if (r_ptr->flags1 & RF1_UNIQUE) return FALSE;
            return (r_ptr->type == TYPE_VERT_HYDRA);

        case SUMMON_ANGEL:
            if (r_ptr->flags1 & RF1_UNIQUE) return FALSE;
            return (r_ptr->type == TYPE_ANGEL);

        case SUMMON_DEMON:
            if (r_ptr->flags1 & RF1_UNIQUE) return FALSE;
            if (r_ptr->flags3 & RF3_DEMON) return TRUE;
            return FALSE;

        case SUMMON_UNDEAD:
            if (r_ptr->flags1 & RF1_UNIQUE) return FALSE;
            if (r_ptr->flags3 & RF3_UNDEAD) return TRUE;
            return FALSE;

        case SUMMON_DRAGON:
            if (r_ptr->flags1 & RF1_UNIQUE) return FALSE;
            if (r_ptr->flags3 & RF3_DRAGON) return TRUE;
            return FALSE;

        case SUMMON_HI_UNDEAD:
            if (r_ptr->type == TYPE_UNDEAD_WIGHT) return TRUE;
            if (r_ptr->type == TYPE_UNDEAD_NAZGUL) return TRUE;
            if (r_ptr->type == TYPE_UNDEAD_VAMPIRE) return TRUE;
            if (r_ptr->type == TYPE_UNDEAD_LICH) return TRUE;
            return FALSE;

        case SUMMON_HI_DRAGON:
            if (r_ptr->type == TYPE_DRAGON_ANCIENT) return TRUE;
            if (r_ptr->type == TYPE_DRAGON_GREAT_WYRM) return TRUE;
            if (r_ptr->type == TYPE_DRAGON_UNIQUE) return TRUE;
            // XXX unusual dragons
            return FALSE;

        case SUMMON_WRAITH:
            if (r_ptr->type == TYPE_UNDEAD_NAZGUL) return TRUE;
            return FALSE;

        case SUMMON_UNIQUE:
            if (r_ptr->flags1 & RF1_UNIQUE) return TRUE;
            return FALSE;
    }

    // Uh oh
    return TRUE;
}


/*
 * Place a monster (of the specified "type") near the given
 * location.  Return TRUE iff a monster was actually summoned.
 *
 * We will attempt to place the monster up to 10 times before giving up.
 *
 * Note: SUMMON_UNIQUE and SUMMON_WRAITH will summon Unique's
 * Note: SUMMON_HI_UNDEAD and SUMMON_HI_DRAGON may summon Unique's
 * Note: None of the other summon codes will ever summon Unique's.
 *
 * This function has been changed.  We now take the "monster level"
 * of the summoning monster as a parameter, and use that, along with
 * the current dungeon level, to help determine the level
 * of the desired monster.  Note that this is an upper bound, and
 * also tends to "prefer" monsters of that level.
 *
 * Currently, we use the average of the dungeon and monster levels,
 * and then add five to allow slight increases in monster power.
 *
 * Note that this function may not succeed, expecially when summoning
 * unique monsters, or when the "legal" summon group is very small.
 *
 * This would be another place where using a "temporary" monster
 * allocation table would be a useful optimization.  XXX XXX XXX
 */
bool summon_specific(int y1, int x1, int lev, int type)
{
    int i, x, y, r_idx;

    bool result = FALSE;

    /* Try to place it */
    for (i = 0; i < 20; i++) {
        /* Pick a distance */
        int d = (i / 15) + 1;

        /* Pick a location */
        scatter(&y, &x, y1, x1, d, 0);

        /* Require "empty" floor grid */
        if (!empty_grid_bold(y, x)) continue;

        /* Hack -- no summon on glyph of warding */
        if (cave[y][x].get_feat() == CF_GLYPH) continue;

        /* Save the "summon" type */
        summon_specific_type = type;

        /* Require "okay" monsters */
        get_mon_num_hook = summon_specific_okay;

        /* Pick a monster */
        r_idx = get_mon_num((dun_level + lev) / 2 + 5);

        /* Remove restriction */
        get_mon_num_hook = NULL;

        /* Forget "summon" type */
        summon_specific_type = 0;

        /* Handle failure */
        if (!r_idx) return (FALSE);

        /* Attempt to place the monster (awake, allow groups) */
        result = place_monster_aux(y, x, r_idx, PM_ALLOW_GROUP);

        /* Done */
        break;
    }

    /* Failure */
    return (result);
}


/*
 * Let the given monster attempt to reproduce.
 *
 * Note that "reproduction" REQUIRES empty space.
 *
 * Also used for cloning.
 */
bool multiply_monster(CMonster *m_ptr)
{
    int i, y, x;
    bool result = FALSE;

    // Make the monsters marked as spawned
    spawning = TRUE;

    // Try up to 18 times
    for (i = 0; i < 18; i++) {
        int d = 1;

        /* Pick a location */
        scatter(&y, &x, m_ptr->GetY(), m_ptr->GetX(), d, 0);

        /* Require an "empty" floor grid */
        if (!empty_grid_bold(y, x)) continue;

        /* Create a new monster (awake, no groups) */
        result = place_monster_aux(y, x, m_ptr->get_r_idx(), 0);

        /* Done */
        break;
    }

    /* Future monsters not spawned */
    spawning = FALSE;

    /* Result */
    return result;
}


/*
 * Dump a message describing a monster's reaction to damage
 *
 * Technically should attempt to treat "Beholder"'s as jelly's
 */
void CMonster::message_pain(int dam)
{
    long oldhp, newhp, tmp;
    int percentage;

    CMonsterRace *r_ptr = get_r_ptr();

    char m_name[80];


    /* Get the monster name */
    get_desc(m_name, 0);

    /* Notice non-damage */
    if (dam == 0) {
        msg_format("%^s is unharmed.", m_name);
        return;
    }

    /* Note -- subtle fix -CFT */
    newhp = (long)(GetCHP());
    oldhp = newhp + (long)(dam);
    tmp = (newhp * 100L) / oldhp;
    percentage = (int)(tmp);


    switch (r_ptr->type) {
        case TYPE_LIFE_FUNGI:
        case TYPE_LIFE_JELLY_IMMOB:
        case TYPE_LIFE_JELLY_MOB:
        case TYPE_LIFE_OOZE:
        case TYPE_ELEMENT_VORTEX:
        case TYPE_QUYLTHULG:
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
            break;

        case TYPE_MAMMAL_DOG:
        case TYPE_ELEMENT_HOUND:
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
            break;

        case TYPE_TOWN:
        case TYPE_MAMMAL_QUAD:
        case TYPE_MAMMAL_RODENT:
        case TYPE_MAMMAL_BAT:
        case TYPE_VERT_AMPHIB:
        case TYPE_VERT_SNAKE:
        case TYPE_VERT_REPTILE:
        case TYPE_VERT_HYDRA:
        case TYPE_INSECT_SPIDER:
        case TYPE_INSECT_SCORPION:
        case TYPE_INSECT_ANT:
        case TYPE_INSECT_BEETLE:
        case TYPE_INSECT_CENTIPEDE:
        case TYPE_INSECT_TICK:
        case TYPE_INSECT_LOUSE:
        case TYPE_INSECT_DRAG_FLY:
        case TYPE_INSECT_FLY:
        case TYPE_UNDEAD_SKELETON:
        case TYPE_UNDEAD_DRUJ:
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
            break;

        case TYPE_HUMAN_WARRIOR:
        case TYPE_HUMAN_MAGE:
        case TYPE_HUMAN_PRIEST:
        case TYPE_HUMAN_ROGUE:
        case TYPE_HUMAN_RANGER:
        case TYPE_HUMAN_MISC:
        case TYPE_HUMANOID_D_ELF:
        case TYPE_HUMANOID_ORC:
        case TYPE_HUMANOID_TROLL:
        case TYPE_HUMANOID_OGRE:
        case TYPE_HUMANOID_GIANT:
        case TYPE_HUMANOID_TITAN:
        case TYPE_HUMANOID_YEEK:
        case TYPE_HUMANOID_KOBOLD:
        case TYPE_HUMANOID_MISC:
        case TYPE_MAMMAL_YETI:
        case TYPE_MAMMAL_CAT:
        case TYPE_VERT_HARPY:
        case TYPE_VERT_BIRD:
        case TYPE_VERT_NAGA:
        case TYPE_VERT_HYBRID:
        case TYPE_LIFE_ICKY_THING:
        case TYPE_LIFE_WORM:
        case TYPE_LIFE_EYE:
        case TYPE_LIFE_BEHOLDER:
        case TYPE_ELEMENT_SPIRIT:
        case TYPE_DRAGON_BABY:
        case TYPE_DRAGON_YOUNG:
        case TYPE_DRAGON_MATURE:
        case TYPE_DRAGON_ANCIENT:
        case TYPE_DRAGON_GREAT_WYRM:
        case TYPE_DRAGON_UNUSUAL:
        case TYPE_DRAGON_UNIQUE:
        case TYPE_DEMON_MINOR:
        case TYPE_DEMON_MAJOR:
        case TYPE_UNDEAD_ZOMBIE:
        case TYPE_UNDEAD_MUMMY:
        case TYPE_UNDEAD_GHOST:
        case TYPE_UNDEAD_WIGHT:
        case TYPE_UNDEAD_NAZGUL:
        case TYPE_UNDEAD_VAMPIRE:
        case TYPE_UNDEAD_LICH:
        case TYPE_ANGEL:
        case TYPE_MIMIC:
        case TYPE_GOLEM:
        case TYPE_COINS:
        case TYPE_MISC:
        case TYPE_QUESTOR:
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
            break;
                    
        case TYPE_NONE:
        default:
            msg_format("%^s notifies you of a bug.");
            break;
    }
}


/* Determine a monster's saving throw.  For simplicity, for now, assume
   that the monster's chance is based only on its level.  Return TRUE if
   the monster succeeds, and FALSE if the monster bites it. -GJW */
bool monster_saves(int mlev, int plev)
{
    int prob = 50 + 3 * (mlev - plev);
    if (prob < 5) prob = 5;
    else if (prob > 95) prob = 95;
    return percent(prob);
}


/*
 * Get a monster's speed
 */
byte CMonster::get_speed(void)
{
    // Get racial speed
    byte speed = get_r_ptr()->speed;

    // Handle haste, slow
    if (get_fast()) speed += 10;
    if (get_slow()) speed -= 10;

    // Return speed
    return speed;
}
