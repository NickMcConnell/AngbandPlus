/* File: borg5.c */
/* Purpose: Medium level stuff for the Borg -BEN- */

#include "angband.h"

#ifdef ALLOW_BORG

#include "borg1.h"
#include "borg2.h"
#include "borg3.h"
#include "borg4.h"
#include "borg5.h"



/*
 * This file is responsible for the "borg_update" routine, which is used
 * to notice changes in the world, and to keep track of terrain features,
 * objects, monsters, both from visual changes, and from world messages.
 *
 * One big thing this file does is "object/monster tracking", which
 * attempts to gather information about the objects and monsters in
 * the dungeon, including their identity, location, and state, and
 * to "follow" them if they "move", and to delete them when they die.
 *
 * Information about terrain is used to help plan "flow" paths.  Info
 * about objects and monsters is used to optimize planning paths to
 * those objects and monsters.  Info about monsters is also used for
 * the "danger" functions, which help avoid dangerous situations.
 *
 * Notes:
 *   We assume that monsters/objects can never appear in walls/doors
 *   We count the occurance of invisible or offscreen monsters
 *   We treat "mimics" and "trappers" as "invisible" monsters
 *
 * To Do:
 *   Track approximate monster hitpoints (min/max hitpoints?)
 *   If so, factor in regeneration and various spell attacks
 *   Take account of monster "fear" when "following" monsters
 *
 * Bugs:
 *   Groups of monsters may induce faulty monster matching
 *   Teleporting monsters may induce faulty monster matching
 *   Monsters which appear singly and in groups are "weird"
 *   The timestamps are not quite in sync properly (?)
 */


/*
 * Old values
 */


static s16b old_chp = -1;   /* Previous hit points */
static s16b old_csp = -1;   /* Previous spell points */

static int o_w_x = -1;      /* Old panel */
static int o_w_y = -1;      /* Old panel */

static int o_c_x = -1;      /* Old location */
static int o_c_y = -1;      /* Old location */


/*
 * Hack -- message memory
 */

static s16b auto_msg_len;

static s16b auto_msg_siz;

static char *auto_msg_buf;

static s16b auto_msg_num;

static s16b auto_msg_max;

static s16b *auto_msg_pos;

static s16b *auto_msg_use;


/*
 * Hack -- help identify "unique" monster names
 */

static int auto_unique_size;        /* Number of uniques */
static s16b *auto_unique_what;      /* Indexes of uniques */
static cptr *auto_unique_text;      /* Names of uniques */

/*
 * Hack -- help identify "normal" monster names
 */

static int auto_normal_size;        /* Number of normals */
static s16b *auto_normal_what;      /* Indexes of normals */
static cptr *auto_normal_text;      /* Names of normals */



/*
 * Hack -- monster/object tracking grids
 */

typedef struct auto_wank auto_wank;

struct auto_wank
{
    byte x;
    byte y;

    byte t_a;
    char t_c;

    bool is_take;
    bool is_kill;
};



/*
 * Hack -- object/monster tracking array
 */

static int auto_wank_num = 0;

static auto_wank *auto_wanks;




/*
 * Attempt to guess what kind of object is at the given location.
 *
 * This routine should rarely, if ever, return "zero".
 *
 * Hack -- we use "base level" instead of "allocation levels".
 */
static int borg_guess_kind(byte a, char c,int y,int x)
{
    /* ok, this is an real cheat.  he ought to use the look command
     * in order to correctly id the object.  But I am passing that up for
     * the sake of speed and accuracy
     */

    /* Cheat the Actual item */
    s16b k_idx;
    object_type *o_ptr;
    k_idx = cave_o_idx[y][x];
    o_ptr= &o_list[k_idx];
    return (o_ptr->k_idx);

    /* Actual item */
    {
    int i, s;

    int b_i = 0, b_s = 0;
    /* Find an "acceptable" object */

    for (i = 1; i < MAX_K_IDX; i++)
    {
        object_kind *k_ptr = &k_info[i];

        /* Skip non-objects */
        if (!k_ptr->name) continue;


        /* Base score */
        s = 10000;


        /* Hack -- penalize "extremely" out of depth */
        if (k_ptr->level > auto_depth + 50) s = s - 500;

        /* Hack -- penalize "very" out of depth */
        if (k_ptr->level > auto_depth + 15) s = s - 100;

        /* Hack -- penalize "rather" out of depth */
        if (k_ptr->level > auto_depth + 5) s = s - 50;

        /* Hack -- penalize "somewhat" out of depth */
        if (k_ptr->level > auto_depth) s = s - 10;

        /* Hack -- Penalize "depth miss" */
        s = s - ABS(k_ptr->level - auto_depth);


        /* Hack -- Penalize INSTA_ART items */
        if (k_ptr->flags3 & TR3_INSTA_ART) s = s - 1000;


        /* Hack -- Penalize CURSED items */
        if (k_ptr->flags3 & TR3_LIGHT_CURSE) s = s - 5000;

        /* Hack -- Penalize BROKEN items */
        if (k_ptr->cost <= 0) s = s - 5000;


        /* Verify char */
        if (c != k_ptr->d_char) continue;


        /* Flavored objects */
        if (k_ptr->flavor)
        {
            /* Hack -- penalize "flavored" objects */
            s = s - 20;
        }

        /* Normal objects */
        else
        {
            /* Verify attr */
              if (a != k_ptr->d_attr) continue;
        }


        /* Desire "best" possible score */
        if (b_i && (s < b_s)) continue;

        /* Track it */
        b_i = i; b_s = s;
    }

    /* Result */
    return (b_i);
}
}


/*
 * Delete an old "object" record
 */
static void borg_delete_take(int i)
{
    auto_grid *ag;

    auto_take *take = &auto_takes[i];

    /* Paranoia -- Already wiped */
    if (!take->k_idx) return;

    /* Note */
    borg_note(format("# Forgetting an object '%s' at (%d,%d)",
                     (k_name + k_info[take->k_idx].name),
                     take->x, take->y));

    /* Access the grid */
    ag = &auto_grids[take->y][take->x];

    /* Forget it */
    ag->take = 0;

    /* Kill the object */
    WIPE(take, auto_take);

    /* One less object */
    auto_takes_cnt--;

    /* Wipe goals */
    goal = 0;
}


/*
 * Determine if an object should be "viewable"
 */
static bool borg_follow_take_aux(int i, int y, int x)
{
    auto_grid *ag;


    /* Access the grid */
    ag = &auto_grids[y][x];

    /* Not on-screen */
    if (!(ag->info & BORG_OKAY)) return (FALSE);

    /* Assume viewable */
    return (TRUE);
}


/*
 * Attempt to "follow" a missing object
 *
 * This routine is not called when the player is blind or hallucinating.
 *
 * This function just deletes objects which have disappeared.
 *
 * We assume that a monster walking onto an object destroys the object.
 */
static void borg_follow_take(int i)
{
    int ox, oy;

    auto_take *take = &auto_takes[i];


    /* Paranoia */
    if (!take->k_idx) return;


    /* Old location */
    ox = take->x;
    oy = take->y;


    /* Out of sight */
    if (!borg_follow_take_aux(i, oy, ox)) return;


    /* Note */
    borg_note(format("# There was an object '%s' at (%d,%d)",
                     (k_name + k_info[take->k_idx].name),
                     ox, oy));


    /* Kill the object */
    borg_delete_take(i);
}



/*
 * Obtain a new "take" index
 */
static int borg_new_take(int k_idx, int y, int x)
{
    int i, n = -1;

    auto_take *take;

    auto_grid *ag = &auto_grids[y][x];


    /* Look for a "dead" object */
    for (i = 1; (n < 0) && (i < auto_takes_nxt); i++)
    {
        /* Reuse "dead" objects */
        if (!auto_takes[i].k_idx) n = i;
    }

    /* Allocate a new object */
    if ((n < 0) && (auto_takes_nxt < 256))
    {
        /* Acquire the entry, advance */
        n = auto_takes_nxt++;
    }

    /* Hack -- steal an old object */
    if (n < 0)
    {
        /* Note */
        borg_note("# Too many objects");

        /* Hack -- Pick a random object */
        n = rand_int(auto_takes_nxt-1) + 1;

        /* Delete it */
        borg_delete_take(n);
    }


    /* Count new object */
    auto_takes_cnt++;

    /* Obtain the object */
    take = &auto_takes[n];

    /* Save the kind */
    take->k_idx = k_idx;

    /* Save the location */
    take->x = x;
    take->y = y;

    /* Save the index */
    ag->take = n;

    /* Timestamp */
    take->when = c_t;

    /* Note */
    borg_note(format("# Creating an object '%s' at (%d,%d)",
                     (k_name + k_info[take->k_idx].name),
                     take->y, take->x));

    /* Wipe goals */
    goal = 0;

    /* Result */
    return (n);
}



/*
 * Attempt to notice a changing "take"
 */
static bool observe_take_diff(int y, int x, byte a, char c)
{
    int i, k_idx;

    auto_take *take;

    /* Guess the kind */
    k_idx = borg_guess_kind(a, c,y,x);

    /* Oops */
    if (!k_idx) return (FALSE);

    /* Make a new object */
    i = borg_new_take(k_idx, y, x);

    /* Get the object */
    take = &auto_takes[i];

    /* Timestamp */
    take->when = c_t;

    /* Okay */
    return (TRUE);
}


/*
 * Attempt to "track" a "take" at the given location
 * Assume that the object has not moved more than "d" grids
 * Note that, of course, objects are never supposed to move,
 * but we may want to take account of "falling" missiles later.
 */
static bool observe_take_move(int y, int x, int d, byte a, char c)
{
    int i, z, ox, oy;

    object_kind *k_ptr;

    /* Scan the objects */
    for (i = 1; i < auto_takes_nxt; i++)
    {
        auto_take *take = &auto_takes[i];

        /* Skip dead objects */
        if (!take->k_idx) continue;

        /* Skip assigned objects */
        if (take->seen) continue;

        /* Extract old location */
        ox = take->x;
        oy = take->y;

        /* Calculate distance */
        z = distance(oy, ox, y, x);

        /* Possible match */
        if (z > d) continue;

        /* Access the kind */
        k_ptr = &k_info[take->k_idx];

        /* Require matching char */
        if (c != k_ptr->d_char) continue;

        /* Require matching attr rr9*/
          if (a != k_ptr->d_attr)
        {
            /* Ignore "flavored" colors */
            if (!k_ptr->flavor) continue;
        }

        /* Actual movement (?) */
        if (z)
        {
            /* Update the grids */
            auto_grids[take->y][take->x].take = 0;

            /* Track it */
            take->x = x;
            take->y = y;

            /* Update the grids */
            auto_grids[take->y][take->x].take = i;

            /* Note */
            borg_note(format("# Tracking an object '%s' at (%d,%d) from (%d,%d)",
                             (k_name + k_ptr->name),
                             take->x, take->y, ox, oy));

            /* Clear goals */
            goal = 0;
        }

        /* Timestamp */
        take->when = c_t;

        /* Mark as seen */
        take->seen = TRUE;

        /* Done */
        return (TRUE);
    }

    /* Oops */
    return (FALSE);
}




/*
 * Attempt to guess what type of monster is at the given location.
 *
 * If we are unable to think of any monster that could be at the
 * given location, we will assume the monster is a player ghost.
 * This is a total hack, but may prevent crashes.
 *
 * The guess can be improved by the judicious use of a specialized
 * "attr/char" mapping, especially for unique monsters.  Currently,
 * the Borg does not stoop to such redefinitions.
 *
 * We will probably fail to identify "trapper" and "lurker" monsters,
 * since they look like whatever they are standing on.  Now we will
 * probably just assume they are player ghosts.  XXX XXX XXX
 *
 * Note that "town" monsters may only appear in town, and in "town",
 * only "town" monsters may appear, unless we summon or polymorph
 * a monster while in town, which should never happen.
 *
 * To guess which monster is at the given location, we consider every
 * possible race, keeping the race (if any) with the best "score".
 *
 * Certain monster races are "impossible", including town monsters
 * in the dungeon, dungeon monsters in the town, unique monsters
 * known to be dead, monsters more than 50 levels out of depth,
 * and monsters with an impossible char, or an impossible attr.
 *
 * Certain aspects of a monster race are penalized, including extreme
 * out of depth, minor out of depth, clear/multihued attrs.
 *
 * Certain aspects of a monster race are rewarded, including monsters
 * that appear in groups, monsters that reproduce, monsters that have
 * been seen on this level a lot.
 *
 * We are never called for "trapper", "lurker", or "mimic" monsters.
 *
 * The actual rewards and penalties probably need some tweaking.
 *
 * Hack -- try not to choose "unique" monsters, or we will flee a lot.
 */
static int borg_guess_race(byte a, char c, bool multi, int y, int x)
{
    /*   ok, this is an real cheat.  he ought to use the look command
     *  in order to correctly id the monster.  but i am passing that up for
     *  the sake of speed
     */
    int i, s, n;
    int b_i = 0, b_s = 0;

    s16b m_idx;
    monster_type   *m_ptr;
    m_idx = cave_m_idx[y][x];
    m_ptr= &m_list[m_idx];
    /* Actual monsters */
    return (m_ptr->r_idx);

    /* If I cannot locate it, then use the old routine to id the monster */
    /* Find an "acceptable" monster */
    for (i = 1; i < MAX_R_IDX-1; i++)
    {
        monster_race *r_ptr = &r_info[i];

        /* Skip non-monsters */
        if (!r_ptr->name) continue;


        /* Base score */
        s = 10000;


        /* Verify char rr9*/
        if (c != r_ptr->d_char) continue;

        /* Clear or multi-hued monsters */
        if (r_ptr->flags1 & (RF1_ATTR_MULTI | RF1_ATTR_CLEAR))
        {
            /* Penalize "weird" monsters */
            if (!multi) s = s - 1000;
        }

        /* Normal monsters */
        else
        {
            /* Verify multi */
            if (multi) continue;

            /* Verify attr */
            if (a != r_ptr->d_attr) continue;
        }


        /* Check uniques */
        if (r_ptr->flags1 & RF1_UNIQUE)
        {
            /* Hack -- Dead uniques stay dead */
            if (auto_race_death[i] > 0) continue;

            /* Prefer normals */
            s = s - 10;
        }


        /* Hack -- penalize "extremely" out of depth */
        if (r_ptr->level > auto_depth + 50) continue;

        /* Hack -- penalize "very" out of depth */
        if (r_ptr->level > auto_depth + 15) s = s - 100;

        /* Hack -- penalize "rather" out of depth */
        if (r_ptr->level > auto_depth + 5) s = s - 50;

        /* Hack -- penalize "somewhat" out of depth */
        if (r_ptr->level > auto_depth) s = s - 10;

        /* Penalize "depth miss" */
        s = s - ABS(r_ptr->level - auto_depth);


        /* Hack -- Reward group monsters */
        if (r_ptr->flags1 & (RF1_FRIEND | RF1_FRIENDS)) s = s + 5;

        /* Hack -- Reward multiplying monsters */
        if (r_ptr->flags2 & RF2_MULTIPLY) s = s + 10;


        /* Count occurances */
        n = auto_race_count[i];

        /* Mega-Hack -- Reward occurances XXX XXX XXX */
        s = s + (n / 100) + (((n < 100) ? n : 100) / 10) + ((n < 10) ? n : 10);


        /* Desire "best" possible score */
        if (b_i && (s < b_s)) continue;

        /* Track it */
        b_i = i; b_s = s;
    }

    /* Success */
    if (b_i) return (b_i);


    /* Message */
    borg_note(format("# Assuming player ghost (char %d, attr %d)", c, a));

    /* Assume player ghost */
    return (MAX_R_IDX - 1);

}


/*
 * Attempt to convert a monster name into a race index
 *
 * First we check for all possible "unique" monsters, including
 * ones we have killed, and even if the monster name is "prefixed"
 * (as in "The Tarrasque" and "The Lernean Hydra").  Since we use
 * a fast binary search, this is acceptable.
 *
 * Otherwise, if the monster is NOT named "The xxx", we assume it
 * must be a "player ghost" (which is impossible).
 *
 * Then, we do a binary search on all "normal" monster names, using
 * a search which is known to find the last matching entry, if one
 * exists, and otherwise to find an entry which would follow the
 * matching entry if there was one, unless the matching entry would
 * follow all the existing entries, in which case it will find the
 * final entry in the list.  Thus, we can search *backwards* from
 * the result of the search, and know that we will access all of
 * the matching entries, as long as we stop once we find an entry
 * which does not match, since this will catch all cases above.
 *
 * Finally, we assume the monster must be a "player ghost" (which
 * as noted above is impossible), which is a hack, but may prevent
 * crashes, even if it does induce strange behavior.
 */
static int borg_guess_race_name(cptr who)
{
    int k, m, n;

    int i, b_i = 0;
    int s, b_s = 0;

    monster_race *r_ptr;

    char partial[160];

	int len = strlen(who);

    /* Start the search */
    m = 0; n = auto_unique_size;

    /* Binary search */
    while (m < n - 1)
    {
        /* Pick a "middle" entry */
        i = (m + n) / 2;

        /* Search to the right (or here) */
        if (strcmp(auto_unique_text[i], who) <= 0)
        {
            m = i;
        }

        /* Search to the left */
        else
        {
            n = i;
        }
    }

    /* Check for equality */
    if (streq(who, auto_unique_text[m]))
    {
        /* Use this monster */
        return (auto_unique_what[m]);
    }


    /* Assume player ghost */
    if (!prefix(who, "The "))
    {
        /* Message */
        borg_note(format("# Assuming player ghost (%s)", who));

        /* Oops */
        return (MAX_R_IDX-1);
    }

    /* Hack -- handle "offscreen" */
	if (suffix(who, " (offscreen)"))
	{
		/* Remove the suffix */
		strcpy(partial, who);
		partial[len - 12] = '\0';
		who = partial;

		/* Message */
		borg_note(format("# Handling offscreen monster (%s)", who));
	}

    /* Skip the prefix */
    who += 4;


    /* Start the search */
    m = 0; n = auto_normal_size;

    /* Binary search */
    while (m < n - 1)
    {
        /* Pick a "middle" entry */
        i = (m + n) / 2;
        /* Search to the right (or here) */
        if (strcmp(auto_normal_text[i], who) <= 0)
        {
            m = i;
        }

        /* Search to the left */
        else
        {
            n = i;
        }
    }

    /* Scan possibilities */
    for (k = m; k >= 0; k--)
    {
        /* Stop when done */
        if (!streq(who, auto_normal_text[k])) break;

        /* Extract the monster */
        i = auto_normal_what[k];

        /* Access the monster */
        r_ptr = &r_info[i];

        /* Basic score */
        s = 1000;

        /* Penalize "depth miss" */
        s = s - ABS(r_ptr->level - auto_depth);

        /* Track best */
        if (b_i && (s < b_s)) continue;

        /* Track it */
        b_i = i; b_s = s;
    }

    /* Success */


    if (b_i) return (b_i);


    /* Message */
    borg_note(format("# Assuming player ghost (%s)", who));

    /* Oops */
    return (MAX_R_IDX-1);
}


/*
 * Hack -- Update a "new" monster
 */
static void borg_update_kill_new(int i)
{
    auto_kill *kill = &auto_kills[i];

    monster_type    *m_ptr = &m_list[cave_m_idx[kill->y][kill->x]];
    monster_race    *r_ptr = &r_info[kill->r_idx];

    /* Extract the monster speed */
    kill->speed = (m_ptr->mspeed);

#if 0
    /* Hack -- assume optimal racial variety */
    if (!(r_ptr->flags1 & RF1_UNIQUE))
    {
        /* Hack -- Assume full speed bonus */
        kill->speed += (extract_energy[kill->speed] / 10);
    }
#endif


    /* Extract actual Hitpoints, this is a cheat.  Borg does not look
     * at the bar at the bottom and frankly that would take a lot of time.
     * It would involve targeting every monster to read their individual bar.
     * then keeping track of it.  When the borg has telepathy this would
     * cripple him down and be tremendously slow.
     *
     * Other option involves keeping track of how much damage we do to the
     * guy, then subtracting.  But then we have to account for heal spells,
     * draining charges, and regeneration.  That just takes too much time
     * and memory.
     *
     * This cheat is not too bad.  A human could draw the same info from
     * from the screen.
     *
     * Kill->power is used a lot in borg_danger,
     * for calculating damage from breath attacks.
     */
     kill->power = m_ptr->hp;

    /* Extract the Level*/
    kill->level = r_ptr->level;

    /* Some monsters never move */
    if (r_ptr->flags1 & RF1_NEVER_MOVE) kill->awake = TRUE;

    /* Is it sleeping */
    if (m_ptr->csleep == 0) kill->awake = TRUE;
    else kill->awake = FALSE;

    /* Is it paralysed? */
    if (m_ptr->invuln == 0) kill->invuln = FALSE;
    else kill->invuln = TRUE;

    /* Is it afraid */
    if (m_ptr->monfear == 0) kill->afraid = FALSE;
    else kill->afraid = TRUE;

    /* Is it confused */
    if (m_ptr->confused == 0) kill->confused = FALSE;
    else kill->confused = TRUE;

    /* Is it stunned*/
    if (m_ptr->stunned == 0) kill->stunned = FALSE;
    else kill->stunned = TRUE;

}


/*
 * Hack -- Update a "old" monster
 *
 * We round the player speed down, and the monster speed up,
 * and we assume maximum racial speed for each monster.
 */
static void borg_update_kill_old(int i)
{
    int t, e;

    auto_kill *kill = &auto_kills[i];

    monster_type    *m_ptr = &m_list[cave_m_idx[kill->y][kill->x]];

    /* Extract actual Hitpoints, this is a cheat.  Borg does not look
     * at the bar at the bottom and frankly that would take a lot of code.
     * It would involve targeting every monster to read their individual bar.
     * then keeping track of it.  When the borg has telepathy this would
     * cripple him down and be tremendously slow.
     *
     * Other option involves keeping track of how much damage we do to the
     * guy, then subtracting.  But then we have to account for heal spells,
     * draining charges, and regeneration.  That just takes too much time
     * and memory.
     *
     * This cheat is not too bad.  A human could draw the same info from
     * from the screen.
     *
     * Kill->power is used a lot in borg_danger,
     * for calculating damage from breath attacks.
     */
     kill->power = m_ptr->hp;

    /* Is it sleeping */
    if (m_ptr->csleep == 0) kill->awake = TRUE;
    else kill->awake = FALSE;

    /* Is it afraid */
    if (m_ptr->monfear == 0) kill->afraid = FALSE;
    else kill->afraid = TRUE;

    /* Is it paralysed? */
    if (m_ptr->invuln == 0) kill->invuln = FALSE;
    else kill->invuln = TRUE;

    /* Is it confused */
    if (m_ptr->confused == 0) kill->confused = FALSE;
    else kill->confused = TRUE;

    /* Is it stunned*/
    if (m_ptr->stunned == 0) kill->stunned = FALSE;
    else kill->stunned = TRUE;

    /* Extract the monster speed */
    kill->speed = (m_ptr->mspeed);

    /* Player energy per game turn */
    e = extract_energy[auto_speed];

    /* Game turns per player move */
    t = (100 + (e - 1)) / e;

    /* Monster energy per game turn */
    e = extract_energy[kill->speed];

    /* Monster moves (times ten) */
    kill->moves = (t * e) / 10;
}


/*
 * Delete an old "kill" record
 */
void borg_delete_kill(int i)
{
    auto_kill *kill = &auto_kills[i];

    /* Paranoia -- Already wiped */
    if (!kill->r_idx) return;

    /* Note */
    borg_note(format("# Forgetting a monster '%s' at (%d,%d)",
                     (r_name + r_info[kill->r_idx].name),
                     kill->x, kill->y));

    /* Update the grids */
    auto_grids[kill->y][kill->x].kill = 0;

    /* Kill the monster */
    WIPE(kill, auto_kill);

    /* One less monster */
    auto_kills_cnt--;

    /* Recalculate danger */
    auto_danger_wipe = TRUE;


    /* Wipe goals */
    goal = 0;
}

/*
 * Force sleep onto a "kill" record
 * ??? Since this check is done at update_kill should I have it here?
 */
static void borg_sleep_kill(int i)
{
    auto_kill *kill = &auto_kills[i];

    /* Paranoia -- Already wiped */
    if (!kill->r_idx) return;

    /* Note */
    borg_note(format("# Noting sleep on a monster '%s' at (%d,%d)",
                     (r_name + r_info[kill->r_idx].name),
                     kill->x, kill->y));

    /* note sleep */
    kill->awake = FALSE;

    /* Recalculate danger */
    auto_danger_wipe = TRUE;
}


/*
 * Determine if a monster should be "viewable"
 */
static bool borg_follow_kill_aux(int i, int y, int x)
{
    int d;

    auto_grid *ag;

    auto_kill *kill = &auto_kills[i];

    monster_race *r_ptr = &r_info[kill->r_idx];


    /* Distance to player */
    d = distance(c_y, c_x, y, x);

    /* Too far away */
    if (d > MAX_SIGHT) return (FALSE);


    /* Access the grid */
    ag = &auto_grids[y][x];

    /* Not on-screen */
    if (!(ag->info & BORG_OKAY)) return (FALSE);


    /* Line of sight */
    if (ag->info & BORG_VIEW)
    {
        /* Use "illumination" */
        if (ag->info & (BORG_LITE | BORG_GLOW))
        {
            /* We can see invisible */
            if (my_see_inv) return (TRUE);

            /* Monster is not invisible */
            if (!(r_ptr->flags2 & RF2_INVISIBLE)) return (TRUE);
        }

        /* Use "infravision" */
        if (d <= my_see_infra)
        {
            /* Infravision works on "warm" creatures */
            if (!(r_ptr->flags2 & RF2_COLD_BLOOD)) return (TRUE);
        }
    }


    /* Telepathy requires "telepathy" */
    if (my_telepathy)
    {
        /* Telepathy fails on "strange" monsters */
        if (r_ptr->flags2 & RF2_EMPTY_MIND) return (FALSE);
        if (r_ptr->flags2 & RF2_WEIRD_MIND) return (FALSE);

        /* Success */
        return (TRUE);
    }


    /* Nope */
    return (FALSE);
}


/*
 * Attempt to "follow" a missing monster
 *
 * This routine is not called when the player is blind or hallucinating.
 *
 * Currently this function is a total hack, but handles the case of only
 * one possible location (taking it), two adjacent possible locations
 * (taking the diagonal one), and three adjacent locations with the
 * central one being a diagonal (taking the diagonal one).
 *
 * We should perhaps handle the case of three adjacent locations with
 * the central one being a non-diagonal (taking the non-diagonal one).
 *
 * We should perhaps attempt to take into account "last known direction",
 * which would allow us to "predict" motion up to walls, and we should
 * perhaps attempt to take into account the "standard" flee algorithm,
 * though that feels a little like cheating.
 */
static void borg_follow_kill(int i)
{
    int j;
    int x, y;
    int ox, oy;

    int dx, b_dx = 0;
    int dy, b_dy = 0;

    auto_grid *ag;

    auto_kill *kill = &auto_kills[i];


    /* Paranoia */
    if (!kill->r_idx) return;


    /* Old location */
    ox = kill->x;
    oy = kill->y;


    /* Out of sight */
    if (!borg_follow_kill_aux(i, oy, ox)) return;

    /* Note */
    borg_note(format("# There was a monster '%s' at (%d,%d)",
                     (r_name + r_info[kill->r_idx].name),
                     ox, oy));


    /* Prevent silliness */
    if (!borg_cave_floor_bold(oy, ox))
    {
        /* Delete the monster */
        borg_delete_kill(i);

        /* Done */
        return;
    }

    /* Prevent loops */
    if (rand_int(100) < 1)
    {
        /* Just delete the monster */
        borg_delete_kill(i);

        /* Done */
        return;
    }

    /* prevent overflows */
    if (c_t > 20000)
    {
        /* Just delete the monster */
        borg_delete_kill(i);

        /* Done */
        return;
    }

    /* Some never move, no reason to follow them */
    if ((r_info[kill->r_idx].flags1 & RF1_NEVER_MOVE) ||
    /* Some are sleeping and don't move, no reason to follow them */
       (kill->awake == FALSE))
       {
        /* delete them if they are under me */
        if (kill->y == c_y && kill->x == c_x)
        {
            borg_delete_kill(i);
        }
        /* Dont 'forget' certain ones */
        return;
       }


    /* Scan locations */
    for (j = 0; j < 8; j++)
    {
        /* Access offset */
        dx = ddx_ddd[j];
        dy = ddy_ddd[j];

        /* Access location */
        x = ox + dx;
        y = oy + dy;

        /* Access the grid */
        ag = &auto_grids[y][x];

        /* Skip known walls and doors */
        if (!borg_cave_floor_grid(ag)) continue;

        /* Skip known monsters */
        if (ag->kill) continue;

        /* Skip visible grids */
        if (borg_follow_kill_aux(i, y, x)) continue;

        /* Collect the offsets */
        b_dx += dx;
        b_dy += dy;
    }


    /* Don't go too far */
    if (b_dx < -1) b_dx = -1;
    else if (b_dx > 1) b_dx = 1;

    /* Don't go too far */
    if (b_dy < -1) b_dy = -1;
    else if (b_dy > 1) b_dy = 1;


    /* Access location */
    x = ox + b_dx;
    y = oy + b_dy;

    /* Access the grid */
    ag = &auto_grids[y][x];

    /* Avoid walls and doors */
    if (!borg_cave_floor_grid(ag))
    {
        /* Just delete the monster */
        borg_delete_kill(i);

        /* Done */
        return;
    }

    /* Avoid monsters */
    if (ag->kill)
    {
        /* Just delete the monster */
        borg_delete_kill(i);

        /* Done */
        return;
    }


    /* Update the grids */
    auto_grids[kill->y][kill->x].kill = 0;

    /* Save the old Location */
    kill->ox = ox;
    kill->oy = oy;

    /* Save the Location */
    kill->x = ox + b_dx;
    kill->y = oy + b_dy;

    /* Update the grids */
    auto_grids[kill->y][kill->x].kill = i;

    /* Note */
    borg_note(format("# Following a monster '%s' to (%d,%d) from (%d,%d)",
                     (r_name + r_info[kill->r_idx].name),
                     kill->x, kill->y, ox, oy));

    /* Recalculate danger */
    auto_danger_wipe = TRUE;

    /* Clear goals */
    goal = 0;
}



/*
 * Obtain a new "kill" index
 */
static int borg_new_kill(int r_idx, int y, int x)
{
    int i, n = -1;

    auto_kill *kill;


    /* Look for a "dead" monster */
    for (i = 1; (n < 0) && (i < auto_kills_nxt); i++)
    {
        /* Skip real entries */
        if (!auto_kills[i].r_idx) n = i;
    }

    /* Allocate a new monster */
    if ((n < 0) && (auto_kills_nxt < 256))
    {
        /* Acquire the entry, advance */
        n = auto_kills_nxt++;
    }

    /* Hack -- steal an old monster */
    if (n < 0)
    {
        /* Note */
        borg_note("# Too many monsters");

        /* Hack -- Pick a random monster */
        n = rand_int(auto_kills_nxt-1) + 1;

        /* Kill it */
        borg_delete_kill(n);
    }


    /* Count the monsters */
    auto_kills_cnt++;

    /* Access the monster */
    kill = &auto_kills[n];

    /* Save the race */
    kill->r_idx = r_idx;

    /* Location */
    kill->ox = kill->x = x;
    kill->oy = kill->y = y;

    /* Update the grids */
    auto_grids[kill->y][kill->x].kill = n;

    /* Timestamp */
    kill->when = c_t;

    /* Update the monster */
    borg_update_kill_new(n);

    /* Update the monster */
    borg_update_kill_old(n);

    /* Recalculate danger */
    auto_danger_wipe = TRUE;

    /* Clear goals */
    goal = 0;

    /* Note */
    borg_note(format("# Creating a monster '%s' at (%d,%d)",
                     (r_name + r_info[kill->r_idx].name),
                     kill->y, kill->x));

    /* Wipe goals */
    goal = 0;

    /* Return the monster */
    return (n);
}



/*
 * Attempt to notice a changing "kill"
 */
static bool observe_kill_diff(int y, int x, byte a, char c)
{
    int i, r_idx;

    auto_kill *kill;

    /* Guess the race */
    r_idx = borg_guess_race(a, c, FALSE, y ,x);

    /* Oops */
    /* if (!r_idx) return (FALSE); */

    /* Create a new monster */
    i = borg_new_kill(r_idx, y, x);

    /* Get the object */
    kill = &auto_kills[i];

    /* Timestamp */
    kill->when = c_t;

    /* Done */
    return (TRUE);
}


/*
 * Attempt to "track" a "kill" at the given location
 * Assume that the monster moved at most 'd' grids.
 * If "flag" is TRUE, allow monster "conversion"
 */
static bool observe_kill_move(int y, int x, int d, byte a, char c, bool flag)
{
    int i, z, ox, oy;

    int r_idx;

    auto_kill *kill;

    monster_race *r_ptr;


    /* Look at the monsters */
    for (i = 1; i < auto_kills_nxt; i++)
    {
        kill = &auto_kills[i];

        /* Skip dead monsters */
        if (!kill->r_idx) continue;

        /* Skip assigned monsters */
        if (kill->seen) continue;

        /* Old location */
        ox = kill->x;
        oy = kill->y;

        /* Calculate distance */
        z = distance(oy, ox, y, x);

        /* Verify distance */
        if (z > d) continue;

        /* Verify "reasonable" motion, if allowed */
        if (!flag && (z > (kill->moves / 10) + 1)) continue;

        /* Access the monster race */
        r_ptr = &r_info[kill->r_idx];

        /* Verify matching char */
        if (c != r_ptr->d_char) continue;

        /* Verify matching attr */
          if (a != r_ptr->d_attr)
        {
            /* Require matching attr (for normal monsters) */
            if (!(r_ptr->flags1 & (RF1_ATTR_MULTI | RF1_ATTR_CLEAR)))
            {
                /* Require flag */
                if (!flag) continue;

                /* Never flicker known monsters */
                if (kill->known) continue;

                /* Find a multi-hued monster */
                r_idx = borg_guess_race(a, c, TRUE, y , x);

                /* Handle failure */
                if (r_idx == MAX_R_IDX - 1) continue;

                /* Note */
                borg_note(format("# Flickering monster '%s' at (%d,%d)",
                                 (r_name + r_info[r_idx].name),
                                 x, y));

                /* Note */
                borg_note(format("# Converting a monster '%s' at (%d,%d)",
                                 (r_name + r_info[kill->r_idx].name),
                                 kill->x, kill->y));

                /* Change the race */
                kill->r_idx = r_idx;

                /* Update the monster */
                borg_update_kill_new(i);

                /* Update the monster */
                borg_update_kill_old(i);

                /* Recalculate danger */
                auto_danger_wipe = TRUE;

                /* Clear goals */
                goal = 0;
            }
        }

        /* Actual movement */
        if (z)
        {

            /* Update the grids */
            auto_grids[kill->y][kill->x].kill = 0;

            /* Save the old Location */
            kill->ox = kill->x;
            kill->oy = kill->y;

            /* Save the Location */
            kill->x = x;
            kill->y = y;

            /* Update the grids */
            auto_grids[kill->y][kill->x].kill = i;

            /* Note */
            borg_note(format("# Tracking a monster '%s' at (%d,%d) from (%d,%d)",
                             (r_name + r_ptr->name),
                             kill->x, kill->y, ox, oy));

            /* Recalculate danger */
            auto_danger_wipe = TRUE;

            /* Clear goals */
            goal = 0;
        }

        /* Note when last seen */
        kill->when = c_t;

        /* Update the monster */
        borg_update_kill_old(i);

        /* Mark as seen */
        kill->seen = TRUE;

        /* Done */
        return (TRUE);
    }

    /* Oops */
    return (FALSE);
}



/*
 * Calculate base danger from a spell attack by an invisible monster
 *
 * We attempt to take account of various resistances, both in
 * terms of actual damage, and special effects, as appropriate.
 */
static int borg_fear_spell(int i)
{
    int z = 0;
    int p = 0;

    int ouch = 0;


    /* Damage taken */
    if (old_chp > auto_chp) ouch = (old_chp - auto_chp) * 2;


    /* Check the spell */
    switch (i)
    {
        case 96+0:    /* RF4_SHRIEK */
        p += 10;
        break;

        case 96+1:    /* RF4_FAILED spell by monster.  Fear it! */
                      /* It could be a unique like Azriel */
        p += auto_depth;
        break;

        case 96+2:    /* RF4_XXX3X4 */
        break;

        case 96+3:    /* RF4_XXX4X4 */
        break;

        case 96+4:    /* RF4_ARROW_1 */
        z = (1 * 6);
        break;

        case 96+5:    /* RF4_ARROW_2 */
        z = (3 * 6);
        break;

        case 96+6:    /* RF4_ARROW_3 */
        z = (5 * 6);
        break;

        case 96+7:    /* RF4_ARROW_4 */
        z = (7 * 6);
        break;

        case 96+8:    /* RF4_BR_ACID */
        if (my_immune_acid) break;
        z = ouch;
        p += 40;
        break;

        case 96+9:    /* RF4_BR_ELEC */
        if (my_immune_elec) break;
        z = ouch;
        p += 20;
        break;

        case 96+10:    /* RF4_BR_FIRE */
        if (my_immune_fire) break;
        z = ouch;
        p += 40;
        break;

        case 96+11:    /* RF4_BR_COLD */
        if (my_immune_cold) break;
        z = ouch;
        p += 20;
        break;

        case 96+12:    /* RF4_BR_POIS */
        z = ouch;
        if (my_resist_pois) break;
        if (my_oppose_pois) break;
        p += 20;
        break;

        case 96+13:    /* RF4_BR_NETH */
        z = ouch + 100;
        if (my_resist_neth) break;
        p += 50;
        if (my_hold_life) break;
        /* do not worry about drain exp after level 50 */
        if (auto_level >= 50) break;
        p += 150;
        break;

        case 96+14:    /* RF4_BR_LITE */
        z = ouch;
        if (my_resist_lite) break;
        if (my_resist_blind) break;
        p += 20;
        break;

        case 96+15:    /* RF4_BR_DARK */
        z = ouch;
        if (my_resist_dark) break;
        if (my_resist_blind) break;
        p += 20;
        break;

        case 96+16:    /* RF4_BR_CONF */
        z = ouch;
        if (my_resist_conf) break;
        p += 100;
        break;

        case 96+17:    /* RF4_BR_SOUN */
        z = ouch;
        if (my_resist_sound) break;
        p += 50;
        break;

        case 96+18:    /* RF4_BR_CHAO */
        z = ouch;
        if (my_resist_chaos) break;
        p += 200;
        if (my_resist_neth) break;
        if (my_hold_life) break;
        /* do not worry about drain exp after level 50 */
        if (auto_level == 50) break;
        p += 100;
        if (my_resist_conf) break;
        p += 50;
        break;

        case 96+19:    /* RF4_BR_DISE */
        z = ouch;
        if (my_resist_disen) break;
        p += 500;
        break;

        case 96+20:    /* RF4_BR_NEXU */
        z = ouch;
        if (my_resist_nexus) break;
        p += 100;
        break;

        case 96+21:    /* RF4_BR_TIME */
        z = ouch;
        p += 200;
        break;

        case 96+22:    /* RF4_BR_INER */
        z = ouch;
        p += 50;
        break;

        case 96+23:    /* RF4_BR_GRAV */
        z = ouch;
        p += 50;
        if (my_resist_sound) break;
        p += 50;
        break;

        case 96+24:    /* RF4_BR_SHAR */
        z = ouch;
        if (my_resist_shard) break;
        p += 50;
        break;

        case 96+25:    /* RF4_BR_PLAS */
        z = ouch;
        if (my_resist_sound) break;
        p += 50;
        break;

        case 96+26:    /* RF4_BR_WALL */
        z = ouch;
        if (my_resist_sound) break;
        p += 50;
        break;

        case 96+27:    /* RF4_BR_MANA */
        /* XXX XXX XXX */
        break;

        case 96+28:    /* RF4_XXX5X4 */
        break;

        case 96+29:    /* RF4_XXX6X4 */
        break;

        case 96+30:    /* RF4_XXX7X4 */
        break;

        case 96+31:    /* RF4_XXX8X4 */
        break;



        case 128+0:    /* RF5_BA_ACID */
        if (my_immune_acid) break;
        z = ouch;
        p += 40;
        break;

        case 128+1:    /* RF5_BA_ELEC */
        if (my_immune_elec) break;
        z = ouch;
        p += 20;
        break;

        case 128+2:    /* RF5_BA_FIRE */
        if (my_immune_fire) break;
        z = ouch;
        p += 40;
        break;

        case 128+3:    /* RF5_BA_COLD */
        if (my_immune_cold) break;
        z = ouch;
        p += 20;
        break;

        case 128+4:    /* RF5_BA_POIS */
        z = (12 * 2);
        if (my_resist_pois) z = (z + 2) / 3;
        if (my_resist_pois) break;
        p += 20;
        break;

        case 128+5:    /* RF5_BA_NETH */
        z = ouch + 100;
        if (my_resist_neth) break;
        p += 200;
        break;

        case 128+6:    /* RF5_BA_WATE */
        z = ouch;
        p += 50;
        break;

        case 128+7:    /* RF5_BA_MANA */
        z = ouch;
        break;

        case 128+8:    /* RF5_BA_DARK */
        z = ouch;
        if (my_resist_dark) break;
        if (my_resist_blind) break;
        p += 20;
        break;

        case 128+9:    /* RF5_DRAIN_MANA */
        if (auto_msp) p += 10;
        break;

        case 128+10:    /* RF5_MIND_BLAST */
        z = 20;
        break;

        case 128+11:    /* RF5_BRAIN_SMASH */
        z = (12 * 15);
        p += 100;
        break;

        case 128+12:    /* RF5_CAUSE_1 */
        z = (3 * 8);
        break;

        case 128+13:    /* RF5_CAUSE_2 */
        z = (8 * 8);
        break;

        case 128+14:    /* RF5_CAUSE_3 */
        z = (10 * 15);
        break;

        case 128+15:    /* RF5_CAUSE_4 */
        z = (15 * 15);
        p += 50;
        break;

        case 128+16:    /* RF5_BO_ACID */
        if (my_immune_acid) break;
        z = ouch;
        p += 40;
        break;

        case 128+17:    /* RF5_BO_ELEC */
        if (my_immune_elec) break;
        z = ouch;
        p += 20;
        break;

        case 128+18:    /* RF5_BO_FIRE */
        if (my_immune_fire) break;
        z = ouch;
        p += 40;
        break;

        case 128+19:    /* RF5_BO_COLD */
        if (my_immune_cold) break;
        z = ouch;
        p += 20;
        break;

        case 128+20:    /* RF5_BO_POIS */
        /* XXX XXX XXX */
        break;

        case 128+21:    /* RF5_BO_NETH */
        z = ouch + 100;
        if (my_resist_neth) break;
        p += 200;
        break;

        case 128+22:    /* RF5_BO_WATE */
        z = ouch;
        p += 20;
        break;

        case 128+23:    /* RF5_BO_MANA */
        z = ouch;
        break;

        case 128+24:    /* RF5_BO_PLAS */
        z = ouch;
        p += 20;
        break;

        case 128+25:    /* RF5_BO_ICEE */
        z = ouch;
        p += 20;
        break;

        case 128+26:    /* RF5_MISSILE */
        z = ouch;
        break;

        case 128+27:    /* RF5_SCARE */
        p += 10;
        break;

        case 128+28:    /* RF5_BLIND */
        p += 10;
        break;

        case 128+29:    /* RF5_CONF */
        p += 10;
        break;

        case 128+30:    /* RF5_SLOW */
        p += 5;
        break;

        case 128+31:    /* RF5_HOLD */
        p += 20;
        break;

        case 160+0:    /* RF6_HASTE */
        p += 10;
        break;

        case 160+1:    /* RF6_XXX1X6 */
        break;

        case 160+2:    /* RF6_HEAL */
        p += 10;
        break;

        case 160+3:    /* RF6_XXX2X6 */
        break;

        case 160+4:    /* RF6_BLINK */
        break;

        case 160+5:    /* RF6_TPORT */
        break;

        case 160+6:    /* RF6_XXX3X6 */
        break;

        case 160+7:    /* RF6_XXX4X6 */
        break;

        case 160+8:    /* RF6_TELE_TO */
        p += 20 + auto_depth;
        break;

        case 160+9:    /* RF6_TELE_AWAY */
        p += 10;
        break;

        case 160+10:    /* RF6_TELE_LEVEL */
        p += 50;
        break;

        case 160+11:    /* RF6_XXX5 */
        break;

        case 160+12:    /* RF6_DARKNESS */
        p += 5;
        break;

        case 160+13:    /* RF6_TRAPS */
        p += 50;
        break;

        case 160+14:    /* RF6_FORGET */
        /* if you have lots of cash this is not very scary... just re-ID.*/
        if (auto_level < 35)
            p += 500;
        else
            p += 50;
        break;

        case 160+15:    /* RF6_XXX6X6 */
        break;

        /* Summoning is only as dangerious as the monster that is */
        /* attually summoned.  This helps borgs kill summoners */
        case 160+16:    /* S_KIN */
        p +=30;
        break;

        case 160+17:    /* S_HI_DEMON */
        p +=75;
        break;

        case 160+18:    /* RF6_S_MONSTER */
        p +=25;
        break;

        case 160+19:    /* RF6_S_MONSTERS */
        p += 30;
        break;

        case 160+20:    /* RF6_S_ANT */
        p +=25;
        break;

        case 160+21:    /* RF6_S_SPIDER */
        p +=25;
        break;

        case 160+22:    /* RF6_S_HOUND */
        p +=25;
        break;

        case 160+23:    /* RF6_S_HYDRA */
        p += 70;
        break;

        case 160+24:    /* RF6_S_ANGEL */
        p += 70;
        break;

        case 160+25:    /* RF6_S_DEMON */
        p += 70;
        break;

        case 160+26:    /* RF6_S_UNDEAD */
        p += 70;
        break;

        case 160+27:    /* RF6_S_DRAGON */
        p += 70;
        break;

        case 160+28:    /* RF6_S_HI_UNDEAD */
        p += 95;
        break;

        case 160+29:    /* RF6_S_HI_DRAGON */
        p += 95;
        break;

        case 160+30:    /* RF6_S_WRAITH */
        p += 95;
        break;

        case 160+31:    /* RF6_S_UNIQUE */
        p += 70;
        break;
    }

    /* Notice damage */
    return (p + z);
}



/*
 * Increase the "region danger"
 */
static void borg_fear_grid(cptr who, int y, int x, int k)
{
    int x0, y0, x1, x2, y1, y2;

    /* Message */
    borg_note(format("# Fearing grid (%d,%d) value %d because of a non-LOS %s",
                        x, y, k, who));

    /* Current region */
    y0 = (y/11);
    x0 = (x/11);

    /* Nearby regions */
    y1 = (y0 > 0) ? (y0 - 1) : 0;
    x1 = (x0 > 0) ? (x0 - 1) : 0;
    y2 = (x0 < 5) ? (x0 + 1) : 5;
    x2 = (x0 < 17) ? (x0 + 1) : 17;


    /* Collect "fear", limit to 10000, spread around */
    auto_fear_region[y0][x0] = MIN(auto_fear_region[y0][x0] + k, 10000);
    auto_fear_region[y0][x1] = MIN(auto_fear_region[y0][x1] + k / 2, 10000);
    auto_fear_region[y0][x2] = MIN(auto_fear_region[y0][x2] + k / 2, 10000);
    auto_fear_region[y1][x0] = MIN(auto_fear_region[y1][x0] + k / 2, 10000);
    auto_fear_region[y2][x0] = MIN(auto_fear_region[y2][x0] + k / 2, 10000);
    auto_fear_region[y1][x1] = MIN(auto_fear_region[y1][x1] + k / 3, 10000);
    auto_fear_region[y1][x2] = MIN(auto_fear_region[y1][x2] + k / 3, 10000);
    auto_fear_region[y2][x1] = MIN(auto_fear_region[y2][x1] + k / 3, 10000);
    auto_fear_region[y2][x2] = MIN(auto_fear_region[y2][x1] + k / 3, 10000);
}


/*
 * Attempt to locate a monster which could explain a message involving
 * the given monster name, near the given location, up to the given
 * distance from the given location.
 *
 * Invisible monsters, bizarre monsters, and unexplainable monsters are
 * all treated the same way, and should eventually contribute some amount
 * of basic "fear" to the current region.
 *
 * First, we attempt to convert "similar" objects into the desired monster,
 * then we attempt to convert "similar" monsters into the desired monster,
 * then we attempt to match an existing monster, and finally, we give up.
 *
 * XXX XXX XXX Hack -- To prevent fatal situations, every time we think
 * there may be a monster nearby, we look for a nearby object which could
 * be the indicated monster, and convert it into that monster.  This allows
 * us to correctly handle a room full of multiplying clear mushrooms.
 *
 * XXX XXX XXX When surrounded by multiple monsters of the same type,
 * we will ascribe most messages to one of those monsters, and ignore
 * the existance of all the other similar monsters.
 *
 * XXX XXX XXX Currently, confusion may cause messages to be ignored.
 */
static int borg_locate_kill(cptr who, int y, int x, int r)
{
    int i, d, r_idx;

    int b_i, b_d;

    auto_take *take;
    auto_kill *kill;

    object_kind *k_ptr;

    monster_race *r_ptr;

    /* Handle invisible monsters */
    if (streq(who, "It") ||
        streq(who, "Someone") ||
        streq(who, "Something"))
    {
        /* Note */
        borg_note("# Invisible monster nearby.");
        /* if I can, cast detect inviso--time stamp it
         * We stamp it now if we can, or later if we just did the spell
         * That way we dont loop casting the spell.    APW
         */
        /* detect invis spell not working right, for now just shift panel
         * and cast a light beam if in a hallway and we have see_inv*/
        if (need_see_inviso < (c_t))
        {
            need_see_inviso = (c_t);
        }


        /* Ignore */
        return (0);
    }

    /* Handle offsreen monsters */
    if (suffix(who, " (offscreen)"))
    {
        /* Note */
        borg_note("# Offscreen monster nearby");

        /* Shift the panel */
        need_shift_panel = TRUE;

        /* Ignore */
        return (0);
    }

    /* Guess the monster race */
    r_idx = borg_guess_race_name(who);

    /* Access the monster race */
    r_ptr = &r_info[r_idx];

    /* Note */
    borg_note(format("# There is a monster '%s' within %d grids of %d,%d",
                     (r_name + r_ptr->name),
                     r, x, y));

    /* Hack -- count racial appearances */
    if (auto_race_count[r_idx] < MAX_SHORT) auto_race_count[r_idx]++;


    /* Handle trappers and lurkers and mimics, Darkblade */
    if (r_ptr->flags1 & (RF1_CHAR_CLEAR | RF1_CHAR_MULTI))
    {
        /* Note */
        borg_note("# Bizarre monster nearby");

        /* Ignore */
    }

    /*** Hack -- Find a similar object ***/

    /* Nothing yet */
    b_i = -1; b_d = 999;

    /* Scan the objects */
    for (i = 1; i < auto_takes_nxt; i++)
    {
        take = &auto_takes[i];

        /* Skip "dead" objects */
        if (!take->k_idx) continue;

        /* Access kind */
        k_ptr = &k_info[take->k_idx];

        /* Verify char */
        if (k_ptr->d_char != r_ptr->d_char) continue;

        /* Verify attr (unless clear or multi-hued) */
        if (!(r_ptr->flags1 & (RF1_ATTR_MULTI | RF1_ATTR_CLEAR)))
        {
            /* Verify attr (unless flavored) */
            if (!(k_ptr->flavor))
            {
                /* Verify attr */
                if (k_ptr->d_attr != r_ptr->d_attr) continue;
            }
        }

        /* Calculate distance */
        d = distance(take->y, take->x, y, x);

        /* Skip "wrong" objects */
        if (d > r) continue;

        /* Track closest one */
        if (d > b_d) continue;

        /* Track it */
        b_i = i; b_d = d;
    }

    /* Found one */
    if (b_i >= 0)
    {
        take = &auto_takes[b_i];

        /* Access kind */
        k_ptr = &k_info[take->k_idx];

        /* Note */
        borg_note(format("# Converting an object '%s' at (%d,%d)",
                         (k_name + k_ptr->name),
                         take->x, take->y));

        /* Save location */
        x = take->x;
        y = take->y;

        /* Delete the object */
        borg_delete_take(b_i);

        /* Make a new monster */
        b_i = borg_new_kill(r_idx, y, x);

        /* Get the monster */
        kill = &auto_kills[b_i];

        /* Timestamp */
        kill->when = c_t;

        /* Known identity */
        if (!r) kill->known = TRUE;


        /* Return the index */
        return (b_i);
    }


    /*** Hack -- Find a similar monster ***/

    /* Nothing yet */
    b_i = -1; b_d = 999;

    /* Scan the monsters */
    for (i = 1; i < auto_kills_nxt; i++)
    {
        kill = &auto_kills[i];

        /* Skip "dead" monsters */
        if (!kill->r_idx) continue;

        /* Skip "matching" monsters */
        if (kill->r_idx == r_idx) continue;

        /* Verify char */
        if (r_info[kill->r_idx].d_char != r_ptr->d_char) continue;

        /* Verify attr (unless clear or multi-hued) */
        if (!(r_ptr->flags1 & (RF1_ATTR_MULTI | RF1_ATTR_CLEAR)))
        {
            /* Verify attr */
            if (r_info[kill->r_idx].d_attr != r_ptr->d_attr) continue;
        }

        /* Distance away */
        d = distance(kill->y, kill->x, y, x);

        /* Check distance */
        if (d > r) continue;

        /* Track closest one */
        if (d > b_d) continue;

        /* Track it */
        b_i = i; b_d = d;
    }

    /* Found one */
    if (b_i >= 0)
    {
        kill = &auto_kills[b_i];

        /* Note */
        borg_note(format("# Converting a monster '%s' at (%d,%d)",
                         (r_name + r_info[kill->r_idx].name),
                         kill->x, kill->y));

        /* Change the race */
        kill->r_idx = r_idx;

        /* Update the monster */
        borg_update_kill_new(b_i);

        /* Update the monster */
        borg_update_kill_old(b_i);

        /* Known identity */
        if (!r) kill->known = TRUE;


        /* Recalculate danger */
        auto_danger_wipe = TRUE;

        /* Clear goals */
        goal = 0;

        /* Index */
        return (b_i);
    }


    /*** Hack -- Find an existing monster ***/

    /* Nothing yet */
    b_i = -1; b_d = 999;

    /* Scan the monsters */
    for (i = 1; i < auto_kills_nxt; i++)
    {
        kill = &auto_kills[i];

        /* Skip "dead" monsters */
        if (!kill->r_idx) continue;

        /* Skip "different" monsters */
        if (kill->r_idx != r_idx) continue;

        /* Distance away */
        d = distance(kill->y, kill->x, y, x);

        /* In a darkened room with ESP we can get hit and ignore it */
        /* Check distance */
        if (d > r) continue;

        /* Hopefully this will add fear to our grid */
        if (!borg_projectable(kill->y,kill->x,y,x)) continue;

        /* Track closest one */
        if (d > b_d) continue;

        /* Track it */
        b_i = i; b_d = d;
    }

    /* Found one */
    if (b_i >= 0)
    {
        kill = &auto_kills[b_i];

        /* Note */
        borg_note(format("# Matched a monster '%s' at (%d,%d)",
                         (r_name + r_info[kill->r_idx].name),
                         kill->x, kill->y));

        /* Known identity */
        if (!r) kill->known = TRUE;


        /* Index */
        return (b_i);
    }


    /*** Oops ***/

    /* Note */
    borg_note(format("# Ignoring a monster '%s' near (%d,%d)",
                     (r_name + r_ptr->name),
                     x, y));

    /* Oops */
    /* this is the case where we know the name of the monster */
    /* but cannot locate it on the monster list. */
    return (-1);
}




/*
 * Notice the "death" of a monster
 */
static void borg_count_death(int i)
{
    int r_idx;

    auto_kill *kill = &auto_kills[i];

    /* Access race */
    r_idx = kill->r_idx;

    /* Hack -- count racial deaths */
    if (auto_race_death[r_idx] < MAX_SHORT) auto_race_death[r_idx]++;
}




/*
 * Handle "detection" spells and "call lite"
 *
 * Note that we must use the "old" player location
 */
static bool borg_handle_self(cptr str)
{
    int i;

    int q_x, q_y;


    /* Extract panel */
    q_x = o_w_x / PANEL_WID;
    q_y = o_w_y / PANEL_HGT;


    /* Handle failure */
    if (auto_failure)
    {
        borg_note("# Something failed");
    }

    /* Handle "call lite" */
    else if (prefix(str, "lite"))
    {
        /* Message */
        borg_note(format("# Called lite at (%d,%d)",
                         o_c_x, o_c_y));

        /* Hack -- convert torch-lit grids to perma-lit grids */
        for (i = 0; i < auto_lite_n; i++)
        {
            int x = auto_lite_x[i];
            int y = auto_lite_y[i];

            /* Get the grid */
            auto_grid *ag = &auto_grids[y][x];

            /* Mark as perma-lit */
            ag->info |= BORG_GLOW;

            /* Mark as not dark */
            ag->info &= ~BORG_DARK;

        }
    }


    /* Handle "detect walls" */
    else if (prefix(str, "wall"))
    {
        /* Message */
        borg_note(format("# Detected walls (%d,%d to %d,%d)",
                         q_x, q_y, q_x+1, q_y+1));

        /* Mark detected walls */
        auto_detect_wall[q_y+0][q_x+0] = TRUE;
        auto_detect_wall[q_y+0][q_x+1] = TRUE;
        auto_detect_wall[q_y+1][q_x+0] = TRUE;
        auto_detect_wall[q_y+1][q_x+1] = TRUE;
    }

    /* Handle "detect traps" */
    else if (prefix(str, "trap"))
    {
        /* Message */
        borg_note(format("# Detected traps (%d,%d to %d,%d)",
                         q_x, q_y, q_x+1, q_y+1));

        /* Mark detected traps */
        auto_detect_trap[q_y+0][q_x+0] = TRUE;
        auto_detect_trap[q_y+0][q_x+1] = TRUE;
        auto_detect_trap[q_y+1][q_x+0] = TRUE;
        auto_detect_trap[q_y+1][q_x+1] = TRUE;
    }

    /* Handle "detect doors" */
    else if (prefix(str, "door"))
    {
        /* Message */
        borg_note(format("# Detected doors (%d,%d to %d,%d)",
                         q_x, q_y, q_x+1, q_y+1));

        /* Mark detected doors */
        auto_detect_door[q_y+0][q_x+0] = TRUE;
        auto_detect_door[q_y+0][q_x+1] = TRUE;
        auto_detect_door[q_y+1][q_x+0] = TRUE;
        auto_detect_door[q_y+1][q_x+1] = TRUE;
    }

    /* Handle "detect evil" */
    else if (prefix(str, "evil"))
    {
        /* Message */
        borg_note(format("# Detected evil (%d,%d to %d,%d)",
                         q_x, q_y, q_x+1, q_y+1));

        /* Mark detected walls */
        auto_detect_evil[q_y+0][q_x+0] = TRUE;
        auto_detect_evil[q_y+0][q_x+1] = TRUE;
        auto_detect_evil[q_y+1][q_x+0] = TRUE;
        auto_detect_evil[q_y+1][q_x+1] = TRUE;
    }

    /* Handle "detect traps and doors" */
    else if (prefix(str, "both"))
    {
        /* Message */
        borg_note(format("# Detected traps and doors (%d,%d to %d,%d)",
                         q_x, q_y, q_x+1, q_y+1));

        /* Mark detected traps */
        auto_detect_trap[q_y+0][q_x+0] = TRUE;
        auto_detect_trap[q_y+0][q_x+1] = TRUE;
        auto_detect_trap[q_y+1][q_x+0] = TRUE;
        auto_detect_trap[q_y+1][q_x+1] = TRUE;

        /* Mark detected doors */
        auto_detect_door[q_y+0][q_x+0] = TRUE;
        auto_detect_door[q_y+0][q_x+1] = TRUE;
        auto_detect_door[q_y+1][q_x+0] = TRUE;
        auto_detect_door[q_y+1][q_x+1] = TRUE;
    }

    /* Handle "detect traps and doors and evil" */
    else if (prefix(str, "TDE"))
    {
        /* Message */
        borg_note(format("# Detected traps, doors, and evil (%d,%d to %d,%d)",
                         q_x, q_y, q_x+1, q_y+1));

        /* Mark detected traps */
        auto_detect_trap[q_y+0][q_x+0] = TRUE;
        auto_detect_trap[q_y+0][q_x+1] = TRUE;
        auto_detect_trap[q_y+1][q_x+0] = TRUE;
        auto_detect_trap[q_y+1][q_x+1] = TRUE;

        /* Mark detected doors */
        auto_detect_door[q_y+0][q_x+0] = TRUE;
        auto_detect_door[q_y+0][q_x+1] = TRUE;
        auto_detect_door[q_y+1][q_x+0] = TRUE;
        auto_detect_door[q_y+1][q_x+1] = TRUE;

        /* Mark detected evil */
        auto_detect_evil[q_y+0][q_x+0] = TRUE;
        auto_detect_evil[q_y+0][q_x+1] = TRUE;
        auto_detect_evil[q_y+1][q_x+0] = TRUE;
        auto_detect_evil[q_y+1][q_x+1] = TRUE;
    }

    /* Done */
    return (TRUE);
}



/*
 * Update the Borg based on the current "map"
 */
static void borg_forget_map(void)
{
    int x, y;

    auto_grid *ag;


#ifdef BORG_ROOMS

    /* Wipe the "rooms" */
    borg_wipe_rooms();

#endif


    /* Clean up the grids */
    for (y = 0; y < AUTO_MAX_Y; y++)
    {
        for (x = 0; x < AUTO_MAX_X; x++)
        {
            /* Access the grid */
            ag = &auto_grids[y][x];

            /* Wipe it */
            WIPE(ag, auto_grid);

            /* Lay down the outer walls */
            ag->feat = FEAT_PERM_SOLID;
        }
    }

    /* Clean up the grids */
    for (y = 1; y < AUTO_MAX_Y-1; y++)
    {
        for (x = 1; x < AUTO_MAX_X-1; x++)
        {
            /* Access the grid */
            ag = &auto_grids[y][x];

            /* Forget the contents */
            ag->feat = FEAT_NONE;

            /* Hack -- prepare the town */
            if (!auto_depth) ag->feat = FEAT_FLOOR;
        }
    }


    /* Reset "auto_data_cost" */
    COPY(auto_data_cost, auto_data_hard, auto_data);

    /* Reset "auto_data_flow" */
    COPY(auto_data_flow, auto_data_hard, auto_data);


    /* Clear "auto_data_know" */
    WIPE(auto_data_know, auto_data);

    /* Clear "auto_data_icky" */
    WIPE(auto_data_icky, auto_data);


    /* Forget the view */
    borg_forget_view();
}


static byte Get_f_info_number[256];

/*
 * Update the "map" based on visual info on the screen
 *
 * Note that we make assumptions about the grid under the player,
 * to prevent painful situations such as seeming to be standing
 * in a wall, or on a trap, etc.
 *
 * In general, we use the same "feat" codes as the game itself, but
 * sometimes we are just guessing (as with "visible traps"), and we
 * use some special codes, explained below.
 *
 * Note that we use the "feat" code of "FEAT_NONE" for grids which
 * have never been seen, or which, when seen, have always contained
 * an object or monster.  These grids are probably walls, unless
 * they contain a monster or object, in which case they are probably
 * floors, unless they contain a monster which passes through walls,
 * in which case they are probably walls.
 *
 * Note that we use the "feat" code of "FEAT_FLOOR" for grids which
 * were a normal floor last time we checked.  These grids may have
 * changed into non-floor grids recently (via earthquake?), unless
 * the grid is on the current panel, and is currently "lit" in some
 * manner, and does not contain a monster.
 *
 * Note that we use the "feat" code of "FEAT_INVIS" for grids which
 * once contained a wall/door, but then contained a monster or object.
 * These grids are probably floors, unless the grid contains a monster
 * which can pass through walls, in which case note that missiles and
 * spells may not affect a monster in the grid.
 *
 * Note that we use the other "feat" codes for grids which probably
 * contain the given feature type, unless several feature types use
 * the same symbol, in which case we use some "default" code, changing
 * our guess when messages provide us with more information.  This is
 * especially necessary for distinguishing magma from quartz, and for
 * distinguishing normal doors from locked doors from jammed doors.
 * Note that most "feat" codes, even if they are not "guesses", may
 * not be valid unless the grid is on the current panel.
 *
 * We use the "BORG_MARK" flag to mark a grid as having been "observed",
 * though this may or may not indicate that the "feature" code is known,
 * since observations of monsters or objects via telepathy and/or remote
 * detection may trigger this flag.
 *
 * We use the "BORG_OKAY" flag to mark a grid as being on the current
 * panel, which is used for various things, including verifying that
 * a grid can be used as the destination of a target, and to allow us
 * to assume that off-panel monsters are not "visible".
 *
 * Note the "interesting" code used to learn which floor grids are "dark"
 * and which are "perma-lit", by tracking those floor grids which appear
 * to be "lit", and then marking all of these grids which do not appear
 * to be lit by the torch as "known" to be illuminated, and by marking
 * any grids which "disappear" or which are displayed as "dark floors"
 * as "known" to be "dark".  This leaves many grids, especially those
 * lit by the torch, as being neither lit nor dark.
 *
 * The basic problem is that, especially with no special options set,
 * the player has very little direct information about which grids
 * are perma-lit, since all non-floor grids are memorized when they
 * are seen, and torch-lit floor grids look just like perma-lit
 * floor grids.  Also, monsters hide any object or feature in their
 * grid, and objects hide any feature in their grid, and objects are
 * memorized when they are seen, and monsters can be detected by a
 * variety of methods, including infravision and telepathy.
 *
 * So we ignore most non-floor grids, and we mark any floor grids which
 * are "known" to be perma-lit as "BORG_GLOW", and any which are "known"
 * to be dark as "BORG_DARK".  These flags are used for many purposes,
 * most importantly, to determine when "call lite" would be useful, and
 * to help determine when a monster is standing in a viewable perma-lit
 * grid, and should thus be "visible", and to determine when the player
 * has "lite", even though his torch has gone out.
 *
 * When a "call lite" spell succeeds, we mark the grids around the
 * player as "BORG_GLOW" and not "BORG_DARK", but we do not attempt
 * to "spread" the lite to the entire room, since, in general, it is
 * not possible to know what grids are in the "room".
 *
 * Note that we assume that normally, when the player steps onto
 * something, it disappears, and turns into a normal floor, unless
 * the player is stepping onto a grid which is normally "permanent"
 * (floors, stairs, store doors), in which case it does not change.
 *
 * Note that when we encounter a grid which blocks motion, but which
 * was previously thought to not block motion, we must be sure to
 * remove it from any "flow" which might be in progress, to prevent
 * nasty situations in which we attempt to flow into a wall grid
 * which was thought to be something else, like an unknown grid.
 *
 */
static void borg_update_map(void)
{
    int i, x, y, dx, dy;

    auto_grid *ag;

    byte t_a;
    byte t_c;

    /* Analyze the current map panel */
    for (dy = 0; dy < SCREEN_HGT; dy++)
    {
        /* Direct access XXX XXX XXX */
        byte *aa = &(Term->scr->a[dy+1][13]);
        char *cc = &(Term->scr->c[dy+1][13]);

#ifdef ALLOW_BORG_GRAPHICS
       byte a_trans;
       char c_trans;
#endif /* ALLOW_BORG_GRAPHICS */

        /* Scan the row */
        for (dx = 0; dx < SCREEN_WID; dx++)
        {
            bool old_wall;
            bool new_wall;


            /* Obtain the map location */
            x = w_x + dx;
            y = w_y + dy;


            /* Save contents */
            t_a = *aa++;
            t_c = *cc++;
#ifdef ALLOW_BORG_GRAPHICS

           /* Translate the glyph into an ASCII char */
           a_trans = translate_visuals[(byte)t_a][(byte)t_c].d_attr;
           c_trans = translate_visuals[(byte)t_a][(byte)t_c].d_char;

           if ((a_trans != 0) || (c_trans != 0))
           {
               /* Translation found */
               t_a = a_trans;
               t_c = c_trans;
           }

#endif /* ALLOW_BORG_GRAPHICS */

            /* Get the auto_grid */
            ag = &auto_grids[y][x];

            /* Notice "on-screen" */
            ag->info |= BORG_OKAY;

            /* Notice "knowledge" */
            if (t_c != ' ') ag->info |= BORG_MARK;


            /* Notice the player */
            if (y == p_ptr->py && x == p_ptr->px )
            {
                /* Memorize player location */
                c_x = p_ptr->px;
                c_y = p_ptr->py;

                /* Hack -- white */
                t_a = TERM_WHITE;

                /* Mark this grid as having been stepped on */
                track_step_x[track_step_num] = x;
                track_step_y[track_step_num] = y;
                track_step_num++;

                /* Hack - Clean the steps every so often */
                if (track_step_num > 75)
                {
                    for (i = 0; i < 75; i++)
                    {
                        /* Move each step down one position */
                        track_step_x[i] = track_step_x[i + 1];
                        track_step_y[i] = track_step_y[i + 1];
                    }
                track_step_num = 75;
                }


                /* AJG Just get the char from the features array */
                if (ag->feat != FEAT_NONE)
                    t_c = f_info[ag->feat].d_char;
                else
                    t_c = f_info[FEAT_FLOOR].d_char;
            }


            /* Save the old "wall" or "door" */
            old_wall = !borg_cave_floor_grid(ag);

            /* Analyze symbol */
            /* AJG Adjust for funky graphics */
            switch (Get_f_info_number[t_c])
            {
                /* Darkness */
                case FEAT_NONE:
                {
                    /* The grid is not lit */
                    ag->info &= ~BORG_GLOW;

                    /* Known grids must be dark floors */
                    if (ag->feat != FEAT_NONE) ag->info |= BORG_DARK;

                    /* Done */
                    break;
                }

                /* Floors */
                case FEAT_FLOOR:
                {
                    /* Handle "blind" */
                    if (do_blind)
                    {
                        /* Nothing */
                    }

                    /* Handle "dark" floors */
                    else if (t_a == TERM_L_DARK)
                    {
                        /* Dark floor grid */
                        ag->info |= BORG_DARK;
                        ag->info &= ~BORG_GLOW;
                    }

                    /* Handle "lit" floors */
                    else
                    {
                        if (!my_cur_lite && y !=c_y && x != c_x)
                        {
                            /* Track "lit" floors */
                            auto_temp_y[auto_temp_n] = y;
                            auto_temp_x[auto_temp_n] = x;
                            auto_temp_n++;
                        }
                    }

                    /* Known floor */
                    ag->feat = FEAT_FLOOR;

                    /* Done */
                    break;
                }

                /* Open doors */
                case FEAT_OPEN:
				case FEAT_BROKEN:
                {
                    /* steal info from the game for broken doors */
                    byte feat = cave_feat [y][x];

                    /* Accept broken */
                    if (ag->feat == FEAT_BROKEN) break;

                    /* Cheat the broken into memory */
                    if (feat == FEAT_BROKEN)
                    {
                        ag->feat = FEAT_BROKEN;
                        break;
                    }

                    /* Assume normal */
                    ag->feat = FEAT_OPEN;

                    /* Done */
                    break;
                }

                /* Walls */
                case FEAT_WALL_EXTRA:
                case FEAT_WALL_INNER:
                case FEAT_WALL_OUTER:
                case FEAT_WALL_SOLID:
                case FEAT_PERM_SOLID:
                case FEAT_PERM_EXTRA:
                case FEAT_PERM_INNER:
                case FEAT_PERM_OUTER:
                {
                    /* ok this is a humongo cheat.  He is pulling the
                     * grid information from the game rather than from
                     * his memory.  He is going to see if the wall is perm.
                     * This is a cheat. May the Lord have mercy on my soul.
                     *
                     * The only other option is to have him "dig" on each
                     * and every granite wall to see if it is perm.  Then he
                     * can mark it as a non-perm.  However, he would only have
                     * to dig once and only in a range of spaces near the
                     * center of the map.  Since perma-walls are located in
                     * vaults and vaults have a minimum size.  So he can avoid
                     * digging on walls that are, say, 10 spaces from the edge
                     * of the map.  He can also limit the dig by his depth.
                     * Vaults are found below certain levels and with certain
                     * "feelings."  Can be told not to dig on boring levels
                     * and not before level 50 or whatever.
                     *
                     * Since the code to dig slows the borg down a lot.
                     * (Found in borg6.c in _flow_dark_interesting()) We will
                     * limit his capacity to search.  We will set a flag on
                     * the level is perma grids are found.
                     */
                    byte feat = cave_feat[y][x];

                    /* forget previously located walls */
                    if (ag->feat == FEAT_PERM_INNER) break;

                    /* is it a perma grid? */
                    if (feat == FEAT_PERM_INNER)
                    {
                        ag->feat = FEAT_PERM_INNER;
                        vault_on_level = TRUE;
                        break;
                    }
                    /* is it a non vault perma grid? */
                    if (feat >= FEAT_PERM_OUTER)
                    {
                        ag->feat = FEAT_PERM_SOLID;
                        break;
                    }
                    /* Accept non-granite */
                    if (ag->feat >= FEAT_WALL_EXTRA &&
                        ag->feat <= FEAT_PERM_EXTRA) break;

                    /* Assume granite */
                    ag->feat = FEAT_WALL_EXTRA;

                    /* Done */
                    break;
                }

                /* Seams */
                case FEAT_MAGMA:
                case FEAT_QUARTZ:
                {
                    /* Accept quartz */
                    if (ag->feat == FEAT_QUARTZ) break;

                    /* Assume magma */
                    ag->feat = FEAT_MAGMA;

                    /* Done */
                    break;
                }

                /* Hidden */
                case FEAT_MAGMA_K:
                case FEAT_QUARTZ_K:
                {
                    /* Accept quartz */
                    if (ag->feat == FEAT_QUARTZ_K) break;

                    /* Assume magma */
                    ag->feat = FEAT_MAGMA_K;

                    /* Done */
                    break;
                }

                /* Rubble */
                case FEAT_RUBBLE:
                {
                    /* Assume rubble */
                    ag->feat = FEAT_RUBBLE;

                    /* Done */
                    break;
                }

                /* Doors */
                case FEAT_DOOR_HEAD:
                case FEAT_DOOR_HEAD+1:
                case FEAT_DOOR_HEAD+2:
                case FEAT_DOOR_HEAD+3:
                case FEAT_DOOR_HEAD+4:
                case FEAT_DOOR_HEAD+5:
                case FEAT_DOOR_HEAD+6:
                case FEAT_DOOR_HEAD+7:
                case FEAT_DOOR_HEAD+8:
                case FEAT_DOOR_HEAD+9:
                case FEAT_DOOR_HEAD+10:
                case FEAT_DOOR_HEAD+11:
                case FEAT_DOOR_HEAD+12:
                case FEAT_DOOR_HEAD+13:
                case FEAT_DOOR_HEAD+14:
                case FEAT_DOOR_TAIL:
                {
                    /* Accept closed and locked and jammed doors */
                    if ((ag->feat >= FEAT_DOOR_HEAD) && (ag->feat <= FEAT_DOOR_TAIL)) break;

                    /* Assume easy */
                    ag->feat = FEAT_DOOR_HEAD + 0x00;

                    /* Done */
                    break;
                }

                /* Traps */
                case FEAT_TRAP_HEAD:
                case FEAT_TRAP_HEAD+1:
                case FEAT_TRAP_HEAD+2:
                case FEAT_TRAP_HEAD+3:
                case FEAT_TRAP_HEAD+4:
                case FEAT_TRAP_HEAD+5:
                case FEAT_TRAP_HEAD+6:
                case FEAT_TRAP_HEAD+7:
                case FEAT_TRAP_HEAD+8:
                case FEAT_TRAP_HEAD+9:
                case FEAT_TRAP_HEAD+10:
                case FEAT_TRAP_HEAD+11:
                case FEAT_TRAP_HEAD+12:
                case FEAT_TRAP_HEAD+13:
                case FEAT_TRAP_HEAD+14:
                case FEAT_TRAP_TAIL:
                {
                    /* Minor cheat for the borg.  If the borg is running
                     * in the graphics mode (not the AdamBolt Tiles) he will
                     * mis-id the glyph of warding as a trap
                     */
                    byte feat = cave_feat[y][x];
                    if (feat == FEAT_GLYPH)
                    {
                        ag->feat = FEAT_GLYPH;
                        /* Check for an existing glyph */
                        for (i = 0; i < track_glyph_num; i++)
                        {
                            /* Stop if we already new about this glyph */
                            if ((track_glyph_x[i] == x) && (track_glyph_y[i] == y)) break;
                        }

                        /* Track the newly discovered glyph */
                        if ((i == track_glyph_num) && (i < track_glyph_size))
                        {
                            track_glyph_x[i] = x;
                            track_glyph_y[i] = y;
                            track_glyph_num++;
                        }

                        /* done */
                        break;
                    }

                    /* Assume trap door */
                    ag->feat = FEAT_TRAP_HEAD + 0x00;

                    /* Done */
                    break;
                }

                /* glyph of warding stuff here,  */
                case FEAT_GLYPH:
                {
                    ag->feat = FEAT_GLYPH;

                    /* Check for an existing glyph */
                    for (i = 0; i < track_glyph_num; i++)
                    {
                        /* Stop if we already new about this glyph */
                        if ((track_glyph_x[i] == x) && (track_glyph_y[i] == y)) break;
                    }

                    /* Track the newly discovered glyph */
                    if ((i == track_glyph_num) && (i < track_glyph_size))
                    {
                        track_glyph_x[i] = x;
                        track_glyph_y[i] = y;
                        track_glyph_num++;
                    }

                    /* done */
                    break;
                }

                /* Up stairs */
                case FEAT_LESS:
                {
                    /* Obvious */
                    ag->feat = FEAT_LESS;

                    /* Check for an existing "up stairs" */
                    for (i = 0; i < track_less_num; i++)
                    {
                        /* Stop if we already new about these stairs */
                        if ((track_less_x[i] == x) && (track_less_y[i] == y)) break;
                    }

                    /* Track the newly discovered "up stairs" */
                    if ((i == track_less_num) && (i < track_less_size))
                    {
                        track_less_x[i] = x;
                        track_less_y[i] = y;
                        track_less_num++;
                    }

                    /* Done */
                    break;
                }

                /* Down stairs */
                case FEAT_MORE:
                {
                    /* Obvious */
                    ag->feat = FEAT_MORE;

                    /* Check for an existing "down stairs" */
                    for (i = 0; i < track_more_num; i++)
                    {
                        /* We already knew about that one */
                        if ((track_more_x[i] == x) && (track_more_y[i] == y)) break;
                    }

                    /* Track the newly discovered "down stairs" */
                    if ((i == track_more_num) && (i < track_more_size))
                    {
                        track_more_x[i] = x;
                        track_more_y[i] = y;
                        track_more_num++;
                    }

                    /* Done */
                    break;
                }

                /* Store doors */
                case FEAT_SHOP_HEAD:
                case FEAT_SHOP_HEAD+1:
                case FEAT_SHOP_HEAD+2:
                case FEAT_SHOP_HEAD+3:
                case FEAT_SHOP_HEAD+4:
                case FEAT_SHOP_HEAD+5:
                case FEAT_SHOP_HEAD+6:
                case FEAT_SHOP_TAIL:

                {
                    /* Shop type */
                    i = D2I(t_c) - 1;

                    /* Obvious */
                    ag->feat = FEAT_SHOP_HEAD + i;

                    /* Save new information */
                    track_shop_x[i] = x;
                    track_shop_y[i] = y;

                    /* Done */
                    break;
                }

                /* Monsters/Objects */
                default:
                {
                    auto_wank *wank;

                    /* Access next wank, advance */
                    wank = &auto_wanks[auto_wank_num++];

                    /* Save some information */
                    wank->x = x;
                    wank->y = y;
                    wank->t_a = t_a;
                    wank->t_c = t_c;
                    wank->is_take = auto_is_take[(byte)(t_c)];
                    wank->is_kill = auto_is_kill[(byte)(t_c)];

                    /* mark old unknow squares as possible floor grids */
                    if (ag->feat == FEAT_NONE)
                        ag->feat = FEAT_INVIS;

                    /* Mark old wall/door grids as probable floor grids */
                    if (!borg_cave_floor_grid(ag))
                        ag->feat = FEAT_INVIS;

                    /* Done */
                    break;
                }
            }

            /* Save the new "wall" or "door" */
            new_wall = !borg_cave_floor_grid(ag);

            /* Notice wall changes */
            if (old_wall != new_wall)
            {
                /* Remove this grid from any flow */
                if (new_wall) auto_data_flow->data[y][x] = 255;

                /* Remove this grid from any flow */
                auto_data_know->data[y][x] = FALSE;

                /* Remove this grid from any flow */
                auto_data_icky->data[y][x] = FALSE;

                /* Recalculate the view (if needed) */
                if (ag->info & BORG_VIEW) auto_update_view = TRUE;

                /* Recalculate the lite (if needed) */
                if (ag->info & BORG_LITE) auto_update_lite = TRUE;
            }
        }
    }
}



/*
 * Look at the screen and update the borg
 *
 * Uses the "panel" info (w_x, w_y) obtained earlier
 *
 * Note that all the "important" messages that occured after our last
 * action have been "queued" in a usable form.  We must attempt to use
 * these messages to update our knowledge about the world, keeping in
 * mind that the world may have changed in drastic ways.
 *
 * Note that the "c_t" variable corresponds *roughly* to player turns,
 * except that resting and "repeated" commands count as a single turn,
 * and "free" moves (including "illegal" moves, such as attempted moves
 * into walls, or tunneling into monsters) are counted as turns.
 *
 * Also note that "c_t" is not incremented until the Borg is about to
 * do something, so nothing ever has a time-stamp of the current time.
 *
 * We rely on the fact that all "perma-lit" grids are memorized when
 * they are seen, so any grid on the current panel that appears "dark"
 * must not be perma-lit.
 *
 * We rely on the fact that all "objects" are memorized when they are
 * seen, so any grid on the current panel that appears "dark" must not
 * have an object in it.  But it could have a monster which looks like
 * an object, but this is very rare.  XXX XXX XXX
 *
 * XXX XXX XXX The basic problem with timestamping the monsters
 * and objects is that we often get a message about a monster, and so
 * we want to timestamp it, but then we cannot use the timestamp to
 * indicate that the monster has not been "checked" yet.  Perhaps
 * we need to do something like give each monster a "moved" flag,
 * and clear all the flags to FALSE each turn before tracking. (?)
 *
 * Note that when two monsters of the same race are standing next to
 * each other, and they both move, such that the second monster ends
 * up where the first one began, we will incorrectly assume that the
 * first monster stayed still, and either the second monster moved
 * two spaces, or the second monster disappeared and a third monster
 * appeared, which is technically possible, if the first monster ate
 * the second, and then cloned the third.
 *
 * There is a problem with monsters which look like objects, namely,
 * they are assumed to be objects, and then if they leave line of
 * sight, they disappear, and the Borg assumes that they are gone,
 * when really they should be identified as monsters.
 *
 * XXX XXX Hack -- note the fast direct access to the screen.
 */
void borg_update(void)
{
    int i, k, x, y, dx, dy;

    int hit_dist;

    cptr msg;

    cptr what;

    auto_grid *ag;

    bool reset = FALSE;


    /*** Process objects/monsters ***/

    /* Scan monsters */
    for (i = 1; i < auto_kills_nxt; i++)
    {
        auto_kill *kill = &auto_kills[i];

        /* Skip dead monsters */
        if (!kill->r_idx) continue;

        /* Clear flags */
        kill->seen = FALSE;
        kill->used = FALSE;

        /* Skip recently seen monsters */
        if (c_t - kill->when < 2000) continue;

        /* Note */
        borg_note(format("# Expiring a monster '%s' (%d) at (%d,%d)",
                         (r_name + r_info[kill->r_idx].name), kill->r_idx,
                         kill->x, kill->y));

        /* Kill the monster */
        borg_delete_kill(i);
    }

    /* Scan objects */
    for (i = 1; i < auto_takes_nxt; i++)
    {
        auto_take *take = &auto_takes[i];

        /* Skip dead objects */
        if (!take->k_idx) continue;

        /* Clear flags */
        take->seen = FALSE;

        /* Skip recently seen objects */
        if (c_t - take->when < 2000) continue;

        /* Note */
        borg_note(format("# Expiring an object '%s' (%d) at (%d,%d)",
                         (k_name + k_info[take->k_idx].name), take->k_idx,
                         take->x, take->y));

        /* Kill the object */
        borg_delete_take(i);
    }

    /*** Handle messages ***/

    /* Process messages */
    for (i = 0; i < auto_msg_num; i++)
    {
        /* Get the message */
        msg = auto_msg_buf + auto_msg_pos[i];

        /* Note the message */
        borg_note(format("# %s (+)", msg));
    }

    /* Process messages */
    for (i = 0; i < auto_msg_num; i++)
    {
        /* Skip parsed messages */
        if (auto_msg_use[i]) continue;

        /* Get the message */
        msg = auto_msg_buf + auto_msg_pos[i];

        /* Get the arguments */
        what = strchr(msg, ':') + 1;

        /* Hack -- Handle "SELF" info */
        if (prefix(msg, "SELF:"))
        {
            (void)borg_handle_self(what);
            auto_msg_use[i] = 1;
        }

        /* Handle "You feel..." */
        else if (prefix(msg, "FEELING:"))
        {
            auto_feeling = atoi(what);
            auto_msg_use[i] = 1;
        }
    }

    /* Process messages */
    for (i = 0; i < auto_msg_num; i++)
    {
        /* Skip parsed messages */
        if (auto_msg_use[i]) continue;

        /* Get the message */
        msg = auto_msg_buf + auto_msg_pos[i];

        /* Get the arguments */
        what = strchr(msg, ':') + 1;

        /* Handle "You hit xxx." */
        if (prefix(msg, "HIT:"))
        {
            /* Attempt to find the monster */
            if ((k = borg_locate_kill(what, g_y, g_x, 0)) > 0)
            {
                auto_msg_use[i] = 2;
            }
        }

        /* Handle "You miss xxx." */
        else if (prefix(msg, "MISS:"))
        {
            /* Attempt to find the monster */
            if ((k = borg_locate_kill(what, g_y, g_x, 0)) > 0)
            {
                auto_msg_use[i] = 2;
            }
        }

        /* Handle "You have killed xxx." */
        else if (prefix(msg, "KILL:"))
        {
            /* Attempt to find the monster */
            if ((k = borg_locate_kill(what, g_y, g_x, 0)) > 0)
            {
                borg_count_death(k);

                borg_delete_kill(k);
                auto_msg_use[i] = 2;
                /* reset the panel.  He's on a roll */
                time_this_panel = 1;
            }
        /* Shooting through darkness worked */
        if (successful_target == -1) successful_target = 2;

        }

        /* Handle "The xxx disappears!"  via teleport other, and blinks away */
        else if (prefix(msg, "KILL:"))
        {
            /* Attempt to find the monster */
            if ((k = borg_locate_kill(what, g_y, g_x, 0)) > 0)
            {
                borg_count_death(k);
                borg_delete_kill(k);
                auto_msg_use[i] = 2;
                /* reset the panel.  He's on a roll */
                time_this_panel = 1;
            }
        /* Shooting through darkness worked */
        if (successful_target == -1) successful_target = 2;
        }

        /* Handle "xxx dies." */
        else if (prefix(msg, "DIED:"))
        {
            /* Attempt to find the monster */
            if ((k = borg_locate_kill(what, g_y, g_x, 3)) > 0)
            {
                borg_count_death(k);
                borg_delete_kill(k);
                auto_msg_use[i] = 2;
                /* reset the panel.  He's on a roll */
                time_this_panel = 1;
            }
        /* Shooting through darkness worked */
        if (successful_target == -1) successful_target = 2;
        }

        /* Handle "xxx screams in pain." */
        else if (prefix(msg, "PAIN:"))
        {
            /* Attempt to find the monster */
            if ((k = borg_locate_kill(what, g_y, g_x, 3)) > 0)
            {
                auto_msg_use[i] = 2;
            }
        /* Shooting through darkness worked */
        if (successful_target == -1) successful_target = 2;

        }

        /* Handle "sleep" */
        else if (prefix(msg, "STATE__FEAR:"))
        {
            /* Attempt to find the monster */
            if ((k = borg_locate_kill(what, g_y, g_x, 0)) > 0)
            {
                auto_msg_use[i] = 2;
            }
        }

        /* Handle "sleep" */
        else if (prefix(msg, "STATE__BOLD:"))
        {
            /* Attempt to find the monster */
            if ((k = borg_locate_kill(what, g_y, g_x, 0)) > 0)
            {
                auto_msg_use[i] = 2;
            }
        }
    }

    /* Process messages */
    /* getting distance to allow for 'hit's */
    hit_dist = 1;
    for (i = 0; i < auto_msg_num; i++)
    {
        /* Skip parsed messages */
        if (auto_msg_use[i]) continue;

        /* Get the message */
        msg = auto_msg_buf + auto_msg_pos[i];

        /* if you have moved then do not count the monsters as unknown */
        /* unless they are very far away */
        if (prefix(msg, "SPELL_168") ||
            prefix(msg, "SPELL_169"))
        {
            hit_dist = 100;
            break;
        }

        /* monsters move from earthquake */
        if (prefix(msg, "QUAKE"))
        {
            hit_dist = 3;
            break;
        }
    }

    /* Process messages */
    for (i = 0; i < auto_msg_num; i++)
    {
        /* Skip parsed messages */
        if (auto_msg_use[i]) continue;

        /* Get the message */
        msg = auto_msg_buf + auto_msg_pos[i];

        /* Get the arguments */
        what = strchr(msg, ':') + 1;

        /* Handle "You hit xxx." */
        if (prefix(msg, "HIT:"))
        {
            /* Attempt to find the monster */
            if ((k = borg_locate_kill(what, g_y, g_x, hit_dist)) > 0)
            {
                auto_msg_use[i] = 3;
            }
        }

        /* Handle "You miss xxx." */
        else if (prefix(msg, "MISS:"))
        {
            /* Attempt to find the monster */
            if ((k = borg_locate_kill(what, g_y, g_x, hit_dist)) > 0)
            {
                auto_msg_use[i] = 3;
            }
        }

        /* Handle "You have killed xxx." */
        else if (prefix(msg, "KILL:"))
        {
            /* Attempt to find the monster */
            if ((k = borg_locate_kill(what, g_y, g_x, 1)) > 0)
            {
                borg_count_death(k);
                borg_delete_kill(k);
                auto_msg_use[i] = 3;
                /* reset the panel.  He's on a roll */
                time_this_panel = 1;
            }
        /* Shooting through darkness worked */
        if (successful_target == -1) successful_target = 2;
        }

        /* Handle "The xxx disappears!"  via teleport other */
        else if (prefix(msg, "KILL:"))
        {
            /* Attempt to find the monster */
            if ((k = borg_locate_kill(what, g_y, g_x, 1)) > 0)
            {
                borg_count_death(k);
                borg_delete_kill(k);
                auto_msg_use[i] = 3;
                /* reset the panel.  He's on a roll */
                time_this_panel = 1;
            }
        /* Shooting through darkness worked */
        if (successful_target == -1) successful_target = 2;
        }


        /* Handle "xxx dies." */
        else if (prefix(msg, "DIED:"))
        {
            /* Attempt to find the monster */
            if ((k = borg_locate_kill(what, o_c_y, o_c_x, 20)) > 0)
            {
                borg_count_death(k);
                borg_delete_kill(k);
                auto_msg_use[i] = 3;
                /* reset the panel.  He's on a roll */
                time_this_panel = 1;
            }
        /* Shooting through darkness worked */
        if (successful_target == -1) successful_target = 2;
        }

        /* Handle "xxx screams in pain." */
        else if (prefix(msg, "PAIN:"))
        {
            /* Attempt to find the monster */
            if ((k = borg_locate_kill(what, o_c_y, o_c_x, 20)) > 0)
            {
                auto_msg_use[i] = 3;
            }
        /* Shooting through darkness worked */
        if (successful_target == -1) successful_target = 2;
        }

        /* Handle "xxx hits you." */
        else if (prefix(msg, "HIT_BY:"))
        {
            /* Attempt to find the monster */
            if ((k = borg_locate_kill(what, o_c_y, o_c_x, 1)) > 0)
            {
                auto_msg_use[i] = 3;
            }
        }

        /* Handle "xxx misses you." */
        else if (prefix(msg, "MISS_BY:"))
        {
            /* Attempt to find the monster */
            if ((k = borg_locate_kill(what, o_c_y, o_c_x, 1)) > 0)
            {
                auto_msg_use[i] = 3;
            }
        }

        /* Handle "sleep" */
        else if (prefix(msg, "STATE_SLEEP:"))
        {
            /* Attempt to find the monster */
            if ((k = borg_locate_kill(what, o_c_y, o_c_x, 20)) > 0)
            {
                borg_sleep_kill(k);
                auto_msg_use[i] = 3;
            }
        }

        /* Handle "awake" */
        else if (prefix(msg, "STATE_AWAKE:"))
        {
            /* Attempt to find the monster */
            if ((k = borg_locate_kill(what, o_c_y, o_c_x, 20)) > 0)
            {
                auto_msg_use[i] = 3;
            }
        }

        /* Handle "sleep" */
        else if (prefix(msg, "STATE__FEAR:"))
        {
            /* Attempt to find the monster */
            if ((k = borg_locate_kill(what, o_c_y, o_c_x, 20)) > 0)
            {
                auto_msg_use[i] = 3;
            }
        }

        /* Handle "sleep" */
        else if (prefix(msg, "STATE__BOLD:"))
        {
            /* Attempt to find the monster */
            if ((k = borg_locate_kill(what, o_c_y, o_c_x, 20)) > 0)
            {
                auto_msg_use[i] = 3;
            }
        }

        /* Hack -- Handle "spell" */
        else if (prefix(msg, "SPELL_"))
        {
            /* Attempt to find the monster */
            if ((k = borg_locate_kill(what, o_c_y, o_c_x, 20)) > 0)
            {
                auto_msg_use[i] = 3;
            }
        }
    }


    /*** Handle new levels ***/

    /* Hack -- note new levels */
    if (old_depth != auto_depth)
    {
        /* if we are not leaving town increment time since town clock */
        if (!old_depth)
            auto_time_town = 0;
        else
            auto_time_town += c_t - auto_began;

        /* Hack -- Restart the clock */
        c_t = 1000;

        /* reset our panel clock */
        time_this_panel =1;

        /* reset our vault/unique check */
        vault_on_level = FALSE;
        unique_on_level = FALSE;

        /* reset our breeder flag */
        breeder_level = FALSE;

        /* reset our need to see inviso clock */
        need_see_inviso = 1;

        /* reset our 'shoot in the dark' flag */
        successful_target = 0;

        /* When level was begun */
        auto_began = c_t;

        /* Not completed */
        auto_completed = FALSE;

        /* New danger thresh-hold */
        avoidance = auto_chp;

        /* Wipe the danger */
        auto_danger_wipe = TRUE;

        /* Update some stuff */
        auto_update_view = TRUE;
        auto_update_lite = TRUE;

        /* Examine the world */
        auto_do_inven = TRUE;
        auto_do_equip = TRUE;
        auto_do_spell = TRUE;
        auto_do_panel = TRUE;
        auto_do_frame = TRUE;

        /* Enable some functions */
        auto_do_crush_junk = TRUE;
        auto_do_crush_hole = TRUE;
        auto_do_crush_slow = TRUE;

        /* Mega-Hack -- Clear "call lite" stamp */
        when_call_lite = 0;

        /* Mega-Hack -- Clear "wizard lite" stamp */
        when_wizard_lite = 0;

        /* Mega-Hack -- Clear "detect traps" stamp */
        when_detect_traps = 0;

        /* Mega-Hack -- Clear "detect doors" stamp */
        when_detect_doors = 0;

        /* Mega-Hack -- Clear "detect walls" stamp */
        when_detect_walls = 0;

        /* Mega-Hack -- Clear "detect evil" stamp */
        when_detect_evil = 0;

        /* Hack -- Clear "panel" flags */
        for (y = 0; y < 6; y++)
        {
            for (x = 0; x < 6; x++)
            {
                auto_detect_wall[y][x] = FALSE;
                auto_detect_trap[y][x] = FALSE;
                auto_detect_door[y][x] = FALSE;
                auto_detect_evil[y][x] = FALSE;
            }
        }

        /* Hack -- Clear "fear" */
        for (y = 0; y < 6; y++)
        {
            for (x = 0; x < 18; x++)
            {
                auto_fear_region[y][x] = 0;
            }
        }

        /* Hack -- Clear "shop visit" stamps */
        for (i = 0; i < (MAX_STORES ); i++) auto_shops[i].when = 0;

        /* No goal yet */
        goal = 0;

        /* Hack -- Clear "shop" goals */
        goal_shop = goal_ware = goal_item = -1;

        /* Do not use any stairs */
        stair_less = stair_more = FALSE;

        /* Hack -- cannot rise past town */
        if (!auto_depth) goal_rising = FALSE;

        /* Assume not leaving the level */
        goal_leaving = FALSE;

        /* Assume not fleeing the level */
        goal_fleeing = FALSE;

        /* Assume not ignoring monsters */
        goal_ignoring = FALSE;

        /* Went through entire level without running off level */
        finished_level = FALSE;


        /* -1 is unknown. */
        borg_ready_morgoth = -1;

        /* No known stairs */
        track_less_num = 0;
        track_more_num = 0;

        /* No known glyph */
        track_glyph_num = 0;

        /* No known steps */
        track_step_num = 0;

        /* No known closed doors */
        track_door_num = 0;

        /* No objects here */
        auto_takes_cnt = 0;
        auto_takes_nxt = 1;

        /* Forget old objects */
        C_WIPE(auto_takes, 256, auto_take);

        /* No monsters here */
        auto_kills_cnt = 0;
        auto_kills_nxt = 1;

        /* Forget old monsters */
        C_WIPE(auto_kills, 256, auto_kill);

        /* Hack -- Forget race counters */
        C_WIPE(auto_race_count, MAX_R_IDX, s16b);

        /* Forget the map */
        borg_forget_map();

        /* Reset */
        reset = TRUE;

        /* wipe out bad artifacts list */
        for (i = 0; i < 50; i++)
        {
            bad_obj_x[i] = -1;
            bad_obj_y[i] = -1;
        }

        /* Save new depth */
        old_depth = auto_depth;

        /* save once per level */
        if (auto_flag_save) borg_save = TRUE;

        borg_times_twitch = 0;
    }

    /* Handle old level */
    else
    {
        /* reduce GOI count. NOTE: do not reduce below 1.  That is done */
        /* when the spell is cast. */
        /* !FIX need to take speed into account AJG */
        if (borg_goi > 1) borg_goi--;

        /* Reduce fear over time */
        if (!(c_t % 10))
        {
            for (y = 0; y < 6; y++)
            {
                for (x = 0; x < 18; x++)
                {
                    if (auto_fear_region[y][x]) auto_fear_region[y][x]--;
                }
            }
        }

        /* Handle changing map panel */
        if ((o_w_x != w_x) || (o_w_y != w_y))
        {
            /* Forget the previous map panel */
            for (dy = 0; dy < SCREEN_HGT; dy++)
            {
                for (dx = 0; dx < SCREEN_WID; dx++)
                {
                    /* Access the actual location */
                    x = o_w_x + dx;
                    y = o_w_y + dy;

                    /* Get the auto_grid */
                    ag = &auto_grids[y][x];

                    /* Clear the "okay" field */
                    ag->info &= ~BORG_OKAY;
                }
            }

            /* Time stamp this new panel-- to avoid a repeated motion bug */
            time_this_panel = 1;

        }
    }


    /*** Update the map ***/

    /* Track floors */
    auto_temp_n = 0;

    /* Update the map */
    borg_update_map();

    /* Reset */
    if (reset)
    {
        /* Fake old panel */
        o_w_x = w_x;
        o_w_y = w_y;

        /* Fake old location */
        o_c_x = c_x;
        o_c_y = c_y;

        /* Fake goal location */
        g_x = c_x;
        g_y = c_y;
    }

    /* Player moved */
    if ((o_c_x != c_x) || (o_c_y != c_y))
    {
        /* Update view */
        auto_update_view = TRUE;

        /* Update lite */
        auto_update_lite = TRUE;

        /* Assume I can shoot here */
        successful_target = 0;
    }

    /* Update the view */
    if (auto_update_view)
    {
        /* Update the view */
        borg_update_view();

        /* Take note */
        auto_update_view = FALSE;
    }

    /* Update the lite */
    if (auto_update_lite)
    {
        /* Update the lite */
        borg_update_lite();

        /* Take note */
        auto_update_lite = FALSE;
    }

    /* Examine "lit" grids */
    for (i = 0; i < auto_temp_n; i++)
    {
        /* Get location */
        x = auto_temp_x[i];
        y = auto_temp_y[i];

        /* Get the auto_grid */
        ag = &auto_grids[y][x];

        /* Skip torch-lit grids */
        if (ag->info & BORG_LITE) continue;

        /* Assume not dark */
        ag->info &= ~BORG_DARK;

        /* Assume perma-lit */
        if (y !=c_y && x != c_x) ag->info |= BORG_GLOW;
    }

    /*** Track objects and monsters ***/

    /* Pass 1 -- stationary monsters */
    for (i = auto_wank_num - 1; i >= 0; i--)
    {
        auto_wank *wank = &auto_wanks[i];

        /* Track stationary monsters */
        if (wank->is_kill &&
            observe_kill_move(wank->y, wank->x, 0, wank->t_a, wank->t_c, FALSE))
        {
            /* Hack -- excise the entry */
            auto_wanks[i] = auto_wanks[--auto_wank_num];
        }
    }
    /* Pass 2 -- stationary objects */
    for (i = auto_wank_num - 1; i >= 0; i--)
    {
        auto_wank *wank = &auto_wanks[i];

        /* Track stationary objects */
        if (wank->is_take &&
            observe_take_move(wank->y, wank->x, 0, wank->t_a, wank->t_c))
        {
            /* Hack -- excise the entry */
            auto_wanks[i] = auto_wanks[--auto_wank_num];
        }
    }
    /* Pass 3a -- moving monsters (distance 1) */
    for (i = auto_wank_num - 1; i >= 0; i--)
    {
        auto_wank *wank = &auto_wanks[i];

        /* Track moving monsters */
        if (wank->is_kill &&
            observe_kill_move(wank->y, wank->x, 1, wank->t_a, wank->t_c, FALSE))
        {
            /* Hack -- excise the entry */
            auto_wanks[i] = auto_wanks[--auto_wank_num];
        }
    }
    /* Pass 3b -- moving monsters (distance 2) */
    for (i = auto_wank_num - 1; i >= 0; i--)
    {
        auto_wank *wank = &auto_wanks[i];

        /* Track moving monsters */
        if (wank->is_kill &&
            observe_kill_move(wank->y, wank->x, 2, wank->t_a, wank->t_c, FALSE))
        {
            /* Hack -- excise the entry */
            auto_wanks[i] = auto_wanks[--auto_wank_num];
        }
    }
    /* Pass 3c -- moving monsters (distance 3) */
    for (i = auto_wank_num - 1; i >= 0; i--)
    {
        auto_wank *wank = &auto_wanks[i];

        /* Track moving monsters */
        if (wank->is_kill &&
            observe_kill_move(wank->y, wank->x, 3, wank->t_a, wank->t_c, FALSE))
        {
            /* Hack -- excise the entry */
            auto_wanks[i] = auto_wanks[--auto_wank_num];
        }
    }
    /* Pass 3d -- moving monsters (distance 3, allow changes) */
    for (i = auto_wank_num - 1; i >= 0; i--)
    {
        auto_wank *wank = &auto_wanks[i];

        /* Track moving monsters */
        if (wank->is_kill &&
            observe_kill_move(wank->y, wank->x, 3, wank->t_a, wank->t_c, TRUE))
        {
            /* Hack -- excise the entry */
            auto_wanks[i] = auto_wanks[--auto_wank_num];
        }
    }
    /* Pass 4 -- new objects */
    for (i = auto_wank_num - 1; i >= 0; i--)
    {
        auto_wank *wank = &auto_wanks[i];

        /* Track new objects */
        if (wank->is_take &&
            observe_take_diff(wank->y, wank->x, wank->t_a, wank->t_c))
        {
            /* Hack -- excise the entry */
            auto_wanks[i] = auto_wanks[--auto_wank_num];
        }
    }
    /* Pass 5 -- new monsters */
    for (i = auto_wank_num - 1; i >= 0; i--)
    {
        auto_wank *wank = &auto_wanks[i];

        /* Track new monsters */
        if (wank->is_kill &&
            observe_kill_diff(wank->y, wank->x, wank->t_a, wank->t_c))
        {
            /* Hack -- excise the entry */
            auto_wanks[i] = auto_wanks[--auto_wank_num];
        }
    }


    /*** Handle messages ***/

    /* Process messages */
    for (i = 0; i < auto_msg_num; i++)
    {
        /* Skip parsed messages */
        if (auto_msg_use[i]) continue;

        /* Get the message */
        msg = auto_msg_buf + auto_msg_pos[i];

        /* Get the arguments */
        what = strchr(msg, ':') + 1;

        /* Handle "xxx dies." */
        if (prefix(msg, "DIED:"))
        {
            /* Attempt to find the monster */
            if ((k = borg_locate_kill(what, c_y, c_x, 20)) > 0)
            {
                borg_count_death(k);
                borg_delete_kill(k);
                auto_msg_use[i] = 4;
            }
        }

        /* Handle "xxx screams in pain." */
        else if (prefix(msg, "PAIN:"))
        {
            /* Attempt to find the monster */
            if ((k = borg_locate_kill(what, c_y, c_x, 20)) > 0)
            {
                auto_msg_use[i] = 4;
            }
        }

        /* Handle "xxx hits you." */
        else if (prefix(msg, "HIT_BY:"))
        {
            /* Attempt to find the monster */
            if ((k = borg_locate_kill(what, c_y, c_x, hit_dist)) > 0)
            {
                auto_msg_use[i] = 4;
            }
        }

        /* Handle "xxx misses you." */
        else if (prefix(msg, "MISS_BY:"))
        {
            /* Attempt to find the monster */

            if ((k = borg_locate_kill(what, c_y, c_x, hit_dist)) > 0)
            {
                auto_msg_use[i] = 4;
            }
        }

        /* Handle "sleep" */
        else if (prefix(msg, "STATE_SLEEP:"))
        {
            /* Attempt to find the monster */
            if ((k = borg_locate_kill(what, c_y, c_x, 20)) > 0)
            {
                borg_sleep_kill(k);
                auto_msg_use[i] = 4;
            }
        }

        /* Handle "awake" */
        else if (prefix(msg, "STATE_AWAKE:"))
        {
            /* Attempt to find the monster */
            if ((k = borg_locate_kill(what, c_y, c_x, 20)) > 0)
            {
                auto_msg_use[i] = 4;
            }
        }

        /* Handle "sleep" */
        else if (prefix(msg, "STATE__FEAR:"))
        {
            /* Attempt to find the monster */
            if ((k = borg_locate_kill(what, c_y, c_x, 20)) > 0)
            {
                auto_msg_use[i] = 4;
            }
        }

        /* Handle "sleep" */
        else if (prefix(msg, "STATE__BOLD:"))
        {
            /* Attempt to find the monster */
            if ((k = borg_locate_kill(what, c_y, c_x, 20)) > 0)
            {
                auto_msg_use[i] = 4;
            }
        }

        /* Hack -- Handle "spell" */
        else if (prefix(msg, "SPELL_"))
        {
            /* Attempt to find the monster */
            if ((k = borg_locate_kill(what, c_y, c_x, 20)) > 0)
            {
                auto_msg_use[i] = 4;
            }
        }
    }
    /* Process messages */
    for (i = 0; i < auto_msg_num; i++)
    {
        /* Skip parsed messages */
        if (auto_msg_use[i]) continue;

        /* Get the message */
        msg = auto_msg_buf + auto_msg_pos[i];

        /* Get the arguments */
        what = strchr(msg, ':') + 1;

        /* Handle "xxx hits you." */
        if (prefix(msg, "HIT_BY:"))
        {
            borg_fear_grid(what, c_y, c_x, 4 * ((auto_depth / 5) + 1));
            auto_msg_use[i] = 5;
        }

        /* Handle "xxx misses you." */
        else if (prefix(msg, "MISS_BY:"))
        {
            borg_fear_grid(what, c_y, c_x, 2 * ((auto_depth / 5) + 1));
            auto_msg_use[i] = 5;
        }

        /* Hack -- Handle "spell" */
        else if (prefix(msg, "SPELL_"))
        {
            borg_fear_grid(what, c_y, c_x, borg_fear_spell(atoi(msg+6)));
            auto_msg_use[i] = 5;
        }
    }
    /* Display messages */
    for (i = 0; i < auto_msg_num; i++)
    {
        /* Get the message */
        msg = auto_msg_buf + auto_msg_pos[i];

        /* Final message */
        borg_note(format("# %s (%d)", msg, auto_msg_use[i]));
    }


    /*** Notice missing monsters ***/
    /* Scan the monster list */
    for (i = 1; i < auto_kills_nxt; i++)
    {
        auto_kill *kill = &auto_kills[i];

        /* Skip dead monsters */
        if (!kill->r_idx) continue;

        /* Skip seen monsters */
        if (kill->when == c_t) continue;

        /* Hack -- blind or hallucinating */
        if (do_blind || do_image) continue;

        /* Predict the monster */
        borg_follow_kill(i);
    }

    /*** Notice missing objects ***/

    /* Scan the object list */
    for (i = 1; i < auto_takes_nxt; i++)
    {
        auto_take *take = &auto_takes[i];

        /* Skip dead objects */
        if (!take->k_idx) continue;

        /* Skip seen objects */
        if (take->when == c_t) continue;

        /* Hack -- blind or hallucinating */
        if (do_blind || do_image) continue;

        /* Follow the object */
        borg_follow_take(i);
    }

#ifdef BORG_ROOMS

    /* Analyze the current map panel */
    for (dy = 0; dy < SCREEN_HGT; dy++)
    {
        for (dx = 0; dx < SCREEN_WID; dx++)
        {
            /* Obtain the map location */
            x = w_x + dx;
            y = w_y + dy;

            /* Get the auto_grid */
            ag = &auto_grids[y][x];

            /* Clear all "broken" rooms */
            if (!borg_cave_floor_grid(ag))
            {
                /* Clear all rooms containing walls */
                if (ag->room)
                {
                    /* Clear all rooms containing walls */
                    if (borg_clear_room(y, x)) goal = 0;
                }
            }

            /* Create "fake" rooms as needed */
            else
            {
                /* Mega-Hack -- super-fake rooms */
                if (!ag->room)
                {
                    /* Acquire a new room */
                    auto_room *ar = borg_free_room();

                    /* Initialize the room */
                    ar->x1 = ar->x2 = x;
                    ar->y1 = ar->y2 = y;

                    /* Save the room */
                    ag->room = ar->self;
                }
            }
        }
    }


    /*** Maintain room info ***/

    /* Make "fake" rooms for all viewable unknown grids */
    for (n = 0; n < auto_view_n; n++)
    {
        /* Access the location */
        y = auto_view_y[n];
        x = auto_view_x[n];

        /* Access the grid */
        ag = &auto_grids[y][x];

        /* Skip walls/doors */
        if (!borg_cave_floor_grid(ag)) continue;

        /* Skip "known" grids */
        if (ag->feat != FEAT_NONE) continue;

        /* Make "fake" rooms as needed */
        if (!ag->room)
        {
            auto_room *ar;

            /* Get a new room */
            ar = borg_free_room();

            /* Initialize the room */
            ar->x1 = ar->x2 = x;
            ar->y1 = ar->y2 = y;

            /* Save the room */
            ag->room = ar->self;
        }
    }

    /* Access the player grid */
    ag = &auto_grids[c_y][c_x];

    /* Paranoia -- require a self room */
    if (!ag->room)
    {
        /* Acquire a new room */
        auto_room *ar = borg_free_room();

        /* Initialize the room */
        ar->x1 = ar->x2 = c_x;
        ar->y1 = ar->y2 = c_y;

        /* Save the room */
        ag->room = ar->self;
    }

    /* Build a "bigger" room around the player */
    if (borg_build_room(c_y, c_x)) goal = 0;

    /* Hack */
    if (TRUE)
    {
        auto_room *ar;

        /* Mark all the "containing rooms" as visited. */
        for (ar = room(1, c_y, c_x); ar; ar = room(0, 0, 0))
        {
            /* Note the visit */
            ar->when = c_t;
        }
    }

#endif


    /*** Various things ***/

    /* Forget goals while "impaired" in any way */
    if (do_blind || do_confused || do_afraid || do_image) goal = 0;

    /* Forget goals while "bleeding" in any way */
    if (do_weak || do_poisoned || do_cut || do_stun || do_heavy_stun) goal = 0;

    /* Forget goals when HP or SP changes */
    if ((auto_chp != old_chp) || (auto_csp != old_csp)) goal = 0;

    /* Save the hit points */
    old_chp = auto_chp;

    /* Save the spell points */
    old_csp = auto_csp;

    /* Forget failure */
    auto_failure = FALSE;

    /* Forget the messages */
    auto_msg_len = 0;
    auto_msg_num = 0;


    /*** Save old info ***/

    /* Save the old "location" */
    o_c_x = c_x;
    o_c_y = c_y;

    /* Save the old "panel" */
    o_w_x = w_x;
    o_w_y = w_y;


    /*** Defaults ***/

    /* Default "goal" location */
    g_x = c_x;
    g_y = c_y;
}


/*
 * Handle various "important" messages
 *
 * Actually, we simply "queue" them for later analysis
 */
void borg_react(cptr msg, cptr buf)
{
    int len;

    /* Note actual message */
    borg_note(format("> %s", msg));

    /* Extract length of parsed message */
    len = strlen(buf);

    /* Verify space */
    if (auto_msg_num + 1 > auto_msg_max)
    {
        borg_oops("too many messages");
        return;
    }

    /* Verify space */
    if (auto_msg_len + len + 1 > auto_msg_siz)
    {
        borg_oops("too much messages");
        return;
    }

    /* Assume not used yet */
    auto_msg_use[auto_msg_num] = 0;

    /* Save the message position */
    auto_msg_pos[auto_msg_num] = auto_msg_len;

    /* Save the message text */
    strcpy(auto_msg_buf + auto_msg_len, buf);

    /* Advance the buf */
    auto_msg_len += len + 1;

    /* Advance the pos */
    auto_msg_num++;
}



/*
 * Sorting hook -- comp function -- see below
 *
 * We use "u" to point to an array of strings, and "v" to point to
 * an array of indexes, and we sort them together by the strings.
 */
static bool ang_sort_comp_hook(vptr u, vptr v, int a, int b)
{
    cptr *text = (cptr*)(u);
    s16b *what = (s16b*)(v);

    int cmp;

    /* Compare the two strings */
    cmp = (strcmp(text[a], text[b]));

    /* Strictly less */
    if (cmp < 0) return (TRUE);

    /* Strictly more */
    if (cmp > 0) return (FALSE);

    /* Enforce "stable" sort */
    return (what[a] <= what[b]);
}


/*
 * Sorting hook -- swap function -- see below
 *
 * We use "u" to point to an array of strings, and "v" to point to
 * an array of indexes, and we sort them together by the strings.
 */
static void ang_sort_swap_hook(vptr u, vptr v, int a, int b)
{
    cptr *text = (cptr*)(u);
    s16b *what = (s16b*)(v);

    cptr texttmp;
    s16b whattmp;

    /* Swap "text" */
    texttmp = text[a];
    text[a] = text[b];
    text[b] = texttmp;

    /* Swap "what" */
    whattmp = what[a];
    what[a] = what[b];
    what[b] = whattmp;
}



/*
 * Initialize this file
 */
void borg_init_5(void)
{
    int i;

    int size;

    s16b what[512];
    cptr text[512];


    /*** Message tracking ***/

    /* No chars saved yet */
    auto_msg_len = 0;

    /* Maximum buffer size */
    auto_msg_siz = 4096;

    /* Allocate a buffer */
    C_MAKE(auto_msg_buf, auto_msg_siz, char);

    /* No msg's saved yet */
    auto_msg_num = 0;

    /* Maximum number of messages */
    auto_msg_max = 128;

    /* Allocate array of positions */
    C_MAKE(auto_msg_pos, auto_msg_max, s16b);

    /* Allocate array of use-types */
    C_MAKE(auto_msg_use, auto_msg_max, s16b);


    /*** Object/Monster tracking ***/

    /* Array of "wanks" */
    C_MAKE(auto_wanks, AUTO_VIEW_MAX, auto_wank);


    /*** Reset the map ***/

    /* Forget the map */
    borg_forget_map();


    /*** Parse "unique" monster names ***/

    /* Start over */
    size = 0;

    /* Collect "unique" monsters */
    for (i = 1; i < MAX_R_IDX-1; i++)
    {
        monster_race *r_ptr = &r_info[i];

        /* Skip non-monsters */
        if (!r_ptr->name) continue;

        /* Skip non-unique monsters */
        if (!(r_ptr->flags1 & RF1_UNIQUE)) continue;

        /* Use it */
        text[size] = r_name + r_ptr->name;
        what[size] = i;
        size++;
    }

    /* Set the sort hooks */
    ang_sort_comp = ang_sort_comp_hook;
    ang_sort_swap = ang_sort_swap_hook;

    /* Sort */
    ang_sort(text, what, size);

    /* Save the size */
    auto_unique_size = size;

    /* Allocate the arrays */
    C_MAKE(auto_unique_text, auto_unique_size, cptr);
    C_MAKE(auto_unique_what, auto_unique_size, s16b);

    /* Save the entries */
    for (i = 0; i < size; i++) auto_unique_text[i] = text[i];
    for (i = 0; i < size; i++) auto_unique_what[i] = what[i];


    /*** Parse "normal" monster names ***/

    /* Start over */
    size = 0;

    /* Collect "normal" monsters */
    for (i = 1; i < MAX_R_IDX-1; i++)
    {
        monster_race *r_ptr = &r_info[i];

        /* Skip non-monsters */
        if (!r_ptr->name) continue;

        /* Skip unique monsters */
        if (r_ptr->flags1 & RF1_UNIQUE) continue;

        /* Use it */
        text[size] = r_name + r_ptr->name;
        what[size] = i;
        size++;
    }

    /* Set the sort hooks */
    ang_sort_comp = ang_sort_comp_hook;
    ang_sort_swap = ang_sort_swap_hook;

    /* Sort */
    ang_sort(text, what, size);

    /* Save the size */
    auto_normal_size = size;

    /* Allocate the arrays */
    C_MAKE(auto_normal_text, auto_normal_size, cptr);
    C_MAKE(auto_normal_what, auto_normal_size, s16b);

    /* Save the entries */
    for (i = 0; i < size; i++) auto_normal_text[i] = text[i];
    for (i = 0; i < size; i++) auto_normal_what[i] = what[i];

   /* Initialize */
   for (i = 0; i < 256; i++) Get_f_info_number[i] = -1;

   for (i = MAX_F_IDX - 1; i >= 0; i--)
   {
       if (i == FEAT_SECRET || i == FEAT_INVIS)
           continue;

       Get_f_info_number[f_info[i].d_char] = i;
   }

}



#else

#ifdef MACINTOSH
static int HACK = 0;
#endif

#endif
