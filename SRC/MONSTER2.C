/* File: monster2.c */

/* Purpose: misc code for monsters */

/* Monster processing, compacting, generation, goody drops, deletion,
 * place the player, monsters, and their escorts at a given location,
 * generation of monsters, summoning, mutliplying monsters, monster
 * reaction to pain levels, monster learning.
 *
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


int get_wilderness_flag(void)
{
        int x = p_ptr->wilderness_x;
        int y = p_ptr->wilderness_y;

        if (dun_level)
                return (RF8_DUNGEON);
        else
                return (1L << wilderness[y][x].terrain);
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

        /* Hack -- count the number of "reproducers" */
        if (r_ptr->flags2 & (RF2_MULTIPLY)) num_repro--;


        /* Hack -- remove target monster */
        if (i == target_who) target_who = 0;

        /* Hack -- remove tracked monster */
        if (i == health_who) health_track(0);


        /* Monster is gone */
        cave[y][x].m_idx = 0;

	/* Total Hack -- If the monster was a player ghost, remove it from 
	 * the monster memory, ensure that it never appears again, clear 
	 * its bones file selector. -LM-
	 */
	if (r_ptr->flags2 & (RF2_PLAYER_GHOST))
	{
		r_ptr->r_sights = 0;
		r_ptr->r_pkills = 1;
		r_ptr->r_tkills = 0;
		bones_selector = 0;
	}

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
        (void)WIPE(m_ptr, monster_type);

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
        (void)WIPE(&m_list[i1], monster_type);
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
        int             i, num, cnt;
        int             cur_lev, cur_dis, chance;


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
		
		/* Total Hack -- Clear player ghost information. -LM- */
		if (r_ptr->flags2 & (RF2_PLAYER_GHOST))
		{
			r_ptr->r_sights = 0;
			r_ptr->r_pkills = 1;
			r_ptr->r_tkills = 0;
			bones_selector = 0;
		}

                /* Mega-Hack -- preserve Unique's XXX XXX XXX */

                /* Hack -- Reduce the racial counter */
                r_ptr->cur_num--;

                /* Monster is gone */
                cave[m_ptr->fy][m_ptr->fx].m_idx = 0;

                /* Wipe the Monster */
                (void)WIPE(m_ptr, monster_type);
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
errr get_mon_num_prep(monster_hook_type monster_hook,
                                          monster_hook_type monster_hook2)
{
        int i;

        /* Todo: Check the hooks for non-changes */

        /* Set the new hooks */
        get_mon_num_hook = monster_hook;
        get_mon_num2_hook = monster_hook2;

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
        int                     i, j, p;

        int                     r_idx;

        long            value, total;

        monster_race    *r_ptr;

        alloc_entry             *table = alloc_race_table;


        /* Boost the level */
        if (level > 0)
        {
                /* Occasional "nasty" monster */
                if (rand_int(NASTY_MON) == 0)
                {
                        /* Pick a level bonus */
                        int d = level / 4 + 2;

                        /* Boost the level */
                        level += ((d < 5) ? d : 5);
                }

                /* Occasional "nasty" monster */
                if (rand_int(NASTY_MON) == 0)
                {
                        /* Pick a level bonus */
                        int d = level / 4 + 2;

                        /* Boost the level */
                        level += ((d < 5) ? d : 5);
                }
        }


        /* Reset total */
        total = 0L;

        /* Process probabilities */
        for (i = 0; i < alloc_race_size; i++)
        {
                /* Monsters are sorted by depth */
                if (table[i].level > level) break;

                /* Default */
                table[i].prob3 = 0;

                /* Access the "r_idx" of the chosen monster */
                r_idx = table[i].index;

                /* Access the actual race */
                r_ptr = &r_info[r_idx];

                /* Don't ever summon unfair monsters. */
                if (!ironman_unfair_monsters && r_ptr->flags7 & RF7_UNFAIR) {
                  continue;
                }

                /* Hack -- "unique" monsters must be "unique" */
                if ((r_ptr->flags1 & (RF1_UNIQUE)) &&
                    (r_ptr->cur_num >= r_ptr->max_num))
                {
                        continue;
                }

                /* Hack -- "unique" monsters must be "unique" */
                if ((r_ptr->flags3 & (RF3_UNIQUE_7)) &&
                    (r_ptr->cur_num >= 7))
                {
                        continue;
                }

                /* Hack -- don't create questors */
                if (r_ptr->flags1 & RF1_QUESTOR)
                {
                        continue;
                }

                /* Depth Monsters never appear out of depth */
                if ((r_ptr->flags1 & (RF1_FORCE_DEPTH)) && (r_ptr->level != dun_level))
                {
                        continue;
                }

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
        cptr            name = (r_name + r_ptr->name);
	char 		racial_name[40] = "oops";
        char            silly_name[80];
        bool            seen, pron;

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
		/* It could be a Player Ghost. -LM- */
		if (r_ptr->flags2 & (RF2_PLAYER_GHOST))
		{
			/* Clear the descriptor. */
			strcpy(desc, "                               ");

			/* Get the ghost name. */
			strcpy(desc, ghost_name);

			/* Get the racial name. */
			strcpy(racial_name, r_name + r_ptr->name);

			/* Build the ghost name. */
			strcat(desc, ", the ");
			strcat(desc, racial_name);
		}
		
                /* It could be a Unique */
                else if ((r_ptr->flags1 & (RF1_UNIQUE)) && !(p_ptr->image))
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
                                (void)strcpy(desc, "your ");
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



void sanity_blast(monster_type * m_ptr, bool necro)
{
        bool happened = FALSE;
        int power = 100;

        if ((!necro) && (m_ptr))
        {
                char            m_name[80];
                monster_race    *r_ptr = &r_info[m_ptr->r_idx];
                monster_race    *r2_ptr = &r_info[p_ptr->polymon];
                
                power = (r_ptr->level)+10;

                monster_desc(m_name, m_ptr, 0);

                if (!(r_ptr->flags1 & RF1_UNIQUE))
                {
                        if (r_ptr->flags1 & RF1_FRIENDS)
                        power /= 2;
                }
                else power *= 2;

                if (r2_ptr->flags2 & RF2_ELDRITCH_HORROR)
                        return; /* Immune to those effects... */

                if (!hack_mind)
                        return; /* No effect yet, just loaded... */

                if (!(m_ptr->ml))
                        return; /* Cannot see it for some reason */

                if (!(r_ptr->flags2 & RF2_ELDRITCH_HORROR))
                        return; /* oops */

                if (is_pet(m_ptr) && (randint(8)!=1))
                        return; /* Pet eldritch horrors are safe most of the time */

                if (randint(power)<p_ptr->skill_sav)
                {
                        return; /* Save, no adverse effects */
                }


                if (p_ptr->image)
                {
                /* Something silly happens... */
                        msg_format("You behold the %s visage of %s!",
                                funny_desc[(rand_int(MAX_FUNNY))], m_name);
                        if (randint(3)==1)
                        {
                                msg_print(funny_comments[rand_int(MAX_COMMENT)]);
                                p_ptr->image = (p_ptr->image + randint(r_ptr->level));
                        }
                        return; /* Never mind; we can't see it clearly enough */
                }

                /* Something frightening happens... */
                msg_format("You behold the %s visage of %s!",
                        horror_desc[(rand_int(MAX_HORROR))], m_name);

                r_ptr->r_flags2 |= RF2_ELDRITCH_HORROR;

                /* Demon characters are unaffected */
                if (p_ptr->prace == RACE_IMP) return;

		/* Cancri are unaffected */
                if (p_ptr->sign == SIGN_CANCER) return;
		
                /* Undead characters are 50% likely to be unaffected */
                if ((p_ptr->prace == RACE_SKELETON) || (p_ptr->prace == RACE_ZOMBIE)
                        || (p_ptr->prace == RACE_VAMPIRE) || (p_ptr->prace == RACE_SPECTRE))
                {
                        if (randint(100) < (25 + (p_ptr->lev))) return;
                }
        }
        else
        {
                if (cave[py][px].t_idx == TRAP_OF_ELDRITCH_HORROR)
                msg_print("You behold the horrible visages of Outer Gods!");
                else
                msg_print("Your sanity is shaken by reading the Necronomicon!");
        }
        if (randint(power)<p_ptr->skill_sav) /* Mind blast */
        {
                if (!(p_resist_conf || p_ptr->tim_res_conf))
                {
                        (void)set_confused(p_ptr->confused + rand_int(4) + 4);
                }
                if ((!p_resist_chaos) && (randint(3)==1))
                {
                        (void) set_image(p_ptr->image + rand_int(250) + 150);
                }
                return;
        }

        if (randint(power)<p_ptr->skill_sav) /* Lose int & wis */
        {
		do_dec_stat (A_INT, STAT_DEC_NORMAL);
		do_dec_stat (A_WIS, STAT_DEC_NORMAL);
                return;
        }


        if (randint(power)<p_ptr->skill_sav) /* Brain smash */
        {
                if (!(p_resist_conf || p_ptr->tim_res_conf))
                {
                        (void)set_confused(p_ptr->confused + rand_int(4) + 4);
                }
                if (!p_free_act)
                {
                        (void)set_paralyzed(p_ptr->paralyzed + rand_int(4) + 4);
                }
                while (rand_int(100) > p_ptr->skill_sav)
			(void)do_dec_stat(A_INT, STAT_DEC_NORMAL);	
		while (rand_int(100) > p_ptr->skill_sav)
			(void)do_dec_stat(A_WIS, STAT_DEC_NORMAL);
                if (!p_resist_chaos)
                {
                        (void) set_image(p_ptr->image + rand_int(250) + 150);
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

        /* Else gain permanent insanity */
        if ((p_ptr->muta3 & MUT3_MORONIC) && (p_ptr->muta2 & MUT2_BERS_RAGE) &&
           ((p_ptr->muta2 & MUT2_COWARDICE) || (p_resist_fear)) &&
           ((p_ptr->muta2 & MUT2_HALLU) || (p_resist_chaos)))
        {
                /* The poor bastard already has all possible insanities! */
                return;
        }

        while (!happened)
        {
                switch(randint(4))
                {
                        case 1:
                        if (!(p_ptr->muta3 & MUT3_MORONIC))
                        {
                                msg_print("You turn into an utter moron!");
                                if (p_ptr->muta3 & MUT3_HYPER_INT)
                                {
                                        msg_print("Your brain is no longer a living computer.");
                                        p_ptr->muta3 &= ~(MUT3_HYPER_INT);
                                }
                                p_ptr->muta3 |= MUT3_MORONIC;
                                happened = TRUE;
                        }
                        break;
                        case 2:
                        if (!(p_ptr->muta2 & MUT2_COWARDICE) && !(p_resist_fear))
                        {
                                msg_print("You become paranoid!");

                                /* Duh, the following should never happen, but anyway... */
                                if (p_ptr->muta3 & MUT3_FEARLESS)
                                {
                                        msg_print("You are no longer fearless.");
                                        p_ptr->muta3 &= ~(MUT3_FEARLESS);
                                }

                                p_ptr->muta2 |= MUT2_COWARDICE;
                                happened = TRUE;
                        }
                        break;
                        case 3:
                        if (!(p_ptr->muta2 & MUT2_HALLU) && !(p_resist_chaos))
                        {
                                msg_print("You are afflicted by a hallucinatory insanity!");
                                p_ptr->muta2 |= MUT2_HALLU;
                                happened = TRUE;
                        }
                        break;
                        default:
                        if (!(p_ptr->muta2 & MUT2_BERS_RAGE))
                        {
                                msg_print("You become subject to fits of berserk rage!");
                                p_ptr->muta2 |= MUT2_BERS_RAGE;
                                happened = TRUE;
                        }
                        break;
                }
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
                                if (!do_invisible || p_see_invis) easy = flag = TRUE;
                        }
                }

                /* Telepathy can see all "nearby" monsters with "minds" */
                if (p_telepathy)
                {
                        /* Empty mind, no telepathy */
                        if (r_ptr->flags2 & (RF2_EMPTY_MIND))
                        {
                                do_empty_mind = TRUE;
                        }

                        /* Weird mind, occasional telepathy. But always
                           on with timed and pernament ESP -GSN- */

                        else if (r_ptr->flags2 & (RF2_WEIRD_MIND))
                        {
                                if (!p_ptr->tim_esp)
                                {
                                do_weird_mind = TRUE;
                                if (rand_int(100) < 10) hard = flag = TRUE;
                                }
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
            {   if (disturb_pets || is_hostile(m_ptr))
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

/* This function checks whether we are aligned and can convince a monster
 * of given race to be friendly. It returns TRUE if all of our checks
 * succeeded. Note that we can only 'charm' about 50% monsters of our
 * level and alignment -GSN-
 */
 
static bool alignment_match(int r_idx)
{
        monster_race    *r_ptr = &r_info[r_idx];

        /* No friendlies on quest levels */
	if (p_ptr->inside_quest) return (FALSE);
	if (is_quest(dun_level)) return (FALSE);

        /* No friendly uniques, boss monsters, nonliving, stupid */
	if (r_ptr->flags1 & RF1_UNIQUE) return (FALSE);
	if (r_ptr->flags1 & RF1_QUESTOR) return (FALSE);
	if (r_ptr->flags2 & RF2_STUPID) return (FALSE);
	if (r_ptr->flags3 & RF3_NONLIVING) return (FALSE);

        /* Can we prevail over the monster? */
	if (p_ptr->lev * 2 < randint(r_ptr->level))
	   return (FALSE);

        /* We need enough charisma for this to work */		
	if (randint(20) > adj_con_fix[p_ptr->stat_cur[A_CHR]] + 1) 
	   return (FALSE); 

	/* We need be of same alignment. Note special neutral/nature proc. */
	
        if ((p_ptr->align & ALIGN_GOOD) && (r_ptr->flags3 & RF3_GOOD))
	   return (TRUE);
        if ((p_ptr->align & ALIGN_LAWFUL) && (r_ptr->flags2 & RF2_LAWFUL))
	   return (TRUE);
        else if ((p_ptr->align & ALIGN_EVIL) && (r_ptr->flags3 & RF3_EVIL))
	   return (TRUE);
        else if ((p_ptr->align & ALIGN_CHAOTIC) && (r_ptr->flags2 & RF2_CHAOTIC))
	   return (TRUE);
        else if ((p_ptr->align & ALIGN_NEUT) && !((r_ptr->flags3 & RF3_GOOD) ||
	    (r_ptr->flags2 & RF2_LAWFUL) || (r_ptr->flags3 & RF3_EVIL) ||
            (r_ptr->flags2 & RF2_CHAOTIC)))
	   return (TRUE);
	
	else return (FALSE);   /* We're incompatible */
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
bool place_monster_one(int y, int x, int r_idx, bool slp, bool friendly, bool pet)
{
        int             i;
        cave_type       *c_ptr;
        monster_type    *m_ptr;
        monster_race    *r_ptr = &r_info[r_idx];
        cptr            name = (r_name + r_ptr->name);


        /* Verify location */
        if (!in_bounds(y, x)) return (FALSE);

        /* Require empty space */
        if (!cave_empty_bold(y, x)) return (FALSE);

        /* Hack -- no creation on glyph of warding */
        if (cave[y][x].feat == FEAT_GLYPH) return (FALSE);
        if (cave[y][x].feat == FEAT_MINOR_GLYPH) return (FALSE);

        /* Nor on the Pattern */
        if ((cave[y][x].feat >= FEAT_PATTERN_START)
         && (cave[y][x].feat <= FEAT_PATTERN_XTRA2))
                return (FALSE);

        /* Nor on the altars */
        if ((cave[y][x].feat >= FEAT_ALTAR_HEAD)
         && (cave[y][x].feat <= FEAT_ALTAR_TAIL))
                return (FALSE);

        /* Paranoia */
        if (!r_idx) return (FALSE);

        /* Paranoia */
        if (!r_ptr->name) return (FALSE);

        if (!monster_can_cross_terrain(cave[y][x].feat, r_ptr))
        {
                return FALSE;
        }

        /* Hack -- "unique" monsters must be "unique" */
        if ((r_ptr->flags1 & (RF1_UNIQUE)) && (r_ptr->cur_num >= r_ptr->max_num))
        {
                /* Cannot create */
                return (FALSE);
        }
        if ((r_ptr->flags3 & (RF3_UNIQUE_7)) && (r_ptr->cur_num >= 7))
        {
                /* Cannot create */
                return (FALSE);
        }

        if (r_ptr->flags2 & (RF2_EXTINCT))
        {
                /* Cannot create */
                return (FALSE);
        }

        /* Depth monsters may NOT be created out of depth */
        if ((r_ptr->flags1 & (RF1_FORCE_DEPTH)) && (dun_level != r_ptr->level))
        {
                /* Cannot create */
                return (FALSE);
        }


        /* Powerful monster */
        if (r_ptr->level > dun_level)
        {
		/* Player ghosts */
		if (r_ptr->flags2 & (RF2_PLAYER_GHOST))
		{
			/* Message for cheaters */
			if (cheat_hear) msg_print("Deep Unique (Player Ghost).");

			/* Boost rating by twice delta-depth */
			rating += (r_ptr->level - dun_level) * 2;
		}
		
                /* Unique monsters */
                if (r_ptr->flags1 & (RF1_UNIQUE))
                {
                        /* Message for cheaters and precognitors */
                        if (cheat_hear || p_ptr->precog)
                        msg_format("Deep Unique (%s).", name);

                        /* Boost rating by twice delta-depth */
                        rating += (r_ptr->level - dun_level) * 2;
                }

                /* Normal monsters */
                else
                {
                        /* Message for cheaters and precognitors */
                        if (cheat_hear || p_ptr->precog)
                        msg_format("Deep Monster (%s).", name);

                        /* Boost rating by delta-depth */
                        rating += (r_ptr->level - dun_level);
                }
        }

        /* Note the monster */
        else if (r_ptr->flags1 & (RF1_UNIQUE))
        {
                /* Unique monsters induce message */
                if (cheat_hear || p_ptr->precog)
                msg_format("Unique (%s).", name);
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

        /* Unknown distance */
        m_ptr->cdis = 0;

        /* No flags */
        m_ptr->mflag = 0;

        /* Not visible */
        m_ptr->ml = FALSE;

        /* Pet? */
        if (pet || (r_ptr->flags7 & RF7_PET))
        {
                set_pet(m_ptr);
        }

        /* Friendly? */
        else if (friendly || (r_ptr->flags7 & RF7_FRIENDLY) || 
	         alignment_match(r_idx))
        {
                set_friendly(m_ptr);
        }

	/* If the monster is a player ghost, perform various manipulations on it, 
	 * and forbid ghost creation if something goes wrong. -LM-
	 */
	if (r_ptr->flags2 & (RF2_PLAYER_GHOST))
	{
		if (!prepare_ghost(m_ptr->r_idx, m_ptr, FALSE)) return (FALSE);
                if ((m_ptr->maxhp == 0) || (m_ptr->hp == 0))
                        { m_ptr->hp = 1; m_ptr->maxhp = 1; }
	}

        /* Assume no sleeping */
        m_ptr->csleep = 0;


        /* Enforce sleeping if needed */
        if (slp && r_ptr->sleep)
        {
                int val = r_ptr->sleep;
                m_ptr->csleep = ((val * 2) + randint(val * 10));
        }

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


        /* Extract the monster base speed */
        m_ptr->mspeed = r_ptr->speed;

        /* Hack -- small racial variety */
        if (!(r_ptr->flags1 & (RF1_UNIQUE)))
        {
                /* Allow some small variation per monster */
                i = extract_energy[r_ptr->speed] / 10;
                if (i) m_ptr->mspeed += rand_spread(0, i);
        }

        /* Give a random starting energy */
        m_ptr->energy = (byte)rand_int(100);

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


        /* Hack -- Count the number of "reproducers" */
        if (r_ptr->flags2 & (RF2_MULTIPLY)) num_repro++;

        /* Hack -- Notice new multi-hued monsters */
        if (r_ptr->flags1 & (RF1_ATTR_MULTI)) shimmer_monsters = TRUE;

        /* Success */
        return (TRUE);
}

#define GROUP_MAX 32

/*
 * Attempt to place a "group" of monsters around the given location
 */
static bool place_monster_group(int y, int x, int r_idx, bool slp, bool friendly, bool pet)
{
        monster_race *r_ptr = &r_info[r_idx];

        int old, n, i;
        int total = 0, extra = 0;

        int hack_n = 0;

        byte hack_y[GROUP_MAX];
        byte hack_x[GROUP_MAX];


        /* Pick a group size */
        total = damroll(r_ptr->noapp[0], r_ptr->noapp[1]);

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

#if 0
        /* Hack -- limit group reduction */
        if (extra > 12) extra = 12;
#endif

        /* Modify the group size */
        total += extra;

        /* Minimum size */
        if (total < r_ptr->noapp[0]) total = r_ptr->noapp[0];

        /* Maximum size */
        if (total > maxroll(r_ptr->noapp[0], r_ptr->noapp[1]))
                total = maxroll(r_ptr->noapp[0], r_ptr->noapp[1]);

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
			int mx, my;

			scatter(&my, &mx, hy, hx, 4, 0);

                        /* Walls and Monsters block flow */
                        if (!cave_empty_bold(my, mx)) continue;

                        /* Attempt to place another monster */
                        if (place_monster_one(my, mx, r_idx, slp, friendly, pet))
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
bool place_monster_aux(int y, int x, int r_idx, bool slp, bool grp, bool friendly, bool pet)
{
        int             i;
        monster_race    *r_ptr = &r_info[r_idx];


        /* Place one monster, or fail */
        if (!place_monster_one(y, x, r_idx, slp, friendly, pet)) return (FALSE);


        /* Require the "group" flag */
        if (!grp) return (TRUE);


        /* Friends for certain monsters */
        if (r_ptr->flags1 & (RF1_FRIENDS))
        {
                /* Attempt to place a group */
                (void)place_monster_group(y, x, r_idx, slp, friendly, pet);
        }


        /* Escorts for certain monsters */
        if (r_ptr->flags1 & (RF1_ESCORT))
        {
                /* Set the escort index */
                place_monster_idx = r_idx;

                /* Try to place several "escorts" */
                for (i = 0; i < 50; i++)
                {
                        int nx, ny, z, d = 3;

                        /* Pick a location */
                        scatter(&ny, &nx, y, x, d, 0);

                        /* Require empty grids */
                        if (!cave_empty_bold(ny, nx)) continue;

                        /* Prepare allocation table */
                        get_mon_num_prep(place_monster_okay, get_monster_hook2(ny, nx));

                        /* Pick a random race */
                        z = get_mon_num(r_ptr->level);

                        /* Handle failure */
                        if (!z) break;

                        /* Place a single escort */
                        (void)place_monster_one(ny, nx, z, slp, friendly, pet);

                        /* Place a "group" of escorts if needed */
                        if ((r_info[z].flags1 & (RF1_FRIENDS)) ||
                            (r_ptr->flags1 & (RF1_ESCORTS)))
                        {
                                /* Place a group of monsters */
                                (void)place_monster_group(ny, nx, z, slp, friendly, pet);
                        }
                }
        }

        /* Success */
        return (TRUE);
}


/*
 * Hack -- attempt to place a monster at the given location
 *
 * Attempt to find a monster appropriate to the "monster_level"
 */
bool place_monster(int y, int x, bool slp, bool grp)
{
        int r_idx;

        /* Prepare allocation table */
        get_mon_num_prep(get_monster_hook(), get_monster_hook2(y, x));

        /* Pick a monster */
        r_idx = get_mon_num(monster_level);

        /* Handle failure */
        if (!r_idx) return (FALSE);

        /* Attempt to place the monster */
        if (place_monster_aux(y, x, r_idx, slp, grp, FALSE, FALSE)) return (TRUE);

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
        int r_idx;
        monster_race * r_ptr;
        monster_type * m_ptr;
        int attempts = 1000;

        /* Prepare allocation table */
        get_mon_num_prep(get_monster_hook(), get_monster_hook2(y, x));

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

        if (attempts < 1) return FALSE;

        attempts = 1000;

        while (--attempts)
        {
                /* Attempt to place the monster */
                if (place_monster_aux(y, x, r_idx, FALSE, FALSE, FALSE, FALSE)) break;
        }

        if (attempts < 1) return FALSE;


        m_ptr = &m_list[hack_m_idx_ii];

        summon_kin_type = r_ptr->d_char;

        for (attempts = randint(10) + 5; attempts; attempts--)
        {
                (void) summon_specific(m_ptr->fy, m_ptr->fx, dun_level, SUMMON_KIN, TRUE, FALSE, FALSE);
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
        int                     y, x;
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
                        if (cheat_hear) msg_print("Monster horde.");
                        return (TRUE);
                }
        }
        else
        {
#endif /* MONSTER_HORDES */

                /* Attempt to place the monster, allow groups */
                if (place_monster(y, x, slp, TRUE)) return (TRUE);

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
 * Hack -- help decide if a monster race is "okay" to summon
 */
static bool summon_specific_okay(int r_idx)
{
        monster_race *r_ptr = &r_info[r_idx];

        bool okay = FALSE;

        /* Hack - Only summon dungeon monsters */
        if (!monster_dungeon(r_idx)) return (FALSE);

        /* Hack -- no specific type specified */
        if (!summon_specific_type) return (TRUE);

        /* Check our requirements */
        switch (summon_specific_type)
        {
                case SUMMON_ANT:
                {
                        okay = ((r_ptr->d_char == 'a') &&
                                !(r_ptr->flags1 & (RF1_UNIQUE)));
                        break;
                }

                case SUMMON_SPIDER:
                {
                        okay = ((r_ptr->d_char == 'S') &&
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

                case SUMMON_ANGEL:
                {
                        okay = ((r_ptr->d_char == 'A') &&
                                !(r_ptr->flags1 & (RF1_UNIQUE)));
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

                case SUMMON_HI_UNDEAD:
                {
                        okay = ((r_ptr->d_char == 'L') ||
                                (r_ptr->d_char == 'V') ||
                                (r_ptr->d_char == 'W'));
                        break;
                }

                case SUMMON_HI_DRAGON:
                {
                        okay = (r_ptr->d_char == 'D');
                        break;
                }

                case SUMMON_WRAITH:
                {
                        okay = (r_ptr->d_char == 'W');
                        break;
                }

		case SUMMON_QUYLTHULG:
		{
			okay = ((r_ptr->d_char == 'Q') &&
				!(r_ptr->flags1 & (RF1_UNIQUE)));
			break;
		}

                case SUMMON_THIEF:
                {
                        /* Scan through all four blows */
                        int i, effect;

                        for (i = 0; i < 4; i++)
                        {
                                /* Extract infomation about the blow effect */
                                effect = r_ptr->blow[i].effect;
                                if (effect == RBE_EAT_GOLD) okay = TRUE;
                                if (effect == RBE_EAT_ITEM) okay = TRUE;
                        }
                        break;
                }

                case SUMMON_UNIQUE:
                {
                        okay = (r_ptr->flags1 & (RF1_UNIQUE)) ? TRUE : FALSE;
                        break;
                }

                case SUMMON_BIZARRE1:
                {
                        okay = ((r_ptr->d_char == 'm') &&
                               !(r_ptr->flags1 & (RF1_UNIQUE)));
                        break;
                }
                case SUMMON_BIZARRE2:
                {
                        okay = ((r_ptr->d_char == 'b') &&
                               !(r_ptr->flags1 & (RF1_UNIQUE)));
                        break;
                }
                case SUMMON_BIZARRE3:
                {
                        okay = ((r_ptr->d_char == 'Q') &&
                               !(r_ptr->flags1 & (RF1_UNIQUE)));
                        break;
                }

                case SUMMON_BIZARRE4:
                {
                        okay = ((r_ptr->d_char == 'v') &&
                               !(r_ptr->flags1 & (RF1_UNIQUE)));
                        break;
                }

                case SUMMON_BIZARRE5:
                {
                        okay = ((r_ptr->d_char == '$') &&
                               !(r_ptr->flags1 & (RF1_UNIQUE)));
                        break;
                }

                case SUMMON_BIZARRE6:
                {
                        okay = (((r_ptr->d_char == '!') ||
                                 (r_ptr->d_char == '?') ||
                                 (r_ptr->d_char == '=') ||
                                 (r_ptr->d_char == '$') ||
                                 (r_ptr->d_char == '|')) &&
                                !(r_ptr->flags1 & (RF1_UNIQUE)));
                        break;
                }

                case SUMMON_CYBER:
                {
                        okay = ((r_ptr->d_char == 'U') &&
                                (r_ptr->flags4 & RF4_ROCKET) &&
                               !(r_ptr->flags1 & RF1_UNIQUE));
                        break;
                }


                case SUMMON_KIN:
                {
                        okay = ((r_ptr->d_char == summon_kin_type) &&
                               !(r_ptr->flags1 & (RF1_UNIQUE)));
                        break;
                }

                case SUMMON_DAWN:
                {
                        okay = ((strstr((r_name + r_ptr->name),"the Dawn")) &&
                               !(r_ptr->flags1 & (RF1_UNIQUE)));
                        break;
                }

                case SUMMON_ANIMAL:
                {
                        okay = ((r_ptr->flags3 & (RF3_ANIMAL)) &&
                               !(r_ptr->flags1 & (RF1_UNIQUE)));
                        break;
                }

                case SUMMON_ANIMAL_RANGER:
                {
                        okay = ((r_ptr->flags3 & (RF3_ANIMAL)) &&
                               (strchr("abcflqrwBCIJKMRS", r_ptr->d_char)) &&
                               !(r_ptr->flags3 & (RF3_DRAGON))&&
                               !(r_ptr->flags3 & (RF3_EVIL)) &&
                               !(r_ptr->flags3 & (RF3_UNDEAD))&&
                               !(r_ptr->flags3 & (RF3_DEMON)) &&
                               !(r_ptr->flags4 || r_ptr->flags5 || r_ptr->flags6) &&
                               !(r_ptr->flags1 & (RF1_UNIQUE)));
                        break;
                }

                case SUMMON_HI_UNDEAD_NO_UNIQUES:
                {
                        okay = (((r_ptr->d_char == 'L') ||
                                 (r_ptr->d_char == 'V') ||
                                 (r_ptr->d_char == 'W')) &&
                                !(r_ptr->flags1 & (RF1_UNIQUE)));
                        break;
                }

                case SUMMON_HI_DRAGON_NO_UNIQUES:
                {
                        okay = ((r_ptr->d_char == 'D') &&
                               !(r_ptr->flags1 &(RF1_UNIQUE)));
                        break;
                }

                case SUMMON_NO_UNIQUES:
                {
                        okay = (!(r_ptr->flags1 & (RF1_UNIQUE)));
                        break;
                }

                case SUMMON_PHANTOM:
                {
                        okay = ((strstr((r_name + r_ptr->name),"Phantom")) &&
                               !(r_ptr->flags1 & (RF1_UNIQUE)));
                        break;
                }

                case SUMMON_AQUATIC:
                {
                        okay = (r_ptr->flags7 & (RF7_AQUATIC)) ? TRUE : FALSE;
                        break;
                }

                case SUMMON_SPIRIT:
                {
                 if ((p_ptr->realm1 == 2) || (p_ptr->realm2 == 2))
                        okay = ((strstr((r_name + r_ptr->name),"ir spirit")) &&
                               !(r_ptr->flags1 & (RF1_UNIQUE)));
                 else if ((p_ptr->realm1 == 3) || (p_ptr->realm2 == 3))
                        okay = ((strstr((r_name + r_ptr->name),"arth spirit")) &&
                               !(r_ptr->flags1 & (RF1_UNIQUE)));
                 else if ((p_ptr->realm1 == 4) || (p_ptr->realm2 == 4))
                        okay = ((strstr((r_name + r_ptr->name),"ire spirit")) &&
                               !(r_ptr->flags1 & (RF1_UNIQUE)));
                 else if ((p_ptr->realm1 == 6) || (p_ptr->realm2 == 6))
                        okay = ((strstr((r_name + r_ptr->name),"ater spirit")) &&
                               !(r_ptr->flags1 & (RF1_UNIQUE)));
                        break;
                }

                case SUMMON_ELEMENTAL:
                {
                 if ((p_ptr->realm1 == 2) || (p_ptr->realm2 == 2))
                        okay = ((strstr((r_name + r_ptr->name),"ir elemental")) &&
                               !(r_ptr->flags1 & (RF1_UNIQUE)));
                 else if ((p_ptr->realm1 == 3) || (p_ptr->realm2 == 3))
                        okay = ((strstr((r_name + r_ptr->name),"arth elemental")) &&
                               !(r_ptr->flags1 & (RF1_UNIQUE)));
                 else if ((p_ptr->realm1 == 4) || (p_ptr->realm2 == 4))
                        okay = ((strstr((r_name + r_ptr->name),"ire elemental")) &&
                               !(r_ptr->flags1 & (RF1_UNIQUE)));
                 else if ((p_ptr->realm1 == 6) || (p_ptr->realm2 == 6))
                        okay = ((strstr((r_name + r_ptr->name),"ater elemental")) &&
                               !(r_ptr->flags1 & (RF1_UNIQUE)));
                        break;
                }

                case SUMMON_ELEMENTAL2:
                {
                 if ((p_ptr->realm1 == 2) || (p_ptr->realm2 == 2))
                        okay = ((strstr((r_name + r_ptr->name),"moke elemental")) &&
                               !(r_ptr->flags1 & (RF1_UNIQUE)));
                 else if ((p_ptr->realm1 == 3) || (p_ptr->realm2 == 3))
                        okay = ((strstr((r_name + r_ptr->name),"oze elemental")) &&
                               !(r_ptr->flags1 & (RF1_UNIQUE)));
                 else if ((p_ptr->realm1 == 4) || (p_ptr->realm2 == 4))
                        okay = ((strstr((r_name + r_ptr->name),"agma elemental")) &&
                               !(r_ptr->flags1 & (RF1_UNIQUE)));
                 else if ((p_ptr->realm1 == 6) || (p_ptr->realm2 == 6))
                        okay = ((strstr((r_name + r_ptr->name),"ce elemental")) &&
                               !(r_ptr->flags1 & (RF1_UNIQUE)));
                        break;
                }

		case SUMMON_SKULL:
		{
			okay = ((r_ptr->flags3 & (RF2_DOOM)) &&
					  strstr((r_name + r_ptr->name), "kull"));
			break;
		}

		case SUMMON_DOOM:
		{
			okay = (r_ptr->flags3 & (RF2_DOOM)) &&
					 !(r_ptr->flags1 & (RF1_FORCE_MAXHP)) &&
					 !(r_ptr->flags1 & (RF1_UNIQUE));
			break;
		}

		case SUMMON_HI_DOOM:
		{
			okay = !!(r_ptr->flags3 & (RF2_DOOM));
			break;
		}
        }

        /* Result */
        return (okay);
}


/*
 * Place a monster (of the specified "type") near the given
 * location.  Return TRUE if a monster was actually summoned.
 *
 * We will attempt to place the monster up to 10 times before giving up.
 *
 * Note: SUMMON_UNIQUE will summon Uniques
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
bool summon_specific(int y1, int x1, int lev, int type, bool group, bool friendly, bool pet)
{
        int i, x, y, r_idx;

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

        /* Prepare allocation table */
        get_mon_num_prep(summon_specific_okay, get_monster_hook2(y, x));

        /* Pick a monster, using the level calculation */
        r_idx = get_mon_num((dun_level + lev) / 2 + 5);

        /* Handle failure */
        if (!r_idx) return (FALSE);

        /* Attempt to place the monster (awake, allow groups) */
        if (!place_monster_aux(y, x, r_idx, FALSE, group, friendly, pet)) return (FALSE);

        /* Success */
        return (TRUE);
}



/*
 * Let the given monster attempt to reproduce.
 *
 * Note that "reproduction" REQUIRES empty space.
 */
bool multiply_monster(int m_idx, bool clone, bool friendly, bool pet)
{
        monster_type    *m_ptr = &m_list[m_idx];

        int                     i, y, x;

        bool result = FALSE;

        /* Try up to 18 times */
        for (i = 0; i < 18; i++)
        {
                int d = 1;

                /* Pick a location */
                scatter(&y, &x, m_ptr->fy, m_ptr->fx, d, 0);

                /* Require an "empty" floor grid */
                if (!cave_empty_bold(y, x)) continue;


                /* Create a new monster (awake, no groups) */
                result = place_monster_aux(y, x, m_ptr->r_idx, FALSE, FALSE, friendly, pet);

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
void message_pain(int m_idx, int dam)
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


        /* Jellies, Molds, Vortices, Quthl-s and nonliving */
        if (strchr("jmvQ|?=/($#", r_ptr->d_char))
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
	
	/* Snakes, Hydrae, Reptiles */
	else if (strchr("JMR", r_ptr->d_char))
	{
		if (percentage > 95)
			msg_format("%^s barely notices.", m_name);
		else if (percentage > 75)
			msg_format("%^s hisses.", m_name);
		else if (percentage > 50)
			msg_format("%^s rears up in anger.", m_name);
		else if (percentage > 35)
			msg_format("%^s hisses furiously.", m_name);
		else if (percentage > 20)
			msg_format("%^s writhes about.", m_name);
		else if (percentage > 10)
			msg_format("%^s writhes in agony.", m_name);
		else
			msg_format("%^s jerks limply.", m_name);
	}


	/* Ants, Centipedes, Flies, Insects, Beetles, Spiders */
	else if (strchr("acFIKS", r_ptr->d_char))
	{
		if (percentage > 95)
			msg_format("%^s ignores the attack.", m_name);
		else if (percentage > 75)
			msg_format("%^s chitters.", m_name);
		else if (percentage > 50)
			msg_format("%^s scuttles about.", m_name);
		else if (percentage > 35)
			msg_format("%^s twitters.", m_name);
		else if (percentage > 20)
			msg_format("%^s jerks in pain.", m_name);
		else if (percentage > 10)
			msg_format("%^s jerks in agony.", m_name);
		else
			msg_format("%^s twitches.", m_name);
	}


	/* Dragons, Demons, High Undead */
	else if (strchr("duDLUW", r_ptr->d_char))
	{
		if (percentage > 95)
			msg_format("%^s ignores the attack.", m_name);
		else if (percentage > 75)
			msg_format("%^s flinches.", m_name);
		else if (percentage > 50)
			msg_format("%^s hisses in pain.", m_name);
		else if (percentage > 35)
			msg_format("%^s snarls with pain.", m_name);
		else if (percentage > 20)
			msg_format("%^s roars with pain.", m_name);
		else if (percentage > 10)
			msg_format("%^s gasps.", m_name);
		else
			msg_format("%^s snarls feebly.", m_name);
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
        else if (strchr("Xblqrsx", r_ptr->d_char))
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
        switch (what)
        {
                case DRS_ACID:
                if (p_resist_acid) m_ptr->smart |= (SM_RES_ACID);
                if (p_immune_acid) m_ptr->smart |= (SM_IMM_ACID);
                if (p_ptr->oppose_acid) m_ptr->smart |= (SM_OPP_ACID);
                break;

                case DRS_ELEC:
                if (p_resist_elec) m_ptr->smart |= (SM_RES_ELEC);
                if (p_immune_elec) m_ptr->smart |= (SM_IMM_ELEC);
                if (p_ptr->oppose_elec) m_ptr->smart |= (SM_OPP_ELEC);
                break;

                case DRS_FIRE:
                if (p_resist_fire) m_ptr->smart |= (SM_RES_FIRE);
                if (p_immune_fire) m_ptr->smart |= (SM_IMM_FIRE);
                if (p_ptr->oppose_fire) m_ptr->smart |= (SM_OPP_FIRE);
                break;

                case DRS_COLD:
                if (p_resist_cold) m_ptr->smart |= (SM_RES_COLD);
                if (p_immune_cold) m_ptr->smart |= (SM_IMM_COLD);
                if (p_ptr->oppose_cold) m_ptr->smart |= (SM_OPP_COLD);
                break;

                case DRS_POIS:
                if (p_resist_pois) m_ptr->smart |= (SM_RES_POIS);
                if (p_ptr->oppose_pois) m_ptr->smart |= (SM_OPP_POIS);
                break;


                case DRS_NETH:
                if (p_resist_neth) m_ptr->smart |= (SM_RES_NETH);
                break;

                case DRS_LITE:
                if (p_resist_lite) m_ptr->smart |= (SM_RES_LITE);
                break;

                case DRS_DARK:
                if (p_resist_dark) m_ptr->smart |= (SM_RES_DARK);
                break;

                case DRS_FEAR:
                if (p_resist_fear) m_ptr->smart |= (SM_RES_FEAR);
                break;

                case DRS_CONF:
                if (p_resist_conf) m_ptr->smart |= (SM_RES_CONF);
                if (p_ptr->tim_res_conf) m_ptr->smart |= (SM_RES_CONF);
                break;

                case DRS_CHAOS:
                if (p_resist_chaos) m_ptr->smart |= (SM_RES_CHAOS);
                break;

                case DRS_DISEN:
                if (p_resist_disen) m_ptr->smart |= (SM_RES_DISEN);
                break;

                case DRS_BLIND:
                if (p_resist_blind) m_ptr->smart |= (SM_RES_BLIND);
                if (p_ptr->tim_res_blind) m_ptr->smart |= (SM_RES_BLIND);
                break;

                case DRS_NEXUS:
                if (p_resist_nexus) m_ptr->smart |= (SM_RES_NEXUS);
                break;

                case DRS_SOUND:
                if (p_resist_sound) m_ptr->smart |= (SM_RES_SOUND);
                break;

                case DRS_SHARD:
                if (p_resist_shard) m_ptr->smart |= (SM_RES_SHARD);
                break;

                case DRS_FREE:
                if (p_free_act) m_ptr->smart |= (SM_IMM_FREE);
                break;

                case DRS_MANA:
                if (!p_ptr->msp) m_ptr->smart |= (SM_IMM_MANA);
                break;

        case DRS_REFLECT:
        if (p_reflect) m_ptr-> smart |= (SM_IMM_REFLECT);
        }

#endif /* DRS_SMART_OPTIONS */

}


/*
 * Place the player in the dungeon XXX XXX
 */
bool player_place(int y, int x)
{
        /* Paranoia XXX XXX */
        if (cave[y][x].m_idx != 0) return (FALSE);

	/* Paranoic zero-values handling */
	if ((y == 0) || (x == 0))
	{
            (void)new_player_spot();
               
	}
	
        /* Save player location */
        py = y;
        px = x;

        /* Success */
        return (-1);
	return TRUE;
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
