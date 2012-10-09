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
 * Delete a monster by index.
 *
 * This function causes the given monster to cease to exist for
 * all intents and purposes.  The monster record is left in place
 * but the record is wiped, marking it as "dead" (no race index)
 * so that it can be "skipped" when scanning the monster array,
 * and "excised" from the "mn_fast" array when needed.
 *
 * Thus, anyone who makes direct reference to the "mn_list[]" array
 * using monster indexes that may have become invalid should be sure
 * to verify that the "r_idx" field is non-zero.  All references
 * to "mn_list[c_ptr->m_idx]" are guaranteed to be valid, see below.
 */
void delete_monster_idx(s16b i)
{
   s16b x, y;

   monster_type *m_ptr = &mn_list[i];

   monster_race *r_ptr = &r_info[m_ptr->r_idx];

   /* Get location */
   y = m_ptr->fy;
   x = m_ptr->fx;

   /* Hack -- Reduce the racial counter */
   r_ptr->cur_num--;

   /* Hack -- count the number of "reproducers" */
   if (r_ptr->flags2 & RF2_MULTIPLY) num_repro--;

   /* Hack -- remove target monster */
   if (i == target_who) target_who = 0;

   /* Hack -- remove tracked monster */
   if (i == health_who) health_track(0);

   /* Monster is gone */
   dungeon.level[sublevel][y][x].m_idx = 0;

   /* Visual update */
   lite_spot(x, y);

   if (m_ptr->has_drop)
   {
      dlog(DEBUGALWAYS,"monster2.c: delete_monster_idx: deleting monster with has_drop still TRUE!");
      dump_stack(NULL);
      quit("monster2.c: delete_monster_idx: deleting monster with has_drop still TRUE!");
   }

   /* Wipe the Monster */
   WIPE(m_ptr, monster_type);
}

/*
 * Delete the monster, if any, at a given location
 */
void delete_monster(s16b x, s16b y)
{
   cave_cell_type *c_ptr;

   /* Paranoia */
   if (!in_bounds(x,y)) return;

   /* Check the grid */
   c_ptr = &dungeon.level[sublevel][y][x];

   /* Delete the monster (if any) */
   if (c_ptr->m_idx)
   {
      monster_type  *m_ptr = &mn_list[c_ptr->m_idx];
      monster_race  *r_ptr = &r_info[m_ptr->r_idx];

dlog(DEBUGMONST,"monster2.c: delete_monster: m_idx %d (%s) just got zapped @ %d,%d\n",
                c_ptr->m_idx, r_name+r_ptr->name, m_ptr->fx, m_ptr->fy);
      delete_monster_idx(c_ptr->m_idx);
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
void compact_monsters(s16b size,bool inven_too)
{
   s16b         i, num, cnt;

   s16b         cur_lev, cur_dis, chance;

   bool artifacts_found = FALSE;

   /* Message (only if compacting) */
   if (size) msg_print("Compacting monsters...");

dlog(DEBUGMONST,"monster2.c: compact_monsters: mn_top %d MAX_M_IDX %d size %d inven_too %d\n",
                mn_top, MAX_M_IDX, size, inven_too);
   /* Compact at least 'size' objects */
   for (num = 0, cnt = 1; num < size; cnt++)
   {
      /* Get more vicious each iteration */
      cur_lev = 5 * cnt;
dlog(DEBUGMONST,"monster2.c: compact_monsters: loop cnt %d cur_level %d num %d\n",
                cnt, cur_lev, num);

      /* Get closer each iteration */
      if (cnt > 20)
      {
         cur_dis = 0;
      }
      else
      {
         cur_dis = 5 * (20 - cnt);
      }
dlog(DEBUGMONST,"monster2.c: compact_monsters: loop cnt %d cur_level %d cur_dis %d num %d\n",
                cnt, cur_lev, cur_dis, num);

      /* Check all the monsters */
      for (i = 1; i < mn_max; i++)
      {
         monster_type *m_ptr = &mn_list[i];

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
         if ((r_ptr->flags1 & RF1_QUESTOR) && (cnt < 1000)) chance = 100;

         /* Try not to compact Unique Monsters */
         if (r_ptr->flags1 & RF1_UNIQUE) chance = 99;

         /* All monsters get a saving throw */
         if (rand_int(100) < chance) continue;

         artifacts_found = FALSE;
         if (m_ptr->has_drop)
         {
            s16b is_idx, j = 0, number = 0;

            if (!inven_too) continue;

            is_idx = item_set_this_monster(i);
            number = items_in_set(is_idx);
            for (j=0;j<number;j++)
            {
              if (artifact_p(&i_list[is_list[is_idx].index[j]]))
                 artifacts_found = TRUE;
                 break;
            }
            if (artifacts_found == TRUE)
            {
               continue;
            }
            else
            {
               s16b is_idx = item_set_this_monster(i);
               s16b j,number = items_in_set(is_idx);
               for (j=0;j<number;j++)
               {
                  invwipe(&i_list[is_list[is_idx].index[j]]);
               }
               is_list[is_idx].inuse = FALSE;
               is_list[is_idx].x = 0;
               is_list[is_idx].y = 0;
               is_list[is_idx].z = 0;
               /* this is probably paranoia - can delete_monster_idx fail? */
               m_ptr->has_drop = FALSE;
            }
         }
dlog(DEBUGMONST,"monster2.c: compact_monsters: m_idx %d (%s) just got compacted @ %d,%d\n",
                i, r_name+r_ptr->name, m_ptr->fx, m_ptr->fy);
         /* Delete the monster */
         delete_monster_idx(i);

         /* Count the monster */
         num++;
      }
   }

   /* Excise dead monsters (backwards!) */
   for (i = mn_max - 1; i >= 1; i--)
   {
      /* Get the i'th monster */
      monster_type *m_ptr = &mn_list[i];

      /* Skip real monsters */
      if (m_ptr->r_idx) continue;

      /* One less monster */
      mn_max--;

      /* Reorder */
      if (i != mn_max)
      {
         s16b nx = mn_list[mn_max].fx;
         s16b ny = mn_list[mn_max].fy;
         s16b nz = mn_list[mn_max].fz;
         s16b old_sublevel = 0;

         /* access the cave cell */
         if (dungeon.level_used[nz])
         {
            old_sublevel = sublevel;
            sublevel = nz;
         }
         else
         {
            quit("monster2.c: compact_monsters: monster exists on unknown sublevel!\n");
         }

         /* Update the cave */
         dungeon.level[sublevel][ny][nx].m_idx = i;

         /* Hack -- Update the target */
         if (target_who == (int)(mn_max)) target_who = i;

         /* Hack -- Update the health bar */
         if (health_who == (int)(mn_max)) health_track(i);

         /* Structure copy */
         mn_list[i] = mn_list[mn_max];

         if (mn_list[i].has_drop)
         {
            /* get the item_set */
            s16b is_idx = item_set_this_monster(mn_max);
            s16b j,number = items_in_set(is_idx);
            /* and set the new monster number */
            is_list[is_idx].y = i;
            for (j=0;j<number;j++)
            {
              object_type *i_ptr = &i_list[is_list[is_idx].index[j]];
              /* and set the new monster number again */
              i_ptr->iy = i;
            }
         }
         /* Wipe the hole */
         WIPE(&mn_list[mn_max], monster_type);

         /* restore the main level if necessary */
         sublevel = old_sublevel;
      }
   }

   /* Reset "mn_nxt" */
   mn_nxt = mn_max;

   /* Reset "mn_top" */
   mn_top = 0;

   /* Collect "live" monsters */
   for (i = 0; i < mn_max; i++)
   {
       /* Collect indexes */
       mn_fast[mn_top++] = i;
   }
   if (size) msg_print(NULL);
}

/*
 * Delete/Remove all the monsters when the player leaves the level
 *
 * This is an efficient method of simulating multiple calls to the
 * "delete_monster()" function, with no visual effects.
 */
void wipe_mn_list()
{
   s16b i;

   /* Delete all the monsters */
   for (i = mn_max - 1; i >= 1; i--)
   {
      monster_type *m_ptr = &mn_list[i];

      monster_race *r_ptr = &r_info[m_ptr->r_idx];

      /* Skip dead monsters */
      if (!m_ptr->r_idx) continue;

      /* Mega-Hack -- preserve Unique's XXX XXX XXX */

      /* Hack -- Reduce the racial counter */
      r_ptr->cur_num--;

      /* Monster is gone */
      dungeon.level[sublevel][m_ptr->fy][m_ptr->fx].m_idx = 0;

      /* Wipe the Monster */
      WIPE(m_ptr, monster_type);
   }

   /* Reset the monster array */
   mn_nxt = mn_max = 1;

   /* No live monsters */
   mn_top = 0;

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
 *
 * Note that this function must maintain the special "mn_fast"
 * array of indexes of "live" monsters.
 */
s16b mn_pop(void)
{
   s16b i, n, k;


   /* Normal allocation */
   if (mn_max < MAX_M_IDX)
   {
      /* Access the next hole */
      i = mn_max;

      /* Expand the array */
      mn_max++;

      /* Update "mn_fast" */
      mn_fast[mn_top++] = i;

      /* Return the index */
      return (i);
   }


   /* Check for some space */
   for (n = 1; n < MAX_M_IDX; n++)
   {
      /* Get next space */
      i = mn_nxt;

      /* Advance (and wrap) the "next" pointer */
      if (++mn_nxt >= MAX_M_IDX) mn_nxt = 1;

      /* Skip monsters in use */
      if (mn_list[i].r_idx) continue;

      /* Verify space XXX XXX */
      if (mn_top >= MAX_M_IDX) continue;

      /* Verify not allocated */
      for (k = 0; k < mn_top; k++)
      {
         /* Hack -- Prevent errors */
         if (mn_fast[k] == i) i = 0;
      }

      /* Oops XXX XXX */
      if (!i) continue;

      /* Update "mn_fast" */
      mn_fast[mn_top++] = i;

      /* Use this monster */
      return (i);
   }

   /* Warn the player */
   if (character_dungeon) msg_print("Too many monsters!");

   /* Try not to crash */
   return (0);
}

static s16b get_ghost(s16b monster_level)
{
   s16b          r_idx = -1, tries = 0, prob;
   monster_race *r_ptr;

dlog((DEBUGMONST | DEBUGGHOST),"monster2.c: get_ghost: level %d\n", monster_level);
   while ((tries<1000) && (r_idx<0))
   {
      r_idx = r_number + rand_int(r_number_total - r_number);
      tries++;

      /* Access the actual race */
      r_ptr = &r_info[r_idx];

      /* Hack -- "unique" monsters must be "unique" */
      if ((r_ptr->flags1 & RF1_UNIQUE) &&
          (r_ptr->cur_num >= r_ptr->max_num))
      {
          r_idx = -1;
          continue;
      }
      prob = 1000; /* a one in 1000 chance is default */
      /* the test for out-of-level monsters */
      if ((r_ptr->level == monster_level) && (r_ptr->level < (monster_level + 2)))
         prob = 2;
      else if ((r_ptr->level > (monster_level - 2)) && (r_ptr->level < (monster_level + 4)))
         prob = 4;
      else if ((r_ptr->level > (monster_level - 4)) && (r_ptr->level < (monster_level + 8)))
         prob = 8;
      else if ((r_ptr->level > (monster_level - 6)) && (r_ptr->level < (monster_level + 12)))
         prob = 12;
      else if ((r_ptr->level > (monster_level - 8)) && (r_ptr->level < (monster_level + 16)))
         prob = 16;
      else if ((r_ptr->level > (monster_level - 10)) && (r_ptr->level < (monster_level + 20)))
         prob = 20;
      else if ((r_ptr->level < (monster_level - 20)) || (monster_level < 8))
         prob = 0; /* don't summon them more than 20 levels out of depth or below level 8 */
dlog((DEBUGMONST | DEBUGGHOST),"monster2.c: get_ghost: r_idx %d name %s level %d p_ptr->depth %d prob %d\n",
                r_idx, r_ptr->name + r_name, r_ptr->level, monster_level, prob);

      if (randint(prob)!=1) continue;
   }
   if (tries==1000)
      return (0);
   else
      return (r_idx);
}

/*
 * Place a player ghost near the given
 * location.  Return TRUE if a monster was actually summoned.
 *
 * We will attempt to place the ghost up to 10 times before giving up.
 *
 */
bool summon_ghost(s16b x_org, s16b y_org)
{
   s16b i, x, y, r_idx;

   bool result = FALSE;

dlog((DEBUGMONST | DEBUGGHOST),"monster2.c: summon_ghost: %d,%d starting\n", x_org, y_org);
   /* Try to place it */
   for (i = 0; i < 20; ++i)
   {
      /* Pick a distance */
      s16b d = (i / 15) + 1;

      /* Pick a location */
      scatter(&x, &y, x_org, y_org, d, 0);

      /* Require "empty" floor grid */
      if (!empty_grid_bold(x, y)) continue;

      /* Hack -- no summon on glyph of warding */
      if (test_grid(x, y, DUNG_FLOOR, DUNG_FLOOR_GLYPH)) continue;

      /* Pick a monster */
      r_idx = get_ghost(monster_level);

      /* Restore monster level */
      monster_level = p_ptr->mdepth;

      /* Handle failure */
      if (!r_idx) return (FALSE);

      /* Attempt to place the monster (awake, allow groups) */
      result = place_monster_aux(x, y, r_idx, FALSE, TRUE, 0, 0);

      /* Done */
      break;
   }

   /* Failure */
   return (result);
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
s16b get_mon_num(s16b level)
{
   s16b          i, j, p;
   s16b          r_idx;
   long          value, total;
   monster_race *r_ptr;
   alloc_entry  *table = alloc_race_table;


   /* Boost the level */
   if (level > 0)
   {
      /* Occasional "nasty" monster */
      if (rand_int(NASTY_MON) == 0)
      {
         /* Pick a level bonus */
         int d = level / 10 + 2;

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

#if 0
this is the routine to get ghosts - disfunctional now
      if ((level>5) && (randint(200-level) == 1))
      {
         s16b temp;
         temp = get_ghost(level);
         if (temp)
         {
dlog((DEBUGMONST | DEBUGGHOST),"monster2.c: get_mon_num: ghost r_idx %d accepted\n", r_idx);
            return (temp);
         }
      }
#endif

   /* Process probabilities */
   for (i = 0; i < alloc_race_size; i++)
   {
      /* Monsters are sorted by depth */
      if (table[i].level > level) break;

      /* Default */
      table[i].prob3 = 0;

      /* Hack -- No town monsters in dungeon */
      if ((level > 0) && (table[i].level <= 0)) continue;

      /* Access the "r_idx" of the chosen monster */
      r_idx = table[i].index;

      /* Access the actual race */
      r_ptr = &r_info[r_idx];

      /* Hack -- "unique" monsters must be "unique" */
      if ((r_ptr->flags1 & (RF1_UNIQUE)) &&
          (r_ptr->cur_num >= r_ptr->max_num))
      {
         continue;
      }

      /* Forced-depth monsters never appear at levels other than their native depth. */
      if ((r_ptr->flags1 & (RF1_FORCE_DEPTH)) && (r_ptr->level != p_ptr->mdepth)) continue;

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

   /* Try for a "harder" monster once (60%) or twice (10%) */
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
 *   0x0001 --> Objective (or Reflexive)
 *   0x0002 --> Possessive (or Reflexive)
 *   0x0004 --> Use indefinites for hidden monsters ("something")
 *   0x0008 --> Use indefinites for visible monsters ("a kobold")
 *   0x0010 --> Pronominalize hidden monsters
 *   0x0020 --> Pronominalize visible monsters
 *   0x0040 --> Assume the monster is hidden
 *   0x0080 --> Assume the monster is visible
 *   0x0100 --> No the in front of the name
 *
 * Useful Modes:
 *   0x00 --> Full nominative name ("the kobold") or "it"
 *   0x04 --> Full nominative name ("the kobold") or "something"
 *   0x80 --> Genocide resistance name ("the kobold")
 *   0x88 --> Killing name ("a kobold")
 *   0x22 --> Possessive, genderized if visable ("his") or "its"
 *   0x23 --> Reflexive, genderized if visable ("himself") or "itself"
 */
void monster_desc(char *desc, monster_type *m_ptr, u16b mode)
{
   cptr           res;

   monster_race  *r_ptr = &r_info[m_ptr->r_idx];

   cptr           name = (r_name + r_ptr->name);

   bool           seen, pron;


   /* Can we "see" it (exists + forced, or visible + not unforced) */
   seen = (m_ptr && ((mode & 0x80) || (!(mode & 0x40) && m_ptr->ml)));

   /* Sexed Pronouns (seen and allowed, or unseen and allowed) */
   pron = (m_ptr && ((seen && (mode & 0x20)) || (!seen && (mode & 0x10))));

   /* First, try using pronouns, or describing hidden monsters */
   if (!seen || pron)
   {
      /* an encoding of the monster "sex" */
      s16b kind = 0x00;

      /* Extract the gender (if applicable) */
      if (r_ptr->flags1 & RF1_FEMALE) kind = 0x20;
      else if (r_ptr->flags1 & RF1_MALE) kind = 0x10;

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
         if (!(mode & 0x0100))
         {
            (void)strcpy(desc, "the ");
            (void)strcat(desc, name);
         }
         else
         {
            (void)strcpy(desc, name);
         }
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
void lore_do_probe(s16b m_idx)
{
   monster_type *m_ptr = &mn_list[m_idx];

   monster_race *r_ptr = &r_info[m_ptr->r_idx];

   /* Hack -- Memorize some flags */
   r_ptr->r_flags1 = r_ptr->flags1;
   r_ptr->r_flags2 = r_ptr->flags2;
   r_ptr->r_flags3 = r_ptr->flags3;

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
void lore_treasure(s16b m_idx, s16b num_item, s16b num_gold)
{
   monster_type *m_ptr = &mn_list[m_idx];

   monster_race *r_ptr = &r_info[m_ptr->r_idx];

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
 * Note that only monsters on the current panel can be "visible",
 * and then only if they are (1) in line of sight and illuminated
 * by light or infravision, or (2) nearby and detected by telepathy.
 *
 * The player can choose to be disturbed by several things, including
 * "disturb_enter" (monster becomes "easily" viewable), "disturb_leave"
 * (monster becomes no longer "easily" viewable), "disturb_move" (monster
 * which is "easily" viewable moves), and "disturb_near" (monster which
 * is "easily" viewable, and within 5 grids of the player, moves).  This
 * may or may not be the "best" way to handle disturbance and telepathy,
 * but it should be better than the old method.
 */
void update_mon(s16b m_idx, bool dist)
{
   monster_type *m_ptr = &mn_list[m_idx];

   monster_race *r_ptr = &r_info[m_ptr->r_idx];

   /* The current monster location */
   s16b fy = m_ptr->fy;
   s16b fx = m_ptr->fx;

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
   if (dist)
   {
      s16b d, dy, dx;

      /* Distance components */
      dy = (py > fy) ? (py - fy) : (fy - py);
      dx = (px > fx) ? (px - fx) : (fx - px);

      /* Approximate distance */
      d = (dy > dx) ? (dy + (dx>>1)) : (dx + (dy>>1));

      /* Save the distance */
      m_ptr->cdis = d;
      if (m_ptr->fz != sublevel) m_ptr->cdis = (MAX_SIGHT + 1);
   }

   /* Process "distant" monsters */
   if (m_ptr->cdis > MAX_SIGHT)
   {
      /* Ignore unseen monsters */
      if (!m_ptr->ml) return;
   }

   /* Process "nearby" monsters on the current "panel" */
   else if (panel_contains(fx, fy) && (m_ptr->fz == sublevel))
   {
      cave_cell_type *c_ptr = &dungeon.level[sublevel][fy][fx];

      /* Normal line of sight, and player is not blind */
      if ((c_ptr->fdat & CAVE_VIEW) && (!p_ptr->blind))
      {
         /* Use "infravision" */
         if (m_ptr->cdis <= (p_ptr->see_infra))
         {
            /* Infravision only works on "warm" creatures */
            /* Below, we will need to know that infravision failed */
            if (r_ptr->flags2 & RF2_COLD_BLOOD) do_cold_blood = TRUE;

            /* Infravision works */
            if (!do_cold_blood) easy = flag = TRUE;
         }

         /* Use "illumination" */
         if (c_ptr->fdat & (CAVE_LITE | CAVE_GLOW))
         {
            /* Take note of invisibility */
            if (r_ptr->flags2 & RF2_INVISIBLE) do_invisible = TRUE;

            /* Visible, or detectable, monsters get seen */
            if (!do_invisible || p_ptr->see_inv) easy = flag = TRUE;
         }
      }

      /* Telepathy can see all "nearby" monsters with "minds" */
      if (m_ptr->cdis <= (p_ptr->telepathy))
      {
          /* Empty mind, no telepathy */
          if (r_ptr->flags2 & RF2_EMPTY_MIND)
          {
              do_empty_mind = TRUE;
          }

          /* Weird mind, occasional telepathy */
          else if (r_ptr->flags2 & RF2_WEIRD_MIND)
          {
              do_weird_mind = TRUE;
              if (rand_int(100) < 10) hard = flag = TRUE;
          }

          /* Normal mind, allow telepathy */
          else
          {
              hard = flag = TRUE;
          }

          /* Apply telepathy */
          if (hard)
          {
              /* Hack -- Memorize mental flags */
              if (r_ptr->flags2 & RF2_SMART) r_ptr->r_flags2 |= RF2_SMART;
              if (r_ptr->flags2 & RF2_STUPID) r_ptr->r_flags2 |= RF2_STUPID;
          }
      }

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
         lite_spot(fx, fy);

         /* Update health bar as needed */
         if (health_who == m_idx) p_ptr->redraw1 |= (PR1_HEALTH);

         /* Hack -- Count "fresh" sightings */
         if (r_ptr->r_sights < MAX_SHORT) r_ptr->r_sights++;
      }

      /* Memorize various observable flags */
      if (do_empty_mind) r_ptr->r_flags2 |= RF2_EMPTY_MIND;
      if (do_weird_mind) r_ptr->r_flags2 |= RF2_WEIRD_MIND;
      if (do_cold_blood) r_ptr->r_flags2 |= RF2_COLD_BLOOD;
      if (do_invisible) r_ptr->r_flags2 |= RF2_INVISIBLE;

      /* Efficiency -- Notice multi-hued monsters */
      if (r_ptr->flags1 & RF1_ATTR_MULTI) scan_monsters = TRUE;
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
         lite_spot(fx, fy);

         /* Update health bar as needed */
         if (health_who == m_idx) p_ptr->redraw1 |= (PR1_HEALTH);
      }
   }

   /* The monster is now easily visible */
   if (easy)
   {
      /* Change */
      if (!m_ptr->los)
      {
         /* Mark as easily visible */
         m_ptr->los = TRUE;

         /* Disturb on appearance */
         if (disturb_enter) disturb(1, 0);
      }
   }

   /* The monster is not easily visible */
   else
   {
      /* Change */
      if (m_ptr->los)
      {
         /* Mark as not easily visible */
         m_ptr->los = FALSE;

         /* Disturb on disappearance */
         if (disturb_leave) disturb(1, 0);
      }
   }
}

/*
 * This function simply updates all the (non-dead) monsters (see above).
 * we check for correct sublevels in update_mon() above.
 */
void update_monsters(bool dist)
{
   s16b          i;

   /* Update each (live) monster */
   for (i = 1; i < mn_max; i++)
   {
      monster_type *m_ptr = &mn_list[i];

      /* Skip dead monsters */
      if (!m_ptr->r_idx) continue;

      /* Update the monster */
      update_mon(i, dist);
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
/* jk - s16b extra_drop is a means of giving greater vault monsters a better drop */
/* and not wasting the floor below them with it! */
/* no_check is used when a wizard wants a monster - means don't check!      */
bool place_monster_one(s16b x, s16b y, s16b r_idx, bool slp, s16b extra_drop, bool no_check)
{
   s16b                 i, drop = 0;
   cave_cell_type      *c_ptr;
   monster_type        *m_ptr;
   monster_race        *r_ptr = &r_info[r_idx];
   cptr                 name = (r_name + r_ptr->name);

   /* Verify location */
dlog(DEBUGMONST,"monster2.c: place_monster_one: %d,%d r_idx %d slp %d drop %d no_check %d\n",
                x, y, r_idx, slp, drop, no_check);
   if (!in_bounds(x, y)) return (FALSE);

   /* Require empty space */
   if (!empty_grid_bold(x, y)) return (FALSE);

   /* Hack -- no creation on glyph of warding */
   if (!no_check && test_grid(x, y, DUNG_FLOOR, DUNG_FLOOR_GLYPH)) return (FALSE);

   /* Paranoia */
   if (!r_idx) return (FALSE);

   /* Paranoia */
   if (!r_ptr->name) return (FALSE);

   /* Hack -- "unique" monsters must be "unique" */
   if (!no_check && (r_ptr->flags1 & RF1_UNIQUE) && (r_ptr->cur_num >= r_ptr->max_num))
   {
      /* Cannot create */
      return (FALSE);
   }

   /* Depth monsters may NOT be created out of depth */
   if (!no_check && (r_ptr->flags1 & RF1_FORCE_DEPTH) && (p_ptr->mdepth < r_ptr->level))
   {
      /* Cannot create */
      return (FALSE);
   }

dlog(DEBUGMONST,"monster2.c: place_monster_one: step 1\n");
   /* Powerful monster */
   if (r_ptr->level > p_ptr->mdepth)
   {
      /* Unique monsters */
      if (r_ptr->flags1 & RF1_UNIQUE)
      {
         /* Message for cheaters */
         if (cheat_hear) msg_format("Deep Unique (%s).", name);

         /* Boost rating by twice delta-depth */
         rating += (r_ptr->level - p_ptr->mdepth) * 2;
      }

      /* Normal monsters */
      else
      {
         /* Message for cheaters */
         if (cheat_hear) msg_format("Deep Monster (%s).", name);

         /* Boost rating by delta-depth */
         rating += (r_ptr->level - p_ptr->mdepth);
      }
   }

   /* Note the monster */
   else if (r_ptr->flags1 & RF1_UNIQUE)
   {
      /* Unique monsters induce message */
      if (cheat_hear) msg_format("Unique (%s).", name);
   }

dlog(DEBUGMONST,"monster2.c: place_monster_one: step 2\n");
   /* Access the location */
   c_ptr = &dungeon.level[sublevel][y][x];

   /* Make a new monster */
   c_ptr->m_idx = mn_pop();

   /* Mega-Hack -- catch "failure" */
   if (!c_ptr->m_idx) return (FALSE);
dlog(DEBUGMONST,"monster2.c: place_monster_one: step 3 m_idx %d\n", c_ptr->m_idx);

   /* Get a new monster record */
   m_ptr = &mn_list[c_ptr->m_idx];

   /* Save the race */
   m_ptr->r_idx = r_idx;

   /* Place the monster at the location */
   m_ptr->fx = x;
   m_ptr->fy = y;
   m_ptr->fz = sublevel;

   m_ptr->has_drop = FALSE;
   m_ptr->attacked = MAX_ATTACK_HISTORY;
dlog(DEBUGMONST,"monster2.c: place_monster_one: step 4\n");

dlog(DEBUGMONST,"monster2.c: place_monster_one: placed @ %d,%d t_idx %d m_idx %d mt,st %d,%d %s\n",
                 x, y, c_ptr->t_idx, c_ptr->m_idx, c_ptr->mtyp, c_ptr->styp,
                 f_name + f_info[get_f_idx(c_ptr->mtyp, c_ptr->styp)].name);

   if (r_info[r_idx].flags2 & RF2_MULTIPLY)
   {
       m_ptr->breed_counter = (byte)r_info[r_idx].max_gen;
   }

   drop = extra_drop;
   if (r_ptr->flags1 & RF1_DROP_GREAT) drop += p_ptr->mdepth + 30;
   if (r_ptr->flags1 & RF1_DROP_GOOD) drop += p_ptr->mdepth + 15;

   create_monster_inventory(c_ptr->m_idx, drop);
dlog(DEBUGGHOST,"monster2.c: monster_place_one: r_idx %d r_number %d\n",
                r_idx, r_number);
   if (r_idx >= r_number)
   {
      create_ghost_inventory(c_ptr->m_idx);
   }

   /* Hack -- Count the monsters on the level */
   r_ptr->cur_num++;

   /* Hack -- count the number of "reproducers" */
   if (r_ptr->flags2 & RF2_MULTIPLY) num_repro++;

   /* Assign maximal hitpoints */
   if (r_ptr->flags1 & RF1_FORCE_MAXHP)
   {
      m_ptr->maxhp = maxroll(r_ptr->hdice, r_ptr->hside);
   }
   else
   {
      m_ptr->maxhp = damroll(r_ptr->hdice, r_ptr->hside);
   }

   /* And start out fully healthy */
   m_ptr->hp = m_ptr->maxhp;

dlog(DEBUGGHOST,"monster2.c: monster_place_one: force_maxhp %d hp %d\n",
                r_ptr->flags1 & RF1_FORCE_MAXHP, m_ptr->hp);

   /* Extract the monster base speed */
   m_ptr->mspeed = r_ptr->speed;

   /* Hack -- small racial variety */
   if (!(r_ptr->flags1 & RF1_UNIQUE))
   {
      /* Allow some small variation per monster */
      i = extract_energy[r_ptr->speed] / 10;
      if (i) m_ptr->mspeed += rand_spread(0, i);
   }

   /* Give a random starting energy */
   m_ptr->energy = rand_int(100);

   /* Hack -- Reduce risk of "instant death by breath weapons" */
   if (r_ptr->flags1 & RF1_FORCE_SLEEP)
   {
      /* Start out with minimal energy */
      m_ptr->energy = rand_int(10);
      /* Monster is still being nice */
      m_ptr->mflag |= (MFLAG_NICE);

   }

   /* No "damage" yet */
   m_ptr->stun = 0;
   m_ptr->confused = 0;
   m_ptr->afraid = 0;

   m_ptr->escaping = 0;
   /* No knowledge */
   m_ptr->cdis = 0;
   m_ptr->los = FALSE;
   m_ptr->ml = FALSE;

   /* Update the monster */
   update_mon(c_ptr->m_idx, TRUE);

   /* Assume no sleeping */
   m_ptr->csleep = 0;

   /* Enforce sleeping if needed */
   if (slp && r_ptr->sleep)
   {
      s16b val = r_ptr->sleep;
      m_ptr->csleep = ((val * 2) + randint(val * 10));
   }

dlog(DEBUGMONST,"monster2.c: place_monster_one: step 5 - returning\n");
   /* Success */
   return (TRUE);
}

/*
 * Maximum size of a group of monsters
 */
#define GROUP_MAX       32

/*
 * Attempt to place a "group" of monsters around the given location
 */
static bool place_monster_group(s16b x, s16b y, s16b r_idx, bool slp, s16b extra_drop, s16b max_group)
{
   monster_race *r_ptr = &r_info[r_idx];

   s16b old, n, i;
   s16b total = 0, extra = 0;

   s16b hack_n = 0;

   s16b hack_y[GROUP_MAX];
   s16b hack_x[GROUP_MAX];

   /* Pick a group size */
   total = randint(13);

   /* Hard monsters, small groups */
   if (r_ptr->level > p_ptr->mdepth)
   {
      extra = r_ptr->level - p_ptr->mdepth;
      extra = 0 - randint(extra);
   }

   /* Easy monsters, large groups */
   else if (r_ptr->level < p_ptr->mdepth)
   {
      extra = p_ptr->mdepth - r_ptr->level;
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
   if ( (!(r_ptr->flags1 & RF1_UNIQUE)) && (total > max_group) && (max_group > 0) )
   {
      total = max_group;
   }

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
      s16b hx = hack_x[n];
      s16b hy = hack_y[n];

      /* Check each direction, up to total */
      for (i = 0; (i < 8) && (hack_n < total); i++)
      {
         s16b mx = hx + ddx_ddd[i];
         s16b my = hy + ddy_ddd[i];

         /* Walls and Monsters block flow */
         if (!empty_grid_bold(mx, my)) continue;

         /* Attempt to place another monster */
         if (place_monster_one(mx, my, r_idx, slp, extra_drop, FALSE))
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
static s16b place_monster_idx = 0;

/*
 * Hack -- help pick an escort type
 */
static bool place_monster_okay(s16b r_idx)
{
   monster_race *r_ptr = &r_info[place_monster_idx];

   monster_race *z_ptr = &r_info[r_idx];

   /* Require similar "race" */
   if (z_ptr->d_char != r_ptr->d_char) return (FALSE);

   /* Skip more advanced monsters */
   if (z_ptr->level > r_ptr->level) return (FALSE);

   /* Skip unique monsters */
   if (z_ptr->flags1 & RF1_UNIQUE) return (FALSE);

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
 * max_group means the maximum number that is grouped extra when
 * summoning. It doesn't count for uniques, and it doesn't count
 * when max_group = 0
 */
bool place_monster_aux(s16b x, s16b y, s16b r_idx, bool slp,
                       bool grp, s16b extra_drop, s16b max_group)
{
   s16b                 i;

   monster_race        *r_ptr = &r_info[r_idx];

   /* Place one monster, or fail */
   if (!place_monster_one(x, y, r_idx, slp, extra_drop, FALSE)) return (FALSE);

   /* Require the "group" flag */
   if (!grp) return (TRUE);

   /* Friends for certain monsters */
   if (r_ptr->flags1 & RF1_FRIENDS)
   {
       /* Attempt to place a group */
       (void)place_monster_group(x, y, r_idx, slp, extra_drop, max_group);
   }

   /* Escorts for certain monsters */
   if (r_ptr->flags1 & RF1_ESCORT)
   {
       s16b group_done = 0;

       /* Try to place several "escorts" */
       for (i = 0; i < 50; i++)
       {
           s16b nx, ny, z, d = 3;

           /* Pick a location */
           scatter(&nx, &ny, x, y, d, 0);

           /* Require empty grids */
           if (!empty_grid_bold(nx, ny)) continue;

           /* Set the escort index */
           place_monster_idx = r_idx;

           /* Set the escort hook */
           get_mon_num_hook = place_monster_okay;

           /* Prepare allocation table */
           get_mon_num_prep();

           /* Pick a random race */
           z = get_mon_num(r_ptr->level);

           /* Remove restriction */
           get_mon_num_hook = NULL;

           /* Prepare allocation table */
           get_mon_num_prep();

           /* Handle failure */
           if (!z) break;

           /* Place a single escort */
           /* they should not have any extra_drop, of course */
           (void)place_monster_one(nx, ny, z, slp, 0, FALSE);

           if (max_group > 0)
           {
              if (!r_ptr->flags1 & RF1_UNIQUE) group_done++;
              if (group_done > max_group) break;
           }

           /* Place a "group" of escorts if needed */
           if ((r_info[z].flags1 & RF1_FRIENDS) ||
               (r_ptr->flags1 & RF1_ESCORTS))
           {
               /* Place a group of monsters  -- again without extra drop */
               (void)place_monster_group(nx, ny, z, slp, 0, max_group);
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
bool place_monster(s16b x, s16b y, bool slp, bool grp, s16b extra_drop)
{
   s16b r_idx;

   /* Pick a monster */
   r_idx = get_mon_num(monster_level);

   /* Handle failure */
   if (!r_idx) return (FALSE);

   /* Attempt to place the monster */
   if (place_monster_aux(x, y, r_idx, slp, grp, extra_drop, 0)) return (TRUE);

   /* Oops */
   return (FALSE);
}

/*
 * Attempt to allocate a random monster in the dungeon.
 * Place the monster at least "dis" distance from the player.
 * Use "slp" to choose the initial "sleep" status
 * Use "monster_level" for the monster level
 */
/* jk - town: 0 means in town 1 means out of town 2 means anywhere */
bool alloc_monster(s16b dis, s16b slp, s16b town)
{
   s16b                 x = 0, y = 0, count = 0;

   /* Find a legal, distant, unoccupied, space */
   while ((count++) < 100)
   {
      /* Pick a location */
      x = rand_int(cur_wid);
      y = rand_int(cur_hgt);

      if (dungeon.level[sublevel][y][x].fdat & CAVE_AREN) continue;
      if (dungeon.level[sublevel][y][x].fdat & CAVE_VAULT) continue;

      if (!town)
      {
         if ((x>SCREEN_WID) || (y>SCREEN_HGT)) continue;
      }
      else if (town==1)
      {
         if ((x<SCREEN_WID) && (y<SCREEN_HGT)) continue;
      }

      /* Require "naked" floor grid */
      if (!naked_grid_bold(x,y)) continue;

      /* Accept far away grids */
      if (distance(x, y, px, py) > dis) break;
   }

   /* Attempt to place the monster, allow groups */
   if ((count < 100) && place_monster(x, y, slp, TRUE, 0)) return (TRUE);

   /* Nope */
   return (FALSE);
}

/*
 * Hack -- the "type" of the current "summon specific"
 */
static s16b summon_specific_type = 0;

/*
 * Hack -- help decide if a monster race is "okay" to summon
 */
bool summon_specific_okay(s16b r_idx)
{
   monster_race *r_ptr = &r_info[r_idx];

   bool okay = FALSE;

   /* Hack -- no specific type specified */
   if (!summon_specific_type) return (TRUE);

   /* Check our requirements */
   switch (summon_specific_type)
   {
      case SUMMON_QUYLTHULG:
         okay = ((r_ptr->d_char == 'Q') && !(r_ptr->flags1 & RF1_UNIQUE));
         break;

      /* I had loads of trouble: troll high priests come in groups, so they */
      /* summon up whole groups of other troll high priests, which is a bad */
      /* thing: 20 summoning 2400 HP trolls are not funny anymore           */
      case SUMMON_TROLL:
         okay = ((r_ptr->d_char == 'T') && !(r_ptr->flags1 & RF1_UNIQUE));
         if (r_ptr->flags6 & RF6_S_TROLLS) okay = (randint(10)==0);
         break;

      case SUMMON_ANT:
         okay = ((r_ptr->d_char == 'a') && !(r_ptr->flags1 & RF1_UNIQUE));
         break;

      /* summon non-multiplying animals to the wilderness */
      case SUMMON_ANIMAL:
         okay = ( (r_ptr->flags3 & RF3_ANIMAL) && !(r_ptr->flags2 & RF2_MULTIPLY));
         break;

      case SUMMON_SPIDER:
         okay = ((r_ptr->d_char == 'S') && !(r_ptr->flags1 & RF1_UNIQUE));
         break;

      case SUMMON_HOUND:
         okay = (((r_ptr->d_char == 'C') || (r_ptr->d_char == 'Z')) &&
                 !(r_ptr->flags1 & RF1_UNIQUE));
         break;

      case SUMMON_HYDRA:
         okay = ((r_ptr->d_char == 'M') && !(r_ptr->flags1 & RF1_UNIQUE));
         break;

      case SUMMON_ANGEL:
         okay = ((r_ptr->d_char == 'A') && !(r_ptr->flags1 & RF1_UNIQUE));
         break;

      case SUMMON_DEMON:
         okay = ((r_ptr->flags3 & RF3_DEMON) && !(r_ptr->flags1 & RF1_UNIQUE));
         break;

      case SUMMON_UNDEAD:
         okay = ((r_ptr->flags3 & RF3_UNDEAD) && !(r_ptr->flags1 & RF1_UNIQUE));
         /* don't summon player ghosts unless as greater undead */
         if (r_idx > r_number) okay = FALSE;
         break;

      case SUMMON_DRAGON:
         okay = ((r_ptr->flags3 & RF3_DRAGON) && !(r_ptr->flags1 & RF1_UNIQUE));
         break;

      case SUMMON_HI_UNDEAD:
         okay = ((r_ptr->d_char == 'L') ||
                 (r_ptr->d_char == 'V') ||
                 (r_ptr->d_char == 'W'));
         /* summon player ghosts as greater undead */
         if (r_idx > r_number) okay = TRUE;
         break;

      case SUMMON_HI_DRAGON:
         okay = (r_ptr->d_char == 'D');
         break;

      case SUMMON_WRAITH:
         okay = ((r_ptr->d_char == 'W') && (r_ptr->flags1 & RF1_UNIQUE));
         break;

      case SUMMON_UNIQUE:
         okay = (r_ptr->flags1 & RF1_UNIQUE);
         break;

      case SUMMON_NON_UNIQUE:
         okay = !(r_ptr->flags1 & RF1_UNIQUE);
         break;
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
 * Note: SUMMON_UNIQUE and SUMMON_WRAITH (XXX) will summon Unique's
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
bool summon_specific(s16b x1, s16b y1, s16b lev, s16b type, s16b max_group)
{
   s16b i, x, y, r_idx;

   bool result = FALSE;

   /* Try to place it */
   for (i = 0; i < 20; ++i)
   {
      /* Pick a distance */
      s16b d = (i / 15) + 1;

      /* Pick a location */
      scatter(&x, &y, x1, y1, d, 0);

      /* Require "empty" floor grid */
      if (!empty_grid_bold(x, y)) continue;

      /* Hack -- no summon on glyph of warding */
      if (test_grid(x, y, DUNG_FLOOR, DUNG_FLOOR_GLYPH)) continue;

      /* Save the "summon" type */
      summon_specific_type = type;

      /* Require "okay" monsters */
      get_mon_num_hook = summon_specific_okay;

      /* Prepare allocation table */
      get_mon_num_prep();

      /* Choose a monster level */
      monster_level = (p_ptr->mdepth + lev) / 2 + 5;

      /* Pick a monster */
      r_idx = get_mon_num(monster_level);
      /* Restore monster level */
      monster_level = p_ptr->mdepth;

      /* Remove restriction */
      get_mon_num_hook = NULL;

      /* Prepare allocation table */
      get_mon_num_prep();

      /* Forget "summon" type */
      summon_specific_type = 0;

      /* Handle failure */
      if (!r_idx) return (FALSE);

      /* Attempt to place the monster (awake, allow groups) */
      /* Groups must be allowed, or you'll be able to summon */
      /* for example the Queen Ant without any other ants */
      result = place_monster_aux(x, y, r_idx, FALSE, TRUE, 0, max_group);

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
 */
bool multiply_monster(s16b m_idx)
{
   monster_type        *m_ptr = &mn_list[m_idx];

   s16b                 i, x, y;

   bool result = FALSE;

   if (r_info[m_ptr->r_idx].flags2 & RF2_MULTIPLY)
   {
      /* do we have any generations left */
      if (!m_ptr->breed_counter) return FALSE;
   }

   /* Try up to 18 times */
   for (i = 0; i < 18; i++)
   {
      s16b d = 1;

      /* Pick a location */
      scatter(&x, &y, m_ptr->fx, m_ptr->fy, d, 0);

      /* Require an "empty" floor grid */
      if (!empty_grid_bold(x, y)) continue;

      /* Create a new monster (awake, no groups) */
      result = place_monster_aux(x, y, m_ptr->r_idx, FALSE, FALSE, 0, 0);
      if (result)
      {
         cave_cell_type    *c_ptr     = &dungeon.level[sublevel][y][x];
         monster_type *m_ptr_new = &mn_list[c_ptr->m_idx];
         m_ptr_new->breed_counter = m_ptr->breed_counter - 1;
      }

      /* Done */
      break;
   }
   if (r_info[m_ptr->r_idx].flags2 & RF2_MULTIPLY)
   {
      /* do we have any generations left */
      m_ptr->breed_counter--;
   }

   /* Result */
   return (result);
}

/*
 * Dump a message describing a monster's reaction to damage
 *
 * Technically should attempt to treat "Beholder"'s as jelly's
 */
void message_pain(s16b m_idx, s16b dam)
{
   long                 oldhp, newhp, tmp;
   s16b                 percentage;
   monster_type        *m_ptr = &mn_list[m_idx];
   monster_race        *r_ptr = &r_info[m_ptr->r_idx];
   project_who_type     who;
   char                 m_name[80];

   if (!health_who) health_track(m_idx);

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

   /* it doesn't really matter who does it, since aggravate_monsters only uses */
   /* this value to determine if any monsters should be skipped. If it is      */
   /* WHO_PLAYER, the player is skipped, which is harmless                     */
   who.type = WHO_PLAYER;
   /* this to see what monsters are neighbours of the monster we hit           */
   who.index = m_idx;

   /* players with very high stealth succeed in somehow silencing the screams  */
   /* note that dam isn't needed for anything else later on in this function   */
   if (randint(1000) < (p_ptr->skill_stl * p_ptr->skill_stl))
   {
      dam = 0;
   }
   else if (randint(625) < (p_ptr->skill_stl * p_ptr->skill_stl))
   {
      dam = dam / 5;
   }
   /* Jelly's, Mold's, Vortex's, Quthl's */
   if (strchr("jmvQ", r_ptr->d_char))
   {
      if (percentage > 95)
      {
         msg_format("%^s barely notices.", m_name);
      }
      else if (percentage > 75)
      {
         msg_format("%^s flinches.", m_name);
      }
      else if (percentage > 50)
      {
         msg_format("%^s squelches.", m_name);
      }
      else if (percentage > 35)
      {
         msg_format("%^s quivers in pain.", m_name);
      }
      else if (percentage > 20)
      {
         msg_format("%^s writhes about.", m_name);
      }
      else if (percentage > 10)
      {
         msg_format("%^s writhes in agony.", m_name);
      }
      else
      {
         msg_format("%^s jerks limply.", m_name);
      }
   }

   /* Dogs and Hounds */
   else if (strchr("CZ", r_ptr->d_char))
   {
      if (percentage > 95)
      {
         msg_format("%^s shrugs off the attack.", m_name);
      }
      else if (percentage > 75)
      {
         msg_format("%^s snarls with pain.", m_name);
         aggravate_monsters(&who, dam*2);
      }
      else if (percentage > 50)
      {
         msg_format("%^s yelps in pain.", m_name);
         aggravate_monsters(&who, dam*5);
      }
      else if (percentage > 35)
      {
         msg_format("%^s howls in pain.", m_name);
         aggravate_monsters(&who, dam*10);
      }
      else if (percentage > 20)
      {
         msg_format("%^s howls in agony.", m_name);
         aggravate_monsters(&who, dam*20);
      }
      else if (percentage > 10)
      {
         msg_format("%^s writhes in agony.", m_name);
      }
      else
      {
         msg_format("%^s yelps feebly.", m_name);
      }
   }

   /* One type of monsters (ignore,squeal,shriek) */
   else if (strchr("FIKMRSXabclqrst", r_ptr->d_char))
   {
      if (percentage > 95)
      {
         msg_format("%^s ignores the attack.", m_name);
      }
      else if (percentage > 75)
      {
         msg_format("%^s grunts with pain.", m_name);
      }
      else if (percentage > 50)
      {
         msg_format("%^s squeals in pain.", m_name);
         aggravate_monsters(&who, dam*2);
      }
      else if (percentage > 35)
      {
         msg_format("%^s shrieks in pain.", m_name);
         aggravate_monsters(&who, dam*5);
      }
      else if (percentage > 20)
      {
         msg_format("%^s shrieks in agony.", m_name);
         aggravate_monsters(&who, dam*10);
      }
      else if (percentage > 10)
      {
         msg_format("%^s writhes in agony.", m_name);
      }
      else
      {
         msg_format("%^s cries out feebly.", m_name);
      }
   }

   /* Another type of monsters (shrug,cry,scream) */
   else
   {
      if (percentage > 95)
      {
         msg_format("%^s shrugs off the attack.", m_name);
      }
      else if (percentage > 75)
      {
         msg_format("%^s grunts with pain.", m_name);
      }
      else if (percentage > 50)
      {
         msg_format("%^s cries out in pain.", m_name);
         aggravate_monsters(&who, dam*2);
      }
      else if (percentage > 35)
      {
         msg_format("%^s screams in pain.", m_name);
         aggravate_monsters(&who, dam*5);
      }
      else if (percentage > 20)
      {
         msg_format("%^s screams in agony.", m_name);
         aggravate_monsters(&who, dam*10);
      }
      else if (percentage > 10)
      {
         msg_format("%^s writhes in agony.", m_name);
      }
      else
      {
         msg_format("%^s cries out feebly.", m_name);
      }
   }
}

/*
 * Learn about an "observed" resistance.
 */
void update_smart_learn(s16b m_idx, s16b what)
{

#ifdef DRS_SMART_OPTIONS

   monster_type *m_ptr = &mn_list[m_idx];

   monster_race *r_ptr = &r_info[m_ptr->r_idx];

   /* Not allowed to learn */
   if (!smart_learn) return;

   /* Too stupid to learn anything */
   if (r_ptr->flags2 & RF2_STUPID) return;

   /* Not intelligent, only learn sometimes */
   if (!(r_ptr->flags2 & RF2_SMART) && (rand_int(100) < 50)) return;


   /* Analyze the knowledge */
   switch (what)
   {
      case DRS_ACID:
         if (p_ptr->resist_acid) m_ptr->smart |= SM_RES_ACID;
         if (p_ptr->oppose_acid) m_ptr->smart |= SM_OPP_ACID;
         if (p_ptr->immune_acid) m_ptr->smart |= SM_IMM_ACID;
         break;

      case DRS_ELEC:
         if (p_ptr->resist_elec) m_ptr->smart |= SM_RES_ELEC;
         if (p_ptr->oppose_elec) m_ptr->smart |= SM_OPP_ELEC;
         if (p_ptr->immune_elec) m_ptr->smart |= SM_IMM_ELEC;
         break;

      case DRS_FIRE:
         if (p_ptr->resist_fire) m_ptr->smart |= SM_RES_FIRE;
         if (p_ptr->oppose_fire) m_ptr->smart |= SM_OPP_FIRE;
         if (p_ptr->immune_fire) m_ptr->smart |= SM_IMM_FIRE;
         break;

      case DRS_COLD:
         if (p_ptr->resist_cold) m_ptr->smart |= SM_RES_COLD;
         if (p_ptr->oppose_cold) m_ptr->smart |= SM_OPP_COLD;
         if (p_ptr->immune_cold) m_ptr->smart |= SM_IMM_COLD;
         break;

      case DRS_POIS:
         if (p_ptr->resist_pois) m_ptr->smart |= SM_RES_POIS;
         if (p_ptr->oppose_pois) m_ptr->smart |= SM_OPP_POIS;
         break;


      case DRS_NETH:
         if (p_ptr->resist_neth) m_ptr->smart |= SM_RES_NETH;
         break;

      case DRS_LITE:
         if (p_ptr->resist_lite) m_ptr->smart |= SM_RES_LITE;
         break;

      case DRS_DARK:
         if (p_ptr->resist_dark) m_ptr->smart |= SM_RES_DARK;
         break;

      case DRS_FEAR:
         if (p_ptr->resist_fear) m_ptr->smart |= SM_RES_FEAR;
         break;

      case DRS_CONF:
         if (p_ptr->resist_conf) m_ptr->smart |= SM_RES_CONF;
         break;

      case DRS_CHAOS:
         if (p_ptr->resist_chaos) m_ptr->smart |= SM_RES_CHAOS;
         break;

      case DRS_DISEN:
         if (p_ptr->resist_disen) m_ptr->smart |= SM_RES_DISEN;
         break;

      case DRS_BLIND:
         if (p_ptr->resist_blind) m_ptr->smart |= SM_RES_BLIND;
         break;

      case DRS_NEXUS:
         if (p_ptr->resist_nexus) m_ptr->smart |= SM_RES_NEXUS;
         break;

      case DRS_SOUND:
         if (p_ptr->resist_sound) m_ptr->smart |= SM_RES_SOUND;
         break;

      case DRS_SHARD:
         if (p_ptr->resist_shard) m_ptr->smart |= SM_RES_SHARD;
         break;

      case DRS_FREE:
         if (p_ptr->free_act) m_ptr->smart |= SM_IMM_FREE;
         break;

      case DRS_MANA:
         if (!p_ptr->msp) m_ptr->smart |= SM_IMM_MANA;
         break;
   }

#endif /* DRS_SMART_OPTIONS */

}


