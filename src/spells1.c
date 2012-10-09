/* File: spells1.c */

/* Purpose: Spell code (part 1) */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

/*
 * Helper function -- return a "nearby" race for polymorphing
 *
 * XXX XXX XXX This function may be very expensive...
 */
s16b poly_r_idx(s16b r_idx)
{
   monster_race *r_ptr = &r_info[r_idx];

   s16b i, r, lev1, lev2;

   /* Hack -- Uniques never polymorph */
   if (r_ptr->flags1 & RF1_UNIQUE) return (r_idx);

   /* Pick a (possibly new) non-unique race */
   for (i = 0; i < 1000; i++)
   {
      /* Allowable range of "levels" for resulting monster */
      lev1 = r_ptr->level - ((randint(20)/randint(9))+1);
      lev2 = r_ptr->level + ((randint(20)/randint(9))+1);

      /* Increase monster depth */
      monster_level = (p_ptr->mdepth+ r_ptr->level) / 2 + 5;

      /* Hack -- Pick a new race */
      r = get_mon_num(monster_level);

      /* Restore monster depth */
      monster_level = p_ptr->mdepth;

      /* Handle failure */
      if (!r) continue;

      /* Extract that monster */
      r_ptr = &r_info[r];

      /* Skip uniques */
      if (r_ptr->flags1 & RF1_UNIQUE) continue;

      /* Accept valid monsters */
      if ((r_ptr->level < lev1) && (r_ptr->level > lev2)) continue;

      /* Use that index */
      r_idx = r;

      /* Done */
      break;
   }

   /* Use the original */
   return (r_idx);
}


/*
 * Teleport a monster, normally up to "dis" grids away.
 *
 * Attempt to move the monster at least "dis/2" grids away.
 *
 * But allow variation to prevent infinite loops.
 */
void teleport_away(s16b m_idx, s16b dis)
{
   s16b                nx, ny, ox, oy, d, i, min;
   bool                look = TRUE;
   u32b                in_arena, in_town;
   monster_type        *m_ptr = &mn_list[m_idx];

   /* Paranoia */
   if (!m_ptr->r_idx) return;

   /* Save the old location */
   oy = m_ptr->fy;
   ox = m_ptr->fx;

   in_arena = dungeon.level[sublevel][oy][ox].fdat & CAVE_AREN;
   in_town = (ox < TOWN_WID) & (oy < TOWN_HGT);

   /* Minimum distance */
   min = dis / 2;

   /* Look until done */
   while (look)
   {
      /* Verify max distance */
/* jk - changed from 200 at ratio 3 to 67*RATIO */
      if (dis > (RATIO*67)) dis = (RATIO*67);

      /* Try several locations */
      for (i = 0; i < 500; i++)
      {
         /* Pick a (possibly illegal) location */
         while (1)
         {
            nx = rand_spread(ox, dis);
            ny = rand_spread(oy, dis);
            d = distance(ox, oy, nx, ny);
            if ((d >= min) && (d <= dis)) break;
         }

         /* Ignore illegal locations */
         if (!in_bounds(nx, ny)) continue;

/* jk - don't teleport into the arena */
         if ((dungeon.level[sublevel][ny][nx].fdat & CAVE_AREN) != in_arena) continue;

         if (in_town & ((nx>=TOWN_WID) | (ny >=TOWN_HGT))) continue;
         /* Require "empty" floor space */
         if (!empty_grid_bold(nx, ny)) continue;

         /* Hack -- no teleport onto glyph of warding */
         if (test_grid(nx, ny, DUNG_FLOOR, DUNG_FLOOR_GLYPH)) continue;

         /* No teleporting into vaults and such */
         if (dungeon.level[sublevel][ny][nx].fdat & CAVE_VAULT) continue;

         /* This grid looks good */
         look = FALSE;

         /* Stop looking */
         break;
      }
      /* if we have tried everything, just return */
      if ( (min==0) && (dis==(RATIO*67)) ) return;

      /* Increase the maximum distance */
      dis = dis * 2;

      /* Decrease the minimum distance */
      min = min / 2;
   }

   /* Update the new location */
   dungeon.level[sublevel][ny][nx].m_idx = m_idx;

   /* Update the old location */
   dungeon.level[sublevel][oy][ox].m_idx = 0;

   /* Move the monster */
   m_ptr->fx = nx;
   m_ptr->fy = ny;
   /* stay on this level, fz doesn't change */

   /* Update the monster (new location) */
   update_mon(m_idx, TRUE);

   /* Redraw the old grid */
   lite_spot(ox, oy);

   /* Redraw the new grid */
   lite_spot(nx, ny);
}

/*
 * Teleport a monster as close as possible to a given grid
 *
 * Attempt to move the monster at least "dis/2" grids away.
 *
 * But allow variation to prevent infinite loops.
 */
bool teleport_monster_to(s16b m_idx, s16b x, s16b y)
{
   s16b                nx, ny, ox, oy, i, dis;
   bool                look = TRUE;
   u32b                in_arena, in_town;
   monster_type        *m_ptr = &mn_list[m_idx];

   /* Paranoia */
   if (!m_ptr->r_idx) return (FALSE);

   /* Save the old location */
   oy = m_ptr->fy;
   ox = m_ptr->fx;

   in_arena = dungeon.level[sublevel][oy][ox].fdat & CAVE_AREN;
   in_town = (ox < TOWN_WID) & (oy < TOWN_HGT);

   dis = 1;
   nx = -1;
   ny = -1;

   /* Look until done */
   while (look)
   {
      /* Verify max distance */
/* jk - changed from 200 at ratio 3 to 67*RATIO */
      if (dis > 10)
      {
         dis = -1;
         break; /* we didn't succeed */
      }

      /* Try several locations */
      for (i = 0; i < 500; i++)
      {
         /* Pick a (possibly illegal) location */
         nx = rand_spread(x, dis);
         ny = rand_spread(y, dis);

         /* Ignore illegal locations */
         if (!in_bounds(nx, ny)) continue;

/* jk - don't teleport into the arena */
         if ((dungeon.level[sublevel][ny][nx].fdat & CAVE_AREN) != in_arena) continue;

         if (in_town & ((nx>=TOWN_WID) | (ny >=TOWN_HGT))) continue;
         /* Require "empty" floor space */
         if (!empty_grid_bold(nx, ny)) continue;

         /* Hack -- no teleport onto glyph of warding */
         if (test_grid(nx, ny, DUNG_FLOOR, DUNG_FLOOR_GLYPH)) continue;

         /* No teleporting into vaults and such */
         if (dungeon.level[sublevel][ny][nx].fdat & CAVE_VAULT) continue;

         /* This grid looks good */
         look = FALSE;

         /* Stop looking */
         break;
      }

      /* Increase the maximum distance */
      dis = dis * 2;
   }
   if (dis == -1)
   {
      return (FALSE);
   }

   /* Update the new location */
   dungeon.level[sublevel][ny][nx].m_idx = m_idx;

   /* Update the old location */
   dungeon.level[sublevel][oy][ox].m_idx = 0;

   /* Move the monster */
   m_ptr->fx = nx;
   m_ptr->fy = ny;
   /* stay on this level, fz doesn't change */

   /* Update the monster (new location) */
   update_mon(m_idx, TRUE);

   /* Redraw the old grid */
   lite_spot(ox, oy);

   /* Redraw the new grid */
   lite_spot(nx, ny);
   return (TRUE);
}


/*
 * Teleport the player to a location up to "dis" grids away.
 *
 * If no such spaces are readily available, the distance may increase.
 * Try very hard to move the player at least a quarter that distance.
 */
void teleport_player(s16b dis)
{
   s16b      d, i, min, ox, oy, x = py, y = px;

   bool     look = TRUE;

   u32b     in_arena;

   /* Minimum distance */
   min = dis / 2;

   in_arena = dungeon.level[sublevel][py][px].fdat & CAVE_AREN;
   if (in_maze(px, py))
   {
      msg_print("You hear a disappointing fizzle.");
      dis = 1;
      min = 1;
   }

   /* Look until done */
   while (look)
   {
      /* Verify max distance */
/* jk - changed from 200 at ratio 3 to 67*RATIO */
      if (dis > (RATIO*67)) dis = (RATIO*67);

      /* Try several locations */
      for (i = 0; i < 500; i++)
      {
         /* Pick a (possibly illegal) location */
         while (1)
         {
            x = rand_spread(px, dis);
            y = rand_spread(py, dis);
            d = distance(px, py, x, y);
            if ((d >= min) && (d <= dis)) break;
         }

         /* Ignore illegal locations */
         if (!in_bounds(x, y)) continue;

/* jk - d on't teleport into the arena */
         if ((dungeon.level[sublevel][y][x].fdat & CAVE_AREN) != in_arena) continue;

         /* Require "naked" floor space */
         if (!naked_grid_bold(x, y)) continue;

         /* No teleporting into vaults and such */
         if (dungeon.level[sublevel][y][x].fdat & CAVE_VAULT) continue;

         /* This grid looks good */
         look = FALSE;

         /* Stop looking */
         break;
      }

      /* if we have tried everything, just return */
      if ( (min==0) && (dis==(RATIO*67)) ) return;

      /* Increase the maximum distance */
      dis = dis * 2;

      /* Decrease the minimum distance */
      min = min / 2;
   }

   /* Save the old location */
   oy = py;
   ox = px;

   /* Move the player */
   py = y;
   px = x;

   /* Redraw the old spot */
   lite_spot(ox, oy);

   /* Redraw the new spot */
   lite_spot(px, py);

   /* Check for new panel (redraw map) */
   verify_panel();

   /* Update stuff */
   p_ptr->update |= (PU_VIEW | PU_FLOW);

   /* Update the monsters */
   p_ptr->update |= (PU_DISTANCE);

   /* Handle stuff XXX XXX XXX */
   handle_stuff();
}



/*
 * Teleport player to a grid near the given location
 *
 * This function is slightly obsessive about correctness.
 * This function allows teleporting into vaults (!)
 */
void teleport_player_to(s16b nx, s16b ny)
{
   s16b y, x, oy, ox, dis = 0, ctr = 0;
   s16b cnt = 0;

   /* Find a usable location */
   while (1)
   {
      cnt++;
      /* Pick a nearby legal location */
      while (1)
      {
         cnt++;
         y = rand_spread(ny, dis);
         x = rand_spread(nx, dis);
         if (in_bounds(x, y)) break;
         /* don't get stuck here, rather fail */
         if (cnt==30000) break;
      }
      /* don't get stuck here, rather fail */
      if (cnt==30000) break;

      /* Accept "naked" floor grids */
      if (naked_grid_bold(x, y)) break;

      /* Occasionally advance the distance */
      if (++ctr > (4 * dis * dis + 4 * dis + 1))
      {
         ctr = 0;
         dis++;
      }
   }

   /* don't get stuck here, rather fail */
   if (cnt == 30000) return;

   /* Save the old location */
   oy = py;
   ox = px;

   /* Move the player */
   py = y;
   px = x;

   /* Redraw the old spot */
   lite_spot(ox, oy);

   /* Redraw the new spot */
   lite_spot(px, py);

   /* Check for new panel (redraw map) */
   verify_panel();

   /* Update stuff */
   p_ptr->update |= (PU_VIEW | PU_FLOW);

   /* Update the monsters */
   p_ptr->update |= (PU_DISTANCE);

   /* Handle stuff XXX XXX XXX */
   handle_stuff();
}

/*
 * Teleport the player one level up or down (random when legal)
 */
void teleport_player_level(void)
{
   if (!p_ptr->mdepth)
   {
      if (in_maze(px, py))
      {
         msg_print("You suddenly feel very heavy, but the feeling passes.");
      }
      else
      {
         msg_print("You sink through the floor.");
         p_ptr->new_mdepth = p_ptr->mdepth + 1;
         p_ptr->new_sdepth = 0;
         new_level_flag = TRUE;
      }
   }
   else if (is_quest(p_ptr->mdepth) || (p_ptr->mdepth >= MAX_LEVEL-1) || (rand_int(100) < 50) )
   {
      if (in_maze(px, py))
      {
         msg_print("You suddenly feel very light, but the feeling passes.");
      }
      else
      {
         msg_print("You rise up through the ceiling.");
         p_ptr->new_mdepth=p_ptr->mdepth-1;
         p_ptr->new_sdepth = 0;
         new_level_flag = TRUE;
      }
   }
   else
   {
      if (in_maze(px, py))
      {
         msg_print("You suddenly feel very heavy, but the feeling passes.");
      }
      else
      {
         msg_print("You sink through the floor.");
         p_ptr->new_mdepth=p_ptr->mdepth+1;
         p_ptr->new_sdepth = 0;
         new_level_flag = TRUE;
      }
   }
}






#ifdef USE_COLOR

/*
 * Get a legal "multi-hued" color for drawing "spells"
 */
static byte mh_attr(void)
{
   switch (randint(9))
   {
      case 1: return (TERM_RED);
      case 2: return (TERM_GREEN);
      case 3: return (TERM_BLUE);
      case 4: return (TERM_YELLOW);
      case 5: return (TERM_ORANGE);
      case 6: return (TERM_VIOLET);
      case 7: return (TERM_L_RED);
      case 8: return (TERM_L_GREEN);
      case 9: return (TERM_L_BLUE);
   }

   return (TERM_WHITE);
}

#endif


/*
 * Return a color to use for the bolt/ball spells
 */
static byte spell_color(s16b type)
{

#ifdef USE_COLOR

   if (!use_color) return (TERM_WHITE);

   switch (type)
   {
      case GF_MISSILE:     return (mh_attr());
      case GF_ACID:        return (TERM_SLATE);
      case GF_ELEC:        return (TERM_BLUE);
      case GF_FIRE:        return (TERM_RED);
      case GF_COLD:        return (TERM_WHITE);
      case GF_POIS:        return (TERM_GREEN);
      case GF_HOLY_ORB:    return (TERM_L_DARK);
      case GF_MANA:        return (TERM_L_DARK);
      case GF_ARROW:       return (TERM_WHITE);
      case GF_WATER:       return (TERM_SLATE);
      case GF_NETHER:      return (TERM_L_GREEN);
      case GF_CHAOS:       return (mh_attr());
      case GF_DISENCHANT:  return (TERM_VIOLET);
      case GF_NEXUS:       return (TERM_L_RED);

      case GF_CONF_STRONG:
      case GF_CONFUSION:   return (TERM_L_BROWN);
      case GF_SOUND:       return (TERM_YELLOW);
      case GF_SHARDS:      return (TERM_BROWN);
      case GF_FORCE:       return (TERM_BROWN);
      case GF_INERTIA:     return (TERM_L_WHITE);
      case GF_GRAVITY:     return (TERM_L_WHITE);
      case GF_TIME:        return (TERM_L_BLUE);
      case GF_LITE_WEAK:   return (TERM_ORANGE);
      case GF_LITE:        return (TERM_ORANGE);
      case GF_DARK_WEAK:   return (TERM_L_DARK);
      case GF_DARK:        return (TERM_L_DARK);
      case GF_PLASMA:      return (TERM_RED);
      case GF_METEOR:      return (TERM_RED);
      case GF_ICE:         return (TERM_WHITE);
   }

#endif

   /* Standard "color" */
   return (TERM_WHITE);
}

/*
 * Decreases players hit points and sets death flag if necessary
 *
 * XXX XXX XXX Invulnerability needs to be changed into a "shield"
 *
 * XXX XXX XXX Hack -- this function allows the user to save (or quit)
 * the game when he dies, since the "You die." message is shown before
 * setting the player to "dead".
 */
void take_hit(s16b damage, cptr hit_from)
{
   s16b old_chp = p_ptr->chp;

   s16b warning = (p_ptr->mhp * hitpoint_warn / 10);

   /* Paranoia */
   if (death) return;

   /* Disturb */
   disturb(1, 0);

   /* Mega-Hack -- Apply "invulnerability" */
   if (p_ptr->invuln)
   {
      if (p_ptr->invuln >= damage)
      {
         p_ptr->invuln -= damage;
         return;
      }
      msg_print("Uh-oh. Your magical shield just crumbled.");
      damage -= p_ptr->invuln;
   }

   /* Hurt the player */
   p_ptr->chp -= damage;

   /* Display the hitpoints */
   p_ptr->redraw1 |= (PR1_HP);

   if (cheat_hitpoints && (p_ptr->chp < 0))
   {
      msg_format("You miserable cheater - you live on with %d HP.", p_ptr->chp);
      p_ptr->chp = p_ptr->mhp;
      /* have you ever watched a character being killed by a master mystic */
      /* only he wasn't killed..... */
      do_cmd_wiz_cure_all();
   }

   /* Dead player */
   if (p_ptr->chp < 0)
   {
      /* Sound */
      sound(SOUND_DEATH);

      /* Hack -- Note death */
      msg_print("You die.");
      msg_print(NULL);

      /* Note cause of death */
      (void)strcpy(died_from, hit_from);

      /* No longer a winner */
      total_winner = FALSE;

      /* Note death */
      death = TRUE;

      /* Dead */
      return;
   }

   /* Hack -- hitpoint warning */
   if (warning && (p_ptr->chp <= warning))
   {
      /* Hack -- bell on first notice */
      if (alert_hitpoint && (old_chp > warning))
         bell("Low hitpoint warning!");

      /* Message */
      msg_print("*** LOW HITPOINT WARNING! ***");
      msg_print(NULL);
   }
}

/*
 * Note that amulets, rods, and high-level spell books are immune
 * to "inventory damage" of any kind.  Also sling ammo and shovels.
 */


/*
 * Does a given class of objects (usually) hate acid?
 * Note that acid can either melt or corrode something.
 */
static bool hates_acid(object_type *i_ptr)
{
   /* Analyze the type */
   switch (i_ptr->tval)
   {
      /* Wearable items */
      case TV_ARROW:
      case TV_BOLT:
      case TV_BOW:
      case TV_SWORD:
      case TV_HAFTED:
      case TV_POLEARM:
      case TV_HELM:
      case TV_CROWN:
      case TV_SHIELD:
      case TV_BOOTS:
      case TV_GLOVES:
      case TV_CLOAK:
      case TV_SOFT_ARMOR:
      case TV_HARD_ARMOR:
      case TV_DRAG_ARMOR:

      /* Staffs/Scrolls are wood/paper */
      case TV_STAFF:
      case TV_SCROLL:

      /* Ouch */
      case TV_CHEST:

      /* Junk is useless */
      case TV_SKELETON:
      case TV_BOTTLE:
      case TV_JUNK:

         return (TRUE);

      /* books not often, but they melt */
      case TV_BOOK:
         if (randint(5)==1) return (TRUE);

      case TV_CORPSE:
      {
      /* if the corpse in any way gives or takes acid, it isn't affected */
         return ~((r_info[i_ptr->sval].corpse_gives_alw & CORPSE_GIVES_ACID) ||
               (r_info[i_ptr->sval].corpse_gives_smt & CORPSE_GIVES_ACID) ||
               (r_info[i_ptr->sval].corpse_takes_alw & CORPSE_TAKES_ACID) ||
               (r_info[i_ptr->sval].corpse_takes_smt & CORPSE_TAKES_ACID));
      }
   }

   return (FALSE);
}


/*
 * Does a given object (usually) hate electricity?
 */
static bool hates_elec(object_type *i_ptr)
{
   switch (i_ptr->tval)
   {
      case TV_RING:

      case TV_WAND:

         return (TRUE);
   }

   return (FALSE);
}


/*
 * Does a given object (usually) hate fire?
 * Hafted/Polearm weapons have wooden shafts.
 * Arrows/Bows are mostly wooden.
 */
static bool hates_fire(object_type *i_ptr)
{
   /* Analyze the type */
   switch (i_ptr->tval)
   {
      /* Wearable */
      case TV_LITE:
      case TV_ARROW:
      case TV_BOW:
      case TV_HAFTED:
      case TV_POLEARM:
      case TV_BOOTS:
      case TV_GLOVES:
      case TV_CLOAK:
      case TV_SOFT_ARMOR:

      /* Chests */
      case TV_CHEST:

      /* Staffs/Scrolls burn */
      case TV_STAFF:
      case TV_SCROLL:

         return (TRUE);

      /* books not often, but they burn */
      case TV_BOOK:
         if (randint(5)==1) return (TRUE);

      case TV_CORPSE:
      {
      /* if the corpse in any way gives or takes acid, it isn't affected */
         return ~((r_info[i_ptr->sval].corpse_gives_alw & CORPSE_GIVES_FIRE) ||
               (r_info[i_ptr->sval].corpse_gives_smt & CORPSE_GIVES_FIRE) ||
               (r_info[i_ptr->sval].corpse_takes_alw & CORPSE_TAKES_FIRE) ||
               (r_info[i_ptr->sval].corpse_takes_smt & CORPSE_TAKES_FIRE));
      }
   }

   return (FALSE);
}


/*
 * Does a given object (usually) hate cold?
 */
static bool hates_cold(object_type *i_ptr)
{
   switch (i_ptr->tval)
   {
      case TV_POTION:

/* flask removed, since oil doesn't freeze! */
      case TV_BOTTLE:

         return (TRUE);
   }

   return (FALSE);
}

/*
 * Melt something
 */
bool set_acid_destroy(object_type *i_ptr)
{
   u64b f1, f2, f3;
   if (!hates_acid(i_ptr)) return (FALSE);
   object_flags(i_ptr, &f1, &f2, &f3);
   if (f3 & TR3_IGNORE_ACID) return (FALSE);
   return (TRUE);
}

/*
 * Electrical damage
 */
bool set_elec_destroy(object_type *i_ptr)
{
   u64b f1, f2, f3;
   if (!hates_elec(i_ptr)) return (FALSE);
   object_flags(i_ptr, &f1, &f2, &f3);
   if (f3 & TR3_IGNORE_ELEC) return (FALSE);
   return (TRUE);
}

/*
 * Burn something
 */
bool set_fire_destroy(object_type *i_ptr)
{
   u64b f1, f2, f3;
   if (!hates_fire(i_ptr)) return (FALSE);
   object_flags(i_ptr, &f1, &f2, &f3);
   if (f3 & TR3_IGNORE_FIRE) return (FALSE);
   return (TRUE);
}

/*
 * Freeze things
 */
bool set_cold_destroy(object_type *i_ptr)
{
   u64b f1, f2, f3;
   if (!hates_cold(i_ptr)) return (FALSE);
   object_flags(i_ptr, &f1, &f2, &f3);
   if (f3 & TR3_IGNORE_COLD) return (FALSE);
   return (TRUE);
}

static s16b handle_book_fire_damage(s16b item, object_type *i_ptr)
{
   s16b count, amt;
   s16b index[MAX_SPELLS_PER_ITEM], maxnum, num, start, i;
   char        i_name[80];

   object_desc(i_name, i_ptr, FALSE, 3);
   maxnum = 2;
   amt = 0;
   if (i_ptr->sval == SV_BOOK_AVG) maxnum = 3;
   if (i_ptr->sval == SV_BOOK_LARGE) maxnum = 7;

   for (i=0, count=0; i < (s16b)s_number; i++)
   {
      if (has_spell(i_ptr, i)) index[count++]=i;
   }
   /* how many to destroy */
   num=rand_int(maxnum);

   /* trying to destroy more spells than the book holds? tough luck! */
   if (num >= count)
   {
      msg_format("Your %s (%c) is burned and destroyed!",
              i_name, index_to_label(item));
      /* remove spells */
      for (i=0; i < count; i++)
      {
         clr_spell(i_ptr, index[i]);
      }
      /* Destroy "amt" items */
      item_increase(item,-1, px, py);
      item_optimize(i,px,py);
   }
   else
   {
      /* where to start in the book */
      start=rand_int(count);
      while (num > 0)
      {
         spell_type *s_ptr = &s_info[index[start]];

         msg_format("The page with your spell of %s burns away!", s_name + s_ptr->name);
         clr_spell(i_ptr, index[start]);
         start++;
         /* wrap around if necessary */
         if (start==count) start=0;
         num--;
      }
   }
   return (count); /* each removed spell counts as one item */
}
        

/*
 * Destroys a type of item on a given percent chance
 * Note that missiles are no longer necessarily all destroyed
 * Destruction taken from "melee.c" code for "stealing".
 * Returns number of items destroyed.
 */
static s16b inven_damage(inven_func typ, s16b perc)
{
   s16b        i, j, k, amt;
   object_type *i_ptr;
   char        i_name[80];

   /* Count the casualties */
   k = 0;

   /* Scan through the slots backwards */
   for (i = 0; i < INVEN_PACK; i++)
   {
      /* Get the item in that slot */
      i_ptr = &inventory[i];

      /* Hack -- for now, skip artifacts */
      if (artifact_p(i_ptr)) continue;

      /* Give this item slot a shot at death */
      if ((*typ)(i_ptr))
      {
         /* Count the casualties */
         for (amt = j = 0; j < i_ptr->number; ++j)
         {
             if (rand_int(100) < perc) amt++;
         }

         /* Some casualities */
         if (amt)
         {
            /* Get a description */
            object_desc(i_name, i_ptr, FALSE, 3);

            if (i_ptr->tval == TV_BOOK)
            {
               amt = handle_book_fire_damage(i, i_ptr);
            }
            else
            {
               /* Message */
               msg_format("%sour %s (%c) %s destroyed!",
                          ((i_ptr->number > 1) ?
                           ((amt == i_ptr->number) ? "All of y" :
                           (amt > 1 ? "Some of y" : "One of y")) : "Y"),
                          i_name, index_to_label(i),
                          ((amt > 1) ? "were" : "was"));

               /* Destroy "amt" items */
               item_increase(i,-amt, px, py);
               item_optimize(i,px,py);
            }

            /* Count the casualties */
            k += amt;
         }
      }
   }

   /* Return the casualty count */
   return (k);
}

/*
 * Acid has hit the player, attempt to affect some armor.
 *
 * Note that the "base armor" of an object never changes.
 *
 * If any armor is damaged (or resists), the player takes less damage.
 */
static s16b minus_ac(void)
{
   object_type         *i_ptr = NULL;

   u64b                f1, f2, f3;

   char                i_name[80];


   /* Pick a (possibly empty) inventory slot */
   switch (randint(6))
   {
      case 1: i_ptr = &inventory[INVEN_BODY]; break;
      case 2: i_ptr = &inventory[INVEN_ARM]; break;
      case 3: i_ptr = &inventory[INVEN_OUTER]; break;
      case 4: i_ptr = &inventory[INVEN_HANDS]; break;
      case 5: i_ptr = &inventory[INVEN_HEAD]; break;
      case 6: i_ptr = &inventory[INVEN_FEET]; break;
   }

   /* Nothing to damage */
   if (!i_ptr->k_idx) return (FALSE);

   /* No damage left to be done */
   if (i_ptr->ac + i_ptr->to_a <= 0) return (FALSE);

   /* Describe */
   object_desc(i_name, i_ptr, FALSE, 0);

   /* Extract the flags */
   object_flags(i_ptr, &f1, &f2, &f3);

   /* Object resists */
   if (f3 & TR3_IGNORE_ACID)
   {
      msg_format("Your %s is unaffected!", i_name);

      return (TRUE);
   }

   /* Message */
   msg_format("Your %s is damaged!", i_name);

   /* Damage the item */
   i_ptr->to_a--;

   /* Window stuff */
   p_ptr->window |= (PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

   /* Calculate bonuses */
   p_ptr->update |= (PU_BONUS);

   /* Item was damaged */
   return (TRUE);
}


/*
 * Hurt the player with Acid
 */
void acid_dam(s16b dam, cptr kb_str)
{
   s16b inv = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;

   /* Total Immunity */
   if (p_ptr->immune_acid || (dam <= 0)) return;

   /* Resist the damage */
   if (p_ptr->resist_acid) dam = (dam + 2) / 3;
   if (p_ptr->oppose_acid) dam = (dam + 2) / 3;

   /* If any armor gets hit, defend the player */
   if (minus_ac()) dam = (dam + 1) / 2;

   /* Take damage */
   take_hit(dam, kb_str);
   if (wizard)
   {
      msg_format("The acid does %d damage.", dam);
   }

   /* Inventory damage */
   inven_damage(set_acid_destroy, inv);
}

/*
 * Hurt the player with electricity
 */
void elec_dam(s16b dam, cptr kb_str)
{
   s16b inv = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;

   /* Total immunity */
   if (p_ptr->immune_elec || (dam <= 0)) return;

   /* Resist the damage */
   if (p_ptr->oppose_elec) dam = (dam + 2) / 3;
   if (p_ptr->resist_elec) dam = (dam + 2) / 3;

   /* Take damage */
   take_hit(dam, kb_str);
   if (wizard)
   {
      msg_format("The electricity does %d damage.", dam);
   }

   /* Inventory damage */
   inven_damage(set_elec_destroy, inv);
}

/*
 * Hurt the player with Fire
 */
void fire_dam(s16b dam, cptr kb_str)
{
   s16b inv = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;

   /* Totally immune */
   if (p_ptr->immune_fire || (dam <= 0)) return;

   /* Resist the damage */
   if (p_ptr->resist_fire) dam = (dam + 2) / 3;
   if (p_ptr->oppose_fire) dam = (dam + 2) / 3;

   /* Take damage */
   take_hit(dam, kb_str);
   if (wizard)
   {
      msg_format("The fire does %d damage.", dam);
   }

   /* Inventory damage */
   inven_damage(set_fire_destroy, inv);
}

/*
 * Hurt the player with Poison
 */
bool poison_dam(s16b dam)
{
   s16b chance = (dam < 30) ? 2 : (dam < 60) ? 4 : 7;
   s16b mychance;
   bool result = FALSE;

   s16b i;
   object_type *j_ptr;

   for (i=0;i<INVEN_PACK;i++)
   {
      if (!inventory[i].k_idx) continue;

      /* only one item per turn if damage is low */
      if (result && (randint(chance)<3)) continue;

      j_ptr = &inventory[i];
      mychance = chance * j_ptr->number;
      if ((j_ptr->tval==TV_FOOD) && (j_ptr->sval!=SV_FOOD_CURE_POISON))
      {
         if (rand_int(100)<mychance)
         {
            object_type tmp_obj;
            s16b num = randint(j_ptr->number/(9-chance));
            if (j_ptr->number == 1) num = 1;
            msg_print("The poison seeps into your backpack.");
            invcopy(&tmp_obj, lookup_kind(TV_FOOD, rand_int(SV_FOOD_NASTY_FOOD)));
            tmp_obj.number = num;
            item_increase(i, -num, px, py);
            item_optimize(i, px, py);
            tmp_obj.log.where = OBJ_FOUND_POISON;
            tmp_obj.log.whose = 0;
            tmp_obj.log.mlevel = p_ptr->mdepth;
            tmp_obj.log.slevel = p_ptr->sdepth;
            if (inven_carry(&tmp_obj,1)==-1)
            {
               msg_print("You feel your backpack overflow.");
               (void)drop_near(&tmp_obj,0,px,py, drop_how(&tmp_obj), FALSE, FALSE);
            }
            result = TRUE;
         }
      }
      else if ((j_ptr->tval==TV_POTION) && (j_ptr->sval!=SV_POTION_CURE_POISON))
      {
         if (rand_int(100)<mychance)
         {
            object_type tmp_obj;
            s16b num = randint(j_ptr->number/(9-chance));
            if (j_ptr->number == 1) num = 1;
            msg_print("The poison seeps into your backpack.");
            invcopy(&tmp_obj, lookup_kind(TV_POTION, SV_POTION_POISON));
            tmp_obj.number = num;

            item_increase(i, -num, px, py);
            item_optimize(i, px, py);
            tmp_obj.log.where = OBJ_FOUND_POISON;
            tmp_obj.log.whose = 0;
            tmp_obj.log.mlevel = p_ptr->mdepth;
            tmp_obj.log.slevel = p_ptr->sdepth;
            if (inven_carry(&tmp_obj,1)==-1)
            {
               msg_print("You feel your backpack overflow.");
               (void)drop_near(&tmp_obj,0,px,py, drop_how(&tmp_obj), FALSE, FALSE);
            }
            result = TRUE;
         }
      }
   }
   return(result);
}

/*
 * Hurt the player with Cold
 */
void cold_dam(s16b dam, cptr kb_str)
{
   s16b inv = (dam < 30) ? 1 : (dam < 60) ? 2 : 3;

   /* let's say dam 0-500, total_weight 0-500? */
   /* don't get sliding from potions of cold, when kb_str = cold skin */
   if (rand_int(dam/10)==rand_int(1000-p_ptr->total_weight) &&
      !cmp_strngs(kb_str,"cold skin"))
   {
      /* only 10% chance of sliding if immune to cold */
      if (!p_ptr->immune_cold || (randint(10)==1))
      {
         if (set_sliding(20+rand_int(dam/20)))
         {
          msg_print("Your feet suddenly have a life of their own!");
          p_ptr->sliding_now=TRUE;
         }
         else
         {
            msg_print("The ground seems slippery for a moment.");
         }
      }
   }
   /* Total immunity */
   if (p_ptr->immune_cold || (dam <= 0)) return;

   /* Resist the damage */
   if (p_ptr->resist_cold) dam = (dam + 2) / 3;
   if (p_ptr->oppose_cold) dam = (dam + 2) / 3;

   /* Take damage */
   take_hit(dam, kb_str);
   if (wizard)
   {
      msg_format("The cold does %d damage.", dam);
   }

   /* Inventory damage */
   inven_damage(set_cold_destroy, inv);
}

/*
 * Increases a stat by one randomized level        -RAK-
 *
 * Note that this function (used by stat potions) now restores
 * the stat BEFORE increasing it.
 */
bool inc_stat(s16b stat, bool only_temporary)
{
   s16b value, gain;

   /* Then augment the current/max stat */
   value = p_ptr->stat_cur[stat];

   /* Cannot go above 18/100 */
   if (value < 18+100)
   {
      /* Gain one (sometimes two) points */
      if (value < 18)
      {
         gain = ((rand_int(100) < 75) ? 1 : 2);
         value += gain;
      }

      /* Gain 1/6 to 1/3 of distance to 18/100 */
      else if (value < 18+98)
      {
         /* Approximate gain value */
         gain = (((18+100) - value) / 2 + 3) / 2;

         /* Paranoia */
         if (gain < 1) gain = 1;

         /* Apply the bonus */
         value += randint(gain) + gain / 2;

         /* Maximal value */
         if (value > 18+99) value = 18 + 99;
      }

      /* Gain one point at a time */
      else
      {
         value++;
      }
      /* if we have any temporary losses, repair them too */
      if (p_ptr->stat_los[stat]>0)
      {
         if (only_temporary)
         {
            value = p_ptr->stat_cur[stat] + p_ptr->stat_los[stat];
         }
         else
         {
            value += p_ptr->stat_los[stat];
         }
         p_ptr->stat_los[stat]=0; /* temporary loss now 0 */
      }

      /* Save the new value */
      p_ptr->stat_cur[stat] = value;

      /* Bring up the maximum too */
      if (value > p_ptr->stat_max[stat])
      {
         p_ptr->stat_max[stat] = value;
      }

      /* Recalculate bonuses */
      p_ptr->update |= (PU_BONUS);

      /* Success */
      return (TRUE);
   }

   /* Nothing to gain */
   return (FALSE);
}



/*
 * Decreases a stat by an amount indended to vary from 0 to 100 percent.
 *
 * Amount could be a little higher in extreme cases to mangle very high
 * stats from massive assaults.  -CWS
 *
 * Note that "permanent" means that the *given* amount is permanent,
 * not that the new value becomes permanent.  This may not work exactly
 * as expected, due to "weirdness" in the algorithm, but in general,
 * if your stat is already drained, the "max" value will not drop all
 * the way down to the "cur" value.
 */
bool dec_stat(s16b stat, s16b amount, s16b mode)
{
   s16b cur, max, loss = 0, same, res = FALSE;

   /* Acquire current value */
   cur = p_ptr->stat_cur[stat];
   max = p_ptr->stat_max[stat];

   /* Note when the values are identical */
   same = (cur == max);

   /* Damage "current" value */
   if (cur > 3)
   {
      /* Handle "low" values */
      if (cur <= 18)
      {
         loss=1;
         if (amount > 90) loss++;
         if (amount > 50) loss++;
         if (amount > 20) loss++;
         cur -= loss;
      }

      /* Handle "high" values */
      else
      {
         /* Hack -- Decrement by a random amount between one-quarter */
         /* and one-half of the stat bonus times the percentage, with a */
         /* minimum damage of half the percentage. -CWS */
         loss = (((cur-18) / 2 + 1) / 2 + 1);

         /* Paranoia */
         if (loss < 1) loss = 1;

         /* Randomize the loss */
         loss = ((randint(loss) + loss) * amount) / 100;

         /* Maximal loss */
         if (loss < amount/2) loss = amount/2;

         /* Lose some points */
         cur -= loss;

         /* Hack -- Only reduce stat to 17 sometimes */
         if (cur < 18) cur = (amount <= 20) ? 18 : 17;
      }

      /* Prevent illegal values */
      if (cur < 3) cur = 3;

      /* Something happened */
      if (cur != p_ptr->stat_cur[stat]) res = TRUE;
   }

   /* Damage "max" value */
   if ((mode==STAT_DEC_PERMANENT) && (max > 3))
   {
      /* Handle "low" values */
      if (max <= 18)
      {
         if (amount > 90) max--;
         if (amount > 50) max--;
         if (amount > 20) max--;
         max--;
      }

      /* Handle "high" values */
      else
      {
         /* Hack -- Decrement by a random amount between one-quarter */
         /* and one-half of the stat bonus times the percentage, with a */
         /* minimum damage of half the percentage. -CWS */
         loss = (((max-18) / 2 + 1) / 2 + 1);
         loss = ((randint(loss) + loss) * amount) / 100;
         if (loss < amount/2) loss = amount/2;

         /* Lose some points */
         max = max - loss;

         /* Hack -- Only reduce stat to 17 sometimes */
         if (max < 18) max = (amount <= 20) ? 18 : 17;
      }

      /* Hack -- keep it clean */
      if (same || (max < cur)) max = cur;

      /* Something happened */
      if (max != p_ptr->stat_max[stat]) res = TRUE;
   }

   /* Apply changes */
   if (res)
   {
      /* Actually set the stat to its new value. */
      p_ptr->stat_cur[stat] = cur;
      p_ptr->stat_max[stat] = max;

      if (mode==STAT_DEC_TEMPORARY)
      {
         u16b dectime;

         /* a little crude, perhaps */
         dectime = rand_int(p_ptr->mdepth*50) + 50;
         /* prevent overflow, stat_cnt = u16b */
         /* or add another temporary drain... */
         if ( ((p_ptr->stat_cnt[stat]+dectime)<p_ptr->stat_cnt[stat]) ||
              (p_ptr->stat_los[stat]>0) )

         {
            p_ptr->stat_cnt[stat] += dectime;
            p_ptr->stat_los[stat] += loss;
         }
         else
         {
            p_ptr->stat_cnt[stat] = dectime;
            p_ptr->stat_los[stat] = loss;
         }
      }
      /* Recalculate bonuses */
      p_ptr->update |= (PU_BONUS);

   }

   /* Done */
   return (res);
}


/*
 * Restore a stat.   Return TRUE only if this actually makes a difference.
 */
bool res_stat(s16b stat)
{
   /* Restore if needed */
   if (p_ptr->stat_cur[stat] != p_ptr->stat_max[stat])
   {
      /* Restore */
      p_ptr->stat_cur[stat] = p_ptr->stat_max[stat];
      p_ptr->stat_cnt[stat] = 0;

      /* Recalculate bonuses */
      p_ptr->update |= (PU_BONUS);

      /* Success */
      return (TRUE);
   }

   /* Nothing to restore */
   return (FALSE);
}




/*
 * Apply disenchantment to the player's stuff
 *
 * XXX XXX XXX This function is also called from the "melee" code
 *
 * The "mode" is currently unused.
 *
 * Return "TRUE" if the player notices anything
 */
bool apply_disenchant(s16b mode)
{
   s16b            t = 0;

   object_type       *i_ptr;

   char           i_name[80];


   /* Unused */
   mode = mode;


   /* Pick a random slot */
   switch (randint(8))
   {
       case 1: t = INVEN_WIELD; break;
       case 2: t = INVEN_BOW; break;
       case 3: t = INVEN_BODY; break;
       case 4: t = INVEN_OUTER; break;
       case 5: t = INVEN_ARM; break;
       case 6: t = INVEN_HEAD; break;
       case 7: t = INVEN_HANDS; break;
       case 8: t = INVEN_FEET; break;
   }

   /* Get the item */
   i_ptr = &inventory[t];

   /* No item, nothing happens */
   if (!i_ptr->k_idx) return (FALSE);

   /* Nothing to disenchant */
   if ((i_ptr->to_h <= 0) && (i_ptr->to_d <= 0) && (i_ptr->to_a <= 0))
   {
      /* Nothing to notice */
      return (FALSE);
   }

   /* Describe the object */
   object_desc(i_name, i_ptr, FALSE, 0);

   /* Artifacts have 60% chance to resist */
   if (artifact_p(i_ptr) && (rand_int(100) < 60))
   {
      /* Message */
      msg_format("Your %s (%c) resist%s disenchantment!",
               i_name, index_to_label(t),
               ((i_ptr->number != 1) ? "" : "s"));

      /* Notice */
      return (TRUE);
   }

   /* Disenchant tohit */
   if (i_ptr->to_h > 0) i_ptr->to_h--;
   if ((i_ptr->to_h > 5) && (rand_int(100) < 20)) i_ptr->to_h--;

   /* Disenchant todam */
   if (i_ptr->to_d > 0) i_ptr->to_d--;
   if ((i_ptr->to_d > 5) && (rand_int(100) < 20)) i_ptr->to_d--;

   /* Disenchant toac */
   if (i_ptr->to_a > 0) i_ptr->to_a--;
   if ((i_ptr->to_a > 5) && (rand_int(100) < 20)) i_ptr->to_a--;

   /* Message */
   msg_format("Your %s (%c) %s disenchanted!",
            i_name, index_to_label(t),
            ((i_ptr->number != 1) ? "were" : "was"));

   /* Window stuff */
   p_ptr->window |= (PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

   /* Recalculate bonuses */
   p_ptr->update |= (PU_BONUS);

   /* Notice */
   return (TRUE);
}


/*
 * Apply Nexus
 * if m_ptr is NULL, we cannot teleport to, obviously.
 * teleporting to a trap would be useless, we are already there.
 */
static void apply_nexus(monster_type *m_ptr)
{
   s16b max1, cur1, max2, cur2, ii, jj;

   switch (randint(7))
   {
      case 1: case 2:

         if ( m_ptr != NULL )
         {
            teleport_player_to(m_ptr->fx, m_ptr->fy);
            break;
         }
         /* else fall through */

      case 3: case 4: case 5:

/* jk - changed from 200 at ratio 3 to 67*RATIO */
         teleport_player(67*RATIO);
         break;

      case 6:

         if (rand_int(100) < p_ptr->skill_sav)
         {
            msg_print("You resist the effects!");
            break;
         }

         /* Teleport Level */
         teleport_player_level();
         break;

      case 7:

         if (rand_int(100) < p_ptr->skill_sav)
         {
            msg_print("You resist the effects!");
            break;
         }

         msg_print("Your body starts to scramble...");

         /* Pick a pair of stats */
         ii = rand_int(6);
         for (jj = ii; jj == ii; jj = rand_int(6)) ;

         max1 = p_ptr->stat_max[ii];
         cur1 = p_ptr->stat_cur[ii];
         max2 = p_ptr->stat_max[jj];
         cur2 = p_ptr->stat_cur[jj];

         p_ptr->stat_max[ii] = max2;
         p_ptr->stat_cur[ii] = cur2;
         p_ptr->stat_max[jj] = max1;
         p_ptr->stat_cur[jj] = cur1;

         p_ptr->update |= (PU_BONUS);

         break;
   }
}

/*
 * remove all traps at x,y
 * return TRUE if the player noticed it
 */
static bool remove_traps(s16b x, s16b y, cptr message)
{
   cave_cell_type   *c_ptr = &dungeon.level[sublevel][y][x];

   /* Destroy invisible traps */
   if (c_ptr->t_idx != 0)
   {
      s16b t_idx = c_ptr->t_idx;
      t_list[t_idx].inuse = FALSE; /* trap no longer in use */
      t_list[t_idx].tx = 0;        /* so? I'm paranoid!     */
      t_list[t_idx].ty = 0;
      c_ptr->t_idx = 0;

      /* Hack -- special message */
      if (player_can_see_bold(x,y))
      {
         msg_print(message);
         return(TRUE);
      }
   }
   return (FALSE);
}

/*
 * Hack -- track "affected" monsters
 */
static s16b project_m_n;
static s16b project_m_x;
static s16b project_m_y;

/*
 * We are called from "project()" to "damage" terrain features
 *
 * We are called both for "beam" effects and "ball" effects.
 *
 * The "r" parameter is the "distance from ground zero".
 *
 * Note that we determine if the player can "see" anything that happens
 * by taking into account: blindness, line-of-sight, and illumination.
 *
 * We return "TRUE" if the effect of the projection is "obvious".
 *
 * XXX XXX XXX We also "see" grids which are "memorized", probably a hack
 *
 * XXX XXX XXX Perhaps we should affect doors?
 */
static bool project_f(project_who_type *who, s16b r, s16b x, s16b y, s16b dam, s16b typ)
{
   cave_cell_type   *c_ptr = &dungeon.level[sublevel][y][x];

   bool     obvious = FALSE;

   s16b      div;

   /* Extract radius */
   div = r + 1;

   /* Decrease damage */
   dam = dam / div;

   /* XXX XXX */
   who = who ? who : 0;

   /* Analyze the type */
   switch (typ)
   {
      /* Ignore most effects */
      case GF_ELEC:
      case GF_COLD:
      case GF_METEOR:
      case GF_ICE:
      case GF_SHARDS:
      case GF_FORCE:
      case GF_SOUND:
      case GF_MANA:
      case GF_HOLY_ORB:
         break;

      case GF_PLASMA:
      case GF_FIRE:
      /* if the fire is really hot, make lava. */
      /* if it is less hot, make lava sometimes */
         if ((dam>500) || (randint(10-(dam/50))==1))
         {
            if (is_floor(x, y) && (!(dungeon.level[sublevel][y][x].fdat & CAVE_AREN)))
            {
               (void)set_grid_type(x, y, DUNG_LAVA,
                             DUNG_LAVA_NORMAL, GRID_ADD, 0);
               if (player_has_los_bold(x,y) && (dungeon.level[sublevel][y][x].fdat & CAVE_VIEW) )
               {
                  msg_print("You see the floor melt.");
                  /* Notice */
                  note_spot(x, y);

                  /* Redraw */
                  lite_spot(x, y);
                  p_ptr->update |= PU_VIEW;
               }
            }
         }
         break;

      /* Destroy Traps (and Locks) */
      case GF_KILL_TRAP:

         /* Destroy invisible traps */
         if (test_grid_ptr(c_ptr, DUNG_FLOOR, DUNG_FLOOR_TRAP))
         {
            obvious = remove_traps(x, y, "There is a bright flash of light!");

            /* Destroy the trap */
            (void)set_grid_type(x, y, DUNG_FLOOR,
                           DUNG_FLOOR_NORMAL, GRID_KEEP, 0);

            note_spot(x, y);
            lite_spot(x, y);
            p_ptr->update |= PU_VIEW;
         }

         /* Destroy visible traps */
         if (c_ptr->mtyp == DUNG_TRAP)
         {
            obvious = remove_traps(x, y, "There is a bright flash of light!");

            /* Destroy the trap */
            (void)set_grid_type(x, y, DUNG_FLOOR,
                           DUNG_FLOOR_NORMAL, GRID_KEEP, 0);

            note_spot(x, y);
            lite_spot(x, y);
            p_ptr->update |= PU_VIEW;
         }

         /* Jammed / Locked doors are found and untrapped */
         else if (c_ptr->mtyp==DUNG_DOOR)
         {
            obvious = remove_traps(x, y, "Click!");

            note_spot(x, y);
            lite_spot(x, y);
            p_ptr->update |= PU_VIEW;
         }

         break;

      /* Destroy Doors (and traps) */
      case GF_KILL_DOOR:

         /* Destroy all visible traps and open doors */
         if ( (c_ptr->mtyp == DUNG_TRAP) ||
              ( (c_ptr->mtyp == DUNG_DOOR) &&
                !(test_grid_ptr(c_ptr,DUNG_DOOR,DUNG_DOOR_SECRET)) ) )
         {
            obvious = remove_traps(x, y, "Click!");
            /* Destroy the feature */
            (void)set_grid_type(x, y, DUNG_FLOOR,
                                DUNG_FLOOR_NORMAL, GRID_KEEP, 0);

            /* Forget the wall */
            c_ptr->fdat &= ~CAVE_MARK;

            /* Notice */
            note_spot(x, y);

            /* Redraw */
            lite_spot(x, y);
            p_ptr->update |= PU_VIEW;
         }
         break;

      /* Destroy walls (and doors) */
/* jk - acid should do something */
      case GF_ACID:
      case GF_KILL_WALL:

         /* Non-walls (etc) */
         if (!is_wall(x, y) && (c_ptr->mtyp != DUNG_DOOR)) break;

         /* Permanent walls */
         if (c_ptr->mtyp == DUNG_PERWALL) break;

         /* Granite */
         switch (wall_art(c_ptr->styp))
         {
            case DUNG_WALL_ART_GRANITE:

              if ((typ==GF_ACID) && (rand_int(10)!=1)) break;
              /* Message */
              if (c_ptr->fdat & CAVE_MARK)
              {
                 if (typ==GF_ACID)
                   msg_print("The granite is eaten away!");
                 else
                   msg_print("The granite turns into mud!");
                 obvious = TRUE;
              }

              /* Destroy the wall */
              (void)set_grid_type(x, y, DUNG_FLOOR,
                             DUNG_FLOOR_NORMAL, GRID_KEEP, 0);
              break;

            case DUNG_WALL_ART_QUARTZ:
            {
              bool gold = FALSE;
              if (typ==GF_ACID) break;
              /* Message */
              if (c_ptr->fdat & CAVE_MARK)
              {
                 msg_print("The quartz turns into mud!");
                 if (treasure(x,y)) gold = TRUE;
                 obvious = TRUE;
              }

              /* Destroy the wall */
              (void)set_grid_type(x, y, DUNG_FLOOR,
                             DUNG_FLOOR_NORMAL, GRID_KEEP, 0);
              if (gold)
              {
                 msg_print("You have found something!");
                 /* Place some gold */
                 place_gold(x, y);
              }
              break;
            }

            case DUNG_WALL_ART_MAGMA:
            {
              bool gold = FALSE;
              if ((typ==GF_ACID) && (rand_int(10)>2)) break;
              /* Message */
              if (c_ptr->fdat & CAVE_MARK)
              {
                 if (typ==GF_ACID)
                   msg_print("The magma is eaten away!");
                 else
                   msg_print("The magma turns into mud!");
                 if (treasure(x,y)) gold = TRUE;
                 obvious = TRUE;
              }

              /* Destroy the wall */
              (void)set_grid_type(x, y, DUNG_FLOOR,
                             DUNG_FLOOR_NORMAL, GRID_KEEP, 0);
              if (gold)
              {
                 msg_print("You have found something!");
                 /* Place some gold */
                 place_gold(x, y);
              }
              break;
            }

            case DUNG_WALL_ART_CHALK:
            {
              bool gold = FALSE;
              /* Message */
              if (c_ptr->fdat & CAVE_MARK)
              {
                 if (typ==GF_ACID)
                   msg_print("The chalk is eaten away!");
                 else
                   msg_print("The chalk turns into mud!");
                 if (treasure(x,y)) gold = TRUE;
                 obvious = TRUE;
              }

              /* Destroy the wall */
              (void)set_grid_type(x, y, DUNG_FLOOR,
                             DUNG_FLOOR_NORMAL, GRID_KEEP, 0);
              if (gold)
              {
                 msg_print("You have found something!");
                 /* Place some gold */
                 place_gold(x, y);
              }
              break;
            }
            case DUNG_WALL_ART_RUBBLE:
            {
              if ((typ==GF_ACID) && (rand_int(10)>5)) break;
              /* Message */
              if (c_ptr->fdat & CAVE_MARK)
              {
                 if (typ==GF_ACID)
                   msg_print("The rubble is eaten away!");
                 else
                   msg_print("The rubble turns into mud!");
                 obvious = TRUE;
              }

              /* Destroy the wall */
              (void)set_grid_type(x, y, DUNG_FLOOR,
                             DUNG_FLOOR_NORMAL, GRID_KEEP, 0);

              if (rand_int(100) < 10)
              {
                 /* Found something */
                 if (player_can_see_bold(x,y))
                 {
                    msg_print("There was something buried in the rubble!");
                    obvious = TRUE;
                 }

                 /* Place gold - we know the floor is clear! */
                 place_object(x, y, FALSE, FALSE, FALSE);
              }

              break;
            }
            /* Destroy doors (and secret doors) */

            case -1:
            {
               obvious = remove_traps(x, y, "Click!");
               /* Hack -- special message */
               if (c_ptr->fdat & CAVE_MARK)
               {
                  if (typ==GF_ACID) msg_print("The door is eaten away!");
                  else msg_print("The door turns into mud!");
                  obvious = TRUE;
               }

               /* Destroy the feature */
               (void)set_grid_type(x, y, DUNG_FLOOR,
                                   DUNG_FLOOR_NORMAL, GRID_KEEP, 0);
            }
         } /* end switch */

         /* Forget the wall */
         c_ptr->fdat &= ~CAVE_MARK;

         /* Notice */
         note_spot(x, y);

         /* Redraw */
         lite_spot(x, y);

         /* Update some things */
         p_ptr->update |= (PU_VIEW | PU_FLOW | PU_MONSTERS);

         break;

      /* Make doors */
      case GF_MAKE_DOOR:

         /* Require a "naked" floor grid */
         if (!naked_grid_bold(x, y)) break;

         obvious = remove_traps(x, y, "Click!");

         /* Create a closed door */
         (void)set_grid_type(x, y, DUNG_DOOR,
                        DUNG_DOOR_CLOSED, GRID_KEEP, 0);
         c_ptr->t_idx = 0;    /* let's not make a trap on it right away */

         /* Notice */
         note_spot(x, y);

         /* Redraw */
         lite_spot(x, y);

         /* Observe */
         if (c_ptr->fdat & CAVE_MARK) obvious = TRUE;

         /* Update some things */
         p_ptr->update |= (PU_VIEW | PU_MONSTERS);

         break;

      /* Make traps */
      case GF_MAKE_TRAP:

         /* Require a "naked" floor grid */
         if (test_grid(x, y, DUNG_FLOOR, DUNG_FLOOR_NORMAL))
         {
            /* disarm any old traps */
            obvious = remove_traps(x, y, "Click!");

            /* Place a trap */
dlog(DEBUGTRAPS,"spells1.c: project_f: about to place trap (player %d,%d) @ %d,%d mt %d st %d fdat %08lx\n",
                px, py, x, y, dungeon.level[sublevel][y][x].mtyp, dungeon.level[sublevel][y][x].styp, dungeon.level[sublevel][y][x].fdat);
            place_trap(x, y, p_ptr->mdepth, 5, 1);
dlog(DEBUGTRAPS,"spells1.c: project_f: after placing trap @ %d,%d mt %d st %d fdat %08lx\n",
                x, y, dungeon.level[sublevel][y][x].mtyp, dungeon.level[sublevel][y][x].styp, dungeon.level[sublevel][y][x].fdat);

            /* Notice */
            note_spot(x, y);

            /* Redraw */
            lite_spot(x, y);

            p_ptr->update |= PU_VIEW;
         }
/* jk - place traps on doors as well */
         else if (test_grid(x, y, DUNG_DOOR, DUNG_DOOR_CLOSED) ||
                  test_grid(x, y, DUNG_DOOR, DUNG_DOOR_LOCKED))
         {
            /* disarm any old traps */
            obvious = remove_traps(x, y, "Click!");

            place_traps_door(x, y, p_ptr->mdepth, 20, 5);
            /* Notice */
            note_spot(x, y);

            /* Redraw */
            lite_spot(x, y);
            p_ptr->update |= PU_VIEW;
         }

         break;

      /* Lite up the grid */
      case GF_LITE_WEAK:
      case GF_LITE:

         /* Turn on the light */
         c_ptr->fdat |= CAVE_GLOW;

         /* Notice */
         note_spot(x, y);

         /* Redraw */
         lite_spot(x, y);

         /* Observe */
         if (player_can_see_bold(x,y)) obvious = TRUE;

         /* Mega-Hack -- Update the monster in the affected grid */
         /* This allows "spear of light" (etc) to work "correctly" */
         if (c_ptr->m_idx) update_mon(c_ptr->m_idx, FALSE);

         break;

      /* Darken the grid */
      case GF_DARK_WEAK:
      case GF_DARK:

         /* Notice */
         if (player_can_see_bold(x,y)) obvious = TRUE;

         /* Turn off the light. */
         c_ptr->fdat &= ~CAVE_GLOW;

         /* Hack -- Forget "boring" grids */
         if ( (c_ptr->mtyp == DUNG_FLOOR) &&
             ( (c_ptr->styp == DUNG_FLOOR_NORMAL) ||
               (c_ptr->styp == DUNG_FLOOR_TRAP) ) )
         {
            /* Forget */
            c_ptr->fdat &= ~CAVE_MARK;

            /* Notice */
            note_spot(x, y);
         }

         /* Redraw */
         lite_spot(x, y);

         /* Mega-Hack -- Update the monster in the affected grid */
         /* This allows "spear of light" (etc) to work "correctly" */
         if (c_ptr->m_idx) update_mon(c_ptr->m_idx, FALSE);

         /* All done */
         break;
   }

   /* Return "Anything seen?" */
   return (obvious);
}

/*
 * We are called from "project()" to "damage" objects
 *
 * We are called both for "beam" effects and "ball" effects.
 *
 * Perhaps we should only SOMETIMES damage things on the ground.
 *
 * The "r" parameter is the "distance from ground zero".
 *
 * Note that we determine if the player can "see" anything that happens
 * by taking into account: blindness, line-of-sight, and illumination.
 *
 * XXX XXX XXX We also "see" grids which are "memorized", probably a hack
 *
 * We return "TRUE" if the effect of the projection is "obvious".
 */
static bool project_i(project_who_type *who, s16b r, s16b x, s16b y, s16b dam, s16b typ)
{
   object_type *i_ptr;
/* jk */
   s16b j,objs = objects_on_floor(x,y);

   bool        obvious = FALSE;

   bool        is_art = FALSE;
   bool        ignore = FALSE;
   bool        plural = FALSE;
   bool        do_kill = FALSE;

   cptr        note_kill = NULL;

   u64b        f1, f2, f3;

   char        i_name[80];

   s16b         div;


   /* Nothing here */
/* jk */
   if (objs==0) return(FALSE);

   for (j=0;j<objs;j++)
   {
      /* necessary: deleting takes place further on, and objs stays the same */
      /* even if the number of objects goes down! */
      if (!floor_item_xy(j,x,y)) continue;
      i_ptr = get_item_pointer_floor_xy(j,x,y);

      /* Extract radius */
      div = r + 1;

      /* Adjust damage */
      dam = dam / div;

      /* Extract the flags */
      object_flags(i_ptr, &f1, &f2, &f3);

      /* Get the "plural"-ness */
      if (i_ptr->number > 1) plural = TRUE;

      /* Check for artifact */
      if (artifact_p(i_ptr)) is_art = TRUE;

      /* Analyze the type */
      switch (typ)
      {
         /* Acid -- Lots of things */
         case GF_ACID:
            if (hates_acid(i_ptr))
            {
               do_kill = TRUE;
               note_kill = (plural ? " melt!" : " melts!");
               if (f3 & TR3_IGNORE_ACID) ignore = TRUE;
            }
            break;

         /* Elec -- Rings and Wands */
         case GF_ELEC:
            if (hates_elec(i_ptr))
            {
               do_kill = TRUE;
               note_kill= (plural ? " is destroyed!" : " is destroyed!");
               if (f3 & TR3_IGNORE_ELEC) ignore = TRUE;
            }
            break;

         /* Fire -- Flammable objects */
         case GF_FIRE:
            if (hates_fire(i_ptr))
            {
               do_kill = TRUE;
               note_kill = (plural ? " burn up!" : " burns up!");
               if (f3 & TR3_IGNORE_FIRE) ignore = TRUE;
            }
            break;

         /* Cold -- potions and flasks */
         case GF_COLD:
            if (hates_cold(i_ptr))
            {
               note_kill = (plural ? " shatter!" : " shatters!");
               do_kill = TRUE;
               if (f3 & TR3_IGNORE_COLD) ignore = TRUE;
            }
            break;

         /* Fire + Elec */
         case GF_PLASMA:
            if (hates_fire(i_ptr))
            {
               do_kill = TRUE;
               note_kill = (plural ? " burn up!" : " burns up!");
               if (f3 & TR3_IGNORE_FIRE) ignore = TRUE;
            }
            if (hates_elec(i_ptr))
            {
               ignore = FALSE;
               do_kill = TRUE;
               note_kill= (plural ? " is destroyed!" : " is destroyed!");
               if (f3 & TR3_IGNORE_ELEC) ignore = TRUE;
            }
            break;

         /* Fire + Cold */
         case GF_METEOR:
            if (hates_fire(i_ptr))
            {
               do_kill = TRUE;
               note_kill = (plural ? " burn up!" : " burns up!");
               if (f3 & TR3_IGNORE_FIRE) ignore = TRUE;
            }
            if (hates_cold(i_ptr))
            {
               ignore = FALSE;
               do_kill = TRUE;
               note_kill= (plural ? " shatter!" : " shatters!");
               if (f3 & TR3_IGNORE_COLD) ignore = TRUE;
            }
            break;

         /* Hack -- break potions and such */
         case GF_ICE:
         case GF_SHARDS:
         case GF_FORCE:
         case GF_SOUND:
            if (hates_cold(i_ptr))
            {
               note_kill = (plural ? " shatter!" : " shatters!");
               do_kill = TRUE;
            }
            break;

         /* Mana -- destroys everything */
         case GF_MANA:
            do_kill = TRUE;
            note_kill = (plural ? " is destroyed!" : " is destroyed!");

         /* Holy Orb -- destroys cursed non-artifacts */
         case GF_HOLY_ORB:
            if (cursed_p(i_ptr))
            {
               do_kill = TRUE;
               note_kill= (plural ? " is destroyed!" : " is destroyed!");
            }
            break;

         /* Unlock chests */
         case GF_KILL_TRAP:
         case GF_KILL_DOOR:

            /* Chests are noticed only if trapped or locked */
            if (i_ptr->tval == TV_CHEST)
            {
               /* Disarm/Unlock traps */
               if (i_ptr->p1val > 0)
               {
                  /* Disarm or Unlock */
                  i_ptr->p1val = (0 - i_ptr->p1val);

                  if (i_ptr->xtra2) i_ptr->xtra2 = 0; /* disarm */
                  /* Identify */
                  object_known(i_ptr);

                  /* Notice */
                  if (i_ptr->marked)
                  {
                     msg_print("Click!");
                     obvious = TRUE;
                  }
               }
            }

            break;
      }

      /* Attempt to destroy the object */
      if (do_kill)
      {
         /* Effect "observed" */
         if (i_ptr->marked)
         {
            obvious = TRUE;
            object_desc(i_name, i_ptr, FALSE, 0);
         }

         /* Artifacts, and other objects, get to resist */
         if (is_art || ignore)
         {
            /* Observe the resist */
            if (i_ptr->marked)
            {
               msg_format("The %s %s unaffected!",
                          i_name, (plural ? "are" : "is"));
            }
         }

         /* Kill some spell pages */
         else if (i_ptr->tval == TV_BOOK)
         {
            s16b count;
            s16b index[MAX_SPELLS_PER_ITEM], maxnum, num, start, i;

            maxnum = 2;
            if (i_ptr->sval == SV_BOOK_AVG) maxnum = 3;
            if (i_ptr->sval == SV_BOOK_LARGE) maxnum = 7;

            for (i=0, count=0; i < (s16b)s_number; i++)
            {
               if (has_spell(i_ptr, i)) index[count++]=i;
            }
            /* how many to destroy */
            num=rand_int(maxnum);

            /* trying to destroy more spells than the book holds? tough luck! */
            if (num >= count)
            {
               /* Describe if needed */
               if (i_ptr->marked && note_kill)
               {
                  msg_format("The %s%s", i_name, note_kill);
                  msg_format("debug: maxnum %d num %d count %d", maxnum, num, count);
               }
               /* Delete the object */
               delete_object(x,y,j);
            }
            else
            {
               /* where to start in the book */
               start=randint(count);
               while (num > 0)
               {
                  spell_type *s_ptr = &s_info[index[start]];
   
                  msg_format("the page with your spell of %s%s!", s_name + s_ptr->name, note_kill);
                  clr_spell(i_ptr, index[start]);
                  start++;
                  /* wrap around if necessary */
                  if (start==count) start=0;
                  num--;
               }
            }
                 
            /* now get one back in the loop! */
            j--;
            /* have we done all */
            if (j<0) break;

            /* Redraw */
            lite_spot(y,x);
         }
         /* Kill It */
         else
         {
            /* Describe if needed */
            if (i_ptr->marked && note_kill)
            {
               msg_format("The %s%s", i_name, note_kill);
            }

            /* Delete the object */
            delete_object(x,y,j);
            /* now get one back in the loop! */
            j--;
            /* have we done all */
            if (j<0) break;

            /* Redraw */
            lite_spot(y,x);
         }
      }
   }
   /* Return "Anything seen?" */
   return (obvious);
}

/*
 * Helper function for "project()" below.
 *
 * Handle a beam/bolt/ball causing damage to a monster.
 *
 * This routine takes a "source monster" (by index) which is mostly used to
 * determine if the player is causing the damage, and a "radius" (see below),
 * which is used to decrease the power of explosions with distance, and a
 * location, via integers which are modified by certain types of attacks
 * (polymorph and teleport being the obvious ones), a default damage, which
 * is modified as needed based on various properties, and finally a "damage
 * type" (see below).
 *
 * Note that this routine can handle "no damage" attacks (like teleport) by
 * taking a "zero" damage, and can even take "parameters" to attacks (like
 * confuse) by accepting a "damage", using it to calculate the effect, and
 * then setting the damage to zero.  Note that the "damage" parameter is
 * divided by the radius, so monsters not at the "epicenter" will not take
 * as much damage (or whatever)...
 *
 * Note that "polymorph" is dangerous, since a failure in "place_monster()"'
 * may result in a dereference of an invalid pointer.  XXX XXX XXX
 *
 * Various messages are produced, and damage is applied.
 *
 * Just "casting" a substance (i.e. plasma) does not make you immune, you must
 * actually be "made" of that substance, or "breathe" big balls of it.
 *
 * We assume that "Plasma" monsters, and "Plasma" breathers, are immune
 * to plasma.
 *
 * We assume "Nether" is an evil, necromantic force, so it doesn't hurt undead,
 * and hurts evil less.  If can breath nether, then it resists it as well.
 *
 * Damage reductions use the following formulas:
 *   Note that "dam = dam * 6 / (randint(6) + 6);"
 *     gives avg damage of .655, ranging from .858 to .500
 *   Note that "dam = dam * 5 / (randint(6) + 6);"
 *     gives avg damage of .544, ranging from .714 to .417
 *   Note that "dam = dam * 4 / (randint(6) + 6);"
 *     gives avg damage of .444, ranging from .556 to .333
 *   Note that "dam = dam * 3 / (randint(6) + 6);"
 *     gives avg damage of .327, ranging from .427 to .250
 *   Note that "dam = dam * 2 / (randint(6) + 6);"
 *     gives something simple.
 *
 * In this function, "result" messages are postponed until the end, where
 * the "note" string is appended to the monster name, if not NULL.  So,
 * to make a spell have "no effect" just set "note" to NULL.  You should
 * also set "notice" to FALSE, or the player will learn what the spell does.
 *
 * We attempt to return "TRUE" if the player saw anything "useful" happen.
 */
static bool project_m(project_who_type *who, s16b r, s16b x, s16b y, s16b dam, s16b typ)
{
   s16b i, div;

   cave_cell_type *c_ptr = &dungeon.level[sublevel][y][x];
   monster_type   *m_ptr = &mn_list[c_ptr->m_idx];
   monster_race   *r_ptr = &r_info[m_ptr->r_idx];
   cptr            name = (r_name + r_ptr->name);

   /* Is the monster "seen"? */
   bool seen = m_ptr->ml;

   /* Were the "effects" obvious (if seen)? */
   bool obvious = FALSE;

   /* Polymorph setting (true or false) */
   s16b do_poly = 0;

   /* Teleport setting (max distance) */
   s16b do_dist = 0;

   /* Confusion setting (amount to confuse) */
   s16b do_conf = 0;

   /* Stunning setting (amount to stun) */
   s16b do_stun = 0;

   /* Sleep amount (amount to sleep) */
   s16b do_sleep = 0;

   /* Fear amount (amount to fear) */
   s16b do_fear = 0;

   /* Hold the monster name */
   char m_name[80];

   /* Assume no note */
   cptr note = NULL;

   /* Assume a default death */
   cptr note_dies = " dies.";

   /* Nobody here */
   if (!c_ptr->m_idx) return (FALSE);

   /* Never affect projector */
   if ((who->type == WHO_MONSTER) && (c_ptr->m_idx == who->index)) return (FALSE);

   /* Extract radius */
   div = r + 1;

   /* Decrease damage */
   dam = dam / div;

   /* Mega-Hack */
   project_m_n++;
   project_m_x = x;
   project_m_y = y;

   /* Get the monster name (BEFORE polymorphing) */
   monster_desc(m_name, m_ptr, 0x0200);

   /* Some monsters get "destroyed" */
   if ((r_ptr->flags3 & RF3_DEMON) ||
       (r_ptr->flags3 & RF3_UNDEAD) ||
       (r_ptr->flags2 & RF2_STUPID) ||
       (strchr("Evg", r_ptr->d_char)))
   {
      /* Special note at death */
      note_dies = " is destroyed.";
   }

   /* Analyze the damage type */
   switch (typ)
   {
      /* Magic Missile -- pure damage */
      case GF_MISSILE:
         if (seen)
         {
            msg_format("%^s is hit by a magic missile.", m_name);
            obvious = TRUE;
         }
         break;

      /* Acid */
      case GF_ACID:
         if (seen)
         {
            msg_format("%^s is hit by acidic gasses.", m_name);
            obvious = TRUE;
         }
         if (r_ptr->flags3 & RF3_IM_ACID)
         {
            note = " resists a lot.";
            dam /= 9;
            if (seen) r_ptr->r_flags3 |= RF3_IM_ACID;
         }
         break;

      /* Electricity */
      case GF_ELEC:
         if (seen)
         {
            msg_format("%^s is hit by lightning.", m_name);
            obvious = TRUE;
         }
         if (r_ptr->flags3 & RF3_IM_ELEC)
         {
            note = " resists a lot.";
            dam /= 9;
            if (seen) r_ptr->r_flags3 |= RF3_IM_ELEC;
         }
         break;

      /* Fire damage */
      case GF_FIRE:
         if (seen)
         {
            msg_format("%^s is hit by fire.", m_name);
            obvious = TRUE;
         }
         if (r_ptr->flags3 & RF3_IM_FIRE)
         {
            note = " resists a lot.";
            dam /= 9;
            if (seen) r_ptr->r_flags3 |= RF3_IM_FIRE;
         }
         break;

      /* Cold */
      case GF_COLD:
         if (seen)
         {
            msg_format("%^s is hit by cold.", m_name);
            obvious = TRUE;
         }
         if (r_ptr->flags3 & RF3_IM_COLD)
         {
            note = " resists a lot.";
            dam /= 9;
            if (seen) r_ptr->r_flags3 |= RF3_IM_COLD;
         }
         break;

      /* Poison */
      case GF_POIS:
         if (seen)
         {
            msg_format("%^s is hit by poisonous gasses.", m_name);
            obvious = TRUE;
         }
         if (r_ptr->flags3 & RF3_IM_POIS)
         {
            note = " resists a lot.";
            dam /= 9;
            if (seen) r_ptr->r_flags3 |= RF3_IM_POIS;
         }
         break;

      /* Holy Orb -- hurts Evil */
      case GF_HOLY_ORB:
         if (seen)
         {
            msg_format("%^s is hit by divine energy.", m_name);
            obvious = TRUE;
         }
         if (r_ptr->flags3 & RF3_EVIL)
         {
            dam *= 2;
            note = " is hit hard.";
            if (seen) r_ptr->r_flags3 |= RF3_EVIL;
         }
         break;

      /* Arrow -- XXX no defense */
      case GF_ARROW:
         if (seen)
         {
            msg_format("%^s is hit by an arrow.", m_name);
            obvious = TRUE;
         }
         if (seen) obvious = TRUE;
         break;

      /* Plasma -- XXX perhaps check ELEC or FIRE */
      case GF_PLASMA:
         if (seen)
         {
            msg_format("%^s is hit by a plasma cloud.", m_name);
            obvious = TRUE;
         }
         if (prefix(name, "Plasma") || (r_ptr->flags4 & RF4_BR_PLAS))
         {
            note = " resists.";
            dam *= 3; dam /= (randint(6)+6);
         }
         break;

      /* Nether -- see above */
      case GF_NETHER:
         if (seen)
         {
            msg_format("%^s is hit by a nether cloud.", m_name);
            obvious = TRUE;
         }
         if (r_ptr->flags3 & RF3_UNDEAD)
         {
            note = " is immune.";
            dam = 0;
            if (seen) r_ptr->r_flags3 |= RF3_UNDEAD;
         }
         else if (r_ptr->flags4 & RF4_BR_NETH)
         {
            note = " resists.";
            dam *= 3; dam /= (randint(6)+6);
         }
         else if (r_ptr->flags3 & RF3_EVIL)
         {
            dam /= 2;
            note = " resists somewhat.";
            if (seen) r_ptr->r_flags3 |= RF3_EVIL;
         }
         break;

      /* Water (acid) damage -- Water spirits/elementals are immune */
      case GF_WATER:
         if (seen)
         {
            msg_format("%^s is hit by a acidic water.", m_name);
            obvious = TRUE;
         }
         if ((r_ptr->d_char == 'E') && prefix(name, "W"))
         {
            note = " is immune.";
            dam = 0;
         }
         break;

      /* Chaos -- Chaos breathers resist */
      case GF_CHAOS:
         if (seen)
         {
            msg_format("%^s is hit by chaos energy.", m_name);
            obvious = TRUE;
         }
         do_poly = TRUE;
         do_conf = (5 + randint(11)) / div;
         if (r_ptr->flags4 & RF4_BR_CHAO)
         {
            note = " resists.";
            dam *= 3; dam /= (randint(6)+6);
            do_poly = FALSE;
         }
         break;

      /* Shards -- Shard breathers resist */
      case GF_SHARDS:
         if (seen)
         {
            msg_format("%^s is hit by shards.", m_name);
            obvious = TRUE;
         }
         if (r_ptr->flags4 & RF4_BR_SHAR)
         {
            note = " resists.";
            dam *= 3; dam /= (randint(6)+6);
         }
         break;

      /* Sound -- Sound breathers resist */
      case GF_SOUND:
         if (seen)
         {
            msg_format("%^s is hit by concentrated sound.", m_name);
            obvious = TRUE;
         }
         do_stun = (10 + randint(15)) / div;
         if (r_ptr->flags4 & RF4_BR_SOUN)
         {
            note = " resists.";
            dam *= 2; dam /= (randint(6)+6);
         }
         break;

      /* Confusion */
      case GF_CONFUSION:
         if (seen)
         {
            msg_format("%^s is hit by confusion.", m_name);
            obvious = TRUE;
         }
         do_conf = (10 + randint(15)) / div;
         if (r_ptr->flags4 & RF4_BR_CONF)
         {
            note = " resists.";
            dam *= 2; dam /= (randint(6)+6);
         }
         else if (r_ptr->flags3 & RF3_NO_CONF)
         {
            note = " resists somewhat.";
            dam /= 2;
         }
         break;

      /* Confusion */
      case GF_CONF_STRONG:
         if (seen)
         {
            msg_format("%^s is hit by confusion.", m_name);
            obvious = TRUE;
         }
         do_conf = (50 + randint(75)) / div;
         if (r_ptr->flags4 & RF4_BR_CONF)
         {
            note = " resists.";
            dam *= 2; dam /= (randint(6)+6);
         }
         else if (r_ptr->flags3 & RF3_NO_CONF)
         {
            note = " resists somewhat.";
            dam /= 2;
         }
         break;

      /* Disenchantment -- Breathers and Disenchanters resist */
      case GF_DISENCHANT:
         if (seen)
         {
            msg_format("%^s is hit by disenchantment.", m_name);
            obvious = TRUE;
         }
         if ((r_ptr->flags4 & RF4_BR_DISE) ||
             prefix(name, "Disen"))
         {
            note = " resists.";
            dam *= 3; dam /= (randint(6)+6);
         }
         break;

      /* Nexus -- Breathers and Existers resist */
      case GF_NEXUS:
         if (seen)
         {
            msg_format("%^s is hit by nexus energy.", m_name);
            obvious = TRUE;
         }
         if ((r_ptr->flags4 & RF4_BR_NEXU) ||
             prefix(name, "Nexus"))
         {
            note = " resists.";
            dam *= 3; dam /= (randint(6)+6);
         }
         break;

      /* Force */
      case GF_FORCE:
         if (seen)
         {
            msg_format("%^s is hit by pure force.", m_name);
            obvious = TRUE;
         }
         do_stun = randint(15) / div;
         if (r_ptr->flags4 & RF4_BR_WALL)
         {
            note = " resists.";
            dam *= 3; dam /= (randint(6)+6);
         }
         break;

      /* Inertia -- breathers resist */
      case GF_INERTIA:
         if (seen)
         {
            msg_format("%^s is hit by inertia.", m_name);
            obvious = TRUE;
         }
         if (r_ptr->flags4 & RF4_BR_INER)
         {
            note = " resists.";
            dam *= 3; dam /= (randint(6)+6);
         }
         break;

      /* Time -- breathers resist */
      case GF_TIME:
         if (seen)
         {
            msg_format("%^s is hit by magic time vortices.", m_name);
            obvious = TRUE;
         }
         if (r_ptr->flags4 & RF4_BR_TIME)
         {
            note = " resists.";
            dam *= 3; dam /= (randint(6)+6);
         }
         break;

      /* Gravity -- breathers resist */
      case GF_GRAVITY:
         if (seen)
         {
            msg_format("%^s is hit by gravity.", m_name);
            obvious = TRUE;
         }
         do_dist = 10;
         if (r_ptr->flags4 & RF4_BR_GRAV)
         {
            note = " resists.";
            dam *= 3; dam /= (randint(6)+6);
            do_dist = 0;
         }
         break;

      /* Pure damage */
      case GF_MANA:
         if (seen)
         {
            msg_format("%^s is hit by pure mana.", m_name);
            obvious = TRUE;
         }
         break;

      /* Meteor -- powerful magic missile */
      case GF_METEOR:
         if (seen)
         {
            msg_format("%^s is hit by a large magic missile.", m_name);
            obvious = TRUE;
         }
         break;

      /* Ice -- Cold + Cuts + Stun */
      case GF_ICE:
         if (seen)
         {
            msg_format("%^s is hit by a icy shards.", m_name);
            obvious = TRUE;
         }
         do_stun = randint(15) / div;
         if (r_ptr->flags3 & RF3_IM_COLD)
         {
            note = " resists a lot.";
            dam /= 9;
            if (seen) r_ptr->r_flags3 |= RF3_IM_COLD;
         }
         break;

      /* Drain Life */
      case GF_DRAIN:
         if (seen)
         {
            msg_format("%^s is hit by a deep-black cloud.", m_name);
            obvious = TRUE;
         }
         if ((r_ptr->flags3 & RF3_UNDEAD) ||
             (r_ptr->flags3 & RF3_DEMON) ||
             (strchr("Egv", r_ptr->d_char)))
         {
            if (r_ptr->flags3 & RF3_UNDEAD)
            {
               if (seen) r_ptr->r_flags3 |= RF3_UNDEAD;
            }
            if (r_ptr->flags3 & RF3_DEMON)
            {
               if (seen) r_ptr->r_flags3 |= RF3_DEMON;
            }

            note = " is unaffected!";
            obvious = FALSE;
            dam = 0;
         }

         break;

      /* Polymorph monster (Use "dam" as "power") */
      case GF_POLY:

         if (seen)
         {
            msg_format("%^s is hit by a whirling energy.", m_name);
            obvious = TRUE;
         }

         /* Attempt to polymorph (see below) */
         do_poly = TRUE;

         /* Powerful monsters can resist */
         if ((r_ptr->flags1 & RF1_UNIQUE) ||
             (r_ptr->level > randint((dam - 10) < 1 ? 1 : (dam - 10)) + 10))
         {
            note = " is unaffected!";
            do_poly = FALSE;
            obvious = FALSE;
         }

         /* No "real" damage */
         dam = 0;

         break;

      /* Clone monsters (Ignore "dam") */
      case GF_CLONE:

         if (seen)
         {
            msg_format("%^s is hit by a yellow glow.", m_name);
            obvious = TRUE;
         }

         /* Heal fully */
         m_ptr->hp = m_ptr->maxhp;

         /* Speed up */
         if (m_ptr->mspeed < 150) m_ptr->mspeed += 10;

         /* Attempt to clone. */
         if (multiply_monster(c_ptr->m_idx))
         {
            note = " spawns!";
         }

         /* No "real" damage */
         dam = 0;

         break;

      /* Heal Monster (use "dam" as amount of healing) */
      case GF_HEAL:

         if (seen)
         {
            msg_format("%^s is hit by a soft green glow.", m_name);
            obvious = TRUE;
         }

         /* Wake up */
         m_ptr->csleep = 0;

         /* Heal */
         m_ptr->hp += dam;

         /* No overflow */
         if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

         /* Redraw (later) if needed */
         if (health_who == c_ptr->m_idx) p_ptr->redraw1 |= (PR1_HEALTH);

         /* Message */
         note = " looks healthier.";

         /* No "real" damage */
         dam = 0;
         break;

      /* Speed Monster (Ignore "dam") */
      case GF_SPEED:
         if (seen)
         {
            msg_format("%^s is hit by a soft purple glow.", m_name);
            obvious = TRUE;
         }

         /* Speed up */
         if (m_ptr->mspeed < 150) m_ptr->mspeed += 10;
         note = " starts moving faster.";

         /* No "real" damage */
         dam = 0;
         break;

      /* Slow Monster (Use "dam" as "power") */
      case GF_SLOW:

         if (seen)
         {
            msg_format("%^s is hit by an intense purple glow.", m_name);
            obvious = TRUE;
         }
         if (seen) obvious = TRUE;

         /* Powerful monsters can resist */
         if ((r_ptr->flags1 & RF1_UNIQUE) ||
             (r_ptr->level > randint((dam - 10) < 1 ? 1 : (dam - 10)) + 10))
         {
            note = " is unaffected!";
            obvious = FALSE;
         }

         /* Normal monsters slow down */
         else
         {
            if (m_ptr->mspeed > 60) m_ptr->mspeed -= 10;
            note = " starts moving slower.";
         }

         /* No "real" damage */
         dam = 0;
         break;

      /* Sleep (Use "dam" as "power") */
      case GF_SLEEP:

         if (seen)
         {
            msg_format("%^s is hit by an soothing blue cloud.", m_name);
            obvious = TRUE;
         }

         /* Attempt a saving throw */
         if ((r_ptr->flags1 & RF1_UNIQUE) ||
             (r_ptr->flags3 & RF3_NO_SLEEP) ||
             (r_ptr->level > randint((dam - 10) < 1 ? 1 : (dam - 10)) + 10))
         {
            /* Memorize a flag */
            if (r_ptr->flags3 & RF3_NO_SLEEP)
            {
               if (seen) r_ptr->r_flags3 |= RF3_NO_SLEEP;
            }

            /* No obvious effect */
            note = " is unaffected!";
            obvious = FALSE;
         }
         else
         {
            /* Go to sleep (much) later */
            if (m_ptr->csleep == 0)
            {
               note = " falls asleep!";
            }
            else
            {
               note = "seems unaffected!";
            } 
            do_sleep = 500;
         }

         /* No "real" damage */
         dam = 0;
         break;

      /* Strong Sleep (Use "dam" as "power") */
      case GF_SLEEP_STRONG:

         if (seen)
         {
            msg_format("%^s is hit by an soothing, intense blue cloud.", m_name);
            obvious = TRUE;
         }

         /* Attempt a saving throw - start with a 2/3 chance it will work */
         if ( (randint(3)==1) &&
              ( (r_ptr->flags1 & RF1_UNIQUE) ||
                (r_ptr->flags3 & RF3_NO_SLEEP) ||
                ((r_ptr->level*2) > randint( ( (dam - 10) < 1 ) ? 1 : (dam - 10) ) + 10)) )
         {
            /* Memorize a flag */
            if (r_ptr->flags3 & RF3_NO_SLEEP)
            {
               if (seen) r_ptr->r_flags3 |= RF3_NO_SLEEP;
            }

            /* No obvious effect */
            note = " is unaffected!";
            obvious = FALSE;
         }
         else
         {
            /* Go to sleep (much) later */
            if (m_ptr->csleep == 0)
            {
               note = " falls asleep!";
            }
            else
            {
               note = "seems unaffected!";
            } 
            do_sleep = 750;
            /* uniques sleep short */
            if (r_ptr->flags1 & RF1_UNIQUE) do_sleep = 150+randint(350);
            /* resistant monsters sleep even shorter */
            if (r_ptr->flags3 & RF3_NO_SLEEP) do_sleep = 50+randint(450);
         }

         /* No "real" damage */
         dam = 0;
         break;


      /* Confusion (Use "dam" as "power") */
      case GF_CONF:

         if (seen)
         {
            msg_format("%^s is hit by confusion.", m_name);
            obvious = TRUE;
         }
         if (seen) obvious = TRUE;

         /* Get confused later */
         do_conf = damroll(3, (dam / 2)) + 1;

         /* Attempt a saving throw */
         if ((r_ptr->flags1 & RF1_UNIQUE) ||
             (r_ptr->flags3 & RF3_NO_CONF) ||
             (r_ptr->level > randint((dam - 10) < 1 ? 1 : (dam - 10)) + 10))
         {
            /* Memorize a flag */
            if (r_ptr->flags3 & RF3_NO_CONF)
            {
               if (seen) r_ptr->r_flags3 |= RF3_NO_CONF;
            }

            /* Resist */
            do_conf = 0;

            /* No obvious effect */
            note = " is unaffected!";
            obvious = FALSE;
         }

         /* No "real" damage */
         dam = 0;
         break;

      /* Lite, but only hurts susceptible creatures */
      case GF_LITE_WEAK:

         if (seen)
         {
            msg_format("%^s is hit by a strong light.", m_name);
            obvious = TRUE;
         }

         /* Hurt by light */
         if (r_ptr->flags3 & RF3_HURT_LITE)
         {
            /* Obvious effect */
            if (seen) obvious = TRUE;

            /* Memorize the effects */
            if (seen) r_ptr->r_flags3 |= RF3_HURT_LITE;

            /* Special effect */
            note = " cringes from the light!";
            note_dies = " shrivels away in the light!";
         }

         /* Normally no damage */
         else
         {
            /* No damage */
            dam = 0;
         }

         break;

      /* Lite -- opposite of Dark */
      case GF_LITE:
         if (seen)
         {
            msg_format("%^s is hit by a blinding light.", m_name);
            obvious = TRUE;
         }
         if (r_ptr->flags4 & RF4_BR_LITE)
         {
            note = " resists.";
            dam *= 2; dam /= (randint(6)+6);
         }
         else if (r_ptr->flags3 & RF3_HURT_LITE)
         {
            if (seen) r_ptr->r_flags3 |= RF3_HURT_LITE;
            note = " cringes from the light!";
            note_dies = " shrivels away in the light!";
            dam *= 2;
         }
         break;

      /* Dark -- opposite of Lite */
      case GF_DARK:
         if (seen)
         {
            msg_format("%^s is hit by a patch of darkness.", m_name);
            obvious = TRUE;
         }

         if (r_ptr->flags4 & RF4_BR_DARK)
         {
            note = " resists.";
            dam *= 2; dam /= (randint(6)+6);
         }
         break;

      /* Stone to Mud */
      case GF_KILL_WALL:

         if (seen)
         {
            msg_format("%^s is hit by an acidic glob.", m_name);
            obvious = TRUE;
         }

         /* Hurt by rock remover */
         if (r_ptr->flags3 & RF3_HURT_ROCK)
         {
            /* Notice effect */
            if (seen) obvious = TRUE;

            /* Memorize the effects */
            if (seen) r_ptr->r_flags3 |= RF3_HURT_ROCK;

            /* Cute little message */
            note = " loses some skin!";
            note_dies = " dissolves!";
         }

         /* Usually, ignore the effects */
         else
         {
            /* No damage */
            dam = 0;
         }

         break;


      /* Teleport undead (Use "dam" as "power") */
      case GF_AWAY_UNDEAD:

         /* Only affect undead */
         if (r_ptr->flags3 & RF3_UNDEAD)
         {
            if (seen)
            {
               msg_format("%^s is hit by pure white energy.", m_name);
               obvious = TRUE;
               r_ptr->r_flags3 |= RF3_UNDEAD;
            }
            do_dist = dam;
         }

         /* No "real" damage */
         dam = 0;
         break;

      /* Teleport evil (Use "dam" as "power") */
      case GF_AWAY_EVIL:

         /* Only affect undead */
         if (r_ptr->flags3 & RF3_EVIL)
         {
            if (seen)
            {
               msg_format("%^s is hit by pure blue energy.", m_name);
               obvious = TRUE;
               r_ptr->r_flags3 |= RF3_EVIL;
            }
            do_dist = dam;
         }

         /* No "real" damage */
         dam = 0;
         break;

      /* Teleport monster (Use "dam" as "power") */
      case GF_AWAY_ALL:

         /* Obvious */
         if (seen)
         {
            msg_format("%^s is hit by yellow energy.", m_name);
            obvious = TRUE;
         }

         /* Prepare to teleport */
         do_dist = dam;

         /* No "real" damage */
         dam = 0;
         break;

      /* Turn undead (Use "dam" as "power") */
      case GF_TURN_UNDEAD:

         /* Only affect undead */
         if (r_ptr->flags3 & RF3_UNDEAD)
         {
            /* Learn about type */
            if (seen)
            {
               msg_format("%^s is enveloped by pure white energy.", m_name);
               obvious = TRUE;
               r_ptr->r_flags3 |= RF3_UNDEAD;
            }

            /* Obvious */
            if (seen) obvious = TRUE;

            /* Apply some fear */
            do_fear = damroll(3, (dam / 2)) + 1;

            /* Attempt a saving throw */
            if (r_ptr->level > randint((dam - 10) < 1 ? 1 : (dam - 10)) + 10)
            {
               /* No obvious effect */
               note = " is unaffected!";
               obvious = FALSE;
               do_fear = 0;
            }
         }

         /* No "real" damage */
         dam = 0;
         break;

      /* Turn evil (Use "dam" as "power") */
      case GF_TURN_EVIL:

         /* Only affect evil */
         if (r_ptr->flags3 & RF3_EVIL)
         {
            if (seen)
            {
               msg_format("%^s is enveloped by pure blue energy.", m_name);
               obvious = TRUE;
               r_ptr->r_flags3 |= RF3_EVIL;
            }

            /* Apply some fear */
            do_fear = damroll(3, (dam / 2)) + 1;

            /* Attempt a saving throw */
            if (r_ptr->level > randint((dam - 10) < 1 ? 1 : (dam - 10)) + 10)
            {
               /* No obvious effect */
               note = " is unaffected!";
               obvious = FALSE;
               do_fear = 0;
            }
         }

         /* No "real" damage */
         dam = 0;
         break;

      /* Turn monster (Use "dam" as "power") */
      case GF_TURN_ALL:

         if (seen)
         {
            msg_format("%^s is enveloped by yellow energy.", m_name);
            obvious = TRUE;
         }

         /* Apply some fear */
         do_fear = damroll(3, (dam / 2)) + 1;

         /* Attempt a saving throw */
         if ((r_ptr->flags1 & RF1_UNIQUE) ||
             (r_ptr->flags3 & RF3_NO_FEAR) ||
             (r_ptr->level > randint((dam - 10) < 1 ? 1 : (dam - 10)) + 10))
         {
            /* No obvious effect */
            note = " is unaffected!";
            obvious = FALSE;
            do_fear = 0;
         }

         /* No "real" damage */
         dam = 0;
         break;

      /* Dispel undead */
      case GF_DISP_UNDEAD:

         /* Only affect undead */
         if (r_ptr->flags3 & RF3_UNDEAD)
         {
            if (seen)
            {
               msg_format("%^s is surrounded by pure white energy.", m_name);
               obvious = TRUE;
               r_ptr->r_flags3 |= RF3_UNDEAD;
            }

            /* Message */
            note = " shudders.";
            note_dies = " dissolves!";
         }

         /* Ignore other monsters */
         else
         {
            /* No damage */
            dam = 0;
         }

         break;

      /* Dispel evil */
      case GF_DISP_EVIL:

         /* Only affect evil */
         if (r_ptr->flags3 & RF3_EVIL)
         {
            if (seen)
            {
               msg_format("%^s is surrounded by pure blue energy.", m_name);
               obvious = TRUE;
               r_ptr->r_flags3 |= RF3_EVIL;
            }

            /* Message */
            note = " shudders.";
            note_dies = " dissolves!";
         }

         /* Ignore other monsters */
         else
         {
            /* No damage */
            dam = 0;
         }

         break;

      /* Dispel monster */
      case GF_DISP_ALL:

         if (seen)
         {
            msg_format("%^s is surrounded by pure yellow energy.", m_name);
            obvious = TRUE;
         }

         /* Message */
         note = " shudders.";
         note_dies = " dissolves!";

         break;

      /* Default */
      default:

         /* No damage */
         dam = 0;

         break;
   }
/* jk - now throwing a ball of cold spell at a monster has certain risks! */
   if (m_ptr->has_drop)
   {
      test_monster_inven_damage(c_ptr->m_idx, typ, dam);
   }

   /* "Unique" monsters cannot be polymorphed */
   if (r_ptr->flags1 & RF1_UNIQUE) do_poly = FALSE;


   /* "Unique" monsters can only be "killed" by the player */
   if ( (r_ptr->flags1 & RF1_UNIQUE) && (dam > m_ptr->hp) )
   {
      /* Uniques may only be killed by the player */
      if ((who->type != WHO_PLAYER) && (who->type != WHO_TRAPBYPLAYER))
      {
         dam = m_ptr->hp; /* XXX XXX XXX ugly hack: the unique now has 0 HP */
      }
   }

   /* Check for death */
   if (dam > m_ptr->hp)
   {
      /* Extract method of death */
      note = note_dies;
   }

   /* Mega-Hack -- Handle "polymorph" -- monsters get a saving throw */
   /* polymorphing monsters doesn't change the monster anymore - too */
   /* much trouble with keeping the inventory intact with artifacts  */
   /* they are switched with another monster on the same level       */
   else if (do_poly && (randint(90) > r_ptr->level))
   {
      s16b monsters_on_level = 0;
      s16b monster_index[MAX_M_IDX];
      s16b new_monster = 0;
      s16b newx, newy;
      monster_type *m_ptr2;

      /* Default -- assume no polymorph */
      note = " is unaffected!";

      /* find out how many other monsters there still are on the level */
      for (i = 1; i < mn_max; i++)
      {
          m_ptr2 = &mn_list[i];
   
          /* Paranoia -- Skip dead monsters */
          if (!m_ptr2->r_idx) continue;
          /* don't call them from other sublevels! */
          if (m_ptr2->fz != sublevel) continue;
  
          monster_index[monsters_on_level++] = i; 
      }

      /* Pick a "new" monster race */
      i = poly_r_idx(m_ptr->r_idx);

      /* Handle polymorh */
      if ((i != m_ptr->r_idx) && (monsters_on_level > 5))
      {
         /* Obvious */
         if (seen) obvious = TRUE;

         /* Monster polymorphs */
         note = " changes!";

         /* Turn off the damage */
         dam = 0;
        
         /* Find the monster to switch with */ 
         new_monster = monster_index[rand_int(monsters_on_level)];
         while (new_monster == c_ptr->m_idx)
         {
            new_monster = monster_index[rand_int(monsters_on_level)];
         }
         m_ptr2 = &mn_list[new_monster];

         /* "Switch" the monsters */
         newx = m_ptr2->fx; 
         newy = m_ptr2->fy; 
         m_ptr2->fx = m_ptr->fx;
         m_ptr2->fy = m_ptr->fy;
         m_ptr->fx = newx; 
         m_ptr->fy = newy; 
         dungeon.level[sublevel][newy][newx].m_idx = c_ptr->m_idx;
         c_ptr->m_idx = new_monster;
         /* make sure they are awake and visisble */
         m_ptr2->csleep = 0;
         update_mon(c_ptr->m_idx, TRUE);
         update_mon(new_monster, TRUE);
dlog(DEBUGMONST,"spells1.c: project_m: m_idx %d (%s) just polymorphed @ %d,%d\n",
                c_ptr->m_idx, r_name+r_ptr->name, x, y);

         /* Hack -- Get new monster */
         m_ptr = m_ptr2;

         /* Hack -- Get new race */
         r_ptr = &r_info[m_ptr->r_idx];
      }
   }

   /* Handle "teleport" */
   else if (do_dist)
   {
       /* Obvious */
       if (seen) obvious = TRUE;

       /* Message */
       note = " disappears!";

       /* Teleport */
       teleport_away(c_ptr->m_idx, do_dist);

       /* Hack -- get new location */
       x = m_ptr->fx;
       y = m_ptr->fy;

       /* Hack -- get new grid */
       c_ptr = &dungeon.level[sublevel][y][x];
   }

   /* Sound and Impact breathers never stun */
   else if (do_stun &&
            !(r_ptr->flags4 & RF4_BR_SOUN) &&
            !(r_ptr->flags4 & RF4_BR_WALL))
   {
      /* Obvious */
      if (seen) obvious = TRUE;

      /* Get confused */
      if (m_ptr->stun)
      {
         note = " is more dazed.";
         i = m_ptr->stun + (do_stun / 2);
      }
      else
      {
         note = " is dazed.";
         i = do_stun;
      }

      /* Apply stun */
      m_ptr->stun = (i < 200) ? i : 200;
   }

   /* Confusion and Chaos breathers (and sleepers) never confuse */
   else if (do_conf &&
           !(r_ptr->flags3 & RF3_NO_CONF) &&
           !(r_ptr->flags4 & RF4_BR_CONF) &&
           !(r_ptr->flags4 & RF4_BR_CHAO))
   {
      /* Obvious */
      if (seen) obvious = TRUE;

      /* Already partially confused */
      if (m_ptr->confused)
      {
         note = " looks more confused.";
         i = m_ptr->confused + (do_conf / 2);
      }

      /* Was not confused */
      else
      {
         note = " looks confused.";
         i = do_conf;
      }

      /* Apply confusion */
      m_ptr->confused = (i < 200) ? i : 200;
   }

   /* Fear */
   if (do_fear)
   {
      /* Increase fear */
      i = m_ptr->afraid + do_fear;

      /* Set fear */
      m_ptr->afraid = (i < 200) ? i : 200;
   }

dlog(DEBUGTRAPS,"spells1.c: project_m: who (type %d trigger %d trigger_race %d index %d index_race %d) dam %d\n",
                 who->type, who->trigger, who->trigger_race, who->index, who->index_race, dam);
   /* If another monster did the damage, hurt the monster by hand */
   if ((who->type == WHO_MONSTER) || (who->type == WHO_TRAPBYMONSTER))
   {
      /* Redraw (later) if needed */
      if (health_who == c_ptr->m_idx) p_ptr->redraw1 |= (PR1_HEALTH);

      /* Wake the monster up */
      m_ptr->csleep = 0;

      /* we were attacked now */
      m_ptr->attacked = 0;

      /* Hurt the monster */
      m_ptr->hp -= dam;

      /* Dead monster */
      if (m_ptr->hp < 0)
      {
         /* Generate treasure, etc */
         monster_death(who, c_ptr->m_idx);

         /* Delete the monster */
dlog(DEBUGMONST,"spells1.c: project_m: m_idx %d (%s) just got killed @ %d,%d\n",
                c_ptr->m_idx, r_name+r_ptr->name, x, y);
         delete_monster_idx(c_ptr->m_idx);

         /* Give detailed messages if destroyed */
         if (note) msg_format("%^s%s", m_name, note);
      }

      /* Damaged monster */
      else
      {
         /* Give detailed messages if visible or destroyed */
         if (note && seen) msg_format("%^s%s", m_name, note);

         /* Hack -- Pain message */
         else if (dam > 0) message_pain(c_ptr->m_idx, dam);

         /* Hack -- handle sleep */
         if (do_sleep && (m_ptr->csleep < do_sleep) ) m_ptr->csleep = do_sleep;
      }
   }

   /* If the player did it, give him experience, check fear */
   else
   {
      bool             fear = FALSE;
      project_who_type who;

      if (wizard)
      {
         msg_format("you do %d damage.",dam);
      }

      /* Hurt the monster, check for fear and death */
      who.type = WHO_PLAYER;
      if (mon_take_hit(&who, c_ptr->m_idx, dam, &fear, note_dies))
      {
         /* Dead monster */
      }
      /* Damaged monster */
      else
      {
         /* Give detailed messages if visible or destroyed */
         if (note && seen) msg_format("%^s%s", m_name, note);

         /* Hack -- Pain message */
         else if (dam > 0) message_pain(c_ptr->m_idx, dam);

         /* Take note */
         if ((fear || do_fear) && (m_ptr->ml))
         {
            /* Sound */
            sound(SOUND_FLEE);

            /* Message */
            msg_format("%^s flees in terror!", m_name);
         }

         /* Hack -- handle sleep */
         if (do_sleep) m_ptr->csleep = do_sleep;
      }
   }

   /* Update the monster XXX XXX XXX */
   update_mon(c_ptr->m_idx, FALSE);

   /* Hack -- Redraw the monster grid anyway */
   lite_spot(x, y);
   
   /* Update monster recall window */
   if (p_ptr->monster_race_idx == m_ptr->r_idx)
   {
      /* Window stuff */
      p_ptr->window |= (PW_MONSTER);
   }


   /* Track it */
   project_m_n++;
   project_m_x = x;
   project_m_y = y;

   /* Return "Anything seen?" */
   return (obvious);
}

/*
 * Helper function for "project()" below.
 *
 * Handle a beam/bolt/ball causing damage to the player.
 *
 * This routine takes a "source monster" (by index), a "distance", a default
 * "damage", and a "damage type".  See "project_m()" above.
 *
 * If "rad" is non-zero, then the blast was centered elsewhere, and the damage
 * is reduced (see "project_m()" above).  This can happen if a monster breathes
 * at the player and hits a wall instead.
 *
 * We return "TRUE" if any "obvious" effects were observed.  XXX XXX Actually,
 * we just assume that the effects were obvious, for historical reasons.
 */
static bool project_p(project_who_type *who, s16b r, s16b x, s16b y, s16b dam, s16b typ)
{
   s16b k = 0;

   s16b div;

   /* Hack -- assume obvious */
   bool obvious = TRUE;

   /* Player blind-ness */
   bool blind = (p_ptr->blind ? TRUE : FALSE);

   /* Player needs a "description" (he is blind) */
   bool fuzzy = FALSE;

   /* Source monster */
   monster_type *m_ptr = NULL;

   /* Monster name (for attacks) */
   char m_name[80];

   /* Monster name (for damage) */
   char killer[80];
   char killed_by[255];

   /* Hack -- messages */
   cptr act = NULL;

dlog(DEBUGPROJ,"spells1.c: project_p: who (type %d trigger %d trigger_race %d index %d index_race %d) r %d @ %d,%d dam %d typ %d\n",
               who->type, who->trigger, who->trigger_race, who->index, who->index_race, r, x, y, dam, typ);

   /* Player is not here */
   if ((x != px) || (y != py)) return (FALSE);

   /* Player cannot hurt himself, unless it's a trap of course */
   if (who->type == WHO_PLAYER) return (FALSE);

   /* Extract radius */
   div = r + 1;

   /* Decrease damage */
   dam = dam / div;

   /* Hack -- always do at least one point of damage */
   if (dam <= 0) dam = 1;

   /* Hack -- Never do excessive damage */
   if (dam > 1600) dam = 1600;

   /* If the player is blind, be more descriptive */
   if (blind) fuzzy = TRUE;

   /* if it's a trap hitting the player, be more descriptive */
   if ((who->type == WHO_TRAPBYMONSTER) || (who->type == WHO_TRAPBYPLAYER)) fuzzy = TRUE;

   if ( who->type == WHO_MONSTER )
   {
      /* Get the source monster */
      m_ptr = &mn_list[who->index];

      /* Get the monster name */
      monster_desc(m_name, m_ptr, 0);

      /* Get the monster's real name */
      monster_desc(killer, m_ptr, 0x88);
      strcpy(killed_by, killer);
   }
   else if ( who->type == WHO_TRAPBYMONSTER )
   {
      /* Get the source monster */
      trap_type *t_ptr = &t_info[who->index];
      char       trap_name[255];
      char       monster_name[255];

      /* Get the monster's real name */
      strcpy(monster_name, r_name + r_info[who->trigger_race].name);
      monster_name[0]=tolower(monster_name[0]);

      strcpy(trap_name, t_name + t_ptr->name);

      sprintf(killed_by, "a%s %s which a%s %s triggered", 
              is_a_vowel(trap_name[0])?"n":"", trap_name,
              is_a_vowel(monster_name[0])?"n":"", monster_name);
   }
   if (who->type == WHO_TRAPBYPLAYER)
   {
      trap_type *t_ptr = &t_info[who->index];
      char       trap_name[255];
      strcpy(trap_name, t_name + t_ptr->name);

      sprintf(killed_by, "a%s %s which you triggered",
              is_a_vowel(trap_name[0])?"n":"", trap_name);
   }

   /* Analyze the damage */
   switch (typ)
   {
      /* Standard damage -- hurts inventory too */
      case GF_ACID:
         if (fuzzy) msg_print("You are hit by acid!");
         acid_dam(dam, killed_by);
         break;

      /* Standard damage -- hurts inventory too */
      case GF_FIRE:
         if (fuzzy) msg_print("You are hit by fire!");
         fire_dam(dam, killed_by);
         break;

      /* Standard damage -- hurts inventory too */
      case GF_COLD:
         if (fuzzy) msg_print("You are hit by cold!");
         cold_dam(dam, killed_by);
         break;

      /* Standard damage -- hurts inventory too */
      case GF_ELEC:
         if (fuzzy) msg_print("You are hit by lightning!");
         elec_dam(dam, killed_by);
         break;

      /* Standard damage -- also poisons player */
      case GF_POIS:
         if (fuzzy) msg_print("You are hit by poison!");
         if (p_ptr->resist_pois) dam = (dam + 2) / 3;
         if (p_ptr->oppose_pois) dam = (dam + 2) / 3;
         take_hit(dam, killed_by);
         if (wizard)
         {
            msg_format("The poison does %d damage",dam);
         }
         if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
         {
            (void)set_poisoned(p_ptr->poisoned + rand_int(dam) + 10);
         }
         (void)poison_dam(dam);
         break;

      /* Standard damage */
      case GF_MISSILE:
         if (fuzzy) msg_print("You are hit by a magic missile!");
         take_hit(dam, killed_by);
         if (wizard)
         {
            msg_format("The missile does %d damage",dam);
         }

         break;

      /* Holy Orb -- Player only takes partial damage */
      case GF_HOLY_ORB:
         if (fuzzy) msg_print("You are hit by divine energy!");
         dam /= 2;
         take_hit(dam, killed_by);
         if (wizard)
         {
            msg_format("The holy orb does %d damage",dam);
         }

         break;

      /* Arrow -- XXX no dodging */
      case GF_ARROW:
         if (fuzzy) msg_print("You are hit by an arrow!");
         take_hit(dam, killed_by);
         if (wizard)
         {
            msg_format("The arrow does %d damage",dam);
         }

         break;

      /* Plasma -- XXX No resist */
      case GF_PLASMA:
         if (fuzzy) msg_print("You are hit by a plasma cloud!");
         take_hit(dam, killed_by);
         if (wizard)
         {
            msg_format("The plasma does %d damage",dam);
         }

         if (!p_ptr->resist_sound)
         {
            s16b k = (randint((dam > 40) ? 35 : (dam * 3 / 4 + 5)));
            (void)set_stun(p_ptr->stun + k);
         }
         break;

      /* Nether -- drain experience */
      case GF_NETHER:
         if (fuzzy) msg_print("You are hit by a nether cloud!");
         if (p_ptr->resist_neth)
         {
            dam *= 6; dam /= (randint(6) + 6);
         }
         else
         {
            if (p_ptr->hold_life && (rand_int(100) < 75))
            {
               msg_print("You keep hold of your life force!");
            }
            else if (p_ptr->hold_life)
            {
               msg_print("You feel your life slipping away!");
               lose_exp(200 + (p_ptr->exp/1000) * MON_DRAIN_LIFE);
            }
            else
            {
               msg_print("You feel your life draining away!");
               lose_exp(200 + (p_ptr->exp/100) * MON_DRAIN_LIFE);
            }
         }
         take_hit(dam, killed_by);
         if (wizard)
         {
            msg_format("The nether does %d damage",dam);
         }

         break;

      /* Water -- stun/confuse */
      case GF_WATER:
         if (fuzzy) msg_print("You are hit by acidic water!");
         if (!p_ptr->resist_sound)
         {
            (void)set_stun(p_ptr->stun + randint(40));
         }
         if (!p_ptr->resist_conf)
         {
            (void)set_confused(p_ptr->confused + randint(5) + 5);
         }
         take_hit(dam, killed_by);
         if (wizard)
         {
            msg_format("The water does %d damage",dam);
         }

         break;

      /* Chaos -- many effects */
      case GF_CHAOS:
         if (fuzzy) msg_print("You are hit by chaos energy!");
         if (p_ptr->resist_chaos)
         {
            dam *= 6; dam /= (randint(6) + 6);
         }
         if (!p_ptr->resist_conf)
         {
            (void)set_confused(p_ptr->confused + rand_int(20) + 10);
         }
         if (!p_ptr->resist_chaos)
         {
            (void)set_image(p_ptr->image + randint(10));
         }
         if (!p_ptr->resist_neth && !p_ptr->resist_chaos)
         {
            if (p_ptr->hold_life && (rand_int(100) < 75))
            {
               msg_print("You keep hold of your life force!");
            }
            else if (p_ptr->hold_life)
            {
               msg_print("You feel your life slipping away!");
               lose_exp(500 + (p_ptr->exp/1000) * MON_DRAIN_LIFE);
            }
            else
            {
               msg_print("You feel your life draining away!");
               lose_exp(5000 + (p_ptr->exp/100) * MON_DRAIN_LIFE);
            }
         }
         take_hit(dam, killed_by);
         if (wizard)
         {
            msg_format("The chaos does %d damage",dam);
         }

         break;

      /* Shards -- mostly cutting */
      case GF_SHARDS:
         if (fuzzy) msg_print("You are hit by shards!");
         if (p_ptr->resist_shard)
         {
            dam *= 6; dam /= (randint(6) + 6);
         }
         else
         {
            (void)set_cut(p_ptr->cut + dam);
         }
         take_hit(dam, killed_by);
         if (wizard)
         {
            msg_format("The shards do %d damage",dam);
         }

         break;

      /* Sound -- mostly stunning */
      case GF_SOUND:
         if (fuzzy) msg_print("You are hit by concentrated sound!");
         if (p_ptr->resist_sound)
         {
            dam *= 5; dam /= (randint(6) + 6);
         }
         else
         {
            s16b k = (randint((dam > 90) ? 35 : (dam / 3 + 5)));
            (void)set_stun(p_ptr->stun + k);
         }
         take_hit(dam, killed_by);
         if (wizard)
         {
            msg_format("The sound does %d damage",dam);
         }

         break;

      /* Pure confusion */
      case GF_CONFUSION:
         if (fuzzy) msg_print("You are hit by confusion!");
         if (p_ptr->resist_conf)
         {
            dam *= 5; dam /= (randint(6) + 6);
         }
         if (!p_ptr->resist_conf)
         {
            (void)set_confused(p_ptr->confused + randint(20) + 10);
         }
         take_hit(dam, killed_by);
         if (wizard)
         {
            msg_format("The confusion does %d damage",dam);
         }

         break;

      /* Disenchantment -- see above */
      case GF_DISENCHANT:
         if (fuzzy) msg_print("You are hit by disenchantment!");
         if (p_ptr->resist_disen)
         {
            dam *= 6; dam /= (randint(6) + 6);
         }
         else
         {
            (void)apply_disenchant(0);
         }
         take_hit(dam, killed_by);
         if (wizard)
         {
            msg_format("The disenchantment does %d damage",dam);
         }

         break;

      /* Nexus -- see above */
      case GF_NEXUS:
         if (fuzzy) msg_print("You are hit by nexus energy!");
         if (p_ptr->resist_nexus)
         {
            dam *= 6; dam /= (randint(6) + 6);
         }
         else
         {
            /* m_ptr may be NULL!! */
            apply_nexus(m_ptr);
         }
         take_hit(dam, killed_by);
         if (wizard)
         {
            msg_format("The nexus does %d damage",dam);
         }

         break;

      /* Force -- mostly stun */
      case GF_FORCE:
         if (fuzzy) msg_print("You are hit by pure force!");
         if (!p_ptr->resist_sound)
         {
            (void)set_stun(p_ptr->stun + randint(20));
         }
         take_hit(dam, killed_by);
         if (wizard)
         {
            msg_format("The force does %d damage",dam);
         }
         break;

      /* Inertia -- slowness */
      case GF_INERTIA:
         if (fuzzy) msg_print("You are hit by inertia!");
         (void)set_slow(p_ptr->slow + rand_int(4) + 4);
         take_hit(dam, killed_by);
         if (wizard)
         {
            msg_format("The inertia does %d damage",dam);
         }
         break;

      /* Lite -- blinding */
      case GF_LITE:
         if (fuzzy) msg_print("You are hit by a blinding light!");
         if (p_ptr->resist_lite)
         {
            dam *= 4; dam /= (randint(6) + 6);
         }
         else if (!blind && !p_ptr->resist_blind)
         {
            (void)set_blind(p_ptr->blind + randint(5) + 2);
         }
         take_hit(dam, killed_by);
         if (wizard)
         {
            msg_format("The light does %d damage",dam);
         }
         break;

      /* Dark -- blinding */
      case GF_DARK:
         if (fuzzy) msg_print("You are hit by a patch of darkness!");
         if (p_ptr->resist_dark)
         {
            dam *= 4; dam /= (randint(6) + 6);
         }
         else if (!blind && !p_ptr->resist_blind)
         {
            (void)set_blind(p_ptr->blind + randint(5) + 2);
         }
         take_hit(dam, killed_by);
         if (wizard)
         {
            msg_format("The dark does %d damage",dam);
         }
         break;

      /* Time -- bolt fewer effects XXX */
      case GF_TIME:
         if (fuzzy) msg_print("You are hit by magic time vortices!");
         switch (randint(10))
         {
            case 1: case 2: case 3: case 4: case 5:
               msg_print("You feel life has clocked back.");
               lose_exp(100 + (p_ptr->exp / 100) * MON_DRAIN_LIFE);
               break;

            case 6: case 7: case 8: case 9:

               switch (randint(6))
               {
                  case 1: k = A_STR; act = "strong"; break;
                  case 2: k = A_INT; act = "bright"; break;
                  case 3: k = A_WIS; act = "wise"; break;
                  case 4: k = A_DEX; act = "agile"; break;
                  case 5: k = A_CON; act = "hale"; break;
                  case 6: k = A_CHR; act = "beautiful"; break;
               }

               msg_format("You're not as %s as you used to be...", act);

               p_ptr->stat_cur[k] = (p_ptr->stat_cur[k] * 3) / 4;
               if (p_ptr->stat_cur[k] < 3) p_ptr->stat_cur[k] = 3;
               p_ptr->update |= (PU_BONUS);
               break;

            case 10:
               msg_print("You're not as powerful as you used to be...");

               for (k = 0; k < 6; k++)
               {
                  p_ptr->stat_cur[k] = (p_ptr->stat_cur[k] * 3) / 4;
                  if (p_ptr->stat_cur[k] < 3) p_ptr->stat_cur[k] = 3;
               }
               p_ptr->update |= (PU_BONUS);
               break;
         }
         take_hit(dam, killed_by);
         if (wizard)
         {
            msg_format("The time does %d damage",dam);
         }

         break;

      /* Gravity -- stun plus slowness plus teleport */
      case GF_GRAVITY:
         if (fuzzy) msg_print("You are hit by gravity!");
         msg_print("Gravity warps around you.");
         teleport_player(5);
         (void)set_slow(p_ptr->slow + rand_int(4) + 4);
         if (!p_ptr->resist_sound)
         {
            s16b k = (randint((dam > 90) ? 35 : (dam / 3 + 5)));
            (void)set_stun(p_ptr->stun + k);
         }
         take_hit(dam, killed_by);
         if (wizard)
         {
            msg_format("The gravity does %d damage",dam);
         }
         break;

      /* Pure damage */
      case GF_MANA:
         if (fuzzy) msg_print("You are hit by pure mana!");
         take_hit(dam, killed_by);
         if (wizard)
         {
            msg_format("The mana does %d damage",dam);
         }
         break;

      /* Pure damage */
      case GF_METEOR:
         if (fuzzy) msg_print("You are hit by a large magic missile!");
         take_hit(dam, killed_by);
         if (wizard)
         {
            msg_format("The meteor does %d damage",dam);
         }
         break;

      /* Ice -- cold plus stun plus cuts */
      case GF_ICE:
         if (fuzzy) msg_print("You are hit by icy shards!");
         cold_dam(dam, killed_by);
         if (!p_ptr->resist_shard)
         {
             (void)set_cut(p_ptr->cut + damroll(5, 8));
         }
         if (!p_ptr->resist_sound)
         {
             (void)set_stun(p_ptr->stun + randint(15));
         }
         break;

      /* Default */
      default:

         /* No damage */
         dam = 0;
         break;
   }

   /* Disturb */
   disturb(1, 0);

   /* Return "Anything seen?" */
   return (obvious);
}

/*
 * Find the attr/char pair to use for a spell effect
 *
 * It is moving (or has moved) from (x,y) to (nx,ny).
 *
 * If the distance is not "one", we (may) return "*".
 */
static u16b bolt_pict(s16b x, s16b y, s16b nx, s16b ny, s16b typ, s16b rad)
{
   s16b  base;
   byte k;

   byte a;
   char c;

   /* ball spell */
   if ( (ny==y) && (nx==x) )
   {
      base = FLAVOR_BALL_START;
   }
   /* Vertical (|) */
   else if (nx == x) base = FLAVOR_BOLTBEAM_VSTART;

   /* Horizontal (-) */
   else if (ny == y) base = FLAVOR_BOLTBEAM_HSTART;

   /* Diagonal (/) */
   else if ((ny-y) == (x-nx)) base = FLAVOR_BOLTBEAM_D1START;

   /* Diagonal (\) */
   else if ((ny-y) == (nx-x)) base = FLAVOR_BOLTBEAM_D2START;

   /* Weird (*) */
   else base = FLAVOR_BOLTBEAM_HSTART;

   /* Basic spell color */
   k = spell_color(typ);

   /* Obtain attr/char */
   a = misc_to_attr[base+k];
   c = misc_to_char[base+k];

   /* Create pict */
   return (PICT(a,c));
}
/*
 * Generic "beam"/"bolt"/"ball" projection routine.  -BEN-
 *
 * Input:
 * jk -1 means from x,y towards player to handle chests *
 * jk -2 means from a trap, is only used with project_jump
 *   rad: Radius of explosion (0 = beam/bolt, 1 to 9 = ball)
 *   x,y: Target location (or location to travel "towards")
 *   dam: Base damage roll to apply to affected monsters (or player)
 *   typ: Type of damage to apply to monsters (and objects)
 *   flg: Extra bit flags (see PROJECT_xxxx in "defines.h")
 *
 * Return:
 *   TRUE if any "effects" of the projection were observed, else FALSE
 *
 * Allows a monster (or player) to project a beam/bolt/ball of a given kind
 * towards a given location (optionally passing over the heads of interposing
 * monsters), and have it do a given amount of damage to the monsters (and
 * optionally objects) within the given radius of the final location.
 *
 * A "bolt" travels from source to target and affects only the target grid.
 * A "beam" travels from source to target, affecting all grids passed through.
 * A "ball" travels from source to the target, exploding at the target, and
 *   affecting everything within the given radius of the target location.
 *
 * Traditionally, a "bolt" does not affect anything on the ground, and does
 * not pass over the heads of interposing monsters, much like a traditional
 * missile, and will "stop" abruptly at the "target" even if no monster is
 * positioned there, while a "ball", on the other hand, passes over the heads
 * of monsters between the source and target, and affects everything except
 * the source monster which lies within the final radius, while a "beam"
 * affects every monster between the source and target, except for the casting
 * monster (or player), and rarely affects things on the ground.
 *
 * Two special flags allow us to use this function in special ways, the
 * "PROJECT_HIDE" flag allows us to perform "invisible" projections, while
 * the "PROJECT_JUMP" flag allows us to affect a specific grid, without
 * actually projecting from the source monster (or player).
 *
 * The player will only get "experience" for monsters killed by himself
 * Unique monsters can only be destroyed by attacks from the player
 *
 * Only 256 grids can be affected per projection, limiting the effective
 * "radius" of standard ball attacks to nine units (diameter nineteen).
 *
 * One can project in a given "direction" by combining PROJECT_THRU with small
 * offsets to the initial location (see "line_spell()"), or by calculating
 * "virtual targets" far away from the player.
 *
 * One can also use PROJECT_THRU to send a beam/bolt along an angled path,
 * continuing until it actually hits somethings (useful for "stone to mud").
 *
 * Bolts and Beams explode INSIDE walls, so that they can destroy doors.
 *
 * Balls must explode BEFORE hitting walls, or they would affect monsters
 * on both sides of a wall.  Some bug reports indicate that this is still
 * happening in 2.7.8 for Windows, though it appears to be impossible.
 *
 * We "pre-calculate" the blast area only in part for efficiency.
 * More importantly, this lets us do "explosions" from the "inside" out.
 * This results in a more logical distribution of "blast" treasure.
 * It also produces a better (in my opinion) animation of the explosion.
 * It could be (but is not) used to have the treasure dropped by monsters
 * in the middle of the explosion fall "outwards", and then be damaged by
 * the blast as it spreads outwards towards the treasure drop location.
 *
 * Walls and doors are included in the blast area, so that they can be
 * "burned" or "melted" in later versions.
 *
 * This algorithm is intended to maximize simplicity, not necessarily
 * efficiency, since this function is not a bottleneck in the code.
 *
 * We apply the blast effect from ground zero outwards, in several passes,
 * first affecting features, then objects, then monsters, then the player.
 * This allows walls to be removed before checking the object or monster
 * in the wall, and protects objects which are dropped by monsters killed
 * in the blast, and allows the player to see all affects before he is
 * killed or teleported away.  The semantics of this method are open to
 * various interpretations, but they seem to work well in practice.
 *
 * We process the blast area from ground-zero outwards to allow for better
 * distribution of treasure dropped by monsters, and because it provides a
 * pleasing visual effect at low cost.
 *
 * Note that the damage done by "ball" explosions decreases with distance.
 * This decrease is rapid, grids at radius "dist" take "1/dist" damage.
 *
 * Notice the "napalm" effect of "beam" weapons.  First they "project" to
 * the target, and then the damage "flows" along this beam of destruction.
 * The damage at every grid is the same as at the "center" of a "ball"
 * explosion, since the "beam" grids are treated as if they ARE at the
 * center of a "ball" explosion.
 *
 * Currently, specifying "beam" plus "ball" means that locations which are
 * covered by the initial "beam", and also covered by the final "ball", except
 * for the final grid (the epicenter of the ball), will be "hit twice", once
 * by the initial beam, and once by the exploding ball.  For the grid right
 * next to the epicenter, this results in 150% damage being done.  The center
 * does not have this problem, for the same reason the final grid in a "beam"
 * plus "bolt" does not -- it is explicitly removed.  Simply removing "beam"
 * grids which are covered by the "ball" will NOT work, as then they will
 * receive LESS damage than they should.  Do not combine "beam" with "ball".
 *
 * The array "gy[],gx[]" with current size "grids" is used to hold the
 * collected locations of all grids in the "blast area" plus "beam path".
 *
 * Note the rather complex usage of the "gm[]" array.  First, gm[0] is always
 * zero.  Second, for N>1, gm[N] is always the index (in gy[],gx[]) of the
 * first blast grid (see above) with radius "N" from the blast center.  Note
 * that only the first gm[1] grids in the blast area thus take full damage.
 * Also, note that gm[rad+1] is always equal to "grids", which is the total
 * number of blast grids.
 *
 * Note that once the projection is complete, (y2,x2) holds the final location
 * of bolts/beams, and the "epicenter" of balls.
 *
 * Note also that "rad" specifies the "inclusive" radius of projection blast,
 * so that a "rad" of "one" actually covers 5 or 9 grids, depending on the
 * implementation of the "distance" function.  Also, a bolt can be properly
 * viewed as a "ball" with a "rad" of "zero".
 *
 * Note that if no "target" is reached before the beam/bolt/ball travels the
 * maximum distance allowed (MAX_RANGE), no "blast" will be induced.  This
 * may be relevant even for bolts, since they have a "1x1" mini-blast.
 *
 * Some people have requested an "auto-explode ball attacks at max range"
 * option, which should probably be handled by this function.  XXX XXX XXX
 *
 * Note that for consistency, we "pretend" that the bolt actually takes "time"
 * to move from point A to point B, even if the player cannot see part of the
 * projection path.  Note that in general, the player will *always* see part
 * of the path, since it either starts at the player or ends on the player.
 *
 * Hack -- we assume that every "projection" is "self-illuminating".
 */
bool project(project_who_type *who, s16b rad, s16b x, s16b y, s16b dam, s16b typ, u16b flg)
{
   s16b                 i, t, dist;
   s16b                 y1, x1, y2, x2;
   s16b                 y0, x0;

   int msec = delay_spd * delay_spd;

   /* Assume the player sees nothing */
   bool notice = FALSE;

   /* Assume the player has seen nothing */
   bool visual = FALSE;

   /* Assume the player has seen no blast grids */
   bool drawn = FALSE;

   /* Is the player blind? */
   bool blind = (p_ptr->blind ? TRUE : FALSE);

   /* Number of grids in the "path" */
   int path_n = 0;

   /* Actual grids in the "path" */
   u16b path_g[512];

   /* Number of grids in the "blast area" (including the "beam" path) */
   s16b grids = 0;

   /* Coordinates of the affected grids */
/* jk - these were bytes [256] */
   s16b gx[512], gy[512];

   /* Encoded "radius" info (see above) */
   byte gm[16];

   /* Location of player */
   x0 = px;
   y0 = py;

   /* Hack -- Jump to target */
   if (flg & PROJECT_JUMP)
   {
      x1 = x;
      y1 = y;
   }

   /* Hack -- Start at player */
   else if ((who->type == WHO_PLAYER) || (who->type == WHO_TRAPBYPLAYER))
   {
      x1 = px;
      y1 = py;
   }

   /* Start at a monster */
   else
   {
      x1 = mn_list[who->index].fx;
      y1 = mn_list[who->index].fy;
   }
dlog(DEBUGPROJ, "spells1.c: project: starting at %d,%d px,py %d,%d\n", x1,y1,px,py);
   /* Default "destination" */
   y2 = y; x2 = x;

   /* Hack -- verify stuff */
   if (flg & PROJECT_THRU)
   {
      if ((x1 == x2) && (y1 == y2))
      {
         flg &= ~PROJECT_THRU;
      }
   }

   /* Hack -- Assume there will be no blast (max radius 16) */
   for (dist = 0; dist < 16; dist++) gm[dist] = 0;

   /* Initial grid */
   x = x1;
   y = y1;

   /* Collect beam grids */
   if (flg & (PROJECT_BEAM))
   {
      gx[grids] = x;
      gy[grids] = y;
      grids++;
   }


   /* Calculate the projection path */
dlog(DEBUGPROJ,"spells1.c: project: calling project_path range %d x1,y1 %d,%d x2,y2 %d,%d\n", MAX_RANGE,
              x1,y1,x2,y2);
   path_n = project_path(path_g, MAX_RANGE, x1, y1, x2, y2, flg);

   /* Hack -- Handle stuff */
   handle_stuff();
dlog(DEBUGPROJ,"spells1.c: project: path computed, now expanding\n");
   /* Project along the path */
   for (i = 0; i < path_n; ++i)
   {
      int ox = x;
      int oy = y;

      int nx = GRID_X(path_g[i]);
      int ny = GRID_Y(path_g[i]);
dlog(DEBUGPROJ,"spells1.c: project: path computed, now at %d,%d bold %ld\n", nx, ny, floor_grid_bold(nx, ny));
      /* Hack -- Balls explode before reaching walls */
      if (!floor_grid_bold(nx, ny) && (rad > 0)) break;

      /* Advance */
      x = nx;
      y = ny;

      /* Collect beam grids */
      if (flg & (PROJECT_BEAM))
      {
         gx[grids] = x;
         gy[grids] = y;
         grids++;
      }

      /* Only do visuals if requested */
      if (!blind && !(flg & (PROJECT_HIDE)))
      {
         /* Only do visuals if the player can "see" the bolt */
         if (panel_contains(x, y) && player_has_los_bold(x, y))
         {
            u16b p;

            byte a;
            char c;

            /* Obtain the bolt pict */
            p = bolt_pict(ox, oy, x, y, typ, rad);

            /* Extract attr/char */
            a = PICT_A(p);
            c = PICT_C(p);

            /* Visual effects */
            print_rel(c, a, x, y);
            move_cursor_relative(x, y);
            if (fresh_before) Term_fresh();
            Term_xtra(TERM_XTRA_DELAY, msec);
            lite_spot(x, y);
            if (fresh_before) Term_fresh();

            /* Display "beam" grids */
            if (flg & (PROJECT_BEAM))
            {
               /* Obtain the explosion pict */
               p = bolt_pict(x, y, x, y, typ, rad);

               /* Extract attr/char */
               a = PICT_A(p);
               c = PICT_C(p);

               /* Visual effects */
               print_rel(c, a, x, y);
            }

            /* Hack -- Activate delay */
            visual = TRUE;
         }

         /* Hack -- delay anyway for consistency */
         else if (visual)
         {
            /* Delay for consistency */
            Term_xtra(TERM_XTRA_DELAY, msec);
         }
      }
   }

   /* Save the "blast epicenter" */
   x2 = x;
   y2 = y;

   /* Start the "explosion" */
   gm[0] = 0;
dlog(DEBUGPROJ,"spells1.c: project: starting explosion at %d,%d\n", x2, y2);

   /* Hack -- make sure beams get to "explode" */
   gm[1] = grids;

   /* If we found a "target", explode there */
   if (dist <= MAX_RANGE)
   {
      /* Mega-Hack -- remove the final "beam" grid */
      if ((flg & PROJECT_BEAM) && (grids > 0)) grids--;

      /* Determine the blast area, work from the inside out */
      for (dist = 0; dist <= rad; dist++)
      {
         /* Scan the maximal blast area of radius "dist" */
         for (y = y2 - dist; y <= y2 + dist; y++)
         {
            for (x = x2 - dist; x <= x2 + dist; x++)
            {
               /* Ignore "illegal" locations */
               if (!in_bounds2(x, y)) continue;

               /* Enforce a "circular" explosion */
               if (distance(x2, y2, x, y) != dist) continue;

               /* Ball explosions are stopped by walls */
               if (!los(x2, y2, x, y)) continue;

               /* Save this grid */
               gy[grids] = y;
               gx[grids] = x;
               grids++;
            }
         }

         /* Encode some more "radius" info */
         gm[dist+1] = grids;
      }
   }

   /* Speed -- ignore "non-explosions" */
   if (!grids) return (FALSE);
dlog(DEBUGPROJ,"spells1.c: project: explosion occured\n");

   /* Display the "blast area" */
   if (!blind && !(flg & PROJECT_HIDE))
   {
      /* Then do the "blast", from inside out */
      for (t = 0; t <= rad; t++)
      {
         /* Dump everything with this radius */
         for (i = gm[t]; i < gm[t+1]; i++)
         {
            /* Extract the location */
            x = gx[i];
            y = gy[i];

            /* The player can see it */
            if (player_has_los_bold(x, y) &&
                panel_contains(x, y))
            {
               drawn = TRUE;
               print_rel('*', spell_color(typ), x, y);
            }
         }

         /* Hack -- center the cursor */
         move_cursor_relative(x2, y2);

         /* Flush each "radius" seperately */
         Term_fresh();

         /* Delay (efficiently) */
         if (visual || drawn) delay(10 * delay_spd);
      }

      /* Flush the erasing */
      if (drawn)
      {
         /* Erase the explosion drawn above */
         for (i = 0; i < grids; i++)
         {
            /* Extract the location */
            x = gx[i];
            y = gy[i];

            /* Erase if needed */
            if (player_has_los_bold(x, y) &&
                panel_contains(x, y))
            {
               lite_spot(x, y);
            }
         }

         /* Hack -- center the cursor */
         move_cursor_relative(x2, y2);

         /* Flush the explosion */
         Term_fresh();
      }
   }

   /* Check features */
   if (flg & PROJECT_GRID)
   {
dlog(DEBUGPROJ,"spells1.c: project: about to affect grids\n");
      /* Start with "dist" of zero */
      dist = 0;

      /* Now hurt the cave grids (and objects) from the inside out */
      for (i = 0; i < grids; i++)
      {
         /* Hack -- Notice new "dist" values */
         if (gm[dist+1] == i) dist++;

         /* Get the grid location */
         x = gx[i];
         y = gy[i];

         /* Affect the feature */
         if (project_f(who, dist, x, y, dam, typ)) notice = TRUE;
      }
   }

   /* Check objects */
   if (flg & PROJECT_ITEM)
   {
dlog(DEBUGPROJ,"spells1.c: project: about to affect items\n");
      /* Start with "dist" of zero */
      dist = 0;

      /* Now hurt the cave grids (and objects) brom the inside out */
      for (i = 0; i < grids; i++)
      {
         /* Hack -- Notice new "dist" values */
         if (gm[dist+1] == i) dist++;

         /* Get the grid location */
         x = gx[i];
         y = gy[i];
dlog(DEBUGPROJ,"spells1.c: items: i %d x %d y %d\n", i, x, y);

         /* Affect the object */
         if (project_i(who, dist, x, y, dam, typ)) notice = TRUE;
dlog(DEBUGPROJ,"spells1.c: items: project_i called\n");
      }
   }

   /* Check monsters */
   if (flg & PROJECT_KILL_MONSTER)
   {
dlog(DEBUGPROJ,"spells1.c: project: about to affect monsters\n");
      /* Start with "dist" of zero */
      dist = 0;

      /* Mega-Hack */
      project_m_n = 0;
      project_m_x = 0;
      project_m_y = 0;

      /* Now hurt the monsters, from inside out */
      for (i = 0; i < grids; i++)
      {
         /* Hack -- Notice new "dist" values */
         if (gm[dist+1] == i) dist++;

         /* Get the grid location */
         x = gx[i];
         y = gy[i];

         /* Walls protect monsters */
         if (!floor_grid_bold(x,y)) continue;

         /* Affect thf monster */
         if (project_m(who, dist, x, y, dam, typ)) notice = TRUE;
      }

      /* Mega-Hack */
      if ( ( (who->type == WHO_PLAYER) || (who->type == WHO_TRAPBYPLAYER) ) && (project_m_n == 1))
      {
         /* Location */
         x = project_m_x;
         y = project_m_y;

         /* Still here */
         if (dungeon.level[sublevel][y][x].m_idx)
         {
            monster_type *m_ptr = &mn_list[dungeon.level[sublevel][y][x].m_idx];

            /* Hack -- auto-recall */
            if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

            /* Hack - auto-track */
            if (m_ptr->ml) health_track(dungeon.level[sublevel][y][x].m_idx);
         }
      }
   }

   /* Check player */
   if (flg & PROJECT_KILL_PLAYER)
   {
dlog(DEBUGPROJ,"spells1.c: project: about to affect player with who (type %d trigger %d trigger_race %d index %d index_race %d)\n",
               who->type, who->trigger, who->trigger_race, who->index, who->index_race);
      /* Start with "dist" of zero */
      dist = 0;

      /* Now see if the player gets hurt */
      for (i = 0; i < grids; i++)
      {
         /* Hack -- Notice new "dist" values */
         if (gm[dist+1] == i) dist++;

         /* Get the grid location */
         x = gx[i];
         y = gy[i];

         /* Affect the player */
         if (project_p(who, dist, x, y, dam, typ)) notice = TRUE;
      }
   }

   /* Return "something was noticed" */
   return (notice);
}
