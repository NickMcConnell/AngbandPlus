/* File: traps.c */

/* Purpose: handle traps */

/* the below copyright probably still applies, but it is heavily changed
 * copied, adapted & re-engineered by JK.
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

bool do_player_trap_call_out(void)
{
   s16b          i,sn,cx,cy;
   s16b          h_index = 0;
   s16b          h_level = 0;
   monster_type  *m_ptr;
   monster_race  *r_ptr;
   char          m_name[80];
   bool          ident = FALSE;

   for (i = 1; i < m_max; i++)
   {
       m_ptr = &m_list[i];
       r_ptr = &r_info[m_ptr->r_idx];

       /* Paranoia -- Skip dead monsters */
       if (!m_ptr->r_idx) continue;

       if (r_ptr->level>=h_level)
       {
         h_level = r_ptr->level;
         h_index = i;
       }
   }
   /* if the level is empty of monsters, h_index will be 0 */
   if (!h_index) return(FALSE);

   m_ptr = &m_list[h_index];

   sn = 0;
   for (i = 0; i < 8; i++)
   {
      cx = px + ddx[i];
      cy = py + ddy[i];
      /* Skip non-empty grids */
      if (!cave_valid_bold(cx, cy)) continue;
      if (cave[cy][cx].feat == FEAT_GLYPH) continue;
      if ((cx==px) && (cy==py)) continue;
      sn++;
      /* Randomize choice */
      if (rand_int(sn) > 0) continue;
      cave[cy][cx].m_idx=h_index;
      cave[m_ptr->fy][m_ptr->fx].m_idx=0;
      m_ptr->fx = cx;
      m_ptr->fy = cy;
      /* we do not change the sublevel! */
      ident=TRUE;
      update_mon(h_index, TRUE);
      monster_desc(m_name, m_ptr, 0x08);
      msg_format("You hear a rapid-shifting wail, and %s appears!",m_name);
      break;
   }
   return (ident);
}

static bool do_trap_teleport_away(object_type *i_ptr, s16b x, s16b y)
{
	bool ident = FALSE;
	char o_name[80];

	s16b  x1  = rand_int(cur_wid);
	s16b  y1  = rand_int(cur_hgt);
	
	if (i_ptr == NULL) return(FALSE);

	i_ptr->ix = x1;
	i_ptr->iy = y1;
	
	cave[y1][x1].o_idx=cave[y][x].o_idx;
	cave[y][x].o_idx=0;

	if (!p_ptr->blind)
	{
		note_spot(y,x);
		lite_spot(y,x);
		ident=TRUE;
		object_desc(o_name, i_ptr, FALSE, 0);
		if (player_has_los_bold(y1, x1))
		{
			lite_spot(y1, x1);
			msg_format("The %s suddenly stands elsewhere", o_name);
			
		}
		else
		{
			msg_format("You suddenly don't see the %s anymore!", o_name);
		}
	}
	else
	{
		msg_print("You hear something move");
	}
	return (ident);
}

/*
 * this handles a trap that places walls around the player
 */
static bool player_handle_trap_of_walls(void)
{
   bool ident;

   {
      s16b dx,dy,cx,cy;
      s16b sx=0,sy=0,sn,i;
      cave_type *cv_ptr;
      bool map[5][5] = { {FALSE,FALSE,FALSE,FALSE,FALSE},
                         {FALSE,FALSE,FALSE,FALSE,FALSE},
                         {FALSE,FALSE,FALSE,FALSE,FALSE},
                         {FALSE,FALSE,FALSE,FALSE,FALSE},
                         {FALSE,FALSE,FALSE,FALSE,FALSE} };
      for (dy = -2; dy <= 2; dy++)
      {
         for (dx = -2; dx <= 2; dx++)
         {
            cx = px+dx;
            cy = py+dy;
            if (!in_bounds(cx, cy)) continue;
            cv_ptr = &cave[cy][cx];
            /* Lose room and vault */
            cv_ptr->info &= ~(CAVE_ROOM | CAVE_ICKY);
            /* Lose light and knowledge */
            cv_ptr->info &= ~(CAVE_GLOW | CAVE_MARK);
            /* Skip the center */
            if (!dx && !dy) continue;
            /* test for dungeon level */
            if (randint(100) > 10+max_dlv[dungeon_type]) continue;
            /* Damage this grid */
            map[2+dx][2+dy] = TRUE;
         }
      }
      for (dy = -2; dy <= 2; dy++)
      {
         for (dx = -2; dx <= 2; dx++)
         {
            cx = px+dx;
            cy = py+dy;
            if (!map[2+dx][2+dy]) continue;
            cv_ptr = &cave[cy][cx];

            if (cv_ptr->m_idx)
            {
               monster_type *m_ptr = &m_list[cv_ptr->m_idx];
               monster_race *r_ptr = &r_info[m_ptr->r_idx];
               /* Most monsters cannot co-exist with rock */
               if (!(r_ptr->flags2 & RF2_KILL_WALL) &&
                   !(r_ptr->flags2 & RF2_PASS_WALL))
               {
                  char m_name[80];

                  /* Assume not safe */
                  sn = 0;
                  /* Monster can move to escape the wall */
                  if (!(r_ptr->flags1 & RF1_NEVER_MOVE))
                  {
                     /* Look for safety */
                     for (i = 0; i < 8; i++)
                     {
                        /* Access the grid */
                        cy = py + ddy[i];
                        cx = px + ddx[i];

                        /* Skip non-empty grids */
                        if (!cave_clean_bold(cx, cy)) continue;

                        /* Hack -- no safety on glyph of warding */
			if (cave[cy][cx].feat == FEAT_GLYPH) continue;

                        /* Important -- Skip "quake" grids */
                        if (map[2+(cx-px)][2+(cy-py)]) continue;
                        /* Count "safe" grids */
                        sn++;
                        /* Randomize choice */
                        if (rand_int(sn) > 0) continue;
                        /* Save the safe grid */
                        sx = cx; sy = cy;
                        ident=TRUE;
                        break; /* discontinue for loop - safe grid found */
                     }
                  }
                  /* Describe the monster */
                  monster_desc(m_name, m_ptr, 0);
                  /* Scream in pain */
                  msg_format("%^s wails out in pain!", m_name);
                  /* Monster is certainly awake */
                  m_ptr->csleep = 0;
                  /* Apply damage directly */
                  m_ptr->hp -= (sn ? damroll(4, 8) : 200);
                  /* Delete (not kill) "dead" monsters */
                  if (m_ptr->hp < 0)
                  {
                     /* Message */
                     msg_format("%^s is entombed in the rock!", m_name);
                     /* Delete the monster */
                     delete_monster_idx(cave[cy][cx].m_idx);
                     /* No longer safe */
                     sn = 0;
                  }
                  /* Hack -- Escape from the rock */
                  if (sn)
                  {
                     s16b m_idx = cave[cy][cx].m_idx;
                     /* Update the new location */
                     cave[sy][sx].m_idx = m_idx;
                     /* Update the old location */
                     cave[cy][cx].m_idx = 0;
                     /* Move the monster */
                     m_ptr->fy = sy;
                     m_ptr->fx = sx;
                     /* do not change fz */
                     /* don't make rock on that square! */
                     if ( (sx>=(px-2)) &&
                        (sx<=(px+2)) &&
                        (sy>=(py-2)) &&
                        (sy<=(py+2)) )
                     {
                        map[2+(sx-px)][2+(sy-py)]=FALSE;
                     }
                     /* Update the monster (new location) */
                     update_mon(m_idx, TRUE);
                     /* Redraw the old grid */
                     lite_spot(cx, cy);
                     /* Redraw the new grid */
                     lite_spot(sx, sy);
                  } /* if sn */
               } /* if monster can co-exist with rock */
            } /* if monster on square */
         } /* for dx */
      } /* for dy */

      /* Examine the quaked region */
      for (dy = -2; dy <= 2; dy++)
      {
         for (dx = -2; dx <= 2; dx++)
         {
            /* Extract the location */
            cx = px + dx;
            cy = py + dy;

            /* Skip unaffected grids */
            if (!map[2+dx][2+dy]) continue;

            /* Access the cave grid */
            cv_ptr = &cave[cy][cx];

            /* Paranoia -- never affect player */
            if (!dy && !dx) continue;

            /* Destroy location (if valid) */
            if ((cx < cur_wid) && (cy < cur_hgt) && cave_valid_bold(cy, cx))
            {
               bool floor = (f_info[cave[cy][cx].feat].flags1 & FF1_FLOOR);

               /* Delete any object that is still there */
               delete_object(cx, cy);

               if (floor)
               {
		  cave[cy][cx].feat = FEAT_WALL_OUTER;
               }
               else
               {
                  /* Clear previous contents, add floor */
		  cave[cy][cx].feat = FEAT_FLOOR;
               }
            } /* valid */
         } /* dx */
      } /* dy */
      /* Mega-Hack -- Forget the view and lite */
      p_ptr->update |= PU_UN_VIEW;

      /* Update stuff */
      p_ptr->update |= (PU_VIEW | PU_FLOW);

      /* Update the monsters */
      p_ptr->update |= (PU_DISTANCE);

      /* Update the health bar */
      p_ptr->redraw |= (PR_HEALTH);

      /* Redraw map */
      p_ptr->redraw |= (PR_MAP);

      /* Window stuff */
      p_ptr->window |= (PW_OVERHEAD);
      handle_stuff();
      msg_print("Suddenly the cave shifts around you. The air is getting stale!");
      ident=TRUE;
   }
   return (ident);
}


/*
 * this function handles arrow & dagger traps, in various types.
 * num = number of missiles
 * tval, sval = kind of missiles
 * dd,ds = damage roll for missiles
 * poison_dam = additional poison damage
 * name = name given if you should die from it...
 *
 * return value = ident (always TRUE)
 */
static bool player_handle_missile_trap(s16b num, s16b tval, s16b sval, s16b dd, s16b ds,
                                     s16b pdam, cptr name)
{
   object_type *o_ptr, forge;
   s16b        i, k_idx = lookup_kind(tval, sval);
   char        i_name[80];

   o_ptr = &forge;
   object_prep(o_ptr, k_idx);
   o_ptr->number = num;
   apply_magic(o_ptr, max_dlv[dungeon_type], FALSE, FALSE, FALSE, FALSE);
   object_desc(i_name, o_ptr, TRUE, 0);

   if (num == 1)
      msg_format("Suddenly %s hits you!", i_name);
   else
      msg_format("Suddenly %s hit you!", i_name);

   for (i=0; i < num; i++)
   {
      take_hit(damroll(dd, ds), name);
      redraw_stuff();
   }
   
   drop_near(o_ptr, -1, py, px);
   
   return TRUE;
}

/*
 * this function handles a "breath" type trap - acid bolt, lightning balls etc.
 */
static bool player_handle_breath_trap(s16b rad, s16b type, u16b trap)
{
   trap_type *t_ptr = &t_info[trap];
   bool       ident;
   s16b       my_dd, my_ds, dam;

   my_dd = t_ptr->dd;
   my_ds = t_ptr->ds;

   /* these traps gets nastier as levels progress */
   if (max_dlv[dungeon_type]> (2* t_ptr->minlevel))
   {
      my_dd += (max_dlv[dungeon_type] / 15);
      my_ds += (max_dlv[dungeon_type] / 15);
   }
   dam = damroll(my_dd, my_ds);

   ident = project(-2, rad, py, px, dam, type, PROJECT_KILL | PROJECT_JUMP);
   return (ident);
}

/*
 * This function damages the player by a trap
 */
static void trap_hit(s16b trap)
{
   s16b dam;
   trap_type *t_ptr = &t_info[trap];
   
   dam = damroll(t_ptr->dd, t_ptr->ds);
   
   take_hit(dam, t_name + t_ptr->name);
}

/* Player activated a trap. */
/* As of Portralis 0.2, this does NOTHING. Will need to be entirely recoded! */
bool player_activate_trap_type(s16b y, s16b x, object_type *i_ptr, s16b item)
{
   bool ident = FALSE;
   s16b trap;

   s16b k, l;

   return (FALSE);
}

void player_activate_door_trap(s16b y, s16b x)
{
	cave_type *c_ptr;
	bool ident = FALSE;
	
	c_ptr = &cave[y][x];
	
	/* Return if trap or door not found */
	if ((c_ptr->t_idx == 0) || 
	    !(f_info[c_ptr->feat].flags1 & FF1_DOOR)) return;
	
	/* Disturb */
	disturb(0, 0);
	
	/* Message */
	msg_print("You found a trap!");
	
	/* Pick a trap */
	pick_trap(y, x);
	
	/* Hit the trap */
	ident = player_activate_trap_type(y, x, NULL, -1);
	if (ident)
	{
		t_info[c_ptr->t_idx].ident = TRUE;
                msg_format("You identified that trap as %s.",
			   t_name + t_info[c_ptr->t_idx].name);
	}
}

/*
 * Places a random trap at the given location.
 *
 * The location must be a valid, empty, clean, floor grid.
 */
void place_trap(int y, int x)
{
   bool           more       = TRUE;
   s16b           trap;
   trap_type     *t_ptr;
   s16b           cnt        = 0;
   u32b flags;
   cave_type *c_ptr = &cave[y][x];

   /* no traps in town or on first level */
   if (dun_level<=1) return;

   /* traps only appears on empty floor */
   if (!cave_floor_grid(c_ptr) && (!(f_info[c_ptr->feat].flags1 & FF1_DOOR))) return;
   
   /* set flags */
   if (f_info[c_ptr->feat].flags1 & FF1_DOOR)
      flags = FTRAP_DOOR;
   else flags = FTRAP_FLOOR;
   
   /* try 100 times */
   while ((more) && (cnt++)<100)
   {
      trap = randint(max_t_idx - 1);
      t_ptr = &t_info[trap];
      
      /* no traps below their minlevel */
      if (t_ptr->minlevel>dun_level) continue;
      
      /* is this a correct trap now?   */
      if (!(t_ptr->flags & flags)) continue;

      /* how probable is this trap   */
      if (rand_int(100)<t_ptr->probability)
      {
	 c_ptr->t_idx = trap;
	 more = TRUE;
      }
   }

   return;
}

/*
 * Places a random trap on the given chest.
 *
 * The object must be a valid chest.
 */
void place_trap_object(object_type *o_ptr)
{
   bool           more       = TRUE;
   s16b           trap;
   trap_type     *t_ptr;
   s16b           cnt        = 0;

   /* no traps in town or on first level */
   if (dun_level<=1) return;
   
   /* try 100 times */
   while ((more) && (cnt++)<100)
   {
      trap = randint(max_t_idx - 1);
      t_ptr = &t_info[trap];
      
      /* no traps below their minlevel */
      if (t_ptr->minlevel>dun_level) continue;
      
      /* is this a correct trap now?   */
      if (!(t_ptr->flags & FTRAP_CHEST)) continue;

      /* how probable is this trap   */
      if (rand_int(100)<t_ptr->probability)
      {
         o_ptr->pval = trap;
	 more = TRUE;
      }
   }

   return;
}
