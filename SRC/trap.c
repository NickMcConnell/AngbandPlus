/* File: object3.c */

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

/*
 * Let the floor carry an object
 */
static s16b floor_carry(int y, int x, object_type *j_ptr)
{
	int n = 0;

	s16b o_idx;

	s16b this_o_idx, next_o_idx = 0;

	/* Scan objects in that grid for combination */
        for (this_o_idx = cave[y][x].o_idx; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Acquire object */
		o_ptr = &o_list[this_o_idx];

		/* Acquire next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Check for combination */
		if (object_similar(o_ptr, j_ptr))
		{
			/* Combine the items */
			object_absorb(o_ptr, j_ptr);

			/* Result */
			return (this_o_idx);
		}

		/* Count objects */
		n++;
	}


	/* Make an object */
	o_idx = o_pop();

	/* Success */
	if (o_idx)
	{
		object_type *o_ptr;

		/* Acquire object */
		o_ptr = &o_list[o_idx];

		/* Structure Copy */
		object_copy(o_ptr, j_ptr);

		/* Location */
		o_ptr->iy = y;
		o_ptr->ix = x;

		/* Forget monster */
		o_ptr->held_m_idx = 0;

		/* Build a stack */
                o_ptr->next_o_idx = cave[y][x].o_idx;

		/* Place the object */
                cave[y][x].o_idx = o_idx;

		/* Notice */
		note_spot(y, x);

		/* Redraw */
		lite_spot(y, x);
	}

	/* Result */
	return (o_idx);
}

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
            if (randint(100) > 10+p_ptr->max_dlv) continue;
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
   apply_magic(o_ptr, p_ptr->max_dlv, FALSE, FALSE, FALSE);
   object_desc(i_name, o_ptr, TRUE, 0);

   if (num == 1)
      msg_format("Suddenly %s hits you!", i_name);
   else
      msg_format("Suddenly %s hit you!", i_name);

   for (i=0; i < num; i++)
   {
      take_hit(damroll(dd, ds), name);
      redraw_stuff();
      if (pdam > 0)
      {
        if (((p_resist_pois) || p_ptr->oppose_pois) && (randint(3)==1))
         {
            (void)set_poisoned(p_ptr->poisoned + pdam);
         }
      set_poisoned(pdam);
      }
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
   if (p_ptr->max_dlv > (2* t_ptr->minlevel))
   {
      my_dd += (p_ptr->max_dlv / 15);
      my_ds += (p_ptr->max_dlv / 15);
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

/*
 * this function activates one trap type, and returns
 * a bool indicating if this trap is now identified
 */
bool player_activate_trap_type(s16b y, s16b x, object_type *i_ptr, s16b item)
{
   bool ident = FALSE;
   s16b trap;

   s16b k, l;
   
   trap = cave[y][x].t_idx;
   
   if (i_ptr == NULL)
   {
      if (cave[y][x].o_idx==0) i_ptr=NULL;
      else i_ptr=&o_list[cave[y][x].o_idx];
   }
   else
      trap=i_ptr->pval;
   
   switch(trap)
   {
      /* stat traps */

      case TRAP_OF_WEAKNESS_I:       ident=do_dec_stat(A_STR, STAT_DEC_TEMPORARY); break;
      case TRAP_OF_WEAKNESS_II:      ident=do_dec_stat(A_STR, STAT_DEC_NORMAL); break;
      case TRAP_OF_WEAKNESS_III:     ident=do_dec_stat(A_STR, STAT_DEC_PERMANENT); break;
      case TRAP_OF_INTELLIGENCE_I:   ident=do_dec_stat(A_INT, STAT_DEC_TEMPORARY); break;
      case TRAP_OF_INTELLIGENCE_II:  ident=do_dec_stat(A_INT, STAT_DEC_NORMAL); break;
      case TRAP_OF_INTELLIGENCE_III: ident=do_dec_stat(A_INT, STAT_DEC_PERMANENT); break;
      case TRAP_OF_WISDOM_I:         ident=do_dec_stat(A_WIS, STAT_DEC_TEMPORARY); break;
      case TRAP_OF_WISDOM_II:        ident=do_dec_stat(A_WIS, STAT_DEC_NORMAL); break;
      case TRAP_OF_WISDOM_III:       ident=do_dec_stat(A_WIS, STAT_DEC_PERMANENT); break;
      case TRAP_OF_FUMBLING_I:       ident=do_dec_stat(A_DEX, STAT_DEC_TEMPORARY); break;
      case TRAP_OF_FUMBLING_II:      ident=do_dec_stat(A_DEX, STAT_DEC_NORMAL); break;
      case TRAP_OF_FUMBLING_III:     ident=do_dec_stat(A_DEX, STAT_DEC_PERMANENT); break;
      case TRAP_OF_WASTING_I:        ident=do_dec_stat(A_CON, STAT_DEC_TEMPORARY); break;
      case TRAP_OF_WASTING_II:       ident=do_dec_stat(A_CON, STAT_DEC_NORMAL); break;
      case TRAP_OF_WASTING_III:      ident=do_dec_stat(A_CON, STAT_DEC_PERMANENT); break;
      case TRAP_OF_BEAUTY_I:         ident=do_dec_stat(A_CHR, STAT_DEC_TEMPORARY); break;
      case TRAP_OF_BEAUTY_II:        ident=do_dec_stat(A_CHR, STAT_DEC_NORMAL); break;
      case TRAP_OF_BEAUTY_III:       ident=do_dec_stat(A_CHR, STAT_DEC_PERMANENT); break;

      /* Trap of Curse Weapon */
      case TRAP_OF_CURSE_WEAPON:
         ident = curse_weapon();
         break;
      /* Trap of Curse Armor */
      case TRAP_OF_CURSE_ARMOR:
         ident = curse_armor();
         break;
      /* Earthquake Trap */
      case TRAP_OF_EARTHQUAKE:
         msg_print("As you touch the trap, the ground starts to shake.");
         earthquake(y, x, 10);
         ident=TRUE;
         break;
      /* Poison Needle Trap */
      case TRAP_OF_POISON_NEEDLE:
         if (!(p_resist_pois || p_ptr->oppose_pois))
         {
            msg_print("You prick yourself on a poisoned needle.");
            (void)set_poisoned(p_ptr->poisoned + rand_int(15) + 10);
            ident=TRUE;
         }
         else
         {
            msg_print("You prick yourself on a needle.");
         }
         break;
      /* Summon Monster Trap */
      case TRAP_OF_SUMMON_MONSTER:
         {
            msg_print("A spell hangs in the air.");
            for (k = 0; k < randint(3); k++) ident |= summon_specific(y, x, p_ptr->max_dlv, 0, TRUE, FALSE, FALSE);
            break;
         }
      /* Summon Undead Trap */
      case TRAP_OF_SUMMON_UNDEAD:
         {
            msg_print("A mighty spell hangs in the air.");
            for (k = 0; k < randint(3); k++) ident |= summon_specific(y, x, p_ptr->max_dlv, SUMMON_UNDEAD, TRUE, FALSE, FALSE);
            break;
         }
      /* Summon Greater Undead Trap */
      case TRAP_OF_SUMMON_GREATER_UNDEAD:
         {
            msg_print("An old and evil spell hangs in the air.");
            for (k = 0; k < randint(3); k++) ident |= summon_specific(y, x, p_ptr->max_dlv, SUMMON_HI_UNDEAD, FALSE, FALSE, FALSE);
            break;
         }
      /* Teleport Trap */
      case TRAP_OF_TELEPORT:
         msg_print("The world whirls around you.");
         teleport_player(67);
         ident=TRUE;
         break;
      /* Paralyzing Trap */
      case TRAP_OF_PARALYZING:
         if (!p_free_act)
         {
            msg_print("You touch a poisoned part and can't move.");
            (void)set_paralyzed(p_ptr->paralyzed + rand_int(10) + 10);
            ident=TRUE;
         }
         else
         {
            msg_print("You prick yourself on a needle.");
         }
         break;
      /* Explosive Device */
      case TRAP_OF_EXPLOSIVE_DEVICE:
         msg_print("A hidden explosive device explodes in your face.");
         take_hit(damroll(5, 8), "an explosion");
         ident=TRUE;
         break;
      /* Teleport Away Trap */
      case TRAP_OF_TELEPORT_AWAY:
         if (cave[y][x].o_idx==0)
         {
         }
         else
         {
	    object_type *o_ptr = &o_list[cave[y][x].o_idx];
	    
            ident = do_trap_teleport_away(o_ptr, x, y);
         }
         break;
      /* Lose Memory Trap */
      case TRAP_OF_LOSE_MEMORY:
         lose_exp(p_ptr->exp / 4);
         ident |= dec_stat(A_WIS, rand_int(20)+10, STAT_DEC_NORMAL);
         ident |= dec_stat(A_INT, rand_int(20)+10, STAT_DEC_NORMAL);
         if (!p_resist_conf)
         {
            if (set_confused(p_ptr->confused + rand_int(100) + 50))
            ident=TRUE;
         }
         if (ident)
            msg_print("You suddenly don't remember what you were doing.");
         else
            msg_print("You feel an alien force probing your mind.");
         break;
      /* Bitter Regret Trap */
      case TRAP_OF_BITTER_REGRET:
         msg_print("An age-old and hideous sounding spell reverbs of the walls.");
         ident |= dec_stat(A_DEX, 25, TRUE);
         ident |= dec_stat(A_WIS, 25, TRUE);
         ident |= dec_stat(A_CON, 25, TRUE);
         ident |= dec_stat(A_STR, 25, TRUE);
         ident |= dec_stat(A_CHR, 25, TRUE);
         ident |= dec_stat(A_INT, 25, TRUE);
         break;
      /* Bowel Cramps Trap */
      case TRAP_OF_BOWEL_CRAMPS:
         msg_print("A wretched smelling gas cloud upsets your stomach.");
         (void)set_food(PY_FOOD_STARVE - 1);
         (void)set_poisoned(0);
         if (!p_free_act)
         {
            (void)set_paralyzed(p_ptr->paralyzed + rand_int(10) + 10);
         }
         ident=TRUE;
         break;
      /* Blindness/Confusion Trap */
      case TRAP_OF_BLINDNESS_CONFUSION:
         msg_print("A powerful magic protected this.");
         if (!p_resist_blind)
         {
            (void)set_blind(p_ptr->blind + rand_int(100) + 100);
            ident=TRUE;
         }
         if (!p_resist_conf)
         {
            if (set_confused(p_ptr->confused + rand_int(20) + 15))
            ident=TRUE;
         }
         break;
      /* Aggravation Trap */
      case TRAP_OF_AGGRAVATION:
         msg_print("You hear a hollow noise echoing through the dungeons.");
         aggravate_monsters(1);
         break;
      /* Multiplication Trap */
      case TRAP_OF_MULTIPLICATION:
      {
         msg_print("You hear a loud click.");
         for(k = -1; k <= 1; k++)
         for(l = -1; l <= 1; l++)
         {
                if(in_bounds(py + l, px + k) && !cave[py + l][px + k].t_idx)
                {
                        place_trap(py + l, px + k);
                }
         }
         ident = TRUE;
         break;
      }
      /* Steal Item Trap */
      case TRAP_OF_STEAL_ITEM:
         {	    
            /* please note that magical stealing is not so easily circumvented */
            if (!p_ptr->paralyzed &&
                (rand_int(160) < (adj_dex_safe[p_ptr->stat_ind[A_DEX]] +
                                  p_ptr->lev)))
            {
               /* Saving throw message */
               msg_print("Your backpack seems to vibrate strangely!");
               break;
            }
            /* Find an item */
            for (k = 0; k < rand_int(10); k++)
            {
               char i_name[80];

               /* Pick an item */
               s16b i = rand_int(INVEN_PACK);

               /* Obtain the item */
               object_type *j_ptr = &inventory[i], *q_ptr, forge;

               /* Accept real items */
               if (!j_ptr->k_idx) continue;

               /* Don't steal artifacts  -CFT */
               if (artifact_p(j_ptr)) continue;

               /* Get a description */
               object_desc(i_name, j_ptr, FALSE, 3);

               /* Message */
               msg_format("%sour %s (%c) was stolen!",
                          ((j_ptr->number > 1) ? "One of y" : "Y"),
                          i_name, index_to_label(i));

               /* Create the item */
	       q_ptr = &forge;
	       object_copy(q_ptr, j_ptr);
	       q_ptr->number = 1;
	       
	       /* Drop it somewhere */
               do_trap_teleport_away(q_ptr, y, x);
	       
	       inven_item_increase(i,-1);
               inven_item_optimize(i);
               ident=TRUE;
            }
            break;
         }
      /* Summon Fast Quylthulgs Trap */
      case TRAP_OF_SUMMON_FAST_QUYLTHULGS:
         for (k = 0; k < randint(3); k++)
         {
            ident |= summon_specific(y, x, p_ptr->max_dlv, SUMMON_QUYLTHULG, TRUE, FALSE, FALSE);
         }
         if (ident)
         {
            msg_print("You suddenly have company.");
            (void)set_slow(p_ptr->slow + randint(25) + 15);
         }
         break;
      /* Trap of Sinking */
      case TRAP_OF_SINKING:
         msg_print("You fell through a trap door!");
         if (p_ffall)
         {
            msg_print("You float gently down to the next level.");
         }
         else
         {
            take_hit(damroll(2,8), "a trap door");
         }
	 
	 /* Still alive and autosave enabled */
	 if (autosave_l && (p_ptr->chp >= 0))
	 {
	    is_autosave = TRUE;
	    msg_print("Autosaving the game...");
	    do_cmd_save_game();
	    is_autosave = FALSE;
	 }
	 
	 dun_level++;
	   
	 /* Leaving */
	 p_ptr->leaving = TRUE;
         break;
      /* Trap of Mana Drain */
      case TRAP_OF_MANA_DRAIN:
         if (p_ptr->csp > 0)
         {
            p_ptr->csp = 0;
            p_ptr->csp_frac = 0;
            p_ptr->redraw |= (PR_MANA);
            msg_print("You sense a great loss.");
            ident=TRUE;
         }
         else
         {
            if (p_ptr->msp == 0) /* no sense saying this unless you never have mana */
            {
               msg_format("Suddenly you feel glad you're only a %s",cp_ptr->title);
            }
            else
            {
               msg_print("Your head feels dizzy for a moment.");
            }
         }
         break;
      /* Trap of Missing Money */
      case TRAP_OF_MISSING_MONEY:
         {
            u32b gold = (p_ptr->au / 10) + randint(25);

            if (gold < 2) gold = 2;
            if (gold > 5000) gold = (p_ptr->au / 20) + randint(3000);
            if (gold > p_ptr->au) gold = p_ptr->au;
            p_ptr->au -= gold;
            if (gold <= 0)
            {
                msg_print("You feel something touching you.");
            }
            else if (p_ptr->au)
            {
                msg_print("Your purse feels lighter.");
                msg_format("%ld coins were stolen!", (long)gold);
                ident=TRUE;
            }
            else
            {
                msg_print("Your purse feels empty.");
                msg_print("All of your coins were stolen!");
                ident=TRUE;
            }
            p_ptr->redraw |= (PR_GOLD);
         }
         break;
      /* Trap of No Return */
      case TRAP_OF_NO_RETURN:
         {
            object_type *j_ptr;
            s16b j;

            for (j=0;j<INVEN_WIELD;j++)
            {
               if (!inventory[j].k_idx) continue;
               j_ptr = &inventory[j];
               if ((j_ptr->tval==TV_SCROLL)
                    && (j_ptr->sval==SV_SCROLL_WORD_OF_RECALL))
               {
		  inven_item_increase(j, -randint(10));
                  combine_pack();
                  reorder_pack();
                  j=-1; /* start all over */
                  if (!ident)
                     msg_print("A small fire works it's way through your backpack. Some scrolls are burnt.");
                  else
                     msg_print("The fire hasn't finished.");
                  ident=TRUE;
               }
               else if ((j_ptr->tval==TV_ROD) && (j_ptr->sval == SV_ROD_RECALL))
               {
                  j_ptr->pval = 5000; /* a long time */
                  if (!ident) msg_print("You have a feeling of staying.");
                  ident=TRUE;
               }
            }
            if ((!ident) && (p_ptr->word_recall == 0))
            {
               msg_print("You feel like leaving against your will!");
               p_ptr->word_recall = 15 + randint(20);
               ident=TRUE;
            }
            else if ((!ident) && (p_ptr->word_recall))
            {
               msg_print("You feel like staying around.");
               p_ptr->word_recall = 0;
               ident=TRUE;
            }
         }
         break;
      /* Trap of Silent Switching */
      case TRAP_OF_SILENT_SWITCHING:
         {
            s16b i,j,slot1,slot2;
            object_type *j_ptr, *k_ptr;
            for (i=INVEN_WIELD;i<INVEN_TOTAL;i++)
            {
               j_ptr = &inventory[i];
               if (!j_ptr->k_idx) continue;
               slot1=wield_slot(j_ptr);
               for (j=0;j<INVEN_WIELD;j++)
               {
                  k_ptr = &inventory[j];
                  if (!k_ptr->k_idx) continue;
                  /* this is a crude hack, but it prevent wielding 6 torches... */
                  if (k_ptr->number > 1) continue;
                  slot2=wield_slot(k_ptr);
                  /* a chance of 4 in 5 of switching something, then 2 in 5 to do it again */
                  if ((slot1==slot2) && (rand_int(100)<(80-ident*40)))
                  {
                     object_type tmp_obj;
                     tmp_obj = inventory[j];
                     inventory[j] = inventory[i];
                     inventory[i] = tmp_obj;
                     ident=TRUE;
                  }
               }
            }
            if (ident)
            {
               p_ptr->update |= (PU_BONUS);
               p_ptr->update |= (PU_TORCH);
               p_ptr->update |= (PU_MANA);
               msg_print("You somehow feel an other person.");
            }
            else
            {
               msg_print("You feel a lack of useful items.");
            }
         }
         break;
      /* Trap of Walls */
      case TRAP_OF_WALLS:
         ident = player_handle_trap_of_walls();
         break;
      /* Trap of Calling Out */
      case TRAP_OF_CALLING_OUT:
         {
            ident=do_player_trap_call_out();
            if (!ident)
            {
               /* Increase "afraid" */
               if (p_resist_fear)
                   msg_print("You feel as if you had a nightmare!");
               else if (rand_int(100) < p_ptr->skill_sav)
                   msg_print("You remember having a nightmare!");
               else if (set_afraid(p_ptr->afraid + 3 + randint(40)))
                     msg_print("You have the vision of a powerful enemy.");
            }
         }
         break;
      /* Trap of Sliding */
      case TRAP_OF_SLIDING:
         break;
      /* Trap of Charges Drain */
      case TRAP_OF_CHARGES_DRAIN:
         {
            s16b         i;
            object_type *j_ptr;
            /* Find an item */
            for (k = 0; k < 10; k++)
            {
               i = rand_int(INVEN_PACK);
               j_ptr = &inventory[i];
               /* Drain charged wands/staffs */
               if (((j_ptr->tval == TV_STAFF) || (j_ptr->tval == TV_WAND)) &&
                   (j_ptr->pval))
               {
                  ident = TRUE;
                  j_ptr->pval = j_ptr->pval / (randint(4)+1);
                  /* Window stuff */
                  p_ptr->window |= PW_INVEN;
                  /* Combine / Reorder the pack */
                  p_ptr->notice |= (PN_COMBINE | PN_REORDER);
                  if (randint(10)>3) break; /* 60% chance of only 1 */
               }
            }
            if (ident)
              msg_print("Your backpack seems to be turned upside down.");
            else
              msg_print("You hear a wail of great disappointment.");
         }
         break;
      /* Trap of Stair Movement */
      case TRAP_OF_STAIR_MOVEMENT:
         {
            s16b cx,cy,i,j;
            s16b cnt = 0;
            s16b cnt_seen = 0;
            s16b tmps, tmpx;
            u32b tmpf;
            bool seen = FALSE;
            s16b index_x[20],index_y[20]; /* 20 stairs per level is enough? */
            cave_type *cv_ptr;
            if (p_ptr->max_dlv!=99) /* no sense in relocating that stair! */
            {
               for (cx=0;cx<cur_wid;cx++)
               {
                  for (cy=0;cy<cur_hgt;cy++)
                  {
                     cv_ptr = &cave[cy][cx];
                     if ((cv_ptr->feat != FEAT_LESS) &&
			 (cv_ptr->feat != FEAT_MORE) &&
			 (cv_ptr->feat != FEAT_QUEST_ENTER) &&
			 (cv_ptr->feat != FEAT_QUEST_EXIT) &&
			 (cv_ptr->feat != FEAT_QUEST_UP) &&
			 (cv_ptr->feat != FEAT_QUEST_DOWN) &&
			 (cv_ptr->feat != FEAT_SHAFT_UP) &&
			 (cv_ptr->feat != FEAT_SHAFT_DOWN)) continue;

                     index_x[cnt]=cx;
                     index_y[cnt]=cy;
                     cnt++;
                  }
               }
               if (cnt==0)
               {
                  quit("Executing moving stairs trap on level with no stairs!");
               }
               else
               {
                  for (i=0;i<cnt;i++)
                  {
                     for (j=0;j<10;j++) /* try 10 times to relocate */
                     {
                        cave_type *c_ptr2 = &cave[index_y[i]][index_x[i]];

                        cx=rand_int(cur_wid);
                        cy=rand_int(cur_hgt);

                        if ((cx==index_x[i]) || (cy==index_y[i])) continue;
                        if (!cave_valid_bold(cy, cx) || cave[cy][cx].o_idx!=0) continue;

                        /* don't put anything in vaults */
                        if (cave[cy][cx].info & CAVE_ICKY) continue;

                        tmpf = cave[cy][cx].feat;
                        tmps = cave[cy][cx].info;
                        tmpx = cave[cy][cx].mimic;
                        cave[cy][cx].feat = c_ptr2->feat;
                        cave[cy][cx].info = c_ptr2->info;
                        cave[cy][cx].mimic = c_ptr2->mimic;
                        c_ptr2->feat  = tmpf;
                        c_ptr2->info  = tmps;
                        c_ptr2->mimic = tmpx;

                        /* if we are placing walls in rooms, make them rubble instead */
                        if ((c_ptr2->info & CAVE_ROOM) &&
			    (c_ptr2->feat >= FEAT_WALL_EXTRA) &&
			    (c_ptr2->feat <= FEAT_PERM_SOLID))
                        {
			   cave[index_y[i]][index_x[i]].feat = FEAT_RUBBLE;
                        }
                        if (player_has_los_bold(cy, cx))
                        {
                           note_spot(cy,cx);
                           lite_spot(cy,cx);
                           seen=TRUE;
                        }
                        else
                        {
                           cave[cy][cx].info &=~CAVE_MARK;
                        }
                        if (player_has_los_bold(index_y[i],index_x[i]))
                        {
                           note_spot(index_y[i],index_x[i]);
                           lite_spot(index_y[i],index_x[i]);
                           seen = TRUE;
                        }
                        else
                        {
                           cave[index_y[i]][index_x[i]].info &=~CAVE_MARK;
                        }
                        break;
                     }
                     if (seen) cnt_seen++;
                     seen = FALSE;
                  } /* cnt loop */
                  ident = (cnt_seen>0);
                  if ((ident) && (cnt_seen>1))
                  {
                     msg_print("You see some stairs move.");
                  }
                  else if (ident)
                  {
                     msg_print("You see a stair move.");
                  }
                  else
                  {
                     msg_print("You hear distant scraping noises.");
                  }
		  p_ptr->redraw |= PR_MAP;
               } /* any stairs found */
            }
            else /* are we on level 99 */
            {
               msg_print("You have a feeling that this trap could be dangerous.");
            }
         }
         break;
      /* Trap of New Trap */
      case TRAP_OF_NEW:
         {
            s16b i = 0;
            /* if we're on a floor or on a door, place a new trap */
            if ((item == -1) || (item == -2))
            {
		  place_trap(y, x);
                  if (player_has_los_bold(x, y))
                  {
                     note_spot(x, y);
                     lite_spot(x, y);
                  }
               }
            else
            {
               /* re-trap the chest */
	       place_trap(y, x);
               i_ptr->pval += i; /* this trap itself has p1val+0, so it's ok here */
//               forget_item(i_ptr);
            }
            msg_print("You hear a noise, and then an echo.");
            ident=FALSE;
         }
         break;
      /* Trap of Scatter Items */
      case TRAP_OF_SCATTER_ITEMS:
         {
             s16b i,j;
            bool message = FALSE;
            for (i=0;i<INVEN_PACK;i++)
            {
               if (!inventory[i].k_idx) continue;
               if (rand_int(10)<3) continue;
               for (j=0;j<10;j++)
               {
                  object_type tmp_obj;
                  s16b cx = x+15-rand_int(30);
                  s16b cy = y+15-rand_int(30);
                  if (!in_bounds(cy,cx)) continue;
                  if (!cave_valid_bold(cy,cx)) continue;
                  tmp_obj = inventory[i];
                  inven_item_increase(i,-999);
                  inven_item_optimize(i);
                  p_ptr->notice |= (PN_COMBINE | PN_REORDER);
                  (void)floor_carry(cy, cx, &tmp_obj);
                  if (!message)
                  {
                     msg_print("You feel light-footed.");
                     message = TRUE;
                  }
                  if (player_has_los_bold(cy, cx))
                  {
                     char i_name[80];
                     object_desc(i_name, &tmp_obj, TRUE, 3);
                     note_spot(cy, cx);
                     lite_spot(cy, cx);
                     ident=TRUE;
                     msg_format("Suddenly %s appear%s!",i_name, (tmp_obj.number>1)?"":"s");
                  }
                  break;
               }
               ident = message;
            }
         }
         break;
      /* Trap of Decay */
      case TRAP_OF_DECAY:
         break;
      /* Trap of Wasting Wands */
      case TRAP_OF_WASTING_WANDS:
         {
            s16b i;
            object_type *j_ptr;
            for (i=0;i<INVEN_PACK;i++)
            {
               if (!inventory[i].k_idx) continue;
               j_ptr = &inventory[i];
               if (j_ptr->tval==TV_WAND)
               {
                  if ((j_ptr->sval>=SV_WAND_TELEPORT_AWAY) && (rand_int(5)==1))
                  {
                     if (object_known_p(j_ptr)) ident=TRUE;
                     j_ptr->sval = rand_int(SV_WAND_TELEPORT_AWAY);
                     j_ptr->k_idx = lookup_kind(j_ptr->tval, j_ptr->sval);
                     p_ptr->notice |= (PN_COMBINE | PN_REORDER);
                  }
                  if ((j_ptr->sval>=SV_STAFF_TELEPORTATION) && (rand_int(5)==1))
                  {
                     if (object_known_p(j_ptr)) ident=TRUE;
                     j_ptr->sval = rand_int(SV_STAFF_TELEPORTATION);
                     j_ptr->k_idx = lookup_kind(j_ptr->tval, j_ptr->sval);
                     p_ptr->notice |= (PN_COMBINE | PN_REORDER);
                  }
               }
            }
         }
         if (ident)
         {
            msg_print("You have lost trust in your backpack!");
         }
         else
         {
            msg_print("You hear an echoing cry of rage.");
         }
         break;
      /* Trap of Filling */
      case TRAP_OF_FILLING:
         {
            s16b nx, ny;
            for (nx=x-8;nx<=x+8;nx++)
            {
               for (ny=y-8;ny<=y+8;ny++)
               {
                  if (!in_bounds (nx, ny)) continue;

                  if (rand_int(distance(nx,ny,x,y))>3)
                  {
                     place_trap(ny,nx);
                  }
               }
            }
            msg_print("The floor vibrates in a strange way.");
            ident = FALSE;
         }
         break;
      case TRAP_OF_DRAIN_SPEED:
         {
            object_type *j_ptr;
            s16b j, chance = 75;
            u64b f1, f2, f3;

            for (j=0;j<INVEN_TOTAL;j++)
            {
               /* don't bother the overflow slot */
               if (j==INVEN_PACK) continue;

               if (!inventory[j].k_idx) continue;
               j_ptr = &inventory[j];
               object_flags(j_ptr, &f1, &f2, &f3);

               /* is it a non-artifact speed item? */
               if ((!j_ptr->name1) && (f1 & TR1_SPEED))
               {
                  if (randint(100)<chance)
                  {
                     j_ptr->pval = j_ptr->pval / 2;
                     if (j_ptr->pval == 0)
                     {
                        j_ptr->pval--;
//                        j_ptr->ident |= ID_CURSED;
//                        j_ptr->ident |= ID_SENSE;
                     }
                     chance /= 2;
                     ident = TRUE;
                  }
                  inven_item_optimize(j);
               }
            }
            if (!ident)
               msg_print("You feel some things in your pack vibrating.");
            else
            {
               combine_pack();
               reorder_pack();
               msg_print("You suddenly feel you have time for self-reflection.");
               /* Recalculate bonuses */
               p_ptr->update |= (PU_BONUS);

               /* Recalculate mana */
               p_ptr->update |= (PU_MANA);

               /* Window stuff */
               p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);
            }
         }
         break;

      /*
       * single missile traps
       */

      case TRAP_OF_ARROW_I:
         ident = player_handle_missile_trap(1, TV_ARROW, SV_AMMO_NORMAL, 4, 8, 0, "Arrow Trap"); break;
      case TRAP_OF_ARROW_II:
         ident = player_handle_missile_trap(1, TV_BOLT, SV_AMMO_NORMAL, 10, 8, 0, "Bolt Trap"); break;
      case TRAP_OF_ARROW_III:
         ident = player_handle_missile_trap(1, TV_ARROW, SV_AMMO_HEAVY, 12, 12, 0, "Seeker Arrow Trap"); break;
      case TRAP_OF_ARROW_IV:
         ident = player_handle_missile_trap(1, TV_BOLT, SV_AMMO_HEAVY, 12, 16, 0, "Seeker Bolt Trap"); break;
      case TRAP_OF_POISON_ARROW_I:
         ident = player_handle_missile_trap(1, TV_ARROW, SV_AMMO_NORMAL, 4, 8, 10+randint(20), "Poison Arrow Trap"); break;
      case TRAP_OF_POISON_ARROW_II:
         ident = player_handle_missile_trap(1, TV_BOLT, SV_AMMO_NORMAL, 10, 8, 15+randint(30), "Poison Bolt Trap"); break;
      case TRAP_OF_POISON_ARROW_III:
         ident = player_handle_missile_trap(1, TV_ARROW, SV_AMMO_HEAVY, 12, 12, 30+randint(50), "Poison Seeker Arrow Trap"); break;
      case TRAP_OF_POISON_ARROW_IV:
         ident = player_handle_missile_trap(1, TV_BOLT, SV_AMMO_HEAVY, 12, 16, 40+randint(70), "Poison Seeker Bolt Trap"); break;
      case TRAP_OF_DAGGER_I:
         ident = player_handle_missile_trap(1, TV_DAGGER, SV_BROKEN_DAGGER, 4, 8, 0, "Dagger Trap"); break;
      case TRAP_OF_DAGGER_II:
         ident = player_handle_missile_trap(1, TV_DAGGER, SV_DAGGER, 10, 8, 0, "Dagger Trap"); break;
      case TRAP_OF_POISON_DAGGER_I:
         ident = player_handle_missile_trap(1, TV_DAGGER, SV_BROKEN_DAGGER, 4, 8, 15+randint(20), "Poison Dagger Trap"); break;
      case TRAP_OF_POISON_DAGGER_II:
         ident = player_handle_missile_trap(1, TV_DAGGER, SV_DAGGER, 10, 8, 20+randint(30), "Poison Dagger Trap"); break;

      /*
       * multiple missile traps
       * numbers range from 2 (level 0 to 14) to 10 (level 120 and up)
       */

      case TRAP_OF_ARROWS_I:
         ident = player_handle_missile_trap(2+(p_ptr->max_dlv / 15), TV_ARROW, SV_AMMO_NORMAL, 4, 8, 0, "Arrow Trap"); break;
      case TRAP_OF_ARROWS_II:
         ident = player_handle_missile_trap(2+(p_ptr->max_dlv / 15), TV_BOLT, SV_AMMO_NORMAL, 10, 8, 0, "Bolt Trap"); break;
      case TRAP_OF_ARROWS_III:
         ident = player_handle_missile_trap(2+(p_ptr->max_dlv / 15), TV_ARROW, SV_AMMO_HEAVY, 12, 12, 0, "Seeker Arrow Trap"); break;
      case TRAP_OF_ARROWS_IV:
         ident = player_handle_missile_trap(2+(p_ptr->max_dlv / 15), TV_BOLT, SV_AMMO_HEAVY, 12, 16, 0, "Seeker Bolt Trap"); break;
      case TRAP_OF_POISON_ARROWS_I:
         ident = player_handle_missile_trap(2+(p_ptr->max_dlv / 15), TV_ARROW, SV_AMMO_NORMAL, 4, 8, 10+randint(20), "Poison Arrow Trap"); break;
      case TRAP_OF_POISON_ARROWS_II:
         ident = player_handle_missile_trap(2+(p_ptr->max_dlv / 15), TV_BOLT, SV_AMMO_NORMAL, 10, 8, 15+randint(30), "Poison Bolt Trap"); break;
      case TRAP_OF_POISON_ARROWS_III:
         ident = player_handle_missile_trap(2+(p_ptr->max_dlv / 15), TV_ARROW, SV_AMMO_HEAVY, 12, 12, 30+randint(50), "Poison Seeker Arrow Trap"); break;
      case TRAP_OF_POISON_ARROWS_IV:
         ident = player_handle_missile_trap(2+(p_ptr->max_dlv / 15), TV_BOLT, SV_AMMO_HEAVY, 12, 16, 40+randint(70), "Poison Seeker Bolt Trap"); break;
      case TRAP_OF_DAGGERS_I:
         ident = player_handle_missile_trap(2+(p_ptr->max_dlv / 15), TV_DAGGER, SV_BROKEN_DAGGER, 4, 8, 0, "Dagger Trap"); break;
      case TRAP_OF_DAGGERS_II:
         ident = player_handle_missile_trap(2+(p_ptr->max_dlv / 15), TV_DAGGER, SV_DAGGER, 10, 8, 0, "Dagger Trap"); break;
      case TRAP_OF_POISON_DAGGERS_I:
         ident = player_handle_missile_trap(2+(p_ptr->max_dlv / 15), TV_DAGGER, SV_BROKEN_DAGGER, 4, 8, 15+randint(20), "Poison Dagger Trap"); break;
      case TRAP_OF_POISON_DAGGERS_II:
         ident = player_handle_missile_trap(2+(p_ptr->max_dlv / 15), TV_SWORD, SV_DAGGER, 10, 8, 20+randint(30), "Poison Dagger Trap"); break;

      case TRAP_OF_DROP_ITEMS:
         {
            s16b i;
            bool message = FALSE;
            for (i=0;i<INVEN_PACK;i++)
            {
               object_type tmp_obj;
               if (!inventory[i].k_idx) continue;
               if (randint(100)<80) continue;
               tmp_obj = inventory[i];
               /* drop carefully */
               drop_near(&tmp_obj, 0, y, x);
               inven_item_increase(i,-999);
               inven_item_optimize(i);
               p_ptr->notice |= (PN_COMBINE | PN_REORDER);
               if (!message)
               {
                  msg_print("You are startled by a sudden sound.");
                  message = TRUE;
               }
               ident = TRUE;
            }
            if (!ident)
            {
               msg_print("You hear a sudden, strange sound.");
            }
         }
         break;
      case TRAP_OF_DROP_ALL_ITEMS:
         {
            s16b i;
            bool message = FALSE;
            for (i=0;i<INVEN_PACK;i++)
            {
               object_type tmp_obj;
               if (!inventory[i].k_idx) continue;
               if (randint(100)<10) continue;
               tmp_obj = inventory[i];
               /* drop carefully */
               drop_near(&tmp_obj, 0, y, x);
               inven_item_increase(i,-999);
               inven_item_optimize(i);
               p_ptr->notice |= (PN_COMBINE | PN_REORDER);
               if (!message)
               {
                  msg_print("You are startled a lot by a sudden sound.");
                  message = TRUE;
               }
               ident = TRUE;
            }
            if (!ident)
            {
               msg_print("You hear a sudden, strange sound.");
            }
         }
         break;
      case TRAP_OF_DROP_EVERYTHING:
         {
            s16b i;
            bool message = FALSE;
            for (i=0;i<INVEN_TOTAL;i++)
            {
               object_type tmp_obj;
               if (!inventory[i].k_idx) continue;
               if (randint(100)<30) continue;
               tmp_obj = inventory[i];
               /* drop carefully */
               drop_near(&tmp_obj, 0, y, x);
               inven_item_increase(i,-999);
               inven_item_optimize(i);
               p_ptr->notice |= (PN_COMBINE | PN_REORDER);
               if (!message)
               {
                  msg_print("You are completely startled by a sudden sound.");
                  message = TRUE;
               }
               ident = TRUE;
            }
            if (!ident)
            {
               msg_print("You hear a sudden, strange sound.");
            }
         }
         break;

      /* Bolt Trap */
      case TRAP_OF_ELEC_BOLT:       ident=player_handle_breath_trap(1, GF_ELEC, TRAP_OF_ELEC_BOLT); break;
      case TRAP_OF_POIS_BOLT:       ident=player_handle_breath_trap(1, GF_POIS, TRAP_OF_POIS_BOLT); break;
      case TRAP_OF_ACID_BOLT:       ident=player_handle_breath_trap(1, GF_ACID, TRAP_OF_ACID_BOLT); break;
      case TRAP_OF_COLD_BOLT:       ident=player_handle_breath_trap(1, GF_COLD, TRAP_OF_COLD_BOLT); break;
      case TRAP_OF_FIRE_BOLT:       ident=player_handle_breath_trap(1, GF_FIRE, TRAP_OF_FIRE_BOLT); break;
      case TRAP_OF_PLASMA_BOLT:     ident=player_handle_breath_trap(1, GF_PLASMA, TRAP_OF_PLASMA_BOLT); break;
      case TRAP_OF_WATER_BOLT:      ident=player_handle_breath_trap(1, GF_WATER, TRAP_OF_WATER_BOLT); break;
      case TRAP_OF_LITE_BOLT:       ident=player_handle_breath_trap(1, GF_LITE, TRAP_OF_LITE_BOLT); break;
      case TRAP_OF_DARK_BOLT:       ident=player_handle_breath_trap(1, GF_DARK, TRAP_OF_DARK_BOLT); break;
      case TRAP_OF_SHARDS_BOLT:     ident=player_handle_breath_trap(1, GF_SHARDS, TRAP_OF_SHARDS_BOLT); break;
      case TRAP_OF_SOUND_BOLT:      ident=player_handle_breath_trap(1, GF_SOUND, TRAP_OF_SOUND_BOLT); break;
      case TRAP_OF_CONFUSION_BOLT:  ident=player_handle_breath_trap(1, GF_CONFUSION, TRAP_OF_CONFUSION_BOLT); break;
      case TRAP_OF_FORCE_BOLT:      ident=player_handle_breath_trap(1, GF_FORCE, TRAP_OF_FORCE_BOLT); break;
      case TRAP_OF_INERTIA_BOLT:    ident=player_handle_breath_trap(1, GF_INERTIA, TRAP_OF_INERTIA_BOLT); break;
      case TRAP_OF_MANA_BOLT:       ident=player_handle_breath_trap(1, GF_MANA, TRAP_OF_MANA_BOLT); break;
      case TRAP_OF_ICE_BOLT:        ident=player_handle_breath_trap(1, GF_ICE, TRAP_OF_ICE_BOLT); break;
      case TRAP_OF_CHAOS_BOLT:      ident=player_handle_breath_trap(1, GF_CHAOS, TRAP_OF_CHAOS_BOLT); break;
      case TRAP_OF_NETHER_BOLT:     ident=player_handle_breath_trap(1, GF_NETHER, TRAP_OF_NETHER_BOLT); break;
      case TRAP_OF_DISENCHANT_BOLT: ident=player_handle_breath_trap(1, GF_DISENCHANT, TRAP_OF_DISENCHANT_BOLT); break;
      case TRAP_OF_NEXUS_BOLT:      ident=player_handle_breath_trap(1, GF_NEXUS, TRAP_OF_NEXUS_BOLT); break;
      case TRAP_OF_TIME_BOLT:       ident=player_handle_breath_trap(1, GF_TIME, TRAP_OF_TIME_BOLT); break;
      case TRAP_OF_GRAVITY_BOLT:    ident=player_handle_breath_trap(1, GF_GRAVITY, TRAP_OF_GRAVITY_BOLT); break;

      /* Ball Trap */
      case TRAP_OF_ELEC_BALL:       ident=player_handle_breath_trap(3, GF_ELEC, TRAP_OF_ELEC_BALL); break;
      case TRAP_OF_POIS_BALL:       ident=player_handle_breath_trap(3, GF_POIS, TRAP_OF_POIS_BALL); break;
      case TRAP_OF_ACID_BALL:       ident=player_handle_breath_trap(3, GF_ACID, TRAP_OF_ACID_BALL); break;
      case TRAP_OF_COLD_BALL:       ident=player_handle_breath_trap(3, GF_COLD, TRAP_OF_COLD_BALL); break;
      case TRAP_OF_FIRE_BALL:       ident=player_handle_breath_trap(3, GF_FIRE, TRAP_OF_FIRE_BALL); break;
      case TRAP_OF_PLASMA_BALL:     ident=player_handle_breath_trap(3, GF_PLASMA, TRAP_OF_PLASMA_BALL); break;
      case TRAP_OF_WATER_BALL:      ident=player_handle_breath_trap(3, GF_WATER, TRAP_OF_WATER_BALL); break;
      case TRAP_OF_LITE_BALL:       ident=player_handle_breath_trap(3, GF_LITE, TRAP_OF_LITE_BALL); break;
      case TRAP_OF_DARK_BALL:       ident=player_handle_breath_trap(3, GF_DARK, TRAP_OF_DARK_BALL); break;
      case TRAP_OF_SHARDS_BALL:     ident=player_handle_breath_trap(3, GF_SHARDS, TRAP_OF_SHARDS_BALL); break;
      case TRAP_OF_SOUND_BALL:      ident=player_handle_breath_trap(3, GF_SOUND, TRAP_OF_SOUND_BALL); break;
      case TRAP_OF_CONFUSION_BALL:  ident=player_handle_breath_trap(3, GF_CONFUSION, TRAP_OF_CONFUSION_BALL); break;
      case TRAP_OF_FORCE_BALL:      ident=player_handle_breath_trap(3, GF_FORCE, TRAP_OF_FORCE_BALL); break;
      case TRAP_OF_INERTIA_BALL:    ident=player_handle_breath_trap(3, GF_INERTIA, TRAP_OF_INERTIA_BALL); break;
      case TRAP_OF_MANA_BALL:       ident=player_handle_breath_trap(3, GF_MANA, TRAP_OF_MANA_BALL); break;
      case TRAP_OF_ICE_BALL:        ident=player_handle_breath_trap(3, GF_ICE, TRAP_OF_ICE_BALL); break;
      case TRAP_OF_CHAOS_BALL:      ident=player_handle_breath_trap(3, GF_CHAOS, TRAP_OF_CHAOS_BALL); break;
      case TRAP_OF_NETHER_BALL:     ident=player_handle_breath_trap(3, GF_NETHER, TRAP_OF_NETHER_BALL); break;
      case TRAP_OF_DISENCHANT_BALL: ident=player_handle_breath_trap(3, GF_DISENCHANT, TRAP_OF_DISENCHANT_BALL); break;
      case TRAP_OF_NEXUS_BALL:      ident=player_handle_breath_trap(3, GF_NEXUS, TRAP_OF_NEXUS_BALL); break;
      case TRAP_OF_TIME_BALL:       ident=player_handle_breath_trap(3, GF_TIME, TRAP_OF_TIME_BALL); break;
      case TRAP_OF_GRAVITY_BALL:    ident=player_handle_breath_trap(3, GF_GRAVITY, TRAP_OF_GRAVITY_BALL); break;

      /* -SC- */
      case TRAP_OF_FEMINITY:
      {
         msg_print("Gas sprouts out... you feel you transmute.");
	 p_ptr->psex = SEX_FEMALE;
	 sp_ptr = &sex_info[p_ptr->psex];
	 ident = TRUE;
	 trap_hit(trap);
	 break;
      }
      case TRAP_OF_MASCULINITY:
      {
         msg_print("Gas sprouts out... you feel you transmute.");
	 p_ptr->psex = SEX_MALE;
	 sp_ptr = &sex_info[p_ptr->psex];
	 ident = TRUE;
	 trap_hit(trap);
	 break;
      }
      case TRAP_OF_NEUTRALITY:
      {
         msg_print("Gas sprouts out... you feel you transmute.");
	 p_ptr->psex = 2;
	 sp_ptr = &sex_info[p_ptr->psex];
	 ident = TRUE;
	 trap_hit(trap);
	 break;
      }
      case TRAP_OF_AGING:
      {
         msg_print("Colors are scintillating around you, you see your past running before your eyes.");
	 p_ptr->age += randint(rp_ptr->b_age/2);
	 ident = TRUE;
	 trap_hit(trap);
	 break;
      }
      case TRAP_OF_GROWING:
      {
	 s16b tmp;
	 
         msg_print("Heavy fumes sprout out... you feel you transmute.");
	 if (p_ptr->psex == SEX_FEMALE) tmp = rp_ptr->f_b_ht;
	 else tmp = rp_ptr->m_b_ht;
	 
	 p_ptr->ht += randint(tmp/4);
	 ident = TRUE;
	 trap_hit(trap);
	 break;
      }
      case TRAP_OF_SHRINKING:
      {
	 s16b tmp;
	 
         msg_print("Heavy fumes sprout out... you feel you transmute.");
	 if (p_ptr->psex == SEX_FEMALE) tmp = rp_ptr->f_b_ht;
	 else tmp = rp_ptr->m_b_ht;
	 
	 p_ptr->ht -= randint(tmp/4);
	 if (p_ptr->ht <= tmp/4) p_ptr->ht = tmp/4;
	 ident = TRUE;
	 trap_hit(trap);
	 break;
      }
      /* Trap of Eldritch Horror */
      case TRAP_OF_ELDRITCH_HORROR:
      {
	 object_type *j_ptr;
	 
	 msg_print("Cold winds begin to blow around you...");
	 sanity_blast(NULL, FALSE);

	 j_ptr = &inventory[INVEN_LITE];
	 if (j_ptr->k_idx != 0)
	 {
	    if ((j_ptr->tval == TV_LITE) && !artifact_p(j_ptr) && 
		(j_ptr->pval > 0))
	    {
	       j_ptr->pval = 0;
	       msg_print("Your light has gone out!");
	       p_ptr->update |= (PU_TORCH);
	    }
	 }
	 ident = TRUE;
         break;
      }
	   
      /* Trap of Tanker Drain */
      case TRAP_OF_TANKER_DRAIN:
            take_sanity_hit(damroll(t_info[trap].dd, t_info[trap].ds), 
                    "an insanity trap");
            p_ptr->redraw |= (PR_SANITY);
            msg_print("You feel dizzy.");
            ident= TRUE;
         break;

#if 0
      /* Trap of Divine Anger */
      case TRAP_OF_DIVINE_ANGER:
      {
         if (p_ptr->pgod == 0)
	 {
	    msg_print("Suddenly you feel you're an atheist.");
	 }
	 else
	 {
	    cptr name;
	    
	    name=deity_info[p_ptr->pgod-1].name;
	    msg_format("You feel you have angered %s.", name);
	    set_grace(p_ptr->grace - 3000);
	 }
      }
      break;

      /* Trap of Divine Wrath */
      case TRAP_OF_DIVINE_WRATH:
      {
         if (p_ptr->pgod == 0)
	 {
	    msg_print("Suddenly you feel you're an atheist.");
	 }
	 else
	 {
	    cptr name;
	    
	    name=deity_info[p_ptr->pgod-1].name;
	    
	    msg_format("%s quakes in rage: ``Thou art supremely insolent, mortal!!''", name);
	    nasty_side_effect();
	    set_grace(p_ptr->grace - 5000);
	 }
      }
      break;
#endif

      /* Trap of hallucination */
      case TRAP_OF_HALLUCINATION:
      {
	 msg_print("Scintillating colors hypnotize you for a moment.");
	 
	 set_image(80);
      }
      break;

      /* Trap of hallucination */
      case TRAP_OF_POLYMORPH:
      {
	 msg_print("Twisting magics swirl around you.");

	 polymorph_player(0);
      }
      break;

      /* Bolt Trap */
      case TRAP_OF_ROCKET: ident=player_handle_breath_trap(1, GF_ROCKET, trap); break;
      case TRAP_OF_NUKE_BOLT: ident=player_handle_breath_trap(1, GF_NUKE, trap); break;
      case TRAP_OF_DEATH_RAY: ident=player_handle_breath_trap(1, GF_DEATH_RAY, trap); break;
      case TRAP_OF_HOLY_FIRE: ident=player_handle_breath_trap(1, GF_HOLY_FIRE, trap); break;
      case TRAP_OF_HELL_FIRE: ident=player_handle_breath_trap(1, GF_HELL_FIRE, trap); break;
      case TRAP_OF_PSI_BOLT: ident=player_handle_breath_trap(1, GF_PSI, trap); break;
      case TRAP_OF_PSI_DRAIN: ident=player_handle_breath_trap(1, GF_PSI_DRAIN, trap); break;

      /* Ball Trap */
      case TRAP_OF_NUKE_BALL: ident=player_handle_breath_trap(3, GF_NUKE, TRAP_OF_NUKE_BALL); break;
      case TRAP_OF_PSI_BALL: ident=player_handle_breath_trap(3, GF_PSI, TRAP_OF_NUKE_BALL); break;

      default:
      {
         msg_print(format("Executing unknown trap %d",trap));
      }
   }

   return ident;
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

   /* no traps in town */
   if (!dun_level) return;

   if (!(cave_clean_bold (y, x) || ((cave[y][x].feat >= FEAT_DOOR_HEAD) &&
           (cave[y][x].feat <= FEAT_DOOR_TAIL)))) return;
   
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
