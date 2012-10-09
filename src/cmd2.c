/* File: cmd2.c */

/* Purpose: Movement commands (part 2) */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"
/*
 * Go up one level                                      -RAK-
 */
void do_cmd_go_up(void)
{
   cave_cell_type *c_ptr;
   cptr      name;

   /* Player grid */
   c_ptr = &dungeon.level[sublevel][py][px];

   /* Verify stairs */
   if (c_ptr->mtyp != DUNG_STAIR)
   {
      msg_print("I see no staircase here.");
      return;
   }
   if ((c_ptr->styp != DUNG_STAIR_UP) && (c_ptr->styp != DUNG_STAIR_SHAFTUP))
   {
      msg_print("You can't go up on this type of stair.");
      return;
   }
   name = f_name + f_info[get_f_idx(c_ptr->mtyp, c_ptr->styp)].name;

   /* Hack -- take a turn */
   energy_use = 100;

/* jk */
   used_stairs=TRUE;

   /* Go up the stairs */
/* jk - this allows the level reading and writing to be generic, */
/* since we need to save a level when exited, not when entered!   */

   get_stair_target(px, py, &p_ptr->new_mdepth, &p_ptr->new_sdepth);

   /* print a nice message */
   if ( (p_ptr->mdepth == p_ptr->new_mdepth) &&
        (p_ptr->sdepth == 0) && (p_ptr->new_sdepth != 0) )
   {
      msg_format("You enter the %s.", v_name + dungeon.level_name[p_ptr->new_sdepth]);
   }
   else if ( (p_ptr->mdepth == p_ptr->new_mdepth) &&
             (p_ptr->sdepth != 0) && (p_ptr->new_sdepth == 0) )
   {
      msg_format("You leave the %s.", v_name + dungeon.level_name[p_ptr->sdepth]);
   }
   else
   {
      if (c_ptr->styp==DUNG_STAIR_DOWN)
      {
         msg_format("You use %s %s.", is_a_vowel(name[0])?"an":"a", name);
      }
      if (c_ptr->styp==DUNG_STAIR_SHAFTDN)
      {
         msg_format("You enter %s %s.", is_a_vowel(name[0])?"an":"a", name);
      }
   }

   new_level_flag = TRUE;

   /* Create a way back if we change the baselevel */
   if (p_ptr->mdepth != p_ptr->new_mdepth) create_down_stair = TRUE;
dlog(DEBUGFLOW,"cmd2.c: do_cmd_go_down: now %d,%d target %d,%d\n",
               p_ptr->mdepth, p_ptr->sdepth, p_ptr->new_mdepth, p_ptr->new_sdepth);
}

/*
 * Go down one level
 */
void do_cmd_go_down(void)
{
   cave_cell_type *c_ptr;
   cptr      name;

   /* Player grid */
   c_ptr = &dungeon.level[sublevel][py][px];

   /* Verify stairs */
   if (c_ptr->mtyp != DUNG_STAIR)
   {
      msg_print("I see no staircase here.");
      return;
   }
   if ((c_ptr->styp != DUNG_STAIR_DOWN) && (c_ptr->styp != DUNG_STAIR_SHAFTDN))
   {
      msg_print("You can't go down on this type of stair.");
      return;
   }

   name = f_name + f_info[get_f_idx(c_ptr->mtyp, c_ptr->styp)].name;

   /* Hack -- take a turn */
   energy_use = 100;

/* jk */
   used_stairs=TRUE;

   /* Go down */
/* jk */
   get_stair_target(px, py, &p_ptr->new_mdepth, &p_ptr->new_sdepth);
   new_level_flag = TRUE;

   /* print a nice message */
   if ( (p_ptr->mdepth == p_ptr->new_mdepth) &&
        (p_ptr->sdepth == 0) && (p_ptr->new_sdepth != 0) )
   {
      msg_format("You enter the %s.", v_name + dungeon.level_name[p_ptr->new_sdepth]);
   }
   else if ( (p_ptr->mdepth == p_ptr->new_mdepth) &&
             (p_ptr->sdepth != 0) && (p_ptr->new_sdepth == 0) )
   {
      msg_format("You leave the %s.", v_name + dungeon.level_name[p_ptr->sdepth]);
   }
   else
   {
      if (c_ptr->styp==DUNG_STAIR_DOWN)
      {
         msg_format("You use %s %s.", is_a_vowel(name[0])?"an":"a", name);
      }
      if (c_ptr->styp==DUNG_STAIR_SHAFTDN)
      {
         msg_format("You enter %s %s.", is_a_vowel(name[0])?"an":"a", name);
      }
   }

   /* Create a way back if we change the baselevel */
   if (p_ptr->mdepth != p_ptr->new_mdepth) create_up_stair = TRUE;
dlog(DEBUGFLOW,"cmd2.c: do_cmd_go_down: now %d,%d target %d,%d\n",
               p_ptr->mdepth, p_ptr->sdepth, p_ptr->new_mdepth, p_ptr->new_sdepth);
}

/*
 * Simple command to "search" for one turn
 */
void do_cmd_search(void)
{
   /* Allow repeated command */
   if (p_ptr->command_arg)
   {
      /* Set repeat count */
      p_ptr->command_rep = p_ptr->command_arg - 1;

      /* Redraw the state */
      p_ptr->redraw1 |= (PR1_STATE);

      /* Cancel the arg */
      p_ptr->command_arg = 0;
   }

   /* Take a turn */
   energy_use = 100;

   /* Search */
   search();
}

/*
 * Hack -- toggle search mode
 */
void do_cmd_toggle_search(void)
{
   /* Stop searching */
   if (p_ptr->searching)
   {
      /* Clear the searching flag */
      p_ptr->searching = FALSE;

      /* Recalculate bonuses */
      p_ptr->update |= (PU_BONUS);

      /* Redraw the state */
      p_ptr->redraw1 |= (PR1_STATE);
   }

   /* Start searching */
   else
   {
      /* Set the searching flag */
      p_ptr->searching = TRUE;

      /* Update stuff */
      p_ptr->update |= (PU_BONUS);

      /* Redraw stuff */
      p_ptr->redraw1 |= (PR1_STATE | PR1_SPEED);
   }
}

/*
 * Allocates objects upon opening a chest    -BEN-
 * Disperse treasures from the chest "i_ptr", centered at (x,y).
 */
static void chest_death(s16b x, s16b y, object_type *i_ptr)
{
   s16b         number, small;
   s16b         tmpx,tmpy;

   /* Must be a chest */
   if (i_ptr->tval != TV_CHEST) return;

   /* Determine if the chest is small */
   small = (i_ptr->sval < SV_CHEST_MIN_LARGE);

   /* Determine how many items to drop */
/* jk - i_ptr->p1val/3 added */
   number = (i_ptr->sval % SV_CHEST_MIN_LARGE) * 2 * (i_ptr->p1val / 3);
   /* Always drop something! */
   if (number<1) number = 1;

   /* Generate some treasure */
   if (i_ptr->p1val && (number > 0))
   {
      /* Drop some objects (non-chests) */
      for ( ; number > 0; --number)
      {
         tmpx = x;
         tmpy = y;
         /* Opening a chest */
         opening_chest = TRUE;

         /* The "p1val" of a chest is how "good" it is */
         object_level = ABS(i_ptr->p1val) + 10;

         /* Small chests often drop gold */
         if (small && (rand_int(100) < 75))
         {
            place_gold_known(&tmpx, &tmpy);
         }

         /* Otherwise drop an item */
         else
         {
            place_object_known(&tmpx, &tmpy, FALSE, FALSE, FALSE, FALSE);
         }

         /* Reset the object level */
         object_level = p_ptr->mdepth;

         /* No longer opening a chest */
         opening_chest = FALSE;

         /* Notice it */
         note_spot(tmpx, tmpy);

         /* Display it */
         lite_spot(tmpx, tmpy);

         /* Under the player */
         if ((tmpx == px) && (tmpy == tmpy))
         {
            msg_print("You feel something roll beneath your feet.");
         }
      }
   }

   /* Empty */
   i_ptr->p1val = 0;
   /* Known */
   object_known(i_ptr);
}

void count_openables(s16b x, s16b y, s16b *number)
{
   s16b         k;
   cave_cell_type   *c_ptr;
   object_type *i_ptr;

   if (!in_bounds(x,y)) return;
   c_ptr = &dungeon.level[sublevel][y][x];

   if ( (c_ptr->mtyp == DUNG_DOOR) &&
        ( (c_ptr->styp == DUNG_DOOR_CLOSED) ||
          (c_ptr->styp == DUNG_DOOR_LOCKED) ||
          (c_ptr->styp == DUNG_DOOR_JAMMED) ) )
   {
           tmp_x[(*number)] = x;
           tmp_y[(*number)] = y;
           tmp_i[(*number)++] = -1; /* signify door */
           return; /* on a closed door no chests can be found :-) */
   }
   for (k=0;k<objects_on_floor(x,y);k++)
   {
      /* Get the object (if any) */
      i_ptr = get_item_pointer_floor_xy(k,x,y);
/* don't count empty chests if we know them */
      if ((i_ptr->tval==TV_CHEST) && !(object_known_p(i_ptr) && i_ptr->p1val==0))
      {
         tmp_x[(*number)] = x;
         tmp_y[(*number)] = y;
         tmp_i[(*number)++] = k+INVEN_TOTAL;
      }
   }
}

void count_open_around_us(s16b *number)
{
   count_openables(px-1,py,number);
   count_openables(px-1,py-1,number);
   count_openables(px-1,py+1,number);
   count_openables(px+1,py,number);
   count_openables(px+1,py-1,number);
   count_openables(px+1,py+1,number);
   count_openables(px,py-1,number);
   count_openables(px,py+1,number);
}

void count_chests(s16b *number)
{
   s16b k;
   object_type *i_ptr;

   /* first count the chests on the floor */
   for (k=0;k<objects_on_floor(px,py);k++)
   {
      /* Get the object (if any) */
      i_ptr = get_item_pointer_floor_xy(k,px,py);
/* count chests unless we know them to be empty */
      if ((i_ptr->tval==TV_CHEST) &&
          (object_known_p(i_ptr) && i_ptr->p1val==0))
      {
         tmp_x[(*number)] = px;
         tmp_y[(*number)] = py;
         tmp_i[(*number)++] = k+INVEN_TOTAL;
      }
   }
   for (k=0;k<INVEN_WIELD;k++)
   {
      if (!inventory[k].k_idx) continue;
      i_ptr = &inventory[k];
      /* count unopened items */
      if ((i_ptr->tval==TV_CHEST) &&
          !(object_known_p(i_ptr) && i_ptr->p1val==0))
      {
         tmp_x[(*number)] = px;
         tmp_y[(*number)] = py;
         tmp_i[(*number)++] = k;
      }
   }
}

/* jk - this allows us to split do_cmd_open in nice parts */
/* the return type allows us to break off easily */
bool open_something(s16b x, s16b y, s16b item)
{
   cave_cell_type   *c_ptr = &dungeon.level[sublevel][y][x];
   object_type *i_ptr = NULL;
   bool        more   = TRUE;
   s16b         i,j;
   bool        flag;

   if (item!=-1) /* we open no door, so get the item */
   {
      i_ptr=get_item_pointer_xy(item,x,y);
      if (i_ptr->number>1)
      {
         msg_print("Stacked chests don't open yet");
         return(FALSE);
      }
   }
   if (item==-1)
   {
      if (! ( (c_ptr->mtyp == DUNG_DOOR) &&
              ( (c_ptr->styp == DUNG_DOOR_CLOSED) ||
                (c_ptr->styp == DUNG_DOOR_LOCKED) ||
                (c_ptr->styp == DUNG_DOOR_JAMMED) ) ) )
      {
         msg_print("You see nothing there to open.");
         return(FALSE);
      }
      if (c_ptr->t_idx)
      {
         trap_item_type *tr_ptr = &t_list[c_ptr->t_idx];
         energy_use = 100;
         msg_print("You touch the door, and ....");
         player_execute_trap(tr_ptr,-2,x,y, FALSE);
         note_spot(x, y);
         lite_spot(x, y);
         p_ptr->update |= (PU_VIEW);
         update_stuff();
         return (FALSE); /* we certainly don't want to try again */
      }
      if (c_ptr->styp == DUNG_DOOR_JAMMED)  /* stuck door */
      {
         energy_use = 100;            /* Take a turn */
         msg_print("The door appears to be stuck.");    /* Stuck */
         return(FALSE); /* we don't need to try again */
      }
      else if (c_ptr->styp == DUNG_DOOR_LOCKED)      /* Locked door */
      {
         energy_use = 100;            /* Take a turn */
         i = p_ptr->skill_dis;        /* Disarm factor */
         if (p_ptr->blind || no_lite()) i=i/10; /* Penalize some conditions */
         if (p_ptr->confused || p_ptr->image) i = i / 10;
         j = i - c_ptr->extra * 4; /* Extract the difficulty */
         if (j < 2) j = 2;       /* Always have a small chance of success */
         if (rand_int(100) < j)            /* Success */
         {
            msg_print("You have picked the lock.");
            /* gain experience if this door is not in town */
            if (p_ptr->mdepth>0) gain_exp(c_ptr->extra);
            set_grid_type(x,y, DUNG_DOOR, DUNG_DOOR_OPEN, GRID_ADD, 0);
            note_spot(x, y);              /* Notice */
            lite_spot(x, y);              /* Redraw */
            p_ptr->update |= (PU_VIEW | PU_MONSTERS);
            return(FALSE); /* the job is done */
         }
         else            /* Failure */
         {
            if (flush_failure) flush();                /* Failure */
            msg_format("You failed to pick the lock.");
            return(TRUE);
         }
      }
      else if (c_ptr->styp==DUNG_DOOR_CLOSED)     /* Closed door */
      {
         set_grid_type(x,y, DUNG_DOOR, DUNG_DOOR_OPEN, GRID_ADD, 0);
         note_spot(x, y);            /* Notice */
         lite_spot(x, y);            /* Redraw */
         p_ptr->update |= (PU_VIEW | PU_MONSTERS);
         return(FALSE);
      }
      else
      {
         msg_print("You see nothing there to open 2.");
         return(FALSE);
      }
   } /* tried opening doors, so now for chests */
   else
   {
      char i_name[80];
      object_desc(i_name, i_ptr, TRUE, 3);

      energy_use = 100;          /* Take a turn */
      flag = TRUE;               /* Assume opened successfully */
      if (i_ptr->xtra2)
      {
         trap_item_type *tr_ptr = &t_list[i_ptr->xtra2];
         flag = FALSE;          /* Assume locked, and thus not open */
         i = p_ptr->skill_dis;  /* Get the "disarm" factor */
         if (p_ptr->blind || no_lite()) i = i / 10; /* Penalize some */
         if (p_ptr->confused || p_ptr->image) i = i / 10; /* conditions */
         j = i - i_ptr->p1val;   /* Extract the difficulty */
         if (j < 2) j = 2;      /* Always have a small chance of success */
         if (rand_int(100) < j) /* Success -- May still have traps */
         {
            if (first_trap(tr_ptr)!=-1)
            {
               /* Apply chest traps, if any */
               player_execute_trap(tr_ptr, item, x, y, FALSE);
            }
            else
            {
              gain_exp(1);
              msg_format("You have picked the lock and open %s.",i_name);
              chest_death(x, y, i_ptr); /* Let the Chest drop items */
            }
            return(FALSE);
         }
         else                   /* Failure -- Keep trying */
         {
            more = TRUE;       /* We may continue repeating */
            if (flush_failure) flush();
            msg_format("You failed to pick the lock on %s.",i_name);
            return(TRUE);
         }
      }
      else /* untrapped */
      {

         msg_format("You open %s.",i_name);
         chest_death(x,y,i_ptr);
      }
   } /* tried opening chests */
   return (FALSE); /* we don't come here, but djgpp doesn't know that */
}

/* jk - give the direction that is consistent with the locations
 * small distances are not computed to speed things up
 * first the only 1 square away version
 */
s16b what_dir1(s16b x, s16b y, s16b nx, s16b ny)
{
   if ((nx==x) && (ny==y)) return (DIR_BEL1);   /* below */
   if ((nx==x) && (ny==y-1)) return (DIR_N); /* north */
   if ((nx==x) && (ny==y+1)) return (DIR_S); /* south */
   if ((nx==x+1) && (ny==y)) return (DIR_E); /* east */
   if ((nx==x-1) && (ny==y)) return (DIR_W); /* west */
   if ((nx==x+1) && (ny==y+1)) return (DIR_SE); /* south-east */
   if ((nx==x+1) && (ny==y-1)) return (DIR_NE); /* north-east */
   if ((nx==x-1) && (ny==y+1)) return (DIR_SW); /* south-west */
   if ((nx==x-1) && (ny==y-1)) return (DIR_NW); /* north-west */
   return (-1);
}

/*
 *  now the distant version
 */
s16b what_dir(s16b x, s16b y, s16b nx, s16b ny)
{
   s16b dst;

   dst = distance(x, y, nx, ny);
   if (dst<=1)
   {
      return (what_dir1(x, y, nx, ny));
   }
   else if (dst<5)
   {
      s16b table[9][9] =
      {
         { DIR_NW,DIR_NW,DIR_NW,DIR_N, DIR_N,   DIR_N, DIR_NE, DIR_NE, DIR_NE },
         { DIR_NW,DIR_NW,DIR_NW,DIR_N, DIR_N,   DIR_N, DIR_NE, DIR_NE, DIR_NE },
         { DIR_W, DIR_NW,DIR_NW,DIR_NW,DIR_N,   DIR_NE,DIR_NE, DIR_NE, DIR_E  },
         { DIR_W, DIR_W, DIR_NW,DIR_NW,DIR_N,   DIR_NE,DIR_NE, DIR_E,  DIR_E  },
         { DIR_W, DIR_W, DIR_W, DIR_W, DIR_BEL1,DIR_E, DIR_E,  DIR_E,  DIR_E  },
         { DIR_W, DIR_W, DIR_SW,DIR_SW,DIR_S,   DIR_SE,DIR_SE, DIR_E,  DIR_E  },
         { DIR_W, DIR_SW,DIR_SW,DIR_SW,DIR_S,   DIR_SE,DIR_SE, DIR_SE, DIR_E  },
         { DIR_SW,DIR_SW,DIR_SW,DIR_S, DIR_S,   DIR_S, DIR_SE, DIR_SE, DIR_SE },
         { DIR_SW,DIR_SW,DIR_SW,DIR_S, DIR_S,   DIR_S, DIR_SE, DIR_SE, DIR_SE }
      };

      return (table[4+(ny-y)][4+(nx-x)]);
   }
   else
   {
      s16b dx = nx - x;
      s16b dy = ny - y;

      /* handle n, s, e, w directions */
      if (dx==0)
      {
         return ((dy<0)?DIR_N:DIR_S);
      }
      if (dy==0)
      {
         return ((dx<0)?DIR_W:DIR_E);
      }
      /* which quadrant? */
      if ((dx>0) && (dy>0))
      {
         /* choice between E, SE and S */
         if (dx>dy*2) return (DIR_E);
         else if (dy>dx*2) return (DIR_S);
         else return (DIR_SE);
      }
      else if ((dx>0) && (dy<0))
      {
         /* choice between N, NE and E */
         if ((-dy)>2*dx) return (DIR_N);
         else if (dx>2*(-dy)) return (DIR_E);
         else return (DIR_NE);
      }
      else if ((dx<0) && (dy<0))
      {
         /* choice between W, NW and N */
         if ((-dx)>2*(-dy)) return (DIR_W);
         else if ((-dy)>2*(-dx)) return (DIR_N);
         else return (DIR_NW);
      }
      else if ((dx<0) && (dy>0))
      {
         /* choice between S, SW and E */
         if (dy>2*(-dx)) return (DIR_S);
         else if ((-dx)>2*dy) return (DIR_W);
         else return (DIR_SW);
      }
   }
   return (-1); /* oops */
}

int disarm_open_dir(s16b number,bool disarm)
{
   s16b   max_scrn = number / 20;
   s16b   cur_scrn = 0;
   s16b   cur_index;
   s16b   cur_max;
   char   name[80],keys[40];
   char   c;
   s16b   result = 0;
   s16b   i,dir;
   bool   chosen = FALSE;

dlog(DEBUGTRAPS,"cmd2.c: disarm_open_dir: num %d disarm %d\n",
                number, disarm);

   dir = what_dir(px,py,tmp_x[0],tmp_y[0]);

   if (number==1)
   {
      if (tmp_i[0] == -1)
      {
         if (disarm)
         {
            strcpy(name,format("the %sdoor trap ",
                        num_traps_xy(tmp_x[0], tmp_y[0], TRAP_FOUND)>1?"complex ":""));
         }
         else
         {
            strcpy(name,"the door ");
         }
         strcat(name,dirstr[dir]);
      }
      else if (tmp_i[0] == -2)
      {
         strcpy(name,format("the %sfloor trap ",
                        num_traps_xy(tmp_x[0], tmp_y[0], TRAP_FOUND)>1?"complex ":""));
         strcat(name,dirstr[dir]);
      }
      else
      {
         char i_name[80];
         object_type *i_ptr;
         i_ptr = get_item_pointer_xy(tmp_i[0],tmp_x[0],tmp_y[0]);
         object_desc(i_name, i_ptr, FALSE, 3);
dlog(DEBUGTRAPS,"cmd2.c: disarm_open_dir: i_name %s\n", i_name);
         strcpy(name,i_name);
         if ((tmp_x[0]!=px) || (tmp_y[0]!=py))
         {
           strcat(name," ");
           strcat(name,dirstr[dir]);
         }
         else
           strcat(name," in your inventory");
      }
      /* JK - this part of the code is used to ask you about  */
      /* opening the <something> to the <north or whatever>   */
      /* of you (y/n), but also takes a direction as yes, so  */
      /* as before, you can press o8 to open something to the */
      /* north of you                                         */
      c=' ';
      strcpy(keys,"yn12346789BbJjNnHhLlYyKkUu");
      strcat(keys,format("%c",ESCAPE));
      strcat(keys,format("%c",KTRL('M')));
      while (strchr(keys,c)==NULL)
      {
         prt(format("%s %s?",(disarm ? "Disarm":"Open"),name),0,MESSAGE_ROW);
         c=inkey();
         prt("",0,MESSAGE_ROW);
      }
      switch(c)
      {
         case '1':
         case 'B':
         case 'b':
            if (dir==1) c='y';
            break;
         case '2':
         case 'J':
         case 'j':
            if (dir==2) c='y';
            break;
         case '3':
         case 'N':
         case 'n':
            if (dir==3) c='y';
            break;
         case '4':
         case 'H':
         case 'h':
            if (dir==4) c='y';
            break;
         case '6':
         case 'L':
         case 'l':
            if (dir==6) c='y';
            break;
         case '7':
         case 'Y':
         case 'y':
            if (dir==7) c='y';
            break;
         case '8':
         case 'K':
         case 'k':
            if (dir==8) c='y';
            break;
         case '9':
         case 'U':
         case 'u':
            if (dir==9) c='y';
            break;
      }
      if (c=='y')
      {
         return (0);
      }
      else
      {
         return(-1);
      }
   } /* more than 1 item to choose from */
   Term_save();
   while (!chosen)
   {
      cur_max = number - max_scrn * 20;
      for (i=0;i<cur_max;i++)
      {
         cur_index = cur_scrn * 20+i;
         if (tmp_i[cur_index] == -1)
         {
            strcpy(name,(disarm ? "The door trap " : "The door "));
            strcat(name,dirstr[what_dir(px,py,tmp_x[cur_index],tmp_y[cur_index])]);
         }
         else if (tmp_i[cur_index] == -2)
         {
            /* we are here only if we disarm - floors can't be opened! */
            strcpy(name,"The floor trap ");
            strcat(name,dirstr[what_dir(px,py,tmp_x[cur_index],tmp_y[cur_index])]);
         }
         else
         {
            char i_name[80];
            object_type *i_ptr;

            i_ptr = get_item_pointer_xy(tmp_i[cur_index],tmp_x[cur_index],tmp_y[cur_index]);
            object_desc(i_name, i_ptr, FALSE, 3);
            strcpy(name,i_name);
            if (disarm)
               strcpy(name,"trap");
            if ((tmp_x[cur_index]!=px) || (tmp_y[cur_index]!=py))
            {
               strcat(name," ");
               strcat(name,dirstr[what_dir(px,py,tmp_x[cur_index],tmp_y[cur_index])]);
            }
            else
               strcat(name," in your inventory");
         }
         prt(format("  %c - %s",97+cur_index,name),2, i+1);
      }
      if (cur_max==1)
      {
         if (max_scrn>1)
         {
            prt("Press 'a' to choose this item, space to switch screens, ESC to abort",0,0);
         }
         else
         {
            prt("Press 'a' to choose this item, ESC to abort",0,0);
         }
      }
      else
      {
         if (max_scrn>1)
         {
            prt(format("Press 'a'-'%c' or direction to %s, space to switch screens, ESC to abort",
                       96+cur_max,(disarm? "disarm":"open")),0,0);
         }
         else
         {
            prt(format("Press 'a'-'%c' or direction to %s an item, ESC to abort",
                       96+cur_max,(disarm? "disarm":"open")),0,0);
         }
      }
      c = inkey();
      switch (c)
      {
         case ESCAPE: chosen = TRUE;
                      result   = -1;
                      break;
         case ' ':  cur_scrn++;
                      cur_scrn = cur_scrn % max_scrn;
                      break;

         default: c = (ang_isupper(c) ? tolower(c) : c);
                  if ((c>='a') && (c<='t'))
                  {
                     result = cur_scrn*20+(s16b)c-'a';
                     chosen = TRUE;
                     break;
                  }
                  else if ((c>='1') && (c<='9'))
                  {
                     s16b tx, ty;
                     dir = c-'0';
                     tx = px + ddx[dir];
                     ty = py + ddy[dir];
                     for (i=0; i<cur_max; i++)
                     {
                        if ((tmp_x[i]==tx) && (tmp_y[i]==ty))
                        {
                           result = i;
                           chosen = TRUE;
                           break;
                        }
                     }
                  }
                  else
                    bell("Unrecognized key");
                  break;
      }
   }
   Term_load();
   return (result);
}

/*
 * Open a closed door or closed chest.
 *
 * Note unlocking a locked door/chest is worth one experience point.
 */
void do_cmd_open()
{
/* jk - to get a list of possible things in */
/* max = 24 in inven, and 8 * 16 around us + 16 below us = 168 :-) */
   s16b          number = 0, what;
   static s16b   nx,ny,item;

   if (p_ptr->command_arg)
   {
       /* Set repeat count */
       p_ptr->command_rep = p_ptr->command_arg - 1;

       /* Redraw the state */
       p_ptr->redraw1 |= (PR1_STATE);

       /* Cancel the arg */
       p_ptr->command_arg = 0;
   }

   /* first count in inven and below us */
   count_chests(&number);
   /* then count around us for chests or doors */
   count_open_around_us(&number);

   if (number==0)
   {
     msg_print("There is nothing to open here.");
     p_ptr->command_rep = 0;
     p_ptr->command_dir = 0;
     disturb(0,0);
     /* Redraw the state */
     p_ptr->redraw1 |= (PR1_STATE);
     return;
   }

   if (!p_ptr->command_dir)
   {
      what = disarm_open_dir(number,FALSE);
      p_ptr->command_dir = 1; /* just to signal we are now repeating */
      if (what==-1)
      {
         prt("",0,MESSAGE_ROW);
         msg_print("Opening aborted.");
         p_ptr->command_rep = 0;
         p_ptr->command_dir = 0;
         /* Redraw the state */
         p_ptr->redraw1 |= (PR1_STATE);

         disturb(0,0);
         return;
      }

      nx   = tmp_x[what];
      ny   = tmp_y[what];
      item = tmp_i[what];
   }

   if (!open_something(nx,ny,item))
   {
      /* FALSE signifies no repeating necessary , so don't repeat it */
      p_ptr->command_rep = 0;
      p_ptr->command_dir = 0;
      item = -1;      /* reset the standard item to a door */
      disturb(0,0);
      /* Redraw the state */
      p_ptr->redraw1 |= (PR1_STATE);
   }
}

void count_closables(s16b x, s16b y, s16b *number)
{
   cave_cell_type   *c_ptr;

   if (!in_bounds(x,y)) return;
   c_ptr = &dungeon.level[sublevel][y][x];

   if ( (c_ptr->mtyp == DUNG_DOOR) && (c_ptr->styp == DUNG_DOOR_OPEN) )
   {
      tmp_x[(*number)] = x;
      tmp_y[(*number)++] = y;
      return;
   }
}

/*
 * this function counts how many doors we can close around us
 */
s16b count_closable_around_us(void)
{
   s16b number = 0;

   count_closables(px-1,py,&number);
   count_closables(px-1,py-1,&number);
   count_closables(px-1,py+1,&number);
   count_closables(px+1,py,&number);
   count_closables(px+1,py-1,&number);
   count_closables(px+1,py+1,&number);
   count_closables(px,py-1,&number);
   count_closables(px,py+1,&number);

   return (number);
}

/*
 * Close an open door.
 */
void do_cmd_close()
{
   s16b            x, y, dir, number;
   cave_cell_type *c_ptr;

   /* Allow repeated command */
   if (p_ptr->command_arg)
   {
      /* Set repeat count */
      p_ptr->command_rep = p_ptr->command_arg - 1;

      /* Redraw the state */
      p_ptr->redraw1 |= (PR1_STATE);

      /* Cancel the arg */
      p_ptr->command_arg = 0;
   }

   dir = -1;
   number = count_closable_around_us();

   if (number == 0)
   {
      /* Message */
      msg_print("You see nothing here to close.");
      /* Cancel repeat - always, there's no such thing as a retryable closing */
      disturb(0, 0);
      return;
   }
   else if (number == 1)
   {
      /* be smart if there is only one door in sight */
      dir = what_dir1(px, py, tmp_x[0], tmp_y[0]);
   }
   else
   {
      /* Get a "repeated" direction */
      if (!get_rep_dir(&dir)) return;
   }

   /* Get requested location */
   y = py + ddy[dir];
   x = px + ddx[dir];

   /* Get grid and contents */
   c_ptr = &dungeon.level[sublevel][y][x];

   /* Broken door */
   if ((c_ptr->mtyp == DUNG_DOOR) && (c_ptr->styp == DUNG_DOOR_BROKEN))
   {
      /* Message */
      msg_print("The door appears to be broken.");
   }

   /* Monster in the way */
   else if (c_ptr->m_idx)
   {
      /* Take a turn */
      energy_use = 100;

      /* Message */
      msg_print("There is a monster in the way!");

      /* Attack */
      energy_use = py_attack(x, y);
   }

   /* Item in the way */
   else if (c_ptr->i_idx)
   {
      /* Message */
      msg_print("The door is blocked!");
   }

   /* Require open door */
   else if ((c_ptr->mtyp != DUNG_DOOR) || (c_ptr->styp != DUNG_DOOR_OPEN))
   {
      /* Message */
      msg_print("You see nothing there to close.");
   }

   /* Close the door */
   else
   {
      /* Take a turn */
      energy_use = 100;

      /* Close the door */
      set_grid_type(x, y, DUNG_DOOR, DUNG_DOOR_CLOSED, GRID_ADD, 0);
      c_ptr->fdat &= ~(CAVE_WALK|CAVE_LIGHT|CAVE_MAGIC);

      /* Notice */
      note_spot(x, y);

      /* Redraw */
      lite_spot(x, y);

      /* Update some things */
      p_ptr->update |= (PU_VIEW | PU_MONSTERS);
   }

   /* Cancel repeat - always, there's no such thing as a retryable closing */
   disturb(0, 0);
}

/*
 * Tunnel through wall.  Assumes valid location.
 *
 * Note that it is impossible to "extend" rooms past their
 * outer walls (which are actually part of the room).
 *
 * This will, however, produce grids which are NOT illuminated
 * (or darkened) along with the rest of the room.
 */
bool twall(s16b x, s16b y)
{
   cave_cell_type   *c_ptr = &dungeon.level[sublevel][y][x];

   /* Paranoia -- Require a wall or door or some such */
   if ( (c_ptr->mtyp != DUNG_WALL) &&
        (c_ptr->styp != DUNG_DOOR)) return (FALSE);

   /* Remove the feature */
   (void)set_grid_type(x, y, DUNG_FLOOR, DUNG_FLOOR_NORMAL, GRID_ADD, 0);
   if (c_ptr->t_idx!=-1)
   {
      t_list[c_ptr->t_idx].inuse=FALSE; /* item can be reused now */
      c_ptr->t_idx = 0;      /* and no traps either */
   }

   c_ptr->fdat &= ~CAVE_MARK;      /* Forget the "field mark" */

   /* Notice */
   note_spot(x, y);

   /* Redisplay the grid */
   lite_spot(x, y);

   /* Update some things */
   p_ptr->update |= (PU_VIEW | PU_FLOW | PU_MONSTERS);
   (void)update_stuff();

   /* Result */
   return (TRUE);
}

/*
 * Tunnels through "walls" (including rubble and closed doors)
 *
 * Note that tunneling almost always takes time, since otherwise
 * you can use tunnelling to find monsters.  Also note that you
 * must tunnel in order to hit invisible monsters in walls (etc).
 *
 * Digging is very difficult without a "digger" weapon, but can be
 * accomplished by strong players using heavy weapons.
 */
/* jk - acid branded weapons dig much more easily */
void do_cmd_tunnel()
{
   s16b                 x, y, dir, chance;

   cave_cell_type           *c_ptr;

   /* jk */
   object_type         *i_ptr = &inventory[INVEN_WIELD];
   u64b                f1, f2,f3;
   s16b                 acid_factor = 0;

   bool old_floor = FALSE;

   bool more = FALSE;

   /* jk */
   object_flags(i_ptr, &f1, &f2, &f3);

   if ((f1 & TR1_BRAND_ACID) ||
       test_acid_brand_extra()) acid_factor = 1;

   /* Allow repeated command */
   if (p_ptr->command_arg)
   {
      /* Set repeat count */
      p_ptr->command_rep = p_ptr->command_arg - 1;

      /* Redraw the state */
      p_ptr->redraw1 |= (PR1_STATE);

      /* Cancel the arg */
      p_ptr->command_arg = 0;
   }

   /* Get a direction to tunnel, or Abort */
   if (get_rep_dir(&dir))
   {
      /* Get location */
      y = py + ddy[dir];
      x = px + ddx[dir];

      /* Get grid */
      c_ptr = &dungeon.level[sublevel][y][x];

      /* Check the floor-hood */
      old_floor = floor_grid_bold(x, y);

      /* No tunnelling through emptiness */
      if ((c_ptr->mtyp != DUNG_WALL) && (c_ptr->mtyp != DUNG_PERWALL) &&
          (c_ptr->mtyp != DUNG_DOOR))
      {
         /* Message */
         msg_print("You see nothing there to tunnel through.");
      }

      /* No tunnelling through doors */
      else if (c_ptr->mtyp == DUNG_DOOR)
      {
         if (c_ptr->styp == DUNG_DOOR_SECRET)
         {
            msg_print("You have found a secret door."); /* Message */
            chance=randint(150-p_ptr->mdepth);
            if (c_ptr->fdat & CAVE_VAULT) chance=chance/4;
            if (chance<5)
            {
               (void)set_grid_type(x,y,DUNG_DOOR,
                                   DUNG_DOOR_JAMMED, GRID_ADD, 0);
            }
            else if (chance<10)
            {
               (void)set_grid_type(x,y,DUNG_DOOR,
                                   DUNG_DOOR_LOCKED, GRID_ADD, 0);
            }
            else
            {
               (void)set_grid_type(x,y,DUNG_DOOR,
                                   DUNG_DOOR_CLOSED, GRID_ADD, 0);
            }
            note_spot(x, y); /* Notice */
            lite_spot(x, y); /* Redraw */
            disturb(0, 0); /* Disturb */
         }
         else /* Message */
            msg_print("You cannot tunnel through doors.");
      }

      /* A monster is in the way */
      else if (c_ptr->m_idx)
      {
         /* Take a turn */
         energy_use = 100;

         /* Message */
         msg_print("There is a monster in the way!");

         /* Attack */
         energy_use = py_attack(x, y);
      }

      else if (wall_art(c_ptr->styp) == DUNG_WALL_ART_RUBBLE)
      {
         /* Remove the rubble */
         if ((p_ptr->skill_dig > rand_int(200)) && twall(x,y))
         {
            /* Message */
            msg_print("You have removed the rubble.");

            /* Hack -- place an object */
            if (rand_int(100) < 10)
            {
               place_object(x, y, FALSE, FALSE, FALSE);
               if (player_can_see_bold(x, y))
               {
                  msg_print("You have found something!");
               }
            }
            note_spot(x, y);  /* Notice */
            lite_spot(x, y);  /* Display */
         }

         else
         {
            msg_print("You dig in the rubble."); /* Message, keep digging */
            more = TRUE;
         }
      }

      /* Okay, try digging */
      else
      {
         bool gold = FALSE;
         bool hard = FALSE;
         bool soft = FALSE;
         s16b acid = 0;
         s16b skill_needed;
         s16b skill_chance;
         s16b wall;
         static s16b weight_chance[7] = { 2, 10, 20, 40, 70, 90, 95 };
         s16b weight_index = 0;

         /* Take a turn */
         energy_use = 100;

         if (treasure(x,y)) gold=TRUE;

         wall = wall_art(c_ptr->styp);
         switch (wall)
         {
            case DUNG_WALL_ART_QUARTZ:  hard=TRUE; break;
            case DUNG_WALL_ART_MAGMA:   acid=1; hard=TRUE; break;
            case DUNG_WALL_ART_CHALK:   acid=4; soft=TRUE; break;
            case DUNG_WALL_ART_GRANITE: acid=2; hard=TRUE; break;
         }

         /* a puny weapon gives no good results, unless you are *really* strong */
         if ((i_ptr->weight<50) && (adj_str_dig[p_ptr->stat_ind[A_STR]] < 100))
            weight_index = 0;
         /* a heavy weapon does well */
         else if ((i_ptr->weight>300) || (i_ptr->tval == TV_DIGGING))
            weight_index = 6;
         else
            weight_index = (i_ptr->weight-50) / 50;

         /* what do we need: from 2000-500-1500 (acid, soft) */
         /*                  to   7000 (hard) */
         /* soft = 1500, takes 15 turns. hard = 7000 takes 70 turns */
         skill_needed = 2000+5000*(hard?1:0)-500*(soft?1:0)-(acid_factor?1500*acid:0);
         skill_chance = randint(skill_needed + 10 * p_ptr->skill_dig);

         /* try it with a little weapon and it won't work */
         if (randint(100)>weight_chance[weight_index]) skill_chance = 0;

         /* if you have a low strength, you need a lot of skill */
         /* unless you wield a digger */
         if ((randint(100)>adj_str_dig[p_ptr->stat_ind[A_STR]]) && (i_ptr->tval != TV_DIGGING))
            skill_chance = skill_chance / 2;

         /* Titanium */
         if (c_ptr->mtyp == DUNG_PERWALL)
         {
            msg_print("This seems to be permanent rock.");
         }

         else if ((weight_index == 0) && (acid_factor == 0))
         {
            if (p_ptr->pclass == CLASS_HIGHPRST)
            {
               msg_print("You break a nail and stop digging with your hands.");
            }
            else
            {
               msg_print("You soon realize your error and stop digging with your puny tool.");
            }   
         }

         else if ((skill_chance > skill_needed) && twall(x, y))
         {
            msg_print("You have finished the tunnel.");
            if (gold)
            {
               place_gold(x, y);          /* Place some gold */
               note_spot(x, y);           /* Notice it */
               lite_spot(x, y);           /* Display it */
               msg_print("You have found something!");
            }
         }

         /* Keep trying */
         else
         {
            /* We may continue tunnelling */
            switch(wall)
            {
               case DUNG_WALL_ART_RUBBLE:
                    msg_print("You tunnel into the rubble."); break;
               case DUNG_WALL_ART_GRANITE:
                    msg_print("You tunnel into the granite wall."); break;
               case DUNG_WALL_ART_CHALK:
                    msg_print("You tunnel into the chalk wall."); break;
               case DUNG_WALL_ART_QUARTZ:
                    msg_print("You tunnel into the quartz wall."); break;
               case DUNG_WALL_ART_MAGMA:
                    msg_print("You tunnel into the magma wall."); break;
               default:
                    msg_print("You tunnel into the wall."); break;
            }
            more = TRUE;
         }
      }

      /* Notice "blockage" changes */
      if (old_floor != floor_grid_bold(x, y))
      {
         /* Update some things */
         p_ptr->update |= (PU_VIEW | PU_FLOW | PU_MONSTERS);
      }
   }

   /* Cancel repetition unless we can continue */
   if (!more) disturb(0, 0);
}

void count_disarmables(s16b x,s16b y,s16b *number)
{
   s16b         k;
   cave_cell_type   *c_ptr;
   object_type *i_ptr;

   if (!in_bounds(x,y)) return;

   c_ptr = &dungeon.level[sublevel][y][x];
#if (debuglevel & DEBUGTRAPS)
   if ((c_ptr->mtyp==DUNG_DOOR) && (c_ptr->t_idx))
   {
      trap_item_type *tr_ptr = &t_list[c_ptr->t_idx];
dlog(DEBUGTRAPS,"cmd2.c: count_disarmables: at %d,%d t_idx %d\n",x,y,c_ptr->t_idx);
dlog(DEBUGTRAPS,"cmd2.c: type %016Lx found %016Lx\n", tr_ptr->type, tr_ptr->found);
dlog(DEBUGTRAPS,"num_traps_ptr always %d never %d no_matter %d\n", num_traps_ptr(tr_ptr, TRAP_FOUND),
                num_traps_ptr(tr_ptr, TRAP_NOTFOUND), num_traps_ptr(tr_ptr, TRAP_EXISTS));
   }
#endif
   /* if it's a door and we've already found traps there */
   if ((c_ptr->mtyp==DUNG_DOOR) && num_traps_xy(x, y, TRAP_FOUND))
   {
      tmp_x[(*number)] = x;
      tmp_y[(*number)] = y;
      tmp_i[(*number)++] = -1; /* signify door */
   }
   if (c_ptr->mtyp==DUNG_TRAP)
   {
      tmp_x[(*number)] = x;
      tmp_y[(*number)] = y;
      tmp_i[(*number)++] = -2; /* signify floor */
   }

   for (k=0;k<objects_on_floor(x,y);k++)
   {
      /* Get the object (if any) */
      i_ptr = get_item_pointer_floor_xy(k,x,y);

      if (i_ptr->tval != TV_CHEST) continue;
      /* don't count known, opened chests */
      if (object_known_p(i_ptr) && (i_ptr->xtra2 == 0)) continue;
      /* don't count unknown chests - first search for traps */
      if (!object_known_p(i_ptr)) continue;

      tmp_x[(*number)] = x;
      tmp_y[(*number)] = y;
      tmp_i[(*number)++] = k+INVEN_TOTAL;
   }
}

void count_disarm_around_us(s16b *number)
{
dlog(DEBUGTRAPS,"cmd2.c: count_disarm_around_us start num %d\n", *number);
   count_disarmables(px-1,py,number);
dlog(DEBUGTRAPS,"cmd2.c: count_disarm_around_us start W   %d\n", *number);
   count_disarmables(px-1,py-1,number);
dlog(DEBUGTRAPS,"cmd2.c: count_disarm_around_us start NW  %d\n", *number);
   count_disarmables(px-1,py+1,number);
dlog(DEBUGTRAPS,"cmd2.c: count_disarm_around_us start SW  %d\n", *number);
   count_disarmables(px+1,py,number);
dlog(DEBUGTRAPS,"cmd2.c: count_disarm_around_us start E   %d\n", *number);
   count_disarmables(px+1,py-1,number);
dlog(DEBUGTRAPS,"cmd2.c: count_disarm_around_us start NE  %d\n", *number);
   count_disarmables(px+1,py+1,number);
dlog(DEBUGTRAPS,"cmd2.c: count_disarm_around_us start SE  %d\n", *number);
   count_disarmables(px,py-1,number);
dlog(DEBUGTRAPS,"cmd2.c: count_disarm_around_us start N   %d\n", *number);
   count_disarmables(px,py+1,number);
dlog(DEBUGTRAPS,"cmd2.c: count_disarm_around_us start S   %d\n", *number);
}

/* jk */
bool disarm_something(s16b x, s16b y, s16b item)
{
   cave_cell_type           *c_ptr = &dungeon.level[sublevel][y][x];
   trap_item_type      *tr_ptr;
   object_type         *i_ptr = NULL;
   trap_type           *t_ptr;
   char                name[80];
   s16b                 i,j,k;
   s16b                 power, trap, cnt;
   s16b                 index[MAX_TRAPS_IN_SET];

   if ((item!=-1) && (item!=-2)) /* we open no door or floor, so get the item */
   {
      i_ptr = get_item_pointer_xy(item,x,y);
      if (i_ptr->number>1)
      {
         msg_print("Stacked chests don't disarm yet");
         return(FALSE);
      }
   }
   /* presumably, item <-2 would have crashed above! */
   if (item<0) /* we look for a door or a floor item? */
   {
      if (!c_ptr->t_idx)
      {
         msg_print("You see nothing there to disarm.");
         return(FALSE);
      }
      else if (c_ptr->mtyp==DUNG_TRAP) /* floor trap */
      {
         tr_ptr = &t_list[c_ptr->t_idx];
dlog(DEBUGTRAPS,"cmd2.c: disarm_something: on floor at %d,%d: i_idx %d m_idx %d t_idx %d\n",
                px, py, c_ptr->i_idx, c_ptr->m_idx, c_ptr->t_idx);
dlog(DEBUGTRAPS,"cmd2.c: disarm_something: fdat %08lx mtyp %d styp %d\n", c_ptr->fdat,c_ptr->mtyp,c_ptr->styp);
         cnt = 0;
         for (k=0;k<MAX_TRAPS_IN_SET;k++)    /* find out which traps exist in tr_ptr */
         {
            /* if they exist and we know they do  */
            if ((tr_ptr->type[k]!=0) && (tr_ptr->found[k]==TRUE))
            {
               index[cnt++]=tr_ptr->type[k];
            }
         }
         if (cnt==0)
         {
            msg_format("disarm_trap: floor 0x10 t_idx %d no traps found at %d,%d",
                      c_ptr->t_idx,x,y);
            msg_print("Disarming non-existant floor trap");
            return(FALSE);
         }
         trap = index[rand_int(cnt)]; /* choose a random trap */
dlog(DEBUGTRAPS,"cmd2.c: disarm_something: %d traps found, chosen %d\n", cnt, trap);
         t_ptr = &t_info[trap];
         sprintf(name,"%s %s",t_ptr->ident?"the":"an",
                      t_ptr->ident?(t_name+t_ptr->name):"unknown trap");
         energy_use = 100;            /* Take a turn */
         i = p_ptr->skill_dis;        /* Get the "disarm" factor */
         if (p_ptr->blind || no_lite()) i = i / 10;
         if (p_ptr->confused || p_ptr->image) i = i / 10;
         if (!t_ptr->ident) i = i / (10-trap_experience(t_ptr));
         power = t_ptr->difficulty;  /* Extract trap "power" */
         j = i - power;              /* Extract the difficulty */
         if (j < 2) j = 2;      /* Always have a small chance of success */
         if (rand_int(100) < j) /* Success */
         {
            msg_format("You have disarmed %s.", name);
            gain_exp(t_ptr->difficulty*10);
            t_ptr->known++;
            set_trap(tr_ptr,trap,FALSE);

dlog(DEBUGTRAPS,"cmd2.c: disarm_something: cnt %d\n", cnt);
            if (!tr_ptr->inuse)
            {
               c_ptr->t_idx = 0; /* no longer any traps */
               (void)set_grid_type(x, y, DUNG_FLOOR, DUNG_FLOOR_NORMAL, GRID_ADD, 0);
               c_ptr->fdat &= ~CAVE_MARK;      /* Forget the "field mark" */

            }
            /* find all other traps on this item */
            if ((cnt-1) < num_traps_ptr(tr_ptr, TRAP_EXISTS) )
            {
               find_all_other_traps(x, y);
            }
            else if (cnt>1)
            {
               msg_print("You have not disarmed all traps yet.");
               /* we just disarmed 1 of 2 traps, so that we now have a single trap left */
               handle_floor_with_traps(tr_ptr, x, y);
            }

            note_spot(x, y);                /* Notice */
            lite_spot(x, y);                /* Redisplay the grid */
            return(FALSE);
         } /* success */
         else if ((i > 5) && (randint(i) > 5))   /* Failure -- Keep trying */
         {
            if (flush_failure) flush();         /* Failure */
            msg_format("You failed to disarm %s.", name); /* Message */
            return(TRUE);                        /* We may keep trying */
         }
         else            /* Failure -- Set off the trap */
         {
            if (num_traps_ptr(tr_ptr,TRAP_EXISTS)>1)
            {
               msg_print("You set off a trap!");
            }
            else
            {
               msg_format("Oops! You touch %s.", name);
            }
            /* Move the player onto the trap */
            move_player(what_dir(px,py,x,y), FALSE, FALSE);
            return(FALSE);
         }
      }
      else if (c_ptr->mtyp==DUNG_DOOR) /* door trap */
      {
         tr_ptr = &t_list[c_ptr->t_idx];
dlog(DEBUGTRAPS,"cmd2.c: disarm_something: on door at %d,%d: i_idx %d m_idx %d t_idx %d\n",
                px, py, c_ptr->i_idx, c_ptr->m_idx, c_ptr->t_idx);
dlog(DEBUGTRAPS,"cmd2.c: disarm_something: fdat %08lx mtyp %d styp %d\n", c_ptr->fdat,c_ptr->mtyp,c_ptr->styp);
         cnt = 0;
         for (k=1;k<t_number;k++)    /* find out which traps exist in tr_ptr */
         {
            if (get_trap(tr_ptr,k) &&  /* if they exist and we know they do  */
                trap_found_ptr(tr_ptr, k)) index[cnt++]=k;
         }
         if (cnt==0)
         {
            msg_format("disarm_trap: door t_idx %d no traps found at %d,%d",
                      c_ptr->t_idx,x,y);
            msg_print("Disarming non-existant door trap");
            return(FALSE);
         }

         trap = index[rand_int(cnt)]; /* choose a random trap */
dlog(DEBUGTRAPS,"cmd2.c: disarm_something: %d traps found, chosen %d\n", cnt, trap);
         t_ptr = &t_info[trap];
         sprintf(name,"%s %s",t_ptr->ident?"the":"an",
             t_ptr->ident?(t_name+t_ptr->name):"unknown trap");

         energy_use = 100;            /* Take a turn */
         i = p_ptr->skill_dis;        /* Get the "disarm" factor */
         if (p_ptr->blind || no_lite()) i = i / 10;
         if (p_ptr->confused || p_ptr->image) i = i / 10;
         if (!t_ptr->ident) i = i / (10-trap_experience(t_ptr));
         power = t_ptr->difficulty;
         j=i-power;
         if (j<2) j=2;
         if (rand_int(100) < j) /* Success */
         {
           msg_format("You sucessfully disable %s on the door.",name);
           set_trap(tr_ptr,trap,FALSE);
           t_ptr->known++;
           if (!tr_ptr->inuse)
           {
             c_ptr->t_idx = 0; /* no longer any traps */
             (void)set_grid_type(x, y, DUNG_DOOR, DUNG_DOOR_CLOSED, GRID_ADD, 0);
             note_spot(x,y);
             lite_spot(x,y);
           }
             if (num_traps_ptr(tr_ptr, TRAP_FOUND) <
                 num_traps_ptr(tr_ptr, TRAP_EXISTS) )
             {
                find_all_other_traps(x, y);
             }
             else if (num_traps_ptr(tr_ptr, TRAP_FOUND) >=1 )
             {
                msg_print("You have not disarmed all traps yet.");
             }
           gain_exp(t_ptr->difficulty*10);
           return (FALSE);
         }
         else if ((i > 5) && (randint(i) > 5))             /* Failure -- Keep trying */
         {
             if (flush_failure) flush();
             msg_format("You failed to disarm %s on the door.",name);
             return(TRUE);
         }
         else
         {
           msg_format("You set off %s!", name);
           player_execute_trap(tr_ptr,-2,x,y, FALSE);
           note_spot(x, y);
           lite_spot(x, y);
           p_ptr->update |= (PU_VIEW);
           update_stuff();
           /* no movement here, since moving onto a closed door with a known */
           /* trap is silly */
           return(FALSE);
         }
      } /* tried all doors */
   } /* tried traps */
   if (!object_known_p(i_ptr))   /* Must find the trap first. */
   {
       msg_print("I don't see any traps.");
       return(FALSE);
   }
   else if (i_ptr->xtra2==-1)  /* Already disarmed/unlocked */
   {
       msg_print("The chest is not trapped.");
       return(FALSE);
   }
   tr_ptr = &t_list[i_ptr->xtra2];
   cnt = 0;
   for (k=1;k<t_number;k++)    /* find out which traps exist in tr_ptr */
   {
     if (get_trap(tr_ptr,k)) index[cnt++]=k;
   }
   if (cnt==0)
   {
      msg_format("disarm_trap: chest xtra2 %d no traps found at %d,%d",
                 i_ptr->xtra2,x,y);
      msg_print("Disarming non-existant chest trap");
      return(FALSE);
   }
   trap = index[rand_int(cnt)]; /* choose a random trap */
   t_ptr = &t_info[trap];
   sprintf(name,"%s %s",t_ptr->ident?"the":"an",
       t_ptr->ident?(t_name+t_ptr->name):"unknown trap");

   energy_use = 100;             /* Take a turn */
   i = p_ptr->skill_dis;         /* Get the "disarm" factor */
   if (p_ptr->blind || no_lite()) i = i / 10; /* Penalize some conditions */
   if (p_ptr->confused || p_ptr->image) i = i / 10;
   if (!t_ptr->ident) i = i / (10-trap_experience(t_ptr));
   power = t_ptr->difficulty;
   j=i-power;
   if (j < 2) j = 2;             /* Always have a small chance of success */
   if (rand_int(100) < j)        /* Success (get a lot of experience) */
   {
       if (cnt>1) 
       {
          msg_print("You have partially disarmed the chest.");
       }
       else
       {
          msg_print("You have disarmed the chest.");
       }
       gain_exp(t_ptr->difficulty*10);
       set_trap(tr_ptr,first_trap(tr_ptr),FALSE);
       t_ptr->known++;
       if (!tr_ptr->inuse) i_ptr->xtra2 = 0; /* no longer any traps */
       return(FALSE);
   }
   else if ((i > 5) && (randint(i) > 5))             /* Failure -- Keep trying */
   {
       if (flush_failure) flush();
       msg_format("You failed to disarm %s on the chest.",name);
       return(TRUE);
   }
   else             /* Failure -- Set off the trap */
   {
       msg_format("You set off %s",name);
       player_execute_trap(tr_ptr, item, x, y, FALSE);
       return(FALSE);
   }
}

/*
 * Disarms a trap, or chest     -RAK-
 */
/* jk - somewhat changed - each line twice or so - JK */
void do_cmd_disarm()
{
   s16b          number = 0, what;
   static s16b   nx, ny, item;

   /* Allow repeated command */
   if (p_ptr->command_arg)
   {
       /* Set repeat count */
       p_ptr->command_rep = p_ptr->command_arg - 1;

       /* Redraw the state */
       p_ptr->redraw1 |= (PR1_STATE);

       /* Cancel the arg */
       p_ptr->command_arg = 0;
   }

   /* first count in inven and below us */
   count_chests(&number);
   /* then count around us for chests or doors */
   count_disarm_around_us(&number);

   if (number==0)
   {
     msg_print("There is nothing to disarm here.");
     p_ptr->command_rep = 0;
     p_ptr->command_dir = 0;
     disturb(0,0);
     /* Redraw the state */
     p_ptr->redraw1 |= (PR1_STATE);
     return;
   }
dlog(DEBUGTRAPS,"cmd2.c: do_cmd_disarm: %d objects found\n", number);

   if (!p_ptr->command_dir)
   {
      what = disarm_open_dir(number,TRUE);
      p_ptr->command_dir = 1; /* just to signal we are now repeating */
      if (what==-1)
      {
         prt("",0,MESSAGE_ROW);
         msg_print("Disarming aborted.");
         p_ptr->command_rep = 0;
         p_ptr->command_dir = 0;
         /* Redraw the state */
         p_ptr->redraw1 |= (PR1_STATE);

         disturb(0,0);
         return;
      }

      nx   = tmp_x[what];
      ny   = tmp_y[what];
      item = tmp_i[what];
   }
dlog(DEBUGTRAPS,"cmd2.c: do_cmd_disarm: nx,ny %d,%d item %d\n",
                nx, ny, item);

   if (!disarm_something(nx,ny,item))
   {
      /* FALSE signifies no repeating necessary , so don't repeat it */
      p_ptr->command_rep = 0;
      p_ptr->command_dir = 0;
      item = -1;      /* reset the standard item to a door */
      disturb(0,0);
      /* Redraw the state */
      p_ptr->redraw1 |= (PR1_STATE);
   }
dlog(DEBUGTRAPS,"cmd2.c: do_cmd_disarm: returning\n");
}

/*
 * Bash open a door, success based on character strength
 *
 * For a closed door, p1val is positive if locked; negative if stuck.
 *
 * For an open door, p1val is positive for a broken door.
 *
 * A closed door can be opened - harder if locked. Any door might be
 * bashed open (and thereby broken). Bashing a door is (potentially)
 * faster! You move into the door way. To open a stuck door, it must
 * be bashed. A closed door can be jammed (see do_cmd_spike()).
 *
 * Creatures can also open or bash doors, see elsewhere.
 *
 * We need to use character body weight for something, or else we need
 * to no longer give female characters extra starting gold.
 */
void do_cmd_bash()
{
   s16b                x, y, dir;
   s16b                bash, temp;
   cave_cell_type          *c_ptr;
   bool                more = FALSE;


   /* Allow repeated command */
   if (p_ptr->command_arg)
   {
      /* Set repeat count */
      p_ptr->command_rep = p_ptr->command_arg - 1;

      /* Redraw the state */
      p_ptr->redraw1 |= (PR1_STATE);

      /* Cancel the arg */
      p_ptr->command_arg = 0;
   }

   /* Get a "repeated" direction */
   if (get_rep_dir(&dir))
   {
      /* Bash location */
      x = px + ddx[dir];
      y = py + ddy[dir];

      /* Get grid */
      c_ptr = &dungeon.level[sublevel][y][x];

      /* Only doors & known traps can be bashed */
      if ( (c_ptr->mtyp != DUNG_DOOR) &&
           (c_ptr->mtyp != DUNG_TRAP) )
      {
         /* Message */
         msg_print("You see nothing there to bash.");
      }
      else if ( c_ptr->mtyp == DUNG_DOOR &&
                ( (c_ptr->styp == DUNG_DOOR_OPEN) ||
                  (c_ptr->styp == DUNG_DOOR_BROKEN) ) )
      {
         /* Message */
         msg_print("There's no need to bash an open door.");
      }

      /* Monster in the way */
      else if (c_ptr->m_idx)
      {
         /* Take a turn */
         energy_use = 100;

         /* Message */
         msg_print("There is a monster in the way!");

         /* Attack */
         energy_use = py_attack(x, y);
      }

      /* Bash a closed door */
      else if (c_ptr->mtyp == DUNG_DOOR)
      {
         /* Take a turn */
         energy_use = 100;

         /* Message */
         msg_print("You smash into the door!");

         if (c_ptr->t_idx)
         {
            trap_item_type *tr_ptr = &t_list[c_ptr->t_idx];
            energy_use = 100;
            msg_print("You touch the door, and ....");
            player_execute_trap(tr_ptr,-2,x,y, TRUE);
            note_spot(x, y);
            lite_spot(x, y);
            p_ptr->update |= (PU_VIEW);
            update_stuff();
            return;
         }

         /* Hack -- Bash power based on strength */
         /* (Ranges from 3 to 20 to 100 to 200) */
         bash = adj_str_blow[p_ptr->stat_ind[A_STR]];

         /* Compare bash power to door power XXX XXX XXX */
         temp = (bash - c_ptr->extra * 10);

         /* Hack -- always have a chance */
         if (temp < 1) temp = 1;

         /* Hack -- attempt to bash down the door */
         if (rand_int(100) < temp)
         {
            /* Message */
            msg_print("The door crashes open!");

            /* Break down the door */
            if (rand_int(100) < 50)
            {
               (void)set_grid_type(x, y, DUNG_DOOR,
                                   DUNG_DOOR_BROKEN, GRID_KEEP, 0);
            }

            /* Open the door */
            else
            {
               (void)set_grid_type(x, y, DUNG_DOOR,
                                   DUNG_DOOR_OPEN, GRID_KEEP, 0);
            }

            /* Notice */
            note_spot(x, y);

            /* Redraw */
            lite_spot(x, y);

            /* Hack -- Fall through the door */
            move_player(dir, FALSE, FALSE);

            /* Update some things */
            p_ptr->update |= (PU_VIEW);
            p_ptr->update |= (PU_DISTANCE);
         }

         /* Saving throw against stun */
         else if (rand_int(100) < adj_dex_safe[p_ptr->stat_ind[A_DEX]] +
                                  p_ptr->lev)
         {
            /* Message */
            msg_print("The door holds firm.");

            /* Allow repeated bashing */
            more = TRUE;
         }

         /* High dexterity yields coolness */
         else
         {
            /* Message */
            msg_print("You are off-balance.");

            /* Hack -- Lose balance ala paralysis */
            (void)set_paralyzed(p_ptr->paralyzed + 2 + rand_int(2));
         }
      }
      /* Bash a closed door */
      else if (c_ptr->mtyp == DUNG_TRAP)
      {
         trap_item_type *tr_ptr = &t_list[c_ptr->t_idx];

         /* Take a turn */
         energy_use = 100;

         /* Message */
         msg_print("You smash the trap!");

         energy_use = 100;
         player_execute_trap(tr_ptr,-2,x,y, TRUE);

         /* Notice */
         note_spot(x, y);

         /* Redraw */
         lite_spot(x, y);

         /* Update some things */
         p_ptr->update |= (PU_VIEW);
         p_ptr->update |= (PU_DISTANCE);
      }
   }

   /* Unless valid action taken, cancel bash */
   if (!more) disturb(0, 0);
}

/*
 * Find the index of some "spikes", if possible.
 *
 * XXX XXX XXX Let user choose a pile of spikes, perhaps?
 */
/* jk - spikes on the floor not usable ? */
/* now they are */
static bool get_spike(s16b *ip)
{
   s16b i;

   /* Check every item in the pack */
   for (i = 0; i < INVEN_PACK; i++)
   {
      object_type *i_ptr = &inventory[i];

      /* Check the "tval" code */
      if (i_ptr->tval == TV_SPIKE)
      {
         /* Save the spike index */
         (*ip) = i;

         /* Success */
         return (TRUE);
      }
   }
   for (i = 0; i<objects_on_floor(px,py);i++)
   {
      object_type *i_ptr = get_item_pointer_floor(i);
      /* Check the "tval" code */
      if (i_ptr->tval == TV_SPIKE)
      {
         /* Save the spike index and signify it's on the floor! */
         (*ip) = i+INVEN_TOTAL;
         /* Success */
         return (TRUE);
      }
   }
   /* Oops */
   return (FALSE);
}

/*
 * Jam a closed door with a spike
 *
 * This command may NOT be repeated
 */
void do_cmd_spike()
{
   s16b                  x, y, dir, item;

   cave_cell_type       *c_ptr;


   /* Get a "repeated" direction */
   if (get_rep_dir(&dir))
   {
      /* Get location */
      x = px + ddx[dir];
      y = py + ddy[dir];

      /* Get grid and contents */
      c_ptr = &dungeon.level[sublevel][y][x];

      /* Require closed door */
      if ( (c_ptr->mtyp != DUNG_DOOR) ||
           ( (c_ptr->styp == DUNG_DOOR_OPEN) ||
             (c_ptr->styp == DUNG_DOOR_BROKEN) ) )
      {
         /* Message */
         msg_print("You see nothing there to spike.");
      }

      /* Get a spike */
      else if (!get_spike(&item))
      {
         /* Message */
         msg_print("You have no spikes!");
      }

      /* Is a monster in the way? */
      else if (c_ptr->m_idx)
      {
         /* Take a turn */
         energy_use = 100;

         /* Message */
         msg_print("There is a monster in the way!");

         /* Attack */
         energy_use = py_attack(x, y);
      }

      /* Go for it */
      else
      {
         /* Take a turn */
         energy_use = 100;

         /* Successful jamming */
         msg_print("You jam the door with a spike.");

         /* Convert "locked" to "stuck" XXX XXX XXX */
         if (c_ptr->styp != DUNG_DOOR_JAMMED)
            (void)set_grid_type(x, y, DUNG_DOOR, DUNG_DOOR_JAMMED, GRID_KEEP, 0);

         /* Add one spike to the door XXX XXX XXX */
         c_ptr->extra++;

         /* Use up, and describe, a single spike, from the bottom */
         item_increase(item, -1, px, py);
         item_describe(item, px, py);
         item_optimize(item, px, py);
      }
   }
}

/* if we slide, we may have moved to a spot where on the next turn in the */
/* sliding dir, we hit a wall or something else. It doesn't do to take action */
/* then, so we do that here. */
bool test_sliding(s16b dir)
{
   s16b                 x, y;
   cave_cell_type      *c_ptr;
   monster_type        *m_ptr;

   /* Find the result of moving */
   y = py + ddy[dir];
   x = px + ddx[dir];

   /* Examine the destination */
   c_ptr = &dungeon.level[sublevel][y][x];
dlog(DEBUGMOVES,"cmd2.c: test_sliding at %d,%d: i_idx %d m_idx %d t_idx %d\n", x, y,
          c_ptr->i_idx, c_ptr->m_idx, c_ptr->t_idx);
dlog(DEBUGMOVES,"fdat %08lx mtyp %d styp %d\n", c_ptr->fdat,c_ptr->mtyp,c_ptr->styp);
   /* Get the monster */
   m_ptr = &mn_list[c_ptr->m_idx];

   /* Hack -- attack monsters */
   if (c_ptr->m_idx)
   {
      p_ptr->sliding_now=FALSE;  /* we just collided on a monster */
      sliding_dir=-1;
      if (p_ptr->blind)
      {
         msg_print("You hit against some moving object.");
      }
      else
      {
         char            m_name[80];
         monster_type    *m_ptr = &mn_list[c_ptr->m_idx];
         if (m_ptr->ml)
         {
            /* Get the monster name ("a kobold") */
            monster_desc(m_name, m_ptr, 0x08);
         }
         else
            strcpy(m_name, "some shadowy presence");
         msg_format("You slide against %s.", m_name);
         if (m_ptr->csleep)
         {
            m_ptr->csleep = 0;
            msg_format("You've awakened %s.", m_name);
         }
         /* now we attack the player, otherwise he can attack first */
         /* in the next turn */
         if (!make_attack_spell(c_ptr->m_idx))
            make_attack_normal(c_ptr->m_idx);
      }
   }
   else if (!floor_grid_bold(x,y)) /* Player can not walk through "walls" */
   {
      p_ptr->sliding_now=FALSE;  /* we just collided on something else */
      sliding_dir=-1;

      /* Notice things in the dark */
      if (!(c_ptr->fdat & CAVE_MARK) &&
          (p_ptr->blind || !(c_ptr->fdat & CAVE_VIEW)))
      {
         msg_print("Ouch. You bump against something.");
      }
      /* Notice things */
      else
      {
         cptr name;
         s16b f_idx;
         /* what feature type do we hit? */
         f_idx = get_f_idx(c_ptr->mtyp, c_ptr->styp);
         /* please don't give away anything */
         if (f_info[f_idx].flags & CAVE_MIMIC)
         {
            f_idx = get_f_idx(f_info[f_idx].mim_m, f_info[f_idx].mim_s);
         }
         name = f_name + f_info[f_idx].name;

         msg_format("You stop sliding against %s %s.",
                    is_a_vowel(name[0])?"an":"a",name);
      }
   }
   return (!p_ptr->sliding_now);
}

/*
 * Support code for the "Walk" and "Jump" commands
 */
void do_cmd_walk(s16b pickup)
{
   s16b dir;
   bool voluntary = TRUE;

   bool more = FALSE;

   if (p_ptr->sliding)
   {
      voluntary = FALSE;
dlog(DEBUGTRAPS,"cmd2.c: do_cmd_walk: sliding_dir %d\n", sliding_dir);
   }

   /* Allow repeated command */
   if (p_ptr->command_arg)
   {
      /* Set repeat count */
      p_ptr->command_rep = p_ptr->command_arg - 1;

      /* Redraw the state */
      p_ptr->redraw1 |= (PR1_STATE);

      /* Cancel the arg */
      p_ptr->command_arg = 0;
   }

   if ((p_ptr->sliding) && (!p_ptr->sliding_now))
   {
      /* since we walk, we start to slide again! */
dlog(DEBUGTRAPS,"cmd2.c: do_cmd_walk: sliding_now set, dir %d\n", sliding_dir);
      p_ptr->sliding_now = TRUE;
      if (sliding_dir>=0) dir = sliding_dir;
   }

   /* Get a "repeated" direction */
   if (get_rep_dir(&dir))
   {
      if (p_ptr->sliding && p_ptr->sliding_now)
      {
         if (sliding_dir<0) sliding_dir = dir;
         if (dir!=sliding_dir)
         {
            msg_print(slidingstr[rand_int(10)]);
         }
         dir = sliding_dir;
      }
      /* Take a turn */
      energy_use = 100;

      /* Actually move the character */
      move_player(dir, pickup, voluntary);
dlog(DEBUGMOVES,"cmd2.c: do_cmd_walk: move_player called, update %016Lx\n", p_ptr->update);
      if (p_ptr->sliding && p_ptr->sliding_now)
      {
         more = test_sliding(dir);
      }
      else
      {
         /* Allow more walking */
         more = TRUE;
      }
   }

   /* Cancel repeat unless we may continue */
   if (!more) disturb(0, 0);
}

/*
 * Stay still.  Search.  Enter stores.
 * Pick up treasure if "pickup" is true.
 */
static void hold_or_stay(s16b pickup)
{
   cave_cell_type *c_ptr = &dungeon.level[sublevel][py][px];

   /* Allow repeated command */
   if (p_ptr->command_arg)
   {
      /* Set repeat count */
      p_ptr->command_rep = p_ptr->command_arg - 1;

      /* Redraw the state */
      p_ptr->redraw1 |= (PR1_STATE);

      /* Cancel the arg */
      p_ptr->command_arg = 0;
   }

   /* Take a turn */
   energy_use = 100;

   /* Spontaneous Searching */
   if ((p_ptr->skill_pcp >= 50) || (0 == rand_int(50 - p_ptr->skill_pcp)))
   {
      search();
   }

   /* Continuous Searching */
   if (p_ptr->searching)
   {
      search();
   }

   /* Hack -- enter a store if we are on one */
   if (c_ptr->mtyp == DUNG_ENTR)
   {
      /* Disturb */
      disturb(0, 0);

      /* Hack -- enter store */
      p_ptr->command_new = '_';
   }

   /* Try to Pick up anything under us */
   carry(pickup, TRUE);
}

/*
 * Hold still (usually pickup)
 */
void do_cmd_hold(void)
{
   /* Hold still (usually pickup) */
   hold_or_stay(always_pickup);
}

/*
 * Stay still (usually do not pickup)
 */
void do_cmd_stay(void)
{
   /* Stay still (usually do not pickup) */
   hold_or_stay(!always_pickup);
}

/*
 * Resting allows a player to safely restore his hp     -RAK-
 */
void do_cmd_rest(void)
{
   if (p_ptr->movement>4)
   {
      msg_print("You cannot rest while running!");
      return;
   }
   /* Prompt for time if needed */
   if (p_ptr->command_arg <= 0)
   {
      cptr p = "Rest (0-9999, '*' for HP/SP, '&' as needed): ";

      char out_val[80];

      /* Default */
      strcpy(out_val, "&");

      /* Ask for duration */
      if (!get_string(p, out_val, 4)) return;

      /* Rest until done */
      if (out_val[0] == '&')
      {
         p_ptr->command_arg = (-2);
      }

      /* Rest a lot */
      else if (out_val[0] == '*')
      {
         p_ptr->command_arg = (-1);
      }

      /* Rest some */
      else
      {
         p_ptr->command_arg = atoi(out_val);
         if (p_ptr->command_arg <= 0) return;
      }
   }

   /* Paranoia */
   if (p_ptr->command_arg > 9999) p_ptr->command_arg = 9999;

   /* Take a turn XXX XXX XXX (?) */
   energy_use = 100;

   /* Save the rest code */
   p_ptr->resting = p_ptr->command_arg;

   /* Cancel searching */
   p_ptr->searching = FALSE;

   /* Recalculate bonuses */
   p_ptr->update |= (PU_BONUS);

   /* Redraw the state */
   p_ptr->redraw1 |= (PR1_STATE);

   /* Handle stuff */
   handle_stuff();

   /* Refresh */
   Term_fresh();
}

/*
 * Determines the number of the i_ptr objects that break
 * when thrown.
 * Note that "impact" is true if the object hit a monster
 * Artifacts never break, see the "drop_near()" function.
 * Assume the object has NOT hit a wall or monster
 * Hitting a monster doubles the breakage chance
 * Hitting a wall less than 3 grids away does too.
 */
s16b breakage_chance(object_type *i_ptr, bool hit_body)
{
   s16b num, chance, i;
   s16b tvals[] = { TV_FLASK,                     TV_POTION,
                    TV_BOTTLE,                    TV_FOOD,
                    TV_JUNK,                      TV_LITE,
                    TV_SCROLL,                    TV_ARROW,
                    TV_SKELETON,                  TV_WAND,
                    TV_SHOT,                      TV_BOLT,
                    TV_SPIKE,                     TV_CORPSE,
                    TV_HAFTED,                    TV_SWORD,
                    TV_GOLD,                      TV_POLEARM,
                    -1};
   s16b chances[]={ 95,                           50,
                    50,                           50,
                    50,                           30,
                    30,                            8,
                    30,                           20,
                     4,                            8,
                    20,                           20,
                     5,                            5,
                     0,                            5,
                    -1};

   /* Examine the item type - jk - paranoia */
   if (artifact_p(i_ptr)) return (0);

   for (i=0; tvals[i]!=-1; i++)
   {
      if (i_ptr->tval == tvals[i]) break;
   }
   if (tvals[i]==-1)
   {
      /* we hit some unknown item? */
      chance = 10;
   }
   else
   {
      chance = chances[i];
   }

   /* they should be better crafted, or less likely to get rid of if cursed */
   if (ego_item_p(i_ptr))
   {
      chance = chance / 5;
   }

   if (hit_body)
   {
      if ((i_ptr->tval == TV_FLASK) || (i_ptr->tval == TV_BOTTLE) || (i_ptr->tval == TV_POTION))
      {
         chance = 100;
      }
      else
      {
         chance /= 2;
      }
   }

   if (chance>100) chance = 100;
   /* jk - this should give a nice way of breaking some in a bundle of arrows */
   num = 0;
   for (i=0; i<i_ptr->number; i++)
   {
      if (randint(100)<chance) num++;
   }
   return (num);
}


/*
 * Fire an object from the pack or floor.
 *
 * You may only fire items that "match" your missile launcher.
 *
 * You must use slings + pebbles/shots, bows + arrows, xbows + bolts.
 *
 * See "calc_bonuses()" for more calculations and such.
 *
 * Note that "firing" a missile is MUCH better than "throwing" it.
 *
 * Note: "unseen" monsters are very hard to hit.
 *
 * Objects are more likely to break if they "attempt" to hit a monster.
 *
 * Rangers (with Bows) and Anyone (with "Extra Shots") get extra shots.
 *
 * The "extra shot" code works by decreasing the amount of energy
 * required to make each shot, spreading the shots out over time.
 *
 * Note that when firing missiles, the launcher multiplier is applied
 * after all the bonuses are added in, making multipliers very useful.
 *
 * Note that Bows of "Extra Might" get extra range and an extra bonus
 * for the damage multiplier.
 *
 * Note that Bows of "Extra Shots" give an extra shot.
 */
void do_cmd_fire(void)
{
   s16b                 dir, item;
   s16b                 j, x, y, nx, ny, tx, ty;
   s16b                 tdam, tdis, thits, tmul;
   s16b                 bonus, chance;
   s16b                 cur_dis, visible;

   object_type          throw_obj;
   object_type         *i_ptr = NULL;
   object_type         *j_ptr;
   project_who_type     who;

   bool                 hit_body = FALSE;
   bool                 hit_wall = FALSE;

   s16b                 missile_attr;
   s16b                 missile_char;

   char                 i_name[80];

/* jk */
   s16b amt = 1;

   /* Get the "bow" (if any) */
   j_ptr = &inventory[INVEN_BOW];

   /* Require a launcher */
   if (!j_ptr->tval)
   {
       msg_print("You have nothing to fire with.");
       return;
   }

   if (inventory[INVEN_AMMO].k_idx &&
       (!ask_for_other_ammo) &&
       (inventory[INVEN_AMMO].tval == p_ptr->tval_ammo) )
   {
      i_ptr = &inventory[INVEN_AMMO];
      item = INVEN_AMMO;
      object_desc(i_name, i_ptr, FALSE, 3);
      msg_format("You prepare to fire %s %s",
                 (i_ptr->number>1)?"one of your":"your", i_name);
   }
   else
   {
      if (inventory[INVEN_AMMO].k_idx && (!ask_for_other_ammo) )
      {
         msg_print("Your wielded ammunition doesn't suit your wielded missile weapon.");
      }
      /* Require proper missile */
      item_tester_tval = p_ptr->tval_ammo;
      /* Get an item (from equip, inven or floor) */
      if (!get_item(&item, &amt, "Fire which ammunition? ", TRUE, TRUE, TRUE))
      {
         if (item == -2) /* no ammo in inventory */
            msg_print("You have nothing to fire.");
         /* -2 or -1, meaning user hit escape */
         return;
      }

      /* Access the item (if in the pack) */
      i_ptr=get_item_pointer(item);
   }

   /* Get a direction (or cancel) */
   if (!get_aim_dir(&dir)) return;

   /* Create a "local missile object" */
   throw_obj = *i_ptr;
   throw_obj.number = 1;

   /* Reduce and describe inventory */
   item_increase(item, -1, px, py);
   item_describe(item, px, py);
   item_optimize(item, px, py);

   /* Use the missile object */
   i_ptr = &throw_obj;

   /* Describe the object */
   object_desc(i_name, i_ptr, FALSE, 3);

   /* Find the color and symbol for the object for throwing */
   missile_attr = object_attr(i_ptr);
   missile_char = object_char(i_ptr);

   /* Use the proper number of shots */
   thits = p_ptr->num_fire;

   /* Use a base distance */
   tdis = 10;

   /* Base damage from thrown object plus launcher bonus */
   tdam = damroll(i_ptr->dd, i_ptr->ds) + i_ptr->to_d + j_ptr->to_d;

   /* Actually "fire" the object */
   bonus = (p_ptr->to_h + i_ptr->to_h + j_ptr->to_h);
   chance = (p_ptr->skill_thb + (bonus * BTH_PLUS_ADJ));

   /* Assume a base multiplier */
   tmul = 1;

   /* Analyze the launcher */
   tmul = (j_ptr->sval % 10);

   /* Get extra "power" from "extra might" */
   tmul += p_ptr->xtra_might;

   /* Boost the damage */
   tdam *= tmul;

   /* Base range */
   tdis = 10 + 5 * tmul;


   /* Take a (partial) turn */
   energy_use = (100 / thits);


   /* Start at the player */
   y = py;
   x = px;

   /* Predict the "target" location */
   tx = px + 99 * ddx[dir];
   ty = py + 99 * ddy[dir];

   /* Check for "target request" */
   if ((dir == 5) && target_okay())
   {
      tx = target_col;
      ty = target_row;
   }


   /* Hack -- Handle stuff */
   handle_stuff();


   /* Travel until stopped */
   for (cur_dis = 0; cur_dis < tdis; )
   {
      /* Hack -- Stop at the target */
      if ((y == ty) && (x == tx)) break;

      /* Calculate the new location (see "project()") */
      ny = y;
      nx = x;
      mmove2(&nx, &ny, px, py, tx, ty);

      /* Stopped by walls/doors */
      if (!floor_grid_bold(nx,ny)) break;

      /* Advance the distance */
      cur_dis++;

      /* Save the new location */
      x = nx;
      y = ny;


      /* The player can see the (on screen) missile */
      if (panel_contains(x, y) && player_can_see_bold(x, y))
      {
         /* Draw, Hilite, Fresh, Pause, Erase */
         print_rel(missile_char, missile_attr, x, y);
         move_cursor_relative(x, y);
         Term_fresh();
         delay(10 * delay_spd);
         lite_spot(x, y);
         Term_fresh();
      }

      /* The player cannot see the missile */
      else
      {
         /* Pause anyway, for consistancy */
         delay(10 * delay_spd);
      }


      /* Monster here, Try to hit it */
      if (dungeon.level[sublevel][y][x].m_idx)
      {
         cave_cell_type *c_ptr = &dungeon.level[sublevel][y][x];

         monster_type *m_ptr = &mn_list[c_ptr->m_idx];
         monster_race *r_ptr = &r_info[m_ptr->r_idx];

         /* Check the visibility */
         visible = m_ptr->ml;

         /* Did we hit it (penalize range) */
         if (test_hit_fire(chance - cur_dis, r_ptr->ac, m_ptr->ml))
         {
            bool fear = FALSE;

            /* Assume a default death */
            cptr note_dies = " dies.";

            /* Some monsters get "destroyed" */
            if ((r_ptr->flags3 & RF3_DEMON) ||
                (r_ptr->flags3 & RF3_UNDEAD) ||
                (r_ptr->flags2 & RF2_STUPID) ||
                (strchr("Evg", r_ptr->d_char)))
            {
               /* Special note at death */
               note_dies = " is destroyed.";
            }


            /* Note the collision */
            hit_body = TRUE;


            /* Handle unseen monster */
            if (!visible)
            {
               /* Invisible monster */
               msg_format("The %s finds a mark.", i_name);
            }

            /* Handle visible monster */
            else
            {
               char m_name[80];

               /* Get "the monster" or "it" */
               monster_desc(m_name, m_ptr, 0);

               /* Message */
               msg_format("The %s hits %s.", i_name, m_name);

               /* Hack -- Track this monster race */
               if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

               /* Hack -- Track this monster */
               if (m_ptr->ml) health_track(c_ptr->m_idx);
            }

            /* Apply special damage XXX XXX XXX */
            tdam = tot_dam_aux(i_ptr, tdam, m_ptr);
/* jk  TRE mans show messages about it! */
            tdam = critical_shot(i_ptr->weight, i_ptr->to_h, tdam, TRUE);

            /* No negative damage */
            if (tdam < 0) tdam = 0;

            /* Complex message */
            if (wizard)
            {
               msg_format("You do %d (out of %d) damage.",
                          tdam, m_ptr->hp);
            }

            /* Hit the monster, check for death */
            who.type = WHO_PLAYER;
            if (mon_take_hit(&who, c_ptr->m_idx, tdam, &fear, note_dies))
            {
               /* Dead monster */
            }

            /* No death */
            else
            {
               /* Message */
               message_pain(c_ptr->m_idx, tdam);

               /* Take note */
               if (fear && m_ptr->ml)
               {
                  char m_name[80];

                  /* Sound */
                  sound(SOUND_FLEE);

                  /* Get the monster name (or "it") */
                  monster_desc(m_name, m_ptr, 0);

                  /* Message */
                  msg_format("%^s flees in terror!", m_name);
               }
            }
         }

         /* Stop looking */
         break;
      }
   }
   if (!hit_body)
   {
      hit_wall = (!floor_grid_bold(nx, ny));
   }

   /* Chance of breakage */
   /* halve the chance if we hit a monster, except for potions / bottles - they always break */
   j = breakage_chance(i_ptr, hit_body);

   /* Drop (or break) near that location */
   (void)drop_near(i_ptr, j, x, y, DROP_BREAK, hit_body, hit_wall);
}

/*
 * Throw an object from the pack or floor.
 *
 * Note: "unseen" monsters are very hard to hit.
 *
 * Should throwing a weapon do full damage?  Should it allow the magic
 * to hit bonus of the weapon to have an effect?  Should it ever cause
 * the item to be destroyed?  Should it do any damage at all?
 *
 * Do we really need "verification" (note the "destroy" command)?
 */
void do_cmd_throw(void)
{
   s16b               dir, item;
   s16b               j, x, y, nx, ny, tx, ty;
   s16b               chance, tdam, tdis;
   s16b               mul, div;
   s16b               cur_dis, visible;

   object_type        throw_obj;
   object_type       *i_ptr = NULL;
   project_who_type   who;

   bool               hit_body = FALSE;
   bool               hit_wall = FALSE;
   bool               buried = FALSE;

   s16b               missile_attr;
   s16b               missile_char;

   char               i_name[80];

/* jk */
   s16b amt = 1;

   /* Get an item (from inven or floor) */
   if (!get_item(&item, &amt, "Throw which item? ", FALSE, TRUE, TRUE))
   {
      if (item == -2) msg_print("You have nothing to throw.");
      return;
   }

   /* Access the item (if in the pack) */
   i_ptr=get_item_pointer(item);

   /* Get a direction (or cancel) */
   if (!get_aim_dir(&dir)) return;

   /* Create a "local missile object" */
   throw_obj = *i_ptr;
   throw_obj.number = 1;

   /* Reduce and describe inventory */
   item_increase(item, -1, px, py);
   item_describe(item, px, py);
   item_optimize(item, px, py);

   /* Use the local object */
   i_ptr = &throw_obj;

   /* Description */
   object_desc(i_name, i_ptr, FALSE, 3);

   /* Find the color and symbol for the object for throwing */
   missile_attr = object_attr(i_ptr);
   missile_char = object_char(i_ptr);

   /* Extract a "distance multiplier" */
   mul = 10;

   /* Enforce a minimum "weight" of one pound */
   div = ((i_ptr->weight > 10) ? i_ptr->weight : 10);

   /* Hack -- Distance -- Reward strength, penalize weight */
   tdis = (adj_str_blow[p_ptr->stat_ind[A_STR]] + 20) * mul / div;

   /* Max distance of 10 */
   if (tdis > 10) tdis = 10;

   /* Hack -- Base damage from thrown object */
   tdam = damroll(i_ptr->dd, i_ptr->ds) + i_ptr->to_d;

   /* Chance of hitting */
   chance = (p_ptr->skill_tht + (p_ptr->to_h * BTH_PLUS_ADJ));

   /* Take a turn */
   energy_use = 100;

   /* Start at the player */
   x = px;
   y = py;

   /* Predict the "target" location */
   tx = px + 99 * ddx[dir];
   ty = py + 99 * ddy[dir];

   /* Check for "target request" */
   if ((dir == 5) && target_okay())
   {
      tx = target_col;
      ty = target_row;
   }

   /* Hack -- Handle stuff */
   handle_stuff();

   /* Travel until stopped */
   for (cur_dis = 0; cur_dis < tdis; )
   {
      /* Hack -- Stop at the target */
      if ((y == ty) && (x == tx)) break;

      /* Calculate the new location (see "project()") */
      nx = x;
      ny = y;
      mmove2(&nx, &ny, px, py, tx, ty);

      /* Stopped by walls/doors */
      if (!floor_grid_bold(nx,ny)) break;

      /* Advance the distance */
      cur_dis++;

      /* Save the new location */
      x = nx;
      y = ny;

      /* The player can see the (on screen) missile */
      if (panel_contains(x, y) && player_can_see_bold(x, y))
      {
         /* Draw, Hilite, Fresh, Pause, Erase */
         print_rel(missile_char, missile_attr, x, y);
         move_cursor_relative(x, y);
         Term_fresh();
         delay(10 * delay_spd);
         lite_spot(x, y);
         Term_fresh();
      }

      /* The player cannot see the missile */
      else
      {
         /* Pause anyway, for consistancy */
         delay(10 * delay_spd);
      }

      /* Monster here, Try to hit it */
      if (dungeon.level[sublevel][y][x].m_idx)
      {
         cave_cell_type *c_ptr = &dungeon.level[sublevel][y][x];

         monster_type *m_ptr = &mn_list[c_ptr->m_idx];
         monster_race *r_ptr = &r_info[m_ptr->r_idx];

         /* Check the visibility */
         visible = m_ptr->ml;

         /* Did we hit it (penalize range) */
         if (test_hit_fire(chance - cur_dis, r_ptr->ac, m_ptr->ml))
         {
            bool fear = FALSE;

            /* Assume a default death */
            cptr note_dies = " dies.";

            /* Some monsters get "destroyed" */
            if ((r_ptr->flags3 & RF3_DEMON) ||
                (r_ptr->flags3 & RF3_UNDEAD) ||
                (r_ptr->flags2 & RF2_STUPID) ||
                (strchr("Evg", r_ptr->d_char)))
            {
               /* Special note at death */
               note_dies = " is destroyed.";
            }

            /* Note the collision */
            hit_body = TRUE;

            /* Handle unseen monster */
            if (!visible)
            {
               /* Invisible monster */
               msg_format("The %s finds a mark.", i_name);
            }

            /* Handle visible monster */
            else
            {
               char m_name[80];

               /* Get "the monster" or "it" */
               monster_desc(m_name, m_ptr, 0);

               /* Message */
               msg_format("The %s hits %s.", i_name, m_name);

               /* Hack -- Track this monster race */
               if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

               /* Hack -- Track this monster */
               if (m_ptr->ml) health_track(c_ptr->m_idx);
            }

            /* Apply special damage XXX XXX XXX */
            tdam = tot_dam_aux(i_ptr, tdam, m_ptr);
/* jk  TRE mans show messages about it! */
            tdam = critical_shot(i_ptr->weight, i_ptr->to_h, tdam, TRUE);

            /* No negative damage */
            if (tdam < 0) tdam = 0;

            /* Complex message */
            if (wizard)
            {
               msg_format("You do %d (out of %d) damage.",
                          tdam, m_ptr->hp);
            }

            /* Hit the monster, check for death */
            who.type = WHO_PLAYER;
            if (mon_take_hit(&who, c_ptr->m_idx, tdam, &fear, note_dies))
            {
               /* Dead monster */
            }

            /* No death */
            else
            {
               /* Message */
               message_pain(c_ptr->m_idx, tdam);

               /* Take note */
               if (fear && m_ptr->ml)
               {
                  char m_name[80];

                  /* Sound */
                  sound(SOUND_FLEE);

                  /* Get the monster name (or "it") */
                  monster_desc(m_name, m_ptr, 0);

                  /* Message */
                  msg_format("%^s flees in terror!", m_name);
               }
            }
         }

         /* Stop looking */
         break;
      }
   }
   /* if we hit a chalk wall, maybe do something */
   /* Double the chance if we hit a monster */
   j = breakage_chance(i_ptr, hit_body);

   if (!hit_body)
   {
      hit_wall = (!floor_grid_bold(nx, ny));
   }

   if (!hit_body)
   {
      cave_cell_type *c_ptr = &dungeon.level[sublevel][y][x];

      if ( (c_ptr->mtyp == DUNG_WALL) &&
           (wall_art(c_ptr->styp)==DUNG_WALL_ART_CHALK) &&
           (randint(5)==1))
      {
         if (p_ptr->blind)
            msg_print("You hear a wall cave in.");
         else
            msg_format("You see a wall crumble when the %s hits it.",i_name);

         /* we want to hide the missile under the rubble */
         c_ptr->mtyp=DUNG_FLOOR;
         c_ptr->styp=DUNG_FLOOR_NORMAL;
         drop_near(&throw_obj, j, x, y, DROP_DISAPPEAR, FALSE, FALSE);
         c_ptr->styp=DUNG_WALL_RUBBLE;
         note_spot(x,y);
         lite_spot(x,y);
         buried = TRUE;
      }
   }

   if (!buried)
   {
      /* Drop (or break) near that location */
      (void)drop_near(i_ptr, j, x, y, drop_how(i_ptr), hit_body, hit_wall);
   }
}

/* jk */
s16b drop_how(object_type *i_ptr)
{
   switch (i_ptr->tval)
   {
      case TV_BOTTLE:
      case TV_SHOT:
      case TV_ARROW:
      case TV_BOLT:
      case TV_BOW:
      case TV_DIGGING:
      case TV_HAFTED:
      case TV_POLEARM:
      case TV_STAFF:
      case TV_WAND:
      case TV_SWORD:
      case TV_POTION:
           return (DROP_BREAK);

      case TV_NOTHING:
      case TV_FOOD:
      case TV_BOOTS:
      case TV_GLOVES:
      case TV_CLOAK:
      case TV_SOFT_ARMOR:
      case TV_HELM:
           return (DROP_DISAPPEAR);

      case TV_SKELETON:
      case TV_JUNK:
      case TV_SPIKE:
      case TV_CHEST:
      case TV_SHIELD:
      case TV_HARD_ARMOR:
      case TV_DRAG_ARMOR:
      case TV_LITE:
      case TV_AMULET:
      case TV_RING:
      case TV_ROD:
      case TV_FLASK:
      case TV_CROWN:
           return (DROP_SHATTER);

      case TV_SCROLL:
      case TV_CORPSE:
      case TV_BOOK:
           return (DROP_CRUMBLE);

      case TV_GOLD:
           return (DROP_FISSURE);

      default:
           return (DROP_DISAPPEAR);
   }
}
