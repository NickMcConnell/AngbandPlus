/* File: wizard2.c */

/* Purpose: Wizard commands */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

/*
 * Verify use of "wizard" mode
 */
bool enter_wizard_mode(void)
{
   /* No permission */
   if (!can_be_wizard) return (FALSE);

   /* Ask first time */
   if (!(noscore & 0x0002))
   {
      /* Mention effects */
      msg_print("Wizard mode is for debugging and experimenting.");
      msg_print("The game will not be scored if you enter wizard mode.");
      msg_print(NULL);

      /* Verify request */
      if (!get_check("Are you sure you want to enter wizard mode? "))
      {
         return (FALSE);
      }

      /* Mark savefile */
      noscore |= 0x0002;
   }

   /* Success */
   return (TRUE);
}

void do_cmd_toggle_wizard_mode(void)
{
   if (wizard && (debuglevel & DEBUGEXTRA))
   {
      wizard = FALSE;
      msg_print("Wizard mode off.");
   }
   else if (enter_wizard_mode())
   {
      wizard = TRUE;
      msg_print("Wizard mode on.");
   }

   /* Update monsters */
   p_ptr->update |= (PU_MONSTERS);

   /* Redraw "title" */
   p_ptr->redraw1 |= (PR1_TITLE);
}

#ifdef ALLOW_WIZARD

/* jk - allows you to reset some things, starting with the max dungeon level */

void do_cmd_wiz_reset()
{
   char tmp_str[80];
   s16b tmp_val = p_ptr->max_dlv;

   sprintf(tmp_str,"%d",tmp_val);
   if (!get_string("Max dungeon level: ", tmp_str, 9)) return;

   /* Extract */
   tmp_val = atoi(tmp_str);

   /* Verify */
   if (tmp_val < 0) tmp_val = 0L;
   if (tmp_val < p_ptr->mdepth);
   {
      msg_print(
             format("Max dungeon level reset to current level %d, instead of %d",
             p_ptr->mdepth,tmp_val));
      tmp_val = p_ptr->mdepth;
   }
   p_ptr->max_dlv = tmp_val;
}

/*
 * try to generate a room on the next level
 */
void do_cmd_wiz_generate_room()
{
   char tmp_str[80];
   s16b tmp1, tmp2, tmp3, tmp4;

   sprintf(tmp_str,"%d",(s16b)(wizard_target>>24));
   if (!get_string("Room type 1: ", tmp_str, 9)) return;
   tmp1 = (atoi(tmp_str) & 0xff);

   sprintf(tmp_str,"%d",(s16b)((wizard_target & 0x00ff000L)>>16));
   if (!get_string("Room type 2: ", tmp_str, 9)) return;
   tmp2 = (atoi(tmp_str) & 0xff);

   sprintf(tmp_str,"%d",(s16b)((wizard_target & 0x0000ff00L)>>8));
   if (!get_string("Room type 3: ", tmp_str, 9)) return;
   tmp3 = (atoi(tmp_str) & 0xff);

   sprintf(tmp_str,"%d",(s16b)(wizard_target & 0x000000ffL));
   if (!get_string("Room type 4: ", tmp_str, 9)) return;
   tmp4 = (atoi(tmp_str) & 0xff);

   wizard_target=((u32b)tmp1)<<24;
   wizard_target+=((u32b)tmp2)<<16;
   wizard_target+=((u32b)tmp3)<<8;
   wizard_target+=((u32b)tmp4)<<0;

}


/*
 * Hack -- quick debugging hook
 */
static void do_cmd_wiz_hack_ben(void)
{
   msg_print("Not implemented right now.");
}

/*
 * Hack -- Teleport to the target
 */
static void do_cmd_wiz_bamf(void)
{
   /* Must have a target */
   if (!target_who) return;

   /* Teleport to the target */
   teleport_player_to(target_col, target_row);
}

/*
 * Aux function for "do_cmd_wiz_change()".      -RAK-
 */
static void do_cmd_wiz_change_aux(void)
{
   s16b    i;
   s16b    tmp_int;
   long    tmp_long;
   char    tmp_val[160];
   char    ppp[80];

   /* Query the stats */
   for (i = 0; i < 6; i++)
   {
      /* Prompt */
      sprintf(ppp, "%s (3-118): ", stat_names[i]);

      /* Default */
      sprintf(tmp_val, "%d", p_ptr->stat_max[i]);

      /* Query */
      if (!get_string(ppp, tmp_val, 3)) return;

      /* Extract */
      tmp_int = atoi(tmp_val);

      /* Verify */
      if (tmp_int > 18+100) tmp_int = 18+100;
      else if (tmp_int < 3) tmp_int = 3;

      /* Save it */
      p_ptr->stat_cur[i] = p_ptr->stat_max[i] = tmp_int;
   }

   /* Default */
   sprintf(tmp_val, "%ld", (long)(p_ptr->au));

   /* Query */
   if (!get_string("Gold: ", tmp_val, 9)) return;

   /* Extract */
   tmp_long = atol(tmp_val);

   /* Verify */
   if (tmp_long < 0) tmp_long = 0L;

   /* Save */
   p_ptr->au = tmp_long;


   /* Default */
   sprintf(tmp_val, "%ld", (long)(p_ptr->max_exp));

   /* Query */
   if (!get_string("Experience: ", tmp_val, 9)) return;

   /* Extract */
   tmp_long = atol(tmp_val);

   /* Verify */
   if (tmp_long < 0) tmp_long = 0L;

   /* Save */
   p_ptr->max_exp = tmp_long;

   /* Update */
   check_experience();
}

/*
 * Change various "permanent" player variables.
 */
static void do_cmd_wiz_change()
{
   /* Interact */
   do_cmd_wiz_change_aux();

   /* Redraw everything */
   do_cmd_redraw();
}

/* this function allows wizards to alter the terrain */
void do_cmd_wiz_terrain(void)
{
   s16b max_flag = 16;

   s16b mtyp = DUNG_NOTHING;
   s16b styp = DUNG_NOTHING_NORMAL;
   s16b f_num, dir, x, y, i;
   u32b fdat;
   char name[240];
   char ch;
   bool not_valid = FALSE;
   cave_cell_type *c_ptr;
   cptr cave_flags_set[16]=   { "MRK", "GLW", "ICK", "ROM", "LIT",
                                "VIW", "TMP", "XTR", "WLK", "LTH",
                                "MIM", "MAG", "SWM", "NOT", "ARN",
                                "ARC" };
   cptr cave_flags_unset[16]= { "mrk", "glw", "ick", "rom", "lit",
                                "viw", "tmp", "xtr", "wlk", "lth",
                                "mim", "mag", "swm", "not", "arn",
                                "arc" };

   feature_type *f_ptr = &f_info[get_f_idx(mtyp, styp)];
   strcpy(name, f_name + f_ptr->name);

   msg_print(NULL);
   ch = 0;
   while (ch!=KTRL('M'))
   {
      prt(format("MmSs <return>: mtyp %2d styp %2d %s",mtyp,styp,name),0,MESSAGE_ROW);
      inkey_scan=FALSE;
      ch = inkey();
      if (ch=='M') mtyp++;
      if (ch=='m') mtyp--;
      if (ch=='S') styp++;
      if (ch=='s') styp--;
      if (mtyp<0) mtyp = MAX_DUNG_MTYP;
      if (mtyp>MAX_DUNG_MTYP) mtyp = 0;
      if (styp<0) styp = MAX_DUNG_STYP;
      if (styp>MAX_DUNG_STYP) styp = 0;
      f_num = get_f_idx_tolerant(mtyp, styp);
      if (f_num<0)
      {
         strcpy(name,"not valid");
         not_valid = TRUE;
      }
      else
      {
         f_ptr = &f_info[f_num];
         strcpy(name, f_name + f_ptr->name);
         not_valid = FALSE;
      }
   }
   prt("",0,MESSAGE_ROW);
   prt("Enter direction", 0,MESSAGE_ROW);
   if (!get_rep_dir(&dir))
   {
      return;
   }
   /* Get requested location */
   x = px + ddx[dir];
   y = py + ddy[dir];
   if (!in_bounds(x, y))
   {
      msg_print("terrain is out of bounds");
      return;
   }
   c_ptr = &dungeon.level[sublevel][y][x];
   if (c_ptr->m_idx)
   {
      msg_print("terrain contains monster");
      return;
   }
   if (c_ptr->i_idx)
   {
      msg_print("terrain contains item");
      return;
   }
   c_ptr->mtyp = mtyp;
   c_ptr->styp = styp;
   fdat = c_ptr->fdat;
   ch = 0;
   while (ch!=KTRL('M'))
   {
      i = 0;
      prt("",0,MESSAGE_ROW);
      for (i=0; i<max_flag; i++)
      {
         c_put_str(TERM_YELLOW,format("%c",'A'+i), 4*i, 0);
         if (fdat & (1L<<i))
         {
            c_put_str(TERM_WHITE, cave_flags_set[i], 4*i+1, 0);
         }
         else
         {
            c_put_str(TERM_WHITE, cave_flags_unset[i], 4*i+1, 0);
         }
      }
      inkey_scan=FALSE;
      ch = inkey();
      ch=toupper(ch);
      if ((ch>='A') && (ch<('A'+max_flag)))
      {
         i = ch-'A';
         if (fdat & (1L<<i))
            fdat &= ~(1L<<i);
         else
            fdat |= (1L<<i);
      }
   }
   c_ptr->fdat = fdat;
   prt("", 0,MESSAGE_ROW);
}

/* jk */
void wizard_trap_creation(void)
{
   s16b           trap = 1;
   s16b           tr_idx;
   s16b           newp1val = 0;
   s16b           cnt,i,dir,x,y;
   s16b           index[MAX_TRAPS_IN_SET];
   char           ch = ' ';
   bool           ok;
   bool           not_chest = FALSE;
   bool           not_door = FALSE;
   cave_cell_type      *c_ptr;
   trap_type      *t_ptr;
   trap_item_type *tr_ptr;

   t_ptr = &t_info[0];
   tr_idx = t_pop(); /* get a free trap_item */
   if (tr_idx==-1)
   {
      msg_print("Trap item list full - exiting");
      return;
   }
dlog(DEBUGTRAPS,"wizard2.c: wizard_trap_creation: using tr_idx %d\n", tr_idx);
   tr_ptr = &t_list[tr_idx];
   cnt = 0;
   while (cnt<MAX_TRAPS_IN_SET)
   {
      msg_print(NULL);
      prt(format("< > <return> in list %2d trap %2d - %s",cnt,trap,t_name+t_ptr->name),0,MESSAGE_ROW);
      ch = 0;
      while (ch!=KTRL('M'))
      {
         if (ch==ESCAPE)
         {
            prt("", 0, MESSAGE_ROW);
            return;
         }
         if (ch=='<')
         {
            bool cont = TRUE;
            while (cont)
            {
               trap--;
               if (trap<0) trap = t_number - 1;
               cont = FALSE;
               if (t_info[trap].name == 0) cont = TRUE;
            }
         }
         if (ch=='>')
         {
            bool cont = TRUE;
            while (cont)
            {
               trap++;
               if (trap == t_number) trap = 1;
               cont = FALSE;
               if (t_info[trap].name == 0) cont = TRUE;
            }
         }
         t_ptr = &t_info[trap];
         msg_print(NULL);
         prt(format("< > <return> in list %2d current trap %2d - %s",cnt,trap,t_name+t_ptr->name),0,MESSAGE_ROW);
         inkey_scan=FALSE;
         ch = inkey();
      }
      ok=TRUE;
      if (cnt>1)
      {
         for (i=0;i<cnt-1;i++) /* cnt-1, else it's always found */
         {
            if (index[i]==trap)
            {
               msg_print("This trap already is in your list!");
               ok=FALSE;
               break;
            }
         }
      }
      if (ok) index[cnt++]=trap;
      if (!(t_ptr->flags & FTRAP_CHEST)) not_chest = TRUE;
      if (!(t_ptr->flags & FTRAP_DOOR)) not_door = TRUE;
      newp1val += t_ptr->p1valinc;
      msg_print(NULL);
      if (!get_check("Add another trap?")) break;
   }

   tr_ptr->inuse = TRUE;
   for (i=0;i<cnt;i++)
   {
      set_trap(tr_ptr,index[i],TRUE);
dlog(DEBUGTRAPS,"wizard2.c: wizard_trap_creation: setting trap %d to type %d\n",
                i, index[i]);
   }
   if (not_door && not_chest)
   {
      msg_print("Your set of traps can't be combined.");
      return;
   }
   else if (!not_door && !not_chest)
   {
      not_door  = !get_check(format("Put %2d trap(s) on terrain?",cnt));
      not_chest = !not_door;
   }
   if (get_rep_dir(&dir))
   {
      /* Get requested location */
      x = px + ddx[dir];
      y = py + ddy[dir];

      if (not_door)
      {
         object_type forge, *i_ptr;
         s16b k_idx = lookup_kind(TV_CHEST,SV_CHEST_WOODL);

         /* strange things happen otherwise, you have been warned! */
         forge.spell_set = 0;
         invwipe(&forge);

         invcopy(&forge, k_idx);
         apply_magic(&forge, object_level, TRUE, FALSE, FALSE);
         /* remove the traps, if any */
         if (forge.xtra2)
         {
dlog(DEBUGTRAPS,"wizard2.c: wizard_trap_creation: discarding tr_idx %d from chest\n", forge.xtra2);
            t_list[forge.xtra2].inuse = FALSE;
         }
         (void)drop_near(&forge, 0, x, y, 0, FALSE, FALSE);

         if (objects_on_floor(x,y)!=1)
         {
            msg_print("Not just a single object on your square!");
            return;
         }

         i_ptr = get_item_pointer_floor_xy(0,x,y);
         if (i_ptr->tval!=TV_CHEST)
         {
            msg_print("You didn't choose an empty square!");
            return;
         }

         i_ptr->xtra2=tr_idx; /* point to our trap */
dlog(DEBUGTRAPS,"wizard2.c: wizard_trap_creation: giving xtra2 = tr_idx %d to chest\n", i_ptr->xtra2);
dlog(DEBUGTRAPS,"wizard2.c: wizard_trap_creation: exist %d found %d notfound %d\n",
                num_traps_ptr(tr_ptr, TRAP_EXISTS),
                num_traps_ptr(tr_ptr, TRAP_FOUND),
                num_traps_ptr(tr_ptr, TRAP_NOTFOUND));

         i_ptr->p1val = newp1val;
         tr_ptr->tx = -1; /* it's on a chest now */
         tr_ptr->ty = 0;
         msg_format("Created chest %s with %2d trap(s)!",dirstr[dir],cnt);
         note_spot(x,y);
         lite_spot(x,y);
         return;
      }
      else
      {
         c_ptr = &dungeon.level[sublevel][y][x];
         if (c_ptr->t_idx)
         {
            msg_print("terrain already has valid t_idx, trap already present");
            return;
         }
         if ((c_ptr->mtyp == DUNG_FLOOR) && (c_ptr->styp == DUNG_FLOOR_NORMAL))
         {
            (void)set_grid_type(x, y, DUNG_FLOOR, DUNG_FLOOR_TRAP, GRID_ADD, 0);
         }
         else if (c_ptr->mtyp == DUNG_DOOR)
         {
            (void)set_grid_type(x, y, DUNG_DOOR, DUNG_DOOR_CLOSED, GRID_ADD, 0);
         }
         c_ptr->t_idx = tr_idx;
         tr_ptr->tx = x;
         tr_ptr->ty = y;
         msg_format("Created floor %s with %2d trap(s)!",dirstr[dir],cnt);
         return;
      }
   }
}

/*
 * Wizard routines for creating objects         -RAK-
 * And for manipulating them!                   -Bernd-
 *
 * This has been rewritten to make the whole procedure
 * of debugging objects much easier and more comfortable.
 *
 * The following functions are meant to play with objects:
 * Create, modify, roll for them (for statistic purposes) and more.
 * The original functions were by RAK.
 * The function to show an item's debug information was written
 * by David Reeve Sward <sward+@CMU.EDU>.
 *                             Bernd (wiebelt@mathematik.hu-berlin.de)
 *
 * Here are the low-level functions
 * - wiz_display_item()
 *     display an item's debug-info
 * - wiz_get_itemtype()
 *     specify tval and sval (type and subtype of object)
 * - wiz_tweak_item()
 *     specify p1val, +AC, +tohit, +todam
 *     Note that the wizard can leave this function anytime,
 *     thus accepting the default-values for the remaining values.
 *     p1val comes first now, since it is most important.
 * - wiz_reroll_item()
 *     apply some magic to the item or turn it into an artifact.
 * - wiz_roll_item()
 *     Get some statistics about the rarity of an item:
 *     We create a lot of fake items and see if they are of the
 *     same type (tval and sval), then we compare p1val and +AC.
 *     If the fake-item is better or equal it is counted.
 *     Note that cursed items that are better or equal (absolute values)
 *     are counted, too.
 *     HINT: This is *very* useful for balancing the game!
 * - wiz_quantity_item()
 *     change the quantity of an item, but be sane about it.
 *
 * And now the high-level functions
 * - do_cmd_wiz_play()
 *     play with an existing object
 * - wiz_create_item()
 *     create a new object
 *
 * Note -- You do not have to specify "p1val" and other item-properties
 * directly. Just apply magic until you are satisfied with the item.
 *
 * Note -- For some items (such as wands, staffs, some rings, etc), you
 * must apply magic, or you will get "broken" or "uncharged" objects.
 *
 * Note -- Redefining artifacts via "do_cmd_wiz_play()" may destroy
 * the artifact.  Be careful.
 *
 * Hack -- this function will allow you to create multiple artifacts.
 * This "feature" may induce crashes or other nasty effects.
 */

/*
 * this function displays a colored flag-text on screen
 */
static void prt_flag(u64b on_off, char *text, s16b x, s16b y)
{
   if (on_off != 0LL)
   {
      c_put_str(TERM_YELLOW, text, x, y);
   }
   else
   {
      c_put_str(TERM_WHITE, text, x, y);
   }
}

/*
 * Just display an item's properties (debug-info)
 * Originally by David Reeve Sward <sward+@CMU.EDU>
 * Verbose item flags by -Bernd-
 */
static void wiz_display_item(object_type *i_ptr)
{
   s16b        i;
   u64b        f1, f2, f3;
   char        buf[256];

   /* Extract the flags */
   object_flags(i_ptr, &f1, &f2, &f3);

   /* Clear the screen */
   for (i = 1; i <= 23; i++) prt("", 0, i);

   /* Describe fully */
   object_desc_store(buf, i_ptr, TRUE, 3);

   prt(buf, 0, 2);

   prt(format("k_idx %d level %d tv/sv/p1v/p2v = %d/%d/%d/%d val %ld",
               i_ptr->k_idx, k_info[i_ptr->k_idx].level,
               i_ptr->tval, i_ptr->sval, i_ptr->p1val, i_ptr->p2val,
               object_value_real(i_ptr)), 0, 4);

   prt(format("number %d wgt %d damage %dd%d name1/2 %d/%d cost %ld",
               i_ptr->number, i_ptr->weight, i_ptr->dd, i_ptr->ds,
               i_ptr->name1, i_ptr->name2, object_value(i_ptr)), 0, 5);

   prt(format("ac = %d to_a = %d to_h/to_d = %d/%d ident %04x timeout %d",
               i_ptr->ac, i_ptr->to_a, i_ptr->to_h, i_ptr->to_d,
               i_ptr->ident, i_ptr->timeout), 0, 6);


/*
we have a space of 80 x 14
    0         1         2         3         4         5         6         7
    01234567890123456789012345678901234567890123456789012345678901234567890123456789
01: Affect1:  Affect2:  Slay:   Kill: |Sust:   Resist:   | Other Effects:
02: str stlt  str stlt  animal  animal| str   acid  elec |easy_know    activate
03: int srch  int srch  evil    evil  | int   fire  cold |hide_type    drain_exp
04: wis infr  wis infr  undead  undead| wis   pois  fear |show_mods    teleport
05: dex digg  dex digg  demon   demon | dex   lite  dark |insta_art    aggravate
06: con spd   con spd   orc     orc   | con   blnd  conf |featherf     blessed
07: chr blws  chr blws  troll   troll | chr   snd   shrd |perm lite    cursed
08:     mght      mght  giant   giant |       neth  nexs |see_invis    hvy curse
09:     shot      shot  dragon  dragon|       chaos disen|telepathy    prm curse
10:     mgic      mgic                |                  |slw digest
11:     lite      lite                | free act         |regenerate
12:     vamp      vamp  impact        | hold life        |
13: Brand:                            | Immunity:        |Ignore:
14: acid elec                         | acid elec        |acid elec
15: fire cold                         | fire cold        |fire cold

*/
   prt("[   flags1                      ]  [     flags2      ] [   flags3        ]", 0, 8);
   prt("Affect1:  Affect2:  Slay:   Kill:  Sust:   Resist:     Other Effects:", 0, 9);
   prt_flag(f1 & TR1_STR1,      "str", 0, 10);
   prt_flag(f1 & TR1_INT1,      "int", 0, 11);
   prt_flag(f1 & TR1_WIS1,      "wis", 0, 12);
   prt_flag(f1 & TR1_DEX1,      "dex", 0, 13);
   prt_flag(f1 & TR1_CON1,      "con", 0, 14);
   prt_flag(f1 & TR1_CHR1,      "chr", 0, 15);
   prt_flag(f1 & TR1_STEALTH1,  "stlt", 4, 10);
   prt_flag(f1 & TR1_SEARCH1,   "srch", 4, 11);
   prt_flag(f1 & TR1_INFRA1,    "infr", 4, 12);
   prt_flag(f1 & TR1_TUNNEL1,   "digg", 4, 13);
   prt_flag(f1 & TR1_SPEED1,    "spd ", 4, 14);
   prt_flag(f1 & TR1_BLOWS1,    "blws", 4, 15);
   prt_flag(f1 & TR1_MIGHT1,    "mght", 4, 16);
   prt_flag(f1 & TR1_SHOTS1,    "shot", 4, 17);
   prt_flag(f1 & TR1_MAGIC1,    "mgic", 4, 18);
   prt_flag(f1 & TR1_LITE1,     "lite", 4, 19);
   prt_flag(f1 & TR1_VAMPIRIC1, "vamp", 4, 19);

   prt_flag(f1 & TR1_STR2,      "str", 10, 10);
   prt_flag(f1 & TR1_INT2,      "int", 10, 11);
   prt_flag(f1 & TR1_WIS2,      "wis", 10, 12);
   prt_flag(f1 & TR1_DEX2,      "dex", 10, 13);
   prt_flag(f1 & TR1_CON2,      "con", 10, 14);
   prt_flag(f1 & TR1_CHR2,      "chr", 10, 15);
   prt_flag(f1 & TR1_STEALTH2,  "stlt", 14, 10);
   prt_flag(f1 & TR1_SEARCH2,   "srch", 14, 11);
   prt_flag(f1 & TR1_INFRA2,    "infr", 14, 12);
   prt_flag(f1 & TR1_TUNNEL2,   "digg", 14, 13);
   prt_flag(f1 & TR1_SPEED2,    "spd ", 14, 14);
   prt_flag(f1 & TR1_BLOWS2,    "blws", 14, 15);
   prt_flag(f1 & TR1_MIGHT2,    "mght", 14, 16);
   prt_flag(f1 & TR1_SHOTS2,    "shot", 14, 17);
   prt_flag(f1 & TR1_MAGIC2,    "mgic", 14, 18);
   prt_flag(f1 & TR1_LITE2,     "lite", 14, 19);
   prt_flag(f1 & TR1_VAMPIRIC2, "vamp", 14, 19);


   prt_flag(f1 & TR1_IMPACT,   "impact", 20, 20);

   prt("Brand:", 0, 21);
   prt_flag(f1 & TR1_BRAND_ACID, "acid", 0, 22);
   prt_flag(f1 & TR1_BRAND_ELEC, "elec", 5, 22);
   prt_flag(f1 & TR1_BRAND_FIRE, "fire", 0, 23);
   prt_flag(f1 & TR1_BRAND_COLD, "cold", 5, 23);

   prt_flag(f1 & TR1_SLAY_ANIMAL, "animal", 20, 10);
   prt_flag(f1 & TR1_SLAY_EVIL,   "evil  ", 20, 11);
   prt_flag(f1 & TR1_SLAY_UNDEAD, "undead", 20, 12);
   prt_flag(f1 & TR1_SLAY_DEMON,  "demon ", 20, 13);
   prt_flag(f1 & TR1_SLAY_ORC,    "orc   ", 20, 14);
   prt_flag(f1 & TR1_SLAY_ANIMAL, "troll ", 20, 15);
   prt_flag(f1 & TR1_SLAY_GIANT,  "giant ", 20, 16);
   prt_flag(f1 & TR1_SLAY_DRAGON, "dragon", 20, 17);

   prt_flag(f1 & TR1_KILL_ANIMAL, "animal", 28, 10);
   prt_flag(f1 & TR1_KILL_EVIL,   "evil  ", 28, 11);
   prt_flag(f1 & TR1_KILL_UNDEAD, "undead", 28, 12);
   prt_flag(f1 & TR1_KILL_DEMON,  "demon ", 28, 13);
   prt_flag(f1 & TR1_KILL_ORC,    "orc   ", 28, 14);
   prt_flag(f1 & TR1_KILL_ANIMAL, "troll ", 28, 15);
   prt_flag(f1 & TR1_KILL_GIANT,  "giant ", 28, 16);
   prt_flag(f1 & TR1_KILL_DRAGON, "dragon", 28, 17);

   prt_flag(f2 & TR2_SUST_STR,"str", 36, 10);
   prt_flag(f2 & TR2_SUST_INT,"int", 36, 11);
   prt_flag(f2 & TR2_SUST_WIS,"wis", 36, 12);
   prt_flag(f2 & TR2_SUST_DEX,"dex", 36, 13);
   prt_flag(f2 & TR2_SUST_CON,"con", 36, 14);
   prt_flag(f2 & TR2_SUST_CHR,"chr", 36, 15);

   prt_flag(f2 & TR2_RES_ACID,"acid", 42, 10);
   prt_flag(f2 & TR2_RES_ELEC,"elec", 48, 10);

   prt_flag(f2 & TR2_RES_FIRE,"fire", 42, 11);
   prt_flag(f2 & TR2_RES_COLD,"cold", 48, 11);

   prt_flag(f2 & TR2_RES_POIS,"pois", 42, 12);
   prt_flag(f2 & TR2_RES_FEAR,"fear", 48, 12);

   prt_flag(f2 & TR2_RES_LITE,"lite", 42, 13);
   prt_flag(f2 & TR2_RES_DARK,"dark", 48, 13);

   prt_flag(f2 & TR2_RES_BLIND,"blnd", 42, 14);
   prt_flag(f2 & TR2_RES_CONF,"conf", 48, 14);

   prt_flag(f2 & TR2_RES_SOUND,"snd ", 42, 15);
   prt_flag(f2 & TR2_RES_SHARDS,"shrd", 48, 15);

   prt_flag(f2 & TR2_RES_NETHER,"neth", 42, 16);
   prt_flag(f2 & TR2_RES_NEXUS,"nexs", 48, 16);

   prt_flag(f2 & TR2_RES_CHAOS,"chs", 42, 17);
   prt_flag(f2 & TR2_RES_DISEN,"disen", 48, 17);

   prt_flag(f2 & TR2_FREE_ACT,"free act", 36, 19);
   prt_flag(f2 & TR2_HOLD_LIFE,"hold life", 36, 20);

   prt("Immunity:", 36, 21);
   prt_flag(f2 & TR2_IM_ACID, "acid", 36, 22);
   prt_flag(f2 & TR2_IM_ELEC, "elec", 41, 22);
   prt_flag(f2 & TR2_IM_FIRE, "fire", 36, 23);
   prt_flag(f2 & TR2_IM_COLD, "cold", 41, 23);

   prt_flag(f3 & TR3_EASY_KNOW,"easy_know",    54, 10);
   prt_flag(f3 & TR3_HIDE_TYPE,"hide_type",    54, 11);
   prt_flag(f3 & TR3_SHOW_MODS,"show_mods",    54, 12);
   prt_flag(f3 & TR3_INSTA_ART,"insta_art",    54, 13);
   prt_flag(f3 & TR3_FEATHER,"featherf",       54, 14);
   prt_flag(f3 & TR3_SOMELITE,"perm lite",     54, 15);
   prt_flag(f3 & TR3_SEE_INVIS,"see_invis",    54, 16);
   prt_flag(f3 & TR3_TELEPATHY,"telepathy",    54, 17);
   prt_flag(f3 & TR3_SLOW_DIGEST,"slw digest", 54, 18);
   prt_flag(f3 & TR3_REGEN,"regenerate",       54, 19);
   prt_flag(f3 & TR3_ACTIVATE,"activate",      67, 10);
   prt_flag(f3 & TR3_DRAIN_EXP,"drain_exp",    67, 11);
   prt_flag(f3 & TR3_TELEPORT,"teleport",      67, 12);
   prt_flag(f3 & TR3_AGGRAVATE,"aggravate",    67, 13);
   prt_flag(f3 & TR3_BLESSED,"blessed",        67, 14);
   prt_flag(f3 & TR3_CURSED,"cursed",          67, 15);
   prt_flag(f3 & TR3_HEAVY_CURSE, "hvy curse", 67, 16);
   prt_flag(f3 & TR3_PERMA_CURSE, "prm curse", 67, 17);

   prt("Ignore:", 54, 21);
   prt_flag(f3 & TR3_IGNORE_ACID,"acid", 54, 22);
   prt_flag(f3 & TR3_IGNORE_ELEC,"elec", 59, 22);
   prt_flag(f3 & TR3_IGNORE_FIRE,"fire", 54, 23);
   prt_flag(f3 & TR3_IGNORE_COLD,"cold", 59, 23);
}

/*
 * A structure to hold a tval and its description
 */
typedef struct tval_desc
{
   s16b       tval;
   cptr       desc;
   cptr       plural;
} tval_desc;

/*
 * A list of tvals and their textual names
 */
static tval_desc tvals[] =
{
   { TV_SWORD,             "Sword",             "Swords"                },
   { TV_POLEARM,           "Polearm",           "Polearm"               },
   { TV_HAFTED,            "Hafted Weapon",     "Hafted Weapons"        },
   { TV_BOW,               "Bow",               "Bows"                  },
   { TV_ARROW,             "Arrows",            "Arrows"                },
   { TV_BOLT,              "Bolts",             "Bolts"                 },
   { TV_SHOT,              "Shots",             "Shots"                 },
   { TV_SHIELD,            "Shield",            "Shields"               },
   { TV_CROWN,             "Crown",             "Crowns"                },
   { TV_HELM,              "Helm",              "Helms"                 },
   { TV_GLOVES,            "Gloves",            "Gloves"                },
   { TV_BOOTS,             "Boots",             "Boots"                 },
   { TV_CLOAK,             "Cloak",             "Cloaks"                },
   { TV_DRAG_ARMOR,        "Dragon Scale Mail", "Dragon Scale Mail"     },
   { TV_HARD_ARMOR,        "Hard Armor",        "Hard Armor"            },
   { TV_SOFT_ARMOR,        "Soft Armor",        "Soft Armor"            },
   { TV_RING,              "Ring",              "Rings"                 },
   { TV_AMULET,            "Amulet",            "Amulets"               },
   { TV_LITE,              "Lite",              "Lites"                 },
   { TV_POTION,            "Potion",            "Potions"               },
   { TV_SCROLL,            "Scroll",            "Scrolls"               },
   { TV_SPELL,             "Spell",             "Spells"                },
   { TV_BOOK,              "Book of Spells",    "Books of Spells"       },
   { TV_WAND,              "Wand",              "Wands"                 },
   { TV_STAFF,             "Staff",             "Staffs"                },
   { TV_ROD,               "Rod",               "Rods"                  },
   { TV_SPIKE,             "Spikes",            "Spikes"                },
   { TV_DIGGING,           "Digger",            "Diggers"               },
   { TV_CHEST,             "Chest",             "Chests"                },
   { TV_FOOD,              "Food",              "Food"                  },
   { TV_FLASK,             "Flask",             "Flasks"                },
   { TV_SKELETON,          "Skeleton",          "Skeletons"             },
   { TV_CORPSE,            "Corpse",            "Corpses"               },
   { TV_BOTTLE,            "Empty Bottle",      "Empty Bottles"         },
   { TV_JUNK,              "Junk",              "Junk"                  },
   { TV_NOTHING,           NULL,                NULL                    }
};

/*
 * Strip an "object name" into a buffer
 */
static void strip_name(char *buf, s16b k_idx)
{
   char *t;

   object_kind *k_ptr = &k_info[k_idx];

   cptr str = (k_name + k_ptr->name);

   /* Skip past leading characters */
   while ((*str == ' ') || (*str == '&')) str++;

   /* Copy useful chars */
   for (t = buf; *str; str++)
   {
      if (*str != '~') *t++ = *str;
   }

   /* Terminate the new name */
   *t = '\0';
}

/*
 * Specify tval and sval (type and subtype of object) originally
 * by RAK, heavily modified by -Bernd-
 *
 * This function returns the k_idx of an object type, or zero if failed
 *
 * List up to 50 choices in three columns
 *
 * note that if you call it with tval_only FALSE, you will get a k_idx
 *                                         TRUE                  tval
 * returns -1 when aborted
 */
static s16b wiz_get_itemtype(bool tval_only)
{
   s16b                 i, num;
   s16b                 min, max, max_page, max_choice;
   s16b                 col, row;
   s16b                 tval, tval_idx;
   s16b                 choice_idx[512];
   char                 buf[160];
   s16b                 page = 0;

   /* Clear the screen */
   clear_screen();

   /* Print all tval's and their descriptions */
   for (num = 0; (num < 60) && tvals[num].tval; num++)
   {
       row = 2 + (num % 20);
       col = 30 * (num / 20);
/* jk - print numbers instead of chars - some chars aren't accepted */
       prt(format("%3d %s", num, tvals[num].desc), col, row);
   }

   /* Me need to know the maximal possible tval_index */
   max_choice = num;
   max_page = (max_choice / 60)+1;

   /* Choose! */
/* changes here too */
   (void)sprintf(buf,"0");
   if (!get_string(format("Get what type of object?%s ", tval_only?" (-1 for all kinds)":""),
                   buf, (max_choice%10)+1)) return (-1);
   tval_idx = atoi(buf);

   /* Bail out if choice is illegal */
   if ((tval_idx < 0) || (tval_idx >= max_choice)) return (-1);

   /* Base object type chosen, fill in tval */
   tval = tvals[tval_idx].tval;

   /* if that is what we want, return it */
   if (tval_only) return(tval);

   /* now find all possible k_idx'es in all items */
   num = 0;
   for (i=0; i<k_number; i++)
   {
      object_kind *k_ptr = &k_info[i];

      if ((k_ptr->tval != tval) || (k_ptr->flags3 & TR3_INSTA_ART)) continue;

      choice_idx[num++] = i;

      if (num==512)
      {
         quit(format("Too many items with tval %d in wiz_get_itemtype",tval));
      }
   }
   max_choice = num;
   max_page = (max_choice / 60) + 1;

   num = -1;
   page = 1;
   while (num==-1)
   {
      /* Clear the screen */
      clear_screen();

      min = (page-1)*60;
      max = page*60;
      if (max>max_choice) max = max_choice;
      num=0;

      for (i=min; i<max; i++)
      {
         /* Acquire the "name" of object "i" */
         strip_name(buf, choice_idx[i]);

         row = 2 + (num % 20);
         col = 26 * (num / 20);

         /* Print it */
         prt(format("%3d %s", i, buf), col, row);

         num++;
      }

      (void)sprintf(buf,"0");
      if (max_page!=1)
      {
         prt(format("Page %d of %d    ", page, max_page), 30, 23);
         if (!get_string(format("What kind of %s? (<space> for other page) ",
                                tvals[tval_idx].desc),
                         buf, (int)(max_choice/10)+1)) return (-1);
      }
      else
      {
         if (!get_string(format("What kind of %s? ", tvals[tval_idx].desc),
                         buf, (int)(max_choice/10)+1)) return (-1);
      }

      if (buf[0]==' ')
      {
         page++;
         if (page>max_page) page = 1;
         num=-1;
      }
      else
      {
         num = atoi(buf);

         /* Bail out if choice is "illegal" */
         if ((num < 0) || (num >= max_choice)) return (-1);
      }
   }

   /* And return successful */
   return (choice_idx[num]);
}

/*
 * Tweak an item
 */
static void wiz_tweak_item(object_type *i_ptr)
{
   cptr        p;
   char        tmp_val[80];

/* jk - artifacts can be tweaked also */

   p = "Enter new 'p1val' setting: ";
   sprintf(tmp_val, "%ld", i_ptr->p1val);
   if (!get_string(p, tmp_val, 5)) return;
   i_ptr->p1val = atoi(tmp_val);
   wiz_display_item(i_ptr);

   p = "Enter new 'p2val' setting: ";
   sprintf(tmp_val, "%d", i_ptr->p2val);
   if (!get_string(p, tmp_val, 5)) return;
   i_ptr->p2val = atoi(tmp_val);
   wiz_display_item(i_ptr);

   p = "Enter new 'to_a' setting: ";
   sprintf(tmp_val, "%d", i_ptr->to_a);
   if (!get_string(p, tmp_val, 5)) return;
   i_ptr->to_a = atoi(tmp_val);
   wiz_display_item(i_ptr);

   p = "Enter new 'to_h' setting: ";
   sprintf(tmp_val, "%d", i_ptr->to_h);
   if (!get_string(p, tmp_val, 5)) return;
   i_ptr->to_h = atoi(tmp_val);
   wiz_display_item(i_ptr);

   p = "Enter new 'to_d' setting: ";
   sprintf(tmp_val, "%d", i_ptr->to_d);
   if (!get_string(p, tmp_val, 5)) return;
   i_ptr->to_d = atoi(tmp_val);
   wiz_display_item(i_ptr);

   p = "Enter new 'dd' setting: ";
   sprintf(tmp_val, "%d", i_ptr->dd);
   if (!get_string(p, tmp_val, 5)) return;
   i_ptr->dd = (byte)atoi(tmp_val);
   wiz_display_item(i_ptr);

   p = "Enter new 'ds' setting: ";
   sprintf(tmp_val, "%d", i_ptr->ds);
   if (!get_string(p, tmp_val, 5)) return;
   i_ptr->ds = (byte)atoi(tmp_val);
   wiz_display_item(i_ptr);
}

/*
 * Apply magic to an item or turn it into an artifact. -Bernd-
 */
static void wiz_reroll_item(object_type *i_ptr)
{
   object_type mod_item;
   char        ch;
/* jk - invcopy destroys ix,iy in i_ptr - that is nasty! */
   s16b        oldx, oldy, old_spell_set;

   bool        changed = FALSE;

/* jk - if you really want to reroll this... */
   if (artifact_p(i_ptr) && !get_check("Reroll this artifact?")) return;

   oldx = i_ptr->ix;
   oldy = i_ptr->iy;
   old_spell_set = i_ptr->spell_set;

   /* Copy the item to be modified. */
   mod_item = *i_ptr;

   /* Main loop. Ask for magification and artifactification */
   while (TRUE)
   {
      /* Display full item debug information */
      wiz_display_item(&mod_item);

      /* Ask wizard what to do. */
/* jk - allow creation of artifacts */
      if (!get_com("[t]ake this, [n]ormal, [g]ood, [e]xcellent, [a]rtifact? ", &ch))
      {
         changed = FALSE;
         break;
      }

      /* Create/change it! */
      if (ch == 'T' || ch == 't')
      {
         changed = TRUE;
         break;
      }

      /* Apply normal magic, but first clear object */
      else if (ch == 'n' || ch == 'N')
      {
         invcopy(&mod_item, i_ptr->k_idx);
      }

      /* Apply good magic, but first clear object */
      else if (ch == 'g' || ch == 'G')
      {
         invcopy(&mod_item, i_ptr->k_idx);
         apply_magic(&mod_item, p_ptr->mdepth, FALSE, TRUE, FALSE);
         if (artifact_p(&mod_item)) a_info[mod_item.name1].cur_num = 0;
      }

      /* Apply great magic, but first clear object */
      else if (ch == 'e' || ch == 'E')
      {
         invcopy(&mod_item, i_ptr->k_idx);
         apply_magic(&mod_item, p_ptr->mdepth, FALSE, TRUE, TRUE);
         if (artifact_p(&mod_item)) a_info[mod_item.name1].cur_num = 0;
      }
/* jk - allow creation of artifacts */
      /* Apply great magic, but first clear object */
      else if (ch == 'a' || ch == 'A')
      {
         s16b old_depth=p_ptr->mdepth;
         s16b old_obj_level = object_level;
         invcopy(&mod_item, i_ptr->k_idx);
         p_ptr->mdepth= 127;
         object_level = 127;
         (void)make_artifact_special(&mod_item, TRUE);
         if (!mod_item.name1) (void)make_artifact(&mod_item, TRUE);
         p_ptr->mdepth= old_depth;
         object_level = old_obj_level;
         apply_magic(&mod_item, 127, TRUE, TRUE, TRUE);
         if (artifact_p(&mod_item)) a_info[mod_item.name1].cur_num = 0;
      }
   }

   /* Notice change */
   if (changed)
   {
      /* Apply changes */
      *i_ptr = mod_item;

      /* Window stuff */
      p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

      /* Recalculate bonuses */
      p_ptr->update |= (PU_BONUS);

      /* Combine / Reorder the pack (later) */
      p_ptr->notice |= (PN_COMBINE | PN_REORDER);
   }
/* jk - make sure the object is where it's supposed to be */
   i_ptr->ix = oldx;
   i_ptr->iy = oldy;
   i_ptr->log.where = OBJ_FOUND_WIZARD;
   i_ptr->log.whose = 0;
   i_ptr->log.mlevel = p_ptr->mdepth;
   i_ptr->log.slevel = p_ptr->sdepth;
   i_ptr->spell_set = old_spell_set;

}

static void do_cmd_wiz_advanced_statistics(void)
{
   u32b        numfound[MAX_TVAL][MAX_SVAL];
   u32b        ego_items[MAX_K_IDX][255]; /* how many ego-items are found? */
   u32b        artifacts[255];
   u32b        total = 0;

   object_type forge;
   s16b        i, j, k;
   u32b        counter, maxcount = 200000L, showcount, totalcount;
   s16b        k_idx, old_level = 0, new_level = 0, tval_wanted = 0;
   char        name[80], input[80], tval_name[80];
   char        ch;
   cptr        typestr[] = {"Normal", "Good", "Excellent" };
   s16b        itemtype = 0, old_mdepth;
   bool        show_all = FALSE, good = FALSE, great = FALSE, ok = FALSE;
   bool        done_header;

   Term_save();
   Term_clear();

   /* strange things happen otherwise, you have been warned! */
   forge.spell_set = 0;
   invwipe(&forge);

   Term_putstr(5, 2, -1, TERM_YELLOW, "Advanced Item Creation Statistics");
   while (!ok)
   {
      if (tval_wanted)
      {
         i=0;
         while (tvals[i].tval != tval_wanted) i++;
         strcpy(tval_name, tvals[i].desc);
         if (tval_name[strlen(tval_name)-1] != 's') strcat(tval_name,"s");
      }
      Term_putstr(5, 5, -1, TERM_WHITE,
                  format("a - number of rolls                : %08ld", maxcount));
      Term_putstr(5, 7, -1, TERM_WHITE,
                  format("b - item creation level            : %03d", new_level));

      Term_erase(50, 9, 25);
      Term_putstr(5, 9, -1, TERM_WHITE,
                  format("c - item type Normal/Good/Excellent: %s        ",
                         typestr[itemtype]));

      Term_erase(50, 11, 25);
      Term_putstr(5, 11, -1, TERM_WHITE,
                  format("d - All/Generated items in output  : %s",
                         show_all?"All      ":"Generated"));

      Term_erase(50, 13, 25);
      Term_putstr(5, 13, -1, TERM_WHITE,
                  format("e - generate only one type of item : %s",
                         tval_wanted?tval_name:"all items"));

      Term_putstr(5, 15, -1, TERM_YELLOW, "Press a/b/c/d/e to change an option, escape or return to start");

      ch=toupper(inkey());
      if (ch==ESCAPE)
      {
         Term_load();
         return;
      }
      if (ch==KTRL('M'))
      {
         ok=TRUE;
      }
      if (ch=='A')
      {
         Term_gotoxy(42, 5);
         sprintf(input, "%08ld", maxcount);
         if (askfor_aux(input, 8))
         {
            u32b test;
            test = atoi(input);
            if (test<=0)
            {
               if (ring_bell) Term_xtra(TERM_XTRA_NOISE, 0);
            }
            else
               maxcount = test;
         }
      }
      if (ch=='B')
      {
         Term_gotoxy(42, 7);
         sprintf(input, "%03d", new_level);
         if (askfor_aux(input, 3))
         {
            s16b test;
            test = atoi(input);
            if ((test<=0) || (test>127))
            {
               if (ring_bell) Term_xtra(TERM_XTRA_NOISE, 0);
            }
            else
               new_level = test;
         }
      }
      if (ch=='C')
      {
         itemtype++;
         itemtype %= 3;
      }
      if (ch=='D') show_all=!show_all;
      if (ch=='E')
      {
         /* it is possible to specify only tval 0 (nothing) but it won't work */
         tval_wanted = wiz_get_itemtype(TRUE);
         if (tval_wanted == -1)
         {
            tval_wanted = 0;
         }
         if (tval_wanted) show_all = FALSE;
         Term_clear();
      }
   }
   switch(itemtype)
   {
      case 0: good = FALSE; great = FALSE; break;
      case 1: good = TRUE;  great = FALSE; break;
      case 2: good = TRUE;  great = TRUE;  break;
   }

   for (i=0;i<MAX_TVAL;i++)
   {
      for (j=0;j<MAX_SVAL;j++)
      {
         numfound[i][j]=0;
      }
   }

   showcount = maxcount/1000;
   if (!showcount) showcount=1;
   old_level = object_level;
   object_level = new_level;
   counter = 0;
   totalcount=0;
   if (tval_wanted)
   {
      Term_putstr(51, 4, -1, TERM_WHITE,"       total  successfull");
   }
   clear_from(15);
   Term_putstr(5, 15, -1, TERM_WHITE, "Running - press any key to interrupt");

   /* if p_ptr->mpdepth == 0, *NO* artifacts are ever created.. */
   old_mdepth = p_ptr->mdepth;
   p_ptr->mdepth = 1;

   while (counter < maxcount)
   {
      counter++;
      totalcount++;

      create_item(&forge, good, great, FALSE);
      /* if we created an artifact, set the count of creation to 0 again */
      /* do it right here, before break out of this loop                 */
      if (artifact_p(&forge)) a_info[forge.name1].cur_num = 0;

      if (tval_wanted)
      {
         if ((totalcount % showcount) == 0)
         {
            Term_putstr(51, 5, -1, TERM_YELLOW,
                        format("roll %08ld  %08ld",totalcount, counter-1));
            Term_fresh();

            /* Do not wait */
            inkey_scan = TRUE;

            /* Allow interupt */
            if (inkey())
            {
               /* Flush */
               flush();

               /* Stop rolling */
               break;
            }
         }
      }
      else if ((counter % showcount) == 0)
      {
         Term_putstr(51, 5, -1, TERM_YELLOW, format("roll %08ld", counter));
         Term_fresh();
         /* Do not wait */
         inkey_scan = TRUE;

         /* Allow interupt */
         if (inkey())
         {
            /* Flush */
            flush();

            /* Stop rolling */
            break;
         }
      }
      if (forge.name1) artifacts[forge.name1]++;
      if (forge.name2) ego_items[forge.k_idx][forge.name2]++;

      /* recycle trap-pointers on chests, or we will fail */
      if ((forge.tval == TV_CHEST) && (forge.xtra2>0))
      {
         init_trap(&t_list[forge.xtra2]);
      }

      if (tval_wanted && (forge.tval != tval_wanted))
      {
         /* we don't want this item */
         counter--;
         continue;
      }
      numfound[forge.tval][forge.sval]++;
   }
   object_level = old_level;

   for (i=0;i<MAX_TVAL;i++)
   {
      for (j=0;j<MAX_SVAL;j++)
      {
         total+=numfound[i][j];
      }
   }

   Term_putstr(51, 5, -1, TERM_WHITE, "                       ");
   message_add(format("Advanced Statistics: %ld %s %s with object level %d.%s",
                      maxcount, typestr[itemtype],tval_wanted?tval_name:"items",
                      new_level,show_all?" All items shown":""));
   for (i=0;i<MAX_TVAL;i++)
   {
      u32b tval_total = 0;
      u32b artifact_total_per_tval = 0, ego_total_per_tval = 0;
      s16b tval;
       
      clear_from(14);
      Term_putstr(5, 14, -1, TERM_WHITE,
                  format("Evaluating statistics: tval %3d of %d", i, MAX_TVAL));
      Term_fresh();

      tval=0;
      while (tvals[tval].tval && (tvals[tval].tval != i))
      {
         tval++;
      }
      if (!tvals[tval].tval)
      {
         /* we found one of the tvals that isn't yet defined. */
         continue;
      }
      for (k=0; k < MAX_SVAL; k++)
      {
         tval_total += numfound[i][k];
      }
      /* if we didn't ask for everything, and found nothing: next item */
      if (!show_all && (tval_total == 0))
      {
         continue;
      }   

      message_add("==================================================");
      message_add(format("%-47s %6d %6.3f%%",
                  tvals[tval].plural, tval_total, 100.0*(float)tval_total/(float)total));
      message_add("==================================================");

      for (j=0; j<MAX_SVAL; j++)
      {
         bool old_aware;
         bool old_tried;
         u32b artifact_total, ego_total;

         if (show_all || (numfound[i][j]>0))
         {
            /* Look for it */
            for (k_idx = 1; k_idx < k_number; k_idx++)
            {
               object_kind *k_ptr = &k_info[k_idx];
               if ((k_ptr->tval == i) && (k_ptr->sval == j)) break;
            }
            if (k_idx==k_number) continue;
            if (!k_idx) continue;

            /* object_desc isn't really usable here, alas */
            invcopy(&forge, k_idx);
            forge.ident |= ID_KNOWN;

            old_aware = k_info[k_idx].aware;
            k_info[k_idx].aware = TRUE;
            old_tried = k_info[k_idx].tried;
            k_info[k_idx].tried = TRUE;

            if (numfound[i][j]>1) forge.number=2;

            object_desc(name, &forge, FALSE, 0);

            k_info[k_idx].aware = old_aware;
            k_info[k_idx].tried = old_tried;

            /* the phial for example should be displayed as an artifact, */
            /* not as a separate item                                    */
            if ( !(k_info[k_idx].flags3 & TR3_INSTA_ART) )
            {
               message_add(format("  %-45s %6ld %6.3f%%",
                                  name, numfound[i][j],
                                  100.0*(float)numfound[i][j]/(float)total));

            }

            done_header = FALSE;

            artifact_total = 0;
            ego_total = 0;
            for (k=0; k < 255; k++)
            {
               if ( (a_info[k].tval == i) && (a_info[k].sval == j) && (artifacts[k]>0) )
               {
                  artifact_total += artifacts[k];
                  artifact_total_per_tval += artifacts[k];
               }
               if ( ego_items[k_idx][k]>0 )
               {
                  ego_total += ego_items[k_idx][k];
                  ego_total_per_tval += ego_items[k_idx][k];
               }
            }
            for (k=0; k < 255; k++)
            {
               if ( (a_info[k].tval == i) && (a_info[k].sval == j) && (artifacts[k]>0) )
               {
                   char art_name[256];

                   if (!done_header)
                   {
                      message_add(format("   artifacts %5d %6.3f%%                       number total   item   artif",
                                         artifact_total, 100.0*(float)artifact_total/(float)numfound[i][j]));
                      done_header = TRUE;
                   }
                   if ( k_info[k_idx].flags3 & TR3_INSTA_ART )
                   {
                      strcpy(art_name, name);
                      strcat(art_name, " ");
                      strcat(art_name, a_name + a_info[k].name);
                   }
                   else
                   {
                      strcpy(art_name, a_name + a_info[k].name);
                   }
                   message_add(format("         %-40s %4ld %6.3f%% %6.3f%% %6.3f%%",
                                      art_name, artifacts[k],
                                      100.0*(float)artifacts[k]/(float)total,
                                      100.0*(float)artifacts[k]/(float)numfound[i][j],
                                      100.0*(float)artifacts[k]/(float)artifact_total));
               }
            }
            done_header = FALSE;
            for (k=0; k < 255; k++)
            {
               if ( ego_items[k_idx][k]>0 )
               {
                   if (!done_header)
                   {
                      message_add(format("   ego items %5d %6.3f%%                       number total   item    ego",
                                         ego_total, 100.0*(float)ego_total/(float)numfound[i][j]));
                      done_header = TRUE;
                   }
                   message_add(format("         %-40s %4ld %6.3f%% %6.3f%% %6.3f%%",
                                      e_name + e_info[k].name, ego_items[k_idx][k],
                                      100.0*(float)ego_items[k_idx][k]/(float)total,
                                      100.0*(float)ego_items[k_idx][k]/(float)numfound[i][j],
                                      100.0*(float)ego_items[k_idx][k]/(float)ego_total));
               }
            }
         }
      } /* sval loop */
      message_add("==================================================");
      message_add(format("%-17s normal %6d %6.3f%% ego %6d %6.3f%% art %6d %6.3f%%",
                  tvals[tval].plural, tval_total, 100.0*(float)tval_total/(float)total,
                                   ego_total_per_tval, 100.0*(float)ego_total_per_tval/(float)tval_total,
                                   artifact_total_per_tval, 100.0*(float)artifact_total_per_tval/(float)tval_total));
   }
   clear_from(14);
   Term_putstr(5, 21, -1, TERM_WHITE, "Done. View messages for info.");
   pause_line(23);
   p_ptr->mdepth = old_mdepth;
   Term_load();
}

bool wiz_statistics_kind_ok(s16b k_idx)
{
   return ( (k_info[k_idx].tval==(byte)tmp_x[0]) &&
            (k_info[k_idx].sval==(byte)tmp_y[0]));
}

/*
 * Try to create an item again. Output some statistics.    -Bernd-
 *
 * The statistics are correct now.  We acquire a clean grid, and then
 * repeatedly place an object in this grid, copying it into an item
 * holder, and then deleting the object.  We fiddle with the artifact
 * counter flags to prevent weirdness.  We use the items to collect
 * statistics on item creation relative to the initial item.
 */
static void wiz_statistics(object_type *i_ptr)
{
   u32b  i, matches, better, worse, other, max_roll;
   s16b  old_level = 0;
   char  ch;
   char *quality;
   bool  good, great, roll_match;
   char  buf[80];

   object_type test_item;

   cptr q = "Rolls: %ld, Match: %ld, +: %ld, -: %ld, Other: %ld";

   /* XXX XXX XXX Mega-Hack -- allow multiple artifacts */
   if (artifact_p(i_ptr)) a_info[i_ptr->name1].cur_num = 0;

   /* Interact */
   while (TRUE)
   {
      cptr pmt = "Roll for [n]ormal, [g]ood, [e]xcellent or [a]rtifact treasure? ";

      /* Display item */
      wiz_display_item(i_ptr);

      /* Get choices */
      ch='x';
      if (!get_com(pmt, &ch)) break;

      if (ch == 'n' || ch == 'N')
      {
         good = FALSE;
         great = FALSE;
         quality = "normal";
      }
      else if (ch == 'g' || ch == 'G')
      {
         good = TRUE;
         great = FALSE;
         quality = "good";
      }
      else if (ch == 'e' || ch == 'E')
      {
         good = TRUE;
         great = TRUE;
         quality = "excellent";
      }
      else if (ch == 'a' || ch == 'A')
      {
         old_level = p_ptr->mdepth;
         p_ptr->mdepth= 127;
         good = TRUE;
         great = TRUE;
         quality = "artifact";
      }
      else
      {
         good = FALSE;
         great = FALSE;
         break;
      }
      roll_match=get_check("Create only matching items?");

      strcpy(buf, "2000000");

      if (!get_string("Roll how many items?", buf, 32)) break;
      max_roll = atol(buf);
      if (max_roll <= 0) break;

      /* Let us know what we are doing */
      msg_format("Rolling %ld items of %s quality. Base level = %d.",
                 max_roll, quality, p_ptr->mdepth);
      msg_print(NULL);

      /* Set counters to zero */
      matches = better = worse = other = 0;

      if (roll_match)
      {
         get_obj_num_hook = wiz_statistics_kind_ok;
         get_obj_num_prep();
         tmp_x[0] = i_ptr->tval;
         tmp_y[0] = i_ptr->sval;
      }

      /* Let's rock and roll */
      for (i = 0; i < max_roll; i++)
      {
         /* Output every few rolls */
         if (i % 100 == 0)
         {
            /* Do not wait */
            inkey_scan = TRUE;

            /* Allow interupt */
            if (inkey())
            {
               /* Flush */
               flush();

               /* Stop rolling */
               break;
            }

            /* Dump the stats */
            prt(format(q, i, matches, better, worse, other), 0, MESSAGE_ROW);
            Term_fresh();
         }

         /* Create an item */
         create_item(&test_item, good, great, FALSE);

         /* allow multiple artifacts */
         if (artifact_p(&test_item)) a_info[test_item.name1].cur_num = 0;

         /* don't deplete the trap list when creating traps */
         if (test_item.tval == TV_CHEST)
         {
            if (test_item.xtra2) t_list[test_item.xtra2].inuse = FALSE;
            test_item.xtra2 = 0;
         }

         /* Test for the same tval and sval. */
         if ( ((i_ptr->tval) != (test_item.tval)) ||
              ((i_ptr->sval) != (test_item.sval)) )
         {
            continue;
         }

         /* Check for match */
         if ((test_item.p1val == i_ptr->p1val) &&
             (test_item.to_a == i_ptr->to_a) &&
             (test_item.to_h == i_ptr->to_h) &&
             (test_item.to_d == i_ptr->to_d))
         {
            matches++;
         }

         /* Check for better */
         else if ((test_item.p1val >= i_ptr->p1val) &&
                 (test_item.to_a >= i_ptr->to_a) &&
                 (test_item.to_h >= i_ptr->to_h) &&
                 (test_item.to_d >= i_ptr->to_d))
         {
            better++;
         }

         /* Check for worse */
         else if ((test_item.p1val <= i_ptr->p1val) &&
                 (test_item.to_a <= i_ptr->to_a) &&
                 (test_item.to_h <= i_ptr->to_h) &&
                 (test_item.to_d <= i_ptr->to_d))
         {
            worse++;
         }

         /* Assume different */
         else
         {
            other++;
         }
      }

      /* Final dump */
      msg_format(q, i, matches, better, worse, other);
      msg_print(NULL);
   }

   get_obj_num_hook = NULL;
   get_obj_num_prep();

   /* Hack -- Normally only make a single artifact */
   if (artifact_p(i_ptr)) a_info[i_ptr->name1].cur_num = 1;

   if (ch == 'a' || ch == 'A')
   {
      p_ptr->mdepth= old_level;
   }
}

/* jk */
s16b choose_race(s16b start)
{
   char                 c;
   bool                 firsttime = TRUE;
   bool                 searching, skip_key,gone_round;
   s16b                 monster_index = start;
   monster_race        *r_ptr = NULL;
   char                 searchname[80], m_name[80];

   prt("<>#s name:",0,MESSAGE_ROW);
   c=0;
   if (firsttime==TRUE)
   {
       strcpy(searchname,"");
       firsttime     = FALSE;
       searching     = FALSE;
       skip_key      = FALSE;
       monster_index = 1;
   }

   while (!(c==13))
   {
      r_ptr = &r_info[monster_index];
      strcpy(m_name,r_name+r_ptr->name);
      prt(format("<>#s %s num %3d lev %3d hp %5d name: %s",
                 searchname,monster_index, r_ptr->level,
                 (s16b)r_ptr->hdice * (s16b)r_ptr->hside,m_name),0,MESSAGE_ROW);

      if (!skip_key)
      { /* ask for a key? */
         c=inkey();
      }
      else
      {
         skip_key=FALSE;
      }

      switch (c)
      {
         case ESCAPE:
            return (-1);
            break;

         case '<':
         case '4':
            if (searching)
            {
               monster_index--;   /* start with the previous monster */
               if (monster_index==1) monster_index=r_number_total-1;
               r_ptr = &r_info[monster_index];
               strcpy(m_name,r_name+r_ptr->name);
               gone_round=FALSE;
               while ((searching) && (cmp_strngs(m_name,searchname)==FALSE))
               {
                  monster_index--;
                  if ((monster_index==1) && gone_round)
                  {
                     prt(format("searching: %s not found - press a key",searchname),0,MESSAGE_ROW);
                     c=inkey();
                     prt(format("<>#s %s num %3d lev %3d hp %5d name: %s",
                                     searchname,monster_index, r_ptr->level,
                                     (s16b)r_ptr->hdice * (s16b)r_ptr->hside,m_name),0,MESSAGE_ROW);
                     searching=FALSE;
                     monster_index++;
                  }
                  if (monster_index==1)
                  {
                     monster_index=r_number_total-1;
                     gone_round=TRUE;
                  }
                  r_ptr = &r_info[monster_index];
                  strcpy(m_name,r_name+r_ptr->name);
               } /* while */
            } /* if searching */
            else
            {
               monster_index--;
               if (monster_index<1) monster_index=(r_number_total-1);
               r_ptr = &r_info[monster_index];
               strcpy(m_name,r_name+r_ptr->name);
              break;
            } /* not searching */
            break;

         case '>':
         case '6':
           if (searching)
           {
              monster_index++;  /* start looking with the next monster */
              if (monster_index>r_number_total-1) monster_index=2;
              r_ptr = &r_info[monster_index];
              strcpy(m_name,r_name+r_ptr->name);
              gone_round=FALSE;
              while (searching && (cmp_strngs(m_name,searchname)==FALSE))
              {
                 monster_index++;
                 if ((monster_index>r_number_total-1) && gone_round)
                 {
                    prt(format("searching: %s not found - press a key",searchname),0,MESSAGE_ROW);
                    c=inkey();
                    prt(format("<>#s %s num %3d lev %3d hp %5d name: %s",
                               searchname,monster_index, r_ptr->level,
                               (s16b)r_ptr->hdice * (s16b)r_ptr->hside,m_name),0,MESSAGE_ROW);
                    searching=FALSE;
                 }
                 if (monster_index>r_number_total-1)
                 {
                    monster_index=2;
                    gone_round=TRUE;
                 }
                 r_ptr = &r_info[monster_index];
                 strcpy(m_name,r_name+r_ptr->name);
              } /* while */
           } /* if searching */
           else
           {
              monster_index++;
              if (monster_index==r_number_total) monster_index=1;
              r_ptr = &r_info[monster_index];
              strcpy(m_name,r_name+r_ptr->name);
           } /* not searching */
           break;

         case 's': case 'S':
            prt("Enter part of name:",0,MESSAGE_ROW);
            Term_gotoxy(19,0);
            askfor_aux(searchname,30);
            if (searchname[0])
            {
               searching=TRUE;
               c='>';          /* start searching immediately */
               skip_key=TRUE;  /* in the right direction */
            }
            else
            {
               searching = FALSE;
               skip_key = FALSE;
            };
            prt(format("<>#s %s num %3d lev %3d hp %5d name: %s",
                  searchname,monster_index, r_ptr->level,
                  (s16b)r_ptr->hdice * (s16b)r_ptr->hside,m_name),0,MESSAGE_ROW);
            break;
      } /* switch */
   } /* while */
   return (monster_index);
}

/* jk - new function - test your weapon against an imaginary opponent */
void do_cmd_wiz_rate_weapon()
{
   monster_type         *m_ptr; /* what monster do we fight */
   monster_type         my_monster;
   monster_race         *r_ptr = NULL; /* and it's race */
   object_type           throw_obj; /* needed for ammo */
   object_type          *w_ptr = NULL; /* weapon pointer */
   object_type          *a_ptr = NULL; /* ammo pointer   */

   static s16b monster_index;   /* which monster is current */

   s32b max_hp = 0,hp;      /* max and current monster hp */
   s32b turns;              /* turns to test weapon */
   s32b tries;              /* actual number of attacks tried */
   u32b i;                  /* current turn or whatever */
   s16b chance,bonus;       /* fighting chance parameters */
   s16b tohit;              /* to hit bonus on wielded weapon */
   s16b num = 0;            /* current blow */
   s16b dam;                /* damage counter */
   u64b f1,f2,f3;           /* weapon/ammo slay xxx flags */
   u32b critical_damage;    /* temporary critical damage counter */
   u32b total_damage;       /* total damage done until now */
   u32b good_hits;          /* total good hits until now */
   u32b hits;               /* total hits until now */
   u32b good_damage;        /* total good damage until now */
   u32b kills;              /* total kills until now */

   char inputline[30];      /* to hold input in */
   s16b amt = 1;            /* signify firing one arrow at a time :-) */
   bool use_bow;            /* rate bow (TRUE) or normal weapon (FALSE) */
   bool took_swing = FALSE; /* to keep track of rogues hit monster for the */
                            /* first time */
   s16b item;               /* to keep ammo-item in */
   s16b thits = 0;          /* number of hits with bow */
   s16b tmul;               /* damage multiplier with bow */
   s16b distance = 10;      /* at what distance bow attacking */
   char m_name[80];         /* to hold monster name */
   char a_name[80];         /* to hold ammo name */
   char w_name[80], w_name2[80]; /* to hold weapon name */
   char outline[80];        /* to print various info on the screen */
   char c;                  /* keypresses go here */

   character_icky = TRUE;
   Term_save();
   clear_screen();
   monster_index = choose_race(monster_index);
   if (monster_index==-1)
   {
     Term_load();
     character_icky = FALSE;
     return;
   }
   m_ptr = &my_monster;
   r_ptr = &r_info[monster_index];
   max_hp = r_ptr->hside*r_ptr->hside;
   strcpy(m_name,r_name+r_ptr->name);

   c_put_str(TERM_WHITE,"Weapon ratings screen",1,1);
   turns=0;
   while (turns==0)
   {
      prt("turns to test :",1,8);
      Term_gotoxy(16,8);
      (void)sprintf(inputline,"2000");
      askfor_aux(inputline,6);
      turns = atoi(inputline);
      if (turns<=0) turns=0;
   }
   use_bow=FALSE;
   chance=0;
   bonus=0;
   prt("<B>ow <W>eapon <Q>uit:               ",1,8);
   Term_gotoxy(8,23);
   c='A';
   while ( (c!='b') && (c!='B') && (c!='w') && (c!='W') &&
           (c!='Q') && (c!='q'))
   {
      c=inkey();
      if ( (c=='q') || (c=='Q'))
      {
         prt("Press any key to continue",1,23);
         c=inkey();
         Term_load();
         character_icky = FALSE;
         Term_fresh();
         return;
      } /* if quit */
      Term_gotoxy(8,23);
      if ( (c=='b') || (c=='B') )
      {
         w_ptr = &inventory[INVEN_BOW];
         if (!w_ptr->tval)
         {
            msg_print("You have nothing to fire with.");
            c='A';
            continue;
         }
         if (!get_item(&item, &amt, "Fire which item? ", FALSE, TRUE, TRUE))
         {
            if (item == -2) msg_print("You have nothing to fire.");
            c='A';
            continue;
         }
         use_bow=TRUE;
         a_ptr=get_item_pointer(item);
         throw_obj = *a_ptr;
         throw_obj.number = 1;
         a_ptr = &throw_obj;
         prt("Distance:",60,6);
         (void)sprintf(inputline,"10");
         askfor_aux(inputline,8);
         distance = atoi(inputline);
         bonus = (p_ptr->to_h + a_ptr->to_h + w_ptr->to_h);
         chance = (p_ptr->skill_thb + (bonus * BTH_PLUS_ADJ));
      } /* if */
      if ( (c=='w') || (c=='W') )
      {
         w_ptr = &inventory[INVEN_WIELD];
         if (!w_ptr->k_idx)
         {
            msg_print("You wield no weapon");
            c='A';
            continue;
         } /* if */
         /* Extract the flags XXX XXX XXX */
         object_flags(w_ptr, &f1, &f2, &f3);
         bonus = p_ptr->to_h +
                 (p_ptr->twohands ? w_ptr->to_h*3/2 : w_ptr->to_h);
         chance = (p_ptr->skill_thn + (bonus * BTH_PLUS_ADJ));
      } /* if 'w' pressed */
   } /* while */
   prt("                      ",1,8); /* remove <B>ow <W>eapon <Q>uit */
   object_desc(w_name, w_ptr, FALSE, 3);
   prt(format("Attack chance: %3d with %s",chance,w_name),1,7);
   if ( (p_ptr->pclass == CLASS_GLADIATR) && inventory[INVEN_ARM].k_idx)
   {
      bonus = p_ptr->to_h + inventory[INVEN_ARM].to_h;
      chance = (p_ptr->skill_thn + (bonus * BTH_PLUS_ADJ));
      object_desc(w_name2, &inventory[INVEN_ARM], FALSE, 3);
      prt(format("Attack chance: %3d with %s",chance,w_name2),1,8);
   }
   if (use_bow)
   {
      object_desc(a_name, a_ptr, FALSE, 3);
      prt(format("with %s",a_name),20,8);
   }
   tries=0;
   hits=0;
   kills=0;
   good_hits=0;
   good_damage=0;
   total_damage=0;
   hp=max_hp;

   m_ptr->ml=1;                                /* monster visible */
   m_ptr->r_idx=monster_index;
   for (i=0;i<=turns;i++)
   {
      bool have_hit_deep = FALSE;

      if (use_bow)
      {
         thits=p_ptr->num_fire;
         num=0;
         while (num++ < thits)
         {
            tries++;
            tmul = 1;
            dam = damroll(a_ptr->dd, a_ptr->ds) + a_ptr->to_d + w_ptr->to_d;
            tmul = (w_ptr->sval % 10);
            tmul += p_ptr->xtra_might;
            dam *= tmul;

            if (test_hit_fire(chance - distance, r_ptr->ac, m_ptr->ml))
            {
               hits++;
               dam = tot_dam_aux(a_ptr, dam, m_ptr);
               critical_damage=
                      critical_shot(a_ptr->weight, a_ptr->to_h, dam, FALSE);
               if (critical_damage>dam)
               {
                  good_damage+=critical_damage;
                  good_hits++;
                  dam=critical_damage;
               }
               if (dam < 0) dam = 0;
               total_damage +=dam;
               /* Hit the monster, check for death */
               hp -=dam;
               if (hp<0)
               {
                  kills++;
                  hp = max_hp;
               } /* if dead */
            } /* if hit */
         } /* while num<blows */
      } /* if use_bow */
      else
      {
         s16b i;
         s16b blows_type[30];
         if (p_ptr->num_blow1 + p_ptr->num_blow2 > 30)
         {
            msg_print("You have too many extra attacks to use this function.");
            Term_load();
            character_icky = FALSE;
            Term_fresh();
            return;
         }
         for (i=1; i<=p_ptr->num_blow1; i++)
         {
            blows_type[i]=INVEN_WIELD;
         }
         if ( (p_ptr->pclass == CLASS_GLADIATR) &&
              inventory[INVEN_ARM].k_idx)
         {

            /* build standard sequence */
            for (i=p_ptr->num_blow1+1; i <= p_ptr->num_blow1 + p_ptr->num_blow2; i++)
            {
               blows_type[i]=INVEN_ARM;
            }

            /* make random permutations */
            for (i=1; i <= p_ptr->num_blow1 + p_ptr->num_blow2; i++)
            {
               s16b temp;
               s16b t1,t2;
               t1 = randint(p_ptr->num_blow1 + p_ptr->num_blow2);
               t2 = randint(p_ptr->num_blow1 + p_ptr->num_blow2);
               if (t1==t2) continue;
               temp = blows_type[t1];
               blows_type[t1]=blows_type[t2];
               blows_type[t2]=temp;
            }
         }

         /* Attack once for each legal blow */
         num = 0;
         while (num++ < p_ptr->num_blow1 + p_ptr->num_blow2)
         {
            bool hit_with_first; /* which weapon is used */

            hit_with_first = TRUE;
            tries++;

            /* Access the weapon */
            if (p_ptr->pclass == CLASS_GLADIATR)
            {
               w_ptr = &inventory[blows_type[num]];
               if (blows_type[num] == INVEN_ARM)
               {
                  hit_with_first = FALSE;
               }
            }
            else
            {
               w_ptr = &inventory[INVEN_WIELD];
            }

/* jk  - for vampiric weapons */
            /* Extract some flags */
            object_flags(w_ptr, &f1, &f2, &f3);

            /* Calculate the "attack quality" */
/* jk - two handed */
            bonus = p_ptr->to_h + (p_ptr->twohands ? w_ptr->to_h*3/2 : w_ptr->to_h);
            chance = (p_ptr->skill_thn + (bonus * BTH_PLUS_ADJ));

            /* Test for hit */
            if (test_hit_norm(chance, r_ptr->ac, m_ptr->ml))
            {
               hits++;
               dam = damroll(w_ptr->dd, w_ptr->ds);
               dam = tot_dam_aux(w_ptr, dam, m_ptr);
               tohit = w_ptr->to_h;
               if (p_ptr->twohands) tohit=tohit*3/2;
               if ((p_ptr->pclass==CLASS_ROGUE) && !took_swing )
               {
                  tohit+=p_ptr->lev*8;
                  took_swing=TRUE;
               }
               critical_damage = critical_norm(w_ptr->weight, tohit, dam, FALSE);
               if (critical_damage>dam)
               {
                  good_hits++;
                  good_damage+=critical_damage+w_ptr->to_d+p_ptr->to_d;
               }

               dam = critical_damage + w_ptr->to_d + p_ptr->to_d;
               if (!have_hit_deep && (randint(p_ptr->num_blow1 + p_ptr->num_blow2 - num)==1))
               {
                  dam += p_ptr->ring_to_d;
                  have_hit_deep=TRUE;
               }
               total_damage +=dam;

               /* No negative damage */
               if (dam < 0) dam = 0;

               hp -=dam;
               if (hp<0)
               {
                  kills++;
                  hp = max_hp;
                  took_swing=FALSE;
               } /* hf dead */
            } /* if hit */
         } /*  hile num<blows */
      } /* else non-bow */

      prt(format("turn         :   %5d of %5d",i,turns),1,10);
      prt(format("kills        :   %5d",kills),1,11);

      (void)sprintf(outline,"%5.2f %%",(float)((100*hits)/tries));
      prt(format("hits         :   %5d = %s",
                 hits,outline),1,12);

      (void)sprintf(outline,"%5.2f %%    damage/good hit %7.1f",
                    (hits>0) ? (float)((100*good_hits)/hits) : 0,
                    (good_hits>0) ? (float)good_damage/(float)good_hits : 0);
      prt(format("good hits    :   %5d = %s",
                 good_hits,outline),1,13);

      (void)sprintf(outline,
      "damage/hit   : %7.1f   damage/try        : %7.1f damage/turn   : %7.1f",
              (hits>0) ? (float)total_damage/(float)hits : 0,
              (tries>0) ? (float)total_damage/(float)tries : 0,
              (i>0) ? (float)total_damage/(float)i : 0);
      prt(outline,1,16);

      (void)sprintf(outline,"tries/turn   : %5d",
                    (use_bow) ? thits : (p_ptr->num_blow1 + p_ptr->num_blow2));
      prt(outline,1,14);
      (void)sprintf(outline,
                    "%7.1f   tries/kill        : %7.1f turns/kill    : %7.1f",
                    (kills>0) ? (float)hits/(float)kills : 0,
                    (kills>0) ? (float)tries/(float)kills : 0,
                    (kills>0) ? (float)i/(float)kills : 0);
      prt(format("hits/kill    : %s",outline),1,15);

      Term_fresh();
      /* Do not wait for a key */
      inkey_scan = TRUE;
      /* Check for a keypress */
      if (inkey()) break;
   } /* for i<turns */

   prt("Press any key to continue",1,23);
   c=inkey();
   message_add(format("Weapon evaluation on %s",m_name));
   message_add(format("Attack chance: %3d with %s",chance,w_name));
   if ( (p_ptr->pclass == CLASS_GLADIATR) && inventory[INVEN_ARM].k_idx)
   {
      message_add(format("Attack chance: %3d with %s",chance,w_name2));
   }
   if (use_bow)
   {
      message_add(format("                  with %s",a_name));
   }
   message_add(format("monster no: %4d hp: %5d ac: %3d level: %4d",monster_index,
              max_hp,r_ptr->ac,r_ptr->level));
   message_add(format("kills        :   %5d",kills));
   (void)sprintf(outline,"%5.2f %%",(float)((100*hits)/tries));
   message_add(format("hits         :   %5d = %s",hits,outline));
   (void)sprintf(outline,"%5.2f %%    damage/good hit %7.1f",
                 (hits>0) ? (float)((100*good_hits)/hits) : 0,
                 (good_hits>0) ? (float)good_damage/(float)good_hits : 0);
   message_add(format("good hits    :   %5d = %s",good_hits,outline));
   (void)sprintf(outline,"tries/turn   : %5d",
                 (use_bow) ? thits : (p_ptr->num_blow1 + p_ptr->num_blow2));
   message_add(outline);
   (void)sprintf(outline,
   "%7.1f   tries/kill        : %7.1f turns/kill    : %7.1f",
                (kills>0) ? (float)hits/(float)kills : 0,
                (kills>0) ? (float)tries/(float)kills : 0,
                (kills>0) ? (float)i/(float)kills : 0);
   message_add(format("hits/kill    : %s",outline));
   (void)sprintf(outline,
    "damage/hit   : %7.1f   damage/try        : %7.1f damage/turn   : %7.1f",
                  (hits>0) ? (float)total_damage/(float)hits : 0,
                  (tries>0) ? (float)total_damage/(float)tries : 0,
                  (i>0) ? (float)total_damage/(float)i : 0);
   message_add(outline);

   /* Restore the screen */
   Term_load();
   do_cmd_redraw();
   /* Leave "icky" mode */
   character_icky = FALSE;
}



/*
 * Change the quantity of a the item
 */
static void wiz_quantity_item(object_type *i_ptr)
{
   s16b         tmp_int;

   char        tmp_val[100];

   /* Never duplicate artifacts */
   if (artifact_p(i_ptr)) return;

   /* Default */
   sprintf(tmp_val, "%d", i_ptr->number);

   /* Query */
   if (get_string("Quantity: ", tmp_val, 2))
   {
      /* Extract */
      tmp_int = atoi(tmp_val);

      /* Paranoia */
      if (tmp_int < 1) tmp_int = 1;
      if (tmp_int > 99) tmp_int = 99;

      /* Accept modifications */
      i_ptr->number = tmp_int;
   }
}

/*jk */
static void do_cmd_wiz_value(void)
{
   s16b         item;
   s16b         amt = 1;
   object_type *i_ptr;

   /* Get an item (from equip or inven) */
   if (!get_item(&item, &amt, "Give value of which object? ", TRUE, TRUE, TRUE))
   {
      if (item == -2) msg_print("You have no objects to value.");
      return;
   }

   /* Get the item (in the pack) */
   i_ptr=get_item_pointer(item);

   if (amt>1)
      msg_print(format("That is worth %d AU per piece, %d in total.",
                object_value(i_ptr),i_ptr->number*object_value(i_ptr)));
   else
      msg_print(format("That is worth %d AU.",object_value(i_ptr)));
}

/*
 * Play with an item. Options include:
 *   - Output statistics (via wiz_roll_item)
 *   - Reroll item (via wiz_reroll_item)
 *   - Change properties (via wiz_tweak_item)
 *   - Change the number of items (via wiz_quantity_item)
 */
static void do_cmd_wiz_play(void)
{
   s16b         item;
   object_type *i_ptr;
   object_type forge;
   char        ch;
   bool        changed;
/* jk */
   s16b         amt = 1;

   /* strange things happen otherwise, you have been warned! */
   forge.spell_set = 0;
   invwipe(&forge);

   /* Get an item (from equip or inven) */
   if (!get_item(&item, &amt, "Play with which object? ", TRUE, TRUE, TRUE))
   {
       if (item == -2) msg_print("You have nothing to play with.");
       return;
   }

   /* Get the item (in the pack) */
   i_ptr=get_item_pointer(item);

   /* The item was not changed */
   changed = FALSE;


   /* Icky */
   character_icky = TRUE;

   /* Save the screen */
   Term_save();

   /* Get a copy of the item */
   forge = (*i_ptr);

   /* The main loop */
   while (TRUE)
   {
      /* Display the item */
      wiz_display_item(&forge);

      /* Get choice */
      if (!get_com("[a]ccept [s]tatistics [r]eroll [t]weak [q]uantity? ", &ch))
      {
         changed = FALSE;
         break;
      }

      if (ch == 'A' || ch == 'a')
      {
         changed = TRUE;
         break;
      }

      if (ch == 's' || ch == 'S')
      {
         wiz_statistics(&forge);
      }

      if (ch == 'r' || ch == 'r')
      {
         wiz_reroll_item(&forge);
      }

      if (ch == 't' || ch == 'T')
      {
         wiz_tweak_item(&forge);
      }

      if (ch == 'q' || ch == 'Q')
      {
         wiz_quantity_item(&forge);
      }
   }

   /* Restore the screen */
   Term_load();

   /* Not Icky */
   character_icky = FALSE;

   /* Accept change */
   if (changed)
   {
      /* Message */
      msg_print("Changes accepted.");

      /* Change */
      (*i_ptr) = forge;

      /* Window stuff */
      p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

      /* Recalculate bonuses */
      p_ptr->update |= (PU_BONUS);

      /* Combine / Reorder the pack (later) */
      p_ptr->notice |= (PN_COMBINE | PN_REORDER);
   }

   /* Ignore change */
   else
   {
      msg_print("Changes ignored.");
   }
}
/*
 * this routine crashes the game, to allow testing of the crash-dump routines
 */
void wiz_create_crash(void)
{
   bool useless;
   s16b a, b, c;

   if (get_check("Do you want to make a panic-save and crashdump?"))
   {
      /* this doesn't seem to crash DOS */
      strcpy(NULL, "crashing now!");
      msg_print("Hmmm, we survived strcpy(NULL, <somestring>).");

      /* but this should :-) doesn't also */
      item_tester_hook = NULL;
      useless = (*item_tester_hook)(NULL);
      msg_print("Hmmm, we survived calling (*NULL)(<arguments>) too.");
      a=3;
      b=0;
      c=a/b;
      msg_print("Hmmmmm, it survived a divide by zero - I'm sorry, it won't crash.");
   }
   else
   {
      msg_print("OK, maybe later?");
      msg_print(NULL);
   }
}

/*
 * Wizard routine for creating objects          -RAK-
 * Heavily modified to allow magification and artifactification  -Bernd-
 *
 * Note that wizards cannot create objects on top of other objects.
 *
 * Hack -- this routine always makes a "dungeon object", and applies
 * magic to it, and attempts to decline cursed items.
 */
static void wiz_create_item(void)
{
   object_type forge;

   s16b         k_idx;

   /* Icky */
   character_icky = TRUE;

   /* Save the screen */
   Term_save();

   /* Get object base type */
   k_idx = wiz_get_itemtype(FALSE);

   /* strange things happen otherwise, you have been warned! */
   forge.spell_set = 0;
   invwipe(&forge);

   /* Restore the screen */
   Term_load();
   /* Not Icky */
   character_icky = FALSE;

   /* Return if failed */
   if (k_idx<=0) return;

   /* Create the item */
   invcopy(&forge, k_idx);

   /* Apply magic (no messages, no artifacts) */
   wizard = FALSE;
   apply_magic(&forge, p_ptr->mdepth, FALSE, FALSE, FALSE);
   /* if we created an artifact, set the count of creation to 0 again */
   a_info[forge.name1].cur_num = 0;
   wizard = TRUE;

   forge.log.where = OBJ_FOUND_WIZARD;
   forge.log.whose = 0;
   forge.log.mlevel = p_ptr->mdepth;
   forge.log.slevel = p_ptr->sdepth;

   /* Drop the object from heaven */
   if (drop_near(&forge, 0, px, py, 0, FALSE, FALSE))
   {
      /* All done */
      msg_print("Allocated.");
   }
   else
   {
      msg_print("Couldn't find space for object.");
   }
}

/* jk - create lnamed item */
static void wiz_create_named_item()
{
   object_type forge;

   s16b        k_idx;
   char        searchname[80]; /* to hold name to search for in */
   char        i_name[256];     /* to hold monster name */

   s16b        max_searches = 20000;
   char        ch;
   s16b        cur_search;
   u64b        f1, f2, f3;
   bool        old_cheat_peek;

   /* Icky */
   character_icky = TRUE;
/* jk - cheat_peek makes you press a key with any creation, so it's */
/* shut off here */
   old_cheat_peek = cheat_peek;
   cheat_peek = FALSE;

   /* Save the screen */
   Term_save();

   /* strange things happen otherwise, you have been warned! */
   forge.spell_set = 0;
   invwipe(&forge);

   /* Get object base type */
   k_idx = wiz_get_itemtype(FALSE);
   if (k_idx<=0)
   {
      Term_load();
      character_icky = FALSE;
      cheat_peek = old_cheat_peek;
      return;
   }
   Term_clear();
   strcpy(searchname,"");
   prt("Enter (part of) name to search for:",1,1);
   askfor_aux(searchname,40);
   if (!get_com("[n]ormal, [g]ood, [e]xcellent, [a]rtifact? ", &ch))
   {
      Term_load();
      character_icky = FALSE;
      cheat_peek = old_cheat_peek;
      return;
   }
   cur_search=0;
   strcpy(i_name,"");
   while ((cur_search<max_searches) && (cmp_strngs(i_name,searchname)==FALSE))
   {
      cur_search++;
      prt(format("Trying %d of %d",cur_search,max_searches),1,2);
      inkey_scan = TRUE;
      /* Check for a keypress */
      if (inkey())
      {
/* i_name<>search_name, else it would have been noted in the while above! */
         cur_search = max_searches;
         break;
      }

      invcopy(&forge, k_idx);
      switch (ch)
      {
         case 'n':
            apply_magic(&forge, p_ptr->mdepth, FALSE, FALSE, FALSE);
            break;
         case 'g':
            apply_magic(&forge, p_ptr->mdepth, FALSE, TRUE, FALSE);
            break;
         case 'e':
            apply_magic(&forge, p_ptr->mdepth, FALSE, TRUE, TRUE);
            break;
         case 'a':
         {
            s16b old_depth= p_ptr->mdepth;
            s16b old_obj_level = object_level;

            p_ptr->mdepth = 127;
            object_level = 127;
            (void)make_artifact_special(&forge, TRUE);

            /* we did not succeed yet? try this */
            if (!forge.name1) (void)make_artifact(&forge, TRUE);

            p_ptr->mdepth= old_depth;
            object_level = old_obj_level;
            apply_magic(&forge, 127, TRUE, TRUE, TRUE);
            break;
         }
      }
      object_flags(&forge, &f1, &f2, &f3);
      object_desc_store(i_name, &forge, TRUE, 3);
      prt(i_name, 1, 3);
/* tthis makes every thing really appear on screen */
      Term_fresh();
   }
   if ( (cur_search==max_searches) && (cmp_strngs(i_name,searchname)==FALSE))
   {
      prt("No such object found.",1,5);
      pause_line(23);
      Term_load();
      character_icky = FALSE;
      cheat_peek = old_cheat_peek;
      return;
   }

   /* Restore the screen */
   Term_load();

   /* Not Icky */
   character_icky = FALSE;
   cheat_peek = old_cheat_peek;
   /* Create the item */

   forge.log.where = OBJ_FOUND_WIZARD;
   forge.log.whose = 0;
   forge.log.mlevel = p_ptr->mdepth;
   forge.log.slevel = p_ptr->sdepth;

   /* Drop the object from heaven */
   if (drop_near(&forge, 0, px, py, 0, FALSE, FALSE))
   {
      /* All done */
      msg_print("Allocated.");
   }
   else
      msg_print("Couldn't find space for object.");
}

/*
 * Cure everything instantly
 */
void do_cmd_wiz_cure_all()
{
   /* Remove curses */
   (void)remove_all_curse();

   /* Restore stats */
   (void)res_stat(A_STR);
   (void)res_stat(A_INT);
   (void)res_stat(A_WIS);
   (void)res_stat(A_CON);
   (void)res_stat(A_DEX);
   (void)res_stat(A_CHR);

   /* Restore the level */
   (void)restore_level();

   /* Heal the player */
   p_ptr->chp = p_ptr->mhp;
   p_ptr->chp_frac = 0;

   /* Restore mana */
   p_ptr->csp = p_ptr->msp;
   p_ptr->csp_frac = 0;

   /* Cure stuff */
   (void)set_blind(0);
   (void)set_confused(0);
   (void)set_poisoned(0);
   (void)set_afraid(0);
   (void)set_paralyzed(0);
   (void)set_image(0);
   (void)set_stun(0);
   (void)set_cut(0);
   (void)set_slow(0);
/* jk */
   (void)set_sliding(0);
   (void)set_cold(0);
   (void)set_fire(0);
   (void)set_acid(0);
   (void)set_elec(0);
   (void)set_throat(0);
   p_ptr->word_recall = 0;

   /* No longer hungry */
   (void)set_food(PY_FOOD_MAX - 1);

   /* Redraw everything */
   do_cmd_redraw();
}

/*
 * Show up every disease
 */
static void do_cmd_wiz_disease_all()
{
   (void)set_blind(50);
   (void)set_confused(50);
   (void)set_poisoned(50);
   (void)set_afraid(50);
   (void)set_image(50);
   (void)set_stun(50);
   (void)set_cut(50);
   (void)set_slow(50);
/* jk */
   (void)set_sliding(50);
   (void)set_throat(50);
   (void)set_fire(50);
   (void)set_cold(50);
   (void)set_acid(50);
   (void)set_elec(50);
   (void)set_lift(50);
   (void)set_reflecting(50);

   (void)set_food(PY_FOOD_WEAK - 1);

   /* Redraw everything */
   do_cmd_redraw();
}

/*
 * Go to any level
 */
static void do_cmd_wiz_jump(void)
{
   /* Ask for level */
   if (p_ptr->command_arg <= 0)
   {
      char    ppp[80];

      char    tmp_val[160];

      /* Prompt */
      sprintf(ppp, "Jump to level (0-%d): ", MAX_LEVEL-1);

      /* Default */
      sprintf(tmp_val, "%d", p_ptr->mdepth);

      /* Ask for a level */
      if (!get_string(ppp, tmp_val, 10)) return;

      /* Extract request */
      p_ptr->command_arg = atoi(tmp_val);
   }

   /* Paranoia */
   if (p_ptr->command_arg < 0) p_ptr->command_arg = 0;

   /* Paranoia */
   if (p_ptr->command_arg > MAX_LEVEL - 1) p_ptr->command_arg = MAX_LEVEL - 1;

/* jk - trying to jump to the same level? */
   if (p_ptr->command_arg == p_ptr->mdepth) return;

   /* Change level */
   p_ptr->new_mdepth = p_ptr->command_arg;
   p_ptr->new_sdepth = 0;
   new_level_flag = TRUE;
}

/* jk - when testing characters at higher levels, it's often a bother */
/* to equip them. */
/* here's the solution. */
void do_cmd_wiz_outfit(void)
{
   /* a character will have: */
   /* maxed stats */
   /* level 50    */
   /* all cured   */
   /* a Mace of Disruption (+20, +20) */
   /* a Heavy Crossbow of Xtra Might (+20,+20) */
   /* PDSM +20 */
   /* 1 ring of damage +20 */
   /* 1 ring of speed +15 */
   /* boots of speed +10 */
   /* Shadow Cloak of Aman +3 stl res nether */
   /* Helm of Might +3 */
   /* brass lantern */

   s16b          i, k_idx;
   object_type *i_ptr;

   if ((equip_cnt) && (!get_check("This will wipe out your current equipment!")))
      return;
   for (i = 0; i < 6; i++)
   {
      p_ptr->stat_cur[i] = p_ptr->stat_max[i] = 18 + 100;
   }
   p_ptr->max_exp = 5000000L;
   p_ptr->au = 2000000L;
   check_experience();
   do_cmd_wiz_cure_all();

   i_ptr = &inventory[INVEN_WIELD];
   k_idx = lookup_kind(TV_HAFTED,SV_MACE_OF_DISRUPTION);
   invcopy(i_ptr,k_idx);
   i_ptr->to_h = 20;
   i_ptr->to_d = 20;
   p_ptr->twohands = TRUE;
   object_tried(i_ptr);
   object_aware(i_ptr);
   object_known(i_ptr);
   i_ptr->log.where = OBJ_FOUND_WIZARD;
   i_ptr->log.whose = 0;
   i_ptr->log.mlevel = p_ptr->mdepth;
   i_ptr->log.slevel = p_ptr->sdepth;

   i_ptr = &inventory[INVEN_BOW];
   k_idx = lookup_kind(TV_BOW,SV_HEAVY_XBOW);
   invcopy(i_ptr,k_idx);
   i_ptr->to_h = 20;
   i_ptr->to_d = 20;
   i_ptr->name2 = EGO_EXTRA_MIGHT;
   object_tried(i_ptr);
   object_aware(i_ptr);
   object_known(i_ptr);
   i_ptr->log.where = OBJ_FOUND_WIZARD;
   i_ptr->log.whose = 0;
   i_ptr->log.mlevel = p_ptr->mdepth;
   i_ptr->log.slevel = p_ptr->sdepth;

   i_ptr = &inventory[INVEN_AMMO];
   k_idx = lookup_kind(TV_BOLT,SV_BOLT_HEAVY);
   invcopy(i_ptr,k_idx);
   i_ptr->to_h = 20;
   i_ptr->to_d = 20;
   i_ptr->number = 40;
   i_ptr->name2 = EGO_WOUNDING;
   object_tried(i_ptr);
   object_aware(i_ptr);
   object_known(i_ptr);
   i_ptr->log.where = OBJ_FOUND_WIZARD;
   i_ptr->log.whose = 0;
   i_ptr->log.mlevel = p_ptr->mdepth;
   i_ptr->log.slevel = p_ptr->sdepth;

   /* gladiators wound up with a unseen dragon armor, which could be activated none the less */
   if (p_ptr->pclass != CLASS_GLADIATR)
   {
      i_ptr = &inventory[INVEN_BODY];
      k_idx = lookup_kind(TV_DRAG_ARMOR,SV_DRAGON_POWER);
      invcopy(i_ptr,k_idx);
      i_ptr->to_a = 20;
      object_tried(i_ptr);
      object_aware(i_ptr);
      object_known(i_ptr);
      i_ptr->log.where = OBJ_FOUND_WIZARD;
      i_ptr->log.whose = 0;
      i_ptr->log.mlevel = p_ptr->mdepth;
      i_ptr->log.slevel = p_ptr->sdepth;
   }
   else
   {
      i_ptr = &inventory[INVEN_BODY];
      k_idx = lookup_kind(TV_HAFTED,SV_WHIP);
      invcopy(i_ptr,k_idx);
      i_ptr->to_h = 19;
      i_ptr->to_d = 23;
      i_ptr->dd = 2;
      object_tried(i_ptr);
      object_aware(i_ptr);
      object_known(i_ptr);
      i_ptr->log.where = OBJ_FOUND_WIZARD;
      i_ptr->log.whose = 0;
      i_ptr->log.mlevel = p_ptr->mdepth;
      i_ptr->log.slevel = p_ptr->sdepth;
   }


   i_ptr = &inventory[INVEN_NECK];
   k_idx = lookup_kind(TV_AMULET,SV_AMULET_THE_MAGI);
   invcopy(i_ptr,k_idx);
   i_ptr->to_a = 10;
   i_ptr->p1val = 10;
   object_tried(i_ptr);
   object_aware(i_ptr);
   object_known(i_ptr);
   i_ptr->log.where = OBJ_FOUND_WIZARD;
   i_ptr->log.whose = 0;
   i_ptr->log.mlevel = p_ptr->mdepth;
   i_ptr->log.slevel = p_ptr->sdepth;

   i_ptr = &inventory[INVEN_OUTER];
   k_idx = lookup_kind(TV_CLOAK,SV_SHADOW_CLOAK);
   invcopy(i_ptr,k_idx);
   i_ptr->to_a = 20;
   i_ptr->name2 = EGO_AMAN;
   i_ptr->xtra1 |= EGO_XTRA_HIGHRESIST;
   i_ptr->xtra2 = 4;
   object_tried(i_ptr);
   object_aware(i_ptr);
   object_known(i_ptr);
   i_ptr->log.where = OBJ_FOUND_WIZARD;
   i_ptr->log.whose = 0;
   i_ptr->log.mlevel = p_ptr->mdepth;
   i_ptr->log.slevel = p_ptr->sdepth;

   i_ptr = &inventory[INVEN_LEFT];
   k_idx = lookup_kind(TV_RING,SV_RING_DAMAGE);
   invcopy(i_ptr,k_idx);
   i_ptr->to_d = 20;
   object_aware(i_ptr);
   object_known(i_ptr);
   i_ptr->log.where = OBJ_FOUND_WIZARD;
   i_ptr->log.whose = 0;
   i_ptr->log.mlevel = p_ptr->mdepth;
   i_ptr->log.slevel = p_ptr->sdepth;

   i_ptr = &inventory[INVEN_RIGHT];
   k_idx = lookup_kind(TV_RING,SV_RING_SPEED);
   invcopy(i_ptr,k_idx);
   i_ptr->p1val = 15;
   object_aware(i_ptr);
   object_known(i_ptr);
   i_ptr->log.where = OBJ_FOUND_WIZARD;
   i_ptr->log.whose = 0;
   i_ptr->log.mlevel = p_ptr->mdepth;
   i_ptr->log.slevel = p_ptr->sdepth;

   i_ptr = &inventory[INVEN_FEET];
   k_idx = lookup_kind(TV_BOOTS,SV_PAIR_OF_METAL_SHOD_BOOTS);
   invcopy(i_ptr,k_idx);
   i_ptr->to_h = 20;
   i_ptr->p1val = 10;
   i_ptr->name2 = EGO_SPEED;
   object_tried(i_ptr);
   object_aware(i_ptr);
   object_known(i_ptr);
   i_ptr->log.where = OBJ_FOUND_WIZARD;
   i_ptr->log.whose = 0;
   i_ptr->log.mlevel = p_ptr->mdepth;
   i_ptr->log.slevel = p_ptr->sdepth;

   i_ptr = &inventory[INVEN_HANDS];
   k_idx = lookup_kind(TV_GLOVES,SV_SET_OF_CESTI);
   invcopy(i_ptr,k_idx);
   i_ptr->to_a = 20;
   i_ptr->to_h = 5;
   i_ptr->to_d = 5;
   i_ptr->p1val = 5;
   i_ptr->name2 = EGO_POWER;
   object_tried(i_ptr);
   object_aware(i_ptr);
   object_known(i_ptr);
   i_ptr->log.where = OBJ_FOUND_WIZARD;
   i_ptr->log.whose = 0;
   i_ptr->log.mlevel = p_ptr->mdepth;
   i_ptr->log.slevel = p_ptr->sdepth;

   i_ptr = &inventory[INVEN_LITE];
   k_idx = lookup_kind(TV_LITE,SV_LITE_LANTERN);
   invcopy(i_ptr,k_idx);
   i_ptr->p1val = 15000;
   object_aware(i_ptr);
   object_known(i_ptr);
   i_ptr->log.where = OBJ_FOUND_WIZARD;
   i_ptr->log.whose = 0;
   i_ptr->log.mlevel = p_ptr->mdepth;
   i_ptr->log.slevel = p_ptr->sdepth;

   i_ptr = &inventory[INVEN_HEAD];
   k_idx = lookup_kind(TV_CROWN,SV_IRON_CROWN);
   invcopy(i_ptr,k_idx);
   i_ptr->to_a = 20;
   i_ptr->p1val = 3;
   i_ptr->name2 = EGO_MIGHT;
   object_tried(i_ptr);
   object_aware(i_ptr);
   object_known(i_ptr);
   i_ptr->log.where = OBJ_FOUND_WIZARD;
   i_ptr->log.whose = 0;
   i_ptr->log.mlevel = p_ptr->mdepth;
   i_ptr->log.slevel = p_ptr->sdepth;

   /* Recalculate bonuses */
   p_ptr->update |= (PU_BONUS);

   /* Recalculate torch */
   p_ptr->update |= (PU_TORCH);

   /* Recalculate mana */
   p_ptr->update |= (PU_MANA);

   /* Window stuff */
   p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

   redraw_stuff();
   update_stuff();
}

/*
 * Become aware of a lot of objects
 */
static void do_cmd_wiz_learn(void)
{
   s16b         i;

   /* Scan every object */
   for (i = 1; i < k_number; i++)
   {
      object_kind *k_ptr = &k_info[i];

      /* Induce awareness */
      if (k_ptr->level <= p_ptr->command_arg)
      {
         object_type inv;
         invcopy(&inv, i);
         object_aware(&inv);
      }
   }
   for (i=1; i < t_number; i++)
   {
      if (!t_info[i].name) continue;
      t_info[i].known=25000;
   }
}

/*
 * Hack -- Rerate Hitpoints
 */
static void do_cmd_rerate()
{
   s16b      min_value, max_value, i, percent;

   min_value = (PY_MAX_LEVEL * 3 * (p_ptr->hitdie - 1)) / 8 +
      PY_MAX_LEVEL;
   max_value = (PY_MAX_LEVEL * 5 * (p_ptr->hitdie - 1)) / 8 +
      PY_MAX_LEVEL;
   player_hp[0] = p_ptr->hitdie;

   /* XXX XXX XXX XXX */
   do
   {
      for (i = 1; i < PY_MAX_LEVEL; i++)
      {
         player_hp[i] = randint(p_ptr->hitdie);
         player_hp[i] += player_hp[i - 1];
      }
   }
   while ((player_hp[PY_MAX_LEVEL - 1] < min_value) ||
         (player_hp[PY_MAX_LEVEL - 1] > max_value));
   /* XXX XXX XXX XXX */

   percent = (int)(((long)player_hp[PY_MAX_LEVEL - 1] * 200L) /
            (p_ptr->hitdie + ((PY_MAX_LEVEL - 1) * p_ptr->hitdie)));

   /* Update and redraw hitpoints */
   p_ptr->update |= (PU_HP);
   p_ptr->redraw1 |= (PR1_HP);

   /* Handle stuff */
   handle_stuff();

   /* Message */
   msg_format("Current Life Rating is %d/100.", percent);
}

/*
 * Summon some creatures
 */
static void do_cmd_wiz_summon(s16b num)
{
   s16b i;

   for (i = 0; i < num; i++)
   {
      (void)summon_specific(px, py, p_ptr->mdepth, 0, 0);
   }
}

/*
 * Summon a creature of the specified type
 */
static void do_cmd_wiz_summon_named(void)
{
   s16b         i, j, x, y, number;
   static s16b  r_idx = 1;
   bool         unique_added = FALSE;
   bool         placed = FALSE;

   r_idx = choose_race(r_idx);

   if (r_idx==-1) return;

   /* Prevent illegal monsters */
   if (r_idx > r_number_total) return;
   if (r_idx > r_number)
   {
      msg_print("Summoning a player ghost.");
   }

   if (r_info[r_idx].flags1 & RF1_UNIQUE)
   {
dlog(DEBUGGHOST,"wizard2.c: do_cmd_wiz_summon_named: r_ptr->max_num %d\n", r_info[r_idx].max_num);
      if (!r_info[r_idx].max_num) r_info[r_idx].max_num = 2;
      unique_added = TRUE;
      msg_print("You clone this unique monster!");
   }
   number=get_quantity("How many of these?", 40, 1);

   for (j=0; j < number; j++)
   {
      /* Try for a few times */
      for (i = 0; i < 10; i++)
      {
         /* Pick a location */
         scatter(&x, &y, px, py, (j/3)+1, 0);

         if (place_monster_one(x, y, r_idx, TRUE, 0, TRUE))
         {
            placed = TRUE;
            break;
         }
      }
   }
   prt("",0,MESSAGE_ROW);
   if (placed && unique_added) r_info[r_idx].max_num = 1;
}

/*
 * Hack -- Delete all nearby monsters
 */
static void do_cmd_wiz_zap(void)
{
   s16b        i;

   /* Genocide everyone nearby */
   for (i = 1; i < mn_max; i++)
   {
      monster_type *m_ptr = &mn_list[i];

      /* Paranoia -- Skip dead monsters */
      if (!m_ptr->r_idx) continue;

      /* only on this sublevel */
      if (m_ptr->fz != sublevel) continue;

      /* Delete nearby monsters */
      if (m_ptr->cdis <= MAX_SIGHT)
      {
         project_who_type who;
         who.type = WHO_PLAYERASWIZARD;
         monster_death(&who, i);
         delete_monster_idx(i);
      }
   }
}

/* jk */
/*
 * Hack -- Delete all nearby monsters
 */
static void do_cmd_wiz_kill(void)
{
   s16b        i;

   /* Genocide everyone nearby */
   for (i = 1; i < mn_max; i++)
   {
      monster_type *m_ptr = &mn_list[i];
      monster_race *r_ptr = &r_info[m_ptr->r_idx];

      /* Paranoia -- Skip dead monsters */
      if (!m_ptr->r_idx) continue;

/* to prevent from killing too many monsters that can't be seen */
      if (!player_has_los_bold(m_ptr->fx,m_ptr->fy)) continue;

      /* Delete nearby monsters */
      if (m_ptr->cdis <= MAX_SIGHT)
      {
         char m_name[80];
         project_who_type who;
         who.type = WHO_PLAYERASWIZARD;

         monster_desc(m_name, m_ptr, 0x88);
         if (r_ptr->flags1 & RF1_UNIQUE)
         {
            if (!get_check(format("Kill unique %s at %d,%d?", m_name, m_ptr->fx, m_ptr->fy)))
            {
               continue;
            }
         }
         msg_format("Wizard killing %s m_idx %d at %d,%d - px,py %d,%d",m_name,i,m_ptr->fx,m_ptr->fy,px,py);
         monster_death(&who, i);
         delete_monster_idx(i);
      }
   }
}

#ifdef ALLOW_SPOILERS

/*
 * External function
 */
extern void do_cmd_spoilers(void);

#endif

/*
 * Verify use of "debug" commands
 */
static void wizard_no_more_debug(void)
{
   msg_print("You leave debug mode.");
   noscore = (noscore & (~0x0008));
}

/*
 * Verify use of "debug" commands
 */
static bool enter_debug_mode(void)
{
   /* No permission */
   if (!can_be_wizard) return (FALSE);

   /* Ask first time */
   if (!(noscore & 0x0008))
   {
      /* Verify request */
      if (!get_check("Using debug commands invalidates your score - are you very sure? "))
      {
         return (FALSE);
      }

      /* Mark savefile */
      noscore |= 0x0008;
   }

   /* Success */
   return (TRUE);
}


/*
 * Ask for and parse a "wizard command"
 * The "p_ptr->command_arg" may have been set.
 * We return "FALSE" on unknown commands.
 */
void do_cmd_wizard(void)
{
   char  cmd;

   if (!enter_debug_mode()) return;

   /* Get a "wizard command" */
   (void)(get_com("Debug Command(\"?aAbBcCdefFghiIjklmnNoprRstTvVwWxz_): ", &cmd));

   /* Analyze the command */
   switch (cmd)
   {
       /* Nothing */
       case ESCAPE:
       case ' ':
       case '\n':
       case '\r':
           break;

#ifdef ALLOW_SPOILERS

       /* Hack -- Generate Spoilers */
       case '"':
          do_cmd_spoilers();
          break;

#endif

       /* Hack -- Help */
       case '?':
          do_cmd_help("wizard.hlp");
          break;

       /* Cure all maladies */
       case 'a':
          do_cmd_wiz_cure_all();
          break;

       /* Advanced statistics */
       case 'A':
          do_cmd_wiz_advanced_statistics();
          break;

       /* Teleport to target */
       case 'b':
           do_cmd_wiz_bamf();
           break;

       /* Create any object */
       case 'c':
          (void)wiz_create_item();
          break;

       /* induce a crash */
       case 'C':
          (void)wiz_create_crash();
          break;

       /* Detect everything */
       case 'd':
          detection();
          break;

       case 'D':
          do_cmd_wiz_disease_all();
          break;

       /* Edit character */
       case 'e':
          do_cmd_wiz_change();
          break;

       /* View item info */
       case 'f':
          (void)identify_fully();
          break;

       case 'F':
          (void)do_cmd_wiz_terrain();
          break;

       /* Good Objects */
       case 'g':
          if (p_ptr->command_arg <= 0) p_ptr->command_arg = 1;
          acquirement(px, py, p_ptr->command_arg, FALSE, TRUE);
          break;

       /* Force generation of room */
       case 'G':
          do_cmd_wiz_generate_room();
          break;

       /* Hitpoint rerating */
       case 'h':
          do_cmd_rerate(); break;

       /* Identify */
       case 'i':
          (void)ident_spell();
          break;

/* jk */
       case 'I':
          do_cmd_wiz_outfit();
          break;

       /* Go up or down in the dungeon */
       case 'j':
          do_cmd_wiz_jump();
          break;

       /* Self-Knowledge */
       case 'k':
          self_knowledge();
          break;

       /* Learn about objects */
       case 'l':
          do_cmd_wiz_learn();
          break;

       /* Magic Mapping */
       case 'm':
          map_area();
          break;

       /* Summon Named Monster */
       case 'n':
          do_cmd_wiz_summon_named();
          break;
/* jk */
       case 'N':
          wiz_create_named_item();
          break;

       /* Object playing routines */
       case 'o':
          do_cmd_wiz_play();
          break;

       /* Phase Door */
       case 'p':
          teleport_player(10);
          break;

/* jk - allows wizard to reset some things */
      case 'R':
          do_cmd_wiz_reset();
          break;
      case 'r':
          do_cmd_wiz_rate_weapon();
          break;
      case 'V':
          do_cmd_wiz_value();
          break;

      /* Summon Random Monster(s) */
      case 's':
          if (p_ptr->command_arg <= 0) p_ptr->command_arg = 1;
          do_cmd_wiz_summon(p_ptr->command_arg);
          break;

      /* Update store inventories */
      case 'S':
      {
         s16b n;
         store_type *st_ptr;
         msg_print("Wizard updating Stores...");
         for (n = 0; n < num_stores; n++)
         {
            st_ptr = &store[n];
            if (st_ptr->store_type != DUNG_ENTR_HOME) store_maint(n);
         }
         if (shuffle_owners && (rand_int(STORE_SHUFFLE) == 0))
         {
            if (cheat_xtra) msg_print("Shuffling a Store...");
            store_shuffle();
         }
         msg_print("done.");
         break;
      }

      /* Teleport */
      case 't':
          teleport_player(67*RATIO);
          break;
/* jk */
      case 'T':
          wizard_trap_creation();
          break;

      /* Very Good Objects */
      case 'v':
          if (p_ptr->command_arg <= 0) p_ptr->command_arg = 1;
          acquirement(px, py, p_ptr->command_arg, TRUE, TRUE);
          break;

      /* Wizard Light the Level */
      case 'w':
          wiz_lite();
          break;

      case 'W':
          wizard_no_more_debug();
          break;

      /* Increase Experience */
      case 'x':
          if (p_ptr->command_arg)
          {
              gain_exp(p_ptr->command_arg);
          }
          else
          {
              gain_exp(p_ptr->exp + 1);
          }
          break;

      /* Zap Monsters (Genocide) */
      case 'z':
          do_cmd_wiz_zap();
          break;

/* jk */
      case 'Z':
          do_cmd_wiz_kill();
          break;

      /* Hack -- whatever I desire */
      case '_':
          do_cmd_wiz_hack_ben();
          break;

      /* Not a Wizard Command */
      default:
          msg_print("That is not a valid debug command.");
          break;
   }
}

#else

#ifdef MACINTOSH
static s16b i = 0;
#endif

#endif
