/* File: misc.c */

/* Purpose: misc code */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

/*
 * Converts stat num into a six-char (right justified) string
 */
void cnv_stat(s16b val, char *out_val)
{
   /* Above 18 */
   if (val > 18)
   {
      s16b bonus = (val - 18);

      if (bonus >= 220)
      {
         sprintf(out_val, "18/%3s", "***");
      }
      else if (bonus >= 100)
      {
         sprintf(out_val, "18/%03d", bonus);
      }
      else
      {
         sprintf(out_val, " 18/%02d", bonus);
      }
   }

   /* From 3 to 18 */
   else
   {
      sprintf(out_val, "    %2d", val);
   }
}

/*
 * Modify a stat value by a "modifier", return new value
 *
 * Stats go up: 3,4,...,17,18,18/10,18/20,...,18/220
 * Or even: 18/13, 18/23, 18/33, ..., 18/220
 *
 * Stats go down: 18/220, 18/210,..., 18/10, 18, 17, ..., 3
 * Or even: 18/13, 18/03, 18, 17, ..., 3
 */
s16b modify_stat_value(s16b value, s16b amount)
{
   s16b  i;

   /* Reward */
   if (amount > 0)
   {
      /* Apply each point */
      for (i = 0; i < amount; i++)
      {
         /* One point at a time */
         if (value < 18) value++;

         /* Ten "points" at a time */
         else value += 10;
      }
   }

   /* Penalty */
   else if (amount < 0)
   {
      /* Apply each point */
      for (i = 0; i < (0 - amount); i++)
      {
         /* Ten points at a time */
         if (value >= 18+10) value -= 10;

         /* Hack -- prevent weirdness */
         else if (value > 18) value = 18;

         /* One point at a time */
         else if (value > 3) value--;
      }
   }

   /* Return new value */
   return (value);
}

/*
 * set the player on a certain level
 */
s32b set_depth(u16b to_baselevel, u16b to_sublevel)
{
   return (to_baselevel * 1000 + to_sublevel);
}

/*
 * where is the player?
 */
void get_depth(s32b depth, u16b *new_baselevel, u16b *new_sublevel)
{
   (*new_baselevel) = (depth / 1000);
   (*new_sublevel) = (depth % 1000);
}


/*
 * Print character info at given row, column in a 13 char field
 */
static void prt_field(cptr info, s16b row, s16b col)
{
   /* Dump 13 spaces to clear */
   c_put_str(TERM_WHITE, "          ", col, row);

   /* Dump the info itself */
   c_put_str(TERM_L_BLUE, info, col, row);
}

/*
 * Print character stat in given row, column
 */
static void prt_stat(s16b stat)
{
   char tmp[32];

   /* Display "injured" stat */
   if (p_ptr->stat_cur[stat] < p_ptr->stat_max[stat])
   {
      c_put_str(TERM_RED, stat_names_reduced[stat], 0, ROW_STAT + stat);
      cnv_stat(p_ptr->stat_use[stat], tmp);
/* jk - this was yellow, but the difference between yellow & l green is small */
      c_put_str(TERM_BLUE, tmp, COL_STAT + 6, ROW_STAT + stat);
   }

   /* Display "healthy" stat */
   else
   {
      put_str(stat_names[stat], 0, ROW_STAT + stat);
      cnv_stat(p_ptr->stat_use[stat], tmp);
      c_put_str(TERM_L_GREEN, tmp, COL_STAT + 6, ROW_STAT + stat);
   }
}

/*
 * Prints "title", including "wizard" or "winner" as needed.
 */
static void prt_title(void)
{
      cptr p = "";

      /* Wizard */
      if (wizard)
      {
            p = "[=-WIZARD-=]";
      }

      /* Winner */
      else if (total_winner || (p_ptr->lev > PY_MAX_LEVEL))
      {
            p = "***WINNER***";
      }

      /* Normal */
      else
      {
            p = player_title[p_ptr->pclass][(p_ptr->lev-1)/5];
      }
      prt_field("            ", ROW_TITLE, COL_TITLE);
      prt_field(p, ROW_TITLE, COL_TITLE);
}

/*
 * Prints level
 */
static void prt_level()
{
   char tmp[32];

   sprintf(tmp, "%6d", p_ptr->lev);

   if (p_ptr->lev >= p_ptr->max_plv)
   {
      put_str("LEVEL ", 0, ROW_LEVEL);
      c_put_str(TERM_L_GREEN, tmp, COL_LEVEL + 6, ROW_LEVEL);
   }
   else
   {
      put_str("Level ", 0, ROW_LEVEL);
      c_put_str(TERM_YELLOW, tmp, COL_LEVEL + 6, ROW_LEVEL);
   }
}

/* jk */
static void prt_position()
{
   char tmp_val[20];
   if (!display_coords) return; /* don't do this if the user doesn't want it */
   sprintf(tmp_val,"%3d,%3d",px,py);
   c_put_str(TERM_WHITE,tmp_val,COL_POS,ROW_POS);
}

/*
 * Display the experience
 */
static void prt_exp()
{
   char out_val[32];

   if ((p_ptr->lev < PY_MAX_LEVEL) && (print_experience_advance))
   {
      sprintf(out_val, "%8ld",(long)player_exp[p_ptr->lev-1]*p_ptr->expfact/100L-p_ptr->exp);
      if (p_ptr->exp >= p_ptr->max_exp)
      {
         put_str("ADV ", 0, ROW_EXP);
         c_put_str(TERM_L_GREEN, out_val, COL_EXP + 4, ROW_EXP);
      }
      else
      {
         put_str("Adv ", 0, ROW_EXP);
/* jk - this was yellow, but the difference between yellow & l green is small */
         c_put_str(TERM_BLUE, out_val, COL_EXP + 4, ROW_EXP);
      }
   }
   else
   {
      sprintf(out_val, "%8ld", (long)p_ptr->exp);
      if (p_ptr->exp >= p_ptr->max_exp)
      {
         put_str("EXP ", 0, ROW_EXP);
         c_put_str(TERM_L_GREEN, out_val, COL_EXP + 4, ROW_EXP);
      }
      else
      {
         put_str("Exp ", 0, ROW_EXP);
         c_put_str(TERM_BLUE, out_val, COL_EXP + 4, ROW_EXP);
      }
   }
}

/*
 * Prints current gold
 */
static void prt_gold()
{
   char tmp[32];

   put_str("AU ", COL_GOLD, ROW_GOLD);
   sprintf(tmp, "%9ld", (long)p_ptr->au);
   c_put_str(TERM_L_GREEN, tmp, COL_GOLD + 3, ROW_GOLD);
}

/*
 * Prints current AC
 */
static void prt_ac()
{
   char tmp[32];

   put_str("Cur AC ", COL_AC, ROW_AC);
   sprintf(tmp, "%5d", p_ptr->dis_ac + p_ptr->dis_to_a);
   c_put_str(TERM_L_GREEN, tmp, COL_AC + 7, ROW_AC);
}


/*
 * Prints Cur/Max hit points
 */
static void prt_hp()
{
   char tmp[32];

   byte color;

   put_str("HP ", COL_CURHP, ROW_CURHP);

   sprintf(tmp, "%4d", p_ptr->mhp);
   color = TERM_L_GREEN;

   c_put_str(color, tmp, COL_CURHP + 3, ROW_CURHP);

   put_str("/", COL_CURHP+7, ROW_CURHP);

   sprintf(tmp, "%4d", p_ptr->chp);

   if (p_ptr->chp >= p_ptr->mhp)
      color = TERM_L_GREEN;
   else if (p_ptr->chp > (p_ptr->mhp * hitpoint_warn) / 10)
      color = TERM_YELLOW;
   else
      color = TERM_RED;

   c_put_str(color, tmp, COL_MAXHP, ROW_MAXHP);
}

/*
 * Prints players max/cur spell points
 */
static void prt_sp()
{
   char tmp[32];
   byte color;

   /* Do not show mana unless it matters */
   if (!cp_ptr->spell_stat) return;

   put_str("Max SP ", COL_MAXSP, ROW_MAXSP);

   sprintf(tmp, "%5d", p_ptr->msp);
   color = TERM_L_GREEN;

   c_put_str(color, tmp, COL_MAXSP + 7, ROW_MAXSP);

   put_str("Cur SP ", COL_CURSP, ROW_CURSP);

   sprintf(tmp, "%5d", p_ptr->csp);

   if (p_ptr->csp >= p_ptr->msp)
   {
      color = TERM_L_GREEN;
   }
   else if (p_ptr->csp > (p_ptr->msp * hitpoint_warn) / 10)
   {
      color = TERM_YELLOW;
   }
   else
   {
      color = TERM_RED;
   }

   /* Show mana */
   c_put_str(color, tmp, COL_CURSP + 7, ROW_CURSP);
}

/*
 * print current depth in feet
 */
static void prt_depth_in_feet(void)
{
   char depths[80];
   (void)strcpy(depths, "Main Dungeon");
   if (!p_ptr->mdepth)
   {
      (void)strcat(depths, ": Town");
      if (p_ptr->sdepth!=0)
      {
         (void)strcat(depths,format(" (%s, %+d ft)",
                                    v_name + dungeon.level_name[p_ptr->sdepth],
                                    dungeon.level_depth[p_ptr->sdepth] * 10));
      }
   }
   else
   {
      (void)strcat(depths,format(": %d ft", p_ptr->mdepth * 50));
      if (p_ptr->sdepth!=0)
      {
         (void)strcat(depths,format(" (%s, %+d ft)",
                                    v_name + dungeon.level_name[p_ptr->sdepth],
                                    dungeon.level_depth[p_ptr->sdepth] * 10));
      }
   }

   /* Right-Adjust the "depth", and clear old values */
   prt(depths, COL_DEPTH, ROW_DEPTH);
}

/*
 * print current depth in "levels"
 */
static void prt_depth_in_levels(void)
{
   char depths[80];
   (void)strcpy(depths, "Main Dungeon");
   if (!p_ptr->mdepth)
   {
      (void)strcat(depths, ": Town");
      if (p_ptr->sdepth!=0)
      {
         (void)strcat(depths,format(" (%s, sublevel %+d)",
                                    v_name + dungeon.level_name[p_ptr->sdepth],
                                    dungeon.level_depth[p_ptr->sdepth]));
      }
   }
   else
   {
      (void)strcat(depths,format(": lev %d",p_ptr->mdepth));
      if (p_ptr->sdepth!=0)
      {
         (void)strcat(depths,format(" (%s, sublev %+d)",
                                    v_name + dungeon.level_name[p_ptr->sdepth],
                                    dungeon.level_depth[p_ptr->sdepth]));
      }
   }

   /* Right-Adjust the "depth", and clear old values */
   prt(depths, COL_DEPTH, ROW_DEPTH);
}

/*
 * Prints depth in stat area
 */
static void prt_depth(void)
{
   if (depth_in_feet)
   {
      prt_depth_in_feet();
   }
   else
   {
      prt_depth_in_levels();
   }
}


/*
 * Prints status of hunger
 */
static void prt_hunger()
{
   if (p_ptr->food < PY_FOOD_FAINT)    /* Fainting / Starving */
      c_put_str(TERM_RED,     "Weak   ", COL_HUNGRY, ROW_HUNGRY);

   else if (p_ptr->food < PY_FOOD_WEAK)   /* Weak */
      c_put_str(TERM_ORANGE,  "Weak   ", COL_HUNGRY, ROW_HUNGRY);

   else if (p_ptr->food < PY_FOOD_ALERT)  /* Hungry */
      c_put_str(TERM_YELLOW,  "Hungry ", COL_HUNGRY, ROW_HUNGRY);

   else if (p_ptr->food < PY_FOOD_FULL)   /* Normal */
      c_put_str(TERM_L_GREEN, "       ", COL_HUNGRY, ROW_HUNGRY);

   else if (p_ptr->food < PY_FOOD_MAX)    /* Full */
      c_put_str(TERM_L_GREEN, "Full   ", COL_HUNGRY, ROW_HUNGRY);

   else if (p_ptr->food < (2*PY_FOOD_MAX)) /* Bloated */
      c_put_str(TERM_ORANGE,  "Bloated", COL_HUNGRY, ROW_HUNGRY);

   else                          /* Gorged */
      c_put_str(TERM_RED,     "Gorged ", COL_HUNGRY, ROW_HUNGRY);
}

/*
 * Prints Blind status
 */
static void prt_blind(void)
{
   if (p_ptr->blind)
      c_put_str(TERM_ORANGE, "Blind", COL_BLIND, ROW_BLIND);
   else
      put_str("     ", COL_BLIND, ROW_BLIND);
}

/* jk */
/*
 * Prints Sliding status
 */
static void prt_sliding(void)
{
   if (p_ptr->sliding)
      c_put_str(TERM_ORANGE,
                "Sliding", COL_SLIDING, ROW_SLIDING);
   else
      put_str(  "       ", COL_SLIDING, ROW_SLIDING);
}


/*
 * jk - prints reflect status
 */
static void prt_reflect(void)
{
   if (p_ptr->reflecting)
     c_put_str(TERM_ORANGE, "Reflecting", COL_REFLECT, ROW_REFLECT);
   else
     put_str("          ", COL_REFLECT, ROW_REFLECT);
}

/* jk */
/*
 * Prints Reading status
 */
static void prt_reading(void)
{
   if (p_ptr->reading)
   {
      char tmp[12] = "Read xxxx";
      s16b tmpi = p_ptr->reading;
      tmp[5]=' ';
      tmp[6]=' ';
      tmp[7]=' ';
      tmp[8]=' ';
      if (tmpi >= 1000)
      {
         tmp[5]=48+(tmpi / 1000); tmpi = tmpi % 1000;
         tmp[6]=48+(tmpi / 100); tmpi = tmpi % 100;
         tmp[7]=48+(tmpi / 10); tmpi = tmpi % 10;
         tmp[8]=48+tmpi;
      }
      else if (tmpi >= 100)
      {
         tmp[6]=48+(tmpi / 100); tmpi = tmpi % 100;
         tmp[7]=48+(tmpi / 10); tmpi = tmpi % 10;
         tmp[8]=48+tmpi;
      }
      else if (tmpi >= 10)
      {
         tmp[7]=48+(tmpi / 10); tmpi = tmpi % 10;
         tmp[8]=48+tmpi;
      }
      else
      {
         tmp[8]=48+tmpi;
      }
      c_put_str(TERM_ORANGE, tmp, COL_READING, ROW_READING);
   }
   else
   {
      put_str("         ", COL_READING, ROW_READING);
   }
}

/* jk */
/*
 * Prints Throat status
 */
static void prt_throat(void)
{
   if (p_ptr->throat)
      c_put_str(TERM_ORANGE, "Throat", COL_THROAT, ROW_THROAT);
   else
      put_str("      ", COL_THROAT, ROW_THROAT);
}

/*
 * Prints Confusion status
 */
static void prt_confused(void)
{
   if (p_ptr->confused)
      c_put_str(TERM_ORANGE,
                "Confused", COL_CONFUSED, ROW_CONFUSED);
   else
      put_str(  "        ", COL_CONFUSED, ROW_CONFUSED);
}

/*
 * Prints Fire status
 */
static void prt_fire(void)
{
   if (p_ptr->fire)
      c_put_str(TERM_ORANGE, "Fire", COL_FIRE, ROW_FIRE);
   else
      put_str("    ", COL_FIRE, ROW_FIRE);
}

/*
 * Prints Lift status
 */
static void prt_lift(void)
{
   if (p_ptr->lift)
   {
      c_put_str(TERM_ORANGE, "Mule Legs", COL_LIFT, ROW_LIFT);
   }
   else
   {
      put_str(               "         ", COL_LIFT, ROW_LIFT);
   }
}

/*
 * Prints cold status
 */
static void prt_cold(void)
{
   if (p_ptr->cold)
      c_put_str(TERM_ORANGE, "Cold", COL_COLD, ROW_COLD);
   else
      put_str("    ", COL_COLD, ROW_COLD);
}

/*
 * Prints elec status
 */
static void prt_elec(void)
{
   if (p_ptr->elec)
      c_put_str(TERM_ORANGE, "Elec", COL_ELEC, ROW_ELEC);
   else
      put_str("    ", COL_ELEC, ROW_ELEC);
}

/*
 * Prints acid status
 */
static void prt_acid(void)
{
   if (p_ptr->acid)
      c_put_str(TERM_ORANGE, "Acid", COL_ACID, ROW_ACID);
   else
      put_str("    ", COL_ACID, ROW_ACID);
}

/*
 * Prints Fear status
 */
static void prt_afraid()
{
   if (p_ptr->afraid)
   {
      c_put_str(TERM_ORANGE, "Afraid", COL_AFRAID, ROW_AFRAID);
   }
   else
   {
      put_str("      ", COL_AFRAID, ROW_AFRAID);
   }
}


/*
 * Prints Poisoned status
 */
static void prt_poisoned(void)
{
   if (p_ptr->poisoned)
   {
      c_put_str(TERM_ORANGE,
                "Poisoned", COL_POISONED, ROW_POISONED);
   }
   else
   {
      put_str(  "        ", COL_POISONED, ROW_POISONED);
   }
}


/*
 * Prints Searching, Resting, Paralysis, or 'count' status
 * Display is always exactly 10 characters wide (see below)
 *
 * This function was a major bottleneck when resting, so a lot of
 * the text formatting code was optimized in place below.
 */
static void prt_state(void)
{
   byte attr = TERM_WHITE;

   char text[16];


   /* Paralysis */
   if (p_ptr->paralyzed)
   {
      attr = TERM_RED;

      strcpy(text, "Paralyzed!");
   }

   /* Resting */
   else if (p_ptr->resting)
   {
      s16b i;

      /* Start with "Rest" */
      strcpy(text, "Rest      ");

      /* Extensive (timed) rest */
      if (p_ptr->resting >= 1000)
      {
         i = p_ptr->resting / 100;
         text[9] = '0';
         text[8] = '0';
         text[7] = '0' + (i % 10);
         if (i >= 10)
         {
            i = i / 10;
            text[6] = '0' + (i % 10);
            if (i >= 10)
            {
               text[5] = '0' + (i / 10);
            }
         }
      }

      /* Long (timed) rest */
      else if (p_ptr->resting >= 100)
      {
         i = p_ptr->resting;
         text[9] = '0' + (i % 10);
         i = i / 10;
         text[8] = '0' + (i % 10);
         text[7] = '0' + (i / 10);
      }

      /* Medium (timed) rest */
      else if (p_ptr->resting >= 10)
      {
         i = p_ptr->resting;
         text[9] = '0' + (i % 10);
         text[8] = '0' + (i / 10);
      }

      /* Short (timed) rest */
      else if (p_ptr->resting > 0)
      {
         i = p_ptr->resting;
         text[9] = '0' + (i);
      }

      /* Rest until healed */
      else if (p_ptr->resting == -1)
      {
         text[5] = text[6] = text[7] = text[8] = text[9] = '*';
      }

      /* Rest until done */
      else if (p_ptr->resting == -2)
      {
         text[5] = text[6] = text[7] = text[8] = text[9] = '&';
      }
   }

   /* Repeating */
   else if (p_ptr->command_rep)
   {
      if (p_ptr->command_rep > 999)
      {
         (void)sprintf(text, "Rep. %3d00", p_ptr->command_rep / 100);
      }
      else
      {
         (void)sprintf(text, "Repeat %3d", p_ptr->command_rep);
      }
   }

   /* Searching */
   else if (p_ptr->searching)
   {
      strcpy(text, "Searching ");
   }

   /* Nothing interesting */
   else
   {
      strcpy(text, "          ");
   }

   /* Display the info (or blanks) */
   c_put_str(attr, text, COL_STATE, ROW_STATE);
}


/*
 * Prints the speed of a character.                -CJS-
 */
static void prt_speed()
{
   s16b i = p_ptr->pspeed;

   s16b attr = TERM_WHITE;
   char buf[12] = "";

   /* Hack -- Visually "undo" the Search Mode Slowdown */
   if (p_ptr->searching) i += 10;

   /* Fast */
   if (i > 110)
   {
      attr = TERM_L_GREEN;
      sprintf(buf, "Fast (+%d)", (i - 110));
   }

   /* Slow */
   else if (i < 110)
   {
      attr = TERM_L_BROWN;
      sprintf(buf, "Slow (-%d)", (110 - i));
   }

   /* Display the speed */
   c_put_str(attr, format("%-11s", buf), COL_SPEED, ROW_SPEED);
}

static void prt_cut()
{
   s16b c = p_ptr->cut;

   if (c > 1000)     c_put_str(TERM_L_RED, "Mortal Cut ", COL_CUT, ROW_CUT);
   else if (c > 200) c_put_str(TERM_RED,   "Deep gash  ", COL_CUT, ROW_CUT);
   else if (c > 100) c_put_str(TERM_RED,    "Severe cut", COL_CUT, ROW_CUT);
   else if (c > 50)  c_put_str(TERM_ORANGE, "Nasty cut ", COL_CUT, ROW_CUT);
   else if (c > 25)  c_put_str(TERM_ORANGE, "Bad cut   ", COL_CUT, ROW_CUT);
   else if (c > 10)  c_put_str(TERM_YELLOW, "Light cut ", COL_CUT, ROW_CUT);
   else if (c)       c_put_str(TERM_YELLOW, "Graze     ", COL_CUT, ROW_CUT);
   else
                                    put_str("          ", COL_CUT, ROW_CUT);
}

static void prt_stun(void)
{
   s16b s = p_ptr->stun;

   if (s > 100)     c_put_str(TERM_L_RED,  "Knocked out"   , COL_STUN, ROW_STUN);
   else if (s > 50) c_put_str(TERM_RED,    "Very Dazed ", COL_STUN, ROW_STUN);
   else if (s > 50) c_put_str(TERM_ORANGE, "Heavy stun ", COL_STUN, ROW_STUN);
   else if (s > 25) c_put_str(TERM_ORANGE, "Staggering ", COL_STUN, ROW_STUN);
   else if (s)      c_put_str(TERM_YELLOW, "Stun       ", COL_STUN, ROW_STUN);
   else
                                   put_str("           "           , COL_STUN, ROW_STUN);
}



/*
 * Redraw the "monster health bar"     -DRS-
 * Rather extensive modifications by   -BEN-
 *
 * The "monster health bar" provides visual feedback on the "health"
 * of the monster currently being "tracked".  There are several ways
 * to "track" a monster, including targetting it, attacking it, and
 * affecting it (and nobody else) with a ranged attack.
 *
 * Display the monster health bar (affectionately known as the
 * "health-o-meter").  Clear health bar if nothing is being tracked.
 * Auto-track current target monster when bored.  Note that the
 * health-bar stops tracking any monster that "disappears".
 */
static void health_redraw(void)
{

#ifdef DRS_SHOW_HEALTH_BAR

   /* Disabled */
   if (!show_health_bar) return;

   /* Not tracking */
   if (!health_who)
   {
      /* Erase the health bar */
      Term_erase(COL_INFO, ROW_INFO, 12);
   }

   /* Tracking an unseen monster */
   else if (!mn_list[health_who].ml)
   {
      /* Indicate that the monster health is "unknown" */
      Term_putstr(COL_INFO, ROW_INFO, 12, TERM_WHITE, "[----------]");
   }

   /* Tracking a hallucinatory monster */
   else if (p_ptr->image)
   {
      /* Indicate that the monster health is "unknown" */
      Term_putstr(COL_INFO, ROW_INFO, 12, TERM_WHITE, "[----------]");
   }

   /* Tracking a dead monster */
   else if (!mn_list[health_who].hp < 0)
   {
      /* Indicate that the monster health is "unknown" */
      Term_putstr(COL_INFO, ROW_INFO, 12, TERM_WHITE, "[----------]");
   }

   /* Tracking a visible monster */
   else
   {
      s16b pct, len;

      monster_type *m_ptr = &mn_list[health_who];

      /* Default to almost dead */
      byte attr = TERM_RED;

      /* Extract the "percent" of health */
      pct = 100L * m_ptr->hp / m_ptr->maxhp;

      /* Badly wounded */
      if (pct >= 10) attr = TERM_L_RED;

      /* Wounded */
      if (pct >= 25) attr = TERM_ORANGE;

      /* Somewhat Wounded */
      if (pct >= 60) attr = TERM_YELLOW;

      /* Healthy */
      if (pct >= 100) attr = TERM_L_GREEN;

      /* Afraid */
      if (m_ptr->afraid) attr = TERM_VIOLET;

      if (m_ptr->escaping) attr = TERM_WHITE;

      /* Asleep */
      if (m_ptr->csleep) attr = TERM_BLUE;

      /* Convert percent into "health" */
      len = (pct < 10) ? 1 : (pct < 90) ? (pct / 10 + 1) : 10;

      /* Default to "unknown" */
      Term_putstr(COL_INFO, ROW_INFO, 12, TERM_WHITE, "[----------]");

      /* Dump the current "health" (use '*' symbols) */
      Term_putstr(COL_INFO + 1, ROW_INFO, len, attr, "**********");
   }

#endif

}



/*
 * Display basic info (mostly left of map)
 */
static void prt_frame_basic()
{
   s16b i;

   /* Race and Class */
   prt_field(rp_ptr->title, ROW_RACE, COL_RACE);
   prt_field(cp_ptr->title, ROW_CLASS, COL_CLASS);

   /* Title */
   prt_title();

   /* Level/Experience */
   prt_level();
   prt_exp();

   /* All Stats */
   for (i = 0; i < 6; i++) prt_stat(i);

   /* Armor */
   prt_ac();

   /* Hitpoints */
   prt_hp();

   /* Spell points (mana) */
   prt_sp();

   /* Gold */
   prt_gold();

   /* Special */
   if (show_health_bar) health_redraw();

   /* Current depth (bottom right) */
   prt_depth();

/* jk */
   prt_position();
}


/*
 * Display extra info (mostly below map)
 */
static void prt_frame_extra()
{
   /* Cut/Stun */
   prt_cut();
   prt_stun();

   /* Food */
   prt_hunger();

   /* Various */
   prt_blind();
   prt_confused();
   prt_afraid();
   prt_poisoned();
/* jk */
   prt_sliding();
   prt_throat();
   prt_cold();
   prt_fire();
   prt_acid();
   prt_elec();

   /* State */
   prt_state();

   /* Speed */
   prt_speed();

}

static void display_last_kills(void)
{
   s16b i, j;

   Term_clear();
   j=0;

   for (i=0; i < 25; i++)
   {
      monster_race *r_ptr;

      /* don't display empty lines */
      if (last_kills[i].x == 0) continue;
      /* if term = 10 high, don't display first 15 entries */
      if (i < (25 - Term->hgt)) continue;

      r_ptr = &r_info[last_kills[i].x];
      if (last_kills[i].y == 1)
      {
         put_str(r_name + r_ptr->name, 0, j++);
      }
      else
      {
         put_str(format("%s (%dx)", r_name + r_ptr->name, last_kills[i].y), 0, j++);
      } 
   }
}

/*
 * Hack -- display last kills in sub-window
 */
static void fix_last_kills(void)
{
   int j;

   /* Scan windows */
   for (j = 0; j < 8; j++)
   {
      term *old = Term;

      /* No window */
      if (!angband_term[j]) continue;

      /* No relevant flags */
      if (!(op_ptr->window_flag[j] & (PW_LASTKILL))) continue;

      /* Activate */
      Term_activate(angband_term[j]);

      /* Display inventory */
      display_last_kills();

      /* Fresh */
      Term_fresh();

      /* Restore */
      Term_activate(old);
   }
}

/*
 * Hack -- display inventory in sub-windows
 */
static void fix_inven(void)
{
   int j;

   /* Scan windows */
   for (j = 0; j < 8; j++)
   {
      term *old = Term;

      /* No window */
      if (!angband_term[j]) continue;

      /* No relevant flags */
      if (!(op_ptr->window_flag[j] & (PW_INVEN))) continue;

      /* Activate */
      Term_activate(angband_term[j]);

      /* Display inventory */
      display_inven();

      /* Fresh */
      Term_fresh();

      /* Restore */
      Term_activate(old);
   }
}

/*
 * Hack -- display equipment in sub-windows
 */
static void fix_equip(void)
{
   int j;

   /* Scan windows */
   for (j = 0; j < 8; j++)
   {
      term *old = Term;

      /* No window */
      if (!angband_term[j]) continue;

      /* No relevant flags */
      if (!(op_ptr->window_flag[j] & (PW_EQUIP))) continue;

      /* Activate */
      Term_activate(angband_term[j]);

      /* Display equipment */
      display_equip();

      /* Fresh */
      Term_fresh();

      /* Restore */
      Term_activate(old);
   }
}


/*
 * Hack -- display player in sub-windows (mode 0)
 */
static void fix_player_0(void)
{
   int j;

   /* Scan windows */
   for (j = 0; j < 8; j++)
   {
      term *old = Term;

      /* No window */
      if (!angband_term[j]) continue;

      /* No relevant flags */
      if (!(op_ptr->window_flag[j] & (PW_PLAYER_0))) continue;

      /* Activate */
      Term_activate(angband_term[j]);

      /* Display player */
      display_player(0, TRUE);

      /* Fresh */
      Term_fresh();

      /* Restore */
      Term_activate(old);
   }
}



/*
 * Hack -- display player in sub-windows (mode 1)
 */
static void fix_player_1(void)
{
   int j;

   /* Scan windows */
   for (j = 0; j < 8; j++)
   {
      term *old = Term;

      /* No window */
      if (!angband_term[j]) continue;

      /* No relevant flags */
      if (!(op_ptr->window_flag[j] & (PW_PLAYER_1))) continue;

      /* Activate */
      Term_activate(angband_term[j]);

      /* Display flags */
      display_player(1, TRUE);

      /* Fresh */
      Term_fresh();

      /* Restore */
      Term_activate(old);
   }
}


/*
 * Hack -- display recent messages in sub-windows
 *
 * Adjust for width and split messages.  XXX XXX XXX
 */
static void fix_message(void)
{
   s16b j;
   int h, w;
   int y, x;
   s32b i, max;
   term *old = Term;

   /* Scan windows */
   for (j = 0; j < 8; j++)
   {
      if (!angband_term[j]) continue; /* No window */

      if (!(op_ptr->window_flag[j] & (PW_MESSAGE))) continue; /* No relevant flags */
      Term_activate(angband_term[j]);                         /* Activate */

      Term_get_size(&w, &h);                                  /* Get size */
      max=message_num();
dlog(DEBUGMESG,"xtra1.c: fix_message: message_num %ld h %d\n", max, h);      
      if ((s32b)h > max) h = (s32b)max;
      if (h)
      {
dlog(DEBUGMESG,"xtra1.c: fix_message: printing messages %d to %d\n", 0, h);      
         for (i = 0; i < h; i++)                              /* Dump messages */
         {
            /* Dump the message on the appropriate line */
dlog(DEBUGMESG,"xtra1.c: fix_message: messages %d = %s\n", i, message_str(i));      
            Term_putstr(0, (h - 1) - i, -1, TERM_WHITE, message_str(i));

            Term_locate(&x, &y);                              /* Cursor */

            Term_erase(x, y, 255);                            /* Clear to end of line */
         }

         Term_fresh();                                        /* Fresh */
      }
   }
/* activate term here, so that even if there are no messages, the old term is */
/* reactivated!                                                               */
   Term_activate(old);                                     /* Restore */
}


/*
 * Hack -- display overhead view in sub-windows
 *
 * Note that the "player" symbol does NOT appear on the map.
 */
static void fix_overhead(void)
{
   int j;

   /* Scan windows */
   for (j = 0; j < 8; j++)
   {
      term *old = Term;

      /* No window */
      if (!angband_term[j]) continue;

      /* No relevant flags */
      if (!(op_ptr->window_flag[j] & (PW_OVERHEAD))) continue;

      /* Activate */
      Term_activate(angband_term[j]);
      /* Redraw map */
      do_cmd_view_map(FALSE);

      /* Fresh */
      Term_fresh();

      /* Restore */
      Term_activate(old);
   }
}

/*
 * Hack -- display monster recall in sub-windows
 */
static void fix_monster(void)
{
   int j;

   /* Scan windows */
   for (j = 0; j < 8; j++)
   {
      term *old = Term;

      /* No window */
      if (!angband_term[j]) continue;

      /* No relevant flags */
      if (!(op_ptr->window_flag[j] & (PW_MONSTER))) continue;

      /* Activate */
      Term_activate(angband_term[j]);

      /* Display monster race info */
      if (p_ptr->monster_race_idx) display_roff(p_ptr->monster_race_idx);

      /* Fresh */
      Term_fresh();

      /* Restore */
      Term_activate(old);
   }
}

/*
 * Calculate maximum mana.  You do not need to know any spells.
 * Note that mana is lowered by heavy (or inappropriate) armor.
 *
 * This function induces status messages.
 */
static void calc_mana(void)
{
   s16b         new_mana, levels, cur_wgt, max_wgt;

   object_type *i_ptr;

   /* Hack -- Must be literate */
   if (!cp_ptr->spell_stat) return;

   levels = (p_ptr->lev - cp_ptr->spell_level) + 1;

   /* Hack -- no negative mana */
   if (levels < 0) levels = 0;

   /* Extract total mana */
   new_mana = adj_mag_mana[p_ptr->stat_ind[cp_ptr->spell_stat]] * levels / 2;

   /* Hack -- always start with 2 mana */
   new_mana += 2;

   /* Hack -- allow high priests a bonus ranging from +5 on lower levels to +0 @ level 50 */
   if (p_ptr->pclass == CLASS_HIGHPRST)
   {
      new_mana+=(5-(p_ptr->lev/10));
   }

   /* Only mages are affected */
   if (cp_ptr->spell_stat==A_INT)
   {
      u64b f1, f2, f3;

      /* Assume player is not encumbered by gloves */
      p_ptr->cumber_glove = FALSE;

      /* Get the gloves */
      i_ptr = &inventory[INVEN_HANDS];

      /* Examine the gloves */
      object_flags(i_ptr, &f1, &f2, &f3);

      /* Normal gloves hurt mage-type spells */
      if (i_ptr->k_idx &&
          !(f2 & TR2_FREE_ACT) &&
          !( (f1 & TR1_DEX1) && (i_ptr->p1val > 0) ) &&
          !( (f1 & TR1_DEX2) && (i_ptr->p2val > 0) ) )
      {
         /* Encumbered */
         p_ptr->cumber_glove = TRUE;

         /* Reduce mana */
         new_mana = (3 * new_mana) / 4;
      }
   }

   /* Assume player not encumbered by armor */
   p_ptr->cumber_armor = FALSE;

   /* Weigh the armor */
   cur_wgt = 0;
   cur_wgt += inventory[INVEN_BODY].weight;
   cur_wgt += inventory[INVEN_HEAD].weight;
   cur_wgt += inventory[INVEN_ARM].weight;
   cur_wgt += inventory[INVEN_OUTER].weight;
   cur_wgt += inventory[INVEN_HANDS].weight;
   cur_wgt += inventory[INVEN_FEET].weight;

   /* Determine the weight allowance */
   max_wgt = cp_ptr->spell_encumbrance;

   /* Heavy armor penalizes mana */
   if (((cur_wgt - max_wgt) / 10) > 0)
   {
      /* Encumbered */
      p_ptr->cumber_armor = TRUE;

      /* Reduce mana */
      new_mana -= ((cur_wgt - max_wgt) / 10);
   }

   /* Mana can never be negative */
   if (new_mana < 0) new_mana = 0;

   /* Maximum mana has changed */
   if (p_ptr->msp != new_mana)
   {

      /* Enforce maximum */
      if (p_ptr->csp >= p_ptr->msp)
      {
         p_ptr->csp = p_ptr->msp;
         p_ptr->csp_frac = 0;
      }

      /* Save new mana */
      p_ptr->msp = new_mana;

      /* Display mana later */
      p_ptr->redraw1 |= (PR1_MANA);
   }


   /* Hack -- handle "xtra" mode */
   if (character_xtra) return;

   /* Take note when "glove state" changes */
   if (p_ptr->old_cumber_glove != p_ptr->cumber_glove)
   {
      /* Message */
/* jk - message makes not much sense when 0 sp */
      if ( (p_ptr->cumber_glove) && (p_ptr->msp>0) )
      {
         msg_print("Your covered hands feel unsuitable for spellcasting.");
      }
      else if (p_ptr->msp>0)
      {
         msg_print("Your hands feel more suitable for spellcasting.");
      }

      /* Save it */
      p_ptr->old_cumber_glove = p_ptr->cumber_glove;
   }


   /* Take note when "armor state" changes */
   if (p_ptr->old_cumber_armor != p_ptr->cumber_armor)
   {
      /* Message */
      if (p_ptr->cumber_armor)
      {
         msg_print("The weight of your armor encumbers your movement.");
      }
      else
      {
         msg_print("You feel able to move more freely.");
      }

      /* Save it */
      p_ptr->old_cumber_armor = p_ptr->cumber_armor;
   }
}



/*
 * Calculate the players (maximal) hit points
 * Adjust current hitpoints if necessary
 */
static void calc_hitpoints()
{
   s16b bonus, mhp;

   /* Un-inflate "half-hitpoint bonus per level" value */
   bonus = ((int)(adj_con_mhp[p_ptr->stat_ind[A_CON]]) - 128);

   /* Calculate hitpoints */
   mhp = player_hp[p_ptr->lev-1] + (bonus * p_ptr->lev / 2);

   /* Always have at least one hitpoint per level */
   if (mhp < p_ptr->lev + 1) mhp = p_ptr->lev + 1;

   /* Factor in the hero / superhero settings */
   if (p_ptr->hero) mhp += 10;
   if (p_ptr->shero) mhp += 30;

   /* New maximum hitpoints */
   if (mhp != p_ptr->mhp)
   {
      s32b value;

      /* change current hit points proportionately to change of mhp */
      /* divide first to avoid overflow, little loss of accuracy */
      value = (((long)p_ptr->chp << 16) + p_ptr->chp_frac) / p_ptr->mhp;
      value = value * mhp;
      p_ptr->chp = (value >> 16);
      p_ptr->chp_frac = (value & 0xFFFF);

      /* Save the new max-hitpoints */
      p_ptr->mhp = mhp;

      /* Display hitpoints (later) */
      p_ptr->redraw1 |= (PR1_HP);
   }
}

/*
 * Extract and set the current "lite radius"
 */
static void calc_torch(void)
{
   object_type *i_ptr = &inventory[INVEN_LITE];

   /* Assume no light */
   p_ptr->cur_lite = 0;

   /* Player is glowing */
   if (p_ptr->lite) p_ptr->cur_lite = 1;

   /* Examine actual lites */
   if (i_ptr->tval == TV_LITE)
   {
      /* Torches (with fuel) provide some lite */
      if ((i_ptr->sval == SV_LITE_TORCH) && (i_ptr->p1val > 0))
      {
         p_ptr->cur_lite += 1;
      }

      /* Lanterns (with fuel) provide more lite */
      if ((i_ptr->sval == SV_LITE_LANTERN) && (i_ptr->p1val > 0))
      {
         p_ptr->cur_lite += 2;
      }

      /* Lanterns (with fuel) provide more lite */
      if ((i_ptr->sval == SV_LITE_NOLDOR) && (i_ptr->p1val > 0))
      {
         p_ptr->cur_lite += 4;
      }

      /* Artifact Lites provide permanent, bright, lite */
      if (artifact_p(i_ptr)) p_ptr->cur_lite += 3;
   }

   /* Reduce lite when running if requested */
   if (p_ptr->running && view_reduce_lite)
   {
      /* Reduce the lite radius if needed */
      if (p_ptr->cur_lite > 1) p_ptr->cur_lite = 1;
   }

   /* Notice changes in the "lite radius" */
   if (p_ptr->old_lite != p_ptr->cur_lite)
   {
      /* Update the lite */
      p_ptr->update |= PU_VIEW;

      /* Update the monsters */
      p_ptr->update |= (PU_MONSTERS);

      /* Remember the old lite */
      p_ptr->old_lite = p_ptr->cur_lite;
   }
}

/*
 * Computes current weight limit.
 */
s16b weight_limit(void)
{
   s16b i;

   /* Weight limit based only on strength */
   i = adj_str_wgt[p_ptr->stat_ind[A_STR]] * 100;

   /* Return the result */
   return (i);
}


/*
 * Calculate the players current "state", taking into account
 * not only race/class intrinsics, but also objects being worn
 * and temporary spell effects.
 *
 * See also calc_mana() and calc_hitpoints().
 *
 * Take note of the new "speed code", in particular, a very strong
 * player will start slowing down as soon as he reaches 150 pounds,
 * but not until he reaches 450 pounds will he be half as fast as
 * a normal kobold.  This both hurts and helps the player, hurts
 * because in the old days a player could just avoid 300 pounds,
 * and helps because now carrying 300 pounds is not very painful.
 *
 * The "weapon" and "bow" do *not* add to the bonuses to hit or to
 * damage, since that would affect non-combat things.  These values
 * are actually added in later, at the appropriate place.
 *
 * This function induces various "status" messages.
 */
static void calc_bonuses(void)
{
   s16b                 i, j, hold;
   s16b                 old_speed;
   s16b                 old_telepathy;
   s16b                 old_see_inv;
   s16b                 old_dis_ac;
   s16b                 old_dis_to_a;
   object_type         *i_ptr;
   u64b                 f1, f2, f3;

   /* Save the old speed */
   old_speed = p_ptr->pspeed;

   /* Save the old vision stuff */
   old_telepathy = p_ptr->telepathy;
   old_see_inv = p_ptr->see_inv;

   /* Save the old armor class */
   old_dis_ac = p_ptr->dis_ac;
   old_dis_to_a = p_ptr->dis_to_a;

   /* Clear the stat modifiers */
   for (i = 0; i < 6; i++) p_ptr->stat_add[i] = 0;

   /* Clear the Displayed/Real armor class */
   p_ptr->dis_ac = p_ptr->ac = 0;

   /* Clear the Displayed/Real Bonuses */
   p_ptr->dis_to_h = p_ptr->to_h = 0;
   p_ptr->dis_to_d = p_ptr->ring_to_d = p_ptr->to_d = 0;
   p_ptr->dis_to_a = p_ptr->to_a = 0;

   /* Clear all the flags */
   p_ptr->aggravate = FALSE;
   p_ptr->teleport = FALSE;
   p_ptr->exp_drain = FALSE;
   p_ptr->bless_blade = FALSE;
   p_ptr->xtra_might = 0;
   p_ptr->xtra_shots = 0;
   p_ptr->xtra_blows1 = 0;
   p_ptr->xtra_blows2 = 0;
   p_ptr->see_inv = FALSE;
   p_ptr->free_act = FALSE;
   p_ptr->slow_digest = FALSE;
   p_ptr->regenerate = FALSE;
   p_ptr->ffall = FALSE;
   p_ptr->hold_life = FALSE;
   p_ptr->telepathy = 0;
   p_ptr->lite = FALSE;
   p_ptr->sustain_str = FALSE;
   p_ptr->sustain_int = FALSE;
   p_ptr->sustain_wis = FALSE;
   p_ptr->sustain_con = FALSE;
   p_ptr->sustain_dex = FALSE;
   p_ptr->sustain_chr = FALSE;
   p_ptr->resist_acid = FALSE;
   p_ptr->resist_fire = FALSE;
   p_ptr->resist_elec = FALSE;
   p_ptr->resist_cold = FALSE;
   p_ptr->resist_pois = FALSE;
   p_ptr->resist_conf = FALSE;
   p_ptr->resist_sound = FALSE;
   p_ptr->resist_lite = FALSE;
   p_ptr->resist_dark = FALSE;
   p_ptr->resist_chaos = FALSE;
   p_ptr->resist_disen = FALSE;
   p_ptr->resist_shard = FALSE;
   p_ptr->resist_nexus = FALSE;
   p_ptr->resist_blind = FALSE;
   p_ptr->resist_neth = FALSE;
   p_ptr->resist_fear = FALSE;
   p_ptr->immune_acid = FALSE;
   p_ptr->immune_elec = FALSE;
   p_ptr->immune_fire = FALSE;
   p_ptr->immune_cold = FALSE;

   /* Base infravision (purely racial) */
   p_ptr->see_infra = rp_ptr->infra;

   /* Base skill -- disarming */
   p_ptr->skill_dis = rp_ptr->r_dis + cp_ptr->c_dis;

   /* Base skill -- magic devices */
   p_ptr->skill_dev = rp_ptr->r_dev + cp_ptr->c_dev;

   /* Base skill -- saving throw */
   p_ptr->skill_sav = rp_ptr->r_sav + cp_ptr->c_sav;

   /* Base skill -- stealth */
   p_ptr->skill_stl = rp_ptr->r_stl + cp_ptr->c_stl;

   /* Base skill -- searching ability */
   p_ptr->skill_srh = rp_ptr->r_srh + cp_ptr->c_srh;

   /* Base skill -- searching frequency */
   p_ptr->skill_pcp = rp_ptr->r_pcp + cp_ptr->c_pcp;

   /* Base skill -- combat (normal) */
   p_ptr->skill_thn = rp_ptr->r_thn + cp_ptr->c_thn;

/* jk - make mages fight progressively worse, priests too */
   if ((p_ptr->pclass==CLASS_MAGE) && (p_ptr->lev>8))
       p_ptr->skill_thn-=((p_ptr->lev-8)/2);
   if ((p_ptr->pclass==CLASS_PRIEST) && (p_ptr->lev>9))
       p_ptr->skill_thn-=((p_ptr->lev-9)/3);

   /* Base skill -- combat (shooting) */
   p_ptr->skill_thb = rp_ptr->r_thb + cp_ptr->c_thb;

   /* Base skill -- combat (throwing) */
   p_ptr->skill_tht = rp_ptr->r_thb + cp_ptr->c_thb;

   if (p_ptr->teach_birth)
   {
      p_ptr->skill_dis += p_ptr->teach_dis;
      p_ptr->skill_dev += p_ptr->teach_dev;
      p_ptr->skill_sav += p_ptr->teach_sav;
      p_ptr->skill_stl += p_ptr->teach_stl;
      p_ptr->skill_srh += p_ptr->teach_srh;
      p_ptr->skill_pcp += p_ptr->teach_pcp;
      p_ptr->skill_thn += p_ptr->teach_thn;
      p_ptr->skill_thb += p_ptr->teach_thb;
   }

   /* Base skill -- digging */
   p_ptr->skill_dig = 0;

   /* Elf */
   if (p_ptr->prace == RACE_ELF) p_ptr->resist_lite = TRUE;

   /* Hobbit */
   if (p_ptr->prace == RACE_HOBBIT) p_ptr->sustain_dex = TRUE;

   /* Gnome */
   if (p_ptr->prace == RACE_GNOME) p_ptr->free_act = TRUE;

   /* Dwarf */
   if (p_ptr->prace == RACE_DWARF) p_ptr->resist_blind = TRUE;

   /* Half-Orc */
   if (p_ptr->prace == RACE_HALF_ORC) p_ptr->resist_dark = TRUE;

   /* Half-Troll */
   if (p_ptr->prace == RACE_HALF_TROLL) p_ptr->sustain_str = TRUE;

   /* Dunadan */
   if (p_ptr->prace == RACE_DUNADAN) p_ptr->sustain_con = TRUE;

   /* High Elf */
   if (p_ptr->prace == RACE_HIGH_ELF) p_ptr->resist_lite = TRUE;
   if (p_ptr->prace == RACE_HIGH_ELF) p_ptr->see_inv = TRUE;

   /* Start with "normal" speed */
   p_ptr->pspeed = 110;

   /* but druedain start slow, and with telepathy */
   if (p_ptr->prace == RACE_DRUEDAIN)
   {
      p_ptr->pspeed = 105;
      p_ptr->telepathy = 9;
   }

   /* Start with a single blow per turn */
   p_ptr->num_blow1 = 1;
   p_ptr->num_blow2 = 0;
   p_ptr->dis_ring_to_d = 0;

   /* Start with a single shot per turn */
   p_ptr->num_fire = 1;

   /* Reset the "xtra" tval */
   p_ptr->tval_xtra = 0;

   /* Reset the "ammo" tval */
   p_ptr->tval_ammo = 0;

   /* Scan the usable inventory */
   for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
   {
      i_ptr = &inventory[i];

      /* Skip missing items */
      if (!i_ptr->k_idx) continue;

      /* Extract the item flags */
      object_flags(i_ptr, &f1, &f2, &f3);

      /* Affect stats */
      if (f1 & TR1_STR1) p_ptr->stat_add[A_STR] += i_ptr->p1val;
      if (f1 & TR1_INT1) p_ptr->stat_add[A_INT] += i_ptr->p1val;
      if (f1 & TR1_WIS1) p_ptr->stat_add[A_WIS] += i_ptr->p1val;
      if (f1 & TR1_DEX1) p_ptr->stat_add[A_DEX] += i_ptr->p1val;
      if (f1 & TR1_CON1) p_ptr->stat_add[A_CON] += i_ptr->p1val;
      if (f1 & TR1_CHR1) p_ptr->stat_add[A_CHR] += i_ptr->p1val;

      if (f1 & TR1_STR2) p_ptr->stat_add[A_STR] += i_ptr->p2val;
      if (f1 & TR1_INT2) p_ptr->stat_add[A_INT] += i_ptr->p2val;
      if (f1 & TR1_WIS2) p_ptr->stat_add[A_WIS] += i_ptr->p2val;
      if (f1 & TR1_DEX2) p_ptr->stat_add[A_DEX] += i_ptr->p2val;
      if (f1 & TR1_CON2) p_ptr->stat_add[A_CON] += i_ptr->p2val;
      if (f1 & TR1_CHR2) p_ptr->stat_add[A_CHR] += i_ptr->p2val;

      /* Affect stealth */
      if (f1 & TR1_STEALTH1) p_ptr->skill_stl += i_ptr->p1val;
      if (f1 & TR1_STEALTH2) p_ptr->skill_stl += i_ptr->p2val;

      /* Affect searching ability (factor of five) */
      if (f1 & TR1_SEARCH1) p_ptr->skill_srh += (i_ptr->p1val * 5);
      if (f1 & TR1_SEARCH2) p_ptr->skill_srh += (i_ptr->p2val * 5);

      /* Affect searching frequency (factor of five) */
      if (f1 & TR1_SEARCH1) p_ptr->skill_pcp += (i_ptr->p1val * 5);
      if (f1 & TR1_SEARCH2) p_ptr->skill_pcp += (i_ptr->p2val * 5);

      /* Affect infravision */
      if (f1 & TR1_INFRA1) p_ptr->see_infra += i_ptr->p1val;
      if (f1 & TR1_INFRA2) p_ptr->see_infra += i_ptr->p2val;

      /* Affect digging (factor of 20) */
      if (f1 & TR1_TUNNEL1) p_ptr->skill_dig += (i_ptr->p1val * 20);
      if (f1 & TR1_TUNNEL2) p_ptr->skill_dig += (i_ptr->p2val * 20);

      /* Affect speed */
      if (f1 & TR1_SPEED1) p_ptr->pspeed += i_ptr->p1val;
      if (f1 & TR1_SPEED2) p_ptr->pspeed += i_ptr->p2val;

      /* Affect blows */
      if (p_ptr->pclass == CLASS_GLADIATR)
      {
/* jk - gladiators: xtra blows are per weapon, eventual */
/* rings of extra attacks (don't exist yet) on work for */
/* 1st weapon on left hand, 2nd weapon on right hand */
         if (i == INVEN_WIELD)
         {
            if (f1 & TR1_BLOWS1) p_ptr->xtra_blows1 += i_ptr->p1val;
            if (f1 & TR1_BLOWS2) p_ptr->xtra_blows1 += i_ptr->p2val;
         }
         else if (i == INVEN_ARM)
         {
            if (f1 & TR1_BLOWS1) p_ptr->xtra_blows2 += i_ptr->p1val;
            if (f1 & TR1_BLOWS2) p_ptr->xtra_blows2 += i_ptr->p2val;
         }
         else if (i == INVEN_LEFT)
         {
            if (f1 & TR1_BLOWS1) p_ptr->xtra_blows1 += i_ptr->p1val;
            if (f1 & TR1_BLOWS2) p_ptr->xtra_blows1 += i_ptr->p2val;
         }
         else if (i == INVEN_RIGHT)
         {
            if (f1 & TR1_BLOWS1) p_ptr->xtra_blows2 += i_ptr->p1val;
            if (f1 & TR1_BLOWS2) p_ptr->xtra_blows2 += i_ptr->p2val;
         }
         else
         {
            if (f1 & TR1_BLOWS1) p_ptr->xtra_blows1 += i_ptr->p1val;
            if (f1 & TR1_BLOWS2) p_ptr->xtra_blows1 += i_ptr->p2val;
         }

      }
      else
      {
         if (f1 & TR1_BLOWS1) p_ptr->xtra_blows1 += i_ptr->p1val;
         if (f1 & TR1_BLOWS2) p_ptr->xtra_blows1 += i_ptr->p2val;
      }

      /* Affect shots */
      if (f1 & (TR1_SHOTS1)) p_ptr->xtra_shots += i_ptr->p1val;
      if (f1 & (TR1_SHOTS2)) p_ptr->xtra_shots += i_ptr->p2val;

      /* Affect Might */
      if (f1 & (TR1_MIGHT1)) p_ptr->xtra_might += i_ptr->p1val;
      if (f1 & (TR1_MIGHT2)) p_ptr->xtra_might += i_ptr->p2val;

      /* affect magic skill */
      if (f1 & TR1_MAGIC1) p_ptr->skill_dev += i_ptr->p1val*12;
      if (f1 & TR1_MAGIC2) p_ptr->skill_dev += i_ptr->p2val*12;

      /* affect lite radius */
      if (f1 & TR1_LITE1) p_ptr->lite += i_ptr->p1val;
      if (f1 & TR1_LITE2) p_ptr->lite += i_ptr->p2val;

      /* Various flags */
      if (f3 & TR3_AGGRAVATE) p_ptr->aggravate = TRUE;
      if (f3 & TR3_TELEPORT) p_ptr->teleport = TRUE;
      if (f3 & TR3_DRAIN_EXP) p_ptr->exp_drain = TRUE;
      if (f3 & TR3_BLESSED) p_ptr->bless_blade = TRUE;
      if (f3 & TR3_SLOW_DIGEST) p_ptr->slow_digest = TRUE;
      if (f3 & TR3_REGEN) p_ptr->regenerate = TRUE;
      if (f3 & TR3_TELEPATHY) p_ptr->telepathy += 30;
      if (f3 & TR3_SOMELITE) p_ptr->lite = p_ptr->lite + 1;
      if (f3 & TR3_SEE_INVIS) p_ptr->see_inv = TRUE;
      if (f3 & TR3_FEATHER) p_ptr->ffall = TRUE;
      if (f2 & TR2_FREE_ACT) p_ptr->free_act = TRUE;
      if (f2 & TR2_HOLD_LIFE) p_ptr->hold_life = TRUE;

      /* Immunity flags */
      if (f2 & TR2_IM_FIRE) p_ptr->immune_fire = TRUE;
      if (f2 & TR2_IM_ACID) p_ptr->immune_acid = TRUE;
      if (f2 & TR2_IM_COLD) p_ptr->immune_cold = TRUE;
      if (f2 & TR2_IM_ELEC) p_ptr->immune_elec = TRUE;

      /* Resistance flags */
      if (f2 & TR2_RES_ACID) p_ptr->resist_acid = TRUE;
      if (f2 & TR2_RES_ELEC) p_ptr->resist_elec = TRUE;
      if (f2 & TR2_RES_FIRE) p_ptr->resist_fire = TRUE;
      if (f2 & TR2_RES_COLD) p_ptr->resist_cold = TRUE;
      if (f2 & TR2_RES_POIS) p_ptr->resist_pois = TRUE;
      if (f2 & TR2_RES_CONF) p_ptr->resist_conf = TRUE;
      if (f2 & TR2_RES_SOUND) p_ptr->resist_sound = TRUE;
      if (f2 & TR2_RES_LITE) p_ptr->resist_lite = TRUE;
      if (f2 & TR2_RES_DARK) p_ptr->resist_dark = TRUE;
      if (f2 & TR2_RES_CHAOS) p_ptr->resist_chaos = TRUE;
      if (f2 & TR2_RES_DISEN) p_ptr->resist_disen = TRUE;
      if (f2 & TR2_RES_SHARDS) p_ptr->resist_shard = TRUE;
      if (f2 & TR2_RES_NEXUS) p_ptr->resist_nexus = TRUE;
      if (f2 & TR2_RES_BLIND) p_ptr->resist_blind = TRUE;
      if (f2 & TR2_RES_NETHER) p_ptr->resist_neth = TRUE;
      if (f2 & TR2_RES_FEAR) p_ptr->resist_fear = TRUE;

      /* Sustain flags */
      if (f2 & TR2_SUST_STR) p_ptr->sustain_str = TRUE;
      if (f2 & TR2_SUST_INT) p_ptr->sustain_int = TRUE;
      if (f2 & TR2_SUST_WIS) p_ptr->sustain_wis = TRUE;
      if (f2 & TR2_SUST_DEX) p_ptr->sustain_dex = TRUE;
      if (f2 & TR2_SUST_CON) p_ptr->sustain_con = TRUE;
      if (f2 & TR2_SUST_CHR) p_ptr->sustain_chr = TRUE;

      /* Modify the base armor class */
      p_ptr->ac += i_ptr->ac;

      /* The base armor class is always known */
      p_ptr->dis_ac += i_ptr->ac;

      /* Apply the bonuses to armor class */
      p_ptr->to_a += i_ptr->to_a;

      /* Apply the mental bonuses to armor class, if known */
      if (object_known_p(i_ptr)) p_ptr->dis_to_a += i_ptr->to_a;

      /* Hack -- do not apply "weapon" bonuses */
      if (i == INVEN_WIELD) continue;

      /* Hack -- do not apply "bow" bonuses */
      if (i == INVEN_BOW) continue;

      if ((p_ptr->pclass == CLASS_GLADIATR) && (i == INVEN_ARM)) continue;

      /* Apply the bonuses to hit/damage */
      p_ptr->to_h += i_ptr->to_h;

      if ((i == INVEN_LEFT) || (i == INVEN_RIGHT))
         p_ptr->ring_to_d += i_ptr->to_d;
      else
         p_ptr->to_d += i_ptr->to_d;


      /* Apply the mental bonuses tp hit/damage, if known */
      if ((i == INVEN_LEFT) || (i == INVEN_RIGHT))
      {
         if (object_known_p(i_ptr)) p_ptr->dis_ring_to_d += i_ptr->to_d;
      }
      else
      {
         if (object_known_p(i_ptr)) p_ptr->dis_to_h += i_ptr->to_h;
         if (object_known_p(i_ptr)) p_ptr->dis_to_d += i_ptr->to_d;
         p_ptr->to_d += i_ptr->to_d;
      }
   }

   /* Hack -- apply racial/class stat maxes */
   /* Apply the racial modifiers */
   for (i = 0; i < 6; i++)
   {
      if (!p_ptr->teach_birth)
      {
         /* Modify the stats for "race" */
         p_ptr->stat_add[i] += (rp_ptr->r_adj[i] + cp_ptr->c_adj[i]);
      }
      else
      {
         p_ptr->stat_add[i] += (rp_ptr->r_adj[i] + cp_ptr->c_adj[i]) / 3;
      }
   }


   /* jk - if we have mule legs, we can carry more but DEX suffers */
   if (p_ptr->lift)
   {
      p_ptr->stat_add[A_DEX] -= 5;
   }

   /* Calculate stats */
   for (i = 0; i < 6; i++)
   {
      s16b top, use, ind;

      /* Extract the new "stat_use" value for the stat */
      if (!p_ptr->teach_birth)
      {
         top = modify_stat_value(p_ptr->stat_max[i], p_ptr->stat_add[i]);
      }
      else
      {
         top = modify_stat_value(p_ptr->stat_max[i], p_ptr->teach_stat[i] + p_ptr->stat_add[i]);
      }

      /* Notice changes */
      if (p_ptr->stat_top[i] != top)
      {
         /* Save the new value */
         p_ptr->stat_top[i] = top;

         /* Redisplay the stats later */
         p_ptr->redraw1 |= (PR1_STATS);
      }

      /* Extract the new "stat_use" value for the stat */
      if (!p_ptr->teach_birth)
      {
         use = modify_stat_value(p_ptr->stat_cur[i], p_ptr->stat_add[i]);
      }
      else
      {
         use = modify_stat_value(p_ptr->stat_cur[i], p_ptr->teach_stat[i] + p_ptr->stat_add[i]);
      }

      /* Notice changes */
      if (p_ptr->stat_use[i] != use)
      {
         /* Save the new value */
         p_ptr->stat_use[i] = use;

         /* Redisplay the stats later */
         p_ptr->redraw1 |= (PR1_STATS);
      }

      /* Values: 3, 4, ..., 17 */
      if (use <= 18) ind = (use - 3);

      /* Ranges: 18/00-18/09, ..., 18/210-18/219 */
      else if (use <= 18+219) ind = (15 + (use - 18) / 10);

      /* Range: 18/220+ */
      else ind = (37);

      /* Notice changes */
      if (p_ptr->stat_ind[i] != ind)
      {
         /* Save the new index */
         p_ptr->stat_ind[i] = ind;

         /* Change in CON affects Hitpoints */
         if (i == A_CON)
         {
            p_ptr->update |= (PU_HP);
         }

         /* Change in INT may affect Mana/Spells */
         else if (i == A_INT)
         {
            if (cp_ptr->spell_stat == A_INT)
            {
               p_ptr->update |= PU_MANA;
            }
         }

         /* Change in WIS may affect Mana/Spells */
         else if (i == A_WIS)
         {
            if (cp_ptr->spell_stat == A_WIS)
            {
               p_ptr->update |= PU_MANA;
            }
         }
      }
   }

/* jk - add in the tactics */

   p_ptr->dis_to_h += tactic_info[p_ptr->tactic].to_hit;
   p_ptr->to_h += tactic_info[p_ptr->tactic].to_hit;
   p_ptr->dis_to_d += tactic_info[p_ptr->tactic].to_dam;
   p_ptr->to_d += tactic_info[p_ptr->tactic].to_dam;
   p_ptr->dis_to_a += tactic_info[p_ptr->tactic].to_ac;
   p_ptr->to_a += tactic_info[p_ptr->tactic].to_ac;

   p_ptr->skill_stl += tactic_info[p_ptr->tactic].to_stealth;
   p_ptr->skill_dis += tactic_info[p_ptr->tactic].to_disarm;
   p_ptr->skill_sav += tactic_info[p_ptr->tactic].to_saving;

   p_ptr->pspeed += move_info[p_ptr->movement].to_speed;
   p_ptr->skill_srh += move_info[p_ptr->movement].to_search;
   p_ptr->skill_pcp += move_info[p_ptr->movement].to_percep;
   p_ptr->skill_stl += move_info[p_ptr->movement].to_stealth;

   /* Apply temporary "stun" */
   if (p_ptr->stun > 50)
   {
      p_ptr->to_h -= 20;
      p_ptr->dis_to_h -= 20;
      p_ptr->to_d -= 20;
      p_ptr->dis_to_d -= 20;
   }
   else if (p_ptr->stun)
   {
      p_ptr->to_h -= 5;
      p_ptr->dis_to_h -= 5;
      p_ptr->to_d -= 5;
      p_ptr->dis_to_d -= 5;
   }

   /* Invulnerability */
   if (p_ptr->invuln)
   {
      p_ptr->to_a += 100;
      p_ptr->dis_to_a += 100;
   }

   /* Temporary blessing */
   if (p_ptr->blessed)
   {
      p_ptr->to_a += 5;
      p_ptr->dis_to_a += 5;
      p_ptr->to_h += 10;
      p_ptr->dis_to_h += 10;
   }

   /* Temporary shield */
   if (p_ptr->shield)
   {
      p_ptr->to_a += 50;
      p_ptr->dis_to_a += 50;
   }

   /* Temporary "Hero" */
   if (p_ptr->hero)
   {
      p_ptr->to_h += 12;
      p_ptr->dis_to_h += 12;
   }

   /* Temporary "Beserk" */
   if (p_ptr->shero)
   {
      p_ptr->to_h += 24;
      p_ptr->dis_to_h += 24;
      p_ptr->to_a -= 10;
      p_ptr->dis_to_a -= 10;
   }

   /* Temporary "fast" */
   if (p_ptr->fast)
   {
       p_ptr->pspeed += 10;
   }
   if (p_ptr->food>PY_FOOD_MAX)
   {
      if (p_ptr->food<(PY_FOOD_MAX+100)) p_ptr->pspeed -= 1;
      else if (p_ptr->food<(PY_FOOD_MAX+750)) p_ptr->pspeed -=2;
      else if (p_ptr->food<(PY_FOOD_MAX+2000)) p_ptr->pspeed -=4;
      else if (p_ptr->food<(PY_FOOD_MAX+5000)) p_ptr->pspeed -=7;
      else p_ptr->pspeed -=10;
   }

   /* Temporary "slow" */
   if (p_ptr->slow)
   {
      p_ptr->pspeed -= 10;
   }

   /* Temporary see invisible */
   if (p_ptr->tim_invis)
   {
      p_ptr->see_inv = TRUE;
   }

   /* Temporary infravision boost */
   if (p_ptr->tim_infra)
   {
      p_ptr->see_infra++;
   }

   /* Hack -- Res Chaos -> Res Conf */
   if (p_ptr->resist_chaos)
   {
      p_ptr->resist_conf = TRUE;
   }

   /* Hack -- Hero/Shero -> Res fear */
   if (p_ptr->hero || p_ptr->shero)
   {
      p_ptr->resist_fear = TRUE;
   }

   /* Hack -- Telepathy Change */
   if (p_ptr->telepathy != old_telepathy)
   {
      p_ptr->update |= (PU_MONSTERS);
   }

   /* Hack -- See Invis Change */
   if (p_ptr->see_inv != old_see_inv)
   {
      p_ptr->update |= (PU_MONSTERS);
   }

   /* Extract the current weight (in tenth pounds) */

   j = p_ptr->total_weight;

   /* Extract the "weight limit" (in tenth pounds) */
   i = weight_limit();
   /* jk - if we have mule legs, we can carry more */
   /* but stealth suffers! */
   if (p_ptr->lift)
   {
      i = 3 * i;
      p_ptr->skill_stl -= 5;
   }

   /* XXX XXX XXX Apply "encumbrance" from weight */
   if (j > i/2) p_ptr->pspeed -= ((j - (i/2)) / (i / 10));

   /* Bloating slows the player down (a little) */
   if (p_ptr->food >= PY_FOOD_MAX) p_ptr->pspeed -= 10;

   /* Searching slows the player down */
   if (p_ptr->searching) p_ptr->pspeed -= 10;

   /* Display the speed (if needed) */
   if (p_ptr->pspeed != old_speed) p_ptr->redraw1 |= (PR1_SPEED);

   /* Actual Modifier Bonuses (Un-inflate stat bonuses) */
   p_ptr->to_a += ((int)(adj_dex_ta[p_ptr->stat_ind[A_DEX]]) - 128);
   p_ptr->to_d += ((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128);
   p_ptr->to_h += ((int)(adj_dex_th[p_ptr->stat_ind[A_DEX]]) - 128);
   p_ptr->to_h += ((int)(adj_str_th[p_ptr->stat_ind[A_STR]]) - 128);

   /* Displayed Modifier Bonuses (Un-inflate stat bonuses) */
   p_ptr->dis_to_a += ((int)(adj_dex_ta[p_ptr->stat_ind[A_DEX]]) - 128);
   p_ptr->dis_to_d += ((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128);
   p_ptr->dis_to_h += ((int)(adj_dex_th[p_ptr->stat_ind[A_DEX]]) - 128);
   p_ptr->dis_to_h += ((int)(adj_str_th[p_ptr->stat_ind[A_STR]]) - 128);

   /* Redraw armor (if needed) */
   if (p_ptr->dis_ac != old_dis_ac) p_ptr->redraw1 |= (PR1_ARMOR);
   if (p_ptr->dis_to_a != old_dis_to_a) p_ptr->redraw1 |= (PR1_ARMOR);

   /* Obtain the "hold" value */
   hold = adj_str_hold[p_ptr->stat_ind[A_STR]];

   /* Examine the "current bow" */
   i_ptr = &inventory[INVEN_BOW];

   /* Assume not heavy */
   p_ptr->heavy_shoot = FALSE;

   /* It is hard to carholdry a heavy bow */
   if (hold < i_ptr->weight / 10)
   {
      /* Hard to wield a heavy bow */
      p_ptr->to_h += 2 * (hold - i_ptr->weight / 10);
      p_ptr->dis_to_h += 2 * (hold - i_ptr->weight / 10);

      /* Heavy Bow */
      p_ptr->heavy_shoot = TRUE;
   }

   /* Compute "extra shots" if needed */
   if (i_ptr->k_idx)
   {
      /* Take note of required "tval" for missiles */
      switch (i_ptr->sval)
      {
         case SV_HOBBIT_SLING:
         case SV_SLING:
             p_ptr->tval_ammo = TV_SHOT;
             break;

         case SV_SHORT_BOW:
         case SV_LONG_BOW:
         case SV_ELVEN_BOW:
             p_ptr->tval_ammo = TV_ARROW;
             break;

         case SV_LIGHT_XBOW:
         case SV_HEAVY_XBOW:
             p_ptr->tval_ammo = TV_BOLT;
             break;
      }

      /* Hack -- Reward High Level Rangers using Bows */
      if ((p_ptr->pclass == CLASS_RANGER) &&
          (p_ptr->tval_ammo == TV_ARROW) &&
          (!p_ptr->heavy_shoot) )
      {
         /* Extra shot at level 20 */
         if (p_ptr->lev >= 20) p_ptr->num_fire++;

         /* Extra shot at level 40 */
         if (p_ptr->lev >= 40) p_ptr->num_fire++;
      }

      /* Add in the "bonus shots" */
      p_ptr->num_fire += p_ptr->xtra_shots;

      /* Require at least one shot */
      if (p_ptr->num_fire < 1) p_ptr->num_fire = 1;
   }

   /* Examine the "main weapon" */
   i_ptr = &inventory[INVEN_WIELD];

   /* Assume not heavy */
   p_ptr->heavy_wield1 = FALSE;

   /* It is hard to hold a heavy weapon */
   if (hold < i_ptr->weight / 10)
   {
      /* Hard to wield a heavy weapon */
      p_ptr->to_h += 2 * (hold - i_ptr->weight / 10);
      p_ptr->dis_to_h += 2 * (hold - i_ptr->weight / 10);

      /* Heavy weapon */
      p_ptr->heavy_wield1 = TRUE;
   }

   p_ptr->heavy_wield2 = FALSE;

   if (p_ptr->pclass == CLASS_GLADIATR)
   {
      i_ptr = &inventory[INVEN_ARM];
      /* It is hard to hold a heavy weapon */
/* jk - and a secondary weapon may only be half as heavy */
      if ((hold/2) < i_ptr->weight / 10)
      {
         /* Hard to wield a heavy weapon */
         p_ptr->to_h += 2 * (hold - i_ptr->weight / 10);
         p_ptr->dis_to_h += 2 * (hold - i_ptr->weight / 10);

         /* Heavy weapon */
         p_ptr->heavy_wield2 = TRUE;
      }
      /* restore i_ptr to main weapon */
      i_ptr = &inventory[INVEN_WIELD];
   }

   /* Assume okay */
   p_ptr->icky_wield = FALSE;

   /* Priest weapon penalty for non-blessed edged weapons */
   if ((p_ptr->pclass == CLASS_PRIEST) && (!p_ptr->bless_blade) &&
       ((i_ptr->tval == TV_SWORD) || (i_ptr->tval == TV_POLEARM)))
   {
      /* Reduce the real bonuses */
      p_ptr->to_h -= 2;
      p_ptr->to_d -= 2;

      /* Reduce the mental bonuses */
      p_ptr->dis_to_h -= 2;
      p_ptr->dis_to_d -= 2;

      /* Icky weapon */
      p_ptr->icky_wield = TRUE;
   }

   /* jk - Mages get fewer and fewer blows.... */
   if ((p_ptr->pclass == CLASS_MAGE) && (inventory[INVEN_WIELD].k_idx))
   {
      bool mage_weapon = FALSE;
      if ( (inventory[INVEN_WIELD].tval == TV_HAFTED) &&
           (inventory[INVEN_WIELD].sval == SV_QUARTERSTAFF) )
      {
         mage_weapon = TRUE;
      }
      if ( (inventory[INVEN_WIELD].tval == TV_SWORD) &&
           ( (inventory[INVEN_WIELD].sval == SV_BROKEN_DAGGER) ||
             (inventory[INVEN_WIELD].sval == SV_DAGGER) ) )
      {
         mage_weapon = TRUE;
      }

      if (!mage_weapon)
      {
         p_ptr->icky_wield = TRUE;
      }
   }

   /* Normal weapons */
   if (i_ptr->k_idx && !p_ptr->heavy_wield1)
   {
      s16b str_index, dex_index, div;

      /* Enforce a minimum "weight" (tenth pounds) */
      div = ((i_ptr->weight < cp_ptr->c_wgt) ? cp_ptr->c_wgt : i_ptr->weight);

      /* Access the strength vs weight */
      str_index = (adj_str_blow[p_ptr->stat_ind[A_STR]] * cp_ptr->c_mul / div);

      /* Maximal value */
      if (str_index > 11) str_index = 11;

      /* Index by dexterity */
      dex_index = (adj_dex_blow[p_ptr->stat_ind[A_DEX]]);

      /* Maximal value */
      if (dex_index > 11) dex_index = 11;

      /* Use the blows table */

      p_ptr->num_blow1 = blows_table[str_index][dex_index];

      /* Maximal value */
      if (p_ptr->num_blow1 > cp_ptr->c_num) p_ptr->num_blow1 = cp_ptr->c_num;

      /* Mages start with 2 or 3 attacks max after level 35 resp 15 */
      /* with non-mage weapons - see above */
      if ((p_ptr->pclass == CLASS_MAGE) && (p_ptr->icky_wield))
      {
         if ((p_ptr->lev >=35) && (p_ptr->num_blow1 > 2))
         {
            p_ptr->num_blow1 = 2;
         }
         else if ((p_ptr->lev >=15) && (p_ptr->num_blow1 > 3))
         {
            p_ptr->num_blow1 = 3;
         }
      }

      /* Add in the "bonus blows" */
      p_ptr->num_blow1 += p_ptr->xtra_blows1;

      /* Require at least one blow */
      if (p_ptr->num_blow1 < 1) p_ptr->num_blow1 = 1;

      /* Boost digging skill by weapon weight */
      p_ptr->skill_dig += (i_ptr->weight / 10);
   }

   if (p_ptr->pclass == CLASS_GLADIATR)
   {
      i_ptr = &inventory[INVEN_ARM];
      /* Normal weapons */
      if (i_ptr->k_idx && !p_ptr->heavy_wield2)
      {
         s16b str_index, dex_index;
         s16b num = 6, wgt = 30, mul = 3, div = 0;

         /* Enforce a minimum "weight" (tenth pounds) */
         div = ((i_ptr->weight < wgt) ? wgt : i_ptr->weight);

         /* Access the strength vs weight */
         str_index = (adj_str_blow[p_ptr->stat_ind[A_STR]] * mul / div);

         /* Maximal value */
         if (str_index > 11) str_index = 11;

         /* Index by dexterity */
         dex_index = (adj_dex_blow[p_ptr->stat_ind[A_DEX]]);

         /* Maximal value */
         if (dex_index > 11) dex_index = 11;

         /* Use the blows table */

         p_ptr->num_blow2 = blows_table[str_index][dex_index];
dlog(DEBUGEXTRA,"xtra1.c: calc_bonuses: after table, num_blow2 = %d\n", p_ptr->num_blow2);

         /* Maximal value */
         if (p_ptr->num_blow2 > num) p_ptr->num_blow2 = num;
dlog(DEBUGEXTRA,"xtra1.c: calc_bonuses: after max, num_blow2 = %d\n", p_ptr->num_blow2);

         /* Add in the "bonus blows" */
         p_ptr->num_blow2 += p_ptr->xtra_blows2;
dlog(DEBUGEXTRA,"xtra1.c: calc_bonuses: after extra hits, num_blow2 = %d\n", p_ptr->num_blow2);

         /* Require at least one blow */
         if (p_ptr->num_blow2 < 1) p_ptr->num_blow2 = 1;
dlog(DEBUGEXTRA,"xtra1.c: calc_bonuses: after min, num_blow2 = %d\n", p_ptr->num_blow2);

         /* Boost digging skill by weapon weight */
         p_ptr->skill_dig += (i_ptr->weight / 10);
      }
   }

/* jk - warriors/gladiators now immune to fear at level 25 */
   if ( ( (p_ptr->pclass == CLASS_WARRIOR) || (p_ptr->pclass == CLASS_GLADIATR) ) &&
        (p_ptr->lev >= 25))
   {
      p_ptr->resist_fear = TRUE;
   }
/* jk - Druedain auto-regenerate from level 25 onwards */
   if ((p_ptr->prace == RACE_DRUEDAIN) && (p_ptr->lev >= 25) && (p_ptr->regenerate == 0))
   {
      p_ptr->regenerate = 1;
   }
   /* and they also get somewhat higher saving throws */
   if (p_ptr->prace == RACE_DRUEDAIN)
   {
      p_ptr->skill_sav += (p_ptr->lev / 3);
   }

   /* Affect Skill -- stealth (bonus one) */
   p_ptr->skill_stl += 1;

   /* Affect Skill -- disarming (DEX and INT) */
   p_ptr->skill_dis += adj_dex_dis[p_ptr->stat_ind[A_DEX]];
   p_ptr->skill_dis += adj_int_dis[p_ptr->stat_ind[A_INT]];

   /* Affect Skill -- magic devices (INT) */
   p_ptr->skill_dev += adj_int_dev[p_ptr->stat_ind[A_INT]];

   /* Affect Skill -- saving throw (WIS) */
   p_ptr->skill_sav += adj_wis_sav[p_ptr->stat_ind[A_WIS]];

   /* Affect Skill -- digging (STR) */
   p_ptr->skill_dig += adj_str_dig[p_ptr->stat_ind[A_STR]];


   /* Affect Skill -- disarming (Level, by Class) */
   p_ptr->skill_dis += (cp_ptr->x_dis * p_ptr->lev / 10);

   /* Affect Skill -- magic devices (Level, by Class) */
   p_ptr->skill_dev += (cp_ptr->x_dev * p_ptr->lev / 10);

   /* Affect Skill -- saving throw (Level, by Class) */
   p_ptr->skill_sav += (cp_ptr->x_sav * p_ptr->lev / 10);

   /* Affect Skill -- stealth (Level, by Class) */
   p_ptr->skill_stl += (cp_ptr->x_stl * p_ptr->lev / 10);

   /* Affect Skill -- search ability (Level, by Class) */
   p_ptr->skill_srh += (cp_ptr->x_srh * p_ptr->lev / 10);

   /* Affect Skill -- search frequency (Level, by Class) */
   p_ptr->skill_pcp += (cp_ptr->x_pcp * p_ptr->lev / 10);

   /* Affect Skill -- combat (normal) (Level, by Class) */
   p_ptr->skill_thn += (cp_ptr->x_thn * p_ptr->lev / 10);

   /* Affect Skill -- combat (shooting) (Level, by Class) */
   p_ptr->skill_thb += (cp_ptr->x_thb * p_ptr->lev / 10);

   /* Affect Skill -- combat (throwing) (Level, by Class) */
   p_ptr->skill_tht += (cp_ptr->x_thb * p_ptr->lev / 10);


   /* Limit Skill -- stealth from 0 to 30 */
   if (p_ptr->skill_stl > 30) p_ptr->skill_stl = 30;
   if (p_ptr->skill_stl < 0) p_ptr->skill_stl = 0;

   /* Limit Skill -- digging from 1 up */
   if (p_ptr->skill_dig < 1) p_ptr->skill_dig = 1;

   /* Hack -- handle "xtra" mode */
   if (character_xtra) return;

   /* Take note when "heavy bow" changes */
   if (p_ptr->old_heavy_shoot != p_ptr->heavy_shoot)
   {
      /* Message */
      if (p_ptr->heavy_shoot)
      {
         msg_print("You have trouble wielding such a heavy bow.");
      }
      else if (inventory[INVEN_BOW].k_idx)
      {
         msg_print("You have no trouble wielding your bow.");
      }
      else
      {
         msg_print("You feel relieved to put down your heavy bow.");
      }

      /* Save it */
      p_ptr->old_heavy_shoot = p_ptr->heavy_shoot;
   }


   /* Take note when "heavy weapon 1" changes */
   if (p_ptr->old_heavy_wield1 != p_ptr->heavy_wield1)
   {
      /* Message */
      if (p_ptr->heavy_wield1)
      {
         msg_print("You have trouble wielding such a heavy weapon.");
      }
      else if (inventory[INVEN_WIELD].k_idx)
      {
         msg_print("You have no trouble wielding your weapon.");
      }
      else
      {
         msg_print("You feel relieved to put down your heavy weapon.");
      }

      /* Save it */
      p_ptr->old_heavy_wield1 = p_ptr->heavy_wield1;
   }

   /* Take note when "heavy weapon 2" changes */
   if (p_ptr->old_heavy_wield2 != p_ptr->heavy_wield2)
   {
      /* Message */
      if (p_ptr->heavy_wield2)
      {
         msg_print("You have trouble wielding such a heavy weapon.");
      }
      else if (inventory[INVEN_WIELD].k_idx)
      {
         msg_print("You have no trouble wielding your weapon.");
      }
      else
      {
         msg_print("You feel relieved to put down your heavy weapon.");
      }

      /* Save it */
      p_ptr->old_heavy_wield1 = p_ptr->heavy_wield1;
   }

   /* Take note when "illegal weapon" changes */
   if (p_ptr->old_icky_wield != p_ptr->icky_wield)
   {
      /* Message */
      if (p_ptr->icky_wield)
      {
         msg_print("You do not feel comfortable with your weapon.");
      }
      else if (inventory[INVEN_WIELD].k_idx)
      {
         msg_print("You feel comfortable with your weapon.");
      }
      else
      {
         msg_print("You feel more comfortable after removing your weapon.");
      }

      /* Save it */
      p_ptr->old_icky_wield = p_ptr->icky_wield;
   }
}

/*
 * Handle "p_ptr->notice"
 */
void notice_stuff(void)
{
   /* Notice stuff */
   if (!p_ptr->notice) return;


   /* Combine the pack */
   if (p_ptr->notice & PN_COMBINE)
   {
      p_ptr->notice &= ~(PN_COMBINE);
      combine_pack();
   }

   /* Reorder the pack */
   if (p_ptr->notice & PN_REORDER)
   {
      p_ptr->notice &= ~(PN_REORDER);
      reorder_pack();
   }
}

/*
 * Handle "p_ptr->update"
 */
void update_stuff(void)
{
   /* Update stuff */
   if (!p_ptr->update) return;

   if (p_ptr->update & PU_BONUS)
   {
      p_ptr->update &= ~(PU_BONUS);
      calc_bonuses();
   }

   if (p_ptr->update & PU_TORCH)
   {
      p_ptr->update &= ~(PU_TORCH);
      calc_torch();
   }

   if (p_ptr->update & PU_HP)
   {
      p_ptr->update &= ~(PU_HP);
      calc_hitpoints();
   }

   if (p_ptr->update & PU_MANA)
   {
      p_ptr->update &= ~(PU_MANA);
      calc_mana();
   }

   /* Character is not ready yet, no screen updates */
   if (!character_generated) return;

   /* Character is in "icky" mode, no screen updates */
   if (character_icky) return;

   if (p_ptr->update & PU_UN_VIEW)
   {
      p_ptr->update &= ~(PU_UN_VIEW);
      forget_view();
   }

   if (p_ptr->update & PU_VIEW)
   {
      p_ptr->update &= ~(PU_VIEW);
      update_view();
   }

   if (p_ptr->update & PU_FLOW)
   {
      p_ptr->update &= ~(PU_FLOW);
      update_flow();
   }

   if (p_ptr->update & PU_DISTANCE)
   {
      p_ptr->update &= ~(PU_DISTANCE);
      p_ptr->update &= ~(PU_MONSTERS);
      update_monsters(TRUE);
   }

   if (p_ptr->update & PU_MONSTERS)
   {
      p_ptr->update &= ~(PU_MONSTERS);
      update_monsters(FALSE);
   }

   if (p_ptr->update & PU_PANEL)
   {
      p_ptr->update &= ~(PU_PANEL);
      verify_panel();
   }
}


/*
 * Handle "p_ptr->redraw1" and "p_ptr->redraw2"
 */
void redraw_stuff(void)
{
   /* Redraw stuff */
   if ((!p_ptr->redraw1) && (!p_ptr->redraw2)) return;

   /* Character is not ready yet, no screen updates */
   if (!character_generated) return;

   /* Character is in "icky" mode, no screen updates */
   if (character_icky) return;

   /* Hack -- clear the screen */
   if ( (p_ptr->redraw1 & PR1_WIPE) ||
        (p_ptr->redraw2 & PR2_WIPE) )
   {
      p_ptr->redraw1 &= ~PR1_WIPE;
      p_ptr->redraw2 &= ~PR2_WIPE;
      msg_print(NULL);
      clear_screen();
   }

   if (p_ptr->redraw1 & PR1_MAP)
   {
      p_ptr->redraw1 &= ~(PR1_MAP);
      prt_map();
   }

   if (p_ptr->redraw2 & (PR2_EQUIPPY))
   {
      p_ptr->redraw2 &= ~(PR2_EQUIPPY);
      print_equippy(); /* To draw / delete equippy chars */
   }


   if (p_ptr->redraw1 & PR1_BASIC)
   {
      p_ptr->redraw1 &= ~(PR1_BASIC);
/* jk - PR1_position added */
      p_ptr->redraw1 &= ~(PR1_MISC | PR1_TITLE | PR1_STATS | PR1_POSITION);
      p_ptr->redraw1 &= ~(PR1_LEV | PR1_EXP | PR1_GOLD);
      p_ptr->redraw1 &= ~(PR1_ARMOR | PR1_HP | PR1_MANA);
      p_ptr->redraw1 &= ~(PR1_DEPTH | PR1_HEALTH);
      prt_frame_basic();
   }

   if (p_ptr->redraw1 & PR1_MISC)
   {
      p_ptr->redraw1 &= ~(PR1_MISC);
      prt_field(rp_ptr->title, ROW_RACE, COL_RACE);
      prt_field(cp_ptr->title, ROW_CLASS, COL_CLASS);
   }

   if (p_ptr->redraw1 & PR1_TITLE)
   {
      p_ptr->redraw1 &= ~(PR1_TITLE);
      prt_title();
   }

   if (p_ptr->redraw1 & PR1_LEV)
   {
      p_ptr->redraw1 &= ~(PR1_LEV);
      prt_level();
   }

   if (p_ptr->redraw1 & PR1_EXP)
   {
      p_ptr->redraw1 &= ~(PR1_EXP);
      prt_exp();
   }

   if (p_ptr->redraw1 & PR1_STATS)
   {
      p_ptr->redraw1 &= ~(PR1_STATS);
      prt_stat(A_STR);
      prt_stat(A_INT);
      prt_stat(A_WIS);
      prt_stat(A_DEX);
      prt_stat(A_CON);
      prt_stat(A_CHR);
   }

   if (p_ptr->redraw1 & PR1_ARMOR)
   {
      p_ptr->redraw1 &= ~(PR1_ARMOR);
      prt_ac();
   }

   if (p_ptr->redraw1 & PR1_HP)
   {
      p_ptr->redraw1 &= ~(PR1_HP);
      prt_hp();
   }

   if (p_ptr->redraw1 & PR1_MANA)
   {
      p_ptr->redraw1 &= ~(PR1_MANA);
      prt_sp();
   }

   if (p_ptr->redraw1 & PR1_GOLD)
   {
      p_ptr->redraw1 &= ~(PR1_GOLD);
      prt_gold();
   }

   if (p_ptr->redraw1 & PR1_DEPTH)
   {
      p_ptr->redraw1 &= ~(PR1_DEPTH);
      prt_depth();
   }

   if (p_ptr->redraw1 & PR1_HEALTH)
   {
      p_ptr->redraw1 &= ~(PR1_HEALTH);
      if (show_health_bar) health_redraw();
   }

   if (p_ptr->redraw1 & PR1_EXTRA)
   {
      p_ptr->redraw1 &= ~(PR1_EXTRA);
      p_ptr->redraw1 &= ~(PR1_CUT | PR1_STUN);
      p_ptr->redraw1 &= ~(PR1_HUNGER);
      p_ptr->redraw1 &= ~(PR1_SLIDING);
      p_ptr->redraw1 &= ~(PR1_BLIND | PR1_CONFUSED);
      p_ptr->redraw1 &= ~(PR1_AFRAID | PR1_POISONED);
      p_ptr->redraw1 &= ~(PR1_STATE | PR1_SPEED);
      p_ptr->redraw2 &= ~(PR2_COLD | PR2_FIRE | PR2_ACID | PR2_ELEC);
      p_ptr->redraw2 &= ~(PR2_REFLECT);
      prt_frame_extra();
   }

   if (p_ptr->redraw1 & PR1_CUT)
   {
      p_ptr->redraw1 &= ~(PR1_CUT);
      prt_cut();
   }

   if (p_ptr->redraw1 & PR1_STUN)
   {
      p_ptr->redraw1 &= ~(PR1_STUN);
      prt_stun();
   }

   if (p_ptr->redraw1 & PR1_HUNGER)
   {
      p_ptr->redraw1 &= ~(PR1_HUNGER);
      prt_hunger();
   }

   if (p_ptr->redraw1 & PR1_BLIND)
   {
      p_ptr->redraw1 &= ~(PR1_BLIND);
      prt_blind();
   }
/* jk */
   if (p_ptr->redraw1 & PR1_SLIDING)
   {
      p_ptr->redraw1 &= ~PR1_SLIDING;
      prt_sliding();
   }
/* jk */
   if (p_ptr->redraw1 & PR1_THROAT)
   {
      p_ptr->redraw1 &= ~PR1_THROAT;
      prt_throat();
   }
   if (p_ptr->redraw2 & PR2_FIRE)
   {
      p_ptr->redraw2 &= ~PR2_FIRE;
      prt_fire();
   }

   if (p_ptr->redraw2 & PR2_COLD)
   {
      p_ptr->redraw2 &= ~PR2_COLD;
      prt_cold();
   }

   if (p_ptr->redraw2 & PR2_ELEC)
   {
      p_ptr->redraw2 &= ~PR2_ELEC;
      prt_elec();
   }

   if (p_ptr->redraw2 & PR2_ACID)
   {
      p_ptr->redraw2 &= ~PR2_ACID;
      prt_acid();
   }

   if (p_ptr->redraw2 & PR2_LIFT)
   {
      p_ptr->redraw2 &= ~PR2_LIFT;
      prt_lift();
   }

   if (p_ptr->redraw2 & PR2_READ)
   {
      p_ptr->redraw2 &= ~PR2_READ;
      prt_reading();
   }

   if (p_ptr->redraw2 & PR2_REFLECT)
   {
      p_ptr->redraw2 &= ~PR2_REFLECT;
      prt_reflect();
   }


/* jk */
   if (p_ptr->redraw1 & PR1_POSITION)
   {
      p_ptr->redraw1 &= ~PR1_POSITION;
      prt_position();
   }

   if (p_ptr->redraw1 & PR1_CONFUSED)
   {
      p_ptr->redraw1 &= ~(PR1_CONFUSED);
      prt_confused();
   }

   if (p_ptr->redraw1 & PR1_AFRAID)
   {
      p_ptr->redraw1 &= ~(PR1_AFRAID);
      prt_afraid();
   }

   if (p_ptr->redraw1 & PR1_POISONED)
   {
      p_ptr->redraw1 &= ~(PR1_POISONED);
      prt_poisoned();
   }

   if (p_ptr->redraw1 & PR1_STATE)
   {
      p_ptr->redraw1 &= ~(PR1_STATE);
      prt_state();
   }

   if (p_ptr->redraw1 & PR1_SPEED)
   {
      p_ptr->redraw1 &= ~(PR1_SPEED);
      prt_speed();
   }
}

/*
 * Handle "p_ptr->window"
 */
void window_stuff(void)
{
   int j;

   u32b mask = 0L;

   /* Nothing to do */
   if (!p_ptr->window) return;

   /* Scan windows */
   for (j = 0; j < 8; j++)
   {
      /* Save usable flags */
      if (angband_term[j])
      {
         /* Build the mask */
         mask |= op_ptr->window_flag[j];
      }
   }

   /* Apply usable flags */
   p_ptr->window &= (mask);

   /* Nothing to do */
   if (!p_ptr->window) return;

   /* Display last kills */
   if (p_ptr->window & (PW_LASTKILL))
   {
      p_ptr->window &= ~(PW_LASTKILL);
      fix_last_kills();
   }

   /* Display inventory */
   if (p_ptr->window & (PW_INVEN))
   {
      p_ptr->window &= ~(PW_INVEN);
      fix_inven();
   }

   /* Display equipment */
   if (p_ptr->window & (PW_EQUIP))
   {
      p_ptr->window &= ~(PW_EQUIP);
      fix_equip();
   }

   /* Display player (mode 0) */
   if (p_ptr->window & (PW_PLAYER_0))
   {
      p_ptr->window &= ~(PW_PLAYER_0);
      fix_player_0();
   }

   /* Display player (mode 1) */
   if (p_ptr->window & (PW_PLAYER_1))
   {
      p_ptr->window &= ~(PW_PLAYER_1);
      fix_player_1();
   }

   /* Display overhead view */
   if (p_ptr->window & (PW_MESSAGE))
   {
      p_ptr->window &= ~(PW_MESSAGE);
      fix_message();
   }

   /* Display overhead view */
   if (p_ptr->window & (PW_OVERHEAD))
   {
      p_ptr->window &= ~(PW_OVERHEAD);
      fix_overhead();
   }

   /* Display monster recall */
   if (p_ptr->window & (PW_MONSTER))
   {
      p_ptr->window &= ~(PW_MONSTER);
      fix_monster();
   }

   /* Display object recall */
   if (p_ptr->window & (PW_OBJECT))
   {
      p_ptr->window &= ~(PW_OBJECT);
      /* this doesn't work at the moment! */
   }
}


/*
 * Handle "p_ptr->update" and "p_ptr->redraw" and "p_ptr->window"
 */
void handle_stuff(void)
{
   /* Update stuff */
   if (p_ptr->update) update_stuff();

   /* Redraw stuff */
   if ((p_ptr->redraw1) || (p_ptr->redraw2)) redraw_stuff();

   /* Window stuff */
   if (p_ptr->window) window_stuff();
}

