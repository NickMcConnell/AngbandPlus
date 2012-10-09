/* File: birth.c */

/* Purpose: create a player character */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

/*
 * A buffer to store the history in while it's being built
 */
char      historybuf[420];

/*
 * Forward declare
 */
typedef struct hist_type hist_type;

/*
 * Player background information
 */
struct hist_type
{
   cptr info;                      /* Textual History                  */

   byte roll;                      /* Frequency of this entry          */
   byte chart;                     /* Chart index                      */
   byte next;                      /* Next chart index                 */
   byte bonus;                     /* Social Class Bonus + 50          */
};

/*
 * Background information (see below)
 *
 * Chart progression by race:
 *   Human/Dunadan -->  1 -->  2 -->  3 --> 50 --> 51 --> 52 --> 53
 *   Half-Elf      -->  4 -->  1 -->  2 -->  3 --> 50 --> 51 --> 52 --> 53
 *   Elf/High-Elf  -->  7 -->  8 -->  9 --> 54 --> 55 --> 56
 *   Hobbit        --> 10 --> 11 -->  3 --> 50 --> 51 --> 52 --> 53
 *   Gnome         --> 13 --> 14 -->  3 --> 50 --> 51 --> 52 --> 53
 *   Dwarf         --> 16 --> 17 --> 18 --> 57 --> 58 --> 59 --> 60 --> 61
 *   Half-Orc      --> 19 --> 20 -->  2 -->  3 --> 50 --> 51 --> 52 --> 53
 *   Half-Troll    --> 22 --> 23 --> 62 --> 63 --> 64 --> 65 --> 66
 *   Druedain      -->  5 -->  6 --> 12 --> 15 --> 21 --> 24 --> 25
 *
 * XXX XXX XXX This table *must* be correct or drastic errors may occur!
 */
 /*  string, roll, chart, next, bonus */
static hist_type bg[] =
{
   {"You are the illegitimate and unacknowledged child ",   10, 1, 2, 25},
   {"You are the illegitimate but acknowledged child ",     20, 1, 2, 35},
   {"You are one of several children ",                     80, 1, 2, 45},
   {"You are firstborn of several children ",               90, 1, 2, 48},
   {"You are the only child ",                             100, 1, 2, 50},

   {"of a Serf.  ",                                         30, 2, 3, 65},
   {"of a Yeoman.  ",                                       40, 2, 3, 80},
   {"of a Craftsman.  ",                                    60, 2, 3, 85},
   {"of a Townsman.  ",                                     70, 2, 3, 90},
   {"of a Guildsman.  ",                                    80, 2, 3,105},
   {"of a Bard.  ",                                         85, 2, 3,110},
   {"of a Landed Knight.  ",                                90, 2, 3,120},
   {"of a Titled Noble.  ",                                 96, 2, 3,130},
   {"of the Royal Blood Line.  ",                          100, 2, 3,140},

   {"You are not considered part of the family.  ",         15, 3,50, 10},
   {"You are the black sheep of the family.  ",             20, 3,50, 20},
   {"You are a credit to the family.  ",                    90, 3,50, 55},
   {"You are a well liked child.  ",                        95, 3,50, 60},
   {"You are an example for other family members.  ",      100, 3,50, 80},

   {"Your mother was of the Laiquendi.  ",                   5, 4, 1, 40},
   {"Your father was of the Laiquendi.  ",                  15, 4, 1, 45},
   {"Your mother was of the Nandor.  ",                     20, 4, 1, 40},
   {"Your father was of the Nandor.  ",                     30, 4, 1, 45},
   {"Your mother was of the Teleri.  ",                     40, 4, 1, 50},
   {"Your father was of the Teleri.  ",                     50, 4, 1, 55},
   {"Your mother was of the Sindar.  ",                     60, 4, 1, 50},
   {"Your father was of the Sindar.  ",                     70, 4, 1, 55},
   {"Your mother was of the Noldor.  ",                     75, 4, 1, 55},
   {"Your father was of the Noldor.  ",                     85, 4, 1, 60},
   {"Your mother was of the Vanyar.  ",                     94, 4, 1, 65},
   {"Your father was of the Vanyar.  ",                    100, 4, 1, 70},

   {"You are the illegitimate and unacknowledged child ",   10, 5, 6, 25},
   {"You are the illegitimate but acknowledged child ",     20, 5, 6, 35},
   {"You are one of several children ",                     80, 5, 6, 45},
   {"You are firstborn of several children ",               90, 5, 6, 48},
   {"You are the only child ",                             100, 5, 6, 50},

   {"of a Druedain ",                                      100, 6,12, 50},

   {"You are one of several children ",                     60, 7, 8, 50},
   {"You are firstborn of several children ",               80, 7, 8, 53},
   {"You are the only child ",                             100, 7, 8, 55},

   {"of a Laiquendi ",                                      20, 8, 9, 20},
   {"of a Nandor ",                                         30, 8, 9, 30},
   {"of a Teleri ",                                         40, 8, 9, 40},
   {"of a Sindar ",                                         50, 8, 9, 50},
   {"of a Noldor ",                                         70, 8, 9, 55},
   {"of a Vanyar ",                                        100, 8, 9, 60},

   {"Hunter.  ",                                            40, 9,54, 80},
   {"Ranger.  ",                                            40, 9,54, 80},
   {"Archer.  ",                                            70, 9,54, 90},
   {"Warrior.  ",                                           85, 9,54,110},
   {"Mage.  ",                                              90, 9,54,125},
   {"Scholar.  ",                                           93, 9,54,132},
   {"Prince.  ",                                            96, 9,54,140},
   {"King.  ",                                             100, 9,54,145},

   {"You are one of several children of a Hobbit ",         85,10,11, 45},
   {"You are the only child of a Hobbit ",                 100,10,11, 55},

   {"Bum.  ",                                               20,11, 3, 55},
   {"Tavern Owner.  ",                                      30,11, 3, 80},
   {"Miller.  ",                                            40,11, 3, 90},
   {"Home Owner.  ",                                        50,11, 3,100},
   {"Burglar.  ",                                           80,11, 3,110},
   {"Hotel Owner.  ",                                       95,11, 3,115},
   {"Plantation owner.  ",                                  99,11, 3,125},
   {"Town Mayor.  ",                                       100,11, 3,140},

   {"Doorwarden.  ",                                        20,12,15, 35},
   {"House-Guard.  ",                                       40,12,15, 45},
   {"Orc-Hunter.  ",                                        60,12,15, 55},
   {"Tracker.  ",                                           80,12,15, 65},
   {"Tribe Leader.  ",                                      90,12,15, 75},
   {"Diviner.  ",                                          100,12,15, 85},

   {"You are one of several children of a Gnome ",          85,13,14, 45},
   {"You are the only child of a Gnome ",                  100,13,14, 55},

   {"Beggar.  ",                                            20,14, 3, 55},
   {"Braggart.  ",                                          50,14, 3, 70},
   {"Prankster.  ",                                         60,14, 3, 85},
   {"Warrior.  ",                                           70,14, 3,100},
   {"Veteran.  ",                                           80,14, 3,110},
   {"Squad Leader.  ",                                      90,14, 3,120},
   {"Mage.  ",                                              95,14, 3,130},
   {"Warchief.  ",                                         100,14, 3,140},

   {"You have very dark brown eyes, ",                      20,15,21, 50},
   {"You have very dark blue eyes, ",                       60,15,21, 60},
   {"You have very dark green eyes, ",                      80,15,21, 70},
   {"You have completely black eyes, ",                    100,15,21, 70},

   {"You are one of two children of a Dwarven ",            25,16,17, 40},
   {"You are the only child of a Dwarven ",                100,16,17, 50},

   {"Thief.  ",                                             10,17,18, 60},
   {"Prison Guard.  ",                                      25,17,18, 75},
   {"Miner.  ",                                             75,17,18, 90},
   {"Smith.  ",                                             80,17,18,100},
   {"Warrior.  ",                                           88,17,18,110},
   {"Priest.  ",                                            95,17,18,130},
   {"King.  ",                                             100,17,18,150},

   {"You are not considered part of the family.  ",         15,18,57, 10},
   {"You are the black sheep of the family.  ",             35,18,57, 30},
   {"You are a credit to the family.  ",                    75,18,57, 50},
   {"You are a well liked child.  ",                        80,18,57, 60},
   {"You are an example for other family members.  ",      100,18,57, 80},

   {"Your mother was an Orc, but it is unacknowledged.  ",  25,19,20, 25},
   {"Your father was an Orc, but it is unacknowledged.  ", 100,19,20, 25},

   {"You are the adopted child ",                          100,20, 2, 50},

   {"short, stumpy legs ",                                  20,21,24, 40},
   {"short, thick legs ",                                   60,21,24, 50},
   {"short, powerful legs ",                               100,21,24, 60},

   {"Your mother was a Stone-Troll ",                       10,22,23, 10},
   {"Your father was a Stone-Troll ",                       20,22,23, 15},
   {"Your mother was a Cave-Troll ",                        30,22,23, 20},
   {"Your father was a Cave-Troll ",                        60,22,23, 25},
   {"Your mother was a Hill-Troll ",                        75,22,23, 30},
   {"Your father was a Hill-Troll ",                        90,22,23, 35},
   {"Your mother was a Water-Troll ",                       95,22,23, 40},
   {"Your father was a Water-Troll ",                      100,22,23, 45},

   {"Cook.  ",                                               5,23,62, 30},
   {"Butcher.  ",                                           15,23,62, 40},
   {"Smith.  ",                                             45,23,62, 50},
   {"Warrior.  ",                                           95,23,62, 55},
   {"Shaman.  ",                                            99,23,62, 65},
   {"Clan Chief.  ",                                       100,23,62, 80},

   {"and long arms.",                                       30,24,25, 30},
   {"and long, well-muscled arms.",                         60,24,25, 60},
   {"and long arms, descending from very broad shoulders.",100,24,25, 90},

   {"You have no facial hair.",                             30,25, 0, 30},
   {"You have a very thin beard, 2 inches long.",           60,25, 0, 60},
   {"You have a thin beard, a foot long.",                  90,25, 0, 80},
   {"You have a sparse beard, over a foot long.",          100,25, 0, 90},

   {"You have dark brown eyes, ",                           20,50,51, 50},
   {"You have brown eyes, ",                                60,50,51, 50},
   {"You have hazel eyes, ",                                70,50,51, 50},
   {"You have green eyes, ",                                80,50,51, 50},
   {"You have blue eyes, ",                                 90,50,51, 50},
   {"You have blue-gray eyes, ",                           100,50,51, 50},

   {"thin ",                                                50,51,52, 30},
   {"straight ",                                            60,51,52, 50},
   {"wavy ",                                                70,51,52, 50},
   {"curly ",                                               80,51,52, 50},
   {"shiny ",                                              100,51,52, 60},

   {"black hair, ",                                         30,52,53, 50},
   {"brown hair, ",                                         70,52,53, 50},
   {"auburn hair, ",                                        80,52,53, 50},
   {"red hair, ",                                           90,52,53, 50},
   {"blond hair, ",                                        100,52,53, 50},

   {"and a very dark complexion.",                          10,53, 0, 50},
   {"and a dark complexion.",                               30,53, 0, 50},
   {"and an average complexion.",                           80,53, 0, 50},
   {"and a fair complexion.",                               90,53, 0, 50},
   {"and a very fair complexion.",                         100,53, 0, 50},

   {"You have light grey eyes, ",                           85,54,55, 50},
   {"You have light blue eyes, ",                           95,54,55, 50},
   {"You have light green eyes, ",                         100,54,55, 50},

   {"straight ",                                            75,55,56, 50},
   {"wavy ",                                               100,55,56, 50},

   {"black hair, and a fair complexion.",                   75,56, 0, 50},
   {"brown hair, and a fair complexion.",                   85,56, 0, 50},
   {"blond hair, and a fair complexion.",                   95,56, 0, 50},
   {"silver hair, and a fair complexion.",                 100,56, 0, 50},

   {"You have yellow eyes, ",                               40,57,58, 40},
   {"You have dark brown eyes, ",                           99,57,58, 50},
   {"You have glowing red eyes, ",                         100,57,58, 60},

   {"straight ",                                            90,58,59, 50},
   {"wavy ",                                               100,58,59, 50},

   {"black hair, ",                                         75,59,60, 50},
   {"brown hair, ",                                        100,59,60, 50},

   {"an embarrassing small beard, ",                        10,60,61, 40},
   {"a one foot beard, ",                                   25,60,61, 50},
   {"a two foot beard, ",                                   60,60,61, 51},
   {"a three foot beard, ",                                 90,60,61, 53},
   {"a four foot beard, ",                                 100,60,61, 55},

   {"and a rather light complexion.",                       30,61, 0, 50},
   {"and a dark complexion.",                              100,61, 0, 50},

   {"You have slime green eyes, ",                          60,62,63, 50},
   {"You have puke yellow eyes, ",                          85,62,63, 50},
   {"You have blue-bloodshot eyes, ",                       99,62,63, 50},
   {"You have glowing red eyes, ",                         100,62,63, 55},

   {"dirty ",                                               33,63,64, 50},
   {"tangled ",                                             44,63,64, 50},
   {"sticky ",                                              55,63,64, 50},
   {"mangy ",                                               66,63,64, 50},
   {"oily ",                                               100,63,64, 50},

   {"sea-weed green hair, ",                                33,64,65, 50},
   {"dung-brown hair, ",                                    44,64,65, 50},
   {"mold grey hair, ",                                     55,64,65, 50},
   {"bright red hair, ",                                    66,64,65, 50},
   {"dark purple hair, ",                                  100,64,65, 50},

   {"and green ",                                           25,65,66, 50},
   {"and blue ",                                            50,65,66, 50},
   {"and white ",                                           75,65,66, 50},
   {"and black ",                                          100,65,66, 50},

   {"ulcerous skin.",                                       33,66, 0, 50},
   {"splotchy skin.",                                       44,66, 0, 50},
   {"flaky skin.",                                          55,66, 0, 50},
   {"scabby skin.",                                         66,66, 0, 50},
   {"leprous skin.",                                       100,66, 0, 50}
};

/*
 * this returns the experience-boost that goes with a stat-boost
 */
static s16b stat_bonus_experience_boost(s16b statbonus)
{
   s16b bonus[] = { 0, 1, 2, 4, 9, 16, 25};

   if (statbonus < -6) return -36;
   if (statbonus > 6) return 36;

   if (statbonus < 0) 
   {
      return -bonus[-statbonus];
   }
   else
   {
      return bonus[statbonus];
   }
}
   
static void get_1_teacher(s16b num, bool all_random)
{
   s16b i, j, choose_race[5], choose_class[5], choose_exp[5];
   s16b race, class, tmp = 0, pref, stat_pref = 0;
   bool ok;

   char buf[80], c;

   clear_from(14);

   for (i=0; i < 5; i++)
   {
      ok = FALSE;
      while (!ok)
      {
         choose_race[i] = rand_int(MAX_RACES);
         choose_class[i] = rand_int(MAX_CLASS);
         ok = TRUE;
         for (j=0; j < i; j++)
         {
            if ((choose_race[j]==choose_race[i]) &&
                (choose_class[j]==choose_class[j]))
            {
               ok = FALSE;
               break;
            }
         }
         /* The first 'Advanced' teacher is the player's own */
         /* race & class                                     */
         if ((num==2) && (i==0))
         {
            choose_race[i] = p_ptr->prace;
            choose_class[i] = p_ptr->pclass;
            ok = TRUE;
         }
      }

      /* try to guess how usefull such a teacher would be              */
      /* ranging from usefullnes  0 - tmp becomes  5 = 0.5x experience */
      /*           to            10               15   1.5x            */
dlog(DEBUGBIRTH,"birth.c: get_1_teacher: player = race %s class %s, testing teacher race %s class %s\n",
                 rp_ptr->title, cp_ptr->title,
                 race_info[choose_race[i]].title, class_info[choose_class[i]].title);
dlog(DEBUGBIRTH,"birth.c: get_1_teacher: class_pref %d, race_pref %d, total pref %d\n",
                teach_class_pref[p_ptr->pclass][choose_class[i]],
                teach_race_pref[p_ptr->prace][choose_class[i]],
                teach_class_pref[p_ptr->pclass][choose_class[i]] *
                   teach_race_pref[p_ptr->prace][choose_class[i]]);

      pref = teach_class_pref[p_ptr->pclass][choose_class[i]];
      pref = pref * teach_race_pref[p_ptr->prace][choose_class[i]];


      stat_pref = 0;
      /* determine how 'good' the stat bonuses from this char are */
      for (j=0; j < 4; j++)
      {
         s16b this_stat = teach_stat_pref[p_ptr->pclass][j];
         s16b raceclassbonus;

         if (this_stat == 111) continue;
dlog(DEBUGBIRTH,"birth.c: get_1_teacher: stat_pref[%d]: this_stat %d=%s now %d adding race %d class %d\n",
                j, this_stat, stat_names[this_stat],
                stat_pref, race_info[p_ptr->prace].r_adj[this_stat],
                class_info[p_ptr->pclass].c_adj[this_stat]);

         raceclassbonus = race_info[p_ptr->prace].r_adj[this_stat];   /* -6 to 5 */
         raceclassbonus += class_info[p_ptr->pclass].c_adj[this_stat]; /* -5 to 5 */
         stat_pref += stat_bonus_experience_boost(raceclassbonus);
      }
      /* max 4 times 2 times -6 to +5 means -48 to +40 */
      pref += (stat_pref + 40) / 4;

      /* each pref can range from 0-10, so this can range from   */
      /* 0 to 100. That's too much spread, since                 */
      /* every teacher only adds 1/3 of your character           */
      tmp = (pref * 10) / 50;

      if (tmp < 1) tmp = 1;

      choose_exp[i] = tmp;
dlog(DEBUGBIRTH,"birth.c: get_1_teacher: stat_pref %d, adding %d to pref, pref ends at %d choose_exp %d.%d\n",
                stat_pref, (stat_pref + 40) / 4, pref, choose_exp[i]/10, choose_exp[i]%10);

      if (!all_random)
      {
         if ((choose_exp[i]%10)==0)
         {
            sprintf(buf, "%dx", choose_exp[i]/10);
         }
         else
         {
            sprintf(buf, "%d.%dx", choose_exp[i] / 10, choose_exp[i] % 10);
         }
dlog(DEBUGBIRTH,"birth.c: get_1_teacher: teacher %d exp factor %d displayed as %s\n",
                i, choose_exp[i], buf);
         c_put_str(TERM_WHITE, format("%c - A %s %s (%s exp).",
                                      65+i, race_info[choose_race[i]].title,
                                      class_info[choose_class[i]].title,
                                      buf),
                   1, i+15);
      }
   }

   if (num==0)
   {
      sprintf(buf, "Choose the teacher in your youth:");
   }
   else if (num==1)
   {
      sprintf(buf, "Choose the teacher for your study as a pupil:");
   }
   else if (num==2)
   {
      sprintf(buf, "Choose the teacher for your advanced studies:");
   }

   if (!all_random)
   {
      /* Choose */
      while (1)
      {

         put_str(buf, 1, 23);
         c = inkey();
         c = FORCEUPPER(c);
         if (c == 'Q') quit(NULL);
         if ((c>='A') && (c<='E'))
         {
            i = (s16b)(c-'A');
            break;
         }
         if (c == '?') do_cmd_help("help.hlp");
         else bell("Illegal choice!");
      }
   }
   else
   {
      /* now try to choose good teachers :-) */
      s16b best[5];
      s16b max = -1, maxi=-1;
      for (i=0; i < 5; i++)
      {
         best[i]=teach_class_pref[p_ptr->pclass][choose_class[i]]+
                 teach_race_pref[p_ptr->prace][choose_class[i]];
         if ((best[i]>max) && (randint(2)==1))
         {
            max=best[i];
            maxi=i;
         }

      }
      i=maxi;
      /* 5 times rolling randint(2)!=1 is possible...                   */
      /* once in 3 teachers, meaning once per char, choose a random one */
      if ( (i==-1) || (randint(3)==1) ) i = rand_int(5);
   }

dlog(DEBUGBIRTH,"birth.c: get_1_teacher: teacher %d chosen, race %d (%s), class %d (%s), choose_exp %d.%d\n",
                i, choose_race[i], race_info[choose_race[i]].title,
                choose_class[i], class_info[choose_class[i]].title,
                choose_exp[i]/10, choose_exp[i]%10);

   race = choose_race[i];
   class = choose_class[i];
   for (j=0; j < 6; j++)
   {
      p_ptr->teach_stat[j] += race_info[race].r_adj[j] +
                              class_info[class].c_adj[j];
   }

   p_ptr->teach_dis += (race_info[race].r_dis +
                        class_info[class].c_dis) / 3;
   p_ptr->teach_dev += (race_info[race].r_dev +
                        class_info[class].c_dev) / 3;
   p_ptr->teach_sav += (race_info[race].r_sav +
                        class_info[class].c_sav) / 3;
   p_ptr->teach_stl += (race_info[race].r_stl +
                        class_info[class].c_stl) / 3;

   p_ptr->teach_srh += (race_info[race].r_srh +
                        class_info[class].c_srh) / 3;
   p_ptr->teach_pcp += (race_info[race].r_pcp +
                        class_info[class].c_pcp) / 3;
   p_ptr->teach_thn += (race_info[race].r_thn +
                        class_info[class].c_thn) / 3;
   p_ptr->teach_thb += (race_info[race].r_thb +
                        class_info[class].c_thb) / 3;

   /* add in the bonus/malus in experience factors */
dlog(DEBUGBIRTH, "birth.c: get_1_teacher: teach_exp from %d to %d\n",
                p_ptr->teach_exp , ( p_ptr->teach_exp * choose_exp[i]) / 10);

   p_ptr->teach_exp = (p_ptr->teach_exp * choose_exp[i]) / 10;

   if (num==0)
   {
      sprintf(teacher[num], "In your youth you were taught by a %s %s.",
              race_info[race].title, class_info[class].title);
   }
   else if (num==1)
   {
      sprintf(teacher[num], "You were a pupil with an experienced %s %s.",
              race_info[race].title, class_info[class].title);
   }
   else if (num==2)
   {
      sprintf(teacher[num], "You completed your studies with a %s master %s.",
              race_info[race].title, class_info[class].title);
   }
}

static void choose_teachers(bool all_random)
{
   s16b i;

   for (i=0; i < 6; i++)
   {
      p_ptr->teach_stat[i] = 0;
   }
   p_ptr->teach_dis = 0;
   p_ptr->teach_dev = 0;
   p_ptr->teach_sav = 0;
   p_ptr->teach_stl = 0;
   p_ptr->teach_srh = 0;
   p_ptr->teach_pcp = 0;
   p_ptr->teach_thn = 0;
   p_ptr->teach_thb = 0;

   /* start with normal experience compared to unteached characters 10 = 1.0x */
   p_ptr->teach_exp = 5;

dlog(DEBUGBIRTH,"birth.c: choose_teachers: player exp factor starts at %d\n", p_ptr->teach_exp);

   for (i=0; i < 3; i++)
   {
dlog(DEBUGBIRTH, "birth.c: choose_teachers: getting teacher %d of 3\n", i);
      get_1_teacher(i, all_random);
   }
dlog(DEBUGBIRTH,"birth.c: choose_teachers: player exp factor now %d\n", p_ptr->teach_exp);

   /* a character gets 2/3 of the stat bonuses from his teachers */
   for (i=0; i < 6; i++)
   {
      p_ptr->teach_stat[i] = (2*p_ptr->teach_stat[i])/3;
   }
dlog(DEBUGBIRTH,"birth.c: choose_teachers: player exp factor ends at %d\n", p_ptr->teach_exp);
}

/*
 * Roll for a characters stats
 *
 * For efficiency, we include a chunk of "calc_bonuses()".
 */
static void get_stats(void)
{
   s16b         i, j;
   s16b         bonus;
   s16b         dice[18];

   /* Roll and verify some stats */
   while (TRUE)
   {
      /* Roll some dice */
      for (j = i = 0; i < 18; i++)
      {
         /* Roll the dice */
         dice[i] = randint(3 + i % 3);

         /* Collect the maximum */
         j += dice[i];
      }

      /* Verify totals */
      if ((j > 42) && (j < 54)) break;
   }

   /* Acquire the stats */
   for (i = 0; i < 6; i++)
   {
      /* Extract 5 + 1d3 + 1d4 + 1d5 */
      /* min 7 max 17 */
      j = 5 + dice[3*i] + dice[3*i+1] + dice[3*i+2];

      /* Save that value */
      if (!p_ptr->teach_birth)
      {
         p_ptr->stat_max[i] = j;
         p_ptr->stat_cur[i] = j;
         bonus = rp_ptr->r_adj[i] + cp_ptr->c_adj[i];
         p_ptr->stat_use[i] = modify_stat_value(p_ptr->stat_max[i], bonus);
      }
      else
      {
         p_ptr->stat_max[i] = j;
         p_ptr->stat_cur[i] = j;
         bonus = (rp_ptr->r_adj[i] + cp_ptr->c_adj[i])/3;
         bonus += p_ptr->teach_stat[i];
dlog(DEBUGBIRTH,"birth.c: get_stats: stat %d cur %d max %d bonus %d\n",
                i, j, j, p_ptr->teach_stat[i]);
         p_ptr->stat_use[i] = modify_stat_value(p_ptr->stat_max[i], bonus);
dlog(DEBUGBIRTH,"birth.c: get_stats: stat %d cur %d max %d use %d teach_stat %d\n",
                i, p_ptr->stat_cur[i], p_ptr->stat_max[i], p_ptr->stat_use[i], p_ptr->teach_stat[i]);
      }

      /* Start fully healed */
      p_ptr->stat_max[i] = p_ptr->stat_cur[i];
      p_ptr->stat_los[i] = 0;
      p_ptr->stat_cnt[i] = 0;
dlog(DEBUGBIRTH,"birth.c: get_stats: final stat %d cur %d max %d use %d\n",
                i, p_ptr->stat_cur[i], p_ptr->stat_max[i], p_ptr->stat_use[i]);
   }
}

/*
 * Roll for some info that the auto-roller ignores
 */
static void get_extra(void)
{
   s16b         i, j, min_value, max_value;

   /* Level one (never zero!) */
   p_ptr->lev = 1;

   /* Experience factor */
   p_ptr->expfact = rp_ptr->r_exp + cp_ptr->c_exp;

   if (p_ptr->teach_birth)
   {
      s16b tmp;
      tmp = (p_ptr->expfact * p_ptr->teach_exp) / 10;
dlog(DEBUGBIRTH,"birth.c: get_extra: expfact from %d to %d teach_exp %d\n",
                p_ptr->expfact, tmp, p_ptr->teach_exp);
      /* never need (much less) than the base experience for this race/class combo */
      if (p_ptr->teach_exp < 5) tmp = p_ptr->expfact;
      p_ptr->expfact = tmp;
   }

   /* Hitdice */
   p_ptr->hitdie = rp_ptr->r_mhp + cp_ptr->c_mhp;

   /* Assume base hitpoints (fully healed) */
   p_ptr->chp = p_ptr->mhp = p_ptr->hitdie;

   /* Minimum hitpoints at highest level */
   min_value = (PY_MAX_LEVEL * (p_ptr->hitdie - 1) * 3) / 8;
   min_value += PY_MAX_LEVEL;

   /* Maximum hitpoints at highest level */
   max_value = (PY_MAX_LEVEL * (p_ptr->hitdie - 1) * 5) / 8;
   max_value += PY_MAX_LEVEL;

   /* Pre-calculate level 1 hitdice */
   player_hp[0] = p_ptr->hitdie;

   /* Roll out the hitpoints */
   while (TRUE)
   {
      /* Roll the hitpoint values */
      for (i = 1; i < PY_MAX_LEVEL; i++)
      {
         j = randint(p_ptr->hitdie);
         player_hp[i] = player_hp[i-1] + j;
      }

      /* XXX Could also require acceptable "mid-level" hitpoints */

      /* Require "valid" hitpoints at highest level */
      if (player_hp[PY_MAX_LEVEL-1] < min_value) continue;
      if (player_hp[PY_MAX_LEVEL-1] > max_value) continue;

      /* Acceptable */
      break;
   }
/* jk */
   p_ptr->tactic = (rp_ptr->tactic + cp_ptr->tactic) / 2;
   p_ptr->movement = 4;
}

/*
 * this function builds the characters history from historybuf
 * a global variable
 */
static void create_history(void)
{
   char     *s, *t;
   s16b     i, n;

   /* Skip leading spaces */
   for (s = historybuf; *s == ' '; s++) ;

   /* Get apparent length */
   n = strlen(s);

   /* Kill trailing spaces */
   while ((n > 0) && (s[n-1] == ' ')) s[--n] = '\0';

   /* Start at first line */
   i = 0;

   /* Collect the history */
   while (TRUE)
   {
      /* Extract remaining length */
      n = strlen(s);

      /* All done */
      if (n < 70)
      {
         /* Save one line of history */
         strcpy(history[i++], s);

         /* All done */
         break;
      }

      /* Find a reasonable break-point */
      for (n = 70; ((n > 0) && (s[n-1] != ' ')); n--) ;

      /* Save next location */
      t = s + n;

      /* Wipe trailing spaces */
      while ((n > 0) && (s[n-1] == ' ')) s[--n] = '\0';

      /* Save one line of history */
      strcpy(history[i++], s);

      /* Start next line */
      for (s = t; *s == ' '; s++) ;
   }
}

/*
 * Get the racial history, and social class, using the "history charts".
 */
static void get_history(bool boost_social)
{
   s16b      i, chart, roll, social_class;

   /* Clear the previous history strings */
   for (i = 0; i < MAX_HIST; i++) history[i][0] = '\0';

   /* Clear the history text */
   historybuf[0] = '\0';

   /* Initial social class */
   social_class = randint(4);

   /* Starting place */
   switch (p_ptr->prace)
   {
      case RACE_HUMAN:
      case RACE_DUNADAN:
         chart = 1;
         break;

      case RACE_HALF_ELF:
         chart = 4;
         break;

      case RACE_ELF:
      case RACE_HIGH_ELF:
         chart = 7;
         break;

      case RACE_HOBBIT:
         chart = 10;
         break;

      case RACE_GNOME:
         chart = 13;
         break;

      case RACE_DWARF:
         chart = 16;
         break;

      case RACE_HALF_ORC:
         chart = 19;
         break;

      case RACE_HALF_TROLL:
         chart = 22;
         break;

      case RACE_DRUEDAIN:
         chart = 5;
         break;

      default:
         chart = 0;
   }

   /* Process the history */
   while (chart)
   {
      /* Start over */
      i = 0;

      /* Roll for nobility */
      if (boost_social)
      {
         roll = 30+randint(70);
      }
      else
      {
         roll = randint(100);
      }

      /* Access the proper entry in the table */
      while ((chart != bg[i].chart) || (roll > bg[i].roll)) i++;

      /* Acquire the textual history */
      (void)strcat(historybuf, bg[i].info);

      /* Add in the social class */
      social_class += (int)(bg[i].bonus) - 50;

      /* Enter the next chart */
      chart = bg[i].next;
   }

   /* Verify social class */
   if (social_class > 100) social_class = 100;
   else if (social_class < 1) social_class = 1;

   /* Save the social class */
   p_ptr->sc = social_class;

   if (boost_social)
   {
      strcat(historybuf, " Your family is poor but well-connected.");
   }
}

/*
 * Computes character's age, height, and weight
 */
static void get_physique(void)
{
   /* Calculate the age */
   p_ptr->age = rp_ptr->b_age + randint(rp_ptr->m_age);

   /* Calculate the height/weight for males */
   if (p_ptr->psex == SEX_MALE)
   {
      p_ptr->ht = randnor(rp_ptr->m_b_ht, rp_ptr->m_m_ht);
      p_ptr->wt = randnor(rp_ptr->m_b_wt, rp_ptr->m_m_wt);
   }

   /* Calculate the height/weight for females */
   else
   {
      p_ptr->ht = randnor(rp_ptr->f_b_ht, rp_ptr->f_m_ht);
      p_ptr->wt = randnor(rp_ptr->f_b_wt, rp_ptr->f_m_wt);
   }
}

/*
 * Get the player's starting money
 */
static void get_money(bool boost_social)
{
   s16b     i, gold;

   /* Social Class determines starting gold */
   if (boost_social)
   {
      gold = (p_ptr->sc * 4) + randint(50) + 100;
   }
   else
   {
      gold = (p_ptr->sc * 6) + randint(100) + 300;
   }
dlog(DEBUGBIRTH,"birth.c: get_money: sc %d, starting at %d\n", p_ptr->sc, gold);
   /* Process the stats */
   for (i = 0; i < 6; i++)
   {
      /* Mega-Hack -- reduce gold for high stats */
      if (p_ptr->stat_cur[i] >= 18+50) gold -= 300;
      else if (p_ptr->stat_cur[i] >= 18+20) gold -= 200;
      else if (p_ptr->stat_cur[i] > 18) gold -= 150;
      else gold -= (p_ptr->stat_cur[i] - 8) * 10;
   }
dlog(DEBUGBIRTH,"birth.c: get_money: after stats %d\n", gold);

   /* Minimum 100 gold */
   if (gold < 100) gold = 100;

   /* She charmed the banker into it! -CJS- */
   /* She slept with the banker.. :) -GDH-  */
   if (p_ptr->psex == SEX_FEMALE) gold += 50;

   /* if you were poorly teached, add some bonus */
   if (p_ptr->teach_birth && (p_ptr->teach_exp < 5))
   {
      switch(p_ptr->teach_exp)
      {
         case 1: strcat(historybuf," You were born deformed, but ammassed a fortune by begging.");
                 break;
         case 2: strcat(historybuf," You were a strange child, but a rich merchant took pity on you.");
                 break;
         case 3: strcat(historybuf," You are misshapen, but made money by begging.");
                 break;
         case 4: strcat(historybuf," You were plagued by strange diseases.");
                 break;
      }
      /* 4 means 1.5x gold, 3 2x gold, 2 2.5x gold, 1 3x gold */
      gold = (gold*(7-p_ptr->teach_exp))/2;
dlog(DEBUGBIRTH,"birth.c: get_money: teach_exp %d, gold %d\n", p_ptr->teach_exp, gold);
   }
dlog(DEBUGBIRTH,"birth.c: get_money: result %d\n", gold);

   /* Save the gold */
   p_ptr->au = gold;
}

/*
 * Clear all the global "character" data
 */
static void player_wipe()
{
   s16b i;

   /* Hack -- zero the struct */
   WIPE(p_ptr, player_type);

   /* Wipe the history */
   for (i = 0; i < 5; i++)
   {
      strcpy(history[i], "");
   }
   for (i = 0; i < 3; i++)
   {
      strcpy(teacher[i], "");
   }

   /* No weight */
   p_ptr->total_weight = 0;

   /* No items */
   inven_cnt = 0;
   equip_cnt = 0;

   /* jk - start with 1h weapons */
   p_ptr->twohands = FALSE;

   /* Clear the inventory */
   for (i = 0; i < INVEN_TOTAL; i++)
   {
      invwipe(&inventory[i]);
   }

   /* Start with no artifacts made yet */
   for (i = 0; i < a_number; i++)
   {
       artifact_type *a_ptr = &a_info[i];
       a_ptr->cur_num = 0;
   }

   /* Start with no quests */
   for (i = 0; i < MAX_Q_IDX; i++)
   {
       q_list[i].level = 0;
   }

   /* Add a special quest */
   q_list[0].level = 99;

   /* Add a second quest */
   q_list[1].level = 100;

   /* Reset the "objects" */
   for (i = 1; i < k_number; i++)
   {
      object_kind *k_ptr = &k_info[i];

      /* Reset "tried" */
      k_ptr->tried = FALSE;

      /* Reset "aware" */
      k_ptr->aware = FALSE;
   }

   /* Reset the "monsters" */
   for (i = 1; i < r_number; i++)
   {
      monster_race *r_ptr = &r_info[i];

      /* Hack -- Reset the counter */
      r_ptr->cur_num = 0;

      /* Hack -- Reset the max counter */
      r_ptr->max_num = 100;

      /* Hack -- Reset the max counter */
      if (r_ptr->flags1 & RF1_UNIQUE) r_ptr->max_num = 1;

      /* Clear player kills */
      r_ptr->r_pkills = 0;
   }

   /* Hack -- Well fed player */
   p_ptr->food = PY_FOOD_FULL - 1;

   /* Assume no winning game */
   total_winner = FALSE;

   /* Assume no panic save */
   panic_save = 0;

   /* Assume no cheating */
   noscore = 0;
}

/*
 * Each player starts out with a few items, given as tval/sval pairs.
 * In addition, he always has some food and a few torches.
 */

static byte player_init[MAX_CLASS][9][2] =
{
   {
      /* Warrior */
      { TV_POTION, SV_POTION_BESERK_STRENGTH },
      { TV_SWORD, SV_BROAD_SWORD },
      { TV_HARD_ARMOR, SV_CHAIN_MAIL },
      { 0, 0 }, { 0, 0 }, { 0, 0 }, { 0, 0 }, { 0, 0 },
      { 0, 0 }
   },

   {
      /* Mage */
      { TV_BOOK, SV_BOOK_SMALL },
      { TV_HAFTED, SV_QUARTERSTAFF },
      { TV_WAND, SV_WAND_FIRE_BALL },
      { TV_WAND, SV_WAND_ACID_BOLT },
      { TV_SPELL, SV_SPELL_MMAGIC_MISSILE },
      { TV_SPELL, SV_SPELL_MDETECT_MONSTERS },
      { 0, 0 }, { 0, 0 },
      { 0,0 }

   },

   {
      /* Priest */
      { TV_BOOK, SV_BOOK_SMALL },
      { TV_HAFTED, SV_MACE },
      { TV_POTION, SV_POTION_HEALING },
      { TV_SPELL, SV_SPELL_PDETECT_EVIL },
      { TV_SPELL, SV_SPELL_PCURE_LIGHT_WOUNDS },
      { 0, 0 }, { 0, 0 }, { 0, 0 },
      { 0, 0 }
   },

   {
      /* Rogue */
      { TV_BOOK, SV_BOOK_SMALL },
      { TV_SWORD, SV_RAPIER },
      { TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR },
      { TV_SPELL, SV_SPELL_MPHASE_DOOR },
      { TV_SPELL, SV_SPELL_MCREATESIMPLETRAP },
      { 0, 0 }, { 0, 0 }, { 0, 0 },
      { 0, 0 }
   },

   {
       /* Ranger */
      { TV_BOOK, SV_BOOK_SMALL },
      { TV_SWORD, SV_BROAD_SWORD },
      { TV_BOW, SV_LONG_BOW },
      { TV_SPELL, SV_SPELL_MMAGIC_MISSILE },
      { TV_SPELL, SV_SPELL_MDETECT_MONSTERS },
      { 0, 0 }, { 0, 0 }, { 0, 0 },
      { 0, 0 }
   },

   {
      /* Paladin */
      { TV_BOOK, SV_BOOK_SMALL },
      { TV_SWORD, SV_BROAD_SWORD },
      { TV_SCROLL, SV_SCROLL_PROTECTION_FROM_EVIL },
      { TV_SPELL, SV_SPELL_PDETECT_EVIL },
      { TV_SPELL, SV_SPELL_PCURE_LIGHT_WOUNDS },
      { 0, 0 } , { 0, 0 }, { 0, 0 },
      { 0, 0 }
   },

   {
      /* War Mage */
      { TV_BOOK, SV_BOOK_SMALL },
      { TV_POLEARM, SV_BATTLE_AXE },
      { TV_SPELL, SV_SPELL_MMAGIC_MISSILE },
      { TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR },
      { TV_POTION, SV_POTION_BESERK_STRENGTH },
      { 0, 0 } , { 0, 0 }, { 0, 0 },
      { 0, 0 }
   },

   {
      /* High Priest */
      { TV_BOOK, SV_BOOK_AVG },
      { TV_SPELL, SV_SPELL_PCURE_LIGHT_WOUNDS },
      { TV_SPELL, SV_SPELL_MMAGIC_MISSILE },
      { TV_SPELL, SV_SPELL_PDETECT_MONSTERS },
      { TV_SPELL, SV_SPELL_PFIND_TRAPS },
      { TV_SOFT_ARMOR, SV_ROBE } ,
      { TV_WAND, SV_WAND_WONDER },
      { TV_ROD, SV_ROD_FIRE_BOLT },
      { 0, 0 }
   },

   {
      /* Gladiator */
      { TV_POTION, SV_POTION_BESERK_STRENGTH },
      { TV_POLEARM, SV_TRIDENT },
      { TV_SWORD, SV_DAGGER },
      { TV_HELM, SV_METAL_CAP },
      { 0, 0 }, { 0, 0 }, { 0, 0 }, { 0, 0 },
      { 0, 0 }
   }

};

static byte player_bonus[MAX_CLASS][9][2] =
{
   {
      /* Warrior */
      { TV_POTION, SV_POTION_SPEED },
      { TV_SWORD, SV_RAPIER },
      { TV_BOOTS, SV_PAIR_OF_METAL_SHOD_BOOTS },
      { TV_POTION, SV_POTION_CURE_SERIOUS },
      { TV_HAFTED, SV_MORNING_STAR },
      { TV_SWORD, SV_LONG_SWORD },
      { TV_POLEARM, SV_TRIDENT },
      { TV_POLEARM, SV_BROAD_AXE },
      { TV_SHIELD, SV_SMALL_METAL_SHIELD }
   },

   {
      /* Mage */
      { TV_BOOK, SV_BOOK_SMALL },
      { TV_LITE, SV_LITE_LANTERN },
      { TV_WAND, SV_WAND_COLD_BALL },
      { TV_WAND, SV_WAND_ELEC_BOLT },
      { TV_SPELL, SV_SPELL_MPHASE_DOOR },
      { TV_SPELL, SV_SPELL_MLIGHT_AREA },
      { TV_ROD, SV_ROD_LITE },
      { TV_STAFF, SV_STAFF_CURE_LIGHT },
      { TV_RING, SV_RING_SUSTAIN_INT }

   },

   {
      /* Priest */
      { TV_BOOK, SV_BOOK_SMALL },
      { TV_HAFTED, SV_WHIP },
      { TV_RING, SV_RING_SUSTAIN_WIS },
      { TV_SPELL, SV_SPELL_PCALL_LIGHT },
      { TV_SPELL, SV_SPELL_PBLESS },
      { TV_HAFTED, SV_WAR_HAMMER },
      { TV_BOOTS, SV_PAIR_OF_HARD_LEATHER_BOOTS },
      { TV_WAND, SV_WAND_SLEEP_MONSTER },
      { TV_STAFF, SV_STAFF_CURE_LIGHT }
   },

   {
      /* Rogue */
      { TV_BOOK, SV_BOOK_SMALL },
      { TV_HAFTED, SV_QUARTERSTAFF },
      { TV_SOFT_ARMOR, SV_HARD_STUDDED_LEATHER },
      { TV_SPELL, SV_SPELL_MLIGHT_AREA },
      { TV_SPELL, SV_SPELL_MPHASE_DOOR },
      { TV_RING, SV_RING_SEE_INVIS },
      { TV_RING, SV_RING_SEARCHING },
      { TV_CLOAK, SV_SHADOW_CLOAK },
      { TV_SCROLL, SV_SCROLL_MONSTER_CONFUSION }
   },

   {
       /* Ranger */
      { TV_SWORD, SV_RAPIER },
      { TV_BOW, SV_SLING },
      { TV_BOW, SV_ELVEN_BOW },
      { TV_SPELL, SV_SPELL_MPHASE_DOOR },
      { TV_SPELL, SV_SPELL_MLIGHT_AREA },
      { TV_RING, SV_RING_SUSTAIN_DEX },
      { TV_RING, SV_RING_SEARCHING },
      { TV_SHOT, SV_SHOT_NORMAL },
      { TV_ARROW, SV_ARROW_NORMAL }
   },

   {
      /* Paladin */
      { TV_BOOK, SV_BOOK_SMALL },
      { TV_SWORD, SV_LONG_SWORD },
      { TV_STAFF, SV_STAFF_DETECT_EVIL },
      { TV_SPELL, SV_SPELL_PBLESS },
      { TV_SPELL, SV_SPELL_PREMOVE_FEAR },
      { TV_POTION, SV_POTION_SPEED },
      { TV_POTION, SV_POTION_HEROISM },
      { TV_HARD_ARMOR, SV_CHAIN_MAIL },
      { TV_SHIELD, SV_SMALL_METAL_SHIELD }
   },

   {
      /* War Mage */
      { TV_BOOK, SV_BOOK_SMALL },
      { TV_SWORD, SV_RAPIER },
      { TV_POTION, SV_POTION_CURE_SERIOUS },
      { TV_SPELL, SV_SPELL_MSTINKING_CLOUD },
      { TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR },
      { TV_POTION, SV_POTION_BESERK_STRENGTH },
      { TV_RING, SV_RING_SUSTAIN_INT } ,
      { TV_RING, SV_RING_ACCURACY },
      { TV_BOOTS, SV_PAIR_OF_HARD_LEATHER_BOOTS }
   },

   {
      /* High Priest */
      { TV_BOOK, SV_BOOK_SMALL },
      { TV_SPELL, SV_SPELL_MPHASE_DOOR },
      { TV_SPELL, SV_SPELL_PCALL_LIGHT },
      { TV_SPELL, SV_SPELL_PBLESS },
      { TV_SPELL, SV_SPELL_PSLOW_POISON },
      { TV_STAFF, SV_STAFF_SLOW_MONSTERS } ,
      { TV_WAND, SV_WAND_TELEPORT_AWAY },
      { TV_ROD, SV_ROD_FIRE_BOLT },
      { TV_RING, SV_RING_PROTECTION }
   },

   {
      /* Gladiator */
      { TV_GLOVES, SV_SET_OF_CESTI },
      { TV_POLEARM, SV_LOCHABER_AXE },
      { TV_SWORD, SV_BASTARD_SWORD },
      { TV_HELM, SV_STEEL_HELM },
      { TV_BOOTS, SV_PAIR_OF_METAL_SHOD_BOOTS },
      { TV_RING, SV_RING_ACCURACY },
      { TV_RING, SV_RING_SUSTAIN_STR },
      { TV_AMULET, SV_AMULET_SLOW_DIGEST },
      { TV_SCROLL, SV_SCROLL_STAR_ENCHANT_WEAPON }
   }

};

/*
 * this function creates a wanted tv,sv defined item for the players outfit.
 * it tries mighty hard, but may return a random item...
 */
static void make_outfit_item(object_type *i_ptr, s16b tval, s16b sval)
{
   s16b k_idx;
   s32b cnt;

   k_idx = lookup_kind(tval, sval);

   /* try really hard! */
   for (cnt = 0; cnt < 100000; cnt++)
   {
      invcopy(i_ptr, k_idx);

      apply_magic(i_ptr, 0, FALSE, FALSE, FALSE);

      /* no cursed or broken items please */
      if (cursed_p(i_ptr) || broken_p(i_ptr) || (i_ptr->p1val < 0) ) continue;
      /* also no +3,+2 items please */
      if ((i_ptr->to_a>0) || (i_ptr->to_d>0) || (i_ptr->to_h>0)) continue;
      if ((i_ptr->dd < k_info[i_ptr->k_idx].dd) ||
          (i_ptr->ds < k_info[i_ptr->k_idx].ds)) continue;

      if ((i_ptr->tval == tval) && (i_ptr->sval == sval)) break;
   }

   /* if we did 100000 loops without getting the right item, we */
   /* return a random item..... */
   object_tried(i_ptr);
   object_aware(i_ptr);
   object_known(i_ptr);
   return;
}

/*
 * Init players with some belongings
 *
 * Having an item makes the player "aware" of its purpose.
 */
static void player_outfit()
{
   s16b         i, j, tv, sv;

   object_type forge;

   object_type *i_ptr = &forge;

   /* strange things happen otherwise, you have been warned! */
   forge.spell_set = 0;
   invwipe(&forge);

   /* Hack -- Give the player some food */
   invcopy(i_ptr, lookup_kind(TV_FOOD, SV_FOOD_RATION));
   i_ptr->number = rand_range(3,7);
   object_aware(i_ptr);
   object_known(i_ptr);
   i_ptr->log.where = OBJ_FOUND_BIRTH;
   (void)inven_carry(i_ptr,i_ptr->number);

   /* Hack -- Give the player some torches */
   invcopy(i_ptr, lookup_kind(TV_LITE, SV_LITE_TORCH));
   i_ptr->number = rand_range(3,7);
   i_ptr->p1val = rand_range(3,7) * 500;
   object_known(i_ptr);
   i_ptr->log.where = OBJ_FOUND_BIRTH;
   (void)inven_carry(i_ptr,i_ptr->number);

   /* Give the player useful objects */
   /* continue until tval = 0 in player_init */

   for (i = 0; player_init[p_ptr->pclass][i][0]; i++)
   {
      tv = player_init[p_ptr->pclass][i][0];
      sv = player_init[p_ptr->pclass][i][1];
dlog(DEBUGBIRTH,"birth.c: outfit_player: item %d, tv %d sv %d\n",
                i, tv, sv);
      if (tv)
      {
         make_outfit_item(i_ptr, tv, sv);
dlog(DEBUGBIRTH,"birth.c: outfit_player: made tv %d sv %d pv %d name %s\n",
                i_ptr->tval, i_ptr->sval, i_ptr->p1val,
                k_name + k_info[i_ptr->k_idx].name);
         i_ptr->log.where = OBJ_FOUND_BIRTH;
         (void)inven_carry(i_ptr,i_ptr->number);
      }
   }
   if (p_ptr->sc>=90)
   {
      i = rand_int(9);
dlog(DEBUGBIRTH,"birth.c: player_outfit: 1 of 2 random items: index %d\n", i);
      tv = player_bonus[p_ptr->pclass][i][0];
      sv = player_bonus[p_ptr->pclass][i][1];
      make_outfit_item(i_ptr, tv, sv);
      i_ptr->log.where = OBJ_FOUND_BIRTH;
      (void)inven_carry(i_ptr,i_ptr->number);
      j=i;
      while (j==i) j=rand_int(9);
dlog(DEBUGBIRTH,"birth.c: player_outfit: 2 of 2 random items: index %d\n", j);
      tv = player_bonus[p_ptr->pclass][j][0];
      sv = player_bonus[p_ptr->pclass][j][1];
      make_outfit_item(i_ptr, tv, sv);
      i_ptr->log.where = OBJ_FOUND_BIRTH;
      (void)inven_carry(i_ptr,i_ptr->number);
   }
   else if (p_ptr->sc>=60)
   {
      i = rand_int(9);
dlog(DEBUGBIRTH,"birth.c: player_outfit: 1 of 1 random items: index %d\n", i);
      tv = player_bonus[p_ptr->pclass][i][0];
      sv = player_bonus[p_ptr->pclass][i][1];
      make_outfit_item(i_ptr, tv, sv);
      i_ptr->log.where = OBJ_FOUND_BIRTH;
      (void)inven_carry(i_ptr,i_ptr->number);
   }
}

static s16b get_sex(void)
{
   s16b n, k;
   cptr str;
   char c, buf[80];
   char p2 = ')';

   /* Clean up */
   clear_from(15);

   /*** Player sex ***/

   /* Extra info */
   Term_putstr(5, 15, -1, TERM_WHITE,
           "Your 'sex' does not have any significant gameplay effects.");

   /* Prompt for "Sex" */
   for (n = 0; n < MAX_SEXES; n++)
   {
      /* Analyze */
      p_ptr->psex = n;
      sp_ptr = &sex_info[p_ptr->psex];
      str = sp_ptr->title;

      /* Display */
      sprintf(buf, "%c%c %s", I2A(n), p2, str);
      put_str(buf, 2 + 15 * (n%5), 21 + (n/5));
   }

   /* Choose */
   while (1)
   {
      sprintf(buf, "Choose a sex (%c-%c) or press R for random character generation: ", I2A(0), I2A(n-1));
      put_str(buf, 2, 20);
      c = inkey();
      c = FORCEUPPER(c);
      if (c == 'Q') quit(NULL);
      if (c == 'S') return (FALSE);
      if ((c == 'R') || (c == 'r'))
      {
         return(-1);
      }
      else
         k = A2I(FORCELOWER(c));
      if ((k >= 0) && (k < n)) break;
      if (c == '?') do_cmd_help("help.hlp");
      else bell("Illegal sex!");
   }
   return (k);
}

static byte get_race(void)
{
   s16b n, k;
   cptr str;
   char c, buf[80];
   char p2 = ')';

   /* Clean up */
   clear_from(15);

   /*** Player race ***/

   /* Extra info */
   Term_putstr(5, 15, -1, TERM_WHITE,
           "Your 'race' determines various intrinsic factors and bonuses.");


   /* Dump races */
   for (n = 0; n < MAX_RACES; n++)
   {
      /* Analyze */
      p_ptr->prace = (byte)n;
      rp_ptr = &race_info[p_ptr->prace];
      str = rp_ptr->title;

      /* Display */
      sprintf(buf, "%c%c %s", I2A(n), p2, str);
      put_str(buf, 2 + 15 * (n%5), 21 + (n/5));
   }

   /* Choose */
   while (1)
   {
      sprintf(buf, "Choose a race (%c-%c): ", I2A(0), I2A(n-1));
      put_str(buf, 2, 20);
      c = inkey();
      c = FORCEUPPER(c);
      if (c == 'Q') quit(NULL);
      if (c == 'S') return (FALSE);
      k = A2I(FORCELOWER(c));
      if ((k >= 0) && (k < n)) break;
      if (c == '?') do_cmd_help("help.hlp");
      else bell("Illegal race!");
   }
   return (byte)(k);
}

static s16b get_class(void)
{
   s16b n, k;
   cptr str;
   char c, buf[80];
   char p2 = ')';

   /* Clean up */
   clear_from(15);

   /* Extra info */
   Term_putstr(5, 15, -1, TERM_WHITE,
           "Your 'class' determines various intrinsic abilities and bonuses.");
   Term_putstr(5, 16, -1, TERM_WHITE,
           "Any entries with a (*) should only be used by advanced players.");

   /* Dump classes */
   for (n = 0; n < MAX_CLASS; n++)
   {
      cptr mod = "";

      /* Analyze */
      p_ptr->pclass = n;
      cp_ptr = &class_info[p_ptr->pclass];
      str = cp_ptr->title;

      /* Display */
      sprintf(buf, "%c%c %s%s", I2A(n), p2, str, mod);
      put_str(buf, 2 + 20 * (n%3), 21 + (n/3));
   }

   /* Get a class */
   while (1)
   {
      sprintf(buf, "Choose a class (%c-%c): ", I2A(0), I2A(n-1));
      put_str(buf, 2, 20);
      c = inkey();
      c = FORCEUPPER(c);
      if (c == 'Q') quit(NULL);
      if (c == 'S') return (FALSE);
      k = A2I(FORCELOWER(c));
      if ((k >= 0) && (k < n)) break;
      if (c == '?') do_cmd_help("help.hlp");
      else bell("Illegal class!");
   }
   return (k);
}

static bool get_save_levels(void)
{
   char c;

   /* Clear */
   clear_from(15);

   /*** Save levels ***/

   /* Extra info */
   Term_putstr(5, 15, -1, TERM_WHITE,
           "Saving levels creates persistant dungeon levels, which will stay");
   Term_putstr(5, 16, -1, TERM_WHITE,
           "the same if you later return. Not saving levels generates new");
   Term_putstr(5, 17, -1, TERM_WHITE,
           "levels each time you enter them.");

   /* Ask about "save levels" */
   while (1)
   {
      put_str("Save levels? (y/n) ", 2, 20);
      c = inkey();
      c = FORCEUPPER(c);
      if (c == 'Q') quit(NULL);
      if (c == 'S') return (FALSE);
      if (c == ESCAPE) break;
      if ((c == 'Y') || (c == 'N')) break;
      if (c == '?') do_cmd_help("help.hlp");
      else bell("Answer y or n!");
   }
   return (c=='Y');
}

static bool get_teachers(void)
{
   char c;

   /* Clear */
   clear_from(15);

   /* Extra info */
   Term_putstr(5, 15, -1, TERM_WHITE,
           "Using 'teacher'-mode birth creates exiting characters.");

   /* Ask about "teachers" mode */
   while (1)
   {
      put_str("Use 'teacher' mode? (y/n) ", 2, 20);
      c = inkey();
      c = FORCEUPPER(c);
      if (c == 'Q') quit(NULL);
      if (c == 'S') return (FALSE);
      if (c == ESCAPE) break;
      if ((c == 'Y') || (c == 'N')) break;
      if (c == '?') do_cmd_help("help.hlp");
      else bell("Illegal teacher flag!");
   }
   return (c=='Y');
}

static bool do_roll(all_random)
{
   char c;
   bool ok = FALSE, boost_social = FALSE;

dlog(DEBUGBIRTH,"birth.c: do_roll: step 01\n");
   while (!ok)
   {
      /* Get a new character */
      if (p_ptr->teach_birth) choose_teachers(all_random);

dlog(DEBUGBIRTH,"birth.c: do_roll: step 02\n");
      get_stats();
dlog(DEBUGBIRTH,"birth.c: get_teachers: step 1 stat_use %d %d %d %d %d %d, stat_max %d %d %d %d %d %d\n",
                p_ptr->stat_use[0], p_ptr->stat_use[1], p_ptr->stat_use[2],
                p_ptr->stat_use[3], p_ptr->stat_use[4], p_ptr->stat_use[5],
                p_ptr->stat_max[0], p_ptr->stat_max[1], p_ptr->stat_max[2],
                p_ptr->stat_max[3], p_ptr->stat_max[4], p_ptr->stat_max[5]);

dlog(DEBUGBIRTH,"birth.c: do_roll: step 03\n");
      /* Roll for base hitpoints */
      get_extra();
dlog(DEBUGBIRTH,"birth.c: get_teachers: step 2 stat_use %d %d %d %d %d %d, stat_max %d %d %d %d %d %d\n",
                p_ptr->stat_use[0], p_ptr->stat_use[1], p_ptr->stat_use[2],
                p_ptr->stat_use[3], p_ptr->stat_use[4], p_ptr->stat_use[5],
                p_ptr->stat_max[0], p_ptr->stat_max[1], p_ptr->stat_max[2],
                p_ptr->stat_max[3], p_ptr->stat_max[4], p_ptr->stat_max[5]);

dlog(DEBUGBIRTH,"birth.c: do_roll: step 04\n");
      /* Roll for age/height/weight */
      get_physique();

      /* Roll for social class */

      /* once in 4 times, boost social class */
      boost_social = (randint(4)==1);
dlog(DEBUGBIRTH,"birth.c: do_roll: boost_social %d\n", boost_social);

dlog(DEBUGBIRTH,"birth.c: do_roll: step 05\n");
      /* Roll for social class */
      get_history(boost_social);

dlog(DEBUGBIRTH,"birth.c: do_roll: step 06\n");
      /* Roll for gold */
      get_money(boost_social);

dlog(DEBUGBIRTH,"birth.c: do_roll: step 07\n");
      /* Now create the history */
      create_history();

dlog(DEBUGBIRTH,"birth.c: do_roll: step 08\n");
dlog(DEBUGBIRTH,"birth.c: get_teachers: step 3 stat_use %d %d %d %d %d %d, stat_max %d %d %d %d %d %d\n",
                p_ptr->stat_use[0], p_ptr->stat_use[1], p_ptr->stat_use[2],
                p_ptr->stat_use[3], p_ptr->stat_use[4], p_ptr->stat_use[5],
                p_ptr->stat_max[0], p_ptr->stat_max[1], p_ptr->stat_max[2],
                p_ptr->stat_max[3], p_ptr->stat_max[4], p_ptr->stat_max[5]);
      /* Calculate the bonuses and hitpoints */
      p_ptr->update |= (PU_BONUS | PU_HP);

      /* Update stuff */
      update_stuff();
dlog(DEBUGBIRTH,"birth.c: get_teachers: step 4 stat_use %d %d %d %d %d %d, stat_max %d %d %d %d %d %d\n",
                p_ptr->stat_use[0], p_ptr->stat_use[1], p_ptr->stat_use[2],
                p_ptr->stat_use[3], p_ptr->stat_use[4], p_ptr->stat_use[5],
                p_ptr->stat_max[0], p_ptr->stat_max[1], p_ptr->stat_max[2],
                p_ptr->stat_max[3], p_ptr->stat_max[4], p_ptr->stat_max[5]);

      /* Fully healed */
      p_ptr->chp = p_ptr->mhp;

      /* Fully rested */
      p_ptr->csp = p_ptr->msp;

      /* Display the player */
      display_player(0, TRUE);
dlog(DEBUGBIRTH,"birth.c: get_teachers: stat_use %d %d %d %d %d %d, stat_max %d %d %d %d %d %d\n",
                p_ptr->stat_use[0], p_ptr->stat_use[1], p_ptr->stat_use[2],
                p_ptr->stat_use[3], p_ptr->stat_use[4], p_ptr->stat_use[5],
                p_ptr->stat_max[0], p_ptr->stat_max[1], p_ptr->stat_max[2],
                p_ptr->stat_max[3], p_ptr->stat_max[4], p_ptr->stat_max[5]);

      /* if random, this char is already OK */
      if (all_random) return (TRUE);

      /* Prepare a prompt (must squeeze everything in) */
      c_put_str(TERM_WHITE, "['r' reroll character, 'n' new character, Q to quit, other key to accept]", 4, 23);

      /* Prompt and get a command */
      c = inkey();
      c = FORCEUPPER(c);

      /* Quit */
      if (c == 'Q') quit(NULL);

      /* Reroll this character */
      if ((c == 'n') || (c == 'N')) return (FALSE);
      ok = TRUE;
      if ((c == 'r') || (c == 'R')) ok = FALSE;
   }

   return (TRUE);
}


static bool player_birth_aux()
{
   s16b i;
   char c;

   bool all_random = FALSE;
   bool char_good = FALSE;
   bool first_time = TRUE;

   cptr str;

   /*** Intro ***/

   while (!char_good)
   {
      /* Clear screen */
      Term_clear();

      /* Title everything */
      put_str("Name        :", 1, 2);
      put_str("Sex         :", 1, 3);
      put_str("Race        :", 1, 4);
      put_str("Class       :", 1, 5);

      /* Dump the default name */
      c_put_str(TERM_L_BLUE, player_name, 15, 2);

      /*** Instructions ***/

      /* Display some helpful information */
      Term_putstr(5, 10, -1, TERM_WHITE,
              "Please answer the following questions.  Most of the questions");
      Term_putstr(5, 11, -1, TERM_WHITE,
              "display a set of standard answers, and many will also accept");
      Term_putstr(5, 12, -1, TERM_WHITE,
              "some special responses, including 'Q' to quit, 'S' to restart,");
      Term_putstr(5, 13, -1, TERM_WHITE,
              "and '?' for help.  Note that 'Q' and 'S' must be capitalized.");

      /* Set sex */
      if (!all_random)
      {
         i = get_sex();

         if (i == -1)
         {
            all_random = TRUE;
            p_ptr->psex = rand_int(MAX_SEXES);
         }
         else
         {
            p_ptr->psex = i;
         }
      }
      else
      {
         p_ptr->psex = rand_int(MAX_SEXES);
      }

      sp_ptr = &sex_info[p_ptr->psex];
      str = sp_ptr->title;

      /* Display */
      c_put_str(TERM_L_BLUE, str, 15, 3);

      /* Set race */
      if (all_random)
         p_ptr->prace = (byte)rand_int(MAX_RACES);
      else
         p_ptr->prace = get_race();

      rp_ptr = &race_info[p_ptr->prace];
      str = rp_ptr->title;

      /* Display */
      c_put_str(TERM_L_BLUE, str, 15, 4);

      /*** Player class ***/

      /* Set class */
      if (all_random)
         p_ptr->pclass = rand_int(MAX_CLASS);
      else
         p_ptr->pclass = get_class();

      cp_ptr = &class_info[p_ptr->pclass];
      str = cp_ptr->title;

dlog(DEBUGBIRTH,"birth.c: player_birth_aux: race %s class %s\n",
                 rp_ptr->title, cp_ptr->title);
      /* Display */
      c_put_str(TERM_L_BLUE, cp_ptr->title, 15, 5);

      if (first_time)
      {
         p_ptr->teach_birth = get_teachers();

         save_levels = get_save_levels();
         first_time = FALSE;
      }

      /* Clean up */
      clear_from(10);

      /*** Generate ***/

      /* Roll */
      char_good = do_roll(all_random);
      if (!char_good) continue;

      /* Clear prompt */
      clear_from(23);

      /*** Finish up ***/

      /* Prompt for it */
      if (all_random)
      {
         prt("['Q' suicide, 'S' start over, 'R' reroll character, other key to continue]", 2, 23);
      }
      else
      {
         prt("['Q' to suicide, 'S' to start over, other key to continue]", 10, 23);
      }

      /* Get a key */
      c = inkey();
      c = FORCEUPPER(c);

      /* Quit */
      if (c == 'Q') quit(NULL);

      /* Start over */
      if (c == 'S') return (FALSE);

      if (all_random)
      {
         char_good = (c != 'R');
      }
      else
      {
         char_good = TRUE;
      }
   }
   /* Get a name, recolor it, prepare savefile */
   get_name();

   /* Accept */
/* first set tactic to suitable value */
   p_ptr->tactic = (rp_ptr->tactic + cp_ptr->tactic) / 2;
   return (TRUE);
}

/*
 * Create a new character.
 *
 * Note that we may be called with "junk" leftover in the various
 * fields, so we must be sure to clear them first.
 */
void player_birth(void)
{
dlog(DEBUGBIRTH,"birth.c: player_birth: before: stat_use %d %d %d %d %d %d, stat_max %d %d %d %d %d %d\n",
                p_ptr->stat_use[0], p_ptr->stat_use[1], p_ptr->stat_use[2],
                p_ptr->stat_use[3], p_ptr->stat_use[4], p_ptr->stat_use[5],
                p_ptr->stat_max[0], p_ptr->stat_max[1], p_ptr->stat_max[2],
                p_ptr->stat_max[3], p_ptr->stat_max[4], p_ptr->stat_max[5]);
   /* Create a new character */
   while (1)
   {
      /* Wipe the player */
      player_wipe();

      /* Roll up a new character */
      if (player_birth_aux()) break;
   }

   /* Note player birth in the message recall */
   message_add("===============================================================================");

dlog(DEBUGFLOW,"birth.c: player_birth: about to outfit player\n");
   player_outfit();
   /* Read all the ghosts in this game */
dlog(DEBUGFLOW,"birth.c: player_birth: about to call rd_ghost_files\n");
   rd_ghost_files();

   /* we are currently not in the arena */
   p_ptr->arena_state = ARENA_NONE;

   /* make sure no previous level-file is about to be deleted */
   sf_lastfile_name[0]=0;
dlog(DEBUGBIRTH,"birth.c: player_birth: finished. stat_use %d %d %d %d %d %d, stat_max %d %d %d %d %d %d\n",
                p_ptr->stat_use[0], p_ptr->stat_use[1], p_ptr->stat_use[2],
                p_ptr->stat_use[3], p_ptr->stat_use[4], p_ptr->stat_use[5],
                p_ptr->stat_max[0], p_ptr->stat_max[1], p_ptr->stat_max[2],
                p_ptr->stat_max[3], p_ptr->stat_max[4], p_ptr->stat_max[5]);
}
