/* create.c: create a player character

   Copyright (c) 1989 James E. Wilson, Robert A. Koeneke

   This software may be copied and distributed for educational, research, and
   not for profit purposes provided that this copyright and statement are
   included in all such copies. */

#include "constant.h"
#include "config.h"
#include "types.h"
#include "externs.h"

#ifdef USG
#ifndef ATARIST_MWC
#include <string.h>
#endif
#else
#include <strings.h>
#endif

struct previous {
  int16u age;
  int16u wt;
  int16u ht;
  int16 disarm;
  int16u str;
  int16u itl;
  int16u wis;
  int16u dex;
  int16u con;
  int16u chr;
  int16u luc;
  int16u sc;
  char history[4][60];
  background_type bg;
} prev;


short autoroll; /* If >0, we are autorolling */
int minstat[6]; /* Min stats, for autoroll */
int modstat[6]; /* Modifiers to stats, chosen by player */
static char statstr[]="StrIntWisDexConChrLuc";
extern int peek, notarget;

/* Generates character's stats				-JWT-	*/
static void get_stats()
{
  register int i, tot;
  int dice[21];

  do
    {
      tot = 0;
      for (i = 0; i < 21; i++)
	{
	  dice[i] = randint (3 + i % 3);  /* Roll 3,4,5 sided dice once each */
	  tot += dice[i];
	}
    }
  while (tot <= 42 || tot >= 72);

  for (i = 0; i < 7; i++)
    py.stats.max_stat[i] = 5 + dice[3*i] + dice[3*i+1] + dice[3*i+2];
}


/* Changes stats by given amount				-JWT-	*/
static void change_stat(stat, amount)
int stat;
int16 amount;
{
  register int i;
  register int tmp_stat;

  tmp_stat = py.stats.max_stat[stat];
  if (amount < 0)
    for (i = 0; i > amount; i--)
      {
	if (tmp_stat > 108)
	  tmp_stat--;
	else if (tmp_stat > 88)
	  tmp_stat += -randint(6) - 2;
	else if (tmp_stat > 18)
	  {
	    tmp_stat += -randint(15) - 5;
	    if (tmp_stat < 18)
	      tmp_stat = 18;
	  }
	else if (tmp_stat > 3)
	  tmp_stat--;
      }
  else
    for (i = 0; i < amount; i++)
      {
	if (tmp_stat < 18)
	  tmp_stat++;
	else if (tmp_stat < 88)
	  tmp_stat += randint(15) + 5;
	else if (tmp_stat < 108)
	  tmp_stat += randint(6) + 2;
	else if (tmp_stat < 118)
	  tmp_stat++;
      }
  py.stats.max_stat[stat] = tmp_stat;
}

static int get_prev_stats() {
  if (!prev.str) return 0; 
  py.stats.cur_stat[0]=(py.stats.max_stat[0]=prev.str);
  py.stats.cur_stat[1]=(py.stats.max_stat[1]=prev.itl);
  py.stats.cur_stat[2]=(py.stats.max_stat[2]=prev.wis);
  py.stats.cur_stat[3]=(py.stats.max_stat[3]=prev.dex);
  py.stats.cur_stat[4]=(py.stats.max_stat[4]=prev.con);
  py.stats.cur_stat[5]=(py.stats.max_stat[5]=prev.chr);
  py.stats.cur_stat[6]=(py.stats.max_stat[6]=prev.luc);
  set_use_stat(0);
  set_use_stat(1);
  set_use_stat(2);
  set_use_stat(3);
  set_use_stat(4);
  set_use_stat(5);
  set_use_stat(6);
  py.misc.ptodam = todam_adj();
  py.misc.ptohit = tohit_adj();
  py.misc.pac    = toac_adj();
  py.misc.timeout = 0; /* Used for Iron Will */
  prev.str=0;
  return 1;
}

/* generate all stats and modify for race. needed in a separate module so
   looping of character selection would be allowed     -RGM- */
static void get_all_stats ()
{
  register player_type *p_ptr;
  register race_type *r_ptr;
  register int j;
  
  prev.str = (int16u)py.stats.max_stat[0];
  prev.itl = (int16u)py.stats.max_stat[1];
  prev.wis = (int16u)py.stats.max_stat[2];
  prev.dex = (int16u)py.stats.max_stat[3];
  prev.con = (int16u)py.stats.max_stat[4];
  prev.chr = (int16u)py.stats.max_stat[5];
  prev.luc = (int16u)py.stats.max_stat[6];
  p_ptr = &py;
  r_ptr = &race[p_ptr->misc.prace];
  get_stats (); 
  change_stat (A_STR, r_ptr->str_adj);
  change_stat (A_INT, r_ptr->int_adj);
  change_stat (A_WIS, r_ptr->wis_adj);
  change_stat (A_DEX, r_ptr->dex_adj);
  change_stat (A_CON, r_ptr->con_adj);
  change_stat (A_CHR, r_ptr->chr_adj);
  change_stat (A_LUC, r_ptr->luc_adj);
  for (j = 0; j < 7; j++)
    { /* set_use_stat(j) is crashing */
      py.stats.cur_stat[j] = py.stats.max_stat[j];
      set_use_stat (j); 
    }
  p_ptr->misc.srh    = r_ptr->srh;
  p_ptr->misc.hitdie = r_ptr->bhitdie;
  p_ptr->misc.ptodam = todam_adj();
  p_ptr->misc.ptohit = tohit_adj();
  p_ptr->misc.ptoac  = toac_adj();
  p_ptr->misc.pac    = 0;
  p_ptr->misc.expfact = r_ptr->b_exp;
  p_ptr->flags.see_infra = r_ptr->infra;
}

/* Modified set_use_stat, so we don't get "you can learn new spells now"
message all the time */
void set_stat(stat)
int stat;
{
 py.stats.use_stat[stat]=modify_stat(stat,py.stats.mod_stat[stat]);
}

/* Allows player to select a race			-JWT-	*/
static void choose_race()
{
  register int j, k;
  int l, m, exit_flag;
  char s;
  char tmp_str[80];
  register player_type *p_ptr;
  register race_type *r_ptr;

  j = 0;
  k = 0;
  l = 2;
  m = 21;
  clear_from (20);
  put_buffer("Choose a race (? for Help):", 20, 2);
  do
    {
      (void) sprintf(tmp_str, "%c) %s", k+'a', race[j].trace);
      put_buffer(tmp_str, m, l);
      k++;
      l += 15;
      if (l > 70)
	{
	  l = 2;
	  m++;
	}
      j++;
    }
  while (j < MAX_RACES);
  exit_flag = FALSE;
  do
    {
      move_cursor (20, 30);
      s = inkey();
      j = s - 'a';
      if ((j < MAX_RACES) && (j >= 0))
	exit_flag = TRUE;
      else if (s == '?')
	helpfile (ANGBAND_WELCOME);
      else
	bell ();
    }
  while (!exit_flag);

  p_ptr = &py;
  r_ptr = &race[j];
  p_ptr->misc.prace  = j;
  put_buffer(r_ptr->trace, 3, 15);
  /* Now we roll up the skills */
  for (j=0;j<S_NUM;j++)
    {
      exit_flag=race[py.misc.prace].start[j];
      k=(exit_flag&0xF)+(((exit_flag>>4)&0xF)<<4)/3+1;
      py.skills.cur_skill[j]=randint(k)+randint(k)+randint(k);
      py.skills.max_skill[j]=py.skills.cur_skill[j];
      py.skills.min_skill[j]=py.skills.cur_skill[j];
      py.skills.adv_skill[j]=0;
    }
}


/* Will print the history of a character			-JWT-	*/
static void print_history()
{
  register int i;

  put_buffer("Character Background", 14, 27);
  for (i = 0; i < 4; i++)
    prt(py.misc.history[i], i+15, 10);
}

static void get_prev_history()
{
  background->info = prev.bg.info;
  background->roll = prev.bg.roll;
  background->chart = prev.bg.chart;
  background->next = prev.bg.next;
  background->bonus = prev.bg.bonus;
  py.misc.sc = prev.sc;
  
  strncpy(py.misc.history[0], prev.history[0], 60);
  strncpy(py.misc.history[1], prev.history[1], 60);
  strncpy(py.misc.history[2], prev.history[2], 60);
  strncpy(py.misc.history[3], prev.history[3], 60);
}  

/* Get the racial history, determines social class	-RAK-	*/
/* Assumptions:	Each race has init history beginning at		*/
/*		(race-1)*3+1					*/
/*		All history parts are in ascending order	*/
static void get_history()
{
  int hist_ptr, cur_ptr, test_roll, flag;
  register int start_pos, end_pos, cur_len;
  int line_ctr, new_start, social_class;
  char history_block[240];
  register background_type *b_ptr;

  prev.bg.info = background->info;
  prev.bg.roll = background->roll;
  prev.bg.chart = background->chart;
  prev.bg.next = background->next;
  prev.bg.bonus = background->bonus;
  prev.sc = py.misc.sc;

  (void) strncpy(prev.history[0], py.misc.history[0], 60);
  (void) strncpy(prev.history[1], py.misc.history[1], 60);
  (void) strncpy(prev.history[2], py.misc.history[2], 60);
  (void) strncpy(prev.history[3], py.misc.history[3], 60);

  /* Get a block of history text				*/
  if (py.misc.prace == 8)
    hist_ptr = 1;
  else if (py.misc.prace > 8)
    hist_ptr = 2*3 +1;
  else
    hist_ptr = py.misc.prace*3 + 1;
  history_block[0] = '\0';
  social_class = randint(4);
  cur_ptr = 0;
  do
    {
      flag = FALSE;
      do
	{
	  if (background[cur_ptr].chart == hist_ptr)
	    {
	      test_roll = randint(100);
	      while (test_roll > background[cur_ptr].roll)
		cur_ptr++;
	      b_ptr = &background[cur_ptr];
	      (void) strcat(history_block, b_ptr->info);
	      social_class += b_ptr->bonus - 50;
	      if (hist_ptr > b_ptr->next)
		cur_ptr = 0;
	      hist_ptr = b_ptr->next;
	      flag = TRUE;
	    }
	  else
	    cur_ptr++;
	}
      while (!flag);
    }
  while (hist_ptr >= 1);

  /* clear the previous history strings */
  for (hist_ptr = 0; hist_ptr < 4; hist_ptr++)
    py.misc.history[hist_ptr][0] = '\0';

  /* Process block of history text for pretty output	*/
  start_pos = 0;
  end_pos   = strlen(history_block) - 1;
  line_ctr  = 0;
  flag = FALSE;
  while (history_block[end_pos] == ' ')
    end_pos--;
  do
    {
      while (history_block[start_pos] == ' ')
	start_pos++;
      cur_len = end_pos - start_pos + 1;
      if (cur_len > 60)
	{
	  cur_len = 60;
	  while (history_block[start_pos+cur_len-1] != ' ')
	    cur_len--;
	  new_start = start_pos + cur_len;
	  while (history_block[start_pos+cur_len-1] == ' ')
	    cur_len--;
	}
      else
	flag = TRUE;
      (void) strncpy(py.misc.history[line_ctr], &history_block[start_pos],
		     cur_len);
      py.misc.history[line_ctr][cur_len] = '\0';
      line_ctr++;
      start_pos = new_start;
    }
  while (!flag);

  /* Compute social class for player			*/
  if (social_class > 100)
    social_class = 100;
  else if (social_class < 1)
    social_class = 1;
  py.misc.sc = social_class;
}


/* Gets the character's sex				-JWT-	*/
static void get_sex()
{
  register int exit_flag;
  char c;

  exit_flag = FALSE;
  clear_from (20);
  put_buffer("Choose a sex (? for Help):", 20, 2);
  put_buffer("m) Male       f) Female", 21, 2);
  do
    {
      move_cursor (20, 29);
      /* speed not important here */
      c = inkey();
      if (c == 'f' || c == 'F')
	{
	  py.misc.male = FALSE;
	  put_buffer("Female", 4, 15);
	  exit_flag = TRUE;
	}
      else if (c == 'm' || c == 'M')
	{
	  py.misc.male = TRUE;
	  put_buffer("Male", 4, 15);
	  exit_flag = TRUE;
	}
      else if (c == '?')
	helpfile (ANGBAND_WELCOME);
      else
	bell ();
    }
  while (!exit_flag);
}


/* Computes character's age, height, and weight		-JWT-	*/
static void get_ahw()
{
  register int i;

  prev.age = py.misc.age;
  prev.wt = py.misc.wt;
  prev.ht = py.misc.ht;
  prev.disarm = py.misc.disarm;
  i = py.misc.prace;
  py.misc.age = race[i].b_age + randint((int)race[i].m_age);
  if (py.misc.male)
    {
      py.misc.ht = randnor((int)race[i].m_b_ht, (int)race[i].m_m_ht);
      py.misc.wt = randnor((int)race[i].m_b_wt, (int)race[i].m_m_wt);
    }
  else
    {
      py.misc.ht = randnor((int)race[i].f_b_ht, (int)race[i].f_m_ht);
      py.misc.wt = randnor((int)race[i].f_b_wt, (int)race[i].f_m_wt);
    }
  py.misc.disarm = race[i].b_dis + todis_adj();
}

static void get_prev_ahw()
{
  py.misc.age = prev.age;
  py.misc.wt = prev.wt;
  py.misc.ht = prev.ht;
  py.misc.disarm = prev.disarm;
  prev.age = prev.wt = prev.ht = prev.disarm = 0;
}


/* We now use this to modify stats */
static void mod_stats()
{
  register int i, tmp;
  int min_value, max_value;
  int percent;
  char buf[50];
  register struct misc *m_ptr;
  register player_type *p_ptr;

  p_ptr = &py;	
  m_ptr = &(py.misc);

  m_ptr->mhp = con_adj() + m_ptr->hitdie;

  /* initialize hit_points array */
  /* We eliminated the requirements---the way we roll HP makes really low
     HP impossible */
  min_value = 0;
  max_value = 65535;
  player_hp[0] = m_ptr->hitdie;
  tmp=m_ptr->hitdie/3+1;
  do {
    for (i = 1; i < MAX_PLAYER_LEVEL; i++) {
      player_hp[i] = randint(tmp)+randint(tmp)+randint(tmp);
      player_hp[i] += player_hp[i-1];
    }
  }
  while ((player_hp[MAX_PLAYER_LEVEL-1] < min_value) ||
	 (player_hp[MAX_PLAYER_LEVEL-1] > max_value));

  if (peek) {
    percent = (player_hp[MAX_PLAYER_LEVEL-1]*200) /
      (m_ptr->hitdie + ((MAX_PLAYER_LEVEL-1)*m_ptr->hitdie));
    sprintf(buf, "%d%% Life Rating", percent);
    msg_print(buf);
  }
}

void rerate() {
  int min_value, max_value, i, percent;
  char buf[50];
  struct misc *m_ptr = &py.misc;

  min_value = (MAX_PLAYER_LEVEL*2/8 * (m_ptr->hitdie-1)) +
    MAX_PLAYER_LEVEL;
  max_value = (MAX_PLAYER_LEVEL*7/8 * (m_ptr->hitdie-1)) +
    MAX_PLAYER_LEVEL;
  player_hp[0] = m_ptr->hitdie;
  do {
    for (i = 1; i < MAX_PLAYER_LEVEL; i++)
      {
	player_hp[i] = randint((int)m_ptr->hitdie);
	player_hp[i] += player_hp[i-1];
      }
  }
  while ((player_hp[MAX_PLAYER_LEVEL-1] < min_value) ||
	 (player_hp[MAX_PLAYER_LEVEL-1] > max_value));

  percent = (int) (((double) (player_hp[MAX_PLAYER_LEVEL-1]*100) /
		    (double) ((double)m_ptr->hitdie + 
			      (double)((double)((MAX_PLAYER_LEVEL-1)*
						m_ptr->hitdie)/(double)2))));
  sprintf(buf, "%d%% Life Rating", percent);
  calc_hitpoints();
  prt_stat_block();
  msg_print(buf);
}

/* Modifies stats based on player preferences */
static void get_stats_choice()
{
  register int i, j, total;
  int k, l, m, cstat;
  int exit_flag;
  char tmp_str[80], s;

  i = py.misc.prace;
  j = 0;
  k = 0;
  l = 2;
  m = 21;
  notarget=-128; /* Special value so calc_bonuses will ignore inventory when
		    first starting out */
  clear_from (20);
  exit_flag = FALSE;
  put_buffer("Press 'Y' if you want to autoroll",22,2);
  for(exit_flag=0;exit_flag<=5;exit_flag++)
    minstat[exit_flag]=0; /* Accept anything */
  autoroll=0;
  s=inkey();
 if (s=='y' || s=='Y')
 {
  clear_from (15);
  autoroll=1;
  put_buffer("Please enter minimum value for:",15,6);
  for(cstat=0;cstat<6;cstat++) /* Luck is random, still */
  {
  strncpy(tmp_str,statstr+cstat*3,3);
  tmp_str[3]=0; /* Keep it nice and clean */
  strcat(tmp_str,":");
  put_buffer(tmp_str,17+cstat,2);
  while (!get_string(tmp_str,17+cstat,6,3)); /* Wait for something */
  minstat[cstat]=atoi(tmp_str);  
  if (minstat[cstat]<2) minstat[cstat]=2;
  if (minstat[cstat]>118) minstat[cstat]=118;
  modstat[cstat]=0;
  }
 }
  put_buffer("Press 'Y' if you want to apply modifiers to your stats.",22,2);  
  s=inkey();
  if (s=='y' || s=='Y')
    {
      total=50;
      while(total>10)
	{
	  clear_from(15);
	  put_buffer("Please enter a modifier for each stat.",13,6);
	  put_buffer
	    ("NOTE:  The total of the modifiers may be no larger than 10.",
		 14,6);
	  total=0;
	  for(cstat=0;cstat<6;cstat++) /* Leave Luck random */
	    {
	      strncpy(tmp_str,statstr+cstat*3,3);
	      tmp_str[3]=0;
	      strcat(tmp_str,":");
	      put_buffer(tmp_str,17+cstat,2);
	      while (!get_string(tmp_str,17+cstat,6,3)); /* Waiting for # */
	      modstat[cstat]=atoi(tmp_str);  
	      if (modstat[cstat]<-7) modstat[cstat]=-7;
	      if (modstat[cstat]>7) modstat[cstat]=7;
	      total+=modstat[cstat];
	    }
	}
      clear_from(13); /* Make a fresh screen */
    }
  py.misc.realm=NONE;
}


/* Given a stat value, return a monetary value, which affects the amount
   of gold a player has. */
static int monval (i)
int16u i;
{
  return 7 * ((int)i - 10);
}


static void get_money()
{
  register int tmp, gold;
  register int16u *a_ptr;

  a_ptr = py.stats.max_stat;
  tmp = monval (a_ptr[A_STR]) + monval (a_ptr[A_INT])
      + monval (a_ptr[A_WIS]) + monval (a_ptr[A_CON])
      + monval (a_ptr[A_DEX]);
  /* Luck doesn't figure in */

  gold = py.misc.sc*6 + randint (25) + 325;	/* Social Class adj */
  gold -= tmp;					/* Stat adj */
  gold += monval (a_ptr[A_CHR]);		/* Charisma adj	*/
  if (!py.misc.male)
    gold += 50;			/* She charmed the banker into it! -CJS- */
                                /* She slept with the banker.. :) -GDH- */
  if (gold < 80)
    gold = 80;			/* Minimum */
  py.misc.au = gold*4+900;
}


/* ---------- M A I N  for Character Creation Routine ---------- */
/*							-JWT-	*/
void create_character()
{
  register int exit_flag = 1,holding,loop,bye;
  register char c;
  char str[80];

  min_hp=0; /* Just so we don't die immediately */
  put_character();
  choose_race();
  get_sex();
  get_stats_choice();
  bye=1;
  holding=0;
  put_buffer("Rolling Char",12,30);
  while(holding<20000 && bye)
    {
      sprintf(str,"%d       ",holding);
      put_buffer(str,12,7);
      ++holding;
      bye=0;
      get_all_stats();
      mod_stats();
      get_history();
      get_ahw();
      for(loop=0;loop<6;loop++)
	{
	  change_stat(loop,modstat[loop]);
	  if (minstat[loop]>py.stats.max_stat[loop])
	    bye=1;
	  py.stats.cur_stat[loop]=py.stats.max_stat[loop];
	  set_stat(loop); /* was set_use_stat(loop) */
	}
    }
  clear_from (13);
  print_history();
  put_misc1();
  put_stats();
  
  clear_from (20);
  if (!autoroll) /* ONLY show this if we didn't autoroll */
  {
  put_buffer("Hit space: Reroll, ^P: Previous or ESC: Accept: ", 20, 2);
  do
    {
      move_cursor (20, 50);
      c = inkey();
      if (c == ESCAPE) {
	exit_flag = 0;
      } else if (c == ' ') {
	get_all_stats();
	get_history();
	mod_stats();
	get_ahw();
	  for(loop=0;loop<6;loop++)
	    {
	      change_stat(loop,modstat[loop]);
	      py.stats.cur_stat[loop]=py.stats.max_stat[loop];
	      set_stat(loop);
	    }
	print_history();
	put_misc1();
	put_stats();
      } else if (c == CTRL('P')) {
	if (get_prev_stats()) {
	  get_prev_history();
	  get_prev_ahw();
	  print_history();
	  put_misc1();
	  for(loop=0;loop<6;loop++)
	    {
	      change_stat(loop,modstat[loop]);
	      py.stats.cur_stat[loop]=py.stats.max_stat[loop];
	      set_stat(loop);
	    }
	  put_stats();
	} else {
	  bell();
	}
      } else {
	bell();
      }
    }		    /* done with stats generation */
  while (exit_flag == 1);
 }
  calc_bonuses();
  py.misc.exp+=30*race[py.misc.prace].lifexp;
  bye=0;
  for(loop=0;loop<6;loop++)
    bye+=modstat[loop];
  if (bye<10)
    py.misc.exp+=60*(10-bye);
  get_money();
  put_stats();
  put_misc2();
  put_misc3();
  targetx=-128; /* No targeting yet */
  py.misc.realm=NONE;
  get_name();
  /* This delay may be reduced, but is recommended to keep players	*/
  /* from continuously rolling up characters, which can be VERY	*/
  /* expensive CPU wise.						*/
  pause_exit(23, PLAYER_EXIT_PAUSE);
}





