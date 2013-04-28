/* misc2.c: misc code for maintaining the dungeon, printing player info

   Copyright (c) 1989 James E. Wilson, Robert A. Koeneke

   This software may be copied and distributed for educational, research, and
   not for profit purposes provided that this copyright and statement are
   included in all such copies. */

#include "monster.h"

/* include before constant.h because param.h defines NULL incorrectly */
#ifndef USG
#include <sys/types.h>
#include <sys/param.h>
#endif

#include "constant.h"
#include "config.h"
#include "types.h"
#include "externs.h"

#include <ctype.h>

#ifdef USG
#ifndef ATARIST_MWC
#include <string.h>
#else
char *index();
#endif
#else
#include <strings.h>
#endif


#define BOTTOM 23


#if defined(LINT_ARGS)
static void prt_lnum(char *, int32, int, int);
static void prt_num(char *, int, int, int);
static void prt_long(int32, int, int);
static void prt_int(int, int, int);
#endif

static char *stat_names[] = { "STR: ", "INT: ", "WIS: ",
				 "DEX: ", "CON: ", "CHR: ","LUC: " };
#define BLANK_LENGTH	BOTTOM
static char blank_string[] = "                        ";

extern int peek;
extern int rating;
char *malloc();

/* Places a particular trap at location y, x		-RAK-	*/
void place_trap(y, x, subval)
int y, x, subval;
{
  register int cur_pos;

  cur_pos = popt();
  cave[y][x].tptr  = cur_pos;
  invcopy(&t_list[cur_pos], OBJ_TRAP_LIST + subval);
}


/* Returns a percent modifier for Luck stat */
int luck()
{
 int t,mod;
 t= py.stats.use_stat[A_LUC];
 if (t<4)
  mod=-30;
 else if (t<5)
  mod=-20;
 else if (t<6)
  mod=-10;
 else if (t<7)
  mod=-5;  
 else if (t<14)
  mod=0;
 else if (t<17)
  mod=(t-7)/3;
 else if (t<18)
  mod=(t-18)/2+10;
 else if (t<43)
  mod=(t-43)+15;
 else
  mod=(t-68)*3/2+20; /* REALLY lucky dude! */
 return mod;
}

/* Places rubble at location y, x			-RAK-	*/
void place_rubble(y, x)
int y, x;
{
  register int cur_pos;
  register cave_type *cave_ptr;

  cur_pos = popt();
  cave_ptr = &cave[y][x];
  cave_ptr->tptr = cur_pos;
  cave_ptr->fval = BLOCKED_FLOOR;
  invcopy(&t_list[cur_pos], OBJ_RUBBLE);
}


/* Places a treasure (Gold or Gems) at given row, column -RAK-	*/
void place_gold(y, x)
int y, x;
{
  register int i, cur_pos;
  register inven_type *t_ptr;
  int dun;
  long d;

  dun=dun_level;
  if (dun==-1)  dun=90;
  d=(dun/3+8);
  cur_pos = popt();
  i = ((randint(dun+2)+2) / 2) - 1;
  if (randint(OBJ_GREAT) == 1)
    i += randint(dun+1);
  if (i >= MAX_GOLD)
    i = MAX_GOLD - 1;
  cave[y][x].tptr = cur_pos;
  invcopy(&t_list[cur_pos], OBJ_GOLD_LIST+i);
  t_ptr = &t_list[cur_pos];
  t_ptr->cost += (d * (long)randint((int)t_ptr->cost)) + randint(8);
  if (cave[y][x].cptr == 1)
    msg_print ("You feel something roll beneath your feet.");
}


/* Returns the array number of a random object		-RAK-	*/
int get_obj_num(level, good)
int level, good;
{
  register int i, j;

  do {
    if (level == 0)
      i = randint(t_level[0]) - 1;
    else
      {
	if (level >= MAX_OBJ_LEVEL)
	  level = MAX_OBJ_LEVEL;
	else if (randint(OBJ_GREAT) == 1)
	  {
	    level = level * MAX_OBJ_LEVEL / randint (MAX_OBJ_LEVEL) + 1;
	    if (level > MAX_OBJ_LEVEL)
	      level = MAX_OBJ_LEVEL;
	  }

      /* This code has been added to make it slightly more likely to get the
	 higher level objects.	Originally a uniform distribution over all
	 objects less than or equal to the dungeon level.  This distribution
	 makes a level n objects occur approx 2/n% of the time on level n,
	 and 1/2n are 0th level. */

	if (randint(2) == 1)
	  i = randint(t_level[level]) - 1;
	else /* Choose three objects, pick the highest level. */
	  {
	    i = randint(t_level[level]) - 1;
	    j = randint(t_level[level]) - 1;
	    if (i < j)
	      i = j;
	    j = randint(t_level[level]) - 1;
	    if (i < j)
	      i = j;
	    j = object_list[sorted_objects[i]].level;
	    if (j == 0)
	      i = randint(t_level[0]) - 1;
	    else
	      i = randint(t_level[j]-t_level[j-1]) - 1 + t_level[j-1];
	  }
      }
  } while ((object_list[sorted_objects[i]].rare?
	   (randint(object_list[sorted_objects[i]].rare)-1) : 0) && !good);
  return(i);
}



int special_place_object(y, x)
  int y, x;
{
  register int cur_pos, tmp, luc;
  char str[100];
  int done=0;

  luc = luck()/5;
 again:
  if (done>20+luc) return 0;
  tmp = randint(MAX_OBJECTS-(SPECIAL_OBJ-1))+(SPECIAL_OBJ-1)-1;
  switch (tmp) {
  case (SPECIAL_OBJ-1):
    done++;
    if (randint(30-luc)>1) goto again;
    if (NARYA && NARYA != dun_level) goto again;
    if ((object_list[tmp].level-40)>dun_level) goto again;
    if ((object_list[tmp].level>dun_level) && (randint(50+luc)>1))
      goto again;
    if ((wizard||peek))
      sprintf(str,"Narya");
    else good_item_flag=TRUE;
    NARYA=dun_level;
    break;
  case (SPECIAL_OBJ):
    done++;
    if (randint(35-luc)>1) goto again;
    if (NENYA && NENYA != dun_level) goto again;
    if ((object_list[tmp].level-40)>dun_level) goto again;
    if ((object_list[tmp].level>dun_level) && (randint(60+luc)>1))
      goto again;
    if ((wizard||peek))
      sprintf(str,"Nenya");
    else good_item_flag=TRUE;
    NENYA=dun_level;
    break;
  case (SPECIAL_OBJ+1):
    done++;
    if (randint(40-luc*2)>1) goto again;
    if (VILYA && VILYA != dun_level) goto again;
    if ((object_list[tmp].level-40)>dun_level) goto again;
    if ((object_list[tmp].level>dun_level) && (randint(70+luc)>1))
      goto again;
    if ((wizard||peek))
      sprintf(str,"Vilya");
    else good_item_flag=TRUE;
    VILYA=dun_level;
    break;
  case (SPECIAL_OBJ+2):
    done++;
    if (randint(60-luc*3)>1) goto again;
    if (POWER && POWER != dun_level) goto again;
    if ((object_list[tmp].level-40)>dun_level) goto again;
    if ((object_list[tmp].level>dun_level) && (randint(100+luc)>1))
      goto again;
    if ((wizard||peek))
      sprintf(str,"Power (The One Ring)");
    else good_item_flag=TRUE;
    POWER=dun_level;
    break;
  case (SPECIAL_OBJ+3):
    done++;
    if (PHIAL && PHIAL != dun_level) goto again;
    if ((object_list[tmp].level-40)>dun_level) goto again;
    if ((object_list[tmp].level>dun_level) && (randint(30+luc)>1))
      goto again;
    if ((wizard||peek))
      sprintf(str,"Phial of Galadriel");
    else good_item_flag=TRUE;
    PHIAL=dun_level;
    break;
  case (SPECIAL_OBJ+4):
    done++;
    if (randint(10-luc/2)>1) goto again;
    if (INGWE && INGWE != dun_level) goto again;
    if ((object_list[tmp].level-40)>dun_level) goto again;
    if ((object_list[tmp].level>dun_level) && (randint(50+luc)>1))
      goto again;
    if ((wizard||peek))
      sprintf(str,"Amulet of Ingwe");
    else good_item_flag=TRUE;
    INGWE=dun_level;
    break;
  case (SPECIAL_OBJ+5):
    done++;
    if (randint(6)>1) goto again;
    if (CARLAMMAS && CARLAMMAS != dun_level) goto again;
    if ((object_list[tmp].level-40)>dun_level) goto again;
    if ((object_list[tmp].level>dun_level) && (randint(35+luc)>1))
      goto again;
    if ((wizard||peek))
      sprintf(str,"Amulet of Carlammas");
    else good_item_flag=TRUE;
    CARLAMMAS=dun_level;
    break;
  case (SPECIAL_OBJ+6):
    done++;
    if (randint(8)>1) goto again;
    if (ELENDIL && ELENDIL != dun_level) goto again;
    if ((object_list[tmp].level-40)>dun_level) goto again;
    if ((object_list[tmp].level>dun_level) && (randint(30+luc)>1))
      goto again;
    if ((wizard||peek))
      sprintf(str,"Star of Elendil");
    else good_item_flag=TRUE;
    ELENDIL=dun_level;
    break;
  case (SPECIAL_OBJ+7):
    done++;
    if (randint(18)>1) goto again;
    if (THRAIN && THRAIN!=dun_level) goto again;
    if ((object_list[tmp].level-40)>dun_level) goto again;
    if ((object_list[tmp].level>dun_level) && (randint(60+luc)>1))
      goto again;
    if ((wizard||peek))
      sprintf(str,"Arkenstone of Thrain");
    else good_item_flag=TRUE;
    THRAIN=dun_level;
    break;
  case (SPECIAL_OBJ+8):
    done++;
    if (randint(25)>1) goto again;
    if (TULKAS && TULKAS!=dun_level) goto again;
    if ((object_list[tmp].level-40)>dun_level) goto again;
    if ((object_list[tmp].level>dun_level) && (randint(65+luc)>1))
      goto again;
    if ((wizard||peek))
      sprintf(str,"Ring of Tulkas");
    else good_item_flag=TRUE;
    TULKAS=dun_level;
    break;
  case (SPECIAL_OBJ+9):
    done++;
    if (randint(25)>1) goto again;
    if (NECKLACE && NECKLACE!=dun_level) goto again;
    if ((object_list[tmp].level-40)>dun_level) goto again;
    if ((object_list[tmp].level>dun_level) && (randint(60+luc)>1))
      goto again;
    if ((wizard||peek))
      sprintf(str,"Necklace of the Dwarves");
    else good_item_flag=TRUE;
    NECKLACE=dun_level;
    break;
  case (SPECIAL_OBJ+10):
    done++;
    if (randint(20)>1) goto again;
    if (BARAHIR && BARAHIR!=dun_level) goto again;
    if ((object_list[tmp].level-40)>dun_level) goto again;
    if ((object_list[tmp].level>dun_level) && (randint(50+luc)>1))
      goto again;
    if ((wizard||peek))
      sprintf(str,"Ring of Barahir");
    else good_item_flag=TRUE;
    BARAHIR=dun_level;
    break;
  }
  if (strlen(str)>0 && (wizard || peek)) msg_print(str);
  cur_pos = popt();
  cave[y][x].tptr = cur_pos;
  invcopy(&t_list[cur_pos], tmp);
  t_list[cur_pos].timeout=0;
  if (object_list[tmp].level > dun_level) {
    rating += 2*(object_list[sorted_objects[tmp]].level - dun_level);
  }
  if (cave[y][x].cptr == 1)
    msg_print ("You feel something roll beneath your feet.");
  return (-1);
}

/* Places a specific type of object at a given row, column and level of
object */
void place_general(y,x,tval,lvl)
int y,x,lvl;
int tval;
{
  register int cur_pos, tmp, dun_lev, charges;

  cur_pos = popt();
  cave[y][x].tptr = cur_pos;
  dun_lev=lvl; /* Level of "droppped" object */
  tmp = 0;
  while (object_list[sorted_objects[tmp]].tval!=tval)
    tmp = get_obj_num(dun_lev, FALSE);
  invcopy(&t_list[cur_pos], sorted_objects[tmp]);
  if (tval==TV_WAND || tval==TV_STAFF)
    {
      charges=randint(lvl/2)+5;
      if (charges<0) charges=-charges;
      t_list[cur_pos].p1=charges;
    }
  if (tval==TV_ROD)
    t_list[cur_pos].p1=0;
  if (cave[y][x].cptr == 1)
    msg_print ("You feel something roll beneath your feet.");
}

/* Places an object at given row, column co-ordinate	-RAK-	*/
void place_object(y, x)
int y, x;
{
  register int cur_pos, tmp, luc, dun_lev;

  luc = luck()/10;
  if (luc>8)
   luc=8;
  if (randint(MAX_OBJECTS)>SPECIAL_OBJ && randint(10-luc)<=1)
    if (special_place_object(y,x)==(-1))
      return;
  cur_pos = popt();
  cave[y][x].tptr = cur_pos;
  dun_lev=dun_level;
  if (dun_lev==-1)
    dun_lev=70;
  tmp = get_obj_num(dun_lev, FALSE);
  invcopy(&t_list[cur_pos], sorted_objects[tmp]);
  magic_treasure(cur_pos, dun_lev, FALSE, 0);
  if (object_list[sorted_objects[tmp]].level > dun_lev)
    rating += object_list[sorted_objects[tmp]].level - dun_lev;
  if (peek) {
    if (object_list[sorted_objects[tmp]].level > dun_lev) {
      char buf[200];
      int8u tmp;

      tmp=t_list[cur_pos].ident;
      t_list[cur_pos].ident |= ID_STOREBOUGHT;
      objdes(buf, &t_list[cur_pos], TRUE);
      t_list[cur_pos].ident = tmp;
      msg_print(buf);
    }
  }
  if (cave[y][x].cptr == 1)
    msg_print ("You feel something roll beneath your feet.");
}

/* Places a GOOD-object at given row, column co-ordinate ~Ludwig*/
void place_special(y, x, good)
int y, x;
int32u good;
{
  register int cur_pos, tmp, luc;

  luc=luck();
  if (luc>8)
   luc=8;
  if (randint(10-luc)<=1)
    if (special_place_object(y,x)==(-1)) return;
  cur_pos = popt();
  cave[y][x].tptr = cur_pos;
  do {
    tmp = get_obj_num((dun_level+20), TRUE);
  } while (object_list[sorted_objects[tmp]].tval != TV_SOFT_ARMOR &&
	   object_list[sorted_objects[tmp]].tval != TV_ROBE &&
	   object_list[sorted_objects[tmp]].tval != TV_HELM &&
	   object_list[sorted_objects[tmp]].tval != TV_SHIELD &&
	   object_list[sorted_objects[tmp]].tval != TV_CLOAK &&
	   object_list[sorted_objects[tmp]].tval != TV_HAFTED &&
	   object_list[sorted_objects[tmp]].tval != TV_SWORD &&
	   object_list[sorted_objects[tmp]].tval != TV_POLEARM &&
	   object_list[sorted_objects[tmp]].tval != TV_BOW &&
	   object_list[sorted_objects[tmp]].tval != TV_BOLT &&
	   object_list[sorted_objects[tmp]].tval != TV_ARROW &&
	   object_list[sorted_objects[tmp]].tval != TV_HARD_ARMOR &&
	   object_list[sorted_objects[tmp]].tval != TV_BOOTS &&
	   object_list[sorted_objects[tmp]].tval != TV_GLOVES);
  invcopy(&t_list[cur_pos], sorted_objects[tmp]);
  magic_treasure(cur_pos, dun_level, (good & SPECIAL)?666:1,0);
  if (peek) {
    if (object_list[sorted_objects[tmp]].level > dun_level) {
      char buf[200];
      int8u tmp;

      tmp=t_list[cur_pos].ident;
      t_list[cur_pos].ident |= ID_STOREBOUGHT;
      objdes(buf, &t_list[cur_pos], TRUE);
      t_list[cur_pos].ident = tmp;
      msg_print(buf);
    }
  }
  if (cave[y][x].cptr == 1)
    msg_print ("You feel something roll beneath your feet.");
}


/* Allocates an object for tunnels and rooms		-RAK-	*/
void alloc_object(alloc_set, typ, num)
int (*alloc_set)();
int typ, num;
{
  register int i, j, k;

  for (k = 0; k < num; k++)
    {
      do
	{
	  i = randint(cur_height) - 1;
	  j = randint(cur_width) - 1;
	}
      /* don't put an object beneath the player, this could cause problems
	 if player is standing under rubble, or on a trap */
      while ((!(*alloc_set)(cave[i][j].fval)) ||
	     (cave[i][j].tptr != 0) || (i == char_row && j == char_col));
      if (typ < 4) {	/* typ == 2 not used, used to be visible traps */
	if (typ == 1) place_trap(i, j, randint(MAX_TRAP)-1); /* typ == 1 */
	else	      place_rubble(i, j); /* typ == 3 */
      } else {
	if (typ == 4) place_gold(i, j); /* typ == 4 */
	else	      place_object(i, j); /* typ == 5 */
      }
    }
}


/* Creates objects nearby the coordinates given		-RAK-	*/
void random_object(y, x, num)
int y, x, num;
{
  register int i, j, k;
  register cave_type *cave_ptr;

  do
    {
      i = 0;
      do
	{
	  j = y - 3 + randint(5);
	  k = x - 4 + randint(7);
	  cave_ptr = &cave[j][k];
	  if ((cave_ptr->fval <= MAX_CAVE_FLOOR) && (cave_ptr->tptr == 0))
	    {
	      if (randint(100) < 75)
		place_object(j, k);
	      else
		place_gold(j, k);
	      i = 9;
	    }
	  i++;
	}
      while(i <= 10);
      num--;
    }
  while (num != 0);
}

void special_random_object(y, x, num)
int y, x, num;
{
  register int i, j, k;
  register cave_type *cave_ptr;

  do
    {
      i = 0;
      do
	{
	  j = y - 3 + randint(5);
	  k = x - 4 + randint(7);
	  cave_ptr = &cave[j][k];
	  if ((cave_ptr->fval <= MAX_CAVE_FLOOR) && (cave_ptr->tptr == 0))
	    {
	      if (randint(5)==1) {
		if (!special_place_object(j, k))
		  place_special(j, k, SPECIAL);
	      } else {
		place_special(j, k, SPECIAL);
	      }
	      i = 9;
	    }
	  i++;
	}
      while(i <= 10);
      num--;
    }
  while (num != 0);
}

/* Converts stat num into string			-RAK-	*/
void cnv_stat(stat, out_val)
int16u stat;
char *out_val;
{
  register int part1, part2;

  if (stat > 18)
    {
      part1 = 18;
      part2 = stat - 18;
      if (part2 >= 220)
	(void) sprintf(out_val, "%2d/*** ", part1);
      else if (part2 >= 100)
	(void) sprintf(out_val, "%2d/%03d ", part1, part2);
      else
	(void) sprintf(out_val, " %2d/%02d ", part1, part2);
    }
  else
    (void) sprintf(out_val, "%6d ", stat);
}


/* Print character stat in given row, column		-RAK-	*/
void prt_stat(stat)
  int stat;
{
  stat_type out_val1;

  cnv_stat(py.stats.use_stat[stat], out_val1);
  put_buffer(stat_names[stat], 5+stat, STAT_COLUMN);
  put_buffer (out_val1, 5+stat, STAT_COLUMN+6);
}


/* Print character info in given row, column		-RAK-	*/
/* the longest title is 13 characters, so only pad to 13 */
void prt_field(info, row, column)
char *info;
int row, column;
{
  put_buffer (&blank_string[BLANK_LENGTH-13], row, column);
  put_buffer (info, row, column);
}

/* Print long number with header at given row, column */
static void prt_lnum(header, num, row, column)
char *header;
int32 num;
int row, column;
{
  vtype out_val;

  (void) sprintf(out_val, "%s:%8ld", header, num);
  put_buffer(out_val, row, column);
}

/* Print number with header at given row, column	-RAK-	*/
static void prt_num(header, num, row, column)
char *header;
int num, row, column;
{
  vtype out_val;

  (void) sprintf(out_val, "%s:  %6d", header, num);
  put_buffer(out_val, row, column);
}

/* Print long number at given row, column */
static void prt_long(num, row, column)
int32 num;
int row, column;
{
  vtype out_val;

  (void) sprintf(out_val, "%6ld", num);
  put_buffer(out_val, row, column);
}

/* Print number at given row, column	-RAK-	*/
static void prt_int(num, row, column)
int num, row, column;
{
  vtype out_val;

  (void) sprintf(out_val, "%6d", num);
  put_buffer(out_val, row, column);
}


/* Adjustment for wisdom/intelligence				-JWT-	*/
int stat_adj(stat)
int stat;
{
  register int value;

  value = py.stats.use_stat[stat];
  if (value > 228)
    return(20);
  else if (value > 218)
    return(18);
  else if (value > 198)
    return(16);
  else if (value > 188)
    return(15);
  else if (value > 178)
    return(14);
  else if (value > 168)
    return(13);
  else if (value > 158)
    return(12);
  else if (value > 148)
    return(11);
  else if (value > 138)
    return(10);
  else if (value > 128)
    return(9);
  else if (value > 118)
    return(8);
  else if (value == 118)
    return(7);
  else if (value > 107)
    return(6);
  else if (value > 87)
    return(5);
  else if (value > 67)
    return(4);
  else if (value > 17)
    return(3);
  else if (value > 14)
    return(2);
  else if (value > 7)
    return(1);
  else
    return(0);
}


/* Adjustment for charisma				-RAK-	*/
/* Percent decrease or increase in price of goods		 */
int chr_adj()
{
  register int charisma;

  charisma = py.stats.use_stat[A_CHR];
  if (charisma > 217)
    return(80);
  else if (charisma > 187)
    return(86);
  else if (charisma > 147)
    return(88);
  else if (charisma > 117)
    return(90);
  else if (charisma > 107)
    return(92);
  else if (charisma > 87)
    return(94);
  else if (charisma > 67)
    return(96);
  else if (charisma > 18)
    return(98);
  else
    switch(charisma)
      {
      case 18:	return(100);
      case 17:	return(101);
      case 16:	return(102);
      case 15:	return(103);
      case 14:	return(104);
      case 13:	return(106);
      case 12:	return(108);
      case 11:	return(110);
      case 10:	return(112);
      case 9:  return(114);
      case 8:  return(116);
      case 7:  return(118);
      case 6:  return(120);
      case 5:  return(122);
      case 4:  return(125);
      case 3:  return(130);
      default: return(100);
      }
}


/* Returns a character's adjustment to hit points	 -JWT-	 */
int con_adj()
{
  register int con;

  con = py.stats.use_stat[A_CON];
  if (con < 7)
    return(con - 7);
  else if (con < 17)
    return(0);
  else if (con ==  17)
    return(1);
  else if (con <  94)
    return(2);
  else if (con < 117)
    return(3);
  else if (con < 119)
    return(4);
  else if (con < 128)
    return(5);
  else if (con < 138)
    return(6);
  else if (con < 158)
    return(7);
  else if (con < 168)
    return(8);
  else if (con < 178)
    return(9);
  else if (con < 188)
    return(10);
  else if (con < 198)
    return(11);
  else if (con < 208)
    return(12);
  else if (con < 228)
    return(13);
  else
    return(14);
}


char *title_string()
{
  register char *p;
  register int lvl;
  lvl=get_level();
  if (lvl < 1)
    p = "Babe in arms";
  else if (lvl<10)
    p = "Novice Player";
  else if (lvl<20)
    p = "Average Player";
  else if (lvl<40)
    p = "Expert Player";
  else if (lvl<=50)
    p = "Master";
  else if (py.misc.male)
    p = "**KING**";
  else
    p = "**QUEEN**";
  return p;
}


/* Prints title of character				-RAK-	*/
void prt_title()
{
}


/* Prints level						-RAK-	*/
void prt_level()
{
  prt_int(get_level(), 13, STAT_COLUMN+6);
}


/* Prints players current mana points.		 -RAK-	*/
void prt_cmana()
{
  prt_int(py.misc.cmana, 15, STAT_COLUMN+6);
}


/* Prints Max hit points				-RAK-	*/
void prt_mhp()
{
  prt_int(py.misc.mhp, 16, STAT_COLUMN+6);
}


/* Prints players current hit points			-RAK-	*/
void prt_chp()
{
  prt_int(py.misc.chp, 17, STAT_COLUMN+6);
}


/* prints current AC					-RAK-	*/
void prt_pac()
{
  prt_int(py.misc.dis_ac, 18, STAT_COLUMN+6);
}


/* Prints current gold					-RAK-	*/
void prt_gold()
{
  prt_long(py.misc.au, 19, STAT_COLUMN+6);
}


/* Prints depth in stat area				-RAK-	*/
void prt_depth()
{
  char depths[12];
  register int depth;

  depth = dun_level*50;
  if (depth < 0)
    (void) strcpy(depths,  "  Quest   ");
  else if (depth == 0)
    (void) strcpy(depths,  "* Town >  ");
  else
    (void) sprintf(depths, "< %d ft >", depth);
  put_buffer(depths, 4, 0);
}


/* Prints status of hunger				-RAK-	*/
void prt_hunger()
{
  if (PY_WEAK & py.flags.status)
    put_buffer("Weak  ", BOTTOM, 0);
  else if (PY_HUNGRY & py.flags.status)
    put_buffer("Hungry", BOTTOM, 0);
  else
    put_buffer("      ", BOTTOM, 0);
}


/* Prints Blind status					-RAK-	*/
void prt_blind()
{
  if (PY_BLIND & py.flags.status)
    put_buffer("Blind", BOTTOM, 7);
  else
    put_buffer("     ", BOTTOM, 7);
}


/* Prints Confusion status				-RAK-	*/
void prt_confused()
{
  if (PY_CONFUSED & py.flags.status)
    put_buffer("Confused", BOTTOM, 13);
  else
    put_buffer("        ", BOTTOM, 13);
}


/* Prints Fear status					-RAK-	*/
void prt_afraid()
{
  if (PY_FEAR & py.flags.status)
    put_buffer("Afraid", BOTTOM, 22);
  else
    put_buffer("      ", BOTTOM, 22);
}


/* Prints Poisoned status				-RAK-	*/
void prt_poisoned()
{
  if (PY_POISONED & py.flags.status)
    put_buffer("Poisoned", BOTTOM, 29);
  else
    put_buffer("        ", BOTTOM, 29);
}


/* Prints Searching, Resting, Paralysis, or 'count' status	-RAK-	*/
void prt_state()
{
  char tmp[16];

  py.flags.status &= ~PY_REPEAT;
  if (py.flags.paralysis > 1)
    put_buffer ("Paralysed ", BOTTOM, 38);
  else if (PY_REST & py.flags.status)
    {
      if (py.flags.rest>0)
	(void) sprintf (tmp, "Rest %-5d", py.flags.rest);
      else if (py.flags.rest==-1)
	(void) sprintf (tmp, "Rest *****");
      put_buffer (tmp, BOTTOM, 38);
    }
  else if (command_count > 0)
    {
      (void) sprintf (tmp, "Repeat %-3d", command_count);
      py.flags.status |= PY_REPEAT;
      put_buffer (tmp, BOTTOM, 38);
      if (PY_SEARCH & py.flags.status)
	put_buffer ("Search    ", BOTTOM, 38);
    }
  else if (PY_SEARCH & py.flags.status)
    put_buffer("Searching ", BOTTOM, 38);
  else		/* "repeat 999" is 10 characters */
    put_buffer("          ", BOTTOM, 38);
}


/* Prints the speed AND invisibility of a character.		  -CJS- */
void prt_speed ()
{
  register int i;

  i = py.flags.speed;
  if (PY_SEARCH & py.flags.status)   /* Search mode. */
    i--;
  if (i > 2)
    put_buffer ("Extremely Slow", BOTTOM, 49);
  else if (i == 2)
    put_buffer ("Very Slow     ", BOTTOM, 49);
  else if (i == 1)
    put_buffer ("Slow          ", BOTTOM, 49);
  else if (i == 0)
    put_buffer ("              ", BOTTOM, 49);
  else if (i == -1)
    put_buffer ("Fast          ", BOTTOM, 49);
  else if (i == -2)
    put_buffer ("Very Fast     ", BOTTOM, 49);
  else if (i == -3)
    put_buffer ("Extremely Fast", BOTTOM, 49);
  else if (i == -4)
    put_buffer ("Deadly Speed  ", BOTTOM, 49);
  else
    put_buffer ("Light Speed   ", BOTTOM, 49);
  if (py.flags.tim_invis)
    put_buffer("Invisible",BOTTOM,69);
  else
    put_buffer("         ",BOTTOM,69);
  if (py.flags.dodge)
    put_buffer("Dodging",BOTTOM,30);
  else
    put_buffer("       ",BOTTOM,30);
}


void prt_study()
{
  py.flags.status &= ~PY_STUDY;
  if (py.flags.new_spells == 0)
    put_buffer ("     ", BOTTOM, 63);
  else
    put_buffer ("Study", BOTTOM, 63);
}

void cut_player(c)
  int c;
{
  py.flags.cut+=c;
  c=py.flags.cut;
  if (c>5000)
    msg_print("You have been given a mortal wound.");
  else if (c>900)
    msg_print("You have been given a deep gash.");
  else if (c>200)
    msg_print("You have been given a severe cut.");
  else if (c>100)
    msg_print("You have been given a nasty cut.");
  else if (c>50)
    msg_print("You have been given a bad cut.");
  else if (c>10)
    msg_print("You have been given a light cut.");
  else if (c>0)
    msg_print("You have been given a graze.");
}

void prt_cut()
{
  int c=py.flags.cut;

  if (c>900)
    put_buffer("Mortal wound", 22, 0);
  else if (c>300)
    put_buffer("Deep gash   ", 22, 0);
  else if (c>200)
    put_buffer("Severe cut  ", 22, 0);
  else if (c>45)
    put_buffer("Nasty cut   ", 22, 0);
  else if (c>15)
    put_buffer("Bad cut     ", 22, 0);
  else if (c>5)
    put_buffer("Light cut   ", 22, 0);
  else if (c>0)
    put_buffer("Graze       ", 22, 0);
  else
    put_buffer("            ", 22, 0);
}

void stun_player(s)
  int s;
{
  int t;

  if (!py.flags.sound_resist) {
    t=py.flags.stun;
    py.flags.stun+=s;
    s=py.flags.stun;
    if (s>100) {
      msg_print("You have been knocked out.");
      if (t==0) {
        py.misc.ptohit-=20;
        py.misc.ptodam-=20;
      } else if (t<=50) {
        py.misc.ptohit-=15;
        py.misc.ptodam-=15;
      }
    } else if (s>50) {
      msg_print("You've been heavily stunned.");
      if (t==0) {
        py.misc.ptohit-=20;
        py.misc.ptodam-=20;
      } else if (t<=50) {
        py.misc.ptohit-=15;
        py.misc.ptodam-=15;
      }
    } else if (s>0) {
      msg_print("You've been stunned.");
      if (t==0) {
        py.misc.ptohit-=5;
        py.misc.ptodam-=5;
      }
    }
  }
}

void prt_stun()
{
  int s=py.flags.stun;

  if (!py.flags.sound_resist) {
    if (s>100)
      put_buffer("Knocked out ", 22, 0);
    else if (s>50)
      put_buffer("Heavy stun  ", 22, 0);
    else if (s>0)
      put_buffer("Stun        ", 22, 0);
    else
      put_buffer("            ", 22, 0);
  }
}

/* Prints winner status on display			-RAK-	*/
void prt_winner()
{
  if (wizard)
    put_buffer("Wizard", 20, 0);
  else if (total_winner)
    put_buffer("Winner", 20, 0);
  else
    put_buffer("       ", 20, 0);
}


int16u modify_stat (stat, amount)
int stat;
int16 amount;
{
  register int loop, i;
  register int16u tmp_stat;

  tmp_stat = py.stats.cur_stat[stat];
  loop = (amount < 0 ? -amount : amount);
  for (i = 0; i < loop; i++)
    {
      if (amount > 0)
	{
	  if (tmp_stat < 18)
	    tmp_stat++;
	  else
	    tmp_stat += 10;
	}
      else
	{
	  if (tmp_stat > 27)
	    tmp_stat -= 10;
	  else if (tmp_stat > 18)
	    tmp_stat = 18;
	  else if (tmp_stat > 3)
	    tmp_stat--;
	}
    }
  return tmp_stat;
}


/* Set the value of the stat which is actually used.	 -CJS- */
void set_use_stat(stat)
int stat;
{
  py.stats.use_stat[stat] = modify_stat (stat, py.stats.mod_stat[stat]);
  if (stat == A_STR)
    {
      py.flags.status |= PY_STR_WGT;
      calc_bonuses();
    } 
  else if (stat == A_DEX)
    calc_bonuses();
  else if (stat == prime_stat[py.misc.realm] && py.misc.realm!=NONE)
    {
      calc_spells(prime_stat[py.misc.realm]);
      calc_mana(prime_stat[py.misc.realm]);
    }
  else if (stat == A_CON)
    calc_hitpoints();
}


/* Increases a stat by one randomized level		-RAK-	*/
int inc_stat(stat)
register int stat;
{
  register int tmp_stat, gain;

  tmp_stat = py.stats.cur_stat[stat];
  if (tmp_stat < 118) {
    if (tmp_stat < 18)
      tmp_stat+=randint(2);
    else if (tmp_stat < 116) {
      /* stat increases by 1/6 to 1/3 of difference from max */
      gain = ((118 - tmp_stat)/2 + 3) >> 1;
      tmp_stat += randint(gain) + gain/2;
      if (tmp_stat>117) tmp_stat=117;
    } else
      tmp_stat++;

    py.stats.cur_stat[stat] = tmp_stat;
    if (tmp_stat > py.stats.max_stat[stat])
      py.stats.max_stat[stat] = tmp_stat;
    set_use_stat (stat);
    prt_stat (stat);
    return TRUE;
  }
  else
    return FALSE;
}


/* Decreases a stat by one randomized level		-RAK-	*/
int dec_stat(stat)
register int stat;
{
  register int tmp_stat, loss;

  tmp_stat = py.stats.cur_stat[stat];
  if (tmp_stat > 3)
    {
      if (tmp_stat < 19)
	tmp_stat--;
      else if (tmp_stat < 117)
	{
	  loss = (((118 - tmp_stat) >> 1) + 1) >> 1;
	  tmp_stat += -randint(loss) - loss;
	  if (tmp_stat < 18)
	    tmp_stat = 18;
	}
      else
	tmp_stat--;

      py.stats.cur_stat[stat] = tmp_stat;
      set_use_stat (stat);
      prt_stat (stat);
      return TRUE;
    }
  else
    return FALSE;
}


/* Restore a stat.  Return TRUE only if this actually makes a difference. */
int res_stat (stat)
int stat;
{
  register int i;

  i = py.stats.max_stat[stat] - py.stats.cur_stat[stat];
  if (i)
    {
      py.stats.cur_stat[stat] += i;
      set_use_stat (stat);
      prt_stat (stat);
      return TRUE;
    }
  return FALSE;
}

/* Boost a stat artificially (by wearing something). If the display argument
   is TRUE, then increase is shown on the screen. */
void bst_stat (stat, amount)
int stat, amount;
{
  py.stats.mod_stat[stat] += amount;

  set_use_stat (stat);
  /* can not call prt_stat() here, may be in store, may be in inven_command */
  py.flags.status |= (PY_STR << stat);
}


/* Returns a character's adjustment to hit.		 -JWT-	 */
int tohit_adj()
{
  register int total, stat;

  stat = py.stats.use_stat[A_DEX];
  if	  (stat <   4)	total = -3;
  else if (stat <   6)	total = -2;
  else if (stat <   8)	total = -1;
  else if (stat <  16)	total =	 0;
  else if (stat <  17)	total =	 1;
  else if (stat <  18)	total =	 2;
  else if (stat <  69)	total =	 3;
  else if (stat < 118)	total =	 4;
  else if (stat ==118)	total =	 5;
  else if (stat < 128)	total =	 6;
  else if (stat < 138)	total =	 7;
  else if (stat < 148)	total =	 8;
  else if (stat < 158)	total =	 9;
  else if (stat < 168)	total =	10;
  else if (stat < 178)	total =	11;
  else if (stat < 188)	total =	12;
  else if (stat < 198)	total =	13;
  else if (stat < 218)  total = 14;
  else if (stat < 228)  total = 15;
  else total = 17;
  stat = py.stats.use_stat[A_STR];
  if	  (stat <   4)	total -= 3;
  else if (stat <   5)	total -= 2;
  else if (stat <   7)	total -= 1;
  else if (stat <  18)	total -= 0;
  else if (stat <  94)	total += 1;
  else if (stat < 109)	total += 2;
  else if (stat < 117)	total += 3;
  else if (stat < 119)	total += 4;
  else if (stat < 128)	total += 5;
  else if (stat < 138)	total += 6;
  else if (stat < 148)	total += 7;
  else if (stat < 158)	total += 8;
  else if (stat < 168)	total += 9;
  else if (stat < 178)	total +=10;
  else if (stat < 188)	total +=11;
  else if (stat < 198)	total +=12;
  else if (stat < 218)  total +=13;
  else if (stat < 228)  total +=14;
  else total += 16;
  total+=py.flags.tohit;
  return(total);
}


/* Returns a character's adjustment to armor class	 -JWT-	 */
int toac_adj()
{
  register int stat;
  register int tmp;

  stat = py.stats.use_stat[A_DEX];
  if	  (stat <   4)	tmp=-4;
  else if (stat ==  4)	tmp=-3;
  else if (stat ==  5)	tmp=-2;
  else if (stat ==  6)	tmp=-1;
  else if (stat <  15)	tmp=0;
  else if (stat <  18)	tmp=1;
  else if (stat <  59)	tmp=2;
  else if (stat <  94)	tmp=3;
  else if (stat < 117)	tmp=4;
  else if (stat <=118)	tmp=5;
  else if (stat < 128)	tmp=6;
  else if (stat < 138)	tmp=7;
  else if (stat < 148)	tmp=8;
  else if (stat < 158)	tmp=9;
  else if (stat < 168)  tmp=10;
  else if (stat < 178)  tmp=11;
  else if (stat < 188)	tmp=12;
  else if (stat < 198)	tmp=13;
  else if (stat < 218)  tmp=14;
  else if (stat < 228)  tmp=15;
  else                  tmp=17;
  tmp+=py.flags.ac_mod;
  return tmp;
}


/* Returns a character's adjustment to disarm		 -RAK-	 */
int todis_adj()
{
  register int stat,tmp,t2;

  stat = py.stats.use_stat[A_DEX];
  if	  (stat <   3)	tmp=(-16);
  else if (stat ==  4)	tmp=(-12);
  else if (stat ==  5)	tmp=(-8);
  else if (stat ==  6)	tmp=(-4);
  else if (stat ==  7)	tmp=(-2);
  else if (stat <  13)	tmp=( 0);
  else if (stat <  16)	tmp=( 2);
  else if (stat <  18)	tmp=( 4);
  else if (stat <  59)	tmp=( 8);
  else if (stat <  94)	tmp=( 10);
  else if (stat < 117)	tmp=( 12);
  else                  tmp=( 16); /* All are DOUBLED */
  /* Skill for disarming here */
  t2=py.skills.cur_skill[S_DISARM];
  if (t2<30)
    tmp-=(30-t2)/4;
  else if (t2<50)
    tmp+=(t2-30)/4;
  else
    tmp+=(t2-50)/3;
  return tmp;
}


/* Returns a character's adjustment to damage		 -JWT-	 */
int todam_adj()
{
  register int stat,tmp;

  stat = py.stats.use_stat[A_STR];
  if	  (stat <   4)	tmp=-2;
  else if (stat <   5)	tmp=-1;
  else if (stat <  16)	tmp= 0;
  else if (stat <  17)	tmp= 1;
  else if (stat <  18)	tmp= 2;
  else if (stat <  94)	tmp= 3;
  else if (stat < 109)	tmp= 4;
  else if (stat < 117)	tmp= 5;
  else if (stat <=118)	tmp= 5;
  else if (stat < 128)	tmp= 6;
  else if (stat < 138)	tmp= 7;
  else if (stat < 148)	tmp= 8;
  else if (stat < 158)	tmp= 9;
  else if (stat < 168)	tmp=10;
  else if (stat < 178)	tmp=11;
  else if (stat < 188)	tmp=12;
  else if (stat < 198)	tmp=13;
  else if (stat < 218)  tmp=14;
  else if (stat < 228)  tmp=16;
  else          	tmp=20;
  tmp+=py.flags.todam;
  return tmp;
}


/* Prints character-screen info				-RAK-	*/
void prt_stat_block()
{
  register int32u status;
  register struct misc *m_ptr;
  register int i;

  m_ptr = &py.misc;
  prt_field(race[py.misc.prace].trace,	  1, STAT_COLUMN);
  for (i = 0; i < 7; i++)
    prt_stat (i);
  prt_num ("LEV", (int)get_level(),    13, STAT_COLUMN);
  prt_lnum("EXP", m_ptr->exp,	       14, STAT_COLUMN);
  prt_num ("MNA", m_ptr->cmana,	 15, STAT_COLUMN);
  prt_num ("MHP", m_ptr->mhp,	       16, STAT_COLUMN);
  prt_num ("CHP", m_ptr->chp,	 17, STAT_COLUMN);
  prt_num ("AC ", m_ptr->dis_ac,      18, STAT_COLUMN);
  prt_lnum("AU ", m_ptr->au,	       19, STAT_COLUMN);
  prt_winner();
  prt_cut();
  prt_stun();
  status = py.flags.status;
  if ((PY_HUNGRY|PY_WEAK) & status)
    prt_hunger();
  if (PY_BLIND & status)
    prt_blind();
  if (PY_CONFUSED & status)
    prt_confused();
  if (PY_FEAR & status)
    prt_afraid();
  if (PY_POISONED & status)
    prt_poisoned();
  if ((PY_SEARCH|PY_REST) & status)
    prt_state ();
  /* if speed non zero, print it, modify speed if Searching */
  if (py.flags.speed - ((PY_SEARCH & status) >> 8) != 0)
    prt_speed ();
}


/* Draws entire screen					-RAK-	*/
void draw_cave()
{
  clear_screen ();
  prt_stat_block();
  prt_map();
  prt_depth();
}


/* Prints the following information on the screen.	-JWT-	*/
void put_character()
{
  register struct misc *m_ptr;

  m_ptr = &py.misc;
  clear_screen ();
  put_buffer ("Name        :", 2, 1);
  put_buffer ("Race        :", 3, 1);
  put_buffer ("Sex         :", 4, 1);
  put_buffer ("Class       :", 5, 1);
  if (character_generated)
    {
      put_buffer (m_ptr->name, 2, 15);
      put_buffer (race[m_ptr->prace].trace, 3, 15);
      put_buffer ((m_ptr->male ? "Male" : "Female"), 4, 15);
    }
}


/* Prints the following information on the screen.	-JWT-	*/
void put_stats()
{
  register struct misc *m_ptr;
  register int i;
  vtype buf;

  m_ptr = &py.misc;
  for (i = 0; i < 7; i++)
    {
      cnv_stat (py.stats.use_stat[i], buf);
      put_buffer (stat_names[i], 2+i, 61);
      put_buffer (buf, 2+i, 66);
      if (py.stats.max_stat[i] > py.stats.cur_stat[i])
	{
	  cnv_stat (py.stats.max_stat[i], buf);
	  put_buffer (buf, 2+i, 73);
	}
    }
  prt_num("+ To Hit    ", m_ptr->dis_th, 10, 1);
  prt_num("+ To Damage ", m_ptr->dis_td, 11, 1);
  prt_num("+ To AC     ", m_ptr->dis_tac, 12, 1);
  prt_num("  Total AC  ", m_ptr->dis_ac, 13, 1);
}


/* Tmp=s a rating of x depending on y			-JWT-	*/
char *likert(x, y)
int x, y;
{
  switch((x/y))
    {
      case -3: case -2: case -1: return("Very Bad");
      case 0: case 1:		 return("Bad");
      case 2:			 return("Poor");
      case 3: case 4:		 return("Fair");
      case  5:			 return("Good");
      case 6:			 return("Very Good");
      case 7: case 8:		 return("Excellent");
      case 9: case 10:           return("Superb");
      case 11: case 12:          return("Powerful");
      case 13:                   return("Heroic");
      case 14: case 15:          return("Legendary");
      case 16: case 17:          return("Inhuman");
      default:			 return("Ungodly");
      }
}


/* Prints age, height, weight, and SC			-JWT-	*/
void put_misc1()
{
  register struct misc *m_ptr;

  m_ptr = &py.misc;
  prt_num("Age          ", (int)m_ptr->age, 2, 38);
  prt_num("Height       ", (int)m_ptr->ht, 3, 38);
  prt_num("Weight       ", (int)m_ptr->wt, 4, 38);
  prt_num("Social Class ", (int)m_ptr->sc, 5, 38);
}


/* Prints the following information on the screen.	-JWT-	*/
void put_misc2()
{
  register struct misc *m_ptr;

  m_ptr = &py.misc;
  prt_num("Level      ", (int)get_level(), 9, 29);
  prt_lnum("Experience ", m_ptr->exp, 10, 29);
  prt_lnum("Max Exp    ", m_ptr->max_exp, 11, 29);
  prt_lnum("Gold       ", m_ptr->au, 12, 29);
  prt_num("Max Hit Points ", m_ptr->mhp, 10, 52);
  prt_num("Cur Hit Points ", m_ptr->chp, 11, 52);
  prt_num("Max Mana       ", m_ptr->mana, 12, 52);
  prt_num("Cur Mana       ", m_ptr->cmana, 13, 52);
}


/* This used to put ratings on abilities, but this is now done via skills */
void put_misc3()
{  /* All this does now is print Infravision out */
  char out[80]; 
  sprintf(out,"Infra      :  %3d ft",py.flags.see_infra*10);
  prt(out,13,29);
  prt_num("Cur Mana       ", py.misc.cmana, 13, 52);
  return;
}


/* Used to display the character on the screen.		-RAK-	*/
void display_char()
{
  put_character();
  put_misc1();
  put_stats();
  put_misc2();
  put_misc3();
}


/* Gets a name for the character			-JWT-	*/
void get_name()
{
  char tmp[100];

  strcpy(tmp, py.misc.name);
  prt("Enter your player's name  [press <RETURN> when finished]", 21, 2);
  put_buffer (&blank_string[BLANK_LENGTH-23], 2, 15);
#ifdef MAC
  /* Force player to give a name, would be nice to get name from chooser
     (STR -16096), but that name might be too long */
  while (!get_string(py.misc.name, 2, 15, 23) || py.misc.name[0] == 0);
#else
  if (!get_string(py.misc.name, 2, 15, 23) || py.misc.name[0] == 0)
    {
      strcpy(py.misc.name, tmp);
      put_buffer (tmp, 2, 15);
    }
#endif
  clear_from (20);
#ifdef MAC
  /* Use the new name to set save file default name. */
  initsavedefaults();
#endif
}


/* Changes the name of the character			-JWT-	*/
void change_name()
{
  register char c;
  register int flag;
#ifndef MAC
  vtype temp;
#endif

  flag = FALSE;
  display_char();
  do
    {
      prt( "<f>ile character description. <c>hange character name.", 22, 2);
      c = inkey();
      switch(c)
	{
	case 'c':
	  get_name();
	  flag = TRUE;
	  break;
	case 'f':
#ifdef MAC
	  /* On mac, file_character() gets filename with std file dialog. */
	  if (file_character ())
	    flag = TRUE;
#else
	  prt ("File name:", 0, 0);
	  if (get_string (temp, 0, 10, 60) && temp[0])
	    if (file_character (temp))
	      flag = TRUE;
#endif
	  break;
	case ESCAPE: case ' ':
	case '\n': case '\r':
	  flag = TRUE;
	  break;
	default:
	  bell ();
	  break;
	}
    }
  while (!flag);
}


/* Destroy an item in the inventory			-RAK-	*/
void inven_destroy(item_val)
int item_val;
{
  register int j;
  register inven_type *i_ptr;

  i_ptr = &inventory[item_val];
  if ((i_ptr->number > 1) && (i_ptr->subval <= ITEM_SINGLE_STACK_MAX))
    {
      i_ptr->number--;
      inven_weight -= i_ptr->weight;
    }
  else
    {
      inven_weight -= i_ptr->weight*i_ptr->number;
      for (j = item_val; j < inven_ctr-1; j++)
	inventory[j] = inventory[j+1];
      invcopy(&inventory[inven_ctr-1], OBJ_NOTHING);
      inven_ctr--;
    }
  py.flags.status |= PY_STR_WGT;
}


/* Copies the object in the second argument over the first argument.
   However, the second always gets a number of one except for ammo etc. */
void take_one_item (s_ptr, i_ptr)
register inven_type *s_ptr, *i_ptr;
{
  *s_ptr = *i_ptr;
  if ((s_ptr->number > 1) && (s_ptr->subval >= ITEM_SINGLE_STACK_MIN)
      && (s_ptr->subval <= ITEM_SINGLE_STACK_MAX))
    s_ptr->number = 1;
}


/* Drops an item from inventory to given location	-RAK-	*/
void inven_drop(item_val, drop_all)
register int item_val, drop_all;
{
  int i;
  register inven_type *i_ptr;
  vtype prt2;
  bigvtype prt1;

  if (cave[char_row][char_col].tptr != 0)
    (void) delete_object(char_row, char_col);
  i = popt ();
  i_ptr = &inventory[item_val];
  t_list[i] = *i_ptr;
  cave[char_row][char_col].tptr = i;

  if (item_val >= INVEN_WIELD)
    takeoff (item_val, -1);
  else
    {
      if (drop_all || i_ptr->number == 1)
	{
	  inven_weight -= i_ptr->weight*i_ptr->number;
	  inven_ctr--;
	  while (item_val < inven_ctr)
	    {
	      inventory[item_val] = inventory[item_val+1];
	      item_val++;
	    }
	  invcopy(&inventory[inven_ctr], OBJ_NOTHING);
	}
      else
	{
	  t_list[i].number = 1;
	  inven_weight -= i_ptr->weight;
	  i_ptr->number--;
	}
      objdes (prt1, &t_list[i], TRUE);
      (void) sprintf (prt2, "Dropped %s", prt1);
      msg_print (prt2);
    }
  py.flags.status |= PY_STR_WGT;
}


/* Destroys a type of item on a given percent chance	-RAK-	*/
int inven_damage(typ, perc)
int (*typ)();
register int perc;
{
  register int i, j;

  j = 0;
  for (i = 0; i < inven_ctr; i++)
    if ((*typ)(&inventory[i]) && (randint(100) < perc))
      {
	inven_destroy(i);
	j++;
      }
  return(j);
}


/* Computes current weight limit			-RAK-	*/
int weight_limit()
{
  register int weight_cap;

  weight_cap = py.stats.use_stat[A_STR] * PLAYER_WEIGHT_CAP + py.misc.wt;
  if (weight_cap > 3500)  weight_cap = 3500;
  return(weight_cap);
}


/* this code must be identical to the inven_carry() code below */
int inven_check_num (t_ptr)
register inven_type *t_ptr;
{
  register int i;

  if (inven_ctr < INVEN_WIELD)
    return TRUE;
  else if (t_ptr->subval >= ITEM_SINGLE_STACK_MIN)
    for (i = 0; i < inven_ctr; i++)
      if (inventory[i].tval == t_ptr->tval &&
	  inventory[i].subval == t_ptr->subval &&
	  /* make sure the number field doesn't overflow */
	  ((int)inventory[i].number + (int)t_ptr->number < 256) &&
	  /* they always stack (subval < 192), or else they have same p1 */
	  ((t_ptr->subval < ITEM_GROUP_MIN) || (inventory[i].p1 == t_ptr->p1))
	  /* only stack if both or neither are identified */
	  && (known1_p(&inventory[i]) == known1_p(t_ptr)))
	return TRUE;
  return FALSE;
}

/* return FALSE if picking up an object would change the players speed */
int inven_check_weight(i_ptr)
register inven_type *i_ptr;
{
  register int i, new_inven_weight;

  i = weight_limit();
  new_inven_weight = i_ptr->number*i_ptr->weight + inven_weight;
  if (i < new_inven_weight)
    i = new_inven_weight / (i + 1);
  else
    i = 0;

  if (pack_heavy != i)
    return FALSE;
  else
    return TRUE;
}


/* Are we strong enough for the current pack and weapon?  -CJS-	 */
void check_strength()
{
  register int i;
  register inven_type *i_ptr;

  i_ptr = &inventory[INVEN_WIELD];
  if (i_ptr->tval != TV_NOTHING
      && (py.stats.use_stat[A_STR]*15 < i_ptr->weight))
    {
      if (weapon_heavy == FALSE)
	{
	  msg_print("You have trouble wielding such a heavy weapon.");
	  weapon_heavy = TRUE;
	  calc_bonuses();
	}
    }
  else if (weapon_heavy == TRUE)
    {
      weapon_heavy = FALSE;
      if (i_ptr->tval != TV_NOTHING)
	msg_print("You are strong enough to wield your weapon.");
      calc_bonuses();
    }
  i = weight_limit();
  if (i < inven_weight)
    i = inven_weight / (i+1);
  else
    i = 0;
  if (pack_heavy != i)
    {
      if (pack_heavy < i)
	msg_print("Your pack is so heavy that it slows you down.");
      else
	msg_print("You move more easily under the weight of your pack.");
      change_speed(i - pack_heavy);
      pack_heavy = i;
    }
  py.flags.status &= ~PY_STR_WGT;
}


/* Add an item to players inventory.  Return the	*/
/* item position for a description if needed.	       -RAK-   */
/* this code must be identical to the inven_check_num() code above */
int inven_carry(i_ptr)
register inven_type *i_ptr;
{
  register int locn, i;
  register int typ, subt;
  register inven_type *t_ptr;

  typ = i_ptr->tval;
  subt = i_ptr->subval;
  /* Now, check to see if player can carry object  */
  for (locn = 0; ; locn++)
    {
      t_ptr = &inventory[locn];
      if ((typ == t_ptr->tval) && (subt == t_ptr->subval)
	  && (subt >= ITEM_SINGLE_STACK_MIN) &&
	  ((int)t_ptr->number + (int)i_ptr->number < 256) &&
	  ((subt < ITEM_GROUP_MIN) || (t_ptr->p1 == i_ptr->p1)) &&
	  /* only stack if both or neither are identified */
	  (known1_p(i_ptr) == known1_p(t_ptr)))
	{
	  t_ptr->number += i_ptr->number;
	  break;
	}
      else if (typ > t_ptr->tval)
	{
	  for (i = inven_ctr - 1; i >= locn; i--)
	    inventory[i+1] = inventory[i];
	  inventory[locn] = *i_ptr;
	  inven_ctr++;
	  break;
	}
    }

  inven_weight += i_ptr->number*i_ptr->weight;
  py.flags.status |= PY_STR_WGT;
  return locn;
}


/* Returns spell chance of failure for spell		-RAK-	*/
int spell_chance(spell)
int spell;
{
  register spell_type *s_ptr;
  register int chance;
  register int stat,slvl,slevel;
  int minfail,sk;

  slvl=smod(S_MAGIC);
  /* Here we modify the failure SLIGHTLY for the Relevant Skill which
     enhances the power of said spells */
  slvl+=(py.skills.cur_skill[magical[py.misc.realm]]/30)-4;
  s_ptr = &magic_spell[py.misc.realm][spell];
  slevel=(s_ptr->slevel);
  if (slevel<slvl)
   chance = s_ptr->sfail - 3*(slvl-slevel);
  else
   chance = s_ptr->sfail + 4*(slevel-slvl);
  stat = prime_stat[py.misc.realm];
  chance -= 3 * (stat_adj(stat)-1);
  if (s_ptr->smana > py.misc.cmana)
    chance += 5 * (s_ptr->smana-py.misc.cmana);
  switch(stat_adj(stat)){
     case 0: minfail = 50; break; /* I doubt can cast spells with stat this low, anyways... */
     case 1: minfail = 12; break; /* 8-14 stat */
     case 2: minfail = 8; break; /* 15-17 stat */
     case 3: minfail = 5; break; /* 18-18/49 stat */
     case 4: minfail = 4; break; /* 18/50-18/69 */
     case 5: minfail = 4; break; /* 18/70-18/89 */
     case 6: minfail = 3; break; /* 18/90-18/99 */
     case 7: minfail = 3; break; /* 18/100 */
     case 8: case 9: case 10: minfail = 2; break; /* 18/101 - /130 */
     case 11: case 12: minfail = 2; break; /* /131 - /150 */
     case 13: case 14: minfail = 1; break; /* /151 - /170 */
     case 15: case 16: minfail = 1; break; /* /171 - /200 */
     default: minfail = 0; break; /* > 18/200 */
   }   
   if ((minfail < 10) && slvl<30)
     minfail = 10; /* only true magic users get best failure rates */
  else if ((minfail < 5) && slvl<37)
    minfail = 5;
  else if ((minfail < 1) && slvl<45)
    minfail = 1;
  if (chance > 95)
    chance = 95;
  else if (chance < minfail)
    chance = minfail;
  return chance;
}


/* Print list of spells					-RAK-	*/
/* if nonconsec is -1: spells numbered consecutively from 'a' to 'a'+num
                  >=0: spells numbered by offset from nonconsec */
void print_spells(spell, num, comment, nonconsec, tval)
int *spell;
register int num;
int comment, nonconsec, tval; /* Last is used to find spell offset */
{
  register int i, j;
  char ok[4];
  vtype out_val;
  register spell_type *s_ptr;
  int col, offset;
  char *p;
  char spell_char;

  if (comment)
    col = 22;
  else
    col = 31;
  if (tval==TV_MAGIC_BOOK)
    offset = SPELL_OFFSET;
  else if (tval==TV_PRAYER_BOOK)
    offset = PRAYER_OFFSET;
  else if (tval==TV_NATURE_BOOK)
    offset = DRUID_OFFSET;
  else
    offset = NECROS_OFFSET;
  erase_line(1, col);
  put_buffer("Name", 1, col+5);
  put_buffer("Ok? Mana Fail", 1, col+35);
  /* only show the first 22 choices */
  if (num > 22)
    num = 22;
  for (i = 0; i < num; i++)
    {
      j = spell[i];
      s_ptr = &magic_spell[py.misc.realm][j];
      if (comment == FALSE)
	p = "";
      else if (j>=32?((spell_forgotten2 & (1L << (j-32))) != 0)
	           :((spell_forgotten & (1L << j)) != 0))
	p = " forgotten";
      else if (j>=32?((spell_learned2 & (1L << (j-32))) == 0)
	           :((spell_learned & (1L << j)) == 0))
	p = " unknown";
      else if (j>=32?((spell_worked2 & (1L << (j-32))) == 0)
	           :((spell_worked & (1L << j)) == 0))
	p = " untried";
      else
	p = "";
      /* determine whether or not to leave holes in character choices,
	 nonconsec -1 when learning spells, consec offset>=0 when asking which
	 spell to cast */
      if (nonconsec == -1)
	spell_char = 'a' + i;
      else
	spell_char = 'a' + j - nonconsec;
      if (s_ptr->slevel>smod(S_MAGIC))
	strcpy(ok,"No ");
      else
	strcpy(ok,"Yes");
      (void) sprintf(out_val, "  %c) %-30s%s %4d %3d%%%s", spell_char,
		     spell_names[j+offset], ok, s_ptr->smana,
		     spell_chance (j), p);
      prt(out_val, 2+i, col);
    }
}


/* Returns spell pointer				-RAK-	*/
int get_spell(spell, num, sn, sc, prompt, first_spell, tval)
int *spell;
register int num;
register int *sn, *sc, tval;
char *prompt;
int first_spell;
{
  register spell_type *s_ptr;
  int flag, redraw, offset, i;
  char choice;
  vtype out_str, tmp_str;

  *sn = -1;
  flag = FALSE;
  (void) sprintf(out_str, "(Spells %c-%c, *=List, <ESCAPE>=exit) %s",
		 spell[0]+'a'-first_spell, spell[num-1]+'a'-first_spell,
		 prompt);
  redraw = FALSE;
  if (tval==TV_MAGIC_BOOK)
    offset = SPELL_OFFSET;
  else if (tval==TV_PRAYER_BOOK)
    offset = PRAYER_OFFSET;
  else if (tval==TV_NATURE_BOOK)
    offset = DRUID_OFFSET;
  else
    offset = NECROS_OFFSET;
  while (flag == FALSE && get_com (out_str, &choice))
    {
      if (isupper((int)choice))
	{
	  *sn = choice-'A'+first_spell;
	  /* verify that this is in spell[], at most 22 entries in spell[] */
	  for (i = 0; i < num; i++)
	    if (*sn == spell[i])
	      break;
	  if (i == num)
	    *sn = -2;
	  else
	    {
	      s_ptr = &magic_spell[py.misc.realm][*sn];
	      (void) sprintf (tmp_str, "Cast %s (%d mana, %d%% fail)?",
			      spell_names[*sn+offset], s_ptr->smana,
			      spell_chance (*sn));
	      if (get_check (tmp_str))
		flag = TRUE;
	      else
		*sn = -1;
	    }
	}
      else if (islower((int)choice))
	{
	  *sn = choice-'a'+first_spell;
	  /* verify that this is in spell[], at most 22 entries in spell[] */
	  for (i = 0; i < num; i++)
	    if (*sn == spell[i])
	      break;
	  if (i == num)
	    *sn = -2;
	  else
	    flag = TRUE;
	}
      else if (choice == '*')
	{
	  /* only do this drawing once */
	  if (!redraw)
	    {
	      save_screen ();
	      redraw = TRUE;
	      print_spells (spell, num, FALSE, first_spell, tval);
	    }
	}
      else if (isalpha((int)choice))
	*sn = -2;
      else
	{
	  *sn = -1;
	  bell();
	}
      if (*sn == -2) {
	if (tval==TV_MAGIC_BOOK || tval==TV_DARK_BOOK)
	  sprintf(tmp_str, "You don't know that spell.");
	else if (tval==TV_PRAYER_BOOK)
	  sprintf(tmp_str,"You don't know that prayer.");
	else if (tval==TV_NATURE_BOOK)
	  sprintf(tmp_str,"You don't know that technique.");
	msg_print(tmp_str);
      }
    }
  if (redraw)
    restore_screen ();

  erase_line(MSG_LINE, 0);
  if (flag)
    *sc = spell_chance (*sn);

  return(flag);
}


/* calculate number of spells player should have, and learn forget spells
   until that number is met -JEW- */
void calc_spells(stat)
int stat;
{
  register int i;
  register int32u mask;
  int32u spell_flag;
  int j, offset;
  int num_allowed, new_spells, num_known, levels;
  vtype tmp_str;
  char *p;
  register struct misc *p_ptr;
  register spell_type *msp_ptr;

  p_ptr = &py.misc;
  msp_ptr = &magic_spell[p_ptr->realm][0];
  if (stat == prime_stat[MAGE])
    {
      p = "spell";
      offset = SPELL_OFFSET;
    }
  else if (stat == prime_stat[PRIEST])
    {
      p = "prayer";
      offset = PRAYER_OFFSET;
    }
  else if (stat == prime_stat[DRUID])
    {
      p = "technique";
      offset = DRUID_OFFSET;
    }
  else if (stat == prime_stat[NECROS])
    {
      p = "spell";
      offset = NECROS_OFFSET;
    }
  /* check to see if know any spells greater than level, eliminate them */
  for (i = 31, mask = 0x80000000L; mask; mask >>= 1, i--) {
    if (mask & spell_learned)
      {
	if (msp_ptr[i].slevel > smod(S_MAGIC))
	  {
	    spell_learned &= ~mask;
	    spell_forgotten |= mask;
	    (void) sprintf(tmp_str, "You have forgotten the %s of %s.", p,
			   spell_names[i+offset]);
	    msg_print(tmp_str);
	  }
      }
    if (mask & spell_learned2)
      {
	if (msp_ptr[i+32].slevel > smod(S_MAGIC))
	  {
	    spell_learned2 &= ~mask;
	    spell_forgotten2 |= mask;
	    (void) sprintf(tmp_str, "You have forgotten the %s of %s.", p,
			   spell_names[i+offset+32]);
	    msg_print(tmp_str);
	  }
      }
  }

  /* calc number of spells allowed */
  levels = smod(S_MAGIC);
  if (levels<12)
    levels=levels/2+1;/* REAL slow learning */
  else if (levels<25)
    levels=levels*2/3+1; /* Still kinda slow */
  switch(stat_adj(stat))
    {
    case 0:		    num_allowed = 0; break;
    case 1: case 2: case 3: num_allowed = 1 * levels; break;
    case 4: case 5:	    num_allowed = 3 * levels / 2; break;
    case 6:		    num_allowed = 2 * levels; break;
    default:		    num_allowed = 5 * levels / 2; break;
    }
  num_known = 0;
  for (mask = 0x1; mask; mask <<= 1) {
    if (mask & spell_learned)
      num_known++;
    if (mask & spell_learned2)
      num_known++;
  }

  new_spells = num_allowed - num_known;

  if (new_spells > 0)
    {
      /* remember forgotten spells while forgotten spells exist of new_spells
	 positive, remember the spells in the order that they were learned */
      for (i = 0; ((spell_forgotten|spell_forgotten2) && new_spells
		   && (i < num_allowed) && (i < 64)); i++)
	{
	  /* j is (i+1)th spell learned */
	  j = spell_order[i];
	  /* shifting by amounts greater than number of bits in long gives
	     an undefined result, so don't shift for unknown spells */
	  if (j == 99)
            continue; /* don't process unknown spells... -CFT */
          
            if (j < 32) { /* use spell_learned, spell_forgotten... -CFT */
            mask = 1L << j; /* bit in spell fields */
            if (mask & spell_forgotten){            
              if (msp_ptr[j].slevel <= smod(S_MAGIC)) {
                spell_forgotten &= ~mask;
                spell_learned |= mask;
                new_spells--;
                (void) sprintf(tmp_str, "You have remembered the %s of %s.", p,
                             spell_names[j+offset]);
                msg_print(tmp_str);
                }
              else num_allowed++; /* if was too high lv to remember */
              } /* if mask&spell_forgotten */
            } /* j < 32 */
          else { /* j > 31, use spell_learned2, spell_forgotten2... -CFT */
            mask = 1L << (j - 32); /* bit in spell fields */
            if (mask & spell_forgotten2){           
              if (msp_ptr[j].slevel <= smod(S_MAGIC)) {
                spell_forgotten2 &= ~mask;
                spell_learned2 |= mask;
                new_spells--;
                (void) sprintf(tmp_str, "You have remembered the %s of %s.", p,
                             spell_names[j+offset]);
                msg_print(tmp_str);
                }
              else num_allowed++; /* if was too high lv to remember */
              } /* if mask&spell_forgotten2 */
            } /* j > 31 */
        } /* for loop... */         

      if (new_spells > 0)
	{
	  /* determine which spells player can learn */
	  /* must check all spells here, in gain_spell() we actually check
	     if the books are present */
	  spell_flag = 0x7FFFFFFFL & ~spell_learned;
	  mask = 0x1;
	  i = 0;
	  for (j = 0, mask = 0x1; spell_flag; mask <<= 1, j++)
	    if (spell_flag & mask)
	      {
		spell_flag &= ~mask;
		if (msp_ptr[j].slevel <= smod(S_MAGIC))
		  i++;
	      }
	  spell_flag = 0x7FFFFFFFL & ~spell_learned2;
	  mask = 0x1;
	  for (j = 0, mask = 0x1; spell_flag; mask <<= 1, j++)
	    if (spell_flag & mask)
	      {
		spell_flag &= ~mask;
		if (msp_ptr[j].slevel <= smod(S_MAGIC))
		  i++;
	      }

	  if (new_spells > i)
	    new_spells = i;
	}
    }
  else if (new_spells < 0)
    {
      /* forget spells until new_spells zero or no more spells know, spells
	 are forgotten in the opposite order that they were learned */
      for (i = 63; new_spells && (spell_learned|spell_learned2); i--)
	{
	  /* j is the (i+1)th spell learned */
	  j = spell_order[i];
	  /* shifting by amounts greater than number of bits in long gives
	     an undefined result, so don't shift for unknown spells */
	  if (j == 99)
            continue; /* don't process unknown spells... -CFT */
          
            if (j < 32) { /* use spell_learned, spell_forgotten... -CFT */
            mask = 1L << j; /* bit in spell fields */
            if (mask & spell_learned){      
             spell_learned &= ~mask;
             spell_forgotten |= mask;
             new_spells++;
              (void) sprintf(tmp_str, "You have forgotten the %s of %s.", p,
                             spell_names[j+offset]);
              msg_print(tmp_str);
              } /* if mask&spell_learned */
            } /* j < 32 */
          else { /* j > 31, use spell_learned2, spell_forgotten2... -CFT */
            mask = 1L << (j - 32); /* bit in spell fields */
            if (mask & spell_learned2){     
             spell_learned2 &= ~mask;
             spell_forgotten2 |= mask;
             new_spells++;
              (void) sprintf(tmp_str, "You have forgotten the %s of %s.", p,
                             spell_names[j+offset]);
              msg_print(tmp_str);
             } /* if mask&spell_learned2 */
            } /* j > 31 */
        } /* for loop... */         
       new_spells = 0; /* we've forgotten, so we shouldn't be learning any... */
}

  if (new_spells != py.flags.new_spells && py.misc.realm!=NONE)
    {
      if (new_spells > 0 && py.flags.new_spells == 0)
	{
	  (void) sprintf(tmp_str, "You can learn some new %ss now.", p);
	  msg_print(tmp_str);
	}

      py.flags.new_spells = new_spells;
      py.flags.status |= PY_STUDY;
    }
}


/* gain spells when player wants to		- jw */
void gain_spells()
{
  char query;
  int stat, diff_spells, new_spells;
  int spells[63], offset, last_known;
  register int i, j;
  register int32u spell_flag, spell_flag2, mask;
  vtype tmp_str;
  struct misc *p_ptr;
  register spell_type *msp_ptr;

  i = 0;
  if (py.flags.blind > 0)
    msg_print("You can't see to read your spell book!");
  else if (no_light())
    msg_print("You have no light to read by.");
  else if (py.flags.confused > 0)
    msg_print("You are too confused.");
  else
    i = 1;
  if (i == 0)
    return;

  new_spells = py.flags.new_spells;
  diff_spells = 0;
  p_ptr = &py.misc;
  msp_ptr = &magic_spell[py.misc.realm][0];
  stat = prime_stat[py.misc.realm];
  if (py.misc.realm == MAGE)
    offset = SPELL_OFFSET;
  else if (py.misc.realm == PRIEST)
    offset = PRAYER_OFFSET;
  else if (py.misc.realm == DRUID)
    offset = DRUID_OFFSET;
  else if (py.misc.realm == NECROS)
    offset = NECROS_OFFSET;

  for (last_known = 0; last_known < 64; last_known++)
    if (spell_order[last_known] == 99)
      break;

  if (!new_spells && py.misc.realm != DRUID)
    (void) sprintf(tmp_str, "You can't learn any new %ss!",
		   (stat == A_INT ? "spell" : "prayer"));
  if (!new_spells && py.misc.realm == DRUID)
    (void) sprintf(tmp_str,"You can't learn any new techniques!");
  if (!new_spells && py.misc.realm == NECROS)
    (void) sprintf(tmp_str,"You can't learn any new spells!");
  if (!new_spells) {
    msg_print(tmp_str);
    free_turn_flag = TRUE;
    }
 else {
    /* determine which spells player can learn */
    /* mages need the book to learn a spell, priests do not need the book */
    spell_flag = 0;
    spell_flag2 = 0;
    for (i = 0; i < inven_ctr; i++)
      if (((stat == prime_stat[0]) && (inventory[i].tval == TV_MAGIC_BOOK))
	  || ((stat == prime_stat[1]) && (inventory[i].tval == TV_PRAYER_BOOK))
	  || ((stat == prime_stat[2]) && (inventory[i].tval == TV_NATURE_BOOK))
	  || ((stat == prime_stat[3]) && (inventory[i].tval==TV_DARK_BOOK)))
	{
	    spell_flag |= inventory[i].flags;
	    spell_flag2 |= inventory[i].flags2;
	  }
  }

  /* clear bits for spells already learned */
  spell_flag &= ~spell_learned;
  spell_flag2 &= ~spell_learned2;

  mask = 0x1;
  i = 0;
  for (j = 0, mask = 0x1; (spell_flag|spell_flag2); mask <<= 1, j++) {
    if (spell_flag & mask) {
      spell_flag &= ~mask;
      if (msp_ptr[j].slevel <= smod(S_MAGIC)) {
	spells[i] = j;
	i++;
      }
    }
    if (spell_flag2 & mask) {
      spell_flag2 &= ~mask;
      if (msp_ptr[j+32].slevel <= smod(S_MAGIC)) {
	spells[i] = j+32;
	i++;
      }
    }
  }

  if (new_spells > i) {
    msg_print("You seem to be missing a book.");
    diff_spells = new_spells - i;
    new_spells = i;
  }
  if (new_spells == 0)
    ;
  else if (py.misc.realm!=PRIEST) {
    /* get to choose which mage spells will be learned */
    save_screen();
    if (py.misc.realm==MAGE)
      print_spells (spells, i, FALSE, -1, TV_MAGIC_BOOK);
    else if (py.misc.realm==DRUID)
      print_spells (spells, i, FALSE, -1, TV_NATURE_BOOK);
    else if (py.misc.realm==NECROS)
      print_spells (spells, i, FALSE, -1, TV_DARK_BOOK);
    while (new_spells && get_com ("What do you want to learn?", &query)) {
      j = query - 'a';
      /* test j < 23 in case i is greater than 22, only 22 spells
	are actually shown on the screen, so limit choice to those */
      if (j >= 0 && j < i && j < 22) {
	new_spells--;
	if (spells[j]<32)
	  spell_learned |= 1L << spells[j];
	else
	  spell_learned2 |= 1L << (spells[j]-32);
	spell_order[last_known++] = spells[j];
	for (; j <= i-1; j++)
	  spells[j] = spells[j+1];
	i--;
	erase_line (j+1, 31);
	if (py.misc.realm==MAGE)
	  print_spells (spells, i, FALSE, -1, TV_MAGIC_BOOK);
	else if (py.misc.realm==DRUID)
	  print_spells (spells, i, FALSE, -1, TV_NATURE_BOOK);
	else if (py.misc.realm==NECROS)
	  print_spells (spells, i, FALSE, -1, TV_DARK_BOOK);
      } else
	bell();
    }
    restore_screen();
  } else {
    /* pick a prayer at random */
    while (new_spells) {
      j = randint(i) - 1;
      if (spells[j]<32)
	spell_learned |= 1L << spells[j];
      else
	spell_learned2 |= 1L << (spells[j]-32);
	      spell_order[last_known++] = spells[j];
      (void) sprintf (tmp_str,
		      "You have learned the prayer of %s.",
		      spell_names[spells[j]+offset]);
      msg_print(tmp_str);
      for (; j <= i-1; j++)
	spells[j] = spells[j+1];
      i--;
      new_spells--;
    }
  }
  py.flags.new_spells = new_spells + diff_spells;
  if (py.flags.new_spells == 0)
    py.flags.status |= PY_STUDY;
  /* set the mana for first level characters when they learn first spell */
  if (py.misc.mana == 0)
    calc_mana(stat);
}



/* Gain some mana if you know at least one spell	-RAK-	*/
void calc_mana(stat)
int stat;
{
  register int new_mana, levels,tmp;
  register struct misc *p_ptr;
  register int32 value;

  p_ptr = &py.misc;
  if (spell_learned != 0 || spell_learned2 != 0)
    {
      levels = smod(S_MPOWER);
      if (levels<20)
	levels=levels/2+1;
      else if (levels<35)
	levels=levels*2/3+1;
      tmp=stat_adj(stat);
      switch(stat_adj(stat))
	{
	case 0: new_mana = 0; break;
	case 1: case 2: new_mana = 1 * levels; break;
	case 3: new_mana = 3 * levels / 2; break;
	case 4: new_mana = 2 * levels; break;
	case 5: new_mana = 5 * levels / 2; break;
	case 6: new_mana = 3 * levels; break;
	case 7: new_mana = 4 * levels; break;
	case 8: new_mana = 9 * levels / 2; break;
	case 9: new_mana = 5 * levels; break;
	case 10: new_mana = 11 * levels / 2; break;
	case 11: new_mana = 6 * levels; break;
	case 12: new_mana = 13 * levels / 2; break;
	case 13: new_mana = 7 * levels; break;
	case 14: new_mana = 15 * levels / 2; break;
	case 15: new_mana = 8 * levels; break;
	case 16: new_mana = 9 * levels; break;
	default: new_mana = 10 * levels; break;
	}
      /* increment mana by one, so that first level chars have 2 mana */
      if (new_mana > 0)
	new_mana++;
      switch(py.misc.realm) /* Restrictions for various realms */
	{
	case NONE:
	  break;
	case MAGE:
	  if (p_ptr->pac > 20)
	    new_mana-=2*(p_ptr->pac-20);
	  if (inventory[INVEN_HANDS].tval!=TV_NOTHING)
	    if (!(inventory[INVEN_HANDS].flags & TR_FREE_ACT))
	      new_mana=(new_mana/2)+1;
	  break;
	case PRIEST:
	  if (p_ptr->pac > 40)
	    new_mana-=(p_ptr->pac-40);
	  break;
	case DRUID:
	  if (inventory[INVEN_HANDS].tval!=TV_NOTHING)
	    if (!(inventory[INVEN_HANDS].flags & TR_FREE_ACT))
	      new_mana=(new_mana*2/3)+1;
	  if (p_ptr->pac > 50)
	    new_mana-=(p_ptr->pac-50);
	  break;
	case NECROS:
	  if (p_ptr->pac > 30)
	    new_mana-=(p_ptr->pac-30);
	  if (inventory[INVEN_HANDS].tval!=TV_NOTHING)
	    if (!(inventory[INVEN_HANDS].flags & TR_FREE_ACT))
	      new_mana=(new_mana*2/3)+1;
	  break;
	}
      if (new_mana<0) new_mana=0;
      /* mana can be zero when creating character */
      if (p_ptr->mana != new_mana)
	{
	  if (p_ptr->mana != 0)
	    {
	      /* change current mana proportionately to change of max mana,
		 divide first to avoid overflow, little loss of accuracy */
	      value = (((long)p_ptr->cmana << 16) + p_ptr->cmana_frac)
		/ p_ptr->mana * new_mana;
	      p_ptr->cmana = value >> 16;
	      p_ptr->cmana_frac = value & 0xFFFF;
	    }
	  else
	    {
	      p_ptr->cmana = new_mana;
	      p_ptr->cmana_frac = 0;
	    }
	  p_ptr->mana = new_mana;
	  /* can't print mana here, may be in store or inventory mode */
	  py.flags.status |= PY_MANA;
	}
    }
  else if (p_ptr->mana != 0)
    {
      p_ptr->mana = 0;
      p_ptr->cmana = 0;
      /* can't print mana here, may be in store or inventory mode */
      py.flags.status |= PY_MANA;
    }
}

/* Prints experience					-RAK-	*/
void prt_experience()
{
  register struct misc *p_ptr;
  char out_val[100];

  p_ptr = &py.misc;
  if (p_ptr->exp > MAX_EXP)
    p_ptr->exp = MAX_EXP;
  if (p_ptr->exp > p_ptr->max_exp)
    p_ptr->max_exp = p_ptr->exp;
  (void) sprintf(out_val, "%8ld", p_ptr->exp);
  put_buffer(out_val, 14, STAT_COLUMN+4);
  prt_level();
}


/* Calculate the players hit points */
/* This seems to screw up */
void calc_hitpoints()
{
  register int hitpoints,lvl;
  register struct misc *p_ptr;
  register int32 value;

  lvl=get_level();
  p_ptr = &py.misc;
  hitpoints = player_hp[lvl-1] + (con_adj() * lvl) +
    (smod(S_ENDURANCE)*lvl);
  /* always give at least one point per level + 1 */
  if (hitpoints < (lvl + 1))
    hitpoints = lvl + 1;
  calc_bonuses();
  if (py.flags.status & PY_HERO)
    hitpoints += 10;
  if (py.flags.status & PY_SHERO)
    hitpoints += 20;

  /* mhp can equal zero while character is being created */
  if ((hitpoints != p_ptr->mhp) && (p_ptr->mhp != 0))
    {
      /* change current hit points proportionately to change of mhp,
	 divide first to avoid overflow, little loss of accuracy */
      value = (((long)p_ptr->chp << 16) + p_ptr->chp_frac) / p_ptr->mhp
	* hitpoints;
      p_ptr->chp = value >> 16;
      p_ptr->chp_frac = value & 0xFFFF;
      p_ptr->mhp = hitpoints;

      /* can't print hit points here, may be in store or inventory mode */
      py.flags.status |= PY_HP;
    }
}


/* Inserts a string into a string				*/
void insert_str(object_str, mtc_str, insert)
char *object_str, *mtc_str, *insert;
{
  int obj_len;
  char *bound, *pc;
  register int i, mtc_len;
  register char *temp_obj, *temp_mtc;
  char out_val[80];

  mtc_len = strlen(mtc_str);
  obj_len = strlen(object_str);
  bound = object_str + obj_len - mtc_len;
  for (pc = object_str; pc <= bound; pc++)
    {
      temp_obj = pc;
      temp_mtc = mtc_str;
      for (i = 0; i < mtc_len; i++)
	if (*temp_obj++ != *temp_mtc++)
	  break;
      if (i == mtc_len)
	break;
    }

  if (pc <= bound)
    {
      (void) strncpy(out_val, object_str, (int)(pc-object_str));
      out_val[(int)(pc-object_str)] = '\0';
      if (insert)
	(void) strcat(out_val, insert);
      (void) strcat(out_val, (char *)(pc+mtc_len));
      (void) strcpy(object_str, out_val);
    }
}


#if 0
/* this is no longer used anywhere */
/* Inserts a number into a string				*/
void insert_num(object_str, mtc_str, number, show_sign)
char *object_str;
register char *mtc_str;
int number;
int show_sign;
{
  int mlen;
  vtype str1, str2;
  register char *string, *tmp_str;
  int flag;

  flag = 1;
  mlen = strlen(mtc_str);
  tmp_str = object_str;
  do
    {
      string = index(tmp_str, mtc_str[0]);
      if (string == NULL)
	flag = 0;
      else
	{
	  flag = strncmp(string, mtc_str, mlen);
	  if (flag)
	    tmp_str = string+1;
	}
    }
  while (flag);
  if (string)
    {
      (void) strncpy(str1, object_str, (int)(string - object_str));
      str1[(int)(string - object_str)] = '\0';
      (void) strcpy(str2, string + mlen);
      if ((number >= 0) && (show_sign))
	(void) sprintf(object_str, "%s+%d%s", str1, number, str2);
      else
	(void) sprintf(object_str, "%s%d%s", str1, number, str2);
    }
}
#endif

void insert_lnum(object_str, mtc_str, number, show_sign)
char *object_str;
register char *mtc_str;
int32 number;
int show_sign;
{
  int mlen;
  vtype str1, str2;
  register char *string, *tmp_str;
  int flag;

  flag = 1;
  mlen = strlen(mtc_str);
  tmp_str = object_str;
  do
    {
      string = index(tmp_str, mtc_str[0]);
      if (string == 0)
	flag = 0;
      else
	{
	  flag = strncmp(string, mtc_str, mlen);
	  if (flag)
	    tmp_str = string+1;
	}
    }
  while (flag);
  if (string)
    {
      (void) strncpy(str1, object_str, string - object_str);
      str1[string - object_str] = '\0';
      (void) strcpy(str2, string + mlen);
      if ((number >= 0) && (show_sign))
	(void) sprintf(object_str, "%s+%ld%s", str1, number, str2);
      else
	(void) sprintf(object_str, "%s%ld%s", str1, number, str2);
    }
}


/* lets anyone enter wizard mode after a disclaimer...		- JEW - */
int enter_wiz_mode()
{
  register int answer;

  if (!is_wizard(player_uid))
    return FALSE;
  if (!noscore)
    {
      msg_print("Wizard mode is for debugging and experimenting.");
      answer = get_check(
	"The game will not be scored if you enter wizard mode. Are you sure?");
    }
  if (noscore || answer)
    {
      noscore |= 0x2;
      wizard = TRUE;
      return (TRUE);
    }
  return(FALSE);
}


/* Weapon weight VS strength and dexterity		-RAK-	*/
int attack_blows(weight, wtohit)
int weight;
int *wtohit;
{
  register int adj_weight;
  register int str_index, dex_index, s, d;

  s = py.stats.use_stat[A_STR];
  d = py.stats.use_stat[A_DEX];
  if (s * 15 < weight)
    {
      *wtohit = s * 15 - weight;
      return 1;
    }
  else
    {
      *wtohit = 0;
      if      (d <  10)	 dex_index = 0;
      else if (d <  19)	 dex_index = 1;
      else if (d <  68)	 dex_index = 2;
      else if (d < 108)	 dex_index = 3;
      else if (d < 118)	 dex_index = 4;
      else if (d==118)   dex_index = 5;
      else if (d < 128)	 dex_index = 6;
      else if (d < 138)	 dex_index = 7;
      else if (d < 148)	 dex_index = 8;
      else if (d < 158)	 dex_index = 9;
      else if (d < 168)	 dex_index = 10;
      else dex_index = 11;
      adj_weight = ((s*10) / ((weight<50)? 50 : weight));
      if      (adj_weight < 2)	str_index = 0;
      else if (adj_weight < 3)	str_index = 1;
      else if (adj_weight < 4)	str_index = 2;
      else if (adj_weight < 6)	str_index = 3;
      else if (adj_weight < 8)	str_index = 4;
      else if (adj_weight < 10)	str_index = 5;
      else if (adj_weight < 13)	str_index = 6;
      else if (adj_weight < 15)	str_index = 7;
      else if (adj_weight < 18)	str_index = 8;
      else if (adj_weight < 20)	str_index = 9;
      else			str_index = 10;
      return (int)blows_table[str_index][dex_index];
    }
}


/* Special damage due to magical abilities of object	-RAK-	*/
int tot_dam(i_ptr, tdam, monster)
register inven_type *i_ptr;
register int tdam;
int monster;
{
  register creature_type *m_ptr;
  register recall_type *r_ptr;

  if ((((i_ptr->tval >= TV_SLING_AMMO) && (i_ptr->tval <= TV_ARROW)) ||
       ((i_ptr->tval >= TV_HAFTED) && (i_ptr->tval <= TV_SWORD)) ||
       (i_ptr->tval == TV_FLASK)))
    {
      m_ptr = &c_list[monster];
      r_ptr = &c_recall[monster];
      /* Mjollnir? :-> */
      if (!(m_ptr->cdefense & IM_LIGHTNING) && (i_ptr->flags2 & TR_LIGHTNING))
	{
	  tdam = tdam * 5;
	}
      /* Execute Dragon */
      else if ((m_ptr->cdefense & DRAGON) && (i_ptr->flags & TR_SLAY_X_DRAGON))
	{
	  tdam = tdam * 5;
	  r_ptr->r_cdefense |= DRAGON;
	}
      /* Slay Dragon  */
      else if ((m_ptr->cdefense & DRAGON) && (i_ptr->flags & TR_SLAY_DRAGON))
	{
	  tdam = tdam * 3;
	  r_ptr->r_cdefense |= DRAGON;
	}
      /* Slay Undead  */
      else if ((m_ptr->cdefense & UNDEAD) &&(i_ptr->flags & TR_SLAY_UNDEAD))
	{
	  tdam = tdam * 3;
	  r_ptr->r_cdefense |= UNDEAD;
	}
      /* Slay ORC     */
      else if ((m_ptr->cdefense & ORC) && (i_ptr->flags2 & TR_SLAY_ORC))
	{
	  tdam = tdam * 3;
	  r_ptr->r_cdefense |= ORC;
	}
      /* Slay TROLL     */
      else if ((m_ptr->cdefense & TROLL) && (i_ptr->flags2 & TR_SLAY_TROLL))
	{
	  tdam = tdam * 3;
	  r_ptr->r_cdefense |= TROLL;
	}
      /* Slay GIANT     */
      else if ((m_ptr->cdefense & GIANT) && (i_ptr->flags2 & TR_SLAY_GIANT))
	{
	  tdam = tdam * 3;
	  r_ptr->r_cdefense |= GIANT;
	}
      /* Slay DEMON     */
      else if ((m_ptr->cdefense & DEMON) && (i_ptr->flags2 & TR_SLAY_DEMON))
	{
	  tdam = tdam * 3;
	  r_ptr->r_cdefense |= DEMON;
	}
      /* Venom         */
      else if ((!(m_ptr->cdefense & IM_POISON))
	       && (i_ptr->flags2 & TR_VENOM))
	{
	  tdam = tdam * 4;
	}
      /* Frost	       */
      else if ((!(m_ptr->cdefense & IM_FROST))
	       && (i_ptr->flags & TR_FROST_BRAND))
	{
	  tdam = tdam * 3;
	}
      /* Fire	      */
      else if ((!(m_ptr->cdefense & IM_FIRE))
	       && (i_ptr->flags & TR_FLAME_TONGUE))
	{
	  tdam = tdam * 3;
	}
      /* Slay Evil     */
      else if ((m_ptr->cdefense & EVIL) && (i_ptr->flags & TR_SLAY_EVIL))
	{
	  tdam = tdam * 2;
	  r_ptr->r_cdefense |= EVIL;
	}
      /* Slay Animal  */
      else if ((m_ptr->cdefense & ANIMAL) &&(i_ptr->flags & TR_SLAY_ANIMAL))
	{
	  tdam = tdam * 2;
	  r_ptr->r_cdefense |= ANIMAL;
	}
      if (((m_ptr->cdefense & IM_FROST)) && (i_ptr->flags & TR_FROST_BRAND))
	  r_ptr->r_cdefense |= IM_FROST;
      if (((m_ptr->cdefense & IM_FIRE)) && (i_ptr->flags & TR_FLAME_TONGUE))
	  r_ptr->r_cdefense |= IM_FIRE;
      if (((m_ptr->cdefense & IM_LIGHTNING)) && (i_ptr->flags2 & TR_LIGHTNING))
	  r_ptr->r_cdefense |= IM_LIGHTNING;
      if ((i_ptr->flags2 & TR_IMPACT) && (tdam>50)) earthquake();
    }
  return(tdam);
}


/* Critical hits, Nasty way to die.			-RAK-	*/
int critical_blow(weight, plus, dam, attack_type)
register int weight, plus, dam;
int attack_type;
{
  register int critical;

  critical = dam;
  /* Weight of weapon, plusses to hit, and character level all	    */
  /* contribute to the chance of a critical			   */
  if (randint(3000) <= (int)(weight + 5 * plus))
    {
      weight += randint(650);
      if (weight < 400)
	{
	  critical = 2*dam + 5;
	  msg_print("It was a good hit!");
	}
      else if (weight < 700)
	{
	  critical = 2*dam + 10;
	  msg_print("It was an excellent hit!");
	}
      else if (weight < 900)
	{
	  critical = 3*dam + 15;
	  msg_print("It was a superb hit!");
	}
      else
	{
	  critical = 3*dam + 20;
	  msg_print("It was a *GREAT* hit!");
	}
    }
  return(critical);
}


/* Given direction "dir", returns new row, column location -RAK- */
int mmove(dir, y, x)
int dir;
register int *y, *x;
{
  register int new_row, new_col;
  int bool;

  switch(dir)
    {
    case 1:
      new_row = *y + 1;
      new_col = *x - 1;
      break;
    case 2:
      new_row = *y + 1;
      new_col = *x;
      break;
    case 3:
      new_row = *y + 1;
      new_col = *x + 1;
      break;
    case 4:
      new_row = *y;
      new_col = *x - 1;
      break;
    case 5:
      new_row = *y;
      new_col = *x;
      break;
    case 6:
      new_row = *y;
      new_col = *x + 1;
      break;
    case 7:
      new_row = *y - 1;
      new_col = *x - 1;
      break;
    case 8:
      new_row = *y - 1;
      new_col = *x;
      break;
    case 9:
      new_row = *y - 1;
      new_col = *x + 1;
      break;
    }
  bool = FALSE;
  if ((new_row >= 0) && (new_row < cur_height)
      && (new_col >= 0) && (new_col < cur_width))
    {
      *y = new_row;
      *x = new_col;
      bool = TRUE;
    }
  return(bool);
}

/* Saving throws for player character.		-RAK-	*/
int player_saves()
{
  int tt,t2;

  t2 = smod(S_SAVE);
  tt = stat_adj(A_WIS)+t2;
  tt += luck(); /* % modifier for luck */
  if (randint(100) <= tt)
    return(TRUE);
  else
    return(FALSE);
}


/* Finds range of item in inventory list		-RAK-	*/
int find_range(item1, item2, j, k)
int item1, item2;
register int *j, *k;
{
  register int i;
  register inven_type *i_ptr;
  int flag;

  i = 0;
  *j = -1;
  *k = -1;
  flag = FALSE;
  i_ptr = &inventory[0];
  while (i < inven_ctr)
    {
      if (!flag)
	{
	  if ((i_ptr->tval == item1) || (i_ptr->tval == item2))
	    {
	      flag = TRUE;
	      *j = i;
	    }
	}
      else
	{
	  if ((i_ptr->tval != item1) && (i_ptr->tval != item2))
	    {
	      *k = i - 1;
	      break;
	    }
	}
      i++;
      i_ptr++;
    }
  if (flag && (*k == -1))
    *k = inven_ctr - 1;
  return(flag);
}


/* Teleport the player to a new location		-RAK-	*/
void teleport(dis)
int dis;
{
  register int y, x, i, j, count;

  do {

    count=0;
    do {
      count+=1;
      y = randint(cur_height) - 1;
      x = randint(cur_width) - 1;
      while (distance(y, x, char_row, char_col) > dis) {
	y += ((char_row-y)/2);
	x += ((char_col-x)/2);
      }
    }
    while ( ((cave[y][x].fval >= MIN_CLOSED_SPACE) ||
	     (cave[y][x].cptr >= 2) ||
	     (t_list[ cave[y][x].tptr ].index == OBJ_OPEN_DOOR) ||
	     (cave[y][x].fval == NT_DARK_FLOOR) ||
	     (cave[y][x].fval == NT_LIGHT_FLOOR)) && count<1000);

    dis *=2;
  } while (count==1000);

  move_rec(char_row, char_col, y, x);
  for (i = char_row-1; i <= char_row+1; i++)
    for (j = char_col-1; j <= char_col+1; j++) {
      cave[i][j].tl = FALSE;
      lite_spot(i, j);
    }
  lite_spot(char_row, char_col);
  char_row = y;
  char_col = x;
  check_view();
  creatures(FALSE);
  teleport_flag = FALSE;
}


/* Add a comment to an object description.		-CJS- */
void scribe_object()
{
  int item_val, j;
  vtype out_val, tmp_str;

  if (inven_ctr > 0 || equip_ctr > 0)
    {
      if (get_item(&item_val, "Which one? ", 0, INVEN_ARRAY_SIZE, 0))
	{
	  objdes(tmp_str, &inventory[item_val], TRUE);
	  (void) sprintf(out_val, "Inscribing %s", tmp_str);
	  msg_print(out_val);
	  if (inventory[item_val].inscrip[0] != '\0')
	    (void) sprintf(out_val, "Replace %s New inscription:",
			   inventory[item_val].inscrip);
	  else
	    (void) strcpy(out_val, "Inscription: ");
	  j = 78 - strlen(tmp_str);
	  if (j > 24)
	    j = 12;
	  prt(out_val, 0, 0);
	  if (get_string(out_val, 0, strlen(out_val), j))
	    inscribe(&inventory[item_val], out_val);
	}
    }
  else
    msg_print("You are not carrying anything to inscribe.");
}

/* Append an additional comment to an object description.	-CJS- */
void add_inscribe(i_ptr, type)
inven_type *i_ptr;
int8u type;
{
  i_ptr->ident |= type;
}

/* Replace any existing comment in an object description with a new one. CJS*/
void inscribe(i_ptr, str)
inven_type *i_ptr;
char *str;
{
  (void) strcpy(i_ptr->inscrip, str);
}


/* We need to reset the view of things.			-CJS- */
void check_view()
{
  register int i, j;
  register cave_type *c_ptr, *d_ptr;

  c_ptr = &cave[char_row][char_col];
  /* Check for new panel		   */
  if (get_panel(char_row, char_col, FALSE))
    prt_map();
  /* Move the light source		   */
  move_light(char_row, char_col, char_row, char_col);
  /* A room of light should be lit.	 */
  if (c_ptr->fval == LIGHT_FLOOR)
    {
      if ((py.flags.blind < 1) && !c_ptr->pl)
	light_room(char_row, char_col);
    }
  /* In doorway of light-room?		   */
  else if (c_ptr->lr && (py.flags.blind < 1))
    {
      for (i = (char_row - 1); i <= (char_row + 1); i++)
	for (j = (char_col - 1); j <= (char_col + 1); j++)
	  {
	    d_ptr = &cave[i][j];
	    if ((d_ptr->fval == LIGHT_FLOOR) && !d_ptr->pl)
	      light_room(i, j);
	  }
    }
}
