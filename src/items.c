/* items.c : Various misc. items like Deck of Many Things + Orbs */

#include "constant.h"
#include "config.h"
#include "types.h"
#include "externs.h"
#include "monster.h"

#ifdef USG
#ifndef ATARIST_MWC
#include <string.h>
#endif
#else
#include <strings.h>
#endif

/* Sets a skill to specified value or changes it by specified value, if
   passed skill<0. */
void sskill(skill,val)
int skill,val;
{
 int res;
 if (skill<0)
   {
     skill=-skill;
     res=py.skills.max_skill[skill]+val;
   }
 else
   res=val;
 if (res<0) res=0;
 if (res>255) res=255;
 py.skills.max_skill[skill]=res;
 py.skills.cur_skill[skill]=res;
 if (res==0) py.skills.adv_skill[skill]=0;
}

/* Items.  Can be <A>ctivated */
void activate_item(which)
int which;
{
  int32u i;
  register int l, ident;
  int item_val, j, k, chance, dir;
  register inven_type *i_ptr;
  register struct misc *m_ptr;

  free_turn_flag = TRUE;
  i_ptr=&inventory[which];
  switch(i_ptr->p1)
    {
    case 1: /* Deck of Many Things */
      msg_print("You draw a card from the deck...                       ");
      switch(randint(18))
	{
	case 1:
	  msg_print("You suddenly are immersed in riches!");
	  py.misc.au+=(long)300000;
	  break;
	case 2:
	  msg_print("Your gold coins turn to dust.");
	  py.misc.au=0;
	  break;
	case 3:
	  msg_print("A magic item suddenly appears!");
	  place_special(char_row, char_col, SPECIAL);
	  prt_map();
	  break;
	case 4:
	  msg_print("You feel like a better fighter!");
	  sskill(-S_SWORD,50);
	  sskill(-S_HAFTED,50);
	  sskill(-S_POLEARM,50);
	  sskill(-S_KARATE,50);
	  sskill(-S_WRESTLING,50);
	  sskill(-S_MAGIC,-50);
	  sskill(-S_MPOWER,-50);
	  break;
	case 5:
	  msg_print("You feel your abilities increase!");
	  for(i=0;i<A_LUC;i++)
	    {
	      if (py.stats.max_stat[i]<19)
		py.stats.max_stat[i]+=3;
	      else
		py.stats.max_stat[i]+=20;
	      if (py.stats.max_stat[i]>118)
		py.stats.max_stat[i]=118;
	      py.stats.cur_stat[i]=py.stats.max_stat[i];
	    }
	  for(i=0;i<S_NUM;i++)
	    sskill(-i,20);
	  break;
	case 6:
	  msg_print("You feel your abilities weaken.");
	  for(i=0;i<A_LUC;i++)
	    {
	      if (py.stats.max_stat[i]<19)
		py.stats.max_stat[i]-=3;
	      else
		py.stats.max_stat[i]-=10;
	      if (py.stats.max_stat[i]<3)
		py.stats.max_stat[i]=3;
	      py.stats.cur_stat[i]=py.stats.max_stat[i];
	    }
	  for(i=0;i<S_NUM;i++)
	    sskill(-i,-60);
	  break;
	case 7:
	  msg_print("You feel incredibly magical!");
	  sskill(-S_MAGIC,60);
	  sskill(-S_MPOWER,60);
	  break;
	case 8:
	  msg_print("You feel yourself permanently speed up!");
	  change_speed(-1);
	  break;
	case 9:
	  msg_print("You feel yourself permanently slow down.");
	  change_speed(2);
	  break;
	case 10:
	  msg_print("You suddenly feel incredibly experienced!");
	  py.misc.exp+=100000;
	  break;
	case 11:
	  msg_print("You feel much less experienced.");
	  py.misc.exp=py.misc.exp/3;
	  py.misc.max_exp=py.misc.exp;
	  break;
	case 12:
	  msg_print("You fall through many floors in the dungeon!");
	  py.misc.max_dlv=100;
	  dun_level=100;
	  new_level_flag = TRUE;
	  (void) prt_map();
	  break;
	case 13:
	  msg_print("You feel very sick.");
	  sskill(-S_ENDURANCE,-60);
	  break;
	case 14:
	  msg_print("Every creature nearby suddenly disappears!");;
	  (void) banishment(ANIMAL|DRAGON|UNDEAD|EVIL,200);
	  break;
	case 15:
	  msg_print("You see a divine light call your name!");
	  py.misc.realm=PRIEST;
	  py.misc.exp+=60000;
	  sskill(-S_MAGIC,80);
	  sskill(-S_MPOWER,80);
	  break;
	case 16:
	  msg_print("You suddenly feel epic powers flow into your body!");
	  for(i=0;i<=A_LUC;i++)
	    {
	      py.stats.max_stat[i]=118;
	      py.stats.cur_stat[i]=118;
	    }
	  py.misc.exp=(long)100000;
	  prt_experience();
	  /* Now create some cool items */
	  special_random_object(char_row, char_col, 1);
	  prt_map();
	  break;
	case 17:
	  msg_print("You suddenly feel incredibly pathetic.");
	  for(i=0;i<=A_LUC;i++)
	    {
	      if (py.stats.max_stat[i]>18)
		py.stats.max_stat[i]=16;
	      else
		py.stats.max_stat[i]-=5;
	      if (py.stats.max_stat[i]<3)
		py.stats.max_stat[i]=3;
	      py.stats.cur_stat[i]=py.stats.max_stat[i];
	    }
	  py.flags.word_recall=2;
	  py.misc.max_exp=0;
	  py.misc.exp=0;
	  py.misc.max_dlv=0;
	  for(i=0;i<S_NUM;i++)
	    sskill(-i,-120);
	  break;
	case 18:
	  msg_print("You feel very non-magical.");
	  for(i=0;i<=A_LUC;i++)
	    py.stats.cur_stat[i]=py.stats.max_stat[i];
	  sskill(-S_MAGIC,-80);
	  sskill(-S_MPOWER,-80);
	  break;
	}
      prt_level();
      prt_title();
      prt_gold();
      prt_cmana();
      prt_mhp();
      prt_chp();
      prt_experience();
      for(i=0;i<=A_LUC;i++)
	prt_stat(i);
      prt_map();
      prt_state();
      break;
    case 2: /* Orb of Clairvoyance */
      break;
    default:
      msg_print("Internal error in activate_item() ");
      break;
    }
  if (ident) {
    if (!known1_p(i_ptr)) {
      m_ptr = &py.misc;
      /* round half-way case up */
      m_ptr->exp += (i_ptr->level +(get_level() >> 1)) /
	get_level();
      prt_experience();
      identify(&which);
    }
  }	else if (!known1_p(i_ptr)) {
    sample (i_ptr);
  }
}
