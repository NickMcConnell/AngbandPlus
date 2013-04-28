/* rods.c : rod code

   Copyright (c) 1989 Andrew Astrand 1991

   Do what you like with it

   I will sucker! ~Ludwig
*/

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

int direction(dir)
  int *dir;
{
  if (get_dir(NULL, dir)) {
    if (py.flags.confused > 0){
      msg_print("You are confused.");
      do { *dir = randint(9);} while (*dir == 5);
    }
    return 1;
  }
  return 0;
}


/* Rods for the slaughtering    			*/
void activate_rod()
{
  int32u i;
  register int l, ident;
  int item_val, j, k, chance, dir;
  register inven_type *i_ptr;
  register struct misc *m_ptr;

  free_turn_flag = TRUE;
  if (inven_ctr == 0)
    msg_print("But you are not carrying anything.");
  if (!find_range(TV_ROD, TV_NEVER, &j, &k))
      msg_print("You are not carrying any rods.");
  else if (get_item(&item_val, "What do you want to activate?", j, k, 0)) {
    i_ptr = &inventory[item_val];
    free_turn_flag = FALSE;
    ident = FALSE;
    m_ptr = &py.misc;
    chance = smod(S_SAVE) + (stat_adj(A_INT)*2) -
      (int)((i_ptr->level>50)?50:i_ptr->level)
      + (smod(S_DEVICE));
    if (py.flags.confused > 0)
      chance = chance / 2;
    if (chance <= 0)  chance = 1;
    else if (randint(chance) < USE_DEVICE)
      msg_print("You failed to use the rod properly.");
    else if (i_ptr->timeout <= 0) {
      i = i_ptr->flags;
      k = char_row;
      l = char_col;
      switch(i) {
      case RD_LT:
	light_area(k,l);
	msg_print("Blue shimmering light radiates out from you!");
	for(i=1;i<=9;i++)
	  if (i!=5)
	    light_line(i, char_row, char_col);
	ident = TRUE;
	i_ptr->timeout=5;
	break;
      case RD_ILLUME:
	wizard_light(TRUE);
	ident=TRUE;
	i_ptr->timeout=900;
	break;
      case RD_AC_BLTS: /* Acid , New */
	if (!direction(&dir)) goto no_charge;
	fire_bolt(GF_ACID,dir,k,l,damroll(8,10),"Acid Bolt");
	ident=TRUE;
	i_ptr->timeout=12;
	break;
      case RD_LT_BLTS: /* Lightning */
	if (!direction(&dir)) goto no_charge;
	fire_bolt(GF_LIGHTNING, dir, k, l, damroll(6, 9),
		  spell_names[10]);
	ident = TRUE;
	i_ptr->timeout=11;
	break;
      case RD_SONIC_BLTS: /* Sound */
	if (!direction(&dir)) goto no_charge;
	fire_bolt(GF_SOUND, dir, k, l, damroll(11, 11),
		  "wave of sound");
	ident = TRUE;
	i_ptr->timeout=13;
	break;
      case RD_CHAOS_BLTS: /* Confusion */
	if (!direction(&dir)) goto no_charge;
	fire_bolt(GF_CONFUSION, dir, k, l, damroll(10, 12),
		  "colored swirl");
	ident = TRUE;
	i_ptr->timeout=15;
	break;
      case RD_POLY:
	if (!direction(&dir)) goto no_charge;
	ident = poly_monster(dir, k, l);
	i_ptr->timeout=25;
	break;
      case RD_SLOW_MN:
	if (!direction(&dir)) goto no_charge;
	ident = speed_monster(dir, k, l, -1);
	i_ptr->timeout=20;
	break;
      case RD_SLEE_MN:
	if (!direction(&dir)) goto no_charge;
	ident = sleep_monster(dir, k, l);
	i_ptr->timeout=18;
	break;
      case RD_DRAIN:
	if (!direction(&dir)) goto no_charge;
	ident = drain_life(dir, k, l, 100);
	i_ptr->timeout=23;
	break;
      case RD_TELE:
	if (!direction(&dir)) goto no_charge;
	ident = teleport_monster(dir, k, l);
	i_ptr->timeout=25;
	break;
      case RD_DISARM:
	if (!direction(&dir)) goto no_charge;
	ident = disarm_all(dir, k, l);
	i_ptr->timeout=30;
	break;
      case RD_LT_BALL:
	if (!direction(&dir)) goto no_charge;
	fire_ball(GF_LIGHTNING, dir, k, l, 50, "Lightning Ball");
	ident = TRUE;
	i_ptr->timeout=23;
	break;
      case RD_CD_BALL:
	if (!direction(&dir)) goto no_charge;
	fire_ball(GF_FROST, dir, k, l, 60, "Cold Ball");
	ident = TRUE;
	i_ptr->timeout=25;
	break;
      case RD_FR_BALL:
	if (!direction(&dir)) goto no_charge;
	fire_ball(GF_FIRE, dir, k, l, 90, spell_names[30]);
	ident = TRUE;
	i_ptr->timeout=30;
	break;
      case RD_AC_BALL:
	if (!direction(&dir)) goto no_charge;
	fire_ball(GF_ACID, dir, k, l, 80, "Acid Ball");
	ident = TRUE;
	i_ptr->timeout=27;
	break;
      case RD_MAPPING:
	map_area();
	ident = TRUE;
	i_ptr->timeout=99;
	break;
      case RD_IDENT:
	ident_spell();
	ident = TRUE;
	i_ptr->timeout=10;
	break;
      case RD_CURE:
	if ((cure_blindness()) || (cure_poison()) ||
	    (cure_confusion()) || (py.flags.stun>0) || (py.flags.cut>0))
	  ident = TRUE;
	if (py.flags.stun>0) {
	  if (py.flags.stun>50) {
	    py.misc.ptohit+=20;
	    py.misc.ptodam+=20;
	  } else {
	    py.misc.ptohit+=5;
	    py.misc.ptodam+=5;
	  }
	  py.flags.stun=0;
	  ident = TRUE;
	  msg_print("You're head stops stinging.");
	} else if (py.flags.cut>0) {
	  py.flags.cut=0;
	  ident = TRUE;
	  msg_print("You feel better.");
	}
	i_ptr->timeout=888;
	break;
      case RD_DRAINING:
	if (!direction(&dir)) goto no_charge;
	drain_life(dir,char_row,char_col,-200);
	ident = TRUE;
	i_ptr->timeout=80;
	break;
      case RD_RECALL:
	if (py.flags.word_recall == 0)
          py.flags.word_recall = 25 + randint(30);
        msg_print("The air about you becomes charged.");
	ident = TRUE;
	i_ptr->timeout=60;
        break;
      case RD_PROBE:
	probing();
	ident = TRUE;
	i_ptr->timeout=50;
	break;
      case RD_DETECT:
	detection();
	ident = TRUE;
	i_ptr->timeout=5;
	break;
      case RD_RESTORE:
	if (restore_level() || res_stat(A_STR) || res_stat(A_INT) ||
	    res_stat(A_WIS) || res_stat(A_DEX) || res_stat(A_CON) ||
	    res_stat(A_CHR))
	  ident = TRUE;
	i_ptr->timeout=999;
	break;
      case RD_SPEED:
	if (py.flags.fast == 0) ident = TRUE;
	py.flags.fast += randint(30) + 15;
	i_ptr->timeout=99;
	break;
      default:
	msg_print("Internal error in rods() ");
	break;
      }
      if (ident) {
	if (!known1_p(i_ptr)) {
	  m_ptr = &py.misc;
	  /* round half-way case up */
	  m_ptr->exp += (i_ptr->level +(get_level() >> 1)) /
	    get_level();
	  prt_experience();

	  identify(&item_val);
	  i_ptr = &inventory[item_val];
	}
      }	else if (!known1_p(i_ptr)) {
	sample (i_ptr);
      }
    no_charge: ;
    } else {
      msg_print("The rod is currently exhausted.");
    }
  }
}
