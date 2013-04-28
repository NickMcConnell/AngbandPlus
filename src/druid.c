/* druid.c:  Code to perform "nature spells"

   Copyright (c) 1989 James E. Wilson, Robert A. Koeneke

   This software may be copied and distributed for educational, research, and
   not for profit purposes provided that this copyright and statement are
   included in all such copies. */

#include "constant.h"
#include "monster.h"
#include "config.h"
#include "types.h"
#include "externs.h"

/* Mod to AC, +tohit, +todam, Speed, Offset of Karate type */
void change(ac,tohit,todam,spd,i)
short int ac,tohit,todam,spd,i;
{
 py.flags.ac_mod=ac;
 py.flags.tohit=tohit;
 py.flags.todam=todam;
 if (py.flags.tspeed!=spd)
 {
  if (spd)
   change_speed(-1); /* Go faster */
  else
   change_speed(1); /* Go slower */
 }
 py.flags.tspeed=spd;
 py.flags.whichone=i;
 (void) calc_bonuses();
}


int attack(typ,dam,bolt,hit)
int typ,dam,bolt;
char *hit;
{
 int result,da,dir;
 result=1;
 da=weather(typ,dam);
 if (da)
   {
     if (get_dir(NULL,&dir))
       {
	 if (bolt)
	   fire_bolt(typ,dir,char_row,char_col,
		     dam,hit);
	 else
	   fire_ball(typ,dir,char_row,char_col,
		     dam,hit);
	 return 1;
       }  
     return 2;
   }
 return 0;
}

/* Do a druid "prayer" */
void druid()
{
  int i, j, item_val, dir, y, x, spell, result, dam;
  int choice, chance;
  register spell_type *s_ptr;

  free_turn_flag = TRUE;
  if (py.misc.realm != DRUID)
    msg_print("You don't understand nature!");
  else if (py.flags.blind > 0)
    msg_print("You can't see to read your nature book!");
  else if (no_light())
    msg_print("You have no light to read by.");
  else if (py.flags.confused > 0)
    msg_print("You are too confused.");
  else if (py.flags.shero)
    msg_print("You are too berserk to do a technique!");
  else if (no_magic())
    msg_print("Your magical abilities are unusable right now.");
  else if (!find_range(TV_NATURE_BOOK, TV_NEVER, &i, &j))
    msg_print("But you are not carrying any nature books!");
  else if (get_item(&item_val, "Use which nature book?", i, j, 0))
    {
      result = cast_spell("Do which technique?", item_val,
			  &choice, &chance, TV_NATURE_BOOK);
      spell=smod(S_MAGIC)+(py.skills.cur_skill[magical[DRUID]]/10)-10;
      if (result < 0)
	msg_print("You don't know any techniques in that book.");
      else if (result > 0)
	{
	  s_ptr = &magic_spell[py.misc.realm][choice];
	  free_turn_flag = FALSE;
	  if (py.flags.stun>50) chance+=25;
	  else if (py.flags.stun>0) chance+=15;
	  if (randint(100) < chance)
	    msg_print("You didn't do the technique right!");
	  else
	{
	  /* Techniques */
	  switch(choice+1)
	    {
	    case 1:
	      (void) detect_general(1,UNDEAD,"life");
	      break;
	    case 2:
	      (void) predict_weather(10+spell*2);
	      break;
	    case 3:
	      bless(spell*2+10);
	      break;
	    case 4:
	      (void) remove_fear();
	    case 5:
	      (void) light_area(char_row, char_col);
	      break;
	    case 6:
	      (void) detect_sdoor();
	      (void) detect_trap();
	      break;
	    case 7:
	      (void) hp_player(3,4);
	      break;
	    case 8:
	      (void) cure_poison();
	      break;
	    case 9:
	      if (!py.flags.word_recall)
		{
		  msg_print("You feel disoriented.");
		  py.flags.word_recall+=30;
		}
	      break;
	    case 10:
	      (void) sleep_monsters2();
	      break;
	    case 11:
	      (void) hp_player(damroll(5,6)+spell*2/3);
	      break;
	    case 12:
	      (void) banish_creature(ANIMAL,20+spell);
	      break;
	    case 13:
	      teleport(10+spell*2);
	      break;
	    case 14:
	      py.flags.resist_heat+=20+spell+randint(spell);
	      py.flags.resist_cold+=20+spell+randint(spell);
	      py.flags.resist_acid+=20+spell+randint(spell);
	      py.flags.resist_poison+=20+spell+randint(spell);
	      py.flags.resist_light+=20+spell+randint(spell);
	      break;
	    case 15:
	      (void) hp_player(damroll(6,7)+spell);
	      break;
	    case 16:
	      (void) create_food();
	      break;
	    case 17:
	      if (!attack(GF_POISON_GAS,20+spell/4,1,"black gas"))
		msg_print("It's too windy.");
	      break;
	    case 18:
	      if (!attack(GF_LIGHTNING,25+spell/4,1,"chain lightning"))
		msg_print("It's too moist and too warm.");
	      break;
	    case 19:
	      (void) map_area();
	      break;
	    case 20:
	      if (!attack(GF_FROST,30+spell/4,1,"icy spear"))
	       msg_print("It's too warm.");
	      break;
	    case 21:
	      (void) hp_player(damroll(10,10)+spell*3/2);
	      break;
	    case 22:
	      if (!attack(GF_FIRE,35+spell/4,1,"flaming stone"))
		msg_print("It's too cold out.");
	      break;
	    case 23:
	      if (!attack(GF_MAGIC_MISSILE,40+spell/4,1,"fierce wind"))
		msg_print("It's too still out.");
	      break;
	    case 24:
	      if (!attack(GF_FROST,45+spell/2,0,"blizzard"))
		msg_print("It's too warm out.");
	      break;
	    case 25:
	      if (!attack(GF_FIRE,50+spell/2,0,"blast of heat"))
		msg_print("It's too cold out.");
	      break;
	    case 26:
	      if (!attack(GF_LIGHTNING,55+spell/2,0,"sheet lightning"))
		msg_print("It's too warm and moist out.");
	      break;
	    case 27:
	      if (!attack(GF_POISON_GAS,60+spell/2,0,"poison gases"))
		msg_print("It's too windy out.");
	      break;
	    case 28:
	      change(5,-6,2,0,choice);
	      break;
	    case 29:
	      change(8,-3,1,0,choice);
	      break;
	    case 30:
	      (void) hp_player(1000);
	      break;
	    case 31:
	      py.flags.fast+=50+spell+randint(spell);
	      break;
	    case 32:
	      i=py.misc.cmana+randint(50);
	      if (i>py.misc.mana) i=py.misc.mana;
	      py.misc.cmana=i;
	      msg_print("You feel your head clear a bit.");
	      break;
	    case 33: /* Form of the Bear */
	      change(10,-3,10,0,choice);
	      break;
	    case 34: /* Form of the Lion */
	      change(15,-1,3,0,choice);
	      break;
	    case 35: /* Form of the Gazelle */
	      change(5,1,-4,1,choice);
	      break;
	    case 36: /* Form of the Cheetah */
	      change(35,4,-1,1,choice);
	      break;
	    case 37: /* Form of the Dragon */
	      change(20,2,5,1,choice);
	      break;
	    case 38: /* Understand Item */
	      ident_spell();
	      break;
	    case 39: /* Detect Material Goods */
	      (void) detect_object();
	      (void) detect_treasure();
	      (void) detect_enchantment();
	      break;
	    case 40: /* Probing */
	      (void) probing();
	      break;
	    case 41: /* Magical Staircase */
	      (void) stair_creation();
	      break;
	    case 42: /* View Dungeon */
	      (void) wizard_light(TRUE);
	      break;
	    case 43: /* Battle Blessing */
	      bless(60+spell);
	      break;
	    case 44: /* Iron Will */
	      if (!py.misc.timeout)
		{
		  msg_print("You feel your will to live strengthen.");
		  py.misc.timeout+=200+spell*3/2;
		}
	      else
		py.misc.timeout+=20+spell;
	      break;
	    case 45: /* Fighting Fury */
	      py.flags.shero+=50+spell;
	      break;
	    case 46: /* Mystic Shield */
	      py.flags.shield+= randint(spell)+10+spell;
	      calc_bonuses();
	      prt_pac();
	      calc_mana(prime_stat[py.misc.realm]);
	      msg_print("A mystic shield forms around your body!");
	      break;
	    case 47: /* Mass Sleep */
	      (void) mass_sleep();
	      break;
	    case 48: /* Essence of Flame */
	      if (!attack(GF_FIRE,200+spell*3/2,0,"pulsing heat"))
		msg_print("It's too cold.");
	      break;
	    case 49: /* Absolute Zero */
	      if (!attack(GF_FROST,210+spell*3/2,0,"force of cold"))
		msg_print("It's too warm.");
	      break;
	    case 50: /* Lethal Plague */
	      if (!attack(GF_POISON_GAS,220+spell*3/2,0,"deadly plague"))
		msg_print("It's too windy.");
	      break;
	    case 51: /* Hurricane */
	      if (!attack(GF_SOUND,230+spell*3/2,0,"roaring winds"))
		msg_print("It's too calm.");
	      break;
	    case 52: /* Star Core */
	      if (!attack(GF_LIGHTNING,240+spell*3/2,0,"plasma burst"))
		msg_print("It's too moist and too warm.");
	      break;
	    case 53: /* Arid Wastes */
	      py.flags.flags[F_WEATHER]|=W_DRY;
	      py.flags.flags[F_WEATHER]&=~W_MOIST;
	      msg_print("The air seems dry.");
   	      break;
	    case 54: /* Torrential Downpour */
	      py.flags.flags[F_WEATHER]|=W_MOIST;
	      py.flags.flags[F_WEATHER]&=~W_DRY;
	      msg_print("The air seems moist.");
	      break;
	    case 55: /* Wind Songs */
	      py.flags.flags[F_WEATHER]|=W_WINDY;
	      py.flags.flags[F_WEATHER]&=~W_STILL;
	      msg_print("You feel the wind pick up.");
	      break;
	    case 56: /* Calm Winds */
	      py.flags.flags[F_WEATHER]|=W_STILL;
	      py.flags.flags[F_WEATHER]&=~W_WINDY;
	      msg_print("You feel the wind die down.");
	      break;
	    case 57: /* Call Blizzards */
	      py.flags.flags[F_WEATHER]|=W_COOL;
	      py.flags.flags[F_WEATHER]&=~W_WARM;
	      msg_print("You feel the air grow chilly.");
	      break;
	    case 58: /* Scorch Earth */
	      py.flags.flags[F_WEATHER]|=W_WARM;
	      py.flags.flags[F_WEATHER]&=~W_COOL;
	      msg_print("You feel uncomfortably warm.");
	      break;
	    default:
	      break;
	    }
	  /* End of techniques. */
	  if (!free_turn_flag)
	    {
	      if (choice<32) {
		if ((spell_worked & (1L << choice)) == 0) {
		  py.misc.exp += s_ptr->sexp << 2;
		  spell_worked |= (1L << choice);
		  prt_experience();
		}
	      } else {
		if ((spell_worked2 & (1L << (choice-32))) == 0) {
		  py.misc.exp += s_ptr->sexp << 2;
		  spell_worked2 |= (1L << (choice-32));
		  prt_experience();
		}
	      }
	    }
	}
      if (!free_turn_flag)
	{
	  if (s_ptr->smana > py.misc.cmana)
	    {
	      msg_print("You faint from fatigue!");
	      py.flags.paralysis =
		randint((int)(5 * (s_ptr->smana-py.misc.cmana)));
	      py.misc.cmana = 0;
	      py.misc.cmana_frac = 0;
	      if (randint(3) == 1)
		{
		  msg_print("You have damaged your health!");
		  (void) dec_stat (A_CON);
		}
	    }
	  else
	    py.misc.cmana -= s_ptr->smana;
	  prt_cmana();
	}
	}
    }
}
