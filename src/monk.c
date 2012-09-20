/* monk.c: code for monk karate techniques

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


/* Do a karate technique */
void monk()
{
  int i, j, item_val, dir, y, x, spell;
  int choice, chance, result;
  register spell_type *s_ptr;
  register struct misc *m_ptr;
  register struct flags *f_ptr;
  register inven_type *i_ptr;

  spell=(py.misc.lev)*(class[py.misc.pclass].magicity+
		       race[py.misc.prace].base_mag)/200;
  free_turn_flag = TRUE;
  if (py.flags.blind > 0)
    msg_print("You can't see to read your book!");
  else if (no_light())
    msg_print("You have no light to read by.");
  else if (py.flags.confused > 0)
    msg_print("You are too confused.");
  else if (inven_ctr == 0)
    msg_print ("But you are not carrying anything!");
  else if (!find_range(TV_MONK_BOOK, TV_NEVER, &i, &j))
    msg_print ("You are not carrying any Karate Books!");
  else if (get_item(&item_val, "Use which Karate Book?", i, j, 0))
    {
      result = cast_spell("Perform which technique?", item_val, &choice, &chance);
      y=char_row;
      x=char_col; /* Used for Elemental Critical attacks */
      if (result < 0)
	msg_print("You don't know any techniques in that book.");
      else if (result > 0)
	{
	  s_ptr = &magic_spell[py.misc.pclass-1][choice];
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
		  (void) detect_monsters();
		  break;
		case 2:
		  (void) remove_fear();
		  break;
		case 3:
		  change(10,0,0,0,choice);
		  break;
		case 4:
		  (void) light_area(char_row, char_col);
		  break;
		case 5:
		  change(-3,-1,2,0,choice);
		  break;
		case 6:
		  (void) detect_sdoor();
		  (void) detect_trap();
		  break;
		case 7:
		  change(-15,4,4,0,choice);
		  break;
		case 8:
		  if (get_dir(NULL, &dir))
		    (void) confuse_monster(dir, char_row, char_col);
		  break;
		case 9:
		  teleport(20);
		  break;
		case 10:
		  change(0,1,3,0,choice);
		  break;
		case 11:
		  if (py.flags.cut>0) {
		    py.flags.cut=0;
		    msg_print("Your wounds heal.");
		  }
		  break;
		case 12:
		  change(-2,0,9,0,choice);
		  break;
		case 13:
		  change(9,0,3,0,choice);
		  break;
		case 14:
		  (void) sleep_monsters2();
		  break;
		case 15:
		  (void) hp_player(damroll(6+spell/8,7+spell/8));
		  break;
		case 16:
		  teleport(150);
		  break;
		case 17:
		  py.flags.word_recall+=70-randint(spell);
		  msg_print("You feel a tingle in the air.");
		  break;
		case 18:
		  (void) create_food();
		  break;
		case 19:
		  detect_inv2(randint(spell/2+10)+spell/2+10);
		  py.flags.tim_infra+=spell+20;
		  break;
		case 20:
		  change(10,1,4,0,choice);
		  break;
		case 21:
		  change(40,0,-2,0,choice);
		  break;
		case 22:
		  /* Flaming Touch */
		  if (get_dir(NULL, &dir))
		    {
		    if (mmove(dir, &y, &x))
		      {
		       if (panel_contains(y,x) && cave[y][x].cptr)
			 if (cave[y][x].cptr)
		          fire_bolt(GF_FIRE,dir, char_row, char_col,
				    350+spell*5,"Flaming Hand");
		      }
		    }
		  break;
		case 23:
		  /* Wintry Touch */
		  if (get_dir(NULL, &dir))
		    {
		    if (mmove(dir, &y, &x))
		      {
		       if (panel_contains(y,x))
			 if (cave[y][x].cptr)
		          fire_bolt(GF_FROST,dir, char_row, char_col,
				    375+spell*5,"Frosty Hand");
		      }
		    }
		  break;
		case 24:
		  /* Glowing Touch */
		  if (get_dir(NULL, &dir))
		    {
		    if (mmove(dir, &y, &x))
		      {
		       if (panel_contains(y,x))
			if (cave[y][x].cptr)
		          fire_bolt(GF_LIGHTNING,dir, char_row, char_col,
				    400+spell*5,"Glowing Hand");
		      }
		    }
		  break;
		case 25:
                  /* Whirlwind */
		  for(j=1;j<10;j++)
		   if (j!=5)
		     if (mmove(j, &y, &x))
		       {
		        if (panel_contains(y,x))
			 if (cave[y][x].cptr)
			  py_attack(char_row, char_col, 0);
			(void) mmove(9-j, &y, &x); /* Move back for more */
		       }
		  break;
		case 26:
		  (void) hp_player(200);
		  break;
		case 27:
		  change(8,0,6,1,choice);
		  break;
		case 28:
		  change(20,0,4,1,choice);
		  break;
		case 29:
		  if (py.flags.tim_invis)
		    py.flags.tim_invis+=10+randint(10);
		  else
		    {
		      msg_print("You feel your body fade away.");
		      py.flags.tim_invis+=40+randint(spell);
		    }
		  break;
		case 30:
                  py.flags.shield+=30+randint(spell);
                  calc_bonuses();
		  prt_pac();
                  calc_mana(A_DEX);
		  msg_print("Your body begins to shimmer!");
    		  break;
		case 31:
		  wizard_light(TRUE);
		  break;
		case 32:
		  hp_player(400);
		  break;
		case 33:
		  if (get_dir(NULL, &dir))
		   {
		     for(i=1;i<=5;i++)
		       (void) wall_to_mud(dir, char_row, char_col);
		     build_wall(9-dir,char_row,char_col);
	           }
		  break;
		case 34:
		  py.flags.resist_heat+=40+spell;
		  py.flags.resist_cold+=40+spell;
		  break;
		case 35: /* Ninjutsu fighting style */
		  py.flags.ac_mod=30;
		  py.flags.tohit=6;
		  py.flags.todam=1;
		  if (py.flags.tspeed==0)
		    change_speed(-1); /* Go faster */
		  py.flags.tspeed=1;
		  py.flags.whichone=choice | 0x80; /* Ninja bonuses */
		  (void) calc_bonuses();
		  break;
		case 36:
                  py.flags.invuln+=10+randint(spell/2);
                  calc_bonuses();
		  prt_pac();
                  calc_mana(A_DEX);
    		  break;
		case 37: /* Death Touch */
		  if (get_dir(NULL, &dir))
		    {
		    if (mmove(dir, &y, &x))
		      {
		       if (panel_contains(y,x))
			 {
		          if (cave[y][x].cptr)
			    fire_bolt(GF_MAGIC_MISSILE,dir, char_row, char_col,
				    3000,"Death Touch");
		         }
		      }
		    }
		  break;
		case 38:
		  (void) restore_level();
		  break;
		case 39: /* Internal Insight; self-knowledge */
		  self_knowledge();
		  break;
		case 40: /* Restoration */
		  i=randint(A_CHR)+1;
		  switch(i)
		  {
		  case A_STR:
		  if (res_stat (A_STR))
		    msg_print("You feel warm all over.");
		  break;
	      	  case A_INT:
		  if (res_stat (A_INT))
		    msg_print("You have a warm feeling.");
		  break;
		  case A_WIS:
		  if (res_stat (A_WIS))
		    msg_print("You feel your wisdom returning.");
		  break;
		case A_DEX:
		  if (res_stat(A_DEX))
		    msg_print("You feel less clumsy.");
		  break;
		case A_CON:
		  if (res_stat (A_CON))
		    msg_print("You feel your health returning!");
		  break;
		case A_CHR:
		  if (res_stat (A_CHR))
		    msg_print("You feel your looks returning.");
		  break;
	          }
		  break;
		case 41: /* Total Restoration */
		  if (res_stat (A_STR))
		    msg_print("You feel warm all over.");
		  if (res_stat (A_INT))
		    msg_print("You have a warm feeling.");
		  if (res_stat (A_WIS))
		    msg_print("You feel your wisdom returning.");
		  if (res_stat(A_DEX))
		    msg_print("You feel less clumsy.");
		  if (res_stat (A_CON))
		    msg_print("You feel your health returning!");
		  if (res_stat (A_CHR))
		    msg_print("You feel your looks returning.");
		  break;
		case 42:
		  (void) hp_player(3000);
		  break;
		case 43:
		  if (get_dir(NULL, &dir))
		    (void) fire_bolt(GF_MAGIC_MISSILE, dir, char_row, char_col,
				      50,"White Thread");
		  break;
		case 44:
		  if (get_dir(NULL,&dir))
		    (void) fire_ball(GF_MAGIC_MISSILE, dir, char_row, char_col,
				      75,"Blast of Wind");
		  break;
		case 45:
		  if (get_dir(NULL,&dir))
		    (void) fire_ball(GF_LIGHTNING, dir, char_row, char_col,
				     100,"White Flash");
		  break;
		case 46:
		  (void) speed_monsters(-3);
		  break;
		case 47:
		  if (get_dir(NULL,&dir))
		    (void) fire_bolt(GF_MAGIC_MISSILE, dir, char_row, char_col,
				     150,"Purple Bolt");
		  break;
		case 48:
		  (void) detect_treasure();
		  (void) detect_object();
		  break;
		case 49:
		  (void) detect_evil();
		  break;
		case 50:
		  (void) ident_spell();
		  break;
		case 51: /* Sense Artifacts */
		  break;
		case 52:
		  (void) probing();
		  break;
		case 53:
		  if (get_dir(NULL,&dir))
		    teleport_away(dir, char_row, char_col);
		  break;
		case 54: /* Create Stairs */
		  break;
		case 55: /* Create Doors */
		  break;
		case 56: /* Create Walls */
		  break;
		case 57: /* Escape Level */
		  break;
		default:
		  break;
		}
	      /* End of techniques. */
	      if (!free_turn_flag)
		{
		  m_ptr = &py.misc;
		  if (choice<32) {
		    if ((spell_worked & (1L << choice)) == 0) {
		      m_ptr->exp += s_ptr->sexp << 2;
		      spell_worked |= (1L << choice);
		      prt_experience();
		    }
		  } else {
		    if ((spell_worked2 & (1L << (choice-32))) == 0) {
		      m_ptr->exp += s_ptr->sexp << 2;
		      spell_worked2 |= (1L << (choice-32));
		      prt_experience();
		    }
		  }
		}
	    }
	  m_ptr = &py.misc;
	  if (!free_turn_flag)
	    {
	      if (s_ptr->smana > m_ptr->cmana)
		{
		  msg_print("You faint from fatigue!");
		  py.flags.paralysis =
		    randint((int)(5 * (s_ptr->smana-m_ptr->cmana)));
		  m_ptr->cmana = 0;
		  m_ptr->cmana_frac = 0;
		  if (randint(3) == 1)
		    {
		      msg_print("You have damaged your health!");
		      (void) dec_stat (A_CON);
		    }
		}
	      else
		m_ptr->cmana -= s_ptr->smana;
	      prt_cmana();
	    }
	}
    }
}
