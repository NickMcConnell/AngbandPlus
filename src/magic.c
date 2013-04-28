/* magic.c: code for mage spells

   Copyright (c) 1989 James E. Wilson, Robert A. Koeneke

   This software may be copied and distributed for educational, research, and
   not for profit purposes provided that this copyright and statement are
   included in all such copies. */

#include "monster.h"
#include "constant.h"
#include "config.h"
#include "types.h"
#include "externs.h"

extern void jamdoor(int);
int door_creation();
int detect_enchantment();
int stair_creation();
void player_breathe(void);

/* Throw a magic spell					-RAK-	*/
void cast()
{
  int i, j, item_val, dir, spell;
  int choice, chance, result;
  register struct flags *f_ptr;
  register struct misc *p_ptr;
  register spell_type *m_ptr;

  free_turn_flag = TRUE;
  if (py.misc.realm == NECROS)
    necros();
  else if (py.flags.blind > 0)
    msg_print("You can't see to read your spell book!");
  else if (no_light())
    msg_print("You have no light to read by.");
  else if (py.flags.shero)
    msg_print("You are too berserk to cast spells!");
  else if (py.flags.confused > 0)
    msg_print("You are too confused.");
  else if (py.misc.realm != MAGE)
    msg_print("You can't cast spells!");
  else if (no_magic())
    msg_print("Your magical abilities are unusable right now.");
  else if (!find_range(TV_MAGIC_BOOK, TV_NEVER, &i, &j))
    msg_print("But you are not carrying any spell-books!");
  else if (get_item(&item_val, "Use which spell-book?", i, j, 0))
    {
      spell=smod(S_MAGIC)+(py.skills.cur_skill[magical[MAGE]]/20)-5;
      /* Determines effective spell level */
      result = cast_spell("Cast which spell?", item_val, &choice, &chance,
			  TV_MAGIC_BOOK);
      if (py.flags.stun>50) chance+=25;
      else if (py.flags.stun>0) chance+=15;
      if (result < 0)
	msg_print("You don't know any spells in that book.");
      else if (result > 0)
	{
	  m_ptr = &magic_spell[py.misc.realm][choice];
	  free_turn_flag = FALSE;

	  if (randint(100) < chance)
	    msg_print("You failed to get the spell off!");
	  else
	    {
	      /* Spells.  */
	      switch(choice+1)
		{
		case 1:
		  if (get_dir(NULL, &dir))
		    fire_bolt(GF_MAGIC_MISSILE, dir, char_row, char_col,
			      damroll(3+(spell-1)/4, 4),spell_names[0]);
		  break;
		case 2:
		  (void) detect_monsters();
		  break;
		case 3:
		  teleport(10);
		  break;
		case 4:
		  (void) light_area(char_row, char_col);
		  break;
		case 5: /* treasure/object detection */
		  (void) detect_treasure();
		  (void) detect_object();
		  break;
		case 6:
		  (void) detect_sdoor();
		  (void) detect_trap();
		  break;
		case 7: /* Fetch Object */
		  if (get_dir(NULL, &dir))
		    (void) fetch(dir, char_row, char_col, spell*15);
		  break;
		case 8:
		  jamdoor(1); /* Magic door jamming */
		  break;
		case 9:
		  if (get_dir(NULL, &dir))
		    fire_ball(GF_POISON_GAS, dir, char_row, char_col,
			      10+(spell/6),
			      spell_names[8]);
		  break;
		case 10:
		  if (get_dir(NULL, &dir))
		    (void) scare_monster(dir, char_row, char_col);
		  break;
		case 11:
		  if (get_dir(NULL, &dir))
		    fire_bolt(GF_LIGHTNING, dir, char_row, char_col,
			      damroll(3+((spell-5)/6),5)
			      , spell_names[10]);
		  break;
		case 12:
		  if (get_dir(NULL, &dir))
		    fire_ball(GF_MAGIC_MISSILE, dir, char_row, char_col,
			      15+(spell/5),"Magic Vortex");
		  break;
		case 13:
		  if (get_dir(NULL, &dir))
		    (void) sleep_monster(dir, char_row, char_col);
		  break;
		case 14:
		  (void) cure_poison();
		  break;
		case 15:
		  teleport((int)(spell*5));
		  break;
		case 16:
		  if (get_dir(NULL, &dir)) {
 		     fire_bolt(GF_MAGIC_MISSILE, dir, char_row, char_col,
			       damroll(4+(spell-5)/4,6),"Mana Bolt");
		  }
        	  break;
		case 17:
		  if (get_dir(NULL, &dir))
		    fire_bolt(GF_FROST, dir, char_row, char_col,
			      damroll(4+((spell-5)/5),6),
			      spell_names[16]);
		  break;
		case 18:
		  if (get_dir(NULL, &dir))
		    (void) wall_to_mud(dir, char_row, char_col);
		  break;
		case 19:
		  create_food();
		  break;
		case 20:
		  (void) recharge(30+spell/3);
		  break;
		case 21:
		  (void) sleep_monsters2();
		  break;
		case 22:
		  if (get_dir(NULL, &dir))
		    (void) poly_monster(dir, char_row, char_col);
		  break;
		case 23:
		  (void) ident_spell();
		  break;
		case 24:
		  if (get_dir(NULL, &dir))
		    fire_ball(GF_LIGHTNING, dir, char_row, char_col,
			      30+spell/4,
			      "Lightning Ball");
		  break;
		case 25:
		  if (get_dir(NULL, &dir))
		    fire_bolt(GF_FIRE, dir, char_row, char_col,
			      damroll(6+((spell-7)/4),7),
			      spell_names[24]);
		  break;
		case 26:
		  if (get_dir(NULL, &dir))
		    (void)speed_monster(dir, char_row, char_col, -1);
		  break;
		case 27:
		  if (get_dir(NULL, &dir))
		    fire_ball(GF_FROST, dir, char_row, char_col,
			      60+spell/2,
			      spell_names[26]);
		  break;
		case 28:
		  (void) recharge(50+spell/2);
		  break;
		case 29:
		  if (get_dir(NULL, &dir))
		    (void) teleport_monster(dir, char_row, char_col);
		  break;
		case 30:
		  f_ptr = &py.flags;
		  if (f_ptr->fast <= 0)
		    f_ptr->fast += randint(20) + spell;
		  else
		    f_ptr->fast += randint(5);
		  break;
		case 31:
		  if (get_dir(NULL, &dir))
		    fire_ball(GF_FIRE, dir, char_row, char_col
			      ,90+spell/2,
			      spell_names[30]);
		  break;
		case 32:
		  destroy_area(char_row, char_col);
		  break;
		case 33: /* Restores some random amount of mana */
		  msg_print("You feel your head clear a bit.");
		  i=py.misc.cmana+randint(35);
		  if (i>py.misc.mana) i=py.misc.mana;
		  py.misc.cmana=i;
		  break;
		case 34: /*door creation*/
		  (void) door_creation();
		  break;
		case 35: /* Stair creation */
		  (void) stair_creation();
		  break;
		case 36: /* Word of Recall */
		  if (py.flags.word_recall == 0)
		    py.flags.word_recall = 25 + randint(30);
		  msg_print("The air about you becomes charged.");
		  break;
		case 37: /* Sonic Storm */
		  if (get_dir(NULL, &dir))
		    fire_ball(GF_SOUND, dir, char_row, char_col,
			      150+spell*3/2,"wave of sound");
		  break;
		case 38: /* Acid Ball */
		  if (get_dir(NULL, &dir))
		    fire_ball(GF_ACID, dir, char_row, char_col,
			      220+spell*3/2,"Acid Ball");
		  break;
		case 39: /* Plasma Vortex */
		  if (get_dir(NULL,&dir))
		    fire_ball(GF_LIGHTNING,dir,char_row,char_col,
			      240+spell*3/2,"Plasma Vortex");
		  break;
		case 40: /* Mana Storm */
		  if (get_dir(NULL,&dir))
		    fire_ball(GF_MAGIC_MISSILE,dir,char_row,char_col,
			      260+spell*3/2,"Swirl of magic");
		  break;
		case 41: /* Chaos Vortex */
		  if (get_dir(NULL, &dir))
		    fire_ball(GF_CONFUSION, dir, char_row, char_col,
			      180+spell*3/2,
			      "swirling colors");
		  break;
		case 42: /* Charge Floor */
		  notarget=1;
		  if (get_dir(NULL, &dir))
		    fire_bolt(GF_LIGHTNING, dir, char_row, char_col,
			      -(180+spell*3/2),
			      "lightning");
		  break;
		case 43: /* Creeping Walls */
		  notarget=1;
		  if (get_dir(NULL, &dir))
		    fire_bolt(GF_MAGIC_MISSILE, dir, char_row, char_col,
			      -(190+spell*3/2),
			      "sharp spike");
		  break;
		case 44: /* Hellfire */
		  if (get_dir(NULL, &dir))
		    fire_ball(GF_HOLY_ORB, dir, char_row, char_col, 300,
			      "Hellfire");
		  break;
                case 45: /*Detect Evil */
                  (void) detect_general(0,EVIL,"evil");
                  break;
                case 46: /* Detect Enchantment */
                  (void) detect_enchantment();
                  break;
                case 47:
                  recharge(100+spell*2);
                  break;
                case 48:
		  (void) genocide(TRUE);
                  break;
                case 49:
                  (void) mass_genocide(TRUE);
                  break;
                case 50:
	          py.flags.resist_heat += randint(20) + 20;
                  break;
                case 51:
	          py.flags.resist_cold += randint(20) + 20;
                  break;
                case 52:
	          py.flags.resist_acid += randint(20) + 20;
                  break;
                case 53:
	          py.flags.resist_poison += randint(20) + 20;
                 break;
                case 54:
	          py.flags.resist_heat += randint(20) + 20;
	          py.flags.resist_cold += randint(20) + 20;
	          py.flags.resist_light += randint(20) + 20;
	          py.flags.resist_poison += randint(20) + 20;
	          py.flags.resist_acid += randint(20) + 20;
                  break;
                case 55:
                  py.flags.hero += randint(25)+25;
                  break;
                case 56:
                  py.flags.shield += randint(20)+30;
		  calc_bonuses();
		  prt_pac(); 
		  calc_mana(prime_stat[py.misc.realm]);
                  msg_print("A mystic shield forms around your body!");
                  break;
                case 57:
                  py.flags.shero += randint(25)+25;
                  break;
                case 58:
		  if (py.flags.fast <= 0)
		    py.flags.fast += randint(80)+90+spell;
		  else
		    py.flags.fast += randint(5)+spell/5;
		  break;
                case 59:
                  py.flags.invuln += randint(8)+6;
                  break;
		default:
		  break;
		}
	      /* End of spells.				     */
	      if (!free_turn_flag)
		{
		  p_ptr = &py.misc;
		  if (choice<32) {
		    if ((spell_worked & (1L << choice)) == 0) {
		      p_ptr->exp += m_ptr->sexp << 2;
		      spell_worked |= (1L << choice);
		      prt_experience();
		    }
		  } else {
		    if ((spell_worked2 & (1L << (choice-32))) == 0) {
		      p_ptr->exp += m_ptr->sexp << 2;
		      spell_worked2 |= (1L << (choice-32));
		      prt_experience();
		    }
		  }
		}
	    }
	  p_ptr = &py.misc;
	  if (!free_turn_flag)
	    {
	      if (m_ptr->smana > p_ptr->cmana)
		{
		  msg_print("You faint from the effort!");
		  py.flags.paralysis =
		    randint((int)(5*(m_ptr->smana-p_ptr->cmana)));
		  p_ptr->cmana = 0;
		  p_ptr->cmana_frac = 0;
		  if (randint(3) == 1)
		    {
		      msg_print("You have damaged your health!");
		      (void) dec_stat (A_CON);
		    }
		}
	      else
		p_ptr->cmana -= m_ptr->smana;
	      prt_cmana();
	    }
	}
    }
}
