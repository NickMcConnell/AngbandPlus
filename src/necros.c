/* necros.c: code for necromancer spells

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

/* Cast a magic spell					-RAK-	*/
void necros()
{
  int i, j, item_val, dir, spell;
  int choice, chance, result;
  register struct misc *p_ptr;
  register inven_type *i_ptr;
  register spell_type *m_ptr;
  register creature_type *r_ptr;
  register monster_type *cr_ptr;
  vtype out_val, m_name;

  free_turn_flag = TRUE;
  if (py.flags.blind > 0)
    msg_print("You can't see to read your spell book!");
  else if (no_light())
    msg_print("You have no light to read by.");
  else if (py.flags.confused > 0)
    msg_print("You are too confused.");
  else if (py.misc.realm != NECROS)
    msg_print("You can't cast spells!");
  else if (py.flags.shero)
    msg_print("You are too berserk to cast spells!");
  else if (no_magic())
    msg_print("You are unable to cast spells for now.");
  else if (!find_range(TV_DARK_BOOK, TV_NEVER, &i, &j))
    msg_print("But you are not carrying any dark spell-books!");
  else if (get_item(&item_val, "Use which dark spell-book?", i, j, 0))
    {
      spell=smod(S_MAGIC)+(py.skills.cur_skill[magical[NECROS]]/20)-5;
      /* Determines effective spell level */
      result = cast_spell("Cast which spell?", item_val, &choice, &chance,
			  TV_DARK_BOOK);
      if (py.flags.stun>50) chance+=10;
      else if (py.flags.stun>0) chance+=5;
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
		  (void) detect_general(0,UNDEAD,"undead");
		  break;
		case 2:
		  teleport(20);
		  break;
		case 3:
		  (void) remove_curse();
		  break;
		case 4:
		  (void) light_area(char_row, char_col);
		  break;
		case 5: 
		  if (get_dir(NULL, &dir))
		    (void) confuse_monster(dir, char_row, char_col);
		  break;
		case 6:
		  (void) detect_sdoor();
		  (void) detect_trap();
		  break;
		case 7:
		  (void) slow_poison();
		  break;
		case 8:
		  if (get_dir(NULL, &dir))
		    (void) sleep_monster(dir, char_row, char_col);
		  break;
		case 9:
		  (void) dispel_creature(UNDEAD, 4*spell);
		  break;
		case 10:
		  (void) create_food();
		  break;
		case 11:
		  if (!py.flags.resist_cold)
		   py.flags.resist_cold+=30+spell;
		  else
		    py.flags.resist_cold+=10;
		  if (!py.flags.resist_heat)
		    py.flags.resist_heat+=30+spell;
		  else
		    py.flags.resist_heat+=10;
		  break;
		case 12: /* MAY slow all undead nearby */
		  for(i = mfptr - 1; i >= MIN_MONIX ; i--)
		    {
		      cr_ptr=&m_list[i];
		      r_ptr = &c_list[cr_ptr->mptr];
		      monster_name(m_name, cr_ptr, r_ptr);
		      if (los(char_row, char_col, (int)cr_ptr->fy,
			      (int)cr_ptr->fx) && (r_ptr->cdefense & UNDEAD))
			{
			  if ((spell>r_ptr->level ||
			      randint(3)!=1) && !(r_ptr->cdefense & UNIQUE))
			    {
			      cr_ptr->cspeed -= 2;
			      cr_ptr->csleep = 0;
			      if (cr_ptr->ml)
				{
				  (void) sprintf(out_val,
						 "%s slows down.",m_name);
				  msg_print(out_val);
				}
			    }
			}
		    }
		  break;
		case 13: /* Iron Will */
		  if (!py.misc.timeout)
		    {
		      msg_print("You feel your will to live strengthen.");
		      py.misc.timeout+=30+spell*3;
		    }
		  else
		    py.misc.timeout+=5+spell;
		  break;
		case 14:
		  teleport((int)(spell*4));
		  break;
		case 15:
		  if (!py.flags.word_recall)
		    {
		      msg_print("You feel somewhat disoriented.");
		      py.flags.word_recall+=40-(randint(spell)/2);
		    }
		  break;
		case 16:
		  detect_general(1,UNDEAD,"life");
        	  break;
		case 17:
		  msg_print("Your eyes feel infused with power!");
		  py.flags.tim_infra+=50+spell*2;
		  detect_inv2(50+spell*2);
		  break;
		case 18:
		  if (get_dir(NULL, &dir))
		    fire_ball(GF_SPIRIT,dir, char_row, char_col,
			      20+spell/3,"force of death");
		  break;
		case 19:
		  (void) hp_player(50+spell*3/2);
		  (void) cure_poison();
                  if (py.flags.stun>0) {
                    if (py.flags.stun>50) {
                      py.misc.ptohit+=20;
                      py.misc.ptodam+=20;
                    } else {
                      py.misc.ptohit+=5;
                      py.misc.ptodam+=5;
                    }
                    py.flags.stun=0;
                    msg_print("You're head stops stinging.");
                  }
                  if (py.flags.cut>0) {
                    py.flags.cut=0;
                    msg_print("You feel better.");
                  }
		  break;
		case 20:
		  if (!py.flags.protevil)
		    {
		      py.flags.protevil+=50+spell*2;
		      msg_print("You feel safe from evil.");
		    }
		  else
		    py.flags.protevil+=10+spell/4;
		  break;
		case 21:
		  (void) ident_spell();
		  break;
		case 22:
		  if (get_dir(NULL, &dir))
		    drain_life(dir, char_row, char_col, -(35+spell/3));
		  break;
		case 23:
		  speed_monsters(-2);
		  break;
		case 24:
		  if (get_dir(NULL, &dir))
		    (void) teleport_monster(dir, char_row, char_col);
		  break;
		case 25: /* Curse Weapon (+todam exchange for +tohit) */
		  i_ptr = &inventory[INVEN_WIELD];
		  if (i_ptr->flags & TR_CURSED)
		    msg_print("The weapon is already cursed.");
		  else
		    {
		      i_ptr->flags|=TR_CURSED;
		      for(i=1;i<3;i++)
			{ enchant(&i_ptr->todam);
			  enchant(&i_ptr->tohit); }
		      i_ptr->cost=0; /* Can't sell it now! */
		      msg_print("Your weapon became cursed!");
		    }
		  break;
		case 26:
		  if (!py.flags.shero)
		    py.flags.shero+=30+spell*2;
		  else
		    py.flags.shero+=10;
		  break;
		case 27:
		  if (!py.flags.resist_poison)
		    {
		      msg_print("You feel safe from poison.");
		      py.flags.resist_poison+=50+spell*2;
		    }
		  else
		    py.flags.resist_poison+=20;
		  break;
		case 28:
                  py.flags.shield += randint(20)+30;
		  calc_bonuses();
		  prt_pac(); 
		  calc_mana(prime_stat[py.misc.realm]);
                  msg_print("A mystic shield forms around your body!");
		  break;
		case 29:
		  if (get_dir(NULL, &dir))
		    fire_ball(GF_SPIRIT,dir, char_row, char_col,
			      80+spell*3/2,"force of death");
		  break;
		case 30: /* Infuse Weapon */
		  if (inventory[INVEN_WIELD].tval!=TV_NOTHING &&
		      inventory[INVEN_WIELD].name2==SN_NULL)
		    {
		      msg_print("A deep purple light surrounds your weapon.");
		      i_ptr=&inventory[INVEN_WIELD];
		      i_ptr->name2=SN_SU;
		      i_ptr->tohit+=1+randint(spell/8);
		      i_ptr->todam+=1+randint(spell/8);
		      i_ptr->flags|=TR_SLAY_UNDEAD|TR_SEE_INVIS;
		      i_ptr->flags2|=TR_HOLD_LIFE|TR_LIGHT;
		    }
		  break;
		case 31:
		  destroy_area(char_row, char_col);
		  break;
		case 32: 
		  (void) banishment(UNDEAD|DEMON,(int)2*spell);
		  break;
		case 33: /* Annihilate Undead */
		  (void) dispel_creature(UNDEAD, 6*spell);
		  break;
		case 34:
		  if (get_dir(NULL,&dir))
		    fire_ball(GF_SPIRIT,dir, char_row, char_col,
			      220+spell*3/2,"force of death");
		  break;
		case 35:
		  if (get_dir(NULL,&dir))
		    fire_ball(GF_HOLY_ORB,dir,char_row,char_col,
			      150+spell,"benevolent spirit");
		  break;
		case 36:
		  switch(randint(6))
		    {
		    case 1:
		      if (res_stat(A_STR))
			msg_print("You feel less weak.");
		      break;
		    case 2:
		      if (res_stat(A_INT))
			msg_print("You feel less stupid.");
		      break;
		    case 3:
		      if (res_stat(A_WIS))
			msg_print("You feel less naive.");
		      break;
		    case 4:
		      if (res_stat(A_DEX))
			msg_print("You feel less clumsy.");
		      break;
		    case 5:
		      if (res_stat(A_CON))
			msg_print("You feel healthy again.");
		      break;
		    case 6:
		      if (res_stat(A_CHR))
			msg_print("You feel your looks returning.");
		      break;
		    }
		  break;
		case 37:
		  (void) restore_level();
		  break;
		case 38:
		  if (res_stat(A_STR))
		    msg_print("You feel less weak.");
		  if (res_stat(A_INT))
		    msg_print("You feel less stupid.");
		  if (res_stat(A_WIS))
		    msg_print("You feel less naive.");
		  if (res_stat(A_DEX))
		    msg_print("You feel less clumsy.");
		  if (res_stat(A_CON))
		    msg_print("You feel healthy again.");
		  if (res_stat(A_CHR))
		    msg_print("You feel your looks returning.");
		  break;
		case 39:
		  if (py.misc.exp < py.misc.max_exp)
		    {
		      msg_print("You feel your life force return.");
		      py.misc.exp=py.misc.max_exp;
		    }
		  prt_experience();
		  break;
		case 40:
		  i=py.misc.cmana+randint(35);
		  if (i>py.misc.mana) i=py.misc.mana;
		  py.misc.cmana=i;
		  msg_print("You feel your head clear a bit.");
		  break;
		case 41:
		  (void) dispel_creature(ANIMAL,(int)5*spell);
		  break;
		case 42:
		  if (get_dir(NULL, &dir))
		    (void) drain_life(dir, char_row,char_col,-(120+spell*3/2));
		  break;
		case 43:
		  (void) genocide();
		  break;
                case 44:
		  (void) mass_genocide();
                  break;
                case 45: /* Regeneration */
		  (void) hp_player(200+spell*2);
                  break;
                case 46: /* Fighting Rage */
		  if (!py.flags.shero)
		    {
		      py.flags.shero+=50+spell*2;
		      py.flags.fast+=50+spell*2;
		    }
		  else
		    {
		      py.flags.shero+=10+spell;
		      py.flags.fast+=10+spell;
		    }
                  break;
                case 47: /* Lich Form */
		  if (!py.flags.resist_heat)
		    {
		      msg_print("You feel somewhat like a Lich");
		      py.flags.tim_invis = 100+spell*2;
		      py.flags.resist_heat = 100+spell*2;
		      py.flags.resist_cold = 100+spell*2;
		      py.flags.resist_acid = 100+spell*2;
		      py.flags.resist_poison = 100+spell*2;
		      py.misc.timeout = 300+spell*4;
		      py.flags.fast =100+spell*2;
		    }
                  break;
		case 48:
		  if (!py.flags.invuln)
		    py.flags.invuln+=10+randint(spell/5);
		  else
		    py.flags.invuln+=5+randint(5);
                case 49:
		  (void) detection();
                  break;
                case 50:
		  (void) probing();
                  break;
                case 51:
		  (void) map_area();
                  break;
                case 52:
		  (void) wizard_light(TRUE);
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
