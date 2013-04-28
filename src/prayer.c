/* prayer.c: code for priest spells

   Copyright (c) 1989 James E. Wilson, Robert A. Koeneke

   This software may be copied and distributed for educational, research, and
   not for profit purposes provided that this copyright and statement are
   included in all such copies. */

#include "constant.h"
#include "monster.h"
#include "config.h"
#include "types.h"
#include "externs.h"


/* Pray like HELL.					-RAK-	*/
void pray()
{
  int i, j, item_val, dir, spell;
  int choice, chance, result, tval;
  register spell_type *s_ptr;
  register struct misc *m_ptr;
  register struct flags *f_ptr;
  register inven_type *i_ptr;

  free_turn_flag = TRUE;
  tval=inventory[INVEN_WIELD].tval;
  if (py.flags.blind > 0)
    msg_print("You can't see to read your prayer!");
  else if (no_light())
    msg_print("You have no light to read by.");
  else if (py.flags.confused > 0)
    msg_print("You are too confused.");
  else if (py.misc.realm == DRUID)
    druid();
  else if (py.misc.realm != PRIEST)
    msg_print("Pray hard enough and your prayers may be answered.");
  else if (py.flags.shero)
    msg_print("You are too berserk to pray!");
  else if (no_magic())
    msg_print("Your link to your god is broken for now.");
  else if (inven_ctr == 0)
    msg_print ("But you are not carrying anything!");
  else if (!find_range(TV_PRAYER_BOOK, TV_NEVER, &i, &j))
    msg_print ("You are not carrying any Holy Books!");
  else if (get_item(&item_val, "Use which Holy Book?", i, j, 0))
    {
      spell=smod(S_MAGIC)+(py.skills.cur_skill[magical[PRIEST]]/20)-5;
      result = cast_spell("Recite which prayer?", item_val, &choice, &chance,
			  TV_PRAYER_BOOK);
      if (result < 0)
	msg_print("You don't know any prayers in that book.");
      else if (result > 0)
	{
	  s_ptr = &magic_spell[py.misc.realm][choice];
	  free_turn_flag = FALSE;

	  if ((tval==TV_SWORD || tval==TV_POLEARM) &&
	      inventory[INVEN_WIELD].name2!=SN_HA &&
	      inventory[INVEN_WIELD].tval!=TV_NOTHING)
	    { /* Penalize priest while using non-blunt weapon */
	      chance+=20;
	      spell=spell*2/3;
	    }
	  if (py.flags.stun>50) chance+=25;
	  else if (py.flags.stun>0) chance+=15;
	  if (randint(100) < chance)
	    msg_print("Your god ignores your prayer!");
	  else
	    {
	      /* Prayers.					*/
	      switch(choice+1)
		{
		case 1:
		  (void) detect_general(0,EVIL,"evil");
		  break;
		case 2:
		  (void) hp_player(damroll(3+spell/8, 3+spell/5));
		  if (py.flags.cut>0) {
		    py.flags.cut-=10;
		    if (py.flags.cut<0) py.flags.cut=0;
		    msg_print("Your wounds heal.");
		  }
		  break;
		case 3:
		  bless(randint(12)+5+spell/4*2);
		  break;
		case 4:
		  (void) remove_fear();
		  break;
		case 5:
		  (void) light_area(char_row, char_col);
		  for(i=1;i<=9;i++)
		    if (i!=5)
		      light_line(i,char_row,char_col);
		  break;
		case 6:
		  (void) detect_trap();
		  (void) detect_sdoor();
		  break;
		case 7:
		  if (get_dir(NULL, &dir))
		    fire_bolt(GF_HOLY_ORB, dir, char_row, char_col,
			      damroll(3,5),"Spiritual Hammer");
		  break;
		case 8:
		  (void) cure_poison();
		  break;
		case 9:
		  if (get_dir(NULL, &dir))
		    (void) sleep_monster(dir, char_row, char_col);
		  break;
		case 10:
		  teleport((int)(spell*3));
		  break;
		case 11:
		  (void) hp_player(damroll(4+spell/5, 5+spell/8));
		  if (py.flags.flags[F_DISEASE]>0) {
		    py.flags.flags[F_DISEASE]=0;
		    msg_print("Your sickness heals.");
		  }
		  if (py.flags.cut>0) {
		    py.flags.cut=(py.flags.cut/2)-20;
		    if (py.flags.cut<0) py.flags.cut=0;
		    msg_print("Your wounds heal.");
		  }
		  break;
		case 12:
		  bless(randint(24)+20+spell);
		  break;
		case 13:
                  (void) banishment(ANIMAL,(int)2*spell);
		  break;
		case 14:
		  create_food();
		  break;
		case 15:
      	  /* this old code allows priests to uncurse the One Ring.  It looks
         wrong to me.  Why not just call remove_curse()??  In case this really
         was what was meant, I've left it here. -CFT 
		  for (i = 0; i < INVEN_ARRAY_SIZE; i++)
		    {
		      i_ptr = &inventory[i]; */
		      /* only clear flag for items that are wielded or worn */
		  /*    if (i_ptr->tval >= TV_MIN_WEAR
			  && i_ptr->tval <= TV_MAX_WEAR)
			if (!(i_ptr->name2 & SN_MORGUL) &&
			    !(i_ptr->name2 & SN_MORMEGIL) &&
			    !(i_ptr->name2 & SN_CALRIS))
			  i_ptr->flags &= ~TR_CURSED;
		    }
		  break; */
		  remove_curse(); /* -CFT */
		  break;
		case 16:
		  f_ptr = &py.flags;
		  f_ptr->resist_heat += randint(10) + 10;
		  f_ptr->resist_cold += randint(10) + 10;
		  break;
		case 17:
		  if (!py.flags.word_recall)
		  {
		   msg_print("The air around you becomes charged.");
		   py.flags.word_recall = 20 + randint(20);
		  }
		  break;
		case 18:
		  if (get_dir(NULL, &dir))
		    fire_ball(GF_HOLY_ORB, dir, char_row, char_col,
			      (int)(damroll(4,4)+spell*3/2),
			      "Black Sphere");
		  break;
		case 19:
		  (void) hp_player(damroll(5+spell/5, 5+spell/6));
		  if (py.flags.flags[F_DISEASE]>0) {
		    py.flags.flags[F_DISEASE]=0;
		    msg_print("Your sickness heals.");
		  }
		  if (py.flags.cut>0) {
		    py.flags.cut=0;
		    msg_print("Your wounds heal.");
		  }
		  break;
		case 20:
		  if (py.flags.tim_invis>0)
		    py.flags.tim_invis+=5+randint(5);
		  else
		    py.flags.tim_invis+=15+spell+randint(24);
		  detect_inv2(randint(24)+15+spell);
		  prt_speed(); /* Let player know he's invisible */
		  break;
		case 21:
		  (void) protect_evil();
		  break;
		case 22:
		  earthquake();
		  break;
		case 23:
		  map_area();
		  break;
		case 24:
		  (void) hp_player(damroll(10+spell/9, 4+spell/8));
		  if (py.flags.flags[F_DISEASE]>0) {
		    py.flags.flags[F_DISEASE]=0;
		    msg_print("Your sickness heals.");
		  }
		  if (py.flags.cut>0) {
		    py.flags.cut=0;
		    msg_print("Your wounds heal.");
		  }
		  break;
		case 25:
		  (void) turn_undead();
		  break;
		case 26:
		  (void) banishment(EVIL,spell);
		  break;
		case 27:
		  (void) dispel_creature(UNDEAD, (int)(4*spell));
		  break;
		case 28:
		  (void) hp_player(200+spell*3);
		  if (py.flags.flags[F_DISEASE]>0) {
		    py.flags.flags[F_DISEASE]=0;
		    msg_print("Your sickness heals.");
		  }
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
		case 29:
		  (void) dispel_creature(EVIL, (int)(4*spell));
		  break;
		case 30:
		  warding_glyph();
		  break;
		case 31:
		  (void) dispel_creature(EVIL, (int)(5*spell));
		  (void) remove_fear();
		  (void) cure_poison();
		  (void) hp_player(2000);
		  if (py.flags.flags[F_DISEASE]>0) {
		    py.flags.flags[F_DISEASE]=0;
		    msg_print("Your sickness heals.");
		  }
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
		  i=py.misc.cmana+randint(40);
		  if (i>py.misc.mana) i=py.misc.mana;
		  py.misc.cmana=i;
		  msg_print("You feel your head clear a bit.");
		  break;
		case 32:
		  (void) detection();
		  break;
		case 33:
		  (void) ident_spell();
		  break;
		case 34:/* probing */
		  (void) probing();
		  break;
		case 35: /* Clairvoyance */
		  wizard_light(TRUE);
		  break;
		case 36: /* Self-Knowledge */
		  self_knowledge();
		  break;
		case 37:
		  py.flags.resist_heat+=50+spell+randint(10+spell);
		  py.flags.resist_cold+=50+spell+randint(10+spell);
		  py.flags.resist_light+=50+spell+randint(10+spell);
		  py.flags.resist_poison+=50+spell+randint(10+spell);
		  py.flags.resist_acid+=50+spell+randint(10+spell);
		  break;
		case 38:
                  py.flags.shield+=50+randint(spell)+spell/3;
                  calc_bonuses();
		  prt_pac();
                  calc_mana(A_WIS);
		  msg_print("The essence of your god surrounds you!");
		  break;
		case 39: /* restoration */
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
		case 40: /* rememberance */
		  (void) restore_level();
		  break;
		case 41: /* Holy Barrier */
		  py.flags.invuln+=randint(10)+2+spell/5;
		  break;
		case 42: /* Stunning Bolt */
		  if (get_dir(NULL,&dir))
		    fire_bolt(GF_SOUND,dir,char_row,char_col,
			      -(40+spell),"wave of sound");
		  break;
		case 43: /* dispel evil */
		  (void) dispel_creature(EVIL, (int)(5*spell));
		  break;
		case 44: /* Annihilate Evil */
		  notarget=1;
		  if (get_dir(NULL,&dir))
		    fire_bolt(GF_HOLY_ORB,dir,char_row,char_col,
			      -(150+spell*3/2),"divine force");
		  break;
		case 45: /* word of destruction */
		  destroy_area(char_row, char_col);
		  break;
		case 46: /* annihilation */
		  if (get_dir(NULL, &dir))
		    drain_life(dir, char_row, char_col, 300);
		  break;
		case 47: /* recharging */
		  (void) recharge(50+spell);
		  break;
		case 48: /* dispel curse */
		  (void) remove_all_curse();
		  break;
		case 49: /* Battle Blessing */
		  bless(20+spell);
		  if (!py.flags.fast)
		    py.flags.fast+=20+spell;
		  else
		    py.flags.fast+=randint(spell/2)+5;
		  break;
		case 50: /* enchant weapon */
		  i_ptr = &inventory[INVEN_WIELD];
		  if (i_ptr->tval != TV_NOTHING)
		    {
		      int flag, k;
		      char tmp_str[100], out_val[100];

		      objdes(tmp_str, i_ptr, FALSE);
		      sprintf(out_val, "Your %s glows brightly!", tmp_str);
		      msg_print(out_val);
		      flag = FALSE;
		      for (k = 0; k < randint(4); k++)
			if (enchant(&i_ptr->tohit))
			  flag = TRUE;
		      for (k = 0; k < randint(4); k++)
			if (enchant(&i_ptr->todam))
			  flag = TRUE;
		      if (flag)
			{
			  i_ptr->flags &= ~TR_CURSED;
			  calc_bonuses ();
			}
		      else
			msg_print("The enchantment fails.");
		    }
		  break;
		case 51: /* enchant armor */
		  if (1) {
		    int k=0;
		    int l=0;
		    int tmp[100];

		    if (inventory[INVEN_BODY].tval != TV_NOTHING)
		      tmp[k++] = INVEN_BODY;
		    if (inventory[INVEN_ARM].tval != TV_NOTHING)
		      tmp[k++] = INVEN_ARM;
		    if (inventory[INVEN_OUTER].tval != TV_NOTHING)
		      tmp[k++] = INVEN_OUTER;
		    if (inventory[INVEN_HANDS].tval != TV_NOTHING)
		      tmp[k++] = INVEN_HANDS;
		    if (inventory[INVEN_HEAD].tval != TV_NOTHING)
		      tmp[k++] = INVEN_HEAD;
		    /* also enchant boots */
		    if (inventory[INVEN_FEET].tval != TV_NOTHING)
		      tmp[k++] = INVEN_FEET;

		    if (k > 0)  l = tmp[randint(k)-1];
		    if (TR_CURSED & inventory[INVEN_BODY].flags)
		      l = INVEN_BODY;
		    else if (TR_CURSED & inventory[INVEN_ARM].flags)
		      l = INVEN_ARM;
		    else if (TR_CURSED & inventory[INVEN_OUTER].flags)
		      l = INVEN_OUTER;
		    else if (TR_CURSED & inventory[INVEN_HEAD].flags)
		      l = INVEN_HEAD;
		    else if (TR_CURSED & inventory[INVEN_HANDS].flags)
		      l = INVEN_HANDS;
		    else if (TR_CURSED & inventory[INVEN_FEET].flags)
		      l = INVEN_FEET;

		    if (l > 0)
		      {
			char out_val[100], tmp_str[100];

			i_ptr = &inventory[l];
			objdes(tmp_str, i_ptr, FALSE);
			sprintf(out_val, "Your %s glows faintly!", tmp_str);
			msg_print(out_val);
			if (enchant(&i_ptr->toac))
			  {
			    i_ptr->flags &= ~TR_CURSED;
			    calc_bonuses ();
			  }
			else
			  msg_print("The enchantment fails.");
		      }
		  }
		  break;
		case 52: /* Elemental brand */
		    i_ptr = &inventory[INVEN_WIELD];
		  if (i_ptr->tval != TV_NOTHING &&
		      i_ptr->name2 == SN_NULL)
		    {
		      int hot = randint(20);
		      char tmp_str[100], out_val[100];

		      objdes(tmp_str, i_ptr, FALSE);
		      if (hot<10) {
			sprintf(out_val,
				"Your %s is covered in a fiery shield!",
				tmp_str);
			i_ptr->name2 |= SN_FT;
			i_ptr->flags2 |= TR_IM_FIRE;
			if (i_ptr->tval!=TV_SHIELD)
			  i_ptr->flags |= TR_FLAME_TONGUE;
		      } else if (hot<18) {
			sprintf(out_val,"Your %s glows deep, icy blue!",
				tmp_str);
			i_ptr->name2 |= SN_FROST;
			i_ptr->flags2 |= TR_IM_COLD;
			if (i_ptr->tval != TV_SHIELD)
			  i_ptr->flags |= TR_FROST_BRAND;
		      }
		       else /* Lightning Brand */
			 {
			   sprintf
			     (out_val,"Fierce lightning bonds with your %s!",
				   tmp_str);
			   i_ptr->name2 |= SN_LIGHTNING;
			   i_ptr->flags2 |= TR_IM_LIGHT;
			   if (i_ptr->tval!=TV_SHIELD)
			     i_ptr->flags2 |= TR_LIGHTNING;
			 }
		      msg_print(out_val);
		      i_ptr->cost += 5000;
		      i_ptr->tohit+=4+randint(4);
		      i_ptr->todam+=4+randint(4);
		      i_ptr->flags &= ~TR_CURSED;
		      calc_bonuses ();
		    } else {
		      msg_print("The Branding fails.");
		    }
		  break;
		case 53: /* blink */
		  teleport(10);
		  break;
		case 54: /* teleport */
		  teleport((int)(spell*8));
		  break;
		case 55: /* teleport away */
		  if (get_dir(NULL, &dir))
		    (void) teleport_monster(dir, char_row, char_col);
		  break;
		case 56: /* teleport level */
		  (void) tele_level();
		  break;
		case 57: /* Resist Death */
		  if (py.misc.timeout)
		    { msg_print("You feel your will to live strengthen.");
		    py.misc.timeout=300+spell*2;
		    }
		  else
		    {
		      py.misc.timeout+=50+spell/4;
		    }
		  break;
		case 58: /* alter reality */
		  new_level_flag = TRUE;
		  break;
		default:
		  break;
		}
	      /* End of prayers.				*/
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
