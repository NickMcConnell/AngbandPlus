/* scrolls.c: scroll code

   Copyright (c) 1989 James E. Wilson, Robert A. Koeneke

   This software may be copied and distributed for educational, research, and
   not for profit purposes provided that this copyright and statement are
   included in all such copies. */

#include "constant.h"
#include "monster.h"
#include "config.h"
#include "types.h"
#include "externs.h"

#ifdef USG
#include <string.h>
#else
#include <strings.h>
#endif

/* Scrolls for the reading				-RAK-	*/
void read_scroll()
{
  int32u i;
  int j, k, item_val, y, x, sm_type;
  int tmp[6], flag, used_up;
  bigvtype out_val, tmp_str;
  register int ident, l;
  register inven_type *i_ptr;
  register struct misc *m_ptr;
  char destination[80];
  vtype artif_ptr;
  

  free_turn_flag = TRUE;
  if (py.flags.blind > 0)
    msg_print("You can't see to read the scroll.");
  else if (no_light())
    msg_print("You have no light to read by.");
  else if (py.flags.confused > 0)
    msg_print("You are too confused to read a scroll.");
  else if (inven_ctr == 0)
    msg_print("You are not carrying anything!");
  else if (!find_range(TV_SCROLL1, TV_SCROLL2, &j, &k))
    msg_print ("You are not carrying any scrolls!");
  else if (get_item(&item_val, "Read which scroll?", j, k, 0))
    {
      i_ptr = &inventory[item_val];
      free_turn_flag = FALSE;
      used_up = TRUE;
      i = i_ptr->flags;
      ident = FALSE;

      while (i != 0)
	{
	  j = bit_pos(&i) + 1;
	  if (i_ptr->tval == TV_SCROLL2)
	    j += 32;

	  /* Scrolls.			*/
	  switch(j)
	    {
	    case 1:
	      i_ptr = &inventory[INVEN_WIELD];
	      if (i_ptr->tval != TV_NOTHING)
		{
		  objdes(tmp_str, i_ptr, FALSE);
		  (void) sprintf(out_val, "Your %s glows faintly!", tmp_str);
		  msg_print(out_val);
		  if (!enchant(i_ptr, 1, ENCH_TOHIT))
		    msg_print("The enchantment fails.");
		  ident = TRUE;
		}
	      break;
	    case 2:
	      i_ptr = &inventory[INVEN_WIELD];
	      if (i_ptr->tval != TV_NOTHING)
		{
		  objdes(tmp_str, i_ptr, FALSE);
		  (void) sprintf(out_val, "Your %s glows faintly!", tmp_str);
		  msg_print(out_val);
		  if (!enchant(i_ptr, 1, ENCH_TODAM))
		    msg_print("The enchantment fails.");
		  ident = TRUE;
		}
	      break;
	    case 3:
	      k = 0;
	      l = 0;
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
		  i_ptr = &inventory[l];
		  objdes(tmp_str, i_ptr, FALSE);
		  (void) sprintf(out_val, "Your %s glows faintly!", tmp_str);
		  msg_print(out_val);
		  if (!enchant(i_ptr, 1, ENCH_TOAC))
		    msg_print("The enchantment fails.");
		  ident = TRUE;
		}
	      break;
	    case 4:
	      msg_print("This is an identify scroll.");
	      ident = TRUE;
	      used_up = ident_spell();

	      /* The identify may merge objects, causing the identify scroll
		 to move to a different place.	Check for that here.  It can
		 move arbitrarily far if an identify scroll was used on
		 another identify scroll, but it always moves down. */
	      while (i_ptr->tval != TV_SCROLL1 || i_ptr->flags != 0x00000008)
		{
		  item_val--;
		  i_ptr = &inventory[item_val];
		}
	      break;
	    case 5:
	      if (remove_curse())
		{
		  msg_print("You feel as if someone is watching over you.");
		  ident = TRUE;
		}
	      break;
	    case 6:
	      ident = light_area(char_row, char_col);
          break;
   case 7:
        ident = TRUE;
        sm_type = randint(100);
        for (k = 0; k < randint(randint(12)==1?8:3); k++)
        { 
	      y = char_row;
	      x = char_col;
          switch(sm_type) {
         case 1: ident |= summon_ancientd(&y, &x); break;
         case 2: ident |= summon_gundead(&y, &x); break;
         case 3: ident |= summon_wraith(&y, &x); break;
         case 4: ident |= summon_jabberwock(&y, &x); break;
         case 5: ident |= summon_ant(&y, &x); break;
         case 6: ident |= summon_unique(&y, &x);break;
         case 7: ident |= summon_angel(&y, &x); break;
         case 8: ident |= summon_undead(&y, &x); break;
         case 9: ident |= summon_dragon(&y, &x); break;
         case 10:ident |=  summon_spider(&y, &x); break;
         case 11:ident |=  summon_hound(&y, &x); break;
         case 12:ident |=  summon_reptile(&y, &x); break;
         default:
          ident |= summon_monster(&y, &x, FALSE);}
	    }
        break;
	    case 8:
	      teleport(10);
	      ident = TRUE;
	      break;
	    case 9:
	      teleport(100);
	      ident = TRUE;
	      break;
	    case 10:
	      (void) tele_level();
	      ident = TRUE;
	      break;
	    case 11:
	      if (py.flags.confuse_monster == 0)
		{
		  msg_print("Your hands begin to glow.");
		  py.flags.confuse_monster = TRUE;
		  ident = TRUE;
		}
	      break;
	    case 12:
	      ident = TRUE;
	      map_area();
	      break;
	    case 13:
	      ident = sleep_monsters1(char_row, char_col);
	      break;
	    case 14:
	      ident = TRUE;
	      warding_glyph();
	      break;
	    case 15:
	      ident = detect_treasure();
	      break;
	    case 16:
	      ident = detect_object();
	      break;
	    case 17:
	      ident = detect_trap();
	      break;
	    case 18:
	      ident = detect_sdoor();
	      break;
	    case 19:
	      msg_print("This is a mass genocide scroll.");
	      mass_genocide(TRUE);
	      ident = TRUE;
	      break;
        case 20:
          msg_print("There is a message on the scroll. It says:");
          if (randint(40)==1) {
                  if (randint(7)==1)
                  getLine("chainswd.txt",0,destination);
                  else  if (randint(2)==1)
                  getLine("error.txt",0,destination);
                        else
                  getLine("death.txt",0,destination);}
            else       {   getLine("rumors.txt",0,destination);}
                    msg_print(destination);
          msg_print("The scroll vanishes in a puff of smoke!");
          ident = TRUE;
	      break;
	    case 21:
	      ident = aggravate_monster(20);
	      if (ident)
		msg_print("There is a high pitched humming noise.");
	      break;
	    case 22:
	      ident = trap_creation();
	      break;
	    case 23:
	      ident = td_destroy();
	      break;
	    case 24:  /* Not Used , used to be door creation */
	      break;
	    case 25:
	      msg_print("This is a Recharge-Item scroll.");
	      ident = TRUE;
	      used_up = recharge(60);
	      break;
	    case 26:
	      msg_print("This is a genocide scroll.");
	      genocide(TRUE);
	      ident = TRUE;
	      break;
	    case 27:
	      ident = unlight_area(char_row, char_col);
	      break;
	    case 28:
	      ident = protect_evil();
	      break;
	    case 29:
	      ident = TRUE;
	      create_food();
	      break;
	    case 30:
	      ident = dispel_creature(UNDEAD, 60);
	      break;
	    case 31:
	      remove_all_curse();
	      ident = TRUE;
	      break;
	    case 33:
	      i_ptr = &inventory[INVEN_WIELD];
	      if (i_ptr->tval != TV_NOTHING)
		{
		  objdes(tmp_str, i_ptr, FALSE);
		  (void) sprintf(out_val, "Your %s glows brightly!", tmp_str);
		  msg_print(out_val);
		  if (!enchant(i_ptr, randint(3), ENCH_TOHIT|ENCH_TODAM))
		    msg_print("The enchantment fails.");
		  ident = TRUE;
		}
	      break;
	    case 34:
	      i_ptr = &inventory[INVEN_WIELD];
	      if (i_ptr->tval != TV_NOTHING)
		{
		  objdes(tmp_str, i_ptr, TRUE);
		  tmp_str[strlen(tmp_str)-1] = 0; /* remove final '.' -CFT */
		  if ((i_ptr->flags2 & TR_ARTIFACT) && (randint(7) < 4)){
		    (void)sprintf(out_val,
			"A terrible black aura tries to surround your weapon,"
			" but your %s resists the effects!", tmp_str);
		    msg_print(out_val);
		  } else { /* not artifact or failed save... */
		    (void)sprintf(out_val,
			"A terrible black aura blasts your %s!", tmp_str);
		    msg_print(out_val);
		    py_bonuses(i_ptr, -1); /* take off current bonuses -CFT */
		    i_ptr->name2 = SN_SHATTERED;
		    i_ptr->tohit = -randint(5) - randint(5);
		    i_ptr->todam = -randint(5) - randint(5);
		    i_ptr->flags = TR_CURSED; 
		    i_ptr->flags2 = 0;
		    i_ptr->damage[0] = i_ptr->damage[1] = 1;
		    i_ptr->toac = 0; /* in case defender... */
		    i_ptr->cost = -1;
		    py_bonuses(i_ptr, 1); /* now apply new "bonuses" -CFT */
		    calc_bonuses ();
		  }
		  ident = TRUE; /* even if artifact makes save... */
		}
	      break;
	    case 35:
	      k = 0;
	      l = 0;
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
		  i_ptr = &inventory[l];
		  objdes(tmp_str, i_ptr, FALSE);
		  (void) sprintf(out_val,"Your %s glows brightly!", tmp_str);
		  msg_print(out_val);
		  if (!enchant(i_ptr, randint(3)+1, ENCH_TOAC))
		    msg_print("The enchantment fails.");
		  ident = TRUE;
		}
	      break;
	    case 36:
	      if ((inventory[INVEN_BODY].tval != TV_NOTHING)
		  && (randint(4) == 1))
		k = INVEN_BODY;
	      else if ((inventory[INVEN_ARM].tval != TV_NOTHING)
		       && (randint(3) ==1))
		k = INVEN_ARM;
	      else if ((inventory[INVEN_OUTER].tval != TV_NOTHING)
		       && (randint(3) ==1))
		k = INVEN_OUTER;
	      else if ((inventory[INVEN_HEAD].tval != TV_NOTHING)
		       && (randint(3) ==1))
		k = INVEN_HEAD;
	      else if ((inventory[INVEN_HANDS].tval != TV_NOTHING)
		       && (randint(3) ==1))
		k = INVEN_HANDS;
	      else if ((inventory[INVEN_FEET].tval != TV_NOTHING)
		       && (randint(3) ==1))
		k = INVEN_FEET;
	      else if (inventory[INVEN_BODY].tval != TV_NOTHING)
		k = INVEN_BODY;
	      else if (inventory[INVEN_ARM].tval != TV_NOTHING)
		k = INVEN_ARM;
	      else if (inventory[INVEN_OUTER].tval != TV_NOTHING)
		k = INVEN_OUTER;
	      else if (inventory[INVEN_HEAD].tval != TV_NOTHING)
		k = INVEN_HEAD;
	      else if (inventory[INVEN_HANDS].tval != TV_NOTHING)
		k = INVEN_HANDS;
	      else if (inventory[INVEN_FEET].tval != TV_NOTHING)
		k = INVEN_FEET;
	      else
		k = 0;

	      if (k > 0)
		{
		  i_ptr = &inventory[k];
		  objdes(tmp_str, i_ptr, TRUE);
		  tmp_str[strlen(tmp_str)-1] = 0; /* kill final '.' -CFT */
		  if ((i_ptr->flags2 & TR_ARTIFACT) && (randint(7) < 4)){
		    (void)sprintf(out_val,
			"A terrible black aura tries to surround your "
			"%s, but it resists the effects!", tmp_str);
		    msg_print(out_val);
		  } else { /* not artifact or failed save... */
		    (void)sprintf(out_val,
			"A terrible black aura blasts your %s!", tmp_str);
		    msg_print(out_val);
		    py_bonuses(i_ptr, -1); /* take off current bonuses -CFT */
		    i_ptr->name2 = SN_BLASTED;
		    i_ptr->flags = TR_CURSED;
		    i_ptr->flags2 = 0;
		    i_ptr->toac = -randint(5) - randint(5);
		    i_ptr->tohit = i_ptr->todam = 0; /* in case gaunlets of
		    					slaying... */
		    i_ptr->ac = (i_ptr->ac > 9) ? 1 : 0;
		    i_ptr->cost = -1;
		    py_bonuses(i_ptr, 1); /* now apply new "bonuses" -CFT */
		    calc_bonuses ();
		    }
		  ident = TRUE; /* even if artifact makes save... */
		}
	      break;
        case 37:
	      ident = FALSE;
          sm_type = randint(7);
          for (k = 0; k < randint(randint(12)==1?8:3); k++)
		{
		  y = char_row;
		  x = char_col;
          switch(sm_type){
            case 1: ident |= summon_gundead(&y, &x); break;
            default: ident |= summon_undead(&y, &x);}
		}
	      break;
	    case 38:
	      ident = TRUE;
	      bless(randint(12)+6);
	      break;
	    case 39:
	      ident = TRUE;
          bless(randint(36)+18);
	      break;
        case 40:
        i_ptr  = &inventory[INVEN_WIELD];
        if (i_ptr->tval != TV_NOTHING &&
            i_ptr->tval != TV_SLING_AMMO &&     /* I don't want anybody getting */
            i_ptr->tval != TV_BOLT &&           /* rich by selling artifact */
            i_ptr->tval != TV_ARROW &&           /* arrows!!! - TY*/
              i_ptr->name2 == SN_NULL)
              { 
                int k, l, i, j, has_magplus, magcon;
                char tmp_str[100], out_val[100];
                objdes(tmp_str, i_ptr, FALSE);
                py_bonuses(i_ptr, -1);
                sprintf(out_val, "Your %s radiates a blinding glow!", tmp_str);
                msg_print(out_val);
                has_magplus = 0;
                magcon = 5;
                k = randint(100);
                i = randint(k<=1?20:7);
                for (j=0;j<=i;j++)
                switch (randint(58))
                    {case 1: i_ptr->flags |= TR_SLOW_DIGEST;
                                    msg_print("You feel less hungry.");
                                    i_ptr->cost += 400L;
                                     break;
                     case 2: i_ptr->flags |= TR_REGEN;
                                     msg_print("You feel less vulnerable.");
                                     i_ptr->cost += 2000L;
                                     break;
                     case 3: i_ptr->flags |= TR_SLAY_DRAGON;
                                    msg_print("You feel aggravated at dragons.");
                                    i_ptr->cost += 1500L;
                                    break;
                     case 4: i_ptr->flags |= TR_SLAY_ANIMAL; 
                                    msg_print("You hate animals!");
                                    i_ptr->cost += 1000L;
                                    break;
                     case 5: i_ptr->flags |= TR_SLAY_EVIL;
                                    msg_print("You feel like a great champion of good.");
                                    i_ptr->cost += 2000L;
                                    break;
                     case 6: i_ptr->flags |= TR_SLAY_UNDEAD;
                                    msg_print("You hate undead abominities!");
                                    i_ptr->cost += 1800L;
                                    break;
                     case 7: i_ptr->flags |= TR_FROST_BRAND;
                                    msg_print("Your weapon feels cool.");
                                    i_ptr->cost += 2000L;
                                    break;
                     case 8: i_ptr->flags |= TR_FLAME_TONGUE;
                                    msg_print("Your weapon feels hot!");
                                    i_ptr->cost += 2000L;
                                    break;
                     case 9: i_ptr->flags |= TR_RES_FIRE;
                                    msg_print("You feel cool.");
                                    i_ptr->cost += 400L;
                                    break;
                     case 10: i_ptr->flags |= TR_RES_ACID;
                                    msg_print("Your stomach rumbles.");
                                    i_ptr->cost += 400L;
                                    break;
                     case 11: i_ptr->flags |= TR_RES_COLD;
                                    msg_print("You feel full of hot air!");
                                    i_ptr->cost += 400L;
                                    break;
                     case 12: i_ptr->flags |= TR_FREE_ACT;
                                    msg_print("You feel like a young rebel!");
                                    i_ptr->cost += 3500L;
                                    break;
                     case 13: i_ptr->flags |= TR_SEE_INVIS;
                                    msg_print("You can see the air!");
                                    i_ptr->cost += 3000L;
                                    break;
                     case 14: i_ptr->flags |= TR_RES_LIGHT;
                                    msg_print("You feel conductive.");
                                    i_ptr->cost += 400L;
                                    break;
                     case 15: i_ptr->flags |= TR_FFALL;
                                    msg_print("For a second, you thought you could levitate!");
                                    i_ptr->cost += 400L;
                                    break;
                     case 16: i_ptr->flags |= TR_SLAY_X_DRAGON;
                                    msg_print("You can hardly wait to kill your next dragon!");
                                    i_ptr->cost += 4500L;
                                    break;
                     case 17: i_ptr->flags |= TR_POISON;
                                    msg_print("You feel like eating strange mushrooms.");
                                    i_ptr->cost += 3500L;
                                    break;
                     case 18: i_ptr->flags2 |= TR_SLAY_DEMON;
                                    msg_print("You hate demons!");
                                    i_ptr->cost += 1666L;
                                    break;
                     case 19: i_ptr->flags2 |= TR_SLAY_TROLL;
                                    msg_print("You hate trolls!");
                                    i_ptr->cost += 1000L;
                                    break;
                     case 20: i_ptr->flags2 |= TR_SLAY_GIANT;
                                    msg_print("You hate giants!");
                                    i_ptr->cost += 1300L;
                                    break;
                     case 21: i_ptr->flags2 |= TR_HOLD_LIFE;
                                    msg_print("You feel you can live forever!");
                                    i_ptr->cost += 4000L;
                                    break;
                     case 22: i_ptr->flags2 |= TR_SLAY_ORC;
                                    msg_print("You hate orcs!");
                                    i_ptr->cost += 1200L;
                                    break;
                     case 23: i_ptr->flags2 |= TR_TELEPATHY;
                                    msg_print("You hear somebody else's voice inside your head!");
                                    i_ptr->cost += 4500L;
                                    break;
                     case 24: i_ptr->flags2 |= TR_IM_FIRE;
                                    msg_print("You feel you could bathe in the Sun!");
                                    i_ptr->cost += 4000L;
                                    break;
                     case 25: i_ptr->flags2 |= TR_IM_COLD;
                                    msg_print("You feel you could live in Siberia!");
                                    i_ptr->cost += 4000L;
                                    break;
                     case 26: i_ptr->flags2 |= TR_IM_ACID;
                                    msg_print("You feel like eating a Green jelly!");
                                    i_ptr->cost += 3900L;
                                    break;
                     case 27: i_ptr->flags2 |= TR_IM_LIGHT;
                                    msg_print("You feel you could ride a lightning bolt!");
                                    i_ptr->cost += 4000L;
                                    break;
                     case 28: i_ptr->flags2 |= TR_LIGHT;
                                    msg_print("Your weapon starts shining!");
                                    i_ptr->cost += 400L;
                                    break;
                     case 29: i_ptr->flags2 |= TR_LIGHTNING;
                                    msg_print("Sparks fly from your weapon!");
                                    i_ptr->cost += 4500L;
                                    break;
                     case 30: i_ptr->flags2 |= TR_IM_POISON;
                                    msg_print("Cyanide must be rather tasty!");
                                    i_ptr->cost += 5000L;
                                    break;
                     case 31: i_ptr->flags2 |= TR_RES_CONF;
                                    msg_print("You can think clearly.");
                                    i_ptr->cost += 780L;
                                    break;
                     case 32: i_ptr->flags2 |= TR_RES_SOUND;
                                     msg_print("You can hardly hear a thing!");
                                     i_ptr->cost += 800L;
                                    break;
                     case 33: i_ptr->flags2 |= TR_RES_LT;
                                    msg_print("It seems dark here.");
                                    i_ptr->cost += 350L;
                                    break;
                     case 34: i_ptr->flags2 |= TR_RES_DARK;
                                    msg_print("Light gets in your eyes!");
                                    i_ptr->cost += 350L;
                                    break;
                     case 35: i_ptr->flags2 |= TR_RES_CHAOS;
                                    msg_print("You feel very firm.");
                                    i_ptr->cost += 1000L;
                                    break;
                     case 36: i_ptr->flags2 |= TR_RES_DISENCHANT;
                                    msg_print("Your weapon looks indestructible!");
                                    i_ptr->cost += 4000L;
                                    break;
                     case 37: i_ptr->flags2 |= TR_RES_SHARDS;
                                    msg_print("Your skin looks thicker!");
                                    i_ptr->cost += 850L;
                                    break;
                     case 38: i_ptr->flags2 |= TR_RES_NEXUS;
                                    msg_print("You feel normal.");
                                    i_ptr->cost += 700L;
                                    break;
                     case 39: i_ptr->flags2 |= TR_RES_BLIND;
                                    msg_print("Suddenly, you can see much better!");
                                    i_ptr->cost += 1000L;
                                    break;
                     case 40: i_ptr->flags |= TR_STR; has_magplus = 1;
                                     msg_print("You feel strong!");
                                     i_ptr->cost += 750L;
                                    break;
                     case 41: i_ptr->flags |= TR_INT; has_magplus = 1;
                                    msg_print("You feel smart!");
                                    i_ptr->cost += 750L;
                                    break;
                     case 42: i_ptr->flags |= TR_WIS; has_magplus = 1;
                                   msg_print("You feel wise!");
                                   i_ptr->cost += 750L;
                                   break;
                     case 43: i_ptr->flags |= TR_DEX; has_magplus = 1;
                                    msg_print("You feel agile!");
                                    i_ptr->cost += 750L;
                                     break;
                     case 44: i_ptr->flags |= TR_CON; has_magplus = 1;
                                    msg_print("You feel healthy!");
                                    i_ptr->cost += 750L;
                                    break;
                     case 45: i_ptr->flags |= TR_CHR; has_magplus = 1;
                                    msg_print("Your looks get better!");
                                    i_ptr->cost += 550L;
                                    break;
                     case 46: i_ptr->flags |= TR_SEARCH; has_magplus = 1;
                                    msg_print("Suddenly, you spot a worm hiding behind a stone!");
                                    i_ptr->cost += 550L;
                                     break;
                     case 47: i_ptr->flags |= TR_STEALTH;
                                    msg_print("You feel you are wearing silk boots.");
                                    i_ptr->cost += 950L;
                                    has_magplus = 1; break;
                     case 48: i_ptr->flags |= TR_TELEPORT;
                                    msg_print("You feel very jumpy.");
                                    i_ptr->cost -= 750L;
                                    break;
                     case 49: i_ptr->flags |= TR_SUST_STAT; has_magplus = 1;
                                    msg_print("You feel you can't get worse.");
                                    i_ptr->cost += 1900L;
                                    break;
                     case 50: i_ptr->flags |= TR_TUNNEL; has_magplus = 1;
                                    msg_print("You feel like a miner!");
                                    i_ptr->cost += 800L;
                                    break;
                     case 51: i_ptr->flags |= TR_INFRA; has_magplus = 1;
                                    msg_print("You can see small red animals around you!");
                                    i_ptr->cost += 500L;
                                    break;
                     case 52: i_ptr->toac = randint
                                     (randint(7)==1?21:7); break;
                     case 53: i_ptr->damage[0] += 1+randint(3); break;
                     case 54: i_ptr->damage[1] += 1+randint(5); break;
                     case 55: i_ptr->flags2 |= TR_RES_FEAR;
                                    msg_print("You feel suicidal.");
                                    i_ptr->cost += 900L;
                                    break;
                     case 56: i_ptr->flags2 |= TR_ATTACK_SPD;
                                    msg_print("Your weapon starts moving faster!");
                                    magcon = 3;
                                    has_magplus = 1;
                                    i_ptr->cost += 4000L;
                                    break;
                    case 57: i_ptr->flags |= TR_SPEED;
                                    msg_print("You feel your feet moving faster!");
                                    has_magplus = 1;
                                    i_ptr->cost += 10000L;
                                    break;
                     default: i_ptr->flags2 |= TR_RES_NETHER;
                                    msg_print("You feel like visiting a graveyard!");
                                    i_ptr->cost += 900L;
                                    break;
                     }

                i_ptr->tohit = randint(20);
                i_ptr->todam = randint(20);
                i_ptr->flags &= ~TR_CURSED;
                i_ptr->flags2 |= TR_ARTIFACT;
                i_ptr->cost *= 2;
                i_ptr->cost += 10000L;  /* Base cost */
                if(has_magplus==1) { i_ptr->p1 = randint(magcon);
                        i_ptr->ident |= ID_SHOW_P1;
                        i_ptr->cost += i_ptr->p1 * 250;}
               py_bonuses(i_ptr, +1);
               prt("What do you want to call the weapon?",0,0);
                if (get_string(artif_ptr, 0, 37, 16))
                {  
                    strncpy(i_ptr->artname, artif_ptr, 16);
                    i_ptr->artname[16]='\0';
    /* Now THIS should put a stop to those stack overflows! Thanks, CWS!
     - TY*/
               i_ptr->name2 |= SN_CALLED;
                }
                if (strlen(i_ptr->artname)==0) i_ptr->name2 = SN_NO_NAME;
                 /*To make using more than one scroll per weapon impossible!
                                   -TY */

                       }
                else {
                    msg_print("You feel a powerful weapon in your hands, but it vanishes!");
                }
          ident = TRUE;
          break;
	    case 41:
	      ident = TRUE;
	      { char c; int f = TRUE;
	      do { /* loop, so RET or other key doesn't accidently exit */
	      	f = get_com("Do you really want to return?", &c);
	        } while (f && (c != 'y') && (c != 'Y') && (c != 'n') &&
	        		(c != 'N'));
	      if (f && (c != 'n') && (c != 'N')) {
	        if (py.flags.word_recall == 0)
		  py.flags.word_recall = 25 + randint(30);
	        msg_print("The air about you becomes charged.");
	        }
	        }
	      break;
	    case 42:
	      destroy_area(char_row, char_col);
	      ident = TRUE;
	      break;
	    case 43:
	      place_special(char_row, char_col, SPECIAL);
	      ident = TRUE;
	      prt_map();
	      break;
	    case 44:
	      special_random_object(char_row, char_col, 1);
	      ident = TRUE;
	      prt_map();
	      break;
	    default:
	      msg_print("Internal error in scroll()");
	      break;
	    }
	  /* End of Scrolls.			       */
	}
      i_ptr = &inventory[item_val];
      if (ident)
	{
	  if (!known1_p(i_ptr))
	    {
	      m_ptr = &py.misc;
	      /* round half-way case up */
	      m_ptr->exp += (i_ptr->level +(m_ptr->lev >> 1)) / m_ptr->lev;
	      prt_experience();

	      identify(&item_val);
	      i_ptr = &inventory[item_val];
	    }
	}
      else if (!known1_p(i_ptr))
	sample (i_ptr);
      if (used_up)
	{
	  desc_remain(item_val);
	  inven_destroy(item_val);
	}
    }
}
