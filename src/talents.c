/* talents.c:	Talents based on high levels of each skill */
#include "angband.h"

#define MAXP 70	/* Max % chance for adding attribute */
#define LOW	 5	/* % taken off per point of level BELOW attribute */
#define HIGH	4	/* % decrease per point of level ABOVE attribute */

/* These correspond to the skills listed.	Empty string = No talent for
   this skill */
cptr tnames[S_NUM]=
{"","","","","","Fenneling","","","Find Traps","","","","",
 "","Meditation","Weapon Forging","Armor Forging","Predict Weather",
 "Sense Evil","Sense Animals","Restore Experience","","","Bowmaking",
 "Alchemy","Infusion","","","",""};

/* These say the lowest value of skill you can have to use this talent */
byte min[S_NUM] =
{
  0,0,0,0,0,50,0,0,200,0,0,210,0,
  0,150,50,50,50,200,150,220,0,0,80,100,120,0,0,0
};

#define sqr(x)	((x)*(x))
int isqrt(int x)
{
  int root=0;
  do
    {
      root++;
      if (root>250) break;
    } while(root*root<=x);
  root--;
  return(root);
}

/* A function to give us an estimate of object power, for use in fenneling.
   Should really be in object[12].c, and I may move it there if it gets
   used for other things. */
s16b eval_object(object_type *o_ptr)
{
  s16b total=0, tmp = 0;
  s16b pval, basedam, bonusdam;
  bool ignoreslays = FALSE; /* hack to deal with older forged bows */
  int i;
  u32b flags;

  /* save some stuff to save time later */
  pval = o_ptr->pval;
  basedam = (o_ptr->dd * (o_ptr->ds +1))/2;
  bonusdam = o_ptr->to_d;
  switch (o_ptr->tval) {
  case TV_SHOT: case TV_ARROW: case TV_BOLT:
    total += basedam + bonusdam + o_ptr->to_h / 2;
    break;
  case TV_BOW:
    ignoreslays = TRUE;
    i = o_ptr->sval %10; /* damage mult. */
    if (o_ptr->flags3 & TR3_XTRA_MIGHT) i++;
    basedam = (((o_ptr->sval/10) + 4) * i) /2; /* hack */
    total += basedam + bonusdam + (o_ptr->to_h + o_ptr->to_a) / 2;
    break;
  case TV_DIGGING: case TV_HAFTED: case TV_POLEARM: case TV_SWORD:
    total += basedam + bonusdam + (o_ptr->to_h + o_ptr->to_a) / 2;
    break;
  case TV_BOOTS: case TV_GLOVES: case TV_CROWN: case TV_SHIELD:
  case TV_CLOAK: case TV_SOFT_ARMOR: case TV_HARD_ARMOR: case TV_DRAG_ARMOR:
    total += (o_ptr->ac + o_ptr->to_a + o_ptr->to_h) / 2 + bonusdam;
    break;
  default:
    ignoreslays=TRUE;
    total += bonusdam + (o_ptr->to_a + o_ptr->to_h) / 2;
  }
  flags = o_ptr->flags1;
  if (flags & TR1_STR)
    total += pval * 3;
  if (flags & TR1_INT)
    {
      if (p_ptr->realm == MAGE)
	total += pval * 2;
      total += pval * 2;
    }
  if (flags & TR1_WIS)
    {
      if (p_ptr->realm == PRIEST || p_ptr->realm == DRUID)
	total += pval * 2;
      total += pval * 2;
    }
  if (flags & TR1_DEX)
    total += pval * 3;
  if (flags & TR1_CON)
    {
      if (p_ptr->realm == NECRO)
	total += pval * 2;
      total += pval * 3;
    }
  if (flags & TR1_CHR)
    total += pval/2;
  if (flags & TR1_STEALTH)
    total += pval * 2;
  if (flags & TR1_SEARCH)
    total += pval/2;
  if (flags & TR1_INFRA)
    total += pval/2;
  if (flags & TR1_TUNNEL)
    total += pval/2;
  if (flags & TR1_SPEED)
    {
      if (pval>0)
	total += pval * pval;
      else total -= pval * pval;
    }
  if (flags & TR1_BLOWS)
    total += (pval > 2 ? 2 : pval) * (basedam + bonusdam);
  if (flags & TR1_NOMAGIC)
    total -= 50;
  if (!ignoreslays)
    {
      if (flags & TR1_SLAY_ANIMAL)
	total += basedam * 2;
      if (flags & TR1_SLAY_EVIL)
	total += basedam * 3;
      else
	{
	  if (flags & TR1_SLAY_DEMON)
	    total += (basedam * 3)/2;
	  if (flags & TR1_SLAY_ORC)
	    total += basedam;
	  if (flags & TR1_SLAY_TROLL)
	    total += basedam;
	  if (flags & TR1_SLAY_GIANT)
	    total += basedam;
	}
      if (flags & TR1_SLAY_UNDEAD)
	total += basedam * 2;
      if (flags & TR1_KILL_DRAGON)
	total += basedam * 3;
      else
	if (flags & TR1_SLAY_DRAGON)
	  total += basedam * 2;
      if (flags & TR1_VORPAL)
	total += basedam + bonusdam;
      if (flags & TR1_VENOM)
	tmp += (basedam * 3)/2;
      if (flags & TR1_BRAND_ACID)
	tmp += basedam * 3;
      if (flags & TR1_BRAND_ELEC)
	tmp += (basedam * 5)/2;
      if (flags & TR1_BRAND_FIRE)
	tmp += basedam * 2;
      if (flags & TR1_BRAND_COLD)
	tmp += basedam * 2;
      if (tmp > basedam * 5)
	total += basedam *5;
      else total += tmp;
    }
  if (flags & TR1_SOULSTEAL)
    total -= 10;
  if (flags & TR1_IMPACT)
    total += 10;
  flags = o_ptr->flags2;
  if (flags & TR2_SUST_STR)
    total += 2;
  if (flags & TR2_SUST_INT)
    total += 2;
  if (flags & TR2_SUST_WIS)
    total += 2;
  if (flags & TR2_SUST_DEX)
    total += 2;
  if (flags & TR2_SUST_CON)
    total += 2;
  if (flags & TR2_IM_ACID)
    total += 100;
  else
    if (flags & TR2_RES_ACID)
      total += 10;
  if (flags & TR2_IM_ELEC)
    total += 100;
  else
    if (flags & TR2_RES_ELEC)
      total += 10;
  if (flags & TR2_IM_FIRE)
    total += 100;
  else
    if (flags & TR2_RES_FIRE)
      total += 15;
  if (flags & TR2_IM_COLD)
    total += 100;
  else
    if (flags & TR2_RES_COLD)
      total += 15;
  if (flags & TR2_FREE_ACT)
    total += 15;
  if (flags & TR2_HOLD_LIFE)
    total += 30;
  if (flags & TR2_RES_POIS)
    total += 40;
  if (flags & TR2_IRONWILL)
    total += 30;
  if (flags & TR2_RES_LITE)
    total += 8;
  if (flags & TR2_RES_DARK)
    total += 10;
  if (flags & TR2_RES_BLIND)
    total += 9;
  if (flags & TR2_RES_SOUND)
    total += 8;
  if (flags & TR2_RES_SHARDS)
    total += 6;
  if (flags & TR2_RES_NETHER)
    total += 20;
  if (flags & TR2_RES_NEXUS)
    total += 6;
  if (flags & TR2_RES_CHAOS)
    total += 40;
  else if (flags & TR2_RES_CONF)
    total += 9;
  if (flags & TR2_RES_DISEN)
    total += 20;
  flags = o_ptr->flags3;
  if (flags & TR3_FEATHER)
    total += 2;
  if (flags & TR3_LITE)
    total += 1;
  if (flags & TR3_SEE_INVIS)
    total += 5;
  if (flags & TR3_TELEPATHY)
    total += 40;
  if (flags & TR3_SLOW_DIGEST)
    total += 3;
  if (flags & TR3_REGEN)
    total += 8;
  if (flags & TR3_XTRA_SHOTS)
    total += basedam + bonusdam;
  if (flags & TR3_IGNORE_ACID)
    total += 1;
  if (flags & TR3_IGNORE_ELEC)
    total += 1;
  if (flags & TR3_IGNORE_FIRE)
    total += 1;
  if (flags & TR3_IGNORE_COLD)
    total += 1;
  if (flags & TR3_DRAIN_EXP)
    total -= 15;
  if (flags & TR3_TELEPORT)
    total -= 20;
  if (flags & TR3_AGGRAVATE)
    total -= 20;
  if (flags & TR3_BLESSED)
    if (p_ptr->realm == PRIEST &&
	(o_ptr->tval == TV_SWORD || o_ptr->tval == TV_HAFTED))
      total += 5;
  if (flags & TR3_PERMA_CURSE) total -= 50;
  else if (flags & TR3_HEAVY_CURSE) total -= 10;
  else if (flags & TR3_CURSED) total -= 5;
  return total;
}

/* add a curse to an object. returns the object's new power rating. */
s16b hose_obj(object_type *o_ptr)
{
  s16b pow;
  int i;

  pow = eval_object(o_ptr);
  i = rand_int((pow >200 ? 200 : pow));
  if (i<100)
    {
      if (rand_int(100) < 85)
	{
	  o_ptr->flags3 |= TR3_CURSED;
	  o_ptr->ident |= IDENT_CURSED;
	}
      else
	o_ptr->flags3 |= TR3_TELEPORT;
    }
  else if (i<125)
    {
      if (o_ptr->tval == TV_SWORD || o_ptr->tval == TV_HAFTED || o_ptr->tval == TV_POLEARM)
	{
	  o_ptr->flags3 |= TR1_SOULSTEAL;
	}
    }
  else if (i<150)
    {
	  o_ptr->flags3 |= TR3_CURSED;
	  o_ptr->flags3 |= TR3_HEAVY_CURSE;
	  o_ptr->ident |= IDENT_CURSED;
    }
  else if (i<165)
    {
      o_ptr->flags3 |= TR3_AGGRAVATE;
    }
  else if (i<180)
    {
      o_ptr->flags3 |= TR3_DRAIN_EXP;
    }
  else
    {
      if (rand_int(2))
	{
	  o_ptr->flags3 |= TR3_CURSED;
	  o_ptr->flags3 |= TR3_HEAVY_CURSE;
	  o_ptr->flags3 |= TR3_PERMA_CURSE;
	  o_ptr->ident |= IDENT_CURSED;
	}
      else o_ptr->flags1 |= TR1_NOMAGIC;
    }
  return eval_object(o_ptr);
}

/* a function that decides whether two objects like being fenneled. */

bool fennel_ok(object_type *src, object_type *dest)
{
  s16b srcpow, destpow;
  char buf[80];

  srcpow = eval_object(src);
  destpow = eval_object(dest);
  if (srcpow - 50 >= destpow)
    {
      object_desc(buf, src, FALSE , 2);
      msg_format("Your %s will not be fenneled!", buf);
      return FALSE;
    }
  if (destpow > 200)
    {
      object_desc(buf, dest, FALSE , 2);
      msg_format("Your %s will not be fenneled!", buf);
      return FALSE;
    }
  return TRUE;
}

/* A function to make certain that fenneled objects don't get too
   absurd. */
void cap_vals(object_type *o_ptr)
{
  if (o_ptr->to_h > 30) o_ptr->to_h = 30;
  if (o_ptr->to_d > 30) o_ptr->to_d = 30;
  if (o_ptr->to_a > 40) o_ptr->to_a = 40;
  if (o_ptr->dd > 9) o_ptr->dd = 9;
  if (o_ptr->pval > 20) o_ptr->pval = 20;
  if (o_ptr->flags1 & (TR1_PVAL_MASK & ~(TR1_INFRA | TR1_SPEED | TR1_BLOWS)))
    if (o_ptr->pval > 8) o_ptr->pval = 8;    
}

/* Helper function for fenneling. Adds some of the flags belonging to
   src to dest, then sets the special flags correctly. */
void merge_flags(object_type *dest, object_type *src)
{
  u32b nflags[3], tmp;
  s16b pow, i, count = 0;
  bool add;

  nflags[0] = src->flags1 & ~(src->flags1 & dest->flags1);
  nflags[1] = src->flags2 & ~(src->flags2 & dest->flags2);
  nflags[2] = src->flags3 & ~(src->flags3 & dest->flags3);

  pow = eval_object(dest);
  for(i=0; i<3; i++)
    {
      for(tmp=1;tmp>0;tmp<<=1)
	{
	  if (nflags[i] & tmp)
	    {
	      nflags[i] &= ~(tmp);
	      if (pow < 10) add = TRUE;
	      else if (pow < 100) add = (rand_int(4) ? TRUE : FALSE);
	      else if (pow < 150) add = (rand_int(2) ? FALSE : TRUE);
	      else if (pow < 200) add = (rand_int(4) ? FALSE : TRUE);
	      else add = (rand_int(10) ? FALSE : TRUE);
	      if (add)
		switch (i) {
		case 0: dest->flags1 |= tmp; break;
		case 1: dest->flags2 |= tmp; break;
		case 2: dest->flags3 |= tmp; break;
		}
	    }
	}
    }

  /* Set the special flags correctly. */
  dest->flags3 &= ~(TR3_EASY_KNOW);
  if (src->flags3 & TR3_SHOW_MODS) dest->flags3 |= TR3_SHOW_MODS;
  if (!(dest->flags3 & TR3_HIDE_TYPE && src->flags3 & TR3_HIDE_TYPE))
    {
      count = 0;
      tmp = dest->flags1;
      if (tmp & TR1_STR) count++;
      if (tmp & TR1_INT) count++;
      if (tmp & TR1_WIS) count++;
      if (tmp & TR1_DEX) count++;
      if (tmp & TR1_CON) count++;
      if (tmp & TR1_CHR) count++;
      if (tmp & TR1_STEALTH) count++;
      if (tmp & TR1_SEARCH) count++;
      if (tmp & TR1_INFRA) count++;
      if (tmp & TR1_TUNNEL) count++;
      if (tmp & TR1_SPEED) count++;
      if (tmp & TR1_BLOWS) count++;
      if (count > 1) dest->flags3 |= TR3_HIDE_TYPE;
      else dest->flags3 &= ~(TR3_HIDE_TYPE);
    }
}

void do_cmd_talents()
{
  int i, j, k, x, item, item2, dir, mask, level, ability, sv;
  char str[80],tval;
  object_type *o_ptr, *j_ptr;
  object_kind *k_ptr;
  object_type tmp_obj;
  char tmp;

  if (p_ptr->tt && !wizard)
    {
      msg_print("You can't use any talents right now.");
      return;
    }
  Term_save();
  clear_from(0);
  put_str("Selecting a Skill",3,30);
  /* Note we reselect ability, since we may have many talents */
  i=0; j=0;
  for(k=0;k<S_NUM;k++)
    {
      if (tnames[k][0] && p_ptr->cur_skill[k]>=min[k])
	{
	  ++i;
	  ++j;
	  sprintf(str,"%c) %s",j+96,tnames[k]);
	  put_str(str,j+5,30); /* Ignore 'blank' skills */
	}
    }
  if (!i)
    { /* No talents available */
      Term_load();
      msg_print("You can't use any talents yet.");
      return;
    }
  put_str(format("Select a Talent (a-%c) or press ESC.", i+96), 18, 30);
  while (1)
    {
      tmp=inkey();
      if (tmp==27)
	{
	  /* Don't use a talent */
	  Term_load();
	  return; 
	}
      if (tmp>='a' && tmp<='z')
	tmp-=32;
      if (tmp > 64 && tmp <= 64+i) break;
    }
  i = tmp-64;
  k = 0;
  while(i > 0)
    {
      if (tnames[k][0] && p_ptr->cur_skill[k]>=min[k])
	--i;
      if (i)
	++k;
    }
  ability = k; /* k holds the REAL ability we're using */
  level = smod(ability);
  energy_use = 100;
  Term_load();
  switch(ability) {
      /* fenneling talent (has to be VERY difficult! otherwise unbalancing.) */
      /* this part (fenneling talent) Copyright Frits Daalmans, 1995 */

      /* Well, it was still unbalancing, and not hard enough, so I
	 hacked on it. -- Julian Lighton, 1997 */
    case S_DEVICE:
      {
	bool trashsrc = FALSE, trashdest = FALSE, kaboom = FALSE;
	s16b oldpow = 0, newpow = 0;

	if (p_ptr->confused > 0)
	  {
	    msg_print ("You are too confused.");
	    return;
	  }
	if (no_lite ())
	  {
	    msg_print ("You have no light!");
	    return;
	  }
	if (p_ptr->blind > 0)
	  {
	    msg_print ("You can't see!");
	    return;
	  }

	if (!get_com("Merge W)ands, S)taffs, R)ings, aM)ulets, A)rmor, wE)aponry? ", &tmp))
	  return;

	switch (tmp) {
	    /* Wands */
	  case 'w':
	    {
	      if (smod (S_INFUSION) < 10)
		{
		  msg_print ("You do not know enough about wands yet!");
		  return;
		}
	      tval = TV_WAND;
	      break;
	    }

	  /* Staves */
	  case 's':
	    {
	      if (level <= 10)
		{
		  msg_print("You are not skilled enough!");
		  return;
		}

	      if (smod (S_INFUSION) < 20)
		{
		  msg_print ("You do not know enough about staves yet!");
		  return;
		}
	      tval = TV_STAFF;
	      break;
	    }

	  /* Rings */
	  case 'r':
	    {
	      if (level <= 30)
		{
		  msg_print("You are not skilled enough!");
		  return;
		}

	      if (smod (S_INFUSION) < 30)
		{
		  msg_print ("You need to know more about magical infusion of rings!");
		  return;
		}
	      tval = TV_RING;
	      break;
	    }

	  /* aMulets */
	  case 'm':
	    {
	      if (level <= 35)
		{
		  msg_print("You are not skilled enough!");
		  return;
		}

	      if (smod (S_INFUSION) < 35)

		{
		  tmp = 0;
		  msg_print ("You need to know more about magical infusion of amulets!");
		}
	      tval = TV_AMULET;
	      break;
	    }

	  /* Armor */
	  case 'a':
	    {
	      if (level <= 40)
		{
		  msg_print("You are not skilled enough!");
		  return;
		}

	      if (smod(S_INFUSION) < 40)
		{
		  msg_print("You are not skilled enough at infusion.");
		  return;
		}
	      if(smod(S_ARMOR) <= 15)
		{
		  msg_print("You need to be better at forging armor first.");
		  return;
		}
	      if (!get_com("What kind, (H)elmet, (B)oots, (S)hield, (C)loak, (G)auntlets, or (A)rmor? ", &tmp)) return;
	      switch (tmp)
		{
		case 'h': tval = TV_HELM; break;
		case 'b': tval = TV_BOOTS; break;
		case 's': tval = TV_SHIELD; break;
		case 'c': tval = TV_CLOAK; break;
		case 'g': tval = TV_GLOVES; break;
		case 'a': tval = TV_HARD_ARMOR; break;
		default: return;
		}
	      break;
	    }

	  /* wEaponry */
	  case 'e':
	    {
	      if (level <= 50)
		{
		  msg_print("You are not skilled enough!");
		  return;
		}
	      if (smod(S_INFUSION) <= 40)
		{
		  msg_print("You are not skilled enough at infusion.");
		  return;
		}
	      if(smod(S_WEAPON) <= 15)
		{
		  msg_print("You need to be better at smithing first.");
		  return;
		}

	      if (!get_com("What kind of weapon, (S)word, (M)ace, or (P)olearm?", &tmp)) return;
	      switch (tmp)
		{
		case 's': tval = TV_SWORD; break;
		case 'm': tval = TV_HAFTED; break;
		case 'p': tval = TV_POLEARM; break;
		default: return;
		}
	      break;
	    }
	  default:
	    return;
	  }

	item_tester_tval = tval;
	if (!get_item (&item, "Merge which item?",
		       FALSE, TRUE, FALSE)) return;
	item_tester_tval = tval;
	if ((!get_item (&item2, "Into which other item?",
			FALSE, TRUE, FALSE)) || (item == item2)) return;
	o_ptr = &inventory[item];
	if(inventory[item2].number > 1)
	  {
	    tmp_obj = inventory[item2];
	    inventory[item2].number--;
	    tmp_obj.number = 1;
	    total_weight -= tmp_obj.weight;
	    item2 = inven_carry(&tmp_obj, FALSE);
	  }

	j_ptr = &inventory[item2];
	if(artifact_p(o_ptr) || artifact_p(j_ptr))
	  {
	    msg_print("You cannot fennel artifacts.");
	    return;
	  }
	if(o_ptr->ident & IDENT_BROKEN || j_ptr->ident & IDENT_BROKEN)
	  {
	    msg_print("There must be more material left to work with.");
	    return;
	  }

	j_ptr->number = 1;

	switch (tval) {
	case TV_WAND:
	  if (o_ptr->sval != j_ptr->sval)
	    {
	      msg_print ("You can only fennel between wands of the same type!");
	      break;
	    }
	  j_ptr->pval += (o_ptr->pval * level) / 50;
	  /* 	  j_ptr->b_cost = isqrt (sqr (j_ptr->b_cost) + sqr (o_ptr->b_cost)); */
	  /* Hack -- we no longer "know" the item */
	  j_ptr->ident &= ~(IDENT_KNOWN);

	  /* Hack -- we no longer think the item is empty */
	  j_ptr->ident &= ~(IDENT_EMPTY);
	  trashsrc = TRUE;

	  /* prevent absurd numbers of charges.
	     Taken from recharging code. */
	  x = (level + 100 - (k_info[j_ptr->k_idx].level) - (10 * j_ptr->pval)) / 15;
	  /* Paranoia -- prevent crashes */
	  if (x < 1) x = 1;
	  if (randint (25) > level || rand_int(x) == 0)
	    {
	      msg_print ("You blew up both wands!");
	      trashdest = TRUE;
	    }
	  break;

	case TV_STAFF:
	  if (o_ptr->sval != j_ptr->sval)
	    {
	      msg_print ("You can only fennel between staves of the same type!");
	      break;
	    }
	  j_ptr->pval += (o_ptr->pval * level) / 50;
	  /* 	  j_ptr->b_cost = isqrt (sqr (j_ptr->b_cost) + sqr (o_ptr->b_cost)); */
	  /* Hack -- we no longer "know" the item */
	  j_ptr->ident &= ~(IDENT_KNOWN);
	  
	  /* Hack -- we no longer think the item is empty */
	  j_ptr->ident &= ~(IDENT_EMPTY);
	  trashsrc=TRUE;

	  /* prevent absurd numbers of charges.
	     Taken from recharging code. */
	  x = (level + 100 - (k_info[j_ptr->k_idx].level) - (10 * j_ptr->pval)) / 15;
	  /* Paranoia -- prevent crashes */
	  if (x < 1) x = 1;
	  if (randint (35) > level || rand_int(x) == 0)
	    {
	      msg_print ("You blew up both staves!");
	      trashdest=TRUE;
	    }
	  break;
	case TV_RING:
	  if (!fennel_ok(o_ptr, j_ptr)) break;
	  oldpow = eval_object(j_ptr);

	  if (j_ptr->pval < 1 || o_ptr->pval < 1)
	    j_ptr->pval += o_ptr->pval;
	  else
	    j_ptr->pval = isqrt (sqr (j_ptr->pval) + sqr (o_ptr->pval));

	  j_ptr->b_cost = isqrt (sqr (j_ptr->b_cost) + sqr (o_ptr->b_cost));

	  /* rings of slaying */
	  if (j_ptr->to_a < 1 || o_ptr->to_a < 1)
	    j_ptr->to_a += o_ptr->to_a;
	  else
	    j_ptr->to_a = isqrt(sqr (o_ptr->to_a) + sqr (j_ptr->to_a));

	  if (j_ptr->to_h < 1 || o_ptr->to_h < 1)
	    j_ptr->to_h += o_ptr->to_h;
	  else
	    j_ptr->to_h = isqrt(sqr (o_ptr->to_h) + sqr (j_ptr->to_h));

	  if (j_ptr->to_d < 1 || o_ptr->to_d < 1)
	    j_ptr->to_d += o_ptr->to_d;
	  else
	    j_ptr->to_d = isqrt(sqr (o_ptr->to_d) + sqr (j_ptr->to_d));

	  merge_flags(j_ptr, o_ptr);
	  cap_vals(j_ptr);

	  trashsrc = TRUE;

	  if (randint (45) > level)
	    {
	      msg_print ("You blew up both rings!");
	      trashdest = TRUE;
	      break;
	    }
	  newpow = eval_object(j_ptr);
	  if (rand_int(newpow) > (level * 3)/2)
	    {
	      if (newpow < 150)
		{
		  if (rand_int(4)) newpow = hose_obj(j_ptr);
		  else kaboom = TRUE;
		}
	      else if (newpow < 200)
		{
		  if (rand_int(2)) newpow = hose_obj(j_ptr);
		  else kaboom = TRUE;
		}
	      else
		{
		  if (rand_int(5)) newpow = hose_obj(j_ptr);
		  else kaboom = TRUE;
		}
	    }
	  
	  /* if you shoved too much power into the object, it could explode. */
	  if (newpow > 10 && oldpow > 10)
	    {
	      s16b diff;

	      diff = newpow-oldpow;
	      if (diff > oldpow * 3) kaboom = TRUE;
	      else if (diff > oldpow && rand_int(10) == 0) kaboom = TRUE;
	      else if (diff > 50 && rand_int(20) == 0) kaboom = TRUE;
	    }

	  if (kaboom)
	    {
	      char buf[80];

	      trashdest = TRUE;
	      object_desc(buf, j_ptr, FALSE , 2);
	      msg_format("Your %s explodes violently!", buf);
	      take_hit(randint(2) * randint(newpow), "an exploding object.");
	    }

	  break;

	case TV_AMULET:
	  if (!fennel_ok(o_ptr, j_ptr)) break;
	  oldpow = eval_object(j_ptr);

	  if (j_ptr->pval < 1 || o_ptr->pval < 1)
	    j_ptr->pval += o_ptr->pval;
	  else
	    j_ptr->pval = isqrt (sqr (j_ptr->pval) + sqr (o_ptr->pval));

	  /* merge ac bonii (Amulets of the Magi) */
	  if (j_ptr->to_a < 1 || o_ptr->to_a < 1)
	    j_ptr->to_a += o_ptr->to_a;
	  else
	    j_ptr->to_a = isqrt(sqr (o_ptr->to_a) + sqr (j_ptr->to_a));

	  j_ptr->b_cost = isqrt (sqr (j_ptr->b_cost) + sqr (o_ptr->b_cost));

	  merge_flags(j_ptr, o_ptr);
	  cap_vals(j_ptr);

	  trashsrc = TRUE;

	  if (randint (45) > level)
	    {
	      msg_print ("You blew up both amulets!");
	      trashdest = TRUE;
	      break;
	    }
	  newpow = eval_object(j_ptr);
	  if (rand_int(newpow) > (level * 3)/2)
	    {
	      if (newpow < 150)
		{
		  if (rand_int(4)) newpow = hose_obj(j_ptr);
		  else kaboom = TRUE;
		}
	      else if (newpow < 200)
		{
		  if (rand_int(2)) newpow = hose_obj(j_ptr);
		  else kaboom = TRUE;
		}
	      else
		{
		  if (rand_int(5)) newpow = hose_obj(j_ptr);
		  else kaboom = TRUE;
		}
	    }
	  
	  /* if you shoved too much power into the object, it could explode. */
	  if (newpow > 10 && oldpow > 10)
	    {
	      s16b diff;

	      diff = newpow-oldpow;
	      if (diff > oldpow * 3) kaboom = TRUE;
	      else if (diff > oldpow && rand_int(10) == 0) kaboom = TRUE;
	      else if (diff > 50 && rand_int(20) == 0) kaboom = TRUE;
	    }

	  if (kaboom)
	    {
	      char buf[80];

	      trashdest = TRUE;
	      object_desc(buf, j_ptr, FALSE , 2);
	      msg_format("Your %s explodes violently!", buf);
	      take_hit(randint(2) * randint(newpow), "an exploding object.");
	    }

	  break;
	case TV_HELM:
	case TV_BOOTS:
	case TV_SHIELD:
	case TV_CLOAK:
	case TV_GLOVES:
	case TV_HARD_ARMOR:
	  if (!fennel_ok(o_ptr, j_ptr)) break;
	  oldpow = eval_object(j_ptr);

	  if (j_ptr->pval < 1 || o_ptr->pval < 1)
	    j_ptr->pval += o_ptr->pval;
	  else
	    j_ptr->pval = isqrt (sqr (j_ptr->pval) + sqr (o_ptr->pval));

	  j_ptr->b_cost = isqrt (sqr (j_ptr->b_cost) + sqr (o_ptr->b_cost)) * level / 50;

	  if (j_ptr->to_a < 1 || o_ptr->to_a < 1)
	    j_ptr->to_a += o_ptr->to_a;
	  else
	    j_ptr->to_a = isqrt(sqr (o_ptr->to_a) + sqr (j_ptr->to_a));

	  if (j_ptr->to_h < 1 || o_ptr->to_h < 1)
	    j_ptr->to_h += o_ptr->to_h;
	  else
	    j_ptr->to_h = isqrt(sqr (o_ptr->to_h) + sqr (j_ptr->to_h));

	  if (j_ptr->to_d < 1 || o_ptr->to_d < 1)
	    j_ptr->to_d += o_ptr->to_d;
	  else
	    j_ptr->to_d = isqrt(sqr (o_ptr->to_d) + sqr (j_ptr->to_d));

	  merge_flags(j_ptr, o_ptr);
	  cap_vals(j_ptr);

	  trashsrc = TRUE;
	  if (randint (45) > level)
	    {
	      msg_print ("You blew up both items!");
	      trashdest = TRUE;
	      break;
	    }
	  newpow = eval_object(j_ptr);
	  if (rand_int(newpow) > (level * 3)/2)
	    {
	      if (newpow < 150)
		{
		  if (rand_int(4)) newpow = hose_obj(j_ptr);
		  else kaboom = TRUE;
		}
	      else if (newpow < 200)
		{
		  if (rand_int(2)) newpow = hose_obj(j_ptr);
		  else kaboom = TRUE;
		}
	      else
		{
		  if (rand_int(5)) newpow = hose_obj(j_ptr);
		  else kaboom = TRUE;
		}
	    }
	  
	  /* if you shoved too much power into the object, it could explode. */
	  if (newpow > 10 && oldpow > 10)
	    {
	      s16b diff;

	      diff = newpow-oldpow;
	      if (diff > oldpow * 3) kaboom = TRUE;
	      else if (diff > oldpow && rand_int(10) == 0) kaboom = TRUE;
	      else if (diff > 50 && rand_int(20) == 0) kaboom = TRUE;
	    }

	  if (kaboom)
	    {
	      char buf[80];

	      trashdest = TRUE;
	      object_desc(buf, j_ptr, FALSE , 2);
	      msg_format("Your %s explodes violently!", buf);
	      take_hit(randint(2) * randint(newpow), "an exploding object.");
	    }

	  break;


	case TV_SWORD:
	case TV_HAFTED:
	case TV_POLEARM:
	  if (!fennel_ok(o_ptr, j_ptr)) break;
	  oldpow = eval_object(j_ptr);

	  if (j_ptr->pval < 1 || o_ptr->pval < 1)
	    j_ptr->pval += o_ptr->pval;
	  else
	    j_ptr->pval = isqrt (sqr (j_ptr->pval) + sqr (o_ptr->pval));

	  j_ptr->b_cost = isqrt (sqr (j_ptr->b_cost) + sqr (o_ptr->b_cost)) * level / 50;

	  if (j_ptr->to_a < 1 || o_ptr->to_a < 1)
	    j_ptr->to_a += o_ptr->to_a;
	  else
	    j_ptr->to_a = isqrt(sqr (o_ptr->to_a) + sqr (j_ptr->to_a));

	  if (j_ptr->to_h < 1 || o_ptr->to_h < 1)
	    j_ptr->to_h += o_ptr->to_h;
	  else
	    j_ptr->to_h = isqrt(sqr (o_ptr->to_h) + sqr (j_ptr->to_h));

	  if (j_ptr->to_d < 1 || o_ptr->to_d < 1)
	    j_ptr->to_d += o_ptr->to_d;
	  else
	    j_ptr->to_d = isqrt(sqr (o_ptr->to_d) + sqr (j_ptr->to_d));

	  if (j_ptr->dd < 1 || o_ptr->dd < 1)
	    j_ptr->dd += o_ptr->dd;
	  else
	    j_ptr->dd = isqrt(sqr (o_ptr->dd) + sqr (j_ptr->dd));

	  merge_flags(j_ptr, o_ptr);
	  cap_vals(j_ptr);

	  trashsrc = TRUE;
	  if (randint (45) > level)
	    {
	      msg_print ("You blew up both items!");
	      trashdest = TRUE;
	      break;
	    }
	  newpow = eval_object(j_ptr);
	  if (rand_int(newpow) > (level * 3)/2)
	    {
	      if (newpow < 150)
		{
		  if (rand_int(4)) newpow = hose_obj(j_ptr);
		  else kaboom = TRUE;
		}
	      else if (newpow < 200)
		{
		  if (rand_int(2)) newpow = hose_obj(j_ptr);
		  else kaboom = TRUE;
		}
	      else
		{
		  if (rand_int(5)) newpow = hose_obj(j_ptr);
		  else kaboom = TRUE;
		}
	    }
	  
	  /* if you shoved too much power into the object, it could explode. */
	  if (newpow > 10 && oldpow > 10)
	    {
	      s16b diff;

	      diff = newpow-oldpow;
	      if (diff > oldpow * 3) kaboom = TRUE;
	      else if (diff > oldpow && rand_int(10) == 0) kaboom = TRUE;
	      else if (diff > 50 && rand_int(20) == 0) kaboom = TRUE;
	    }

	  if (kaboom)
	    {
	      char buf[80];

	      trashdest = TRUE;
	      object_desc(buf, j_ptr, FALSE , 2);
	      msg_format("Your %s explodes violently!", buf);
	      take_hit(randint(2) * randint(newpow), "an exploding object.");
	    }

	  break;
	default:
	  break;
	}
	if (trashsrc) inven_item_decrease(item);
	p_ptr->tt = newpow;
	if (p_ptr->tt < 100) p_ptr->tt = 100;
	if (trashdest) inven_item_decrease(item2);

	p_ptr->notice |= (PN_COMBINE | PN_REORDER);
	p_ptr->window |= (PW_INVEN);
	break;
      }
    case S_ALCHEMY: case S_INFUSION:
      if (ability==S_INFUSION) ability=-ability;
      tmp=1;
      if (dun_level!=0)
	{
	  msg_print("You can only forge while in the town!");
	  tmp=0;
	}
      else /* Ok. Make a cool item.	But make it RANDOM */
	{
	  if (ability<0)
	    {
	      if (!get_com("Make a (W)and or (S)taff?", &tmp)) return;
	      if (tmp=='w')
		tval=TV_WAND;
	      else if (tmp=='s')
		tval=TV_STAFF;
	      else
		tmp=0;
	    }
	  else
	    {
	      if (!get_com("Make a (S)croll or (P)otion?", &tmp)) return;
	      if (tmp=='s')
		{
		  tval = TV_SCROLL;
		}
	      else if (tmp=='p')
		tval = TV_POTION;
	      else
		tmp=0;
	    }
	  if (tmp)
	    {
	      if (p_ptr->confused > 0)
		{
		  msg_print("You are too confused.");
		  return;
		}
	      if (no_lite())
		{
		  msg_print("You have no light!");
		  return;
		}
	      if (p_ptr->blind > 0)
		{
		  msg_print("You can't see!");
		  return;
		}
	      item_tester_tval = TV_COMPONENT;
	      if (!get_item(&item, "Use which component?", FALSE, TRUE, FALSE))
		{
		  msg_print("You have no components.");
		  return;
		}
	      o_ptr = &inventory[item];
	      j=(o_ptr->pval*5)+1;
	      if (ability<0) ability=-ability;
	      k=smod(ability);
	      tmp=(j+k)/2;
	      inven_item_decrease(item);
	      item_tester_tval = tval;
	      get_obj_num_hook = kind_fits_tval;
	      get_obj_num_prep();
	      place_general(py, px, tval,
			    k_info[get_obj_num(tmp)].sval);
	      item_tester_tval = 0;
	      get_obj_num_hook = NULL;
	      get_obj_num_prep();
	    }
	  if (tmp)
	    p_ptr->tt = 200;
	}
      p_ptr->notice |= (PN_COMBINE | PN_REORDER);
      p_ptr->window |= (PW_INVEN);
      break;
    case S_PERCEPTION:
      predict_weather(level*3);
      p_ptr->tt = 20;
      break;
    case S_SLAY_EVIL:
      detect_general(0, RF3_EVIL, "evil");
      p_ptr->tt = 100;
      break;
    case S_SLAY_ANIMAL:
      detect_general(0, RF3_ANIMAL, "animals");
      p_ptr->tt = 75;
      break;
    case S_DISARM:
      detect_traps();
      p_ptr->tt = 50;
      break;
    case S_SLAY_UNDEAD:
      if (p_ptr->exp < p_ptr->max_exp)
	{
	  msg_print("You feel your life force return to your body.");
	  p_ptr->exp =p_ptr->max_exp;
	}
      else
	msg_print("Nothing happened.");
      p_ptr->tt = 200;
      break;
    case S_KARATE:
      (void)set_afraid(0);
      (void)set_confused(0);
      (void)set_blind(0);
      p_ptr->tt=255;
      return;
      break;
    case S_BOWMAKE: /* Forge weapons*/
    case S_WEAPON:
    case S_ARMOR:
      tmp=0;
      if (dun_level!=0)
	{
	  msg_print("You can only forge while in the town!");
	  tmp=1;
	}
      i=0;
      if (ability==S_WEAPON && !tmp)
	tmp='w';
      else if (ability==S_ARMOR && !tmp)
	tmp='a';
      else if (!tmp)
	tmp='z';
      if (tmp=='w')
	{
	  if (!get_com("Forge a (S)word, (M)ace, or (P)olearm?", &tmp)) return;
	  if (tmp=='s')
	    i=TV_SWORD;
	  else if (tmp=='m')
	    i=TV_HAFTED;
	  else if (tmp=='p')
	    i=TV_POLEARM;
	  else
	    return;
	}
      if (tmp=='a')
	{
	  if (!get_com("Forge a (H)elmet, (B)oots, (S)hield, (G)auntlets, or (A)rmor?", &tmp)) return;
	  if (tmp=='h')
	    i=TV_HELM;
	  else if (tmp=='a')
	    i=TV_HARD_ARMOR;
	  else if (tmp=='b')
	    i=TV_BOOTS;
	  else if (tmp=='s')
	    i=TV_SHIELD;
	  else if (tmp=='g')
	    i=TV_GLOVES;
	  else
	    return;
	}
      if (tmp=='z')
	{
	  if (!get_com("Forge (B)ow, (C)rossbow, (A)rrows, or (S)hots?", &tmp)) return;
	  if (tmp=='b')
	    i=TV_BOW;
	  else if (tmp=='c')
	    i=-(TV_BOW);
	  else if (tmp=='a')
	    i=TV_ARROW;
	  else if (tmp=='s')
	    i=TV_BOLT;
	  else
	    return;
	}
      if (i) /* Now forge it */
	{
	  mask=i;
	  i=0;
	  if (p_ptr->confused > 0)
	    {
	      msg_print("You are too confused.");
	      return;
	    }
	  if (no_lite())
	    {
	      msg_print("You have no light to forge by!");
	      return;
	    }
	  if (p_ptr->blind)
	    {
	      msg_print("You can't see to forge!");
	      return;
	    }
	  item_tester_tval = TV_COMPONENT;
	  if (!get_item(&item, "Use which component?",
			FALSE, TRUE, FALSE))
	    {
	      msg_print("You have no components.");
	      return;
	    }
	  i = mask;

	  if(inventory[item].number > 1)
	    {
	      tmp_obj = inventory[item];
	      tmp_obj.number = 1;
	      inventory[item].number--;
	      total_weight -= tmp_obj.weight;

	      item = inven_carry(&tmp_obj, TRUE);
	    }

	  o_ptr = &inventory[item];
	  j = level+2;
	  dir = (j + (o_ptr->pval*10)/6)/2;
	  dir=dir*11/7;
	  k=0; /* Counter for # of special things added */
	  /* Max value for dir = 36 */
	  if (j<randint(dir/2))
	    {
	      msg_print("The component didn't work well.");
	      if (o_ptr->pval >= 2)
		o_ptr->pval -= 2;
	      else
		o_ptr->pval = 0;
	    }
	}

      /* Now try to forge the item */
      if (!i) return;
      o_ptr->pval = 0;
      o_ptr->tval = i;
      o_ptr->sval = o_ptr->k_idx - K_MIN_COMPONENT + 64;
      total_weight -= o_ptr->weight;
      o_ptr->weight *= 2; /* Whittle some away */
      o_ptr->weight /= rand_int(9)+2;
      total_weight += o_ptr->weight;

      switch(i)
	{
	case TV_ARROW: case TV_BOLT:
	  if (i==TV_BOLT)
	    {
	      o_ptr->to_h = randint(dir/7);
	      o_ptr->to_d = 2+randint(dir/5);
	      o_ptr->ds=4+(dir/7);
	      o_ptr->k_idx=80;
	    }
	  else
	    { /* Arrows */
	      o_ptr->to_h = 2+randint(dir/5);
	      o_ptr->to_d = randint(dir/6);
	      o_ptr->ds=3+(dir/8);
	      o_ptr->k_idx=78;
	    }
	  o_ptr->dd=1;
	  o_ptr->ident |= (IDENT_KNOWN);
	  o_ptr->number=5+randint(dir/2)+dir/4;
	  /* No specials---these are powerful ENOUGH! */
	  k_ptr = &k_info[o_ptr->k_idx];
	  o_ptr->sval = k_ptr->sval;
	  total_weight -= o_ptr->weight;
	  o_ptr->weight = k_ptr->weight;
	  total_weight += o_ptr->weight * o_ptr->number;
	  break;
	case TV_BOW: case -(TV_BOW):
	  if (i>0) /* Forge Bow */
	    { /* Max=(+13,+10) */
	      o_ptr->to_h = randint(dir/3)+1;
	      o_ptr->to_d = randint(dir/4+1);
	      tmp=2; /* Min Pval value */
	      if (dir>6)
		++tmp;
	      if (dir>12)
		++tmp;
	      if (tmp==2) o_ptr->k_idx=73;
	      else o_ptr->k_idx = 74;
	    }
	  else /* Crossbow */
	    {
	      o_ptr->to_h = randint(dir/4+1);
	      o_ptr->to_d = randint(dir/3)+1;
	      tmp=5; /* Max=(+9,+13) */
	      if (dir>7)
		++tmp;
	      if (tmp==5) o_ptr->k_idx=75;
	      else o_ptr->k_idx=76;
	      o_ptr->tval=TV_BOW;
	    }
	  o_ptr->dd=1;
	  o_ptr->ds=1;
	  o_ptr->ident |= (IDENT_KNOWN);
	  o_ptr->pval=tmp;
	  if (randint(dir*3/2) > 3)
	    {
	      j=randint(dir/2)+2;
	      if (i>0)
		msg_print("This is a special bow!");
	      else
		msg_print("This is a special crossbow!");
	    }
	  for(k=1;k<=j;k++)
	    {
	      sv = randint(10);
	      if (sv <= 3)
		select_attrib(dir*2, 1, TR1_DEX|TR1_CON| TR1_STR, o_ptr);
	      if (sv > 3 && sv <= 6)
		select_attrib(dir*2, 2, TR2_FREE_ACT| TR2_RES_COLD|TR2_RES_ACID| TR2_RES_FIRE| TR2_IRONWILL|
			      TR2_RES_DISEN| TR2_RES_NEXUS| TR2_RES_SHARDS| TR2_HOLD_LIFE|
			      TR2_RES_CONF| TR2_RES_BLIND, o_ptr);
	      if (sv > 6 && sv <= 8)
		select_attrib(dir*2, 3, TR3_SEE_INVIS|
			      TR3_SLOW_DIGEST| TR3_REGEN| TR3_LITE, o_ptr);
	      else {
		++o_ptr->to_h;
		++o_ptr->to_d;
	      }
	    }
	  k_ptr = &k_info[o_ptr->k_idx];
	  o_ptr->sval = k_ptr->sval;
	  total_weight -= o_ptr->weight;
	  o_ptr->weight = k_ptr->weight;
	  total_weight += o_ptr->weight;
	  break;
	case TV_SWORD:
	  o_ptr->to_h = randint(dir/4+1); /* Max=(+10,+8) */
	  o_ptr->to_d = randint(dir/5+1);
	  o_ptr->dd = 1+dir/11+randint(dir/19);
	  o_ptr->ds = 2+dir/8+randint(dir/13);
	  o_ptr->weight *= 2; /* Max=5d8 */
	  o_ptr->ident |= (IDENT_KNOWN);
	  if (randint(dir*3/2) > 3)
	    {
	      j=randint(dir/5)+2;
	      msg_print("This is a special sword!");
	      for(k=1;k<=j;k++)
		{
		  sv = randint(14);
		  /* This adds ONE of the listed attributes. */
		  if(sv<=6) select_attrib(dir*2, 1,
					  TR1_STR| TR1_DEX| TR1_CON| TR1_INT| TR1_WIS| TR1_CHR|
					  TR1_SLAY_EVIL| TR1_SLAY_UNDEAD| TR1_SLAY_DRAGON|
					  TR1_KILL_DRAGON| TR1_BRAND_FIRE| TR1_BRAND_COLD|
					  TR1_BRAND_ELEC| TR1_SPEED| TR1_VORPAL, o_ptr);
		  else if(sv<=9) select_attrib(dir*2, 2,
					       TR2_RES_ELEC| TR2_RES_ACID| TR2_RES_COLD| TR2_RES_FIRE|
					       TR2_IRONWILL, o_ptr);
		  else if(sv<=12) select_attrib(dir*2, 3,
						TR3_SEE_INVIS| TR3_REGEN| TR3_LITE, o_ptr);
		  else if(sv==13) add_sust(dir*2, o_ptr);
		  else
		    {
		      o_ptr->to_d += 3;
		      o_ptr->to_h += 3;
		    }
		}
	    }
	  break;
	case TV_POLEARM:
	  o_ptr->to_h =	randint(dir/5+1); /* Max=(+7,+10) */
	  o_ptr->to_d = randint(dir/4+1);
	  o_ptr->dd = 1+(dir/13); /* Max=3d14 */
	  o_ptr->ds = 2+dir/5+randint(dir/7);
	  o_ptr->weight = o_ptr->weight * 2/3;
	  o_ptr->ident |= (IDENT_KNOWN);
	  if (randint(dir*3/2) > 3)
	    {
	      msg_print("This is a special polearm!");
	      j=randint(dir/4)+3;
	      for(k=1;k<=j;k++)
		{
		  sv = randint(10);
		  if(sv<=2) select_attrib(dir*2, 1,
					  TR1_STR| TR1_INT| TR1_WIS| TR1_DEX| TR1_CON| TR1_CHR|
					  TR1_SLAY_UNDEAD| TR1_KILL_DRAGON| TR1_SLAY_ANIMAL|
					  TR1_SLAY_EVIL| TR1_SLAY_ORC| TR1_SLAY_TROLL|
					  TR1_SLAY_GIANT| TR1_INFRA, o_ptr);
		  else if(sv<=4) select_attrib(dir*2, 2,
					       TR2_RES_BLIND| TR2_RES_CONF|
					       TR2_RES_LITE| TR2_RES_DARK|
					       TR2_RES_NEXUS| TR2_RES_NETHER| TR2_HOLD_LIFE|
					       TR2_RES_ACID| TR2_RES_FIRE| TR2_RES_ELEC|
					       TR2_FREE_ACT, o_ptr);
		  else if(sv<=6) select_attrib(dir*2, 3,
					       TR3_SEE_INVIS| TR3_LITE| TR3_TELEPATHY, o_ptr);
		  else if(sv==7) add_sust(dir*2, o_ptr);
		  else
		    { o_ptr->to_h+=2; o_ptr->to_d+=2; }
		}
	    }
	  break;
	case TV_HAFTED:
	  o_ptr->to_h = 1+randint(dir/5); /* Max=(+8,+8) */
	  o_ptr->to_d = 1+randint(dir/5);
	  o_ptr->dd = 2+dir/11+randint(dir/19);
	  o_ptr->ds = 3+dir/11+randint(dir/11);
	  o_ptr->weight = o_ptr->weight*5/2;
	  o_ptr->ident |= (IDENT_KNOWN); /* Max=6d9 */
	  if (randint(dir*3/2) > 3)
	    {
	      msg_print("This is a special mace!");
	      j=randint(dir/7)+2;
	      for(k=1;k<=j;k++)
		{
		  sv=randint(10);
		  if(sv<=4) select_attrib(dir*2, 1,
					  TR1_STR| TR1_INT| TR1_WIS| TR1_DEX| TR1_CON| TR1_CHR|
					  TR1_SPEED| TR1_TUNNEL| TR1_KILL_DRAGON| TR1_SLAY_ORC|
					  TR1_SLAY_TROLL| TR1_SLAY_DEMON| TR1_SLAY_GIANT, o_ptr);
		  else if(sv<=6) select_attrib(dir*2, 2,
					       TR2_FREE_ACT| TR2_RES_ELEC| TR2_RES_FIRE| TR2_RES_CHAOS |
					       TR2_RES_COLD| TR2_RES_ACID| TR2_RES_LITE| TR2_RES_CONF|
					       TR2_RES_BLIND| TR2_IRONWILL, o_ptr);
		  else if(sv==7) select_attrib(dir*2, 3,
					       TR3_LITE| TR3_TELEPATHY| TR3_SEE_INVIS, o_ptr);
		  else
		    {
		      o_ptr->to_h+=2;
		      o_ptr->to_d+=2;
		    }
		}
	    }
	  break;
	case TV_HELM:
	  o_ptr->ac = 1+dir/7; /* Max=[6,+3] */
	  o_ptr->to_a = 1+randint(dir/11);
	  if (randint(dir*3/2) > 3)
	    {
	      msg_print("This is a special helm!");
	      j=randint(dir/8)+1;
	      for(k=1;k<=j;k++)
		{
		  sv=randint(16);
		  if(sv<=5) select_attrib(dir*2, 1,
					  TR1_STR| TR1_INT| TR1_WIS| TR1_DEX| TR1_CON| TR1_CHR|
					  TR1_INFRA, o_ptr);
		  else if(sv<=10) select_attrib(dir*2, 2,
						TR2_RES_LITE| TR2_RES_BLIND|
				 		TR2_RES_ACID| TR2_RES_FIRE| TR2_RES_COLD|
						TR2_RES_CONF| TR2_RES_SOUND| TR2_RES_ELEC, o_ptr);
		  else if(sv<=12) select_attrib(dir*2, 3,
						TR3_LITE| TR3_TELEPATHY| TR3_SEE_INVIS, o_ptr);
		  else
		    o_ptr-> to_a += 5;
		}
	    }
	  break;
	case TV_HARD_ARMOR:
	  o_ptr->ac=3+dir+randint(dir/6); /* Max=[43,+10] */
	  o_ptr->to_a  =  1 + (dir/8) +randint(dir/7);
	  o_ptr->weight*=2;
	  o_ptr->to_h=-(o_ptr->ac/7);
	  o_ptr->to_h+=dir/5;
	  if (o_ptr->to_h>0)
	    o_ptr->to_h=0;
	  if (randint(dir*3/2) > 3)
	    {
	      msg_print("This is special armor!");
	      j=randint(dir/2)+2;
	      for(k=1;k<=j;k++)
		{
		  sv=randint(10);
		  if(sv<=7) select_attrib(dir*2, 2,
					  TR2_RES_ACID| TR2_RES_FIRE| TR2_RES_COLD|
					  TR2_RES_POIS| TR2_RES_DISEN| TR2_RES_NEXUS|
					  TR2_RES_SHARDS| TR2_RES_CONF| TR2_RES_BLIND|
					  TR2_RES_DARK| TR2_HOLD_LIFE| TR2_RES_CHAOS|
					  TR2_RES_ELEC| TR2_IM_FIRE| TR2_IM_COLD| TR2_IM_ELEC|
					  TR2_IRONWILL, o_ptr);
		  else if(sv==8) select_attrib(dir*2, 3,
					       TR3_SLOW_DIGEST| TR3_REGEN, o_ptr);
		  else
		    o_ptr->to_a += 8;
		}
	    }
	  break;
	case TV_BOOTS:
	  o_ptr->ac=1+dir/11+randint(dir/19); /* Max=[5,+5] */
	  o_ptr->to_a=1+(dir/13)+randint(dir/14);
	  if (randint(dir*3/2) > 6)
	    {
	      msg_print("You made excellent boots!");
	      j=randint(dir/9)+1;
	      for(k=1;k<=j;k++)
		{
		  sv=randint(7);
		  if(sv<=2) select_attrib(dir*2, 1,
					  TR1_STEALTH| TR1_SPEED, o_ptr);
		  else if(sv<=4) select_attrib(dir*2, 2,
					       TR2_RES_NEXUS| TR2_RES_ACID | TR2_FREE_ACT, o_ptr);
		  else if(sv<=6) select_attrib(dir*2, 3,
					       TR3_FEATHER| TR3_REGEN, o_ptr);
		  else
		    o_ptr->to_a += 3;
		}
	    }
	  break;
	case TV_GLOVES:
	  o_ptr->ac=1+dir/13; /* Max=[3,+6] */
	  o_ptr->to_a=1+(dir/12)+randint(dir/13);
	  o_ptr->weight=o_ptr->weight*2/3;
	  if (randint(dir*3/2) > 4)
	    {
	      msg_print("You made excellent gloves!");
	      j=randint(dir/5)+2;
	      for(k=1;k<=j;k++)
		{
		  sv=randint(9);
		  if(sv==1) select_attrib(dir*2, 1,
					  TR1_STR| TR1_DEX, o_ptr);
		  else if(sv<=6) select_attrib(dir*2, 2,
					       TR2_RES_CONF| TR2_IM_FIRE| TR2_IM_COLD| TR2_RES_FIRE|
					       TR2_RES_COLD| TR2_RES_ACID| TR2_RES_ELEC| TR2_FREE_ACT|
					       TR2_RES_NETHER, o_ptr);
		  else if(sv==7) add_sust(dir*2, o_ptr);
		  else
		    {
		      o_ptr->to_h+=2+randint(dir/5);
		      o_ptr->to_d+=2+randint(dir/5);
		      ++o_ptr->to_a;
		    }
		}
	    }
	  break;
	case TV_SHIELD:
	  o_ptr->ac=2+dir/10+randint(dir/13); /* Max=[7,+7] */
	  o_ptr->to_a=1+(dir/15)+randint(dir/12);
	  o_ptr->weight=o_ptr->weight*3/2;
	  if (randint(dir*3/2) > 5)
	    {
	      msg_print("You made an excellent shield!");
	      j=randint(dir/6)+1;
	      for(k=1;k<=j;k++)
		{
		  sv=randint(5);
		  if(sv<=4) select_attrib(dir*2, 2,
					  TR2_RES_FIRE| TR2_RES_COLD| TR2_RES_ACID| TR2_RES_ELEC|
					  TR2_RES_POIS| TR2_RES_SHARDS| TR2_RES_CONF|
					  TR2_RES_BLIND| TR2_FREE_ACT| TR2_RES_NEXUS|
					  TR2_RES_DISEN| TR2_RES_SOUND| TR2_RES_CHAOS|
					  TR2_IM_COLD| TR2_IM_ELEC, o_ptr);
		  else
		    o_ptr->to_a += 2;
		}
	    }
	  if (tmp) p_ptr->tt = 255;
	  break;
	default:
	  break;
	}

      /* Window stuff */
      p_ptr->window |= (PW_INVEN);

      /* Combine and reorder the pack */
      p_ptr->notice |= (PN_COMBINE | PN_REORDER);

      /* Evaluate cost of the item */
      j = 0; /* Current cost */
      switch(i)
	{
	case TV_ARROW: case TV_BOLT: j = 1; break;
	case TV_BOW: j = 300; break;
	case TV_SWORD: j = 100; break;
	case TV_HAFTED: j = 150; break;
	case TV_POLEARM: j = 100; break;
	case TV_HELM: j = 50; break;
	case TV_HARD_ARMOR: j = 100; break;
	case TV_BOOTS: j = 50; break;
	case TV_SHIELD: j = 100; break;
	case TV_GLOVES: j = 50; break;
	}

      j += 200 * o_ptr->to_h;
      j += 200 * o_ptr->to_d;
      j += 100 * o_ptr->ac;
      j += 150 * o_ptr->to_a;
      j += 300 * o_ptr->pval;
      mask=1;
      /* This adds value for each special added */
      for(k = 0; k < 32; k++)
	{
	  if (o_ptr->flags1 & mask)
	    j += levels[k]*100;
	  if (o_ptr->flags2 & mask)
	    j += levels[k+32]*120;
	  if (o_ptr->flags3 & mask)
	    j += levels[k+64]*120;
	  mask <<= 1;
	}
      o_ptr->b_cost = j;

      /* Clear flags1 if no pval so "(+0)" isn't displayed */
/*       if (!o_ptr->pval) o_ptr->flags1 = 0; */
      break;
    }
}
