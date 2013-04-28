/* talents.c:	Talents based on high levels of each skill */
#include "angband.h"

#define MAXP 70	/* Max % chance for adding attribute */
#define LOW	 5	/* % taken off per point of level BELOW attribute */
#define HIGH	4	/* % decrease per point of level ABOVE attribute */

/* These correspond to the skills listed.	Empty string = No talent for
   this skill */
cptr tnames[S_NUM]=
{"","","","","","Fennling","","","Find Traps","","","","",
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

void do_cmd_talents()
{
	int i, j, k, item, item2, dir, mask, level, skill, ability, sv;
	char str[80],tval;
	object_type *o_ptr, *j_ptr;
	object_kind *k_ptr;
	object_type tmp_obj;
	char tmp;
	skill = 0;

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
		if (tmp==27) goto done; /* Don't use a talent */
		if (tmp>='a' && tmp<='z')
			tmp-=32;
		if (tmp > 64 && tmp <= tmp+i) break;
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
	switch(ability)
	{
	/* fennling talent (has to be VERY difficult! otherwise unbalancing.) */
	/* this part (fennling talent) Copyright Frits Daalmans, 1995 */
		case S_DEVICE:
		if (!get_com("Merge W)ands, S)taffs, R)ings, aM)ulets, A)rmor, wE)aponry? ", &tmp)) goto done;

		/* Wands */
		if (tmp == 'w')
		{
			if (smod (S_INFUSION) > 10)
				tval = TV_WAND;
			 else
			{
				tmp = 0;
				msg_print ("You do not know enough about wands yet!");
			}
		}

		/* Staffs */
		if (tmp == 's')
		{
			if (level <= 10)
			{
				msg_print("You are not skilled enough!");
				goto done;
			}

			if (smod (S_INFUSION) > 20)
				tval = TV_STAFF;
			 else
			{
				tmp = 0;
				msg_print ("You do not know enough about staffs yet!");
			}
		}

		/* Rings */
		if (tmp == 'r')
		{
			if (level <= 30)
			{
				msg_print("You are not skilled enough!");
				goto done;
			}

			if (smod (S_INFUSION) > 30)
				tval = TV_RING;
			 else
			{
				tmp = 0;
				msg_print ("You need to know more about magical infusion of rings!");
			}
		}

		/* aMulets */
		if (tmp == 'm')
		{
			if (level <= 35)
			{
				msg_print("You are not skilled enough!");
				goto done;
			}

			if (smod (S_INFUSION) > 35)
				tval = TV_AMULET;
			else
			{
				tmp = 0;
				msg_print ("You need to know more about magical infusion of amulets!");
			}
		}

		/* Armor */
		if (tmp == 'a')
		{
			if (level <= 40)
			{
				msg_print("You are not skilled enough!");
				goto done;
			}

			if (smod(S_INFUSION) <= 40)
			{
				msg_print("You are not skilled enough at infusion.");
				goto done;
			}
			if(smod(S_ARMOR) <= 15)
			{
				msg_print("You need to be better at forging armor first.");
				goto done;
			}
			if (get_com("What kind, (H)elmet, (B)oots, (S)hield, (C)loak, (G)auntlets, or (A)rmor? ", &tmp)) goto done;
			if (tmp == 'h')
				tval = TV_HELM;
			else if (tmp == 'b')
				tval = TV_BOOTS;
			else if (tmp == 's')
				tval = TV_SHIELD;
			else if (tmp == 'c')
				tval = TV_CLOAK;
			else if (tmp == 'g')
				tval = TV_GLOVES;
			else if (tmp == 'a')
				tval = TV_HARD_ARMOR;
		}
		/* wEaponry */
		if (tmp == 'e')
		{
			if (level <= 50)
			{
				msg_print("You are not skilled enough!");
				goto done;
			}
			if (smod(S_INFUSION) <= 40)
			{
				msg_print("You are not skilled enough at infusion.");
				goto done;
			}
			if(smod(S_WEAPON) <= 15)
			{
				msg_print("You need to be better at smithing first.");
				goto done;
			}

			if (!get_com("What kind of weapon, (S)word, (M)ace, or (P)olearm?", &tmp)) goto done;
			if (tmp == 's')
				tval = TV_SWORD;
			else if (tmp == 'm')
				tval = TV_HAFTED;
			else if (tmp == 'p')
				tval = TV_POLEARM;
		}

		if (!tmp) goto done;
		if (p_ptr->confused > 0)
		{
			msg_print ("You are too confused.");
			goto done;
		}
		if (no_lite ())
		{
			msg_print ("You have no light!");
			goto done;
		}
		 if (p_ptr->blind > 0)
		{
			msg_print ("You can't see!");
			goto done;
		}

		item_tester_tval = tval;
		if (!get_item (&item, "Merge which item?",
			FALSE, TRUE, FALSE)) goto done;
		item_tester_tval = tval;
		if ((!get_item (&item2, "With which other item?",
			FALSE, TRUE, FALSE)) || (item == item2)) goto done;
		o_ptr = &inventory[item];
		if(inventory[item2].number > 1)
		{
			tmp_obj = inventory[item2];
			inventory[item2].number--;
			tmp_obj.number = 1;
			total_weight -= tmp_obj.weight;
			item2 = inven_carry(&tmp_obj);
		}

		j_ptr = &inventory[item2];
		if(artifact_p(o_ptr) || artifact_p(j_ptr))
		{
			msg_print("You cannot fennle artifacts.");
			goto done;
		}

		j_ptr->number = 1;

		switch (tval)
				{
			case TV_WAND:
			if (o_ptr->sval != j_ptr->sval)
			{
				msg_print ("You can only fennl between wands of the same type!");
				break;
			}
			j_ptr->pval += (o_ptr->pval * level) / 50;
			j_ptr->b_cost = isqrt (sqr (j_ptr->b_cost) + sqr (o_ptr->b_cost));
			inven_item_decrease(item);

			if (randint (25) > level)
			{
				msg_print ("You blew up both wands!");
				inven_item_decrease(item2);
			}
			break;

			case TV_STAFF:
			if (o_ptr->sval != j_ptr->sval)
			{
				msg_print ("You can only fennl between staffs of the same type!");
				break;
			}
			j_ptr->pval += (o_ptr->pval * level) / 50;
			j_ptr->b_cost = isqrt (sqr (j_ptr->b_cost) + sqr (o_ptr->b_cost));
			inven_item_decrease(item);
			if (randint (35) > level)
			{
				msg_print ("You blew up both staffs!");
				inven_item_decrease(item2);
			}
			break;

			case TV_RING:
			j_ptr->pval = j_ptr->pval + (o_ptr->pval * level / 50) / 2;
			j_ptr->pval = j_ptr->pval;
			j_ptr->b_cost = isqrt (sqr (j_ptr->b_cost) + sqr (o_ptr->b_cost));

			j_ptr->flags1 |= o_ptr->flags1;
			j_ptr->flags2 |= o_ptr->flags2;
			j_ptr->flags3 |= o_ptr->flags3;

			/* rings of slaying */
			j_ptr->to_a = isqrt (sqr (o_ptr->to_a) + sqr (j_ptr->to_a));
			j_ptr->to_a = isqrt (sqr (o_ptr->to_a) + sqr (j_ptr->to_a));
			j_ptr->to_h = isqrt (sqr (o_ptr->to_h) + sqr (j_ptr->to_h));
			j_ptr->to_d = isqrt (sqr (o_ptr->pval) + sqr (j_ptr->pval));

			inven_item_decrease (item);
			if (randint (45) > level)
			{
				msg_print ("You blew up both rings!");
				inven_item_decrease(item2);
			}
			break;

			case TV_AMULET:
			/* prevent the player to enchant augmentation amulets too high */
			j_ptr->pval = isqrt (sqr (o_ptr->pval) + sqr (j_ptr->pval)) * level / 50;
			if (j_ptr->pval > 5)
				j_ptr->pval = j_ptr->pval /2 +3;
			j_ptr->b_cost = isqrt (sqr (j_ptr->b_cost) + sqr (o_ptr->b_cost));

			j_ptr->flags1 |= o_ptr->flags1;
			j_ptr->flags2 |= o_ptr->flags2;
			j_ptr->flags3 |= o_ptr->flags3;
			inven_item_decrease (item);
			if (randint (45) > level)
			{
					msg_print ("You blew up both amulets!");
					inven_item_decrease(item2);
			}
			break;
			case TV_HELM:
			case TV_BOOTS:
			case TV_SHIELD:
			case TV_CLOAK:
			case TV_GLOVES:
			case TV_HARD_ARMOR:
			j_ptr->pval = isqrt (sqr (o_ptr->pval) + sqr (j_ptr->pval));
			j_ptr->b_cost = isqrt (sqr (j_ptr->b_cost) + sqr (o_ptr->b_cost)) * level / 50;

			j_ptr->flags1 |= o_ptr->flags1;
			j_ptr->flags2 |= o_ptr->flags2;
			j_ptr->flags3 |= o_ptr->flags3;

			j_ptr->to_a = isqrt (sqr (o_ptr->to_a) + sqr (j_ptr->to_a));

			j_ptr->ac = isqrt (sqr (o_ptr->ac) + sqr (j_ptr->ac));
			j_ptr->to_h = isqrt (sqr (o_ptr->to_h) + sqr (j_ptr->to_h));
			j_ptr->to_d = isqrt (sqr (o_ptr->to_d) + sqr (j_ptr->to_d));

			inven_item_decrease (item);
			if (randint (45) > level)
			{
					msg_print ("You blew up both items!");
					inven_item_decrease(item2);
			}
			break;


			case TV_SWORD:
			case TV_HAFTED:
			case TV_POLEARM:
			j_ptr->pval = isqrt (sqr (o_ptr->pval) + sqr (j_ptr->pval));
			j_ptr->b_cost = isqrt (sqr (j_ptr->b_cost) + sqr (o_ptr->b_cost)) * level / 50;

			j_ptr->flags1 |= o_ptr->flags1;
			j_ptr->flags2 |= o_ptr->flags2;
			j_ptr->flags3 |= o_ptr->flags3;

			j_ptr->to_a = isqrt (sqr (o_ptr->to_a) + sqr (j_ptr->to_a));

			j_ptr->to_h = isqrt (sqr (o_ptr->to_h) + sqr (j_ptr->to_h));
			j_ptr->to_d = isqrt (sqr (o_ptr->to_d) + sqr (j_ptr->to_d));
			j_ptr->dd = isqrt (sqr (o_ptr->dd) + sqr (j_ptr->dd));
			j_ptr->ds = isqrt (sqr (o_ptr->ds) + sqr (j_ptr->ds));
			inven_item_decrease (item);
			if (randint (45) > level)
			{
				msg_print ("You blew up both items!");
				inven_item_decrease(item2);
			}
			break;
		default:
			break;
		}
		p_ptr->window |= (PW_INVEN);
		break;
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
				if (!get_com("Make a (W)and or (S)taff?", &tmp)) goto done;
				if (tmp=='w')
					tval=TV_WAND;
				 else if (tmp=='s')
					tval=TV_STAFF;
				 else
					tmp=0;
			}
			 else
			{
				if (!get_com("Make a (S)croll or (P)otion?", &tmp)) goto done;
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
					goto done;
				}
				if (no_lite())
				{
					msg_print("You have no light!");
					goto done;
				}
				if (p_ptr->blind > 0)
				{
					msg_print("You can't see!");
					goto done;
				}
				item_tester_tval = TV_COMPONENT;
				if (!get_item(&item, "Use which component?", FALSE, TRUE, FALSE))
				{
					msg_print("You have no components.");
					goto done;
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

			}
			if (tmp)
				skill=200;
			}
		break;
		case S_PERCEPTION:
		predict_weather(level-60);
		skill=20;
		break;
		case S_SLAY_EVIL:
		detect_general(0, RF3_EVIL, "evil");
		skill=100;
		break;
		case S_SLAY_ANIMAL:
		detect_general(0, RF3_ANIMAL, "animals");
		skill=75;
		break;
		case S_DISARM:
		detect_trap();
		skill=50;
		break;
		case S_SLAY_UNDEAD:
		if (p_ptr->exp < p_ptr->max_exp)
		{
			msg_print("You feel your life force return to your body.");
			p_ptr->exp =p_ptr->max_exp;
		}
		 else
			msg_print("Nothing happened.");
		skill=200;
		break;
		case S_KARATE:
		Term_load();		/* Need to do this now so the screen updates */
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
			if (!get_com("Forge a (S)word, (M)ace, or (P)olearm?", &tmp)) goto done;
			if (tmp=='s')
				i=TV_SWORD;
			 else if (tmp=='m')
				i=TV_HAFTED;
			 else if (tmp=='p')
				i=TV_POLEARM;
			 else
				goto done;
		}
		if (tmp=='a')
		{
			if (!get_com("Forge a (H)elmet, (B)oots, (S)hield, (G)auntlets, or (A)rmor?", &tmp)) goto done;
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
				goto done;
		}
		if (tmp=='z')
		{
			if (!get_com("Forge (B)ow, (C)rossbow, (A)rrows, or (S)hots?", &tmp)) goto done;
			if (tmp=='b')
				i=TV_BOW;
			else if (tmp=='c')
				i=-(TV_BOW);
			else if (tmp=='a')
				i=TV_ARROW;
			else if (tmp=='s')
				i=TV_BOLT;
			else
				goto done;
		}
		if (i) /* Now forge it */
		{
			mask=i;
			i=0;
			 if (p_ptr->confused > 0)
			{
				msg_print("You are too confused.");
				goto done;
			}
			if (no_lite())
			{
				msg_print("You have no light to forge by!");
				goto done;
			}
			if (p_ptr->blind)
			{
				msg_print("You can't see to forge!");
				goto done;
			}
			item_tester_tval = TV_COMPONENT;
			if (!get_item(&item, "Use which component?",
				FALSE, TRUE, FALSE))
			{
				msg_print("You have no components.");
				goto done;
			}
			i = mask;

		if(inventory[item].number > 1)
			{
				tmp_obj = inventory[item];
				tmp_obj.number = 1;
				inventory[item].number--;
				total_weight -= tmp_obj.weight;

				/* Hack it so it doesn't stack */
				tmp_obj.pval += 100;
				item = inven_carry(&tmp_obj);
				inventory[item].pval -= 100;
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
		if (!i) goto done;
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
			o_ptr->ident |= ID_KNOWN;
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
			o_ptr->ident |= ID_KNOWN;
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
					select_attrib(dir*2, 1, TR1_DEX|TR1_CON| TR1_STR|
						TR1_SLAY_EVIL| TR1_BRAND_COLD|
						TR1_BRAND_FIRE, o_ptr);
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
			o_ptr->ident |= ID_KNOWN;
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
							TR2_RES_ELEC| TR2_RES_POIS| TR2_RES_COLD| TR2_RES_FIRE|
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
			o_ptr->ident |= ID_KNOWN;
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
						TR2_RES_CHAOS| TR2_RES_BLIND| TR2_RES_CONF|
						TR2_RES_LITE| TR2_RES_DARK| TR2_RES_DISEN|
						TR2_RES_NEXUS| TR2_RES_NETHER| TR2_HOLD_LIFE|
						TR2_RES_ACID| TR2_RES_FIRE| TR2_RES_POIS|
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
			o_ptr->ident |= ID_KNOWN; /* Max=6d9 */
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
						TR2_FREE_ACT| TR2_RES_ELEC| TR2_RES_FIRE|
						TR2_RES_COLD| TR2_RES_POIS| TR2_RES_LITE| TR2_RES_CONF|
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
						TR2_RES_CONF| TR2_RES_SOUND| TR2_RES_POIS, o_ptr);
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
						TR2_RES_ACID| TR2_RES_FIRE| TR2_RES_COLD| TR2_RES_ELEC|
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
						TR2_RES_NEXUS| TR2_RES_ACID, o_ptr);
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
						TR2_RES_POIS, o_ptr);
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
			if (tmp) skill=255;
			break;
			default:
			break;
		}

		/* Window stuff */
		p_ptr->window |= (PW_INVEN);

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
		if (!o_ptr->pval) o_ptr->flags1 = 0;
		break;
	}
done:
	p_ptr->tt = skill;
	if (msg_flag) msg_print(NULL);
	Term_load();
}
