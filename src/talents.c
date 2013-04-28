/* talents.c:  Talents based on high levels of each skill */
#include "monster.h"
#include "constant.h"
#include "config.h"
#include "types.h"
#include "externs.h"

#define MAXP 70  /* Max % chance for adding attribute */
#define LOW   5  /* % taken off per point of level BELOW attribute */
#define HIGH  4  /* % decrease per point of level ABOVE attribute */

/* This holds the 'relative' level of each attribute.  First 32 are the
   levels for the 'flags' bit flags.  Level 255 means not a valid flag.
   Also used for weapons appraisal */
unsigned short int levels[64]=
{ 30, 35, 35, 30, 30, 25, 35, 20, 25,100,100, 40, 90,
  30, 25, 45, 20, 15, 15,
  10, 15, 10, 45, 55, 35, 20, 15, 65, 75, 10, 50,100,

  35, 30, 25, 80, 25, 60, 80, 80, 80, 80, 35,255, 65,
 110, 85, 55, 50, 45, 45, 50, 85, 45, 90, 45, 70,255,
  80,255,255, 40,255,255
};

/* These correspond to the skills listed.  Empty string = No talent for this
skill */
char tnames[S_NUM][S_NUM]=
{"","","","","","","","","Find Traps","","","Sense Magic",
"","","Meditation","Weapon Forging","Armor Forging","Predict Weather",
"Sense Evil","Sense Animals","Sense Undead","","","Bowmaking",
"Alchemy","Infusion","","","",""};

/* These say the lowest value of skill you can have to use this talent */
int8u min[S_NUM]={0,0,0,0,0,0,0,0,200,0,0,210,0,
0,150,50,50,50,200,150,220,0,0,80,100,120,0,0,0};

char get2(char *);
void select_attrib(int,int,long, inven_type *);

void talents(t)
int t;
{
  int i,j,k,item_val, dir, mask, level, skill, ability;
  register cave_type *c_ptr;
  register monster_type *m_ptr;
  register creature_type *r_ptr;
  char str[80],tval;
  int choice, chance,result;
  register inven_type *i_ptr;
  char tmp;
  if (py.flags.flags[F_SKILLS] && !is_wizard(player_uid))
    {
      msg_print("You can't use your talent yet.");
      return;
    }
  save_screen();
  for(k=1;k<20;k++)
    erase_line(k,30);
  prt("Selecting a Skill",3,30);
  /* Note we reselect ability, since we may have many talents */
  i=0; j=0;
  for(k=0;k<S_NUM;k++)
    {
      if (tnames[k][0] && py.skills.cur_skill[k]>=min[k])
	{
	  ++i;
	  ++j;
	  sprintf(str,"%c) %s",j+96,tnames[k]);
	  prt(str,j+5,30); /* Ignore 'blank' skills */
	}
    }
  if (!i) /* No talents available */
    sprintf(str,"You can't use any talents yet.  Press ESC.");
  else
    sprintf(str,"Select a Talent (a-%c) or press ESC.",i+96);
  prt(str,18,30);
  ability=256;
  while(ability==256)
    {
      tmp=inkey();
      if (tmp>='a' && tmp<='z')
	tmp-=32;
      i=tmp-65;
      if (i<0) i=0;
      ++i;
      k=0;
      if (tmp>97+i)
	{
	  prt("There is no such talent!",1,30);
	  inkey();
	  prt("                        ",1,30);
	  tmp=27;
	}
      if (tmp==27)
	{
	  restore_screen();
	  return; /* Don't use a talent */
	}
      while((i>0) && k<S_NUM)
	{
	  if (tnames[k][0] && py.skills.cur_skill[k]>=min[k])
	    --i;
	  if (i)
	    ++k;
	}
      ability=k; /* k holds the REAL ability we're using */
      if (k>S_NUM)
	ability=256;
      if (ability<S_NUM)
	if (min[ability]>py.skills.cur_skill[ability])
	  {
	    ability=256;
	    prt("You can't use that talent! -more-",1,30);
	    inkey();
	    restore_screen();
	    return;
	  }
    }
  restore_screen();
  if (i==-1)
    {
      restore_screen();
      return;
    }
  level=smod(ability);
  switch(ability)
    {
    case S_ALCHEMY: case S_INFUSION:
      if (ability==S_INFUSION) ability=-ability;
      tmp=1;
      if (dun_level!=0)
	{
	  msg_print("You can only forge while in the town!");
	  tmp=0;
	}
      else /* Ok. Make a cool item.  But make it RANDOM */
	{
	  if (ability<0)
	    {
	      tmp=get2("Make a (W)and or (S)taff?");
	      if (tmp=='w')
		tval=TV_WAND;
	      else if (tmp=='s')
		tval=TV_STAFF;
	      else
		tmp=0;
	    }
	  else
	    {
	      tmp=get2("Make a (S)croll or (P)otion?");
	      if (tmp=='s')
		{ if (randint(2)==1)
		    tval=TV_SCROLL1;
		  else
		    tval=TV_SCROLL2;
		}
	      else if (tmp=='p')
		{
		  if (randint(2)==1)
		    tval=TV_POTION1;
		  else
		    tval=TV_POTION2;
		}
	      else
		tmp=0;
	    }
	  if (tmp)
	    {
	      if (!find_range(TV_COMPONENT, TV_NEVER, &j, &k))
		msg_print("You have no components!");
	      else if (py.flags.confused > 0)
		msg_print("You are too confused.");
	      else if (no_light())
		msg_print("You have no light!");
	      else if (py.flags.blind > 0)
		msg_print("You can't see!");
	      else if (get_item(&item_val, "Use which component?", j, k, 0))
		{
		  j=(inventory[item_val].p1*5)+1;
		  if (ability<0) ability=-ability;
		  k=smod(ability);
		  tmp=(j+k)/2;
		  inven_destroy(item_val);
		  place_general(char_row,char_col,tval,tmp);
		}
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
      detect_general(0,EVIL,"evil");
      skill=100;
      break;
    case S_SLAY_ANIMAL:
      detect_general(0,ANIMAL,"animal");
      skill=75;
      break;
    case S_DISARM:
      detect_trap();
      skill=50;
      break;
    case S_SLAY_UNDEAD:
      if (py.misc.exp < py.misc.max_exp)
	{
	  msg_print("You feel your life force return to your body.");
	  py.misc.exp=py.misc.max_exp;
	}
      else
	msg_print("Nothing happened.");
      skill=200;
      break;
    case S_KARATE:
      (void) remove_fear();
      py.flags.confused=0;
      py.flags.blind=0;
      skill=255;
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
	 tmp=get2("Forge a (S)word, (M)ace, or (P)olearm?");
	 if (tmp=='s')
	    i=TV_SWORD;
	 else if (tmp=='m')
	    i=TV_HAFTED;
	 else if (tmp=='p')
	    i=TV_POLEARM;
	 else
	   tmp=0;
	}
      if (tmp=='a')
	{
	  tmp=get2
	    ("Forge a (H)elmet, (B)oots, (S)hield, (G)auntlets, or (A)rmor?");
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
	    tmp=0;
     	}
      if (tmp=='z')
	{
	  tmp=get2
	    ("Forge (B)ow, (C)rossbow, (A)rrows, or (S)hots?");
	  if (tmp=='b')
	    i=TV_BOW;
	  if (tmp=='c')
	    i=-(TV_BOW);
	  if (tmp=='a')
	    i=TV_ARROW;
	  if (tmp=='s')
	    i=TV_BOLT;
	}
      if (i) /* Now forge it */
	{
	  mask=i;
	  i=0;
	  if (!find_range(TV_COMPONENT, TV_NEVER, &j, &k))
	    msg_print("You have no components!");
	  else if (py.flags.confused > 0)
	    msg_print("You are too confused.");
	  else if (no_light())
	    msg_print("You have no light to forge by!");
	  else if (py.flags.blind > 0)
	    msg_print("You can't see to forge!");
	  else if (get_item(&item_val, "Use which component?", j, k, 0))
	    {
	      i=mask;
	      /* What we do to find the quality of the item is below: */
	      i_ptr = &inventory[item_val];
	      i_ptr->subval+=138; /* Treat them as unique */
	      j = level+2;
	      dir = (j + (i_ptr->p1*10)/6)/2;
	      dir=dir*11/7;
	      k=0; /* Counter for # of special things added */
	      /* Max value for dir = 36 */
	      if (j<randint(dir/2))
		{
		  msg_print("The component didn't work well.");
		  if (i_ptr->p1 >= 2)
		    i_ptr->p1 -= 2;
		  else
		    i_ptr->p1 = 0;
		}
	      else /* Make sure item can be forged, then do it */
		{
		  if (i_ptr->name2 > 0)
		    {
		      msg_print("The component has already been forged.");
		      i=0;
		    }
		  /* Now try to forge the item */
		  if (i)
		    {
		      i_ptr->p1 = 0;
		      i_ptr->tval = i;
		      i_ptr->weight *= 5; /* Whittle some away */
		      i_ptr->weight /= randint(10);
		      switch(i)
			{
			case TV_ARROW: case TV_BOLT:
			  if (i==TV_BOLT)
			    {
			      i_ptr->tohit = randint(dir/7);
			      i_ptr->todam = 2+randint(dir/5);
			      i_ptr->damage[1]=4+(dir/7);
			      i_ptr->weight=i_ptr->weight*2/3;
			      i_ptr->index=80;
			    }
			  else
			    { /* Arrows */
			      i_ptr->tohit = 2+randint(dir/5);
			      i_ptr->todam = randint(dir/6);
			      i_ptr->damage[1]=3+(dir/8);
			      i_ptr->weight/=2; /* Arrows are light */
			      i_ptr->index=78;
			    }
			  i_ptr->damage[0]=1;
			  i_ptr->ident |= ID_SHOW_HITDAM;
			  i_ptr->number=5+randint(dir/2)+dir/4;
			  /* No specials---these are powerful ENOUGH! */
			  break;
			case TV_BOW: case -(TV_BOW):
			  if (i>0) /* Forge Bow */
			    { /* Max=(+13,+10) */
			      i_ptr->tohit = randint(dir/3)+1;
			      i_ptr->todam = randint(dir/4+1);
			      tmp=2; /* Min P1 value */
			      if (dir>6)
				{
				 ++tmp;
				 if (dir>12)
				   ++tmp;
			       }
			      if (tmp==2) i_ptr->index=73;
			      if (tmp==3) i_ptr->index=74;
			      else i_ptr->index=79;
			    }
			  else /* Crossbow */
			    {
			      i_ptr->tohit = randint(dir/4+1);
			      i_ptr->todam = randint(dir/3)+1;
			      tmp=5; /* Max=(+9,+13) */
			      if (dir>7)
				++tmp;
			      if (tmp==5) i_ptr->index=75;
			      else i_ptr->index=76;
			      i_ptr->tval=TV_BOW;
			    }
			  i_ptr->damage[0]=1;
			  i_ptr->damage[1]=1;
			  i_ptr->ident |= ID_SHOW_HITDAM;
			  i_ptr->p1=tmp;
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
			      if (randint(10)>5)
				select_attrib(dir*2,1,TR_FREE_ACT|
					      TR_RES_COLD|TR_RES_ACID|
					      TR_RES_FIRE|TR_DEX|TR_CON|
					      TR_STR|TR_REGEN|TR_SEE_INVIS|
					      TR_SLAY_EVIL|TR_FROST_BRAND|
					      TR_FLAME_TONGUE|
					      TR_SLOW_DIGEST,i_ptr);
			      else if (randint(2)==1)
				select_attrib(dir*2,2,TR_LIGHT|TR_IRONWILL|
					      TR_RES_DISENCHANT|TR_RES_NEXUS|
					      TR_RES_SHARDS|TR_HOLD_LIFE|
					      TR_RES_CONF|TR_RES_BLIND
					      ,i_ptr);
			      else
				{ ++i_ptr->tohit;
				  ++i_ptr->todam;
				}
 			    }
			  break;
			case TV_SWORD:
			  i_ptr->index=34; /* "Broadsword" */
			  i_ptr->tohit = randint(dir/4+1); /* Max=(+10,+8) */
			  i_ptr->todam = randint(dir/5+1);
			  i_ptr->damage[0] = 1+dir/11+randint(dir/19);
			  i_ptr->damage[1] = 2+dir/8+randint(dir/13);
			  i_ptr->weight *= 2; /* Max=5d8 */
			  i_ptr->ident |= ID_SHOW_HITDAM;
			  if (randint(dir*3/2) > 3)
			    {
			      j=randint(dir/5)+2;
			      msg_print("This is a special sword!");
			      for(k=1;k<=j;k++)
				{
				  /* This adds ONE of the listed
				     attributes. */
				  if (randint(10)>2)
				    select_attrib(dir*2,1,TR_SEE_INVIS|
				    TR_STR|TR_CON|TR_DEX|TR_RES_FIRE|
					       TR_RES_COLD|TR_INT|TR_WIS|
					       TR_CHR|TR_RES_LIGHT|TR_POISON|
					       TR_SLAY_EVIL|TR_SLAY_UNDEAD|
					       TR_SLAY_X_DRAGON|TR_SLAY_DRAGON|
					       TR_FLAME_TONGUE|TR_SPEED|
					       TR_FROST_BRAND|TR_SUST_STAT|
					       TR_REGEN,i_ptr);
				  else if (randint(3)!=1)
				    select_attrib(dir*2,2,TR_VORPAL|TR_LIGHT|
					       TR_RES_LIGHT|TR_LIGHTNING|
						  TR_IRONWILL,
					       i_ptr);
				  else
				 {
				   i_ptr->todam += 3;
				   i_ptr->tohit += 3;
				 }
				}
			    }
			  break;
			case TV_POLEARM:
			  i_ptr->name2 = SN_STAFF;
			  i_ptr->tohit =  randint(dir/5+1); /* Max=(+7,+10) */
			  i_ptr->todam = randint(dir/4+1);
			  i_ptr->damage[0] = 1+(dir/13); /* Max=3d14 */
			  i_ptr->damage[1] = 2+dir/5+randint(dir/7);
			  i_ptr->weight = i_ptr->weight * 2/3;
			  i_ptr->ident |= ID_SHOW_HITDAM;
			  if (randint(dir*3/2) > 3)
			    {
			      msg_print("This is a special polearm!");
			      j=randint(dir/4)+3;
			      for(k=1;k<=j;k++)
				{
				  if (randint(10)<4)
				    select_attrib(dir*2,1,
						  TR_STR|TR_INT|TR_WIS|TR_DEX|
						  TR_CON|TR_CHR|TR_RES_ACID|
						  TR_RES_FIRE|TR_SLAY_UNDEAD|
						  TR_SLAY_X_DRAGON|
						  TR_SLAY_ANIMAL|
						  TR_SLAY_EVIL|TR_INFRA|
						  TR_FREE_ACT|TR_SEE_INVIS|
						  TR_POISON|
						  TR_SUST_STAT,i_ptr);
				  else if (randint(10)<6)
				    select_attrib(dir*2,2,
						  TR_SLAY_ORC|TR_SLAY_TROLL|
						  TR_SLAY_GIANT|TR_LIGHT|
						  TR_TELEPATHY|TR_RES_CHAOS|
						  TR_RES_BLIND|TR_RES_CONF|
						  TR_RES_LIGHT|TR_RES_DARK|
						  TR_RES_DISENCHANT|
						  TR_RES_NEXUS|
						  TR_HOLD_LIFE|TR_RES_NETHER,
						  i_ptr);
				  else
				    { i_ptr->tohit+=2; i_ptr->todam+=2; }
				}
			    }
			  break;
			case TV_HAFTED:
			  i_ptr->name2 = SN_MACE;
			  i_ptr->tohit = 1+randint(dir/5); /* Max=(+8,+8) */
			  i_ptr->todam = 1+randint(dir/5);
			  i_ptr->damage[0] = 2+dir/11+randint(dir/19);
			  i_ptr->damage[1] = 3+dir/11+randint(dir/11);
			  i_ptr->weight = i_ptr->weight*5/2;
			  i_ptr->ident |= ID_SHOW_HITDAM; /* Max=6d9 */
			  if (randint(dir*3/2) > 3)
			    {
			      msg_print("This is a special mace!");
			      j=randint(dir/7)+2;
			      for(k=1;k<=j;k++)
				{
				  if (randint(10)<8)
				    select_attrib(dir*2,1,
					       TR_SEE_INVIS|TR_FREE_ACT|
					       TR_SPEED|TR_STR|TR_INT|TR_WIS|
					       TR_DEX|TR_CON|TR_CHR|
					       TR_TUNNEL|TR_RES_LIGHT|
					       TR_RES_FIRE|TR_RES_COLD|
					       TR_SLAY_X_DRAGON|TR_POISON,
					       i_ptr);
				  else if (randint(3)<=2)
				    select_attrib(dir*2,2,
					       TR_SLAY_ORC|TR_SLAY_TROLL|
					       TR_SLAY_DEMON|TR_SLAY_GIANT|
					       TR_TELEPATHY|TR_RES_LIGHT|
					       TR_RES_CONF|TR_RES_BLIND|
					       TR_LIGHT|TR_IRONWILL
						  ,i_ptr);
				  else
				    {
				      i_ptr->tohit+=2;
				      i_ptr->todam+=2;
				    }
				}
			    }
			  if (i_ptr->flags & TR_SPEED)
			    {
			      if (randint(100)==1)
				i_ptr->p1 = 2;
			      else
				i_ptr->p1 = 1;
			    }
			  break;
			case TV_HELM:
			  i_ptr->name2 = SN_HELM;
			  i_ptr->ac = 1+dir/7; /* Max=[6,+3] */
			  i_ptr->toac = 1+randint(dir/11);
			  if (randint(dir*3/2) > 3)
			    {
			      msg_print("This is a special helm!");
			      j=randint(dir/8)+1;
			      for(k=1;k<=j;k++)
				{
				  if (randint(10)<6)
				    select_attrib(dir*2,1,TR_SEE_INVIS|
					       TR_STR|TR_INT|TR_WIS|TR_DEX|
					       TR_CON|TR_CHR|TR_RES_ACID|
					       TR_RES_FIRE|TR_RES_COLD|
					       TR_RES_LIGHT|TR_INFRA,
					       i_ptr);
				  else if (randint(4)<=3)
				    select_attrib(dir*2,2,TR_TELEPATHY|
					       TR_LIGHT|TR_RES_BLIND|
					       TR_RES_CONF|TR_RES_SOUND|
						  TR_IM_POISON,
					       i_ptr);
				  else
				    i_ptr-> toac += 5;
				}
			    }
			  break;
			case TV_HARD_ARMOR:
			  i_ptr->name2 = SN_ARMOR;
			  i_ptr->ac=3+dir+randint(dir/6); /* Max=[43,+10] */
			  i_ptr->toac=1+(dir/8)+randint(dir/7);
			  i_ptr->weight*=2;
			  i_ptr->tohit=-(i_ptr->ac/7);
			  i_ptr->tohit+=dir/5;
			  if (i_ptr->tohit>0)
			    i_ptr->tohit=0;
			  if (randint(dir*3/2) > 3)
			    {
			      msg_print("This is special armor!");
			      j=randint(dir/2)+2;
			      for(k=1;k<=j;k++)
				{
				  if (randint(10)<5)
				    select_attrib(dir*2,1,TR_RES_ACID|
						  TR_RES_FIRE|TR_RES_COLD|
						  TR_RES_LIGHT|
					       TR_POISON|TR_SLOW_DIGEST|
					       TR_REGEN,i_ptr);
				  else if (randint(10)<8)
				    select_attrib(dir*2,2,TR_RES_DISENCHANT|
					       TR_RES_NEXUS|TR_RES_SHARDS|
					       TR_RES_CONF|TR_RES_BLIND|
					       TR_HOLD_LIFE|TR_RES_DARK|
					       TR_RES_CHAOS|TR_RES_LIGHT|
						  TR_IM_FIRE|TR_IM_COLD|
						  TR_IM_LIGHT|TR_IRONWILL,
					       i_ptr);
				  else
				    i_ptr->toac += 8;
				}
			    }
			  break;
			case TV_BOOTS:
			  i_ptr->name2 = SN_BOOTS;
			  i_ptr->ac=1+dir/11+randint(dir/19); /* Max=[5,+5] */
			  i_ptr->toac=1+(dir/13)+randint(dir/14);
			  if (randint(dir*3/2) > 6)
			    {
			      msg_print("You made excellent boots!");
			      j=randint(dir/9)+1;
			      for(k=1;k<=j;k++)
				{
				  if (randint(10)<8)
				    select_attrib(dir*2,1,TR_STEALTH|TR_SPEED|
					       TR_FREE_ACT|TR_FFALL|TR_REGEN|
					       TR_RES_ACID,
					       i_ptr);
				  else if (randint(6)<4)
				    select_attrib(dir*2,1,TR_RES_NEXUS,i_ptr);
				  else
				    i_ptr->toac += 3;
				}
			    }
			  if (i_ptr->flags & TR_SPEED)
			    {
			      if (randint(100)==1)
				i_ptr->p1 = 2;
			      else
				i_ptr->p1 = 1;
			    }
			  break;
			case TV_GLOVES:
			  i_ptr->name2 = SN_GLOVES;
			  i_ptr->ac=1+dir/13; /* Max=[3,+6] */
			  i_ptr->toac=1+(dir/12)+randint(dir/13);
			  i_ptr->weight=i_ptr->weight*2/3;
			  if (randint(dir*3/2) > 4)
			    {
			      msg_print("You made excellent gloves!");
			      j=randint(dir/5)+2;
			      for(k=1;k<=j;k++)
				{
				  if (randint(10)<8)
				    select_attrib(dir*2,1,TR_RES_FIRE|
						  TR_RES_COLD|TR_RES_ACID|
						  TR_RES_LIGHT|TR_DEX|TR_STR|
						  TR_SUST_STAT|TR_FREE_ACT|
						  TR_POISON,i_ptr);
				  else if (randint(10)<3)
				    select_attrib(dir*2,1,TR_RES_CONF|
						  TR_IM_FIRE|TR_IM_COLD,
						  i_ptr);
				  else
				    {
				      i_ptr->tohit+=2+randint(dir/5);
				      i_ptr->todam+=2+randint(dir/5);
				      ++i_ptr->toac;
				    }
				}
			    }
			      break;
			case TV_SHIELD:
			  i_ptr->name2 = SN_SHIELD;
			  i_ptr->ac=2+dir/10+randint(dir/13); /* Max=[7,+7] */
			  i_ptr->toac=1+(dir/15)+randint(dir/12);
			  i_ptr->weight=i_ptr->weight*3/2;
			  if (randint(dir*3/2) > 5)
			    {
			      msg_print("You made an excellent shield!");
			      j=randint(dir/6)+1;
			      for(k=1;k<=j;k++)
				{
				  if (randint(10)<5)
				    select_attrib(dir*2,1,TR_RES_FIRE|
						  TR_RES_COLD|TR_RES_ACID|
						  TR_RES_LIGHT|
					       TR_POISON|TR_FREE_ACT,i_ptr);
				  else if (randint(10)<8)
				    select_attrib(dir*2,1,TR_RES_SHARDS|
					       TR_RES_CONF|TR_RES_BLIND|
					       TR_RES_NEXUS|TR_RES_DISENCHANT|
					       TR_RES_SOUND|TR_RES_CHAOS|
					       TR_IM_LIGHT|TR_IM_COLD,i_ptr);
				  else
				    i_ptr->toac += 2;
				}
			    }
			  if (tmp) skill=255;
			  break;
		        default:
			  break;
			}
		    }
		}
	    }
	}
      /* Evaluate cost of the item */
      j = 0; /* Current cost */
      switch(i)
	{
	case TV_ARROW: case TV_BOLT:
	  j = 1;
	  break;
	case TV_BOW:
	  j = 300;
	  break;
	case TV_SWORD:
	  j = 100;
	 break;
	case TV_HAFTED:
	  j = 150;
	  break;
	case TV_HELM:
	  j = 50;
	  break;
	case TV_HARD_ARMOR:
	  j = 100;
	  break;
	case TV_BOOTS:
	  j = 50;
	  break;
	case TV_SHIELD:
	  j = 100;
	  break;
	}
      if (i)
	{
      j += 200 * i_ptr->tohit;
      j += 200 * i_ptr->todam;
      j += 100 * i_ptr->ac;
      j += 150 * i_ptr->toac;
      j += 300 * i_ptr->p1;
      mask=1;
      /* This adds value for each special added */
      for(k=1;k<33;k++)
	{
	  if (i_ptr->flags & mask)
	    {
	      j += levels[k-1]*100;
	    }
	  if (i_ptr->flags2 & mask)
	    {
	      j += levels[k-1]*120;
	    }
	  mask = mask << 1;
	}
      i_ptr->cost = j;
    }
      break;
    }
  py.flags.flags[F_SKILLS]=skill;
}


char get2(message)
char *message;
{
 char t;
 msg_print(message);
 t=inkey();
 if (t>='A' && t<='Z')
   t+=32;
 return t;
}

/* Tries to add ONE unique attribute to the passed inventory item */
void select_attrib(lvl,which,flags,i_ptr)
int lvl;
int which;
long flags;
inven_type *i_ptr;
{
 long find;
 long mask;
 int count;
 int i,j,k,p,l2;
 count=2+(lvl/10);
 if (which==1)
   find=i_ptr->flags;
 else
   find=i_ptr->flags2;
 /* First, go through the passed flags.  Then pick one.  If the attribute
    passes, then we can stop.  Otherwise, go on.  If end of loop reached
    without a flag selected, repeat */
 k=0;
 for(i=1;i<count && !k;i++)  
   {
     mask=1;
     /* Scan the specific bits */
     for(j=1;j<33 && !k;j++)
       {
	/* Add a UNIQUE attribute---it's not on the item already */
	 if (!(find & mask) && (flags & mask))
	   {
	     l2=levels[(j-1)+((which-1)*32)];
	     p=MAXP;
	     if (lvl<l2)
	       p-=(LOW*(l2-lvl));
	     else if (lvl>l2)
	       p-=(HIGH*(lvl-l2));
	     if (p<2 && lvl<l2)
	       p=2; /* Hard to make things ya don't know how---but doable */
	     if (p<5 && lvl>l2)
	       p=5; /* More likely to make weaker ones */
	     if (randint(100)<=p)
		 k=j;
	   }
	 if (!k)
	   mask=mask << 1;
       }
   }
 if (k) /* Don't add 'null' attribute */
   {
     if (which==1)
       i_ptr->flags|=1 << (k-1);
     else
       i_ptr->flags2|=1 << (k-1);
     mask=1<<k;
     /* Here, fix P1 for the flag */
     if (i_ptr->tval==TV_BOW)
       return; /* Do NOT touch P1 */
     if (which==1)
       {
       if (mask==TR_STR || mask==TR_INT || mask==TR_WIS || mask==TR_DEX ||
	   mask==TR_CON || mask==TR_SUST_STAT || mask==TR_TUNNEL)
	 i_ptr->p1=1+randint(lvl/15);
       else if (mask==TR_SPEED)
	 i_ptr->p1=1+randint(lvl/50);
       else if (mask==TR_INFRA || mask==TR_STEALTH)
	 i_ptr->p1=2+randint(lvl/20);
     }
   }
}
