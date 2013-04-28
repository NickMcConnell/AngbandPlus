/* skills.c:  ALL the routines for handling skills are in here */
#include <curses.h>
#include "constant.h"
#include "config.h"
#include "types.h"
#include "externs.h"
#include "monster.h"

char *snames[S_NUM]={"Swordsmanship","Clubbing","Jousting","Endurance",
"Archery","Magical Devices","Spellcasting","Spell Resistance","Disarming",
"Backstabbing","Sneaking","Magical Power","Dual Wield","Dodging","Karate",
"Weaponsmithing","Armor Forging","Perception","Morality","Hunting",
"Vampire Hunting","Precognition","Wrestling","Bowmaking","Alchemy",
"Magical Infusion","","","",""};

char *shelp[S_NUM]=
{"fight with swords",
 "fight with maces",
 "fight with polearms",
 "take more damage",
 "shoot a Bow+Arrow",
 "use a Magic Device",
 "learn new spells",
 "resist spells cast at you",
 "disarm traps",
 "badly wound sleeping creatures",
 "move quietly through the dungeon",
 "cast more spells",
 "fight with 2 weapons at once",
 "protect yourself from attacks",
 "kick and punch your opponents",
 "forge custom-made weapons",
 "forge custom-made armor",
 "find special areas and items",
 "fight evil creatures well",
 "fight natural creatures well",
 "fight undead creatures well",
 "sense powerful monsters",
 "grapple your opponent",
 "make bows and arrows",
 "create scrolls and potions",
 "create wands and staves",
 "","",""};

/* This is necessary so we can have a "logical" progression between the
virtual levels---so early levels go quickly, and later levels slowly.  One
needs this many advances to go BEYOND this level; 2 advances get the player
past Level 1, 2 MORE advances get player past level 2, and so on */

unsigned num_adv[MAX_PLAYER_LEVEL+1]=
{4,4,4,5,5,5,6,6,6,7,
 7,7,8,8,8,9,9,9,10,10,
 10,11,11,11,12,12,12,13,13,13,
 14,14,15,15,16,16,17,17,18,18,
 19,19,20,20,21,22,23,24,25,26,65535};
/* The last prevents players from advancing to level 51 */

/*  This returns a "logical" value for ALL NON FIGHTING SKILLS.  Fighting
skills handled in 3 other routines---one for +todam, one for +tohit, and
one for # of blows/round modifier
   For all magical skills, this returns an appropiate "level" of the caster,
for determining spell failure, mana, etc.
   For weapon/armorsmithing, this returns an effective level of the user---
if 0, cannot use the ability.
   For backstabbing, this returns an effective level (0 means no backstab).
   For Slay Evil/Animal/etc, this returns a modifier to the +todam.  Twice
that is the AC bonus.  Is NEVER below 0.
   IF you pass it a value of 255, it returns the player's "aggregate"
level---sort of the level the PC seems to be at in general */

int smod(skill)
int skill;
{
 int tmp;
 if (skill>=S_NUM)
   return 0;
 tmp=py.skills.cur_skill[skill];
 switch(skill)
   {
   case S_DEVICE: /* Magic Devices hard to use---scale is 4 points/lvl, 1
		   if below 30 points. */
     if (tmp<30) return 1;
     return ((tmp-30)/4)+1;
     break;
   case S_ENDURANCE:
     if (tmp>=50)
       return ((tmp-50)/20);
     return ((tmp-50)/12);
     break;
   case S_SAVE: /* Should be fairly easy.  We return the Save/2 */
     if (tmp<20) return 0;
     return ((tmp-20)/2);
     break;
   case S_MAGIC: /* Should be HARD.  We have 5 points/lvl.  Spellcasting. None
		  if below 40 points. */
     if (tmp<40) return 0;
     return ((tmp-40)/4)+1;
     break;
   case S_MPOWER: /* Is HARD.  4 points/lvl.  Determines Mana. */
     if (tmp<35) return 0;
     return ((tmp-35)/4)+1;
     break;
   case S_STEALTH:
     return (tmp/20);
     break;
   case S_BACKSTAB:
     if (tmp<80) return 0;
     return ((tmp-80)/33)+1;
     break;
   case S_DODGING:
     if (tmp<30) return 0;
     tmp=(tmp-25)/2; /* POSSIBLE AC mod---if wearing nothing */
     tmp-=py.misc.pac*2;
     if (tmp<0) return 0;
     return tmp;
     break;
   case S_WEAPON:
   case S_ARMOR:
     if (tmp<40) return 0;
     return ((tmp-40)/9)+1;
     break;
   case S_BOWMAKE:
     if (tmp<80) return 0;
     return ((tmp-80)/8)+1;
     break;
   case S_SLAY_EVIL:
     if (tmp<40) return 0;
     return ((tmp-40)/16)+1;
     break;
   case S_PERCEPTION:
     if (tmp<50) return ((tmp-50)/3);
     return ((50-tmp)/15);
     break;
   case S_SLAY_ANIMAL:
     if (tmp<25) return 0;
     return ((tmp-25)/12)+1;
     break;
   case S_DISARM:
     if (tmp<30) return ((tmp-30)/2)-2;
     return((tmp-30)/2)+5;
     break;
   case S_INFUSION:
     if (tmp<120) return 0; /* Means we can't do it */
     return((tmp-110)/2)+1;
     break;
   case S_ALCHEMY:
     if (tmp<100) return 0;
     return ((tmp-90)/2)-2;
     break;
   case 255:
     break;
   default:
     return tmp;
     break;
   }
}

/* Routine to determine todam modifier granted by the passed fighting skill */
/* We don't NEED one for the tohit, since it's already determined by the
   appropiate fighting skill */
int stodam(skill)
int skill;
{
 int tmp;
 tmp=py.skills.cur_skill[skill];
 switch(skill)
   {
   case S_KARATE: /* Up to +10 to damage each blow */
     if (tmp<40) return 0; /* Can't do anything, takes a lot of skill */
     return ((tmp-50)/19);
     break;
   case S_WRESTLING: /* Damage is not modified */
     return 0;
     break;
   case S_SWORD: case S_HAFTED: case S_POLEARM: case S_2HANDED:
     if (tmp<60)
       return (tmp-60)/3; /* Hefty negative here */
     return (tmp-60)/30;
     break;
   case S_ARCHERY: /* This advances VERY slowly */
     if (tmp<80)
       return (tmp-80)/6; /* Even worse, since Bows are very specialized */
     return (tmp-80)/33;
     break;
   default:
     return 0;
     break;
   }
}

/* Returns a MODIFIER to the # of blows/round gained with said weapon.  For
   Bows, we may fire more than 1 arrow/round.  Note, we do NOT drop below
   1 blow/round */
int sblows(skill)
int skill;
{
  int tmp;
  tmp=py.skills.cur_skill[skill];
  switch(skill)
    {
    case S_KARATE: /* Should be a HUGE modifier to # of blows/round */
      if (tmp<50)
	return (tmp-50)/15;
      return (tmp-50)/35;
      break;
    case S_WRESTLING:
      return -9; /* Only ONE blow/round! */
      break;
    case S_SWORD: case S_HAFTED: case S_POLEARM: case S_2HANDED:
      if (tmp<60)
	return (tmp-60)/10;
      return (tmp-60)/40;
      break;
    case S_ARCHERY:
      if (tmp<100)
	return (tmp-100)/10;
      return (tmp-100)/20;
      break;
    default:
      return tmp;
      break;
    }
}

/* This will determine a "general level" based on the various skills you
have.  Useful for determining extra HP and so on */

int get_level()
{
 register int loop,sum,lvl;
 sum=0;
 lvl=0;
 for(loop=0;loop<S_NUM;loop++)
   sum+=py.skills.adv_skill[loop];
 /* Now we search the "num_adv" array and find the player's REAL level */
 for(loop=0;loop<MAX_PLAYER_LEVEL && sum>0;loop++,sum-=num_adv[loop],++lvl);
 if (lvl>MAX_PLAYER_LEVEL) lvl=MAX_PLAYER_LEVEL;
 if (lvl<1) lvl=1;
 return lvl;
}

/* This determines the amount of XP required to advance a skill.  NOTE: A
skill WILL NOT always advance by 1 point---if a race is good at something, it
may go up 5 or 6 points when advanced. */
long get_xp(sk)
int sk;
{
 register int mult; /* Used to make XP required advance very quickly */
 int loop,total;
 long cur;
 total=0;
 for(loop=0;loop<S_NUM;loop++)
   total+=py.skills.adv_skill[loop];
 mult=(total/45)+2;
 total*=mult/3;
 cur=2+total; /* Thus we always need AT LEAST 2 XP to advance a skill */
 cur+=py.skills.adv_skill[sk]*mult;
 if (py.skills.max_skill[sk]>py.skills.cur_skill[sk])
   cur=(cur/3)+1; /* Advance much more quickly if skill drained */
 return cur;
}

/* These will change most skills so they fit the new values.  
First needs the OLD VALUE of the skill, then the skill identifier, and
finally a 0 if never called before, and a 1 otherwise. */
void smodperm(skill,which,t)
int skill,which,t;
{
 int old;
 old=py.skills.cur_skill[which];
 if (t) /* We must undo the old value */
   {
     py.skills.cur_skill[which]=skill;
     switch(which)
       {
       case S_STEALTH:
	 py.misc.stl-=smod(S_STEALTH);
	 break;
       case S_PERCEPTION:
	 py.misc.fos+=smod(S_PERCEPTION);
	 break;
       default: /* Do nothing to non-permanent skills */
	 break;
       }
   }
 /* Now change to the new value */
 py.skills.cur_skill[which]=old;
 switch(which)
   {
   case S_STEALTH:
     py.misc.stl+=smod(S_STEALTH);
     break;
   case S_PERCEPTION:
     py.misc.fos-=smod(S_PERCEPTION);
     break;
   default: /* Do nothing to non-permanent skills */
     break;
   }
}


/* This returns a TOTAL todamage modifier (including weapon skill AND other
'specials'.  Needs the cdefense flag. */
int smoddam(defense,spells3)
long defense,spells3;
{
 register int todam,s,t,k;
 s=sweapon();
 t=inventory[INVEN_WIELD].tval;
 todam=stodam(s); /* Get base todamage for the current weapon skill */
 k=0;
 if ((defense & EVIL) && py.skills.cur_skill[S_SLAY_EVIL]>=30)
     todam+=(py.skills.cur_skill[S_SLAY_EVIL]-20)/10;
 if ((defense & ANIMAL) && py.skills.cur_skill[S_SLAY_ANIMAL]>=30)
     todam+=(py.skills.cur_skill[S_SLAY_ANIMAL]-20)/5;
 if ((defense & UNDEAD) && py.skills.cur_skill[S_SLAY_UNDEAD]>=30)
     todam+=(py.skills.cur_skill[S_SLAY_UNDEAD]-20)/5;
 if (((spells3 & R_EDGED) && s==S_SWORD) ||
     ((spells3 & R_BLUNT) && s!=S_SWORD))
   k=-10000; /* Means we resisted the attack */
 else if (((defense & I_EDGED) && s==S_SWORD) ||
	  ((defense & I_BLUNT) && (s==S_HAFTED || s==S_POLEARM)))
   k=-4000; /* IMMUNE to the attack */
 return todam+k;
}

/* This returns an AC modifier for any and all skills OTHER THAN dodging (that
is innate AC bonus) */
int stoac(defense)
long defense;
{
 int toac;
 toac=0; /* All other skills accounted for */
 if ((defense & EVIL) && py.skills.cur_skill[S_SLAY_EVIL]>=50)
   toac+=(py.skills.cur_skill[S_SLAY_EVIL]-10)/4;
 if ((defense & ANIMAL) && py.skills.cur_skill[S_SLAY_ANIMAL]>=40)
   toac+=(py.skills.cur_skill[S_SLAY_EVIL]-10)/3;
 if ((defense & UNDEAD) && py.skills.cur_skill[S_SLAY_UNDEAD]>=40)
   toac+=(py.skills.cur_skill[S_SLAY_EVIL]-10)/3;
 return toac;
}

/* This returns which weapon skill we're using */
int sweapon()
{
 int sk;
 inven_type *i_ptr;
 i_ptr=&inventory[INVEN_WIELD];
 if (i_ptr->tval == TV_SWORD)
   sk=S_SWORD;
 else if (i_ptr->tval == TV_BOW)
   sk=S_ARCHERY;
 else if (i_ptr->tval == TV_HAFTED)
   sk=S_HAFTED;
 else if (i_ptr->tval == TV_POLEARM)
   sk=S_POLEARM;
 else 
   sk=py.flags.flags[F_BAREHAND];
 if (i_ptr->tval!=TV_NOTHING)
   {
     i_ptr=&inventory[INVEN_ARM];
     if (i_ptr->tval!=TV_SHIELD && i_ptr->tval!=TV_NOTHING)
       sk=S_2HANDED;
   }
 return sk;
}


/* This will print out skills onscreen (separate routine to a file) */

void prt_skills(title,me)
char *title;
FILE *me;
{
 int i,j,t;
 char out[40],rank[20],filestr[80];
 save_screen();
 t=0;
 for(j=1;j<=23;j++)
   erase_line(j, 5);
 t=40-strlen(title)/2;
 if (!me)
   prt(title,1,t);
 else
   {
     for(j=0;j<79;j++)
       filestr[j]=' ';
     filestr[79]=0; /* Make a 'blank' string */
     filestr[t]=0;
     fprintf(me,"%c\n\n%s%s\n\n",12,filestr,title);
     filestr[t]=' '; /* Keep it for later */
   }
 for(t=0;t<S_NUM-2;t++)
   { /* Leave last line free for Spell Info */
     i=(t%2)*30+15;
     j=t/2+3;
     switch(py.skills.cur_skill[t]/15)
       {
       case 0:
	 strcpy(rank,"Awful");
	 break;
       case 1:
	 strcpy(rank,"Poor");
	 break;
       case 2: case 3:
	 strcpy(rank,"Fair");
	 break;
       case 4: case 5:
	 strcpy(rank,"Good");
	 break;
       case 6: case 7:
	 strcpy(rank,"Very Good");
	 break;
       case 8:
	 strcpy(rank,"Excellent");
	 break;
       case 9:
	 strcpy(rank,"Superb");
	 break;
       case 10:
	 strcpy(rank,"Powerful");
	 break;
       case 11:
	 strcpy(rank,"Incredible");
	 break;
       case 12:
	 strcpy(rank,"Transhuman");
	 break;
       case 13:
	 strcpy(rank,"Legendary");
	 break;
       default:
	 strcpy(rank,"Ungodly");
	 break;
       }
     if (py.skills.cur_skill[t]>=py.skills.max_skill[t])
       sprintf(out,"%s: %s",snames[t],rank);
     else /* Uh oh.  Skill drained! */
       sprintf(out,"%s:(%s)",snames[t],rank);
     if (snames[t][0] && !me) /* Don't print unimplimented skills */
       prt(out,j,i);
     else if (snames[t][0]) /* Print to file */
       {
	 if (t%2) /* Advance a line */
	   fprintf(me,"%s\n",out);
	 else
	   {
	     filestr[40-strlen(out)]=0;
	     fprintf(me,"%s%s",out,filestr);
	     filestr[40-strlen(out)]=' ';
	   }
       }
   }
 if (me)
   fprintf(me,"\n\n"); /* Nicely formatted */
 switch(py.misc.realm)
   {
   case NONE:
     sprintf(out,"You are not familiar with any magical art.");
     break;
   case MAGE:
     sprintf(out,"You know the Magic arts.");
     break;
   case PRIEST:
     sprintf(out,"You are a pious character.");
     break;
   case DRUID:
     sprintf(out,"You feel in harmony with the world.");
     break;
   case NECROS:
     sprintf(out,"You understand the forces of life and death.");
     break;
   }
 if (!me)
   prt(out,20,15);
 else
   fprintf(me,"%s\n%c\n",out,12); /* The last part prints out a ^L */
}

/* This allows PC to advance a skill */
void sadvance()
{
 int i,j,stay,k,di,dj;
 char key;
 char out[70];
 sprintf(out,"** Skill Advancement:  Average Level %d **",get_level());
 prt_skills(out,(FILE *)0);
 prt("Directions:  k      <Spacebar> or <Return> to advance the skill.",21,15);
 prt("            h l     Information about skill is in top line.",22,15);
 prt("             j      Press ESC to exit, or ? for skill info.",23,15);
 i=py.flags.flags[F_LASTADV]%2;
 j=py.flags.flags[F_LASTADV]/2;
 mvaddstr(j+3,i*30+11,"-->");
 sprintf(out,"Advancements: %d, XP Needed: %ld       ",
	 py.skills.adv_skill[j*2+i],get_xp(j*2+i));
 mvaddstr(0,10,out);
 stay=1;
 while(stay)
   {
     key=inkey();
     if (key>='A' && key<='Z')
       key-=32;
     di=0;
     dj=0;
     switch(key)
       {
       case '?':
	 k=j*2+i;
	 sprintf(out,"%s will help you to %s.,
             ",snames[k],shelp[k]);
	 mvaddstr(0,10,out);
	 break;
       case 'h':
	 di=-1;
	 break;
       case 'l':
	 di=1;
	 break;
       case 'j':
	 dj=1;
	 break;
       case 'k':
	 dj=-1;
	 break;
       case ' ': case 13: /* Return pressed or Spacebar */
	 (void) advance(j*2+i,1);
	 k=j*2+i;
	 restore_screen();
	 sprintf(out,"** Skill Advancement:  Average Level %d **                 ",
		 get_level());
	 prt_skills(out,(FILE *)0);
	 prt("Directions:  k      <Spacebar> or <Return> to advance the skill.",21,15);
	 prt("            h l     Information about skill is in top line.",22,15);
	 prt("             j      Press ESC to exit.",23,15);
	 i=py.flags.flags[F_LASTADV]%2;
	 j=py.flags.flags[F_LASTADV]/2;
	 mvaddstr(j+3,i*30+11,"***");
	 mvaddstr(j+3,i*30+11,"-->");
	 sprintf(out,"Advancements: %d, XP Needed: %ld       ",
		 py.skills.adv_skill[k],get_xp(k));
	 mvaddstr(0,10,out);
	 break;
       case 27: /* ESC */
	 stay=0;
	 break;
       }
     if (key=='h' || key=='j' || key=='k' || key=='l')
       {
	 mvaddstr(j+3,i*30+11,"   ");
	 i+=di; j+=dj;
	 if (i<0) i=1;
	 if (i>1) i=0;
	 if (j<0) j=12;
	 if (j>12) j=0;
	 k=j*2+i;
	 sprintf(out,"Advancements: %d, XP Needed: %ld       ",
		 py.skills.adv_skill[k],get_xp(k));
	 mvaddstr(j+3,i*30+11,"-->");
	 mvaddstr(0,10,out);
       }
   }
 restore_screen();
 prt_experience();
 prt_level();
 prt_title();
}

/* This attempts to advance the given skill.  It will return 0 if failed, 1 if
successful.  Also, pass a 1 for dir to advance the skill, and a - to weaken
it by that many iterations.  The amount of experience is only relevant when
INCREASING a skill. */
int advance(which,d2)
int which,d2;
{
 int i,j,next,old,loop,do_dec,dir,restore;
 char key;
 char out[80];
 long tmp;
 do_dec=0; /* If 1, we decrease skill */
 restore=0; /* If 1, we are restoring skill to normal */
 if (d2<0) { 
   dir=-d2;
   do_dec=1; }
 else
   dir=d2;
 if (dir==255) restore=1;
 for(loop=0;loop<dir;loop++)
  {
    tmp=get_xp(which);
    if (py.skills.cur_skill[which]==255)
      {
	msg_print("That skill is already at its limit!");
	return 0;
      }
    if (py.misc.exp<tmp && !do_dec && !restore) 
      {
	mvaddstr(1,13,
		 "You don't have enough experience to advance that skill! -more-");
	inkey();
	erase_line(2,13);
	sprintf(out,"** Skill Advancement:  Average Level %d **",get_level());
	i=40-strlen(out)/2;
	prt("       ",1,i-7);
     prt(out,1,i);
	return 0;
      }
    if (!race[py.misc.prace].start[which])
      return 0; /* Ignore non-working skills */
    old=py.skills.cur_skill[which];
    next=py.skills.cur_skill[which];
    if ((which==S_MAGIC || which==S_MPOWER) && (!py.skills.adv_skill[S_MAGIC]
						&& !py.skills.adv_skill[S_MPOWER]) && !do_dec)
      {
	msg_print("Choose a Realm:  (S)orcery, (P)iety, (D)ruid, or (N)ecromacy");
	key=inkey();
	j=0;
	switch(key)
	  {
	  case 's': case 'S':
	    py.misc.realm=MAGE;
	    strcpy(out,"You are learning Sorcery!");
	    j=1;
	    break;
	  case 'p': case 'P':
	    py.misc.realm=PRIEST;
	    strcpy(out,"You are learning the ways of God!");
	    j=1;
	    break;
	  case 'd': case 'D':
	    py.misc.realm=DRUID;
	    strcpy(out,"You are learning Nature magic!");
	    j=1;
	    break;
	  case 'n': case 'N':
	    py.misc.realm=NECROS;
	    strcpy(out,"You are learning Dark Sorcery!");
	    j=1;
	    break;
	  default:
	    strcpy(out,
		   "You need to pick a Realm before advancing magical skills!");
	    break;
	  }
	strcat(out," -more-");
	prt(out,0,0);
	inkey();
	prt("                                   ",0,0);
	if (!j) return j; /* Otherwise, advance it */
      }
    i=race[py.misc.prace].skills[which]; /* Starting advance value */
    if (!do_dec && !restore)
      py.flags.flags[F_LASTADV]=which; /* Tells us the last-advanced skill */
    j=py.skills.adv_skill[which];
    if (j<5)
      i-=0;
    else if (j<9)
      --i;
    else if (j<16)
      i-=2;
    else if (j<27)
      i-=3;
    else if (j<42)
      i-=4;
    else if (j<69)
      i-=5;
    else
      i-=6;
    if (i<2) i=2; /* Never stop, just slow way down */
    if (do_dec && py.skills.adv_skill[which]>=1)
      py.skills.cur_skill[which]-=i;
    else
      py.skills.cur_skill[which]+=i;
    if (!do_dec && !restore) {
      py.misc.exp-=tmp;
      py.misc.max_exp-=tmp;
    }
    calc_hitpoints();
    prt_level();
    if (py.misc.realm!=NONE)
      {
	calc_mana(prime_stat[py.misc.realm]);
	calc_spells(prime_stat[py.misc.realm]);
      }
    if (do_dec && py.skills.adv_skill[which]>=1)
      --py.skills.adv_skill[which];
    else
      ++py.skills.adv_skill[which];
    if (py.skills.adv_skill[which]==255)
      py.skills.adv_skill[which]=0; /* More wraparound */
    if (py.skills.cur_skill[which]<py.skills.min_skill[which])
      py.skills.cur_skill[which]=py.skills.min_skill[which];
    if (py.skills.cur_skill[which]<old && !do_dec)
      py.skills.cur_skill[which]=old; /* Wraparound */
    if (py.skills.cur_skill[which]>=py.skills.max_skill[which])
      py.skills.max_skill[which]=py.skills.cur_skill[which];
    if (restore && py.skills.cur_skill[which]==py.skills.max_skill[which])
      break; /* Get out of the loop now */
  }
 return 1;
}
