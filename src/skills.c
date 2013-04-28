	/* skills.c:  ALL the routines for handling skills are in here */
#include "angband.h"

cptr snames[S_NUM]={"Swordsmanship","Clubbing","Jousting","Endurance",
"Archery","Magical Devices","Spellcasting","Spell Resistance","Disarming",
"Backstabbing","Sneaking","Magical Power","Dual Wield","Dodging","Karate",
"Weaponsmithing","Armor Forging","Perception","Morality","Hunting",
"Vampire Hunting","Precognition","Wrestling","Bowmaking","Alchemy",
"Magical Infusion","","","",""};

cptr shelp[S_NUM]=
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
	"fight with two weapons at once",
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

unsigned num_adv[PY_MAX_LEVEL+1]=
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

int smod(int skill)
{
	int tmp;
	if (skill>=S_NUM)
		return 0;
	tmp=p_ptr->cur_skill[skill];
	switch(skill)
	{
		case S_DEVICE: /* Magic Devices hard to use---scale is 2 points/lvl, 1
				  if below 30 points. */
		if (tmp<30) return 1;
		return ((tmp-30)/2)+1;
		break;
		case S_ENDURANCE:
		if (tmp>=50)
			return ((tmp-50)/22);
		return ((tmp-50)/12);
		break;
		case S_SAVE: /* Should be fairly easy.  We return the Save/2 */
		if (tmp<66) return(tmp/2);
		return ((tmp+33)/3);
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
		return (tmp/25);
		break;
		case S_BACKSTAB:
		if (tmp<80) return 0;
		return ((tmp-80)/33)+1;
		break;
		case S_DODGING:
		if (tmp<30) return 0;
		tmp=(tmp-25)/2; /* POSSIBLE AC mod---if wearing nothing */
		tmp-=armor_weight();
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
	return(tmp/7);
		break;
		case S_SLAY_ANIMAL:
		if (tmp<25) return 0;
		return ((tmp-25)/12)+1;
		break;
		case S_DISARM:
	return(tmp/2);
		break;
		case S_INFUSION:
		if (tmp<120) return 0; /* Means we can't do it */
		return((tmp-110)/2)+1;
		break;
		case S_ALCHEMY:
		if (tmp<100) return 0;
		return ((tmp-90)/2)-2;
		break;
		default:
		return tmp;
		break;
	}
}

/* Routine to determine todam modifier granted by the passed fighting skill  * We don't NEED one for the tohit, since it's already determined by the
	* appropriate fighting skill */
int stodam()
{
	int tmp;
	tmp = p_ptr->cur_skill[p_ptr->wskill];
	switch(p_ptr->wskill)
	{
		case S_KARATE: case S_WRESTLING: /* Damage is not modified */
		return 0;
		break;
		case S_SWORD: case S_HAFTED: case S_POLEARM:
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

/* This will determine a "general level" based on the various skills you
have.  Useful for determining extra HP and so on */

int get_level()
{
	int loop, sum, lvl;
	sum=0;
	lvl=0;
	for(loop=0;loop<S_NUM;loop++)
		sum+=p_ptr->adv_skill[loop];
	/* Now we search the "num_adv" array and find the player's REAL level */
	for(loop=0;loop<PY_MAX_LEVEL && sum>0;loop++,sum-=num_adv[loop],++lvl);
	if (lvl>PY_MAX_LEVEL) lvl=PY_MAX_LEVEL;
	if (lvl<1) lvl=1;
	return lvl;
}

/* This determines the amount of XP required to advance a skill.  NOTE: A
skill WILL NOT always advance by 1 point---if a race is good at something, it
may go up 5 or 6 points when advanced. */
s32b get_xp(int sk)
{
	s16b mult; /* Used to make XP required advance very quickly */
	int loop,total;
	s32b cur;
	total=0;
	for(loop=0;loop<S_NUM;loop++)
		total+=p_ptr->adv_skill[loop];
	mult=(total/45)+2;
	total*=mult;
	total/=3;
	cur=2+total; /* Thus we always need AT LEAST 2 XP to advance a skill */
	cur+=p_ptr->adv_skill[sk]*mult;
	if (p_ptr->max_skill[sk]>p_ptr->cur_skill[sk])
		cur=(cur/3)+1; /* Advance much more quickly if skill drained */
	return cur;
}

/* This returns a TOTAL todamage modifier (including weapon skill AND other
'specials'.  Needs flags3. */
int smoddam(u32b flags3)
{
	int todam, s, t, k;
	t=inventory[INVEN_WIELD].tval;
	s=p_ptr->wskill;
	todam=stodam(); /* Get base todamage for the current weapon skill */
	k=0;
	if ((flags3 & RF3_EVIL) && p_ptr->cur_skill[S_SLAY_EVIL]>=30)
		todam+=(p_ptr->cur_skill[S_SLAY_EVIL]-20)/10;
	if ((flags3 & RF3_ANIMAL) && p_ptr->cur_skill[S_SLAY_ANIMAL]>=30)
		todam+=(p_ptr->cur_skill[S_SLAY_ANIMAL]-20)/5;
	if ((flags3 & RF3_UNDEAD) && p_ptr->cur_skill[S_SLAY_UNDEAD]>=30)
		todam+=(p_ptr->cur_skill[S_SLAY_UNDEAD]-20)/5;
	if (((flags3 & RF3_RES_EDGED) && s==S_SWORD) ||
		((flags3 & RF3_RES_BLUNT) && s!=S_SWORD))
		k=-10000; /* Means we resisted the attack */
	else if (((flags3 & RF3_IM_EDGED) && s==S_SWORD) ||
			((flags3 & RF3_IM_BLUNT) && (s==S_HAFTED || s==S_POLEARM)))
		k=-4000; /* IMMUNE to the attack */
	return todam+k;
}

/* This returns an AC modifier for any and all skills OTHER THAN dodging (that is innate AC bonus) */
int stoac(u32b flags3)
{
	int toac;
	toac=0; /* All other skills accounted for */
	if ((flags3 & RF3_EVIL) && p_ptr->cur_skill[S_SLAY_EVIL]>=50)
		toac+=(p_ptr->cur_skill[S_SLAY_EVIL]-10)/4;
	if ((flags3 & RF3_ANIMAL) && p_ptr->cur_skill[S_SLAY_ANIMAL]>=40)
		toac+=(p_ptr->cur_skill[S_SLAY_EVIL]-10)/3;
	if ((flags3 & RF3_UNDEAD) && p_ptr->cur_skill[S_SLAY_UNDEAD]>=40)
		toac+=(p_ptr->cur_skill[S_SLAY_EVIL]-10)/3;
	return toac;
}

/* This returns which weapon skill we're using */
int sweapon()
{
	object_type *o_ptr;
	o_ptr = &inventory[INVEN_WIELD];
	if (!o_ptr->k_idx) return (p_ptr->barehand);
	if (o_ptr->tval == TV_HAFTED) return (S_HAFTED);
	if (o_ptr->tval == TV_POLEARM) return (S_POLEARM);
	return (S_SWORD);
}

/* Description for realm */
cptr realm_desc()
{
	switch(p_ptr->realm)
	{
		case NONE:
		return("You are not familiar with any magical art.");
		break;
		case MAGE:
		return("You know the Magic arts.");
		break;
		case PRIEST:
		return("You are a pious character.");
		break;
		case DRUID:
		return("You feel in harmony with the world.");
		break;
		case NECRO:
		return("You understand the forces of life and death.");
		break;
	}
}

/* Print a skill value */
void prt_skills_aux(int t, FILE *me)
{
	char out[50], filestr[80];
	cptr rank;
	int i, j;

	i=(t%2)*30+15;
	j=t/2+3;
	switch(p_ptr->cur_skill[t]/15)
	{
		case 0: case 1: rank = "Awful"; break;
		case 2: case 3: rank = "Poor"; break;
		case 4: case 5: case 6: rank = "Fair"; break;
		case 7: case 8: rank = "Good"; break;
		case 9: case 10: rank = "Very Good"; break;
		case 11: case 12: rank = "Excellent"; break;
		case 13: case 14: rank = "Superb"; break;
		case 15: rank = "Legendary"; break;
		default: rank = "Ungodly"; break;
	}
	if (p_ptr->cur_skill[t]==p_ptr->max_skill[t])
		sprintf(out,"%s: %-15s",snames[t],rank);
	else /* Uh oh.  Skill drained! */
		sprintf(out,"%s:(%-15s)",snames[t],rank);
	if (snames[t][0] && !me) /* Don't print unimplemented skills */
	{
		put_str(format("%c) ", t+65), j, i-3);
		put_str(out,j,i);
	}
	else if (snames[t][0]) /* Print to file */
	{
		if (t%2) /* Advance a line */
			fprintf(me,"%s\n",out);
		else
		{
			filestr[40-strlen(out)] = 0;
			fprintf(me, "%s%s", out, filestr);
			filestr[40-strlen(out)] = ' ';
		}
	}
}
/* This will print out skills onscreen (separate routine to a file) */
void prt_skills(char *title, FILE *me)
{
	int i, t;
	char filestr[80];

	if(!me)
	{
		Term_save();
		clear_from(0);
	}
	t=0;
	t=40-strlen(title)/2;
	if (!me)
		prt(title,1,t);
	else
	{
		for(i=0; i<79; i++)
			filestr[i]=' ';
		filestr[79]=0; /* Make a 'blank' string */
		filestr[t]=0;
		fprintf(me,"%c\n\n%s%s\n\n",12,filestr,title);
		filestr[t]=' '; /* Keep it for later */
	}
	for(t=0;t<S_NUM-2;t++) /* Leave last line free for Spell Info */
		prt_skills_aux(t, me);
	if (me)
		fprintf(me,"\n\n"); /* Nicely formatted */
	if (!me)
		prt(realm_desc(), 20, 15);
	else
		fprintf(me,"%s\n%c\n", realm_desc(), 12);	/* Add a ^l */
}

/* This allows PC to advance a skill */
void do_cmd_advance()
{
	int i, j, stay, k;
	char key;
	char out[70];
	sprintf(out,"** Skill Advancement:  Average Level %d **",get_level());
	prt_skills(out,(FILE *)0);
	prt("Type the appropriate letter to select a skill and", 21, 15);
	prt("<Spacebar> or <Enter> to advance it.  Skill info is in", 22, 15);
	prt("top line.  Press ESC to exit, or ? for skill info.", 23, 15);
	k = p_ptr->lastadv;
	sprintf(out,"Advancements: %d, XP Needed: %ld       ",
			p_ptr->adv_skill[k], get_xp(k));
	prt(out,0,10);
	stay=1;
	while(stay)
	{
		i = k%2;
		j = k/2;
		move_cursor(j+3,i*30+15);
		key=inkey();
		if (key >= 'a' && key <= 'z')
			key -= 32;
		switch(key)
		{
			case '?':
			sprintf(out,"%s will help you to %s.",snames[k],shelp[k]);
			msg_print(out);
			msg_print(NULL);
			prt(format("Advancements: %d, XP Needed: %ld       ",
				p_ptr->adv_skill[k], get_xp(k)), 0, 0);
			break;
			case ' ': case 13: /* Return pressed or Spacebar */
			(void) advance(k, 1);
			prt_skills_aux(k, (FILE *)0);
			prt(format("Advancements: %d, XP Needed: %ld       ",
				p_ptr->adv_skill[k], get_xp(k)), 0, 0);
			energy_use = 100;
			break;
			case 27: /* ESC */
			stay=0;
			break;
			default:
			{
				if(key<65 || key>90) break;
				key -= 65;
				if(!snames[(int)key][0]) break;
				k = key;
			prt(format("Advancements: %d, XP Needed: %ld       ",
				p_ptr->adv_skill[k], get_xp(k)), 0, 0);
			}
		}
	}
	Term_load();
	p_ptr->redraw |= (PR_EXP | PR_HP | PR_LEV | PR_TITLE);
}

/* This attempts to advance the given skill.  It will return 0 if failed, 1 if
successful.  Also, pass a 1 for dir to advance the skill, and a - to weaken
it by that many iterations.  The amount of experience is only relevant when
INCREASING a skill. */
int advance(int which, int d2)
{
	int i,j,next,old,loop,do_dec,dir,restore;
	char key;
	char out[80];
	long tmp;
	do_dec=0; /* If 1, we decrease skill */
	restore=0; /* If 1, we are restoring skill to normal */
	if (d2<0)
	{
		dir=-d2;
		do_dec=1; }
	else
		dir=d2;
	if (dir==255) restore=1;
	for(loop=0;loop<dir;loop++)
	{
		tmp=get_xp(which);
		if (p_ptr->cur_skill[which]==255)
		{
			msg_print("That skill is already at its limit!");
			msg_print(NULL);
			return 0;
		}
		if (p_ptr->exp<tmp && !do_dec && !restore) 
		{
			msg_print("You don't have enough experience to advance that skill!");
			msg_print(NULL);
			sprintf(out,"** Skill Advancement:  Average Level %d **",get_level());
			i=40-strlen(out)/2;
			prt("       ",1,i-7);
		prt(out,1,i);
			return 0;
		}
		if (!rp_ptr->start[which])
			return 0; /* Ignore non-working skills */
		old=p_ptr->cur_skill[which];
		next=p_ptr->cur_skill[which];
		if ((which==S_MAGIC || which==S_MPOWER) && (!p_ptr->adv_skill[S_MAGIC]
				                   && !p_ptr->adv_skill[S_MPOWER]) && !do_dec)
		{
			prt("Choose a Realm:  (S)orcery, (P)iety, (D)ruid, or (N)ecromacy", 0, 0);
			key=inkey();
			prt("", 0, 0);
			j=0;
			switch(key)
			{
				case 's': case 'S':
				p_ptr->realm = MAGE;
				strcpy(out,"You are learning Sorcery!");
				j=1;
				break;
				case 'p': case 'P':
				p_ptr->realm=PRIEST;
				strcpy(out,"You are learning the ways of God!");
				j=1;
				break;
				case 'd': case 'D':
				p_ptr->realm=DRUID;
				strcpy(out,"You are learning Nature magic!");
				j=1;
				break;
				case 'n': case 'N':
				p_ptr->realm=NECRO;
				strcpy(out,"You are learning Dark Sorcery!");
				j=1;
				break;
				default:
				strcpy(out,
			 "You need to pick a Realm before advancing magical skills!");
				break;
			}
			msg_print(out);
			msg_print(NULL);
			mp_ptr = &magic_info[p_ptr->realm];
			if (!j) return j; /* Otherwise, advance it */

			/* Update screen */
			prt(realm_desc(), 20, 15);
		}
		i = rp_ptr->skills[which]; /* Starting advance value */
		if (!do_dec && !restore)
			p_ptr->lastadv = which; /* Tells us the last-advanced skill */
		j=p_ptr->adv_skill[which];
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
		if (do_dec) p_ptr->cur_skill[which]-=i;
		else p_ptr->cur_skill[which]+=i;
		if (!do_dec && !restore)
		{
			p_ptr->exp-=tmp;
			p_ptr->max_exp-=tmp;
		}
		p_ptr->update |= (PU_HP | PU_BONUS);
		p_ptr->redraw |= PR_LEV;
		if (p_ptr->realm!=NONE)
		{
			p_ptr->update |= PU_MANA;
			p_ptr->update |= PU_SPELLS;
		}
		if (do_dec && p_ptr->adv_skill[which]>=1)
			--p_ptr->adv_skill[which];
		else
			++p_ptr->adv_skill[which];
		if (p_ptr->cur_skill[which]<p_ptr->min_skill[which])
			p_ptr->cur_skill[which]=p_ptr->min_skill[which];
		if (p_ptr->cur_skill[which]<old && !do_dec)
			p_ptr->cur_skill[which]=255; /* Wraparound */
		if (p_ptr->cur_skill[which]>=p_ptr->max_skill[which])
			p_ptr->max_skill[which]=p_ptr->cur_skill[which];
		if (restore && p_ptr->cur_skill[which]==p_ptr->max_skill[which])
			break; /* Get out of the loop now */
	}
	return 1;
}
