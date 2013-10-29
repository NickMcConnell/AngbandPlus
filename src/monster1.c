/* File: monster1.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"


/*
 * Pronoun arrays, by gender.
 */
static cptr wd_he[3] =
{ "it", "he", "she" };
static cptr wd_his[3] =
{ "its", "his", "her" };


/*
 * Pluralizer.  Args(count, singular, plural)
 */
#define plural(c,s,p) \
	(((c) == 1) ? (s) : (p))






/*
 * Determine if the "armor" is known
 * The higher the level, the fewer kills needed.
 */
static bool know_armour(int r_idx, const monster_lore *l_ptr)
{
	const monster_race *r_ptr = &r_info[r_idx];

	s32b level = r_ptr->level;

	s32b kills = l_ptr->tkills;

	/* this race has been probed */
	if (l_ptr->xtra1) return (TRUE);

	/* Normal monsters */
	if (kills > 304 / (4 + level)) return (TRUE);

	/* Skip non-uniques */
	if (!(r_ptr->flags1 & RF1_UNIQUE)) return (FALSE);

	/* Unique monsters */
	if (kills > 304 / (38 + (5 * level) / 4)) return (TRUE);

	/* Assume false */
	return (FALSE);
}


/*
 * Determine if the "damage" of the given attack is known
 * the higher the level of the monster, the fewer the attacks you need,
 * the more damage an attack does, the more attacks you need
 */
static bool know_damage(int r_idx, const monster_lore *l_ptr, int i)
{
	const monster_race *r_ptr = &r_info[r_idx];

	s32b level = r_ptr->level;

	s32b a = l_ptr->blows[i];

	s32b d1 = r_ptr->blow[i].d_dice;
	s32b d2 = r_ptr->blow[i].d_side;

	s32b d = d1 * d2;

	/* this race has been probed */
	if (l_ptr->xtra1) return (TRUE);

	/* Normal monsters */
	if ((4 + level) * a > 80 * d) return (TRUE);

	/* Skip non-uniques */
	if (!(r_ptr->flags1 & RF1_UNIQUE)) return (FALSE);

	/* Unique monsters */
	if ((4 + level) * (2 * a) > 80 * d) return (TRUE);

	/* Assume false */
	return (FALSE);
}

/* 
 * Print the monster description
 * ..This now includes a hack to allow statues to have different
 * individual descriptions.
 */
static void describe_monster_desc(int r_idx, const monster_type *m_ptr)
{
	const monster_race *r_ptr = &r_info[r_idx];
	char buf[2048];
	cptr mon_text = (r_text + r_ptr->text);
	int statue_descidx;
	
	if (m_ptr) statue_descidx = m_ptr->tinvis;
	else statue_descidx = 0;
	
	/* These statues are larger than life (bigger than the average man) */
	if ((strstr(mon_text, "statue special")) && (statue_descidx))
	{
		/* hack: tinvis used to remember this statue's decription */
		switch (statue_descidx)
		{
			/* #1-7 are historical(Tolkien) / military figures */
            case 1: 
			{
				text_out("This is an oversized statue of Al-Pharazon the Golden, the Last King of ");
				text_out("Numenor. He had the statue built of himself so the future ages could ");
                text_out("remember the most powerful of the kings who took Sauron prisoner and ");
                text_out("set out to conquer the Undying Lands. \n"); break;
			}
			case 2:   
			{
				text_out("This is a statue of Eorl on the back of a mighty horse,  He was the ");
				text_out("king of Rohan who tamed the first of the mearas, the great horses ");
				text_out("from whom Shadowfax, Gandalf's steed, was decended. \n"); break;
			}
			case 3: text_out("It's a statue of a Rohirrim knight on horseback. \n"); break;
			case 4: text_out("It's a statue of the Lord of the Nazgul on a black steed. \n"); break;
			case 5: text_out("It's a statue of an olog troll captain. \n"); break;
			case 6: text_out("It's a statue of an ogre chieftain. \n"); break;
			case 7: text_out("It's a statue of a mumak in haradrim battle gear. \n"); break;
			case 8: text_out("It's a statue of a fearsome ballrog. \n"); break;
			case 9: text_out("It's a statue of one of the great spiders, the decendants of Ungoliant. \n"); break;
			case 10: text_out("It's a statue of a noble dragon. \n"); break;
			case 11: text_out("It's a statue of a fierce dragon. \n"); break;
			case 12: text_out("It's a statue of a griffon. \n"); break;
			/* duplicates to maybe replace later */
			case 13: text_out("It's a statue of an ogre chieftain. \n"); break;
			case 14: text_out("It's a statue of a Rohirrim knight on horseback. \n"); break;
			/* gargoyles can mimmic these few statues (they BLOCK_LOS only while mimmicing) */
			/* at least they will, not yet */
			case 15: text_out("It's a stone troll turned to stone by a previous hero's magical light. \n"); break;
			case 16: text_out("It's a statue of a gargoyle about to swing his axe. \n"); break;
			case 17: text_out("It's a statue of a gargoyle with wings spread. \n"); break;
			/* 18 the beginning of the ugly/weird DARK_CITY statues (start at end) */
			case 18: text_out("It's a statue of a gargoyle sitting and making an ugly face. \n"); break;
			case 19: text_out("It's a statue of a gargoyle holding a man's severed head. \n"); break;
			case 20: text_out("It's a statue of a centaur with a bow and arrow. \n"); break;
			case 21: text_out("It's a statue of a winged horror. \n"); break;
			case 22: text_out("It's a statue of a giant serpent. \n"); break;
			case 23: text_out("It's a statue of a headless horseman. \n"); break;
			case 24: text_out("It's a statue of a pooka in the form of giant goblin. \n"); break;
			case 25: text_out("It's a statue of the dullahan driving its black coach. \n"); break;
			/* special for ruined statue */
			case 26: 
			{
				text_out("It's a ruined statue which once depicted somebody (or something)");
                text_out("on a horse, but all that's left of the rider is his legs. \n"); break;
			}
		}
	}
	/* These are man-sized statues or smaller */
	else if ((strstr(mon_text, "small statue")) && (statue_descidx))
	{
		/* hack: tinvis used to remember this statue's decription */
		switch (statue_descidx)
		{
			/* The first several statues should be dwarves & gnomes */
			case 1: 
			{
				text_out("This is a statue of Dain II 'Ironfoot', a king of Durin's folk ");
				text_out("who killed Azog the orc and later came to the aid of Thorin in the ");
				text_out("battle of five armies. \n"); break;
			}
            case 2: text_out("It's a statue of a gnome with gemcutting tools in hand. \n"); break;
			case 3: text_out("This is a statue of Nain, a dwarf king who ruled in Khazad-dum in the third age. \n"); break;
			case 4:  
			{
				text_out("This is a statue of Azaghal, a dwarf lord of ancient times who dealt ");
				text_out("Glaurung the dragon a near-fatal wound as he was dying. \n"); break;
			}
			case 5: 
			{
				text_out("This is a statue of Durin III, a dwarf king in the second age who was ");
				text_out("given the chief of the Seven dwarven rings. \n"); break;
			}
			case 6: 
			{
				text_out("This is a statue of Thorin, King Under the Mountain, who died ");
				text_out("in the battle of five armies as a result of his greed for the ");
				text_out("Arkenstone of Thrain. \n"); break;
			}
			case 7:  
			{
				text_out("This is a statue of Durin I, eldest of the seven fathers of ");
				text_out("the dwarves and ancestor of the most important clan of dwarves. "); 
				text_out("It was he who began the building of Khazad-dum. \n"); break;
			}
			case 8:  
			{
				text_out("This is a statue of Thrain I, a dwarf king of Durin's folk ");
				text_out("who led the dwarves to Erebor after the balrog had ");
				text_out("driven them out of Khazad-dum. He was the first with the ");
				text_out("title of King Under the Mountain. \n"); break;
			}
			/* Then a few statues of other historical figures */
			/* FOR LATER: a stone golem may rarely mimmic one of these */
			case 9:  
			{
				text_out("This is a statue of Isildur, the one who took the ");
				text_out("One Ring from Sauron's finger. \n"); break;
			}
			case 10:  text_out("This is a statue of King Theoden of Rohan. \n"); break;
			case 11: 
			{
				text_out("This is a statue of Tarondor, the king of Gondor who planted the ");
				text_out("seedling of the White Tree in the citadel of Minas Anor, which was ");
				text_out("later renamed Minas Tirith. \n"); break;
			}
			case 12: 
			{
				text_out("This is a statue of Earnur, who, with help from Glorfindel the elf ");
				text_out("lord, defeated Angmar and drove the Witch-King out of the north. He ");
				text_out("was later challenged and killed by the Witch-King, who then was ");
				text_out("ruling Minas Morgul.  Earnur was the last king of Gondor before ");
				text_out("the rule of the Stewards. \n"); break;
			}
			case 13: 
			{
				text_out("This is a statue of Bandobras Took, aka Bullroarer, ");
				text_out("who led the hobbits to defeat an attacking band of orcs. ");
				text_out("He was the third tallest hobbit in history. \n"); break;
			}
			case 14:  
			{
				text_out("This is a statue of Elendil who survived the fall of ");
				text_out("Numenor and became the first king of Arnor and Gondor. ");
				text_out("He was the father of Isildur and Anarion and he was ");
				text_out("the leader of the men of the Last Alliance between ");
				text_out("men and elves who defeated Sauron at the end of the ");
				text_out("second age. \n"); break;
			}
			/* a few misc statues */
			case 15: text_out("This is a staute of Mughash the kobold lord. \n"); break;
			case 16: text_out("It's a statue of an orc captain. \n"); break;
			case 17: text_out("It's a statue of an archer with an arrow fitted to his bow. \n"); break;
			case 18: text_out("It's a statue of some really old guy you don't recognise. \n"); break;
			/* 19 the beginning of the ugly/weird DARK_CITY statues */
			case 19: 
			{
				text_out("It's a statue of a giant rat. ");
				text_out("You have no idea why someone would make a statue of that. "); break;
			}
			case 20: text_out("It's a statue of a large cat. \n"); break;
			case 21: text_out("It's a statue of a satyr. \n"); break;
			case 22: text_out("It's a statue of a garden gnome. \n"); break;
			case 23: text_out("It's a statue of a dancing hobgoblin. \n"); break;
			case 24: text_out("It's a statue of a dancing erlbold. \n"); break;
			case 25: text_out("It's a statue of a dark fairy goblin king. \n"); break;
			/* 26 special for empty (ruined) vaults */
			case 26: text_out("It's a ruined statue. You can't even tell what it used to look like. \n"); break;
		}
	}
	/* Fountain statue descriptions (also have BLOCK_LOS) */
	else if ((strstr(mon_text, "fountain special")) && (statue_descidx))
	{
		/* hack: tinvis used to remember this statue's decription */
		switch (statue_descidx)
		{
            case 1: text_out("It's a fountain with the water pouring out of a gargoyle's mouth. \n"); break;
			case 2: text_out("Water pours from the mouth of this giant serpent made of stone. \n"); break;
			case 3: text_out("The spouts of this fountain are in the shape of four dragon heads. \n"); break;
			case 4: 
			{
				text_out("It's a statue of a zhelung lizard leaping out of the water, \n");
				text_out("spraying water and light. \n"); break;
			}
			case 5: text_out("It's a fountain with the water pouring out of a gargoyle's mouth. \n"); break;
			case 6: text_out("It's a fountain with the water pouring out of a gargoyle's mouth. \n"); break;
			case 7: text_out("It's a fountain with a statue of a mermaid. \n"); break;
			case 8: 
			{
				text_out("Water once poured out of this gargoyle's mouth except that ");
				text_out("now half the statue's face has fallen off. \n"); break;
			}
			case 9:
			{
				text_out("This was once a fountain. Over time it has crumbled so ");
				text_out("badly that you can just barely tell that it used to ");
				text_out("depict something like a dragon or serpent. \n"); break;
			}
		}
	}
	/* Have to have something to display for monster recall for statues */
	/*  when there is no m_ptr->tinvis */
	else if ((strstr(mon_text, "fountain special")) ||
		(strstr(mon_text, "statue special")) ||
		(strstr(mon_text, "small statue")))
	{
		text_out("The diverse dungeon denizens have collected or carved ");
		text_out("several various types of statues, as small as a halfling ");
		text_out("or as big as a life-size mumak. Some of them depict ");
		text_out("Morgoth's enemies or victims of times long past. ");
		text_out("Others depict his allies and monsters. \n");
	}
	else /* anything not a statue */
	{
		/* Simple method */
		my_strcpy(buf, r_text + r_ptr->text, sizeof(buf));

		/* Dump it */
		text_out(buf);
		text_out("\n");
	}
}


static void describe_monster_spells(int r_idx, const monster_lore *l_ptr)
{
	const monster_race *r_ptr = &r_info[r_idx];
	int m, n;
	int msex = 0;
	bool breath = FALSE;
	bool magic = FALSE;
	int vn;
	cptr vp[64];


	/* Extract a gender (if applicable) */
	if (r_ptr->flags1 & RF1_FEMALE) msex = 2;
	else if (r_ptr->flags1 & RF1_MALE) msex = 1;

	/* Collect innate attacks */
	vn = 0;
	if (l_ptr->flags4 & RF4_SHRIEK)  vp[vn++] = "shriek for help";
	if (l_ptr->flags4 & RF4_T_AXE)   vp[vn++] = "hurl a throwing axe";
	if (l_ptr->flags4 & RF4_THROW)   vp[vn++] = "throw a piece of junk";
	if (l_ptr->flags4 & RF4_ARROW_1) vp[vn++] = "fire an arrow";
	if (l_ptr->flags4 & RF4_ARROW_2) vp[vn++] = "fire arrows";
	if (l_ptr->flags4 & RF4_ARROW_3) vp[vn++] = "fire a missile";
	if (l_ptr->flags4 & RF4_ARROW_4) vp[vn++] = "fire missiles";
	if (l_ptr->flags4 & RF4_BOULDER) vp[vn++] = "throw boulders";

	/* Describe innate attacks */
	if (vn)
	{
		/* Intro */
		text_out(format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) text_out(" may ");
			else if (n < vn-1) text_out(", ");
			else text_out(" or ");

			/* Dump */
			text_out_c(TERM_L_RED, vp[n]);
		}

		/* End */
		text_out(".  ");
	}


	/* Collect breaths */
	vn = 0;
	if (l_ptr->flags4 & RF4_BR_ACID)		vp[vn++] = "acid";
	if (l_ptr->flags4 & RF4_BR_ELEC)		vp[vn++] = "lightning";
	if (l_ptr->flags4 & RF4_BR_FIRE)		vp[vn++] = "fire";
	if (l_ptr->flags4 & RF4_BR_COLD)		vp[vn++] = "frost";
	if (l_ptr->flags4 & RF4_BR_POIS)		vp[vn++] = "poison";
	if (l_ptr->flags4 & RF4_BR_NETH)		vp[vn++] = "nether";
	if (l_ptr->flags4 & RF4_BR_LITE)		vp[vn++] = "light";
	if (l_ptr->flags4 & RF4_BR_DARK)		vp[vn++] = "darkness";
	if (l_ptr->flags4 & RF4_BR_CONF)		vp[vn++] = "confusion";
	if (l_ptr->flags4 & RF4_BR_SOUN)		vp[vn++] = "sound";
	if (l_ptr->flags4 & RF4_BR_CHAO)		vp[vn++] = "chaos";
	if (l_ptr->flags4 & RF4_BR_DISE)		vp[vn++] = "disenchantment";
	if (l_ptr->flags4 & RF4_BR_NEXU)		vp[vn++] = "nexus";
	if (l_ptr->flags4 & RF4_BR_TIME)		vp[vn++] = "time";
	if (l_ptr->flags4 & RF4_BR_INER)		vp[vn++] = "inertia";
	if (l_ptr->flags4 & RF4_BR_GRAV)		vp[vn++] = "gravity";
	if (l_ptr->flags4 & RF4_BR_SHAR)		vp[vn++] = "shards";
	if (l_ptr->flags4 & RF4_BR_PLAS)		vp[vn++] = "plasma";
	if (l_ptr->flags4 & RF4_BR_WALL)		vp[vn++] = "force";
	if (l_ptr->flags4 & RF4_BR_FEAR)		vp[vn++] = "fear";
	if (l_ptr->flags4 & RF4_BR_AMNS)		vp[vn++] = "amnesia";
	if (l_ptr->flags4 & RF4_BR_SLIME)		vp[vn++] = "slime";

	/* Describe breaths */
	if (vn)
	{
		/* Note breath */
		breath = TRUE;

		/* Intro */
		text_out(format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) text_out(" may breathe ");
			else if (n < vn-1) text_out(", ");
			else text_out(" or ");

			/* Dump */
			text_out_c(TERM_L_RED, vp[n]);
		}
	}


	/* Collect spells */
	vn = 0;
	if (l_ptr->flags5 & RF5_BA_ACID)     vp[vn++] = "produce acid balls";
	if (l_ptr->flags5 & RF5_BA_ELEC)     vp[vn++] = "produce lightning balls";
	if (l_ptr->flags5 & RF5_BA_FIRE)     vp[vn++] = "produce fire balls";
	if (l_ptr->flags5 & RF5_BA_COLD)     vp[vn++] = "produce frost balls";
	if (l_ptr->flags5 & RF5_BA_POIS)     vp[vn++] = "produce poison balls";
	if (l_ptr->flags5 & RF5_BA_NETH)     vp[vn++] = "produce nether balls";
	if (l_ptr->flags5 & RF5_BA_WATE)     vp[vn++] = "produce water balls";
	if (l_ptr->flags4 & RF4_EXPLODE)     vp[vn++] = "produce explosions";
	if (l_ptr->flags5 & RF5_BA_MANA)     vp[vn++] = "invoke mana storms";
	if (l_ptr->flags5 & RF5_BA_DARK)     vp[vn++] = "invoke darkness storms";
	if (l_ptr->flags5 & RF5_DRAIN_MANA)  vp[vn++] = "drain mana";
	if (l_ptr->flags5 & RF5_MIND_BLAST)  vp[vn++] = "cause mind blasting";
	if (l_ptr->flags5 & RF5_BRAIN_SMASH) vp[vn++] = "cause brain smashing";
	if (l_ptr->flags5 & RF5_CAUSE_1)     vp[vn++] = "cause light wounds";
	if (l_ptr->flags5 & RF5_CAUSE_2)     vp[vn++] = "cause serious wounds";
	if (l_ptr->flags5 & RF5_CAUSE_3)     vp[vn++] = "cause critical wounds";
	if (l_ptr->flags5 & RF5_CAUSE_4)     vp[vn++] = "cause mortal wounds";
	if (l_ptr->flags5 & RF5_BO_ACID)     vp[vn++] = "produce acid bolts";
	if (l_ptr->flags5 & RF5_BO_ELEC)     vp[vn++] = "produce lightning bolts";
	if (l_ptr->flags5 & RF5_BO_FIRE)     vp[vn++] = "produce fire bolts";
	if (l_ptr->flags5 & RF5_BO_COLD)     vp[vn++] = "produce frost bolts";
	if (l_ptr->flags5 & RF5_BO_POIS)     vp[vn++] = "produce poison bolts";
	if (l_ptr->flags5 & RF5_BO_NETH)     vp[vn++] = "produce nether bolts";
	if (l_ptr->flags5 & RF5_BO_WATE)     vp[vn++] = "produce water bolts";
	if (l_ptr->flags5 & RF5_BO_MANA)     vp[vn++] = "produce mana bolts";
	if (l_ptr->flags5 & RF5_BO_PLAS)     vp[vn++] = "produce plasma bolts";
	if (l_ptr->flags5 & RF5_BO_ICEE)     vp[vn++] = "produce ice bolts";
	if (l_ptr->flags5 & RF5_MISSILE)     vp[vn++] = "produce magic missiles";
	if (l_ptr->flags5 & RF5_SCARE)       vp[vn++] = "terrify";
	if (l_ptr->flags4 & RF4_MCONTROL)    vp[vn++] = "control your body";
	if (l_ptr->flags5 & RF5_BLIND)       vp[vn++] = "blind";
	if (l_ptr->flags5 & RF5_CONF)        vp[vn++] = "confuse";
	if (l_ptr->flags5 & RF5_SLOW)        vp[vn++] = "slow";
	if (l_ptr->flags5 & RF5_HOLD)        vp[vn++] = "paralyze";
	if (l_ptr->flags6 & RF6_HASTE)       vp[vn++] = "haste-self";
	if (l_ptr->flags6 & RF6_HEAL_KIN)    vp[vn++] = "heal kin";
	if ((l_ptr->flags6 & RF6_HEAL) && (l_ptr->flags3 & RF3_HELPER)) vp[vn++] = "heal-self and you";
	if (l_ptr->flags6 & RF6_HEAL)        vp[vn++] = "heal-self";
	if (l_ptr->flags6 & RF6_HEAL_OTHR)   vp[vn++] = "heal other monsters";
	if (l_ptr->flags6 & RF6_BLINK)       vp[vn++] = "blink-self";
	if (l_ptr->flags6 & RF6_TPORT)       vp[vn++] = "teleport-self";
	if (l_ptr->flags6 & RF6_XXX4)        vp[vn++] = "do something";
	if (l_ptr->flags6 & RF6_TELE_TO)     vp[vn++] = "teleport to";
	if (l_ptr->flags6 & RF6_TELE_AWAY)   vp[vn++] = "teleport away";
	if (l_ptr->flags6 & RF6_TELE_LEVEL)  vp[vn++] = "teleport level";
	if (l_ptr->flags6 & RF6_CURSE_PC)    vp[vn++] = "curse you";
	if (l_ptr->flags6 & RF6_DARKNESS)    vp[vn++] = "create darkness";
	if (l_ptr->flags6 & RF6_TRAPS)       vp[vn++] = "create traps";
	if (l_ptr->flags6 & RF6_FORGET)      vp[vn++] = "cause amnesia";
	if (l_ptr->flags6 & RF6_INVIS)       vp[vn++] = "temporarily turn itself invisible";
	if (l_ptr->flags6 & RF6_S_SILVER)    vp[vn++] = "summon grepse (silver beings)";
	if (l_ptr->flags6 & RF6_S_KIN)       vp[vn++] = "summon similar monsters";
	if (l_ptr->flags6 & RF6_S_MONSTER)   vp[vn++] = "summon a monster";
	if (l_ptr->flags6 & RF6_S_MONSTERS)  vp[vn++] = "summon monsters";
	if (l_ptr->flags6 & RF6_S_ANIMAL)    vp[vn++] = "summon animals";
	if (l_ptr->flags6 & RF6_S_SPIDER)    vp[vn++] = "summon spiders";
	if (l_ptr->flags6 & RF6_S_HOUND)     vp[vn++] = "summon hounds";
	if (l_ptr->flags6 & RF6_S_HYDRA)     vp[vn++] = "summon hydras";
	if (l_ptr->flags6 & RF6_S_ANGEL)     vp[vn++] = "summon an ape";
	if (l_ptr->flags6 & RF6_S_DEMON)     vp[vn++] = "summon a demon";
	if (l_ptr->flags6 & RF6_S_UNDEAD)    vp[vn++] = "summon an undead";
	if (l_ptr->flags6 & RF6_S_DRAGON)    vp[vn++] = "summon a dragon";
	if (l_ptr->flags6 & RF6_S_HI_UNDEAD) vp[vn++] = "summon Greater Undead";
	if (l_ptr->flags6 & RF6_S_HI_DRAGON) vp[vn++] = "summon Ancient Dragons";
	if (l_ptr->flags6 & RF6_S_HI_DEMON)  vp[vn++] = "summon Greater Demons";
	if (l_ptr->flags6 & RF6_S_WRAITH)    vp[vn++] = "summon Ring Wraiths";
	if (l_ptr->flags6 & RF6_S_UNIQUE)    vp[vn++] = "summon Unique Monsters";

	/* Describe spells */
	if (vn)
	{
		/* Note magic */
		magic = TRUE;

		/* Intro */
		if (breath)
		{
			text_out(", and is also");
		}
		else
		{
			text_out(format("%^s is", wd_he[msex]));
		}

		/* Verb Phrase */
		text_out(" magical, casting spells");

		/* Adverb */
		if (l_ptr->flags2 & RF2_SMART) text_out_c(TERM_ORANGE, " intelligently");

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) text_out(" which ");
			else if (n < vn-1) text_out(", ");
			else text_out(" or ");

			/* Dump */
			text_out_c(TERM_L_RED, vp[n]);
		}
	}


	/* End the sentence about innate/other spells */
	if (breath || magic)
	{
		/* Total casting */
		m = l_ptr->cast_innate + l_ptr->cast_spell;

		/* Average frequency */
		n = (r_ptr->freq_innate + r_ptr->freq_spell) / 2;

		/* Describe the spell frequency */
		if (m > 100)
		{
			text_out(format("; 1 time in %d", 100 / n));
		}

		/* Guess at the frequency */
		else if (m)
		{
			n = ((n + 9) / 10) * 10;
			text_out(format("; about 1 time in %d", 100 / n));
		}

		/* End this sentence */
		text_out(".  ");
	}
	
	if ((breath == TRUE) && (r_ptr->flags2 & (RF2_BR_STRONG)))
	{
		text_out(format("%^s", wd_he[msex]));
		text_out(" has powerful breath.  ");
    }
}


static void describe_monster_drop(int r_idx, const monster_lore *l_ptr)
{
	const monster_race *r_ptr = &r_info[r_idx];
	bool sin = FALSE;
	int n;
	cptr p;
	int msex = 0;


	/* Extract a gender (if applicable) */
	if (r_ptr->flags1 & RF1_FEMALE) msex = 2;
	else if (r_ptr->flags1 & RF1_MALE) msex = 1;

	/* Drops gold and/or items */
	if (l_ptr->drop_gold || l_ptr->drop_item)
	{
		/* Intro */
		text_out(format("%^s may carry", wd_he[msex]));

		/* Count maximum drop */
		n = MAX(l_ptr->drop_gold, l_ptr->drop_item);

		/* One drop (may need an "n") */
		if (n == 1)
		{
			text_out(" a");
			sin = TRUE;
		}

		/* Two drops */
		else if (n == 2)
		{
			text_out(" one or two");
		}

		/* Many drops */
		else
		{
			text_out(format(" up to %d", n));
		}


		/* Great */
		if (l_ptr->flags1 & RF1_DROP_GREAT)
		{
			p = " exceptional";
		}

		/* Good (no "n" needed) */
		else if (l_ptr->flags1 & RF1_DROP_GOOD)
		{
			p = " good";
			sin = FALSE;
		}

		/* Okay */
		else
		{
			p = NULL;
		}


		/* Objects */
		if (l_ptr->drop_item)
		{
			/* Handle singular "an" */
			if (sin) text_out("n");
			sin = FALSE;

			/* Dump "object(s)" */
			if (p) text_out(p);
			text_out(" object");
			if (n != 1) text_out("s");

			/* Conjunction replaces variety, if needed for "gold" below */
			p = " or";
		}

		/* Treasures */
		if (l_ptr->drop_gold)
		{
			/* Cancel prefix */
			if (!p) sin = FALSE;

			/* Handle singular "an" */
			if (sin) text_out("n");

			/* Dump "treasure(s)" */
			if (p) text_out(p);
			text_out(" treasure");
			if (n != 1) text_out("s");
		}

		/* End this sentence */
		text_out(".  ");
	}
}


static void describe_monster_attack(int r_idx, const monster_lore *l_ptr)
{
	const monster_race *r_ptr = &r_info[r_idx];
	int m, n, r;
	cptr p, q;

	int msex = 0;

	/* Extract a gender (if applicable) */
	if (r_ptr->flags1 & RF1_FEMALE) msex = 2;
	else if (r_ptr->flags1 & RF1_MALE) msex = 1;

	
	/* Count the number of "known" attacks */
	for (n = 0, m = 0; m < MONSTER_BLOW_MAX; m++)
	{
		/* Skip non-attacks */
		if (!r_ptr->blow[m].method) continue;

		/* Count known attacks */
		if (l_ptr->blows[m]) n++;
	}

	/* Examine (and count) the actual attacks */
	for (r = 0, m = 0; m < MONSTER_BLOW_MAX; m++)
	{
		int method, effect, d1, d2;

		/* Skip non-attacks */
		if (!r_ptr->blow[m].method) continue;

		/* Skip unknown attacks */
		if ((!l_ptr->blows[m]) && (!l_ptr->xtra1)) continue;


		/* Extract the attack info */
		method = r_ptr->blow[m].method;
		effect = r_ptr->blow[m].effect;
		d1 = r_ptr->blow[m].d_dice;
		d2 = r_ptr->blow[m].d_side;


		/* No method yet */
		p = NULL;

		/* Get the method */
		switch (method)
		{
			case RBM_HIT:	p = "hit"; break;
			case RBM_TOUCH:	p = "touch"; break;
			case RBM_PUNCH:	p = "punch"; break;
			case RBM_KICK:	p = "kick"; break;
			case RBM_CLAW:	p = "claw"; break;
			case RBM_CLAWB:	p = "claw"; break;
			case RBM_BITE:	p = "bite"; break;
			case RBM_STING:	p = "sting"; break;
			case RBM_TAIL:	p = "swing its tail"; break;
			case RBM_BUTT:	p = "butt"; break;
			case RBM_CRUSH:	p = "crush"; break;
			case RBM_ENGULF:	p = "engulf"; break;
			/* case RBM_XXX2:	break; */
			case RBM_CRAWL:	p = "crawl on you"; break;
			case RBM_DROOL:	p = "drool on you"; break;
			case RBM_SPIT:	p = "spit"; break;
			case RBM_KISS:	p = "kiss you"; break;
			case RBM_GAZE:	p = "gaze"; break;
			case RBM_WAIL:	p = "wail"; break;
			case RBM_SPORE:	p = "release spores"; break;
			case RBM_XXX4:	break;
			case RBM_BEG:	p = "beg"; break;
			case RBM_INSULT:	p = "insult"; break;
			case RBM_MOAN:	p = "moan"; break;
			case RBM_XXX5:	break;
		}


		/* Default effect */
		q = NULL;

		/* Get the effect */
		switch (effect)
		{
			case RBE_HURT:      q = "attack"; break;
			case RBE_POISON:    q = "poison"; break;
			case RBE_UN_BONUS:  q = "disenchant"; break;
			case RBE_UN_POWER:  q = "drain charges"; break;
			case RBE_EAT_GOLD:  q = "steal gold"; break;
			case RBE_EAT_ITEM:  q = "steal items"; break;
			case RBE_EAT_FOOD:  q = "eat your food"; break;
			case RBE_HUNGER:    q = "make you hungry"; break;
			case RBE_EAT_LITE:  q = "absorb light"; break;
			case RBE_ACID:      q = "shoot acid"; break;
			case RBE_ELEC:      q = "electrify"; break;
			case RBE_FIRE:      q = "burn"; break;
			case RBE_COLD:      q = "freeze"; break;
			case RBE_BLIND:     q = "blind"; break;
			case RBE_SBLIND:    q = "poke your eyes (blind)"; break;
			case RBE_CONFUSE:   q = "confuse"; break;
			case RBE_XCONF:     q = "powerfully confuse"; break;
			case RBE_TERRIFY:   q = "terrify"; break;
			case RBE_PARALYZE:  q = "paralyze"; break;
			case RBE_LOSE_STR:  q = "reduce strength"; break;
			case RBE_LOSE_INT:  q = "reduce intelligence"; break;
			case RBE_LOSE_WIS:  q = "reduce wisdom"; break;
			case RBE_LOSE_DEX:  q = "reduce dexterity"; break;
			case RBE_LOSE_CON:  q = "reduce constitution"; break;
			case RBE_LOSE_CHR:  q = "reduce charisma"; break;
			case RBE_LOSE_ALL:  q = "reduce all stats"; break;
			case RBE_SHATTER:   q = "shatter"; break;
			case RBE_EXP_10:    q = "lower experience"; break;
			case RBE_EXP_20:    q = "lower experience"; break;
			case RBE_EXP_40:    q = "lower experience"; break;
			case RBE_EXP_80:    q = "lower experience"; break;
			case RBE_HALLU:     q = "cause hallucinations"; break;
			case RBE_SILVER:    q = "corrupt with silver poison"; break;
			case RBE_SLIME:     q = "slime"; break;
			case RBE_CHARM:     q = "charm"; break;
			case RBE_FRENZY:    q = "put you in a mad careless frenzy"; break;
			case RBE_PIXIEKISS: q = "bless you"; break;
			case RBE_FIREDARK:  q = "sometimes give you its powers"; break;
			case RBE_HATELIFE:  q = "cast dispel living"; break;
			case RBE_BLOODWRATH: q = "give you berserk rage"; break;
			case RBE_STUDY:     q = "restore your magical abilities"; break;
/*			case RBE_SPHCHARM:  q = ""; break; (no description) */
/*			case RBE_ZIMPKISS:  q = ""; break; (no description) */
			case RBE_ENTHELP:   q = "give you entdraught"; break;
			case RBE_PURIFY:    q = "purify you"; break;
			case RBE_UNLUCKY:   q = "drain luck"; break;
			case RBE_BHOLD:		q = "grab you"; break;
		}


		/* Introduce the attack description */
		if (!r)
		{
			text_out(format("%^s can ", wd_he[msex]));
		}
		else if (r < n-1)
		{
			text_out(", ");
		}
		else
		{
			text_out(", and ");
		}


		/* Hack -- force a method */
		if (!p) p = "do something weird";

		/* Describe the method */
		text_out(p);


		/* Describe the effect (if any) */
		if (q)
		{
			/* Describe the attack type */
			text_out(" to ");
			text_out_c(TERM_L_RED, q);

			/* Describe damage (if known) */
			if (d1 && d2 && know_damage(r_idx, l_ptr, m))
			{
				if (r_ptr->flags3 & RF3_HELPER) /* no real damage */;
				else
				{
					/* Display the damage */
					text_out(" with damage");
					text_out(format(" %dd%d", d1, d2));
				}
			}
		}


		/* Count the attacks as printed */
		r++;
	}

	/* Finish sentence above */
	if (r)
	{
		text_out(".  ");
	}

	/* Notice lack of attacks */
	else if (l_ptr->flags1 & RF1_NEVER_BLOW)
	{
		text_out(format("%^s has no physical attacks.  ", wd_he[msex]));
	}

	/* Or describe the lack of knowledge */
	/* (assume NONMONSTERs don't attack) */
	else if (!(r_ptr->flags7 & (RF7_NONMONSTER)))
	{
		text_out(format("Nothing is known about %s attack.  ", wd_his[msex]));
	}
}


static void describe_monster_abilities(int r_idx, const monster_lore *l_ptr)
{
	const monster_race *r_ptr = &r_info[r_idx];

	int n, vn, stl;
	cptr vp[64];
	int drastic[64];
	int msex = 0;

	/* Extract a gender (if applicable) */
	if (r_ptr->flags1 & RF1_FEMALE) msex = 2;
	else if (r_ptr->flags1 & RF1_MALE) msex = 1;

	/* Collect special abilities. */
	vn = 0;
	if (l_ptr->flags2 & RF2_OPEN_DOOR) vp[vn++] = "open doors";
	if (l_ptr->flags2 & RF2_BASH_DOOR) vp[vn++] = "bash down doors";
	if (l_ptr->flags2 & RF2_PASS_WALL) vp[vn++] = "pass through walls";
	if (l_ptr->flags2 & RF2_PASS_DOOR)
    {
        if (strchr("JScjlvw,", r_ptr->d_char)) vp[vn++] = "pass under doors";
        else vp[vn++] = "pass through doors";
    }
	if (l_ptr->flags2 & RF2_KILL_WALL) vp[vn++] = "bore through walls";
	if (l_ptr->flags2 & RF2_MOVE_BODY) vp[vn++] = "push past weaker monsters";
	if (l_ptr->flags2 & RF2_KILL_BODY) vp[vn++] = "destroy weaker monsters";
	if (l_ptr->flags2 & RF2_TAKE_ITEM) vp[vn++] = "pick up objects";
	if (l_ptr->flags2 & RF2_KILL_ITEM) vp[vn++] = "destroy objects";

	/* Describe special abilities. */
	if (vn)
	{
		/* Intro */
		text_out(format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) text_out(" can ");
			else if (n < vn-1) text_out(", ");
			else text_out(" and ");

			/* Dump */
			text_out(vp[n]);
		}

		/* End */
		text_out(".  ");
	}


	/* Describe special abilities. */
	if (l_ptr->flags2 & RF2_INVISIBLE)
	{
		text_out(format("%^s is invisible.  ", wd_he[msex]));
	}
	if (l_ptr->flags2 & RF2_MONGLOW)
	{
		text_out(format("%^s gives off light.  ", wd_he[msex]));
	}
	if (l_ptr->flags2 & RF2_EMPTY_MIND)
	{
		text_out(format("%^s is not detected by telepathy.  ", wd_he[msex]));
	}
	if (l_ptr->flags2 & RF2_WEIRD_MIND)
	{
		text_out(format("%^s is rarely detected by telepathy.  ", wd_he[msex]));
	}
	if (l_ptr->flags2 & RF2_MULTIPLY)
	{
		text_out(format("%^s breeds explosively.  ", wd_he[msex]));
	}
	if (l_ptr->flags2 & RF2_REGENERATE)
	{
		text_out(format("%^s regenerates quickly.  ", wd_he[msex]));
	}
	if ((l_ptr->flags2 & RF2_RETURNS) && (l_ptr->flags3 & RF3_UNDEAD))
	{
		text_out(format("%^s may reassemble itself after it has been destroyed.  ", wd_he[msex]));
	}
	else if (l_ptr->flags2 & RF2_RETURNS)
	{
		text_out(format("%^s may rise from the dead.  ", wd_he[msex]));
	}

	if (!(r_ptr->flags7 & (RF7_NONMONSTER)))
	{
		stl = r_ptr->stealth;
		/* WATER_ONLY monsters get automatic stealth bonus unless out of water (which is rare) */
		if ((stl < 4) && (r_ptr->flags7 & (RF7_WATER_ONLY))) stl += 2;

	    if (stl == 2) text_out(format("%^s is slightly stealthy", wd_he[msex]));
	    else if (stl == 3) text_out(format("%^s is stealthy", wd_he[msex]));
	    else if (stl == 4) text_out(format("%^s is very stealthy", wd_he[msex]));
	    else if (stl == 5) text_out(format("%^s is extremely stealthy", wd_he[msex]));
	    else if (stl > 5) text_out(format("%^s is extremely stealthy!  ", wd_he[msex]));
	    else if (stl < 1) text_out(format("%^s is not at all stealthy", wd_he[msex]));
    	else /* stl == 1 */ text_out(format("%^s is not very stealthy", wd_he[msex]));

		if (r_ptr->stealth <= 5)
		{
			if ((r_ptr->stealth < 3) && (r_ptr->flags7 & (RF7_WATER_HIDE)))
				text_out(", but can hide better in water.  ");
			else if (r_ptr->flags7 & (RF7_WATER_HIDE))
				text_out(", and can hide even better in water.  ");
			else text_out(".  ");
		}
	}

	if (r_ptr->flags7 & (RF7_HATE_WATER))
		text_out(format("%^s hates water.  ", wd_he[msex]));
	if (r_ptr->flags7 & (RF7_WATER_ONLY))
		text_out(format("%^s lives only in water.  ", wd_he[msex]));
	if (r_ptr->flags7 & (RF7_BLOCK_LOS))
		text_out(format("%^s blocks your view.  ", wd_he[msex]));

	/* Collect susceptibilities */
	vn = 0;
	if (l_ptr->flags3 & RF3_HURT_ROCK)
	{
		vp[vn++] = "rock remover";
	}
	if ((l_ptr->know_MRlite) && (r_ptr->Rlite < 0)) 
	{
		drastic[vn] = r_ptr->Rlite;
		vp[vn++] = "bright light";
	}
	if ((l_ptr->know_MRdark) &&	(r_ptr->Rdark < 0)) 
	{
		drastic[vn] = r_ptr->Rdark;
		vp[vn++] = "magical darkness";
	}
	if ((l_ptr->know_MRsilv) &&	(r_ptr->Rsilver < 0)) 
	{
		drastic[vn] = r_ptr->Rsilver;
		vp[vn++] = "silver";
	}
	if ((l_ptr->know_MRfire) &&	(r_ptr->Rfire < 0)) 
	{
		drastic[vn] = r_ptr->Rfire;
		vp[vn++] = "fire";
	}
	if ((l_ptr->know_MRcold) &&	(r_ptr->Rcold < 0)) 
	{
		drastic[vn] = r_ptr->Rcold;
		vp[vn++] = "cold";
	}
	if ((l_ptr->know_MRelec) &&	(r_ptr->Relec < 0)) 
	{
		drastic[vn] = r_ptr->Relec;
		vp[vn++] = "electricity";
	}
	if ((l_ptr->know_MRacid) &&	(r_ptr->Racid < 0)) 
	{
		drastic[vn] = r_ptr->Racid;
		vp[vn++] = "acid";
	}
	if ((l_ptr->know_MRpois) &&	(r_ptr->Rpois < 0)) 
	{
		drastic[vn] = r_ptr->Rpois;
		vp[vn++] = "poison";
	}
	if ((l_ptr->know_MRwatr) &&	(r_ptr->Rwater < 0))
	{
		drastic[vn] = r_ptr->Rwater;
		vp[vn++] = "water";
	}

	/* Describe susceptibilities */
	if (vn)
	{
		/* Intro */
		text_out(format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) text_out(" is hurt by ");
			else if (n < vn-1) text_out(", ");
			else text_out(", and ");

			/* Dump */
			text_out_c(TERM_YELLOW, vp[n]);
			
			if (drastic[n] == -1) text_out(" (a little)");
			if (drastic[n] == -2) text_out(" (moderately)");
			if (drastic[n] == -3) text_out(" (a lot)");
		}

		/* End */
		text_out(".  ");
	}


	/* Collect immunities and resistances (needswork) */
	vn = 0;
	if ((l_ptr->know_MRacid) &&	(r_ptr->Racid > 0))
	{
		drastic[vn] = r_ptr->Racid;
		vp[vn++] = "acid";
	}
	if ((l_ptr->know_MRelec) &&	(r_ptr->Relec > 0))
	{
		drastic[vn] = r_ptr->Relec;
		vp[vn++] = "electricity";
	}
	if ((l_ptr->know_MRfire) &&	(r_ptr->Rfire > 0))
	{
		drastic[vn] = r_ptr->Rfire;
		vp[vn++] = "fire";
	}
	if ((l_ptr->know_MRcold) &&	(r_ptr->Rcold > 0))
	{
		drastic[vn] = r_ptr->Rcold;
		vp[vn++] = "cold";
	}
	if ((l_ptr->know_MRpois) &&	(r_ptr->Rpois > 0))
	{
		drastic[vn] = r_ptr->Rpois;
		vp[vn++] = "poison";
	}
	if ((l_ptr->know_MRwatr) &&	(r_ptr->Rwater > 0))
	{
		drastic[vn] = r_ptr->Rwater;
		vp[vn++] = "water";
	}
	if (l_ptr->flags3 & RF3_RES_NETH)
	{
		drastic[vn] = 2;
		vp[vn++] = "nether";
	}
	if ((l_ptr->know_MRnexu) &&	(r_ptr->Rnexus > 0))
	{
		drastic[vn] = r_ptr->Rnexus;
		vp[vn++] = "nexus";
	}
	if ((l_ptr->know_MRdise) &&	(r_ptr->Rdisen > 0))
	{
		drastic[vn] = r_ptr->Rdisen;
		vp[vn++] = "disenchantment";
	}
	if ((l_ptr->know_MRmisl) &&	(r_ptr->Rmissile > 0))
	{
		drastic[vn] = r_ptr->Rmissile;
		vp[vn++] = "projectiles";
	}

	/* Describe immunities and resistances */
	if (vn)
	{
		/* Intro */
		text_out(format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) text_out(" resists ");
			else if (n < vn-1) text_out(", ");
			else text_out(", and ");

			/* Dump */
			text_out_c(TERM_ORANGE, vp[n]);
			
			if (drastic[n] == 1) text_out(" (slightly)");
			if (drastic[n] == 2) text_out(" (moderately)");
			if (drastic[n] == 3) text_out(" (immune)");
		}

		/* End */
		text_out(".  ");
	}


	/* Collect non-effects */
	vn = 0;
	if (l_ptr->flags3 & RF3_NO_STUN) vp[vn++] = "stunned";
	if (l_ptr->flags3 & RF3_NO_FEAR) vp[vn++] = "frightened";
	if (l_ptr->flags3 & RF3_NO_CONF) vp[vn++] = "confused";
	if (l_ptr->flags3 & RF3_NO_SLEEP) vp[vn++] = "slept";

	/* Describe non-effects */
	if (vn)
	{
		/* Intro */
		text_out(format("%^s", wd_he[msex]));

		/* Scan */
		for (n = 0; n < vn; n++)
		{
			/* Intro */
			if (n == 0) text_out(" cannot be ");
			else if (n < vn-1) text_out(", ");
			else text_out(" or ");

			/* Dump */
			text_out_c(TERM_YELLOW, vp[n]);
		}

		/* End */
		text_out(".  ");
	}
	
	/* Do we know how aware it is? */
	if ((((int)l_ptr->wake * (int)l_ptr->wake) > r_ptr->sleep) ||
	    (l_ptr->ignore == MAX_UCHAR) || (l_ptr->xtra1) ||
	    ((r_ptr->sleep == 0) && (l_ptr->tkills >= 10)))
	{
		cptr act;

		if (r_ptr->sleep > 200)
		{
			act = "prefers to ignore";
		}
		else if (r_ptr->sleep > 95)
		{
			act = "pays very little attention to";
		}
		else if (r_ptr->sleep > 75)
		{
			act = "pays little attention to";
		}
		else if (r_ptr->sleep > 45)
		{
			act = "tends to overlook";
		}
		else if (r_ptr->sleep > 25)
		{
			act = "takes quite a while to see";
		}
		else if (r_ptr->sleep > 10)
		{
			act = "takes a while to see";
		}
		else if (r_ptr->sleep > 5)
		{
			act = "is fairly observant of";
		}
		else if (r_ptr->sleep > 3)
		{
			act = "is observant of";
		}
		else if (r_ptr->sleep > 1)
		{
			act = "is very observant of";
		}
		else if (r_ptr->sleep > 0)
		{
			act = "is vigilant for";
		}
		else
		{
			act = "is ever vigilant for";
		}

		if (!(r_ptr->flags7 & (RF7_NONMONSTER)))
		{
			text_out(format("%^s %s intruders, which %s may notice from %d feet.  ",
		            wd_he[msex], act, wd_he[msex], 10 * r_ptr->aaf));
		}
	}

	/* Describe escorts */
	if ((l_ptr->flags1 & RF1_ESCORT) || (l_ptr->flags1 & RF1_ESCORTS) || (l_ptr->flags2 & RF2_ESCORT1))
	{
		text_out(format("%^s usually appears with escorts.  ",
		            wd_he[msex]));
	}

	/* Describe friends */
	else if (l_ptr->flags1 & RF1_FRIENDS)
	{
		text_out(format("%^s usually appears in groups.  ",
		            wd_he[msex]));
	}
	/* Describe friend1 */
	else if (l_ptr->flags2 & RF2_FRIEND1)
	{
		text_out(format("%^s usually appears in small groups.  ",
		            wd_he[msex]));
	}	
	/* Describe friend */
	else if (l_ptr->flags1 & RF1_FRIEND)
	{
		text_out(format("%^s usually appears in pairs.  ",
		            wd_he[msex]));
	}
}


static void describe_monster_kills(int r_idx, const monster_lore *l_ptr)
{
	const monster_race *r_ptr = &r_info[r_idx];

	int msex = 0;

	bool out = TRUE;


	/* Extract a gender (if applicable) */
	if (r_ptr->flags1 & RF1_FEMALE) msex = 2;
	else if (r_ptr->flags1 & RF1_MALE) msex = 1;


	/* Treat uniques differently */
	if (l_ptr->flags1 & RF1_UNIQUE)
	{
		/* Hack -- Determine if the unique is "dead" */
		bool dead = (r_ptr->max_num == 0) ? TRUE : FALSE;

		/* We've been killed... */
		if (l_ptr->deaths)
		{
			/* Killed ancestors */
			text_out(format("%^s has slain %d of your ancestors",
			            wd_he[msex], l_ptr->deaths));

			/* But we've also killed it */
			if (dead)
			{
				text_out(", but you have taken revenge!  ");
			}

			/* Unavenged (ever) */
			else
			{
				text_out(format(", who %s unavenged.  ",
				            plural(l_ptr->deaths, "remains", "remain")));
			}
		}

		/* Dead unique who never hurt us */
		else if (dead)
		{
			text_out("You have slain this foe.  ");
		}
		else
		{
			/* Alive and never killed us */
			out = FALSE;
		}
	}

	/* Not unique, but killed us */
	else if (l_ptr->deaths)
	{
		/* Dead ancestors */
		text_out(format("%d of your ancestors %s been killed by this creature, ",
		            l_ptr->deaths, plural(l_ptr->deaths, "has", "have")));

		/* Some kills this life */
		if (l_ptr->pkills)
		{
			text_out(format("and you have exterminated at least %d of the creatures.  ",
			            l_ptr->pkills));
		}

		/* Some kills past lives */
		else if (l_ptr->tkills)
		{
			text_out(format("and %s have exterminated at least %d of the creatures.  ",
			            "your ancestors", l_ptr->tkills));
		}

		/* No kills */
		else
		{
			text_out_c(TERM_RED, format("and %s is not ever known to have been defeated.  ",
			            wd_he[msex]));
		}
	}

	/* Normal monsters */
	else
	{
		/* Killed some this life */
		if (l_ptr->pkills)
		{
			text_out(format("You have killed at least %d of these creatures.  ",
			            l_ptr->pkills));
		}

		/* Killed some last life */
		else if (l_ptr->tkills)
		{
			text_out(format("Your ancestors have killed at least %d of these creatures.  ",
			            l_ptr->tkills));
		}

		/* Killed none */
		else
		{
			text_out("No battles to the death are recalled.  ");
		}
	}

	/* Separate */
	if (out) text_out("\n");
}


static void describe_monster_toughness(int r_idx, const monster_lore *l_ptr)
{
	const monster_race *r_ptr = &r_info[r_idx];

	int msex = 0;


	/* Extract a gender (if applicable) */
	if (r_ptr->flags1 & RF1_FEMALE) msex = 2;
	else if (r_ptr->flags1 & RF1_MALE) msex = 1;
	
	/* Describe monster "toughness" */
	if (know_armour(r_idx, l_ptr))
	{
		/* Armor */
		text_out(format("%^s has an armor rating of %d",
		            wd_he[msex], r_ptr->ac));

		/* Maximized hitpoints */
		if (l_ptr->flags1 & RF1_FORCE_MAXHP)
		{
			text_out(format(" and a life rating of %d.  ",
			            r_ptr->hdice * r_ptr->hside));
		}

		/* Variable hitpoints */
		else
		{
			text_out(format(" and a life rating of %dd%d.  ",
			            r_ptr->hdice, r_ptr->hside));
		}
	}
}


static void describe_monster_exp(int r_idx, const monster_lore *l_ptr)
{
	const monster_race *r_ptr = &r_info[r_idx];
	cptr p, q;
	long i, j;


	/* Describe experience if known */
	if (l_ptr->tkills)
	{
		if (r_ptr->mexp == 0)
		{
			text_out(" There is no reward for killing this monster.  ");
			return;
		}
		/* Introduction */
		if ((r_ptr->flags1 & RF1_UNIQUE) || (r_ptr->maxpop == 1))
			text_out("Killing");
		else
			text_out("A kill of");

			text_out(" this creature");

		/* calculate the integer exp part */
		i = (long)r_ptr->mexp * r_ptr->level / p_ptr->lev;

		/* calculate the fractional exp part scaled by 100, */
		/* must use long arithmetic to avoid overflow */
		j = ((((long)r_ptr->mexp * r_ptr->level % p_ptr->lev) *
			  (long)1000 / p_ptr->lev + 5) / 10);

		/* HELPER monsters and some town monsters have an XP penalty */
        if (r_ptr->mexp < 0)
		{
			i = ((long)r_ptr->mexp * (1 + (p_ptr->lev/8)));

			/* Mention the experience penalty */
			text_out(format(" would inflict a %d-point penalty", i));
		}
		else
		{
			/* Mention the experience */
			text_out(format(" is worth %ld.%02ld point%s",
				        (long)i, (long)j,
				        (((i == 1) && (j == 0)) ? "" : "s")));
		}

		/* Take account of annoying English */
		p = "th";
		i = p_ptr->lev % 10;
		if ((p_ptr->lev / 10) == 1) /* nothing */;
		else if (i == 1) p = "st";
		else if (i == 2) p = "nd";
		else if (i == 3) p = "rd";

		/* Take account of "leading vowels" in numbers */
		q = "";
		i = p_ptr->lev;
		if ((i == 8) || (i == 11) || (i == 18)) q = "n";

		/* Mention the dependance on the player's level */
		text_out(format(" for a%s %lu%s level character.  ",
			        q, (long)i, p));
	}
}


static void describe_monster_movement(int r_idx, const monster_lore *l_ptr, monster_type *m_ptr)
{
	const monster_race *r_ptr = &r_info[r_idx];

	int msex;
	s16b knowevil;
	bool old = FALSE;
	bool evil_individual = FALSE;
	bool evilflag = FALSE;
	/* knowevil: 2=discern evil, 3=discern not evil, 4=detected evil, 6=known evil by cheating, 7=known not evil by cheating */
	if (m_ptr) knowevil = m_ptr->meet;
	else knowevil = 0;
	if ((r_ptr->flags2 & RF2_S_EVIL1) || (r_ptr->flags2 & RF2_S_EVIL2)) evilflag = TRUE;
	if ((r_ptr->flags3 & RF3_EVIL) && (!(l_ptr->flags3 & RF3_EVIL))) evilflag = TRUE;
	if ((r_ptr->flags2 & RF2_STUPID) && (knowevil == 3)) knowevil = 0;

	/* changed from cheat_know to cheat_hear because cheat_know (know_races) */
	/* is no longer considered cheating. So monster-related stuff which */
	/* is considered cheating (or testing) should use cheat_hear. */
    if ((cheat_hear) && (m_ptr))
	{
		int rmspeed;
        text_out(format("This monster has %d hps out of %d max hit points.  ", m_ptr->hp, m_ptr->maxhp));
		/* know evil or not by cheating */
		if ((m_ptr->evil) && (!knowevil)) knowevil = 6;
		else if ((!m_ptr->evil) && (!knowevil) && (evilflag)) knowevil = 7;
		
		rmspeed = m_ptr->mspeed - r_ptr->speed;
		if (rmspeed > 0)
			text_out(format("This monster is %d point faster than average for its race.  ", rmspeed));
		else if (rmspeed < 0)
			text_out(format("This monster is %d point slower than average for its race.  ", ABS(rmspeed)));
		else text_out("This monster is average speed for its race.  ");

#if nolongerneeded
		/* (attempting to test pathfinding) */
		if ((m_ptr->headtoy) && (m_ptr->headtox))
		{
			bool atpc = FALSE;
			text_out(format("This monster's grid location is %d, %d. ", m_ptr->fy, m_ptr->fx));
			text_out(format("This monster's get_moves destination is %d, %d", m_ptr->headtoy, m_ptr->headtox));
			if ((m_ptr->headtoy == p_ptr->py) && (m_ptr->headtox == p_ptr->px))
				{ text_out(" (the PC's location)"); atpc = TRUE; }
			else if (cave_feat[m_ptr->headtoy][m_ptr->headtox] == FEAT_WATER)
				text_out(" (a water-filled grid)");
			else if (m_ptr->roaming) text_out(" (roaming destination)");
			text_out(". ");
			if (!atpc) text_out(format("The PC's grid location is %d, %d. ", p_ptr->py, p_ptr->px));
		}
#endif
	}

    /* most of this description is irrevelent for NONMONSTERs */
    if ((r_ptr->rarity == 0) && (r_ptr->flags7 & (RF7_NONMONSTER))) return;

	/* pointed at an individual monster and recognised as evil */	
	if (((knowevil == 2) || (knowevil == 4) || (knowevil == 6)) && (evilflag))
	{
       evil_individual = TRUE;
       if (knowevil == 2) text_out(" You discern");
       else if (knowevil == 4) text_out(" You have detected");
	   else if (knowevil == 6) text_out(" You know by cheating");
       text_out(" that this individual monster is");
       text_out_c(TERM_L_BLUE, " evil");
       /* occationally evil */
       if (l_ptr->flags2 & RF2_S_EVIL1) text_out(" though this monster race isn't usually evil.  ");
       /* usually evil */
       else if (l_ptr->flags2 & RF2_S_EVIL2) text_out(" as is typical for its race.  ");
       else text_out(".  ");
    }
    else if ((knowevil == 3) || (knowevil == 7))
    {
       if (knowevil == 3) text_out(" You discern");
	   else if (knowevil == 7) text_out(" You know by cheating");
       text_out(" that this individual monster is not evil");
       if ((l_ptr->flags2 & RF2_S_EVIL1) || (l_ptr->flags2 & RF2_S_EVIL2)) text_out(",");
       else text_out(".  ");
       if (l_ptr->flags2 & RF2_S_EVIL1) text_out(" as monsters of its type occationally are.  ");
       else if (l_ptr->flags2 & RF2_S_EVIL2) text_out(" though monsters of its type usually are.  ");
    }
	/* separate individual monster info from race info */
	if ((knowevil) || (cheat_hear)) text_out("\n");

	text_out("This");

	if (r_ptr->mrsize == 1) text_out(" diminutive");
	else if (r_ptr->mrsize == 2) text_out(" tiny");
	else if (r_ptr->mrsize == 3) text_out(" small");
	else if (r_ptr->mrsize == 4) text_out(" human-sized");
	else if (r_ptr->mrsize == 5) text_out(" large");
	else if (r_ptr->mrsize == 6) text_out(" very large");
	else if (r_ptr->mrsize == 7) text_out(" huge");
	else if (r_ptr->mrsize == 8) text_out(" gigantic");

	if ((l_ptr->flags3 & RF3_EVIL) && (r_ptr->flags1 & RF1_UNIQUE)) 
		text_out_c(TERM_L_BLUE, " evil");
	else if (l_ptr->flags3 & RF3_EVIL) text_out_c(TERM_L_BLUE, " inherently evil");
	else if ((knowevil < 2) && (l_ptr->flags2 & RF2_S_EVIL2)) text_out_c(TERM_L_BLUE, " usually evil");
	else if ((knowevil < 2) && (l_ptr->flags2 & RF2_S_EVIL1)) text_out_c(TERM_L_BLUE, " sometimes evil");
	
	if (l_ptr->flags3 & RF3_UNDEAD) text_out_c(TERM_L_BLUE, " undead");
	else if (l_ptr->flags3 & RF3_NON_LIVING) text_out_c(TERM_L_BLUE, " lifeless");
	if (l_ptr->flags3 & RF3_SILVER) text_out_c(TERM_L_BLUE, " silver");
	if (l_ptr->flags3 & RF3_DEMON) text_out_c(TERM_L_BLUE, " demonic");

	if (l_ptr->flags3 & RF3_GIANT) text_out_c(TERM_L_BLUE, " giant");
	else if (l_ptr->flags3 & RF3_TROLL) text_out_c(TERM_L_BLUE, " troll");
	else if (l_ptr->flags3 & RF3_BUG) text_out_c(TERM_L_BLUE, " bug");
	else if (l_ptr->flags3 & RF3_ORC) text_out_c(TERM_L_BLUE, " orc");
	else if (l_ptr->flags3 & RF3_ANIMAL) text_out_c(TERM_L_BLUE, " animal");
	/* light fairy: special case */
	else if ((r_ptr->flags3 & (RF3_FEY)) && (l_ptr->flags3 & (RF3_CLIGHT))) text_out_c(TERM_L_BLUE, " light fairy");
	else if (l_ptr->flags3 & (RF3_CLIGHT)) text_out_c(TERM_L_BLUE, " creature of light");
	else if (l_ptr->flags3 & RF3_DRAGON) text_out_c(TERM_L_BLUE, " dragon");
	else if (l_ptr->flags3 & RF3_UNDEAD) text_out_c(TERM_L_BLUE, " monster");
	else if (l_ptr->flags3 & RF3_DEMON) text_out_c(TERM_L_BLUE, " monster");
	else text_out(" creature");

	if (r_ptr->rarity == 0)
	{
       /* don't bother with depth of something which never appears naturally */
    }
	/* Describe location */
	else if (r_ptr->level == 0)
	{
		text_out_c(TERM_SLATE, " lives in the town");
		old = TRUE;
	}
	/* DJA: depth should always be known */
	else
	{
		if (l_ptr->flags1 & RF1_FORCE_DEPTH)
			text_out_c(TERM_SLATE, " is found ");
		else
			text_out_c(TERM_SLATE, " is normally found ");
		
		if (depth_in_feet)
		{
			text_out_c(TERM_SLATE, format("at depths of %d feet",
			                            r_ptr->level * 50));
		}
		else
		{
			text_out_c(TERM_SLATE, format("on dungeon level %d",
			                            r_ptr->level));
		}
		old = TRUE;
	}

	if (old) text_out(" and");

    if ((r_ptr->flags1 & RF1_UNIQUE) && (r_ptr->flags7 & RF7_THEME_ONLY))
    {
       text_out(" never appears outside of its home.  ");

	   /* Extract a gender (if applicable) */
	   msex = 0;
	   if (r_ptr->flags1 & RF1_FEMALE) msex = 2;
	   else if (r_ptr->flags1 & RF1_MALE) msex = 1;

	   text_out(format("%^s moves", wd_he[msex]));
    }                  
    else if (r_ptr->flags1 & RF1_UNIQUE)
	{
		text_out(" moves");
	}                  
	else 
	{
		/* trees and statues are placed as terrain, so don't comment on rarity */
        if ((r_ptr->rarity == 0) && (r_ptr->flags7 & (RF7_NONMONSTER))) /* nothing */;
		/* all "Helper" monsters have rarity 0 */
		else if (r_ptr->rarity == 0) text_out(" never appears unless summoned.  ");
		/* describe rarity */
		else if (r_ptr->rarity == 1) text_out(" is very common.  ");
		else if (r_ptr->rarity == 2) text_out(" is common.  ");
	   else if ((r_ptr->rarity == 3) || (r_ptr->rarity == 4)) text_out(" is not very common.  ");
	   else if ((r_ptr->rarity > 4) && (r_ptr->rarity <= 6)) text_out(" is rare.  ");
	   else if ((r_ptr->rarity > 6) && (r_ptr->rarity <= 16)) text_out(" is very rare.  ");
	   else if (r_ptr->rarity > 16) text_out(" is extremely rare.  ");
	   /* note about THEME_ONLY and TOWNOK monsters */
		if (r_ptr->flags7 & RF7_TOWNOK)
		{
			text_out("It also may appear in the town ");
			if (r_ptr->level > 10) text_out("(but the town version has a greatly reduced XP reward).  ");
			else text_out("(but the town version isn't worth any experience).  ");
		}
	   if (r_ptr->flags7 & RF7_THEME_ONLY) text_out("It never appears outside of its home.  ");
	
	   msex = 0;

	   /* Extract a gender (if applicable) */
	   if (r_ptr->flags1 & RF1_FEMALE) msex = 2;
	   else if (r_ptr->flags1 & RF1_MALE) msex = 1;

	   text_out(format("%^s moves", wd_he[msex]));
    }	   

 	/* Random-ness */
	if ((l_ptr->flags1 & RF1_RAND_50) || (l_ptr->flags1 & RF1_RAND_25))
	{
		/* Adverb */
		if ((l_ptr->flags1 & RF1_RAND_50) && (l_ptr->flags1 & RF1_RAND_25))
		{
			text_out(" extremely");
		}
		else if (l_ptr->flags1 & RF1_RAND_50)
		{
			text_out(" somewhat");
		}
		else if (l_ptr->flags1 & RF1_RAND_25)
		{
			text_out(" a bit");
		}

		/* Adjective */
		text_out(" erratically");

		/* Hack -- Occasional conjunction */
		if (r_ptr->speed != 110) text_out(", and");
	}

	/* Speed */
	if (r_ptr->speed > 110)
	{
		if (r_ptr->speed > 130) text_out_c(TERM_GREEN, " incredibly");
		else if (r_ptr->speed > 121) text_out_c(TERM_GREEN, " very");
		else if (r_ptr->speed < 113) text_out_c(TERM_GREEN, " slightly");
		else if (r_ptr->speed < 115) text_out_c(TERM_GREEN, " somewhat");
		text_out_c(TERM_GREEN, " quickly");
	}
	else if (r_ptr->speed < 110)
	{
		if (r_ptr->speed < 90) text_out_c(TERM_GREEN, " incredibly");
		else if (r_ptr->speed < 96) text_out_c(TERM_GREEN, " very");
		else if (r_ptr->speed > 107) text_out_c(TERM_GREEN, " slightly");
		else if (r_ptr->speed > 105) text_out_c(TERM_GREEN, " somewhat");
		text_out_c(TERM_GREEN, " slowly");
	}
	else
	{
		text_out_c(TERM_GREEN, " at normal speed");
	}

	/* The code above includes "attack speed" */
	if (l_ptr->flags1 & RF1_NEVER_MOVE)
	{
		text_out(", but does not deign to chase intruders");
	}

	/* End this sentence */
	text_out(".  ");

	if (r_ptr->flags7 & (RF7_NONMONSTER))
	{
		text_out(format("%^s is not hostile.  ", wd_he[msex]));
	}
	/* non aggressive monsters */
	else if (r_ptr->sleep == 255)
	{
		text_out(format("%^s does not become hostile unless attacked or aggravated.  ", wd_he[msex]));
	}

	/* This monster scales with depth */
	if (r_ptr->flags3 & (RF3_SCALE))
	{
		text_out("This monster may become tougher when it appears deeper than its native depth.  ");
	}
}



/*
 * Learn everything about a monster (by cheating)
 */
static void cheat_monster_lore(int r_idx, monster_lore *l_ptr)
{
	const monster_race *r_ptr = &r_info[r_idx];
	int i;

	/* Hack -- Maximal kills */
	l_ptr->tkills = MAX_SHORT;

	/* Hack -- Maximal info */
	l_ptr->wake = l_ptr->ignore = MAX_UCHAR;

	/* Observe "maximal" attacks */
	for (i = 0; i < MONSTER_BLOW_MAX; i++)
	{
		/* Examine "actual" blows */
		if (r_ptr->blow[i].effect || r_ptr->blow[i].method)
		{
			/* Hack -- maximal observations */
			l_ptr->blows[i] = MAX_UCHAR;
		}
	}

	/* Hack -- maximal drops */
	l_ptr->drop_gold = l_ptr->drop_item =
	(((r_ptr->flags1 & RF1_DROP_4D2) ? 8 : 0) +
	 ((r_ptr->flags1 & RF1_DROP_3D2) ? 6 : 0) +
	 ((r_ptr->flags1 & RF1_DROP_2D2) ? 4 : 0) +
	 ((r_ptr->flags1 & RF1_DROP_1D2) ? 2 : 0) +
	 ((r_ptr->flags1 & RF1_DROP_90)  ? 1 : 0) +
	 ((r_ptr->flags1 & RF1_DROP_30)  ? 1 : 0) +
	 ((r_ptr->flags1 & RF1_DROP_60)  ? 1 : 0));

	/* Hack -- but only "valid" drops */
	if (r_ptr->flags1 & RF1_ONLY_GOLD) l_ptr->drop_item = 0;
	if (r_ptr->flags1 & RF1_ONLY_ITEM) l_ptr->drop_gold = 0;

	/* Hack -- observe many spells */
	l_ptr->cast_innate = MAX_UCHAR;
	l_ptr->cast_spell = MAX_UCHAR;

	/* Hack -- know all the flags */
	l_ptr->flags1 = r_ptr->flags1;
	l_ptr->flags2 = r_ptr->flags2;
	l_ptr->flags3 = r_ptr->flags3;
	l_ptr->flags4 = r_ptr->flags4;
	l_ptr->flags5 = r_ptr->flags5;
	l_ptr->flags6 = r_ptr->flags6;

	/* know all resistances */
	l_ptr->know_MRfire = 1;
	l_ptr->know_MRcold = 1;
	l_ptr->know_MRelec = 1;
	l_ptr->know_MRacid = 1;
	l_ptr->know_MRpois = 1;
	l_ptr->know_MRlite = 1;
	l_ptr->know_MRdark = 1;
	l_ptr->know_MRwatr = 1;
	l_ptr->know_MRnexu = 1;
	l_ptr->know_MRmisl = 1;
	l_ptr->know_MRchao = 1;
	l_ptr->know_MRdise = 1;
	l_ptr->know_MRsilv = 1;
	l_ptr->know_MRtame = 1;
	l_ptr->know_R4latr = 1;
	l_ptr->know_R4lat2 = 1;
}


/*
 * Hack -- display monster information using "roff()"
 *
 * Note that there is now a compiler option to only read the monster
 * descriptions from the raw file when they are actually needed, which
 * saves about 60K of memory at the cost of disk access during monster
 * recall, which is optional to the user.
 *
 * This function should only be called with the cursor placed at the
 * left edge of the screen, on a cleared line, in which the recall is
 * to take place.  One extra blank line is left after the recall.
 */
void describe_monster(int r_idx, bool spoilers, monster_type *m_ptr)
{
	monster_lore lore;

	/* Get the race and lore */
	const monster_race *r_ptr = &r_info[r_idx];
	monster_lore *l_ptr = &l_list[r_idx];

	/* Hack -- create a copy of the monster-memory */
	COPY(&lore, l_ptr, monster_lore);

	/* Assume some "obvious" flags */
	lore.flags1 |= (r_ptr->flags1 & RF1_OBVIOUS_MASK);

	/* Killing a monster reveals some properties */
	if ((lore.tkills) || (l_ptr->xtra1))
	{
		/* Know "race" flags */
		lore.flags2 |= (r_ptr->flags2 & RF2_RACE_MASK);
		lore.flags3 |= (r_ptr->flags3 & RF3_RACE_MASK);

		/* Know "forced" flags */
		lore.flags1 |= (r_ptr->flags1 & (RF1_FORCE_DEPTH | RF1_FORCE_MAXHP));
	}

	/* Cheat -- know everything */
	if (know_races || spoilers) cheat_monster_lore(r_idx, &lore);

	/* Show kills of monster vs. player(s) */
	if ((!spoilers) && (!(r_ptr->flags7 & (RF7_NONMONSTER)))) 
		describe_monster_kills(r_idx, &lore);

	/* Monster description */
	describe_monster_desc(r_idx, m_ptr);

	/* Describe the race, movement and level of the monster */
	/* sometimes include stuff about the individual monster */
	describe_monster_movement(r_idx, &lore, m_ptr);

	/* Describe experience */
	if (!spoilers) describe_monster_exp(r_idx, &lore);
	
	if (!(r_ptr->flags7 & (RF7_NONMONSTER)))
	{
		/* Describe spells and innate attacks */
		describe_monster_spells(r_idx, &lore);
	}

	/* Describe monster "toughness" */
	if (!spoilers) describe_monster_toughness(r_idx, &lore);

	/* Describe the abilities of the monster */
	describe_monster_abilities(r_idx, &lore);

	/* Describe the monster drop */
	describe_monster_drop(r_idx, &lore);

	/* Describe the known attacks */
	describe_monster_attack(r_idx, &lore);

	/* Notice "Quest" monsters */
	if (lore.flags1 & RF1_QUESTOR)
	{
		text_out("You feel an intense desire to kill this monster...  ");
	}

	/* All done */
	text_out("\n");
}





/*
 * Hack -- Display the "name" and "attr/chars" of a monster race
 */
void roff_top(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	byte a1, a2;
	char c1, c2;

	/* Get the chars */
	c1 = r_ptr->d_char;
	c2 = r_ptr->x_char;

	/* Get the attrs */
	a1 = r_ptr->d_attr;
	a2 = r_ptr->x_attr;


	/* Clear the top line */
	Term_erase(0, 0, 255);

	/* Reset the cursor */
	Term_gotoxy(0, 0);

	/* A title (use "The" for non-uniques) */
	if (!((r_ptr->flags1 & RF1_UNIQUE) || (r_ptr->maxpop == 1)))
	{
		Term_addstr(-1, TERM_WHITE, "The ");
	}

	/* Dump the name */
	Term_addstr(-1, TERM_WHITE, (r_name + r_ptr->name));

	/* Append the "standard" attr/char info */
	Term_addstr(-1, TERM_WHITE, " ('");
	Term_addch(a1, c1);
	Term_addstr(-1, TERM_WHITE, "')");

	/* Append the "optional" attr/char info */
	Term_addstr(-1, TERM_WHITE, "/('");
	Term_addch(a2, c2);
	if (use_bigtile && (a2 & 0x80)) Term_addch(255, -1);
	Term_addstr(-1, TERM_WHITE, "'):");
}

/*
 * Hack -- describe the given monster race at the top of the screen
 */
void screen_roff(int r_idx, monster_type *m_ptr)
{
	/* Flush messages */
	message_flush();

	/* Begin recall */
	Term_erase(0, 1, 255);

	/* Output to the screen */
	text_out_hook = text_out_to_screen;

	/* Recall monster */
	describe_monster(r_idx, FALSE, m_ptr);

	/* Describe monster */
	roff_top(r_idx);
}




/*
 * Hack -- describe the given monster race in the current "term" window
 */
void display_roff(int r_idx)
{
	int y;

	/* Erase the window */
	for (y = 0; y < Term->hgt; y++)
	{
		/* Erase the line */
		Term_erase(0, y, 255);
	}

	/* Begin recall */
	Term_gotoxy(0, 1);

	/* Output to the screen */
	text_out_hook = text_out_to_screen;

	/* Recall monster */
	describe_monster(r_idx, FALSE, 0);

	/* Describe monster */
	roff_top(r_idx);
}
