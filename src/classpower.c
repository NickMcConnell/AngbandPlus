
/* File: classpower.c */
/* Purpose: Special Class abilities code */

/*
 *
 * Classpower.c is your egress to excitement wherein information and powers
 * that are inherint to a variety of classes reside. Here you will discover 
 * code for displaying and picking those powers based off your skill level, 
 * The actual activation of the powers and claims to soon display extra 
 * information about those powers.
 *
 * Copyright (c) 2002 Courtney Campbell
 *
 * This software may be copied and distributed for educational, research, 
 * and not for profit purposes provided that this copyright and statement 
 * are included in all such copies.
 *
 * Large sections of this code have been copied almost verbatim from other
 * variants and then altered. 
 *
 * For a list of variants that I stole from, check the info that came with 
 * the source. I do not take credit for coming up with this code myself.
 * This block of code came from Zangband orginally (i think).
 *
 * I come so far only because I stand on the shoulders of giants. -CCC
 *
 * I've made some _exteremly_ IMHO hackish alterations to this code. . . 
 * There are 3 sections to each of the character's spell lists that can
 * be based off different skills.
 * 
 * They can put points into any skill and get the powers for that skill.
 * I imagine a nice non-hackish way to do this is to use a linked list 
 * as the powers are opened up. Perhaps this should eventually be done,
 * this may eventually allow all skill based powers to be opened in any
 * order - however for the moment it is beyond my ability.
 *
 * The way I did it is defineing the limits as CLASS_POWER_BREAK_ONE and 
 * CLASS_POWER_BREAK_TWO, in addition to MAX_CLASS_POWERS, and running
 * seperate for loops for display, and checking if spells are valid,
 * then keeping track  of the number of times the loop runs using local
 * variables, and _then_ I determined whether or not the players choice
 * is greater or fewer than the number of the times the loops have ran,
 * and adding / subtracting  the proper amount to hit the correct index
 * in the spell array.
 *
 * Seems hackish to me.
 *
 * Antimatter, (RML) says that it's not a hack, but uh. . . I don't 
 * know that I buy that. He says he does stuff like this all the time.
 * That my be true, but it seems like no less a hack. Antimatter is a
 * swell guy in IRL, by the way.
 */
 
#include "angband.h"
 
mind_power mind_powers[10] =
{
	{
		{
			/* Officer Powers */
			/* Level gained,  cost,  %fail,  name */
			{ 2,   5,  10, "Sense Recruits"},
			{ 4,   5,  20, "Inspire Fear"},
			{ 6,   8,  30, "Command"},
			{ 9,  15,  35, "Enlist"},
			{ 14, 30,  45, "Evaluate Opponent"},
			{ 20, 40,  99, "Mass Enlistment"},
			{ 99,  0,   0, ""},
			/* --- */
			{ 4,  10,  10, "Stunning blow"},
			{ 8,  20,  45, "Stiff Upper Lip"},
			{ 12,  7,  25, "Counter-Strike"},
			{ 15, 50,  80, "Defensive Techniques"},
			{ 18, 35,  50, "Blasting Strike"},
			{ 20, 70,  60, "Critical Strike"},
			/* --- */
			{ 99,  0,   0, ""},
			{ 99,  0,   0, ""},
			{ 99,  0,   0, ""},
			{ 99,  0,   0, ""},
			{ 99,  0,   0, ""},
			{ 99,  0,   0, ""},
			{ 99,  0,   0, ""},
			{ 99,  0,   0, ""},
		}
	},

	{
		{
			/* Aesthete Powers */
			/* Level gained,  cost,  %fail,  name */
			{ 1,   1,  10, "Arrogant Laugh"},
			{ 4,   2,  10, "Treasure Sense"},
			{ 10, 10,  40, "Legendary Lore"},
			{ 12,  5,  30, "Duck out the Back"},
			{ 18, 40,  60, "Touch of Fortune"},
			{ 20, 20,  60, "Merchant's Trail"},
			{ 99,  0,   0, ""},
			/* --- */
			{ 2,   8,  65, "Enhance Weapon"},
			{ 6,  12,  65, "Enhance Armor"},
			{ 14, 30,  60, "Item Overhaul"},
			{ 19, 40,  55, "Brand Weapon"},
			{ 99,  0,   0, ""},
			{ 99,  0,   0, ""},
			/* --- */
			{ 99,  0,   0, ""},
			{ 99,  0,   0, ""},
			{ 99,  0,   0, ""},
			{ 99,  0,   0, ""},
			{ 99,  0,   0, ""},
			{ 99,  0,   0, ""},
			{ 99,  0,   0, ""},
			{ 99,  0,   0, ""},
		}
	},

	{
		{
			/* Explorer Powers */
			/* Level gained,  cost,  %fail,  name */
			{ 1,  1,   35, "Light area"},
			{ 4,  3,   30, "Detection"},
			{ 6,  5,   25, "Portal"},
			{ 9,  10,  20, "Satisfy Hunger"},
			{ 13, 20,  40, "Identify"},
			{ 17, 25,  35, "Word of Recall"},
			{ 20, 60,  55, "Recharge Item"},
			/* --- */
			{ 99,  0,   0, ""},
			{ 99,  0,   0, ""},
			{ 99,  0,   0, ""},
			{ 99,  0,   0, ""},
			{ 99,  0,   0, ""},
			{ 99,  0,   0, ""},
			/* --- */
			{ 99,  0,   0, ""},
			{ 99,  0,   0, ""},
			{ 99,  0,   0, ""},
			{ 99,  0,   0, ""},
			{ 99,  0,   0, ""},
			{ 99,  0,   0, ""},
			{ 99,  0,   0, ""},
			{ 99,  0,   0, ""},
		}
	},

	{
		{
			/* Medium Powers */
			/* Level gained,  cost,  %fail,  name */
			{1,   1, 15, "Neural Blast"},
			{6,  12, 50, "Psychic Disturbance"},
			{8,  10, 30, "Spirit Blast"},
			{12, 15, 50, "Domination"},
			{14, 20, 45, "Soul Purge"},
			{18, 10, 40, "Psychic Drain"},
			{20, 40, 45, "Entropic blast"},
			/* --- */
			{ 2,   1,  20, "Precognition"},
			{ 10, 12,  60, "Psychometry"},
			{ 99,  0,   0, ""},
			{ 99,  0,   0, ""},
			{ 99,  0,   0, ""},
			{ 99,  0,   0, ""},
			/* --- */
			{  2,  2,   0,  "Healing"},
			{  4,  2,  25, "Minor Displacement"},
			{  9, 12,  50, "Character Armour"},
			{ 11,  6,  35, "Major Displacement"},
			{ 13, 15,  50, "Drawing upon the Sprits"},
			{ 20, 50,  50,  "Body Dispersion"},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
		}
	},

	{
		{
			/* xxx Powers */
			/* Level gained,  cost,  %fail,  name */
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			/* --- */
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			/* --- */
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
		}
	},
{
	{
			/* Level gained,  cost,  %fail,  name */
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			/* --- */
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			/* --- */
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
		}
	},
	{
		{
			/* Hussar combat powers */
			/* Level gained,  cost,  %fail,  name */
			{  2,  3,  15, "Battle Preperation"},
			{  4,  5,  12, "Three-Way Strike"},
			{  5,  8,  18, "Resistant Stance"},
			{  8, 15,  10, "Stunning Blow"},
			{ 12, 20,  30, "Defensive Stance"},
			{ 18, 30,  50, "Rush Attack"},
			{ 20, 50,  70, "Blasting Strike"},
			/* --- */
			{ 1,  25,  25, "Critical Strike"},
			{ 4,  10,  10, "Flanking Maneuver"},
			{ 8,  15,  20, "Triple Strike"},
			{ 12,  0,   0, "Gun Kata"},
			{ 16, 80,  25, "Charge!"},
			{ 20, 40,  45, "Lightning Strike"},
			/* --- */
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
		}
	},
	{
		{
			/* Naturalist powers */
			/* Level gained,  cost,  %fail,  name */
			{ 1,   6,   25, "Woodland Sanctuary"},
			{ 4,   2,   20, "Detect Doors + Traps"},
			{ 8,   3,   30, "Daylight"},
			{ 10,  4,   33, "Stone to Mud"},
			{ 11, 10,   43, "Entangle"},
			{ 13, 11,   50, "Resist Environment"},
			{ 99, 99,   99, "???"},
			/* --- */
			{ 1,   1,   10, "Detect Creatures"},
			{ 4,   18,  15, "Summon Animal"},
			{ 8,   10,  45, "Animal Taming"},
			{ 12,  38,  50, "Major Summon Animal"},
			{ 16,  20,  60, "Stoneskin"},
			{ 19,  50,  70, "Summon Elemental"},
			/* --- */
			{ 2,   2,  15, "Dancing Flame"},
			{ 4,   4,  45, "Lightning Bolt"},
			{ 7,   24, 65, "Blizzard"},
			{ 10,  35, 70, "Earthquake"},
			{ 13,  85, 65, "Lightning Storm"},
			{ 15,  80, 65, "Whirlpool"},
			{ 17,  60, 70, "Lord of the Forest"},
			{ 20,  50, 70, "Nature's Wrath"},
		}
	},
	{
		{
			/* Ninjitsu powers - from hengband */
			/* Level gained,  cost,  %fail,  name */
			/* --- */
			{ 3,  4,  20, "Distraction"},
			{ 4,  6,  25, "Bind monster"},
			{ 5,  7,  25, "Smoke Bomb"},
			{ 10, 14,  35, "Swap Position"},
			{ 12, 12,  35, "Poison Bomb"},
			{ 18, 30,  40, "Passwall"},
			{ 20, 50,  50, "Shadow Travel"},
			/* --- */
			{ 1,   3,  8, "Quick Escape"},
			{ 2,   5,  20, "Detect Near"},
			{ 5,   7,  30, "Absconding"},
			{ 11, 25,  70, "Ancient Knowledge"},
			{ 20, 60,  55, "Invisiblity"},
			{ 99, 99,  99, ""},
			/* --- */
			{ 1,   1,   5, "Counterstrike"},
			{ 4,   2,  10, "Strike and Away"},
			{ 6,   3,  15, "Dash"},
			{ 9,   4,  20, "Step in and Strike"},
			{ 13,  8,  25, "Strikethrough"},
			{ 15, 12,  30, "Chain Hook"},
			{ 19, 35,  35, "Knife Kata"},
			{ 20, 28,  40, "Death Dance"},
		}
	},
	{
		{
			/* Quabalist powers */
			/* Level gained,  cost,  %fail,  name */
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			/* --- */
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			/* --- */
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
			{ 99,  0,   0,  ""},
		}
	},
};

/* This function could be _much_ _*MUCH*_ more informative */
/* And indeed breaks come after case 6: and case 12: */
void mindcraft_info(char *p, int use_mind, int power)
{
	/* going to remove this and add in skills */
	int plev = p_ptr->lev;
	
	int officer2, aesthete, explorer, telepathy, mesmerwill, psychometabolism;
	int combattech, floral, fauna, elemental, ninjamagic, ninjastealth;

	/* paranoia */
	officer2 = aesthete = explorer = telepathy = mesmerwill = psychometabolism = 0;
	combattech = floral = fauna = elemental = ninjamagic = ninjastealth = 0;
	
	if (p_ptr->skills[SK_OFFICER2].skill_max > 0)
		officer2 = p_ptr->skills[SK_OFFICER2].skill_rank;
	if (p_ptr->skills[SK_AESTHETE].skill_max > 0)
		aesthete = p_ptr->skills[SK_AESTHETE].skill_rank;
	if (p_ptr->skills[SK_EXPLORER].skill_max > 0)
		explorer = p_ptr->skills[SK_EXPLORER].skill_rank;
	if (p_ptr->skills[SK_TELEPATHY].skill_max > 0)
		telepathy = p_ptr->skills[SK_TELEPATHY].skill_rank;
	if (p_ptr->skills[SK_MESMERIC_WILL].skill_max > 0)
		mesmerwill = p_ptr->skills[SK_MESMERIC_WILL].skill_rank;
	if (p_ptr->skills[SK_PSYCHOMETABOLISM].skill_max > 0)
		psychometabolism = p_ptr->skills[SK_PSYCHOMETABOLISM].skill_rank;
	if (p_ptr->skills[SK_COMBATTECHNIQUES].skill_max > 0)
		combattech = p_ptr->skills[SK_COMBATTECHNIQUES].skill_rank;
	if (p_ptr->skills[SK_FLORAL].skill_max > 0)
		floral = p_ptr->skills[SK_FLORAL].skill_rank;
	if (p_ptr->skills[SK_FAUNA].skill_max > 0)
		fauna = p_ptr->skills[SK_FAUNA].skill_rank;
	if (p_ptr->skills[SK_ELEMENTAL].skill_max > 0)
		elemental = p_ptr->skills[SK_ELEMENTAL].skill_rank;
	if (p_ptr->skills[SK_NINJAMAGIC].skill_max > 0)
		ninjamagic = p_ptr->skills[SK_NINJAMAGIC].skill_rank;
	if (p_ptr->skills[SK_NINJASTEALTH].skill_max > 0)
		ninjastealth = p_ptr->skills[SK_NINJASTEALTH].skill_rank;

	strcpy(p, "");

	switch (use_mind)
	{
		case MIND_OFFICER:		
		switch (power)
			{
				case 0:  strcpy(p, " detect monsters"); break;
				case 1:  strcpy(p, " fear & confusion"); break;
				case 2:  strcpy(p, " stun monsters"); break;
				case 3:  strcpy(p, " charm monsters"); break;
				case 4:  strcpy(p, " probing"); break;
				case 5:  strcpy(p, " mass charm"); break;
				case 6:  break;
				/* As so. . . */
				case 7:  strcpy(p, " hit with added stun"); break;
				case 8:  sprintf(p, " heal %dd%d", officer2 / 2, 
									(officer2 + (p_ptr->lev / 3))); break;
				case 9:  strcpy(p, " hit attackers"); break;
				case 10: sprintf(p, " prot. %d + 1d25", 
									(6 * officer2));  break;
				case 11: strcpy(p, " knockback"); break;
				case 12: strcpy(p, " critical strike"); break;
				/* And so. */
				case 13: break;
				case 14: break;
				case 15: break;
				case 16: break;
				case 17: break;
				case 18: break;
				case 19: break;
				case 20: break;
			}
		break;
		case MIND_AESTHETE:				
		switch (power)
			{
				case 0:  strcpy(p, " fear or aggravates"); break;
				case 1:  strcpy(p, " treasure sense"); break;
				case 2:  strcpy(p, " identify");  break;
				case 3:  sprintf(p, " range %d", (aesthete * 2)); break;
				case 4:  strcpy(p, " midas touch"); break;
				case 5:  strcpy(p, " recall"); break;
				case 6:  break;
				/* As so. . . */
				case 7:  strcpy(p, " enhance weapon"); break;
				case 8:  strcpy(p, " enhance armor"); break;
				case 9:  strcpy(p, " recharge"); break;
				case 10: strcpy(p, " brand weapon"); break;
				case 11: break;
				case 12: break;
				/* And so. */
				case 13: break;
				case 14: break;
				case 15: break;
				case 16: break;
				case 17: break;
				case 18: break;
				case 19: break;
				case 20: break;

			}
		break;
		case MIND_EXPLORER:	
		switch (power)
			{
				case 0:  strcpy(p, " light"); break;
				case 1:  strcpy(p, " detection"); break;
				case 2:  sprintf(p, " range %d", (explorer * 3)); break;
				case 3:  strcpy(p, " satisfy hunger");  break;
				case 4:  strcpy(p, " identify"); break;
				case 5:  strcpy(p, " recall"); break;
				case 6:  strcpy(p, " recharge"); break;
				case 7:  break;
				case 8:  break;
				case 9:  break;
				case 10: break;
				case 11: break;
				case 12: break;
				case 13: break;
				case 14: break;
				case 15: break;
				case 16: break;
				case 17: break;
				case 18: break;
				case 19: break;
				case 20: break;
			}
		break;
		case MIND_MEDIUM: 	
		switch (power)
			{
				case 0:  sprintf(p, " dam %dd%d", 5 + (telepathy / 2), 
									(3 + (telepathy / 6)));  break;
				case 1:  sprintf(p, " dam %dd%d", 1 + (telepathy / 2), 
									(telepathy / 3));break;
				case 2:  sprintf(p, " dam %dd8", telepathy); break;
				case 3:  strcpy(p, " dominate"); break;
				case 4:  strcpy(p, " spirit"); break;
				case 5:  strcpy(p, " slow"); break;
				case 6:  sprintf(p, " dam %d", 
									telepathy * (mesmerwill / 3)); break;
				/* Break one */
				case 7:  strcpy(p, " detect");break;
				case 8:  strcpy(p, " identify"); break;
				case 9:  break;
				case 10: break;
				case 11: break;
				case 12: break;
				case 13: sprintf(p, " heal 1-%d", 
									(psychometabolism + mesmerwill) * 2); break;
				case 14: sprintf(p, " range %d", 
									psychometabolism); break;
				case 15: sprintf(p, " dur 1-%d", 
									(psychometabolism + mesmerwill) * 2); break;
				case 16: sprintf(p, " range %d", 
									psychometabolism * 10); break;
				case 17: sprintf(p, " dur 11-%d", 
									(psychometabolism + mesmerwill) * 2);break;
				case 18: strcpy(p, " wraithform dur 3-8"); ;break;
				case 19: break;
				case 20: break;

			}
		break;
		case MIND_RECKONER: 	
		switch (power)
			{
				case 0:  break;
				case 1:  break;
				case 2:  break;
				case 3:  break;
				case 4:  break;
				case 5:  break;
				case 6:  break;
				case 7:  break;
				case 8:  break;
				case 9:  break;
				case 10: break;
				case 11: break;
				case 12: break;
				case 13: break;
				case 14: break;
				case 15: break;
				case 16: break;
				case 17: break;
				case 18: break;
				case 19: break;
				case 20: break;
			}
		break;
		case MIND_TOURIST: 	
		switch (power)
			{
				case 0:  break;
				case 1:  break;
				case 2:  break;
				case 3:  break;
				case 4:  break;
				case 5:  break;
				case 6:  break;
				case 7:  break;
				case 8:  break;
				case 9:  break;
				case 10: break;
				case 11: break;
				case 12: break;
				case 13: break;
				case 14: break;
				case 15: break;
				case 16: break;
				case 17: break;
				case 18: break;
				case 19: break;
				case 20: break;
			}
		break;	
		case MIND_HUSSAR: 	
		switch (power)
			{
				case 0:  sprintf(p, " heroism %dd3", (combattech)); break;
				case 1:  strcpy(p, " attack three enemies"); break;
				case 2:  sprintf(p, " low resist %d-%d turns",
								 	combattech, (combattech * 3)); break;
				case 3:  strcpy(p, " stun attack"); break;
				case 4:  sprintf(p, " shield %d-%d turns", 
									combattech, (combattech * 3)); break;
				case 5:  strcpy(p, " step, attack"); break;
				case 6:  strcpy(p, " knock-back"); break;
				/* First */
				case 7:  strcpy(p, " critical strike"); break;
				case 8:  strcpy(p, " sidestep, strike"); break;
				case 9:  strcpy(p, " strike three times"); break;
				case 10: strcpy(p, " gunkata "); break;
				case 11: strcpy(p, " charge"); break;
				case 12: strcpy(p, " step, strike, return"); break;
				/* second */
				case 13: break;
				case 14: break;
				case 15: break;
				case 16: break;
				case 17: break;
				case 18: break;
				case 19: break;
				case 20: break;
			}
		break;
		case MIND_NATURE: 	
		switch (power)
			{
				case 0:  sprintf(p, " heal %dd8 + cure/food", 
									floral * 3); break;
				case 1:  strcpy(p, " detect doors/traps"); break;
				case 2:  sprintf(p, " dam 2d%d", floral); break;
				case 3:  strcpy(p, " stone to mud"); break;
				case 4:  strcpy(p, " slow"); break;
				case 5:  sprintf(p, " resist %d-%d", 
									floral, floral * 2); break;
				case 6:  strcpy(p, " unknown power"); break;
				case 7:  strcpy(p, " detect creatures");  break;
				case 8:  strcpy(p, " summon animal"); break;
				case 9:  strcpy(p, " charm animal"); break;
				case 10: strcpy(p, " summon animal"); break;
				case 11: sprintf(p, " shield 30-%d", 
									(fauna + 30)); break;
				case 12: strcpy(p, " summon elemental"); break;
				case 13: sprintf(p, " damage %dd%d", 
									(1 + (elemental / 5)), (elemental / 2));  break;
				case 14: sprintf(p, " damage %dd8", 
									(3 + (elemental / 3))); break;
				case 15: sprintf(p, " damage %dd%d", 
									(elemental / 5), ((elemental / 6) + 1)); break;
				case 16: strcpy(p, " earthquake");   break;
				case 17: sprintf(p, " damage 4d%d", 
									(elemental * 3)); break;
				case 18: sprintf(p, " damage %dd%d", 
									(elemental / 2), (elemental * 2)); break;
				case 19: strcpy(p, " 31-50 dur"); break;
				case 20: strcpy(p, " natures wrath"); break;
			}	
		break;
		case MIND_NINJA: 	
		switch (power)
			{
				case 0:  strcpy(p, " invisiblity 1-3 dur"); break;
				case 1:  strcpy(p, " bind monster"); break;
				case 2:  strcpy(p, " confusion ball"); break;
				case 3:  strcpy(p, " swap w/monster"); break;
				case 4:  strcpy(p, " poison ball"); break;
				case 5:  strcpy(p, " passwall 5-8 dur"); break;
				case 6:  sprintf(p, " teleport range %d", 
									(ninjamagic * 2)); break;
				case 7:  strcpy(p, " range 10"); break;
				case 8:  strcpy(p, " detection"); break;
				case 9:  strcpy(p, " range 40"); break;
				case 10: strcpy(p, " identify"); break;
				case 11: sprintf(p, " invisiblity dur 5-%d", 
										(4 + ninjastealth)); break;
				case 12: /* nothing */break;
				case 13: strcpy(p, " counterstrike"); break;
				case 14: strcpy(p, " hit & step back"); break;
				case 15: strcpy(p, " run 4 squares"); break;
				case 16: strcpy(p, " step in & strike"); break;
				case 17: strcpy(p, " strike, flank, strike"); break;
				case 18: strcpy(p, " drag monster back"); break;
				case 19: strcpy(p, " strike 24 times"); break;
				case 20: break;
			}
		break;
		case MIND_QABALIST: 	
		switch (power)
			{
				case 0:  break;
				case 1:  break;
				case 2:  break;
				case 3:  break;
				case 4:  break;
				case 5:  break;
				case 6:  break;
				case 7:  break;
				case 8:  break;
				case 9:  break;
				case 10: break;
				case 11: break;
				case 12: break;
				case 13: break;
				case 14: break;
				case 15: break;
				case 16: break;
				case 17: break;
				case 18: break;
				case 19: break;
				case 20: break;
			}
		break;
	}
	
}
	
/*
 * Allow user to choose a class power.
 *
 * If a valid spell is chosen, saves it in '*sn' and returns TRUE
 * If the user hits escape, returns FALSE, and set '*sn' to -1
 * If there are no legal choices, returns FALSE, and sets '*sn' to -2
 *
 * The "prompt" should be "cast", "recite", or "study"
 * The "known" should be TRUE for cast/pray, FALSE for study
 *
 * making changes for skills -CCC
 * from the maximum number of skills
 */
 

static int get_class_power(int *sn, int *chnc)
{
	int   i;
	int	  j = 0;
	int   num = 0;
	int   num2 = 0;
	int   y = 1;
	int   x = 10;
	int   minfail =0;
	byte  skill[3];
	int	  chance[MAX_CLASS_POWERS]; 
	int   ask;
	int   use_mind;
	char  choice;
	char  out_val[160];
	char  comment[80];
	cptr  p;

	bool named = FALSE;
	
	mind_type  spell;
	mind_power *mind_ptr;
	bool       flag, redraw;

	/* The class determines the skill that allows casting */
	if (p_ptr->pclass == CLASS_OFFICER)
	{
		use_mind = MIND_OFFICER;
		p = "presence";
		skill[0] = SK_OFFICER;
		skill[1] = SK_OFFICER2;
		skill[2] = 0;
	}
	else if (p_ptr->pclass == CLASS_AESTHETE)
	{
		use_mind = MIND_AESTHETE;
		p = "skill";
		skill[0] = SK_AESTHETE;
		skill[1] = SK_AESTHETE2;
		skill[2] = 0;
	}
	else if ((p_ptr->pclass == CLASS_ADVENTURER) && 
				(p_ptr->skills[SK_EXPLORER].skill_max > 0))
	{
		use_mind = MIND_EXPLORER;
		p = "talent";
		skill[0] = SK_EXPLORER;
		skill[1] = 0;
		skill[2] = 0;
	}
	else if (p_ptr->pclass == CLASS_MEDIUM)
	{
		use_mind = MIND_MEDIUM;
		p = "mental power";
		skill[0] = SK_TELEPATHY;
		skill[1] = SK_CLAIRSENTIENCE;
		skill[2] = SK_PSYCHOMETABOLISM;
	}
	else if (p_ptr->pclass == CLASS_DASHING_H)
	{
		use_mind = MIND_HUSSAR;
		p = "combat tech";
		skill[0] = SK_COMBATTECHNIQUES;
		skill[1] = SK_ELITEMANEUVERS;
		skill[2] = 0;
	}
	else if (p_ptr->pclass == CLASS_NATURAL)
	{
		use_mind = MIND_NATURE;
		p = "natural empathy";
		skill[0] = SK_FLORAL;
		skill[1] = SK_FAUNA;
		skill[2] = SK_ELEMENTAL;
	}
	else if ((p_ptr->pclass == CLASS_ROGUE) && 
				(p_ptr->skills[SK_NINJUTSU].skill_rank > 0))
	{
		use_mind = MIND_NINJA;
		p = "ninjutsu";
		skill[0] = SK_NINJAMAGIC;
		skill[1] = SK_NINJASTEALTH;
		skill[2] = SK_NINJUTSU;
	}
	else if ((p_ptr->pclass == CLASS_GENTLEMAN) &&
				(1 == 0))
	{
		use_mind = MIND_QABALIST;
		p = "Qabala";
		skill[0] = SK_HEBREW;
		skill[1] = 0;
		skill[2] = 0;
	}
	else
	{
		msg_print("You have no powers.");
		return (FALSE);
	}


	mind_ptr = &mind_powers[use_mind];

	/* Assume cancelled */
	*sn = (-1);
	*chnc = 0;

#ifdef ALLOW_REPEAT /* TNB */

	/* Get the spell, if available */
	if (repeat_pull(sn))
	{
		/* Verify the spell */
		if (mind_ptr->info[*sn].min_lev <= p_ptr->skills[skill[0]].skill_rank)
		{
			/* Success */
			return (TRUE);
		}
	}

#endif /* ALLOW_REPEAT -- TNB */

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	j = 0;
	for (i = 0; i < MAX_CLASS_POWERS; i++)
	{

		if (i == CLASS_POWER_BREAK_ONE || i == CLASS_POWER_BREAK_TWO)
			j++;
		if (mind_ptr->info[i].min_lev <= p_ptr->skills[skill[j]].skill_rank)
			num = i; 
	
		spell = mind_ptr->info[i];
	
		chance[i] = spell.fail;

		chance[i] -= 3 * (p_ptr->skills[skill[j]].skill_rank - spell.min_lev);

		/* Reduce failure rate by INT/WIS adjustment */
		chance[i] -= 3 * (p_ptr->stat_use[cp_ptr->spell_stat] / 50);

		/* Not enough mana to cast */
		if (spell.mana_cost > p_ptr->csp)
		{
			chance[i] += 5 * (spell.mana_cost - p_ptr->csp);
		}

		/* Extract the minimum failure rate */
		minfail = 15 - p_ptr->stat_use[cp_ptr->spell_stat] / 40;
		if (minfail < 0) minfail = 0;

		/* Minimum failure rate */
		if (chance[i] < minfail) chance[i] = minfail;

		/* Stunning makes spells harder */
		if (p_ptr->stun > 50) chance[i] += 25;
		else if (p_ptr->stun) chance[i] += 15;

		/* Always a 5 percent chance of working */
		if (chance[i] > 95) chance[i] = 95;
	}
	j = 0;

	/* Build a prompt (accept all spells) */
	(void)strnfmt(out_val, 78, "(%^ss %c-%c, *=List, @=by name, ESC=exit) Use which? ",
			p, I2A(0), I2A(num));
	/* Get a spell from the user */
	while (!flag && get_com(out_val, &choice))
	{
		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Show the list */
			if (!redraw)
			{
				char psi_desc[80];
				
				num2 = 0;
				
				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				screen_save();
				
				prt("", y, x);
				put_str("Name", y, x + 4);

				put_str(format("Rank   SP Fail Info"), y, x + 34);
			
				j = 0;
				for (i = 0; i < MAX_CLASS_POWERS; i++)
				{
					if (i == CLASS_POWER_BREAK_ONE || i == CLASS_POWER_BREAK_TWO)
						j++;

					spell = mind_ptr->info[i];
					if (spell.min_lev > p_ptr->skills[skill[j]].skill_rank)   
						continue;

					num2++;

					/* Get info */
					mindcraft_info(comment, use_mind, i);
					
					psi_desc[0] = '\0';

					strcat(psi_desc, format("%c) ", I2A(i)));
					strcat(psi_desc,
					       format("%-30s%2d %4d %3d%%%s",
						      spell.name, spell.min_lev, spell.mana_cost,
						      chance[i], comment));
					prt(psi_desc, y + num2, x + 3);
					
					 
				}

				/* Clear the bottom line */
				prt("", y + num2 + 1, x);

			}

			/* Hide the list */
			else 
			{
				/* Hide list */
				redraw = FALSE;

				/* Restore the screen */
				screen_load();
			}

			/* Redo asking */
			continue;
		}
		else if (choice == '@')
		{
			char buf[80];

			strcpy(buf, " ");
			if (!get_string(format("Which %s? ", p), buf, 79))
			{
				if (redraw) screen_load();
				return FALSE;
			}

			if (strlen(buf) > 2)
			{
				/* Find the skill it is related to */
				for (i = 0; i < MAX_CLASS_POWERS; i++)
				{
					spell = mind_ptr->info[i];
					if (!strcmp(buf, spell.name))
						break;
				}
				if (i < MAX_CLASS_POWERS)
				{
					named = TRUE;
				}
				else
				{
					msg_print(format("Unknown %s!", p));
					if (redraw) screen_load();
					return FALSE;
				}
			}
		}

		if (!named)
		{
			/* Note verify */
			ask = isupper(choice);

			/* Lowercase */
			if (ask) choice = tolower(choice);

			/* Extract request */
			i = (islower(choice) ? A2I(choice) : -1);

			/* Totally Illegal */
			if ((i < 0) ||
				(i < CLASS_POWER_BREAK_ONE && mind_ptr->info[i].min_lev > p_ptr->skills[skill[0]].skill_rank) ||
				(((i >= CLASS_POWER_BREAK_ONE) && (i < CLASS_POWER_BREAK_TWO)) && mind_ptr->info[i].min_lev > p_ptr->skills[skill[1]].skill_rank) ||
				(((i >= CLASS_POWER_BREAK_TWO) && (i < MAX_CLASS_POWERS)) && mind_ptr->info[i].min_lev > p_ptr->skills[skill[2]].skill_rank) )
			{
				bell("Illegal class power choice!");
				continue;
			}

		}
		else
		{
			ask = 0;
		}
	
		/* Save the spell index */
		/* spell = mind_ptr->info[i]; */

		/* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Prompt */
			(void)strnfmt(tmp_val, 78, "Use %s? ", spell.name);


			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}

	/* Restore the screen */
	if (redraw) screen_load();

	/* Show choices */
	/* Update */
	p_ptr->window |= (PW_OVERHEAD);

	/* Window stuff */
	window_stuff();
	
	/* Abort if needed */
	if (!flag) return (FALSE);

	/* Save the choice */
	(*sn) = i;
	(*chnc) = chance[i];

#ifdef ALLOW_REPEAT /* TNB */

	repeat_push(*sn);

#endif /* ALLOW_REPEAT -- TNB */

	/* Success */
	return (TRUE);
}

/*
 * do_cmd_mind calls this function if the player's class
 * is 'Officer'.
 */
static bool cast_officer_spell(int spell)
{
	int	dir;
	int y, x, i;
	int time = randint(20) + 20;
	int officer, officer2, presense;
	
	officer = officer2 = presense = 0;
	
	if (p_ptr->skills[SK_OFFICER].skill_max > 0)
		officer = p_ptr->skills[SK_OFFICER].skill_rank;
	if (p_ptr->skills[SK_OFFICER2].skill_max > 0)
		officer2 = p_ptr->skills[SK_OFFICER2].skill_rank;
	if (p_ptr->skills[SK_PRESENCE].skill_max > 0)
		presense = p_ptr->skills[SK_PRESENCE].skill_rank;
		
	/* spell code */
	switch (spell)
	{
						
		case 0: /* Sense recruits */
				(void)detect_monsters_charm(FALSE);
				break;

		case 1: /* Inspire Fear */
				/* get direction */
				if (!get_aim_dir(&dir)) return FALSE;

				/* scare away monsters */
				(void)fear_monster(dir, officer);
				break;

		case 2: /* Command */
				/* get direction */
				if (!get_aim_dir(&dir)) return FALSE;

				/* fire a stun bolt */
				(void)fire_bolt(GF_STUN, dir,
					damroll(officer, 5 + (presense / 4)));
				break;

		case 3: /* Enlist */
				/* get direction */
				if (!get_aim_dir(&dir)) return FALSE; 

				/* Charm a monster. */
				/* Saving throw is monster level > randint damage */
				msg_print("You shout orders!");
				(void)fire_bolt(GF_CHARM, dir,
					damroll(presense + 1, presense + 2));
				break;

		case 4: /* Evaluate Opponent */
				probing();
				break;

		case 5: /* Mass enlistment */
				/* Get direction */ 
				if (!get_aim_dir(&dir)) return FALSE;

				/* Fireball of charm! damage restricted to 40d6 (max 240) */
				fire_ball(GF_CHARM, dir,
							damroll((presense * 2), 6), 
							(presense / 6));
				break;

		case 6: break;

		case 7: /* Stunning Blow */
				/* Get direction */ 
				if (!get_rep_dir(&dir)) return FALSE;

				/* Get the coordinates */
				y = p_ptr->py + ddy[dir];
				x = p_ptr->px + ddx[dir];

				/* Check to see if anything is there */
				if (cave_m_idx[y][x] > 0)
				{
					/* Attack w/Stunning blow! */
					py_attack(y, x, PY_ATTACK_STUN);
				}
				else
				{
					/* Inform of bad news ;_; */
					msg_print("There is no monster.");
				}
				break;

		case 8: /* Stiff upper lip */
				/* Get Healed */
				(void)hp_player(damroll((officer2 / 2), 
									(officer2 + (p_ptr->lev / 3))));
				(void)wp_player(damroll((officer2 / 4) + 1,
									2));

				/* Stop feeling suckish */
				if (officer2 > 13)
				{
					(void)set_afraid(0);
					(void)set_stun(0);
					(void)set_cut(0);
				}

				/* Restore yourself! (powerful!) */
				if (officer2 > 19)
				{
					for (i = 0; i < A_MAX; i++) do_res_stat(i);
				}
				break;
				
		case 9:	/* Counter-strike */
				msg_print("You prepare to counter blow.");
				p_ptr->counter = TRUE;
				break;
				
		case 10:/* Defensive Techniques */
				/* set protection from evil. */
				(void)set_protevil(p_ptr->protevil + randint(25) + 
										(6 * officer2));

				/* set fast movement */
				if (!p_ptr->fast) (void)set_fast(randint(20)
												 + officer2 * 2);
				else (void)set_fast(p_ptr->fast + randint(5));

				/* set resistance */
				if (officer2 > 18)
				{
					(void)set_tim_res(RS_FIR, p_ptr->tim_res[RS_FIR] + time);
					(void)set_tim_res(RS_EAR, p_ptr->tim_res[RS_EAR] + time);
					(void)set_tim_res(RS_WTR, p_ptr->tim_res[RS_WTR] + time);
					(void)set_tim_res(RS_AIR, p_ptr->tim_res[RS_AIR] + time);
				}

				/* set INVULNERABILITY!!! for 2-4 turns. */
				if (presense > 15)	
				{
					(void)set_invuln(p_ptr->invuln + (2 + rand_int(1)));
					msg_print("HOLD THE LINE!!");
				}
				break;
				
		case 11:/* Blasting Strike */
				/* Get direction */ 
				if (!get_rep_dir(&dir)) return FALSE;

				/* Get location */
				y = p_ptr->py + ddy[dir];
				x = p_ptr->px + ddx[dir];

				/* See if anything is there */
				if (cave_m_idx[y][x] > 0)
				{
					/* Attack w/Impact blow! */
					py_attack(y, x, PY_ATTACK_IMPACT);
				}
				else
				{
					/* Inform that life is full of little dissapointments */
					msg_print("There is no monster.");
				}

				break;

		case 12:/* Critical Strike */
				/* Get direction */ 
				if (!get_rep_dir(&dir)) return FALSE;

				/* Get location */
				y = p_ptr->py + ddy[dir];
				x = p_ptr->px + ddx[dir];

				/* See if anything is there */
				if (cave_m_idx[y][x] > 0)
				{
					/* Attack w/Critical blow! */
					py_attack(y, x, PY_ATTACK_CRITICAL);
				}
				else
				{
					/* relate dissapointment */
					msg_print("There is no monster.");
				}
				 break;
		case 13: break;
		case 14: break;
		case 15: break;
		case 16: break;
		case 17: break;
		case 18: break;
		case 19: break;
		case 20: break;
		default: msg_print("Unknown Class power!");
	}
return (TRUE);
}

/*
 * do_cmd_mind calls this function if the player's class
 * is 'Aesthete'.
 */
static bool cast_aesthete_spell(int spell)
{	
	int aesthete, aesthete2;

	aesthete = aesthete2 = 0;

	if (p_ptr->skills[SK_AESTHETE].skill_max > 0)
		aesthete = p_ptr->skills[SK_AESTHETE].skill_rank;
	if (p_ptr->skills[SK_AESTHETE2].skill_max > 0)
		aesthete2 = p_ptr->skills[SK_AESTHETE2].skill_rank;

	/* spell code */
	switch (spell)
	{
		case 0: /* Arrogant Laugh (almost bad) */
				if ((aesthete * 2) > (randint(36) + 4))
				{
					msg_print("Creatures Cower in fear before you!");
					project_los(GF_FEAR, (aesthete * 8));
				}
				else
				{
					msg_print("Your Arrogant Laugh angers the monsters around you!");
					aggravate_monsters(-1);
				}
				break;

		case 1: /* Treasure Sense */
				if (aesthete > 18) detect_traps(FALSE);
				if (aesthete > 16) detect_objects_magic(FALSE);
				if (aesthete > 14) detect_objects_normal(FALSE);
				if (aesthete > 10) detect_treasure(FALSE);
				detect_objects_gold(FALSE);
				break;

		case 2: /* Legendary lore */
				if (aesthete > 19) identify_fully();
				else ident_spell();
				break;

		case 3: /* Duck out the back */
				teleport_player(aesthete * 2); 
				break;

		case 4: /* Touch of Fortune */ 
				alchemy(); 
				break;

		case 5: /* Merchant's Trail */
				set_recall(); 
				break;

		case 6: break;	
		case 7: /* Enhance weapon */
				(void)enchant_spell(rand_int(aesthete2) + 1, 
										rand_int(aesthete2) + 1, 0);
				break;

		case 8: /* Enhance Armor */
				(void)enchant_spell(0, 0, rand_int(aesthete2) + 1);
				break;

		case 9: /* Item Overhaul */
				recharge(aesthete2 * 3); 
				break;

		case 10: brand_weapon(); 
				 break;

		case 11: break;
		case 12: break;
		case 13: break;
		case 14: break;
		case 15: break;
		case 16: break;
		case 17: break;
		case 18: break;
		case 19: break;
		case 20: break;
		default: msg_print("Unknown Class power!");
	}
	return TRUE;
}

/*
 * do_cmd_mind calls this function if the player's class
 * is 'explorer'.
 */
static bool cast_explorer_spell(int spell)
{
	/* this will vary based on the spells, and what they depend on */
	int b = 0;
	int	px = p_ptr->px;
	int py = p_ptr->py;
	int explorer = 0;
	
	if (p_ptr->skills[SK_EXPLORER].skill_max > 0)
		explorer = p_ptr->skills[SK_EXPLORER].skill_rank;
	
	/* spell code */
	switch (spell)
	{
		case 0: /* Light Area */
				lite_room(py, px);
				break;
		case 1: /* Detection */
				if (explorer > 19) wiz_lite();
				else if (explorer > 7) map_area();
				if (explorer < 10)
				{
					b = detect_monsters_normal(FALSE);
					if (explorer > 6)
						b |= detect_monsters_invis(FALSE);

					if (explorer > 1)
					{
						b |= detect_traps(FALSE);
						b |= detect_doors(FALSE);
					}
				}
				else
				{
					b = detect_all();
				}

				 if (explorer > 18)
				 {
				 	(void)set_tim_esp(p_ptr->tim_esp + (explorer * 3));
				 }
				 if (!b) msg_print("You feel safe.");
				 break;
		case 2:  /* Portal */
				 teleport_player(explorer * 2);
				 break;
		case 3:  /* Satisfy Hunger */
				 (void)set_food(PY_FOOD_MAX - 1);
				 break;
		case 4:  /* Identify */
				 ident_spell();
				 break;
		case 5:  /* Word of Recall */
				 set_recall();
				 break;
		case 6:  /* Recharge Item */
				 (void)recharge(30);
				 break;
		case 7:  break;
		case 8:  break;
		case 9:  break;
		case 10: break;
		case 11: break;
		case 12: break;
		case 13: break;
		case 14: break;
		case 15: break;
		case 16: break;
		case 17: break;
		case 18: break;
		case 19: break;
		case 20: break;
		default: msg_print("Unknown Class power!");

	}
	return TRUE;
}

/*
 * do_cmd_mind calls this function if the player's class
 * is 'Medium'.
 */
static bool cast_medium_spell(int spell)
{
	/* this will vary based on the spells, and what they depend on */
	int             b = 0;
	int 			dir, time, i;
	int 			telepathy, psychometabolism, clairsentience, mesmerwill;
	
	telepathy = psychometabolism = clairsentience = mesmerwill = 0;
	
	if (p_ptr->skills[SK_TELEPATHY].skill_max > 0)
		telepathy = p_ptr->skills[SK_TELEPATHY].skill_rank;
	if (p_ptr->skills[SK_PSYCHOMETABOLISM].skill_max > 0)
		psychometabolism = p_ptr->skills[SK_PSYCHOMETABOLISM].skill_rank;
	if (p_ptr->skills[SK_CLAIRSENTIENCE].skill_max > 0)
		clairsentience = p_ptr->skills[SK_CLAIRSENTIENCE].skill_rank;
	if (p_ptr->skills[SK_MESMERIC_WILL].skill_max > 0)
		mesmerwill = p_ptr->skills[SK_MESMERIC_WILL].skill_rank;
	
	
	/* spell code */
	switch (spell)
	{
		case 0:  /* Neural Blast */
				 /* Get direction */
				 if (!get_aim_dir(&dir)) return FALSE;
				 
				 (void)fire_bolt(GF_PSI, dir,
					damroll(5 + (telepathy / 2), (3 + telepathy / 6)));
				 break;
				 
		case 1:  /* Psychic Disturbance */
				 msg_print("You disturb specters from beyond the veil!");
				 for (b = 0; b < (telepathy / 3); b++)
				 {
					 /* Get a new effect index */
					 i = effect_prep();
	
					 /* Note failure */
					 if (i < 0) break;
	
					 /* We want a spirit, */
					 x_list[i].index = EFFECT_SPIRIT_VORTEX;
	
					 /* Of Spirit */
					 x_list[i].type = GF_SPIRIT;
	
					 /* That starts at the character location. */
					 x_list[i].y0 = p_ptr->py;
					 x_list[i].x0 = p_ptr->px;
	
					 /* Moves with a speed */
					 x_list[i].time_delay = 4;
	
					 /* Does damage, */
					 x_list[i].power = rand_range(mesmerwill * 2, mesmerwill * 4) + 
							damroll(1 + (telepathy / 2), telepathy / 3);
	
					 /* And lasts for a certain period of time. */
					 x_list[i].lifespan = telepathy;
				}
				 break;

		case 2:	 /* spirit blast  */
				 if (!get_aim_dir(&dir)) return FALSE;
				 (void)fire_ball(GF_TK, dir, damroll(telepathy, 8),
							(mesmerwill > 6 ? (mesmerwill) / 6 : 0));
				 break;

		case 3:  /* Domination */
				 if (!get_aim_dir(&dir)) return FALSE; 
				 (void)fire_bolt(GF_DOMINATION, dir,
						damroll((telepathy), (3 + mesmerwill) / 2));				
				 break;

		case 4:  /* Soul Purge - Need to change this to spirit. */
				 msg_print("The anguish of the dead emanates from your brain!");
				 fire_star(GF_ECTOPLASM, 5, damroll((telepathy + mesmerwill) * 2, 4),
							2 + (mesmerwill / 6));
				 break;

		case 5:  /* Psychic Drain */
				 /* Get direction */
				 if (!get_aim_dir(&dir)) return FALSE;
				
				 /* Damage */
				 b = damroll((telepathy + mesmerwill) * 2, 3);
	
				 /* This is always a radius-0 ball now */
				 if (fire_ball(GF_SLOW, dir, b, 0)) p_ptr->energy -= randint(150);
				 break;
				 
		case 6:  /* Entropic Blast */
				 msg_print("A wave of pure entropic force blasts out from your spirit!");

				 fire_star(GF_LIQUESCE, 5, telepathy * (mesmerwill / 3),
				 	2 + (mesmerwill / 5));
				 break;
				 
		case 7:  /* Precognition */
				 if (clairsentience > 19) wiz_lite();
				 else if (clairsentience > 7) map_area();
				 if (clairsentience < 10)
				 {
				 	b = detect_monsters_normal(FALSE);
				 	if (clairsentience > 4)
				 		b |= detect_monsters_invis(FALSE);
				 		
				 	if (clairsentience > 2)
				 	{
				 		b |= detect_traps(FALSE);
				 		b |= detect_doors(FALSE);
				 	}
				 }
				 else
				 {
				 	b = detect_all();
				 }
				 if (clairsentience > 14)
				 {
				 	(void)set_tim_esp(p_ptr->tim_esp + (clairsentience * 3));
				 }
				 if (!b) msg_print("You feel safe.");
				 break;

		case 8:  /*Psychometry */
				 ident_spell();
				 break;

		case 9:  break;
		case 10: break;
		case 11: break;
		case 12: break;
		case 13: /* Healing */
				 (void)hp_player(randint((psychometabolism + 
				 						 mesmerwill) * 2));
				 (void)wp_player(rand_int((psychometabolism +
				 						 mesmerwill) / 3));
				 break;

		case 14: /* Minor displace */
				teleport_player(psychometabolism);break;

	    case 15: /* Character Armour */
				time = randint((psychometabolism + 
								mesmerwill) * 2);
				(void)set_shield(p_ptr->shield + time);
				if (psychometabolism > 11)
				{
							(void)set_tim_res(RS_FIR, p_ptr->tim_res[RS_FIR] + time);
							(void)set_tim_res(RS_EAR, p_ptr->tim_res[RS_EAR] + time);
							(void)set_tim_res(RS_AIR, p_ptr->tim_res[RS_AIR] + time);
							(void)set_tim_res(RS_WTR, p_ptr->tim_res[RS_WTR] + time);
				}
				if (psychometabolism > 14)
				{
							(void)set_tim_res(RS_ELC, p_ptr->tim_res[RS_ELC] + time);
							(void)set_tim_res(RS_ICE, p_ptr->tim_res[RS_ICE] + time);
							(void)set_tim_res(RS_ACD, p_ptr->tim_res[RS_ACD] + time);
							(void)set_tim_res(RS_PSN, p_ptr->tim_res[RS_PSN] + time);
				}
				if (psychometabolism > 19)
				{
							(void)set_tim_res(RS_TIM, p_ptr->tim_res[RS_TIM] + time);
							(void)set_tim_res(RS_ETH, p_ptr->tim_res[RS_ETH] + time);
							(void)set_tim_res(RS_SND, p_ptr->tim_res[RS_SND] + time);
							(void)set_tim_res(RS_NTH, p_ptr->tim_res[RS_NTH] + time);
				}
							
				break;

		case 16: /* Major displace */
				teleport_player(psychometabolism * 10); break;

		case 17: /* drawing upon the spirits */
				 (void)set_afraid(0);
				 (void)set_stun(0);

				/* set timer */				
				b = 10 + randint((psychometabolism + mesmerwill) * 2);

				/* Set normal heroism */
				if (mesmerwill < 15) (void)set_hero(p_ptr->hero + b);
				
				/* or super heroism */
				else (void)set_shero(p_ptr->shero + b);
				
				/* if not fast, set fast */
				if (!p_ptr->fast)
				{
					/* Haste */
					(void)set_fast(b);
				}
				else (void)set_fast(p_ptr->fast + randint(5));
				break;

		case 18: /* Body Dispersion */
				 set_shadow(p_ptr->tim_wraith + randint (6) + 2);
				 break;

		case 19: break;
		case 20: break;
		default: msg_print("Unknown Class power!");
	}
	return TRUE;
}

/*
 * do_cmd_mind calls this function if the player's class
 * is 'Reckoner'.
 */
static bool cast_reckoner_spell(int spell)
{
	/* spell code */
	switch (spell)
	{
		case 0:  break;
		case 1:  break;
		case 2:  break;
		case 3:  break;
		case 4:  break;
		case 5:  break;
		case 6:  break;
		case 7:  break;
		case 8:  break;
		case 9:  break;
		case 10: break;
		case 11: break;
		case 12: break;
		case 13: break;
		case 14: break;
		case 15: break;
		case 16: break;
		case 17: break;
		case 18: break;
		case 19: break;
		case 20: break;
	}
	return TRUE;
}

/*
 * do_cmd_mind calls this function if the player's class
 * is 'tourist'.
 */
static bool cast_tourist_spell(int spell)
{
	/* spell code */
	switch (spell)
	{
		case 0:  break;
		case 1:  break;
		case 2:  break;
		case 3:  break;
		case 4:  break;
		case 5:  break;
		case 6:  break;
		case 7:  break;
		case 8:  break;
		case 9:  break;
		case 10: break;
		case 11: break;
		case 12: break;
		case 13: break;
		case 14: break;
		case 15: break;
		case 16: break;
		case 17: break;
		case 18: break;
		case 19: break;
		case 20: break;
	}
	return TRUE;
}

/*
 * do_cmd_mind calls this function if the player's class
 * is 'Hussar'.
 */
static bool cast_hussar_spell(int spell)
{
	int y, x, dir, ybank, xbank, time;
	int cdir;
	int	py = p_ptr->py;
	int	px = p_ptr->px;
	int combattech = 0;
	
	if (p_ptr->skills[SK_COMBATTECHNIQUES].skill_max > 0)
		combattech = p_ptr->skills[SK_COMBATTECHNIQUES].skill_rank;

	time = 	randint(combattech * 2) + combattech;
		
	/* spell code */
	switch (spell)
	{
    	case 0: /* Battle Preperation */
				(void)set_afraid(0);
    			(void)set_hero(combattech * randint(3));
    			break;    					

		case 1: /* Three way strike */							
				/* Get main direction */
				if (!get_rep_dir(&dir)) return FALSE;
				if (dir == 5) return FALSE;
				
				/* Translate the dir to cdir */
				for (cdir = 0;cdir < 8; cdir++)
				{
					if (cdd[cdir] == dir) break;
				}
				if (cdir == 8) return FALSE;
				
				/* get the location of the strike */
				y = py + ddy_cdd[cdir];
				x = px + ddx_cdd[cdir];
				
				/* Check to make sure there's a monster, and strike */
				if (cave_m_idx[y][x] > 0) py_attack(y, x, 0);
				
				/* Otherwise print error message */
				else msg_print("You attack the empty air.");
				
				/* Make the second strike - get location of strike */
				y = py + ddy_cdd[(cdir + 7) % 8];
				x = px + ddx_cdd[(cdir + 7) % 8];

				/* Check to make sure there's a monster */
				if (cave_m_idx[y][x] > 0) py_attack(y, x, 0);
				
				/* Otherwise print error message */
				else msg_print("You attack the empty air.");

				/* Make the third strike - get location of strike */
				y = py + ddy_cdd[(cdir + 1) % 8];
				x = px + ddx_cdd[(cdir + 1) % 8];

				/* Check to make sure there's a monster */
				if (cave_m_idx[y][x] > 0) py_attack(y, x, 0);

				/* Otherwise print error message */
				else msg_print("You attack the empty air.");

				break;
				
		case 2: /* Resistance Stance */
				(void)set_tim_res(RS_FIR, p_ptr->tim_res[RS_FIR] + time);
				(void)set_tim_res(RS_EAR, p_ptr->tim_res[RS_EAR] + time);
				(void)set_tim_res(RS_AIR, p_ptr->tim_res[RS_AIR] + time);
				(void)set_tim_res(RS_WTR, p_ptr->tim_res[RS_WTR] + time);
				break;

		case 3: /* Stunning Blow */
				/* Get direction */ 
				if (!get_rep_dir(&dir)) return FALSE;
				if (dir == 5) return FALSE;
				y = p_ptr->py + ddy[dir];
				x = p_ptr->px + ddx[dir];
				if (cave_m_idx[y][x] > 0)
				{
					py_attack(y, x, PY_ATTACK_STUN);
				}
				else
				{
					msg_print("There is no monster.");
				}
				break;

		case 4: /* Defensive Stance */
		 		msg_print("You prepare to counter blow.");
				p_ptr->counter = TRUE;
				/* Need to fix this so that it has 'non-magical text' */
				(void)set_shield(time);
				break;

		case 5: /* Rush Attack */
				if (!get_rep_dir(&dir)) return FALSE;
				if (dir == 5) return FALSE;

				/* Step 0 */
				y = p_ptr->py + ddy[dir];
				x = p_ptr->px + ddx[dir];

				if (cave_m_idx[y][x] > 0)
				{
					msg_print("There is a monster in the way!");
					py_attack(y, x, 0);
					return TRUE;
				}
				else if (cave_passable_bold(y, x))
				{
					/* Note - this uses no energy, skips traps, */
					/* and doesn't search - it's direct */
					monster_swap(p_ptr->py, p_ptr->px, y, x);
				}
				else 
				{	
					/* Nothing happens */
					msg_print("Argh - You can't step there!");
					return TRUE;
				}
				
				/* prepare the attack */
				y = p_ptr->py + ddy[dir];
				x = p_ptr->px + ddx[dir];

				if (cave_m_idx[y][x] > 0)
				{
					/* attack */
					py_attack(y, x, 0);
					return TRUE;
				}
				break;

		case 6: /* Blasting Strike */
				/* Get direction */ 
				if (!get_rep_dir(&dir)) return FALSE;
				if (dir == 5) return FALSE;
				y = p_ptr->py + ddy[dir];
				x = p_ptr->px + ddx[dir];
				if (cave_m_idx[y][x] > 0)
				{
					py_attack(y, x, PY_ATTACK_IMPACT);
				}
				else
				{
					msg_print("There is no monster.");
				}
				break;

		case 7: /* Critical Strike */
				/* Get direction */ 
				if (!get_rep_dir(&dir)) return FALSE;
				if (dir == 5) return FALSE;
				y = p_ptr->py + ddy[dir];
				x = p_ptr->px + ddx[dir];
				if (cave_m_idx[y][x] > 0)
				{
					py_attack(y, x, PY_ATTACK_CRITICAL);
				}
				else
				{
					msg_print("There is no monster.");
				}
				break;

		case 8: /* Flanking Maneuver */
				if (!get_rep_dir(&dir)) return FALSE;
				if (dir == 5) return FALSE;
				/* Make the first strike */
				for (cdir = 0;cdir < 8; cdir++)
				{
					if (cdd[cdir] == dir) break;
				}
				if (cdir == 8) return FALSE;

				/* get the location of the strike */
				y = ybank = py + ddy_cdd[cdir];
				x = xbank = px + ddx_cdd[cdir];
				
				if (cave_m_idx[y][x] > 0)
				{
					y = py + ddy_cdd[(cdir + 7) % 8];
					x = px + ddx_cdd[(cdir + 7) % 8];
					
					if ((cave_m_idx[y][x] > 0) || (!cave_passable_bold(y,x)))
					{
						y = py + ddy_cdd[(cdir + 1) % 8];
						x = px + ddx_cdd[(cdir + 1) % 8];
						if ((cave_m_idx[y][x] > 0) || (!cave_passable_bold(y,x)))
						{
							msg_print("You cannot flank this monster!");
							return TRUE;
						}
						else if (cave_passable_bold(y,x))
						{
							monster_swap(p_ptr->py, p_ptr->px, y, x);
							if (cave_m_idx[ybank][xbank] > 0) 
						 	{
						 		py_attack(ybank, xbank, PY_ATTACK_CRITICAL);
						 	}
						 	return TRUE;
						}
					}
					else if (cave_passable_bold(y,x))
					{
						monster_swap(p_ptr->py, p_ptr->px, y, x);
						if (cave_m_idx[ybank][xbank] > 0) 
					 	{
					 		py_attack(ybank, xbank, PY_ATTACK_CRITICAL);
					 	}
					 	return TRUE;

					}
				}
				else
				{						
					msg_print("There is no monster to flank there!");
					return TRUE;
				}
				break;
				
		case 9: /* Triple Strike */
				if (!get_rep_dir(&dir)) return FALSE;
				if (dir == 5) return FALSE;
				y = py + ddy[dir];
				x = px + ddx[dir];
				if (cave_m_idx[y][x] > 0)
				{
					py_attack(y, x, 0);
					if (cave_m_idx[y][x] > 0)
					{
						handle_stuff();
						py_attack(y, x, 0);
						if (cave_m_idx[y][x] > 0)
						{
							handle_stuff();
							py_attack(y, x, 0);
						}
					}
				}
				else
				{
					msg_print("You don't see any monster in this direction");
					return FALSE;
				}
				break;

		case 10:do_cmd_fire(24); 
				break;

		case 11:/* Charge! */
				if (!get_rep_dir(&dir)) return FALSE;
				if (dir == 5) return FALSE;

				/* Step 0 */
				y = p_ptr->py + ddy[dir];
				x = p_ptr->px + ddx[dir];
				
				if (cave_m_idx[y][x] > 0)
				{
					msg_print("You don't have enough room to charge!");
					/* do a normal attack */
					py_attack(y, x, 0);
					return TRUE;
				}
				else if (cave_passable_bold(y, x))
				{
					/* Note - this uses no energy, skips traps, */
					/* and doesn't search - it's direct */
					monster_swap(p_ptr->py, p_ptr->px, y, x);
				}
				else 
				{	
					/* Nothing happens */
					msg_print("Argh - Your charge failed!");
					return TRUE;
				}
				
				/* Step 1 */
				y = p_ptr->py + ddy[dir];
				x = p_ptr->px + ddx[dir];
				
				if (cave_m_idx[y][x] > 0)
				{
					/* do a normal attack */
					py_attack(y, x, PY_ATTACK_STUN);
					return TRUE;
				}
				else if (cave_passable_bold(y,x))
				{
					/* Note - this uses no energy, skips traps, */
					/* and doesn't search - it's direct */
					monster_swap(p_ptr->py, p_ptr->px, y, x);
				}
				else 
				{	
					/* Nothing happens */
					msg_print("Argh - Your charge failed!");
					return TRUE;
				}

				/* Step 2 */
				y = p_ptr->py + ddy[dir];
				x = p_ptr->px + ddx[dir];
				
				if (cave_m_idx[y][x] > 0)
				{
					/* do a normal attack */
					py_attack(y, x, PY_ATTACK_STUN);
					if (cave_m_idx[y][x] > 0) 
					{
						py_attack(y, x, PY_ATTACK_CRITICAL);
					}
					return TRUE;
				}
				else if (cave_passable_bold(y,x))
				{
					/* Note - this uses no energy, skips traps, */
					/* and doesn't search - it's direct */
					monster_swap(p_ptr->py, p_ptr->px, y, x);
				}
				else 
				{	
				/* Nothing happens */
					msg_print("Argh - Your charge failed!");
					return TRUE;
				}
				/* Step 3 */
				y = p_ptr->py + ddy[dir];
				x = p_ptr->px + ddx[dir];
				
				if (cave_m_idx[y][x] > 0)
				{
					/* do a normal attack */
					py_attack(y, x, 0);
					 if (cave_m_idx[y][x] > 0) py_attack(y, x, PY_ATTACK_STUN);
					 if (cave_m_idx[y][x] > 0) 
					 {
					 	py_attack(y, x, PY_ATTACK_CRITICAL);
					 }
					 return TRUE;
				}
				else if (cave_passable_bold(y,x))
				{
					/* Note - this uses no energy, skips traps, */
					/* and doesn't search - it's direct */
					monster_swap(p_ptr->py, p_ptr->px, y, x);
				}
				else 
				{	
					/* Nothing happens */
					msg_print("Argh - Your charge failed!");
					return TRUE;
				}
				/* Step 3 */
				y = p_ptr->py + ddy[dir];
				x = p_ptr->px + ddx[dir];
				
				if (cave_m_idx[y][x] > 0)
				{
					/* do a normal attack */
					py_attack(y, x, 0);
					if (cave_m_idx[y][x] > 0) py_attack(y, x, 0);
					if (cave_m_idx[y][x] > 0) py_attack(y, x, PY_ATTACK_STUN);
					if (cave_m_idx[y][x] > 0) py_attack(y, x, PY_ATTACK_CRITICAL);
					if (cave_m_idx[y][x] > 0) 
					{
						py_attack(y, x, PY_ATTACK_IMPACT);
					}
					return TRUE;
				}
				else 
				{	
					/* Nothing happens */
					msg_print("Arrrggh - You've reached the end of your charge!");
					return TRUE;
				}
		
				break;

		case 12:/* Lightning Strike */
				if (!get_rep_dir(&dir)) return FALSE;
				if (dir == 5) return FALSE;

				/* Translate the dir to cdir */
				for (cdir = 0;cdir < 8; cdir++)
				{
					if (cdd[cdir] == dir) break;
				}
				if (cdir == 8) return FALSE;

				/* get the location of the strike */
				y = p_ptr->py + ddy_cdd[cdir];
				x = p_ptr->px + ddx_cdd[cdir];

				if (cave_m_idx[y][x] > 0)
				{
					msg_print("There is a monster in the way!");
					py_attack(y, x, 0);
					return TRUE;
				}
				else if (cave_passable_bold(y, x))
				{
					/* Note - this uses no energy, skips traps, */
					/* and doesn't search - it's direct */
					monster_swap(p_ptr->py, p_ptr->px, y, x);
				}
				else 
				{	
					/* Nothing happens */
					msg_print("Argh - You can't step there!");
					return TRUE;
				}
				
				/* prepare the attack */
				y = p_ptr->py + ddy_cdd[cdir];
				x = p_ptr->px + ddx_cdd[cdir];

				if (cave_m_idx[y][x] > 0)
				{
					/* attack */
					py_attack(y, x, 0);
				}
				else 
				{
					/* Nothing happens */
					msg_print("There's nothing to attack there!");
					return TRUE;
				}
				
				/* move back */
				y = p_ptr->py + ddy_cdd[(cdir + 4) % 8];
				x = p_ptr->px + ddx_cdd[(cdir + 4) % 8];
				
				if (cave_passable_bold(y, x))
				{
					/* Note - this uses no energy, skips traps, */
					/* and doesn't search - it's direct */
					monster_swap(p_ptr->py, p_ptr->px, y, x);
				}						 
				else
				{
					/* sanity check */
					msg_print("There's a serious problem!");
					return TRUE;						 	
				}
				 break;
		case 13: break;
		case 14: break;
		case 15: break;
		case 16: break;
		case 17: break;
		case 18: break;
		case 19: break;
		case 20: break;
	}
	return TRUE;
}

/*
 * do_cmd_mind calls this function if the player's class
 * is 'Naturalist'.
 */
static bool cast_nature_spell(int spell)
{
	/* this will vary based on the spells, and what they depend on */
	int 			dir, k, ty, tx, time, b, dam;
	int 			px = p_ptr->px;
	int				py = p_ptr->py;
	int				floral, fauna, elemental;
	
	floral = fauna = elemental = 0;	

	if (p_ptr->skills[SK_FLORAL].skill_max > 0)
		floral = p_ptr->skills[SK_FLORAL].skill_rank;
	if (p_ptr->skills[SK_FAUNA].skill_max > 0)
		fauna = p_ptr->skills[SK_FAUNA].skill_rank;
	if (p_ptr->skills[SK_ELEMENTAL].skill_max > 0)
		elemental = p_ptr->skills[SK_ELEMENTAL].skill_rank;
	
	/* spell code */
	switch (spell)
	{
	case 0: /* Woodland Sanctuary */
			(void)set_food(PY_FOOD_FULL - 1);
			(void)hp_player(damroll(floral * 3, 8));
			(void)wp_player(damroll(floral / 2, 2));
			(void)set_cut(0);
			(void)set_poisoned(0);
			break;
	case 1: /* Detect Doors + Traps */
			(void)detect_traps(FALSE);
			(void)detect_doors(FALSE);
			(void)detect_stairs(FALSE);
			break;

	case 2: /* Daylight */
			(void)lite_area(damroll(2, (floral)), 
								(floral / 6) + 1);
			break;

	case 3: /* Stone to Mud */
			if (!get_aim_dir(&dir)) return FALSE;
			(void)wall_to_mud(dir);
			break;

	case 4: /* Entangle */
			slow_monsters();
			break;

	case 5: /* Resist Environment */
			time = (randint(floral * 2) + floral);
			(void)set_tim_res(RS_FIR, p_ptr->tim_res[RS_FIR] + time);
			(void)set_tim_res(RS_EAR, p_ptr->tim_res[RS_EAR] + time);
			(void)set_tim_res(RS_AIR, p_ptr->tim_res[RS_AIR] + time);
			(void)set_tim_res(RS_WTR, p_ptr->tim_res[RS_WTR] + time);
			if (floral > 14)
			{
						(void)set_tim_res(RS_ELC, p_ptr->tim_res[RS_ELC] + time);
						(void)set_tim_res(RS_ICE, p_ptr->tim_res[RS_ICE] + time);
						(void)set_tim_res(RS_ACD, p_ptr->tim_res[RS_ACD] + time);
						(void)set_tim_res(RS_PSN, p_ptr->tim_res[RS_PSN] + time);
			}
			if (floral > 19)
			{
						(void)set_tim_res(RS_TIM, p_ptr->tim_res[RS_TIM] + time);
						(void)set_tim_res(RS_ETH, p_ptr->tim_res[RS_ETH] + time);
						(void)set_tim_res(RS_SND, p_ptr->tim_res[RS_SND] + time);
						(void)set_tim_res(RS_NTH, p_ptr->tim_res[RS_NTH] + time);
			}
			break;

	case 6: /* UNKNOWN POWER */
			break;

	case 7: /* Detect Creatures */
			detect_monsters_normal(FALSE);
			break;

	case 8: /* Summon Animal */
			summon_specific(p_ptr->py, p_ptr->px, 
							(p_ptr->depth + (fauna / 4)), SUMMON_ANIMAL, TRUE);
			break;

	case 9: /* Animal Taming */
			if (!get_aim_dir(&dir)) return FALSE;
			(void)charm_animal(dir, fauna * 3);
			break;

	case 10: /* Summon Animal */
			summon_specific(p_ptr->py, p_ptr->px, 
							(p_ptr->depth + 2 + (fauna / 3)), SUMMON_ANIMAL, TRUE);
			break;

	case 11: /* Stone Skin */
			(void)set_shield(p_ptr->shield + randint(fauna) + 30);
			break;

	case 12: /* Summon Elemental */
			summon_specific(p_ptr->py, p_ptr->px, p_ptr->depth + 5, SUMMON_ELEMENTAL, TRUE);
			break;

	case 13:/* dancing flame */
			/* Get a new effect index */
			dam = damroll((1 + (elemental / 5)), (elemental / 2));
			k = effect_prep();

			/* Note failure XXX */
			if (k < 0) break;

			/* We want a patch, */
			x_list[k].index = EFFECT_SEEKER_VORTEX;

			/* Of fire */
			x_list[k].type = GF_FIRE;

			/* That starts at the character location. */
			x_list[k].y0 = py;
			x_list[k].x0 = px;

			/* Practices the spellcasting skill */
			/* x_list[k].practice_skill = S_MAGIC; */

			/* Moves with a speed that depends on the wind, */
			x_list[k].time_delay = 24 - (elemental); 

			/* Does damage, */
			x_list[k].power = dam;

			/* And lasts for a certain period of time. */
			x_list[k].lifespan = (8 + (elemental) / 6) * 10 / x_list[k].time_delay;

			break;
	case 14: /* Lightning Bolt */
			if (!get_aim_dir(&dir)) return FALSE;
			fire_beam(GF_ELEC, dir,
							damroll(3 + (elemental / 3), 8));
			break;
	case 15: /* Blizzard */
			/* Get a new effect index */
			k = effect_prep();

			/* Note failure XXX */
			if (k < 0) break;

			/* Get a direction */
			if (!get_hack_dir(&dir)) return FALSE;

			/* Use the given direction */
			ty = py + ddy[dir];
			tx = px + ddx[dir];
			
			/* Hack -- Use an actual "target" */
			if ((dir == 5) && target_okay())
			{
				ty = p_ptr->target_row;
				tx = p_ptr->target_col;
			}

			/* We want an lingering cloud, */
			x_list[k].index = EFFECT_IRREGULAR_CLOUD;

			/* Of ICE */
			if (elemental < 14) x_list[k].type = GF_ICE;
			else x_list[k].type = GF_GLACIAL;

			/* That starts at the monster location. */
			x_list[k].y0 = x_list[k].y1 = ty;
			x_list[k].x0 = x_list[k].x1 = tx;

			/* It attacks every 8 -> 5 game turns, */
			x_list[k].time_delay = 10;

			/* Does damage, has a large radius, */
			x_list[k].power = damroll(elemental / 5, ((elemental / 6) + 1));
			x_list[k].power2 = 8;

			/* And lasts for about 10 attacks */
			x_list[k].lifespan = elemental;
			break;

	case 16: /* Earthquake */
			earthquake(py, px, 10);
			break;

	case 17: /* Lightning Storm */
 			/* Get a new effect index */
			k = effect_prep();

			/* Note failure XXX */
			if (k < 0) break;

			/* Get a direction */
			if (!get_aim_dir(&dir)) return FALSE;

			/* Use the given direction */
			ty = py + ddy[dir];
			tx = px + ddx[dir];
			
			/* Hack -- Use an actual "target" */
			if ((dir == 5) && target_okay())
			{
				ty = p_ptr->target_row;
				tx = p_ptr->target_col;
			}

			/* We want an lingering cloud, */
			x_list[k].index = EFFECT_IRREGULAR_CLOUD;

			/* Of electricity */
			if (elemental < 14) x_list[k].type = GF_ELEC;
			else x_list[k].type = GF_VOLT;

			/* That starts at the monster location. */
			x_list[k].y0 = x_list[k].y1 = ty;
			x_list[k].x0 = x_list[k].x1 = tx;

			/* It attacks every 8 -> 5 game turns,  is quick.*/
			x_list[k].time_delay = 0;

			/* Does damage, has a small radius, */
			x_list[k].power = damroll(4 + (elemental / 5), randint(elemental * 2));
			if (elemental < 14)  x_list[k].power2 = 2;
			else x_list[k].power2 = 3;

			/* And lasts for about 10 attacks */
			/* Sometimes supercharge */
			if (one_in_(9)) x_list[k].lifespan = 3 + randint(elemental * 2);
			else x_list[k].lifespan = randint(elemental / 2);
			
			break;

	case 18: /* Whirlpool */
		 	/* Get a direction */
			if (!get_aim_dir(&dir)) return FALSE;

			/* Use the given direction */
			ty = py + ddy[dir];
			tx = px + ddx[dir];
			
			/* Hack -- Use an actual "target" */
			if ((dir == 5) && target_okay())
			{
				ty = p_ptr->target_row;
				tx = p_ptr->target_col;
			}
			 for (b = 0; b < (elemental) / 2; b++)
			 {
				 /* Get a new effect index */
				 k = effect_prep();

				 /* Note failure XXX */
				 if (k < 0) break;

				 /* We want a whirlpool, */
				 x_list[k].index = EFFECT_WHIRLPOOL;

				 /* Of water */
				 x_list[k].type = GF_STORM;

				 /* That starts at the target location. */
				 x_list[k].y0 = ty + ddy_cdd[(b + 1) % 8];
				 x_list[k].x0 = tx + ddx_cdd[(b + 1) % 8]; 

				 /* Moves with a speed that depends on the wind, */
				 x_list[k].time_delay = 1;

				 /* Does damage, */
				 x_list[k].power = rand_range(elemental / 2, elemental * 2);

				 /* And lasts for a certain period of time. */
				 x_list[k].lifespan = 80;
			}
 			break;
	case 19: /* Lord of the forest (possibly shapechange later?) */
			/*Buff spells */
			hp_player(800);
			warding_glyph();
			detect_animals(FALSE);
			(void)set_afraid(0);
			(void)set_stun(0);
			(void)set_cut(0);
			(void)do_res_stat(A_MUS);
	 		(void)do_res_stat(A_AGI);
	 		(void)do_res_stat(A_VIG);
	 		(void)do_res_stat(A_SCH);
	 		(void)do_res_stat(A_EGO);
	 		(void)do_res_stat(A_CHR);
			set_blessed(randint(20)+30);
			set_hero(randint(20)+30);
			set_protevil(randint(20)+30);
			break;
	case 20: /* Nature's Wrath */
			earthquake(py, px, 20 + (floral));
			project(0, 1 + elemental / 5, py, px, py, px,
				100 + (elemental * 2), GF_HURT, PROJECT_KILL | PROJECT_ITEM, 0, 0);
			break;
	default:
		msg_format("You cast an unknown Nature spell: %d.", spell);
		msg_print(NULL);
	}
	return TRUE;
}

/*
 * do_cmd_mind calls this function if the player's class
 * is 'Ninja'.
 */
static bool cast_ninja_spell(int spell)
{
	int dir, b, y, x, cdir, my, mx;
  	int tx, ty, m_idx, msec, dance;
	int ninjamagic, ninjastealth;
	char m_name[80];
	monster_type *m_ptr;
	
	ninjamagic = ninjastealth = 0;
	
	if (p_ptr->skills[SK_NINJAMAGIC].skill_max > 0)
		ninjamagic = p_ptr->skills[SK_NINJAMAGIC].skill_rank;
	if (p_ptr->skills[SK_NINJASTEALTH].skill_max > 0)
		ninjastealth = p_ptr->skills[SK_NINJASTEALTH].skill_rank;

	/* spell code */
	switch (spell)
	{
		case 0:  /* Distraction */
				 set_tim_invisiblity(p_ptr->tim_invisiblity + 1 + rand_int(3));
				 break;

		case 1:  /* Bind Monster */
				 if (!get_aim_dir(&dir)) return FALSE;
		 
				 fire_bolt(GF_STASIS, dir, (ninjamagic));						 
				 break;

		case 2:  /* Smoke Bomb */
				 fire_star(GF_CONFUSION, 5, damroll(ninjamagic / 2, ninjamagic), 
				 				(ninjamagic / 4));
				 teleport_player(3);
				 break;

		case 3:  /* Swap Position */
				 if (!get_aim_dir(&dir)) return FALSE;
				 teleport_swap(dir);
				 break;

		case 4:  /* Smoke Bomb */
				 fire_star(GF_POISON, 5, damroll(ninjamagic, ninjamagic * 2), 
				 				(ninjamagic / 4));
				 teleport_player(5);
				 break;

		case 5:  /* Passwall */
				 set_shadow(p_ptr->tim_wraith + randint (4) + 4);
				 break;

		case 6:  /* Shadow Travel */
				 msg_print("Choose a location to teleport to.");
				 message_flush();
				 dimen_door(ninjamagic * 2, 0);
				 break;

		case 7:  teleport_player(10);
				 break;

		case 8:  /* Detect Near */
				 if (ninjastealth > 19) wiz_lite();
				 else if (ninjastealth > 13) map_area();
				 if (ninjastealth < 11)
				 {
				 	b = detect_monsters_normal(FALSE);
				 	if (ninjastealth > 7)
				 		b |= detect_monsters_invis(FALSE);
				 		
				 	if (ninjastealth > 3)
				 	{
				 		b |= detect_traps(FALSE);
				 		b |= detect_doors(FALSE);
				 	}
				 }
				 else
				 {
				 	b = detect_all();
				 }
				 if (ninjastealth > 17)
				 {
				 	(void)set_tim_esp(p_ptr->tim_esp + (ninjastealth * 2));
				 }
				 if (!b) msg_print("You feel safe.");
				 break;

		case 9:  /* Absconding */
				 teleport_player(40);
				 break;

		case 10: /* Ancient Knowledge */
				 ident_spell();
				 break;

		case 11: /* Invisibilty */
				 if (!p_ptr->tim_invisiblity)
				 	set_tim_invisiblity(p_ptr->tim_invisiblity + 4 + randint(ninjastealth));
				 else 
					 	set_tim_invisiblity(p_ptr->tim_invisiblity + 2);   					 	
				 break;

		case 12: /* No Power */
				 break;

		case 13: /* Counterstrike */
				 msg_print("You prepare to counter blow.");
				 p_ptr->counter = TRUE;
				 break;

		case 14: /* Strike and Away */
				if (!get_rep_dir(&dir)) return FALSE;
				if (dir == 5) return FALSE;

				/* Translate the dir to cdir */
				for (cdir = 0;cdir < 8; cdir++)
				{
					if (cdd[cdir] == dir) break;
				}
				if (cdir == 8) return FALSE;

				/* Step 0 */
				y = p_ptr->py + ddy_cdd[cdir];
				x = p_ptr->px + ddx_cdd[cdir];

				if (cave_m_idx[y][x] > 0)
				{
					py_attack(y, x, 0);
				}
				else 
				{
					/* Nothing happens */
					msg_print("There isn't a monster there!");
					return TRUE;
				}
				
				/* prepare the escape */
				y = p_ptr->py + ddy_cdd[(cdir + 4) % 8];
				x = p_ptr->px + ddx_cdd[(cdir + 4) % 8];

				if (cave_m_idx[y][x] > 0)
				{
					/* attack */
					msg_print("There is a monster in the way!");
					return TRUE;
				}
				else if (cave_passable_bold(y, x))
				{
					/* Note - this uses no energy, skips traps, */
				 	/* and doesn't search - it's direct */
				 	monster_swap(p_ptr->py, p_ptr->px, y, x);
					/* success */
					return TRUE;
				}
				else 
				{
					/* Failure */
					msg_print("You can't escape that way!");
					return TRUE;
				}
				break;

		case 15: /* Dash */
				 if (!get_rep_dir(&dir)) return FALSE;
				 if (dir == 5) return FALSE;

				 /* Step 0 */
				 y = p_ptr->py + ddy[dir];
				 x = p_ptr->px + ddx[dir];

				 if (cave_m_idx[y][x] > 0)
				 {
				 	msg_print("There's a monster in the way!");
				 	return TRUE;
				 }
				 else if (cave_passable_bold(y, x))
				 {
				 	/* Note - this uses no energy, skips traps, */
				 	/* and doesn't search - it's direct */
				 	monster_swap(p_ptr->py, p_ptr->px, y, x);
				 }
				 else 
				 {	
				 	/* Nothing happens */
				 	msg_print("Argh - Your dash failed!");
				 	return TRUE;
				 }

				 /* Step 1 */
				 y = p_ptr->py + ddy[dir];
				 x = p_ptr->px + ddx[dir];
				 
				 if (cave_m_idx[y][x] > 0)
				 {
				 	msg_print("There's a monster in the way!");
				 	return TRUE;
				 }
				 else if (cave_passable_bold(y,x))
				 {
				 	/* Note - this uses no energy, skips traps, */
				 	/* and doesn't search - it's direct */
				 	monster_swap(p_ptr->py, p_ptr->px, y, x);
				 }
				 else 
				 {	
				 	/* Nothing happens */
				 	msg_print("Argh - Your dash failed!");
				 	return TRUE;
				 }

				 /* Step 2 */
				 y = p_ptr->py + ddy[dir];
				 x = p_ptr->px + ddx[dir];
				 
				 if (cave_m_idx[y][x] > 0)
				 {
				 	/* do a normal attack */
				 	msg_print("There's a monster in the way!");
				 	return TRUE;
				 }
				 else if (cave_passable_bold(y,x))
				 {
				 	/* Note - this uses no energy, skips traps, */
				 	/* and doesn't search - it's direct */
				 	monster_swap(p_ptr->py, p_ptr->px, y, x);
				 }
				 else 
				 {	
				 	/* Nothing happens */
				 	msg_print("Argh - Your dash failed!");
				 	return TRUE;
				 }

				 /* Step 3 */
				 y = p_ptr->py + ddy[dir];
				 x = p_ptr->px + ddx[dir];
				 
				 if (cave_m_idx[y][x] > 0)
				 {
				 	 msg_print("There's a monster in the way!");
				 	 return TRUE;
				 }
				 else if (cave_passable_bold(y,x))
				 {
				 	/* Note - this uses no energy, skips traps, */
				 	/* and doesn't search - it's direct */
				 	monster_swap(p_ptr->py, p_ptr->px, y, x);
				 }
				 else 
				 {	
				 	/* Nothing happens */
				 	return TRUE;
				 }
				 break;

		case 16: /* Step in and Strike */
				if (!get_rep_dir(&dir)) return FALSE;
				if (dir == 5) return FALSE;

				/* Step 0 */
				y = p_ptr->py + ddy[dir];
				x = p_ptr->px + ddx[dir];
				
				if (cave_m_idx[y][x] > 0)
				{
					msg_print("There is a monster in the way!");
					py_attack(y, x, 0);
					return TRUE;
				}
				else if (cave_passable_bold(y, x))
				{
					/* Note - this uses no energy, skips traps, */
					/* and doesn't search - it's direct */
					monster_swap(p_ptr->py, p_ptr->px, y, x);
				}
				else 
				{	
					/* Nothing happens */
					msg_print("Argh - You can't step there!");
					return TRUE;
				}

				/* prepare the attack */
				y = p_ptr->py + ddy[dir];
				x = p_ptr->px + ddx[dir];

				if (cave_m_idx[y][x] > 0)
				{
					/* attack */
					py_attack(y, x, 0);
					return TRUE;
				}
				break;

		case 17: /* Strikethrough */
				 if (!get_rep_dir(&dir)) return FALSE;
				if (dir == 5) return FALSE;

				/* Step 0 */
				y = my = p_ptr->py + ddy[dir];
				x = mx = p_ptr->px + ddx[dir];
				
				if (cave_m_idx[y][x] > 0)
				{
					py_attack(y, x, 0);
				}
				else if (cave_passable_bold(y, x))
				{
					msg_print("There's no monster here!");
					return TRUE;
				}
				else 
				{	
					/* Nothing happens */
					msg_print("Something is wrong!");
					return TRUE;
				}

				/* prepare the stepthrough */
				y = p_ptr->py + 2 * ddy[dir];
				x = p_ptr->px + 2 * ddx[dir];

				if (cave_m_idx[y][x] > 0)
				{
					msg_print("There is a monster in the way!");
					return TRUE;
				}
				else if (cave_passable_bold(y, x))
				{
					/* Note - this uses no energy, skips traps, */
					/* and doesn't search - it's direct */
					monster_swap(p_ptr->py, p_ptr->px, y, x);
					/* success - should there be a rear attack here? */
					if (cave_m_idx[my][mx] > 0) py_attack(my, mx, PY_ATTACK_CRITICAL);
					return TRUE;
				}
				else 
				{	
					/* Nothing happens */
					msg_print("Argh - You can't step there!");
					return TRUE;
				}
				break;

		case 18:/* Chain Hook */ 
				/* should change this into a function  XCCCX */

				if (!get_aim_dir(&dir)) return FALSE;
				if ((dir == 5) && target_okay())
				{
					tx = p_ptr->target_col;
					ty = p_ptr->target_row;
				}
				else
				 {
					tx = p_ptr->px + ddx[dir];
					ty = p_ptr->py + ddy[dir];
				 }

				 if (cave_m_idx[ty][tx] < 1)
				 {
					msg_print("There's nothing there to hook!");
					/* Failure */
					return FALSE;
				 }
				 if (!player_has_los_bold(ty,tx))
				 {
				 	msg_print("You can't hook monsters through walls!");
				 	/* Failure */
				 	return FALSE;
				 }
			
				 /* Need something better than player level */
				 if ((cave_info[ty][tx] & (CAVE_ICKY)) || (distance(ty, tx, p_ptr->py, p_ptr->px) > p_ptr->lev * 3 / 2 + 10))
				 { 
					msg_print("Failed to grab monster.");
					/* Failure */
					return FALSE;
				 }

				 m_ptr = &m_list[cave_m_idx[ty][tx]];
				 m_idx = cave_m_idx[ty][tx];
				 monster_desc(m_name, m_ptr, 0);
				 msg_format("You pull back %s.", m_name);
								
				 /* Wake the monster up */
				 m_ptr->csleep = 0;

				 /* Move monster near player (also updates "m_ptr->ml"). */
				 teleport_to_player(ty, tx, p_ptr->py, p_ptr->px);

				 /* Update the monster (new location) */
				 update_mon(m_idx, TRUE, FALSE);

				 /* Handle stuff XXX XXX XXX */
				 handle_stuff();

				 /* Success */
				 return TRUE;

				 break;

		case 19: /* Knife Kata */
				 for (b = 0; b < 25; b++)
				 {
					 /* Predict the "target" location */
					 ty = p_ptr->py + ddy_cdd[(b * 3) % 8];
					 tx = p_ptr->px + ddx_cdd[(b * 3) % 8];

					 if (cave_m_idx[ty][tx] > 0)
					 {
					 	/* Attack */
					 	py_attack(ty, tx, 0);
					 }
				}
				 break;
		case 20: /* Death Dance */
				 msec = op_ptr->delay_factor * op_ptr->delay_factor * 2;
				 dance = rand_range(20, 45);
				 for (b = 0; b < dance; b++)
				 {
					 /* Predict the "target" location */
					 ty = p_ptr->py + ddy_cdd[(b * 3) % 8];
					 tx = p_ptr->px + ddx_cdd[(b * 5) % 8];
					 
					 if (cave_m_idx[ty][tx] > 0)
					 {
					 	/* Attack */
					 	py_attack(ty, tx, 0);
					 }
					 else if (cave_passable_bold(ty, tx))
					 {
						/* Note - this uses no energy, skips traps, */
						/* and doesn't search - it's direct */
						monster_swap(p_ptr->py, p_ptr->px, ty, tx);
						lite_spot(ty,tx);

						ty = p_ptr->py + ddy_cdd[(b * 3) % 8];
						tx = p_ptr->px + ddx_cdd[(b * 5) % 8];
						if (randint(b) > randint (b))
						{
							 if (cave_m_idx[ty][tx] > 0)
							 {
							 	/* Attack */
							 	py_attack(ty, tx, 0);
							 }
							 else if (cave_passable_bold(ty, tx))
							 {
								/* Note - this uses no energy, skips traps, */
								/* and doesn't search - it's direct */
								monster_swap(p_ptr->py, p_ptr->px, ty, tx);
							 	lite_spot(ty,tx);
							 }
						}
					 }
					 else 
					 {
						 ty = p_ptr->py + ddy_cdd[(b * 7) % 8];
						 tx = p_ptr->px + ddx_cdd[(b * 1) % 8];
						 if (cave_m_idx[ty][tx] > 0)
						 {
						 	/* Attack */
						 	py_attack(ty, tx, 0);
						 }
						 else if (cave_passable_bold(ty, tx))
						 {
							/* Note - this uses no energy, skips traps, */
							/* and doesn't search - it's direct */
							monster_swap(p_ptr->py, p_ptr->px, ty, tx);
						 	lite_spot(ty,tx);
						 }

					 }
					 
					 Term_fresh();
					 Term_xtra(TERM_XTRA_DELAY, msec);
				 }
				 break;
	}
	return TRUE;
}

/*
 * do_cmd_mind calls this function if the player's class
 * is well, currently, nothing at all.
 */
static bool cast_qabalist_spell(int spell)
{

	/* spell code */
	switch (spell)
	{
				case 0:  break;
				case 1:  break;
				case 2:  break;
				case 3:  break;
				case 4:  break;
				case 5:  break;
				case 6:  break;
				case 7:  break;
				case 8:  break;
				case 9:  break;
				case 10: break;
				case 11: break;
				case 12: break;
				case 13: break;
				case 14: break;
				case 15: break;
				case 16: break;
				case 17: break;
				case 18: break;
				case 19: break;
				case 20: break;
	}
	return TRUE;
}

/*
 * do_cmd_cast calls this function based on player class
 */
 void do_cmd_mind(void)
{
	int             n = 0;
	int             chance;
	int             minfail = 0;
	int             old_csp = p_ptr->csp;
	mind_type       spell;
	bool            cast = FALSE;
	int             use_mind;
	
	/* not if confused */
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}

	/* get power */
	if (!get_class_power(&n, &chance)) return;

	if (p_ptr->pclass == CLASS_OFFICER)
		use_mind = MIND_OFFICER;
	else if (p_ptr->pclass == CLASS_AESTHETE)
		use_mind = MIND_AESTHETE;
	else if (p_ptr->pclass == CLASS_ADVENTURER)
		use_mind = MIND_EXPLORER;
	else if (p_ptr->pclass == CLASS_MEDIUM)
		use_mind = MIND_MEDIUM;
	else if (p_ptr->pclass == CLASS_DASHING_H)
		use_mind = MIND_HUSSAR;
	else if (p_ptr->pclass == CLASS_NATURAL)
		use_mind = MIND_NATURE;
	else if (p_ptr->pclass == CLASS_ROGUE)
		use_mind = MIND_NINJA;
	else {
			msg_print("You have no powers!");
			return;
		 }

	spell = mind_powers[use_mind].info[n];
	
	/* Verify "dangerous" spells */
	if (spell.mana_cost > p_ptr->csp)
	{
		/* Warning */
		msg_print("You do not have enough mana to use this power.");

		/* Verify */
		if (!get_check("Attempt it anyway? ")) return;
	}

	/* Failed spell */
	if (randint(100) < chance)
	{
		if (flush_failure) flush();
		msg_format("You failed to concentrate hard enough!");

		/* Take a turn */
		p_ptr->energy_use = 100;
		
		/* Sufficient mana */
		if (spell.mana_cost <= old_csp)
		{
			/* Use some mana */
			p_ptr->csp -= spell.mana_cost;
	
			/* Limit */
			if (p_ptr->csp < 0) p_ptr->csp = 0;
		}
	
		/* Over-exert the player */
		else
		{
			int oops = spell.mana_cost - old_csp;
	
			/* No mana left */
			p_ptr->csp = 0;
			p_ptr->csp_frac = 0;
			
			/* Message */
			msg_print("You faint from the effort!");
	
			/* Hack -- Bypass free action */
			(void)set_paralyzed(p_ptr->paralyzed + randint(5 * oops + 1));
	
			/* Damage WIS (possibly permanently) */
			if (randint(100) < 50)
			{
				bool perm = (randint(100) < 25);
	
				/* Message */
				msg_print("You have damaged your mind!");
	
	
				/* Reduce constitution */
				(void)dec_stat(A_EGO, 15 + randint(10), perm);
			}
		}

		/* Redraw mana */
		p_ptr->redraw |= (PR_MANA);

		/* Window stuff */
		 p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

	}
	else
	{
		sound(SOUND_ZAP);

		switch(use_mind)
		{
		case MIND_OFFICER:
			/* Cast the spell */
			cast = cast_officer_spell(n);
			break;
		case MIND_AESTHETE:
			/* Cast the spell */
			cast = cast_aesthete_spell(n);
			break;
		case MIND_EXPLORER:
			/* Cast the spell */
			cast = cast_explorer_spell(n);
			break;
		case MIND_MEDIUM:
			/* Cast the spell */
			cast = cast_medium_spell(n);
			break;
		case MIND_HUSSAR:
			/* Cast the spell */
			cast = cast_hussar_spell(n);
			break;
		case MIND_NATURE:
			/* Cast the spell */
			cast = cast_nature_spell(n);
			break;
		case MIND_NINJA:
			/* Cast the spell */
			cast = cast_ninja_spell(n);
			break;
		case MIND_QABALIST:
			/* Cast the spell */
			cast = cast_qabalist_spell(n);
			break;
		default:
			msg_format("Mystery power:%d, %d",use_mind, n);
			return;
		}
	}
	if (!cast) return;
	
	/* Take a turn */
	p_ptr->energy_use = 100;
	
	/* Sufficient mana */
	if (spell.mana_cost <= old_csp)
	{
		/* Use some mana */
		p_ptr->csp -= spell.mana_cost;

		/* Limit */
		if (p_ptr->csp < 0) p_ptr->csp = 0;
	}

	/* Over-exert the player */
	else
	{
		int oops = spell.mana_cost - old_csp;

		/* No mana left */
		p_ptr->csp = 0;
		p_ptr->csp_frac = 0;
		
		/* Message */
		msg_print("You faint from the effort!");

		/* Hack -- Bypass free action */
		(void)set_paralyzed(p_ptr->paralyzed + randint(5 * oops + 1));

		/* Damage WIS (possibly permanently) */
		if (randint(100) < 50)
		{
			bool perm = (randint(100) < 25);

			/* Message */
			msg_print("You have damaged your mind!");


			/* Reduce constitution */
			(void)dec_stat(A_EGO, 15 + randint(10), perm);
		}
	}

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1); 
}


/* add in do cmd browse here */
