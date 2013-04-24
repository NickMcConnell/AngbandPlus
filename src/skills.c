/* File: skills.c */

/* Purpose: Stores player skills */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

/* Useful variables */
/* I'm not sure whether these are needed any more at all */
bool displayed = FALSE;

/* 
 * Welcome to [Steam]band skills implimentation 
 * This is an array containing the skills  - This array is a part of the 
 * player type array, so it can be reached by p_ptr->skills[NAMEOFSKILL].skill_rank
 *
 * The names of the skills can be found in defines.h. 
 *
 * Inactive skills have a value of -2, and which skills are active 
 * are set by the race and class int functions at the end player creation. 
 * All of this information is hardcoded. 
 *
 * The reason that several skills appear in the list below mutiple times is because
 * there are different levels of 'quality' of a skill. Some classes are just not 
 * as good as others at disarming traps, so there is a DISARM_GOOD, DISARM_NORM, and
 * DISARM_POOR skill. There is a function at the end to check for race and class
 * duplication of these skills - and then remove the duplicates, keeping their highest
 * natural value
 *
 * When implmenting skills through out the code we assign the skill to a local variable, 
 * and then check that variable to make sure that it has a value of at least one.
 * If it isn't at least one we set that variable to one. This allows classes
 * who lack skills to do things without having the skill value of -2 to screw up 
 * the in-game formula. Skills are generally very specific and as you raise your
 * skills new skills will become available. These new skills are even _more_ specific.
 *
 * Skills have a natrual maximum of 20 - and cannot be raised higher than your
 * level the skill was gained at + 2. 
 * 
 * NOTE: It is this code's _WORST_ TERRIBLE NIGHTMARE ever if a character somehow
 * ends up with a positive value for more than one skill in the same 'grouping'
 * for instance if a character ends up with positive values for both 
 * p_ptr->skills[SK_DISARM_GOOD]skill_rank(or max) and
 * p_ptr->skills[SK_DISARM_POOR]skill_rank(or max).
 * If this happens, there are a _few_ safegaurds in place (using the worse values)
 * REGARDLESS something will break. be careful -CCC
 *
 * Thanks to Rbowman, Antimatter(RML), Yumi, and various other people for their
 * assitance with this.
 *
 * Skill categories are racial/class/resistance/general, combat, spells/items/other. 
 *
 * Implimentation by Courtney Campbell. -CCC
 */
player_skills skills[N_SKILLS] = {
	 
	/* 0-4 */
	{"Disarming Traps", -2, -2, -2, 0, 1, 
		"This is your ability to open locks and disarm traps.\
		You are particularly talented at this skill."},
	{"Disarming Traps", -2, -2, -2, 0, 1, 
		"This is your ability to open locks and disarm traps.\
		You have an average ability in this skill."},
	{"Disarming Traps", -2, -2, -2, 0, 1, 
		"This is your ability to open locks and disarm traps.\
		You are all thumbs when it comes to traps and locks!"},
	{"Disable Machine", -2, -2, -2, 0, 1, 
		"This skill allows you to disable rogue and dangerous machines.\
		When you strike an automaton or construct there is a chance that you will drain some of it's power (mana)."},
	{"Saving Throw", -2, -2, -2, 0, 1, 
		"This skill improves your ability to protect yourself versus dangerous and harmful attacks."},

	/* 5-9 etc. */	
	{"Assassination", -2, -2, -2, 0, 1, 
		"If a monster is asleep, assassination allows you to strike a deadly blow."},
	{"Theft", -2, -2, -2, 0, 1, 
		"Sometimes you can steal gold from monsters when you attack them.\
		The higher this skill is, the better you are at stealing from monsters."},
	{"Master Theft", -2, -2, -2, 0, 1, 
		"Sometimes you can steal a *lot* of gold from monsters.\
		The higher this skill is, the more gold it is possible to get from monsters."},
	{"Stealth", -2, -2, -2, 0, 1, 
		"You are silent like the night.\
		Stealth is your ability to avoid detection."},
	{"Stealth", -2, -2, -2, 0, 1, 
		"Stealth is your ability to avoid detection."},

	/* 10 */		
	{"Stealth", -2, -2, -2, 0, 1, 
		"You make a loud racket.\
		Stealth is your ability to avoid detection"},
	{"Superstealth", -2, -2, -2, 0, 1, 
		"You are neither seen nor heard.\
		Superstealth allows you to dampen the very noise around you and cloud your form making yourself almost impossible to detect."},
	{"Searching", -2, -2, -2, 0, 1, 
		"Needles are *easy* to find in haystacks.\
		Searching is your ability to detect traps and find secret doors."},
	{"Searching", -2, -2, -2, 0, 1, 
		"Searching is your ability to detect traps and find secret doors."},
	{"Searching", -2, -2, -2, 0, 1, 
		"You've lost your hat on your head before.\
		Searching is your ability to detect traps and find secret doors."},
	
	/* 15 */	
	{"Trailblazer", -2, -2, -2, 0, 1, 
		"Because noone has ever been there is reason enough to go.\
		Trailblazing increases your searching ability and adds to your wound points."},
	{"Spot Weakness", -2, -2, -2, 0, 1, 
		"Everyone has a weak spot.\
		You are good at striking the weak spot of monsters allowing you to do massive critical damage."},
	{"Keen Eyes", -2, -2, -2, 0, 1, 
		"Range is just a suggestion for maximum accuracy.\
		Your keen eyes allow you to extend the range of your firearms up to 50'"},
	{"Sense Dungeon", -2, -2, -2, 0, 1, "Nothing"},
	{"Dungeon Lore", -2, -2, -2, 0, 1, "Nothing"},
	
	/* 20 */	
	{"Pain Tolerance", -2, -2, -2, 0, 2, 
		"'Tis but a flesh wound!\
		Your tolerance for pain increases and eliminates the penalties incurred while wounded. "},
	{"Perilous Sorcery", -2, -2, -2, 0, 3, 
		"If you take risks, great power can be gotten very quickly.\
		Lowers your elemental and mental resistances, but increases the spells you can access.\
		This skill does not take effect until it is raised past one rank. "},
	{"Luck", -2, -2, -2, 0, 1, 
		"Felix ille tamen corvo quoque rarior albo."},
	{"Erudite", -2, -2, -2, 0, 3, 
		"Hours spent in reading and study sharpen your mind, and weaken the body.\
		This skill raises your Schooling and Ego at the cost of your Muscle."},
	{"DEFAULT", -2, -2, -2, 0, 1, "Nothing"},
	
	/* 25 */
	/* racial skills */	
	{"British", -2, -2, -2, 0, 1, "Nothing"},
	{"British2", -2, -2, -2, 0, 1, "Nothing"},
	{"Asiatic", -2, -2, -2, 0, 1, "Nothing"},
	{"Asiatic2", -2, -2, -2, 0, 1, "Nothing"},
	{"American", -2, -2, -2, 0, 1, "Nothing"},
	
	/* 30 */	
	{"American2", -2, -2, -2, 0, 1, "Nothing"},
	{"African", -2, -2, -2, 0, 1, "Nothing"},
	{"African2", -2, -2, -2, 0, 1, "Nothing"},
	{"Connoisseur", -2, -2, -2, 0, 1, 
		"You can prepare fantastic meals.\
		As connoisseur increases, so do the benefits from the meal."},
	{"French2", -2, -2, -2, 0, 1, "Nothing"},
	
	/* 35 */	
	{"Spanish", -2, -2, -2, 0, 1, "Nothing"},
	{"Spanish2", -2, -2, -2, 0, 1, "Nothing"},
	{"Ransack & Plunder", -2, -2, -2, 0, 1, 
		"You can quickly and effectively locate loot."},
	{"German2", -2, -2, -2, 0, 1, "Nothing"},
	{"Russian", -2, -2, -2, 0, 1, "Nothing"},

	/* 40 */	
	{"Russian2", -2, -2, -2, 0, 1, "Nothing"},
	{"Finnish", -2, -2, -2, 0, 1, "Nothing"},
	{"Finnish2", -2, -2, -2, 0, 1, "Nothing"},
	{"Arabic", -2, -2, -2, 0, 1, "Nothing"},
	{"Arabic2", -2, -2, -2, 0, 1, "Nothing"},

	/* 45 */	
	{"Stonelore", -2, -2, -2, 0, 1, 
		"The earth holds no secrets from you.\
		By concentrating and examining your surroundings you can gather information about the dungeon."},
	{"Dwarf2", -2, -2, -2, 0, 1, "Nothing"},
	{"Fae Pathways", -2, -2, -2, 0, 1, 
		"The world is filled with mystic portals and pathways.\
		You can access and travel through these strange gates.\
		The more you know about fae pathways the father you can travel along them."},
	{"Brownie2", -2, -2, -2, 0, 1, "Nothing"},
	{"Daoinesidhe", -2, -2, -2, 0, 1, "Nothing"},

	/* 50 */	
	{"Daoinesidhe2", -2, -2, -2, 0, 1, "Nothing"},
	{"seeliefae", -2, -2, -2, 0, 1, "Nothing"},
	{"seeliefae2", -2, -2, -2, 0, 1, "Nothing"},
	{"unseelfae", -2, -2, -2, 0, 1, "Nothing"},
	{"unseeliefae2", -2, -2, -2, 0, 1, "Nothing"},

	/* 55 */
	/* Automata / Steam-Mecha skills */	
	{"Systems Cypher", -2, -2, -2, 0, 1, 
		"*tink* mmmmmwhirrrrrrrrrr-analysis complete.\
		Activating environmental preperations.\
		Systems cypher is your ability to hyper-cogitate, adapt, and prepare yourself to your environment."},
	{"Onslaught Cypher", -2, -2, -2, 0, 1, 
		"Onslaught cypher is your weapons system.\
		As you upgrade you recieve new, more powerful attacks."},
	{"Aegis Cypher", -2, -2, -2, 0, 1, 
		"This is a defensive utility allowing you to protect yourself from attacks, both physical and magical."},
	{"Utility Cypher", -2, -2, -2, 0, 1, 
		"Utility cypher is a sensor array allows you to analyze your environment and discover previously unknown information."},
	{"Rocketry Cypher", -2, -2, -2, 0, 1, 
		"The rocketry cypher increases the damage of your rockets."},

	/* 60 */	
	{"Djinn1", -2, -2, -2, 0, 1, "Nothing"},
	{"Djinn2", -2, -2, -2, 0, 1, "Nothing"},
	{"Djinn3", -2, -2, -2, 0, 1, "Nothing"},
	{"Demonic Attunement", -2, -2, -2, 0, 1, 
		"This is the degree of your alignment with dark forces.\
		The dark forces give you the power to cause fear and confusion."},
	{"Dark Charm", -2, -2, -2, 0, 1, 
		"This is the ability to dominate the weak willed creatures that surround you."},

	/* 65 */	
	{"Rakshasa3", -2, -2, -2, 0, 1, "Nothing"},
	{"Rock Tossing", -2, -2, -2, 0, 1, 
		"RARGH. YOU TOSS ROCK GOOD!\
		THIS MAKE TOSS ROCK SMASH GOOD!"},
	{"Bezerk Strength", -2, -2, -2, 0, 1, 
		"You have an untapped reserve of strength."},
	{"Troll", -2, -2, -2, 0, 1, "Nothing"},
	{"Etheric Attunement", -2, -2, -2, 0, 1, 
		"This is your degree of alignment with the etherial plane.\
		It prevents you from taking damage as you move through solid rock and helps you dodge certain spells."},

	/* 70 */	
	{"ghost2", -2, -2, -2, 0, 1, "Nothing"},
	{"Cowardice", -2, -2, -2, 0, 1, 
		"This is how good you are at running away."},
	{"old1", -2, -2, -2, 0, 1, "Nothing"},
	{"old12", -2, -2, -2, 0, 1, "Nothing"},
	{"old13", -2, -2, -2, 0, 1, "Nothing"},

	/* 75 */
	/* combat skills */	
	{"Neophyte Combat", -2, -2, -2, 0, 2, 
		"Neophyte combat is your basic melee combat abilities.\
		It affects both martial arts and weapon combat."},
	{"Standard Combat", -2, -2, -2, 0, 2, 
		"Standard combat is your basic melee weapon abilities.\
		It affects your ability to successfully land a blow, as well as slightly increasing the damage you can cause with a weapon. "},
	{"Advanced Combat", -2, -2, -2, 0, 2, 
		"Advanced combat is your advanced melee weapon abilities.\
		It somewhat affects your ability to successfully land a blow, as well as increasing the damage you can cause with a weapon. "},
	{"Master Combat", -2, -2, -2, 0, 2, 
		"Master combat is your mastery of melee weapon strikes.\
		It somewhat affects your ability to successfully land a blow, as well as increasing the damage you can cause with a weapon. "},
	{"Power Strike", -2, -2, -2, 0, 2, 
		"Power strike is your ability to land *powerful* blows with a weapon.\
		It lets you get the most out of heavy weapons, and increases the striking power of lighter weapons."},

	/* 80 */
	{"Critical Strike", -2, -2, -2, 0, 2, 
		"Critical strike allows you to strike devestating critical blows.\
		It increases both the frequency and power of criticals."},
	{"Accurate Strike", -2, -2, -2, 0, 2, 
		"Accurate strike increases your precision and accuracy with your weapon."},
	{"Vicious Strike", -2, -2, -2, 0, 2, 
		"Vicious strike is your ability to strike hard and fast with your weapon, causing additional damage."},
	{"Swift Blow", -2, -2, -2, 0, 2, 
		"Swift blow increases the speed of your blows.\
		With the weapon finesse skill this helps to determine the attacks per round you receive with weapons only.\
		If you are very strong, raising this skill is most effective."},
	{"Weapon Finesse", -2, -2, -2, 0, 2, 
		"Weapon finesse increases your ability to handle large weapons.\
		With the swift blows skill this helps determine the attacks per round you receive with weapons only.\
		If you are very agile, raising this skill is most effective."},

	/* 85 */
	{"Martial Arts", -2, -2, -2, 0, 2, 
		"Martial arts is your skill at either pugilism, modern defensive techniques, or the ancient eastern art of self defense without weapons."},
	{"Eagle Strike Arts", -2, -2, -2, 0, 2, 
		"Eagle strike arts is your ability to increase the damage you can cause with your bare hands."},
	{"Dragon Claw Arts", -2, -2, -2, 0, 2, 
		"Dragon claw arts is your ability to greatly increase the damage you can cause with your bare hands.\
		It allows the possiblity of very deadly blows and holds."},
	{"Neophyte Firearms", -2, -2, -2, 0, 2, 
		"Neophyte firearms is your basic ranged weapon combat ability.\
		It affects your accuracy with guns."},
	{"Standard Firearms", -2, -2, -2, 0, 2, 
		"Standard firearms is your standard ranged weapon combat ability.\
		It further affects your accuracy with guns."},

	/* 90 */
	{"Advanced Firearms", -2, -2, -2, 0, 2, 
		"Advanced firearms is your advanced ranged weapon combat ability.\
		It still further increases your accuracy with guns."},
	{"Master Firearms", -2, -2, -2, 0, 2, 
		"Master firearms is your mastery of ranged weapons."},
	{"Pistols", -2, -2, -2, 0, 2, 
		"This is your skill with pistols.\
		It greatly increases your accuracy with pistols."},
	{"Rifles", -2, -2, -2, 0, 2, 
		"This is your skill with rifles.\
		It greatly increases your accuracy with rifles."},
	{"Shotguns", -2, -2, -2, 0, 2, 
		"This is your skill with shotguns.\
		It greatly increases your accuracy with shotguns."},

	/* 95 */
	{"Critical Shot", -2, -2, -2, 0, 2, 
		"Critical shot allows you to strike devestating critical attacks with your firearm.\
		It increases botht he frequency and power of your criticals."},
	{"Accurate Shot", -2, -2, -2, 0, 2, 
		"Accurate shot greatly increases the accuracy of your firearm attack."},
	{"Vicious Shot", -2, -2, -2, 0, 2, 
		"Vicious shot is your ability to time your shots so that they do more damage."},
	{"Swift Shot", -2, -2, -2, 0, 2, 
		"Swift shot is your ability to rapidly reload and fire your firearm."},
	{"Throwing", -2, -2, -2, 0, 2, 
		"Throwing is your ability to toss objects effectively with damaging effects."},

	/* 100 */
	{"Advance Throwing", -2, -2, -2, 0, 2, 
		"Advanced throwing is your advanced ability to toss objects with damaging effects.\
		It also increases the range of your throw."},
	{"Master Throwing", -2, -2, -2, 0, 2, 
		"Master throwing is your mastery of thrown objects.\
		It also increases the range of your throw."},
	{"Power Throw", -2, -2, -2, 0, 2, 
		"Power throw greatly increases the damage of thrown objects."},
	{"Critical Throw", -2, -2, -2, 0, 2, 
		"Critical throw allows you to strike devestating critical throws.\
		It increases both the frequency and power of your criticals."},
	{"Throwing Accuracy", -2, -2, -2, 0, 2, 
		"Throwing accuracy greatly increases the accuracy of your throws."},

	/* 105-109 */	
	{"Hafted Weapons", -2, -2, -2, 0, 2,
		"Hafted weapons is your proficiency with blunt hafted weapons, such as whips, maces, and hammers."},
	{"Polearms", -2, -2, -2, 0, 2, 
		"Polearms is your proficiency with piercing polearms, such as javelins, spears, pikes, and halberds."},
	{"Swords", -2, -2, -2, 0, 2, 
		"Swords is your proficiency with slashing swords, such as rapiers, sabres, scimitars, and cutlasses."},
	{"Daggers", -2, -2, -2, 0, 2, 
		"Daggers is your proficiency with piercing daggers, such as throwing knives, short swords, and bowie knives."},
	{"Axes", -2, -2, -2, 0, 2, 
		"Axes is your proficiency with slashing axes, such as throwing hatchets, and beaked, broad, battle, and great axes."},

	/* 110-114 */	
	{"Blunt Weapons", -2, -2, -2, 0, 2, 
		"Blunt weapons is your proficiency with bludgeoning weapons, such as canes, clubs, and quarterstaves."},
	{"Hafted Mastery", -2, -2, -2, 0, 2, 
		"Hafted mastery is your mastery of hafted weapons."},
	{"Polearm Mastery", -2, -2, -2, 0, 2, 
		"Polearm mastery is your mastery of polearms."},
	{"Sword Mastery", -2, -2, -2, 0, 2, 
		"Sword mastery is your mastery of swords."},
	{"Dagger Mastery", -2, -2, -2, 0, 2, 
		"Dagger mastery is your mastery of daggers."},

	/* 115-119 */	
	/* note the "special" names for martial defense */
	{"Axe Mastery", -2, -2, -2, 0, 2, 
		"Axe mastery is your mastery of axes."},
	{"Blunt Mastery", -2, -2, -2, 0, 2, 
		"Blunt mastery is your mastery of blunt weapons."},
	{"Acrobatics", -2, -2, -2, 0, 2, 
		"Acrobatics increases your defensive abilities, as well as eventually allowing you to evade attacks and move around quickly."},
	{"Iron Skin", -2, -2, -2, 0, 2, 
		"Iron skin toughens up your skin making it harder for monsters to successfully hit you."},
	{"The Gentle Way", -2, -2, -2, 0, 2, 
		"The gentle way allows you to counterstrike monsters that attack you as well as providing resistance to some elemental attacks."},

	/* 120-124 */	
	{"Fleetness", -2, -2, -2, 0, 2, 
		"Fleetness increases your speed."},
	{"Double Throw", -2, -2, -2, 0, 2, "Nothing"},
	{"Quick Toss", -2, -2, -2, 0, 2, 
		"Quick Toss greatly reduces the time it takes to throw objects at monsters."},
	{"Dirty Fighting", -2, -2, -2, 0, 2, 
		"Dirty fighting allows you to do a bit of extra damage to monsters."},
	{"", -2, -2, -2, 0, 2, ""},

	/* 125-129 */	
	{"Command Techniques", -2, -2, -2, 0, 1, 
		"Command techniques is the Officer's ability to lead and command various denizens of the dungeon.\
		Activated by class 'p'owers. (press 'p')"},
	{"Combat Techniques", -2, -2, -2, 0, 1, 
		"Combat techniques is the Officer's ability to perform special combat maneuvers.\
		Activated by class 'p'owers. (press 'p')"},
	{"Presence", -2, -2, -2, 0, 1, 
		"Presence increases the Officer's ability to lead and command his troops."},
	{"Debutante Ability", -2, -2, -2, 0, 1, 
		"The debutante ability of the Aesthete allows him to sense valueables, turn items into gold, escape, and return quickly back to town.\
		Activated by class 'p'owers. (press 'p')"},
	{"Artificer Ability", -2, -2, -2, 0, 1, 
		"Artificer ability is the ability of the aesthete to enhance and recharge items.\
		Activated by class 'p'owers. (press 'p')"},

	/* 130-134 */	
	{"Survival Technique", -2, -2, -2, 0, 1, 
		"Survival technique provides several basic useful powers.\
		Activated by class 'p'owers. (press 'p')"},
	{"Telepathy", -2, -2, -2, 0, 1, 
		"Telepathy is the Medium's ability to sense and affect the minds of the creatures around him.\
		Activated by class 'p'owers. (press 'p')"},
	{"Clairsentience", -2, -2, -2, 0, 1, 
		"Clairsentience is the Medium's ability to know the unknown.\
		Activated by class 'p'owers. (press 'p')"},
	{"Psychometabolism", -2, -2, -2, 0, 1, 
		"Psychometabolism is the Medium's ability to alter and repair his body using his mind.\
		Activated by class 'p'owers. (press 'p')"},
	{"Mesmeric Will", -2, -2, -2, 0, 1, 
		"Mesmeric will increases the power of the Medium's abilities."},

	/* 135-139 */	
	{"Combat Techniques", -2, -2, -2, 0, 1, 
		"Combat techniques are the various stances and strikes learned by the Dashing Hussars.\
		Activated by class 'p'owers. (press 'p')"},
	{"Elite Maneuvers", -2, -2, -2, 0, 1, 
		"Elite maneuvers are the advanced stances and strikes learned by the Dashing Hussars.\
		Activated by class 'p'owers. (press 'p')"},
	{"Flora", -2, -2, -2, 0, 1, 
		"Flora is the Naturalist's powers over plants.\
		Activated by class 'p'owers. (press 'p')"},
	{"Fauna", -2, -2, -2, 0, 1, 
		"Fauna is the Naturalist's powers over animals.\
		Activated by class 'p'owers. (press 'p')"},
	{"Elemental", -2, -2, -2, 0, 1, 
		"Elemental is the Naturalist's powers over the elements.\
		Activated by class 'p'owers. (press 'p')"},

	/* 140-144 */	
	{"Ninja Mysticism", -2, -2, -2, 0, 1, 
		"Ninja Mysticism is the Ninja's access to ancient secret magics.\
		Activated by class 'p'owers. (press 'p')"},
	{"Ninja Stealth", -2, -2, -2, 0, 1, 
		"Ninja Stealth is the Ninja's ability to travel long distances, remain unoticed, and learn about items.\
		Activated by class 'p'owers. (press 'p')"},
	{"Ninjutsu", -2, -2, -2, 0, 1, 
		"Ninjutsu is the Ninja's secret martial maneuvers.\
		Activated by class 'p'owers. (press 'p')"},
	{"Hebrew", -2, -2, -2, 0, 1, "Nothing"},
	{"Qabala", -2, -2, -2, 0, 1, "Nothing"},

	/* 145-149 */	
	{"Gematria", -2, -2, -2, 0, 1, "Nothing"},
	{"Bribery", -2, -2, -2, 0, 1, 
		"Bribery is the Aesthete's skill at paying monsters gold to leave him alone."},
	{"Art Appreciation", -2, -2, -2, 0, 3, 	
		"Art appreciation is the Aesthete's ability to finagle a better sale price from items out of merchants."},
	{"Battle Endurance", -2, -2, -2, 0, 2, 
		"Battle endurance increases your wound points."},
	{"Earth's Hearth", -2, -2, -2, 0, 1, 
		"Earth's hearth increases your reserve of spellpoints."},

	/* 150-154 */	
	/* Note: Have to be very careful that the r/c descrptions match */
	/* what skills are available- Should add a check in cmd-book.c */
	{"Latin {*}", -2, -2, -2, 0, 3, 
		"Latin is your ability to read Latin.\
		It is very helpful in decyphering old mystic works."},
	{"Occult Lore {O}", -2, -2, -2, 0, 3, 
		"Occult Lore is your understanding of the occult.\
		It allows comprehension of basic occult works. "},
	{"Adv. Occult Lore {O}", -2, -2, -2, 0, 3, 
		"Advanced Occult Lore is your advanced understanding of occult topics and lore.\
		It allows comprehension of advanced occult works."},
	{"Cthulhu Mythos {O}", -2, -2, -2, 0, 3, 
		"Cthulhu mythos is your understanding of dark and forbidden cthulhu lore.\
		It allows understanding of the most powerful and dark occult works."},
	{"Ritual Magic {O}", -2, -2, -2, 0, 3, 
		"Ritual magic is your understanding of the techniques and processes of ritual magic.\
		It increases the potency and power of your occult spells."},

	/* 155-159 */	
	{"Thaumic Energy {O}", -2, -2, -2, 0, 3, 
		"Thaumic energy is your understanding of magical thaumic energy.\
		Raising this skill increases the safety of your arcane spells."},
	{"Thaumaturgy {O}", -2, -2, -2, 0, 3, 
		"Thaumaturgy further increases your understanding of magical thaumic energy.\
		Raising this skill further increases the safety of your arcane spells."},
	{"Thelma {O}", -2, -2, -2, 0, 3, 
		"Thelma is your unity with the all.\
		It phenomenally increases the power of your spells."},
	{"Spirituality {S}", -2, -2, -2, 0, 3, 
		"Spirituality is your level of being in touch with the divine.\
		It allows you to learn basic prayers."},
	{"Prayer {S}", -2, -2, -2, 0, 3, 
		"Prayer is your skill at prayer.\
		It allows you to learn advanced prayers."},

	/* 160-164 */	
	{"Devotion {S}", -2, -2, -2, 0, 3, 
		"Devotion is your level of devotion to god.\
		It allows you to learn the most powerful prayers."},
	{"Lesser Warding {S}", -2, -2, -2, 0, 3, 
		"Lesser warding is your knowledge of lesser wards.\
		It increases the saftey of your prayers."},
	{"Greater Warding {S}", -2, -2, -2, 0, 3, 
		"Greater warding is your knowledge of greater wards.\
		It increases the saftey of your prayers."},
	{"Tempered Will {*}", -2, -2, -2, 0, 3, 
		"Tempered will is the strength of your will.\
		It increases the power of all your incantations."},
	{"Hardened Will {*}", -2, -2, -2, 0, 3, 
		"Hardened will is the force of your will.\
		It increases the power of all your spells."},

	/* 165-169 */	
	{"Iron Will {*}", -2, -2, -2, 0, 3, 
		"Iron will is the center of your will.\
		It increases the power of all your spells."},
	{"Spell Quickening {*}", -2, -2, -2, 0, 3, 
		"Spell quickening is your ability to call up and discharge magic quickly and with ease.\
		It reduces the amount of time it takes to cast a spell."},
	{"Spiritual Infusion", -2, -2, -2, 0, 3, 
		"Spiritual infusion allows you to draw upon the spirit realm to add spirit energy to your melee attacks."},
	{"Mental Resistance", -2, -2, -2, 0, 3, 
		"Mental Resistance provides resistance to several types of elemental attacks."},
	{"Spiritual Shield", -2, -2, -2, 0, 3,
		"Spiritual shield creates a sphere of spirit energy that surrounds you making it harder for monsters to strike you."},

	/* 170-174 */	
	{"Spiritual Healing", -2, -2, -2, 0, 3, 
		"Spiritual healing increases the rate at which you recover from damage."},
	{"Spiritual Speed", -2, -2, -2, 0, 3, 
		"The spirits themselves provide you with extra energy allowing you to move much quicker than mortals."},
	{"Anatomy", -2, -2, -2, 0, 3, 
		"Anatomy is your knowledge of biology and the human body.\
		It increases the power and frequency of your critcals versus living creatures."},
	{"Sabotage", -2, -2, -2, 0, 1, 
		"Sabotage is your ability to effectively destroy or disable non-living creatures.\
		It increases the power and frequency of your criticals versus non-living creatures."},
	{"Theology", -2, -2, -2, 0, 3, "nothing"},

	/* 175-179 */	
	{"Gadgeteer", -2, -2, -2, 0, 3, 
		"Gadgeteer is your base skill with powerful devices.\
		It increases the amount and ease of effects you can get from devices."},
	{"Utility Bandolier", -2, -2, -2, 0, 3, 
		"This is your familarity with the utility bandolier."},
	{"Detectives Kit", -2, -2, -2, 0, 3, 
		"This is your familarity with the detectives kit."},
	{"Clockwork Chassis", -2, -2, -2, 0, 3, 
		"This is your familarity with the clockwork chassis."},
	{"Clockwork Carbine", -2, -2, -2, 0, 3, 
		"This is your familarity with the clockwork carbine."},

	/* 180-184 */	
	{"Velocipede", -2, -2, -2, 0, 3, 
		"This is your familarity with the velocipede."},
	{"Analytic Engine", -2, -2, -2, 0, 3, 
		"This is your familarity with the analytic engine."},
	{"", -2, -2, -2, 0, 3, "nothing"},
	{"", -2, -2, -2, 0, 3, "nothing"},
	{"", -2, -2, -2, 0, 3, "nothing"},

	/* 185-189 */	
	{"", -2, -2, -2, 0, 3, "nothing"},
	{"", -2, -2, -2, 0, 3, "nothing"},
	{"", -2, -2, -2, 0, 3, "nothing"},
	{"", -2, -2, -2, 0, 3, "nothing"},
	{"Pyrokinetics", -2, -2, -2, 0, 3, 
		"This is the art of using science to project fire.\
		It increases your ranged damage  by adding more and more fire damage to your shots."},

	/* 190-194 */	
	{"Using Devices", -2, -2, -2, 0, 1, 
		"This is your skill with devices.\
		You are extremely talented at this skill."},
	{"Using Devices", -2, -2, -2, 0, 1, 
		"This is your skill with devices."},
	{"Using Devices", -2, -2, -2, 0, 1, 
		"This is your skill with devices.\
		You are terrible at this skill."},
	{"Advanced Devices", -2, -2, -2, 0, 1, 
		"Advanced devices further increases your success rate at activating devices."},
	{"Fast Device Use", -2, -2, -2, 0, 1, 
		"Fast devices use reduces the amount of time it takes to use devices."},

	/* 195-199 */	
	{"Device Efficiency", -2, -2, -2, 0, 1, 
		"Device efficiency allows you to reduce the recharge time between uses of apparatuses."},
	{"Device Power Amp", -2, -2, -2, 0, 1, 
		"Device power amplifier increases the amount of damage you can cause with devices."},
	{"Advanced Power Amp", -2, -2, -2, 0, 1, 
		"Advanced power amplifier further increases the amount of damage you can cause with devices."},
	{"Device Bio Amp", -2, -2, -2, 0, 1, 
		"Device Biological Amplifier increases the healing effect of devices."},
	{"", -2, -2, -2, 0, 3, "nothing"},

	/* 200-204 */	
	{"Fire Lore", -2, -2, -2, 0, 1, 
		"Fire lore is your basic understanding of fire.\
		It attacks your enemies with fire when you strike them in melee combat, raises the amount your strength can increase, and gives you access to powerful fire attacks at high levels."},
	{"Fire Mastery", -2, -2, -2, 0, 1, 
		"Fire mastery is your mastery of fire and flame.\
		It increases your resistance versus fire, shields you in fire, and allows powerful fire attacks."},
	{"Wind Lore", -2, -2, -2, 0, 1, 
		"Wind lore is your basic understanding of wind and air.\
		It raises the amount your agility can increase, and raises your speed."},
	{"Wind Mastery", -2, -2, -2, 0, 1, 
		"Wind Mastery is your mastery of wind and air.\
		It allows you to see as a bird, conjure a cyclone, raises your resistance to air, speed, and protects you with an electric sheath."},
	{"Water Lore", -2, -2, -2, 0, 1, 
		"Water lore is your basic understanding of water.\
		It increases your rate of regeneration, and increases the amount your physical stats can increase."},

	/* 205-209 */	
	{"Water Mastery", -2, -2, -2, 0, 1, 
		"Water mastery is your mastery of water and the sea.\
		It further increases your regeneration rate, raises your resistance to water, raises your armor class and allows you to conjure rushing streams of water."},
	{"Earth Lore", -2, -2, -2, 0, 1, 
		"Earth lore is your basic understanding of earth.\
		It raises the amount your vigor can increase, and increases your armor class."},
	{"Earth Mastery", -2, -2, -2, 0, 1, 
		"Earth mastery is your mastery of earth and stone.\
		It allows you to conjure bolts of earth and stone, raises your resistance to earth, further rasies your armor class and protects your body with sharp spines."},
	{"Elemental Defense", -2, -2, -2, 0, 1, 
		"Elemental defense raises your resistance against the low and mid resists."},
	{"Basic Survival", -2, -2, -2, 0, 1, 
		"Basic survival raises your resistance against all the low resists."},

	/* 210-214 */	
	{"Toughness", -2, -2, -2, 0, 2, 
		"Toughness raises your wound points."},
	{"Mental Toughness", -2, -2, -2, 0, 3, 
		"Mental toughness raises your wound points."},
	{"Fortitude", -2, -2, -2, 0, 2, 
		"Fortitude raises your wound points."},
	{"Iron Body", -2, -2, -2, 0, 2, 
		"Raises your wound points and armor class."},
	{"Spiritual Battery", -2, -2, -2, 0, 1, 
		"Spiritual battery increases your reserve of spellpoints."},

	/* 215-219 */	
	{"Hold the Line", -2, -2, -2, 0, 2, 
		"Hold the line increases your armor class."},
	{"Knock-down", -2, -2, -2, 0, 2, 
		"Attempts to stun monsters you hit with a melee attack."},
	{"Dragon's Heart", -2, -2, -2, 0, 1,
		"Adds to the amount your physical stats increase when raised."},
	{"Athletics", -2, -2, -2, 0, 2, 
		"Athletics mproves your physical fitness making you more agile, stronger, and faster.\
		A very athletic person can produce boosts of speed allowing them to move much faster than normal."},
	{"", -2, -2, -2, 0, 1, "nothing"},

	/* 220-224 */	
	{"", -2, -2, -2, 0, 1, "nothing"},
	{"", -2, -2, -2, 0, 1, "nothing"},
	{"", -2, -2, -2, 0, 1, "nothing"},
	{"", -2, -2, -2, 0, 1, "nothing"},
	{"", -2, -2, -2, 0, 1, "nothing"},
};


/* Skill tables */

/*
 * The void skill_raceinit(void) function below this array parses this array, and sets
 * all the corresponding skill values in the table above to 1 if their value
 * in the table is greater than 0.
 *
 * perhaps I should have a second value that sets the value in skills to -1 for skills
 * that the player has a possiblility to acquire? XXX
 *
 * I'm not really concerned about peoples ability to modify them for their own ends, 
 * perhaps sometime before version 1.0 I'll change this so that this information is pulled
 * from an external text file. -CCC
 */
/*
 * There is almost certainly a better way to be doing this, but I'm learning on my own, and
 * well, hell this works!
 */
 
#if 0 
const s16b race_skills[25][N_SKILLS] =
{
	/* British Skills */
	{0,0,1,0,0,
	 0,0,0,0,0,
	 1,0,0,1,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,1,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0},
	 
	/* Asiatic skills */
	{0,0,1,0,0,
	 0,0,0,0,1,
	 0,0,0,1,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,1,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0},

	 /* American Skills */
	{0,0,1,0,0,
	 0,0,0,0,0,
	 1,0,0,1,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,1,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0},

	 /* African Skills */
	{0,0,1,0,0,
	 0,0,0,0,1,
	 0,0,0,0,1,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,1,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0},

	 /* French Skills */
	{0,0,1,0,0,
	 0,0,0,0,0,
	 1,0,0,1,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,1,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,1,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0},

	 /* Spanish Skills */
	{0,0,1,0,0,
	 0,0,0,0,0,
	 1,0,0,0,1,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,1,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,1,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0},

	 /* German Skills */
	{0,0,1,0,0,
	 0,0,0,0,0,
	 1,0,0,1,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,1,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,1,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0},

	/* Russian Skills */
	{0,1,0,0,0,
	 0,0,0,0,1,
	 0,0,0,0,1,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,1,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0},

	/* Scandinavian Skills */
	{0,0,1,0,0,
	 0,0,0,0,0,
	 1,0,0,0,1,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,1,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0},
	 
	/* Arabic Skills */
	{0,1,0,0,0,
	 0,0,0,0,1,
	 0,0,0,0,1,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,1,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0},

	/* Dwarf Skills */
	{1,0,0,0,0,
	 0,0,0,0,0,
	 1,0,0,1,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 1,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,1,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,1,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 1,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0},

	/* Brownie Skills */
	{0,1,0,0,0,
	 0,0,0,0,1,
	 0,0,0,0,1,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,1,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 1,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0},

	/* Daoine Sidhe Skills */
	{0,1,0,0,0,
	 0,0,0,1,0,
	 0,0,1,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,1,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0},
	
	/* Seelie Fae Skills */ 	 
	{0,1,0,0,0,
	 0,0,0,1,0,
	 0,0,0,1,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,1,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,1,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0},

	/* Unseelie Fae Skills */
	{0,1,0,0,0,
	 0,0,0,1,0,
	 0,0,0,1,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,1,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,1,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0},

	/* Automata Skills */
	{0,0,1,0,0,
	 0,0,0,0,0,
	 1,0,0,0,1,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,1,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,1,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,1,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,1,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0},

	/* Steam-Mecha Skills */
	{0,0,1,0,0,
	 0,0,0,0,0,
	 1,0,0,0,1,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,1,1,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,1,1,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 1,1,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,1,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0},	 

	/* Djinn Skills */
	{0,1,0,0,0,
	 0,0,0,1,0,
	 0,0,0,1,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,1,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0},
	 
	/* Rakshasa Skills */
	{0,0,1,0,0,
	 0,0,0,1,0,
	 0,0,0,1,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,1,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,1,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,1,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 1,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0},

	/* Giant Skills*/
	{0,0,1,0,0,
	 0,0,0,0,0,
	 1,0,0,0,1,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,1,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,1,
	 0,0,1,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 1,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,1,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0},

	/* Ogre Skills */
	{0,0,1,0,0,
	 0,0,0,0,0,
	 1,0,0,0,1,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,1,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,1,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 1,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,1,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0},

	/* Troll Skills */
	{0,0,1,0,0,
	 0,0,0,0,0,
	 1,0,0,0,1,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,1,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,1,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0},

	/* Ghost Skills */
	{0,0,1,0,0,
	 0,0,0,1,0,
	 0,0,0,1,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,1,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,1,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0},

	/* Goblin Skills */
	{0,0,1,0,0,
	 0,0,0,0,0,
	 1,0,0,0,1,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,1,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,1,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0},

	/* Old One Skills */
	{0,0,1,0,0,
	 0,0,0,0,0,
	 1,0,0,0,1,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,1,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0},
};

/*
 * See the description above.
 * This array holds the skills which will start active for each class.
 */
const s16b class_skills[20][N_SKILLS] =
{
	/* Officer Skills */
	{0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,1,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 1,0,0,0,0,
	 0,0,0,0,0,
	 1,0,0,1,0,
	 0,0,1,0,0,
	 0,0,0,0,1,

	 0,0,0,0,0,
	 0,0,1,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 1,1,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 1,1,0,0,1,
	 0,0,0,1,0,
	 0,0,0,1,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,1,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,1,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0},

	/* Aesthete Skills */
	{0,0,0,0,0,
	 0,0,0,0,1,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 1,0,0,0,0,
	 0,0,0,0,0,
	 1,0,0,1,0,
	 0,0,0,0,0,
	 0,0,0,0,1,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,1,1,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 1,1,0,0,1,
	 0,0,0,1,0,
	 0,0,0,1,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 1,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 1,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0},

	/* Engineer Skills */
	{0,1,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 1,0,0,0,0,
	 0,0,0,0,0,
	 1,0,0,1,0,
	 0,0,0,0,0,
	 0,0,0,0,1,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 1,1,0,0,1,
	 0,0,0,1,0,
	 0,0,0,1,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 1,1,0,1,1,
	 1,1,0,0,0,
	 0,0,0,0,0,
	 1,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0},

	/* Medium Skills */
	{0,0,0,0,0,
	 0,0,0,0,1,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 1,0,0,0,0,
	 0,0,0,0,0,
	 1,0,0,1,0,
	 0,0,0,0,0,
	 0,0,0,0,1,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,1,1,1,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 1,1,0,0,1,
	 0,0,0,1,0,
	 0,0,0,1,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0},

	/* Adventurer Skills */
	{0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 1,0,0,0,0,
	 0,0,0,0,0,
	 1,0,0,1,0,
	 0,0,1,1,1,
	 0,0,0,0,1,

	 0,0,0,0,0,
	 1,1,1,1,1,
	 1,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 1,1,0,0,1,
	 0,0,0,1,0,
	 0,0,0,1,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0},

	/* Dashing Hussar */
	{0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 1,0,0,0,0,
	 0,0,0,0,0,
	 1,0,0,1,0,
	 0,0,0,1,0,
	 0,0,0,0,1,

	 0,0,0,0,0,
	 0,1,1,1,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 1,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 1,1,0,0,1,
	 0,0,0,1,0,
	 0,0,0,1,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,1,0,0,
	 0,0,0,0,0},

	/* Gentleman / Lady */
	{0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 1,0,0,0,0,
	 0,0,0,0,0,
	 1,0,0,1,0,
	 0,0,1,0,0,
	 0,0,0,0,1,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 1,1,0,0,1,
	 0,0,0,1,0,
	 0,0,0,1,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 1,0,1,0,0,
	 1,1,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0},

	/* Rogue */
	{1,0,0,0,0,
	 0,0,0,1,0,
	 0,0,1,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 1,0,0,0,0,
	 0,0,0,1,0,
	 1,0,0,1,0,
	 0,0,0,0,0,
	 1,0,0,0,1,

	 0,0,0,1,1,
	 0,0,1,1,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 1,1,0,0,1,
	 0,0,0,1,0,
	 0,0,0,1,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 1,1,0,0,0,
	 1,1,0,0,0,
	 0,0,0,0,0,
	 1,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0},

	/* Naturalist */
	{0,0,0,0,0,
	 0,0,0,0,1,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 1,0,0,0,0,
	 0,0,0,0,0,
	 1,0,0,1,0,
	 0,0,0,0,0,
	 0,0,0,0,1,

	 0,0,0,0,0,
	 1,0,0,0,0,
	 1,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,1,1,1,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 1,1,0,0,1,
	 0,0,0,1,0,
	 0,0,0,1,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,

	 1,0,1,0,1,
	 0,1,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0,
	 0,0,0,0,0}
};
#endif

#define RACE_SKILL_MAX 15      /* Number of max starting skills/race */
#define CLASS_SKILL_MAX 25     /* Number of max starting skills/class */
/* Feel free to increase if needed, just remember to add array members below */

/*
 * New race skills table.
 * Every race should have one disarm skill, one stealth skill,
 * one searching skill and one device skill.
 * I hope I got every skill right.
 * -CJN-
 */
const s16b race_skills_new[RACE_MAX][RACE_SKILL_MAX] = 
{
	/* British Skills */
	{
		SK_DISARM_POOR,
		SK_STEALTH_POOR,
		SK_SEARCHING_NORM,
		SK_DEVICE_POOR,
		-1,

		-1,
		-1,
		-1,
		-1,
		-1,

		-1,
		-1,
		-1,
		-1,
		-1
	},
	/* Asiatic skills */
	{
		SK_DISARM_POOR,
		SK_STEALTH_NORM,
		SK_SEARCHING_NORM,
		SK_DEVICE_POOR,
		-1,

		-1,
		-1,
		-1,
		-1,
		-1,

		-1,
		-1,
		-1,
		-1,
		-1
	},
	/* American Skills */
	{
		SK_DISARM_POOR,
		SK_STEALTH_POOR,
		SK_SEARCHING_NORM,
		SK_DEVICE_NORM,
		-1,

		-1,
		-1,
		-1,
		-1,
		-1,

		-1,
		-1,
		-1,
		-1,
		-1
	},
	/* African Skills */
	{
		SK_DISARM_POOR,
		SK_STEALTH_NORM,
		SK_SEARCHING_POOR,
		SK_DEVICE_POOR,
		-1,

		-1,
		-1,
		-1,
		-1,
		-1,

		-1,
		-1,
		-1,
		-1,
		-1
	},
	/* French Skills */
	{
		SK_DISARM_POOR,
		SK_STEALTH_POOR,
		SK_SEARCHING_NORM,
		SK_CUISINE,
		SK_DEVICE_POOR,

		-1,
		-1,
		-1,
		-1,
		-1,

		-1,
		-1,
		-1,
		-1,
		-1
	},
	/* Spanish Skills */
	{
		SK_DISARM_POOR,
		SK_STEALTH_POOR,
		SK_SEARCHING_POOR,
		SK_SWORD,
		SK_DEVICE_POOR,

		-1,
		-1,
		-1,
		-1,
		-1,

		-1,
		-1,
		-1,
		-1,
		-1
	},
	/* German Skills */
	{
		SK_DISARM_POOR,
		SK_STEALTH_POOR,
		SK_SEARCHING_NORM,
		SK_RANSACK,
		SK_DEVICE_NORM,

		-1,
		-1,
		-1,
		-1,
		-1,

		-1,
		-1,
		-1,
		-1,
		-1
	},
	/* Russian Skills */
	{
		SK_DISARM_NORM,
		SK_STEALTH_NORM,
		SK_SEARCHING_POOR,
		SK_DEVICE_POOR,
		-1,

		-1,
		-1,
		-1,
		-1,
		-1,

		-1,
		-1,
		-1,
		-1,
		-1
	},
	/* Scandinavian Skills */
	{
		SK_DISARM_POOR,
		SK_STEALTH_POOR,
		SK_SEARCHING_POOR,
		SK_DEVICE_POOR,
		-1,

		-1,
		-1,
		-1,
		-1,
		-1,

		-1,
		-1,
		-1,
		-1,
		-1
	},
	/* Arabic Skills */
	{
		SK_DISARM_NORM,
		SK_STEALTH_NORM,
		SK_SEARCHING_POOR,
		SK_DEVICE_POOR,
		-1,

		-1,
		-1,
		-1,
		-1,
		-1,

		-1,
		-1,
		-1,
		-1,
		-1
	},
	/* Dwarf Skills */
	{
		SK_DISARM_GOOD,
		SK_STEALTH_POOR,
		SK_SEARCHING_NORM,
		SK_STONELORE,
		SK_POWER_STRIKE,

		SK_AXES,
		SK_DEVICE_GOOD,
		-1,
		-1,
		-1,

		-1,
		-1,
		-1,
		-1,
		-1
	},
	/* Brownie Skills */
	{
		SK_DISARM_NORM,
		SK_STEALTH_NORM,
		SK_SEARCHING_POOR,
		SK_FAE_PATH,
		SK_DEVICE_GOOD,

		-1,
		-1,
		-1,
		-1,
		-1,

		-1,
		-1,
		-1,
		-1,
		-1
	},
	/* Daoine Sidhe Skills */
	{
		SK_DISARM_NORM,
		SK_STEALTH_GOOD,
		SK_SEARCHING_GOOD,
		SK_DEVICE_NORM,
		-1,

		-1,
		-1,
		-1,
		-1,
		-1,

		-1,
		-1,
		-1,
		-1,
		-1
	},
	/* Seelie Fae Skills */
	{
		SK_DISARM_NORM,
		SK_STEALTH_GOOD,
		SK_SEARCHING_NORM,
		SK_FAE_PATH,
		SK_DEVICE_NORM,

		-1,
		-1,
		-1,
		-1,
		-1,

		-1,
		-1,
		-1,
		-1,
		-1
	},
	/* Unseelie Fae Skills */
	{
		SK_DISARM_NORM,
		SK_STEALTH_GOOD,
		SK_SEARCHING_NORM,
		SK_FAE_PATH,
		SK_DEVICE_NORM,

		-1,
		-1,
		-1,
		-1,
		-1,

		-1,
		-1,
		-1,
		-1,
		-1
	},
	/* Automata Skills */
	{
		SK_DISARM_POOR,
		SK_STEALTH_POOR,
		SK_SEARCHING_POOR,
		SK_UTILITY_CYPHER,
		SK_ACC_STRIKE,

		SK_ACC_SHOT,
		SK_DEVICE_POOR,
		-1,
		-1,
		-1,

		-1,
		-1,
		-1,
		-1,
		-1
	},
	/* Steam-Mecha Skills */
	{
		SK_DISARM_POOR,
		SK_STEALTH_POOR,
		SK_SEARCHING_POOR,
		SK_ONSLAUGHT_CYPHER,
		SK_AEGIS_CYPHER,

		SK_ACC_STRIKE,
		SK_VICIOUS_STRIKE,
		SK_CRIT_SHOT,
		SK_ACC_SHOT,
		SK_DEVICE_POOR,

		-1,
		-1,
		-1,
		-1,
		-1
	},
	/* Djinn Skills */
	{
		SK_DISARM_NORM,
		SK_STEALTH_GOOD,
		SK_SEARCHING_NORM,
		SK_DEVICE_NORM,
		-1,

		-1,
		-1,
		-1,
		-1,
		-1,

		-1,
		-1,
		-1,
		-1,
		-1
	},
	/* Rakshasa Skills */
	{
		SK_DISARM_POOR,
		SK_STEALTH_GOOD,
		SK_SEARCHING_NORM,
		SK_DEMON_ATTUNE,
		SK_WEAPON_FINESSE,

		SK_CRIT_THROW,
		SK_DEVICE_GOOD,
		-1,
		-1,
		-1,

		-1,
		-1,
		-1,
		-1,
		-1
	},
	/* Giant Skills */
	{
		SK_DISARM_POOR,
		SK_STEALTH_POOR,
		SK_SEARCHING_POOR,
		SK_ROCK_TOSS,
		SK_POWER_STRIKE,

		SK_VICIOUS_STRIKE,
		SK_BLUNT,
		SK_DEVICE_POOR,
		-1,
		-1,

		-1,
		-1,
		-1,
		-1,
		-1
	},
	/* Ogre Skills */
	{
		SK_DISARM_POOR,
		SK_STEALTH_POOR,
		SK_SEARCHING_POOR,
		SK_BZRK_STR,
		SK_VICIOUS_STRIKE,

		SK_BLUNT,
		SK_DEVICE_POOR,
		-1,
		-1,
		-1,

		-1,
		-1,
		-1,
		-1,
		-1
	},
	/* Troll Skills */
	{
		SK_DISARM_POOR,
		SK_STEALTH_POOR,
		SK_SEARCHING_POOR,
		SK_VICIOUS_STRIKE,
		SK_DEVICE_POOR,

		-1,
		-1,
		-1,
		-1,
		-1,

		-1,
		-1,
		-1,
		-1,
		-1
	},
	/* Ghost Skills */
	{
		SK_DISARM_POOR,
		SK_STEALTH_GOOD,
		SK_SEARCHING_NORM,
		SK_ETHERIC_ATTUNE,
		SK_DEVICE_POOR,

		-1,
		-1,
		-1,
		-1,
		-1,

		-1,
		-1,
		-1,
		-1,
		-1
	},
	/* Goblin Skills */
	{
		SK_DISARM_POOR,
		SK_STEALTH_POOR,
		SK_SEARCHING_POOR,
		SK_COWARDICE,
		SK_DEVICE_POOR,

		-1,
		-1,
		-1,
		-1,
		-1,

		-1,
		-1,
		-1,
		-1,
		-1
	},
	/* Old One Skills */
	{
		SK_DISARM_POOR,
		SK_STEALTH_POOR,
		SK_SEARCHING_POOR,
		SK_DEVICE_POOR,
		-1,

		-1,
		-1,
		-1,
		-1,
		-1,

		-1,
		-1,
		-1,
		-1,
		-1
	}
};


/*
 * New class skills table.
 * I hope I got every skill right.
 * -CJN-
 */
const s16b class_skills_new[CLASS_MAX][CLASS_SKILL_MAX] =
{
	/* Officer Skills */
	{
		SK_SEARCHING_NORM,
		SK_TOHIT,
		SK_TOHIT_MARTIAL,
		SK_TOHIT_SHOOTING,
		SK_PISTOL,

		SK_TOHIT_THROWING,
		SK_SWORD,
		SK_OFFICER,
		SK_OFFICER2,
		SK_LATIN,

		SK_OCCULT,
		SK_RITUAL_MAGIC,
		SK_SPIRITUALITY,
		SK_TMPR_WILL,
		SK_UTILITY_BANDOLIER,

		SK_DEVICE_NORM,
		-1,
		-1,
		-1,
		-1,

		-1,
		-1,
		-1,
		-1,
		-1
	},
	/* Aesthete Skills */
	{
		SK_STEALTH_NORM,
		SK_TOHIT,
		SK_TOHIT_MARTIAL,
		SK_TOHIT_SHOOTING,
		SK_TOHIT_THROWING,

		SK_AESTHETE,
		SK_AESTHETE2,
		SK_LATIN,
		SK_OCCULT,
		SK_RITUAL_MAGIC,

		SK_SPIRITUALITY,
		SK_TMPR_WILL,
		SK_GADGETEER,
		SK_DEVICE_GOOD,
		SK_DISARM_NORM,

		-1,
		-1,
		-1,
		-1,
		-1,

		-1,
		-1,
		-1,
		-1,
		-1
	},
	/* Engineer Skills */
	{
		SK_DISARM_NORM,
		SK_TOHIT,
		SK_TOHIT_MARTIAL,
		SK_TOHIT_SHOOTING,
		SK_TOHIT_THROWING,

		SK_LATIN,
		SK_OCCULT,
		SK_RITUAL_MAGIC,
		SK_SPIRITUALITY,
		SK_TMPR_WILL,

		SK_GADGETEER,
		SK_UTILITY_BANDOLIER,
		SK_CLOCKWORK_CHASSIS,
		SK_CLOCKWORK_CARBINE,
		SK_VELOCIPEDE,

		SK_ANALYTIC_ENGINE,
		SK_DEVICE_GOOD,
		SK_SEARCHING_NORM,
		-1,
		-1,

		-1,
		-1,
		-1,
		-1,
		-1
	},
	/* Medium Skills */
	{
		SK_STEALTH_NORM,
		SK_TOHIT,
		SK_TOHIT_MARTIAL,
		SK_TOHIT_SHOOTING,
		SK_TOHIT_THROWING,

		SK_TELEPATHY,
		SK_CLAIRSENTIENCE,
		SK_PSYCHOMETABOLISM,
		SK_LATIN,
		SK_OCCULT,

		SK_RITUAL_MAGIC,
		SK_SPIRITUALITY,
		SK_TMPR_WILL,
		-1,
		-1,

		-1,
		-1,
		-1,
		-1,
		-1,

		-1,
		-1,
		-1,
		-1,
		-1
	},
	/* Adventurer Skills */
	{
		SK_TOHIT,
		SK_TOHIT_MARTIAL,
		SK_TOHIT_SHOOTING,
		SK_PISTOL,
		SK_RIFLE,

		SK_SHOTGUN,
		SK_TOHIT_THROWING,
		SK_HAFTED,
		SK_POLEARM,
		SK_SWORD,

		SK_DAGGER,
		SK_AXES,
		SK_BLUNT,
		SK_LATIN,
		SK_OCCULT,

		SK_RITUAL_MAGIC,
		SK_SPIRITUALITY,
		SK_TMPR_WILL,
		SK_SEARCHING_NORM,
		SK_STEALTH_NORM,

		SK_DISARM_NORM,
		-1,
		-1,
		-1,
		-1
	},
	/* Dashing Hussar */
	{
		SK_TOHIT,
		SK_TOHIT_MARTIAL,
		SK_TOHIT_SHOOTING,
		SK_RIFLE,
		SK_TOHIT_THROWING,

		SK_POLEARM,
		SK_SWORD,
		SK_DAGGER,
		SK_COMBATTECHNIQUES,
		SK_LATIN,

		SK_OCCULT,
		SK_RITUAL_MAGIC,
		SK_SPIRITUALITY,
		SK_TMPR_WILL,
		SK_DRAGON_HEART,

		SK_SEARCHING_NORM,
		-1,
		-1,
		-1,
		-1,

		-1,
		-1,
		-1,
		-1,
		-1
	},
	/* Gentleman / Lady */
	{
		SK_TOHIT,
		SK_TOHIT_MARTIAL,
		SK_TOHIT_SHOOTING,
		SK_PISTOL,
		SK_TOHIT_THROWING,

		SK_LATIN,
		SK_OCCULT,
		SK_RITUAL_MAGIC,
		SK_SPIRITUALITY,
		SK_TMPR_WILL,

		SK_GADGETEER,
		SK_DETECTIVES_KIT,
		SK_VELOCIPEDE,
		SK_ANALYTIC_ENGINE,
		SK_SEARCHING_NORM,

		-1,
		-1,
		-1,
		-1,
		-1,

		-1,
		-1,
		-1,
		-1,
		-1
	},
	/* Rogue */
	{
		SK_DISARM_GOOD,
		SK_STEALTH_GOOD,
		SK_SEARCHING_GOOD,
		SK_TOHIT,
		SK_SWIFT_BLOW,

		SK_TOHIT_MARTIAL,
		SK_TOHIT_SHOOTING,
		SK_CRIT_SHOT,
		SK_TOHIT_THROWING,
		SK_CRIT_THROW,

		SK_ACC_THROW,
		SK_SWORD,
		SK_DAGGER,
		SK_LATIN,
		SK_OCCULT,

		SK_RITUAL_MAGIC,
		SK_SPIRITUALITY,
		SK_TMPR_WILL,
		SK_GADGETEER,
		SK_UTILITY_BANDOLIER,

		SK_VELOCIPEDE,
		SK_ANALYTIC_ENGINE,
		SK_DEVICE_GOOD,
		-1,
		-1
	},
	/* Naturalist */
	{
		SK_STEALTH_NORM,
		SK_TOHIT,
		SK_TOHIT_MARTIAL,
		SK_TOHIT_SHOOTING,
		SK_TOHIT_THROWING,

		SK_HAFTED,
		SK_BLUNT,
		SK_FLORAL,
		SK_FAUNA,
		SK_ELEMENTAL,

		SK_LATIN,
		SK_OCCULT,
		SK_RITUAL_MAGIC,
		SK_SPIRITUALITY,
		SK_TMPR_WILL,

		SK_FIRE_LORE,
		SK_WIND_LORE,
		SK_WATER_LORE,
		SK_EARTH_LORE,
		-1,

		-1,
		-1,
		-1,
		-1,
		-1
	}
};


/*
 * Determines if the skill has reached it's max rank.
 * Which should be 20 for all skills.
 *
 * Also checks to make sure that the skills are not higher than plev + 3 -CCC
 *
 * I should probably also have a method of limiting skills a second way (such as a skill
 * you get when combat skill is at 4 can't ever be higher than your level - 1, which
 * if you'll notice is (level + 3) - 4) XXX
 * 
 */
bool can_raise_skill(int skill)
{
	/* is this truly more simple? */
	int lev = p_ptr->lev;

	/* Skills can't be raised higher than SK_MAX_POWER (Which is 20) */	
	if (p_ptr->skills[skill].skill_max >= SK_MAX_POWER) return (FALSE);

	/* 
	 * I should probably track when the skill becomes available and set
	 * That character level at one, or possibly have an intrinsic value
	 * for each skill that represents when it becomes available (in addition
	 * to having a requirement -CCC
	 *
	 */

	/* Skills can't be raised higher than your level + 2 */
	/* Changed from + 3 to + 2 to encourage the placement of skills in broader areas */
	if (p_ptr->wizard) return (TRUE);
	if (p_ptr->skills[skill].skill_max >= (lev + 2)) return (FALSE);
	if (p_ptr->skills[skill].skill_max >= (p_ptr->skills[skill].skill_raise + 2)) return (FALSE);

	/* Assume no limits */
	return (TRUE);
}

/*
 * This function takes the race of the player and initalizations all skills
 * that the race recieves at start with a value of 1.
 *
 * This is only called at the end of character creation. -CCC
 *
 * Updated to use the new arrays with SK_ constants to initialize skills. -CJN-
 */
void skill_raceinit(void)
{
	int	race = p_ptr->prace;
	int i, j;

	/* check all elements of new race skill table */
	for (i = 0; i < RACE_SKILL_MAX; i++)
	{
		/* Get next skill in table */
		j = race_skills_new[race][i];
		if (j >= 0) /* Valid skill? */
		{
			 /* Then set skill_rank, and skill_max to one */
			 p_ptr->skills[j].skill_rank = p_ptr->skills[j].skill_max = p_ptr->skills[j].skill_raise = 1;		 
		}
	}
	return;
}

/* 
 * This function takes the class of the player and initalizes all skills that the
 * class recieves at the start with a value of 1
 *
 * This is only called at the end of character creation. -CCC
 *
 * Updated to use the new arrays with SK_constants to initialize skills. -CJN-
 */
void skill_classinit(void)
{
	int class = p_ptr->pclass;
	int i, j;

	/* check all elements of new class skill table */
	for (i = 0; i < CLASS_SKILL_MAX; i ++)
	{
		/* Get next skill in table */
		j = class_skills_new[class][i];
		if (j >= 0) /* Valid skill? */
		{
			/* set skill_rank, and skill_max to one */
			 p_ptr->skills[j].skill_rank = p_ptr->skills[j].skill_max = p_ptr->skills[j].skill_raise = 1;		 
		}
	}
}

/*
 * Enables a disabled skill.
 * A helper function I added to make skill_link more readable.
 * -CJN-
 */
static void enable_skill(int skill)
{
	if (p_ptr->skills[skill].skill_max == -2)
	{
		p_ptr->skills[skill].skill_rank = p_ptr->skills[skill].skill_max = p_ptr->skills[skill].skill_raise = 1;
	}
}

/*
 * Disbles a skill.
 * A helper function for skill_cleanup & skill_link.
 * -CJN-
 */
static void disable_skill(int skill)
{
	p_ptr->skills[skill].skill_rank = p_ptr->skills[skill].skill_max = p_ptr->skills[skill].skill_raise = -2;
}


/* 
 * This will parse the skill list to check and make sure no skills are duplicated. 
 * Mainly all the special case early skills. 
 * It should also set skill cur = skill max.
 * I don't do this until other things are working.
 */
void skill_cleanup()
{
 
	 /* each step below condenses a specific skill */
	
	 /* disarm */
	 if (p_ptr->skills[SK_DISARM_GOOD].skill_max > 0)
	 {
		 disable_skill(SK_DISARM_NORM);
		 disable_skill(SK_DISARM_POOR);
	 }
	 else if(p_ptr->skills[SK_DISARM_NORM].skill_max > 0)
	 {
		 disable_skill(SK_DISARM_POOR);
	 }
	
	 /* Magic Device */
	 if (p_ptr->skills[SK_DEVICE_GOOD].skill_max > 0)
	 {
		 disable_skill(SK_DEVICE_NORM);
		 disable_skill(SK_DEVICE_POOR);
	 }
	 else if(p_ptr->skills[SK_DEVICE_NORM].skill_max > 0)
	 {
		 disable_skill(SK_DEVICE_POOR);
	 }
	 	 
	 /* Stealth */
	 if (p_ptr->skills[SK_STEALTH_GOOD].skill_max > 0)
	 {
		 disable_skill(SK_STEALTH_NORM);
		 disable_skill(SK_STEALTH_POOR);
	 }
	 else if(p_ptr->skills[SK_STEALTH_NORM].skill_max > 0)
	 {
		 disable_skill(SK_STEALTH_POOR);
	 }
	 
	 /* Searching Ability */
	 if (p_ptr->skills[SK_SEARCHING_GOOD].skill_max > 0)
	 {
		 disable_skill(SK_SEARCHING_NORM);
		 disable_skill(SK_SEARCHING_POOR);
	 }
	 else if(p_ptr->skills[SK_SEARCHING_NORM].skill_max > 0)
	 {
		 disable_skill(SK_SEARCHING_POOR);
	 }
}


/* 
 * A function perhaps to determine whether or not another skill meets 
 * a minimum requirement (for ex, can't have pyrotechnics unless your
 * pyro skill is higher than 8), using table lookup? maybe?
 * Maybe a big if statement (huge, down each skill path) This should be called from
 * skill up. Basically run the function - and it checks all your skills, and activates
 * any new ones that open up. By using specific if statments. Note that opening up is perm.
 * if your skill gets drained, you still have access to it. Drain isn't perm.
 */

void skill_link(void)
{
	/* Racial skills */
	if (p_ptr->skills[SK_UTILITY_CYPHER].skill_max > 9)
	{
		enable_skill(SK_SYSTEMS_CYPHER);
	}
	if (p_ptr->skills[SK_ONSLAUGHT_CYPHER].skill_max > 3 && p_ptr->skills[SK_AEGIS_CYPHER].skill_max > 5)
	{
		enable_skill(SK_SYSTEMS_CYPHER);
	}
	if (p_ptr->skills[SK_DEMON_ATTUNE].skill_max > 3)
	{
		enable_skill(SK_DARK_CHARM);
	}
	if (p_ptr->skills[SK_ONSLAUGHT_CYPHER].skill_max > 14)
	{
		enable_skill(SK_ROCKETRY);
	}
	if (((p_ptr->skills[SK_INTER_COMBAT].skill_max > 9) ||
		(p_ptr->skills[SK_INTER_MARTIAL].skill_max > 9)) &&
		(!(p_ptr->prace == RACE_GHOST)))
	{
		enable_skill(SK_TOUGHNESS);
	}
	if ((p_ptr->skills[SK_TOUGHNESS].skill_max > 19) &&
		(
		(p_ptr->pclass == CLASS_ADVENTURER) ||
		(p_ptr->pclass == CLASS_OFFICER)
		)
		)
	{
		enable_skill(SK_FORTITUDE);
	}
	if ((p_ptr->skills[SK_TOUGHNESS].skill_max > 9) &&
		(
		(p_ptr->pclass == CLASS_ADVENTURER) ||
		(p_ptr->pclass == CLASS_DASHING_H) ||
		(p_ptr->pclass == CLASS_OFFICER)
		)
		)
	{
		enable_skill(SK_PAIN_TOLERANCE);
	}
	if ((p_ptr->skills[SK_TOUGHNESS].skill_max > 4) &&
		(
		(p_ptr->pclass == CLASS_NATURAL) ||
		(p_ptr->pclass == CLASS_DASHING_H) ||
		(p_ptr->pclass == CLASS_GENTLEMAN)
		)
		)
	{
		enable_skill(SK_ATHLETICS);
	}
	if ((p_ptr->skills[SK_FORTITUDE].skill_max > 19) &&
		(
		(p_ptr->pclass == CLASS_ADVENTURER)
		)
		)
	{
		enable_skill(SK_IRON_BODY);
	}
	if 	(
		(p_ptr->skills[SK_TOHIT].skill_max > 4) &&
		( 
		(p_ptr->pclass == CLASS_OFFICER) ||
		(p_ptr->pclass == CLASS_GENTLEMAN) ||
		(p_ptr->pclass == CLASS_AESTHETE) ||
		(p_ptr->pclass == CLASS_ENGINEER) ||
		(p_ptr->pclass == CLASS_ADVENTURER) ||
		(p_ptr->pclass == CLASS_DASHING_H) ||
		(p_ptr->pclass == CLASS_ROGUE) ||
		(p_ptr->pclass == CLASS_NATURAL) ||
		(p_ptr->pclass == CLASS_MEDIUM)
		)	
		)
	{
		enable_skill(SK_INTER_COMBAT);
	}
	if 	(
		(p_ptr->skills[SK_INTER_COMBAT].skill_max > 14) &&
		( 
		(p_ptr->pclass == CLASS_OFFICER) ||
		(p_ptr->pclass == CLASS_ENGINEER) ||
		(p_ptr->pclass == CLASS_ADVENTURER) ||
		(p_ptr->pclass == CLASS_DASHING_H) ||
		(p_ptr->pclass == CLASS_NATURAL) ||
		(p_ptr->pclass == CLASS_ROGUE) 
		)	
		)
	{
		enable_skill(SK_ADV_COMBAT);
	}	
	if 	(
		(p_ptr->skills[SK_ADV_COMBAT].skill_max > 14) &&
		( 
		(p_ptr->pclass == CLASS_OFFICER) ||
		(p_ptr->pclass == CLASS_ADVENTURER) ||
		(p_ptr->pclass == CLASS_DASHING_H) 
		)	
		)
	{
		enable_skill(SK_MASTER_COMBAT);
	}
	if 	(
		(p_ptr->skills[SK_TOHIT_SHOOTING].skill_max > 14) &&
		( 
		(p_ptr->pclass == CLASS_OFFICER) ||
		(p_ptr->pclass == CLASS_AESTHETE) ||
		(p_ptr->pclass == CLASS_ENGINEER) ||
		(p_ptr->pclass == CLASS_MEDIUM) ||
		(p_ptr->pclass == CLASS_ADVENTURER) ||
		(p_ptr->pclass == CLASS_DASHING_H) ||
		(p_ptr->pclass == CLASS_GENTLEMAN) ||
		(p_ptr->pclass == CLASS_ROGUE) ||
		(p_ptr->pclass == CLASS_NATURAL)
		)	
		)
	{
		enable_skill(SK_INTER_SHOOTING);
	}
	if 	(
		(p_ptr->skills[SK_INTER_SHOOTING].skill_max > 14) &&
		( 
		(p_ptr->pclass == CLASS_OFFICER) ||
		(p_ptr->pclass == CLASS_AESTHETE) ||
		(p_ptr->pclass == CLASS_GENTLEMAN) ||
		(p_ptr->pclass == CLASS_ENGINEER) ||
		(p_ptr->pclass == CLASS_ADVENTURER) ||
		(p_ptr->pclass == CLASS_DASHING_H) ||
		(p_ptr->pclass == CLASS_ROGUE) ||
		(p_ptr->pclass == CLASS_MEDIUM)
		)	
		)
	{
		enable_skill(SK_ADV_SHOOTING);
	}
	if 	(
		(p_ptr->skills[SK_ADV_SHOOTING].skill_max > 14) &&
		( 
		(p_ptr->pclass == CLASS_OFFICER) ||
		(p_ptr->pclass == CLASS_ADVENTURER) ||
		(p_ptr->pclass == CLASS_DASHING_H) 
		)	
		)
	{
		enable_skill(SK_MASTER_SHOOTING);
	}
	if 	(
		(p_ptr->skills[SK_TOHIT_THROWING].skill_max > 14) &&
		( 
		(p_ptr->pclass == CLASS_MEDIUM) ||
		(p_ptr->pclass == CLASS_ADVENTURER) ||
		(p_ptr->pclass == CLASS_ENGINEER) ||
		(p_ptr->pclass == CLASS_DASHING_H) ||
		(p_ptr->pclass == CLASS_GENTLEMAN) ||
		(p_ptr->pclass == CLASS_ROGUE) ||
		(p_ptr->pclass == CLASS_NATURAL) ||
		(p_ptr->pclass == CLASS_AESTHETE)	
		)	
		)
	{
		enable_skill(SK_ADV_THROWING);
	}
	if 	(
		(p_ptr->skills[SK_ADV_THROWING].skill_max > 14) &&
		( 
		(p_ptr->pclass == CLASS_ADVENTURER) ||
		(p_ptr->pclass == CLASS_ENGINEER) ||
		(p_ptr->pclass == CLASS_AESTHETE) ||
		(p_ptr->pclass == CLASS_NATURAL) ||
		(p_ptr->pclass == CLASS_ROGUE)
		)	
		)
	{
		enable_skill(SK_MASTER_THROWING);
	}
	if  (
		(p_ptr->skills[SK_TOHIT_MARTIAL].skill_max > 14) &&
		(
		(p_ptr->pclass == CLASS_MEDIUM) ||
		(p_ptr->pclass == CLASS_ROGUE) ||
		(p_ptr->pclass == CLASS_GENTLEMAN) ||
		(p_ptr->pclass == CLASS_NATURAL) ||
		(p_ptr->pclass == CLASS_DASHING_H) ||
		(p_ptr->prace == RACE_ASIATIC)
		)
		)
	{
		enable_skill(SK_INTER_MARTIAL);
	}
	if  (
		(p_ptr->skills[SK_TOHIT_MARTIAL].skill_max > 4) &&
		(
		(p_ptr->pclass == CLASS_ENGINEER) ||
		(p_ptr->pclass == CLASS_ROGUE) ||
		(p_ptr->pclass == CLASS_DASHING_H)
		)
		)
	{
		enable_skill(SK_ACROBATICS);
	}
	if  (
		(p_ptr->skills[SK_INTER_MARTIAL].skill_max > 14) &&
		(
		(p_ptr->pclass == CLASS_ROGUE) ||
		(p_ptr->pclass == CLASS_MEDIUM) ||
		(p_ptr->pclass == CLASS_GENTLEMAN) ||
		(p_ptr->pclass == CLASS_NATURAL) ||
		(p_ptr->prace == RACE_ASIATIC)
		)
		)
	{
		enable_skill(SK_ADV_MARTIAL);
	}
	if (p_ptr->skills[SK_INTER_COMBAT].skill_max > 7)
	{
		enable_skill(SK_VICIOUS_STRIKE);
		enable_skill(SK_ACC_STRIKE);
	}
	if (p_ptr->skills[SK_VICIOUS_STRIKE].skill_max > 9 && p_ptr->skills[SK_ADV_COMBAT].skill_max > 0 && p_ptr->skills[SK_CRIT_STRIKE].skill_max < 2)
	{
		enable_skill(SK_POWER_STRIKE);
	}
	if (p_ptr->skills[SK_VICIOUS_STRIKE].skill_max > 9)
	{
		enable_skill(SK_WEAPON_FINESSE);
	}
	if (p_ptr->skills[SK_ACC_STRIKE].skill_max > 9  && p_ptr->skills[SK_ADV_COMBAT].skill_max > 0 && p_ptr->skills[SK_POWER_STRIKE].skill_max < 2)
	{
		enable_skill(SK_CRIT_STRIKE);
	}
	if (p_ptr->skills[SK_ACC_STRIKE].skill_max > 9)
	{
		enable_skill(SK_SWIFT_BLOW);
	}
	if (p_ptr->skills[SK_CRIT_STRIKE].skill_max > 1)
	{
		disable_skill(SK_POWER_STRIKE);
	}
	if (p_ptr->skills[SK_POWER_STRIKE].skill_max > 1)
	{
		disable_skill(SK_CRIT_STRIKE);
	}
	if (p_ptr->skills[SK_INTER_SHOOTING].skill_max > 7)
	{
		enable_skill(SK_ACC_SHOT);
		enable_skill(SK_VICIOUS_SHOT);
	}
	if (p_ptr->skills[SK_ACC_SHOT].skill_max > 9  && p_ptr->skills[SK_ADV_SHOOTING].skill_max > 0)
	{
		enable_skill(SK_CRIT_SHOT);
	}
	if (p_ptr->skills[SK_TOHIT_SHOOTING].skill_max > 9)
	{
		enable_skill(SK_SWIFT_SHOT);
	}
	if (p_ptr->skills[SK_ADV_THROWING].skill_max > 9)
	{
		enable_skill(SK_ACC_THROW);
	} 
	if (p_ptr->skills[SK_ACC_THROW].skill_max > 9)
	{
		enable_skill(SK_CRIT_THROW);
	} 
	if (p_ptr->skills[SK_ACC_THROW].skill_max > 3 && p_ptr->skills[SK_MASTER_THROWING].skill_max > 0)		
	{
		enable_skill(SK_POWER_THROW);
	}
	if 	((p_ptr->pclass == CLASS_ROGUE) &&
		((p_ptr->skills[SK_MASTER_THROWING].skill_max > 9)))
	{
		enable_skill(SK_FAST_THROW);
	}
	if  (
		(p_ptr->skills[SK_INTER_MARTIAL].skill_max > 7) &&
		(
		(p_ptr->pclass == CLASS_ROGUE) ||
		(p_ptr->pclass == CLASS_MEDIUM) ||
		(p_ptr->pclass == CLASS_GENTLEMAN) ||
		(p_ptr->pclass == CLASS_NATURAL) ||
		(p_ptr->prace == RACE_ASIATIC)
		)
		)
	{
		enable_skill(SK_MARTIAL_DEFENSE);
	} 
#if 0
	if  (
		(p_ptr->skills[SK_INTER_MARTIAL].skill_max > 7) &&
		(
		(p_ptr->pclass == CLASS_ROGUE)
		)
		)
	{
		enable_skill(SK_JUMPING);
	} 
#endif
	if  (
		(p_ptr->skills[SK_MARTIAL_DEFENSE].skill_max > 14) &&
		(
		(p_ptr->pclass == CLASS_ROGUE) ||
		(p_ptr->pclass == CLASS_GENTLEMAN) ||
		(p_ptr->prace == RACE_ASIATIC)
		)
		)
	{
		enable_skill(SK_MARTIAL_DEFENSE_II);
	} 
	if  (
		(p_ptr->skills[SK_ADV_MARTIAL].skill_max > 9) &&
		(p_ptr->skills[SK_MARTIAL_DEFENSE_II].skill_max > 0) &&
		(
		(p_ptr->pclass == CLASS_ROGUE) ||
		(p_ptr->pclass == CLASS_GENTLEMAN) ||
		(p_ptr->prace == RACE_ASIATIC)
		)
		)
	{
		enable_skill(SK_MARTIAL_SPEED);
	} 
	if (p_ptr->skills[SK_HAFTED].skill_max > 1)
	{
		disable_skill(SK_POLEARM);
		disable_skill(SK_SWORD);
		disable_skill(SK_DAGGER);
		disable_skill(SK_AXES);
		disable_skill(SK_BLUNT);
	}
	if (p_ptr->skills[SK_BLUNT].skill_max > 1)
	{
		disable_skill(SK_POLEARM);
		disable_skill(SK_SWORD);
		disable_skill(SK_DAGGER);
		disable_skill(SK_AXES);
		disable_skill(SK_HAFTED);
	}
	if (p_ptr->skills[SK_AXES].skill_max > 1)
	{
		disable_skill(SK_POLEARM);
		disable_skill(SK_SWORD);
		disable_skill(SK_DAGGER);
		disable_skill(SK_HAFTED);
		disable_skill(SK_BLUNT);
	}
	if (p_ptr->skills[SK_DAGGER].skill_max > 1)
	{
		disable_skill(SK_POLEARM);
		disable_skill(SK_SWORD);
		disable_skill(SK_HAFTED);
		disable_skill(SK_AXES);
		disable_skill(SK_BLUNT);
	}
	if (p_ptr->skills[SK_SWORD].skill_max > 1)
	{
		disable_skill(SK_POLEARM);
		disable_skill(SK_HAFTED);
		disable_skill(SK_DAGGER);
		disable_skill(SK_AXES);
		disable_skill(SK_BLUNT);
	}
	if (p_ptr->skills[SK_POLEARM].skill_max > 1)
	{
		disable_skill(SK_HAFTED);
		disable_skill(SK_SWORD);
		disable_skill(SK_DAGGER);
		disable_skill(SK_AXES);
		disable_skill(SK_BLUNT);
	}
	if (p_ptr->skills[SK_PISTOL].skill_max > 1)
	{
		disable_skill(SK_RIFLE);
		disable_skill(SK_SHOTGUN);
	}
	if (p_ptr->skills[SK_RIFLE].skill_max > 1)
	{
		disable_skill(SK_PISTOL);
		disable_skill(SK_SHOTGUN);
	}
	if (p_ptr->skills[SK_SHOTGUN].skill_max > 1)
	{
		disable_skill(SK_PISTOL);
		disable_skill(SK_RIFLE);
	}
	if ((p_ptr->pclass == CLASS_ADVENTURER) &&
		(p_ptr->skills[SK_HAFTED].skill_max > 19))
	{
		enable_skill(SK_HAFTED_MASTER);
	}
	if ((p_ptr->pclass == CLASS_ADVENTURER) &&
	 	(p_ptr->skills[SK_POLEARM].skill_max > 19))
	{
		enable_skill(SK_POLEARM_MASTER);
	}
	if ((p_ptr->pclass == CLASS_ADVENTURER) &&
		(p_ptr->skills[SK_SWORD].skill_max > 19))
	{
		enable_skill(SK_SWORD_MASTER);
	}
	if ((p_ptr->pclass == CLASS_ADVENTURER) &&
		(p_ptr->skills[SK_DAGGER].skill_max > 19))
	{
		enable_skill(SK_DAGGER_MASTER);
	}
	if ((p_ptr->pclass == CLASS_ADVENTURER) &&
		(p_ptr->skills[SK_AXES].skill_max > 19))
	{
		enable_skill(SK_AXES_MASTER);
	}
	if ((p_ptr->pclass == CLASS_ADVENTURER) &&
		(p_ptr->skills[SK_BLUNT].skill_max > 19))
	{
		enable_skill(SK_BLUNT_MASTER);
	}
	if ((p_ptr->pclass == CLASS_ADVENTURER) && 
		((p_ptr->skills[SK_SEARCHING_GOOD].skill_max > 4) ||
		 (p_ptr->skills[SK_SEARCHING_NORM].skill_max > 4) ||
		 (p_ptr->skills[SK_SEARCHING_POOR].skill_max > 4)))
	{
		enable_skill(SK_TRAILBLAZER);
	}
	if (p_ptr->skills[SK_TRAILBLAZER].skill_max > 14)
	{
		enable_skill(SK_EXPLORER);
	}
	if ((p_ptr->skills[SK_SEARCHING_GOOD].skill_max > 4) ||
	    (p_ptr->skills[SK_SEARCHING_NORM].skill_max > 4))
	{
		enable_skill(SK_KEEN_EYES);
	}
	
	if ( ((p_ptr->pclass == CLASS_ADVENTURER) || 
		 (p_ptr->pclass == CLASS_ENGINEER) || 
		 (p_ptr->pclass == CLASS_OFFICER)) && 
		 (p_ptr->skills[SK_KEEN_EYES].skill_max > 9) &&
		((p_ptr->skills[SK_SEARCHING_GOOD].skill_max > 14) ||
		 (p_ptr->skills[SK_SEARCHING_NORM].skill_max > 14) ||
		 (p_ptr->skills[SK_SEARCHING_POOR].skill_max > 14)))
	{
		enable_skill(SK_SPOT_WEAKNESS);
	}
	
	if ((p_ptr->pclass == CLASS_ADVENTURER) && (p_ptr->skills[SK_FORTITUDE].skill_max > 4) &&
		((p_ptr->skills[SK_STEALTH_GOOD].skill_max > 9) ||
		 (p_ptr->skills[SK_STEALTH_NORM].skill_max > 9) ||
		 (p_ptr->skills[SK_STEALTH_POOR].skill_max > 9)))
	{
		enable_skill(SK_BASIC_SURVIVAL);
	}
	if (((p_ptr->pclass == CLASS_ADVENTURER) || 
		 (p_ptr->pclass == CLASS_ENGINEER) || 
		 (p_ptr->pclass == CLASS_ROGUE)) &&
		((p_ptr->skills[SK_DISARM_GOOD].skill_max > 9) ||
		 (p_ptr->skills[SK_DISARM_NORM].skill_max > 9) ||
		 (p_ptr->skills[SK_DISARM_POOR].skill_max > 9)))
	{
		enable_skill(SK_DISABLE_MACHINE);
	}
	if ((p_ptr->pclass == CLASS_ROGUE) && (p_ptr->skills[SK_ADV_MARTIAL].skill_max > 0) &&
		((p_ptr->skills[SK_STEALTH_GOOD].skill_max > 19) ||
		 (p_ptr->skills[SK_STEALTH_NORM].skill_max > 19) ||
		 (p_ptr->skills[SK_STEALTH_POOR].skill_max > 19)))
	{
		enable_skill(SK_NINJASTEALTH);
		enable_skill(SK_NINJUTSU);
	}
	if 	((p_ptr->skills[SK_NINJASTEALTH].skill_max > 4) ||
		(p_ptr->skills[SK_NINJUTSU].skill_max > 4))
	{
		enable_skill(SK_NINJAMAGIC);
	}
	if 	((p_ptr->pclass == CLASS_ROGUE) &&
		((p_ptr->skills[SK_TOHIT].skill_max > 4) ||
		(p_ptr->skills[SK_TOHIT_MARTIAL].skill_max > 4)))
	{
		enable_skill(SK_DIRTY_FIGHTING);
	}
	if 	(((p_ptr->pclass == CLASS_ROGUE) || 
		  (p_ptr->pclass == CLASS_ENGINEER) ||
 		  (p_ptr->pclass == CLASS_OFFICER)) &&
		((p_ptr->skills[SK_LATIN].skill_max > 9)))
	{
		enable_skill(SK_ANATOMY);
	}

	if 	((((p_ptr->pclass == CLASS_GENTLEMAN) || 
		  (p_ptr->pclass == CLASS_ENGINEER) ||
 		  (p_ptr->pclass == CLASS_MEDIUM)) &&
		((p_ptr->skills[SK_LATIN].skill_max > 2))) &&
		(!(p_ptr->prace == RACE_STEAM_MECHA)) &&
		(!(p_ptr->prace == RACE_AUTOMATA)))
	{
		enable_skill(SK_ERUDITE);
	}
	if (((p_ptr->pclass == CLASS_ROGUE) || 
		 (p_ptr->pclass == CLASS_ENGINEER) || 
		 (p_ptr->pclass == CLASS_AESTHETE)) &&
		((p_ptr->skills[SK_DISARM_GOOD].skill_max > 9) ||
		 (p_ptr->skills[SK_DISARM_NORM].skill_max > 9) ||
		 (p_ptr->skills[SK_DISARM_POOR].skill_max > 9)))
	{
		enable_skill(SK_SABOTAGE);
	}
	if ((p_ptr->pclass == CLASS_ROGUE) &&
		((p_ptr->skills[SK_STEALTH_GOOD].skill_max > 9) ||
		 (p_ptr->skills[SK_STEALTH_NORM].skill_max > 9) ||
		 (p_ptr->skills[SK_STEALTH_POOR].skill_max > 9)))
	{
		enable_skill(SK_ASSASSINATION);
	}
	if ((p_ptr->pclass == CLASS_ROGUE) &&
		((p_ptr->skills[SK_STEALTH_GOOD].skill_max > 14) ||
		 (p_ptr->skills[SK_STEALTH_NORM].skill_max > 14) ||
		 (p_ptr->skills[SK_STEALTH_POOR].skill_max > 14)))
	{
		enable_skill(SK_THEFT);
	}
	if (p_ptr->skills[SK_THEFT].skill_max > 19)
	{
		enable_skill(SK_MASTER_THEFT);
	}
	if ((p_ptr->pclass == CLASS_ROGUE) &&
		((p_ptr->skills[SK_STEALTH_GOOD].skill_max > 19) ||
		 (p_ptr->skills[SK_STEALTH_NORM].skill_max > 19) ||
		 (p_ptr->skills[SK_STEALTH_POOR].skill_max > 19)))
	{
		enable_skill(SK_SUPERSTEALTH);
	}
	if (p_ptr->skills[SK_OFFICER].skill_max > 4)
	{
		enable_skill(SK_PRESENCE);
	}
	if ((p_ptr->pclass == CLASS_OFFICER) && (p_ptr->skills[SK_INTER_COMBAT].skill_max > 4))
	{
		enable_skill(SK_HOLD_THE_LINE);
	}
	if ((p_ptr->pclass == CLASS_OFFICER) && (p_ptr->skills[SK_ADV_COMBAT].skill_max > 9))
	{
		enable_skill(SK_KNOCK_DOWN);
	}
	if 	((p_ptr->skills[SK_HOLD_THE_LINE].skill_max > 14) &&
		(p_ptr->skills[SK_ADV_COMBAT].skill_max > 14))
	{
		enable_skill(SK_BATTLE_ENDURANCE);
	}
	if (p_ptr->skills[SK_TELEPATHY].skill_max > 4 ||
		p_ptr->skills[SK_CLAIRSENTIENCE].skill_max > 4 ||
		p_ptr->skills[SK_PSYCHOMETABOLISM].skill_max > 4)
	{
		enable_skill(SK_MESMERIC_WILL);
	}
	if (p_ptr->skills[SK_FLORAL].skill_max > 4 ||
		p_ptr->skills[SK_FAUNA].skill_max > 4 ||
		p_ptr->skills[SK_ELEMENTAL].skill_max > 4)
	{
		enable_skill(SK_EARTH_HEARTH);
	}
	if (p_ptr->skills[SK_MESMERIC_WILL].skill_max > 4)
	{
		enable_skill(SK_SPIRIT_BATTERY);
	}
	if (p_ptr->skills[SK_COMBATTECHNIQUES].skill_max > 14 )
	{
		enable_skill(SK_ELITEMANEUVERS);
	}

	if (p_ptr->skills[SK_HEBREW].skill_max > 14 && p_ptr->skills[SK_QABALA].skill_max > 14)
	{
		enable_skill(SK_GEMATRIA);
	}
	if ((p_ptr->pclass == CLASS_AESTHETE) && (p_ptr->skills[SK_LATIN].skill_max > 4))
	{
		enable_skill(SK_ART_APPRECIATION);
	}
	if ((p_ptr->pclass == CLASS_AESTHETE) &&
		((p_ptr->skills[SK_STEALTH_GOOD].skill_max > 4) ||
		 (p_ptr->skills[SK_STEALTH_NORM].skill_max > 4) ||
		 (p_ptr->skills[SK_STEALTH_POOR].skill_max > 4)))
	{
		if (!(p_ptr->muta1 & MUT1_BRIBERY))
			gain_random_mutation(109);
		enable_skill(SK_BRIBERY);
	}
	/* spells */
	/* Everyone gets latin, and access to books */
	/* then there is the occult chain. */
	if (p_ptr->skills[SK_GADGETEER].skill_max > 4)
	{
		enable_skill(SK_PYROKINETICS);
	}
	if ((p_ptr->pclass == CLASS_GENTLEMAN) && (p_ptr->skills[SK_LATIN].skill_max > 4))
	{
		enable_skill(SK_PERILOUS_SORCERY);
	}
	if 	(
		(p_ptr->skills[SK_OCCULT].skill_max > 14) &&
		(
		(p_ptr->pclass == CLASS_AESTHETE) ||
		(p_ptr->pclass == CLASS_ENGINEER) ||
		(p_ptr->pclass == CLASS_MEDIUM) ||
		(p_ptr->pclass == CLASS_GENTLEMAN) ||
		(p_ptr->pclass == CLASS_ROGUE) ||
		(p_ptr->pclass == CLASS_NATURAL)
		)
		)
	{
		enable_skill(SK_ADV_OCCULT);
	}
	if 	(
		(p_ptr->skills[SK_ADV_OCCULT].skill_max > 14) &&
		(
		(p_ptr->pclass == CLASS_GENTLEMAN) ||
		(p_ptr->pclass == CLASS_ROGUE) 
		)
		)
	{
		enable_skill(SK_CTHULHU_MYTHOS);
	}
	if 	(
		(p_ptr->skills[SK_RITUAL_MAGIC].skill_max > 2) &&
		(
		(p_ptr->pclass == CLASS_MEDIUM) ||
		(p_ptr->pclass == CLASS_GENTLEMAN) ||
		(p_ptr->pclass == CLASS_ENGINEER) ||
		(p_ptr->pclass == CLASS_ROGUE) ||
		(p_ptr->pclass == CLASS_NATURAL) ||
		(p_ptr->pclass == CLASS_AESTHETE)		
		)
		)
	{
		enable_skill(SK_THAUMIC_ENERGY);
	}
	if 	(
		(p_ptr->skills[SK_THAUMIC_ENERGY].skill_max > 14) &&
		(
		(p_ptr->pclass == CLASS_MEDIUM) ||
		(p_ptr->pclass == CLASS_ENGINEER) ||
		(p_ptr->pclass == CLASS_GENTLEMAN) ||
		(p_ptr->pclass == CLASS_NATURAL) 
		)
		)
	{
		enable_skill(SK_ADV_THAUMIC_ENERGY);
	}
	if 	(
		(p_ptr->skills[SK_ADV_THAUMIC_ENERGY].skill_max > 14) &&
		(
		(p_ptr->pclass == CLASS_GENTLEMAN) 
		)
		)
	{
		enable_skill(SK_THELMA);
	}
	if 	(
		(p_ptr->skills[SK_SPIRITUALITY].skill_max > 14) &&
		(
		(p_ptr->pclass == CLASS_NATURAL) ||
		(p_ptr->pclass == CLASS_MEDIUM) ||
		(p_ptr->pclass == CLASS_GENTLEMAN) ||
		(p_ptr->pclass == CLASS_ENGINEER) ||
		(p_ptr->pclass == CLASS_DASHING_H) ||
		(p_ptr->pclass == CLASS_ADVENTURER) ||
		(p_ptr->pclass == CLASS_OFFICER) ||
		(p_ptr->pclass == CLASS_ROGUE) ||
		(p_ptr->pclass == CLASS_AESTHETE)		
		)
		)
	{
		enable_skill(SK_PRAYER);
	}
	if 	(
		(p_ptr->skills[SK_PRAYER].skill_max > 14) &&
		(
		(p_ptr->pclass == CLASS_GENTLEMAN) ||
		(p_ptr->pclass == CLASS_NATURAL) ||
		(p_ptr->pclass == CLASS_MEDIUM) 
		)
		)
		
	{
		enable_skill(SK_DEVOTION);
	}
	if 	(
		(p_ptr->skills[SK_SPIRITUALITY].skill_max > 2) &&
		(
		(p_ptr->pclass == CLASS_NATURAL) ||
		(p_ptr->pclass == CLASS_ENGINEER) ||
		(p_ptr->pclass == CLASS_GENTLEMAN) ||
		(p_ptr->pclass == CLASS_MEDIUM) ||
		(p_ptr->pclass == CLASS_ROGUE) ||
		(p_ptr->pclass == CLASS_OFFICER) ||
		(p_ptr->pclass == CLASS_DASHING_H) ||
		(p_ptr->pclass == CLASS_ADVENTURER) ||
		(p_ptr->pclass == CLASS_AESTHETE)		
		)
		)
	{
		enable_skill(SK_LESSER_WARD);
	}
	if 	(
		(p_ptr->skills[SK_LESSER_WARD].skill_max > 14) &&
		(
		(p_ptr->pclass == CLASS_NATURAL) ||
		(p_ptr->pclass == CLASS_MEDIUM) ||
		(p_ptr->pclass == CLASS_GENTLEMAN) ||
		(p_ptr->pclass == CLASS_ADVENTURER) ||
		(p_ptr->pclass == CLASS_AESTHETE)		
		)
		)
	{
		enable_skill(SK_GREATER_WARD);
	}
	if (
	   (p_ptr->skills[SK_GREATER_WARD].skill_max > 9 && p_ptr->skills[SK_DEVOTION].skill_max > 4) ||
	   (p_ptr->skills[SK_ADV_THAUMIC_ENERGY].skill_max > 9 && p_ptr->skills[SK_CTHULHU_MYTHOS].skill_max > 4) 
	   )
	{
		enable_skill(SK_FAST_CAST);
	}	   
	   
	if (p_ptr->skills[SK_TMPR_WILL].skill_max > 14)
	{
		enable_skill(SK_HARD_WILL);
	}
	if (p_ptr->skills[SK_HARD_WILL].skill_max > 14)
	{
		enable_skill(SK_IRON_WILL);
	}
	if (p_ptr->skills[SK_DEVICE_GOOD].skill_max > 4)
	{
		enable_skill(SK_ADV_DEVICE);
	}
	if (p_ptr->skills[SK_DEVICE_NORM].skill_max > 9)
	{
		enable_skill(SK_ADV_DEVICE);
	}
	if (p_ptr->skills[SK_DEVICE_POOR].skill_max > 14)
	{
		enable_skill(SK_ADV_DEVICE);
	}
	if (
		(p_ptr->skills[SK_ADV_DEVICE].skill_max > 14) && 
		(p_ptr->skills[SK_EFF_DEVICE].skill_max > 9)
	   )
	{
		enable_skill(SK_SPEED_DEVICE);
	}
	if (p_ptr->skills[SK_ADV_DEVICE].skill_max > 9)
	{
		enable_skill(SK_EFF_DEVICE);
	}
	if 	(
		 (p_ptr->skills[SK_DEVICE_POOR].skill_max > 14) ||
		 (p_ptr->skills[SK_DEVICE_GOOD].skill_max > 4) ||
		 (p_ptr->skills[SK_DEVICE_NORM].skill_max > 9)
		)
	{
		enable_skill(SK_POW_DEVICE);
	}
	if (p_ptr->skills[SK_POW_DEVICE].skill_max > 14)
	{
		enable_skill(SK_POW2_DEVICE);
	}
	if (p_ptr->skills[SK_ADV_DEVICE].skill_max > 4)
	{
		enable_skill(SK_HEAL_DEVICE);
	}
	if 	(
		(p_ptr->skills[SK_HARD_WILL].skill_max > 4) &&
		(
		(p_ptr->pclass == CLASS_MEDIUM)
		)
		)
	{
		enable_skill(SK_MENTAL_RESISTANCE);
	}
	if 	(
		(p_ptr->skills[SK_HARD_WILL].skill_max > 14) &&
		(
		(p_ptr->pclass == CLASS_MEDIUM)
		)
		)
	{
		enable_skill(SK_TOUGHNESS_WILL);
	}
	if 	(
		(p_ptr->skills[SK_SPIRITUALITY].skill_max > 9) &&
		(
		(p_ptr->pclass == CLASS_MEDIUM)
		)
		)
	{
		enable_skill(SK_SPIRIT_INFUSION);
	}
	if 	(
		(p_ptr->skills[SK_SPIRIT_INFUSION].skill_max > 4) &&
		(
		(p_ptr->pclass == CLASS_MEDIUM)
		)
		)
	{
		enable_skill(SK_SPIRIT_SHIELD);
		enable_skill(SK_SPIRIT_HEALING);
	}
	if 	(
		(p_ptr->skills[SK_SPIRIT_SHIELD].skill_max > 14) ||
		(p_ptr->skills[SK_SPIRIT_HEALING].skill_max > 14)
		)
	{
		enable_skill(SK_SPIRIT_SPEED);
	}	
	if (p_ptr->skills[SK_FIRE_LORE].skill_max > 1)
	{
		disable_skill(SK_WIND_LORE);
		disable_skill(SK_WATER_LORE);
		disable_skill(SK_EARTH_LORE);
	}
	if (p_ptr->skills[SK_WIND_LORE].skill_max > 1)
	{
		disable_skill(SK_FIRE_LORE);
		disable_skill(SK_WATER_LORE);
		disable_skill(SK_EARTH_LORE);
	}
	if (p_ptr->skills[SK_WATER_LORE].skill_max > 1)
	{
		disable_skill(SK_WIND_LORE);
		disable_skill(SK_FIRE_LORE);
		disable_skill(SK_EARTH_LORE);
	}
	if (p_ptr->skills[SK_EARTH_LORE].skill_max > 1)
	{
		disable_skill(SK_WIND_LORE);
		disable_skill(SK_WATER_LORE);
		disable_skill(SK_FIRE_LORE);
	}
	if (p_ptr->skills[SK_FIRE_LORE].skill_max > 19)
	{
		enable_skill(SK_FIRE_MASTERY);
	}
	if (p_ptr->skills[SK_WIND_LORE].skill_max > 19)
	{
		enable_skill(SK_WIND_MASTERY);
	}
	if (p_ptr->skills[SK_WATER_LORE].skill_max > 19)
	{
		enable_skill(SK_WATER_MASTERY);
	}
	if (p_ptr->skills[SK_EARTH_LORE].skill_max > 19)
	{
		enable_skill(SK_EARTH_MASTERY);
	}

	if ((p_ptr->skills[SK_EARTH_LORE].skill_max > 14) ||
		(p_ptr->skills[SK_WIND_LORE].skill_max > 14) ||
		(p_ptr->skills[SK_WATER_LORE].skill_max > 14) ||
		(p_ptr->skills[SK_FIRE_LORE].skill_max > 14))
	{
		enable_skill(SK_ELEMENTAL_RESISTANCE);
	}

	if (p_ptr->skills[SK_DETECTIVES_KIT].skill_max > 1)
	{
		disable_skill(SK_UTILITY_BANDOLIER);
		disable_skill(SK_CLOCKWORK_CHASSIS);
		disable_skill(SK_CLOCKWORK_CARBINE);
		disable_skill(SK_VELOCIPEDE);
		disable_skill(SK_ANALYTIC_ENGINE);
	}
	if (p_ptr->skills[SK_UTILITY_BANDOLIER].skill_max > 1)
	{
		disable_skill(SK_DETECTIVES_KIT);
	}
	if (p_ptr->skills[SK_CLOCKWORK_CHASSIS].skill_max > 1)
	{
		disable_skill(SK_DETECTIVES_KIT);
	}
	if (p_ptr->skills[SK_CLOCKWORK_CARBINE].skill_max > 1)
	{
		disable_skill(SK_DETECTIVES_KIT);
	}
	if (p_ptr->skills[SK_VELOCIPEDE].skill_max > 1)
	{
		disable_skill(SK_DETECTIVES_KIT);
	}
	if (p_ptr->skills[SK_ANALYTIC_ENGINE].skill_max > 1)
	{
		disable_skill(SK_DETECTIVES_KIT);
	}

	/* Trigger Mutations from skills */
	if (p_ptr->skills[SK_FIRE_LORE].skill_max == 10)
	{
		if (!(p_ptr->muta1 & MUT1_FIRE_BOLT))
			gain_random_mutation(100);
	}
	if (p_ptr->skills[SK_FIRE_LORE].skill_max == 15)
	{
		if (!(p_ptr->muta1 & MUT1_FIRE_BALL))
			gain_random_mutation(101);
	}
	if (p_ptr->skills[SK_FIRE_MASTERY].skill_max == 5)
	{
		if (!(p_ptr->muta1 & MUT1_FIRE_BREATH))
			gain_random_mutation(102);
	}
	if (p_ptr->skills[SK_FIRE_MASTERY].skill_max == 15)
	{
		if (!(p_ptr->muta1 & MUT1_FIRE_STORM))
			gain_random_mutation(103);
	}
	if (p_ptr->skills[SK_EARTH_MASTERY].skill_max == 1)
	{
		if (!(p_ptr->muta1 & MUT1_EARTH_BOLT))
			gain_random_mutation(104);
	}
	if (p_ptr->skills[SK_EARTH_MASTERY].skill_max == 10)
	{
		if (!(p_ptr->muta1 & MUT1_EARTH_SHOWER))
			gain_random_mutation(105);
	}
	if (p_ptr->skills[SK_WIND_MASTERY].skill_max == 1)
	{
		if (!(p_ptr->muta1 & MUT1_BIRDS_VIEW))
			gain_random_mutation(106);
	}
	if (p_ptr->skills[SK_WIND_MASTERY].skill_max == 5)
	{
		if (!(p_ptr->muta1 & MUT1_CYCLONE))
			gain_random_mutation(107);
	}
	if (p_ptr->skills[SK_WATER_MASTERY].skill_max == 5)
	{
		if (!(p_ptr->muta1 & MUT1_RUSHING_STREAMS))
			gain_random_mutation(108);
	}
	/* Bribery is 109 and is located above (it comes with the skill) */
	if (p_ptr->skills[SK_ACROBATICS].skill_max == 5)
	{
		if (!(p_ptr->muta1 & MUT1_EVASION))
			gain_random_mutation(110);
	}
	if (p_ptr->skills[SK_ACROBATICS].skill_max == 20)
	{
		if (!(p_ptr->muta1 & MUT1_SPRING))
			gain_random_mutation(111);
	}
	if (p_ptr->skills[SK_ATHLETICS].skill_max == 15)
	{
		if (!(p_ptr->muta1 & MUT1_BURST))
			gain_random_mutation(112);
	}
	
}

/*  possibly bool? This function raises a skill max(Perm) */
void skill_up(int skill)
{
	/* sanity check */
	if (p_ptr->skills[skill].skill_max == -2) return;
	
	/* Must have skill points to spend */
	if (p_ptr->free_skpts < 1) return;
	
	/* check to make sure the skill can be raised */
	/* not working? previously defined as something else? WTF!?! */
	if (!can_raise_skill(skill)) return;
	
	/* Raise the skill by one */
	p_ptr->skills[skill].skill_max += 1;
	
	/* Set the maximum to the new value */
	p_ptr->skills[skill].skill_rank = p_ptr->skills[skill].skill_max;
	
	/* Reduce the avaialble number of skill points */
	p_ptr->free_skpts -= 1;
	
	/* sanity check */
	if (p_ptr->free_skpts < 0)
	{
		p_ptr->free_skpts = 0;
	}	
	
	/* Check to see if any new skills have opened up */
	skill_link();
	

}

void skill_max_up(int skill)
{
	cptr boxtop, boxbottom, boxmessage_gold, boxmessage_max, boxmessage_level;
	boxtop = 			"--------------------------------------------";
	boxmessage_gold = 	"|            You lack the Gold!            |";
	boxmessage_level =  "|      No skill max > character level!     |";
	boxmessage_max = 	"| You can now train that skill to maximum! |";
	boxbottom = 		"--------------------------------------------";
	if (p_ptr->au < (p_ptr->lev * SKILL_INCREASE_CONSTANT)) 
	{
		Term_putstr(15, 9, -1, TERM_RED, boxtop);
		Term_putstr(15, 10, -1, TERM_RED, boxmessage_gold);
		Term_putstr(15, 11, -1, TERM_RED, boxbottom);
		pause_line(12);
	}
	else if (p_ptr->skills[skill].skill_raise + 2 >= SK_MAX_POWER)
	{
		Term_putstr(15, 9, -1, TERM_RED, boxtop);
		Term_putstr(15, 10, -1, TERM_RED, boxmessage_max);
		Term_putstr(15, 11, -1, TERM_RED, boxbottom);
		pause_line(12);
	}
	else if (p_ptr->skills[skill].skill_raise > p_ptr->max_lev)
	{
		Term_putstr(15, 9, -1, TERM_RED, boxtop);
		Term_putstr(15, 10, -1, TERM_RED, boxmessage_level);
		Term_putstr(15, 11, -1, TERM_RED, boxbottom);
		pause_line(12);
	}
	else 
	{
		/* spend the gold */
		p_ptr->au -= (p_ptr->lev * SKILL_INCREASE_CONSTANT);

		/* Raise the skill by one */
		p_ptr->skills[skill].skill_raise += 1;
		
		/* Update the display */
		/* store_prt_gold(); */

		/* gain a skill point */
		/* p_ptr->free_skpts += 1; */

		/* feedback */
		/* msg_print("You train your skill maximum up!"); */
	}

}

/* possibly bool? This function lowers a skill max(temp) */
/* Note that this function will only be called by things damageing the player */
/* Once a skill is increased on the stats page blam, that's it */
void skill_down(int skill)
{
	/* can't lower a skill you don't have */
	if (p_ptr->skills[skill].skill_rank == -2) return;
	
	/* lower the rank of the skill *leave maximum alone */
	p_ptr->skills[skill].skill_rank -= 1;
	
	/* Skill cannot go below 1 */
	if (p_ptr->skills[skill].skill_rank < 1)
	{
		p_ptr->skills[skill].skill_rank = 1;
	}
	
}

/* restores a skill to max value */
void skill_restore(int skill)
{
	/* can't restore a skill you don't have */
	if (p_ptr->skills[skill].skill_rank == -2) return;
	
	/* raise the skill back to maximum */
	p_ptr->skills[skill].skill_rank = p_ptr->skills[skill].skill_max;
	
	/* should there be another check here? */
	
}


/* This function alters a skill (due to a weapon or item) */
/* 
 * Alter implies a temporarary increase in the skill, have to make sure
 * to alter only the skills temporary value. Do I need this function? 
 */
/*void alter_skill(int skill, int value)
 *{
 *}
 */

/*
 * Print information about the currently selected skill.
 * This should be a seperate array or something for each field, maybe a large
 * switch statement. (yeah that seems like a good idea.) That prints a line
 * of information on the bottom of the screen. Eventually this will be called 
 * from level.c, and will print info about the currently selected skill
 */
static void prt_skill_select(int selected)
{
	char a = TERM_L_BLUE;

	Term_erase(0, 20, 255);
	Term_erase(0, 21, 255);
	Term_erase(0, 22, 255);

	move_cursor(20, 4);

	c_roff(a, skills[selected].description);
}


/*
 * Print the given skill and its rank in the appropriate column
 * and row.
 */

/* This whole function is the for loop in the function below */
void prt_skill_rank(int skill, int selected, int mode, bool train)
{
	char buf1[80], buf2[18];
	int row, col;
	char c;


	byte a = TERM_WHITE;
	

	/* Highlight selected skill */
	if (skill == selected) a = TERM_BLUE; 
	
	/* Skip unused skills */
	if ((p_ptr->skills[skill].skill_max == -2) || (p_ptr->skills[skill].skill_type != mode))
	{
		displayed = FALSE;
		return; 
	}
	
	
	/* Skill cannot be raised further */
	if (train)
	{
		if (p_ptr->skills[skill].skill_raise >= SK_MAX_POWER)
		{
			a = TERM_VIOLET;
			if (skill == selected) a = TERM_BLUE;
		}
		else
		{
			a = TERM_L_BLUE;
			if (skill == selected) a = TERM_BLUE;
		}
	}
	else if (!can_raise_skill(skill))
	{
		/* Skill is maxed out */
		if (p_ptr->skills[skill].skill_max == SK_MAX_POWER)
		{
			a = TERM_L_GREEN;
			if (skill == selected) a = TERM_GREEN;
		}
		/* Skill is maxed due to char level */
		else
		{
			a = TERM_L_DARK;
			if (skill == selected) a = TERM_SLATE;
		}
	}

	/* something to handle drained skills */

	/* Work out the row and column of the screen to use */
	displayed = TRUE;
	row = SKILL_ROW_START + skill_count;
	col = COL_ONE;
	
	if (skill_count < 9)
	{
	row = SKILL_ROW_START + skill_count;
	col = COL_ONE;
	}
	else if (skill_count < 18)
	{
	row = SKILL_ROW_START + (skill_count - 9);
	col = COL_TWO;
	}
	else 
	{
	row = SKILL_ROW_START + (skill_count -18);
	col = COL_THREE;
	}
	
	c = I2A(skill_count);
	 
	/* Display the skillrank */
	if (train) sprintf(buf2, "%2d", ((p_ptr->skills[skill].skill_raise < 18) ? p_ptr->skills[skill].skill_raise + 2 : 20));
	else sprintf(buf2, "%2d", p_ptr->skills[skill].skill_max);

	/* Format the string */
	sprintf(buf1, "%c) %18s%c %s", c, skills[skill].name,
		((p_ptr->skills[skill].skill_max == SK_MAX_POWER) ? '!' : ':'),
		buf2);
	
	/* Print the skill */
	c_prt(a, buf1, row, col);	
}


/*
 * Print out all the skills, along with their ranks.
 */
void print_all_skills(int selected, int mode, bool train)
{
	int i;
	
#if 0
	/* Clear all of the screen except the special message area */
	for (i = SKILL_ROW_START; i < SKILL_ROW_END; i++)
	{
		Term_erase(i, 0, 255); /* find out what function clears rows */
	}
	pause_line(2); 
#endif
	/* Print the static information */
	/* will do this later */

	/* reset the displayed book */
	displayed = FALSE;

	/* reset the skill count */
	skill_count = 0;
	
	prt_skill_select(selected);
	
	/* Print all the skills */
	for (i = 0; i < N_SKILLS; i++) 
	{
		/* clear the index */
		p_ptr->skills[i].skill_index = 0;
		prt_skill_rank(i, selected, mode, train);
		if (displayed == TRUE) 
		{
			skill_count++;
			p_ptr->skills[i].skill_index = skill_count;
		}
	}
}






