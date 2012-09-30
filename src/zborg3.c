/* File: borg3.c */

/* Purpose: Object and Spell routines for the Borg -BEN- */

#include "angband.h"


#ifdef ALLOW_BORG

#include "zborg1.h"
#include "zborg3.h"


/*
 * This file helps the Borg analyze "objects" and "shops", and to
 * deal with objects and spells.
 */




/*
 * Spell info
 */

borg_magic borg_magics[8][4][8];	/* Spell info, by realm/book/what */
borg_mind borg_minds[MINDCRAFT_MAX];

/*
 * Hack -- help analyze the magic
 *
 * The comments yield the "name" of the spell or prayer.
 *
 * Also, the leading letter in the comment indicates how we use the
 * spell or prayer, if at all, using "A" for "attack", "D" for "call
 * light" and "detection", "E" for "escape", "H" for healing, "O" for
 * "object manipulation", and "F" for "terrain feature manipulation",
 * plus "!" for entries that can soon be handled.
 */

static byte borg_magic_method[8][4][8] =
{
	{							/* 0 Realm 0 -- Non spell caster */
	 {							/* Book0... (sval 0) */
	  BORG_MAGIC_ICK /* ! "(blank)" */ ,
	  BORG_MAGIC_ICK /*   "(blank)" */ ,
	  BORG_MAGIC_ICK /* ! "(blank)" */ ,
	  BORG_MAGIC_ICK /*   "(blank)" */ ,
	  BORG_MAGIC_ICK /*   "(blank)" */ ,
	  BORG_MAGIC_ICK /*   "(blank)" */ ,
	  BORG_MAGIC_ICK /*   "(blank)" */ ,
	  BORG_MAGIC_ICK /*   "(blank)" */ },
	 {							/* Book1... (sval 1) */
	  BORG_MAGIC_ICK /* ! "(blank)" */ ,
	  BORG_MAGIC_ICK /* ! "(blank)" */ ,
	  BORG_MAGIC_ICK /*   "(blank)" */ ,
	  BORG_MAGIC_ICK /*   "(blank)" */ ,
	  BORG_MAGIC_ICK /*   "(blank)" */ ,
	  BORG_MAGIC_ICK /*   "(blank)" */ ,
	  BORG_MAGIC_ICK /*   "(blank)" */ ,
	  BORG_MAGIC_ICK /*   "(blank)" */ },
	 {							/* Book0... (sval 2) */
	  BORG_MAGIC_ICK /* ! "(blank)" */ ,
	  BORG_MAGIC_ICK /*   "(blank)" */ ,
	  BORG_MAGIC_ICK /* ! "(blank)" */ ,
	  BORG_MAGIC_ICK /*   "(blank)" */ ,
	  BORG_MAGIC_ICK /*   "(blank)" */ ,
	  BORG_MAGIC_ICK /*   "(blank)" */ ,
	  BORG_MAGIC_ICK /*   "(blank)" */ ,
	  BORG_MAGIC_ICK /*   "(blank)" */ },
	 {							/* Book3... (sval 3) */
	  BORG_MAGIC_ICK /* ! "(blank)" */ ,
	  BORG_MAGIC_ICK /*   "(blank)" */ ,
	  BORG_MAGIC_ICK /* ! "(blank)" */ ,
	  BORG_MAGIC_ICK /*   "(blank)" */ ,
	  BORG_MAGIC_ICK /*   "(blank)" */ ,
	  BORG_MAGIC_ICK /*   "(blank)" */ ,
	  BORG_MAGIC_ICK /*   "(blank)" */ ,
	  BORG_MAGIC_ICK /*   "(blank)" */ }
	 },							/* end of realm 0 */

	{							/* 1 Life Realm */
	 {							/* Common Prayers (sval 0) */
	  BORG_MAGIC_NOP /*   "Detect Evil" */ ,
	  BORG_MAGIC_NOP /*   "Cure Light Wounds" */ ,
	  BORG_MAGIC_NOP /*   "Bless" */ ,
	  BORG_MAGIC_NOP /* H "Remove Fear" */ ,
	  BORG_MAGIC_NOP /* D "Call Light" */ ,
	  BORG_MAGIC_NOP /* D "Find Traps" */ ,
	  BORG_MAGIC_NOP /* D "Cure Medium Wounds" */ ,
	  BORG_MAGIC_NOP /*   "Satisfy Hunger" */ ,},

	 {							/* High Mass (sval 1) */
	  BORG_MAGIC_NOP /*   "Remove Curse" */ ,
	  BORG_MAGIC_NOP /* E "Cure Poison" */ ,
	  BORG_MAGIC_NOP /* H "Cure Crit Wounds" */ ,
	  BORG_MAGIC_NOP /*   "See Inv" */ ,
	  BORG_MAGIC_AIM /*   "Holy Orb" */ ,
	  BORG_MAGIC_NOP /* H "PFE" */ ,
	  BORG_MAGIC_NOP /*   "Healing" */ ,
	  BORG_MAGIC_NOP /*   "Rune of Protection" */ ,},

	 {							/* Book of the Unicorn (sval 2) */
	  BORG_MAGIC_NOP /* H "Exorcism" */ ,
	  BORG_MAGIC_NOP /* A "Dispel Curse" */ ,
	  BORG_MAGIC_NOP /* H "Disp Undead and Demon" */ ,
	  BORG_MAGIC_NOP /*   "Day of Dove" */ ,
	  BORG_MAGIC_NOP /*   "Dispel Evil" */ ,
	  BORG_MAGIC_NOP /* D "Banishment" */ ,
	  BORG_MAGIC_NOP /* H "Holy Word" */ ,
	  BORG_MAGIC_NOP /*   "Warding True" */ },

	 {							/* Blessings of the Grail (sval 3) */
	  BORG_MAGIC_NOP /*   "Heroism" */ ,
	  BORG_MAGIC_NOP /* ! "Prayer" */ ,
	  BORG_MAGIC_NOP /* H "Bless Weapon" */ ,
	  BORG_MAGIC_NOP /* ! "Restoration" */ ,
	  BORG_MAGIC_NOP /*   "Healing True" */ ,
	  BORG_MAGIC_OBJ /* ! "Holy Vision" */ ,
	  BORG_MAGIC_NOP /*   "Divine Intervent" */ ,
	  BORG_MAGIC_NOP /*   "Holy Invuln" */ }

	 },							/* endof Life Realm */

	{							/*2. Sorcery Realm */
	 {							/* Beginners Handbook (sval 0) */
	  BORG_MAGIC_NOP /*   "Detect Monster" */ ,
	  BORG_MAGIC_NOP /*   "Phase Door" */ ,
	  BORG_MAGIC_NOP /*   "Detect Doors & Traps" */ ,
	  BORG_MAGIC_NOP /*   "Light Area" */ ,
	  BORG_MAGIC_AIM /*   "Confuse Monster" */ ,
	  BORG_MAGIC_NOP /*   "Teleport Self" */ ,
	  BORG_MAGIC_NOP /*   "Sleep Monster" */ ,
	  BORG_MAGIC_OBJ /*   "Recharging" */ },

	 {							/* Master Sorc (sval 1) */
	  BORG_MAGIC_NOP /*   "Magic Map" */ ,
	  BORG_MAGIC_OBJ /*   "Ident" */ ,
	  BORG_MAGIC_AIM /*   "Slow Monster" */ ,
	  BORG_MAGIC_NOP /*   "Mass Sleep " */ ,
	  BORG_MAGIC_AIM /*   "Teleport Away" */ ,
	  BORG_MAGIC_NOP /*   "Haste Self" */ ,
	  BORG_MAGIC_NOP /*   "Detection True" */ ,
	  BORG_MAGIC_OBJ /*   "*ID*" */ },

	 {							/* Pattern Sorc (sval 2) */
	  BORG_MAGIC_NOP /*   "Detect Obj" */ ,
	  BORG_MAGIC_NOP /*   "Detect Enchant" */ ,
	  BORG_MAGIC_ICK /*   "Charm Mon" */ ,
	  BORG_MAGIC_AIM /*   "Dimension Door" */ ,
	  BORG_MAGIC_NOP /*   "Sense Minds" */ ,
	  BORG_MAGIC_NOP /*   "Self Knowledge" */ ,
	  BORG_MAGIC_NOP /*   "Teleport Level" */ ,
	  BORG_MAGIC_NOP /*   "Word of Recall" */ },

	 {							/* Grimoir of Power (sval 3) */
	  BORG_MAGIC_AIM /*   "Stasis" */ ,
	  BORG_MAGIC_ICK /*   "Telekinesis" */ ,
	  BORG_MAGIC_ICK /*   "Explosive Rune" */ ,
	  BORG_MAGIC_NOP /*   "Clairvoyance" */ ,
	  BORG_MAGIC_OBJ /*   "*Enchant Weap" */ ,
	  BORG_MAGIC_OBJ /*   "*Enchant Armor" */ ,
	  BORG_MAGIC_ICK /*   "Alchemy" */ ,
	  BORG_MAGIC_NOP /*   "GOI" */ }
	 },							/* End of Sorcery Realm */

	{							/* 3 Nature Realm */
	 {							/* Call of the Wild (sval 0) */
	  BORG_MAGIC_NOP /*   "Detect Creature" */ ,
	  BORG_MAGIC_NOP /*   "First Aid" */ ,
	  BORG_MAGIC_NOP /* ! "Detect Door" */ ,
	  BORG_MAGIC_NOP /*   "Foraging" */ ,
	  BORG_MAGIC_NOP /*   "Daylight" */ ,
	  BORG_MAGIC_AIM /*   "Animal Taming" */ ,
	  BORG_MAGIC_NOP /*   "Resist Environment" */ ,
	  BORG_MAGIC_NOP /*   "Cure Wound&Poison" */ },
	 {							/* Nature Mastery (sval 1) */
	  BORG_MAGIC_AIM /* ! "Stone to Mud" */ ,
	  BORG_MAGIC_AIM /* ! "Lightning Bolt" */ ,
	  BORG_MAGIC_NOP /*   "Nature Awareness" */ ,
	  BORG_MAGIC_AIM /*   "Frost Bolt" */ ,
	  BORG_MAGIC_AIM /*   "Ray of Sunlight" */ ,
	  BORG_MAGIC_NOP /*   "Entangle" */ ,
	  BORG_MAGIC_ICK /*   "Summon Animals" */ ,
	  BORG_MAGIC_NOP /*   "Herbal Healing" */ },
	 {							/* Nature Gifts (sval 2) */
	  BORG_MAGIC_NOP /* ! "Door Building" */ ,
	  BORG_MAGIC_NOP /*   "Stair Building" */ ,
	  BORG_MAGIC_NOP /* ! "Stone Skin" */ ,
	  BORG_MAGIC_NOP /*   "Resistance True" */ ,
	  BORG_MAGIC_NOP /*   "Animal Friend" */ ,
	  BORG_MAGIC_OBJ /*   "Stone Tell" */ ,
	  BORG_MAGIC_NOP /*   "Wall of Stone" */ ,
	  BORG_MAGIC_OBJ /*   "Protect From Corros." */ },
	 {							/* Natures Wrath (sval 3) */
	  BORG_MAGIC_NOP /* ! "Earthquake" */ ,
	  BORG_MAGIC_NOP /*   "Whirlwind" */ ,
	  BORG_MAGIC_AIM /* ! "Blizzard" */ ,
	  BORG_MAGIC_AIM /*   "Lightning" */ ,
	  BORG_MAGIC_AIM /*   "Whirpool" */ ,
	  BORG_MAGIC_NOP /*   "Call Sunlight" */ ,
	  BORG_MAGIC_OBJ /*   "Elemental Brand" */ ,
	  BORG_MAGIC_NOP /*   "Natures Wrath" */ }
	 },							/* end of Natural realm  */

	{							/* 4.Chaos Realm */
	 {							/* Sign of Chaos... (sval 0) */
	  BORG_MAGIC_AIM /* "Magic Missile" */ ,
	  BORG_MAGIC_NOP /* "Trap/Door Dest" */ ,
	  BORG_MAGIC_NOP /* "Flash of Light" */ ,
	  BORG_MAGIC_NOP /* "Touch of Conf" */ ,
	  BORG_MAGIC_NOP /* "ManaBurst" */ ,
	  BORG_MAGIC_AIM /* "Fire Bolt" */ ,
	  BORG_MAGIC_AIM /* "Fist of Force" */ ,
	  BORG_MAGIC_NOP /* "Teleport" */ },
	 {							/* Chaos Mastery... (sval 1) */
	  BORG_MAGIC_ICK /*   "Wonder" */ ,
	  BORG_MAGIC_AIM /*   "Chaos Bolt" */ ,
	  BORG_MAGIC_NOP /*   "Sonic Boom" */ ,
	  BORG_MAGIC_AIM /*   "Doom Beam" */ ,
	  BORG_MAGIC_AIM /*   "Fire Ball" */ ,
	  BORG_MAGIC_AIM /*   "Teleport Other" */ ,
	  BORG_MAGIC_NOP /*   "Word of Dest" */ ,
	  BORG_MAGIC_NOP /*   "Invoke Logrus" */ },
	 {							/* Chaos Channels (sval 2) */
	  BORG_MAGIC_AIM /*   "Polymorph Other" */ ,
	  BORG_MAGIC_NOP /*   "Chain Lightn" */ ,
	  BORG_MAGIC_OBJ /*   "Arcane Binding" */ ,
	  BORG_MAGIC_AIM /*   "Disintegration" */ ,
	  BORG_MAGIC_NOP /*   "Alter Reality" */ ,
	  BORG_MAGIC_ICK /*   "Polymorph Self" */ ,
	  BORG_MAGIC_ICK /*   "Chaos Brinding" */ ,
	  BORG_MAGIC_ICK /*   "Summon Demon" */ },
	 {							/* Armageddon Tome (sval 3) */
	  BORG_MAGIC_AIM /*   "Gravity Beam" */ ,
	  BORG_MAGIC_AIM /*   "Meteor Swarm" */ ,
	  BORG_MAGIC_NOP /*   "Flame Strike" */ ,
	  BORG_MAGIC_NOP /*   "Call Chaos" */ ,
	  BORG_MAGIC_AIM /*   "Magic Rocket" */ ,
	  BORG_MAGIC_AIM /*   "Mana Storm" */ ,
	  BORG_MAGIC_AIM /*   "Breath Logrus" */ ,
	  BORG_MAGIC_NOP /*   "Call Void" */ }
	 },							/* end of Chaos Realm */

	{							/* 5. Death Realm */
	 {							/* Black Prayers (sval 0) */
	  BORG_MAGIC_NOP /* ! "Detect Unlife" */ ,
	  BORG_MAGIC_AIM /*   "Maledition" */ ,
	  BORG_MAGIC_NOP /* ! "Detect Evil" */ ,
	  BORG_MAGIC_AIM /*   "Stinking Cloud" */ ,
	  BORG_MAGIC_AIM /*   "Black Sleep" */ ,
	  BORG_MAGIC_NOP /*   "Resist Poison" */ ,
	  BORG_MAGIC_AIM /*   "Horrify" */ ,
	  BORG_MAGIC_AIM /*   "Enslave Undead" */ },
	 {							/* Black Mass (sval 1) */
	  BORG_MAGIC_AIM /* ! "Orb of Entropy" */ ,
	  BORG_MAGIC_AIM /*   "Nether Bolt" */ ,
	  BORG_MAGIC_NOP /*   "Terror" */ ,
	  BORG_MAGIC_AIM /*   "Vamp Drain" */ ,
	  BORG_MAGIC_OBJ /*   "Poison Brand" */ ,
	  BORG_MAGIC_NOP /*   "Disp Good" */ ,
	  BORG_MAGIC_WHO /*   "Genocide" */ ,
	  BORG_MAGIC_NOP /*   "Restore Life" */ },
	 {							/* Black Channels (sval 2) */
	  BORG_MAGIC_NOP /* ! "Berserk" */ ,
	  BORG_MAGIC_NOP /*   "Invoke Spirits" */ ,
	  BORG_MAGIC_AIM /* ! "Dark Bolt" */ ,
	  BORG_MAGIC_NOP /*   "Battle Frenzy" */ ,
	  BORG_MAGIC_AIM /*   "Vamp True" */ ,
	  BORG_MAGIC_OBJ /*   "Vamp Brand" */ ,
	  BORG_MAGIC_AIM /*   "Dark Storm" */ ,
	  BORG_MAGIC_NOP /*   "Mass Genocide" */ },
	 {							/* Necronomicon (sval 3) */
	  BORG_MAGIC_AIM /* ! "Death Ray" */ ,
	  BORG_MAGIC_ICK /*   "Raise the Dead" */ ,
	  BORG_MAGIC_OBJ /* ! "Esoteria" */ ,
	  BORG_MAGIC_NOP /*   "Word of Death" */ ,
	  BORG_MAGIC_NOP /*   "Evocation" */ ,
	  BORG_MAGIC_AIM /*   "Hellfire" */ ,
	  BORG_MAGIC_NOP /*   "Omnicide" */ ,
	  BORG_MAGIC_NOP /*   "Wraithform" */ }
	 },							/* end of Death Realm */

	{							/* 6 Trump Realm */
	 {							/* Conjuring and Tricks (sval 0) */
	  BORG_MAGIC_NOP /* ! "Phase Door" */ ,
	  BORG_MAGIC_AIM /*   "Mind Blast" */ ,
	  BORG_MAGIC_ICK /*   "Shuffle" */ ,
	  BORG_MAGIC_ICK /*   "Reset Recall" */ ,
	  BORG_MAGIC_NOP /*   "Teleport Self" */ ,
	  BORG_MAGIC_AIM /*   "Dimension Door" */ ,
	  BORG_MAGIC_NOP /*   "Trump Spying" */ ,
	  BORG_MAGIC_AIM /*   "Teleport Away" */ },
	 {							/* Deck of Many Things (sval 1) */
	  BORG_MAGIC_ICK /* ! "Trump Object" */ ,
	  BORG_MAGIC_ICK /* ! "Trump Animal" */ ,
	  BORG_MAGIC_NOP /*   "Phantasmal Servant" */ ,
	  BORG_MAGIC_ICK /*   "Trump Monster" */ ,
	  BORG_MAGIC_ICK /*   "Conjure Elemental" */ ,
	  BORG_MAGIC_NOP /*   "Teleport Level" */ ,
	  BORG_MAGIC_NOP /*   "Word of Recall" */ ,
	  BORG_MAGIC_NOP /*   "Banish" */ },
	 {							/* Trumps of Doom (sval 2) */
	  BORG_MAGIC_ICK /* ! "Joker Card" */ ,
	  BORG_MAGIC_ICK /*   "Trump Spiders" */ ,
	  BORG_MAGIC_ICK /*   "T. Reptiles" */ ,
	  BORG_MAGIC_ICK /*   "T. Houdns" */ ,
	  BORG_MAGIC_ICK /*   "T. Branding" */ ,
	  BORG_MAGIC_ICK /*   "Living Trump" */ ,
	  BORG_MAGIC_ICK /*   "Death Dealing" */ ,
	  BORG_MAGIC_ICK /*   "T. Cyberdemon" */ },
	 {							/* Five Aces (sval 3) */
	  BORG_MAGIC_NOP /* ! "T. Divination" */ ,
	  BORG_MAGIC_OBJ /*   "T. Lore" */ ,
	  BORG_MAGIC_ICK /*   "T. Undead" */ ,
	  BORG_MAGIC_ICK /*   "T. Dragon" */ ,
	  BORG_MAGIC_ICK /*   "Mass Trump" */ ,
	  BORG_MAGIC_ICK /*   "T. Demon" */ ,
	  BORG_MAGIC_ICK /*   "T. Ancient Dragon " */ ,
	  BORG_MAGIC_ICK /*   "T. Greater Undead" */ }
	 },							/* end of Trump Realm */

	{							/* 7 Arcane Realm */
	 {							/* Cantrips (sval 0) */
	  BORG_MAGIC_AIM /* ! "Zap" */ ,
	  BORG_MAGIC_AIM /*   "Wiz Lock" */ ,
	  BORG_MAGIC_NOP /*   "Det Invis" */ ,
	  BORG_MAGIC_NOP /*   "Det Mon" */ ,
	  BORG_MAGIC_NOP /*   "Blink" */ ,
	  BORG_MAGIC_NOP /*   "Light Area" */ ,
	  BORG_MAGIC_AIM /*   "Trap/Door Dest" */ ,
	  BORG_MAGIC_NOP /*   "Cure Light Wounds" */ },
	 {							/* Minor Arcana (sval 1) */
	  BORG_MAGIC_NOP /* ! "Det Door/Trap" */ ,
	  BORG_MAGIC_NOP /* ! "Phlogiston" */ ,
	  BORG_MAGIC_NOP /*   "Det Treasure" */ ,
	  BORG_MAGIC_NOP /*   "Det Enchant" */ ,
	  BORG_MAGIC_NOP /*   "Det Object" */ ,
	  BORG_MAGIC_NOP /*   "Cure Poison" */ ,
	  BORG_MAGIC_NOP /*   "Resist Cold" */ ,
	  BORG_MAGIC_NOP /*   "Resist Fre" */ },
	 {							/* Major Arcana (sval 2) */
	  BORG_MAGIC_NOP /* ! "Resist Elec" */ ,
	  BORG_MAGIC_NOP /*   "Resist Acid" */ ,
	  BORG_MAGIC_NOP /* ! "Cure Med Wounds" */ ,
	  BORG_MAGIC_NOP /*   "Teleport" */ ,
	  BORG_MAGIC_AIM /*   "Stone to Mud" */ ,
	  BORG_MAGIC_AIM /*   "Ray of Light" */ ,
	  BORG_MAGIC_NOP /*   "Satisfy Hunger" */ ,
	  BORG_MAGIC_NOP /*   "See Invis" */ },
	 {							/* Manual of Mastery (sval 3) */
	  BORG_MAGIC_OBJ /* ! "Recharge" */ ,
	  BORG_MAGIC_NOP /*   "Teleport Level" */ ,
	  BORG_MAGIC_OBJ /* ! "Ident" */ ,
	  BORG_MAGIC_AIM /*   "Teleport Away" */ ,
	  BORG_MAGIC_AIM /*   "Elemental Ball" */ ,
	  BORG_MAGIC_NOP /*   "Detection" */ ,
	  BORG_MAGIC_NOP /*   "Word of Recall" */ ,
	  BORG_MAGIC_NOP /*   "Clairvoyance" */ }
	 }							/* end of Arcane Realm */


};



/*
 * Hack -- help analyze the magic
 *
 * The comments yield the "name" of the spell or prayer.
 *
 * Also, the leading letter in the comment indicates how we use the
 * spell or prayer, if at all, using "A" for "attack", "D" for "call
 * light" and "detection", "E" for "escape", "H" for healing, "O" for
 * "object manipulation", "F" for "terrain feature manipulation",
 * "X" for "never use this", and "!" for "soon to be handled".
 *
 * The value indicates how much we want to know the spell/prayer.  A
 * rating of zero indicates that the spell/prayer is useless, and should
 * never be learned or used.  A rating from 1 to 49 indicates that the
 * spell/prayer is worth some experience to use once, so we should study
 * (and use) it when we get bored in town.  A rating from 50 to 99 means
 * that the spell/prayer should be learned as soon as possible (and used
 * when bored).
 *
 * XXX XXX XXX Verify ratings.
 */

static byte borg_magic_rating[8][4][8] =
{
	{							/* Null Realm */
	 {							/* Book0... (sval 0) */
	  0 /* ! "(blank)" */ ,
	  0 /*   "(blank)" */ ,
	  0 /*   "(blank)" */ ,
	  0 /*   "(blank)" */ ,
	  0 /*   "(blank)" */ ,
	  0 /*   "(blank)" */ ,
	  0 /*   "(blank)" */ ,
	  0 /*   "(blank)" */ },
	 {							/* Book1... (sval 1) */
	  0 /*   "(blank)" */ ,
	  0 /* ! "(blank)" */ ,
	  0 /*   "(blank)" */ ,
	  0 /*   "(blank)" */ ,
	  0 /*   "(blank)" */ ,
	  0 /*   "(blank)" */ ,
	  0 /*   "(blank)" */ ,
	  0 /*   "(blank)" */ },
	 {							/* Book2... (sval 2) */
	  0 /*   "(blank)" */ ,
	  0 /* ! "(blank)" */ ,
	  0 /*   "(blank)" */ ,
	  0 /*   "(blank)" */ ,
	  0 /*   "(blank)" */ ,
	  0 /*   "(blank)" */ ,
	  0 /*   "(blank)" */ ,
	  0 /*   "(blank)" */ },
	 {							/* Book3... (sval 3) */
	  0 /* ! "(blank)" */ ,
	  0 /*   "(blank)" */ ,
	  0 /* ! "(blank)" */ ,
	  0 /*   "(blank)" */ ,
	  0 /*   "(blank)" */ ,
	  0 /*   "(blank)" */ ,
	  0 /*   "(blank)" */ ,
	  0 /*   "(blank)" */ }
	 },							/* end of Null Realm */

	{							/* Life Realm */
	 {							/* Beginners Handbook (sval 0) */
	  85 /*   "Detect Evil" */ ,
	  55 /*   "Cure Light Wounds" */ ,
	  85 /*   "Bless" */ ,
	  35 /*   "Remove Fear" */ ,
	  35 /*   "Call Light" */ ,
	  75 /*   "Find Traps & Doors" */ ,
	  65 /*   "Cure Medium Wounds" */ ,
	  85 /*   "Satisfy Hunger" */ },
	 {							/* Words of Wisdom (sval 1) */
	  65 /*   "Remove Curse" */ ,
	  65 /*   "Cure Poison" */ ,
	  85 /*   "Cure Crit Wounds" */ ,
	  55 /*   "See Invis" */ ,
	  95 /*   "Holy Orb" */ ,
	  85 /*   "Prot/Evil" */ ,
	  65 /*   "Heal 300" */ ,
	  55 /*   "Glyph" */ },
	 {							/* Chants and Blessings (sval 2) */
	  65 /*   "Exorcism" */ ,
	  65 /*   "Dispel Curse" */ ,
	  55 /*   "Dispel Demon" */ ,
	  0 /*   "Day of Dove" */ ,
	  65 /*   "Dispel Evil" */ ,
	  55 /*   "Banishment" */ ,
	  65 /*   "Holy Word" */ ,
	  55 /*   "Warding True" */ },
	 {							/* Exorcism and Dispelling (sval 3) */
	  55 /*   "Heroism" */ ,
	  65 /*   "Prayer" */ ,
	  45 /*   "Bless Weapon" */ ,
	  55 /*   "Restoration" */ ,
	  65 /*   "Healing 2000" */ ,
	  55 /*   "Holy Vision" */ ,
	  55 /*   "Divine Intervent" */ ,
	  55 /*   "Holy Invuln" */ }
	 },							/* end of Life Magic */

	{							/* Sorcery Realm */
	 {							/* Magic for Beginners (sval 0) */
	  95 /*   "Detect Monsters" */ ,
	  85 /*   "Phase Door" */ ,
	  65 /*   "Detect Door" */ ,
	  85 /*   "Light Area" */ ,
	  75 /*   "Confuse Monster" */ ,
	  75 /*   "Teleport Selft" */ ,
	  65 /*   "Sleep Monster" */ ,
	  65 /*   "Recharging" */ },
	 {							/* Conjurings and Tricks (sval 1) */
	  55 /*   "Magic Map" */ ,
	  85 /*   "Identify" */ ,
	  55 /*   "Slow Monster" */ ,
	  65 /*   "Mass Sleep" */ ,
	  95 /*   "Teleport Away" */ ,
	  55 /*   "Haste Self" */ ,
	  85 /*   "Detection True" */ ,
	  75 /*   "*Identify*" */ },
	 {							/* Incantations and Illusions (sval 2) */
	  55 /*   "Detect Obj/Treasure" */ ,
	  55 /*   "Detect Enchantment" */ ,
	  75 /*   "Charm Monster" */ ,
	  65 /*   "Dimension Door" */ ,
	  65 /*   "Sense Minds" */ ,
	  0 /*   "Self Knowledge" */ ,
	  65 /*   "Teleport Level" */ ,
	  65 /*   "Word of Recall" */ },
	 {							/* Sorcery and Evocations (sval 3) */
	  55 /*   "Stasis" */ ,
	  0 /*   "Telekinesis" */ ,
	  0 /*   "Explosive Rune" */ ,
	  65 /*   "Clairvoyance" */ ,
	  55 /*   "Enchant Weap" */ ,
	  55 /*   "Enchant Armour" */ ,
	  0 /*   "Alchemy" */ ,
	  95 /*   "GOI" */ }
	 },							/* end of Sorcery Realm */

	{							/* 3 Nature Realm */
	 {							/* Call of the Wild (sval 0) */
	  65 /*   "Detect Creature" */ ,
	  65 /*   "First Aid" */ ,
	  55 /* ! "Detect Door" */ ,
	  75 /*   "Foraging" */ ,
	  75 /*   "Daylight" */ ,
	  55 /*   "Animal Taming" */ ,
	  75 /*   "Resist Environment" */ ,
	  65 /*   "Cure Wound&Poison" */ },
	 {							/* Nature Mastery (sval 1) */
	  55 /* ! "Stone to Mud" */ ,
	  65 /* ! "Lightning Bolt" */ ,
	  65 /*   "Nature Awareness" */ ,
	  65 /*   "Frost Bolt" */ ,
	  65 /*   "Ray of Sunlight" */ ,
	  65 /*   "Entangle" */ ,
	  65 /*   "Summon Animals" */ ,
	  65 /*   "Herbal Healing" */ },
	 {							/* Nature Gifts (sval 2) */
	  65 /* ! "Door Building" */ ,
	  45 /*   "Stair Building" */ ,
	  65 /* ! "Stone Skin" */ ,
	  65 /*   "Resistance True" */ ,
	  55 /*   "Animal Friend" */ ,
	  65 /*   "Stone Tell" */ ,
	  45 /*   "Wall of Stone" */ ,
	  45 /*   "Protect From Corros." */ },
	 {							/* Natures Wrath (sval 3) */
	  65 /* ! "Earthquake" */ ,
	  65 /*   "Whirlwind" */ ,
	  65 /* ! "Blizzard" */ ,
	  65 /*   "Lightning" */ ,
	  65 /*   "Whirpool" */ ,
	  65 /*   "Call Sunlight" */ ,
	  45 /*   "Elemental Brand" */ ,
	  65 /*   "Natures Wrath" */ }
	 },							/* end of Natural realm  */

	{							/* 4.Chaos Realm */
	 {							/* Sign of Chaos... (sval 0) */
	  95 /* "Magic Missile" */ ,
	  65 /* "Trap/Door Dest" */ ,
	  75 /* "Flash of Light" */ ,
	  55 /* "Touch of Conf" */ ,
	  65 /* "ManaBurst" */ ,
	  65 /* "Fire Bolt" */ ,
	  65 /* "Fist of Force" */ ,
	  75 /* "Teleport" */ },
	 {							/* Chaos Mastery... (sval 1) */
	  5 /*   "Wonder" */ ,
	  65 /*   "Chaos Bolt" */ ,
	  65 /*   "Sonic Boom" */ ,
	  65 /*   "Doom Beam" */ ,
	  65 /*   "Fire Ball" */ ,
	  65 /*   "Teleport Other" */ ,
	  65 /*   "Word of Dest" */ ,
	  55 /*   "Invoke Logrus" */ },
	 {							/* Chaos Channels (sval 2) */
	  45 /*   "Polymorph Other" */ ,
	  65 /*   "Chain Lightn" */ ,
	  65 /*   "Arcane Binding" */ ,
	  65 /*   "Disintegration" */ ,
	  55 /*   "Alter Reality" */ ,
	  5 /*   "Polymorph Self" */ ,
	  55 /*   "Chaos Binding" */ ,
	  55 /*   "Summon Demon" */ },
	 {							/* Armageddon Tome (sval 3) */
	  65 /*   "Gravity Beam" */ ,
	  65 /*   "Meteor Swarm" */ ,
	  65 /*   "Flame Strike" */ ,
	  65 /*   "Call Chaos" */ ,
	  75 /*   "Magic Rocket" */ ,
	  75 /*   "Mana Storm" */ ,
	  65 /*   "Breath Logrus" */ ,
	  65 /*   "Call Void" */ }
	 },							/* end of Chaos Realm */

	{							/* 5. Death Realm */
	 {							/* Black Prayers (sval 0) */
	  65 /* ! "Detect Unlife" */ ,
	  75 /*   "Maledition" */ ,
	  75 /* ! "Detect Evil" */ ,
	  75 /*   "Stinking Cloud" */ ,
	  65 /*   "Black Sleep" */ ,
	  65 /*   "Resist Poison" */ ,
	  65 /*   "Horrify" */ ,
	  65 /*   "Enslave Undead" */ },
	 {							/* Black Mass (sval 1) */
	  70 /* ! "Orb of Entropy" */ ,
	  65 /*   "Nether Bolt" */ ,
	  50 /*   "Terror" */ ,
	  65 /*   "Vamp Drain" */ ,
	  55 /*   "Poison Brand" */ ,
	  65 /*   "Disp Good" */ ,
	  65 /*   "Genocide" */ ,
	  65 /*   "Restore Life" */ },
	 {							/* Black Channels (sval 2) */
	  65 /* ! "Berserk" */ ,
	  65 /*   "Invoke Spirits" */ ,
	  65 /* ! "Dark Bolt" */ ,
	  85 /*   "Battle Frenzy" */ ,
	  65 /*   "Vamp True" */ ,
	  65 /*   "Vamp Brand" */ ,
	  65 /*   "Dark Storm" */ ,
	  65 /*   "Mass Genocide" */ },
	 {							/* Necronomicon (sval 3) */
	  65 /* ! "Death Ray" */ ,
	  65 /*   "Raise the Dead" */ ,
	  75 /* ! "Esoteria" */ ,
	  65 /*   "Word of Death" */ ,
	  65 /*   "Evocation" */ ,
	  65 /*   "Hellfire" */ ,
	  65 /*   "Omnicide" */ ,
	  55 /*   "Wraithform" */ }
	 },							/* end of Death Realm */


	{							/* Trump Realm */
	 {							/* Trump Magic (sval 0) */
	  95 /* ! "Phase Door" */ ,
	  85 /* ! "Mind Blast " */ ,
	  0 /*   "Shuffle" */ ,
	  0 /*   "Reset Recall" */ ,
	  75 /*   "Tlelport Self" */ ,
	  65 /*   "Dimension Door " */ ,
	  65 /*   "Trump Spying " */ ,
	  70 /*   "Teleport Away " */ },
	 {							/* Deck of Many Things (sval 1) */
	  0 /* ! "Trump Object " */ ,
	  0 /* ! "Trump animal " */ ,
	  85 /*   "Phantasmal Servant " */ ,
	  0 /*   "Trump Monster " */ ,
	  0 /*   "Conjure Elemental " */ ,
	  50 /*   "Teleport Level " */ ,
	  65 /*   "Word of recall " */ ,
	  65 /*   "Banishment" */ },
	 {							/* Trump of Doom (sval 2) */
	  0 /* ! "Joker Card " */ ,
	  0 /*   "Trump Spiders " */ ,
	  0 /*   "Trump Reptiles " */ ,
	  0 /*   "Trump Hounds " */ ,
	  0 /*   "Trump Branding " */ ,
	  0 /*   "Living Trump " */ ,
	  0 /*   "Death Dealing " */ ,
	  0 /*   "Trump Cyberdemon " */ },
	 {							/* Five Aces (sval 3) */
	  0 /* ! "Trump Divination " */ ,
	  0 /*   "Trump Lore " */ ,
	  0 /*   "Trump Undead " */ ,
	  0 /*   "Trump Dragon " */ ,
	  0 /*   "Mass Trump " */ ,
	  0 /*   "Trump Demon " */ ,
	  0 /*   "Trump Ancient Dragon " */ ,
	  0 /*   "Trump Greater Undead " */ }
	 },							/* end of Trump Realm */

	{							/* 7 Arcane Realm */
	 {							/* Cantrips (sval 0) */
	  85 /* ! "Zap" */ ,
	  85 /*   "Wiz Lock" */ ,
	  75 /*   "Det Invis" */ ,
	  75 /*   "Det Mon" */ ,
	  75 /*   "Blink" */ ,
	  75 /*   "Light Area" */ ,
	  85 /*   "Trap/Door Dest" */ ,
	  75 /*   "Cure Light Wounds" */ },
	 {							/* Minor Arcana (sval 1) */
	  75 /* ! "Det Door/Trap" */ ,
	  75 /* ! "Phlogiston" */ ,
	  75 /*   "Det Treasure" */ ,
	  75 /*   "Det Enchant" */ ,
	  75 /*   "Det Object" */ ,
	  75 /*   "Cure Poison" */ ,
	  75 /*   "Resist Cold" */ ,
	  75 /*   "Resist Fre" */ },
	 {							/* Major Arcana (sval 2) */
	  75 /* ! "Resist Elec" */ ,
	  75 /*   "Resist Acid" */ ,
	  75 /* ! "Cure Med Wounds" */ ,
	  75 /*   "Teleport" */ ,
	  85 /*   "Stone to Mud" */ ,
	  85 /*   "Ray of Light" */ ,
	  75 /*   "Satisfy Hunger" */ ,
	  75 /*   "See Invis" */ },
	 {							/* Manual of Mastery (sval 3) */
	  75 /* ! "Recharge" */ ,
	  75 /*   "Teleport Level" */ ,
	  85 /* ! "Ident" */ ,
	  85 /*   "Teleport Away" */ ,
	  70 /*   "Elemental Ball" */ ,
	  75 /*   "Detection" */ ,
	  75 /*   "Word of Recall" */ ,
	  75 /*   "Clairvoyance" */ }
	 }							/* end of Arcane Realm */

};

/*
 * Return the slot that items of the given type are wielded into
 *
 * Note that "rings" are tough because there are two slots
 *
 * Returns "-1" if the item cannot (or should not) be wielded
 */
int borg_wield_slot(list_item *l_ptr)
{
	if ((l_ptr->tval == TV_SWORD) ||
		(l_ptr->tval == TV_POLEARM) ||
		(l_ptr->tval == TV_HAFTED) ||
		(l_ptr->tval == TV_DIGGING)) return (EQUIP_WIELD);

	if ((l_ptr->tval == TV_DRAG_ARMOR) ||
		(l_ptr->tval == TV_HARD_ARMOR) ||
		(l_ptr->tval == TV_SOFT_ARMOR)) return (EQUIP_BODY);

	if (l_ptr->tval == TV_SHIELD) return (EQUIP_ARM);

	if ((l_ptr->tval == TV_CROWN) ||
		(l_ptr->tval == TV_HELM)) return (EQUIP_HEAD);

	if (l_ptr->tval == TV_BOW) return (EQUIP_BOW);

	if (l_ptr->tval == TV_RING) return (EQUIP_LEFT);

	if (l_ptr->tval == TV_AMULET) return (EQUIP_NECK);

	if (l_ptr->tval == TV_LITE) return (EQUIP_LITE);

	if (l_ptr->tval == TV_CLOAK) return (EQUIP_OUTER);

	if (l_ptr->tval == TV_GLOVES) return (EQUIP_HANDS);

	if (l_ptr->tval == TV_BOOTS) return (EQUIP_FEET);

	/* No slot available */
	return (-1);
}

/*
 * Find the index of the object_kind with the given tval and sval
 */
object_kind *borg_get_kind(int tval, int sval)
{
	int k;
	int num = 0;
	object_kind *kb_ptr = &k_info[0];

	/* Look for it */
	for (k = 1; k < z_info->k_max; k++)
	{
		object_kind *k_ptr = &k_info[k];

		/* Require correct tval */
		if (k_ptr->tval != tval) continue;

		/* Found a match */
		if (k_ptr->sval == sval) return (k_ptr);

		/* Ignore illegal items */
		if (sval != SV_ANY) continue;

		/* Apply the randomizer */
		if (!one_in_(++num)) continue;

		/* Use this value */
		kb_ptr = k_ptr;
	}

	/* Failure? */
	if (sval != SV_ANY)
	{
		/* Oops */
		borg_note_fmt("No object (%d,%d)", tval, sval);
	}

	return (kb_ptr);
}


/*
 * Find the slot of an item with the given tval/sval, if available.
 * Given multiple choices, choose the item with the largest "pval".
 * Given multiple choices, choose the smallest available pile.
 */
list_item *borg_slot(int tval, int sval)
{
	int i;

	object_kind *k_ptr;

	/* Scan the pack */
	for (i = 0; i < inven_num; i++)
	{
		list_item *l_ptr = &inventory[i];

		/* Skip un-aware items */
		if (!l_ptr->k_idx) continue;

		k_ptr = &k_info[l_ptr->k_idx];

		/* Require correct tval */
		if (k_ptr->tval != tval) continue;

		/* Require correct sval */
		if (k_ptr->sval != sval) continue;

		/* Hack - Prefer the first match, it is sorted nicely already */
		return (l_ptr);
	}

	/* Done */
	return (NULL);
}

/*
 * Get the index of an item so we can send commands to the game
 */
int look_up_index(list_item *l_ptr)
{
	int i;

	/* Scan inventory */
	for (i = 0; i < inven_num; i++)
	{
		if (&inventory[i] == l_ptr) return (i);
	}

	/* Paranoia */
	borg_oops("Trying to find invalid object!");

	return (-1);
}


/*
 * Hack -- refuel a torch
 */
bool borg_refuel_torch(void)
{
	list_item *l_ptr;

	/* Look for a torch */
	l_ptr = borg_slot(TV_LITE, SV_LITE_TORCH);

	/* None available */
	if (!l_ptr) return (FALSE);

	/* Must first wield before one can refuel */
	if (!equipment[EQUIP_LITE].k_idx)
	{
		return (FALSE);
	}

	/* Must wield torch */
	if (k_info[equipment[EQUIP_LITE].k_idx].sval != SV_LITE_TORCH)
	{
		return (FALSE);
	}

	/* Dont bother with empty */
	if (l_ptr->timeout == 0)
	{
		return (FALSE);
	}

	/* Cant refuel nothing */
	if (l_ptr->number == 0)
	{
		return (FALSE);
	}

	/* Log the message */
	borg_note_fmt("# Refueling with %s.", l_ptr->o_name);

	/* Perform the action */
	borg_keypress('F');
	borg_keypress(I2A(look_up_index(l_ptr)));

	/* Success */
	return (TRUE);
}


/*
 * Hack -- refuel a lantern
 */
bool borg_refuel_lantern(void)
{
	list_item *l_ptr;

	/* Look for a torch */
	l_ptr = borg_slot(TV_FLASK, 0);

	/* None available */
	if (!l_ptr) return (FALSE);

	/* Need to be wielding a light */
	if (!equipment[EQUIP_LITE].k_idx)
	{
		return (FALSE);
	}

	/* Cant refuel a torch with oil */
	if (k_info[equipment[EQUIP_LITE].k_idx].sval != SV_LITE_LANTERN)
	{
		return (FALSE);
	}

	/* Log the message */
	borg_note_fmt("# Refueling with %s.", l_ptr->o_name);

	/* Perform the action */
	borg_keypress('F');
	borg_keypress(I2A(look_up_index(l_ptr)));

	/* Success */
	return (TRUE);
}




/*
 * Hack -- attempt to eat the given food (by sval)
 */
bool borg_eat_food(int sval)
{
	list_item *l_ptr;

	/* Look for that food */
	l_ptr = borg_slot(TV_FOOD, sval);

	/* None available */
	if (!l_ptr) return (FALSE);

	/* Log the message */
	borg_note_fmt("# Eating %s.", l_ptr->o_name);

	/* Perform the action */
	borg_keypress('E');
	borg_keypress(I2A(look_up_index(l_ptr)));

	/* Success */
	return (TRUE);
}


static s32b when_last_quaff = 0;

/*
 * Quaff a potion of cure critical wounds.  This is a special case
 *   for several reasons.
 *   1) it is usually the only healing potion we have on us
 *   2) we should try to conserve a couple for when we really need them
 *   3) if we are burning through them fast we should probably teleport out of
 *      the fight.
 *   4) When it is the only/best way out of danger, drink away
  */
bool borg_quaff_crit(bool no_check)
{
	if (no_check)
	{
		if (borg_quaff_potion(SV_POTION_CURE_CRITICAL))
		{
			when_last_quaff = borg_t;
			return (TRUE);
		}
		return (FALSE);
	}

	/* Save the last two for when we really need them */
	if (bp_ptr->able.ccw < 2) return FALSE;

	/* Avoid drinking CCW twice in a row */
	if (when_last_quaff > (borg_t - 4) &&
		when_last_quaff <= borg_t && (randint0(100) < 75))
		return FALSE;

	if (borg_quaff_potion(SV_POTION_CURE_CRITICAL))
	{
		when_last_quaff = borg_t;
		return (TRUE);
	}
	return (FALSE);
}


/*
 * Hack -- attempt to quaff the given potion (by sval)
 */
bool borg_quaff_potion(int sval)
{
	list_item *l_ptr;

	/* Look for that potion */
	l_ptr = borg_slot(TV_POTION, sval);

	/* None available */
	if (!l_ptr) return (FALSE);

	/* Log the message */
	borg_note_fmt("# Quaffing %s.", l_ptr->o_name);

	/* Perform the action */
	borg_keypress('q');
	borg_keypress(I2A(look_up_index(l_ptr)));

	/* Success */
	return (TRUE);
}

/*
 * Hack -- attempt to quaff an unknown potion
 */
bool borg_quaff_unknown(void)
{
	int i;

	/* Scan the pack */
	for (i = 0; i < inven_num; i++)
	{
		list_item *l_ptr = &inventory[i];

		/* Require correct tval */
		if (l_ptr->tval != TV_POTION) continue;

		/* Skip aware items */
		if (l_ptr->k_idx) continue;

		/* Log the message */
		borg_note_fmt("# Quaffing unknown potion %s.", l_ptr->o_name);

		/* Perform the action */
		borg_keypress('q');
		borg_keypress(I2A(i));

		/* Success */
		return (TRUE);
	}

	/* None available */
	return (FALSE);
}

/*
 * Hack -- attempt to read an unknown scroll
 */
bool borg_read_unknown(void)
{
	int i;
	map_block *mb_ptr = map_loc(c_x, c_y);

	/* Scan the pack */
	for (i = 0; i < inven_num; i++)
	{
		list_item *l_ptr = &inventory[i];

		/* Skip aware items */
		if (l_ptr->k_idx) continue;

		/* Require correct tval */
		if (l_ptr->tval != TV_SCROLL) continue;

		/* Not when dark */
		if (!(mb_ptr->flags & MAP_GLOW) && !bp_ptr->cur_lite) return (FALSE);

		/* Blind or Confused */
		if (bp_ptr->status.blind || bp_ptr->status.confused) return (FALSE);

		/* Log the message */
		borg_note_fmt("# Reading unknown scroll %s.", l_ptr->o_name);

		/* Perform the action */
		borg_keypress('r');
		borg_keypress(I2A(i));

		/* Success */
		return (TRUE);
	}

	/* None available */
	return (FALSE);
}


/*
 * Hack -- attempt to eat an unknown potion.  This is done in emergencies.
 */
bool borg_eat_unknown(void)
{
	int i;

	/* Scan the pack */
	for (i = 0; i < inven_num; i++)
	{
		list_item *l_ptr = &inventory[i];

		/* Skip aware items */
		if (l_ptr->k_idx) continue;

		/* Require correct tval */
		if (l_ptr->tval != TV_FOOD) continue;

		/* Log the message */
		borg_note_fmt("# Eating unknown mushroom %s.", l_ptr->o_name);

		/* Perform the action */
		borg_keypress('E');
		borg_keypress(I2A(i));

		/* Success */
		return (TRUE);
	}

	/* None available */
	return (FALSE);
}

/*
 * Hack -- attempt to use an unknown staff.  This is done in emergencies.
 */
bool borg_use_unknown(void)
{
	int i;

	/* Scan the pack */
	for (i = 0; i < inven_num; i++)
	{
		list_item *l_ptr = &inventory[i];

		/* Skip aware items */
		if (l_ptr->k_idx) continue;

		/* Require correct tval */
		if (l_ptr->tval != TV_STAFF) continue;

		/* Log the message */
		borg_note_fmt("# Using unknown Staff %s.", l_ptr->o_name);

		/* Perform the action */
		borg_keypress('u');
		borg_keypress(I2A(i));

		/* Success */
		return (TRUE);
	}

	/* None available */
	return (FALSE);
}


/*
 * Hack -- attempt to read the given scroll (by sval)
 */
bool borg_read_scroll(int sval)
{
	list_item *l_ptr;
	map_block *mb_ptr = map_loc(c_x, c_y);

	/* Dark */
	if (!(mb_ptr->flags & MAP_GLOW) && !bp_ptr->cur_lite) return (FALSE);

	/* Blind or Confused */
	if (bp_ptr->status.blind || bp_ptr->status.confused) return (FALSE);

	/* Look for that scroll */
	l_ptr = borg_slot(TV_SCROLL, sval);

	/* None available */
	if (!l_ptr) return (FALSE);

	/* Log the message */
	borg_note_fmt("# Reading %s.", l_ptr->o_name);

	/* Perform the action */
	borg_keypress(ESCAPE);
	borg_keypress(ESCAPE);
	borg_keypress('r');
	borg_keypress(I2A(look_up_index(l_ptr)));

	/* reset recall depth in dungeon? */
	if ((sval == SV_SCROLL_WORD_OF_RECALL) &&
		(bp_ptr->depth < bp_ptr->max_depth) && bp_ptr->depth)
	{
		/* Do not reset Depth */
		borg_keypress('n');
	}

	/* Success */
	return (TRUE);
}

/*
 * Hack -- checks rod (by sval) and
 * make a fail check on it.
 */
bool borg_equips_rod(int sval)
{
	list_item *l_ptr;
	object_kind *k_ptr;

	int chance, lev;

	/* Look for that staff */
	l_ptr = borg_slot(TV_ROD, sval);

	/* None available */
	if (!l_ptr) return (FALSE);

	/* No charges */
	if (!l_ptr->pval) return (FALSE);

	/* Get item type */
	k_ptr = &k_info[l_ptr->k_idx];

	/* Extract the item level */
	lev = (k_ptr->level);

	/* Base chance of success */
	chance = bp_ptr->skill_dev;

	/* Confusion hurts skill */
	if (bp_ptr->status.confused) chance = chance / 2;

	/* High level objects are harder */
	chance = chance - ((lev > 50) ? 50 : lev);

	/* Roll for usage */
	if (chance < USE_DEVICE * 2) return (FALSE);

	/* Yep we got one */
	return (TRUE);
}



/*
 * Hack -- attempt to zap the given (charged) rod (by sval)
 */
bool borg_zap_rod(int sval)
{
	list_item *l_ptr;
	object_kind *k_ptr;

	int lev, chance;

	/* Look for that rod */
	l_ptr = borg_slot(TV_ROD, sval);

	/* None available */
	if (!l_ptr) return (FALSE);

	/* Hack -- Still charging */
	if (!l_ptr->pval) return (FALSE);

	/* Get item type */
	k_ptr = &k_info[l_ptr->k_idx];

	/* Extract the item level */
	lev = (k_ptr->level);

	/* Base chance of success */
	chance = bp_ptr->skill_dev;

	/* Confusion hurts skill */
	if (bp_ptr->status.confused) chance = chance / 2;

	/* High level objects are harder */
	chance = chance - ((lev > 50) ? 50 : lev);

	/* Roll for usage */
	if (chance < USE_DEVICE + 2) return (FALSE);

	/* Log the message */
	borg_note_fmt("# Zapping %s.", l_ptr->o_name);

	/* Perform the action */
	borg_keypress('z');
	borg_keypress(I2A(look_up_index(l_ptr)));

	/* Success */
	return (TRUE);
}


/*
 * Hack -- attempt to aim the given (charged) wand (by sval)
 */
bool borg_aim_wand(int sval)
{
	list_item *l_ptr;

	/* Look for that wand */
	l_ptr = borg_slot(TV_WAND, sval);

	/* None available */
	if (!l_ptr) return (FALSE);

	/* No charges */
	if (!l_ptr->pval) return (FALSE);

	/* Log the message */
	borg_note_fmt("# Aiming %s.", l_ptr->o_name);

	/* Perform the action */
	borg_keypress('a');
	borg_keypress(I2A(look_up_index(l_ptr)));

	/* Success */
	return (TRUE);
}


/*
 * Hack -- attempt to use the given (charged) staff (by sval)
 */
bool borg_use_staff(int sval)
{
	list_item *l_ptr;

	/* Look for that staff */
	l_ptr = borg_slot(TV_STAFF, sval);

	/* None available */
	if (!l_ptr) return (FALSE);

	/* No charges */
	if (!l_ptr->pval) return (FALSE);

	/* Log the message */
	borg_note_fmt("# Using %s.", l_ptr->o_name);

	/* Perform the action */
	borg_keypress('u');
	borg_keypress(I2A(look_up_index(l_ptr)));

	/* Success */
	return (TRUE);
}

/*
 * Hack -- attempt to use the given (charged) staff (by sval) and
 * make a fail check on it.
 */
bool borg_use_staff_fail(int sval)
{
	list_item *l_ptr;
	object_kind *k_ptr;

	int chance, lev;

	/* Look for that staff */
	l_ptr = borg_slot(TV_STAFF, sval);

	/* None available */
	if (!l_ptr) return (FALSE);

	/* No charges */
	if (!l_ptr->pval) return (FALSE);

	/* Get item type */
	k_ptr = &k_info[l_ptr->k_idx];

	/* Extract the item level */
	lev = k_ptr->level;

	/* Base chance of success */
	chance = bp_ptr->skill_dev;

	/* Confusion hurts skill */
	if (bp_ptr->status.confused) chance = chance / 2;

	/* High level objects are harder */
	chance = chance - ((lev > 50) ? 50 : lev);

	/* Roll for usage, but if its a Teleport be generous. */
	if (chance < USE_DEVICE * 2)
	{
		if (sval != SV_STAFF_TELEPORTATION)
		{
			return (FALSE);
		}

		/* We need to give some "desparation attempt to teleport staff" */
		if (!bp_ptr->status.confused && !bp_ptr->status.blind)
		{
			/* We really have no chance, return false, attempt the scroll */
			if (chance < USE_DEVICE) return (FALSE);
		}
		/* We might have a slight chance, or we cannot not read */
	}

	/* Log the message */
	borg_note_fmt("# Using %s.", l_ptr->o_name);

	/* Perform the action */
	borg_keypress('u');
	borg_keypress(I2A(look_up_index(l_ptr)));

	/* Success */
	return (TRUE);
}

/*
 * Hack -- checks staff (by sval) and
 * make a fail check on it.
 */
bool borg_equips_staff_fail(int sval)
{
	list_item *l_ptr;
	object_kind *k_ptr;

	int chance, lev;

	/* Look for that staff */
	l_ptr = borg_slot(TV_STAFF, sval);

	/* None available */
	if (!l_ptr) return (FALSE);

	/* No charges */
	if (!l_ptr->pval) return (FALSE);

	/* Get item type */
	k_ptr = &k_info[l_ptr->k_idx];

	/* Extract the item level */
	lev = k_ptr->level;

	/* Base chance of success */
	chance = bp_ptr->skill_dev;

	/* Confusion hurts skill */
	if (bp_ptr->status.confused) chance = chance / 2;

	/* High level objects are harder */
	chance = chance - ((lev > 50) ? 50 : lev);

	/* Roll for usage, but if its a Teleport be generous. */
	if (chance < USE_DEVICE * 2)
	{
		if (sval != SV_STAFF_TELEPORTATION && sval != SV_STAFF_DESTRUCTION)
		{
			return (FALSE);
		}

		/* We need to give some "desparation attempt to teleport staff" */
		if (!bp_ptr->status.confused)
		{
			/* We really have no chance, return false, attempt the scroll */
			if (chance < USE_DEVICE) return (FALSE);
		}

		/* We might have a slight chance, continue on */
	}

	/* Yep we got one */
	return (TRUE);
}


/*
 * Hack -- attempt to use the given artifact (by index)
 */
bool borg_activate_artifact(int name1, bool secondary)
{
	/* int i; */

	/* Hack - ignore unused parameter */
	(void)name1;
	(void)secondary;

#if 0
	/* Check the equipment */
	for (i = 0; i < equip_num; i++)
	{
		list_item *l_ptr = &equipment[i];

		/* Skip non-artifacts */
		if (!(l_ptr->kn_flags3 & TR3_INSTA_ART)) continue;

		/* Check charge */
		if (l_ptr->timeout) return (FALSE);

		/*
		 * Random Artifact must be *ID* to know the activation power.
		 * The borg will cheat with random artifacts to know if the
		 * artifact number is activatable, but artifact names and
		 * types will be scrambled.  So he must first *ID* the artifact
		 * he must play with the artifact to learn its power, just as
		 * he plays with magic to gain experience.  But I am not about
		 * to undertake that coding.  He needs to *ID* it anyway to learn
		 * of the resists that go with the artifact.
		 * Lights dont need *id* just regular id.
		 */
		if ((i != EQUIP_LITE) && !borg_obj_known_full(l_ptr))
		{
			borg_note_fmt("# %s must be *ID*'d before activation.",
						  l_ptr->o_name);
			return (FALSE);
		}

		/* Log the message */
		borg_note_fmt("# Activating artifact %s.", l_ptr->o_name);

		/* Perform the action */
		borg_keypress('A');
		borg_keypress(I2A(i));
#if 0
		/* Jewel also gives Recall */
		if (item->name1 == ART_THRAIN)
		{
			if (secondary == FALSE)
			{
				borg_keypress('n');
			}
			else
			{
				borg_keypress('y');
			}
		}
#endif /* 0 */
		/* Success */
		return (TRUE);
	}
#endif /* 0 */

	/* Oops */
	return (FALSE);
}

/*
 * Hack -- check and see if borg is wielding a dragon armor and if
 * he will pass a fail check.
 */
bool borg_equips_dragon(int drag_sval)
{
	int lev, chance;

	object_kind *k_ptr;

	/* Check the equipment */
	list_item *l_ptr = &equipment[EQUIP_BODY];

	/* Get object type */
	k_ptr = &k_info[l_ptr->k_idx];

	/* Skip incorrect armours */
	if (k_ptr->tval != TV_DRAG_ARMOR) return (FALSE);
	if (k_ptr->sval != drag_sval) return (FALSE);

	/* Check charge */
	if (l_ptr->timeout) return (FALSE);

	/* Make Sure Mail is IDed */
	if (!borg_obj_known_p(l_ptr)) return (FALSE);

	/* check on fail rate
	 * The fail check is automatic for dragon armor.  It is an attack
	 * item.  He should not sit around failing 5 or 6 times in a row.
	 * he should attempt to activate it, and if he is likely to fail, then
	 * eh should look at a different attack option.  We are assuming
	 * that the fail rate is about 50%.  So He may still try to activate it
	 * and fail.  But he will not even try if he has negative chance or
	 * less than twice the USE_DEVICE variable
	 */
	/* Extract the item level */
	lev = k_ptr->level;

	/* Base chance of success */
	chance = bp_ptr->skill_dev;

	/* Confusion hurts skill */
	if (bp_ptr->status.confused) chance = chance / 2;

	/* High level objects are harder */
	chance = chance - ((lev > 50) ? 50 : lev);

	/* Roll for usage */
	if (chance < (USE_DEVICE * 2)) return (FALSE);

	/* Success */
	return (TRUE);

}

/*
 * apw Hack -- attempt to use the given dragon armour
 */
bool borg_activate_dragon(int drag_sval)
{
	object_kind *k_ptr;

	/* Check the equipment */
	list_item *l_ptr = &equipment[EQUIP_BODY];

	/* Get object type */
	k_ptr = &k_info[l_ptr->k_idx];

	/* Skip incorrect mails */
	if (k_ptr->tval != TV_DRAG_ARMOR) return (FALSE);
	if (k_ptr->sval != drag_sval) return (FALSE);

	/* Check charge */
	if (l_ptr->timeout) return (FALSE);

	/* apw Make Sure Mail is IDed */
	if (!borg_obj_known_p(l_ptr)) return (FALSE);

	/* Log the message */
	borg_note_fmt("# Activating dragon scale %s.", l_ptr->o_name);

	/* Perform the action */
	borg_keypress('A');
	borg_keypress(I2A(EQUIP_BODY));

	/* Success */
	return (TRUE);
}


/*
 * Determine if borg can cast a given spell (when fully rested)
 */
bool borg_spell_legal(int realm, int book, int what)
{
	borg_magic *as = &borg_magics[realm][book][what];

	/* The borg must be able to "cast" spells this realm */
	if (bp_ptr->realm1 != realm && bp_ptr->realm2 != realm) return (FALSE);

	/* Make sure we have this realm book */
	if (amt_book[realm][book] <= 0) return (FALSE);

	/* The spell must be "known" */
	if (as->status < BORG_MAGIC_TEST) return (FALSE);

	/* The spell must be affordable (when rested) */
	if (as->power > bp_ptr->msp) return (FALSE);

	/* Success */
	return (TRUE);
}

/*
 * Determine if borg can cast a given spell (right now)
 */
bool borg_spell_okay(int realm, int book, int what)
{
	int reserve_mana = 0;

	borg_magic *as = &borg_magics[realm][book][what];

	map_block *mb_ptr = map_loc(c_x, c_y);

	/* Dark */
	if (!(mb_ptr->flags & MAP_GLOW) && !bp_ptr->cur_lite) return (FALSE);

	/* Define reserve_mana for each class */
	if (bp_ptr->realm1 == REALM_SORCERY) reserve_mana = 6;
	if (bp_ptr->realm1 == REALM_TRUMP) reserve_mana = 6;
	if (bp_ptr->realm1 == REALM_ARCANE) reserve_mana = 15;
	if (bp_ptr->realm1 == REALM_CHAOS) reserve_mana = 15;

	/* Low level spell casters should not worry about this */
	if (bp_ptr->lev < 35) reserve_mana = 0;

	/* Require ability (when rested) */
	if (!borg_spell_legal(realm, book, what)) return (FALSE);

	/* Hack -- blind/confused */
	if (bp_ptr->status.blind || bp_ptr->status.confused) return (FALSE);


	/* The spell must be affordable (now) */
	if (as->power > bp_ptr->csp) return (FALSE);

	/* Do not cut into reserve mana (for final teleport) */
	if (bp_ptr->csp - as->power < reserve_mana && realm == REALM_SORCERY)
	{
		/* Phase spells ok */
		if (book == 0 && what == 2) return (TRUE);

		/* Teleport spells ok */
		if (book == 1 && what == 5) return (TRUE);

		/* Satisfy Hunger OK */
		if (book == 2 && what == 0) return (TRUE);

		/* others are rejected */
		return (FALSE);
	}

	/* Success */
	return (TRUE);
}

/*
 * fail rate on a spell
 */
static int borg_spell_fail_rate(int realm, int book, int what)
{
	int chance, minfail;
	borg_magic *as = &borg_magics[realm][book][what];

	/* Access the spell  */
	chance = as->sfail;

	/* Reduce failure rate by "effective" level adjustment */
	chance -= 3 * (bp_ptr->lev - as->level);

	/* Reduce failure rate by INT/WIS adjustment */
	chance -= 3 * (adj_mag_stat[my_stat_ind[A_INT]] - 1);

	/* Extract the minimum failure rate */
	minfail = adj_mag_fail[my_stat_ind[A_INT]];

	/* Non mage characters never get too good */
	if (borg_class != CLASS_MAGE)
	{
		if (minfail < 5) minfail = 5;
	}

	/* Minimum failure rate */
	if (chance < minfail) chance = minfail;

	/* Stunning makes spells harder */
	if (bp_ptr->status.heavy_stun) chance += 25;
	if (bp_ptr->status.stun) chance += 15;

	/* Always a 5 percent chance of working */
	if (chance > 95) chance = 95;

	/* Return the chance */
	return (chance);


}

/*
 * same as borg_spell_okay with a fail % check
 */
bool borg_spell_okay_fail(int realm, int book, int what, int allow_fail)
{
	if (borg_spell_fail_rate(realm, book, what) > allow_fail)
		return FALSE;
	return borg_spell_okay(realm, book, what);
}

/*
 * Same as borg_spell with a fail % check
 */
bool borg_spell_fail(int realm, int book, int what, int allow_fail)
{
	if (borg_spell_fail_rate(realm, book, what) > allow_fail)
		return FALSE;
	return borg_spell(realm, book, what);
}

/*
 * Same as borg_spell_legal with a fail % check
 */
bool borg_spell_legal_fail(int realm, int book, int what, int allow_fail)
{
	if (borg_spell_fail_rate(realm, book, what) > allow_fail)
		return FALSE;
	return borg_spell_legal(realm, book, what);
}

/*
 * Attempt to cast a spell
 */
bool borg_spell(int realm, int book, int what)
{
	int i;

	borg_magic *as = &borg_magics[realm][book][what];

	/* Require ability (right now) */
	if (!borg_spell_okay(realm, book, what)) return (FALSE);

	/* Not if locked down */
	if (bp_ptr->flags3 & TR3_NO_MAGIC) return (FALSE);

	/* Look for the book */
	i = borg_book[realm][book];

	/* Paranoia */
	if (i < 0) return (FALSE);

	/* Debugging Info */
	borg_note_fmt("# Casting %s (%d,%d).", as->name, book, what);

	/* Cast a spell */
	borg_keypress('m');
	borg_keypress(I2A(i));
	borg_keypress(I2A(what));

	/* increment the spell counter */
	as->times++;

	/* Dimension Door -- Must target the landing zone */
	if ((realm == REALM_SORCERY && book == 2 && what == 3) ||
		(realm == REALM_TRUMP && book == 0 && what == 5))
	{
		int x1, y1, x2, y2;

		/* Report a little bit */
		borg_note_fmt
			("# Targetting Landing Zone (%d,%d)", dim_door_x, dim_door_y);

		/* Determine "path" */
		x1 = c_x;
		y1 = c_y;
		x2 = dim_door_x;
		y2 = dim_door_y;

		/* Move to the location (diagonals) */
		for (; (y1 < y2) && (x1 < x2); y1++, x1++) borg_keypress('3');
		for (; (y1 < y2) && (x1 > x2); y1++, x1--) borg_keypress('1');
		for (; (y1 > y2) && (x1 < x2); y1--, x1++) borg_keypress('9');
		for (; (y1 > y2) && (x1 > x2); y1--, x1--) borg_keypress('7');

		/* Move to the location */
		for (; y1 < y2; y1++) borg_keypress('2');
		for (; y1 > y2; y1--) borg_keypress('8');
		for (; x1 < x2; x1++) borg_keypress('6');
		for (; x1 > x2; x1--) borg_keypress('4');

		/* Select the target */
		borg_keypress(' ');
	}

	/* Success */
	return (TRUE);
}

/*** Mindcrafter spells are much like realm spells ***/

/*
 * Determine if borg can cast a given Mindcraft spell (when fully rested)
 */
bool borg_mindcr_legal(int spell, int level)
{
	borg_mind *as = &borg_minds[spell];

	/* The borg must be able to "cast" spells this realm */
	if (borg_class != CLASS_MINDCRAFTER) return (FALSE);

	/* The spell must be "known" */
	if (bp_ptr->lev < level) return (FALSE);

	/* The spell must be affordable (when rested) */
	if (as->power > bp_ptr->msp) return (FALSE);

	/* Success */
	return (TRUE);
}

/*
 * Determine if borg can cast a given spell (right now)
 */
bool borg_mindcr_okay(int spell, int level)
{
	int reserve_mana = 0;

	borg_mind *as = &borg_minds[spell];

	map_block *mb_ptr = map_loc(c_x, c_y);

	/* Dark */
	if (!(mb_ptr->flags & MAP_GLOW) && !bp_ptr->cur_lite) return (FALSE);

	/* Define reserve_mana for Displacement */
	if (bp_ptr->lev >= 3) reserve_mana = 2;
	if (bp_ptr->lev >= 7) reserve_mana = 6;

	/* Low level spell casters should not worry about this */
	if (bp_ptr->lev < 35) reserve_mana = 0;

	/* Require ability (when rested) */
	if (!borg_mindcr_legal(spell, level)) return (FALSE);

	/* Hack -- blind/confused */
	if (bp_ptr->status.blind || bp_ptr->status.confused) return (FALSE);


	/* The spell must be affordable (now) */
	if (as->power > bp_ptr->csp) return (FALSE);

	/* Do not cut into reserve mana (for final teleport) */
	if (bp_ptr->csp - as->power < reserve_mana)
	{
		/* Minor Displacement spells ok */
		if (spell == 2) return (TRUE);

		/* Major Displacement ok */
		if (spell == 3) return (TRUE);

		/* others are rejected */
		return (FALSE);
	}
	/* Success */
	return (TRUE);
}

/*
 * fail rate on a mindcrafter spell
 */
static int borg_mindcr_fail_rate(int spell, int level)
{
	int chance, minfail;
	borg_mind *as = &borg_minds[spell];

	/* Hack - ignore parameter */
	(void)level;

	/* Access the spell  */
	chance = as->sfail;

	/* Reduce failure rate by "effective" level adjustment */
	chance -= 3 * (bp_ptr->lev - as->level);

	/* Reduce failure rate by INT/WIS adjustment */
	chance -= 3 * (adj_mag_stat[my_stat_ind[A_WIS]] - 1);

	/* Extract the minimum failure rate */
	minfail = adj_mag_fail[my_stat_ind[A_WIS]];


	/* Minimum failure rate */
	if (chance < minfail) chance = minfail;

	/* Stunning makes spells harder */
	if (bp_ptr->status.heavy_stun) chance += 25;
	if (bp_ptr->status.stun) chance += 15;

	/* Always a 5 percent chance of working */
	if (chance > 95) chance = 95;

	/* Return the chance */
	return (chance);
}

/*
 * same as borg_mind_okay with a fail % check
 */
bool borg_mindcr_okay_fail(int spell, int level, int allow_fail)
{
	if (borg_mindcr_fail_rate(spell, level) > allow_fail)
		return FALSE;
	return borg_mindcr_okay(spell, level);
}

/*
 * Same as borg_mind with a fail % check
 */
bool borg_mindcr_fail(int spell, int level, int allow_fail)
{
	if (borg_mindcr_fail_rate(spell, level) > allow_fail)
		return FALSE;
	return borg_mindcr(spell, level);
}

/*
 * Same as borg_mind_legal with a fail % check
 */
bool borg_mindcr_legal_fail(int spell, int level, int allow_fail)
{
	if (borg_mindcr_fail_rate(spell, level) > allow_fail)
		return FALSE;
	return borg_mindcr_legal(spell, level);
}

/*
 * Attempt to cast a mindcrafter spell
 */
bool borg_mindcr(int spell, int level)
{
	borg_mind *as = &borg_minds[spell];

	/* Require ability (right now) */
	if (!borg_mindcr_okay(spell, level)) return (FALSE);

	/* Not if locked down */
	if (bp_ptr->flags3 & TR3_NO_MAGIC) return (FALSE);

	/* Debugging Info */
	borg_note_fmt
		("# Casting %s (spell: %d, level: %d).", as->name, spell, level);

	/* Cast a spell */
	borg_keypress('m');
	borg_keypress(as->letter);

	/* increment the spell counter */
	as->times++;

	/* Dimension Door -- need a landing Zone */
	if (spell == MIND_MINOR_DISP && level >= 40)
	{
		int x1, y1, x2, y2;

		/* Report a little bit */
		borg_note_fmt
			("# Targetting Landing Zone (%d,%d)", dim_door_x, dim_door_y);

		/* Determine "path" */
		x1 = c_x;
		y1 = c_y;
		x2 = dim_door_x;
		y2 = dim_door_y;

		/* Move to the location (diagonals) */
		for (; (y1 < y2) && (x1 < x2); y1++, x1++) borg_keypress('3');
		for (; (y1 < y2) && (x1 > x2); y1++, x1--) borg_keypress('1');
		for (; (y1 > y2) && (x1 < x2); y1--, x1++) borg_keypress('9');
		for (; (y1 > y2) && (x1 > x2); y1--, x1--) borg_keypress('7');

		/* Move to the location */
		for (; y1 < y2; y1++) borg_keypress('2');
		for (; y1 > y2; y1--) borg_keypress('8');
		for (; x1 < x2; x1++) borg_keypress('6');
		for (; x1 > x2; x1--) borg_keypress('4');

		/* Select the target */
		borg_keypress(' ');
	}

	/* Success */
	return (TRUE);
}


/*** Racial abilities are much like magic spells ***/

/*
 * Determine if borg can cast a given Racial spell
 * (when fully rested).
 * -or-
 * with a reasonable degree of difficulty with Check_fail
 */
bool borg_racial_check(int race, bool check_fail)
{

	int i;
	int val;
	int sum = 0;

	int lev_req = 99;
	int cost = 0;
	int stat = 0;
	int diff = 0;
	int use_stat = 0;
	int difficulty = 0;

	bool use_hp = FALSE;

	/* The borg must be able to "cast" spells this race */
	if (borg_race != race) return (FALSE);

	/* The spell must be "known" */
	switch (borg_race)
	{
		case RACE_HUMAN:
		case RACE_HALF_ELF:
		case RACE_ELF:
		{
			lev_req = 99;
			break;
		}
		case RACE_HOBBIT:
		{
			lev_req = 15;
			cost = 10;
			use_stat = A_INT;
			diff = 10;
			break;
		}
		case RACE_GNOME:
		{
			lev_req = 5;
			cost = 5 + (bp_ptr->lev / 5);
			use_stat = A_INT;
			diff = 12;
			break;
		}
		case RACE_DWARF:
		{
			lev_req = 5;
			cost = 5;
			use_stat = A_WIS;
			diff = 12;
			break;
		}
		case RACE_HALF_ORC:
		{
			lev_req = 3;
			cost = 5;
			use_stat = A_WIS;
			diff = ((borg_class == CLASS_WARRIOR) ? 5 : 10);
			break;
		}
		case RACE_HALF_TROLL:
		{
			lev_req = 10;
			cost = 12;
			use_stat = A_WIS;
			diff = ((borg_class == CLASS_WARRIOR) ? 6 : 12);
			break;
		}
		case RACE_AMBERITE:	/* not coded yet */
		{
			lev_req = 99;
			cost = 5;
			use_stat = A_WIS;
			diff = 50;
			break;
		}
		case RACE_HIGH_ELF:
		{
			lev_req = 99;
			cost = 0;
			use_stat = A_WIS;
			diff = 0;
			break;
		}
		case RACE_BARBARIAN:
		{
			lev_req = 8;
			cost = 10;
			use_stat = A_WIS;
			diff = ((borg_class == CLASS_WARRIOR) ? 6 : 12);
			break;
		}
		case RACE_HALF_OGRE:
		{
			lev_req = 25;
			cost = 35;
			use_stat = A_STR;
			diff = 12;
			break;
		}
		case RACE_HALF_GIANT:
		{
			lev_req = 99;		/* no support */
			cost = 35;
			use_stat = A_INT;
			diff = 15;
			break;
		}
		case RACE_HALF_TITAN:
		{
			lev_req = 99;		/* no support */
			cost = 20;
			use_stat = A_INT;
			diff = 12;
			break;
		}
		case RACE_CYCLOPS:
		{
			lev_req = 20;
			cost = 15;
			use_stat = A_STR;
			diff = 12;
			break;
		}
		case RACE_YEEK:
		{
			lev_req = 15;
			cost = 15;
			use_stat = A_WIS;
			diff = 10;
			break;
		}
		case RACE_KLACKON:
		{
			lev_req = 9;
			cost = 9;
			use_stat = A_DEX;
			diff = 14;
			break;
		}
		case RACE_KOBOLD:
		{
			lev_req = 12;
			cost = 8;
			use_stat = A_DEX;
			diff = 14;
			break;
		}
		case RACE_NIBELUNG:
		{
			lev_req = 10;
			cost = 5;
			use_stat = A_WIS;
			diff = 10;
			break;
		}
		case RACE_DARK_ELF:
		{
			lev_req = 2;
			cost = 2;
			use_stat = A_INT;
			diff = 9;
			break;
		}
		case RACE_DRACONIAN:
		{
			lev_req = 1;
			cost = bp_ptr->lev;
			use_stat = A_CON;
			diff = 12;
			break;
		}
		case RACE_MIND_FLAYER:
		{
			lev_req = 15;
			cost = 12;
			use_stat = A_STR;
			diff = 15;
			break;
		}
		case RACE_IMP:
		{
			lev_req = 9;
			cost = 15;
			use_stat = A_WIS;
			diff = 15;
			break;
		}
		case RACE_GOLEM:
		{
			lev_req = 20;
			cost = 15;
			use_stat = A_CON;
			diff = 8;
			break;
		}
		case RACE_SKELETON:
		case RACE_ZOMBIE:
		{
			lev_req = 30;
			cost = 30;
			use_stat = A_WIS;
			diff = 18;
			break;
		}
		case RACE_VAMPIRE:
		{
			lev_req = 2;
			cost = 1 + (bp_ptr->lev / 3);
			use_stat = A_WIS;
			diff = 18;
			break;
		}
		case RACE_SPECTRE:
		{
			lev_req = 4;
			cost = 6;
			use_stat = A_INT;
			diff = 3;
			break;
		}
		case RACE_SPRITE:
		{
			lev_req = 12;
			cost = 12;
			use_stat = A_INT;
			diff = 15;
			break;
		}
		case RACE_BEASTMAN:
		{
			lev_req = 99;		/* No ability */
			cost = 30;
			use_stat = A_WIS;
			diff = 18;
			break;
		}
	}

	/* Power is not available yet */
	if (bp_ptr->lev < lev_req) return (FALSE);

	/* Not enough mana - use hp */
	if (bp_ptr->msp < cost) use_hp = TRUE;

	/* Too confused */
	if (bp_ptr->status.confused) return FALSE;

	/* Cost -- dont go into debt */
	if (use_hp && (cost > bp_ptr->chp * 7 / 10)) return (FALSE);

	/* Legal check ends here */
	if (!check_fail) return (TRUE);

	/* Otherwise continue on to a fail check */

	/* Reasonable chance of success */
	stat = my_stat_cur[use_stat];

	/* Stun makes it more difficult */
	if (bp_ptr->status.stun)
	{
		difficulty += 10;
	}
	else
	{
		int lev_adj = (bp_ptr->lev - lev_req) / 3;
		if (lev_adj > 10) lev_adj = 10;
		difficulty -= lev_adj;
	}

	if (difficulty < 5) difficulty = 5;

	/* We only need halfs of the difficulty */
	difficulty = difficulty / 2;

	for (i = 1; i <= stat; i++)
	{
		val = i - difficulty;
		if (val > 0)
			sum += (val <= difficulty) ? val : difficulty;
	}

	if (difficulty >= 40)
		return (FALSE);
	else
		/* Success */
		return (TRUE);
}


/*
 * Attempt to cast a racial spell
 */
bool borg_racial(int race)
{
	/* Require ability (right now) */
	if (!borg_racial_check(race, TRUE)) return (FALSE);

	/* Cost -- dont go into debt */
	if (bp_ptr->chp < bp_ptr->mhp * 5 / 10 &&
		borg_race != RACE_GNOME) return (FALSE);

	/* Gnomes can go into emergency zone (mostly) */
	if (bp_ptr->chp < bp_ptr->mhp * 3 / 10 &&
		borg_race == RACE_GNOME) return (FALSE);

	/* Debugging Info */
	borg_note("# Racial Power.");

	/* Cast a spell */
	borg_keypress('U');
	borg_keypress('a');

	/* Success */
	return (TRUE);
}

/*
 * Hack -- Cheat the "spell" info
 *
 * Hack -- note the use of the "cheat" field for efficiency
 */
void borg_cheat_spell(int realm)
{
	int j, what;
	int book;

	/* Can we use spells/prayers? */
	if (realm == 0) return;


	/* process books */
	for (book = 0; book < 4; book++)
	{
		/* Process the spells */
		for (what = 0; what < 8; what++)
		{
			/* Access the spell */
			borg_magic *as = &borg_magics[realm][book][what];

			/* Skip illegible spells */
			if (as->status == BORG_MAGIC_ICKY) continue;

			/* Access the index */
			j = as->cheat;

			/* Note "forgotten" spells */
			if ((realm == bp_ptr->realm1) ?
				((p_ptr->spell.r[0].forgotten & (1L << j))) :
				((p_ptr->spell.r[1].forgotten & (1L << j))))
			{
				/* Forgotten */
				as->status = BORG_MAGIC_LOST;
			}

			/* Note "difficult" spells */
			else if (bp_ptr->lev < as->level)
			{
				/* Unknown */
				as->status = BORG_MAGIC_HIGH;
			}

			/* Note "unknown" spells */
			else if (!((realm == bp_ptr->realm1) ?
					   (p_ptr->spell.r[0].learned & (1L << j)) :
					   (p_ptr->spell.r[1].learned & (1L << j))))
			{
				/* Unknown */
				as->status = BORG_MAGIC_OKAY;
			}

			/* Note "untried" spells */
			else if (!((realm == bp_ptr->realm1) ?
					   (p_ptr->spell.r[0].learned & (1L << j)) :
					   (p_ptr->spell.r[1].learned & (1L << j))))
			{
				/* Untried */
				as->status = BORG_MAGIC_TEST;
			}

			/* Note "known" spells */
			else
			{
				/* Known */
				as->status = BORG_MAGIC_KNOW;
			}
		}						/* book */
	}							/* Realm */
}


/*
 * Prepare a book
 */
static void prepare_book_info(int realm, int book)
{
	int i, what;

	int spell[64], num = 0;

	/* Reset each spell entry */
	for (what = 0; what < 8; what++)
	{
		borg_magic *as = &borg_magics[realm][book][what];

		/* Assume no name */
		as->name = NULL;

		/* Know the Realm, if any */
		as->realm = realm;

		/* Assume illegible */
		as->status = BORG_MAGIC_ICKY;

		/* Assume illegible */
		as->method = BORG_MAGIC_ICK;

		/* Impossible values */
		as->level = 99;
		as->power = 99;

		/* Impossible value */
		as->cheat = 99;

		/* Delete the text name */
		as->realm_name = NULL;
	}


	/* Can we use spells/prayers? */
	if (!bp_ptr->realm1) return;


	/* Extract spells */
	for (i = 0; i < 32; i++)
	{
		/* Check for this spell */
		if ((fake_spell_flags[book] & (1L << i)))
		{
			/* Collect this spell */
			spell[num++] = i;
		}
	}

	/* Process each existing spell */
	for (what = 0; what < num; what++)
	{
		borg_magic *as = &borg_magics[realm][book][what];

		magic_type *s_ptr = &pmb_ptr->info[realm][spell[what]];

		/* Skip "illegible" spells */
		if (s_ptr->slevel == 99) continue;

		/* Save the spell name */
		as->name = spell_names[realm][spell[what]];

		/* Realm Name */
		if (realm == 1) as->realm_name = "Life";
		if (realm == 2) as->realm_name = "Sorcery";
		if (realm == 3) as->realm_name = "Nature";
		if (realm == 4) as->realm_name = "Chaos";
		if (realm == 5) as->realm_name = "Death";
		if (realm == 6) as->realm_name = "Trump";
		if (realm == 7) as->realm_name = "Arcane";

		/* Save the Realm, if any */
		as->realm = realm;

		/* Save the spell index */
		as->cheat = spell[what];

		/* Hack -- assume excessive level */
		as->status = BORG_MAGIC_HIGH;

		/* Access the correct "method" */
		as->method = borg_magic_method[realm][book][what];

		/* Access the correct "rating" */
		as->rating = borg_magic_rating[realm][book][what];

		/* Extract the level and power */
		as->level = s_ptr->slevel;
		as->power = s_ptr->smana;

		/* extract fail rate. */
		as->sfail = s_ptr->sfail;
	}
}

/*
 * Prepare a Mindcrafter Array
 */
static void prepare_mind_info(void)
{
	int spell;

	/* Reset each spell entry */
	for (spell = 0; spell < MINDCRAFT_MAX; spell++)
	{
		borg_mind *as = &borg_minds[spell];
		mindcraft_power *s_ptr = &mindcraft_powers[spell];

		/* name */
		as->name = s_ptr->name;

		/* values */
		as->level = s_ptr->min_lev;
		as->power = s_ptr->mana_cost;

		/* Fail Rate */
		as->sfail = s_ptr->fail;

		/* Delete the text letter address */
		as->letter = 'a' + spell;

	}
}


/*
 * Hack -- prepare some stuff based on the player race and class
 */
void prepare_race_class_info(void)
{
	int book;

	/* Hack -- Realms */
	bp_ptr->realm1 = p_ptr->spell.r[0].realm;
	bp_ptr->realm2 = p_ptr->spell.r[1].realm;

	/* Initialize the various spell arrays by book */
	for (book = 0; book < 4; book++)
	{
		prepare_book_info(bp_ptr->realm1, book);
	}

	for (book = 0; book < 4; book++)
	{
		prepare_book_info(bp_ptr->realm2, book);
	}

	/* MindCrafters */
	if (borg_class == CLASS_MINDCRAFTER)
	{
		prepare_mind_info();
	}
}

/*
 * Initialize this file
 */
void borg_init_3(void)
{
	/* Track the shop locations */
	track_shop_num = 0;
	track_shop_size = 16;

	/* Make the stores in the town */
	C_MAKE(borg_shops, track_shop_size, borg_shop);
}

#else

#ifdef MACINTOSH
static int HACK = 0;
#endif

#endif
