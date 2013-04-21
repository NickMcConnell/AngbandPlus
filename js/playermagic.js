/*********************************************************
* All sources in Hellbound are Copyright (c) 2008 Konijn *
* I Konijn, release all code and data under the terms of *
* of the GNU General Public License (version 2), as well *
* as under the traditional Angband license. Distribution *
* is allowed both under the terms of the GPL (version 2) *
* or under the terms of the traditional Angband license. *
*********************************************************/

//Global Objects

//Binary choices for character creation
var CH_NONE          = 0x000
var CH_MIRACLES      = 0x001
var CH_SORCERY       = 0x002
var CH_NATURE        = 0x004
var CH_CHAOS         = 0x008
var CH_DEATH         = 0x010
var CH_TAROT         = 0x020
var CH_CHARMS        = 0x040
var CH_SOMATIC       = 0x080
var CH_DEMONIC       = 0x100
var CH_ALL           = 0x1FF //<- Update this when adding a new realm

//Integers representing the chosen realm(s)
var REALM_NONE       = 0
var REALM_MIRACLES   = 1
var REALM_SORCERY    = 2
var REALM_NATURE     = 3
var REALM_CHAOS      = 4
var REALM_DEATH      = 5
var REALM_TAROT      = 6
var REALM_CHARMS     = 7
var REALM_SOMATIC    = 8
var REALM_DEMONIC    = 9
var MAX_REALM        = 9 //<-Update this when adding a new realm

//Integers representing item type values for spellbooks
var TV_NO_BOOK       = 0;
var TV_MIRACLES_BOOK = 90;
var TV_SORCERY_BOOK  = 91;
var TV_BOOK_REALM1   = 91;
var TV_NATURE_BOOK   = 92;
var TV_CHAOS_BOOK    = 93;
var TV_DEATH_BOOK    = 94;
var TV_BOOK_REALM2   = 94;
var TV_TAROT_BOOK    = 95;
var TV_CHARMS_BOOK   = 96;
var TV_SOMATIC_BOOK  = 97;
var TV_DEMONIC_BOOK  = 98;


//Integer presenting racial skills vs. Human Mage
var NO               = 0
var POOR             = 1
var WORSE            = 2
var SAME             = 3
var BETTER           = 4
var SUPER            = 5

var realmChoices =
{
  Warrior       :[ (CH_NONE)                , (CH_NONE) ],
  Mage          :[ (CH_ALL)                 , (CH_ALL) ],
  Priest        :[ (CH_MIRACLES | CH_DEATH) ,  CH_ALL - (CH_MIRACLES | CH_DEATH) ],
  Rogue         :[ (CH_ALL)                 , (CH_NONE) ],
  Hunter        :[ (CH_NATURE)              , (CH_TAROT | CH_CHARMS | CH_SOMATIC | CH_SORCERY) ],
  Paladin       :[ (CH_MIRACLES | CH_DEATH) , (CH_NONE) ],
  Spellblade    :[ (CH_CHARMS)              ,  CH_ALL - CH_CHARMS ],
  "Hell Knight" :[ (CH_DEMONIC)             , (CH_NONE) ],
  Mystic        :[ (CH_SOMATIC)             , (CH_NONE) ],
  Orphic        :[ (CH_NONE)                , (CH_NONE) ],
  "High Mage"   :[ (CH_ALL)                 , (CH_NONE) ],
  Druid         :[ (CH_NATURE)              , (CH_NONE) ],
  Warlock       :[ (CH_DEMONIC)             ,  CH_ALL - CH_DEMONIC ]
};

var realms =
{
  NoMagic: { item:0                , illiterate:"& Book~ of Absent Magic #"  , literate: "& Blank Spellbook~ #"   },
  Miracles:{ item:TV_MIRACLES_BOOK , illiterate:"& Book~ of Miracles #"      , literate: "& Book~ of Miracles #"  },
  Sorcery: { item:TV_SORCERY_BOOK  , illiterate:"& Book~ of Sorcery #"       , literate: "& Sorcery Spellbook~ #" },
  Nature:  { item:TV_NATURE_BOOK   , illiterate:"& Book~ of Nature Magic #"  , literate: "& Nature Spellbook~ #"  },
  Chaos:   { item:TV_CHAOS_BOOK    , illiterate:"& Book~ of the Abyss #"     , literate: "& Chaos Spellbook~ #"   },
  Death:   { item:TV_DEATH_BOOK    , illiterate:"& Book~ of Death Magic #"   , literate: "& Death Spellbook~ #"   },
  Tarot:   { item:TV_TAROT_BOOK    , illiterate:"& Book~ of Tarot Magic #"   , literate: "& Tarot Spellbook~ #"   },
  Charms:  { item:TV_CHARMS_BOOK   , illiterate:"& Book~ of Charms Magic#"   , literate: "& Charms Spellbooks~ #" },
  Somatic: { item:TV_SOMATIC_BOOK  , illiterate:"& Book~ of Somatic Magic #" , literate: "& Somatic Spellbook~ #" },
  Demonic: { item:TV_DEMONIC_BOOK  , illiterate:"& Book~ of the Damned #"    , literate: "& Demonic Spellbook~ #" }
};

indexObject( realms );

var realmTexts = 
{ //         "0123456789012345678901234567890123456789012345678901234\n"
  None      :"The elegance of null magic, reduces the universe to a\n" +
             "bare bones reality, void of any wonder or miracles.\n",

  Miracles  :"Miracle magic is good magic; relying heavily on healing\n" +
             "and protective spells but with few attacking spells. It\n" +
             "is mostly used for harming and banishing foul minions  \n" +
             "evil.",

  Sorcery   :"Sorcery is a meta realm, including utility spells such \n" +
             "as detection and identify, protection spells, spells   \n" +
             "for fleeing and spells to enhance your odds in combat. \n" +
             "It does not, however, have few direct attacking spells.\n",

  Nature    :"Nature magic provides the user with mastery of the     \n" +
             "elements. It contains spells for protection, detection,\n" + 
             "curing and offense. It also contains the only powerful \n" +
             "healing spell outside the Life Realm.",

  Chaos     :"Chaos magic contains the very essence of unmaking, and \n" +
             "the Chaos spells are the most destructives in nature.  \n" +
             "The caster can also call on the primal forces of Chaos \n" + 
             "to to induce mutations in his/her opponents and even in\n" +
             "themselves. Chaos has no protective spells.",

  Death     :"There is no fouler or more evil category of spells than\n" +
             "the necromantic spells of Death Magic. These spells are\n" +
             "relatively hard to learn, but at higher levels give the\n" +
             "caster power over living and the undead. Unfortunately,\n" +
             "the most powerful spells demand their casters' blood as\n" +
             "the focus, often hurting the caster in the process of  \n" +
             "casting.:",

  Tarot     :"Tarot magic has an admirable selection of teleportation\n" +
             "and summoning spells. Like the Tarot, a lot of spells  \n" +
             "have an unpredictable nature, you will not always get  \n" +
             "what you want, but you'll get strong magic.",

  Charms    :"More so than Sorcery, Arcane magic is a general purpose\n" +
             "realm of magic. It attempts to encompass all 'useful'  \n" +
             "spells from all realms, and almost succeeds, with the  \n" + 
             "probable exception of *Identify*. It is the downside of\n" +
             "Arcane magic: while Arcane does have all the necessary \n" +
             "'tool' spells for a dungeon delver, it has no hard-core\n" +
             "ultra-powerful high level spells. As a consequence, all\n" +
             "Arcane spellbooks can be bought in town.",

  Somatic   :"Somatic magic focuses solely on the caster, it offers  \n" +
             "speed, protection, shapeshifting and extreme control   \n" +
             "over the body. Experienced Somatics can walk through   \n" +
             "walss and journey through space and time.",

  Demonic   :"The Demonic realm focuses on demons, devils and fallen \n" +
             "angels. Some spells will require sweat, blood and some \n" +
             "of your sanity. The closer you get to hell, the easier \n" +
             "and the more rewarding these spells become."
};

indexObject( realmTexts );

var MAGIC = 0;
var DIVINE = 1;
var MENTAL = 2;

var NO = 0;
var POOR = 1;
var WORSE = 2;
var SAME = 3;
var BETTER = 4;
var SUPER = 5;

var spells = 
[
 /*Miracles */
 [
 [ 1 , 1 , 30, 4, "Detect Evil"        , {}                                 , "Detects all evil creatures that are nearby." ],
 [ 3 , 2 , 35, 4, "Cure Light Wounds"  , {heal: "2d10"}                     , "Cures you of $heal damage, and reduces bleeding." ],
 [ 4 , 3 , 35, 4, "Bless"              , {dur: "12+1d12"}                   , "Blesses you, giving you a +5 armour bonus and a +10 hit bonus." ],
 [ 5 , 5 , 35, 4, "Remove Fear"        , {}                                 , "Stops you from being afraid." ],
 [ 7 , 7 , 35, 4, "Call Light"         , {dam:"2d(PLEV/2)",rad:"PLEV/10+1"} , "Showers the area that you are in with True light" , ],
 [ 9 , 8 , 40, 4, "Detect Hidden"      , {}                                 , "Finds all traps, stairs and secret doors in your surrounding area." ],
 [ 12, 12, 40, 3, "Cure Medium Wounds" , {heal: "4d10"}                     , "Cures you of $heal damage, and reduces bleeding." ],
 [ 15, 14, 45, 3, "Satisfy Hunger"     , {}                                 , "Removes all hunger, leaving you comfortably full." ],

 [ 16, 16, 45, 4, "Remove Curse"         , {}                   , "Removes minor curses from carried objects, allowing them to be taken off." ],
 [ 17, 17, 50, 4, "Cure Poison"          , {}                   , "Removes all poison from your system." ],
 [ 18, 18, 50, 4, "Cure Critical Wounds" , {heal:"8d10"}        , "Heals $heal damage, and completely stops bleeding or stunning." ],
 [ 19, 19, 50, 4, "Sense Unseen"         , {dur:"24+1d24"}      , "Allows you to see invisible creatures for a short while." ],
 [ 20, 20, 50, 4, "Holy Orb"             , {dam:"3d6+PLEV+ORB"} , "Fires a damaging ball of holy fire." ],
 [ 23, 23, 50, 4, "Protection from Evil" , {dur:"1d25+3*PLEV"}  , "Puts up a barrier that has a chance of stopping attacks from evil creatures." ],
 [ 30, 30, 55, 5, "Healing"              , {heal:300}           , "Heals $heal damage and completely stops bleeding and stunning." ],
 [ 35, 70, 75, 5, "Glyph of Warding"     , {}                   , "Places a rune on the floor that many creatures will be unable to pass" ],

 [ 26, 30, 50, 75 , "Exorcism"               , {dam:"PLEV"}             , "Banishes demons and undead, and scares other evil creatures." ],
 [ 28, 25, 70, 150, "Dispel Curse"           , {}                       , "Removes more powerful curses from carried objects." ],
 [ 33, 33, 60, 75 , "Dispel Undead & Demons" , {dam:"PLEV*3"}           , "Triple strength banishment against undead and demonic creatures." ],
 [ 35, 35, 60, 75 , "Day of the Dove"        , {charm:"PLEV*2"}         , "Befriends all nearby monsters." ],
 [ 35, 35, 70, 75 , "Dispel Evil"            , {dam:"PLEV*4"}           , "Quadruple strength banishment against all evil creatures." ],
 [ 35, 55, 80, 115, "Banish"                 , {dam: 100}               , "Teleports away all nearby evil creatures to elsewhere on the level." ],
 [ 39, 40, 80, 125, "Holy Word"              , {dam:"PLEV*4",heal:1000} , "Banishes evil, heals $heal damage, removes bleeding, fear, poison and stunning." ],
 [ 46, 70, 80, 150, "Warding True"           , {}                       , "Surrounds you with Glyphs of Warding." ], 

 [ 9 , 9  , 50, 40 , "Heroism"              , {dur:"1d25+25",heal: 10} , "Gives you a +12 bonus to hit, 10 extra hit points and protection from fear." ],
 [ 25, 25 , 50, 50 , "Prayer"               , {dur:"1d48+48"}          , "Like Bless, except that it lasts longer." ],
 [ 35, 85 , 80, 115, "Bless Weapon"         , {}                       , "Makes a weapon blessed - and thus usable by priests. This also removes any curse on the weapon. It is dangerous to use this spell on artifacts, as it may damage them." ],
 [ 42, 100, 80, 225, "Restoration"          , {}                       , "Removes any drain or damage from any of your abilities, and removes any experience drain." ],
 [ 45, 90 , 80, 115, "Healing True"         , {heal:2000}              , "Heals $heal damage, all bleeding and all stunning." ],
 [ 48, 50 , 80, 100, "Holy Vision"          , {}                       , "Fully identifies an item" ],
 [ 49, 100, 80, 250, "Divine Intervention", , {heal:300, dam:"PLEV*4"} , "Banishes, stuns, confuses, scares, slows and paralyses monsters that surround you, as well as healing you and hasting you to allow you a retreat." ],
 [ 50, 100, 80, 250, "Holy Invulnerability" , {dur:"7+1d7"}            , "Makes you completely immune to damage for a short period of time." ], 
 ],
 /*Sorcery*/
 [
 [ 1, 1, 23, 4, "Detect Monsters"       , {}                                    , "Detects the presence of all monsters in the nearby area." ],
 [ 1, 2, 24, 4, "Phase Door"            , {range: 10}                           , "Teleports you to a random location within 10o'" ],
 [ 3, 3, 25, 1, "Detect Doors and Traps", {}                                    , "Reveals any doors, stairs and traps that are in the nearby area." ],
 [ 3, 3, 30, 1, "Light Area"            , {dam:"2d(PLEV/2)",rad:"(PLEV/10)+1"}  , "Creates a permanent light in your surrounding area, possibly damaging creatures." ],
 [ 4, 4, 30, 1,  "Confuse Monster"      , {strength: "(PLEV*3)/2"}              , "Confuses a nearby (non-undead) monster. " ],
 [ 5, 5, 35, 5, "Teleport"              , {range : "PLEV*5"}                    , "Teleports you to a random location on the level." ],
 [ 6, 5, 30, 4, "Sleep Monster"         , {}                                    , "Sends a nearby monster to sleep." ],
 [ 7, 7, 75, 9, "Recharging"            , {}                                    , "Recharges a wand, rod or staff." ],

 [ 9 , 7 , 75, 8 , "Magic Mapping"  , {}                       , "Reveals the layout of your nearby surroundings."],
 [ 10, 7 , 75, 8 , "Identify"       , {}                       , "Identifies the basic abilities of an item."],
 [ 11, 7 , 75, 7 , "Slow Monster"   , {}                       , "Slows down the movement and attacks of a nearby monster."],
 [ 13, 7 , 50, 6 , "Mass Sleep"     , {}                       , "Sends all nearby monsters to sleep."],
 [ 18, 12, 60, 8 , "Teleport Away"  , {}                       , "Teleports a nearby monster away from you, to a random place on the level."],
 [ 22, 12, 60, 8 , "Haste Self"     , {dur:"1d(20+PLEV)+PLEV"} , "Speeds your movement for a limited duration. "],
 [ 28, 20, 70, 15, "Detection True" , {}                       , "Detects secret doors, stairs, traps, treasure, objects and monsters (including invisible ones)."],
 [ 33, 30, 75, 20, "Identify True"  , {}                       , "Fully identifies an item."],

 [ 3,  3,  25, 15, "Detect Objects and Treasure" , {}                , "Reveals the location of all objects and money. "],
 [ 10, 10, 70, 40, "Detect Enchantment"          , {}                , "Reveals the location of magical items."],
 [ 10, 10, 80, 40, "Charm Monster"               , {}                , "Makes a nearby monster friendly."],
 [ 12, 12, 80, 40, "Dimension Door"              , {range:"PLEV+2"}  , "Teleports you to a location of your choice on the current level."],
 [ 14, 10, 60, 25, "Sense Minds"                 , {dur:"20+1d30"}   , "Gives you ESP for a short duration."],
 [ 20, 18, 85, 50, "Self Knowledge"              , {}                , "Reveals knowledge about yourself and your abilities (including those given by items)."],
 [ 20, 18, 60, 25, "Teleport Level"              , {}                , "Teleports you completely off the level - either to the level below or the level above."],
 [ 25, 25, 75, 19, "Word of Recall"              , {delay:"15+1d21"} , "Teleports you back to the town, or - if you are in the town - back to the deepest level you have explored."],

 [ 10, 10, 40, 20,   "Stasis",                   {}                    , "Freezes a monster in time, preventing it from taking any actions for a while."],
 [ 25, 25, 75, 70,   "Telekinesis",              {weight:"PLEV*15/10"} , "Picks up an object from a distance. "],
 [ 25, 30, 95, 160,  "Explosive Rune",           {dam:"7d7+PLEV/2"}    , "Leaves a rune on the floor that will explode when a creature walks over it."],
 [ 30, 40, 80, 120,  "Clairvoyance",             {dur:"25+1d30"}       , "Reveals the layout of the entire level, and gives you ESP for a short duration."],
 [ 40, 80, 95, 200,  "Enchant Weapon",           {}                    , "Increases the magical bonuses on your weapon."],
 [ 40, 100, 95, 200, "Enchant Armour",           {}                    , "Increases the magical bonuses on a piece of your armour."],
 [ 42, 50, 90, 175,  "Alchemy",                  {}                    , "Turns an item to gold, giving you approximately a third of the amount you would be able to get by selling it."],
 [ 45, 70, 75, 250,  "Globe of Invulnerability", {dur:"8+1d8"}         , "Makes you completely immune to damage for a short while."], 
 ],
 /*Nature*/
 [
 [ 1, 1, 23, 4, "Detect Creatures"      , {}                 , "Reveals the presence of all nearby creatures." ],
 [ 3, 3, 25, 3, "First Aid"             , {heal:"2d8"}       , "Cures you of 2d8 damage, and reduces your bleeding." ],
 [ 3, 3, 25, 1, "Detect Doors and Traps", {}                 , "Reveals the location of any nearby secret doors, traps or stairs." ],
 [ 4, 4, 35, 4, "Foraging"              , {}                 , "Produces enough food to fill you up." ],
 [ 4, 4, 50, 5, "Daylight"              , {dam:"2d(PLEV/2)"} , "Lights your immediate area with a permanent light, possibly damaging creatures." ],
 [ 4, 5, 50, 5, "Animal Taming"         , {charm:"PLEV"}     , "Makes an animal your friend." ],
 [ 5, 5, 50, 5, "Resist Environment"    , {dur:"20+1d20"}    , "Gives you a temporary resistance to cold, fire and electricity." ],
 [ 5, 5, 35, 4, "Cure Cuts & Poison"    , {}                 , "Removes all poison and bleeding." ],

 [ 5 , 5  , 40, 6,  "Stone to Mud"    , {}                         , "Turns a square of rock into loose mud, effectively removing it."],
 [ 5 , 5  , 30, 6,  "Lightning Bolt"  , {dam:"(3+((PLEV-5)/4))d8"} , "Fires a bolt of lightning doing electricity damage."],
 [ 7 , 6  , 45, 6,  "Nature Awareness", {}                         , "Reveals the layout of the local area, as well as finding secret doors, stairs and traps. Also reveals the location of any nearby monsters."],
 [ 7 , 6  , 40, 6,  "Frost Bolt"      , {dam:"(5+((PLEV-5)/4))d8"} , "Fires a bolt frost doing cold damage."],
 [ 9 , 6  , 30, 5,  "Ray of Sunlight" , {}                         , "Creates a beam of permanent light, possibly damaging creatures."],
 [ 19, 12 , 55, 8,  "Entangle"        , {}                         , "Causes vines to grow from the floor, slowing all nearby monsters."],
 [ 25, 25 , 90, 50, "Summon Animal"   , {}                         , "Summons an animal ally to help you."],
 [ 40, 100, 95, 50, "Herbal Healing"  , {heal: 1000}               , "Cures $heal damage, and removes bleeding, poison and stunning."],

 [ 7, 7, 20, 28,    "Door Building"         , {}               , "Makes a door to protect you from anything which might be following you."],
 [ 9, 12, 40, 44,   "Stair Building"        , {}               , "Creates a staircase at your feet for quick escape."],
 [ 10, 12, 75, 120, "Stone Skin"            , {dur:"1d20+30"}  , "Adds 50 to your armour class for a short while."],
 [ 15, 20, 85, 60,  "Resistance True"       , {dur:"1d20+20"}  , "Gives you a temporary resistance to acid, cold, fire, electricity and poison."],
 [ 30, 30, 90, 100, "Animal Friendship"     , {charm:"PLEV*2"} , "Makes all nearby animals your friends."],
 [ 37, 40, 90, 200, "Stone Tell"            , {}               , "Fully identifies an item."],
 [ 38, 45, 75, 200, "Wall of Stone"         , {}               , "Creates a wall of stone to protect you from anything which might be following you."],
 [ 40, 90, 90, 250, "Protect from Corrosion", {}               , "Permanently protects an item from acid damage."],

 [ 20, 18, 60, 25 , "Earthquake"        , {rad:10}                           , "Creates a large earthquake centered on your location, strong enough to collapse the roof. You are protected from the effects of this spell, safe in the epicenter."],
 [ 23, 23, 80, 50 , "Whirlwind Attack"  , {}                                 , "Allows you to attack all adjacent creatures in melee."],
 [ 25, 25, 75, 29 , "Blizzard"          , {dam:"70+PLEV",rad:"(PLEV/12)+1"}  , "Creates a ball of frost, dealing cold damage."],
 [ 30, 27, 75, 35 , "Lightning Storm"   , {dam:"90+PLEV",rad:"(PLEV/12)+1"}  , "Creates a ball of lightning, doing electricity damage."],
 [ 35, 30, 85, 65 , "Whirlpool"         , {dam:"100+PLEV",rad:"(PLEV/12)+1"} , "Creates a whirling ball of water, doing elemental water damage."],
 [ 37, 35, 90, 100, "Call Sunlight"     , {dam:150}                          , "Creates a ball of elemental light, and also lights the whole level permanently."],
 [ 40, 90, 95, 250, "Elemental Branding", {}                                 , "Permanently engulfs the striking surface of a weapon in an element, causing it to do more damage."],
 [ 40, 75, 65, 150, "Nature's Wrath"    , {dam:"4*PLEV+100+PLEV"}            , "Severely damages all nearby creatures and causes an earthquake."],
 ],
 /*Chaos*/
 [
 [ 1 , 1, 20, 4, "Magic Missile"          , {dam:"(3+(PLEV - 1)/5)d4"}               , "Creates a physical missile which strikes a nearby target."],
 [ 1 , 2, 22, 4, "Trap / Door Destruction", {}                                       , "Destroys any adjacent doors or traps."],
 [ 2 , 2, 25, 4, "Flash of Light"         , {dam:"(2d(PLEV / 2))d((PLEV / 10) + 1)"} , "Permanently lights the nearby area, possibly damaging creatures."],
 [ 5 , 5, 30, 1, "Touch of Confusion"     , {}                                       , "Creates an aura around your hands that will confuse the next creature you attack."],
 [ 9 , 6, 50, 1, "Mana Burst"             , {dam:"3d5+PLEV+ORB"}                     , "Fires a ball of raw magical (not elemental) energy."],
 [ 13, 9, 45, 6, "Fire Bolt"              , {dam:"(8+((PLEV-5)/4))d8"}               , "Fires a bolt of flame that does fire damage."],
 [ 14, 9, 45, 6, "Fist of Force"          , {dam:"(8+((PLEV-5)/4))d8"}               , "Fires a bolt of pure force, that can disintegrate what it touches."],
 [ 15, 9, 35, 5, "Teleport Self"          , {}                                       , "Teleports you to a random place on the level."],
  
 [ 17, 10, 25, 5 , "Wonder"             , {}                          , "Creates a random (but often useful) chaotic effect. The more powerful you are, the more useful the effect is likely to be."],
 [ 19, 12, 45, 9 , "Chaos Bolt"         , {dam:"(10+((PLEV-5)/4))d8"} , "Fires a bolt of raw chaos."],
 [ 21, 13, 45, 10, "Sonic Boom"         , {dam:"PLEV+45"}             , "Fires a ball of sound."],
 [ 23, 15, 50, 11, "Doom Bolt"          , {dam:"(11+((PLEV-5)/4))d8"} , "Fires a stream of raw magic."],
 [ 25, 16, 50, 12, "Fire Ball"          , {dam:"PLEV+55",rad:2}       , "Fires a ball of flame, doing fire damage."],
 [ 25, 18, 60, 8 , "Teleport Other"     , {}                          , "Teleports the targeted creature to somewhere random on the level."],
 [ 30, 20, 80, 15, "Word of Destruction", {rad:15}                    , "Causes the roof to collapse around you, much like the effects of an earthquake."],
 [ 35, 40, 85, 40, "Invoke Primal Chaos", {dam:"PLEV+66"}             , "Fires a ball of raw chaos."],
  
 [ 11, 7  , 45, 9  , "Polymorph Other" , {}                              , "Turns the target into a random creature."],
 [ 15, 15 , 80, 35 , "Chain Lightning" , {dam:"(5+(PLEV/10))d8"}         , "Fires bolts of lightning in eight directions, doing electricity damage."],
 [ 16, 14 , 80, 35 , "Arcane Binding"  , {}                              , "Recharges a wand, staff or rod."],
 [ 25, 25 , 85, 100, "Disintegrate"    , {dam:"PLEV+80",rad:"3+PLEV/40"} , "Fires a powerful beam of disintegrating energy."],
 [ 30, 25 , 85, 150, "Alter Reality"   , {}                              , "Completely alters reality, placing you in a different (but similar depth) dungeon level."],
 [ 42, 50 , 85, 250, "Polymorph Self"  , {}                              , "Invokes chaos into your own body, warping it in a random way."],
 [ 45, 90 , 80, 250, "Chaos Branding"  , {}                              , "Invokes chaos into your weapon, changing it."],
 [ 47, 100, 90, 250, "Summon Demon"    , {}                              , "Summons a friendly demon to help you."],
  
 [ 20, 20 , 66, 8  , "Beam of Gravity"     , {dam:"(9+((PLEV-5)/4))d8"}  , "Fires a beam of elemental gravity."],
 [ 35, 32 , 85, 35 , "Meteor Swarm"        , {dam:"(3*PLEV)/2",each:-1}  , "Makes a number of meteors appear and explode in your vicinity."],
 [ 37, 34 , 75, 40 , "Flame Strike"        , {dam:"150+2*PLEV"}          , "Fires an intense ball of flame, doing massive amounts of fire damage."],
 [ 41, 42 , 85, 100, "Call Primal Chaos"   , {dam:"75*1d2"}              , "Fires a random beam or ball of energy."],
 [ 43, 44 , 80, 150, "Shard Ball"          , {dam:"120+PLEV",rad:2}      , "Fires a ball of sharp shards of metal and stone."],
 [ 45, 48 , 85, 200, "Mana Storm"          , {dam:"300+PLEV*2",rad:4}    , "Fires a ball of extreme magical energy."],
 [ 47, 75 , 80, 200, "Breathe Primal Chaos", {dam:"CHP",rad:2}           , "Makes you breathe a huge gout of raw chaos."],
 [ 49, 100, 85, 250, "Call the Void"       , {dam:175,times:3}           , "Invokes an extremely destructive series of explosions around yourself that might be very dangerous in an enclosed area."],    
 ],
 /*Death*/
 [
 [ 1 , 1 , 25, 4, "Detect Unlife",  {}                           , "Reveals the location of any nearby undead or demonic creatures."],
 [ 2 , 2 , 25, 4, "Malediction",    {dam:"(3 + ((PLEV-1)/5))d3"} , "Fires a bolt of hell fire that may also scare, confuse or stun creatures."],
 [ 2 , 2 , 25, 4, "Detect Evil",    {}                           , "Reveals the location of any nearby evil creatures."],
 [ 3 , 3 , 27, 3, "Stinking Cloud", {dam:"10+(PLEV / 2)"}        , "Fires a ball of poison gas."],
 [ 5 , 5 , 30, 4, "Black Sleep",    {}                           , "Sends a nearby monster to sleep."],
 [ 7 , 10, 75, 6, "Resist Poison",  {dur:"1d20+20"}              , "Gives you a temporary resistance to poison."],
 [ 9 , 9 , 30, 4, "Horrify",        {scare:"PLEV"}               , "Scares and stuns a nearby monster."],
 [ 10, 10, 30, 4, "Enslave Undead", {charm:"PLEV"}               , "Makes an undead your friend."],
  
 [ 12, 12, 40, 5  , "Orb of Entropy",  {dam:"3d6+ORB"}                   , "Fires a life draining ball of energy."],
 [ 13, 12, 30, 4  , "Nether Bolt",     {dam:"(6+((PLEV-5)/4))d8"}        , "Fires a bolt of nether."],
 [ 18, 15, 50, 10 , "Terror",          {scare:"PLEV+30"}                 , "Scares away all nearby monsters."],
 [ 23, 20, 60, 16 , "Vampiric Drain",  {dam:"PLEV+1d(PLEV)*(PLEV/10+1)"} , "Drains the life from a nearby creature, both healing you and sating your hunger."],
 [ 30, 75, 50, 30 , "Poison Branding", {}                                , "Permanently coats your weapon with poison."],
 [ 33, 35, 60, 16 , "Dispel Good",     {dam:"PLEV*4"}                    , "Banishes good creatures."],
 [ 37, 25, 95, 25 , "Genocide",        {}                                , "Destroys all creatures of a chosen type on a level. Unique creatures are teleported off the level, rather than destroyed. You get no experience for monsters killed with Genocide."],
 [ 45, 50, 95, 150, "Restore Life",    {}                                , "Restores any drained experience you have."],
  
 [ 10, 20, 80, 180, "Berserk",           {dur:"25+1d25", heal:30}   , "Sends you berserk giving you 30 extra hit points, +24 to hit and -10 to armour class."],
 [ 10, 15, 80, 30 , "Invoke Spirits",    {}                         , "Has a random (but usually beneficial) effect. The higher level you are, the more likely it is to be useful."],
 [ 11, 11, 30, 15 , "Dark Bolt",         {dam:"(4+((PLEV-5)/4))d8"} , "Fires a bolt of darkness."],
 [ 30, 25, 75, 50 , "Battle Frenzy",     {dur:"1d25+25"}            , "Sends you berserk and also hastes you for a short while."],
 [ 33, 35, 60, 125, "Vampirism True",    {dam:100,heal:100}         , "Drains a large amount of life from a target and heals you by the amount drained."],
 [ 33, 90, 70, 90 , "Vampiric Branding", {}                         , "Permanently turns your weapon into a life-draining vampiric blade."],
 [ 40, 40, 70, 200, "Darkness Storm",    {dam:120,rad:4}            , "Fires a ball of darkness."],
 [ 40, 75, 80, 100, "Mass Genocide",     {}                         , "Destroys all non-unique creatures on the level. You get no experience for monsters killed with Mass Genocide."],
  
 [ 20, 20 , 75, 50 , "Death Ray"      , {dam:"PLEV"}                  , "Fires a ray that will kill almost any living creature."],
 [ 25, 66 , 95, 250, "Raise the Dead" , {}                            , "Creates undead servants to help you."],
 [ 30, 40 , 95, 250, "Esoteria"       , {}                            , "Identifies (with varying accuracy) an item you are carrying."],
 [ 33, 35 , 70, 40 , "Word of Death"  , {dam:"PLEV*3"}                , "Kills or damages all nearby living creatures."],
 [ 37, 35 , 80, 70 , "Evocation"      , {dam:"PLEV*4"}                , "Banishes, teleports away, and scares all nearby living creatures."],
 [ 42, 120, 95, 250, "Hellfire"       , {dam:666}                     , "Fires a huge ball of hell fire."],
 [ 45, 100, 90, 250, "Omnicide"       , {}                            , "Destroys all non-unique creatures on the level, absorbing their essence as spell points. You get no experience for monsters killed with Omnicide."],
 [ 47, 100, 90, 250, "Wraithform"     , {dur:"1d(PLEV/2)+1d(PLEV/2)"} ,  "Temporarily makes you intangible so that you can walk through walls."],
 ],
 /*Tarot*/
 [
 [ 1 , 1 , 50, 3, "Shift"              , "range: 10",  "Teleports you to a random nearby location."],
 [ 3 , 3 , 50, 4, "The Challenge"      , "dam: (3+((PLEV-1)/5))d3",  "Harms a nearby monster with psychic energy."],
 [ 5 , 5 , 75, 8, "Hopes & Fears"      , "random: -1",  "Invokes a random (but usually beneficial) effect from a Tarot card."],
 [ 6 , 6 , 80, 8, "Restack"            , {},  "Resets the depth that you will go to with Recall."],
 [ 7 , 7 , 40, 4, "Fool's Journey"     , "range: PLEV*4",  "Teleports you to a random location on the level."],
 [ 9 , 9 , 60, 6, "Sleight of Hand"    , "range: PLEV+2",  "Teleports you to a nearby location that you specify."],
 [ 14, 12, 60, 6, "The High Priestess" , "dur: 25+1d30",  "Gives you temporary ESP."],
 [ 17, 15, 60, 5, "The Chariot"        , "range: PLEV",  "Teleports a creature to a random place on the level."],
  
 [ 20, 20, 80, 8 , "The Wheel of Fortune"      , {weight:"PLEV*15/10"} , "Teleports a nearby object to your hand."],
 [ 23, 5 , 50, 5 , "Temperance"                , {dur:"20+1d20"}       , "Gives you a temporary resistance to cold, fire and electricity."],
 [ 28, 24, 60, 8 , "King of Swords"            , {}                    , "Summons a demon to serve you."],
 [ 30, 10, 80, 40, "The Lover"                 , {}                    , "Makes a nearby human friendly."],
 [ 33, 28, 80, 12, "Elements of The Minchiate" , {}                    , "Summons a fire elemental to help you"],
 [ 35, 30, 70, 10, "The Hermit"                , {}                    , "Teleports you off the level you are on, onto either the one above or the one below."],
 [ 40, 35, 80, 15, "Search for the Self"       , {delay:"15+1d21"}     , "Teleports you back to the town, or - if you are in the town - down to the deepest dungeon level that you have yet visited."],
 [ 42, 40, 70, 12, "Shuffle"                   , {}                    , "Teleports all nearby monsters to elsewhere on the level."],
  
 [ 15, 15 , 80, 20 , "Ink Blot"              , {}                , "Summons a bizarre creature to help you."],
 [ 24, 24 , 70, 25 , "The Star"              , {heal:150}        , "Heals 150 damage and completely stops bleeding or stunning."],
 [ 26, 26 , 70, 30 , "Fortitude"             , {dur:"PLEV"}      , "Blesses and also hastes you for a short while."],
 [ 30, 30 , 70, 35 ,"The Emperor"            , {charm:"PLEV*2"}  , "Makes a nearby monster friendly."],
 [ 35, 70 , 80, 100, "Mark of the Minchiate" , {}                , "Brands your weapon with fire, cold and electricity"],
 [ 40, 100, 90, 250, "Tarot Ascension"       , {}                , "Attunes you with the planes, altering your body."],
 [ 42, 50 , 50, 75 , "Death"                 , {dispel:"PLEV*3"} , "Banishes all nearby living creatures."],
 [ 45, 100, 90, 200, "The Devil"             , {}                , "Summons devils to help you."],
  
 [ 30, 30 , 60, 50 , "Read The Lay"            , {}               , "Detects secret doors, stairs, traps, treasure, objects and monsters (including invisible ones)."],
 [ 35, 50 , 90, 100, "The Magician"            , {}               , "Fully identifies an item."],
 [ 36, 80 , 80, 150, "Patter"                  , {dam:200,rad:5}  , "Confuse surrounding monsters."],
 [ 39, 80 , 80, 150, "The Tower"               , {dam: 200,rad:7} , "Surrounding monsters get hit with fire and lightning. Walls will crumble."],
 [ 42, 100, 80, 200, "Lay of The Celtic Cross" , {}               , "Summons multiple monsters to help you"],
 [ 47, 100, 80, 150, "Ten of Pentacles"        , {}               , "Summons Higher Demons to help you."],
 [ 48, 100, 80, 200, "The Traitor"             , {}               , "Summons Fallen Angels."],
 [ 49, 100, 80, 220, "Justice"                 , {dam :"PLEV*3"}  , "Banishes all evil, undead, demons, devils and fallen angels."],
 ],
 /*Charms*/
 [
 [ 1, 1, 20, 4, "Zap"                     , {dam:"(3 + ((PLEV-1)/5))d3"} , "Fires a bolt of electricity."],
 [ 1, 1, 33, 5, "Wizard Lock"             , {}                           , "Locks a nearby door."],
 [ 1, 1, 33, 4, "Detect Invisibility"     , {}                           , "Reveals the location of nearby invisible creatures."],
 [ 2, 1, 33, 5, "Detect Monsters"         , {}                           , "Reveals the location of nearby creatures."],
 [ 2, 2, 33, 5, "Blink"                   , {range:4}                    , "Teleports you to a random nearby location."],
 [ 4, 4, 40, 6, "Light Area"              , {dam:"2d(PLEV/2)"}           , "Permanently lights the nearby area, possibly damaging creatures."],
 [ 5, 5, 33, 7, "Trap & Door Destruction" , {}                           , "Destroys any adjacent doors or traps."],
 [ 6, 5, 44, 5, "Cure Light Wounds"       , {heal:"2d8"}                 , "Heals 2d8 damage and reduces bleeding."],
  
 [ 7 , 6 , 40, 7, "Detect Doors & Traps" , {},               "Detects all nearby secret doors, traps and stairs."],
 [ 8 , 8 , 60, 7, "Phlogiston"           , {},               "Provides extra fuel for a light source."],
 [ 9 , 8 , 50, 6, "Detect Treasure"      , {},               "Detects nearby money and seams of treasure."],
 [ 9 , 9 , 50, 6, "Detect Enchantment"   , {},               "Detects nearby magical items."],
 [ 9 , 9 , 50, 6, "Detect Objects"       , {},               "Reveals the location of nearby objects."],
 [ 11, 10, 50, 6, "Cure Poison"          , {},               "Removes all poison from your system."],
 [ 12, 12, 50, 5, "Resist Cold"          , {dur:"1d20+20"} , "Provides a temporary resistance to cold."],
 [ 13, 12, 50, 5, "Resist Fire"          , {dur:"1d20+20"} , "Provides a temporary resistance to fire."],
  
 [ 14, 12, 50, 5,    "Resist Lightning"   , {dur:"1d20+20"}  , "Provides a temporary resistance to electricity."],
 [ 15, 12, 50, 5,    "Resist Acid"        , {dur:"1d20+20"}  , "Provides a temporary resistance to acid."],
 [ 16, 14, 33, 6,    "Cure Medium Wounds" , {heal:"4d8"}     , "Cures 4d8 damage and severely reduces bleeding."],
 [ 18, 15, 50, 8,    "Teleport"           , {range:"PLEV*5"} , "Teleports you to a random location on the level."],
 [ 20, 16, 60, 9,    "Stone to Mud"       , {}               , "Turns a square of rock into loose mud, effectively removing it."],
 [ 23, 18, 60, 9,    "Ray of Light"       , {dam:"6d8"}      , "Creates a beam of permanent light, possibly damaging creatures."],
 [ 25, 20, 70, 12,   "Satisfy Hunger"     , {}               , "Removes all hunger, leaving you comfortably full."],
 [ 25, 20, 60, 13,   "See Invisible"      , {dur:"1d24+24"}  , "Allows you to see invisible creatures for a short while."],
  
 [ 28, 25, 70, 30,   "Recharging"     , {}                    , "Recharges a wand, staff or rod."],
 [ 35, 35, 80, 25,   "Teleport Level" , {}                    , "Teleports you off the level you are on, onto either the one above or the one below."],
 [ 38, 30, 60, 25,   "Identify"       , {}                    , "Identifies the basic abilities of an item."],
 [ 40, 30, 70, 25,   "Teleport Away"  , {range:"PLEV"}        , "Teleports a creature to a random place on the level."],
 [ 41, 30, 66, 30,   "Elemental Ball" , {dam:"75+PLEV",rad:2} , "Fires a ball of a random element (fire, cold, acid of electricity)."],
 [ 42, 30, 80, 40,   "Detection"      , {}                    , "Detects secret doors, stairs, traps, treasure, objects and monsters (including invisible ones)."],
 [ 45, 50, 70, 50,   "Word of Recall" , {delay:"15+1d21"}     , "Teleports you back to the town, or - if you are in the town - back to the deepest level you have explored."],
 [ 49, 100, 80, 200, "Clairvoyance"   , {dur:"1d30+25"}       , "Gives you temporary ESP."],  
 ],
 /*Somatic*/
 [
 [ 1, 1, 23, 4, "Cure Light Wounds"  ,   "heal: 2d10",  "Heals 2d10 damage and reduces bleeding."],
 [ 1, 2, 24, 4, "Shift"              ,      "range: 10",  "Teleports you to a random nearby location."],
 [ 3, 3, 25, 1, "Embrace Fear"       ,     {},  "Stops you being afraid."],
 [ 3, 3, 30, 1, "Bat's Sense"        ,     {},  "Reveals the layout of your nearby surroundings."],
 [ 4, 4, 30, 1, "Eagle's Vision"     ,    {},  "Detects secret doors, stairs, traps, treasure, objects and monsters (including invisible ones)."],
 [ 5, 5, 35, 5, "Mind Vision"        ,     "dur: 1d30+25",  "Gives you temporary ESP."],
 [ 6, 5, 30, 4, "Cure Medium Wounds" ,   "heal: 4d10",  "Cures 4d10 damage and severely reduces bleeding."],
 [ 7, 7, 75, 9, "Satisfy Hunger"     ,    {},  "Removes all hunger, leaving you comfortably full."],
  
 [ 9 , 7 , 75, 8 , "Burn Resistance"      , {dur:"1d20+20"}          , "Gives you temporary resistance to fire, acid and electricity."],
 [ 10, 7 , 75, 8 , "Detoxify"             , {}                       , "Removes all poison from your system."],
 [ 11, 7 , 75, 7 , "Cure Critical Wounds" , {heal:"8d10"}            , "Heals 8d10 damage, and completely stops bleeding or stunning."],
 [ 13, 7 , 50, 6 , "See Invisible"        , {dur:"1d24+24"}          , "Allows you to see invisible creatures for a short while."],
 [ 18, 12, 60, 8 , "Teleport"             , {range:"PLEV*3"}         , "Teleports you to a random location on the level."],
 [ 22, 12, 60, 8 , "Haste"                , {dur:"1d(PLEV+20)+PLEV"} , "Temporarily speeds up your movement."],
 [ 28, 20, 70, 15, "Healing"              , {heal:300}               , "Heals 300 damage and completely stops bleeding and stunning."],
 [ 33, 30, 75, 20, "Resist True"          , {dur:"1d25+25"}          , "Gives you a temporary resistance to acid, cold, fire, electricity and poison."],
  
 [ 3 , 3 , 25, 15, "Horrific Visage" , {scare:"PLEV"}    ,  "Scares and stuns a nearby monster."],
 [ 10, 10, 70, 40, "See Magic"       , {}                ,  "Detects nearby magical items."],
 [ 10, 10, 80, 40, "Stone Skin"      , {dur:"1d20+30"}   ,  "Adds 50 to your armour class for a short while."],
 [ 12, 12, 80, 40, "Move Body"       , {range:"PLEV+2"}  ,  "Teleports you to a nearby location that you specify."],
 [ 14, 10, 60, 25, "Corrupt Body"    , {}                ,  "Warps your body in a random manner."],
 [ 20, 18, 85, 50, "Know Self"       , {}                ,  "Reveals knowledge about yourself and your abilities (including those given by items)."],
 [ 20, 18, 60, 25, "Teleport Level"  , {}                ,  "Teleports you off the level you are on, onto either the one above or the one below."],
 [ 25, 25, 75, 19, "Word of Recall"  , {delay:"15+1d21"} ,  "Teleports you back to the town, or - if you are in the town - back to the deepest level you have explored."],
  
 [ 10, 10 , 40, 20 , "Heroism"         , {dur:"1d25+25",heal:10}       , "Makes you feel heroic, giving you a +12 bonus to hit and 10 extra hit points.Also removes all fear."],
 [ 25, 25 , 75, 70 , "Wraithform"      , {dur:"1d(PLEV/2)+1d(PLEV/2)"} , "Temporarily makes you intangible so that you can walk through walls."],
 [ 25, 30 , 95, 160, "Attune to Magic" , {}                            , "Fully identifies an item."],
 [ 30, 40 , 80, 120, "Restore Body"    , {}                            , "Heals damage to all six ability scores."],
 [ 40, 80 , 95, 200, "Healing True"    , {heal:2000}                   , "Heals 2000 damage, all bleeding and all stunning."],
 [ 40, 100, 95, 200, "Hypnotic Eyes"   , {charm:"PLEV"}                , "Makes a monster your friend."],
 [ 42, 50 , 90, 175, "Restore Soul"    , {}                            , "Restores any drained experience."],
 [ 45, 70 , 75, 250, "Invulnerability" , {dur:"1d7+7"}                 , "Makes you completely immune to damage for a short while."],
 ],
 /*Demonic*/
 [
 [ 1 , 3 , 25, 4 , "Unholy Strength"   , {dur:"1d25+25",dam:"(PLEV/10)*5+5"} , "+12 bonus to hit, 10 extra hit points, protection from fear at the cost of hitpoints"], 
 [ 1 , 1 , 30, 4 , "Sense Evil"        , {}                                  , "Detects all evil creatures that are nearby."],
 [ 2 , 2 , 20, 4 , "Scorch"            , {dam:"(3+(PLEV-1)/5)d4"}            , "Fires a bolt of flame that does fire damage."],
 [ 4 , 2 , 30, 4 , "Perilous Shadows"  , {dam:"(3+(PLEV-1)/5)d4"}            , "Fires a bolt of darkness that does darkness damage."],
 [ 12, 9 , 35, 5 , "Teleport"          , {range:75}                          , "Teleports you to a random place on the level."],
 [ 14, 9 , 45, 6 , "Disintegrate"      , {dam:"(8+((PLEV-5)/4))d8"}          , "Fires a bolt of pure force, that can disintegrate what it touches."],
 [ 25, 30, 95, 10, "Demonic Sigil"     , {dam:"7d7+PLEV/2"}                  , "Leaves a rune on the floor that will explode when a creature walks over it."],
 [ 25, 30, 95, 10, "Hecate's Radiance" , {charm:"(PLEV/10)+1"}               , "Lights up your surrounding, causing monster to be charmed, confused or affraid."],
  
 [10, 20 , 80, 15,  "Abaddon's Rage"       , {dur:"1d25+25"}            , "+ 5 AC, +22 to hit, 10 extra hit points, protection from fear"],   
 [15, 1  , 15, 15,  "Mind Leech"           , {}                         , "Converts all your mana into physical health"],
 [15, 1  , 15, 15,  "Blood Leech"          , {}                         , "Converts your health into mana, potentially leaving you with 1 hit point"],
 [35, 70 , 75, 5,   "Glyph of Warding"     , {}                         , "Places a rune on the floor that many creatures will be unable to pass"],
 [23, 23 , 50, 4,   "Protection from Evil" , {dur:"1d25+3*PLEV"}        , "Puts up a barrier around yourself that has a chance of stopping attacks from evil creatures."],
 [37, 100, 80, 150, "Summon Demons"        , {}                         , "Summons Higher Demons to help you."],
 [37, 100, 80, 150, "Summon the Fallen"    , {}                         , "Summons Fallen Angels to help you."],
 [28, 20 , 70, 15,  "Balm of the Cocytus"  , {heal:300,drain:-1,con:-1} , "Heals $heal damage and completely stops bleeding and stunning at the cost of your constitution."],
  
 [ 20, 18, 60, 25,   "Araqiel's Wrath"      , {rad:8}                 , "Creates a large earthquake centered on your location, strong enough to collapse the roof. You are protected from the effects of this spell, safe in the epicenter."],
 [ 42, 100, 80, 200, "Kokabiel's Call"      , {}                      , "Summons multiple spirits to help you."],                             
 [ 30, 40, 80, 120,  "Baraquiel's Guile"    , {}                      , "Detects and pseudo-id's all magical items on the floor"],
 [ 30, 25, 75, 50,   "Sariel's Ire"         , {dur:"1d50+25"}         , "+12 bonus to hit, 10 extra hit points, protection from fear while in a magical shell."],               
 [ 33, 35, 60, 125,  "Azazel's Rule"        , {}                      , "Charm all goats on the entire level"],
 [ 40, 40, 70, 200,  "Danel's Deluge"       , {dam:"3d6+PLEV+PLEV/3"} , "Radiate strong sunlight."],
 [ 40, 75, 80, 100,  "Amaros' Grief"        , {dam:"PLEV*4"}          , "Banish all Demons, Devils and Fallen Angels"],
 [ 40, 40, 80, 120,  "Teachings of Kasyade" , {}                      , "Light up the entire level"],    
  
 [ 12, 12,  40, 5,   "Orb of Impending Doom" , {dam:"3d6+ORB+PLEV"} , "Fires a damaging ball of hellfire."],     
 [ 15, 5,   50, 5,   "Temperance"            , {dur:"1d50+50"}      , "Resist fire and cold."],
 [ 46, 70,  80, 150, "True Warding"          , {}                   , "Surrounds you with Glyphs of Warding."],
 [ 30, 20,  80, 15,  "Word of Destruction"   , {rad:15}             , "Causes the roof to collapse around you, much like the effects of an earthquake."],
 [ 40, 100, 95, 200, "Gift of Malphas "      , {}                   , "Not done yet"],     
 [ 40, 100, 95, 200, "Kiss of Lillith"       , {charm:"PLEV*4"}     , "Quadriple charm attack on all monsters around you."],
 [ 40, 100, 95, 200, "Behemoth's Call"       , {rad:16}             , "Flood the dungeon with its items and monsters around you."],
 [ 47, 75,  80, 200, "Chaos Rift"            , {dam:"MHP",rad:2}    , "Creates a huge rift causing Primal Chaos damage."],
 ]
];




//Private Objects

var _yyy;          //xxx

//Initialization

function initxxx(){
}

//Public Functions

function doXXX(){}

//Private Functions

function doYYY(){}

