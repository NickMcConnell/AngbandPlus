/*********************************************************
* All sources in Hellbound are Copyright (c) 2008 Konijn *
* I Konijn, release all code and data under the terms of *
* of the GNU General Public License (version 2), as well *
* as under the traditional Angband license. Distribution *
* is allowed both under the terms of the GPL (version 2) *
* or under the terms of the traditional Angband license. *
*********************************************************/

//Global Objects

var gizmos = {}; //This will be attached to the player
gizmos.tester = {};
gizmos.tester.type = 0;
gizmos.tester.full = false;
gizmos.tester.hook = null;


gizmos.tester.ok = function( gizmo )
{
	/* Hack -- allow listing empty slots */
	if( gizmos.tester.full )
		return true;

	/* Require an item */
	if( gizmo.type === undefined )
		return false;

	/* Hack -- ignore "gold" */
	if( gizmo.type == GOLD )
		return false;

	/* Check the tval */
	if (gizmo.tester.type != 0 )
	{
		/* Is it a spellbook? If so, we need a hack -- TY */
		if (gizmo.tester.type <= BOOK_DEMONIC && gizmo.tester.type >=TV_MIRACLES_BOOK )
			return ( gizmo.realm !== undefined && ( realmTexts[gizmo.realm] == player.realm1 || realmTexts[gizmo.realm] == player.realm2 ) )
		else
			if( item.tester.type != gizmo.type )
				return false;
	}

	if (item.tester.hook != null) //Check the hook
		return item.tester.hook( gizmo );

	return true; //Assume okay
}






//All object types or in legacy speak 'tvals'
var NOTHING =  0;
var FOOD =  1;
var BLADE =  2;
var HAFTED =  3;
var POLEARM =  4;
var LAUNCHER =  5;
var ARROW =  6;
var BOLT =  7;
var SHOT =  8;
var DIGGER =  9;
var BOOTS =  10;
var HELM =  11;
var CROWN =  12;
var SOFT_ARMOR =  13;
var HARD_ARMOR =  14;
var SHIELD =  15;
var RING =  16;
var AMULET =  17;
var SCROLL =  18;
var POTION =  19;
var WAND =  20;
var STAFF =  21;
var BOOK_MIRACLES =  22;
var BOOK_SORCERY =  23;
var BOOK_NATURE =  24;
var BOOK_CHAOS =  25;
var BOOK_DEATH =  26;
var BOOK_TAROT =  27;
var BOOK_CHARMS =  28;
var BOOK_SOMATIC =  29;
var BOOK_DEMONIC =  30;
var CHEST =  31;
var LITE =  32;
var SPIKE =  33;
var FLASK =  34;
var BOTTLE =  35;
var ROD =  36;
var JUNK =  37;
var SKELETON =  38;
var DRAGON_ARMOR =  39;
var GOLD =  40;
var GLOVES =  41;
var CLOAK = 42;
var SHROOM = 43;

//Easy know or not ?
var EASY = 0;
var HARD = 1;

//Description modifiers
var ACT_NOTHING     = 0;
var ACT_SHOW_WEAPON = 1;
var ACT_SHOW_ARMOUR = 2;
var ACT_FULL_MONTY  = 4;

var NO_FLAVORS = 0;

//Flavor arrays for each type of flavored object

/*
* Syllables for scrolls (must be 1-4 letters each)
*/

var scroll_syllables = [
		"a", "ab", "ag", "aks", "ala", "an", "ankh", "app",
		"arg", "arze", "ash", "aus", "ban", "bar", "bat", "bek",
		"bie", "bin", "bit", "bjor", "blu", "bot", "bu",
		"byt", "comp", "con", "cos", "cre", "dalf", "dan",
		"den", "der", "doe", "dok", "eep", "el", "eng", "er", "ere", "erk",
		"esh", "evs", "fa", "fid", "flit", "for", "fri", "fu", "gan",
		"gar", "glen", "gop", "gre", "ha", "he", "hyd", "i",
		"ing", "ion", "ip", "ish", "it", "ite", "iv", "jo",
		"kho", "kli", "klis", "la", "lech", "man", "mar",
		"me", "mi", "mic", "mik", "mon", "mung", "mur", "nag", "nej",
		"nelg", "nep", "ner", "nes", "nis", "nih", "nin", "o",
		"od", "ood", "org", "orn", "ox", "oxy", "pay", "pet",
		"ple", "plu", "po", "pot", "prok", "re", "rea", "rhov",
		"ri", "ro", "rog", "rok", "rol", "sa", "san", "sat",
		"see", "sef", "seh", "shu", "ski", "sna", "sne", "snik",
		"sno", "so", "sol", "sri", "sta", "sun", "ta", "tab",
		"tem", "ther", "ti", "tox", "trol", "tue", "turs", "u",
		"ulk", "um", "un", "uni", "ur", "val", "viv", "vly",
		"vom", "wah", "wed", "werg", "wex", "whon", "wun", "x",
		"yerg", "yp", "zun", "tri", "blaa", "jah", "bul", "on",
		"foo", "ju", "xuxu" ];

var scroll_adj = [];

//More docs in stuff.txt for flavour history and research

var ring_adj   = ["Skull","Bone","Bloodstone","Chevron","Reeve's","Signet","Poisoning","Onyx","Obsidian","Black Marble","Black Pearl","Jasper","Ruby","Prosperity","Sun","Crusader","Inquisitor","Imperial","Byzantine","Ancient","Celtic","Etruscan","Goth","Umbrian","Double banded","Triple banded","Gimmal","Iron","Steel","Copper","White Gold","Bronze","Gold","Silver","Regards","Posie","Chastity","Claddaigh","Mage's","Wizard's","Witches'","Grandmaster's","Sorcerer's","Orphean","Spikard","Serpentine","Double","Dragon's","Plain","Rusted","Tarnished","Dull","Blotted","Stained","Bishop's","Cardinal's","Papal"];
var amulet_adj = ["Amber","Driftwood","Coral","Agate","Ivory","Obsidian","Bone","Brass","Bronze","Pewter","Tortoise Shell","Golden","Azure","Crystal","Silver","Copper","Swastika","Dragon Tooth","Dragon Claw"];
var staff_adj  = ["Aspen","Fir","Cercis","Birch","Cedar","Willow","Cypress","Cornel","Elm","Beech","Poplar","Hickory","Sandal Wood","Locust","Ash","Maple","Mulberry","Oak","Pine","Myrtle","Rosewood","Spruce","Fig Tree","Teak","Walnut","Mistletoe","Hawthorn","Chestnut","Tamarisk","Wormwood","Olive Tree","Juniper","Gnarled","Ivory"];
var wand_adj   = ["Triple Plated","Cast Iron","Double Plated","Copper","Gold","Iron","Pointy","Master","Nickel","Rusty","Silver","Steel","Tin","Femur","Scheelite","Black","Zinc","Aluminum-Plated","Copper-Plated","Gold-Plated","Nickel-Plated","Silver-Plated","Steel-Plated","Tin-Plated","Zinc-Plated","Dragon Tooth","Unicorn","Runed","Bronze","Brass","Crystal","Lead","Lead-Plated","Ivory","Bejeweled","Wizard's","Long","Short","Hexagonal"];
var rod_adj    = ["Triple Plated","Cast Iron","Double Plated","Copper","Gold","Iron","Pointy","Master","Nickel","Rusty","Silver","Steel","Tin","Femur","Scheelite","Black","Zinc","Aluminum-Plated","Copper-Plated","Gold-Plated","Nickel-Plated","Silver-Plated","Steel-Plated","Tin-Plated","Zinc-Plated","Dragon Tooth","Unicorn","Runed","Bronze","Brass","Crystal","Lead","Lead-Plated","Ivory","Bejeweled","Wizard's","Long","Short","Hexagonal"];
var shroom_adj = ["Blue","Black","Black Spotted","Brown","Dark Blue","Dark Green","Dark Red","Yellow","Furry","Green","Grey","Light Blue","Light Green","Violet","Red","Slimy","Tan","White","White Spotted","Wrinkled",];
var potion_adj = ["Clear","Light Brown","Icky Green","Burgundy","Azure","Blue","Blue Speckled","Black","Brown","Brown Speckled","Bubbling","Chartreuse","Cloudy","Copper Speckled","Crimson","Cyan","Dark Blue","Dark Green","Dark Red","Gold Speckled","Green","Green Speckled","Grey","Grey Speckled","Hazy","Indigo","Light Blue","Light Green","Magenta","Metallic Blue","Metallic Red","Metallic Green","Metallic Purple","Misty","Orange","Orange Speckled","Pink","Pink Speckled","Puce","Purple","Purple Speckled","Red","Red Speckled","Silver Speckled","Smoky","Tangerine","Violet","Vermilion","White","Yellow","Violet Speckled","Pungent","Clotted Red","Viscous Pink","Oily Yellow","Gloopy Green","Shimmering","Coagulated Crimson","Yellow Speckled","Gold","Manly","Stinking","Oily Black","Ichor","Ivory White","Sky Blue"];

//Types of traps for chests
var CHEST_LOSE_STR = 0x01 
var CHEST_LOSE_CON = 0x02 
var CHEST_POISON   = 0x04
var CHEST_PARALYZE = 0x08
var CHEST_EXPLODE  = 0x10
var CHEST_SUMMON   = 0x20

//Types of identifying flags
var IDENT_SENSE  = 0x01;  // Item has been "sensed"
var IDENT_FIXED  = 0x02;  // Item has been "haggled"
var IDENT_EMPTY  = 0x04;  // Item charges are known
var IDENT_KNOWN  = 0x08;  // Item abilities are known
var IDENT_STOREB = 0x10;  // Item is storebought !!!!
var IDENT_MENTAL = 0x20;  // Item information is known
var IDENT_CURSED = 0x40;  // Item is temporarily cursed
var IDENT_BROKEN = 0x80;  // Item is permanently worthless

var NONE = 0;
var BOW = 1;
var WIELD = 2;
var FEET = 3;
var HANDS = 4;
var OUTER = 5;
var HEAD = 6;
var ARM = 7;
var BODY = 8;
var LITE = 9;
var NECK = 10;
var FINGER = 11;
var POUCH = 12;

var itemTypes = [
	{ type: SKELETON      ,slot: NONE   , easyKnow: EASY , color: TERM_WHITE   , action: ACT_NOTHING     , flavors: NO_FLAVORS , plain: NO_FLAVORS    , full: NO_FLAVORS               },
	{ type: BOTTLE        ,slot: NONE   , easyKnow: EASY , color: TERM_WHITE   , action: ACT_NOTHING     , flavors: NO_FLAVORS , plain: NO_FLAVORS    , full: NO_FLAVORS               },
	{ type: JUNK          ,slot: NONE   , easyKnow: EASY , color: TERM_WHITE   , action: ACT_NOTHING     , flavors: NO_FLAVORS , plain: NO_FLAVORS    , full: NO_FLAVORS               },
	{ type: SPIKE         ,slot: NONE   , easyKnow: EASY , color: TERM_SLATE   , action: ACT_NOTHING     , flavors: NO_FLAVORS , plain: NO_FLAVORS    , full: NO_FLAVORS               },
	{ type: FLASK         ,slot: NONE   , easyKnow: EASY , color: TERM_YELLOW  , action: ACT_NOTHING     , flavors: NO_FLAVORS , plain: NO_FLAVORS    , full: NO_FLAVORS               },
	{ type: CHEST         ,slot: NONE   , easyKnow: HARD , color: TERM_SLATE   , action: ACT_NOTHING     , flavors: NO_FLAVORS , plain: NO_FLAVORS    , full: NO_FLAVORS               },
	{ type: SHOT          ,slot: NONE   , easyKnow: HARD , color: TERM_UMBER   , action: ACT_SHOW_WEAPON , flavors: NO_FLAVORS , plain: NO_FLAVORS    , full: NO_FLAVORS               },
	{ type: BOLT          ,slot: NONE   , easyKnow: HARD , color: TERM_UMBER   , action: ACT_SHOW_WEAPON , flavors: NO_FLAVORS , plain: NO_FLAVORS    , full: NO_FLAVORS               },
	{ type: ARROW         ,slot: NONE   , easyKnow: HARD , color: TERM_UMBER   , action: ACT_SHOW_WEAPON , flavors: NO_FLAVORS , plain: NO_FLAVORS    , full: NO_FLAVORS               },
	{ type: LAUNCHER      ,slot: BOW    , easyKnow: HARD , color: TERM_UMBER   , action: ACT_SHOW_WEAPON , flavors: NO_FLAVORS , plain: NO_FLAVORS    , full: NO_FLAVORS               },
	{ type: HAFTED        ,slot: WIELD  , easyKnow: HARD , color: TERM_L_WHITE , action: ACT_SHOW_WEAPON , flavors: NO_FLAVORS , plain: NO_FLAVORS    , full: NO_FLAVORS               },
	{ type: POLEARM       ,slot: WIELD  , easyKnow: HARD , color: TERM_L_WHITE , action: ACT_SHOW_WEAPON , flavors: NO_FLAVORS , plain: NO_FLAVORS    , full: NO_FLAVORS               },
	{ type: BLADE         ,slot: WIELD  , easyKnow: HARD , color: TERM_L_WHITE , action: ACT_SHOW_WEAPON , flavors: NO_FLAVORS , plain: NO_FLAVORS    , full: NO_FLAVORS               },
	{ type: DIGGER        ,slot: WIELD  , easyKnow: HARD , color: TERM_SLATE   , action: ACT_SHOW_WEAPON , flavors: NO_FLAVORS , plain: NO_FLAVORS    , full: NO_FLAVORS               },
	{ type: BOOTS         ,slot: FEET   , easyKnow: HARD , color: TERM_L_UMBER , action: ACT_SHOW_ARMOUR , flavors: NO_FLAVORS , plain: NO_FLAVORS    , full: NO_FLAVORS               },
	{ type: GLOVES        ,slot: HANDS  , easyKnow: HARD , color: TERM_L_UMBER , action: ACT_SHOW_ARMOUR , flavors: NO_FLAVORS , plain: NO_FLAVORS    , full: NO_FLAVORS               },
	{ type: CLOAK         ,slot: OUTER  , easyKnow: HARD , color: TERM_L_UMBER , action: ACT_SHOW_ARMOUR , flavors: NO_FLAVORS , plain: NO_FLAVORS    , full: NO_FLAVORS               },
	{ type: CROWN         ,slot: HEAD   , easyKnow: HARD , color: TERM_L_UMBER , action: ACT_SHOW_ARMOUR , flavors: NO_FLAVORS , plain: NO_FLAVORS    , full: NO_FLAVORS               },
	{ type: HELM          ,slot: HEAD   , easyKnow: HARD , color: TERM_L_UMBER , action: ACT_SHOW_ARMOUR , flavors: NO_FLAVORS , plain: NO_FLAVORS    , full: NO_FLAVORS               },
	{ type: SHIELD        ,slot: ARM    , easyKnow: HARD , color: TERM_L_UMBER , action: ACT_SHOW_ARMOUR , flavors: NO_FLAVORS , plain: NO_FLAVORS    , full: NO_FLAVORS               },
	{ type: SOFT_ARMOR    ,slot: BODY   , easyKnow: HARD , color: TERM_SLATE   , action: ACT_SHOW_ARMOUR , flavors: NO_FLAVORS , plain: NO_FLAVORS    , full: NO_FLAVORS               },
	{ type: HARD_ARMOR    ,slot: BODY   , easyKnow: HARD , color: TERM_SLATE   , action: ACT_SHOW_ARMOUR , flavors: NO_FLAVORS , plain: NO_FLAVORS    , full: NO_FLAVORS               },
	{ type: DRAGON_ARMOR  ,slot: BODY   , easyKnow: HARD , color: TERM_SLATE   , action: ACT_SHOW_ARMOUR , flavors: NO_FLAVORS , plain: NO_FLAVORS    , full: NO_FLAVORS               },
	{ type: LITE          ,slot: LITE   , easyKnow: HARD , color: TERM_YELLOW  , action: ACT_NOTHING     , flavors: NO_FLAVORS , plain: NO_FLAVORS    , full: NO_FLAVORS               },
	{ type: AMULET        ,slot: NECK   , easyKnow: HARD , color: TERM_ORANGE  , action: ACT_FULL_MONTY  , flavors: amulet_adj , plain: "  & Amulet~"   , full: "  & # Amulet~"            },
	{ type: RING          ,slot: FINGER , easyKnow: HARD , color: TERM_ORANGE  , action: ACT_FULL_MONTY  , flavors: ring_adj   , plain: "  & Ring~"     , full: "  & # Ring~"              },
	{ type: STAFF         ,slot: NONE   , easyKnow: EASY , color: TERM_UMBER   , action: ACT_FULL_MONTY  , flavors: staff_adj  , plain: "  & Staff~"    , full: "  & # Staff~"             },
	{ type: WAND          ,slot: POUCH  , easyKnow: EASY , color: TERM_GREEN   , action: ACT_FULL_MONTY  , flavors: wand_adj   , plain: "  & Wand~"     , full: "  & # Wand~"              },
	{ type: ROD           ,slot: NONE   , easyKnow: EASY , color: TERM_VIOLET  , action: ACT_FULL_MONTY  , flavors: rod_adj    , plain: "  & Rod~"      , full: "  & # Rod~"               },
	{ type: SCROLL        ,slot: POUCH  , easyKnow: EASY , color: TERM_WHITE   , action: ACT_FULL_MONTY  , flavors: scroll_adj , plain: "  & Scroll~"   , full: "  & Scroll~ titled \"#\"" },
	{ type: POTION        ,slot: POUCH  , easyKnow: EASY , color: TERM_BLUE    , action: ACT_FULL_MONTY  , flavors: potion_adj , plain: "  & Potion~"   , full: "  & # Potion~"            },
	{ type: FOOD          ,slot: NONE   , easyKnow: EASY , color: TERM_L_UMBER , action: ACT_FULL_MONTY  , flavors: NO_FLAVORS , plain:  NO_FLAVORS    , full: NO_FLAVORS                  },
	{ type: SHROOM        ,slot: NONE   , easyKnow: EASY , color: TERM_L_UMBER , action: ACT_FULL_MONTY  , flavors: shroom_adj , plain: "& Mushroom~" , full: "& # Mushroom~"          },
	{ type: BOOK_MIRACLES ,slot: NONE   , easyKnow: EASY , color: TERM_WHITE   , action: ACT_NOTHING     , flavors: NO_FLAVORS , plain: NO_FLAVORS    , full: NO_FLAVORS               },
	{ type: BOOK_SORCERY  ,slot: NONE   , easyKnow: EASY , color: TERM_L_BLUE  , action: ACT_NOTHING     , flavors: NO_FLAVORS , plain: NO_FLAVORS    , full: NO_FLAVORS               },
	{ type: BOOK_NATURE   ,slot: NONE   , easyKnow: EASY , color: TERM_L_GREEN , action: ACT_NOTHING     , flavors: NO_FLAVORS , plain: NO_FLAVORS    , full: NO_FLAVORS               },
	{ type: BOOK_CHAOS    ,slot: NONE   , easyKnow: EASY , color: TERM_L_RED   , action: ACT_NOTHING     , flavors: NO_FLAVORS , plain: NO_FLAVORS    , full: NO_FLAVORS               },
	{ type: BOOK_DEATH    ,slot: NONE   , easyKnow: EASY , color: TERM_L_DARK  , action: ACT_NOTHING     , flavors: NO_FLAVORS , plain: NO_FLAVORS    , full: NO_FLAVORS               },
	{ type: BOOK_TAROT    ,slot: NONE   , easyKnow: EASY , color: TERM_ORANGE  , action: ACT_NOTHING     , flavors: NO_FLAVORS , plain: NO_FLAVORS    , full: NO_FLAVORS               },
	{ type: BOOK_CHARMS   ,slot: NONE   , easyKnow: EASY , color: TERM_L_WHITE , action: ACT_NOTHING     , flavors: NO_FLAVORS , plain: NO_FLAVORS    , full: NO_FLAVORS               },
	{ type: BOOK_SOMATIC  ,slot: NONE   , easyKnow: EASY , color: TERM_YELLOW  , action: ACT_NOTHING     , flavors: NO_FLAVORS , plain: NO_FLAVORS    , full: NO_FLAVORS               },
	{ type: BOOK_DEMONIC  ,slot: NONE   , easyKnow: EASY , color: TERM_RED     , action: ACT_NOTHING     , flavors: NO_FLAVORS , plain: NO_FLAVORS    , full: NO_FLAVORS               },
];

itemTypes.getType = function( type ){

	for( var key in this )
		if( this[key].type == type )
			return this[key];
	return NOTHING;
}

itemTypes.getKey = function( type ){

	for( var key in this )
		if( this[key].type == type )
			return key;
	return 0;
}

//Private Objects

function initScrollFlavors()
{
	var syllableCount = scroll_syllables.length;
	for( var i = 0 ; i <= 52 ; i++ )
	{
		var name = ""
		var unique = false;
		while( !unique )
		{
			while( name.length < 15 )
				name = name + scroll_syllables[ rng.base0.random( syllableCount ) ];
			
			unique = ( undefined === scroll_adj[name] );

			if( unique )
				scroll_adj[name] = scroll_adj[i] = name;
			else
				name = "";	
		}
	}
}



//Initialization

//no initialization



//Public Functions


//Private Functions

