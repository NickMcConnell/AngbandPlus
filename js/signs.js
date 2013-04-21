/*********************************************************
* All sources in Hellbound are Copyright (c) 2008 Konijn *
* I Konijn, release all code and data under the terms of *
* of the GNU General Public License (version 2), as well *
* as under the traditional Angband license. Distribution *
* is allowed both under the terms of the GPL (version 2) *
* or under the terms of the traditional Angband license. *
*********************************************************/

//Global Objects

var signs;

//Private Objects

//Initialization

function initializeSigns(){

  signs = new Object();

  signs["Free"] = { 
    "strength" : 0,
    "intelligence" : 0,
    "wisdom" : 0,
    "dexterity" : 0,
    "constitution" : 0,
    "charisma" : 0,
    "disarming" : 0,
    "wizardry" : 0,
    "occlusion" : 0,
    "alchemy" : 0,
    "appraising" : 0,
    "stealth" : 0,
    "searching" : 0,
    "perception" : 0,
    "combat" : 0,
    "missiles" : 0,
    "hitdice" : 0,
    "experience_rate" : 0,
    "base_age" : 0,
    "mod_age" : 0,
    "base_height_male" : 0,
    "mod_height_weight" : 0,
    "base_weight_male" : 0,
    "mod_weight_male" : 0,
    "base_height_female" : 0,
    "mod_height_female" : 0,
    "base_weight_female" : 0,
    "mod_weight_female" : 0,
    "infravision" : 0,
    "class_choices" : 0x1FFF,
    "eats" : true,
    "undead" : false,
    "fearless" : false,
    "hates_light" : false
  };

  signs["Draco"] = { 
    "strength" : 2,
    "intelligence" : 1,
    "wisdom" : 1,
    "dexterity" : 1,
    "constitution" : 2,
    "charisma" : -3,
    "disarming" : -2,
    "wizardry" : 4,
    "occlusion" : 4,
    "alchemy" : 4,
    "appraising" : 3,
    "stealth" : 0,
    "searching" : 1,
    "perception" : 10,
    "combat" : 5,
    "missiles" : 5,
    "hitdice" : 0,
    "experience_rate" : 150,
    "base_age" : 75,
    "mod_age" : 33,
    "base_height_male" : 0,
    "mod_height_weight" : 0,
    "base_weight_male" : 0,
    "mod_weight_male" : 0,
    "base_height_female" : 0,
    "mod_height_female" : 0,
    "base_weight_female" : 0,
    "mod_weight_female" : 0,
    "infravision" : 2,
    "class_choices" : 0x1FFF,
    "eats" : true,
    "undead" : false,
    "fearless" : false,
    "hates_light" : false,
    "free_fall" : true,
    "resist_fire" : 5,
    "resist_cold" : 10,
    "resist_acid" : 15,
    "resist_elec" : 20,
    "resist_poison" : 25
  };

  signs["Serpens"] = { 
    "strength" : 0,
    "intelligence" : 0,
    "wisdom" : 0,
    "dexterity" : 0,
    "constitution" : 0,
    "charisma" : 0,
    "disarming" : 0,
    "wizardry" : 0,
    "occlusion" : 0,
    "alchemy" : 0,
    "appraising" : 0,
    "stealth" : 10,
    "searching" : 0,
    "perception" : 10,
    "combat" : 0,
    "missiles" : 0,
    "hitdice" : 0,
    "experience_rate" : 0,
    "base_age" : 0,
    "mod_age" : 0,
    "base_height_male" : 0,
    "mod_height_weight" : 0,
    "base_weight_male" : 0,
    "mod_weight_male" : 0,
    "base_height_female" : 0,
    "mod_height_female" : 0,
    "base_weight_female" : 0,
    "mod_weight_female" : 0,
    "infravision" : 0,
    "class_choices" : 0x1FFF,
    "eats" : true,
    "undead" : false,
    "fearless" : false,
    "hates_light" : false,
    "resist_poison" : true
  };

  signs["Plutus"] = { 
    "strength" : 0,
    "intelligence" : 0,
    "wisdom" : 0,
    "dexterity" : 0,
    "constitution" : 0,
    "charisma" : 0,
    "disarming" : 0,
    "wizardry" : 5,
    "occlusion" : 5,
    "alchemy" : 5,
    "appraising" : 5,
    "stealth" : 0,
    "searching" : 0,
    "perception" : 10,
    "combat" : 0,
    "missiles" : 0,
    "hitdice" : 0,
    "experience_rate" : 0,
    "base_age" : 0,
    "mod_age" : 0,
    "base_height_male" : 0,
    "mod_height_weight" : 0,
    "base_weight_male" : 0,
    "mod_weight_male" : 0,
    "base_height_female" : 0,
    "mod_height_female" : 0,
    "base_weight_female" : 0,
    "mod_weight_female" : 0,
    "infravision" : 0,
    "class_choices" : 0x1FFF,
    "eats" : true,
    "undead" : false,
    "fearless" : false,
    "hates_light" : false,
    "resist_drain" : true
  };

  signs["Morui"] = { 
    "strength" : 2,
    "intelligence" : -1,
    "wisdom" : -1,
    "dexterity" : 1,
    "constitution" : 2,
    "charisma" : -2,
    "disarming" : 10,
    "wizardry" : 5,
    "occlusion" : 5,
    "alchemy" : 5,
    "appraising" : 0,
    "stealth" : 0,
    "searching" : -1,
    "perception" : 10,
    "combat" : 5,
    "missiles" : 5,
    "hitdice" : 2,
    "experience_rate" : 35,
    "base_age" : 6,
    "mod_age" : 0,
    "base_height_male" : 0,
    "mod_height_weight" : 0,
    "base_weight_male" : 0,
    "mod_weight_male" : 0,
    "base_height_female" : 0,
    "mod_height_female" : 0,
    "base_weight_female" : 0,
    "mod_weight_female" : 0,
    "infravision" : 2,
    "class_choices" : 0x1FFF,
    "eats" : true,
    "undead" : false,
    "fearless" : false,
    "hates_light" : false,
    "resist_confusion" : true,
    "resist_acid" : true
  };
}

//Public Functions



//Private Functions

