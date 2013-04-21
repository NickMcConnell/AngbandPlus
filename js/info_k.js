/*********************************************************
* All sources in Hellbound are Copyright (c) 2008 Konijn *
* I Konijn, release all code and data under the terms of *
* of the GNU General Public License (version 2), as well *
* as under the traditional Angband license. Distribution *
* is allowed both under the terms of the GPL (version 2) *
* or under the terms of the traditional Angband license. *
*********************************************************/


function initGizmos(){

  gizmos.prototypes = new Array();

  gizmos.prototypes["& Something"] = { 
    "symbol" : "&",
    "color" : "W"
  };

  gizmos.prototypes[", Poison"] = { 
    "symbol" : ",",
    "color" : "d",
    "type" : SHROOM,
    "subtype" : 0,
    "pval" : 500,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 1,
    "cost" : 0,
    "rarity1" : "common5",
    "armor" : 0,
    "damage" : "4d4",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes[", Blindness"] = { 
    "symbol" : ",",
    "color" : "d",
    "type" : SHROOM,
    "subtype" : 1,
    "pval" : 500,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 1,
    "cost" : 0,
    "rarity1" : "common5"
  };

  gizmos.prototypes[", Paranoia"] = { 
    "symbol" : ",",
    "color" : "d",
    "type" : SHROOM,
    "subtype" : 2,
    "pval" : 500,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 1,
    "cost" : 0,
    "rarity1" : "common5"
  };

  gizmos.prototypes[", Confusion"] = { 
    "symbol" : ",",
    "color" : "d",
    "type" : SHROOM,
    "subtype" : 3,
    "pval" : 500,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 1,
    "cost" : 0,
    "rarity1" : "common5"
  };

  gizmos.prototypes[", Hallucination"] = { 
    "symbol" : ",",
    "color" : "d",
    "type" : SHROOM,
    "subtype" : 4,
    "pval" : 500,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 1,
    "cost" : 0,
    "rarity1" : "common10"
  };

  gizmos.prototypes[", Paralysis"] = { 
    "symbol" : ",",
    "color" : "d",
    "type" : SHROOM,
    "subtype" : 5,
    "pval" : 500,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 1,
    "cost" : 0,
    "rarity1" : "common20"
  };

  gizmos.prototypes[", Weakness"] = { 
    "symbol" : ",",
    "color" : "d",
    "type" : SHROOM,
    "subtype" : 6,
    "pval" : 500,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 1,
    "cost" : 0,
    "rarity1" : "common10",
    "armor" : 0,
    "damage" : "5d5",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes[", Sickness"] = { 
    "symbol" : ",",
    "color" : "d",
    "type" : SHROOM,
    "subtype" : 7,
    "pval" : 500,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 1,
    "cost" : 0,
    "rarity1" : "common10",
    "armor" : 0,
    "damage" : "4d4",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes[", Stupidity"] = { 
    "symbol" : ",",
    "color" : "d",
    "type" : SHROOM,
    "subtype" : 8,
    "pval" : 500,
    "depth" : 15,
    "rarity" : 0,
    "weight" : 1,
    "cost" : 0,
    "rarity1" : "common15"
  };

  gizmos.prototypes[", Naivety"] = { 
    "symbol" : ",",
    "color" : "d",
    "type" : SHROOM,
    "subtype" : 9,
    "pval" : 500,
    "depth" : 15,
    "rarity" : 0,
    "weight" : 1,
    "cost" : 0,
    "rarity1" : "common15"
  };

  gizmos.prototypes[", Unhealth"] = { 
    "symbol" : ",",
    "color" : "d",
    "type" : SHROOM,
    "subtype" : 10,
    "pval" : 500,
    "depth" : 15,
    "rarity" : 0,
    "weight" : 1,
    "cost" : 50,
    "rarity1" : "common15",
    "armor" : 0,
    "damage" : "10d10",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes[", Disease"] = { 
    "symbol" : ",",
    "color" : "d",
    "type" : SHROOM,
    "subtype" : 11,
    "pval" : 500,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 1,
    "cost" : 50,
    "rarity1" : "common20",
    "armor" : 0,
    "damage" : "10d10",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes[", Cure Poison"] = { 
    "symbol" : ",",
    "color" : "d",
    "type" : SHROOM,
    "subtype" : 12,
    "pval" : 500,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 1,
    "cost" : 60,
    "rarity1" : "common10"
  };

  gizmos.prototypes[", Cure Blindness"] = { 
    "symbol" : ",",
    "color" : "d",
    "type" : SHROOM,
    "subtype" : 13,
    "pval" : 500,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 1,
    "cost" : 50,
    "rarity1" : "common10"
  };

  gizmos.prototypes[", Cure Paranoia"] = { 
    "symbol" : ",",
    "color" : "d",
    "type" : SHROOM,
    "subtype" : 14,
    "pval" : 500,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 1,
    "cost" : 25,
    "rarity1" : "common10"
  };

  gizmos.prototypes[", Cure Confusion"] = { 
    "symbol" : ",",
    "color" : "d",
    "type" : SHROOM,
    "subtype" : 15,
    "pval" : 500,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 1,
    "cost" : 50,
    "rarity1" : "common10"
  };

  gizmos.prototypes[", Cure Serious Wounds"] = { 
    "symbol" : ",",
    "color" : "d",
    "type" : SHROOM,
    "subtype" : 16,
    "pval" : 500,
    "depth" : 15,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 75,
    "rarity1" : "common15"
  };

  gizmos.prototypes[", Restore Strength"] = { 
    "symbol" : ",",
    "color" : "d",
    "type" : SHROOM,
    "subtype" : 17,
    "pval" : 500,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 1,
    "cost" : 350,
    "rarity1" : "common20"
  };

  gizmos.prototypes[", Restore Constitution"] = { 
    "symbol" : ",",
    "color" : "d",
    "type" : SHROOM,
    "subtype" : 18,
    "pval" : 500,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 1,
    "cost" : 350,
    "rarity1" : "common20"
  };

  gizmos.prototypes[", Restoring"] = { 
    "symbol" : ",",
    "color" : "d",
    "type" : SHROOM,
    "subtype" : 19,
    "pval" : 500,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 1,
    "cost" : 1000,
    "rarity1" : "not_so_rare20",
    "rarity2" : "uncommon30",
    "rarity3" : "common40"
  };

  gizmos.prototypes[", & Hard Biscuit~"] = { 
    "symbol" : ",",
    "color" : "U",
    "type" : FOOD,
    "subtype" : 32,
    "pval" : 500,
    "depth" : 0,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 1
  };

  gizmos.prototypes[", & Strip~ of Venison"] = { 
    "symbol" : ",",
    "color" : "u",
    "type" : FOOD,
    "subtype" : 33,
    "pval" : 1500,
    "depth" : 0,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 2
  };

  gizmos.prototypes[", & Ration~"] = { 
    "symbol" : ",",
    "color" : "U",
    "type" : FOOD,
    "subtype" : 35,
    "pval" : 5000,
    "depth" : 0,
    "rarity" : 0,
    "weight" : 10,
    "cost" : 3,
    "rarity1" : "common0",
    "rarity2" : "common5",
    "rarity3" : "common20"
  };

  gizmos.prototypes[", & Slime Mold~"] = { 
    "symbol" : ",",
    "color" : "g",
    "type" : FOOD,
    "subtype" : 36,
    "pval" : 3000,
    "depth" : 1,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 2,
    "rarity1" : "level1"
  };

  gizmos.prototypes[", & Piece~ of Ambrosia"] = { 
    "symbol" : ",",
    "color" : "B",
    "type" : FOOD,
    "subtype" : 37,
    "pval" : 7500,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 3,
    "cost" : 10,
    "rarity1" : "common5",
    "rarity2" : "common10",
    "rarity3" : "common20"
  };

  gizmos.prototypes[", & Pint~ of Fine Ale"] = { 
    "symbol" : ",",
    "color" : "y",
    "type" : FOOD,
    "subtype" : 38,
    "pval" : 500,
    "depth" : 0,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 1
  };

  gizmos.prototypes[", & Pint~ of Fine Wine"] = { 
    "symbol" : ",",
    "color" : "r",
    "type" : FOOD,
    "subtype" : 39,
    "pval" : 1000,
    "depth" : 0,
    "rarity" : 0,
    "weight" : 10,
    "cost" : 2
  };

  gizmos.prototypes["| & Broken Dagger~"] = { 
    "symbol" : "|",
    "color" : "D",
    "type" : BLADE,
    "subtype" : 1,
    "pval" : 0,
    "depth" : 0,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 1,
    "rarity1" : "common0",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : -2,
    "damnonus" : -4,
    "acbonus" : 0,
    "SHOW_MODS" : true
  };

  gizmos.prototypes["| & Broken Sword~"] = { 
    "symbol" : "|",
    "color" : "D",
    "type" : BLADE,
    "subtype" : 2,
    "pval" : 0,
    "depth" : 0,
    "rarity" : 0,
    "weight" : 30,
    "cost" : 2,
    "rarity1" : "common0",
    "armor" : 0,
    "damage" : "1d2",
    "hitbonus" : -2,
    "damnonus" : -4,
    "acbonus" : 0,
    "SHOW_MODS" : true
  };

  gizmos.prototypes["| & Dagger~"] = { 
    "symbol" : "|",
    "color" : "W",
    "type" : BLADE,
    "subtype" : 4,
    "pval" : 0,
    "depth" : 0,
    "rarity" : 0,
    "weight" : 12,
    "cost" : 10,
    "rarity1" : "common0",
    "rarity2" : "common5",
    "rarity3" : "common10",
    "rarity4" : "common20",
    "armor" : 0,
    "damage" : "1d4",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true
  };

  gizmos.prototypes["| & Main Gauche~"] = { 
    "symbol" : "|",
    "color" : "W",
    "type" : BLADE,
    "subtype" : 5,
    "pval" : 0,
    "depth" : 3,
    "rarity" : 0,
    "weight" : 30,
    "cost" : 25,
    "rarity1" : "common3",
    "armor" : 0,
    "damage" : "1d5",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true
  };

  gizmos.prototypes["| & Rapier~"] = { 
    "symbol" : "|",
    "color" : "W",
    "type" : BLADE,
    "subtype" : 7,
    "pval" : 0,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 40,
    "cost" : 42,
    "rarity1" : "common5",
    "armor" : 0,
    "damage" : "1d6",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true
  };

  gizmos.prototypes["| & Small Sword~"] = { 
    "symbol" : "|",
    "color" : "W",
    "type" : BLADE,
    "subtype" : 8,
    "pval" : 0,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 75,
    "cost" : 48,
    "rarity1" : "common5",
    "armor" : 0,
    "damage" : "1d6",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true
  };

  gizmos.prototypes["| & Short Sword~"] = { 
    "symbol" : "|",
    "color" : "W",
    "type" : BLADE,
    "subtype" : 10,
    "pval" : 0,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 80,
    "cost" : 90,
    "rarity1" : "common5",
    "armor" : 0,
    "damage" : "1d7",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true
  };

  gizmos.prototypes["| & Sabre~"] = { 
    "symbol" : "|",
    "color" : "W",
    "type" : BLADE,
    "subtype" : 11,
    "pval" : 0,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 50,
    "cost" : 50,
    "rarity1" : "common5",
    "armor" : 0,
    "damage" : "1d7",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true
  };

  gizmos.prototypes["| & Cutlass~"] = { 
    "symbol" : "|",
    "color" : "W",
    "type" : BLADE,
    "subtype" : 12,
    "pval" : 0,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 110,
    "cost" : 85,
    "rarity1" : "common5",
    "armor" : 0,
    "damage" : "1d7",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true
  };

  gizmos.prototypes["| & Tulwar~"] = { 
    "symbol" : "|",
    "color" : "W",
    "type" : BLADE,
    "subtype" : 15,
    "pval" : 0,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 100,
    "cost" : 200,
    "rarity1" : "common5",
    "armor" : 0,
    "damage" : "2d4",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true
  };

  gizmos.prototypes["| & Broad Sword~"] = { 
    "symbol" : "|",
    "color" : "W",
    "type" : BLADE,
    "subtype" : 16,
    "pval" : 0,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 150,
    "cost" : 255,
    "rarity1" : "common10",
    "rarity2" : "common15",
    "armor" : 0,
    "damage" : "2d5",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true
  };

  gizmos.prototypes["| & Long Sword~"] = { 
    "symbol" : "|",
    "color" : "W",
    "type" : BLADE,
    "subtype" : 17,
    "pval" : 0,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 130,
    "cost" : 300,
    "rarity1" : "common10",
    "rarity2" : "common20",
    "armor" : 0,
    "damage" : "2d5",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true
  };

  gizmos.prototypes["| & Scimitar~"] = { 
    "symbol" : "|",
    "color" : "W",
    "type" : BLADE,
    "subtype" : 18,
    "pval" : 0,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 130,
    "cost" : 250,
    "rarity1" : "common10",
    "armor" : 0,
    "damage" : "2d5",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true
  };

  gizmos.prototypes["| & Katana~"] = { 
    "symbol" : "|",
    "color" : "W",
    "type" : BLADE,
    "subtype" : 20,
    "pval" : 0,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 120,
    "cost" : 400,
    "rarity1" : "common20",
    "armor" : 0,
    "damage" : "3d4",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true
  };

  gizmos.prototypes["| & Bastard Sword~"] = { 
    "symbol" : "|",
    "color" : "W",
    "type" : BLADE,
    "subtype" : 21,
    "pval" : 0,
    "depth" : 15,
    "rarity" : 0,
    "weight" : 140,
    "cost" : 350,
    "rarity1" : "common15",
    "armor" : 0,
    "damage" : "3d4",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true
  };

  gizmos.prototypes["| & Two-Handed Sword~"] = { 
    "symbol" : "|",
    "color" : "W",
    "type" : BLADE,
    "subtype" : 25,
    "pval" : 0,
    "depth" : 30,
    "rarity" : 0,
    "weight" : 200,
    "cost" : 775,
    "rarity1" : "common30",
    "rarity2" : "common40",
    "armor" : 0,
    "damage" : "3d6",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true
  };

  gizmos.prototypes["| & Executioner's Sword~"] = { 
    "symbol" : "|",
    "color" : "r",
    "type" : BLADE,
    "subtype" : 28,
    "pval" : 0,
    "depth" : 40,
    "rarity" : 0,
    "weight" : 260,
    "cost" : 850,
    "rarity1" : "common40",
    "armor" : 0,
    "damage" : "4d5",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true
  };

  gizmos.prototypes["| & Blade~ of Chaos"] = { 
    "symbol" : "|",
    "color" : "v",
    "type" : BLADE,
    "subtype" : 30,
    "pval" : 0,
    "depth" : 70,
    "rarity" : 0,
    "weight" : 180,
    "cost" : 4000,
    "rarity1" : "not_so_rare70",
    "armor" : 0,
    "damage" : "6d5",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true,
    "RES_CHAOS" : true
  };

  gizmos.prototypes["\\ & Whip~"] = { 
    "symbol" : "\\",
    "color" : "D",
    "type" : HAFTED,
    "subtype" : 2,
    "pval" : 0,
    "depth" : 3,
    "rarity" : 0,
    "weight" : 30,
    "cost" : 30,
    "rarity1" : "common3",
    "armor" : 0,
    "damage" : "2d4",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true
  };

  gizmos.prototypes["\\ & Quarterstaff~"] = { 
    "symbol" : "\\",
    "color" : "U",
    "type" : HAFTED,
    "subtype" : 3,
    "pval" : 0,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 150,
    "cost" : 200,
    "rarity1" : "common10",
    "armor" : 0,
    "damage" : "1d6",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true
  };

  gizmos.prototypes["\\ & Mace~"] = { 
    "symbol" : "\\",
    "color" : "D",
    "type" : HAFTED,
    "subtype" : 5,
    "pval" : 0,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 120,
    "cost" : 130,
    "rarity1" : "common5",
    "armor" : 0,
    "damage" : "2d6",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true
  };

  gizmos.prototypes["\\ & Ball-and-Chain~"] = { 
    "symbol" : "\\",
    "color" : "D",
    "type" : HAFTED,
    "subtype" : 6,
    "pval" : 0,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 150,
    "cost" : 200,
    "rarity1" : "common20",
    "armor" : 0,
    "damage" : "3d6",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true
  };

  gizmos.prototypes["\\ & War Hammer~"] = { 
    "symbol" : "\\",
    "color" : "D",
    "type" : HAFTED,
    "subtype" : 8,
    "pval" : 0,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 120,
    "cost" : 225,
    "rarity1" : "common5",
    "armor" : 0,
    "damage" : "2d6",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true
  };

  gizmos.prototypes["\\ & Lucerne Hammer~"] = { 
    "symbol" : "\\",
    "color" : "B",
    "type" : HAFTED,
    "subtype" : 10,
    "pval" : 0,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 120,
    "cost" : 376,
    "rarity1" : "common10",
    "armor" : 0,
    "damage" : "2d4",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true
  };

  gizmos.prototypes["\\ & Morning Star~"] = { 
    "symbol" : "\\",
    "color" : "D",
    "type" : HAFTED,
    "subtype" : 12,
    "pval" : 0,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 150,
    "cost" : 396,
    "rarity1" : "common10",
    "armor" : 0,
    "damage" : "1d9",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true
  };

  gizmos.prototypes["\\ & Flail~"] = { 
    "symbol" : "\\",
    "color" : "D",
    "type" : HAFTED,
    "subtype" : 13,
    "pval" : 0,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 150,
    "cost" : 353,
    "rarity1" : "common10",
    "armor" : 0,
    "damage" : "3d3",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true
  };

  gizmos.prototypes["\\ & Lead-Filled Mace~"] = { 
    "symbol" : "\\",
    "color" : "D",
    "type" : HAFTED,
    "subtype" : 15,
    "pval" : 0,
    "depth" : 15,
    "rarity" : 0,
    "weight" : 180,
    "cost" : 502,
    "rarity1" : "common15",
    "armor" : 0,
    "damage" : "3d4",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true
  };

  gizmos.prototypes["\\ & Two-Handed Flail~"] = { 
    "symbol" : "\\",
    "color" : "y",
    "type" : HAFTED,
    "subtype" : 18,
    "pval" : 0,
    "depth" : 45,
    "rarity" : 0,
    "weight" : 280,
    "cost" : 590,
    "rarity1" : "common45",
    "armor" : 0,
    "damage" : "5d8",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true
  };

  gizmos.prototypes["\\ & Mace~ of Disruption"] = { 
    "symbol" : "\\",
    "color" : "v",
    "type" : HAFTED,
    "subtype" : 20,
    "pval" : 0,
    "depth" : 80,
    "rarity" : 0,
    "weight" : 400,
    "cost" : 4300,
    "rarity1" : "not_so_rare80",
    "armor" : 0,
    "damage" : "2d5",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true,
    "SLAY_UNDEAD" : true
  };

  gizmos.prototypes["/ & Beaked Axe~"] = { 
    "symbol" : "/",
    "color" : "s",
    "type" : POLEARM,
    "subtype" : 10,
    "pval" : 0,
    "depth" : 15,
    "rarity" : 0,
    "weight" : 180,
    "cost" : 408,
    "rarity1" : "common15",
    "armor" : 0,
    "damage" : "2d6",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true
  };

  gizmos.prototypes["/ & Glaive~"] = { 
    "symbol" : "/",
    "color" : "s",
    "type" : POLEARM,
    "subtype" : 13,
    "pval" : 0,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 190,
    "cost" : 363,
    "rarity1" : "common20",
    "armor" : 0,
    "damage" : "2d6",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true
  };

  gizmos.prototypes["/ & Halberd~"] = { 
    "symbol" : "/",
    "color" : "s",
    "type" : POLEARM,
    "subtype" : 15,
    "pval" : 0,
    "depth" : 25,
    "rarity" : 0,
    "weight" : 190,
    "cost" : 430,
    "rarity1" : "common25",
    "armor" : 0,
    "damage" : "3d5",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true
  };

  gizmos.prototypes["/ & Awl-Pike~"] = { 
    "symbol" : "/",
    "color" : "s",
    "type" : POLEARM,
    "subtype" : 4,
    "pval" : 0,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 160,
    "cost" : 340,
    "rarity1" : "common10",
    "armor" : 0,
    "damage" : "1d8",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true
  };

  gizmos.prototypes["/ & Pike~"] = { 
    "symbol" : "/",
    "color" : "s",
    "type" : POLEARM,
    "subtype" : 8,
    "pval" : 0,
    "depth" : 15,
    "rarity" : 0,
    "weight" : 160,
    "cost" : 358,
    "rarity1" : "common15",
    "armor" : 0,
    "damage" : "2d5",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true
  };

  gizmos.prototypes["/ & Spear~"] = { 
    "symbol" : "/",
    "color" : "s",
    "type" : POLEARM,
    "subtype" : 2,
    "pval" : 0,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 50,
    "cost" : 36,
    "rarity1" : "common5",
    "armor" : 0,
    "damage" : "1d6",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true
  };

  gizmos.prototypes["/ & Trident~"] = { 
    "symbol" : "/",
    "color" : "y",
    "type" : POLEARM,
    "subtype" : 5,
    "pval" : 0,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 70,
    "cost" : 120,
    "rarity1" : "common5",
    "armor" : 0,
    "damage" : "1d8",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true
  };

  gizmos.prototypes["/ & Lance~"] = { 
    "symbol" : "/",
    "color" : "s",
    "type" : POLEARM,
    "subtype" : 20,
    "pval" : 0,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 300,
    "cost" : 230,
    "rarity1" : "common10",
    "armor" : 0,
    "damage" : "2d8",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true
  };

  gizmos.prototypes["/ & Great Axe~"] = { 
    "symbol" : "/",
    "color" : "s",
    "type" : POLEARM,
    "subtype" : 25,
    "pval" : 0,
    "depth" : 40,
    "rarity" : 0,
    "weight" : 230,
    "cost" : 500,
    "rarity1" : "common40",
    "armor" : 0,
    "damage" : "4d4",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true
  };

  gizmos.prototypes["/ & Battle Axe~"] = { 
    "symbol" : "/",
    "color" : "s",
    "type" : POLEARM,
    "subtype" : 22,
    "pval" : 0,
    "depth" : 15,
    "rarity" : 0,
    "weight" : 170,
    "cost" : 334,
    "rarity1" : "common15",
    "armor" : 0,
    "damage" : "2d8",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true
  };

  gizmos.prototypes["/ & Lochaber Axe~"] = { 
    "symbol" : "/",
    "color" : "D",
    "type" : POLEARM,
    "subtype" : 28,
    "pval" : 0,
    "depth" : 45,
    "rarity" : 0,
    "weight" : 250,
    "cost" : 750,
    "rarity1" : "common45",
    "armor" : 0,
    "damage" : "3d8",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true
  };

  gizmos.prototypes["/ & Broad Axe~"] = { 
    "symbol" : "/",
    "color" : "s",
    "type" : POLEARM,
    "subtype" : 11,
    "pval" : 0,
    "depth" : 15,
    "rarity" : 0,
    "weight" : 160,
    "cost" : 304,
    "rarity1" : "common15",
    "armor" : 0,
    "damage" : "2d6",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true
  };

  gizmos.prototypes["/ & Scythe~"] = { 
    "symbol" : "/",
    "color" : "s",
    "type" : POLEARM,
    "subtype" : 17,
    "pval" : 0,
    "depth" : 45,
    "rarity" : 0,
    "weight" : 250,
    "cost" : 800,
    "rarity1" : "common45",
    "armor" : 0,
    "damage" : "5d3",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true
  };

  gizmos.prototypes["/ & Scythe~ of Slicing"] = { 
    "symbol" : "/",
    "color" : "r",
    "type" : POLEARM,
    "subtype" : 30,
    "pval" : 0,
    "depth" : 60,
    "rarity" : 0,
    "weight" : 250,
    "cost" : 3500,
    "rarity1" : "uncommon60",
    "armor" : 0,
    "damage" : "8d4",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true,
    "VORPAL" : true
  };

  gizmos.prototypes["} & Short Bow~"] = { 
    "symbol" : "}",
    "color" : "U",
    "type" : LAUNCHER,
    "subtype" : 12,
    "pval" : 0,
    "depth" : 3,
    "rarity" : 0,
    "weight" : 30,
    "cost" : 50,
    "rarity1" : "common3",
    "SHOW_MODS" : true
  };

  gizmos.prototypes["} & Long Bow~"] = { 
    "symbol" : "}",
    "color" : "U",
    "type" : LAUNCHER,
    "subtype" : 13,
    "pval" : 0,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 40,
    "cost" : 120,
    "rarity1" : "common10",
    "SHOW_MODS" : true
  };

  gizmos.prototypes["} & Light Crossbow~"] = { 
    "symbol" : "}",
    "color" : "s",
    "type" : LAUNCHER,
    "subtype" : 23,
    "pval" : 0,
    "depth" : 15,
    "rarity" : 0,
    "weight" : 110,
    "cost" : 140,
    "rarity1" : "common15",
    "SHOW_MODS" : true
  };

  gizmos.prototypes["} & Heavy Crossbow~"] = { 
    "symbol" : "}",
    "color" : "s",
    "type" : LAUNCHER,
    "subtype" : 24,
    "pval" : 0,
    "depth" : 30,
    "rarity" : 0,
    "weight" : 200,
    "cost" : 300,
    "rarity1" : "common30",
    "SHOW_MODS" : true
  };

  gizmos.prototypes["} & Sling~"] = { 
    "symbol" : "}",
    "color" : "u",
    "type" : LAUNCHER,
    "subtype" : 2,
    "pval" : 0,
    "depth" : 1,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 5,
    "rarity1" : "common1",
    "SHOW_MODS" : true
  };

  gizmos.prototypes["{ & Arrow~"] = { 
    "symbol" : "{",
    "color" : "U",
    "type" : ARROW,
    "subtype" : 1,
    "pval" : 0,
    "depth" : 3,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 1,
    "rarity1" : "common3",
    "rarity2" : "common15",
    "armor" : 0,
    "damage" : "1d4",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true
  };

  gizmos.prototypes["{ & Seeker Arrow~"] = { 
    "symbol" : "{",
    "color" : "G",
    "type" : ARROW,
    "subtype" : 2,
    "pval" : 0,
    "depth" : 55,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 20,
    "rarity1" : "not_so_common55",
    "armor" : 0,
    "damage" : "4d4",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true
  };

  gizmos.prototypes["{ & Bolt~"] = { 
    "symbol" : "{",
    "color" : "s",
    "type" : BOLT,
    "subtype" : 1,
    "pval" : 0,
    "depth" : 3,
    "rarity" : 0,
    "weight" : 3,
    "cost" : 2,
    "rarity1" : "common3",
    "rarity2" : "common25",
    "armor" : 0,
    "damage" : "1d5",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true
  };

  gizmos.prototypes["{ & Seeker Bolt~"] = { 
    "symbol" : "{",
    "color" : "B",
    "type" : BOLT,
    "subtype" : 2,
    "pval" : 0,
    "depth" : 65,
    "rarity" : 0,
    "weight" : 3,
    "cost" : 25,
    "rarity1" : "uncommon65",
    "armor" : 0,
    "damage" : "4d5",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true
  };

  gizmos.prototypes["{ & Rounded Pebble~"] = { 
    "symbol" : "{",
    "color" : "s",
    "type" : SHOT,
    "subtype" : 0,
    "pval" : 0,
    "depth" : 0,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 1,
    "rarity1" : "common0",
    "armor" : 0,
    "damage" : "1d2",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true
  };

  gizmos.prototypes["{ & Iron Shot~"] = { 
    "symbol" : "{",
    "color" : "s",
    "type" : SHOT,
    "subtype" : 1,
    "pval" : 0,
    "depth" : 3,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 2,
    "rarity1" : "common3",
    "armor" : 0,
    "damage" : "1d3",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true
  };

  gizmos.prototypes["\\ & Shovel~"] = { 
    "symbol" : "\\",
    "color" : "s",
    "type" : DIGGER,
    "subtype" : 1,
    "pval" : 1,
    "depth" : 1,
    "rarity" : 0,
    "weight" : 60,
    "cost" : 10,
    "rarity1" : "rare5",
    "armor" : 0,
    "damage" : "1d2",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true,
    "TUNNEL" : true
  };

  gizmos.prototypes["\\ & Gnomish Shovel~"] = { 
    "symbol" : "\\",
    "color" : "G",
    "type" : DIGGER,
    "subtype" : 2,
    "pval" : 2,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 60,
    "cost" : 100,
    "rarity1" : "uncommon20",
    "armor" : 0,
    "damage" : "1d2",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true,
    "TUNNEL" : true
  };

  gizmos.prototypes["\\ & Dwarven Shovel~"] = { 
    "symbol" : "\\",
    "color" : "B",
    "type" : DIGGER,
    "subtype" : 3,
    "pval" : 3,
    "depth" : 40,
    "rarity" : 0,
    "weight" : 120,
    "cost" : 200,
    "rarity1" : "common40",
    "armor" : 0,
    "damage" : "1d3",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true,
    "TUNNEL" : true
  };

  gizmos.prototypes["\\ & Pick~"] = { 
    "symbol" : "\\",
    "color" : "s",
    "type" : DIGGER,
    "subtype" : 4,
    "pval" : 1,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 150,
    "cost" : 50,
    "rarity1" : "not_so_rare16",
    "armor" : 0,
    "damage" : "1d3",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true,
    "TUNNEL" : true
  };

  gizmos.prototypes["\\ & Ekkuyan Pick~"] = { 
    "symbol" : "\\",
    "color" : "g",
    "type" : DIGGER,
    "subtype" : 5,
    "pval" : 2,
    "depth" : 30,
    "rarity" : 0,
    "weight" : 150,
    "cost" : 300,
    "rarity1" : "uncommon30",
    "armor" : 0,
    "damage" : "1d3",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true,
    "TUNNEL" : true
  };

  gizmos.prototypes["\\ & Dwarven Pick~"] = { 
    "symbol" : "\\",
    "color" : "b",
    "type" : DIGGER,
    "subtype" : 6,
    "pval" : 3,
    "depth" : 50,
    "rarity" : 0,
    "weight" : 200,
    "cost" : 600,
    "rarity1" : "common50",
    "armor" : 0,
    "damage" : "1d4",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true,
    "TUNNEL" : true
  };

  gizmos.prototypes["] & Pair~ of Sandals"] = { 
    "symbol" : "]",
    "color" : "U",
    "type" : BOOTS,
    "subtype" : 1,
    "pval" : 0,
    "depth" : 3,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 3,
    "rarity1" : "common0",
    "armor" : 1,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["] & Pair~ of Soft Leather Boots"] = { 
    "symbol" : "]",
    "color" : "U",
    "type" : BOOTS,
    "subtype" : 2,
    "pval" : 0,
    "depth" : 3,
    "rarity" : 0,
    "weight" : 20,
    "cost" : 7,
    "rarity1" : "common3",
    "armor" : 2,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["] & Pair~ of Hard Leather Boots"] = { 
    "symbol" : "]",
    "color" : "U",
    "type" : BOOTS,
    "subtype" : 3,
    "pval" : 0,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 40,
    "cost" : 12,
    "rarity1" : "common5",
    "armor" : 3,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["] & Pair~ of Metal Shod Boots"] = { 
    "symbol" : "]",
    "color" : "s",
    "type" : BOOTS,
    "subtype" : 6,
    "pval" : 0,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 80,
    "cost" : 50,
    "rarity1" : "common20",
    "armor" : 6,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["] & Hard Leather Cap~"] = { 
    "symbol" : "]",
    "color" : "u",
    "type" : HELM,
    "subtype" : 2,
    "pval" : 0,
    "depth" : 3,
    "rarity" : 0,
    "weight" : 15,
    "cost" : 12,
    "rarity1" : "common3",
    "armor" : 2,
    "damage" : "0d0",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["] & Metal Cap~"] = { 
    "symbol" : "]",
    "color" : "s",
    "type" : HELM,
    "subtype" : 3,
    "pval" : 0,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 20,
    "cost" : 30,
    "rarity1" : "common10",
    "armor" : 3,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["] & Iron Helm~"] = { 
    "symbol" : "]",
    "color" : "s",
    "type" : HELM,
    "subtype" : 5,
    "pval" : 0,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 75,
    "cost" : 75,
    "rarity1" : "common20",
    "armor" : 5,
    "damage" : "1d3",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["] & Steel Helm~"] = { 
    "symbol" : "]",
    "color" : "W",
    "type" : HELM,
    "subtype" : 6,
    "pval" : 0,
    "depth" : 40,
    "rarity" : 0,
    "weight" : 60,
    "cost" : 200,
    "rarity1" : "common40",
    "armor" : 6,
    "damage" : "1d3",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["] & Shedu Helmet~"] = { 
    "symbol" : "]",
    "color" : "G",
    "type" : HELM,
    "subtype" : 7,
    "pval" : 0,
    "depth" : 45,
    "rarity" : 0,
    "weight" : 50,
    "cost" : 10000,
    "rarity1" : "uncommon80",
    "armor" : 8,
    "damage" : "1d3",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 10,
    "IGNORE_ACID" : true,
    "IGNORE_FIRE" : true,
    "IGNORE_ELEC" : true,
    "IGNORE_COLD" : true
  };

  gizmos.prototypes["] & Iron Crown~"] = { 
    "symbol" : "]",
    "color" : "s",
    "type" : CROWN,
    "subtype" : 10,
    "pval" : 0,
    "depth" : 45,
    "rarity" : 0,
    "weight" : 20,
    "cost" : 500,
    "rarity1" : "common40",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["] & Golden Crown~"] = { 
    "symbol" : "]",
    "color" : "y",
    "type" : CROWN,
    "subtype" : 11,
    "pval" : 0,
    "depth" : 45,
    "rarity" : 0,
    "weight" : 30,
    "cost" : 1000,
    "rarity1" : "common45",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "IGNORE_ACID" : true
  };

  gizmos.prototypes["] & Jewel Encrusted Crown~"] = { 
    "symbol" : "]",
    "color" : "v",
    "type" : CROWN,
    "subtype" : 12,
    "pval" : 0,
    "depth" : 50,
    "rarity" : 0,
    "weight" : 40,
    "cost" : 2000,
    "rarity1" : "common50",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "IGNORE_ACID" : true
  };

  gizmos.prototypes["] & Lead Crown~"] = { 
    "symbol" : "]",
    "color" : "D",
    "type" : CROWN,
    "subtype" : 50,
    "pval" : 0,
    "depth" : 44,
    "rarity" : 0,
    "weight" : 20,
    "cost" : 1000,
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "INSTA_ART" : true
  };

  gizmos.prototypes["( & Robe~"] = { 
    "symbol" : "(",
    "color" : "b",
    "type" : SOFT_ARMOR,
    "subtype" : 2,
    "pval" : 0,
    "depth" : 1,
    "rarity" : 0,
    "weight" : 20,
    "cost" : 4,
    "rarity1" : "common1",
    "armor" : 2,
    "damage" : "0d0",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["( & Filthy Rag~"] = { 
    "symbol" : "(",
    "color" : "D",
    "type" : SOFT_ARMOR,
    "subtype" : 1,
    "pval" : 0,
    "depth" : 0,
    "rarity" : 0,
    "weight" : 20,
    "cost" : 1,
    "rarity1" : "common0",
    "armor" : 1,
    "damage" : "0d0",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : -1
  };

  gizmos.prototypes["( Soft Leather Armour"] = { 
    "symbol" : "(",
    "color" : "U",
    "type" : SOFT_ARMOR,
    "subtype" : 4,
    "pval" : 0,
    "depth" : 3,
    "rarity" : 0,
    "weight" : 80,
    "cost" : 18,
    "rarity1" : "common3",
    "armor" : 4,
    "damage" : "0d0",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["( Soft Studded Leather"] = { 
    "symbol" : "(",
    "color" : "U",
    "type" : SOFT_ARMOR,
    "subtype" : 5,
    "pval" : 0,
    "depth" : 3,
    "rarity" : 0,
    "weight" : 90,
    "cost" : 35,
    "rarity1" : "common3",
    "armor" : 5,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["( Hard Leather Armour"] = { 
    "symbol" : "(",
    "color" : "U",
    "type" : SOFT_ARMOR,
    "subtype" : 6,
    "pval" : 0,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 100,
    "cost" : 150,
    "rarity1" : "common5",
    "armor" : 6,
    "damage" : "1d1",
    "hitbonus" : -1,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["( Hard Studded Leather"] = { 
    "symbol" : "(",
    "color" : "U",
    "type" : SOFT_ARMOR,
    "subtype" : 7,
    "pval" : 0,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 110,
    "cost" : 200,
    "rarity1" : "common10",
    "armor" : 7,
    "damage" : "1d2",
    "hitbonus" : -1,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["( Leather Scale Mail"] = { 
    "symbol" : "(",
    "color" : "U",
    "type" : SOFT_ARMOR,
    "subtype" : 11,
    "pval" : 0,
    "depth" : 15,
    "rarity" : 0,
    "weight" : 140,
    "cost" : 450,
    "rarity1" : "common15",
    "armor" : 11,
    "damage" : "1d1",
    "hitbonus" : -1,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["[ Rusty Chain Mail"] = { 
    "symbol" : "[",
    "color" : "r",
    "type" : HARD_ARMOR,
    "subtype" : 1,
    "pval" : 0,
    "depth" : 25,
    "rarity" : 0,
    "weight" : 200,
    "cost" : 550,
    "rarity1" : "common25",
    "armor" : 14,
    "damage" : "1d4",
    "hitbonus" : -5,
    "damnonus" : 0,
    "acbonus" : -8
  };

  gizmos.prototypes["[ Metal Scale Mail"] = { 
    "symbol" : "[",
    "color" : "s",
    "type" : HARD_ARMOR,
    "subtype" : 3,
    "pval" : 0,
    "depth" : 25,
    "rarity" : 0,
    "weight" : 250,
    "cost" : 550,
    "rarity1" : "common25",
    "armor" : 13,
    "damage" : "1d4",
    "hitbonus" : -2,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["[ Chain Mail"] = { 
    "symbol" : "[",
    "color" : "s",
    "type" : HARD_ARMOR,
    "subtype" : 4,
    "pval" : 0,
    "depth" : 25,
    "rarity" : 0,
    "weight" : 220,
    "cost" : 750,
    "rarity1" : "common25",
    "armor" : 14,
    "damage" : "1d4",
    "hitbonus" : -2,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["[ Augmented Chain Mail"] = { 
    "symbol" : "[",
    "color" : "s",
    "type" : HARD_ARMOR,
    "subtype" : 6,
    "pval" : 0,
    "depth" : 30,
    "rarity" : 0,
    "weight" : 270,
    "cost" : 900,
    "rarity1" : "common30",
    "armor" : 16,
    "damage" : "1d4",
    "hitbonus" : -2,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["[ Double Chain Mail"] = { 
    "symbol" : "[",
    "color" : "s",
    "type" : HARD_ARMOR,
    "subtype" : 7,
    "pval" : 0,
    "depth" : 30,
    "rarity" : 0,
    "weight" : 250,
    "cost" : 850,
    "rarity1" : "common30",
    "armor" : 16,
    "damage" : "1d4",
    "hitbonus" : -2,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["[ Bar Chain Mail"] = { 
    "symbol" : "[",
    "color" : "s",
    "type" : HARD_ARMOR,
    "subtype" : 8,
    "pval" : 0,
    "depth" : 35,
    "rarity" : 0,
    "weight" : 280,
    "cost" : 950,
    "rarity1" : "common35",
    "armor" : 18,
    "damage" : "1d4",
    "hitbonus" : -2,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["[ Metal Brigandine Armour"] = { 
    "symbol" : "[",
    "color" : "s",
    "type" : HARD_ARMOR,
    "subtype" : 9,
    "pval" : 0,
    "depth" : 35,
    "rarity" : 0,
    "weight" : 290,
    "cost" : 1100,
    "rarity1" : "common35",
    "armor" : 19,
    "damage" : "1d4",
    "hitbonus" : -3,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["[ Partial Plate Armour"] = { 
    "symbol" : "[",
    "color" : "W",
    "type" : HARD_ARMOR,
    "subtype" : 12,
    "pval" : 0,
    "depth" : 45,
    "rarity" : 0,
    "weight" : 260,
    "cost" : 1200,
    "rarity1" : "common45",
    "armor" : 22,
    "damage" : "1d6",
    "hitbonus" : -3,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["[ Metal Lamellar Armour"] = { 
    "symbol" : "[",
    "color" : "W",
    "type" : HARD_ARMOR,
    "subtype" : 13,
    "pval" : 0,
    "depth" : 45,
    "rarity" : 0,
    "weight" : 340,
    "cost" : 1250,
    "rarity1" : "common45",
    "armor" : 23,
    "damage" : "1d6",
    "hitbonus" : -3,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["[ Full Plate Armour"] = { 
    "symbol" : "[",
    "color" : "W",
    "type" : HARD_ARMOR,
    "subtype" : 15,
    "pval" : 0,
    "depth" : 45,
    "rarity" : 0,
    "weight" : 380,
    "cost" : 1350,
    "rarity1" : "common45",
    "armor" : 25,
    "damage" : "2d4",
    "hitbonus" : -3,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["[ Ribbed Plate Armour"] = { 
    "symbol" : "[",
    "color" : "W",
    "type" : HARD_ARMOR,
    "subtype" : 18,
    "pval" : 0,
    "depth" : 50,
    "rarity" : 0,
    "weight" : 380,
    "cost" : 1500,
    "rarity1" : "common50",
    "armor" : 28,
    "damage" : "2d4",
    "hitbonus" : -3,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["[ Platinum Chain Mail"] = { 
    "symbol" : "[",
    "color" : "B",
    "type" : HARD_ARMOR,
    "subtype" : 20,
    "pval" : 0,
    "depth" : 55,
    "rarity" : 0,
    "weight" : 150,
    "cost" : 7000,
    "rarity1" : "uncommon55",
    "armor" : 28,
    "damage" : "1d4",
    "hitbonus" : -1,
    "damnonus" : 0,
    "acbonus" : 0,
    "IGNORE_ACID" : true
  };

  gizmos.prototypes["[ Platinum Plate Mail"] = { 
    "symbol" : "[",
    "color" : "B",
    "type" : HARD_ARMOR,
    "subtype" : 25,
    "pval" : 0,
    "depth" : 60,
    "rarity" : 0,
    "weight" : 300,
    "cost" : 15000,
    "rarity1" : "uncommon60",
    "armor" : 35,
    "damage" : "2d4",
    "hitbonus" : -3,
    "damnonus" : 0,
    "acbonus" : 0,
    "IGNORE_ACID" : true
  };

  gizmos.prototypes["[ Adamantite Plate Mail"] = { 
    "symbol" : "[",
    "color" : "G",
    "type" : HARD_ARMOR,
    "subtype" : 30,
    "pval" : 0,
    "depth" : 75,
    "rarity" : 0,
    "weight" : 420,
    "cost" : 20000,
    "rarity1" : "not_so_rare75",
    "armor" : 40,
    "damage" : "2d4",
    "hitbonus" : -4,
    "damnonus" : 0,
    "acbonus" : 0,
    "IGNORE_ACID" : true
  };

  gizmos.prototypes["[ Golden corslet"] = { 
    "symbol" : "[",
    "color" : "y",
    "type" : HARD_ARMOR,
    "subtype" : 35,
    "pval" : 0,
    "depth" : 40,
    "rarity" : 0,
    "weight" : 340,
    "cost" : 7500,
    "rarity1" : "uncommon40",
    "armor" : 40,
    "damage" : "2d4",
    "hitbonus" : -6,
    "damnonus" : 0,
    "acbonus" : 15,
    "IGNORE_ACID" : true
  };

  gizmos.prototypes["[ & Shedu Shield~"] = { 
    "symbol" : "[",
    "color" : "G",
    "type" : SHIELD,
    "subtype" : 6,
    "pval" : 0,
    "depth" : 70,
    "rarity" : 0,
    "weight" : 100,
    "cost" : 10000,
    "rarity1" : "uncommon_80",
    "armor" : 8,
    "damage" : "1d3",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 10,
    "IGNORE_ACID" : true,
    "IGNORE_FIRE" : true,
    "IGNORE_ELEC" : true,
    "IGNORE_COLD" : true
  };

  gizmos.prototypes["[ & Shield~ of Deflection"] = { 
    "symbol" : "[",
    "color" : "B",
    "type" : SHIELD,
    "subtype" : 10,
    "pval" : 0,
    "depth" : 70,
    "rarity" : 0,
    "weight" : 100,
    "cost" : 10000,
    "rarity1" : "not_so_rare70",
    "armor" : 10,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 10,
    "IGNORE_ACID" : true
  };

  gizmos.prototypes[") & Small Leather Shield~"] = { 
    "symbol" : ")",
    "color" : "U",
    "type" : SHIELD,
    "subtype" : 2,
    "pval" : 0,
    "depth" : 3,
    "rarity" : 0,
    "weight" : 50,
    "cost" : 30,
    "rarity1" : "common3",
    "armor" : 2,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes[") & Large Leather Shield~"] = { 
    "symbol" : ")",
    "color" : "U",
    "type" : SHIELD,
    "subtype" : 4,
    "pval" : 0,
    "depth" : 15,
    "rarity" : 0,
    "weight" : 100,
    "cost" : 120,
    "rarity1" : "common15",
    "armor" : 4,
    "damage" : "1d2",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes[") & Small Metal Shield~"] = { 
    "symbol" : ")",
    "color" : "s",
    "type" : SHIELD,
    "subtype" : 3,
    "pval" : 0,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 65,
    "cost" : 50,
    "rarity1" : "common10",
    "armor" : 3,
    "damage" : "1d2",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes[") & Large Metal Shield~"] = { 
    "symbol" : ")",
    "color" : "s",
    "type" : SHIELD,
    "subtype" : 5,
    "pval" : 0,
    "depth" : 30,
    "rarity" : 0,
    "weight" : 120,
    "cost" : 200,
    "rarity1" : "common30",
    "armor" : 5,
    "damage" : "1d3",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["= Woe"] = { 
    "symbol" : "=",
    "color" : "y",
    "type" : RING,
    "subtype" : 0,
    "pval" : -5,
    "depth" : 50,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 0,
    "rarity1" : "common50",
    "CURSED" : true,
    "TELEPORT" : true,
    "WIS" : true,
    "CHA" : true,
    "HIDE_TYPE" : true
  };

  gizmos.prototypes["= Aggravation"] = { 
    "symbol" : "=",
    "color" : "y",
    "type" : RING,
    "subtype" : 1,
    "pval" : 0,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 0,
    "rarity1" : "common5",
    "CURSED" : true,
    "AGGRAVATE" : true,
    "EASY_KNOW" : true
  };

  gizmos.prototypes["= Weakness"] = { 
    "symbol" : "=",
    "color" : "y",
    "type" : RING,
    "subtype" : 2,
    "pval" : -5,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 0,
    "rarity1" : "common5",
    "CURSED" : true,
    "STR" : true,
    "HIDE_TYPE" : true
  };

  gizmos.prototypes["= Stupidity"] = { 
    "symbol" : "=",
    "color" : "y",
    "type" : RING,
    "subtype" : 3,
    "pval" : -5,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 0,
    "rarity1" : "common5",
    "CURSED" : true,
    "INT" : true,
    "HIDE_TYPE" : true
  };

  gizmos.prototypes["= Teleportation"] = { 
    "symbol" : "=",
    "color" : "y",
    "type" : RING,
    "subtype" : 4,
    "pval" : 0,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 250,
    "rarity1" : "common5",
    "CURSED" : true,
    "TELEPORT" : true,
    "EASY_KNOW" : true
  };

  gizmos.prototypes["= Slow Digestion"] = { 
    "symbol" : "=",
    "color" : "y",
    "type" : RING,
    "subtype" : 6,
    "pval" : 0,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 250,
    "rarity1" : "common5",
    "SLOW_DIGEST" : true,
    "EASY_KNOW" : true
  };

  gizmos.prototypes["= Levitation"] = { 
    "symbol" : "=",
    "color" : "y",
    "type" : RING,
    "subtype" : 7,
    "pval" : 0,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 200,
    "rarity1" : "common5",
    "FEATHER" : true,
    "EASY_KNOW" : true
  };

  gizmos.prototypes["= Resist Fire"] = { 
    "symbol" : "=",
    "color" : "y",
    "type" : RING,
    "subtype" : 8,
    "pval" : 0,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 250,
    "rarity1" : "common10",
    "RES_FIRE" : true,
    "IGNORE_FIRE" : true,
    "EASY_KNOW" : true
  };

  gizmos.prototypes["= Resist Cold"] = { 
    "symbol" : "=",
    "color" : "y",
    "type" : RING,
    "subtype" : 9,
    "pval" : 0,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 250,
    "rarity1" : "common10",
    "RES_COLD" : true,
    "IGNORE_COLD" : true,
    "EASY_KNOW" : true
  };

  gizmos.prototypes["= Sustain Body"] = { 
    "symbol" : "=",
    "color" : "o",
    "type" : RING,
    "subtype" : 10,
    "pval" : 0,
    "depth" : 30,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 750,
    "rarity1" : "common30",
    "SUST_STR" : true,
    "SUST_CON" : true,
    "SUST_DEX" : true,
    "EASY_KNOW" : true
  };

  gizmos.prototypes["= Sustain Mind"] = { 
    "symbol" : "=",
    "color" : "b",
    "type" : RING,
    "subtype" : 11,
    "pval" : 0,
    "depth" : 30,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 600,
    "rarity1" : "common30",
    "SUST_INT" : true,
    "SUST_WIS" : true,
    "EASY_KNOW" : true
  };

  gizmos.prototypes["= Combat Lust"] = { 
    "symbol" : "=",
    "color" : "r",
    "type" : RING,
    "subtype" : 12,
    "pval" : 0,
    "depth" : 30,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 600,
    "rarity1" : "common30",
    "armor" : 0,
    "damage" : "0d0",
    "hitbonus" : 5,
    "damnonus" : 5,
    "acbonus" : 0,
    "AGGRAVATE" : true,
    "EASY_KNOW" : true
  };

  gizmos.prototypes["= Elevation"] = { 
    "symbol" : "=",
    "color" : "y",
    "type" : RING,
    "subtype" : 13,
    "pval" : 1,
    "depth" : 30,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 750,
    "rarity1" : "common30",
    "STR" : true,
    "CON" : true,
    "DEX" : true,
    "CHA" : true,
    "WIS" : true,
    "INT" : true,
    "EASY_KNOW" : true
  };

  gizmos.prototypes["= Temperance"] = { 
    "symbol" : "=",
    "color" : "y",
    "type" : RING,
    "subtype" : 14,
    "pval" : 0,
    "depth" : 30,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 750,
    "rarity1" : "common30",
    "RES_COLD" : true,
    "RES_FIRE" : true,
    "EASY_KNOW" : true
  };

  gizmos.prototypes["= Shadows"] = { 
    "symbol" : "=",
    "color" : "d",
    "type" : RING,
    "subtype" : 15,
    "pval" : 0,
    "depth" : 30,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 500,
    "rarity1" : "common30",
    "RES_LITE" : true,
    "RES_DARK" : true,
    "EASY_KNOW" : true
  };

  gizmos.prototypes["= Protection"] = { 
    "symbol" : "=",
    "color" : "y",
    "type" : RING,
    "subtype" : 16,
    "pval" : 0,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 500,
    "rarity1" : "common10"
  };

  gizmos.prototypes["= Acid"] = { 
    "symbol" : "=",
    "color" : "y",
    "type" : RING,
    "subtype" : 17,
    "pval" : 0,
    "depth" : 50,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 3000,
    "rarity1" : "common50",
    "armor" : 0,
    "damage" : "0d0",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 15,
    "RES_ACID" : true,
    "IGNORE_ACID" : true,
    "ACTIVATE" : true
  };

  gizmos.prototypes["= Flames"] = { 
    "symbol" : "=",
    "color" : "y",
    "type" : RING,
    "subtype" : 18,
    "pval" : 0,
    "depth" : 50,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 3000,
    "rarity1" : "common50",
    "armor" : 0,
    "damage" : "0d0",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 15,
    "RES_FIRE" : true,
    "IGNORE_FIRE" : true,
    "ACTIVATE" : true
  };

  gizmos.prototypes["= Ice"] = { 
    "symbol" : "=",
    "color" : "y",
    "type" : RING,
    "subtype" : 19,
    "pval" : 0,
    "depth" : 50,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 3000,
    "rarity1" : "common50",
    "armor" : 0,
    "damage" : "0d0",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 15,
    "RES_COLD" : true,
    "IGNORE_COLD" : true,
    "ACTIVATE" : true
  };

  gizmos.prototypes["= Poison Resistance"] = { 
    "symbol" : "=",
    "color" : "y",
    "type" : RING,
    "subtype" : 20,
    "pval" : 0,
    "depth" : 60,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 16000,
    "rarity1" : "common60",
    "RES_POIS" : true,
    "EASY_KNOW" : true
  };

  gizmos.prototypes["= Free Action"] = { 
    "symbol" : "=",
    "color" : "y",
    "type" : RING,
    "subtype" : 21,
    "pval" : 0,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 1500,
    "rarity1" : "common20",
    "FREE_ACT" : true,
    "EASY_KNOW" : true
  };

  gizmos.prototypes["= See Invisible"] = { 
    "symbol" : "=",
    "color" : "y",
    "type" : RING,
    "subtype" : 22,
    "pval" : 0,
    "depth" : 30,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 340,
    "rarity1" : "common30",
    "SEE_INVIS" : true,
    "EASY_KNOW" : true
  };

  gizmos.prototypes["= Searching"] = { 
    "symbol" : "=",
    "color" : "y",
    "type" : RING,
    "subtype" : 23,
    "pval" : 0,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 250,
    "rarity1" : "common5",
    "SEARCH" : true,
    "HIDE_TYPE" : true
  };

  gizmos.prototypes["= Strength"] = { 
    "symbol" : "=",
    "color" : "y",
    "type" : RING,
    "subtype" : 24,
    "pval" : 0,
    "depth" : 30,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 500,
    "rarity1" : "common30",
    "STR" : true,
    "HIDE_TYPE" : true,
    "SUST_STR" : true
  };

  gizmos.prototypes["= Intelligence"] = { 
    "symbol" : "=",
    "color" : "y",
    "type" : RING,
    "subtype" : 25,
    "pval" : 0,
    "depth" : 30,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 500,
    "rarity1" : "common30",
    "INT" : true,
    "HIDE_TYPE" : true,
    "SUST_INT" : true
  };

  gizmos.prototypes["= Dexterity"] = { 
    "symbol" : "=",
    "color" : "y",
    "type" : RING,
    "subtype" : 26,
    "pval" : 0,
    "depth" : 30,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 500,
    "rarity1" : "common30",
    "DEX" : true,
    "HIDE_TYPE" : true,
    "SUST_DEX" : true
  };

  gizmos.prototypes["= Constitution"] = { 
    "symbol" : "=",
    "color" : "y",
    "type" : RING,
    "subtype" : 27,
    "pval" : 0,
    "depth" : 30,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 500,
    "rarity1" : "common30",
    "CON" : true,
    "HIDE_TYPE" : true,
    "SUST_CON" : true
  };

  gizmos.prototypes["= Accuracy"] = { 
    "symbol" : "=",
    "color" : "y",
    "type" : RING,
    "subtype" : 28,
    "pval" : 0,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 500,
    "rarity1" : "common20"
  };

  gizmos.prototypes["= Damage"] = { 
    "symbol" : "=",
    "color" : "y",
    "type" : RING,
    "subtype" : 29,
    "pval" : 0,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 500,
    "rarity1" : "common20"
  };

  gizmos.prototypes["= Slaying"] = { 
    "symbol" : "=",
    "color" : "y",
    "type" : RING,
    "subtype" : 30,
    "pval" : 0,
    "depth" : 40,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 1000,
    "rarity1" : "common40",
    "SHOW_MODS" : true
  };

  gizmos.prototypes["= Speed"] = { 
    "symbol" : "=",
    "color" : "y",
    "type" : RING,
    "subtype" : 31,
    "pval" : 0,
    "depth" : 80,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 100000,
    "rarity1" : "common80",
    "SPEED" : true,
    "HIDE_TYPE" : true
  };

  gizmos.prototypes["= Fear Resistance"] = { 
    "symbol" : "=",
    "color" : "y",
    "type" : RING,
    "subtype" : 38,
    "pval" : 0,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 300,
    "rarity1" : "not_so_common10",
    "RES_FEAR" : true,
    "EASY_KNOW" : true
  };

  gizmos.prototypes["= Playful Shadows"] = { 
    "symbol" : "=",
    "color" : "d",
    "type" : RING,
    "subtype" : 39,
    "pval" : 4,
    "depth" : 30,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 3000,
    "rarity1" : "not_so_common30",
    "RES_LITE" : true,
    "RES_DARK" : true,
    "STEALTH" : true,
    "EASY_KNOW" : true
  };

  gizmos.prototypes["= Nether Resistance"] = { 
    "symbol" : "=",
    "color" : "y",
    "type" : RING,
    "subtype" : 40,
    "pval" : 0,
    "depth" : 34,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 14500,
    "rarity1" : "not_so_common34",
    "RES_NETHER" : true,
    "HOLD_LIFE" : true,
    "EASY_KNOW" : true
  };

  gizmos.prototypes["= Nexus Resistance"] = { 
    "symbol" : "=",
    "color" : "y",
    "type" : RING,
    "subtype" : 41,
    "pval" : 0,
    "depth" : 24,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 3000,
    "rarity1" : "not_so_common24",
    "RES_NEXUS" : true,
    "EASY_KNOW" : true
  };

  gizmos.prototypes["= Sound Resistance"] = { 
    "symbol" : "=",
    "color" : "y",
    "type" : RING,
    "subtype" : 42,
    "pval" : 0,
    "depth" : 26,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 3000,
    "rarity1" : "not_so_common26",
    "RES_SOUND" : true,
    "EASY_KNOW" : true
  };

  gizmos.prototypes["= Confusion Resistance"] = { 
    "symbol" : "=",
    "color" : "y",
    "type" : RING,
    "subtype" : 43,
    "pval" : 0,
    "depth" : 22,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 3000,
    "rarity1" : "not_so_common22",
    "RES_CONF" : true,
    "EASY_KNOW" : true
  };

  gizmos.prototypes["= Shard Resistance"] = { 
    "symbol" : "=",
    "color" : "y",
    "type" : RING,
    "subtype" : 44,
    "pval" : 0,
    "depth" : 25,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 3000,
    "rarity1" : "not_so_common25",
    "RES_SHARDS" : true,
    "EASY_KNOW" : true
  };

  gizmos.prototypes["= Disenchantment Resistance"] = { 
    "symbol" : "=",
    "color" : "y",
    "type" : RING,
    "subtype" : 45,
    "pval" : 0,
    "depth" : 90,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 15000,
    "rarity1" : "not_so_rare90",
    "RES_DISEN" : true,
    "EASY_KNOW" : true
  };

  gizmos.prototypes["= Chaos Resistance"] = { 
    "symbol" : "=",
    "color" : "y",
    "type" : RING,
    "subtype" : 46,
    "pval" : 0,
    "depth" : 50,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 13000,
    "rarity1" : "not_so_common50",
    "RES_CHAOS" : true,
    "RES_CONF" : true,
    "EASY_KNOW" : true
  };

  gizmos.prototypes["= Blindness Resistance"] = { 
    "symbol" : "=",
    "color" : "y",
    "type" : RING,
    "subtype" : 47,
    "pval" : 0,
    "depth" : 60,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 7500,
    "rarity1" : "not_so_common60",
    "RES_BLIND" : true,
    "EASY_KNOW" : true
  };

  gizmos.prototypes["= Lordly Protection"] = { 
    "symbol" : "=",
    "color" : "y",
    "type" : RING,
    "subtype" : 48,
    "pval" : 0,
    "depth" : 100,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 100000,
    "rarity1" : "uncommon100",
    "RES_DISEN" : true,
    "RES_POIS" : true,
    "HOLD_LIFE" : true,
    "FREE_ACT" : true
  };

  gizmos.prototypes["= & Ring of Gabriel, the Messenger"] = { 
    "symbol" : "=",
    "color" : "g",
    "type" : RING,
    "subtype" : 32,
    "pval" : 0,
    "depth" : 50,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 65000,
    "INSTA_ART" : true
  };

  gizmos.prototypes["= & Ring of Saint George, the Slayer"] = { 
    "symbol" : "=",
    "color" : "y",
    "type" : RING,
    "subtype" : 33,
    "pval" : 0,
    "depth" : 90,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 150000,
    "INSTA_ART" : true
  };

  gizmos.prototypes["= & Ring of Saint Raphael, the Healer"] = { 
    "symbol" : "=",
    "color" : "r",
    "type" : RING,
    "subtype" : 34,
    "pval" : 0,
    "depth" : 80,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 100000,
    "INSTA_ART" : true
  };

  gizmos.prototypes["= & Ring of Saint Michael, Prince of Mercy"] = { 
    "symbol" : "=",
    "color" : "b",
    "type" : RING,
    "subtype" : 35,
    "pval" : 0,
    "depth" : 90,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 200000,
    "INSTA_ART" : true
  };

  gizmos.prototypes["= & Ring of the Emmanuel"] = { 
    "symbol" : "=",
    "color" : "b",
    "type" : RING,
    "subtype" : 36,
    "pval" : 0,
    "depth" : 100,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 5000000,
    "INSTA_ART" : true
  };

  gizmos.prototypes["= & Ring of Sheating"] = { 
    "symbol" : "=",
    "color" : "y",
    "type" : RING,
    "subtype" : 37,
    "pval" : 0,
    "depth" : 110,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 5000000,
    "INSTA_ART" : true
  };

  gizmos.prototypes["= & First Ring"] = { 
    "symbol" : "=",
    "color" : "y",
    "type" : RING,
    "subtype" : 50,
    "pval" : 0,
    "depth" : 110,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 5000000,
    "INSTA_ART" : true
  };

  gizmos.prototypes["= Extra Attacks"] = { 
    "symbol" : "=",
    "color" : "y",
    "type" : RING,
    "subtype" : 49,
    "pval" : 0,
    "depth" : 50,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 100000,
    "rarity1" : "not_so_common50",
    "BLOWS" : true
  };

  gizmos.prototypes["\" DOOM"] = { 
    "symbol" : "\"",
    "color" : "d",
    "type" : AMULET,
    "subtype" : 0,
    "pval" : -5,
    "depth" : 50,
    "rarity" : 0,
    "weight" : 3,
    "cost" : 0,
    "rarity1" : "common50",
    "CURSED" : true,
    "STR" : true,
    "INT" : true,
    "WIS" : true,
    "DEX" : true,
    "CON" : true,
    "CHA" : true,
    "HIDE_TYPE" : true
  };

  gizmos.prototypes["\" Teleportation"] = { 
    "symbol" : "\"",
    "color" : "d",
    "type" : AMULET,
    "subtype" : 1,
    "pval" : 0,
    "depth" : 15,
    "rarity" : 0,
    "weight" : 3,
    "cost" : 250,
    "rarity1" : "common15",
    "CURSED" : true,
    "TELEPORT" : true,
    "EASY_KNOW" : true
  };

  gizmos.prototypes["\" Adornment"] = { 
    "symbol" : "\"",
    "color" : "d",
    "type" : AMULET,
    "subtype" : 2,
    "pval" : 0,
    "depth" : 15,
    "rarity" : 0,
    "weight" : 3,
    "cost" : 20,
    "rarity1" : "common15",
    "EASY_KNOW" : true
  };

  gizmos.prototypes["\" Slow Digestion"] = { 
    "symbol" : "\"",
    "color" : "d",
    "type" : AMULET,
    "subtype" : 3,
    "pval" : 0,
    "depth" : 15,
    "rarity" : 0,
    "weight" : 3,
    "cost" : 200,
    "rarity1" : "common15",
    "SLOW_DIGEST" : true,
    "EASY_KNOW" : true
  };

  gizmos.prototypes["\" Resist Acid"] = { 
    "symbol" : "\"",
    "color" : "d",
    "type" : AMULET,
    "subtype" : 4,
    "pval" : 0,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 3,
    "cost" : 300,
    "rarity1" : "common20",
    "RES_ACID" : true,
    "IGNORE_ACID" : true,
    "EASY_KNOW" : true
  };

  gizmos.prototypes["\" Searching"] = { 
    "symbol" : "\"",
    "color" : "d",
    "type" : AMULET,
    "subtype" : 5,
    "pval" : 0,
    "depth" : 30,
    "rarity" : 0,
    "weight" : 3,
    "cost" : 600,
    "rarity1" : "uncommon30",
    "SEARCH" : true,
    "HIDE_TYPE" : true
  };

  gizmos.prototypes["\" Brilliance"] = { 
    "symbol" : "\"",
    "color" : "d",
    "type" : AMULET,
    "subtype" : 6,
    "pval" : 2,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 3,
    "cost" : 500,
    "rarity1" : "common20",
    "INT" : true,
    "WIS" : true,
    "HIDE_TYPE" : true
  };

  gizmos.prototypes["\" Charisma"] = { 
    "symbol" : "\"",
    "color" : "d",
    "type" : AMULET,
    "subtype" : 7,
    "pval" : 0,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 3,
    "cost" : 500,
    "rarity1" : "common20",
    "CHA" : true,
    "HIDE_TYPE" : true,
    "SUST_CHA" : true
  };

  gizmos.prototypes["\" the Magi"] = { 
    "symbol" : "\"",
    "color" : "d",
    "type" : AMULET,
    "subtype" : 8,
    "pval" : 0,
    "depth" : 50,
    "rarity" : 0,
    "weight" : 3,
    "cost" : 30000,
    "rarity1" : "uncommon50",
    "rarity2" : "not_so_common80",
    "armor" : 0,
    "damage" : "0d0",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 3,
    "FREE_ACT" : true,
    "SEE_INVIS" : true,
    "SEARCH" : true,
    "IGNORE_ACID" : true,
    "IGNORE_ELEC" : true,
    "IGNORE_FIRE" : true,
    "IGNORE_COLD" : true
  };

  gizmos.prototypes["\" Reflection"] = { 
    "symbol" : "\"",
    "color" : "d",
    "type" : AMULET,
    "subtype" : 9,
    "pval" : 0,
    "depth" : 60,
    "rarity" : 0,
    "weight" : 3,
    "cost" : 30000,
    "rarity1" : "uncommon60",
    "REFLECT" : true,
    "EASY_KNOW" : true,
    "F" : true,
    "IGNORE_ACID" : true,
    "IGNORE_ELEC" : true,
    "IGNORE_FIRE" : true,
    "IGNORE_COLD" : true
  };

  gizmos.prototypes["\" & Amulet of Purity"] = { 
    "symbol" : "\"",
    "color" : "w",
    "type" : AMULET,
    "subtype" : 10,
    "pval" : 0,
    "depth" : 50,
    "rarity" : 0,
    "weight" : 3,
    "cost" : 60000,
    "INSTA_ART" : true
  };

  gizmos.prototypes["\" & Amulet of the Inquisition"] = { 
    "symbol" : "\"",
    "color" : "o",
    "type" : AMULET,
    "subtype" : 11,
    "pval" : 0,
    "depth" : 60,
    "rarity" : 0,
    "weight" : 3,
    "cost" : 90000,
    "INSTA_ART" : true
  };

  gizmos.prototypes["\" & Amulet of the Dwarves"] = { 
    "symbol" : "\"",
    "color" : "o",
    "type" : AMULET,
    "subtype" : 12,
    "pval" : 0,
    "depth" : 70,
    "rarity" : 0,
    "weight" : 3,
    "cost" : 75000,
    "INSTA_ART" : true
  };

  gizmos.prototypes["\" Anti-Magic"] = { 
    "symbol" : "\"",
    "color" : "d",
    "type" : AMULET,
    "subtype" : 13,
    "pval" : 0,
    "depth" : 40,
    "rarity" : 0,
    "weight" : 3,
    "cost" : 30000,
    "rarity1" : "uncommon40",
    "NO_MAGIC" : true,
    "IGNORE_ACID" : true,
    "IGNORE_ELEC" : true,
    "IGNORE_FIRE" : true,
    "IGNORE_COLD" : true
  };

  gizmos.prototypes["\" Anti-Teleportation"] = { 
    "symbol" : "\"",
    "color" : "d",
    "type" : AMULET,
    "subtype" : 14,
    "pval" : 0,
    "depth" : 30,
    "rarity" : 0,
    "weight" : 3,
    "cost" : 15000,
    "rarity1" : "uncommon30",
    "NO_TELE" : true,
    "IGNORE_ACID" : true,
    "IGNORE_ELEC" : true,
    "IGNORE_FIRE" : true,
    "IGNORE_COLD" : true
  };

  gizmos.prototypes["\" Resistance"] = { 
    "symbol" : "\"",
    "color" : "d",
    "type" : AMULET,
    "subtype" : 15,
    "pval" : 0,
    "depth" : 50,
    "rarity" : 0,
    "weight" : 3,
    "cost" : 25000,
    "rarity1" : "uncommon50",
    "RES_ACID" : true,
    "RES_ELEC" : true,
    "RES_FIRE" : true,
    "RES_COLD" : true,
    "IGNORE_ACID" : true,
    "IGNORE_ELEC" : true,
    "IGNORE_FIRE" : true,
    "IGNORE_COLD" : true
  };

  gizmos.prototypes["? Darkness"] = { 
    "symbol" : "?",
    "color" : "d",
    "type" : SCROLL,
    "subtype" : 0,
    "pval" : 0,
    "depth" : 1,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 0,
    "rarity1" : "common1"
  };

  gizmos.prototypes["? Aggravate Monster"] = { 
    "symbol" : "?",
    "color" : "d",
    "type" : SCROLL,
    "subtype" : 1,
    "pval" : 0,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 0,
    "rarity1" : "common5"
  };

  gizmos.prototypes["? Curse Armor"] = { 
    "symbol" : "?",
    "color" : "d",
    "type" : SCROLL,
    "subtype" : 2,
    "pval" : 0,
    "depth" : 50,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 0,
    "rarity1" : "common50"
  };

  gizmos.prototypes["? Curse Weapon"] = { 
    "symbol" : "?",
    "color" : "d",
    "type" : SCROLL,
    "subtype" : 3,
    "pval" : 0,
    "depth" : 50,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 0,
    "rarity1" : "common50"
  };

  gizmos.prototypes["? Summon Monster"] = { 
    "symbol" : "?",
    "color" : "d",
    "type" : SCROLL,
    "subtype" : 4,
    "pval" : 0,
    "depth" : 1,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 0,
    "rarity1" : "common1"
  };

  gizmos.prototypes["? Summon Undead"] = { 
    "symbol" : "?",
    "color" : "d",
    "type" : SCROLL,
    "subtype" : 5,
    "pval" : 0,
    "depth" : 15,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 0,
    "rarity1" : "common15"
  };

  gizmos.prototypes["? Trap Creation"] = { 
    "symbol" : "?",
    "color" : "d",
    "type" : SCROLL,
    "subtype" : 7,
    "pval" : 0,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 0,
    "rarity1" : "common10"
  };

  gizmos.prototypes["? Phase Door"] = { 
    "symbol" : "?",
    "color" : "d",
    "type" : SCROLL,
    "subtype" : 8,
    "pval" : 0,
    "depth" : 1,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 15,
    "rarity1" : "common1"
  };

  gizmos.prototypes["? Teleportation"] = { 
    "symbol" : "?",
    "color" : "d",
    "type" : SCROLL,
    "subtype" : 9,
    "pval" : 0,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 40,
    "rarity1" : "common10"
  };

  gizmos.prototypes["? Teleport Level"] = { 
    "symbol" : "?",
    "color" : "d",
    "type" : SCROLL,
    "subtype" : 10,
    "pval" : 0,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 50,
    "rarity1" : "common20"
  };

  gizmos.prototypes["? Word of Recall"] = { 
    "symbol" : "?",
    "color" : "d",
    "type" : SCROLL,
    "subtype" : 11,
    "pval" : 0,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 150,
    "rarity1" : "common5"
  };

  gizmos.prototypes["? Identify"] = { 
    "symbol" : "?",
    "color" : "d",
    "type" : SCROLL,
    "subtype" : 12,
    "pval" : 0,
    "depth" : 1,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 50,
    "rarity1" : "common1",
    "rarity2" : "common5",
    "rarity3" : "common10",
    "rarity4" : "common30"
  };

  gizmos.prototypes["? *Identify*"] = { 
    "symbol" : "?",
    "color" : "d",
    "type" : SCROLL,
    "subtype" : 13,
    "pval" : 0,
    "depth" : 30,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 1000,
    "rarity1" : "common30",
    "rarity2" : "not_so_common50",
    "rarity3" : "common80",
    "rarity4" : "common100"
  };

  gizmos.prototypes["? Remove Curse"] = { 
    "symbol" : "?",
    "color" : "d",
    "type" : SCROLL,
    "subtype" : 14,
    "pval" : 0,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 100,
    "rarity1" : "common10",
    "rarity2" : "not_so_common20",
    "rarity3" : "not_so_common40"
  };

  gizmos.prototypes["? *Remove Curse*"] = { 
    "symbol" : "?",
    "color" : "d",
    "type" : SCROLL,
    "subtype" : 15,
    "pval" : 0,
    "depth" : 50,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 8000,
    "rarity1" : "not_so_common50",
    "rarity2" : "not_so_common75",
    "rarity3" : "not_so_common85",
    "rarity4" : "common95"
  };

  gizmos.prototypes["? Enchant Armor"] = { 
    "symbol" : "?",
    "color" : "d",
    "type" : SCROLL,
    "subtype" : 16,
    "pval" : 0,
    "depth" : 15,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 125,
    "rarity1" : "common15"
  };

  gizmos.prototypes["? Enchant Weapon To-Hit"] = { 
    "symbol" : "?",
    "color" : "d",
    "type" : SCROLL,
    "subtype" : 17,
    "pval" : 0,
    "depth" : 15,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 125,
    "rarity1" : "common15"
  };

  gizmos.prototypes["? Enchant Weapon To-Dam"] = { 
    "symbol" : "?",
    "color" : "d",
    "type" : SCROLL,
    "subtype" : 18,
    "pval" : 0,
    "depth" : 15,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 125,
    "rarity1" : "common15"
  };

  gizmos.prototypes["? *Enchant Armor*"] = { 
    "symbol" : "?",
    "color" : "d",
    "type" : SCROLL,
    "subtype" : 20,
    "pval" : 0,
    "depth" : 50,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 500,
    "rarity1" : "common50",
    "rarity2" : "common50"
  };

  gizmos.prototypes["? *Enchant Weapon*"] = { 
    "symbol" : "?",
    "color" : "d",
    "type" : SCROLL,
    "subtype" : 21,
    "pval" : 0,
    "depth" : 50,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 500,
    "rarity1" : "common50"
  };

  gizmos.prototypes["? Recharging"] = { 
    "symbol" : "?",
    "color" : "d",
    "type" : SCROLL,
    "subtype" : 22,
    "pval" : 0,
    "depth" : 40,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 200,
    "rarity1" : "common40"
  };

  gizmos.prototypes["? Light"] = { 
    "symbol" : "?",
    "color" : "d",
    "type" : SCROLL,
    "subtype" : 24,
    "pval" : 0,
    "depth" : 0,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 15,
    "rarity1" : "common0",
    "rarity2" : "common3",
    "rarity3" : "common10"
  };

  gizmos.prototypes["? Magic Mapping"] = { 
    "symbol" : "?",
    "color" : "d",
    "type" : SCROLL,
    "subtype" : 25,
    "pval" : 0,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 40,
    "rarity1" : "common5"
  };

  gizmos.prototypes["? Treasure Detection"] = { 
    "symbol" : "?",
    "color" : "d",
    "type" : SCROLL,
    "subtype" : 26,
    "pval" : 0,
    "depth" : 0,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 15,
    "rarity1" : "common0"
  };

  gizmos.prototypes["? Object Detection"] = { 
    "symbol" : "?",
    "color" : "d",
    "type" : SCROLL,
    "subtype" : 27,
    "pval" : 0,
    "depth" : 0,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 15,
    "rarity1" : "common0"
  };

  gizmos.prototypes["? Trap Detection"] = { 
    "symbol" : "?",
    "color" : "d",
    "type" : SCROLL,
    "subtype" : 28,
    "pval" : 0,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 35,
    "rarity1" : "common5",
    "rarity2" : "common10"
  };

  gizmos.prototypes["? Door/Stair Location"] = { 
    "symbol" : "?",
    "color" : "d",
    "type" : SCROLL,
    "subtype" : 29,
    "pval" : 0,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 35,
    "rarity1" : "common5",
    "rarity2" : "common10",
    "rarity3" : "common15"
  };

  gizmos.prototypes["? Detect Invisible"] = { 
    "symbol" : "?",
    "color" : "d",
    "type" : SCROLL,
    "subtype" : 30,
    "pval" : 0,
    "depth" : 1,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 15,
    "rarity1" : "common1"
  };

  gizmos.prototypes["? Satisfy Hunger"] = { 
    "symbol" : "?",
    "color" : "d",
    "type" : SCROLL,
    "subtype" : 32,
    "pval" : 0,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 10,
    "rarity1" : "common5",
    "rarity2" : "common20",
    "rarity3" : "common50",
    "rarity4" : "common75"
  };

  gizmos.prototypes["? Blessing"] = { 
    "symbol" : "?",
    "color" : "d",
    "type" : SCROLL,
    "subtype" : 33,
    "pval" : 0,
    "depth" : 1,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 15,
    "rarity1" : "common1"
  };

  gizmos.prototypes["? Holy Chant"] = { 
    "symbol" : "?",
    "color" : "d",
    "type" : SCROLL,
    "subtype" : 34,
    "pval" : 0,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 40,
    "rarity1" : "common10"
  };

  gizmos.prototypes["? Holy Prayer"] = { 
    "symbol" : "?",
    "color" : "d",
    "type" : SCROLL,
    "subtype" : 35,
    "pval" : 0,
    "depth" : 25,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 80,
    "rarity1" : "common25"
  };

  gizmos.prototypes["? Monster Confusion"] = { 
    "symbol" : "?",
    "color" : "d",
    "type" : SCROLL,
    "subtype" : 36,
    "pval" : 0,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 30,
    "rarity1" : "common5"
  };

  gizmos.prototypes["? Protection from Evil"] = { 
    "symbol" : "?",
    "color" : "d",
    "type" : SCROLL,
    "subtype" : 37,
    "pval" : 0,
    "depth" : 30,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 250,
    "rarity1" : "common30"
  };

  gizmos.prototypes["? Rune of Protection"] = { 
    "symbol" : "?",
    "color" : "d",
    "type" : SCROLL,
    "subtype" : 38,
    "pval" : 0,
    "depth" : 50,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 500,
    "rarity1" : "uncommon50",
    "rarity2" : "not_so_common90"
  };

  gizmos.prototypes["? Trap/Door Destruction"] = { 
    "symbol" : "?",
    "color" : "d",
    "type" : SCROLL,
    "subtype" : 39,
    "pval" : 0,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 50,
    "rarity1" : "common10"
  };

  gizmos.prototypes["? *Destruction*"] = { 
    "symbol" : "?",
    "color" : "d",
    "type" : SCROLL,
    "subtype" : 41,
    "pval" : 0,
    "depth" : 40,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 250,
    "rarity1" : "common40"
  };

  gizmos.prototypes["? Dispel Undead"] = { 
    "symbol" : "?",
    "color" : "d",
    "type" : SCROLL,
    "subtype" : 42,
    "pval" : 0,
    "depth" : 40,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 200,
    "rarity1" : "common40"
  };

  gizmos.prototypes["? Unsummon"] = { 
    "symbol" : "?",
    "color" : "d",
    "type" : SCROLL,
    "subtype" : 44,
    "pval" : 0,
    "depth" : 40,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 750,
    "rarity1" : "uncommon40",
    "rarity2" : "uncommon80"
  };

  gizmos.prototypes["? Mass Unsummon"] = { 
    "symbol" : "?",
    "color" : "d",
    "type" : SCROLL,
    "subtype" : 45,
    "pval" : 0,
    "depth" : 50,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 1000,
    "rarity1" : "uncommon50",
    "rarity2" : "uncommon100"
  };

  gizmos.prototypes["? Acquirement"] = { 
    "symbol" : "?",
    "color" : "d",
    "type" : SCROLL,
    "subtype" : 46,
    "pval" : 0,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 100000,
    "rarity1" : "not_so_rare20"
  };

  gizmos.prototypes["? *Acquirement*"] = { 
    "symbol" : "?",
    "color" : "d",
    "type" : SCROLL,
    "subtype" : 47,
    "pval" : 0,
    "depth" : 60,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 200000,
    "rarity1" : "rare60"
  };

  gizmos.prototypes["? Fire"] = { 
    "symbol" : "?",
    "color" : "d",
    "type" : SCROLL,
    "subtype" : 48,
    "pval" : 0,
    "depth" : 50,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 1000,
    "rarity1" : "uncommon50",
    "IGNORE_FIRE" : true
  };

  gizmos.prototypes["? Ice"] = { 
    "symbol" : "?",
    "color" : "d",
    "type" : SCROLL,
    "subtype" : 49,
    "pval" : 0,
    "depth" : 75,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 5000,
    "rarity1" : "uncommon75",
    "IGNORE_COLD" : true
  };

  gizmos.prototypes["? Chaos"] = { 
    "symbol" : "?",
    "color" : "d",
    "type" : SCROLL,
    "subtype" : 50,
    "pval" : 0,
    "depth" : 100,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 10000,
    "rarity1" : "not_so_rare100",
    "IGNORE_FIRE" : true,
    "IGNORE_ACID" : true,
    "IGNORE_COLD" : true,
    "IGNORE_ELEC" : true
  };

  gizmos.prototypes["? Rumour"] = { 
    "symbol" : "?",
    "color" : "d",
    "type" : SCROLL,
    "subtype" : 51,
    "pval" : 0,
    "depth" : 1,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 10,
    "rarity1" : "common1"
  };

  gizmos.prototypes["? Artifact Creation"] = { 
    "symbol" : "?",
    "color" : "d",
    "type" : SCROLL,
    "subtype" : 52,
    "pval" : 0,
    "depth" : 70,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 200000,
    "rarity1" : "rare70"
  };

  gizmos.prototypes["! Water"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 0,
    "pval" : 200,
    "depth" : 0,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 1,
    "rarity1" : "common0",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Apple Juice"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 1,
    "pval" : 250,
    "depth" : 0,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 1,
    "rarity1" : "common0",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Slime Mold Juice"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 2,
    "pval" : 400,
    "depth" : 0,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 2,
    "rarity1" : "common0",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Slowness"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 4,
    "pval" : 50,
    "depth" : 1,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 0,
    "rarity1" : "common1",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Salt Water"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 5,
    "pval" : 0,
    "depth" : 0,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 0,
    "rarity1" : "common0",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Poison"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 6,
    "pval" : 0,
    "depth" : 3,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 0,
    "rarity1" : "common3",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Blindness"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 7,
    "pval" : 0,
    "depth" : 0,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 0,
    "rarity1" : "common0",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Booze"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 9,
    "pval" : 50,
    "depth" : 0,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 0,
    "rarity1" : "common0",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Sleep"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 11,
    "pval" : 100,
    "depth" : 0,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 0,
    "rarity1" : "common0",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Lose Memories"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 13,
    "pval" : 0,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 0,
    "rarity1" : "common10",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Ruination"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 15,
    "pval" : -1,
    "depth" : 40,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 0,
    "rarity1" : "not_so_rare40",
    "armor" : 0,
    "damage" : "20d20",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Weakness"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 16,
    "pval" : 0,
    "depth" : 3,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 0,
    "rarity1" : "common3",
    "armor" : 0,
    "damage" : "3d12",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Stupidity"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 17,
    "pval" : 0,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 0,
    "rarity1" : "common20",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Naivety"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 18,
    "pval" : 0,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 0,
    "rarity1" : "common20",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Ugliness"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 21,
    "pval" : 0,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 0,
    "rarity1" : "common20",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Detonations"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 22,
    "pval" : -1,
    "depth" : 60,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 10000,
    "rarity1" : "not_so_rare80",
    "armor" : 0,
    "damage" : "25d25",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Iocaine"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 23,
    "pval" : -1,
    "depth" : 55,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 0,
    "rarity1" : "uncommon55",
    "armor" : 0,
    "damage" : "20d20",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Infra-vision"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 24,
    "pval" : 0,
    "depth" : 3,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 20,
    "rarity1" : "common3",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Detect Invisible"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 25,
    "pval" : 0,
    "depth" : 3,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 50,
    "rarity1" : "common3",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Slow Poison"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 26,
    "pval" : 0,
    "depth" : 1,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 25,
    "rarity1" : "common1",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Neutralize Poison"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 27,
    "pval" : 0,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 75,
    "rarity1" : "common5",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Boldness"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 28,
    "pval" : 0,
    "depth" : 1,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 10,
    "rarity1" : "common1",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Speed"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 29,
    "pval" : 0,
    "depth" : 1,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 75,
    "rarity1" : "common1",
    "rarity2" : "common10",
    "rarity3" : "common40",
    "rarity4" : "common60",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Resist Heat"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 30,
    "pval" : 0,
    "depth" : 1,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 30,
    "rarity1" : "common1",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Resist Cold"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 31,
    "pval" : 0,
    "depth" : 1,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 30,
    "rarity1" : "common1",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Heroism"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 32,
    "pval" : 0,
    "depth" : 1,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 35,
    "rarity1" : "common1",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Berserk Strength"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 33,
    "pval" : 0,
    "depth" : 3,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 100,
    "rarity1" : "common3",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Cure Serious Wounds"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 35,
    "pval" : 100,
    "depth" : 3,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 40,
    "rarity1" : "common3",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Cure Critical Wounds"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 36,
    "pval" : 100,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 100,
    "rarity1" : "common5",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Healing"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 37,
    "pval" : 200,
    "depth" : 15,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 300,
    "rarity1" : "common15",
    "rarity2" : "common30",
    "rarity3" : "common60",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! *Healing*"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 38,
    "pval" : 200,
    "depth" : 40,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 1500,
    "rarity1" : "uncommon40",
    "rarity2" : "uncommon60",
    "rarity3" : "common80",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Life"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 39,
    "pval" : 200,
    "depth" : 60,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 5000,
    "rarity1" : "uncommon60",
    "rarity2" : "no_so_common100",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Restore Mana"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 40,
    "pval" : 50,
    "depth" : 25,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 350,
    "rarity1" : "common25",
    "rarity2" : "not_so_common50",
    "rarity3" : "not_so_common80",
    "rarity4" : "not_so_common100",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Restore Life Levels"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 41,
    "pval" : 0,
    "depth" : 40,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 400,
    "rarity1" : "common40",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Restore Strength"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 42,
    "pval" : 0,
    "depth" : 25,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 300,
    "rarity1" : "common25",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Restore Intelligence"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 43,
    "pval" : 0,
    "depth" : 25,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 300,
    "rarity1" : "common25",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Restore Wisdom"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 44,
    "pval" : 0,
    "depth" : 25,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 300,
    "rarity1" : "common25",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Restore Dexterity"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 45,
    "pval" : 0,
    "depth" : 25,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 300,
    "rarity1" : "common25",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Restore Constitution"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 46,
    "pval" : 0,
    "depth" : 25,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 300,
    "rarity1" : "common25",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Restore Charisma"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 47,
    "pval" : 0,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 300,
    "rarity1" : "common20",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Strength"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 48,
    "pval" : 0,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 8000,
    "rarity1" : "not_so_rare15",
    "rarity2" : "uncommon25",
    "rarity3" : "common30",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Intelligence"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 49,
    "pval" : 0,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 8000,
    "rarity1" : "not_so_rare15",
    "rarity2" : "uncommon25",
    "rarity3" : "common30",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Wisdom"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 50,
    "pval" : 0,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 8000,
    "rarity1" : "not_so_rare15",
    "rarity2" : "uncommon25",
    "rarity3" : "common30",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Dexterity"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 51,
    "pval" : 0,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 8000,
    "rarity1" : "not_so_rare15",
    "rarity2" : "uncommon25",
    "rarity3" : "common30",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Constitution"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 52,
    "pval" : 0,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 8000,
    "rarity1" : "not_so_rare15",
    "rarity2" : "uncommon25",
    "rarity3" : "common30",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Charisma"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 53,
    "pval" : 0,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 1000,
    "rarity1" : "common20",
    "rarity2" : "common25",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Augmentation"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 55,
    "pval" : 100,
    "depth" : 40,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 60000,
    "rarity1" : "rare40",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Enlightenment"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 56,
    "pval" : 0,
    "depth" : 25,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 800,
    "rarity1" : "common25",
    "rarity2" : "common50",
    "rarity3" : "common100",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! *Enlightenment*"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 57,
    "pval" : 50,
    "depth" : 70,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 80000,
    "rarity1" : "uncommon70",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Self Knowledge"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 58,
    "pval" : 20,
    "depth" : 40,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 2000,
    "rarity1" : "common40",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Experience"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 59,
    "pval" : 0,
    "depth" : 65,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 25000,
    "rarity1" : "common65",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Resistance"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 60,
    "pval" : 100,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 250,
    "rarity1" : "common20",
    "rarity2" : "common45",
    "rarity3" : "common80",
    "rarity4" : "common100",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "IGNORE_ACID" : true,
    "IGNORE_ELEC" : true,
    "IGNORE_FIRE" : true,
    "IGNORE_COLD" : true
  };

  gizmos.prototypes["! Curing"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 61,
    "pval" : 100,
    "depth" : 18,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 250,
    "rarity1" : "common18",
    "rarity2" : "common40",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Invulnerability"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 62,
    "pval" : -2500,
    "depth" : 90,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 100000,
    "rarity1" : "not_so_rare90",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! New Life"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 63,
    "pval" : 100,
    "depth" : 50,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 750000,
    "rarity1" : "veryrare50",
    "rarity2" : "not_so_rare100",
    "rarity3" : "uncommon120",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Cure Light Wounds"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 34,
    "pval" : 50,
    "depth" : 0,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 15,
    "rarity1" : "common0",
    "rarity2" : "common1",
    "rarity3" : "common30",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Clumsiness"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 19,
    "pval" : 0,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 0,
    "rarity1" : "common5",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! Sickliness"] = { 
    "symbol" : "!",
    "color" : "d",
    "type" : POTION,
    "subtype" : 20,
    "pval" : 0,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 4,
    "cost" : 0,
    "rarity1" : "common10",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["- Heal Monster"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : WAND,
    "subtype" : 0,
    "pval" : 0,
    "depth" : 2,
    "rarity" : 0,
    "weight" : 10,
    "cost" : 0,
    "rarity1" : "commmon2",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["- Haste Monster"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : WAND,
    "subtype" : 1,
    "pval" : 0,
    "depth" : 2,
    "rarity" : 0,
    "weight" : 10,
    "cost" : 0,
    "rarity1" : "commmon2",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["- Clone Monster"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : WAND,
    "subtype" : 2,
    "pval" : 0,
    "depth" : 15,
    "rarity" : 0,
    "weight" : 10,
    "cost" : 0,
    "rarity1" : "common15",
    "rarity2" : "common50",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["- Teleport Other"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : WAND,
    "subtype" : 3,
    "pval" : 0,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 10,
    "cost" : 350,
    "rarity1" : "common20",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["- Disarming"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : WAND,
    "subtype" : 4,
    "pval" : 0,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 10,
    "cost" : 700,
    "rarity1" : "common20",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["- Trap/Door Destruction"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : WAND,
    "subtype" : 5,
    "pval" : 0,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 10,
    "cost" : 100,
    "rarity1" : "common10",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["- Stone to Mud"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : WAND,
    "subtype" : 6,
    "pval" : 0,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 10,
    "cost" : 300,
    "rarity1" : "common10",
    "rarity2" : "common40",
    "rarity3" : "common80",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["- Light"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : WAND,
    "subtype" : 7,
    "pval" : 0,
    "depth" : 3,
    "rarity" : 0,
    "weight" : 10,
    "cost" : 200,
    "rarity1" : "common3",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["- Sleep Monster"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : WAND,
    "subtype" : 8,
    "pval" : 0,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 10,
    "cost" : 500,
    "rarity1" : "common5",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["- Slow Monster"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : WAND,
    "subtype" : 9,
    "pval" : 0,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 10,
    "cost" : 500,
    "rarity1" : "commmon2",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["- Confuse Monster"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : WAND,
    "subtype" : 10,
    "pval" : 0,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 10,
    "cost" : 500,
    "rarity1" : "common3",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["- Scare Monster"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : WAND,
    "subtype" : 11,
    "pval" : 0,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 10,
    "cost" : 500,
    "rarity1" : "no_so_rare10",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["- Drain Life"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : WAND,
    "subtype" : 12,
    "pval" : 0,
    "depth" : 50,
    "rarity" : 0,
    "weight" : 10,
    "cost" : 1200,
    "rarity1" : "common50",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["- Polymorph"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : WAND,
    "subtype" : 13,
    "pval" : 0,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 10,
    "cost" : 400,
    "rarity1" : "common20",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["- Stinking Cloud"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : WAND,
    "subtype" : 14,
    "pval" : 0,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 10,
    "cost" : 400,
    "rarity1" : "common5",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["- Magic Missile"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : WAND,
    "subtype" : 15,
    "pval" : 0,
    "depth" : 2,
    "rarity" : 0,
    "weight" : 10,
    "cost" : 200,
    "rarity1" : "common2",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["- Acid Bolts"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : WAND,
    "subtype" : 16,
    "pval" : 0,
    "depth" : 30,
    "rarity" : 0,
    "weight" : 10,
    "cost" : 950,
    "rarity1" : "common30",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["- Tame Monster"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : WAND,
    "subtype" : 17,
    "pval" : 0,
    "depth" : 30,
    "rarity" : 0,
    "weight" : 10,
    "cost" : 1500,
    "rarity1" : "not_so_common30",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["- Fire Bolts"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : WAND,
    "subtype" : 18,
    "pval" : 0,
    "depth" : 30,
    "rarity" : 0,
    "weight" : 10,
    "cost" : 1000,
    "rarity1" : "common30",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["- Frost Bolts"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : WAND,
    "subtype" : 19,
    "pval" : 0,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 10,
    "cost" : 800,
    "rarity1" : "common20",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["- Acid"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : WAND,
    "subtype" : 20,
    "pval" : 0,
    "depth" : 50,
    "rarity" : 0,
    "weight" : 10,
    "cost" : 1650,
    "rarity1" : "common50",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "IGNORE_ACID" : true
  };

  gizmos.prototypes["- Lightning Balls"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : WAND,
    "subtype" : 21,
    "pval" : 0,
    "depth" : 35,
    "rarity" : 0,
    "weight" : 10,
    "cost" : 1200,
    "rarity1" : "common35",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "IGNORE_ELEC" : true
  };

  gizmos.prototypes["- Fire Balls"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : WAND,
    "subtype" : 22,
    "pval" : 0,
    "depth" : 50,
    "rarity" : 0,
    "weight" : 10,
    "cost" : 1800,
    "rarity1" : "common50",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "IGNORE_FIRE" : true
  };

  gizmos.prototypes["- Hail"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : WAND,
    "subtype" : 23,
    "pval" : 0,
    "depth" : 40,
    "rarity" : 0,
    "weight" : 10,
    "cost" : 1500,
    "rarity1" : "common40",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "IGNORE_COLD" : true
  };

  gizmos.prototypes["- Wonder"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : WAND,
    "subtype" : 24,
    "pval" : 0,
    "depth" : 3,
    "rarity" : 0,
    "weight" : 10,
    "cost" : 250,
    "rarity1" : "common3",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "IGNORE_ACID" : true,
    "IGNORE_ELEC" : true,
    "IGNORE_FIRE" : true,
    "IGNORE_COLD" : true
  };

  gizmos.prototypes["- Annihilation"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : WAND,
    "subtype" : 25,
    "pval" : 0,
    "depth" : 60,
    "rarity" : 0,
    "weight" : 10,
    "cost" : 3000,
    "rarity1" : "uncommon60",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "IGNORE_ACID" : true,
    "IGNORE_ELEC" : true,
    "IGNORE_FIRE" : true,
    "IGNORE_COLD" : true
  };

  gizmos.prototypes["- Dragon's Flame"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : WAND,
    "subtype" : 26,
    "pval" : 0,
    "depth" : 50,
    "rarity" : 0,
    "weight" : 10,
    "cost" : 2400,
    "rarity1" : "uncommon50",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "IGNORE_ACID" : true,
    "IGNORE_ELEC" : true,
    "IGNORE_FIRE" : true,
    "IGNORE_COLD" : true
  };

  gizmos.prototypes["- Dragon's Frost"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : WAND,
    "subtype" : 27,
    "pval" : 0,
    "depth" : 50,
    "rarity" : 0,
    "weight" : 10,
    "cost" : 2400,
    "rarity1" : "uncommon50",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "IGNORE_ACID" : true,
    "IGNORE_ELEC" : true,
    "IGNORE_FIRE" : true,
    "IGNORE_COLD" : true
  };

  gizmos.prototypes["- Dragon's Breath"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : WAND,
    "subtype" : 28,
    "pval" : 0,
    "depth" : 60,
    "rarity" : 0,
    "weight" : 10,
    "cost" : 2400,
    "rarity1" : "uncommon60",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "IGNORE_ACID" : true,
    "IGNORE_ELEC" : true,
    "IGNORE_FIRE" : true,
    "IGNORE_COLD" : true
  };

  gizmos.prototypes["- Shards"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : WAND,
    "subtype" : 29,
    "pval" : 0,
    "depth" : 75,
    "rarity" : 0,
    "weight" : 10,
    "cost" : 95000,
    "rarity1" : "uncommon75",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "IGNORE_ACID" : true,
    "IGNORE_ELEC" : true,
    "IGNORE_FIRE" : true,
    "IGNORE_COLD" : true
  };

  gizmos.prototypes["_ Darkness"] = { 
    "symbol" : "_",
    "color" : "d",
    "type" : STAFF,
    "subtype" : 0,
    "pval" : 0,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 50,
    "cost" : 0,
    "rarity1" : "common5",
    "rarity2" : "common50",
    "armor" : 0,
    "damage" : "1d2",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["_ Slowness"] = { 
    "symbol" : "_",
    "color" : "d",
    "type" : STAFF,
    "subtype" : 1,
    "pval" : 0,
    "depth" : 40,
    "rarity" : 0,
    "weight" : 50,
    "cost" : 0,
    "rarity1" : "common40",
    "armor" : 0,
    "damage" : "1d2",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["_ Haste Monsters"] = { 
    "symbol" : "_",
    "color" : "d",
    "type" : STAFF,
    "subtype" : 2,
    "pval" : 0,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 50,
    "cost" : 0,
    "rarity1" : "common10",
    "armor" : 0,
    "damage" : "1d2",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["_ Summoning"] = { 
    "symbol" : "_",
    "color" : "d",
    "type" : STAFF,
    "subtype" : 3,
    "pval" : 0,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 50,
    "cost" : 0,
    "rarity1" : "common10",
    "rarity2" : "common50",
    "armor" : 0,
    "damage" : "1d2",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["_ Teleportation"] = { 
    "symbol" : "_",
    "color" : "d",
    "type" : STAFF,
    "subtype" : 4,
    "pval" : 0,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 50,
    "cost" : 2000,
    "rarity1" : "common20",
    "armor" : 0,
    "damage" : "1d2",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["_ Perception"] = { 
    "symbol" : "_",
    "color" : "d",
    "type" : STAFF,
    "subtype" : 5,
    "pval" : 0,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 50,
    "cost" : 400,
    "rarity1" : "common10",
    "armor" : 0,
    "damage" : "1d2",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["_ Remove Curse"] = { 
    "symbol" : "_",
    "color" : "d",
    "type" : STAFF,
    "subtype" : 6,
    "pval" : 0,
    "depth" : 40,
    "rarity" : 0,
    "weight" : 50,
    "cost" : 500,
    "rarity1" : "common40",
    "armor" : 0,
    "damage" : "1d2",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["_ Starlight"] = { 
    "symbol" : "_",
    "color" : "d",
    "type" : STAFF,
    "subtype" : 7,
    "pval" : 0,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 50,
    "cost" : 800,
    "rarity1" : "common20",
    "armor" : 0,
    "damage" : "1d2",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["_ Light"] = { 
    "symbol" : "_",
    "color" : "d",
    "type" : STAFF,
    "subtype" : 8,
    "pval" : 0,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 50,
    "cost" : 250,
    "rarity1" : "common5",
    "armor" : 0,
    "damage" : "1d2",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["_ Enlightenment"] = { 
    "symbol" : "_",
    "color" : "d",
    "type" : STAFF,
    "subtype" : 9,
    "pval" : 0,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 50,
    "cost" : 750,
    "rarity1" : "common20",
    "armor" : 0,
    "damage" : "1d2",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["_ Treasure Location"] = { 
    "symbol" : "_",
    "color" : "d",
    "type" : STAFF,
    "subtype" : 10,
    "pval" : 0,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 50,
    "cost" : 200,
    "rarity1" : "common5",
    "armor" : 0,
    "damage" : "1d2",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["_ Object Location"] = { 
    "symbol" : "_",
    "color" : "d",
    "type" : STAFF,
    "subtype" : 11,
    "pval" : 0,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 50,
    "cost" : 200,
    "rarity1" : "common5",
    "armor" : 0,
    "damage" : "1d2",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["_ Trap Location"] = { 
    "symbol" : "_",
    "color" : "d",
    "type" : STAFF,
    "subtype" : 12,
    "pval" : 0,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 50,
    "cost" : 350,
    "rarity1" : "common10",
    "armor" : 0,
    "damage" : "1d2",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["_ Door/Stair Location"] = { 
    "symbol" : "_",
    "color" : "d",
    "type" : STAFF,
    "subtype" : 13,
    "pval" : 0,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 50,
    "cost" : 350,
    "rarity1" : "common10",
    "armor" : 0,
    "damage" : "1d2",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["_ Detect Invisible"] = { 
    "symbol" : "_",
    "color" : "d",
    "type" : STAFF,
    "subtype" : 14,
    "pval" : 0,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 50,
    "cost" : 200,
    "rarity1" : "common5",
    "armor" : 0,
    "damage" : "1d2",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["_ Detect Evil"] = { 
    "symbol" : "_",
    "color" : "d",
    "type" : STAFF,
    "subtype" : 15,
    "pval" : 0,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 50,
    "cost" : 350,
    "rarity1" : "common20",
    "armor" : 0,
    "damage" : "1d2",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["_ Cure Light Wounds"] = { 
    "symbol" : "_",
    "color" : "d",
    "type" : STAFF,
    "subtype" : 16,
    "pval" : 0,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 50,
    "cost" : 350,
    "rarity1" : "common5",
    "armor" : 0,
    "damage" : "1d2",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["_ Curing"] = { 
    "symbol" : "_",
    "color" : "d",
    "type" : STAFF,
    "subtype" : 17,
    "pval" : 0,
    "depth" : 25,
    "rarity" : 0,
    "weight" : 50,
    "cost" : 1000,
    "rarity1" : "common25",
    "armor" : 0,
    "damage" : "1d2",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["_ Healing"] = { 
    "symbol" : "_",
    "color" : "d",
    "type" : STAFF,
    "subtype" : 18,
    "pval" : 0,
    "depth" : 70,
    "rarity" : 0,
    "weight" : 50,
    "cost" : 5000,
    "rarity1" : "not_so_common70",
    "armor" : 0,
    "damage" : "1d2",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["_ the Magi"] = { 
    "symbol" : "_",
    "color" : "d",
    "type" : STAFF,
    "subtype" : 19,
    "pval" : 0,
    "depth" : 70,
    "rarity" : 0,
    "weight" : 50,
    "cost" : 4500,
    "rarity1" : "not_so_common70",
    "armor" : 0,
    "damage" : "1d2",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["_ Sleep Monsters"] = { 
    "symbol" : "_",
    "color" : "d",
    "type" : STAFF,
    "subtype" : 20,
    "pval" : 0,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 50,
    "cost" : 700,
    "rarity1" : "common10",
    "armor" : 0,
    "damage" : "1d2",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["_ Slow Monsters"] = { 
    "symbol" : "_",
    "color" : "d",
    "type" : STAFF,
    "subtype" : 21,
    "pval" : 0,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 50,
    "cost" : 800,
    "rarity1" : "common10",
    "armor" : 0,
    "damage" : "1d2",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["_ Speed"] = { 
    "symbol" : "_",
    "color" : "d",
    "type" : STAFF,
    "subtype" : 22,
    "pval" : 0,
    "depth" : 40,
    "rarity" : 0,
    "weight" : 50,
    "cost" : 1000,
    "rarity1" : "common40",
    "armor" : 0,
    "damage" : "1d2",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["_ Probing"] = { 
    "symbol" : "_",
    "color" : "d",
    "type" : STAFF,
    "subtype" : 23,
    "pval" : 0,
    "depth" : 30,
    "rarity" : 0,
    "weight" : 50,
    "cost" : 2000,
    "rarity1" : "common30",
    "armor" : 0,
    "damage" : "1d2",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["_ Dispel Evil"] = { 
    "symbol" : "_",
    "color" : "d",
    "type" : STAFF,
    "subtype" : 24,
    "pval" : 0,
    "depth" : 50,
    "rarity" : 0,
    "weight" : 50,
    "cost" : 1200,
    "rarity1" : "common50",
    "armor" : 0,
    "damage" : "1d2",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["_ Power"] = { 
    "symbol" : "_",
    "color" : "d",
    "type" : STAFF,
    "subtype" : 25,
    "pval" : 0,
    "depth" : 70,
    "rarity" : 0,
    "weight" : 50,
    "cost" : 4000,
    "rarity1" : "not_so_common70",
    "armor" : 0,
    "damage" : "1d2",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["_ Holiness"] = { 
    "symbol" : "_",
    "color" : "d",
    "type" : STAFF,
    "subtype" : 26,
    "pval" : 0,
    "depth" : 70,
    "rarity" : 0,
    "weight" : 50,
    "cost" : 4500,
    "rarity1" : "not_so_common70",
    "armor" : 0,
    "damage" : "1d2",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["_ Genocide"] = { 
    "symbol" : "_",
    "color" : "d",
    "type" : STAFF,
    "subtype" : 27,
    "pval" : 0,
    "depth" : 70,
    "rarity" : 0,
    "weight" : 50,
    "cost" : 3500,
    "rarity1" : "not_so_rare70",
    "armor" : 0,
    "damage" : "1d2",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["_ Earthquakes"] = { 
    "symbol" : "_",
    "color" : "d",
    "type" : STAFF,
    "subtype" : 28,
    "pval" : 0,
    "depth" : 40,
    "rarity" : 0,
    "weight" : 50,
    "cost" : 350,
    "rarity1" : "common40",
    "armor" : 0,
    "damage" : "1d2",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["_ *Destruction*"] = { 
    "symbol" : "_",
    "color" : "d",
    "type" : STAFF,
    "subtype" : 29,
    "pval" : 0,
    "depth" : 50,
    "rarity" : 0,
    "weight" : 50,
    "cost" : 2500,
    "rarity1" : "common1",
    "rarity2" : "common70",
    "armor" : 0,
    "damage" : "1d2",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["? [Book of Common Prayer]"] = { 
    "symbol" : "?",
    "color" : "W",
    "type" : BOOK_MIRACLES,
    "subtype" : 0,
    "pval" : 0,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 30,
    "cost" : 100,
    "rarity1" : "common10",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "realm" : REALM_MIRACLES
  };

  gizmos.prototypes["? [High Mass]"] = { 
    "symbol" : "?",
    "color" : "W",
    "type" : BOOK_MIRACLES,
    "subtype" : 1,
    "pval" : 0,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 30,
    "cost" : 1000,
    "rarity1" : "common20",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "realm" : REALM_MIRACLES
  };

  gizmos.prototypes["? [Book of the Unicorn]"] = { 
    "symbol" : "?",
    "color" : "w",
    "type" : BOOK_MIRACLES,
    "subtype" : 2,
    "pval" : 0,
    "depth" : 40,
    "rarity" : 0,
    "weight" : 30,
    "cost" : 25000,
    "rarity1" : "common40",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "IGNORE_ACID" : true,
    "IGNORE_ELEC" : true,
    "IGNORE_FIRE" : true,
    "IGNORE_COLD" : true,
    "realm" : REALM_MIRACLES
  };

  gizmos.prototypes["? [Blessings of the Grail]"] = { 
    "symbol" : "?",
    "color" : "w",
    "type" : BOOK_MIRACLES,
    "subtype" : 3,
    "pval" : 0,
    "depth" : 90,
    "rarity" : 0,
    "weight" : 30,
    "cost" : 100000,
    "rarity1" : "uncommon90",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "IGNORE_ACID" : true,
    "IGNORE_ELEC" : true,
    "IGNORE_FIRE" : true,
    "IGNORE_COLD" : true,
    "realm" : REALM_MIRACLES
  };

  gizmos.prototypes["? [Beginner's Handbook]"] = { 
    "symbol" : "?",
    "color" : "B",
    "type" : BOOK_SORCERY,
    "subtype" : 0,
    "pval" : 0,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 30,
    "cost" : 100,
    "rarity1" : "common10",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "realm" : REALM_SORCERY
  };

  gizmos.prototypes["? [Master Sorcerer's Handbook]"] = { 
    "symbol" : "?",
    "color" : "B",
    "type" : BOOK_SORCERY,
    "subtype" : 1,
    "pval" : 0,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 30,
    "cost" : 1000,
    "rarity1" : "common20",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "realm" : REALM_SORCERY
  };

  gizmos.prototypes["? [Pattern Sorcery]"] = { 
    "symbol" : "?",
    "color" : "b",
    "type" : BOOK_SORCERY,
    "subtype" : 2,
    "pval" : 0,
    "depth" : 40,
    "rarity" : 0,
    "weight" : 30,
    "cost" : 25000,
    "rarity1" : "common40",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "IGNORE_ACID" : true,
    "IGNORE_ELEC" : true,
    "IGNORE_FIRE" : true,
    "IGNORE_COLD" : true,
    "realm" : REALM_SORCERY
  };

  gizmos.prototypes["? [Grimoire of Power]"] = { 
    "symbol" : "?",
    "color" : "b",
    "type" : BOOK_SORCERY,
    "subtype" : 3,
    "pval" : 0,
    "depth" : 90,
    "rarity" : 0,
    "weight" : 30,
    "cost" : 100000,
    "rarity1" : "uncommon90",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "IGNORE_ACID" : true,
    "IGNORE_ELEC" : true,
    "IGNORE_FIRE" : true,
    "IGNORE_COLD" : true,
    "realm" : REALM_SORCERY
  };

  gizmos.prototypes["? [Call of the Wild]"] = { 
    "symbol" : "?",
    "color" : "G",
    "type" : BOOK_NATURE,
    "subtype" : 0,
    "pval" : 0,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 30,
    "cost" : 100,
    "rarity1" : "common10",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "realm" : REALM_NATURE
  };

  gizmos.prototypes["? [Nature Mastery]"] = { 
    "symbol" : "?",
    "color" : "G",
    "type" : BOOK_NATURE,
    "subtype" : 1,
    "pval" : 0,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 30,
    "cost" : 1000,
    "rarity1" : "common20",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "realm" : REALM_NATURE
  };

  gizmos.prototypes["? [Nature's Gifts]"] = { 
    "symbol" : "?",
    "color" : "g",
    "type" : BOOK_NATURE,
    "subtype" : 2,
    "pval" : 0,
    "depth" : 50,
    "rarity" : 0,
    "weight" : 30,
    "cost" : 25000,
    "rarity1" : "common40",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "IGNORE_ACID" : true,
    "IGNORE_ELEC" : true,
    "IGNORE_FIRE" : true,
    "IGNORE_COLD" : true,
    "realm" : REALM_NATURE
  };

  gizmos.prototypes["? [Nature's Wrath]"] = { 
    "symbol" : "?",
    "color" : "g",
    "type" : BOOK_NATURE,
    "subtype" : 3,
    "pval" : 0,
    "depth" : 80,
    "rarity" : 0,
    "weight" : 30,
    "cost" : 100000,
    "rarity1" : "uncommon90",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "IGNORE_ACID" : true,
    "IGNORE_ELEC" : true,
    "IGNORE_FIRE" : true,
    "IGNORE_COLD" : true,
    "realm" : REALM_NATURE
  };

  gizmos.prototypes["? [Sign of Chaos]"] = { 
    "symbol" : "?",
    "color" : "R",
    "type" : BOOK_CHAOS,
    "subtype" : 0,
    "pval" : 0,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 30,
    "cost" : 100,
    "rarity1" : "common10",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "realm" : REALM_CHAOS
  };

  gizmos.prototypes["? [Chaos Mastery]"] = { 
    "symbol" : "?",
    "color" : "R",
    "type" : BOOK_CHAOS,
    "subtype" : 1,
    "pval" : 0,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 30,
    "cost" : 1000,
    "rarity1" : "common20",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "realm" : REALM_CHAOS
  };

  gizmos.prototypes["? [Chaos Channels]"] = { 
    "symbol" : "?",
    "color" : "r",
    "type" : BOOK_CHAOS,
    "subtype" : 2,
    "pval" : 0,
    "depth" : 50,
    "rarity" : 0,
    "weight" : 30,
    "cost" : 25000,
    "rarity1" : "common40",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "IGNORE_ACID" : true,
    "IGNORE_ELEC" : true,
    "IGNORE_FIRE" : true,
    "IGNORE_COLD" : true,
    "realm" : REALM_CHAOS
  };

  gizmos.prototypes["? [Armageddon Tome]"] = { 
    "symbol" : "?",
    "color" : "r",
    "type" : BOOK_CHAOS,
    "subtype" : 3,
    "pval" : 0,
    "depth" : 100,
    "rarity" : 0,
    "weight" : 30,
    "cost" : 100000,
    "rarity1" : "uncommon90",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "IGNORE_ACID" : true,
    "IGNORE_ELEC" : true,
    "IGNORE_FIRE" : true,
    "IGNORE_COLD" : true,
    "realm" : REALM_CHAOS
  };

  gizmos.prototypes["? [Black Prayers]"] = { 
    "symbol" : "?",
    "color" : "D",
    "type" : BOOK_DEATH,
    "subtype" : 0,
    "pval" : 0,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 30,
    "cost" : 100,
    "rarity1" : "common10",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "realm" : REALM_DEATH
  };

  gizmos.prototypes["? [Black Mass]"] = { 
    "symbol" : "?",
    "color" : "D",
    "type" : BOOK_DEATH,
    "subtype" : 1,
    "pval" : 0,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 30,
    "cost" : 1000,
    "rarity1" : "common20",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "realm" : REALM_DEATH
  };

  gizmos.prototypes["? [Black Channels]"] = { 
    "symbol" : "?",
    "color" : "v",
    "type" : BOOK_DEATH,
    "subtype" : 2,
    "pval" : 0,
    "depth" : 50,
    "rarity" : 0,
    "weight" : 30,
    "cost" : 25000,
    "rarity1" : "common40",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "IGNORE_ACID" : true,
    "IGNORE_ELEC" : true,
    "IGNORE_FIRE" : true,
    "IGNORE_COLD" : true,
    "realm" : REALM_DEATH
  };

  gizmos.prototypes["? [Necronomicon]"] = { 
    "symbol" : "?",
    "color" : "v",
    "type" : BOOK_DEATH,
    "subtype" : 3,
    "pval" : 0,
    "depth" : 90,
    "rarity" : 0,
    "weight" : 30,
    "cost" : 100000,
    "rarity1" : "uncommon90",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "IGNORE_ACID" : true,
    "IGNORE_ELEC" : true,
    "IGNORE_FIRE" : true,
    "IGNORE_COLD" : true,
    "realm" : REALM_DEATH
  };

  gizmos.prototypes["? [Initiations]"] = { 
    "symbol" : "?",
    "color" : "U",
    "type" : BOOK_TAROT,
    "subtype" : 0,
    "pval" : 0,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 30,
    "cost" : 100,
    "rarity1" : "common10",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "realm" : REALM_TAROT
  };

  gizmos.prototypes["? [Minchiate]"] = { 
    "symbol" : "?",
    "color" : "U",
    "type" : BOOK_TAROT,
    "subtype" : 1,
    "pval" : 0,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 30,
    "cost" : 1000,
    "rarity1" : "common20",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "realm" : REALM_TAROT
  };

  gizmos.prototypes["? [Book of Thoth]"] = { 
    "symbol" : "?",
    "color" : "o",
    "type" : BOOK_TAROT,
    "subtype" : 2,
    "pval" : 0,
    "depth" : 40,
    "rarity" : 0,
    "weight" : 30,
    "cost" : 25000,
    "rarity1" : "common40",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "IGNORE_ACID" : true,
    "IGNORE_ELEC" : true,
    "IGNORE_FIRE" : true,
    "IGNORE_COLD" : true,
    "realm" : REALM_TAROT
  };

  gizmos.prototypes["? [Major Arcana]"] = { 
    "symbol" : "?",
    "color" : "o",
    "type" : BOOK_TAROT,
    "subtype" : 3,
    "pval" : 0,
    "depth" : 90,
    "rarity" : 0,
    "weight" : 30,
    "cost" : 100000,
    "rarity1" : "uncommon90",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "IGNORE_ACID" : true,
    "IGNORE_ELEC" : true,
    "IGNORE_FIRE" : true,
    "IGNORE_COLD" : true,
    "realm" : REALM_TAROT
  };

  gizmos.prototypes["? [Cantrips for Beginners]"] = { 
    "symbol" : "?",
    "color" : "s",
    "type" : BOOK_CHARMS,
    "subtype" : 0,
    "pval" : 0,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 30,
    "cost" : 100,
    "rarity1" : "common10",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "realm" : REALM_CHARMS
  };

  gizmos.prototypes["? [Minor Magicks]"] = { 
    "symbol" : "?",
    "color" : "s",
    "type" : BOOK_CHARMS,
    "subtype" : 1,
    "pval" : 0,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 30,
    "cost" : 250,
    "rarity1" : "common20",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "realm" : REALM_CHARMS
  };

  gizmos.prototypes["? [Major Magicks]"] = { 
    "symbol" : "?",
    "color" : "s",
    "type" : BOOK_CHARMS,
    "subtype" : 2,
    "pval" : 0,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 30,
    "cost" : 1000,
    "rarity1" : "common40",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "realm" : REALM_CHARMS
  };

  gizmos.prototypes["? [Magicks of Mastery]"] = { 
    "symbol" : "?",
    "color" : "s",
    "type" : BOOK_CHARMS,
    "subtype" : 3,
    "pval" : 0,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 30,
    "cost" : 2500,
    "rarity1" : "uncommon90",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "realm" : REALM_CHARMS
  };

  gizmos.prototypes["? [Basic Chi Flow]"] = { 
    "symbol" : "?",
    "color" : "U",
    "type" : BOOK_SOMATIC,
    "subtype" : 0,
    "pval" : 0,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 30,
    "cost" : 100,
    "rarity1" : "common10",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "realm" : REALM_SOMATIC
  };

  gizmos.prototypes["? [Yogic Mastery]"] = { 
    "symbol" : "?",
    "color" : "U",
    "type" : BOOK_SOMATIC,
    "subtype" : 1,
    "pval" : 0,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 30,
    "cost" : 1000,
    "rarity1" : "common20",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "realm" : REALM_SOMATIC
  };

  gizmos.prototypes["? [Temple of the Soul]"] = { 
    "symbol" : "?",
    "color" : "y",
    "type" : BOOK_SOMATIC,
    "subtype" : 2,
    "pval" : 0,
    "depth" : 40,
    "rarity" : 0,
    "weight" : 30,
    "cost" : 25000,
    "rarity1" : "common40",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "IGNORE_ACID" : true,
    "IGNORE_ELEC" : true,
    "IGNORE_FIRE" : true,
    "IGNORE_COLD" : true,
    "realm" : REALM_SOMATIC
  };

  gizmos.prototypes["? [Perfection of Body]"] = { 
    "symbol" : "?",
    "color" : "y",
    "type" : BOOK_SOMATIC,
    "subtype" : 3,
    "pval" : 0,
    "depth" : 90,
    "rarity" : 0,
    "weight" : 30,
    "cost" : 100000,
    "rarity1" : "uncommon90",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "IGNORE_ACID" : true,
    "IGNORE_ELEC" : true,
    "IGNORE_FIRE" : true,
    "IGNORE_COLD" : true,
    "realm" : REALM_SOMATIC
  };

  gizmos.prototypes["? [Lesser Pacts]"] = { 
    "symbol" : "?",
    "color" : "U",
    "type" : BOOK_DEMONIC,
    "subtype" : 0,
    "pval" : 0,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 30,
    "cost" : 100,
    "rarity1" : "common10",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "IGNORE_FIRE" : true,
    "realm" : REALM_DEMONIC
  };

  gizmos.prototypes["? [Ghastly Endeavours]"] = { 
    "symbol" : "?",
    "color" : "U",
    "type" : BOOK_DEMONIC,
    "subtype" : 1,
    "pval" : 0,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 30,
    "cost" : 1000,
    "rarity1" : "common20",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "IGNORE_FIRE" : true,
    "realm" : REALM_DEMONIC
  };

  gizmos.prototypes["? [Book of the Grigori]"] = { 
    "symbol" : "?",
    "color" : "y",
    "type" : BOOK_DEMONIC,
    "subtype" : 2,
    "pval" : 0,
    "depth" : 40,
    "rarity" : 0,
    "weight" : 30,
    "cost" : 25000,
    "rarity1" : "common40",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "IGNORE_ACID" : true,
    "IGNORE_ELEC" : true,
    "IGNORE_FIRE" : true,
    "IGNORE_COLD" : true,
    "realm" : REALM_DEMONIC
  };

  gizmos.prototypes["? [Lucifer's Spurn]"] = { 
    "symbol" : "?",
    "color" : "y",
    "type" : BOOK_DEMONIC,
    "subtype" : 3,
    "pval" : 0,
    "depth" : 90,
    "rarity" : 0,
    "weight" : 30,
    "cost" : 100000,
    "rarity1" : "uncommon90",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "IGNORE_ACID" : true,
    "IGNORE_ELEC" : true,
    "IGNORE_FIRE" : true,
    "IGNORE_COLD" : true,
    "realm" : REALM_DEMONIC
  };

  gizmos.prototypes["~ & Small wooden chest~"] = { 
    "symbol" : "~",
    "color" : "s",
    "type" : CHEST,
    "subtype" : 1,
    "pval" : 0,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 250,
    "cost" : 20,
    "rarity1" : "common5",
    "armor" : 0,
    "damage" : "2d3",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["~ & Large wooden chest~"] = { 
    "symbol" : "~",
    "color" : "s",
    "type" : CHEST,
    "subtype" : 5,
    "pval" : 0,
    "depth" : 15,
    "rarity" : 0,
    "weight" : 500,
    "cost" : 60,
    "rarity1" : "common15",
    "armor" : 0,
    "damage" : "2d5",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["~ & Small iron chest~"] = { 
    "symbol" : "~",
    "color" : "s",
    "type" : CHEST,
    "subtype" : 2,
    "pval" : 0,
    "depth" : 25,
    "rarity" : 0,
    "weight" : 300,
    "cost" : 100,
    "rarity1" : "common25",
    "armor" : 0,
    "damage" : "2d4",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["~ & Large iron chest~"] = { 
    "symbol" : "~",
    "color" : "s",
    "type" : CHEST,
    "subtype" : 6,
    "pval" : 0,
    "depth" : 35,
    "rarity" : 0,
    "weight" : 1000,
    "cost" : 150,
    "rarity1" : "common35",
    "armor" : 0,
    "damage" : "2d6",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["~ & Small steel chest~"] = { 
    "symbol" : "~",
    "color" : "s",
    "type" : CHEST,
    "subtype" : 3,
    "pval" : 0,
    "depth" : 45,
    "rarity" : 0,
    "weight" : 500,
    "cost" : 200,
    "rarity1" : "common45",
    "armor" : 0,
    "damage" : "2d4",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["~ & Large steel chest~"] = { 
    "symbol" : "~",
    "color" : "s",
    "type" : CHEST,
    "subtype" : 7,
    "pval" : 0,
    "depth" : 55,
    "rarity" : 0,
    "weight" : 1000,
    "cost" : 250,
    "rarity1" : "common55",
    "armor" : 0,
    "damage" : "2d6",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["~ & Ruined chest~"] = { 
    "symbol" : "~",
    "color" : "s",
    "type" : CHEST,
    "subtype" : 0,
    "pval" : 0,
    "depth" : 0,
    "rarity" : 0,
    "weight" : 250,
    "cost" : 0,
    "rarity1" : "common75"
  };

  gizmos.prototypes["~ & Small bag~"] = { 
    "symbol" : "~",
    "color" : "b",
    "type" : CHEST,
    "subtype" : 8,
    "pval" : 0,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 25,
    "cost" : 20,
    "rarity1" : "veryrare5",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["~ & Wooden Torch~"] = { 
    "symbol" : "~",
    "color" : "u",
    "type" : LITE,
    "subtype" : 0,
    "pval" : 4000,
    "depth" : 1,
    "rarity" : 0,
    "weight" : 30,
    "cost" : 2,
    "rarity1" : "common1",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "EASY_KNOW" : true
  };

  gizmos.prototypes["~ & Brass Lantern~"] = { 
    "symbol" : "~",
    "color" : "U",
    "type" : LITE,
    "subtype" : 1,
    "pval" : 7500,
    "depth" : 3,
    "rarity" : 0,
    "weight" : 50,
    "cost" : 35,
    "rarity1" : "common3",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "EASY_KNOW" : true,
    "IGNORE_FIRE" : true
  };

  gizmos.prototypes["! & Phial of Beatrice"] = { 
    "symbol" : "!",
    "color" : "y",
    "type" : LITE,
    "subtype" : 4,
    "pval" : 0,
    "depth" : 1,
    "rarity" : 0,
    "weight" : 10,
    "cost" : 10000,
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "INSTA_ART" : true
  };

  gizmos.prototypes["* & Gem of Seeing"] = { 
    "symbol" : "*",
    "color" : "y",
    "type" : LITE,
    "subtype" : 5,
    "pval" : 0,
    "depth" : 30,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 25000,
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "INSTA_ART" : true
  };

  gizmos.prototypes["* & Gem of Insight"] = { 
    "symbol" : "*",
    "color" : "R",
    "type" : LITE,
    "subtype" : 6,
    "pval" : 0,
    "depth" : 60,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 60000,
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "INSTA_ART" : true
  };

  gizmos.prototypes["~ & Orb~"] = { 
    "symbol" : "~",
    "color" : "v",
    "type" : LITE,
    "subtype" : 7,
    "pval" : 0,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 80,
    "cost" : 1000,
    "rarity1" : "common10",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! & White Banner~"] = { 
    "symbol" : "!",
    "color" : "w",
    "type" : LITE,
    "subtype" : 8,
    "pval" : 0,
    "depth" : 1,
    "rarity" : 0,
    "weight" : 10,
    "cost" : 10000,
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "INSTA_ART" : true
  };

  gizmos.prototypes["! & Banner~"] = { 
    "symbol" : "!",
    "color" : "R",
    "type" : LITE,
    "subtype" : 9,
    "pval" : 0,
    "depth" : 1,
    "rarity" : 0,
    "weight" : 10,
    "cost" : 10000,
    "rarity1" : "uncommon60",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["~ & Iron Spike~"] = { 
    "symbol" : "~",
    "color" : "W",
    "type" : SPIKE,
    "subtype" : 0,
    "pval" : 0,
    "depth" : 1,
    "rarity" : 0,
    "weight" : 10,
    "cost" : 1,
    "rarity1" : "common1",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! & Flask~ of oil"] = { 
    "symbol" : "!",
    "color" : "y",
    "type" : FLASK,
    "subtype" : 0,
    "pval" : 7500,
    "depth" : 1,
    "rarity" : 0,
    "weight" : 10,
    "cost" : 3,
    "rarity1" : "common1",
    "armor" : 0,
    "damage" : "2d6",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["! & Empty Bottle~"] = { 
    "symbol" : "!",
    "color" : "w",
    "type" : BOTTLE,
    "subtype" : 1,
    "pval" : 0,
    "depth" : 0,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 1,
    "rarity1" : "common0",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["- Havoc"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : ROD,
    "subtype" : 28,
    "pval" : 0,
    "depth" : 95,
    "rarity" : 0,
    "weight" : 15,
    "cost" : 150000,
    "rarity1" : "not_so_rare100",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["- Door/Stair Location"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : ROD,
    "subtype" : 1,
    "pval" : 0,
    "depth" : 15,
    "rarity" : 0,
    "weight" : 15,
    "cost" : 1000,
    "rarity1" : "common15",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["- Trap Location"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : ROD,
    "subtype" : 0,
    "pval" : 0,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 15,
    "cost" : 100,
    "rarity1" : "common5",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["- Probing"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : ROD,
    "subtype" : 7,
    "pval" : 0,
    "depth" : 40,
    "rarity" : 0,
    "weight" : 15,
    "cost" : 4000,
    "rarity1" : "uncommon40",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["- Recall"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : ROD,
    "subtype" : 3,
    "pval" : 0,
    "depth" : 30,
    "rarity" : 0,
    "weight" : 15,
    "cost" : 4000,
    "rarity1" : "uncommon30",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["- Illumination"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : ROD,
    "subtype" : 4,
    "pval" : 0,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 15,
    "cost" : 1000,
    "rarity1" : "common20",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["- Light"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : ROD,
    "subtype" : 15,
    "pval" : 0,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 15,
    "cost" : 500,
    "rarity1" : "common10",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["- Lightning Bolts"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : ROD,
    "subtype" : 21,
    "pval" : 0,
    "depth" : 20,
    "rarity" : 0,
    "weight" : 15,
    "cost" : 2000,
    "rarity1" : "common20",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["- Frost Bolts"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : ROD,
    "subtype" : 23,
    "pval" : 0,
    "depth" : 25,
    "rarity" : 0,
    "weight" : 15,
    "cost" : 2500,
    "rarity1" : "common25",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["- Fire Bolts"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : ROD,
    "subtype" : 22,
    "pval" : 0,
    "depth" : 30,
    "rarity" : 0,
    "weight" : 15,
    "cost" : 3000,
    "rarity1" : "common30",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["- Polymorph"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : ROD,
    "subtype" : 19,
    "pval" : 0,
    "depth" : 35,
    "rarity" : 0,
    "weight" : 15,
    "cost" : 1200,
    "rarity1" : "common35",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["- Slow Monster"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : ROD,
    "subtype" : 17,
    "pval" : 0,
    "depth" : 30,
    "rarity" : 0,
    "weight" : 15,
    "cost" : 1500,
    "rarity1" : "common30",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["- Sleep Monster"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : ROD,
    "subtype" : 16,
    "pval" : 0,
    "depth" : 30,
    "rarity" : 0,
    "weight" : 15,
    "cost" : 1500,
    "rarity1" : "common30",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["- Drain Life"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : ROD,
    "subtype" : 18,
    "pval" : 0,
    "depth" : 75,
    "rarity" : 0,
    "weight" : 15,
    "cost" : 3600,
    "rarity1" : "uncommon75",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["- Teleport Other"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : ROD,
    "subtype" : 13,
    "pval" : 0,
    "depth" : 45,
    "rarity" : 0,
    "weight" : 15,
    "cost" : 1400,
    "rarity1" : "not_so_common45",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["- Disarming"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : ROD,
    "subtype" : 14,
    "pval" : 0,
    "depth" : 35,
    "rarity" : 0,
    "weight" : 15,
    "cost" : 2100,
    "rarity1" : "common35",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["- Lightning Balls"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : ROD,
    "subtype" : 25,
    "pval" : 0,
    "depth" : 55,
    "rarity" : 0,
    "weight" : 15,
    "cost" : 4000,
    "rarity1" : "common55",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["- Cold Balls"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : ROD,
    "subtype" : 27,
    "pval" : 0,
    "depth" : 60,
    "rarity" : 0,
    "weight" : 15,
    "cost" : 4500,
    "rarity1" : "common60",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["- Fire Balls"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : ROD,
    "subtype" : 26,
    "pval" : 0,
    "depth" : 75,
    "rarity" : 0,
    "weight" : 15,
    "cost" : 5000,
    "rarity1" : "common75",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["- Acid Balls"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : ROD,
    "subtype" : 24,
    "pval" : 0,
    "depth" : 70,
    "rarity" : 0,
    "weight" : 15,
    "cost" : 5500,
    "rarity1" : "common70",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["- Acid Bolts"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : ROD,
    "subtype" : 20,
    "pval" : 0,
    "depth" : 40,
    "rarity" : 0,
    "weight" : 15,
    "cost" : 3500,
    "rarity1" : "common40",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["- Enlightenment"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : ROD,
    "subtype" : 5,
    "pval" : 0,
    "depth" : 65,
    "rarity" : 0,
    "weight" : 15,
    "cost" : 10000,
    "rarity1" : "uncommon65",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["- Perception"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : ROD,
    "subtype" : 2,
    "pval" : 0,
    "depth" : 50,
    "rarity" : 0,
    "weight" : 15,
    "cost" : 13000,
    "rarity1" : "not_so_rare50",
    "rarity2" : "not_so_rare100",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["- Curing"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : ROD,
    "subtype" : 8,
    "pval" : 0,
    "depth" : 65,
    "rarity" : 0,
    "weight" : 15,
    "cost" : 15000,
    "rarity1" : "not_so_rare65",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["- Healing"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : ROD,
    "subtype" : 9,
    "pval" : 0,
    "depth" : 80,
    "rarity" : 0,
    "weight" : 15,
    "cost" : 20000,
    "rarity1" : "not_so_rare80",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["- Detection"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : ROD,
    "subtype" : 6,
    "pval" : 0,
    "depth" : 30,
    "rarity" : 0,
    "weight" : 15,
    "cost" : 5000,
    "rarity1" : "not_so_rare30",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["- Restoration"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : ROD,
    "subtype" : 10,
    "pval" : 0,
    "depth" : 80,
    "rarity" : 0,
    "weight" : 15,
    "cost" : 25000,
    "rarity1" : "rare80",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["- Speed"] = { 
    "symbol" : "-",
    "color" : "d",
    "type" : ROD,
    "subtype" : 11,
    "pval" : 0,
    "depth" : 95,
    "rarity" : 0,
    "weight" : 15,
    "cost" : 50000,
    "rarity1" : "rare95",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["~ & Shard~ of Pottery"] = { 
    "symbol" : "~",
    "color" : "r",
    "type" : JUNK,
    "subtype" : 3,
    "pval" : 0,
    "depth" : 0,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 0,
    "rarity1" : "common0",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["~ & Broken Stick~"] = { 
    "symbol" : "~",
    "color" : "r",
    "type" : JUNK,
    "subtype" : 6,
    "pval" : 0,
    "depth" : 0,
    "rarity" : 0,
    "weight" : 3,
    "cost" : 0,
    "rarity1" : "common0",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["~ & Broken Skull~"] = { 
    "symbol" : "~",
    "color" : "w",
    "type" : SKELETON,
    "subtype" : 1,
    "pval" : 0,
    "depth" : 0,
    "rarity" : 0,
    "weight" : 1,
    "cost" : 0,
    "rarity1" : "common0",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["~ & Broken Bone~"] = { 
    "symbol" : "~",
    "color" : "w",
    "type" : SKELETON,
    "subtype" : 2,
    "pval" : 0,
    "depth" : 0,
    "rarity" : 0,
    "weight" : 2,
    "cost" : 0,
    "rarity1" : "common0",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["~ & Femur~"] = { 
    "symbol" : "~",
    "color" : "w",
    "type" : SKELETON,
    "subtype" : 4,
    "pval" : 0,
    "depth" : 1,
    "rarity" : 0,
    "weight" : 10,
    "cost" : 0,
    "rarity1" : "common1",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["~ & Humerus~"] = { 
    "symbol" : "~",
    "color" : "w",
    "type" : SKELETON,
    "subtype" : 3,
    "pval" : 0,
    "depth" : 1,
    "rarity" : 0,
    "weight" : 10,
    "cost" : 0,
    "rarity1" : "common1",
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["~ & Skeletal Hand~"] = { 
    "symbol" : "~",
    "color" : "w",
    "type" : SKELETON,
    "subtype" : 8,
    "pval" : 0,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 60,
    "cost" : 0,
    "rarity1" : "common5",
    "armor" : 0,
    "damage" : "1d2",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["~ & Tibia~"] = { 
    "symbol" : "~",
    "color" : "w",
    "type" : SKELETON,
    "subtype" : 7,
    "pval" : 0,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 50,
    "cost" : 0,
    "rarity1" : "common5",
    "armor" : 0,
    "damage" : "1d2",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["~ & Skeleton~"] = { 
    "symbol" : "~",
    "color" : "w",
    "type" : SKELETON,
    "subtype" : 6,
    "pval" : 0,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 40,
    "cost" : 0,
    "rarity1" : "common5",
    "armor" : 0,
    "damage" : "1d2",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["~ & Rib~"] = { 
    "symbol" : "~",
    "color" : "w",
    "type" : SKELETON,
    "subtype" : 5,
    "pval" : 0,
    "depth" : 5,
    "rarity" : 0,
    "weight" : 30,
    "cost" : 0,
    "rarity1" : "common5",
    "armor" : 0,
    "damage" : "1d2",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["[ Black Dragon Scale Mail~"] = { 
    "symbol" : "[",
    "color" : "s",
    "type" : DRAGON_ARMOR,
    "subtype" : 1,
    "pval" : 0,
    "depth" : 60,
    "rarity" : 0,
    "weight" : 200,
    "cost" : 30000,
    "rarity1" : "not_so_rare60",
    "armor" : 30,
    "damage" : "2d4",
    "hitbonus" : -2,
    "damnonus" : 0,
    "acbonus" : 10,
    "RES_ACID" : true,
    "ACTIVATE" : true,
    "IGNORE_ACID" : true,
    "IGNORE_ELEC" : true,
    "IGNORE_FIRE" : true,
    "IGNORE_COLD" : true
  };

  gizmos.prototypes["[ Blue Dragon Scale Mail~"] = { 
    "symbol" : "[",
    "color" : "b",
    "type" : DRAGON_ARMOR,
    "subtype" : 2,
    "pval" : 0,
    "depth" : 40,
    "rarity" : 0,
    "weight" : 200,
    "cost" : 35000,
    "rarity1" : "not_so_rare40",
    "armor" : 30,
    "damage" : "2d4",
    "hitbonus" : -2,
    "damnonus" : 0,
    "acbonus" : 10,
    "RES_ELEC" : true,
    "ACTIVATE" : true,
    "IGNORE_ACID" : true,
    "IGNORE_ELEC" : true,
    "IGNORE_FIRE" : true,
    "IGNORE_COLD" : true
  };

  gizmos.prototypes["[ White Dragon Scale Mail~"] = { 
    "symbol" : "[",
    "color" : "w",
    "type" : DRAGON_ARMOR,
    "subtype" : 3,
    "pval" : 0,
    "depth" : 50,
    "rarity" : 0,
    "weight" : 200,
    "cost" : 40000,
    "rarity1" : "not_so_rare50",
    "armor" : 30,
    "damage" : "2d4",
    "hitbonus" : -2,
    "damnonus" : 0,
    "acbonus" : 10,
    "RES_COLD" : true,
    "ACTIVATE" : true,
    "IGNORE_ACID" : true,
    "IGNORE_ELEC" : true,
    "IGNORE_FIRE" : true,
    "IGNORE_COLD" : true
  };

  gizmos.prototypes["[ Red Dragon Scale Mail~"] = { 
    "symbol" : "[",
    "color" : "r",
    "type" : DRAGON_ARMOR,
    "subtype" : 4,
    "pval" : 0,
    "depth" : 80,
    "rarity" : 0,
    "weight" : 200,
    "cost" : 100000,
    "rarity1" : "not_so_rare80",
    "armor" : 30,
    "damage" : "2d4",
    "hitbonus" : -2,
    "damnonus" : 0,
    "acbonus" : 10,
    "RES_FIRE" : true,
    "ACTIVATE" : true,
    "IGNORE_ACID" : true,
    "IGNORE_ELEC" : true,
    "IGNORE_FIRE" : true,
    "IGNORE_COLD" : true
  };

  gizmos.prototypes["[ Green Dragon Scale Mail~"] = { 
    "symbol" : "[",
    "color" : "g",
    "type" : DRAGON_ARMOR,
    "subtype" : 5,
    "pval" : 0,
    "depth" : 70,
    "rarity" : 0,
    "weight" : 200,
    "cost" : 80000,
    "rarity1" : "not_so_rare70",
    "armor" : 30,
    "damage" : "2d4",
    "hitbonus" : -2,
    "damnonus" : 0,
    "acbonus" : 10,
    "RES_POIS" : true,
    "ACTIVATE" : true,
    "IGNORE_ACID" : true,
    "IGNORE_ELEC" : true,
    "IGNORE_FIRE" : true,
    "IGNORE_COLD" : true
  };

  gizmos.prototypes["[ Multi-Hued Dragon Scale Mail~"] = { 
    "symbol" : "[",
    "color" : "v",
    "type" : DRAGON_ARMOR,
    "subtype" : 6,
    "pval" : 0,
    "depth" : 100,
    "rarity" : 0,
    "weight" : 200,
    "cost" : 150000,
    "rarity1" : "rare_100",
    "armor" : 30,
    "damage" : "2d4",
    "hitbonus" : -2,
    "damnonus" : 0,
    "acbonus" : 10,
    "RES_ACID" : true,
    "RES_ELEC" : true,
    "RES_FIRE" : true,
    "RES_COLD" : true,
    "RES_POIS" : true,
    "ACTIVATE" : true,
    "IGNORE_ACID" : true,
    "IGNORE_ELEC" : true,
    "IGNORE_FIRE" : true,
    "IGNORE_COLD" : true
  };

  gizmos.prototypes["[ Pseudo Dragon Scale Mail~"] = { 
    "symbol" : "[",
    "color" : "v",
    "type" : DRAGON_ARMOR,
    "subtype" : 10,
    "pval" : 0,
    "depth" : 65,
    "rarity" : 0,
    "weight" : 200,
    "cost" : 60000,
    "rarity1" : "rare_65",
    "armor" : 30,
    "damage" : "2d4",
    "hitbonus" : -2,
    "damnonus" : 0,
    "acbonus" : 10,
    "RES_LITE" : true,
    "RES_DARK" : true,
    "ACTIVATE" : true,
    "IGNORE_ACID" : true,
    "IGNORE_ELEC" : true,
    "IGNORE_FIRE" : true,
    "IGNORE_COLD" : true
  };

  gizmos.prototypes["[ Law Dragon Scale Mail~"] = { 
    "symbol" : "[",
    "color" : "B",
    "type" : DRAGON_ARMOR,
    "subtype" : 12,
    "pval" : 0,
    "depth" : 80,
    "rarity" : 0,
    "weight" : 200,
    "cost" : 80000,
    "rarity1" : "rare_80",
    "armor" : 30,
    "damage" : "2d4",
    "hitbonus" : -2,
    "damnonus" : 0,
    "acbonus" : 10,
    "RES_SOUND" : true,
    "RES_SHARDS" : true,
    "ACTIVATE" : true,
    "IGNORE_ACID" : true,
    "IGNORE_ELEC" : true,
    "IGNORE_FIRE" : true,
    "IGNORE_COLD" : true
  };

  gizmos.prototypes["[ Bronze Dragon Scale Mail~"] = { 
    "symbol" : "[",
    "color" : "U",
    "type" : DRAGON_ARMOR,
    "subtype" : 14,
    "pval" : 0,
    "depth" : 55,
    "rarity" : 0,
    "weight" : 200,
    "cost" : 30000,
    "rarity1" : "not_so_rare55",
    "armor" : 30,
    "damage" : "2d4",
    "hitbonus" : -2,
    "damnonus" : 0,
    "acbonus" : 10,
    "RES_CONF" : true,
    "ACTIVATE" : true,
    "IGNORE_ACID" : true,
    "IGNORE_ELEC" : true,
    "IGNORE_FIRE" : true,
    "IGNORE_COLD" : true
  };

  gizmos.prototypes["[ Gold Dragon Scale Mail~"] = { 
    "symbol" : "[",
    "color" : "y",
    "type" : DRAGON_ARMOR,
    "subtype" : 16,
    "pval" : 0,
    "depth" : 65,
    "rarity" : 0,
    "weight" : 200,
    "cost" : 40000,
    "rarity1" : "not_so_rare65",
    "armor" : 30,
    "damage" : "2d4",
    "hitbonus" : -2,
    "damnonus" : 0,
    "acbonus" : 10,
    "RES_SOUND" : true,
    "ACTIVATE" : true,
    "IGNORE_ACID" : true,
    "IGNORE_ELEC" : true,
    "IGNORE_FIRE" : true,
    "IGNORE_COLD" : true
  };

  gizmos.prototypes["[ Chaos Dragon Scale Mail~"] = { 
    "symbol" : "[",
    "color" : "v",
    "type" : DRAGON_ARMOR,
    "subtype" : 18,
    "pval" : 0,
    "depth" : 75,
    "rarity" : 0,
    "weight" : 200,
    "cost" : 70000,
    "rarity1" : "rare_75",
    "armor" : 30,
    "damage" : "2d4",
    "hitbonus" : -2,
    "damnonus" : 0,
    "acbonus" : 10,
    "RES_CHAOS" : true,
    "RES_DISEN" : true,
    "ACTIVATE" : true,
    "IGNORE_ACID" : true,
    "IGNORE_ELEC" : true,
    "IGNORE_FIRE" : true,
    "IGNORE_COLD" : true
  };

  gizmos.prototypes["[ Balance Dragon Scale Mail~"] = { 
    "symbol" : "[",
    "color" : "v",
    "type" : DRAGON_ARMOR,
    "subtype" : 20,
    "pval" : 0,
    "depth" : 90,
    "rarity" : 0,
    "weight" : 200,
    "cost" : 100000,
    "rarity1" : "rare_90",
    "armor" : 30,
    "damage" : "2d4",
    "hitbonus" : -2,
    "damnonus" : 0,
    "acbonus" : 10,
    "RES_CHAOS" : true,
    "RES_DISEN" : true,
    "RES_SOUND" : true,
    "RES_SHARDS" : true,
    "ACTIVATE" : true,
    "IGNORE_ACID" : true,
    "IGNORE_ELEC" : true,
    "IGNORE_FIRE" : true,
    "IGNORE_COLD" : true
  };

  gizmos.prototypes["[ Power Dragon Scale Mail~"] = { 
    "symbol" : "[",
    "color" : "v",
    "type" : DRAGON_ARMOR,
    "subtype" : 30,
    "pval" : 0,
    "depth" : 110,
    "rarity" : 0,
    "weight" : 250,
    "cost" : 350000,
    "rarity1" : "ultra_rare_110",
    "armor" : 40,
    "damage" : "2d4",
    "hitbonus" : -3,
    "damnonus" : 0,
    "acbonus" : 15,
    "RES_ACID" : true,
    "RES_FIRE" : true,
    "RES_COLD" : true,
    "RES_ELEC" : true,
    "RES_POIS" : true,
    "RES_NETHER" : true,
    "RES_NEXUS" : true,
    "RES_CHAOS" : true,
    "RES_LITE" : true,
    "RES_DARK" : true,
    "RES_SHARDS" : true,
    "RES_SOUND" : true,
    "RES_DISEN" : true,
    "RES_CONF" : true,
    "ACTIVATE" : true,
    "IGNORE_ACID" : true,
    "IGNORE_ELEC" : true,
    "IGNORE_FIRE" : true,
    "IGNORE_COLD" : true
  };

  gizmos.prototypes["$ copper"] = { 
    "symbol" : "$",
    "color" : "u",
    "type" : GOLD,
    "subtype" : 1,
    "pval" : 0,
    "depth" : 1,
    "rarity" : 0,
    "weight" : 0,
    "cost" : 3
  };

  gizmos.prototypes["$ copper"] = { 
    "symbol" : "$",
    "color" : "u",
    "type" : GOLD,
    "subtype" : 2,
    "pval" : 0,
    "depth" : 1,
    "rarity" : 0,
    "weight" : 0,
    "cost" : 4
  };

  gizmos.prototypes["$ copper"] = { 
    "symbol" : "$",
    "color" : "u",
    "type" : GOLD,
    "subtype" : 3,
    "pval" : 0,
    "depth" : 1,
    "rarity" : 0,
    "weight" : 0,
    "cost" : 5
  };

  gizmos.prototypes["$ silver"] = { 
    "symbol" : "$",
    "color" : "s",
    "type" : GOLD,
    "subtype" : 4,
    "pval" : 0,
    "depth" : 1,
    "rarity" : 0,
    "weight" : 0,
    "cost" : 6
  };

  gizmos.prototypes["$ silver"] = { 
    "symbol" : "$",
    "color" : "s",
    "type" : GOLD,
    "subtype" : 5,
    "pval" : 0,
    "depth" : 1,
    "rarity" : 0,
    "weight" : 0,
    "cost" : 7
  };

  gizmos.prototypes["$ silver"] = { 
    "symbol" : "$",
    "color" : "s",
    "type" : GOLD,
    "subtype" : 6,
    "pval" : 0,
    "depth" : 1,
    "rarity" : 0,
    "weight" : 0,
    "cost" : 8
  };

  gizmos.prototypes["$ garnets"] = { 
    "symbol" : "$",
    "color" : "r",
    "type" : GOLD,
    "subtype" : 7,
    "pval" : 0,
    "depth" : 1,
    "rarity" : 0,
    "weight" : 0,
    "cost" : 9
  };

  gizmos.prototypes["$ garnets"] = { 
    "symbol" : "$",
    "color" : "r",
    "type" : GOLD,
    "subtype" : 8,
    "pval" : 0,
    "depth" : 1,
    "rarity" : 0,
    "weight" : 0,
    "cost" : 10
  };

  gizmos.prototypes["$ gold"] = { 
    "symbol" : "$",
    "color" : "y",
    "type" : GOLD,
    "subtype" : 9,
    "pval" : 0,
    "depth" : 1,
    "rarity" : 0,
    "weight" : 0,
    "cost" : 12
  };

  gizmos.prototypes["$ gold"] = { 
    "symbol" : "$",
    "color" : "y",
    "type" : GOLD,
    "subtype" : 10,
    "pval" : 0,
    "depth" : 1,
    "rarity" : 0,
    "weight" : 0,
    "cost" : 14
  };

  gizmos.prototypes["$ gold"] = { 
    "symbol" : "$",
    "color" : "y",
    "type" : GOLD,
    "subtype" : 11,
    "pval" : 0,
    "depth" : 1,
    "rarity" : 0,
    "weight" : 0,
    "cost" : 16
  };

  gizmos.prototypes["$ opals"] = { 
    "symbol" : "$",
    "color" : "W",
    "type" : GOLD,
    "subtype" : 12,
    "pval" : 0,
    "depth" : 1,
    "rarity" : 0,
    "weight" : 0,
    "cost" : 18
  };

  gizmos.prototypes["$ sapphires"] = { 
    "symbol" : "$",
    "color" : "b",
    "type" : GOLD,
    "subtype" : 13,
    "pval" : 0,
    "depth" : 1,
    "rarity" : 0,
    "weight" : 0,
    "cost" : 20
  };

  gizmos.prototypes["$ rubies"] = { 
    "symbol" : "$",
    "color" : "r",
    "type" : GOLD,
    "subtype" : 14,
    "pval" : 0,
    "depth" : 1,
    "rarity" : 0,
    "weight" : 0,
    "cost" : 24
  };

  gizmos.prototypes["$ diamonds"] = { 
    "symbol" : "$",
    "color" : "w",
    "type" : GOLD,
    "subtype" : 15,
    "pval" : 0,
    "depth" : 1,
    "rarity" : 0,
    "weight" : 0,
    "cost" : 28
  };

  gizmos.prototypes["$ emeralds"] = { 
    "symbol" : "$",
    "color" : "g",
    "type" : GOLD,
    "subtype" : 16,
    "pval" : 0,
    "depth" : 1,
    "rarity" : 0,
    "weight" : 0,
    "cost" : 32
  };

  gizmos.prototypes["$ topaz"] = { 
    "symbol" : "$",
    "color" : "B",
    "type" : GOLD,
    "subtype" : 17,
    "pval" : 0,
    "depth" : 1,
    "rarity" : 0,
    "weight" : 0,
    "cost" : 40
  };

  gizmos.prototypes["$ adamantite"] = { 
    "symbol" : "$",
    "color" : "G",
    "type" : GOLD,
    "subtype" : 18,
    "pval" : 0,
    "depth" : 1,
    "rarity" : 0,
    "weight" : 0,
    "cost" : 80
  };

  gizmos.prototypes["$ bronze"] = { 
    "symbol" : "$",
    "color" : "r",
    "type" : GOLD,
    "subtype" : 19,
    "pval" : 0,
    "depth" : 1,
    "rarity" : 0,
    "weight" : 0,
    "cost" : 10
  };

  gizmos.prototypes["\\ & Mighty Hammer~"] = { 
    "symbol" : "\\",
    "color" : "D",
    "type" : HAFTED,
    "subtype" : 50,
    "pval" : 0,
    "depth" : 15,
    "rarity" : 0,
    "weight" : 200,
    "cost" : 1000,
    "armor" : 0,
    "damage" : "3d9",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "SHOW_MODS" : true,
    "INSTA_ART" : true
  };

  gizmos.prototypes["] & Lead Crown~"] = { 
    "symbol" : "]",
    "color" : "D",
    "type" : CROWN,
    "subtype" : 50,
    "pval" : 0,
    "depth" : 44,
    "rarity" : 0,
    "weight" : 20,
    "cost" : 1000,
    "armor" : 0,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0,
    "INSTA_ART" : true
  };

  gizmos.prototypes["] & Set~ of Leather Gloves"] = { 
    "symbol" : "]",
    "color" : "U",
    "type" : GLOVES,
    "subtype" : 1,
    "pval" : 0,
    "depth" : 1,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 3,
    "rarity1" : "common1",
    "armor" : 1,
    "damage" : "0d0",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["] & Set~ of Gauntlets"] = { 
    "symbol" : "]",
    "color" : "U",
    "type" : GLOVES,
    "subtype" : 2,
    "pval" : 0,
    "depth" : 10,
    "rarity" : 0,
    "weight" : 25,
    "cost" : 35,
    "rarity1" : "common10",
    "armor" : 2,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["] & Set~ of Cesti"] = { 
    "symbol" : "]",
    "color" : "W",
    "type" : GLOVES,
    "subtype" : 5,
    "pval" : 0,
    "depth" : 50,
    "rarity" : 0,
    "weight" : 40,
    "cost" : 100,
    "rarity1" : "common50",
    "armor" : 5,
    "damage" : "1d1",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["( & Cloak~"] = { 
    "symbol" : "(",
    "color" : "g",
    "type" : CLOAK,
    "subtype" : 1,
    "pval" : 0,
    "depth" : 1,
    "rarity" : 0,
    "weight" : 10,
    "cost" : 3,
    "rarity1" : "common1",
    "rarity2" : "common20",
    "armor" : 1,
    "damage" : "0d0",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 0
  };

  gizmos.prototypes["( & Shadow Cloak~"] = { 
    "symbol" : "(",
    "color" : "D",
    "type" : CLOAK,
    "subtype" : 6,
    "pval" : 0,
    "depth" : 60,
    "rarity" : 0,
    "weight" : 5,
    "cost" : 7500,
    "rarity1" : "uncommon75",
    "armor" : 6,
    "damage" : "0d0",
    "hitbonus" : 0,
    "damnonus" : 0,
    "acbonus" : 4,
    "RES_DARK" : true,
    "RES_LITE" : true
  };


}

