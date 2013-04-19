add_module
{
	["name"]    = "FuryBand T",
	["version"] = { 5, 1, 0 },
	["author"]  = { "The Fury", "thefury@furytech.net" },
	["desc"] = {
		"This version is closer to Angband in that there's only one town and one dungeon.",
		},

	["rand_quest"] = TRUE,
	["C_quest"] = FALSE,

	["base_dungeon"] = 4,
	["death_dungeon"] = 28,

	 ["astral_dungeon"] = 4,
        ["astral_wild_x"] = 35,
        ["astral_wild_y"] = 21,

	["random_artifact_weapon_chance"] = 33,
	["random_artifact_armor_chance"] = 33,
	["random_artifact_jewelry_chance"] = 34,

	["max_plev"] = 75,
	["max_skill_overage"] = 5,
	
	["mod_savefiles"]=
	{
		"FuryBand T",
	},
	["layout"] = 
	{ 
	["apex"] = "fury2", 
	["core"] = "fury2",
	["data"] = "fury2", 
	["dngn"] = "fury2",
	["edit"] = "fury2", 
	["file"] = "fury2",
	["help"] = "fury2",
	["note"] = "fury2",
	["save"] = "fury2",
	["scpt"] = "fury2",
	["user"] = "fury2",
	["pref"] = "fury2",
	}, 
}