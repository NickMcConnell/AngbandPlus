add_module
{
	["name"]    = "Town Version",
	["version"] = { 3, 0, 0 },
	["author"]  = { "The Fury", "thefurry@hvc.rr.com" },
	["desc"] = {
		"The alternate mode of FuryBand, one town, one dungeon",
		},

	["rand_quest"] = TRUE,
	["C_quest"] = FALSE,

	["base_dungeon"] = 4,
	["death_dungeon"] = 28,

	 ["astral_dungeon"] = 1,
        ["astral_wild_x"] = 1,
        ["astral_wild_y"] = 1,

	["random_artifact_weapon_chance"] = 40,
	["random_artifact_armor_chance"] = 40,
	["random_artifact_jewelry_chance"] = 10,

	["max_plev"] = 50,
	["max_skill_overage"] = 7,
	

	["mod_savefiles"]=
	{
		"Town Version",
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