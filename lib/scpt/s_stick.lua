-- Spells that are stick or artifacts/... only

DEVICE_HEAL_MONSTER = add_spell
{
	["name"] = 	"Heal Monster",
        ["school"] = 	{SCHOOL_DEVICE},
        ["level"] = 	3,
        ["mana"] = 	5,
        ["mana_max"] = 	20,
        ["fail"] = 	15,
        ["random"] =    -1,
        ["stick"] =
        {
                        ["charge"] =    { 10, 10 },
                        [TV_WAND] =
                        {
                                ["rarity"] = 		17,
                                ["base_level"] =        { 1, 15 },
                                ["max_level"] =        	{ 20, 50 },
                        },
        },
        ["spell"] = 	function()
                        local ret, dir = get_aim_dir()
                        if ret == FALSE then return end

                        return fire_ball(GF_OLD_HEAL, dir, 20 + get_level(DEVICE_HEAL_MONSTER, 380), 0)
	end,
	["info"] = 	function()
                	return "heal "..(20 + get_level(DEVICE_HEAL_MONSTER, 380))
	end,
        ["desc"] =	{
        		"Heals a monster",
        }
}

DEVICE_SPEED_MONSTER = add_spell
{
	["name"] = 	"Haste Monster",
        ["school"] = 	{SCHOOL_DEVICE},
        ["level"] = 	10,
        ["mana"] = 	10,
        ["mana_max"] = 	10,
        ["fail"] = 	30,
        ["random"] =    -1,
        ["stick"] =
        {
                        ["charge"] =    { 10, 5 },
                        [TV_WAND] =
                        {
                                ["rarity"] = 		7,
                                ["base_level"] =        { 1, 1 },
                                ["max_level"] =        	{ 20, 50 },
                        },
        },
        ["spell"] = 	function()
                        local ret, dir = get_aim_dir()
                        if ret == FALSE then return end

                        return fire_ball(GF_OLD_SPEED, dir, 1, 0)
	end,
	["info"] = 	function()
                	return "speed +10"
	end,
        ["desc"] =	{
        		"Haste a monster",
        }
}

DEVICE_WISH = add_spell
{
	["name"] = 	"Wish",
        ["school"] = 	{SCHOOL_DEVICE},
        ["level"] = 	50,
        ["mana"] = 	400,
        ["mana_max"] = 	400,
        ["fail"] = 	99,
        ["random"] =    -1,
        ["stick"] =
        {
                        ["charge"] =    { 1, 2 },
                        [TV_STAFF] =
                        {
                                ["rarity"] = 		98,
                                ["base_level"] =        { 1, 1 },
                                ["max_level"] =        	{ 1, 1 },
                        },
        },
        ["spell"] = 	function()
                        make_wish()
                        return TRUE
	end,
	["info"] = 	function()
                	return ""
	end,
        ["desc"] =	{
        		"This grants you a wish, beware of what you ask for!",
        }
}

DEVICE_SUMMON = add_spell
{
	["name"] = 	"Summon",
        ["school"] = 	{SCHOOL_DEVICE},
        ["level"] = 	5,
        ["mana"] = 	5,
        ["mana_max"] = 	25,
        ["fail"] = 	20,
        ["random"] =    -1,
        ["stick"] =
        {
                        ["charge"] =    { 1, 20 },
                        [TV_STAFF] =
                        {
                                ["rarity"] = 		13,
                                ["base_level"] =        { 1, 40 },
                                ["max_level"] =        	{ 25, 50 },
                        },
        },
        ["spell"] = 	function()
                        local i, obvious
                        obvious = nil
                        for i = 1, 4 + get_level(DEVICE_SUMMON, 30) do
                                obvious = is_obvious(summon_specific(py, px, dun_level, 0), obvious)
                        end
                        return obvious
	end,
	["info"] = 	function()
                	return ""
	end,
        ["desc"] =	{
        		"Summons hostile monsters near you",
        }
}

DEVICE_MANA = add_spell
{
	["name"] = 	"Mana",
        ["school"] = 	{SCHOOL_DEVICE},
        ["level"] = 	30,
        ["mana"] = 	1,
        ["mana_max"] = 	1,
        ["fail"] = 	80,
        ["random"] =    -1,
        ["stick"] =
        {
                        ["charge"] =    { 2, 3 },
                        [TV_STAFF] =
                        {
                                ["rarity"] = 		78,
                                ["base_level"] =        { 1, 20 },
                                ["max_level"] =        	{ 20, 45 },
                        },
        },
        ["spell"] = 	function()
                        increase_mana((player.msp * (20 + get_level(DEVICE_MANA, 80))) / 100)
                        return TRUE
	end,
	["info"] = 	function()
                	return "restore "..(20 + get_level(DEVICE_MANA, 80)).."%"
	end,
        ["desc"] =	{
        		"Restores a part(or all) of your mana",
        }
}

DEVICE_NOTHING = add_spell
{
	["name"] = 	"Nothing",
        ["school"] = 	{SCHOOL_DEVICE},
        ["level"] = 	1,
        ["mana"] = 	0,
        ["mana_max"] = 	0,
        ["fail"] = 	0,
        ["random"] =    -1,
        ["stick"] =
        {
                        ["charge"] =    { 0, 0 },
                        [TV_WAND] =
                        {
                                ["rarity"] = 		3,
                                ["base_level"] =        { 1, 1 },
                                ["max_level"] =        	{ 1, 1 },
                        },
                        [TV_STAFF] =
                        {
                                ["rarity"] = 		3,
                                ["base_level"] =        { 1, 1 },
                                ["max_level"] =        	{ 1, 1},
                        },
        },
        ["spell"] = 	function()
                        return FALSE
	end,
	["info"] = 	function()
                	return ""
	end,
        ["desc"] =	{
        		"It does nothing.",
        }
}

DEVICE_LEBOHAUM = add_spell
{
	["name"] = 	"Artifact Lebauhaum",
        ["school"] = 	{SCHOOL_DEVICE},
        ["level"] = 	1,
        ["mana"] = 	0,
        ["mana_max"] = 	0,
        ["fail"] = 	0,
        ["random"] =    -1,
        ["activate"] =  3,
        ["spell"] = 	function()
        		msg_print("You hear a little song in your head: 'With the helm Lebohaum, your head is *SAFE* !'")
	end,
	["info"] = 	function()
                	return ""
	end,
        ["desc"] =	{
        		"sing a cheerfull song",
        }
}

DEVICE_MAGGOT = add_spell
{
	["name"] = 	"Artifact Maggot",
        ["school"] = 	{SCHOOL_DEVICE},
        ["level"] = 	1,
        ["mana"] = 	7,
        ["mana_max"] = 	7,
        ["fail"] = 	20,
        ["random"] =    -1,
        ["activate"] =  { 10, 50 },
        ["spell"] = 	function()
                        local ret, dir = get_aim_dir()
                        if ret == FALSE then return end
                        return fire_ball(GF_TURN_ALL, dir, 40, 2)
	end,
	["info"] = 	function()
                	return "power 40 rad 2"
	end,
        ["desc"] =	{
        		"terrify",
        }
}

DEVICE_HOLY_FIRE = add_spell
{
	["name"] = 	"Holy Fire of Mithrandir",
        ["school"] = 	{SCHOOL_DEVICE},
        ["level"] = 	30,
        ["mana"] = 	50,
        ["mana_max"] = 	150,
        ["fail"] = 	75,
        ["random"] =    -1,
        ["stick"] =
        {
                        ["charge"] =    { 2, 5 },
                        [TV_STAFF] =
                        {
                        	-- Rarity higher than 100 to be sure to not have it generated randomly
                                ["rarity"] = 		999,
                                ["base_level"] =        { 1, 1 },
                                ["max_level"] =        	{ 35, 35 },
                        },
        },
        ["spell"] = 	function()
        		return project_los(GF_HOLY_FIRE, 50 + get_level(DEVICE_HOLY_FIRE, 300))
	end,
	["info"] = 	function()
                	return "dam "..(50 + get_level(DEVICE_HOLY_FIRE, 250))
	end,
        ["desc"] =	{
        		"The Holy Fire created by this staff will deeply(double damage) burn",
                        "all that is evil.",
        }
}

--[[ Template
DEVICE_ = add_spell
{
	["name"] = 	"",
        ["school"] = 	{SCHOOL_DEVICE},
        ["level"] = 	1,
        ["mana"] = 	2,
        ["mana_max"] = 	15,
        ["fail"] = 	10,
        ["random"] =    -1,
        ["stick"] =
        {
                        ["charge"] =    { 10, 5 },
                        [TV_STAFF] =
                        {
                                ["rarity"] = 		7,
                                ["base_level"] =        { 1, 15 },
                                ["max_level"] =        	{ 25, 50 },
                        },
        },
        ["spell"] = 	function()
                        return FALSE
	end,
	["info"] = 	function()
                	return ""
	end,
        ["desc"] =	{
        		"",
        }
}
]]
