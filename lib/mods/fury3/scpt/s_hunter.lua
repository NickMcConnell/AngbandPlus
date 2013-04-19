-- Hunter Abilities
function get_hfirebeam_dam()
	return 3 + get_level(HUNTER_FLAMEBEAM, 20), 1 + get_level(HUNTER_FLAMEBEAM, 15)
end
HUNTER_FLAMEBEAM = add_spell
{
	["name"] = 	"Flamethrower",
	["school"] = 	SCHOOL_HUNTING,
	["level"] = 	1,
	["mana"] = 	1,
	["fail"] = 	10,
		["random"] =    0,
	["spell"] = 	function()
			local ret, dir	
			ret, dir = get_aim_dir()
			if ret == FALSE then return end
			return fire_beam(GF_FIRE, dir, damroll(get_hfirebeam_dam()))
	              end,
	["info"] = 	function()
			
			return ""
	end,
	["desc"] =	{
			"Fires a beam of Fire",
			}

				
}
function get_hpoisdart_dam()
	return 3 + get_level(HUNTER_POISDART, 20), 1 + get_level(HUNTER_POISDART, 15)
end
HUNTER_POISDART = add_spell
{
	["name"] = 	"Poison Darts",
	["school"] = 	SCHOOL_HUNTING,
	["level"] = 	5,
	["mana"] = 	5,
		["random"] =    0,
	["fail"] = 	10,
	["spell"] = 	function()
			local ret, dir	
			ret, dir = get_aim_dir()
			if ret == FALSE then return end
			return fire_bolt(GF_POIS, dir, damroll(get_hpoisdart_dam()))
	              end,
	["info"] = 	function()
			
			return ""
	end,
	["desc"] =	{
			"Launch a Poison Dart",
			}

				
}
function get_hflamethrow_dam()
	return 3 + get_level(HUNTER_FLAMETHROW, 20), 1 + get_level(HUNTER_FLAMETHROW, 15)
end
HUNTER_FLAMETHROW = add_spell
{
	["name"] = 	"Mega Flamethrower",
	["school"] = 	SCHOOL_HUNTING,
	["level"] = 	15,
	["mana"] = 	15,
		["random"] =    0,
	["fail"] = 	10,
	["spell"] = 	function()
			local ret, dir	
			ret, dir = get_aim_dir()
			if ret == FALSE then return end
			return fire_ball_beam(GF_HOLY_FIRE, dir, damroll(get_hflamethrow_dam()), 4)
	              end,
	["info"] = 	function()
			
			return ""
	end,
	["desc"] =	{
			"Fires a small burst of Fire",
			}

				
}

HUNTER_ROCKET = add_spell
{
	["name"] = 	"Rocket Launcher",
	["school"] = 	SCHOOL_HUNTING,
	["level"] = 	10,
	["mana"] = 	10,
	["fail"] = 	10,
		["random"] =    0,
	["spell"] = 	function()
			local ret, dir	
			ret, dir = get_aim_dir()
			if ret == FALSE then return end
			return fire_ball(GF_ROCKET, dir, 20 + get_level(HUNTER_ROCKET, 500), 2 + get_level(HUNTER_ROCKET, 5))
	              end,
	["info"] = 	function()
			
			return ""
	end,
	["desc"] =	{
			"Launches a Rocket",
			}

				
}

HUNTER_MISSILE = add_spell
{
	["name"] = 	"Missile Launcher",
	["school"] = 	SCHOOL_HUNTING,
	["level"] = 	25,
	["mana"] = 	25,
	["fail"] = 	10,
		["random"] =    0,
	["spell"] = 	function()
			local ret, dir	
			ret, dir = get_aim_dir()
			if ret == FALSE then return end
			return fire_ball(GF_MISSILE, dir, 20 + get_level(HUNTER_MISSILE, 500), 2 + get_level(HUNTER_MISSILE, 5))
	              end,
	["info"] = 	function()
			
			return ""
	end,
	["desc"] =	{
			"Launches a Missile",
			}

				
}

HUNTER_NUKE = add_spell
{
	["name"] = 	"Mini-Nuke",
	["school"] = 	SCHOOL_HUNTING,
	["level"] = 	50,
	["mana"] = 	100,
	["fail"] = 	10,
		["random"] =    0,
	["spell"] = 	function()
			local ret, dir	
			ret, dir = get_aim_dir()
			if ret == FALSE then return end
			return fire_ball(GF_NUKE, dir, 20 + get_level(HUNTER_NUKE, 500), 2 + get_level(HUNTER_NUKE, 5))
	              end,
	["info"] = 	function()
			
			return ""
	end,
	["desc"] =	{
			"Launches a Nuke",
			}

				
}

HSENSEMONSTERS = add_spell
{
	["name"] = 	"Detect Monsters",
	["school"] = 	{SCHOOL_HUNTING},
	["level"] = 	1,
	["mana"] =      1,
	["mana_max"] =  20,
		["random"] =    0,
	["fail"] = 	10,
	["inertia"] = 	{ 1, 10 },
	["spell"] = 	function()
			local obvious
			obvious = detect_monsters_normal(10 + get_level(HSENSEMONSTERS, 40, 0))
			if get_level(HSENSEMONSTERS, 50) >= 30 then
				obvious = is_obvious(set_tim_esp(10 + randint(10) + get_level(HSENSEMONSTERS, 20)), obvious)
			end
			return obvious
	end,
	["info"] = 	function()
			if get_level(HSENSEMONSTERS, 50) >= 30 then
				return "rad "..(10 + get_level(HSENSEMONSTERS, 40)).." dur "..(10 + get_level(HSENSEMONSTERS, 20)).."+d10"
			else
				return "rad "..(10 + get_level(HSENSEMONSTERS, 40))
			end
	end,
	["desc"] =	{
			"Detects all monsters near you",
			"At level 30 it allows you to sense monster minds for a while"
	}
}

HSENSEHIDDEN = add_spell
{
	["name"] = 	"Detect Hidden",
	["school"] = 	{SCHOOL_HUNTING},
	["level"] = 	5,
	["mana"] = 	2,
		["random"] =    0,
	["mana_max"] = 	10,
	["fail"] = 	25,
	["inertia"] = 	{ 1, 10 },
	["spell"] = 	function()
			local obvious = nil
			obvious = detect_traps(15 + get_level(HSENSEHIDDEN, 40, 0))
			if get_level(HSENSEHIDDEN, 50) >= 15 then
				obvious = is_obvious(set_tim_invis(10 + randint(20) + get_level(HSENSEHIDDEN, 40)), obvious)
			end
			return obvious
	end,
	["info"] = 	function()
			if get_level(HSENSEHIDDEN, 50) >= 15 then
				return "rad "..(15 + get_level(HSENSEHIDDEN, 40)).." dur "..(10 + get_level(HSENSEHIDDEN, 40)).."+d20"
			else
				return "rad "..(15 + get_level(HSENSEHIDDEN, 40))
			end
	end,
	["desc"] =	{
			"Detects the traps in a certain radius around you",
			"At level 15 it allows you to sense invisible for a while"
	}
}
