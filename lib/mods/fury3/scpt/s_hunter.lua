-- Hunter Abilities
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
			return fire_beam(GF_FIRE, dir, damroll(5 + get_level(HUNTER_FLAMEBEAM, 20), 1 + get_level(HUNTER_FLAMEBEAM, 15)))
	              end,
	["info"] = 	function()
			return "dam "..(5 + get_level(HUNTER_FLAMEBEAM, 20).."d"..(1 + get_level(HUNTER_FLAMEBEAM, 15)))
			
	end,
	["desc"] =	{
			"Fires a spray of fire from your wrist mounted flamethrower",
			}

				
}

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
			return fire_bolt(GF_POIS, dir, damroll(5 + get_level(HUNTER_POISDART, 25), 1 + get_level(HUNTER_POISDART, 20)))
	              end,
	["info"] = 	function()
			
			return "dam "..(5 + get_level(HUNTER_POISDART, 25).."d"..1 + (get_level(HUNTER_POISDART, 20)))
	end,
	["desc"] =	{
			"Launch a Poison Dart from your knee launcher",
			}

				
}

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
			return fire_ball_beam(GF_HELL_FIRE, dir, 20 + get_level(HUNTER_FLAMETHROW, 500), 5)
	              end,
	["info"] = 	function()
			
			return "dam "..((20 + get_level(HUNTER_FLAMETHROW, 500)).." rad "..5)
	end,
	["desc"] =	{
			"Fires a massive ball of fire",
			}

				
}

HUNTER_ROCKET = add_spell
{
	["name"] = 	"Rocket Launcher",
	["school"] = 	SCHOOL_HUNTING,
	["level"] = 	30,
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
			
			return "dam "..((20 + get_level(HUNTER_ROCKET, 500)).." rad "..(2 + get_level(HUNTER_ROCKET, 5)))
	end,
	["desc"] =	{
			"Launches a Wrist-Rocket",
			}

				
}

HUNTER_MISSILE = add_spell
{
	["name"] = 	"Missile Launcher",
	["school"] = 	SCHOOL_HUNTING,
	["level"] = 	20,
	["mana"] = 	25,
	["fail"] = 	10,
		["random"] =    0,
	["spell"] = 	function()
			local ret, dir	
			ret, dir = get_aim_dir()
			if ret == FALSE then return end
			return fire_ball(GF_MISSILE, dir, 25 + get_level(HUNTER_MISSILE, 500), 3 + get_level(HUNTER_MISSILE, 5))
	              end,
	["info"] = 	function()
			
			return "dam "..((25 + get_level(HUNTER_MISSILE, 500)).." rad "..(3 + get_level(HUNTER_MISSILE, 5)))
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
	["mana"] = 	125,
	["fail"] = 	10,
		["random"] =    0,
	["spell"] = 	function()
			local ret, dir	
			ret, dir = get_aim_dir()
			if ret == FALSE then return end
			return fire_ball(GF_NUKE, dir, 100 + get_level(HUNTER_NUKE, 500), 3 + get_level(HUNTER_NUKE, 5)),
					fire_ball(GF_HELL_FIRE, dir, 100 + get_level(HUNTER_NUKE, 500), 3 + get_level(HUNTER_NUKE, 5)),
					fire_ball(GF_HOLY_FIRE, dir, 100 + get_level(HUNTER_NUKE, 500), 3 + get_level(HUNTER_NUKE, 5))
	              end,
	["info"] = 	function()
			
			return "dam "..((100 + get_level(HUNTER_NUKE, 500)).." rad "..(3 + get_level(HUNTER_NUKE, 5)))
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

HUNTERIDENTIFY = add_spell
{
	["name"] = 	"Greater Identify",
	["school"] = 	{SCHOOL_HUNTING},
	["level"] = 	45,
	["mana"] = 	1,
	["mana_max"] = 	2,
	["fail"] =  1,
	["spell"] = 	function()
			if get_check("Cast on yourself?") == TRUE then
				self_knowledge()
			else
				identify_fully()
			end
			return TRUE
	end,
	["info"] = 	function()
		       	return ""
	end,
	["desc"] =	{
			"Asks for an object and fully identify it, providing the full list of powers",
			"Cast at yourself it will reveal your powers"
	}
}

