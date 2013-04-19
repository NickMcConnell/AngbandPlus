# s_ninja.lua
-- controls the ninja powers. (C) Billy McVicker 2005
THROWDAGGER = add_spell
{
	["name"] =      "Throw Dagger",
	["school"] =    {SCHOOL_NINJA},
	["level"] =     1,
	["mana"] =      1,
	["fail"] = 	1,
		["random"] =    0,
	-- Uses piety to cast
	["stat"] =      A_STR,
	["random"] = 	SKILL_NINJA,
	["spell"] = 	function()
			local ret, dir = get_aim_dir()
			if ret == FALSE then return end

			return fire_bolt(GF_ARROW, dir, (5 + get_level(THROWDAGGER)))
	end,
	["info"] = 	function()
			return ""
	end,
	["desc"] =	{
			"Thow a Ninja Dagger",
	}
}

POISONSTAR = add_spell
{
	["name"] =      "Thrown Poison Knife",
	["school"] =    {SCHOOL_NINJA},
	["level"] =     5,
	["mana"] =      5,
	["fail"] = 	1,
		["random"] =    0,
	-- Uses piety to cast
	["stat"] =      A_STR,
	["random"] = 	SKILL_NINJA,
	["spell"] = 	function()
			local ret, dir = get_aim_dir()
			if ret == FALSE then return end

			return fire_bolt(GF_POIS, dir, (5 + get_level(POISONSTAR)))
	end,
	["info"] = 	function()
			return ""
	end,
	["desc"] =	{
			"Thows a poisoned throwing knife",
	}
}

IRONDEFENSE = add_spell
{
	["name"] = 	"Ninja Defense",
	["school"] = 	SCHOOL_NINJA,
	["level"] = 	10,
	["mana"] = 	10,
	["fail"] = 	10,
		["random"] =    0,
	["inertia"] = 	{ 2, 50 },
	["spell"] = 	function()
			local type
			if get_level(IRONDEFENSE, 50) >= 25 then
				type = SHIELD_COUNTER
			else
				type = 0
			end
			return set_shield(randint(10) + 10 + get_level(IRONDEFENSE, 100), 10 + get_level(IRONDEFENSE, 50), type, 2 + get_level(IRONDEFENSE, 5), 3 + get_level(IRONDEFENSE, 5))
	end,
	["info"] = 	function()
			if get_level(IRONDEFENSE, 50) >= 25 then
				return "dam "..(2 + get_level(IRONDEFENSE, 5)).."d"..(3 + get_level(IRONDEFENSE, 5)).." dur "..(10 + get_level(IRONDEFENSE, 100)).."+d10 AC "..(10 + get_level(IRONDEFENSE, 50))
			else
				return "dur "..(10 + get_level(IRONDEFENSE, 100)).."+d10 AC "..(10 + get_level(IRONDEFENSE, 50))
			end
	end,
	["desc"] =	{
			"Toughens your body to defend against attacks",
			"At level 25 it starts dealing damage to attackers"
		}
}

NINJADISARM = add_spell
{
	["name"] =      "Disarm Traps",
	["school"] =    {SCHOOL_NINJA},
	["level"] =     15,
	["mana"] =      15,
	["fail"] =      35,
		["random"] =    0,
	["spell"] =     function()
		local ret, dir, type
		if (get_level(KNOCKOUTSPIN, 50) >= 20) then
			type = GF_KILL_TRAP
		else
			type = GF_KILL_TRAP
		end
		
		return fire_ball(type, 0, 20 + get_level(NINJADISARM, 500), 1)
	end,
	["info"] =      function()
		return "dam "..(20 + get_level(NINJADISARM, 500)).." rad 1"
	end,
	["desc"] =      {
			"Disarms the traps around you",
			""
	}
}

KNOCKOUTSPIN = add_spell
{
	["name"] =      "Knockout Spin",
	["school"] =    {SCHOOL_NINJA},
	["level"] =     25,
	["mana"] =      25,
	["fail"] =      35,
		["random"] =    0,
	["spell"] =     function()
		local ret, dir, type
		if (get_level(KNOCKOUTSPIN, 50) >= 20) then
			type = GF_OLD_SLEEP
		else
			type = GF_OLD_SLEEP
		end
	
		return fire_ball(type, 0, 20 + get_level(KNOCKOUTSPIN, 500), 1)
	end,
	["info"] =      function()
		return "dam "..(20 + get_level(KNOCKOUTSPIN, 500)).." rad 1"
	end,
	["desc"] =      {
			"Attempts to knock out all monsters around you.",
			""
	}
}

SETMASSTRAPS = add_spell
{
	["name"] =      "Drop Spiked Balls",
	["school"] =    {SCHOOL_NINJA},
	["level"] =     35,
	["mana"] =      35,
	["fail"] =      35,
		["random"] =    0,
	["spell"] =     function()
		local ret, dir, type
		if (get_level(KNOCKOUTSPIN, 50) >= 20) then
			type = GF_MAKE_GLYPH
		else
			type = GF_MAKE_GLYPH
		end
	
		return fire_ball(type, 0, 20 + get_level(SETMASSTRAPS, 500), 2)
	end,
	["info"] =      function()
		return "dam "..(20 + get_level(SETMASSTRAPS, 500)).." rad 1"
	end,
	["desc"] =      {
			"Sets a group of spiked balls around you",
			""
	}
}

NINJAKNOCKBACK = add_spell
{
	["name"] =      "Knockback Spin",
	["school"] =    {SCHOOL_NINJA},
	["level"] =     40,
	["mana"] =      40,
	["fail"] =      35,
		["random"] =    0,
	["spell"] =     function()
		local ret, dir, type
		if (get_level(KNOCKOUTSPIN, 50) >= 20) then
			type = GF_FORCE
		else
			type = GF_FORCE
		end
	
		return fire_ball(type, 0, 20 + get_level(NINJAKNOCKBACK, 500), 1)
	end,
	["info"] =      function()
		return "dam "..(20 + get_level(NINJAKNOCKBACK, 500)).." rad 1"
	end,
	["desc"] =      {
			"Attempts to knock back all monsters around you.",
			""
	}
}