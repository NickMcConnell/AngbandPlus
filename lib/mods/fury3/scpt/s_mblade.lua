#s_mblade.lua

THROWMINDBLADE = add_spell
{
	["name"] = 	"Project Mind Shard",
	["school"] = 	SCHOOL_MINDBLADE,
	["level"] = 	5,
	["mana"] = 	1,
	["mana_max"] =  10,
	["fail"] = 	10,
	["random"] =    0,
	["spell"] = 	function()
			local ret, dir	
			ret, dir = get_aim_dir()
			if ret == FALSE then return end
			return fire_bolt(GF_ATTACK, dir, get_level(THROWMINDBLADE, player.num_blow))
	              end,
	["info"] = 	function()
			
			return "check your 'C' screen"
	end,
	["desc"] =	{
			"Lobs a group of attacks at an enemy",
			}

				
}

MIND_BLADE = add_spell
{
	["name"] =      "Mind Blade",
	["school"] =    {SCHOOL_MINDBLADE},
	["level"] =     1,
	["mana"] =      4,
	["mana_max"] =  44,
	["fail"] =      10,
	["random"] =    0,
	["spell"] =     function()
			local type, rad

			type = GF_FORCE
			if get_level(MIND_BLADE) >= 30 then type = GF_GRAVITY end

			rad = 0
			if get_level(MIND_BLADE) >= 45 then rad = 1 end

			return set_project(randint(20) + get_level(MIND_BLADE, 80),
				    type,
				    4 + get_level(MIND_BLADE, 40),
				    rad,
				    bor(PROJECT_STOP, PROJECT_KILL))
	end,
	["info"] =      function()
			return "dur "..(get_level(MIND_BLADE, 80)).."+d20 dam "..(4 + get_level(MIND_BLADE, 40)).."/blow"
	end,
	["desc"] =      {
			"Imbues your blade with force to deal more damage",
			
	}
}


MINDSPEED = add_spell
{
	["name"] = 	"Mind Speed",
	["school"] = 	{SCHOOL_MINDBLADE},
	["level"] = 	15,
	["mana"] = 	20,
	["mana_max"] = 	40,
	["fail"] = 	50,
		["random"] =    0,
	["inertia"] = 	{ 5, 20 },
	["spell"] = 	function()
			if player.fast == 0 then return set_fast(10 + randint(10) + get_level(MINDSPEED, 50), 5 + get_level(MINDSPEED, 20)) end
	end,
	["info"] = 	function()
		       	return "dur "..(10 + get_level(MINDSPEED, 50)).."+d10 speed "..(5 + get_level(MINDSPEED, 20))
	end,
	["desc"] =	{
			"Magically increases the passing of time around you",
	}
}

MINDSHIELD = add_spell
{
	["name"] = 	"Mind Shield",
	["school"] = 	SCHOOL_MINDBLADE,
	["level"] = 	45,
	["mana"] = 	45,
	["mana_max"] =  90,
	["fail"] = 	10,
		["random"] =    0,
	["spell"] = 	function()
			return fire_wave(GF_ATTACK, 0, 80 + get_level(MINDSHIELD, 200), 1 + get_level(MINDSHIELD, 3, 0), 20 + get_level(MINDSHIELD, 70), EFF_STORM)
	              end,
	["info"] = 	function()
			
			return "check your 'C' screen"
	end,
	["desc"] =	{
			"Raises a shield of attacks",
			}

				
}









