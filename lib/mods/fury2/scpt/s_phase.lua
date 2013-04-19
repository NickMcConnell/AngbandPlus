# test

PHASEBOLT = add_spell
{
	["name"] = 	"Phase Attack Bolt",
	["school"] = 	SCHOOL_PHASING,
	["level"] = 	1,
	["mana"] = 	1,
	["mana_max"] =  5,
	["fail"] = 	10,
		["random"] =    0,
	["spell"] = 	function()
			local ret, dir	
			ret, dir = get_aim_dir()
			if ret == FALSE then return end
			return fire_bolt(GF_ATTACK, dir, get_level(PHASEBOLT, player.num_blow))
	              end,
	["info"] = 	function()
			
			return "check your 'C' screen"
	end,
	["desc"] =	{
			"Lobs a group of attacks at an enemy",
			}

				
}

PHASEBEAM = add_spell
{
	["name"] = 	"Phase Attack Beam",
	["school"] = 	SCHOOL_PHASING,
	["level"] = 	10,
	["mana"] = 	5,
	["mana_max"] =  10,
	["fail"] = 	10,
		["random"] =    0,
	["spell"] = 	function()
			local ret, dir	
			ret, dir = get_aim_dir()
			if ret == FALSE then return end
			return fire_beam(GF_ATTACK, dir, get_level(PHASEBEAM, player.num_blow))
	              end,
	["info"] = 	function()
			
			return "check your 'C' screen"
	end,
	["desc"] =	{
			"Lobs a beam group of attacks at an enemy",
			}

				
}

PHASEBALL = add_spell
{
	["name"] = 	"Phase Attack Ball",
	["school"] = 	SCHOOL_PHASING,
	["level"] = 	15,
	["mana"] = 	10,
	["mana_max"] =  20,
	["fail"] = 	10,
		["random"] =    0,
	["spell"] = 	function()
			local ret, dir	
			ret, dir = get_aim_dir()
			if ret == FALSE then return end
			return fire_ball(GF_ATTACK, dir, 20 + get_level(PHASEBALL, 500), 2 + get_level(PHASEBALL, 5))
	              end,
	["info"] = 	function()
			
			return "check your 'C' screen"
	end,
	["desc"] =	{
			"Lobs a ball of attacks at an enemy",
			}

				
}

PHASEWAVE = add_spell
{
	["name"] = 	"Phase Attack Wave",
	["school"] = 	SCHOOL_PHASING,
	["level"] = 	25,
	["mana"] = 	25,
	["mana_max"] =  50,
	["fail"] = 	10,
		["random"] =    0,
	["spell"] = 	function()
		
			return fire_wave(GF_ATTACK, 0, 40 + get_level(PHASEWAVE, 200), 0, 6 + get_level(PHASEWAVE, 10), EFF_WAVE)
	              end,
	["info"] = 	function()
			
			return "check your 'C' screen"
	end,
	["desc"] =	{
			"Lobs a wave group of attacks at an enemy",
			}

				
}

PHASECLOUD = add_spell
{
	["name"] = 	"Phase Attack Cloud",
	["school"] = 	SCHOOL_PHASING,
	["level"] = 	30,
	["mana"] = 	45,
	["mana_max"] =  90,
	["fail"] = 	10,
		["random"] =    0,
	["spell"] = 	function()
			local ret, dir	
			ret, dir = get_aim_dir()
			if ret == FALSE then return end
			return fire_cloud(GF_ATTACK, dir, 7 + get_level(PHASECLOUD, 150), 3, 5 + get_level(PHASECLOUD, 40))
	              end,
	["info"] = 	function()
			
			return "check your 'C' screen"
	end,
	["desc"] =	{
			"Lobs a cloud group of attacks at an enemy",
			}

				
}
PHASESHIELD = add_spell
{
	["name"] = 	"Phase Attack Shield",
	["school"] = 	SCHOOL_PHASING,
	["level"] = 	45,
	["mana"] = 	45,
	["mana_max"] =  90,
	["fail"] = 	10,
		["random"] =    0,
	["spell"] = 	function()
			return fire_wave(GF_ATTACK, 0, 80 + get_level(PHASESHIELD, 200), 1 + get_level(PHASESHIELD, 3, 0), 20 + get_level(PHASESHIELD, 70), EFF_STORM)
	              end,
	["info"] = 	function()
			
			return "check your 'C' screen"
	end,
	["desc"] =	{
			"Raises a shield of attacks around you",
			}

				
}
