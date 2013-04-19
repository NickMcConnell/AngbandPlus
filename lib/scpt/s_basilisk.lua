# s_basilisk

function get_basilisk_sleep_dam()
	return 3 + get_level(BASILISK_SLEEP_BOLT, 50), 1 + get_level(BASILISK_SLEEP_BOLT, 20)
end

function get_basilisk_stun_dam()
	return 3 + get_level(BASILISK_STUN_BOLT, 50), 1 + get_level(BASILISK_STUN_BOLT, 20)
end

function get_basilisk_death_dam()
	return 3 + get_level(BASILISK_KILL_BOLT, 50), 1 + get_level(BASILISK_KILL_BOLT, 20)
end
BASILISK_SLEEP_BOLT = add_spell
{
	["name"] = 	"Sleep Monster",
	["school"] = 	SCHOOL_BASILISK,
	["level"] = 	1,
	["mana"] = 	1,
	["mana_max"] =  25,
	["fail"] = 	10,
	["spell"] = 	function()
			local ret, dir	

			ret, dir = get_aim_dir()
			if ret == FALSE then return end
			return fire_bolt(GF_OLD_SLEEP, dir, damroll(get_basilisk_sleep_dam()))
	end,
	["info"] = 	function()
			local x, y

			x, y = get_basilisk_sleep_dam()
			return "dam "..x.."d"..y
	end,
	["desc"] =	{
			"Attempts to put targeted monster to sleep",
			"The damage will increase with level"
		}
}

BASILISK_STUN_BOLT = add_spell
{
	["name"] = 	"Stun Monster",
	["school"] = 	SCHOOL_BASILISK,
	["level"] = 	5,
	["mana"] = 	5,
	["mana_max"] =  50,
	["fail"] = 	10,
	["spell"] = 	function()
			local ret, dir	

			ret, dir = get_aim_dir()
			if ret == FALSE then return end
			return fire_bolt(GF_STUN, dir, damroll(get_basilisk_stun_dam()))
	end,
	["info"] = 	function()
			local x, y

			x, y = get_basilisk_stun_dam()
			return "dam "..x.."d"..y
	end,
	["desc"] =	{
			"Attempts to stun the targeted monster",
			"The damage will increase with level"
		}
}

BASILISK_SLEEP_BALL = add_spell
{
	["name"] =      "Sleep Monsters",
	["school"] =    SCHOOL_BASILISK,
	["level"] =     10,
	["mana"] =      5,
	["mana_max"] =  70,
	["fail"] =      35,
	["spell"] =     function()
		local ret, dir, type
		if (get_level(FIREFLASH, 50) >= 20) then
			type = GF_OLD_SLEEP
		else
			type = GF_OLD_SLEEP
		end
		ret, dir = get_aim_dir()
		if ret == FALSE then return end
		return fire_ball(GF_OLD_SLEEP, dir, 20 + get_level(BASILISK_SLEEP_BALL, 500), 2 + get_level(BASILISK_SLEEP_BALL, 5))
	end,
	["info"] =      function()
		return "dam "..(20 + get_level(BASILISK_SLEEP_BALL, 500)).." rad "..(2 + get_level(BASILISK_SLEEP_BALL, 5))
	end,
	["desc"] =      {
			"Creates a ball of sleep",
		
	}
}

BASILISK_STUN_BALL = add_spell
{
	["name"] =      "Stun Monsters",
	["school"] =    SCHOOL_BASILISK,
	["level"] =     20,
	["mana"] =      5,
	["mana_max"] =  70,
	["fail"] =      35,
	["spell"] =     function()
		local ret, dir, type
		if (get_level(FIREFLASH, 50) >= 20) then
			type = GF_STUN
		else
			type = GF_STUN
		end
		ret, dir = get_aim_dir()
		if ret == FALSE then return end
		return fire_ball(GF_STUN, dir, 20 + get_level(BASILISK_STUN_BALL, 500), 2 + get_level(BASILISK_STUN_BALL, 5))
	end,
	["info"] =      function()
		return "dam "..(20 + get_level(BASILISK_STUN_BALL, 500)).." rad "..(2 + get_level(BASILISK_STUN_BALL, 5))
	end,
	["desc"] =      {
			"Creates a ball of stunning force",
		
	}
}

BASILISK_KILL_BOLT = add_spell
{
	["name"] = 	"Kill Monster",
	["school"] = 	SCHOOL_BASILISK,
	["level"] = 	35,
	["mana"] = 	200,
	["mana_max"] =  300,
	["fail"] = 	10,
	["spell"] = 	function()
			local ret, dir	

			ret, dir = get_aim_dir()
			if ret == FALSE then return end
			return fire_bolt(GF_DEATH, dir, damroll(get_basilisk_death_dam()))
	end,
	["info"] = 	function()
			local x, y

			x, y = get_basilisk_death_dam()
			return "dam "..x.."d"..y
	end,
	["desc"] =	{
			"Attempts to kill the targeted monster",
			"The damage will increase with level"
		}
}

BASILISK_KILL_BALL = add_spell
{
	["name"] =      "Kill Monsters",
	["school"] =    SCHOOL_BASILISK,
	["level"] =     45,
	["mana"] =      200,
	["mana_max"] =  300,
	["fail"] =      35,
	["spell"] =     function()
		local ret, dir, type
		if (get_level(FIREFLASH, 50) >= 20) then
			type = GF_DEATH
		else
			type = GF_DEATH
		end
		ret, dir = get_aim_dir()
		if ret == FALSE then return end
		return fire_ball(GF_DEATH, dir, 20 + get_level(BASILISK_KILL_BALL, 500), 2 + get_level(BASILISK_KILL_BALL, 5))
	end,
	["info"] =      function()
		return "dam "..(20 + get_level(BASILISK_KILL_BALL, 500)).." rad "..(2 + get_level(BASILISK_KILL_BALL, 5))
	end,
	["desc"] =      {
			"Creates a ball of killing power",
		
	}
}