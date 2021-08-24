-- handle the amybsod school

function get_banshee_damage()
	return get_level(AMYBSOD_SOUND, 5), 3 + get_level(AMYBSOD_SOUND, 15)
end

AMYBSOD_SOUND = add_spell
{
	["name"] = "Banshee Scream",
	["school"] = SCHOOL_AMYBSOD,
	["level"] = 1,
	["mana"] = 50,
	["mana_max"] = 300,
	["fail"] = 15,
	-- Uses piety to cast
	["piety"] =     TRUE,
	["stat"] =      A_WIS,
	["random"] = 	SKILL_SPIRITUALITY,
	["spell"] = function()
		local ret, dir
		ret, dir = get_aim_dir()
		if ret == FALSE then return end
		return fire_bolt_or_beam(get_level(AMYBSOD_SOUND, 85), GF_SOUND, dir, damroll(get_banshee_damage()))
	end,
	["info"] = function()
		local n, d
		n, d = get_banshee_damage()
		return "dam "..n.."d"..d
	end,
	["desc"] =
	{
		"Scream like a banshee to damage the monsters' ears and daze them.",
		"Sometimes it can blast through its first target."
	},
}

AMYBSOD_DARK = add_spell
{
	["name"] =      "Lights Out",
	["school"] =    {SCHOOL_AMYBSOD},
	["level"] =     20,
	["mana"] =      120,
	["mana_max"] =  500,
	["fail"] =      50,
	-- Uses piety to cast
	["piety"] =     TRUE,
	["stat"] =      A_WIS,
	["random"] = 	SKILL_SPIRITUALITY,
	["spell"] =     function()
		local ret, dir, type
		ret, dir = get_aim_dir()
		if ret == FALSE then return end
		return fire_ball(GF_DARK, dir, 20 + get_level(AMYBSOD_DARK, 300), 2 + get_level(AMYBSOD_DARK, 4))
	end,
	["info"] =      function()
		return "dam "..(20 + get_level(AMYBSOD_DARK, 300)).." rad "..(2 + get_level(AMYBSOD_DARK, 4))
	end,
	["desc"] =      {
			"Conjures a ball of darkness to punch the monsters' lights out"
	}
}

AMYBSOD_TROLL = add_spell
{
	["name"] =      "Trololololol",
	["school"] =    {SCHOOL_AMYBSOD},
	["level"] =     30,
	["mana"] =      100,
	["mana_max"] =  400,
	["fail"] = 	60,
	-- Uses piety to cast
	["piety"] =     TRUE,
	["stat"] =      A_WIS,
	["random"] = 	SKILL_SPIRITUALITY,
	["spell"] = 	function()
			local y, x, m_idx

			y, x = find_position(player.py, player.px)
			m_idx = place_monster_one(y, x, test_monster_name("Cave troll"), 0, FALSE, MSTATUS_FRIEND)

			if m_idx ~= 0 then
				monster_set_level(m_idx, 20 + get_level(AMYBSOD_TROLL, 70, 0))
				return TRUE
			end
	end,
	["info"] =      function()
			return "level "..(get_level(AMYBSOD_TROLL, 70) + 20)
	end,
	["desc"] =	{
			"Summons a troll that fights on your side"
	}
}

