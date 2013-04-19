# S_JEDI.LUA 
function get_electricbolt_dam()
	return 3 + get_level(ELECTRICBOLT, 50), 1 + get_level(ELECTRICBOLT, 20)
end

ELECTRICBOLT = add_spell
{
	["name"] = 	"Lightening Bolt",
	["school"] = 	SCHOOL_DARK,
	["level"] = 	1,
	["mana"] = 	1,
	["mana_max"] =  25,
	["fail"] = 	10,
		["random"] =    0,
	["spell"] = 	function()
			local ret, dir	

			ret, dir = get_aim_dir()
			if ret == FALSE then return end
			return fire_bolt(GF_ELEC, dir, damroll(get_electricbolt_dam()))
	end,
	["info"] = 	function()
			local x, y

			x, y = get_electricbolt_dam()
			return "dam "..x.."d"..y
	end,
	["desc"] =	{
			"Conjures up electricity into a powerful bolt",
				}
}

FORCEPUSH = add_spell
{
	["name"] = 	"Force Push",
	["school"] = 	SCHOOL_JEDI,
	["level"] = 	5,
	["mana"] = 	10,
	["mana_max"] =  50,
	["fail"] = 	10,
		["random"] =    0,
	["spell"] = 	function()
			local ret, dir	

			ret, dir = get_aim_dir()
			if ret == FALSE then return end
			return fire_bolt(GF_FORCE, dir, damroll(get_electricbolt_dam()))
	end,
	["info"] = 	function()
			local x, y

			x, y = get_electricbolt_dam()
			return "dam "..x.."d"..y
	end,
	["desc"] =	{
			"Shoves a monster from you and damages them",
				}
}

FORCEPULL = add_spell
{
	["name"] = 	"Force Pull",
	["school"] = 	SCHOOL_JEDI,
	["level"] = 	5,
	["mana"] = 	10,
	["mana_max"] =  50,
	["fail"] = 	10,
		["random"] =    0,
	["spell"] = 	function()
			local ret, dir	

			ret, dir = get_aim_dir()
			if ret == FALSE then return end
			return fire_bolt(GF_TELEKINESIS, dir, damroll(get_electricbolt_dam()))
	end,
	["info"] = 	function()
			local x, y

			x, y = get_electricbolt_dam()
			return "dam "..x.."d"..y
	end,
	["desc"] =	{
			"Pulls a monster towards you and damages them",
				}
}

FORCEGRIP = add_spell
{
	["name"] = 	"Force Grip",
	["school"] = 	SCHOOL_DARK,
	["level"] = 	15,
	["mana"] = 	1,
	["mana_max"] =  25,
	["fail"] = 	10,
		["random"] =    0,
	["spell"] = 	function()
			local ret, dir	

			ret, dir = get_aim_dir()
			if ret == FALSE then return end
			return fire_bolt(GF_UNBREATH, dir, damroll(get_electricbolt_dam()))
	end,
	["info"] = 	function()
			local x, y

			x, y = get_electricbolt_dam()
			return "dam "..x.."d"..y
	end,
	["desc"] =	{
			"Choke a monster",
				}
}

FORCEWAVE = add_spell
{
	["name"] = 	"Force Wave",
	["school"] = 	SCHOOL_DARK,
	["level"] = 	20,
	["mana"] = 	16,
		["random"] =    0,
	["mana_max"] = 	40,
	["fail"] = 	65,
	["inertia"] = 	{ 4, 100 },
	["spell"] = 	function()
			fire_wave(GF_FORCE, 0, 40 + get_level(FORCEWAVE, 200), 0, 6 + get_level(FORCEWAVE, 10), EFF_WAVE)
			return TRUE
	end,
	["info"] = 	function()
			return "dam "..(40 + get_level(FORCEWAVE,  200)).." rad "..(6 + get_level(FORCEWAVE,  10))
	end,
	["desc"] =	{
			"Summons a monstrous Force wave that will expand and crush the",
			"monsters under it's mighty waves"
	}
}

FORCESTORM = add_spell
{
	["name"] = 	"Force Storm",
	["school"] = 	SCHOOL_DARK,
	["level"] = 	20,
	["mana"] = 	3,
	["mana_max"] = 	30,
	["fail"] = 	20,
		["random"] =    0,
	["spell"] = 	function()
			local ret, dir, type

			ret, dir = get_aim_dir()
			if ret == FALSE then return end
			if get_level(FORCESTORM, 50) >= 30 then type = GF_DISINTEGRATE
			else type = GF_ELEC end
			fire_cloud(type, dir, 7 + get_level(FORCESTORM, 150), 3, 5 + get_level(FORCESTORM, 40))
			return TRUE
	end,
	["info"] = 	function()
			return "dam "..(7 + get_level(FORCESTORM, 150)).." rad 3 dur "..(5 + get_level(FORCESTORM, 40))
	end,
	["desc"] =	{
			"Creates a cloud of lightening",
			"The cloud will persist for some turns, damaging all monsters passing by",
			"At spell level 50 it turns into a cloud that destroys everything"
	}
}

FORCEKILL = add_spell
{
	["name"] =      "Force Kill",
	["school"] =    SCHOOL_DARK,
	["level"] =     50,
	["mana"] =      500,
	["mana_max"] =  500,
	["fail"] =      100,
		["random"] =    0,
	["spell"] =     function()
		local ret, dir, type
		if (get_level(FORCEKILL, 50) >= 20) then
			type = GF_DEATH
		else
			type = GF_DEATH
		end
		ret, dir = get_aim_dir()
		if ret == FALSE then return end
		return fire_bolt(type, dir, FORCEKILL)
	end,
	["info"] =      function()
		return "dam "..(20 + get_level(FORCEKILL, 500)).." rad "..(2 + get_level(FORCEKILL, 5))
	end,
	["desc"] =      {
			"Instantly kills any monster. But it has a huge failure rate",
			"and mana cost"
	}
}

FORCECRUSH = add_spell
{
	["name"] =      "Force Crush",
	["school"] =    SCHOOL_DARK,
	["level"] =     25,
	["mana"] =      10,
	["mana_max"] =  50,
	["fail"] =      35,
		["random"] =    0,
	["spell"] =     function()
		local ret, dir, type
		if (get_level(FORCECRUSH, 50) >= 20) then
			type = GF_IMPLOSION
		else
			type = GF_IMPLOSION
		end
		ret, dir = get_aim_dir()
		if ret == FALSE then return end
		return fire_ball(type, dir, 20 + get_level(FORCECRUSH, 500), 2 + get_level(FORCECRUSH, 5))
	end,
	["info"] =      function()
		return "dam "..(20 + get_level(FORCECRUSH, 500)).." rad "..(2 + get_level(FORCECRUSH, 5))
	end,
	["desc"] =      {
			"Crushes a group of enemies",
			""
	}
}

FORCEMIND = add_spell
{
	["name"] =      "Force Mind Control",
	["school"] =   SCHOOL_LIGHT,
	["level"] =     20,
	["mana"] =      10,
	["mana_max"] =  50,
	["fail"] =      35,
		["random"] =    0,
	["spell"] =     function()
		local ret, dir, type
		if (get_level(FORCEMIND, 50) >= 20) then
			type = GF_DOMINATION
		else
			type = GF_DOMINATION
		end
		ret, dir = get_aim_dir()
		if ret == FALSE then return end
		return fire_ball(type, dir, 20 + get_level(FORCEMIND, 500), 2 + get_level(FORCEMIND, 5))
	end,
	["info"] =      function()
		return "dam "..(20 + get_level(FORCEMIND, 500)).." rad "..(2 + get_level(FORCEMIND, 5))
	end,
	["desc"] =      {
			"Charms a group of monsters to your side",
			""
	}
}

JHEALING = add_spell
{
	["name"] = 	"Force Healing",
	["school"] = 	{SCHOOL_LIGHT},
	["level"] = 	10,
	["mana"] = 	15,
	["mana_max"] = 	50,
	["fail"] = 	45,
		["random"] =    0,
	["spell"] = 	function()
			return hp_player(player.mhp * (15 + get_level(HEALING, 35)) / 100)
	end,
	["info"] = 	function()
			return "heal "..(15 + get_level(HEALING, 35)).."% = "..(player.mhp * (15 + get_level(HEALING, 35)) / 100).."hp"
	end,
	["desc"] =	{
			"Heals a percent of hitpoints",
	}
}

JRECOVERY = add_spell
{
	["name"] = 	"Force Recovery",
	["school"] = 	{SCHOOL_LIGHT},
	["level"] = 	15,
	["mana"] = 	10,
	["mana_max"] = 	25,
	["fail"] = 	60,
		["random"] =    0,
	["inertia"] = 	{ 2, 100 },
	["spell"] = 	function()
			local obvious
			obvious = set_poisoned(player.poisoned / 2)
			if get_level(RECOVERY, 50) >= 5 then
				obvious = is_obvious(set_poisoned(0), obvious)
				obvious = is_obvious(set_cut(0), obvious)
			end
			if get_level(RECOVERY, 50) >= 10 then
				obvious = is_obvious(do_res_stat(A_STR, TRUE), obvious)
				obvious = is_obvious(do_res_stat(A_CON, TRUE), obvious)
				obvious = is_obvious(do_res_stat(A_DEX, TRUE), obvious)
				obvious = is_obvious(do_res_stat(A_WIS, TRUE), obvious)
				obvious = is_obvious(do_res_stat(A_INT, TRUE), obvious)
				obvious = is_obvious(do_res_stat(A_CHR, TRUE), obvious)
			end
			if get_level(RECOVERY, 50) >= 15 then
				obvious = is_obvious(restore_level(), obvious)
			end
			return obvious
	end,
	["info"] = 	function()
			return ""
	end,
	["desc"] =	{
			"Reduces the length of time that you are poisoned",
			"At level 5 it cures poison and cuts",
			"At level 10 it restores drained stats",
			"At level 15 it restores lost experience"
	}
}

JSENSEHIDDEN = add_spell
{
	["name"] = 	"Force Danger Sense",
	["school"] = 	{SCHOOL_JEDI},
	["level"] = 	5,
	["mana"] = 	2,
	["mana_max"] = 	10,
	["fail"] = 	25,
		["random"] =    0,
	["inertia"] = 	{ 1, 10 },
	["spell"] = 	function()
			local obvious = nil
			obvious = detect_traps(15 + get_level(SENSEHIDDEN, 40, 0))
			if get_level(SENSEHIDDEN, 50) >= 15 then
				obvious = is_obvious(set_tim_invis(10 + randint(20) + get_level(SENSEHIDDEN, 40)), obvious)
			end
			return obvious
	end,
	["info"] = 	function()
			if get_level(SENSEHIDDEN, 50) >= 15 then
				return "rad "..(15 + get_level(SENSEHIDDEN, 40)).." dur "..(10 + get_level(SENSEHIDDEN, 40)).."+d20"
			else
				return "rad "..(15 + get_level(SENSEHIDDEN, 40))
			end
	end,
	["desc"] =	{
			"Detects the traps in a certain radius around you",
			"At level 15 it allows you to sense invisible for a while"
	}
}

JSENSEMONSTERS = add_spell
{
	["name"] = 	"Force Life Sense",
	["school"] = 	{SCHOOL_JEDI},
	["level"] = 	1,
	["mana"] =      1,
	["mana_max"] =  20,
	["fail"] = 	10,
		["random"] =    0,
	["inertia"] = 	{ 1, 10 },
	["spell"] = 	function()
			local obvious
			obvious = detect_monsters_normal(10 + get_level(SENSEMONSTERS, 40, 0))
			if get_level(SENSEMONSTERS, 50) >= 30 then
				obvious = is_obvious(set_tim_esp(10 + randint(10) + get_level(SENSEMONSTERS, 20)), obvious)
			end
			return obvious
	end,
	["info"] = 	function()
			if get_level(SENSEMONSTERS, 50) >= 30 then
				return "rad "..(10 + get_level(SENSEMONSTERS, 40)).." dur "..(10 + get_level(SENSEMONSTERS, 20)).."+d10"
			else
				return "rad "..(10 + get_level(SENSEMONSTERS, 40))
			end
	end,
	["desc"] =	{
			"Detects all monsters near you",
			"At level 30 it allows you to sense monster minds for a while"
	}
}

JVISION = add_spell
{
	["name"] = 	"Force Sight",
	["school"] = 	{SCHOOL_JEDI},
	["level"] = 	15,
	["mana"] = 	7,
	["mana_max"] = 	55,
	["fail"] = 	45,
		["random"] =    0,
	["inertia"] = 	{ 2, 200 },
	["spell"] = 	function()
			if get_level(VISION, 50) >= 25 then
				wiz_lite_extra()
			else
				map_area()
			end
			return TRUE
	end,
	["info"] = 	function()
			return ""
	end,
	["desc"] =	{
			"Detects the layout of the surrounding area",
			"At level 25 it maps and lights the whole level",
	}
}

JSTARIDENTIFY = add_spell
{
	["name"] = 	"Force Learn",
	["school"] = 	{SCHOOL_LIGHT},
	["level"] = 	35,
	["mana"] = 	30,
	["mana_max"] = 	30,
	["fail"] = 	80,
		["random"] =    0,
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

JDISARM = add_spell
{
	["name"] = 	"Force Disarm",
	["school"] = 	{SCHOOL_JEDI},
	["level"] = 	3,
	["mana"] = 	2,
	["mana_max"] = 	4,
	["fail"] = 	15,
		["random"] =    0,
	["spell"] = 	function()
			local obvious
			obvious = destroy_doors_touch()
			if get_level(DISARM, 50) >= 10 then obvious = is_obvious(destroy_traps_touch(), obvious) end
			return obvious
	end,
	["info"] = 	function()
			return ""
	end,
	["desc"] =	{
			"Destroys doors and disarms traps",
			"At level 10 it unlocks doors and disarms traps",
	}
}

FESSENCESPEED = add_spell
{
	["name"] = 	"Force Speed",
	["school"] = 	{SCHOOL_LIGHT},
	["level"] = 	15,
	["mana"] = 	20,
	["mana_max"] = 	40,
	["fail"] = 	50,
		["random"] =    0,
	["inertia"] = 	{ 5, 20 },
	["spell"] = 	function()
			if player.fast == 0 then return set_fast(10 + randint(10) + get_level(FESSENCESPEED, 50), 5 + get_level(FESSENCESPEED, 20)) end
	end,
	["info"] = 	function()
		       	return "dur "..(10 + get_level(FESSENCESPEED, 50)).."+d10 speed "..(5 + get_level(FESSENCESPEED, 20))
	end,
	["desc"] =	{
			"Magically increases the passing of time around you",
	}
}

JREBUILD = add_spell
{
	["name"] =      "Force Change",
	["school"] =    {SCHOOL_LIGHT},
	["level"] =     35,
	["mana"] =      70,
	["mana_max"] =  70,
	["fail"] =      60,
		["random"] =    0,
	["spell"] =     function()
			alter_reality()
			return TRUE
	end,
	["info"] =      function()
			return ""
	end,
	["desc"] =      {
			"Produces a reality shaking thing that transports you to a nearly",
			"identical reality.",
	}
}