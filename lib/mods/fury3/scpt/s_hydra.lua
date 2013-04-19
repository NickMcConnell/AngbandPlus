# s_hydra

HYDRAFIRE = add_spell
{
	["name"] = 	"Breathe Fire",
	["school"] = 	{SCHOOL_HYDRA},
	["level"] = 	1,
	["mana"] = 	5,
	["mana_max"] = 	30,
	["fail"] = 	20,

	["spell"] = 	function()
			local ret, dir, type

			ret, dir = get_aim_dir()
			if ret == FALSE then return end
			if get_level(HYDRAFIRE, 50) >= 30 then type = GF_HOLY_FIRE
			else type = GF_FIRE end
			fire_cloud(type, dir, 7 + get_level(HYDRAFIRE, 150), 3, 5 + get_level(HYDRAFIRE, 40))
			return TRUE
	end,
	["info"] = 	function()
			return "dam "..(7 + get_level(HYDRAFIRE, 150)).." rad 3 dur "..(5 + get_level(HYDRAFIRE, 40))
	end,
	["desc"] =	{
			"Breath Fire",
			"The cloud will persist for some turns, damaging all monsters passing by",

	}
}

HYDRAPOISON = add_spell
{
	["name"] = 	"Breathe Poison",
	["school"] = 	{SCHOOL_HYDRA},
	["level"] = 	10,
	["mana"] = 	5,
	["mana_max"] = 	30,
	["fail"] = 	20,

	["spell"] = 	function()
			local ret, dir, type

			ret, dir = get_aim_dir()
			if ret == FALSE then return end
			if get_level(NOXIOUSCLOUD, 50) >= 30 then type = GF_POIS
			else type = GF_POIS end
			fire_cloud(type, dir, 7 + get_level(HYDRAPOISON, 150), 3, 5 + get_level(HYDRAPOISON, 40))
			return TRUE
	end,
	["info"] = 	function()
			return "dam "..(7 + get_level(HYDRAPOISON, 150)).." rad 3 dur "..(5 + get_level(HYDRAPOISON, 40))
	end,
	["desc"] =	{
			"Breath Poison",
			"The cloud will persist for some turns, damaging all monsters passing by",

	}
}

HYDRAFIREWALL = add_spell
{
	["name"] =      "Breathe Fire Wall",
	["school"] =    {SCHOOL_HYDRA},
	["level"] =     15,
	["mana"] =      25,
	["mana_max"] =  100,
	["fail"] =      40,
	["spell"] =     function()
		local ret, dir, type
		if (get_level(HYDRAFIREWALL, 50) >= 6) then
			type = GF_HELL_FIRE
		else
			type = GF_FIRE
		end
		ret, dir = get_aim_dir()
		if ret == FALSE then return end
		fire_wall(type, dir, 40 + get_level(HYDRAFIREWALL, 150), 10 + get_level(HYDRAFIREWALL, 14))
		return TRUE
	end,
	["info"] =      function()
		return "dam "..(40 + get_level(HYDRAFIREWALL, 150)).." dur "..(10 + get_level(HYDRAFIREWALL, 14))
	end,
	["desc"] =      {
			"Creates a fiery wall to incinerate monsters stupid enough to attack you",
			"At level 6 it turns into a wall of hell fire"
	}
}

HYDRAPOISWALL = add_spell
{
	["name"] =      "Breathe Poison Wall",
	["school"] =    {SCHOOL_HYDRA},
	["level"] =     25,
	["mana"] =      25,
	["mana_max"] =  100,
	["fail"] =      40,
	["spell"] =     function()
		local ret, dir, type
		type = GF_POIS

		ret, dir = get_aim_dir()
		if ret == FALSE then return end
		fire_wall(type, dir, 40 + get_level(HYDRAPOISWALL, 150), 10 + get_level(HYDRAPOISWALL, 14))
		return TRUE
	end,
	["info"] =      function()
		return "dam "..(40 + get_level(HYDRAPOISWALL, 150)).." dur "..(10 + get_level(HYDRAPOISWALL, 14))
	end,
	["desc"] =      {
			"Creates a wall of Poison in front of you",
		
	}
}


HYDRAFIREAURA = add_spell
{
	["name"] = 	"Aura of Fire",
	["school"] = 	SCHOOL_HYDRA,
	["level"] = 	30,
	["mana"] = 	40,
	["mana_max"] =  140,
	["fail"] = 	45,
	["spell"] = 	function()
			return fire_wave(GF_FIRE, 0, 80 + get_level(HYDRAFIREAURA, 200), 1 + get_level(HYDRAFIREAURA, 3, 0), 20 + get_level(HYDRAFIREAURA, 70), EFF_STORM)
	              
		      end,
	["info"] = 	function()
			
			return ""
	end,
	["desc"] =	{
			"Surrounds you with an aura of fire",
			}

				
}

HYDRAPOISAURA = add_spell
{
	["name"] = 	"Aura of Poison",
	["school"] = 	SCHOOL_HYDRA,
	["level"] = 	30,
	["mana"] = 	50,
	["mana_max"] =  150,
	["fail"] = 	50,
	["spell"] = 	function()
			return fire_wave(GF_POIS, 0, 80 + get_level(HYDRAPOISAURA, 200), 1 + get_level(HYDRAPOISAURA, 3, 0), 20 + get_level(HYDRAPOISAURA, 70), EFF_STORM)
	              
		      end,
	["info"] = 	function()
			
			return ""
	end,
	["desc"] =	{
			"Surrounds you with an aura of poison",
			}

				
}
