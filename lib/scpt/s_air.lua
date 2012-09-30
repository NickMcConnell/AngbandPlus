-- handle the air school

NOXIOUSCLOUD = add_spell
{
	["name"] = 	"Noxious Cloud",
        ["school"] = 	{SCHOOL_AIR},
        ["level"] = 	3,
        ["mana"] = 	3,
        ["mana_max"] = 	30,
        ["fail"] = 	20,
        ["spell"] = 	function()
			local ret, dir, type

        		ret, dir = get_aim_dir()
        	        if ret == FALSE then return FALSE end
                        if get_level(NOXIOUSCLOUD, 50) >= 30 then type = GF_UNBREATH
                        else type = GF_POIS end
		        fire_cloud(type, dir, 7 + get_level(NOXIOUSCLOUD, 150), 3, 5 + get_level(NOXIOUSCLOUD, 40))
	end,
	["info"] = 	function()
			return "dam "..(7 + get_level(NOXIOUSCLOUD, 150)).." rad 3 dur "..(5 + get_level(NOXIOUSCLOUD, 40))
	end,
        ["desc"] =	{
        		"Creates a cloud of poison",
                        "The clound will persist for some turns, damaging all monsters passing by",
                        "At level 30 it turns into a thick gas preventing living beings from breathing"
        }
}

AIRWINGS = add_spell
{
	["name"] = 	"Wings of Winds",
        ["school"] = 	{SCHOOL_AIR, SCHOOL_CONVEYANCE},
        ["level"] = 	22,
        ["mana"] = 	30,
        ["mana_max"] = 	40,
        ["fail"] = 	70,
        ["spell"] = 	function()
			if get_level(AIRWINGS, 50) >= 16 then
                        	if player.tim_fly == 0 then set_tim_fly(randint(10) + 5 + get_level(AIRWINGS, 25)) end
                        else
                        	if player.tim_ffall == 0 then set_tim_ffall(randint(10) + 5 + get_level(AIRWINGS, 25)) end
                        end
	end,
	["info"] = 	function()
			return "dur "..(5 + get_level(AIRWINGS, 25)).."+d10"
	end,
        ["desc"] =	{
        		"Grants the power or leviation",
                        "At level 16 it grants the power of controled flight"
        }
}

INVISIBILITY = add_spell
{
	["name"] = 	"Invisibility",
        ["school"] = 	{SCHOOL_AIR},
        ["level"] = 	16,
        ["mana"] = 	10,
        ["mana_max"] = 	20,
        ["fail"] = 	70,
        ["spell"] = 	function()
                       	if player.tim_invisible == 0 then set_invis(randint(20) + 15 + get_level(INVISIBILITY, 50), 20 + get_level(INVISIBILITY, 50)) end
       	end,
	["info"] = 	function()
			return "dur "..(15 + get_level(INVISIBILITY, 50)).."+d20 power "..(20 + get_level(INVISIBILITY, 50))
	end,
        ["desc"] =	{
                        "Grants invisibility"
        }
}

POISONBLOOD = add_spell
{
	["name"] = 	"Poison Blood",
        ["school"] = 	{SCHOOL_AIR},
        ["level"] = 	12,
        ["mana"] = 	10,
        ["mana_max"] = 	20,
        ["fail"] = 	70,
        ["spell"] = 	function()
                       	if player.oppose_pois == 0 then set_oppose_pois(randint(30) + 25 + get_level(POISONBLOOD, 25)) end
                       	if (player.tim_poison == 0) and (get_level(POISONBLOOD, 50) >= 15) then set_poison(randint(30) + 25 + get_level(POISONBLOOD, 25)) end
       	end,
	["info"] = 	function()
			return "dur "..(25 + get_level(POISONBLOOD, 25)).."+d30"
	end,
        ["desc"] =	{
                        "Grants resist poison",
                        "At level 15 it provides poison branding to wielded weapon"
        }
}

THUNDERSTORM = add_spell
{
	["name"] = 	"Thunderstorm",
        ["school"] = 	{SCHOOL_AIR, SCHOOL_NATURE},
        ["level"] = 	25,
        ["mana"] = 	40,
        ["mana_max"] = 	60,
        ["fail"] = 	70,
        ["spell"] = 	function()
                       	if player.tim_thunder == 0 then set_tim_thunder(randint(10) + 10 + get_level(THUNDERSTORM, 25), 5 + get_level(THUNDERSTORM, 10), 10 + get_level(THUNDERSTORM, 25)) end
       	end,
	["info"] = 	function()
			return "dam "..(5 + get_level(THUNDERSTORM, 10)).."d"..(10 + get_level(THUNDERSTORM, 25)).." dur "..(10 + get_level(THUNDERSTORM, 25)).."+d10"
	end,
        ["desc"] =	{
                        "Charges up the air around you with electricity",
                        "Each turns it will throw a thunder bolt at a random monster in sight",
                        "The thunder does 3 types of damage, one third of lightning",
                        "one third of sound and one third of light"
        }
}
