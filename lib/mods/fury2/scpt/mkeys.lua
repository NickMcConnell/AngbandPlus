-- Mkeys for skills & abilities
BOOK_AIR = 2
BOOK_MOVE = 5
BOOK_ENERGY = 68
BOOK_FIRE = 1
BOOK_WATER = 4
BOOK_TREE = 6
BOOK_EARTH = 3
BOOK_KNOW = 7 
BOOK_TEMP = 8
BOOK_MIND = 10
BOOK_META = 9
BOOK_UDUN = 11
BOOK_MASTER = 67
BOOK_JEDI = 89
BOOK_NINJA = 99
BOOK_PHASING = 77
BOOK_HUNTING = 79
BOOK_MINDBLADE = 97
BOOK_CONST = 98
GF_INSTA_DEATH = add_spell_type
{
	["color"]       = { TERM_DARK, 0 },
	["angry"]       = function() return TRUE, TRUE end,
	["monster"]     = function(who, dam, rad, y, x, monst)
			local race = race_info_idx(monst.r_idx, monst.ego)
			if magik(5) == FALSE or band(race.flags1, RF1_UNIQUE) ~= FALSE or band(race.flags3, RF3_UNDEAD) ~= FALSE or band(race.flags3, RF3_NONLIVING) ~= FALSE then
				return TRUE, FALSE
			else
				-- Reduce the exp gained this way
				monst.level = monst.level / 3
				return TRUE, FALSE, 32535, 0, 0, 0, 0, 0, 0, 0, " faints.", " is sucked out of life."
			end
	end,
}

-- Death touch ability
add_mkey
{
	["mkey"] 	= 100,
	["fct"] 	= function()
			if player.csp > 40 then
				increase_mana(-40)
				set_project(randint(30) + 10, GF_INSTA_DEATH, 1, 0, bor(PROJECT_STOP, PROJECT_KILL))
				energy_use = 100
			else
				msg_print("You need at least 40 mana.")
			end
	end,
}


-- Geomancy skill
add_mkey
{
	["mkey"] 	= 101,
	["fct"] 	= function()
			local s

			-- No magic
			if (player.antimagic > 0) then
				msg_print("Your anti-magic field disrupts any magic attempts.")
				return
			end

			local obj = get_object(INVEN_WIELD)
			if (obj.k_idx <= 0) or (obj.tval ~= TV_MSTAFF) then
				msg_print('You must wield a magestaff to use Geomancy.')
				return
			end

			s = get_school_spell("cast", "is_ok_spell", 62);

			-- Actualy cast the choice
			if (s ~= -1) then
				cast_school_spell(s, spell(s))
			end
	end,
}

-- Far reaching attack of polearms
add_mkey
{
	["mkey"]	= 102,
	["fct"]	 = function()
			local weapon = get_object(INVEN_WIELD);
			if weapon.tval == TV_POLEARM and (weapon.sval == SV_HALBERD or weapon.sval == SV_PIKE or weapon.sval == SV_HEAVY_LANCE or weapon.sval == SV_LANCE) then
			else
				msg_print("You will need a long polearm for this!")
				return
			end

			ret, dir = get_rep_dir()
			if ret == FALSE then return end

			local dy, dx = explode_dir(dir)
			dy = dy * 2
			dx = dx * 2
		    	targety = player.py + dy
		    	targetx = player.px + dx

			local max_blows = get_skill_scale(SKILL_POLEARM, player.num_blow / 2)
			if max_blows == 0 then max_blows = 1 end

			if get_skill(SKILL_POLEARM) >= 40 then
				energy_use = energy_use + 200
				return project(0, 0, targety, targetx, max_blows, GF_ATTACK, bor(PROJECT_BEAM, PROJECT_KILL))
			else
				energy_use = energy_use + 200
				return project(0, 0, targety, targetx, max_blows, GF_ATTACK, bor(PROJECT_BEAM, PROJECT_STOP, PROJECT_KILL))
			end
	end,
}




add_mkey
{
	["mkey"] = 104,
	["fct"] = function()
		local s

		-- No magic
		if (player.antimagic > 0) then
			msg_print("Your anti-magic field disrupts any magic attempts.")
			return
		end

		s = get_school_spell("cast", "is_ok_spell", BOOK_AIR)
		-- Actualy cast the choice
		if (s ~= -1) then
			cast_school_spell(s, spell(s))
		end
	end,
}

add_mkey
{
	["mkey"] = 186,
	["fct"] = function()
		local s

		-- No magic
		if (player.antimagic > 0) then
			msg_print("Your anti-magic field disrupts any magic attempts.")
			return
		end

		s = get_school_spell("cast", "is_ok_spell", BOOK_ENERGY)
		-- Actualy cast the choice
		if (s ~= -1) then
			cast_school_spell(s, spell(s))
		end
	end,
}

add_mkey
{
	["mkey"] = 106,
	["fct"] = function()
		local s

		-- No magic
		if (player.antimagic > 0) then
			msg_print("Your anti-magic field disrupts any magic attempts.")
			return
		end

		s = get_school_spell("cast", "is_ok_spell", BOOK_MOVE)
		-- Actualy cast the choice
		if (s ~= -1) then
			cast_school_spell(s, spell(s))
		end
	end,
}

add_mkey
{
	["mkey"] = 107,
	["fct"] = function()
		local s

		-- No magic
		if (player.antimagic > 0) then
			msg_print("Your anti-magic field disrupts any magic attempts.")
			return
		end

		s = get_school_spell("cast", "is_ok_spell", BOOK_FIRE)
		-- Actualy cast the choice
		if (s ~= -1) then
			cast_school_spell(s, spell(s))
		end
	end,
}

add_mkey
{
	["mkey"] = 108,
	["fct"] = function()
		local s

		-- No magic
		if (player.antimagic > 0) then
			msg_print("Your anti-magic field disrupts any magic attempts.")
			return
		end

		s = get_school_spell("cast", "is_ok_spell", BOOK_WATER)
		-- Actualy cast the choice
		if (s ~= -1) then
			cast_school_spell(s, spell(s))
		end
	end,
}

add_mkey
{
	["mkey"] = 109,
	["fct"] = function()
		local s

		-- No magic
		if (player.antimagic > 0) then
			msg_print("Your anti-magic field disrupts any magic attempts.")
			return
		end

		s = get_school_spell("cast", "is_ok_spell", BOOK_TREE)
		-- Actualy cast the choice
		if (s ~= -1) then
			cast_school_spell(s, spell(s))
		end
	end,
}

add_mkey
{
	["mkey"] = 110,
	["fct"] = function()
		local s

		-- No magic
		if (player.antimagic > 0) then
			msg_print("Your anti-magic field disrupts any magic attempts.")
			return
		end

		s = get_school_spell("cast", "is_ok_spell", BOOK_EARTH)
		-- Actualy cast the choice
		if (s ~= -1) then
			cast_school_spell(s, spell(s))
		end
	end,
}

add_mkey
{
	["mkey"] = 111,
	["fct"] = function()
		local s

		-- No magic
		if (player.antimagic > 0) then
			msg_print("Your anti-magic field disrupts any magic attempts.")
			return
		end

		s = get_school_spell("cast", "is_ok_spell", BOOK_KNOW)
		-- Actualy cast the choice
		if (s ~= -1) then
			cast_school_spell(s, spell(s))
		end
	end,
}

add_mkey
{
	["mkey"] = 112,
	["fct"] = function()
		local s

		-- No magic
		if (player.antimagic > 0) then
			msg_print("Your anti-magic field disrupts any magic attempts.")
			return
		end

		s = get_school_spell("cast", "is_ok_spell", BOOK_TEMP)
		-- Actualy cast the choice
		if (s ~= -1) then
			cast_school_spell(s, spell(s))
		end
	end,
}

add_mkey
{
	["mkey"] = 113,
	["fct"] = function()
		local s

		-- No magic
		if (player.antimagic > 0) then
			msg_print("Your anti-magic field disrupts any magic attempts.")
			return
		end

		s = get_school_spell("cast", "is_ok_spell", BOOK_META)
		-- Actualy cast the choice
		if (s ~= -1) then
			cast_school_spell(s, spell(s))
		end
	end,
}

add_mkey
{
	["mkey"] = 114,
	["fct"] = function()
		local s

		-- No magic
		if (player.antimagic > 0) then
			msg_print("Your anti-magic field disrupts any magic attempts.")
			return
		end

		s = get_school_spell("cast", "is_ok_spell", BOOK_MIND)
		-- Actualy cast the choice
		if (s ~= -1) then
			cast_school_spell(s, spell(s))
		end
	end,
}

add_mkey
{
	["mkey"] = 115,
	["fct"] = function()
		local s
	
		-- No magic
		if (player.antimagic > 0) then
			msg_print("Your anti-magic field disrupts any magic attempts.")
			return
		end

		s = get_school_spell("cast", "is_ok_spell", BOOK_UDUN)
	
		-- Actualy cast the choice
		if (s ~= -1) then
			cast_school_spell(s, spell(s))
		end
	end,
}

add_mkey
{
	["mkey"] = 119,
	["fct"] = function()
		local s
		-- No magic
		if (player.antimagic > 0) then
			msg_print("Your anti-magic field disrupts any magic attempts.")
			return
		end
		
		s = get_school_spell("cast", "is_ok_spell", BOOK_MASTER)
		-- Actualy cast the choice
		if (s ~= -1) then
			cast_school_spell(s, spell(s))
		end
	end,
}

add_mkey
{
	["mkey"] = 678,
	["fct"] = function()
		local s
		-- No magic
		if (player.antimagic > 0) then
			msg_print("Your anti-magic field disrupts any magic attempts.")
			return
		end
		
		s = get_school_spell("cast", "is_ok_spell", BOOK_ALCHEMY)
		-- Actualy cast the choice
		if (s ~= -1) then
			cast_school_spell(s, spell(s))
		end
	end,
}

add_mkey
{
	["mkey"] = 891,
	["fct"] = function()
		local s
		-- No magic
		if (player.antimagic > 0) then
			msg_print("Your anti-magic field disrupts any magic attempts.")
			return
		end
		
		s = get_school_spell("cast", "is_ok_spell", BOOK_JEDI)
		-- Actualy cast the choice
		if (s ~= -1) then
			cast_school_spell(s, spell(s))
		end
	end,
}


add_mkey
{
	["mkey"] = 99,
	["fct"] = function()
		local s
		
		
		s = get_school_spell("cast", "is_ok_spell", BOOK_NINJA)
		-- Actualy cast the choice
		if (s ~= -1) then
			cast_school_spell(s, spell(s))
		end
	end,
}

add_mkey
{
	["mkey"] = 689,
	["fct"] = function()
		local s
				-- No magic
		if (player.antimagic > 0) then
			msg_print("Your anti-magic field disrupts any magic attempts.")
			return
		end
		s = get_school_spell("cast", "is_ok_spell", BOOK_PHASING)
		-- Actualy cast the choice
		if (s ~= -1) then
			cast_school_spell(s, spell(s))
		end
	end,
}

add_mkey
{
	["mkey"] = 690,
	["fct"] = function()
		local s
				-- No magic
	
		s = get_school_spell("cast", "is_ok_spell", BOOK_HUNTING)
		-- Actualy cast the choice
		if (s ~= -1) then
			cast_school_spell(s, spell(s))
		end
	end,
}

add_mkey
{
	["mkey"] = 691,
	["fct"] = function()
		local s
				-- No magic
	
		s = get_school_spell("cast", "is_ok_spell", BOOK_MINDBLADE)
		-- Actualy cast the choice
		if (s ~= -1) then
			cast_school_spell(s, spell(s))
		end
	end,
}

add_mkey
{
	["mkey"] = 130,
	["fct"] = function()
		local s
				-- No magic
	
		s = get_school_spell("cast", "is_ok_spell", BOOK_CONST)
		-- Actualy cast the choice
		if (s ~= -1) then
			cast_school_spell(s, spell(s))
		end
	end,
}
