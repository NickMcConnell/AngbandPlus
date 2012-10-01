-- File: events.lua
-- This file contains various scripts that are called when something happens.
-- For example, when a monster attacks, moves, etc...

-- Monsters functions have two parameters. m_idx is the monster's number,
-- but the second one is a code parameter which tells which script is going
-- to be called. All event functions only calls the "monster_events_code"
-- function, which can do anything you want.


-- PLAYER EVENTS

-- For the Monster race special event codes, the "auraon" variable is used,
-- which is the same used for Elemental Lord's aura.

function before_player_move (x, y)

	-- Before move scripts of Monster race.
	if (m_race(p_ptr.body_monster).event_before_move > 0 and (p_ptr.auraon)) then

		 player_events_code(m_race(p_ptr.body_monster).event_before_move)
	end

end

function after_player_move (x, y)

	-- Ranger's Wilderness Lore
	if (p_ptr.abilities[(CLASS_RANGER * 10) + 1] >= 1) then

		local rad
		rad = p_ptr.abilities[(CLASS_RANGER * 10) + 1] + 3
        	reveal_spell(px, py, rad)
	end
	
	-- After move scripts of Monster race.
	if (m_race(p_ptr.body_monster).event_after_move > 0 and (p_ptr.auraon)) then

		player_events_code(m_race(p_ptr.body_monster).event_after_move)
	end

	-- Handle songs.
	if (p_ptr.events[29042] == 1) then

		-- If Music skill becomes 0, turn off singning.
		if (p_ptr.skill[29] == 0) then

			p_ptr.events[29042] = 0
		else

			execute_song(p_ptr.events[29041])
		end
	end

	-- Portals of the Flow.
	-- Are we standing on the Strange Portal?
	if (cave(py, px).feat == 235) then

		local ch

		msg_print("Enter the strange portal? [y/n]")
		ch = inkey()

		-- Characters 89 and 121 are "Y" and "y"
                if (ch == 89 or ch == 121) then

			local klev
			if (global_object.tval == TV_ESSENCE) then

				klev = m_race(global_object.pval).level + (object_skill_points_value(global_object) / 5)
			else
				klev = kind(global_object).level + (object_skill_points_value(global_object) / 5)
			end

			msg_print("Now entering The Flow...")

			-- Prepare the dungeon.
			if (klev == 0) then
				prepare_flow_dungeon(1)
			else
				prepare_flow_dungeon(klev)
			end

			p_ptr.oldpx = 99
                        p_ptr.oldpy = 32

			dungeon_type = 201

			p_ptr.events[29033] = 0
			p_ptr.events[29034] = 0

			if (klev == 0) then
				dun_level = 1
			else
				dun_level = klev
			end

			p_ptr.leaving = TRUE
                end
	end

	-- Portal of Power
	if (cave(py, px).feat == 236) then

		local ch

		msg_print("Enter the Portal of Power? [y/n]")
		ch = inkey()

		-- Characters 89 and 121 are "Y" and "y"
                if (ch == 89 or ch == 121) then

			msg_print("You enter the portal...")

			global_object.xtra1 = global_object.xtra1 + 2

			global_object.to_h = global_object.to_h + 5
			global_object.to_d = global_object.to_d + 5
			global_object.to_h = global_object.to_h + multiply_divide(global_object.to_h, 10, 100)
			global_object.to_d = global_object.to_d + multiply_divide(global_object.to_d, 10, 100)

			p_ptr.events[29033] = p_ptr.events[29033] + 2

			if (lua_mod(p_ptr.events[29033], 10) == 0) then
				if (global_object.dd < 4 and maxroll(global_object.dd, global_object.ds) >= 10) then
					global_object.dd = global_object.dd + 1
				else
					global_object.dd = global_object.dd + multiply_divide(global_object.dd, 25, 100)
				end
				global_object.ds = global_object.ds + multiply_divide(global_object.ds, 25, 100)
			end

			if (global_object.tval == TV_RANGED) then
				global_object.extra4 = global_object.extra4 + 5
				global_object.extra4 = global_object.extra4 + multiply_divide(global_object.extra4, 2, 100)
			end

			if (global_object.branddam > 0) then global_object.branddam = global_object.branddam + multiply_divide(global_object.branddam, 25, 100) end

			-- Prepare the dungeon.
			prepare_flow_dungeon(dun_level + 2)

			dungeon(201).mindepth = dun_level + 2
			dungeon(201).maxdepth = dun_level + 2

			dun_level = dun_level + 2

			-- There is a chance we'll find a special level instead.
			if (lua_randint(100) <= 15) then
				msg_print("Entering special level!")
				p_ptr.events[29034] = dun_level
				p_ptr.inside_quest = 9002
			end

			p_ptr.leaving = TRUE
                end
	end
	-- Portal of the Shield
	if (cave(py, px).feat == 237) then

		local ch

		msg_print("Enter the Portal of The Shield? [y/n]")
		ch = inkey()

		-- Characters 89 and 121 are "Y" and "y"
                if (ch == 89 or ch == 121) then

			msg_print("You enter the portal...")

			global_object.xtra1 = global_object.xtra1 + 2

			global_object.to_a = global_object.to_a + 5
			global_object.to_a = global_object.to_a + multiply_divide(global_object.to_a, 10, 100)
			if (global_object.tval == TV_HARD_ARMOR or global_object.tval == TV_DRAG_ARMOR) then global_object.ac = global_object.ac + 2
			else global_object.ac = global_object.ac + 1 end
			global_object.ac = global_object.ac + multiply_divide(global_object.ac, 10, 100)

			-- Prepare the dungeon.
			prepare_flow_dungeon(dun_level + 2)

			dungeon(201).mindepth = dun_level + 2
			dungeon(201).maxdepth = dun_level + 2

			dun_level = dun_level + 2

			-- There is a chance we'll find a special level instead.
			if (lua_randint(100) <= 15) then
				msg_print("Entering special level!")
				p_ptr.events[29034] = dun_level
				p_ptr.inside_quest = 9002
			end

			p_ptr.leaving = TRUE
                end
	end
	-- Portal of Skill
	if (cave(py, px).feat == 238) then

		local ch

		msg_print("Enter the Portal of Skill? [y/n]")
		ch = inkey()

		-- Characters 89 and 121 are "Y" and "y"
                if (ch == 89 or ch == 121) then

			msg_print("You enter the portal...")

			global_object.xtra1 = global_object.xtra1 + 2

			global_object.tweakpoints = global_object.tweakpoints + 2

			-- Prepare the dungeon.
			prepare_flow_dungeon(dun_level + 2)

			dungeon(201).mindepth = dun_level + 2
			dungeon(201).maxdepth = dun_level + 2

			dun_level = dun_level + 2

			-- There is a chance we'll find a special level instead.
			if (lua_randint(100) <= 15) then
				msg_print("Entering special level!")
				p_ptr.events[29034] = dun_level
				p_ptr.inside_quest = 9002
			end

			p_ptr.leaving = TRUE
                end
	end
	-- Portal of Resistance
	if (cave(py, px).feat == 239) then

		local ch

		msg_print("Enter the Portal of Resistance? [y/n]")
		ch = inkey()

		-- Characters 89 and 121 are "Y" and "y"
                if (ch == 89 or ch == 121) then

			local whichres

			msg_print("You enter the portal...")

			whichres = lua_randint(14)

			global_object.xtra1 = global_object.xtra1 + 2

			global_object.resistances[whichres+1] = global_object.resistances[whichres+1] + 5

			-- Prepare the dungeon.
			prepare_flow_dungeon(dun_level + 2)

			dungeon(201).mindepth = dun_level + 2
			dungeon(201).maxdepth = dun_level + 2

			dun_level = dun_level + 2

			-- There is a chance we'll find a special level instead.
			if (lua_randint(100) <= 15) then
				msg_print("Entering special level!")
				p_ptr.events[29034] = dun_level
				p_ptr.inside_quest = 9002
			end

			p_ptr.leaving = TRUE
                end
	end
	-- Final Portal
	if (cave(py, px).feat == 240) then

		local ch

		msg_print("Enter the Final Portal? [y/n]")
		ch = inkey()

		-- Characters 89 and 121 are "Y" and "y"
                if (ch == 89 or ch == 121) then

			msg_print("You enter the portal...")

			global_object.xtra1 = global_object.xtra1 + 2

			global_object.to_h = global_object.to_h + 5
			global_object.to_d = global_object.to_d + 5
			global_object.to_h = global_object.to_h + multiply_divide(global_object.to_h, 10, 100)
			global_object.to_d = global_object.to_d + multiply_divide(global_object.to_d, 10, 100)
			global_object.to_a = global_object.to_a + multiply_divide(global_object.to_a, 10, 100)
			global_object.ac = global_object.ac + multiply_divide(global_object.ac, 10, 100)

			if (global_object.dd < 4 and maxroll(global_object.dd, global_object.ds) >= 10) then
				global_object.dd = global_object.dd + 1
			else
				global_object.dd = global_object.dd + multiply_divide(global_object.dd, 25, 100)
			end
			global_object.ds = global_object.ds + multiply_divide(global_object.ds, 25, 100)

			if (global_object.tval == TV_RANGED) then
				global_object.extra4 = global_object.extra4 + 5
				global_object.extra4 = global_object.extra4 + multiply_divide(global_object.extra4, 2, 100)
			end

			if (global_object.branddam > 0) then global_object.branddam = global_object.branddam + multiply_divide(global_object.branddam, 25, 100) end

			global_object.tweakpoints = global_object.tweakpoints + 2

			dun_level = dungeon(201).maxdepth + 2
			p_ptr.events[29034] = dun_level

			p_ptr.leaving = TRUE
                end
	end
	-- Portal (used in special levels, etc...)
	if (cave(py, px).feat == 241) then

		local ch

		msg_print("Enter the Portal? [y/n]")
		ch = inkey()

		-- Characters 89 and 121 are "Y" and "y"
                if (ch == 89 or ch == 121) then

			msg_print("You enter the portal...")

			dun_level = p_ptr.events[29034]

			p_ptr.inside_quest = 0
			dungeon(201).mindepth = dun_level
			dungeon(201).maxdepth = dun_level

			-- Prepare the dungeon.
			prepare_flow_dungeon(dun_level)

			p_ptr.leaving = TRUE
                end
	end
end

-- Before and after melee occurs for every blows.
function player_before_melee ()

	-- Before melee scripts of Monster race.
	if (m_race(p_ptr.body_monster).event_before_melee > 0 and (p_ptr.auraon)) then

		 player_events_code(m_race(p_ptr.body_monster).event_before_melee)
	end
end

function player_after_melee (dam)

	-- Take damages scripts of Monster race.
	if (m_race(p_ptr.body_monster).event_after_melee > 0 and (p_ptr.auraon)) then

		 player_events_code(m_race(p_ptr.body_monster).event_after_melee)
	end
end

-- Before and after melee occurs for every shots.
function player_before_ranged ()

	-- Before ranged scripts of Monster race.
	if (m_race(p_ptr.body_monster).event_before_ranged > 0 and (p_ptr.auraon)) then

		 player_events_code(m_race(p_ptr.body_monster).event_before_ranged)
	end
end

function player_after_ranged (dam)

	-- After ranged scripts of Monster race.
	if (m_race(p_ptr.body_monster).event_after_ranged > 0 and (p_ptr.auraon)) then

		 player_events_code(m_race(p_ptr.body_monster).event_after_ranged)
	end
end

function player_before_magic ()

	-- Before magic scripts of Monster race.
	if (m_race(p_ptr.body_monster).event_before_magic > 0 and (p_ptr.auraon)) then

		 player_events_code(m_race(p_ptr.body_monster).event_before_magic)
	end
end

function player_after_magic ()

	-- After magic scripts of Monster race.
	if (m_race(p_ptr.body_monster).event_after_magic > 0 and (p_ptr.auraon)) then

		 player_events_code(m_race(p_ptr.body_monster).event_after_magic)
	end
end

function player_take_damages (dam)

	-- Take damages scripts of Monster race.
	if (m_race(p_ptr.body_monster).event_take_damages > 0 and (p_ptr.auraon)) then

		 player_events_code(m_race(p_ptr.body_monster).event_take_damages)
	end
end

function player_skip_turn ()

	-- Handle songs.
	if (p_ptr.events[29042] == 1) then

		-- If Music skill becomes 0, turn off singning.
		if (p_ptr.skill[29] == 0) then

			p_ptr.events[29042] = 0
		else

			execute_song(p_ptr.events[29041])
		end
	end
	
end

-- Events when entering a new dungeon.
function player_enter_dungeon (dnum)
	
end

-- Events before a new floor is created.
function player_before_floor (dnum)

end

-- Events after creation of a new floor.
function player_after_floor (dnum)

	local x
	local y
	local placed

	-- If we're in the Flow, place some portals.
	if (dnum == 201) then

		local klev
		klev = kind(global_object).level
		if (global_object.tval == TV_ESSENCE) then

			klev = m_race(global_object.pval).level + (object_skill_points_value(global_object) / 5)
		else
			klev = kind(global_object).level + (object_skill_points_value(global_object) / 5)
		end

		if (klev <= 0) then klev = 1 end

		-- We may place some Final Portals or not. It depends on how deep you are, and on the item.
		-- But never place Final Portals on the first level.
		if (dun_level > (klev + (klev / 2))) then

			if (lua_randint(dun_level - klev) >= lua_randint(klev * 2)) then

				placed = 0
				while (placed < 20) do
					x = lua_randint(cur_wid-1)
					y = lua_randint(cur_hgt-1)

					if (x <= 0) then x = 1 end
					if (y <= 0) then y = 1 end

					if (cave(y, x).feat == FEAT_FLOOR or cave(y, x).feat == FEAT_GRASS or cave(y, x).feat == FEAT_SNOW or cave(y, x).feat == FEAT_SHAL_WATER or cave(y, x).feat == FEAT_SHAL_LAVA or cave(y, x).feat == FEAT_DEEP_WATER or cave(y, x).feat == FEAT_DEEP_LAVA or cave(y, x).feat == FEAT_DIRT) then

						cave_set_feat(y, x, 240)
						placed = placed + 1
					end
				end
				return
			end
		end

		-- Place 5 Portal of Power.
		placed = 0
		while (placed < 5) do
			x = lua_randint(cur_wid-1)
			y = lua_randint(cur_hgt-1)

			if (x <= 0) then x = 1 end
			if (y <= 0) then y = 1 end

			if (cave(y, x).feat == FEAT_FLOOR or cave(y, x).feat == FEAT_GRASS or cave(y, x).feat == FEAT_SNOW or cave(y, x).feat == FEAT_SHAL_WATER or cave(y, x).feat == FEAT_SHAL_LAVA or cave(y, x).feat == FEAT_DEEP_WATER or cave(y, x).feat == FEAT_DEEP_LAVA or cave(y, x).feat == FEAT_DIRT) then

				cave_set_feat(y, x, 236)
				placed = placed + 1
			end
		end
		-- Place 5 Portal of the Shield.
		placed = 0
		while (placed < 5) do
			x = lua_randint(cur_wid-1)
			y = lua_randint(cur_hgt-1)

			if (x <= 0) then x = 1 end
			if (y <= 0) then y = 1 end

			if (cave(y, x).feat == FEAT_FLOOR or cave(y, x).feat == FEAT_GRASS or cave(y, x).feat == FEAT_SNOW or cave(y, x).feat == FEAT_SHAL_WATER or cave(y, x).feat == FEAT_SHAL_LAVA or cave(y, x).feat == FEAT_DEEP_WATER or cave(y, x).feat == FEAT_DEEP_LAVA or cave(y, x).feat == FEAT_DIRT) then

				cave_set_feat(y, x, 237)
				placed = placed + 1
			end
		end
		-- Place 5 Portal of Skill.
		placed = 0
		while (placed < 5) do
			x = lua_randint(cur_wid-1)
			y = lua_randint(cur_hgt-1)

			if (x <= 0) then x = 1 end
			if (y <= 0) then y = 1 end

			if (cave(y, x).feat == FEAT_FLOOR or cave(y, x).feat == FEAT_GRASS or cave(y, x).feat == FEAT_SNOW or cave(y, x).feat == FEAT_SHAL_WATER or cave(y, x).feat == FEAT_SHAL_LAVA or cave(y, x).feat == FEAT_DEEP_WATER or cave(y, x).feat == FEAT_DEEP_LAVA or cave(y, x).feat == FEAT_DIRT) then

				cave_set_feat(y, x, 238)
				placed = placed + 1
			end
		end
		-- Place 5 Portals of Resistance.
		placed = 0
		while (placed < 5) do
			x = lua_randint(cur_wid-1)
			y = lua_randint(cur_hgt-1)

			if (x <= 0) then x = 1 end
			if (y <= 0) then y = 1 end

			if (cave(y, x).feat == FEAT_FLOOR or cave(y, x).feat == FEAT_GRASS or cave(y, x).feat == FEAT_SNOW or cave(y, x).feat == FEAT_SHAL_WATER or cave(y, x).feat == FEAT_SHAL_LAVA or cave(y, x).feat == FEAT_DEEP_WATER or cave(y, x).feat == FEAT_DEEP_LAVA or cave(y, x).feat == FEAT_DIRT) then

				cave_set_feat(y, x, 239)
				placed = placed + 1
			end
		end
		
	end
end

-- Events code for players who are Monsters.
function player_events_code (eventcode)

	-- Code 1: Complete regeneration.
	if (eventcode == 1) then

		if (p_ptr.chp < p_ptr.mhp) then

			msg_print("You regenerate!")
			p_ptr.chp = p_ptr.mhp

			update_and_handle()
		end
	end

	-- Code 2: Becomes friendly if your alignment is 0 or higher.
	-- No effects with players.

	-- Code 3: Place ice fields around the monster.
	-- Used by Spirit of Blizzard.
	if (eventcode == 3) then

		local fdam
		local damstat

		if (p_ptr.stat_ind[A_INT+1] >= p_ptr.stat_ind[A_WIS+1]) then
			damstat = p_ptr.stat_ind[A_INT+1] - 5
		else
			damstat = p_ptr.stat_ind[A_WIS+1] - 5
		end

		fdam = (10 * damstat)
		fdam = fdam + multiply_divide(fdam, p_ptr.skill[2] * 10, 100)
		fdam = fdam + multiply_divide(fdam, p_ptr.abilities[(CLASS_MONSTER * 10) + 8] * 50, 100)

		place_field(FEAT_COLD_FIELD, 5, px, py, fdam)
		update_and_handle()
	end

	-- Code 4: Place ice fields around the monster.
	-- Same thing as 3. Only stronger.
	if (eventcode == 4) then

		local fdam
		local damstat

		if (p_ptr.stat_ind[A_INT+1] >= p_ptr.stat_ind[A_WIS+1]) then
			damstat = p_ptr.stat_ind[A_INT+1] - 5
		else
			damstat = p_ptr.stat_ind[A_WIS+1] - 5
		end

		fdam = (15 * damstat)
		fdam = fdam + multiply_divide(fdam, p_ptr.skill[2] * 10, 100)
		fdam = fdam + multiply_divide(fdam, p_ptr.abilities[(CLASS_MONSTER * 10) + 8] * 50, 100)

		place_field(FEAT_COLD_FIELD, 5, px, py, fdam)
		update_and_handle()
	end

	-- Code 5: Place ice fields around the monster.
	-- Same thing as 4. Only stronger.
	-- Yes, I like this enemy. :)
	if (eventcode == 5) then

		local fdam
		local damstat

		if (p_ptr.stat_ind[A_INT+1] >= p_ptr.stat_ind[A_WIS+1]) then
			damstat = p_ptr.stat_ind[A_INT+1] - 5
		else
			damstat = p_ptr.stat_ind[A_WIS+1] - 5
		end

		fdam = (20 * damstat)
		fdam = fdam + multiply_divide(fdam, p_ptr.skill[2] * 10, 100)
		fdam = fdam + multiply_divide(fdam, p_ptr.abilities[(CLASS_MONSTER * 10) + 8] * 50, 100)

		place_field(FEAT_COLD_FIELD, 6, px, py, fdam)
		update_and_handle()
	end

	-- An aura of Harm!
	if (eventcode == 6) then

		local sdam
		local damstat

		if (p_ptr.stat_ind[A_INT+1] >= p_ptr.stat_ind[A_WIS+1]) then
			damstat = p_ptr.stat_ind[A_INT+1] - 5
		else
			damstat = p_ptr.stat_ind[A_WIS+1] - 5
		end

		sdam = (40 * damstat)
		sdam = sdam + multiply_divide(sdam, p_ptr.skill[2] * 10, 100)
		sdam = sdam + multiply_divide(sdam, p_ptr.abilities[(CLASS_MONSTER * 10) + 8] * 50, 100)

		no_magic_return = TRUE
		attack_aura(162, sdam, 5)
		no_magic_return = FALSE

		update_and_handle()
	end

	-- Leap away(distance 3)
	if (eventcode == 7) then

		local ch
		local rad

		msg_print(string.format('Leap away? (Radius %d) [y/n]', 3 + (p_ptr.abilities[(CLASS_MONSTER * 10) + 8] / 10)))

		ch = inkey()

		-- Characters 89 and 121 are "Y" and "y"
                if (ch == 89 or ch == 121) then
		
			local x
			local y

			-- Actually jump!
			-- We use a special function that returns x and y coordinates in global
			-- variables, so we can use these in Lua.
			msg_print("You jump very high!")
        		if (not(lua_tgt_pt())) then return end
			x = global_x
			y = global_y

			-- Most functions here use a (y,x) format, instead of (x,y).
			-- This is important, because if you use (x,y), it might crash.
        		if (not(lua_cave_empty_bold(y,x)) or (get_cave_info_flag(y, x, CAVE_ICKY)) or (distance(y,x,py,px) > 3 + (p_ptr.abilities[(CLASS_MONSTER * 10) + 8] / 10))) then

              			msg_print("You can't jump there...")

        		else

                		if (not(get_cave_info_flag(y, x, CAVE_MARK))) then

                        		if (get_cave_info_flag(y, x, CAVE_LITE)) then

						teleport_player_to(y,x)
                        		else
						msg_print("You can't jump there...")
					end

                		else

					teleport_player_to(y,x)
				end
        		end
		end
	end

	-- An aura of Warp!
	if (eventcode == 8) then

		local sdam
		local damstat

		if (p_ptr.stat_ind[A_INT+1] >= p_ptr.stat_ind[A_WIS+1]) then
			damstat = p_ptr.stat_ind[A_INT+1] - 5
		else
			damstat = p_ptr.stat_ind[A_WIS+1] - 5
		end

		sdam = (50 * damstat)
		sdam = sdam + multiply_divide(sdam, p_ptr.skill[2] * 10, 100)
		sdam = sdam + multiply_divide(sdam, p_ptr.abilities[(CLASS_MONSTER * 10) + 8] * 50, 100)

		no_magic_return = TRUE
		attack_aura(GF_WARP, sdam, 5)
		no_magic_return = FALSE

		update_and_handle()
	end

	-- Teleports away.
	if (eventcode == 9) then

		msg_print("You teleport!")
		teleport_player(10 + p_ptr.abilities[(CLASS_MONSTER * 10) + 8])
	end

	-- An aura of Life Blast(10%).
	if (eventcode == 10) then

		no_magic_return = TRUE
		attack_aura(GF_LIFE_BLAST, 10, 10)
		no_magic_return = FALSE

		update_and_handle()
	end

	-- An aura of Fire!
	if (eventcode == 11) then

		local sdam
		local damstat

		if (p_ptr.stat_ind[A_INT+1] >= p_ptr.stat_ind[A_WIS+1]) then
			damstat = p_ptr.stat_ind[A_INT+1] - 5
		else
			damstat = p_ptr.stat_ind[A_WIS+1] - 5
		end

		sdam = (20 * damstat)
		sdam = sdam + multiply_divide(sdam, p_ptr.skill[2] * 10, 100)
		sdam = sdam + multiply_divide(sdam, p_ptr.abilities[(CLASS_MONSTER * 10) + 8] * 50, 100)

		no_magic_return = TRUE
		attack_aura(GF_FIRE, sdam, 3)
		no_magic_return = FALSE

		update_and_handle()
	end

	-- An aura of Cold!
	if (eventcode == 12) then

		local sdam
		local damstat

		if (p_ptr.stat_ind[A_INT+1] >= p_ptr.stat_ind[A_WIS+1]) then
			damstat = p_ptr.stat_ind[A_INT+1] - 5
		else
			damstat = p_ptr.stat_ind[A_WIS+1] - 5
		end

		sdam = (20 * damstat)
		sdam = sdam + multiply_divide(sdam, p_ptr.skill[2] * 10, 100)
		sdam = sdam + multiply_divide(sdam, p_ptr.abilities[(CLASS_MONSTER * 10) + 8] * 50, 100)

		no_magic_return = TRUE
		attack_aura(GF_COLD, sdam, 3)
		no_magic_return = FALSE

		update_and_handle()
	end

	-- An aura of Elemental!
	if (eventcode == 13) then

		local sdam
		local damstat

		if (p_ptr.stat_ind[A_INT+1] >= p_ptr.stat_ind[A_WIS+1]) then
			damstat = p_ptr.stat_ind[A_INT+1] - 5
		else
			damstat = p_ptr.stat_ind[A_WIS+1] - 5
		end

		sdam = (30 * damstat)
		sdam = sdam + multiply_divide(sdam, p_ptr.skill[2] * 10, 100)
		sdam = sdam + multiply_divide(sdam, p_ptr.abilities[(CLASS_MONSTER * 10) + 8] * 50, 100)

		no_magic_return = TRUE
		attack_aura(GF_ELEMENTAL, sdam, 3)
		no_magic_return = FALSE

		update_and_handle()
	end

	-- An aura of Acid!
	if (eventcode == 14) then

		local sdam
		local damstat

		if (p_ptr.stat_ind[A_INT+1] >= p_ptr.stat_ind[A_WIS+1]) then
			damstat = p_ptr.stat_ind[A_INT+1] - 5
		else
			damstat = p_ptr.stat_ind[A_WIS+1] - 5
		end

		sdam = (20 * damstat)
		sdam = sdam + multiply_divide(sdam, p_ptr.skill[2] * 10, 100)
		sdam = sdam + multiply_divide(sdam, p_ptr.abilities[(CLASS_MONSTER * 10) + 8] * 50, 100)

		no_magic_return = TRUE
		attack_aura(GF_ACID, sdam, 3)
		no_magic_return = FALSE

		update_and_handle()
	end

	-- An aura of Water!
	if (eventcode == 15) then

		local sdam
		local damstat

		if (p_ptr.stat_ind[A_INT+1] >= p_ptr.stat_ind[A_WIS+1]) then
			damstat = p_ptr.stat_ind[A_INT+1] - 5
		else
			damstat = p_ptr.stat_ind[A_WIS+1] - 5
		end

		sdam = (20 * damstat)
		sdam = sdam + multiply_divide(sdam, p_ptr.skill[2] * 10, 100)
		sdam = sdam + multiply_divide(sdam, p_ptr.abilities[(CLASS_MONSTER * 10) + 8] * 50, 100)

		no_magic_return = TRUE
		attack_aura(GF_WATER, sdam, 3)
		no_magic_return = FALSE

		update_and_handle()
	end

	-- An aura of Earth!
	if (eventcode == 16) then

		local sdam
		local damstat

		if (p_ptr.stat_ind[A_INT+1] >= p_ptr.stat_ind[A_WIS+1]) then
			damstat = p_ptr.stat_ind[A_INT+1] - 5
		else
			damstat = p_ptr.stat_ind[A_WIS+1] - 5
		end

		sdam = (20 * damstat)
		sdam = sdam + multiply_divide(sdam, p_ptr.skill[2] * 10, 100)
		sdam = sdam + multiply_divide(sdam, p_ptr.abilities[(CLASS_MONSTER * 10) + 8] * 50, 100)

		no_magic_return = TRUE
		attack_aura(GF_EARTH, sdam, 3)
		no_magic_return = FALSE

		update_and_handle()
	end

	-- An aura of Wind!
	if (eventcode == 17) then

		local sdam
		local damstat

		if (p_ptr.stat_ind[A_INT+1] >= p_ptr.stat_ind[A_WIS+1]) then
			damstat = p_ptr.stat_ind[A_INT+1] - 5
		else
			damstat = p_ptr.stat_ind[A_WIS+1] - 5
		end

		sdam = (20 * damstat)
		sdam = sdam + multiply_divide(sdam, p_ptr.skill[2] * 10, 100)
		sdam = sdam + multiply_divide(sdam, p_ptr.abilities[(CLASS_MONSTER * 10) + 8] * 50, 100)

		no_magic_return = TRUE
		attack_aura(GF_WIND, sdam, 3)
		no_magic_return = FALSE

		update_and_handle()
	end

	-- An aura of Frostfire!
	if (eventcode == 18) then

		local sdam
		local damstat

		if (p_ptr.stat_ind[A_INT+1] >= p_ptr.stat_ind[A_WIS+1]) then
			damstat = p_ptr.stat_ind[A_INT+1] - 5
		else
			damstat = p_ptr.stat_ind[A_WIS+1] - 5
		end

		sdam = (20 * damstat)
		sdam = sdam + multiply_divide(sdam, p_ptr.skill[2] * 10, 100)
		sdam = sdam + multiply_divide(sdam, p_ptr.abilities[(CLASS_MONSTER * 10) + 8] * 50, 100)

		no_magic_return = TRUE
		attack_aura(GF_FROSTFIRE, sdam, 3)
		no_magic_return = FALSE

		update_and_handle()
	end

	-- An aura of Mud!
	if (eventcode == 19) then

		local sdam
		local damstat

		if (p_ptr.stat_ind[A_INT+1] >= p_ptr.stat_ind[A_WIS+1]) then
			damstat = p_ptr.stat_ind[A_INT+1] - 5
		else
			damstat = p_ptr.stat_ind[A_WIS+1] - 5
		end

		sdam = (20 * damstat)
		sdam = sdam + multiply_divide(sdam, p_ptr.skill[2] * 10, 100)
		sdam = sdam + multiply_divide(sdam, p_ptr.abilities[(CLASS_MONSTER * 10) + 8] * 50, 100)

		no_magic_return = TRUE
		attack_aura(GF_MUD, sdam, 3)
		no_magic_return = FALSE

		update_and_handle()
	end

	-- Fire fields + aura.
	if (eventcode == 20) then

		local fdam
		local damstat

		if (p_ptr.stat_ind[A_INT+1] >= p_ptr.stat_ind[A_WIS+1]) then
			damstat = p_ptr.stat_ind[A_INT+1] - 5
		else
			damstat = p_ptr.stat_ind[A_WIS+1] - 5
		end

		fdam = (30 * damstat)
		fdam = fdam + multiply_divide(fdam, p_ptr.skill[2] * 10, 100)
		fdam = fdam + multiply_divide(fdam, p_ptr.abilities[(CLASS_MONSTER * 10) + 8] * 50, 100)

		no_magic_return = TRUE
		attack_aura(GF_FIRE, fdam, 5)
		no_magic_return = FALSE

		place_field(FEAT_FIRE_FIELD, 5, px, py, fdam)
		update_and_handle()
	end

end

-- MONSTERS EVENTS

-- The general monster events scripts.

function monster_events_code (m_idx, eventcode)

	-- Code 1: Complete regeneration.
	if (eventcode == 1) then

		if ((monster(m_idx).hp < monster(m_idx).maxhp) or (monster(m_idx).lives < m_race(monster(m_idx).r_idx).lives)) then

			local m_name = ""

			if not (monster(m_idx).ml) then
				m_name = "it"
			elseif (get_monster_flag1(monster(m_idx).r_idx, RF1_UNIQUE)) then
				m_name = m_race(monster(m_idx).r_idx).name_char
			else
				m_name = string.format('%s %s', "The", m_race(monster(m_idx).r_idx).name_char)
			end

			msg_print(string.format('%s regenerates.', m_name))

			monster(m_idx).hp = monster(m_idx).maxhp
			monster(m_idx).lives = m_race(monster(m_idx).r_idx).lives
			if (m_race(monster(m_idx).r_idx).cursed > 0) then monster(m_idx).lives = monster(m_idx).lives + multiply_divide(monster(m_idx).lives, monster(m_idx).level, 100) end

			update_and_handle()
		end
	end

	-- Code 2: Becomes friendly if your alignment is 0 or higher.
	if (eventcode == 2) then

		if (p_ptr.alignment >= 0) then

			set_pet(monster(m_idx), TRUE)
		end
	end

	-- Code 3: Place ice fields around the monster.
	-- Used by Spirit of Blizzard.
	if (eventcode == 3) then

		local fdam

		fdam = (10 * monster(m_idx).mind)
		fdam = fdam + multiply_divide(fdam, monster(m_idx).skill_magic * 10, 100)

		place_field_monsters(FEAT_COLD_FIELD, 5, monster(m_idx).fx, monster(m_idx).fy, fdam)
		update_and_handle()
	end

	-- Code 4: Place ice fields around the monster.
	-- Same thing as 3. Only stronger.
	if (eventcode == 4) then

		local fdam

		fdam = (15 * monster(m_idx).mind)
		fdam = fdam + multiply_divide(fdam, monster(m_idx).skill_magic * 10, 100)

		place_field_monsters(FEAT_COLD_FIELD, 5, monster(m_idx).fx, monster(m_idx).fy, fdam)
		update_and_handle()
	end

	-- Code 5: Place ice fields around the monster.
	-- Same thing as 4. Only stronger.
	-- Yes, I like this enemy. :)
	if (eventcode == 5) then

		local fdam

		fdam = (20 * monster(m_idx).mind)
		fdam = fdam + multiply_divide(fdam, monster(m_idx).skill_magic * 10, 100)

		place_field_monsters(FEAT_COLD_FIELD, 6, monster(m_idx).fx, monster(m_idx).fy, fdam)
		update_and_handle()
	end

	-- An aura of Harm!
	if (eventcode == 6) then

		local sdam

		sdam = (40 * monster(m_idx).mind)
		sdam = sdam + multiply_divide(sdam, monster(m_idx).skill_magic * 10, 100)

		donthurtmonsters = 1
		lua_project(m_idx, 5, monster(m_idx).fy, monster(m_idx).fx, sdam, 162, 2)
		donthurtmonsters = 0

		update_and_handle()
	end

	-- Leap away(distance 3), 50%
	if (eventcode == 7) then
		
		if (lua_randint(100) >= 50) then
			local m_name = ""

			if not (monster(m_idx).ml) then
				m_name = "it"
			elseif (get_monster_flag1(monster(m_idx).r_idx, RF1_UNIQUE)) then
				m_name = m_race(monster(m_idx).r_idx).name_char
			else
				m_name = string.format('%s %s', "The", m_race(monster(m_idx).r_idx).name_char)
			end

			msg_print(string.format('%s leaps away!', m_name))
			teleport_away(m_idx, 3)
		end
	end

	-- An aura of Warp!
	if (eventcode == 8) then

		local sdam

		sdam = (50 * monster(m_idx).mind)
		sdam = sdam + multiply_divide(sdam, monster(m_idx).skill_magic * 10, 100)

		donthurtmonsters = 1
		lua_project(m_idx, 5, monster(m_idx).fy, monster(m_idx).fx, sdam, GF_WARP, 2)
		donthurtmonsters = 0

		update_and_handle()
	end

	-- Teleports away.
	if (eventcode == 9) then

		local m_name = ""

		if not (monster(m_idx).ml) then
			m_name = "it"
		elseif (get_monster_flag1(monster(m_idx).r_idx, RF1_UNIQUE)) then
			m_name = m_race(monster(m_idx).r_idx).name_char
		else
			m_name = string.format('%s %s', "The", m_race(monster(m_idx).r_idx).name_char)
		end

		msg_print(string.format('%s teleports!', m_name))
		teleport_away(m_idx, 10)
	end

	-- An aura of Life Blast(10%).
	if (eventcode == 10) then

		donthurtmonsters = 1
		lua_project(m_idx, 10, monster(m_idx).fy, monster(m_idx).fx, 10, GF_LIFE_BLAST, 2)
		donthurtmonsters = 0

		update_and_handle()
	end

	-- An aura of Fire!
	if (eventcode == 11) then

		local sdam

		sdam = (20 * monster(m_idx).mind)
		sdam = sdam + multiply_divide(sdam, monster(m_idx).skill_magic * 10, 100)

		donthurtmonsters = 1
		lua_project(m_idx, 3, monster(m_idx).fy, monster(m_idx).fx, sdam, GF_FIRE, 2)
		donthurtmonsters = 0

		update_and_handle()
	end

	-- An aura of Cold!
	if (eventcode == 12) then

		local sdam

		sdam = (20 * monster(m_idx).mind)
		sdam = sdam + multiply_divide(sdam, monster(m_idx).skill_magic * 10, 100)

		donthurtmonsters = 1
		lua_project(m_idx, 3, monster(m_idx).fy, monster(m_idx).fx, sdam, GF_COLD, 2)
		donthurtmonsters = 0

		update_and_handle()
	end

	-- An aura of Elemental!
	if (eventcode == 13) then

		local sdam

		sdam = (30 * monster(m_idx).mind)
		sdam = sdam + multiply_divide(sdam, monster(m_idx).skill_magic * 10, 100)

		donthurtmonsters = 1
		lua_project(m_idx, 3, monster(m_idx).fy, monster(m_idx).fx, sdam, GF_ELEMENTAL, 2)
		donthurtmonsters = 0

		update_and_handle()
	end

	-- An aura of Acid!
	if (eventcode == 14) then

		local sdam

		sdam = (20 * monster(m_idx).mind)
		sdam = sdam + multiply_divide(sdam, monster(m_idx).skill_magic * 10, 100)

		donthurtmonsters = 1
		lua_project(m_idx, 3, monster(m_idx).fy, monster(m_idx).fx, sdam, GF_ACID, 2)
		donthurtmonsters = 0

		update_and_handle()
	end

	-- An aura of Water!
	if (eventcode == 15) then

		local sdam

		sdam = (20 * monster(m_idx).mind)
		sdam = sdam + multiply_divide(sdam, monster(m_idx).skill_magic * 10, 100)

		donthurtmonsters = 1
		lua_project(m_idx, 3, monster(m_idx).fy, monster(m_idx).fx, sdam, GF_WATER, 2)
		donthurtmonsters = 0

		update_and_handle()
	end

	-- An aura of Earth!
	if (eventcode == 16) then

		local sdam

		sdam = (20 * monster(m_idx).mind)
		sdam = sdam + multiply_divide(sdam, monster(m_idx).skill_magic * 10, 100)

		donthurtmonsters = 1
		lua_project(m_idx, 3, monster(m_idx).fy, monster(m_idx).fx, sdam, GF_EARTH, 2)
		donthurtmonsters = 0

		update_and_handle()
	end

	-- An aura of Wind!
	if (eventcode == 17) then

		local sdam

		sdam = (20 * monster(m_idx).mind)
		sdam = sdam + multiply_divide(sdam, monster(m_idx).skill_magic * 10, 100)

		donthurtmonsters = 1
		lua_project(m_idx, 3, monster(m_idx).fy, monster(m_idx).fx, sdam, GF_WIND, 2)
		donthurtmonsters = 0

		update_and_handle()
	end

	-- An aura of Frostfire!
	if (eventcode == 18) then

		local sdam

		sdam = (20 * monster(m_idx).mind)
		sdam = sdam + multiply_divide(sdam, monster(m_idx).skill_magic * 10, 100)

		donthurtmonsters = 1
		lua_project(m_idx, 3, monster(m_idx).fy, monster(m_idx).fx, sdam, GF_FROSTFIRE, 2)
		donthurtmonsters = 0

		update_and_handle()
	end

	-- An aura of Mud!
	if (eventcode == 19) then

		local sdam

		sdam = (20 * monster(m_idx).mind)
		sdam = sdam + multiply_divide(sdam, monster(m_idx).skill_magic * 10, 100)

		donthurtmonsters = 1
		lua_project(m_idx, 3, monster(m_idx).fy, monster(m_idx).fx, sdam, GF_MUD, 2)
		donthurtmonsters = 0

		update_and_handle()
	end

	-- Place fire fields, AND a fire aura!
	if (eventcode == 20) then

		local fdam

		fdam = (30 * monster(m_idx).mind)
		fdam = fdam + multiply_divide(fdam, monster(m_idx).skill_magic * 10, 100)

		donthurtmonsters = 1
		lua_project(m_idx, 5, monster(m_idx).fy, monster(m_idx).fx, fdam, GF_FIRE, 2)
		donthurtmonsters = 0

		place_field_monsters(FEAT_FIRE_FIELD, 5, monster(m_idx).fx, monster(m_idx).fy, fdam)
		update_and_handle()
	end

	-- Shadow Mistress's teleportation.
	if (eventcode == 2303) then

		-- Check range
		if (monster(m_idx).cdis <= MAX_RANGE and not(is_pet(monster(m_idx)))) then

			-- Check path
			if (projectable(monster(m_idx).fy, monster(m_idx).fx, py, px)) then

				msg_print("The Nightmare Shadow Mistress teleports through the shadows!")
				teleport_to_player(m_idx)
			end
		end
	end

	-- Flow event codes.

	-- Because they now have them. :)

	-- An aura of the monster's misc code!
	if (eventcode == -1) then

		local sdam
		local basepower
		local rad
		local element

		basepower = m_race(monster(m_idx).r_idx).level / 2
		if (basepower < 1) then basepower = 1 end
		rad = (m_race(monster(m_idx).r_idx).level / 10) - 2
		if (rad < 2) then rad = 2 end
		if (rad > 10) then rad = 10 end

		element = (m_race(monster(m_idx).r_idx).event_misc * (-1))

		sdam = (basepower * monster(m_idx).mind)
		sdam = sdam + multiply_divide(sdam, monster(m_idx).skill_magic * 10, 100)

		donthurtmonsters = 1
		lua_project(m_idx, rad, monster(m_idx).fy, monster(m_idx).fx, sdam, element, 2)
		donthurtmonsters = 0

		update_and_handle()
	end

	-- Elemental fields.
	if (eventcode == -2) then

		local fdam
		local basepower
		local rad
		local ftype

		basepower = m_race(monster(m_idx).r_idx).level / 2
		if (basepower < 1) then basepower = 1 end
		rad = (m_race(monster(m_idx).r_idx).level / 20) - 2
		if (rad < 2) then rad = 2 end
		if (rad > 10) then rad = 10 end

		fdam = (basepower * monster(m_idx).mind)
		fdam = fdam + multiply_divide(fdam, monster(m_idx).skill_magic * 10, 100)

		if (m_race(monster(m_idx).r_idx).event_misc == -1) then ftype = FEAT_FIRE_FIELD end
		if (m_race(monster(m_idx).r_idx).event_misc == -2) then ftype = FEAT_COLD_FIELD end
		if (m_race(monster(m_idx).r_idx).event_misc == -3) then ftype = FEAT_ELEC_FIELD end
		if (m_race(monster(m_idx).r_idx).event_misc == -4) then ftype = FEAT_THORNED_VINES end
		if (m_race(monster(m_idx).r_idx).event_misc == -5) then ftype = FEAT_STORMS end

		place_field_monsters(ftype, rad, monster(m_idx).fx, monster(m_idx).fy, fdam)
		update_and_handle()
	end

end


-- Event-specific functions.

function monster_before_melee (m_idx, eventcode)

	monster_events_code(m_idx, eventcode)
end

function monster_after_melee (m_idx, eventcode)

	monster_events_code(m_idx, eventcode)
end

function monster_before_ranged (m_idx, eventcode)

	monster_events_code(m_idx, eventcode)
end

function monster_after_ranged (m_idx, eventcode)

	monster_events_code(m_idx, eventcode)
end

function monster_before_magic (m_idx, eventcode)

	monster_events_code(m_idx, eventcode)
end

function monster_after_magic (m_idx, eventcode)

	monster_events_code(m_idx, eventcode)
end

function monster_before_move (m_idx, eventcode)

	monster_events_code(m_idx, eventcode)
end

function monster_after_move (m_idx, eventcode)

	monster_events_code(m_idx, eventcode)
end

function monster_passive (m_idx, eventcode)

	-- Bardic reputation.
	if (p_ptr.abilities[(CLASS_BARD * 10) + 7] > 0 and monster(m_idx).boss == 0 and monster(m_idx).cursed == 0 and monster(m_idx).monfear == 0 and not(is_pet(monster(m_idx))) and not(get_monster_flag1(monster(m_idx).r_idx, RF1_UNIQUE)) and monster(m_idx).cdis <= 5) then

		local racekills
		local ppower
		local mpower

		-- This function is hard-coded for speed issues.
		racekills = get_race_kills(monster(m_idx).d_char)

		if (racekills > 0 and not(get_monster_flag3(monster(m_idx).r_idx, RF3_NO_FEAR))) then

			ppower = (racekills * 2)
			if (ppower > 30) then ppower = 30 end
			ppower = ppower * p_ptr.abilities[(CLASS_BARD * 10) + 7]
			mpower = monster(m_idx).level + monster(m_idx).mind

			if (lua_randint(ppower) >= lua_randint(mpower)) then

				if (monster(m_idx).monfear == 0) then msg_print(string.format('%s becomes scared!', m_race(monster(m_idx).r_idx).name_char)) end
				monster(m_idx).monfear = (2 + racekills)
			end
		end
	end

	monster_events_code(m_idx, eventcode)
end

function monster_take_damages (m_idx, eventcode)

	monster_events_code(m_idx, eventcode)
end

function monster_dies (m_idx, eventcode)

	monster_events_code(m_idx, eventcode)
end

function monster_spawn (m_idx, eventcode)

	monster_events_code(m_idx, eventcode)
end

add_event_handler("before_player_move", before_player_move)
add_event_handler("after_player_move", after_player_move)
add_event_handler("player_before_melee", player_before_melee)
add_event_handler("player_after_melee", player_after_melee)
add_event_handler("player_before_ranged", player_before_ranged)
add_event_handler("player_after_ranged", player_after_ranged)
add_event_handler("player_before_magic", player_before_magic)
add_event_handler("player_after_magic", player_after_magic)
add_event_handler("player_take_damages", player_take_damages)
add_event_handler("player_skip_turn", player_skip_turn)
add_event_handler("player_events_code", player_events_code)
add_event_handler("player_enter_dungeon", player_enter_dungeon)
add_event_handler("player_before_floor", player_before_floor)
add_event_handler("player_after_floor", player_after_floor)
add_event_handler("monster_events_code", monster_events_code)
add_event_handler("monster_before_melee", monster_before_melee)
add_event_handler("monster_after_melee", monster_after_melee)
add_event_handler("monster_before_ranged", monster_before_ranged)
add_event_handler("monster_after_ranged", monster_after_ranged)
add_event_handler("monster_before_magic", monster_before_magic)
add_event_handler("monster_after_magic", monster_after_magic)
add_event_handler("monster_before_move", monster_before_move)
add_event_handler("monster_after_move", monster_after_move)
add_event_handler("monster_passive", monster_passive)
add_event_handler("monster_take_damages", monster_take_damages)
add_event_handler("monster_dies", monster_dies)
add_event_handler("monster_spawn", monster_spawn)
