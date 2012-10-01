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
	if (p_ptr.events[29042] == 1 and resting == 0) then

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
			elseif (global_object.tval == TV_CHEST) then

				klev = global_object.pval + (object_skill_points_value(global_object) / 5)
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

	-- Sealed stairway.
	if (cave(py, px).feat == 242) then

		local ch
		local item

		msg_print("You have discovered a sealed stairway. Try to unseal it? [y/n]")
		ch = inkey()

		-- Characters 89 and 121 are "Y" and "y"
                if (ch == 89 or ch == 121) then

			msg_print("Choose an item to undo the seal. ")

			item = lua_pick_item(0)

			if (item.k_idx == dungeon(dungeon_type).secretitem) then

				msg_print("The seal has been broken, and a stairway appears!")
				p_ptr.events[dungeon(dungeon_type).secretevent+1] = 1
				cave(py, px).feat = 243
				update_and_handle()
			else
				msg_print("Nothing happened.")
			end
                end
	end

	-- Secret Stairway.
	if (cave(py, px).feat == 243) then

		local ch

		msg_print("Enter Secret Level? [y/n/(h)elp]")
		ch = inkey()

		if (ch == 72 or ch == 104) then

			show_file("secret.txt", NULL, 0, 0)
                end

		-- Characters 89 and 121 are "Y" and "y"
                if (ch == 89 or ch == 121) then

			show_dialog(dungeon(dungeon_type).secretintro)
			p_ptr.inside_quest = dungeon(dungeon_type).secret
			p_ptr.inside_secret = dungeon(dungeon_type).secret
			p_ptr.questx = 0
			p_ptr.questy = 0

			p_ptr.leaving = TRUE
			--generate_cave()
                end
	end

	-- Portal of Final Death.
	if (cave(py, px).feat == 244) then

		local ch
		local ch2

		msg_print("Enter the Portal of Final Death? [y/n]")
		ch = inkey()

		-- Characters 89 and 121 are "Y" and "y"
                if (ch == 89 or ch == 121) then

			msg_print("Your character will be gone forever! Are you sure? [y/n]")
			ch2 = inkey()

			if (ch2 == 89 or ch2 == 121) then

				death = TRUE
				p_ptr.leaving = TRUE
			end
                end
	end

	-- Portal of Resurrection
	if (cave(py, px).feat == 245) then

		local ch
		local i

		msg_print("Enter the Portal of Resurrection? [y/n]")
		ch = inkey()

		-- Characters 89 and 121 are "Y" and "y"
                if (ch == 89 or ch == 121) then

			local x

			msg_print("You enter the portal of resurrection...")
			
			for i = 1, 100 do
				no_more_items()
			end
                	p_ptr.au = 0
                	p_ptr.chp = p_ptr.mhp
                	p_ptr.inside_quest = 0
			p_ptr.inside_secret = 0
                	p_ptr.cut = 0
                	p_ptr.stun = 0
                	p_ptr.poisoned = 0
			p_ptr.confused = 0
			p_ptr.afraid = 0
			p_ptr.blind = 0
                	p_ptr.word_recall = 0
                	restore_level()
                	dun_level = 0
			dying = FALSE

			p_ptr.town_num = p_ptr.events[29999]
			p_ptr.deathcount = p_ptr.deathcount + 1

			-- Restart to the latest town's startx/starty.
			lua_revive_in_town()

			p_ptr.wild_mode = FALSE

			p_ptr.leaving = TRUE
			generate_cave()
			do_cmd_save_game()
                end
	end

	-- Portal of Reincarnation
	if (cave(py, px).feat == 246) then

		local ch

		msg_print("Enter the Portal of Reincarnation? [y/n]")
		ch = inkey()

		-- Characters 89 and 121 are "Y" and "y"
                if (ch == 89 or ch == 121) then

			local i

			msg_print("You enter the portal of reincarnation...")
			
			for i = 1, 100 do
				no_more_items()
			end
                	p_ptr.au = 0
                	p_ptr.chp = p_ptr.mhp
                	p_ptr.inside_quest = 0
			p_ptr.inside_secret = 0
                	p_ptr.cut = 0
                	p_ptr.stun = 0
                	p_ptr.poisoned = 0
			p_ptr.confused = 0
			p_ptr.afraid = 0
			p_ptr.blind = 0
                	p_ptr.word_recall = 0
                	restore_level()
                	dun_level = 0
			dying = FALSE

			p_ptr.town_num = p_ptr.events[29999]

			-- Reincarnation is hard-coded.
			p_ptr.deathcount = p_ptr.deathcount + 1
			p_ptr.reincarnations = p_ptr.reincarnations + 1

			-- Reset base stats.
			p_ptr.stat_cur[A_STR+1] = 5
			p_ptr.stat_cur[A_INT+1] = 5
			p_ptr.stat_cur[A_WIS+1] = 5
			p_ptr.stat_cur[A_DEX+1] = 5
			p_ptr.stat_cur[A_CON+1] = 5
			p_ptr.stat_cur[A_CHR+1] = 5
			p_ptr.stat_max[A_STR+1] = 5
			p_ptr.stat_max[A_INT+1] = 5
			p_ptr.stat_max[A_WIS+1] = 5
			p_ptr.stat_max[A_DEX+1] = 5
			p_ptr.stat_max[A_CON+1] = 5
			p_ptr.stat_max[A_CHR+1] = 5

			for i = 1, SKILL_MAX do

				p_ptr.skill_base[i] = 0
			end

			for i = 1, MAX_ABILITIES do

				p_ptr.abilities[i] = 0
			end

			for i = 1, 36 do

				p_ptr.abilities_powers[i] = 0
			end

			p_ptr.num_abilities = 0

			if (p_ptr.prace == RACE_MONSTER) then

				for i = 1, 20 do

					p_ptr.abilities_monster_attacks[i] = 0
					p_ptr.abilities_monster_spells[i] = 0
				end
				p_ptr.boss_abilities = 0
				p_ptr.abilities_powers[1] = (CLASS_APPRENTICE * 10) + 1
				p_ptr.abilities_powers[2] = (CLASS_APPRENTICE * 10) + 2
			end

			-- Certain "events" variables needs to be turned off.
			p_ptr.events[29050] = 0
			p_ptr.events[29051] = 0
			p_ptr.events[29054] = 0

			p_ptr.statpoints = p_ptr.lev * 2
			p_ptr.skillpoints = p_ptr.lev * 10
			p_ptr.ability_points = p_ptr.lev

			p_ptr.statpoints = p_ptr.statpoints + (p_ptr.secretscleared * 4)
			p_ptr.skillpoints = p_ptr.skillpoints + (p_ptr.secretscleared * 10)
			p_ptr.ability_points = p_ptr.ability_points + (p_ptr.secretscleared * 2)

			-- Restart to the latest town's startx/starty.
			lua_revive_in_town()

			p_ptr.wild_mode = FALSE

			p_ptr.leaving = TRUE
			generate_cave()
			do_cmd_save_game()
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
	if (p_ptr.events[29042] == 1 and resting == 0) then

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

	generate_monster(2050, dun_level, 0)
end

-- Events after creation of a new floor.
function player_after_floor (dnum)

	local x
	local y
	local placed

	-- Reset Great Guard's chance.
	if (p_ptr.abilities[(CLASS_DEFENDER * 10) + 10] >= 1) then

		p_ptr.events[29052] = 90 + (p_ptr.abilities[(CLASS_DEFENDER * 10) + 10] * 10)
	else
		p_ptr.events[29052] = 0
	end

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
			damstat = A_INT
		else
			damstat = A_WIS
		end

		fdam = 10 + (p_ptr.lev / 3)
		fdam = spell_damages(fdam, damstat, 0)
		fdam = fdam + multiply_divide(fdam, p_ptr.abilities[(CLASS_MONSTER * 10) + 8] * 20, 100)

		place_field(FEAT_COLD_FIELD, 5, px, py, fdam)
		update_and_handle()
	end

	-- Code 4: Place ice fields around the monster.
	-- Same thing as 3. Only stronger.
	if (eventcode == 4) then

		local fdam
		local damstat

		if (p_ptr.stat_ind[A_INT+1] >= p_ptr.stat_ind[A_WIS+1]) then
			damstat = A_INT
		else
			damstat = A_WIS
		end

		fdam = 15 + (p_ptr.lev / 3)
		fdam = spell_damages(fdam, damstat, 0)
		fdam = fdam + multiply_divide(fdam, p_ptr.abilities[(CLASS_MONSTER * 10) + 8] * 20, 100)

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
			damstat = A_INT
		else
			damstat = A_WIS
		end

		fdam = 20 + (p_ptr.lev / 3)
		fdam = spell_damages(fdam, damstat, 0)
		fdam = fdam + multiply_divide(fdam, p_ptr.abilities[(CLASS_MONSTER * 10) + 8] * 20, 100)

		place_field(FEAT_COLD_FIELD, 6, px, py, fdam)
		update_and_handle()
	end

	-- An aura of Harm!
	if (eventcode == 6) then

		local sdam
		local damstat

		if (p_ptr.stat_ind[A_INT+1] >= p_ptr.stat_ind[A_WIS+1]) then
			damstat = A_INT
		else
			damstat = A_WIS
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
			damstat = A_INT
		else
			damstat = A_WIS
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
			damstat = A_INT
		else
			damstat = A_WIS
		end

		sdam = 20 + (p_ptr.lev / 3)
		sdam = spell_damages(sdam, damstat, 0)
		sdam = sdam + multiply_divide(sdam, p_ptr.abilities[(CLASS_MONSTER * 10) + 8] * 20, 100)

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
			damstat = A_INT
		else
			damstat = A_WIS
		end

		sdam = 20 + (p_ptr.lev / 3)
		sdam = spell_damages(sdam, damstat, 0)
		sdam = sdam + multiply_divide(sdam, p_ptr.abilities[(CLASS_MONSTER * 10) + 8] * 20, 100)

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
			damstat = A_INT
		else
			damstat = A_WIS
		end

		sdam = 30 + (p_ptr.lev / 3)
		sdam = spell_damages(sdam, damstat, 0)
		sdam = sdam + multiply_divide(sdam, p_ptr.abilities[(CLASS_MONSTER * 10) + 8] * 20, 100)

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
			damstat = A_INT
		else
			damstat = A_WIS
		end

		sdam = 20 + (p_ptr.lev / 3)
		sdam = spell_damages(sdam, damstat, 0)
		sdam = sdam + multiply_divide(sdam, p_ptr.abilities[(CLASS_MONSTER * 10) + 8] * 20, 100)

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
			damstat = A_INT
		else
			damstat = A_WIS
		end

		sdam = 20 + (p_ptr.lev / 3)
		sdam = spell_damages(sdam, damstat, 0)
		sdam = sdam + multiply_divide(sdam, p_ptr.abilities[(CLASS_MONSTER * 10) + 8] * 20, 100)

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
			damstat = A_INT
		else
			damstat = A_WIS
		end

		sdam = 20 + (p_ptr.lev / 3)
		sdam = spell_damages(sdam, damstat, 0)
		sdam = sdam + multiply_divide(sdam, p_ptr.abilities[(CLASS_MONSTER * 10) + 8] * 20, 100)

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
			damstat = A_INT
		else
			damstat = A_WIS
		end

		sdam = 20 + (p_ptr.lev / 3)
		sdam = spell_damages(sdam, damstat, 0)
		sdam = sdam + multiply_divide(sdam, p_ptr.abilities[(CLASS_MONSTER * 10) + 8] * 20, 100)

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
			damstat = A_INT
		else
			damstat = A_WIS
		end

		sdam = 20 + (p_ptr.lev / 3)
		sdam = spell_damages(sdam, damstat, 0)
		sdam = sdam + multiply_divide(sdam, p_ptr.abilities[(CLASS_MONSTER * 10) + 8] * 20, 100)

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
			damstat = A_INT
		else
			damstat = A_WIS
		end

		sdam = 20 + (p_ptr.lev / 3)
		sdam = spell_damages(sdam, damstat, 0)
		sdam = sdam + multiply_divide(sdam, p_ptr.abilities[(CLASS_MONSTER * 10) + 8] * 20, 100)

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
			damstat = A_INT
		else
			damstat = A_WIS
		end

		fdam = 30 + (p_ptr.lev / 3)
		fdam = spell_damages(fdam, damstat, 0)
		fdam = fdam + multiply_divide(fdam, p_ptr.abilities[(CLASS_MONSTER * 10) + 8] * 20, 100)

		no_magic_return = TRUE
		attack_aura(GF_FIRE, fdam, 5)
		no_magic_return = FALSE

		place_field(FEAT_FIRE_FIELD, 5, px, py, fdam)
		update_and_handle()
	end

	-- An aura of Disable. (power 20)
	-- Useless for the player.
	if (eventcode == 21) then

		no_magic_return = TRUE
		attack_aura(GF_DISABLE, 20, 5)
		no_magic_return = FALSE

		update_and_handle()
	end

	-- An aura of Fire! (power 30)
	if (eventcode == 22) then

		local sdam
		local damstat

		if (p_ptr.stat_ind[A_INT+1] >= p_ptr.stat_ind[A_WIS+1]) then
			damstat = A_INT
		else
			damstat = A_WIS
		end

		sdam = 30 + (p_ptr.lev / 3)
		sdam = spell_damages(sdam, damstat, 0)
		sdam = sdam + multiply_divide(sdam, p_ptr.abilities[(CLASS_MONSTER * 10) + 8] * 20, 100)

		no_magic_return = TRUE
		attack_aura(GF_FIRE, sdam, 3)
		no_magic_return = FALSE

		update_and_handle()
	end

	-- An aura of Water! (power 30)
	if (eventcode == 23) then

		local sdam
		local damstat

		if (p_ptr.stat_ind[A_INT+1] >= p_ptr.stat_ind[A_WIS+1]) then
			damstat = A_INT
		else
			damstat = A_WIS
		end

		sdam = 30 + (p_ptr.lev / 3)
		sdam = spell_damages(sdam, damstat, 0)
		sdam = sdam + multiply_divide(sdam, p_ptr.abilities[(CLASS_MONSTER * 10) + 8] * 20, 100)

		no_magic_return = TRUE
		attack_aura(GF_WATER, sdam, 3)
		no_magic_return = FALSE

		update_and_handle()
	end

	-- An aura of Earth! (power 30)
	if (eventcode == 24) then

		local sdam
		local damstat

		if (p_ptr.stat_ind[A_INT+1] >= p_ptr.stat_ind[A_WIS+1]) then
			damstat = A_INT
		else
			damstat = A_WIS
		end

		sdam = 30 + (p_ptr.lev / 3)
		sdam = spell_damages(sdam, damstat, 0)
		sdam = sdam + multiply_divide(sdam, p_ptr.abilities[(CLASS_MONSTER * 10) + 8] * 20, 100)

		no_magic_return = TRUE
		attack_aura(GF_EARTH, sdam, 3)
		no_magic_return = FALSE

		update_and_handle()
	end

	-- An aura of Wind! (power 30)
	if (eventcode == 25) then

		local sdam
		local damstat

		if (p_ptr.stat_ind[A_INT+1] >= p_ptr.stat_ind[A_WIS+1]) then
			damstat = A_INT
		else
			damstat = A_WIS
		end

		sdam = 30 + (p_ptr.lev / 3)
		sdam = spell_damages(sdam, damstat, 0)
		sdam = sdam + multiply_divide(sdam, p_ptr.abilities[(CLASS_MONSTER * 10) + 8] * 20, 100)

		no_magic_return = TRUE
		attack_aura(GF_WIND, sdam, 3)
		no_magic_return = FALSE

		update_and_handle()
	end

	-- An aura of Chaos! (power 30)
	if (eventcode == 26) then

		local sdam
		local damstat

		if (p_ptr.stat_ind[A_INT+1] >= p_ptr.stat_ind[A_WIS+1]) then
			damstat = A_INT
		else
			damstat = A_WIS
		end

		sdam = 30 + (p_ptr.lev / 3)
		sdam = spell_damages(sdam, damstat, 0)
		sdam = sdam + multiply_divide(sdam, p_ptr.abilities[(CLASS_MONSTER * 10) + 8] * 20, 100)

		no_magic_return = TRUE
		attack_aura(GF_CHAOS, sdam, 3)
		no_magic_return = FALSE

		update_and_handle()
	end

	-- Teleports away.
	if (eventcode == 28) then

		msg_print("You teleport!")
		teleport_player(10 + p_ptr.abilities[(CLASS_MONSTER * 10) + 8])
	end

	-- An aura of Warp! (power 30)
	if (eventcode == 29) then

		local sdam
		local damstat

		if (p_ptr.stat_ind[A_INT+1] >= p_ptr.stat_ind[A_WIS+1]) then
			damstat = A_INT
		else
			damstat = A_WIS
		end

		sdam = 30 + (p_ptr.lev / 3)
		sdam = spell_damages(sdam, damstat, 0)
		sdam = sdam + multiply_divide(sdam, p_ptr.abilities[(CLASS_MONSTER * 10) + 8] * 20, 100)

		no_magic_return = TRUE
		attack_aura(GF_WARP, sdam, 3)
		no_magic_return = FALSE

		update_and_handle()
	end

	-- An aura of Darkness! (power 25)
	if (eventcode == 30) then

		local sdam
		local damstat

		if (p_ptr.stat_ind[A_INT+1] >= p_ptr.stat_ind[A_WIS+1]) then
			damstat = A_INT
		else
			damstat = A_WIS
		end

		sdam = 25 + (p_ptr.lev / 3)
		sdam = spell_damages(sdam, damstat, 0)
		sdam = sdam + multiply_divide(sdam, p_ptr.abilities[(CLASS_MONSTER * 10) + 8] * 20, 100)

		no_magic_return = TRUE
		attack_aura(GF_DARK, sdam, 3)
		no_magic_return = FALSE

		update_and_handle()
	end

	-- Spikefiend's chains!
	if (eventcode == 31) then

		local x
		local y
		local m_name

        	if (not(lua_tgt_pt())) then return end
		x = global_x
		y = global_y

		-- Check if we have a monster.
		if (cave(y, x).m_idx == 0) then

			msg_print("No monster here.")
			return
		end

		-- Monster must be in line of sight.
		if (not(projectable(monster(cave(y, x).m_idx).fy, monster(cave(y, x).m_idx).fx, py, px))) then

			msg_print("Monster must be in line of sight.")
			return
		end

		-- All right, we're done validating. Proceed with the actual ability!
		m_name = m_race(monster(cave(y, x).m_idx).r_idx).name_char

		if (player_hit_monster(monster(cave(y, x).m_idx), p_ptr.skill[2])) then

			local dam
			local power
			local ddice
			local dside

			ddice = 7 + (p_ptr.lev / 10)
			dside = 11 + (p_ptr.lev / 5)

			power = damroll(ddice, dside)

			-- Uses "spell_damages", but actually use strength, and it's considered
			-- a ranged attack. Yes, it's weird. But it all makes sense when your enemy
			-- dies before you can even drag it to you. :)
			dam = spell_damages(power, A_STR, 0)

			msg_print(string.format('You grab %s with your chains!', m_name))

			no_magic_return = TRUE
			nevermiss = TRUE
			melee_attack = TRUE
			fire_ball_specific_grid(dam, x, y, 0, GF_PHYSICAL)
			melee_attack = FALSE
			nevermiss = FALSE
			no_magic_return = FALSE

			if (not(cave(y, x).m_idx == 0)) then
				teleport_to_player(cave(y, x).m_idx)
			end
		else
			msg_print(string.format('%s avoided your chains.', m_name))
		end

        	update_and_handle()
		energy_use = 100
	end

	-- Fire fields + aura. (more powerful)
	if (eventcode == 32) then

		local fdam
		local damstat

		if (p_ptr.stat_ind[A_INT+1] >= p_ptr.stat_ind[A_WIS+1]) then
			damstat = A_INT
		else
			damstat = A_WIS
		end

		fdam = 50 + (p_ptr.lev / 3)
		fdam = spell_damages(fdam, damstat, 0)
		fdam = fdam + multiply_divide(fdam, p_ptr.abilities[(CLASS_MONSTER * 10) + 8] * 20, 100)

		no_magic_return = TRUE
		attack_aura(GF_FIRE, fdam, 5)
		no_magic_return = FALSE

		place_field(FEAT_FIRE_FIELD, 10, px, py, fdam)
		update_and_handle()
	end

	-- If you play as a monster that has the ability to teleport to the player, you gain
	-- the power to teleport where you want. The same restrictions as jump applies though.
	if (eventcode == 2303) then

		local x
		local y

		-- We use a special function that returns x and y coordinates in global
		-- variables, so we can use these in Lua.
        	if (not(lua_tgt_pt())) then return end
		x = global_x
		y = global_y

		-- Most functions here use a (y,x) format, instead of (x,y).
		-- This is important, because if you use (x,y), it might crash.
        	if (not(lua_cave_empty_bold(y,x))) then

              		msg_print("You cannot teleport at that location.")

        	else

                	if (not(get_cave_info_flag(y, x, CAVE_MARK))) then

                        	if (get_cave_info_flag(y, x, CAVE_LITE)) then

					teleport_player_to(y,x)
                        	else
					msg_print("You cannot teleport at that location.")
				end

                	else

				teleport_player_to(y,x)
			end
        	end
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

		fdam = 10 + (monster(m_idx).level / 3)
		fdam = monster_spell_damages(monster(m_idx), fdam)
		if (get_monster_ability(monster(m_idx), BOSS_DOUBLE_MAGIC)) then fdam = fdam * 2 end

		place_field_monsters(FEAT_COLD_FIELD, 5, monster(m_idx).fx, monster(m_idx).fy, fdam)
		update_and_handle()
	end

	-- Code 4: Place ice fields around the monster.
	-- Same thing as 3. Only stronger.
	if (eventcode == 4) then

		local fdam

		fdam = 15 + (monster(m_idx).level / 3)
		fdam = monster_spell_damages(monster(m_idx), fdam)
		if (get_monster_ability(monster(m_idx), BOSS_DOUBLE_MAGIC)) then fdam = fdam * 2 end

		place_field_monsters(FEAT_COLD_FIELD, 5, monster(m_idx).fx, monster(m_idx).fy, fdam)
		update_and_handle()
	end

	-- Code 5: Place ice fields around the monster.
	-- Same thing as 4. Only stronger.
	-- Yes, I like this enemy. :)
	if (eventcode == 5) then

		local fdam

		fdam = 20 + (monster(m_idx).level / 3)
		fdam = monster_spell_damages(monster(m_idx), fdam)
		if (get_monster_ability(monster(m_idx), BOSS_DOUBLE_MAGIC)) then fdam = fdam * 2 end

		place_field_monsters(FEAT_COLD_FIELD, 6, monster(m_idx).fx, monster(m_idx).fy, fdam)
		update_and_handle()
	end

	-- An aura of Harm!
	if (eventcode == 6) then

		local sdam

		sdam = 40 + (monster(m_idx).level / 3)
		sdam = monster_spell_damages(monster(m_idx), sdam)
		if (get_monster_ability(monster(m_idx), BOSS_DOUBLE_MAGIC)) then sdam = sdam * 2 end

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

		sdam = 50 + (monster(m_idx).level / 2)
		sdam = monster_spell_damages(monster(m_idx), sdam)
		if (get_monster_ability(monster(m_idx), BOSS_DOUBLE_MAGIC)) then sdam = sdam * 2 end

		donthurtmonsters = 1
		lua_project(m_idx, 5, monster(m_idx).fy, monster(m_idx).fx, sdam, GF_WARP, 2)
		donthurtmonsters = 0

		update_and_handle()
	end

	-- Teleports away.
	if (eventcode == 9) then

		if (no_monster_teleport == 0) then
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

		sdam = 20 + (monster(m_idx).level / 3)
		sdam = monster_spell_damages(monster(m_idx), sdam)
		if (get_monster_ability(monster(m_idx), BOSS_DOUBLE_MAGIC)) then sdam = sdam * 2 end

		donthurtmonsters = 1
		lua_project(m_idx, 3, monster(m_idx).fy, monster(m_idx).fx, sdam, GF_FIRE, 2)
		donthurtmonsters = 0

		update_and_handle()
	end

	-- An aura of Cold!
	if (eventcode == 12) then

		local sdam

		sdam = 20 + (monster(m_idx).level / 3)
		sdam = monster_spell_damages(monster(m_idx), sdam)
		if (get_monster_ability(monster(m_idx), BOSS_DOUBLE_MAGIC)) then sdam = sdam * 2 end

		donthurtmonsters = 1
		lua_project(m_idx, 3, monster(m_idx).fy, monster(m_idx).fx, sdam, GF_COLD, 2)
		donthurtmonsters = 0

		update_and_handle()
	end

	-- An aura of Elemental!
	if (eventcode == 13) then

		local sdam

		sdam = 30 + (monster(m_idx).level / 3)
		sdam = monster_spell_damages(monster(m_idx), sdam)
		if (get_monster_ability(monster(m_idx), BOSS_DOUBLE_MAGIC)) then sdam = sdam * 2 end

		donthurtmonsters = 1
		lua_project(m_idx, 3, monster(m_idx).fy, monster(m_idx).fx, sdam, GF_ELEMENTAL, 2)
		donthurtmonsters = 0

		update_and_handle()
	end

	-- An aura of Acid!
	if (eventcode == 14) then

		local sdam

		sdam = 20 + (monster(m_idx).level / 3)
		sdam = monster_spell_damages(monster(m_idx), sdam)
		if (get_monster_ability(monster(m_idx), BOSS_DOUBLE_MAGIC)) then sdam = sdam * 2 end

		donthurtmonsters = 1
		lua_project(m_idx, 3, monster(m_idx).fy, monster(m_idx).fx, sdam, GF_ACID, 2)
		donthurtmonsters = 0

		update_and_handle()
	end

	-- An aura of Water!
	if (eventcode == 15) then

		local sdam

		sdam = 20 + (monster(m_idx).level / 3)
		sdam = monster_spell_damages(monster(m_idx), sdam)
		if (get_monster_ability(monster(m_idx), BOSS_DOUBLE_MAGIC)) then sdam = sdam * 2 end

		donthurtmonsters = 1
		lua_project(m_idx, 3, monster(m_idx).fy, monster(m_idx).fx, sdam, GF_WATER, 2)
		donthurtmonsters = 0

		update_and_handle()
	end

	-- An aura of Earth!
	if (eventcode == 16) then

		local sdam

		sdam = 20 + (monster(m_idx).level / 3)
		sdam = monster_spell_damages(monster(m_idx), sdam)
		if (get_monster_ability(monster(m_idx), BOSS_DOUBLE_MAGIC)) then sdam = sdam * 2 end

		donthurtmonsters = 1
		lua_project(m_idx, 3, monster(m_idx).fy, monster(m_idx).fx, sdam, GF_EARTH, 2)
		donthurtmonsters = 0

		update_and_handle()
	end

	-- An aura of Wind!
	if (eventcode == 17) then

		local sdam

		sdam = 20 + (monster(m_idx).level / 3)
		sdam = monster_spell_damages(monster(m_idx), sdam)
		if (get_monster_ability(monster(m_idx), BOSS_DOUBLE_MAGIC)) then sdam = sdam * 2 end

		donthurtmonsters = 1
		lua_project(m_idx, 3, monster(m_idx).fy, monster(m_idx).fx, sdam, GF_WIND, 2)
		donthurtmonsters = 0

		update_and_handle()
	end

	-- An aura of Frostfire!
	if (eventcode == 18) then

		local sdam

		sdam = 20 + (monster(m_idx).level / 3)
		sdam = monster_spell_damages(monster(m_idx), sdam)
		if (get_monster_ability(monster(m_idx), BOSS_DOUBLE_MAGIC)) then sdam = sdam * 2 end

		donthurtmonsters = 1
		lua_project(m_idx, 3, monster(m_idx).fy, monster(m_idx).fx, sdam, GF_FROSTFIRE, 2)
		donthurtmonsters = 0

		update_and_handle()
	end

	-- An aura of Mud!
	if (eventcode == 19) then

		local sdam

		sdam = 20 + (monster(m_idx).level / 3)
		sdam = monster_spell_damages(monster(m_idx), sdam)
		if (get_monster_ability(monster(m_idx), BOSS_DOUBLE_MAGIC)) then sdam = sdam * 2 end

		donthurtmonsters = 1
		lua_project(m_idx, 3, monster(m_idx).fy, monster(m_idx).fx, sdam, GF_MUD, 2)
		donthurtmonsters = 0

		update_and_handle()
	end

	-- Place fire fields, AND a fire aura!
	if (eventcode == 20) then

		local fdam

		fdam = 30 + (monster(m_idx).level / 3)
		fdam = monster_spell_damages(monster(m_idx), fdam)
		if (get_monster_ability(monster(m_idx), BOSS_DOUBLE_MAGIC)) then fdam = fdam * 2 end

		donthurtmonsters = 1
		lua_project(m_idx, 5, monster(m_idx).fy, monster(m_idx).fx, fdam, GF_FIRE, 2)
		donthurtmonsters = 0

		place_field_monsters(FEAT_FIRE_FIELD, 5, monster(m_idx).fx, monster(m_idx).fy, fdam)
		update_and_handle()
	end

	-- An aura of Disable! (power 20)
	if (eventcode == 21) then

		donthurtmonsters = 1
		lua_project(m_idx, 5, monster(m_idx).fy, monster(m_idx).fx, 20, GF_DISABLE, 2)
		donthurtmonsters = 0

		update_and_handle()
	end

	-- An aura of Fire! (power 30)
	if (eventcode == 22) then

		local sdam

		sdam = 30 + (monster(m_idx).level / 3)
		sdam = monster_spell_damages(monster(m_idx), sdam)
		if (get_monster_ability(monster(m_idx), BOSS_DOUBLE_MAGIC)) then sdam = sdam * 2 end

		donthurtmonsters = 1
		lua_project(m_idx, 3, monster(m_idx).fy, monster(m_idx).fx, sdam, GF_FIRE, 2)
		donthurtmonsters = 0

		update_and_handle()
	end

	-- An aura of Water! (power 30)
	if (eventcode == 23) then

		local sdam

		sdam = 30 + (monster(m_idx).level / 3)
		sdam = monster_spell_damages(monster(m_idx), sdam)
		if (get_monster_ability(monster(m_idx), BOSS_DOUBLE_MAGIC)) then sdam = sdam * 2 end

		donthurtmonsters = 1
		lua_project(m_idx, 3, monster(m_idx).fy, monster(m_idx).fx, sdam, GF_WATER, 2)
		donthurtmonsters = 0

		update_and_handle()
	end

	-- An aura of Earth! (power 30)
	if (eventcode == 24) then

		local sdam

		sdam = 30 + (monster(m_idx).level / 3)
		sdam = monster_spell_damages(monster(m_idx), sdam)
		if (get_monster_ability(monster(m_idx), BOSS_DOUBLE_MAGIC)) then sdam = sdam * 2 end

		donthurtmonsters = 1
		lua_project(m_idx, 3, monster(m_idx).fy, monster(m_idx).fx, sdam, GF_EARTH, 2)
		donthurtmonsters = 0

		update_and_handle()
	end

	-- An aura of Wind! (power 30)
	if (eventcode == 25) then

		local sdam

		sdam = 30 + (monster(m_idx).level / 3)
		sdam = monster_spell_damages(monster(m_idx), sdam)
		if (get_monster_ability(monster(m_idx), BOSS_DOUBLE_MAGIC)) then sdam = sdam * 2 end

		donthurtmonsters = 1
		lua_project(m_idx, 3, monster(m_idx).fy, monster(m_idx).fx, sdam, GF_WIND, 2)
		donthurtmonsters = 0

		update_and_handle()
	end

	-- An aura of Chaos! (power 30)
	if (eventcode == 26) then

		local sdam

		sdam = 30 + (monster(m_idx).level / 3)
		sdam = monster_spell_damages(monster(m_idx), sdam)
		if (get_monster_ability(monster(m_idx), BOSS_DOUBLE_MAGIC)) then sdam = sdam * 2 end

		donthurtmonsters = 1
		lua_project(m_idx, 3, monster(m_idx).fy, monster(m_idx).fx, sdam, GF_CHAOS, 2)
		donthurtmonsters = 0

		update_and_handle()
	end

	-- Balcia(Banshee)'s special move.
	if (eventcode == 27) then

		if (p_ptr.events[1550] == 0 and monster(m_idx).hp <= (monster(m_idx).maxhp / 4)) then

			if (inven(INVEN_ESSENCE).tval > 0 and inven(INVEN_WIELD).tval == 0 and inven(INVEN_WIELD+1).tval == 0) then

				show_dialog(1541)
				inven(INVEN_ESSENCE).disabled = 20
			elseif (inven(INVEN_WIELD).tval > 0 or inven(INVEN_WIELD+1).tval > 0) then

				show_dialog(1541)
				inven(INVEN_WIELD).disabled = 20
				inven(INVEN_WIELD+1).disabled = 20
			elseif (inven(INVEN_GLOVES).tval > 0) then
				show_dialog(1541)
				inven(INVEN_HANDS).disabled = 20
			elseif (inven(INVEN_TOOL).tval > 0) then
				show_dialog(1541)
				inven(INVEN_TOOL).disabled = 20
			end
			update_and_handle()
			p_ptr.events[1550] = 1
		end
	end

	-- Teleports away(on a lit grid).
	if (eventcode == 28) then

		if (lua_randint(100) <= 33 and no_monster_teleport == 0) then

			local m_name = ""

			if not (monster(m_idx).ml) then
				m_name = "it"
			elseif (get_monster_flag1(monster(m_idx).r_idx, RF1_UNIQUE)) then
				m_name = m_race(monster(m_idx).r_idx).name_char
			else
				m_name = string.format('%s %s', "The", m_race(monster(m_idx).r_idx).name_char)
			end

			msg_print(string.format('%s teleports!', m_name))
			teleport_away_light(m_idx, 10)
		end
	end

	-- An aura of Warp! (power 30)
	if (eventcode == 29) then

		local sdam

		sdam = 30 + (monster(m_idx).level / 3)
		sdam = monster_spell_damages(monster(m_idx), sdam)
		if (get_monster_ability(monster(m_idx), BOSS_DOUBLE_MAGIC)) then sdam = sdam * 2 end

		donthurtmonsters = 1
		lua_project(m_idx, 3, monster(m_idx).fy, monster(m_idx).fx, sdam, GF_WARP, 2)
		donthurtmonsters = 0

		update_and_handle()
	end

	-- An aura of Darkness! (power 25)
	if (eventcode == 30) then

		local sdam

		sdam = 25 + (monster(m_idx).level / 3)
		sdam = monster_spell_damages(monster(m_idx), sdam)
		if (get_monster_ability(monster(m_idx), BOSS_DOUBLE_MAGIC)) then sdam = sdam * 2 end

		donthurtmonsters = 1
		lua_project(m_idx, 3, monster(m_idx).fy, monster(m_idx).fx, sdam, GF_DARK, 2)
		donthurtmonsters = 0

		update_and_handle()
	end

	-- Spikefiend's Spiked Chains.
	-- Could be used for other monsters... mainly Spikefiend upgrades. :)
	if (eventcode == 31) then

		local m_name = ""

		if not (monster(m_idx).ml) then
			m_name = "it"
		elseif (get_monster_flag1(monster(m_idx).r_idx, RF1_UNIQUE)) then
			m_name = m_race(monster(m_idx).r_idx).name_char
		else
			m_name = string.format('%s %s', "The", m_race(monster(m_idx).r_idx).name_char)
		end

		-- You must be in the monster's line of sight.

		-- Check range
		if (monster(m_idx).cdis <= MAX_RANGE and monster(m_idx).cdis > 1 and not(is_pet(monster(m_idx)))) then

			-- Check path
			if (projectable(monster(m_idx).fy, monster(m_idx).fx, py, px)) then

				msg_print(string.format('%s unleashes magical spiked chains to grab you!', m_name))

				if (monster_hit_player(monster(m_idx), monster(m_idx).skill_magic) == 1) then

					local ddice
					local dside
					local damage

					ddice = 7 + (monster(m_idx).level / 10)
					dside = 11 + (monster(m_idx).level / 5)

					damage = monster_damages(monster(m_idx), ddice, dside, monster(m_idx).str)

					no_magic_return = TRUE
					monster_ranged = TRUE
					lua_project(m_idx, 0, py, px, damage, GF_PHYSICAL, 2)
					monster_ranged = FALSE
					no_magic_return = FALSE

					update_and_handle()

					msg_print(string.format('%s caught you!', m_name))
					teleport_player_to(monster(m_idx).fy,monster(m_idx).fx)

					update_and_handle()
				else

					msg_print("You avoid the chains.")
				end
			end
		end

	end

	-- Place fire fields, AND a fire aura! (more powerful)
	if (eventcode == 32) then

		local fdam

		fdam = 50 + (monster(m_idx).level / 3)
		fdam = monster_spell_damages(monster(m_idx), fdam)
		if (get_monster_ability(monster(m_idx), BOSS_DOUBLE_MAGIC)) then fdam = fdam * 2 end

		donthurtmonsters = 1
		lua_project(m_idx, 10, monster(m_idx).fy, monster(m_idx).fx, fdam, GF_FIRE, 2)
		donthurtmonsters = 0

		place_field_monsters(FEAT_FIRE_FIELD, 10, monster(m_idx).fx, monster(m_idx).fy, fdam)
		update_and_handle()
	end

	-- Recover 10% hp every turns.
	if (eventcode == 2301) then

		if (monster(m_idx).hp < monster(m_idx).maxhp and monster(m_idx).hp > 1) then

			monster(m_idx).hp = monster(m_idx).hp + multiply_divide(monster(m_idx).maxhp, 10, 100)
			if (monster(m_idx).hp > monster(m_idx).maxhp) then monster(m_idx).hp = monster(m_idx).maxhp end

			update_and_handle()
		end
	end

	-- Shadow Mistress's teleportation.
	-- Code has been rewritten so that this ability can be used for more creatures.
	if (eventcode == 2303) then

		local m_name = ""

		if not (monster(m_idx).ml) then
			m_name = "it"
		elseif (get_monster_flag1(monster(m_idx).r_idx, RF1_UNIQUE)) then
			m_name = m_race(monster(m_idx).r_idx).name_char
		else
			m_name = string.format('%s %s', "The", m_race(monster(m_idx).r_idx).name_char)
		end

		-- Check range
		if (monster(m_idx).cdis <= MAX_RANGE and not(is_pet(monster(m_idx)))) then

			-- Check path
			if (projectable(monster(m_idx).fy, monster(m_idx).fx, py, px)) then

				msg_print(string.format('%s teleports to you!', m_name))
				teleport_to_player(m_idx)
			end
		end
	end

	-- Recover 5% hp every turns.
	if (eventcode == 2500) then

		if (monster(m_idx).hp < monster(m_idx).maxhp) then

			monster(m_idx).hp = monster(m_idx).hp + multiply_divide(monster(m_idx).maxhp, 5, 100)
			if (monster(m_idx).hp > monster(m_idx).maxhp) then monster(m_idx).hp = monster(m_idx).maxhp end

			update_and_handle()
		end
	end

	-- Firestorm Elemental.
	-- As you can see, it's Wind aura, although short ranged, is very deadly.
	if (eventcode == 2503) then

		local fdam
		local winddam

		fdam = 50 + (monster(m_idx).level / 3)
		fdam = monster_spell_damages(monster(m_idx), fdam)
		if (get_monster_ability(monster(m_idx), BOSS_DOUBLE_MAGIC)) then fdam = fdam * 2 end

		winddam = 50 + (monster(m_idx).level * 3)
		winddam = monster_spell_damages(monster(m_idx), winddam)
		if (get_monster_ability(monster(m_idx), BOSS_DOUBLE_MAGIC)) then winddam = winddam * 2 end

		donthurtmonsters = 1
		lua_project(m_idx, 1, monster(m_idx).fy, monster(m_idx).fx, winddam, GF_WIND, 2)
		donthurtmonsters = 0

		place_field_monsters(FEAT_FIRE_FIELD, 4, monster(m_idx).fx, monster(m_idx).fy, fdam)
		update_and_handle()
	end

	-- Pyrex has an extremely deadly aura of flames during phase 1!
	if (eventcode == 2505) then

		if (monster(m_idx).lives == 2) then

			local fdam

			fdam = 75 + (monster(m_idx).level * 15)
			fdam = monster_spell_damages(monster(m_idx), fdam)
			if (get_monster_ability(monster(m_idx), BOSS_DOUBLE_MAGIC)) then fdam = fdam * 2 end

			msg_print("Pyrex emits a powerful Fire aura!")

			donthurtmonsters = 1
			lua_project(m_idx, 10, monster(m_idx).fy, monster(m_idx).fx, fdam, GF_FIRE, 2)
			donthurtmonsters = 0

			update_and_handle()
		end

		-- Fore the second phase, Pyrex will use two weaker auras, one being of Wind type.
		if (monster(m_idx).lives == 1) then

			local fdam

			fdam = 60 + (monster(m_idx).level / 3)
			fdam = monster_spell_damages(monster(m_idx), fdam)
			if (get_monster_ability(monster(m_idx), BOSS_DOUBLE_MAGIC)) then fdam = fdam * 2 end

			donthurtmonsters = 1
			lua_project(m_idx, 10, monster(m_idx).fy, monster(m_idx).fx, fdam, GF_WIND, 2)
			lua_project(m_idx, 10, monster(m_idx).fy, monster(m_idx).fx, fdam, GF_FIRE, 2)
			donthurtmonsters = 0

			-- But every 3 turns, he casts new fields.
			if (monster(m_idx).extra1 >= 3) then

				place_field_monsters(FEAT_FIRE_FIELD, 20, monster(m_idx).fx, monster(m_idx).fy, fdam)
				monster(m_idx).extra1 = 0
			else
				monster(m_idx).extra1 = monster(m_idx).extra1 + 1
			end

			update_and_handle()
		end

		-- Third phase, he stops using auras. Instead, an elemental will appear
		-- every turns!
		if (monster(m_idx).lives == 0) then

			summon_specific_ridx(monster(m_idx).fy, monster(m_idx).fx, 2504, FALSE, FALSE, 0)
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

		basepower = m_race(monster(m_idx).r_idx).level / 3
		if (basepower < 1) then basepower = 1 end
		rad = (m_race(monster(m_idx).r_idx).level / 10) - 2
		if (rad < 2) then rad = 2 end
		if (rad > 10) then rad = 10 end

		element = (m_race(monster(m_idx).r_idx).event_misc * (-1))

		sdam = basepower + (monster(m_idx).level / 3)
		sdam = monster_spell_damages(monster(m_idx), sdam)
		if (get_monster_ability(monster(m_idx), BOSS_DOUBLE_MAGIC)) then sdam = sdam * 2 end

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

		basepower = m_race(monster(m_idx).r_idx).level / 3
		if (basepower < 1) then basepower = 1 end
		rad = (m_race(monster(m_idx).r_idx).level / 20) - 2
		if (rad < 2) then rad = 2 end
		if (rad > 10) then rad = 10 end

		fdam = basepower + (monster(m_idx).level / 3)
		fdam = monster_spell_damages(monster(m_idx), fdam)
		if (get_monster_ability(monster(m_idx), BOSS_DOUBLE_MAGIC)) then fdam = fdam * 2 end

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

	-- Additional code for Pyrex.
	if (m_race(monster(m_idx).r_idx).event_misc == 2505) then

		if (monster(m_idx).lives == 1 and p_ptr.events[25010] == 0) then

			local fdam

			p_ptr.events[25010] = 1
			show_dialog(25007)
			msg_print("Pyrex makes fire rains down on the floor, creating fire fields everywhere!")

			fdam = 60 + (monster(m_idx).level / 3)
			fdam = monster_spell_damages(monster(m_idx), fdam)
			if (get_monster_ability(monster(m_idx), BOSS_DOUBLE_MAGIC)) then fdam = fdam * 2 end

			place_field_monsters(FEAT_FIRE_FIELD, 20, monster(m_idx).fx, monster(m_idx).fy, fdam)
			update_and_handle()
		end

		if (monster(m_idx).lives == 0 and p_ptr.events[25010] == 1) then

			local fdam

			p_ptr.events[25010] = 2
			show_dialog(25007)
		end

		if (cave(monster(m_idx).fy, monster(m_idx).fx).feat == 221) then

			local m_name = ""

			if not (monster(m_idx).ml) then
				m_name = "it"
			elseif (get_monster_flag1(monster(m_idx).r_idx, RF1_UNIQUE)) then
				m_name = m_race(monster(m_idx).r_idx).name_char
			else
				m_name = string.format('%s %s', "The", m_race(monster(m_idx).r_idx).name_char)
			end

			if (monster(m_idx).hp < monster(m_idx).maxhp and monster(m_idx).lives > 0) then

				monster(m_idx).hp = monster(m_idx).maxhp
				if (monster(m_idx).hp > monster(m_idx).maxhp) then monster(m_idx).hp = monster(m_idx).maxhp end
				msg_print(string.format('%s absorbs flames from the fire fields, and heals himself!', m_name))
				update_and_handle()
			end

			monster(m_idx).extra2 = monster(m_idx).extra2 + 1
			msg_print(string.format('%s gathers energy from the flames... (%d/10)', m_name, monster(m_idx).extra2))

			if (monster(m_idx).extra2 >= 10) then

				local fdam

				fdam = 100 + (monster(m_idx).level * 30)
				fdam = monster_spell_damages(monster(m_idx), fdam)
				if (get_monster_ability(monster(m_idx), BOSS_DOUBLE_MAGIC)) then fdam = fdam * 2 end

				msg_print(string.format('%s unleashes all gathered energy!!', m_name))

				donthurtmonsters = 1
				lua_project(m_idx, 10, monster(m_idx).fy, monster(m_idx).fx, fdam, GF_FIRE, 2)
				lua_project(m_idx, 10, monster(m_idx).fy, monster(m_idx).fx, fdam, GF_FIRE, 2)
				lua_project(m_idx, 10, monster(m_idx).fy, monster(m_idx).fx, fdam, GF_FIRE, 2)
				lua_project(m_idx, 10, monster(m_idx).fy, monster(m_idx).fx, fdam, GF_FIRE, 2)
				lua_project(m_idx, 10, monster(m_idx).fy, monster(m_idx).fx, fdam, GF_FIRE, 2)
				donthurtmonsters = 0

				monster(m_idx).extra2 = 0
				update_and_handle()
			end
		end
	end

	if (eventcode > 0) then monster_events_code(m_idx, eventcode) end
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

-- ITEMS EVENTS

-- Scripts related to item events, such as dropping, destroying, picking up items, etc...

function item_events_code (m_idx, eventcode)

	if (eventcode == 1) then

		msg_print("I bet you're happy, right?")
	end
end

function item_passive_equipped (item, eventcode)

	item_events_code(item, eventcode)
end

function item_passive_carried (item, eventcode)

	item_events_code(item, eventcode)
end

function item_passive_floor (item, eventcode)

	item_events_code(item, eventcode)
end

function item_pickup (item, eventcode)

	item_events_code(item, eventcode)
end

function item_drop (item, eventcode)

	item_events_code(item, eventcode)
end

function item_destroy (item, eventcode)

	item_events_code(item, eventcode)
end

function item_equip (item, eventcode)

	item_events_code(item, eventcode)
end

function item_takeoff (item, eventcode)

	item_events_code(item, eventcode)
end

function item_summon (item, eventcode)

	item_events_code(item, eventcode)
end

function item_unsummon (item, eventcode)

	item_events_code(item, eventcode)
end

function item_spawn (item, eventcode)

	item_events_code(item, eventcode)
end

-- This function are for things that happens during a turn.
-- Used for cooldowns, temporary effects, regeneration, etc...
function world_passive ()

	-- Various cooldowns for abilities.

	-- Ki Restoration.
	if (p_ptr.events[29048] > 0) then p_ptr.events[29048] = p_ptr.events[29048] - 1 end

	-- Jump.
	if (p_ptr.events[29049] > 0) then p_ptr.events[29049] = p_ptr.events[29049] - 1 end

	-- Divine Intervention.
	if (p_ptr.events[29055] > 0) then p_ptr.events[29055] = p_ptr.events[29055] - 1 end
	if (p_ptr.events[29056] > 0) then p_ptr.events[29056] = p_ptr.events[29056] - 1 end

	-- Blessed Blood's effect.
	if (p_ptr.abilities[(CLASS_PRIEST * 10) + 6] >= 1 and resting == 0) then

		local power
		power = spell_damages(1 + (p_ptr.abilities[(CLASS_PRIEST * 10) + 6] / 2), A_WIS, 0)
		lua_project(-2, 0, py, px, power, GF_OLD_HEAL, 1)
	end

	-- Elemental Being's aura.
	if (p_ptr.abilities[(CLASS_ELEM_LORD * 10) + 10] >= 1 and resting == 0 and p_ptr.events[29057] == 1) then

		local dam
		dam = spell_damages(p_ptr.abilities[(CLASS_ELEM_LORD * 10) + 10] * 4, A_INT, 0)

		no_magic_return = TRUE
		no_effect_allies = 1
		no_elemental_damage = 1
		attack_aura(p_ptr.elemlord, dam, 2 + (p_ptr.abilities[(CLASS_ELEM_LORD * 10) + 10] / 10))
		no_elemental_damage = 0
		no_effect_allies = 0
		no_magic_return = FALSE
	end

	-- Restless Force's regen.
	if (p_ptr.abilities[(CLASS_NIGHT1 * 10) + 2] >= 1) then

		local regen = 0
		local percent = 0

		percent = 1 + (p_ptr.abilities[(CLASS_NIGHT1 * 10) + 2] / 3)
		if (percent > 10) then percent = 10 end

		p_ptr.hp = p_ptr.hp + multiply_divide(p_ptr.maxhp, percent, 100)
		if (p_ptr.hp > p_ptr.maxhp) then p_ptr.hp = p_ptr.maxhp end
	end


	-- Other stuff.

	-- Used in Q25000.txt
	if (p_ptr.inside_secret == 0 and p_ptr.events[25007] > 0) then

		-- Reset Last encounter before Pyrex.
		if (not(p_ptr.events[25007] == 2)) then

			p_ptr.events[25007] = 0
		end
	end
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
add_event_handler("item_events_code", item_events_code)
add_event_handler("item_passive_equipped", item_passive_equipped)
add_event_handler("item_passive_carried", item_passive_carried)
add_event_handler("item_passive_floor", item_passive_floor)
add_event_handler("item_pickup", item_pickup)
add_event_handler("item_drop", item_drop)
add_event_handler("item_destroy", item_destroy)
add_event_handler("item_equip", item_equip)
add_event_handler("item_takeoff", item_takeoff)
add_event_handler("item_summon", item_summon)
add_event_handler("item_unsummon", item_unsummon)
add_event_handler("item_spawn", item_spawn)
add_event_handler("world_passive", world_passive)