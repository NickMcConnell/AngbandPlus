-- File: scripts.lua
-- This file contains various functions that can be called in quests
-- or by monsters attacks. If a function is called by a monster during
-- combat, by either a spell or an attack, it must have one parameter
-- that will be the monster's m_idx id. You will then have to reference
-- the monster using the monster(m_idx) function.
-- Example:
-- function my_scripted_attack (m_idx)
--     msg_print(string.format('The monster has %d hp.', monster(m_idx).hp))
-- end
--
-- Other scripted functions do not have a parameter.


-- Used in Q1001.txt
-- This is when you leave the sewers by the slums entrance.
-- It makes sure you get teleported to the slums district of Jindar.
function sewers_exit_to_slums ()

	dun_level = 0
	p_ptr.inside_quest = 0
	p_ptr.town_num = 1015
	p_ptr.wild_x = 46
	p_ptr.wild_y = 41
	p_ptr.startx = 12
	p_ptr.starty = 58
	p_ptr.leaving = TRUE
	generate_cave()
	verify_panel()
end

-- Used in Q1003.txt
-- Leaving the palace uses a script to bring you back to town 1012,
-- in case you used the sewers to enter the palace.
function palace_exit_to_city ()

	dun_level = 0
	p_ptr.inside_quest = 0
	p_ptr.town_num = 1012
	p_ptr.wild_x = 48
	p_ptr.wild_y = 40
	p_ptr.startx = 99
	p_ptr.starty = 27
	p_ptr.leaving = TRUE
	generate_cave()
	verify_panel()
end

-- Used in Q1004.txt
-- When you enter the throne room, a dialog will be shown.
-- Note that event 1005 here is actually event 1004, but due to
-- the way Lua handles arrays, you must add 1 to the event's number.
-- So event 1005 here is really 1004, and 1004 should be used elsewhere.
function jindar_throne_enter ()

	if (p_ptr.events[1005] == 0) then
		p_ptr.events[1005] = 1
		show_dialog(1018)
	end
end

-- Used in T1020.txt
-- When you see the temple for the first time, a dialog appear.
-- Again, event 1020 here is really event 1019.
function twisted_temple_arrive ()

	if (p_ptr.events[1020] == 0) then
		p_ptr.events[1020] = 1
		show_dialog(1026)
	end
end

-- Used in Q1008.txt
-- Enter quest 1009. However, if quest is completed, enter 1005 instead.
-- Once again, event 1022 is really 1021.
function go_inside_twisted_temple ()

	-- Player enters a new quest
	--p_ptr.oldpy = py
	--p_ptr.oldpx = px

	if (p_ptr.events[1022] == 1) then

		p_ptr.inside_quest = 1005
	else
		p_ptr.inside_quest = 1009
	end

	dun_level = 0

	p_ptr.questx = 26
	p_ptr.questy = 14
	p_ptr.leaving = TRUE
	generate_cave()
	verify_panel_always_update()
end

-- Used in Q1009.txt
-- Just a dialog when you enter the twisted temple for the first time.
function twisted_temple_first_time ()

	if (p_ptr.events[1023] == 0) then
		p_ptr.events[1023] = 1
		show_dialog(1028)
	end
end



-- ########## SPECIAL ATTACKS OF MONSTERS ##########

-- Quazar's reality twisting magic.
-- There are two options here:
-- 1. Will set your experience to 0.
-- 2. Sets mana to 0, and wisdom to 1!
-- 3. Move the player around.
function reality_twists_quazar (m_idx)

	local twist_result
	local tmpx
	local tmpy
	
	twist_result = lua_randint(100)

	if (twist_result >= 66) then

		show_dialog(1032)
		p_ptr.exp = 0
		p_ptr.lev = 1
		check_experience()
		update_and_handle()
	elseif (twist_result >= 33) then

		show_dialog(1033)
		p_ptr.csp = 0
		dec_stat(A_WIS, 3000, 2)
		update_and_handle()
	else
		msg_print("Quazar teleports you around!")
		p_ptr.inside_quest = 0
		teleport_player(5)
		p_ptr.inside_quest = 1013
		update_and_handle()
	end
end

-- Event handlers should be added for every functions you plan on using.
add_event_handler("sewers_exit_to_slums", sewers_exit_to_slums)
add_event_handler("palace_exit_to_city", palace_exit_to_city)
add_event_handler("jindar_throne_enter", jindar_throne_enter)
add_event_handler("twisted_temple_arrive", twisted_temple_arrive)
add_event_handler("go_inside_twisted_temple", go_inside_twisted_temple)
add_event_handler("twisted_temple_first_time", twisted_temple_first_time)
add_event_handler("reality_twists_quazar", reality_twists_quazar)
