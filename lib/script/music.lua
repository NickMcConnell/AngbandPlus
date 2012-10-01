-- File: music.lua
-- This file contains code related to the new Music system and skill!

-- When you use 'n', this menu will be brought up.
function music_menu ()

	local ch

	if (p_ptr.skill[29] > 0) then

		if (dun_level == 0) then
			msg_print("[S]ing a Song, [C]ompose a song?")

			ch = inkey()

			if (ch == 83 or ch == 115) then
				sing_song()
			elseif (ch == 67 or ch == 99) then
				compose_song()
			else
				return
			end
		else
			sing_song()
		end
	else
		msg_print("You cannot use this command without any skill in Music.")
	end
end

-- Sing a song.
-- You can have up to 15 songs.
function sing_song ()

	local song
	local reduction

	-- Charisma requirement reduction.
	reduction = p_ptr.skill_base[29] / 2

	-- pick_song() is hardcoded for now.
	song = pick_song(reduction)

	if (song >= 15) then return end

	-- Make sure song exists.
	if (not(music_song[song+1].created)) then

		msg_print("You did not compose any songs in this slot.")
		return
	end

	-- To use any music, you must have a Charisma of 5+.
	if (p_ptr.stat_ind[A_CHR+1] < 5) then

		msg_print("You need a Charisma of at least 5 to use any songs.")
		return
	end

	-- Cannot use music while confused.
	if (p_ptr.confused > 0) then

		msg_print("You cannot use songs while confused.")
		return
	end

	-- Check if you have enough charisma.
	if (p_ptr.stat_ind[A_CHR+1] >= (music_song[song+1].cost - reduction)) then

		if (music_song[song+1].type == 1 or music_song[song+1].type == 3) then

			p_ptr.events[29041] = song
			p_ptr.events[29042] = 1

		else
			execute_song(song)
			energy_use = 100
		end
	else
		msg_print("Your Charisma is too low to use this song.")
	end
end

-- Compose a song.
function compose_song ()

	local melody
	local harmony
	local rhythm
	local totalcost

	local discost
	local reduction
	local song
	local songname
	local songtype
	local ch
	local typech

	-- Charisma cost reduction.
	reduction = p_ptr.skill_base[29] / 2

	-- First, get a melody.
	msg_print("Choose a melody.")
	melody = lua_pick_item(TV_MELODY)

	if (not(melody)) then
		msg_print("Cannot compose song without a melody.")
		return
	end

	-- Then an harmony.
	msg_print("Choose an harmony.")
	harmony = lua_pick_item(TV_HARMONY)

	if (not(harmony)) then
		msg_print("Cannot compose song without an harmony.")
		return
	end

	-- And finally, a rhythm.
	msg_print("Choose a rhythm.")
	rhythm = lua_pick_item(TV_RHYTHM)

	if (not(rhythm)) then
		msg_print("Cannot compose song without a rhythm.")
		return
	end

	-- Choose a type.
	msg_print("Song type: [P]assive, [I]instant, [E]ffect?")

	typech = inkey()

	if (typech == 80 or typech == 112) then songtype = 1
	elseif (typech == 73 or typech == 105) then songtype = 2
	elseif (typech == 69 or typech == 101) then songtype = 3
	else return end

	-- Total cost.
	-- This is the required charisma for the song.
	totalcost = melody.pval2 + harmony.pval2 + rhythm.pval2
	discost = totalcost - reduction
	if (discost < 0) then discost = 0 end

	msg_print(string.format('You will need %d charisma for this song. Compose it? [y/n]', discost))
	ch = inkey()

	if (ch == 89 or ch == 121) then

		-- Enter song's name.
		msg_print("Enter a name for the song: ")
		lua_get_string(30)
		songname = tmpluastring

		-- Pick a slot for the song.
		song = pick_song(reduction)

		music_song[song+1].name = songname
		music_song[song+1].type = songtype
		music_song[song+1].power = harmony.pval
		music_song[song+1].element = melody.pval
		music_song[song+1].radius = rhythm.pval
		music_song[song+1].cost = totalcost
		music_song[song+1].created = TRUE

		msg_print("You composed a new song!")
	end



end

-- The function that actually have songs do something.
function execute_song (num)

	local mode
	local dam
	local bonus
	local stat
	local instrumentbonus

	-- Reset some variables, just in case.
	no_effect_allies = 0
	no_effect_hostile = 0
	music = 0
	instrumentbonus = 0

	-- Determine the mode. Whether the song will affect you or not.
	if (music_song[num+1].type == 3) then mode = -2
	else mode = 0
	end

	if (inven(INVEN_TOOL).tval == TV_INSTRUMENT) then

		instrumentbonus = damroll(inven(INVEN_TOOL).dd, inven(INVEN_TOOL).ds)
		instrumentbonus = instrumentbonus + multiply_divide(instrumentbonus, p_ptr.skill[29] * 3, 100)
	elseif (p_ptr.prace == RACE_MONSTER and inven(INVEN_ESSENCE).tval > 0) then

		instrumentbonus = damroll(inven(INVEN_ESSENCE).dd, inven(INVEN_ESSENCE).ds)
		instrumentbonus = instrumentbonus + multiply_divide(instrumentbonus, p_ptr.skill[29] * 3, 100)
	end

	-- Music skill and Charisma enhance songs in different ways, depending on the type of song.
	-- Effects from songs are considered magical.

	-- Duration (Summon monsters)
	if (music_song[num+1].element == GF_DURATION) then

		stat = p_ptr.stat_ind[A_CHR+1] - 5
		bonus = (stat * 3) + (p_ptr.skill[29] * 3) + (p_ptr.skill[2])
		dam = music_song[num+1].power + (instrumentbonus / 5)
		dam = dam + multiply_divide(dam, bonus, 100)
	elseif (music_song[num+1].element == GF_OLD_HEAL and music_song[num+1].type == 3) then
		bonus = (p_ptr.skill[29] * 2)
		stat = p_ptr.stat_ind[A_CHR+1] - 5
		if (stat <= 0) then stat = 0 end
		dam = ((music_song[num+1].power + instrumentbonus) * (stat))
		dam = dam + multiply_divide(dam, bonus, 100)
		dam = dam + multiply_divide(dam, p_ptr.to_s, 100)
	else
		bonus = (p_ptr.skill[29] * 20) + (p_ptr.skill[2] * 10)
		stat = p_ptr.stat_ind[A_CHR+1] - 5
		if (stat <= 0) then stat = 0 end
		dam = ((music_song[num+1].power + instrumentbonus) * (stat))
		dam = dam + multiply_divide(dam, bonus, 100)
		dam = dam + multiply_divide(dam, p_ptr.to_s, 100)

		-- Improved Songs Bard ability.
		if (p_ptr.abilities[(CLASS_BARD * 10) + 1] > 0) then

			dam = dam + multiply_divide(dam, p_ptr.abilities[(CLASS_BARD * 10) + 1] * 10, 100)
		end
	end

	-- No negative damages.
	if (dam < 0) then dam = 0 end

	-- It's a music effect.
	music = 1

	if (music_song[num+1].element == GF_SOUND) then no_effect_allies = 1 end
	if (music_song[num+1].element == GF_OLD_HEAL) then no_effect_hostile = 1 end
	if (music_song[num+1].element == GF_DURATION) then

		if (dam == 0) then no_effect_allies = 1
		else no_effect_hostile = 1 end
	end

	ignore_spellcraft = TRUE
	lua_project(mode, music_song[num+1].radius + (p_ptr.abilities[(CLASS_BARD * 10) + 1] / 10), py, px, dam, music_song[num+1].element, 1)
	ignore_spellcraft = FALSE

	no_effect_allies = 0
	no_effect_hostile = 0
	music = 0

end

add_event_handler("music_menu", music_menu)
add_event_handler("sing_song", sing_song)
add_event_handler("compose_song", compose_song)
add_event_handler("execute_song", execute_song)
