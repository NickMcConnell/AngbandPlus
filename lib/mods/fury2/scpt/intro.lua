function drop_text_left(c, str, y, o)
	local i = strlen(str)
	local x = 39 - (strlen(str) / 2) + o
	while (i > 0)
	do
		local a = 0
		local time = 0

		if (strbyte(str, i) ~= strbyte(" ", 1)) then
			while (a < x + i - 1)
			do
				Term_putch(a - 1, y, c, 32)
				Term_putch(a, y, c, strbyte(str, i))
				time = time + 1
				if time >= 4 then
					Term_xtra(TERM_XTRA_DELAY, 1)
					time = 0
				end
				Term_redraw_section(a - 1, y, a, y)
				a = a + 1

				inkey_scan = TRUE
				if (inkey() ~= 0) then
					return TRUE
				end
			end
		end

		i = i - 1
	end
	return FALSE
end

function drop_text_right(c, str, y, o)
	local x = 39 - (strlen(str) / 2) + o
	local i = 1
	while (i <= strlen(str))
	do
		local a = 79
		local time = 0

		if (strbyte(str, i) ~= strbyte(" ", 1)) then
			while (a >= x + i - 1)
			do
				Term_putch(a + 1, y, c, 32)
				Term_putch(a, y, c, strbyte(str, i))
				time = time + 1
				if time >= 4 then
					Term_xtra(TERM_XTRA_DELAY, 1)
					time = 0
				end
				Term_redraw_section(a, y, a + 1, y)
				a = a - 1

				inkey_scan = TRUE
				if (inkey() ~= 0) then
					return TRUE
				end
			end
		end

		i = i + 1
	end
	return FALSE
end

function tome_intro()
	screen_save()
	Term_clear()

	if (TRUE == drop_text_left(TERM_L_BLUE, "Three Rings for the Elven-kings under the sky,", 10, 0)) then screen_load() return end
	if (TRUE == drop_text_right(TERM_L_BLUE, "Seven for the Dwarf-lords in their halls of stone,", 11, -1)) then screen_load() return end
	if (TRUE == drop_text_left(TERM_L_BLUE, "Nine for Mortal Men doomed to die,", 12, 0)) then screen_load() return end
	if (TRUE == drop_text_right(TERM_L_BLUE, "One for the Dark Lord on his dark throne", 13, -1)) then screen_load() return end
	if (TRUE == drop_text_left(TERM_L_BLUE, "In the land of Mordor, where the Shadows lie.", 14, 0)) then screen_load() return end
	if (TRUE == drop_text_right(TERM_L_BLUE, "One Ring to rule them all, One Ring to find them,", 15, -1)) then screen_load() return end
	if (TRUE == drop_text_left(TERM_L_BLUE, "One Ring to bring them all and in the darkness bind them", 16, 0)) then screen_load() return end
	if (TRUE == drop_text_right(TERM_L_BLUE, "In the land of Mordor, where the Shadows lie.", 17, -1)) then screen_load() return end
	if (TRUE == drop_text_right(TERM_L_GREEN, "--J.R.R. Tolkien", 18, 0)) then screen_load() return end
	if (TRUE == drop_text_left(TERM_WHITE, "[Press any key to continue]", 23, -1)) then screen_load() return end

	Term_putch(0, 0, TERM_DARK, 32)
	inkey_scan = FALSE
	inkey()

	Term_clear()

	if (TRUE == drop_text_left(TERM_L_BLUE, "The Fury", 8, 0)) then screen_load() return end
	if (TRUE == drop_text_right(TERM_WHITE, "with contributions from", 9, -1)) then screen_load() return end
	if (TRUE == drop_text_left(TERM_L_GREEN, "Theme, T-Plus, Tome and a Half,", 10, 0)) then screen_load() return end
	if (TRUE == drop_text_right(TERM_L_GREEN, "NTangband, and Zangband", 11, -1)) then screen_load() return end
	if (TRUE == drop_text_left(TERM_WHITE, "and", 12, 0)) then screen_load() return end
	if (TRUE == drop_text_right(TERM_L_GREEN, "by the grace of the Valar", 13, -1)) then screen_load() return end

	if (TRUE == drop_text_left(TERM_WHITE, "present", 15, 1)) then screen_load() return end
	if (TRUE == drop_text_right(TERM_YELLOW, "FuryBand", 16, 0)) then screen_load() return end

	if (TRUE == drop_text_left(TERM_WHITE, "[Press any key to continue]", 23, -1)) then screen_load() return end
	Term_putch(0, 0, TERM_DARK, 32)

	inkey_scan = FALSE

	inkey()

	screen_load()
	return
end

add_hook_script(HOOK_INIT, "tome_intro", "lua_intro_init")
