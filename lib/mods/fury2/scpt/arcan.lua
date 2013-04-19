-- Handling for the Alchemic Arcanorum

-- This isn't exported to Lua, so we hardcode it here.
MAX_BATERIE_SVAL = 18

-- Essence names, indexed by sval.
ess_nam = {
	"Poison",
	"Explosion",
	"Teleport",
	"Cold",
	"Fire",
	"Acid",
	"Life",
	"Confusion",
	"Light",
	"Chaos",
	"Time",
	"Magic",
	"ExtraLife",
	"Darkness",
	"Knowledge",
	"Force",
	"Lightning",
	"Mana",
}
-- The Arcanorum data --- how many of each essence is currently stored.
-- This is shared across all Arcanora (though we arrange for there to be
-- only one).  We'd rather index by number than by name, but add_loadsave()
-- can only save tables with name-based indices.
player.arcanorum = {
	["Poison"]	= 0,
	["Explosion"]	= 0,
	["Teleport"]	= 0,
	["Cold"]	= 0,
	["Fire"]	= 0,
	["Acid"]	= 0,
	["Life"]	= 0,
	["Confusion"]	= 0,
	["Light"]	= 0,
	["Chaos"]	= 0,
	["Time"]	= 0,
	["Magic"]	= 0,
	["ExtraLife"]	= 0,
	["Darkness"]	= 0,
	["Knowledge"]	= 0,
	["Force"]	= 0,
	["Lightning"]	= 0,
	["Mana"]	= 0,
}
add_loadsave("player.arcanorum", player.arcanorum)

-- Small Annoying Hack(TM):  We can't use "Extra Life" as a field name in
-- the arcanorum table because it chokes add_loadsave().  Instead, we
-- "pretty-print" the essence name to get the right name for display
-- purposes.
function ess_nam_pretty(i)
	if ess_nam[i] == "ExtraLife" then
		return "Extra Life"
	else
		return ess_nam[i]
	end
end

-- Utility function:  draw the extraction menu on screen, showing how many
-- of each type of essence the player currently wants to extract.
function arcanorum_menu (t)
	local i
	local r = strchar(strbyte("a") + MAX_BATERIE_SVAL - 1)
	local R = strchar(strbyte("A") + MAX_BATERIE_SVAL - 1)

	c_prt(TERM_WHITE, "Arcanorum Extraction Screen", 0, 0)
	c_prt(TERM_WHITE, "a-"..r.." to increase, A-"..R.." to decrease, RETURN to finish, ESC to cancel", 1, 0)
	c_prt(TERM_WHITE, "                           ", 2, 0)
	for i = 1, MAX_BATERIE_SVAL do
		local char = strbyte("a") + i - 1
		local str = format("   %c) %-12s  %2d/%d", char, ess_nam_pretty(i), t[i], player.arcanorum[ess_nam[i]])
		local color = TERM_WHITE
		if t[i] > 0 then color = TERM_GREEN end
		c_prt(color, str, i + 2, 0)
	end
end

-- The Arcanorum's activatation.
function act_arcanorum ()
	local ret, c

	ret, c = get_com("[A]bsorb or e[X]tract essences?", strbyte("a"))
	if ret == FALSE then return TRUE end
	if c == strbyte("a") then
		local i = 0
		while i < INVEN_WIELD do
			local obj = player.inventory(i)
			if obj == nil then break end
			if obj.tval == TV_BATERIE and obj.sval <= MAX_BATERIE_SVAL and obj.number > 0 then
				local nam = ess_nam[obj.sval]
				player.arcanorum[nam] = player.arcanorum[nam] + obj.number
				inven_item_increase(i, -obj.number)
				inven_item_describe(i)
				inven_item_optimize(i)
			else
				i = i + 1
			end
		end
	elseif c == strbyte("x") then
		local i
		local done = FALSE
		local n = {}
		for i = 1, MAX_BATERIE_SVAL do n[i] = 0 end
		local a = strbyte('a')
		local A = strbyte('A')
		local r = a + MAX_BATERIE_SVAL - 1
		local R = A + MAX_BATERIE_SVAL - 1

		screen_save()
		while done == FALSE do
			local char

			arcanorum_menu(n)
			inkey_scan = FALSE
			inkey_base = TRUE
			char = inkey()
			if char == ESCAPE or char == strbyte('\r') then
				done = TRUE
			end
			if char == strbyte('\r') then
				for i = 1, MAX_BATERIE_SVAL do
					local nam = ess_nam[i]
					player.arcanorum[nam] = player.arcanorum[nam] - n[i]
					if n[i] > 0 then
						local obj = create_object(TV_BATERIE, i)
						obj.number = n[i]
						if inven_carry_okay(obj) then
							local idx = inven_carry(obj, FALSE)
							inven_item_describe(idx)
						else
							drop_near(obj, 0, player.px, player.py)
						end
						end_object(obj)
					end
				end
			elseif char >= a and char <= r then
				i = char - a + 1
				local nam = ess_nam[i]
				if n[i] < 99 and n[i] < player.arcanorum[nam] then
					n[i] = n[i] + 1
				end
			elseif char >= A and char <= R then
				i = char - A + 1
				if n[i] > 0 then n[i] = n[i] - 1 end
			end
		end
		screen_load()
	end
end

-- Hook to give Alchemists an Arcanorum in their starting inventory.  Also
-- clear the Arcanorum storage at the same time.
function arcanorum_inven_start ()
	if get_class_name() == "Alchemist" then
		local i
		local obj = create_object(TV_BATERIE, 255)
		obj.ident = bor(obj.ident, IDENT_MENTAL, IDENT_KNOWN)
		inven_carry(obj, FALSE)
		end_object(obj)
		for i = 1, MAX_BATERIE_SVAL do
			player.arcanorum[ess_nam[i]] = 0
		end
	end
end

-- Hook to describe the Arcanorum's contents.  There's no generic hook for
-- this, so we cheat and hijack HOOK_ACTIVATE_DESC.
function arcanorum_describe (obj)
	local s = "absorbing or extracting Essences"
	-- If it's not an Arcanorum, we're not interested
	if obj.tval ~= TV_BATERIE or obj.sval ~= 255 then return FALSE end

	local first = 0
	local last = 0
	local i
	for i = 1, MAX_BATERIE_SVAL do
		if player.arcanorum[ess_nam[i]] > 0 then
			last = i
			if first == 0 then first = i end
		end
	end

	if first > 0 then
		for i = 1, MAX_BATERIE_SVAL do
			local n = player.arcanorum[ess_nam[i]]
			if n > 0 then
				if i == first then
					s = s .. ".  It contains "
				elseif i == last then
					s = s .. " and "
				else
					s = s .. ", "
				end
				if n == 1 then
					s = s .. "an Essence of "
				else
					s = s .. n .. " Essences of "
				end
				s = s .. ess_nam_pretty(i)
			end
		end
	else
		s = s .. ".  It contains no Essences"
	end

	return TRUE, s
end

-- The "spell" attached to the Arcanorum's activation.
DEVICE_ARCANORUM = add_spell
{
	["name"]	= "Use Arcanorum",
	["school"]	= {SCHOOL_DEVICE},
	["level"]	= 1,
	["mana"]	= 0,
	["mana_max"]	= 0,
	["fail"]	= 0,
	["random"]	= -1,
	["activate"]	= { 0, 0 },
	["spell"]	= act_arcanorum,
	["info"]	= function() return "" end,
	["desc"]	= { "absorbing or extracting essences" }
}

add_hook_script(HOOK_BIRTH_OBJECTS, "arcanorum_inven_start", "arcanorum_inven_start")
add_hook_script(HOOK_ACTIVATE_DESC, "arcanorum_describe", "arcanorum_describe")
