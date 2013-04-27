-- Helper functions for various things

function summon_monsters(num, kind)
	local ident; ident = FALSE
	
	for k = 0, num do
		if summon_specific(0, player.px, player.py, player.depth,
				kind, TRUE, FALSE, FALSE) then
			ident = TRUE
		end
	end

	return ident
end

function restore_mana()
	if player.csp < player.msp then
		player.csp = player.msp
		player.csp_frac = 0
		msgf("You feel your head clear.")
		player.redraw = bOr(player.redraw, PR_MANA)
		player.window = bOr(player.window, PW_PLAYER)
		player.window = bOr(player.window, PW_SPELL)
		return TRUE
	else
		return FALSE
	end
end

function cure_all_mutations()
	if (player.muta1 ~= 0) or (player.muta2 ~= 0) or (player.muta3 ~= 0) then
		msgf("You are cured of all mutations.")
		player.muta1 = 0
		player.muta2 = 0
		player.muta3 = 0
		player.update = bOr(player.update, PU_BONUS)
		handle_stuff()
		return TRUE
	else
		return FALSE
	end
end

