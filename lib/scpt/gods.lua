add_hooks
{
	[HOOK_FOLLOW_GOD] = function(god, action)
		if action == "ask" then
			if not (god == GOD_MELKOR) then
				local i = INVEN_WIELD
				while i < INVEN_TOTAL do
					-- 13 is ART_POWER
					if player.inventory(i).name1 == 13 then
						msg_print("The One Ring has corrupted you, and you are rejected.")
						return TRUE
					end
					i = i + 1
				end
			end
		end
		return FALSE
	end,
}
