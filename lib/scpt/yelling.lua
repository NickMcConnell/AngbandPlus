-- A new key... press X to scream Xtremely loud!

add_hooks
{
[HOOK_KEYPRESS] = function(key)
                if key == strbyte('X') then
                    msg_print('You shout:  Cry Havoc! And let Slip the Dogs of War')
                    aggravate_monsters(1)
                    return TRUE
                end
        end,
}

add_hooks
{
	[HOOK_FIRE] = function(object)
		if object.sval == SV_HEAVY_XBOW then
			if object.name2 == 237 then
				if object.pval3 == 0 then
					object.pval3 = 2
					return FALSE
				else
					msg_print("You reload...");
					object.pval3 = object.pval3 - 1
					if object.pval3 == 0 then msg_print("The crossbow is ready") end
					energy_use = (100/player.num_fire)
					return TRUE
				end
			end
		end
	end
}