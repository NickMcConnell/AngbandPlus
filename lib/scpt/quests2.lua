-- Quest helper files

-- Savefile helpers

-- function called when a key in the variable part ofthe savefile is read
-- if the key matches what we need, we use it, otehrwise just ignore it
function __savefile_load(key, val)
	local index, name
        
        for index, name in __loadsave_name do
	        if (key == name) then
        	        dostring(name.." = "..val)
	       	end
        end
end

-- called when the game is saved, can only save numbers
-- assosiate a key with them to allow the loading code to recognize them
function __savefile_save()
	local index, name
        for index, name in __loadsave_name do
        	dostring("__loadsave_tmp = "..name)
	        save_number_key(name, __loadsave_tmp);
	end
end

-- Savefile end

register_savefile(__loadsave_max)
add_hook_script(HOOK_LOAD_GAME, "__savefile_load", "__hook_load")
add_hook_script(HOOK_SAVE_GAME, "__savefile_save", "__hook_save")
