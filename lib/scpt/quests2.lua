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

--[[
add_quest
{
        ["global"] =    "MY_QUEST",
        ["name"] =      "My funny quest",
        ["desc"] =      {
                        "My funny quest desc",
        },
        ["level"] =     42,
        ["data"] =      {
                        ["test_var"] = 0,
                        ["test_var2"] = 1
        },
        ["hooks"] =     {
                        [HOOK_BIRTH_OBJECTS] = function()
                                quest(MY_QUEST).status = QUEST_STATUS_TAKEN
                        end,
                        [HOOK_BUILD_ROOM1] = function(by0, bx0)
                                local x, y, y2, x2, ret

                                y, x = get_map_size("qrand6.map")
                                ret, y, x, y2, x2 = alloc_room(by0, bx0, y, x)
                                if ret == TRUE then
                                        y, x, y2, x2 = load_map("qrand6.map", y, x)
                                end
                        end,
        },
}
]]

-- Savefile end

register_savefile(__loadsave_max)
add_hook_script(HOOK_LOAD_GAME, "__savefile_load", "__hook_load")
add_hook_script(HOOK_SAVE_GAME, "__savefile_save", "__hook_save")
