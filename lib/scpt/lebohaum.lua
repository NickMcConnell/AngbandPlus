-- -- lines are comments
-- It doesnt add anything to game play, but provides
-- a little example of how to use lua scripting

function activate_lebohaum_desc(tval, sval, artifact, egoitem)
        -- Is it lebohaum ?
        if (artifact == 165) then
		return TRUE, "sing a funny song every 3 turns"
        end
        return FALSE
end

function activate_lebohaum(item)
        -- Ask for the object fiven it's index
        local o_ptr = get_object(item)

        -- Is it lebohaum ?
        if (o_ptr.name1 == 165) then
                msg_print("You hear a little song in your head: \"With the helm Lebohaum, your head is *SAFE* !\"")
                o_ptr.timeout = 3

                -- return TRUE to stop parsing the hooks
                return TRUE
        end
        -- return FALSE to continue parsing the hooks
        return FALSE
end

-- Register ourselves
add_hook_script(HOOK_ACTIVATE_DESC, "activate_lebohaum_desc", "activate_lebohaum_desc")
add_hook_script(HOOK_ACTIVATE, "activate_lebohaum", "activate_lebohaum")
