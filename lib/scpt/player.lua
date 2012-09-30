-- SYSTEM FILE
--
-- Lua player funtions
--

-- modify mana
-- returns TRUE if there is a pb
function increase_mana(amt)
        player.csp = player.csp + amt
        player.redraw = bor(player.redraw, PR_MANA)
        if (player.csp < 0) then
                player.csp = 0
                return TRUE
        end
        if (player.csp > player.msp) then
                player.csp = player.msp
        end
        return FALSE
end


-- Create a new power
__power_fct = {}
function add_power(p)
        local i

        assert(p.name, "No power name!")
        assert(p.desc, "No power desc!")
        assert(p.desc_get, "No power desc get!")
        assert(p.desc_lose, "No power desc lose!")
        assert(p.stat, "No power stat!")
        assert(p.level, "No power level!")
        assert(p.cost, "No power cost!")
        assert(p.fail, "No power fail!")
        assert(p.power, "No power power!")

	i = add_new_power(p.name, p.desc, p.desc_get, p.desc_lose, p.level, p.cost, p.stat, p.fail)
        __power_fct[i] = p.power
        return i
end

function __power_fct_activate(power)
        if __power_fct[power] then
                __power_fct[power]()
                return TRUE
        else
                return FALSE
        end
end

-- Register in the hook list
add_hook_script(HOOK_ACTIVATE_POWER, "__power_fct_activate", "__power_fct_activate")


--- Mkeys

-- Create a new power
__mkey_fct = {}
function add_mkey(p)
        local i

        assert(p.mkey, "No mkey mkey!")
        assert(p.fct, "No mkeey fct!")

        __mkey_fct[p.mkey] = p.fct
end

function __mkey_fct_activate(power)
        if __mkey_fct[power] then
                __mkey_fct[power]()
                return TRUE
        else
                return FALSE
        end
end

-- Register in the hook list
add_hook_script(HOOK_MKEY, "__mkey_fct_activate", "__mkey_fct_activate")




------------------------------------------------------------------------------
----------------------- Hook to create birth objects -------------------------
------------------------------------------------------------------------------
function __birth_hook_objects()
        -- Provide a book of blink to rangers
        if get_class_name() == "Ranger" then
                local obj = create_object(TV_BOOK, 255);
                obj.pval = find_spell("Phase Door")
                inven_carry(obj, FALSE)
                end_object(obj)
        end

        -- Provide a book of prayer to priests
        if get_class_name() == "Priest(Eru)" then
                local obj = create_object(TV_BOOK, 255);
                obj.pval = find_spell("See the Music")
                inven_carry(obj, FALSE)
                end_object(obj)
        end
        if get_class_name() == "Priest(Manwe)" then
                local obj = create_object(TV_BOOK, 255);
                obj.pval = find_spell("Manwe's Blessing")
                inven_carry(obj, FALSE)
                end_object(obj)
        end
        if get_class_name() == "Dark-Priest" then
                local obj = create_object(TV_BOOK, 255);
                obj.pval = find_spell("Curse")
                inven_carry(obj, FALSE)
                end_object(obj)
        end
        if get_class_name() == "Paladin" then
                local obj = create_object(TV_BOOK, 255);
                obj.pval = find_spell("Divine Aim")
                inven_carry(obj, FALSE)
                end_object(obj)
        end
end

-- Register in the hook list
add_hook_script(HOOK_BIRTH_OBJECTS, "__birth_hook_objects", "__birth_hook_objects")
