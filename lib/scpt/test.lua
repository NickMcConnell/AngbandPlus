--
-- This file takes care of providing the Shiny-Test class
-- with .. erm .. powerfull spells
--

------------------------------ MAGICTYPE -- M KEY ---------------------------------

-- Register a new magic type
MKEY_SHINY_TEST = 1000

-- Create it
MAX_SHINY_TEST_SPELL = 4
shiny_test_magic = new_magic_power(MAX_SHINY_TEST_SPELL)

-- register the first member
get_magic_power(shiny_test_magic, 0).name = "Obj"
get_magic_power(shiny_test_magic, 0).desc = "Groovy Obj"
get_magic_power(shiny_test_magic, 0).mana_cost = 4
get_magic_power(shiny_test_magic, 0).min_lev = 1
get_magic_power(shiny_test_magic, 0).fail = 10

-- register the second member
get_magic_power(shiny_test_magic, 1).name = "Kill"
get_magic_power(shiny_test_magic, 1).desc = "Groovy Kill"
get_magic_power(shiny_test_magic, 1).mana_cost = 5
get_magic_power(shiny_test_magic, 1).min_lev = 10
get_magic_power(shiny_test_magic, 1).fail = 60

-- register the third member
get_magic_power(shiny_test_magic, 2).name = "Uber-ize"
get_magic_power(shiny_test_magic, 2).desc = "Groovy Uber"
get_magic_power(shiny_test_magic, 2).mana_cost = 10
get_magic_power(shiny_test_magic, 2).min_lev = 12
get_magic_power(shiny_test_magic, 2).fail = 13

-- register the 4th member
get_magic_power(shiny_test_magic, 3).name = "Summon"
get_magic_power(shiny_test_magic, 3).desc = "Groovy Monster"
get_magic_power(shiny_test_magic, 3).mana_cost = 10
get_magic_power(shiny_test_magic, 3).min_lev = 15
get_magic_power(shiny_test_magic, 3).fail = 13

-- Provide info on the magic type
function shiny_test_magic_info(power)
	if (power == 0) then return " groovy" end
	if (power == 1) then return " dam 2000" end
	if (power == 2) then return " UBBBBBER" end
	if (power == 3) then return " MONSTTTTER" end
	return " zogzog"
end


-- Summon a greater balrog
function shiny_summon()
	local list = {[1] = "Novice Warrior", [2] = "Novice Mage"}
	local x, y, num, max
        
	num = rand_range(1, 2)
	max = damroll(1, 2)
	while (max > 0) do
		y, x = find_position(py, px)
	        place_monster_one(y, x, test_monster_name(list[num]), 0, FALSE, MSTATUS_FRIEND)
		max = max - 1
	end
end


-- stupid fct demonstrationg a simple object creation
function create_obj()
        -- k for quantities
	local dd = get_quantity("What dd you want ?", 200) + rand_int(10)
	local ds = get_quantity("What ds you want ?", 200) + rand_range(10, 20)
        -- Create the object
        local obj = create_object(TV_SWORD, 1);
        -- Set some attributes
        obj.dd = dd;
        obj.ds = ds;
        -- Enchant it up
        apply_magic(obj, 100, 1, 1, 1)
        -- Drop on the floor near the player
        drop_near(obj, -1, py, px)
        -- Free it
        end_object(obj);
end

-- stupid fct that casts a nasty spell
function cast_nasty_spell()
	local ret, dir
        -- Get a direction
        ret, dir = get_aim_dir();
        if (ret == FALSE) then return end
        fire_ball(GF_MANA, dir, 2000, 10)
end

-- uber-ize an object
function uber_object_tester(obj)
	if (obj.tval == TV_HAFTED) or (obj.tval == TV_SWORD) or (obj.tval == TV_POLEARM) or (obj.tval == TV_AXE) then
        	return TRUE
        end
        return FALSE
end

function uber_object()
	local ret, item, obj, o_name
        
        -- Ask for an item
        set_item_tester("uber_object_tester")
	ret, item = get_item(0, "What to uber-ize?", "You have nothing you can uber-ize", bor(USE_INVEN, USE_EQUIP))
        
        if ret == TRUE then
        	-- get the item
        	obj = get_object(item)
                -- modify it
                obj.dd = 255
                obj.ds = 255
                obj.to_d = 1000
                obj.to_h = 1000

		-- get the name
                o_name = object_desc(obj, FALSE, 0);
                msg_print("Your "..o_name.." is hit by a pure wave of uber-ification!")
	end
end

-- Callback for spell failure(not needed)
function shiny_test_magic_fail(chance)
	msg_print("So bad, we had "..chance.." chances to succeed.")
end

-- activate the magic type
function mkey_shiny_test(mkey)
	if (mkey == MKEY_SHINY_TEST) then
        	local sn, ret

		-- Ask for a spell
                ret, sn = select_magic_power(0, shiny_test_magic, MAX_SHINY_TEST_SPELL, "shiny_test_magic_info")
                if (ret == FALSE) then return end
                
                -- Verify mana needs
	     	if (get_magic_power(shiny_test_magic, sn).mana_cost > p_ptr.csp) then msg_print("Not enough mana!") return end

                -- Verify failure(second parameter is optional)
                if (magic_power_sucess(get_magic_power(shiny_test_magic, sn), "shiny_test_magic_fail") == FALSE) then return end

		-- Actually cast the spells
                if (sn == 0) then create_obj() end
                if (sn == 1) then cast_nasty_spell() end
                if (sn == 2) then uber_object() end
                if (sn == 3) then shiny_summon() end

		-- use up some mana
                increase_mana(-get_magic_power(shiny_test_magic, sn).mana_cost)
                
                -- use up some energy
                energy_use = energy_use + 100;
                
                -- Do not execute the next hooks
                return TRUE
        end
        -- Not the right hook, continue
        return FALSE
end

-- Register in the 'm' key hook list
add_hook_script(HOOK_MKEY, "mkey_shiny_test", "mkey_shiny_test_hook")



------------------------------ EXTRA POWERS ---------------------------------


-- Register a new power (the 'U' menu)
TEST_POWER = add_new_power("test_power", "You are test", "You become a test", "You are no longer a test", 1, 3, A_INT, 10)

-- Callback for the new power
function test_power(power)
	if (power == TEST_POWER) then
        	msg_print("You scream: 'ZOGZOG!!!'")
        	return TRUE
        end
        return FALSE
end

-- Register in the hook list
add_hook_script(HOOK_ACTIVATE_POWER, "test_power", "test_power")



--------------------------------- QUEST ---------------------------------------
--MY_QUEST = new_quest("My funny quest");
--quest_desc(MY_QUEST, 0, "");
--my_quest = quest[MY_QUEST + 1]
--my_quest.level = 42
--my_quest.silent = FALSE

function my_quest_init()
	my_quest.status = QUEST_STATUS_TAKEN
end

-- Uncomment the line below to enable the example quest
-- add_hook_script(HOOK_BIRTH_OBJECTS, "my_quest_init", "my_quest_init")

------------------------------ SAVEFILE ---------------------------------
-- function called when a key in the variable part ofthe savefile is read
-- if the key matches what we need, we use it, otehrwise just ignore it
--function savefile_load_test(key, val)
--        if (key == "funny_quest_status") then
--                my_quest.status = val
--        end
--end

-- called when the game is saved, can only save numbers
-- assosiate a key with them to allow the loading code to recognize them
--function savefile_save_test()
--        save_number_key("funny_quest_status", my_quest.status);
--end

-- WARNING, we register 1 slot, we *MUST* then call 1 and only 1 time save_number_key()
-- But the good thing is that if one want to remove this lua script it
-- wont harm savefiles, they will just ignore the extra info and wont resave it
-- next time, cool eh ? :)
--register_savefile(1)
--add_hook_script(HOOK_LOAD_GAME, "savefile_load_test", "load_test")
--add_hook_script(HOOK_SAVE_GAME, "savefile_save_test", "save_test")


--
-- test fct
function chaos(s)
        local i = 1
        local r
        
        r = ""
        while (i <= strlen(s))
        do
        	if (magik(50) == TRUE)
                then r = r .. strupper(strchar(strbyte(s, i)))
                else r = r .. strlower(strchar(strbyte(s, i))) end
                i = i + 1
	end	
        msg_print(r)
end