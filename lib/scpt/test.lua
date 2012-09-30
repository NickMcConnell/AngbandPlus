--
-- This file takes care of providing the Shiny-Test class
-- with .. erm .. powerfull spells
--

------------------------------ MAGICTYPE -- M KEY ---------------------------------

zog_magic = add_magic
{
        ["fail"] =      function(chance)
                        msg_print("So bad, we had "..chance.." chances to succeed.")
                        msg_print("Hooo bad luck the spell backfires !.")
                        take_hit("stupidity", 5)
        end,
        ["spell_list"] =
        {
                ["Zog1"] = {
                        ["desc"] = "dessssc zog1",
                        ["mana"] = 1,
                        ["level"] = 1,
                        ["fail"] = 10,
                        ["spell"] = function()
                                local ret, dir
                                -- Get a direction
                                ret, dir = get_aim_dir();
                                if (ret == FALSE) then return end
                                fire_ball(GF_MANA, dir, 2000, 10)
                        end,
                        ["info"] = function()
                                return " dam 2000"
                        end,
                },
                ["Zog2"] = {
                        ["desc"] = "dessssc zog2",
                        ["mana"] = 3,
                        ["level"] = 3,
                        ["fail"] = 30,
                        ["spell"] = function()
                                local ret, item, obj, o_name
        
                                -- Ask for an item
                                ret, item = get_item("What to uber-ize?",
                                		     "You have nothing you can uber-ize",
                                                     bor(USE_INVEN, USE_EQUIP),
                                		     function (obj)
							if (obj.tval == TV_HAFTED) or (obj.tval == TV_SWORD) or (obj.tval == TV_POLEARM) or (obj.tval == TV_AXE) then
        							return TRUE
        						end
        						return FALSE
						     end
				)
        
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
                        end,
                        ["info"] = function()
                                return " cooool"
                        end,
                },
                ["Zog3"] = {
                        ["desc"] = "dessssc zog3",
                        ["mana"] = 4,
                        ["level"] = 5,
                        ["fail"] = 50,
                        ["spell"] = function()
                                local list = {[1] = "Novice Warrior", [2] = "Novice Mage"}
                                local x, y, num, max
        
                                num = rand_range(1, 2)
                                max = damroll(1, 2)
                                while (max > 0) do
                                        y, x = find_position(py, px)
                                        place_monster_one(y, x, test_monster_name(list[num]), 0, FALSE, MSTATUS_FRIEND)
                                        max = max - 1
                                end
                        end,
                        ["info"] = function()
                                return " summons 1d2 monsters"
                        end,
                },
        },
}

-- Register a new magic type
MKEY_SHINY_TEST = 1000
add_mkey
{
        ["mkey"] =      MKEY_SHINY_TEST,
        ["fct"] =       function()
                execute_magic(zog_magic)

                -- use up some energy
                energy_use = energy_use + 100;
        end
}


------------------------------ EXTRA POWERS ---------------------------------


-- Register a new power (the 'U' menu)
TEST_POWER = add_power
{
        ["name"] =      "test_power",
        ["desc"] =      "You are test",
        ["desc_get"] =  "You become a test",
        ["desc_lose"] = "You are no longer a test",
        ["level"] =     1,
        ["cost"] =      3,
        ["stat"] =      A_INT,
        ["fail"] =      10,
        ["power"] =     function()
                        msg_print("You scream: 'ZOGZOG!!!'")
        end
}


--------------------------------- QUEST ---------------------------------------

-- --[[ ... ]] is an other comment type, it runs over multiple lines
-- if the quest is commented it is to not annoy people that arent testing :)
-- It isnt a real quet, only a bare example

--[[
add_quest
{
        -- Need an identifier
        ["global"] =    "MY_QUEST",
        -- Need a name too :)
        ["name"] =      "My funny quest",
        -- Description, can be up to 10 lines
        ["desc"] =      {
                        "My funny quest desc",
                        "Foooo ! Barrrrrzzzz !",
        },
        -- The danger level
        ["level"] =     42,
        -- Some variables that need to be added to the savefile
        -- No dont worry it cannot break the savefile compatibility
        ["data"] =      {
                        ["test_var"] = 0,
                        ["test_var2"] = 1,
        },
        -- List of hooks to do whatever you want :)
        ["hooks"] =     {
                        [HOOK_BIRTH_OBJECTS] = function()
                                quest(MY_QUEST).status = QUEST_STATUS_TAKEN
                        end,
                        [HOOK_NEW_LEVEL] = function()
                                test_var = 0
                        end,
                        [HOOK_LEVEL_REGEN] = function()
                                test_var = 0
                        end,
                        [HOOK_BUILD_ROOM1] = function(by0, bx0)
                                if test_var == 0 then
                                        local x, y, y2, x2, ret

                                        y, x = get_map_size("qrand6.map")
                                        ret, y, x, y2, x2 = alloc_room(by0, bx0, y, x)
                                        if ret == TRUE then
                                                y, x, y2, x2 = load_map("qrand6.map", y, x)
                                        end
                                        test_var = 1
                                end
                        end,
        },
}
]]
