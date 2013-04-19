-- New scrolls
function sterilize_scroll(tval, sval)
	if tval == 70 and sval == 54 then
		msg_print("A neutralising wave radiates from you!")
		set_no_breeders(randint(100) + 100)
		return TRUE
	end
end

add_hook_script(HOOK_READ, "sterilize_scroll", "sterilize_scroll")


-- Neil's automagic statgain script

player.last_rewarded_level = 1
add_loadsave("player.last_rewarded_level", 1)

add_hooks
	{
	[HOOK_PLAYER_LEVEL] = function()
		while player.last_rewarded_level * 5 <= player.lev do
			do_inc_stat(A_STR)
			do_inc_stat(A_INT)
			do_inc_stat(A_WIS)
			do_inc_stat(A_DEX)
			do_inc_stat(A_CON)
			do_inc_stat(A_CHR)
			player.last_rewarded_level = player.last_rewarded_level + 1
			end
		end,
	}

-- silly function that allows a drunk to take a bottle of wine/ale from the player

function drunk_takes_wine(m_idx, item)

	m_ptr = monster(m_idx)
	o_ptr = get_object(item)

	if (m_ptr.r_idx == test_monster_name("Singing, happy drunk")) 
			and (o_ptr.tval == TV_FOOD) and ((o_ptr.sval == 38) or (o_ptr.sval == 39)) then

		cmsg_print(TERM_YELLOW, "'Hic!'")

		inven_item_increase(item, -1)
		inven_item_optimize(item)

-- HackSmurf: the drunk may drop an empty bottle
		bottle = create_object(TV_BOTTLE,1)
		drop_near(bottle, 50, player.py, player.px)
		return TRUE
	else
		return FALSE
	end
end

add_hook_script(HOOK_GIVE, "drunk_takes_wine", "drunk_takes_wine")

-- winged races are allowed soft armor only, no cloaks (from T-Plus)
function __hook_wings_wear(obj) 
    local str = get_race_name() 
    local type = obj.tval
         if (str == "Dragon" or str == "Eagle") and (type == 37) then 
            return TRUE, -1 
         end        
end 

add_hook_script(HOOK_WIELD_SLOT, "__hook_wings_wear", "__hook_wings_wear") 





-- A not-too-scummy way of generating junk for ammo
function food_vessel(object)
   if ((object.tval == 80) and (object.sval == 43)) or
	((object.tval == 80) and (object.sval == 44)) then
      local obj = create_object(TV_JUNK, 3)
	obj.ident = bor(obj.ident, IDENT_MENTAL, IDENT_KNOWN)
	inven_carry(obj, FALSE)
	end_object(obj)
	return FALSE
   end
end

add_hook_script(HOOK_EAT, "food_vessel", "food_vessel")

-- Longbottom Leaf *is* a great stress reliever:
function longbottom_leaf(object)
	if (object.tval == 80) and (object.sval == 45) then
		msg_print("What a stress reliever!")
		heal_insanity(1000)
		return FALSE
	end
end
add_hook_script(HOOK_EAT, "longbottom_leaf", "longbottom_leaf")

function miruvor(object)
if (object.tval == 80) and (object.sval == 43) then
cmsg_print(TERM_WHITE, "You are full!")
set_food(PY_FOOD_MAX - 1)
set_oppose_cold(9999)
do_res_stat(A_STR, TRUE)
return FALSE
end
end
add_hook_script(HOOK_EAT, "miruvor", "miruvor")

-- Hobbits like food
function hobbit_food(m_idx, item)

	m_ptr = monster(m_idx)
	o_ptr = get_object(item)

	if (m_ptr.r_idx == test_monster_name("Scruffy-looking hobbit")) 
	and (o_ptr.tval == TV_FOOD) then
		cmsg_print(TERM_YELLOW, "'Yum!'")
		inven_item_increase(item, -1)
		inven_item_optimize(item)
		return TRUE
	else
		return FALSE
	end
end

add_hook_script(HOOK_GIVE, "hobbit_food", "hobbit_food")

-- Smeagol likes rings
function smeagol_ring(m_idx, item)

   m_ptr = monster(m_idx)
   o_ptr = get_object(item)

   if (m_ptr.r_idx == test_monster_name("Smeagol"))
         and (o_ptr.tval == TV_RING) then

      cmsg_print(TERM_YELLOW, "'MY... PRECIOUSSSSS!!!'")

      inven_item_increase(item, -1)
      inven_item_optimize(item)
      return TRUE
   else
      return FALSE
   end
end

add_hook_script(HOOK_GIVE, "smeagol_ring", "smeagol_ring") 

-- functions to check for Map and Key of Thror before proceeding in Erebor
-- Thank you, Massimiliano Marangio :-)
add_hooks
{
   [HOOK_STAIR] = function(direction)
      if ((current_dungeon_idx == 20) and (dun_level == 60) and (direction == "down")) then
            local i
            local mapkey = 0
            for i = 0, INVEN_TOTAL - 1 do
               if ((player.inventory(i).name1 == 209) or (player.inventory(i).name1 == 210)) then
                  mapkey = mapkey + 1
               end
            end

            if (mapkey == 2) then
                msg_print("The moon-letters on the map show you the keyhole! You use the key to enter.")
                return FALSE
            else
                msg_print("You have found a door, but you cannot find a way to enter. Ask in Dale, perhaps?")
                return TRUE
            end
      end
      return FALSE
   end,
}

-- function to make the Dale mayor tell you about how to get to Erebor 61
add_building_action
{
	["index"] =     66,
	["action"] =    function()
		msg_print("You will need Thorin's Key and Thrain's Map to get anywhere in Erebor.")
	end
}

function minas_gate()
        if (quest(16).status == QUEST_STATUS_FINISHED) and (player.wilderness_y == 56) and (player.wilderness_x == 60) and (player.wild_mode == FALSE) then
                cave(35,10).feat = 159
        end
end

add_hook_script(HOOK_QUEST_FINISH, "minas_gate", "minas_gate")
add_hook_script(HOOK_WILD_GEN, "minas_gate", "minas_gate")

function minas_jump(direction)
        if (quest(16).status == QUEST_STATUS_FINISHED) and (player.wilderness_y == 56) and (player.wilderness_x == 60) and (player.wild_mode == FALSE) then
                if (player.px == 10) and (player.py == 35) then
                        if (direction == "down") then
                                player.wilderness_x = 3
                                player.wilderness_y = 11
                                player.wild_mode = FALSE
                                player.px = 119
                                player.py = 25
                                player.oldpx = player.px
                                player.oldpy = player.py
                                dun_level = 0
                                player.leaving = TRUE
                                return TRUE
                        end
                end
        end
end

add_hook_script(HOOK_STAIR, "minas_jump", "minas_jump")


-- Function to let spiders go over their webs. Created by TheFalcon for Annals of Ea, with much adaptation from BauMog's
-- push_past script. This is probably somewhat over complex, and could have been merged with the aforementioned script, but
-- (hopefully) it gives greater flexibility not to do so...

function spider_move(y, x, m_idx)
	-- Is the player a spider?
	if (get_race_name() == "Spider") then
		c_ptr = cave(y,x)
		local oldy, oldx
		-- variable to stop getting repeat messages
		local message = 0
		local m_ptr = monster(c_ptr.m_idx)
		-- Is the player's destination a web?
		if (c_ptr.feat == 16) then
			-- Is there a monster there?
			if (c_ptr.m_idx > 0) then
				-- Is it a neutral monster
				if (m_ptr.status >= MSTATUS_NEUTRAL) then
					-- Can the monster pass walls, or is it a spider?
					if (m_ptr.flags2 == RF2_PASS_WALL) or ((m_ptr.r_idx > 362) and (m_ptr.r_idx < 382)) then
						-- Make sure the player hasn't got the message before, then give it them...
						if (message == 0) then
							msg_print(format("You push past %s.", monster_desc(m_ptr, 0)))
							message = 1
						end
						-- Remember the monsters old x and y...
						local oldmonstery = m_ptr.fy
						local oldmonsterx = m_ptr.fx
						-- Place the monster where the player is...
						m_ptr.fy = player.py
						m_ptr.fx = player.px
						-- Tell the game the monster is there, and not where it was...
						cave(player.py, player.px).m_idx = c_ptr.m_idx
						c_ptr.m_idx = 0
						-- Move the player
						player.py = oldmonstery
						player.px = oldmonsterx
						-- Refresh the two coordinates so it displays correctly
						lite_spot(player.py, player.px)
						lite_spot(m_ptr.fy, m_ptr.fx)
						-- Don't get any other messages
						return TRUE
					-- This should not arise (neutral monster, non-wall passing, on a web already), but better 
					-- to be safe...
					else
						-- Don't get the message more than once, then give it them...
						if (message == 0) then
							msg_print(format("%s is in your way.", monster_desc(m_ptr, 0)))
							message = 1
						end
						-- No other messages...
						return TRUE
					end
				end
			-- No monster in the way...
			elseif (c_ptr.m_idx == 0) then
				-- Don't get the message more than once...
				if (message == 0) then
					msg_print("You move over the web.")
					message = 1
				end
				-- Remember the player's old position...
				oldy = player.py
				oldx = player.px
				-- Move the player...
				player.py = y
				player.px = x
				-- Refresh the two coordinates...
				lite_spot(player.py, player.px)
				lite_spot(oldy, oldx)
				-- No other messages
				return TRUE
			end
		end
	end
end
add_hook_script(HOOK_MOVE, "spider_move", "spider_move")

add_hooks
{
	[HOOK_WIELD_SLOT] =     function (obj, ideal)
			if ((obj.tval == 55) or (obj.tval ==65)) then
				local slot
				
					if(ideal == TRUE) then
						slot = INVEN_ARM
					else
						slot = get_slot(INVEN_ARM)
					end
				
				
				return TRUE, slot
			end
	end,
}

add_hooks
{
	[HOOK_WIELD_SLOT] =     function (obj, ideal)
			if ((obj.tval == 11 and obj.sval == 25)) then
				local slot
				
					if(ideal == TRUE) then
						slot = INVEN_HEAD
					else
						slot = get_slot(INVEN_HEAD)
					end
				
				
				return TRUE, slot
			end
	end,
}

add_hooks
{
	[HOOK_WIELD_SLOT] =     function (obj, ideal)
			if ((obj.tval == 1 and obj.sval == 1)) then
				local slot
				
					if(ideal == TRUE) then
						slot = INVEN_HEAD
					else
						slot = get_slot(INVEN_HEAD)
					end
				
				
				return TRUE, slot
			end
	end,
}

add_hooks
{
	[HOOK_WIELD_SLOT] =     function (obj, ideal)
			if ((obj.tval == 11 and obj.sval == 55 and obj.pval == 0)) then
				local slot
				
					if(ideal == TRUE) then
						slot = INVEN_WIELD
					else
						slot = get_slot(INVEN_WIELD)
					end
				
				
				return TRUE, slot
			end
	end,
}

add_hooks
{
	[HOOK_WIELD_SLOT] =     function (obj, ideal)
			if ((obj.tval == 5 and obj.sval == 0 and obj.pval == 0)) then
				local slot
				
					if(ideal == TRUE) then
						slot = INVEN_WIELD
					else
						slot = get_slot(INVEN_WIELD)
					end
				
				
				return TRUE, slot
			end
	end,
}

function __jedi_saber(obj) 
    local str = get_class_name() 
    local type4 = obj.tval
    local type2 = obj.sval
         if (str ~= "Jedi") and (type4 == 23 and type2 == 55 ) then 
	 return TRUE, -1 
      end        
end 
add_hook_script(HOOK_WIELD_SLOT, "__jedi_saber", "__jedi_saber") 