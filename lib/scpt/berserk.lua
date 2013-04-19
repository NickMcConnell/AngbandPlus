-- Berserker powers, restrictions and bonuses
-- $Id: berserk.lua,v 1.3 2003/06/14 17:55:27 vrak Exp $
-- Berserker stuff
add_hooks
{
        -- Add the bonuses
        [HOOK_CALC_BONUS] = function()
            if get_class_name() == "Berserker" then
                -- AC bonus
                player.to_a = player.to_a + 10 + (player.lev / 2)
                player.dis_to_a = player.dis_to_a + 10 + (player.lev / 2)
                -- Digging bonus
                player.skill_dig = player.skill_dig + 100 + (player.lev * 8)
                -- ToHit bonus
                player.to_h = player.to_h + (player.lev / 5)
                player.dis_to_h = player.dis_to_h + (player.lev / 5)
                -- ToDam bonus
                player.to_d = player.to_d + (player.lev / 6)
                player.dis_to_d = player.dis_to_d + (player.lev / 6 )
                -- We don't get stunned after clev 35
                if player.lev >= 35 then set_stun(0) end
            end
        end,
        
        -- Then make sure we can't use magic >:)
        [HOOK_GAME_START] = function()
            if get_class_name() == "Berserker" then
                cast_school_spell = function()
                    msg_print("You cannot use magic!")
                end
            end
        end,
        [HOOK_KEYPRESS] = function(key)
            if get_class_name() == "Berserker" then
                if key == strbyte('a') then
                    msg_print('You cannot use wands.')
                    return TRUE
                		    -- no reading scrolls key,they lack literacy
                elseif key == strbyte('u') then
                    msg_print('You cannot use staffs.')
                    return TRUE
                elseif key == strbyte('z') then
                    msg_print('You cannot use rods.')
                    return TRUE
		    elseif key == strbyte('b') then
                    msg_print('You cannot browse spellbooks.')
                    return TRUE
		  elseif key == strbyte('r') then
                    msg_print('You cannot read.')
                    return TRUE
                end
            end
        end,

        -- And finally, let's be nice and give 'em uncurse by strenght alone :)
        [HOOK_TAKEOFF] = function(o_idx)
            local obj = get_object(o_idx)
            if get_class_name() == "Berserker" and band(obj.ident, IDENT_CURSED) ~= FALSE then
			local berserk_takeoff = player.stat_ind[A_STR+1] + player.lev + 5
         if rand_range(1,100) < berserk_takeoff then
             if remove_curse_object(obj, FALSE) == TRUE then
                       msg_print("You break the curse.")
				    take_hit(obj.weight/5 + 10," breaking a curse")
                    else
                           msg_print("You fail to break the curse.")
				
                        end
                else
                    msg_print("You fail to break the curse.")
                end

            end
        end,
}

-- Recall power
POWER_RECALL = add_power
{
    ["name"] =          "Berserker's Recall",
    ["desc"] =          "You are able to recall to and from dungeons by will.",
    ["desc_get"] =      "You become able to recall.",
    ["desc_lose"] =     "You loose your ability to recall.",
    ["level"] =         10,
    ["cost"] =          10,
    ["stat"] =          A_CON,
    ["fail"] =          10,
    ["power"] =         function()
                        recall_player(500 / player.lev , 10)
    end,
}
