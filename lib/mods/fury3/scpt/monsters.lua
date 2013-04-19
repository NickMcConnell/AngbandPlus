-- This file holds various things that govern monster behaviour with respect to the player
-- Enables player to push past any monster who is >= MSTATUS_NEUTRAL.
-- Originally written by BauMog for the Intets Hevn module, now adapted for Annals of Ea, with certain extra precautions, 
-- and more commenting, and arranged to complement a script to allow (player race) spiders to move over their webs, 
-- (see "p_race.lua").

-- Adapted from defines.h, makes sure the move is not onto a trap or into a wall.
function cave_floor_bold(y, x)
	local c_ptr = cave(y, x)
	-- Is it a floor, and not a trap?
	if(cave_is(c_ptr, FF1_FLOOR) == TRUE) and (c_ptr.feat ~= FEAT_MON_TRAP) then
		-- It's safe, so...
		return TRUE
	else
		-- It's not safe, so...
		return FALSE
	end
end

-- Adapted from cmd1.c
function push_past(y, x, r_idx)
	local c_ptr = cave(y, x)
	-- Is there a monster there?
	if(c_ptr.m_idx > 0) then
		m_ptr = monster(c_ptr.m_idx)
		-- Is it neutral?
		if(m_ptr.status >= MSTATUS_NEUTRAL) then
			-- Are the monster's coordinates safe?
			if (cave_floor_bold(y, x) == TRUE) then
				-- Are the player's coordinates safe, or can the monster pass through walls, or is the player on a web, and the monster a spider?
				if (cave_is(cave(player.py, player.px), FF1_FLOOR) == TRUE) or (m_ptr.flags2 == RF2_PASS_WALL) or ((cave(player.py,player.px).feat == 16) and ((m_ptr.r_idx > 362) and (m_ptr.r_idx < 382))) then
					msg_print(format("You push past %s.", monster_desc(m_ptr, 0)))
					-- Move the monster...
					m_ptr.fy = player.py
					m_ptr.fx = player.px
					-- Tell the game there's a monster there...
					cave(player.py, player.px).m_idx = c_ptr.m_idx
					-- and no longer one where it used to be...
					c_ptr.m_idx = 0;
				else
					msg_print(format("%s is in your way!", monster_desc(m_ptr, 0)))
					-- Don't take up the player's turn
					energy_use = 0
					-- return true to avoid getting more than one message
					return TRUE
				end
			else
				msg_print(format("%s is in your way!", monster_desc(m_ptr, 0)))
				-- Don't take up the player's turn
				energy_use = 0
				-- return TRUE to avoid getting more than one message
				return TRUE
			end
		end
	end
end

add_hook_script(HOOK_MOVE, "push_past", "push_past");


-- Monster vs. Player Race alignment script
-- From T-Plus by Ingeborg S. Norden

monst_al = {} 

function monst_al_add(status, mrs, prs) 
for i,v in mrs do 
-- added end 
if not monst_al[v] then monst_al[v] = {} end 
for j, w in prs do 
monst_al[v][w] = status 
end 
end 
end 

function monst_al_get(mr,pr) 
 if monst_al[mr] then return monst_al[mr][pr] 
 else return end 
end 

-- Maia aggravation for evil beings (provided that no demonic corruptions are present)
-- Based on parts of angel.lua from T-Plus by Ingeborg S. Norden

-- cast dispel evil with 0 damage every 10 turns 

TIMER_AGGRAVATE_EVIL = new_timer 
{ 
   ["enabled"] = FALSE, 
   ["delay"] = 10, 
   ["callback"] = function() 
      dispel_evil(0) 
   end, 
} 

add_hooks{ 
[HOOK_GAME_START] = function() 

  	if ((get_race_name() == "Maia") and 
	(player.corruption(CORRUPT_BALROG_AURA) ~= TRUE) and 
	(player.corruption(CORRUPT_BALROG_WINGS) ~= TRUE) and 
	(player.corruption(CORRUPT_BALROG_STRENGTH) ~= TRUE) and 
	(player.corruption(CORRUPT_BALROG_FORM) ~= TRUE)) then 
	-- "Proper" Maiar aggravate evil beings
    	TIMER_AGGRAVATE_EVIL.enabled = TRUE

	end
end,

[HOOK_LEVEL_END_GEN] = function() 

for i=0,m_max-1 do
	local monst = monster(i)
	local s = monst_al_get(monst.r_idx, player.prace)
	if s then monst.status = s end
end	

end, 

[HOOK_NEW_MONSTER] = function() 

for i=0,m_max-1 do
	local monst = monster(i)
	local s = monst_al_get(monst.r_idx, player.prace)
	if s then monst.status = s end
end	

end, 

}