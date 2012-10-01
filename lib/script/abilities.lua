-- File: abilities.lua
-- This file contains the code needed when you use an active power.
-- All combat feats and other "U" abilities should be defined here.

-- The main function that is called when using an activated ability.
-- Every abilities have their code id.

-- Some useful constants:

-- ELEMENTS:
-- GF_FIRE: Fire
-- GF_COLD: Cold
-- GF_ELEC: Electricity
-- GF_ACID: Acid
-- GF_POIS: Poison
-- GF_LITE: Light
-- GF_DARK: Darkness
-- GF_WARP: Warp
-- GF_WATER: Water
-- GF_WIND: Wind
-- GF_EARTH: Earth
-- GF_SOUND: Sound
-- GF_RADIO: Radioactivity
-- GF_CHAOS: Chaos
-- GF_PHYSICAL: Physical
-- GF_MANA: Mana
-- GF_FROSTFIRE: FrostFire
-- GF_GREY: Grey
-- GF_TOXIC: Toxic

function use_ability (powernum)

  -- Spin Attack
  if (powernum == 1) then

        local dam

	-- If used trough combat feats, we already selected a weapon.
	if (not(combatfeat)) then choose_current_weapon() end

        if (current_weapon.tval == 0) then

                msg_print("You must use a weapon!")
                return
        end
        
	-- Damages.
	dam = weapon_damages()
        dam = dam + ((dam * ((p_ptr.abilities[(CLASS_WARRIOR * 10) + 1]) * 20)) / 100)
        if (dam < 0) then dam = 0 end

	-- The function "attack_aura" projects an attack around the player.
	-- Here, we project Physical over a radius of 1.
	attack_aura(GF_PHYSICAL, dam, 1)

	-- The attack takes a turn.
	-- Every 100 pts of "energy_use", an extra turn is taken.
	energy_use = 100
  end

  -- Accurate Strike
  if (powernum == 2) then

	local dam
	local dir

	if (not(combatfeat)) then choose_current_weapon() end

	if (current_weapon.tval == 0) then

                msg_print("You must use a weapon!")
                return
        end

	-- We need to choose a direction to attack. If used as combat feat,
	-- this won't be asked.
	dir = lua_get_rep_dir()

	dam = weapon_damages()
        dam = dam + ((dam * ((p_ptr.abilities[(CLASS_WARRIOR * 10) + 5]) * 10)) / 100)

	-- We use a radius 0, range 1 chain attack.
	-- The "nevermiss" global variable is set to true. This way, the attack
	-- will never miss. Once the attack is executed, the variable is set to
	-- false again.
	nevermiss = TRUE
    	chain_attack(dir, GF_PHYSICAL, dam, 0, 1)
	nevermiss = FALSE

    	energy_use = 100;
  end

  -- War Cry
  if (powernum == 3) then

	local duration
        duration = p_ptr.abilities[(CLASS_WARRIOR * 10) + 7] + 4
        attack_aura(GF_WARCRY, duration, 3)
        update_and_handle()
	energy_use = 100
  end

  -- Leaping Spin
  if (powernum == 4) then

        local dam
	local x
	local y

	-- If used trough combat feats, we already selected a weapon.
	if (not(combatfeat)) then choose_current_weapon() end

        if (current_weapon.tval == 0) then

                msg_print("You must use a weapon!")
                return
        end
        
	-- Damages.
	dam = weapon_damages()
        dam = dam + ((dam * ((p_ptr.abilities[(CLASS_WARRIOR * 10) + 9]) * 10)) / 100)
        if (dam < 0) then dam = 0 end

	-- Actually jump!
	-- We use a special function that returns x and y coordinates in global
	-- variables, so we can use these in Lua.
	msg_print("You jump high!")
        if (not(lua_tgt_pt())) then return end
	x = global_x
	y = global_y

	-- Most functions here use a (y,x) format, instead of (x,y).
	-- This is important, because if you use (x,y), it might crash.
        if (not(lua_cave_empty_bold(y,x)) or (get_cave_info_flag(y, x, CAVE_ICKY)) or (distance(y,x,py,px) > (3 + (p_ptr.abilities[(CLASS_WARRIOR * 10) + 9] / 10)))) then

              msg_print("You can't jump there...")

        else

                if (not(get_cave_info_flag(y, x, CAVE_MARK))) then

                        if (get_cave_info_flag(y, x, CAVE_LITE)) then
				teleport_player_to(ij,ii)
                        else
				msg_print("You can't jump there...")
			end

                else
			-- Move the player, then use attacl_aura to attack everything
			-- around the player!
			teleport_player_to(y,x)
			attack_aura(GF_PHYSICAL, dam, 1)
		end
        end

	-- The attack will take a turn no matter what. So calculate your jump!
	energy_use = 100
  end

  -- Taunt
  -- Elements are mostly hard-coded right now, so the effect of taunt can't really
  -- be fully customized yet.
  if (powernum == 5) then

	attack_aura(GF_TAUNT, 1, 5)
        update_and_handle()
	energy_use = 100
  end

  -- Throw
  if (powernum == 6) then

	local dir
	local dir_x
	local dir_y
	local tgt_x
	local tgt_y
	local dam
	local rad

	-- Get the direction.
	dir = lua_get_rep_dir()

	-- Determine the coordinates where we will look for a monster.
	if (dir == 1) then
		dir_x = px - 1
		dir_y = py + 1
	end
	if (dir == 2) then
		dir_x = px
		dir_y = py + 1
	end
	if (dir == 3) then
		dir_x = px + 1
		dir_y = py + 1
	end
	if (dir == 4) then
		dir_x = px - 1
		dir_y = py
	end
	if (dir == 5) then
		dir_x = px
		dir_y = py
	end
	if (dir == 6) then
		dir_x = px + 1
		dir_y = py
	end
	if (dir == 7) then
		dir_x = px - 1
		dir_y = py - 1
	end
	if (dir == 8) then
		dir_x = px
		dir_y = py - 1
	end
	if (dir == 9) then
		dir_x = px + 1
		dir_y = py - 1
	end

	-- Damages
	dam = monk_damages()
	dam = dam + ((dam * ((p_ptr.abilities[(CLASS_FIGHTER * 10) + 8]) * 10)) / 100)

	-- The radius of the throw.
	rad = 3 + (p_ptr.abilities[(CLASS_FIGHTER * 10) + 8] / 20)

	-- Actually throw the monster

	-- First, we must check if there's a monster on the grid we're targetting.
	if (cave(dir_y, dir_x).m_idx == 0) then
	
		msg_print("No monster here.")
		-- Does take a turn.
		energy_use = 100
		return
	end

	-- Can we lift the monster?
	if (((p_ptr.abilities[(CLASS_FIGHTER * 10) + 8] * 700)+700) >= m_race(monster(cave(dir_y, dir_x).m_idx).r_idx).weight) then

		-- Then, we must successfully grab the monster.
		-- It's a regular player_hit_monster test.
		if (player_hit_monster(monster(cave(dir_y, dir_x).m_idx), 0)) then

			-- We grab the monster. Throw it!
			msg_print("You grab the monster!")
			lua_tgt_pt()
			tgt_x = global_x
			tgt_y = global_y
			if((lua_cave_empty_bold(tgt_y,tgt_x)) and (distance(tgt_y,tgt_x,py,px) <= rad)) then

				-- This ones moves the monster, and uses (x,y) as the coordinates, not (y,x).
				move_monster_spot(cave(dir_y, dir_x).m_idx, tgt_x, tgt_y)
				update_and_handle()
				msg_print("You throw the monster!")
				nevermiss = TRUE
				fire_ball_specific_grid(dam, tgt_x, tgt_y, 0, GF_PHYSICAL)
				nevermiss = FALSE
			else
				-- If we point an invalid location, we throw it on the ground!
				-- So we still do damages.
				msg_print("You can't throw the monster there!")
				msg_print("You throw it on the ground!")
				fire_ball_specific_grid(dam, dir_x, dir_y, 0, GF_PHYSICAL)
			end

		else
			msg_print("You miss the monster.")
		end

	else
		msg_print("This monster is too heavy!")
	end

	-- It takes a turn.
	energy_use = 100

  end

  -- Extreme Strike
  if (powernum == 7) then

	local dam
	local dir

	if (p_ptr.powerattack <= 0) then

		msg_print("This can only be used while in Power Attack mode.")
		return
	end

	if (not(combatfeat)) then choose_current_weapon() end

	-- If we don't use a weapon, for this ability, we can use unarmed damages.
	if (current_weapon.tval == 0) then

                dam = monk_damages()
		dam = dam * p_ptr.abilities[(CLASS_FIGHTER * 10) + 9]
        else
		dam = weapon_damages()
		dam = dam * p_ptr.abilities[(CLASS_FIGHTER * 10) + 9]
	end
	
	-- Get direction.
	dir = lua_get_rep_dir()

	-- We use the hard_kick function. It does damages, and sends the enemy away.
	-- As you probably guessed, it was first made for the Hard Kick ability, but can
	-- be used anywhere. A real time saver! :) Otherwise, you're in for quite a few
	-- lines of code. ;)
	-- hard_kick(dir, dam, distance the enemy will be moved by)
	hard_kick(dir, dam, 5 + (p_ptr.abilities[(CLASS_FIGHTER * 10) + 9] / 5))

    	energy_use = 100;
  end

  -- Force Field
  if (powernum == 8) then
	p_ptr.pres = 25 + (p_ptr.abilities[(CLASS_MAGE * 10) + 2] / 5)
        p_ptr.mres = 25 + (p_ptr.abilities[(CLASS_MAGE * 10) + 2] / 5)
        set_pres((5 + (p_ptr.abilities[(CLASS_MAGE * 10) + 2] * 2)))
        set_mres((5 + (p_ptr.abilities[(CLASS_MAGE * 10) + 2] * 2)))
        energy_use = 100
  end

  -- Magic Missile
  if (powernum == 9) then
	local dam
	local dir
	local spellstat

	-- This determines casting power
	-- Remember! In Lua, you must use the stat's constant +1, hence A_INT+1.
	spellstat = (p_ptr.stat_ind[A_INT+1] - 5)

	-- No lower than 0.
	if (spellstat < 0) then spellstat = 0 end

        dam = p_ptr.abilities[(CLASS_MAGE * 10) + 3] * 10
	dam = dam * spellstat

	-- This is different from lua_get_rep_dir(). This one allow you to set
	-- the monster as a target.
	dir = lua_get_aim_dir()        

	-- Use a bolt spell against an ememy.
        fire_bolt(GF_MISSILE, dir, dam)

        update_and_handle()
        energy_use = 100
  end

  -- Slow Down
  if (powernum == 10) then
	local dir
	dir = lua_get_aim_dir()
        fire_ball(GF_SLOW_DOWN, dir, 0, (p_ptr.abilities[(CLASS_MAGE * 10) + 5] / 20))
        energy_use = 100
  end

  -- Mirror Images
  if (powernum == 11) then
	local i
        msg_print("You create illusions of yourself!")

	for i = 1, ((p_ptr.abilities[(CLASS_MAGE * 10) + 6] / 5) + 3) do

		-- Summon a specific monster. In this case, the Mirror Image is 1077. Could be anything.
		-- y1 and x1 are just the "center", and monsters will randomly appear around this
		-- point. Syntax for the command:
		-- summon_specific_ridx(int y1, int x1, int ridx, bool Group_ok, bool friendly, int dur)
		-- Group_ok is used in case the monster has the FRIENDS flag, and this can appear in groups.
		-- If false, the monster will appear alone, ignoring the FRIENDS flag.

		summon_specific_ridx(py, px, 1077, FALSE, TRUE, (p_ptr.abilities[(CLASS_MAGE * 10) + 6] + 10))
	end

        energy_use = 100
  end

  -- Damages Curse
  if (powernum == 12) then
	local dir
	dir = lua_get_aim_dir()
        fire_ball(GF_DAMAGES_CURSE, dir, 0, (p_ptr.abilities[(CLASS_MAGE * 10) + 7] / 20))
        energy_use = 100
  end

  -- Heal
  if (powernum == 16) then

	local spellstat

	-- This determines casting power
	spellstat = (p_ptr.stat_ind[A_WIS+1] - 5)

	-- No lower than 0.
	if (spellstat < 0) then spellstat = 0 end
	msg_print("You heal yourself!")
        p_ptr.chp = p_ptr.chp + ((10 + (p_ptr.abilities[(CLASS_PRIEST * 10) + 1] / 2)) * spellstat)
        if (p_ptr.chp > p_ptr.mhp) then p_ptr.chp = p_ptr.mhp end
        if (p_ptr.abilities[(CLASS_PRIEST * 10) + 1] >= 15) then

		-- These two functions cures bleeding and poison.
		-- Bleeding has not been reimplemented yet since NewAngband 1.7.1, not sure it will be.
		-- But for now, the function will be called anyway.
        	set_cut(0)
        	set_poisoned(0)
        end
        update_and_handle()
        energy_use = 100
  end

end

add_event_handler("use_ability", use_ability)
