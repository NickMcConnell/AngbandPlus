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

-- A function to check is an item is "divinable"
function divinable (item)

	if (item.tval == TV_WEAPON or item.tval == TV_RANGED or item.tval == TV_SOFT_ARMOR or item.tval == TV_HARD_ARMOR
	or item.tval == TV_DRAG_ARMOR or item.tval == TV_GLOVES or item.tval == TV_HELM or item.tval == TV_BOOTS or item.tval == TV_ARM_BAND
	or item.tval == TV_CLOAK or item.tval == TV_CROWN or item.tval == TV_ROD or item.tval == TV_AMMO or item.tval == TV_BOOMERANG
	or item.tval == TV_RING or item.tval == TV_AMULET or item.tval == TV_SHIELD or item.tval == TV_INSTRUMENT) then return TRUE end

	return FALSE

end

function use_ability (powernum)

  -- Spin Attack
  if (powernum == 1) then

        local dam
	local dtype

	-- If used trough combat feats, we already selected a weapon.
	if (not(combatfeat)) then choose_current_weapon() end

        if (current_weapon.tval == 0) then

                msg_print("You must use a weapon!")
                return
        end
        
	-- Damages.
	dam = weapon_damages()
	dam = dam + multiply_divide(dam, ((p_ptr.abilities[(CLASS_WARRIOR * 10) + 1]) * 20), 100)
        if (dam < 0) then dam = 0 end

	dtype = current_weapon.extra1
	if (dtype == 0) then dtype = GF_PHYSICAL end

	-- The function "attack_aura" projects an attack around the player.
	-- Here, we project Physical over a radius of 1.
	melee_attack = TRUE
	no_magic_return = TRUE
	attack_aura(dtype, dam, 1)
	no_magic_return = FALSE
	melee_attack = FALSE

	-- The attack takes a turn.
	-- Every 100 pts of "energy_use", an extra turn is taken.
	energy_use = 100
  end

  -- Accurate Strike
  if (powernum == 2) then

	local dam
	local dir
	local dtype

	if (not(combatfeat)) then choose_current_weapon() end

	if (current_weapon.tval == 0) then

                msg_print("You must use a weapon!")
                return
        end

	-- We need to choose a direction to attack. If used as combat feat,
	-- this won't be asked.
	dir = lua_get_rep_dir()

	dam = weapon_damages()
	dam = dam + multiply_divide(dam, ((p_ptr.abilities[(CLASS_WARRIOR * 10) + 5]) * 10), 100)

	dtype = current_weapon.extra1
	if (dtype == 0) then dtype = GF_PHYSICAL end

	-- We use a radius 0, range 1 chain attack.
	-- The "nevermiss" global variable is set to true. This way, the attack
	-- will never miss. Once the attack is executed, the variable is set to
	-- false again.
	if (not(combatfeat)) then nevermiss = TRUE
	else accuratestrike = 1 end

	melee_attack = TRUE
	no_magic_return = TRUE
    	chain_attack(dir, dtype, dam, 0, 1)
	no_magic_return = FALSE
	melee_attack = FALSE
	nevermiss = FALSE
	accuratestrike = 0

    	energy_use = 100;
  end

  -- War Cry
  if (powernum == 3) then

	local duration
	local rad
        duration = p_ptr.abilities[(CLASS_WARRIOR * 10) + 7] + 4
	rad = 5 + (p_ptr.abilities[(CLASS_WARRIOR * 10) + 7] / 10)
	nevermiss = TRUE
	no_magic_return = TRUE
        attack_aura(GF_WARCRY, duration, rad)
	no_magic_return = FALSE
	nevermiss = FALSE
        update_and_handle()
	energy_use = 100
  end

  -- Leaping Spin
  if (powernum == 4) then

        local dam
	local x
	local y
	local dtype

	-- If used trough combat feats, we already selected a weapon.
	if (not(combatfeat)) then choose_current_weapon() end

        if (current_weapon.tval == 0) then

                msg_print("You must use a weapon!")
                return
        end
        
	-- Damages.
	dam = weapon_damages()
	dam = dam + multiply_divide(dam, ((p_ptr.abilities[(CLASS_WARRIOR * 10) + 9]) * 20), 100)
        if (dam < 0) then dam = 0 end

	dtype = current_weapon.extra1
	if (dtype == 0) then dtype = GF_PHYSICAL end

	-- Actually jump!
	-- We use a special function that returns x and y coordinates in global
	-- variables, so we can use these in Lua.
	msg_print("You jump high!")
        if (not(lua_tgt_pt())) then return end
	x = global_x
	y = global_y

	-- Most functions here use a (y,x) format, instead of (x,y).
	-- This is important, because if you use (x,y), it might crash.
        if (not(lua_cave_empty_bold(y,x)) or (distance(y,x,py,px) > (3 + (p_ptr.abilities[(CLASS_WARRIOR * 10) + 9] / 10)))) then

              msg_print("You can't jump there...")

        else

                if (not(get_cave_info_flag(y, x, CAVE_MARK))) then

                        if (get_cave_info_flag(y, x, CAVE_LITE)) then
				-- Move the player, then use attack_aura to attack everything
				-- around the player!
				teleport_player_to(y,x)
				melee_attack = TRUE
				no_magic_return = TRUE
				attack_aura(dtype, dam, 1)
				no_magic_return = FALSE
				melee_attack = FALSE
                        else
				msg_print("You can't jump there...")
			end

                else
			-- Move the player, then use attack_aura to attack everything
			-- around the player!
			teleport_player_to(y,x)
			melee_attack = TRUE
			no_magic_return = TRUE
			attack_aura(dtype, dam, 1)
			no_magic_return = FALSE
			melee_attack = FALSE
		end
        end

	-- The attack will take a turn no matter what. So calculate your jump!
	energy_use = 100
  end

  -- Taunt
  if (powernum == 5) then

	local rad

	rad = 5 + (p_ptr.abilities[(CLASS_FIGHTER * 10) + 3] / 5)

	nevermiss = TRUE
	no_magic_return = TRUE
	attack_aura(GF_TAUNT, 100, rad)
	no_magic_return = FALSE
	nevermiss = FALSE
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
	dam = dam + multiply_divide(dam, ((p_ptr.abilities[(CLASS_FIGHTER * 10) + 8]) * 20), 100)

	-- The radius of the throw.
	rad = 3 + (p_ptr.abilities[(CLASS_FIGHTER * 10) + 8] / 10)

	-- Actually throw the monster

	-- First, we must check if there's a monster on the grid we're targetting.
	if (cave(dir_y, dir_x).m_idx == 0) then
	
		msg_print("No monster here.")
		-- Does take a turn.
		energy_use = 100
		return
	end

	-- Can we lift the monster?
	if (((p_ptr.abilities[(CLASS_FIGHTER * 10) + 8] * 2000)+2000) >= m_race(monster(cave(dir_y, dir_x).m_idx).r_idx).weight) then

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
	melee_attack = TRUE
	no_magic_return = TRUE
	hard_kick(dir, dam, 5 + (p_ptr.abilities[(CLASS_FIGHTER * 10) + 9] / 5))
	no_magic_return = FALSE
	melee_attack = FALSE

    	energy_use = 100;
  end

  -- Force Field
  if (powernum == 8) then
	p_ptr.pres = 25 + (p_ptr.abilities[(CLASS_MAGE * 10) + 2])
        p_ptr.mres = 25 + (p_ptr.abilities[(CLASS_MAGE * 10) + 2])
	if (p_ptr.pres > 75) then p_ptr.pres = 75 end
	if (p_ptr.mres > 75) then p_ptr.mres = 75 end
        set_pres((10 + (p_ptr.abilities[(CLASS_MAGE * 10) + 2] * 2)))
        set_mres((10 + (p_ptr.abilities[(CLASS_MAGE * 10) + 2] * 2)))
        energy_use = 100
  end

  -- Magic Missile
  if (powernum == 9) then
	local dam
	local dir
	local spellstat
	local rodbonus
	local rad

	-- This determines casting power
	-- Remember! In Lua, you must use the stat's constant +1, hence A_INT+1.
	spellstat = (p_ptr.stat_ind[A_INT+1] - 5)

	-- No lower than 0.
	if (spellstat < 0) then spellstat = 0 end

	-- Bonus from rods.
	rodbonus = 0
	if (inven(INVEN_WIELD).tval == TV_ROD) then

		rodbonus = rodbonus + damroll(inven(INVEN_WIELD).dd, inven(INVEN_WIELD).ds)
		rodbonus = rodbonus + multiply_divide(rodbonus, p_ptr.skill[18] * 3, 100)
	end
	if (inven(INVEN_WIELD+1).tval == TV_ROD) then

		rodbonus = rodbonus + damroll(inven(INVEN_WIELD+1).dd, inven(INVEN_WIELD+1).ds)
		rodbonus = rodbonus + multiply_divide(rodbonus, p_ptr.skill[18] * 3, 100)
	end

        dam = (p_ptr.abilities[(CLASS_MAGE * 10) + 3] * 15) + rodbonus
	dam = dam * spellstat
	dam = dam + multiply_divide(dam, p_ptr.to_s, 100)

	rad = 0 + (p_ptr.abilities[(CLASS_MAGE * 10) + 3] / 10)

	-- This is different from lua_get_rep_dir(). This one allow you to set
	-- the monster as a target.
	dir = lua_get_aim_dir()

	-- Use a bolt spell against an ememy.
        fire_ball(GF_MISSILE, dir, dam, rad)

        update_and_handle()
        energy_use = 100
  end

  -- Slow Down
  if (powernum == 10) then
	local dir
	dir = lua_get_aim_dir()
        fire_ball(GF_SLOW_DOWN, dir, 100, 2 + (p_ptr.abilities[(CLASS_MAGE * 10) + 5] / 20))
        energy_use = 100
  end

  -- Mirror Images
  if (powernum == 11) then
	local i
        msg_print("You create illusions of yourself!")

	for i = 1, ((p_ptr.abilities[(CLASS_MAGE * 10) + 6]) + 5) do

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
        fire_ball(GF_DAMAGES_CURSE, dir, 100, 2 + (p_ptr.abilities[(CLASS_MAGE * 10) + 7] / 20))
        energy_use = 100
  end

  -- Elemental Lord's Piercing Spells
  if (powernum == 13) then

	local dir
	local rad

	-- This is different from lua_get_rep_dir(). This one allow you to set
	-- the monster as a target.
	dir = lua_get_aim_dir()

	rad = 2 + (p_ptr.abilities[(CLASS_ELEM_LORD * 10) + 5] / 10)

	-- Use a ball spell against an ememy.
	fire_ball(GF_VULNERABILITY, dir, 100, rad)

        update_and_handle()
        energy_use = 100
  end

  -- Turn Undeads
  if (powernum == 15) then

	local dam
	local dir
	local spellstat
	local rodbonus
	local rad

	-- This determines casting power
	spellstat = (p_ptr.stat_ind[A_WIS+1] - 5)

	-- No lower than 0.
	if (spellstat < 0) then spellstat = 0 end

	-- Bonus from rods.
	rodbonus = 0
	if (inven(INVEN_WIELD).tval == TV_ROD) then

		rodbonus = rodbonus + damroll(inven(INVEN_WIELD).dd, inven(INVEN_WIELD).ds)
		rodbonus = rodbonus + multiply_divide(rodbonus, p_ptr.skill[18] * 3, 100)
	end
	if (inven(INVEN_WIELD+1).tval == TV_ROD) then

		rodbonus = rodbonus + damroll(inven(INVEN_WIELD+1).dd, inven(INVEN_WIELD+1).ds)
		rodbonus = rodbonus + multiply_divide(rodbonus, p_ptr.skill[18] * 3, 100)
	end

        dam = (p_ptr.abilities[(CLASS_PRIEST * 10) + 2] * 40) + rodbonus
	dam = dam * spellstat
	dam = dam + multiply_divide(dam, p_ptr.to_s, 100)

	no_magic_return = TRUE
        attack_aura(GF_TURN_UNDEAD, dam, 10)
	no_magic_return = FALSE
        update_and_handle()
        energy_use = 100
  end

  -- Blessed Meditation
  if (powernum == 16) then

	local wisbonus

	wisbonus = 0

	-- We actually set an event variable here.
	-- Represents the times we used it.
	if (p_ptr.wis_boost_dur == 0) then p_ptr.events[29013] = 0 end

	p_ptr.events[29013] = p_ptr.events[29013] + 1
	if (p_ptr.events[29013] > 5) then p_ptr.events[29013] = 5 end

	wisbonus = ((p_ptr.stat_cur[A_WIS+1] * (p_ptr.abilities[(CLASS_PRIEST * 10) + 4] * 5)) / 100) * p_ptr.events[29013]

	p_ptr.wis_boost = wisbonus
        set_wis_boost(2 + (p_ptr.abilities[(CLASS_PRIEST * 10) + 4] / 3))

        update_and_handle()
        energy_use = 100
  end

  -- Light of Life
  if (powernum == 17) then

	local dam
        dam = (p_ptr.chp * p_ptr.abilities[(CLASS_PRIEST * 10) + 10])
	ignore_spellcraft = TRUE
        attack_aura(GF_LITE, dam, 5 + (p_ptr.abilities[(CLASS_PRIEST * 10) + 10] / 30))
	ignore_spellcraft = FALSE
        update_and_handle()
        energy_use = 100
  end

  -- Retrograde Darkness.
  if (powernum == 18) then

	local dir
	dir = lua_get_aim_dir()        
        fire_ball(GF_RETROGRADE_DARKNESS, dir, 100, (p_ptr.abilities[(CLASS_PRIEST * 10) + 9] / 10))
        update_and_handle()
        energy_use = 100
  end

  -- Dark Prayer.
  if (powernum == 19) then

	if (p_ptr.alignment < 1) then
        	p_ptr.wis_boost = ((p_ptr.stat_cur[A_WIS+1] * (p_ptr.abilities[(CLASS_PRIEST * 10) + 5] * 20)) / 100)
        	set_wis_boost(2 + (p_ptr.abilities[(CLASS_PRIEST * 10) + 5] / 3))
		dec_stat(A_WIS, 5, 2)
        	update_and_handle()
        	energy_use = 100
	else
		msg_print("Because of your good alignment, the prayer was unanswered.")
	end
  end

  -- Finishing Blow
  if (powernum == 26) then

        local dam
	local dtype

        -- If used trough combat feats, we already selected a weapon.
	if (not(combatfeat)) then choose_current_weapon() end

        if (current_weapon.tval == 0) then

                msg_print("You must use a weapon!")
                return
        end

        dam = weapon_damages()

        dir = lua_get_rep_dir()

	dtype = current_weapon.extra1
	if (dtype == 0) then dtype = GF_PHYSICAL end

	melee_attack = TRUE
	no_magic_return = TRUE
	finishingblow = 1
        chain_attack(dir, dtype, dam, 0, 1)
	finishingblow = 0
	melee_attack = FALSE
	no_magic_return = FALSE

	energy_use = 100
  end

  -- Animal Empathy
  if (powernum == 30) then

	attack_aura(GF_ANIMAL_EMPATHY, 100, (p_ptr.abilities[(CLASS_RANGER * 10) + 4] / 10) + 5)
        update_and_handle()
        energy_use = 100
  end

  -- Divine Strength
  if (powernum == 36) then

	p_ptr.str_boost = multiply_divide(p_ptr.stat_ind[A_STR+1], p_ptr.abilities[(CLASS_PALADIN * 10) + 1] * 10, 100)
        set_str_boost(4 + p_ptr.abilities[(CLASS_PALADIN * 10) + 1])
	energy_use = 100
  end

  -- Holy Bolts
  if (powernum == 37) then

  	local dam
        local dir
	local num
	local i

        -- If used trough combat feats, we already selected a weapon.
	if (not(combatfeat)) then choose_current_weapon() end

        if (current_weapon.tval == 0) then

                msg_print("You must use a weapon!")
                return
        end

	num = (p_ptr.abilities[(CLASS_PALADIN * 10) + 2] / 10) + 1

	for weap = 1, num do

		dam = weapon_damages()
		dam = dam + multiply_divide(dam, ((p_ptr.abilities[(CLASS_PALADIN * 10) + 2]) * 10), 100)
        	if (dam < 0) then dam = 0 end

		dir = lua_get_aim_dir()

        	fire_bolt(GF_LITE, dir, dam)

        	update_and_handle()
	end

        energy_use = 100

  end

  -- Evil Slayer
  if (powernum == 38) then

	local dam
	local dir
	local dtype

	if (not(combatfeat)) then choose_current_weapon() end

	if (current_weapon.tval == 0) then

                msg_print("You must use a weapon!")
                return
        end

	-- We need to choose a direction to attack. If used as combat feat,
	-- this won't be asked.
	dir = lua_get_rep_dir()

	dam = weapon_damages()

	dtype = current_weapon.extra1
	if (dtype == 0) then dtype = GF_PHYSICAL end

	-- We use a radius 0, range 1 chain attack.
	melee_attack = TRUE
	no_magic_return = TRUE
	evil_slayer = 1
    	chain_attack(dir, dtype, dam, 0, 1)
	evil_slayer = 0
	no_magic_return = FALSE
	melee_attack = FALSE

    	energy_use = 100;
  end

  -- Element Strike
  if (powernum == 53) then

	local dam
	local dir
	local dtype
	local rad

	if (not(combatfeat)) then choose_current_weapon() end

	if (current_weapon.tval == 0) then

                msg_print("You must use a weapon!")
                return
        end

	-- We need to choose a direction to attack. If used as combat feat,
	-- this won't be asked.
	dir = lua_get_rep_dir()

	dam = weapon_damages()
	dam = dam + multiply_divide(dam, ((p_ptr.abilities[(CLASS_ELEM_LORD * 10) + 2]) * 15), 100)

	dtype = p_ptr.elemlord

	rad = p_ptr.abilities[(CLASS_ELEM_LORD * 10) + 2] / 10

	-- We use a radius 0, range 1 chain attack.
	-- The "nevermiss" global variable is set to true. This way, the attack
	-- will never miss. Once the attack is executed, the variable is set to
	-- false again.
	no_magic_return = TRUE
	ignore_spellcraft = TRUE
    	chain_attack(dir, dtype, dam, rad, 1)
	ignore_spellcraft = FALSE
	no_magic_return = FALSE

    	energy_use = 100;
  end

  -- Wave of Element
  if (powernum == 57) then

  	local dam
        local dir
	local rad

        -- If used trough combat feats, we already selected a weapon.
	if (not(combatfeat)) then choose_current_weapon() end

        if (current_weapon.tval == 0) then

                msg_print("You must use a weapon!")
                return
        end

	dir = lua_get_rep_dir()

	dam = weapon_damages()
	dam = dam + multiply_divide(dam, ((p_ptr.abilities[(CLASS_ELEM_LORD * 10) + 8]) * 10), 100)
        if (dam < 0) then dam = 0 end

	rad = 0 + (p_ptr.abilities[(CLASS_ELEM_LORD * 10) + 8] / 20)

	no_magic_return = TRUE
	ignore_spellcraft = TRUE
	chain_attack(dir, p_ptr.elemlord, dam, rad, 30)
	ignore_spellcraft = FALSE
	no_magic_return = FALSE

	energy_use = 100

  end

  -- Dominate Monsters.
  if (powernum == 59) then

  	local dir
	dir = lua_get_aim_dir()        
        fire_bolt(GF_DOMINATE_MONSTER, dir, 100)
        update_and_handle()
        energy_use = 100
  end

  -- Defensive Strike
  if (powernum == 62) then

        local dam
	local dtype

        -- If used trough combat feats, we already selected a weapon.
	if (not(combatfeat)) then choose_current_weapon() end

        if (current_weapon.tval == 0) then

                msg_print("You must use a weapon!")
                return
        end

	defensive_strike = 1
        dam = weapon_damages()
	defensive_strike = 0
	dam = dam + multiply_divide(dam, ((p_ptr.abilities[(CLASS_DEFENDER * 10) + 8]) * 20), 100);

        dir = lua_get_rep_dir()

	dtype = current_weapon.extra1
	if (dtype == 0) then dtype = GF_PHYSICAL end

	melee_attack = TRUE
	no_magic_return = TRUE
        chain_attack(dir, dtype, dam, 0, 1)
	melee_attack = FALSE
	no_magic_return = FALSE

	energy_use = 100
  end

  -- Stealth Attack
  if (powernum == 86) then

  	if (p_ptr.tim_invisible >= 1) then

        	local dam
		local dtype

                -- If used trough combat feats, we already selected a weapon.
		if (not(combatfeat)) then choose_current_weapon() end

        	if (current_weapon.tval == 0) then

                	msg_print("You must use a weapon!")
                	return
        	end

		stealth_attack = 1
                dam = weapon_damages()
		stealth_attack = 0
		dam = dam + multiply_divide(dam, ((p_ptr.abilities[(CLASS_SHADOW * 10) + 1]) * 20), 100);

                dir = lua_get_rep_dir()

		dtype = current_weapon.extra1
		if (dtype == 0) then dtype = GF_PHYSICAL end

		melee_attack = TRUE
		no_magic_return = TRUE
                chain_attack(dir, dtype, dam, 0, 1)
		melee_attack = FALSE
		no_magic_return = FALSE

		energy_use = 100

        else
  		msg_print("You can only use this if you are using a temporary invisibility ability!")
	end
  end

  -- High-Velocity Fanning
  if (powernum == 92) then

  	local dam
	local dir
	local rad
	local totalammos
	local returning
	local element
	local shooting

	shooting = 1
	dropshots = FALSE
	dropnum = 0
	shoot_type = 3

	-- If we have two ranged weapons, then choose which one we're gonna use.
	if ((inven(INVEN_WIELD).tval == TV_RANGED and inven(INVEN_WIELD).itemtype == 3 and not(get_object_flag4(inven(INVEN_WIELD), TR4_MUST2H))) and (inven(INVEN_WIELD+1).tval == TV_RANGED and inven(INVEN_WIELD+1).itemtype == 3) and not(get_object_flag4(inven(INVEN_WIELD+1), TR4_MUST2H))) then
		choose_current_weapon()
	else
		if (inven(INVEN_WIELD).tval == TV_RANGED and inven(INVEN_WIELD).itemtype == 3 and not(get_object_flag4(inven(INVEN_WIELD), TR4_MUST2H))) then
			current_weapon = inven(INVEN_WIELD)
		else
			current_weapon = inven(INVEN_WIELD+1)
		end
	end

	-- Make sure we wield a ranged weapon.
	if (not(current_weapon.tval == TV_RANGED and current_weapon.itemtype == 3 and not(get_object_flag4(inven(INVEN_WIELD), TR4_MUST2H)))) then

                msg_print("You must wield a one-handed pistol.")
                return
        end

	-- Make sure we have the proper type and number of ammos.
	if (not(current_weapon.itemtype == inven(INVEN_AMMO).itemtype)) then
		
		msg_print("You must use the proper type of ammos.")
		return
	end

	-- We need to choose a direction to attack.
	dir = lua_get_aim_dir()

	while (shooting == 1) do

		totalammos = current_weapon.extra2
		drop_ranged = inven(INVEN_AMMO)
		if (inven(INVEN_AMMO).number < totalammos) then

			shooting = 0
		end

		if (current_weapon.pval2 < totalammos) then

			shooting = 0
		end

		dam = ranged_damages()
		dam = dam + multiply_divide(dam, ((p_ptr.abilities[(CLASS_GUNNER * 10) + 1]) * 10), 100)

		-- Determine the element.
		-- The shooter has priority over the ammo.
		if (current_weapon.extra1 == 0) then
			element = inven(INVEN_AMMO).extra1
		else
			element = current_weapon.extra1
		end

		-- 0 defaults to physical.
		if (element == 0) then element = 15 end

		-- Determine radius
		-- Again, shooter has priority.
		if (current_weapon.extra5 >= inven(INVEN_AMMO).extra3) then
			rad = current_weapon.extra5
		else
			rad = inven(INVEN_AMMO).extra3
		end

		-- Shoot!
		if (shooting == 1) then
			ranged_attack = TRUE
			ignore_spellcraft = TRUE
			fire_ball(element, dir, dam, rad)
			ignore_spellcraft = FALSE
			ranged_attack = FALSE

			-- Shooter loses some ammos.
			current_weapon.pval2 = current_weapon.pval2 - totalammos

			-- Reduce ammos in inventory.
			inven_item_increase(INVEN_AMMO, -totalammos)
        		inven_item_describe(INVEN_AMMO)
        		inven_item_optimize(INVEN_AMMO)
		end
	end

    	energy_use = 100
  end

  -- Rifle Impact
  if (powernum == 93) then

	-- Make sure we use a rifle.
	if (not(inven(INVEN_WIELD).tval == TV_RANGED and inven(INVEN_WIELD).itemtype == 4) and not(inven(INVEN_WIELD+1).tval == TV_RANGED and inven(INVEN_WIELD+1).itemtype == 4)) then

                msg_print("You must wield a rifle.")
                return
        end

	special_ranged_attacks(0, 1 + (p_ptr.abilities[(CLASS_GUNNER * 10) + 3] / 5), p_ptr.abilities[(CLASS_GUNNER * 10) + 3] * 30, 2 + (p_ptr.abilities[(CLASS_GUNNER * 10) + 3] / 10))
	energy_use = 100
  end

  -- Point-Blank Shot
  if (powernum == 94) then

	pointblankshot = 1
	need_gun = 1
	special_ranged_attacks(0, 1 + (p_ptr.abilities[(CLASS_GUNNER * 10) + 4] / 4), p_ptr.abilities[(CLASS_GUNNER * 10) + 4] * 50, 0)
	need_gun = 0
	pointblankshot = 0
	energy_use = 100
  end

  -- True Shot
  if (powernum == 95) then

  	need_gun = 1
	nevermiss = TRUE
	special_ranged_attacks(0, 1 + (p_ptr.abilities[(CLASS_GUNNER * 10) + 5] / 10), p_ptr.abilities[(CLASS_GUNNER * 10) + 5] * 10, 0)
	nevermiss = FALSE
	need_gun = 0
	energy_use = 100
  end

  -- Dashing Shot
  if (powernum == 96) then

	need_gun = 1
	dashingshot = 1
	special_ranged_attacks(0, 1 + (p_ptr.abilities[(CLASS_GUNNER * 10) + 6] / 10), p_ptr.abilities[(CLASS_GUNNER * 10) + 6] * 10, 0)
	dashingshot = 0
	need_gun = 0
	energy_use = 100
  end

  -- Magic Bullets
  if (powernum == 97) then

        local tmpstring
	local bulnum
	local dur

	-- First, check if we wield a gun.
	if ((inven(INVEN_WIELD).tval == TV_RANGED and (inven(INVEN_WIELD).itemtype == 3 or inven(INVEN_WIELD).itemtype == 4)) or (inven(INVEN_WIELD+1).tval == TV_RANGED and (inven(INVEN_WIELD+1).itemtype == 3 or inven(INVEN_WIELD+1).itemtype == 4))) then
		if (inven(INVEN_WIELD).tval == TV_RANGED and (inven(INVEN_WIELD).itemtype == 3 or inven(INVEN_WIELD).itemtype == 4)) then
			current_weapon = inven(INVEN_WIELD)
		else
			current_weapon = inven(INVEN_WIELD+1)
		end
	else
		msg_print("You must wield a gun.")
		return
	end

	tmpstring = string.format('Create how many bullets? (Max: 99)')
        bulnum = get_quantity(tmpstring, 99)
	if (bulnum <= 0) then bulnum = 1 end
	if (bulnum > 99) then bulnum = 99 end

        if (p_ptr.csp >= (bulnum * 5)) then

		dur = 10 + (p_ptr.abilities[(CLASS_GUNNER * 10) + 9] * 2)

		if (current_weapon.itemtype == 3) then
			conjure_item_any(TV_AMMO, 43, dur, bulnum, FALSE, FALSE)
		else
			conjure_item_any(TV_AMMO, 44, dur, bulnum, FALSE, FALSE)
		end

		-- Once conjured, we'll improve them a bit!
		inven(INVEN_AMMO).to_h = p_ptr.abilities[(CLASS_GUNNER * 10) + 9] * 5
		inven(INVEN_AMMO).to_d = p_ptr.abilities[(CLASS_GUNNER * 10) + 9] * 5
		inven(INVEN_AMMO).dd = inven(INVEN_AMMO).dd + (p_ptr.abilities[(CLASS_GUNNER * 10) + 9] / 3)
		inven(INVEN_AMMO).ds = inven(INVEN_AMMO).ds + (p_ptr.abilities[(CLASS_GUNNER * 10) + 9] / 3)

		p_ptr.csp = p_ptr.csp - (bulnum * 5)
		update_and_handle()
	else
		msg_print("You do not have enough mana.")
		return
	end

	energy_use = 100
  end

  -- Immolating Shot
  if (powernum == 98) then

  	need_gun = 1
	immolating = 1
	special_ranged_attacks(GF_FIRE, 1 + (p_ptr.abilities[(CLASS_GUNNER * 10) + 10] / 5), p_ptr.abilities[(CLASS_GUNNER * 10) + 10] * 20, 3 + (p_ptr.abilities[(CLASS_GUNNER * 10) + 10] / 10))
	immolating = 0
	need_gun = 0
	energy_use = 100
  end

  -- Brandcasting
  if (powernum == 99) then

  	local dam
	local dir
	local rad

	-- If we have two ranged weapons, then choose which one we're gonna use.
	-- It shouldn't happen though.
	if ((inven(INVEN_WIELD).tval == TV_WEAPON or inven(INVEN_WIELD).tval == TV_ROD) and (inven(INVEN_WIELD+1).tval == TV_WEAPON or inven(INVEN_WIELD+1).tval == TV_ROD)) then
		choose_current_weapon()
	else
		if (inven(INVEN_WIELD).tval == TV_WEAPON or inven(INVEN_WIELD).tval == TV_ROD) then
			current_weapon = inven(INVEN_WIELD)
		elseif (inven(INVEN_WIELD+1).tval == TV_WEAPON or inven(INVEN_WIELD+1).tval == TV_ROD) then
			current_weapon = inven(INVEN_WIELD+1)
		elseif (inven(INVEN_HANDS).tval == TV_GLOVES) then
			current_weapon = inven(INVEN_HANDS)
		else
			msg_print("You need to wield a weapon, or use gloves if unarmed.")
			return
		end
	end

	-- Make the weapon is an "orange" weapon.
	if (not(get_object_flag4(current_weapon, TR4_CRAFTED))) then

                msg_print("You can only use this ability with enchanted crafted weapons.")
                return
        end

	-- Make sure it has a brand.
	if (current_weapon.branddam == 0) then
		
		msg_print("This weapon has no brands.")
		return
	end

	-- Direction.
	dir = lua_get_aim_dir()

	dam = current_weapon.branddam + multiply_divide(current_weapon.branddam, ((p_ptr.abilities[(CLASS_ENCHANTER * 10) + 2]) * 20), 100)
	rad = current_weapon.brandrad + 2 + (p_ptr.abilities[(CLASS_ENCHANTER * 10) + 2] / 20)

	fire_ball(current_weapon.brandtype, dir, dam, rad)

    	energy_use = 100
  end

  -- Divine Items
  if (powernum == 105) then

	local item
	local power
	local i

	-- Pick an item of any tval.
	item = lua_pick_item(0)

	-- This condition is to check if we actually selected an item.
	if (item) then

		-- Check if the item is identified.
		if (not(is_identified(item))) then

			-- "power" based on Diviner's ability.
			power = lua_randint(p_ptr.abilities[(CLASS_DIVINER * 10) + 1] / 2) + (p_ptr.abilities[(CLASS_DIVINER * 10) + 1] / 2)
			power = power * 4

			-- Power can never exceed half of your Divination skill.
			if (power > (p_ptr.skill[27] / 2)) then power = p_ptr.skill[27] / 2 end

			-- Give abilities.
			-- Code is heavily based on objects.lua, "make_item_magic" function.
			i = 0

			-- Some items may skip that part.
			if (not(divinable(item))) then i = power end

			-- Artifacts gains no additional powers.
			if (item.name1 > 0) then i = power end

			while (i < power) do

				local btype
				local amt
				local whichone
				local misctype

				-- Roll for what type of bonus it will be.
				btype = lua_randint(100)

				-- A resistances bonus.
				if (btype >= 80) then

					-- Determine a random amount.
					amt = lua_randint(power - i)

					amt = amt / 2

					if (amt > 100) then amt = 100 end

					-- Give resistances to elements 1 to 15.
					whichone = lua_randint(15)
					item.resistances[whichone+1] = item.resistances[whichone+1] + amt
					if (item.resistances[whichone+1] > 100) then item.resistances[whichone+1] = 100 end

					-- Increase i.
					i = i + (amt * 1)

				-- A stats bonus.
				elseif (btype >= 60) then

					-- Random amount.
					amt = lua_randint(power - i)

					amt = amt / 3
					if (amt <= 0) then amt = 1 end

					-- Apply it to a random stat.
					whichone = lua_randint(6)
					item.statsbonus[whichone] = item.statsbonus[whichone] + amt

					-- Increase i.
					i = i + (amt * 8)

				-- A skills bonus.
				elseif (btype >= 40) then

					-- Random amount.
					amt = lua_randint(power - i)

					amt = amt / 3
					if (amt <= 0) then amt = 1 end

					-- Apply it to a random skill.
					whichone = lua_randint(28)

					-- phlinn's code snippet.
					if (item.tval == TV_WEAPON or item.tval == TV_ROD or item.tval == TV_RANGED) then
						if (whichone >= 13 and whichone <= 22) then 
							whichone = item.itemskill + 1
						end
					end

					item.skillsbonus[whichone] = item.skillsbonus[whichone] + amt

					-- Increase i.
					i = i + (amt * 8)

				-- A misc bonus.
				elseif (btype >= 20) then

					-- What type of "misc" bonus do you get.
					misctype = lua_randint(6)

					-- The amount of "i" that it costs depends on the bonus you get.

					-- Extra blows.
					if (misctype == 1) then

						-- Random amount.
						amt = lua_randint(power - i)

						amt = amt / 8
						if (amt <= 0) then amt = 1 end

						-- Apply it.
						item.extrablows = item.extrablows + amt

						-- Increase i.
						i = i + (amt * 9)
					end

					-- Speed bonus.
					if (misctype == 2) then

						-- Random amount.
						amt = lua_randint(power - i)

						amt = amt / 6
						if (amt <= 0) then amt = 1 end

						-- Apply it.
						item.speedbonus = item.speedbonus + amt

						-- Increase i.
						i = i + (amt * 6)
					end

					-- Life bonus.
					if (misctype == 3) then

						-- Random amount.
						amt = lua_randint(power - i)

						-- Apply it.
						item.lifebonus = item.lifebonus + amt

						-- Increase i.
						i = i + (amt / 2)
					end

					-- Light
					if (misctype == 4) then

						-- Random amount.
						amt = lua_randint(power - i)

						amt = amt / 5
						if (amt <= 0) then amt = 1 end

						-- Light NEVER exceeds 5.
						if (amt > 5) then amt = 5 end
						amt = amt - item.light

						-- Apply it.
						item.light = item.light + amt

						-- Increase i.
						i = i + amt
					end

					-- Reflection.
					if (misctype == 5) then

						-- Random amount.
						amt = lua_randint(power - i)

						-- Apply it.
						item.reflect = item.reflect + amt

						-- Increase i.
						i = i + (amt / 2)
					end

					-- Mana bonus.
					if (misctype == 6) then

						-- Random amount.
						amt = lua_randint(power - i)

						-- Apply it.
						item.manabonus = item.manabonus + amt

						-- Increase i.
						i = i + (amt / 2)
					end

				-- Gain a flag.
				else

					-- Pick a flag
					if (p_ptr.abilities[(CLASS_DIVINER * 10) + 1] >= 10) then
						misctype = lua_randint(15)
					elseif (p_ptr.abilities[(CLASS_DIVINER * 10) + 1] >= 5) then
						misctype = lua_randint(14)
					else misctype = lua_randint(13) end

					-- Give the flag. i cost varies per flags.

					-- Resist fear.
					if (misctype == 1 and not(get_object_flag2(item, TR2_RES_FEAR))) then

						give_object_flag2(item, TR2_RES_FEAR)
						i = i + 3
					end

					-- Resist conf.
					if (misctype == 2 and not(get_object_flag2(item, TR2_RES_CONF))) then

						give_object_flag2(item, TR2_RES_CONF)
						i = i + 8
					end

					-- Resist blind.
					if (misctype == 3 and not(get_object_flag2(item, TR2_RES_BLIND))) then

						give_object_flag2(item, TR2_RES_BLIND)
						i = i + 1
					end

					-- Hold life.
					if (misctype == 4 and not(get_object_flag2(item, TR2_HOLD_LIFE))) then

						give_object_flag2(item, TR2_HOLD_LIFE)
						i = i + 5
					end

					-- Safety.
					if (misctype == 5 and not(get_object_flag4(item, TR4_SAFETY))) then

						give_object_flag4(item, TR4_SAFETY)
						i = i + 8
					end

					-- Sustain Strength.
					if (misctype == 6 and not(get_object_flag2(item, TR2_SUST_STR))) then

						give_object_flag2(item, TR2_SUST_STR)
						i = i + 2
					end

					-- Sustain Intelligence.
					if (misctype == 7 and not(get_object_flag2(item, TR2_SUST_INT))) then

						give_object_flag2(item, TR2_SUST_INT)
						i = i + 2
					end

					-- Sustain Wisdom.
					if (misctype == 8 and not(get_object_flag2(item, TR2_SUST_WIS))) then

						give_object_flag2(item, TR2_SUST_WIS)
						i = i + 2
					end

					-- Sustain Dexterity.
					if (misctype == 9 and not(get_object_flag2(item, TR2_SUST_DEX))) then

						give_object_flag2(item, TR2_SUST_DEX)
						i = i + 2
					end

					-- Sustain Constitution.
					if (misctype == 10 and not(get_object_flag2(item, TR2_SUST_CON))) then

						give_object_flag2(item, TR2_SUST_CON)
						i = i + 2
					end

					-- Sustain Charisma.
					if (misctype == 11 and not(get_object_flag2(item, TR2_SUST_CHR))) then

						give_object_flag2(item, TR2_SUST_CHR)
						i = i + 2
					end

					-- Telepathy.
					if (misctype == 12 and not(get_object_flag3(item, TR3_TELEPATHY))) then

						give_object_flag3(item, TR3_TELEPATHY)
						i = i + 8
					end

					-- Regen.
					if (misctype == 13 and not(get_object_flag3(item, TR3_REGEN))) then

						give_object_flag3(item, TR3_REGEN)
						i = i + 5
					end

					-- Eternal.
					if (misctype == 14 and not(get_object_flag4(item, TR4_ETERNAL))) then

						give_object_flag4(item, TR4_ETERNAL)
						i = i + 10
					end

					-- Levels.
					if (misctype == 15 and not(get_object_flag4(item, TR4_LEVELS))) then

						give_object_flag4(item, TR4_LEVELS)
						item.level = 1
						item.kills = 0
						item.tweakpoints = item.tweakpoints + 2
						i = i + 30
					end
				end

			end

			-- Improve the base AC, base damages, to_h, to_d, etc...
			if (not(item.name1 > 0) and divinable(item)) then
				item.dd = item.dd + ((item.dd * (power / 2)) / 100)
				item.ds = item.ds + ((item.ds * (power / 2)) / 100)
				item.ac = item.ac + ((item.ac * (power / 2)) / 100)
				item.to_h = item.to_h + lua_randint(power / 2)
				item.to_d = item.to_d + lua_randint(power / 2)
				item.to_a = item.to_a + lua_randint(power / 2)

				-- Give a flag to mark is as a magic item.
				give_object_flag4(item, TR4_ENCHANTED)
			end

			-- Fully identify the item.
			identify_fully_specific(item)
		else
			msg_print("This item is already identified.")
		end
	end

  end

  -- Nightmares.
  if (powernum == 106) then

	local dir
	dir = lua_get_aim_dir()        
        fire_ball(GF_NIGHTMARES, dir, 5 + p_ptr.abilities[(CLASS_DIVINER * 10) + 6], 3 + (p_ptr.abilities[(CLASS_DIVINER * 10) + 6] / 10))
        update_and_handle()
        energy_use = 100
  end

  -- Finish Him!
  if (powernum == 108) then


	local dir
	local dir_x
	local dir_y
	local dam
	local dtype

	if (not(kensai_equip())) then 

                msg_print("Your equipment is not suitable for this attack")
                return
        end

	choose_current_weapon()

	if (current_weapon.tval == 0 or not(current_weapon.itemskill == 12) or get_object_flag4(current_weapon, TR4_MUST2H)) then

                msg_print("You must use a 1 handed sword!")
                return
        end

	-- We need to choose a direction to attack. If used as combat feat,
	-- this won't be asked.
	-- Get the direction.
	dir = lua_get_rep_dir()

	-- Determine the coordinates where we will look for a monster.
	if (dir == 0) then return end
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
        -- shouldn't be possible...
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

	dam = weapon_damages()
        dam = dam + (dam * p_ptr.stat_ind[A_WIS+1] * 5 * (3 + p_ptr.abilities[(CLASS_KENSAI * 10) + 4]) / 100)

	dtype = current_weapon.extra1
	if (dtype == 0) then dtype = GF_PHYSICAL end

	melee_attack = TRUE
	no_magic_return = TRUE
    	fire_ball_specific_grid(dam, dir_x, dir_y, 0, dtype)
	no_magic_return = FALSE
	melee_attack = FALSE

    	energy_use = 100    

  end

  -- Lightning Strike
  if (powernum == 109) then


	local dir
	local tgt_x
	local tgt_y
	local dam
	local dtype
	local rad

	if (not(kensai_equip())) then 

                msg_print("Your equipment is not suitable for this attack")
                return
        end

	if (not(combatfeat)) then choose_current_weapon() end

	if (current_weapon.tval == 0 or not(current_weapon.itemskill == 12) or get_object_flag4(current_weapon, TR4_MUST2H)) then

                msg_print("You must use a 1 handed sword!")
                return
        end

	rad = 2 + (p_ptr.abilities[(CLASS_KENSAI * 10) + 5] / 10)

	-- We need to choose a location to attack.
	-- Get the target.
	if (not(lua_tgt_pt())) then return end
	tgt_x = global_x
	tgt_y = global_y
	--dir = lua_get_aim_dir()

	--if (dir == 0) then return end

	--if (dir == 5) then

	--	if (not(lua_target_okay())) then return end

	--	tgt_y = target_row
	--	tgt_x = target_col

		if (distance(tgt_y,tgt_x,py,px) > rad) then
			msg_print("Target out of range.")
			return
		end

	--else

	--	tgt_y = py
	--	tgt_x = px

	--	while (distance(tgt_y,tgt_x,py,px) < rad) do

	--		if (dir == 1) then
	--			tgt_x = tgt_x - 1
	--			tgt_y = tgt_y + 1
	--		end
	--		if (dir == 2) then
	--			dir_y = py + 1
	--		end
	--		if (dir == 3) then
	--			tgt_x = tgt_x + 1
	--			tgt_y = tgt_y + 1
	--		end
	--		if (dir == 4) then
	--			tgt_x = tgt_x - 1
	--		end
	--		if (dir == 6) then
	--			tgt_x = tgt_x + 1
	--		end
	--		if (dir == 7) then
	--			tgt_x = tgt_x - 1
	--			tgt_y = tgt_y - 1
	--		end
	--		if (dir == 8) then
	--			tgt_y = tgt_y - 1
	--		end
	--		if (dir == 9) then
	--			tgt_x = tgt_x + 1
	--			tgt_y = tgt_y - 1
	--		end			

	--	end

	--end

	dam = weapon_damages()
	dam = dam + multiply_divide(dam, p_ptr.stat_ind[A_WIS+1], 100)
	dam = dam + multiply_divide(dam, p_ptr.abilities[(CLASS_KENSAI * 10) + 5] * 10, 100)

	dtype = current_weapon.extra1
	if (dtype == 0) then dtype = GF_PHYSICAL end

	melee_attack = TRUE
	no_magic_return = TRUE
	--if (dir == 5) then
    		fire_ball_specific_grid(dam, tgt_x, tgt_y, 0, dtype)
	--else
	--	lua_project(0, 0, tgt_y, tgt_x, dam, dtype, 1)
	--end
	no_magic_return = FALSE
	melee_attack = FALSE

    	energy_use = 100    

  end

  -- The Lion's Roar
  if (powernum == 110) then

	local duration
        duration = p_ptr.abilities[(CLASS_KENSAI * 10) + 7] + 4
	lionroar = 1
	lua_project(-2, 0, py, px, duration, GF_HEROISM, 1)

	update_and_handle()
	
        attack_aura(GF_WARCRY, duration, 3)
        update_and_handle()
	lionroar = 0
	energy_use = 100
  end

  -- The Scorpion's Tail
  if (powernum == 111) then

	local dir
	local dir_x
	local dir_y

	if (not(kensai_equip())) then 

                msg_print("Your equipment is not suitable for this attack")
                return
        end

	-- We need to choose a direction to attack. If used as combat feat,
	-- this won't be asked.
	-- Get the direction.
	dir = lua_get_rep_dir()

	-- Determine the coordinates where we will look for a monster.
	if (dir == 0) then return end
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
        -- shouldn't be possible...
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

	if (not(cave(dir_y, dir_x).m_idx == 0)) then
		monster(cave(dir_y, dir_x).m_idx).defense = monster(cave(dir_y, dir_x).m_idx).defense - (monster(cave(dir_y, dir_x).m_idx).defense * 5 * (2 + (p_ptr.abilities[(CLASS_KENSAI * 10) + 8] / 10)) / 100)
		if (monster(cave(dir_y, dir_x).m_idx).defense < 0) then monster(cave(dir_y, dir_x).m_idx).defense = 0 end
	end

	scorptail = 1
	no_magic_return = TRUE
    	fire_ball_specific_grid(1, dir_x, dir_y, 0, GF_TAUNT)
	no_magic_return = FALSE
	scorptail = 0

    	energy_use = 100    

  end

  -- Whirlwind
  if (powernum == 112) then

	local dir
	local ch
	local moves
	local curmove
	local dam
	local dtype

	if (not(kensai_equip())) then 

                msg_print("Your equipment is not suitable for this attack")
                return
        end

	choose_current_weapon()

	if (current_weapon.tval == 0 or not(current_weapon.itemskill == 12) or get_object_flag4(current_weapon, TR4_MUST2H)) then

                msg_print("You must use a 1 handed sword!")
                return
        end

	dam = weapon_damages() / 2
	dam = dam + multiply_divide(dam, p_ptr.stat_ind[A_WIS+1] * 2, 100)
	dam = dam + multiply_divide(dam, p_ptr.abilities[(CLASS_KENSAI * 10) + 10] * 10, 100)

	dtype = current_weapon.extra1
	if (dtype == 0) then dtype = GF_PHYSICAL end

	moves = 3 + (p_ptr.abilities[(CLASS_KENSAI * 10) + 10] / 4)
	curmove = 1
	while (curmove <= moves) do


		-- We need to choose a direction to move.
		-- Get the direction.
		msg_print("Direction (Esc to cancel, 5/enter to not move)? ")

		ch = 0
		while (not(ch == 13) and not(ch == 27) and not(ch > 48 and ch < 58)) do
			ch = inkey()
		end
		--dir = lua_get_rep_dir()

		-- escape cancels out
		if (ch == 27) then return end
		-- enter or 5 keys, no movement, but continue
		if (ch == 13 or ch == 53) then 
			dir = 0
		else -- other number keys
			dir = ch - 48
		end
		-- Determine the coordinates where we will move.
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
	       	-- shouldn't be possible...
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
		
		if (dir > 0) then

        		if (lua_cave_empty_bold(dir_y,dir_x)) then
				teleport_player_to(dir_y,dir_x)
	        		--move_player(dir,0)
			else
				msg_print("You can't move there.")

	        	end
		end

		melee_attack = TRUE
		no_magic_return = TRUE
		attack_aura(dtype, dam, 1)
		no_magic_return = FALSE
		melee_attack = FALSE

		curmove = curmove + 1

	end

    	energy_use = 100    

  end

  -- Racial Champion
  if (powernum == 113) then

  	local dir
	dir = lua_get_aim_dir()
	no_magic_return = TRUE
	fire_ball(GF_RACIAL_CHAMPION, dir, 100, 3 + (p_ptr.abilities[(CLASS_MONSTER * 10) + 9] / 20))
	no_magic_return = FALSE
        update_and_handle()
        energy_use = 100
  end

  -- Compose Songs.
  if (powernum == 114) then
  	
	local song
	local songname
	local songtype
	local ch
	local typech
	local spell

	-- Pick a spell.
	spell = pick_spell()

	if (not(is_elemental(magic_spell[spell+1].type[1]) or is_mysticism(magic_spell[spell+1].type[1]))) then

		msg_print("You cannot use this spell for a song.")
		return
	end

	-- Choose a type.
	msg_print("Song type: [P]assive, [I]instant, [E]ffect?")

	typech = inkey()

	if (typech == 80 or typech == 112) then songtype = 1
	elseif (typech == 73 or typech == 105) then songtype = 2
	elseif (typech == 69 or typech == 101) then songtype = 3
	else return end

	-- Enter song's name.
	msg_print(NULL)
	msg_print("Enter a name for the song: ")
	lua_get_string(30)
	songname = tmpluastring

	-- Pick a slot for the song.
	song = pick_song(0)

	music_song[song+1].name = songname
	music_song[song+1].type = songtype
	music_song[song+1].power = 5 * p_ptr.abilities[(CLASS_BARD * 10) + 2]
	music_song[song+1].element = magic_spell[spell+1].type[1]
	music_song[song+1].radius = (p_ptr.abilities[(CLASS_BARD * 10) + 2] / 5) + 1
	music_song[song+1].cost = 0
	music_song[song+1].created = TRUE

	msg_print("You composed a new song!")
  end

  -- Inspire Courage
  if (powernum == 115) then

	local rad
	rad = 5 + (p_ptr.abilities[(CLASS_BARD * 10) + 3] / 10)
	nevermiss = TRUE
	no_magic_return = TRUE
        attack_aura(GF_INSPIRE_COURAGE, 100, rad)
	no_magic_return = FALSE
	nevermiss = FALSE
        update_and_handle()
	energy_use = 100
  end

  -- Sotto Voce
  if (powernum == 116) then

	local i

	-- Does not work in quest levels.
	if (p_ptr.inside_quest > 0 or dun_level == 0) then

		msg_print("This ability cannot be used here.")
		return
	end

	-- Go through every monsters on the level.
	for i = 1, m_max do

		-- Check distance.
		if ((is_pet(monster(i))) and (monster(i).cdis <= (p_ptr.abilities[(CLASS_BARD * 10) + 5] * 20))) then

			teleport_to_player(i)
		end
	end
  end

  -- Power Shot
  if (powernum == 117) then

	special_ranged_attacks(0, 1 + (p_ptr.abilities[(CLASS_MARKSMAN * 10) + 1] / 3), p_ptr.abilities[(CLASS_MARKSMAN * 10) + 1] * 30, 0)
	energy_use = 100
  end

  -- Paralyzing Shot
  if (powernum == 118) then

	paralyze_shot = 1
	special_ranged_attacks(0, 1 + (p_ptr.abilities[(CLASS_MARKSMAN * 10) + 2] / 10), p_ptr.abilities[(CLASS_MARKSMAN * 10) + 2] * 10, 0)
	paralyze_shot = 0
	energy_use = 100
  end

  -- Ricochet Shot
  if (powernum == 119) then

	special_ranged_attacks(0, 1 + (p_ptr.abilities[(CLASS_MARKSMAN * 10) + 3] / 3), p_ptr.abilities[(CLASS_MARKSMAN * 10) + 3] * 10, 2 + (p_ptr.abilities[(CLASS_MARKSMAN * 10) + 3] / 10))
	energy_use = 100
  end

  -- Counter Shot
  -- Note that the actual trigger of it is hard-coded.
  if (powernum == 120) then

	counter_shot = 1
	msg_print("You fire back!")
	special_ranged_attacks(0, 1, p_ptr.abilities[(CLASS_MARKSMAN * 10) + 4] * 20, 0)
	counter_shot = 0
  end

  -- Rapid Shot
  if (powernum == 121) then

	rapid_shot = 1
	special_ranged_attacks(0, 3 + p_ptr.abilities[(CLASS_MARKSMAN * 10) + 5], p_ptr.abilities[(CLASS_MARKSMAN * 10) + 5] * 5, 0)
	rapid_shot = 0
	energy_use = 100
  end

  -- Spinning Storm Shot
  if (powernum == 122) then

	storm_shot = 1
	special_ranged_attacks(GF_WIND, 1 + (p_ptr.abilities[(CLASS_MARKSMAN * 10) + 6] / 5), p_ptr.abilities[(CLASS_MARKSMAN * 10) + 6] * 20, 1 + (p_ptr.abilities[(CLASS_MARKSMAN * 10) + 6] / 10))
	storm_shot = 0
	energy_use = 100
  end

  -- Enhance Ranged Weapon
  if (powernum == 123) then

	local item
	local ch
	local ch2

	item = lua_pick_item(TV_RANGED)

	if (not(item)) then return 0 end

	if (get_object_flag4(item, TR4_ENHANCED)) then

		msg_print("You already enhanced this item.")
		return 0
	end

	msg_print(NULL)
	msg_print("Choose Enhancement: [1] General [2] Velocity [3] Power")

	ch = inkey()

	if (ch == 49) then

		item.to_h = item.to_h + (p_ptr.abilities[(CLASS_MARKSMAN * 10) + 7] * 3)
		item.to_d = item.to_d + (p_ptr.abilities[(CLASS_MARKSMAN * 10) + 7] * 3)
		item.extra4 = item.extra4 + (p_ptr.abilities[(CLASS_MARKSMAN * 10) + 7] * 10)
		item.extra3 = item.extra3 + (p_ptr.abilities[(CLASS_MARKSMAN * 10) + 7] / 5)
		item.extrashots = item.extrashots + (p_ptr.abilities[(CLASS_MARKSMAN * 10) + 7] / 10)

		give_object_flag4(item, TR4_ENHANCED)
		give_object_ident(item, IDENT_BROKEN)
        end

	if (ch == 50) then

		item.extra3 = item.extra3 + (p_ptr.abilities[(CLASS_MARKSMAN * 10) + 7] / 2)
		item.extrashots = item.extrashots + (p_ptr.abilities[(CLASS_MARKSMAN * 10) + 7] / 5)
		item.extra2 = item.extra2 - (p_ptr.abilities[(CLASS_MARKSMAN * 10) + 7] / 10)
		if (item.extra2 < 1) then item.extra2 = 1 end

		give_object_flag4(item, TR4_ENHANCED)
		give_object_ident(item, IDENT_BROKEN)
        end

	if (ch == 51) then

		item.to_h = item.to_h + (p_ptr.abilities[(CLASS_MARKSMAN * 10) + 7] * 5)
		item.to_d = item.to_d + (p_ptr.abilities[(CLASS_MARKSMAN * 10) + 7] * 5)
		item.extra4 = item.extra4 + (p_ptr.abilities[(CLASS_MARKSMAN * 10) + 7] * 20)

		if (p_ptr.abilities[(CLASS_MARKSMAN * 10) + 7] >= 10) then

			msg_print(NULL)
			msg_print("Do you want to change the elemental damages type? [y/n]")

			ch2 = inkey()

			if (ch2 == 89 or ch2 == 121) then

				local spell

				-- Pick a spell.
				spell = pick_spell()

				if (not(is_elemental(magic_spell[spell+1].type[1]))) then

					msg_print("You cannot select this spell for the enhancement.")
					return
				end

				item.extra1 = magic_spell[spell+1].type[1]
			end
		end

		give_object_flag4(item, TR4_ENHANCED)
		give_object_ident(item, IDENT_BROKEN)
        end

	msg_print("You enhance the weapon!")
	energy_use = 100
  end

  -- Aim
  if (powernum == 124) then

	p_ptr.dex_boost = multiply_divide(p_ptr.stat_ind[A_DEX+1], p_ptr.abilities[(CLASS_MARKSMAN * 10) + 8] * 10, 100)
        set_dex_boost(4 + p_ptr.abilities[(CLASS_MARKSMAN * 10) + 8])
	energy_use = 100
  end

  -- Piercing Shot
  if (powernum == 125) then

	piercing_shot = p_ptr.abilities[(CLASS_MARKSMAN * 10) + 9] * 5
	if (piercing_shot > 100) then piercing_shot = 100 end
	special_ranged_attacks(0, 1 + (p_ptr.abilities[(CLASS_MARKSMAN * 10) + 9] / 4), p_ptr.abilities[(CLASS_MARKSMAN * 10) + 9] * 10, 0)
	piercing_shot = 0
	energy_use = 100
  end

  -- Power Attacks feats.
  if (powernum == 1000) then

	local pdur

	pdur = 3 + p_ptr.abilities[(CLASS_FIGHTER * 10) + 10]

	p_ptr.powerlevel = 1
	p_ptr.str_boost = p_ptr.stat_cur[A_STR+1]
	if (p_ptr.abilities[(CLASS_FIGHTER * 10) + 7] >= 1) then

		p_ptr.pres = 15 + p_ptr.abilities[(CLASS_FIGHTER * 10) + 7]
		p_ptr.mres = 15 + p_ptr.abilities[(CLASS_FIGHTER * 10) + 7]
		if (p_ptr.pres > 75) then p_ptr.pres = 75 end
		if (p_ptr.mres > 75) then p_ptr.mres = 75 end
		set_pres(pdur)
		set_mres(pdur)
	end
	set_powerattack(pdur)
	set_str_boost(pdur)
        energy_use = 200
  end

  if (powernum == 1001) then

	local pdur

	pdur = 4 + p_ptr.abilities[(CLASS_FIGHTER * 10) + 10]

	p_ptr.powerlevel = 2
	p_ptr.str_boost = p_ptr.stat_cur[A_STR+1] * 5
	if (p_ptr.abilities[(CLASS_FIGHTER * 10) + 7] >= 1) then

		p_ptr.pres = 15 + p_ptr.abilities[(CLASS_FIGHTER * 10) + 7]
		p_ptr.mres = 15 + p_ptr.abilities[(CLASS_FIGHTER * 10) + 7]
		if (p_ptr.pres > 75) then p_ptr.pres = 75 end
		if (p_ptr.mres > 75) then p_ptr.mres = 75 end
		set_pres(pdur)
		set_mres(pdur)
	end
	set_powerattack(pdur)
	set_str_boost(pdur)
        energy_use = 300
  end

  if (powernum == 1002) then

	local pdur

	pdur = 5 + p_ptr.abilities[(CLASS_FIGHTER * 10) + 10]

	p_ptr.powerlevel = 3
	p_ptr.str_boost = p_ptr.stat_cur[A_STR+1] * 9
	if (p_ptr.abilities[(CLASS_FIGHTER * 10) + 7] >= 1) then

		p_ptr.pres = 15 + p_ptr.abilities[(CLASS_FIGHTER * 10) + 7]
		p_ptr.mres = 15 + p_ptr.abilities[(CLASS_FIGHTER * 10) + 7]
		if (p_ptr.pres > 75) then p_ptr.pres = 75 end
		if (p_ptr.mres > 75) then p_ptr.mres = 75 end
		set_pres(pdur)
		set_mres(pdur)
	end
	set_powerattack(pdur)
	set_str_boost(pdur)
        energy_use = 400
  end

  -- Agility Jump
  if (powernum == 1006) then

	local x
	local y

	-- Actually jump!
	-- We use a special function that returns x and y coordinates in global
	-- variables, so we can use these in Lua.
	msg_print("You jump very high!")
        if (not(lua_tgt_pt())) then return end
	x = global_x
	y = global_y

	-- Most functions here use a (y,x) format, instead of (x,y).
	-- This is important, because if you use (x,y), it might crash.
        if (not(lua_cave_empty_bold(y,x)) or (distance(y,x,py,px) > 1 + (p_ptr.skill[6] / 10))) then

              msg_print("You can't jump there...")

        else

                if (not(get_cave_info_flag(y, x, CAVE_MARK))) then

                        if (get_cave_info_flag(y, x, CAVE_LITE)) then

				teleport_player_to(y,x)
                        else
				msg_print("You can't jump there...")
			end

                else

			teleport_player_to(y,x)
		end
        end

	-- Take a turn.
	energy_use = 100
  end

  -- Dig/Crumble Mining feat!
  if (powernum == 1032) then

	local dir
	local dir_x
	local dir_y
	local dam

	-- We need to choose a direction to attack. If used as combat feat,
	-- this won't be asked.
	-- Get the direction.
	dir = lua_get_rep_dir()

	-- Determine the coordinates where we will look for a monster.
	if (dir == 0) then return end
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
        -- shouldn't be possible...
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

	-- If used on a floor grid, make some rubble.
	-- If used on a pit, make a floor.
	-- Otherwise, do nothing.
	if (get_feat_flag1(cave(dir_y, dir_x).feat, FF1_FLOOR)) then

		cave_set_feat(dir_y, dir_x, FEAT_RUBBLE)
	elseif (cave(dir_y, dir_x).feat == 87) then

		cave_set_feat(dir_y, dir_x, FEAT_FLOOR)
	end

	-- Does some damages.
	dam = (10 * p_ptr.skill[30])
	dam = dam + multiply_divide(dam, p_ptr.skill[30] * 20, 100)

	-- It is treated as a melee attack.
	no_magic_return = TRUE
	melee_attack = TRUE
    	fire_ball_specific_grid(dam, dir_x, dir_y, 0, GF_PHYSICAL)
	melee_attack = FALSE
	no_magic_return = FALSE

    	energy_use = 100    

  end

  -- Hexblaze Strike
  if (powernum == 2302) then

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
	dam = dam + multiply_divide(dam, ((p_ptr.abilities[(CLASS_NIGHT1 * 10) + 3]) * 20), 100)

	-- We use a radius 0, range 1 chain attack.
	-- The "nevermiss" global variable is set to true. This way, the attack
	-- will never miss. Once the attack is executed, the variable is set to
	-- false again.

	melee_attack = TRUE
	no_magic_return = TRUE
	hexblaze = 1
    	chain_attack(dir, GF_FIRE, dam, 0, 1)
	hexblaze = 0
	no_magic_return = FALSE
	melee_attack = FALSE
	nevermiss = FALSE

    	energy_use = 100;
  end

  -- Thunderglow Shots
  if (powernum == 2305) then

	special_ranged_attacks(GF_ELEC, 1 + (p_ptr.abilities[(CLASS_NIGHT1 * 10) + 6] / 5), p_ptr.abilities[(CLASS_NIGHT1 * 10) + 6] * (5 + (p_ptr.cursed / 4)), (p_ptr.cursed / 50))
	special_ranged_attacks(GF_LITE, 1 + (p_ptr.abilities[(CLASS_NIGHT1 * 10) + 6] / 5), p_ptr.abilities[(CLASS_NIGHT1 * 10) + 6] * (5 + (p_ptr.cursed / 4)), (p_ptr.cursed / 50))

	energy_use = 100
  end

  -- Light Armored Nightmare
  if (powernum == 2307) then

	local dam
	local dtype
	local dbonus

	-- If used trough combat feats, we already selected a weapon.
	if (not(combatfeat)) then choose_current_weapon() end

        if (current_weapon.tval == 0) then

                msg_print("You must use a weapon!")
                return
        end

	dbonus = 20 * p_ptr.abilities[(CLASS_NIGHT1 * 10) + 8]
	dbonus = dbonus + multiply_divide(dbonus, p_ptr.cursed, 100)
        
	-- Damages.
	dam = weapon_damages()
	dam = dam + multiply_divide(dam, dbonus, 100)
        if (dam < 0) then dam = 0 end

	dtype = current_weapon.extra1
	if (dtype == 0) then dtype = GF_PHYSICAL end

	-- The function "attack_aura" projects an attack around the player.
	-- Here, we project Physical over a radius of 1.
	melee_attack = TRUE
	no_magic_return = TRUE
	attack_aura(dtype, dam, 2 + (p_ptr.abilities[(CLASS_NIGHT1 * 10) + 8] / 5))
	no_magic_return = FALSE
	melee_attack = FALSE

	-- The attack takes a turn.
	-- Every 100 pts of "energy_use", an extra turn is taken.
	energy_use = 100
  end

end

-- Returns the name of a scripted power.
function get_scripted_spell_name (r_idx, which)

	local attname

	-- Default
	attname = "Unknown Power"

	if (m_race(r_idx).spell[which].name == "mind_gazer_divination") then attname = "Mind Gazer Divination" end
	if (m_race(r_idx).spell[which].name == "naga_princess_mystic_blade") then attname = "Mystic Blade" end
	if (m_race(r_idx).spell[which].name == "christina_sword_of_gaia") then attname = "Sword of Gaia" end
	if (m_race(r_idx).spell[which].name == "grey_wight_gaze") then attname = "Grey Wight's Gaze" end
	if (m_race(r_idx).spell[which].name == "monster_teleport_to_player") then attname = "Teleport to Player" end
	if (m_race(r_idx).spell[which].name == "reality_twists_quazar") then attname = "Quazar's Reality Twists" end
	if (m_race(r_idx).spell[which].name == "reality_twists_simon") then attname = "Simon's Reality Twists" end
	if (m_race(r_idx).spell[which].name == "reality_twists_simon_2") then attname = "Simon's Reality Twists" end
	if (m_race(r_idx).spell[which].name == "ancient_phantom_golem_boost") then attname = "Enhance Bone Golems" end

	return attname
end

-- Monster race: use a scripted spell(if any).
function use_scripted_spell (which)

	-- Mind Gazer Divination.
	if (m_race(p_ptr.body_monster).spell[which].name == "mind_gazer_divination") then

		local ppower
		local mpower
		local x
		local y
		local dam

		msg_print("Point a monster.")
		if (not(lua_tgt_pt())) then return end
		x = global_x
		y = global_y

		-- Check if there is a monster.
		if (cave(y, x).m_idx == 0) then

			msg_print("No monsters there.")
			return
		end

		ppower = p_ptr.stat_ind[A_INT+1] + p_ptr.stat_ind[A_WIS+1]
		ppower = ppower + (p_ptr.abilities[(CLASS_MONSTER * 10) + 3] * 10) + (p_ptr.abilities_monster_spells[which] * 20)
		mpower = monster(cave(y, x).m_idx).level + monster(cave(y, x).m_idx).mind

		if (lua_randint(ppower) >= lua_randint(mpower)) then
			dam = m_race(monster(cave(y, x).m_idx).r_idx).level + monster(cave(y, x).m_idx).level
			health_track(cave(y, x).m_idx)
			target_who = cave(y, x).m_idx
			target_row = y
			target_col = x

			fire_bolt(GF_DIVINATION, 5, dam)
		else
			msg_print(string.format('%s resists your power.', m_race(monster(cave(y, x).m_idx).r_idx).name_char))
		end
	end

	-- Mystic Blade
	if (m_race(p_ptr.body_monster).spell[which].name == "naga_princess_mystic_blade") then

		local dam
		local dir

		if (unarmed()) then dam = monk_damages()
		else dam = weapon_damages() end

		dam = dam + multiply_divide(dam, (p_ptr.abilities[(CLASS_MONSTER * 10) + 3] * 10) + (p_ptr.abilities_monster_spells[which] * 20), 100)

		-- This is different from lua_get_rep_dir(). This one allow you to set
		-- the monster as a target.
		dir = lua_get_aim_dir()

		-- Use a bolt spell against an ememy.
        	fire_bolt(GF_HARM, dir, dam)
	end

end

add_event_handler("use_ability", use_ability)
add_event_handler("use_scripted_spell", use_scripted_spell)
add_event_handler("get_scripted_spell_name", get_scripted_spell_name)
