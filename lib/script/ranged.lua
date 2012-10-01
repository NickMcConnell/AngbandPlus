-- File: ranged.lua
-- Contains all codes related to ranged attacks.

-- k_info ranged parameters:
-- RANGED
-- itemtype = type of ammos used.
-- itemskill = Specialized skill needed to increase damages.
-- extra1 = type of damages
-- extra2 = ammos consumed per shots.
-- extra3 = max ammos. If 0, it's unlimited.
-- extra4 = Mult factor(used damages).
-- extra5 = radius.

-- AMMOS
-- itemtype = ranged weapon that goes with this ammo.
-- extra1 = type of damages
-- extra2 = chance of breaking(in %).
-- extra3 = radius.

-- Main function for shooting with shooting weapons.
-- Entirely rewritten! This one is so much simpler than the old one.
function ranged_shoot ()

	local i
	local j
	local totfire
	local weap
	local dir
	local element
	local dam
	local rad
	local totalammos
	local returning

	-- Reset some variables.
	dropshots = FALSE
	dropnum = 0
	ranged_attack = FALSE

	-- First, check if we actually use this function.
	-- If we're a monster, and have no ranged weapons, we will use the monster's powers.
	if (not(inven(INVEN_WIELD).tval == TV_RANGED) and not(inven(INVEN_WIELD+1).tval == TV_RANGED)) then


		if (inven(INVEN_AMMO).tval == TV_THROWING) then

			p_ptr.events[29045] = 1
			ranged_throw()
			return
		else
			msg_print("You have nothing to fire with.")
			return
		end
	end

	-- Shoot once for every ranged weapons equipped.
	for weap = 0, 1 do

		if (weap == 0) then
			totfire = p_ptr.num_fire
		else
			totfire = p_ptr.num_fire2
		end

		for j = 1, totfire do

			dir = 0

			-- Look for a ranged weapon.
			if (inven(INVEN_WIELD + weap).tval == TV_RANGED) then

				-- Look if we have the proper ammos.
				if (inven(INVEN_WIELD + weap).itemtype == inven(INVEN_AMMO).itemtype) then

					if (inven(INVEN_WIELD + weap).pval2 >= inven(INVEN_WIELD + weap).extra2) then

						-- Determine the needed amount of ammos.
						totalammos = inven(INVEN_WIELD + weap).extra2
				
						-- Look if we have the proper quantity of ammo.
						if (inven(INVEN_AMMO).number >= totalammos) then

							-- The ammo we're gonna shoot(and possibly drop).
							drop_ranged = inven(INVEN_AMMO)

							-- But maybe we run a script first?
							player_before_ranged()
					
							-- Direction
							dir = lua_get_aim_dir()

							-- Return if we abort this step.
							if (dir == 0) then return end

							-- Determine the element.
							-- The shooter has priority over the ammo.
							if (inven(INVEN_WIELD + weap).extra1 == 0) then
								element = inven(INVEN_AMMO).extra1
							else
								element = inven(INVEN_WIELD + weap).extra1
							end

							-- 0 defaults to physical.
							if (element == 0) then element = 15 end
					
							-- Determine damages.
							current_weapon = inven(INVEN_WIELD + weap)
							dam = ranged_damages()

							-- Determine radius
							-- Again, shooter has priority.
							if (inven(INVEN_WIELD + weap).extra5 >= inven(INVEN_AMMO).extra3) then
								rad = inven(INVEN_WIELD + weap).extra5
							else
								rad = inven(INVEN_AMMO).extra3
							end

							-- Are we returning?
							returning = 0
							if ((p_ptr.skill_base[3] >= 70 and not(inven(INVEN_WIELD + weap).itemtype == 3 or inven(INVEN_WIELD + weap).itemtype == 4)) or (get_object_flag4(inven(INVEN_AMMO), TR4_RETURNING))) then

								returning = 1
							end

							-- Determine if the ammo will drop on the ground.
							-- A couple of global variables are used here to make
							-- things a lot easier.
							if ((lua_randint(100) > inven(INVEN_AMMO).extra2) and returning == 0) then
								dropshots = TRUE
								dropnum = totalammos
							end

							-- Actually fire a shot!
							-- The "ranged_attack" variable is set to true.
							-- See elements.lua for extra code for ranged attacks hitting
							-- a monster.
							-- Finally, "shoot_type" is the type of shooter that was used.
							shoot_type = inven(INVEN_WIELD + weap).itemtype
							ranged_attack = TRUE
							ignore_spellcraft = TRUE
							fire_ball(element, dir, dam, rad)
							ignore_spellcraft = FALSE
							ranged_attack = FALSE

							-- Shooter loses some ammos(except for bows).
							if (not(inven(INVEN_WIELD + weap).itemtype == 1) and returning == 0) then
								inven(INVEN_WIELD + weap).pval2 = inven(INVEN_WIELD + weap).pval2 - totalammos
							end

							-- Reduce ammos in inventory.
							if (returning == 0) then
								inven_item_increase(INVEN_AMMO, -totalammos)
                        					inven_item_describe(INVEN_AMMO)
                        					inven_item_optimize(INVEN_AMMO)
							end

							-- Run a script?
							player_after_ranged(dam)

							-- Take a turn.
							energy_use = 100

							-- Update.
							update_and_handle()
						else
							msg_print("You need more ammos!")
						end
					else
						msg_print("This weapon needs to be reloaded!")
					end
				else
					msg_print("Cannot shoot these ammos with this weapon!")
				end
			end

		end
	end

end

-- Main function for throwing items.
-- Similar to ranged_shoot. Again, an entirely rewritten code!
function ranged_throw ()

	local i
	local j
	local dir
	local element
	local dam
	local rad
	local returning
	local getitem

	-- Throw twice if Throwing skill is 40+.
	j = p_ptr.num_fire

	for i = 1, j do

		-- Reset some variables.
		dropshots = FALSE
		dropnum = 0
		throw_attack = FALSE
		throw_item = 0
		throw_floorpack = 0

		-- First, choose something to throw.
		-- If event 29044 is 1, it will select INVEN_AMMO.
		getitem = throw_select()

		-- If we have no items, return.
		if (not(getitem)) then return end

		-- Determine the element.
		element = drop_ranged.extra1

		-- 0 defaults to physical.
		if (element == 0) then element = 15 end
					
		-- Determine damages.
		dam = throw_damages()

		-- Radius is 0.
		rad = 0

		-- Are we returning?
		returning = 0
		if (get_object_flag4(drop_ranged, TR4_RETURNING)) then

			returning = 1
		end

		-- For now, thrown items don't break, unless they're potions.
		dropnum = 1
		if (returning == 0 and not(drop_ranged.tval == TV_POTION)) then
			dropshots = TRUE
		end

		-- Direction
		dir = lua_get_aim_dir()

		-- Return if we abort this step.
		if (dir == 0) then return end

		-- Actually throw the item!
		-- The "throw_attack" variable is set to true.
		-- See elements.lua for extra code for throw attacks hitting
		-- a monster. Nevermiss is set to true to prevent Physical from
		-- being treated differently than other spells.

		-- Potions uses a special code.

		if (drop_ranged.tval == TV_POTION) then

			-- Thrown potions are considered magical attacks.
			-- But they could have some particularities.

			element = drop_ranged.brandtype
			dam = potion_power(drop_ranged)
			rad = drop_ranged.brandrad

			potion_throw = 1
			fire_ball(element, dir, dam, rad)
			potion_throw = 0
		else
			throw_attack = TRUE
			ignore_spellcraft = TRUE
			fire_ball(element, dir, dam, rad)
			ignore_spellcraft = FALSE
			throw_attack = FALSE
		end

		-- Remove item.
		if (returning == 0) then
			if (throw_floorpack == 0) then
				inven_item_increase(throw_item, -1)
        			inven_item_describe(throw_item)
        			inven_item_optimize(throw_item)
			else
				floor_item_increase(0 - throw_item, -1);
				floor_item_describe(0 - throw_item);
				floor_item_optimize(0 - throw_item);
			end
		end

		-- Take a turn.
		energy_use = 100

		-- Update.
		update_and_handle()
	end
end

-- Ranged attacks by Monster race.
function monster_ranged_attacks (which)

	local i
	local j
	local totfire
	local weap
	local dir
	local element
	local dam
	local rad
	local totalammos
	local returning

	-- Reset some variables.
	dropshots = FALSE
	dropnum = 0
	ranged_attack = FALSE

	-- First, check if we actually use this function.
	-- If we're a monster, and have no ranged weapons, we will use the monster's powers.
	if (not(inven(INVEN_WIELD).tval == TV_RANGED) and not(inven(INVEN_WIELD+1).tval == TV_RANGED)) then

		-- Run a script?
		player_before_ranged()

		-- With no ranged weapons, we use a magic attack.
		dam = damroll(m_race(p_ptr.body_monster).attack[which].ddice, m_race(p_ptr.body_monster).attack[which].dside)
		dam = dam * (p_ptr.skill[3] + 1)
		dam = dam + multiply_divide(dam, (p_ptr.stat_ind[A_DEX+1] - 5) * 5, 100)
		dam = dam + multiply_divide(dam, p_ptr.stat_ind[A_DEX+1], 100)

		dam = dam + (dam * p_ptr.abilities_monster_attacks[which])
		rad = m_race(p_ptr.body_monster).attack[which].special1 + (p_ptr.abilities_monster_attacks[which] / 10)

		dir = lua_get_aim_dir()

		ignore_spellcraft = TRUE
		fire_ball(m_race(p_ptr.body_monster).attack[which].element, dir, dam, rad)
		ignore_spellcraft = FALSE

		-- Run a script?
		player_after_ranged(dam)

		energy_use = 100

		return
	end

	-- Shoot once for every ranged weapons equipped.
	for weap = 0, 1 do

		if (weap == 0) then
			local maxfire

			maxfire = 1 + (p_ptr.abilities_monster_attacks[which] / 3)
			totfire = p_ptr.num_fire
			if (totfire > maxfire) then totfire = maxfire end
		else
			local maxfire

			maxfire = 1 + (p_ptr.abilities_monster_attacks[which] / 3)
			totfire = p_ptr.num_fire2
			if (totfire > maxfire) then totfire = maxfire end
		end

		for j = 1, totfire do

			dir = 0

			-- Look for a ranged weapon.
			if (inven(INVEN_WIELD + weap).tval == TV_RANGED) then

				-- Look if we have the proper ammos.
				if (inven(INVEN_WIELD + weap).itemtype == inven(INVEN_AMMO).itemtype) then

					if (inven(INVEN_WIELD + weap).pval2 >= inven(INVEN_WIELD + weap).extra2) then

						-- Determine the needed amount of ammos.
						totalammos = inven(INVEN_WIELD + weap).extra2
				
						-- Look if we have the proper quantity of ammo.
						if (inven(INVEN_AMMO).number >= totalammos) then

							-- The ammo we're gonna shoot(and possibly drop).
							drop_ranged = inven(INVEN_AMMO)

							-- Run a script?
							player_before_ranged()
					
							-- Direction
							dir = lua_get_aim_dir()

							-- Return if we abort this step.
							if (dir == 0) then return end

							-- Determine the element.
							-- Decided by the attack you chose.
							element = m_race(p_ptr.body_monster).attack[which].element

							-- 0 defaults to physical.
							if (element == 0) then element = 15 end
					
							-- Determine damages.
							current_weapon = inven(INVEN_WIELD + weap)

							p_ptr.events[29017] = damroll(m_race(p_ptr.body_monster).attack[which].ddice, m_race(p_ptr.body_monster).attack[which].dside)
							dam = ranged_damages()
							dam = dam + multiply_divide(dam, p_ptr.abilities_monster_attacks[which] * 20, 100)

							-- Determine radius
							-- Again, shooter has priority.
							if (inven(INVEN_WIELD + weap).extra5 >= inven(INVEN_AMMO).extra3) then
								rad = inven(INVEN_WIELD + weap).extra5
							else
								rad = inven(INVEN_AMMO).extra3
							end

							rad = rad + m_race(p_ptr.body_monster).attack[which].special1 + (p_ptr.abilities_monster_attacks[which] / 10)

							-- Are we returning?
							returning = 0
							if ((p_ptr.skill_base[3] >= 70 and not(inven(INVEN_WIELD + weap).itemtype == 3 or inven(INVEN_WIELD + weap).itemtype == 4)) or (get_object_flag4(inven(INVEN_AMMO), TR4_RETURNING))) then

								returning = 1
							end

							-- Determine if the ammo will drop on the ground.
							-- A couple of global variables are used here to make
							-- things a lot easier.
							if ((lua_randint(100) > inven(INVEN_AMMO).extra2) and returning == 0) then
								dropshots = TRUE
								dropnum = totalammos
							end

							-- Actually fire a shot!
							-- The "ranged_attack" variable is set to true.
							-- See elements.lua for extra code for ranged attacks hitting
							-- a monster.
							-- Finally, "shoot_type" is the type of shooter that was used.
							shoot_type = inven(INVEN_WIELD + weap).itemtype
							ranged_attack = TRUE
							ignore_spellcraft = TRUE
							fire_ball(element, dir, dam, rad)
							ignore_spellcraft = FALSE
							ranged_attack = FALSE

							-- Shooter loses some ammos(except for bows).
							if (not(inven(INVEN_WIELD + weap).itemtype == 1) and returning == 0) then
								inven(INVEN_WIELD + weap).pval2 = inven(INVEN_WIELD + weap).pval2 - totalammos
							end

							-- Reduce ammos in inventory.
							if (returning == 0) then
								inven_item_increase(INVEN_AMMO, -totalammos)
                        					inven_item_describe(INVEN_AMMO)
                        					inven_item_optimize(INVEN_AMMO)
							end

							-- Run a script?
							player_after_ranged(dam)

							-- Take a turn.
							energy_use = 100

							-- Update.
							update_and_handle()
						else
							msg_print("You need more ammos!")
						end
					else
						msg_print("This weapon needs to be reloaded!")
					end
				else
					msg_print("Cannot shoot these ammos with this weapon!")
				end
			end

		end
	end

end

-- Used by ranged attacks abilities.
function special_ranged_attacks (selement, maxshots, modifier, radbonus)

	local i
	local j
	local totfire
	local weap
	local dir
	local element
	local dam
	local rad
	local totalammos
	local returning
	local movedir

	-- Reset some variables.
	dropshots = FALSE
	dropnum = 0
	ranged_attack = FALSE
	movedir = 5

	-- First, check if we actually use this function.
	if (not(inven(INVEN_WIELD).tval == TV_RANGED) and not(inven(INVEN_WIELD+1).tval == TV_RANGED)) then

		msg_print("You have nothing to fire with.")
		return
	end

	-- Make sure maxshots is at least 1.
	if (maxshots < 1) then maxshots = 1 end

	-- Shoot once for every ranged weapons equipped.
	for weap = 0, 1 do

		if (weap == 0) then
			local maxfire

			maxfire = maxshots
			totfire = p_ptr.num_fire
			if (totfire > maxfire) then totfire = maxfire end
		else
			local maxfire

			maxfire = maxshots
			totfire = p_ptr.num_fire2
			if (totfire > maxfire) then totfire = maxfire end
		end

		for j = 1, totfire do

			dir = 0

			-- Look for a ranged weapon.
			if (inven(INVEN_WIELD + weap).tval == TV_RANGED) then

				-- Some abilities requires a firearm.
				if (need_gun == 1) then

					if (not(inven(INVEN_WIELD + weap).itemtype == 3 or inven(INVEN_WIELD + weap).itemtype == 4)) then

						msg_print("This ability requires a firearm.")
						break
					end
				end

				-- Look if we have the proper ammos.
				if (inven(INVEN_WIELD + weap).itemtype == inven(INVEN_AMMO).itemtype) then

					if (inven(INVEN_WIELD + weap).pval2 >= inven(INVEN_WIELD + weap).extra2) then

						-- Determine the needed amount of ammos.
						totalammos = inven(INVEN_WIELD + weap).extra2
				
						-- Look if we have the proper quantity of ammo.
						if (inven(INVEN_AMMO).number >= totalammos) then

							-- The ammo we're gonna shoot(and possibly drop).
							drop_ranged = inven(INVEN_AMMO)

							-- Run a script?
							player_before_ranged()
					
							-- Direction
							-- Spinning Storm Shot does not use this.
							if (storm_shot == 0) then
								if (counter_shot == 1) then

									target_who = p_ptr.events[29047]
									dir = 5
								else
									if (pointblankshot == 1) then dir = lua_get_rep_dir()
									else dir = lua_get_aim_dir()
									end
								end

								-- Return if we abort this step.
								if (dir == 0) then return end
							end

							-- Determine the element.
							-- Decided by the attack you chose.
							if (selement > 0) then

								element = selement
							else
								-- The shooter has priority over the ammo.
								if (inven(INVEN_WIELD + weap).extra1 == 0) then
									element = inven(INVEN_AMMO).extra1
								else
									element = inven(INVEN_WIELD + weap).extra1
								end
							end

							-- 0 defaults to physical.
							if (element == 0) then element = 15 end
					
							-- Determine damages.
							current_weapon = inven(INVEN_WIELD + weap)

							dam = ranged_damages()
							dam = dam + multiply_divide(dam, modifier, 100)

							-- Determine radius
							-- Again, shooter has priority.
							if (inven(INVEN_WIELD + weap).extra5 >= inven(INVEN_AMMO).extra3) then
								rad = inven(INVEN_WIELD + weap).extra5
							else
								rad = inven(INVEN_AMMO).extra3
							end

							rad = rad + radbonus

							-- Are we returning?
							returning = 0
							if ((p_ptr.skill_base[3] >= 70 and not(inven(INVEN_WIELD + weap).itemtype == 3 or inven(INVEN_WIELD + weap).itemtype == 4)) or (get_object_flag4(inven(INVEN_AMMO), TR4_RETURNING))) then

								returning = 1
							end

							-- Determine if the ammo will drop on the ground.
							-- A couple of global variables are used here to make
							-- things a lot easier.
							if ((lua_randint(100) > inven(INVEN_AMMO).extra2) and returning == 0) then
								dropshots = TRUE
								dropnum = totalammos
							end

							-- Actually fire a shot!
							-- The "ranged_attack" variable is set to true.
							-- See elements.lua for extra code for ranged attacks hitting
							-- a monster.
							-- Finally, "shoot_type" is the type of shooter that was used.
							shoot_type = inven(INVEN_WIELD + weap).itemtype
							ranged_attack = TRUE
							ignore_spellcraft = TRUE
							if (storm_shot == 1) then attack_aura(element, dam, rad)
							elseif (pointblankshot == 1) then chain_attack(dir, element, dam, rad, 1)
							else fire_ball(element, dir, dam, rad) end
							ignore_spellcraft = FALSE
							ranged_attack = FALSE

							-- Shooter loses some ammos(except for bows).
							if (not(inven(INVEN_WIELD + weap).itemtype == 1) and returning == 0) then
								inven(INVEN_WIELD + weap).pval2 = inven(INVEN_WIELD + weap).pval2 - totalammos

								-- Rapid Shot's reload bonus!
								if (rapid_shot == 1 and lua_randint(100) <= (10 + (p_ptr.abilities[(CLASS_MARKSMAN * 10) + 5] * 3))) then

									msg_print("You reload!")
									inven(INVEN_WIELD + weap).pval2 = inven(INVEN_WIELD + weap).extra3
								end
							end

							-- Reduce ammos in inventory.
							if (returning == 0) then
								inven_item_increase(INVEN_AMMO, -totalammos)
                        					inven_item_describe(INVEN_AMMO)
                        					inven_item_optimize(INVEN_AMMO)
							end

							-- Run a script?
							player_after_ranged(dam)

							-- Place some fields with Spinning Storm Shots.
							if (storm_shot == 1 and p_ptr.abilities[(CLASS_MARKSMAN * 10) + 6] >= 10) then

								place_field(FEAT_STORMS, rad, px, py, dam / 5)
							end

							-- Move.
							if (dashingshot == 1) then

								movedir = lua_get_rep_dir()
								move_player(movedir, 0)
							end

							-- Take a turn.
							energy_use = 100

							-- Update.
							update_and_handle()
						else
							msg_print("You need more ammos!")
						end
					else
						msg_print("This weapon needs to be reloaded!")
					end
				else
					msg_print("Cannot shoot these ammos with this weapon!")
				end
			end

		end
	end

end


function ranged_damages ()

	local k = 0
	local tskill = 0
	local xmight = 0
	local craftbonus = 0
	local craftmax = p_ptr.abilities[(CLASS_ENCHANTER * 10) + 1] * 5
	local stealthbonus
	local i

	craftbonus = p_ptr.skill[12]
	if (craftbonus > craftmax) then craftbonus = craftmax end

	if (not(get_object_flag4(current_weapon, TR4_CRAFTED))) then craftbonus = 0 end

	tskill = p_ptr.skill[3] + craftbonus

	-- Rogue's Stealthy Fighter ability.
	if (p_ptr.abilities[(CLASS_ROGUE * 10) + 1] >= 1) then

		local tmod

		if (p_ptr.abilities[(CLASS_ROGUE * 10) + 1] >= 10) then tmod = 100
		else tmod = p_ptr.abilities[(CLASS_ROGUE * 10) + 1] * 10
		end

		tskill = tskill + multiply_divide(p_ptr.skill[7], tmod, 100)

		i = p_ptr.abilities[(CLASS_ROGUE * 10) + 1]
		if (i > 10) then i = 10 end
		stealthbonus = multiply_divide(p_ptr.skill[7], i, 100)
		if (stealthbonus > 10) then stealthbonus = 10 end
		stealthbonus = stealthbonus * p_ptr.abilities[(CLASS_ROGUE * 10) + 1]
	end

	if (current_weapon.tval == TV_RANGED) then

		tskill = tskill + (p_ptr.skill[current_weapon.itemskill + 1] + (p_ptr.skill[current_weapon.itemskill + 1] / 2))
	else
		return 0
	end

	if (get_object_flag3(current_weapon, TR3_XTRA_MIGHT)) then xmight = 1 end

	-- If no ammos, damages is 0.
	if (inven(INVEN_AMMO).k_idx == 0) then
		return 0
	end

	if (inven(INVEN_AMMO).disabled == 0) then
        	k = damroll(inven(INVEN_AMMO).dd, inven(INVEN_AMMO).ds)
	else
		k = 1
	end

	if (p_ptr.prace == RACE_MONSTER and p_ptr.events[29017] > 0) then
		k = k + p_ptr.events[29017]
		p_ptr.events[29017] = 0
	end

	-- Ranger's Forestry ability!
        if ((p_ptr.abilities[(CLASS_RANGER * 10) + 2] >= 1) and (standing_on_forest())) then

                k = k + multiply_divide(k, p_ptr.abilities[(CLASS_RANGER * 10) + 2] * 10, 100)
        end

	if (current_weapon.disabled == 0) then k = multiply_divide(k, current_weapon.extra4, 100) end
        k = k * (tskill + 1)

	-- Stealthy Fighter
	if (stealthbonus > 0) then

		k = k + multiply_divide(k, stealthbonus, 100)
	end

	-- Rogue Weapons Mastery.
	if (p_ptr.abilities[(CLASS_ROGUE * 10) + 3] >= 1) then

		if (current_weapon.tval == TV_RANGED) then

			if (not(get_object_flag4(current_weapon, TR4_MUST2H))) then

				k = k + multiply_divide(k, p_ptr.abilities[(CLASS_ROGUE * 10) + 3] * 10, 100)
			end
		end
	end

	if (p_ptr.abilities[(CLASS_ENCHANTER * 10) + 6] >= 1 and get_object_flag4(current_weapon, TR4_CRAFTED)) then

		local dexbonus = (p_ptr.stat_ind[A_DEX+1] - 5)
		local usedint = (p_ptr.stat_ind[A_INT+1] - 5)
		if (usedint > (p_ptr.abilities[(CLASS_ENCHANTER * 10) + 6] * 5)) then

			usedint = (p_ptr.abilities[(CLASS_ENCHANTER * 10) + 6] * 5)
		end
		if (usedint > dexbonus) then

			k = k + multiply_divide(k, (inven(INVEN_AMMO).to_d + current_weapon.to_d + (usedint * 5)), 100)
			k = k + multiply_divide(k, usedint, 100)
		else

			k = k + multiply_divide(k, (inven(INVEN_AMMO).to_d + current_weapon.to_d + (dexbonus * 5)), 100)
			k = k + multiply_divide(k, dexbonus, 100)
		end
        else
		k = k + multiply_divide(k, (inven(INVEN_AMMO).to_d + current_weapon.to_d + ((p_ptr.stat_ind[A_DEX+1] - 5) * 5)), 100)
		k = k + multiply_divide(k, p_ptr.stat_ind[A_DEX+1], 100)
	end

	-- At high levels, crossbows hurts.
	if (current_weapon.itemtype == 2 and p_ptr.skill_base[21] >= 60) then k = k * 2 end

	-- Pistols Specialization.
	if (current_weapon.itemtype == 3 and p_ptr.abilities[(CLASS_GUNNER * 10) + 7] >= 1) then
		k = k + multiply_divide(k, (p_ptr.abilities[(CLASS_GUNNER * 10) + 7] * 10), 100)
	end

	-- Rifles Specialization.
	if (current_weapon.itemtype == 4 and p_ptr.abilities[(CLASS_GUNNER * 10) + 8] >= 1) then
		k = k + multiply_divide(k, (p_ptr.abilities[(CLASS_GUNNER * 10) + 8] * 10), 100)
	end

	-- Bard's War Songs ability.
	if (p_ptr.events[29042] == 1 and p_ptr.abilities[(CLASS_BARD * 10) + 8] > 0) then

		local chrbonus

		chrbonus = multiply_divide(p_ptr.stat_ind[A_CHR+1], p_ptr.abilities[(CLASS_BARD * 10) + 8] * 10, 100)
		k = k + multiply_divide(k, chrbonus, 100)
	end

	-- Marksman's Accurate Shots.
	if (p_ptr.abilities[(CLASS_MARKSMAN * 10) + 10] > 0) then

		k = k + multiply_divide(k, p_ptr.abilities[(CLASS_MARKSMAN * 10) + 10] * 10, 100)
	end

	return k;

end

function max_ranged_damages ()

	local k = 0
	local tskill = 0
	local xmight = 0
	local craftbonus = 0
	local craftmax = p_ptr.abilities[(CLASS_ENCHANTER * 10) + 1] * 5
	local stealthbonus
	local i

	craftbonus = p_ptr.skill[12]
	if (craftbonus > craftmax) then craftbonus = craftmax end

	if (not(get_object_flag4(current_weapon, TR4_CRAFTED))) then craftbonus = 0 end

	tskill = p_ptr.skill[3] + craftbonus

	-- Rogue's Stealthy Fighter ability.
	if (p_ptr.abilities[(CLASS_ROGUE * 10) + 1] >= 1) then

		local tmod

		if (p_ptr.abilities[(CLASS_ROGUE * 10) + 1] >= 10) then tmod = 100
		else tmod = p_ptr.abilities[(CLASS_ROGUE * 10) + 1] * 10
		end

		tskill = tskill + multiply_divide(p_ptr.skill[7], tmod, 100)

		i = p_ptr.abilities[(CLASS_ROGUE * 10) + 1]
		if (i > 10) then i = 10 end
		stealthbonus = multiply_divide(p_ptr.skill[7], i, 100)
		if (stealthbonus > 10) then stealthbonus = 10 end
		stealthbonus = stealthbonus * p_ptr.abilities[(CLASS_ROGUE * 10) + 1]
	end

	if (current_weapon.tval == TV_RANGED) then

		tskill = tskill + (p_ptr.skill[current_weapon.itemskill + 1] + (p_ptr.skill[current_weapon.itemskill + 1] / 2))
	else
		return 0
	end

	if (get_object_flag3(current_weapon, TR3_XTRA_MIGHT)) then xmight = 1 end

	-- If no ammos, damages is 0.
	if (inven(INVEN_AMMO).k_idx == 0) then
		return 0
	end

	if (inven(INVEN_AMMO).disabled == 0) then
        	k = maxroll(inven(INVEN_AMMO).dd, inven(INVEN_AMMO).ds)
	else
		k = 1
	end

	if (p_ptr.prace == RACE_MONSTER and p_ptr.events[29017] > 0) then
		k = k + p_ptr.events[29017]
		p_ptr.events[29017] = 0
	end

	-- Ranger's Forestry ability!
        if ((p_ptr.abilities[(CLASS_RANGER * 10) + 2] >= 1) and (standing_on_forest())) then

                k = k + multiply_divide(k, p_ptr.abilities[(CLASS_RANGER * 10) + 2] * 10, 100)
        end

	if (current_weapon.disabled == 0) then k = multiply_divide(k, current_weapon.extra4, 100) end
        k = k * (tskill + 1)

	-- Stealthy Fighter
	if (stealthbonus > 0) then

		k = k + multiply_divide(k, stealthbonus, 100)
	end

	-- Rogue Weapons Mastery.
	if (p_ptr.abilities[(CLASS_ROGUE * 10) + 3] >= 1) then

		if (current_weapon.tval == TV_RANGED) then

			if (not(get_object_flag4(current_weapon, TR4_MUST2H))) then

				k = k + multiply_divide(k, p_ptr.abilities[(CLASS_ROGUE * 10) + 3] * 10, 100)
			end
		end
	end

        if (p_ptr.abilities[(CLASS_ENCHANTER * 10) + 6] >= 1 and get_object_flag4(current_weapon, TR4_CRAFTED)) then

		local dexbonus = (p_ptr.stat_ind[A_DEX+1] - 5)
		local usedint = (p_ptr.stat_ind[A_INT+1] - 5)
		if (usedint > (p_ptr.abilities[(CLASS_ENCHANTER * 10) + 6] * 5)) then

			usedint = (p_ptr.abilities[(CLASS_ENCHANTER * 10) + 6] * 5)
		end
		if (usedint > dexbonus) then

			k = k + multiply_divide(k, (inven(INVEN_AMMO).to_d + current_weapon.to_d + (usedint * 5)), 100)
			k = k + multiply_divide(k, usedint, 100)
		else

			k = k + multiply_divide(k, (inven(INVEN_AMMO).to_d + current_weapon.to_d + (dexbonus * 5)), 100)
			k = k + multiply_divide(k, dexbonus, 100)
		end
        else
		k = k + multiply_divide(k, (inven(INVEN_AMMO).to_d + current_weapon.to_d + ((p_ptr.stat_ind[A_DEX+1] - 5) * 5)), 100)
		k = k + multiply_divide(k, p_ptr.stat_ind[A_DEX+1], 100)
	end

	-- At high levels, crossbows hurts.
	if (current_weapon.itemtype == 2 and p_ptr.skill_base[21] >= 60) then k = k * 2 end

	-- Pistols Specialization.
	if (current_weapon.itemtype == 3 and p_ptr.abilities[(CLASS_GUNNER * 10) + 7] >= 1) then
		k = k + multiply_divide(k, (p_ptr.abilities[(CLASS_GUNNER * 10) + 7] * 10), 100)
	end

	-- Rifles Specialization.
	if (current_weapon.itemtype == 4 and p_ptr.abilities[(CLASS_GUNNER * 10) + 8] >= 1) then
		k = k + multiply_divide(k, (p_ptr.abilities[(CLASS_GUNNER * 10) + 8] * 10), 100)
	end

	-- Bard's War Songs ability.
	if (p_ptr.events[29042] == 1 and p_ptr.abilities[(CLASS_BARD * 10) + 8] > 0) then

		local chrbonus

		chrbonus = multiply_divide(p_ptr.stat_ind[A_CHR+1], p_ptr.abilities[(CLASS_BARD * 10) + 8] * 10, 100)
		k = k + multiply_divide(k, chrbonus, 100)
	end

	-- Marksman's Accurate Shots.
	if (p_ptr.abilities[(CLASS_MARKSMAN * 10) + 10] > 0) then

		k = k + multiply_divide(k, p_ptr.abilities[(CLASS_MARKSMAN * 10) + 10] * 10, 100)
	end

	return k;

end

function min_ranged_damages ()

	local k = 0
	local tskill = 0
	local xmight = 0
	local craftbonus = 0
	local craftmax = p_ptr.abilities[(CLASS_ENCHANTER * 10) + 1] * 5
	local stealthbonus
	local i

	craftbonus = p_ptr.skill[12]
	if (craftbonus > craftmax) then craftbonus = craftmax end

	if (not(get_object_flag4(current_weapon, TR4_CRAFTED))) then craftbonus = 0 end

	tskill = p_ptr.skill[3] + craftbonus

	-- Rogue's Stealthy Fighter ability.
	if (p_ptr.abilities[(CLASS_ROGUE * 10) + 1] >= 1) then

		local tmod

		if (p_ptr.abilities[(CLASS_ROGUE * 10) + 1] >= 10) then tmod = 100
		else tmod = p_ptr.abilities[(CLASS_ROGUE * 10) + 1] * 10
		end

		tskill = tskill + multiply_divide(p_ptr.skill[7], tmod, 100)

		i = p_ptr.abilities[(CLASS_ROGUE * 10) + 1]
		if (i > 10) then i = 10 end
		stealthbonus = multiply_divide(p_ptr.skill[7], i, 100)
		if (stealthbonus > 10) then stealthbonus = 10 end
		stealthbonus = stealthbonus * p_ptr.abilities[(CLASS_ROGUE * 10) + 1]
	end

	if (current_weapon.tval == TV_RANGED) then

		tskill = tskill + (p_ptr.skill[current_weapon.itemskill + 1] + (p_ptr.skill[current_weapon.itemskill + 1] / 2))
	else
		return 0
	end

	if (get_object_flag3(current_weapon, TR3_XTRA_MIGHT)) then xmight = 1 end

	-- If no ammos, damages is 0.
	if (inven(INVEN_AMMO).k_idx == 0) then
		return 0
	end

	if (inven(INVEN_AMMO).disabled == 0) then
        	k = damroll(inven(INVEN_AMMO).dd, 1)
	else
		k = 1
	end

	if (p_ptr.prace == RACE_MONSTER and p_ptr.events[29017] > 0) then
		k = k + p_ptr.events[29017]
		p_ptr.events[29017] = 0
	end

	-- Ranger's Forestry ability!
        if ((p_ptr.abilities[(CLASS_RANGER * 10) + 2] >= 1) and (standing_on_forest())) then

                k = k + multiply_divide(k, p_ptr.abilities[(CLASS_RANGER * 10) + 2] * 10, 100)
        end

	if (current_weapon.disabled == 0) then k = multiply_divide(k, current_weapon.extra4, 100) end
        k = k * (tskill + 1)

	-- Stealthy Fighter
	if (stealthbonus > 0) then

		k = k + multiply_divide(k, stealthbonus, 100)
	end

	-- Rogue Weapons Mastery.
	if (p_ptr.abilities[(CLASS_ROGUE * 10) + 3] >= 1) then

		if (current_weapon.tval == TV_RANGED) then

			if (not(get_object_flag4(current_weapon, TR4_MUST2H))) then

				k = k + multiply_divide(k, p_ptr.abilities[(CLASS_ROGUE * 10) + 3] * 10, 100)
			end
		end
	end

        if (p_ptr.abilities[(CLASS_ENCHANTER * 10) + 6] >= 1 and get_object_flag4(current_weapon, TR4_CRAFTED)) then

		local dexbonus = (p_ptr.stat_ind[A_DEX+1] - 5)
		local usedint = (p_ptr.stat_ind[A_INT+1] - 5)
		if (usedint > (p_ptr.abilities[(CLASS_ENCHANTER * 10) + 6] * 5)) then

			usedint = (p_ptr.abilities[(CLASS_ENCHANTER * 10) + 6] * 5)
		end
		if (usedint > dexbonus) then

			k = k + multiply_divide(k, (inven(INVEN_AMMO).to_d + current_weapon.to_d + (usedint * 5)), 100)
			k = k + multiply_divide(k, usedint, 100)
		else

			k = k + multiply_divide(k, (inven(INVEN_AMMO).to_d + current_weapon.to_d + (dexbonus * 5)), 100)
			k = k + multiply_divide(k, dexbonus, 100)
		end
        else
		k = k + multiply_divide(k, (inven(INVEN_AMMO).to_d + current_weapon.to_d + ((p_ptr.stat_ind[A_DEX+1] - 5) * 5)), 100)
		k = k + multiply_divide(k, p_ptr.stat_ind[A_DEX+1], 100)
	end

	-- At high levels, crossbows hurts.
	if (current_weapon.itemtype == 2 and p_ptr.skill_base[21] >= 60) then k = k * 2 end

	-- Pistols Specialization.
	if (current_weapon.itemtype == 3 and p_ptr.abilities[(CLASS_GUNNER * 10) + 7] >= 1) then
		k = k + multiply_divide(k, (p_ptr.abilities[(CLASS_GUNNER * 10) + 7] * 10), 100)
	end

	-- Rifles Specialization.
	if (current_weapon.itemtype == 4 and p_ptr.abilities[(CLASS_GUNNER * 10) + 8] >= 1) then
		k = k + multiply_divide(k, (p_ptr.abilities[(CLASS_GUNNER * 10) + 8] * 10), 100)
	end

	-- Bard's War Songs ability.
	if (p_ptr.events[29042] == 1 and p_ptr.abilities[(CLASS_BARD * 10) + 8] > 0) then

		local chrbonus

		chrbonus = multiply_divide(p_ptr.stat_ind[A_CHR+1], p_ptr.abilities[(CLASS_BARD * 10) + 8] * 10, 100)
		k = k + multiply_divide(k, chrbonus, 100)
	end

	-- Marksman's Accurate Shots.
	if (p_ptr.abilities[(CLASS_MARKSMAN * 10) + 10] > 0) then

		k = k + multiply_divide(k, p_ptr.abilities[(CLASS_MARKSMAN * 10) + 10] * 10, 100)
	end

	return k;

end

function throw_damages ()

	local k = 0
	local tskill = 0
	local xmight = 0

	tskill = p_ptr.skill[3] + (p_ptr.skill[4] + (p_ptr.skill[4] / 2))

	if (drop_ranged.disabled == 0) then
        	k = damroll(drop_ranged.dd, drop_ranged.ds)
	else
		k = 1
	end
        k = k * (tskill + 1)
	k = k + multiply_divide(k, (((p_ptr.stat_ind[A_STR+1] * 5 + p_ptr.stat_ind[A_DEX+1] * 5)) + drop_ranged.to_d), 100)
	k = k + multiply_divide(k, (p_ptr.stat_ind[A_STR+1] + p_ptr.stat_ind[A_DEX+1]), 100)

	-- Power Throw feat.
	if (p_ptr.skill_base[4] >= 25) then k = k + (k / 2) end

	-- Bard's War Songs ability.
	if (p_ptr.events[29042] == 1 and p_ptr.abilities[(CLASS_BARD * 10) + 8] > 0) then

		local chrbonus

		chrbonus = multiply_divide(p_ptr.stat_ind[A_CHR+1], p_ptr.abilities[(CLASS_BARD * 10) + 8] * 10, 100)
		k = k + multiply_divide(k, chrbonus, 100)
	end

	return k;

end

function max_throw_damages ()

	local k = 0
	local tskill = 0
	local xmight = 0

	tskill = p_ptr.skill[3] + (p_ptr.skill[4] + (p_ptr.skill[4] / 2))

	if (drop_ranged.disabled == 0) then
        	k = maxroll(drop_ranged.dd, drop_ranged.ds)
	else
		k = 1
	end
        k = k * (tskill + 1)
        k = k + multiply_divide(k, (((p_ptr.stat_ind[A_STR+1] * 5 + p_ptr.stat_ind[A_DEX+1] * 5)) + drop_ranged.to_d), 100)
	k = k + multiply_divide(k, (p_ptr.stat_ind[A_STR+1] + p_ptr.stat_ind[A_DEX+1]), 100)

	-- Power Throw feat.
	if (p_ptr.skill_base[4] >= 25) then k = k + (k / 2) end

	-- Bard's War Songs ability.
	if (p_ptr.events[29042] == 1 and p_ptr.abilities[(CLASS_BARD * 10) + 8] > 0) then

		local chrbonus

		chrbonus = multiply_divide(p_ptr.stat_ind[A_CHR+1], p_ptr.abilities[(CLASS_BARD * 10) + 8] * 10, 100)
		k = k + multiply_divide(k, chrbonus, 100)
	end

	return k;

end

function min_throw_damages ()

	local k = 0
	local tskill = 0
	local xmight = 0

	tskill = p_ptr.skill[3] + (p_ptr.skill[4] + (p_ptr.skill[4] / 2))

	if (drop_ranged.disabled == 0) then
        	k = damroll(drop_ranged.dd, 1)
	else
		k = 1
	end
        k = k * (tskill + 1)
        k = k + multiply_divide(k, (((p_ptr.stat_ind[A_STR+1] * 5 + p_ptr.stat_ind[A_DEX+1] * 5)) + drop_ranged.to_d), 100)
	k = k + multiply_divide(k, (p_ptr.stat_ind[A_STR+1] + p_ptr.stat_ind[A_DEX+1]), 100)

	-- Power Throw feat.
	if (p_ptr.skill_base[4] >= 25) then k = k + (k / 2) end

	-- Bard's War Songs ability.
	if (p_ptr.events[29042] == 1 and p_ptr.abilities[(CLASS_BARD * 10) + 8] > 0) then

		local chrbonus

		chrbonus = multiply_divide(p_ptr.stat_ind[A_CHR+1], p_ptr.abilities[(CLASS_BARD * 10) + 8] * 10, 100)
		k = k + multiply_divide(k, chrbonus, 100)
	end

	return k;

end

add_event_handler("ranged_shoot", ranged_shoot)
add_event_handler("ranged_throw", ranged_throw)
add_event_handler("monster_ranged_attacks", monster_ranged_attacks)
add_event_handler("special_ranged_attacks", special_ranged_attacks)
add_event_handler("ranged_damages", ranged_damages)
add_event_handler("max_ranged_damages", max_ranged_damages)
add_event_handler("min_ranged_damages", min_ranged_damages)
add_event_handler("throw_damages", throw_damages)
add_event_handler("max_throw_damages", max_throw_damages)
add_event_handler("min_throw_damages", min_throw_damages)
