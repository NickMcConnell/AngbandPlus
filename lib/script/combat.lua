-- File: combat.lua
-- Contains the code for melee combat, damages formulas, etc...

function py_attack_execute (y, x, max_blow)

        local           num = 0
        local           k = 0
	local           m_name = ""
	local           fear = FALSE
	local           mdeath = FALSE
	local           backstab = FALSE
	local           stab_fleeing = FALSE
        local           weap
        local           totalcombo = 0
        local           maxcombo = 0
        local           oldpy = py
        local           oldpx = px
        local           ch
	local		noweapon = FALSE
	local		canattack = TRUE
	local           daggerbonus = 0
        local           usedcombo = FALSE
	local           blocked = FALSE
	local		tmp_r_idx = 0
	local		meleex = 0
	local		meleey = 0
	local		mtype = 0
	local		dam = 0

	-- Don't attack if using dashing shot
	if (dashingshot == 1) then return end

	-- Reset crushing blows.
	crushingblows = 0

	-- Reset normal attacks.
	normalattack = 0

        -- First and foremost, determine the maximum combos the
        -- player can do(if any)
        if (p_ptr.skill[8] >= 1) then

                local combatskill = p_ptr.skill[8]
                while (combatskill >= 5) do
                        maxcombo = maxcombo + 1
                        combatskill = combatskill - 5
                end
                do
                        local combochance = combatskill * 20

                        if (combochance > 0 and lua_randint(100) <= combochance) then
                                maxcombo = maxcombo + 1
                        end
                end
        end

	-- Disturb the player
	disturb(0, 0)

        if (p_ptr.skill_base[7] >= 30) then

		if ((monster(cave(y, x).m_idx).csleep > 0) and (monster(cave(y, x).m_idx).ml)) then

			-- Can't backstab creatures that we can't see, right?
			backstab = TRUE

		elseif ((monster(cave(y, x).m_idx).monfear > 0) and (monster(cave(y, x).m_idx).ml)) then
		
			stab_fleeing = TRUE
		end
	end

	-- Extract monster name (or "it")
	--m_name = get_monster_desc(monster(cave(y, x).m_idx), 0)
	--m_name = "The monster"
	if not (monster(cave(y, x).m_idx).ml) then
		m_name = "it"
	elseif (get_monster_flag1(monster(cave(y, x).m_idx).r_idx, RF1_UNIQUE)) then
		m_name = m_race(monster(cave(y, x).m_idx).r_idx).name_char
	else
		m_name = string.format('%s %s', "the", m_race(monster(cave(y, x).m_idx).r_idx).name_char)
	end

	-- Auto-Recall if possible and visible
	if (monster(cave(y, x).m_idx).ml) then monster_race_track(monster(cave(y, x).m_idx).r_idx) end

	-- Track a new monster
	if (monster(cave(y, x).m_idx).ml) then health_track(cave(y, x).m_idx) end

	-- Willingly attacking a friend makes it angry!
	if (is_pet(monster(cave(y, x).m_idx)) and (p_ptr.stun == 0 and p_ptr.confused == 0 and p_ptr.image == 0)) then
	
		msg_print(string.format('%s gets angry!', m_name))
		set_pet(monster(cave(y, x).m_idx), FALSE)
		monster(cave(y, x).m_idx).angered_pet = 1
	end

	-- Handle player fear
	if (p_ptr.afraid > 0) then

		-- Message
		if (monster(cave(y, x).m_idx).ml) then
			msg_print(string.format('You are too afraid to attack %s', m_name))
		else
			msg_print("There is something scary in your way!")

		end

		-- Done
		canattack = FALSE
	end
	
        if (canattack == TRUE) then
        -- Attack with first weapon, then with second weapon

        for weap = 0, p_ptr.dualwield do

        -- Monster is already dead ? oh :(
        if (mdeath) then break end

        -- Reset the blows counter
        num = 0

	-- Access the weapon
	current_weapon = inven(INVEN_WIELD + weap)

	-- Break if no second weapons!
	if (weap == 1) then
		if (inven(INVEN_WIELD + weap).tval == 0 and not(unarmed())) then break end
	end


        if(not(get_object_flag4(inven(INVEN_WIELD + weap), TR4_NEVER_BLOW))) then

        local num_blow

	if (weap == 0) then num_blow = p_ptr.num_blow
	else num_blow = p_ptr.num_blow2
	end

        -- Restrict to max_blow(if max_blow >= 0)
        if((max_blow >= 0) and (num_blow > max_blow)) then num_blow = max_blow end

	-- Attack once for each legal blow
        while (num < num_blow and cave(y, x).m_idx > 0 and px == oldpx and py == oldpy) do

                daggerbonus = 0
                usedcombo = 0
		blocked = FALSE

		num = num + 1
		current_weapon = inven(INVEN_WIELD + weap) -- This is from Adelie.

		-- Run a script?
		player_before_melee()

                -- Maybe the player want to use a special move instead...
                -- Offer the choice...if possible.
                if (totalcombo < maxcombo and mdeath == FALSE) then

			local comboleft

			comboleft = string.format('Use an ability? (%d feats left, attack %d/%d.) [y/n/no[t] this turn]', maxcombo - totalcombo, num + (p_ptr.num_blow * weap), p_ptr.num_blow + (p_ptr.num_blow2 * p_ptr.dualwield))

			msg_print(NULL)

  			msg_print(comboleft)

			ch = inkey()

			-- Characters 89 and 121 are "Y" and "y"
                        if (ch == 89 or ch == 121) then

				combatfeat = TRUE
                                do_cmd_racial_power(1)
                                usedcombo = 1
                                totalcombo = totalcombo + 1
				combatfeat = FALSE
                        end

			-- Characters "T" and "t"
                        if (ch == 84 or ch == 116) then

                                totalcombo = maxcombo
                        end

                end


                if (usedcombo == 0) then

			meleex = monster(cave(y, x).m_idx).fx
			meleey = monster(cave(y, x).m_idx).fy
			melee_attack = TRUE

			-- This is a normal attack.
			normalattack = 1

			if (unarmed()) then

				if (inven(INVEN_HANDS).extra1 == 0) then

					if (p_ptr.prace == RACE_MONSTER) then

						if (p_ptr.events[29028] == 1 and inven(INVEN_ESSENCE).tval > 0) then

							mtype = inven(INVEN_ESSENCE).extra1
						else

							mtype = m_race(p_ptr.body_monster).attack[p_ptr.events[29020]+1].element
						end
					else
						mtype = GF_PHYSICAL
					end
				else
					mtype = inven(INVEN_HANDS).extra1
				end

				dam = monk_damages()
				if (backstab) then

					backstab = FALSE
                                        dam = dam * (3 + p_ptr.abilities[(CLASS_ROGUE * 10) + 2])
				elseif (stab_fleeing) then

                                        dam = dam * (3 + p_ptr.abilities[(CLASS_ROGUE * 10) + 2]) / 2
				end

                                -- No negative damage
                                if (dam < 0) then dam = 0 end
			else
				if (inven(INVEN_WIELD + weap).extra1 == 0) then mtype = GF_PHYSICAL
				else mtype = inven(INVEN_WIELD + weap).extra1
				end
				dam = weapon_damages()
				if (backstab) then

					backstab = FALSE
                                        dam = dam * (3 + p_ptr.abilities[(CLASS_ROGUE * 10) + 2])
				elseif (stab_fleeing) then

                                        dam = dam * (3 + p_ptr.abilities[(CLASS_ROGUE * 10) + 2]) / 2
				end

                                -- No negative damage
                                if (dam < 0) then dam = 0 end
			end
			
			no_magic_return = FALSE
			nevermiss = FALSE
			fire_jump_ball(mtype, dam, 0, meleex, meleey, FALSE)
			melee_attack = FALSE

			-- Run a script?
			player_after_melee(dam)

			update_and_handle()

		-- End of combo checks
                end
		
	end

        else

                msg_print("You can't attack with that weapon.")
        end
        end
        end
	
	--if (fear and monster(cave(y, x).m_idx).ml) then
		-- Sound
		--sound(SOUND_FLEE);

		-- Message
		--msg_print(string.format('%s flees in terror!', m_name))
	--end

end

-- Damages by weapons
function weapon_damages()
        local k = 0
	local tskill = 0
	local craftbonus = 0
	local craftmax = p_ptr.abilities[(CLASS_ENCHANTER * 10) + 1] * 5
	local intfight = 0
	local usedstat = 0

	craftbonus = p_ptr.skill[12]
	if (craftbonus > craftmax) then craftbonus = craftmax end

	if (not(get_object_flag4(current_weapon, TR4_CRAFTED))) then craftbonus = 0 end

	tskill = p_ptr.skill[1] + craftbonus

	-- Defensive Strike!
	if (defensive_strike == 1) then tskill = tskill + p_ptr.skill[5] end

	-- Stealth Attack!
	if (stealth_attack == 1) then tskill = tskill + p_ptr.skill[7] end

	if ((current_weapon.tval == TV_WEAPON or current_weapon.tval == TV_ROD) and not(current_weapon.itemskill == 0)) then

		tskill = tskill + (p_ptr.skill[current_weapon.itemskill + 1] + (p_ptr.skill[current_weapon.itemskill + 1] / 2))
	end

        k = damroll(current_weapon.dd, current_weapon.ds)
	k = k + ((k * (p_ptr.abilities[(CLASS_FIGHTER * 10) + 1] * 10)) / 100)

	-- Ranger's Forestry ability!
        if ((p_ptr.abilities[(CLASS_RANGER * 10) + 2] >= 1) and (standing_on_forest())) then

                k = k + multiply_divide(k, p_ptr.abilities[(CLASS_RANGER * 10) + 2] * 10, 100)
        end

	if (current_weapon.itemskill == 14 and (p_ptr.skill_base[15] >= 90)) then k = k * 2 end

	-- Bonus damages.
	if (p_ptr.events[29017] > 0) then

		k = k + p_ptr.events[29017]
		p_ptr.events[29017] = 0
	end

        k = k * (tskill + 1)
	k = k + multiply_divide(k, p_ptr.dis_to_d, 100)
	if (get_object_flag4(current_weapon, TR4_COULD2H) and ((inven(INVEN_WIELD).tval == 0) or (inven(INVEN_WIELD+1).tval == 0))) then k = k + (k / 3) end

	-- An additional boost from strength.
	usedstat = (p_ptr.stat_ind[A_STR+1] - 5)

	-- We might use another stat instead.
	if (p_ptr.abilities[(CLASS_ENCHANTER * 10) + 6] >= 1 and get_object_flag4(current_weapon, TR4_CRAFTED)) then

		local strbonus = (p_ptr.stat_ind[A_STR+1] - 5)
		local usedint = (p_ptr.stat_ind[A_INT+1] - 5)
		if (usedint > (p_ptr.abilities[(CLASS_ENCHANTER * 10) + 6] * 5)) then

			usedint = (p_ptr.abilities[(CLASS_ENCHANTER * 10) + 6] * 5)
		end
		if (usedint > strbonus) then

			usedstat = (p_ptr.stat_ind[A_INT+1] - 5)
			intfight = 1
		else

			usedstat = (p_ptr.stat_ind[A_STR+1] - 5)
		end
	end

        if (p_ptr.abilities[(CLASS_RANGER * 10) + 7] >= 1 and intfight == 0) then

		local strbonus = (p_ptr.stat_ind[A_STR+1] - 5)
		local useddex = (p_ptr.stat_ind[A_DEX+1] - 5)
		if (useddex > (p_ptr.abilities[(CLASS_RANGER * 10) + 7] * 5)) then

			useddex = (p_ptr.abilities[(CLASS_RANGER * 10) + 7] * 5)
		end
		if (useddex > strbonus) then

			usedstat = (p_ptr.stat_ind[A_DEX+1] - 5)
		else

			usedstat = (p_ptr.stat_ind[A_STR+1] - 5)
		end
	end

	k = k + multiply_divide(k, usedstat, 100)

	if (p_ptr.powerattack > 0) then

		if (p_ptr.powerlevel == 1) then k = k * (2 + ((2 * (p_ptr.abilities[(CLASS_FIGHTER * 10)+1] * 5)) / 100))
		elseif (p_ptr.powerlevel == 2) then k = k * (3 + ((3 * (p_ptr.abilities[(CLASS_FIGHTER * 10)+1] * 5)) / 100))
		elseif (p_ptr.powerlevel == 3) then k = k * (4 + ((4 * (p_ptr.abilities[(CLASS_FIGHTER * 10)+1] * 5)) / 100))
		end
		if (p_ptr.prace == RACE_MONSTER and k > 0 and get_player_monster_ability(BOSS_CURSED_HITS)) then

			if (p_ptr.powerlevel == 1) then crushingblows = 2 end
			if (p_ptr.powerlevel == 2) then crushingblows = 3 end
			if (p_ptr.powerlevel == 3) then crushingblows = 4 end
		end
		if (p_ptr.abilities[(CLASS_FIGHTER * 10) + 4] > 0) then

			if (p_ptr.powerlevel == 1) then critmod = (2 + ((2 * (p_ptr.abilities[(CLASS_FIGHTER * 10)+1] * 5)) / 100))
			elseif (p_ptr.powerlevel == 2) then critmod = (3 + ((3 * (p_ptr.abilities[(CLASS_FIGHTER * 10)+1] * 5)) / 100))
			elseif (p_ptr.powerlevel == 3) then critmod = (4 + ((4 * (p_ptr.abilities[(CLASS_FIGHTER * 10)+1] * 5)) / 100))
			end
		end
		set_powerattack(0)
		p_ptr.str_boost = 0
		p_ptr.str_boost_dur = 0
		update_and_handle()
	end

	-- Bard's War Songs ability.
	if (p_ptr.events[29042] == 1 and p_ptr.abilities[(CLASS_BARD * 10) + 8] > 0) then

		local chrbonus

		chrbonus = multiply_divide(p_ptr.stat_ind[A_CHR+1], p_ptr.abilities[(CLASS_BARD * 10) + 8] * 10, 100)
		k = k + multiply_divide(k, chrbonus, 100)
	end

	-- Weapons Mastery.
	if (p_ptr.abilities[(CLASS_WARRIOR * 10) + 10] > 0) then

		k = k + multiply_divide(k, p_ptr.abilities[(CLASS_WARRIOR * 10) + 10] * 10, 100)
	end

        return k
end

-- A function used to calculate monk damages
function monk_damages()

        local k
        local mdice
	local mside
	local tskill
	local glovebonus
	local intfight = 0
	local usedstat = 0

        mdice = (p_ptr.skill[19] / 15) + (p_ptr.skill[1] / 40) + 1
        mside = (p_ptr.skill[19] / 15) + (p_ptr.skill[1] / 40) + 3

	-- Damages bonus from gloves!
	glovebonus = 0
	if (inven(INVEN_HANDS).tval ~= 0 and inven(INVEN_HANDS).ac > 0) then
		glovebonus = lua_randint(inven(INVEN_HANDS).ac)
	end
	
        -- For Monsters, it's different.
	if (p_ptr.prace == RACE_MONSTER) then

		-- Use the main essence if enabled.
		if (p_ptr.events[29028] == 1 and inven(INVEN_ESSENCE).tval > 0) then

			k = damroll(inven(INVEN_ESSENCE).dd, inven(INVEN_ESSENCE).ds)
			if (k <= 0) then k = 1 end
		else

			k = damroll(m_race(p_ptr.body_monster).attack[p_ptr.events[29020]+1].ddice, m_race(p_ptr.body_monster).attack[p_ptr.events[29020]+1].dside)
			if (k <= 0) then k = 1 end
		end

		-- 1% increase per MArtial Arts points.
		k = k + multiply_divide(k, p_ptr.skill[19], 100)
		
	else

        	k = damroll(mdice, mside)
	end

	k = k + ((k * (p_ptr.abilities[(CLASS_FIGHTER * 10) + 1] * 10)) / 100)

	-- Ranger's Forestry ability!
        if ((p_ptr.abilities[(CLASS_RANGER * 10) + 2] >= 1) and (standing_on_forest())) then

                k = k + multiply_divide(k, p_ptr.abilities[(CLASS_RANGER * 10) + 2] * 10, 100)
        end

	-- Bonus damages.
	if (p_ptr.events[29017] > 0) then

		k = k + p_ptr.events[29017]
		p_ptr.events[29017] = 0
	end

	k = k + ((p_ptr.skill[1] * (p_ptr.abilities[(CLASS_FIGHTER * 10) + 5] * 10)) / 100)
	k = k + glovebonus
	tskill = p_ptr.skill[1]
	tskill = tskill + (p_ptr.skill[19] + (p_ptr.skill[19] / 2))
        k = k * (tskill + 1)
	k = k + multiply_divide(k, p_ptr.dis_to_d, 100)

	-- An additional boost from strength.
	usedstat = (p_ptr.stat_ind[A_STR+1] - 5)

	-- We might use another stat instead.
	if (p_ptr.abilities[(CLASS_ENCHANTER * 10) + 6] >= 1 and get_object_flag4(inven(INVEN_HANDS), TR4_CRAFTED)) then

		local strbonus = (p_ptr.stat_ind[A_STR+1] - 5)
		local usedint = (p_ptr.stat_ind[A_INT+1] - 5)
		if (usedint > (p_ptr.abilities[(CLASS_ENCHANTER * 10) + 6] * 5)) then

			usedint = (p_ptr.abilities[(CLASS_ENCHANTER * 10) + 6] * 5)
		end
		if (usedint > strbonus) then

			usedstat = (p_ptr.stat_ind[A_INT+1] - 5)
			intfight = 1
		else

			usedstat = (p_ptr.stat_ind[A_STR+1] - 5)
		end
	end

        if (p_ptr.abilities[(CLASS_RANGER * 10) + 7] >= 1 and intfight == 0) then

		local strbonus = (p_ptr.stat_ind[A_STR+1] - 5)
		local useddex = (p_ptr.stat_ind[A_DEX+1] - 5)
		if (useddex > (p_ptr.abilities[(CLASS_RANGER * 10) + 7] * 5)) then

			useddex = (p_ptr.abilities[(CLASS_RANGER * 10) + 7] * 5)
		end
		if (useddex > strbonus) then

			usedstat = (p_ptr.stat_ind[A_DEX+1] - 5)
		else

			usedstat = (p_ptr.stat_ind[A_STR+1] - 5)
		end
	end

	k = k + multiply_divide(k, usedstat, 100)

	if (p_ptr.powerattack > 0) then

		local chance_no_end

		chance_no_end = 0

		if (p_ptr.powerlevel == 1) then k = k * (2 + ((2 * (p_ptr.abilities[(CLASS_FIGHTER * 10)+1] * 5)) / 100))
		elseif (p_ptr.powerlevel == 2) then k = k * (3 + ((3 * (p_ptr.abilities[(CLASS_FIGHTER * 10)+1] * 5)) / 100))
		elseif (p_ptr.powerlevel == 3) then k = k * (4 + ((4 * (p_ptr.abilities[(CLASS_FIGHTER * 10)+1] * 5)) / 100))
		end

		if (p_ptr.abilities[(CLASS_FIGHTER * 10) + 10] > 0) then

			if (p_ptr.powerlevel == 1) then chance_no_end = p_ptr.abilities[(CLASS_FIGHTER * 10) + 10] * 5
			elseif (p_ptr.powerlevel == 2) then chance_no_end = p_ptr.abilities[(CLASS_FIGHTER * 10) + 10] * 3
			elseif (p_ptr.powerlevel == 3) then chance_no_end = p_ptr.abilities[(CLASS_FIGHTER * 10) + 10]
			end
		end

		if (p_ptr.prace == RACE_MONSTER and k > 0 and get_player_monster_ability(BOSS_CURSED_HITS)) then

			if (p_ptr.powerlevel == 1) then crushingblows = 2 end
			if (p_ptr.powerlevel == 2) then crushingblows = 3 end
			if (p_ptr.powerlevel == 3) then crushingblows = 4 end
		end
		if (lua_randint(100) > chance_no_end) then
			set_powerattack(0)
			p_ptr.str_boost = 0
			p_ptr.str_boost_dur = 0
		end
		update_and_handle()
	end

	-- Bard's War Songs ability.
	if (p_ptr.events[29042] == 1 and p_ptr.abilities[(CLASS_BARD * 10) + 8] > 0) then

		local chrbonus

		chrbonus = multiply_divide(p_ptr.stat_ind[A_CHR+1], p_ptr.abilities[(CLASS_BARD * 10) + 8] * 10, 100)
		k = k + multiply_divide(k, chrbonus, 100)
	end

	return k
end

-- Return minimum weapon damages. Used by files.c to display damages.
-- Could be used for abilities as well.
function min_weapon_damages()

        local k
	local tskill
	local craftbonus = 0
	local craftmax = p_ptr.abilities[(CLASS_ENCHANTER * 10) + 1] * 5
	local intfight = 0
	local usedstat = 0

	craftbonus = p_ptr.skill[12]
	if (craftbonus > craftmax) then craftbonus = craftmax end

	if (not(get_object_flag4(current_weapon, TR4_CRAFTED))) then craftbonus = 0 end

	tskill = p_ptr.skill[1] + craftbonus

	-- Defensive Strike!
	if (defensive_strike == 1) then tskill = tskill + p_ptr.skill[5] end

	-- Stealth Attack!
	if (stealth_attack == 1) then tskill = tskill + p_ptr.skill[7] end

	if ((current_weapon.tval == TV_WEAPON or current_weapon.tval == TV_ROD) and not(current_weapon.itemskill == 0)) then

		tskill = tskill + (p_ptr.skill[current_weapon.itemskill + 1] + (p_ptr.skill[current_weapon.itemskill + 1] / 2))
	end

        k = damroll(current_weapon.dd, 1)
	k = k + ((k * (p_ptr.abilities[(CLASS_FIGHTER * 10) + 1] * 10)) / 100)

	-- Ranger's Forestry ability!
        if ((p_ptr.abilities[(CLASS_RANGER * 10) + 2] >= 1) and (standing_on_forest())) then

                k = k + multiply_divide(k, p_ptr.abilities[(CLASS_RANGER * 10) + 2] * 10, 100)
        end

	if (current_weapon.itemskill == 14 and (p_ptr.skill_base[15] >= 90)) then k = k * 2 end

	-- Bonus damages.
	if (p_ptr.events[29017] > 0) then

		k = k + p_ptr.events[29017]
		p_ptr.events[29017] = 0
	end

        k = k * (tskill + 1)
	k = k + multiply_divide(k, p_ptr.dis_to_d, 100)
	if (get_object_flag4(current_weapon, TR4_COULD2H) and ((inven(INVEN_WIELD).tval == 0) or (inven(INVEN_WIELD+1).tval == 0))) then k = k + (k / 3) end

	-- An additional boost from strength.
	usedstat = (p_ptr.stat_ind[A_STR+1] - 5)

	-- We might use another stat instead.
	if (p_ptr.abilities[(CLASS_ENCHANTER * 10) + 6] >= 1 and get_object_flag4(current_weapon, TR4_CRAFTED)) then

		local strbonus = (p_ptr.stat_ind[A_STR+1] - 5)
		local usedint = (p_ptr.stat_ind[A_INT+1] - 5)
		if (usedint > (p_ptr.abilities[(CLASS_ENCHANTER * 10) + 6] * 5)) then

			usedint = (p_ptr.abilities[(CLASS_ENCHANTER * 10) + 6] * 5)
		end
		if (usedint > strbonus) then

			usedstat = (p_ptr.stat_ind[A_INT+1] - 5)
			intfight = 1
		else

			usedstat = (p_ptr.stat_ind[A_STR+1] - 5)
		end
	end

        if (p_ptr.abilities[(CLASS_RANGER * 10) + 7] >= 1 and intfight == 0) then

		local strbonus = (p_ptr.stat_ind[A_STR+1] - 5)
		local useddex = (p_ptr.stat_ind[A_DEX+1] - 5)
		if (useddex > (p_ptr.abilities[(CLASS_RANGER * 10) + 7] * 5)) then

			useddex = (p_ptr.abilities[(CLASS_RANGER * 10) + 7] * 5)
		end
		if (useddex > strbonus) then

			usedstat = (p_ptr.stat_ind[A_DEX+1] - 5)
		else

			usedstat = (p_ptr.stat_ind[A_STR+1] - 5)
		end
	end

	k = k + multiply_divide(k, usedstat, 100)

	if (p_ptr.powerattack > 0) then

		if (p_ptr.powerlevel == 1) then k = k * (2 + ((2 * (p_ptr.abilities[(CLASS_FIGHTER * 10)+1] * 5)) / 100))
		elseif (p_ptr.powerlevel == 2) then k = k * (3 + ((3 * (p_ptr.abilities[(CLASS_FIGHTER * 10)+1] * 5)) / 100))
		elseif (p_ptr.powerlevel == 3) then k = k * (4 + ((4 * (p_ptr.abilities[(CLASS_FIGHTER * 10)+1] * 5)) / 100))
		end
	end

	-- Bard's War Songs ability.
	if (p_ptr.events[29042] == 1 and p_ptr.abilities[(CLASS_BARD * 10) + 8] > 0) then

		local chrbonus

		chrbonus = multiply_divide(p_ptr.stat_ind[A_CHR+1], p_ptr.abilities[(CLASS_BARD * 10) + 8] * 10, 100)
		k = k + multiply_divide(k, chrbonus, 100)
	end

	-- Weapons Mastery.
	if (p_ptr.abilities[(CLASS_WARRIOR * 10) + 10] > 0) then

		k = k + multiply_divide(k, p_ptr.abilities[(CLASS_WARRIOR * 10) + 10] * 10, 100)
	end

        return k
end

-- Return maximum weapon damages. Used by files.c to display damages.
-- Could be used for abilities as well.
function max_weapon_damages()

        local k
	local tskill
	local craftbonus = 0
	local craftmax = p_ptr.abilities[(CLASS_ENCHANTER * 10) + 1] * 5
	local intfight = 0
	local usedstat = 0

	craftbonus = p_ptr.skill[12]
	if (craftbonus > craftmax) then craftbonus = craftmax end

	if (not(get_object_flag4(current_weapon, TR4_CRAFTED))) then craftbonus = 0 end

	tskill = p_ptr.skill[1] + craftbonus

	-- Defensive Strike!
	if (defensive_strike == 1) then tskill = tskill + p_ptr.skill[5] end

	-- Stealth Attack!
	if (stealth_attack == 1) then tskill = tskill + p_ptr.skill[7] end

	if ((current_weapon.tval == TV_WEAPON or current_weapon.tval == TV_ROD) and not(current_weapon.itemskill == 0)) then

		tskill = tskill + (p_ptr.skill[current_weapon.itemskill + 1] + (p_ptr.skill[current_weapon.itemskill + 1] / 2))
	end

        k = maxroll(current_weapon.dd, current_weapon.ds)
	k = k + ((k * (p_ptr.abilities[(CLASS_FIGHTER * 10) + 1] * 10)) / 100)

	-- Ranger's Forestry ability!
        if ((p_ptr.abilities[(CLASS_RANGER * 10) + 2] >= 1) and (standing_on_forest())) then

                k = k + multiply_divide(k, p_ptr.abilities[(CLASS_RANGER * 10) + 2] * 10, 100)
        end

	if (current_weapon.itemskill == 14 and (p_ptr.skill_base[15] >= 90)) then k = k * 2 end

	-- Bonus damages.
	if (p_ptr.events[29017] > 0) then

		k = k + p_ptr.events[29017]
		p_ptr.events[29017] = 0
	end

        k = k * (tskill + 1)
	k = k + multiply_divide(k, p_ptr.dis_to_d, 100)
	if (get_object_flag4(current_weapon, TR4_COULD2H) and ((inven(INVEN_WIELD).tval == 0) or (inven(INVEN_WIELD+1).tval == 0))) then k = k + (k / 3) end

	-- An additional boost from strength.
	usedstat = (p_ptr.stat_ind[A_STR+1] - 5)

	-- We might use another stat instead.
	if (p_ptr.abilities[(CLASS_ENCHANTER * 10) + 6] >= 1 and get_object_flag4(current_weapon, TR4_CRAFTED)) then

		local strbonus = (p_ptr.stat_ind[A_STR+1] - 5)
		local usedint = (p_ptr.stat_ind[A_INT+1] - 5)
		if (usedint > (p_ptr.abilities[(CLASS_ENCHANTER * 10) + 6] * 5)) then

			usedint = (p_ptr.abilities[(CLASS_ENCHANTER * 10) + 6] * 5)
		end
		if (usedint > strbonus) then

			usedstat = (p_ptr.stat_ind[A_INT+1] - 5)
			intfight = 1
		else

			usedstat = (p_ptr.stat_ind[A_STR+1] - 5)
		end
	end

        if (p_ptr.abilities[(CLASS_RANGER * 10) + 7] >= 1 and intfight == 0) then

		local strbonus = (p_ptr.stat_ind[A_STR+1] - 5)
		local useddex = (p_ptr.stat_ind[A_DEX+1] - 5)
		if (useddex > (p_ptr.abilities[(CLASS_RANGER * 10) + 7] * 5)) then

			useddex = (p_ptr.abilities[(CLASS_RANGER * 10) + 7] * 5)
		end
		if (useddex > strbonus) then

			usedstat = (p_ptr.stat_ind[A_DEX+1] - 5)
		else

			usedstat = (p_ptr.stat_ind[A_STR+1] - 5)
		end
	end

	k = k + multiply_divide(k, usedstat, 100)

	if (p_ptr.powerattack > 0) then

		if (p_ptr.powerlevel == 1) then k = k * (2 + ((2 * (p_ptr.abilities[(CLASS_FIGHTER * 10)+1] * 5)) / 100))
		elseif (p_ptr.powerlevel == 2) then k = k * (3 + ((3 * (p_ptr.abilities[(CLASS_FIGHTER * 10)+1] * 5)) / 100))
		elseif (p_ptr.powerlevel == 3) then k = k * (4 + ((4 * (p_ptr.abilities[(CLASS_FIGHTER * 10)+1] * 5)) / 100))
		end
	end

	-- Bard's War Songs ability.
	if (p_ptr.events[29042] == 1 and p_ptr.abilities[(CLASS_BARD * 10) + 8] > 0) then

		local chrbonus

		chrbonus = multiply_divide(p_ptr.stat_ind[A_CHR+1], p_ptr.abilities[(CLASS_BARD * 10) + 8] * 10, 100)
		k = k + multiply_divide(k, chrbonus, 100)
	end

	-- Weapons Mastery.
	if (p_ptr.abilities[(CLASS_WARRIOR * 10) + 10] > 0) then

		k = k + multiply_divide(k, p_ptr.abilities[(CLASS_WARRIOR * 10) + 10] * 10, 100)
	end

        return k
end

-- A function used to calculate minimum monk damages
function min_monk_damages()

        local k
        local mdice
	local mside
	local tskill
	local glovebonus
	local intfight = 0
	local usedstat = 0

        mdice = (p_ptr.skill[19] / 15) + (p_ptr.skill[1] / 40) + 1
        mside = (p_ptr.skill[19] / 15) + (p_ptr.skill[1] / 40) + 3

	-- Damages bonus from gloves!
	glovebonus = 0
	if (inven(INVEN_HANDS).tval ~= 0 and inven(INVEN_HANDS).ac > 0) then
		glovebonus = 1
	end

	-- For Monsters, it's different.
	if (p_ptr.prace == RACE_MONSTER) then

		-- Use the main essence if enabled.
		if (p_ptr.events[29028] == 1 and inven(INVEN_ESSENCE).tval > 0) then

			k = damroll(inven(INVEN_ESSENCE).dd, 1)
			if (k <= 0) then k = 1 end
		else

			k = damroll(m_race(p_ptr.body_monster).attack[p_ptr.events[29020]+1].ddice, 1)
			if (k <= 0) then k = 1 end
		end

		-- 1% increase per MArtial Arts points.
		k = k + multiply_divide(k, p_ptr.skill[19], 100)
		
	else

        	k = damroll(mdice, 1)
	end

	k = k + ((k * (p_ptr.abilities[(CLASS_FIGHTER * 10) + 1] * 10)) / 100)

	-- Ranger's Forestry ability!
        if ((p_ptr.abilities[(CLASS_RANGER * 10) + 2] >= 1) and (standing_on_forest())) then

                k = k + multiply_divide(k, p_ptr.abilities[(CLASS_RANGER * 10) + 2] * 10, 100)
        end

	-- Bonus damages.
	if (p_ptr.events[29017] > 0) then

		k = k + p_ptr.events[29017]
		p_ptr.events[29017] = 0
	end

	k = k + ((p_ptr.skill[1] * (p_ptr.abilities[(CLASS_FIGHTER * 10) + 5] * 10)) / 100)
	k = k + glovebonus
	tskill = p_ptr.skill[1]
	tskill = tskill + (p_ptr.skill[19] + (p_ptr.skill[19] / 2))
        k = k * (tskill + 1)
	k = k + multiply_divide(k, p_ptr.dis_to_d, 100)

	-- An additional boost from strength.
	usedstat = (p_ptr.stat_ind[A_STR+1] - 5)

	-- We might use another stat instead.
	if (p_ptr.abilities[(CLASS_ENCHANTER * 10) + 6] >= 1 and get_object_flag4(inven(INVEN_HANDS), TR4_CRAFTED)) then

		local strbonus = (p_ptr.stat_ind[A_STR+1] - 5)
		local usedint = (p_ptr.stat_ind[A_INT+1] - 5)
		if (usedint > (p_ptr.abilities[(CLASS_ENCHANTER * 10) + 6] * 5)) then

			usedint = (p_ptr.abilities[(CLASS_ENCHANTER * 10) + 6] * 5)
		end
		if (usedint > strbonus) then

			usedstat = (p_ptr.stat_ind[A_INT+1] - 5)
			intfight = 1
		else

			usedstat = (p_ptr.stat_ind[A_STR+1] - 5)
		end
	end

        if (p_ptr.abilities[(CLASS_RANGER * 10) + 7] >= 1 and intfight == 0) then

		local strbonus = (p_ptr.stat_ind[A_STR+1] - 5)
		local useddex = (p_ptr.stat_ind[A_DEX+1] - 5)
		if (useddex > (p_ptr.abilities[(CLASS_RANGER * 10) + 7] * 5)) then

			useddex = (p_ptr.abilities[(CLASS_RANGER * 10) + 7] * 5)
		end
		if (useddex > strbonus) then

			usedstat = (p_ptr.stat_ind[A_DEX+1] - 5)
		else

			usedstat = (p_ptr.stat_ind[A_STR+1] - 5)
		end
	end

	k = k + multiply_divide(k, usedstat, 100)

	if (p_ptr.powerattack > 0) then

		if (p_ptr.powerlevel == 1) then k = k * (2 + ((2 * (p_ptr.abilities[(CLASS_FIGHTER * 10)+1] * 5)) / 100))
		elseif (p_ptr.powerlevel == 2) then k = k * (3 + ((3 * (p_ptr.abilities[(CLASS_FIGHTER * 10)+1] * 5)) / 100))
		elseif (p_ptr.powerlevel == 3) then k = k * (4 + ((4 * (p_ptr.abilities[(CLASS_FIGHTER * 10)+1] * 5)) / 100))
		end
	end

	-- Bard's War Songs ability.
	if (p_ptr.events[29042] == 1 and p_ptr.abilities[(CLASS_BARD * 10) + 8] > 0) then

		local chrbonus

		chrbonus = multiply_divide(p_ptr.stat_ind[A_CHR+1], p_ptr.abilities[(CLASS_BARD * 10) + 8] * 10, 100)
		k = k + multiply_divide(k, chrbonus, 100)
	end

        return k
end

-- A function used to calculate maximum monk damages
function max_monk_damages()

        local k
        local mdice
	local mside
	local tskill
	local glovebonus
	local intfight = 0
	local usedstat = 0

	mdice = (p_ptr.skill[19] / 15) + (p_ptr.skill[1] / 40) + 1
        mside = (p_ptr.skill[19] / 15) + (p_ptr.skill[1] / 40) + 3

	-- Damages bonus from gloves!
	glovebonus = 0
	if (inven(INVEN_HANDS).tval ~= 0 and inven(INVEN_HANDS).ac > 0) then
		glovebonus = inven(INVEN_HANDS).ac
	end

	-- For Monsters, it's different.
	if (p_ptr.prace == RACE_MONSTER) then

		-- Use the main essence if enabled.
		if (p_ptr.events[29028] == 1 and inven(INVEN_ESSENCE).tval > 0) then

			k = maxroll(inven(INVEN_ESSENCE).dd, inven(INVEN_ESSENCE).ds)
			if (k <= 0) then k = 1 end
		else

			k = maxroll(m_race(p_ptr.body_monster).attack[p_ptr.events[29020]+1].ddice, m_race(p_ptr.body_monster).attack[p_ptr.events[29020]+1].dside)
			if (k <= 0) then k = 1 end
		end

		-- 1% increase per MArtial Arts points.
		k = k + multiply_divide(k, p_ptr.skill[19], 100)
		
	else

        	k = maxroll(mdice, mside)
	end

	k = k + ((k * (p_ptr.abilities[(CLASS_FIGHTER * 10) + 1] * 10)) / 100)

	-- Ranger's Forestry ability!
        if ((p_ptr.abilities[(CLASS_RANGER * 10) + 2] >= 1) and (standing_on_forest())) then

                k = k + multiply_divide(k, p_ptr.abilities[(CLASS_RANGER * 10) + 2] * 10, 100)
        end

	-- Bonus damages.
	if (p_ptr.events[29017] > 0) then

		k = k + p_ptr.events[29017]
		p_ptr.events[29017] = 0
	end

	k = k + ((p_ptr.skill[1] * (p_ptr.abilities[(CLASS_FIGHTER * 10) + 5] * 10)) / 100)
	k = k + glovebonus
	tskill = p_ptr.skill[1]
	tskill = tskill + (p_ptr.skill[19] + (p_ptr.skill[19] / 2))
        k = k * (tskill + 1)
	k = k + multiply_divide(k, p_ptr.dis_to_d, 100)

	-- An additional boost from strength.
	usedstat = (p_ptr.stat_ind[A_STR+1] - 5)

	-- We might use another stat instead.
	if (p_ptr.abilities[(CLASS_ENCHANTER * 10) + 6] >= 1 and get_object_flag4(inven(INVEN_HANDS), TR4_CRAFTED)) then

		local strbonus = (p_ptr.stat_ind[A_STR+1] - 5)
		local usedint = (p_ptr.stat_ind[A_INT+1] - 5)
		if (usedint > (p_ptr.abilities[(CLASS_ENCHANTER * 10) + 6] * 5)) then

			usedint = (p_ptr.abilities[(CLASS_ENCHANTER * 10) + 6] * 5)
		end
		if (usedint > strbonus) then

			usedstat = (p_ptr.stat_ind[A_INT+1] - 5)
			intfight = 1
		else

			usedstat = (p_ptr.stat_ind[A_STR+1] - 5)
		end
	end

        if (p_ptr.abilities[(CLASS_RANGER * 10) + 7] >= 1 and intfight == 0) then

		local strbonus = (p_ptr.stat_ind[A_STR+1] - 5)
		local useddex = (p_ptr.stat_ind[A_DEX+1] - 5)
		if (useddex > (p_ptr.abilities[(CLASS_RANGER * 10) + 7] * 5)) then

			useddex = (p_ptr.abilities[(CLASS_RANGER * 10) + 7] * 5)
		end
		if (useddex > strbonus) then

			usedstat = (p_ptr.stat_ind[A_DEX+1] - 5)
		else

			usedstat = (p_ptr.stat_ind[A_STR+1] - 5)
		end
	end

	k = k + multiply_divide(k, usedstat, 100)

	if (p_ptr.powerattack > 0) then

		if (p_ptr.powerlevel == 1) then k = k * (2 + ((2 * (p_ptr.abilities[(CLASS_FIGHTER * 10)+1] * 5)) / 100))
		elseif (p_ptr.powerlevel == 2) then k = k * (3 + ((3 * (p_ptr.abilities[(CLASS_FIGHTER * 10)+1] * 5)) / 100))
		elseif (p_ptr.powerlevel == 3) then k = k * (4 + ((4 * (p_ptr.abilities[(CLASS_FIGHTER * 10)+1] * 5)) / 100))
		end
	end

	-- Bard's War Songs ability.
	if (p_ptr.events[29042] == 1 and p_ptr.abilities[(CLASS_BARD * 10) + 8] > 0) then

		local chrbonus

		chrbonus = multiply_divide(p_ptr.stat_ind[A_CHR+1], p_ptr.abilities[(CLASS_BARD * 10) + 8] * 10, 100)
		k = k + multiply_divide(k, chrbonus, 100)
	end

        return k
end

-- A very simple hit rate system...yet, it's effective!
function player_hit_monster(monster, bonus)

        local phit = 0
	local proll = 0
	local mroll = 0
        local mistpenalities = 25 + (p_ptr.abilities[(CLASS_SHADOW * 10) + 7] / 2)

        -- First, let's calculate the player's hit rate!
        phit = (p_ptr.lev + p_ptr.to_h) + bonus

        -- Somehow, we should not miss this attack...
        if (nevermiss == TRUE) then return 1 end

	-- Diviner's Accuracy!
	if (p_ptr.abilities[(CLASS_DIVINER * 10) + 5] >= 1) then

		local ppower
		local mpower

		ppower = (p_ptr.skill[27] / 5) * p_ptr.abilities[(CLASS_DIVINER * 10) + 5]
		mpower = monster.level + monster.mind

		if (lua_randint(ppower) >= lua_randint(mpower)) then return 1 end
	end

	-- Monster race attacks have accuracy bonus.
	if (p_ptr.prace == RACE_MONSTER and p_ptr.events[29019] > 0) then

		phit = phit + multiply_divide(phit, p_ptr.abilities_monster_attacks[p_ptr.events[29019]] * 20, 100)
	end

        -- If the hit rate is negative or 0, well, give up, you can't hit! ;)
        if (phit <= 0) then return 0 end

        -- Now, let's roll the dices! Player's hit rate VS monster's def
        proll = lua_randint(phit)
        mroll = lua_randint(monster.defense)

        -- Enemies in the dark mist are easier to hit!
        if (cave(monster.fy, monster.fx).feat == FEAT_DARK_MIST) then

                local rollpenality
                rollpenality = mroll * (mistpenalities / 100)
                mroll = mroll - rollpenality
        end

        if (proll >= mroll) then return 1
        elseif (always_hit_check() == TRUE) then return 1
        else return 0
	end
end

-- How a monster hit you is not much more complicated...
function monster_hit_player(monster, bonus)

        local pdef
	local proll
	local mroll
        local mistpenalities = 25 + (p_ptr.abilities[(CLASS_SHADOW * 10) + 7] / 2)

	-- Kensai's Iajutsu!
	if (p_ptr.abilities[(CLASS_KENSAI * 10) + 2] >= 1 and (monster_physical) and (kensai_equip())) then
		msg_print("In Iajutsu")
		if (inven(INVEN_WIELD).itemskill == 12) then
			current_weapon = inven(INVEN_WIELD)
		else
			current_weapon = inven(INVEN_WIELD+1)
		end
		local hitbonus = (p_ptr.to_h * p_ptr.stat_ind[A_WIS+1] * 3 / 100)
		local iajutsudam = weapon_damages()
		iajutsudam = iajutsudam + (iajutsudam * p_ptr.stat_ind[A_WIS+1] * 3 / 100)
		iajutsudam = iajutsudam + (iajutsudam * p_ptr.abilities[(CLASS_KENSAI * 10) + 2] * 10 / 100)
		local damtype = current_weapon.extra1
		if (damtype == 0) then damtype = 15 end
		if (player_hit_monster(monster, hitbonus)) then
			melee_attack = TRUE
			no_magic_return = TRUE
			nevermiss = TRUE
			fire_ball_specific_grid(iajutsudam, monster.fx, monster.fy, 0, damtype)
			melee_attack = FALSE
			no_magic_return = FALSE
			nevermiss = FALSE
			if (monster_died) then

				monster_died = FALSE
				return 0
			end
		else
			msg_print(string.format('Your iajutsu misses %s!', m_race(monster.r_idx).name_char))
		end

	end

        -- First, let's calculate the player's defense!
        pdef = p_ptr.ac + p_ptr.to_a

        -- Now, let's roll the dices! Player's def VS monster's hit rate
        proll = lua_randint(pdef)
	if ((monster.hitrate + bonus) <= 0) then mroll = 0
        else mroll = lua_randint((monster.hitrate + bonus))
	end

        -- Enemies in the dark mist fight less well.
        if (cave(monster.fy, monster.fx).feat == FEAT_DARK_MIST) then

                local rollpenality
                rollpenality = mroll * (mistpenalities / 100)
                mroll = mroll - rollpenality
        end

        -- Do we have the Displacement ability?
        if (p_ptr.abilities[(CLASS_SHADOW * 10) + 2] >= 1) then

                local disroll
                local dismroll

		disroll = (p_ptr.skill[7] / 4) * p_ptr.abilities[(CLASS_SHADOW * 10) + 2]
                dismroll = monster.dex + monster.level

                if (lua_randint(disroll) >= lua_randint(dismroll)) then return 0 end
        end

        if (mroll >= proll) then return 1
        else return 0
	end
end

-- And a monster hitting another monster... 
function monster_hit_monster(monster, target)

        local tdef
	local mroll
	local troll
        local hitbonus = 0

        -- First, let's calculate the target's defense!
        tdef = target.defense

        -- Calculate hitbonus...if any
        if (is_pet(monster)) then

		if (p_ptr.stat_ind[A_CHR+1] > 5) then
                	hitbonus = hitbonus + (p_ptr.stat_ind[A_CHR+1] - 5) * 10
			hitbonus = hitbonus + ((hitbonus * ((p_ptr.stat_ind[A_CHR+1] - 5) * 5)) / 100)
		end
		hitbonus = hitbonus + (p_ptr.skill[10] * p_ptr.skill[10])
                if (hitbonus < 0) then hitbonus = 0 end
        end

        -- Now, let's roll the dices! Attacker's hit rate VS target's def
	if ((monster.hitrate + hitbonus) <= 0) then mroll = 0
        else mroll = lua_randint((monster.hitrate + hitbonus))
	end

        troll = lua_randint(tdef)
        if (mroll >= troll) then return 1
        else return 0
	end
end

function py_attack_executetemp (y, x, max_blow)
	py_attack(y, x, max_blow)
end

function damroll(dd, ds)
	local i = 0
	local tot = 0
	for i = 1,dd do
		tot = tot + lua_randint(ds)
	end
	return tot
end

function maxroll(dd, ds)
	local tot = (dd * ds)
	return tot
end

-- Event handlers
add_event_handler("py_attack", py_attack_execute)
add_event_handler("weapon_damages", weapon_damages)
add_event_handler("monk_damages", monk_damages)
add_event_handler("min_weapon_damages", min_weapon_damages)
add_event_handler("min_monk_damages", min_monk_damages)
add_event_handler("max_weapon_damages", max_weapon_damages)
add_event_handler("max_monk_damages", max_monk_damages)
add_event_handler("player_hit_monster", player_hit_monster)
add_event_handler("monster_hit_player", monster_hit_player)
add_event_handler("monster_hit_monster", monster_hit_monster)
