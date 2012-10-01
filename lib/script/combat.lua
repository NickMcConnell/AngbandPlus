-- File: combat.lua
-- Contains the code for melee combat, damages formulas, etc...

function py_attack_execute (y, x, max_blow)

        local           num = 0
        local           k = 0
	local           m_name = ""
	local           fear = FALSE
	local           mdeath = FALSE
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
	local		unarmed_attack = 0

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

	-- If you're unarmed, you should NOT be dual wielding, but putting it just in case.
	if (unarmed() and p_ptr.dualwield == 0) then

		unarmed_attack = 1
	end
	
        if (canattack == TRUE) then
        -- Attack with first weapon, then with second weapon

        for weap = 0, (p_ptr.dualwield + unarmed_attack) do

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

				-- Sneak Attack is rolled in advance, even if you don't actually use it.
				if (p_ptr.abilities[(CLASS_ROGUE * 10) + 6] >= 1 and monster(cave(y, x).m_idx).hp >= monster(cave(y, x).m_idx).maxhp) then

					local proll
					local mroll

					proll = p_ptr.skill[7]
					proll = proll + multiply_divide(proll, p_ptr.abilities[(CLASS_ROGUE * 10) + 6] * 40, 100)
					mroll = monster(cave(y, x).m_idx).skill_evasion

					if (lua_randint(proll) >= lua_randint(mroll)) then

						potential_sneak_attack = 1
					end
				end

				combatfeat = TRUE
                                do_cmd_racial_power(1)
                                usedcombo = 1
                                totalcombo = totalcombo + 1
				combatfeat = FALSE
				potential_sneak_attack = 0
				update_and_handle()
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

                                -- No negative damage
                                if (dam < 0) then dam = 0 end
			else
				if (inven(INVEN_WIELD + weap).extra1 == 0) then mtype = GF_PHYSICAL
				else mtype = inven(INVEN_WIELD + weap).extra1
				end
				dam = weapon_damages()

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
	local damstat = 0

	-- Defensive Strike!
	-- if (defensive_strike == 1) then tskill = tskill + p_ptr.skill[5] end

	-- Stealth Attack!
	-- if (stealth_attack == 1) then tskill = tskill + p_ptr.skill[7] end

	if (current_weapon.disabled > 0 or current_weapon.disabled == -1) then k = 1
        else
		local maxchance = 0

		maxchance = p_ptr.skill_base[1] + multiply_divide(p_ptr.skill_base[current_weapon.itemskill + 1], 150, 100)

		-- Stealthy Fighter.
		if (p_ptr.abilities[(CLASS_ROGUE * 10) + 1] > 0 and not(inven(INVEN_BODY).tval == TV_HARD_ARMOR or inven(INVEN_BODY).tval == TV_DRAG_ARMOR)) then

			local sbonus

			sbonus = p_ptr.abilities[(CLASS_ROGUE * 10) + 1] * 10
			if (sbonus > 100) then sbonus = 100 end
			maxchance = maxchance + multiply_divide(p_ptr.skill_base[7], sbonus, 100)
		end

		maxchance = maxchance - (kind(current_weapon).level * 2)
		if (maxchance > 0) then

			if (lua_randint(100) <= maxchance) then
				k = maxroll(current_weapon.dd, current_weapon.ds)
			else
				k = damroll(current_weapon.dd, current_weapon.ds)
			end
		else
			k = damroll(current_weapon.dd, current_weapon.ds)
		end
	end

	-- Two-handed weapons produces more damages.
	if ((get_object_flag4(current_weapon, TR4_COULD2H) or get_object_flag4(current_weapon, TR4_MUST2H)) and (inven(INVEN_WIELD).tval > 0 or inven(INVEN_WIELD+1).tval > 0)) then

		k = k + multiply_divide(k, 33, 100)
	end

	-- Damages are multiplied by every points of strength above 5.
	damstat = (p_ptr.stat_ind[A_STR+1]-5)

	-- Is using a weapon with the DEX_WEAPON flag, also add every points of Dexterity above five.
	if (get_object_flag4(current_weapon, TR4_DEX_WEAPON)) then

		damstat = damstat + (p_ptr.stat_ind[A_DEX+1]-5)
	end

	if (damstat > 0) then

		k = k + (k * damstat)
	end

	-- Power Attacks.
	if (p_ptr.powerattack > 0) then

		local chance_no_end

		chance_no_end = 0

		if (p_ptr.powerlevel == 1) then k = k * 3
		elseif (p_ptr.powerlevel == 2) then k = k * 5
		elseif (p_ptr.powerlevel == 3) then k = k * 7
		end

		if (p_ptr.prace == RACE_MONSTER and k > 0 and get_player_monster_ability(BOSS_CURSED_HITS)) then

			if (p_ptr.powerlevel == 1) then crushingblows = 2 end
			if (p_ptr.powerlevel == 2) then crushingblows = 3 end
			if (p_ptr.powerlevel == 3) then crushingblows = 4 end
		end
		update_and_handle()
	end
	
        return k
end

-- A function used to calculate monk damages
function monk_damages()

        local k
        local mdice
	local mside
	local glovebonus
	local wisbonus = 0
	local maxchance = 0

        mdice = (p_ptr.skill[19] / 40) + (p_ptr.skill[1] / 80) + 1
        mside = (p_ptr.skill[19] / 40) + (p_ptr.skill[1] / 80) + 3

	-- Damages bonus from gloves!
	glovebonus = 0
	if (not(inven(INVEN_HANDS).tval == 0) and inven(INVEN_HANDS).ac > 0 and inven(INVEN_HANDS).disabled == 0) then
		glovebonus = lua_randint(inven(INVEN_HANDS).ac)
	end
	
        -- For Monsters, it's different.
	if (p_ptr.prace == RACE_MONSTER) then

		-- Use the main essence if enabled.
		if (p_ptr.events[29028] == 1 and inven(INVEN_ESSENCE).tval > 0) then

			mdice = inven(INVEN_ESSENCE).dd
			mside = inven(INVEN_ESSENCE).ds
		else

			mdice = m_race(p_ptr.body_monster).attack[p_ptr.events[29020]+1].ddice
			mside = m_race(p_ptr.body_monster).attack[p_ptr.events[29020]+1].dside

			if (m_race(p_ptr.body_monster).attack[p_ptr.events[29020]+1].ddscalefactor > 0) then
				mdice = mdice + ((p_ptr.lev / m_race(p_ptr.body_monster).attack[p_ptr.events[29020]+1].ddscalefactor) * (m_race(p_ptr.body_monster).attack[p_ptr.events[29020]+1].ddscale))
			end
			if (m_race(p_ptr.body_monster).attack[p_ptr.events[29020]+1].dsscalefactor > 0) then
				mside = mside + ((p_ptr.lev / m_race(p_ptr.body_monster).attack[p_ptr.events[29020]+1].dsscalefactor) * (m_race(p_ptr.body_monster).attack[p_ptr.events[29020]+1].dsscale))
			end
		end
	end

	maxchance = p_ptr.skill_base[19]
	maxchance = maxchance - 50
	if (maxchance > 0) then

		if (lua_randint(100) <= maxchance) then
			if (not(inven(INVEN_HANDS).tval == 0) and inven(INVEN_HANDS).ac > 0 and inven(INVEN_HANDS).disabled == 0) then
				glovebonus = inven(INVEN_HANDS).ac
			end
			k = maxroll(mdice, mside) + glovebonus
		else
			k = damroll(mdice, mside) + glovebonus
		end
	else
		k = damroll(mdice, mside) + glovebonus
	end

	if (k <= 0) then k = 1 end

	-- Bonus damages. (Monsters)
	if (p_ptr.events[29017] > 0) then

		k = k + p_ptr.events[29017]
		p_ptr.events[29017] = 0
	end

	-- Damages may benefit from Wisdom with the proper abilities.
	if (p_ptr.abilities[(CLASS_MONK * 10) + 1] >= 1) then

		local wbonus

		wbonus = p_ptr.abilities[(CLASS_MONK * 10) + 1] * 10
		if (wbonus > 100) then wbonus = 100 end

		wisbonus = multiply_divide(p_ptr.stat_ind[A_WIS+1], wbonus, 100)
        end

	-- Damages are multiplied by every points of strength above 5.
	if ((p_ptr.stat_ind[A_STR+1] + wisbonus) > 5) then

		k = k + (k * ((p_ptr.stat_ind[A_STR+1] + wisbonus)-5))
	end

	-- Power Attacks.
	if (p_ptr.powerattack > 0) then

		local chance_no_end

		chance_no_end = 0

		if (p_ptr.powerlevel == 1) then k = k * 3
		elseif (p_ptr.powerlevel == 2) then k = k * 5
		elseif (p_ptr.powerlevel == 3) then k = k * 7
		end

		if (p_ptr.prace == RACE_MONSTER and k > 0 and get_player_monster_ability(BOSS_CURSED_HITS)) then

			if (p_ptr.powerlevel == 1) then crushingblows = 2 end
			if (p_ptr.powerlevel == 2) then crushingblows = 3 end
			if (p_ptr.powerlevel == 3) then crushingblows = 4 end
		end
		update_and_handle()
	end

	return k
end

-- Return minimum weapon damages. Used by files.c to display damages.
-- Could be used for abilities as well.
function min_weapon_damages()

        local k = 0
	local damstat = 0

	-- Defensive Strike!
	-- if (defensive_strike == 1) then tskill = tskill + p_ptr.skill[5] end

	-- Stealth Attack!
	-- if (stealth_attack == 1) then tskill = tskill + p_ptr.skill[7] end

	if (current_weapon.disabled > 0 or current_weapon.disabled == -1) then k = 1
        else k = damroll(current_weapon.dd, 1)
	end

	-- Two-handed weapons produces more damages.
	if ((get_object_flag4(current_weapon, TR4_COULD2H) or get_object_flag4(current_weapon, TR4_MUST2H)) and (inven(INVEN_WIELD).tval > 0 or inven(INVEN_WIELD+1).tval > 0)) then

		k = k + multiply_divide(k, 33, 100)
	end

	-- Damages are multiplied by every points of strength above 5.
	damstat = (p_ptr.stat_ind[A_STR+1]-5)

	-- Is using a weapon with the DEX_WEAPON flag, also add every points of Dexterity above five.
	if (get_object_flag4(current_weapon, TR4_DEX_WEAPON)) then

		damstat = damstat + (p_ptr.stat_ind[A_DEX+1]-5)
	end

	if (damstat > 0) then

		k = k + (k * damstat)
	end
	
        return k
end

-- Return maximum weapon damages. Used by files.c to display damages.
-- Could be used for abilities as well.
function max_weapon_damages()

        local k = 0
	local damstat = 0

	-- Defensive Strike!
	-- if (defensive_strike == 1) then tskill = tskill + p_ptr.skill[5] end

	-- Stealth Attack!
	-- if (stealth_attack == 1) then tskill = tskill + p_ptr.skill[7] end

	if (current_weapon.disabled > 0 or current_weapon.disabled == -1) then k = 1
        else k = maxroll(current_weapon.dd, current_weapon.ds)
	end

	-- Two-handed weapons produces more damages.
	if ((get_object_flag4(current_weapon, TR4_COULD2H) or get_object_flag4(current_weapon, TR4_MUST2H)) and (inven(INVEN_WIELD).tval > 0 or inven(INVEN_WIELD+1).tval > 0)) then

		k = k + multiply_divide(k, 33, 100)
	end

	-- Damages are multiplied by every points of strength above 5.
	damstat = (p_ptr.stat_ind[A_STR+1]-5)

	-- Is using a weapon with the DEX_WEAPON flag, also add every points of Dexterity above five.
	if (get_object_flag4(current_weapon, TR4_DEX_WEAPON)) then

		damstat = damstat + (p_ptr.stat_ind[A_DEX+1]-5)
	end

	if (damstat > 0) then

		k = k + (k * damstat)
	end
	
        return k
end

-- A function used to calculate minimum monk damages
function min_monk_damages()

        local k
        local mdice
	local mside
	local glovebonus
	local wisbonus = 0

        mdice = (p_ptr.skill[19] / 40) + (p_ptr.skill[1] / 80) + 1
        mside = (p_ptr.skill[19] / 40) + (p_ptr.skill[1] / 80) + 3

	-- Damages bonus from gloves!
	glovebonus = 0
	if (not(inven(INVEN_HANDS).tval == 0) and inven(INVEN_HANDS).ac > 0 and inven(INVEN_HANDS).disabled == 0) then
		glovebonus = 1
	end
	
        -- For Monsters, it's different.
	if (p_ptr.prace == RACE_MONSTER) then

		-- Use the main essence if enabled.
		if (p_ptr.events[29028] == 1 and inven(INVEN_ESSENCE).tval > 0) then

			mdice = inven(INVEN_ESSENCE).dd
			mside = inven(INVEN_ESSENCE).ds
		else

			mdice = m_race(p_ptr.body_monster).attack[p_ptr.events[29020]+1].ddice
			mside = m_race(p_ptr.body_monster).attack[p_ptr.events[29020]+1].dside

			if (m_race(p_ptr.body_monster).attack[p_ptr.events[29020]+1].ddscalefactor > 0) then
				mdice = mdice + ((p_ptr.lev / m_race(p_ptr.body_monster).attack[p_ptr.events[29020]+1].ddscalefactor) * (m_race(p_ptr.body_monster).attack[p_ptr.events[29020]+1].ddscale))
			end
			if (m_race(p_ptr.body_monster).attack[p_ptr.events[29020]+1].dsscalefactor > 0) then
				mside = mside + ((p_ptr.lev / m_race(p_ptr.body_monster).attack[p_ptr.events[29020]+1].dsscalefactor) * (m_race(p_ptr.body_monster).attack[p_ptr.events[29020]+1].dsscale))
			end
		end
	end

        k = damroll(mdice, 1) + glovebonus
	if (k <= 0) then k = 1 end

	-- Bonus damages. (Monsters)
	if (p_ptr.events[29017] > 0) then

		k = k + p_ptr.events[29017]
		p_ptr.events[29017] = 0
	end

	-- Damages may benefit from Wisdom with the proper abilities.
	if (p_ptr.abilities[(CLASS_MONK * 10) + 1] >= 1) then

		local wbonus

		wbonus = p_ptr.abilities[(CLASS_MONK * 10) + 1] * 10
		if (wbonus > 100) then wbonus = 100 end

		wisbonus = multiply_divide(p_ptr.stat_ind[A_WIS+1], wbonus, 100)
        end

	-- Damages are multiplied by every points of strength above 5.
	if ((p_ptr.stat_ind[A_STR+1] + wisbonus) > 5) then

		k = k + (k * ((p_ptr.stat_ind[A_STR+1] + wisbonus)-5))
	end

	-- Power Attacks.
	if (p_ptr.powerattack > 0) then

		local chance_no_end

		chance_no_end = 0

		if (p_ptr.powerlevel == 1) then k = k * 3
		elseif (p_ptr.powerlevel == 2) then k = k * 5
		elseif (p_ptr.powerlevel == 3) then k = k * 7
		end

		if (p_ptr.prace == RACE_MONSTER and k > 0 and get_player_monster_ability(BOSS_CURSED_HITS)) then

			if (p_ptr.powerlevel == 1) then crushingblows = 2 end
			if (p_ptr.powerlevel == 2) then crushingblows = 3 end
			if (p_ptr.powerlevel == 3) then crushingblows = 4 end
		end
		update_and_handle()
	end

	return k
end

-- A function used to calculate maximum monk damages
function max_monk_damages()

        local k
        local mdice
	local mside
	local glovebonus
	local wisbonus = 0

        mdice = (p_ptr.skill[19] / 40) + (p_ptr.skill[1] / 80) + 1
        mside = (p_ptr.skill[19] / 40) + (p_ptr.skill[1] / 80) + 3

	-- Damages bonus from gloves!
	glovebonus = 0
	if (not(inven(INVEN_HANDS).tval == 0) and inven(INVEN_HANDS).ac > 0 and inven(INVEN_HANDS).disabled == 0) then
		glovebonus = inven(INVEN_HANDS).ac
	end

        -- For Monsters, it's different.
	if (p_ptr.prace == RACE_MONSTER) then

		-- Use the main essence if enabled.
		if (p_ptr.events[29028] == 1 and inven(INVEN_ESSENCE).tval > 0) then

			mdice = inven(INVEN_ESSENCE).dd
			mside = inven(INVEN_ESSENCE).ds
		else

			mdice = m_race(p_ptr.body_monster).attack[p_ptr.events[29020]+1].ddice
			mside = m_race(p_ptr.body_monster).attack[p_ptr.events[29020]+1].dside

			if (m_race(p_ptr.body_monster).attack[p_ptr.events[29020]+1].ddscalefactor > 0) then
				mdice = mdice + ((p_ptr.lev / m_race(p_ptr.body_monster).attack[p_ptr.events[29020]+1].ddscalefactor) * (m_race(p_ptr.body_monster).attack[p_ptr.events[29020]+1].ddscale))
			end
			if (m_race(p_ptr.body_monster).attack[p_ptr.events[29020]+1].dsscalefactor > 0) then
				mside = mside + ((p_ptr.lev / m_race(p_ptr.body_monster).attack[p_ptr.events[29020]+1].dsscalefactor) * (m_race(p_ptr.body_monster).attack[p_ptr.events[29020]+1].dsscale))
			end
		end
	end

        k = maxroll(mdice, mside) + glovebonus
	if (k <= 0) then k = 1 end

	-- Bonus damages. (Monsters)
	if (p_ptr.events[29017] > 0) then

		k = k + p_ptr.events[29017]
		p_ptr.events[29017] = 0
	end

	-- Damages may benefit from Wisdom with the proper abilities.
	if (p_ptr.abilities[(CLASS_MONK * 10) + 1] >= 1) then

		local wbonus

		wbonus = p_ptr.abilities[(CLASS_MONK * 10) + 1] * 10
		if (wbonus > 100) then wbonus = 100 end

		wisbonus = multiply_divide(p_ptr.stat_ind[A_WIS+1], wbonus, 100)
        end

	-- Damages are multiplied by every points of strength above 5.
	if ((p_ptr.stat_ind[A_STR+1] + wisbonus) > 5) then

		k = k + (k * ((p_ptr.stat_ind[A_STR+1] + wisbonus)-5))
	end

	-- Power Attacks.
	if (p_ptr.powerattack > 0) then

		local chance_no_end

		chance_no_end = 0

		if (p_ptr.powerlevel == 1) then k = k * 3
		elseif (p_ptr.powerlevel == 2) then k = k * 5
		elseif (p_ptr.powerlevel == 3) then k = k * 7
		end

		if (p_ptr.prace == RACE_MONSTER and k > 0 and get_player_monster_ability(BOSS_CURSED_HITS)) then

			if (p_ptr.powerlevel == 1) then crushingblows = 2 end
			if (p_ptr.powerlevel == 2) then crushingblows = 3 end
			if (p_ptr.powerlevel == 3) then crushingblows = 4 end
		end
		update_and_handle()
	end

	return k
end

-- A very simple hit rate system...yet, it's effective!
-- "bonus" should be your skills value.
function player_hit_monster(monster, bonus)

        local phit = 0
	local mdodge = 0
	local proll = 0
	local mroll = 0
        local mistpenalities = 25 + (p_ptr.abilities[(CLASS_SHADOW * 10) + 7] / 2)
	local totalbonus = 0

        -- First, let's calculate the player's hit rate.
	-- It's Dex + Skill(bonus should be the skill's value) + to_h.
        phit = p_ptr.stat_ind[A_DEX+1] + bonus + p_ptr.dis_to_h

	-- Next is the monster's dodge rate.
	-- It's Dex + Evasion.
	mdodge = monster.dex + monster.skill_evasion

        -- Somehow, we should not miss this attack...
        if (nevermiss == TRUE) then return 1 end

	-- Paralyzed enemies are easier to hit.
	if (monster.seallight > 0) then phit = phit + (phit / 2) end

	-- Sleeping enemies cannot dodge.
	if (monster.csleep > 0) then return 1 end

        -- If the hit rate is negative or 0, well, give up, you can't hit! ;)
        if (phit <= 0) then return 0 end

        -- Now, let's roll the dices! Player's hit rate VS monster's def
        proll = lua_randint(phit)
        mroll = lua_randint(mdodge)

	-- PASSIVE BONUS TO HIT ROLL --

	-- Battle Skill.
	if (p_ptr.abilities[(CLASS_WARRIOR * 10) + 4] >= 1) then

		totalbonus = totalbonus + (p_ptr.abilities[(CLASS_WARRIOR * 10) + 4] * 5)
        end

	-- Charge(7+ grids)
	if (charge_hit_bonus == 1) then

		totalbonus = totalbonus + (p_ptr.abilities[(CLASS_FIGHTER * 10) + 7] * 10)
	end

	-- Marksman's Rapid Shot.
	if (rapid_shot == 1) then

		totalbonus = totalbonus + (p_ptr.abilities[(CLASS_MARKSMAN * 10) + 5] * 20)
	end

	-- Marksman's Accurate Shots.
	if ((ranged_attack) and p_ptr.abilities[(CLASS_MARKSMAN * 10) + 10] >= 1) then

		totalbonus = totalbonus + (p_ptr.abilities[(CLASS_MARKSMAN * 10) + 10] * 10)
	end

	-- Monster race attacks have accuracy bonus.
	if (p_ptr.prace == RACE_MONSTER and p_ptr.events[29019] > 0) then

		totalbonus = totalbonus + (p_ptr.abilities_monster_attacks[p_ptr.events[29019]] * 10)
	end

	proll = proll + multiply_divide(proll, totalbonus, 100)

        -- Enemies in the dark mist are easier to hit!
        if (cave(monster.fy, monster.fx).feat == FEAT_DARK_MIST) then

                local rollpenality
                rollpenality = mroll * (mistpenalities / 100)
                mroll = mroll - rollpenality
        end

        if (proll >= mroll) then return 1
        elseif (always_hit_check() == TRUE) then return 1
        else
		-- Unavoidable Power Attacks.
		if (p_ptr.abilities[(CLASS_FIGHTER * 10) + 8] >= 1 and (p_ptr.powerattack > 0)) then

			unavoidable_powerattack = 1
			return 1
		else
			return 0
		end
	end
end

-- How a monster hit you is not much more complicated...
function monster_hit_player(monster, bonus)

        local pdodge = 0
	local mhit = 0
	local proll = 0
	local mroll = 0
        local mistpenalities = 25 + (p_ptr.abilities[(CLASS_SHADOW * 10) + 7] / 2)
	local dodgebonus = 0

        -- First, let's calculate the player's dodge rate.
	-- It's Dex + Agility.
        pdodge = p_ptr.stat_ind[A_DEX+1] + p_ptr.skill[6]

	-- Passive bonus to dodge rolls.

	if (p_ptr.abilities[(CLASS_FIGHTER * 10) + 2] > 0) then

		dodgebonus = dodgebonus + multiply_divide(p_ptr.skill[1], p_ptr.abilities[(CLASS_FIGHTER * 10) + 2] * 10, 100)
	end

	-- Rogue's Improved Dodge.
	if (p_ptr.abilities[(CLASS_ROGUE * 10) + 4] >= 1) then

		dodgebonus = dodgebonus + multiply_divide(pdodge, p_ptr.abilities[(CLASS_ROGUE * 10) + 4] * 20, 100)
	end

	-- Defender's Universal Avoidance.
	if (p_ptr.abilities[(CLASS_DEFENDER * 10) + 6] >= 1) then

		dodgebonus = dodgebonus + multiply_divide(pdodge, p_ptr.abilities[(CLASS_DEFENDER * 10) + 6] * 10, 100)
	end

	-- Monk's Perfect Union.
	if (p_ptr.abilities[(CLASS_MONK * 10) + 10] >= 1) then

		if (not(inven(INVEN_BODY).tval == TV_HARD_ARMOR) and not(inven(INVEN_BODY).tval == TV_DRAG_ARMOR)) then

			dodgebonus = dodgebonus + multiply_divide(p_ptr.skill[19], p_ptr.abilities[(CLASS_MONK * 10) + 10] * 10, 100)
		end
	end

	pdodge = pdodge + dodgebonus

	-- Next, the monster's hit rate.
	-- It's dex + bonus(should be skill_attack or skill_ranged)
	mhit = monster.dex + bonus

        -- Now, let's roll the dices! Player's def VS monster's hit rate
        proll = lua_randint(pdodge)
	mroll = lua_randint(mhit)

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

        if (mroll >= proll) then

		-- Cursed Evasion Nightmare ability.
		if (p_ptr.abilities[(CLASS_NIGHT1 * 10) + 4] >= 1 and p_ptr.cursed > 0) then

			local newproll

			newproll = multiply_divide(proll, p_ptr.abilities[(CLASS_NIGHT1 * 10) + 4] * (4 * p_ptr.cursed), 100)

			if (lua_randint(mroll) >= lua_randint(newproll)) then

				return 1
			end
		else
			return 1
		end
        else return 0
	end
end

-- And a monster hitting another monster...
function monster_hit_monster(monster, target, bonus)

        local tdef
	local mroll
	local troll
        local hitbonus = 0

        -- First, let's calculate the target's defense!
        tdef = target.dex + target.skill_evasion

        -- Calculate hitbonus...if any
        if (is_pet(monster)) then

		if (p_ptr.stat_ind[A_CHR+1] > 5) then
                	hitbonus = hitbonus + (p_ptr.stat_ind[A_CHR+1] - 5) * 10
			hitbonus = hitbonus + ((hitbonus * ((p_ptr.stat_ind[A_CHR+1] - 5) * 5)) / 100)
		end
		hitbonus = hitbonus + (p_ptr.skill[10] * p_ptr.skill[10])
                if (hitbonus < 0) then hitbonus = 0 end
        end

        -- Now, let's roll the dices!

	mroll = monster.dex + bonus + hitbonus
        troll = tdef

        if (lua_randint(mroll) >= lua_randint(troll)) then return 1
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
