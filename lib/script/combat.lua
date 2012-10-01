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

        if (get_monster_flag1(p_ptr.body_monster, RF1_NEVER_BLOW) == TRUE) then
                msg_print("You cannot attack in this form!")
                canattack = FALSE
        end

        if (p_ptr.skill[7] >= 30) then

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
	
        -- Monsters that can dont use weapons, us etheir natural attacks
        if ((p_ptr.body_monster > 0) and (m_race(p_ptr.body_monster).body_parts[BODY_WEAPON+1] == 0)) then

                incarnate_monster_attack(cave(y, x).m_idx, y, x)
     
        elseif (canattack == TRUE) then
        -- Attack with first weapon, then with second weapon

        for weap = 0, 1 do

        -- Monster is already dead ? oh :(
        if (mdeath) then break end

        -- Reset the blows counter
        num = 0

	-- Access the weapon
	current_weapon = inven(INVEN_WIELD + weap)

        -- No weapon found? Go for natural attacks then...
        if (inven(INVEN_WIELD + weap).tval == 0 and not (p_ptr.body_monster == 0) and unarmed() == TRUE) then

                incarnate_monster_attack(cave(y, x).m_idx, y, x)
                return
        end

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

                -- Maybe the player want to use a special move instead...
                -- Offer the choice...if possible.
                if (totalcombo < maxcombo and mdeath == FALSE) then

			msg_print(NULL)  
			if (unarmed() == TRUE) then msg_print("Use a special ability? [y/n/no[t] this turn]")
			elseif (weap == 0) then msg_print("Use a special ability? [y/n/no[t] this turn]")
                        else msg_print("Use a special ability? [y/n/no[t] this turn]")
			end

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

                -- Piercing Stab passive feat
                if (dagger_check() and p_ptr.skill[16] >= 5) then daggerbonus = p_ptr.to_h / 4 end

                if (usedcombo == 0) then

                -- Test for hit
                if (player_hit_monster(monster(cave(y, x).m_idx), daggerbonus) == 1) then

			local critical_hit = 0
			local pcrit = p_ptr.abilities[(CLASS_FIGHTER * 10) + 4] * 5
			local mcritres = monster(cave(y, x).m_idx).level + monster(cave(y, x).m_idx).str

			-- Check for Fighter's critical hits!
			if (p_ptr.abilities[(CLASS_FIGHTER * 10) + 4] >= 1) then

				if (lua_randint(pcrit) >= lua_randint(mcritres)) then

					critical_hit = 1
				end
			end

			-- Sound
			sound(SOUND_HIT)

			-- Hack -- bare hands do one damage
			k = 1
                        if (inven(INVEN_WIELD + weap).k_idx == 0) then

				-- If we have nothing in this slots, but we have in another one */
				-- We're not unarmed. Skip attack phase. */
				if (unarmed()) then

                                	k = monk_damages()
				
					if (backstab) then

						backstab = FALSE
                                        	k = k * (3 + p_ptr.abilities[(CLASS_ROGUE * 10) + 2])
					elseif (stab_fleeing) then

                                        	k = k * (3 + p_ptr.abilities[(CLASS_ROGUE * 10) + 2]) / 2
					end

                                	-- No negative damage
                                	if (k < 0) then k = 0 end

				else 
					noweapon = TRUE
				end                              
                        else
                                k = weapon_damages()
                                k = tot_dam_aux(inven(INVEN_WIELD + weap), k, monster(cave(y, x).m_idx))

                                if (backstab) then

					backstab = FALSE
                                        k = k * (3 + p_ptr.abilities[(CLASS_ROGUE * 10) + 2])

				elseif (stab_fleeing) then

                                        k = k * (3 + p_ptr.abilities[(CLASS_ROGUE * 10) + 2]) / 2
				end
                                --do_cmd_damage_weapon()
			end

			tmp_r_idx = monster(cave(y, x).m_idx).r_idx
			if (noweapon == FALSE) then
			-- Physical resistance of monsters...
			k = k - ((k * m_race(tmp_r_idx).physres) / 100)
			m_race(monster(cave(y, x).m_idx).r_idx).r_resist[GF_PHYSICAL+1] = 1

                                -- Bosses/Elites can be immune to weapons...
				if (get_monster_ability(monster(cave(y, x).m_idx), BOSS_RETURNING)) then

                                        local returndamages
                                        returndamages = k / 2
                                        msg_print("You hurt yourself!")
                                        take_hit(returndamages, "A monster ability")
                                end
				-- Returning counter!
				if ((m_race(monster(cave(y, x).m_idx).r_idx).countertype == 7 or m_race(monster(cave(y, x).m_idx).r_idx).countertype == 9) and lua_randint(100) <= m_race(monster(cave(y, x).m_idx).r_idx).counterchance) then

                                        msg_print("Damages are reflected to you!")
                                        take_hit(k, "Melee returning counter")
				end
				-- Block & Return counter!
				if ((m_race(monster(cave(y, x).m_idx).r_idx).countertype == 10 or m_race(monster(cave(y, x).m_idx).r_idx).countertype == 12) and lua_randint(100) <= m_race(monster(cave(y, x).m_idx).r_idx).counterchance) then

					if (monster_hit_player(monster(cave(y, x).m_idx), 0) == TRUE) then

						msg_print(string.format('%s blocked your atack!', m_name))
						msg_print("Damages are reflected to you!")
                                        	take_hit(k, "Melee returning counter")
						k = 0
						blocked = TRUE
					end
				end
				if ((m_race(monster(cave(y, x).m_idx).r_idx).countertype == 13 or m_race(monster(cave(y, x).m_idx).r_idx).countertype == 15) and lua_randint(100) <= m_race(monster(cave(y, x).m_idx).r_idx).counterchance) then

					msg_print(string.format('%s blocked your atack!', m_name))
					msg_print("Damages are reflected to you!")
                                        take_hit(k, "Melee returning counter")
					k = 0
					blocked = TRUE
				end

                                if (get_monster_ability(monster(cave(y, x).m_idx), BOSS_HALVE_DAMAGES)) then
 
                                        k = k / 2
                                end
                                if (get_monster_ability(monster(cave(y, x).m_idx), BOSS_IMMUNE_WEAPONS)) then
                                        k = 0
                                        msg_print("The monster seems to be immune...")
                                end
                                
				-- Some counters...
				if ((m_race(monster(cave(y, x).m_idx).r_idx).countertype == 1 or m_race(monster(cave(y, x).m_idx).r_idx).countertype == 3 or m_race(monster(cave(y, x).m_idx).r_idx).countertype == 17 or m_race(monster(cave(y, x).m_idx).r_idx).countertype == 19) and lua_randint(100) <= m_race(monster(cave(y, x).m_idx).r_idx).counterchance) then

					if (monster_hit_player(monster(cave(y, x).m_idx), 0) == TRUE) then

						msg_print(string.format('%s blocked your atack!', m_name))
						k = 0
						blocked = TRUE
					end
				end
				if ((m_race(monster(cave(y, x).m_idx).r_idx).countertype == 4 or m_race(monster(cave(y, x).m_idx).r_idx).countertype == 6 or m_race(monster(cave(y, x).m_idx).r_idx).countertype == 21 or m_race(monster(cave(y, x).m_idx).r_idx).countertype == 23) and lua_randint(100) <= m_race(monster(cave(y, x).m_idx).r_idx).counterchance) then

					msg_print(string.format('%s blocked your atack!', m_name))
					k = 0
					blocked = TRUE
				end

			-- No negative damage
			if (k < 0) then k = 0 end

			-- Lower defense?
                        if (get_object_flag4(inven(INVEN_WIELD + weap), TR4_LOWER_DEF) and (blocked == FALSE)) then

                                local defamount

                                defamount = (damroll(inven(INVEN_WIELD + weap).dd, inven(INVEN_WIELD + weap).ds) * 3) / 10
                                if (monster(cave(y, x).m_idx).defense <= 0) then defamount = 0 end
				msg_print(string.format('%s loses %d defense!', m_name, defamount))
                                monster(cave(y, x).m_idx).defense = monster(cave(y, x).m_idx).defense - defamount
                                if (monster(cave(y, x).m_idx).defense <= 0) then m_ptr.defense = 0 end
                        end
                        -- Lower hit rate?
                        if (get_object_flag4(inven(INVEN_WIELD + weap), TR4_LOWER_HIT) and (blocked == FALSE)) then

                                local hitamount

                                hitamount = (damroll(inven(INVEN_WIELD + weap).dd, inven(INVEN_WIELD + weap).ds) * 3) / 10
                                if (monster(cave(y, x).m_idx).hitrate <= 0) then hitamount = 0 end
				msg_print(string.format('%s loses %d hit rate!', m_name, hitamount))
                                monster(cave(y, x).m_idx).hitrate = monster(cave(y, x).m_idx).hitrate - hitamount
                                if (monster(cave(y, x).m_idx).hitrate <= 0) then m_ptr.hitrate = 0 end
                        end
			-- High Monk's Disabling Blows!
                        if (unarmed() == TRUE and p_ptr.abilities[(CLASS_ZELAR * 10) + 6] >= 1 and blocked == FALSE) then

                                local defreduction = (p_ptr.abilities[(CLASS_ZELAR * 10) + 6] * 15)
                                local speedreduction = 1 + (p_ptr.abilities[(CLASS_ZELAR * 10) + 6] / 2)

                                if (not (get_monster_ability(monster(cave(y, x).m_idx), BOSS_IMMUNE_WEAPONS)) and not (get_monster_flag1(monster(cave(y, x).m_idx).r_idx, RF1_UNIQUE))) then

                                        monster(cave(y, x).m_idx).hitrate = monster(cave(y, x).m_idx).hitrate - defreduction
                                        monster(cave(y, x).m_idx).defense = monster(cave(y, x).m_idx).hitrate - defreduction
                                        monster(cave(y, x).m_idx).mspeed = monster(cave(y, x).m_idx).hitrate - speedreduction
					msg_print(string.format('Def/Hit loss: %d, Speed loss: %d.', defreduction, speedreduction))
                                end
                        end

			-- Message
			if ((backstab == FALSE or stab_fleeing == FALSE) and (blocked == FALSE)) then

                        	msg_print(string.format('You hit %s!', m_name))
			elseif (blocked == FALSE) then
				msg_print(string.format('You backstab %s!', m_name))
			end

			-- Fighter's critical hits!
			if ((k > 0) and (critical_hit == 1)) then

				msg_print(string.format('%s receives critical hit!', m_name))
				k = k * 2
				if (not (get_monster_flag1(monster(cave(y, x).m_idx).r_idx, RF1_UNIQUE)) and not (get_monster_flag3(monster(cave(y, x).m_idx).r_idx, RF3_NO_STUN))) then

                                        monster(cave(y, x).m_idx).seallight = 2 + (p_ptr.abilities[(CLASS_FIGHTER * 10) + 4] / 20)
                                end
			end
			
			-- Damage, check for fear and death
			if (mon_take_hit(cave(y, x).m_idx, k, fear, nil) == TRUE) then

				mdeath = TRUE
				break
			end
			
			if (is_pet(monster(cave(y, x).m_idx)) == TRUE) then

				msg_print(string.format('%s gets angry!', m_name))
				set_pet(monster(cave(y, x).m_idx), FALSE)
			end
			if (unarmed() == TRUE) then
				if (inven(INVEN_HANDS).brandtype > 0 and (blocked == FALSE) and (mdeath == FALSE)) then

					no_magic_return = TRUE
					fire_jump_ball(inven(INVEN_HANDS).brandtype, (inven(INVEN_HANDS).branddam * (inven(INVEN_HANDS).pval + 1)), inven(INVEN_HANDS).brandrad, monster(cave(y, x).m_idx).fx, monster(cave(y, x).m_idx).fy, TRUE)
					no_magic_return = FALSE
				end
			else
				if (inven(INVEN_WIELD + weap).brandtype > 0 and (blocked == FALSE) and (mdeath == FALSE)) then

					no_magic_return = TRUE
					fire_jump_ball(inven(INVEN_WIELD + weap).brandtype, (inven(INVEN_WIELD + weap).branddam * (inven(INVEN_WIELD + weap).pval + 1)), inven(INVEN_WIELD + weap).brandrad, monster(cave(y, x).m_idx).fx, monster(cave(y, x).m_idx).fy, TRUE)
					no_magic_return = FALSE
				end
			end
			-- End of weapons check
			end

		-- Player misses
		else

			-- Sound
			sound(SOUND_MISS)

			backstab = FALSE

			-- Message
			msg_print(string.format('You miss %s!', m_name))

		end
		-- End of combo checks
                end
		update_and_handle()
	end

        else

                msg_print("You can't attack with that weapon.")
        end
        end
        end
	
	if (fear and monster(cave(y, x).m_idx).ml) then
		-- Sound
		sound(SOUND_FLEE);

		-- Message
		msg_print(string.format('%s flees in terror!', m_name))
	end

end

-- Damages by weapons
function weapon_damages()
        local k = 0
	local tskill = 0

	tskill = p_ptr.skill[1]
	if (current_weapon.tval == TV_SWORD or current_weapon.tval == TV_SWORD_DEVASTATION) then tskill = tskill + (p_ptr.skill[13] + (p_ptr.skill[13] / 2)) end
        if (current_weapon.tval == TV_HAFTED or current_weapon.tval == TV_MSTAFF or current_weapon.tval == TV_HELL_STAFF) then tskill = tskill + (p_ptr.skill[14] + (p_ptr.skill[14] / 2)) end
        if (current_weapon.tval == TV_POLEARM) then tskill = tskill + (p_ptr.skill[15] + (p_ptr.skill[15] / 2)) end
        if (current_weapon.tval == TV_DAGGER) then tskill = tskill + (p_ptr.skill[16] + (p_ptr.skill[16] / 2)) end
        if (current_weapon.tval == TV_AXE) then tskill = tskill + (p_ptr.skill[17] + (p_ptr.skill[17] / 2)) end
        if (current_weapon.tval == TV_ROD) then tskill = tskill + (p_ptr.skill[18] + (p_ptr.skill[18] / 2)) end
        if (current_weapon.tval == TV_ZELAR_WEAPON) then tskill = tskill + (p_ptr.skill[19] + (p_ptr.skill[19] / 2)) end

        k = damroll(current_weapon.dd, current_weapon.ds)
	k = k + ((k * (p_ptr.abilities[(CLASS_FIGHTER * 10) + 1] * 10)) / 100)
	if (current_weapon.tval == TV_POLEARM and (p_ptr.skill[15] >= 90)) then k = k * 2 end
        k = k * (tskill + 1)
        k = k + ((k * p_ptr.dis_to_d) / 100)
        if (p_ptr.abilities[(CLASS_RANGER * 10) + 7] >= 1) then

		local strbonus = (p_ptr.stat_ind[A_STR+1] - 5)
		local useddex = (p_ptr.stat_ind[A_DEX+1] - 5)
		if (useddex > (p_ptr.abilities[(CLASS_RANGER * 10) + 7] * 5)) then

			useddex = (p_ptr.abilities[(CLASS_RANGER * 10) + 7] * 5)
		end
		if (useddex > strbonus) then

			k = k + ((k * useddex) / 100)
		else

			k = k + ((k * strbonus) / 100)
		end
        else
		k = k + ((k * p_ptr.stat_ind[A_STR+1]) / 100)
	end

	if (p_ptr.powerattack > 0) then

		if (p_ptr.powerlevel == 1) then k = k * (2 + ((2 * (p_ptr.abilities[(CLASS_FIGHTER * 10)+1] * 5)) / 100))
		elseif (p_ptr.powerlevel == 2) then k = k * (3 + ((3 * (p_ptr.abilities[(CLASS_FIGHTER * 10)+1] * 5)) / 100))
		elseif (p_ptr.powerlevel == 3) then k = k * (4 + ((4 * (p_ptr.abilities[(CLASS_FIGHTER * 10)+1] * 5)) / 100))
		end
		set_powerattack(0)
		p_ptr.str_boost = 0
		p_ptr.str_boost_dur = 0
		update_and_handle()
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

        mdice = (p_ptr.skill[19] / 15) + (p_ptr.skill[1] / 40) + 1
        mside = (p_ptr.skill[19] / 15) + (p_ptr.skill[1] / 40) + 3

	-- Damages bonus from gloves!
	glovebonus = 0
	if (inven(INVEN_HANDS).tval ~= 0 and inven(INVEN_HANDS).ac > 0) then
		glovebonus = lua_randint(inven(INVEN_HANDS).ac)
	end

        k = damroll(mdice, mside)
	k = k + ((k * (p_ptr.abilities[(CLASS_FIGHTER * 10) + 1] * 10)) / 100)
	k = k + ((p_ptr.skill[1] * (p_ptr.abilities[(CLASS_FIGHTER * 10) + 5] * 5)) / 100)
	k = k + glovebonus
	tskill = p_ptr.skill[1]
	tskill = tskill + (p_ptr.skill[19] + (p_ptr.skill[19] / 2))
        k = k * (tskill + 1)
        k = k + ((k * p_ptr.dis_to_d) / 100)
        if (p_ptr.abilities[(CLASS_RANGER * 10) + 7] >= 1) then

		local strbonus = (p_ptr.stat_ind[A_STR+1] - 5)
		local useddex = (p_ptr.stat_ind[A_DEX+1] - 5)
		if (useddex > (p_ptr.abilities[(CLASS_RANGER * 10) + 7] * 5)) then

			useddex = (p_ptr.abilities[(CLASS_RANGER * 10) + 7] * 5)
		end
		if (useddex > strbonus) then

			k = k + ((k * useddex) / 100)
		else

			k = k + ((k * strbonus) / 100)
		end
        else 
		k = k + ((k * p_ptr.stat_ind[A_STR+1]) / 100)
	end
	if (p_ptr.powerattack > 0) then

		if (p_ptr.powerlevel == 1) then k = k * (2 + ((2 * (p_ptr.abilities[(CLASS_FIGHTER * 10)+1] * 5)) / 100))
		elseif (p_ptr.powerlevel == 2) then k = k * (3 + ((3 * (p_ptr.abilities[(CLASS_FIGHTER * 10)+1] * 5)) / 100))
		elseif (p_ptr.powerlevel == 3) then k = k * (4 + ((4 * (p_ptr.abilities[(CLASS_FIGHTER * 10)+1] * 5)) / 100))
		end
		set_powerattack(0)
		p_ptr.str_boost = 0
		p_ptr.str_boost_dur = 0
		update_and_handle()
	end
        return k
end

-- Return minimum weapon damages. Used by files.c to display damages.
-- Could be used for abilities as well.
function min_weapon_damages()

        local k
	local tskill

	tskill = p_ptr.skill[1]
	if (current_weapon.tval == TV_SWORD or current_weapon.tval == TV_SWORD_DEVASTATION) then tskill = tskill + (p_ptr.skill[13] + (p_ptr.skill[13] / 2)) end
        if (current_weapon.tval == TV_HAFTED or current_weapon.tval == TV_MSTAFF or current_weapon.tval == TV_HELL_STAFF) then tskill = tskill + (p_ptr.skill[14] + (p_ptr.skill[14] / 2)) end
        if (current_weapon.tval == TV_POLEARM) then tskill = tskill + (p_ptr.skill[15] + (p_ptr.skill[15] / 2)) end
        if (current_weapon.tval == TV_DAGGER) then tskill = tskill + (p_ptr.skill[16] + (p_ptr.skill[16] / 2)) end
        if (current_weapon.tval == TV_AXE) then tskill = tskill + (p_ptr.skill[17] + (p_ptr.skill[17] / 2)) end
        if (current_weapon.tval == TV_ROD) then tskill = tskill + (p_ptr.skill[18] + (p_ptr.skill[18] / 2)) end
        if (current_weapon.tval == TV_ZELAR_WEAPON) then tskill = tskill + (p_ptr.skill[19] + (p_ptr.skill[19] / 2)) end

        k = damroll(current_weapon.dd, 1)
	k = k + ((k * (p_ptr.abilities[(CLASS_FIGHTER * 10) + 1] * 10)) / 100)
	if (current_weapon.tval == TV_POLEARM and (p_ptr.skill[15] >= 90)) then k = k * 2 end
        k = k * (tskill + 1)
        k = k + ((k * p_ptr.dis_to_d) / 100)
        if (p_ptr.abilities[(CLASS_RANGER * 10) + 7] >= 1) then

		local strbonus = (p_ptr.stat_ind[A_STR+1] - 5)
		local useddex = (p_ptr.stat_ind[A_DEX+1] - 5)
		if (useddex > (p_ptr.abilities[(CLASS_RANGER * 10) + 7] * 5)) then

			useddex = (p_ptr.abilities[(CLASS_RANGER * 10) + 7] * 5)
		end
		if (useddex > strbonus) then

			k = k + ((k * useddex) / 100)
		else

			k = k + ((k * strbonus) / 100)
		end
        else 
		k = k + ((k * p_ptr.stat_ind[A_STR+1]) / 100)
	end
	if (p_ptr.powerattack > 0) then

		if (p_ptr.powerlevel == 1) then k = k * (2 + ((2 * (p_ptr.abilities[(CLASS_FIGHTER * 10)+1] * 5)) / 100))
		elseif (p_ptr.powerlevel == 2) then k = k * (3 + ((3 * (p_ptr.abilities[(CLASS_FIGHTER * 10)+1] * 5)) / 100))
		elseif (p_ptr.powerlevel == 3) then k = k * (4 + ((4 * (p_ptr.abilities[(CLASS_FIGHTER * 10)+1] * 5)) / 100))
		end
	end
        return k
end

-- Return maximum weapon damages. Used by files.c to display damages.
-- Could be used for abilities as well.
function max_weapon_damages()

        local k
	local tskill

	tskill = p_ptr.skill[1]
	if (current_weapon.tval == TV_SWORD or current_weapon.tval == TV_SWORD_DEVASTATION) then tskill = tskill + (p_ptr.skill[13] + (p_ptr.skill[13] / 2)) end
        if (current_weapon.tval == TV_HAFTED or current_weapon.tval == TV_MSTAFF or current_weapon.tval == TV_HELL_STAFF) then tskill = tskill + (p_ptr.skill[14] + (p_ptr.skill[14] / 2)) end
        if (current_weapon.tval == TV_POLEARM) then tskill = tskill + (p_ptr.skill[15] + (p_ptr.skill[15] / 2)) end
        if (current_weapon.tval == TV_DAGGER) then tskill = tskill + (p_ptr.skill[16] + (p_ptr.skill[16] / 2)) end
        if (current_weapon.tval == TV_AXE) then tskill = tskill + (p_ptr.skill[17] + (p_ptr.skill[17] / 2)) end
        if (current_weapon.tval == TV_ROD) then tskill = tskill + (p_ptr.skill[18] + (p_ptr.skill[18] / 2)) end
        if (current_weapon.tval == TV_ZELAR_WEAPON) then tskill = tskill + (p_ptr.skill[19] + (p_ptr.skill[19] / 2)) end

        k = maxroll(current_weapon.dd, current_weapon.ds)
	k = k + ((k * (p_ptr.abilities[(CLASS_FIGHTER * 10) + 1] * 10)) / 100)
	if (current_weapon.tval == TV_POLEARM and (p_ptr.skill[15] >= 90)) then k = k * 2 end
        k = k * (tskill + 1)
        k = k + ((k * p_ptr.dis_to_d) / 100)
        if (p_ptr.abilities[(CLASS_RANGER * 10) + 7] >= 1) then

		local strbonus = (p_ptr.stat_ind[A_STR+1] - 5)
		local useddex = (p_ptr.stat_ind[A_DEX+1] - 5)
		if (useddex > (p_ptr.abilities[(CLASS_RANGER * 10) + 7] * 5)) then

			useddex = (p_ptr.abilities[(CLASS_RANGER * 10) + 7] * 5)
		end
		if (useddex > strbonus) then

			k = k + ((k * useddex) / 100)
		else

			k = k + ((k * strbonus) / 100)
		end
        else 
		k = k + ((k * p_ptr.stat_ind[A_STR+1]) / 100)
	end
	if (p_ptr.powerattack > 0) then

		if (p_ptr.powerlevel == 1) then k = k * (2 + ((2 * (p_ptr.abilities[(CLASS_FIGHTER * 10)+1] * 5)) / 100))
		elseif (p_ptr.powerlevel == 2) then k = k * (3 + ((3 * (p_ptr.abilities[(CLASS_FIGHTER * 10)+1] * 5)) / 100))
		elseif (p_ptr.powerlevel == 3) then k = k * (4 + ((4 * (p_ptr.abilities[(CLASS_FIGHTER * 10)+1] * 5)) / 100))
		end
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

        mdice = (p_ptr.skill[19] / 15) + (p_ptr.skill[1] / 40) + 1
        mside = (p_ptr.skill[19] / 15) + (p_ptr.skill[1] / 40) + 3

	-- Damages bonus from gloves!
	glovebonus = 0
	if (inven(INVEN_HANDS).tval ~= 0 and inven(INVEN_HANDS).ac > 0) then
		glovebonus = 1
	end

        k = damroll(mdice, 1)
	k = k + ((k * (p_ptr.abilities[(CLASS_FIGHTER * 10) + 1] * 10)) / 100)
	k = k + ((p_ptr.skill[1] * (p_ptr.abilities[(CLASS_FIGHTER * 10) + 5] * 5)) / 100)
	k = k + glovebonus
	tskill = p_ptr.skill[1]
	tskill = tskill + (p_ptr.skill[19] + (p_ptr.skill[19] / 2))
        k = k * (tskill + 1)
        k = k + ((k * p_ptr.dis_to_d) / 100)
        if (p_ptr.abilities[(CLASS_RANGER * 10) + 7] >= 1) then

		local strbonus = (p_ptr.stat_ind[A_STR+1] - 5)
		local useddex = (p_ptr.stat_ind[A_DEX+1] - 5)
		if (useddex > (p_ptr.abilities[(CLASS_RANGER * 10) + 7] * 5)) then

			useddex = (p_ptr.abilities[(CLASS_RANGER * 10) + 7] * 5)
		end
		if (useddex > strbonus) then

			k = k + ((k * useddex) / 100)
		else

			k = k + ((k * strbonus) / 100)
		end
        else 
		k = k + ((k * p_ptr.stat_ind[A_STR+1]) / 100)
	end
	if (p_ptr.powerattack > 0) then

		if (p_ptr.powerlevel == 1) then k = k * (2 + ((2 * (p_ptr.abilities[(CLASS_FIGHTER * 10)+1] * 5)) / 100))
		elseif (p_ptr.powerlevel == 2) then k = k * (3 + ((3 * (p_ptr.abilities[(CLASS_FIGHTER * 10)+1] * 5)) / 100))
		elseif (p_ptr.powerlevel == 3) then k = k * (4 + ((4 * (p_ptr.abilities[(CLASS_FIGHTER * 10)+1] * 5)) / 100))
		end
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

        mdice = (p_ptr.skill[19] / 15) + (p_ptr.skill[1] / 40) + 1
        mside = (p_ptr.skill[19] / 15) + (p_ptr.skill[1] / 40) + 3

	-- Damages bonus from gloves!
	glovebonus = 0
	if (inven(INVEN_HANDS).tval ~= 0 and inven(INVEN_HANDS).ac > 0) then
		glovebonus = inven(INVEN_HANDS).ac
	end

        k = maxroll(mdice, mside)
	k = k + ((k * (p_ptr.abilities[(CLASS_FIGHTER * 10) + 1] * 10)) / 100)
	k = k + ((p_ptr.skill[1] * (p_ptr.abilities[(CLASS_FIGHTER * 10) + 5] * 5)) / 100)
	k = k + glovebonus
	tskill = p_ptr.skill[1]
	tskill = tskill + (p_ptr.skill[19] + (p_ptr.skill[19] / 2))
        k = k * (tskill + 1)
        k = k + ((k * p_ptr.dis_to_d) / 100)
        if (p_ptr.abilities[(CLASS_RANGER * 10) + 7] >= 1) then

		local strbonus = (p_ptr.stat_ind[A_STR+1] - 5)
		local useddex = (p_ptr.stat_ind[A_DEX+1] - 5)
		if (useddex > (p_ptr.abilities[(CLASS_RANGER * 10) + 7] * 5)) then

			useddex = (p_ptr.abilities[(CLASS_RANGER * 10) + 7] * 5)
		end
		if (useddex > strbonus) then

			k = k + ((k * useddex) / 100)
		else

			k = k + ((k * strbonus) / 100)
		end
        else 
		k = k + ((k * p_ptr.stat_ind[A_STR+1]) / 100)
	end
	if (p_ptr.powerattack > 0) then

		if (p_ptr.powerlevel == 1) then k = k * (2 + ((2 * (p_ptr.abilities[(CLASS_FIGHTER * 10)+1] * 5)) / 100))
		elseif (p_ptr.powerlevel == 2) then k = k * (3 + ((3 * (p_ptr.abilities[(CLASS_FIGHTER * 10)+1] * 5)) / 100))
		elseif (p_ptr.powerlevel == 3) then k = k * (4 + ((4 * (p_ptr.abilities[(CLASS_FIGHTER * 10)+1] * 5)) / 100))
		end
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

                local disroll = lua_randint((p_ptr.abilities[(CLASS_SHADOW * 10) + 2] * 10))
                local dismroll = lua_randint(((monster.hitrate + bonus) / 2))

                if (disroll >= dismroll) then return 0 end
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

                hitbonus = hitbonus + p_ptr.skill[10] * 2
                hitbonus = hitbonus + (p_ptr.stat_ind[A_CHR+1] - 5) * 5
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
