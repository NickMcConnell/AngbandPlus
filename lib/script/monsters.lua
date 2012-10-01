-- File: monsters.lua
-- This file contains various scripts and codes related to monsters.
-- Includes monster stats calculations, etc...

-- This function sets the stats of a monster, and is called by the hard-coded
-- "place_monster" functions.

-- Possible uses of the "misc" parameters:
-- 1. Force a specific level. Misc2 is the level.
-- 2. No elites/bosses.
-- 3. No elites/boss, specific level. Misc2 is the level.

function monster_stats (mon, sleep, friendly, dur, misc, misc2)

	local depth
	local elitestatbonus
	local monspeed
	local hpmult = 0
	local scaledbonus = 0

	-- Initialize some variables.
	mon.stunned = 0
	mon.confused = 0
	mon.monfear = 0
        mon.boss = 0
        mon.seallight = 0
	mon.hasted = 0
	mon.boosted = 0
	mon.summoned = 0
	mon.lives = 0
	mon.no_experience = FALSE
        mon.hold_o_idx = 0
	mon.str = 0
	mon.dex = 0
	mon.mind = 0
	mon.skill_attack = 0
	mon.skill_ranged = 0
	mon.skill_magic = 0
	mon.mana = 0
        mon.animated = FALSE
        mon.animdam_d = 0
        mon.animdam_s = 0
	mon.extra1 = 0
	mon.extra2 = 0
	mon.extra3 = 0
	mon.extra4 = 0
	mon.extra5 = 0
	elitestatbonus = 0
	crbonus = 0

	-- Monster's depth level.
	depth = m_race(mon.r_idx).level

	-- Give the monster a level.
	mon.level = depth

	if (mon.level < dun_level) then

		mon.level = mon.level + lua_randint((dun_level - mon.level))
	end

	-- Special: prepare a monster of a specific level.
	if (misc == 1 or misc == 3) then

		if (misc2 > 0) then
			mon.level = misc2
		end
	end

	-- Level must be at least 1.
	if (mon.level < 1) then mon.level = 1 end

	-- All stats and skills starts at 5.
	mon.str = 5
	mon.dex = 5
	mon.mind = 5
	mon.skill_attack = 5
	mon.skill_ranged = 5
	mon.skill_magic = 5
	mon.skill_evasion = 5
	mon.skill_mdef = 5
	mon.defense = 5

	-- Friendly monster.
	if ((friendly) or (get_monster_flag7(mon.r_idx, RF7_PET))) then

		set_pet(mon, TRUE)
	end

	-- Elite.
	if (not(p_ptr.inside_quest) and p_ptr.events[29004] == 1) then

		if (lua_randint(100) <= fate_monsters(3) and not(get_monster_flag1(mon.r_idx, RF1_UNIQUE))) then mon.boss = 1 end

        else
		if (lua_randint(100) <= (15 + (p_ptr.cursed * 2)) and not(get_monster_flag1(mon.r_idx, RF1_UNIQUE))) then mon.boss = 1 end
	end

	-- If the monster is elite, 20% chance of being a boss instead.
	if (mon.boss == 1) then

		if (not(is_pet(mon)) and not(get_monster_flag7(mon.r_idx, RF7_NEVER_BOSS)) and (dun_level > 0)) then

			if (lua_randint(100) <= 20) then

				mon.boss = 2
			end
		end
	end

	-- Force a monster to be elite/boss, or not.
	if (p_ptr.events[29027] > 0) then mon.boss = p_ptr.events[29027] end
	if (m_race(mon.r_idx).cursed > 0) then mon.boss = 0 end
	if (is_pet(mon)) then mon.boss = 0 end
	if (misc == 2 or misc == 3) then mon.boss = 0 end
	if (get_monster_flag7(mon.r_idx, RF7_NEVER_BOSS)) then mon.boss = 0 end

	-- Level adjustment.
	if (get_monster_flag7(mon.r_idx, RF7_SCALED)) then

		if ((p_ptr.lev + (p_ptr.lev / 2)) < m_race(mon.r_idx).level) then

			mon.level = m_race(mon.r_idx).level
		else
			mon.level = p_ptr.lev + (p_ptr.lev / 2)
		end

		scaledbonus = mon.level

	elseif (mon.boss > 0 or m_race(mon.r_idx).cursed > 0) then

		if (not((misc == 1 or misc == 3) and (misc2 > 0))) then
			if (dun_level < m_race(mon.r_idx).level) then
				mon.level = m_race(mon.r_idx).level
			else
				mon.level = dun_level
			end

			if (m_race(mon.r_idx).cursed > 0) then

				mon.level = mon.level + (p_ptr.cursed * 2)
			end
		end
	end

	-- If quests scaling has been turned on and we're in a quest, scale the level.
	if ((p_ptr.inside_quest) and (quest_scaling)) then

		local scaleamount

		scaleamount = p_ptr.lev + multiply_divide(p_ptr.lev, 25, 100)

		if (mon.level < scaleamount) then

			mon.level = scaleamount
		end
	end

	-- Increase all stats and skills by 2 for every levels above 1.
	if (mon.level > 1) then

		mon.str = mon.str + ((mon.level - 1) * 2)
		mon.dex = mon.dex + ((mon.level - 1) * 2)
		mon.mind = mon.mind + ((mon.level - 1) * 2)
		mon.skill_attack = mon.skill_attack + ((mon.level - 1) * 2)
		mon.skill_ranged = mon.skill_ranged + ((mon.level - 1) * 2)
		mon.skill_magic = mon.skill_magic + ((mon.level - 1) * 2)
		mon.skill_evasion = mon.skill_evasion + ((mon.level - 1) * 2)
		mon.skill_mdef = mon.skill_mdef + ((mon.level - 1) * 2)
		mon.defense = mon.defense + ((mon.level - 1) * 2)
	end

	-- Further 2% bonus for each levels.
	mon.str = mon.str + multiply_divide(mon.str, mon.level * 2, 100)
	mon.dex = mon.dex + multiply_divide(mon.dex, mon.level * 2, 100)
	mon.mind = mon.mind + multiply_divide(mon.mind, mon.level * 2, 100)
	mon.skill_attack = mon.skill_attack + multiply_divide(mon.skill_attack, mon.level * 2, 100)
	mon.skill_ranged = mon.skill_ranged + multiply_divide(mon.skill_ranged, mon.level * 2, 100)
	mon.skill_magic = mon.skill_magic + multiply_divide(mon.skill_magic, mon.level * 2, 100)
	mon.skill_evasion = mon.skill_evasion + multiply_divide(mon.skill_evasion, mon.level * 2, 100)
	mon.skill_mdef = mon.skill_mdef + multiply_divide(mon.skill_mdef, mon.level * 2, 100)
	mon.defense = mon.defense + multiply_divide(mon.defense, mon.level * 2, 100)

	-- And yet another 1%.
	-- Doesn't do much early on, but later on, this will make monsters really tough!
	mon.str = mon.str + multiply_divide(mon.str, mon.level, 100)
	mon.dex = mon.dex + multiply_divide(mon.dex, mon.level, 100)
	mon.mind = mon.mind + multiply_divide(mon.mind, mon.level, 100)
	mon.skill_attack = mon.skill_attack + multiply_divide(mon.skill_attack, mon.level, 100)
	mon.skill_ranged = mon.skill_ranged + multiply_divide(mon.skill_ranged, mon.level, 100)
	mon.skill_magic = mon.skill_magic + multiply_divide(mon.skill_magic, mon.level, 100)
	mon.skill_evasion = mon.skill_evasion + multiply_divide(mon.skill_evasion, mon.level, 100)
	mon.skill_mdef = mon.skill_mdef + multiply_divide(mon.skill_mdef, mon.level, 100)
	mon.defense = mon.defense + multiply_divide(mon.defense, mon.level, 100)

	-- Elite/Boss stats modifiers.
	if (m_race(mon.r_idx).cursed > 0) then

		elitestatbonus = 100
	elseif (mon.boss == 2 or get_monster_flag7(mon.r_idx, RF7_SCALED)) then

		elitestatbonus = 50
	elseif (mon.boss == 1) then

		elitestatbonus = 25
	end

	-- Apply a modifier for CR.
	if (m_race(mon.r_idx).cr >= 6) then

		crbonus = 100
	elseif (m_race(mon.r_idx).cr >= 3) then

		crbonus = 50
	elseif (m_race(mon.r_idx).cr >= 2) then

		crbonus = 25
	else
		crbonus = 0
	end

	-- Use either the elite/boss modifier, or CR modifier, but not both.
	if (crbonus > elitestatbonus) then

		elitestatbonus = crbonus
	end

	mon.str = mon.str + multiply_divide(mon.str, elitestatbonus, 100)
	mon.dex = mon.dex + multiply_divide(mon.dex, elitestatbonus, 100)
	mon.mind = mon.mind + multiply_divide(mon.mind, elitestatbonus, 100)
	mon.skill_attack = mon.skill_attack + multiply_divide(mon.skill_attack, elitestatbonus, 100)
	mon.skill_ranged = mon.skill_ranged + multiply_divide(mon.skill_ranged, elitestatbonus, 100)
	mon.skill_magic = mon.skill_magic + multiply_divide(mon.skill_magic, elitestatbonus, 100)
	mon.skill_evasion = mon.skill_evasion + multiply_divide(mon.skill_evasion, elitestatbonus, 100)
	mon.skill_mdef = mon.skill_mdef + multiply_divide(mon.skill_mdef, elitestatbonus, 100)
	mon.defense = mon.defense + multiply_divide(mon.defense, elitestatbonus, 100)

	-- Determine monster's hp.
	if (get_monster_flag1(mon.r_idx, RF1_FORCE_MAXHP)) then
		mon.maxhp = mon.level * (10 + (mon.level - 1))
		--mon.maxhp = mon.level * multiply_divide(10, 100 + ((mon.level-1) * 5), 100)
	else
		if (mon.level > 1) then
			mon.maxhp = (lua_randint(mon.level / 2) + (mon.level / 2)) * (10 + (mon.level - 1))
		else
			mon.maxhp = mon.level * (10 + (mon.level - 1))
		end
	end
	mon.maxhp = mon.maxhp + multiply_divide(mon.maxhp, (mon.level-1) * 110, 100)
	--mon.maxhp = mon.maxhp + multiply_divide(mon.maxhp, (mon.level-1) * 8, 100)
	--mon.maxhp = mon.maxhp + multiply_divide(mon.maxhp, (mon.level-1) * 3, 100)

	-- Elites/Boss modifiers.
	if (m_race(mon.r_idx).cursed > 0) then

		hpmult = hpmult + 5
	elseif (mon.boss == 2 or get_monster_flag7(mon.r_idx, RF7_SCALED)) then

		hpmult = hpmult + 2
	elseif (mon.boss == 1) then

		hpmult = hpmult + 1
	end

	-- CR modifier
	hpmult = hpmult + m_race(mon.r_idx).cr

	-- HP multiplier.
	mon.maxhp = mon.maxhp * hpmult

	-- Boss abilities.
	if (mon.boss > 0) then

		local abnum

		abnum = 1
		if (mon.boss == 2) then

			abnum = abnum + (mon.level / 20)
		end

		if (abnum > 3) then abnum = 3 end

		while (abnum > 0) do

			local whichone

			whichone = lua_randint(3)

			if (whichone == 1) then

				if (not(get_monster_ability(mon, BOSS_DOUBLE_DAMAGES))) then
					give_monster_ability(mon, BOSS_DOUBLE_DAMAGES)
					abnum = abnum - 1
				end
			end
			if (whichone == 2) then

				if (not(get_monster_ability(mon, BOSS_HALVE_DAMAGES))) then
					give_monster_ability(mon, BOSS_HALVE_DAMAGES)
					abnum = abnum - 1
				end
			end
			--if (whichone == 3) then

			--	if (not(get_monster_ability(mon, BOSS_CURSED_HITS))) then
			--		give_monster_ability(mon, BOSS_CURSED_HITS)
			--		abnum = abnum - 1
			--	end
			--end
			if (whichone == 3) then

				if (not(get_monster_ability(mon, BOSS_DOUBLE_MAGIC))) then
					give_monster_ability(mon, BOSS_DOUBLE_MAGIC)
					abnum = abnum - 1
				end
			end
		end
	end

	-- Apply the percentile stats modifiers.
	-- May be positive or negative.
	mon.str = multiply_divide(mon.str, m_race(mon.r_idx).str + scaledbonus, 100)
	mon.dex = multiply_divide(mon.dex, m_race(mon.r_idx).dex + scaledbonus, 100)
	mon.mind = multiply_divide(mon.mind, m_race(mon.r_idx).mind + scaledbonus, 100)
	mon.skill_attack = multiply_divide(mon.skill_attack, m_race(mon.r_idx).skill_attack + scaledbonus, 100)
	mon.skill_ranged = multiply_divide(mon.skill_ranged, m_race(mon.r_idx).skill_ranged + scaledbonus, 100)
	mon.skill_magic = multiply_divide(mon.skill_magic, m_race(mon.r_idx).skill_magic + scaledbonus, 100)
	mon.skill_evasion = multiply_divide(mon.skill_evasion, m_race(mon.r_idx).skill_evasion + scaledbonus, 100)
	mon.skill_mdef = multiply_divide(mon.skill_mdef, m_race(mon.r_idx).skill_mdef + scaledbonus, 100)
	mon.defense = multiply_divide(mon.defense, m_race(mon.r_idx).ac + scaledbonus, 100)
	mon.maxhp = multiply_divide(mon.maxhp, m_race(mon.r_idx).hp + scaledbonus, 100)

	-- Set current hp to max hp.
	mon.hp = mon.maxhp

	-- Mana.
	if (mon.mind > 5) then
		mon.mana = (mon.mind - 5) * 10

		-- Elites/Boss modifiers.
		if (m_race(mon.r_idx).cursed > 0) then

			mon.mana = mon.mana * 10
		elseif (mon.boss == 2 or get_monster_flag7(mon.r_idx, RF7_SCALED)) then

			mon.mana = mon.mana * 4
		elseif (mon.boss == 1) then

			mon.mana = mon.mana * 2
		end
	end

	-- Sleeping.
	if ((sleep) and (m_race(mon.r_idx).sleep > 0) and mon.boss == 0) then

		local val
		val = m_race(mon.r_idx).sleep
		mon.csleep = ((val * 2) + lua_randint(val * 10))
	end

	-- Summoned.
	mon.summoned = dur
	if (mon.summoned > 0) then mon.no_experience = TRUE end
	if (mon.summoned < 0) then

		mon.summoned = 0
		mon.no_experience = TRUE
	end

	-- Speed.
	if (m_race(mon.r_idx).cr >= 2) then
		monspeed = m_race(mon.r_idx).speed + (mon.level / 2)
	else
		monspeed = m_race(mon.r_idx).speed + (mon.level / 3)
	end
	if (monspeed > 180) then monspeed = 180 end
	mon.mspeed = monspeed

	-- Lives.
	mon.lives = m_race(mon.r_idx).lives
	if (m_race(mon.r_idx).livesscalefactor > 0) then
		mon.lives = mon.lives + ((mon.level / m_race(mon.r_idx).livesscalefactor) * m_race(mon.r_idx).livesscale)
	end
end

-- A melee attack by a monster.
function monster_melee_attack(m_idx)

	local m_name
	local oldmx
	local oldmy
	local i
	local j
	local blows
	local totalblows

	-- Not allowed to attack
	if (get_monster_flag1(monster(m_idx).r_idx, RF1_NEVER_BLOW)) then return end

	-- Do not attack if friendly.
	if (is_pet(monster(m_idx))) then return end

	-- Mutilate Arms.
	-- Not actually permanent.
	if (get_monster_ability(monster(m_idx), MUTILATE_ARMS)) then

		local ppower
		local mpower

		ppower = p_ptr.skill[17] + p_ptr.stat_ind[A_STR+1]
		mpower = monster(m_idx).level + monster(m_idx).str

		if (lua_randint(mpower) >= lua_randint(ppower)) then
		
			remove_monster_ability(monster(m_idx), MUTILATE_ARMS)
		end

		return 0
	end

	-- Get monster's name.
	m_name = m_race(monster(m_idx).r_idx).name_char

	-- Get the "died from" information (i.e. "a kobold")
	-- monster_desc(ddesc, m_ptr, 0x88);

	-- Assume no blink
	-- blinked = FALSE;

	-- Original monster position(in case it gets moved by an ability or something)
	oldmx = monster(m_idx).fx
	oldmy = monster(m_idx).fy

	-- Determine the total number of blows.
	-- There's an initial amount, but it can increase with the level. It varies from monsters to monsters.
	totalblows = m_race(monster(m_idx).r_idx).attacks
	if (m_race(monster(m_idx).r_idx).attacksscalefactor > 0) then
		totalblows = totalblows + ((monster(m_idx).level / m_race(monster(m_idx).r_idx).attacksscalefactor) * m_race(monster(m_idx).r_idx).attacksscale)
	end

	-- Counter attacks.
	-- mcounter is a global variable that is set to TRUE when the "Counter Attack" counter is triggered. (see elements.lua)
	-- monster_counter_attack is another one to tell that the attack is a counter attack.
	monster_counter_attack = FALSE
	if (mcounter) then

		blows = totalblows - 1
		mcounter = FALSE
		monster_counter_attack = TRUE
	else blows = 0
	end

	-- Attack until we have no more blows!
	for i = blows, (totalblows-1) do

		local visible = FALSE
		local obvious = FALSE
		local nothurt = FALSE

		local power = 0
		local numattacks = 0
		local chosen = 0
                local damage = 0

		-- cptr act = NULL;

		-- Stop if player is dead or gone
		if (not(alive) or (death)) then break end

		-- Handle "leaving"
		if (p_ptr.leaving) then return 0 end

		-- Make sure the monster is still alive
		if (not(is_alive(monster(m_idx)))) then break end

		-- If the monster moved somehow, cancel attack.
		if (not(monster(m_idx).fx == oldmx) or not(monster(m_idx).fy == oldmy)) then break end

		-- Extract visibility (before blink)
		if (monster(m_idx).ml) then visible = TRUE end

		-- We can have up to 20 different attacks.
		-- Let's choose one!
		j = 1
		while (m_race(monster(m_idx).r_idx).attack[j].type > 0 and numattacks <= 20) do

			numattacks = numattacks + 1
			j = j + 1
		end

		-- Now, choose an attack!
		chosen = lua_randint(numattacks)

		-- Learn that the monster can do that. (this makes the attack's name show up in the recall info.)
		m_race(monster(m_idx).r_idx).r_blows[chosen] = 1
                
		-- What type of attack did the monster do?
		-- Type 1: Normal attack.
		-- Type 999: Scripted attack.
		if (m_race(monster(m_idx).r_idx).attack[chosen].type == 1) then

			-- Normal attack
			-- Call the "monster_before_melee" function. (see events.lua)
			if (m_race(monster(m_idx).r_idx).event_before_melee > 0) then

				monster_before_melee(m_idx, m_race(monster(m_idx).r_idx).event_before_melee)
			end

			-- Try to hit the player.
			if (monster_hit_player(monster(m_idx), monster(m_idx).skill_attack) == 1) then

				local totaldd
				local totalds

				totaldd = m_race(monster(m_idx).r_idx).attack[chosen].ddice
				if (m_race(monster(m_idx).r_idx).attack[chosen].ddscalefactor > 0) then
					totaldd = totaldd + ((monster(m_idx).level / m_race(monster(m_idx).r_idx).attack[chosen].ddscalefactor) * m_race(monster(m_idx).r_idx).attack[chosen].ddscale)
				end
				totalds = m_race(monster(m_idx).r_idx).attack[chosen].dside
				if (m_race(monster(m_idx).r_idx).attack[chosen].dsscalefactor > 0) then
					totalds = totalds + ((monster(m_idx).level / m_race(monster(m_idx).r_idx).attack[chosen].dsscalefactor) * m_race(monster(m_idx).r_idx).attack[chosen].dsscale)
				end
			
				damage = monster_damages(monster(m_idx), totaldd, totalds, monster(m_idx).str)

				-- Bosses may get higher damages!
				if (get_monster_ability(monster(m_idx), BOSS_IMMUNE_MAGIC)) then damage = damage * 2 end
				if (get_monster_ability(monster(m_idx), CURSE_HALVE_DAMAGES)) then damage = damage / 2 end

				-- Message.
				if (m_race(monster(m_idx).r_idx).attack[chosen].name == "!") then

					msg_print(string.format('%s %s you!', m_name, m_race(monster(m_idx).r_idx).attack[chosen].act))
				else
					msg_print(string.format('%s %s you with %s!', m_name, m_race(monster(m_idx).r_idx).attack[chosen].act, m_race(monster(m_idx).r_idx).attack[chosen].name))
				end
					
				-- Damages Curse. Disabled for now.
                        	--if (m_ptr->abilities & (CURSE_DAMAGES_CURSE))
                        	--{
                                --	m_ptr->hp -= damage * p_ptr->abilities[(CLASS_MAGE * 10) + 6];
                        	--}

                        	-- Active Physical Resistance spell.
                        	if (p_ptr.pres_dur > 0 and m_race(monster(m_idx).r_idx).attack[chosen].element == GF_PHYSICAL) then

					local dampercent

					dampercent = multiply_divide(damage, p_ptr.pres, 100)
					damage = damage - dampercent
                        	end

				-- Attempt to block the hit.
				-- First, you must have a weapon or shield that has a base AC value.
				-- Then, the roll is your strength + defense skill, vs monster's strength + skill.
				if ((not(inven(INVEN_WIELD).tval == 0) or not(inven(INVEN_WIELD+1).tval == 0)) and damage > 0) then

					if (inven(INVEN_WIELD).ac > 0 or inven(INVEN_WIELD+1).ac > 0 or (unarmed() and p_ptr.skill_base[19] >= 40)) then

						local proll
						local mroll
						local blockbonus = 0

						proll = p_ptr.stat_ind[A_STR+1] + p_ptr.skill[5]
						mroll = monster(m_idx).str + monster(m_idx).skill_attack

						-- Also add Wisdom if we're unarmed and we have Strength through Spirit.
						if ((p_ptr.abilities[(CLASS_MONK * 10) + 1] >= 1) and unarmed()) then

							local wbonus = 0

							wbonus = p_ptr.abilities[(CLASS_MONK * 10) + 1] * 10
							if (wbonus > 100) then wbonus = 100 end

							proll = proll + multiply_divide(p_ptr.stat_ind[A_WIS+1], wbonus, 100)
        					end

						-- Your shield's base AC provides a percentile bonus AND a flat bonus.
						if (not(inven(INVEN_WIELD).tval == 0)) then

							proll = proll + inven(INVEN_WIELD).ac
							blockbonus = blockbonus + inven(INVEN_WIELD).ac
						end
						if (not(inven(INVEN_WIELD+1).tval == 0)) then

							proll = proll + inven(INVEN_WIELD+1).ac
							blockbonus = blockbonus + inven(INVEN_WIELD+1).ac
						end

						-- Warrior's balanced Warrior.
						if (p_ptr.abilities[(CLASS_WARRIOR * 10) + 4] >= 1) then

							blockbonus = blockbonus + (p_ptr.abilities[(CLASS_WARRIOR * 10) + 4] * 5)
						end

						-- Defender's Shield Mastery.
						if (p_ptr.abilities[(CLASS_DEFENDER * 10) + 2] >= 1 and shield_has()) then

							blockbonus = blockbonus + (p_ptr.abilities[(CLASS_DEFENDER * 10) + 2] * 15)
						end

						-- Defender's Universal Avoidance.
						if (p_ptr.abilities[(CLASS_DEFENDER * 10) + 6] >= 1) then

							blockbonus = blockbonus + (p_ptr.abilities[(CLASS_DEFENDER * 10) + 6] * 10)
						end

						proll = proll + multiply_divide(proll, blockbonus, 100)

						if (lua_randint(proll) >= lua_randint(mroll)) then

							msg_print("You block!")
							damage = 0
						end
					end
				end

				if (damage > 0) then

					no_magic_return = TRUE
					monster_physical = TRUE
					lua_project(m_idx, 0, py, px, damage, m_race(monster(m_idx).r_idx).attack[chosen].element, 2)
					monster_physical = FALSE
					no_magic_return = FALSE

					-- Boss "Cursed Hits" ability.
					if (get_monster_ability(monster(m_idx), BOSS_CURSED_HITS)) then

						local ppower
						local mpower

						-- Player's power is a combination of constitution, wisdom and level.
						ppower = p_ptr.lev + p_ptr.stat_ind[A_WIS+1] + p_ptr.stat_ind[A_CON+1]

						-- Monster's power is a combination of monster's level and attack skill.
						mpower = monster(m_idx).level + monster(m_idx).skill_attack

						-- Roll.
						if (lua_randint(mpower) >= lua_randint(ppower)) then

							msg_print(string.format('%s cursed you!', m_name))
							if (not(p_ptr.resist_conf)) then set_confused(10) end
							if (not(p_ptr.resist_fear)) then set_afraid(10) end
							if (not(p_ptr.resist_blind)) then set_blind(10) end
						end

					end

					-- Other effects(drains, etc...)

					-- Effects 1 to 6 are stats drains of str, int, wis, dex, con or chr.
					if (m_race(monster(m_idx).r_idx).attack[chosen].special1 >= 1 and m_race(monster(m_idx).r_idx).attack[chosen].special1 <= 6) then

						local sust = FALSE
						local special1 = m_race(monster(m_idx).r_idx).attack[chosen].special1

						if (special1 == 1) then sust = p_ptr.sustain_str end
						if (special1 == 2) then sust = p_ptr.sustain_int end
						if (special1 == 3) then sust = p_ptr.sustain_wis end
						if (special1 == 4) then sust = p_ptr.sustain_dex end
						if (special1 == 5) then sust = p_ptr.sustain_con end
						if (special1 == 6) then sust = p_ptr.sustain_chr end

						if (not(sust)) then

							dec_stat(special1-1, m_race(monster(m_idx).r_idx).attack[chosen].special2, 2)
							update_and_handle()
						end
					end

					-- Effect 7 is a "drain all" effect.
					if (m_race(monster(m_idx).r_idx).attack[chosen].special1 == 7) then

						if (not(p_ptr.sustain_str)) then dec_stat(A_STR, m_race(monster(m_idx).r_idx).attack[chosen].special2, 2) end
						if (not(p_ptr.sustain_int)) then dec_stat(A_INT, m_race(monster(m_idx).r_idx).attack[chosen].special2, 2) end
						if (not(p_ptr.sustain_wis)) then dec_stat(A_WIS, m_race(monster(m_idx).r_idx).attack[chosen].special2, 2) end
						if (not(p_ptr.sustain_dex)) then dec_stat(A_DEX, m_race(monster(m_idx).r_idx).attack[chosen].special2, 2) end
						if (not(p_ptr.sustain_con)) then dec_stat(A_CON, m_race(monster(m_idx).r_idx).attack[chosen].special2, 2) end
						if (not(p_ptr.sustain_chr)) then dec_stat(A_CHR, m_race(monster(m_idx).r_idx).attack[chosen].special2, 2) end

						update_and_handle()
					end

					-- Effect 8 is drain experience.
					if (m_race(monster(m_idx).r_idx).attack[chosen].special1 == 8) then

						if (not(p_ptr.hold_life)) then

							msg_print("Your experience has been reduced!")
							lose_exp(m_race(monster(m_idx).r_idx).attack[chosen].special2)
							update_and_handle()
						end
					end
				end

			-- Monster miss the player.
			else

				msg_print(string.format('%s misses you.', m_name))

				if (unarmed() and p_ptr.abilities[(CLASS_MONK * 10) + 4] >= 1) then

					throw_monster(m_idx)
				end
			end

			-- Counter Attack code here(assuming this isn't a counter attack from the monster itself)
			-- Assuming we're alive, of course.
			if (p_ptr.chp >= 0 and not(monster_counter_attack) and not(inven(INVEN_WIELD).tval == 0)) then

				if (p_ptr.powerattack > 0 and p_ptr.abilities[(CLASS_FIGHTER * 10) + 10] >= 1) then

					local cdam
					local dtype

					current_weapon = inven(INVEN_WIELD)
					cdam = weapon_damages()
					cdam = cdam + multiply_divide(cdam, p_ptr.abilities[(CLASS_FIGHTER * 10) + 10] * 10, 100)

					dtype = current_weapon.extra1
					if (dtype == 0) then dtype = GF_PHYSICAL end

					melee_attack = TRUE
					no_magic_return = TRUE
					lovebattle = 1
					lua_project(0, 0, monster(m_idx).fy, monster(m_idx).fx, cdam, dtype, 2)
					lovebattle = 0
					no_magic_return = FALSE
					melee_attack = FALSE

				elseif (p_ptr.abilities[(CLASS_WARRIOR * 10) + 8] >= 1) then

					local cdam
					local dtype

					current_weapon = inven(INVEN_WIELD)
					cdam = weapon_damages()
					cdam = cdam + multiply_divide(cdam, p_ptr.abilities[(CLASS_WARRIOR * 10) + 8] * 10, 100)

					dtype = current_weapon.extra1
					if (dtype == 0) then dtype = GF_PHYSICAL end

					melee_attack = TRUE
					no_magic_return = TRUE
					lua_project(0, 0, monster(m_idx).fy, monster(m_idx).fx, cdam, dtype, 2)
					no_magic_return = FALSE
					melee_attack = FALSE
				end
			end

			-- Elemental Armor's damage.
			if (p_ptr.abilities[(CLASS_ELEM_LORD * 10) + 4] > 0) then

				local basepower = 0
				local edam = 0
				basepower = p_ptr.abilities[(CLASS_ELEM_LORD * 10) + 4] * 3

				edam = spell_damages(basepower, A_INT, 1)

				no_magic_return = TRUE
				lua_project(0, 0, monster(m_idx).fy, monster(m_idx).fx, edam, p_ptr.elemlord, 2)
				no_magic_return = FALSE
			end

			-- Call the "monster_after_melee" function. (see events.lua)
			if (m_race(monster(m_idx).r_idx).event_after_melee > 0) then

				monster_after_melee(m_idx, m_race(monster(m_idx).r_idx).event_after_melee)
			end

			-- Memorize the attack.
			if (seen) then

				m_race(monster(m_idx).r_idx).r_blows[chosen] = 1	
			end
		end

		-- Scripted attack.
		-- We need to use a hardcoded function to call the code.
		if (m_race(monster(m_idx).r_idx).attack[chosen].type == 999) then

			-- Call the "monster_before_melee" function. (see events.lua)
			if (m_race(monster(m_idx).r_idx).event_before_melee > 0) then

				monster_before_melee(m_idx, m_race(monster(m_idx).r_idx).event_before_melee)
			end

			-- Use the function.
			lua_monster_script(m_idx, m_race(monster(m_idx).r_idx).attack[chosen].name)

			-- Call the "monster_after_melee" function. (see events.lua)
			if (m_race(monster(m_idx).r_idx).event_after_melee > 0) then

				monster_after_melee(m_idx, m_race(monster(m_idx).r_idx).event_after_melee)
			end
		end

		-- Defender's Interrupt ability.
		if (p_ptr.abilities[(CLASS_DEFENDER * 10) + 7] > 0 and player_next_to_monster(monster(m_idx))) then

			local proll
			local mroll
			local skillpercent

			skillpercent = p_ptr.abilities[(CLASS_DEFENDER * 10) + 7] * 10
			if (skillpercent > 100) then skillpercent = 100 end

			proll = multiply_divide(p_ptr.skill[5], skillpercent, 100) * 2
			mroll = monster(m_idx).skill_attack

			proll = proll + multiply_divide(proll, p_ptr.abilities[(CLASS_DEFENDER * 10) + 7] * 20, 100)

			if (lua_randint(proll) >= lua_randint(mroll)) then

				msg_print(string.format('You interrupt the attacks of %s!', m_name))
				i = totalblows
			end
		end
	end                                   

	-- Always notice cause of death
	-- It's a short signed integer, so the max is 32767.
	if (death and (m_race(monster(m_idx).r_idx).r_deaths < 32767)) then

		m_race(monster(m_idx).r_idx).r_deaths = m_race(monster(m_idx).r_idx).r_deaths + 1
	end

	-- Attack successful.
	return 1
end

-- Ranged attacks of monsters.
function monster_ranged_attack(m_idx)

	local m_name = ""
	local ranged = {}
	local i
	local j
	local shots = 0
	local totalshots = 0
	local found_ranged = FALSE
	local chosen = 0
	local taunt = 0

	-- Target location
	local x = px
	local y = py
	 
	-- Do we see the monster?
	local seen = (monster(m_idx).ml)

	-- Assume "projectable"
	local direct = TRUE

	monster_counter_attack = FALSE

	-- Cannot shoot when confused
	if (monster(m_idx).confused > 0) then return 0 end

	-- Friendly monsters won't shoot us.
	if (is_pet(monster(m_idx))) then return 0 end	 
	 
	-- Verify if the player can be shot.
	-- Check range. MAX_RANGE is hardcoded to 18.
	if (monster(m_idx).cdis > MAX_RANGE) then return 0 end

	--Check if player can be reached.
	if (not(projectable(monster(m_idx).fy, monster(m_idx).fx, py, px))) then return 0 end

	-- Check if a monster is in the way.
	if (not(clean_shot(monster(m_idx).fy, monster(m_idx).fx, py, px))) then return 0 end

	-- Player stealth/invisibility.
	if (player_invis(monster(m_idx))) then return 0 end

	-- Stop if player is dead or gone
	-- (Not exactly sure why the code had a check for both "not alive" and death...)
	if (not(alive) or death) then return 0 end
 
	-- Stop if player is leaving
	if (p_ptr.leaving) then return 0 end

	-- Get monster's name.
	m_name = m_race(monster(m_idx).r_idx).name_char

	-- Reset ranged count.
	for j = 1, 20 do ranged[j] = 0 end

	-- Scan the various attacks. Try to find some type 3 attacks, which are ranged.
	-- Can also use type 1000 attacks, which are scripted ranged.
	i = 1
	for j = 1, 20 do

		if (m_race(monster(m_idx).r_idx).attack[j].type == 3 or m_race(monster(m_idx).r_idx).attack[j].type == 1000) then

			ranged[i] = j
			i = i + 1
			found_ranged = TRUE
		end
	end
	
	-- If no ranged attacks were found, return.
	if (not(found_ranged)) then return 0 end

	-- Pick a random ranged attack!
	chosen = lua_randint(i)

	-- Taunt can reduce the chance to shoot.
	-- If the chance goes below 20% while taunted, monster will rather melee.
	if (get_monster_ability(monster(m_idx), TAUNTED)) then

		taunt = 25 + (p_ptr.abilities[(CLASS_FIGHTER * 10) + 3] - 1)
		if ((m_race(monster(m_idx).r_idx).attack[chosen].special2 - taunt) <= 20) then return 0 end
	end

	-- If it's a type 1000, process the code here.
	if (m_race(monster(m_idx).r_idx).attack[chosen].type == 1000) then

		-- Call the "monster_before_ranged" function. (see events.lua)
		if (m_race(monster(m_idx).r_idx).event_before_ranged > 0) then

			monster_before_ranged(m_idx, m_race(monster(m_idx).r_idx).event_before_ranged)
		end

		-- Use the function.
		lua_monster_script(m_idx, m_race(monster(m_idx).r_idx).attack[chosen].name)

		-- Call the "monster_after_ranged" function. (see events.lua)
		if (m_race(monster(m_idx).r_idx).event_after_ranged > 0) then

			monster_after_ranged(m_idx, m_race(monster(m_idx).r_idx).event_after_ranged)
		end

		-- It counts as having shot.
		return 1
	end

	-- Now that we have an attack, shoot.
	-- Some monsters may choose not to shoot.
	if (lua_randint(100) <= (m_race(monster(m_idx).r_idx).attack[chosen].special2 - taunt) or (mcounter)) then

		-- Determine the total number of shots.
		-- There's an initial amount, but it can increase with the level. It varies from monsters to monsters.
		totalshots = m_race(monster(m_idx).r_idx).shots
		if (m_race(monster(m_idx).r_idx).shotsscalefactor > 0) then
			totalshots = totalshots + ((monster(m_idx).level / m_race(monster(m_idx).r_idx).shotsscalefactor) * m_race(monster(m_idx).r_idx).shotsscale)
		end

		-- Counter attacks.
		-- mcounter is a global variable that is set to TRUE when the "Counter Attack" counter is triggered. (see elements.lua)
		-- monster_counter_attack is another one to tell that the attack is a counter attack.
		monster_counter_attack = FALSE
		if (mcounter) then

			shots = totalshots - 1
			mcounter = FALSE
			monster_counter_attack = TRUE
		else shots = 0
		end

		-- Shoot once for every shots!
		for i = shots, (totalshots-1) do

			local damage

			-- Return if we're leaving.
			if (p_ptr.leaving) then return 0 end

			-- Call the "monster_before_ranged" function. (see events.lua)
			if (m_race(monster(m_idx).r_idx).event_before_ranged > 0) then

				monster_before_ranged(m_idx, m_race(monster(m_idx).r_idx).event_before_ranged)
			end

			-- Try to hit the player.
			if (monster_hit_player(monster(m_idx), monster(m_idx).skill_ranged) == 1) then

				local totaldd
				local totalds

				totaldd = m_race(monster(m_idx).r_idx).attack[chosen].ddice
				if (m_race(monster(m_idx).r_idx).attack[chosen].ddscalefactor > 0) then
					totaldd = totaldd + ((monster(m_idx).level / m_race(monster(m_idx).r_idx).attack[chosen].ddscalefactor) * m_race(monster(m_idx).r_idx).attack[chosen].ddscale)
				end
				totalds = m_race(monster(m_idx).r_idx).attack[chosen].dside
				if (m_race(monster(m_idx).r_idx).attack[chosen].dsscalefactor > 0) then
					totalds = totalds + ((monster(m_idx).level / m_race(monster(m_idx).r_idx).attack[chosen].dsscalefactor) * m_race(monster(m_idx).r_idx).attack[chosen].dsscale)
				end
			
				damage = monster_damages(monster(m_idx), totaldd, totalds, monster(m_idx).dex)

				-- Bosses may get higher damages!
				if (get_monster_ability(monster(m_idx), BOSS_IMMUNE_MAGIC)) then damage = damage * 2 end
				if (get_monster_ability(monster(m_idx), CURSE_HALVE_DAMAGES)) then damage = damage / 2 end

				-- Message.
				if (m_race(monster(m_idx).r_idx).attack[chosen].name == "!") then

					msg_print(string.format('%s %s you!', m_name, m_race(monster(m_idx).r_idx).attack[chosen].act))
				else
					msg_print(string.format('%s %s you with %s!', m_name, m_race(monster(m_idx).r_idx).attack[chosen].act, m_race(monster(m_idx).r_idx).attack[chosen].name))
				end

				if (damage > 0) then

					no_magic_return = TRUE
					monster_ranged = TRUE
					lua_project(m_idx, m_race(monster(m_idx).r_idx).attack[chosen].special1, py, px, damage, m_race(monster(m_idx).r_idx).attack[chosen].element, 2)
					monster_ranged = FALSE
					no_magic_return = FALSE
				end

			-- Monster miss the player.
			else

				msg_print(string.format('%s %s you, but you dodge.', m_name, m_race(monster(m_idx).r_idx).attack[chosen].act))
			end

			-- Call the "monster_after_ranged" function. (see events.lua)
			if (m_race(monster(m_idx).r_idx).event_after_ranged > 0) then

				monster_after_ranged(m_idx, m_race(monster(m_idx).r_idx).event_after_ranged)
			end

			-- Memorize the attack.
			if (seen) then

				m_race(monster(m_idx).r_idx).r_blows[chosen] = 1	
			end

			-- Counter Shot Marksman ability.
			if (p_ptr.chp > 0 and p_ptr.abilities[(CLASS_MARKSMAN * 10) + 4] > 0 and p_ptr.events[29046] == 1 and not(monster_counter_attack)) then

				p_ptr.events[29047] = m_idx
				use_ability(120)
				p_ptr.events[29047] = 0
			end

			-- Defender's Interrupt ability.
			if (p_ptr.abilities[(CLASS_DEFENDER * 10) + 7] > 0 and player_next_to_monster(monster(m_idx))) then

				local proll
				local mroll
				local skillpercent

				skillpercent = p_ptr.abilities[(CLASS_DEFENDER * 10) + 7] * 10
				if (skillpercent > 100) then skillpercent = 100 end

				proll = multiply_divide(p_ptr.skill[5], skillpercent, 100) * 2
				mroll = monster(m_idx).skill_ranged

				proll = proll + multiply_divide(proll, p_ptr.abilities[(CLASS_DEFENDER * 10) + 7] * 20, 100)

				if (lua_randint(proll) >= lua_randint(mroll)) then

					msg_print(string.format('You interrupt the attacks of %s!', m_name))
					i = totalshots
				end
			end
		end
	else

		return 0
	end
	
	update_and_handle()
	
	-- Always notice cause of death
	-- It's a short signed integer, so the max is 32767.
	if (death and (m_race(monster(m_idx).r_idx).r_deaths < 32767)) then

		m_race(monster(m_idx).r_idx).r_deaths = m_race(monster(m_idx).r_idx).r_deaths + 1
	end

	-- Success.
	return 1
end

-- This function is used to determine which spell a monster will cast.
-- The actual effect of the spell is determined by the "monster_spell_effects" function.
function monster_spell_attack (m_idx)

	local m_name = ""
	local chance
	local i
	local j
	local chosen = 0
	local num_spells = 0

	-- Target location
	local x = px
	local y = py
	 
	-- Do we see the monster?
	local seen = (monster(m_idx).ml)

        -- Locked monsters cannot cast spells.
	if (get_monster_ability(monster(m_idx), CURSE_LOCK)) then return 0 end

	-- Cannot cast spells when confused
	if (monster(m_idx).confused > 0) then return 0 end

	-- Friendly monsters won't cast spells at us.
	if (is_pet(monster(m_idx))) then return 0 end

	-- Chance to cast a spell.
        chance = m_race(monster(m_idx).r_idx).spellchance

	-- Taunt may reduce the chance to cast spells.
	if (get_monster_ability(monster(m_idx), TAUNTED)) then

		chance = chance - (25 + (p_ptr.abilities[(CLASS_FIGHTER * 10) + 3] - 1))
		if (chance < 0) then chance = 0 end
	end
	
	-- If chance is 0, the monster can't cast spells.
	if (chance == 0) then return 0 end

	-- Roll to see if the monster will cast a spell.
	if (lua_randint(100) > chance) then return 0 end

	-- Verify if the player can be shot.
	-- Check range. MAX_RANGE is hardcoded to 18.
	if (monster(m_idx).cdis > MAX_RANGE) then return 0 end

	--Check if player can be reached.
	if (not(projectable(monster(m_idx).fy, monster(m_idx).fx, py, px))) then return 0 end

	-- Check if a monster is in the way.
	if (not(clean_shot(monster(m_idx).fy, monster(m_idx).fx, py, px))) then return 0 end

	-- Player stealth/invisibility.
	if (player_invis(monster(m_idx))) then return 0 end

	-- Stop if player is dead or gone
	-- (Not exactly sure why the code had a check for both "not alive" and death...)
	if (not(alive) or death) then return 0 end
 
	-- Stop if player is leaving
	if (p_ptr.leaving) then return 0 end

	-- Get monster's name.
	m_name = m_race(monster(m_idx).r_idx).name_char

	-- Number of spells.
	num_spells = m_race(monster(m_idx).r_idx).spells
	if (m_race(monster(m_idx).r_idx).spellsscalefactor > 0) then
		num_spells = num_spells + (m_race(monster(m_idx).r_idx).spellsscale * (monster(m_idx).level / m_race(monster(m_idx).r_idx).spellsscalefactor))
	end

	-- Choose spell(s) to cast!
	for j = 1, num_spells do

		local numspells

		-- Return if we're leaving.
		if (p_ptr.leaving) then return 0 end

		-- Count how many spells the monster has.
		i = 1
		numspells = 0
		while (i <= 20 and m_race(monster(m_idx).r_idx).spell[i].type > 0) do

			numspells = numspells + 1
			i = i + 1
		end

		-- No spells chosen yet.
		chosen = 0

		-- The first thing to try is a healing spell, if the monster's hp
		-- is lower or equal to 33%.
		if (monster(m_idx).hp <= (monster(m_idx).maxhp / 3)) then

			for i = 1, numspells do

				if (m_race(monster(m_idx).r_idx).spell[i].type == 3 and monster(m_idx).mana >= m_race(monster(m_idx).r_idx).spell[i].cost) then

					chosen = i
				end
			end
		end

		-- Secondly, try a haste spell.
		if (monster(m_idx).hasted == 0 and chosen == 0) then

			for i = 1, numspells do

				if (m_race(monster(m_idx).r_idx).spell[i].type == 4 and monster(m_idx).mana >= m_race(monster(m_idx).r_idx).spell[i].cost) then

					chosen = i
				end
			end
		end

		-- If not healing or hasting, try a buff spell.
		if (monster(m_idx).boosted == 0 and chosen == 0) then

			for i = 1, numspells do

				if (m_race(monster(m_idx).r_idx).spell[i].type == 5 and monster(m_idx).mana >= m_race(monster(m_idx).r_idx).spell[i].cost) then

					chosen = i
				end
			end
		end

		-- If not healing, hasting or buffing, randomly choose another spell(if any).
		if (chosen == 0) then

			local cancast = FALSE

			-- Let's make sure we have other types of spells available.
			for i = 1, numspells do

				if (not(m_race(monster(m_idx).r_idx).spell[i].type == 3) and not(m_race(monster(m_idx).r_idx).spell[i].type == 4) and not(m_race(monster(m_idx).r_idx).spell[i].type == 5)) then

					if (monster(m_idx).mana >= m_race(monster(m_idx).r_idx).spell[i].cost) then cancast = TRUE end
				end
			end

			-- Found at least one spell, then choose one spell randomly.
			if (cancast) then

				while (chosen == 0) do

					local tmpspell

					tmpspell = lua_randint(numspells)

					if (not(m_race(monster(m_idx).r_idx).spell[tmpspell].type == 3) and not(m_race(monster(m_idx).r_idx).spell[tmpspell].type == 4) and not(m_race(monster(m_idx).r_idx).spell[tmpspell].type == 5)) then

						if (monster(m_idx).mana >= m_race(monster(m_idx).r_idx).spell[tmpspell].cost) then chosen = tmpspell end
					end
				end
			else

				-- If we don't have other spells, return.
				return 0
			end
		end


		-- We saw the monster cast the spell. Remember it.

		--if (seen) then

		--	m_race(monster(m_idx).r_idx).r_spells[chosen] = 1
		--end

		-- Process the spell's effect.
		monster_spell_effects(m_idx, chosen)

		update_and_handle()

		-- Defender's Interrupt ability.
		if (p_ptr.abilities[(CLASS_DEFENDER * 10) + 7] > 0 and player_next_to_monster(monster(m_idx))) then

			local proll
			local mroll
			local skillpercent

			skillpercent = p_ptr.abilities[(CLASS_DEFENDER * 10) + 7] * 10
			if (skillpercent > 100) then skillpercent = 100 end

			proll = multiply_divide(p_ptr.skill[28], skillpercent, 100) * 2
			mroll = monster(m_idx).skill_magic

			proll = proll + multiply_divide(proll, p_ptr.abilities[(CLASS_DEFENDER * 10) + 7] * 20, 100)

			if (lua_randint(proll) >= lua_randint(mroll)) then

				msg_print(string.format('You interrupt the spells of %s!', m_name))
				j = num_spells
			end
		end
	end

	-- Always notice cause of death
	-- It's a short signed integer, so the max is 32767.
	if (death and (m_race(monster(m_idx).r_idx).r_deaths < 32767)) then

		m_race(monster(m_idx).r_idx).r_deaths = m_race(monster(m_idx).r_idx).r_deaths + 1
	end

	-- Success.
	return 1
end

-- Function for the effects of spells.
function monster_spell_effects(m_idx, chosen)

	local damage
	local m_name

	-- Get monster's name.
	m_name = m_race(monster(m_idx).r_idx).name_char

	-- Call the "monster_before_magic" function. (see events.lua)
	if (m_race(monster(m_idx).r_idx).event_before_magic > 0) then

		monster_before_magic(m_idx, m_race(monster(m_idx).r_idx).event_before_magic)
	end

	-- Cast the spell!
	msg_print(string.format('%s %s %s!', m_name, m_race(monster(m_idx).r_idx).spell[chosen].act, m_race(monster(m_idx).r_idx).spell[chosen].name))

	-- Counter Spell code here.
	if (not(get_monster_flag7(monster(m_idx).r_idx, RF7_CANNOT_COUNTERSPELL))) then

	end

	-- Produce an effect based on the spell's type.
	-- Type 1 and 2: Bolt and Ball spells.
	if (m_race(monster(m_idx).r_idx).spell[chosen].type == 1 or m_race(monster(m_idx).r_idx).spell[chosen].type == 2) then

		-- If the special3 attribute is equal to 1, the spell has fixed damages.
		-- Best used with Life Blast and Divination effects.
		if (m_race(monster(m_idx).r_idx).spell[chosen].special3 == 1) then damage = m_race(monster(m_idx).r_idx).spell[chosen].power
		else
			-- Determine damages. The base power can scale with monster's level.
			local spellpower
			spellpower = m_race(monster(m_idx).r_idx).spell[chosen].power
			if (m_race(monster(m_idx).r_idx).spell[chosen].scalefactor > 0) then
				spellpower = spellpower + ((monster(m_idx).level / m_race(monster(m_idx).r_idx).spell[chosen].scalefactor) * m_race(monster(m_idx).r_idx).spell[chosen].scale)
			end
			damage = monster_spell_damages(monster(m_idx), spellpower)
			if (get_monster_ability(monster(m_idx), BOSS_DOUBLE_MAGIC)) then damage = damage * 2 end

			-- Type 1 will produce a bolt, type 2 a ball.
			if (m_race(monster(m_idx).r_idx).spell[chosen].type == 1) then

				lua_project(m_idx, 0, py, px, damage, m_race(monster(m_idx).r_idx).spell[chosen].special1, 1)
			else

				lua_project(m_idx, m_race(monster(m_idx).r_idx).spell[chosen].special2, py, px, damage, m_race(monster(m_idx).r_idx).spell[chosen].special1, 2)
			end
		end
	end

	-- Type 3: Healing spell.
	if (m_race(monster(m_idx).r_idx).spell[chosen].type == 3) then

		-- "Damage" is used for amount healed.
		local spellpower
		spellpower = m_race(monster(m_idx).r_idx).spell[chosen].power
		if (m_race(monster(m_idx).r_idx).spell[chosen].scalefactor > 0) then
			spellpower = spellpower + ((monster(m_idx).level / m_race(monster(m_idx).r_idx).spell[chosen].scalefactor) * m_race(monster(m_idx).r_idx).spell[chosen].scale)
		end
		damage = monster_spell_damages(monster(m_idx), spellpower)
		if (get_monster_ability(monster(m_idx), BOSS_DOUBLE_MAGIC)) then damage = damage * 2 end

		monster(m_idx).hp = monster(m_idx).hp + damage
		if (monster(m_idx).hp > monster(m_idx).maxhp) then monster(m_idx).hp = monster(m_idx).maxhp end

		msg_print(string.format('%s looks healthier.', m_name))
	end

	-- Type 4: Haste.
	if (m_race(monster(m_idx).r_idx).spell[chosen].type == 4) then

		monster(m_idx).mspeed = monster(m_idx).mspeed + m_race(monster(m_idx).r_idx).spell[chosen].power
		if (monster(m_idx).mspeed > 180) then monster(m_idx).mspeed = 180 end
		monster(m_idx).hasted = 1
		msg_print(string.format('%s starts moving faster!', m_name))
	end

	-- Type 5: Buff.
	if (m_race(monster(m_idx).r_idx).spell[chosen].type == 5) then

		local power
		power = m_race(monster(m_idx).r_idx).spell[chosen].power
		if (m_race(monster(m_idx).r_idx).spell[chosen].scalefactor > 0) then
			power = power + ((monster(m_idx).level / m_race(monster(m_idx).r_idx).spell[chosen].scalefactor) * m_race(monster(m_idx).r_idx).spell[chosen].scale)
		end

		if (m_race(monster(m_idx).r_idx).spell[chosen].special1 == 1) then monster(m_idx).str = monster(m_idx).str + power end
		if (m_race(monster(m_idx).r_idx).spell[chosen].special1 == 2) then monster(m_idx).dex = monster(m_idx).dex + power end
		if (m_race(monster(m_idx).r_idx).spell[chosen].special1 == 3) then monster(m_idx).mind = monster(m_idx).mind + power end
		if (m_race(monster(m_idx).r_idx).spell[chosen].special1 == 4) then monster(m_idx).skill_attack = monster(m_idx).skill_attack + power end
		if (m_race(monster(m_idx).r_idx).spell[chosen].special1 == 5) then monster(m_idx).skill_ranged = monster(m_idx).skill_ranged + power end
		if (m_race(monster(m_idx).r_idx).spell[chosen].special1 == 6) then monster(m_idx).skill_magic = monster(m_idx).skill_magic + power end
		if (m_race(monster(m_idx).r_idx).spell[chosen].special1 == 7) then monster(m_idx).skill_evasion = monster(m_idx).skill_evasion + power end
		if (m_race(monster(m_idx).r_idx).spell[chosen].special1 == 8) then monster(m_idx).skill_mdef = monster(m_idx).skill_mdef + power end
		if (m_race(monster(m_idx).r_idx).spell[chosen].special1 == 9) then

			monster(m_idx).str = monster(m_idx).str + power
			monster(m_idx).dex = monster(m_idx).dex + power
			monster(m_idx).mind = monster(m_idx).mind + power
		end
		if (m_race(monster(m_idx).r_idx).spell[chosen].special1 == 10) then

			monster(m_idx).skill_attack = monster(m_idx).skill_attack + power
			monster(m_idx).skill_ranged = monster(m_idx).skill_ranged + power
			monster(m_idx).skill_magic = monster(m_idx).skill_magic + power
			monster(m_idx).skill_evasion = monster(m_idx).skill_evasion + power
			monster(m_idx).skill_mdef = monster(m_idx).skill_mdef + power
		end
		if (m_race(monster(m_idx).r_idx).spell[chosen].special1 == 11) then

			monster(m_idx).str = monster(m_idx).str + power
			monster(m_idx).dex = monster(m_idx).dex + power
			monster(m_idx).mind = monster(m_idx).mind + power
			monster(m_idx).skill_attack = monster(m_idx).skill_attack + power
			monster(m_idx).skill_ranged = monster(m_idx).skill_ranged + power
			monster(m_idx).skill_magic = monster(m_idx).skill_magic + power
			monster(m_idx).skill_evasion = monster(m_idx).skill_evasion + power
			monster(m_idx).skill_mdef = monster(m_idx).skill_mdef + power
		end

		monster(m_idx).boosted = 1
	end

	-- Type 6: Summon Kind.
	if (m_race(monster(m_idx).r_idx).spell[chosen].type == 6) then

		local i

		-- Mighty Defense, Summons.
		if (p_ptr.events[29050] == 4 and player_next_to_monster(monster(m_idx))) then

			local ppower
			local mpower

			ppower = p_ptr.stat_ind[A_CON+1] + p_ptr.skill[5] + p_ptr.skill[28]
			if (inven(INVEN_BODY).tval == TV_HARD_ARMOR or inven(INVEN_BODY).tval == TV_DRAG_ARMOR) then

				ppower = ppower + inven(INVEN_BODY).ac
			end
			if (shield_has()) then

				local sbonus = 0
				if (inven(INVEN_WIELD).tval == TV_SHIELD) then

					sbonus = sbonus + inven(INVEN_WIELD).ac
				end
				if (inven(INVEN_WIELD+1).tval == TV_SHIELD) then

					sbonus = sbonus + inven(INVEN_WIELD+1).ac
				end

				ppower = ppower + multiply_divide(ppower, sbonus, 100)
			end

			ppower = ppower + multiply_divide(ppower, p_ptr.abilities[(CLASS_DEFENDER * 10) + 8] * 20, 100)

			mpower = monster(who).mind + monster(who).skill_magic

			msg_print("You prevent the spell from working.")

			-- Has a chance to break the defense.
			if (lua_randint(mpower) >= lua_randint(ppower)) then

				msg_print("Your Mighty Defense has been broken!!")
				p_ptr.events[29050] = 0
			end
		else

			for i = 0, m_race(monster(m_idx).r_idx).spell[chosen].special1 do

				local power
				local dur
				power = m_race(monster(m_idx).r_idx).spell[chosen].power
				if (m_race(monster(m_idx).r_idx).spell[chosen].scalefactor > 0) then
					power = power + ((monster(m_idx).level / m_race(monster(m_idx).r_idx).spell[chosen].scalefactor) * m_race(monster(m_idx).r_idx).spell[chosen].scale)
				end

				dur = m_race(monster(m_idx).r_idx).spell[chosen].special2

				if (is_pet(monster(m_idx))) then

					summon_specific_kind(monster(m_idx).fy, monster(m_idx).fx, power, m_race(monster(m_idx).r_idx).spell[chosen].summchar, FALSE, TRUE, dur)
				else

					summon_specific_kind(monster(m_idx).fy, monster(m_idx).fx, power, m_race(monster(m_idx).r_idx).spell[chosen].summchar, FALSE, FALSE, dur)
				end
			end
		end
	end

	-- Type 7: Summon Specific.
	if (m_race(monster(m_idx).r_idx).spell[chosen].type == 7) then

		local i

		-- Mighty Defense, Summons.
		if (p_ptr.events[29050] == 4 and player_next_to_monster(monster(m_idx))) then

			local ppower
			local mpower

			ppower = p_ptr.stat_ind[A_CON+1] + p_ptr.skill[5] + p_ptr.skill[28]
			if (inven(INVEN_BODY).tval == TV_HARD_ARMOR or inven(INVEN_BODY).tval == TV_DRAG_ARMOR) then

				ppower = ppower + inven(INVEN_BODY).ac
			end
			if (shield_has()) then

				local sbonus = 0
				if (inven(INVEN_WIELD).tval == TV_SHIELD) then

					sbonus = sbonus + inven(INVEN_WIELD).ac
				end
				if (inven(INVEN_WIELD+1).tval == TV_SHIELD) then

					sbonus = sbonus + inven(INVEN_WIELD+1).ac
				end

				ppower = ppower + multiply_divide(ppower, sbonus, 100)
			end

			ppower = ppower + multiply_divide(ppower, p_ptr.abilities[(CLASS_DEFENDER * 10) + 8] * 20, 100)

			mpower = monster(who).mind + monster(who).skill_magic

			msg_print("You prevent the spell from working.")

			-- Has a chance to break the defense.
			if (lua_randint(mpower) >= lua_randint(ppower)) then

				msg_print("Your Mighty Defense has been broken!!")
				p_ptr.events[29050] = 0
			end
		else

			for i = 0, m_race(monster(m_idx).r_idx).spell[chosen].special1 do

				local dur

				dur = m_race(monster(m_idx).r_idx).spell[chosen].special2

				if (is_pet(monster(m_idx))) then

					summon_specific_ridx(monster(m_idx).fy, monster(m_idx).fx, m_race(monster(m_idx).r_idx).spell[chosen].power, FALSE, TRUE, dur)
				else

					summon_specific_ridx(monster(m_idx).fy, monster(m_idx).fx, m_race(monster(m_idx).r_idx).spell[chosen].power, FALSE, FALSE, dur)
				end
			end
		end
	end

	-- Type 8: Teleport.
	if (m_race(monster(m_idx).r_idx).spell[chosen].type == 8) then

		teleport_away(m_idx, m_race(monster(m_idx).r_idx).spell[chosen].power)
	end

	-- Type 999: Scripted.
	if (m_race(monster(m_idx).r_idx).spell[chosen].type == 999) then

		lua_monster_script(m_idx, m_race(monster(m_idx).r_idx).spell[chosen].name)
	end

	-- Call the "monster_after_magic" function. (see events.lua)
	if (m_race(monster(m_idx).r_idx).event_after_magic > 0) then

		monster_after_magic(m_idx, m_race(monster(m_idx).r_idx).event_after_magic)
	end

	-- Reduce mana.
	monster(m_idx).mana = monster(m_idx).mana - m_race(monster(m_idx).r_idx).spell[chosen].cost

	-- Counter Shot Marksman ability.
	if (p_ptr.chp > 0 and p_ptr.abilities[(CLASS_MARKSMAN * 10) + 4] > 0 and p_ptr.events[29046] == 1 and not(monster_counter_attack)) then

		p_ptr.events[29047] = m_idx
		use_ability(120)
		p_ptr.events[29047] = 0
	end
end

-- Used for both melee and ranged attacks of monsters.
-- stat can be 0 for strength, or 1 for dex.
-- Spell damages are treated in a different function. See "monster_spell_damages".
function monster_damages(mon, dd, ds, stat)

	local k = 0

	k = damroll(dd, ds)

	if (stat == 0) then

		if (mon.str > 5) then

			k = k * (mon.str - 5)
		end
	else
		if (mon.dex > 5) then

			k = k * (mon.dex - 5)
		end
	end

	return k
end

-- Spell damages for monsters.
-- "mon" is the monster_type object that needs to be referenced.
function monster_spell_damages(mon, power)

        local k = 0

	-- Base power.
	-- k = lua_randint((power / 2)) + (power / 2)
	k = power

	-- Damages are multiplied by every points of Mind above 5.
	if (mon.mind > 5) then

		k = k + (k * (mon.mind-5))
	end
	
        return k
end

add_event_handler("monster_stats", monster_stats)
add_event_handler("monster_melee_attack", monster_melee_attack)
add_event_handler("monster_ranged_attack", monster_ranged_attack)
add_event_handler("monster_spell_attack", monster_spell_attack)
add_event_handler("monster_spell_effects", monster_spell_effects)
add_event_handler("monster_damages", monster_damages)
add_event_handler("monster_spell_damages", monster_spell_damages)
