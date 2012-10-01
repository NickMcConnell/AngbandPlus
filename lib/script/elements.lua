-- File: elements.lua
-- This file contains the effects of the various elements. It has two
-- roles. The effect of elements on the player, and the effect of
-- elements on monsters.

-- Here's a list of elemental constants for the various elements.
-- The effects can be customized as you want, however, since some hard-coded parts
-- might use these constants, it's better not to use these numbers for custom
-- elements yet.
-- GF_FIRE         1
-- GF_COLD         2
-- GF_ELEC         3
-- GF_ACID         4
-- GF_POIS         5
-- GF_LITE         6
-- GF_DARK         7
-- GF_WARP         8
-- GF_WATER        9
-- GF_WIND         10
-- GF_EARTH        11
-- GF_SOUND        12
-- GF_RADIO        13
-- GF_CHAOS        14
-- GF_PHYSICAL     15
-- GF_MANA         16
-- GF_MISSILE      17
-- GF_FROSTFIRE    18
-- GF_GREY         19
-- GF_TOXIC        20
-- GF_MUD          21
-- GF_CONFUSION    22 (this is a damaging confusion. It's a chaos/mana dual type that confuses.)
-- GF_ICE          23
-- GF_ELEMENTAL    24
-- GF_STONE_TO_MUD 48
-- GF_RESTORE_MANA 52 (defined in init.lua)
-- GF_OLD_HEAL     53
-- GF_OLD_SPEED    54
-- GF_OLD_SLOW     55
-- GF_OLD_CONF     56
-- GF_OLD_SLEEP    57
-- GF_RESTORE_STATS  58 (defined in init.lua)
-- GF_RESTORE_STATUS  59 (defined in init.lua)
-- GF_RESTORE_LEVELS  60 (defined in init.lua)
-- GF_RESTORATION  61 (defined in init.lua)
-- GF_CURE_MUTATIONS  62 (defined in init.lua)
-- GF_TURN_UNDEAD  64
-- GF_STRENGTH     65 (defined in init.lua)
-- GF_HEROISM      66 (defined in init.lua)
-- GF_STAR_HEROISM 67 (defined in init.lua)
-- GF_STUN         78
-- GF_FEAR         102
-- GF_WEAKEN       108
-- GF_SLOW_DOWN    111
-- GF_LIFE_BLAST   112
-- GF_HALVE_DAMAGES113
-- GF_HALVE_MAGIC  114
-- GF_LOCK         117
-- GF_DAMAGES_CURSE118
-- GF_RETROGRADE   120
-- GF_SLEEP_POLLEN 123
-- GF_WAR_BLESSING 124
-- GF_FEAR_CURSE   127
-- GF_REDUCE_DEF   130
-- GF_REDUCE_HIT   131
-- GF_REDUCE_SPEED 132
-- GF_MORALE_BOOST 133
-- GF_EVOLVE       134
-- GF_UNEVOLVE     135
-- GF_UNSUMMON     136
-- GF_SLEEP_GAS    137
-- GF_ANIMAL_EMPATHY 138
-- GF_SMITE_EVIL   140
-- GF_RETROGRADE_DARKNESS   141
-- GF_DOMINATE_MONSTER 142
-- GF_SHATTER_EVIL 143
-- GF_ANGELIC_VOICE144
-- GF_REPULSE_EVIL 145
-- GF_SLAY_EVIL    146
-- GF_SEAL_LIGHT   147
-- GF_PARALYZE     148
-- GF_STEALTH_ATTACK 149
-- GF_WARCRY       150
-- GF_LOSE_STR     151
-- GF_LOSE_INT     152
-- GF_LOSE_WIS     153
-- GF_LOSE_DEX     154
-- GF_LOSE_CON     155
-- GF_LOSE_CHR     156
-- GF_LOSE_ALL     157
-- GF_LOSE_EXP     158
-- GF_TAUNT        159
-- GF_VULNERABILITY 160 (defined in init.lua)
-- GF_DIVINATION   161
-- GF_HARM         162
-- GF_UNDEAD_SMITE 163
-- GF_DEMON_SMITE  164
-- GF_EVIL_SMITE   165
-- GF_GOOD_SMITE   166
-- GF_NIGHTMARES   167 (defined in init.lua)
-- GF_COMMAND_ELEMENT 168
-- GF_RACIAL_CHAMPION 169
-- GF_SOUL_CRUSH   170
-- GF_DURATION     171 (defined in init.lua)
-- GF_INSPIRE_COURAGE 172 (defined in init.lua)

-- Monster is hit by element!
-- If "who" is 0, the source is the player. Otherwise, it's another monster.
function element_hit_monster (who, m_idx, element, dam)

	local m_name
	local ppower
	local mpower
	local originalpower

	-- Some attacks do not affect allies.
	if (is_pet(monster(m_idx)) and no_effect_allies == 1) then return 0 end

	-- Some attacks do not affect hostile creatures.
	if (not(is_pet(monster(m_idx))) and no_effect_hostile == 1) then return 0 end

	-- Sometimes, we don't want to hurt monsters.
	if (donthurtmonsters == 1) then
		dam = 0
	end

	-- Store the original power of the spell.
	originalpower = dam
	
	m_name = m_race(monster(m_idx).r_idx).name_char

	-- Just in case... element 0 defaults to Physical.
	if (element == 0) then element = GF_PHYSICAL end

	-- We may be able to change the attack's element with War Songs.
	if (p_ptr.events[29042] == 1 and p_ptr.events[29043] == 1 and p_ptr.abilities[(CLASS_BARD * 10) + 8] > 0 and ((melee_attack) or (ranged_attack))) then

		local cursong

		cursong = p_ptr.events[29041]
		if (is_elemental(music_song[cursong+1].element) or is_mysticism(music_song[cursong+1].element)) then

			if (music_song[cursong+1].power >= m_race(monster(m_idx).r_idx).level) then

				element = music_song[cursong+1].element
			end
		end
	end

	-- Determine if an Immortal enemy can be killed.
	if (m_race(monster(m_idx).r_idx).resistances[element+1] < 0) then

		enemy_immortality = FALSE
	else

		enemy_immortality = TRUE
	end

	-- Hexblaze Strike may be "nevermiss".
	if (hexblaze == 1 and p_ptr.cursed >= 3) then

		local proll
		local mroll

		proll = (p_ptr.cursed / 3) * p_ptr.abilities[(CLASS_NIGHT1 * 10) + 3]
		mroll = monster(m_idx).level + ((monster(m_idx).str + monster(m_idx).dex + monster(m_idx).mind) / 3)

		if (lua_randint(proll) >= lua_randint(mroll)) then nevermiss = TRUE end
	end

	-- The basic elements turns to chaos for a zulgor.
	if (p_ptr.prace == RACE_ZULGOR) then

		if (element == GF_FIRE or element == GF_COLD or element == GF_ELEC or element == GF_ACID
		or element == GF_POIS or element == GF_LITE or element == GF_DARK or element == GF_WARP or
		element == GF_WATER or element == GF_WIND or element == GF_EARTH or element == GF_RADIO or
		element == GF_SOUND or element == GF_MANA or element == GF_MISSILE or element == GF_CONFUSION or
		element == GF_FROSTFIRE or element == GF_GREY or element == GF_TOXIC or element == GF_MUD or element == GF_ICE or element == GF_ELEMENTAL) then element = GF_CHAOS end
	end

	-- If we're using Life Blast, it does percentile damages to the monster's hp.
	if (element == GF_LIFE_BLAST) then

		dam = multiply_divide(monster(m_idx).maxhp, dam, 100)
		if (dam == monster(m_idx).maxhp) then dam = dam + 1 end
	end

	-- If we're using Divination, it does percentile damages to the monster's hp.
	if (element == GF_DIVINATION) then

		local msum
		local difference
		local divpower

		msum = m_race(monster(m_idx).r_idx).level + monster(m_idx).level

		-- Store the original power.
		divpower = dam

		-- Damages starts at 100%.
		dam = monster(m_idx).maxhp

		-- No difference yet.
		difference = 0

		-- Increase difference by 5 for every depths+level points different.
		if (msum > divpower) then difference = difference + ((msum - divpower) * 5)
		elseif (msum < divpower) then difference = difference + ((divpower - msum) * 5)
		end

		-- Reduce damages by (difference)%.
		dam = dam - multiply_divide(dam, difference, 100)

		if (dam < 0) then dam = 0 end

		if (dam == monster(m_idx).maxhp) then dam = dam + 1 end
	end

	-- Magic Missile attacks may receive a bonus.
	if (p_ptr.abilities[(CLASS_MAGE * 10) + 3] > 0) then

		dam = dam + multiply_divide(dam, p_ptr.abilities[(CLASS_MAGE * 10) + 3] * 10, 100)
	end

	-- Elemental Lord's Mastery Of Elements! 
        if ((p_ptr.abilities[(CLASS_ELEM_LORD * 10) + 10] >= 1) and element == p_ptr.elemlord) then

		dam = dam + multiply_divide(dam, (p_ptr.abilities[(CLASS_ELEM_LORD * 10) + 10] * 10), 100)
        end

	-- Monster Inner Elemental Mastery
	if ((p_ptr.abilities[(CLASS_MONSTER * 10) + 4] >= 1) and element == p_ptr.elemlord and p_ptr.resistances[element+1] >= 4) then

		dam = dam + multiply_divide(dam, p_ptr.abilities[(CLASS_MONSTER * 10) + 4] * (p_ptr.resistances[element+1] / 10), 100)
	end

	-- Monsters: Elite ability that doubles melee and ranged.
	if (p_ptr.prace == RACE_MONSTER and dam > 0 and get_player_monster_ability(BOSS_DOUBLE_DAMAGES) and ((melee_attack) or (ranged_attack))) then

		dam = dam * 2
	end

	-- Some races gets elemental bonus.
	if (p_ptr.prace == RACE_KOBOLD and element == GF_POIS) then dam = dam + (dam / 2) end
	if (p_ptr.prace == RACE_CELESTIAL and element == GF_LITE) then dam = dam + (dam / 4) end
	if (p_ptr.prace == RACE_DEMON and element == GF_DARK) then dam = dam + (dam / 4) end
	if (p_ptr.prace == RACE_ZULGOR and element == GF_CHAOS) then dam = dam + (dam / 4) end

	-- This is the code for a magic attack.
	if ((dam > 0) and not(ranged_attack) and not(throw_attack) and not(melee_attack)) then

		-- Apply monster's resistances
		if (not((get_monster_ability(monster(m_idx), PIERCING_SPELLS)) and (element == p_ptr.elemlord))) then
			dam = dam - multiply_divide(dam, m_race(monster(m_idx).r_idx).resistances[element+1], 100)
		end

		-- Some monsters may have counters, immunities, returning, etc...

        	-- Missile is Half-Magic, Half-Physical
        	if (element == GF_MISSILE) then

			local magichalf
			local physicalhalf

                	magichalf = dam / 2
                	physicalhalf = dam / 2

			if (get_monster_ability(monster(m_idx), BOSS_IMMUNE_MAGIC)) then magichalf = 0 end
			if (get_monster_ability(monster(m_idx), BOSS_IMMUNE_WEAPONS)) then physicalhalf = 0 end

                	dam = magichalf + physicalhalf
        	end

		-- So is Ice.
        	if (element == GF_ICE) then

			local magichalf
			local physicalhalf

                	magichalf = dam / 2
                	physicalhalf = dam / 2

			if (get_monster_ability(monster(m_idx), BOSS_IMMUNE_MAGIC)) then magichalf = 0 end
			if (get_monster_ability(monster(m_idx), BOSS_IMMUNE_WEAPONS)) then physicalhalf = 0 end

                	dam = magichalf + physicalhalf
        	end

        	-- Arg!! Magic Returning bosses?? :|
		-- The danger of putting this code in a lua script is that someone might comment it... :(
        	if ((element ~= GF_TURN_UNDEAD) and (element ~= GF_AURA_LIFE) and not(no_magic_return) and not(lord_piercing(5, 2, element, monster(m_idx), 1)) and (get_monster_ability(monster(m_idx), BOSS_MAGIC_RETURNING))) then

                	msg_print("You take damages from your spell!")
			if (element == GF_LIFE_BLAST or element == GF_DIVINATION) then lua_project(m_idx, 0, py, px, originalpower / 2, element, 2)
			else lua_project(m_idx, 0, py, px, (dam / 2), element, 2)
			end
        	end
		-- Returning counters!
		-- Commenting this counts as cheating!!
		if ((element ~= GF_TURN_UNDEAD) and (element ~= GF_AURA_LIFE) and not(no_magic_return) and ((m_race(monster(m_idx).r_idx).countertype == 8 or m_race(monster(m_idx).r_idx).countertype == 9) and lua_randint(100) <= m_race(monster(m_idx).r_idx).counterchance)) then

			msg_print("You suffer from your own spell!")
			if (element == GF_LIFE_BLAST or element == GF_DIVINATION) then lua_project(m_idx, 0, py, px, originalpower, element, 2)
			else lua_project(m_idx, 0, py, px, dam, element, 2)
			end
		end
		-- Counters that block magic!
		-- Commenting this code will bring you bad luck.
        	if ((element ~= GF_TURN_UNDEAD) and (element ~= GF_AURA_LIFE) and not(no_magic_return) and ((m_race(monster(m_idx).r_idx).countertype == 2 or m_race(monster(m_idx).r_idx).countertype == 3 or m_race(monster(m_idx).r_idx).countertype == 18 or m_race(monster(m_idx).r_idx).countertype == 19) and lua_randint(100) <= m_race(monster(m_idx).r_idx).counterchance) and not(nevermiss)) then

			local proll
			local mroll

			mroll = monster(m_idx).mind
			if (music == 1) then proll = (p_ptr.stat_ind[A_CHR+1])
			else proll = (p_ptr.stat_ind[A_INT+1] + p_ptr.stat_ind[A_WIS+1]) end

			-- Shadow Stalker's Storm of Shadow Edges ability is harder to counter.
			if (stormshadow) then proll = proll + p_ptr.skill[7] end

			if (lua_randint(mroll) >= lua_randint(proll)) then

				msg_print(string.format('%s blocked your magic!', m_name))
                		dam = 0
			end
        	end
		-- The Boogie Man will come to you in your sleep if you comment this!
		if ((element ~= GF_TURN_UNDEAD) and (element ~= GF_AURA_LIFE) and not(no_magic_return) and ((m_race(monster(m_idx).r_idx).countertype == 5 or m_race(monster(m_idx).r_idx).countertype == 6 or m_race(monster(m_idx).r_idx).countertype == 22 or m_race(monster(m_idx).r_idx).countertype == 23) and lua_randint(100) <= m_race(monster(m_idx).r_idx).counterchance) and not(nevermiss)) then

			-- 100% block!
			msg_print(string.format('%s blocked your magic!', m_name))
                	dam = 0
        	end
		-- Counters that block AND return magic!
		-- Big Foot will eat you if you comment this code!
        	if ((element ~= GF_TURN_UNDEAD) and (element ~= GF_AURA_LIFE) and not(no_magic_return) and ((m_race(monster(m_idx).r_idx).countertype == 11 or m_race(monster(m_idx).r_idx).countertype == 12) and lua_randint(100) <= m_race(monster(m_idx).r_idx).counterchance) and not(nevermiss)) then

			local proll
			local mroll

			mroll = monster(m_idx).mind
			if (music == 1) then proll = (p_ptr.stat_ind[A_CHR+1])
			else proll = (p_ptr.stat_ind[A_INT+1] + p_ptr.stat_ind[A_WIS+1]) end

			-- Shadow Stalker's Storm of Shadow Edges ability is harder to counter.
			if (stormshadow) then proll = proll + p_ptr.skill[7] end

			if (lua_randint(mroll) >= lua_randint(proll)) then

				msg_print(string.format('%s blocked your magic!', m_name))
				msg_print("You suffer from your own spell!")
				if (element == GF_LIFE_BLAST or element == GF_DIVINATION) then lua_project(m_idx, 0, py, px, originalpower, element, 2)
                		else lua_project(m_idx, 0, py, px, dam, element, 2)
				end
                		dam = 0
			end
        	end
		if ((element ~= GF_TURN_UNDEAD) and (element ~= GF_AURA_LIFE) and not(no_magic_return) and ((m_race(monster(m_idx).r_idx).countertype == 14 or m_race(monster(m_idx).r_idx).countertype == 15) and lua_randint(100) <= m_race(monster(m_idx).r_idx).counterchance) and not(nevermiss)) then

			-- 100% block!
                	msg_print(string.format('%s blocked your magic!', m_name))
			msg_print("You suffer from your own spell!")
			if (element == GF_LIFE_BLAST or element == GF_DIVINATION) then lua_project(m_idx, 0, py, px, originalpower, element, 2)
                	else lua_project(m_idx, 0, py, px, dam, element, 2)
			end
                	dam = 0
        	end

        	-- Some elites/bosses are resistant or immune to magic...
        	if (get_monster_ability(monster(m_idx), BOSS_HALVE_DAMAGES) and not(lord_piercing(10, 3, element, monster(m_idx), 1))) then

                	dam = dam / 2
        	end

        	-- GF_PHYSICAL is actually physical damages...
        	if ((get_monster_ability(monster(m_idx), BOSS_IMMUNE_MAGIC)) and element ~= GF_PHYSICAL and element ~= GF_MISSILE and element ~= GF_ICE and element ~= GF_SMITE_EVIL and element ~= GF_STEALTH_ATTACK and not(lord_piercing(1, 1, element, monster(m_idx), 1)) and dam > 0) then

                	msg_print(string.format('%s is immune!', m_name))
                	dam = 0
        	end

        	-- But then, physical immunes are immune to GF_PHYSICAL!
        	if ((get_monster_ability(monster(m_idx), BOSS_IMMUNE_WEAPONS)) and (element == GF_PHYSICAL or element == GF_SMITE_EVIL or element == GF_STEALTH_ATTACK) and element ~= GF_MISSILE and element ~= GF_ICE and dam > 0) then

                	msg_print(string.format('%s is immune!', m_name))
                	dam = 0
        	end
	end

	-- And here's the code for a ranged attack!
	if ((dam > 0) and (ranged_attack) and not(throw_attack) and not(melee_attack)) then

		local hit
		local hitbonus
		local hitpenality
		local blocked

		-- Initialize variables.
		hit = 0
		hitbonus = 0
		hitpenality = 0
		blocked = 0

		-- If we're using Point-Blank Shot ability, give a hit bonus.
		if (pointblankshot == 1) then hitbonus = hitbonus + multiply_divide(p_ptr.dis_to_h, (p_ptr.abilities[(CLASS_GUNNER * 10) + 4]) * 25, 100) end

		-- Crossbow users can get a bonus!
		if ((current_weapon.itemtype == 2) and p_ptr.skill_base[21] >= 30) then hitbonus = hitbonus + (p_ptr.dis_to_h / 2) end

		-- Marksman's Rapid Shot has a bonus.
		if (rapid_shot == 1) then

			hitbonus = hitbonus + multiply_divide(p_ptr.dis_to_h, p_ptr.abilities[(CLASS_MARKSMAN * 10) + 5] * 20, 100)
		end

		-- Pistols Specialization.
		if (current_weapon.itemtype == 3 and p_ptr.abilities[(CLASS_GUNNER * 10) + 7] >= 1) then
			hitbonus = hitbonus + multiply_divide(p_ptr.dis_to_h, p_ptr.abilities[(CLASS_GUNNER * 10) + 7] * 10, 100)
		end

		-- Rifles Specialization.
		if (current_weapon.itemtype == 4 and p_ptr.abilities[(CLASS_GUNNER * 10) + 8] >= 1) then
			hitbonus = hitbonus + multiply_divide(p_ptr.dis_to_h, p_ptr.abilities[(CLASS_GUNNER * 10) + 8] * 10, 100)
		end

		-- May drop ammos on the ground.
		if (dropshots) then
			if (dropnum > 0) then drop_near_ammo(drop_ranged, dropnum, monster(m_idx).fy, monster(m_idx).fx) end
		end

		-- Set dropnum to 0 afterwards, to avoid dropping a second time.
		dropnum = 0

		-- With Accurate Shots, we might always hit!
		if (p_ptr.abilities[(CLASS_MARKSMAN * 10) + 10] > 0) then

			local proll
			local mroll

			proll = p_ptr.abilities[(CLASS_MARKSMAN * 10) + 10] * 20
			mroll = monster(m_idx).level + monster(m_idx).dex

			if (lua_randint(proll) >= lua_randint(mroll)) then

				nevermiss = TRUE
			end
		end

                -- Try to hit the monster.
		-- We get bonus hit rate from the ammos.
		hit = player_hit_monster(monster(m_idx), drop_ranged.to_h + hitbonus - hitpenality)

		-- Did we hit the monster?
		if (hit == 1) then

			-- Apply monster's resistances
			if (not((get_monster_ability(monster(m_idx), PIERCING_SPELLS)) and (element == p_ptr.elemlord))) then

				local resamount

				resamount = m_race(monster(m_idx).r_idx).resistances[element+1]
				if (piercing_shot > 0 and resamount > 0) then

					resamount = resamount - (p_ptr.abilities[(CLASS_MARKSMAN * 10) + 9] * 5)
					if (resamount < 0) then resamount = 0 end
				end
				dam = dam - multiply_divide(dam, resamount, 100)
			end

			-- Some monsters may have counters, immunities, returning, etc...

			if (not(piercing_shot > 0 and p_ptr.abilities[(CLASS_MARKSMAN * 10) + 9] >= 20)) then

        			-- Missile is Half-Magic, Half-Physical
        			if (element == GF_MISSILE) then

					local magichalf
					local physicalhalf

                			magichalf = dam / 2
                			physicalhalf = dam / 2

					if (get_monster_ability(monster(m_idx), BOSS_IMMUNE_MAGIC)) then magichalf = 0 end
					if (get_monster_ability(monster(m_idx), BOSS_IMMUNE_WEAPONS)) then physicalhalf = 0 end

                			dam = magichalf + physicalhalf
        			end

				-- So is Ice.
        			if (element == GF_ICE) then

					local magichalf
					local physicalhalf

                			magichalf = dam / 2
                			physicalhalf = dam / 2

					if (get_monster_ability(monster(m_idx), BOSS_IMMUNE_MAGIC)) then magichalf = 0 end
					if (get_monster_ability(monster(m_idx), BOSS_IMMUNE_WEAPONS)) then physicalhalf = 0 end

                			dam = magichalf + physicalhalf
        			end
			end
        		
			-- Check for ranged counters
			if ((m_race(monster(m_idx).r_idx).countertype == 16 or m_race(monster(m_idx).r_idx).countertype == 17 or m_race(monster(m_idx).r_idx).countertype == 18 or m_race(monster(m_idx).r_idx).countertype == 19) and lua_randint(100) <= m_race(monster(m_idx).r_idx).counterchance and not(nevermiss)) then

				local proll
				local mroll

				mroll = monster(m_idx).dex
				proll = p_ptr.stat_ind[A_DEX+1]

				if (lua_randint(mroll) >= lua_randint(proll)) then

					msg_print(string.format('%s blocked your ammo!', m_name))
                			dam = 0
					blocked = 1
				end
			end
			if ((m_race(monster(m_idx).r_idx).countertype == 20 or m_race(monster(m_idx).r_idx).countertype == 21 or m_race(monster(m_idx).r_idx).countertype == 22 or m_race(monster(m_idx).r_idx).countertype == 23) and lua_randint(100) <= m_race(monster(m_idx).r_idx).counterchance and not(nevermiss)) then

				msg_print(string.format('%s blocked your ammo!', m_name))
                		dam = 0
				blocked = 1
			end

        		-- Some elites/bosses are resistant or immune...
        		if (get_monster_ability(monster(m_idx), BOSS_HALVE_DAMAGES) and not(lord_piercing(10, 3, element, monster(m_idx), 1)) and not(piercing_shot > 0 and p_ptr.abilities[(CLASS_MARKSMAN * 10) + 9] >= 10)) then

                		dam = dam / 2
        		end

        		-- GF_PHYSICAL is actually physical damages...
        		if ((get_monster_ability(monster(m_idx), BOSS_IMMUNE_MAGIC)) and element ~= GF_PHYSICAL and element ~= GF_MISSILE and element ~= GF_ICE and element ~= GF_SMITE_EVIL and element ~= GF_STEALTH_ATTACK and not(lord_piercing(1, 1, element, monster(m_idx), 1)) and not(piercing_shot > 0 and p_ptr.abilities[(CLASS_MARKSMAN * 10) + 9] >= 20) and dam > 0) then

                		msg_print(string.format('%s is immune!', m_name))
                		dam = 0
        		end

        		-- But then, physical immunes are immune to GF_PHYSICAL!
        		if ((get_monster_ability(monster(m_idx), BOSS_IMMUNE_WEAPONS)) and (element == GF_PHYSICAL or element == GF_SMITE_EVIL or element == GF_STEALTH_ATTACK) and element ~= GF_MISSILE and element ~= GF_ICE and not(piercing_shot > 0 and p_ptr.abilities[(CLASS_MARKSMAN * 10) + 9] >= 20) and dam > 0) then

                		msg_print(string.format('%s is immune!', m_name))
                		dam = 0
        		end

			-- If not blocked, it may have additional effects.
			if (blocked == 0) then

				-- Check for the Piercing Shots ability!
				-- Or just defense lowering in general.
                                if (get_object_flag4(drop_ranged, TR4_LOWER_DEF)) then

                                	local defamount
					defamount = 0

                                        defamount = (damroll(drop_ranged.dd, drop_ranged.ds) * 3)

                                        
					if (monster(m_idx).defense <= 0) then defamount = 0 end
                                        msg_print(string.format('%s loses %d defense!', m_name, defamount))
                                        monster(m_idx).defense = monster(m_idx).defense - defamount
                                        if (monster(m_idx).defense <= 0) then monster(m_idx).defense = 0 end
                               end
                               -- Lower hit rate. 
                               if (get_object_flag4(drop_ranged, TR4_LOWER_HIT)) then

                                        local hitamount
					hitamount = 0

                                        hitamount = (damroll(drop_ranged.dd, drop_ranged.ds) * 3)
                                        if (monster(m_idx).hitrate <= 0) then hitamount = 0 end
                                        msg_print(string.format('%s loses %d hit rate!', m_name, hitamount))
                                        monster(m_idx).hitrate = monster(m_idx).hitrate - hitamount
                                        if (monster(m_idx).hitrate <= 0) then monster(m_idx).hitrate = 0 end
                               end
                                
                                -- Paralyzing Shot ability.
                                if (paralyze_shot == 1) then

					local stunpower
					local mresstun

					stunpower = (p_ptr.abilities[(CLASS_MARKSMAN * 10) + 2] * 20)
					mresstun = monster(m_idx).level + monster(m_idx).str

                                        if (not(get_monster_flag3(monster(m_idx).r_idx, RF3_NO_STUN))) then

                                                if (lua_randint(stunpower) >= lua_randint(mresstun)) then

                                                	msg_print(string.format('%s has been paralyzed!', m_name))
                                                        monster(m_idx).seallight = 3 + (p_ptr.abilities[(CLASS_MARKSMAN * 10) + 2] / 10)
                                                end
					end
				end
				if (drop_ranged.itemskill == 22 and p_ptr.skill_base[22] >= 40) then

					local stunpower
					local mresstun

					stunpower = p_ptr.skill[22] + p_ptr.stat_ind[A_DEX+1] + (p_ptr.abilities[(CLASS_GUNNER * 10) + 2] * 20)
					mresstun = monster(m_idx).level + monster(m_idx).str

                                        if (not(get_monster_flag3(monster(m_idx).r_idx, RF3_NO_STUN))) then

                                                if (lua_randint(stunpower) >= lua_randint(mresstun)) then

                                                	msg_print(string.format('%s is stunned!', m_name))
                                                        monster(m_idx).seallight = 3 + (p_ptr.abilities[(CLASS_GUNNER * 10) + 2] / 10)
                                                end
					end
				end
				if (drop_ranged.itemskill == 22 and p_ptr.skill_base[22] >= 70) then

					if (not(get_monster_ability(monster(m_idx), BOSS_IMMUNE_WEAPONS)) and not(m_race(monster(m_idx).r_idx).resistances[GF_PHYSICAL+1] >= 100)) then

						if (lua_randint(p_ptr.skill[22] + p_ptr.stat_ind[A_DEX+1]) >= lua_randint(monster(m_idx).level + m_race(monster(m_idx).r_idx).ac)) then

							msg_print(string.format('%s has lost all defense!', m_name))
                                                        monster(m_idx).defense = 0

						else

							local tdef
							tdef = monster(m_idx).defense / 5
							if (monster(m_idx).defense == 0) then hitamount = 0 end
							msg_print(string.format('%s has lost %d defense!', m_name, tdef))
                                                        monster(m_idx).defense = monster(m_idx).defense - tdef
							if (monster(m_idx).defense < 0) then monster(m_idx).defense = 0 end
						end
					end
				end

				-- Immolating Shot!
				if (immolating == 1) then
					place_field(221, 3 + (p_ptr.abilities[(CLASS_GUNNER * 10) + 10] / 20), monster(m_idx).fx, monster(m_idx).fy, (dam / 4))
				end

				-- Brands.
				if (drop_ranged.brandtype > 0) then

					ranged_attack = FALSE
					no_magic_return = TRUE
					fire_jump_ball(drop_ranged.brandtype, (drop_ranged.branddam), drop_ranged.brandrad, monster(m_idx).fx, monster(m_idx).fy, TRUE)
					no_magic_return = FALSE
					ranged_attack = TRUE
				end

				-- Monster is dead. Return.
				if (monster_died) then

					monster_died = FALSE
					return -1
				end

			end 

		else
			msg_print(string.format('You miss %s.', m_name))
			dam = 0
		end
	end

	-- Code for throw attacks.
	if ((dam > 0) and (throw_attack) and not(melee_attack)) then

		local hit
		local hitbonus
		local hitpenality
		local blocked

		-- Initialize variables.
		hit = 0
		hitbonus = 0
		hitpenality = 0
		blocked = 0

		-- May drop ammos on the ground.
		if (dropshots) then
			if (dropnum > 0) then drop_near_ammo(drop_ranged, dropnum, monster(m_idx).fy, monster(m_idx).fx) end
		end

		-- Set dropnum to 0 afterwards, to avoid dropping a second time.
		dropnum = 0

                -- Try to hit the monster.
		-- We get bonus hit rate from the ammos.
		hit = player_hit_monster(monster(m_idx), drop_ranged.to_h)

		-- Did we hit the monster?
		if (hit == 1) then

			-- Apply monster's resistances
			if (not((get_monster_ability(monster(m_idx), PIERCING_SPELLS)) and (element == p_ptr.elemlord))) then
				dam = dam - multiply_divide(dam, m_race(monster(m_idx).r_idx).resistances[element+1], 100)
			end

			-- Some monsters may have counters, immunities, returning, etc...

        		-- Missile is Half-Magic, Half-Physical
        		if (element == GF_MISSILE) then

				local magichalf
				local physicalhalf

                		magichalf = dam / 2
                		physicalhalf = dam / 2

				if (get_monster_ability(monster(m_idx), BOSS_IMMUNE_MAGIC)) then magichalf = 0 end
				if (get_monster_ability(monster(m_idx), BOSS_IMMUNE_WEAPONS)) then physicalhalf = 0 end

                		dam = magichalf + physicalhalf
        		end

			-- So is Ice.
        		if (element == GF_ICE) then

				local magichalf
				local physicalhalf

                		magichalf = dam / 2
                		physicalhalf = dam / 2

				if (get_monster_ability(monster(m_idx), BOSS_IMMUNE_MAGIC)) then magichalf = 0 end
				if (get_monster_ability(monster(m_idx), BOSS_IMMUNE_WEAPONS)) then physicalhalf = 0 end

                		dam = magichalf + physicalhalf
        		end
        		
			-- Check for ranged counters
			if ((m_race(monster(m_idx).r_idx).countertype == 16 or m_race(monster(m_idx).r_idx).countertype == 17 or m_race(monster(m_idx).r_idx).countertype == 18 or m_race(monster(m_idx).r_idx).countertype == 19) and lua_randint(100) <= m_race(monster(m_idx).r_idx).counterchance) then

				if (lua_randint(monster(m_idx).dex) >= lua_randint(p_ptr.stat_ind[A_DEX+1])) then

					msg_print(string.format('%s blocked your item!', m_name))
                			dam = 0
					blocked = 1
				end
			end
			if ((m_race(monster(m_idx).r_idx).countertype == 20 or m_race(monster(m_idx).r_idx).countertype == 21 or m_race(monster(m_idx).r_idx).countertype == 22 or m_race(monster(m_idx).r_idx).countertype == 23) and lua_randint(100) <= m_race(monster(m_idx).r_idx).counterchance) then

				msg_print(string.format('%s blocked your item!', m_name))
                		dam = 0
				blocked = 1
			end

        		-- Some elites/bosses are resistant or immune...
        		if (get_monster_ability(monster(m_idx), BOSS_HALVE_DAMAGES) and not(lord_piercing(10, 3, element, monster(m_idx), 1))) then

                		dam = dam / 2
        		end

        		-- GF_PHYSICAL is actually physical damages...
        		if ((get_monster_ability(monster(m_idx), BOSS_IMMUNE_MAGIC)) and element ~= GF_PHYSICAL and element ~= GF_MISSILE and element ~= GF_ICE and element ~= GF_SMITE_EVIL and element ~= GF_STEALTH_ATTACK and not(lord_piercing(1, 1, element, monster(m_idx), 1)) and dam > 0) then

                		msg_print(string.format('%s is immune!', m_name))
                		dam = 0
        		end

        		-- But then, physical immunes are immune to GF_PHYSICAL!
        		if ((get_monster_ability(monster(m_idx), BOSS_IMMUNE_WEAPONS)) and (element == GF_PHYSICAL or element == GF_SMITE_EVIL or element == GF_STEALTH_ATTACK) and element ~= GF_MISSILE and element ~= GF_ICE and dam > 0) then

                		msg_print(string.format('%s is immune!', m_name))
                		dam = 0
        		end

			-- If not blocked, it may have additional effects.
			if (blocked == 0) then

				-- Shattering Throw feat!
                        	if (p_ptr.skill_base[4] >= 70) then

                                	local defamount
					defamount = (monster(m_idx).defense / 2)

					if (monster(m_idx).defense <= 0) then defamount = 0 end
                                        msg_print(string.format('%s loses %d defense!', m_name, defamount))
                                        monster(m_idx).defense = monster(m_idx).defense - defamount
                                        if (monster(m_idx).defense <= 0) then monster(m_idx).defense = 0 end
                        	end
				-- Lower defense.
				if (get_object_flag4(drop_ranged, TR4_LOWER_DEF)) then

                                	local defamount
					defamount = 0

                                        defamount = (damroll(drop_ranged.dd, drop_ranged.ds) * 3)

					if (monster(m_idx).defense <= 0) then defamount = 0 end
                                        msg_print(string.format('%s loses %d defense!', m_name, defamount))
                                        monster(m_idx).defense = monster(m_idx).defense - defamount
                                        if (monster(m_idx).defense <= 0) then monster(m_idx).defense = 0 end
                                end
				-- Lower hit rate. 
                        	if (get_object_flag4(drop_ranged, TR4_LOWER_HIT)) then

                                        local hitamount
					hitamount = 0

                                        hitamount = (damroll(drop_ranged.dd, drop_ranged.ds) * 3)
                                        if (monster(m_idx).hitrate <= 0) then hitamount = 0 end
                                        msg_print(string.format('%s loses %d hit rate!', m_name, hitamount))
                                        monster(m_idx).hitrate = monster(m_idx).hitrate - hitamount
                                        if (monster(m_idx).hitrate <= 0) then monster(m_idx).hitrate = 0 end
                        	end

				-- Brands.
				if (drop_ranged.brandtype > 0) then

					throw_attack = FALSE
					no_magic_return = TRUE
					fire_jump_ball(drop_ranged.brandtype, (drop_ranged.branddam), drop_ranged.brandrad, monster(m_idx).fx, monster(m_idx).fy, TRUE)
					no_magic_return = FALSE
					throw_attack = TRUE
				end

				-- Monster is dead. Return.
				if (monster_died) then

					monster_died = FALSE
					return -1
				end
                               
			end 

		else
			msg_print(string.format('You miss %s.', m_name))
			dam = 0
		end
	end

	-- Code for melee attacks.
	if ((dam > 0) and (melee_attack)) then

		local           num = 0
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
		local		daggerbonus = 0
		local		meleex = monster(m_idx).fx
		local		meleey = monster(m_idx).fy
		local 		save_magic_return

		-- Piercing Stab passive feat
                if (dagger_check() and p_ptr.skill_base[16] >= 5) then daggerbonus = p_ptr.to_h / 4 end

		-- Test for hit
                if (player_hit_monster(monster(m_idx), daggerbonus) == 1) then

			-- Apply monster's resistances
			if (not((get_monster_ability(monster(m_idx), PIERCING_SPELLS)) and (element == p_ptr.elemlord))) then
				dam = dam - multiply_divide(dam, m_race(monster(m_idx).r_idx).resistances[element+1], 100)
			end

			-- If it's Smite Evil or Stealth Attack, use Physical resistance.
			if (element == GF_SMITE_EVIL or element == GF_STEALTH_ATTACK) then
				if (not((get_monster_ability(monster(m_idx), PIERCING_SPELLS)) and (p_ptr.elemlord == GF_PHYSICAL))) then
					dam = dam - multiply_divide(dam, m_race(monster(m_idx).r_idx).resistances[GF_PHYSICAL+1], 100)
				end
			end

			-- Missile is Half-Magic, Half-Physical
        		if (element == GF_MISSILE) then

				local magichalf
				local physicalhalf

                		magichalf = dam / 2
                		physicalhalf = dam / 2

				if (get_monster_ability(monster(m_idx), BOSS_IMMUNE_MAGIC)) then magichalf = 0 end
				if (get_monster_ability(monster(m_idx), BOSS_IMMUNE_WEAPONS)) then physicalhalf = 0 end

                		dam = magichalf + physicalhalf
        		end

			-- So is Ice.
        		if (element == GF_ICE) then

				local magichalf
				local physicalhalf

                		magichalf = dam / 2
                		physicalhalf = dam / 2

				if (get_monster_ability(monster(m_idx), BOSS_IMMUNE_MAGIC)) then magichalf = 0 end
				if (get_monster_ability(monster(m_idx), BOSS_IMMUNE_WEAPONS)) then physicalhalf = 0 end

                		dam = magichalf + physicalhalf
        		end

                        if (current_weapon.k_idx == 0) then

				-- We're not unarmed, but it's not a weapon...(shouldn't happen?)
				if (not(unarmed())) then

					noweapon = TRUE
				end
			end

			tmp_r_idx = monster(m_idx).r_idx
			if (noweapon == FALSE) then

				-- Check SLAY flags.
				if (get_object_flag1(current_weapon, TR1_SLAY_ANIMAL) and get_monster_flag3(monster(m_idx).r_idx, RF3_ANIMAL)) then

					dam = dam * 3
				end
				if (get_object_flag1(current_weapon, TR1_SLAY_UNDEAD) and get_monster_flag3(monster(m_idx).r_idx, RF3_UNDEAD)) then

					dam = dam * 3
				end
				if (get_object_flag1(current_weapon, TR1_SLAY_DRAGON) and get_monster_flag3(monster(m_idx).r_idx, RF3_DRAGON)) then

					dam = dam * 3
				end
				if (get_object_flag1(current_weapon, TR1_SLAY_DEMON) and get_monster_flag3(monster(m_idx).r_idx, RF3_DEMON)) then

					dam = dam * 3
				end
				if (get_object_flag1(current_weapon, TR1_SLAY_EVIL) and get_monster_flag3(monster(m_idx).r_idx, RF3_EVIL)) then

					dam = dam * 3
				end
				if (get_object_flag1(current_weapon, TR1_SLAY_ORC) and get_monster_flag3(monster(m_idx).r_idx, RF3_ORC)) then

					dam = dam * 3
				end
				if (get_object_flag1(current_weapon, TR1_SLAY_TROLL) and get_monster_flag3(monster(m_idx).r_idx, RF3_TROLL)) then

					dam = dam * 3
				end
				if (get_object_flag1(current_weapon, TR1_SLAY_GIANT) and get_monster_flag3(monster(m_idx).r_idx, RF3_GIANT)) then

					dam = dam * 3
				end

                                -- Bosses/Elites can be immune to weapons...

				if (get_monster_ability(monster(m_idx), BOSS_RETURNING) and not(no_magic_return)) then

                                        msg_print("Your attack is reflected on you!")
					monster_physical = TRUE
					lua_project(m_idx, 0, py, px, (dam / 2), element, 2)
					monster_physical = FALSE
                                end
				-- Returning counter!
				if ((m_race(monster(m_idx).r_idx).countertype == 7 or m_race(monster(m_idx).r_idx).countertype == 9) and lua_randint(100) <= m_race(monster(m_idx).r_idx).counterchance and not(no_magic_return)) then

                                        msg_print("Damages are reflected to you!")
					monster_physical = TRUE
                                        lua_project(m_idx, 0, py, px, dam, element, 2)
					monster_physical = FALSE
				end
				-- Block & Return counter!
				if ((m_race(monster(m_idx).r_idx).countertype == 10 or m_race(monster(m_idx).r_idx).countertype == 12) and lua_randint(100) <= m_race(monster(m_idx).r_idx).counterchance and not(nevermiss)) then

					local proll
					local mroll

					mroll = monster(m_idx).str + monster(m_idx).dex
					proll = p_ptr.stat_ind[A_STR+1] + p_ptr.stat_ind[A_DEX+1] + (p_ptr.abilities[(CLASS_WARRIOR * 10) + 4] * 10)

					if (lua_randint(mroll) >= lua_randint(proll)) then

						msg_print(string.format('%s blocked your attack!', m_name))
						if (not(no_magic_return)) then
							msg_print("Damages are reflected to you!")
							monster_physical = TRUE
                                        		lua_project(m_idx, 0, py, px, dam, element, 2)
							monster_physical = FALSE
						end
						dam = 0
						blocked = TRUE
					end
				end
				if ((m_race(monster(m_idx).r_idx).countertype == 13 or m_race(monster(m_idx).r_idx).countertype == 15) and lua_randint(100) <= m_race(monster(m_idx).r_idx).counterchance and not(nevermiss)) then

					msg_print(string.format('%s blocked your atack!', m_name))
					if (not(no_magic_return)) then
						msg_print("Damages are reflected to you!")
						monster_physical = TRUE
                                        	lua_project(m_idx, 0, py, px, dam, element, 2)
						monster_physical = FALSE
					end
					dam = 0
					blocked = TRUE
				end

                                if (get_monster_ability(monster(m_idx), BOSS_HALVE_DAMAGES)) then
 
                                        dam = dam / 2
                                end

				-- GF_PHYSICAL is actually physical damages...
        			if ((get_monster_ability(monster(m_idx), BOSS_IMMUNE_MAGIC)) and element ~= GF_PHYSICAL and element ~= GF_MISSILE and element ~= GF_ICE and element ~= GF_SMITE_EVIL and element ~= GF_STEALTH_ATTACK and not(lord_piercing(1, 1, element, monster(m_idx), 1)) and dam > 0) then

                			msg_print(string.format('%s is immune!', m_name))
                			dam = 0
        			end

        			-- But then, physical immunes are immune to GF_PHYSICAL!
        			if ((get_monster_ability(monster(m_idx), BOSS_IMMUNE_WEAPONS)) and (element == GF_PHYSICAL or element == GF_SMITE_EVIL or element == GF_STEALTH_ATTACK) and element ~= GF_MISSILE and element ~= GF_ICE and dam > 0) then

                			msg_print(string.format('%s is immune!', m_name))
                			dam = 0
        			end
                                
				-- Some counters...
				if ((m_race(monster(m_idx).r_idx).countertype == 1 or m_race(monster(m_idx).r_idx).countertype == 3 or m_race(monster(m_idx).r_idx).countertype == 17 or m_race(monster(m_idx).r_idx).countertype == 19) and lua_randint(100) <= m_race(monster(m_idx).r_idx).counterchance and not(nevermiss)) then

					local proll
					local mroll

					mroll = monster(m_idx).str + monster(m_idx).dex
					proll = p_ptr.stat_ind[A_STR+1] + p_ptr.stat_ind[A_DEX+1] + (p_ptr.abilities[(CLASS_WARRIOR * 10) + 4] * 10)
					if (lua_randint(mroll) >= lua_randint(proll)) then

						msg_print(string.format('%s blocked your attack!', m_name))
                				dam = 0
						blocked = TRUE
					end
				end
				if ((m_race(monster(m_idx).r_idx).countertype == 4 or m_race(monster(m_idx).r_idx).countertype == 6 or m_race(monster(m_idx).r_idx).countertype == 21 or m_race(monster(m_idx).r_idx).countertype == 23) and lua_randint(100) <= m_race(monster(m_idx).r_idx).counterchance and not(nevermiss)) then

					msg_print(string.format('%s blocked your atack!', m_name))
					dam = 0
					blocked = TRUE
				end

				-- No negative damage
				if (dam < 0) then dam = 0 end

				-- Lower defense?
                        	if (get_object_flag4(current_weapon, TR4_LOWER_DEF) and (blocked == FALSE)) then

                                	local defamount

                                	defamount = (damroll(current_weapon.dd, current_weapon.ds) * 3) / 10
                                	if (monster(m_idx).defense <= 0) then defamount = 0 end
					msg_print(string.format('%s loses %d defense!', m_name, defamount))
                                	monster(m_idx).defense = monster(m_idx).defense - defamount
                                	if (monster(m_idx).defense <= 0) then monster(m_idx).defense = 0 end
                        	end
                        	-- Lower hit rate?
                        	if (get_object_flag4(current_weapon, TR4_LOWER_HIT) and (blocked == FALSE)) then

                                	local hitamount

                                	hitamount = (damroll(current_weapon.dd, current_weapon.ds) * 3) / 10
                                	if (monster(m_idx).hitrate <= 0) then hitamount = 0 end
					msg_print(string.format('%s loses %d hit rate!', m_name, hitamount))
                                	monster(m_idx).hitrate = monster(m_idx).hitrate - hitamount
                                	if (monster(m_idx).hitrate <= 0) then monster(m_idx).hitrate = 0 end
                        	end
				-- High Monk's Disabling Blows!
                        	if (unarmed() == TRUE and p_ptr.abilities[(CLASS_ZELAR * 10) + 6] >= 1 and blocked == FALSE) then

                                	local defreduction = (p_ptr.abilities[(CLASS_ZELAR * 10) + 6] * 15)
                                	local speedreduction = 1 + (p_ptr.abilities[(CLASS_ZELAR * 10) + 6] / 2)

                                	if (not (get_monster_ability(monster(m_idx), BOSS_IMMUNE_WEAPONS)) and not (get_monster_flag1(monster(m_idx).r_idx, RF1_UNIQUE))) then

                                        	monster(m_idx).hitrate = monster(m_idx).hitrate - defreduction
                                        	monster(m_idx).defense = monster(m_idx).defense - defreduction
                                        	monster(m_idx).mspeed = monster(m_idx).mspeed - speedreduction

						if (monster(m_idx).hitrate < 0) then monster(m_idx).hitrate = 0 end
						if (monster(m_idx).defense < 0) then monster(m_idx).defense = 0 end
						if (monster(m_idx).mspeed < 0) then monster(m_idx).mspeed = 0 end
						msg_print(string.format('Def/Hit loss: %d, Speed loss: %d.', defreduction, speedreduction))
                                	end
                        	end
				-- Monster Mage's Monstrous Brutality!
				if ((unarmed() == TRUE) and p_ptr.abilities[(CLASS_MONSTER_MAGE * 10) + 9] >= 1 and not(p_ptr.body_monster == 0)) then

					local defreduction = (p_ptr.abilities[(CLASS_MONSTER_MAGE * 10) + 9] * 300)
					monster(m_idx).hitrate = monster(m_idx).hitrate - defreduction
                                        monster(m_idx).defense = monster(m_idx).defense - defreduction

					if (monster(m_idx).hitrate < 0) then monster(m_idx).hitrate = 0 end
					if (monster(m_idx).defense < 0) then monster(m_idx).defense = 0 end
					msg_print(string.format('Def/Hit loss: %d.', defreduction))
				end

				-- Message
                        	if (not(blocked)) then

					-- Sound
					sound(SOUND_HIT)

					msg_print(string.format('You hit %s!', m_name))
				end

				-- Paladin's Evil Slayer.
				if ((dam > 0) and evil_slayer == 1) then

					if (get_monster_flag3(monster(m_idx).r_idx, RF3_EVIL)) then

						local proll
						local mroll

						dam = dam + multiply_divide(dam, p_ptr.abilities[(CLASS_PALADIN * 10) + 4] * 50, 100)

						if (not(get_monster_flag3(monster(m_idx).r_idx, RF3_NO_FEAR))) then

							proll = p_ptr.abilities[(CLASS_PALADIN * 10) + 4] * 20
							mroll = monster(m_idx).level + monster(m_idx).mind

							if (lua_randint(proll) >= lua_randint(mroll)) then

								msg_print(string.format('%s becomes scared!', m_name))
								monster(m_idx).monfear = 10 + p_ptr.abilities[(CLASS_PALADIN * 10) + 4]
							end
						end
					else
						dam = dam + multiply_divide(dam, p_ptr.abilities[(CLASS_PALADIN * 10) + 4] * 10, 100)
					end
				end

				-- Fighter's critical hits!
				if ((dam > 0) and p_ptr.abilities[(CLASS_FIGHTER * 10) + 4] > 0) then

					local proll
					local mroll

					if ((normalattack) or (combatfeat)) then proll = p_ptr.abilities[(CLASS_FIGHTER * 10) + 4] * 5
					else
						proll = p_ptr.abilities[(CLASS_FIGHTER * 10) + 4] * 20
						proll = proll + p_ptr.stat_ind[A_STR+1]

						if (critmod > 0) then proll = proll * critmod end

						if (get_monster_ability(monster(m_idx), TAUNTED)) then proll = proll * 2 end
					end

					mroll = monster(m_idx).level + monster(m_idx).str

					if (lua_randint(proll) >= lua_randint(mroll)) then

						msg_print(string.format('%s receives critical hit!', m_name))
						dam = dam * (2 + (p_ptr.abilities[(CLASS_FIGHTER * 10) + 4] / 10))
						if (not(get_monster_flag3(monster(m_idx).r_idx, RF3_NO_STUN))) then

                                        		monster(m_idx).seallight = 3 + (p_ptr.abilities[(CLASS_FIGHTER * 10) + 4] / 10)
                                		end
					end
				end

				-- Monsters: Elite 'Crushing Blows' ability.
				if ((dam > 0) and p_ptr.prace == RACE_MONSTER and get_player_monster_ability(BOSS_CURSED_HITS)) then

					local proll
					local mroll

					if ((p_ptr.events[29019] > 0) and not(combatfeat)) then

						proll = p_ptr.abilities[(CLASS_MONSTER * 10) + 10] * 20
						proll = proll + p_ptr.stat_ind[A_STR+1]
						if (crushingblows > 0) then proll = proll * crushingblows end
					else
						proll = p_ptr.abilities[(CLASS_MONSTER * 10) + 10] * 5
					end

					mroll = monster(m_idx).level + monster(m_idx).str

					if (lua_randint(proll) >= lua_randint(mroll)) then

						msg_print(string.format('%s receives a crushing blow!', m_name))
						if (not(get_monster_flag3(monster(m_idx).r_idx, RF3_NO_STUN))) then

                                        		monster(m_idx).seallight = 5
                                		end
						if (not(get_monster_flag3(monster(m_idx).r_idx, RF3_NO_CONF))) then

                                        		monster(m_idx).confused = 5
                                		end
						
						monster(m_idx).hitrate = monster(m_idx).hitrate - (monster(m_idx).hitrate / 4)
					end
				end

				-- Brands.
				if (unarmed()) then
					if (inven(INVEN_HANDS).brandtype > 0 and not(blocked)) then
						save_magic_return = no_magic_return
						melee_attack = FALSE
						no_magic_return = TRUE
						fire_jump_ball(inven(INVEN_HANDS).brandtype, (inven(INVEN_HANDS).branddam), inven(INVEN_HANDS).brandrad, meleex, meleey, TRUE)
						no_magic_return = save_magic_return
						melee_attack = TRUE
					end
				else
					if (current_weapon.brandtype > 0 and not(blocked)) then
						save_magic_return = no_magic_return
						melee_attack = FALSE
						no_magic_return = TRUE
						fire_jump_ball(current_weapon.brandtype, (current_weapon.branddam), current_weapon.brandrad, meleex, meleey, TRUE)
						no_magic_return = save_magic_return
						melee_attack = TRUE
					end
				end

				-- Monster is dead due to brands? Return.
				if (monster_died) then

					monster_died = FALSE
					crushingblows = 0
					return -1
				end
				-- Kensai's The Dragon's Fury!
				if (p_ptr.abilities[(CLASS_KENSAI * 10)+9] > 0 and not(blocked) and (scorptail == 0) and (kensai_equip()) and (current_weapon.itemskill == 12)) then
					local furydam = originalpower / 5
					furydam = furydam + (furydam * p_ptr.stat_ind[A_WIS+1] * 2 / 100)
					furydam = furydam + (furydam * p_ptr.abilities[(CLASS_KENSAI * 10)+9] * 10 / 100)
					save_magic_return = no_magic_return
					melee_attack = FALSE
					no_magic_return = TRUE
					fire_jump_ball(GF_MANA, furydam, 0, meleex, meleey, TRUE)
					no_magic_return = save_magic_return
					melee_attack = TRUE
				end
				-- Monster is dead due to Dragon's Fury? Return.
				if (monster_died) then

					monster_died = FALSE
					crushingblows = 0
					return -1
				end
			end
			
		else
			-- Sound
			sound(SOUND_MISS)

			-- Message
			msg_print(string.format('You miss %s!', m_name))

			-- No damages when you miss.
			dam = 0
		end

	end

	-- A special counter is coded here.
	-- This is counter 1000, the "Greed" counter.
	-- The more valuable items you possess, the more damages they absorb.
	if ((m_race(monster(m_idx).r_idx).countertype == 1000) and (lua_randint(100) <= m_race(monster(m_idx).r_idx).counterchance)) then
		
		local greedsoak

		greedsoak = 0

		i = 1
        	while (i < 65) do

                	-- Add the values of items you carry/wear.

			if (inven(i).tval > 0) then
                		greedsoak = greedsoak + object_value_real(inven(i))
			end

                	i = i + 1
        	end

		if (greedsoak < 0) then greedsoak = 0 end

		dam = dam - greedsoak
		if (dam < 0) then dam = 0 end
	end

	-- Shadow Queen's evasion ability.
	if (m_race(monster(m_idx).r_idx).event_misc == 305 and dam > 0 and ((melee_attack) or (ranged_attack) or (throw_attack)) and not(nevermiss)) then

		local pstat

		pstat = p_ptr.stat_ind[A_DEX+1]

		if (lua_randint(monster(m_idx).dex) >= lua_randint(pstat)) then

			msg_print(string.format('%s evades your attack!', m_name))
			dam = 0
		end
	end

	-- Nightmare Shadow Mistress's evasion ability.
	-- Kind of like the above ability. Only a lot cheaper.
	if (m_race(monster(m_idx).r_idx).event_misc == 2303 and dam > 0) then

		local pstat

		if ((melee_attack) or (ranged_attack) or (throw_attack)) then

			pstat = p_ptr.stat_ind[A_DEX+1]
		else
			pstat = p_ptr.stat_ind[A_INT+1] + p_ptr.stat_ind[A_WIS+1]
		end

		if (lua_randint(monster(m_idx).dex) >= lua_randint(pstat)) then

			msg_print(string.format('%s evades all damages!', m_name))
			dam = 0
		end
	end

	-- Sphere of Essences.
	if (m_race(monster(m_idx).r_idx).event_misc == 2312 and dam > 0) then

		local totres
		local r

		totres = 0

		for r = 0, MAX_RESIST do

			totres = totres + p_ptr.resistances[r]
		end

		if (totres > 100) then totres = 100 end
		if (totres < 0) then totres = 0 end

		dam = dam - multiply_divide(dam, totres, 100)
	end

	-- Nightmare Beast.
	if (m_race(monster(m_idx).r_idx).event_misc == 2314 and dam > 0) then

		if (not(element == GF_OLD_HEAL)) then

			dam = dam - multiply_divide(dam, 75, 100)
		end
	end

	-- This is Christina's special ability.
	-- All damages and magic powers are halved.
	if (m_race(monster(m_idx).r_idx).event_misc == 1357 and dam > 0) then

		dam = dam / 2
	end

	-- Reset crushing blows counter.
	crushingblows = 0

	-- Reset normal attacks.
	normalattack = 0

	-- Reset Critical mods.
	critmod = 0

	-- If damages are > 0, some elements may have special effects.
	-- Some of them are ONLY that, and do no real damages.

	-- Retrograde and Retrograde Darkness should not be resistable.
	-- Applies to some others as well.
	if (element == GF_RETROGRADE or element == GF_RETROGRADE_DARKNESS or element == GF_TAUNT or element == GF_WARCRY) then dam = 100 end

	if (dam > 0) then

		local anger

		if (who == 0) then anger = 1
		else anger = 0
		end

		-- Toxic, FrostFire, Grey and Missile are multi-types!
		if (element == GF_TOXIC) then

			local aciddam
			local poisdam
			local radiodam

			aciddam = dam / 3
			poisdam = dam / 3
			radiodam = dam / 3

			aciddam = aciddam - multiply_divide(aciddam, m_race(monster(m_idx).r_idx).resistances[GF_ACID+1], 100)
			poisdam = poisdam - multiply_divide(poisdam, m_race(monster(m_idx).r_idx).resistances[GF_POIS+1], 100)
			radiodam = radiodam - multiply_divide(radiodam, m_race(monster(m_idx).r_idx).resistances[GF_RADIO+1], 100)
			dam = aciddam + poisdam + radiodam
			m_race(monster(m_idx).r_idx).r_resist[GF_ACID+1] = 1
			m_race(monster(m_idx).r_idx).r_resist[GF_POIS+1] = 1
			m_race(monster(m_idx).r_idx).r_resist[GF_RADIO+1] = 1
		end

		-- FrostFire!
		-- A combination of Fire and Cold!
		if (element == GF_FROSTFIRE) then

			local firedam
			local colddam

			firedam = dam / 2
			colddam = dam / 2

			firedam = firedam - multiply_divide(firedam, m_race(monster(m_idx).r_idx).resistances[GF_FIRE+1], 100)
			colddam = colddam - multiply_divide(colddam, m_race(monster(m_idx).r_idx).resistances[GF_COLD+1], 100)

			dam = firedam + colddam
			m_race(monster(m_idx).r_idx).r_resist[GF_FIRE+1] = 1
			m_race(monster(m_idx).r_idx).r_resist[GF_COLD+1] = 1

		end

		-- Grey!
		-- A combination of Light and Dark!
		if (element == GF_GREY) then

			local litedam
			local darkdam

			litedam = dam / 2
			darkdam = dam / 2

			litedam = litedam - multiply_divide(litedam, m_race(monster(m_idx).r_idx).resistances[GF_LITE+1], 100)
			darkdam = darkdam - multiply_divide(darkdam, m_race(monster(m_idx).r_idx).resistances[GF_DARK+1], 100)

			dam = litedam + darkdam
			m_race(monster(m_idx).r_idx).r_resist[GF_LITE+1] = 1
			m_race(monster(m_idx).r_idx).r_resist[GF_DARK+1] = 1

		end

		-- Mud!
		-- A combination of Water and Earth!
		if (element == GF_MUD) then

			local earthdam
			local waterdam

			earthdam = dam / 2
			waterdam = dam / 2

			earthdam = earthdam - multiply_divide(earthdam, m_race(monster(m_idx).r_idx).resistances[GF_EARTH+1], 100)
			waterdam = waterdam - multiply_divide(waterdam, m_race(monster(m_idx).r_idx).resistances[GF_WATER+1], 100)

			dam = earthdam + waterdam
			m_race(monster(m_idx).r_idx).r_resist[GF_EARTH+1] = 1
			m_race(monster(m_idx).r_idx).r_resist[GF_WATER+1] = 1

		end

		-- Ice!
		-- A combination of Cold and Physical!
		if (element == GF_ICE) then

			local icedam
			local physdam

			icedam = dam / 2
			physdam = dam / 2

			icedam = icedam - multiply_divide(icedam, m_race(monster(m_idx).r_idx).resistances[GF_COLD+1], 100)
			physdam = physdam - multiply_divide(physdam, m_race(monster(m_idx).r_idx).resistances[GF_PHYSICAL+1], 100)

			dam = icedam + physdam
			m_race(monster(m_idx).r_idx).r_resist[GF_COLD+1] = 1
			m_race(monster(m_idx).r_idx).r_resist[GF_PHYSICAL+1] = 1

		end

		-- Elemental!
		if (element == GF_ELEMENTAL) then

			local res
			local r
			local original

			original = dam

			for r = GF_FIRE, GF_MANA do

				if (not(r == GF_PHYSICAL)) then
					dam = dam - multiply_divide((original / 15), m_race(monster(m_idx).r_idx).resistances[r+1], 100)
					m_race(monster(m_idx).r_idx).r_resist[r+1] = 1
				end
			end
		end

		-- Confusion
		-- This is the damaging confusion.
		-- Damages + confuse chance, but highly resisted by confusion resistant
		-- enemies, as well as mana/chaos immune enemies.
		if (element == GF_CONFUSION) then

			if (music == 1) then
				ppower = (p_ptr.stat_ind[A_CHR+1] + (p_ptr.skill[23] * 3) + p_ptr.skill[2])
				if (p_ptr.abilities[(CLASS_BARD * 10) + 1] > 0) then

					ppower = ppower + multiply_divide(ppower, p_ptr.abilities[(CLASS_BARD * 10) + 1] * 3, 100)
				end
			else ppower = (p_ptr.stat_ind[A_INT+1] + p_ptr.stat_ind[A_WIS+1] + (p_ptr.skill[23] * 3) + p_ptr.skill[2]) end
			mpower = (monster(m_idx).level + monster(m_idx).mind)

			-- Monster race can get some bonus.
			if (p_ptr.prace == RACE_MONSTER) then ppower = ppower + p_ptr.events[29018] end

			if (get_monster_flag3(monster(m_idx).r_idx, RF3_NO_CONF)) then

				memorize_race_flag3(monster(m_idx).r_idx, RF3_NO_CONF)
                                dam = dam / 3
			end

			-- This is a chaos/mana type, though immunity to either negates all damages.
			dam = dam - multiply_divide(dam, m_race(monster(m_idx).r_idx).resistances[GF_CHAOS+1], 100)
			dam = dam - multiply_divide(dam, m_race(monster(m_idx).r_idx).resistances[GF_MANA+1], 100)

			if (dam > 0 and not(get_monster_flag3(monster(m_idx).r_idx, RF3_NO_CONF)) and not(get_monster_flag1(monster(m_idx).r_idx, RF1_UNIQUE)) and monster(m_idx).boss <= 0) then

				if (lua_randint(ppower) >= lua_randint(mpower)) then

					monster(m_idx).confused = 10
                                	msg_print(string.format('%s is confused!', m_name))
				end
			end
		end

		-- Fear!
		if (element == GF_FEAR) then

			if (music == 1) then
				ppower = (dam + p_ptr.stat_ind[A_CHR+1])
				if (p_ptr.abilities[(CLASS_BARD * 10) + 1] > 0) then

					ppower = ppower + multiply_divide(ppower, p_ptr.abilities[(CLASS_BARD * 10) + 1] * 3, 100)
				end
			else ppower = (dam + p_ptr.stat_ind[A_INT+1] + p_ptr.stat_ind[A_WIS+1]) end
			mpower = (monster(m_idx).level + monster(m_idx).mind)

			-- Monster race can get some bonus.
			if (p_ptr.prace == RACE_MONSTER) then ppower = ppower + p_ptr.events[29018] end

			-- It does not affect uniques, elites, bosses and fear resistants.
                        if ((get_monster_flag1(monster(m_idx).r_idx, RF1_UNIQUE)) or (get_monster_flag3(monster(m_idx).r_idx, RF3_NO_FEAR)) or monster(m_idx).boss > 0) then

				if (get_monster_flag3(monster(m_idx).r_idx, RF3_NO_FEAR)) then

					memorize_race_flag3(monster(m_idx).r_idx, RF3_NO_FEAR)
				end
                                msg_print(string.format('%s is unaffected.', m_name))
                        else
			
				if (lua_randint(ppower) >= lua_randint(mpower)) then

                                	monster(m_idx).monfear = dam
					msg_print(string.format('%s becomes scared!', m_name))

				else
					msg_print(string.format('%s resists.', m_name))
				end
			end

                        -- This does not cause damages.
                        dam = 0

                end

		-- Stats lowering.
		-- These does not do anything to other monsters, and you shouldn't be able
		-- to use them. But I put them here just in case.
		if (element == GF_LOSE_STR or element == GF_LOSE_INT or element == GF_LOSE_WIS or element == GF_LOSE_DEX or element == GF_LOSE_CON or element == GF_LOSE_CHR or element == GF_LOSE_ALL or element == GF_LOSE_EXP) then

			dam = 0
		end

		-- Heal monster
		if (element == GF_OLD_HEAL) then

			-- The Nightmare Beast actually takes damages!
			if (m_race(monster(m_idx).r_idx).event_misc == 2314 and dam > 0) then

				-- It does anger(even if NB isn't supposed to be an ally)
				anger = 1
			else

				-- Heal
				monster(m_idx).hp = monster(m_idx).hp + dam

				-- Can't go beyong max hp.
                        	if ((monster(m_idx).hp > monster(m_idx).maxhp) or (monster(m_idx).hp < 0)) then monster(m_idx).hp = monster(m_idx).maxhp end

				-- Update
				update_and_handle()

				-- Message
				-- No messages for healing songs however.
				if (music == 0) then msg_print(string.format('%s looks healthier.', m_name)) end

				-- No damages.
				dam = 0
			
				-- Not angry.
				anger = 0
			end
		end

		-- Haste monster.
		if (element == GF_OLD_SPEED) then

			-- Speed up
			monster(m_idx).mspeed = monster(m_idx).mspeed + dam

			-- Limit of 180 speed.
			if (monster(m_idx).mspeed > 180) then monster(m_idx).mspeed = 180 end

			msg_print(string.format('%s starts moving faster.', m_name))

			-- No damages.
			dam = 0

			-- Not angry.
			anger = 0
		end

		-- Slow Monster
		if (element == GF_OLD_SLOW) then

			if (music == 1) then
				ppower = (dam + p_ptr.stat_ind[A_CHR+1])
				if (p_ptr.abilities[(CLASS_BARD * 10) + 1] > 0) then

					ppower = ppower + multiply_divide(ppower, p_ptr.abilities[(CLASS_BARD * 10) + 1] * 3, 100)
				end
			else ppower = (dam + p_ptr.stat_ind[A_INT+1] + p_ptr.stat_ind[A_WIS+1]) end
			mpower = (monster(m_idx).level + monster(m_idx).mind)

			-- Monster race can get some bonus.
			if (p_ptr.prace == RACE_MONSTER) then ppower = ppower + p_ptr.events[29018] end

			-- It does not affect uniques.
                        if ((get_monster_flag1(monster(m_idx).r_idx, RF1_UNIQUE)) or monster(m_idx).boss > 1) then

                                msg_print(string.format('%s is unaffected.', m_name))
                        else

				if (lua_randint(ppower) >= lua_randint(mpower)) then

					-- Cannot reduce lower than 60.
					if (monster(m_idx).mspeed > 60) then

						monster(m_idx).mspeed = monster(m_idx).mspeed - 10
						msg_print(string.format('%s starts moving slower.', m_name))
					else
						msg_print(string.format('%s cannot be slowed further.', m_name))
					end

				else

					msg_print(string.format('%s resists.', m_name))
				end
			end

			-- No damage.
			dam = 0
		end

		-- Sleep
		if (element == GF_OLD_SLEEP) then

			if (music == 1) then
				ppower = (dam + p_ptr.stat_ind[A_CHR+1])
				if (p_ptr.abilities[(CLASS_BARD * 10) + 1] > 0) then

					ppower = ppower + multiply_divide(ppower, p_ptr.abilities[(CLASS_BARD * 10) + 1] * 3, 100)
				end
			else ppower = (dam + p_ptr.stat_ind[A_INT+1] + p_ptr.stat_ind[A_WIS+1]) end
			mpower = (monster(m_idx).level + monster(m_idx).mind)

			-- Monster race can get some bonus.
			if (p_ptr.prace == RACE_MONSTER) then ppower = ppower + p_ptr.events[29018] end

			-- It does not affect uniques nor bosses. But elites can be affected.
                        if ((get_monster_flag1(monster(m_idx).r_idx, RF1_UNIQUE)) or (get_monster_flag3(monster(m_idx).r_idx, RF3_NO_SLEEP)) or monster(m_idx).boss >= 2) then

				if (get_monster_flag3(monster(m_idx).r_idx, RF3_NO_SLEEP)) then

					memorize_race_flag3(monster(m_idx).r_idx, RF3_NO_SLEEP)
				end
                                msg_print(string.format('%s is unaffected.', m_name))
			else

				if (lua_randint(ppower) >= lua_randint(mpower)) then

					-- Go to sleep.
					monster(m_idx).csleep = 500
					msg_print(string.format('%s falls asleep.', m_name))
				else
					msg_print(string.format('%s resists.', m_name))
				end
			end

			-- No damage.
			dam = 0
		end

		-- Confusion
		if (element == GF_OLD_CONF) then

			if (music == 1) then
				ppower = (dam + p_ptr.stat_ind[A_CHR+1])
				if (p_ptr.abilities[(CLASS_BARD * 10) + 1] > 0) then

					ppower = ppower + multiply_divide(ppower, p_ptr.abilities[(CLASS_BARD * 10) + 1] * 3, 100)
				end
			else ppower = (dam + p_ptr.stat_ind[A_INT+1] + p_ptr.stat_ind[A_WIS+1]) end
			mpower = (monster(m_idx).level + monster(m_idx).mind)

			-- Monster race can get some bonus.
			if (p_ptr.prace == RACE_MONSTER) then ppower = ppower + p_ptr.events[29018] end

			-- It does not affect uniques nor bosses. But elites can be affected.
                        if ((get_monster_flag1(monster(m_idx).r_idx, RF1_UNIQUE)) or (get_monster_flag3(monster(m_idx).r_idx, RF3_NO_CONF)) or monster(m_idx).boss >= 2) then

				if (get_monster_flag3(monster(m_idx).r_idx, RF3_NO_CONF)) then

					memorize_race_flag3(monster(m_idx).r_idx, RF3_NO_CONF)
				end
                                msg_print(string.format('%s is unaffected.', m_name))
			else

				if (lua_randint(ppower) >= lua_randint(mpower)) then

					-- Becomes confused.
					monster(m_idx).confused = 10
					msg_print(string.format('%s becomes confused!', m_name))
				else
					msg_print(string.format('%s resists.', m_name))
				end
			end

			-- No damage.
			dam = 0
		end

		-- Stun
		-- Now changed to be similar to Paralyze, with a lesser success rate, and fixed duration.
		if (element == GF_STUN) then

			if (music == 1) then
				ppower = (dam + p_ptr.stat_ind[A_CHR+1])
				if (p_ptr.abilities[(CLASS_BARD * 10) + 1] > 0) then

					ppower = ppower + multiply_divide(ppower, p_ptr.abilities[(CLASS_BARD * 10) + 1] * 3, 100)
				end
			else ppower = (dam + p_ptr.stat_ind[A_INT+1] + p_ptr.stat_ind[A_WIS+1]) end
			mpower = (monster(m_idx).level + monster(m_idx).mind)

			-- Monster race can get some bonus.
			if (p_ptr.prace == RACE_MONSTER) then ppower = ppower + p_ptr.events[29018] end

			-- It does not affect uniques nor bosses. But elites can be affected.
                        if ((get_monster_flag1(monster(m_idx).r_idx, RF1_UNIQUE)) or (get_monster_flag3(monster(m_idx).r_idx, RF3_NO_STUN)) or monster(m_idx).boss >= 2) then

				if (get_monster_flag3(monster(m_idx).r_idx, RF3_NO_STUN)) then

					memorize_race_flag3(monster(m_idx).r_idx, RF3_NO_STUN)
				end
                                msg_print(string.format('%s is unaffected.', m_name))
			else

				if (lua_randint(ppower) >= lua_randint(mpower)) then

					-- Paralyze enemy.
					monster(m_idx).seallight = 3
					msg_print(string.format('%s is stunned!', m_name))
				else
					msg_print(string.format('%s resists.', m_name))
				end
			end

			-- No damage.
			dam = 0
		end

		-- Alteration: Recuce Level
		-- Formerly known as Weaken in previous versions. This lowers the enemy's level.
		-- It will also lower all attributes accordingly. Hence the name "Weaken".
		-- Note that this does not reduce the enemy's hp.
                if (element == GF_WEAKEN) then

			ppower = (p_ptr.stat_ind[A_INT+1] + p_ptr.stat_ind[A_WIS+1] + (p_ptr.skill[24] * 3) + p_ptr.skill[2])
			mpower = (monster(m_idx).level + monster(m_idx).mind)

                        -- Quest bosses cannot be afflicted.
                        if (not(get_monster_flag1(monster(m_idx).r_idx, RF1_QUESTOR))) then

				-- Otherwise, ANYTHING can be affected by this!
				-- But of course, powerful enemies can resist.
				if (lua_randint(ppower) >= lua_randint(mpower)) then

                        		monster(m_idx).level = monster(m_idx).level - dam
                                	if (monster(m_idx).level < 1) then monster(m_idx).level = 1 end
					apply_monster_level_stats(monster(m_idx))
                                	msg_print(string.format('%s has been weakened.', m_name))

				else
					msg_print(string.format('%s resists.', m_name))
				end

                        else
				msg_print(string.format('%s cannot be affected.', m_name))
			end

			-- No damages.
                        dam = 0

		end

		-- Mage Ability: Slow Down.
		if (element == GF_SLOW_DOWN) then

			ppower = p_ptr.abilities[(CLASS_MAGE * 10) + 5] * 20
			mpower = (monster(m_idx).level + monster(m_idx).mind)

			if (not(get_monster_flag1(monster(m_idx).r_idx, RF1_QUESTOR))) then

				if (lua_randint(ppower) >= lua_randint(mpower)) then

					-- This is actually considered an "ability" of the monster.
                                	if (get_monster_ability(monster(m_idx), CURSE_SLOW_DOWN)) then

                                        	msg_print(string.format('%s has already been slowed down!', m_name))

                                	else

                                        	local speedfrac
                                        	speedfrac = (monster(m_idx).mspeed * (5 + (p_ptr.abilities[(CLASS_MAGE * 10) + 5] / 2))) / 100
                                        	monster(m_idx).mspeed = monster(m_idx).mspeed - speedfrac
						give_monster_ability(monster(m_idx), CURSE_SLOW_DOWN)
                                        	msg_print(string.format('%s is now slower.', m_name))
                                	end

				else
					msg_print(string.format('%s resists.', m_name))
				end
			else
				msg_print(string.format('%s cannot be affected.', m_name))
			end

			-- No damages.
			dam = 0
		end

		-- Alteration: Life Blast
                if (element == GF_LIFE_BLAST) then

			ppower = (p_ptr.stat_ind[A_INT+1] + p_ptr.stat_ind[A_WIS+1] + (p_ptr.skill[24] * 3) + p_ptr.skill[2])
			mpower = (monster(m_idx).level + monster(m_idx).mind)

			-- Monster race can get some bonus.
			if (p_ptr.prace == RACE_MONSTER) then ppower = ppower + p_ptr.events[29018] end

			if (not(lua_randint(ppower) >= lua_randint(mpower))) then

				msg_print(string.format('%s resists.', m_name))
				dam = 0
			end
		end

		-- Alteration: Halve Damages.
		if (element == GF_HALVE_DAMAGES) then

			ppower = (p_ptr.stat_ind[A_INT+1] + p_ptr.stat_ind[A_WIS+1] + (p_ptr.skill[24] * 3) + p_ptr.skill[2])
			mpower = (monster(m_idx).level + monster(m_idx).mind)

                        if (not(get_monster_flag1(monster(m_idx).r_idx, RF1_QUESTOR))) then

				if (lua_randint(ppower) >= lua_randint(mpower)) then

					-- This is actually considered an "ability" of the monster.
                                	if (get_monster_ability(monster(m_idx), CURSE_HALVE_DAMAGES)) then

                                        	msg_print(string.format('%s has already been affected!', m_name))

                                	else
						give_monster_ability(monster(m_idx), CURSE_HALVE_DAMAGES)
                                        	msg_print(string.format('%s melee damages have been halved.', m_name))
                                	end

				else
					msg_print(string.format('%s resists.', m_name))
				end
			else
				msg_print(string.format('%s cannot be affected.', m_name))
			end

			-- No damages.
			dam = 0
		end

		-- Alteration: Halve Magic
                if (element == GF_HALVE_MAGIC) then

			ppower = (p_ptr.stat_ind[A_INT+1] + p_ptr.stat_ind[A_WIS+1] + (p_ptr.skill[24] * 3) + p_ptr.skill[2])
			mpower = (monster(m_idx).level + monster(m_idx).mind)

                        if (not(get_monster_flag1(monster(m_idx).r_idx, RF1_QUESTOR))) then

				if (lua_randint(ppower) >= lua_randint(mpower)) then

					-- This is actually considered an "ability" of the monster.
                                	if (get_monster_ability(monster(m_idx), CURSE_HALVE_MAGIC)) then

                                        	msg_print(string.format('%s has already been affected!', m_name))

                                	else
						give_monster_ability(monster(m_idx), CURSE_HALVE_MAGIC)
                                        	msg_print(string.format('%s magic damages have been halved.', m_name))
                                	end

				else
					msg_print(string.format('%s resists.', m_name))
				end
			else
				msg_print(string.format('%s cannot be affected.', m_name))
			end

			-- No damages.
			dam = 0
		end

		-- Alteration: Lock
		if (element == GF_LOCK) then

			ppower = (p_ptr.stat_ind[A_INT+1] + p_ptr.stat_ind[A_WIS+1] + (p_ptr.skill[24] * 3) + p_ptr.skill[2])
			mpower = (monster(m_idx).level + monster(m_idx).mind)

			-- Elites, Bosses and Uniques are not affected.
			if (not(get_monster_flag1(monster(m_idx).r_idx, RF1_UNIQUE)) and monster(m_idx).boss == 0) then

				if (lua_randint(ppower) >= lua_randint(mpower)) then

					-- This is actually considered an "ability" of the monster.
                                	if (get_monster_ability(monster(m_idx), CURSE_LOCK)) then

                                        	msg_print(string.format('%s has already been affected!', m_name))

                                	else
						give_monster_ability(monster(m_idx), CURSE_LOCK)
                                        	msg_print(string.format('%s magic attacks have been locked.', m_name))
                                	end

				else
					msg_print(string.format('%s resists.', m_name))
				end
			else
				msg_print(string.format('%s cannot be affected.', m_name))
			end

			-- No damages.
			dam = 0
		end

		-- Alteration: Stone to Mud.
		-- This only damages enemies that are weak to it.
                if (element == GF_STONE_TO_MUD) then

			if (m_race(monster(m_idx).r_idx).resistances[GF_STONE_TO_MUD + 1] >= 0) then

				dam = 0
			end
		end

		-- Mysticism: Harm
                if (element == GF_HARM) then

			ppower = (p_ptr.stat_ind[A_INT+1] + p_ptr.stat_ind[A_WIS+1] + (p_ptr.skill[25] * 3) + p_ptr.skill[2])
			mpower = (monster(m_idx).level + monster(m_idx).str)

			-- Monster race can get some bonus.
			if (p_ptr.prace == RACE_MONSTER) then ppower = ppower + p_ptr.events[29018] end

			if (not(lua_randint(ppower) >= lua_randint(mpower))) then

				msg_print(string.format('%s resists.', m_name))
				dam = 0
			end
		end

		-- Mysticism: Smite Undead
		-- Must be an undead monster to deal damages.
                if (element == GF_UNDEAD_SMITE) then

			if (not(get_monster_flag3(monster(m_idx).r_idx, RF3_UNDEAD))) then

				msg_print(string.format('%s is unaffected.', m_name))
				dam = 0
			end
		end

		-- Mysticism: Smite Demon
		-- Must be a demon to deal damages.
                if (element == GF_DEMON_SMITE) then

			if (not(get_monster_flag3(monster(m_idx).r_idx, RF3_DEMON))) then

				msg_print(string.format('%s is unaffected.', m_name))
				dam = 0
			end
		end

		-- Mysticism: Smite Evil
		-- Must be an evil monster to deal damages.
                if (element == GF_EVIL_SMITE) then

			if (not(get_monster_flag3(monster(m_idx).r_idx, RF3_EVIL))) then

				-- msg_print(string.format('%s is unaffected.', m_name))
				dam = 0
			end
		end

		-- Mysticism: Smite Good
		-- Must be a good monster to deal damages.
                if (element == GF_GOOD_SMITE) then

			if (not(get_monster_flag3(monster(m_idx).r_idx, RF3_GOOD))) then

				-- msg_print(string.format('%s is unaffected.', m_name))
				dam = 0
			end
		end

		-- Mage Ability: Damages Curse
		if (element == GF_DAMAGES_CURSE) then

			ppower = p_ptr.abilities[(CLASS_MAGE * 10) + 7] * 10
			mpower = (monster(m_idx).level + monster(m_idx).mind)

			-- No checks for uniques/bosses. This one can work on anything.
			if (lua_randint(ppower) >= lua_randint(mpower)) then

				-- This is actually considered an "ability" of the monster.
                                if (get_monster_ability(monster(m_idx), CURSE_DAMAGES_CURSE)) then

                                        msg_print(string.format('%s has already been cursed!', m_name))

                                else
					give_monster_ability(monster(m_idx), CURSE_DAMAGES_CURSE)
                                        msg_print(string.format('%s has been cursed!', m_name))
                                end
			else

				msg_print(string.format('%s resists.', m_name))
			end
			
			-- No damages.
			dam = 0
		end

		-- Alteration: Retrograde.
		if (element == GF_RETROGRADE) then

			
			ppower = (p_ptr.stat_ind[A_INT+1] + p_ptr.stat_ind[A_WIS+1] + (p_ptr.skill[24] * 3) + p_ptr.skill[2])
			mpower = (monster(m_idx).level + monster(m_idx).mind)

                        -- Must be an elite or boss.
                        if (monster(m_idx).boss >= 1) then

				if (lua_randint(ppower) >= lua_randint(mpower)) then

                                	monster(m_idx).boss = 0
					remove_monster_ability(monster(m_idx), BOSS_IMMUNE_WEAPONS)
					remove_monster_ability(monster(m_idx), BOSS_IMMUNE_MAGIC)
					remove_monster_ability(monster(m_idx), BOSS_DOUBLE_DAMAGES)
					remove_monster_ability(monster(m_idx), BOSS_RETURNING)
					remove_monster_ability(monster(m_idx), BOSS_CURSED_HITS)
					remove_monster_ability(monster(m_idx), BOSS_DOUBLE_MAGIC)
					remove_monster_ability(monster(m_idx), BOSS_HALVE_DAMAGES)
					remove_monster_ability(monster(m_idx), BOSS_MAGIC_RETURNING)
                                	msg_print(string.format('%s has been retrograded.', m_name))

				else

					msg_print(string.format('%s resists.', m_name))
				end
                        end

                        -- No damages.
			dam = 0
		end

		-- Divination: Divination
                if (element == GF_DIVINATION) then

			ppower = (p_ptr.stat_ind[A_INT+1] + p_ptr.stat_ind[A_WIS+1] + (p_ptr.skill[27] * 3) + p_ptr.skill[2]) + ((p_ptr.skill[27] / 5) * p_ptr.abilities[(CLASS_DIVINER * 10) + 9])
			mpower = (monster(m_idx).level + monster(m_idx).mind)

			-- Monster race can get some bonus.
			if (p_ptr.prace == RACE_MONSTER) then ppower = ppower + p_ptr.events[29018] end

			-- Unique takes only 25% damages a normal monster would.
			if (get_monster_flag1(monster(m_idx).r_idx, RF1_UNIQUE)) then dam = dam / 4 end

			if (not(lua_randint(ppower) >= lua_randint(mpower))) then

				msg_print(string.format('%s resists.', m_name))
				dam = 0
			end
		end

		-- Paladin ability: Retrograde Darkness.
		if (element == GF_RETROGRADE_DARKNESS) then

			ppower = p_ptr.abilities[(CLASS_PRIEST * 10) + 9] * 30
			mpower = (monster(m_idx).level + monster(m_idx).mind)

			if (monster(m_idx).boss >= 1 and (get_monster_flag3(monster(m_idx).r_idx, RF3_UNDEAD) or get_monster_flag3(monster(m_idx).r_idx, RF3_DEMON))) then

				if (lua_randint(ppower) >= lua_randint(mpower)) then

                                        monster(m_idx).boss = 0
					remove_monster_ability(monster(m_idx), BOSS_IMMUNE_WEAPONS)
					remove_monster_ability(monster(m_idx), BOSS_IMMUNE_MAGIC)
					remove_monster_ability(monster(m_idx), BOSS_DOUBLE_DAMAGES)
					remove_monster_ability(monster(m_idx), BOSS_RETURNING)
					remove_monster_ability(monster(m_idx), BOSS_CURSED_HITS)
					remove_monster_ability(monster(m_idx), BOSS_DOUBLE_MAGIC)
					remove_monster_ability(monster(m_idx), BOSS_HALVE_DAMAGES)
					remove_monster_ability(monster(m_idx), BOSS_MAGIC_RETURNING)
                                	msg_print(string.format('%s has been retrograded.', m_name))
				else
					msg_print(string.format('%s resists.', m_name))
				end
			else
				msg_print(string.format('%s is unaffected.', m_name))
			end

			-- No damages.
			dam = 0
		end

		-- Fighter Ability: Taunt
		if (element == GF_TAUNT) then

			local chabonus
			if (scorptail == 1) then
				ppower = (p_ptr.abilities[(CLASS_KENSAI * 10) + 8] * 20) + (p_ptr.stat_ind[A_WIS+1] * 2)
				mpower = (monster(m_idx).level + monster(m_idx).mind)
			else
				ppower = p_ptr.abilities[(CLASS_FIGHTER * 10) + 3] * 20
				mpower = (monster(m_idx).level + monster(m_idx).mind)
				chabonus = (p_ptr.stat_ind[A_CHR+1] - 5) * 5

				if (chabonus < 0) then chabonus = 0 end
				if (chabonus > (ppower * 2)) then chabonus = ppower end

				ppower = ppower + chabonus
			end

			if (lua_randint(ppower) >= lua_randint(mpower)) then

                                if (get_monster_ability(monster(m_idx), TAUNTED)) then

                                        msg_print(string.format('%s has already been taunted.', m_name))
                                else
					monster(m_idx).hitrate = monster(m_idx).hitrate - monster(m_idx).hitrate / 3
					monster(m_idx).defense = monster(m_idx).defense - monster(m_idx).defense / 3
					monster(m_idx).mspeed = monster(m_idx).mspeed + 3
					if (monster(m_idx).mspeed > 180) then monster(m_idx).mspeed = 180 end
					give_monster_ability(monster(m_idx), TAUNTED)
                                        msg_print(string.format('%s has been taunted!', m_name))
                                end
			else
				msg_print(string.format('%s resists.', m_name))
			end

			-- No damages.
			dam = 0
		end

		-- Elemental Lord ability: Piercing Spells
		if (element == GF_VULNERABILITY) then

			ppower = p_ptr.abilities[(CLASS_ELEM_LORD * 10) + 5] * 15
			mpower = (monster(m_idx).level + monster(m_idx).mind)

			-- Monsters
			if (p_ptr.abilities[(CLASS_MONSTER * 10) + 4] > 0) then ppower = ppower + (p_ptr.abilities[(CLASS_MONSTER * 10) + 4] * 15) end

			if (m_race(monster(m_idx).r_idx).resistances[p_ptr.elemlord+1] < 0) then
				msg_print("This monster is already vulnerable to this element!")
			else

				if (lua_randint(ppower) >= lua_randint(mpower)) then

                                	if (get_monster_ability(monster(m_idx), PIERCING_SPELLS)) then

                                        	msg_print(string.format('%s has already been affected.', m_name))
                                	else
						give_monster_ability(monster(m_idx), PIERCING_SPELLS)
                                        	msg_print(string.format('%s becomes vulnerable to your element!', m_name))
                                	end
				else
					msg_print(string.format('%s resists.', m_name))
				end
			end

			-- No damages.
			dam = 0
		end

		-- Paladin ability: Smite Evil.
		if (element == GF_SMITE_EVIL) then

                        if (not(get_monster_flag3(monster(m_idx).r_idx, RF3_EVIL))) then

                                msg_print(string.format('%s is unaffected.', m_name))
                                dam = 0
                        else
 
				
				if (monster(m_idx).level <= (p_ptr.abilities[(CLASS_PALADIN * 10) + 4] * 3) and not(get_monster_flag1(monster(m_idx).r_idx, RF1_UNIQUE))) then

                                	monster(m_idx).monfear = 10
                                        msg_print(string.format('%s becomes scared!', m_name))
                                        update_and_handle()
                                end
                        end

		end
                
                -- Shadow Stalker ability: Stealth Attack
		if (element == GF_STEALTH_ATTACK) then

			if (get_monster_flag2(monster(m_idx).r_idx, RF2_INVISIBLE)) then

                                msg_print(string.format('%s is unaffected.', m_name))
                                dam = 0
                        end
		end

		-- Monster Mage ability: Dominate Monster
		if (element == GF_DOMINATE_MONSTER) then

			ppower = p_ptr.abilities[(CLASS_MONSTER_MAGE * 10) + 6] * 10
			mpower = (monster(m_idx).level + monster(m_idx).mind)

			-- Uniques, Elites and Bosses are immune.
                        if ((get_monster_flag1(monster(m_idx).r_idx, RF1_UNIQUE)) or monster(m_idx).boss > 0 or m_race(monster(m_idx).r_idx).cursed > 0) then

				msg_print(string.format('%s cannot be dominated.', m_name))

                        else
				if (monster(m_idx).lives > 0) then

					msg_print("You must first reduce extra lives to 0.")
				else
                                	if (lua_randint(ppower) >= lua_randint(mpower)) then

                                        	msg_print(string.format('%s is now under your control!', m_name))
                                        	set_pet(monster(m_idx), TRUE)
                                	else

                                        	msg_print(string.format('%s resists.', m_name))
                                	end
				end
                                
			end

			-- No damages.
			dam = 0
		end

		-- Justice Warrior ability: Shatter Evil
		if (element == GF_SHATTER_EVIL) then

			ppower = p_ptr.abilities[(CLASS_JUSTICE_WARRIOR * 10) + 1] * 10
			mpower = (monster(m_idx).level + monster(m_idx).mind)

                        if (not(get_monster_flag3(monster(m_idx).r_idx, RF3_EVIL))) then

				dam = 0
				msg_print(string.format('%s is immune.', m_name))

                        else

				if ((lua_randint(ppower) * 2) >= lua_randint(mpower)) then

                                	if ((get_monster_flag3(monster(m_idx).r_idx, RF3_DEMON) or get_monster_flag3(monster(m_idx).r_idx, RF3_UNDEAD)) and not(get_monster_flag1(monster(m_idx).r_idx, RF1_UNIQUE)) and monster(m_idx).boss < 1) then

                                        	monster(m_idx).monfear = 10
						msg_print(string.format('%s becomes scared!', m_name))
						update_and_handle()
                                	end
				end
                        end

		end

		-- Justice Warrior ability: Angelic Voice
		if (element == GF_ANGELIC_VOICE) then

			ppower = p_ptr.abilities[(CLASS_JUSTICE_WARRIOR * 10) + 2] * 10
			mpower = (monster(m_idx).level + monster(m_idx).mind)

                        if ((get_monster_flag1(monster(m_idx).r_idx, RF1_UNIQUE)) or (monster(m_idx).boss >= 1) or m_race(monster(m_idx).r_idx).cursed > 0 or not(get_monster_flag3(monster(m_idx).r_idx, RF3_EVIL))) then

				dam = 0
				msg_print(string.format('%s is immune.', m_name))
                        else

				local chance
                                chance = (lua_randint(ppower) + p_ptr.stat_ind[A_CHR+1])

                                if (chance >= lua_randint(mpower)) then

                                        msg_print(string.format('%s becomes friendly!', m_name))
                                        set_pet(monster(m_idx), TRUE)

                                else

                                        msg_print(string.format('%s resists.', m_name))
                                end      
			end

			-- No damages.
			dam = 0

			-- Not angry.
			anger = 0
		end

		-- Justice Warrior ability: Repulse Evil
		if (element == GF_REPULSE_EVIL) then

			ppower = p_ptr.abilities[(CLASS_JUSTICE_WARRIOR * 10) + 3] * 10
			mpower = (monster(m_idx).level + monster(m_idx).mind)

                        if ((get_monster_flag1(monster(m_idx).r_idx, RF1_UNIQUE)) or (monster(m_idx).boss >= 1) or not(get_monster_flag3(monster(m_idx).r_idx, RF3_EVIL))) then

				dam = 0
				msg_print(string.format('%s is immune.', m_name))

				-- Not angry.
				anger = 0
                        else

				if (lua_randint(ppower) >= lua_randint(mpower)) then

					monster(m_idx).monfear = 5 + (p_ptr.abilities[(CLASS_JUSTICE_WARRIOR * 10) + 3] / 2)
					msg_print(string.format('%s becomes scared!', m_name))
					update_and_handle()
				end
                        end
		end

		-- Justice Warrior ability: Slay Evil
		if (element == GF_SLAY_EVIL) then

			ppower = p_ptr.abilities[(CLASS_JUSTICE_WARRIOR * 10) + 7] * 10
			mpower = (monster(m_idx).level + monster(m_idx).mind)

                        if (not(get_monster_flag3(monster(m_idx).r_idx, RF3_EVIL))) then

				dam = 0
				msg_print(string.format('%s is immune.', m_name))
                        else

                                if (not(get_monster_flag1(monster(m_idx).r_idx, RF1_UNIQUE)) and monster(m_idx).boss < 1) then

                                        if (lua_randint(ppower) >= lua_randint(mpower)) then

						msg_print(string.format('%s is disintegrated by your holy spell!', m_name))
                                                dam = (monster(m_idx).hp + 1)
                                        end
                                end
                        end

		end

		-- Soul Guardian ability: Sealing Light
		if (element == GF_SEAL_LIGHT) then

                        ppower = dam
                        mpower = (monster(m_idx).level + monster(m_idx).mind)

			ppower = ppower + multiply_divide(ppower, (p_ptr.abilities[(CLASS_SOUL_GUARDIAN * 10) + 4] * 20), 100)

			-- Uniques, Elites and Bosses are harder to paralyze.
                        if ((get_monster_flag1(monster(m_idx).r_idx, RF1_UNIQUE)) or monster(m_idx).boss > 0) then

				mpower = mpower * 3
                        end

                        if ((get_monster_flag1(monster(m_idx).r_idx, RF1_QUESTOR))) then

                                msg_print(string.format('%s is immune.', m_name))
                        else

                                if (lua_randint(ppower) >= lua_randint(mpower)) then

                                        msg_print(string.format('%s has been sealed!', m_name))
                                        monster(m_idx).seallight = 10 + (p_ptr.abilities[(CLASS_SOUL_GUARDIAN * 10) + 4] / 2)
                                else

					msg_print(string.format('%s resists.', m_name))
				end
                        end

                        -- No damages.
                        dam = 0
                end

		-- Diviner's Nightmares
		if (element == GF_NIGHTMARES) then

			ppower = (p_ptr.skill[27] / 4) * p_ptr.abilities[(CLASS_DIVINER * 10) + 6]
			mpower = (monster(m_idx).level + monster(m_idx).mind)

			-- Elites and Bosses can resist more easily.
			if (monster(m_idx).boss > 0) then mpower = mpower * 3 end

			-- It does not affect uniques or fear resistants.
                        if ((get_monster_flag1(monster(m_idx).r_idx, RF1_UNIQUE)) or (get_monster_flag3(monster(m_idx).r_idx, RF3_NO_FEAR))) then

				if (get_monster_flag3(monster(m_idx).r_idx, RF3_NO_FEAR)) then

					memorize_race_flag3(monster(m_idx).r_idx, RF3_NO_FEAR)
				end
                                msg_print(string.format('%s is unaffected.', m_name))
                        else
			
				if (lua_randint(ppower) >= lua_randint(mpower)) then

                                	monster(m_idx).monfear = dam
					msg_print(string.format('%s becomes scared!', m_name))

				else
					msg_print(string.format('%s resists.', m_name))
				end
			end

                        -- No damages.
                        dam = 0
		end

		-- Elemental Lord's Command Element
		if (element == GF_COMMAND_ELEMENT) then

			ppower = (m_race(monster(m_idx).r_idx).resistances[p_ptr.elemlord+1] / 4) * p_ptr.abilities[(CLASS_ELEM_LORD * 10) + 7]
			mpower = (monster(m_idx).level + monster(m_idx).mind)

			-- The default "dam" is 100. This ability can theorically be resisted through normal resistances.
			ppower = multiply_divide(ppower, dam, 100)

			-- Elites, Bosses and Uniques are more resistant.
			if ((monster(m_idx).boss > 0) or (get_monster_flag1(monster(m_idx).r_idx, RF1_UNIQUE))) then mpower = mpower * 3 end

			-- If resistance isn't at least 4%, it fails.
                        if ((m_race(monster(m_idx).r_idx).resistances[p_ptr.elemlord+1] < 4) or (ppower <= 0)) then

                                msg_print(string.format('%s is unaffected.', m_name))
                        else
				if (lua_randint(ppower) >= lua_randint(mpower)) then

					-- Depending on event variable 29014, we check what effect we apply.
					-- Note that Dominate doesn't work on Uniques. It will be changed to Paralyze.
					-- Remember, in lua, arrays starts at 1, so we add 1 to 29014, hence 29015.

					-- 1 is Paralyze.
					if (p_ptr.events[29015] == 1) then

						if (get_monster_flag1(monster(m_idx).r_idx, RF1_UNIQUE)) then
							monster(m_idx).seallight = 3
						else
							monster(m_idx).seallight = 10
						end
						msg_print(string.format('%s has been paralyzed!', m_name))
					end

					-- 2 is Dominate.
					if (p_ptr.events[29015] == 2) then

						-- Uniques are paralyzed.
						if (get_monster_flag1(monster(m_idx).r_idx, RF1_UNIQUE) or m_race(monster(m_idx).r_idx).cursed > 0) then
							monster(m_idx).seal_light = 10
							msg_print(string.format('%s has been paralyzed!', m_name))
						else
							if (monster(m_idx).lives > 0) then

								msg_print("You must first reduce extra lives to 0.")
							else
								set_pet(monster(m_idx), TRUE)
								msg_print(string.format('%s is now under your control!', m_name))
								anger = 0
							end
						end
					end

					-- 3 is Sleep.
					if (p_ptr.events[29015] == 3) then

						monster(m_idx).csleep = 500
						msg_print(string.format('%s falls asleep!', m_name))
					end

					-- 4 is Confuse.
					if (p_ptr.events[29015] == 4) then

						if (get_monster_flag1(monster(m_idx).r_idx, RF1_UNIQUE)) then
							monster(m_idx).confused = 3
						else
							monster(m_idx).confused = 10
						end
						msg_print(string.format('%s becomes confused!', m_name))
					end

					-- 5 is Fear.
					if (p_ptr.events[29015] == 5) then

						if (get_monster_flag1(monster(m_idx).r_idx, RF1_UNIQUE)) then
							monster(m_idx).monfear = 3
						else
							monster(m_idx).monfear = 10
						end
						msg_print(string.format('%s becomes scared!', m_name))
					end

				else
					msg_print(string.format('%s resists.', m_name))
				end
			end

                        -- No damages.
                        dam = 0
		end

		-- Monsters: Racial Champion
		if (element == GF_RACIAL_CHAMPION) then

			ppower = p_ptr.abilities[(CLASS_MONSTER * 10) + 9] * 40
			mpower = (monster(m_idx).level + monster(m_idx).mind)

			if (m_race(p_ptr.body_monster).d_char == m_race(monster(m_idx).r_idx).d_char) then

				-- Uniques, Elites and Bosses are immune.
                        	if ((get_monster_flag1(monster(m_idx).r_idx, RF1_UNIQUE)) or monster(m_idx).boss > 0 or m_race(monster(m_idx).r_idx).cursed > 0) then

					msg_print(string.format('%s cannot be charmed.', m_name))

                        	else
                                	if (lua_randint(ppower) >= lua_randint(mpower)) then

                                        	msg_print(string.format('%s joins your ranks!', m_name))
                                        	set_pet(monster(m_idx), TRUE)
                                	else

                                        	msg_print(string.format('%s resists.', m_name))
                                	end                               
				end
			else
				msg_print(string.format('%s is unaffected.', m_name))
			end

			-- No damages.
			dam = 0

			-- Not angry.
			anger = 0
		end

		-- Alteration: Paralyze
		if (element == GF_PARALYZE) then

			ppower = (p_ptr.stat_ind[A_INT+1] + p_ptr.stat_ind[A_WIS+1] + (p_ptr.skill[24] * 3) + p_ptr.skill[2])
			mpower = (monster(m_idx).level + monster(m_idx).mind)

			-- Monster race can get some bonus.
			if (p_ptr.prace == RACE_MONSTER) then ppower = ppower + p_ptr.events[29018] end

                        -- Uniques, Elites and Bosses are harder to paralyze.
                        if ((get_monster_flag1(monster(m_idx).r_idx, RF1_UNIQUE)) or monster(m_idx).boss > 0) then

				mpower = mpower * 3
                        end

			if ((get_monster_flag1(monster(m_idx).r_idx, RF1_QUESTOR)) or (get_monster_flag3(monster(m_idx).r_idx, RF3_NO_STUN))) then

				
				if (get_monster_flag3(monster(m_idx).r_idx, RF3_NO_STUN)) then

					memorize_race_flag3(monster(m_idx).r_idx, RF3_NO_STUN)
				end
                                msg_print(string.format('%s is immune.', m_name))
                        else

                                if (lua_randint(ppower) >= lua_randint(mpower)) then

                                        msg_print(string.format('%s has been paralyzed!', m_name))

					-- It actually uses the same variable as Sealing Light. Sealing Light was made before
					-- Paralyze, that's why... No need for two variables.
                                        monster(m_idx).seallight = dam
                                else

					msg_print(string.format('%s resists.', m_name))
				end
                        end

                        -- No damages.
                        dam = 0
                end
                
		-- Ranger Ability: Sleep Pollen
		if (element == GF_SLEEP_POLLEN) then

			ppower = p_ptr.abilities[(CLASS_RANGER * 10) + 9] * 20
                        mpower = (monster(m_idx).level + monster(m_idx).mind)

                        -- It does not affect uniques, elites or bosses.
                        if (get_monster_flag3(monster(m_idx).r_idx, RF3_NO_SLEEP)) then

				if (get_monster_flag3(monster(m_idx).r_idx, RF3_NO_SLEEP)) then

					memorize_race_flag3(monster(m_idx).r_idx, RF3_NO_SLEEP)
				end
                                msg_print(string.format('%s is unaffected.', m_name))
			else

				if (lua_randint(ppower) >= lua_randint(mpower)) then

					-- Go to sleep.
					monster(m_idx).csleep = 500
					msg_print(string.format('%s falls asleep.', m_name))
				end
			end

			-- No damages.
			dam = 0
		end

                -- Rogue ability: Gas Trap
		if (element == GF_SLEEP_GAS) then

			ppower = p_ptr.abilities[(CLASS_ROGUE * 10) + 7] * 10
                        mpower = (monster(m_idx).level + monster(m_idx).str)

                        -- It does not affect uniques nor bosses. But elites can be affected.
                        if ((get_monster_flag1(monster(m_idx).r_idx, RF1_UNIQUE)) or (get_monster_flag3(monster(m_idx).r_idx, RF3_NO_SLEEP)) or monster(m_idx).boss < 2) then

				if (get_monster_flag3(monster(m_idx).r_idx, RF3_NO_SLEEP)) then

					memorize_race_flag3(monster(m_idx).r_idx, RF3_NO_SLEEP)
				end
                                msg_print(string.format('%s is unaffected.', m_name))
			else

				if (lua_randint(ppower) >= lua_randint(mpower)) then

					-- Go to sleep.
					monster(m_idx).csleep = 500
					msg_print(string.format('%s falls asleep.', m_name))
				end
			end

			-- No damages.
			dam = 0
		end

		-- Alteration: War Blessing
		-- hmmm...perhaps this one would fit better in the Healing spells, rather than Alteration...
		if (element == GF_WAR_BLESSING) then

                        -- Only boost pets...and only once!
                        if (is_pet(monster(m_idx))) then

				if (get_monster_ability(monster(m_idx), WAR_BLESSED)) then

                                	msg_print(string.format('%s has already been blessed.', m_name))
                                else

                                        -- Boost stats.
                                        monster(m_idx).hitrate = monster(m_idx).hitrate * 2
                                        monster(m_idx).defense = monster(m_idx).defense * 2
                                        give_monster_ability(monster(m_idx), WAR_BLESSED)
					msg_print(string.format('%s has been blessed!', m_name))
                                end
			end

			-- No damages.
			dam = 0

			-- Not angry.
			anger = 0
		end

		-- Alteration: Demoralize
		if (element == GF_FEAR_CURSE) then

			ppower = (p_ptr.stat_ind[A_INT+1] + p_ptr.stat_ind[A_WIS+1] + (p_ptr.skill[24] * 3) + p_ptr.skill[2])
			mpower = (monster(m_idx).level + monster(m_idx).mind)

			-- Monster race can get some bonus.
			if (p_ptr.prace == RACE_MONSTER) then ppower = ppower + p_ptr.events[29018] end

			-- It does not affect uniques, elites, bosses or fear resistants.
                        if ((get_monster_flag1(monster(m_idx).r_idx, RF1_UNIQUE)) or (get_monster_flag3(monster(m_idx).r_idx, RF3_NO_FEAR)) or monster(m_idx).boss > 0) then

				if (get_monster_flag3(monster(m_idx).r_idx, RF3_NO_FEAR)) then

					memorize_race_flag3(monster(m_idx).r_idx, RF3_NO_FEAR)
				end
                                msg_print(string.format('%s is unaffected.', m_name))
                        else
			
				if (lua_randint(ppower) >= lua_randint(mpower)) then

                                	monster(m_idx).monfear = dam
					msg_print(string.format('%s becomes scared!', m_name))

				else
					msg_print(string.format('%s resists.', m_name))
				end
			end

                        -- No damages.
                        dam = 0
		end

		-- Warrior ability: War Cry.
		if (element == GF_WARCRY) then

			if (lionroar == 1) then
				ppower = (p_ptr.abilities[(CLASS_KENSAI * 10) + 7] * 15) + (p_ptr.stat_ind[A_WIS+1] * 2)
			else
				ppower = p_ptr.abilities[(CLASS_WARRIOR * 10) + 7] * 30
			end
			mpower = (monster(m_idx).level + monster(m_idx).mind)

			-- It does not affect uniques, elites, bosses or fear resistants.
                        if ((get_monster_flag1(monster(m_idx).r_idx, RF1_UNIQUE)) or (get_monster_flag3(monster(m_idx).r_idx, RF3_NO_FEAR))) then

				if (get_monster_flag3(monster(m_idx).r_idx, RF3_NO_FEAR)) then

					memorize_race_flag3(monster(m_idx).r_idx, RF3_NO_FEAR)
				end
                                msg_print(string.format('%s is unaffected.', m_name))
                        else
			
				if (lua_randint(ppower) >= lua_randint(mpower)) then

                                	monster(m_idx).monfear = dam
					msg_print(string.format('%s becomes scared!', m_name))

				else
					msg_print(string.format('%s resists.', m_name))
				end
			end

                        -- No damages.
                        dam = 0
		end

                -- Alteration: Reduce defense.
		if (element == GF_REDUCE_DEF) then

			ppower = (p_ptr.stat_ind[A_INT+1] + p_ptr.stat_ind[A_WIS+1] + (p_ptr.skill[24] * 3) + p_ptr.skill[2])
			mpower = (monster(m_idx).level + monster(m_idx).mind)

                        -- Reduce defense
			if (lua_randint(ppower) >= lua_randint(mpower)) then

				monster(m_idx).defense = monster(m_idx).defense - dam
				if (monster(m_idx).defense < 0) then

					monster(m_idx).defense = 0
					msg_print(string.format('%s defense cannot be reduced further.', m_name))
				else
                        		msg_print(string.format('%s has lost defense!', m_name))
				end
			else
				msg_print(string.format('%s resists.', m_name))
			end

			-- No damages.
                        dam = 0
		end

                -- Alteration: Reduce hit rate.
		if (element == GF_REDUCE_HIT) then

			ppower = (p_ptr.stat_ind[A_INT+1] + p_ptr.stat_ind[A_WIS+1] + (p_ptr.skill[24] * 3) + p_ptr.skill[2])
			mpower = (monster(m_idx).level + monster(m_idx).mind)

                        -- Reduce hit rate
			if (lua_randint(ppower) >= lua_randint(mpower)) then

				monster(m_idx).hitrate = monster(m_idx).hitrate - dam
				if (monster(m_idx).hitrate < 0) then

					monster(m_idx).hitrate = 0
					msg_print(string.format('%s hit rate cannot be reduced further.', m_name))
				else
                        		msg_print(string.format('%s has lost hit rate!', m_name))
				end
			else
				msg_print(string.format('%s resists.', m_name))
			end

			-- No damages.
                        dam = 0
		end

		-- Alteration: Reduce Speed
		if (element == GF_REDUCE_SPEED) then

			ppower = (p_ptr.stat_ind[A_INT+1] + p_ptr.stat_ind[A_WIS+1] + (p_ptr.skill[24] * 3) + p_ptr.skill[2])
			mpower = (monster(m_idx).level + monster(m_idx).mind)


			-- Does not affect Uniques.
			if (get_monster_flag1(monster(m_idx).r_idx, RF1_UNIQUE)) then

				msg_print(string.format('%s cannot be affected.', m_name))
			else
                        	-- Reduce speed
				if (lua_randint(ppower) >= lua_randint(mpower)) then

					monster(m_idx).mspeed = monster(m_idx).mspeed - dam
					if (monster(m_idx).mspeed < 0) then

						monster(m_idx).mspeed = 0
						msg_print(string.format('%s speed cannot be reduced further.', m_name))
					else
                        			msg_print(string.format('%s is moving slower!', m_name))
					end
				else
					msg_print(string.format('%s resists.', m_name))
				end
			end

			-- No damages.
                        dam = 0
		end

		-- Morale Boost.
		if (element == GF_MORALE_BOOST) then

                        -- Only boost pets...and only once!
                        if (is_pet(monster(m_idx))) then

				if (get_monster_ability(monster(m_idx), MORALE_BOOST)) then

                                	msg_print(string.format('%s has already been boosted.', m_name))
                                else

                                        -- Boost stats.
                                        monster(m_idx).hitrate = monster(m_idx).hitrate + (monster(m_idx).hitrate / 2)
                                        monster(m_idx).mspeed = monster(m_idx).mspeed + (monster(m_idx).mspeed / 4)
                                        give_monster_ability(monster(m_idx), MORALE_BOOST)
					msg_print(string.format('%s is now ready for battle!', m_name))
                                end

			end

			-- No damages.
			dam = 0

			-- Not angry.
			anger = 0
		end

		-- Paladin ability: Aura of Life.
		if (element == GF_AURA_LIFE) then
                        
			-- Heal friendly monsters
			-- Will actually heal friendly undeads!
                        if (is_pet(monster(m_idx))) then

                                monster(m_idx).hp = monster(m_idx).hp + dam
                                if (monster(m_idx).hp > monster(m_idx).maxhp) then monster(m_idx).hp = monster(m_idx).maxhp end
                                dam = 0

				-- Not angry.
				anger = 0
                        else
				if (not(get_monster_flag3(monster(m_idx).r_idx, RF3_UNDEAD))) then dam = 0 end
			end

		end


		-- Alteration: Evolve
		if (element == GF_EVOLVE) then

			ppower = (p_ptr.stat_ind[A_INT+1] + p_ptr.stat_ind[A_WIS+1] + (p_ptr.skill[24] * 3) + p_ptr.skill[2])
			mpower = (monster(m_idx).level + monster(m_idx).mind)

                        -- Evolve non-unique monsters.
                        if (not(get_monster_flag1(monster(m_idx).r_idx, RF1_UNIQUE)) and not(m_race(monster(m_idx).r_idx).cursed > 0)) then

				if ((lua_randint(ppower) >= lua_randint(mpower)) or is_pet(monster(m_idx))) then

                                	do_cmd_evolve_monster(monster(m_idx))
				else
					msg_print(string.format('%s resists.', m_name))
				end

                        else
				msg_print(string.format('%s cannot be evolved.', m_name))
			end

			-- No damages.
                        dam = 0

			-- Not angry.
			anger = 0
		end

            	-- Alteration: Un-Evolve
		if (element == GF_UNEVOLVE) then

			ppower = (p_ptr.stat_ind[A_INT+1] + p_ptr.stat_ind[A_WIS+1] + (p_ptr.skill[24] * 3) + p_ptr.skill[2])
			mpower = (monster(m_idx).level + monster(m_idx).mind)

                        -- Un-Evolve non-unique, non-bosses monsters.
                        if (not(get_monster_flag1(monster(m_idx).r_idx, RF1_UNIQUE)) and not(m_race(monster(m_idx).r_idx).cursed > 0) and monster(m_idx).boss < 1) then

				if ((lua_randint(ppower) >= lua_randint(mpower)) or is_pet(monster(m_idx))) then

                                	do_cmd_unevolve_monster(monster(m_idx))
				else
					msg_print(string.format('%s resists.', m_name))
				end

                        else
				msg_print(string.format('%s cannot be un-evolved.', m_name))
			end

			-- No damages.
                        dam = 0
		end

		-- Warp!
		if (element == GF_WARP) then

			-- Elemental is used here, not Alteration.
			if (music == 1) then
				ppower = (p_ptr.stat_ind[A_CHR+1] + (p_ptr.skill[23] * 3) + p_ptr.skill[2])
				if (p_ptr.abilities[(CLASS_BARD * 10) + 1] > 0) then

					ppower = ppower + multiply_divide(ppower, p_ptr.abilities[(CLASS_BARD * 10) + 1] * 3, 100)
				end
			else ppower = (p_ptr.stat_ind[A_INT+1] + p_ptr.stat_ind[A_WIS+1] + (p_ptr.skill[23] * 3) + p_ptr.skill[2]) end
			mpower = (monster(m_idx).level + monster(m_idx).mind)

			-- Monster race can get some bonus.
			if (p_ptr.prace == RACE_MONSTER) then ppower = ppower + p_ptr.events[29018] end

			-- Elites and Bosses can be teleported, but it's harder.
			if (monster(m_idx).boss > 0) then mpower = mpower * 3 end

			-- Uniques cannot be teleported.
			if (not(get_monster_flag1(monster(m_idx).r_idx, RF1_UNIQUE))) then
				
                                if (lua_randint(ppower) >= lua_randint(mpower)) then

                                        teleport_away(m_idx, 20)
                                end
				
			end
		end


		-- Priest's ability: Turn Undead
		if (element == GF_TURN_UNDEAD) then

			-- Must be undead, obviously.
                        if (get_monster_flag3(monster(m_idx).r_idx, RF3_UNDEAD)) then

				if (not(get_monster_flag1(monster(m_idx).r_idx, RF1_UNIQUE)) and m_race(monster(m_idx).r_idx).cursed == 0) then

					local ppow
					local mpow

					ppower = p_ptr.abilities[(CLASS_PRIEST * 10) + 2] * 20
					mpower = (monster(m_idx).level + monster(m_idx).mind)

					ppow = lua_randint(ppower)
					mpow = lua_randint(mpower)

					if (ppow >= mpow) then

						if (ppow > (mpow * 2)) then

							msg_print(string.format('%s has been turned!', m_name))
							set_pet(monster(m_idx), TRUE)
							dam = 0

						else
							dam = monster(m_idx).hp + 1
						end
					end     
                                end
                        else

				msg_print(string.format('%s is unaffected.', m_name))
				dam = 0

				-- Not angry.
				anger = 0
			end
		end

		-- Ranger ability: Animal Empathy!
		if (element == GF_ANIMAL_EMPATHY) then

			ppower = p_ptr.abilities[(CLASS_RANGER * 10) + 4] * 50
			mpower = (monster(m_idx).level + monster(m_idx).mind)

                        -- Only affect animals
                        if (get_monster_flag3(monster(m_idx).r_idx, RF3_ANIMAL)) then

                                if (not(get_monster_flag1(monster(m_idx).r_idx, RF1_UNIQUE)) and monster(m_idx).boss == 0 and m_race(monster(m_idx).r_idx).cursed == 0) then

                                        if (lua_randint(ppower) >= lua_randint(mpower)) then

                                                msg_print(string.format('%s has been tamed.', m_name))
                                                set_pet(monster(m_idx), TRUE)

                                        else
						msg_print(string.format('%s resists.', m_name))
					end
                                else

					if (lua_randint(ppower) >= lua_randint(mpower)) then

                                                msg_print(string.format('%s starts fleeing from you!', m_name))
                                                monster(m_idx).monfear = 15

                                        else
						msg_print(string.format('%s resists.', m_name))
					end
				end

                        else
				msg_print(string.format('%s is unaffected.', m_name))
			end

                        -- No damage
                        dam = 0

			-- Not angry.
			anger = 0
		end

		-- Soul Crush
		-- Andraos' spell. Reduces mind to 1 if successful and may also confuse.
		if (element == GF_SOUL_CRUSH) then

			ppower = (p_ptr.stat_ind[A_INT+1] + p_ptr.stat_ind[A_WIS+1])
			mpower = (monster(m_idx).level + monster(m_idx).mind)

			-- Monster race can get some bonus.
			if (p_ptr.prace == RACE_MONSTER) then ppower = ppower + p_ptr.events[29018] end

			-- Uniques and Nightmares aren't affected.
			-- Empty Mind enemies are also immune.
			if (not(get_monster_flag1(monster(m_idx).r_idx, RF1_UNIQUE)) and m_race(monster(m_idx).r_idx).cursed == 0) then

				if (lua_randint(ppower) >= lua_randint(mpower)) then

					if (not(get_monster_flag3(monster(m_idx).r_idx, RF3_NO_CONF))) then
						monster(m_idx).confused = 10
					end
					monster(m_idx).mind = 1
                                        msg_print(string.format('%s soul has been crushed!', m_name))

				else
					msg_print(string.format('%s resists.', m_name))
				end
			else
				msg_print(string.format('%s cannot be affected.', m_name))
			end

			-- No damages.
			dam = 0
		end

		-- Duration.
		-- This sets the duration of summoned monsters.
		if (element == GF_DURATION) then

			if (music == 1) then
				ppower = (p_ptr.stat_ind[A_CHR+1])
				if (p_ptr.abilities[(CLASS_BARD * 10) + 1] > 0) then

					ppower = ppower + multiply_divide(ppower, p_ptr.abilities[(CLASS_BARD * 10) + 1] * 3, 100)
				end
			else ppower = (p_ptr.stat_ind[A_INT+1] + p_ptr.stat_ind[A_WIS+1]) end
			mpower = (monster(m_idx).level + monster(m_idx).mind)

			-- Monster must be a summoned monster.
			-- It does not work on normal enemies.
			if (monster(m_idx).summoned > 0) then return end

			-- Enemy summons will require a saving throw.
			if (not(is_pet(monster(m_idx)))) then

				if (lua_randint(ppower) >= lua_randint(mpower)) then

					if (dam == 0) then

						dam = monster(m_idx).hp + 1
						monster(m_idx).lives = 0
                                		msg_print(string.format('%s disappears!', m_name))
					else
						monster(m_idx).summoned = dam
						dam = 0
					end
				end
			else
				monster(m_idx).summoned = dam
				dam = 0
			end

			-- Update
			update_and_handle()
			
			-- Not angry.
			anger = 0
		end

		-- Inspire Courage.
		if (element == GF_INSPIRE_COURAGE) then

                        -- Only boost pets...and only once!
                        if (is_pet(monster(m_idx))) then

				if (not(get_monster_ability(monster(m_idx), MORALE_BOOST))) then

                                        -- Boost stats.
					monster(m_idx).level = monster(m_idx).level + (2 * p_ptr.abilities[(CLASS_BARD * 10) + 3])
					apply_monster_level_hp(monster(m_idx))
					monster(m_idx).str = monster(m_idx).str + multiply_divide(monster(m_idx).str, p_ptr.abilities[(CLASS_BARD * 10) + 3] * 3, 100)
					monster(m_idx).dex = monster(m_idx).dex + multiply_divide(monster(m_idx).dex, p_ptr.abilities[(CLASS_BARD * 10) + 3] * 3, 100)
					monster(m_idx).mind = monster(m_idx).mind + multiply_divide(monster(m_idx).mind, p_ptr.abilities[(CLASS_BARD * 10) + 3] * 3, 100)
					monster(m_idx).skill_attack = monster(m_idx).skill_attack + multiply_divide(monster(m_idx).skill_attack, p_ptr.abilities[(CLASS_BARD * 10) + 3] * 3, 100)
					monster(m_idx).skill_ranged = monster(m_idx).skill_ranged + multiply_divide(monster(m_idx).skill_ranged, p_ptr.abilities[(CLASS_BARD * 10) + 3] * 3, 100)
					monster(m_idx).skill_magic = monster(m_idx).skill_magic + multiply_divide(monster(m_idx).skill_magic, p_ptr.abilities[(CLASS_BARD * 10) + 3] * 3, 100)
					-- Apply again for hit rate/mana.
					apply_monster_level_hp(monster(m_idx))

					if (monster(m_idx).summoned > 0) then monster(m_idx).summoned = monster(m_idx).summoned + (3 * p_ptr.abilities[(CLASS_BARD * 10) + 3]) end
                                        
                                        give_monster_ability(monster(m_idx), MORALE_BOOST)
					msg_print(string.format('%s is inspired to fight!', m_name))
                                end

			end

			-- No damages.
			dam = 0

			-- Not angry.
			anger = 0
		end

		-- Restore Stats, Status, Levels.
		-- Does nothing on monsters.
		if (element == GF_RESTORE_STATS or element == GF_RESTORE_STATUS or element == GF_RESTORE_LEVELS or element == GF_RESTORATION
		or element == GF_CURE_MUTATIONS or element == GF_HEROISM or element == GF_RESTORE_MANA) then

			-- No damages.
			dam = 0

			-- Not angry.
			anger = 0
		end

		-- Strength potions do work on monsters.
		if (element == GF_STRENGTH or element == GF_STAR_HEROISM) then

			if (monster(m_idx).boosted == 0) then
                        	msg_print(string.format('%s is stronger!', m_name))
				monster(m_idx).str = monster(m_idx).str + dam
				monster(m_idx).boosted = 1
			end

			anger = 0
			dam = 0
		end

		-- Dispel friendly monster
		if (element == GF_UNSUMMON) then

                        if (is_pet(monster(m_idx))) then

				if ((get_monster_flag7(monster(m_idx).r_idx, RF7_TOWNSFOLK)) or (get_monster_flag7(monster(m_idx).r_idx, RF7_GUARD))) then

					msg_print("You can't use this on citizens or town guards!")
					dam = 0
					-- Not angry.
					anger = 0
				else

                                	dam = monster(m_idx).hp + 1

                                	msg_print(string.format('%s disappears!', m_name))
				end
                        end
		end

		-- Does it anger friends?
		if (anger == 1 and is_pet(monster(m_idx)) and monster(m_idx).summoned == 0) then

			msg_print(string.format('%s gets angry!', m_name))
			set_pet(monster(m_idx), FALSE)
			monster(m_idx).angered_pet = 1
		end

		-- Bard's Enthralling Songs.
		if (music == 1 and p_ptr.events[29044] == 1 and p_ptr.abilities[(CLASS_BARD * 10) + 9] > 0 and dam > 0) then

			ppower = p_ptr.abilities[(CLASS_BARD * 10) + 9] * 10
			mpower = monster(m_idx).level + monster(m_idx).mind

			if (monster(m_idx).boss == 0 and not(get_monster_flag1(monster(m_idx).r_idx, RF1_UNIQUE)) and monster(m_idx).cursed == 0) then

				if (lua_randint(ppower) >= lua_randint(mpower)) then

					msg_print(string.format('%s has been enthralled!', m_name))
					set_pet(monster(m_idx), TRUE)
				end
			end
		end

	end

	-- Note resistance(if any)
	m_race(monster(m_idx).r_idx).r_resist[element+1] = 1

	-- Display total damages.
	c_put_str(TERM_L_GREEN, string.format('Dam:                           ', dam), 23, 64)
	c_put_str(TERM_L_GREEN, string.format('Dam: %d', dam), 23, 64)
	damages_counter = dam
	damages_counter_player_damages = TRUE
	damages_counter_duration = 3

	-- Return damages dealt.
	return dam

end

-- Player is hit by element
-- Radius is used here for the Reflecting ability, which is harder if there's a radius.
function element_hit_player (who, element, dam, rad)

	local m_name

	-- Just in case... element 0 defaults to Physical.
	if (element == 0) then element = GF_PHYSICAL end

	if (not(who == -2)) then

		-- Get the monster's name
		m_name = m_race(monster(who).r_idx).name_char

		-- Elites/Bosses may cause double damages with spells...
        	if (get_monster_ability(monster(who), BOSS_DOUBLE_MAGIC) and not(monster_physical) and not(monster_ranged)) then

			dam = dam * 2
		end

		-- High Mage's reflect magic!
		if ((p_ptr.abilities[(CLASS_HIGH_MAGE * 10) + 3] > 0) and not(monster_physical) and not(monster_ranged)) then

        		local i
			local ppower
			local mpower

			ppower = p_ptr.abilities[(CLASS_HIGH_MAGE * 10) + 3] * 20
			mpower = (monster(who).level + monster(who).mind)

			-- Should not be.
			if (mpower < 0) then mpower = 0 end

			if (lua_randint(ppower) >= lua_randint(mpower)) then

				msg_print("You reflect the spell!")

				-- The "flg" parameter for "project" doesn't really apply to lua, so we must use an alternate function.
				no_magic_return = TRUE
				lua_project(0, rad, monster(who).fy, monster(who).fx, dam * p_ptr.abilities[(CLASS_HIGH_MAGE * 10) + 3], element, 1)
				no_magic_return = FALSE

				disturb(1, 0)
				return
			end
		end

		-- Reflect ability
		if ((p_ptr.reflect) and not(monster_physical) and not(monster_ranged)) then

        		local i
			local ppower
			local mpower

			ppower = 0
			mpower = 0

        		i = 24
        		while (i < 65) do

                		-- Check for reflecting ability.
				if (inven(i).reflect > 0) then
                	
                        		ppower = ppower + inven(i).reflect
                		end

                		i = i + 1
        		end

			if (ppower < 0) then ppower = 0 end

			mpower = (monster(who).level + monster(who).mind) * (rad + 1)
			if (mpower < 0) then mpower = 0 end

			if (lua_randint(ppower) >= lua_randint(mpower)) then

				msg_print("The attack bounces!")

				-- The "flg" parameter for "project" doesn't really apply to lua, so we must use an alternate function.
				no_magic_return = TRUE
				lua_project(0, rad, monster(who).fy, monster(who).fx, dam, element, 1)
				no_magic_return = FALSE

				disturb(1, 0)
				return
			end
		end

		-- Monsters: counters 7 and 9 returns melee.
		if (p_ptr.prace == RACE_MONSTER and dam > 0 and (monster_physical)) then

			if ((m_race(p_ptr.body_monster).countertype == 7 or m_race(p_ptr.body_monster).countertype == 9) and lua_randint(100) <= m_race(p_ptr.body_monster).counterchance) then

				msg_print(string.format('%s takes a hit from the attack!', m_name))

				-- The "flg" parameter for "project" doesn't really apply to lua, so we must use an alternate function.
				no_magic_return = TRUE
				melee_attack = TRUE
				nevermiss = TRUE
				lua_project(0, rad, monster(who).fy, monster(who).fx, dam * (p_ptr.abilities[(CLASS_MONSTER * 10) + 7] + 1), element, 1)
				no_magic_return = FALSE
				melee_attack = FALSE
				nevermiss = FALSE
			end
		end

		-- Monsters: counter 8 and 9 returns magic.
		if (p_ptr.prace == RACE_MONSTER and dam > 0 and not(monster_physical) and not(monster_ranged)) then

			if ((m_race(p_ptr.body_monster).countertype == 8 or m_race(p_ptr.body_monster).countertype == 9) and lua_randint(100) <= m_race(p_ptr.body_monster).counterchance) then

				msg_print(string.format('%s takes a hit from the spell!', m_name))

				no_magic_return = TRUE
				lua_project(0, rad, monster(who).fy, monster(who).fx, dam * (p_ptr.abilities[(CLASS_MONSTER * 10) + 7] + 1), element, 1)
				no_magic_return = FALSE
			end
		end

		-- Monsters: Elite ability that returns damages.
		if (p_ptr.prace == RACE_MONSTER and (get_player_monster_ability(BOSS_RETURNING)) and dam > 0 and (monster_physical)) then

			msg_print(string.format('%s takes a hit from the attack!', m_name))

			-- The "flg" parameter for "project" doesn't really apply to lua, so we must use an alternate function.
			no_magic_return = TRUE
			melee_attack = TRUE
			nevermiss = TRUE
			lua_project(0, rad, monster(who).fy, monster(who).fx, (dam / 2) * (p_ptr.abilities[(CLASS_MONSTER * 10) + 10] + 1), element, 1)
			no_magic_return = FALSE
			melee_attack = FALSE
			nevermiss = FALSE
		end

		-- Monsters: Elite ability that returns magic.
		if (p_ptr.prace == RACE_MONSTER and (get_player_monster_ability(BOSS_MAGIC_RETURNING)) and dam > 0 and not(monster_physical) and not(monster_ranged)) then

			msg_print(string.format('%s takes a hit from the spell!', m_name))

			no_magic_return = TRUE
			lua_project(0, rad, monster(who).fy, monster(who).fx, (dam / 2) * (p_ptr.abilities[(CLASS_MONSTER * 10) + 10] + 1), element, 1)
			no_magic_return = FALSE
		end

		-- Can you block magic attacks?
		if ((p_ptr.abilities[(CLASS_DEFENDER * 10) + 4] >= 10) and (shield_has()) and not(monster_physical) and not(monster_ranged)) then

			local baseblock = 0
			local proll = 0
			local mroll = 0
			local x

			for x = 0, 1 do

				if (inven(INVEN_WIELD + x).tval == TV_SHIELD) then

					baseblock = baseblock + ((inven(INVEN_WIELD + x).ac + (p_ptr.abilities[(CLASS_DEFENDER * 10) + 4] * 5)) * 3)
					proll = proll + p_ptr.stat_ind[A_STR+1] + p_ptr.stat_ind[A_DEX+1]
				end
			end

			if (baseblock > 0) then

				proll = baseblock + p_ptr.stat_ind[A_STR+1] + p_ptr.stat_ind[A_DEX+1]
				mroll = monster(who).mind * 2

				if (lua_randint(proll) >= lua_randint(mroll)) then

                        		msg_print("You block the magic attack!")
                        		dam = 0
                		end
			end
		end

		-- Monsters: counters 2, 3, 11, 12, 18 and 19 can block magic.
		if (p_ptr.prace == RACE_MONSTER and not(monster_physical) and not(monster_ranged)) then

			if ((m_race(p_ptr.body_monster).countertype == 2 or m_race(p_ptr.body_monster).countertype == 3 or m_race(p_ptr.body_monster).countertype == 11 or m_race(p_ptr.body_monster).countertype == 12 or m_race(p_ptr.body_monster).countertype == 18 or m_race(p_ptr.body_monster).countertype == 19) and lua_randint(100) <= m_race(p_ptr.body_monster).counterchance) then

				local proll
				local mroll

				proll = ((p_ptr.abilities[(CLASS_MONSTER * 10) + 7] * 5) * 3) + ((p_ptr.stat_ind[A_INT+1] + p_ptr.stat_ind[A_WIS+1]) / 2)
				mroll = monster(who).mind

				if (lua_randint(proll) >= lua_randint(mroll)) then

                        		msg_print("You block the magic attack!")

					-- If it was counters 11 or 12, also return the attack.
					if (m_race(p_ptr.body_monster).countertype == 11 or m_race(p_ptr.body_monster).countertype == 12) then

						msg_print("The attack bounces!")

						-- The "flg" parameter for "project" doesn't really apply to lua, so we must use an alternate function.
						no_magic_return = TRUE
						lua_project(0, rad, monster(who).fy, monster(who).fx, dam * (p_ptr.abilities[(CLASS_MONSTER * 10) + 7] + 1), element, 1)
						no_magic_return = FALSE

					end

                        		dam = 0
                		end
			end
		end

		-- Monsters: counters 5, 6, 14, 15, 22, 23 are guaranteed magic blocks if they trigger.
		if (p_ptr.prace == RACE_MONSTER and not(monster_physical) and not(monster_ranged)) then

			if ((m_race(p_ptr.body_monster).countertype == 5 or m_race(p_ptr.body_monster).countertype == 6 or m_race(p_ptr.body_monster).countertype == 14 or m_race(p_ptr.body_monster).countertype == 15 or m_race(p_ptr.body_monster).countertype == 22 or m_race(p_ptr.body_monster).countertype == 23) and lua_randint(100) <= m_race(p_ptr.body_monster).counterchance) then

                        	msg_print("You block the magic attack!")

				-- If it was counters 14 or 15, also return the attack.
				if (m_race(p_ptr.body_monster).countertype == 14 or m_race(p_ptr.body_monster).countertype == 15) then

					msg_print("The attack bounces!")

					-- The "flg" parameter for "project" doesn't really apply to lua, so we must use an alternate function.
					no_magic_return = TRUE
					lua_project(0, rad, monster(who).fy, monster(who).fx, dam * (p_ptr.abilities[(CLASS_MONSTER * 10) + 7] + 1), element, 1)
					no_magic_return = FALSE
				end

                        	dam = 0
			end
		end

        	-- Magic resistance!
        	if (p_ptr.mres_dur > 0 and dam > 0 and not(monster_physical) and not(monster_ranged)) then
        
                	local damfract
			damfract = multiply_divide(dam, p_ptr.mres, 100)
                	dam = dam - damfract
        	end

        	-- Paldin's Resist Impure!
        	if ((element == GF_POIS or element == GF_RADIO or element == GF_DARK or element == GF_CHAOS) and dam > 0 and p_ptr.abilities[(CLASS_PALADIN * 10) + 6] >= 1) then

                	local damfract
			damfract = multiply_divide(dam, ((p_ptr.abilities[(CLASS_PALADIN * 10) + 6] * 5) + 25), 100)
                	dam = dam - damfract
                	if (dam < 0) then

                        	dam = dam * -1
                        	p_ptr.chp = p_ptr.chp + dam
                        	if (p_ptr.chp > p_ptr.mhp) then p_ptr.chp = p_ptr.mhp end
				dam = 0
                        	update_and_handle()
                	end
        	end

		-- Mage's Spell Absorbtion
        	-- Absorb the mana! :)
        	if (p_ptr.abilities[(CLASS_MAGE * 10) + 4] >= 1 and dam > 0 and not(monster_physical) and not(monster_ranged)) then

                	local damfract
			local ppower = p_ptr.abilities[(CLASS_MAGE * 10) + 4] * 10
			local mpower = monster(who).level + monster(who).mind
			damfract = multiply_divide(dam, (p_ptr.abilities[(CLASS_MAGE * 10) + 4] * 10), 100)
                	p_ptr.csp = p_ptr.csp + damfract
                	if (p_ptr.csp > p_ptr.msp) then p_ptr.csp = p_ptr.msp end

			-- May block the spell.
			if (lua_randint(ppower) >= lua_randint(mpower)) then

				msg_print("You absorb the spell!")
				dam = 0
			end
			
                	update_and_handle()
        	end

		-- Diviner's Divine Chaos!
		if (p_ptr.abilities[(CLASS_DIVINER * 10) + 2] >= 1 and dam > 0 and element == GF_CHAOS) then

			local ppower
			local mpower

			ppower = p_ptr.abilities[(CLASS_DIVINER * 10) + 2] * 100
			mpower = monster(who).level + monster(who).mind

                	if (lua_randint(ppower) >= lua_randint(mpower)) then

				msg_print("You predict the chaos, and counter it!")
				dam = 0
			end
		end

		-- Diviner's Dodge!
		if (p_ptr.abilities[(CLASS_DIVINER * 10) + 4] >= 1 and dam > 0 and ((monster_physical) or (monster_ranged))) then

			local ppower
			local mpower

			ppower = (p_ptr.skill[27] / 5) * p_ptr.abilities[(CLASS_DIVINER * 10) + 4]
			mpower = monster(who).level + monster(who).mind

                	if (lua_randint(ppower) >= lua_randint(mpower)) then

				msg_print("You predict the attack, and dodge it!")
				dam = 0
			end
		end

		-- Monsters: counters 1, 3, 10, 12, 17 and 19 can block melee attacks.
		if (p_ptr.prace == RACE_MONSTER and dam > 0 and (monster_physical)) then

			if ((m_race(p_ptr.body_monster).countertype == 1 or m_race(p_ptr.body_monster).countertype == 3 or m_race(p_ptr.body_monster).countertype == 10 or m_race(p_ptr.body_monster).countertype == 12 or m_race(p_ptr.body_monster).countertype == 17 or m_race(p_ptr.body_monster).countertype == 19) and lua_randint(100) <= m_race(p_ptr.body_monster).counterchance) then

				local proll
				local mroll

				proll = ((p_ptr.abilities[(CLASS_MONSTER * 10) + 7] * 5) * 3) + p_ptr.stat_ind[A_STR+1] + p_ptr.stat_ind[A_DEX+1]
				mroll = monster(who).str + monster(who).dex

				if (lua_randint(proll) >= lua_randint(mroll)) then

                        		msg_print("You block the attack!")

					-- If it was counters 10 or 12, also return the attack.
					if (m_race(p_ptr.body_monster).countertype == 10 or m_race(p_ptr.body_monster).countertype == 12) then

						msg_print("The attack is returned!")

						-- The "flg" parameter for "project" doesn't really apply to lua, so we must use an alternate function.
						no_magic_return = TRUE
						melee_attack = TRUE
						nevermiss = TRUE
						lua_project(0, rad, monster(who).fy, monster(who).fx, dam * (p_ptr.abilities[(CLASS_MONSTER * 10) + 7] + 1), element, 1)
						no_magic_return = FALSE
						melee_attack = FALSE
						nevermiss = FALSE
					end

                        		dam = 0
                		end
			end
		end

		-- Monsters: counters 4, 6, 13, 15, 21 and 23 are guaranteed melee block if they occur.
		if (p_ptr.prace == RACE_MONSTER and dam > 0 and (monster_physical)) then

			if ((m_race(p_ptr.body_monster).countertype == 4 or m_race(p_ptr.body_monster).countertype == 6 or m_race(p_ptr.body_monster).countertype == 13 or m_race(p_ptr.body_monster).countertype == 15 or m_race(p_ptr.body_monster).countertype == 21 or m_race(p_ptr.body_monster).countertype == 23) and lua_randint(100) <= m_race(p_ptr.body_monster).counterchance) then

                        	msg_print("You block the attack!")
				-- If it was counters 13 or 15, also return the attack.
				if (m_race(p_ptr.body_monster).countertype == 13 or m_race(p_ptr.body_monster).countertype == 15) then

					msg_print("The attack is returned!")

					-- The "flg" parameter for "project" doesn't really apply to lua, so we must use an alternate function.
					no_magic_return = TRUE
					melee_attack = TRUE
					nevermiss = TRUE
					lua_project(0, rad, monster(who).fy, monster(who).fx, dam * (p_ptr.abilities[(CLASS_MONSTER * 10) + 7] + 1), element, 1)
					no_magic_return = FALSE
					melee_attack = FALSE
					nevermiss = FALSE
				end
                        	dam = 0
			end
		end

		-- Kensai's Arrow Cutting!
		if (p_ptr.abilities[(CLASS_KENSAI * 10) + 6] >= 1 and dam > 0 and (monster_ranged) and (kensai_equip())) then

			local ppower
			local mpower

			ppower = (25 * p_ptr.abilities[(CLASS_KENSAI * 10) + 6]) + (2 * p_ptr.stat_ind[A_WIS+1])
			mpower = monster(who).level + monster(who).skill_ranged

                	if (lua_randint(ppower) >= lua_randint(mpower)) then

				msg_print("You cut the ranged attack with your sword!")
				dam = 0
			end
		end

		-- Kensai's Iajutsu!
		if (p_ptr.abilities[(CLASS_KENSAI * 10) + 2] >= 1 and (monster_physical) and (kensai_equip())) then
			msg_print("With Iajutsu...")
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
			if (damtype == 0) then damtype = GF_PHYSICAL end
			if (player_hit_monster(monster(who), hitbonus)) then
				melee_attack = TRUE
				no_magic_return = TRUE
				nevermiss = TRUE
				fire_ball_specific_grid(iajutsudam, monster(who).fx, monster(who).fy, 0, damtype)
				melee_attack = FALSE
				no_magic_return = FALSE
				nevermiss = FALSE
				if (monster_died) then

					monster_died = FALSE
					dam = 0
					--return 0 -- not sure what to do here.
				end
			else
				msg_print(string.format('Your iajutsu misses %s!', m_race(monster.r_idx).name_char))
			end

		end

		-- Monsters: counters 16, 17, 18 and 19 can block ranged attacks.
		if (p_ptr.prace == RACE_MONSTER and dam > 0 and (monster_ranged)) then

			if ((m_race(p_ptr.body_monster).countertype == 16 or m_race(p_ptr.body_monster).countertype == 17 or m_race(p_ptr.body_monster).countertype == 18 or m_race(p_ptr.body_monster).countertype == 19) and lua_randint(100) <= m_race(p_ptr.body_monster).counterchance) then

				local proll
				local mroll

				proll = ((p_ptr.abilities[(CLASS_MONSTER * 10) + 7] * 5) * 3) + p_ptr.stat_ind[A_DEX+1]
				mroll = monster(who).dex

				if (lua_randint(proll) >= lua_randint(mroll)) then

                        		msg_print("You block the attack!")
                        		dam = 0
                		end
			end
		end

		-- Monsters: counters 20, 21, 22 and 23 are guaranteed block if they occur.
		if (p_ptr.prace == RACE_MONSTER and dam > 0 and (monster_ranged)) then

			if ((m_race(p_ptr.body_monster).countertype == 20 or m_race(p_ptr.body_monster).countertype == 21 or m_race(p_ptr.body_monster).countertype == 22 or m_race(p_ptr.body_monster).countertype == 23) and lua_randint(100) <= m_race(p_ptr.body_monster).counterchance) then

                        	msg_print("You block the attack!")
                        	dam = 0
			end
		end

        	-- Rogue's Evasion ability!
        	if (p_ptr.abilities[(CLASS_ROGUE * 10) + 5] >= 1 and dam > 0 and not(monster_physical) and not(monster_ranged)) then

			local ppower
			local mpower

			ppower = p_ptr.abilities[(CLASS_ROGUE * 10) + 5] * 10
			mpower = monster(who).level + monster(who).mind

                	if (lua_randint(ppower) >= lua_randint(mpower)) then

				msg_print("You evade the attack!")
				dam = 0
			end
        	end

        	-- Enough agility can provide a chance to avoid damages...
        	if (p_ptr.skill_base[6] >= 70 and dam > 0 and not(monster_physical) and not(monster_ranged)) then

			local ppower
			local mpower

			ppower = p_ptr.skill[6]
			mpower = monster(who).level + monster(who).mind

                	if (lua_randint(ppower) >= lua_randint(mpower)) then

				msg_print("You evade the attack!")
				dam = 0
			end
        	end

		-- And of course, the Magic Defense skill!
		if (p_ptr.skill[28] >= 1 and dam > 0 and not(monster_physical) and not(monster_ranged)) then

			local ppower
			local mpower

			ppower = p_ptr.skill[28] * 10

			-- Bardic Grandeur.
        		if (p_ptr.events[29042] == 1 and p_ptr.abilities[(CLASS_BARD * 10) + 10] >= 1) then

				local chrbonus

				chrbonus = multiply_divide(p_ptr.stat_ind[A_CHR+1], 10, 100) * p_ptr.abilities[(CLASS_BARD * 10) + 10]

                		ppower = ppower + chrbonus
        		end

			mpower = monster(who).level + monster(who).mind

			if (lua_randint(ppower) >= lua_randint(mpower)) then

				msg_print("You evade the attack!");
				dam = 0;
			end
		end

		-- Ghostly Misfortune Nightmare ability!
		if (dam > 0 and p_ptr.abilities[(CLASS_NIGHT1 * 10) + 2] > 0 and p_ptr.cursed >= 3) then

			local ppower
			local mpower

			ppower = (p_ptr.cursed / 3) * p_ptr.abilities[(CLASS_NIGHT1 * 10) + 2]
			if (monster_physical) then
				mpower = monster(who).level + monster(who).str
			elseif (monster_ranged) then
				mpower = monster(who).level + monster(who).dex
			else
				mpower = monster(who).level + monster(who).mind
			end

			if (lua_randint(ppower) >= lua_randint(mpower)) then

				msg_print("You cancel the attack.")
				dam = 0
			end
		end

		-- Shadow of Misfortune Nightmare ability!
		if (dam > 0 and p_ptr.abilities[(CLASS_NIGHT1 * 10) + 4] > 0 and p_ptr.tim_invisible > 0) then

			local ppower
			local mpower

			ppower = multiply_divide(p_ptr.stat_ind[A_DEX+1], p_ptr.cursed / 5, 100)
			if (ppower < 5) then ppower = 5 end
			ppower = ppower * p_ptr.abilities[(CLASS_NIGHT1 * 10) + 4]

			if (monster_physical) then
				mpower = monster(who).level + monster(who).str
			elseif (monster_ranged) then
				mpower = monster(who).level + monster(who).dex
			else
				mpower = monster(who).level + monster(who).mind
			end

			if (lua_randint(ppower) >= lua_randint(mpower)) then

				msg_print("You dodge the attack.")
				dam = 0
			end
		end

		-- Shadow Queen's evasion ability.
		if (p_ptr.prace == RACE_MONSTER and m_race(p_ptr.body_monster).event_misc == 305 and dam > 0 and ((monster_physical) or (monster_ranged))) then

			local pstat
			local mstat

			pstat = p_ptr.stat_ind[A_DEX+1]
			mstat = monster(who).dex

			if (lua_randint(pstat) >= lua_randint(mstat)) then

				msg_print("You evade the attack!")
				dam = 0
			end
		end

		-- Nightmare Shadow Mistress's evasion ability.
		if (p_ptr.prace == RACE_MONSTER and m_race(p_ptr.body_monster).event_misc == 2303 and dam > 0) then

			local pstat
			local mstat

			pstat = p_ptr.stat_ind[A_DEX+1]

			if ((monster_physical) or (monster_ranged)) then

				mstat = monster(who).dex
			else
				mstat = monster(who).mind
			end

			if (lua_randint(pstat) >= lua_randint(mstat)) then

				msg_print("You evade all damages!")
				dam = 0
			end
		end

		-- Sphere of Essences.
		if (p_ptr.prace == RACE_MONSTER and m_race(p_ptr.body_monster).event_misc == 2312 and dam > 0) then

			local totres
			local r

			totres = 0

			for r = 0, MAX_RESIST do

				totres = totres + m_race(monster(who).r_idx).resistances[r]
			end

			if (totres > 100) then totres = 100 end
			if (totres < 0) then totres = 0 end

			dam = dam - multiply_divide(dam, totres, 100)	
		end

		-- Nightmare Beast.
		if (p_ptr.prace == RACE_MONSTER and m_race(p_ptr.body_monster).event_misc == 2314 and dam > 0) then

			if (not(element == GF_OLD_HEAL)) then

				dam = dam - multiply_divide(dam, 75, 100)
			end
		end
	end

	-- Elemental Lord's Element Shield
	if (p_ptr.elem_shield > 0 and element == p_ptr.elemlord) then

                dam = 0
        end

	-- Bardic Grandeur.
        if (p_ptr.events[29042] == 1 and p_ptr.abilities[(CLASS_BARD * 10) + 10] >= 1 and not(who == -2)) then

		local cursong
		cursong = p_ptr.events[29041]

		if (music_song[cursong+1].element == element and music_song[cursong+1].power >= m_race(monster(m_idx).r_idx).level) then

			local ppower
			local mpower
			ppower = multiply_divide(p_ptr.stat_ind[A_CHR+1], 25, 100) * p_ptr.abilities[(CLASS_BARD * 10) + 10]
			mpower = monster(m_idx).level + monster(m_idx).mind

			if (lua_randint(ppower) >= lua_randint(mpower)) then

				msg_print("Your performance negates the attack!")
				dam = 0
			end
		end
        end

	-- Monsters: Elite ability that halve damages.
	if (p_ptr.prace == RACE_MONSTER and dam > 0 and get_player_monster_ability(BOSS_HALVE_DAMAGES)) then

		dam = dam / 2
	end

	-- After all that, if the damages are still not prevented, proceed to next step!
	if (dam > 0) then

		-- Two variables used by the RADIO type.
		local poisdam
		local radiodam

		poisdam = 0
		radiodam = 0

		-- First, apply resistances.
		-- Some types does more than one kind of damages.
		-- But resistances won't wake you from a terrible nightmare...
		if (not(who == -2)) then
			if (m_race(monster(who).r_idx).cursed > 0 and p_ptr.resistances[element+1] > 0) then

				local mroll = 0
				local proll = 0

				mroll = multiply_divide(monster(who).mind, m_race(monster(who).r_idx).cursed, 100)
				proll = p_ptr.skill[28] * 10

				if (lua_randint(mroll) >= lua_randint(proll)) then

					msg_print(string.format('%s pierced your resistance!', m_race(monster(who).r_idx).name_char))
				else

					dam = dam - multiply_divide(dam, p_ptr.resistances[element+1], 100)
				end
			else
				dam = dam - multiply_divide(dam, p_ptr.resistances[element+1], 100)
			end
		end

		-- Dual/triple elements...
		if (element == GF_MISSILE) then

			local misphys
			local mismagic

			misphys = dam / 2
			mismagic = dam / 2

			misphys = misphys - multiply_divide(misphys, p_ptr.pres, 100)
			misphys = misphys - multiply_divide(misphys, p_ptr.resistances[GF_PHYSICAL+1], 100)
			mismagic = mismagic - multiply_divide(mismagic, p_ptr.resistances[GF_MANA+1], 100)

			dam =  misphys + mismagic

			-- Elemental Lord's Shield Of Element can reduce some damages...
        		if (p_ptr.elem_shield > 0 and ((p_ptr.elemlord == GF_PHYSICAL) or (p_ptr.elemlord == GF_MANA))) then

                		dam = dam / 2
        		end
			
		end

		if (element == GF_FROSTFIRE) then

			local firedam
			local colddam

			firedam = dam / 2
			colddam = dam / 2

			firedam = firedam - multiply_divide(firedam, p_ptr.resistances[GF_FIRE+1], 100)
			colddam = colddam - multiply_divide(colddam, p_ptr.resistances[GF_COLD+1], 100)

			dam =  firedam + colddam

			-- Elemental Lord's Shield Of Element can reduce some damages...
        		if (p_ptr.elem_shield > 0 and ((p_ptr.elemlord == GF_FIRE) or (p_ptr.elemlord == GF_COLD))) then

                		dam = dam / 2
        		end
			
		end

		if (element == GF_GREY) then

			local lightdam
			local darkdam

			lightdam = dam / 2
			darkdam = dam / 2

			lightdam = lightdam - multiply_divide(lightdam, p_ptr.resistances[GF_LITE+1], 100)
			darkdam = darkdam - multiply_divide(darkdam, p_ptr.resistances[GF_DARK+1], 100)

			dam =  lightdam + darkdam

			-- Elemental Lord's Shield Of Element can reduce some damages...
        		if (p_ptr.elem_shield > 0 and ((p_ptr.elemlord == GF_LITE) or (p_ptr.elemlord == GF_DARK))) then

                		dam = dam / 2
        		end
			
		end

		if (element == GF_MUD) then

			local earthdam
			local waterdam

			earthdam = dam / 2
			waterdam = dam / 2

			earthdam = earthdam - multiply_divide(earthdam, p_ptr.resistances[GF_EARTH+1], 100)
			waterdam = waterdam - multiply_divide(waterdam, p_ptr.resistances[GF_WATER+1], 100)

			dam = earthdam + waterdam

			-- Elemental Lord's Shield Of Element can reduce some damages...
        		if (p_ptr.elem_shield > 0 and ((p_ptr.elemlord == GF_EARTH) or (p_ptr.elemlord == GF_WATER))) then

                		dam = dam / 2
        		end

		end

		if (element == GF_TOXIC) then

			local aciddam

			aciddam = dam / 3
			poisdam = dam / 3
			radiodam = dam / 3

			aciddam = aciddam - multiply_divide(aciddam, p_ptr.resistances[GF_ACID+1], 100)
			poisdam = poisdam - multiply_divide(poisdam, p_ptr.resistances[GF_POIS+1], 100)
			radiodam = radiodam - multiply_divide(radiodam, p_ptr.resistances[GF_RADIO+1], 100)

			dam =  aciddam + poisdam + radiodam

			-- Elemental Lord's Shield Of Element can reduce some damages...
        		if (p_ptr.elem_shield > 0 and ((p_ptr.elemlord == GF_ACID) or (p_ptr.elemlord == GF_POIS) or (p_ptr.elemlord == GF_RADIO))) then

                		dam = dam - (dam / 3)
        		end
			
		end

		if (element == GF_ICE) then

			local icedam
			local physdam

			icedam = dam / 2
			physdam = dam / 2

			icedam = icedam - multiply_divide(icedam, p_ptr.resistances[GF_COLD+1], 100)
			physdam = physdam - multiply_divide(physdam, p_ptr.resistances[GF_PHYSICAL+1], 100)

			dam = icedam + physdam

			-- Elemental Lord's Shield Of Element can reduce some damages...
        		if (p_ptr.elem_shield > 0 and ((p_ptr.elemlord == GF_COLD) or (p_ptr.elemlord == GF_PHYSICAL))) then

                		dam = dam / 2
        		end

		end

		-- Elemental!
		if (element == GF_ELEMENTAL) then

			local res
			local r
			local original

			original = dam

			for r = GF_FIRE, GF_MANA do

				if (not(r == GF_PHYSICAL)) then
					dam = dam - multiply_divide((original / 15), p_ptr.resistances[r+1], 100)
					-- Elemental Lord's Shield Of Element can reduce some damages...
        				if (p_ptr.elem_shield > 0 and (p_ptr.elemlord == r)) then

                				dam = dam - (dam / 15)
        				end
				end
			end
		end

		-- This is not the GF_OLD_CONF, but rather a damaging form of confusion.
		-- It's a chaos/mana hybrid, though resistance to confusion greatly reduces
		-- the damages.
		if (element == GF_CONFUSION) then

			local chaosdam
			local manadam

			chaosdam = dam / 2
			manadam = dam / 2

			chaosdam = chaosdam - multiply_divide(chaosdam, p_ptr.resistances[GF_CHAOS+1], 100)
			manadam = manadam - multiply_divide(manadam, p_ptr.resistances[GF_MANA+1], 100)

			dam =  chaosdam + manadam

			-- Resistance to confusion greatly reduces damages.
			if (p_ptr.resist_conf) then
				dam = dam / 3
			end

			-- Elemental Lord's Shield Of Element can reduce some damages...
        		if (p_ptr.elem_shield > 0 and ((p_ptr.elemlord == GF_CHAOS) or (p_ptr.elemlord == GF_MANA))) then

                		dam = dam / 2
        		end
			
		end

		-- Make sure no negative damages.
		if (dam < 0) then dam = 0 end

		-- Take damages.
		-- Some elements don't actually cause damages.
		if (element ~= GF_LOSE_STR and element ~= GF_LOSE_INT and element ~= GF_LOSE_WIS and element ~= GF_LOSE_DEX
		and element ~= GF_LOSE_CON and element ~= GF_LOSE_CHR and element ~= GF_LOSE_ALL and element ~= GF_LOSE_EXP
		and element ~= GF_LIFE_BLAST and element ~= GF_OLD_CONF and element ~= GF_FEAR and element ~= GF_OLD_SLOW
		and element ~= GF_OLD_HEAL and element ~= GF_RESTORE_STATS and element ~= GF_RESTORE_STATUS and element ~= GF_RESTORE_LEVELS
		and element ~= GF_RESTORATION and element ~= GF_CURE_MUTATIONS and element ~= GF_STRENGTH and element ~= GF_HEROISM
		and element ~= GF_STAR_HEROISM and element ~= GF_RESTORE_MANA and element ~= GF_DIVINATION and element ~= GF_PARALYZE and element ~= GF_HARM
		and element ~= GF_EVIL_SMITE and element ~= GF_GOOD_SMITE and element ~= GF_STONE_TO_MUD) then

			if (who == -2) then take_hit(dam, "n/a")
			else take_hit(dam, m_name) end
		else
			-- Life Blast causes percentile damages.
			-- You can resist it.
			if (element == GF_LIFE_BLAST) then

				local lbdam
				local ppower
				local mpower

				ppower = (p_ptr.stat_ind[A_WIS+1])
				mpower = (monster(who).level + monster(who).mind)

				lbdam = multiply_divide(p_ptr.mhp, dam, 100)
				if (lbdam == p_ptr.mhp) then lbdam = lbdam + 1 end

				if (lua_randint(mpower) >= lua_randint(ppower)) then

					take_hit(lbdam, m_name)
					update_and_handle()
				else
					msg_print("You resist the effects.")
					dam = 0
				end
			end
			-- When used against the player, Divination uses
			-- the player's level and the current dungeon level.
			if (element == GF_DIVINATION) then

				local difference
				local divpower
				local ppower
				local mpower

				-- Store the original power.
				divpower = dam

				-- Damages starts at 100%.
				dam = p_ptr.mhp

				-- No difference yet.
				difference = 0

				-- Increase difference by 5 for every depths points different.
				if (p_ptr.lev > divpower) then
					difference = ((p_ptr.lev - divpower) * 5)
				elseif (p_ptr.lev < divpower) then
					difference = ((divpower - p_ptr.lev) * 5)
				end

				-- Reduce damages by (difference)%.
				dam = dam - multiply_divide(dam, difference, 100)

				if (dam < 0) then dam = 0 end

				if (dam == p_ptr.mhp) then dam = dam + 1 end

				-- Now that we have damages, attempt resistance roll.
				ppower = (p_ptr.stat_ind[A_WIS+1])
				mpower = (monster(who).level + monster(who).mind)

				if (lua_randint(mpower) >= lua_randint(ppower)) then

					take_hit(dam, m_name)
					update_and_handle()
				else
					msg_print("You resist the effects.")
					dam = 0
				end
			end

			-- Stone to Mud will damage you if you're weak to it!
			if (element == GF_STONE_TO_MUD) then
				
				if (p_ptr.resistances[GF_STONE_TO_MUD + 1] >= 0) then
					
					dam = 0
				end
			end

			-- Harm can be resisted by Constitution.
			if (element == GF_HARM) then
				
				local ppower
				local mpower

				ppower = (p_ptr.stat_ind[A_CON+1])
				mpower = (monster(who).level + monster(who).mind)

				if (lua_randint(mpower) >= lua_randint(ppower)) then

					take_hit(dam, m_name)
					update_and_handle()
				else
					msg_print("You resist the effects.")
					dam = 0
				end
			end

			-- Smite Evil only works if you're evil.
			if (element == GF_EVIL_SMITE) then
				
				if (p_ptr.alignment < 0) then

					take_hit(dam, m_name)
					update_and_handle()
				else
					-- msg_print("You are unaffected.")
					dam = 0
				end
			end

			-- Smite Good only works if you're good.
			if (element == GF_GOOD_SMITE) then
				
				if (p_ptr.alignment > 0) then

					take_hit(dam, m_name)
					update_and_handle()
				else
					-- msg_print("You are unaffected.")
					dam = 0
				end
			end

			-- Healing! (mostly used by potions.)
			if (element == GF_OLD_HEAL) then

				p_ptr.chp = p_ptr.chp + dam
				if (p_ptr.chp > p_ptr.mhp) then p_ptr.chp = p_ptr.mhp end
				update_and_handle()
			end
			-- Restore mana.
			if (element == GF_RESTORE_MANA) then

				p_ptr.csp = p_ptr.csp + dam
				if (p_ptr.csp > p_ptr.msp) then p_ptr.csp = p_ptr.msp end
				update_and_handle()
			end
			-- Restore stats.
			if (element == GF_RESTORE_STATS) then

				do_res_stat(A_STR)
				do_res_stat(A_INT)
				do_res_stat(A_WIS)
				do_res_stat(A_DEX)
				do_res_stat(A_CON)
				do_res_stat(A_CHR)
				update_and_handle()
			end
			-- Restore status.
			if (element == GF_RESTORE_STATUS) then

				set_poisoned(0)
				set_blind(0)
				set_confused(0)
				set_image(0)
				set_stun(0)
				set_cut(0)
				set_afraid(0)
				update_and_handle()
			end
			-- Restore levels.
			if (element == GF_RESTORE_LEVELS) then

				restore_level()
				update_and_handle()
			end
			-- Restoration.
			if (element == GF_RESTORATION) then

				do_res_stat(A_STR)
				do_res_stat(A_INT)
				do_res_stat(A_WIS)
				do_res_stat(A_DEX)
				do_res_stat(A_CON)
				do_res_stat(A_CHR)
				set_poisoned(0)
				set_blind(0)
				set_confused(0)
				set_image(0)
				set_stun(0)
				set_cut(0)
				set_afraid(0)
				restore_level()
				update_and_handle()
			end
			-- Cure Mutations.
			if (element == GF_CURE_MUTATIONS) then

				p_ptr.stat_mut[A_STR+1] = 0
				p_ptr.stat_mut[A_INT+1] = 0
				p_ptr.stat_mut[A_WIS+1] = 0
				p_ptr.stat_mut[A_DEX+1] = 0
				p_ptr.stat_mut[A_CON+1] = 0
				p_ptr.stat_mut[A_CHR+1] = 0
				p_ptr.muta1 = 0
				p_ptr.muta2 = 0
				p_ptr.muta3 = 0
				update_and_handle()
			end
			-- Strength.
			if (element == GF_STRENGTH) then

				p_ptr.str_boost = dam
                        	set_str_boost(10 + (p_ptr.skill[11] / 2))
				update_and_handle()
			end
			-- Heroism.
			if (element == GF_HEROISM) then

				set_hero(dam)
				update_and_handle()
			end
			-- Star Heroism.
			if (element == GF_STAR_HEROISM) then

				p_ptr.str_boost = dam
				set_hero(dam)
				set_str_boost(10 + (p_ptr.skill[11] / 2))
				update_and_handle()
			end
		end

		-- Now, apply the various special effects... if we're still alive.
		if (p_ptr.chp >= 0 and not(who == -2)) then

			-- Poison can poison.
			if (element == GF_POIS and dam > 0) then

				if (lua_randint(100) > p_ptr.resistances[GF_POIS+1] and (lua_randint(p_ptr.stat_ind[A_CON+1]) < lua_randint(100))) then

					local ppower
					local mpower

					ppower = (p_ptr.stat_ind[A_CON+1])
					mpower = (monster(who).level + monster(who).mind)

                                	if (lua_randint(mpower) >= lua_randint(ppower)) then

						set_poisoned(p_ptr.poisoned + 10)
						if (p_ptr.poisoned > 100) then p_ptr.poisoned = 100 end
						update_and_handle()
					end

                        		-- No damages.
                        		dam = 0
				end
			end

			-- Radio can cause mutations.
			if (element == GF_RADIO and dam > 0) then

				if (lua_randint(100) > p_ptr.resistances[GF_RADIO+1] and (lua_randint(p_ptr.stat_ind[A_CON+1]) < lua_randint(100))) then

					local ppower
					local mpower

					ppower = (p_ptr.stat_ind[A_CON+1])
					mpower = (monster(who).level + monster(who).mind)

                                	if (lua_randint(mpower) >= lua_randint(ppower)) then

						msg_print("Your body changes from radiations!")
						p_ptr.stat_mut[A_STR+1] = p_ptr.stat_mut[A_STR+1] + (lua_randint(3) - lua_randint(4))
						p_ptr.stat_mut[A_INT+1] = p_ptr.stat_mut[A_INT+1] + (lua_randint(3) - lua_randint(4))
						p_ptr.stat_mut[A_WIS+1] = p_ptr.stat_mut[A_WIS+1] + (lua_randint(3) - lua_randint(4))
						p_ptr.stat_mut[A_DEX+1] = p_ptr.stat_mut[A_DEX+1] + (lua_randint(3) - lua_randint(4))
						p_ptr.stat_mut[A_CON+1] = p_ptr.stat_mut[A_CON+1] + (lua_randint(3) - lua_randint(4))
						p_ptr.stat_mut[A_CHR+1] = p_ptr.stat_mut[A_CHR+1] + (lua_randint(3) - lua_randint(4))
						update_and_handle()
					end

                        		-- No damages.
                        		dam = 0
				end
			end

			-- Toxic can poison AND mutate!!
			if (element == GF_TOXIC and dam > 0) then

				if (poisdam > 0) then

					if (lua_randint(100) > p_ptr.resistances[GF_POIS+1] and (lua_randint(p_ptr.stat_ind[A_CON+1]) < lua_randint(100))) then
						local ppower
						local mpower

						ppower = (p_ptr.stat_ind[A_CON+1])
						mpower = (monster(who).level + monster(who).mind)

                                		if (lua_randint(mpower) >= lua_randint(ppower)) then

							set_poisoned(p_ptr.poisoned + 10)
							if (p_ptr.poisoned > 100) then p_ptr.poisoned = 100 end
							update_and_handle()
						end
					end
				end
				if (radiodam > 0) then

					if (lua_randint(100) > p_ptr.resistances[GF_RADIO+1] and (lua_randint(p_ptr.stat_ind[A_CON+1]) < lua_randint(100))) then

						local ppower
						local mpower

						ppower = (p_ptr.stat_ind[A_CON+1])
						mpower = (monster(who).level + monster(who).mind)

                                		if (lua_randint(mpower) >= lua_randint(ppower)) then

							msg_print("Your body changes from radiations!")
							p_ptr.stat_mut[A_STR+1] = p_ptr.stat_mut[A_STR+1] + (lua_randint(3) - lua_randint(4))
							p_ptr.stat_mut[A_INT+1] = p_ptr.stat_mut[A_INT+1] + (lua_randint(3) - lua_randint(4))
							p_ptr.stat_mut[A_WIS+1] = p_ptr.stat_mut[A_WIS+1] + (lua_randint(3) - lua_randint(4))
							p_ptr.stat_mut[A_DEX+1] = p_ptr.stat_mut[A_DEX+1] + (lua_randint(3) - lua_randint(4))
							p_ptr.stat_mut[A_CON+1] = p_ptr.stat_mut[A_CON+1] + (lua_randint(3) - lua_randint(4))
							p_ptr.stat_mut[A_CHR+1] = p_ptr.stat_mut[A_CHR+1] + (lua_randint(3) - lua_randint(4))
							update_and_handle()
						end
					end
				end

                        	-- No damages.
                        	dam = 0
			end

			-- Confusion confuses!! :O
			if (element == GF_CONFUSION and not(p_ptr.resist_conf) and dam > 0) then

				local ppower
				local mpower

				ppower = (p_ptr.stat_ind[A_WIS+1])
				mpower = (monster(who).level + monster(who).mind)

                                if (lua_randint(mpower) >= lua_randint(ppower)) then

					set_confused(10)
					update_and_handle()
				end

                        	-- No damages.
                        	dam = 0
			end

			-- Slow
			if (element == GF_OLD_SLOW and dam > 0) then

				local ppower
				local mpower

				ppower = (p_ptr.stat_ind[A_WIS+1])
				mpower = (monster(who).level + monster(who).mind)

                                if (lua_randint(mpower) >= lua_randint(ppower)) then

					set_slow(dam)
					update_and_handle()
                                else

					msg_print("You resist the effects.")
				end

                        	-- No damages.
                        	dam = 0
			end
			-- Confuse
			if (element == GF_OLD_CONF and not(p_ptr.resist_conf) and dam > 0) then

				local ppower
				local mpower

				ppower = (p_ptr.stat_ind[A_WIS+1])
				mpower = (monster(who).level + monster(who).mind)

                                if (lua_randint(mpower) >= lua_randint(ppower)) then

					set_confused(dam)
					update_and_handle()
                                else

					msg_print("You resist the effects.")
				end

                        	-- No damages.
                        	dam = 0
			end
			-- Sleep(paralysis)
			if (element == GF_OLD_SLEEP and not(safety_check() or (get_monster_flag3(p_ptr.body_monster, RF3_NO_SLEEP))) and dam > 0) then

				local ppower
				local mpower

				ppower = (p_ptr.stat_ind[A_WIS+1])
				mpower = (monster(who).level + monster(who).mind)

                                if (lua_randint(mpower) >= lua_randint(ppower)) then

					set_paralyzed(dam)
					update_and_handle()
                                else

					msg_print("You resist the effects.")
				end

                        	-- No damages.
                        	dam = 0
			end
			-- Fear
			if (element == GF_FEAR and not(p_ptr.resist_fear) and dam > 0) then

				local ppower
				local mpower

				ppower = (p_ptr.stat_ind[A_WIS+1])
				mpower = (monster(who).level + monster(who).mind)

                                if (lua_randint(mpower) >= lua_randint(ppower)) then

					set_afraid(dam)
					update_and_handle()
                                else

					msg_print("You resist the effects.")
				end

                        	-- No damages.
                        	dam = 0
			end

			-- The Alteration Paralyze.
			if (element == GF_PARALYZE) then

				local ppower
				local mpower

				ppower = (p_ptr.stat_ind[A_WIS+1])
				mpower = (monster(who).level + monster(who).mind)

				if (safety_check() or (get_monster_flag3(p_ptr.body_monster, RF3_NO_STUN))) then

                                	msg_print("You are unaffected.")
                        	else

                                	if (lua_randint(mpower) >= lua_randint(ppower)) then

                                        	msg_print("You have been paralyzed!")

						set_paralyzed(dam)
						update_and_handle()
                                	else

						msg_print("You resist the effects.")
					end
                        	end

                        	-- No damages.
                        	dam = 0
                	end

			-- Soul Crush, Andraos' spell.
			if (element == GF_SOUL_CRUSH) then

				local ppower
				local mpower

				ppower = (p_ptr.stat_ind[A_WIS+1])
				mpower = (monster(who).level + monster(who).mind)

                                if (lua_randint(mpower) >= lua_randint(ppower)) then

                                        msg_print("Your soul has been crushed!")
					dec_stat(A_INT, p_ptr.stat_ind[A_INT+1], 2)
					dec_stat(A_WIS, p_ptr.stat_ind[A_WIS+1], 2)

					if (not(p_ptr.resist_conf)) then

						set_confused(10)
					end
					update_and_handle()
                                else

					msg_print("You resist the effects.")
				end

                        	-- No damages.
                        	dam = 0
                	end

			-- Stats reduction
			if (element == GF_LOSE_STR and not(p_ptr.sustain_str) and dam > 0) then

				local ppower
				local mpower

				ppower = (p_ptr.stat_ind[A_WIS+1])
				mpower = (monster(who).level + monster(who).mind)

                                if (lua_randint(mpower) >= lua_randint(ppower)) then

					-- The third parameter is the mode.
					-- 1 = Temporary stat loss. Will restore by itself.
					-- 2 = Normal stat loss. Need a spell or potion to restore.
					-- 3 = Permanent stat loss. Cannot be restored. Better not use this one carelessly!
					dec_stat(A_STR, dam, 2)
					update_and_handle()
                                else

					msg_print("You resist the effects.")
				end

                        	-- No damages.
                        	dam = 0
			end
			if (element == GF_LOSE_INT and not(p_ptr.sustain_int) and dam > 0) then

				local ppower
				local mpower

				ppower = (p_ptr.stat_ind[A_WIS+1])
				mpower = (monster(who).level + monster(who).mind)

                                if (lua_randint(mpower) >= lua_randint(ppower)) then

					-- The third parameter is the mode.
					-- 1 = Temporary stat loss. Will restore by itself.
					-- 2 = Normal stat loss. Need a spell or potion to restore.
					-- 3 = Permanent stat loss. Cannot be restored. Better not use this one carelessly!
					dec_stat(A_INT, dam, 2)
					update_and_handle()
                                else

					msg_print("You resist the effects.")
				end

                        	-- No damages.
                        	dam = 0
			end
			if (element == GF_LOSE_WIS and not(p_ptr.sustain_wis) and dam > 0) then

				local ppower
				local mpower

				ppower = (p_ptr.stat_ind[A_WIS+1])
				mpower = (monster(who).level + monster(who).mind)

                                if (lua_randint(mpower) >= lua_randint(ppower)) then

					-- The third parameter is the mode.
					-- 1 = Temporary stat loss. Will restore by itself.
					-- 2 = Normal stat loss. Need a spell or potion to restore.
					-- 3 = Permanent stat loss. Cannot be restored. Better not use this one carelessly!
					dec_stat(A_WIS, dam, 2)
					update_and_handle()
                                else

					msg_print("You resist the effects.")
				end

                        	-- No damages.
                        	dam = 0
			end
			if (element == GF_LOSE_DEX and not(p_ptr.sustain_dex) and dam > 0) then

				local ppower
				local mpower

				ppower = (p_ptr.stat_ind[A_WIS+1])
				mpower = (monster(who).level + monster(who).mind)

                                if (lua_randint(mpower) >= lua_randint(ppower)) then

					-- The third parameter is the mode.
					-- 1 = Temporary stat loss. Will restore by itself.
					-- 2 = Normal stat loss. Need a spell or potion to restore.
					-- 3 = Permanent stat loss. Cannot be restored. Better not use this one carelessly!
					dec_stat(A_DEX, dam, 2)
					update_and_handle()
                                else

					msg_print("You resist the effects.")
				end

                        	-- No damages.
                        	dam = 0
			end
			if (element == GF_LOSE_CON and not(p_ptr.sustain_con) and dam > 0) then

				local ppower
				local mpower

				ppower = (p_ptr.stat_ind[A_WIS+1])
				mpower = (monster(who).level + monster(who).mind)

                                if (lua_randint(mpower) >= lua_randint(ppower)) then

					-- The third parameter is the mode.
					-- 1 = Temporary stat loss. Will restore by itself.
					-- 2 = Normal stat loss. Need a spell or potion to restore.
					-- 3 = Permanent stat loss. Cannot be restored. Better not use this one carelessly!
					dec_stat(A_CON, dam, 2)
					update_and_handle()
                                else

					msg_print("You resist the effects.")
				end

                        	-- No damages.
                        	dam = 0
			end
			if (element == GF_LOSE_CHR and not(p_ptr.sustain_chr) and dam > 0) then

				local ppower
				local mpower

				ppower = (p_ptr.stat_ind[A_WIS+1])
				mpower = (monster(who).level + monster(who).mind)

                                if (lua_randint(mpower) >= lua_randint(ppower)) then

					-- The third parameter is the mode.
					-- 1 = Temporary stat loss. Will restore by itself.
					-- 2 = Normal stat loss. Need a spell or potion to restore.
					-- 3 = Permanent stat loss. Cannot be restored. Better not use this one carelessly!
					dec_stat(A_CHR, dam, 2)
					update_and_handle()
                                else

					msg_print("You resist the effects.")
				end

                        	-- No damages.
                        	dam = 0
			end
			if (element == GF_LOSE_ALL and dam > 0) then

				local ppower
				local mpower

				ppower = (p_ptr.stat_ind[A_WIS+1])
				mpower = (monster(who).level + monster(who).mind)

                                if (lua_randint(mpower) >= lua_randint(ppower)) then

					if (not(p_ptr.sustain_str)) then
						msg_print("Your strength has been reduced!")
						dec_stat(A_STR, dam, 2)
					end
					if (not(p_ptr.sustain_int)) then
						msg_print("Your intelligence has been reduced!")
						dec_stat(A_INT, dam, 2)
					end
					if (not(p_ptr.sustain_wis)) then
						msg_print("Your wisdom has been reduced!")
						dec_stat(A_WIS, dam, 2)
					end
					if (not(p_ptr.sustain_dex)) then
						msg_print("Your dexterity has been reduced!")
						dec_stat(A_DEX, dam, 2)
					end
					if (not(p_ptr.sustain_con)) then
						msg_print("Your constitution has been reduced!")
						dec_stat(A_CON, dam, 2)
					end
					if (not(p_ptr.sustain_chr)) then
						msg_print("Your charisma has been reduced!")
						dec_stat(A_CHR, dam, 2)
					end
					update_and_handle()
                                else

					msg_print("You resist the effects.")
				end

                        	-- No damages.
                        	dam = 0
			end
			if (element == GF_LOSE_EXP and not(p_ptr.hold_life) and dam > 0) then

				local ppower
				local mpower

				ppower = (p_ptr.stat_ind[A_WIS+1])
				mpower = (monster(who).level + monster(who).mind)

                                if (lua_randint(mpower) >= lua_randint(ppower)) then

					lose_exp(dam)
					update_and_handle()
                                else

					msg_print("You resist the effects.")
				end

                        	-- No damages.
                        	dam = 0
			end

		end

	end

end

-- Returns the name of a given element.
function get_element_name (element)
	
	local elementname

	if (element == GF_FIRE) then elementname = "Fire"
        elseif (element == GF_COLD) then elementname = "Cold"
        elseif (element == GF_ELEC) then elementname = "Electric"
        elseif (element == GF_ACID) then elementname = "Acid"
        elseif (element == GF_POIS) then elementname = "Poison"
        elseif (element == GF_RADIO) then elementname = "Radio"
        elseif (element == GF_WATER) then elementname = "Water"
        elseif (element == GF_CHAOS) then elementname = "Chaos"
        elseif (element == GF_DARK) then elementname = "Darkness"
        elseif (element == GF_LITE) then elementname = "Light"
        elseif (element == GF_EARTH) then elementname = "Earth"
        elseif (element == GF_SOUND) then elementname = "Sound"
        elseif (element == GF_WIND) then elementname = "Wind"
	elseif (element == GF_WARP) then elementname = "Warp"
        elseif (element == GF_MISSILE) then elementname = "Missile"
        elseif (element == GF_PHYSICAL) then elementname = "Physical"
        elseif (element == GF_MANA) then elementname = "Mana"
	elseif (element == GF_FROSTFIRE) then elementname = "FrostFire"
	elseif (element == GF_GREY) then elementname = "Grey"
	elseif (element == GF_TOXIC) then elementname = "Toxic"
	elseif (element == GF_MUD) then elementname = "Mud"
	elseif (element == GF_ICE) then elementname = "Ice"
	elseif (element == GF_ELEMENTAL) then elementname = "Elemental"
	elseif (element == GF_LIFE_BLAST) then elementname = "Life Blast"
	elseif (element == GF_STONE_TO_MUD) then elementname = "Stone to Mud"
        elseif (element == GF_REDUCE_HIT) then elementname = "Reduce Hit Rate"
        elseif (element == GF_REDUCE_DEF) then elementname = "Reduce Defense"
        elseif (element == GF_WEAKEN) then elementname = "Weaken"
        elseif (element == GF_REDUCE_SPEED) then elementname = "Reduce Speed"
        elseif (element == GF_RETROGRADE) then elementname = "Retrograde"
        elseif (element == GF_LOCK) then elementname = "Lock"
        elseif (element == GF_EVOLVE) then elementname = "Evolve"
        elseif (element == GF_UNEVOLVE) then elementname = "Unevolve"
        elseif (element == GF_FEAR_CURSE) then elementname = "Demoralize"
	elseif (element == GF_PARALYZE) then elementname = "Paralyze"
	elseif (element == GF_CONFUSION) then elementname = "Confusion"
	elseif (element == GF_OLD_CONF) then elementname = "Confuse"
	elseif (element == GF_OLD_HEAL) then elementname = "Healing"
	elseif (element == GF_OLD_SLEEP) then elementname = "Sleep"
	elseif (element == GF_RESTORE_MANA) then elementname = "Restore Mana"
	elseif (element == GF_FEAR) then elementname = "Fear"
	elseif (element == GF_RESTORE_STATS) then elementname = "Restore Stats"
	elseif (element == GF_RESTORE_STATUS) then elementname = "Restore Status"
	elseif (element == GF_RESTORE_LEVELS) then elementname = "Restore Levels"
	elseif (element == GF_RESTORATION) then elementname = "Restoration"
	elseif (element == GF_CURE_MUTATIONS) then elementname = "Cure Mutations"
	elseif (element == GF_STRENGTH) then elementname = "Raise Strength"
	elseif (element == GF_HEROISM) then elementname = "Heroism"
	elseif (element == GF_STAR_HEROISM) then elementname = "*Heroism*"
	elseif (element == GF_LOSE_STR) then elementname = "Reduce Strength"
	elseif (element == GF_LOSE_INT) then elementname = "Reduce Intelligence"
	elseif (element == GF_LOSE_WIS) then elementname = "Reduce Wisdom"
	elseif (element == GF_LOSE_DEX) then elementname = "Reduce Dexterity"
	elseif (element == GF_LOSE_CON) then elementname = "Reduce Constitution"
	elseif (element == GF_LOSE_CHR) then elementname = "Reduce Charisma"
	elseif (element == GF_LOSE_ALL) then elementname = "Reduce Stats"
	elseif (element == GF_LOSE_EXP) then elementname = "Reduce Experience"
	elseif (element == GF_DIVINATION) then elementname = "Divination"
	elseif (element == GF_UNDEAD_SMITE) then elementname = "Smite Undeads"
	elseif (element == GF_DEMON_SMITE) then elementname = "Smite Demons"
	elseif (element == GF_HARM) then elementname = "Harm"
	elseif (element == GF_EVIL_SMITE) then elementname = "Smite Evil"
	elseif (element == GF_GOOD_SMITE) then elementname = "Smite Good"
	elseif (element == GF_NIGHTMARES) then elementname = "Nightmares"
	elseif (element == GF_COMMAND_ELEMENT) then elementname = "Command Element"
	elseif (element == GF_RACIAL_CHAMPION) then elementname = "Racial Champion"
	elseif (element == GF_SOUL_CRUSH) then elementname = "Soul Crush"
	elseif (element == GF_DURATION) then elementname = "Duration (Summoned Monsters)"
	else elementname = "Unknown" end

	return elementname
end

-- Function that returns the power of a potion.
function potion_power (potion)

	local potionpower

	-- Power is based on alchemy.
	if (not(get_object_flag4(potion, TR4_MODERATE_POWER))) then
		potionpower = potion.branddam * (p_ptr.skill[11] + 1)
	else
		potionpower = potion.branddam
	end
	potionpower = potionpower + multiply_divide(potionpower, (p_ptr.skill[11] * 10), 100)

	return (potionpower)
end

-- Function called when drinking a potion.
-- I'm placing the function here, since it's all elements based now anyway.

-- The way it works is that it causes an elemental projection on the player.
-- ...yep, that's it. :)

function drink_potion (potion)

	local power

	power = potion_power (potion)
	
	lua_project(-2, 0, py, px, power, potion.brandtype, 1)

	update_and_handle()
end

-- Event handlers should be added for every functions you plan on using.
add_event_handler("element_hit_monster", element_hit_monster)
add_event_handler("element_hit_player", element_hit_player)
add_event_handler("get_element_name", get_element_name)
add_event_handler("potion_power", potion_power)
add_event_handler("drink_potion", drink_potion)
