-- File: scripts.lua
-- This file contains various functions that can be called in quests
-- or by monsters attacks. If a function is called by a monster during
-- combat, by either a spell or an attack, it must have one parameter
-- that will be the monster's m_idx id. You will then have to reference
-- the monster using the monster(m_idx) function.
-- Example:
-- function my_scripted_attack (m_idx)
--     msg_print(string.format('The monster has %d hp.', monster(m_idx).hp))
-- end
--
-- Other scripted functions do not have a parameter.

-- Insert whatever formula you want here. ;)
function skill_points_per_levels ()

	local amt

	amt = 10

	return (amt)
end

-- Insert whatever formula you want here. ;)
function stat_points_per_levels ()

	local amt

	amt = 2

	return (amt)
end

-- Insert whatever formula you want here. ;)
function ability_points_per_levels ()

	local amt

	amt = 1

	return (amt)
end

-- Starting stats, and stats/skills/ability points.
function starting_stats ()

	local i

	for i = 1, 6 do

                -- Starting stat is 5.
                p_ptr.stat_max[i] = 5

		-- No mutations.
		p_ptr.stat_mut[i] = 0

                -- Save the resulting stat maximum
                p_ptr.stat_cur[i] = 5
		p_ptr.stat_max[i] = 5
		
		p_ptr.stat_cnt[i] = 0
		p_ptr.stat_los[i] = 0
	end

	-- Start with some points to spend.
	p_ptr.statpoints = 2
	p_ptr.skillpoints = 10
	p_ptr.ability_points = 1
end

-- Gain experience(kills)
-- The amount of experience is now based on the level of the monster that you kill,
-- regardless of it's depth. Higher CR monsters are worth more experience. Individual
-- monsters can have an experience modifier to be worth more or less experience
-- than normal. Also, if you are several levels higher than the enemy, they will be
-- worth much less experience.
function gain_exp_kills (mon)

	local amount
	local i

	-- Basic experience gain.
	-- Basic gain equal to monster's level.
	-- Each levels increases the gains by 20% as well.
	-- Exponentially increases with CR.
	amount = mon.level

	-- If the monster is higher level than you, add a bonus.
	if (mon.level > p_ptr.lev) then

		amount = amount + (mon.level - p_ptr.lev)
	end

	amount = amount + multiply_divide(amount, amount * 20, 100)

	-- Exponential increase with CR.
	for i = 1, (m_race(mon.r_idx).cr-1) do

		amount = amount * 3
	end

	-- Monster's experience modifier.
	amount = multiply_divide(amount, m_race(mon.r_idx).mexp, 100)

	-- Make sure we gain 1 experience if the monster is equal or higher level.
	-- This shouldn't be an issue, except against monsters that are worht VERY LITTLE experience.
	-- Beyond depth 1, this won't happen much.
	if ((mon.level >= p_ptr.lev) and amount <= 0) then amount = 1 end

	-- If the monster is lower level than you, reduce the experience worth of the monster.
	-- It's based on how much levels it is lower than you, based on a percentile value.
	if (p_ptr.lev > mon.level) then

		local difference

		-- How much higher you are in terms of level.
		-- This gives a percentile value of this difference.
		difference = multiply_divide((p_ptr.lev - mon.level), 100, mon.level)

		-- The experience penalty is this difference * 2.
		difference = difference * 2

		-- So for example, if you are level 10 fighting a level 5 monster, you will not gain
		-- any experience, since you are 100% higher than the monster.

		-- If you are level 20 fighting a level 15 monster, your gains will be reduced by 66%.

		-- The penalty can rise fairly quickly. From a level 100 player perspective:
		-- MONSTER LEVEL     PENALTY
		-- 95                10%
		-- 90                22%
		-- 85                34%
		-- 80                50%
		-- 75                66%
		-- 70                84%
                -- 67                98%
		-- 66 or less        No experience gains.

		-- If the difference is too high, no experience gains.
		if (difference >= 100) then amount = 0
		else

			amount = amount - multiply_divide(amount, difference, 100)
		end
	end

	-- No experience gains in town.
	if (dun_level == 0) then amount = 0 end

	-- Reduced experience in quest levels for non-unique monsters.
	-- The exception is the final level of random dungeons(quest #9000).
	if ((not(p_ptr.inside_quest == 0) and not(p_ptr.inside_quest == 9000)) and not(get_monster_flag1(mon.r_idx, RF1_UNIQUE))) then

		-- For each kills of the monster, the gained experience is reduced by 5%.
		if (m_race(mon.r_idx).r_tkills >= 20) then

			amount = 0
		else
			amount = multiply_divide(amount, 100 - (m_race(mon.r_idx).r_tkills * 5), 100)
		end
	end

	-- Worth at least 1 experience at level 1.
	if (p_ptr.max_plv == 1 and amount < 1) then amount = 1 end

	-- Might not be worth experience(summoned monsters, multiplied enemies, etc...)
	if (mon.no_experience) then amount = 0 end

	-- Gain experience, along with a class kill.
	if (amount > 0) then

		local x = 0

		p_ptr.exp = p_ptr.exp + amount

		-- Add a class kill.
		if (p_ptr.class_kills[p_ptr.pclass+1] < 1000) then
			p_ptr.class_kills[p_ptr.pclass+1] = p_ptr.class_kills[p_ptr.pclass+1] + 1
		end

		-- May gain class level.
		gain_class_level()

		-- Recover from experience draining.
		if (p_ptr.exp < p_ptr.max_exp) then

			p_ptr.max_exp = p_ptr.max_exp + (amount / 5)
		end

		-- Items may also gain a kill here, if the kill is worth any experience.
		for x = INVEN_WIELD, INVEN_TOTAL do

                        -- Can the item gain levels ?
                        if (inven(x).tval > 0 and get_object_flag4(inven(x), TR4_LEVELS) and inven(x).level < p_ptr.lev) then

				inven(x).kills = inven(x).kills + 1

                                if((inven(x).kills >= (inven(x).level * 5)) and (inven(x).level < 200)) then

                                        -- Gain level.
                                        object_gain_level(inven(x))
                                end
                        end
		end
	end

	check_experience()
end

-- Function called by dialog scripts.
function dialog_script (scriptid)

	-- Simon appears!
	-- Used in Q504.txt, dialog d1510.
	if (scriptid == 1) then

		-- Delete anything that's at 4,62.
		delete_monster(4, 62)

		-- It is floor.
		cave_set_feat(4, 62, FEAT_FLOOR)

		place_monster_one_return(4, 62, 1342, FALSE, FALSE, 35, 0)
		update_and_handle()
	end

	-- Simon appears when fused with red licialhyd.
	-- This script will move the player, and possibly destroy any
	-- friendly monsters on that square.
	if (scriptid == 2) then

		-- Simon appears at 15,8 or 15,9 if the player is at 15,8.
		-- Any monsters that stands in the way will be destroyed.

		if (px == 15 and py == 8) then

			-- Delete anything that's at 9,15.
			delete_monster(9, 15)

			-- It is floor.
			cave_set_feat(9, 15, FEAT_FLOOR)

			place_monster_one_return(9, 15, 1347, FALSE, FALSE, 40, 0)
			update_and_handle()
		else
			-- Delete anything that's at 8,15.
			delete_monster(8, 15)

			-- It is floor.
			cave_set_feat(8, 15, FEAT_FLOOR)

			place_monster_one_return(8, 15, 1347, FALSE, FALSE, 40, 0)
			update_and_handle()
		end
	end

	-- Initiate trip to the Flow!
	if (scriptid == 3) then

		if (not(cave(30, 97).o_idx == 0) and not(cave(30, 101).o_idx == 0)) then

			if ((object(cave(30, 97).o_idx).tval == TV_LICIALHYD) and (object(cave(30, 101).o_idx).tval == TV_LICIALHYD)) then

				if (not(cave(30, 99).o_idx == 0)) then

					if (object(cave(30, 99).o_idx).xtra1 > 0) then

						show_dialog(40)
					else

						local value1
						local value2

						value1 = object_value_real(object(cave(30, 97).o_idx))
						value2 = object_value_real(object(cave(30, 101).o_idx))

						if ((value1 + value2) >= object_value_real(object(cave(30, 99).o_idx))) then

							global_object = object(cave(30, 99).o_idx)

							if (get_object_flag4(global_object, TR4_ENCHANTED) or get_object_flag4(global_object, TR4_CRAFTED) or (global_object.name1 > 0) or (global_object.name2 > 0) or (global_object.tval == TV_ESSENCE)) then

								if (not(object(cave(30, 97).o_idx).pval2 == -1 and object(cave(30, 101).o_idx).pval2 == -1)) then
									if (object(cave(30, 97).o_idx).pval2 == -1 or object(cave(30, 101).o_idx).pval2 == -1) then

										msg_print("The normal Licialhyd shatters!")
									else
										msg_print("The Licialhyds shatter!")
									end
								end
								if (not(object(cave(30, 97).o_idx).pval2 == -1)) then delete_object(30, 97) end
								if (not(object(cave(30, 101).o_idx).pval2 == -1)) then delete_object(30, 101) end
							end

							delete_object(30, 99)
							cave_set_feat(30, 99, 235)
							show_dialog(37)

							-- Tia's gloves quest.
							if (global_object.tval == TV_GLOVES and global_object.sval == 14) then
								p_ptr.events[95] = 1
							end
						else
							show_dialog(39)
						end
					end
				else
					show_dialog(36)
				end
			else
				show_dialog(36)
			end
		else
			show_dialog(36)
		end
	end

	-- Exit the flow.
	if (scriptid == 4) then
		dun_level = 0

		give_object_flag1(global_object, TR1_ENCHANTED)

		p_ptr.inside_quest = 0

		p_ptr.events[29035] = 1

		p_ptr.startx = 99
		p_ptr.starty = 33

		p_ptr.leaving = TRUE
	end

	-- Tia's imbuing service!
	if (scriptid == 5) then

		local weapon
		local potion

		-- Assume failure.
		p_ptr.events[96] = 0

		-- Pick a weapon.
		weapon = lua_pick_item(0)

		if (not(weapon) or not(weapon.tval == TV_WEAPON or weapon.tval == TV_RANGED or weapon.tval == TV_AMMO or weapon.tval == TV_GLOVES or weapon.tval == TV_THROWING)) then

			msg_print("Invalid weapon.")
			msg_print(NULL)
			return
		end

		-- Pick a potion.
		msg_print(NULL)
		msg_print("Choose a potion.")
		potion = lua_get_item(TV_POTION)

		if (inven(potion).tval <= 0) then

			msg_print("Invalid potion.")
			msg_print(NULL)
			return
		end

		-- Weapon must be mundane.
		if (get_object_flag4(weapon, TR4_ENCHANTED) or get_object_flag4(weapon, TR4_CRAFTED) or weapon.name1 > 0) then

			msg_print("Only mundane weapons may be used.")
			msg_print(NULL)
			return
		end

		-- Change the weapon's elemental type to the potion's brand!
		weapon.extra1 = inven(potion).brandtype

		inven_item_increase(potion, -1)
        	inven_item_describe(potion)
        	inven_item_optimize(potion)

		-- Success!
		p_ptr.events[96] = 1
	end

	-- Cleo's misfortune transferring.
	if (scriptid == 6) then

		local item
		local magicitem
		local percentbonus
		local totalmisfortune
		local i

		-- Assume failure.
		p_ptr.events[96] = 0

		-- Pick an item.
		msg_print("Choose a mundane item.")
		item = lua_pick_item(0)

		-- Item must exists.
		if (not(item)) then

			msg_print("Invalid item.")
			msg_print(NULL)
			return
		end

		-- Item must be mundane.
		if (get_object_flag4(item, TR4_ENCHANTED) or get_object_flag4(item, TR4_CRAFTED) or item.name1 > 0) then

			msg_print("Only mundane items may be used.")
			msg_print(NULL)
			return
		end

		-- Pick a magic item.
		msg_print(NULL)
		msg_print("Choose a second item.")
		magicitem = lua_get_item(0)

		if (inven(magicitem).tval <= 0) then

			msg_print("Invalid item.")
			msg_print(NULL)
			return
		end

		if (inven(magicitem).cursed > 0) then

			msg_print("Second item cannot be cursed.")
			msg_print(NULL)
			return
		end

		-- Begin transfer!
		totalmisfortune = 0
		-- First, apply some damages and bonus.
		if (inven(magicitem).dd > 0) then
			percentbonus = ((inven(magicitem).dd / kind(inven(magicitem)).dd) * 100)
			if (percentbonus > 100) then
				item.dd = multiply_divide(item.dd, percentbonus, 100)
				totalmisfortune = totalmisfortune + ((percentbonus - 100) / 3)
			end
		end
		if (inven(magicitem).ds > 0) then
			percentbonus = ((inven(magicitem).ds / kind(inven(magicitem)).ds) * 100)
			if (percentbonus > 100) then
				item.ds = multiply_divide(item.ds, percentbonus, 100)
				totalmisfortune = totalmisfortune + ((percentbonus - 100) / 3)
			end
		end
		if (inven(magicitem).ac > 0) then
			percentbonus = ((inven(magicitem).ac / kind(inven(magicitem)).ac) * 100)
			if (percentbonus > 100) then
				item.ac = multiply_divide(item.ac, percentbonus, 100)
				totalmisfortune = totalmisfortune + ((percentbonus - 100) / 3)
			end
		end

		item.to_h = item.to_h + inven(magicitem).to_h
		item.to_d = item.to_d + inven(magicitem).to_d
		item.to_a = item.to_a + inven(magicitem).to_a
		item.extrablows = item.extrablows + inven(magicitem).extrablows
		item.extrashots = item.extrashots + inven(magicitem).extrashots
		item.speedbonus = item.speedbonus + inven(magicitem).speedbonus
		item.lifebonus = item.lifebonus + inven(magicitem).lifebonus
		item.manabonus = item.manabonus + inven(magicitem).manabonus
		item.infravision = item.infravision + inven(magicitem).infravision
		item.spellbonus = item.spellbonus + inven(magicitem).spellbonus
		item.invisibility = item.invisibility + inven(magicitem).invisibility
		item.light = item.light + inven(magicitem).light
		if (item.light > 5) then item.light = 5 end
		item.reflect = item.reflect + inven(magicitem).reflect
		item.tweakpoints = item.tweakpoints + inven(magicitem).tweakpoints
		item.brandtype = inven(magicitem).brandtype
		item.branddam = inven(magicitem).branddam
		item.brandrad = inven(magicitem).brandrad

		if (get_object_flag4(inven(magicitem), TR4_ETERNAL)) then give_object_flag4(item, TR4_ETERNAL) end
		if (get_object_flag4(inven(magicitem), TR4_LEVELS)) then give_object_flag4(item, TR4_LEVELS) end
		if (get_object_flag4(inven(magicitem), TR4_FLY)) then give_object_flag4(item, TR4_FLY) end
		if (get_object_flag4(inven(magicitem), TR4_CLIMB)) then give_object_flag4(item, TR4_CLIMB) end
		if (get_object_flag2(inven(magicitem), TR2_FREE_ACT)) then give_object_flag2(item, TR2_FREE_ACT) end
		if (get_object_flag3(inven(magicitem), TR3_TELEPATHY)) then give_object_flag3(item, TR3_TELEPATHY) end
		if (get_object_flag2(inven(magicitem), TR2_SUST_STR)) then give_object_flag2(item, TR2_SUST_STR) end
		if (get_object_flag2(inven(magicitem), TR2_SUST_INT)) then give_object_flag2(item, TR2_SUST_INT) end
		if (get_object_flag2(inven(magicitem), TR2_SUST_WIS)) then give_object_flag2(item, TR2_SUST_WIS) end
		if (get_object_flag2(inven(magicitem), TR2_SUST_DEX)) then give_object_flag2(item, TR2_SUST_DEX) end
		if (get_object_flag2(inven(magicitem), TR2_SUST_CON)) then give_object_flag2(item, TR2_SUST_CON) end
		if (get_object_flag2(inven(magicitem), TR2_SUST_CHR)) then give_object_flag2(item, TR2_SUST_CHR) end
		if (get_object_flag2(inven(magicitem), TR2_RES_CONF)) then give_object_flag2(item, TR2_RES_CONF) end
		if (get_object_flag2(inven(magicitem), TR2_RES_FEAR)) then give_object_flag2(item, TR2_RES_FEAR) end
		if (get_object_flag2(inven(magicitem), TR2_RES_BLIND)) then give_object_flag2(item, TR2_RES_BLIND) end
		if (get_object_flag2(inven(magicitem), TR2_HOLD_LIFE)) then give_object_flag2(item, TR2_HOLD_LIFE) end
		if (get_object_flag3(inven(magicitem), TR3_REGEN)) then give_object_flag3(item, TR3_REGEN) end
		if (get_object_flag1(inven(magicitem), TR1_SLAY_ANIMAL)) then give_object_flag1(item, TR1_SLAY_ANIMAL) end
		if (get_object_flag1(inven(magicitem), TR1_SLAY_EVIL)) then give_object_flag1(item, TR1_SLAY_EVIL) end
		if (get_object_flag1(inven(magicitem), TR1_SLAY_UNDEAD)) then give_object_flag1(item, TR1_SLAY_UNDEAD) end
		if (get_object_flag1(inven(magicitem), TR1_SLAY_DEMON)) then give_object_flag1(item, TR1_SLAY_DEMON) end
		if (get_object_flag1(inven(magicitem), TR1_SLAY_ORC)) then give_object_flag1(item, TR1_SLAY_ORC) end
		if (get_object_flag1(inven(magicitem), TR1_SLAY_TROLL)) then give_object_flag1(item, TR1_SLAY_TROLL) end
		if (get_object_flag1(inven(magicitem), TR1_SLAY_GIANT)) then give_object_flag1(item, TR1_SLAY_GIANT) end
		if (get_object_flag1(inven(magicitem), TR1_SLAY_DRAGON)) then give_object_flag1(item, TR1_SLAY_DRAGON) end
		if (get_object_flag1(inven(magicitem), TR1_KILL_DRAGON)) then give_object_flag1(item, TR1_KILL_DRAGON) end

		if (get_object_flag3(inven(magicitem), TR3_ACTIVATE)) then
			give_object_flag3(item, TR3_ACTIVATE)

			for i = 1, 20 do

				item.spell[i].name = inven(magicitem).spell[i].name
				item.spell[i].act = inven(magicitem).spell[i].act
				item.spell[i].type = inven(magicitem).spell[i].type
				item.spell[i].power = inven(magicitem).spell[i].power
				item.spell[i].special1 = inven(magicitem).spell[i].special1
				item.spell[i].special2 = inven(magicitem).spell[i].special2
				item.spell[i].special3 = inven(magicitem).spell[i].special3
				item.spell[i].summchar = inven(magicitem).spell[i].summchar
				item.spell[i].cost = inven(magicitem).spell[i].cost
				
				totalmisfortune = totalmisfortune + (inven(magicitem).spell[i].power / 10)
			end
		end

		for i = 1, 6 do

			item.statsbonus[i] = inven(magicitem).statsbonus[i]
		end

		for i = 1, SKILL_MAX do

			item.skillsbonus[i] = inven(magicitem).skillsbonus[i]
		end

		for i = 1, MAX_RESIST do

			item.resistances[i] = inven(magicitem).resistances[i]
		end

		totalmisfortune = totalmisfortune + (object_skill_points_value(inven(magicitem)) / 2)
		if (totalmisfortune < 10) then totalmisfortune = 10 end

		item.cursed = totalmisfortune

		give_object_flag4(item, TR4_ENCHANTED)

		inven_item_increase(magicitem, -1)
        	inven_item_describe(magicitem)
        	inven_item_optimize(magicitem)

		-- Success!
		p_ptr.events[96] = 1
	end

	-- Gary's dragon armors crafting.
	if (scriptid == 7) then

		local scale
		local armortval
		local armorsval
		local price
		local ch

		-- Assume failure.
		p_ptr.events[1527] = 0

		-- Initialize variables.
		armortval = 0
		armorsval = 0
		price = 0

		-- Pick a scale.
		scale = lua_get_item(4)

		if (not(inven(scale))) then

			msg_print(NULL)
			return
		end

		-- Determine the type of armors we're going to get.
		if (inven(scale).sval == 7) then
			armortval = TV_DRAG_ARMOR
			armorsval = 4
			price = 30000
		elseif (inven(scale).sval == 8) then
			armortval = TV_DRAG_ARMOR
			armorsval = 3
			price = 30000
		elseif (inven(scale).sval == 9) then
			armortval = TV_DRAG_ARMOR
			armorsval = 2
			price = 30000
		elseif (inven(scale).sval == 10) then
			armortval = TV_DRAG_ARMOR
			armorsval = 1
			price = 30000
		elseif (inven(scale).sval == 11) then
			armortval = TV_DRAG_ARMOR
			armorsval = 5
			price = 30000
		elseif (inven(scale).sval == 12) then
			armortval = TV_DRAG_ARMOR
			armorsval = 6
			price = 30000
		elseif (inven(scale).sval == 13) then
			armortval = TV_DRAG_ARMOR
			armorsval = 7
			price = 200000
		elseif (inven(scale).sval == 41) then
			armortval = TV_DRAG_ARMOR
			armorsval = 8
			price = 40000
		elseif (inven(scale).sval == 43) then
			armortval = TV_SOFT_ARMOR
			armorsval = 13
			price = 50000
		elseif (inven(scale).sval == 44) then
			armortval = TV_SOFT_ARMOR
			armorsval = 14
			price = 50000
		elseif (inven(scale).sval == 45) then
			armortval = TV_SOFT_ARMOR
			armorsval = 15
			price = 50000
		elseif (inven(scale).sval == 46) then
			armortval = TV_SOFT_ARMOR
			armorsval = 16
			price = 1000000
		elseif (inven(scale).sval == 47) then
			armortval = TV_SOFT_ARMOR
			armorsval = 17
			price = 60000
		else
			msg_print("Not a dragon scale, or cannot make armor out of it.")
			msg_print(NULL)
			return
		end

		-- Prompt
  		msg_print(string.format('This armor will cost %d golds. Still interested? [y/n]', price))

		ch = inkey()

		-- Characters 89 and 121 are "Y" and "y"
                if (ch == 89 or ch == 121) then

			if (p_ptr.au >= price) then

				p_ptr.au = p_ptr.au - price
				lua_create_object_inven(armortval, armorsval, 1)
				p_ptr.events[1527] = 1
				inven_item_increase(scale+1, -1)
        			inven_item_describe(scale+1)
        			inven_item_optimize(scale+1)
				update_and_handle()
			else
				msg_print("Not enough money.")
				msg_print(NULL)
				return
			end
                else
			msg_print(NULL)
			return
		end
	end

	-- Eliminate all monsters.
	if (scriptid == 8) then

		anihilate_monsters()
	end

	-- Delviaz appears in Wraith form. Based on Simon's code above.
	if (scriptid == 9) then

		-- It appears at 10,7 or 9,7 if the player is at 10,7.
		-- Any monsters that stands in the way will be destroyed.

		if (px == 10 and py == 7) then

			-- Delete anything that's at 7,9.
			delete_monster(7, 9)

			-- It is floor.
			cave_set_feat(7, 9, FEAT_FLOOR)

			place_monster_one_return(7, 9, 1643, FALSE, FALSE, 45, 0)
			update_and_handle()
		else
			-- Delete anything that's at 7,10.
			delete_monster(7, 10)

			-- It is floor.
			cave_set_feat(7, 10, FEAT_FLOOR)

			place_monster_one_return(7, 10, 1643, FALSE, FALSE, 45, 0)
			update_and_handle()
		end
	end

end

-- Function called when using scripted spells/activations.
function activate_spell_script (powernum)

  -- Ally of Kobolds(Viper Cloak of the Viper Champion)
  if (powernum == 1) then

  	local i
	msg_print("You project your influence over all Kobolds in the area!")

	-- m_max is the number of monsters on a given level.
	for i = 1, (m_max - 1) do
		if ((m_race(monster(i).r_idx).d_char == 107) and (m_race(monster(i).r_idx).cr == 1) and not(get_monster_flag1(monster(i).r_idx, RF1_UNIQUE)) and monster(i).level <= p_ptr.lev) then
				
			set_pet(monster(i), TRUE)
		end
	end

    	energy_use = 100
  end

  -- Dragon's Fists Arfiacts activation.
  if (powernum == 2) then

  	local dam
	local dir

	if (not(unarmed())) then
		msg_print("You must be unarmed to use this power.")
		return
	end

	dir = lua_get_rep_dir()

	dam = monk_damages()

	chain_attack(dir, GF_FIRE, dam, 0, 30)

	energy_use = 100
  end

  -- Steel Long Bow 'Solar Beam' activation.
  if (powernum == 7) then

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

	-- This bow should be two-handed.
	if (inven(INVEN_WIELD).tval == TV_RANGED and inven(INVEN_WIELD).name1 == 29) then
		current_weapon = inven(INVEN_WIELD)
	else
		current_weapon = inven(INVEN_WIELD+1)
	end

	-- Make sure we have the proper type and number of ammos.
	if (not(current_weapon.itemtype == inven(INVEN_AMMO).itemtype)) then
		
		msg_print("You must use the proper type of ammos.")
		return
	end

	-- We need to choose a direction to attack.
	dir = lua_get_aim_dir()

	totalammos = current_weapon.extra2
	drop_ranged = inven(INVEN_AMMO)
	if (inven(INVEN_AMMO).number < totalammos) then

		msg_print("You need more ammos!")
		return
	end

	if (current_weapon.pval2 < totalammos) then

		msg_print("This weapon needs to be reloaded!")
		return
	end

	dam = ranged_damages() * 2

	-- Element is always Light.
	element = GF_LITE

	-- Radius is 10.
	rad = 10

	-- Shoot!
	fire_ball(element, dir, dam, rad)

    	energy_use = 100
  end

  if (powernum == 10) then
  	set_stun(0)
        set_poisoned(0)
        set_confused(0)
        set_paralyzed(0)
        set_blind(0)
        set_afraid(0)
	do_res_stat(A_STR)
        do_res_stat(A_INT)
        do_res_stat(A_WIS)
        do_res_stat(A_DEX)
        do_res_stat(A_CON)
        do_res_stat(A_CHR)
        restore_level()
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

  -- Ring of the Tortoise activation.
  if (powernum == 11) then
	if (p_ptr.ac_boost_dur == 0) then
		p_ptr.ac_boost = p_ptr.to_a * 2
        	set_ac_boost(10)
		energy_use = 100
	else
		msg_print("You cannot use this power while your AC is already enhanced.")
	end
  end

  -- Ring of the Cat activation.
  if (powernum == 12) then

	msg_print("You jump high!")
        if (not(lua_tgt_pt())) then return end
	x = global_x
	y = global_y

	-- Most functions here use a (y,x) format, instead of (x,y).
	-- This is important, because if you use (x,y), it might crash.
        if (not(lua_cave_empty_bold(y,x)) or (distance(y,x,py,px) > 4)) then

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

	energy_use = 100
  end

  -- Ring of the Dwarves.
  if (powernum == 13) then

	map_area()
	energy_use = 100
  end

  -- Ring of Lore.
  if (powernum == 14) then

	identify_fully()
	energy_use = 100
  end

  -- Footpad's Boots.
  if (powernum == 15) then

	set_invis(10, 30)
	energy_use = 100
  end

  -- Tolgo Drumset.
  if (powernum == 16) then

	place_field(FEAT_TREES, 1, px, py, 500)
	energy_use = 100
  end

end

-- Some "Flow" dungeons scripts --

-- Used in Q9001.txt
function flow_last_floor ()

	local x
	local y

	dun_level = p_ptr.events[29034]

	-- Generate the random Flow Boss! :)
	-- For depths below 20, the Flow Boss is nothing too difficult.
	-- For depths 20-39, it's more difficult.
	-- For 40+....beware.
	if (p_ptr.events[29034] <= 20) then generate_monster(2098, p_ptr.events[29034], 2)
	elseif (p_ptr.events[29034] <= 40) then generate_monster(2098, p_ptr.events[29034] + (kind(global_object).level / 10) + 2, 2)
	else generate_monster(2098, p_ptr.events[29034] + (kind(global_object).level / 4) + 5, 2) end

	-- Place it!
	place_monster_one_return(5, 15, 2098, FALSE, FALSE, p_ptr.events[29034], 0)

	-- Enlight everything!
	for y = 0, 20 do

		for x = 0, 30 do

			lua_cave_mark(y, x, CAVE_LITE)
			lua_cave_mark(y, x, CAVE_MARK)
		end
	end
end

-- Sometimes, we generate a special level instead of a random, messy one. ;)
function flow_special_level ()

	local special

	dun_level = p_ptr.events[29034]

	special = lua_randint(3)

	-- 1. The mysterious shop number 9! :)
	if (special == 1) then
	
		local x
		local y
		local rad

		-- It's lost in the void...
		for y = 23, 42 do

			for x = 67, 130 do

				cave_set_feat(y, x, 87)
			end
		end

		-- Generate a floor around the player.
		rad = lua_randint(3) + 1

		for y = (py - rad), (py + rad) do

			for x = (px - rad), (px + rad) do

				cave_set_feat(y, x, FEAT_FLOOR)
			end
		end

		-- Place the shop.
		for y = ((py - (rad * 2)) - rad - 1), ((py - (rad * 2)) + rad + 1) do

			for x = (px - rad - 1), (px + rad + 1) do

				cave_set_feat(y, x, FEAT_FLOOR)
			end
		end
		for y = ((py - (rad * 2)) - rad), ((py - (rad * 2)) + rad) do

			for x = (px - rad), (px + rad) do

				cave_set_feat(y, x, FEAT_PERM_SOLID)
				if (y == ((py - (rad * 2)) + rad) and (x == px)) then

					cave_set_feat(y, x, 212)
				end
			end
		end

		-- Place a Portal right behind the player.
		cave_set_feat(py+1, px, 241)
	end

	-- Random boss.
	if (special == 2) then

		local x
		local y
		local rad

		-- Reset event 29999.
		p_ptr.events[30000] = 0

		-- It's lost in the void...
		for y = 23, 42 do

			for x = 67, 130 do

				cave_set_feat(y, x, 87)
			end
		end

		-- Generate a floor around the player.
		rad = 7

		for y = (py - rad), (py + rad) do

			for x = (px - rad), (px + rad) do

				cave_set_feat(y, x, FEAT_FLOOR)
			end
		end

		-- Place the portal and door.
		for y = ((py - (rad-1)) - 1), (((py - (rad-1)) - 1)+2) do

			for x = (px - 1), (px + 1) do

				cave_set_feat(y, x, FEAT_PERM_SOLID)
				if (y == (((py - (rad-1)) - 1)+1) and x == px) then

					cave_set_feat(y, x, 241)
				end
				if (y == (((py - (rad-1)) - 1)+2) and x == px) then

					cave_set_feat(y, x, 32)
					cave(y, x).event = 3
					cave(y, x).eventtype = 1
					cave(y, x).eventextra = 999
					cave(y, x).eventextra2 = 0
					cave(y, x).eventcond = 29999
					cave(y, x).eventcondval = 0
					cave(y, x).eventset = 0
					cave(y, x).eventsetval = 0
				end
			end
		end

		-- Generate a random boss.
		generate_monster(1030, p_ptr.events[29034], 1)

		-- Place it.
		place_monster_one_return(py - 4, px, 1030, FALSE, FALSE, p_ptr.events[29034] + (p_ptr.events[29034] / 2), 0)
	end

	-- 3. Vanilla-style town!
	if (special == 3) then

		local x
		local y
		local i
		local j
		local buildings
		local placed
		
		local swid
		local shgt
		local shopslist = {}

		shopslist[1] = 204
		shopslist[2] = 205
		shopslist[3] = 206
		shopslist[4] = 207
		shopslist[5] = 208
		shopslist[6] = 209
		shopslist[7] = 210
		shopslist[8] = 211

		-- Reminder:
		-- It all starts at 67, 23

		-- Generate the first row.
		y = 27
		x = 76

		for buildings = 1, 4 do

			local doorside
			local doorplaced
			local chosen

			swid = lua_randint(5) + 2
			shgt = lua_randint(3) + 2

			-- 1. Top
			-- 2. Bottom
			-- 3. Left
			-- 4. Right
			doorside = lua_randint(4)
			doorplaced = 0

			for i = (y - (shgt / 2)), (y + (shgt / 2)) do

				for j = (x - (swid / 2)), (x + (swid / 2)) do

					cave_set_feat(i, j, FEAT_PERM_SOLID)
				end

			end

			-- Place the door.
			chosen = 0
			while (chosen == 0) do

				local which
				which = lua_randint(8)
				if (not(shopslist[which] == 0)) then

					chosen = shopslist[which]
					shopslist[which] = 0
				end
			end

			if (doorside == 1) then

				local spot
				spot = lua_randint(swid-1)+1

				cave_set_feat(y - (shgt / 2), (x - (swid / 2))+spot-1, chosen)
			end
			if (doorside == 2) then

				local spot
				spot = lua_randint(swid-1)+1

				cave_set_feat(y + (shgt / 2), (x - (swid / 2))+spot-1, chosen)
			end
			if (doorside == 3) then

				local spot
				spot = lua_randint(shgt-1)+1

				cave_set_feat((y - (shgt / 2))+spot-1, (x - (swid / 2)), chosen)
			end
			if (doorside == 4) then

				local spot
				spot = lua_randint(shgt-1)+1

				cave_set_feat((y - (shgt / 2))+spot-1, (x + (swid / 2)), chosen)
			end

			x = x + 15
		end

		-- Generate the second row.
		y = 38
		x = 76

		for buildings = 1, 4 do

			local doorside
			local doorplaced
			local chosen

			swid = lua_randint(5) + 2
			shgt = lua_randint(3) + 2

			-- 1. Top
			-- 2. Bottom
			-- 3. Left
			-- 4. Right
			doorside = lua_randint(4)
			doorplaced = 0

			for i = (y - (shgt / 2)), (y + (shgt / 2)) do

				for j = (x - (swid / 2)), (x + (swid / 2)) do

					cave_set_feat(i, j, FEAT_PERM_SOLID)
				end

			end

			-- Place the door.
			chosen = 0
			while (chosen == 0) do

				local which
				which = lua_randint(8)
				if (not(shopslist[which] == 0)) then

					chosen = shopslist[which]
					shopslist[which] = 0
				end
			end

			if (doorside == 1) then

				local spot
				spot = lua_randint(swid-1)+1

				cave_set_feat(y - (shgt / 2), (x - (swid / 2))+spot-1, chosen)
			end
			if (doorside == 2) then

				local spot
				spot = lua_randint(swid-1)+1

				cave_set_feat(y + (shgt / 2), (x - (swid / 2))+spot-1, chosen)
			end
			if (doorside == 3) then

				local spot
				spot = lua_randint(shgt-1)+1

				cave_set_feat((y - (shgt / 2))+spot-1, (x - (swid / 2)), chosen)
			end
			if (doorside == 4) then

				local spot
				spot = lua_randint(shgt-1)+1

				cave_set_feat((y - (shgt / 2))+spot-1, (x + (swid / 2)), chosen)
			end

			x = x + 15
		end

		-- Place a portal.
		cave_set_feat(py, px, 241)

		-- Generate some townsfolks! :)
		for i = 2050, 2055 do
			generate_monster(i, 1, 3)
			p_ptr.events[29036+(i-2050)] = 1
		end

		-- Place them.
		placed = 0
		while (placed < 5) do
			x = lua_randint(cur_wid-1)
			y = lua_randint(cur_hgt-1)

			if (x <= 0) then x = 1 end
			if (y <= 0) then y = 1 end

			if (cave(y, x).feat == FEAT_FLOOR or cave(y, x).feat == FEAT_GRASS or cave(y, x).feat == FEAT_SNOW or cave(y, x).feat == FEAT_SHAL_WATER or cave(y, x).feat == FEAT_SHAL_LAVA or cave(y, x).feat == FEAT_DEEP_WATER or cave(y, x).feat == FEAT_DEEP_LAVA or cave(y, x).feat == FEAT_DIRT) then

				place_monster_one_return(y, x, 2050+placed, FALSE, FALSE, 1, 0)
				placed = placed + 1
			end
		end

		-- Enlight everything!
		for y = 22, 43 do

			for x = 66, 131 do

				lua_cave_mark(y, x, CAVE_LITE)
				lua_cave_mark(y, x, CAVE_MARK)
			end
		end
		
	end
end

-- Used in T16.txt
function enter_flow_temple_1 ()

	p_ptr.recall_dungeon = 0
	dungeon_type = 0
	if (p_ptr.events[29035] == 1) then

		-- Tia's gloves quest.
		if (global_object.tval == TV_GLOVES and global_object.sval == 14) then

			quest_artifact_prep(32, 99, 30)
		else
			drop_global_object(99, 30)
		end
		p_ptr.events[29035] = 0
	end
end

------ Mining Ruins Scripts ------

function generate_random_ruin ()

	local roomrad
	local rooms
	local roomwid
	local roomhgt
	local roomcnt
	local x
	local y
	local i
	local j
	local okay
	local dir

	-- Total number of rooms in the ruin.
	-- From 3 to 10.
	rooms = lua_randint(8) + 2

	-- Place the player somewhere else than 1,1. (1,1 can still happen if you're lucky)
	x = lua_randint(127) + 20
	y = lua_randint(25) + 10

	player_place(y, x)

	-- Make a starting room.

	-- Determine radius.
	-- It can be 0.
	okay = 0
	while (okay == 0) do
		roomrad = lua_randint(12) - 1
		if (lua_inbounds(px, py, 198, 66, (roomrad / 2), (roomrad / 2), (roomrad / 2), (roomrad / 2))) then okay = 1 end
	end

	-- Build it.
	for i = (py - (roomrad / 2)), (py + (roomrad / 2)) do

		for j = (px - (roomrad / 2)), (px + (roomrad / 2)) do

			cave_set_feat(i, j, FEAT_FLOOR)
		end
	end

	roomcnt = 0
	x = px
	y = py

	-- From the starting room, build the other rooms.
	while (roomcnt < rooms) do

		local backdir
		backdir = 0
		
		-- Pick a direction.
		-- We will use 2, 4, 6 and 8 to facilitate things.
		dir = 0
		while (dir == backdir) do
			dir = lua_randint(4)
			if (dir == 1) then dir = 6 end
			if (dir == 3) then dir = 8 end
		end

		-- We want to avoid getting backward.
		if (dir == 2) then backdir = 8 end
		if (dir == 4) then backdir = 6 end
		if (dir == 6) then backdir = 4 end
		if (dir == 8) then backdir = 2 end
		
		-- Go in that direction, and make a room.
		if (dir == 4) then

			local num
			num = lua_randint(5) + 5

			-- Find a wall.
			while not(cave(y, x).feat == FEAT_PERM_SOLID) do
				x = x - 1
			end
			
			-- If we've reached the boundaries, stop looping.
			if (x <= 0 or cave(y, x).event == -1) then break end

			i = 0
			while ((i < num) and (x > 0)) do
				cave_set_feat(y, x, FEAT_FLOOR)
				x = x - 1
				i = i + 1
			end

			-- If we've reached the boundaries, stop looping.
			if (x <= 0) then break end

			-- Otherwise, create a room.
			okay = 0
			while (okay == 0) do
				roomwid = lua_randint(22) - 1
				roomhgt = lua_randint(12) - 1
				if (lua_inbounds(x, y, 198, 66, (roomwid / 2), (roomhgt / 2), (roomwid / 2), (roomhgt / 2))) then okay = 1 end
			end

			-- Build it.
			for i = (y - (roomhgt / 2)), (y + (roomhgt / 2)) do

				for j = (x - (roomwid / 2)), (x + (roomwid / 2)) do

					cave_set_feat(i, j, FEAT_FLOOR)
				end
			end

			-- Put something in it.
			populate_ruin_room (x, y, roomwid, roomhgt)
			
			-- Room is done!
			roomcnt = roomcnt + 1
		end

		-- Go in that direction, and make a room.
		if (dir == 6) then

			local num
			num = lua_randint(5) + 5

			-- Find a wall.
			while not(cave(y, x).feat == FEAT_PERM_SOLID) do
				x = x + 1
			end
			
			-- If we've reached the boundaries, stop looping.
			if (x >= 197 or cave(y, x).event == -1) then break end

			i = 0
			while ((i < num) and (x < 197)) do
				cave_set_feat(y, x, FEAT_FLOOR)
				x = x + 1
				i = i + 1
			end

			-- If we've reached the boundaries, stop looping.
			if (x >= 197) then break end

			-- Otherwise, create a room.
			okay = 0
			while (okay == 0) do
				roomwid = lua_randint(22) - 1
				roomhgt = lua_randint(12) - 1
				if (lua_inbounds(x, y, 198, 66, (roomwid / 2), (roomhgt / 2), (roomwid / 2), (roomhgt / 2))) then okay = 1 end
			end

			-- Build it.
			for i = (y - (roomhgt / 2)), (y + (roomhgt / 2)) do

				for j = (x - (roomwid / 2)), (x + (roomwid / 2)) do

					cave_set_feat(i, j, FEAT_FLOOR)
				end
			end

			-- Put something in it.
			populate_ruin_room (x, y, roomwid, roomhgt)
			
			-- Room is done!
			roomcnt = roomcnt + 1
		end

		-- Go in that direction, and make a room.
		if (dir == 2) then

			local num
			num = lua_randint(5) + 5

			-- Find a wall.
			while not(cave(y, x).feat == FEAT_PERM_SOLID) do
				y = y + 1
			end
			
			-- If we've reached the boundaries, stop looping.
			if (y >= 65 or cave(y, x).event == -1) then break end

			i = 0
			while ((i < num) and (y < 65)) do
				cave_set_feat(y, x, FEAT_FLOOR)
				y = y + 1
				i = i + 1
			end

			-- If we've reached the boundaries, stop looping.
			if (y >= 65) then break end

			-- Otherwise, create a room.
			okay = 0
			while (okay == 0) do
				roomwid = lua_randint(22) - 1
				roomhgt = lua_randint(12) - 1
				if (lua_inbounds(x, y, 198, 66, (roomwid / 2), (roomhgt / 2), (roomwid / 2), (roomhgt / 2))) then okay = 1 end
			end

			-- Build it.
			for i = (y - (roomhgt / 2)), (y + (roomhgt / 2)) do

				for j = (x - (roomwid / 2)), (x + (roomwid / 2)) do

					cave_set_feat(i, j, FEAT_FLOOR)
				end
			end

			-- Put something in it.
			populate_ruin_room (x, y, roomwid, roomhgt)
			
			-- Room is done!
			roomcnt = roomcnt + 1
		end

		-- Go in that direction, and make a room.
		if (dir == 8) then

			local num
			num = lua_randint(5) + 5

			-- Find a wall.
			while not(cave(y, x).feat == FEAT_PERM_SOLID) do
				y = y - 1
			end
			
			-- If we've reached the boundaries, stop looping.
			if (y <= 0 or cave(y, x).event == -1) then break end

			i = 0
			while ((i < num) and (y < 66)) do
				cave_set_feat(y, x, FEAT_FLOOR)
				y = y - 1
				i = i + 1
			end

			-- If we've reached the boundaries, stop looping.
			if (y <= 0) then break end

			-- Otherwise, create a room.
			okay = 0
			while (okay == 0) do
				roomwid = lua_randint(22) - 1
				roomhgt = lua_randint(12) - 1
				if (lua_inbounds(x, y, 198, 66, (roomwid / 2), (roomhgt / 2), (roomwid / 2), (roomhgt / 2))) then okay = 1 end
			end

			-- Build it.
			for i = (y - (roomhgt / 2)), (y + (roomhgt / 2)) do

				for j = (x - (roomwid / 2)), (x + (roomwid / 2)) do

					cave_set_feat(i, j, FEAT_FLOOR)
				end
			end

			-- Put something in it.
			populate_ruin_room (x, y, roomwid, roomhgt)
			
			-- Room is done!
			roomcnt = roomcnt + 1
		end
	end

	-- Place a stair up.
	cave_set_feat(py, px, 6)
end

-- Actually put something in those rooms.
-- The bigger, the more stuff! (maybe)
function populate_ruin_room (centerx, centery, wid, hgt)

	local poptype
	local okay
	local i
	local j

	i = 0
	j = 0

	okay = 0
	while (okay == 0) do

		-- Roll for a 'pop' type.
		-- Depending on the room's size, some may not be available.
		-- Sometimes, there's nothing.
		poptype = lua_randint(4) - 1

		-- Undead monsters.
		-- This is an ancient ruin after all, so undeads are to be expected.
		if (poptype == 1) then

			local maxnum
			local num

			-- Maximum number of monsters.
			maxnum = (wid * hgt)

			-- No more than 8 per rooms.
			if (maxnum > 8) then maxnum = 8 end

			-- Number of monsters to place.
			num = lua_randint(maxnum)

			i = 0
			while (i < num) do

				local placex
				local placey

				-- Attempt to place the monster.
				if (lua_randint(100) >= 51) then
					placex = centerx - lua_randint(wid / 2)
				else
					placex = centerx + lua_randint(wid / 2)
				end
				if (lua_randint(100) >= 51) then
					placey = centery - lua_randint(hgt / 2)
				else
					placey = centery + lua_randint(hgt / 2)
				end

				p_ptr.events[29022] = 1
				place_monster(placey, placex, FALSE, FALSE, 0)
				p_ptr.events[29022] = 0
				i = i + 1
			end

			-- Done.
			okay = 1
		end

		-- Random objects.
		if (poptype == 2) then

			local maxnum
			local num

			-- Maximum number of objects.
			maxnum = (wid * hgt)

			-- No more than 8 per rooms.
			if (maxnum > 8) then maxnum = 8 end

			-- Number of objects to place.
			num = lua_randint(maxnum)

			i = 0
			while (i < num) do

				local placex
				local placey

				-- Attempt to place the object.
				if (lua_randint(100) >= 51) then
					placex = centerx - lua_randint(wid / 2)
				else
					placex = centerx + lua_randint(wid / 2)
				end
				if (lua_randint(100) >= 51) then
					placey = centery - lua_randint(hgt / 2)
				else
					placey = centery + lua_randint(hgt / 2)
				end

				-- 50% chances of placing gold.
				if (lua_randint(100) >= 51) then
					place_gold(placey, placex)
				else
					-- 20% chance of placing a magical object.
					if (lua_randint(100) <= 20) then
						place_object(placey, placex, TRUE, TRUE)
					else
						place_object(placey, placex, FALSE, FALSE)
					end
				end
				i = i + 1
			end

			-- Done.
			okay = 1
		end

		-- A sealed room.
		-- It can contain a magical object, or a powerful monster.
		-- Or both.
		-- The smallest room is 5x4, so the room should be at least 7x6 in terms of size.
		if (poptype == 3) then

			if (wid >= 7 and hgt >= 6) then

				local roomwid
				local roomhgt
				local entrancedir
				local roomroll

				-- Variance in room's size.
				roomwid = wid - ((lua_randint(4)-1) + 2)
				roomhgt = hgt - ((lua_randint(4)-1) + 2)

				-- At least 5x4.
				if (roomwid < 5) then roomwid = 5 end
				if (roomhgt < 4) then roomhgt = 4 end

				-- Build the room.
				for i = (centery - (roomhgt / 2)), (centery + (roomhgt / 2)) do

					for j = (centerx - (roomwid / 2)), (centerx + (roomwid / 2)) do

						if ((i == (centery - (roomhgt / 2)) or i == (centery + (roomhgt / 2))) or (j == (centerx - (roomwid / 2)) or j == (centerx + (roomwid / 2)))) then
							cave_set_feat(i, j, FEAT_PERM_SOLID)
						else
							cave_set_feat(i, j, FEAT_FLOOR)
						end

						-- We shall use a special event to prevent the removal of these walls.
						cave(i, j).event = -1
					end
				end

				-- Put a rubble as the entrance.
				
				-- Pick a side for the entrance.
				entrancedir = lua_randint(4)
				if (entrancedir == 1) then entrancedir = 6 end
				if (entrancedir == 3) then entrancedir = 8 end

				if (entrancedir == 2) then

					i = (centery - (roomhgt / 2))
					cave_set_feat(i, centerx, FEAT_RUBBLE)
				end
				if (entrancedir == 8) then

					i = (centery + (roomhgt / 2))
					cave_set_feat(i, centerx, FEAT_RUBBLE)
				end
				if (entrancedir == 4) then

					i = (centerx - (roomwid / 2))
					cave_set_feat(centery, i, FEAT_RUBBLE)
				end
				if (entrancedir == 6) then

					i = (centerx + (roomwid / 2))
					cave_set_feat(centery, i, FEAT_RUBBLE)
				end

				-- Roll for what we're going to find.
				roomroll = lua_randint(2)

				-- Dragon's Hoard!
				-- Room is filled with gold and a few magic items.
				-- A dragon is guarding the treasure!
				if (roomroll == 1) then

					local oldmonsterlevel
					oldmonsterlevel = monster_level

					-- Place a mix of gold and magic items.
					-- They are worth it.
					for i = (centery - ((roomhgt / 2)-1)), (centery + ((roomhgt / 2)-1)) do

						for j = (centerx - ((roomwid / 2)-1)), (centerx + ((roomwid / 2)-1)) do

							if (lua_randint(100) >= 5) then

								place_gold(i, j)
							else

								place_object(i, j, TRUE, TRUE)
							end
						end
					end

					-- And place the dragon in the middle!

					-- 75% of the time, we place a dragon that can be up to 20 levels out of depth.
					if (lua_randint(100) <= 75) then

						p_ptr.events[29022] = 3
						monster_level = monster_level + (lua_randint(21)-1)
						place_monster(centery, centerx, FALSE, FALSE, 0)
						monster_level = oldmonsterlevel
						p_ptr.events[29022] = 0
					else
						-- But sometimes, it's no ordinary dragon.
						generate_monster(2050, monster_level + 10, 4)
						place_monster_one_return(centery, centerx, 2050, FALSE, FALSE, monster_level + 10, 0)
					end
				end

				-- Room of the Skeleton Lord!
				-- Can you defeat this powerful Skeleton Lord, and claim it's treasures?
				if (roomroll == 2) then

					local oldmonsterlevel
					oldmonsterlevel = monster_level

					-- Generate it.
					generate_monster(2050, monster_level + 10, 5)
					place_monster_one_return(centery, centerx, 2050, FALSE, FALSE, monster_level + 10, 0)

					-- An undead escort.
					p_ptr.events[29022] = 1
					place_monster(centery-1, centerx-1, FALSE, FALSE, 0)
					place_monster(centery-1, centerx, FALSE, FALSE, 0)
					place_monster(centery-1, centerx+1, FALSE, FALSE, 0)
					place_monster(centery, centerx-1, FALSE, FALSE, 0)
					place_monster(centery, centerx+1, FALSE, FALSE, 0)
					place_monster(centery+1, centerx-1, FALSE, FALSE, 0)
					place_monster(centery+1, centerx, FALSE, FALSE, 0)
					place_monster(centery+1, centerx+1, FALSE, FALSE, 0)
					p_ptr.events[29022] = 0
				end

				-- Done.
				okay = 1
			end
		end
	end
end

-- Used by the death 'quest', Q30000.txt.
function place_death_portals ()

	cave_set_feat(11, 4, 244)
	cave_set_feat(11, 15, 245)
	cave_set_feat(11, 26, 246)
	do_cmd_save_game()
end

----------------------------------

-- Used in Q25.txt
function altered_ophelia_tomb ()

	if (p_ptr.events[101] == 0) then
		p_ptr.events[101] = 1
		show_dialog(48)
	end
end

-- Used in Q27.txt
function altered_ophelia_tomb_2 ()

	if (p_ptr.events[105] == 1 and p_ptr.events[106] == 1) then

		p_ptr.events[107] = 1
	end
	show_dialog(51)
end

-- Used in Q29.txt
function altered_ophelia_tomb_3 ()

	if (p_ptr.events[116] == 0) then

		p_ptr.events[116] = 1
		show_dialog(54)
	end
end

-- Used in Q24.txt
function jeffrey_killed_reward ()

	if (p_ptr.events[114] == 2) then

		p_ptr.events[114] = 3
		show_dialog(57)
	end
end

-- Used in Q32.txt
function altered_ophelia_tomb_4 ()

	if (p_ptr.events[123] == 0) then

		p_ptr.events[123] = 1
		show_dialog(58)
	end
end

-- Used in Q101.txt
-- This is when you leave the sewers by the slums entrance.
-- It makes sure you get teleported to the slums district of Jindar.
function sewers_exit_to_slums ()

	dun_level = 0
	p_ptr.inside_quest = 0
	p_ptr.town_num = 115
	p_ptr.wild_x = 46
	p_ptr.wild_y = 41
	p_ptr.startx = 12
	p_ptr.starty = 58
	p_ptr.leaving = TRUE
	generate_cave()
	verify_panel()
end

-- Used in Q103.txt
-- Leaving the palace uses a script to bring you back to town 1012,
-- in case you used the sewers to enter the palace.
function palace_exit_to_city ()

	dun_level = 0
	p_ptr.inside_quest = 0
	p_ptr.town_num = 112
	p_ptr.wild_x = 48
	p_ptr.wild_y = 40
	p_ptr.startx = 99
	p_ptr.starty = 27
	p_ptr.leaving = TRUE
	generate_cave()
	verify_panel()
end

-- Used in Q104.txt
-- When you enter the throne room, a dialog will be shown.
-- Note that event 1005 here is actually event 1004, but due to
-- the way Lua handles arrays, you must add 1 to the event's number.
-- So event 1005 here is really 1004, and 1004 should be used elsewhere.
function jindar_throne_enter ()

	if (p_ptr.events[1005] == 0) then
		p_ptr.events[1005] = 1
		show_dialog(1018)
	end
end

-- Used in T120.txt
-- When you see the temple for the first time, a dialog appear.
-- Again, event 1020 here is really event 1019.
function twisted_temple_arrive ()

	if (p_ptr.events[1020] == 0) then
		p_ptr.events[1020] = 1
		show_dialog(1026)
	end
end

-- Used in Q108.txt
-- Enter quest 109. However, if quest is completed, enter 105 instead.
-- Once again, event 1022 is really 1021.
function go_inside_twisted_temple ()

	-- Player enters a new quest
	--p_ptr.oldpy = py
	--p_ptr.oldpx = px

	if (p_ptr.events[1022] == 1) then

		p_ptr.inside_quest = 105
	else
		p_ptr.inside_quest = 109
	end

	dun_level = 0

	p_ptr.questx = 26
	p_ptr.questy = 14
	p_ptr.leaving = TRUE
	generate_cave()
	verify_panel_always_update()
end

-- Used in Q109.txt
-- Just a dialog when you enter the twisted temple for the first time.
function twisted_temple_first_time ()

	if (p_ptr.events[1023] == 0) then
		p_ptr.events[1023] = 1
		show_dialog(1028)
	end
end

-- Used in Q115.txt and T0004.txt
-- This makes all Dragon Lancers hostile to you if you joined the Stealthy Vipers!
function make_all_lancers_hostile ()

	if (p_ptr.events[1029] == 1) then
		
		local i
		-- m_max is the number of monsters on a given level.
		for i = 1, (m_max - 1) do
			if ((monster(i).r_idx == 1603) or (monster(i).r_idx == 1604) or (monster(i).r_idx == 1605)) then
				
				set_pet(monster(i), FALSE)
			end
		end
	end
end

-- Used in Q118.txt
function donoriel_shop_first ()

	if (p_ptr.events[1045] == 0) then
		p_ptr.events[1045] = 1
		show_dialog(1046)
	end
end

-- Used in Q118.txt
function donoriel_to_basement ()

	p_ptr.inside_quest = 119
	dun_level = 0
	p_ptr.startx = 90
	p_ptr.starty = 47
	p_ptr.questx = 38
	p_ptr.questy = 5
	p_ptr.leaving = TRUE
	generate_cave()
	verify_panel_always_update()
end

-- Used in Q119.txt
function basement_to_donoriel ()

	p_ptr.inside_quest = 118
	dun_level = 0
	p_ptr.startx = 97
	p_ptr.starty = 47
	p_ptr.questx = 2
	p_ptr.questy = 5
	p_ptr.leaving = TRUE
	generate_cave()
	verify_panel_always_update()
end

-- Used in Q126.txt
-- If you kill the maid or Harrington, all guards will become hostile.
function harrington_mansion_hostility ()

	if (p_ptr.events[1078] == 1 or p_ptr.events[1079] == 1) then
		
		local i
		-- m_max is the number of monsters on a given level.
		for i = 1, (m_max - 1) do
			if ((monster(i).r_idx == 1620) or (monster(i).r_idx == 1622)) then
				
				set_pet(monster(i), FALSE)
			end
		end
	end
end

-- Used in Q135.txt
function vipers_hq_first ()

	if (p_ptr.events[1130] == 0) then

		show_dialog(1062)
		p_ptr.events[1130] = 1
	end

	if (p_ptr.events[1131] >= 1) then

		cave_set_feat(17, 7, FEAT_FLOOR)
		cave_set_feat(18, 6, FEAT_RUBBLE)
		cave_set_feat(18, 8, FEAT_RUBBLE)
	end
end

-- Used in Q134.txt
function vipers_hq_second_part ()

	if (p_ptr.events[1132] == 1) then

		show_dialog(1064)
		p_ptr.events[1132] = 2
	end
end

-- Delviaz talk script.
function delviaz_talk_1 ()

	p_ptr.events[1134] = 1
	if (p_ptr.events[1137] == 0) then

		show_dialog(1066)
	end
end

function delviaz_talk_2 ()

	if (p_ptr.events[1145] == 0) then

		show_dialog(1066)
	end
end

-- Used in Q500.txt
function barrack_to_entrance ()

	p_ptr.inside_quest = 503
	dun_level = 0
	p_ptr.startx = 53
	p_ptr.starty = 12
	p_ptr.questx = 15
	p_ptr.questy = 17
	p_ptr.leaving = TRUE
	generate_cave()
	verify_panel_always_update()
end

-- Used in Q503.txt
function entrance_to_barrack ()

	p_ptr.inside_quest = 500
	dun_level = 0
	p_ptr.startx = 53
	p_ptr.starty = 20
	p_ptr.questx = 25
	p_ptr.questy = 3
	p_ptr.leaving = TRUE
	generate_cave()
	verify_panel_always_update()
end

-- Used in Q504.txt
function enter_tunnels ()

	-- It's really event number 1511, as you might already know.
	if (p_ptr.events[1512] == 1) then p_ptr.events[1512] = 0 end
end

-- Used in Q509.txt
function enter_underground ()

	-- It's really event number 1511, as you might already know.
	if (p_ptr.events[1520] == 0) then

		show_dialog(1518)
		p_ptr.events[1520] = 1
	end
end

-- Used in Q508.txt
function ivhala_palace_exit_to_city ()

	dun_level = 0
	p_ptr.inside_quest = 0
	p_ptr.town_num = 501
	p_ptr.wild_x = 85
	p_ptr.wild_y = 46
	p_ptr.startx = 99
	p_ptr.starty = 26
	p_ptr.leaving = TRUE
	generate_cave()
	verify_panel()
end

-- Used in Q516.txt
function balcia_first ()

	if (p_ptr.events[1532] == 0) then

		show_dialog(1526)
		p_ptr.events[1532] = 1
	end
end

-- Used in Q517.txt
function loroth_first ()

	if (p_ptr.events[1534] == 0) then

		show_dialog(1528)
		p_ptr.events[1534] = 1
	end
end

-- Used in T511.txt
function ivhala_wight_first ()

	if (p_ptr.events[1537] == 0) then

		show_dialog(1530)
		p_ptr.events[1537] = 1
	end
end

-- Used in Q521.txt
function enter_underground_twisted ()

	if (p_ptr.events[1542] == 0) then

		show_dialog(1533)
		p_ptr.events[1542] = 1
	end
end

-- Used in Q522.txt
function grey_wight_first ()

	if (p_ptr.events[1544] == 0) then

		show_dialog(1535)
		p_ptr.events[1544] = 1
	end
end

-- Used in Q523.txt
function balcia_third ()

	if (p_ptr.events[1546] == 0) then

		show_dialog(1537)
		p_ptr.events[1546] = 1
	end
end

-- Used in Q524.txt
function banshee_first ()

	-- Reset special move.
	p_ptr.events[1550] = 0

	if (p_ptr.events[1548] == 0) then

		show_dialog(1539)
		p_ptr.events[1548] = 1
	end
end

-- Used in Q25000.txt

function ginwhal_prepare_level ()

	p_ptr.events[25005] = 0
	p_ptr.events[25006] = 0
end

function open_rage_wall_1 ()

	msg_print("A secret door opens up!")
	cave_set_feat(2, 12, FEAT_FLOOR)
	update_and_handle()
end

function open_rage_wall_2 ()

	msg_print("You hear some secret doors opening themselves...")
	cave_set_feat(3, 35, FEAT_FLOOR)
	cave_set_feat(7, 33, FEAT_FLOOR)
	update_and_handle()
end

function open_rage_wall_3 ()

	msg_print("You hear some secret doors opening themselves...")
	cave_set_feat(2, 38, FEAT_FLOOR)
	cave_set_feat(5, 37, FEAT_FLOOR)
	cave_set_feat(11, 38, FEAT_FLOOR)
	update_and_handle()
end

function avemorsh_battle ()

	p_ptr.events[25010] = 0
	p_ptr.events[25012] = 0

	if (p_ptr.events[25008] == 0) then

		show_dialog(25001)
		p_ptr.events[25008] = 1
	end
end

-- ########## SPECIAL ATTACKS OF MONSTERS ##########

-- Quazar's reality twisting magic.
-- There are three options here:
-- 1. Will set your experience to 0.
-- 2. Sets mana to 0, and wisdom to 1!
-- 3. Move the player around.
function reality_twists_quazar (m_idx)

	local twist_result
	local tmpx
	local tmpy
	local ppower
	local mpower

	-- The only way to counter it is with the Diviner's ability.
	if (p_ptr.abilities[(CLASS_DIVINER * 10) + 2] >= 1) then

		ppower = p_ptr.abilities[(CLASS_DIVINER * 10) + 2] * 100
		mpower = monster(m_idx).level + monster(m_idx).mind

		if (lua_randint(ppower) >= lua_randint(mpower)) then

			msg_print("Quazar attempts to twist reality, but you counter his power.")
			return
		end
	end
	
	twist_result = lua_randint(100)

	if (twist_result >= 66) then

		if (not(p_ptr.hold_life)) then
			show_dialog(1032)
			p_ptr.exp = 0
			p_ptr.lev = 1
			check_experience()
			update_and_handle()
		else
			msg_print("Quazar attacks your knowledge, but you are unaffected.")
		end
	elseif (twist_result >= 33) then

		show_dialog(1033)
		if (not(p_ptr.sustain_int)) then
			dec_stat(A_INT, 3000, 2)
		else
			msg_print("Your Intelligence remains untouched.")
		end
		if (not(p_ptr.sustain_wis)) then
			dec_stat(A_WIS, 3000, 2)
		else
			msg_print("Your Wisdom remains untouched.")
		end
		update_and_handle()
	else
		if (lua_randint(100) > p_ptr.resistances[GF_WARP+1]) then
			msg_print("Quazar teleports you around!")
			p_ptr.inside_quest = 0
			teleport_player(5)
			p_ptr.inside_quest = 1013
			update_and_handle()
		else
			msg_print("Quazar attempts to teleport you, but Warp resistance prevents it.")
		end
	end
end

function ancient_phantom_golem_boost (m_idx)

	local i

	msg_print("The Ancient Phantom casts a spell, and enhances the golems powers!")

	for i = 1, (m_max - 1) do

		-- Skip dead monsters
		if (not(monster(i).r_idx == 0)) then

			-- Only a very specific kind of enemy can be enhanced.
			if (monster(i).r_idx == 1044) then

				-- Turn the golems into very powerful enemies!
				give_monster_ability(monster(i), BOSS_DOUBLE_DAMAGES)
				give_monster_ability(monster(i), BOSS_DOUBLE_MAGIC)
				give_monster_ability(monster(i), BOSS_HALVE_DAMAGES)

				monster(i).level = monster(i).level + 20
				apply_monster_level_hp(monster(i))

				monster(i).maxhp = monster(i).maxhp * 2
				monster(i).hp = monster(i).maxhp
				monster(i).mspeed = monster(i).mspeed + 10
				monster(i).boss = 2
				if (monster(i).mspeed > 180) then monster(i).mspeed = 180 end

				-- But it only lasts 10 turns, and then they're destroyed!
				monster(i).summoned = 10
			end
		end
	end
end

function reality_twists_simon (m_idx)

	local ppower
	local mpower

	if (p_ptr.abilities[(CLASS_DIVINER * 10) + 2] >= 1) then

		ppower = p_ptr.abilities[(CLASS_DIVINER * 10) + 2] * 100
		mpower = monster(m_idx).level + monster(m_idx).mind

		if (lua_randint(ppower) >= lua_randint(mpower)) then

			msg_print("Simon attempts to twist reality, but you counter his power.")
			return
		end
	end
	
	msg_print("Simon twists reality! Your body is now ruined, and you are extremely sick.")
	p_ptr.csp = 0
	set_poisoned(100)
	dec_stat(A_STR, 3000, 2)
	dec_stat(A_CON, 3000, 2)
	update_and_handle()

end

function reality_twists_simon_2 (m_idx)

	local ppower
	local mpower

	if (p_ptr.abilities[(CLASS_DIVINER * 10) + 2] >= 1) then

		ppower = p_ptr.abilities[(CLASS_DIVINER * 10) + 2] * 100
		mpower = monster(m_idx).level + monster(m_idx).mind

		if (lua_randint(ppower) >= lua_randint(mpower)) then

			msg_print("Simon attempts to twist reality, but you counter his power.")
			return
		end
	end
	
	if (lua_randint(100) >= 50) then
		msg_print("Simon twists reality! Your body is now ruined, and you are extremely sick.")
		p_ptr.csp = 0
		set_poisoned(100)
		dec_stat(A_STR, 3000, 2)
		dec_stat(A_CON, 3000, 2)
		update_and_handle()
	else
		msg_print("Simon twists reality! You take damages!")
		take_hit(3000, "Simon")
		update_and_handle()
	end

end

-- Used by the Mind Gazer enemy.
function mind_gazer_divination (m_idx)

	local ppower
	local mpower

	ppower = p_ptr.lev + p_ptr.stat_ind[A_INT+1] + p_ptr.stat_ind[A_WIS+1]
	mpower = monster(m_idx).level + monster(m_idx).mind

	if (lua_randint(mpower) >= lua_randint(ppower)) then

		msg_print("The Mind Gazer reads your mind, and attacks you with Divination!")
		lua_bolt(m_idx, 161, p_ptr.lev)
	else
		msg_print("The Mind Gazer tries to read your mind, but fails.")
	end
end

-- Used by Christina.
function christina_sword_of_gaia (m_idx)

	local dam
	local ddice
	local dside

	ddice = 6 + (monster(m_idx).level / 12)
	dside = 5 + (monster(m_idx).level / 7)
	dam = monster_damages(monster(m_idx), ddice, dside, monster(m_idx).str)

	msg_print("Christina uses Sword of Gaia!")
	lua_ball(m_idx, GF_EARTH, dam, 3)
	update_and_handle()
end

-- Used by Naga Princess of Blades.
function naga_princess_mystic_blade (m_idx)

	local dam
	local ddice
	local dside

	ddice = 6 + (monster(m_idx).level / 13)
	dside = 6 + (monster(m_idx).level / 8)
	dam = monster_damages(monster(m_idx), ddice, dside, monster(m_idx).str)

	msg_print("The Naga Princess of Blades uses Mystic Blade!")
	lua_bolt(m_idx, GF_HARM, dam)
	update_and_handle()
end

-- Used by the Grey Wight.
function grey_wight_gaze (m_idx)

	local ppower
	local mpower
	local m_name = ""

	ppower = p_ptr.skill[28]
	mpower = monster(m_idx).skill_magic

	if not (monster(m_idx).ml) then
		m_name = "it"
	elseif (get_monster_flag1(monster(m_idx).r_idx, RF1_UNIQUE)) then
		m_name = m_race(monster(m_idx).r_idx).name_char
	else
		m_name = string.format('%s %s', "The", m_race(monster(m_idx).r_idx).name_char)
	end

	if (lua_randint(mpower) >= lua_randint(ppower)) then

		msg_print(string.format('%s gazes at you with the red eye! You are paralyzed by fear!', m_name))
		set_afraid(10)
		set_paralyzed(2)
	else
		msg_print(string.format('%s gazes at you with the red eye, but you defend yourself against the attack.', m_name))
	end
end

-- Teleport to player.
function monster_teleport_to_player (m_idx)

	local m_name = ""

	if not (monster(m_idx).ml) then
		m_name = "it"
	elseif (get_monster_flag1(monster(m_idx).r_idx, RF1_UNIQUE)) then
		m_name = m_race(monster(m_idx).r_idx).name_char
	else
		m_name = string.format('%s %s', "The", m_race(monster(m_idx).r_idx).name_char)
	end

	-- Check range
	if (monster(m_idx).cdis <= MAX_RANGE and not(is_pet(monster(m_idx)))) then

		-- Check path
		if (projectable(monster(m_idx).fy, monster(m_idx).fx, py, px)) then

			msg_print(string.format('%s teleports to you!', m_name))
			teleport_to_player(m_idx)
		end
	end
end

-- Blaze. Place fire fields around the player.
function immolating_blaze (m_idx)

	local m_name = ""
	local dam = 0
	local power = 0
	local rad = 0

	if not (monster(m_idx).ml) then
		m_name = "it"
	elseif (get_monster_flag1(monster(m_idx).r_idx, RF1_UNIQUE)) then
		m_name = m_race(monster(m_idx).r_idx).name_char
	else
		m_name = string.format('%s %s', "The", m_race(monster(m_idx).r_idx).name_char)
	end

	msg_print(string.format('%s cast Immolating Blaze!', m_name))

	-- radius is 1, +1 every 30 levels.
	rad = 1 + (monster(m_idx).level / 30)

	-- Power is 10, +1 every 3 levels.
	power = 10 + (monster(m_idx).level / 3)

	dam = monster_spell_damages(monster(m_idx), power)
	if (get_monster_ability(monster(m_idx), BOSS_DOUBLE_MAGIC)) then dam = dam * 2 end

	place_field_monsters(FEAT_FIRE_FIELD, rad, px, py, dam)
	update_and_handle()
end

-- Used in Q25000.txt
function torment_spells (m_idx)

	local m_name = ""

	if not (monster(m_idx).ml) then
		m_name = "it"
	elseif (get_monster_flag1(monster(m_idx).r_idx, RF1_UNIQUE)) then
		m_name = m_race(monster(m_idx).r_idx).name_char
	else
		m_name = string.format('%s %s', "The", m_race(monster(m_idx).r_idx).name_char)
	end

	if (p_ptr.events[25005] < 100) then

		p_ptr.events[25005] = p_ptr.events[25005] + 10
		msg_print(string.format('%s charges energy... (%d%%)', m_name, p_ptr.events[25005]))
	elseif (p_ptr.events[25005] >= 100) then

		if (p_ptr.events[25006] == 1) then

			msg_print(string.format('%s summon demons!', m_name))
			summon_specific_kind(py, px, p_ptr.max_plv + (p_ptr.max_plv / 2), 117, FALSE, FALSE, 20)
			summon_specific_kind(py, px, p_ptr.max_plv + (p_ptr.max_plv / 2), 117, FALSE, FALSE, 20)
			summon_specific_kind(py, px, p_ptr.max_plv + (p_ptr.max_plv / 2), 117, FALSE, FALSE, 20)
		else
			msg_print(string.format('%s summons soldiers of death!', m_name))
			summon_specific_ridx(py, px, 2308, FALSE, FALSE, 0)
			summon_specific_ridx(py, px, 2308, FALSE, FALSE, 0)
			summon_specific_ridx(py, px, 2308, FALSE, FALSE, 0)
		end
	end
end

-- It's nothing but a big script. :)
function avemorsh_attacks (m_idx)

	local i
	local m_name = ""

	if not (monster(m_idx).ml) then
		m_name = "it"
	elseif (get_monster_flag1(monster(m_idx).r_idx, RF1_UNIQUE)) then
		m_name = m_race(monster(m_idx).r_idx).name_char
	else
		m_name = string.format('%s %s', "The", m_race(monster(m_idx).r_idx).name_char)
	end

	-- Avemorsh has a lot of attacks.
	-- What he does depends on how many lives he has left.

	-- Phase 1: 75%+ lives.
	if (monster(m_idx).lives >= multiply_divide(p_ptr.events[25011], 75, 100)) then

		-- He will do one of three attacks: Fire Storm, Razor Winds or Energy Blast.
		if (p_ptr.events[25010] == 0) then

			p_ptr.events[25010] = lua_randint(3)
			if (p_ptr.events[25010] == 1) then

				msg_print(string.format('%s gather flames!', m_name))
			end
			if (p_ptr.events[25010] == 2) then

				msg_print(string.format('%s surrounds himself with wind!', m_name))
			end
			if (p_ptr.events[25010] == 3) then

				msg_print(string.format('%s begins casting a powerful spell...', m_name))
			end
		else

			if (p_ptr.events[25010] == 1) then

				local dam
				local lvl

				if (p_ptr.max_plv < 30) then lvl = 30
				else lvl = p_ptr.max_plv
				end

				-- That's 7500000 at level 30. :)
				dam = 250000 * lvl

				msg_print(string.format('%s invokes a powerful Fire Storm!', m_name))

				lua_ball(m_idx, GF_FIRE, dam, 10)
				lua_ball(m_idx, GF_FIRE, dam, 10)
				lua_ball(m_idx, GF_FIRE, dam, 10)

				p_ptr.events[25010] = 0
			end

			if (p_ptr.events[25010] == 2) then

				local dam
				local lvl

				if (p_ptr.max_plv < 30) then lvl = 30
				else lvl = p_ptr.max_plv
				end

				-- That's 7500000 at level 30. :)
				dam = 250000 * lvl

				msg_print(string.format('%s conjures mighty Razor Winds!', m_name))

				lua_ball(m_idx, GF_WIND, dam, 10)
				lua_ball(m_idx, GF_WIND, dam, 10)
				lua_ball(m_idx, GF_WIND, dam, 10)

				p_ptr.events[25010] = 0
			end

			if (p_ptr.events[25010] == 3) then

				local dam
				local lvl

				if (p_ptr.max_plv < 30) then lvl = 30
				else lvl = p_ptr.max_plv
				end

				-- Only 3000000 at level 30, and only once.
				-- But you'll have to think a bit to counter Missile!
				dam = 100000 * lvl

				msg_print(string.format('%s casts an Energy Blast!', m_name))

				lua_ball(m_idx, GF_MISSILE, dam, 10)

				p_ptr.events[25010] = 0
			end
		end

	-- Phase 2: 50%+ lives.
	elseif (monster(m_idx).lives >= multiply_divide(p_ptr.events[25011], 50, 100)) then

		if (p_ptr.events[25010] < 4) then

			p_ptr.events[25010] = 4
			show_dialog(25002)
		else

			-- His second phase consists of 5 melee Spin Attacks of Harm type.
			-- If you are next to him, he will use them.
			if ( (((monster(m_idx).fx - 1) == px) or ((monster(m_idx).fx + 1) == px) or ((monster(m_idx).fx) == px)) and (((monster(m_idx).fy - 1) == py) or ((monster(m_idx).fy + 1) == py) or ((monster(m_idx).fy) == py))) then

				local dam
				local lvl

				if (p_ptr.max_plv < 30) then lvl = 30
				else lvl = p_ptr.max_plv
				end

				-- 15000000 at level 30. Five times. Yeouch!
				dam = 500000 * lvl

				msg_print(string.format('%s performs a serie of deadly Spin Attacks!', m_name))

				-- This one is a melee attack.
				monster_physical = TRUE
				lua_project(m_idx, 1, monster(m_idx).fy, monster(m_idx).fx, dam, GF_HARM, 2)
				lua_project(m_idx, 1, monster(m_idx).fy, monster(m_idx).fx, dam, GF_HARM, 2)
				lua_project(m_idx, 1, monster(m_idx).fy, monster(m_idx).fx, dam, GF_HARM, 2)
				lua_project(m_idx, 1, monster(m_idx).fy, monster(m_idx).fx, dam, GF_HARM, 2)
				lua_project(m_idx, 1, monster(m_idx).fy, monster(m_idx).fx, dam, GF_HARM, 2)
				monster_physical = FALSE
			else

				-- Otherwise, 75% of the time, he will teleport at you.
				-- But 25% of the time, he will instead cast a Wind spell.
				if (lua_randint(100) >= 25) then

					-- Check range
					if (monster(m_idx).cdis <= MAX_RANGE and not(is_pet(monster(m_idx)))) then

						-- Check path
						if (projectable(monster(m_idx).fy, monster(m_idx).fx, py, px)) then

							msg_print(string.format('%s jumps right next to you!', m_name))
							teleport_to_player(m_idx)
						end
					end
				else

					local dam
					local lvl

					if (p_ptr.max_plv < 30) then lvl = 30
					else lvl = p_ptr.max_plv
					end

					-- That's 4500000 at level 30.
					dam = 150000 * lvl

					msg_print(string.format('%s casts a Fire Ball!', m_name))

					lua_ball(m_idx, GF_FIRE, dam, 5)
				end
			end
		end
	-- Phase 3: 25%+ lives.
	elseif (monster(m_idx).lives >= multiply_divide(p_ptr.events[25011], 25, 100)) then

		if (p_ptr.events[25010] < 5) then

			p_ptr.events[25010] = 5
			show_dialog(25002)
		else

			-- During the third phase, he will gather energy for 5 turns, the unleash a big spell.
			-- After that, he will jump next to you, spin, then start over.
			if (p_ptr.events[25012] > 100) then

				local dam
				local lvl

				-- Check range
				if (monster(m_idx).cdis <= MAX_RANGE and not(is_pet(monster(m_idx)))) then

					-- Check path
					if (projectable(monster(m_idx).fy, monster(m_idx).fx, py, px)) then

						msg_print(string.format('%s jumps right next to you!', m_name))
						teleport_to_player(m_idx)
					end
				end

				if (p_ptr.max_plv < 30) then lvl = 30
				else lvl = p_ptr.max_plv
				end

				-- That's 3000000 at level 30.
				dam = 100000 * lvl

				msg_print(string.format('%s performs Gust Spin!', m_name))

				monster_physical = TRUE
				lua_project(m_idx, 1, monster(m_idx).fy, monster(m_idx).fx, dam, GF_WIND, 2)
				monster_physical = FALSE

				p_ptr.events[25012] = 0
				
			elseif (p_ptr.events[25012] < 100) then

				p_ptr.events[25012] = p_ptr.events[25012] + 20
				msg_print(string.format('%s gathers energy... (%d%%)', m_name, p_ptr.events[25012]))
			else

				local dam
				local lvl

				if (p_ptr.max_plv < 30) then lvl = 30
				else lvl = p_ptr.max_plv
				end

				-- That's 30000000 at level 30. :)
				-- 5 times. Missile. Can you survive?
				dam = 1000000 * lvl

				msg_print(string.format('%s screams DESTRUCTION!!!', m_name))

				lua_project(m_idx, 10, monster(m_idx).fy, monster(m_idx).fx, dam, GF_HARM, 2)
				lua_project(m_idx, 10, monster(m_idx).fy, monster(m_idx).fx, dam, GF_HARM, 2)
				lua_project(m_idx, 10, monster(m_idx).fy, monster(m_idx).fx, dam, GF_HARM, 2)
				lua_project(m_idx, 10, monster(m_idx).fy, monster(m_idx).fx, dam, GF_HARM, 2)
				lua_project(m_idx, 10, monster(m_idx).fy, monster(m_idx).fx, dam, GF_HARM, 2)

				p_ptr.events[25012] = p_ptr.events[25012] + 20
			end
		end
	-- Phase 4: below 25% lives.
	else

		if (p_ptr.events[25010] < 6) then

			p_ptr.events[25010] = 6
			show_dialog(25002)
		else

			-- The last phase is quite simple. Each turns, he wull summon a Vengeful Orc.
			-- Every once in a while, he will throw one last Fire Spin in the mix.
			if (lua_randint(100) >= 25) then

				msg_print(string.format('%s summons a Vengeful Orc!', m_name))
				summon_specific_ridx(py, px, 2302, FALSE, FALSE, 0)
			else
				local dam
				local lvl

				-- Check range
				if (monster(m_idx).cdis <= MAX_RANGE and not(is_pet(monster(m_idx)))) then

					-- Check path
					if (projectable(monster(m_idx).fy, monster(m_idx).fx, py, px)) then

						msg_print(string.format('%s jumps right next to you!', m_name))
						teleport_to_player(m_idx)
					end
				end

				if (p_ptr.max_plv < 30) then lvl = 30
				else lvl = p_ptr.max_plv
				end

				-- That's 3000000 at level 30.
				dam = 100000 * lvl

				msg_print(string.format('%s performs Fire Spin!', m_name))

				monster_physical = TRUE
				lua_project(m_idx, 1, monster(m_idx).fy, monster(m_idx).fx, dam, GF_FIRE, 2)
				monster_physical = FALSE
			end
		end
	end

end

-- ########## MISC SCRIPTS ##########

function get_counter_name (counter)

	local cname

	if (counter == 1) then

		cname = "Block Melee"
		return cname
	end

	if (counter == 2) then

		cname = "Block Magic"
		return cname
	end

	if (counter == 3) then

		cname = "Block Melee/Magic"
		return cname
	end

	if (counter == 4) then

		cname = "100% Block Melee"
		return cname
	end

	if (counter == 5) then

		cname = "100% Block Magic"
		return cname
	end

	if (counter == 6) then

		cname = "100% Block Melee/Magic"
		return cname
	end

	if (counter == 7) then

		cname = "Return Melee"
		return cname
	end

	if (counter == 8) then

		cname = "Return Magic"
		return cname
	end

	if (counter == 9) then

		cname = "Return Melee/Magic"
		return cname
	end

	if (counter == 10) then

		cname = "Block/Return Melee"
		return cname
	end

	if (counter == 11) then

		cname = "Block/Return Magic"
		return cname
	end

	if (counter == 12) then

		cname = "Block/Return Melee/Magic"
		return cname
	end

	if (counter == 13) then

		cname = "100% Block/Return Melee"
		return cname
	end

	if (counter == 14) then

		cname = "100% Block/Return Magic"
		return cname
	end

	if (counter == 15) then

		cname = "100% Block/Return Mel/Mag"
		return cname
	end

	if (counter == 16) then

		cname = "Block Ranged"
		return cname
	end

	if (counter == 17) then

		cname = "Block Melee/Ranged"
		return cname
	end

	if (counter == 18) then

		cname = "Block Ranged/Magic"
		return cname
	end

	if (counter == 19) then

		cname = "Block All"
		return cname
	end

	if (counter == 20) then

		cname = "100% Block Ranged"
		return cname
	end

	if (counter == 21) then

		cname = "100% Block Melee/Ranged"
		return cname
	end

	if (counter == 22) then

		cname = "100% Block Ranged/Magic"
		return cname
	end

	if (counter == 23) then

		cname = "100% Block All"
		return cname
	end

	if (counter == 24) then

		cname = "Block All, Counter Attack"
		return cname
	end

	if (counter == 1000) then

		cname = "Soak damages based on your items value."
		return cname
	end

	-- If nothing was found, then default to "None"
	cname = "None"
	return cname
end

-- Should be updated as new monsters are made.
-- Try to keep each lines at 35 characters or lower.
function get_misc_monster_info (r_idx, line)

	-- Xythanos.
	if (r_idx == 240) then

		if (line == 1) then return "Can teleport to you before moving.  " end
		if (line == 2) then return "                                    " end
		if (line == 3) then return "                                    " end
		if (line == 4) then return "                                    " end
	end

	-- Fire Giant.
	if (r_idx == 264) then

		if (line == 1) then return "Fire Aura(Power 20, rad 3)          " end
		if (line == 2) then return "                                    " end
		if (line == 3) then return "                                    " end
		if (line == 4) then return "                                    " end
	end

	-- Frost Giant.
	if (r_idx == 265) then

		if (line == 1) then return "Cold Aura(Power 20, rad 3)          " end
		if (line == 2) then return "                                    " end
		if (line == 3) then return "                                    " end
		if (line == 4) then return "                                    " end
	end

	-- Planar Phantom.
	if (r_idx == 269) then

		if (line == 1) then return "Phase when damaged(rad 10)          " end
		if (line == 2) then return "                                    " end
		if (line == 3) then return "                                    " end
		if (line == 4) then return "                                    " end
	end

	-- Acid Horror.
	if (r_idx == 287) then

		if (line == 1) then return "Acid Aura(Power 20, rad 3)          " end
		if (line == 2) then return "                                    " end
		if (line == 3) then return "                                    " end
		if (line == 4) then return "                                    " end
	end

	-- Acid Horror.
	if (r_idx == 287) then

		if (line == 1) then return "Acid Aura(Power 20, rad 3)          " end
		if (line == 2) then return "                                    " end
		if (line == 3) then return "                                    " end
		if (line == 4) then return "                                    " end
	end

	-- Greater Fire Elemental
	if (r_idx == 289) then

		if (line == 1) then return "Fire Aura(Power 20, rad 3)          " end
		if (line == 2) then return "                                    " end
		if (line == 3) then return "                                    " end
		if (line == 4) then return "                                    " end
	end

	-- Greater Water Elemental
	if (r_idx == 290) then

		if (line == 1) then return "Water Aura(Power 20, rad 3)         " end
		if (line == 2) then return "                                    " end
		if (line == 3) then return "                                    " end
		if (line == 4) then return "                                    " end
	end

	-- Greater Earth Elemental
	if (r_idx == 291) then

		if (line == 1) then return "Earth Aura(Power 20, rad 3)         " end
		if (line == 2) then return "                                    " end
		if (line == 3) then return "                                    " end
		if (line == 4) then return "                                    " end
	end

	-- Greater Wind Elemental
	if (r_idx == 292) then

		if (line == 1) then return "Wind Aura(Power 20, rad 3)          " end
		if (line == 2) then return "                                    " end
		if (line == 3) then return "                                    " end
		if (line == 4) then return "                                    " end
	end

	-- Greater FrostFire Elemental
	if (r_idx == 293) then

		if (line == 1) then return "FrostFire Aura(Power 20, rad 3)     " end
		if (line == 2) then return "                                    " end
		if (line == 3) then return "                                    " end
		if (line == 4) then return "                                    " end
	end

	-- Greater Mud Elemental
	if (r_idx == 294) then

		if (line == 1) then return "Mud Aura(Power 20, rad 3)           " end
		if (line == 2) then return "                                    " end
		if (line == 3) then return "                                    " end
		if (line == 4) then return "                                    " end
	end

	-- Storm Giant
	if (r_idx == 299) then

		if (line == 1) then return "Wind Aura(Power 20, rad 3)          " end
		if (line == 2) then return "                                    " end
		if (line == 3) then return "                                    " end
		if (line == 4) then return "                                    " end
	end

	-- Phoenix of Solar Flames
	if (r_idx == 300) then

		if (line == 1) then return "Fire Aura(Power 30, rad 5)          " end
		if (line == 2) then return "Creates Fire fields when moving.    " end
		if (line == 3) then return "(Power 30, rad 5)                   " end
		if (line == 4) then return "                                    " end
	end

	-- Shadow Queen
	if (r_idx == 305) then

		if (line == 1) then return "Can evade melee/ranged attacks      " end
		if (line == 2) then return "more easily.                        " end
		if (line == 3) then return "                                    " end
		if (line == 4) then return "                                    " end
	end

	-- Hunter of Zekis
	if (r_idx == 314) then

		if (line == 1) then return "Disabling Aura(Power 20, rad 5)     " end
		if (line == 2) then return "                                    " end
		if (line == 3) then return "                                    " end
		if (line == 4) then return "                                    " end
	end

	-- Fused Warrior of Fire
	if (r_idx == 316) then

		if (line == 1) then return "Fire Aura(Power 30, rad 3)          " end
		if (line == 2) then return "                                    " end
		if (line == 3) then return "                                    " end
		if (line == 4) then return "                                    " end
	end

	-- Fused Warrior of Water
	if (r_idx == 317) then

		if (line == 1) then return "Water Aura(Power 30, rad 3)         " end
		if (line == 2) then return "                                    " end
		if (line == 3) then return "                                    " end
		if (line == 4) then return "                                    " end
	end

	-- Fused Warrior of Earth
	if (r_idx == 318) then

		if (line == 1) then return "Earth Aura(Power 30, rad 3)         " end
		if (line == 2) then return "                                    " end
		if (line == 3) then return "                                    " end
		if (line == 4) then return "                                    " end
	end

	-- Fused Warrior of Wind
	if (r_idx == 319) then

		if (line == 1) then return "Wind Aura(Power 30, rad 3)          " end
		if (line == 2) then return "                                    " end
		if (line == 3) then return "                                    " end
		if (line == 4) then return "                                    " end
	end

	-- Lura
	if (r_idx == 320) then

		if (line == 1) then return "Can teleport to you before moving.  " end
		if (line == 2) then return "                                    " end
		if (line == 3) then return "                                    " end
		if (line == 4) then return "                                    " end
	end

	-- Four Elemental
	if (r_idx == 325) then

		if (line == 1) then return "Fire Aura(Power 30, rad 3)          " end
		if (line == 2) then return "                                    " end
		if (line == 3) then return "                                    " end
		if (line == 4) then return "                                    " end
	end

	-- Nymph Sorceress
	if (r_idx == 328) then

		if (line == 1) then return "Blocks melee and ranged using Mind. " end
		if (line == 2) then return "                                    " end
		if (line == 3) then return "                                    " end
		if (line == 4) then return "                                    " end
	end

	-- Christina
	if (r_idx == 1357 or r_idx == 1373) then

		if (line == 1) then return "All damage taken is halved.         " end
		if (line == 2) then return "                                    " end
		if (line == 3) then return "                                    " end
		if (line == 4) then return "                                    " end
	end

	-- Korthrax
	if (r_idx == 1363) then

		if (line == 1) then return "Phase when damaged(rad 10)          " end
		if (line == 2) then return "                                    " end
		if (line == 3) then return "                                    " end
		if (line == 4) then return "                                    " end
	end

	-- Balcia.
	if (r_idx == 1366 or r_idx == 1369) then

		if (line == 1) then return "33% to phase when damaged(rad 10)   " end
		if (line == 2) then return "                                    " end
		if (line == 3) then return "                                    " end
		if (line == 4) then return "                                    " end
	end

	-- Balcia(Banshee).
	if (r_idx == 2001) then

		if (line == 1) then return "33% to phase when damaged(rad 10)   " end
		if (line == 2) then return "Chaos Aura(Power 30, rad 3)         " end
		if (line == 3) then return "                                    " end
		if (line == 4) then return "                                    " end
	end

	-- Spirit of Blizzard
	if (r_idx == 2106) then

		if (line == 1) then return "Creates Cold fields when moving.    " end
		if (line == 2) then return "(Power 10, rad 5)                   " end
		if (line == 3) then return "                                    " end
		if (line == 4) then return "                                    " end
	end

	-- Greater Spirit of Blizzard
	if (r_idx == 2111) then

		if (line == 1) then return "Creates Cold fields when moving.    " end
		if (line == 2) then return "(Power 15, rad 5)                   " end
		if (line == 3) then return "                                    " end
		if (line == 4) then return "                                    " end
	end

	-- Greater Spirit of Blizzard
	if (r_idx == 2111) then

		if (line == 1) then return "Creates Cold fields when moving.    " end
		if (line == 2) then return "(Power 15, rad 5)                   " end
		if (line == 3) then return "                                    " end
		if (line == 4) then return "                                    " end
	end

	-- Incarnation of Rhyzendalian Essence
	if (r_idx == 2115) then

		if (line == 1) then return "Creates Cold fields when moving.    " end
		if (line == 2) then return "(Power 20, rad 6)                   " end
		if (line == 3) then return "                                    " end
		if (line == 4) then return "                                    " end
	end

	-- Firestorm Gatekeeper
	if (r_idx == 2500) then

		if (line == 1) then return "Fire Aura(Power ??, rad 3)          " end
		if (line == 2) then return "Wind Aura(Power ??, rad 3)          " end
		if (line == 3) then return "                                    " end
		if (line == 4) then return "                                    " end
	end

	-- Pure Orcish Rage
	if (r_idx == 2501) then

		if (line == 1) then return "Prone to great feats of rage when   " end
		if (line == 2) then return "taking damages.                     " end
		if (line == 3) then return "                                    " end
		if (line == 4) then return "                                    " end
	end

	-- Avemorsh's Torment
	if (r_idx == 2502) then

		if (line == 1) then return "90% damages reduction while         " end
		if (line == 2) then return "charging energy.                    " end
		if (line == 3) then return "                                    " end
		if (line == 4) then return "                                    " end
	end

	-- Flow monsters.

	if (r_idx >= 2050 and r_idx <= 2099) then

		-- Phase when hit.
		if (m_race(r_idx).event_take_damages == 9) then

			if (line == 1) then return "Phase when damaged(rad 10)          " end
			if (line == 2) then return "                                    " end
			if (line == 3) then return "                                    " end
			if (line == 4) then return "                                    " end
		end

		-- Aura.
		if (m_race(r_idx).event_after_move == -1) then

			local basepower
			local rad
			local element

			basepower = m_race(monster(m_idx).r_idx).level / 3
			if (basepower < 1) then basepower = 1 end
			rad = (m_race(monster(m_idx).r_idx).level / 10) - 2
			if (rad < 2) then rad = 2 end
			if (rad > 10) then rad = 10 end

			element = (m_race(monster(m_idx).r_idx).event_misc * (-1))

			if (line == 1) then return string.format('%s Aura(Power %d, rad %d)     ', get_element_name(element), basepower, rad) end
			if (line == 2) then return "                                    " end
			if (line == 3) then return "                                    " end
			if (line == 4) then return "                                    " end
		end

		-- Fields.
		if (m_race(r_idx).event_after_move == -2) then

			local basepower
			local rad
			local ftype

			basepower = m_race(monster(m_idx).r_idx).level / 3
			if (basepower < 1) then basepower = 1 end
			rad = (m_race(monster(m_idx).r_idx).level / 20) - 2
			if (rad < 2) then rad = 2 end
			if (rad > 10) then rad = 10 end

			if (m_race(monster(m_idx).r_idx).event_misc == -1) then ftype = "Fire Fields" end
			if (m_race(monster(m_idx).r_idx).event_misc == -2) then ftype = "Cold Fields" end
			if (m_race(monster(m_idx).r_idx).event_misc == -3) then ftype = "Electric Fields" end
			if (m_race(monster(m_idx).r_idx).event_misc == -4) then ftype = "Thorned Vines" end
			if (m_race(monster(m_idx).r_idx).event_misc == -5) then ftype = "Storms" end

			if (line == 1) then return string.format('Creates %s when moving.   ', ftype) end
			if (line == 2) then return string.format('(Power %d, rad %d)        ', basepower, rad) end
			if (line == 3) then return "                                    " end
			if (line == 4) then return "                                    " end
		end
	end

	-- Default
	return "                                    "
end

-- Returns TRUE or FALSE to see if a room is in bounds or not.
function lua_inbounds (startx, starty, roomwid, roomhgt, left, top, right, bottom)

	if (left < 0 or top < 0 or right > roomwid or bottom > roomhgt) then
		return FALSE
	end

	if (((startx - left) > 0) and ((startx + right) < roomwid)) then

		if (((starty - top) > 0) and ((starty + bottom) < roomhgt)) then

			return TRUE
		end
	end

	return FALSE
end

-- Event handlers should be added for every functions you plan on using.
add_event_handler("skill_points_per_levels", skill_points_per_levels)
add_event_handler("stat_points_per_levels", stat_points_per_levels)
add_event_handler("ability_points_per_levels", ability_points_per_levels)
add_event_handler("starting_stats", starting_stats)
add_event_handler("gain_exp_kills", gain_exp_kills)
add_event_handler("dialog_script", dialog_script)
add_event_handler("activate_spell_script", activate_spell_script)
add_event_handler("sewers_exit_to_slums", sewers_exit_to_slums)
add_event_handler("palace_exit_to_city", palace_exit_to_city)
add_event_handler("jindar_throne_enter", jindar_throne_enter)
add_event_handler("twisted_temple_arrive", twisted_temple_arrive)
add_event_handler("go_inside_twisted_temple", go_inside_twisted_temple)
add_event_handler("twisted_temple_first_time", twisted_temple_first_time)
add_event_handler("make_all_lancers_hostile", make_all_lancers_hostile)
add_event_handler("reality_twists_quazar", reality_twists_quazar)
add_event_handler("donoriel_shop_first", donoriel_shop_first)
add_event_handler("donoriel_to_basement", donoriel_to_basement)
add_event_handler("basement_to_donoriel", basement_to_donoriel)
add_event_handler("harrington_mansion_hostility", harrington_mansion_hostility)
add_event_handler("vipers_hq_first", vipers_hq_first)
add_event_handler("vipers_hq_second_part", vipers_hq_second_part)
add_event_handler("delviaz_talk_1", delviaz_talk_1)
add_event_handler("delviaz_talk_2", delviaz_talk_2)
add_event_handler("entrance_to_barrack", entrance_to_barrack)
add_event_handler("barrack_to_entrance", barrack_to_entrance)
add_event_handler("enter_tunnels", enter_tunnels)
add_event_handler("enter_underground", enter_underground)
add_event_handler("ancient_phantom_golem_boost", ancient_phantom_golem_boost)
add_event_handler("reality_twists_simon", reality_twists_simon)
add_event_handler("reality_twists_simon_2", reality_twists_simon_2)
add_event_handler("mind_gazer_divination", mind_gazer_divination)
add_event_handler("christina_sword_of_gaia", christina_sword_of_gaia)
add_event_handler("naga_princess_mystic_blade", naga_princess_mystic_blade)
add_event_handler("ivhala_palace_exit_to_city", ivhala_palace_exit_to_city)
add_event_handler("get_counter_name", get_counter_name)
add_event_handler("get_misc_monster_info", get_misc_monster_info)
add_event_handler("flow_last_floor", flow_last_floor)
add_event_handler("enter_flow_temple_1", enter_flow_temple_1)
add_event_handler("flow_special_level", flow_special_level)
add_event_handler("generate_random_ruin", generate_random_ruin)
add_event_handler("populate_ruin_room", populate_ruin_room)
add_event_handler("lua_inbounds", lua_inbounds)
add_event_handler("altered_ophelia_tomb", altered_ophelia_tomb)
add_event_handler("altered_ophelia_tomb_2", altered_ophelia_tomb_2)
add_event_handler("altered_ophelia_tomb_3", altered_ophelia_tomb_3)
add_event_handler("jeffrey_killed_reward", jeffrey_killed_reward)
add_event_handler("altered_ophelia_tomb_4", altered_ophelia_tomb_4)
add_event_handler("place_death_portals", place_death_portals)
add_event_handler("balcia_first", balcia_first)
add_event_handler("loroth_first", loroth_first)
add_event_handler("ivhala_wight_first", ivhala_wight_first)
add_event_handler("enter_underground_twisted", enter_underground_twisted)
add_event_handler("grey_wight_first", grey_wight_first)
add_event_handler("grey_wight_gaze", grey_wight_gaze)
add_event_handler("immolating_blaze", immolating_blaze)
add_event_handler("monster_teleport_to_player", monster_teleport_to_player)
add_event_handler("balcia_third", balcia_third)
add_event_handler("banshee_first", banshee_first)
add_event_handler("ginwhal_prepare_level", ginwhal_prepare_level)
add_event_handler("open_rage_wall_1", open_rage_wall_1)
add_event_handler("open_rage_wall_2", open_rage_wall_2)
add_event_handler("open_rage_wall_3", open_rage_wall_3)
add_event_handler("torment_spells", torment_spells)
add_event_handler("avemorsh_battle", avemorsh_battle)
add_event_handler("avemorsh_attacks", avemorsh_attacks)
