-- File: item_archetypes.lua
-- This file contains the "give_item_bonus" function, which gives bonus
-- to an item following a character "archetype".

function give_item_bonus(item, bonuslevel)

	local i
	local archetype = 0
	local chosen = 0

	-- As of Portralis 0.5, this part has been rewritten.
	-- Magic items are rolled following an "archetype". This archetype
	-- includes a serie of bonus to specific stats and skills that will
	-- benefit that particular type of character to whom the item is made for.
	-- Certain items have restrictions on which archetypes they can get.


	-- List of archetypes.

	-- 1. Balanced Melee (Stats: Str, Con  Skills: Fighting/weapon skill/defense)
	-- 2. Melee Offense (Stats: Str  Skills: Fighting/Random weapon skill)
	-- 3. Con/Defense (Stats: Con  Skills: Defense)
	-- 4. Int caster (Stats: Int  Skills: Spellcraft/school)
	-- 5. Wis caster (Stats: Wis  Skills: Spellcraft/school)
	-- 6. Mana (Stats: Int  Bonus: Mana)
	-- 7. Monk (Stats: Wis  Skills: Martial Arts)
	-- 8. Ranged (Stats: Dex  Skills: Shooting/ranged skill)
	-- 9. Evasion (Stats: Dex  Skills: Agility)
	-- 10. Magic Defense (Stats: Con  Skills: Magic Defense)
	-- 11. Three Defenses (Skills: Defense, Agility, Magic Defense)
	-- 12. Speed (Skills: Agility  Bonus: Speed)
	-- 13. Elemental Resilience (Stats: Con  Bonus: resistance)
	-- 14. Magic Mitigation (Skills: Magic Defense  Bonus: resistance)
	-- 15. Pure resistance (Bonus: resistance)
	-- 16. Dex Fighter (Stats: Str, Dex  Skills: Fighting/weapon skill/dual wield/agility)
	-- 17. Rogue (Stats: Dex  Skills: Agility, Stealth)
	-- 18. Throwing (Stats: Str  Skills: Throwing)
	-- 19. Bard (Stats: Chr  Skills: Music)
	-- 20. Bard + Allies (Stats: Chr  Skills: Leadership, Music)
	-- 21. Summoner (Stats: Int, Chr  Skills: Leadership, Conjuration)
	-- 22. Elemental Lord (Stats: Str, Int  Skills: Weapon skill/Elemental)
	-- 23. Random (Stats: 2 random stats  Skills: 2 random skills)
	i = 0
	while (chosen == 0) do

		local valid = 1

		-- Roll for archetype.
		archetype = lua_randint(23)

		-- Validate each archetypes.
		-- Not all of them can appear on every items.

		-- Balanced melee.
		if (archetype == 1) then

			if (not(item.tval == TV_WEAPON) and not(item.tval == TV_SHIELD) and not(item.tval == TV_HARD_ARMOR) and not(item.tval == TV_DRAG_ARMOR)) then

				valid = 0
			end
		end

		-- Melee Offense
		if (archetype == 2) then

			if (not(item.tval == TV_WEAPON) and not(item.tval == TV_GLOVES) and not(item.tval == TV_ARM_BANDS)) then

				valid = 0
			end
		end

		-- Con/defense
		if (archetype == 3) then

			if (not(item.tval == TV_SHIELD) and not(item.tval == TV_HELM) and not(item.tval == TV_SOFT_ARMOR) and not(item.tval == TV_HARD_ARMOR) and not(item.tval == TV_DRAG_ARMOR)) then

				valid = 0
			end
		end

		-- Int caster
		if (archetype == 4) then

			if (not(item.tval == TV_ROD) and not(item.tval == TV_HELM) and not(item.tval == TV_ARM_BANDS) and not(item.tval == TV_SOFT_ARMOR) and not(item.tval == TV_CLOAK)) then

				valid = 0
			end
		end

		-- Wis caster
		if (archetype == 5) then

			if (not(item.tval == TV_HELM) and not(item.tval == TV_GLOVES) and not(item.tval == TV_SOFT_ARMOR) and not(item.tval == TV_HARD_ARMOR) and not(item.tval == TV_DRAG_ARMOR)and not(item.tval == TV_CLOAK)) then

				valid = 0
			end
		end

		-- Mana
		if (archetype == 6) then

			if(not(item.tval == TV_ROD) and not(item.tval == TV_ARM_BAND)) then

				valid = 0
			end
		end

		-- Monk
		if (archetype == 7) then

			if(not(item.tval == TV_GLOVES) and not(item.tval == TV_SOFT_ARMOR)) then

				valid = 0
			end
		end

		-- Ranged
		if (archetype == 8) then

			if(not(item.tval == TV_RANGED) and not(item.tval == TV_AMMO)) then

				valid = 0
			end
		end

		-- Evasion
		if (archetype == 9) then

			if(not(item.tval == TV_BOOTS) and not(item.tval == TV_AMMO)) then

				valid = 0
			end
		end

		-- Magic Defense
		if (archetype == 10) then

			if(not(item.tval == TV_SHIELD) and not(item.tval == TV_SOFT_ARMOR) and not(item.tval == TV_HARD_ARMOR) and not(item.tval == TV_DRAG_ARMOR) and not(item.tval == TV_CLOAK) and not(item.tval == TV_HELM)) then

				valid = 0
			end
		end

		-- Three Defenses
		if (archetype == 11) then

			if(not(item.tval == TV_SOFT_ARMOR) and not(item.tval == TV_HARD_ARMOR) and not(item.tval == TV_DRAG_ARMOR) and not(item.tval == TV_HELM) and not(item.tval == TV_CLOAK) and not(item.tval == TV_BOOTS)) then

				valid = 0
			end
		end

		-- Speed
		if (archetype == 12) then

			if(not(item.tval == TV_BOOTS)) then

				valid = 0
			end
		end

		-- Elemental Resilience
		if (archetype == 13) then

			if(not(item.tval == TV_SHIELD) and not(item.tval == TV_SOFT_ARMOR) and not(item.tval == TV_HARD_ARMOR) and not(item.tval == TV_DRAG_ARMOR) and not(item.tval == TV_CLOAK)) then

				valid = 0
			end
		end

		-- Magic Mitigation
		if (archetype == 14) then

			if(not(item.tval == TV_SHIELD) and not(item.tval == TV_SOFT_ARMOR) and not(item.tval == TV_HARD_ARMOR) and not(item.tval == TV_DRAG_ARMOR) and not(item.tval == TV_CLOAK) and not(item.tval == TV_LITE) and not(item.tval == TV_CRYSTAL)) then

				valid = 0
			end
		end

		-- Pure resistance
		if (archetype == 15) then

			if(not(item.tval == TV_SHIELD)) then

				valid = 0
			end
		end

		-- Dex Fighter
		if (archetype == 16) then

			if(not(item.tval == TV_WEAPON) and not(item.tval == TV_BOOTS) and not(item.tval == TV_CLOAK) and not(item.tval == TV_AMMO)) then

				valid = 0
			end
		end

		-- Rogue
		if (archetype == 17) then

			if(not(item.tval == TV_WEAPON) and not(item.tval == TV_RANGED) and not(item.tval == TV_BOOTS) and not(item.tval == TV_CLOAK) and not(item.tval == TV_AMMO)) then

				valid = 0
			end
		end

		-- Throwing
		if (archetype == 18) then

			if(not(item.tval == TV_THROWING)) then

				valid = 0
			end
		end

		-- Bard
		if (archetype == 19) then

			if(not(item.tval == TV_INSTRUMENT)) then

				valid = 0
			end
		end

		-- Bard + Allies
		if (archetype == 20) then

			if(not(item.tval == TV_INSTRUMENT)) then

				valid = 0
			end
		end

		-- Summoner
		if (archetype == 21) then

			if(not(item.tval == TV_ROD) and not(item.tval == TV_LITE) and not(item.tval == TV_INSTRUMENT)) then

				valid = 0
			end
		end

		-- Elemental Lord
		if (archetype == 22) then

			if(not(item.tval == TV_WEAPON) and not(item.tval == TV_ROD) and not(item.tval == TV_ARM_BAND)) then

				valid = 0
			end
		end

		-- Random
		if (archetype == 23) then

			if (item.tval == TV_WEAPON or item.tval == TV_ROD or item.tval == TV_SHIELD or item.tval == TV_RANGED or item.tval == TV_THROWING) then

				valid = 0
			end
		end

		-- Additional validations.

		-- Rings and Amulets can have ANY archetypes!
		if (item.tval == TV_RING or item.tval == TV_AMULET) then valid = 1 end

		if (valid == 1) then chosen = 1 end
	end

	-- Assign the bonus, based on archetype.

	-- Balanced Melee
	if (archetype == 1) then

		local j = 0
		local skill = 0
		i = bonuslevel

		-- The skill is either Fighting, or a random weapon skill.
		-- However, if this is a weapon, get the skill of that weapon.
		if (item.tval == TV_WEAPON or item.tval == TV_ROD) then

			j = lua_randint(2)

			if (j == 1) then skill = 1
			else skill = item.itemskill + 1
			end
		else

			skill = lua_randint(8) + 12
			if (skill == 18) then skill = 30 end
			if (skill == 19) then skill = 5 end
			if (skill == 20) then skill = 1 end
		end
			

		while (i > 0) do

			j = lua_randint(100)
			if (j >= 66) then

				item.statsbonus[A_STR+1] = item.statsbonus[A_STR+1] + 1
				i = i - 1
			elseif (j >= 33) then

				item.statsbonus[A_CON+1] = item.statsbonus[A_CON+1] + 1
				i = i - 1
			else
				item.skillsbonus[skill] = item.skillsbonus[skill] + 1
				i = i - 1
			end
		end
	end

	-- Melee Offense
	if (archetype == 2) then

		local j = 0
		local skill = 0
		i = bonuslevel

		-- The skill is either Fighting, or a random weapon skill.
		-- However, if this is a weapon, get the skill of that weapon.
		if (item.tval == TV_WEAPON or item.tval == TV_ROD) then

			j = lua_randint(2)

			if (j == 1) then skill = 1
			else skill = item.itemskill + 1
			end
		else
			skill = lua_randint(7) + 12
			if (skill == 18) then skill = 30 end
			if (skill == 19) then skill = 1 end
		end
			

		while (i > 0) do

			j = lua_randint(100)
			if (j >= 50) then

				item.statsbonus[A_STR+1] = item.statsbonus[A_STR+1] + 1
				i = i - 1
			else
				item.skillsbonus[skill] = item.skillsbonus[skill] + 1
				i = i - 1
			end
		end
	end

	-- Defense
	if (archetype == 3) then

		local j = 0
		local skill = 0
		i = bonuslevel

		skill = 5

		while (i > 0) do

			j = lua_randint(100)
			if (j >= 50) then

				item.statsbonus[A_CON+1] = item.statsbonus[A_CON+1] + 1
				i = i - 1
			else
				item.skillsbonus[skill] = item.skillsbonus[skill] + 1
				i = i - 1
			end
		end
	end

	-- Int caster or Wis caster.
	if (archetype == 4 or archetype == 5) then

		local j = 0
		local skill = 0
		i = bonuslevel

		skill = lua_randint(6) + 21
		if (skill == 27) then skill = 2 end

		while (i > 0) do

			j = lua_randint(100)
			if (j >= 50) then

				if (archetype == 4) then
					item.statsbonus[A_INT+1] = item.statsbonus[A_INT+1] + 1
					i = i - 1
				else
					item.statsbonus[A_WIS+1] = item.statsbonus[A_WIS+1] + 1
					i = i - 1
				end
			else
				item.skillsbonus[skill] = item.skillsbonus[skill] + 1
				i = i - 1
			end
		end
	end

	-- Mana
	if (archetype == 6) then

		local j = 0
		i = bonuslevel

		while (i > 0) do

			j = lua_randint(100)
			if (j >= 50) then

				item.statsbonus[A_INT+1] = item.statsbonus[A_INT+1] + 1
				i = i - 1
			else
				item.manabonus = item.manabonus + 3
				i = i - 1
			end
		end
	end

	-- Monk
	if (archetype == 7) then

		local j = 0
		local skill = 0
		i = bonuslevel

		skill = 19

		while (i > 0) do

			j = lua_randint(100)
			if (j >= 50) then

				item.statsbonus[A_WIS+1] = item.statsbonus[A_WIS+1] + 1
				i = i - 1
			else
				item.skillsbonus[skill] = item.skillsbonus[skill] + 1
				i = i - 1
			end
		end
	end

	-- Ranged
	if (archetype == 8) then

		local j = 0
		local skill = 0
		i = bonuslevel

		-- Unlike the melee weapons, there is little benefits to not have a specialized
		-- ranged skill on a ranged weapon.
		if (item.tval == TV_RANGED) then

			skill = item.itemskill + 1
		else
			skill = lua_randint(4) + 19
			if (skill == 23) then skill = 3 end
		end
			

		while (i > 0) do

			j = lua_randint(100)
			if (j >= 50) then

				item.statsbonus[A_DEX+1] = item.statsbonus[A_DEX+1] + 1
				i = i - 1
			else
				item.skillsbonus[skill] = item.skillsbonus[skill] + 1
				i = i - 1
			end
		end
	end

	-- Evasion
	if (archetype == 9) then

		local j = 0
		local skill = 0
		i = bonuslevel

		skill = 6

		while (i > 0) do

			j = lua_randint(100)
			if (j >= 50) then

				item.statsbonus[A_DEX+1] = item.statsbonus[A_DEX+1] + 1
				i = i - 1
			else
				item.skillsbonus[skill] = item.skillsbonus[skill] + 1
				i = i - 1
			end
		end
	end

	-- Magic Defense
	if (archetype == 10) then

		local j = 0
		local skill = 0
		i = bonuslevel

		skill = 28

		while (i > 0) do

			j = lua_randint(100)
			if (j >= 50) then

				item.statsbonus[A_CON+1] = item.statsbonus[A_CON+1] + 1
				i = i - 1
			else
				item.skillsbonus[skill] = item.skillsbonus[skill] + 1
				i = i - 1
			end
		end
	end

	-- Three Defenses
	if (archetype == 11) then

		local j = 0
		i = bonuslevel

		while (i > 0) do

			j = lua_randint(100)
			if (j >= 66) then

				item.skillsbonus[5] = item.skillsbonus[5] + 1
				i = i - 1
			elseif (j >= 33) then

				item.skillsbonus[6] = item.skillsbonus[6] + 1
				i = i - 1
			else
				item.skillsbonus[28] = item.skillsbonus[28] + 1
				i = i - 1
			end
		end
	end

	-- Speed
	if (archetype == 12) then

		local j = 0
		i = bonuslevel

		while (i > 0) do

			j = lua_randint(100)
			if (j >= 50) then

				item.skillsbonus[6] = item.skillsbonus[6] + 1
				i = i - 1
			else
				item.speedbonus = item.speedbonus + 1
				i = i - 2
			end
		end
	end

	-- Elemental Resilience
	if (archetype == 13) then

		local j = 0
		local resist = 0
		i = bonuslevel

		resist = lua_randint(15)+1

		while (i > 0) do

			j = lua_randint(100)
			if (j >= 50 or item.resistances[resist] >= 25) then

				item.statsbonus[A_CON+1] = item.statsbonus[A_CON+1] + 1
				i = i - 1
			else
				item.resistances[resist] = item.resistances[resist] + 1
				if (item.resistances[resist] > 100) then item.resistances[resist] = 100 end
				i = i - 1
			end
		end
	end

	-- Magic Mitigation
	if (archetype == 14) then

		local j = 0
		local resist = 0
		i = bonuslevel

		resist = lua_randint(15)+1

		while (i > 0) do

			j = lua_randint(100)
			if (j >= 50 or item.resistances[resist] >= 25) then

				item.skillsbonus[28] = item.skillsbonus[28] + 1
				i = i - 1
			else
				item.resistances[resist] = item.resistances[resist] + 1
				if (item.resistances[resist] > 100) then item.resistances[resist] = 100 end
				i = i - 1
			end
		end
	end

	-- Pure resistance
	if (archetype == 15) then

		local j = 0
		local resist = 0
		i = bonuslevel

		resist = lua_randint(15)+1

		while (i > 0) do

			if (item.resistances[resist] >= 50) then

				item.statsbonus[A_CON+1] = item.statsbonus[A_CON+1] + 1
				i = i - 1
			else
				item.resistances[resist] = item.resistances[resist] + 1
				if (item.resistances[resist] > 50) then item.resistances[resist] = 50 end
				i = i - 1
			end
		end
	end

	-- Dex Fighter
	if (archetype == 16) then

		local j = 0
		local skill = 0
		i = bonuslevel

		-- The skill is either Fighting, or a random weapon skill.
		-- However, if this is a weapon, get the skill of that weapon.
		if (item.tval == TV_WEAPON or item.tval == TV_ROD) then

			j = lua_randint(2)

			if (j == 1) then skill = 1
			else skill = item.itemskill + 1
			end
		else

			skill = lua_randint(9) + 12
			if (skill == 18) then skill = 30 end
			if (skill == 19) then skill = 9 end
			if (skill == 20) then skill = 1 end
			if (skill == 21) then skill = 6 end
		end


		while (i > 0) do

			j = lua_randint(100)
			if (j >= 66) then

				item.statsbonus[A_STR+1] = item.statsbonus[A_STR+1] + 1
				i = i - 1
			elseif (j >= 33) then

				item.statsbonus[A_DEX+1] = item.statsbonus[A_CON+1] + 1
				i = i - 1
			else
				item.skillsbonus[skill] = item.skillsbonus[skill] + 1
				i = i - 1
			end
		end
	end

	-- Rogue
	if (archetype == 17) then

		local j = 0
		i = bonuslevel

		while (i > 0) do

			j = lua_randint(100)
			if (j >= 66) then

				item.statsbonus[A_DEX+1] = item.statsbonus[A_DEX+1] + 1
				i = i - 1
			elseif (j >= 33) then

				item.skillsbonus[6] = item.skillsbonus[6] + 1
				i = i - 1
			else
				item.skillsbonus[7] = item.skillsbonus[7] + 1
				i = i - 1
			end
		end
	end

	-- Throwing
	if (archetype == 18) then

		local j = 0
		local skill = 0
		i = bonuslevel

		skill = 4

		while (i > 0) do

			j = lua_randint(100)
			if (j >= 50) then

				item.statsbonus[A_STR+1] = item.statsbonus[A_STR+1] + 1
				i = i - 1
			else
				item.skillsbonus[skill] = item.skillsbonus[skill] + 1
				i = i - 1
			end
		end
	end

	-- Bard
	if (archetype == 19) then

		local j = 0
		local skill = 0
		i = bonuslevel

		skill = 29

		while (i > 0) do

			j = lua_randint(100)
			if (j >= 50) then

				item.statsbonus[A_CHR+1] = item.statsbonus[A_CHR+1] + 1
				i = i - 1
			else
				item.skillsbonus[skill] = item.skillsbonus[skill] + 1
				i = i - 1
			end
		end
	end

	-- Bard + Allies
	if (archetype == 20) then

		local j = 0
		local skill = 0
		i = bonuslevel

		while (i > 0) do

			j = lua_randint(100)
			if (j >= 50) then

				item.statsbonus[A_CHR+1] = item.statsbonus[A_CHR+1] + 1
				i = i - 1
			elseif (j >= 33) then

				item.skillsbonus[10] = item.skillsbonus[10] + 1
				i = i - 1
			else
				item.skillsbonus[29] = item.skillsbonus[29] + 1
				i = i - 1
			end
		end
	end

	-- Summoner
	if (archetype == 21) then

		local j = 0
		local skill = 0
		i = bonuslevel

		while (i > 0) do

			j = lua_randint(100)
			if (j >= 75) then

				item.statsbonus[A_CHR+1] = item.statsbonus[A_CHR+1] + 1
				i = i - 1
			elseif (j >= 50) then

				item.statsbonus[A_INT+1] = item.statsbonus[A_INT+1] + 1
				i = i - 1
			elseif (j >= 25) then

				item.skillsbonus[26] = item.skillsbonus[26] + 1
				i = i - 1
			else
				item.skillsbonus[10] = item.skillsbonus[10] + 1
				i = i - 1
			end
		end
	end

	-- Elemental Lord
	if (archetype == 22) then

		local j = 0
		local skill = 0
		i = bonuslevel

		-- Either Elemental or Weapon skill.
		if (item.tval == TV_WEAPON or item.tval == TV_ROD) then

			j = lua_randint(2)

			if (j == 1) then skill = 23
			else skill = item.itemskill + 1
			end
		else

			skill = lua_randint(7) + 12
			if (skill == 19) then skill = 23 end
		end
			

		while (i > 0) do

			j = lua_randint(100)
			if (j >= 66) then

				item.statsbonus[A_STR+1] = item.statsbonus[A_STR+1] + 1
				i = i - 1
			elseif (j >= 33) then

				item.statsbonus[A_INT+1] = item.statsbonus[A_INT+1] + 1
				i = i - 1
			else
				item.skillsbonus[skill] = item.skillsbonus[skill] + 1
				i = i - 1
			end
		end
	end

	-- Random
	if (archetype == 23) then

		local j = 0
		local stat1 = 0
		local stat2 = 0
		local skill1 = 0
		local skill2 = 0
		i = bonuslevel

		-- Pick two random stats.
		-- Can possibly be the same.
		stat1 = lua_randint(6)
		stat2 = lua_randint(6)

		-- Pick two random skills.
		skill1 = lua_randint(30)
		skill2 = lua_randint(30)

		-- We do not want to have two specialized weapon skills on the same item.
		if ((((skill1 >= 13 and skill1 <= 18) and (skill2 >= 13 and skill2 <= 18)) or ((skill1 >= 20 and skill1 <= 22) and (skill2 >= 20 and skill2 <= 22)) or ((skill1 == 30) and (skill2 == 30))) and (not(skill1 == skill2))) then

			skill1 = skill2
		end

		while (i > 0) do

			j = lua_randint(100)
			if (j >= 75) then

				item.statsbonus[stat1] = item.statsbonus[stat1] + 1
				i = i - 1
			elseif (j >= 50) then

				item.statsbonus[stat2] = item.statsbonus[stat2] + 1
				i = i - 1
			elseif (j >= 25) then

				item.skillsbonus[skill1] = item.skillsbonus[skill1] + 1
				i = i - 1
			else
				item.skillsbonus[skill2] = item.skillsbonus[skill2] + 1
				i = i - 1
			end
		end
	end


	-- After it's done, possibly add some additional bonus.
	i = (lua_randint(2) - 1) + lua_randint((bonuslevel / 20))

	while (i > 0) do

		local bonus

		bonus = lua_randint(14)

		if (bonus == 1) then give_object_flag2(item, TR2_RES_FEAR) end
		if (bonus == 2) then give_object_flag2(item, TR2_RES_CONF) end
		if (bonus == 3) then give_object_flag2(item, TR2_RES_BLIND) end
		if (bonus == 4) then give_object_flag2(item, TR2_HOLD_LIFE) end
		if (bonus == 5) then give_object_flag4(item, TR4_SAFETY) end
		if (bonus == 6) then give_object_flag2(item, TR2_SUST_STR) end
		if (bonus == 7) then give_object_flag2(item, TR2_SUST_INT) end
		if (bonus == 8) then give_object_flag2(item, TR2_SUST_WIS) end
		if (bonus == 9) then give_object_flag2(item, TR2_SUST_DEX) end
		if (bonus == 10) then give_object_flag2(item, TR2_SUST_CON) end
		if (bonus == 11) then give_object_flag2(item, TR2_SUST_CHR) end
		if (bonus == 12) then give_object_flag3(item, TR3_TELEPATHY) end
		if (bonus == 13) then

			item.light = item.light + lua_randint(bonuslevel / 5) + 1
			if (item.light > 5) then item.light = 5 end
		end
		if (bonus == 14) then

			item.reflect = item.reflect + (bonuslevel / 3)
		end

		i = i - 1
	end
end

add_event_handler("give_item_bonus", give_item_bonus)
