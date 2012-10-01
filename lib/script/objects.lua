-- File: objects.lua
-- This file contains various scripts and codes related to objects.
-- Includes magic item generation codes, rods code, etc...

-- This function will generate a magic item.
-- By default, itemlevel is equal to dungeon's level, but you can specify a different
-- value.
function make_item_magic (item, itemlevel, special)

	local oldpval
	local bonuslevel
	local unusual
	local i
	local isspecial
	local cursebadluck
	local notred
	local bluechance
	local bluebonus

	isspecial = 0
	cursebadluck = 0
	crappy = 0
	bluechance = 0
	bluebonus = 0

	-- Reset all bonus to default.

	-- Exception for the Blade of Purity.
	if not(item.tval == TV_WEAPON and item.sval == 44) then
		oldpval = item.pval
        	item.name2 = 0
        	item.to_h = kind(item).to_h
        	item.to_d = kind(item).to_d
        	item.to_a = kind(item).to_a
		item.ac = kind(item).ac
        	item.pval = 0
        	item.dd = kind(item).dd
        	item.ds = kind(item).ds
		item.brandtype = kind(item).brandtype
		item.branddam = kind(item).branddam
		item.brandrad = kind(item).brandrad
		item.tweakpoints = 0
	end

	-- Twist Fate: Items can affect the item's rank.
	if (p_ptr.inside_quest == 0 and p_ptr.events[29007] == 1) then

		itemlevel = itemlevel + fate_items(3)
		if (itemlevel <= 0) then itemlevel = 1 end
	end

	-- Cursed Treasure Seeker ability.
	if (p_ptr.abilities[(CLASS_NIGHT1 * 10) + 1] > 0 and p_ptr.cursed > 0) then

		local cursedbonus

		cursedbonus = (p_ptr.cursed / 10) + multiply_divide((p_ptr.cursed / 10), p_ptr.abilities[(CLASS_NIGHT1 * 10) + 1] * 20, 100)
		bluebonus = (p_ptr.cursed / 20) * p_ptr.abilities[(CLASS_NIGHT1 * 10) + 1]
		itemlevel = itemlevel + cursedbonus
	end

	-- Cursed players may have more "unusually bad" items...
	if (p_ptr.cursed > 0 and p_ptr.inside_quest == 0) then

		cursebadluck = (p_ptr.cursed / 10)
		if (cursebadluck > 70) then cursebadluck = 70 end
	end

	-- Most of the time, you get an item with a "bonus level" between itemlevel and (itemlevel * 2).
	-- However, sometimes, you can get really GOOD stuff. :)
	-- ....or really bad stuff. :(
	bonuslevel = lua_randint(itemlevel) + itemlevel
	unusual = lua_randint(100)
	if (unusual >= 95) then bonuslevel = bonuslevel + lua_randint(bonuslevel)
	elseif (unusual <= (5 + cursebadluck) and (forcecurse == 0)) then

		-- Cursed Treasure Seeker may negates this.
		if (p_ptr.abilities[(CLASS_NIGHT1 * 10) + 1] < 5) then

			bonuslevel = bonuslevel / 4

			-- It is crappy.
			crappy = 1
		end
	end

	-- bonuslevel cannot be 0.
	if (bonuslevel <= 0) then bonuslevel = 1 end

	-- Item is magic.
	give_object_flag4(item, TR4_ENCHANTED)

	-- If bonuslevel is at least 40, 10% of being able to level up.
	if ((bonuslevel >= 40 and lua_randint(100) <= (10 + bluebonus)) or (special) or (fate_item_modifier >= 3 and p_ptr.events[29007] == 1) or (p_ptr.events[29023] == 1)) then

		give_object_flag4(item, TR4_LEVELS)
		item.level = 1
		item.kills = 0
		item.tweakpoints = 2
	end

	-- If Chargeable, there is a tiny chance of it becoming special.
	-- Bonuslevel must also be at least 80.
	if ((get_object_flag4(item, TR4_LEVELS) and bonuslevel >= 80 and item.cursed == 0 and crappy == 0 and lua_randint(500) <= 5) or (special) or (fate_item_modifier == 4 and p_ptr.events[29007] == 1) or (p_ptr.events[29025] == 1)) then

		local rndval
		local rndval2
		local newname1
		local newname2
		local randname

		-- Reroll bonuslevel to max it.
		bonuslevel = itemlevel * 2
		bonuslevel = bonuslevel * 2

		item.name2 = 131
		bonuslevel = bonuslevel * 3

		-- Set the "special" to true.
		isspecial = 1

		rndval = lua_randint(200)
                if (rndval <= 10) then newname1 = "'Havoc"
                elseif (rndval <= 20) then newname1 = "'Twilight"
                elseif (rndval <= 30) then newname1 = "'Holy"
                elseif (rndval <= 40) then newname1 = "'Sacred"
                elseif (rndval <= 50) then newname1 = "'Heavenly"
                elseif (rndval <= 60) then newname1 = "'Ancient"
                elseif (rndval <= 70) then newname1 = "'Hell"
                elseif (rndval <= 80) then newname1 = "'Raging"
                elseif (rndval <= 90) then newname1 = "'Soul"
                elseif (rndval <= 100) then newname1 = "'Divine"
                elseif (rndval <= 110) then newname1 = "'Doom"
                elseif (rndval <= 120) then newname1 = "'Infinity"
                elseif (rndval <= 130) then newname1 = "'Mystic"
                elseif (rndval <= 140) then newname1 = "'Demonic"
                elseif (rndval <= 150) then newname1 = "'Chaos"
                elseif (rndval <= 160) then newname1 = "'Ethereal"
                elseif (rndval <= 170) then newname1 = "'Dimension"
                elseif (rndval <= 180) then newname1 = "'Nightmare"
                elseif (rndval <= 190) then newname1 = "'Furious"
                else newname1 = "'Galactic" end
                rndval2 = lua_randint(200)
                if (rndval2 <= 10) then newname2 = " Slayer'"
                elseif (rndval2 <= 20) then newname2 = " Defender'"
                elseif (rndval2 <= 30) then newname2 = " Protector'"
                elseif (rndval2 <= 40) then newname2 = " Blaze'"
                elseif (rndval2 <= 50) then newname2 = " Storm'"
                elseif (rndval2 <= 60) then newname2 = " Breaker'"
                elseif (rndval2 <= 70) then newname2 = " Incarnate'"
                elseif (rndval2 <= 80) then newname2 = " Messenger'"
                elseif (rndval2 <= 90) then newname2 = " Song'"
                elseif (rndval2 <= 100) then newname2 = " Melody'"
                elseif (rndval2 <= 110) then newname2 = " Guardian'"
                elseif (rndval2 <= 120) then newname2 = " Hurricane'"
                elseif (rndval2 <= 130) then newname2 = " Star'"
                elseif (rndval2 <= 140) then newname2 = " Entity'"
                elseif (rndval2 <= 150) then newname2 = " Punisher'"
                elseif (rndval2 <= 160) then newname2 = " Inquisitor'"
                elseif (rndval2 <= 170) then newname2 = " Disaster'"
                elseif (rndval2 <= 180) then newname2 = " Juggernaut'"
                elseif (rndval2 <= 190) then newname2 = " Force'"
                else newname2 = " Distortion'" end

		randname = string.format('%s%s', newname1, newname2)
                        
                item.art_name = quark_add(randname)

		-- Specials are always eternal.
		give_object_flag4(item, TR4_ETERNAL)
	end

	-- A cursed item!
	-- Already cursed players have more chances of finding cursed!
	-- They won't be generated in towns. So black markets won't sell them.
	-- Specials will never become cursed.
	-- And there is no such thing such as a "crappy" cursed item.
	if (((bonuslevel >= 20 and lua_randint(100) <= (5 + cursebadluck)) and (isspecial == 0) and (magicitemscroll == 0) and (crappy == 0) and (dun_level > 0)) or (forcecurse == 1) or (p_ptr.events[29024] == 1)) then

		item.cursed = lua_randint(bonuslevel) + (itemlevel / 4)

		if (forcecurse == 1 and item.cursed < itemlevel) then item.cursed = itemlevel end

		-- They are eternal.
		-- This raises your chance that you will use them. Hehehe...
		give_object_flag4(item, TR4_ETERNAL)

		-- Bonuslevel is increased slightly.
		bonuslevel = bonuslevel + item.cursed
	end

	-- Provide bonus up to bonuslevel.
	i = 0

	while (i < bonuslevel) do

		local btype
		local amt
		local whichone
		local misctype

		whichone = 0

		-- Roll for what type of bonus it will be.
		btype = lua_randint(100)

		-- A resistances bonus.
		if (btype >= 80) then

			-- Determine a random amount.
			amt = lua_randint(bonuslevel - i)

			amt = amt / 3

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
			amt = lua_randint(bonuslevel - i)

			amt = amt / 5
			if (amt <= 0) then amt = 1 end

			-- Apply it to a random stat.
			whichone = lua_randint(6)
			item.statsbonus[whichone] = item.statsbonus[whichone] + amt

			-- Increase i.
			i = i + (amt * 10)

		-- A skills bonus.
		elseif (btype >= 40) then

			-- Random amount.
			amt = lua_randint(bonuslevel - i)

			amt = amt / 5
			if (amt <= 0) then amt = 1 end

			-- Special items are always 'themed' with your character.
			if (isspecial == 1) then

				local a
				local b
				local c
				local d

				-- First check if we do have SOME skills.
				-- We SHOULD...but just in case...
				-- Also, we skip alchemy and crafting.
				d = 0
				for c = 1, 30 do

					if (p_ptr.skill[c] > 0 and not(c == 11) and not(c == 12)) then d = 1 end
				end

				-- We have NO skills?? Oh well...just pick something random.
				if (d == 0) then
					while (whichone == 0 or whichone == 11 or whichone == 12) do
						whichone = lua_randint(30)
					end
				else

					a = 0
					while (a == 0) do

						b = lua_randint(30)
						if (p_ptr.skill[b] > 0) then

							whichone = b
							a = 1
						end
					end
				end
			else

				-- Apply it to a random skill.
				while (whichone == 0 or whichone == 11 or whichone == 12) do
					whichone = lua_randint(30)
				end
			end

			-- phlinn's code snippet.
			if (item.tval == TV_WEAPON or item.tval == TV_ROD or item.tval == TV_RANGED) then
				if (whichone >= 13 and whichone <= 22) then 
					whichone = item.itemskill + 1
				end
			end

			item.skillsbonus[whichone] = item.skillsbonus[whichone] + amt

			-- Increase i.
			i = i + (amt * 10)

		-- A misc bonus.
		elseif (btype >= 20) then

			-- What type of "misc" bonus do you get.
			if (item.tval == TV_ROD or item.tval == TV_ARM_BAND) then misctype = lua_randint(6)
			else misctype = lua_randint(5) end

			-- The amount of "i" that it costs depends on the bonus you get.

			-- Extra blows or extra shots.
			if (misctype == 1) then

				if (item.tval == TV_RANGED or item.tval == TV_THROWING) then

					-- Random amount.
					amt = lua_randint(bonuslevel - i)

					amt = amt / 20
					if (amt <= 0) then amt = 1 end

					-- Apply it.
					item.extrashots = item.extrashots + amt

					-- Increase i.
					i = i + (amt * 25)

				else

					-- Random amount.
					amt = lua_randint(bonuslevel - i)

					amt = amt / 10
					if (amt <= 0) then amt = 1 end

					-- Apply it.
					item.extrablows = item.extrablows + amt

					-- Increase i.
					i = i + (amt * 14)
				end
			end

			-- Speed bonus.
			if (misctype == 2) then

				-- Random amount.
				amt = lua_randint(bonuslevel - i)

				amt = amt / 6
				if (amt <= 0) then amt = 1 end

				-- Apply it.
				item.speedbonus = item.speedbonus + amt

				-- Increase i.
				i = i + (amt * 10)
			end

			-- Life bonus.
			if (misctype == 3) then

				-- Random amount.
				amt = lua_randint(bonuslevel - i)

				-- Apply it.
				item.lifebonus = item.lifebonus + amt

				-- Increase i.
				i = i + amt
			end

			-- Light
			if (misctype == 4) then

				-- Random amount.
				amt = lua_randint(bonuslevel - i)

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
				amt = lua_randint(bonuslevel - i)

				-- Apply it.
				item.reflect = item.reflect + amt

				-- Increase i.
				i = i + amt
			end

			-- Mana bonus or extra shots.
			if (misctype == 6) then

				-- Random amount.
				amt = lua_randint(bonuslevel - i)

				-- Apply it.
				item.manabonus = item.manabonus + amt

				-- Increase i.
				i = i + amt
			end

		-- Gain a flag.
		else

			-- Pick a flag
			misctype = lua_randint(13)

			-- Give the flag. i cost varies per flags.

			-- Resist fear.
			if (misctype == 1) then

				give_object_flag2(item, TR2_RES_FEAR)
				i = i + 5
			end

			-- Resist conf.
			if (misctype == 2) then

				give_object_flag2(item, TR2_RES_CONF)
				i = i + 15
			end

			-- Resist blind.
			if (misctype == 3) then

				give_object_flag2(item, TR2_RES_BLIND)
				i = i + 5
			end

			-- Hold life.
			if (misctype == 4) then

				give_object_flag2(item, TR2_HOLD_LIFE)
				i = i + 10
			end

			-- Safety.
			if (misctype == 5) then

				give_object_flag4(item, TR4_SAFETY)
				i = i + 15
			end

			-- Sustain Strength.
			if (misctype == 6) then

				give_object_flag2(item, TR2_SUST_STR)
				i = i + 5
			end

			-- Sustain Intelligence.
			if (misctype == 7) then

				give_object_flag2(item, TR2_SUST_INT)
				i = i + 5
			end

			-- Sustain Wisdom.
			if (misctype == 8) then

				give_object_flag2(item, TR2_SUST_WIS)
				i = i + 5
			end

			-- Sustain Dexterity.
			if (misctype == 9) then

				give_object_flag2(item, TR2_SUST_DEX)
				i = i + 5
			end

			-- Sustain Constitution.
			if (misctype == 10) then

				give_object_flag2(item, TR2_SUST_CON)
				i = i + 5
			end

			-- Sustain Charisma.
			if (misctype == 11) then

				give_object_flag2(item, TR2_SUST_CHR)
				i = i + 5
			end

			-- Telepathy.
			if (misctype == 12) then

				give_object_flag3(item, TR3_TELEPATHY)
				i = i + 15
			end

			-- Regen.
			if (misctype == 13) then

				give_object_flag3(item, TR3_REGEN)
				i = i + 10
			end
		end

	end

	-- For damages/ac purposes, special items have normal bonus, albeit maxed.
	if (isspecial == 1) then bonuslevel = bonuslevel / 3 end

	-- Give some to_h and to_d. Can give some no matter the item's type.
	if (isspecial == 1) then
		item.to_h = item.to_h + bonuslevel
		item.to_d = item.to_d + bonuslevel
	else
		item.to_h = item.to_h + lua_randint(bonuslevel)
		item.to_d = item.to_d + lua_randint(bonuslevel)
	end

	if (isspecial == 1) then

		item.to_h = item.to_h + bonuslevel
		item.to_d = item.to_d + bonuslevel
	end

	-- Some items gets bonus damages.
	if (item.tval == TV_WEAPON or item.tval == TV_ROD or item.tval == TV_AMMO or item.tval == TV_THROWING) then

		local y
		local x
		local percentincrease

		if (isspecial == 1) then

			item.dd = item.dd + (item.dd / 10) + 1
			item.ds = item.ds + (item.ds / 10) + 1
		end

		x = 0
		percentincrease = 0
		if (isspecial == 1) then
			y = bonuslevel * 2
		else
			y = lua_randint(bonuslevel * 2)
		end

		while (x <= y) do
			if (x <= 20) then percentincrease = percentincrease + 1 end
			if (x <= 15) then percentincrease = percentincrease + 1 end
			if (x <= 10) then percentincrease = percentincrease + 1 end
			if (x <= 5) then percentincrease = percentincrease + 1 end

			x = x + 1
		end
		percentincrease = percentincrease + (y / 2)
			
		if (isspecial == 1) then
			item.dd = item.dd + ((item.dd * percentincrease) / 100)
			item.ds = item.ds + ((item.ds * percentincrease) / 100)
		else
			item.dd = item.dd + ((item.dd * lua_randint(percentincrease)) / 100)
			item.ds = item.ds + ((item.ds * lua_randint(percentincrease)) / 100)
		end

	end

	-- Weapons and gloves can gain brands.
	if (item.tval == TV_WEAPON or item.tval == TV_ROD or item.tval == TV_AMMO or item.tval == TV_THROWING or item.tval == TV_GLOVES) then

		-- Specials always gain it.
		if ((lua_randint(100) >= 75) or (isspecial == 1)) then

			local newdam

			newdam = 0

			if (item.brandtype == 0) then item.brandtype = lua_randint(11) + 1 end

			if (isspecial == 1) then
				newdam = (bonuslevel * 50) + ((bonuslevel * 50) / 4)
			else
				newdam = lua_randint((bonuslevel * 50)) + ((bonuslevel * 50) / 4)
			end
			if (newdam > item.branddam) then item.branddam = newdam end
		end
	end

	-- Anything that has base AC can get a bonus.
	if (item.ac > 0) then

		local y
		local x
		local percentincrease

		x = 0
		percentincrease = 0
		if (isspecial == 1) then
			y = bonuslevel * 2
		else
			y = lua_randint(bonuslevel * 2)
		end

		while (x <= y) do
			if (x <= 20) then percentincrease = percentincrease + 1 end
			if (x <= 15) then percentincrease = percentincrease + 1 end
			if (x <= 10) then percentincrease = percentincrease + 1 end
			if (x <= 5) then percentincrease = percentincrease + 1 end

			x = x + 1
		end
		percentincrease = percentincrease + (y / 2)

		if (isspecial == 1) then
			item.ac = item.ac + ((item.ac * (percentincrease)) / 100)
			item.to_a = item.to_a + (bonuslevel)
		else
			item.ac = item.ac + ((item.ac * lua_randint(percentincrease)) / 100)
			item.to_a = item.to_a + lua_randint(bonuslevel)
		end

		if (isspecial == 1) then

			item.ac = item.ac + (bonuslevel / 4)
			item.to_a = item.to_a + bonuslevel
		end
	end

	-- Hey! Those red items are great!
	if (item.cursed > 0) then

		local c

		-- All existing stats and skills bonus are augmented by the curse's effect.
		-- Increase is at least 1.
		for c = 1, 6 do

			if (item.statsbonus[c] > 0) then
				item.statsbonus[c] = item.statsbonus[c] + multiply_divide(item.statsbonus[c], item.cursed, 100) + (item.cursed / 5) + 1
			end
		end
		for c = 1, 28 do

			if (item.skillsbonus[c] > 0) then
				item.skillsbonus[c] = item.skillsbonus[c] + multiply_divide(item.skillsbonus[c], item.cursed, 100) + (item.cursed / 5) + 1
			end
		end

		-- Same thing goes for the misc bonus, though a bit less.
		if (item.extrablows > 0) then
			item.extrablows = item.extrablows + multiply_divide(item.extrablows, item.cursed / 4, 100) + 1
		end
		if (item.extrashots > 0) then
			item.extrashots = item.extrashots + multiply_divide(item.extrashots, item.cursed / 8, 100) + 1
		end
		if (item.speedbonus > 0) then
			item.speedbonus = item.speedbonus + multiply_divide(item.speedbonus, item.cursed, 100) + 1
		end
		if (item.reflect > 0) then
			item.reflect = item.reflect + multiply_divide(item.reflect, item.cursed, 100) + item.cursed + 20
		end
		if (item.light > 0) then
			item.light = 5
		end
		if (item.lifebonus > 0) then
			item.lifebonus = item.lifebonus + item.cursed + 10
		end
		if (item.manabonus > 0) then
			item.manabonus = item.manabonus + item.cursed + 10
		end

		-- All base values of stuff is raised!
		if (item.dd > 0) then item.dd = item.dd + multiply_divide(item.dd, item.cursed, 100) + 1 end
		if (item.ds > 0) then item.ds = item.ds + multiply_divide(item.ds, item.cursed, 100) + 1 end
		if (item.ac > 0) then item.ac = item.ac + multiply_divide(item.ac, item.cursed, 100) end
		if (item.to_h > 0) then item.to_h = item.to_h + multiply_divide(item.to_h, item.cursed + (item.cursed / 2), 100) + (item.cursed / 2) end
		if (item.to_d > 0) then item.to_d = item.to_d + multiply_divide(item.to_d, item.cursed + (item.cursed / 2), 100) + (item.cursed / 2) end
		if (item.to_a > 0) then item.to_a = item.to_a + multiply_divide(item.to_a, item.cursed + (item.cursed / 2), 100) + (item.cursed / 2) end

	end

	-- Make sure rods have one activation.
	if (item.tval == TV_ROD and get_object_flag3(item, TR3_ACTIVATE)) then

		if (item.spell[1].type == 0) then prepare_rods_activations(item, itemlevel) end
	end


	-- Reset some events.
	p_ptr.events[29023] = 0
	p_ptr.events[29024] = 0
	p_ptr.events[29025] = 0

end

-- The code for Rods zapping!
-- Dual zapping fixed by Khan.
function zap_rod ()


	local dir
	local dam
	local i
	local j

	-- First, let's check if the player is wearing a rod
	-- Any rods in any hands will do.
	if (not(inven(INVEN_WIELD).tval == TV_ROD) and not(inven(INVEN_WIELD + 1).tval == TV_ROD)) then

		msg_print("You must wield a rod.")
		return
	else

		--Now, check for a crystal */
		if (not(inven(INVEN_TOOL).tval == TV_CRYSTAL)) then

			msg_print("You must be using a crystal!")
			return

		else

			local r

			if (inven(INVEN_WIELD).tval == TV_ROD and inven(INVEN_WIELD + 1).tval == TV_ROD) then 
				i = 0
				j = 1
			elseif (inven(INVEN_WIELD).tval == TV_ROD) then
				i = 0
				j = 0
			else
				i = 1
				j = 1
			end

			for r = i, j do 

				-- Is the crystal charged?
				if (inven(INVEN_TOOL).pval <= 0) then

					msg_print("This crystal has no charges left.")
					return
				else
					local bonus
					local rodbonus

					rodbonus = damroll(inven(INVEN_WIELD + r).dd, inven(INVEN_WIELD + r).ds)
					rodbonus = rodbonus + multiply_divide(rodbonus, p_ptr.skill[18] * 3, 100)

					bonus = (p_ptr.skill[18] * 20) + (p_ptr.skill[2] * 10)
					dam = (inven(INVEN_TOOL).branddam + rodbonus) * (p_ptr.skill[18] + 1)
					dam = dam + multiply_divide(dam, bonus, 100)
					dam = dam + multiply_divide(dam, p_ptr.to_s, 100)
					dir = lua_get_aim_dir()
					ignore_spellcraft = TRUE
					fire_ball(inven(INVEN_TOOL).brandtype, dir, dam, inven(INVEN_TOOL).brandrad)
					ignore_spellcraft = FALSE
					if (p_ptr.skill[18] < 15) then inven(INVEN_TOOL).pval = inven(INVEN_TOOL).pval - 1 end
					update_and_handle()
					energy_use = 100
				end
			end
		end
	end
end


-- The code for scrolls!
-- Function should return 1 if the scroll is used up.
function read_scroll (scroll)

	-- Scrolls effects are based on their sval.

	-- Object Detection.
	if (scroll.sval == 1) then
		detect_objects_gold()
		detect_objects_normal()
		detect_objects_magic()
	end

	-- Blessing.
	if (scroll.sval == 2) then
		set_blessed(10)
	end

	-- Detect Invisible.
	if (scroll.sval == 3) then
		detect_monsters_invis()
	end

	-- Identify.
	if (scroll.sval == 4) then
		if (not(ident_spell())) then return 0 end
	end

	-- Light.
	if (scroll.sval == 5) then
		no_magic_return = TRUE
		attack_aura(GF_LITE, 0, 15)
		no_magic_return = FALSE
	end

	-- Phase Door.
	if (scroll.sval == 6) then
		teleport_player(10)
	end

	-- Door/Stairs Location.
	if (scroll.sval == 7) then
		detect_doors()
		detect_stairs()
	end

	-- Magic Mapping.
	if (scroll.sval == 8) then
		map_area()
	end

	-- Trap Detection.
	if (scroll.sval == 10) then
		detect_traps()
	end

	-- Word of Recall.
	if (scroll.sval == 11) then
		recall_player()
	end

	-- Holy Chant.
	if (scroll.sval == 12) then
		set_blessed(25)
	end

	-- Teleportation.
	if (scroll.sval == 13) then
		teleport_player(100)
	end

	-- Holy Prayer.
	if (scroll.sval == 14) then
		set_blessed(50)
	end

	-- *Artifact Identify*.
	if (scroll.sval == 15) then
		if (not(identify_fully())) then return 0 end
	end

	-- Recharge Crystal.
	if (scroll.sval == 16) then
		recharge_crystal()
	end

	-- Magic Item.
	if (scroll.sval == 17) then

		local item

		item = lua_pick_item(0)

		if (not(item)) then return 0 end

		if (get_object_flag4(item, TR4_ENCHANTED) or item.name1 > 0) then

			msg_print("This item is already magical.")
			return 0
		end

		if (item.tval == TV_ESSENCE) then

			msg_print("You cannot use this on Essences.")
			return
		end

		magicitemscroll = 1
		make_item_magic(item, p_ptr.lev, FALSE)
		magicitemscroll = 0
	end

	-- Object Eternality.
	if (scroll.sval == 18) then
		object_eternality()
	end

	-- Item Leveling
	if (scroll.sval == 19) then
		if (not(make_item_levelable())) then return 0 end
	end

	-- Enchanter's Blessing
	if (scroll.sval == 20) then

		local item

		item = lua_pick_item(0)

		if (not(item)) then return 0 end

		if (get_object_flag4(item, TR4_CRAFTED)) then

			msg_print("This item is already an enchanted crafted item.")
			return 0
		end

		if (item.tval == TV_ESSENCE) then

			msg_print("You cannot use this on Essences.")
			return
		end

		give_object_flag4(item, TR4_CRAFTED)
		msg_print("This item is now treated as an enchanted crafted item!")
	end


	-- By default, use up the scroll.
	return 1

end

-- Licialhyds codes is now scripted.
function use_licialhyd ()

	local item

	item = lua_get_item(TV_LICIALHYD)

	if (item == -1) then return end

	-- Healing
	if (inven(item).pval2 == 1) then

		p_ptr.chp = p_ptr.chp + inven(item).pval3
		if (p_ptr.chp > p_ptr.mhp) then p_ptr.mhp = p_ptr.chp end
		msg_print("Your wounds are healed!")
		update_and_handle()
	end

	-- Mana
	if (inven(item).pval2 == 2) then

		p_ptr.csp = p_ptr.csp + inven(item).pval3
		if (p_ptr.csp > p_ptr.msp) then p_ptr.msp = p_ptr.csp end
		msg_print("Your mana is restored!")
		update_and_handle()
	end

	-- Strength
	if (inven(item).pval2 == 3) then

		p_ptr.str_boost = inven(item).pval3
                set_str_boost(inven(item).pval3)
		update_and_handle()
	end

	-- Intelligence
	if (inven(item).pval2 == 4) then

		p_ptr.int_boost = inven(item).pval3
                set_int_boost(inven(item).pval3)
		update_and_handle()
	end

	-- Wisdom
	if (inven(item).pval2 == 5) then

		p_ptr.wis_boost = inven(item).pval3
                set_wis_boost(inven(item).pval3)
		update_and_handle()
	end

	-- Dexterity
	if (inven(item).pval2 == 6) then

		p_ptr.dex_boost = inven(item).pval3
                set_dex_boost(inven(item).pval3)
		update_and_handle()
	end

	-- Constitution
	if (inven(item).pval2 == 7) then

		p_ptr.con_boost = inven(item).pval3
                set_con_boost(inven(item).pval3)
		update_and_handle()
	end

	-- Charisma
	if (inven(item).pval2 == 8) then

		p_ptr.chr_boost = inven(item).pval3
                set_chr_boost(inven(item).pval3)
		update_and_handle()
	end

	-- Restoration
	if (inven(item).pval2 == 9) then

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
		update_and_handle()
	end

	-- Physical Resistance
	if (inven(item).pval2 == 10) then

		p_ptr.pres = inven(item).pval3
                if (p_ptr.pres > 100) then p_ptr.pres = 100 end
                set_pres(inven(item).pval3)
		update_and_handle()
	end

	-- Magic Resistance
	if (inven(item).pval2 == 11) then

		p_ptr.mres = inven(item).pval3
                if (p_ptr.mres > 100) then p_ptr.mres = 100 end
                set_mres(inven(item).pval3)
		update_and_handle()
	end

	-- The Nightmare Licialhyd!
	if (inven(item).pval2 == -1) then

		local ch
		local ch2

		msg_print("[1]. Enchant Item  [2]. Learn ability.")

		ch = inkey()

		if (ch == 50) then

			local i
			local maxabilities
			local learned

			maxabilities = p_ptr.lev / 3
			learned = 0

			i = CLASS_NIGHT1 * 10
			while (i < MAX_ABILITIES) do

				if (p_ptr.abilities[i + 1] > 0) then

					learned = learned + 1
				end
				if (learned >= maxabilities) then

					msg_print("You may not learn new Nightmare abilites at the moment.")
					return
				end
				i = i + 1
			end

			if (p_ptr.abilities[(CLASS_NIGHT1 * 10) + (inven(item).pval - 2300) + 1] == 0 and p_ptr.num_abilities >= 30) then

				msg_print("You may not learn any new abilities.")
				return
			else
				Term_save()
				show_file(string.format('n%d.txt', inven(item).pval), NULL, 0, 0)
				msg_print(string.format('Learn/improve the ability? [y/n] (Remaining: %d/%d)', learned, maxabilities))

				ch2 = inkey()

				if (ch2 == 89 or ch2 == 121) then

					if (p_ptr.abilities[(CLASS_NIGHT1 * 10) + (inven(item).pval - 2300) + 1] == 0) then

						p_ptr.num_abilities = p_ptr.num_abilities + 1
						if (abilities_def[(CLASS_NIGHT1 * 10) + (inven(item).pval - 2300) + 1].abtype == 1) then

							local j
							j = 0
							while (not(p_ptr.abilities_powers[j+1] == 0)) do j = j + 1 end
							p_ptr.abilities_powers[j+1] = (CLASS_NIGHT1 * 10) + (inven(item).pval - 2300) + 1
						end
					end
					p_ptr.abilities[(CLASS_NIGHT1 * 10) + (inven(item).pval - 2300) + 1] = p_ptr.abilities[(CLASS_NIGHT1 * 10) + (inven(item).pval - 2300) + 1] + 1;
					Term_load()
				else
					Term_load()
					return
				end
			end

		else

			local item2

			item2 = lua_pick_item(0)

			if (not(item2)) then return end

			if (get_object_flag4(item2, TR4_ENCHANTED) or item2.name1 > 0) then

				msg_print("This item is already magical.")
				return
			end

			if (item2.tval == TV_ESSENCE) then

				msg_print("You cannot use this on Essences.")
				return
			end

			forcecurse = 1
			make_item_magic(item2, inven(item).pval3, FALSE)
			forcecurse = 0
		end
	end

	inven_item_increase(item, -1)
        inven_item_describe(item)
        inven_item_optimize(item)

	energy_use = 100

end

-- A function called when generating a licialhyd.
-- Used in the source code, so don't remove it.
function prepare_licialhyd (licialhyd)

	licialhyd.pval2 = lua_randint(11);

	if (licialhyd.pval2 == 1 or licialhyd.pval2 == 2) then

		licialhyd.pval3 = lua_randint(30 + dun_level) * (1 + dun_level)

	elseif (licialhyd.pval2 == 10 or licialhyd.pval2 == 11) then

		licialhyd.pval3 = lua_randint(dun_level) + 5
		if (licialhyd.pval3 > 100) then licialhyd.pval3 = 100 end
	else
		licialhyd.pval3 = lua_randint(dun_level) + 5
	end

	-- In towns(shops), boost the power a bit.
	if (dun_level == 0) then

		if (licialhyd.pval2 == 1 or licialhyd.pval2 == 2) then

			licialhyd.pval3 = licialhyd.pval3 + lua_randint(5000)
		else
			licialhyd.pval3 = licialhyd.pval3 + lua_randint(30)
		end
	end
end

-- Returns the "name" of a licialhyd.
-- Used in the source code, so don't remove it.
function get_licialhyd_name (licialhyd)
	
	local licialname

	if (licialhyd.pval2 == 1) then licialname = "Healing"
	elseif (licialhyd.pval2 == 2) then licialname = "Mana"
	elseif (licialhyd.pval2 == 3) then licialname = "Strength"
	elseif (licialhyd.pval2 == 4) then licialname = "Intelligence"
	elseif (licialhyd.pval2 == 5) then licialname = "Wisdom"
	elseif (licialhyd.pval2 == 6) then licialname = "Dexterity"
	elseif (licialhyd.pval2 == 7) then licialname = "Constitution"
	elseif (licialhyd.pval2 == 8) then licialname = "Charisma"
	elseif (licialhyd.pval2 == 9) then licialname = "Restoration"
	elseif (licialhyd.pval2 == 10) then licialname = "Physical Resistance"
	elseif (licialhyd.pval2 == 11) then licialname = "Magic Resistance"
	elseif (licialhyd.pval2 == -1) then licialname = m_race(licialhyd.pval).name_char
	else licialname = "Unknown" end

	return licialname
end

-- Returns a licialhyd's value.
-- Used in the source code.
function get_licialhyd_value (licialhyd)

	local value

	if (licialhyd.pval2 == 1 or licialhyd.pval2 == 2) then

		value = licialhyd.pval3 * 5
	elseif (licialhyd.pval2 == 9) then

		value = 1000
	elseif (licialhyd.pval2 == -1) then

		value = m_race(licialhyd.pval).cursed * 1000
		value = value + multiply_divide(value, m_race(licialhyd.pval).cursed, 100)
	else

		value = (licialhyd.pval3 * 25) * licialhyd.pval3
	end

	return value
end

-- Prepare a random activation for rods!
function prepare_rods_activations (rod, level)

	local atype
	local spelltype
	local acttext
	
	-- If there's already an activation, return.
	if (rod.spell[1].type > 0) then return end

	-- Otherwise, generate an activation!
	-- We determine a type of activation.
	-- 1. Bolt spell.
	-- 2. Ball spell.

	atype = lua_randint(2)

	-- Choose an element for bolt and ball spells.
	if (atype == 1 or atype == 2) then

		local element
		local pick
		local chosen = 0
		local fixed = 0
		local rad = 0

		if (atype == 1) then spelltype = "Bolt"
		else spelltype = "Ball" end

		-- Pick an element.
		-- Depending on the rod's level, a different element may be chosen.
		while (chosen == 0) do
			
			pick = lua_randint(30)

			if (pick == 1) then

				element = GF_MISSILE
				chosen = 1
			end
			if (pick == 2 and level >= 5) then

				element = GF_FIRE
				chosen = 1
			end
			if (pick == 3 and level >= 5) then

				element = GF_COLD
				chosen = 1
			end
			if (pick == 4 and level >= 5) then

				element = GF_ELEC
				chosen = 1
			end
			if (pick == 5 and level >= 5) then

				element = GF_ACID
				chosen = 1
			end
			if (pick == 6 and level >= 5) then

				element = GF_POIS
				chosen = 1
			end
			if (pick == 7 and level >= 20) then

				element = GF_LITE
				chosen = 1
			end
			if (pick == 8 and level >= 10) then

				element = GF_DARK
				chosen = 1
			end
			if (pick == 9 and level >= 25) then

				element = GF_WARP
				chosen = 1
			end
			if (pick == 10 and level >= 15) then

				element = GF_WATER
				chosen = 1
			end
			if (pick == 11 and level >= 15) then

				element = GF_WIND
				chosen = 1
			end
			if (pick == 12 and level >= 15) then

				element = GF_EARTH
				chosen = 1
			end
			if (pick == 13 and level >= 25) then

				element = GF_SOUND
				chosen = 1
			end
			if (pick == 14 and level >= 40) then

				element = GF_RADIO
				chosen = 1
			end
			if (pick == 15 and level >= 60) then

				element = GF_CHAOS
				chosen = 1
			end
			if (pick == 16 and level >= 40) then

				element = GF_PHYSICAL
				chosen = 1
			end
			if (pick == 17 and level >= 60) then

				element = GF_MANA
				chosen = 1
			end
			if (pick == 18 and level >= 30) then

				element = GF_FROSTFIRE
				chosen = 1
			end
			if (pick == 19 and level >= 30) then

				element = GF_GREY
				chosen = 1
			end
			if (pick == 20 and level >= 40) then

				element = GF_TOXIC
				chosen = 1
			end
			if (pick == 21 and level >= 30) then

				element = GF_MUD
				chosen = 1
			end
			if (pick == 22 and level >= 20) then

				element = GF_CONFUSION
				chosen = 1
			end
			if (pick == 23 and level >= 30) then

				element = GF_ICE
				chosen = 1
			end
			if (pick == 24 and level >= 25) then

				element = GF_HARM
				chosen = 1
			end
			if (pick == 25 and level >= 30) then

				element = GF_UNDEAD_SMITE
				chosen = 1
			end
			if (pick == 26 and level >= 30) then

				element = GF_DEMON_SMITE
				chosen = 1
			end
			if (pick == 27 and level >= 50) then

				element = GF_EVIL_SMITE
				chosen = 1
			end
			if (pick == 28 and level >= 25) then

				element = GF_PARALYZE
				fixed = 1
				chosen = 1
			end
			if (pick == 29) then

				element = GF_OLD_HEAL
				chosen = 1
			end
			if (pick == 30 and level >= 80) then

				element = GF_ELEMENTAL
				chosen = 1
			end
		end

		-- Assign element.
		rod.spell[1].type = atype
		rod.spell[1].special1 = element

		-- Fixed power.
		if (fixed == 1) then rod.spell[1].special3 = 1 end

		-- Determine the power and radius(if applicable).
		if (element == GF_PARALYZE) then

			rod.spell[1].power = (lua_randint(level) / 10) + 1

			-- Staves receives more power.
			if (get_object_flag4(rod, TR4_MUST2H)) then rod.spell[1].power = rod.spell[1].power * 2 end

			if (atype == 2) then

				rad = (lua_randint(level) / 20) + lua_randint(2)
				rod.spell[1].special2 = rad
				acttext = string.format('(Power: %d  Rad: %d)', rod.spell[1].power, rad)
			else

				acttext = string.format('(Power: %d)', rod.spell[1].power)
			end
		else

			rod.spell[1].power = (lua_randint(level) * 3) + (level / 4) + 5

			-- Staves receives more power.
			if (get_object_flag4(rod, TR4_MUST2H)) then rod.spell[1].power = rod.spell[1].power * 2 end

			if (atype == 2) then

				rad = (lua_randint(level) / 20) + lua_randint(2)
				rod.spell[1].special2 = rad
				acttext = string.format('(Power: %d, Rad: %d)', rod.spell[1].power, rad)
			else

				acttext = string.format('(Power: %d)', rod.spell[1].power)
			end
		end

		-- Mana cost(recharge time).
		-- Staves receives lower cost.
		if (get_object_flag4(rod, TR4_MUST2H)) then rod.spell[1].cost = (lua_randint(level) / 8) + (level / 30)
		else rod.spell[1].cost = (lua_randint(level) / 5) + (level / 20) + 1 end

		-- Give it a name.
		rod.spell[1].name = string.format('%s %s %s', get_element_name(element), spelltype, acttext)
	end
end

-- Prepare an essence.
-- Mtype is the type of monster slain.
-- 0 = Normal monster
-- 1 = Elite
-- 2 = Boss
-- 3 = Unique
-- 4 = Nightmare
function prepare_essence (essence, r_idx, level, mtype)

	local bonus
	local power
	local statspower
	local acpower
	local skillspower
	local difference
	local res = 0
	local essenceact = 0
	local i = 0
	local resfound = 0
	local attfound = 0
	local chosen = 0

	-- Determine "bonus" points.
	bonus = ((level + m_race(r_idx).level) / 2) + (level / 5)

	-- Special kind of enemies gives more bonus.
	if (mtype == 4) then
		bonus = bonus * 2
		essence.cursed = m_race(r_idx).level
	elseif (mtype == 3) then bonus = bonus + (bonus / 2)
	elseif (mtype == 2) then bonus = bonus + (bonus / 3)
	elseif (mtype == 1) then bonus = bonus + (bonus / 5)
	end

	-- The "power" of the stats.
	statspower = m_race(r_idx).str + m_race(r_idx).dex + m_race(r_idx).mind + ((m_race(r_idx).hdice / 10) + (m_race(r_idx).hside / 10))

	-- The "power" of base AC.
	acpower = m_race(r_idx).ac / 20 + ((level + m_race(r_idx).level) / 2) + (level / 5)

	-- The "power" of skills.
	skillspower = m_race(r_idx).skill_attack + m_race(r_idx).skill_ranged + m_race(r_idx).skill_magic
	difference = (level - m_race(r_idx).level)
	difference = difference * 2
	if (difference < 0) then difference = 0 end
	skillspower = skillspower + difference

	-- Total power
	power = statspower + acpower + skillspower

	-- Roll for bonus.
	while (bonus > 0) do

		local broll

		broll = lua_randint(power)

		-- Stats bonus.
		if (broll >= (power - statspower)) then

			local strpercent
			local dexpercent
			local mindpercent
			local conpercent
			local roll

			strpercent = multiply_divide(m_race(r_idx).str, 100, statspower)
			dexpercent = multiply_divide(m_race(r_idx).dex, 100, statspower)
			mindpercent = multiply_divide(m_race(r_idx).mind, 100, statspower)
			conpercent = multiply_divide(((m_race(r_idx).hdice / 10) + (m_race(r_idx).hside / 10)), 100, statspower)

			roll = lua_randint(100)

			if (roll >= (100 - strpercent)) then

				essence.statsbonus[A_STR+1] = essence.statsbonus[A_STR+1] + 1
				bonus = bonus - 1
			elseif (roll >= (100 - strpercent - dexpercent)) then

				essence.statsbonus[A_DEX+1] = essence.statsbonus[A_DEX+1] + 1
				bonus = bonus - 1
			elseif (roll >= (100 - strpercent - dexpercent - mindpercent)) then
				
				essence.statsbonus[A_INT+1] = essence.statsbonus[A_INT+1] + 1
				essence.statsbonus[A_WIS+1] = essence.statsbonus[A_WIS+1] + 1
				bonus = bonus - 1
			else

				essence.statsbonus[A_CON+1] = essence.statsbonus[A_CON+1] + 1
				bonus = bonus - 1
			end

		-- Base AC increase.
		elseif (broll >= (power - statspower - acpower)) then

			essence.ac = essence.ac + 5
			bonus = bonus - 1
		else
			essence.tweakpoints = essence.tweakpoints + 1
			bonus = bonus - 1
		end
	end

	-- Resistances.
	-- Only one resistance per essences, up to 15%.
	for i = 1, MAX_RESIST do

		if (m_race(r_idx).resistances[i] > 0 or m_race(r_idx).resistances[i] < 0) then resfound = 1 end
	end

	if (resfound == 1) then
		while (res == 0) do

			local resamount
			local which
			local respercent

			which = lua_randint(MAX_RESIST - 1)

			if (m_race(r_idx).resistances[which+1] > 0 or m_race(r_idx).resistances[which+1] < 0) then

				respercent = ((level + m_race(r_idx).level) / 2) / 2
				if (respercent > 15) then respercent = 15 end
				resamount = multiply_divide(m_race(r_idx).resistances[which+1], respercent, 100)
				essence.resistances[which+1] = resamount
				res = 1
			end
		end
	end

	-- Activable spells.
	for i = 1, 20 do

		if (m_race(r_idx).spell[i].type > 0 and m_race(r_idx).spell[i].type < 9) then

			essence.spell[essenceact+1].name = m_race(r_idx).spell[i].name
			essence.spell[essenceact+1].act = m_race(r_idx).spell[i].act
			essence.spell[essenceact+1].type = m_race(r_idx).spell[i].type
			essence.spell[essenceact+1].power = m_race(r_idx).spell[i].power
			essence.spell[essenceact+1].special1 = m_race(r_idx).spell[i].special1
			essence.spell[essenceact+1].special2 = m_race(r_idx).spell[i].special2
			essence.spell[essenceact+1].special3 = m_race(r_idx).spell[i].special3
			essence.spell[essenceact+1].summchar = m_race(r_idx).spell[i].summchar
			if (m_race(r_idx).spell[i].cost <= 0) then essence.spell[essenceact+1].cost = 0
			else essence.spell[essenceact+1].cost = m_race(r_idx).spell[i].cost / 2 end

			essenceact = essenceact + 1
		end
	end

	if (essenceact >= 1) then give_object_flag3(essence, TR3_ACTIVATE) end

	-- Base damages and type.
	-- We pick one random attack, and apply it to the essence's damages.

	-- First let's see if there are any attacks to begin with.
	for i = 1, 20 do

		if (m_race(r_idx).attack[i].type >= 1 and m_race(r_idx).attack[i].type <= 3) then attfound = 1 end
	end

	-- If so, then pick one randomly.
	if (attfound == 1) then
		while (chosen == 0) do

			local pick
			local dambonus

			-- Determine the damages bonus.
			dambonus = ((level + m_race(r_idx).level) / 2) + (level / 5)

			-- Special kind of enemies gives more damages bonus.
			if (mtype == 4) then
				dambonus = dambonus * 2
			elseif (mtype == 3) then dambonus = dambonus + (dambonus / 2)
			elseif (mtype == 2) then dambonus = dambonus + (dambonus / 3)
			elseif (mtype == 1) then dambonus = dambonus + (dambonus / 5)
			end

			pick = lua_randint(20)

			if (m_race(r_idx).attack[pick].type >= 1 and m_race(r_idx).attack[pick].type <= 3) then

				-- Get damages and element.
				essence.dd = m_race(r_idx).attack[pick].ddice
				essence.ds = m_race(r_idx).attack[pick].dside
				if (m_race(r_idx).attack[pick].element == 0) then essence.extra1 = GF_PHYSICAL
				else essence.extra1 = m_race(r_idx).attack[pick].element end

				-- We might enhance the damages a bit.
				essence.dd = essence.dd + multiply_divide(essence.dd, lua_randint(dambonus), 100)
				essence.ds = essence.ds + multiply_divide(essence.ds, lua_randint(dambonus), 100)

				chosen = 1
			end
		end
	else

		essence.dd = 1
		essence.ds = 1
		essence.extra1 = GF_PHYSICAL
	end
	

	-- To_d and to_d
	-- Determine "bonus" points.
	bonus = (level + m_race(r_idx).level) / 2 + (level / 5)

	-- Special kind of enemies gives more bonus.
	if (mtype == 4) then
		bonus = bonus * 2
		essence.cursed = m_race(r_idx).level
	elseif (mtype == 3) then bonus = bonus + (bonus / 2)
	elseif (mtype == 2) then bonus = bonus + (bonus / 3)
	elseif (mtype == 1) then bonus = bonus + (bonus / 5)
	end

	bonus = bonus * 2

	while (bonus > 0) do

		if (lua_randint(100) > 50) then essence.to_h = essence.to_h + 1
		else essence.to_d = essence.to_d + 1 end

		bonus = bonus - 1
	end
	

	-- Unique and Nightmare essences are eternal.
	if (mtype == 3 or mtype == 4) then

		give_object_flag4(essence, TR4_ETERNAL)
	end

	-- Essences are treated as magical.
	give_object_flag1(essence, TR1_ENCHANTED)
end

-- Return an object's value in terms of skill points.
function object_skill_points_value (item)

	local totalvalue
	local i

	totalvalue = 0

	-- 1 point per stats bonus
	for i = 1, 6 do

		totalvalue = totalvalue + item.statsbonus[i]
	end
	-- 1 point per skills bonus
	for i = 1, SKILL_MAX do

		totalvalue = totalvalue + item.skillsbonus[i]
	end
	-- 1 point per 2 resistances
	for i = 1, MAX_RESIST do

		if (item.resistances[i] > 0) then totalvalue = totalvalue + (item.resistances[i] / 2) end
	end

	-- 10 points for the first 5 blows, 20 for the others.
	if (item.extrablows > 5) then

		local j
		j = item.extrablows - 5

		totalvalue = totalvalue + ((j * 20) + 50)
	else
		totalvalue = totalvalue + (item.extrablows * 10)
	end

	-- 20 points for the first 3 shots, 40 for the others.
	if (item.extrashots > 3) then

		local j
		j = item.extrashots - 3

		totalvalue = totalvalue + ((j * 40) + 60)
	else
		totalvalue = totalvalue + (item.extrashots * 20)
	end

	-- 2 points per to_s
	totalvalue = totalvalue + (item.spellbonus * 2)

	-- 2 points per speed.
	totalvalue = totalvalue + (item.speedbonus * 2)

	-- Life and Mana are worth 1 points for every 5 points
	totalvalue = totalvalue + (item.lifebonus / 5)
	totalvalue = totalvalue + (item.manabonus / 5)

	-- 1 point if it has permanent light.
	if (item.light > 0) then totalvalue = totalvalue + 1 end

	-- 3 points per reflect.
	totalvalue = totalvalue + (item.reflect * 3)

	-- 5 points for Eternality.
	if (get_object_flag4(item, TR4_ETERNAL)) then totalvalue = totalvalue + 5 end

	-- 3 points for Telepathy.
	if (get_object_flag3(item, TR3_TELEPATHY)) then totalvalue = totalvalue + 3 end

	-- 2 points for the 'Sustain' flags
	if (get_object_flag2(item, TR2_SUST_STR)) then totalvalue = totalvalue + 2 end
	if (get_object_flag2(item, TR2_SUST_INT)) then totalvalue = totalvalue + 2 end
	if (get_object_flag2(item, TR2_SUST_WIS)) then totalvalue = totalvalue + 2 end
	if (get_object_flag2(item, TR2_SUST_DEX)) then totalvalue = totalvalue + 2 end
	if (get_object_flag2(item, TR2_SUST_CON)) then totalvalue = totalvalue + 2 end
	if (get_object_flag2(item, TR2_SUST_CHR)) then totalvalue = totalvalue + 2 end

	-- 3 points for confusion resistance.
	if (get_object_flag2(item, TR2_RES_CONF)) then totalvalue = totalvalue + 3 end

	-- 2 points for fear resistance.
	if (get_object_flag2(item, TR2_RES_FEAR)) then totalvalue = totalvalue + 2 end

	-- 1 point for blind resistance.
	if (get_object_flag2(item, TR2_RES_BLIND)) then totalvalue = totalvalue + 1 end

	-- 3 points for hold life.
	if (get_object_flag2(item, TR2_HOLD_LIFE)) then totalvalue = totalvalue + 3 end

	-- 5 points for regen.
	if (get_object_flag3(item, TR3_REGEN)) then totalvalue = totalvalue + 5 end

	-- And finally, add all remaining tweak points.
	totalvalue = totalvalue + item.tweakpoints

	return (totalvalue)

end

-- The "treasures" that you can get by digging.
-- This function is called in the source code and should not be removed.
-- The "feat" variable is the wall type that was just dug, so you can have
-- various treasure drops based on the wall. x and y are the wall's coordinates.
function mining_treasures (x, y, feat)

	local pchance
	local dchance
	local power
	local treasure

	-- If we're inside a quest level, return.
	if (not(p_ptr.inside_quest == 0)) then return end

	-- Do not give any treasures in towns.
	if (dun_level == 0) then return end

	-- Trees, snow trees and rubble do not give treasures.
	if (feat == FEAT_TREES or feat == FEAT_SNOW_TREES or feat == FEAT_RUBBLE) then return end

	-- The chance of actually finding something.
	pchance = (p_ptr.skill[30] * 2) + 1
	dchance = (dun_level * 5)
	dchance = dchance + multiply_divide(dchance, dun_level * 5, 100)

	-- Roll for treasure!
	if (lua_randint(pchance) >= lua_randint(dchance)) then

		-- With base Mining 70+, you can find gold, ores, treasures and ruins.
		-- If you have only 15+, you can find gold, ores and treasures.
		-- Otherwise, only gold and ores may be found.
		if (p_ptr.skill_base[30] >= 70) then power = 300
		elseif (p_ptr.skill_base[30] >= 15) then power = 200
		else power = 100 end

		treasure = lua_randint(power)

		-- An ancient ruin!
		if (treasure > 200) then

			msg_print("You have discovered an ancient ruin!")
			cave_set_feat(y, x, 7)

			-- 50% of the time, it's going to be a randomly generated ruin.
			-- Otherwise, depending on the level and Mining skill, you may get
			-- something different. ;)
			cave(y, x).event = 4
			cave(y, x).eventtype = 9003
			cave(y, x).eventextra = lua_randint(196) + 1
			cave(y, x).eventextra = lua_randint(64) + 1
		-- A treasure.
		elseif (treasure > 100) then

			local oldobjectlevel

			-- Save object level.
			oldobjectlevel = object_level

			-- Determine the object's level
			object_level = (dun_level / 4) + (p_ptr.skill[30] / 6)

			-- Object level cannot be higher than the dungeon level.
			if (object_level > (dun_level)) then object_level = (dun_level) end

			-- Place it.
			place_object(y, x, FALSE, FALSE)

			-- It may be magical.
			if (lua_randint(p_ptr.skill[30] * 2) >= lua_randint((dun_level * 10))) then
				-- Try enchanting it, if there IS an object.
				if (not(cave(y, x).o_idx == 0)) then

					local otval

					-- To avoid too big "if" statement.
					otval = object(cave(y, x).o_idx).tval

					if (otval == TV_WEAPON or otval == TV_ROD or otval == TV_SOFT_ARMOR or otval == TV_HARD_ARMOR or otval == TV_DRAG_ARMOR
                                	or otval == TV_GLOVES or otval == TV_SHIELD or otval == TV_BOOTS or otval == TV_HELM or otval == TV_ARM_BAND
                                	or otval == TV_CROWN or otval == TV_RING or otval == TV_AMULET or otval == TV_RANGED or otval == TV_AMMO
                                	or otval == TV_CLOAK or otval == TV_THROWING or otval == TV_LITE) then

						make_item_magic(object(cave(y, x).o_idx), object_level, FALSE)
					end
				end
			end

			-- Restore old object level.
			object_level = oldobjectlevel
		-- 50% chance of finding an ore, otherwise gold.
		else

			if (lua_randint(100) > 50) then

				local oretype
				-- Ore type is based on dungeon level and Mining skill.
				power = (dun_level / 2) + (p_ptr.skill[30] / 5)

				-- Power cannot be higher than three times the dungeon level.
				if (power > (dun_level * 3)) then power = (dun_level * 3) end

				oretype = lua_randint(power)

				if (oretype >= 120) then

					drop_object_specific(y, x, 11, 19, 1, 0)
				elseif (oretype >= 90) then

					drop_object_specific(y, x, 11, 18, 1, 0)
				elseif (oretype >= 60) then

					drop_object_specific(y, x, 11, 17, 1, 0)
				elseif (oretype >= 40) then

					drop_object_specific(y, x, 11, 16, 1, 0)
				elseif (oretype >= 20) then

					drop_object_specific(y, x, 11, 15, 1, 0)
				elseif (oretype >= 10) then

					drop_object_specific(y, x, 11, 14, 1, 0)
				else

					drop_object_specific(y, x, 11, 13, 1, 0)
				end
			else
				place_gold(y, x)

				-- Make sure the gold is there. If so, then increase it.
				if (not(cave(y, x).o_idx == 0)) then

					if (object(cave(y, x).o_idx).tval == TV_GOLD) then

						object(cave(y, x).o_idx).pval = object(cave(y, x).o_idx).pval + multiply_divide(object(cave(y, x).o_idx).pval, p_ptr.skill[30] * 3, 100)
					end
				end
			end
		end
	end
end

add_event_handler("make_item_magic", make_item_magic)
add_event_handler("zap_rod", zap_rod)
add_event_handler("read_scroll", read_scroll)
add_event_handler("use_licialhyd", use_licialhyd)
add_event_handler("prepare_licialhyd", prepare_licialhyd)
add_event_handler("get_licialhyd_name", get_licialhyd_name)
add_event_handler("get_licialhyd_value", get_licialhyd_value)
add_event_handler("prepare_rods_activations", prepare_rods_activations)
add_event_handler("prepare_essence", prepare_essence)
add_event_handler("object_skill_points_value", object_skill_points_value)
add_event_handler("mining_treasures", mining_treasures)
